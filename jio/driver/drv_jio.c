

#include "drv_jio.inc"

typedef unsigned char bool;

typedef struct
{
	char			m_acSig[3];
	unsigned char	m_ucFlags;
	unsigned char	m_ucCommand;
} tdCommonHeader;

typedef struct
{
	unsigned long	m_uiSector;
	unsigned char	m_ucLength;
	void			*m_pvAddress;
} tdReadWriteData;

#define false	0
#define true	1

extern unsigned int W_FLAGS;
extern unsigned int W_COMMAND;

unsigned int		uiXModemCRC16(void *_pvAddress, unsigned int _uiLength, unsigned int _uiCRC);
bool				bJIOReceive(void *_pvDestination, unsigned int _uiSize);
void				vJIOTransmit(void *_pvSource, unsigned int _uiSize);

/*
 =======================================================================================================================
 =======================================================================================================================
 */

unsigned int uiTransmit
(
	void			*_pvAddress,
	unsigned int	_uiLength,
	unsigned char	_ucFlags,
	unsigned int	_uiCRC,
	bool			_bLast
)
{
    if(_ucFlags & FLAG_TX_CRC)
    {
        _uiCRC = uiXModemCRC16(_pvAddress, _uiLength, _uiCRC);
    }

    vJIOTransmit(_pvAddress, _uiLength);

	if(_bLast && (_ucFlags & FLAG_TX_CRC))
	{
		vJIOTransmit(&_uiCRC, sizeof(_uiCRC));
	}

	return _uiCRC;
}

/*
 =======================================================================================================================
 =======================================================================================================================
 */
unsigned char ucReceive(void *_pvAddress, unsigned int _uiLength, unsigned char _ucFlags)
{
	while(!bJIOReceive(_pvAddress, _uiLength))
	{
		if(_ucFlags & FLAG_TIMEOUT)
		{
			return COMMAND_DRIVE_REPORT_DRIVE_NOT_READY;
		}
	}

	return COMMAND_DRIVE_REPORT_OK;
}

/*
 =======================================================================================================================
 =======================================================================================================================
 */
unsigned char ucDoCommand
(
	unsigned long	_ulSector,
	unsigned int	_uiLength,
	void			*_pvAddress,
	unsigned char	*_pucFlagsAndCommand
)
{
	/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	unsigned int	uiTotalLength;
	unsigned int	uiReceivedCRC;
	unsigned int	uiComputedCRC;
	unsigned int	uiTransmitCRC;
	tdCommonHeader	oCommonHeader;
	tdReadWriteData oReadWriteHeader;
	unsigned char	ucCommandOrResult;
	unsigned char	ucFlags;
	unsigned char	ucCommand;
	/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

	ucFlags = _pucFlagsAndCommand[(unsigned int) &W_FLAGS];
	ucCommand = _pucFlagsAndCommand[(unsigned int) &W_COMMAND];

	oCommonHeader.m_acSig[0] = 'J';
	oCommonHeader.m_acSig[1] = 'I';
	oCommonHeader.m_acSig[2] = 'O';
	oCommonHeader.m_ucFlags = ucFlags;

	_uiLength >>= 8;

	oReadWriteHeader.m_uiSector = _ulSector;
	oReadWriteHeader.m_ucLength = _uiLength;
	oReadWriteHeader.m_pvAddress = _pvAddress;

	uiTotalLength = _uiLength * 512;

	do
	{
		ucCommandOrResult = COMMAND_DRIVE_REPORT_OK;
		oCommonHeader.m_ucCommand = ucCommand;

        uiTransmitCRC = uiTransmit(&oCommonHeader, sizeof(oCommonHeader), ucFlags, 0, (ucCommand == COMMAND_DRIVE_INFO) ||  (ucCommand == COMMAND_DRIVE_DISK_CHANGED));

		if(ucCommand == COMMAND_DRIVE_DISK_CHANGED)
		{
			/*~~~~~~~~~~~~~~~~~~~~~~~~~~*/
			unsigned int	uiAcknowledge;
			/*~~~~~~~~~~~~~~~~~~~~~~~~~~*/

			ucCommandOrResult = ucReceive(&uiAcknowledge, sizeof(uiAcknowledge), ucFlags);
			if(ucCommandOrResult == COMMAND_DRIVE_REPORT_OK)
			{
                if(uiAcknowledge == DRIVE_ANSWER_DISK_CHANGED)
				{
                    ucCommandOrResult = RESULT_DRIVE_DISK_CHANGED;
				}
                else if(uiAcknowledge == DRIVE_ANSWER_DISK_UNCHANGED)
				{
                    ucCommandOrResult = RESULT_DRIVE_DISK_UNCHANGED;
				}
				else
				{
					ucCommandOrResult = COMMAND_DRIVE_REPORT_CRC_ERROR;
				}
			}
		}
		else if(ucCommand == COMMAND_DRIVE_WRITE)
		{
			/*~~~~~~~~~~~~~~~~~~~~~~~~~~*/
			unsigned int	uiAcknowledge;
			/*~~~~~~~~~~~~~~~~~~~~~~~~~~*/

			uiTransmitCRC = uiTransmit(&oReadWriteHeader, sizeof(oReadWriteHeader), ucFlags, uiTransmitCRC, false);
			uiTransmit(_pvAddress, uiTotalLength, ucFlags, uiTransmitCRC, true);

			ucCommandOrResult = ucReceive(&uiAcknowledge, sizeof(uiAcknowledge), ucFlags);
			if(ucCommandOrResult == COMMAND_DRIVE_REPORT_OK)
			{
                if(uiAcknowledge == DRIVE_ANSWER_WRITE_FAILED)
				{
					ucCommandOrResult =
                        (uiAcknowledge == DRIVE_ANSWER_WRITE_PROTECTED)
							? COMMAND_DRIVE_REPORT_WRITE_PROTECTED : COMMAND_DRIVE_REPORT_WRITE_FAULT;
				}
                else if(uiAcknowledge != DRIVE_ANSWER_WRITE_OK)
				{
					ucCommandOrResult = COMMAND_DRIVE_REPORT_CRC_ERROR;
				}
			}
		}
		else
		{
			if(ucCommand == COMMAND_DRIVE_READ)
			{
				uiTransmit(&oReadWriteHeader, sizeof(oReadWriteHeader), ucFlags, uiTransmitCRC, true);
			}

			ucCommandOrResult = ucReceive(_pvAddress, uiTotalLength, ucFlags);

			if((ucCommandOrResult == COMMAND_DRIVE_REPORT_OK) && (ucFlags & FLAG_RX_CRC))
			{
				ucCommandOrResult = ucReceive(&uiReceivedCRC, sizeof(uiReceivedCRC), ucFlags);
				if(ucCommandOrResult == COMMAND_DRIVE_REPORT_OK)
				{
					uiComputedCRC = uiXModemCRC16(_pvAddress, uiTotalLength, 0);

					if(uiReceivedCRC != uiComputedCRC)
					{
						ucCommandOrResult = COMMAND_DRIVE_REPORT_CRC_ERROR;
					}
				}
			}
		}

        if((ucCommandOrResult != COMMAND_DRIVE_REPORT_OK) && (ucCommandOrResult != RESULT_DRIVE_DISK_CHANGED) && (ucCommandOrResult != RESULT_DRIVE_DISK_UNCHANGED))
		{
			oCommonHeader.m_ucCommand = ucCommandOrResult;
			uiTransmit(&oCommonHeader, sizeof(oCommonHeader), ucFlags, 0, true);
		}
	} while
	(
		(ucCommandOrResult != COMMAND_DRIVE_REPORT_OK)
    &&	(ucCommandOrResult != RESULT_DRIVE_DISK_CHANGED)
    &&	(ucCommandOrResult != RESULT_DRIVE_DISK_UNCHANGED)
	&&	(ucFlags & FLAG_AUTO_RETRY)
	);

	return ucCommandOrResult;
}

/*$off*/

/*

CALLING CONVENTION

Up to two parameters can be passed in registers; other parameters are
transferred on the stack.

The compiler assembler interface selects the parameters that can be placed
in the registers as follows:

Parameters, types, and locations

1                   2                   Remaining parameters

Byte                Byte                All types
E                   C                   Pushed

Byte                Word                All types
E                   BC                  Pushed

Byte                3 bytes (pointer)   All types
E                   Pushed              Pushed

Word                Byte                All types
DE                  C                   R7

Word                Word                All types
DE                  BC                  Pushed

3 bytes (pointer)   All types           All types
CDE                 Pushed              Pushed

4 bytes (long etc.) All types           All types
BCDE                Pushed              Pushed

Variable arguments  All types           All types
Pushed              Pushed              Pushed


RETURN VALUES

char                    A (from a non-banked function)
char                    L (from a banked function)
word                    HL
pointer                 HL
banked function pointer CHL
long, float, or double  BCHL


PRESERVING REGISTERS

A and HL are always considered destroyed after a function call. Registers
used for parameters are also considered destroyed after a function call; this
includes the entire 16-bit register. For example, if E is used for a parameter
DE is considered destroyed. All other registers (excluding return value
registers) must be preserved by the called function.

For example, consider the following function prototype:
long foo(int,...)

A and HL are always destroyed, BC is destroyed since it is used for the
return value. Since this is a vararg function it does not take any
parameter in registers which means that IX, IY, and DE must be preserved
as usual.

If the alternative register set is in use by the code generator -ua, HL' is
considered destroyed after each function call, but DE' and BC' must be
preserved.

*/

/*$on*/
