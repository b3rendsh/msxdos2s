; Diskdriver MSXDOS 2.20 cartridge (No real diskdriver)

; Source re-created by Z80DIS 2.2
; Z80DIS was written by Kenneth Gielow, Palo Alto, CA

; Code Copyrighted by ASCII and maybe others
; Source comments by Arjen Zeilemaker

; Sourcecode supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyrightholders
;
; ------------------------------------------------------------------------------
; H.J. Berends:
; Converted sources to assemble with z88dk z80asm
; Mod: removed self check command (call dos2memchk)
; Mod: HSH style 2 DOS2 skip key is INS

	SECTION	DRV


; Symbols which are defined by the disk hardware driver

	PUBLIC	INIHRD
	PUBLIC	DRIVES
	PUBLIC	INIENV
	PUBLIC	DSKIO
	PUBLIC	DSKCHG
	PUBLIC	GETDPB
	PUBLIC	CHOICE
	PUBLIC	DSKFMT
	PUBLIC	MTOFF
	PUBLIC	OEMSTA
	PUBLIC	DEFDPB
	PUBLIC	MYSIZE
	PUBLIC	SECLEN

SNSMAT  EQU     0141H   

DEFDPB	EQU	$-1
MYSIZE	EQU	0
SECLEN  EQU     512

DRIVES:
C6EFB:	LD	L,0                     ; number of drives of this interface = 0
        RET

DSKIO:
DSKCHG:
GETDPB:
J6EFE:	LD	A,12                    ; other error
        SCF

MTOFF:
INIENV:
C6F01:	RET

CHOICE:
J6F02:	LD	HL,I6F06                ; empty choice string
        RET

I6F06:	DEFB	0

DSKFMT:
J6F07:	LD	A,16                    ; other error
        SCF
        RET

; ------------------------------------------------------------------------------
; Mod: no self check routine
OEMSTA:	
	SCF
	RET
; ------------------------------------------------------------------------------

; Mod: HSH style 2 DOS2 skip key is INS (also removed obsolete code)

INIHRD:	LD      A,8
        CALL    SNSMAT
        BIT     2,A                     ; INS key pressed ?
        RET	NZ			; Z=yes
	INC     SP
        INC     SP
        RET
