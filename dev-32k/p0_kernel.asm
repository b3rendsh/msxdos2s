; ------------------------------------------------------------------------------
; p0_kernel.asm
; DOS 2.20 kernel page 0: BDOS functions
; Based on ASCII DOS 2.20 codebase s2
;
; Code Copyrighted by ASCII, OKEI and maybe others
; Source origin is the msxsyssrc repository by Arjen Zeilemaker
; Restructure, modifications and additional comments by H.J. Berends
; 
; Sourcecode supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyrightholders
; ------------------------------------------------------------------------------
; Modifications:
; 01. The rom bank switching code is removed
; 02. FAT16 kernel changes based on FAT16 v0.12 by OKEI
; 03. Optimized FAT16 code to free space for format and ramdisk routines
; 04. Relocated format boot sector code if using FAT16 option
; 05. Use Microsoft standards to determine if partition is FAT16 or FAT12 (DPBSET)
; 06. Changed the free disk calculation routine for FAT16 partitions
; 07. Removed unused code (OPTM)
; 08. Added DOSV231 option


		INCLUDE "disk.inc"		; Assembler directives
		INCLUDE	"msx.inc"		; MSX constants and definitions

		SECTION	P0_KERNEL

		ORG	08000H

		PUBLIC	K1_BEGIN		; begin of kernel code
		PUBLIC	K1_END			; end of kernel code

; ------------------------------------------------------------------------------
; Following kernel code is copied to ram in page 0
; ------------------------------------------------------------------------------
K1_BEGIN:
		PHASE  0

; ---------------------------------------------------------
; *** Header ($0000 - $0094) ***
; ---------------------------------------------------------

		JP	K_INIT			; init kernel / BDOS
		DEFS	2,0

C0005:		JP	K_BDOS			; Kernel BDOS handler
		DEFS	4,0
C000C:		JP	SRDSLT			; RDSLT
		DEFS	5,0
C0014:		JP	SWRSLT			; WRSLT
		DEFS	5,0
C001C:		JP	SCALSLT			; CALSLT
		DEFS	5,0
C0024:		JP	SENASLT			; ENASLT
		DEFS	1,0
		JP	CC206			; debugger
		DEFS	5,0
C0030:		JP	SCALLF			; CALLF
		DEFS	5,0
C0038: 		JP	SIRQ			; KEYINT
C003B:		OUT	(0A8H),A		; SSLOT
		LD	A,(DFFFF)
		CPL
		LD	L,A
		AND	H
		OR	D
		JR	J004E
C0046:		OUT	(0A8H),A		; SSLOTL
		LD	A,L
		JR	J004E
C004B:		OUT	(0A8H),A		; SSLOTE
		LD	A,E
J004E:		LD	(DFFFF),A
		LD	A,B
		OUT	(0A8H),A
		RET
		DEFS	7,0
C005C:		JP	K_ALLSEG		; ALL_SEG
C005F:		JP	K_FRESEG		; FRE_SEG

		DEFS	$1E,0
C0080:		JP	KB_CHARIN		; CON input
C0083:		JP	KB_CHAROUTC		; CON output
C0086:		JP	KB_CHARSTAT		; CON check input status
C0089:		JP	KB_LPTOUT		; LPT output
C008C:		JP	KB_LPTSTAT		; LPT check output status
C008F:		JP	KB_AUXOUTC		; AUX output
C0092:		JP	KB_AUXIN		; AUX input

; ---------------------------------------------------------
; *** Initialize kernel / BDOS ***
; ---------------------------------------------------------
K_INIT:		LD	IY,D_BB80		; Base address for IY relative kernel variables
		LD	DE,RM_DPB		; Ramdisk DPB
		LD	A,(MASTER)
		LD	C,A
		LD	L,80H
		LD	B,1
		CALL	K_INIT_DRVTAB		; Allocate and initialize drive table for ramdisk
		JR	NZ,J00D8
		LD	HL,DRVTBL
		LD	DE,SDPBLI
		LD	B,4
J00B1:		LD	A,(HL)
		INC	HL
		LD	C,(HL)
		INC	HL
		OR	A
		PUSH	HL
		PUSH	BC
		LD	B,A
		LD	L,10H
		CALL	NZ,K_INIT_DRVTAB	; Allocate and initialize drive tables
		POP	BC
		POP	HL
		JR	NZ,J00D8
		DJNZ	J00B1
		LD	D,B
		CALL	F_ASSIGN
		LD	HL,I012C		; device table
J00CB:		LD	A,(HL)
		OR	A
		JR	Z,J0101
		INC	HL
		PUSH	HL
		LD	HL,43
		CALL	K_ALLOC_P2
		POP	DE
J00D8:		JR	NZ,J012A
		LD	BC,(D_BBF4)
		LD	(D_BBF4),HL		; Update start of device chain
		LD	(HL),C
		INC	HL
		LD	(HL),B
		INC	HL
		EX	DE,HL
		LDI
		LDI
		EX	DE,HL
		LD	BC,6
		ADD	HL,BC
		EX	DE,HL
		LD	BC,12
		LDIR
		LD	A,80H
		LD	(DE),A
		LD	B,14H
		XOR	A
J00FB:		INC	DE
		LD	(DE),A
		DJNZ	J00FB
		JR	J00CB

J0101:		LD	B,5			; number of buffers
		CALL	F_BUFFER		; allocate buffers
		LD	B,00H
		CALL	F_JOIN
		CALL	C0E35
		CALL	K_CON_INIT
	IF OPTM = 0
		; copy cursor on/off escape codes to data segment
		; used in K_FLUSHBUF
		LD	HL,I0178
		LD	DE,I_B066
		LD	BC,6
		LDIR
	ENDIF
		LD	A,1
		LD	(ST_COU),A
		LD	(IY+16),0FFH
		CALL	C10CA			; initialize clockchip
		OR	A
		RET

J012A:		SCF
		RET

I012C:		DEFB	0FFH
		DEFW	I0932
		DEFB	0A3H			; device, ascii mode, console input device, console output device
	        DEFB    "CON        "

		DEFB	0FFH
		DEFW	I09E2
		DEFB	0A0H			; device, ascii mode
		DEFB	"LST        "

		DEFB	0FFH
		DEFW	I09E2
		DEFB	0A0H			; device, ascii mode
		DEFB	"PRN        "

		DEFB	0FFH
		DEFW	I0A03
		DEFB	0A0H			; device, ascii mode
		DEFB	"NUL        "

		DEFB	0FFH
		DEFW	I09BF
		DEFB	0A0H			; device, ascii mode
		DEFB	"AUX        "

		DEFB	0

	IF OPTM = 0
		; cursor on/of escape codes (used in K_FLUSHBUF)
I0178:		DEFB	27,"y5"
		DEFB	27,"x5"
	ENDIF
; ---------------------------------------------------------
; Subroutine allocate and initialize drive tables
; Input:  B  = number of drives
;         L  = driver jump table offset
;         C  = slot id
;         DE = pointer to DPB entry
; ---------------------------------------------------------
K_INIT_DRVTAB:	XOR	A
J017F:		EX	AF,AF'
		PUSH	HL
		LD	HL,96
J0184:		CALL	K_ALLOC_P2
		JR	NZ,J01C9
		EX	DE,HL
		PUSH	DE
		POP	IX
		PUSH	BC
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
J0191:		INC	HL
		PUSH	HL
J0193:		LD	(IX+2),C
		LD	(IX+3),B
		LD	A,(BC)
		INC	A
		LD	(IX+8),A
		LD	L,A
		LD	H,00H
		INC	BC
		LD	A,(BC)
		LD	(IX+29),A
		LD	BC,D_BBFB
		JR	Z,J01AE
		LD	BC,I_BA23
J01AE:		ADD	HL,HL
		ADD	HL,BC
		LD	(HL),E
		INC	HL
		LD	(HL),D
		POP	DE
		POP	BC
		POP	HL
		LD	(IX+0),C
		LD	(IX+1),L
		EX	AF,AF'
		LD	(IX+6),A
		INC	A
	IFDEF FAT16
		LD	(IX+30),0FFH
	ENDIF
		LD	(IX+31),0FFH
		DJNZ	J017F
		XOR	A
		RET

J01C9:		POP	HL
		RET

; ---------------------------------------------------------
; Subroutine allocate BDOS data block
; Input:  HL = size
; ---------------------------------------------------------
K_ALLOC_P2:	PUSH	DE
		PUSH	BC
		INC	HL
		RES	0,L
		LD	B,H
		LD	C,L
		LD	HL,(D_B064)
J01D5:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,D
		OR	E
		JR	Z,J0203
		BIT	0,E
		JR	NZ,J01E8
		EX	DE,HL
		SBC	HL,BC
		JR	NC,J01ED
		ADD	HL,BC
		EX	DE,HL
J01E8:		RES	0,E
		ADD	HL,DE
		JR	J01D5

J01ED:		EX	DE,HL
		DEC	HL
		DEC	HL
		JR	Z,J0217
		DEC	DE
		DEC	DE
		LD	A,D
		OR	E
		JR	Z,J01FF
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		ADD	HL,DE
		JR	J0217

J01FF:		INC	BC
		INC	BC
		JR	J0217

J0203:		LD	A,_NORAM
		INC	BC
		INC	BC
		LD	HL,(D_B064)
		OR	A
		SBC	HL,BC
		JR	C,J0227
		JP	P,J0227
		LD	(D_B064),HL
		DEC	BC
		DEC	BC
J0217:		LD	(HL),C
		SET	0,(HL)
		INC	HL
		LD	(HL),B
		INC	HL
		PUSH	HL
J021E:		LD	(HL),00H
		INC	HL
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,J021E
		POP	HL
J0227:		POP	BC
		POP	DE
		OR	A
		RET

; ---------------------------------------------------------
; Subroutine free BDOS data block
; Input:  HL = address of block
; ---------------------------------------------------------
K_FREE_P2:	DEC	HL
		DEC	HL
		RES	0,(HL)
		PUSH	DE
		PUSH	BC
		LD	HL,(D_B064)
J0234:		LD	C,(HL)
		BIT	0,C
		JR	NZ,J023F
		INC	HL
		LD	B,(HL)
		INC	HL
		ADD	HL,BC
		JR	J0234

J023F:		LD	(D_B064),HL
J0242:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,D
		OR	E
		JR	Z,J026A
		BIT	0,E
		JR	NZ,J0265
J024E:		PUSH	HL
		ADD	HL,DE
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		POP	HL
		BIT	0,C
		JR	NZ,J025F
		INC	BC
		INC	BC
		EX	DE,HL
		ADD	HL,BC
		EX	DE,HL
		JR	J024E

J025F:		DEC	HL
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		INC	HL
		INC	HL
J0265:		RES	0,E
		ADD	HL,DE
		JR	J0242

J026A:		POP	BC
		POP	DE
		RET

; ---------------------------------------------------------
; *** Kernel BDOS handler ***
; ---------------------------------------------------------
K_BDOS:		EI
		CALL	H_BDOS
		CALL	C0278
		LD	(DSBBFD),A
		RET

; Subroutine BDOS handler (basic)
C0278:		PUSH	HL
		PUSH	BC
		EX	AF,AF'
		LD	A,(CH_COU)
		DEC	A
		CALL	Z,K_CHARFLUSH
		LD	A,C
		CP	71H
		JR	C,J0289
		LD	C,9
J0289:		EX	AF,AF'
		LD	B,00H
		LD	HL,I02A2
		ADD	HL,BC
		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,C
		LD	IY,D_BB80
J0299:		POP	BC
		EX	(SP),HL
		RET

; ---------------------------------------------------------
; Subroutine to handle invalid BDOS function calls
; ---------------------------------------------------------
K_INVALID:	LD	A,_IBDOS
J029E:		LD	HL,0
		RET

; ---------------------------------------------------------
; MSX-DOS 2 Functions 
; Note: Function $09 _STROUT is implemented in PRTBUF (F1C9),
;	 msxdos2.sys handles the call to this routine.
; ---------------------------------------------------------
I02A2:		DEFW	F_TERM0,F_CONIN,F_CONOUT,F_AUXIN	; 0
		DEFW	F_AUXOUT,F_LSTOUT,F_DIRIO,F_DIRIN	; 4
		DEFW	F_INNOE,K_INVALID,F_BUFIN,F_CONST	; 8
		DEFW	F_CPMVER,F_DSKRST,F_SELDSK,F_FOPEN	; 0C
		DEFW	F_FCLOSE,F_SFIRST,F_SNEXT,F_FDEL	; 10
		DEFW	F_RDSEQ,F_WRSEQ,F_FMAKE,F_FREN		; 14
		DEFW	F_LOGIN,F_CURDRV,F_SETDTA,F_ALLOC	; 18
		DEFW	K_INVALID,K_INVALID,K_INVALID,K_INVALID	; 1C
		DEFW	K_INVALID,F_RDRND,F_WRRND,F_FSIZE	; 20
		DEFW	F_SETRND,K_INVALID,F_WRBLK,F_RDBLK	; 24
		DEFW	F_WRZER,K_INVALID,F_GDATE,F_SDATE	; 28
		DEFW	F_GTIME,F_STIME,F_VERIFY,F_RDABS	; 2C
		DEFW	F_WRABS,F_DPARM,K_INVALID,K_INVALID	; 30
		DEFW	K_INVALID,K_INVALID,K_INVALID,K_INVALID	; 34
		DEFW	K_INVALID,K_INVALID,K_INVALID,K_INVALID	; 38
		DEFW	K_INVALID,K_INVALID,K_INVALID,K_INVALID	; 3C
		DEFW	F_FFIRST,F_FNEXT,F_FNEW,F_OPEN		; 40
		DEFW	F_CREATE,F_CLOSE,F_ENSURE,F_DUP		; 44
		DEFW	F_READ,F_WRITE,F_SEEK,F_IOCTL		; 48
		DEFW	F_HTEST,F_DELETE,F_RENAME,F_MOVE	; 4C
		DEFW	F_ATTR,F_FTIME,F_HDELETE,F_HRENAME	; 50
		DEFW	F_HMOVE,F_HATTR,F_HFTIME,F_GETDTA	; 54
		DEFW	F_GETVFY,F_GETCD,F_CHDIR,F_PARSE	; 58
		DEFW	F_PFILE,F_CHKCHR,F_WPATH,F_FLUSH	; 5C
		DEFW	F_FORK,F_JOIN,F_TERM,K_INVALID		; 60
		DEFW	K_INVALID,F_ERROR,F_EXPLAIN,F_FORMAT	; 64
		DEFW	F_RAMD,F_BUFFER,F_ASSIGN,F_GENV		; 68
		DEFW	F_SENV,F_FENV,F_DSKCHK,F_DOSVER		; 6C
		DEFW	F_REDIR					; 70

; ---------------------------------------------------------
; *** Functions: 01-08,0A,0B ***
; *** Console I/O routines	***
; ---------------------------------------------------------

; ---------------------------------------------------------
; Subroutine initialize buffered input history buffer
; ---------------------------------------------------------
K_CON_INIT:	LD	HL,I_B0D0
		LD	(D_BB82),HL
		LD	(D_BB80),HL
		LD	DE,I_B1D0
		EX	DE,HL
		OR	A
		SBC	HL,DE
		EX	DE,HL
J0395:		LD	(HL),0DH
		INC	HL
		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,J0395
		LD	(D_BB7F),A

; ---------------------------------------------------------
; Subroutine clear stored input, console output not duplicated to printer
; ---------------------------------------------------------
K_CON_CLEAR:	XOR	A
		LD	(D_BB8D),A
		LD	(D_BB8A),A
		RET

; ---------------------------------------------------------
; Function $01 _CONIN
; ---------------------------------------------------------
F_CONIN:	CALL	F_INNOE
		PUSH	HL
		LD	A,L
		CALL	C085A
		CALL	NC,C0871
		POP	HL
		XOR	A
		RET

; ---------------------------------------------------------
; Function $02 _CONOUT
; ---------------------------------------------------------
F_CONOUT:	LD	A,E
		CALL	C086C
		XOR	A
		LD	H,A
		LD	L,A
		RET

; ---------------------------------------------------------
; Function $08 _INNOE
; ---------------------------------------------------------
F_INNOE:	BIT	0,(IY+9)
		LD	C,0FFH
		JR	NZ,K_HCONIN
		CALL	C08B2
		LD	L,A
		XOR	A
		LD	H,A
		RET

; ---------------------------------------------------------
; Function $0B _CONST
; ---------------------------------------------------------
F_CONST:	CALL	C0897
		LD	L,A
		XOR	A
		LD	H,A
		RET

; ---------------------------------------------------------
; Function $06 _DIRIO
; ---------------------------------------------------------
F_DIRIO:	LD	A,E
		INC	A
		JR	Z,J03E8
		BIT	1,(IY+9)
		LD	A,E
		LD	C,00H
		JR	NZ,K_HCONOUT
		CALL	KB_CHAROUT
		XOR	A
		LD	H,A
		LD	L,A
		RET

J03E8:		BIT	0,(IY+9)
		LD	C,00H
		JR	NZ,K_HCONIN
		LD	HL,D_BB8D
		CP	(HL)
		JR	NZ,J0406
		CALL	KB_CHARSTAT
		JR	NZ,J0406
		LD	L,A
		LD	H,A
		RET

; ---------------------------------------------------------
; Function $07 _DIRIN
; ---------------------------------------------------------
F_DIRIN:	BIT	0,(IY+9)
		LD	C,00H
		JR	NZ,K_HCONIN
J0406:		LD	A,(D_BB8D)
		OR	A
		CALL	Z,KB_CHARIN
		LD	L,A
		XOR	A
		LD	H,A
		LD	(D_BB8D),A
		RET

; ---------------------------------------------------------
; Subroutine character from console input file handle
; ---------------------------------------------------------
K_HCONIN:	LD	B,00H
		PUSH	BC
		CALL	C1D47
		POP	DE
		OR	A
		JR	NZ,J0439
		OR	E
		JR	Z,J0426
		LD	A,B
		SUB	03H
		JR	Z,J0439
J0426:		LD	L,B
		XOR	A
		LD	H,A
		RET

; ---------------------------------------------------------
; Subroutine character to console output file handle
; ---------------------------------------------------------
K_HCONOUT:	LD	B,1
		JR	J0446

; ---------------------------------------------------------
; Function $03 _AUXIN
; ---------------------------------------------------------
F_AUXIN:	LD	B,3
		LD	C,0FFH
		CALL	C1D47
		OR	A
		LD	L,B
		LD	H,A
		RET	Z
J0439:		LD	C,_INERR
		JR	J044F

; ---------------------------------------------------------
; Subroutine $04 _AUXOUT
; ---------------------------------------------------------
F_AUXOUT:	LD	B,3
		JR	J0443

; ---------------------------------------------------------
; Function $05 _LSTOUT
; ---------------------------------------------------------
F_LSTOUT:	LD	B,4
J0443:		LD	C,0FFH
		LD	A,E
J0446:		CALL	C1D22
		OR	A
		LD	L,A
		LD	H,A
		RET	Z
		LD	C,_OUTERR
J044F:		LD	B,A
		LD	A,C
		CALL	C3723
J0454:		JR	J0454

; ---------------------------------------------------------
; Function $0A _BUFIN
; ---------------------------------------------------------
F_BUFIN:	PUSH	DE
		BIT	0,(IY+9)
		JR	NZ,K_HBUFIN
		XOR	A
		CALL	K_CON_BUFIN
		JR	J049E
	
; ---------------------------------------------------------
; console line buffered input (file handle)
; ---------------------------------------------------------
K_HBUFIN:	EX	DE,HL
		LD	B,(HL)
		LD	C,00H
		INC	HL
		PUSH	HL
J0469:		PUSH	HL
		PUSH	BC
		LD	C,0FFH
		CALL	K_HCONIN
		LD	A,L
		POP	BC
		POP	HL
		OR	A
		JR	Z,J0469
		CP	0AH
		JR	Z,J0469
		CP	0DH
		JR	Z,J0499
		LD	E,A
		LD	A,B
		CP	C
		JR	Z,J048E
		INC	C
		INC	HL
		LD	(HL),E
		LD	A,E
		PUSH	HL
		PUSH	BC
		CALL	C086C
		JR	J0495

J048E:		PUSH	HL
		PUSH	BC
		LD	A,7
		CALL	KB_CHAROUT
J0495:		POP	BC
		POP	HL
		JR	J0469

J0499:		POP	HL
		LD	(HL),C
		CALL	C086C
J049E:		POP	HL
		PUSH	HL
		LD	A,(HL)
		INC	HL
		CP	(HL)
		JR	Z,J04AC
		LD	E,(HL)
		LD	D,0
		ADD	HL,DE
		INC	HL
		LD	(HL),0DH
J04AC:		POP	DE
		XOR	A
		LD	L,A
		LD	H,A
		RET

; ---------------------------------------------------------
; Subroutine console line buffered input (keyboard)
; Input:  A = force console output to screen flag
; ---------------------------------------------------------
K_CON_BUFIN:	LD	(D_BB7A),A
		INC	DE
		XOR	A
		LD	(DE),A
		DEC	DE
		LD	(D_BB7C),A

; restart console line input
J04BB:		PUSH	DE
		CALL	K_EDIT_LINE
		POP	DE
		DEC	A
		JR	Z,J050C
		DEC	A
		JR	Z,J052A
		INC	DE
		LD	A,(DE)
		OR	A
		RET	Z
		LD	B,A
		LD	(D_BB7F),A
		LD	A,(D_BB7C)
		OR	A
		JR	Z,J04EA
		PUSH	DE
		PUSH	BC
		LD	HL,(D_BB82)
J04D9:		INC	DE
		LD	A,(DE)
		CP	(HL)
		JR	NZ,J04E6
		CALL	K_CBUF_INC
		DJNZ	J04D9
		LD	A,(HL)
		CP	0DH
J04E6:		POP	BC
		POP	DE
		JR	Z,J04FE
J04EA:		LD	HL,(D_BB80)
J04ED:		INC	DE
		LD	A,(DE)
		LD	(HL),A
		CALL	K_CBUF_INC
		DJNZ	J04ED
		LD	A,(HL)
		LD	(HL),0DH
		CALL	K_CBUF_INC
		LD	(D_BB80),HL
J04FE:		LD	(D_BB82),HL
J0501:		CP	0DH
		RET	Z
		LD	A,(HL)
		LD	(HL),0DH
		CALL	K_CBUF_INC
		JR	J0501

; previous line
J050C:		LD	A,(D_BB7F)
		OR	A
		JR	Z,J04BB
		LD	HL,(D_BB82)
J0515:		CALL	K_CBUF_DEC
		LD	A,(HL)
		CP	0DH
		JR	Z,J0515
J051D:		CALL	K_CBUF_DEC
		LD	A,(HL)
J0521:		CP	0DH
		JR	NZ,J051D
		CALL	K_CBUF_INC
		JR	J0544

; next line
J052A:		LD	A,(D_BB7F)
		OR	A
		JR	Z,J04BB
		LD	HL,(D_BB82)
J0533:		LD	A,(HL)
		CP	0DH
		CALL	K_CBUF_INC
		JR	NZ,J0533
		SCF
J053C:		CALL	NC,K_CBUF_INC
		LD	A,(HL)
		CP	0DH
		JR	Z,J053C

; update current line
J0544:		LD	(D_BB82),HL
		PUSH	DE
		LD	A,(DE)
		LD	B,A
		INC	DE
		INC	DE
		LD	C,0FFH
J054E:		LD	A,(HL)
		LD	(DE),A
		INC	C
		CALL	K_CBUF_INC
		CP	0DH
		INC	DE
		JR	Z,J055C
		DJNZ	J054E
		INC	C
J055C:		POP	DE
		INC	DE
		LD	A,C
		LD	(DE),A
		DEC	DE
		LD	(D_BB7C),A
		JP	J04BB

; ---------------------------------------------------------
; Subroutine next position in history buffer
; Input:  HL = pointer in history buffer
; Output: HL = updated pointer in history buffer
; ---------------------------------------------------------
K_CBUF_INC:	PUSH	AF
		PUSH	DE
		LD	DE,I_B1CF
		OR	A
		SBC	HL,DE
		ADD	HL,DE
		INC	HL
		JR	NZ,J0576
		LD	HL,I_B0D0
J0576:		POP	DE
		POP	AF
		RET

; ---------------------------------------------------------
; Subroutine previous position in history buffer
; Input:  HL = pointer in history buffer
; Output: HL = updated pointer in history buffer
; ---------------------------------------------------------
K_CBUF_DEC:	PUSH	AF
		PUSH	DE
		LD	DE,I_B0D0
		OR	A
		SBC	HL,DE
		ADD	HL,DE
		DEC	HL
		JR	NZ,J0588
		LD	HL,I_B1CF
J0588:		POP	DE
		POP	AF
		RET

; ---------------------------------------------------------
; Subroutine edit line
; Input:  DE = pointer to buffer
; ---------------------------------------------------------
K_EDIT_LINE:	LD	HL,(D_BB8B)
		LD	(D_BB87),HL
		LD	(D_BB7D),HL
		EX	DE,HL
J0595:		LD	C,(HL)
		INC	HL
		LD	(D_BB84),HL
		LD	A,(HL)
		OR	A
		LD	B,A
		JR	Z,J05A5
		INC	HL
		CALL	C0811
		DEC	HL
		LD	A,B
J05A5:		LD	(D_BB86),A
		XOR	A
		CALL	C06BC
I05AC:		LD	DE,I05AC
		PUSH	DE
		PUSH	HL
		LD	HL,D_BB86
		LD	A,(HL)
		CP	B
		JR	NC,J05B9
		LD	(HL),B
J05B9:		POP	HL
		CALL	C08B2
	IF OPTM = 0
		OR	A
		RET	Z
	ENDIF
		CP	0AH
		RET	Z
		CP	0DH
		JP	Z,J07A8
		CP	1DH
		JP	Z,J06F9
		CP	1CH
		JP	Z,J06DB
		CP	7FH
		JP	Z,J0748
		CP	08H
		JP	Z,J0741
		CP	12H
		JP	Z,J06B8
		CP	1BH
		JR	Z,J05EA
		CP	18H
		JR	Z,J05EA
		CP	15H
J05EA:		JP	Z,J07A1
		CP	1EH
		JP	Z,J07BF
		CP	1FH
		JP	Z,J07C6
		CP	0BH
		JP	Z,J06F2
		LD	E,A
		LD	A,(D_BB7B)
		OR	A
		JP	NZ,J0653
		LD	A,(D_BB86)
		CP	B
		JR	Z,J063F
		INC	HL
		LD	A,E
		CALL	C17D6
		JR	NC,J062E
		LD	A,(D_BB86)
		DEC	A
		CP	B
		JR	NZ,J0623
		INC	A
		CP	C
		DEC	HL
		JP	NC,J06AB
		INC	HL
		INC	A
		LD	(D_BB86),A
J0623:		LD	A,(HL)
		CALL	C17D6
		INC	HL
		CALL	NC,C07CD
		DEC	HL
		JR	J0678

J062E:		CALL	C07CD
		JR	C,J06A3
		LD	A,(HL)
		CP	20H		; " "
		JR	C,J06A3
		LD	A,E
		CP	20H		; " "
		JR	C,J06A3
		JR	J0649

J063F:		CP	C
		JR	NC,J06AE
		LD	A,E
		CALL	C17D6
		JR	C,J0659
		INC	HL
J0649:		LD	(HL),E
		LD	A,E
		INC	B
		CALL	C0836
		CALL	C0821
		RET

J0653:		LD	A,E
		CALL	C17D6
		JR	NC,J0687
J0659:		LD	A,(D_BB86)
		INC	A
		CP	C
		JR	NC,J06AB
		INC	A
		LD	(D_BB86),A
		DEC	A
		DEC	A
		SUB	B
		JR	Z,J0677
		PUSH	DE
		PUSH	BC
		LD	C,A
		LD	B,0
		ADD	HL,BC
		LD	D,H
		LD	E,L
		INC	DE
		INC	DE
		LDDR
		POP	BC
		POP	DE
J0677:		INC	HL
J0678:		LD	(HL),E
		INC	HL
		CALL	C08B2
		LD	(HL),A
		DEC	HL
		CALL	C07DC
		INC	B
		INC	B
		JP	C06FD

J0687:		LD	A,(D_BB86)
		CP	C
		JR	NC,J06AE
		INC	A
		LD	(D_BB86),A
		DEC	A
		SUB	B
		JR	Z,J06A2
		PUSH	DE
		PUSH	BC
		LD	C,A
		LD	B,0
		ADD	HL,BC
		LD	D,H
		LD	E,L
		INC	DE
		LDDR
		POP	BC
		POP	DE
J06A2:		INC	HL
J06A3:		LD	(HL),E
		CALL	C07DC
		INC	B
		JP	C06FD

J06AB:		CALL	C08B2
J06AE:		LD	A,7
		PUSH	BC
		PUSH	HL
		CALL	KB_CHAROUT
		POP	HL
		POP	BC
		RET

; Subroutine INS key, flip insert mode and update cursor shape
J06B8:		LD	A,(D_BB7B)
		CPL

; Subroutine set insert mode and update cursor shape
C06BC:		LD	(D_BB7B),A
		OR	A
		LD	A,79H		; "y"
		JR	NZ,J06C5
		DEC	A
J06C5:		PUSH	BC
		PUSH	HL
		PUSH	DE
		PUSH	AF
		LD	A,1BH
		CALL	KB_CHAROUT
		POP	AF
		CALL	KB_CHAROUT
		LD	A,34H
		CALL	KB_CHAROUT
		POP	DE
		POP	HL
		POP	BC
		RET

J06DB:		LD	A,(D_BB86)
		CP	B
		RET	Z
		INC	HL
		INC	B
		LD	A,(HL)
		CALL	C17D6
		JP	NC,C0836
		CALL	C0836
		INC	HL
		INC	B
		LD	A,(HL)
		JP	C0836

J06F2:		LD	A,B
		OR	A
		RET	Z
		LD	B,00H
		JR	C06FD

J06F9:		LD	A,B
		OR	A
		RET	Z
		DEC	B

; Subroutine cursor to position
; Input:  B = position
C06FD:		LD	HL,(D_BB84)
		LD	DE,(D_BB87)
		PUSH	BC
		INC	B
		JR	J0729

J0708:		INC	HL
		LD	A,(HL)
		CALL	C17D6
		JR	NC,J0719
		INC	HL
		DJNZ	J0727
		DEC	HL
		DEC	HL
		POP	BC
		DEC	B
		PUSH	BC
		JR	J072B

J0719:		CP	09H
		JR	NZ,J0723
		LD	A,E
		OR	07H
		LD	E,A
		JR	J0728

J0723:		CP	20H
		JR	NC,J0728
J0727:		INC	DE
J0728:		INC	DE
J0729:		DJNZ	J0708
J072B:		PUSH	HL
		LD	HL,(D_BB8B)
		OR	A
		SBC	HL,DE
		JR	Z,J073E
J0734:		LD	A,8
		CALL	C0836
		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,J0734
J073E:		POP	HL
		POP	BC
		RET

J0741:		LD	A,B
		OR	A
		RET	Z
		DEC	B
		CALL	C06FD
J0748:		LD	A,(D_BB86)
		CP	B
		RET	Z
		DEC	A
		LD	(D_BB86),A
		SUB	B
		JR	Z,J0784
		LD	E,A
		INC	HL
		LD	A,(HL)
		DEC	HL
		CALL	C17D6
		LD	A,E
		JR	NC,J0777
		PUSH	HL
		LD	HL,D_BB86
		DEC	(HL)
		POP	HL
		DEC	A
		JR	Z,J0784
		PUSH	BC
		PUSH	HL
		LD	C,A
		LD	B,0
		INC	HL
		LD	D,H
		LD	E,L
		INC	HL
		INC	HL
		LDIR
		POP	HL
		POP	BC
		JR	J0784

J0777:		PUSH	BC
		PUSH	HL
		LD	C,A
		LD	B,0
		INC	HL
		LD	D,H
		LD	E,L
		INC	HL
		LDIR
		POP	HL
		POP	BC
J0784:		INC	HL
		CALL	C07DC
		DEC	HL
		JP	C06FD

; Subroutine delete line on screen
C078C:		XOR	A
		CP	B
		LD	B,A
		CALL	NZ,C06FD
		CALL	C07D7
		LD	B,00H
		CALL	C06FD
		LD	HL,(D_BB84)
		LD	(HL),00H
		DEC	HL
		RET

J07A1:		CALL	C078C
		POP	DE
		JP	J0595

J07A8:		INC	HL
		CALL	C07DC
		LD	HL,(D_BB84)
		LD	A,(D_BB86)
		LD	(HL),A
		XOR	A
		CALL	C06BC
		LD	A,13
		CALL	C0836
		POP	HL
		XOR	A
		RET

J07BF:		POP	HL
		CALL	C078C
		LD	A,1
		RET

J07C6:		POP	HL
		CALL	C078C
		LD	A,2
		RET

; Subroutine if double byte header character in buffer, replace double byte charcter with space
C07CD:		LD	A,(HL)
		CALL	C17D6
		RET	NC
		INC	HL
		LD	(HL),20H
		DEC	HL
		RET

; Subroutine clear rest of line on screen
C07D7:		PUSH	BC
		PUSH	DE
		PUSH	HL
		JR	J07E7

; Subroutine refresh line on screen
; Input:  B = current position
C07DC:		PUSH	BC
		PUSH	DE
		PUSH	HL
		LD	A,(D_BB86)
		SUB	B
		LD	B,A
		CALL	C0811
J07E7:		LD	DE,(D_BB8B)
		LD	HL,(D_BB7D)
		OR	A
		SBC	HL,DE
		JR	Z,J0803
		JR	C,J0803
J07F5:		LD	A,20H
		CALL	C0836
		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,J07F5
		LD	(D_BB7D),DE
J0803:		LD	A,1BH
		CALL	KB_CHAROUT
		LD	A,4BH
		CALL	KB_CHAROUT
		POP	HL
		POP	DE
		POP	BC
		RET

; Subroutine text to console, update maximum column position
; Input:  HL = pointer to text
;         B  = size of text
C0811:		PUSH	BC
		INC	B
		JR	J081D

J0815:		LD	A,(HL)
		CALL	C0836
		CALL	C0821
		INC	HL
J081D:		DJNZ	J0815
		POP	BC
		RET

; Subroutine update maximum column position
C0821:		PUSH	HL
		PUSH	BC
		LD	HL,(D_BB7D)
		LD	BC,(D_BB8B)
		OR	A
		SBC	HL,BC
		JR	NC,J0833
		LD	(D_BB7D),BC
J0833:		POP	BC
		POP	HL
		RET

; Subroutine character to console (make control character printable, allow force to screen)
C0836:		PUSH	BC
		PUSH	DE
		PUSH	HL
		CALL	C085A
		JR	NC,J0847
		PUSH	AF
		LD	A,5EH
		CALL	C084E
		POP	AF
		ADD	A,40H
J0847:		CALL	C084E
		POP	HL
		POP	DE
		POP	BC
		RET

; Subroutine character to console (allow force to screen)
C084E:		LD	B,A
		LD	A,(D_BB7A)
		OR	A
		LD	A,B
		JP	Z,C0871
		JP	C08EE

; Subroutine is outputable key ?
C085A:		CP	0DH
		RET	Z
		CP	0AH
		RET	Z
		CP	09H
		RET	Z
		CP	08H
		RET	Z
		CP	7FH
		RET	Z
		CP	20H
		RET

; Subroutine character to console (with console status, redirection support and TAB translation)
C086C:		PUSH	AF
		CALL	C0897
		POP	AF

; Subroutine character to console (with redirection support and TAB translation)
C0871:		CP	09H
		JR	NZ,C0882
J0875:		LD	A,20H
		CALL	C0882
		LD	A,(D_BB8B)
		AND	07H
		JR	NZ,J0875
		RET

; Subroutine character to console (with redirection support)
C0882:		LD	HL,(D_BB8B)
		CALL	C0915
		LD	(D_BB8B),HL
		BIT	1,(IY+9)
		JP	Z,J0908
		LD	C,0FFH
		JP	K_HCONOUT

; Subroutine get console status
C0897:		CALL	KB_CHARSTAT
		LD	B,A
		LD	A,(D_BB8D)
		OR	A
		JR	NZ,J08AF
		LD	A,B
		OR	A
		RET	Z
		CALL	KB_CHARIN
		CALL	C08C5
		OR	A
		RET	Z
		LD	(D_BB8D),A
J08AF:		XOR	A
		DEC	A
		RET

; Subroutine ensure input from keyboard
C08B2:		LD	A,(D_BB8D)
		LD	(IY+13),00H
		OR	A
		RET	NZ
J08BB:		CALL	KB_CHARIN
		CALL	C08C5
		OR	A
		JR	Z,J08BB
		RET

; Subroutine handle special keys
C08C5:		CP	10H
		JR	Z,J08E5
		CP	0EH
		JR	Z,J08E8
		CP	03H
		JR	Z,J08DC
		CP	13H
		RET	NZ
		CALL	KB_CHARIN
		CP	03H
		LD	A,00H
		RET	NZ
J08DC:		LD	A,_CTRLC
		LD	B,00H
		CALL	C3723
J08E3:		JR	J08E3

J08E5:		LD	A,0FFH
		DEFB	0FEH
J08E8:		XOR	A
		LD	(D_BB8A),A
		XOR	A
		RET

; Subroutine character to screen (with TAB translation)
C08EE:		CP	09H
		JR	NZ,C08FF
J08F2:		LD	A,20H
		CALL	C08FF
		LD	A,(D_BB8B)
		AND	07H
		JR	NZ,J08F2
		RET

; Subroutine character to screen
C08FF:		LD	HL,(D_BB8B)
		CALL	C0915
		LD	(D_BB8B),HL
J0908:		CALL	KB_CHAROUT
		LD	HL,D_BB8A
		BIT	0,(HL)
		RET	Z
		LD	E,A
		JP	F_LSTOUT

; Subroutine update console column position
C0915:		INC	HL
		CP	7FH
		JR	Z,J091D
		CP	20H
		RET	NC
J091D:		DEC	HL
		LD	B,A
		LD	A,H
		OR	L
		LD	A,B
		RET	Z
		DEC	HL
		CP	08H
		RET	Z
		CP	7FH
		RET	Z
		INC	HL
		CP	0DH
		RET	NZ
		LD	HL,0
		RET

; CON device jumptable
I0932:		JP	J0941
		JP	J098E
		JP	J09A1
		JP	J0A1C
		JP	J09B1

; CON device input handler
J0941:		BIT	5,C
		JR	NZ,J094B
		CALL	KB_CHARIN
		LD	B,A
		XOR	A
		RET

J094B:		LD	HL,(D_BB78)
		LD	A,(HL)
		OR	A
		JR	NZ,J0979
		LD	DE,I_B1D0
		LD	A,0FFH
		LD	(DE),A
		LD	A,0FFH
		CALL	K_CON_BUFIN
		LD	A,10
		CALL	C08EE
		LD	HL,ISB1D1
		LD	E,(HL)
		LD	D,00H
		INC	HL
		EX	DE,HL
		ADD	HL,DE
		LD	(HL),0DH
		INC	HL
		LD	(HL),0AH
		INC	HL
		LD	(HL),00H
		EX	DE,HL
		LD	A,(HL)
		CP	1AH
		JR	Z,J0985
J0979:		INC	HL
		LD	(D_BB78),HL
		LD	B,A
		CP	0AH
		LD	A,_EOL
		RET	Z
		XOR	A
		RET

J0985:		LD	B,A
		LD	(HL),00H
		LD	(D_BB78),HL
		LD	A,_EOF
		RET

; CON device output handler
J098E:		BIT	5,C
		JR	NZ,J0997
		CALL	KB_CHAROUT
		XOR	A
		RET

J0997:		PUSH	AF
		CALL	C0897
		POP	AF
		CALL	C08EE
		XOR	A
		RET

; CON device check if input ready handler
J09A1:		BIT	5,C
		JR	NZ,J09AB
		CALL	KB_CHARSTAT
		LD	E,A
		XOR	A
		RET

J09AB:		CALL	C0897
		LD	E,A
		XOR	A
		RET

; CON device get screen size handler
J09B1:		CALL	KB_SCREENSIZE
		XOR	A
		RET

; Subroutine clear line input buffer
C09B6:		LD	HL,ISB1D2
		LD	(D_BB78),HL
		LD	(HL),00H
		RET

; AUX device jumptable
I09BF:		JP	J09CE
		JP	J09DD
		JP	J0A1C
		JP	J0A1C
		JP	J0A17

; AUX device input handler
J09CE:		CALL	KB_AUXIN
		LD	B,A
		CP	1AH
		JR	Z,J0A14
		CP	0DH
		LD	A,_EOL
		RET	Z
		XOR	A
		RET

; AUX device output handler
J09DD:		CALL	KB_AUXOUT
		XOR	A
		RET

; LST/PRN device jumptable
I09E2:		JP	J0A12
		JP	J09F1
		JP	J0A1C
		JP	J09FD
		JP	J0A17

; LST/PRN device output handler
J09F1:  	CALL	KB_LPTOUT
		JR	NC,J0A1A
		RES	0,(IY+10)
		LD	A,_STOP
		RET

; LST/PRN device check if output ready handler
J09FD:		CALL	KB_LPTSTAT
		LD	E,A
		XOR	A
		RET

; NUL device jumptable
I0A03:		JP	J0A12
		JP	J0A1A
		JP	J0A1C
		JP	J0A1C
		JP	J0A17

J0A12:		LD	B,1AH
J0A14:		LD	A,_EOF
		RET

J0A17:		LD	DE,0
J0A1A:		XOR	A
		RET

J0A1C:		LD	E,0FFH
		XOR	A
		RET

; ---------------------------------------------------------
; *** BIOS calls ***
; ---------------------------------------------------------

; Subroutine get screen size
KB_SCREENSIZE:	LD	A,(LINLEN)
		LD	E,A
		LD	A,(CRTCNT)
		LD	HL,CNSDFG
		ADD	A,(HL)
		LD	D,A
		RET

; Subroutine get character from keyboard
KB_CHARIN:	CALL	H_CHIN
		CALL	K_CHARFLUSH
		PUSH	IX
		LD	IX,CHGET
		CALL	K_BIOS
		CALL	KB_CHECK_STOP
		POP	IX
		RET

; Subroutine get status from keyboard
KB_CHARSTAT:	CALL	H_CHST
		LD	HL,ST_COU
		DEC	(HL)
		JR	NZ,J0A69
		INC	(HL)
		LD	A,(CH_COU)
		DEC	A
		CALL	Z,K_CHARFLUSH
		PUSH	IX
		LD	IX,CHSNS
		CALL	K_BIOS
		CALL	KB_CHECK_STOP
		POP	IX
		LD	A,0FFH
		RET	NZ
		LD	A,65H
		LD	(ST_COU),A
J0A69:		XOR	A
		RET

KB_CHAROUTC:	LD	A,C

; Subroutine output character to screen
KB_CHAROUT:	CALL	H_CHOU
		PUSH	IX
		LD	IX,CHPUT
		CALL	K_BIOS
		POP	IX
		RET

	IF OPTM = 1

; Screen output is not buffered: keep the H.CHFL hook for compatibility
; the rest of the code is never executed because D_BB77 remains 0.
K_CHARFLUSH:	JP	H_CHFL

	ELSE
; Unused code
Q_0A7B: 	LD	E,A
		CP	1BH
		CALL	Z,K_CHARFLUSH
		LD	HL,D_BB76
		BIT	0,(HL)
		RES	0,(HL)
		JR	NZ,J0A91
		CALL	C17D6
		JR	NC,J0A91
		SET	0,(HL)
J0A91:		LD	A,2
		LD	(CH_COU),A
		LD	A,(D_BB77)
		LD	C,A
		LD	B,00H
		LD	HL,D_B06C
		ADD	HL,BC
		LD	(HL),E
		INC	A
		LD	(D_BB77),A
		CP	64H
		JR	Z,K_FLUSHBUF
		LD	A,(ESCCNT)
		OR	A
		JR	NZ,K_FLUSHBUF
		LD	A,E
		CP	0AH
		JR	Z,K_FLUSHBUF
		CP	07H
		JR	Z,K_FLUSHBUF
		RET

; Subroutine flush screen output buffer (if any)
K_CHARFLUSH:	CALL	H_CHFL
		PUSH	AF
		LD	A,(D_BB77)
		OR	A
		JR	Z,J0ACC
		PUSH	BC
		PUSH	DE
		PUSH	HL
		CALL	K_FLUSHBUF
		POP	HL
		POP	DE
		POP	BC
J0ACC:		POP	AF
		RET

; Subroutine flush screen output buffer
K_FLUSHBUF:	EX	AF,AF'
		EXX
		PUSH	AF
		PUSH	BC
		PUSH	DE
		PUSH	HL
		PUSH	IX
		PUSH	IY
		LD	HL,D_B06C
		LD	A,(D_BB76)
		BIT	0,A
		PUSH	AF
		LD	A,(D_BB77)
		JR	Z,J0AE9
		DEC	A
		JR	Z,J0B07
J0AE9:		LD	B,A
		LD	A,(ESCCNT)
		OR	A
		JR	NZ,J0AF6
		LD	HL,ISB069
		INC	B
		INC	B
		INC	B
J0AF6:		CALL	SFLUSH
		PUSH	HL
		LD	HL,I_B066
		LD	B,3
		LD	A,(ESCCNT)
		OR	A
		CALL	Z,SFLUSH
		POP	HL
J0B07:		XOR	A
		LD	(D_BB77),A
		LD	(CH_COU),A
		POP	AF
		JR	Z,J0B1A
		LD	A,(HL)
		LD	(D_B06C),A
		LD	A,1
		LD	(D_BB77),A
J0B1A:		POP	IY
		POP	IX
		POP	HL
		POP	DE
		POP	BC
		POP	AF
		EXX
		EX	AF,AF'
		RET
	ENDIF ; OPTM

; Subroutine output character to printer
KB_LPTOUT:	CALL	H_LSTO
		CALL	K_CHARFLUSH
		PUSH	IX
		LD	IX,LPTOUT
		CALL	K_BIOS
		POP	IX
		RET

; Subroutine get printer status
KB_LPTSTAT:	CALL	H_LSTS
		PUSH	IX
		LD	IX,LPTSTT
		CALL	K_BIOS
		POP	IX
		RET

; Subroutine get character from AUX device
KB_AUXIN:	CALL	K_CHARFLUSH
		LD	HL,SAUXIN
		CALL	C3726
		RET

KB_AUXOUTC:	LD	A,C

; Subroutine output character to AUX device
KB_AUXOUT:	CALL	K_CHARFLUSH
		LD	HL,SAUXOUT
		CALL	C3726
		RET

; Subroutine check and handle CTRL-STOP
KB_CHECK_STOP:	PUSH	AF
		LD	A,(INTFLG)
		SUB	03H
		JR	Z,J0B65
		POP	AF
		RET

J0B65:		LD	(INTFLG),A
		LD	IX,KILBUF
		CALL	K_BIOS
		LD	A,_STOP
		LD	B,00H
		CALL	C3723
J0B76:		JR	J0B76

; ---------------------------------------------------------
; Subroutine call main-bios
; ---------------------------------------------------------
K_BIOS:		EX	AF,AF'
		EXX
		PUSH	AF
		PUSH	BC
		PUSH	DE
		PUSH	HL
		PUSH	IY
		EXX
		EX	AF,AF'
		CALL	P0_CALL
		EX	AF,AF'
		EXX
		POP	IY
		POP	HL
		POP	DE
		POP	BC
		POP	AF
		EXX
		EX	AF,AF'
		RET

; ------------------------------------------------------------
; *** Functions: 00,0C-0E,18-1B,2A-2E,31,57,58,5F,62,65-70 ***
; ------------------------------------------------------------

; ---------------------------------------------------------
; Function $0C _CPMVER
; ---------------------------------------------------------
F_CPMVER:	LD	HL,0022H
		XOR	A
		RET

; ---------------------------------------------------------
; Function $0D _DSKRST
; ---------------------------------------------------------
F_DSKRST:	LD	B,0FFH
		LD	D,00H
		CALL	F_FLUSH
		LD	A,1
		LD	(CUR_DRV),A	; default drive = A:
		LD	HL,DBUF
		LD	(DTA_AD),HL
		XOR	A
		LD	H,A
		LD	L,A
		RET

; ---------------------------------------------------------
; Function $0E _SELDSK
; ---------------------------------------------------------
F_SELDSK:	INC	E
		LD	A,E
		LD	C,_IDRV
		CALL	NZ,C3606
		JR	Z,J0BBF
		LD	A,(HL)
		INC	HL
		OR	(HL)
		JR	Z,J0BBF
		LD	A,E
		LD	(CUR_DRV),A
		LD	C,00H
J0BBF:		LD	HL,(SNUMDR)
		LD	H,0
		LD	A,C
		RET

; ---------------------------------------------------------
; Function $18 _LOGIN
; ---------------------------------------------------------
F_LOGIN:	LD	B,8
		LD	HL,0
J0BCB:		ADD	HL,HL
		PUSH	HL
		LD	A,B
		CALL	C3606
		LD	A,(HL)
		INC	HL
		OR	(HL)
		POP	HL
		JR	Z,J0BD8
		INC	HL
J0BD8:		DJNZ	J0BCB
		XOR	A
		RET

; ---------------------------------------------------------
; Function $19 _CURDRV
; ---------------------------------------------------------
F_CURDRV:	LD	A,(CUR_DRV)
		DEC	A
		LD	L,A
		XOR	A
		LD	H,A
		RET

; ---------------------------------------------------------
; Function $1A _SETDTA
; Input:  DE = disk transfer address
; ---------------------------------------------------------
F_SETDTA:	LD	(DTA_AD),DE
		XOR	A
		LD	H,A
		LD	L,A
		RET

; ---------------------------------------------------------
; Function $1B _ALLOC
; Input:  C  = 1BH (_ALLOC)
;         E  = Drive number (0=current, 1=A: etc)
; Output: A  = Sectors per cluster
;         BC = Sector size (always 512)
;         DE = Total clusters on disk
;         HL = Free clusters on disk
;         IX = Pointer to DPB
;         IY = Pointer to first FAT sector
; ---------------------------------------------------------
F_ALLOC:	LD	C,E
		LD	B,00H
		LD	IX,I_B9DA
		CALL	C318D		; Check disk change
		OR	A
		LD	C,0FFH
		RET	NZ
		PUSH	HL
		POP	IX
		LD	E,(IX+22)	; DE = total clusters on disk (max cluster number+1)
		LD	D,(IX+23)
		PUSH	DE
	IFDEF FAT16
		CALL	CHKDRV
		JP	Z,ALLOC16	; Z=FAT16
	ENDIF
		LD	DE,2		; Start with cluster 2
		LD	B,D		; BC = 0: free cluster count
		LD	C,D
J0C09:		PUSH	DE
		CALL	C2D86A		; ifdef FAT16 don't check drive again else C2D86A=C2D86
		LD	A,D
		OR	E
		JR	NZ,J0C12
		INC	BC		; Increase free cluster counter
J0C12:		POP	DE
		EX	(SP),HL
		SBC	HL,DE		; Cluster number = max cluster number?
		ADD	HL,DE
		EX	(SP),HL
		INC	DE		; Next cluster number
		JR	NZ,J0C09	; Loop until end of FAT
		PUSH	BC
ALLOC_RET:	LD	E,(IX+12)
		LD	D,(IX+13)
		CALL	C2B6A
		LD	DE,11
		ADD	HL,DE
		LD	DE,(SSECBUF)
		PUSH	DE
		LD	BC,512
		LDIR
		POP	IY
		POP	HL
		POP	DE
		DEC	DE
		NOP
		LD	C,(IX+2)
		LD	B,(IX+3)
		PUSH	BC
		LD	C,(IX+10)
		INC	C
		POP	IX
		XOR	A
		RET

; ---------------------------------------------------------
; Function $2E _VERIFY
; ---------------------------------------------------------
F_VERIFY:	LD	A,E
		LD	(RAWFLG),A
		XOR	A
		LD	H,A
		LD	L,A
		RET

; ---------------------------------------------------------
; Function $31 _DPARM
; ---------------------------------------------------------
F_DPARM:	LD	IX,I_B9DA
		LD	B,00H
		LD	C,L
		PUSH	DE
		CALL	C318D
		POP	DE
		OR	A
		RET	NZ
		PUSH	HL
		POP	IX
		PUSH	DE
		LD	BC,8
		ADD	HL,BC
		LDI
		LD	BC,512
		LD	A,C
		LD	(DE),A
		INC	DE
		LD	A,B
		LD	(DE),A
		INC	DE
		INC	HL
		LD	A,(HL)
		INC	HL
		INC	A
		LD	(DE),A
		INC	DE
		INC	HL
		LDI
		LDI
		LDI
		LD	A,(HL)
		INC	HL
		PUSH	HL
		LD	L,(HL)
		LD	H,00H
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	A,L
		LD	(DE),A
		INC	DE
		LD	A,H
		LD	(DE),A
		INC	DE
		POP	HL
		INC	HL
		INC	DE
		INC	DE
		PUSH	DE
		INC	DE
		LD	BC,12
		LDIR
		PUSH	DE
		EX	DE,HL
		LD	BC,-7
		ADD	HL,BC
		INC	(HL)
		JR	NZ,J0CA4
		INC	HL
		INC	(HL)
J0CA4:		EX	DE,HL
		POP	DE
		LD	B,8
		XOR	A
J0CA9:		LD	(DE),A
		INC	DE
		DJNZ	J0CA9
		POP	DE
		LDI
		LD	L,(IX+22)
		LD	H,(IX+23)
		DEC	HL
		LD	B,(IX+11)
	IFDEF FAT16
FIXCAL:		LD	C,29H
		ADC	A,A
		DJNZ	FIXCAL+1	; jump in middle of instruction which makes it 'add hl,hl'
	ELSE
		JR	J0CBD
J0CBC:		ADD	HL,HL
J0CBD:		DJNZ	J0CBC
	ENDIF
		LD	C,(IX+20)
		LD	B,(IX+21)
		ADD	HL,BC
		EX	DE,HL
		DEC	HL
		DEC	HL
	IFDEF FAT16 ; ALLSEC
		OR	A
		JR	Z,ALLS1		; FAT12
		LD	(HL),0
		DEC	HL
		LD	(HL),0
		LD	BC,0011h
		ADD	HL,BC
		LD	(HL),A
		DEC	HL
	ENDIF
ALLS1:		LD	(HL),D
		DEC	HL
		LD	(HL),E
		POP	DE
		XOR	A
		RET

; ---------------------------------------------------------
; Function $57 _GETDTA
; Output: DE = disk transfer address
; ---------------------------------------------------------
F_GETDTA:	LD	DE,(DTA_AD)
		XOR	A
		RET

; ---------------------------------------------------------
; Function $58 _GETVFY
; ---------------------------------------------------------
F_GETVFY:	LD	A,(RAWFLG)
		OR	A
		JR	Z,J0CDD
		LD	A,0FFH
J0CDD:		LD	B,A
		XOR	A
		RET

; ---------------------------------------------------------
; Function $5F _FLUSH
; ---------------------------------------------------------
F_FLUSH:	LD	A,B
		CP	0FFH
		JR	Z,J0CEB
		CALL	C3606
		LD	B,A
		LD	A,C
		RET	Z
J0CEB:		LD	A,B
		CALL	C2C4A
		LD	A,D
		OR	A
		RET	Z
		LD	A,B
		CALL	C2C5A
		CALL	C35EB
		XOR	A
		RET

; ---------------------------------------------------------
; Function $00 _TERM0
; ---------------------------------------------------------
F_TERM0:	LD	B,00H

; ---------------------------------------------------------
; Function $62 _TERM
; ---------------------------------------------------------
F_TERM:		LD	A,B
		LD	B,00H
		CALL	C3723
J0D03:		JR	J0D03

; ---------------------------------------------------------
; Function $65 _ERROR
; ---------------------------------------------------------
F_ERROR:	LD	B,(IY+125)
		XOR	A
		RET

; ---------------------------------------------------------
; Function $66 _EXPLAIN
; Input:  B = error code
; ---------------------------------------------------------
F_EXPLAIN:	LD	A,B
		PUSH	DE
		PUSH	IY
		LD	IY,(MASTER-1)
		LD	IX,(SERR_M)
		CALL	CALSLT
		EI
		POP	IY
		LD	B,A
		OR	A
		DEC	HL
		CALL	NZ,C0D26
		XOR	A
		LD	(HL),A
		POP	DE
		RET

; Convert byte number to string
; Input:  A  = number (0-99)
;         HL = pointer to string
C0D26:		LD	C,0FFH
J0D28:		INC	C
		SUB	0AH
		JR	NC,J0D28
		ADD	A,3AH
		PUSH	AF
		LD	A,C
		OR	A
		CALL	NZ,C0D26
		POP	AF
		LD	(HL),A
		INC	HL
		RET

; ---------------------------------------------------------
; Function $67 _FORMAT
; ---------------------------------------------------------
F_FORMAT:	EX	AF,AF'
		PUSH	HL
		POP	IX
		LD	A,B
		CALL	C3606
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	H
		LD	A,C
		RET	Z
		EX	AF,AF'
		OR	A
		JR	NZ,J0D58
		CALL	C3013
		PUSH	HL
		POP	IX
		LD	B,(IX+0)
		EX	DE,HL
		OR	A
		RET

J0D58:		EX	AF,AF'
		CALL	C2C49
		CALL	C2C59
		PUSH	HL
J0D60:		PUSH	DE
		PUSH	IX
		POP	DE
		XOR	A
		CALL	C2731
		LD	HL,04000H
		OR	A
		SBC	HL,DE
		EX	(SP),HL
		POP	BC
		SBC	HL,BC
		JR	C,J0D7E
		SBC	HL,BC
		JR	C,J0D81
		ADD	HL,BC
		EX	DE,HL
		ADD	IX,BC
		JR	J0D60

J0D7E:		ADD	HL,BC
		LD	B,H
		LD	C,L
J0D81:		SET	7,D
		PUSH	DE
		POP	IX
		LD	D,A
		EX	AF,AF'
		POP	HL
		CALL	C3030
		LD	BC,9
		ADD	HL,BC
		LD	(HL),00H
		RET

; ---------------------------------------------------------
; Function $68 _RAMD
; ---------------------------------------------------------
F_RAMD:		PUSH	BC
		LD	B,8
		LD	D,B
		CALL	F_ASSIGN
		POP	BC
		INC	B
		JP	Z,J0E2D
		DEC	B
		JP	Z,J0E04
		LD	HL,(D_BA33)
		LD	A,H
		OR	L
		LD	C,_RAMDX
		JP	NZ,J0E2F
		CALL	C0E35
		LD	DE,I_BE02
		LD	HL,D_BE00
J0DB6:		EXX
		LD	A,1
		LD	B,30H
		CALL	K_ALLSEG
		JR	C,J0DCA
		PUSH	BC
		EXX
		INC	(HL)
		LD	(DE),A
		INC	DE
		POP	AF
		LD	(DE),A
		INC	DE
		DJNZ	J0DB6
J0DCA:		LD	A,(D_BE00)
		OR	A
		LD	A,C
		LD	C,_NORAM
		JP	Z,J0E2F
		LD	HL,(D_BBFB)
		LD	(D_BA33),HL
		LD	A,8
		PUSH	HL
		POP	IX
		LD	(IX+8),A
		LD	(IX+9),00H
		LD	(IX+31),0FFH
		LD	C,A
		LD	IX,I_B9DA
		LD	B,00H
		CALL	C318D
		LD	A,0FFH
		CALL	C2F1A
		LD	DE,0
		CALL	C2FA9
		CALL	C2C49
		JR	J0E2D

J0E04:		LD	HL,(D_BA33)
		LD	A,H
		OR	L
		JR	Z,J0E2D
		LD	A,8
		CALL	C2C5A
		LD	HL,0
		LD	(D_BA33),HL
		XOR	A
		LD	(D_BE00),A
		LD	HL,I_BE02
J0E1D:		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		LD	A,B
		OR	A
		JR	Z,J0E2D
		PUSH	HL
		LD	A,C
		CALL	K_FRESEG
		POP	HL
		JR	J0E1D

J0E2D:		LD	C,00H
J0E2F:		LD	A,(D_BE00)
		LD	B,A
		LD	A,C
		RET

; Subroutine clear ramdisk bootsector and ramdisk segment table
C0E35:		LD	HL,I_BC00
		LD	DE,0400H
J0E3B:		LD	(HL),0
		INC	HL
		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,J0E3B
		RET

; ---------------------------------------------------------
; Function $69 _BUFFER
; ---------------------------------------------------------
F_BUFFER:	LD	A,B
		CP	02H
		JR	C,J0E86
J0E49:		LD	A,B
		CP	(IY+122)
		JR	Z,J0E86
		JR	NC,J0E6E
		LD	HL,(D_BBF8)
		CALL	C2D0F
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		DEC	HL
		LD	(D_BBF8),DE
		CALL	K_FREE_P2
		LD	HL,0
		LD	(D_BBF6),HL
		LD	HL,I_BBFA
		DEC	(HL)
		JR	J0E49

J0E6E:		LD	HL,512+11
		CALL	K_ALLOC_P2
		JR	NZ,J0E86
		LD	DE,(D_BBF8)
		LD	(D_BBF8),HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		LD	HL,I_BBFA
		INC	(HL)
		JR	J0E49

J0E86:		LD	B,(IY+122)
		XOR	A
		RET

; ---------------------------------------------------------
; Function $6A _ASSIGN
; ---------------------------------------------------------
F_ASSIGN:	LD	HL,I_BA1A
		LD	A,B
		OR	A
		JR	Z,J0EAC
		CP	09H
		LD	A,_IDRV
		RET	NC
		LD	C,B
		LD	B,0
		ADD	HL,BC
		LD	A,D
		INC	A
		JR	Z,J0EA9
		DEC	A
		JR	Z,J0EA8
		CP	09H
		LD	A,_IDRV
		RET	NC
		LD	C,D
J0EA8:		LD	(HL),C
J0EA9:		LD	D,(HL)
		XOR	A
		RET

J0EAC:		LD	(HL),A
		INC	HL
		INC	A
		CP	09H
		JR	NZ,J0EAC
		XOR	A
		LD	D,A
		RET

; ---------------------------------------------------------
; Function $6E _DSKCHK
; ---------------------------------------------------------
F_DSKCHK:	OR	A
		JR	Z,J0EC2
		LD	A,B
		OR	A
		JR	Z,J0EBF
		LD	A,0FFH
J0EBF:		LD	(DSK_CHK),A
J0EC2:		LD	A,(DSK_CHK)
		LD	B,A
		XOR	A
		RET

; ---------------------------------------------------------
; Function $6F _DOSVER
; ---------------------------------------------------------
F_DOSVER:	LD	B,02H
	IFDEF DOSV231
		LD	C,31H		; MSXDOS version 2.31
	ELSE
		LD	C,20H		; MSXDOS version 2.20
	ENDIF
		XOR	A
		LD	H,A
		LD	L,A
		LD	D,A
		LD	E,A
		RET

; ---------------------------------------------------------
; Function $70 _REDIR
; ---------------------------------------------------------
F_REDIR:	LD	C,(IY+9)
		OR	A
		JR	Z,J0EDB
		LD	(IY+9),B
J0EDB:		LD	B,C
		XOR	A
		RET

; ---------------------------------------------------------
; Function $6B _GENV
; Input:  HL = pointer to ASCIIZ environment name string
;         DE = pointer to buffer for value
;         B  = size of buffer
; Output: A  = error
;         DE = preserved, buffer filled in if A=0
; ---------------------------------------------------------
F_GENV:		XOR	A

; Subroutine get environment
; Input:  A = segment type (0 = TPA, A<>0 = current)
C0EDF:		LD	(D_BBED),A
		XOR	A
		PUSH	BC
		CALL	C0FA2
		POP	BC
		RET	NZ
		PUSH	DE
		PUSH	BC
		LD	DE,D_BBEE
		CALL	C0F8B
		LD	DE,I0F76
		JR	NC,J0EF8
		LD	D,B
		LD	E,C
J0EF8:		POP	BC
		POP	HL
		CALL	C0FC8
		EX	DE,HL
		RET

; ---------------------------------------------------------
; Function $6C _SENV
; Input:  HL = pointer to ASCIIZ environment name string
;         DE = pointer to ASCIIZ value string
; Output: A  = error
; ---------------------------------------------------------
F_SENV:		XOR	A
		LD	(D_BBED),A
		XOR	A
		CALL	C0FA2
		RET	NZ
		LD	A,B
		OR	A
		RET	Z
		EX	AF,AF'
		EX	DE,HL
		LD	A,0FFH
		CALL	C0FA2
		RET	NZ
		LD	A,B
		OR	A
		JR	Z,J0F46
		EX	AF,AF'
		ADD	A,B
		LD	C,A
		LD	A,00H
		ADC	A,A
		LD	B,A
		PUSH	HL
		LD	HL,4
		ADD	HL,BC
		CALL	K_ALLOC_P2
		POP	BC
		RET	NZ
		PUSH	BC
		LD	BC,(D_BBEE)
		LD	(D_BBEE),HL
		LD	(HL),C
		INC	HL
		LD	(HL),B
		INC	HL
		EX	DE,HL
		XOR	A
		CALL	C0FDF
		EX	(SP),HL
		LD	A,0FFH
		CALL	C0FDF
		POP	HL
		LD	DE,(D_BBEE)
		JR	J0F4A

J0F46:		EX	DE,HL
		LD	DE,D_BBEE
J0F4A:		CALL	C0F8B
		LD	HL,D_BBEE
		CALL	C,C2188
		XOR	A
		RET

; ---------------------------------------------------------
; Function $6D _FENV
; Input:  DE = environment item number
;         HL = pointer to buffer for name string
; Output: A  = error
;         HL = preserved, buffer filled in
; ---------------------------------------------------------
F_FENV:		XOR	A
		LD	(D_BBED),A
		PUSH	HL
		PUSH	BC
		LD	B,D
		LD	C,E
		LD	HL,(D_BBEE)
J0F60:		LD	A,H
		OR	L
		LD	DE,I0F76
		JR	Z,J0F71
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		EX	DE,HL
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,J0F60
J0F71:		POP	BC
		POP	HL
		JP	C0FC8

I0F76:		DEFW	0

	IF OPTM = 0
		LD	HL,(D_BBEE)
J0F7B:		LD	A,H
		OR	L
		RET	Z
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		DEC	HL
		CALL	K_FREE_P2
		EX	DE,HL
		LD	(D_BBEE),HL
		JR	J0F7B
	ENDIF

; Subroutine search for environment
; Outputs Cx set if found
C0F8B:		EX	DE,HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	H
		EX	DE,HL
		RET	Z
		PUSH	DE
		PUSH	HL
		INC	DE
		INC	DE
		CALL	C0FF1
		LD	B,D
		LD	C,E
		POP	HL
		POP	DE
		JR	NZ,C0F8B
		SCF
		RET

; Subroutine validate environment string
C0FA2:		PUSH	HL
		AND	01H
		LD	C,A
		LD	B,0FFH
J0FA8:		CALL	C1003
		INC	HL
		CALL	C17AE
		JR	Z,J0FC1
		BIT	0,C
		JR	NZ,J0FBB
		BIT	4,C
		LD	A,_IENV
		JR	NZ,J0FC5
J0FBB:		DJNZ	J0FA8
		LD	A,_ELONG
		JR	J0FC5

J0FC1:		DEC	A
		SUB	B
		LD	B,A
		XOR	A
J0FC5:		POP	HL
		OR	A
		RET

; Subroutine copy from environment to buffer
C0FC8:		PUSH	HL
		PUSH	DE
J0FCA:		LD	A,B
		DEC	B
		OR	A
		LD	A,_ELONG
		JR	Z,J0FDB
		LD	A,(DE)
		CALL	C1012
		INC	HL
		LD	A,(DE)
		INC	DE
		OR	A
		JR	NZ,J0FCA
J0FDB:		POP	DE
		POP	HL
		OR	A
		RET

; Subroutine copy from buffer to environment
C0FDF:		PUSH	HL
		AND	01H
		LD	C,A
J0FE3:		CALL	C1003
		INC	HL
		CALL	C17AE
		LD	(DE),A
		INC	DE
		OR	A
		JR	NZ,J0FE3
		POP	HL
		RET

; Subroutine check if environment
C0FF1:		LD	C,00H
J0FF3:		CALL	C1003
		INC	HL
		CALL	C17AE
		LD	B,A
		LD	A,(DE)
		INC	DE
		CP	B
		RET	NZ
		OR	A
		JR	NZ,J0FF3
		RET

; Subroutine read byte (environment)
C1003:		PUSH	HL
		EX	DE,HL
		LD	A,(D_BBED)
		CALL	C2731
		EX	DE,HL
		CALL	RD_SEG
		EI
		POP	HL
		RET

; Subroutine write byte (environment)
C1012:		PUSH	HL
		PUSH	DE
		LD	E,A
		EX	DE,HL
		LD	A,(D_BBED)
		CALL	C2731
		EX	DE,HL
		CALL	WR_SEG
		EI
		POP	DE
		POP	HL
		RET

; ---------------------------------------------------------
; Function $2A _GDATE
; ---------------------------------------------------------
F_GDATE:	CALL	C111B
		LD	C,D
		LD	B,00H
		LD	E,L
		LD	D,H
		LD	HL,1980		; year base is 1980
		ADD	HL,BC
		LD	A,D
		CP	03H
		LD	A,C
		SBC	A,0FCH
		AND	0FCH
		RRCA
		RRCA
		ADD	A,C
		PUSH	HL
		LD	HL,I104D-1
		LD	C,D
		ADD	HL,BC
		ADD	A,(HL)
		POP	HL
		ADD	A,E
J1044:		SUB	07H
		JR	NC,J1044
		ADD	A,7
		LD	C,A
		XOR	A
		RET

I104D:		DEFB	1,4,4,7,9,12,14,17,20,22,25,27

; ---------------------------------------------------------
; Function $2B _SDATE
; ---------------------------------------------------------
F_SDATE:	LD	BC,-1980	; year base is 1980
		ADD	HL,BC
		JR	NC,J1091
		LD	A,H
		OR	A
		JR	NZ,J1091
		LD	A,L
		CP	100		; year < 2080 ?
		JR	NC,J1091
		LD	B,A
		LD	A,D
		DEC	A
		CP	12		; valid month?
		JR	NC,J1091
		LD	HL,I1096
		ADD	A,L
		LD	L,A
		JR	NC,J1077
		INC	H
J1077:		CP	97H
		JR	NZ,J1083
		LD	A,B
		AND	03H
		JR	NZ,J1083
		LD	HL,I10A2
J1083:		LD	A,E
		DEC	A
		CP	(HL)
		JR	NC,J1091
		LD	L,E
		LD	H,D
		LD	D,B
		CALL	C1167
		XOR	A
		LD	C,A
		RET

J1091:		LD	C,0FFH
		LD	A,_IDATE
		RET

I1096:		DEFB	31,28,31,30,31,30,31,31,30,31,30,31
I10A2:		DEFB	29

; ---------------------------------------------------------
; Function $2C _GTIME
; ---------------------------------------------------------
F_GTIME:	CALL	C111B
		LD	H,B
		LD	L,C
		LD	D,E
		LD	E,00H
		XOR	A
		RET

; ---------------------------------------------------------
; Function $2D _STIME
; ---------------------------------------------------------
F_STIME:	LD	A,H
		CP	24			; 24 hours
		JR	NC,J10C5
		LD	A,L
		CP	60			; 60 minutes
		JR	NC,J10C5
		LD	A,D
		CP	60			; 60 seconds
		JR	NC,J10C5
		LD	B,H
		LD	C,L
		LD	E,D
		CALL	C1155
		XOR	A
		LD	C,A
		RET

J10C5:		LD	C,0FFH
		LD	A,_ITIME
		RET

; ---------------------------------------------------------
; Subroutine initialize clockchip
C10CA:		LD	A,13
		OUT	(0B4H),A
		IN	A,(0B5H)
		AND	04H
		LD	B,A
		INC	A
		OUT	(0B5H),A
		LD	A,10
		OUT	(0B4H),A
		LD	A,1
		OUT	(0B5H),A
		LD	A,13
		OUT	(0B4H),A
		LD	A,B
		OUT	(0B5H),A
		LD	BC,00D00H
J10E8:		LD	A,C
		OUT	(0B4H),A
		IN	A,(0B5H)
		PUSH	AF
		INC	C
		DJNZ	J10E8
		LD	A,14
		OUT	(0B4H),A
		LD	A,00H
		OUT	(0B5H),A
		LD	B,13
J10FB:		DEC	C
		POP	DE
		LD	A,C
		OUT	(0B4H),A
		LD	A,D
		OUT	(0B5H),A
		DJNZ	J10FB

; Subroutine resume real time clock
J1105:		LD	A,13
		OUT	(0B4H),A
		IN	A,(0B5H)
		OR	08H
		OUT	(0B5H),A
		RET

; Subroutine pause real time clock
C1110:		LD	A,13
		OUT	(0B4H),A
		IN	A,(0B5H)
		AND	04H
		OUT	(0B5H),A
		RET

; Subroutine read time and date from real time clock
C111B:		CALL	C1110
		LD	E,13
		CALL	C113C
		LD	D,A
		CALL	C113C
		LD	H,A
		CALL	C113C
		LD	L,A
		DEC	E
		CALL	C113C
		LD	B,A
		CALL	C113C
		LD	C,A
		CALL	C113C
		LD	E,A
		JP	J1105

; Subroutine read byte (BCD) from real time clock
; Input:  E = register+1
; Output: E = register-1
C113C:		PUSH	BC
		CALL	C114C
		LD	B,A
		ADD	A,A
		ADD	A,A
		ADD	A,B
		ADD	A,A
		LD	B,A
		CALL	C114C
		ADD	A,B
		POP	BC
		RET

; Subroutine read nibble from real time clock
; Input:  E = register+1
; Output: E = register
C114C:		DEC	E
		LD	A,E
		OUT	(0B4H),A
		IN	A,(0B5H)
		AND	0FH
		RET

; Subroutine write hour,minute and second to real time clock
C1155:		LD	L,E
		LD	H,C
		LD	D,B
		CALL	C1110
		LD	A,15
		OUT	(0B4H),A
		LD	A,2
		OUT	(0B5H),A
		LD	E,00H
		JR	J117D

; Subroutine write year,month and day to real time clock
C1167:		CALL	C1110
		OR	01H
		OUT	(0B5H),A
		LD	A,11
		OUT	(0B4H),A
		LD	A,D
		OUT	(0B5H),A
		CALL	C1110
		CALL	C1110
		LD	E,7
J117D:		LD	A,L
		CALL	C118C
		LD	A,H
		CALL	C118C
		LD	A,D
		CALL	C118C
		JP	J1105

; Subroutine convert byte to BCD and write to real time clock
C118C:		LD	C,A
		XOR	A
		LD	B,8
J1190:		RLC	C
		ADC	A,A
		DAA
		DJNZ	J1190
		CALL	C119D
		RRCA
		RRCA
		RRCA
		RRCA

; Subroutine write nibble to real time clock
C119D:		LD	B,A
		LD	A,E
		OUT	(0B4H),A
		LD	A,B
		OUT	(0B5H),A
		INC	E
		RET

; ---------------------------------------------------------
; *** K_ALLSEG ***
; ---------------------------------------------------------
K_ALLSEG:	OR	A
		LD	A,(D_BBFE)
		JR	Z,J11AE
		LD	A,0FFH
J11AE:		EX	AF,AF'
		LD	C,B
		LD	A,C
		AND	8FH
		JR	NZ,J11BA
		LD	A,(RAMAD3)
		OR	C
		LD	C,A
J11BA:		LD	A,C
		AND	70H
		JR	NZ,J11C1
		JR	C1206

J11C1:		LD	B,C
		CP	20H
		JR	NZ,J11CB
		CALL	C1206
		JR	NC,J11FF
J11CB:		XOR	A
		LD	HL,EXPTBL
J11CF:		BIT	7,(HL)
		JR	Z,J11D5
		SET	7,A
J11D5:		LD	C,A
		XOR	B
		AND	8FH
		JR	Z,J11E2
		PUSH	HL
		CALL	C1206
		POP	HL
		JR	NC,J11FF
J11E2:		LD	A,C
		BIT	7,A
		JR	Z,J11ED
		ADD	A,4
		BIT	4,A
		JR	Z,J11D5
J11ED:		INC	HL
		INC	A
		AND	03H
		JR	NZ,J11CF
		LD	A,B
		AND	70H
		CP	30H
		SCF
		JR	NZ,J11FF
		LD	C,B
		CALL	C1206
J11FF:		PUSH	AF
		LD	A,C
		AND	8FH
		LD	B,A
		POP	AF
		RET

; Subroutine allocate segment of the specified slot
C1206:		PUSH	BC
		LD	A,C
		AND	0FH
		ADD	A,A
		ADD	A,A
		LD	E,A
		LD	D,00H
		LD	HL,I_BA35
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	H
		JR	Z,J1253
		LD	A,(DE)
		INC	DE
		LD	C,A
		EX	AF,AF'
		LD	B,A
		EX	AF,AF'
		INC	B
		JR	Z,J123B
		LD	B,00H
J1229:		LD	A,(HL)
		OR	A
		JR	Z,J1234
		INC	B
		INC	HL
		DEC	C
		JR	NZ,J1229
		JR	J1253

J1234:		EX	DE,HL
		DEC	(HL)
		INC	HL
		INC	HL
		INC	(HL)
		JR	J124C

J123B:		ADD	HL,BC
J123C:		DEC	HL
		LD	A,(HL)
		OR	A
		JR	Z,J1246
		DEC	C
		JR	NZ,J123C
		JR	J1253

J1246:		LD	B,C
		DEC	B
		EX	DE,HL
		DEC	(HL)
		INC	HL
		INC	(HL)
J124C:		EX	AF,AF'
		LD	(DE),A
		EX	AF,AF'
		LD	A,B
		POP	BC
		OR	A
		RET

J1253:		POP	BC
		SCF
		RET

; ---------------------------------------------------------
; *** K_FRESEG ***
; ---------------------------------------------------------
K_FRESEG:	LD	C,A
		LD	A,B
		AND	8FH
		JR	NZ,J125F
		LD	A,(RAMAD3)
J125F:		AND	0FH
		ADD	A,A
		ADD	A,A
		LD	E,A
		LD	D,00H
		LD	HL,I_BA35
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	H
		JR	Z,J128E
		LD	A,(DE)
		CP	C
		JR	C,J128E
		JR	Z,J128E
		LD	B,0
		ADD	HL,BC
		LD	A,(HL)
		OR	A
		JR	Z,J128E
		LD	(HL),B
		EX	DE,HL
		INC	HL
		INC	(HL)
		INC	HL
		INC	A
		JR	Z,J128B
		INC	HL
J128B:		DEC	(HL)
		OR	A
		RET

J128E:		SCF
		RET

; Subroutine free user segments
; Input:  B = proces id
C1290:		LD	C,10H
		LD	HL,I_BA35
J1295:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		PUSH	DE
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		EX	(SP),HL
		LD	A,H
		OR	L
		JR	Z,J12BE
		PUSH	BC
		LD	C,(HL)
J12A5:		LD	A,(DE)
		INC	A
		JR	Z,J12B9
		DEC	A
		JR	Z,J12B9
		DEC	A
		CP	B
		JR	C,J12B9
		PUSH	HL
		XOR	A
		LD	(DE),A
		INC	HL
		INC	(HL)
		INC	HL
		INC	HL
		DEC	(HL)
		POP	HL
J12B9:		INC	DE
		DEC	C
		JR	NZ,J12A5
		POP	BC
J12BE:		POP	HL
		DEC	C
		JR	NZ,J1295
		RET

; ---------------------------------------------------------
; *** Subroutines: parser  ***
; ---------------------------------------------------------

; ---------------------------------------------------------
; Subroutine parse path
; Input:  A  = drive id
;         B  = attributes
;         C  = parse flag
; ---------------------------------------------------------
C12C3:		LD	(IX+0),0FFH
		LD	(IX+31),B
		LD	(IY+32),C
		LD	(D_BB9E),DE
		OR	A
		JR	NZ,J12D7
		LD	A,(CUR_DRV)
J12D7:		LD	D,A
		CALL	C13BC
		OR	A
		JR	Z,J12E9
		CP	D
		JR	Z,J12E9
		LD	D,A
		BIT	7,(IY+32)
		LD	A,_IDRV
		RET	NZ
J12E9:		LD	(IX+25),D
		BIT	3,(IX+31)
		JR	NZ,J12FF
		CALL	C1782
		JR	Z,J1304
		CP	5CH
		JR	NZ,J1304
		SET	0,B
		SET	1,B
J12FF:		SET	5,(IY+32)
		XOR	A
J1304:		CALL	NZ,C179C
		LD	DE,(D_BB9E)
		LD	(D_BB9C),DE
		CALL	C16BC
		BIT	3,(IX+31)
		JR	Z,J1320
		LD	DE,I_B926
		CALL	C13E9
		JR	J1391

J1320:		LD	DE,I_B926
		CALL	C13FF
		CP	5CH
		JR	NZ,J1366
		SET	1,B
		CALL	C1782
		LD	DE,(D_BB9E)
		LD	(D_BB9C),DE
		LD	DE,I_B926
		CALL	C14CB
		JR	NZ,J13A6
		LD	DE,I_B926
		CALL	C16E7
		JR	Z,J1320
		JR	J13A6

; ---------------------------------------------------------
; Subroutine parse file name
; Input:  B = attributes
;         C = parse flags
; ---------------------------------------------------------
C1349:		LD	(IX+0),0FFH
		LD	(IX+31),B
		LD	(IX+25),A
		LD	(IY+32),C
		LD	(D_BB9E),DE
		LD	(D_BB9C),DE
		LD	B,00H
		LD	DE,I_B926
		CALL	C13FF
J1366:		LD	A,B
		AND	18H
		JR	NZ,J1383
		BIT	1,(IY+32)
		JR	Z,J1383
		PUSH	HL
		PUSH	BC
		LD	HL,I13B1
		LD	DE,I_B926
		LD	BC,11
		LDIR
		POP	AF
		OR	39H
		LD	B,A
		POP	HL
J1383:		XOR	A
		BIT	0,(IY+32)
		LD	DE,I_B926
		CALL	Z,C14F4
		OR	A
		JR	NZ,J139D
J1391:		SET	4,(IY+32)
		LD	DE,I_B926
		CALL	C14CB
		JR	NZ,J13A6
J139D:		LD	(IX+30),A
		LD	DE,I_B926
		CALL	C16E7
J13A6:		PUSH	AF
		CALL	C1782
		CALL	NZ,C179C
		LD	C,A
		POP	AF
		OR	A
		RET

I13B1:		DEFB	"???????????"

; ---------------------------------------------------------
; Subroutine try to parse drive indicator
; ---------------------------------------------------------
C13BC:		LD	(IY+33),00H
		CALL	C1782
		JR	Z,J13E6
		BIT	1,(IY+33)
		JR	NZ,J13E3
		SUB	41H
		JR	C,J13E3
		CP	1AH
		JR	NC,J13E3
		INC	A
		LD	B,A
		CALL	C1782
		JR	Z,J13E3
		CP	3AH
		LD	A,B
		LD	B,4
		RET	Z
		CALL	C179C
J13E3:		CALL	C179C
J13E6:		XOR	A
		LD	B,A
		RET

; ---------------------------------------------------------
; Subroutine parse volume name
; ---------------------------------------------------------
C13E9:		PUSH	HL
		EX	DE,HL
		LD	A,B
		AND	07H
		LD	B,A
		LD	(IY+33),09H
		LD	C,11
		CALL	C146C
		DEC	D
		JR	NZ,J13FD
		SET	3,B
J13FD:		POP	HL
		RET

; ---------------------------------------------------------
; Subroutine parse file name
; ---------------------------------------------------------
C13FF:		PUSH	HL
		EX	DE,HL
		LD	A,B
		AND	07H
		LD	B,A
		LD	(IY+33),00H
		LD	C,8
		CALL	C1782
		JR	Z,J1454
		CP	2EH
		JR	NZ,J1451
		LD	D,1
		CALL	C1782
		JR	Z,J143A
		BIT	4,(IY+33)
		JR	Z,J144E
		CP	2EH
		JR	NZ,J1437
		SET	7,B
		INC	D
		CALL	C1782
		JR	Z,J143A
		BIT	4,(IY+33)
		JR	Z,J1449
		CP	2EH
		JR	Z,J1449
J1437:		CALL	C179C
J143A:		LD	(HL),2EH
		INC	HL
		DEC	C
		DEC	D
		JR	NZ,J143A
		SET	6,B
		SET	3,B
		SET	0,B
		JR	J1454

J1449:		RES	7,B
		CALL	C179C
J144E:		CALL	C179C
J1451:		CALL	C179C
J1454:		CALL	C146C
		DEC	D
		JR	NZ,J145C
		SET	3,B
J145C:		CP	2EH
		JR	NZ,J1465
		SET	4,B
		CALL	C1782
J1465:		LD	C,3
		CALL	C146C
		POP	HL
		RET

; ---------------------------------------------------------
; Subroutine parse name
; ---------------------------------------------------------
C146C:		LD	D,00H
		INC	C
		CALL	C1782
		JR	Z,J14C4
		CALL	C179C
		CP	20H
		JR	Z,J14C4
		DEC	C
J147C:		INC	C
J147D:		CALL	C1782
		JR	Z,J14C4
		BIT	1,(IY+33)
		JR	Z,J1490
		DEC	C
		DEC	C
		JR	NZ,J148E
		LD	A,20H
J148E:		INC	C
		INC	C
J1490:		BIT	4,(IY+33)
		JR	NZ,J14C1
		BIT	3,(IY+33)
		JR	NZ,J14AC
		BIT	2,(IY+33)
		JR	NZ,J14AC
		CP	2AH
		JR	Z,J14B7
		CP	3FH
		JR	NZ,J14AC
J14AA:		SET	5,B
J14AC:		SET	0,B
		LD	D,1
		DEC	C
		JR	Z,J147C
		LD	(HL),A
		INC	HL
		JR	J147D

J14B7:		LD	A,C
J14B8:		LD	C,A
		DEC	A
		JR	Z,J14AA
		LD	(HL),3FH
		INC	HL
		JR	J14B8

J14C1:		CALL	C179C
J14C4:		DEC	C
		RET	Z
		LD	(HL),20H
		INC	HL
		JR	J14C4

; ---------------------------------------------------------
; Subroutine next item
; ---------------------------------------------------------
C14CB:		XOR	A
		BIT	3,(IY+32)
		RET	Z
		PUSH	DE
		BIT	6,(IY+32)
		SET	6,(IY+32)
		CALL	Z,C151A
		POP	DE
		OR	A
		RET	NZ
		BIT	4,(IY+32)
		JR	Z,J14EF
		BIT	0,(IY+32)
		RET	Z
		LD	A,B
		AND	18H
		RET	Z
J14EF:		CALL	C15A6
		OR	A
		RET

; ---------------------------------------------------------
; Subroutine check if device and get device flags
; Output: A = device flags
; ---------------------------------------------------------
C14F4:		PUSH	BC
		PUSH	HL
		CALL	C1698
		LD	A,00H
		JR	NC,J1517
		POP	AF
		LD	(IX+26),L
		LD	(IX+27),H
		PUSH	HL
	IF OPTM = 0
		LD	BC,0
		ADD	HL,BC
	ENDIF
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		LD	(IX+28),C
		LD	(IX+29),B
		LD	BC,7
		ADD	HL,BC
		LD	A,(HL)
J1517:		POP	HL
		POP	BC
		RET

; ---------------------------------------------------------
; Subroutine first directory
; ---------------------------------------------------------
C151A:		PUSH	BC
		LD	C,(IX+25)
		LD	B,00H
		CALL	C318D
		POP	BC
		OR	A
		RET	NZ
		PUSH	BC
		PUSH	HL
		BIT	5,(IY+32)
		JR	NZ,J1597
		LD	DE,30
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
	IFDEF FAT16
		CALL	CLST_1
	ELSE
		LD	D,(HL)
		BIT	7,D
	ENDIF
		JR	NZ,J1597
		LD	BC,(D_BB9E)
		PUSH	BC
		PUSH	HL
		LD	A,D
		OR	E
		JR	Z,J1547
		RES	3,(IY+32)
J1547:		INC	HL
		LD	(D_BB9E),HL
		LD	BC,-32
		ADD	HL,BC
		PUSH	DE
		CALL	C159F
		POP	DE
		LD	B,00H
J1556:		PUSH	DE
		LD	DE,I_B910
		CALL	C13FF
		BIT	3,(IY+32)
		JR	Z,J156E
		LD	DE,I_B910
		CALL	C15A6
		OR	A
		JR	NZ,J157E
		POP	AF
		PUSH	DE
J156E:		LD	DE,I_B910
		CALL	C16E7
		JR	NZ,J157E
		POP	DE
		CALL	C1782
		JR	NZ,J1556
		JR	J1582

J157E:		POP	DE
		LD	DE,0FFFFH
J1582:		POP	HL
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		POP	BC
		LD	(D_BB9E),BC
	IFDEF FAT16
		CALL	CLST_2
	ELSE
		SET	3,(IY+32)
		BIT	7,D
	ENDIF
		JR	Z,J1597
		INC	HL
		INC	HL
		LD	(HL),00H
J1597:		POP	HL
		POP	BC
		JR	NZ,C159F
		CALL	C1C40
		RET

; ---------------------------------------------------------
; Subroutine initialize whole path buffer and select root directory
; ---------------------------------------------------------
C159F:		CALL	C16BC
		CALL	C1C3D
		RET

; ---------------------------------------------------------
; Subroutine next directory
; ---------------------------------------------------------
C15A6:		LD	A,_IPATH
		BIT	3,B
		RET	Z
		BIT	5,B
		RET	NZ
		PUSH	DE
		CALL	C1C53
J15B2:		JR	Z,J15C9
		EX	(SP),HL
		PUSH	HL
		PUSH	BC
		XOR	A
		CALL	C15D2
		POP	BC
		POP	HL
		EX	(SP),HL
		JR	NC,J15C4
		BIT	4,A
		JR	NZ,J15CD
J15C4:		CALL	C1C8A
		JR	NC,J15B2
J15C9:		POP	DE
		LD	A,_NODIR
		RET

J15CD:		POP	AF
		CALL	C1C30
		RET

; ---------------------------------------------------------
; Subroutine does directory entry match the search
; Input:  DE    = pointer to directory entry
;         HL    = pointer to search string
;         A(b3) = volume
; Output: Cx set if match
;         Zx set if free entry or matched
; ---------------------------------------------------------
C15D2:		AND	08H
		LD	C,A
		LD	A,(DE)
		OR	A
		RET	Z
		CP	0E5H
		RET	Z
		PUSH	DE
		LD	B,11
		CP	05H
		JR	NZ,J15E4
		LD	A,0E5H
J15E4:		PUSH	AF
		LD	A,(HL)
		CALL	C17AE
		POP	AF
		BIT	3,C
		JR	NZ,J15FA
		SUB	(HL)
		JR	Z,J15FA
		BIT	2,C
		JR	NZ,J1609
		LD	A,(HL)
		SUB	3FH
		JR	NZ,J1609
J15FA:		INC	HL
		INC	DE
		LD	A,(DE)
		DJNZ	J15E4
		EX	DE,HL
		LD	A,C
		XOR	(HL)
		AND	08H
		JR	NZ,J1609
		LD	A,(HL)
		SCF
		DEFB	006H
J1609:		OR	A
		POP	DE
		RET

; ---------------------------------------------------------
; Subroutine copy name and expand wildcard
; Input:  HL = source
;         DE = current directory entry
;         BC = destination
; ---------------------------------------------------------
C160C:		PUSH	BC
		EX	(SP),IX
		LD	BC,11*256+0
		LD	A,(DE)
		CP	05H
		JR	NZ,J1619
		LD	A,0E5H
J1619:		PUSH	DE
		LD	D,A
		LD	A,(HL)
		CALL	C17AE
		BIT	2,C
		JR	NZ,J1628
		CP	3FH
		JR	NZ,J1628
		LD	A,D
J1628:		LD	(IX+0),A
		POP	DE
		INC	DE
		INC	HL
		INC	IX
		LD	A,(DE)
		DJNZ	J1619
		EX	(SP),IX
		POP	BC
		RET

; ---------------------------------------------------------
; Subroutine test name
; ---------------------------------------------------------
C1637:		PUSH	AF
		CALL	C2535
		POP	BC
		JR	NC,J1640
		XOR	A
		RET

J1640:		PUSH	HL
		PUSH	DE
		LD	A,B
		OR	A
		CALL	NZ,C1698
		POP	HL
		LD	A,_IDEV
		JR	C,J1666
		LD	BC,0B09H
		BIT	3,(IX+31)
		JR	NZ,J1662
		LD	BC,0800H
		LD	A,20H
		CALL	C1669
		JR	NZ,J1666
		LD	BC,0300H
J1662:		XOR	A
		CALL	C1669
J1666:		POP	HL
		OR	A
		RET

; ---------------------------------------------------------
; Subroutine validate name
; Input:  HL = pointer
;         A  = end marker
;         B  = maximum length
;         C  = character flags
; ---------------------------------------------------------
C1669:		CP	(HL)
		JR	Z,J1694
J166C:		LD	A,(HL)
		CALL	C17AE
		LD	(HL),A
		INC	HL
		BIT	4,C
		JR	NZ,J168C
		BIT	2,C
		JR	NZ,J1686
		BIT	3,C
		JR	NZ,J1686
		CP	3FH
		JR	Z,J1694
		CP	2AH
		JR	Z,J1694
J1686:		DJNZ	J166C
		JR	J1692

J168A:		LD	A,(HL)
		INC	HL
J168C:		CP	20H
		JR	NZ,J1694
		DJNZ	J168A
J1692:		XOR	A
		RET

J1694:		LD	A,_IFNM
		OR	A
		RET

; ---------------------------------------------------------
; Subroutine check if device name
; Input:  DE = pointer to string
; Output: Cx = set if device name, reset if no device name
; ---------------------------------------------------------
C1698:		LD	HL,(D_BBF4)
		PUSH	HL
J169C:		POP	HL
		LD	A,H
		OR	L
		RET	Z
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		PUSH	BC
		PUSH	HL
		PUSH	DE
		LD	BC,9
		ADD	HL,BC
		LD	B,8
J16AD:		LD	A,(DE)
		CP	(HL)
		JR	NZ,J16B5
		INC	DE
		INC	HL
		DJNZ	J16AD
J16B5:		POP	DE
		POP	HL
		JR	NZ,J169C
		POP	BC
		SCF
		RET

; ---------------------------------------------------------
; Subroutine initialize whole path buffer
; ---------------------------------------------------------
C16BC:		PUSH	HL
		LD	HL,ISB973
		LD	(HL),02H
		LD	HL,ISB931
		LD	(HL),00H
		INC	HL
		LD	(D_BB9A),HL
		POP	HL
		LD	(IY+25),00H

; ---------------------------------------------------------
; Subroutine terminate whole path buffer
; ---------------------------------------------------------
C16D0:		PUSH	HL
		LD	A,2
		LD	HL,(D_BB9A)
		CP	(HL)
		JR	Z,J16E3
		LD	(HL),00H
		INC	HL
		CP	(HL)
		JR	Z,J16E1
		LD	(HL),00H
J16E1:		LD	A,2AH
J16E3:		ADD	A,0D6H
		POP	HL
		RET

; ---------------------------------------------------------
; Subroutine add item to whole path buffer
; ---------------------------------------------------------
C16E7:		PUSH	BC
		PUSH	HL
		LD	HL,(D_BB9A)
		LD	A,B
		AND	18H
		JR	Z,J172F
		BIT	6,B
		JR	Z,J1706
		BIT	7,B
		JR	Z,J1723
J16F9:		DEC	HL
		LD	A,(HL)
		CP	01H
		JR	Z,J1723
		OR	A
		JR	NZ,J16F9
		LD	A,_IPATH
		JR	J172B

J1706:		PUSH	HL
		LD	HL,ISB901
		PUSH	HL
		LD	(HL),01H
		INC	HL
		LD	A,(DE)
		CALL	C173A
		POP	DE
		POP	HL
J1714:		LD	A,(HL)
		CP	02H
		LD	A,_PLONG
		JR	Z,J172B
		LD	A,(DE)
		LD	(HL),A
		INC	HL
		INC	DE
		OR	A
		JR	NZ,J1714
		DEC	HL
J1723:		LD	(D_BB9A),HL
		CALL	C16D0
		JR	Z,J172F
J172B:		LD	(IY+25),0FFH
J172F:		POP	HL
		POP	BC
		BIT	2,(IY+32)
		JR	Z,J1738
		XOR	A
J1738:		OR	A
		RET

; ---------------------------------------------------------
; Subroutine make ASCIIZ string of file or volume name
; Input:  A  = first character
;         HL = pointer to buffer
; ---------------------------------------------------------
C173A:		PUSH	BC
		PUSH	HL
		LD	B,13
J173E:		LD	(HL),00H
		INC	HL
		DJNZ	J173E
		POP	HL
		LD	BC,0B09H
		BIT	3,(IX+31)
		JR	NZ,J175A
		LD	BC,0800H
		CALL	C176A
		LD	(HL),2EH
		INC	HL
		LD	A,(DE)
		LD	BC,0300H
J175A:		CALL	C176A
		BIT	7,C
		JR	NZ,J1768
		BIT	0,C
		JR	NZ,J1768
		DEC	HL
		LD	(HL),00H
J1768:		POP	BC
		RET

; ---------------------------------------------------------
; Subroutine copy name
; Input:  DE = pointer to string
;         A  = first character
;         HL = buffer
;         B  = maximum length
;         C  = character flags
; ---------------------------------------------------------
C176A:		INC	DE
		CALL	C17AE
		BIT	2,C
		JR	NZ,J177A
		BIT	3,C
		JR	NZ,J177A
		CP	20H
		JR	Z,J177E
J177A:		SET	7,C
		LD	(HL),A
		INC	HL
J177E:		LD	A,(DE)
		DJNZ	C176A
		RET

; ---------------------------------------------------------
; Subroutine get parse string character
; ---------------------------------------------------------
C1782:		PUSH	HL
		LD	HL,(D_BB9E)
		LD	A,(HL)
		OR	A
		JR	Z,J178E
		INC	HL
		LD	(D_BB9E),HL
J178E:		POP	HL
		PUSH	BC
		LD	C,(IY+33)
		CALL	C17AE
		LD	(IY+33),C
		POP	BC
		OR	A
		RET

; ---------------------------------------------------------
; Subroutine undo get parse string character
; ---------------------------------------------------------
C179C:		PUSH	HL
		LD	HL,(D_BB9E)
		DEC	HL
		LD	(D_BB9E),HL
		RES	1,(IY+33)
		RES	2,(IY+33)
		POP	HL
		RET

; ---------------------------------------------------------
; Subroutine check character
; Input:  C  = character flags:
;              b0 = set suppress upcasing
;              b1 = set 1st double byte character
;              b2 = set 2nd double byte character
;              b3 = set volume name
; ---------------------------------------------------------
C17AE:		RES	4,C
		SET	2,C
		BIT	1,C
		RES	1,C
		JR	NZ,J17D1
		RES	2,C
		SET	1,C
		CALL	C17D6X		; if v2.31 call extra routine
		JR	C,J17D1
		RES	1,C
		BIT	0,C
		CALL	Z,C17F3
		BIT	3,C
		CALL	C1803
		JR	NC,J17D1
		SET	4,C
J17D1:		OR	A
		RET	NZ
		SET	4,C
		RET

; ---------------------------------------------------------
; Subroutine check for double byte header character if enabled
; ---------------------------------------------------------
C17D6X:
	IFDEF DOSV231
		; extra routine in DOS v2.31
		PUSH	HL
		LD	HL,I17DC
		JR	J17DD
I17DC:		DB	080H,0A0H
		DB	0E0H,0FDH
	ENDIF

C17D6:		CALL	H_16CH
		PUSH	HL
		LD	HL,KANJTA
J17DD:		CP	(HL)
		INC	HL
		JR	C,J17E4
		CP	(HL)
		JR	C,J17EC
J17E4:		INC	HL
		CP	(HL)
		JR	C,J17F0
		INC	HL
		CP	(HL)
		JR	NC,J17F0
J17EC:		OR	A
		SCF
		POP	HL
		RET

J17F0:		OR	A
		POP	HL
		RET

; ---------------------------------------------------------
; Subroutine make upcase
; ---------------------------------------------------------
C17F3:		PUSH	HL
		LD	HL,I_BA75
		CALL	H_UP
		PUSH	BC
		LD	B,0
		LD	C,A
		ADD	HL,BC
		LD	A,(HL)
		POP	BC
		POP	HL
		RET

; ---------------------------------------------------------
; Subroutine validate character
; Input:  A  = character
;         Zx = set: file name, reset: volume name
; Output: Cx set if illegal character
; ---------------------------------------------------------
C1803:		PUSH	HL
		PUSH	BC
		LD	BC,17
		LD	HL,I181C
		JR	Z,J1810
		LD	BC,6
J1810:		CP	20H
		JR	C,J1818
		CPIR
		JR	NZ,J1819
J1818:		SCF
J1819:		POP	BC
		POP	HL
		RET

I181C:		DEFB	07FH,"|<>/",0FFH," :;.,=+\\\"[]"

; ---------------------------------------------------------
; *** Functions: 40-42,59-5E ***
; ---------------------------------------------------------

; ---------------------------------------------------------
; Function $59 _GETCD
; ---------------------------------------------------------
F_GETCD:	LD	A,B
		LD	B,00H
		LD	IX,I_B9DA
		LD	C,9
		PUSH	DE
		LD	DE,I1842
		CALL	C12C3
		POP	DE
		RET	NZ
		JP	F_WPATH

I1842:		DEFB	0

; ---------------------------------------------------------
; Function $5A _CHDIR
; ---------------------------------------------------------
F_CHDIR:	XOR	A
		LD	B,A
		LD	IX,I_B9DA
		LD	C,9
		CALL	C12C3
		RET	NZ
		OR	C
		LD	A,_IPATH
		RET	NZ
		LD	BC,30
		ADD	HL,BC
		LD	DE,(D_BBE8)
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		EX	DE,HL
		JP	F_WPATH

; ---------------------------------------------------------
; Function $5B _PARSE
; ---------------------------------------------------------
F_PARSE:	LD	C,4
		LD	IX,I_B9DA
		XOR	A
		CALL	C12C3
		LD	C,(IX+25)
		LD	DE,(D_BB9E)
		LD	HL,(D_BB9C)
		RET

; ---------------------------------------------------------
; Function $5C _PFILE
; ---------------------------------------------------------
F_PFILE:	PUSH	HL
		LD	(D_BB9E),DE
		EX	DE,HL
		LD	B,00H
		CALL	C13FF
		LD	DE,(D_BB9E)
		POP	HL
		XOR	A
		RET

; ---------------------------------------------------------
; Function $5D _CHKCHR
; ---------------------------------------------------------
F_CHKCHR:	LD	A,E
		LD	C,D
		CALL	C17AE
		LD	D,C
		LD	E,A
		XOR	A
		RET

; ---------------------------------------------------------
; Function $5E _WPATH
; ---------------------------------------------------------
F_WPATH:	PUSH	DE
		LD	HL,ISB933
		LD	C,00H
		PUSH	DE
		LD	A,(DSBB99)
		OR	A
		JR	NZ,J18B7
J18A0:		LD	A,(HL)
		INC	HL
		CP	02H
		JR	Z,J18B7
		CP	01H
		JR	NZ,J18B0
		POP	AF
		LD	A,5CH
		INC	DE
		PUSH	DE
		DEC	DE
J18B0:		LD	(DE),A
		INC	DE
		OR	A
		JR	NZ,J18A0
		JR	J18B9

J18B7:		LD	A,0D8H
J18B9:		POP	HL
		POP	DE
		RET

; ---------------------------------------------------------
; Function $40 _FFIRST
; ---------------------------------------------------------
F_FFIRST:	LD	A,4
		JR	J18C1

; ---------------------------------------------------------
; Function $42 _FNEW
; ---------------------------------------------------------
F_FNEW:		XOR	A
J18C1:		LD	(DSBBAF),A
		LD	A,(DE)
		INC	A
		JR	Z,J18D5
		XOR	A
		LD	C,10
		CALL	C12C3
		RET	NZ
		OR	C
		LD	A,_IPATH
		RET	NZ
		JR	J191F

J18D5:		PUSH	HL
		PUSH	DE
		EX	(SP),IX
		CALL	C19F8
		LD	C,(IX+25)
		POP	IX
		JP	NZ,J196D
		LD	A,_IDEV
		JP	C,J196D
		PUSH	HL
		LD	HL,11
		ADD	HL,DE
		LD	A,(HL)
		POP	HL
		BIT	4,A
		LD	A,_IATTR
		JR	Z,J196D
		BIT	3,B
		JR	NZ,J196D
		CALL	C1C30
		PUSH	BC
		PUSH	HL
		LD	BC,25
		ADD	HL,BC
		PUSH	IX
		POP	DE
		EX	DE,HL
		LD	BC,26
		ADD	HL,BC
		EX	DE,HL
		LD	BC,4
		LDIR
		POP	HL
		POP	BC
		POP	DE
		LD	A,C
		LD	C,4EH
		CALL	C1349
		RET	NZ
		OR	C
		LD	A,0D9H
		RET	NZ
J191F:		BIT	2,(IY+47)
		JR	NZ,J1955
		BIT	5,B
		JR	Z,J1955
		PUSH	IX
		EX	(SP),HL
		LD	BC,1
		ADD	HL,BC
		LD	(D_BB9E),HL
		POP	HL
		LD	DE,I_B91B
		CALL	C13FF
		OR	A
		LD	A,_IFNM
		RET	NZ
		PUSH	HL
		LD	HL,I_B926
		LD	DE,I_B91B
		LD	BC,I_B926
		CALL	C160C
		POP	HL
		LD	DE,I_B926
		CALL	C14F4
		LD	(IX+30),A
J1955:		LD	DE,I_B926
		PUSH	IX
		EX	(SP),HL
		LD	BC,32
		ADD	HL,BC
		EX	DE,HL
		PUSH	DE
		LD	BC,11
		LDIR
		POP	DE
		POP	HL
		CALL	C1A49
		JR	J1983

J196D:		POP	DE
		RET

; ---------------------------------------------------------
; Function $41 _FNEXT
; ---------------------------------------------------------
F_FNEXT:	LD	(IY+47),04H
		CALL	C19F8
		RET	NZ
		PUSH	IX
		EX	(SP),HL
		LD	DE,32
		ADD	HL,DE
		EX	DE,HL
		POP	HL
		CALL	C1A87
J1983:		PUSH	AF
		CALL	Z,C198F
		POP	AF
		CALL	C1A3B
	IFDEF FAT16
		CALL	STOR_7
	ELSE
		EX	DE,HL
		LDIR
	ENDIF
		RET

; ---------------------------------------------------------
; *** Subroutines: file and directory ***
; ---------------------------------------------------------

; Subroutine update FIB with directory entry info
; Input:  IX = pointer to FIB
;         DE = pointer to directory entry
C198F:		PUSH	IX
		EX	(SP),HL
		PUSH	HL
		INC	HL
		LD	A,(DE)
		CP	05H
		JR	NZ,J199B
		LD	A,0E5H
J199B:		CALL	C173A
		POP	HL
		LD	BC,14
		ADD	HL,BC
		LD	A,(DE)
		LD	(HL),A
		EX	DE,HL
		INC	DE
		LD	BC,11
		ADD	HL,BC
		LD	BC,10
		LDIR
		POP	HL
		RET

; Subroutine open file and create or update FIB
C19B2:		LD	(IY+47),04H
		LD	B,16H
		LD	A,(DE)
		INC	A
		JR	Z,J19DE
		JR	J19C2

; Subroutine create new file and FIB
C19BE:		LD	(IY+47),00H
J19C2:		XOR	A
		LD	IX,I_B9DA
		LD	C,8
		CALL	C12C3
		RET	NZ
		OR	C
		LD	A,_IPATH
		RET	NZ
		BIT	5,B
		LD	A,_IFNM
		RET	NZ
		LD	DE,I_B926
		CALL	C1A49
		JR	J19ED

J19DE:		PUSH	DE
		POP	IX
		CALL	C19F8
		RET	NZ
		BIT	3,(IX+31)
		LD	A,_IATTR
		RET	NZ
		XOR	A
J19ED:		OR	A
		RET	NZ
		PUSH	HL
		LD	HL,11
		ADD	HL,DE
		LD	A,(HL)
		POP	HL
		CP	A
		RET

; Subroutine get directory entry from FIB info with disk change check
C19F8:		PUSH	BC
		BIT	7,(IX+30)
		JR	Z,J1A06
		CALL	C1A1B
		POP	BC
		XOR	A
		SCF
		RET

J1A06:		CALL	C1A3B
		LDIR
	IFDEF FAT16 ; STOR_8
		PUSH	AF
		LD	A,(IX+38H)
		LD	(SDIR_1),A
		POP	AF
	ENDIF
		LD	C,(IX+25)
		LD	B,1
		CALL	C318D
		POP	BC
		OR	A
		RET	NZ
		CALL	C1C70
		XOR	A
		RET

; Subroutine create fake directory for device
C1A1B:		LD	L,(IX+26)
		LD	H,(IX+27)
		PUSH	HL
		LD	DE,9
		ADD	HL,DE
		PUSH	HL
		LD	DE,22
		ADD	HL,DE
		EX	DE,HL
		PUSH	IX
		POP	HL
		LD	BC,15
		ADD	HL,BC
		LD	BC,4
		LDIR
		POP	DE
		POP	HL
		RET

; Subroutine get pointer to directory entry locators
; Input:  IX = pointer to FIB
; Output: HL = pointer to FIB directory entry locators
;         DE = pointer to directory entry locators
C1A3B:		LD	DE,D_BBDE
		PUSH	IX
		POP	HL
		LD	BC,43
		ADD	HL,BC
		LD	BC,12
		RET

; Subroutine get directory entry
; Input:  DE = file name buffer
C1A49:		BIT	2,(IY+47)
		JR	NZ,J1A64
		PUSH	DE
		LD	A,(IX+31)
		AND	10H
		CALL	C1637
		POP	DE
		RET	NZ
		BIT	7,(IX+31)
		JR	Z,J1A64
		SET	3,(IY+47)
J1A64:		RES	7,(IX+31)
		LD	(D_BBAD),DE
		BIT	7,(IX+30)
		JR	NZ,J1A7A
		CALL	C1C53
		CALL	C1BC8
		JR	J1A99

J1A7A:		PUSH	HL
		LD	DE,9
		ADD	HL,DE
		EX	DE,HL
		CALL	C1B96
		POP	HL
		JP	J1B24

; Subroutine get next directory entry
; Input:  DE = pointer to file name buffer
C1A87:		BIT	7,(IX+30)
		LD	A,_NOFIL
		RET	NZ
		SET	1,(IY+41)
		LD	(D_BBAD),DE
J1A96:		CALL	C1BC5
J1A99:		JR	C,J1AA7
		BIT	2,(IY+47)
		LD	A,0D7H
		CALL	Z,C1BFA
		RET	NZ
		JR	J1AE6

J1AA7:		BIT	2,(IY+47)
		JR	Z,J1ABC
		BIT	3,A
		JR	NZ,J1B24
		AND	16H
		CPL
		OR	(IX+31)
		INC	A
		JR	NZ,J1A96
		JR	J1B24

J1ABC:		LD	B,A
		BIT	3,(IY+47)
		JR	NZ,J1AD3
		BIT	2,B
		LD	A,_SYSX
		JR	NZ,J1B25
		BIT	4,B
		LD	A,_DIRX
		JR	NZ,J1B25
		BIT	4,(IX+31)
J1AD3:		LD	A,_FILEX
		JR	NZ,J1B25
		CALL	C1C70
		XOR	A
		CALL	C2332
		PUSH	AF
		CALL	C1C70
		POP	AF
		OR	A
		JR	NZ,J1B25
J1AE6:		LD	DE,(D_BBAD)
		CALL	C2535
		JR	NC,J1AF1
		OR	A
		RET

J1AF1:		LD	BC,0
		BIT	4,(IX+31)
		JR	Z,J1B04
		LD	A,0FFH
		INC	BC
		CALL	C2F40
		RET	NZ
		CALL	C1B27
J1B04:		CALL	C1C70
		LD	A,28H
		BIT	3,(IX+31)
		JR	NZ,J1B1C
		LD	A,(IX+31)
		SET	5,A
		BIT	4,A
		JR	Z,J1B1A
		AND	0DAH
J1B1A:		AND	3FH
J1B1C:		PUSH	HL
		LD	HL,(D_BBAD)
		CALL	C1B69
		POP	HL
J1B24:		XOR	A
J1B25:		CP	A
		RET

; Subroutine setup the first two directory entries
C1B27:		PUSH	DE
		LD	D,B
		LD	E,C
		XOR	A
		CALL	C2DB6
		PUSH	HL
		EX	(SP),IX
		PUSH	BC
		LD	B,1
	IFDEF FAT16
		CALL	BUF_2F
	ELSE
		CALL	C2B78
	ENDIF
		POP	BC
		PUSH	BC
		LD	DE,11
		ADD	HL,DE
		EX	DE,HL
	IF OPTM = 0
		LD	HL,I1B5E
	ELSE
		LD	HL,I254D
	ENDIF
		LD	A,10H
		CALL	C1B69
		LD	HL,32
		ADD	HL,DE
		EX	DE,HL
	IF OPTM = 0
		LD	HL,I1B5D
	ELSE
		LD	HL,I254C
	ENDIF
		LD	BC,(D_BBE8)
		LD	A,10H
		CALL	C1B69
		POP	BC
		EX	(SP),IX
		POP	HL
		POP	DE
		RET

	IF OPTM = 0
I1B5D:		DEFB	"."
I1B5E:  	DEFB    ".          "
	ENDIF

; Subroutine setup directory entry
; Input:  HL = pointer to file name
;         DE = pointer to directory entry
;         BC = cluster
;         A  = attribute
C1B69:		PUSH	DE
		PUSH	BC
		LD	B,A
		LD	A,(HL)
		CP	0E5H
		JR	NZ,J1B73
		LD	A,05H
J1B73:		LD	(DE),A
		LD	A,B
		INC	HL
		INC	DE
		LD	BC,10
		LDIR
		CALL	C2C38
		LD	(DE),A
		EX	DE,HL
		LD	B,14H
J1B83:		INC	HL
		LD	(HL),C
		DJNZ	J1B83
	IFDEF FAT16
		CALL	CLST_8
	ELSE
		POP	DE
		BIT	7,D
	ENDIF
		JR	Z,J1B8E
		LD	D,B
		LD	E,B
J1B8E:		LD	BC,-4
		ADD	HL,BC
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		POP	DE

; Subroutine change time and date directory entry
; Input:  DE = pointer to directory entry
C1B96:		PUSH	HL
		PUSH	BC
		PUSH	DE
		LD	HL,22
		ADD	HL,DE
		PUSH	HL
		CALL	C111B
		EX	(SP),HL
		LD	A,C
		ADD	A,A
		ADD	A,A
		LD	C,3
J1BA7:		ADD	A,A
		RL	B
		DEC	C
		JR	NZ,J1BA7
		SRL	E
		ADD	A,E
		LD	(HL),A
		INC	HL
		LD	(HL),B
		INC	HL
		POP	BC
		LD	A,B
		OR	A
		RRA
		RRA
		RRA
		RRA
		RL	D
		ADD	A,C
		LD	(HL),A
		INC	HL
		LD	(HL),D
		POP	DE
		POP	BC
		POP	HL
		RET

; Subroutine search next directory entry
C1BC5:		CALL	C1C8A

; Subroutine search directory entry
C1BC8:		JR	C,J1BE4
		PUSH	IX
		EX	(SP),HL
		LD	HL,(D_BBAD)
		LD	A,(IX+31)
		CALL	C15D2
		POP	HL
		RET	C
		JR	NZ,C1BC5
		BIT	0,(IY+47)
		CALL	Z,C1BE6
		OR	A
		JR	NZ,C1BC5
J1BE4:		XOR	A
		RET

; Subroutine register free directory entry
C1BE6:		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	C1A3B
		EX	DE,HL
	IFDEF FAT16 ; STOR_1
		PUSH	AF
		LD	A,(SDIR_1)	; BBE2h-3h
		LD	(SDIR_2),A	; BBD6h-7h
		POP	AF
	ENDIF
		LD	DE,I_BBD2
		LDIR
		SET	0,(IY+47)
		POP	BC
		POP	DE
		POP	HL
		RET

; Subroutine get free or registered directory entry
C1BFA:		PUSH	BC
		BIT	0,(IY+47)
		JR	Z,J1C0D
		PUSH	HL
		CALL	C1A3B
	IFDEF FAT16 ; STOR_2
		PUSH	AF
		LD	A,(SDIR_2)
		LD	(SDIR_1),A
		POP	AF
	ENDIF
		LD	HL,I_BBD2
		LDIR
		POP	HL
		JR	J1C2D

J1C0D:  
	IFDEF FAT16
		CALL	CLST_3
	ELSE
		LD	DE,(D_BBE6)
		BIT	7,D
	ENDIF
		LD	A,_DRFUL
		JR	NZ,J1C2E
		LD	A,0FFH
		LD	BC,1
		CALL	C2F40
		JR	NZ,J1C2E
		PUSH	BC
		CALL	C2DD3
		POP	BC
		LD	(D_BBE4),BC
		CALL	C1C8A
J1C2D:		XOR	A
J1C2E:		POP	BC
		RET

; Subroutine select sub directory
C1C30:		PUSH	HL
		LD	HL,26
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		POP	HL
		LD	A,D
		OR	E
		JR	NZ,C1C40

; Subroutine select root directory
C1C3D:		LD	DE,0FFFFH

; Subroutine select sub directory (cluster number)
C1C40:		LD	(D_BBE8),DE
		PUSH	HL
		LD	HL,(D_BBAB)
		XOR	A
		SBC	HL,DE
		JR	NZ,J1C51
		SET	1,(IY+47)
J1C51:		POP	HL
		RET

; Subroutine get first directory entry
; Input:  HL = pointer to drive table
C1C53:  
	IFDEF FAT16
		CALL	CLST_4
	ELSE
		LD	DE,(D_BBE8)
		BIT	7,D
	ENDIF
		JR	Z,J1CBA
		PUSH	BC
		PUSH	HL
		LD	BC,15
		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	A,(HL)
		INC	HL
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		POP	HL
		LD	B,0FFH
		PUSH	BC
		PUSH	BC
		JR	J1CD4

; Subroutine get current directory entry
; Input:  HL = pointer to drive table
C1C70:		PUSH	HL
		LD	DE,8
		ADD	HL,DE
		BIT	7,(HL)
		EX	DE,HL
		INC	DE
		POP	HL
		JP	NZ,J1D1F
		SET	1,(IY+41)
		INC	(IY+96)
		PUSH	BC
		LD	BC,0
		JR	J1C8E

; Subroutine get next directory entry
; Input:  HL = pointer to drive table, DE = pointer to current directory entry
C1C8A:		PUSH	BC
		LD	BC,32
J1C8E:		PUSH	HL
		EX	DE,HL
		ADD	HL,BC
		LD	DE,(D_BBE2)
		LD	A,(D_BBE0)
		DEC	A
		JR	Z,J1CA4
		BIT	1,(IY+41)
		JP	Z,J1D19
		JR	J1CF9

J1CA4:		POP	HL
		INC	DE
	IFDEF FAT16 ; SUB_16
		LD	A,D
		OR	E
		JR	NZ,SUB_
		LD	A,(SDIR_1)
		INC	A
		LD	(SDIR_1),A
	ENDIF
SUB_:		LD	A,(D_BBE1)
		DEC	A
		JR	NZ,J1CE1
		CP	(IY+94)
		JR	NZ,J1CE3
	IFDEF FAT16
		CALL	CLST_9
	ELSE
		LD	DE,(D_BBE4)
		BIT	7,D
	ENDIF
		SCF
		POP	BC
		RET	NZ
J1CBA:		PUSH	BC
		PUSH	DE
		CALL	C2D86
		LD	B,D
		LD	C,E
		POP	DE
		PUSH	DE
		XOR	A
		CALL	C2DB6
	IFDEF FAT16 ; GETSUB
		LD	A,(BIT16)
		LD	(SDIR_1),A
	ENDIF
		PUSH	DE
		EX	DE,HL
		LD	HL,10
		ADD	HL,DE
		LD	A,(HL)
		INC	A
		EX	DE,HL
		POP	DE
		PUSH	BC
		LD	C,00H
J1CD4:		LD	(IY+94),C
		POP	BC
		LD	(D_BBE4),BC
		POP	BC
		LD	(D_BBE6),BC
J1CE1:		LD	B,10H
J1CE3:		OR	A
		JR	NZ,J1CED
		LD	B,(IY+94)
		LD	(D_BBDE),A
		INC	A
J1CED:		LD	(D_BBE1),A
		LD	(D_BBE2),DE
		LD	(IY+95),B
		LD	A,B
		PUSH	HL
J1CF9:		EX	(SP),IX
		PUSH	AF
		LD	B,1
	IFDEF FAT16
		CALL	BUF_4F
	ELSE
		CALL	C2B78
	ENDIF
		POP	BC
		EX	(SP),IX
		LD	DE,-21
		ADD	HL,DE
		LD	A,(D_BBDF)
		INC	A
		SUB	B
		LD	DE,32
J1D10:		ADD	HL,DE
		DEC	A
		JR	NZ,J1D10
		LD	A,B
		RES	1,(IY+41)
J1D19:		LD	(D_BBE0),A
		EX	DE,HL
		POP	HL
		POP	BC
J1D1F:		LD	A,(DE)
		OR	A
		RET

; Subroutine write to file handle
C1D22:		EX	AF,AF'
		CALL	C2136
		RET	NC
		RET	Z
		BIT	7,(IX+30)
		JR	Z,J1D39
		LD	L,(IX+28)
		LD	H,(IX+29)
		INC	HL
		INC	HL
		INC	HL
		EX	AF,AF'

; Subroutine call device handler
C1D38:		JP	(HL)

J1D39:		EX	AF,AF'
		LD	DE,I_BBC5
		LD	(DE),A
		LD	BC,1
		LD	A,0FFH
		CALL	C2753
		RET

; Subroutine read from file handle
C1D47:		CALL	C2136
		RET	NC
		RET	Z
		BIT	7,(IX+30)
		JR	Z,J1D6D
		RES	6,(IX+30)
		LD	L,(IX+28)
		LD	H,(IX+29)
		PUSH	BC
		CALL	C1D38
		POP	DE
		CP	0B9H
		JR	Z,J1D6B
		BIT	5,E
		RET	NZ
		CP	0C7H
		RET	NZ
J1D6B:		XOR	A
		RET

J1D6D:		PUSH	BC
		LD	DE,I_BBC5
		LD	BC,1
		LD	A,0FFH
		CALL	C2757
		LD	HL,I_BBC5
		LD	B,(HL)
		POP	DE
		OR	A
		RET	NZ
		OR	E
		RET	Z
		LD	A,B
		CP	1AH
		LD	A,0C7H
		RET	Z
		XOR	A
		RET

; ---------------------------------------------------------
; *** Functions: 43-56,60,61 ***
; ---------------------------------------------------------

; ---------------------------------------------------------
; Function $44 _CREATE
; ---------------------------------------------------------
F_CREATE:	EX	AF,AF'
		BIT	3,B
		LD	A,0CFH
		RET	NZ
		CALL	C19BE
		RET	NZ
		LD	B,0FFH
		BIT	4,A
		JR	NZ,J1DDC
		JR	J1DA6

; ---------------------------------------------------------
; Function $43 _OPEN
; ---------------------------------------------------------
F_OPEN:		EX	AF,AF'
		CALL	C19B2
		RET	NZ
		BIT	4,A
		LD	A,0CCH
		RET	NZ
J1DA6:		PUSH	HL
		CALL	C2121
		JR	NZ,J1DDF
		PUSH	HL
		CALL	C21A3
		JR	NZ,J1DDE
		EX	DE,HL
		EX	(SP),HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		PUSH	BC
		PUSH	DE
		EX	(SP),IX
		POP	HL
		LD	BC,32
		LDIR
		POP	BC
		POP	DE
		LD	(IX+31),06H
		XOR	A
		LD	(IX+45),A
		LD	(IX+46),A
		LD	(IX+47),A
		LD	(IX+48),A
		POP	HL
		EX	AF,AF'
		CALL	C2284
		CALL	C20E8
J1DDC:		XOR	A
		RET

J1DDE:		POP	HL
J1DDF:		POP	HL
		RET

; ---------------------------------------------------------
; Function $45 _CLOSE
; ---------------------------------------------------------
F_CLOSE:	CALL	C2136
		RET	NC
		RET	Z
		CALL	C22CD
		CALL	C20E8
		JR	J1DF3

; ---------------------------------------------------------
; Function $$46 _ENSURE
; ---------------------------------------------------------
F_ENSURE:	CALL	C2136
		RET	NC
		RET	Z
J1DF3:		CALL	C223A
		LD	A,(IX+25)
		CALL	C2C41
		XOR	A
		RET

; ---------------------------------------------------------
; Function $47 _DUP
; ---------------------------------------------------------
F_DUP:		CALL	C2136
		RET	NC
		RET	Z
		CALL	C2121
		RET	NZ
		CALL	C215C
		RET	NZ
		LD	(HL),E
		INC	HL
		LD	(HL),D
		CALL	C20E8
		XOR	A
		RET

; ---------------------------------------------------------
; Function $48 _READ
; ---------------------------------------------------------
F_READ:		PUSH	DE
		PUSH	HL
		CALL	C2136
		POP	BC
		POP	DE
		RET	NC
		RET	Z
		XOR	A
		CALL	C2757
		PUSH	BC
		POP	HL
		RET

; ---------------------------------------------------------
; Function $49_WRITE
; ---------------------------------------------------------
F_WRITE:	PUSH	DE
		PUSH	HL
		CALL	C2136
		POP	BC
		POP	DE
		RET	NC
		RET	Z
		XOR	A
		CALL	C2753
		PUSH	BC
		POP	HL
		RET

; ---------------------------------------------------------
; Function $4A _SEEK
; ---------------------------------------------------------
F_SEEK:		EX	AF,AF'
		PUSH	DE
		PUSH	HL
		CALL	C2136
		POP	DE
		POP	HL
		RET	NC
		RET	Z
		EX	AF,AF'
		PUSH	HL
		LD	HL,0
		LD	BC,0
		OR	A
		JR	Z,J1E63
		LD	L,(IX+45)
		LD	H,(IX+46)
		LD	C,(IX+47)
		LD	B,(IX+48)
		DEC	A
		JR	Z,J1E63
		LD	L,(IX+21)
		LD	H,(IX+22)
		LD	C,(IX+23)
		LD	B,(IX+24)
J1E63:		ADD	HL,DE
		EX	(SP),HL
		POP	DE
		ADC	HL,BC
		EX	DE,HL
		LD	(IX+45),L
		LD	(IX+46),H
		LD	(IX+47),E
		LD	(IX+48),D
		XOR	A
		RET

; ---------------------------------------------------------
; Function $4B _IOCTL
; ---------------------------------------------------------
F_IOCTL:	EX	AF,AF'
		PUSH	DE
		CALL	C2136
		POP	DE
		RET	NC
		RET	Z
		EX	AF,AF'
		LD	L,(IX+28)
		LD	H,(IX+29)
		OR	A
		JR	Z,J1EAA
		DEC	A
		JR	Z,J1E98
		DEC	A
		JR	Z,J1ECB
		DEC	A
		JR	Z,J1EE1
		DEC	A
		JR	Z,J1EFB
J1E95:		LD	A,_ISBFN
		RET

J1E98:		BIT	7,(IX+30)
		JR	Z,J1E95
		LD	A,(IX+30)
		XOR	E
		AND	0DFH
		XOR	E
		RES	6,A
		LD	(IX+30),A
J1EAA:		LD	E,(IX+30)
		XOR	A
		LD	D,A
		BIT	7,E
		RET	NZ

; Subroutine get mode of handle
C1EB2:		LD	E,(IX+25)
		DEC	E
		LD	B,4
J1EB8:		LD	A,(IX+48)
		CP	(IX+24)
		JR	C,J1EC8
		JR	NZ,J1EC6
		DEC	IX
		DJNZ	J1EB8
J1EC6:		SET	6,E
J1EC8:		XOR	A
		LD	D,A
		RET

J1ECB:		BIT	1,(IX+49)
		JR	NZ,J1EF0
		BIT	7,(IX+30)
		JR	NZ,J1EF3
		CALL	C1EB2
		BIT	6,E
		JR	NZ,J1EF0
		LD	E,00H
		RET

J1EE1:		BIT	0,(IX+49)
		JR	NZ,J1EF0
		INC	HL
		INC	HL
		INC	HL
		BIT	7,(IX+30)
		JR	NZ,J1EF3
J1EF0:		LD	E,0FFH
		RET

J1EF3:		LD	BC,6
		ADD	HL,BC
		LD	C,(IX+30)
		JP	(HL)
J1EFB:		BIT	7,(IX+30)
		JR	NZ,J1F05
		XOR	A
		LD	E,A
		LD	D,A
		RET

J1F05:		LD	BC,12
		ADD	HL,BC
		JP	(HL)

; ---------------------------------------------------------
; Function $4D _DELETE
; ---------------------------------------------------------
F_DELETE:	CALL	C19B2
		RET	NZ
		LD	A,0FFH
		CALL	C2332
		RET

; ---------------------------------------------------------
; Function $52 _HDELETE
; ---------------------------------------------------------
F_HDELETE:	CALL	C2136
		RET	NC
		RET	Z
		CALL	C22CD
		CALL	C20E8
		CALL	C2240
		SET	3,(IX+49)
		OR	A
		RET	NZ
		CALL	C1C70
		LD	A,0FFH
		CALL	C2332
		RET

; ---------------------------------------------------------
; Function $4E _RENAME
; ---------------------------------------------------------
F_RENAME:	PUSH	HL
		CALL	C19B2
		POP	BC
		RET	NZ
		CALL	C2398
		RET

; ---------------------------------------------------------
; Function $53 _HRENAME
; ---------------------------------------------------------
F_HRENAME:	PUSH	HL
		CALL	C2136
		POP	BC
		RET	NC
		RET	Z
		PUSH	BC
		CALL	C2240
		POP	BC
		OR	A
		RET	NZ
		CALL	C1C70
		CALL	C2398
		OR	A
		RET	NZ
		CALL	C227E
		XOR	A
		RET

; ---------------------------------------------------------
; Function $4F _MOVE
; ---------------------------------------------------------
F_MOVE:		PUSH	HL
		CALL	C19B2
		POP	BC
		RET	NZ
		CALL	C23FD
		RET

; ---------------------------------------------------------
; Function $54 _HMOVE
; ---------------------------------------------------------
F_HMOVE:	PUSH	HL
		CALL	C2136
		POP	BC
		RET	NC
		RET	Z
		PUSH	BC
		CALL	C2240
		POP	BC
		OR	A
		RET	NZ
		CALL	C1C70
		CALL	C23FD
		OR	A
		RET	NZ
		CALL	C227E
		XOR	A
		RET

; ---------------------------------------------------------
; Function $50 _ATTR
; ---------------------------------------------------------
F_ATTR:		EX	AF,AF'
		PUSH	HL
		CALL	C19B2
		POP	BC
		RET	NZ
		EX	AF,AF'
		OR	A
		CALL	NZ,C24DF
		OR	A
		RET	NZ
		CALL	C1C70
		LD	HL,11
		ADD	HL,DE
		LD	L,(HL)
		XOR	A
		RET

; ---------------------------------------------------------
; Function $55 _HATTR
; ---------------------------------------------------------
F_HATTR:	EX	AF,AF'
		LD	C,L
		CALL	C2136
		RET	NC
		RET	Z
		EX	AF,AF'
		OR	A
		JR	Z,J1FAF
		PUSH	BC
		CALL	C2240
		POP	BC
		OR	A
		RET	NZ
		CALL	C1C70
		CALL	C24DF
		RET	NZ
		CALL	C227E
J1FAF:		LD	L,(IX+14)
		XOR	A
		RET

; ---------------------------------------------------------
; Function $51 _FTIME
; ---------------------------------------------------------
F_FTIME:	EX	AF,AF'
		PUSH	IX
		PUSH	HL
		CALL	C19B2
		POP	BC
		POP	DE
		RET	NZ
		EX	AF,AF'
		OR	A
		CALL	NZ,C2504
		OR	A
		RET	NZ
		CALL	C1C70
		LD	HL,22
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		XOR	A
		RET

; ---------------------------------------------------------
; Function $56 _HFTIME
; ---------------------------------------------------------
F_HFTIME:	EX	AF,AF'
		PUSH	IX
		PUSH	HL
		CALL	C2136
		POP	BC
		POP	DE
		RET	NC
		RET	Z
		EX	AF,AF'
		OR	A
		JR	Z,J1FF5
		PUSH	BC
		PUSH	DE
		CALL	C2240
		POP	DE
		POP	BC
		OR	A
		RET	NZ
		CALL	C2504
		RET	NZ
		CALL	C227E
J1FF5:		LD	E,(IX+15)
		LD	D,(IX+16)
		LD	L,(IX+17)
		LD	H,(IX+18)
		XOR	A
		RET

; ---------------------------------------------------------
; Function $4C _HTEST
; ---------------------------------------------------------
F_HTEST:	PUSH	BC
		CALL	C19B2
		POP	BC
		RET	NZ
		BIT	7,(IX+30)
		JR	NZ,J201C
		PUSH	HL
		CALL	C2136
		POP	HL
		RET	NC
		RET	Z
		LD	B,0FFH
		CALL	C2308
		RET	Z
J201C:		XOR	A
		LD	B,A
		RET

; ---------------------------------------------------------
; Function $60 _FORK
; ---------------------------------------------------------
F_FORK:		
	IF OPTM = 0
		LD	HL,64
		ADD	HL,HL
	ELSE
		LD	HL,128
	ENDIF
		CALL	K_ALLOC_P2
		RET	NZ
		LD	DE,(D_BBF0)
		LD	(D_BBF0),HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		LD	A,D
		OR	E
		JR	Z,J2059
		INC	DE
		LD	B,3FH
J2038:		PUSH	BC
		INC	HL
		INC	DE
		LD	A,(DE)
		LD	C,A
		INC	DE
		LD	A,(DE)
		LD	B,A
		OR	C
		JR	Z,J2055
		PUSH	BC
		POP	IX
		BIT	2,(IX+49)
		JR	Z,J2055
		CALL	C215C
		JR	NZ,J2055
		LD	(HL),C
		INC	HL
		LD	(HL),B
		DEC	HL
J2055:		INC	HL
		POP	BC
		DJNZ	J2038
J2059:		LD	A,(D_BBFE)
		LD	B,A
		INC	A
		LD	(D_BBFE),A
		CALL	C20E8
		XOR	A
		RET

; ---------------------------------------------------------
; Function $61 _JOIN
; ---------------------------------------------------------
F_JOIN:		LD	A,B
		OR	A
		JR	Z,J2071
		LD	HL,D_BBFE
		CP	(HL)
		LD	A,_IPROC
		RET	NC
J2071:		CALL	C1290
		LD	HL,(D_BBF0)
		PUSH	HL
J2078:		LD	A,H
		OR	L
		JR	Z,J20A9
		PUSH	BC
		PUSH	HL
		CALL	K_FREE_P2
		LD	B,0FFH
J2083:		INC	B
		CALL	C2136
		JR	NC,J208E
		CALL	NZ,C216B
		JR	J2083

J208E:		POP	HL
		POP	BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		LD	(D_BBF0),HL
		LD	A,(D_BBFE)
		DEC	A
		LD	(D_BBFE),A
		INC	B
		DEC	B
		JR	Z,J2078
		CP	B
		JR	NZ,J2078
		XOR	A
		LD	(DE),A
		DEC	DE
		LD	(DE),A
J20A9:		LD	A,B
		LD	(D_BBFE),A
J20AD:		POP	HL
		LD	A,H
		OR	L
		JR	Z,J20CC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		PUSH	DE
		LD	B,3FH
J20B8:		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		PUSH	DE
		POP	IX
		LD	A,D
		OR	E
		PUSH	HL
		PUSH	BC
		CALL	NZ,C223A
		POP	BC
		POP	HL
		DJNZ	J20B8
		JR	J20AD

J20CC:		LD	A,0FFH
		CALL	C2C4A
		LD	A,(D_BBFE)
		OR	A
		JR	NZ,J20E0
		CALL	F_FORK
		CALL	C21BB
		CALL	K_CON_CLEAR
J20E0:		CALL	C20E8
		CALL	C09B6
		XOR	A
		RET

; ---------------------------------------------------------
; *** Subroutines: file and directory ***
; ---------------------------------------------------------

; Subroutine update redirect status
C20E8:		PUSH	BC
		PUSH	DE
		PUSH	HL
		PUSH	IX
		LD	C,00H
		LD	B,00H
		CALL	C2136
		JR	NC,J2103
		JR	Z,J2103
		LD	A,(IX+30)
		AND	81H
		CP	81H
		JR	Z,J2103
		SET	0,C
J2103:		LD	B,1
		CALL	C2136
		JR	NC,J2117
		JR	Z,J2117
		LD	A,(IX+30)
		AND	82H
		CP	82H
		JR	Z,J2117
		SET	1,C
J2117:		LD	A,C
		LD	(DSBB89),A
		POP	IX
		POP	HL
		POP	DE
		POP	BC
		RET

; Subroutine find free file handle
C2121:		PUSH	DE
		PUSH	IX
		LD	B,0FFH
J2126:		INC	B
		CALL	C2136
		LD	A,0C4H
		JR	NC,J2131
		JR	NZ,J2126
		XOR	A
J2131:		POP	IX
		POP	DE
		OR	A
		RET

; Subroutine get pointer to FIB of file handle
; Input:  B  = file handle
; Output: Cx = reset if invalid file handle, set if valid
;		Zx = reset if fib found
C2136:		LD	A,B
		CP	3FH
		JR	NC,J2158
		LD	HL,(D_BBF0)
		LD	A,H
		OR	L
		JR	Z,J2158
		PUSH	BC
		INC	HL
		INC	HL
		LD	C,B
		LD	B,0
		ADD	HL,BC
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		DEC	HL
		POP	BC
		PUSH	DE
		POP	IX
		LD	A,D
		OR	E
		SCF
		LD	A,_NOPEN
		RET

J2158:		LD	A,_IHAND
		OR	A
		RET

; Subroutine increase file handle count of FIB
C215C:		LD	A,(IX-1)
		INC	A
		JR	Z,J2167
		LD	(IX-1),A
		XOR	A
		RET

J2167:		LD	A,_NHAND
		OR	A
		RET

; Subroutine decrease file handle count of FIB and remove FIB if zero count
C216B:		LD	A,(IX-1)
		DEC	A
		LD	(IX-1),A
		RET	NZ
		PUSH	DE
		PUSH	BC
		PUSH	IX
		EX	(SP),HL
		LD	BC,-3
		ADD	HL,BC
		EX	DE,HL
		LD	HL,D_BBF2
		CALL	C2188
		POP	HL
		POP	BC
		POP	DE
		XOR	A
		RET

; Subroutine remove element from chain
; Input:  HL = start of chain
;         DE = address of element
C2188:		EX	DE,HL
		LD	B,H
		LD	C,L
		CALL	K_FREE_P2
		EX	DE,HL
J218F:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	A,D
		OR	E
		RET	Z
		EX	DE,HL
		SBC	HL,BC
		ADD	HL,BC
		JR	NZ,J218F
		DEC	DE
		LD	A,(HL)
		LD	(DE),A
		INC	HL
		INC	DE
		LD	A,(HL)
		LD	(DE),A
		RET

; Subroutine create FIB
C21A3:  
	IFDEF FAT16
		LD	HL,56
	ELSE
		LD	HL,54
	ENDIF
		CALL	K_ALLOC_P2
		RET	NZ
		PUSH	DE
		LD	DE,(D_BBF2)
		LD	(D_BBF2),HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		LD	(HL),01H
		INC	HL
		POP	DE
		RET

; Subroutine open default file handles
C21BB:		LD	B,5
		LD	HL,I21DB
J21C0:		PUSH	BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		PUSH	HL
		PUSH	BC
		LD	A,B
		CALL	F_OPEN
		POP	BC
		OR	A
		LD	DE,I21F6
		LD	A,B
		CALL	NZ,F_OPEN
		POP	HL
		POP	BC
		DJNZ	J21C0
		RET

I21DB:		DEFW	I21EC
		DEFB	101b		; CON read only
		DEFW	I21EC
		DEFB	110b		; CON write only
		DEFW	I21EC
		DEFB	100b		; CON read & write
		DEFW	I21EE
		DEFB	100b		; AUX read & write
		DEFW	I21F2
		DEFB	110b		; PRN write only

I21EC:		DEFB	"CON",0
I21EE:		DEFB	"AUX",0
I21F2:		DEFB	"PRN",0
I21F6:		DEFB	"NUL",0

; Subroutine get directory entry
C21FA:		BIT	7,(IX+30)
		JR	Z,J2205
		CALL	C1A1B
		XOR	A
		RET

J2205:		LD	C,(IX+25)
		LD	B,1
		CALL	C318D
		OR	A
		RET	NZ
		LD	E,(IX+37)
		LD	D,(IX+38)
		LD	(D_BBE8),DE
		PUSH	IX
		EX	(SP),HL
		LD	BC,1
		ADD	HL,BC
		LD	(D_BB9E),HL
		POP	HL
		LD	DE,I_B926
		CALL	C13FF
		OR	A
		LD	A,0DAH
		RET	NZ
		LD	DE,I_B926
		LD	(IY+47),04H
		CALL	C1A49
		OR	A
		RET

; Subroutine ensure directory entry (when file is modified)
C223A:		XOR	A
		BIT	7,(IX+49)
		RET	Z

; Subroutine ensure directory entry
C2240:		BIT	3,(IX+49)
		LD	A,0BAH
		RET	NZ
		CALL	C21FA
		RET	NZ
		BIT	7,(IX+30)
		RET	NZ
		BIT	7,(IX+49)
		RET	Z
		CALL	C1B96
		PUSH	IX
		EX	(SP),HL
		LD	BC,21
		ADD	HL,BC
		EX	DE,HL
		LD	BC,11
		ADD	HL,BC
		SET	5,(HL)
		LD	BC,15
		ADD	HL,BC
		LD	C,(IX+39)
		LD	B,(IX+40)
		LD	(HL),C
		INC	HL
		LD	(HL),B
		INC	HL
		EX	DE,HL
		LD	BC,4
		LDIR
		CALL	C2C38
		POP	HL

; Subroutine update FIB (directory entry changed)
C227E:		CALL	C1C70
		LD	A,(IX+49)

; Subroutine setup FIB for open
C2284:		PUSH	BC
		EX	AF,AF'
		CALL	C198F
		EX	AF,AF'
		AND	07H
		LD	(IX+49),A
		LD	(IX+32),L
		LD	(IX+33),H
		LD	BC,(D_BBE2)
		LD	(IX+34),C
		LD	(IX+35),B	; bit7-15 of sector
	IFDEF FAT16 ; STOR_3
		LD	A,(SDIR_1)
		LD	(IX+32h),A	; file handle
	ENDIF
		LD	A,(D_BBDF)
		SUB	(IY+96)
		LD	(IX+36),A
		LD	BC,(D_BBE8)
		LD	(IX+37),C
		LD	(IX+38),B
		LD	C,(IX+19)
		LD	B,(IX+20)
		LD	(IX+39),C
		LD	(IX+40),B
		LD	(IX+41),C
		LD	(IX+42),B
		XOR	A
		LD	(IX+43),A
		LD	(IX+44),A
		POP	BC
		RET

; Subroutine free file handle
C22CD:		CALL	C216B
		XOR	A
		LD	(HL),A
		INC	HL
		LD	(HL),A
		RET

; Subroutine check if file is opened by some other FIB
C22D5:		PUSH	DE
		PUSH	BC
		EX	DE,HL
		PUSH	IX
		POP	BC
		LD	IX,ISBBF5
J22DF:		LD	L,(IX-3)
		LD	H,(IX-2)
		LD	A,H
		OR	L
		JR	Z,J2300
		PUSH	DE
		LD	DE,3
		ADD	HL,DE
		POP	DE
		PUSH	HL
		POP	IX
		OR	A
		SBC	HL,BC
		JR	Z,J22DF
		EX	DE,HL
		CALL	C2308
		EX	DE,HL
		JR	NZ,J22DF
		LD	A,0CAH
J2300:		PUSH	BC
		POP	IX
		EX	DE,HL
		POP	BC
		POP	DE
		OR	A
		RET

; Subroutine compare with FIB
; Input:  IX = pointer to FIB
C2308:		BIT	7,(IX+30)
		RET	NZ
		LD	A,(D_BBDF)
		SUB	(IY+96)
		CP	(IX+36)
		RET	NZ
		PUSH	HL
		LD	DE,(D_BBE2)
		LD	L,(IX+34)
		LD	H,(IX+35)
		OR	A
		SBC	HL,DE
	IFDEF FAT16 ; STOR_4
		JR	NZ,STOR4A
		LD	A,(SDIR_1)
		SUB	(IX+32h)
	ENDIF
STOR4A:		POP	HL
		RET	NZ
		LD	E,(IX+32)
		LD	D,(IX+33)
		XOR	A
		SBC	HL,DE
		ADD	HL,DE
	IFDEF DOSV231
		RET	NZ
		PUSH	HL
		PUSH	BC
		LD	HL,25
		ADD	HL,DE
		EX	DE,HL
		PUSH	IX
		POP	HL
		LD	BC,26
		ADD	HL,BC
		CALL	C32F3		; compare volume-id
		POP	BC
		POP	HL
	ENDIF
		RET

; Subroutine mark current directory entry deleted and remove FAT chain
; Input:  A = 0 (deleted is not recoverable), <> 0 (delete is recoverable)
C2332:		LD	C,A
		XOR	A
		BIT	7,(IX+30)
		RET	NZ
		CALL	C22D5
		RET	NZ
		PUSH	HL
		LD	HL,11
		ADD	HL,DE
		LD	B,(HL)
		POP	HL
		BIT	0,B
		LD	A,_FILRO
		RET	NZ
		BIT	4,B
		JR	Z,J236A
		CALL	C2535
		RET	C
		CALL	C1BE6
		CALL	C1C30
		CALL	C1C53
J235A:		JR	Z,J2367
		CALL	C2535
		LD	A,_DIRNE
		RET	NZ
		CALL	C1C8A
		JR	NC,J235A
J2367:		CALL	C1BFA
J236A:		CALL	C1C70
		LD	A,C
		PUSH	HL
		LD	HL,26
		ADD	HL,DE
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		POP	HL
		OR	A
		CALL	NZ,C2EAE
		LD	D,B
		LD	E,C
		LD	A,D
		OR	E
		CALL	NZ,C2FFC
		CALL	C1C70
		PUSH	HL
		LD	HL,12
		ADD	HL,DE
		LD	A,(DE)
		LD	(HL),A
		LD	A,0E5H
		LD	(DE),A
		POP	HL
J2390:		CALL	C2523
		CALL	NZ,C34C3
		XOR	A
		RET

; Subroutine rename current directory entry
C2398:		XOR	A
		BIT	7,(IX+30)
		RET	NZ
		CALL	C22D5
		RET	NZ
		CALL	C2535
		RET	C
		LD	(D_BB9E),BC
		PUSH	DE
		LD	DE,I_B91B
		CALL	C13FF
		POP	DE
		OR	A
		LD	A,_IFNM
		RET	NZ
		PUSH	HL
		LD	HL,I_B91B
		LD	BC,I_B91B
		CALL	C160C
		POP	HL
		LD	DE,I_B91B
		LD	(D_BBAD),DE
		LD	A,0FFH
		CALL	C1637
		RET	NZ
		CALL	C2535
		RET	C
		CALL	C1BE6
		CALL	C1C53
		CALL	C1BC8
		LD	A,_DUPF
		RET	C
		CALL	C1BFA
		CALL	C1C70
		PUSH	HL
		PUSH	DE
		LD	HL,I_B91B
		LD	B,11
		LD	A,(HL)
		CP	0E5H
		JR	NZ,J23F2
		LD	A,05H
J23F2:		LD	(DE),A
		INC	HL
		INC	DE
		LD	A,(HL)
		DJNZ	J23F2
		POP	DE
		POP	HL
		JP	J2390

; Subroutine move current directory entry
C23FD:		XOR	A
		BIT	7,(IX+30)
		RET	NZ
		CALL	C22D5
		RET	NZ
		CALL	C2535
		RET	C
		PUSH	HL
		LD	HL,26
		ADD	HL,DE
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		LD	(D_BBAB),HL
		LD	HL,11
		ADD	HL,DE
		LD	A,(HL)
		POP	HL
		BIT	4,A
		CALL	NZ,C34C3
		PUSH	BC
		PUSH	HL
		LD	HL,I_B91B
		LD	B,11
		LD	A,(DE)
		CP	05H
		JR	NZ,J2430
		LD	A,0E5H
J2430:		LD	(HL),A
		INC	HL
		INC	DE
		LD	A,(DE)
		DJNZ	J2430
	IFDEF FAT16 ; STOR_5
		PUSH	AF
		LD	A,(SDIR_1)
		LD	(SDIR_3),A
		POP	AF
	ENDIF
		LD	HL,D_BBDE
		LD	DE,I_BBC6
		LD	BC,12
		LDIR
		POP	HL
		POP	DE
		LD	C,89H
		LD	B,(IX+31)
		LD	A,(IX+25)
		LD	(IY+47),00H
		CALL	C12C3
		RET	NZ
		OR	C
		LD	A,_IPATH
		RET	NZ
		BIT	1,(IY+47)
		LD	A,_DIRE
		RET	NZ
		CALL	C1C53
		LD	BC,I_B91B
		LD	(D_BBAD),BC
		CALL	C1BC8
		LD	A,_DUPF
		RET	C
		CALL	C1BFA
		RET	NZ
		CALL	C1BE6
		PUSH	HL
	IFDEF FAT16 ; STOR_6
		PUSH	AF
		LD	A,(SDIR_3)
		LD	(SDIR_1),A
		POP	AF
	ENDIF
		LD	HL,I_BBC6
		LD	DE,D_BBDE
		LD	BC,12
		LDIR
		POP	HL
		CALL	C1C70
		PUSH	HL
		LD	HL,I_B8D4
		PUSH	DE
		EX	DE,HL
		LD	BC,32
		LDIR
		POP	HL
		LD	(HL),0E5H
		CALL	C2C38
		POP	HL
		CALL	C1BFA
		CALL	C1C70
		PUSH	HL
		LD	HL,I_B8D4
		LD	BC,32
		PUSH	DE
		LDIR
		POP	DE
		POP	HL
		CALL	C2523
		RET	Z
		CALL	C34C3
		LD	BC,(D_BBE8)
		PUSH	BC
		CALL	C1C30
		CALL	C1C53
		LD	BC,I254C
		LD	(D_BBAD),BC
		CALL	C1BC8
		POP	BC
		RET	NC
		AND	10H
		RET	Z
		BIT	7,B
		JR	Z,J24D1
		LD	BC,0
J24D1:		PUSH	HL
		LD	HL,26
		ADD	HL,DE
		LD	(HL),C
		INC	HL
		LD	(HL),B
		CALL	C2C38
		XOR	A
		POP	HL
		RET

; Subroutine update attribute directory entry
C24DF:		XOR	A
		BIT	7,(IX+30)
		RET	NZ
		CALL	C22D5
		RET	NZ
		PUSH	HL
		LD	HL,11
		ADD	HL,DE
		LD	A,(HL)
		LD	B,0DDH
		BIT	4,A
		JR	NZ,J24F7
		LD	B,0D8H
J24F7:		XOR	C
		AND	B
		LD	A,_IATTR
		JR	NZ,J2502
		LD	(HL),C
		CALL	C2C38
		XOR	A
J2502:		POP	HL
		RET

; Subroutine update timestamp directory entry
C2504:		XOR	A
		BIT	7,(IX+30)
		RET	NZ
		CALL	C22D5
		RET	NZ
		PUSH	HL
		PUSH	DE
		CALL	C1C70
		LD	HL,22
		ADD	HL,DE
		EX	DE,HL
		EX	(SP),HL
		EX	DE,HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		LD	(HL),C
		INC	HL
		LD	(HL),B
		POP	DE
		POP	HL

; Subroutine mark directory entry as changed
C2523:		PUSH	HL
		LD	HL,11
		ADD	HL,DE
		BIT	4,(HL)
		JR	NZ,J252E
		SET	5,(HL)
J252E:		CALL	C2C38
		POP	HL
		LD	A,00H
		RET

; Subroutine check if special subdirectory directory entry
; Output: Cx = set if special directory entry
;		Zx = set if free or special directory entry
C2535:		PUSH	HL
		PUSH	BC
		LD	HL,I254D
		XOR	A
		CALL	C15D2
		JR	Z,J2547
		LD	HL,I254C
		XOR	A
		CALL	C15D2
J2547:		POP	BC
		POP	HL
		LD	A,_DOT
		RET

I254C:		DEFB	"."
I254D:  	DEFB    ".          "

; ---------------------------------------------------------
; *** Functions: 2F,30 ***
; ---------------------------------------------------------

; ---------------------------------------------------------
; Function $2F _RDABS
; ---------------------------------------------------------
F_RDABS:	LD	A,1
		DEFB	0FEH

; ---------------------------------------------------------
; Function $30 _WRABS
; ---------------------------------------------------------
F_WRABS:	XOR	A
		LD	(D_BBC4),A
		LD	(D_BBB4),DE
		LD	BC,(DTA_AD)
		LD	(D_BBC2),BC
		LD	A,B
		ADD	A,H
		JR	C,J2570
		ADD	A,H
J2570:		LD	A,_OV64K
J2572:		RET	C
		PUSH	HL
J2574:		LD	IX,I_B9DA
J2578:		LD	B,2
		LD	C,L
		INC	C
		CALL	C318D
		POP	BC
		OR	A
		RET	NZ
		CALL	C2C49
		CALL	C2C59
	IFDEF FAT16 ; ABSSEC
		;SECTOR READ/WRITE
		XOR	A
		LD	(BIT16),A
	ENDIF
		CALL	C2599
		CALL	C2C49
		CALL	C2C59
		LD	DE,9
		ADD	HL,DE
		LD	(HL),01H
		XOR	A
		RET

; ---------------------------------------------------------
; *** Subroutines: disk i/o and buffers ***
; ---------------------------------------------------------

; Subroutine read/write sectors
; Input:  DE = transfer address
;         B  = number of sectors
C2599:		XOR	A
		CP	B
		RET	Z
		LD	A,(D_BBC4)
		AND	04H
		LD	DE,(D_BBC2)
		CALL	C2731
		SET	7,D
		EX	AF,AF'
		PUSH	HL
		LD	HL,D_BE00
		OR	A
		SBC	HL,DE
		JR	C,J261C
		LD	A,H
		SRL	A
		INC	A
		CP	B
		JR	C,J25BC
		LD	A,B
J25BC:		LD	C,A
		POP	HL
		SUB	B
		NEG
		LD	B,A
		PUSH	BC
		LD	B,C
		EX	AF,AF'
		LD	C,A
		PUSH	BC
		XOR	A
J25C8:		PUSH	BC
		PUSH	DE
		PUSH	DE
		EX	(SP),IX
		LD	DE,(D_BBB4)
	IFDEF FAT16 ; RAMRED
		;Read a sector for Random block access
		PUSH	AF
		LD	A,(BIT16)
		LD	(RW_16),A	; write bit16-23 of sector number
		POP	AF
	ENDIF
		PUSH	DE
		DEC	A
		JR	Z,J25E4
		LD	A,00H
		BIT	0,(IY+68)
		JR	NZ,J25DF
		LD	A,1		; DSKIO write
J25DF:		CALL	C324F		; call disk driver read/write sectors with disk check
		JR	J25E6

J25E4:		LD	B,1
J25E6:		EX	AF,AF'
		POP	DE
		POP	IX
		PUSH	HL
		LD	L,B
		LD	H,0
		ADD	HL,DE
	IFDEF FAT16
		CALL	NUM_1
	ELSE
		LD	(D_BBB4),HL
	ENDIF
		POP	HL
		POP	DE
		LD	A,D
		ADD	A,B
		ADD	A,B
		LD	D,A
		LD	A,B
		POP	BC
		SUB	B
		NEG
		LD	B,A
		EX	AF,AF'
		PUSH	DE
		LD	DE,(D_BBB4)
		CALL	NZ,C3686
		POP	DE
		INC	B
		DEC	B
		JR	NZ,J25C8
		POP	BC
		CALL	C2633
		POP	BC
		LD	A,(IY+67)
		ADD	A,C
		ADD	A,C
		LD	(IY+67),A
		JP	C2599

J261C:		POP	HL
		DEC	B
		PUSH	BC
		LD	DE,512
		LD	B,E
		LD	C,E
		CALL	C2688
		POP	BC
		INC	(IY+52)
		JR	NZ,J2630
	IFDEF FAT16
		CALL	NUM_2
	ELSE
		INC	(IY+53)
	ENDIF
J2630:		JP	C2599

; Subroutine correct buffer
C2633:		CALL	C2C6F
J2636:		PUSH	BC
		EXX
		CALL	C2C9F
	IFDEF FAT16
		JP	Z,J2685
	ELSE
		JR	Z,J2685
	ENDIF
		LD	BC,4
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		LD	BC,(D_BBB4)
	IFDEF FAT16 ; DSKBUF
		SBC	HL,BC
		PUSH	AF
		PUSH	DE
		DEC	DE
		DEC	DE
		DEC	DE
		EX	DE,HL
		LD	A,(HL)		; drive number of buffer
		DEC	A
		ADD	A,A
		LD	HL,I_BA25	; (DRIVE-1)*2+BA25h=(DPB address)
		ADD	A,L
		LD	L,A
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		ADD	A,1Dh
		LD	L,A
		LD	A,0
		ADC	A,H		; HL=DPB+1Dh
		BIT	7,(HL)		; Z=1 FAT16  Z=0 FAT12
		EX	DE,HL
		POP	DE
		JR	Z,CALUC		; FAT16
		POP	AF
		POP	BC
		JR	C264C

CALUC:		POP	AF
		PUSH	DE
		INC	DE
		INC	DE
		INC	DE
		LD	A,(DE)
		LD	BC,(BIT16)
		SBC	A,C
		POP	DE
		POP	BC
		INC	A
		JP	NZ,J2665
		LD	A,L
		ADD	A,B
		JR	NC,J2652
		LD	A,0
		ADC	A,H
		JR	J2652
	ELSE
		SBC	HL,BC
		POP	BC
	ENDIF
C264C:		LD	A,H
		INC	A
		JR	NZ,J2665
		LD	A,L
		ADD	A,B
J2652:		JR	NC,J2665
		DEC	DE
		DEC	DE
		EX	DE,HL
		BIT	0,(IY+68)
		JR	NZ,J2668
		DEC	HL
		LD	(HL),00H
		DEC	HL
		DEC	HL
		CALL	C2CB7
J2665:		EXX
		JR	J2636

J2668:		BIT	7,(HL)
		JR	Z,J2665
		LD	A,C
		EXX
		PUSH	DE
		EXX
		LD	BC,8
		ADD	HL,BC
		EX	(SP),HL
		LD	D,E
		LD	E,B
		ADD	HL,DE
		ADD	HL,DE
		EX	DE,HL
		RES	7,D
		POP	HL
		LD	BC,512
		CALL	P0_LDIR
		JR	J2665

J2685:		EXX
		POP	BC
		RET

; Subroutine read/write sector
; Input:  HL = pointer to drive table
;         IX = pointer to FIB
;         DE = number of bytes
;         BC = start offset
C2688:		PUSH	HL
		EX	(SP),IX
		PUSH	DE
		PUSH	BC
		BIT	0,(IY+68)
		JR	NZ,J26B8
		BIT	1,D
		JR	NZ,J26BB
		BIT	1,(IY+68)
		JR	NZ,J26B8
		PUSH	DE
		LD	HL,(D_BBC0)
		LD	DE,(D_BBB2)
		OR	A
		SBC	HL,DE
		POP	DE
		JR	C,J26B5
		LD	A,B
		OR	C
		JR	NZ,J26B8
		SBC	HL,DE
		JR	C,J26BB
		JR	J26B8

J26B5:		ADD	HL,BC
		JR	NC,J26BB
J26B8:		LD	B,3
		DEFB	021H
J26BB:		LD	B,2
		LD	DE,(D_BBB4)
	IFDEF FAT16
		CALL	BUF_2F
	ELSE
		CALL	C2B78
	ENDIF
		POP	DE
		ADD	HL,DE
		LD	BC,11
		ADD	HL,BC
		POP	BC
		PUSH	DE
		PUSH	BC
		LD	DE,(D_BBC2)
		PUSH	DE
		LD	A,(D_BBC4)
		CALL	C26F3
		POP	HL
		POP	BC
		ADD	HL,BC
		LD	(D_BBC2),HL
		BIT	0,(IY+68)
		CALL	Z,C2C38
		POP	HL
		ADD	HL,BC
		BIT	1,H
		LD	HL,(D_BBF6)
		CALL	NZ,C2CB4
		EX	(SP),IX
		POP	HL
		RET

; Subroutine segment LDIR
; Input:  DE = address
;         BC = size
;         A  = (b0 = to/from),(b2 = segment type)
C26F3:		PUSH	DE
		PUSH	BC
		PUSH	AF
		PUSH	HL
		AND	04H
		CALL	C2731
		LD	HL,04000H
		OR	A
		SBC	HL,DE
		SBC	HL,BC
		JR	NC,J2709
		ADD	HL,BC
		LD	C,L
		LD	B,H
J2709:		POP	HL
		EX	(SP),HL
		BIT	0,H
		EX	(SP),HL
		PUSH	BC
		JR	NZ,J2712
		EX	DE,HL
J2712:		CALL	P0_LDIR
		JR	NZ,J2718
		EX	DE,HL
J2718:		POP	BC
		POP	AF
		EX	(SP),HL
		OR	A
		SBC	HL,BC
		LD	B,H
		LD	C,L
		POP	HL
		JR	Z,J272F
		LD	E,A
		POP	AF
		AND	0C0H
		ADD	A,40H
		LD	D,A
		LD	A,E
		LD	E,00H
		JR	C26F3

J272F:		POP	DE
		RET

; Subroutine get segment number
; Input:  DE = address
;         A  = segment type (0 = TPA, A<>0 = current)
; Output: DE = page 0 based address
;         A  = segment number
C2731:		PUSH	DE
		PUSH	HL
		LD	HL,P0_TPA
		OR	A
		JR	Z,J273C
		LD	HL,P0_SEG
J273C:		LD	A,D
		AND	0C0H
		RLCA
		RLCA
		LD	E,A
		LD	D,0
		ADD	HL,DE
		LD	A,(HL)
		POP	HL
		POP	DE
		RES	6,D
		RES	7,D
		RET

	IF OPTM = 0
		; Subroutine zero write to FIB
		; Not used
Q_274D:		AND	04H
		OR	02H
		JR	C275B
	ENDIF

; Subroutine write to FIB
; Input:  BC = size
;         DE = transfer address
C2753:		AND	04H
		JR	C275B

; Subroutine read from FIB
; Input:  BC = size
;         DE = transfer address
C2757:		AND	04H
		OR	01H

; Subroutine read/write from FIB
; Input:  BC = size
;         DE = transfer address
;         IX = FIB
;         A  = operation flags
C275B:		BIT	7,(IX+30)
		JR	NZ,J27AA
		LD	(D_BBC4),A
		AND	10H
		OR	B
		OR	C
		RET	Z
		XOR	A
		LD	(D_BBBE),A
		LD	(DSBBBF),A
		LD	(D_BBC0),BC
		LD	(D_BBC2),DE
		CALL	C2863
		LD	BC,(D_BBBE)
		LD	DE,(D_BBC2)
J2783:		LD	L,(IX+45)
		LD	H,(IX+46)
		ADD	HL,BC
		LD	(IX+45),L
		LD	(IX+46),H
		JR	NC,J279A
		INC	(IX+47)
		JR	NZ,J279A
		INC	(IX+48)
J279A:		OR	A
		RET	NZ
		BIT	4,(IY+68)
		JR	NZ,J27A8
		LD	A,B
		OR	C
		LD	A,0C7H
		JR	Z,J279A
J27A8:		XOR	A
		RET

J27AA:		LD	H,A
		AND	04H
		LD	(IX+50),A
		LD	A,B
		OR	C
		RET	Z
		BIT	0,H
		JR	NZ,J27FC
		LD	L,(IX+28)
		LD	H,(IX+29)
		INC	HL
		INC	HL
		INC	HL
		PUSH	BC
J27C1:		PUSH	DE
		LD	A,(IX+50)
		CALL	C2731
		EX	DE,HL
		CALL	RD_SEG
		EI
		EX	DE,HL
		POP	DE
		BIT	5,(IX+30)
		JR	Z,J27D9
		CP	1AH
		JR	Z,J27F1
J27D9:		PUSH	IX
		PUSH	BC
		PUSH	DE
		PUSH	HL
		CALL	C285F
		POP	HL
		POP	DE
		POP	BC
		POP	IX
		OR	A
		JR	NZ,J27F4
		INC	DE
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,J27C1
		JR	J27F4

J27F1:		XOR	A
		INC	DE
		DEC	BC
J27F4:  POP	HL
		OR	A
		SBC	HL,BC
		LD	B,H
		LD	C,L
J27FA:		JR	J2783

J27FC:		LD	L,(IX+28)
		LD	H,(IX+29)
		PUSH	BC
		RES	6,(IX+30)
J2807:		PUSH	BC
		PUSH	IX
		PUSH	DE
		PUSH	HL
		CALL	C285F
		POP	HL
		POP	DE
		POP	IX
		LD	C,00H
		OR	A
		JR	Z,J2830
		CP	0C7H
		JR	Z,J2827
		CP	0B9H
		JR	NZ,J2850
		BIT	2,(IX+30)
		JR	NZ,J2830
		INC	C
J2827:		INC	C
		BIT	5,(IX+30)
		JR	NZ,J2830
		LD	C,00H
J2830:		PUSH	DE
		PUSH	HL
		LD	A,(IX+50)
		CALL	C2731
		EX	DE,HL
		LD	E,B
		CALL	WR_SEG
		EI
		POP	HL
		POP	DE
		LD	A,C
		POP	BC
		DEC	A
		JR	Z,J2853
		INC	DE
		DEC	BC
		DEC	A
		JR	Z,J2857
		LD	A,B
		OR	C
		JR	NZ,J2807
		JR	J2857

J2850:		POP	BC
		JR	J2857

J2853:		SET	6,(IX+30)
J2857:		POP	HL
		OR	A
		SBC	HL,BC
		LD	B,H
		LD	C,L
		JR	J27FA

; Subroutine call device handler
C285F:		LD	C,(IX+30)
		JP	(HL)

; Subroutine read/write file
C2863:		LD	A,_HDEAD
		BIT	3,(IX+49)
		RET	NZ
		LD	A,_ACCV
		BIT	0,(IY+68)
		JR	Z,J2879
		BIT	1,(IX+49)
		RET	NZ
		JR	J2885

J2879:		BIT	0,(IX+49)
		RET	NZ
		LD	A,_FILRO
		BIT	0,(IX+14)
		RET	NZ
J2885:		BIT	0,(IY+127)
		JR	Z,J2892
		LD	HL,(D_BBC2)
		ADD	HL,BC
		LD	A,_OV64K
		RET	C
J2892:		LD	C,(IX+25)
		LD	B,1
		CALL	C318D
		OR	A
		RET	NZ
		CALL	C29B6
		RET	NZ
		LD	BC,(D_BBC0)
		LD	A,B
		OR	C
		RET	Z
		LD	DE,(D_BBBC)
		CALL	C2B3A
		LD	(D_BBB6),DE
		RET	NZ
		LD	BC,(D_BBB9)
		LD	A,B
		OR	C
		CALL	NZ,C2936
		RET	NZ
J28BD:		LD	C,(IY+65)
		SRL	C
		JR	Z,J291A
		CALL	C295F
		RET	NZ
		SUB	(IY+59)
		LD	B,A
		LD	DE,(D_BBB6)
		CALL	C2993
J28D3:		LD	A,B
		ADD	A,(IY+56)
		LD	B,A
		CP	C
		JR	NC,J28FA
		PUSH	DE
		CALL	C2D86
		EX	(SP),HL
		OR	A
		INC	HL
		SBC	HL,DE
		JR	NZ,J28F1
		INC	(IY+60)
		JR	NZ,J28EE
		INC	(IY+61)
J28EE:		POP	HL
		JR	J28D3

J28F1:		ADD	HL,DE
		EX	DE,HL
		DEC	DE
		POP	HL
		LD	A,(D_BBB8)
		JR	J2900

J28FA:		LD	A,(D_BBB8)
		SUB	B
		ADD	A,C
		LD	B,C
J2900:		DEC	A
		LD	(D_BBBB),A
		LD	(D_BBB6),DE
		PUSH	BC
		CALL	C2599
		POP	BC
		SLA	B
		LD	C,00H
		CALL	C29A4
		SET	3,(IY+68)
		JR	J28BD

J291A:		LD	BC,0
		CALL	C2936
		RET	NZ
		LD	DE,(D_BBB6)
		LD	(IX+41),E
		LD	(IX+42),D
		LD	DE,(D_BBBC)
		LD	(IX+43),E
		LD	(IX+44),D
		RET

; Subroutine transfer sector partly
; Input:  BC = offset, (BC) = size
C2936:		PUSH	HL
		LD	HL,512
		OR	A
		SBC	HL,BC
		EX	DE,HL
		LD	HL,(D_BBC0)
		SBC	HL,DE
		JR	NC,J2948
		ADD	HL,DE
		LD	E,L
		LD	D,H
J2948:		LD	A,D
		OR	E
		POP	HL
		RET	Z
		CALL	C295F
		RET	NZ
		CALL	C2993
		CALL	C2688
		SET	3,(IY+68)
		CALL	C29A4
		XOR	A
		RET

; Subroutine update to next sector
C295F:		XOR	A
		BIT	3,(IY+68)
		RET	Z
		RES	3,(IY+68)
		LD	A,(D_BBBB)
		INC	A
		CP	(IY+56)
		JR	NZ,J298E
		PUSH	DE
		LD	DE,(D_BBBC)
		INC	DE
		LD	(D_BBBC),DE
		LD	DE,(D_BBB6)
		CALL	C2D86
		LD	(D_BBB6),DE
	IFNDEF FAT16
		BIT	7,D
	ENDIF
		POP	DE
		LD	A,0C8H
		RET	NZ
		XOR	A
J298E:		LD	(D_BBBB),A
		XOR	A
		RET

; Subroutine calculate physical sector from physical cluster and sector in cluster
C2993:		PUSH	DE
		LD	A,(D_BBBB)
		LD	DE,(D_BBB6)
		CALL	C2DB6
		LD	(D_BBB4),DE
		POP	DE
		RET

; Subroutine update size done, size left
C29A4:		PUSH	HL
		LD	HL,(D_BBBE)
		ADD	HL,BC
		LD	(D_BBBE),HL
		LD	HL,(D_BBC0)
		SBC	HL,BC
		LD	(D_BBC0),HL
		POP	HL
		RET

; Subroutine start read/write file
; Input:  IX = FIB
;         HL = pointer to drive table
C29B6:		PUSH	HL
		LD	C,(IX+45)
		LD	B,(IX+46)
		LD	E,(IX+47)
		LD	D,(IX+48)
		PUSH	DE
		PUSH	BC
		CALL	C2B05
		JR	NZ,J2A19
		LD	(D_BBBC),DE
		LD	(D_BBBB),A
		LD	(D_BBB9),BC
		POP	HL
		LD	DE,(D_BBC0)
		DEC	DE
		ADD	HL,DE
		EX	(SP),HL
		POP	BC
		EX	DE,HL
		EX	(SP),HL
		PUSH	HL
		LD	HL,0
		BIT	4,(IY+68)
		JR	Z,J29EB
		DEC	HL
J29EB:		ADC	HL,DE
		EX	DE,HL
		BIT	7,D
		JR	NZ,J2A62
		POP	HL
		PUSH	DE
		PUSH	BC
		CALL	C2B05
		JR	NZ,J2A19
		PUSH	DE
		LD	C,(IX+21)
		LD	B,(IX+22)
		LD	E,(IX+23)
		LD	D,(IX+24)
		LD	A,B
		OR	C
		DEC	BC
		JR	NZ,J2A0F
		OR	D
		OR	E
		DEC	DE
J2A0F:		PUSH	DE
		PUSH	BC
		CALL	NZ,C2B05
		EXX
		POP	HL
		POP	DE
		EXX
		POP	BC
J2A19:		JR	NZ,J2A92
		EXX
		POP	BC
		XOR	A
		SBC	HL,BC
		EX	(SP),HL
		EX	DE,HL
		SBC	HL,DE
		EXX
		BIT	7,D
		EX	AF,AF'
		BIT	0,(IY+68)
		JP	NZ,J2AEA
		PUSH	HL
		LD	H,B
		LD	L,C
		INC	DE
		OR	A
		SBC	HL,DE
		LD	C,L
		LD	B,H
		INC	BC
		JR	NC,J2A76
		ADD	HL,DE
		EX	DE,HL
		POP	HL
		BIT	4,(IY+68)
		JR	Z,J2AA9
		CALL	C2B3A
		JR	NZ,J2A94
		PUSH	DE
		CALL	C2D86
	IFDEF FAT16
		CALL	Z,C2FFC
		POP	DE
		LD	BC,0FFFFH
	ELSE
		BIT	7,D
		CALL	Z,C2FFC
		POP	DE
		LD	B,0FFH
	ENDIF
		CALL	C2DD3
		LD	C,(IX+39)
		LD	B,(IX+40)
		SCF
		EX	AF,AF'
		JR	J2A96

J2A62:		EXX
		EX	AF,AF'
		POP	HL
		LD	E,(IX+39)
		LD	D,(IX+40)
		LD	A,D
		OR	E
		CALL	NZ,C2FFC
		PUSH	DE
		LD	BC,0
		JR	J2A96

J2A76:		POP	HL
		LD	A,(IY+68)
		AND	02H
		CALL	C2F40
		JR	NZ,J2A93
		DEC	DE
		BIT	7,D
		JR	NZ,J2A96
		PUSH	BC
		CALL	C2B3A
		POP	BC
		JR	NZ,J2A93
		CALL	C2DD6
		JR	J2AA9

J2A92:		POP	DE
J2A93:		POP	DE
J2A94:		POP	DE
		RET

J2A96:		LD	(IX+39),C
		LD	(IX+40),B
		XOR	A
		LD	(IX+41),C
		LD	(IX+42),B
		LD	(IX+43),A
		LD	(IX+44),A
J2AA9:		SET	7,(IX+49)
		EXX
		EX	AF,AF'
		JR	NZ,J2AB3
		JR	NC,J2AC5
J2AB3:		INC	BC
		LD	A,B
		OR	C
		JR	NZ,J2AB9
		INC	DE
J2AB9:		LD	(IX+21),C
		LD	(IX+22),B
		LD	(IX+23),E
		LD	(IX+24),D
J2AC5:		POP	BC
		POP	DE
		LD	A,B
		OR	C
		DEC	BC
		JR	NZ,J2ACD
		DEC	HL
J2ACD:		BIT	7,H
		JR	Z,J2AE1
		LD	A,H
		AND	L
		INC	A
		LD	HL,0FFFFH
		JR	NZ,J2AE4
		INC	HL
		SBC	HL,BC
		JR	NZ,J2AE4
		DEC	HL
		JR	J2AE4

J2AE1:		LD	HL,0
J2AE4:		LD	(D_BBB2),HL
		EXX
		XOR	A
		RET

J2AEA:		EX	AF,AF'
		POP	DE
		EX	(SP),HL
		JR	NZ,J2AFB
		JR	NC,J2B02
		EXX
		LD	A,H
		AND	L
		INC	A
		EXX
		JR	NZ,J2AFB
		ADD	HL,DE
		JR	C,J2AFE
J2AFB:		LD	HL,0FFFFH
J2AFE:		INC	HL
		LD	(D_BBC0),HL
J2B02:		XOR	A
		POP	HL
		RET

; Subroutine convert position in file to cluster, sector and offset
; Input:  DE,BC = position
;         HL    = pointer to drive table
; Output: DE    = relative cluster
;         A     = sector in cluster
;         HL    = offset in sector
C2B05:		PUSH	HL
		LD	A,B
		SRL	D
		RR	E
		RRA
		LD	B,00H
		RL	B
		PUSH	BC
		LD	BC,10
		ADD	HL,BC
		PUSH	AF
		AND	(HL)
		LD	C,A
		LD	A,(HL)
		INC	A
		LD	(D_BBB8),A
		POP	AF
		INC	HL
		LD	B,(HL)
		JR	J2B27

J2B22:		SRL	D
		RR	E
		RRA
J2B27:		DJNZ	J2B22
		INC	D
		DEC	D
		JR	NZ,J2B35
		BIT	7,E
		JR	NZ,J2B35
		LD	D,E
		LD	E,A
		LD	A,C
		DEFB	021H
J2B35:		LD	A,_FILE
		POP	BC
		POP	HL
		RET

; Subroutine translate logical cluster to physical cluster
C2B3A:		PUSH	HL
		EX	DE,HL
		LD	E,(IX+41)
		LD	D,(IX+42)
		LD	A,D
		OR	E
		JR	Z,J2B65
		LD	C,(IX+43)
		LD	B,(IX+44)
		SBC	HL,BC
		JR	NC,J2B57
		ADD	HL,BC
		LD	E,(IX+39)
		LD	D,(IX+40)
J2B57:		EX	(SP),HL
		POP	BC
J2B59:		LD	A,B
		OR	C
		RET	Z
		DEC	BC
		CALL	C2D86
	IFNDEF FAT16
		BIT	7,D
	ENDIF
		JR	Z,J2B59
		DEFB	03EH
J2B65:		POP	HL
		LD	A,_FILE
		OR	A
		RET

; Subroutine get FAT sector
C2B6A:  
	IFDEF FAT16 ; BUF_1F
		XOR	A
		LD	(DSKEX),A	; FAT
	ENDIF
		LD	B,1
		LD	A,(IX+14)
		BIT	0,(IX+24)
		JR	Z,J2B7A
		DEC	A
		JR	NZ,J2B7A

; Subroutine get sector
; Input:  B  = flag (b0 reset do not real read, b1 reset ignore not recommended)
;         IX = pointer to drive table
;         DE = sector number
C2B78:		LD	A,1
J2B7A:		LD	(DSBBA2),A
		LD	C,(IX+8)
		LD	HL,(D_BBF6)
		LD	A,H
		SUB	01H
		CALL	NC,C2C26
		RET	Z
		LD	HL,(D_BBF8)
		PUSH	HL
J2B8E:		CALL	C2C26
		JP	Z,J2C19
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	H
		JR	NZ,J2B8E
		POP	HL
		INC	HL
		INC	HL
		BIT	7,(HL)
		DEC	HL
		LD	A,(HL)
		DEC	HL
		JR	Z,J2BA7
		LD	L,(HL)
		LD	H,A
J2BA7:		CALL	C2D0F
		PUSH	HL
		INC	HL
		INC	HL
		LD	(HL),C
		INC	HL
		LD	A,B
		AND	02H
		LD	(HL),A
		INC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		LD	A,(IY+34)
		LD	(HL),A
		INC	HL
	IFDEF FAT16 ; SECNUM
		;Write bit16-23 of sector number at buffer+8
		BIT	7,(IX+1Dh)
		LD	A,0
		JR	NZ,SECN_1	; FAT12
		LD	A,(DSKEX)	; FAT16
SECN_1:		LD	C,(IX+11h)
		LD	(HL),C
		INC	HL
		LD	(HL),A
	ELSE
		LD	C,(IX+17)
		LD	(HL),C
		INC	HL
	ENDIF
		PUSH	DE
		INC	HL
		INC	HL
		INC	HL
		PUSH	HL
		LD	E,L
		LD	D,H
		INC	DE
		LD	(HL),00H
		PUSH	BC
		LD	BC,512-1
		LDIR
		POP	BC
		EX	(SP),IX
		POP	HL
		LD	(IX-2),L
		LD	(IX-1),H
		POP	DE
		BIT	0,B
		JR	Z,J2C15
J2BE1:		PUSH	BC
		PUSH	DE
		LD	B,(IY+34)
		JR	J2BEE

J2BE8:		LD	A,E
		ADD	A,C
		LD	E,A
		JR	NC,J2BEE
		INC	D
J2BEE:		PUSH	DE
		PUSH	BC
		LD	B,1
		LD	A,(DATA_S)
		LD	C,A
		LD	A,00H		; DSKIO read
	IFDEF FAT16
		CALL	REDBUF
	ELSE
		CALL	C324F		; call disk driver read/write sectors with disk check
	ENDIF
		POP	BC
		POP	DE
		JR	Z,J2C13
		CP	0F1H
		JR	Z,J2C05
		DJNZ	J2BE8
J2C05:		POP	DE
		POP	BC
		OR	A
		BIT	1,B
		JR	NZ,J2C0D
		SCF
J2C0D:		CALL	C368A
		JR	Z,J2BE1
		DEFB	0CAH
J2C13:		POP	DE
		POP	BC
J2C15:		EX	(SP),HL
		POP	IX
		DEFB	0FEH
J2C19:		POP	BC
		SET	1,(IY+41)
		LD	(D_BBF6),HL
		CALL	C2CD7
		XOR	A
		RET

; Subroutine buffer contains sector of drive ?
C2C26:		PUSH	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		SUB	C
		JR	NZ,J2C36
		INC	HL
		INC	HL
		LD	A,(HL)
		SUB	E
		JR	NZ,J2C36
	IFDEF FAT16 ; CMPSEC
		;Compare sector number at buffer
		INC	HL
		LD	A,(HL)
		SUB	D
		JR	NZ,J2C36	; Z=0 different sector
		BIT	7,(IX+1Dh)
		JR	Z,CMPS_1	; Z=1 FAT16
		XOR	A
		JR	J2C36
CMPS_1:		LD	A,(DSKEX)
		INC	HL
		INC	HL
		INC	HL
		CP	(HL)		; bit16-23 of sector number
	ELSE
		INC	HL
		LD	A,(HL)
		SUB	D
	ENDIF
J2C36:		POP	HL
		RET

; Subroutine mark buffer last read as changed
C2C38:		LD	HL,(D_BBF6)
		INC	HL
		INC	HL
		INC	HL
		SET	7,(HL)
		RET

; Subroutine flush sector buffers of logical drive
C2C41:		PUSH	BC
		PUSH	HL
		CALL	C3606
		POP	HL
		POP	BC
		DEFB	0FEH

; Subroutine flush sector buffers of drive table
C2C49:		DEFB	0F6H

; Subroutine flush sector buffers of physical drive
C2C4A:		SCF
		CALL	C2C6D
		PUSH	HL
J2C4F:		CALL	C2C9F
		JR	Z,J2C9D
		CALL	C2D0F
		JR	J2C4F

; Subroutine mark sector buffers of drive table unused
C2C59:  	DEFB	0F6H

; Subroutine mark sector buffers of physical drive unused
C2C5A:		SCF
		CALL	C2C6D
		PUSH	HL
J2C5F:		CALL	C2C9F
		JR	Z,J2C9D
		CALL	C2CB7
		INC	HL
		INC	HL
		LD	(HL),00H
		JR	J2C5F

; Subroutine mark buffers of drive
C2C6D:		JR	C,J2C78

; Subroutine mark buffers of drive table
C2C6F:		PUSH	HL
		PUSH	BC
		LD	BC,8
		ADD	HL,BC
		LD	A,(HL)
		POP	BC
		POP	HL
J2C78:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	B,A
		LD	HL,(D_BBF8)
J2C7F:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		INC	HL
		RES	0,(HL)
		OR	A
		JR	Z,J2C96
		CP	B
		JR	Z,J2C94
		RLCA
		JR	C,J2C96
		LD	A,B
		INC	A
		JR	NZ,J2C96
J2C94:		SET	0,(HL)
J2C96:		LD	A,D
		OR	E
		EX	DE,HL
		JR	NZ,J2C7F
		POP	BC
		POP	DE
J2C9D:		POP	HL
		RET

; Subroutine find marked sector buffer
C2C9F:		LD	HL,(D_BBF8)
J2CA2:		PUSH	HL
		INC	HL
		INC	HL
		INC	HL
		BIT	0,(HL)
		RES	0,(HL)
		POP	HL
		RET	NZ
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	H
		JR	NZ,J2CA2
		RET

; Subroutine flush buffer, buffer at start of chain
C2CB4:		CALL	C2D0F

; Subroutine buffer at start of chain
; Input:  HL = pointer to buffer
C2CB7:		PUSH	DE
		LD	DE,(D_BBF8)
		OR	A
		SBC	HL,DE
		ADD	HL,DE
		JR	Z,J2CFE
		PUSH	BC
		PUSH	HL
		LD	(D_BBF8),HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		PUSH	BC
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		EX	DE,HL
		LD	B,D
		LD	C,E
		CALL	C2D00
		POP	BC
		JR	J2CF8

; Subroutine buffer at end of chain
C2CD7:		LD	A,(HL)
		INC	HL
		OR	(HL)
		DEC	HL
		RET	Z
		PUSH	DE
		PUSH	BC
		PUSH	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		PUSH	DE
		XOR	A
		LD	(HL),A
		DEC	HL
		LD	(HL),A
		LD	B,H
		LD	C,L
		LD	HL,D_BBF8
		CALL	C2D00
		EX	DE,HL
		POP	DE
		LD	(HL),E
		INC	HL
		LD	(HL),D
		DEC	HL
		CALL	C2D00
J2CF8:		EX	DE,HL
		LD	(HL),C
		INC	HL
		LD	(HL),B
		POP	HL
		POP	BC
J2CFE:		POP	DE
		RET

; Subroutine search requested buffer
; Input:  HL = pointer to starting buffer
;         BC = pointer to requested buffer
; Output: HL = pointer to requested buffer
;         DE = pointer to previous buffer
C2D00:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		DEC	HL
		EX	DE,HL
		LD	A,H
		OR	L
		RET	Z
		SBC	HL,BC
		ADD	HL,BC
		JR	NZ,C2D00
		OR	A
		RET

; Subroutine flush buffer
C2D0F:		SET	1,(IY+41)
		PUSH	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		OR	A
		JR	Z,J2D84
		INC	HL
		BIT	7,(HL)
		JR	Z,J2D84
		RES	7,(HL)
		PUSH	DE
		PUSH	BC
		PUSH	IX
J2D25:		PUSH	HL
		BIT	6,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		JR	Z,J2D3B
		LD	A,E
J2D32:		ADD	A,(HL)
	IFDEF FAT16 ; FSIZE1
		JR	NC,FSIZ_1
		INC	D
FSIZ_1:		INC	(HL)
		DEC	(HL)
		JR	NZ,J2D36
	ELSE
		JR	NC,J2D36
	ENDIF
		INC	D
J2D36:		DJNZ	J2D32
		LD	E,A
		LD	B,1
J2D3B:		PUSH	DE
		INC	HL
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		EX	DE,HL
		PUSH	DE
		POP	IX
		LD	C,1
		POP	DE
		PUSH	DE
J2D4A:		PUSH	DE
		PUSH	BC
		LD	B,1
		LD	A,(DATA_S)
		LD	C,A
		LD	A,1		; DSKIO write
	IFDEF FAT16
		CALL	WRTBUF
	ELSE
		CALL	C324F		; call disk driver read/write sectors with disk check
	ENDIF
		POP	BC
		POP	DE
		JR	NZ,J2D5C
		INC	C
J2D5C:		CP	0F1H
		JR	NZ,J2D63
		LD	BC,00101H
J2D63:		PUSH	AF
		LD	A,E
	IFDEF FAT16 ; FSIZE2
		LD	A,(IX-4)
		OR	A
		JR	NZ,FSIZ_2
		INC	D
FSIZ_2:		ADD	A,E
	ELSE
		ADD	A,(IX-4)
	ENDIF
		LD	E,A
		JR	NC,J2D6C
		INC	D
J2D6C:		POP	AF
		DJNZ	J2D4A
		POP	DE
		DEC	C
		JR	NZ,J2D7D
		BIT	1,(IX-8)
		JR	NZ,J2D7A
		SCF
J2D7A:		CALL	C368A
J2D7D:		POP	HL
		JR	Z,J2D25
		POP	IX
		POP	BC
		POP	DE
J2D84:		POP	HL
		RET

; Subroutine get FAT entry content
; Input:  DE = cluster number
;         HL = pointer to drive table
C2D86:  
	IFDEF FAT16 ; FATRED
		PUSH	AF
		CALL	CHKDRV
		JP	Z,Z0018		; use FAT16
		POP	AF
		CALL	C2D86A		; FAT12 routine
		BIT	7,D
		RET
	ENDIF
C2D86A:		CALL	C2E37
		JR	Z,J2D9B
J2D8B:		XOR	A
		LD	(D_BBEA),A
		LD	A,_IFAT
		LD	DE,0FFFFH
		CALL	C3686
		JR	Z,J2D8B
		JR	J2DB2

J2D9B:		PUSH	HL
		LD	A,(DE)
		LD	L,A
		INC	DE
		LD	A,(DE)
		JR	NC,J2DA7
		LD	H,A
		CALL	C2E98
		LD	L,H
J2DA7:		AND	0FH
		LD	H,A
		EX	DE,HL
		LD	HL,00FF7H
		SBC	HL,DE
		POP	HL
		RET	NC
J2DB2:		LD	DE,0FFFFH
		RET

; Subroutine convert cluster number to sector number
; Input:  HL = pointer to drive table
;         DE = cluster number
;         A  = sector in cluster
C2DB6:		PUSH	HL
		PUSH	BC
		LD	BC,11
		ADD	HL,BC
		LD	B,(HL)
		EX	DE,HL
		DEC	HL
		DEC	HL
	IFDEF FAT16 ; GETSEC
		; Change cluster number to sector number
		; Input:  DE=cluster number
		; Output: DE=sector number bit0-15
		;  	  BIT16 = bit16-23
		; Change cluster number to sector number
		; Input:  DE=cluster number
		; Output: DE=sector number bit0-15
		;  	  BIT16 = bit16-23
		LD	C,A
		XOR	A
		JR	Z0022
Z0023:		ADD	HL,HL		; bit 0-15 of sector number
		ADC	A,A		; bit16-23 of sector number
Z0022:		DJNZ	Z0023
		LD	B,A		; bit16-23
		LD	A,C
		ADD	A,L
		LD	L,A
		EX	DE,HL		; BDE=sector number
		LD	A,B
		LD	BC,0009h
		ADD	HL,BC		; DPB+14h start sector of data area
		LD	C,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,C
		ADD	HL,DE
		JR	NC,Z0024
		INC	A
Z0024:		LD	(BIT16),A	; save bit16-23
	ELSE
		DEFB	00EH
J2DC1:		ADD	HL,HL
		DJNZ	J2DC1
		ADD	A,L
		LD	L,A
		EX	DE,HL
		LD	C,9
		ADD	HL,BC
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		ADD	HL,DE
	ENDIF
		EX	DE,HL		; BDE=sector number
		POP	BC
		POP	HL
		RET

; Subroutine reset deleted files status disk and set FAT entry
C2DD3:		CALL	C2EA2

; Subroutine set FAT entry
; Input:  BC = cluster number
C2DD6:		PUSH	DE
	IFDEF FAT16 ; FATWRT
		CALL	CHKDRV
		JR	NZ,J2DDA	; nz=12bit FAT
FATWR2:		CALL	FATADR
		JR	Z,FATWR3
		LD	A,0FFh
		LD	(D_BBEA),A
FATWR4:		LD	A,0F2h
		LD	DE,0FFFFh
		CALL	C3686
		JR	Z,FATWR4
		JP	J2E35
FATWR3:		PUSH	HL
		LD	A,C
		LD	(DE),A		; FAT write
		INC	DE
		LD	A,B
		LD	(DE),A
		JR	J2E0F
	ENDIF
J2DDA:		LD	A,B
		CP	10H
		JR	C,J2DDF
		LD	BC,00FFFH
J2DDF:		CALL	C2E37
		JR	Z,J2DF5
J2DE4:		LD	A,0FFH
		LD	(D_BBEA),A
		LD	A,0F2H
		LD	DE,0FFFFH
		CALL	C3686
		JR	Z,J2DE4
		JR	J2E35

J2DF5:		PUSH	HL
		JR	C,J2E02
		LD	A,C
		LD	(DE),A
		INC	DE
		LD	A,(DE)
		AND	0F0H
		OR	B
		LD	(DE),A
		JR	J2E0F

J2E02:		LD	H,B
		LD	L,C
		CALL	C2E98
		LD	A,(DE)
		AND	0FH
		OR	L
		LD	(DE),A
		INC	DE
		LD	A,H
		LD	(DE),A
J2E0F:		CALL	C2C38
		BIT	0,(IY+41)
		JR	Z,J2E34
		LD	BC,8
		ADD	HL,BC
		LD	(HL),A
		DEC	DE
		LD	A,(DE)
		LD	DE,(D_BBA7)
		EX	(SP),IX
		PUSH	AF
		CALL	C2B6A
		CALL	C2C38
		LD	BC,512+7
		ADD	HL,BC
		POP	AF
		LD	(HL),A
		EX	(SP),IX
J2E34:		POP	HL
J2E35:		POP	DE
		RET

; Subroutine find cluster
; Input:  DE = cluster number
;         HL = pointer to drive table
C2E37:		PUSH	IX
		PUSH	BC
		PUSH	HL
		PUSH	HL
		POP	IX
		RES	0,(IY+41)
		LD	L,(IX+22)
		LD	H,(IX+23)
		XOR	A
		SBC	HL,DE
		JR	C,J2E93
		LD	H,D
		LD	L,E
		ADD	HL,HL
		ADD	HL,DE
		SRL	H
		RR	L
		PUSH	AF
		PUSH	HL
		LD	E,H
		SRL	E
		LD	D,A
		LD	L,(IX+12)
		LD	H,(IX+13)
		ADD	HL,DE
		EX	DE,HL
		CALL	C2B6A
		LD	BC,11
		ADD	HL,BC
		POP	BC
		LD	A,B
		AND	01H
		LD	B,A
		ADD	HL,BC
		ADD	A,C
		JR	NC,J2E8E
		LD	A,(HL)
		SET	0,(IY+41)
		LD	(D_BBA7),DE
		LD	(D_BBA5),A
		INC	DE
		CALL	C2B6A
		LD	BC,11
		ADD	HL,BC
		LD	A,(HL)
		LD	(DSBBA6),A
		LD	HL,D_BBA5
J2E8E:		EX	DE,HL
		POP	AF
		SBC	A,A
		CP	A
		RRCA
J2E93:		POP	HL
		POP	BC
		POP	IX
		RET

; Subroutine shift 4 bits left
C2E98:		XOR	A
		ADD	HL,HL
		RLA
		ADD	HL,HL
		RLA
		ADD	HL,HL
		RLA
		ADD	HL,HL
		RLA
		RET

; Subroutine reset deleted files status disk
C2EA2:		PUSH	HL
		EX	(SP),IX
		BIT	0,(IX+24)
		JR	Z,J2F16
		XOR	A
		JR	J2EBF

; Subroutine set deleted files status disk
C2EAE:		PUSH	HL
		EX	(SP),IX
		BIT	7,(IX+25)
	IFNDEF FAT16
		JR	NZ,J2F16
	ENDIF
		BIT	0,(IX+24)
		JR	NZ,J2F16
		LD	A,1
J2EBF:		PUSH	BC
		PUSH	DE
		LD	B,(IX+14)
		DEC	B
		JR	Z,J2EF6
		LD	E,(IX+12)
		LD	D,(IX+13)
		LD	B,(IX+17)
J2ED0:		PUSH	BC
		PUSH	AF
		DEC	A
		LD	B,00H
		JR	NZ,J2EDC
	IFDEF FAT16
		CALL	BUF_3F
	ELSE
		CALL	C2B78
	ENDIF
		JR	J2EE9

J2EDC:		CALL	C2B6A
		CALL	C2D0F
		PUSH	HL
		CALL	C2C38
		SET	6,(HL)
		POP	HL
J2EE9:		CALL	C2CB4
		INC	HL
		INC	HL
		LD	(HL),00H
		POP	AF
		POP	BC
		INC	DE
		DJNZ	J2ED0
		LD	B,A
J2EF6:		LD	A,B
		CP	(IX+24)
		JR	Z,J2F14
		PUSH	BC
		LD	DE,0
		LD	B,1
	IFDEF FAT16
		CALL	BUF_3F
	ELSE
		CALL	C2B78
	ENDIF
		CALL	C2CB7
		CALL	C2C38
	IFDEF FAT16 ; CHKVOL
		PUSH	AF
		LD	A,(IX+19h)	; check VOL
		CP	0FFh
		LD	DE,002Eh
		JR	NZ,VOL_1
		LD	DE,0012h
VOL_1:		POP	AF
	ELSE
		LD	DE,46
	ENDIF
		ADD	HL,DE
		POP	BC
		LD	(HL),B
		LD	(IX+24),B
J2F14:		POP	DE
		POP	BC
J2F16:		EX	(SP),IX
		POP	HL
		RET

; Subroutine clear FAT
C2F1A:		LD	DE,0
		LD	C,A
		LD	B,15
		CALL	C2DD3
		INC	DE
		LD	BC,0FFFFH
		CALL	C2DD3
J2F2A:		INC	DE
		LD	BC,0
		CALL	C2DD3
		PUSH	HL
		LD	BC,22
		ADD	HL,BC
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		SBC	HL,DE
		POP	HL
		JR	NZ,J2F2A
		RET

; Subroutine allocate clusters
; Input:  HL = drive table
;         BC = number of clusters
;         DE = previous cluster in chain (0FFFF if none)
;         A <> 0 clears directory
C2F40:		LD	(D_BBAA),A
		PUSH	DE
		PUSH	BC
		LD	DE,0FFFFH
		LD	(D_BBA3),DE
		PUSH	DE
		LD	DE,1
		JR	J2F54

J2F52:		PUSH	BC
		PUSH	DE
J2F54:		INC	DE
		PUSH	HL
		LD	BC,22
		ADD	HL,BC
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		SBC	HL,DE
		POP	HL
		JR	C,J2F99
		PUSH	DE
		CALL	C2D86
		LD	A,D
		OR	E
		POP	DE
		JR	NZ,J2F54
		LD	B,D
		LD	C,E
		POP	DE
	IFDEF FAT16
		CALL	CLST_5
	ELSE
		PUSH	BC
		BIT	7,D
	ENDIF
		JR	Z,J2F78
		LD	(D_BBA3),BC
J2F78:		CALL	Z,C2DD6
		POP	DE
		LD	A,(D_BBAA)
		OR	A
		CALL	NZ,C2FA9
		POP	BC
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,J2F52
		LD	BC,0FFFFH
		CALL	C2DD6
		CALL	C2EA2
		LD	BC,(D_BBA3)
		POP	DE
		XOR	A
		RET

J2F99:		POP	DE
		POP	DE
	IFDEF FAT16
		CALL	CLST_6
	ELSE
		LD	DE,(D_BBA3)
		BIT	7,D
	ENDIF
		CALL	Z,C2FFC
		POP	DE
		LD	A,0D4H
		OR	A
		RET

; Subroutine clear directory (cluster)
; Input:  DE = cluster number
C2FA9:		PUSH	DE
		LD	A,D
		OR	E
		JR	NZ,J2FC4
		PUSH	HL
		LD	BC,18
		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	H,D
		LD	L,E
		SBC	HL,BC
	IFDEF FAT16 ; BUF_5F
		XOR	A
		LD	(BIT16),A
	ENDIF
		LD	B,L
		DEC	B
		DEC	DE
		POP	HL
		JR	J2FCF

J2FC4:		PUSH	HL
		LD	BC,10
		ADD	HL,BC
		LD	B,(HL)
		POP	HL
		LD	A,B
		CALL	C2DB6
J2FCF:		LD	A,B
		INC	A
		POP	BC
		PUSH	HL
		EX	(SP),IX
		PUSH	BC
		LD	B,A
J2FD7:		PUSH	BC
		JR	NZ,J2FDD
		CALL	C2CB7
J2FDD:		LD	B,00H
	IFDEF FAT16
		CALL	BUF_2F
	ELSE
		CALL	C2B78
	ENDIF
		PUSH	HL
		LD	BC,11
		ADD	HL,BC
		XOR	A
J2FE8:		LD	(HL),A
		INC	HL
		LD	(HL),A
		INC	HL
		DJNZ	J2FE8
		CALL	C2C38
		POP	HL
		POP	BC
		DEC	DE
		XOR	A
		DJNZ	J2FD7
		POP	DE
		EX	(SP),IX
		POP	HL
		RET

; Subroutine delete chain
C2FFC:		PUSH	DE
		CALL	C2D86
		POP	BC
		PUSH	DE
		LD	D,B
		LD	E,C
		LD	BC,0
		CALL	C2DD6
		POP	DE
		LD	A,D
		OR	E
	IFDEF FAT16
		CALL	CLST_7
	ELSE
		RET	Z
		BIT	7,D
	ENDIF
		JR	Z,C2FFC
		RET

; Subroutine get format choice string
C3013:		LD	A,4		; CHOICE
		CALL	C34D4		; call disk driver function
		RET	NZ
		LD	A,E
		OR	D
		RET	Z
		PUSH	HL
		EX	(SP),IX
		EX	DE,HL
		LD	A,(IX+0)
		CALL	RDSLT
		EX	DE,HL
		EX	(SP),IX
		POP	HL
		OR	A
		LD	A,0F0H
		RET	Z
		XOR	A
		RET

; Subroutine format disk
C3030:		BIT	7,A
		JR	NZ,J303F
		LD	E,C
		LD	C,D
		LD	D,B
		LD	B,A
		LD	A,5		; DSKFMT
		CALL	C34D4		; call disk driver function
		RET	NZ
		INC	A
J303F:		PUSH	AF
		LD	IX,I_B6D4
		LD	DE,0
		LD	B,1
		LD	A,(DATA_S)
		LD	C,A
		LD	A,00H		; DSKIO read
		CALL	C34D4		; call disk driver function
		POP	BC
		RET	NZ
		PUSH	BC
		CALL	C334E
		LD	DE,1
		JR	NZ,J3063
		LD	E,(IX+14)
		LD	D,(IX+15)
J3063:		LD	B,1
		LD	A,(DATA_S)
		LD	C,A
		LD	A,00H		; DSKIO read
		CALL	C34D4		; call disk driver function
		POP	BC
		RET	NZ
		LD	A,(IX+1)
		AND	(IX+2)
		INC	A
		JR	NZ,J30E6
		LD	A,(IX+0)
		CP	0F8H
		JR	C,J30E6
		LD	C,A
		PUSH	BC
		LD	DE,0
		LD	B,1
		LD	A,(DATA_S)
		LD	C,A
		LD	A,00H		; DSKIO read
		CALL	C34D4		; call disk driver function
		POP	BC
		RET	NZ
		PUSH	BC
		LD	A,C
		CALL	C3382
		POP	BC
		RET	NZ
		BIT	0,B
		JR	Z,J30D7
		PUSH	HL
		CALL	C32CB
		JR	Z,J30B9
		LD	HL,IBOOTCODE
		LD	DE,ISB6F2
		LD	BC,0099H
		LDIR			; copy bootsector code
		EX	DE,HL
		LD	DE,0FFB7H
J30B2:		LD	(HL),B
		INC	HL
		INC	DE
		LD	A,D
		OR	E
		JR	NZ,J30B2
J30B9:		LD	HL,(RANDOM+0)
		LD	A,(RANDOM+2)
		LD	B,A
		XOR	A
		SRL	L
		RLA
		SRL	H
		RLA
		SRL	B
		RLA
		LD	C,A
		LD	(DSB6FB),HL
		LD	(DSB6FD),BC
		XOR	A
		LD	(DSB6FA),A
		POP	HL
J30D7:		LD	DE,0
		LD	B,1
		LD	A,(DATA_S)
		LD	C,A
		LD	A,1		; DSKIO write
		CALL	C34D4		; call disk driver function
		RET

J30E6:		LD	A,0F6H
		RET

	IF OPTM = 0
		; Not used
Q_30E9:		DEFB	0EBH,0FEH	; x86 JMP +256
		DEFB	090H		; x86 NOP
		DEFB	"MSXDOS22"
	ENDIF

IBOOTCODE:	
		DEPHASE

; ------------------------------------------------------------------------------
; MSXDOS 2.2 boot sector code starting at offset 0x1e
; Used in the format routine to initialize a floppy disk

		PHASE  0C01EH

RC01E:  	JR      RC030
		DEFB    "VOL_ID"
		DEFB    0
		DEFW    0FFFFH,0FFFFH
		DEFS    5,0
RC030:  	RET     NC
		LD      (RC069+1),DE
		LD      (RC071+1),A
		LD      (HL),RC067 % 256	; rem: LOW RC067
		INC     HL
		LD      (HL),RC067 / 256	; rem: HIGH RC067
RC03D:  	LD      SP,KBUF+256
		LD      DE,RC0AB
		LD      C,00FH
		CALL    BDOS
		INC     A
		JR      Z,RC071
		LD      DE,00100H
		LD      C,01AH
		CALL    BDOS
		LD      HL,1
		LD      (RC0AB+14),HL
		LD      HL,04000H-00100H
		LD      DE,RC0AB
		LD      C,027H
		CALL    BDOS
		JP      00100H

RC067:  	DEFW    RC069

RC069:  	CALL    0
		LD      A,C
		AND     0FEH
		SUB     002H
RC071:  	OR      000H
		JP      Z,BASENT
		LD      DE,RC085
		LD      C,009H
		CALL    BDOS
		LD      C,007H
		CALL    BDOS
		JR      RC03D

RC085:  	DEFB    "Boot error",13,10
		DEFB    "Press any key for retry",13,10
		DEFB    "$"

RC0AB:  	DEFB    0,"MSXDOS  SYS"
RC0B7:
		DEPHASE
; ------------------------------------------------------------------------------

		PHASE  	IBOOTCODE+RC0B7-RC01E

; Subroutine validate FIB / check disk change
; Input:  C  = drive
;         B  = type (0 = for disk, 1 = for file, 2 = flush dirty sector buffers)
;         IX = pointer to FIB
; VAL_FIB:
C318D:		LD	A,C
		CALL	C3606			; logical to physical drive
		LD	(D_BBEB),A
		LD	E,(HL)
		INC	HL
		LD	D,(HL)			; de = pointer to drive table
		LD	A,D
		OR	E
		LD	A,C
		RET	Z			; z=invalid drive
		PUSH	DE
		DEC	B			; validate for file?
		JR	NZ,J320B		; nz=no

		; validate for file
J319F:		PUSH	IX
		POP	HL			; hl =  pointer in FIB
		LD	BC,26
		ADD	HL,BC
		EX	DE,HL			; de = pointer to disk serial in FIB
		POP	HL
		CALL	C3627			; update disk change counter for all drives
		PUSH	IX
		PUSH	HL
		LD	C,9
		ADD	HL,BC
		LD	A,(HL)			; a = disk change counter
		OR	A
		JR	Z,J31C6			; z=skip test same disk
		LD	C,16
		ADD	HL,BC
		LD	C,A
		PUSH	DE
		CALL	C32F3			; compare disk serial of drive table and FIB
		POP	DE
		JR	Z,J31CF			; z=same serial
		LD	A,(D_BBEB)
		CALL	C2C4A			; flush sector buffers of physical drive
J31C6:		POP	HL
		CALL	C32FD			; read boot sector and make valid
		PUSH	HL
		LD	C,1			; set flag to update drive table
		JR	J31E1

J31CF:		POP	HL
		PUSH	HL
		DEC	C
		JR	Z,J31DC			; z=skip check, treat as disk changed
		DEC	C
		JR	NZ,J31F9		; nz=skip check, winit 0.5s of last disk operation of drive
		CALL	C3294			; get driver disk change status
		JR	Z,J31F9			; z=unchanged
J31DC:		CALL	C32FD			; read boot sector and make valid
		LD	C,00H			; reset flag to update drive table
J31E1:		CALL	C3255			; restart disk change counter
		PUSH	DE
		CALL	C32CB			; get pointer to disk serial in boot sector
		POP	DE
		CALL	C32F3			; compare disk serial
J31EC:		POP	HL
		PUSH	HL
		JR	NZ,J31FB		; nz=not same disk
		DEC	C
		JR	NZ,J31F9		; nz=don't update drive table
		CALL	C3401			; update drive table and BPB
		CALL	C2C59			; mark sector buffers of drive table unused
J31F9:		JR	J324A			; done

J31FB:		LD	A,_WFILE		; wrong disk for file
		LD	DE,0FFFFH
		CALL	C3689			; handle error
		POP	HL
		JR	NZ,J3231		; nz=ignore, update drive table and serials
		POP	IX
		PUSH	HL
		JR	J319F			; retry

		; validate for disk
J320B:		POP	HL
		CALL	C2C49			; flush sector buffers of drive table
		CALL	C3627			; update disk change counter for all drives
		PUSH	IX
		PUSH	HL
		LD	DE,9
		ADD	HL,DE
		LD	A,(HL)			; a = disk change counter
		POP	HL
	IFDEF DOSV231
		; 1st check validation type then process disk change counter
		; this prevents unnecessary boot sector read and drive table update 
		DEC	B
		JR	Z,J3237			; z=flush dirty sector buffers only
		OR	A
		JR	Z,J322E			; z=initialize, treat as disk changed
	ELSE
		OR	A
		JR	Z,J322E
		DEC	B
		JR	Z,J3237
	ENDIF
		DEC	A
		JR	Z,J322E			; z=invalid, treat as disk changed
		DEC	A
		JR	NZ,J323A		; nz=within 0.5s of last disk operation of drive
		CALL	C3294			; get driver disk change status
		JR	C,J322E			; c=driver doesn't know, treat as disk changed
		JR	Z,J323A			; z=unchanged
J322E:		CALL	C32FD			; read boot sector and make valid
J3231:		CALL	C3401			; uppdate drive table and BPB
		CALL	C2C59			; mark sector buffers of drive table unused
J3237:		CALL	C3255			; restart disk change counter
J323A:		POP	DE
		PUSH	DE
		PUSH	HL
		LD	BC,25
		ADD	HL,BC			; disk serial of drive table
		EX	DE,HL
		LD	C,26
		ADD	HL,BC			; disk serial of FIB
		EX	DE,HL
		LD	C,4
		LDIR				; update FIB disk serial
J324A:		POP	HL
		POP	IX
		XOR	A
		RET

; Subroutine execute disk driver read/write sectors (with disk check)
; Input:  A = function (0 = read sectors, 1 = write sectors)
C324F:		CALL	C3264
	IFDEF FAT16
		CALL	C34D8
	ELSE
		CALL	C34D4			;  call disk driver function
	ENDIF

; Subroutine restart disk change counter / next 0.5 seconds no disk change
C3255:		PUSH	HL
		CALL	C3627
		EX	(SP),IX
		LD	(IX+9),2+5
		EX	(SP),IX
		POP	HL
		OR	A
		RET

; Subroutine validate if same disk
C3264:		CALL	C3627
		PUSH	AF
		PUSH	DE
		PUSH	HL
		LD	DE,9
		ADD	HL,DE
		LD	A,2
		CP	(HL)			; disk change counter: init or invalid?
		POP	HL
		JR	C,J3291			; c=yes, quit
		PUSH	BC
		CALL	Z,C3294			; z=get disk change status from driver
		JR	Z,J3290			; z=unchanged
		PUSH	IX
J327C:		CALL	C32FD			; read boot sector and make valid
		CALL	C32B5			; compare disk serial with drive table
		JR	Z,J328E
		LD	A,_WDISK		; wrong disk
		LD	DE,0FFFFH
		CALL	C3689			; handle error
		JR	J327C			; retry

J328E:		POP	IX
J3290:		POP	BC
J3291:		POP	DE
		POP	AF
		RET

; Subroutine get disk change status
; Output: Zx = set: unchanged, reset: changed
;		Cx = set: disk driver does not know
C3294:		PUSH	BC
		PUSH	DE
		LD	A,2			; DSKCHG
		CALL	C34D4			; call disk driver function
		JR	Z,J32A7
		CP	_NRDY			; not ready?
		LD	DE,0FFFFH
		CALL	Z,C3689
		LD	B,0FFH
J32A7:		DEC	B
		JR	Z,J32B2
		INC	B
		JR	NZ,J32B2
		LD	A,(DSK_CHK)
		INC	A
		SCF
J32B2:		POP	DE
		POP	BC
		RET

; Subroutine compare disk serial boot sector and drive table
; Input:  IX = pointer to bootsector
;         HL = pointer to drive table
C32B5:		PUSH	HL
		CALL	C32CB
		EX	DE,HL
		POP	HL
		PUSH	HL
		LD	BC,25
		ADD	HL,BC
		CALL	C32F3
		JR	NZ,J32C9
		LD	A,(HL)
		CP	(IX+21)
J32C9:		POP	HL
		RET

; Subroutine get pointer to disk serial
; Input:  IX = pointer to bootsector
; Output: HL = pointer to disk serial
C32CB:		PUSH	IX
		POP	HL
		LD	DE,32			; bootrecord offset to volume_id
		ADD	HL,DE
		LD	DE,I32E8
		LD	B,6
J32D7:		LD	A,(DE)
		CP	(HL)			; bootsector contains "VOL_ID" string?
		JR	NZ,J32E2		; nz=no
		INC	HL
		INC	DE
		DJNZ	J32D7
		INC	HL
		XOR	A
		RET

J32E2:  
	IFDEF FAT16 ; GETVOL
		PUSH	IX
		POP	HL
		LD	DE,000Ah
		ADD	HL,DE
		LD	A,(HL)			; UNDEL FLG (DOS1,FAT16)
		CP	01h			; FAT16 partition?
		JR	Z,PAT_47		; z=yes
		XOR	A
PAT_47:		LD	HL,I32E8+6
		LD	(HL),A
		INC	HL
	ELSE
		LD	HL,I32EF
	ENDIF
		XOR	A
		DEC	A
		RET

I32E8:		DEFB	"VOL_ID",0
I32EF:		DEFW	0FFFFH,0FFFFH

; Subroutine compare disk serials
C32F3:		LD	B,4
J32F5:		LD	A,(DE)
		SUB	(HL)
		RET	NZ
		INC	DE
		INC	HL
		DJNZ	J32F5
		RET

; Subroutine read bootsector and make valid
C32FD:		PUSH	BC
		PUSH	DE
		LD	IX,I_B6D4
J3303:		LD	DE,0
		LD	B,1
		LD	A,(DATA_S)
		LD	C,A
		LD	A,00H		; DSKIO read
		CALL	C34D4		; call disk driver function
		JR	NZ,J3346
		CALL	C334E
		JR	Z,J3341
	IFDEF FAT16
		JR	J3344
	ELSE
		LD	DE,1
		LD	B,1
		LD	A,(DATA_S)
		LD	C,A
		LD	A,00H		; DSKIO read
		CALL	C34D4		; call disk driver function
		JR	NZ,J3346
		LD	A,(IX+1)
		AND	(IX+2)
		INC	A
		JR	NZ,J3344
		LD	A,(IX+0)
		CP	0F8H
		JR	C,J3344
		CALL	C3382
		JR	NZ,J3346
		LD	(IX+32),00H
	ENDIF
J3341:		POP	DE
		POP	BC
		RET

J3344:		LD	A,0F6H
J3346:		LD	DE,0FFFFH
		CALL	C3689
		JR	J3303

; Subroutine validate bootsector and update DPB
; Input:  IX = pointer to bootsector
;         HL = pointer to DPB
C334E:		PUSH	IX
		EX	(SP),HL
		LD	DE,11
		ADD	HL,DE
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		INC	HL
		SUB	02H
		OR	D
		JR	NZ,J337F
		OR	(HL)
		JR	Z,J337F
		NEG
		AND	(HL)
		CP	(HL)
		INC	HL
		JR	NZ,J337F
		INC	HL
		INC	HL
		LD	A,(HL)
		DEC	A
		CP	07H
		JR	NC,J337F
		LD	DE,6
		ADD	HL,DE
		LD	A,(HL)
	IFDEF FAT16 ; GETDPB
		POP	HL
		XOR	A
		RET
	ELSE
		INC	HL
		DEC	A
		CP	0CH
		JR	NC,J337F
		LD	A,(HL)
		OR	A
		POP	HL
		RET
	ENDIF

J337F:		POP	HL
		OR	H
		RET

; Subroutine create bootsector BPB
C3382:		LD	B,A
		LD	A,3		; GETDPB
		CALL	C34D4		; call disk driver function
		RET	NZ
		PUSH	HL
		LD	DE,2
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		PUSH	DE
		EX	(SP),IX
		POP	HL
		PUSH	HL
		LD	DE,11
		ADD	HL,DE
		LD	C,(IX+2)
		LD	B,(IX+3)
		LD	(HL),C
		INC	HL
		LD	(HL),B
		INC	HL
		LD	A,(IX+6)
		INC	A
		LD	(HL),A
		INC	HL
		LD	C,(IX+8)
		LD	B,(IX+9)
		LD	(HL),C
		INC	HL
		LD	(HL),B
		INC	HL
		LD	A,(IX+10)
		LD	(HL),A
		INC	HL
		EX	DE,HL
		LD	L,(IX+12)
		LD	H,(IX+13)
		PUSH	HL
		LD	C,(IX+17)
		LD	B,(IX+18)
		OR	A
		SBC	HL,BC
		LD	B,(IX+5)
J33CD:		ADD	HL,HL
		DJNZ	J33CD
		DEC	HL
		LD	L,(IX+11)
		DEC	L
		INC	HL
		EX	DE,HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		EX	DE,HL
		LD	L,(IX+14)
		LD	H,(IX+15)
		DEC	HL
		LD	B,(IX+7)
		DEFB	00EH
J33E7:		ADD	HL,HL
		DJNZ	J33E7
		POP	BC
		ADD	HL,BC
		EX	DE,HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		LD	A,(IX+1)
		LD	(HL),A
		INC	HL
		LD	A,(IX+16)
		LD	(HL),A
		INC	HL
		XOR	A
		LD	(HL),A
		POP	IX
		POP	HL
		RET

; Subroutine update drive table with bootsector BPB info
; Input:  HL = pointer to drive table
;         IX = pointer to bootsector
C3401:		PUSH	HL
		LD	BC,10
		ADD	HL,BC
		LD	A,(IX+13)	; get cluster size (in sectors)
		DEC	A
		LD	(HL),A		; set cluster mask (0x0A)
		INC	HL
		LD	C,00H
J340E:		INC	C
		RRCA
		JR	C,J340E
		LD	(HL),C		; set cluster shift (0x0B)
		INC	HL
		LD	E,(IX+14)	; get number of unused sectors
		LD	(HL),E		; set reserved sectors (0x0C)
		INC	HL
		LD	D,(IX+15)	; "
		LD	(HL),D		; "
		INC	HL
		PUSH	DE
		LD	B,(IX+16)	; get number of FAT's
		LD	(HL),B		; set number of FAT's (0x0E)
		INC	HL
		LD	E,(IX+17)	; get first directory sector
		LD	D,(IX+18)
		LD	A,E
		AND	0FH
		LD	(HL),A		; set remainder of directory entries (no whole sector) (0x0F)
		LD	A,4
J3430:		SRL	D
		RR	E
		DEC	A
		JR	NZ,J3430
	IFNDEF FAT16
		CP	(HL)
	ENDIF
		INC	HL
		LD	(HL),E		; set number of directory sectors (whole sectors) (0x10)
		INC	HL
		JR	NC,J343E
		INC	DE
J343E:		LD	A,(IX+22)	; get size of FAT
		LD	(HL),A		; set number of sectors per FAT (0x11)
		INC	HL
		EX	(SP),HL
		PUSH	DE
		LD	E,A
		XOR	A
		LD	D,A
J3448:		ADD	A,E
	IFDEF FAT16 ; FSIZE3
		JR	NC,FSIZ_3
		INC	D
FSIZ_3:		INC	E
		DEC	E
		JR	NZ,J344C
	ELSE
		JR	NC,J344C
	ENDIF
		INC	D
J344C:		DJNZ	J3448
		LD	E,A
		ADD	HL,DE
		EX	DE,HL
		POP	HL
		ADD	HL,DE
		EX	(SP),HL
		LD	(HL),E		; set first sector of rootdirectory (0x12)
		INC	HL
		LD	(HL),D		; "
		INC	HL
		POP	DE
		LD	(HL),E		; set first sector of data area (0x14)
		INC	HL
		LD	(HL),D		; "
		INC	HL
		PUSH	HL
		LD	L,(IX+19)	; get total number of sectors
		LD	H,(IX+20)
	IFDEF FAT16 ; TALCLS
		;Total of cluster
		LD	A,L		; HL=BOOT +13h,14h
		OR	H
		JR	Z,WINFMT	; Format with Windows95
		SBC	HL,DE
		JR	J3467		; 12bitFAT
WINFMT:		LD	L,(IX+20h)	; Cluster size
		LD	H,(IX+21h)
		LD	A,(IX+22h)
		OR	A
		SBC	HL,DE
		SBC	A,0
WINFM_:		DEC	C
		JR	Z,J3470
		SRL	A
		RR	H
		RR	L
		JR	WINFM_
	ELSE
		OR	A
		SBC	HL,DE
	ENDIF
J3467:		DEC	C
		JR	Z,J3470
		SRL	H
		RR	L
		JR	J3467
J3470:		INC	HL
		EX	DE,HL
		POP	HL
		LD	(HL),E		; set number of clusters + 1 on disk (0x16)
		INC	HL
		LD	(HL),D
	IFDEF FAT16 ; DPBSET
		PUSH	DE		; total clusters + 1
	ENDIF
		INC	HL
		PUSH	HL
		CALL	C32CB
		POP	DE
		DEC	HL
		LD	BC,5
		LDIR			; set dirty flag bit (0x18) and volume serial number (0x19)
		EX	DE,HL
		LD	A,(IX+21)	; get Media ID
		LD	(HL),A		; set Media ID (0x1D)
	IFDEF FAT16 ; DPBSET
		POP	DE
		PUSH	HL
		LD	HL,4085		; if total clusters < 4085 then FAT12 else FAT16
		SBC	HL,DE
		POP	HL
		JR	NC,DPB_1
		RES	7,(HL)		; clear FAT12 flag in Media ID (0x1D)
	ENDIF
DPB_1:		POP	HL		; HL=pointer to begin of drivetable
		PUSH	HL
		INC	HL
		INC	HL
		LD	E,(HL)		; DE=pointer to DPB of drive
		INC	HL
		LD	D,(HL)		; "
		EX	DE,HL
		INC	HL
		LD	(HL),A		; set MEDIA
		INC	HL
		LD	(HL),00H	; set SECSIZ
		INC	HL
		LD	(HL),02H	; "
		INC	HL
		LD	(HL),0FH	; set DIRMSK
		INC	HL
		LD	(HL),04H	; set DIRSHFT
		INC	HL
		EX	DE,HL
		LD	BC,7
		ADD	HL,BC
		LD	BC,5
		LDIR			; set CLUSMSK,CLUSSFT,FIRFAT,FATCNT
		LD	C,(HL)
		INC	HL
		LD	A,(HL)
		INC	HL
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		OR	C
		LD	(DE),A		; set MAXENT
		INC	DE
		PUSH	HL
		INC	HL
		INC	HL
		INC	HL
		LD	BC,4
		LDIR			; set FIRREC,MAXCLUS
		POP	HL
		LD	BC,3		; set FATSIZ,FIRDIR
		LDIR
		POP	HL

; Subroutine current directory invalid
; Input:  HL = pointer to drive table
C34C3:		PUSH	HL
		PUSH	BC
		LD	BC,31
		ADD	HL,BC
		POP	BC
	IFDEF FAT16
		CALL	CLST_A
	ELSE
		XOR	A
		BIT	7,(HL)
	ENDIF
		JR	NZ,J34D2
		LD	(HL),A
		DEC	HL
		LD	(HL),A
J34D2:		POP	HL
		RET

; Subroutine execute disk driver function
; Input:  A  = function
;         HL = drive table
;	  BC,DE,IX = disk driver HL
C34D4:		CP	06H
		JR	NC,J351D
	IFDEF FAT16 ; DSKROM
		CP	02h
		JR	NC,C34D8
		PUSH	AF
		XOR	A
		LD	(RW_16),A
		POP	AF
	ENDIF
C34D8:		PUSH	IY
		PUSH	HL
		PUSH	HL
		EX	(SP),IX
		POP	HL
		PUSH	HL
		EXX
		PUSH	HL
		PUSH	DE
		PUSH	BC
		EX	AF,AF'
		PUSH	AF
		LD	A,(IX+8)
		DEC	A
		LD	(TARGET),A
		LD	A,(IX+6)
		EX	AF,AF'
		LD	C,A
		CP	01H
		LD	A,0FFH
		JR	Z,J34F9
		INC	A
J34F9:		LD	(D_BBEA),A
		LD	HL,I3522
		LD	B,0
		ADD	HL,BC
		ADD	HL,BC
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		LD	A,(P2_TPA)
		EX	AF,AF'
		CALL	C3521
		EX	AF,AF'
		POP	AF
		EX	AF,AF'
		EXX
		POP	BC
		POP	DE
		POP	HL
		EXX
		POP	IX
		POP	HL
		POP	IY
		OR	A
		RET

J351D:		LD	A,_INTER
		OR	A
		RET

; Subroutine execute
C3521:		JP	(HL)

; Driver function jump table
I3522:		DEFW	I352E		; 0 = DSKIO read
		DEFW	I3531		; 1 = DSKIO write
		DEFW	I354B		; 2 = DSKCHG
		DEFW	I3566		; 3 = GETDPB
		DEFW	I357C		; 4 = CHOICE
		DEFW	I3587		; 5 = DSKFMT

; Subroutine function DSKIO read
I352E:		OR	A
		JR	J3532

; Subroutine function DSKIO write
I3531:		SCF
J3532:		EX	AF,AF'
		LD	HL,04000H
		EXX
		LD	A,C
	IFDEF FAT16 ; SETNUM
		;Set to bit16-23 of sector number at registerC
		PUSH	AF
		LD	A,(IX+1Dh)
		BIT	7,A
		JR	NZ,MED_ID	; FAT12
		LD	A,(RW_16)
		LD	C,A		; FAT16 C=bit16-23
		POP	AF
		JR	SETN1
MED_ID:		LD	C,A		; C=MEDIA ID
		POP	AF
	ELSE
		LD	C,(IX+29)
	ENDIF
SETN1:		PUSH	BC
		CALL	C35C6
		JR	C,J3544
		POP	BC
		XOR	A
		RET

J3544:		EX	AF,AF'
		POP	AF
		SUB	B
		LD	B,A
		EX	AF,AF'
		JR	J359D

; Subroutine function DSKCHG
I354B:		EX	AF,AF'
		LD	HL,04003H
		EXX
		LD	B,00H
		LD	C,(IX+29)
		LD	L,(IX+2)
		LD	H,(IX+3)
		CALL	C35C6
		JR	C,J3562
		XOR	A
		RET

J3562:		LD	B,00H
		JR	J359D

; Subroutine function GETDPB
I3566:		EX	AF,AF'
		LD	HL,04006H
		EXX
		LD	C,(IX+29)
		LD	L,(IX+2)
		LD	H,(IX+3)
		CALL	C35C6
		LD	A,0FFH
		RET	C
		XOR	A
		RET

; Subroutine function CHOICE
I357C:		EX	AF,AF'
		LD	HL,04009H
		EXX
		CALL	C35C6
		EX	DE,HL
		XOR	A
		RET

; Subroutine function DSKFMT
I3587:		LD	HL,0400CH
		EXX
		PUSH	DE
		LD	D,A
		LD	A,B
		EX	AF,AF'
		LD	A,C
		POP	BC
		CALL	C35C6
		JR	C,J3598
		XOR	A
		RET

J3598:		LD	HL,I35BC
		JR	J35A0

J359D:		LD	HL,I35AE
J35A0:		RRCA
		CP	(HL)
		JR	NC,J35AB
		INC	HL
		LD	E,A
		LD	D,0
		ADD	HL,DE
		LD	A,(HL)
		RET

J35AB:		LD	A,0FDH
		RET

I35AE:		DEFB	13+1
		DEFB	_WPROT
		DEFB	_NRDY
		DEFB	_DATA
		DEFB	_SEEK
		DEFB	_RNF
		DEFB	_WRERR
		DEFB	_DISK
		DEFB	_DISK
		DEFB	_DISK
		DEFB	_NDOS
		DEFB	_NCOMP
		DEFB	_UFORM
		DEFB	_NOUPB

I35BC:		DEFB	9+1
		DEFB	_WPROT
		DEFB	_NRDY
		DEFB	_DATA
		DEFB	_SEEK
		DEFB	_RNF
		DEFB	_WRERR
		DEFB	_IPARM
		DEFB	_NORAM
		DEFB	_DISK

; Subroutine execute disk driver function
C35C6:		CALL	K_CHARFLUSH
		EXX
		LD	E,(IX+1)
		LD	D,0
		ADD	HL,DE
		LD	B,(IX+0)
		CALL	PUT_P2
		PUSH	BC
		POP	IY
		PUSH	HL
		POP	IX
		EXX
		EX	AF,AF'
		CALL	GO_DRV
		EI
		EX	AF,AF'
		LD	A,(DATA_S)
		CALL	PUT_P2
		EX	AF,AF'
		RET

; Subroutine disk change status of all drives to flushed
C35EB:		LD	B,8
J35ED:		LD	A,B
		CALL	C3606
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	A,D
		OR	E
		JR	Z,J3602
		LD	HL,9
		ADD	HL,DE
		LD	A,(HL)
		OR	A
		JR	Z,J3602
		LD	(HL),01H
J3602:		DJNZ	J35ED
		XOR	A
		RET

; Subroutine translate drive assigment and get pointer to drive tabel
; Output: HL = pointer to drive table
;		A  = physical drive id
C3606:		PUSH	DE
		OR	A
		JR	NZ,J360D
		LD	A,(CUR_DRV)
J360D:		LD	C,0DBH
		CP	09H
		JR	C,J3614
		XOR	A
J3614:		LD	HL,I_BA1A
		LD	E,A
		LD	D,0
		ADD	HL,DE
		LD	A,(HL)
		LD	E,A
		LD	D,00H
		LD	HL,I_BA23
		ADD	HL,DE
		ADD	HL,DE
		POP	DE
		OR	A
		RET

; Subroutine update counter of all drives
C3627:		PUSH	AF
		PUSH	HL
		LD	HL,(SSECBUF)
		DEC	HL
		LD	A,(HL)
		INC	A
		JR	NZ,J364D
		LD	(HL),00H
		LD	A,(TARGET)
		PUSH	DE
		LD	E,A
		LD	D,0
		LD	HL,I_BA25
		ADD	HL,DE
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	HL,9
		ADD	HL,DE
		POP	DE
		LD	A,(HL)
		OR	A
		JR	Z,J364D
		LD	(HL),01H
J364D:		LD	HL,TIM_TI
		DI
		LD	A,(HL)
		EI
		LD	(HL),00H
		OR	A
		JR	Z,J3683
		PUSH	BC
		PUSH	DE
		PUSH	IX
		LD	C,A
		LD	HL,I_BA25
		LD	B,8
J3662:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,D
		OR	E
		JR	Z,J367D
		PUSH	DE
		POP	IX
		LD	A,(IX+9)
		SUB	02H
		JR	C,J367D
		SUB	C
		JR	NC,J3678
		XOR	A
J3678:		ADD	A,2
		LD	(IX+9),A
J367D:		DJNZ	J3662
		POP	IX
		POP	DE
		POP	BC
J3683:		POP	HL
		POP	AF
		RET

; Subroutine handle error (ignore is valid)
C3686:		OR	A
		JR	C368A

; Subroutine handle error (ignore not recommended)
C3689:		SCF

; Subroutine handle error
; Input:  HL = pointer to drive table
; Output: A = result (0=retry,1=ignore), Zx set if retry
C368A:		PUSH	BC
		PUSH	DE
		PUSH	HL
		EXX
		PUSH	HL
		PUSH	DE
		PUSH	BC
		EXX
		EX	AF,AF'
		PUSH	AF
		EX	AF,AF'
		PUSH	IX
		PUSH	AF
		LD	C,00H
		JR	NC,J369E
		SET	1,C
J369E:		LD	A,(D_BBEA)
		OR	A
		JR	Z,J36A6
		SET	0,C
J36A6:		LD	A,D
		AND	E
		INC	A
		JR	Z,J36AD
		SET	3,C
J36AD:		PUSH	HL
		PUSH	BC
		LD	BC,8
		ADD	HL,BC
		POP	BC
		LD	B,(HL)
		INC	HL
		LD	A,(HL)
		OR	A
		JR	Z,J36BC
		LD	(HL),01H
J36BC:		POP	HL
		POP	AF
		CP	9FH
		JR	Z,J371E
		CP	0F1H
		JR	Z,J3708
		CP	0F2H
		JR	NZ,J36CC
		SET	2,C
J36CC:		PUSH	AF
		LD	HL,I372F-1
J36D0:		INC	HL
		CP	(HL)
		INC	HL
		JR	Z,J36D9
		BIT	7,(HL)
		JR	Z,J36D0
J36D9:		LD	A,(HL)
		ADD	A,A
		BIT	0,C
		JR	Z,J36E0
		INC	A
J36E0:		BIT	2,C
		JR	Z,J36E6
		SET	7,A
J36E6:		EX	AF,AF'
		POP	AF
		LD	H,(IY+9)
		PUSH	HL
		LD	(IY+9),00H
		PUSH	AF
		PUSH	IY
		LD	HL,(KDSK_V)
		CALL	C3726
		POP	IY
		POP	BC
		POP	HL
		LD	(IY+9),H
		DEC	A
		CP	03H
		JR	C,J370A
		XOR	A
		JR	J370A

J3708:		LD	A,1
J370A:		POP	IX
		EX	AF,AF'
		POP	AF
		EX	AF,AF'
		EXX
		POP	BC
		POP	DE
		POP	HL
		EXX
		POP	HL
		OR	A
		JR	Z,J371C
		POP	DE
		POP	BC
		DEC	A
		RET

J371C:		LD	A,9DH
J371E:		PUSH	AF
		CALL	C2C59
		POP	AF

; Subroutine call program abort routine with TPA segments active
C3723:		LD	HL,(KAB_VE)

; Subroutine call routine with TPA segments active
C3726:		PUSH	HL
		LD	HL,PUT_BD
		EX	(SP),HL
		PUSH	HL
		JP	PUT_US

I372F:		DEFB	_WPROT,0
		DEFB	_NRDY,1
		DEFB	_DATA,2
		DEFB	_SEEK,3
		DEFB	_RNF,4
		DEFB	_WRERR,5
		DEFB	_DISK,6
		DEFB	_DISK,7
		DEFB	_DISK,8
		DEFB	_NDOS,9
		DEFB	_NCOMP,10
		DEFB	_UFORM,11
		DEFB	_NOUPB,12
		DEFB	0,6+128

; ---------------------------------------------------------
; *** Functions: 0F-17,21-24,28 ***
; ---------------------------------------------------------

; ---------------------------------------------------------
; Function $0F _FOPEN
; ---------------------------------------------------------
F_FOPEN:	LD	(IY+47),04H
		LD	A,2
		LD	C,1
J3753:		CALL	C3A33
		JR	NZ,J378B
		XOR	A
		PUSH	DE
		CALL	C2284
		POP	DE
		LD	HL,(D_BB96)
		PUSH	HL
		LD	A,(D_BB98)
		OR	A
		JR	Z,J376C
		LD	A,(D_B9F3)
		LD	(HL),A
J376C:		CALL	C3D02
		INC	HL
		LD	A,(DE)
		LD	(HL),A
		INC	HL
		LD	(HL),00H
		POP	IX
		CALL	C3CB4
		XOR	A
		CALL	C3C88
		CALL	C3C4E
		LD	A,(D_B9F3)
		CALL	C2C41
J3787:		XOR	A
		LD	L,A
		LD	H,A
		RET

J378B:		LD	HL,00FFH
		RET

; ---------------------------------------------------------
; Function $10 _FCLOSE
; Input:  DE = pointer to FCB
; ---------------------------------------------------------
F_FCLOSE:	CALL	C399D
		JR	NZ,J378B
		LD	DE,(D_BB96)
		CALL	C3A15
		CALL	C223A
		OR	A
		JR	NZ,J378B
		LD	A,(IX+25)
		CALL	C2C41
		JR	J3787

; ---------------------------------------------------------
; Function $11 _SFIRST
; ---------------------------------------------------------
F_SFIRST:	LD	HL,12
		ADD	HL,DE
		LD	A,(HL)
		LD	(D_BB95),A
		LD	(IY+47),04H
		LD	A,(DE)
		ADD	A,A
		SBC	A,A
		AND	10H
		LD	C,00H
		CALL	C3A33
		JR	NZ,J378B
		JR	J37DC

; ---------------------------------------------------------
; Function $12 _SNEXT
; ---------------------------------------------------------
F_SNEXT:	LD	DE,I_B9DA
		LD	HL,I_B99A
		LD	BC,64
		LD	A,(HL)
		CP	0FFH
		LD	A,0D7H
		JP	NZ,J378B
		LDIR
		CALL	C3AF7
		JP	NZ,J378B
J37DC:		PUSH	DE
		LD	HL,I_B9DA
		LD	DE,I_B99A
		LD	BC,64
		LDIR
		POP	DE
		LD	HL,I_B975
		LD	A,(D_B9F3)
		LD	(HL),A
		CALL	C3D02
		LD	A,(D_BB95)
		LD	(HL),A
		INC	HL
		LD	A,(DE)
		LD	(HL),A
		INC	HL
		INC	DE
		INC	DE
		LD	(HL),00H
		INC	HL
		INC	DE
		EX	DE,HL
		LD	BC,18
		LDIR
		LD	IX,I_B975
		XOR	A
		CALL	C3C88
		CALL	C3C4E
		JR	C,F_SNEXT
		LD	HL,I_B975
		LD	DE,(DTA_AD)
		LD	BC,33
		LD	A,1
		CALL	C26F3
		JP	J3787

; ---------------------------------------------------------
; Function $14 _RDSEQ
; Input:  DE = pointer to FCB
; ---------------------------------------------------------
F_RDSEQ:	CALL	C399D
		JR	NZ,J3869
		LD	IX,(D_BB96)
		CALL	C3C85
		LD	A,(D_B9F8)
		BIT	7,A
		JR	Z,J3850
		LD	BC,128
		LD	DE,(DTA_AD)
		LD	IX,I_B9DA
		XOR	A
		CALL	C2757
		JR	NZ,J3869
		LD	IX,(D_BB96)
		JR	J3860

J3850:		CALL	C3B4E
		JR	Z,J385D
		CALL	C3B9B
		JR	NZ,J3869
		LD	HL,I_B2D4
J385D:		CALL	C3A26
J3860:		CALL	C3C30
		CALL	C3CB4
		JP	J3787

J3869:		LD	HL,1
		RET

; ---------------------------------------------------------
; Function $15 _WRSEQ
; Input:  DE = pointer to FCB
; ---------------------------------------------------------
F_WRSEQ:	CALL	C399D
		JR	NZ,J3869
		LD	IX,(D_BB96)
		CALL	C3C85
		CALL	C3B7B
		LD	IX,I_B9DA
		LD	BC,128
		LD	DE,(DTA_AD)
		XOR	A
		CALL	C2753
		JR	NZ,J3869
		LD	IX,(D_BB96)
		CALL	C3C30
		CALL	C3CB4
		LD	A,(IX+32)
		LD	HL,(D_BB96)
		LD	BC,15
		ADD	HL,BC
		CP	(HL)
		JR	C,J38A5
		LD	(HL),A
J38A5:		JP	J3787

; ---------------------------------------------------------
; Function $16 _FMAKE
; ---------------------------------------------------------
F_FMAKE:	LD	HL,12
		ADD	HL,DE
		LD	A,(HL)
		OR	A
		JR	Z,J38B7
		CALL	F_FOPEN
		OR	A
		JP	Z,J3787
J38B7:		LD	(IY+47),00H
		XOR	A
		LD	C,A
		JP	J3753

; ---------------------------------------------------------
; Function $13 _FDEL
; ---------------------------------------------------------
F_FDEL:		LD	A,0FFH
		LD	(D_BB90),A
		LD	BC,00FFH
		PUSH	BC
		LD	(IY+47),04H
		XOR	A
		LD	C,A
		CALL	C3A33
		JR	J38DB

J38D4:		POP	BC
		LD	C,00H
		PUSH	BC
J38D8:		CALL	C3AF7
J38DB:		JR	NZ,J3920
		BIT	0,(IX+14)
		JR	NZ,J38D8
		LD	A,1
		CALL	C2332
		OR	A
		JR	Z,J38D4
		JR	J3920

; ---------------------------------------------------------
; Function $17 _FREN
; ---------------------------------------------------------
F_FREN:		LD	BC,00FFH
		PUSH	BC
		PUSH	DE
		LD	IX,I_B9DA
		LD	(IX+31),00H
		LD	HL,17
		ADD	HL,DE
		LD	DE,I_B975
		EX	DE,HL
		LD	A,(DE)
		CALL	C173A
		POP	DE
		XOR	A
		LD	C,A
		CALL	C3A33
		JR	J3915

J390E:		POP	BC
		LD	C,00H
		PUSH	BC
		CALL	C3AF7
J3915:		JR	NZ,J3920
		LD	BC,I_B975
		CALL	C2398
		OR	A
		JR	Z,J390E
J3920:		LD	B,A
		LD	A,(D_B9F3)
		CALL	C2C41
		POP	HL
		LD	A,L
		OR	A
		RET	Z
		LD	A,B
		RET

; ---------------------------------------------------------
; Function $21 _RDRND
; ---------------------------------------------------------
F_RDRND:	LD	A,1
		CALL	C3B0F
		JR	NZ,J3957
		LD	A,C
		NEG
		AND	7FH
		LD	C,A
		LD	A,00H
		CALL	NZ,C3C05
		JR	J394A

; ---------------------------------------------------------
; Function $22 _WRRND
; ---------------------------------------------------------
F_WRRND:	XOR	A
		DEFB	021H

; ---------------------------------------------------------
; Function $28 _WRZER
; ---------------------------------------------------------
F_WRZER:	LD	A,2
		CALL	C3B0F
		JR	NZ,J3957
J394A:		LD	IX,(D_BB96)
		CALL	C3C4E
		CALL	C3CB4
		JP	J3787

J3957:		LD	HL,1
		RET

; ---------------------------------------------------------
; Function $23 _FSIZE
; ---------------------------------------------------------
F_FSIZE:	LD	(IY+47),04H
		LD	A,2
		LD	C,00H
		CALL	C3A33
		JP	NZ,J378B
		LD	C,(IX+21)
		LD	B,(IX+22)
		LD	E,(IX+23)
		LD	D,(IX+24)
		XOR	A
		LD	H,A
		SUB	C
		AND	7FH
		LD	L,A
		ADD	HL,BC
		JR	NC,J397F
		INC	DE
J397F:		ADD	HL,HL
		LD	A,H
		EX	DE,HL
		ADC	HL,HL
		LD	IX,(D_BB96)
		LD	(IX+33),A
		LD	(IX+34),L
		LD	(IX+35),H
		JP	J3787

; ---------------------------------------------------------
; Function $24 _SETRND
; ---------------------------------------------------------
F_SETRND:	PUSH	DE
		POP	IX
		CALL	C3C85
		JP	J3787

; ---------------------------------------------------------
; *** Subroutines: FIB and FCB ***
; ---------------------------------------------------------

; Subroutine rebuild FIB from FCB
; Input:  DE = pointer to FCB
C399D:		EX	DE,HL
		LD	(D_BB96),HL
		LD	IX,I_B9DA
		LD	(IX+0),0FFH
		LD	A,(HL)
		AND	0FH
		LD	(IX+25),A
		LD	DE,D_B9EF
		LD	BC,16
		ADD	HL,BC
		LD	BC,4
		LDIR
		LD	DE,I_B9F4
		LD	BC,4
		LDIR
		LD	DE,I_B9FF
		LD	BC,8
		LDIR
	IFDEF FAT16 ; CLUST
		LD	HL,(I_B9FA)	; FCB+20h DPB address
		CALL	CHKDRV
		RET	Z		; FAT16
	ENDIF
		LD	A,(IX+42)
		LD	B,00H
		BIT	6,A
		JR	Z,J39D6
		LD	B,1
J39D6:		LD	(IX+14),B
		LD	B,00H
		BIT	5,A
		JR	Z,J39E1
		LD	B,0A4H
J39E1:		BIT	4,A
		JR	Z,J39E7
		SET	6,B
J39E7:		LD	(IX+30),B
		LD	B,00H
		BIT	7,A
		JR	Z,J39F2
		LD	B,80H
J39F2:		LD	(IX+49),B
		AND	0FH
		LD	(IX+42),A
		XOR	A
		BIT	7,(IX+30)
		RET	Z
		LD	L,(IX+26)
		LD	H,(IX+27)
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	L,(IX+28)
		LD	H,(IX+29)
		SBC	HL,DE
		RET	Z
		LD	A,0B7H
		RET

; Subroutine update FIB with file name from FCB
C3A15:		LD	IX,I_B9DA
		LD	HL,ISB9DB
		INC	DE
		LD	A,(DE)
		LD	(IX+31),02H
		CALL	C173A
		RET

; Subroutine transfer record from sequential read buffer
C3A26:		LD	DE,(DTA_AD)
		LD	BC,128
		LD	A,1
		CALL	C26F3
		RET

; Subroutine get directory entry and setup FIB
; Input:  DE = pointer to FCB
C3A33:		PUSH	AF
		XOR	A
		LD	(D_BB98),A
		POP	AF
		LD	B,A
		PUSH	BC
		LD	(D_BB96),DE
		LD	IX,I_B9DA
		LD	(IX+31),A
		PUSH	AF
		PUSH	DE
		INC	DE
		LD	HL,I_B8F4
		LD	A,(DE)
		CALL	C173A
		POP	DE
		POP	AF
		LD	B,A
		LD	A,(DE)
		AND	0FH
		LD	DE,I_B8F4
J3A59:		LD	C,8
		CALL	C12C3
		JR	NZ,J3A8A
		OR	C
		LD	A,0D9H
		JR	NZ,J3A8A
		PUSH	HL
		LD	HL,I_B926
		LD	DE,I_B9FA
		LD	BC,11
		PUSH	DE
		LDIR
		POP	DE
		POP	HL
		CALL	C1A49
J3A77:		OR	A
		JR	NZ,J3A8A
		PUSH	DE
		PUSH	HL
		CALL	C198F
		CALL	C1A3B
	IFDEF FAT16
		CALL	STOR_7
	ELSE
		EX	DE,HL
		LDIR
	ENDIF
		POP	HL
		POP	DE
		POP	BC
		XOR	A
		RET

J3A8A:		POP	BC
		BIT	0,C
		JR	NZ,J3A97
		PUSH	AF
		XOR	A
		LD	(D_BB98),A
		POP	AF
		OR	A
		RET

J3A97:		LD	(D_B400),A
		PUSH	BC
		LD	HL,I3AF0
		LD	DE,I_B2D4
		LD	A,0FFH
		LD	(D_BB90),A
		LD	B,0FFH
		LD	A,1
		CALL	C0EDF
		OR	A
		POP	BC
		RET	NZ
		PUSH	BC
		LD	C,4
		LD	DE,I_B2D4
		LD	IX,I_B9DA
		XOR	A
		CALL	C12C3
		POP	DE
		RET	NZ
		LD	A,B
		AND	05H
		JR	Z,J3AEB
		LD	A,0FFH
		LD	(D_BB98),A
		PUSH	DE
		LD	HL,(D_BB9E)
		LD	A,B
		AND	18H
		JR	Z,J3AD6
		LD	(HL),5CH	; "\"
		INC	HL
J3AD6:		LD	DE,I_B8F4
J3AD9:		LD	A,(DE)
		LD	(HL),A
		INC	HL
		INC	DE
		OR	A
		JR	NZ,J3AD9
		POP	BC
		XOR	A
		LD	C,00H
		PUSH	BC
		LD	DE,I_B2D4
		JP	J3A59

J3AEB:		LD	A,(D_B400)
		OR	A
		RET

I3AF0:		DEFB	"APPEND",0

; Subroutine try to get next directory entry
C3AF7:		LD	IX,I_B9DA
		LD	(IY+47),04H
		CALL	C19F8
		RET	NZ
		LD	DE,I_B9FA
		CALL	C1A87
		LD	C,00H
		PUSH	BC
		JP	J3A77

; Subroutine do random record operation
; Input:  A  = operation code
;         DE = pointer to FCB
C3B0F:		EX	AF,AF'
		CALL	C399D
		RET	NZ
		LD	IX,(D_BB96)
		LD	A,(IX+33)
		LD	C,(IX+34)
		LD	B,(IX+35)
		PUSH	AF
		PUSH	BC
		CALL	C3CA1
		CALL	C3B7B
		POP	HL
		POP	AF
		LD	B,A
		ADD	A,A
		ADC	HL,HL
		LD	A,L
		LD	(IX+12),A
		LD	A,H
		LD	(IX+14),A
		LD	A,B
		AND	7FH
		LD	(IX+32),A
		XOR	A
		EX	AF,AF'
		LD	BC,128
		LD	IX,I_B9DA
		LD	DE,(DTA_AD)
		CALL	C275B
		RET

; Subroutine get pointer to record if it is in the sequential read buffer
C3B4E:		CALL	C3B85
		RET	NZ
		LD	A,(D_BB92)
		LD	B,A
		LD	DE,(DSBB93)
		LD	A,(IX+33)
		LD	L,(IX+34)
		LD	H,(IX+35)
		SUB	B
		SBC	HL,DE
		RET	NZ
		LD	B,A
		LD	A,(D_BB91)
		SUB	01H	; 1
		RET	C
		CP	B
		RET	C
		XOR	A
		SRL	B
		RRA
		LD	C,A
		LD	HL,I_B2D4
		ADD	HL,BC
		XOR	A
		RET

; Subroutine if random record is in sequential read buffer then invalidate sequential read buffer
C3B7B:		CALL	C3B85
		RET	NZ
		LD	A,0FFH
		LD	(D_BB90),A
		RET

; Subroutine check sequential read buffer has drive and startcluster of file
C3B85:		LD	A,(IX+0)
		LD	B,A
		LD	A,(D_BB90)
		CP	B
		RET	NZ
		LD	L,(IX+26)
		LD	H,(IX+27)
		LD	DE,(D_BB8E)
		SBC	HL,DE
		RET

; Subroutine fill sequential read buffer
C3B9B:		LD	A,0FFH
		LD	(D_BB90),A
		LD	A,(IX+33)
		AND	03H
		LD	B,A
		LD	A,8
		SUB	B
		LD	B,A
		XOR	A
		SRL	B
		RR	A
		LD	C,A
		PUSH	IX
		LD	IX,I_B9DA
		LD	DE,I_B2D4
		LD	A,0FFH
		CALL	C2757
		POP	IX
		JR	Z,J3BC6
		CP	0C7H
		JR	NZ,J3C01
J3BC6:		LD	A,B
		OR	C
		JR	Z,J3C01
		LD	HL,127
		ADD	HL,BC
		ADD	HL,HL
		LD	A,H
		LD	(D_BB91),A
		XOR	A
		LD	B,A
		SUB	C
		AND	7FH
		LD	C,A
		LD	A,0FFH
		CALL	NZ,C3C05
		PUSH	IX
		POP	HL
		LD	BC,33
		ADD	HL,BC
		LD	DE,D_BB92
		LD	BC,3
		LDIR
		LD	A,(IX+26)
		LD	(D_BB8E),A
		LD	A,(IX+27)
		LD	(DSBB8F),A
		LD	A,(IX+0)
		LD	(D_BB90),A
		XOR	A
		RET

J3C01:		XOR	A
		LD	B,A
		INC	A
		RET

; Subroutine clear space
; Input:  A  = segment type (b2 set BDOS, b2 reset DOS)
;         DE = address
;         BC = size
C3C05: 		PUSH	AF
		PUSH	DE
		AND	04H
		CALL	C2731
		SET	7,D
		CALL	PUT_P2
J3C11:		XOR	A
		LD	(DE),A
		INC	DE
		DEC	BC
		LD	A,B
		OR	C
		JR	Z,J3C28
		BIT	6,D
		JR	Z,J3C11
		POP	AF
		AND	0C0H
		ADD	A,40H
		LD	D,A
		LD	E,00H
		POP	AF
		JR	C3C05

J3C28:		POP	AF
		POP	AF
		LD	A,(DATA_S)
		JP	PUT_P2

; Subroutine increase record number FCB
C3C30:		LD	A,(IX+12)
		LD	L,A
		LD	A,(IX+14)
		LD	H,A
		LD	A,(IX+32)
		INC	A
		JP	P,J3C42
		INC	HL
		LD	A,00H
J3C42:		LD	(IX+32),A
		LD	A,L
		LD	(IX+12),A
		LD	A,H
		LD	(IX+14),A
		RET	P

; Subroutine update record count in current extent FCB
C3C4E:		LD	HL,(D_B9EF)
		XOR	A
		LD	B,A
		SUB	L
		AND	7FH
		LD	C,A
		ADD	HL,BC
		LD	BC,(DSB9F1)
		JR	NC,J3C5F
		INC	BC
J3C5F:		LD	A,(DSBA08)
		AND	0C0H
		LD	D,A
		XOR	A
		LD	E,A
		SBC	HL,DE
		PUSH	BC
		EX	(SP),HL
		LD	BC,(D_BA09)
		SBC	HL,BC
		POP	HL
		LD	B,A
		JR	C,J3C80
		LD	B,80H
		JR	NZ,J3C80
		LD	A,H
		AND	0C0H
		JR	NZ,J3C80
		ADD	HL,HL
		LD	B,H
J3C80:		LD	A,B
		LD	(IX+15),A
		RET

; Subroutine setup random record from current extent FCB
C3C85:		LD	A,(IX+32)

; Subroutine setup random record from current extent FCB
; Input:  A = record in extent
C3C88:		PUSH	AF
		LD	A,(IX+14)
		LD	B,A
		LD	A,(IX+12)
		LD	C,A
		POP	AF
		ADD	A,A
		SRL	B
		RR	C
		RRA
		LD	(IX+33),A
		LD	(IX+34),C
		LD	(IX+35),B

; Subroutine update current file position from random record number
; Input:  BC,A = random record number
C3CA1:		LD	HL,D_BA07
		LD	(HL),00H
		SRL	B
		RR	C
		RRA
		RR	(HL)
		INC	HL
		LD	(HL),A
		INC	HL
		LD	(HL),C
		INC	HL
		LD	(HL),B
		RET

; Subroutine setup DOS2 specific FCB fields
; Input:  IX = pointer to FCB
C3CB4:		PUSH	IX
		POP	HL
		LD	BC,16
		ADD	HL,BC
		EX	DE,HL
		LD	HL,D_B9EF
		LD	BC,4
		LDIR
		LD	HL,I_B9F4
		LD	BC,4
		LDIR
		LD	HL,I_B9FF
		LD	BC,8
		LDIR
	IFDEF FAT16 ; CLUST2
		LD	HL,(I_B9FA)
		CALL	CHKDRV
		RET	Z		; FAT16
	ENDIF
		LD	A,(IX+29)
		AND	0FH
		LD	B,A
		LD	A,(DSB9E8)
		BIT	0,A
		JR	Z,J3CE3
		SET	6,B
J3CE3:		LD	A,(D_B9F8)
		BIT	7,A
		JR	Z,J3CEC
		SET	5,B
J3CEC:		LD	A,(D_B9F8)
		BIT	6,A
		JR	Z,J3CF5
		SET	4,B
J3CF5:		LD	A,(DSBA0B)
		BIT	7,A
		JR	Z,J3CFE
		SET	7,B
J3CFE:		LD	(IX+29),B
		RET

; Subroutine copy file name back to FCB
; Input:  HL = pointer to FCB
C3D02:		INC	HL
		LD	A,(DE)
		CP	05H
		JR	NZ,J3D0A
		LD	A,0E5H
J3D0A:		LD	(HL),A
		INC	HL
		INC	DE
		LD	BC,10
		EX	DE,HL
		LDIR
		EX	DE,HL
		RET

; ---------------------------------------------------------
; *** Functions: 26,27 ***
; ---------------------------------------------------------

; ---------------------------------------------------------
; Function $27 _RDBLK
; Input:  DE = pointer to FCB
;         HL = number of records
; ---------------------------------------------------------
F_RDBLK:	LD	A,1
		JR	J3D1A

; ---------------------------------------------------------
; Function $26 _WRBLK
; Input:  DE = pointer to FCB
;         HL = number of records
; ---------------------------------------------------------
F_WRBLK:	XOR	A
J3D1A:		EX	AF,AF'
		LD	A,0C9H
		LD	(D_B976),A
		PUSH	HL
		CALL	C399D
		JP	NZ,J3DED
		LD	IX,(D_BB96)
		CALL	C3B7B
		LD	C,(IX+14)
		LD	B,(IX+15)
		LD	HL,64-1
		XOR	A
		SBC	HL,BC
		LD	E,(IX+35)
		LD	D,(IX+36)
		JR	NC,J3D43
		LD	D,A
J3D43:		CALL	C3E14
		LD	A,H
		OR	L
		JP	NZ,J3DF0
		PUSH	DE
		LD	E,(IX+33)
		LD	D,(IX+34)
		CALL	C3E14
		POP	BC
		ADD	HL,BC
		JP	C,J3DF0
		LD	(D_BA07),DE
		LD	(D_BA09),HL
		POP	DE
		PUSH	DE
		LD	C,(IX+14)
		LD	B,(IX+15)
		CALL	C3E14
		LD	A,H
		OR	L
		JP	NZ,J3DF0
		LD	C,E
		LD	B,D
		PUSH	BC
		EX	AF,AF'
		PUSH	AF
		LD	IX,I_B9DA
		LD	DE,(DTA_AD)
		BIT	0,A
		JR	NZ,J3D8A
		LD	A,B
		OR	C
		LD	A,00H
		JR	NZ,J3D8A
		SET	4,A
J3D8A:		CALL	C275B
		LD	(D_B976),A
		LD	IX,(D_BB96)
		POP	AF
		EX	AF,AF'
		XOR	A
		POP	HL
		SBC	HL,BC
		JR	Z,J3DC9
		LD	(D_B977),DE
		LD	E,(IX+14)
		LD	D,(IX+15)
		CALL	C3DFE
		LD	A,H
		OR	L
		PUSH	BC
		JR	Z,J3DC0
		POP	BC
		INC	BC
		PUSH	BC
		EX	DE,HL
		SBC	HL,DE
		LD	B,H
		LD	C,L
		LD	DE,(D_B977)
		EX	AF,AF'
		BIT	0,A
		CALL	NZ,C3C05
J3DC0:		POP	BC
		POP	HL
		PUSH	BC
		XOR	A
		SBC	HL,BC
		JR	Z,J3DC9
		INC	A
J3DC9:		EX	AF,AF'
		CALL	C3CB4
		POP	DE
		LD	L,(IX+33)
		LD	H,(IX+34)
		ADD	HL,DE
		LD	(IX+33),L
		LD	(IX+34),H
		JR	NC,J3DEA
		LD	L,(IX+35)
		LD	H,(IX+36)
		INC	HL
		LD	(IX+35),L
		LD	(IX+36),H
J3DEA:		EX	AF,AF'
		JR	J3DF5

J3DED:		LD	(D_B976),A
J3DF0:		POP	HL
		XOR	A
		LD	D,A
		LD	E,A
		INC	A
J3DF5:		LD	L,A
		LD	H,00H
		OR	A
		RET	Z
		LD	A,(D_B976)
		RET

; Subroutine divide
C3DFE:		XOR	A
		LD	H,A
		LD	L,A
		LD	A,10H
J3E03:		CCF
J3E04:		RL	C
		RL	B
		DEC	A
		RET	M
		ADC	HL,HL
		SBC	HL,DE
		JR	NC,J3E03
		ADD	HL,DE
		OR	A
		JR	J3E04

; Subroutine multiply
C3E14:		PUSH	BC
		LD	A,B
		LD	HL,0
		LD	B,10H
J3E1B:		ADD	HL,HL
		RL	C
		RLA
		JR	NC,J3E28
		ADD	HL,DE
		JR	NC,J3E28
		INC	C
		JR	NZ,J3E28
		INC	A
J3E28:		DJNZ	J3E1B
		EX	DE,HL
		LD	L,C
		LD	H,A
		POP	BC
		RET

IFDEF FAT16
; ------------------------------------------------------------------------------
; *** Subroutines: FAT16 ***
; ------------------------------------------------------------------------------

; Calculate disk free space without using an extra RAM segment, start at end of FAT.
; If FASTALLOC is defined then cut off count if number of free clusters is more than 2K.

ALLOC16:	LD	B,D
		LD	C,E		; BC = total cluster countdown
		INC	BC		; Adjust for start cluster 2
		LD	L,(IX+0Ch)
		LD	H,(IX+0Dh)	; HL = first FAT sector
		LD	E,B
		LD	D,0
		ADD	HL,DE		; start at last sector
		INC	B		; total FAT sector count
		LD	A,C
		OR	A		; (Total clusters) MOD 256 = 0 ?
		JR	NZ,R01		; NZ=no
		DEC	HL		; adjust last FAT sector
		DEC	B		; adjust FAT sector counter
R01:		LD	E,D		; DE=0: free cluster counter

		; B  = FAT sector counter
		; C  = End of FAT in last FAT sector
		; DE = Free cluster counter
		; HL = FAT sector address
FREE_0:		PUSH	HL
		PUSH	BC
		PUSH	DE
		EX	DE,HL
		XOR	A
		CALL	C2B6A
		LD	BC,0Bh
		ADD	HL,BC		; HL = Pointer to sector data
		POP	DE		
		POP	BC
		LD	A,C		; End of FAT in last sector
		LD	C,0		; Next FAT sectors
		PUSH	BC
		LD	B,A
		CALL	FREE_1		; Add free clusters in FAT sector to counter
		POP	BC		; Remaining total cluster count
		POP	HL
		DEC	HL		; Next FAT sector
	IFDEF FASTALLOC
		LD	A,D
		AND	0F8H		; Free clusters >= 2048? (0F8H=2048 0F0H=4096)
		JR	Z,FREE_3	; Z=no
		LD	DE,0800H	; Set free clusters to 2048 (0800H=2048 1000H=4096)
		JR	FREE_4
FREE_3:
	ENDIF
		DJNZ	FREE_0
FREE_4:		PUSH	DE
		JP	ALLOC_RET

; Count free clusters in FAT16 sector
FREE_1:		LD	A,(HL)
		INC	HL
		OR	(HL)
		INC	HL
		JR	NZ,FREE_2
		INC	DE
FREE_2:		DJNZ	FREE_1
		RET

; ---------------------------------------------------------
;CLUSTER NUMBER
CLST_1:		LD	D,(HL)
		JR	CHK_C		; Check data 'FFFFh'
	
CLST_2:		SET	3,(IY+20h)
		JR	CHK_C
	
CLST_4:		LD	DE,(D_BBE8)
		JR	CHK_C
	
CLST_8:		POP	AF
		POP	DE
		JR	CHK_D
	
CLST_5:		POP	AF
		PUSH	BC
CHK_D:		PUSH	AF
		JR	CHK_C
	
CLST_6:		LD	DE,(D_BBA3)
CHK_C:		LD	A,D		; DE=FFFFh Z=0
		AND	E
		JR	CHK_A

CLST_7:		JR	NZ,CHK_C
		INC	SP
		INC	SP
		RET

CLST_9:		LD	DE,(D_BBE8)
		CALL	CHK_C
		LD	DE,(D_BBE4)
		RET	NZ
		JR	CHK_C

CLST_3:		LD	DE,(D_BBE6)
		LD	A,D

CHK_A:		INC	A
		JR	Z,CHK_B
		XOR	A
		RET			; z=1
CHK_B:		DEC	A
		RET			; z=0
	
CLST_A:		LD	A,(HL)
		JR	CHK_A
	
; ---------------------------------------------------------
;Read 16 bit FAT
Z0018:		POP	AF
		CALL	FATADR		; get address & sector set
		JR	Z,Z0019		; no error
Z0020:		XOR	A
		LD	(D_BBEA),A
		LD	A,0F2h
		LD	DE,0FFFFh
		CALL	C3686
		JR	Z,Z0020
		JR	Z0021		; error
	
Z0019:		PUSH	HL
		LD	A,(DE)		; DE=FAT address
		LD	L,A
		INC	DE
		LD	A,(DE)
		LD	H,A
		EX	DE,HL		; DE=next cluster number
		LD	HL,0FFF7h	; HL=wrong cluster number
		OR	A
		SBC	HL,DE
		POP	HL
		JR	NC,Z1021	; not end of cluster
Z0021:		LD	DE,0FFFFh	; end of cluster
		OR	D		; Z=0
		SCF			; Cy=1
		RET
Z1021:		XOR	A		; Cy=0 Z=1
		RET
	
; ---------------------------------------------------------
;Get sector number of FAT & read FAT sector in disk buffer
;Get address of FAT
FATADR:		PUSH	IX		; IX=#B9DA
		PUSH	BC
		PUSH	HL		; HL=DPB address
		PUSH	HL
		POP	IX
		RES	0,(IY+29h)
		LD	L,(IX+16h)
		LD	H,(IX+17h)
		XOR	A
		SBC	HL,DE
		JR	C,FATAD_	; ERROR
		LD	H,D		; DE=cluster number
		LD	L,E
		ADD	HL,HL		; cluster number * 2(16bit)
		PUSH	HL
		LD	E,D		; cluster * 2 / 200hbytes = sector
		LD	D,0
		LD	L,(IX+0Ch)	; sector number of FAT top
		LD	H,(IX+0Dh)
		ADD	HL,DE		; get sector number of FAT
		EX	DE,HL
		CALL	C2B6A		; get address of disk buffer
		LD	BC,0Bh
		ADD	HL,BC		; start address of FAT in disk buffer
		POP	BC
		LD	A,B
		AND	01h		; get leave from FATaddress/200h
		LD	B,A
		ADD	HL,BC		; get address
		EX	DE,HL
		CP	A		; Z=1 right
FATAD_:		POP	HL
		POP	BC
		POP	IX
		RET

; ---------------------------------------------------------
; Z=1  FAT16 drive	Z=0  FAT12 drive
CHKDRV:		PUSH	HL
		PUSH	DE
		LD	DE,001Dh
		ADD	HL,DE
		BIT	7,(HL)
		POP	DE
		POP	HL
		RET
	
; ---------------------------------------------------------
;Set sector number
NUM_1:		LD	(D_BBB4),HL
		RET	NC
SECINC:		LD	A,(BIT16)
		INC	A
		LD	(BIT16),A
		RET
	
NUM_2:		INC	(IY+35h)
		RET	NZ
		JR	SECINC
	
; ---------------------------------------------------------
BUF_4F:		LD	BC,(D_BBE8)	; SUB DIR? ROOT?
		INC	BC
		LD	A,B
		OR	C
		LD	B,01h
		JR	Z,BUF_3F	; (BBE8)=FFFF
		LD	A,(SDIR_1)
		JR	BUF_F

BUF_2F:		LD	A,(BIT16)
		JR	BUF_F
	
BUF_3F:		XOR	A
BUF_F:		LD	(DSKEX),A	; FAT
		JP	C2B78

; ---------------------------------------------------------
;Read/write a sector for buffer
REDBUF:		PUSH	AF
		LD	A,(DSKEX)
		JR	REDB_1
WRTBUF:		PUSH	AF
		LD	A,(IX-3)	; bit16-23
REDB_1:		LD	(RW_16),A	; use bit16-23 of sector number
		POP	AF
		JP	C324F		; call disk driver read/write sectors with disk check

; ---------------------------------------------------------
STOR_7:		PUSH	AF
		LD	A,(SDIR_1)
		ld	(ix+38h),a
		POP	AF
		EX	DE,HL
		LDIR
		RET

; ---------------------------------------------------------
; Variables (todo: store these variables in P2 workarea)

BIT16:		DB	0		; bit16-23 for calculation of sector
DSKEX:		DB	0		; bit16-23 for Disk buffer
SDIR_1:		DB	0		; BBE2h	bit16-23
SDIR_2:		DB	0		; BBD2h+4 bit16-23
SDIR_3:		DB	0		; BBC6h+4 bit16-23
RW_16:		DB	0		; bit16-23 for DISKIO

; ------------------------------------------------------------------------------
ENDIF ; FAT16

	IFDEF ROM16K
		DEFS	$3FFE-$,0
		DW	$0220		; DOS kernel version / signature
	ENDIF

	DEPHASE

K1_END:
