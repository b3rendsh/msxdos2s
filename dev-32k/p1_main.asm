; ------------------------------------------------------------------------------
; p1_main.asm
; DOS 2.20 kernel page 1: initialization and Disk BASIC
; Based on ASCII DOS 2.20 codebase s0 and s1
;
; Code Copyrighted by ASCII, OKEI and maybe others
; Source origin is the msxsyssrc repository by Arjen Zeilemaker
; Restructure, modifications and additional comments by H.J. Berends
;
; Sourcecode supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyrightholders
; ------------------------------------------------------------------------------
; Modifications:
; 01. Kanji, rom mapper, self check and unused code is removed
; 02. Removed MSX2 check
; 03. Removed Kanji error messages 
; 04. Simplified international character generator
; 05. Initialize FAT16 kernel and workspace patches based on FAT16 v0.12 by OKEI
; 06. Restructured codebase segments 0 and 1 into one main segment p1
; 07. Relocate code to unused space between msxdos 1 entry points
; 08. Added BOOTMENU option
; 09. Check if boot sector contains a valid boot loader or extended boot signature (FAT16)


		INCLUDE "disk.inc"	; Assembler directives
		INCLUDE	"msx.inc"	; MSX constants and definitions

		SECTION	P1_MAIN

		ORG	04000H
		S_ORG0	EQU	04000H	; Offset for current address: $ calculations

		; Routines which can be used by the disk hardware driver
		PUBLIC  PROMPT		; Prints a message for two drive emulation on a single drive.
		PUBLIC  GETSLT		; Get disk driver's slot address.
		PUBLIC  GETWRK		; Get address of disk driver's work area.
		PUBLIC  DIV16		; BC:=BC/DE, remainder in HL.
		PUBLIC  ENASLT		; Enables a slot a address specified by A:HL.
		PUBLIC  XFER		; Eactly emulates an LDIR.. ..used when transferring data to/fro page-1.
		PUBLIC  SETINT		; Setup routine at (HL) as a timer interrupt routine (50Hz/60Hz).
		PUBLIC  PRVINT		; Calls previous timer interrupt routine...
	IFDEF FAT16
		PUBLIC	IBOOTCODE	; Start of bootsector code
	ENDIF

		; Symbols which must be defined by the disk hardware driver
		EXTERN	INIHRD		; Initialize hardware
		EXTERN	DRIVES		; Return number of drives in system
		EXTERN	INIENV		; Initialize work area
		EXTERN	DSKIO		; Sector read and write
		EXTERN	DSKCHG		; Get disk change status
		EXTERN	GETDPB		; Get disk parameter block (DPB)
		EXTERN	CHOICE		; Return format choice string
		EXTERN	DSKFMT		; Format a disk
		EXTERN	MTOFF		; Turn drive motors off
		EXTERN	OEMSTA		; Used for system expansion (OEMSTATEMENT)
		EXTERN	MYSIZE		; Size of the page-3 RAM work area required by the driver in bytes.
		EXTERN	SECLEN		; Maximum sector size for media supported by this driver (512).
		EXTERN	DEFDPB		; Base address of an 18 byte "default" DPB for this driver.

	IF PPIDE || CFIDE
		; Additional symbol defined by the ide driver module
		EXTERN	BOOTMENU
	ENDIF

		; Routine used in the paging helper module
		PUBLIC	ALLOCMEM

		; Routine defined in the paging helper module
		EXTERN	PH_INIT

		; Symbols defined by the kernel in P0
		EXTERN	K1_BEGIN
		EXTERN	K1_END

; ------------------------------------------------------------------------------
; MACRO

;_RST MACRO replaced with labels
R_SYNCHR	EQU	08H
R_CHRGTR	EQU	10H
R_OUTDO		EQU	18H
R_DCOMPR	EQU	20H
R_GETYPR	EQU	28H
R_CALLF		EQU	30H

DOSST1		MACRO   X,Y
		LOCAL   _D1
		DEFB    X
		DEFB    _D1-$
		DEFB    Y
_D1:		DEFB    0
		ENDM

IFDEF NODOS1
DOSENT		MACRO   X
		DB	0C9H
		ENDM

ELSE
DOSENT		MACRO   X
		DEFS    X-$-S_ORG0,0C9H
		ENDM
ENDIF

DOSCALL		MACRO   X
		LD      C,X
		JP      J4F54
		ENDM

; ------------------------------------------------------------------------------
; *** ROM Header at address 04000H ***
; ------------------------------------------------------------------------------

		DEFB    "AB"

		DEFW    J47D6                   ; EXTENSION ROM INIT handler
		DEFW    C575C                   ; EXTENSION ROM CALL statement handler

		DEFW    0                       ; EXTENSION ROM device handler (no device)
		DEFW    0                       ; EXTENSION ROM basic program (no basic program)
		DEFS    6,0

C4010:  	JP      DSKIO                   ; DRIVER:  DSKIO
C4013:  	JP      DSKCHG                  ; DRIVER:  DSKCHG
C4016:  	JP      GETDPB                  ; DRIVER:  GETDPB
C4019:  	JP      CHOICE                  ; DRIVER:  CHOICE
C401C:  	JP      DSKFMT                  ; DRIVER:  DSKFMT
C401F:  	JP      MTOFF                   ; DRIVER:  MTOFF
C4022:  	JP      J4B1B                   ; SYSTEM:  Start DiskBASIC
C4025:  	SCF                             ; use specified work area
		JP      J4E67			; format disk
C4029:  	JP      J4CD3                  	; SYSTEM:  Stop drives
		NOP

; DOS entry point subroutine GETSLT
GETSLT:  	JP      C4E05                  	; get slot id of page 1

; DOS entry point $INIT
DOS_SINIT:  	LD      HL,(DOSHIM)             ; SYSTEM:  get top of MSXDOS memory
		RET

; DOS1 kernel compatible:  CP/M BIOS CONST entry
; This entry is supported, to use MSXDOS.SYS
DOS_SSBIOS:  	JP      J4177

; DOS2: pointer to kernel version ASCIIZ string
		DEFS    04038H-$-S_ORG0,0
		DEFW    I411E

; Subroutine EXTBIO handler
		DEFS	04043H-$-S_ORG0,0
C4043:		CALL    C410C
		JP      FCALSA

; Subroutine H_TIMI handler
C4049:		PUSH    AF
		CALL    C4CF3                  	; H_TIMI handler
		POP     AF
		RET

J4177:		LD      HL,A0086		; CONST
		JR      J4185
J417C:		LD      HL,A0080		; CONIN
		JR      J4185
J4181:		LD      C,A
J4182:		LD      HL,A0083		; CONOUT
J4185:		JP      GO_BIOS

; DOS1 kernel compatible:  CP/M BIOS CONIN entry
; This entry is supported, to use MSXDOS.SYS
		DEFS    04078H-$-S_ORG0,0
DOS_SIN:	JP      J417C

; DOS2:  RAMDISK driver jumpentries
		DEFS    04080H-$-S_ORG0,0
L4080:		JP      RAMD_DSKIO		; RAMDISK:  DSKIO routine
L4083:		JP      RAMD_DSKCHG		; RAMDISK:  DSKCHG routine
L4086:		JP      RAMD_GETDPB		; RAMDISK:  GETDPB routine
L4089:		JP      RAMD_CHOICE		; RAMDISK:  CHOICE routine
L408C:		JP      RAMD_DSKFMT		; RAMDISK:  DSKFMT routine

; DOS1 kernel compatible:  CP/M BIOS CONOUT entry
; This entry is supported, to use MSXDOS.SYS
		DEFS    0408FH-$-S_ORG0,0
DOS_SOUT:  	JP      J4181

; ------------------------------------------------------------------------------
; Mod: Relocated routines from bank S1
; ------------------------------------------------------------------------------
C4100		EQU	C49D7	; check and invoke memorymapper of 6 or more segments
C4103		EQU	J410F	; install disksystem routines
C4106		EQU	C4C1E	; copy message to buffer
I4109		EQU	J4C09	; copy errorstring to buffer
C410C		EQU	J4AF7	; EXTBIO handler memorymapper

; ------------------------------------------------------------------------------
; Mod: addded suffix after version number to identify build options in modified rom
; ------------------------------------------------------------------------------
I411E:  	DEFB    "MSX-DOS kernel version 2.20",MOD1,MOD2,0
I413A:		DEFB    "Disk BASIC version 2.01",MOD1,MOD2,0
		DEFB    "Copyright (C) 1989 ASCII Corporation",0

; ------------------------------------------------------------------------------
; Mod: Relocated S0 code to the unused space between the DOS entry points
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; *** EXTENSION ROM INIT handler ***
; ------------------------------------------------------------------------------
J47D6:		CALL    INIHRD			; initialize hardware disk driver
		DI
		LD      A,(IDBYT2)
		OR      A			; MSX2 or higher ?
		NOP				; Mod: patch to remove MSX2 check "ret z"
		LD      A,(DISKID)
		OR      A			; initialize disk system canceled ?
		RET     M			; yep, quit
		JR      NZ,J4865		; not the disk interface starting the disk system initialization

; Disk interface starting the disk system initialization
		LD      HL,HOKVLD
		BIT     0,(HL)			; EXTBIO initialized ?
		JR      NZ,J47F9		; yep,
		SET     0,(HL)
		LD      HL,EXTBIO
		LD      B,3*5
J47F4:		LD      (HL),0C9H
		INC     HL
		DJNZ    J47F4			; initialize EXTBIO, DISINT, ENAINT hooks
J47F9:		LD      HL,(BOTTOM)
		LD      DE,0C001H		; rem: I$C000+1 label not defined?
		RST    	R_DCOMPR		; at least 16 Kb RAM ?
		JR      NC,J4817		; nope, cancel disk initialization
		LD      HL,(HIMEM)
		LD      DE,VARWRK
		RST    	R_DCOMPR		; HIMEM been lowered by extension ?
		JR      NZ,J4817		; yep, cancel disk initialization
		LD      A,6
		CALL    SNSMAT
		DI
		RRCA				; SHIFT key pressed ?
		JR      C,J481D			; nope, continue
		LD      A,7
		RST	R_OUTDO			; beep
J4817:		LD      A,0FFH
		LD      (DISKID),A		; initialize disk system canceled
		RET

J481D:		CALL    C492A			; switch system RAM to slot with memory mapper
		RET     C			; not found, quit
		LD      HL,VARWRK-DATABA	; size of the disk system static work area
		CALL    C5604			; allocate memory (adjust BASIC areapointers)
		RET     C
		LD      BC,VARWRK-DATABA
J482B:  	XOR     A
		LD      (HL),A
		INC     HL
		DEC     BC
		LD      A,C
		OR      B
		JR      NZ,J482B		; clear disk system static work area
		LD      (AUTLIN),BC		; biggest sector size = 0
		LD      B,4*2+4*3
		LD      HL,DRVTBL
J483C:  	LD      (HL),A
		INC     HL
		DJNZ    J483C			; initialize DRVTBL and HOOKSA
		LD      HL,HOOKBE
		LD      B,69H
J4845:		LD      (HL),0C9H
		INC     HL
		DJNZ    J4845			; initialize disk system hooks
		LD      HL,RSLREG
		LD      (HL),0DBH
		INC     HL
		LD      (HL),0A8H
		INC     HL
		LD      (HL),0C9H		; intialize RSLREG subroutine
		LD      A,6
		CALL    SNSMAT
		DI
		AND     02H			; CTRL key
		LD      (TARGET),A		; store CTRL key status
		LD      A,7
		RST	R_OUTDO			; beep
		JR      J4870			; continue

; Disk interface not starting the disk system initialization
J4865:		LD      A,(DOSVER)
		CP      22H			; disk system initialization by disk system version 2.20 or higher ?
		JR      NC,J4888		; yep,

; Disk interface taking over disk system initialization
		CALL    C492A			; switch system RAM to slot with memory mapper
		RET     C			; not found, quit
J4870:		LD      A,22H
		LD      (DOSVER),A		; update disk system version to 2.2x
		CALL    GETSLT
		LD      HL,H_RUNC
		LD      (HL),0F7H
		INC     HL
		LD      (HL),A
		INC     HL
		LD      (HL),C495B % 256	; rem: LOW C495B
		INC     HL
		LD      (HL),C495B / 256	; rem: HIGH C495B
		INC     HL
		LD      (HL),0C9H		; patch H_RUNC to regain control after MSX BASIC starts
J4888:		CALL    C4906			; get detected disk interfaces and drives
		RET     Z			; no room for interfaces or drives, quit
		LD      HL,MYSIZE		; size of work area disk driver
		CALL    C5604			; allocate memory (adjust BASIC areapointers)
		JR      C,J48FE			; out of memory, increase disk interface count if first disk interface and quit
		EX      DE,HL
		CALL    C4DBB			; get pointer to SLTWRK entry
		LD      (HL),E
		INC     HL
		LD      (HL),D			; update pointer to disk driver work area
		LD      HL,(AUTLIN)		; bigest sector size sofar
		LD      DE,SECLEN		; bigest sector size supported by the disk driver
		RST    	R_DCOMPR		; sector size of disk driver bigger ?
		JR      NC,J48A8
		LD      (AUTLIN),DE		; yep, update bigest sector size sofar
J48A8:		CALL    C4906			; get detected disk interfaces and drives
		LD      HL,DRVTBL
		LD      D,0
		ADD     HL,DE
		ADD     HL,DE
		EX      DE,HL			; pointer to free DRVTBL entry
		LD      A,(TARGET)
		OR      A			; CTRL status in Zx
		LD      A,C			; store number of drives sofar
		CALL    DRIVES			; get number of drives disk interface
		ADD     A,L			; add drives of disk interface
		CP      8+1			; more as 8 drives ?
		JR      C,J48C2			; nope, use all drives of disk interface
		LD      A,8
J48C2:		SUB     C			; use maximum drives of disk interface
		JR      Z,J48FE			; no drives of disk interface, increase disk interface count if first disk interface and quit
		LD      (DE),A			; store number of drives
		INC     DE
		CALL    GETSLT
		LD      (DE),A			; store slot id
		LD      B,0
		LD      HL,SDPBLI
		ADD     HL,BC
		ADD     HL,BC
		PUSH    HL			; store pointer to drive parameter block entry of first drive of disk interface
		DEC     DE
		JR	DOS_CPMVER+5		; Mod: jump over BDOS entry

; -------------------------------------
		DOSENT  041EFH
DOS_CPMVER:     DOSCALL 00CH
; -------------------------------------

		LD      A,(DE)			; number of drives interface
		PUSH    AF			; store number of drives interface
		LD      C,A
		ADD     A,A
		ADD     A,A
		ADD     A,C
		ADD     A,A
		ADD     A,A
		ADD     A,C
		LD      L,A
		LD      H,B			; *21 (size of drive parameter block)
		CALL    C4C50			; allocate memory (adjust BASIC areapointers, halt when error)
		EX      DE,HL
		POP     AF			; restore number of drives interface
		POP     HL			; restore pointer to drive parameter block entry
J48E5:		LD      (HL),E
		INC     HL
		LD      (HL),D
		INC     HL			; update pointer to drive parameter block, next drive parameter block entry
		PUSH    HL			; store pointer to drive parameter block entry
		LD      HL,DEFDPB		; default drive parameter block disk driver
		LD      BC,21
		LDIR    			; initialize drive parameter block
		POP     HL			; restore pointer to drive parameter block entry
		DEC     A
		JR      NZ,J48E5		; next drive
		CALL    INIENV			; initialize work area disk driver
		LD      HL,DISKID
		INC     (HL)			; increase disk interface count
		RET

; Increase disk interface count if first disk interface and quit
J48FE:		LD      HL,DISKID
		INC     (HL)
		DEC     (HL)			; first disk interface ?
		RET     NZ			; nope, quit
		INC     (HL)			; increase disk interface count
		RET

; Subroutine get detected disk interfaces and drives
C4906:		LD      HL,DRVTBL
		LD      B,4			; number of interfaces = 4
		XOR     A			; number of detected drives = 0
		LD      E,A			; number of detected interfaces = 0
J490D:		LD      C,A			; store drives
		ADD     A,(HL)			; update drives
		JR      C,J4927                 ; invalid DRVTBL, halt system
		CP      C			; entry used ?
		JR      Z,J491B			; nope, check if remaining entries are unused
		INC     E			; update detected interfaces
		INC     HL
		INC     HL
		DJNZ    J490D			; next interface entry
		CP      A			; clear Cx, set Zx
		RET

J491B:		ADD     A,(HL)
		CP      C			; entry used ?
		JR      NZ,J4927		; yep, halt system
		INC     HL
		INC     HL
		DJNZ    J491B			; next interface entry
		CP      8			; valid number of detected drives ?
		RET     Z			; yep, quit
		RET     C			; yep, quit
J4927:		JP      J4C54			; halt system

; Subroutine switch system RAM to slot with memory mapper
C492A:		CALL    C4E12			; get slot id of page 2
		CALL    C4100			; check and invoke memorymapper of 6 or more segments
		RET     C			; not found, quit with error
		LD      HL,5
		ADD     HL,SP			; offset 5
		IN      A,(0A8H)
		RRD
		; update saved primary slot on stack, otherwise memorymapper change
		; will be rolled back after CALLF is finished)
		LD      (HL),A
		CALL    C4E05			; get slot id of page 1
		BIT     7,A			; slot expanded ?
		RET     Z			; nope, quit
		LD      HL,12
		ADD     HL,SP			; offset 12
		LD      C,A
		CALL    C4E21			; get slot id of page 3
		XOR     C
		AND     03H			; same primary slot ?
		JR      NZ,J4954		; nope, skip secondary slot register
		LD      A,(DFFFF)
		CPL     			; current secondary slot register
		RRD
		; update saved secondary slot on stack, otherwise memorymapper change
		; will be rolled back after CALLF is finished
		LD      (HL),A

J4954:		DEC     HL			; offset 11
		IN      A,(0A8H)
		RRD
		; update saved primary slot on stack, otherwise memorymapper change
		; will be rolled back after CALLF is finished)
		LD      (HL),A
		RET

; Regain control from MSX BASIC initialization
C495B:		LD      HL,H_RUNC
		LD      B,5
J4960:		LD      (HL),0C9H
		INC     HL
		DJNZ    J4960			; clear H_RUNC hook
		LD      HL,DISKID
		LD      A,(HL)			; number of disk interfaces
		LD      (HL),B			; clear DEVICE
		OR      A			; initialize disk system canceled ?
		RET     M			; yep, quit
		LD      D,A			; disk interface count
		CALL    C4906			; get detected disk interfaces and drives
		LD      (SNUMDR),A		; store detected drives
		LD      A,D
		SUB     E			; disk interface count same as interface count of DRVTBL ?
		JR      Z,J4986			; yep,
		DEC     A			; 1 extra ?
		JP      NZ,J4C54		; nope, halt system
		LD      DE,HOOKSA+0
		LD      HL,HOOKSA+3
		LD      BC,4*3
		LDIR    			; remove disk driver interrupt handler of MASTER disk interface
J4986:		CALL    GETSLT
		LD      (MASTER),A		; update disk system master slot id
		LD      HL,CHAR_16
		LD      DE,KANJTA
		LD      BC,4
		LDIR				; initialize double byte header char table
		LD      A,(IDBYT0)
		RRCA
		RRCA
		RRCA
		RRCA
		AND     07H
		LD      (COUNTR),A		; update data format
		LD      HL,AUXBOD
		LD      (HL),3EH
		INC     HL
		LD      (HL),1AH
		INC     HL
		LD      B,10-2
J49AE:		LD      (HL),0C9H
		INC     HL
		DJNZ    J49AE			; initialize AUX device hooks
		LD      A,0CDH
		LD      HL,SDOSON
		LD      (SPRTBUF+0),A
		LD      (SPRTBUF+1),HL
		LD      A,0C3H
		LD      HL,C53AC
		LD      (SPRTBUF+3),A
		LD      (SPRTBUF+4),HL		; initialize PRTBUF subroutine
		LD      HL,I4109
		LD      (SERR_M),HL		; address of copy error message subroutine
		LD      HL,I4D32
		LD      (SPROMPT),HL		; address of prompt subroutine
		LD      HL,I6A82
		LD      (SBDOS),HL		; address of BDOS subroutine
		LD      HL,J6A86
		LD      (SDOS1),HL		; address of DOS1 BDOS subroutine
		LD      A,0FFH
		LD      (TIMFLG),A		; clockchip detected (because of the MSX2 requirement not need to detect it)
		LD      HL,21			; size of drive parameter block
		CALL    C4C50			; allocate memory (adjust BASIC areapointers, halt when error)
		LD      (RM_DPB),HL		; update pointer to DPB of ram disk
		LD      HL,(AUTLIN)		; bigest sector size sofar
		LD      DE,512			; DOS2 supports at least 512 bytes sectors
		RST    	R_DCOMPR		; sector size of DOS2 bigger ?
		JR      NC,J49F9
		EX      DE,HL			; yep, use 512 bytes
J49F9:		LD      (SMAXSEC),HL		; update bigest sector size
		INC     HL			; +1 for FAT buffer dirty flag
		CALL    C4C50			; allocate memory (adjust BASIC areapointers, halt when error)
		LD      (HL),0			; FAT buffer = clean
		INC     HL
		LD      (SSECBUF),HL		; update pointer to sector buffer
		LD      HL,RM_DPB		; drive parameter block entries (includes ram disk)
		LD      BC,9*256 + 0FFH		; 8 drives + 1 ram disk drive, drive id = 0FFH (ram disk drive id)
J4A0C:		LD      E,(HL)
		INC     HL
		LD      D,(HL)			; get pointer to drive parameter block
		INC     HL
		LD      A,E
		OR      D			; drive has pointer to drive parameter block (valid drive) ?
		JR      Z,J4A24			; nope, skip
		EX      DE,HL
		LD      (HL),C			; update drive id drive parameter block
		PUSH    BC			; store entry counter, drive id
		LD      BC,19
		ADD     HL,BC			; offset 19, pointer to FAT buffer
		LD      BC,(SSECBUF)
		LD      (HL),C
		INC     HL
		LD      (HL),B			; update pointer to FAT buffer with pointer to sector buffer
		POP     BC			; restore entry counter, drive id
		EX      DE,HL
J4A24:		INC     C			; next drive id
		DJNZ    J4A0C			; next drive
		CALL    C4E12			; get slot id of page 2
		CALL    C4103			; install disk system subroutines
		JP      C,J4C54			; error, halt system
		LD      HL,100+64+100+64
		CALL    C4C50			; allocate memory (adjust BASIC areapointers, halt when error)
		LD      (BUF_1),HL		; pointer to temporary buffer
		LD      DE,100
		ADD     HL,DE
		LD      (BUF_2),HL		; pointer to temporary buffer2
		LD      DE,64
		ADD     HL,DE
		LD      (BUF_3),HL		; pointer to temporary buffer3
		LD      DE,100
		JR	DOS_DELETE+5		; Mod: jump over BDOS entry

; -------------------------------------
		DOSENT  0436CH
DOS_DELETE:     DOSCALL 013H
; -------------------------------------

		ADD     HL,DE
		LD      (ERR_BUF),HL		; pointer to temporary buffer4
		LD      HL,(HIMEM)
		LD      (HIMSAV),HL		; register HIMEM for disk system enviroment
		LD      HL,H_TIMI
		PUSH    HL			; store H_TIMI
		LD      DE,TIMI_S
		LD      BC,5
		LDIR				; save current H_TIMI hook
		POP     HL			; restore H_TIMI
		LD      DE,A4049		; interrupt handler DOS2
		CALL    C4C73			; patch hook
		JR	DOS_RENAME+5		; Mod: jump over BDOS entry

; -------------------------------------
		DOSENT  04392H
DOS_RENAME:     DOSCALL 017H
; -------------------------------------

		LD      HL,EXTBIO
		LD      DE,FCALSA
		LD      BC,5			; save current EXTBIO
		PUSH    HL			; store EXTBIO
		LDIR
		POP     HL			; restore EXTBIO
		LD      DE,A4043		; EXTBIO handler DOS2
		CALL    C4C73			; patch hook
		CALL    C56A5			; patch hooks for disk system
		LD      HL,M7D2F+1
		LD      A,(EXPTBL+0)
		CALL    RDSLT
		PUSH    AF
		INC     SP
		DEC     HL
		LD      A,(EXPTBL+0)
		CALL    RDSLT
		PUSH    AF
		INC     SP
		POP     IX			; get address of BASIC screen initialization
		LD      IY,(EXPTBL+0-1)
		CALL    CALSLT			; execute BASIC screen initialization
		CALL    C4C66			; patch H_LOPD if H_CLEAR is patched (to regain control of BASIC initialization from extension)
		LD      SP,TMPSTK		; switch to temporary stack
		LD      A,(H_STKE)
		CP      0C9H			; extension has patched H_STKE to take control of BASIC initialization ?
		LD      IX,M7D17		; continue BASIC initialization without ROM BASIC program execution
		JR      NZ,J4ABB		; yep, start DiskBASIC
		LD      A,(BASROM)
		OR      A			; about to start BASIC program in ROM ?
		LD      IX,M7DE9		; continue BASIC initialization with ROM BASIC program execution
		JP      NZ,J4ABB		; yep, start DiskBASIC
		CALL    C4C16			; disable page 1 support
		JR      J4AC1			; continue with boot loader

; Start DiskBASIC
J4ABB:		CALL    C4BE8			; initialize DiskBASIC
		JP      CALBAS			; start DiskBASIC

J4AC1:		LD      HL,J4B1B
		PUSH    HL			; start DiskBASIC when failed
	IFDEF FAT16
		LD	A,1
		LD	(CUR_DRV),A		; set default drive to A:
		CALL    C694A			; search for first drive with valid boot loader and set default drive to it
		CALL	NZ,C4AFB		; NZ=found, execute boot loader with flag = BASIC
	ELSE
		CALL    C694A			; get valid boot loader
		RET     Z			; no valid boot loader, start DiskBASIC
		CALL    C4AFB			; (boot loader flag = BASIC), execute boot loader
	ENDIF
		LD      HL,(BOTTOM)
		LD      DE,BOT32K
		RST    	R_DCOMPR		; at least 32 Kb RAM ?
		RET     NZ			; nope, start DiskBASIC
	IF CFIDE || PPIDE
		CALL	BOOTMENU		; boot menu which sets current drive
		RET	C			; If c-flag is set then start DiskBASIC
	ELSE
		LD      A,(CUR_DRV)		; current drive
	ENDIF
		LD      HL,I4B18		; empty command line
		JR      J4ADF			; start MSXDOS

J4ADC:		LD      A,(BOOT_D)		; boot drive

; Start MSXDOS
J4ADF:		LD      SP,TMPSTK		; switch to temporary stack
		PUSH    HL
		LD      HL,J4B1B
		EX      (SP),HL			; start DiskBASIC when failed
		PUSH    AF			; store drive id
		LD      A,0FFH
		LD      (DOSFLG),A		; MSXDOS environment = enabled
		POP     AF			; restore drive id
	IFDEF NODOS1
		JP	C68B3			; try to start MSXDOS2, if it fails start DiskBASIC
	ELSE
		CALL    C68B3			; prepare for MSXDOS, try to start MSXDOS2
		CALL    C694A			; get valid boot loader
		RET     Z			; no valid boot loader, start DiskBASIC
		LD      A,0C3H			; page 1 support = enabled
		CALL    C4C18			; update page 1 support
		SCF     			; boot loader flag = MSXDOS
	ENDIF

; Subroutine start boot loader
C4AFB:		LD      HL,DISKVE		; address BDOS diskerror handler pointer
		LD      DE,SDOSON		; enable DOS kernel subroutine
		LD      A,(NOTFIR)		; cold boot flag
		JP      0C01EH			; (rem: JSC01E)	start boot loader (if boot loader returns, start DiskBASIC)

I4B07:		DEFB	"RUN\"\\AUTOEXEC.BAS"
I4B18:		DEFB    0

; BDOS disk error handler: start DiskBASIC without running BASIC program file
I4B19:		DEFW    J4B7B			; start DiskBASIC without running BASIC program file

; -------------------------------------
		DOSENT  04462H
DOS_OPEN:       DOSCALL 00FH
; -------------------------------------

; Subroutine start DiskBASIC
J4B1B:		LD      SP,TMPSTK		; switch to temporary stack
		CALL    C4C16			; disable page 1 support
		LD      HL,I4B07
		LD      DE,BUF+12
		LD      BC,18
		LDIR				; prepare executing AUTOEXEC.BAS
		LD      HL,NOTFIR
		LD      A,(HL)
		OR      A			; cold start ?
		LD      (HL),H			; next start is warm start
		JR      NZ,J4B50		; nope,
		LD      (DOSFLG),A		; MSXDOS environment = disabled
		LD      HL,I4B19		; start DiskBASIC without running BASIC program file
		LD      (DISKVE),HL		; install BDOS disk error handler
		LD      DE,BUF+16		; AUTOEXEC.BAS
		LD      A,1			; open mode = no write
		LD      C,43H			; function = open file handle
		CALL    BDOS			; execute BDOS function
		JR      NZ,J4B7B		; error, start DiskBASIC without running BASIC program file
		LD      C,45H			; function = close file handle
		CALL    BDOS			; execute BDOS function
		JR      J4B7F			; start DiskBASIC

; Warm start DiskBASIC
J4B50:		LD      A,(WBOOT)
		CP      0C3H			; start DiskBASIC from MSXDOS ?
		JR      NZ,J4B7B		; nope, start DiskBASIC without running BASIC program file
		LD      HL,DBUF
		LD      B,(HL)
		INC     B
		DEC     B			; size of command line
		JR      Z,J4B7B			; empty command line, start DiskBASIC without running BASIC program file
J4B5F:		INC     HL
		LD      A,(HL)
		CALL    C4B6A			; check for white space character
		JR      NZ,J4B70		; nope, command line has a BASIC program file name
		DJNZ    J4B5F			; next character
		JR      J4B7B			; start DiskBASIC without running BASIC program file

; Subroutine check for white space character
C4B6A:		CP      09H			; TAB ?
		RET     Z			; yep, quit
		CP      ' '			; SPACE ?
		RET

J4B70:		XOR     A
		LD      C,B
		LD      B,A
		LD      DE,BUF+12+4
		LDIR				; copy file name
		LD      (DE),A			; end of BASIC line token
		JR      J4B7F			; start DiskBASIC running the specified BASIC program

; Start DiskBASIC without running BASIC program file
J4B7B:		XOR     A
		LD      (BUF+12+3),A		; simple RUN statement
J4B7F:		LD      SP,TMPSTK
		LD      A,(RAMAD2)
		LD      H,80H			; rem: HIGH 8000H
		CALL    ENASLT			; switch page 2 to disk system RAM slot
		LD      A,(EXPTBL+0)
		LD      H,00H			; rem: HIGH 0000H
		CALL    ENASLT			; switch page 0 to MAIN ROM
		CALL    C4BE8			; initialize DiskBASIC
J4B95:		LD      BC,0*256+61H		; function = rejoin parent process
		CALL    BDOS
		JR      NZ,J4B95		; error, retry
		LD      HL,(BOTTOM)		; BASIC bottom
		XOR     A
		LD      (HL),A			; make sure bottom start with BASIC end of line token
		INC     HL
		LD      (TXTTAB),HL		; start of BASIC program
		LD      (HL),A
		INC     HL
		LD      (HL),A			; pointer to next line = 0 (end of program)
		INC     HL
		LD      (VARTAB),HL		; start of BASIC variables
		LD      HL,-1
		LD      (CURLIN),HL		; BASIC interpreter in direct mode
		LD      SP,(STKTOP)		; switch to BASIC stack
		LD      A,0FFH
		LD      (CNSDFG),A		; function keys = enabled
		LD      IX,M7D31
		CALL    CALBAS			; BASIC welcome message
		CALL    C4D61			; CR/LF to screen
		LD      DE,I413A		; DiskBASIC version string
		CALL    C4D7D			; string to screen
		CALL    C4D61			; CR/LF to screen
		LD      HL,NTSTOP
		PUSH    HL                      ; execute RUN command
		LD      HL,BUF+11
		PUSH    HL			; BASIC pointer
		LD      HL,BUF+10
		PUSH    HL			; restore BASIC pointer routine
		LD      (HL),0E1H
		INC     HL
		LD      (HL),0C9H
		LD      A,(EXPTBL+0)
		LD      H,40H			; rem: HIGH 4000H
		JP      ENASLT			; switch page 1 to BASIC ROM

; Subroutine initialize DiskBASIC
C4BE8:		LD      HL,(HIMSAV)
		LD      (HIMEM),HL		; restore HIMEM from registered disk system HIMEM
		CALL    C6A06			; select system segments
		LD      HL,I6563		; warm start DiskBASIC
		LD      (DISKVE),HL		; install BDOS disk error handler
		LD      HL,I6568		; handle orginal BDOS error, restart DiskBASIC when none
		LD      (BREAKV),HL		; install BDOS abort handler
		LD      BC,(SMAXSEC)		; biggest sector size
		CALL    C4C40			; allocate memory (adjust HIMEM, halt when error)
		LD      (SDIRBUF),HL		; pointer to directory buffer
		LD      (PATHNAM),HL		; pointer to path name buffer
		LD      BC,13			; size of BLOAD/SAVE code
		CALL    C4C40			; allocate memory (adjust HIMEM, halt when error)
		CALL    C4C22			; intialize BLOAD/SAVE code
		CALL    C565C			; allocate i/o channels (system + user)

; Subroutine disable page 1 support
C4C16:		LD      A,0C9H

; Subroutine update page 1 support
C4C18:		LD      (SDOSON+0),A
		LD      (SDOSOF+0),A
		LD      (XFER+0),A
		RET

; -------------------------------------
		DOSENT  0456FH
DOS_CLOSE:      DOSCALL 010H
; -------------------------------------

; Subroutine 
C4C22:		LD      (BLDCHK),HL
		EX      DE,HL
		LD      HL,I4C90
		LDIR
		LD      HL,-5
		ADD     HL,DE
		LD      (BSVCHK),HL
		LD      A,(MASTER)
		LD      HL,-12
		ADD     HL,DE
		LD      (HL),A
		LD      HL,-4
		ADD     HL,DE
		LD      (HL),A
		RET

; Subroutine allocate memory (adjust HIMEM, halt when error)
C4C40:		LD      HL,(HIMEM)
		OR      A
		SBC     HL,BC
		LD      (HIMEM),HL
		JR      C,J4C54			; halt system
		LD      A,H
		CP      0C2H
		JR      J4C53

; Subroutine allocate memory (adjust BASIC areapointers, halt when error)
C4C50:		CALL    C5604			; allocate memory (adjust BASIC areapointers)
J4C53:		RET     NC

; Halt system
J4C54:		LD      A,12			; character = CLS
		CALL    C4D97			; character to screen
		LD      A,1			; message = 1
		LD      DE,BUF
		CALL    C4106			; copy message to buffer
		CALL    C4D7D			; string to screen
		DI
		HALT

; Subroutine patch H_LOPD if H_CLEAR is patched (to regain control of BASIC initialization from extension)
C4C66:		LD      HL,H_CLEA
		LD      A,(HL)
		CP      0C9H
		RET     Z
		LD      HL,H_LOPD
		LD      DE,I4C82

; Subroutine patch hook
C4C73:		LD      (HL),0F7H
		INC     HL
		LD      A,(MASTER)
		LD      (HL),A
		INC     HL
		LD      (HL),E
		INC     HL
		LD      (HL),D
		INC     HL
		LD      (HL),0C9H
		RET

I4C82:		LD      A,0C9H
		LD      (H_LOPD),A
		LD      DE,(HIMEM)
		LD      (HIMSAV),DE		; reregister HIMEM for disk system enviroment
		RET

I4C90:		RST    	R_CALLF
		DEFB    0
		DEFW    C5DB6
		PUSH    HL
		JP      BLDFIN

		RST    	R_CALLF
		DEFB    0
		DEFW	C5CF2
		RET

; Subroutine physical disk i/o (H_PHYD)
C4C9D:		EI
		PUSH    HL
		PUSH    AF
		CALL    C4CB9
		JR      C,J4CAB
		SCF
		LD      A,0CH
		POP     HL
		POP     HL
		RET

J4CAB:		LD      L,A
		POP     AF
		LD      A,L
		POP     HL
		PUSH    HL
		LD      IX,A4010
		CALL    CALSLT
		POP     HL
		RET

; -------------------------------------
		DOSENT  0461DH
DOS_CREATE:     DOSCALL 016H
; -------------------------------------

; Subroutine
C4CB9:		PUSH    BC
		LD      (TARGET),A
		LD      HL,DRVTBL
		LD      B,4
J4CC2:		SUB     (HL)
		JR      NC,J4CCD
		ADD     A,(HL)
		INC     HL
		LD      H,(HL)
		PUSH    HL
		POP     IY
		POP     BC
		RET

J4CCD:		INC     HL
		INC     HL
		DJNZ    J4CC2
		POP     BC
		RET

J4CD3:		LD      HL,DRVTBL
		LD      B,4
J4CD8:		INC     HL
		LD      A,(HL)
		PUSH    AF
		POP     IY
		INC     HL
		PUSH    HL
		PUSH    BC
		LD      HL,A401F
		PUSH    HL
		POP     IX
		OR      A
		CALL    NZ,RDSLT
		OR      A
		CALL    NZ,CALSLT
		POP     BC
		POP     HL
		DJNZ    J4CD8
		RET

; Subroutine DOS2 H_TIMI handler
C4CF3:		PUSH    AF			; store VDP status register
		CALL    C6A44			; DOS2 interrupt handler
		CALL    C4CFE			; execute disk interface interrupt handlers
		POP     AF			; restore VDP status register
		JP      TIMI_S			; next H_TIMI handler

; Subroutine execute disk interface interrupt handlers
C4CFE:		LD      DE,DRVTBL
		LD      HL,HOOKSA
		LD      B,4
J4D06:		LD      A,(DE)
		AND     A			; entry used ?
		RET     Z			; nope, end of table, quit
		INC     DE
		LD      A,(DE)			; slot id interface
		INC     DE
		CP      (HL)			; does interface have a driver interrupt handler ?
		JR      NZ,J4D27		; nope, next entry
		LD      A,(MASTER)
		CP      (HL)			; driver interrupt handler in DOS master ROM ?
		LD      A,(HL)			; slot id
		PUSH    BC
		PUSH    DE
		PUSH    HL
		INC     HL
		LD      E,(HL)
		INC     HL
		LD      D,(HL)			; address interrupt handler
		PUSH    AF
		POP     IY			; IYH = slot id
		PUSH    DE
		POP     IX			; IX = address interrupt handler
		CALL    C4D2D			; call driver interrupt handler
		POP     HL
		POP     DE
		POP     BC
J4D27:		INC     HL
		INC     HL
		INC     HL
		DJNZ    J4D06			; next entry
		RET

; Subroutine call driver interrupt handler
C4D2D:		JP      NZ,CALSLT		; not in MASTER rom, use CALSLT
		JP      (IX)			; in MASTER rom, use fast jump

; Subroutine get fresh key, check for abort key
C4D5B:		CALL    C4D86			; get fresh key
		CP      3			; CTRL-STOP ?
		RET

; Subroutine message to screen
C4D6E:		PUSH    HL
		PUSH    DE
		LD      DE,(SSECBUF)
		CALL    C4106			; copy message to buffer
		CALL    C4D7D			; string to screen
		POP     DE
		POP     HL
		RET

; Subroutine string to screen
C4D7D:		LD      A,(DE)
		INC     DE
		OR      A
		RET     Z
		CALL    C4D97			; character to screen
		JR      C4D7D

; -------------------------------------
		DOSENT  046BAH
DOS_ABSREA:     DOSCALL 02FH
; -------------------------------------

; Subroutine prompt for phantom drive
PROMPT:
I4D32:		LD      A,(TARGET)
		ADD     A,'A'
		CALL    H_PROM
		PUSH    AF			; store drive letter
		CALL    C4D61			; CR/LF to screen
		LD      A,7			; message = 7
		CALL    C4D6E			; message to screen
		POP     AF			; restore drive letter
		CALL    C4D97			; character to screen
		LD      A,8			; message = 8
		CALL    C4D6E			; message to screen
		CALL    C4D61			; CR/LF to screen
		LD      A,9			; message = 9
		CALL    C4D6E			; message to screen
J4D54:		CALL    C4D5B			; get fresh key, check for abort key
		JR      Z,J4D54			; abort, again
		; Mod: remove JR C4D61
		; Mod: move routine C4D5B 

; Subroutine CR/LF to screen
C4D61:		PUSH    AF
		LD      A,13
		CALL    C4D97			; character to screen
		LD      A,10
		CALL    C4D97			; character to screen
		POP     AF
		RET

; Subroutine get fresh key
C4D86:		PUSH    IX
		PUSH    HL
		LD      IX,KILBUF
		CALL    C4DA3
		POP     HL
		LD      IX,CHGET
		JR      J4D9D

; Subroutine character to screen
C4D97:		PUSH    IX
		LD      IX,CHPUT
J4D9D:		CALL    C4DA3
		POP     IX
		RET

; Subroutine
C4DA3:		PUSH    IY
		LD      IY,(EXPTBL+0-1)
		CALL    CALSLT
		EI
		POP     IY
		RET

; -------------------------------------
		DOSENT  04720H
DOS_ABSWRI:     DOSCALL 030H
; -------------------------------------

; Subroutine get pointer to driver work area
GETWRK:         CALL    C4DBB			; get pointer to SLTWRK entry
		LD      A,(HL)
		INC     HL
		LD      H,(HL)
		LD      L,A
		PUSH    HL
		POP     IX
		RET

; Subroutine get pointer to SLTWRK entry
C4DBB:		IN      A,(0A8H)
		AND     0CH
		RRCA
		RRCA
		LD      HL,EXPTBL
		CALL    C4DDC			; HL=HL+A
		ADD     A,A
		ADD     A,A
		ADD     A,A
		ADD     A,A
		INC     A
		LD      C,A
		LD      A,(HL)
		ADD     A,A
		SBC     A,A
		AND     0CH
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		AND     (HL)
		OR      C
		ADD     A,A
		LD      HL,SLTWRK

; Subroutine HL=HL+A
C4DDC:		LD      C,A
		LD      B,0
		ADD     HL,BC
		RET

; Subroutine register disk driver interrupt handler
SETINT:	        EX      DE,HL
		CALL    GETSLT
		PUSH    AF
		LD      A,(DISKID)		; current disk interface count
		LD      HL,HOOKSA
		CALL    C4DDC			; HL = HL+A
		ADD     HL,BC
		ADD     HL,BC			; pointer to HOOKSA entry
		POP     AF
		LD      (HL),A			; store slot id interface
		INC     HL
		LD      (HL),E
		INC     HL
		LD      (HL),D			; store address driver interrupt handler

; Subroutine previous interrupt handler
PRVINT:	        RET

; -------------------------------------
		DOSENT  04775H
DOS_SEQRD:      DOSCALL 014H

		DOSENT  0477DH
DOS_SEQWRT:     DOSCALL 015H

		DOSENT  04788H
DOS_RNDRD:      DOSCALL 021H

		DOSENT  04793H
DOS_RNDWRT:     DOSCALL 022H
; -------------------------------------

C53AC:	        LD	HL,0C91AH		; LD A,(DE)  RET
		PUSH	HL 			; on stack (routine)
		CALL	C53C0			; Get char out of memory
		POP	HL
		CP	'$'			; check "$" encountered
		RET	Z			; yes, quit
		PUSH	DE
		LD	E,A
		CALL	DOS_CONOUT		; Print char (BDOS #02)
		POP	DE
		INC	DE
		JR	C53AC			; next char

; -------------------------------------
		DOSENT  047B2H
DOS_BLKRD:      DOSCALL 027H

		DOSENT  047BEH
DOS_BLKWRT:     DOSCALL 026H
; -------------------------------------

C53C0:	        LD	HL,SDOSON
		PUSH	HL 			; Routine Switch SystemDiskROM on page 1 on stack
		LD	HL,4
		ADD	HL,SP
		PUSH	HL 			; Call "routine" on stack
		JP	SDOSOF			; Switch RAM on page 1

; -------------------------------------
		DOSENT  047D1H
DOS_ZWRITE:     DOSCALL 028H
; -------------------------------------

; Subroutine get slot id of page 0
; Mod: this routine is not used
;Q4DF8:		PUSH    HL
;		PUSH    BC
;		IN      A,(0A8H)		; current slot (in page 0)
;		CALL    C4E38			; get secondary slot register (if slot is expanded)
;		JR      Z,J4E35			; slot is not expanded, return slot id
;		RLCA
;		RLCA				; secondary slot page 0 in b3-b2
;		JR      J4E30			; make slot id and return

; Subroutine get slot id of page 1
C4E05:		PUSH    HL
		PUSH    BC
		IN      A,(0A8H)
		RRCA
		RRCA    			; current slot in page 1
		CALL    C4E38			; get secondary slot register (if slot is expanded)
		JR      Z,J4E35			; slot is not expanded, return slot id
		JR      J4E30			; (secondary slot page 1 already in b3-b2), make slot id and return

; Subroutine get slot id of page 2
C4E12:		PUSH    HL
		PUSH    BC
		IN      A,(0A8H)
		RRCA
		RRCA
		RRCA
		RRCA    			; current slot in page 2
		CALL    C4E38			; get secondary slot register (if slot is expanded)
		JR      Z,J4E35			; slot is not expanded, return slot id
		JR      J4E2E			; secondary slot page 2 in b3-b2, make slot id and return

; Subroutine get slot id of page 3
C4E21:		PUSH    HL
		PUSH    BC
		IN      A,(0A8H)
		RLCA
		RLCA    			; current slot in page 3
		CALL    C4E38			; get secondary slot register (if slot is expanded)
		JR      Z,J4E35			; slot is not expanded, return slot id
		RRCA
		RRCA
J4E2E:		RRCA
		RRCA				; secondary slot in b3-b2
J4E30:		AND     0CH			; secondary slot
		OR      80H			; slot expanded
		OR      C			; primary slot
J4E35:		POP     BC
		POP     HL
		RET

; Subroutine get secondary slot register (if slot is expanded)
C4E38:		AND     03H			; primary slot
		LD      C,A
		LD      B,0
		LD      HL,EXPTBL
		ADD     HL,BC
		BIT     7,(HL)			; is slot expanded ?
		RET     Z			; nope, quit
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		LD      A,(HL)			; current secondary slot
		RET

; Subroutine divide
DIV16:		LD      HL,0
		LD      A,17
J4E4F:		RL      C
		RL      B
		DEC     A
		RET     Z
		ADC     HL,HL
		JR      NC,J4E5E
		OR      A
		SBC     HL,DE
		JR      J4E4F

J4E5E:		SBC     HL,DE
		JR      NC,J4E63
		ADD     HL,DE
J4E63:		CCF
		JR      J4E4F

; Subroutine format disk (use free BASIC memory as work area) (H_FORM)
C4E66:		OR      A			; use free BASIC memory as work area

; Subroutine format disk
J4E67:		EI
		JR      C,J4E7C
		LD      HL,-256
		ADD     HL,SP
		LD      DE,(STREND)
		XOR     A
		SBC     HL,DE
		LD      C,L
		LD      B,H
		EX      DE,HL
		JR      NC,J4E7C
		LD      C,A
		LD      B,A
J4E7C:		PUSH    HL			; store start of work area
		PUSH    BC			; store size of work area
		LD      L,0
		LD      BC,8*256+67H		; drive = 8, function = format a disk
J4E83:		ADD     HL,HL
		PUSH    HL			; store drive bitmap
		PUSH    BC			; store drive, function
		XOR     A			; return choice string
		CALL    BDOS
		POP     BC			; restore drive, function
		POP     HL			; restore drive bitmap
		JR      NZ,J4E8F		; error, reset bit
		INC     HL			; set bit
J4E8F:		DJNZ    J4E83			; next drive
		LD      A,L
		OR      A			; no drives ?
		JP      Z,J4F47			; yep, quit
		DEC     A
		AND     L			; 1 drive ?
		LD      A,L
		JR      Z,J4EDE			; yep, skip drive letter choice
		PUSH    HL			; store drive bitmap
		LD      A,2			; message = 2
		CALL    C4D6E			; message to screen
		POP     HL			; restore drive bitmap
		PUSH    HL			; store drive bitmap
		LD      A,'A'-1
J4EA5:		INC     A
		SRL     L
		JR      NC,J4EB4
		CALL    C4D97			; character to screen
		PUSH    AF			; store drive letter
		LD      A,','
		CALL    NZ,C4D97		; character to screen
		POP     AF			; restore drive letter
J4EB4:		JR      NZ,J4EA5		; more drives, next drive
		LD      A,3			; message = 3
		CALL    C4D6E			; message to screen
		POP     HL			; restore drive bitmap
J4EBC:		CALL    C4D5B			; get fresh key, check for abort key
		JP      Z,J4F47			; abort,
		AND     0DFH			; to upper
		LD      C,A			; store drive letter
		SUB     'A'
		CP      8			; valid drive id ?
		JR      NC,J4EBC		; nope, again
		LD      B,A
		INC     B
		XOR     A
		SCF
J4ECF:		RLA
		DJNZ    J4ECF
		AND     L			; valid drive ?
		JR      Z,J4EBC			; nope, again
		PUSH    AF			; store drive bit
		LD      A,C			; restore drive letter
		CALL    C4D97			; character to screen
		CALL    C4D61			; CR/LF to screen
		POP     AF			; restore drive bit
J4EDE:		INC     B
		RRCA
		JR      NC,J4EDE
		LD      C,67H			; function = format a disk
		PUSH    BC			; store drive id
		XOR     A			; return choice string
		CALL    BDOS
		LD      A,L
		OR      H			; drive has choice string ?
		JR      Z,J4F1C			; nope, use choice 1
		LD      A,B			; slot id
J4EEE:		PUSH    AF			; store slot id
		CALL    RDSLT
		OR      A			; end of choice string ?
		JR      Z,J4EFC			; yep,
		CALL    C4D97			; character to screen
		POP     AF			; restore slot id
		INC     HL
		JR      J4EEE

J4EFC:		POP     AF			; restore slot id
		LD      A,'?'
		CALL    C4D97			; character to screen
		LD      A,' '
		CALL    C4D97			; character to screen
J4F07:		CALL    C4D5B			; get fresh key, check for abort key
		JR      Z,J4F46			; abort,
		SUB     '1'
		CP      8+1			; valid choice ?
		JR      NC,J4F07		; nope, again
		ADD     A,'1'
		CALL    C4D97			; character to screen
		CALL    C4D61			; CR/LF to screen
		SUB     '1'
J4F1C:		INC     A			; choice (1 based)
		PUSH    AF			; store choice
		LD      A,4			; message = 4
		CALL    C4D6E			; message to screen
		CALL    C4D5B			; get fresh key, check for abort key
		JR      Z,J4F45			; abort,
		CALL    C4D61			; CR/LF to screen
		POP     AF			; restore choice
		POP     BC			; restore drive id
		POP     DE			; restore size of work area
		POP     HL			; restore start of work area
		CALL    BDOS
		JR      Z,J4F41			; no error,
		LD      B,A			; error code
		LD      DE,(SSECBUF)		; use sector buffer
		LD      C,66H			; function = explain
		CALL    BDOS
		JP      C4D7D			; string to screen

J4F41:		LD      A,6			; message = 6
		JR      J4F4E			; message + CR/LF to screen

J4F45:		POP     AF
J4F46:		POP     AF
J4F47:		POP     AF
		POP     AF
		CALL    C4D61			; CR/LF to screen
		LD      A,5			; message = 5

; Subroutine message + CR/LF to screen
J4F4E:		CALL    C4D6E			; message to screen
		JP      C4D61			; CR/LF to screen

; Subroutine execute DOS1 BDOS function call (with CPMCAL=0)
J4F54:		XOR     A
		LD      (CPMCAL),A		; assume not a CP/M function call (for compatibility with MSXDOS1)
		JP      J6A86			; execute DOS1 BDOS function

; Subroutine allocate memory (adjust BASIC areapointers)
C5604:		LD      A,L
		OR      H
		RET     Z
		EX      DE,HL
		LD      HL,0
		SBC     HL,DE
		LD      C,L
		LD      B,H
		ADD     HL,SP
		CCF
		RET     C
		LD      A,H
		CP      TMPSTK / 256		; rem: HIGH TMPSTK
		RET     C
		LD      DE,(BOTTOM)
		SBC     HL,DE
		RET     C
		LD      A,H
		CP      02H
		RET     C
		PUSH    BC
		LD      HL,0
		ADD     HL,SP
		LD      E,L
		LD      D,H
		ADD     HL,BC
		PUSH    HL
		LD      HL,(STKTOP)
		OR      A
		SBC     HL,DE
		LD      C,L
		LD      B,H
		INC     BC
		POP     HL
		LD      SP,HL
		EX      DE,HL
		LDIR
		POP     BC
		LD      HL,(HIMEM)
		ADD     HL,BC
		LD      (HIMEM),HL
		LD      DE,-(2*256+2*9+2*2)	; 2 i/o channels, 2 FILTAB entries
		ADD     HL,DE
		LD      (FILTAB),HL
		EX      DE,HL
		LD      HL,(MEMSIZ)
		ADD     HL,BC
		LD      (MEMSIZ),HL
		LD      HL,(NULBUF)
		ADD     HL,BC
		LD      (NULBUF),HL
		LD      HL,(STKTOP)
		ADD     HL,BC
		JR      J5681			; update BASIC stack and initialize i/o channels

; Subroutine allocate i/o channels (system + user)
C565C:		LD      A,1
		LD      (MAXFIL),A		; number of user i/o channels = 1
		LD      HL,(HIMEM)
		LD      DE,-(2*256+2*9+2*2)	; 2 i/o channels, 2 FILTAB entries
		ADD     HL,DE
		LD      (FILTAB),HL		; pointer to i/o channel pointers
		LD      E,L
		LD      D,H			; store pointer to system i/o channel pointer
		DEC     HL
		DEC     HL
		LD      (MEMSIZ),HL		; top of BASIC memory
		LD      BC,200
		OR      A
		SBC     HL,BC			; 200 bytes string space
		PUSH    HL			; store top of BASIC stack
		LD      HL,2*2+9
		ADD     HL,DE
		LD      (NULBUF),HL		; pointer to system i/o channel buffer
		POP     HL			; restore top of BASIC stack

J5681:		LD      (STKTOP),HL		; update top of BASIC stack
		DEC     HL
		DEC     HL
		LD      (SAVSTK),HL
		LD      L,E
		LD      H,D
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		LD      A,2			; number of i/o channels
J5691:		EX      DE,HL
		LD      (HL),E
		INC     HL
		LD      (HL),D			; update pointer to i/o channel
		INC     HL
		EX      DE,HL
		LD      BC,7
		LD      (HL),B			; i/o channel mode = closed
		ADD     HL,BC			; +7
		LD      (HL),B			; clear i/o channel flags
		LD      BC,256+9-7
		ADD     HL,BC			; pointer to next i/o channel
		DEC     A
		JR      NZ,J5691		; next i/o channel
		RET

; Subroutine patch hooks for disk system
C56A5:		LD      HL,I5757
		LD      DE,H_POSD
		LD      BC,5
		LDIR
		LD      HL,I56CD
J56B3:		LD      E,(HL)
		INC     HL
		LD      D,(HL)
		INC     HL
		LD      A,E
		OR      D
		RET     Z
		EX      DE,HL
		LD      (HL),0F7H
		INC     HL
		LD      A,(MASTER)
		LD      (HL),A
		INC     HL
		EX      DE,HL
		LDI
		LDI
		LD      A,0C9H
		LD      (DE),A
		JR      J56B3

I56CD:		DEFW	H_DSKO,C5EDC
		DEFW	H_DSKI,C5EBB
		DEFW	H_NAME,C6315
		DEFW	H_KILL,C6303
		DEFW	H_COPY,C6424
		DEFW	H_DSKF,C640D
		DEFW	H_LSET,C6026
		DEFW	H_RSET,C6025
		DEFW	H_FIEL,C5F9F
		DEFW	H_MKIS,C60E6
		DEFW	H_MKSS,C60E9
		DEFW	H_MKDS,C60EC
		DEFW	H_CVI,C6123
		DEFW	H_CVS,C6126
		DEFW	H_CVD,C6129
		DEFW	H_GETP,C5A2A
		DEFW	H_NOFO,C5A39
		DEFW	H_NULO,C5A82
		DEFW	H_NTFL,C5BEF
		DEFW	H_BINS,C5C51
		DEFW	H_BINL,C5C79
		DEFW	H_FILE,C61E0
		DEFW	H_DGET,C5F1E
		DEFW	H_FILO,C5BD6
		DEFW	H_INDS,C5B4F
		DEFW	H_LOC,C6361
		DEFW	H_LOF,C635E
		DEFW	H_EOF,C61C8
		DEFW	H_BAKU,C5BBD
		DEFW	H_PARD,C6665
		DEFW	H_NODE,C674F
		DEFW	H_ERRP,C6753
		DEFW	H_PHYD,C4C9D
		DEFW	H_FORM,C4E66
I5755:		DEFW	0

I5757:		INC     SP
		INC     SP
		JP      PARDEV+8

; ------------------------------------------------------------------------------
; *** EXTENSION ROM CALL statement handler ***
; ------------------------------------------------------------------------------
C575C:		EI
		LD      A,(H_PHYD)
		CP      0C9H
		SCF
		RET     Z
		PUSH    HL
		CALL    GETSLT
		LD      HL,MASTER
		CP      (HL)
		JR      NZ,J5795
		LD      HL,I5799
J5771:		LD      DE,PROCNM
J5774:		LD      A,(DE)
		CP      (HL)
		JR      NZ,J578B
		INC     DE
		INC     HL
		AND     A
		JR      NZ,J5774
		LD      E,(HL)
		INC     HL
		LD      D,(HL)
		POP     HL
		CALL    C665E			; get BASIC character
		CALL    C5789
		AND     A
		RET

; Subroutine
C5789:		PUSH    DE
		RET

J578B:		LD      C,0FFH
		XOR     A
		CPIR
		INC     HL
		INC     HL
		CP      (HL)
		JR      NZ,J5771
J5795:		POP     HL
		JP      OEMSTA

I5799:		DEFB	"SYSTEM"
		DEFB	0
		DEFW	C57D6

		DEFB	"FORMAT"
		DEFB	0
		DEFW	C581A

		DEFB	"CHDRV"
		DEFB	0
		DEFW	C5821

		DEFB	"CHDIR"
		DEFB	0
		DEFW	C585A

		DEFB	"MKDIR"
		DEFB	0
		DEFW	C5869

		DEFB	"RMDIR"
		DEFB	0
		DEFW	C587D

		DEFB	"RAMDISK"
		DEFB	0
		DEFW	C58AF

		DEFB	0

; Subroutine CALL SYSTEM statement
C57D6:		LD      DE,BUF+10
		JR      Z,J5805
		CALL    C6654			; check for BASIC character
		DEFB    "("
		LD      IX,FRMEVL
		CALL    C664F			; evaluate expression
		PUSH    HL
		LD      IX,FRESTR
		CALL    C664F
		LD      C,(HL)
		INC     HL
		LD      A,(HL)
		INC     HL
		LD      H,(HL)
		LD      L,A
		LD      DE,BUF+10
		INC     C
		DEC     C
		JR      Z,J57FF
		LD      B,00H
		LDIR
J57FF:		POP     HL
		CALL    C6654			; check for BASIC character
		DEFB    ")"
		RET     NZ
J5805:		XOR     A
		LD      (DE),A
		LD      IX,CLSALL
		CALL    C664F			; close all i/o channels
		CALL    TOTEXT
		CALL    ERAFNK
		LD      HL,BUF+10		; command line
		JP      J4ADC			; start MSXDOS from boot drive

; Subroutine CALL FORMAT statement
C581A:		RET     NZ
		PUSH    HL
		CALL    C4E66
		POP     HL
		RET

; Subroutine CALL CHDRV statement
C5821:		CALL    C634C			; check for "(", evaluate file expression and check if disk drive
		CALL    C6654			; check for BASIC character
		DEFB    ")"
		PUSH    HL
		LD      BC,0*256+5BH		; function = parse pathname, parse flag = no volume name
		CALL    C6559			; execute BDOS function with ASCIIZ string in buffer (handle error)
		EX      DE,HL
		LD      A,B
		AND     05H
		XOR     04H
		OR      (HL)
		JP      NZ,J65ED		; bad drive name error
		LD      A,C
		CALL    C5846
		DEC     A
		LD      E,A
		LD      C,0EH			; function = select disk
		CALL    BDOS
		POP     HL
		RET

; Subroutine
C5846:		PUSH    AF
		LD      C,18H			; function = get login vector
		CALL    BDOS
		POP     AF
		PUSH    AF
J584E:		SRL     H
		RR      L
		DEC     A
		JR      NZ,J584E
		JP      NC,J65ED		; bad drive name error
		POP     AF
		RET

; Subroutine CALL CHDIR statement
C585A:		CALL    C634C			; check for "(", evaluate file expression and check if disk drive
		CALL    C6654			; check for BASIC character
		DEFB    ")"
		PUSH    HL
		LD      C,5AH			; function = change current directory
		CALL    C6559			; execute BDOS function with ASCIIZ string in buffer (handle error)
		POP     HL
		RET

; Subroutine CALL MKDIR statement
C5869:		CALL    C634C			; check for "(", evaluate file expression and check if disk drive
		CALL    C6654			; check for BASIC character
		DEFB    ")"
		PUSH    HL
		LD      B,10H			; attributes = directory
		LD      C,44H			; function = create file handle
		CALL    C6559			; execute BDOS function with ASCIIZ string in buffer (handle error)
		CALL    C6343			; flush disk buffers
		POP     HL
		RET

; Subroutine CALL RMDIR statement
C587D:		CALL    C634C			; check for "(", evaluate file expression and check if disk drive
		CALL    C6654			; check for BASIC character
		DEFB    ")"
		RET     NZ
		PUSH    HL
		LD      B,10H			; search attributes = directories
		CALL    C59D3			; execute find first entry
		XOR     A
		PUSH    AF
J588D:		LD      A,(BUF+10+14)
		AND     10H			; directory ?
		JR      Z,J589F			; nope,
		LD      DE,BUF+10
		LD      C,4DH			; function = delete file or subdirectory
		CALL    C655D			; execute BDOS function (handle error)
		POP     AF
		SCF
		PUSH    AF
J589F:		CALL    C59D8
		JR      NC,J588D
		POP     AF
		LD      A,0D6H
		JP      NC,J65C3		; handle error
		CALL    C6343			; flush disk buffers
		POP     HL
		RET

; Subroutine CALL RAMDISK statement
C58AF:		CALL    C6654			; check for BASIC character
		DEFB    "("
		CP      ','                     ; Is it a ","
		LD      A,0FFH
		JR      Z,J58DB
		LD      IX,GETIN2
		CALL    C664F
		INC     D
		DEC     D
		JP      M,J662D			; illegal function call error
		LD      B,4
J58C7:		SRL     D
		RR      E
		JR      NC,J58CE
		INC     DE
J58CE:		DJNZ    J58C7
		LD      A,E
		INC     D
		DEC     D
		JR      NZ,J58D9
		CP      0FFH
		JR      NZ,J58DB
J58D9:		LD      A,0FEH
J58DB:		PUSH    AF
		LD      A,(HL)
		CP      ','
		LD      DE,0
		JR      NZ,J58EE
		CALL    C665F			; get next BASIC character
		LD      IX,PTRGET
		CALL    C664F
J58EE:		POP     BC
		CALL    C6654			; check for BASIC character
		DEFB    ")"
		RET     NZ
		LD      A,(VALTYP)
		CP      3
		JP      Z,J6627			; type mismatch error
		PUSH    HL
		PUSH    DE
		PUSH    AF
		LD      C,68H			; function = create or destroy RAM disk
		CALL    C655D			; execute BDOS function (handle error)
		LD      L,B
		LD      H,00H
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		LD      (DAC+2),HL
		POP     AF
		LD      (VALTYP),A
		POP     DE
		LD      A,E
		OR      D
		CALL    NZ,C591A
		POP     HL
		RET

; Subroutine
C591A:		PUSH    DE
		LD      HL,VALTYP
		LD      A,(HL)
		LD      C,A
		LD      (HL),02H
		LD      HL,DAC+2
		CP      02H
		JR      Z,J593E
		CP      04H
		JR      Z,J5932
		CP      08H
		JP      NZ,J6627		; type mismatch error
J5932:		PUSH    BC
		LD      IX,DOCNVF
		CALL    C664F			; convert DAC to other type
		POP     BC
		LD      HL,DAC
J593E:		LD      B,0
		POP     DE
		LDIR
		RET

; Subroutine check for file already open in one of the i/o channels
C5944:		PUSH    HL
		PUSH    DE
		PUSH    BC
		XOR     A
		LD      (BUF+10),A
		LD      HL,(FILTAB)
		LD      A,(MAXFIL)		; number of user i/o channels
J5951:		PUSH    AF
		LD      E,(HL)
		INC     HL
		LD      D,(HL)			; pointer to i/o channel
		INC     HL
		PUSH    HL			; store pointer in FILTAB
		EX      DE,HL
		LD      A,(HL)
		AND     A			; i/o channel open ?
		JR      Z,J598F			; nope, next
		INC     HL
		INC     HL
		INC     HL
		INC     HL			; +4
		LD      A,(HL)                  ; device
		CP      09H			; disk drive ?
		JR      NC,J598F		; nope, next
		LD      A,(BUF+10)
		AND     A
		JR      NZ,J597E
		PUSH    HL
		LD      DE,(PATHNAM)
		LD      B,06H			; search attributes = hidden, system
		LD      IX,BUF+10
		LD      C,40H			; function = find first entry
		CALL    C59DE			; execute BDOS function (allow file not found), handle error
		POP     HL
		JR      C,J5998			; file not found, quit
J597E:		DEC     HL
		DEC     HL
		DEC     HL			; +1
		LD      B,(HL)			; file handle
		LD      DE,BUF+10
		LD      C,4CH			; function = test file handle
		CALL    C655D			; execute BDOS function (handle error)
		LD      A,B
		AND     A			; same file ?
		JP      NZ,J663C		; yep, file already open error
J598F:		POP     HL			; restore pointer in FILTAB
		POP     AF
		DEC     A			; more i/o channels ?
		JP      P,J5951			; yep, next
		JP      J5A26			; restore registers and quit

J5998:		POP     HL
		POP     HL
		JP      J5A26			; restore registers and quit

; Subroutine
C599D:		PUSH    AF			; store search attributes
		LD      B,00H			; parse flag = no volume name
		LD      C,5BH			; function = parse pathname
		CALL    C6559			; execute BDOS function with ASCIIZ string in buffer (handle error)
		POP     AF			; restore search attributes
		PUSH    BC
		PUSH    DE
		PUSH    HL
		PUSH    AF			; store search attributes
		LD      B,A			; search attributes
		CALL    C59D3			; execute find first entry
		POP     AF			; restore search attributes
		POP     HL
		POP     DE
		POP     BC
		BIT     5,B
		RET     NZ
		LD      D,A			; store search attributes
		LD      A,(BUF+24)
		AND     10H
		RET     Z
		LD      A,L
		CP      E
		RET     Z
		PUSH    BC
		LD      B,D			; search attributes
		LD      DE,BUF+10
		PUSH    DE
		POP     IX
		LD      HL,I5755
		LD      C,40H			; function = find first entry
		CALL    C655D			; execute BDOS function (handle error)
		POP     BC
		RET

; Subroutine execute find first entry (execute find first entry (normal attributes))
C59D1:		LD      B,0			; search attributes = normal

; Subroutine execute find first entry
C59D3:		LD      C,40H			; function = find first entry
		JP      J6555			; execute BDOS function with ASCIIZ string in buffer (handle error), result FIB in BUF

; Subroutine
C59D8:		LD      IX,BUF+10
		LD      C,41H			; function = find next entry

; Subroutine execute BDOS function (allow file not found), handle error
C59DE:		CALL    BDOS
		RET     Z
		CP      0D7H			; file not found ?
		SCF
		RET     Z			; yep, return with Cx set
		JP      J65C3			; handle error

; Subroutine take control from caller (move parameters on stack)
; Inputs: IX = return address replacement, IYH = number of bytes to move

; This is what the stack looks like at entry:
; prim    exp
; +0      +0      returnaddress C59E9 caller
; +2      +2      CALLF BIOS registers
; +6      +14     returnaddress CALLF caller
; +8      +16     returnaddress hook caller

C59E9:		EI
		PUSH    HL
		PUSH    DE
		PUSH    BC
		PUSH    AF
		LD      A,(MASTER)
		ADD     A,A
		LD      HL,16
		JR      NC,J59F9
		LD      L,16+8
J59F9:		ADD     HL,SP
		PUSH    IX
		POP     BC
		LD      (HL),C
		INC     HL
		LD      (HL),B
		LD      HL,10
		ADD     HL,SP
		EX      DE,HL
		JR      J5A13

J5A07:		PUSH    IY
		POP     BC
J5A0A:		LD      C,(HL)
		LD      A,(DE)
		LD      (HL),A
		LD      A,C
		LD      (DE),A
		INC     HL
		INC     DE
		DJNZ    J5A0A
J5A13:		LD      A,(MASTER)
		ADD     A,A
		LD      HL,18
		JR      NC,J5A1E
		LD      L,18+8
J5A1E:		ADD     HL,SP
		LD      A,E
		SUB     L
		LD      A,D
		SBC     A,H
		JR      C,J5A07
J5A25:		POP     AF
J5A26:		POP     BC
		POP     DE
		POP     HL
		RET

; Subroutine get pointer to i/o channel (H_GETP)
C5A2A:		LD      IX,RETRTN
		LD      IY,2*256+0
		CALL    C59E9			; take control from caller (move parameters on stack)
		POP     HL			; restore pointer to i/o channel
		LD      A,(HL)			; i/o channel mode
		AND     A			; Zx = i/o channel open
		RET

; Subroutine open without for (H_NOFO)
C5A39:		EI
		LD      BC,256
		LD      (RECSIZE),BC		; default record size = 256
		CALL    C665E			; get BASIC character
		LD      A,E
		RET     Z
		PUSH    AF
		PUSH    HL			; store BASIC pointer
		LD      A,(MASTER)
		ADD     A,A
		LD      HL,12
		JR      NC,J5A53
		LD      L,12+8
J5A53:		ADD     HL,SP
		LD      A,(HL)
		CP      4			; i/o channel mode = random i/o ?
		JP      NZ,J6630		; nope, syntax error
		INC     HL
		LD      A,(HL)			; device code
		CP      09H			; disk drive ?
		JP      NC,J6630		; nope, syntax error
		POP     HL			; restore BASIC pointer
		CALL    C6654			; check for BASIC character
		DEFB    0FFH			; function token
		CALL    C6654			; check for BASIC character
		DEFB	092H			; LEN token
		CALL    C6654			; check for BASIC character
		DEFB    0EFH			; = token
		LD      IX,INTID2
		CALL    C664F
		DEC     DE
		INC     D
		DEC     D			; 0 or > 256 ?
		JP      NZ,J662D		; yep, illegal function call error
		INC     DE
		LD      (RECSIZE),DE		; store record size
		POP     AF
		RET

; Subroutine open i/o channel (H_NULO)
C5A82:		EI
		RET     NC			; not a disk device, quit
		LD      IX,RETRTN
		LD      IY,4*256+0
		CALL    C59E9			; take control from caller (move parameters on stack)
		CALL    C5944			; check for file already open in one of the i/o channels
		LD      (PTRFIL),HL		; interpreter input/output device = i/o channel pointer
		INC     HL
		INC     HL			; +2

		;DOS201 fix
		XOR     A
		LD      (HL),A                  ; record size = 1
		INC     HL			; +3
		LD      (HL),A                  ; clear backup character
		INC     HL			; +4
		LD      (HL),D                  ; update device
		INC     HL
		INC     HL			; +6
		LD      (HL),A                  ; position in buffer = 0
		LD      A,E
		PUSH    AF			; store i/o channel mode
		AND     82H			; i/o channel mode binary save or sequential output ?
		JR      Z,J5AB7			; nope,
J5AA6:		XOR     A			; open mode = normal
		LD      B,A			; attributes = default
		LD      C,44H			; function = create file handle
		CALL    C6559			; execute BDOS function with ASCIIZ string in buffer (handle error)
J5AAD:		POP     AF			; restore i/o channel mode
		LD      HL,(PTRFIL)
		LD      (HL),A			; update i/o channel mode
		INC     HL
		LD      (HL),B			; update file handle
		POP     AF
		POP     HL
		RET

J5AB7:		LD      A,E
		CP      4			; i/o channel mode = random i/o ?
		JR      NZ,J5AD4		; nope,
		LD      HL,(PTRFIL)
		INC     HL
		INC     HL			; +2
		LD      A,(RECSIZE)
		DEC     A
		LD      (HL),A                  ; update record size -1
		LD      DE,(PATHNAM)
		XOR     A
		LD      C,43H			; function = open file handle
		CALL    C59DE			; execute BDOS function (allow file not found), handle error
		JR      C,J5AA6			; file not found,
		JR      J5AAD

J5AD4:		CP      1			; i/o channel mode = sequential input ?
		JR      NZ,J5B0B		; nope,
		XOR     A
		LD      C,43H			; function = open file handle
		CALL    C6559			; execute BDOS function with ASCIIZ string in buffer (handle error)
		LD      HL,FLBMEM
		XOR     A
		CP      (HL)			; i/o channel in raw mode ?
		LD      (HL),A			; i/o channel mode = ascii
		JR      NZ,J5AAD		; yep, update mode,file handle and quit
		POP     AF
		LD      HL,(PTRFIL)
		LD      (HL),A			; update i/o channel mode
		INC     HL
		LD      (HL),B			; update file handle
		DEC     HL			; +0
		EX      DE,HL
		LD      HL,6
		ADD     HL,DE			; +6
		LD      (HL),0FFH               ; position in buffer = end of buffer
		PUSH    HL
		EX      DE,HL
		CALL    C5B60			; read character from i/o channel
		POP     HL
		DEC     HL
		DEC     HL
		DEC     HL			; +3
		LD      (HL),A			; update backup character
		INC     A			; binary save file id ?
		JR      NZ,J5B08		; nope,
		INC     HL
		INC     HL
		INC     HL
		INC     HL			; +7
		LD      (HL),80H
J5B08:		POP     AF
		POP     HL
		RET

J5B0B:		XOR     A
		LD      C,43H			; function = open file handle
		CALL    C6559			; execute BDOS function with ASCIIZ string in buffer (handle error)
		POP     AF
		LD      HL,(PTRFIL)
		LD      (HL),1			; i/o channel mode = sequential input
		INC     HL
		LD      (HL),B			; update file handle
		DEC     HL
		EX      DE,HL
		LD      HL,6
		ADD     HL,DE			; +6
		LD      (HL),0FFH               ; position in buffer = end of buffer
		EX      DE,HL
		LD      BC,0
		LD      E,C
		LD      D,B
J5B27:		PUSH    HL
		PUSH    DE
		PUSH    BC
		CALL    C5B60			; read character from i/o channel
		POP     BC
		POP     DE
		POP     HL
		JR      C,J5B3A			; end of file,
		INC     BC
		LD      A,C
		OR      B
		JR      NZ,J5B27
		INC     DE
		JR      J5B27

J5B3A:		PUSH    BC
		LD      (HL),02H
		INC     HL
		LD      B,(HL)
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		XOR     A
		LD      (HL),A
		POP     HL
		LD      C,4AH			; function = move file handle pointer
		CALL    BDOS			; execute function (relative to the beginning of the file)
		POP     AF
		POP     HL
		RET

; Subroutine read character from i/o channel (H_INDS)
C5B4F:		LD      IX,RETRTN
		LD      IY,6*256+0
		CALL    C59E9			; take control from caller (move parameters on stack)
		CALL    C5B60			; read character from i/o channel
		JP      J5A26			; restore registers and quit

; Subroutine read character from i/o channel
C5B60:		PUSH    HL			; store pointer to i/o channel
		LD      A,(HL)
		CP      1			; i/o channel mode = sequential input ?
		JP      NZ,J65EA		; nope, bad file mode error
		LD      E,L
		LD      D,H			; store pointer to i/o channel
		INC     HL
		INC     HL
		INC     HL			; +3
		LD      A,(HL)
		AND     A			; backup character ?
		JR      NZ,J5BAF		; yep, use backup character
		INC     HL
		INC     HL
		INC     HL			; +6
		INC     (HL)			; update position in buffer
		LD      A,(HL)			; postition in buffer
		INC     HL
		INC     HL
		INC     HL			; +9
		JR      NZ,J5BAA		; still in buffer,
		PUSH    HL			; store pointer to i/o channel buffer
		EX      DE,HL			; transfer address = i/o channel buffer
		INC     HL			; +1
		LD      B,(HL)			; file handle
		LD      HL,256			; number of bytes = 256
		LD      C,48H			; function = read from file handle
		CALL    BDOS
		JR      Z,J5B92			; no error, continue
		CP      0C7H			; end of file error ?
		JP      NZ,J65C3		; nope, handle error
		POP     HL			; restore pointer to i/o channel buffer
		LD      (HL),1AH		; put EOF character in buffer
		JR      J5BAE			; continue

J5B92:		LD      C,L
		LD      B,H			; number of bytes read
		DEC     H
		LD      A,L
		OR      H			; 256 bytes read ?
		POP     HL			; restore pointer to i/o channel buffer
		JR      Z,J5BAA			; yep,
		PUSH    HL			; store pointer to i/o channel buffer
		LD      E,L
		LD      D,H
		INC     D
		DEC     DE			; end of the i/o channel buffer
		ADD     HL,BC
		LD      A,C			; store number of bytes in buffer
		DEC     HL			; end of data in i/o channel buffer
		LDDR
		DEC     HL
		DEC     HL			; +6
		NEG
		LD      (HL),A			; update position in buffer
		POP     HL			; restore pointer to i/o channel buffer
J5BAA:		LD      C,A
		LD      B,0			; position in buffer
		ADD     HL,BC			; pointer to character in buffer
J5BAE:		LD      A,(HL)			; get character
J5BAF:		SUB     1AH
		SUB     1			; EOF character (set Cx) ?
		LD      A,(HL)			; get character
		POP     HL			; restore pointer to i/o channel
		INC     HL
		INC     HL
		INC     HL			; +3
		LD      (HL),0			; clear backup character
		RET     NC			; not end of file, quit
		LD      (HL),A			; end of file, backup character = EOF
		RET

; Subroutine backup character to i/o channel (H_BAKU)
C5BBD:		EI
		PUSH    HL
		LD      A,(MASTER)
		ADD     A,A
		LD      HL,8
		JR      NC,J5BCA
		LD      L,8+8
J5BCA:		ADD     HL,SP
		LD      (HL),NOSKCR % 256	; rem: LOW NOSKCR
		INC     HL
		LD      (HL),NOSKCR / 256	; rem: HIGH NOSKCR
		POP     HL
		INC     HL
		INC     HL
		INC     HL			; +3
		LD      (HL),C			; store backup character
		RET

; Subroutine write character to i/o channel (H_FILO)
C5BD6:		LD      IX,RETRTN
		LD      IY,8*256+0
		CALL    C59E9			; take control from caller (move parameters on stack)
		LD      A,(HL)
		CP      2			; i/o channel mode = sequential output ?
		JP      NZ,J65EA		; nope, bad file mode error
		POP     AF			; restore character
		PUSH    AF			; store character
		CALL    C5C2B			; write character to i/o channel
		JP      J5A25			; restore registers and quit

; Subroutine close i/o channel (H_NTFL)
C5BEF:		LD      IX,RETRTN
		LD      IY,4*256+0
		CALL    C59E9			; take control from caller (move parameters on stack)
		POP     HL			; restore pointer to i/o channel
		LD      A,(HL)
		SUB     2			; i/o channel mode = sequential output ?
		JR      NZ,J5C13		; nope, skip EOF
		PUSH    HL			; store pointer to i/o channel
		LD      HL,FLBMEM
		CP      (HL)			; i/o channel in raw mode ?
		LD      (HL),A			; i/o channel mode = ascii
		POP     HL			; restore pointer to i/o channel
		JR      NZ,J5C13		; yep, skip EOF
		LD      (HL),4			; i/o channel mode = random i/o
		LD      A,1AH			; EOF
		CALL    C5C2B			; write character to i/o channel
		CALL    NZ,C5C39		; buffer not full, flush i/o channel buffer
J5C13:		XOR     A
		CP      (HL)			; i/o channel open ?
		LD      (HL),A			; i/o channel closed
		PUSH    AF			; store i/o channel mode/status
		INC     HL			; +1
		LD      B,(HL)			; file handle
		LD      DE,6
		ADD     HL,DE			; +7
		LD      (HL),A
		LD      L,A
		LD      H,A
		LD      (PTRFIL),HL
		LD      C,45H			; function = close file handle
		CALL    C655D			; execute BDOS function (handle error)
		POP     AF			; restore i/o channel mode/status
		POP     HL
		RET

; Subroutine write character to i/o channel
C5C2B:		PUSH    HL			; store pointer to i/o channel
		LD      BC,6
		ADD     HL,BC			; +6
		LD      C,(HL)			; position in buffer
		INC     (HL)			; update position in buffer
		INC     HL
		INC     HL
		INC     HL			; +9
		ADD     HL,BC			; pointer in buffer
		LD      (HL),A			; put character in buffer
		POP     HL			; restore pointer to i/o channel
		RET     NZ			; buffer not full, quit

; Subroutine flush i/o channel buffer
C5C39:		PUSH    HL			; store pointer to i/o channel
		INC     HL			; +1
		LD      B,(HL)			; file handle
		LD      DE,5
		ADD     HL,DE			; +6
		LD      A,(HL)			; position in buffer
		INC     HL
		INC     HL
		INC     HL			; +9
		EX      DE,HL			; transfer address = i/o channel buffer
		DEC     A
		LD      L,A
		LD      H,0
		INC     HL			; number of bytes
		LD      C,49H			; function = write to file handle
		CALL    C655D			; execute BDOS function (handle error)
		POP     HL			; restore pointer to i/o channel
		RET

; Subroutine binary save (H_BINS)
C5C51:		CALL    C6888			; take control from hook caller
		PUSH    HL
		LD      IX,SCCPTR
		CALL    C664F
		LD      A,0FFH
		CALL    C5E8F			; write byte to file handle
		LD      DE,(TXTTAB)
		LD      HL,(VARTAB)
		AND     A
		SBC     HL,DE
		CALL    C5EA7			; write bytes to i/o channel 0
		LD      (NLONLY),A              ; not loading basic program, close i/o channels when requested
		POP     HL
		LD      IX,CLSFIL
		JP      C664F			; close i/o channel

; Subroutine binairy load (H_BINL)
C5C79:		LD      IX,M739A
		LD      IY,2*256+0
		CALL    C59E9			; take control from caller (move parameters on stack)
		POP     AF
		JP      Z,J65EA			; bad file mode error
		LD      IX,CLSALL
		JR	DOS_SRCHFR+5		; Mod: jump over BDOS entry

; -------------------------------------
		DOSENT  04FB8H
DOS_SRCHFR:     DOSCALL 011H
; -------------------------------------

		CALL    C664F			; close all i/o channels
		LD      HL,-199
		ADD     HL,SP
		LD      DE,(TXTTAB)
		SBC     HL,DE
		JP      C,J662A			; out of memory error
		PUSH    HL
		CALL    C5EAF			; get file handle i/o channel 0
		PUSH    BC
		XOR     A			; relative to the beginning of the file
		LD      DE,0
		LD      HL,1			; offset = 1
		LD      C,4AH			; function = move file handle pointer
		CALL    C655D			; execute BDOS function (handle error)
		POP     BC
		LD      DE,(TXTTAB)
		POP     HL
		PUSH    HL
		LD      C,48H			; function = read from file handle
		CALL    C655D			; execute BDOS function (handle error)
		POP     DE
		PUSH    HL
		AND     A
		SBC     HL,DE
		POP     HL
		JP      NC,J662A		; out of memory error
		LD      DE,(TXTTAB)
		ADD     HL,DE
		LD      (VARTAB),HL
		LD      IX,LINKER
		CALL    C664F
		JR	DOS_FILESI+5		; Mod: jump over BDOS entry

; -------------------------------------
		DOSENT  05006H
DOS_SRCHNX:     DOSCALL 012H

		DOSENT  0501EH
DOS_FILESI:     DOSCALL 023H
; -------------------------------------

		LD      A,(FILNAM+0)
		AND     A
		RET     NZ
		LD      (NLONLY),A              ; not loading basic program, close i/o channels when requested
		LD      HL,I5CED
		LD      DE,BUF+10
		LD      BC,5
		PUSH    DE
		LDIR
		POP     HL
		LD      IX,NEWSTT
		JP      C664F

I5CED:		DEFB	03AH,092H		; :RUN
		DEFB	0			; end of line
		DEFW	0			; end of program

; -------------------------------------
		DOSENT  0504EH
DOS_LOGIN:      DOSCALL 018H

		DOSENT  05058H
DOS_SETDMA:     DOSCALL 01AH

		DOSENT  0505DH
DOS_GETEFA:     DOSCALL 01BH
; -------------------------------------

; Subroutine BSAVE
C5CF2:		PUSH    DE			; store device code
		CALL    C5E7E			; check for ',' and evaluate address operand
		LD      (SAVENT),DE		; store start address
		PUSH    DE			; store start address
		CALL    C5E7E			; check for ',' and evaluate address operand
		LD      (SAVEND),DE		; store end address
		EX      (SP),HL			; store BASIC pointer, restore start address
		EX      DE,HL
		RST    	R_DCOMPR
		JP      C,J662D			; illegal function call error
		EX      DE,HL
		EX      (SP),HL			; store start address, restore BASIC pointer
		CALL    C665E			; get BASIC character
		SCF     			; source = RAM
		JR      Z,J5D26			; end of statement, source = RAM, continue
		CALL    C6654			; check for BASIC character
		DEFB    ","
		CP      'S'			; VRAM ?
		JR      NZ,J5D1E		; nope, evaluate execute address and continue
		CALL    C665F			; get next BASIC character
		AND     A			; source = VRAM
		JR      J5D26			; continue

; -------------------------------------
		DOSENT  0509FH
DOS_DSKRES:     DOSCALL 00DH

		DOSENT  050A9H
DOS_WRTFAT:     LD      BC,0FFH*256+5FH		; drive = all, function = flush disk buffers
		LD      D,0			; flush only
		JP      J4F54			; execute DOS1 BDOS function call (with CPMCAL=0)

		DOSENT  050C4H
DOS_GETDRV:     LD      C,019H
		JR      L50CA

		DOSENT  050C8H
DOS_SETRND:     LD      C,024H
L50CA:		JP      J4F54

		DOSENT  050D5H
DOS_SELDSK:     DOSCALL 00EH

		DOSENT  050E0H
DOS_BUFIN:      DOSCALL 00AH
; -------------------------------------

J5D1E:		CALL    C5E82			; evaluate address operand
		LD      (SAVENT),DE		; store execute address
		SCF     			; source = RAM
J5D26:		POP     BC			; restore start address
		JR      NC,J5D2E		; source = VRAM,
		INC     B
		DEC     B			; start address in page 2 or 3 ?
		JP      P,J662D			; nope, illegal function call error
J5D2E:		POP     DE			; restore device code
		PUSH    HL			; store BASIC pointer
		PUSH    BC			; store start address
		PUSH    AF			; store source type
		XOR     A			; i/o channel = 0
		LD      E,2			; file mode = sequential output
		LD      IX,OPNFIL
		CALL    C664F			; open i/o channel
		LD      A,0FEH			; file id = BSAVE
		CALL    C5E8F			; write byte to file handle
		POP     AF			; restore source type
		POP     HL			; restore start address
		PUSH    HL			; store start address
		PUSH    AF			; store source type
		CALL    C5E89			; write word to file handle
		LD      HL,(SAVEND)		; end address
		CALL    C5E89			; write word to file handle
		LD      HL,(SAVENT)		; execute address
		CALL    C5E89			; write word to file handle
		POP     AF			; restore source type
		POP     BC			; restore start address
		PUSH    AF			; store source type
		LD      HL,(SAVEND)		; end address
		AND     A
		SBC     HL,BC
		INC     HL			; number of bytes = end address - start address + 1
		POP     AF			; restore source type
		JR      NC,J5D76		; source = VRAM
		LD      E,C
		LD      D,B			; transfer address = start address
		CALL    C5EA7			; write bytes to i/o channel 0
J5D66:		LD      A,0FFH
		LD      (FLBMEM),A		; i/o channel mode = raw
		XOR     A			; i/o channel = 0
		LD      IX,CLSFIL
		CALL    C664F			; close i/o channel
		JP      J627A			; restore BASIC pointer and output back to screen

J5D76:		CALL    C6511			; store VRAM start address and number of bytes, get work area
J5D79:		PUSH    HL			; store size of work area
		LD      DE,(SAVENT)		; number of bytes
		RST    	R_DCOMPR		; number of bytes fits in work area ?
		PUSH    AF			; store fit flag
		PUSH    BC			; store start of work area
		LD      C,L
		LD      B,H			; size of VRAM transfer = size of work area
		LD      HL,(SAVEND)
		PUSH    HL			; store VRAM start address
		ADD     HL,BC
		LD      (SAVEND),HL		; update VRAM start address for next
		POP     HL			; restore VRAM start address
		POP     DE			; restore start of work area
		PUSH    DE			; store start of work area
		CALL    LDIRMV			; copy VRAM to RAM
		POP     BC			; restore start of work area
		POP     AF			; restore fit flag
		JR      NC,J5DAB		; fits, write remainer and finish
		POP     HL			; restore size of work area
		PUSH    HL			; store size of work area
		PUSH    BC			; store start of work area
		LD      E,C
		LD      D,B			; transfer address = start of work area
		CALL    C5EA7			; write bytes to i/o channel 0
		POP     BC			; restore start of work area
		POP     DE			; restore size of work area
		LD      HL,(SAVENT)
		AND     A
		SBC     HL,DE
		LD      (SAVENT),HL		; update number of bytes
		EX      DE,HL			; size of work area
		JR      J5D79			; next

J5DAB:		POP     HL			; restore size of work area
		LD      HL,(SAVENT)		; number of bytes
		LD      E,C
		LD      D,B			; transfer address = start of work area
		CALL    C5EA7			; write bytes to i/o channel 0
		JR      J5D66			; finish

; -------------------------------------
		DOSENT  05183H
DOS_CRLF:       LD      E,13
		CALL    DOS_CONOUT
		LD      E,10
		JP      DOS_CONOUT
; -------------------------------------

; Subroutine BLOAD
C5DB6:		PUSH    DE			; store device code
		XOR     A
		LD      (RUNBNF),A		; assume destination = RAM without execute
		LD      C,A
		LD      B,A			; assume offset = 0
		CALL    C665E			; get BASIC character
		JR      Z,J5DDF			; end of statement, use defaults
		CALL    C6654			; check for BASIC character
		DEFB    ","
		CP      'R'			; execute ?
		JR      Z,J5DCE			; yep,
		CP      'S'			; VRAM ?
		JR      NZ,J5DDA		; nope, evaluate offset and continue
J5DCE:		LD      (RUNBNF),A		; update destination/execute flag
		CALL    C665F			; get next BASIC character
		JR      Z,J5DDF			; end of statement, continue
		CALL    C6654			; check for BASIC character
		DEFB    ","
J5DDA:		CALL    C5E82			; evaluate address operand
		LD      B,D
		LD      C,E			; store offset
J5DDF:		POP     DE			; restore device code
		PUSH    HL			; store BASIC pointer
		PUSH    BC			; store offset
		LD      A,0FFH
		LD      (FLBMEM),A		; i/o channel mode = raw
		XOR     A			; i/o channel = 0
		LD      E,1			; file mode = sequential input
		LD      IX,OPNFIL
		CALL    C664F			; open i/o channel
		CALL    C5E92			; read byte from file handle
		CP      0FEH			; BSAVE file id ?
		JP      NZ,J65EA		; nope, bad file mode error
		POP     BC			; restore offset
		CALL    C5E70			; read word from file handle, adjust with offset
		PUSH    HL			; store start address
		CALL    C5E70			; read word from file handle, adjust with offset
		PUSH    HL			; store end address
		CALL    C5E70			; read word from file handle, adjust with offset
		LD      (SAVENT),HL		; store execute address
		POP     HL			; restore end address
		POP     BC			; restore start address
		AND     A
		SBC     HL,BC
		INC     HL			; number of bytes = end - start + 1
		LD      A,(RUNBNF)
		CP      'S'			; destination = VRAM ?
		JR      Z,J5E28			; yep,
		LD      E,C
		LD      D,B			; transfer address = start address
		CALL    C5EAF			; get file handle i/o channel 0
		LD      C,48H			; function = read from file handle
		CALL    C655D			; execute BDOS function (handle error)
J5E1F:		LD      IX,FINPRT
		CALL    C664F			; output back to screen
		POP     HL			; restore BASIC pointer
		RET

J5E28:		CALL    C6511			; store VRAM start address and number of bytes, get work area
J5E2B:		PUSH    HL			; size of work area
		PUSH    BC			; start of work area
		LD      DE,(SAVENT)		; number of bytes
		RST    	R_DCOMPR		; fits in work area ?
		PUSH    AF			; store fit flag
		LD      E,C
		LD      D,B			; transfer address = start of work area
		CALL    C5EAF			; get file handle i/o channel 0
		LD      C,48H			; function = read from file handle
		CALL    BDOS
		POP     AF			; restore fit flag
		POP     HL			; restore start of work area
		POP     BC			; restore size of work area
		PUSH    BC			; store size of work area
		PUSH    HL			; store start of work area
		PUSH    AF			; store fit flag
		LD      HL,(SAVEND)
		PUSH    HL			; store VRAM address
		ADD     HL,BC
		LD      (SAVEND),HL		; update VRAM address for next
		POP     DE			; restore VRAM address
		POP     AF			; restore fit flag
		POP     HL			; restore start of work area
		JR      NC,J5E62		; fit, copy remainer to VRAM and finish
		PUSH    HL			; store start of work area
		CALL    LDIRVM			; copy RAM to VRAM
		POP     BC			; restore start of work area
		POP     DE			; restore size of work area
		LD      HL,(SAVENT)
		AND     A
		SBC     HL,DE
		LD      (SAVENT),HL		; update number of bytes
		EX      DE,HL			; size of work area
		JR      J5E2B			; next

J5E62:		POP     BC			; restore size of work area
		LD      BC,(SAVENT)		; number of bytes
		CALL    LDIRVM			; copy RAM to VRAM
		XOR     A
		LD      (RUNBNF),A		; clear flag
		JR      J5E1F			; finish

; Subroutine read word from file handle, adjust with offset
C5E70:		PUSH    BC			; store offset
		CALL    C5E92			; read byte from file handle
		PUSH    AF			; store low byte
		CALL    C5E92			; read byte from file handle
		LD      H,A			; high byte
		POP     AF
		LD      L,A			; low byte
		POP     BC			; restore offset
		ADD     HL,BC			; word + offset
		RET

; Subroutine check for ',' and evaluate address operand
C5E7E:		CALL    C6654			; check for BASIC character
		DEFB    ","

; Subroutine evaluate address operand
C5E82:		LD      IX,ADRGET
		JP      C664F

; Subroutine write word to file handle
C5E89:		PUSH    HL
		LD      A,L
		CALL    C5E8F			; write byte to file handle
		POP     AF

; Subroutine write byte to file handle
C5E8F:		LD      C,49H			; function = write to file handle
		DEFB	21H

; Subroutine read byte from file handle
C5E92:	        LD	C,48H			; function = read from file handle
		CALL    C5EAF			; get file handle i/o channel 0
		PUSH    AF			; store byte (bogus for read)
		LD      HL,1
		ADD     HL,SP
		EX      DE,HL			; transfer address = high byte
		LD      HL,1			; number of bytes = 1
		PUSH    BC			; store file handle
		CALL    C655D			; execute BDOS function (handle error)
		POP     BC			; restore file handle
		POP     AF			; restore byte from file handle
		RET

; Subroutine write bytes to i/o channel 0
C5EA7:		CALL    C5EAF			; get file handle i/o channel 0
		LD      C,49H			; function = write to file handle
		JP      C655D			; execute BDOS function (handle error)

; Subroutine get file handle i/o channel 0
C5EAF:		PUSH    HL
		LD      HL,(FILTAB)
		LD      B,(HL)
		INC     HL
		LD      H,(HL)
		LD      L,B			; pointer to i/o channel
		INC     HL
		LD      B,(HL)			; file handle
		POP     HL
		RET

; Subroutine disk sector input (H_DSKI)
C5EBB:		CALL    C6888			; take control from hook caller
		CALL    C665F			; get next BASIC character
		CALL    C6654			; check for BASIC character
		DEFB    "("
		CALL    C5F09			; evaluate byte, check for "," and evaluate unsigned integer operand
		CALL    C6654			; check for BASIC character
		DEFB    ")"
		PUSH    HL
		LD      HL,NULSTR
		LD      (DAC+2),HL
		LD      A,3
		LD      (VALTYP),A
		LD      L,2FH			; function = absolute sector read
		JR      J5EE9

; Subroutine disk sector output (H_DSKO)
C5EDC:		CALL    C6888			; take control from hook caller
		CALL    C5F09			; evaluate byte, check for "," and evaluate unsigned integer operand
		CALL    C665E			; get BASIC character
		RET     NZ
		PUSH    HL
		LD      L,30H			; function = absolute sector write
J5EE9:		PUSH    HL
		PUSH    DE
		PUSH    BC
		LD      DE,BUF+10
		LD      L,C			; store drive id
		LD      C,31H			; function = get disk parameters
		CALL    C655D			; execute BDOS function (handle error)
		LD      DE,(SDIRBUF)
		LD      C,1AH			; function = set disk transfer address
		CALL    BDOS
		POP     HL
		DEC     L
		LD      H,1
		POP     DE
		POP     BC
		CALL    C655D			; execute BDOS function (handle error)
		POP     HL
		RET

; Subroutine evaluate byte, check for "," and evaluate unsigned integer operand
C5F09:		LD      IX,GETBYT
		CALL    C664F			; evaluate byte operand
		PUSH    DE			; store byte
		CALL    C6654			; check for BASIC character
		DEFB    ","
		LD      IX,GETUIN
		CALL    C664F			; evaluate unsigned integer operand
		POP     BC			; restore byte
		RET

; Subroutine (H_DGET)
C5F1E:		LD      IX,RETRTN
		LD      IY,4*256+0
		CALL    C59E9			; take control from caller (move parameters on stack)
		LD      A,(HL)			; i/o channel mode
		CP      4			; random i/o ?
		JP      NZ,J65EA		; nope, bad file mode error
		EX      (SP),HL			; store pointer to i/o channel, restore BASIC pointer
		CALL    C665E			; get BASIC character
		JR      Z,J5F7D			; end of statement, 
		CALL    C6654			; check for BASIC character
		DEFB    ","
		LD      IX,FRMEVL
		CALL    C664F			; evaluate expression
		PUSH    HL			; store BASIC pointer
		CALL    C6147			; convert DAC to 32 bit integer
		LD      A,C
		OR      B
		OR      L
		OR      H			; record number = 0 ?
		JP      Z,J662D			; yep, illegal function call error
		LD      A,C
		OR      B
		DEC     BC
		JR      NZ,J5F51
		DEC     HL			; record number zero based
J5F51:		EX      DE,HL
		POP     HL			; restore BASIC pointer
		EX      (SP),HL			; store BASIC pointer, restore pointer to i/o channel
		PUSH    HL			; store pointer to i/o channel
		PUSH    DE			; store high word
		INC     HL
		INC     HL			; +2
		LD      E,(HL)
		LD      D,0
		INC     DE			; record size
		CALL    C689F			; multiply
		POP     IX
		PUSH    BC
		PUSH    IX
		POP     BC			; store low word result, restore high word
		CALL    C68A2			; multiply high word
		LD      A,L
		OR      H			; record within 32 bit limit ?
		JP      NZ,J662D		; nope, illegal function call error
		LD      E,C
		LD      D,B
		POP     HL			; restore low word result
		POP     BC			; restore pointer to i/o channel
		PUSH    BC			; store pointer to i/o channel
		INC     BC			; +1
		LD      A,(BC)
		LD      B,A			; file handle
		XOR     A			; relative to the beginning of the file
		LD      C,4AH			; function = move file handle pointer
		CALL    BDOS
		POP     HL			; restore pointer to i/o channel
		EX      (SP),HL			; store pointer to i/o channel, restore BASIC pointer
		JR	J5F7D			; Mod: jump over BDOS entry

; -------------------------------------
		DOSENT  535DH
DOS_BUFOUT:     EXX
		PUSH    BC              	; errorcode
		EXX
		CALL    DOS_CRLF           	; CR/LF to console
		LD      A,10            	; message 10 (Abort)
		LD      DE,(SSECBUF)
		CALL    C4106			; copy message to buffer
		CALL    L5379           	; print prompt
		POP     BC
		LD      DE,(SSECBUF)
		LD      C,066H			; function = explain
		CALL    BDOS			; BDOS
						; print error string
L5379:		LD      A,(DE)
		OR      A
		RET     Z
		PUSH    DE
		LD      E,A
		CALL    DOS_CONOUT           	; print char
		POP     DE
		INC     DE
		JR      L5379
; -------------------------------------

J5F7D:		EX      (SP),HL			; store BASIC pointer, restore pointer to i/o channel
		INC     HL			; +1
		LD      B,(HL)			; file handle
		INC     HL			; +2
		LD      E,(HL)
		LD      D,0
		INC     DE			; record size
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		INC     HL			; +9
		EX      DE,HL
		POP     IX			; restore BASIC pointer
		POP     AF			; restore GET/PUT flag
		PUSH    IX			; store BASIC pointer
		AND     A			; PUT ?
		LD      C,48H			; function = read from file handle
		JR      Z,J5F99			; nope, GET
		LD      C,49H			; function = write to file handle
J5F99:		CALL    C655D			; execute BDOS function (handle error)
		JP      J627A			; restore BASIC pointer and output back to screen

; -------------------------------------
		DOSENT  053A7H
DOS_CONOUT:     DOSCALL 002H
; -------------------------------------

; Subroutine field (H_FIEL)
C5F9F:		CALL    C6888			; take control from hook caller
		CP      '#'
		CALL    Z,C665F			; get next BASIC character
		LD      IX,GETBYT
		CALL    C664F			; evaluate byte operand
		JP      Z,J6630			; end of statement, syntax error
		PUSH    HL			; store BASIC pointer
		LD      IX,FILIDX
		CALL    C664F			; get pointer to i/o channel
		LD      E,L
		LD      D,H			; store pointer to i/o channel
		JP      Z,J6645			; i/o channel closed, file not open error
		JP      C,J662D			; not a diskdrive device, illegal function call error
		LD      A,(HL)			; i/o channel mode
		CP      4			; random i/o ?
		JP      NZ,J65EA		; nope, bad file mode error
		INC     HL
		INC     HL			; +2
		LD      L,(HL)
		LD      H,0
		INC     HL			; record size
		LD      (BUF+10),HL		; store record size
		LD      HL,0
		LD      (BUF+12),HL		; total field size = 0
		LD      BC,9			; offset = to i/o channel buffer
		POP     HL			; restore BASIC pointer
J5FDA:		EX      DE,HL
		ADD     HL,BC
		EX      DE,HL			; update pointer in buffer
		LD      A,(HL)
		CP      ','			; field definition follow ?
		RET     NZ			; nope, quit
		PUSH    DE			; store pointer in buffer
		LD      IX,GTBYTC
		CALL    C664F			; evaluate ,byte operand
		PUSH    AF			; store field size
		CALL    C6654			; check for BASIC character
		DEFB    "A"
		CALL    C6654			; check for BASIC character
		DEFB    "S"
		LD      IX,PTRGET
		CALL    C664F			; get address of variable
		LD      IX,GETYPR
		CALL    C664F			; get DAC type
		JP      NZ,J6627		; no string, type mismatch error
		POP     AF			; restore field size
		EX      (SP),HL			; store BASIC pointer, restore pointer in buffer
		PUSH    DE			; store address of variable
		PUSH    HL			; store pointer in buffer
		LD      HL,(BUF+12)
		LD      C,A
		LD      B,0			; field size
		ADD     HL,BC
		LD      (BUF+12),HL		; update total field size
		EX      DE,HL
		LD      HL,(BUF+10)		; record size
		RST    	R_DCOMPR		; compare
		JP      C,J6633			; total field size > record size, field overflow error
		POP	DE			; restore pointer in buffer
		POP	HL			; restore address of variable
		LD      (HL),C			; size of string = field size
		INC     HL
		LD      (HL),E
		INC     HL
		LD      (HL),D			; pointer to string = pointer in buffer
		LD      B,0			; ?? B is already zero
		POP     HL			; restore BASIC pointer
		JR      J5FDA			; next field

; -------------------------------------
		DOSENT  0543CH
DOS_CONSTA:     DOSCALL 00BH

		DOSENT  05445H
DOS_CONIN:      DOSCALL 001H

		DOSENT  0544EH
DOS_IN:         DOSCALL 008H

		DOSENT  05454H
DOS_RAWIO:      DOSCALL 006H

		DOSENT  05462H
DOS_RAWINP:     LD      C,007H
		DEFB    011H            	; Pseudo LD DE,nnnn
DOS_LIST:       LD      C,005H
		JP      J4F54

		DOSENT  0546EH
DOS_READER:     DOSCALL 003H

		DOSENT  05474H
DOS_PUNCH:      DOSCALL 004H
; -------------------------------------

; Subroutine right set (H_RSET)
C6025:		DEFB	0F6H		        ; OR xx: trick to skip next instrunction

; Subroutine left set (H_LSET)
C6026:		SCF				; LSET flag
		CALL    C6888			; take control from hook caller
		PUSH    AF			; store LSET flag
		LD      IX,PTRGET
		CALL    C664F			; get address of variable
		LD      IX,GETYPR
		CALL    C664F			; get DAC type
		JP      NZ,J6627		; not a string, type mismatch error
		PUSH    DE			; store address of variable
		LD      IX,FRMEQL
		CALL    C664F			; evaluate =expression
		POP     BC			; restore address of variable
		EX      (SP),HL			; store BASIC pointer, restore LSET flag
		PUSH    HL			; store LSET flag
		PUSH    BC			; store address of variable
		LD      IX,FRESTR
		CALL    C664F			; free temporary string descriptor
		LD      B,(HL)			; size of string
		EX      (SP),HL			; store string descriptor, restore address of variable
		LD      A,(HL)
		LD      C,A			; size of field
		PUSH    BC			; store size of string, size of field
		PUSH    HL			; store address of variable
		PUSH    AF			; store string released
		INC     HL
		LD      E,(HL)
		INC     HL
		LD      D,(HL)			; pointer to string
		OR      A			; size of field = 0 ?
		JR      Z,J60B9			; yep, quit
		LD      HL,(NULBUF)		; pointer to system i/o channel buffer
		DEC     HL
		RST    	R_DCOMPR		; compare
		JR      C,J6097			; pointer to buffer, skip allocation
		LD      HL,(VARTAB)		; start of variable area
		RST    	R_DCOMPR		; compare
		JR      C,J6097			; not a string constant in program, skip allocation
		LD      E,C
		LD      D,0			; size of field
		LD      HL,(STKTOP)
		ADD     HL,DE
		EX      DE,HL
		LD      HL,(FRETOP)
		RST    	R_DCOMPR
		JP      C,J60CC
		POP     AF			; restore string released
J6079:		LD      A,C			; size of field
		LD      IX,GETSPA
		CALL    C664F			; allocate string space
		POP     HL			; restore address of variable
		POP     BC			; restore size of string, size of field
		EX      (SP),HL			; store address of variable, restore string descriptor
		PUSH    DE
		PUSH    BC
		LD      IX,FRESTR
		CALL    C664F			; free temporary string descriptor
		POP     BC
		POP     DE
		EX      (SP),HL
		PUSH    BC
		PUSH    HL
		INC     HL
		PUSH    AF			; store string released
		LD      (HL),E
		INC     HL
		LD      (HL),D			; update pointer to string
J6097:		POP     AF			; restore string released
		POP     HL
		INC     HL
		LD      E,(HL)
		INC     HL
		LD      D,(HL)			; pointer to string
		POP     BC
		POP     HL
		INC     HL
		LD      A,(HL)
		INC     HL
		LD      H,(HL)
		LD      L,A			; pointer to string
		LD      A,C
		CP      B			; compare size of string with size of field
		JR      NC,J60A9		; size of field >= size of string
		LD      B,A
J60A9:		SUB     B
		LD      C,A			; size difference
		POP     AF			; restore LSET flag
		CALL    NC,C60C3		; RSET, add spaces (before)
		INC     B
J60B0:		DEC     B
		JR      Z,J60BE
		LD      A,(HL)
		LD      (DE),A
		INC     HL
		INC     DE
		JR      J60B0

J60B9:		POP     BC
		POP     BC
		POP     BC
		POP     BC
		POP     BC			; clean up stack
J60BE:		CALL    C,C60C3			; LSET, add spaces (after)
		POP     HL
		RET

; Subroutine add spaces
C60C3:		LD      A,' '
		INC     C
J60C6:		DEC     C
		RET     Z
		LD      (DE),A
		INC     DE
		JR      J60C6

J60CC:		POP     AF			; store string released
		POP     HL			; restore address of variable
		POP     BC			; restore size of string, size of field
		EX      (SP),HL			; store address of variable, restore string descriptor
		EX      DE,HL
		JR      NZ,J60E0		; string not released, skip allocation
		PUSH    BC			; store size of string, size of field
		LD      A,B
		LD      IX,STRINI
		CALL    C664F			; allocate temp string
		CALL    C6107
		POP     BC			; restore size of string, size of field
J60E0:		EX      (SP),HL			; store , restore address of variable
		PUSH    BC			; store size of string, size of field
		PUSH    HL			; store address of variable
		JP      J6079			; continue

; -------------------------------------
		DOSENT  0553CH
DOS_GETDAT:     DOSCALL 02AH

		DOSENT  05552H
DOS_SETDAT:     DOSCALL 02BH
; -------------------------------------

; Subroutine make integer (H_MKI)
C60E6:		LD      A,2			; size of string = 2
		DEFB    001H

; Subroutine make single float (H_MKS)
C60E9:		LD      A,4			; size of string = 4
		DEFB    001H

; Subroutine make double float (H_MKD)
C60EC:		LD      A,8			; size of string = 8
		CALL    C6888			; take control from hook caller
		PUSH    AF			; store size of string
		LD      IX,DOCNVF
		CALL    C664F			; convert DAC to other type
		POP     AF			; restore size of string
		LD      IX,STRINI
		CALL    C664F			; allocate temp string
		LD      HL,(DSCTMP+1)		; pointer to string
		CALL    VMOVMF			; copy variable content from DAC

; Subroutine 
C6107:		LD      DE,DSCTMP
		LD      HL,(TEMPPT)
		LD      (DAC+2),HL
		LD      A,3
		LD      (VALTYP),A
		CALL    VMOVE			; copy string descriptor
		LD      DE,FRETOP
		RST    	R_DCOMPR
		LD      (TEMPPT),HL
		JP      Z,J6624			; string formula too complex error
		RET

; Subroutine convert to integer (H_CVI)
C6123:		LD      A,2-1			; target size -1
		DEFB    001H

; Subroutine convert to single float (H_CVS)
C6126:		LD      A,4-1			; target size -1
		DEFB    001H

; Subroutine convert to double float (H_CVD)
C6129:		LD      A,8-1			; target size -1
		CALL    C6888			; take control from hook caller
		PUSH    AF			; store target size -1
		LD      IX,FRESTR
		CALL    C664F			; free temporary string descriptor
		POP     AF			; resture target size -1
		CP      (HL)			; field size to small ?
		JP      NC,J662D		; yep, illegal function call error
		INC     A			; target size
		INC     HL
		LD      C,(HL)
		INC     HL
		LD      H,(HL)
		LD      L,C			; pointer to string
		LD      (VALTYP),A		; target type
		JP      VMOVFM			; copy variable content to DAC

; -------------------------------------
		DOSENT  055DBH
DOS_GETTIM:     DOSCALL 02CH

		DOSENT  055E6H
DOS_SETTIM:     DOSCALL 02DH

		DOSENT  055FFH
DOS_SETRAW:     DOSCALL 02EH
; -------------------------------------

; ------------------------------------------------------------------------------
; *** No more DOS entry points below this point ***
; ------------------------------------------------------------------------------

; Subroutine convert DAC to 32 bit integer
C6147:		LD      IX,GETYPR
		CALL    C664F			; get DAC type
		JP      M,J61B3			; integer,
		JP      Z,J6627			; string, type mismatch error
		LD      HL,DAC+0
		LD      A,(HL)
		AND     A			; positive float ?
		JP      M,J662D			; nope, illegal function call error
		LD      DE,BUF+10
		LD      BC,8
		LDIR				; store DAC
		LD      HL,I61C0
		LD      DE,ARG
		LD      C,8
		LDIR				; 65536 (double float)
		CALL    DECDIV			; DAC = DAC / ARG
		AND     A			; DAC type = double float
		CALL    INT			; integer part of float
		LD      IX,GETUI
		CALL    C664F			; convert to unsigned integer
		PUSH    DE			; store high word value
		EX      DE,HL
		LD      IX,FLTLIN
		CALL    C664F			; convert to single float
		CALL    CONDS			; convert to double float
		LD      BC,06545H
		LD      DE,06053H
		CALL    SGNMUL			; *65536
		LD      HL,DAC
		PUSH    HL
		LD      DE,ARG
		LD      BC,8
		LDIR				; ARG = DAC
		LD      HL,BUF+10
		POP     DE
		LD      C,8
		LDIR				; restore DAC
		CALL    DECSUB			; DAC = DAC - ARG
		LD      IX,GETUI
		CALL    C664F			; convert to unsigned integer
		LD      C,E
		LD      B,D			; low word value
		POP     HL			; restore high word value
		RET

J61B3:		LD      BC,(DAC+2)		; integer in DAC
		INC     B
		DEC     B			; positive integer ?
		JP      M,J662D			; nope, illegal function call error
		LD      HL,0			; high word value = 0
		RET

I61C0:		DEFB	045H,065H,053H,060H,0,0,0,0

; Subroutine end of file (H_EOF)
C61C8:		CALL    C6888			; take control from hook caller
		PUSH    HL
		CALL    C5B60			; get character from i/o channel
		LD      HL,0
		JR      NC,J61D5		; not end of file, result = 0
		DEC     HL			; end of file, result = -1
J61D5:		PUSH    AF
		CALL    MAKINT
		POP     AF
		POP     HL
		INC     HL
		INC     HL
		INC     HL			; +3
		LD      (HL),A			; update backup character
		RET

; Subroutine (H_FILE)
C61E0:		CALL    C6888			; take control from hook caller
		CALL    C665E			; get BASIC character
		JR      Z,J61F1
		CP      ','
		JR      Z,J61F1
		CALL    C6350			; evaluate file expression and check if disk drive
		JR      J61F7

J61F1:		XOR     A
		LD      DE,(PATHNAM)
		LD      (DE),A
J61F7:		CALL    C665E			; get BASIC character
		SCF
		JR      Z,J6206
		CALL    C6654			; check for BASIC character
		DEFB    ","
		CALL    C6654			; check for BASIC character
		DEFB    "L"
		AND     A
J6206:		PUSH    HL
		LD      A,(PRTFLG)
		INC     A
		DEC     A
		PUSH    AF
		LD      IX,CRDONZ
		CALL    C664F
		POP     AF
		PUSH    AF
		LD      A,16H
		JR      NC,J621C
		LD      A,10H			; search attributes = normal
J621C:		CALL    C599D
		LD      A,C
		ADD     A,'A'-1
		RST    	R_OUTDO
		LD      A,':'
		RST    	R_OUTDO
		LD      A,'\\'
		RST    	R_OUTDO
		LD      DE,BUF+75
		PUSH    DE
		LD      C,5EH			; function = get whole path string
		CALL    C655D			; execute BDOS function (handle error)
		LD      (HL),A
		DEC     HL
		LD      (HL),A
		POP     HL
		CALL    C62FC
		LD      IX,CRDO
		CALL    C664F
J6240:		POP     AF
		PUSH    AF
		CALL    C6282
		CALL    CKCNTC
		POP     AF
		PUSH    AF
		JR      NC,J6267
		LD      A,(LINLEN)
		LD      B,A
		LD      A,(TTYPOS)
		JR      Z,J625A
		LD      B,80
		LD      A,(LPTPOS)
J625A:		AND     A
		JR      Z,J626E
		ADD     A,0DH   ; 13
		CP      B
		JR      NC,J6267
		LD      A,20H   ; " "
		RST    	R_OUTDO
		JR      J626E

J6267:		LD      IX,CRDO
		CALL    C664F
J626E:		LD      IX,BUF+10
		LD      C,41H			; function = find next entry
		CALL    BDOS
		JR      Z,J6240
		POP     AF
J627A:		POP     HL			; restore BASIC pointer
		LD      IX,FINPRT
		JP      C664F			; output back to screen

; Subroutine 
C6282:		JR      NC,J62A4

; Subroutine 
C6284:		LD      DE,BUF+11
		LD      HL,(PATHNAM)
		LD      C,5CH			; function = parse filename
		CALL    C655D			; execute BDOS function (handle error)
		LD      B,08H
		CALL    C629E
		LD      A,(HL)
		CP      20H     ; " "
		JR      Z,J629B
		LD      A,2EH   ; "."
J629B:		RST    	R_OUTDO
		LD      B,03H

; Subroutine 
C629E:		LD      A,(HL)
		RST    	R_OUTDO
		INC     HL
		DJNZ    C629E
		RET

J62A4:		CALL    C6284
		LD      A,20H	; " "
		RST    	R_OUTDO
		LD      A,(BUF+24)
		LD      C,A
		BIT     4,C
		LD      A,64H   ; "d"
		CALL    C62F6
		BIT     0,C
		LD      A,72H	; "r"
		CALL    C62F6
		BIT     1,C
		LD      A,68H   ; "h"
		CALL    C62F6
		BIT     2,C
		LD      A,73H   ; "s"
		CALL    C62F6
		BIT     5,C
		LD      A,61H   ; "a"
		CALL    C62F6
		LD      BC,(BUF+31+0)
		LD      HL,(BUF+31+2)		; file size
		CALL    C63BA
		LD      IX,JPFOUT
		CALL    C664F
		INC     HL
		PUSH    HL
		LD      B,0CH
J62E6:		LD      A,(HL)
		INC     HL
		DEC     B
		AND     A
		JR      NZ,J62E6
		LD      A,20H   ; " "
J62EE:		RST    	R_OUTDO
		DJNZ    J62EE
		POP     HL
		CALL    C62FC
		RET

; Subroutine 
C62F6:		JR      NZ,J62FA
		LD      A,2DH   ; "-"
J62FA: 	        RST    	R_OUTDO
		RET

; Subroutine 
C62FC:		LD      A,(HL)
		AND     A
		INC     HL
		RET     Z
		RST    	R_OUTDO
		JR      C62FC

; Subroutine remove file (H_KILL)
C6303:		CALL    C6888			; take control from hook caller
		CALL    C6350			; evaluate file expression and check if disk drive
		CALL    C665E			; get BASIC character
		RET     NZ
		PUSH    HL
		CALL    C59D1			; execute find first entry (normal attributes)
		LD      C,4DH			; function = delete file or subdirectory
		JR      J632E

; Subroutine rename file (H_NAME)
C6315:		CALL    C6888			; take control from hook caller
		CALL    C6350			; evaluate file expression and check if disk drive
		PUSH    HL
		CALL    C59D1			; execute find first entry (normal attributes)
		POP     HL
		CALL    C6654			; check for BASIC character
		DEFB    "A"
		CALL    C6654			; check for BASIC character
		DEFB    "S"
		CALL    C6350			; evaluate file expression and check if disk drive
		PUSH    HL
		LD      C,4EH			; function = rename file or subdirectory
J632E:		PUSH    BC
		LD      DE,BUF+10
		LD      HL,(PATHNAM)
		CALL    C655D			; execute BDOS function (handle error)
		CALL    C59D8
		POP     BC
		JR      NC,J632E
		CALL    C6343			; flush disk buffers
		POP     HL
		RET

; Subroutine flush disk buffers
C6343:		LD      B,0FFH			; drive = all
		LD      D,0			; flush only
		LD      C,5FH			; function = flush disk buffers
		JP      C655D			; execute BDOS function (handle error)

; Subroutine check for "(", evaluate file expression and check if disk drive
C634C:		CALL    C6654			; check for BASIC character
		DEFB    "("

; Subroutine evaluate file expression and check if disk drive
C6350:		LD      IX,FILEVL
		CALL    C664F
		LD      A,D
		CP      09H			; device = disk drive ?
		RET     C			; yep, quit
		JP      J65ED			; bad drive name error

; Subroutine (H_LOF)
C635E:		LD      A,2
		DEFB    011H

; Subroutine (H_LOC)
C6361:		LD      A,1
		CALL    C6888			; take control from hook caller
		PUSH    AF			; store function
		LD      IX,CONINT
		CALL    C664F
		LD      IX,FILIDX
		CALL    C664F
		JP      C,J662D			; illegal function call error
		JP      Z,J6645			; file not open error
		INC     HL
		LD      B,(HL)			; file handle

		;DOS201 fixes
		INC     HL
		LD      C,(HL)			; record size -1
		LD      A,1			; relative to the current position
		LD      DE,0
		LD      HL,0			; offset = 0
		PUSH    BC			; store file handle, record size -1
		LD      C,4AH			; function = move file handle pointer
		CALL    C655D			; execute BDOS function (handle error)
		POP     BC			; restore file handle, record size -1
		POP     AF			; restore function
		DEC     A			; LOC ?
		JR      NZ,J6399		; nope, return file size
		PUSH    BC			; store file handle, record size -1
		CALL    C63E4			; calculate LOC value
		POP     BC			; restore file handle, record size -1
		JR      J63B7			; return LOC value

; Subroutine 
J6399:		PUSH    HL
		PUSH    DE			; store current position
		LD      A,2			; relative to the end of the file
		LD      DE,0
		LD      HL,0			; offset = 0
		LD      C,4AH			; function = move file handle pointer
		PUSH    BC			; store file handle
		CALL    C655D			; execute BDOS function (handle error)
		POP     BC			; restore file handle
		POP     IX
		EX      (SP),HL
		PUSH    DE
		PUSH    IX
		POP     DE			; store file size, restore orginal position
		XOR     A			; relative to start of the file
		CALL    C655D			; execute BDOS function (handle error)
		POP     DE
		POP     HL			; restore file size
J63B7:		LD      C,L
		LD      B,H
		EX      DE,HL			; value to HLBC

; Subroutine 
C63BA:		PUSH    BC			; store low word value
		LD      IX,FLTLIN
		CALL    C664F			; convert to single float (high word value)
		LD      BC,06545H
		LD      DE,06053H
		CALL    SGNMUL			; *65536
		LD      HL,DAC
		LD      DE,ARG
		LD      BC,8
		LDIR				; ARG = DAC
		POP     HL			; restore low word value
		LD      IX,FLTLIN
		CALL    C664F			; convert to single float (low word value)
		CALL    CONDS			; convert to double float
		JP      DECADD			; DAC = DAC + ARG

; Subroutine calculate LOC value
C63E4:		INC     C			; record size = 256 ?
		JR      NZ,J63EF		; nope, divide
		LD      A,L
		LD      L,H
		LD      H,E
		LD      E,D
		LD      D,0
		JR      J6403

J63EF:		XOR     A
		LD      B,32+1
J63F2:		ADC     A,A
		JR      C,J63F9
		CP      C
		CCF
		JR      NC,J63FB
J63F9:		SUB     C
		SCF
J63FB:		ADC     HL,HL
		EX      DE,HL
		ADC     HL,HL
		EX      DE,HL
		DJNZ    J63F2
J6403:		OR      A
		RET     Z
		INC     L
		RET     NZ
		INC     H
		RET     NZ
		INC     E
		RET     NZ
		INC     D
		RET

; Subroutine disk free (H_DSKF)
C640D:		CALL    C6888			; take control from hook caller
		LD      IX,CONINT
		CALL    C664F
		AND     A
		CALL    NZ,C5846
		LD      E,A
		LD      C,1BH			; function = get allocation information
		CALL    BDOS
		JP      MAKINT

; Subroutine copy file (H_COPY)
C6424:		CALL    C6888			; take control from hook caller
		CALL    C6350			; evaluate file expression and check if disk drive
		PUSH    HL
		CALL    C59D1			; execute find first entry (normal attributes)
		POP     HL
		CALL    C665E			; get BASIC character
		LD      A,00H
		PUSH    HL
		LD      HL,(PATHNAM)
		LD      (HL),A
		POP     HL
		JR      Z,J6447
		CALL    C6654			; check for BASIC character
		DEFB    0D9H			; TO token
		CALL    C6350			; evaluate file expression and check if disk drive
		CALL    C665E			; get BASIC character
		RET     NZ
J6447:		PUSH    HL
J6448:		CALL    CKCNTC
		LD      DE,BUF+10
		XOR     A
		LD      C,43H			; function = open file handle
		CALL    C655D			; execute BDOS function (handle error)
		LD      A,B
		LD      (BUF+138),A
		XOR     A
		LD      C,56H			; function = get/set file handle date and time
		CALL    C655D			; execute BDOS function (handle error)
		LD      (BUF+140),DE
		LD      (BUF+142),HL
		LD      A,0FFH
		LD      (BUF+139),A
		LD      HL,I6532
		LD      (BREAKV),HL		; install BDOS abort handler
		CALL    C6518			; get work area
		LD      E,C
		LD      D,B
J6475:		PUSH    HL
		PUSH    DE
J6477:		LD      A,L
		OR      H
		JR      Z,J6498
		PUSH    HL
		PUSH    DE
		LD      A,(BUF+138)
		LD      B,A
		LD      C,48H			; function = read from file handle
		CALL    BDOS
		JR      Z,J648A
		CP      0C7H			; end of file error ?
J648A:		JP      NZ,J6538
		EX      DE,HL
		POP     HL
		ADD     HL,DE
		EX      (SP),HL
		SBC     HL,DE
		LD      A,E
		OR      D
		POP     DE
		JR      NZ,J6477
J6498:		EX      DE,HL
		POP     DE
		PUSH    DE
		SBC     HL,DE
		LD      A,(BUF+139)
		INC     A
		JR      NZ,J64CC
		PUSH    HL
		LD      HL,BUF+11
		LD      DE,BUF+75
		LD      BC,13
		LDIR
		LD      DE,(PATHNAM)
		LD      B,00H
		LD      IX,BUF+74
		LD      C,42H			; function = find new entry
		CALL    C6534
		LD      DE,BUF+74
		XOR     A
		LD      C,43H			; function = open file handle
		CALL    C6534
		LD      A,B
		LD      (BUF+139),A
		POP     HL
J64CC:		POP     DE
		LD      A,(BUF+139)
		LD      B,A
		PUSH    DE
		LD      C,49H			; function = write to file handle
		CALL    C6534
		POP     DE
		POP     BC
		SBC     HL,BC
		LD      L,C
		LD      H,B
		JR      NC,J6475
		LD      A,(BUF+139)
		LD      B,A
		LD      A,01H   ; 1
		LD      IX,(BUF+140)
		LD      HL,(BUF+142)
		LD      C,56H			; function = get/set file handle date and time
		CALL    C6534
		LD      A,(BUF+138)
		LD      B,A
		LD      C,45H			; function = close file handle
		CALL    C6534
		LD      A,(BUF+139)
		LD      B,A
		LD      C,45H			; function = close file handle
		CALL    C6534
		LD      HL,I6568		; handle orginal BDOS error, restart DiskBASIC when none
		LD      (BREAKV),HL		; install BDOS abort handler
		CALL    C59D8
		JP      NC,J6448
		POP     HL
		RET

; Subroutine store VRAM start address and number of bytes, get work area
C6511:		LD      (SAVENT),HL		; store number of bytes
		LD      (SAVEND),BC		; store VRAM start address

; Subroutine get work area
C6518:		LD      HL,-512
		ADD     HL,SP
		JR      NC,J652A
		LD      BC,(STREND)
		AND     A
		SBC     HL,BC
		JR      C,J652A
		LD      A,H
		AND     A
		RET     NZ
J652A:		LD      BC,(NULBUF)
		LD      HL,256
		RET

I6532:		DEFW    C656A

; Subroutine 
C6534:		CALL    BDOS			; execute BDOS function
		RET     Z			; no error, quit
J6538:		PUSH    AF
		LD      HL,I6568		; handle orginal BDOS error, restart DiskBASIC when none
		LD      (BREAKV),HL		; install BDOS abort handler
		LD      A,(BUF+138)
		LD      B,A
		LD      C,45H			; function = close file handle
		CALL    BDOS
		LD      A,(BUF+139)
		LD      B,A
		INC     A
		LD      C,45H			; function = close file handle
		CALL    NZ,BDOS
		POP     AF
		JR      J65C3			; handle error

; Subroutine execute BDOS function with ASCIIZ string in buffer (handle error), result FIB in BUF
J6555:		LD      IX,BUF+10

; Subroutine execute BDOS function with ASCIIZ string in buffer (handle error)
C6559:		LD      DE,(PATHNAM)

; Subroutine execute BDOS function (handle error)
C655D:		CALL    BDOS
		RET     Z
		JR      J65C3			; handle error

; BDOS disk error handler: on BDOS disk error, warm start
I6563:		DEFW	I6565

I6565:		LD      C,2			; request warm start
		RET

; BDOS abort handler: handle orginal BDOS error, restart DiskBASIC when none
I6568:		DEFW	I6571

; Subroutine get orginal error code
C656A:		CP      9DH			; disk operation aborted ?
		JR      NZ,J656F		; nope,
		LD      A,B			; orginal error code
J656F:		OR      A
		RET

; Subroutine handle orginal BDOS error, restart DiskBASIC when none
I6571:		CALL    C656A			; get orginal error code
		JR      NZ,J65C3		; error, handle error
		LD      IX,READYR
		JP      C664F			; restart diskBASIC

I657D:		DEFB	0BAH
		DEFB	03EH			; .NRAMD -> Bad drive name
		DEFB	04BH			; .RAMDX -> RAM disk already exists
		DEFB	0BDH
		DEFB	0BEH
		DEFB	0BFH
		DEFB	0C0H
		DEFB	0C1H
		DEFB	0C2H
		DEFB	0C3H
		DEFB	0C4H
		DEFB	0C5H
		DEFB	0C6H
		DEFB	037H			; .EOF -> Input past end
		DEFB	03CH			; .FILE -> Bad allocation table
		DEFB	0C9H
		DEFB	040H			; .FOPEN -> File still open
		DEFB	041H			; .FILEX -> File already exists
		DEFB	049H			; .DIRX -> Directory already exists
		DEFB	041H			; .SYSX -> File already exists
		DEFB	038H			; .DOT -> Bad file name
		DEFB	0CFH
		DEFB	041H			; .DIRNE -> File already exists
		DEFB	048H			; .FILRO -> File write protected
		DEFB	0D2H
		DEFB	041H			; .DUPF -> File already exists
		DEFB	042H			; .DKFUL -> Disk full
		DEFB	043H			; .DRFUL -> Too many files
		DEFB	04AH			; .NODIR -> Directory not found
		DEFB	035H			; .NOFIL -> File not found
		DEFB	038H			; .PLONG -> Bad filename
		DEFB	038H			; .IPATH -> Bad filename
		DEFB	038H			; .IFNM -> Bad filename
		DEFB	03EH			; .IDRV -> Bad drivename
		DEFB	0DCH
		DEFB	0DDH
		DEFB	0DEH
		DEFB	0DFH
		DEFB	0E0H
		DEFB	0E1H
		DEFB	0E2H
		DEFB	0E3H
		DEFB	0E4H
		DEFB	0E5H
		DEFB	0E6H
		DEFB	0E7H
		DEFB	0E8H
		DEFB	0E9H
		DEFB	0EAH
		DEFB	0EBH
		DEFB	0ECH
		DEFB	0EDH
		DEFB	0EEH
		DEFB	0EFH
		DEFB	0F0H
		DEFB	0F1H
		DEFB	03CH			; .IFAT -> Bad allocation table
		DEFB	045H			; .SEEK -> Disk I/O error
		DEFB	045H			; .WFILE -> Disk I/O error
		DEFB	045H			; .WDISK -> Disk I/O error
		DEFB	045H			; .NDOS -> Disk I/O error
		DEFB	045H			; .UFORM -> Disk I/O error
		DEFB	044H			; .WPROT -> Disk write protected
		DEFB	045H			; .RNF -> Disk I/O error
		DEFB	045H			; .DATA -> Disk I/O error
		DEFB	045H			; .VERFY -> Disk I/O error
		DEFB	046H			; .NRDY -> Disk offline
		DEFB	045H			; .DISK -> Disk I/O error
		DEFB	045H			; .WRERR -> Disk I/O error
		DEFB	045H			; .NCOMP -> Disk I/O error

J65C3:		PUSH    AF			; store error code
		CALL    C6343			; flush disk buffers
		POP     AF			; restore error code
		CP      9FH			; CTRL-STOP pressed ?
		JR      Z,J65CE			; yep,
		CP      9EH			; CTRL-C pressed ?
J65CE:		LD      IX,READYR
		JR      Z,C664F			; yep, restart diskBASIC
		LD      E,A			; store error code
J65D5:		CP      0BAH			; DOS2 error code which need translation to BASIC error code ?
		JR      C,J65E6			; nope,
		LD      C,A
		LD      B,0
		LD      HL,I657D-0BAH
		ADD     HL,BC
		LD      A,(HL)			; translated BASIC error code
		LD      E,A			; store error code
		CP      3CH			; need to close i/o channels ?
		JR      C,J6623			; nope, skip
J65E6:		DEFB    001H

		LD      E,03CH
		DEFB    001H

J65EA:		LD      E,03DH
		DEFB    001H

J65ED:		LD      E,03EH
		DEFB    001H

		LD      E,03FH
		DEFB    001H

		LD      E,040H
		DEFB    001H

		LD      E,041H
		DEFB    001H

		LD      E,042H
		DEFB    001H

		LD      E,043H
		DEFB    001H

		LD      E,044H
		DEFB    001H

		LD      E,045H
		DEFB    001H

		LD      E,046H
		DEFB    001H

		LD      E,047H
		DEFB    001H

		LD      E,048H
		DEFB    001H

		LD      E,049H
		DEFB    001H

		LD      E,04AH
		DEFB    001H

		LD      E,04BH
		XOR     A
		LD      (NLONLY),A              ; not loading basic program, close i/o channels when requested
		PUSH    DE			; store BASIC error code
		LD      IX,CLSFIL
		CALL    C664F			; close i/o channel
		POP     DE			; restore BASIC error code
J6623:		DEFB    001H

J6624:		LD      E,16
		DEFB    001H

J6627:		LD      E,13
		DEFB    001H

J662A:		LD      E,7
		DEFB    001H

J662D:		LD      E,5
		DEFB    001H

J6630:		LD      E,2
		DEFB    001H

J6633:		LD      E,032H
		DEFB    001H

		LD      E,034H
		DEFB    001H

		LD      E,035H
		DEFB    001H

J663C:		LD      E,036H
		DEFB    001H

		LD      E,037H
		DEFB    001H

J6642:		LD      E,038H
		DEFB    001H

J6645:		LD      E,03BH
		XOR     A
		LD      (FLBMEM),A		; i/o channel mode = ascii
		LD      IX,ERROR

; Subroutine execute BASIC routine
C664F:		CALL    CALBAS
		EI
		RET

; Subroutine check for BASIC character
C6654:		CALL    C665E			; get BASIC character
		EX      (SP),HL
		CP      (HL)
		JR      NZ,J6630
		INC     HL
		EX      (SP),HL
		INC     HL

; Subroutine get BASIC character
C665E:		DEC     HL

; Subroutine get next BASIC character
C665F:		LD      IX,CHRGTR
		JR      C664F

; Subroutine parse device (H_PARD)
C6665:		EI
		LD      A,':'
		CP      (HL)
		JR      Z,J6642			; bad file name error
		PUSH    HL
		PUSH    DE
		LD      A,E
		CP      40H
		JR      NC,J6642		; bad file name error
		LD      C,E
		LD      B,0
		LD      DE,(PATHNAM)
		PUSH    BC
		PUSH    DE
		LDIR
		XOR     A
		LD      (DE),A
		POP     HL
		POP     BC
		CPIR
		JR      Z,J6642			; bad file name error
		LD      C,5BH			; function = parse pathname
		CALL    C6559			; execute BDOS function with ASCIIZ string in buffer (handle error)
		LD      A,(DE)
		CP      ':'
		JR      NZ,J6692
		POP     DE
		POP     HL
		RET

J6692:		BIT     2,B
		JR      NZ,J6698
		LD      C,00H
J6698:		LD      A,B
		AND     0C2H
		JR      Z,J66A7
		LD      A,(DE)
		OR      A
		JR      NZ,J6642		; bad file name error
		POP     DE
		LD      E,A
		PUSH    DE
		PUSH    BC
		JR      J66EC

J66A7:		POP     DE
		POP     HL
		LD      IX,(PATHNAM)
		BIT     2,B
		JR      Z,J66B9
		INC     HL
		INC     HL
		DEC     E
		DEC     E
		INC     IX
		INC     IX
J66B9:		PUSH    HL
		PUSH    DE
		PUSH    BC
		INC     E
		DEC     E
		JR      Z,J66EC
		LD      C,E
		LD      A,(HL)
		CP      ' '
		JP      Z,J6642			; bad file name error
		LD      B,08H
		CALL    C6729
		JR      Z,J66E9
		BIT     1,D
		JR      Z,J66D4
		DEC     IX
J66D4:		LD      A,2EH   ; "."
		LD      (IX),A
		INC     IX
		CP      (HL)
		JR      NZ,J66E2
		INC     HL
		DEC     C
		JR      Z,J66E9
J66E2:		LD      B,C
		CALL    C6729
		JP      NZ,J6642		; bad file name error
J66E9:		LD      (IX),C
J66EC:		LD      A,(MASTER)
		ADD     A,A
		LD      HL,12
		JR      NC,J66F7
		LD      L,12+8
J66F7:		ADD     HL,SP
		LD      (HL),RETRTN % 256	; rem: LOW RETRTN
		INC     HL
		LD      (HL),RETRTN / 256	; rem: HIGH RETRTN
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		LD      E,(HL)
		INC     HL
		LD      D,(HL)
		PUSH    HL
		LD      HL,BSAVE+3
		RST    	R_DCOMPR
		LD      BC,(BSVCHK)
		JR      Z,J671C
		LD      HL,BLOAD+3
		RST    	R_DCOMPR
		LD      BC,(BLDCHK)
		JR      Z,J671C
		LD      C,E
		LD      B,D
J671C:		POP     HL
		LD      (HL),B
		DEC     HL
		LD      (HL),C
		POP     BC
		LD      A,C
		OR      A
		CALL    NZ,C5846
		POP     DE
		POP     HL
		RET

; Subroutine
C6729:		LD      D,01H
J672B:		PUSH    HL
		PUSH    BC
		LD      E,(HL)
		LD      C,5DH			; function = check character
		CALL    C655D			; execute BDOS function (handle error)
		POP     BC
		POP     HL
		LD      A,E
		BIT     4,D
		JR      NZ,J6746
		LD      (IX),A
		INC     IX
		INC     HL
		DEC     C
		RET     Z
		DJNZ    J672B
		RET

J6745:		LD      A,(HL)
J6746:		CP      20H     ; " "
		RET     NZ
		INC     HL
		DEC     C
		RET     Z
		DJNZ    J6745
		RET

; Subroutine no device (H_NODE)
C674F:		EI
		LD      A,0			; device = default drive
		RET

; Subroutine get pointer to error string (H_ERRP)
C6753:		EI
		LD      A,E
		SUB     3CH
		RET     C
		CP      10H
		RET     NC
		INC     A
		LD      B,A
		LD      HL,I6775
J6760:		LD      A,(HL)
		AND     A
		INC     HL
		JR      NZ,J6760
		DJNZ    J6760
		DEC     HL
		LD      DE,BUF+10
		PUSH    DE
		LD      BC,26
		LDIR
		LD      E,01H
		POP     HL
		RET

I6775:		DEFB	0
		DEFB	"Bad FAT",0
		DEFB	"Bad file mode",0
		DEFB	"Bad drive name",0
		DEFB	"Bad sector number",0
		DEFB	"File still open",0
		DEFB	"File already exists",0
		DEFB	"Disk full",0
		DEFB	"Too many files",0
		DEFB	"Disk write protected",0
		DEFB	"Disk I/O error",0
		DEFB	"Disk offline",0
		DEFB	"Rename across disk",0
		DEFB	"File write protected",0
		DEFB	"Directory already exists",0
		DEFB	"Directory not found",0
		DEFB	"RAM disk already exists",0

; Subroutine take control from hook caller
C6888:		EI
		PUSH    HL
		PUSH    AF
		LD      A,(MASTER)
J688E:		ADD     A,A
		LD      HL,12
		JR      NC,J6896
		LD      L,12+8
J6896:		ADD     HL,SP
		LD      (HL),RETRTN % 256	; rem: LOW RETRTN
		INC     HL
		LD      (HL),RETRTN / 256	; rem: HIGH RETRTN
		POP     AF
		POP     HL
		RET

; Subroutine 
C689F:		LD      HL,0

; Subroutine 
C68A2:		LD      A,16+1
J68A4:		RR      B
		RR      C
		DEC     A
		RET     Z
J68AA:		JR      NC,J68AD
		ADD     HL,DE
J68AD:		RR      H
J68AF:		RR      L
		JR      J68A4

; Subroutine prepare for MSXDOS, try to start MSXDOS2
C68B3:		LD      B,A			; store boot drive id
		LD      A,(CUR_DRV)		; current drive
		PUSH    AF			; store default drive
		PUSH    BC			; store boot drive
		PUSH    HL			; store pointer to command line
		LD      HL,(HIMSAV)
		LD      (DOSHIM),HL		; register top of MSXDOS memory
		DI
		LD      A,(P0_64K)		; page 0 segment disk system
		CALL    PUT_P0
		LD      A,(P1_64K)		; page 1 segment disk system
		CALL    PUT_P1
		LD      A,(P2_64K)		; page 2 segment disk system
		CALL    PUT_P2
		LD      A,(RAMAD3)
		LD      H,80H			; rem: HIGH 8000H
		CALL    ENASLT			; switch page 2 to disk system RAM slot
		CALL    P0_RAM			; switch page 0 to disk system RAM slot
		LD      HL,WBOOT
J68E1:		LD      (HL),H
		INC     L
		JR      NZ,J68E1		; clear CP/M low storage page
		LD      HL,I6930		; table with MSXDOS jump entry points
J68E8:		LD      E,(HL)
		INC     HL
		LD      D,(HL)
		INC     HL
		LD      A,D
		OR      E			; end of table ?
		JR      Z,J68FA			; yep,
		LD      A,0C3H
		LD      (DE),A
		INC     DE
		LDI
		LDI
		JR      J68E8

J68FA:		CALL    GET_P2
		EX      AF,AF'			; store current segment page 2
		LD      A,(CODE_S)		; BDOS code segment
		CALL    PUT_P2
		LD      HL,SSLOT+8000H
		LD      DE,SSLOT
		LD      BC,26
		LDIR    			; initialize secondary slot helper subroutines from BDOS code segment
		EX      AF,AF'			; restore current segment page 2
		CALL    PUT_P2
		EI
		POP     HL			; restore pointer to command line
		LD      DE,DBUF+1
		LD      B,-1
J691A:		LD      A,(HL)
		LD      (DE),A
		INC     HL
		INC     DE
		INC     B
		OR      A
		JR      NZ,J691A		; copy command line to DBUF
		LD      A,B
		LD      (DBUF+0),A		; update command line size
		POP     AF			; restore boot drive
		OR      A
		CALL    NZ,C699B		; boot drive specified, try to start MSXDOS2
		POP     AF			; restore current drive
		CALL    C699B			; try to start MSXDOS2
		RET

I6930:		DEFW	RDSLT,SRDSLT
		DEFW	WRSLT,SWRSLT
		DEFW	CALSLT,SCALSLT
		DEFW	ENASLT,SENASLT
		DEFW	CALLF,SCALLF
		DEFW	KEYINT,SIRQ
		DEFW	0

; Subroutine get valid boot loader
C694A:		LD      HL,I6A02		; on BDOS disk error warm boot (start DiskBASIC)
		LD      (DISKVE),HL		; install BDOS disk error handler
		LD      HL,I6A04		; ignore BDOS abort
		LD      (BREAKV),HL		; install BDOS abort handler
		LD      DE,(SSECBUF)
		LD      C,1AH			; function = set disk transfer address
		CALL    C69F5			; execute BDOS function (return orginal error code when aborted)
		LD      C,1			; drive id = 1
		LD      DE,DRVTBL
J6964:		PUSH    BC			; store drive id
		PUSH    DE			; store pointer in DRVTBL
		LD      L,C
		DEC     L			; to drive id
		LD      H,1			; number of sectors = 1
		LD      DE,0			; sector number = 0
		LD      C,2FH			; function = absolute sector read
		CALL    C69F5			; execute BDOS function (return orginal error code when aborted)
		POP     DE			; restore pointer in DRVTBL
		POP     BC			; restore drive id
		JR      NZ,J6980		; error, no valid boot loader
		LD      HL,(SSECBUF)
		LD      A,(HL)
		OR      02H
		CP      0EBH			; x86 JMP instruction ?
		JR      Z,J698A			; yep, update default drive and copy boot loader
J6980:		LD      A,(DE)
		ADD     A,C
		LD      C,A			; update drive id
		INC     DE
		INC     DE
		LD      A,(DE)
		AND     A			; more disk interfaces ?
		JR      NZ,J6964		; yep, next disk interface
		RET

J698A:
	IFDEF FAT16
		; add additional test for extended boot signature: FAT16 / MS-DOS boot sector
		; if it exists then there is no valid MSX bootloader
		LD	A,C			; save drive id
		LD	BC,0026H
		ADD	HL,BC
		LD	C,A			; restore drive id
		LD	A,(HL)
		AND	0FEH			; EBS can be 28H or 29H
		CP	28H			; EBS?
		JR	Z,J6980			; Z=yes
	ENDIF
		LD      A,C
		LD      (CUR_DRV),A		; update current drive
		LD      HL,(SSECBUF)
		LD      DE,BOT16K
		LD      BC,256
		LDIR
		OR      A
		RET

; Subroutine try to start MSXDOS2
C699B:		LD      (CUR_DRV),A		; update current drive
		LD      HL,I6A02		; on BDOS disk error warm boot (start DiskBASIC)
		LD      (DISKVE),HL		; install BDOS disk error handler
		LD      HL,I6A04		; ignore BDOS abort
		LD      (BREAKV),HL		; install BDOS abort handler
		LD      B,0			; logical drive = current drive
		LD      D,B			; physical drive = current drive
		LD      C,6AH			; function = logical drive assignment
		CALL    C69F5			; execute BDOS function (return orginal error code when aborted)
		LD      DE,I69E8		; rem: drive/path/file ASCIIZ string
		XOR     A			; open mode = normal
		LD      C,43H			; function = open file handle
		CALL    C69F5			; execute BDOS function (return orginal error code when aborted)
		JR      NZ,J69E7		; error, quit (optimize: a simple RET NZ)
		LD      DE,TBASE
		LD      HL,4000H-TBASE
		LD      C,48H			; function = read from file handle
		PUSH    BC			; store file handle
		CALL    C69F5			; execute BDOS function (return orginal error code when aborted)
		POP     BC			; restore file handle
		PUSH    AF			; store read from file result
		LD      C,45H			; function = close file handle
		CALL    C69F5			; execute BDOS function (return orginal error code when aborted)
		POP     AF			; restore read from file result
		JR      NZ,J69E7		; error, quit (optimize: a simple RET NZ)
		LD      A,(CUR_DRV)		; current drive
		LD      (BOOT_D),A		; update boot drive
		LD      A,0C3H
		LD      (SDOSON+0),A
		LD      (SDOSOF+0),A
		LD      (XFER+0),A		; enable page 1 support
		JP      TBASE			; start MSXDOS2

J69E7:		RET

I69E8:		DEFB	"\\MSXDOS2.SYS"
		DEFB	0

; Subroutine execute BDOS function (return orginal error code when aborted)
C69F5:		CALL    BDOS
		CP      9DH			; disk operation aborted ?
		JR      NZ,J69FD		; nope,
		LD      A,B			; orginal error code
J69FD:		OR      A
		RET

I69FF:		LD      C,2			; requested action = warm start
I6A01:		RET

; BDOS disk error handler: on BDOS disk error, warm start
I6A02:		DEFW	I69FF			; pointer to disk error handler (warm start)

; BDOS abort handler: ignore abort
I6A04:		DEFW	I6A01			; pointer to abort handler (ignore abort request)

; Subroutine select system segments
C6A06:		LD      A,(P0_64K)		; page 0 segment disk system
		CALL    PUT_P0
		LD      A,(P1_64K)		; page 1 segment disk system
		CALL    PUT_P1
		LD      A,(P2_64K)		; page 2 segment disk system
		CALL    PUT_P2
		RET

; Subroutine BDOS abort handler
I6A19:		EXX
		LD      B,A
		EXX
		LD      SP,(SP_BDOS)
		LD      HL,(BREAKV)		; abort BDOS handler
		JP      J6A3C

; Subroutine BDOS disk error handler
I6A26:		EX      AF,AF'
		LD      L,C
		LD      C,A
		LD      A,B
		DEC     A
		LD      B,L
		LD      HL,(DISKVE)		; BDOS disk error handler
		CALL    C6A37
		LD      A,03H   ; 3
		SUB     C
		EI
		RET

; Subroutine 
C6A37:		PUSH    HL
		LD      HL,SDOSON
		EX      (SP),HL
J6A3C:		PUSH    HL
		LD      HL,JP_VEC
		EX      (SP),HL
		JP      SDOSOF

; Subroutine 
C6A44:		DI
		LD      A,1
		LD      (ST_COU),A
		LD      HL,TIM_CO
		INC     (HL)
		LD      A,(TIM_RA)
		CP      (HL)
		JR      NZ,J6A6A
		LD      (HL),00H
		LD      A,(CH_COU)
		CP      02H
		ADC     A,0FFH
		LD      (CH_COU),A
		LD      A,(TIM_TI)
		CP      07H
		ADC     A,00H
		LD      (TIM_TI),A
J6A6A:		LD      HL,(RANDOM+0)
		LD      A,(RANDOM+2)
		LD      C,A
		RRCA
		RRCA
		RRCA
		XOR     C
		RLA
		RLA
		ADC     HL,HL
		LD      A,C
		ADC     A,A
		LD      (RANDOM+2),A
		LD      (RANDOM+0),HL
		RET

I6A82:		LD      IX,(IX_BDOS)

J6A86:		EX      AF,AF'
		EXX     			; alternative register set
		LD      HL,I6A19		; BDOS abort handler
		LD      (KAB_VE),HL		; install DOS2 abort handler
		LD      HL,I6A26		; BDOS disk error handler
		LD      (KDSK_V),HL		; install DOS2 disk error handler
		LD      HL,(ST_BDOS)
		OR      A
		SBC     HL,SP			; using the BDOS stack ?
		JR      C,J6AA3			; nope,
		LD      BC,300
		SBC     HL,BC			; using the BDOS stack ?
		JR      C,J6AB4			; yep,
J6AA3:		LD      (SP_BDOS),SP		; store stack pointer
		LD      SP,(ST_BDOS)		; switch to BDOS stack pointer
		CALL    C6AB9			; execute BDOS function
		LD      SP,(SP_BDOS)		; restore stack pointer
		OR      A			; update Zx flag
		RET

J6AB4:		CALL    C6AB9			; execute BDOS function
		OR      A			; update Zx flag
		RET

; Subroutine execute BDOS function
C6AB9:		EXX     			; normal register set
		PUSH    HL
		LD      A,C
		CP      71H			; valid BDOS function ?
		JR      C,J6AC2			; yep,
		LD      A,1CH			; nope, use unused BDOS function
J6AC2:		LD      HL,I6AD2
		ADD     A,A
		ADD     A,L
		LD      L,A
		JR      NC,J6ACB
		INC     H
J6ACB:		LD      A,(HL)
		INC     HL
		LD      H,(HL)
		LD      L,A
		EX      (SP),HL
		EX      AF,AF'			; normal register set
		RET

; BDOS function jump table
I6AD2:		DEFW	C6C98,C6BB4,C6BB4,C6BB4,C6BB4,C6BB4,C6BB4,C6BB4
		DEFW	C6BB4,C6BBA,C6BCA,C6BB4,C6BB4,C6BB4,C6BB4,C6BEE
		DEFW	C6BEE,C6BEE,C6BB4,C6BEE,C6BEE,C6BEE,C6BEE,C6BEE
		DEFW	C6BB4,C6BB4,C6BB4,C6BE6,C6C98,C6C98,C6C98,C6C98
		DEFW	C6C98,C6BF1,C6BF1,C6BF1,C6BF1,C6C98,C6C12,C6C12
		DEFW	C6BF1,C6C98,C6C2D,C6C2D,C6C2D,C6C2D,C6BB4,C6C98
		DEFW	C6C98,C6C32,C6C98,C6C98,C6C98,C6C98,C6C98,C6C98
		DEFW	C6C98,C6C98,C6C98,C6C98,C6C98,C6C98,C6C98,C6C98
		DEFW	C6C44,C6C4B,C6C44,C6C6D,C6C6D,C6C98,C6C98,C6C98
		DEFW	C6C98,C6C98,C6C98,C6C98,C6C6D,C6C6D,C6C67,C6C67
		DEFW	C6C6D,C6C6D,C6C98,C6C88,C6C88,C6C98,C6C98,C6C98
		DEFW	C6C98,C6CA0,C6CE3,C6C9B,C6CC0,C6C98,C6CA0,C6C98
		DEFW	C6C98,C6C98,C6C98,C6C98,C6C98,C6C98,C6CE9,C6C98
		DEFW	C6C98,C6C98,C6C98,C6C98,C6C98,C6C98,C6C98,C6C98
		DEFW	C6C98

; Subroutine BDOS function CP/M style result
C6BB4:		CALL    C6C98			; simple BDOS function
		LD      A,L
		LD      B,H
		RET

; Subroutine BDOS function print string
C6BBA:		LD      A,(DE)
		INC     DE
		CP      '$'
		JR      Z,J6BE1
		PUSH    DE
		LD      E,A
		LD      C,2
		CALL    C6C98			; simple BDOS function
		POP     DE
		JR      C6BBA

; Subroutine BDOS function buffered console input
C6BCA:		PUSH    DE
		LD      A,(DE)
		LD      DE,(BUF_1)		; pointer to temporary buffer
		LD      (DE),A
		CALL    C6C98			; simple BDOS function
		POP     DE
		LD      A,(DE)
		INC     DE
		LD      HL,(BUF_1)		; pointer to temporary buffer
		INC     HL
		LD      C,A
		LD      B,00H
		INC     BC
		LDIR
J6BE1:		XOR     A
		LD      B,A
		LD      L,A
		LD      H,A
		RET

; Subroutine BDOS function
C6BE6:		CALL    C6C98			; simple BDOS function
		LD      A,C
		LD      BC,512
		RET

; Subroutine BDOS function
C6BEE:		LD      A,21H			; sequential i/o FCB size
		DEFB    021H

; Subroutine BDOS function
C6BF1:		LD      A,24H			; random i/o FCB size
		PUSH    DE
		EXX
		POP     HL
		LD      C,A
		LD      B,0

; Subroutine BDOS function with FCB parameter
C6BF9:		PUSH    HL
		PUSH    BC
		LD      DE,(BUF_1)		; pointer to temporary buffer
		PUSH    DE
		LDIR    			; copy FCB
		EXX
		POP     DE
		PUSH    DE
		CALL    C6C98			; simple BDOS function
		EXX
		POP     HL
		POP     BC
		POP     DE
		LDIR    			; copy FCB
		EXX
		LD      A,L
		LD      B,H
		RET

C6C12:		PUSH    DE
		EXX
		POP     HL
		PUSH    HL
		LD      BC,15
		ADD     HL,BC
		LD      C,24H			; CP/M random i/o FCB size
		LD      A,(HL)
		OR      A			; record size > 255 ?
		JR      NZ,J6C27		; yep,
		DEC     HL
		LD      A,(HL)
		CP      64			; record size < 64 ?
		JR      NC,J6C27		; nope,
		INC     C			; MSX random i/o FCB size
J6C27:		POP     HL
		CALL    C6BF9			; BDOS function with FCB parameter
		EX      DE,HL
		RET

C6C2D:		CALL    C6C98			; simple BDOS function
		LD      A,C
		RET

C6C32:		PUSH    DE
		LD      DE,(BUF_1)		; pointer to temporary buffer
		CALL    C6C98			; simple BDOS function
		EX      DE,HL
		POP     DE
		PUSH    DE
		LD      BC,32
		LDIR
		POP     DE
		RET

C6C44:		CALL    C6CFD			; copy FIB/ASCIIZ parameter to temporary buffer
		CALL    C,C6D20			; parameter is a FIB, copy ASCIIZ string to temporary buffer3
		RET     NZ
C6C4B:		PUSH    IX
		POP     HL
		PUSH    HL
		LD      DE,(BUF_2)		; pointer to temporary buffer2
		PUSH    BC
		LD      BC,64
		LDIR
		POP     BC
		CALL    C6C8D			; BDOS function with 3 pointer parameters
		POP     DE
		LD      HL,(BUF_2)		; pointer to temporary buffer2
		LD      BC,64
		LDIR
		RET

C6C67:		CALL    C6D20			; copy ASCIIZ string to temporary buffer3
		LD      HL,(BUF_3)		; pointer to temporary buffer3
C6C6D:		EX      AF,AF'
		PUSH    DE
		CALL    C6CFD			; copy FIB/ASCIIZ parameter to temporary buffer
		PUSH    AF			; store parameter type
		EX      AF,AF'
		CALL    C6C94			; BDOS function with pointer parameter
		EX      AF,AF'
		EXX
		POP     AF			; restore parameter type
		POP     DE
		JR      NC,J6C85		; parameter is a ASCIIZ string,
		LD      HL,(BUF_1)		; pointer to temporary buffer
		LD      BC,64
		LDIR
J6C85:		EX      AF,AF'
		EXX
		RET

; Subroutine BDOS function with 2 pointer parameters and ASCIIZ string parameter
C6C88:		EX      AF,AF'
		CALL    C6D20			; copy ASCIIZ string to temporary buffer3
		EX      AF,AF'

; Subroutine BDOS function with 3 pointer parameters
C6C8D:		LD      IX,(BUF_2)		; pointer to temporary buffer2
		LD      HL,(BUF_3)		; pointer to temporary buffer3

; Subroutine BDOS function with pointer parameter
C6C94:		LD      DE,(BUF_1)		; pointer to temporary buffer

; Subroutine simple BDOS function
C6C98:		JP      GO_BDOS

; Subroutine BDOS function
C6C9B:		PUSH    DE
		CALL    C6D01			; copy FIB parameter to temporary buffer
		POP     DE
C6CA0:		EX      DE,HL
		PUSH    HL
		LD      DE,(BUF_1)		; pointer to temporary buffer
		OR      A
		SBC     HL,DE
		PUSH    HL
		PUSH    DE
		CALL    C6C98			; simple BDOS function
		EXX
		POP     HL
		POP     BC
		POP     DE
		PUSH    BC
		LD      BC,64
		LDIR
		EXX
		EX      (SP),HL
		EX      DE,HL
		ADD     HL,DE
		EX      (SP),HL
		ADD     HL,DE
		POP     DE
		RET

; Subroutine BDOS function
C6CC0:		PUSH    HL
		PUSH    HL
		LD      L,E
		LD      H,D
		CALL    C6D01			; copy FIB parameter to temporary buffer
		LD      DE,(BUF_1)		; pointer to temporary buffer
		OR      A
		SBC     HL,DE
		EX      (SP),HL
		PUSH    HL
		CALL    C6C8D			; BDOS function with 3 pointer parameters
		EXX
		POP     DE
		LD      BC,11
		LD      HL,(BUF_3)		; pointer to temporary buffer3
		LDIR
		EXX
		POP     HL
		ADD     HL,DE
		EX      DE,HL
		POP     HL
		RET

; Subroutine BDOS function
C6CE3:		CALL    C6D01			; copy FIB parameter to temporary buffer
		JP      C6C94			; BDOS function with pointer parameter

; Subroutine BDOS function
C6CE9:		PUSH    DE
		LD      DE,(ERR_BUF)		; pointer to temporary buffer4
		CALL    C6C98			; simple BDOS function
		EX      DE,HL
		POP     DE
		PUSH    DE
		PUSH    BC
		LD      BC,64
		LDIR
		POP     BC
		POP     DE
		RET

; Subroutine copy FIB/ASCIIZ parameter to temporary buffer
C6CFD:		LD      A,(DE)
		INC     A			; parameter a FIB ?
		JR      NZ,J6D11		; nope, copy ASCIIZ string to temporary buffer

; Subroutine copy FIB parameter to temporary buffer
C6D01:		PUSH    HL
		PUSH    BC
		EX      DE,HL
		LD      DE,(BUF_1)		; pointer to temporary buffer
		LD      BC,64
		LDIR
		POP     BC
		POP     HL
		SCF     			; parameter is a FIB
		RET

; Subroutine copy ASCIIZ string to temporary buffer
J6D11:		PUSH    HL
		PUSH    BC
		EX      DE,HL
		LD      DE,(BUF_1)		; pointer to temporary buffer
		LD      B,100			; maximium size of ASCIIZ string = 100
		CALL    C6D2E			; copy ASCIIZ string
		POP     BC
		POP     HL
		RET

; Subroutine copy ASCIIZ string to temporary buffer3
C6D20:		PUSH    DE
		PUSH    BC
		LD      DE,(BUF_3)		; pointer to temporary buffer3
		LD      B,100			; maximium size of ASCIIZ string = 100
		CALL    C6D2E			; copy ASCIIZ string
		POP     BC
		POP     DE
		RET

; Subroutine copy ASCIIZ string
C6D2E:		LD      A,(HL)
		INC     HL
		LD      (DE),A
		INC     DE
		OR      A
		RET     Z
		DJNZ    C6D2E
		LD      A,0D8H
		RET

; ------------------------------------------------------------------------------
; *** RAMDISK driver ***
; ------------------------------------------------------------------------------

; Subroutine DSKCHG RAMDISK
RAMD_DSKCHG:	LD      HL,I_BC00+32
		LD      A,(DATA_S)		; BDOS data segment
		CALL    RD_SEG                  ; RD_SEG
		CP      'V'
		LD      B,0
		RET     Z
		DEC     B
		RET

; Subroutine DSKIO RAMDISK
RAMD_DSKIO:	EI
		LD      (RD_SNU),DE		; store sector number
		LD      (RD_ADDR),HL		; store transfer address
		LD      A,B
		LD      (RD_SCN),A		; store number of sectors
		EX      AF,AF'
		LD      HL,I_BC00+32
		LD      A,(DATA_S)		; BDOS data segment
		CALL    RD_SEG                  ; RD_SEG
		EI
		SUB     'V'
		CALL    NZ,C6E5B
		RET     C
		LD      HL,(SLTTBL+0)
		PUSH    HL
		LD      HL,(SLTTBL+2)
		PUSH    HL			; store SLTTBL
J6D6E:		LD      DE,(RD_SNU)
		CALL    C6DFF
		JR      C,J6DA6
		LD      E,A
		LD      A,(RD_SCN)
		SUB     E
		JR      NC,J6D81
		ADD     A,E
		LD      E,A
		XOR     A
J6D81:		LD      (RD_SCN),A
		OR      A
		PUSH    AF
		PUSH    HL
		LD      HL,(RD_SNU)
		LD      D,00H
		ADD     HL,DE
		LD      (RD_SNU),HL
		POP     HL
		LD      D,E
		SLA     D
		LD      E,00H
		PUSH    DE
		CALL    C6DCA
		POP     DE
		LD      HL,(RD_ADDR)
		ADD     HL,DE
		LD      (RD_ADDR),HL
		POP     AF
		JR      NZ,J6D6E
		XOR     A
J6DA6:		EX      AF,AF'
		DI
		POP     HL
		LD      (SLTTBL+2),HL
		POP     HL
		LD      (SLTTBL+0),HL		; restore SLTTBL
		LD      HL,SLTTBL+0
		XOR     A
J6DB4:		LD      C,A
		IN      A,(0A8H)
		LD      B,A
		AND     3FH     ; "?"
		OR      C
		LD      E,(HL)
		INC     HL
		CALL    SSLOTE
		LD      A,C
		ADD     A,40H   ; "@"
		JR      NZ,J6DB4
		EI
		EX      AF,AF'
		LD      B,00H
		RET

; Subroutine 
C6DCA:		DI
		LD      A,(RAMAD3)
		CP      B
		JR      Z,J6DE4
		CALL    GET_P1
		PUSH    AF			; store current segment page 1
		LD      A,C
		CALL    PUT_P1
		SET     6,H
		CALL    RD_LDI
		POP     AF
		CALL    PUT_P1
		EI
		RET

J6DE4:		CALL    GET_P0
		PUSH    AF			; store current segment page 0
		LD      A,C
		CALL    PUT_P0
		LD      B,D
		LD      C,E
		LD      DE,(RD_ADDR)
		EX      AF,AF'
		JR      NC,J6DF6
		EX      DE,HL
J6DF6:		EX      AF,AF'
		LDIR
		POP     AF
		CALL    PUT_P0
		EI
		RET

; Subroutine 
C6DFF:		LD      A,D
		OR      E			; sector number 0 ?
		JR      NZ,J6E11		; nope,
		LD      A,(DATA_S)
		LD      C,A			; BDOS data segment
		LD      A,(RAMAD3)
		LD      B,A
		LD      HL,I_BC00-8000H
		LD      A,1
		RET

J6E11:		CALL    GET_P2
		PUSH    AF			; store current segment page 2
		LD      A,(DATA_S)		; BDOS data segment
		CALL    PUT_P2
		LD      HL,(D_BE00)             ; number of ramdisk segments
		LD      H,00H
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		POP     AF
		CALL    PUT_P2
		SBC     HL,DE
		LD      A,0CH
		RET     C
		LD      H,D
		LD      L,E
		DEC     HL
		ADD     HL,HL
		LD      A,L
		PUSH    AF
		ADD     HL,HL
		ADD     HL,HL
		LD      E,H
		LD      D,0
		LD      HL,I_BE02               ; ramdisk segment table
		ADD     HL,DE
		ADD     HL,DE
		CALL    GET_P2
		PUSH    AF			; store current segment page 2
		LD      A,(DATA_S)		; BDOS data segment
		CALL    PUT_P2
		LD      C,(HL)
		INC     HL
		LD      B,(HL)
		POP     AF
		CALL    PUT_P2
		POP     AF
		AND     3EH
		LD      H,A
		LD      L,00H
		LD      A,40H
		SUB     H
		RRCA
		OR      A
		RET

; Subroutine 
C6E5B:		CALL    GET_P2
		PUSH    AF			; store current segment page 2
		LD      A,(DATA_S)		; BDOS data segment
		CALL    PUT_P2
		LD      A,(D_BE00)              ; number of ramdisk segments
		OR      A
		JR      NZ,J6E73
		POP     AF
		CALL    PUT_P2
		LD      A,0CH
		SCF
		RET

J6E73:		EXX
		LD      HL,I_BC00+11
		LD      (HL),00H
		INC     HL
		LD      (HL),02H
		INC     HL
		LD      (HL),01H
		CP      81H
		JR      C,J6E84
		INC     (HL)
J6E84:		INC     HL
		LD      (HL),01H
		INC     HL
		LD      (HL),00H
		INC     HL
		LD      (HL),02H
		INC     HL
		LD      C,A
		SRL     A
		SRL     A
		ADD     A,04H
		LD      E,A
		LD      D,00H
		PUSH    DE
		EX      DE,HL
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		EX      DE,HL
		LD      (HL),E
		INC     HL
		LD      (HL),D
		INC     HL
		LD      E,C
		LD      D,00H
		EX      DE,HL
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		ADD     HL,HL
		INC     HL
		EX      DE,HL
		LD      (HL),E
		INC     HL
		LD      (HL),D
		INC     HL
		LD      (HL),0FFH
		INC     HL
		EX      DE,HL
		LD      A,C
		DEC     HL
		POP     BC
		OR      A
		SBC     HL,BC
		CP      81H
		JR      C,J6EC4
		SRL     H
		RR      L
J6EC4:		LD      B,H
		LD      C,L
		ADD     HL,BC
		ADD     HL,BC
		EX      DE,HL
		DEC     DE
		SRL     D
		SRL     D
		INC     D
		LD      (HL),D
		INC     HL
		LD      (HL),00H
		INC     HL
		LD      HL,I6EE6
		LD      DE,I_BC00+32
		LD      BC,11
		LDIR
		EXX
		POP     AF
		CALL    PUT_P2
		XOR     A
		RET

I6EE6:		DEFB	"VOL_ID"

I6EEC:		DEFB	0
		DEFB	1
		DEFB	2
		DEFB	3
		DEFB	4

; Subroutine GETDPB RAMDISK
RAMD_GETDPB:	RET

; Subroutine CHOICE RAMDISK
RAMD_CHOICE:	LD      HL,I6EEC
		RET

; Subroutine DSKFMT RAMDISK
RAMD_DSKFMT:	LD      A,0CH
		SCF
		RET

; ------------------------------------------------------------------------------
; *** Bank S1 code starts here ***
; ------------------------------------------------------------------------------

; C4103/J410F - Install disksystem routines
J410F:		DI
		PUSH	AF			; save slot of page 2 for later
		CALL    C492F			; initialize memory mapper
		CALL    NC,PH_INIT		; initialize paging helper routines
		POP	HL
		RET     C			; c=error
		PUSH	HL
		CALL	C4E05			; get slot of page 1 (this ROM)
		LD	H,080H			; enable page 2
		CALL	ENASLT
		LD      A,1
		LD      (CUR_DRV),A              ; default drive = A:
		LD      A,(IDBYT0)
		RLCA
		SBC     A,A
		ADD     A,6
		LD      (TIM_RA),A
		LD      (RANDOM),A
		LD      HL,P0_64K
		LD      DE,P0_TPA
		LD      BC,4
		LDIR
		LD      A,(CODE_S)              ; BDOS code segment
		CALL    PUT_P0
		CALL    P0_RAM                  ; enable DOS memory on page 0
		LD	HL,K1_BEGIN		; Source
		LD	DE,0			; Target
		LD	BC,K1_END-K1_BEGIN	; Number of bytes
		LDIR
		POP	AF
		LD	H,080H			; restore page 2 slot (ie. RAM)
		CALL	ENASLT			; uses the ENASLT routine of the just loaded disk rom

; Mod: initialize RAM page 2 after rom copy
		LD      A,(DATA_S)              ; BDOS data segment
		CALL    PUT_P2
		CALL    C418C			; initialize characterset

I416E:		CALL    0                       ; initialize BDOS code

IFDEF FAT16
; DPB+1E init patch:
; change from '00,FF,00' to 'FF,FF,00'
		PUSH	AF
		LD      A,(DATA_S)              ; BDOS data segment
		CALL    PUT_P2
		LD	BC,0801h		; check 8 drives (B), counting from 1 (C)
DIRINT:	        PUSH	BC
		LD	B,0
		LD	HL,I_BA23		; page 2
		ADD	HL,BC
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	A,(HL)
		LD	D,A
		LD	HL,001Eh
		ADD	HL,DE
		XOR	A
		CP	(HL)			;(DPB+1Eh),00
		JR	NZ,DIRIN_
		INC	HL
		DEC	A
		CP	(HL)			;(DPB+1Fh),FFh
		JR	NZ,DIRIN_
		INC	HL
		INC	A
		CP	(HL)			;(DPB+20h),00h
		JR	NZ,DIRIN_
		DEC	HL
		DEC	HL
		DEC	A
		LD	(HL),A			;(DPB+1Eh),FFh
DIRIN_:	        POP	BC
		INC	C
		DJNZ	DIRINT
		POP	AF
ENDIF
		EX      AF,AF'
		DI
		LD      A,(EXPTBL+0)
		LD      H,00H
		CALL    ENASLT                  ; select ROM BIOS on page 0
		LD      A,(P2_TPA)
		CALL    PUT_P2
		LD      A,(P0_TPA)
		CALL    PUT_P0
		EI
		EX      AF,AF'
		RET

; Subroutine 
; Mod: fixed to international character generator and simplified to reduce rom size.
C418C:		LD      A,1
		LD      (KBUF+0),A
		LD      HL,I_BA75
		LD	DE,I423F
		XOR     A
J4199:		PUSH	AF
		CALL    C41A4		; translate char code?
		LD      (HL),A
		POP	AF
		INC     HL
		INC     A
		JR	NZ,J4199
		RET

C41A4:		CP      'a'
		RET	C		; no change
		CP      'z'+1
		JR      C,J41D7		; lower case to upper case
		CP      080H
		RET     C
		CP      0C0H
		RET     NC
		LD	A,(DE)		; translate charcode between 080H and 0C0H
		INC	DE
		RET

J41D7:		ADD     A,-32		; Make uppercase
J41D9:		RET

; international charactergenerator
I423F:		DEFB    080H,09AH,045H,041H,08EH,041H,08FH,080H
		DEFB    045H,045H,045H,049H,049H,049H,08EH,08FH
		DEFB    090H,092H,092H,04FH,099H,04FH,055H,055H
		DEFB    059H,099H,09AH,09BH,09CH,09DH,09EH,09FH
		DEFB    041H,049H,04FH,055H,0A5H,0A5H,0A6H,0A7H
		DEFB    0A8H,0A9H,0AAH,0ABH,0ACH,0ADH,0AEH,0AFH
		DEFB    0B0H,0B0H,0B2H,0B2H,0B4H,0B4H,0B6H,0B6H
		DEFB    0B8H,0B8H,0BAH,0BBH,0BCH,0BDH,0BEH,0BFH

; Subroutine initialize and allocate memory mapper segments for dos
C492F:		CALL    C49D7
		RET     C
		EX      DE,HL
		LD      HL,KBUF+32
		SBC     HL,DE
		ADD     HL,HL
		ADD     HL,HL
		INC     HL
		PUSH    DE
		CALL    ALLOCMEM
		POP     DE
		RET     C
		LD      (MAP_TAB),HL
		EX      DE,HL
J4946:		LD      A,(HL)
		OR      A
		JR      Z,J495A
		INC     HL
		LDI
		LD      (DE),A
		INC     DE
		LD      (DE),A
		INC     DE
		XOR     A
		LD      B,5
R4954:	 LD      (DE),A
		INC     DE
		DJNZ    R4954
		JR      J4946
J495A:		LD      (DE),A
		LD      HL,(MAP_TAB)
		LD      A,(HL)                  ; slotid primary memory mapper
		PUSH    HL
		LD      (RAMAD3),A
		LD      (RAMAD2),A
		LD      (RAMAD1),A
		LD      (RAMAD0),A
		LD      H,80H
		CALL    ENASLT
		POP     HL
		INC     HL
		LD      A,(HL)                  ; number of segments in primary memory mapper
		DEC     A
		LD      (DATA_S),A              ; last segment for BDOS data
		DEC     A
		LD      (CODE_S),A              ; second last segment for BDOS code
		SUB     4                       ; four segments for TPA
		INC     HL
		LD      (HL),A                  ; free segments primary memory mapper
		INC     HL
		LD      (HL),4+1+1              ; 6 reserved segments primary memory mapper
		LD      DE,P0_64K
		LD      HL,P0_SEG
		LD      A,4-1
J498B:		LD      (DE),A
		INC     DE
		LD      (HL),A
		INC     HL
		DEC     A
		JP      P,J498B
		LD      A,(DATA_S)              ; BDOS data segment
		OUT     (0FEH),A
		LD      HL,ISBBFF
J499B:		LD      (HL),00H
		DEC     HL
		BIT     7,H
		JR      NZ,J499B
		LD      HL,ISB062
		LD      (D_B064),HL
		LD      HL,(MAP_TAB)
		LD      A,(HL)                  ; slotid primary memory mapper
		INC     HL
		PUSH    HL
		CALL    C4AD0
		DEC     HL
		DEC     (HL)
		DEC     HL
		DEC     (HL)
		EX      DE,HL
		LD      B,4
J49B8:		DEC     (HL)
		INC     HL
		DJNZ    J49B8
		POP     HL
		LD      BC,7
		ADD     HL,BC
J49C1:		LD      A,(HL)
		OR      A
		JR      Z,J49D1
		INC     HL
		PUSH    HL
		CALL    C4AD0
		POP     HL
		LD      BC,7
		ADD     HL,BC
		JR      J49C1
J49D1:		LD      A,01H
		OUT     (0FEH),A
		OR      A
		RET

; ------------------------------------------------------------------------------
; Subroutine initialize memory mapper
; ------------------------------------------------------------------------------
C49D7:		DI
		PUSH    AF
		LD      HL,KBUF+32
		XOR     A
		LD      (HL),A                  ; empty memory mapper list
		PUSH    HL
		LD      HL,EXPTBL+0
J49E2:		BIT     7,(HL)
		JR      Z,J49E8
		SET     7,A
J49E8:		PUSH    HL
		PUSH    AF
		LD      H,80H
		CALL    ENASLT                  ; enable slot on page 2
		CALL    C4A34                   ; test memory mapper
		OR      A
		JR      Z,J4A0D                 ; no memory mapper, next
		POP     BC
		POP     HL
		EX      (SP),HL
		PUSH    BC
		LD      C,A
		LD      A,(HL)
		CP      C                       ; this memory mapper bigger then sofar ?
		JR      C,J4A05                 ; yep, put this memory mapper last on the list
		LD      (HL),C
		LD      C,A
		INC     HL
		LD      A,(HL)
		LD      (HL),B
		LD      B,A
		DEC     HL                      ; put this memory mapper on the list and keep bigest last on the list
J4A05:		DEC     HL
		LD      (HL),B
		DEC     HL
		LD      (HL),C
		POP     BC
		EX      (SP),HL
		PUSH    HL
		PUSH    BC
J4A0D:		POP     AF
		POP     HL
		BIT     7,A
		JR      Z,J4A19
		ADD     A,4
		BIT     4,A
		JR      Z,J49E8
J4A19:		INC     HL
		INC     A
		AND     03H
		JR      NZ,J49E2
		POP     HL
		POP     AF
		PUSH    HL
		LD      H,80H
		CALL    ENASLT                  ; restore slot on page 2
		POP     HL
		LD      A,(HL)
		CP      6
		RET     C
		PUSH    HL
		INC     HL
		CALL    C4A87
		POP     HL
		XOR     A
		RET

; Subroutine 
C4A34:		LD      HL,08000H
		LD      B,(HL)
		LD      (HL),0AAH
		XOR     A
		OUT     (0FEH),A
		LD      C,(HL)
		LD      (HL),55H
		INC     A
		OUT     (0FEH),A
		LD      E,(HL)
		XOR     A
		OUT     (0FEH),A
		LD      (HL),C
		INC     A
		OUT     (0FEH),A
		LD      (HL),B
		LD      A,E
		CP      0AAH
		LD      B,00H
		JR      NZ,J4A81
		LD      HL,08000H
		LD      B,00H
J4A58:		LD      A,B
		OUT     (0FEH),A
		LD      A,(HL)
		PUSH    AF
		INC     SP
		LD      (HL),0AAH
		INC     B
		JR      NZ,J4A58
J4A63:		LD      A,B
		OUT     (0FEH),A
		LD      A,(HL)
		CP      0AAH
		JR      NZ,J4A75
		LD      A,55H
		LD      (HL),A
		CP      (HL)
		JR      NZ,J4A75
		INC     B
		JR      NZ,J4A63
		DEC     B
J4A75:		LD      C,00H
J4A77:		LD      A,C
		DEC     A
		OUT     (0FEH),A
		DEC     SP
		POP     AF
		LD      (HL),A
		DEC     C
		JR      NZ,J4A77
J4A81:		LD      A,01H
		OUT     (0FEH),A
		LD      A,B
		RET

; Subroutine 
C4A87:		LD      A,(HL)
		PUSH    AF
		LD      H,80H
		CALL    ENASLT
		XOR     A
		OUT     (0FEH),A
		LD      HL,0
		SBC     HL,SP
		LD      C,L
		LD      B,H
		LD      HL,0
		ADD     HL,SP
		LD      E,L
		LD      D,H
		RES     6,D
		LDIR
		OUT     (0FFH),A
		INC     A
		OUT     (0FEH),A
		INC     A
		OUT     (0FDH),A
		INC     A
		OUT     (0FCH),A
		POP     AF
		LD      HL,0
		ADD     HL,SP
		LD      SP,0C000H
		PUSH    AF
		PUSH    HL
		CALL    ENASLT
		POP     HL
		POP     AF
		LD      SP,HL
		BIT     7,A
		JR      Z,J4ACF
		AND     03H
		LD      C,A
		LD      B,00H
		LD      HL,SLTTBL
		ADD     HL,BC
		LD      A,(DFFFF)
		CPL
		LD      (HL),A
J4ACF:		RET

; Subroutine 
C4AD0:		EX      DE,HL
		AND     0FH
		ADD     A,A
		ADD     A,A
		LD      C,A
		LD      B,00H
		LD      HL,I_BA35
		ADD     HL,BC
		LD      (HL),E
		INC     HL
		LD      (HL),D
		INC     HL
		EX      DE,HL
		LD      C,(HL)
		LD      B,00H
		LD      HL,(D_B064)
		OR      A
		SBC     HL,BC
		LD      (D_B064),HL
		INC     HL
		INC     HL
		EX      DE,HL
		LD      (HL),E
		INC     HL
		LD      (HL),D
		LD      H,D
		LD      L,E
		ADD     HL,BC
		RET

J4AF7:		PUSH    DE
		PUSH    AF
		CALL    C4AFF
		POP     AF
		POP     DE
		RET

; Subroutine 
C4AFF:		LD      A,D
		OR      E                       ; broadcast + function: build device name table ?
		JR      NZ,J4B0B                ; nope,
		LD      A,4
		CALL    C4B5C                   ; memory mapper device id
		JP      C4B5C                   ; reserved byte
J4B0B:		LD      A,D
		CP      4                       ; memory mapper device ?
		RET     NZ                      ; nope, quit
		LD      A,E
		OR      A                       ; function 0 ?
		JR      Z,J4B1A                 ; yep,
		DEC     A                       ; function 1 ?
		JR      Z,J4B43                 ; yep,
		DEC     A                       ; function 2 ?
		JR      Z,J4B4C                 ; yep,
		RET

; Function 0: Build 
J4B1A:		PUSH    HL
		LD      HL,(MAP_TAB)
		LD      C,(HL)                  ; slotid primary memory mapper
		INC     HL
		LD      D,(HL)                  ; number of segments in primary memory mapper
		INC     HL
		LD      E,(HL)                  ; number of free segments in primary memory mapper
		POP     HL
		LD      A,C
		CALL    C4B5C                   ; slotid
		LD      A,MAP_VE % 256		; rem: LOW MAP_VE
		CALL    C4B5C
		LD      A,MAP_VE / 256		; rem: HIGH MAP_VE
		CALL    C4B5C                   ; memory mapper jump table
		LD      A,E
		CALL    C4B5C                   ; number of free segments in primary memory mapper
		LD      A,D
		CALL    C4B5C                   ; number of segments in primary memory mapper
		CALL    C4B5C                   ; reserved byte
		CALL    C4B5C                   ; reserved byte
		JP      C4B5C                   ; reserved byte

; Function 1: Get mapper variable table
J4B43:		POP     DE
		POP     AF
		LD      HL,(MAP_TAB)
		LD      A,(HL)                  ; slotid primary memory mapper
		PUSH    AF
		PUSH    DE
		RET

; Function 2: Get mapper support routine address
J4B4C:		POP     DE
		POP     AF
		LD      HL,(MAP_TAB)
		LD      B,(HL)                  ; slotid primary memory mapper
		INC     HL
		LD      A,(HL)                  ; number of segments in primary memory mapper
		INC     HL
		LD      C,(HL)                  ; number of free segments in primary memory mapper
		LD      HL,MAP_VE               ; memory mapper jump table
		PUSH    AF
		PUSH    DE
		RET

; Subroutine 
C4B5C:		PUSH    BC
		PUSH    DE
		LD      E,A
		LD      A,B
		CALL    WRSLT
		POP     DE
		POP     BC
		INC     HL
		XOR     A
		RET

; Subroutine allocate RAM memory
ALLOCMEM:	LD      A,L
		OR      H
		RET     Z
		EX      DE,HL
		LD      HL,0
		SBC     HL,DE
		LD      C,L
		LD      B,H
		ADD     HL,SP
		CCF
		RET     C
		LD      A,H
		CP      0C2H
		RET     C
		LD      DE,(BOTTOM)
		SBC     HL,DE
		RET     C
		LD      A,H
		CP      02H
		RET     C
		PUSH    BC
		LD      HL,0
		ADD     HL,SP
		LD      E,L
		LD      D,H
		ADD     HL,BC
		PUSH    HL
		LD      HL,(STKTOP)
		OR      A
		SBC     HL,DE
		LD      C,L
		LD      B,H
		INC     BC
		POP     HL
		LD      SP,HL
		EX      DE,HL
		LDIR
		POP     BC
		LD      HL,(HIMEM)
		ADD     HL,BC
		LD      (HIMEM),HL
		LD      DE,-534
		ADD     HL,DE
		LD      (FILTAB),HL
		EX      DE,HL
		LD      HL,(MEMSIZ)
		ADD     HL,BC
		LD      (MEMSIZ),HL
		LD      HL,(NULBUF)
		ADD     HL,BC
		LD      (NULBUF),HL
		LD      HL,(STKTOP)
		ADD     HL,BC
		JR      J4BE5
Q4BC0:		LD      A,1
		LD      (MAXFIL),A
		LD      HL,(HIMEM)
		LD      DE,-534
		ADD     HL,DE
		LD      (FILTAB),HL
		LD      E,L
		LD      D,H
		DEC     HL
		DEC     HL
		LD      (MEMSIZ),HL
		LD      BC,200
		OR      A
		SBC     HL,BC
		PUSH    HL
		LD      HL,13
		ADD     HL,DE
		LD      (NULBUF),HL
		POP     HL
J4BE5:		LD      (STKTOP),HL
		DEC     HL
		DEC     HL
		LD      (SAVSTK),HL
		LD      L,E
		LD      H,D
		INC     HL
		INC     HL
		INC     HL
		INC     HL
		LD      A,02H
J4BF5:		EX      DE,HL
		LD      (HL),E
		INC     HL
		LD      (HL),D
		INC     HL
		EX      DE,HL
		LD      BC,7
		LD      (HL),B
		ADD     HL,BC
		LD      (HL),B
		LD      BC,0102H
		ADD     HL,BC
		DEC     A
		JR      NZ,J4BF5
		RET

J4C09:		LD      HL,I4D0E
		CALL    C4C21
		RET     Z
		PUSH    AF
		CP      64
		LD      A,11
		JR      NC,J4C19
		LD      A,12
J4C19:		CALL    C4C1E
		POP     AF
		RET

; Subroutine 
C4C1E:		LD      HL,I4C4A

; Subroutine 
C4C21:		PUSH    BC
		PUSH    DE
		LD      E,A
J4C2F:		LD      A,(HL)
		OR      A
		LD      A,E
		JR      Z,J4C44
		SUB     (HL)
		INC     HL
		LD      C,(HL)
		LD      B,00H
		INC     HL
		JR      Z,J4C3F
		ADD     HL,BC
		JR      J4C2F
J4C3F:		POP     DE
		PUSH    DE
		LDIR
		EX      DE,HL
J4C44:		POP     DE
		POP     BC
		OR      A
		RET

; ------------------------------------------------------------------------------
; *** Messages ***
; ------------------------------------------------------------------------------

I4C4A:		DOSST1  1,"Not enough memory"
		DOSST1  2,"Drive name? ("
		DOSST1  3,") "
		DOSST1  4,"Strike a key when ready "
		DOSST1  5,"Aborted"
		DOSST1  6,"Format complete"
		DOSST1  7,"Insert disk for drive "
		DOSST1  8,":"
		DOSST1  9,"and strike a key when ready "
		DOSST1  10,"*** "
		DOSST1  11,"System error "
		DOSST1  12,"User error "
		DEFB    0

; ERRORS
I4D0E:		DOSST1  0FFH,"Incompatible disk"
		DOSST1  0FEH,"Write error"
		DOSST1  0FDH,"Disk error"
		DOSST1  0FCH,"Not ready"
		DOSST1  0FBH,"Verify error"
		DOSST1  0FAH,"Data error"
		DOSST1  0F9H,"Sector not found"
		DOSST1  0F8H,"Write protected disk"
		DOSST1  0F7H,"Unformatted disk"
		DOSST1  0F6H,"Not a DOS disk"
		DOSST1  0F5H,"Wrong disk"
		DOSST1  0F4H,"Wrong disk for file"
		DOSST1  0F3H,"Seek error"
		DOSST1  0F2H,"Bad file allocation table"
		DOSST1  0F0H,"Cannot format this drive"
		DOSST1  0DFH,"Internal error"
		DOSST1  0DEH,"Not enough memory"
		DOSST1  0DCH,"Invalid MSX-DOS call"
		DOSST1  0DBH,"Invalid drive"
		DOSST1  0DAH,"Invalid filename"
		DOSST1  0D9H,"Invalid pathname"
		DOSST1  0D8H,"Pathname too long"
		DOSST1  0D7H,"File not found"
		DOSST1  0D6H,"Directory not found"
		DOSST1  0D5H,"Root directory full"
		DOSST1  0D4H,"Disk full"
		DOSST1  0D3H,"Duplicate filename"
		DOSST1  0D2H,"Invalid directory move"
		DOSST1  0D1H,"Read only file"
		DOSST1  0D0H,"Directory not empty"
		DOSST1  0CFH,"Invalid attributes"
		DOSST1  0CEH,"Invalid . or .. operation"
		DOSST1  0CDH,"System file exists"
		DOSST1  0CCH,"Directory exists"
		DOSST1  0CBH,"File exists"
		DOSST1  0CAH,"File is already in use"
		DOSST1  0C9H,"Cannot transfer above 64k"
		DOSST1  0C8H,"File allocation error"
		DOSST1  0C7H,"End of file"
		DOSST1  0C6H,"File access violation"
		DOSST1  0C5H,"Invalid process id"
		DOSST1  0C4H,"No spare file handles"
		DOSST1  0C3H,"Invalid file handle"
		DOSST1  0C2H,"File handle not open"
		DOSST1  0C1H,"Invalid device operation"
		DOSST1  0C0H,"Invalid environment string"
		DOSST1  0BFH,"Environment string too long"
		DOSST1  0BEH,"Invalid date"
		DOSST1  0BDH,"Invalid time"
		DOSST1  0BCH,"RAM disk (drive H:) already exists"
		DOSST1  0BBH,"RAM disk does not exist"
		DOSST1  0BAH,"File handle has been deleted"
		DOSST1  0B8H,"Invalid sub-function number"
		DOSST1  0B7H,"Invalid File Control Block"
		DOSST1  09FH,"Ctrl-STOP pressed"
		DOSST1  09EH,"Ctrl-C pressed"
		DOSST1  09DH,"Disk operation aborted"
		DOSST1  09CH,"Error on standard output"
		DOSST1  09BH,"Error on standard input"
		DOSST1  08FH,"Wrong version of command"
		DOSST1  08EH,"Unrecognized command"
		DOSST1  08DH,"Command too long"
		DOSST1  08BH,"Invalid parameter"
		DOSST1  08AH,"Too many parameters"
		DOSST1  089H,"Missing parameter"
		DOSST1  088H,"Invalid option"
		DOSST1  087H,"Invalid number"
		DOSST1  086H,"File for HELP not found"
		DOSST1  085H,"Wrong version of MSX-DOS"
		DOSST1  084H,"Cannot concatenate destination file"
		DOSST1  083H,"Cannot create destination file"
		DOSST1  082H,"File cannot be copied onto itself"
		DOSST1  081H,"Cannot overwrite previous destination file"
		DOSST1  07FH,"Insert MSX-DOS2 disk in drive \x07:"		; rem: \x07 = bell
		DOSST1  07EH,"Press any key to continue... "
		DEFB    0

IFDEF FAT16
; ------------------------------------------------------------------------------
; *** BOOTCODE ***
; If FAT16 is enabled then the bootsector code is stored in the disk rom
; page 1 main code to free space in the page 0 kernel code.
; ------------------------------------------------------------------------------
IBOOTCODE:
		INCLUDE	"bootcode.inc"
ENDIF 

