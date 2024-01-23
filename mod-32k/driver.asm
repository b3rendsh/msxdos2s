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

	SECTION	DRV

BRSLREG EQU     0138H                   ; use BRSLREG instead of RSLREG to avoid name clash with disk.mac

DEFDPB	EQU	$-1
MYSIZE	EQU	0
SECLEN  EQU     512

        IF HSH = 0
INIHRD:
        ENDIF
C6EFA:	RET

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

        IF HSH = 1

INIHRD:
        LD      A,7
        CALL    SNSMAT
        BIT     6,A                     ; SELECT key pressed ?
        JR      Z,LQDOS2                ; yep, quit DOS2
        LD      A,6
        CALL    SNSMAT
        BIT     4,A                     ; CODE key pressed ?
        RET     NZ                      ; nope,
        BIT     2,A                     ; with GRAPH key pressed ?
        RET     NZ                      ; nope,
LQDOS2: INC     SP
        INC     SP
        RET

        ENDIF

        IF HSH = 2

INIHRD:
        LD      A,8
        CALL    SNSMAT
        BIT     2,A                     ; INS key pressed ?
        JR      Z,LQDOS2                ; yep, quit DOS2
        RET
        DEFB    0D2H,000H,000H,000H,000H,000H,0FFH,0FFH,0FFH,0FFH
LQDOS2: INC     SP
        INC     SP
        RET
        DEFB    06EH

        ENDIF

