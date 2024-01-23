; DISK-S1.ASM
;
; DOS 2.20 kernel bank 1 (ASCII version)
;
; Source re-created by Z80DIS 2.2
; Z80DIS was written by Kenneth Gielow, Palo Alto, CA
;
; Code Copyrighted by ASCII and maybe others
; Source comments by Arjen Zeilemaker
;
; Sourcecode supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyrightholders
;
; ------------------------------------------------------------------------------
; H_J. Berends:
; Converted sources to assemble with z88dk z80asm
; Added unused code in the header (top 255 bytes), it is identical to bank 0

	SECTION	BANK1
 	ORG     04000H
S_ORG1	EQU	04000H		; Offset for current address: $ calculations

DOSST1  MACRO   X,Y
        LOCAL   _D1
        DEFB    X
        DEFB    _D1-$
        DEFB    Y
_D1:    DEFB    0
        ENDM

; rem: this macro is not implemented in z80asm
;DOSST2  MACRO   X,Y
;        LOCAL   _D2
;        DEFB    X
;        DEFB    _D2-$
;Z1      ASET    0
;        IRPC    Z,<Y>
;        IF      Z1 EQ 0
;Z1      ASET    1
;        IF      ('&Z' GE '0') AND ('&Z' LE '9')
;Z2      ASET    ('&Z'-'0')*16
;        ENDIF
;        IF      ('&Z' GE 'A') AND ('&Z' LE 'F')
;Z2      ASET    ('&Z'-'A'+10)*16
;        ENDIF
;        ELSE
;        IF      ('&Z' GE '0') AND ('&Z' LE '9')
;Z2      ASET    Z2+('&Z'-'0')
;        ENDIF
;        IF      ('&Z' GE 'A') AND ('&Z' LE 'F')
;Z2      ASET    Z2+('&Z'-'A'+10)
;        ENDIF
;        DEFB    Z2
;Z1      ASET    0
;        ENDIF
;        ENDM
;_D2:    DEFB    0
;        ENDM

        INCLUDE "DISK.INC"

WRSLT   EQU     0014H
CALSLT  EQU     001CH
ENASLT  EQU     0024H
IDBYT0  EQU     002BH
KEYINT  EQU     0038H
SSLOTL  EQU     0046H
SSLOTE  EQU     004BH
C005C   EQU     005CH                   ; ALL_SEG in BDOS code segment
C005F   EQU     005FH                   ; FRE_SEG in BDOS code segment
GETSLT  EQU     402DH
J4092   EQU     4092H			; select ROM segment
J47D6   EQU     47D6H			; initialize DOS2

KBDOS   EQU     0005H

ISB062  EQU     0B062H
D_B064  EQU     0B064H
ISB0ED  EQU     0B0EDH
CSB382  EQU     0B382H
I_B396  EQU     0B396H
C_B582  EQU     0B582H
C_B782  EQU     0B782H
C_B982  EQU     0B982H
ISBA35  EQU     0BA35H
ISBA75  EQU     0BA75H
C_BA89  EQU     0BA89H
ISBBFF  EQU     0BBFFH
CSBD02  EQU     0BD02H


JUMPB   EQU     0F1D3H                  ; handlers
P0_RAM  EQU     0F1FDH                  ; enable DOS memory on page 0 handler
MAP_VE  EQU     0F200H
PUT_P0  EQU     0F218H                  ; PUT_P0
PUT_P2  EQU     0F224H                  ; PUT_P2
CUR_DRV EQU     0F23CH                  ; default drive
H_PROM  EQU     0F24FH
TIM_RA  EQU     0F2B8H                  ; number of VBLANK interrupts per 1/10 second
RANDOM  EQU     0F2BAH                  ; 'random' number
MAP_TAB EQU     0F2C5H                  ; memory mapper table
P0_SEG  EQU     0F2C7H                  ; current segment page 0
P1_SEG  EQU     0F2C8H                  ; current segment page 1
P2_SEG  EQU     0F2C9H                  ; current segment page 2
P3_SEG  EQU     0F2CAH                  ; current segment page 3
P0_TPA  EQU     0F2CBH                  ; segment page 0 at BDOS entry
P1_TPA  EQU     0F2CCH                  ; segment page 1 at BDOS entry
P2_TPA  EQU     0F2CDH                  ; segment page 2 at BDOS entry
DATA_S  EQU     0F2CFH                  ; BDOS data segment
CODE_S  EQU     0F2D0H                  ; BDOS code segment
SP_IRQ  EQU     0F2D1H                  ; temporary save stackpointer (KEYINT)
IRQ_ST  EQU     0F2D3H                  ; pointer to temporary stack (KEYINT)
SPROMPT EQU     0F2DCH                  ; pointer to prompt hander
SBDOS   EQU     0F2DEH                  ; pointer to BDOS handler
SS_TEMP EQU     0F2E0H
RD_ADDR EQU     0F2E1H
IX_BDOS EQU     0F2E6H                  ; temporary save IX register BDOS call
PS_BDOS EQU     0F2EAH
SS_BDOS EQU     0F2EBH
BDOS_ST EQU     0F2FEH                  ; pointer to temporary stack (BDOS)
KANJTA  EQU     0F30FH                  ; double byte header table
P0_64K  EQU     0F314H                  ; TPA segment table
AUXBOD	EQU	0F327H
RAMAD0  EQU     0F341H
RAMAD1  EQU     0F342H
RAMAD2  EQU     0F343H
RAMAD3  EQU     0F344H
MASTER  EQU     0F348H
SDOSON  EQU     0F368H
SDOSOF  EQU     0F36BH
XFER    EQU     0F36EH
SAUXIN  EQU     0F371H
SAUXOU  EQU     0F374H
GO_BIOS EQU     0F377H
GO_BDOS EQU     0F37AH
BDOS  	EQU     0F37DH

RDPRIM  EQU     0F380H
WRPRIM  EQU     0F385H
CLPRIM  EQU     0F38CH
CLPRM1  EQU     0F398H
KBUF    EQU     0F41FH
MEMSIZ  EQU     0F672H
STKTOP  EQU     0F674H
SAVSTK  EQU     0F6B1H
MAXFIL  EQU     0F85FH
FILTAB  EQU     0F860H
NULBUF  EQU     0F862H
BOTTOM  EQU     0FC48H
HIMEM   EQU     0FC4AH
EXPTBL  EQU     0FCC1H
SLTTBL  EQU     0FCC5H
LFFFF   EQU     0FFFFH

; 
        DEFB	"AB"

        IF MSXJE = 0

        DEFW    C403C                   ; EXTENSION ROM INIT handler
	DEFW    0575CH                  ; EXTENSION ROM CALL statement handler


        ELSE

        DEFW    C406A                   ; EXTENSION ROM INIT handler
	DEFW    0	                ; EXTENSION ROM CALL statement handler

        ENDIF

        DEFW	0
        DEFW	0
        DEFW	0
        DEFW	0
        DEFW	0

; -------------------------------------
; rem: unused code in bank 1
        JP	06EFEH
        JP	06EFEH
        JP	06EFEH
	JP	06F02H
	JP	06F07H
	JP	06F01H
	JP	04B1BH
	SCF
	JP	04E67H
	JP	04CD3H
	NOP
	JP	04E05H
	LD	HL,(0F34BH)
	RET
	JP	04177H
	NOP
	DEFW	0411EH
; -------------------------------------

        DEFS    0403CH-$-S_ORG1,0


        IF      MSXJE = 0

;         Subroutine EXTENSION ROM INIT handler
;            Inputs  ________________________
;            Outputs ________________________

C403C:  XOR     A
        CALL    C4092                  ; select DOS2 ROM bank 0

; -------------------------------------
; rem: unused code in bank 1
	JP	047D6H
	CALL	0410CH
	JP	0F2D5H
	PUSH	AF
	LD	A,(040FFH)
	PUSH	AF
	XOR	A
	CALL	04092H
	CALL	04CF3H
	POP	AF
	CALL	04092H
	POP	AF
	RET
; -------------------------------------

;         Subroutine inter DOS2 ROM bank call
;            Inputs  A = DOS2 ROM bank, IX = routine
;            Outputs ________________________

J405B:  CALL    C4092                  ; select DOS2 ROM bank
        EX      AF,AF'
        CALL    C4069                  ; call routine (in IX)
        EX      AF,AF'
        XOR     A
        CALL    C4092                  ; select DOS2 ROM bank 0
        EX      AF,AF'
        RET     

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4069:  JP      (IX)

        DEFS    04078H-$-S_ORG1,0FFH

; -------------------------------------
; rem: unused code in bank 1
	JP	0417CH
        DEFS    04080H-$-S_ORG1,0FFH
	JP      06D49H
	JP	06D39H
	JP	06EF1H
	JP	06EF2H
	JP	06EF6H
	JP      04181H
; -------------------------------------

        ELSE

        DEFS    04051H-$-S_ORG1,0

;         Subroutine inter DOS2 ROM bank 0 call
;            Inputs  AF' = 
;            Outputs ________________________

C4051:  EX      AF,AF'
        LD      A,(D40FF)
        PUSH    AF                      ; save current DOS2 ROM bank
        CALL    C406E                   ; select DOS2 ROM bank 0 and execute routine
        EX      AF,AF'
        POP     AF
        CALL    C4092                   ; restore DOS2 ROM bank
        EX      AF,AF'
        RET     

;         Subroutine inter DOS2 ROM bank call
;            Inputs  A = DOS2 ROM bank, IX = routine
;            Outputs ________________________

J405B:
J4060:  CALL    C406F                   ; select DOS2 ROM bank and execute routine
        EX      AF,AF'
        XOR     A
        CALL    C4092                   ; select DOS2 ROM bank 0
        EX      AF,AF'
        RET     

;         Subroutine EXTENSION ROM INIT handler
;            Inputs  ________________________
;            Outputs ________________________

C406A:  LD      IX,J47D6                ; DOS2 kernel INIT routine

;         Subroutine select DOS2 ROM bank 0 and execute routine
;            Inputs  AF' = 
;            Outputs ________________________

C406E:  XOR     A

;         Subroutine select DOS2 ROM bank and execute routine
;            Inputs  AF' = , A = bank
;            Outputs ________________________

C406F:  CALL    C4092                   ; select DOS2 ROM bank
        EX      AF,AF'
        JP      (IX)

        DEFS    04092H-$-S_ORG1,0

        ENDIF

;         Subroutine Select DOS2 ROM bank
;            Inputs  ________________________
;            Outputs ________________________

        IF BNKTRL = 1

C4092:  PUSH	HL
	PUSH	AF
	ADD	A,L40A0 % 256	; rem: LOW L40A0
	LD	L,A
	LD	H,L40A0 / 256	; rem: HIGH L40A0
	LD	A,(HL)
        BNKCHG
  	POP	AF
	POP	HL
	RET

L40A0:  MBNKTL
        ELSE

C4092:  BNKCHG
        RET     

        ENDIF


        DEFS    040FFH-$-S_ORG1,0

D40FF:  DEFB    1

I4100:  JP      C49D7
Q4103:  JP      J410F
Q4106:  JP      C4C1E
Q4109:  JP      J4C09
C410C:  JP      J4AF7                   ; EXTBIO handler

J410F:  DI
        CALL    C492F
        RET     C
        CALL    C427F
        RET     C
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
        LD      A,(DATA_S)              ; BDOS data segment
        CALL    PUT_P2                  ; PUT_P2
        CALL    C418C
        LD      A,(CODE_S)              ; BDOS code segment
        CALL    PUT_P0                  ; PUT_P0
        LD      HL,0C9F1H
        PUSH    HL
        LD      HL,0B0EDH
        PUSH    HL                      ; LDIR POP AF RET
        LD      HL,I416E
        PUSH    HL                      ; resume
        LD      HL,J4092
        PUSH    HL
        LD      A,(D40FF)
        PUSH    AF                      ; save current DOS2 rom bank (1)
        LD      HL,6
        ADD     HL,SP
        PUSH    HL                      ; helper routine on stack
        CALL    P0_RAM                  ; enable DOS memory on page 0
        LD      HL,I4100
        LD      DE,0
        LD      BC,04000H-0100H
        LD      A,2
        JP      J4092                   ; select DOS2 rom bank BDOS code
                                        ; copy BDOS code
                                        ; restore DOS2 rom bank

I416E:  POP     HL
        POP     HL
        CALL    0                       ; initialize BDOS code
        EX      AF,AF'
        DI
        LD      A,(EXPTBL+0)
        LD      H,00H
        CALL    ENASLT                  ; select ROM BIOS on page 0
        LD      A,(P2_TPA)
        CALL    PUT_P2                  ; PUT_P2
        LD      A,(P0_TPA)
        CALL    PUT_P0                  ; PUT_P0
        EI
        EX      AF,AF'
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C418C:  LD      A,(IDBYT0)
        AND     0FH
        LD      (KBUF+0),A
        LD      HL,ISBA75
        XOR     A
        LD      B,A
J4199:  PUSH    AF
        CALL    C41A4
        LD      (HL),A
        POP     AF
        INC     A
        INC     HL
        DJNZ    J4199
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C41A4:  PUSH    HL
        PUSH    BC
        PUSH    AF
        LD      A,(KBUF+0)
        AND     03H
        ADD     A,A
        LD      C,A
        LD      B,0
        LD      HL,I41DC
        ADD     HL,BC
        LD      A,(HL)
        INC     HL
        LD      H,(HL)
        LD      L,A
        POP     AF
        LD      B,(HL)
        INC     HL
J41BB:  CP      (HL)
        INC     HL
        JR      C,J41C2
        CP      (HL)
        JR      C,J41D7
J41C2:  INC     HL
        INC     HL
        DJNZ    J41BB
        CP      80H
        JR      C,J41D9
        CP      0C0H
        JR      NC,J41D9
        LD      C,(HL)
        INC     HL
        LD      H,(HL)
        LD      L,C
        LD      C,A
        ADD     HL,BC
        LD      A,(HL)
        JR      J41D9
J41D7:  INC     HL
        ADD     A,(HL)
J41D9:  POP     BC
        POP     HL
        RET

I41DC:  DEFW    I41E4
        DEFW    I41EA
        DEFW    I41F0
        DEFW    I41F6


; japanese charactergenerator

I41E4:  DEFB    1
        DEFB    061H,07BH,-32
        DEFW    I41FF-080H

; international charactergenerator

I41EA:  DEFB    1
        DEFB    061H,07BH,-32
        DEFW    I423F-080H

; korean charactergenerator

I41F0:  DEFB    1
        DEFB    061H,07BH,-32
        DEFW    I41FF-080H

; unknown charactergenerator (3)

I41F6:  DEFB    2
        DEFB    061H,07BH,-32
        DEFB    0C0H,0DFH,-32
        DEFW    I41FF-080H

I41FF:  DEFB    080H,081H,082H,083H,084H,085H,086H,087H
        DEFB    088H,089H,08AH,08BH,08CH,08DH,08EH,08FH
        DEFB    090H,091H,092H,093H,094H,095H,096H,097H
        DEFB    098H,099H,09AH,09BH,09CH,09DH,09EH,09FH
        DEFB    0A0H,0A1H,0A2H,0A3H,0A4H,0A5H,0A6H,0A7H
        DEFB    0A8H,0A9H,0AAH,0ABH,0ACH,0ADH,0AEH,0AFH
        DEFB    0B0H,0B1H,0B2H,0B3H,0B4H,0B5H,0B6H,0B7H
        DEFB    0B8H,0B9H,0BAH,0BBH,0BCH,0BDH,0BEH,0BFH

I423F:  DEFB    080H,09AH,045H,041H,08EH,041H,08FH,080H
        DEFB    045H,045H,045H,049H,049H,049H,08EH,08FH
        DEFB    090H,092H,092H,04FH,099H,04FH,055H,055H
        DEFB    059H,099H,09AH,09BH,09CH,09DH,09EH,09FH
        DEFB    041H,049H,04FH,055H,0A5H,0A5H,0A6H,0A7H
        DEFB    0A8H,0A9H,0AAH,0ABH,0ACH,0ADH,0AEH,0AFH
        DEFB    0B0H,0B0H,0B2H,0B2H,0B4H,0B4H,0B6H,0B6H
        DEFB    0B8H,0B8H,0BAH,0BBH,0BCH,0BDH,0BEH,0BFH

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C427F:  LD      HL,1595
        CALL    C4B68
        RET     C
        LD      (HL),0
        LD      E,L
        LD      D,H
        INC     DE
        LD      BC,200
        LDIR
        LD      (IRQ_ST),HL
        LD      BC,300
        LDIR
        LD      (BDOS_ST),HL
        EX      DE,HL
        LD      HL,I4418
        PUSH    DE
        LD      BC,I485F-I4418
        LDIR
        LD      BC,JUMPB
        LD      HL,I485F
J42AB:  LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        LD      A,D
        OR      E
        JR      Z,J42C4
        EX      (SP),HL
        EX      DE,HL
        ADD     HL,DE
        EX      DE,HL
        EX      (SP),HL
        LD      A,0C3H
        LD      (BC),A
        INC     BC
        LD      A,E
        LD      (BC),A
        INC     BC
        LD      A,D
        LD      (BC),A
        INC     BC
        JR      J42AB
J42C4:  POP     DE
        LD      HL,I489F
J42C8:  LD      C,(HL)
        INC     HL
        LD      B,(HL)
        INC     HL
        LD      A,B
        OR      C
        JR      Z,J42DE
        PUSH    HL
        LD      H,B
        LD      L,C
        ADD     HL,DE
        LD      A,(HL)
        ADD     A,E
        LD      (HL),A
        INC     HL
        LD      A,(HL)
        ADC     A,D
        LD      (HL),A
        POP     HL
        JR      J42C8
J42DE:  LD      A,(CODE_S)              ; BDOS code segment
        LD      HL,01EEH
        ADD     HL,DE
        LD      (HL),A
        LD      A,(RAMAD3)
        AND     03H     ; 3 
        LD      HL,026DH
        ADD     HL,DE
        LD      (HL),A
        LD      C,A
        LD      B,00H
        RLCA
        RLCA
        RLCA
        RLCA
        OR      C
        LD      HL,0120H
        ADD     HL,DE
        LD      (HL),A
        LD      A,(RAMAD3)
        BIT     7,A
        JR      Z,J4356
        LD      HL,0255H
        ADD     HL,DE
        LD      (HL),B
        INC     HL
        LD      (HL),B
        LD      HL,00E4H
        ADD     HL,DE
        LD      (HL),B
        INC     HL
        LD      (HL),B
        LD      HL,0105H
        ADD     HL,DE
        LD      (HL),B
        INC     HL
        LD      (HL),B
        LD      HL,0274H
        ADD     HL,DE
        LD      (HL),32H
        RRCA
        RRCA
        AND     03H     ; 3 
        LD      HL,0261H
        ADD     HL,DE
        LD      (HL),A
        LD      L,A
        RLCA
        RLCA
        RLCA
        RLCA
        OR      L
        LD      HL,0111H
        ADD     HL,DE
        LD      (HL),A
        PUSH    DE
        LD      HL,SLTTBL
        ADD     HL,BC
        EX      DE,HL
        LD      BC,0263H
        ADD     HL,BC
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      BC,17
        ADD     HL,BC
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      BC,-396
        ADD     HL,BC
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      BC,40
        ADD     HL,BC
        LD      (HL),E
        INC     HL
        LD      (HL),D
        POP     DE
J4356:  LD      A,(EXPTBL+0)
        AND     03H     ; 3 
        LD      HL,024DH
        ADD     HL,DE
        LD      (HL),A
        LD      C,A
        LD      B,00H
        RRCA
        RRCA
        LD      HL,022EH
        ADD     HL,DE
        LD      (HL),A
        LD      A,(EXPTBL+0)
        BIT     7,A
        JR      Z,J43D1
        LD      HL,0236H
        ADD     HL,DE
        LD      (HL),B
        INC     HL
        LD      (HL),B
        RRCA
        RRCA
        AND     03H     ; 3 
        LD      HL,0241H
        ADD     HL,DE
        LD      (HL),A
        LD      A,(RAMAD3)
        XOR     C
        AND     03H     ; 3 
        JR      NZ,J4397
        LD      HL,0245H
        ADD     HL,DE
        LD      (HL),32H        ; "2"
        INC     HL
        LD      (HL),0FFH
        INC     HL
        LD      (HL),0FFH
        JR      J43BC
J4397:  LD      A,0F5H
        LD      HL,0159H
        ADD     HL,DE
        LD      (HL),A
        LD      HL,0188H
        ADD     HL,DE
        LD      (HL),A
        LD      HL,01E9H
        ADD     HL,DE
        LD      (HL),A
        LD      HL,0221H
        ADD     HL,DE
        LD      (HL),0CDH
        LD      HL,017BH
        ADD     HL,DE
        LD      (HL),B
        INC     HL
        LD      (HL),B
        LD      HL,01B2H
        ADD     HL,DE
        LD      (HL),B
        INC     HL
        LD      (HL),B
J43BC:  PUSH    DE
        LD      HL,SLTTBL
        ADD     HL,BC
        EX      DE,HL
        LD      BC,0239H
        ADD     HL,BC
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      BC,10
        ADD     HL,BC
        LD      (HL),D
        DEC     HL
        LD      (HL),E
        POP     DE
J43D1:  LD      HL,I43F6
J43D4:  LD      C,(HL)
        INC     HL
        LD      B,(HL)
        INC     HL
        LD      A,B
        OR      C
        JR      Z,J43F4
        LD      A,0C3H
        LD      (BC),A
        INC     BC
        PUSH    DE
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        EX      (SP),HL
        EX      DE,HL
        BIT     7,H
        JR      NZ,J43EC
        ADD     HL,DE
J43EC:  LD      A,L
        LD      (BC),A
        INC     BC
        LD      A,H
        LD      (BC),A
        POP     HL
        JR      J43D4
J43F4:  OR      A
        RET

I43F6:  DEFW    SDOSON
        DEFW    R0000
        DEFW    SDOSOF
        DEFW    R0009
        DEFW    XFER
        DEFW    R0022
        DEFW    SAUXIN
        DEFW    AUXBOD+0
        DEFW    SAUXOU
        DEFW    AUXBOD+5
        DEFW    GO_BIOS
        DEFW    R00D4
        DEFW    GO_BDOS
        DEFW    R00DC
        DEFW    BDOS
        DEFW    R0064
        DEFW    0

I4418:
        PHASE  0

R0000:  PUSH    AF
        LD      A,I
        PUSH    AF                      ; store IFF2 flag
        LD      A,(MASTER)
        JR      J0010

R0009:  PUSH    AF
        LD      A,I
        PUSH    AF                      ; store IFF2 flag
        LD      A,(RAMAD1)
J0010:  PUSH    HL
        PUSH    DE
        PUSH    BC
        LD      H,40H
        CALL    ENASLT
        POP     BC
        POP     DE
        POP     HL
        POP     AF                      ; restore IFF2 flag
        JP      PE,J0020                ; maskable interrupts where disabled, leave disabled
        EI
J0020:  POP     AF
        RET

R0022:  PUSH    AF
        PUSH    HL
        PUSH    DE
        PUSH    BC
        CALL    GETSLT
        PUSH    AF
        LD      A,(RAMAD1)
        LD      H,40H
        CALL    ENASLT
        POP     AF
        POP     BC
        POP     DE
        POP     HL
        LDIR

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C0038:  PUSH    HL
        PUSH    DE
        PUSH    BC
        LD      H,40H
        CALL    ENASLT
        POP     BC
        POP     DE
        POP     HL
        POP     AF
        RET

R0045:  PUSH    DE
        PUSH    HL
        LD      A,B
        LD      H,40H
        CALL    ENASLT
        EI
        POP     DE
        POP     BC
        LD      HL,(RD_ADDR)
        EX      AF,AF'
        JR      C,J0057
        EX      DE,HL
J0057:  EX      AF,AF'
        LDIR
        LD      A,(MASTER)
        LD      H,40H
        CALL    ENASLT
        EI
        RET

R0064:  LD      (IX_BDOS),IX
        LD      IY,(MASTER-1)
        LD      IX,(SBDOS)
        JP      CALSLT

R0073:  EI
        PUSH    DE
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        EX      DE,HL
        POP     DE
        JP      (HL)

R007B:  EXX
        EX      AF,AF'
        CALL    C00BA
        EX      AF,AF'
        EXX
        CALL    CALSLT

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C0085:  EXX
        EX      AF,AF'
        LD      A,(SDOSOF)
        LD      (SDOSON),A
        CALL    C00BF
        EX      AF,AF'
        EXX
        RET

J0093:  POP     AF
        CALL    C0085
        CALL    GETSLT
        PUSH    AF
        CALL    R0326                   ; GET_P2
        PUSH    AF
        CALL    R00E2

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C00A2:  LD      IY,(MASTER-1)
        LD      IX,(SPROMPT)
        CALL    CALSLT
        CALL    R0103
        POP     AF
        CALL    R0320                   ; PUT_P2
        POP     AF
        LD      H,40H   ; "@"
        CALL    ENASLT

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C00BA:  LD      A,0C9H
        LD      (SDOSON),A

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C00BF:  LD      HL,H_PROM
        LD      DE,I00D1
        LD      B,3
J00C7:  LD      C,(HL)
        LD      A,(DE)
        LD      (HL),A
        LD      A,C
        LD      (DE),A
        INC     HL
        INC     DE
        DJNZ    J00C7
        RET

I00D1:  JP      J0093

R00D4:  CALL    R0103
        CALL    CLPRM1+1
        JR      R00E2

R00DC:  CALL    R0103
        CALL    KBDOS

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

R00E2:  DI
        PUSH    AF
        JR      J00EF

Q_00E6: LD      A,(SS_BDOS)
        LD      (0),A
        LD      (LFFFF),A
J00EF:  LD      A,(PS_BDOS)
        OUT     (0A8H),A
        LD      A,(P0_TPA)
        CALL    R030C                   ; PUT_P0
        LD      A,(P2_TPA)
        CALL    R0320                   ; PUT_P2
        POP     AF
        EI
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

R0103:  DI
        PUSH    AF
        JR      J0118

Q_0107: LD      A,(LFFFF)
        CPL
        LD      (SS_BDOS),A
        AND     0CCH
        OR      00H
        LD      (0),A
        LD      (LFFFF),A
J0118:  IN      A,(0A8H)
        LD      (PS_BDOS),A
        AND     0CCH
        OR      00H
        OUT     (0A8H),A
        CALL    R0312                   ; GET_P0
        LD      (P0_TPA),A
        CALL    R031C                   ; GET_P1
        LD      (P1_TPA),A
        CALL    R0326                   ; GET_P2
        LD      (P2_TPA),A
        LD      A,(CODE_S)
        CALL    R030C                   ; PUT_P0
        LD      A,(DATA_S)
        CALL    R0320                   ; PUT_P2
        POP     AF
        EI
        RET

R0144:  PUSH    AF
        DI
        CALL    R030C                   ; PUT_P0
        LDIR
        LD      A,(CODE_S)
        CALL    R030C                   ; PUT_P0
        EI
        POP     AF
        RET

R0154:  EX      AF,AF'
        PUSH    AF
        CALL    C0235
        NOP
        LD      A,(P0_TPA)
        CALL    R030C                   ; PUT_P0
        LD      A,(P2_TPA)
        CALL    R0320                   ; PUT_P2
        EI
        EX      AF,AF'
        CALL    CLPRM1
        EX      AF,AF'
        CALL    R0254
        LD      A,(CODE_S)
        CALL    R030C                   ; PUT_P0
        LD      A,(DATA_S)
        CALL    R0320                   ; PUT_P2
        JR      J0181

Q_017D: POP     AF
        CALL    C0225
J0181:  POP     AF
        EX      AF,AF'
        EI
        RET

R0185:  CALL    C0235
        NOP
        LD      A,(P0_TPA)
        CALL    R030C                   ; PUT_P0
J018F:  PUSH    HL
        PUSH    BC
        LD      B,(HL)
        LD      A,(P2_TPA)
        CALL    R0320                   ; PUT_P2
        LD      A,B
        EI
        CALL    C00A2
        DI
        LD      A,(DATA_S)
        CALL    R0320                   ; PUT_P2
        POP     BC
        POP     HL
        INC     HL
        DJNZ    J018F
        LD      A,(CODE_S)
        CALL    R030C                   ; PUT_P0
        CALL    R0254
        JR      J01B8

Q_01B4: POP     AF
        CALL    C0225
J01B8:  EI
        RET

R01BA:  DI
        PUSH    AF
        PUSH    HL
        PUSH    BC
        LD      HL,(IRQ_ST)
        OR      A
        SBC     HL,SP
        JR      C,J01CD
        LD      BC,200
        SBC     HL,BC
        JR      C,J01DE
J01CD:  LD      (SP_IRQ),SP
        LD      SP,(IRQ_ST)
        CALL    C01E6
        LD      SP,(SP_IRQ)
        JR      J01E1

J01DE:  CALL    C01E6
J01E1:  POP     BC
        POP     HL
        POP     AF
        EI
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C01E6:  CALL    C0235
        NOP
        CALL    R0312                   ; GET_P0
        CP      00H
        JR      NZ,J021E
        PUSH    AF
        CALL    R031C                   ; GET_P1
        PUSH    AF
        CALL    R0326                   ; GET_P2
        PUSH    AF
        LD      A,(P2_TPA)
        CALL    R0320                   ; PUT_P2
        LD      A,(P1_TPA)
        CALL    R0316                   ; PUT_P1
        LD      A,(P0_TPA)
        CALL    R030C                   ; PUT_P0
        CALL    KEYINT
        DI
        POP     AF
        CALL    R0320                   ; PUT_P2
        POP     AF
        CALL    R0316                   ; PUT_P1
        POP     AF
        CALL    R030C                   ; PUT_P0
        JR      J0221

J021E:  CALL    C0038
J0221:  JP      R0254

Q_0224: POP     AF

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C0225:  PUSH    BC
        PUSH    DE
        LD      E,A
        IN      A,(0A8H)
        LD      B,A
        AND     3FH     ; "?"
        OR      00H
        CALL    SSLOTE
        POP     DE
        POP     BC
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C0235:  DI
        JR      J0248

Q_0238: LD      A,(0)
        LD      (SS_TEMP),A
        AND     0FCH
        OR      00H
        LD      (0),A
        CALL    C0225
J0248:  IN      A,(0A8H)
        AND     0FCH
        OR      00H
        OUT     (0A8H),A
        LD      A,(SS_TEMP)
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

R0254:  DI
        JR      J0268

Q_0257: LD      A,(LFFFF)
        CPL
        LD      (SS_TEMP),A
        AND     0FCH
        OR      00H
        LD      (0),A
        LD      (LFFFF),A
J0268:  IN      A,(0A8H)
        AND     0FCH
        OR      00H
        OUT     (0A8H),A
        LD      A,(SS_TEMP)
        RET

Q_0274: RET
Q_0275: DEFB    0,0
        LD      (LFFFF),A
        RET

;         Subroutine ALL_SEG handler
;            Inputs  ________________________
;            Outputs ________________________

R027B:  PUSH    HL
        LD      HL,C005C
        JR      J0285

;         Subroutine FRE_SEG handler
;            Inputs  ________________________
;            Outputs ________________________

R0281:  PUSH    HL
        LD      HL,C005F
J0285:  PUSH    DE
        CALL    GO_BIOS
        POP     DE
        POP     HL
        RET

;         Subroutine RD_SEG handler
;            Inputs  ________________________
;            Outputs ________________________

R028C:  DI
        PUSH    HL
        PUSH    BC
        LD      B,A
        CALL    R0326                   ; GET_P2
        LD      C,A
        LD      A,B
        CALL    R0320                   ; PUT_P2
        RES     6,H
        SET     7,H
        LD      B,(HL)
        LD      A,C
        CALL    R0320                   ; PUT_P2
        LD      A,B
        POP     BC
        POP     HL
        RET

;         Subroutine WR_SEG handler
;            Inputs  ________________________
;            Outputs ________________________

R02A5:  DI
        PUSH    HL
        PUSH    BC
        LD      B,A
        CALL    R0326                   ; GET_P2
        LD      C,A
        LD      A,B
        CALL    R0320                   ; PUT_P2
        RES     6,H
        SET     7,H
        LD      (HL),E
        LD      A,C
        CALL    R0320                   ; PUT_P2
        POP     BC
        POP     HL
        RET

;         Subroutine CALLS handler
;            Inputs  ________________________
;            Outputs ________________________

R02BD:  EXX
        EX      (SP),HL
        LD      D,(HL)
        INC     HL
        PUSH    DE
        POP     IY
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        PUSH    DE
        POP     IX
        EX      (SP),HL
        EXX

;         Subroutine CAL_SEG handler
;            Inputs  ________________________
;            Outputs ________________________

R02CD:  EXX
        EX      AF,AF'
        PUSH    IX
        POP     HL
        CALL    R02FC                   ; GET_PH
        PUSH    AF
        PUSH    HL
        PUSH    IY
        POP     AF
        CALL    R02EC                   ; PUT_PH
        EX      AF,AF'
        EXX
        CALL    CLPRM1
        EXX
        EX      AF,AF'
        POP     HL
        POP     AF
        CALL    R02EC                   ; PUT_PH
        EX      AF,AF'
        EXX
        RET

;         Subroutine PUT_PH handler
;            Inputs  ________________________
;            Outputs ________________________

R02EC:  BIT     7,H
        JR      NZ,J02F6
        BIT     6,H
        JR      Z,R030C                 ; PUT_P0
        JR      R0316                   ; PUT_P1

J02F6:  BIT     6,H
        JR      Z,R0320                 ; PUT_P2
        JR      R032D                   ; PUT_P3

;         Subroutine GET_PH handler
;            Inputs  ________________________
;            Outputs ________________________

R02FC:  BIT     7,H
        JR      NZ,J0306
        BIT     6,H
        JR      Z,R0312                 ; GET_P0
        JR      R031C                   ; GET_P1

J0306:  BIT     6,H
        JR      Z,R0326                 ; GET_P2
        JR      R032A                   ; GET_P3

;         Subroutine PUT_P0 handler
;            Inputs  ________________________
;            Outputs ________________________

R030C:  LD      (P0_SEG),A
        OUT     (0FCH),A
        RET

;         Subroutine GET_P0 handler
;            Inputs  ________________________
;            Outputs ________________________

R0312:  LD      A,(P0_SEG)
        RET

;         Subroutine PUT_P1 handler
;            Inputs  ________________________
;            Outputs ________________________

R0316:  LD      (P1_SEG),A
        OUT     (0FDH),A
        RET

;         Subroutine GET_P1 handler
;            Inputs  ________________________
;            Outputs ________________________

R031C:  LD      A,(P1_SEG)
        RET

;         Subroutine PUT_P2 handler
;            Inputs  ________________________
;            Outputs ________________________

R0320:  LD      (P2_SEG),A
        OUT     (0FEH),A
        RET

;         Subroutine GET_P2 handler
;            Inputs  ________________________
;            Outputs ________________________

R0326:  LD      A,(P2_SEG)
        RET

;         Subroutine GET_P3 handler
;            Inputs  ________________________
;            Outputs ________________________

R032A:  LD      A,(P3_SEG)

;         Subroutine PUT_P3 handler
;            Inputs  ________________________
;            Outputs ________________________

R032D:  RET

R032E:  RES     6,D
        JR      J0334

R0332:  SET     6,D
J0334:  DI
        LD      (DS0369),HL
        EX      DE,HL
        LD      (DS036C),HL
        LD      C,A
        LD      B,D
        CALL    C0413
        BIT     7,C
        JR      Z,C0361
        CALL    C03DF
        PUSH    DE
        PUSH    BC
        PUSH    AF
        CALL    C0361
        LD      C,A
        POP     AF
        POP     DE
        LD      B,D
        EX      (SP),HL
        JR      NZ,J0359
        LD      A,E
        LD      (LFFFF),A
J0359:  CALL    NZ,SSLOTE
        LD      (HL),E
        POP     HL
        LD      A,C
        LD      E,C
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C0361:  IN      A,(0A8H)
        LD      B,A
        AND     (HL)
        INC     HL
        OR      (HL)
        DI
DS0368: LD      HL,0
DS0369  EQU     DS0368+1		; rem: replaces z80asm erronous $ in phased/relocated code 
DS036B: LD      DE,0
DS036C  EQU     DS036B+1		; rem: replaces z80asm erronous $ in phased/relocated code
        BIT     6,D
        LD      D,B
        JP      NZ,WRPRIM
        CALL    RDPRIM
        LD      A,E
        RET

R0379:  EXX
        EX      AF,AF'
        POP     HL
        LD      A,(HL)
        INC     HL
        LD      C,(HL)
        INC     HL
        LD      B,(HL)
        INC     HL
        PUSH    HL
        PUSH    BC
        POP     IX
        JR      J0390

R0388:  EXX
        EX      AF,AF'
        PUSH    IX
        POP     BC
        PUSH    IY
        POP     AF
J0390:  LD      C,A
        CALL    C0413
        BIT     7,C
        JR      Z,C03BF
        CALL    C03DF
        PUSH    BC
        PUSH    DE
        PUSH    AF
        CALL    C03BF
        EXX
        EX      AF,AF'
        POP     BC
        POP     HL
        LD      A,I
        DI
        POP     DE
        PUSH    AF                      ; store IFF2 flag
        LD      A,B
        CP      D
        LD      B,D
        JR      NZ,J03B3
        LD      A,E
        LD      (LFFFF),A
J03B3:  CALL    NZ,SSLOTE
        LD      (HL),E
        POP     AF                      ; restore IFF2 flag
        JP      PO,J03BC                ; maskable interrupts where disabled, leave disabled
        EI
J03BC:  EX      AF,AF'
        EXX
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C03BF:  IN      A,(0A8H)
        PUSH    AF
        AND     (HL)
        INC     HL
        OR      (HL)
        EXX
        DI
        JP      CLPRIM

R03CA:  PUSH    HL
        LD      C,A
        LD      B,H
        CALL    C0413
        BIT     7,C
        CALL    NZ,C03DF
        IN      A,(0A8H)
        AND     (HL)
        INC     HL
        OR      (HL)
        DI
        OUT     (0A8H),A
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C03DF:  PUSH    HL
        AND     03H     ; 3 
        LD      E,A
        LD      HL,SLTTBL
        ADD     HL,DE
        PUSH    HL
        LD      A,C
        RRCA
        RRCA
        LD      C,A
        CALL    C0413
        DI
        POP     DE
        LD      A,(DE)
        LD      (SS_TEMP),A
        AND     (HL)
        INC     HL
        OR      (HL)
        LD      (DE),A
        LD      L,A
        IN      A,(0A8H)
        LD      B,A
        XOR     C
        AND     3FH     ; "?"
        XOR     C
        LD      H,A
        CP      B
        JR      NZ,J0409
        LD      A,L
        LD      (LFFFF),A
J0409:  CALL    NZ,SSLOTL
        LD      A,(SS_TEMP)
        LD      C,A
        LD      A,H
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C0413:  AND     03H     ; 3 
        LD      D,A
        LD      A,B
        RRCA
        RRCA
        RRCA
        RRCA
        AND     0CH     ; 12 
        OR      D
        LD      E,A
        LD      D,00H
        LD      HL,I0427
        ADD     HL,DE
        ADD     HL,DE
        RET

I0427:  DEFB    0FCH,000H,0FCH,001H,0FCH,002H,0FCH,003H
        DEFB    0F3H,000H,0F3H,004H,0F3H,008H,0F3H,00CH
        DEFB    0CFH,000H,0CFH,010H,0CFH,020H,0CFH,030H
        DEFB    03FH,000H,03FH,040H,03FH,080H,03FH,0C0H

        DEPHASE

; table with pointers to jump entries 0F1D3-F22D

I485F:  DEFW    R0045                   ; F1D3
        DEFW    R0144                   ; F1D6
        DEFW    R0154                   ; F1D9
        DEFW    R0185                   ; F1DC
        DEFW    R007B                   ; F1DF
        DEFW    R0073                   ; F1E2
        DEFW    R01BA                   ; F1E5
        DEFW    R032E                   ; F1E8
        DEFW    R0332                   ; F1EB
        DEFW    R0388                   ; F1EE
        DEFW    R03CA                   ; F1F1
        DEFW    R0379                   ; F1F4
        DEFW    R0103                   ; F1F7
        DEFW    R00E2                   ; F1FA
        DEFW    R0254                   ; F1FD
        DEFW    R027B                   ; F200
        DEFW    R0281                   ; F203
        DEFW    R028C                   ; F206
        DEFW    R02A5                   ; F209
        DEFW    R02CD                   ; F20C
        DEFW    R02BD                   ; F20F
        DEFW    R02EC                   ; F212
        DEFW    R02FC                   ; F215
        DEFW    R030C                   ; F218
        DEFW    R0312                   ; F21B
        DEFW    R0316                   ; F21E
        DEFW    R031C                   ; F221
        DEFW    R0320                   ; F224
        DEFW    R0326                   ; F227
        DEFW    R032D                   ; F22A
        DEFW    R032A                   ; F22D
        DEFW    0

I489F:  DEFW    001DH
        DEFW    007EH
        DEFW    008EH
        DEFW    0095H
        DEFW    009CH
        DEFW    00A0H
        DEFW    00AEH
        DEFW    00B2H
        DEFW    00C3H
        DEFW    00D2H
        DEFW    00D5H
        DEFW    00DDH
        DEFW    00F8H
        DEFW    00FEH
        DEFW    0124H
        DEFW    012AH
        DEFW    0130H
        DEFW    0139H
        DEFW    013FH
        DEFW    0147H
        DEFW    014FH
        DEFW    0157H
        DEFW    015EH
        DEFW    0164H
        DEFW    016DH
        DEFW    0173H
        DEFW    0179H
        DEFW    017FH
        DEFW    0186H
        DEFW    018DH
        DEFW    0196H
        DEFW    01A2H
        DEFW    01ADH
        DEFW    01B0H
        DEFW    01B6H
        DEFW    01D6H
        DEFW    01DFH
        DEFW    01E7H
        DEFW    01EBH
        DEFW    01F3H
        DEFW    01F7H
        DEFW    01FEH
        DEFW    0204H
        DEFW    020AH
        DEFW    0212H
        DEFW    0216H
        DEFW    021AH
        DEFW    0222H
        DEFW    0246H
        DEFW    0291H
        DEFW    0296H
        DEFW    029FH
        DEFW    02AAH
        DEFW    02AFH
        DEFW    02B8H
        DEFW    02D3H
        DEFW    02DBH
        DEFW    02E7H
        DEFW    0336H
        DEFW    033AH
        DEFW    033FH
        DEFW    0346H
        DEFW    034CH
        DEFW    0392H
        DEFW    0399H
        DEFW    039FH
        DEFW    03B9H
        DEFW    03CEH
        DEFW    03D3H
        DEFW    03EDH
        DEFW    0422H
        DEFW    0

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C492F:  CALL    C49D7
        RET     C
        EX      DE,HL
        LD      HL,KBUF+32
        SBC     HL,DE
        ADD     HL,HL
        ADD     HL,HL
        INC     HL
        PUSH    DE
        CALL    C4B68
        POP     DE
        RET     C
        LD      (MAP_TAB),HL
        EX      DE,HL
J4946:  LD      A,(HL)
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
J4954:  LD      (DE),A
        INC     DE
        DJNZ    J4954
        JR      J4946
J495A:  LD      (DE),A
        LD      HL,(MAP_TAB)
        LD      A,(HL)                  ; slotid primairy memory mapper
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
        LD      (HL),4+1+1              ; 6 reserved segments primairy memory mapper
        LD      DE,P0_64K
        LD      HL,P0_SEG
        LD      A,4-1
J498B:  LD      (DE),A
        INC     DE
        LD      (HL),A
        INC     HL
        DEC     A
        JP      P,J498B
        LD      A,(DATA_S)              ; BDOS data segment
        OUT     (0FEH),A
        LD      HL,ISBBFF
J499B:  LD      (HL),00H
        DEC     HL
        BIT     7,H
        JR      NZ,J499B
        LD      HL,ISB062
        LD      (D_B064),HL
        LD      HL,(MAP_TAB)
        LD      A,(HL)                  ; slotid primairy memory mapper
        INC     HL
        PUSH    HL
        CALL    C4AD0
        DEC     HL
        DEC     (HL)
        DEC     HL
        DEC     (HL)
        EX      DE,HL
        LD      B,4
J49B8:  DEC     (HL)
        INC     HL
        DJNZ    J49B8
        POP     HL
        LD      BC,7
        ADD     HL,BC
J49C1:  LD      A,(HL)
        OR      A
        JR      Z,J49D1
        INC     HL
        PUSH    HL
        CALL    C4AD0
        POP     HL
        LD      BC,7
        ADD     HL,BC
        JR      J49C1
J49D1:  LD      A,01H   ; 1 
        OUT     (0FEH),A
        OR      A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49D7:  DI
        PUSH    AF
        LD      HL,KBUF+32
        XOR     A
        LD      (HL),A                  ; empty memory mapper list
        PUSH    HL
        LD      HL,EXPTBL+0
J49E2:  BIT     7,(HL)
        JR      Z,J49E8
        SET     7,A
J49E8:  PUSH    HL
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
J4A05:  DEC     HL
        LD      (HL),B
        DEC     HL
        LD      (HL),C
        POP     BC
        EX      (SP),HL
        PUSH    HL
        PUSH    BC
J4A0D:  POP     AF
        POP     HL
        BIT     7,A
        JR      Z,J4A19
        ADD     A,4
        BIT     4,A
        JR      Z,J49E8
J4A19:  INC     HL
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

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A34:  LD      HL,08000H
        LD      B,(HL)
        LD      (HL),0AAH
        XOR     A
        OUT     (0FEH),A
        LD      C,(HL)
        LD      (HL),55H        ; "U"
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
J4A58:  LD      A,B
        OUT     (0FEH),A
        LD      A,(HL)
        PUSH    AF
        INC     SP
        LD      (HL),0AAH
        INC     B
        JR      NZ,J4A58
J4A63:  LD      A,B
        OUT     (0FEH),A
        LD      A,(HL)
        CP      0AAH
        JR      NZ,J4A75
        LD      A,55H   ; "U"
        LD      (HL),A
        CP      (HL)
        JR      NZ,J4A75
        INC     B
        JR      NZ,J4A63
        DEC     B
J4A75:  LD      C,00H
J4A77:  LD      A,C
        DEC     A
        OUT     (0FEH),A
        DEC     SP
        POP     AF
        LD      (HL),A
        DEC     C
        JR      NZ,J4A77
J4A81:  LD      A,01H   ; 1 
        OUT     (0FEH),A
        LD      A,B
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A87:  LD      A,(HL)
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
        AND     03H     ; 3 
        LD      C,A
        LD      B,00H
        LD      HL,SLTTBL
        ADD     HL,BC
        LD      A,(LFFFF)
        CPL
        LD      (HL),A
J4ACF:  RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4AD0:  EX      DE,HL
        AND     0FH     ; 15 
        ADD     A,A
        ADD     A,A
        LD      C,A
        LD      B,00H
        LD      HL,ISBA35
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

J4AF7:  PUSH    DE
        PUSH    AF
        CALL    C4AFF
        POP     AF
        POP     DE
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4AFF:  LD      A,D
        OR      E                       ; broadcast + function: build device name table ?
        JR      NZ,J4B0B                ; nope,
        LD      A,4
        CALL    C4B5C                   ; memory mapper device id
        JP      C4B5C                   ; reserved byte
J4B0B:  LD      A,D
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

J4B1A:  PUSH    HL
        LD      HL,(MAP_TAB)
        LD      C,(HL)                  ; slotid primairy memory mapper
        INC     HL
        LD      D,(HL)                  ; number of segments in primairy memory mapper
        INC     HL
        LD      E,(HL)                  ; number of free segments in primairy memory mapper
        POP     HL
        LD      A,C
        CALL    C4B5C                   ; slotid
        LD      A,MAP_VE % 256		; rem: LOW MAP_VE
        CALL    C4B5C
        LD      A,MAP_VE / 256		; rem: HIGH MAP_VE
        CALL    C4B5C                   ; memory mapper jump table
        LD      A,E
        CALL    C4B5C                   ; number of free segments in primairy memory mapper
        LD      A,D
        CALL    C4B5C                   ; number of segments in primairy memory mapper
        CALL    C4B5C                   ; reserved byte
        CALL    C4B5C                   ; reserved byte
        JP      C4B5C                   ; reserved byte

; Function 1: Get mapper variable table

J4B43:  POP     DE
        POP     AF
        LD      HL,(MAP_TAB)
        LD      A,(HL)                  ; slotid primairy memory mapper
        PUSH    AF
        PUSH    DE
        RET

; Function 2: Get mapper support routine address

J4B4C:  POP     DE
        POP     AF
        LD      HL,(MAP_TAB)
        LD      B,(HL)                  ; slotid primairy memory mapper
        INC     HL
        LD      A,(HL)                  ; number of segments in primairy memory mapper
        INC     HL
        LD      C,(HL)                  ; number of free segments in primairy memory mapper
        LD      HL,MAP_VE               ; memory mapper jump table
        PUSH    AF
        PUSH    DE
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4B5C:  PUSH    BC
        PUSH    DE
        LD      E,A
        LD      A,B
        CALL    WRSLT
        POP     DE
        POP     BC
        INC     HL
        XOR     A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4B68:  LD      A,L
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
        CP      02H     ; 2 
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
Q4BC0:  LD      A,1
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
J4BE5:  LD      (STKTOP),HL
        DEC     HL
        DEC     HL
        LD      (SAVSTK),HL
        LD      L,E
        LD      H,D
        INC     HL
        INC     HL
        INC     HL
        INC     HL
        LD      A,02H   ; 2 
J4BF5:  EX      DE,HL
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

J4C09:  LD      HL,I4D0E
        CALL    C4C21
        RET     Z
        PUSH    AF
        CP      64
        LD      A,11 
        JR      NC,J4C19
        LD      A,12 
J4C19:  CALL    C4C1E
        POP     AF
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C1E:  LD      HL,I4C4A


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C21:  PUSH    BC
        PUSH    DE
        LD      E,A
        LD      A,(KANJTA+0)
        OR      A
        JR      Z,J4C2F
        DEC     HL
        LD      A,(HL)
        DEC     HL
        LD      L,(HL)
        LD      H,A
J4C2F:  LD      A,(HL)
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
J4C3F:  POP     DE
        PUSH    DE
        LDIR
        EX      DE,HL
J4C44:  POP     DE
        POP     BC
        OR      A
        RET

        DEFW    L0000
I4C4A:  
 	DOSST1  1,"Not enough memory"
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

        DEFW    L0001
I4D0E:
	DOSST1  0FFH,"Incompatible disk"
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

L0000:
	DEFB	1,$13,$83,$81,$83,$82,$83,$8A,$82,$AA,$91,$AB,$82,$E8,$82,$DC,$82,$B9,$82,$F1,$00
        DEFB	2,$10,$83,$68,$83,$89,$83,$43,$83,$75,$96,$BC,$82,$CD,$3F,$20,$28,$00
        DOSST1  3,") "
        DEFB	4,$18,$89,$BD,$82,$A9,$83,$4C,$81,$5B,$82,$F0,$89,$9F,$82,$B5,$82,$C4,$89,$BA,$82,$B3,$82,$A2,$20,$00
        DEFB	5,$11,$91,$C5,$82,$BF,$90,$D8,$82,$E7,$82,$EA,$82,$DC,$82,$B5,$82,$BD,$00
        DEFB	6,$1B,$83,$74,$83,$48,$81,$5B,$83,$7D,$83,$62,$83,$67,$82,$AA,$8A,$AE,$97,$B9,$82,$B5,$82,$DC,$82,$B5,$82,$BD,$00
        DEFB	7,$09,$83,$68,$83,$89,$83,$43,$83,$75,$00
	DEFB	8,$16,$3A,$97,$70,$82,$CC,$83,$66,$83,$42,$83,$58,$83,$4E,$82,$F0,$93,$FC,$82,$EA,$82,$C4,$00
	DEFB  	9,$17,$89,$BD,$82,$A9,$83,$4C,$81,$5B,$82,$F0,$89,$9F,$82,$B5,$82,$C4,$89,$BA,$82,$B3,$82,$A2,$00
        DOSST1  10,"*** "
        DEFB  	11,$10,$83,$56,$83,$58,$83,$65,$83,$80,$83,$47,$83,$89,$81,$5B,$20,$00
        DEFB	12,$10,$83,$86,$81,$5B,$83,$55,$81,$5B,$83,$47,$83,$89,$81,$5B,$20,$00
        DEFB    0

L0001:
        DEFB  	0FFH,$1D,$82,$B1,$82,$CC,$83,$66,$83,$42,$83,$58,$83,$4E,$82,$CD,$8E,$67,$97,$70,$82,$C5,$82,$AB,$82,$DC,$82,$B9,$82,$F1,$00
        DEFB  	0FEH,$11,$8F,$91,$82,$AB,$8D,$9E,$82,$DD,$88,$D9,$8F,$ED,$82,$C5,$82,$B7,$00
        DEFB  	0FDH,$13,$83,$66,$83,$42,$83,$58,$83,$4E,$82,$AA,$88,$D9,$8F,$ED,$82,$C5,$82,$B7,$00
        DEFB  	0FCH,$19,$83,$66,$83,$42,$83,$58,$83,$4E,$82,$AA,$93,$FC,$82,$C1,$82,$C4,$82,$A2,$82,$DC,$82,$B9,$82,$F1,$00
        DEFB  	0FBH,$1D,$90,$B3,$82,$B5,$82,$AD,$8F,$91,$82,$AB,$8D,$9E,$82,$DC,$82,$EA,$82,$DC,$82,$B9,$82,$F1,$82,$C5,$82,$B5,$82,$BD,$00
        DEFB  	0FAH,$1B,$83,$66,$83,$42,$83,$58,$83,$4E,$82,$CC,$83,$66,$81,$5B,$83,$5E,$82,$AA,$88,$D9,$8F,$ED,$82,$C5,$82,$B7,$00
        DEFB  	0F9H,$19,$83,$5A,$83,$4E,$83,$5E,$81,$5B,$82,$AA,$8C,$A9,$82,$C2,$82,$A9,$82,$E8,$82,$DC,$82,$B9,$82,$F1,$00
        DEFB    0F8H
        DEFB    23H,83H,66H,83H,42H,83H,58H,83H,4EH,82H,0AAH,8FH
        DEFB    91H,82H,0ABH,8DH,9EH,82H,0DDH,95H,0DBH,8CH,0ECH,82H
        DEFB    0B3H,82H,0EAH,82H,0C4H,82H,0A2H,82H,0DCH,82H,0B7H
        DEFB    0
        DEFB    0F7H,25H,83H,66H
        DEFB    83H,42H,83H,58H,83H,4EH,82H,0AAH
        DEFB    83H,74H,83H,48H,81H,5BH,83H,7DH
        DEFB    83H,62H,83H,67H,82H,0B3H,82H,0EAH
        DEFB    82H,0C4H,82H,0A2H,82H,0DCH,82H,0B9H
        DEFB    82H,0F1H,00H,0F6H,1EH,4DH,53H,58H
        DEFB    2DH,44H,4FH,53H,83H,66H,83H,42H
        DEFB    83H,58H,83H,4EH,82H,0C5H,82H,0CDH
        DEFB    82H,0A0H,82H,0E8H,82H,0DCH,82H,0B9H
        DEFB    82H,0F1H,00H,0F5H,13H,83H,66H,83H
        DEFB    42H,83H,58H,83H,4EH,82H,0AAH,88H
        DEFB    0E1H,82H,0A2H,82H,0DCH,82H,0B7H,00H
        DEFB    0F4H,27H,82H,0B1H,82H,0CCH,83H,74H
        DEFB    83H,40H,83H,43H,83H,8BH,97H,70H
        DEFB    82H,0CCH,83H,66H,83H,42H,83H,58H
        DEFB    83H,4EH,82H,0C5H,82H,0CDH,82H,0A0H
        DEFB    82H,0E8H,82H,0DCH,82H,0B9H,82H,0F1H
        DEFB    00H,0F3H,11H,83H,56H,81H,5BH,83H
        DEFB    4EH,83H,47H,83H,89H,81H,5BH,82H
        DEFB    0C5H,82H,0B7H,00H,0F2H,0CH,46H,41H
        DEFB    54H,88H,0D9H,8FH,0EDH,82H,0C5H,82H
        DEFB    0B7H,00H,0F0H,25H,82H,0B1H,82H,0CCH
        DEFB    83H,68H,83H,89H,83H,43H,83H,75H
        DEFB    82H,0CDH,83H,74H,83H,48H,81H,5BH
        DEFB    83H,7DH,83H,62H,83H,67H,82H,0C5H
        DEFB    82H,0ABH,82H,0DCH,82H,0B9H,82H,0F1H
        DEFB    00H,0DFH,0EH,44H,4FH,53H,82H,0AAH
        DEFB    88H,0D9H,8FH,0EDH,82H,0C5H,82H,0B7H
        DEFB    00H,0DEH,11H,83H,81H,83H,82H,83H
        DEFB    8AH,81H,5BH,95H,73H,91H,0ABH,82H
        DEFB    0C5H,82H,0B7H,00H,0DCH,24H,96H,0B3H
        DEFB    8CH,0F8H,82H,0C8H,4DH,53H,58H,2DH
        DEFB    44H,4FH,53H,83H,74H,83H,40H,83H
        DEFB    93H,83H,4EH,83H,56H,83H,87H,83H
        DEFB    93H,94H,0D4H,8DH,86H,82H,0C5H,82H
        DEFB    0B7H,00H,0DBH,15H,96H,0B3H,8CH,0F8H
        DEFB    82H,0C8H,83H,68H,83H,89H,83H,43H
        DEFB    83H,75H,96H,0BCH,82H,0C5H,82H,0B7H
        DEFB    00H,0DAH,15H,95H,73H,90H,0B3H,82H
        DEFB    0C8H,83H,74H,83H,40H,83H,43H,83H
        DEFB    8BH,96H,0BCH,82H,0C5H,82H,0B7H,00H
        DEFB    0D9H,11H,96H,0B3H,8CH,0F8H,82H,0C8H
        DEFB    83H,70H,83H,58H,96H,0BCH,82H,0C5H
        DEFB    82H,0B7H,00H,0D8H,13H,83H,70H,83H
        DEFB    58H,96H,0BCH,82H,0AAH,92H,0B7H,89H
        DEFB    0DFH,82H,0ACH,82H,0DCH,82H,0B7H,00H
        DEFB    0D7H,19H,83H,74H,83H,40H,83H,43H
        DEFB    83H,8BH,82H,0AAH,8CH,0A9H,82H,0C2H
        DEFB    82H,0A9H,82H,0E8H,82H,0DCH,82H,0B9H
        DEFB    82H,0F1H,00H,0D6H,1DH,83H,66H,83H
        DEFB    42H,83H,8CH,83H,4EH,83H,67H,83H
        DEFB    8AH,82H,0AAH,8CH,0A9H,82H,0C2H,82H
        DEFB    0A9H,82H,0E8H,82H,0DCH,82H,0B9H,82H
        DEFB    0F1H,00H,0D5H,21H,83H,8BH,81H,5BH
        DEFB    83H,67H,83H,66H,83H,42H,83H,8CH
        DEFB    83H,4EH,83H,67H,83H,8AH,82H,0AAH
        DEFB    82H,0A2H,82H,0C1H,82H,0CFH,82H,0A2H
        DEFB    82H,0C5H,82H,0B7H,00H,0D4H,17H,83H
        DEFB    66H,83H,42H,83H,58H,83H,4EH,82H
        DEFB    0AAH,82H,0A2H,82H,0C1H,82H,0CFH,82H
        DEFB    0A2H,82H,0C5H,82H,0B7H,00H,0D3H,1BH
        DEFB    83H,74H,83H,40H,83H,43H,83H,8BH
        DEFB    96H,0BCH,82H,0AAH,8FH,64H,95H,0A1H
        DEFB    82H,0B5H,82H,0C4H,82H,0A2H,82H,0DCH
        DEFB    82H,0B7H,00H,0D2H,1DH,83H,66H,83H
        DEFB    42H,83H,8CH,83H,4EH,83H,67H,83H
        DEFB    8AH,82H,0AAH,88H,0DAH,93H,0AEH,82H
        DEFB    0C5H,82H,0ABH,82H,0DCH,82H,0B9H,82H
        DEFB    0F1H,00H,0D1H,1BH,83H,74H,83H,40H
        DEFB    83H,43H,83H,8BH,82H,0AAH,93H,0C7H
        DEFB    82H,0DDH,8FH,6FH,82H,0B5H,90H,0EAH
        DEFB    97H,70H,82H,0C5H,82H,0B7H,00H,0D0H
        DEFB    1FH,83H,66H,83H,42H,83H,8CH,83H
        DEFB    4EH,83H,67H,83H,8AH,82H,0AAH,8BH
        DEFB    0F3H,82H,0C5H,82H,0CDH,82H,0A0H,82H
        DEFB    0E8H,82H,0DCH,82H,0B9H,82H,0F1H,00H
        DEFB    0CFH,0FH,96H,0B3H,8CH,0F8H,82H,0C8H
        DEFB    91H,0AEH,90H,0ABH,82H,0C5H,82H,0B7H
        DEFB    00H,0CEH,1EH,2EH,82H,0E2H,2EH,2EH
        DEFB    82H,0C9H,91H,0CEH,82H,0B5H,82H,0C4H
        DEFB    82H,0CDH,91H,80H,8DH,0ECH,82H,0C5H
        DEFB    82H,0ABH,82H,0DCH,82H,0B9H,82H,0F1H
        DEFB    00H,0CDH,1FH,83H,56H,83H,58H,83H
        DEFB    65H,83H,80H,83H,74H,83H,40H,83H
        DEFB    43H,83H,8BH,82H,0AAH,8AH,0F9H,82H
        DEFB    0C9H,82H,0A0H,82H,0E8H,82H,0DCH,82H
        DEFB    0B7H,00H,0CCH,1BH,83H,66H,83H,42H
        DEFB    83H,8CH,83H,4EH,83H,67H,83H,8AH
        DEFB    82H,0AAH,8AH,0F9H,82H,0C9H,82H,0A0H
        DEFB    82H,0E8H,82H,0DCH,82H,0B7H,00H,0CBH
        DEFB    17H,83H,74H,83H,40H,83H,43H,83H
        DEFB    8BH,82H,0AAH,8AH,0F9H,82H,0C9H,82H
        DEFB    0A0H,82H,0E8H,82H,0DCH,82H,0B7H,00H
        DEFB    0CAH,15H,83H,74H,83H,40H,83H,43H
        DEFB    83H,8BH,82H,0AAH,8EH,67H,97H,70H
        DEFB    92H,86H,82H,0C5H,82H,0B7H,00H,0C9H
        DEFB    1CH,36H,34H,4BH,82H,0F0H,89H,7AH
        DEFB    82H,0A6H,82H,0E9H,93H,5DH,91H,97H
        DEFB    82H,0CDH,82H,0C5H,82H,0ABH,82H,0DCH
        DEFB    82H,0B9H,82H,0F1H,00H,0C8H,17H,83H
        DEFB    74H,83H,40H,83H,43H,83H,8BH,82H
        DEFB    0CCH,8AH,84H,93H,96H,88H,0D9H,8FH
        DEFB    0EDH,82H,0C5H,82H,0B7H,00H,0C7H,15H
        DEFB    83H,74H,83H,40H,83H,43H,83H,8BH
        DEFB    82H,0CCH,8FH,49H,82H,0EDH,82H,0E8H
        DEFB    82H,0C5H,82H,0B7H,00H,0C6H,19H,83H
        DEFB    74H,83H,40H,83H,43H,83H,8BH,83H
        DEFB    41H,83H,4EH,83H,5AH,83H,58H,88H
        DEFB    0D9H,8FH,0EDH,82H,0C5H,82H,0B7H,00H
        DEFB    0C5H,15H,96H,0B3H,8CH,0F8H,82H,0C8H
        DEFB    83H,76H,83H,8DH,83H,5AH,83H,58H
        DEFB    49H,44H,82H,0C5H,82H,0B7H,00H,0C4H
        DEFB    1DH,83H,74H,83H,40H,83H,43H,83H
        DEFB    8BH,83H,6EH,83H,93H,83H,68H,83H
        DEFB    8BH,82H,0AAH,91H,0ABH,82H,0E8H,82H
        DEFB    0DCH,82H,0B9H,82H,0F1H,00H,0C3H,1BH
        DEFB    96H,0B3H,8CH,0F8H,82H,0C8H,83H,74H
        DEFB    83H,40H,83H,43H,83H,8BH,83H,6EH
        DEFB    83H,93H,83H,68H,83H,8BH,82H,0C5H
        DEFB    82H,0B7H,00H,0C2H,29H,83H,74H,83H
        DEFB    40H,83H,43H,83H,8BH,83H,6EH,83H
        DEFB    93H,83H,68H,83H,8BH,82H,0AAH,83H
        DEFB    49H,81H,5BH,83H,76H,83H,93H,82H
        DEFB    0B3H,82H,0EAH,82H,0C4H,82H,0A2H,82H
        DEFB    0DCH,82H,0B9H,82H,0F1H,00H,0C1H,21H
        DEFB    96H,0B3H,8CH,0F8H,82H,0C8H,83H,66H
        DEFB    83H,6FH,83H,43H,83H,58H,83H,49H
        DEFB    83H,79H,83H,8CH,81H,5BH,83H,56H
        DEFB    83H,87H,83H,93H,82H,0C5H,82H,0B7H
        DEFB    00H,0C0H,13H,96H,0B3H,8CH,0F8H,82H
        DEFB    0C8H,8AH,0C2H,8BH,0ABH,95H,0CFH,90H
        DEFB    94H,82H,0C5H,82H,0B7H,00H,0BFH,15H
        DEFB    8AH,0C2H,8BH,0ABH,95H,0CFH,90H,94H
        DEFB    82H,0AAH,92H,0B7H,89H,0DFH,82H,0ACH
        DEFB    82H,0DCH,82H,0B7H,00H,0BEH,11H,96H
        DEFB    0B3H,8CH,0F8H,82H,0C8H,93H,0FAH,95H
        DEFB    74H,82H,0AFH,82H,0C5H,82H,0B7H,00H
        DEFB    0BDH,0FH,96H,0B3H,8CH,0F8H,82H,0C8H
        DEFB    8EH,9EH,8AH,0D4H,82H,0C5H,82H,0B7H
        DEFB    00H,0BCH,23H,52H,41H,4DH,20H,44H
        DEFB    49H,53H,4BH,28H,83H,68H,83H,89H
        DEFB    83H,43H,83H,75H,48H,3AH,29H,82H
        DEFB    0CDH,8AH,0F9H,82H,0C9H,82H,0A0H,82H
        DEFB    0E8H,82H,0DCH,82H,0B7H,00H,0BBH,15H
        DEFB    52H,41H,4DH,20H,44H,49H,53H,4BH
        DEFB    82H,0AAH,82H,0A0H,82H,0E8H,82H,0DCH
        DEFB    82H,0B9H,82H,0F1H,00H,0BAH,1BH,83H
        DEFB    74H,83H,40H,83H,43H,83H,8BH,82H
        DEFB    0AAH,8FH,0C1H,8BH,8EH,82H,0B3H,82H
        DEFB    0EAH,82H,0C4H,82H,0A2H,82H,0DCH,82H
        DEFB    0B7H,00H,0B8H,21H,96H,0B3H,8CH,0F8H
        DEFB    82H,0C8H,83H,54H,83H,75H,83H,74H
        DEFB    83H,40H,83H,93H,83H,4EH,83H,56H
        DEFB    83H,87H,83H,93H,94H,0D4H,8DH,86H
        DEFB    82H,0C5H,82H,0B7H,00H,0B7H,0EH,96H
        DEFB    0B3H,8CH,0F8H,82H,0C8H,46H,43H,42H
        DEFB    82H,0C5H,82H,0B7H,00H,9FH,18H,43H
        DEFB    74H,72H,6CH,2DH,53H,54H,4FH,50H
        DEFB    82H,0AAH,89H,9FH,82H,0B3H,82H,0EAH
        DEFB    82H,0DCH,82H,0B5H,82H,0BDH,00H,9EH
        DEFB    15H,43H,74H,72H,6CH,2DH,43H,82H
        DEFB    0AAH,89H,9FH,82H,0B3H,82H,0EAH,82H
        DEFB    0DCH,82H,0B5H,82H,0BDH,00H,9DH,21H
        DEFB    83H,66H,83H,42H,83H,58H,83H,4EH
        DEFB    93H,0FCH,8FH,6FH,97H,0CDH,82H,0AAH
        DEFB    91H,0C5H,82H,0BFH,90H,0D8H,82H,0E7H
        DEFB    82H,0EAH,82H,0DCH,82H,0B5H,82H,0BDH
        DEFB    00H,9CH,1DH,95H,57H,8FH,80H,8FH
        DEFB    6FH,97H,0CDH,82H,0C5H,83H,47H,83H
        DEFB    89H,81H,5BH,82H,0AAH,8BH,4EH,82H
        DEFB    0ABH,82H,0DCH,82H,0B5H,82H,0BDH,00H
        DEFB    9BH,1DH,95H,57H,8FH,80H,93H,0FCH
        DEFB    97H,0CDH,82H,0C5H,83H,47H,83H,89H
        DEFB    81H,5BH,82H,0AAH,8BH,4EH,82H,0ABH
        DEFB    82H,0DCH,82H,0B5H,82H,0BDH,00H,8FH
        DEFB    1FH,83H,52H,83H,7DH,83H,93H,83H
        DEFB    68H,82H,0CCH,83H,6FH,81H,5BH,83H
        DEFB    57H,83H,87H,83H,93H,82H,0AAH,88H
        DEFB    0E1H,82H,0A2H,82H,0DCH,82H,0B7H,00H
        DEFB    8EH,13H,83H,52H,83H,7DH,83H,93H
        DEFB    83H,68H,82H,0AAH,88H,0E1H,82H,0A2H
        DEFB    82H,0DCH,82H,0B7H,00H,8DH,15H,83H
        DEFB    52H,83H,7DH,83H,93H,83H,68H,82H
        DEFB    0AAH,92H,0B7H,82H,0B7H,82H,0ACH,82H
        DEFB    0DCH,82H,0B7H,00H,8BH,15H,96H,0B3H
        DEFB    8CH,0F8H,82H,0C8H,83H,70H,83H,89H
        DEFB    83H,81H,81H,5BH,83H,5EH,82H,0C5H
        DEFB    82H,0B7H,00H,8AH,17H,83H,70H,83H
        DEFB    89H,83H,81H,81H,5BH,83H,5EH,82H
        DEFB    0AAH,91H,0BDH,82H,0B7H,82H,0ACH,82H
        DEFB    0DCH,82H,0B7H,00H,89H,1BH,83H,70H
        DEFB    83H,89H,83H,81H,81H,5BH,83H,5EH
        DEFB    82H,0AAH,95H,73H,91H,0ABH,82H,0B5H
        DEFB    82H,0C4H,82H,0A2H,82H,0DCH,82H,0B7H
        DEFB    00H,88H,19H,96H,0B3H,8CH,0F8H,82H
        DEFB    0C8H,83H,49H,83H,76H,83H,56H,83H
        DEFB    87H,83H,93H,8EH,77H,92H,0E8H,82H
        DEFB    0C5H,82H,0B7H,00H,87H,0FH,96H,0B3H
        DEFB    8CH,0F8H,82H,0C8H,90H,94H,92H,6CH
        DEFB    82H,0C5H,82H,0B7H,00H,86H,1DH,48H
        DEFB    45H,4CH,50H,83H,74H,83H,40H,83H
        DEFB    43H,83H,8BH,82H,0AAH,8CH,0A9H,82H
        DEFB    0C2H,82H,0A9H,82H,0E8H,82H,0DCH,82H
        DEFB    0B9H,82H,0F1H,00H,85H,1EH,4DH,53H
        DEFB    58H,2DH,44H,4FH,53H,82H,0CCH,83H
        DEFB    6FH,81H,5BH,83H,57H,83H,87H,83H
        DEFB    93H,82H,0AAH,88H,0E1H,82H,0A2H,82H
        DEFB    0DCH,82H,0B7H,00H,84H,1FH,95H,0A1H
        DEFB    8EH,0CAH,90H,0E6H,83H,74H,83H,40H
        DEFB    83H,43H,83H,8BH,82H,0CDH,8CH,8BH
        DEFB    8DH,87H,82H,0C5H,82H,0ABH,82H,0DCH
        DEFB    82H,0B9H,82H,0F1H,00H,83H,19H,83H
        DEFB    74H,83H,40H,83H,43H,83H,8BH,82H
        DEFB    0F0H,8DH,0ECH,90H,0ACH,82H,0C5H,82H
        DEFB    0ABH,82H,0DCH,82H,0B9H,82H,0F1H,00H
        DEFB    82H,1DH,8EH,0A9H,95H,0AAH,8EH,0A9H
        DEFB    90H,67H,82H,0C9H,82H,0CDH,83H,52H
        DEFB    83H,73H,81H,5BH,82H,0C5H,82H,0ABH
        DEFB    82H,0DCH,82H,0B9H,82H,0F1H,00H,81H
        DEFB    1FH,83H,74H,83H,40H,83H,43H,83H
        DEFB    8BH,82H,0CCH,8FH,64H,82H,0CBH,8FH
        DEFB    91H,82H,0ABH,82H,0AAH,82H,0C5H,82H
        DEFB    0ABH,82H,0DCH,82H,0B9H,82H,0F1H,00H
        DEFB    7FH,2AH,4DH,53H,58H,2DH,44H,4FH
        DEFB    53H,32H,20H,82H,0CCH,83H,66H,83H
        DEFB    42H,83H,58H,83H,4EH,82H,0F0H,83H
        DEFB    68H,83H,89H,83H,43H,83H,75H,20H
        DEFB    07H,3AH,20H,82H,0C9H,93H,0FCH,82H
        DEFB    0EAH,82H,0C4H,00H,7EH,18H,89H,0BDH
        DEFB    82H,0A9H,83H,4CH,81H,5BH,82H,0F0H
        DEFB    89H,9FH,82H,0B5H,82H,0C4H,89H,0BAH
        DEFB    82H,0B3H,82H,0A2H,20H,00H
        DEFB    0


        DEFS    08000H-$-S_ORG1,0FFH

        .END
