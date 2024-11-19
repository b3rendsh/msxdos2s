; p3_paging.asm
;
; DOS 2.20 kernel
; Paging helper routines
; ------------------------------------------------------------------------------
; Based on ASCII DOS 2.20 codebase
; Source re-created by Z80DIS 2.2
; Z80DIS was written by Kenneth Gielow, Palo Alto, CA
;
; Code Copyrighted by ASCII and maybe others
; Source comments by Arjen Zeilemaker
; Restructure, modifications and additional comments by H.J. Berends
;
; Sourcecode supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyrightholders
; ------------------------------------------------------------------------------
; Modifications:
; 01. Restructured paging helper routines in separate module


        INCLUDE "disk.inc"	; Assembler directives
	INCLUDE	"msx.inc"	; MSX constants and definitions

	SECTION	P3_PAGING

	; Routine used in the main module
	PUBLIC	C427F

	; Routines defined in the main module
	EXTERN	GETSLT
	EXTERN	C4B68

	; Symbols defined in the kernel module
	EXTERN	SSLOTL
	EXTERN	SSLOTE
	EXTERN	C005C
	EXTERN	C005F

; ------------------------------------------------------------------------------
; Subroutine load and patch BDOS paging helper routines in ram p3

C427F:  LD      HL,500+(I485F-I4418)	; 1595 = 200 + 300 + 1095
        CALL    C4B68
        RET     C
        LD      (HL),0
        LD      E,L
        LD      D,H
        INC     DE
        LD      BC,200			; 200 bytes for IRQ stack
        LDIR
        LD      (IRQ_ST),HL
        LD      BC,300			; 300 bytes for BDOS stack
        LDIR
        LD      (ST_BDOS),HL
        EX      DE,HL
        LD      HL,I4418
        PUSH    DE
        LD      BC,I485F-I4418		; 1095 bytes for BDOS handlers
        LDIR
        LD      BC,JUMPB		; Jump table to BDOS handlers
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
        LD      HL,I489F		; Patch table for dynamic relocated code
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
        LD      HL,Q_01ED+1		; 01EEH
        ADD     HL,DE
        LD      (HL),A			; Write P0 Code segment # to 'CP #' instruction
        LD      A,(RAMAD3)
        AND     03H
        LD      HL,Q_026C+1		; 026DH
        ADD     HL,DE
        LD      (HL),A			; Write slot# ram p3 bits 1+2 to 'OR #' instruction
        LD      C,A
        LD      B,00H			; 00 is NOP instruction
        RLCA
        RLCA
        RLCA
        RLCA
        OR      C
        LD      HL,Q_011F+1		; 0120H		
        ADD     HL,DE
        LD      (HL),A			; Write slot# ram p3 in bits 1+2 and 5+6 to 'OR #' instruction
        LD      A,(RAMAD3)
        BIT     7,A			; Expanded slot?
        JR      Z,J4356			; z=no
        LD      HL,Q_0255		; 0255H
        ADD     HL,DE
        LD      (HL),B			; Replace 'JR J0268' instruction with 'NOP'
        INC     HL
        LD      (HL),B			; "
        LD      HL,Q_00E4		; 00E4H
        ADD     HL,DE
        LD      (HL),B			; Replace 'JR J00EF' instruction with 'NOP'
        INC     HL
        LD      (HL),B			; "
        LD      HL,Q_0105		; 0105H
        ADD     HL,DE
        LD      (HL),B			; Replace 'JR J0118' instruction with 'NOP'
        INC     HL
        LD      (HL),B			; "
        LD      HL,Q_0274		; 0274H
        ADD     HL,DE
        LD      (HL),32H		; 32 is 'LD (#),A' instruction, where # is at Q_0275
        RRCA
        RRCA
        AND     03H  
        LD      HL,Q_0260+1		; 0261H
        ADD     HL,DE
        LD      (HL),A			; Write slot# ram p3 in bits 3+4 to 'OR #'instruction
        LD      L,A
        RLCA
        RLCA
        RLCA
        RLCA
        OR      L
        LD      HL,Q_0110+1		; 0111H
        ADD     HL,DE
        LD      (HL),A                  ; Write slot# ram p3 in bits 3+4 and 7+8 to 'OR #' instruction
        PUSH    DE
        LD      HL,SLTTBL
        ADD     HL,BC
        EX      DE,HL
        LD      BC,Q_0262+1		; 0263H
        ADD     HL,BC
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      BC,Q_0275-(Q_0262+2)	; 17
        ADD     HL,BC
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      BC,Q_00E9-Q_0275	; -396 = -18CH
        ADD     HL,BC			; (0275H + 1) - 18CH = 00EAH
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      BC,Q_0112-(Q_00E9+1)	; 40 = 28H
        ADD     HL,BC			; (00E9H + 1) + 28H = 0112H
        LD      (HL),E
        INC     HL
        LD      (HL),D
        POP     DE
J4356:  LD      A,(EXPTBL+0)
        AND     03H     
        LD      HL,Q_024C+1		; 024DH
        ADD     HL,DE
        LD      (HL),A			; Write slot# main bios to 'OR #' instruction
        LD      C,A
        LD      B,00H			; 00 is NOP instruction
        RRCA
        RRCA
        LD      HL,Q_022D+1		; 022EH
        ADD     HL,DE
        LD      (HL),A			; Write slot# main bios in bits 7+8 to 'OR #' instruction
        LD      A,(EXPTBL+0)
        BIT     7,A			; main bios in expanded slot?
        JR      Z,J43D1			; z=no
        LD      HL,Q_0236		; 0236H
        ADD     HL,DE
        LD      (HL),B			; Replace 'JR 0248' instruction with 'NOP'
        INC     HL
        LD      (HL),B			; "
        RRCA
        RRCA
        AND     03H    
        LD      HL,Q_0240+1		; 0241H
        ADD     HL,DE
        LD      (HL),A			; Write value # to 'OR #' instruction
        LD      A,(RAMAD3)
        XOR     C
        AND     03H   
        JR      NZ,J4397
        LD      HL,Q_0245		; 0245H
        ADD     HL,DE
        LD      (HL),32H		; Replace 'CD C0225' instruction with 'LD (0FFFFH),A'
        INC     HL
        LD      (HL),0FFH		; "
        INC     HL
        LD      (HL),0FFH		; "
        JR      J43BC
J4397:  LD      A,0F5H			; F5 is 'PUSH AF' instruction
        LD      HL,Q_0159		; 0159H
        ADD     HL,DE
        LD      (HL),A			; Replace 'NOP' instruction with 'PUSH AF'
        LD      HL,Q_0188		; 0188H
        ADD     HL,DE
        LD      (HL),A			; Replace 'NOP' instruction with 'PUSH AF'
        LD      HL,Q_01E9		; 01E9H
        ADD     HL,DE
        LD      (HL),A			; Replace 'NOP' instruction with 'PUSH AF'
        LD      HL,Q_0221		; 0221H
        ADD     HL,DE
        LD      (HL),0CDH		; Replace 'JP' instruction with 'CALL'
        LD      HL,Q_017B		; 017BH
        ADD     HL,DE
        LD      (HL),B			; Replace 'JR J0181' instruction with 'NOP'
        INC     HL
        LD      (HL),B			; "
        LD      HL,Q_01B2		; 01B2H
        ADD     HL,DE
        LD      (HL),B			; Replace 'JR J01B8' instruction with 'NOP'
        INC     HL	
        LD      (HL),B			; "
J43BC:  PUSH    DE
        LD      HL,SLTTBL
        ADD     HL,BC
        EX      DE,HL
        LD      BC,Q_0238+1		; 0239H
        ADD     HL,BC
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      BC,Q_0242-Q_0238	; 10
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
        DEFW    SAUXOUT
        DEFW    AUXBOD+5
        DEFW    GO_BIOS
        DEFW    R00D4
        DEFW    GO_BDOS
        DEFW    R00DC
        DEFW    BDOS
        DEFW    R0064
        DEFW    0

; ------------------------------------------------------------------------------
; Following routines are loaded in ram P3
; All instructions preceded by a label Q_nnnnn may be patched at runtime
; e.g. depending if rom or ram is in an expanded slot or not

I4418:
        PHASE  0

; Subroutine SDOSON: enable disksystem rom on page 1

R0000:  PUSH    AF
        LD      A,I
        PUSH    AF                      ; store IFF2 flag
        LD      A,(MASTER)
        JR      J0010

; Subroutine SDOSOF: enable dos ram on page 1

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
        JP      PE,J0020                ; maskable interrupts were disabled, leave disabled
        EI
J0020:  POP     AF
        RET

; Subroutine XFER: transfer to/from dos rom

R0022:  PUSH    AF
        PUSH    HL
        PUSH    DE
        PUSH    BC
        CALL    GETSLT
        PUSH    AF			; save current slot in page 1
        LD      A,(RAMAD1)
        LD      H,40H
        CALL    ENASLT			; enable TPA RAM in page 1
        POP     AF
        POP     BC
        POP     DE
        POP     HL
        LDIR
C0038:  PUSH    HL
        PUSH    DE
        PUSH    BC
        LD      H,40H
        CALL    ENASLT			; restore slot in page 1
        POP     BC
        POP     DE
        POP     HL
        POP     AF
        RET

; Subroutine transfer to/from slot

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

; Subroutine BDOS diskbasic

R0064:  LD      (IX_BDOS),IX
        LD      IY,(MASTER-1)
        LD      IX,(SBDOS)
        JP      CALSLT


; Subroutine jump to address on pointer (F1E2)

R0073:  EI
        PUSH    DE
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        EX      DE,HL
        POP     DE
        JP      (HL)

; Subroutine interslot call with PROMPT routine (F1DF)

R007B:  EXX
        EX      AF,AF'
        CALL    C00BA
        EX      AF,AF'
        EXX
        CALL    CALSLT

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

C00A2:  LD      IY,(MASTER-1)
        LD      IX,(SPROMPT)
        CALL    CALSLT
        CALL    R0103
        POP     AF
        CALL    R0320                   ; PUT_P2
        POP     AF
        LD      H,40H
        CALL    ENASLT

C00BA:  LD      A,0C9H
        LD      (SDOSON),A

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

; Subroutine call routine in BDOS code segment

R00D4:  CALL    R0103			; select BDOS segments
        CALL    CLPRM1+1		; CALL HL
        JR      R00E2

; Subroutine call BDOS handler in BDOS code segment

R00DC:  CALL    R0103			; select BDOS segments
        CALL    KBDOS

; Subroutine restore DOS TPA segments (F1FA)

	; Default code is for ram p3 in non expanded slot

R00E2:  DI			; Patches when ram p3 is in expanded slot:
        PUSH    AF
Q_00E4: JR      J00EF		; NOP (2x)

Q_00E6: LD      A,(SS_BDOS)
Q_00E9: LD      (0),A		; LD (SLTTBL),A
        LD      (DFFFF),A
J00EF:  LD      A,(PS_BDOS)
        OUT     (0A8H),A
        LD      A,(P0_TPA)
        CALL    R030C			; PUT_P0
        LD      A,(P2_TPA)
        CALL    R0320           	; PUT_P2
        POP     AF
        EI
        RET

; Subroutine Enable BDOS segments (F1F7)

	; Default code is for ram p3 in non expanded slot

R0103:  DI			; Patches when ram p3 is in expanded slot:
        PUSH    AF
Q_0105: JR      J0118		; NOP (2x)

Q_0107: LD      A,(DFFFF)
        CPL
        LD      (SS_BDOS),A
        AND     0CCH		; mask page 0 and 2
Q_0110: OR      00H		; 00H patched with slot# ram p3 in bits 3+4 and 7+8
Q_0112: LD      (0),A		; LD (SLTTBL),A
        LD      (DFFFF),A
J0118:  IN      A,(0A8H)
        LD      (PS_BDOS),A
        AND     0CCH
Q_011F: OR      00H		; 00H patched with slot# ram p3 in bits 1+2 and 5+6
        OUT     (0A8H),A
 	CALL    R0312                   ; GET_P0
        LD      (P0_TPA),A
	CALL    R031C                   ; GET_P1
        LD      (P1_TPA),A
        CALL    R0326                   ; GET_P2
        LD      (P2_TPA),A
        LD      A,(CODE_S)
R0138:  CALL    R030C                   ; PUT_P0
        LD      A,(DATA_S)
R013E:  CALL    R0320                   ; PUT_P2
        POP     AF
        EI
        RET

; Subroutine transfer to/from segment (F1D6)

R0144:  PUSH    AF
        DI
        CALL    R030C                   ; PUT_P0
        LDIR
        LD      A,(CODE_S)
R014E:  CALL    R030C                   ; PUT_P0
        EI
        POP     AF
        RET

; Subroutine interslot call to main bios (F1D9)

R0154:  EX      AF,AF'
        PUSH    AF
        CALL    C0235
Q_0159: NOP				; May be patched with PUSH AF
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
R0172:  CALL    R030C                   ; PUT_P0
        LD      A,(DATA_S)
        CALL    R0320                   ; PUT_P2
Q_017B: JR      J0181			; May be patched with NOP (2x)

Q_017D: POP     AF
        CALL    C0225
J0181:  POP     AF
        EX      AF,AF'
        EI
        RET

; Subroutine print string via CHPUT (F1DC)

R0185:  CALL    C0235
Q_0188: NOP				; May be patched with PUSH AF
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
R01AC:  CALL    R030C                   ; PUT_P0
        CALL    R0254
Q_01B2: JR      J01B8			; May be patched with NOP (2x)

Q_01B4: POP     AF
        CALL    C0225
J01B8:  EI
        RET

; Subroutine interrupt handler (F1E5)

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

; Subroutine 

C01E6:  CALL    C0235
Q_01E9:	NOP				; May be patched with PUSH AF
        CALL    R0312                   ; GET_P0
Q_01ED: CP      00H			; Value will be P0 code segment at runtime
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
        JR      Q_0221

J021E:  CALL    C0038
Q_0221:	JP      R0254			; May be patched with 'CALL R0254'

Q_0224: POP     AF

; Subroutine 

C0225:  PUSH    BC
        PUSH    DE
        LD      E,A
        IN      A,(0A8H)
        LD      B,A
        AND     3FH    
Q_022D: OR      00H			 ; 00H patched with slot# main bios in bits 7+8
        CALL    SSLOTE
        POP     DE
        POP     BC
        RET

; Subroutine 

	; Default code is for main bios in non expanded slot

C0235:  DI			; Patches when main bios is in expanded slot:
Q_0236:	JR      J0248		; NOP (2x)

Q_0238: LD      A,(0)			; May be patched with LD A,(SLTTBL)
        LD      (SS_TEMP),A
        AND     0FCH
Q_0240: OR      00H		; 00H patched
Q_0242:	LD      (0),A
Q_0245: CALL    C0225			; May be patched with LD (0FFFFH),A
J0248:  IN      A,(0A8H)
        AND     0FCH
Q_024C: OR      00H			; 00H patched with slot# main bios bits 1+2
        OUT     (0A8H),A
        LD      A,(SS_TEMP)
        RET

; Subroutine select DOS code on page 0 (F1FD)

	; Default code is for ram p3 in non expanded slot

R0254:  DI			; Patches when ram p3 is in expanded slot:
Q_0255:	JR      J0268		; NOP (2x)

Q_0257: LD      A,(DFFFF)
        CPL
        LD      (SS_TEMP),A
        AND     0FCH
Q_0260: OR      00H		; 00H patched with slot# ram p3 in bits 3+4
Q_0262: LD      (0),A		; LD (SLTTBL),A
        LD      (DFFFF),A
J0268:  IN      A,(0A8H)
        AND     0FCH
Q_026C: OR      00H		; 00H patched with slot# ram p3 bits 1+2
        OUT     (0A8H),A
        LD      A,(SS_TEMP)
	RET

Q_0274: RET			; LD (SLTTBL),A
Q_0275: DEFB    0,0		; "
        LD      (DFFFF),A
        RET

; Subroutine ALL_SEG handler

R027B:  PUSH    HL
        LD      HL,C005C
        JR      J0285

; Subroutine FRE_SEG handler

R0281:  PUSH    HL
        LD      HL,C005F
J0285:  PUSH    DE
        CALL    GO_BIOS
        POP     DE
        POP     HL
        RET

; Subroutine RD_SEG handler

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

; Subroutine WR_SEG handler

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

; Subroutine CALLS handler

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

; Subroutine CAL_SEG handler

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

; Subroutine PUT_PH handler

R02EC:  BIT     7,H
        JR      NZ,J02F6
        BIT     6,H
        JR      Z,R030C                 ; PUT_P0
        JR      R0316                   ; PUT_P1

J02F6:  BIT     6,H
        JR      Z,R0320                 ; PUT_P2
        JR      R032D                   ; PUT_P3

; Subroutine GET_PH handler

R02FC:  BIT     7,H
        JR      NZ,J0306
        BIT     6,H
        JR      Z,R0312                 ; GET_P0
        JR      R031C                   ; GET_P1

J0306:  BIT     6,H
        JR      Z,R0326                 ; GET_P2
        JR      R032A                   ; GET_P3

; Subroutine PUT_P0 handler

R030C:  LD      (P0_SEG),A
        OUT     (0FCH),A
        RET

; Subroutine GET_P0 handler

R0312:  LD      A,(P0_SEG)
        RET

; Subroutine PUT_P1 handler

R0316:  LD      (P1_SEG),A
        OUT     (0FDH),A
        RET

; Subroutine GET_P1 handler

R031C:  LD      A,(P1_SEG)
        RET

; Subroutine PUT_P2 handler

R0320:  LD      (P2_SEG),A
        OUT     (0FEH),A
        RET

; Subroutine GET_P2 handler

R0326:  LD      A,(P2_SEG)
        RET

; Subroutine GET_P3 handler

R032A:  LD      A,(P3_SEG)

; Subroutine PUT_P3 handler

R032D:  RET

; Subroutine RDSLT (F1E8)

R032E:  RES     6,D
        JR      J0334

; Subroutine WRSLT (F1EB)

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
        LD      (DFFFF),A
J0359:  CALL    NZ,SSLOTE
        LD      (HL),E
        POP     HL
        LD      A,C
        LD      E,C
        RET

; Subroutine RDSLT/WRSLT on primary slot

C0361:  IN      A,(0A8H)
        LD      B,A
        AND     (HL)
        INC     HL
        OR      (HL)
        DI
DS0368: LD      HL,0
DS0369  EQU     DS0368+1
DS036B: LD      DE,0
DS036C  EQU     DS036B+1
        BIT     6,D
        LD      D,B
        JP      NZ,WRPRIM
        CALL    RDPRIM
        LD      A,E
        RET

; Subroutine CALLF (F1F4)

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

; Subroutine CALSLT (F1EE)

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
        LD      (DFFFF),A
J03B3:  CALL    NZ,SSLOTE
        LD      (HL),E
        POP     AF                      ; restore IFF2 flag
        JP      PO,J03BC                ; maskable interrupts where disabled, leave disabled
        EI
J03BC:  EX      AF,AF'
        EXX
        RET

; Subroutine CALSLT on primary slot

C03BF:  IN      A,(0A8H)
        PUSH    AF
        AND     (HL)
        INC     HL
        OR      (HL)
        EXX
        DI
        JP      CLPRIM

; Subroutine ENASLT (F1F1)

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

; Subroutine change secondary slot register

C03DF:  PUSH    HL
        AND     03H
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
        AND     3FH
        XOR     C
        LD      H,A
        CP      B
        JR      NZ,J0409
        LD      A,L
        LD      (DFFFF),A
J0409:  CALL    NZ,SSLOTL
        LD      A,(SS_TEMP)
        LD      C,A
        LD      A,H
        POP     HL
        RET

; Subroutine get slot masks

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

; ------------------------------------------------------------------------------
; Table with pointers to jump entries 0F1D3-F22D 
; indirect entry points

I485F:  DEFW    R0045                   ; F1D3 RD_LDI / JUMPB transfer to/from page 1
        DEFW    R0144                   ; F1D6 P0_LDIR transfer with page 0 
        DEFW    R0154                   ; F1D9 P0_CALL call main-bios
        DEFW    R0185                   ; F1DC SFLUSH print string via chput
        DEFW    R007B                   ; F1DF GO_DRV interslot call with prompt handler
        DEFW    R0073                   ; F1E2 JP_VEC start DOS1 style handler
        DEFW    R01BA                   ; F1E5 SIRQ KEYINT handler
        DEFW    R032E                   ; F1E8 RDSLT handler
        DEFW    R0332                   ; F1EB WRSLT handler
        DEFW    R0388                   ; F1EE CALSLT handler
        DEFW    R03CA                   ; F1F1 ENASLT handler
        DEFW    R0379                   ; F1F4 CALLF handler
        DEFW    R0103                   ; F1F7 PUT_BD Enable BDOS segments
        DEFW    R00E2                   ; F1FA PUT_US Enable DOS segments
        DEFW    R0254                   ; F1FD P0_RAM Enable DOS RAM on page 0
        DEFW    R027B                   ; F200 MAP_VE 
        DEFW    R0281                   ; F203
        DEFW    R028C                   ; F206 RD_SEG
        DEFW    R02A5                   ; F209 WR_SEG 
        DEFW    R02CD                   ; F20C 
        DEFW    R02BD                   ; F20F
        DEFW    R02EC                   ; F212
        DEFW    R02FC                   ; F215
        DEFW    R030C                   ; F218 PUT_P0
        DEFW    R0312                   ; F21B GET_P0
        DEFW    R0316                   ; F21E PUT_P1
        DEFW    R031C                   ; F221 GET_P1
        DEFW    R0320                   ; F224 PUT_P2
        DEFW    R0326                   ; F227 GET_P2
        DEFW    R032D                   ; F22A
        DEFW    R032A                   ; F22D
        DEFW    0


; Patch relocated code (values for original source in the comments)

I489F:  DEFW    J0010+13	; 001DH
        DEFW    R007B+3		; 007EH
        DEFW    C0085+9		; 008EH
        DEFW    J0093+2		; 0095H
        DEFW    J0093+9		; 009CH
        DEFW    J0093+13	; 00A0H
        DEFW    C00A2+12	; 00AEH
        DEFW    C00A2+16	; 00B2H
        DEFW    C00BF+4		; 00C3H
        DEFW    I00D1+1		; 00D2H
        DEFW    R00D4+1		; 00D5H
        DEFW    R00DC+1		; 00DDH
        DEFW    J00EF+9		; 00F8H
        DEFW    J00EF+15	; 00FEH
        DEFW    J0118+12	; 0124H
        DEFW    J0118+18	; 012AH
        DEFW    J0118+24	; 0130H
        DEFW    R0138+1		; 0139H
        DEFW    R013E+1		; 013FH
        DEFW    R0144+3		; 0147H
        DEFW    R014E+1		; 014FH
        DEFW    R0154+3		; 0157H
        DEFW    R0154+10	; 015EH
        DEFW    R0154+16	; 0164H
        DEFW    R0154+25	; 016DH
        DEFW    R0172+1		; 0173H
        DEFW    R0172+7		; 0179H
        DEFW    Q_017D+2	; 017FH
        DEFW    R0185+1		; 0186H
        DEFW    R0185+8		; 018DH
        DEFW    J018F+7		; 0196H
        DEFW    J018F+19	; 01A2H
        DEFW    R01AC+1		; 01ADH
        DEFW    R01AC+4		; 01B0H
        DEFW    Q_01B4+2	; 01B6H
        DEFW    J01CD+9		; 01D6H
        DEFW    J01DE+1		; 01DFH
        DEFW    J01E1+6		; 01E7H
        DEFW    C01E6+5		; 01EBH
        DEFW    C01E6+13	; 01F3H
        DEFW    C01E6+17	; 01F7H
        DEFW    C01E6+24	; 01FEH
        DEFW    C01E6+30	; 0204H
        DEFW    C01E6+36	; 020AH
        DEFW    C01E6+44	; 0212H
        DEFW    C01E6+48	; 0216H
        DEFW    C01E6+52	; 021AH
        DEFW    Q_0221+1	; 0222H
        DEFW    Q_0238+14	; 0246H
        DEFW    R028C+5		; 0291H
        DEFW    R028C+10	; 0296H
        DEFW    R028C+19	; 029FH
        DEFW    R02A5+5		; 02AAH
        DEFW    R02A5+10	; 02AFH
        DEFW    R02A5+19	; 02B8H
        DEFW    R02CD+6		; 02D3H
        DEFW    R02CD+14	; 02DBH
        DEFW    R02CD+26	; 02E7H
        DEFW    J0334+2		; 0336H
        DEFW    J0334+6		; 033AH
        DEFW    J0334+11	; 033FH
        DEFW    J0334+18	; 0346H
        DEFW    J0334+24	; 034CH
        DEFW    J0390+2		; 0392H
        DEFW    J0390+9		; 0399H
        DEFW    J0390+15	; 039FH
        DEFW    J03B3+6		; 03B9H
        DEFW    R03CA+4		; 03CEH
        DEFW    R03CA+9		; 03D3H
        DEFW    C03DF+14	; 03EDH
        DEFW    C0413+15	; 0422H
        DEFW    0

