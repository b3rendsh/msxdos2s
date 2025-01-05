; ------------------------------------------------------------------------------
; p3_paging.asm
; DOS 2.20 kernel: paging helper routines
; Based on ASCII DOS 2.20 codebase s1
;
; Code Copyrighted by ASCII and maybe others
; Source origin is the msxsyssrc repository by Arjen Zeilemaker
; Restructure, modifications and additional comments by H.J. Berends
;
; Sourcecode supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyrightholders
; ------------------------------------------------------------------------------
; Modifications:
; 01. Moved init of paging helper routines to separate module


		INCLUDE "disk.inc"	; Assembler directives
		INCLUDE	"msx.inc"	; MSX constants and definitions

		SECTION	P3_PAGING

		; Routine used in the main module
		PUBLIC	PH_INIT

		; Routines defined in the main module
		EXTERN	GETSLT
		EXTERN	ALLOCMEM

; ------------------------------------------------------------------------------
; *** Load and patch BDOS paging helper routines in ram p3
; ------------------------------------------------------------------------------
PH_INIT:	LD      HL,500+(I485F-I4418)	; 1595 = 200 + 300 + 1095
		CALL    ALLOCMEM
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
J42AB:		LD      E,(HL)
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
J42C4:		POP     DE
		LD      HL,I489F		; Patch table for dynamic relocated code
J42C8:		LD      C,(HL)
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

J42DE:		LD      A,(CODE_S)              ; BDOS code segment
		LD      HL,R01ED+1
		ADD     HL,DE
		LD      (HL),A			; Write P0 Code segment # to 'CP #' instruction
		LD      A,(RAMAD3)
		AND     03H
		LD      HL,R026C+1
		ADD     HL,DE
		LD      (HL),A			; Write slot# ram p3 bits 1+2 to 'OR #' instruction
		LD      C,A
		LD      B,00H			; 00 is NOP instruction
		RLCA
		RLCA
		RLCA
		RLCA
		OR      C
		LD      HL,R011F+1
		ADD     HL,DE
		LD      (HL),A			; Write slot# ram p3 in bits 1+2 and 5+6 to 'OR #' instruction
		LD      A,(RAMAD3)
		BIT     7,A			; Expanded slot?
		JR      Z,J4356			; z=no
		LD      HL,R0255
		ADD     HL,DE
		LD      (HL),B			; Replace 'JR J0268' instruction with 'NOP'
		INC     HL
		LD      (HL),B			; "
		LD      HL,R00E4
		ADD     HL,DE
		LD      (HL),B			; Replace 'JR J00EF' instruction with 'NOP'
		INC     HL
		LD      (HL),B			; "
		LD      HL,R0105
		ADD     HL,DE
		LD      (HL),B			; Replace 'JR J0118' instruction with 'NOP'
		INC     HL
		LD      (HL),B			; "
		LD      HL,R0274
		ADD     HL,DE
		LD      (HL),32H		; 32 is 'LD (#),A' instruction, where # is at R0275
		RRCA
		RRCA
		AND     03H
		LD      HL,R0260+1
		ADD     HL,DE
		LD      (HL),A			; Write slot# ram p3 in bits 3+4 to 'OR #'instruction
		LD      L,A
		RLCA
		RLCA
		RLCA
		RLCA
		OR      L
		LD      HL,R0110+1
		ADD     HL,DE
		LD      (HL),A                  ; Write slot# ram p3 in bits 3+4 and 7+8 to 'OR #' instruction
		PUSH    DE
		LD      HL,SLTTBL
		ADD     HL,BC
		EX      DE,HL
		LD      BC,R0262+1
		ADD     HL,BC
		LD      (HL),E
		INC     HL
		LD      (HL),D
		LD      BC,R0275-(R0262+2)	; 17
		ADD     HL,BC
		LD      (HL),E
		INC     HL
		LD      (HL),D
		LD      BC,R00E9-R0275		; -396 = -18CH
		ADD     HL,BC			; (0275H + 1) - 18CH = 00EAH
		LD      (HL),E
		INC     HL
		LD      (HL),D
		LD      BC,R0112-(R00E9+1)	; 40 = 28H
		ADD     HL,BC			; (00E9H + 1) + 28H = 0112H
		LD      (HL),E
		INC     HL
		LD      (HL),D
		POP     DE
J4356:		LD      A,(EXPTBL+0)
		AND     03H
		LD      HL,R024C+1
		ADD     HL,DE
		LD      (HL),A			; Write slot# main bios to 'OR #' instruction
		LD      C,A
		LD      B,00H			; 00 is NOP instruction
		RRCA
		RRCA
		LD      HL,R022D+1
		ADD     HL,DE
		LD      (HL),A			; Write slot# main bios in bits 7+8 to 'OR #' instruction
		LD      A,(EXPTBL+0)
		BIT     7,A			; main bios in expanded slot?
		JR      Z,J43D1			; z=no
		LD      HL,R0236
		ADD     HL,DE
		LD      (HL),B			; Replace 'JR 0248' instruction with 'NOP'
		INC     HL
		LD      (HL),B			; "
		RRCA
		RRCA
		AND     03H
		LD      HL,R0240+1
		ADD     HL,DE
		LD      (HL),A			; Write value # to 'OR #' instruction
		LD      A,(RAMAD3)
		XOR     C
		AND     03H
		JR      NZ,J4397
		LD      HL,R0245
		ADD     HL,DE
		LD      (HL),32H		; Replace 'CD C0225' instruction with 'LD (0FFFFH),A'
		INC     HL
		LD      (HL),0FFH		; "
		INC     HL
		LD      (HL),0FFH		; "
		JR      J43BC
J4397:		LD      A,0F5H			; F5 is 'PUSH AF' instruction
		LD      HL,R0159
		ADD     HL,DE
		LD      (HL),A			; Replace 'NOP' instruction with 'PUSH AF'
		LD      HL,R0188
		ADD     HL,DE
		LD      (HL),A			; Replace 'NOP' instruction with 'PUSH AF'
		LD      HL,R01E9
		ADD     HL,DE
		LD      (HL),A			; Replace 'NOP' instruction with 'PUSH AF'
		LD      HL,R0221
		ADD     HL,DE
		LD      (HL),0CDH		; Replace 'JP' instruction with 'CALL'
		LD      HL,R017B
		ADD     HL,DE
		LD      (HL),B			; Replace 'JR J0181' instruction with 'NOP'
		INC     HL
		LD      (HL),B			; "
		LD      HL,R01B2
		ADD     HL,DE
		LD      (HL),B			; Replace 'JR J01B8' instruction with 'NOP'
		INC     HL
		LD      (HL),B			; "
J43BC:		PUSH    DE
		LD      HL,SLTTBL
		ADD     HL,BC
		EX      DE,HL
		LD      BC,R0238+1
		ADD     HL,BC
		LD      (HL),E
		INC     HL
		LD      (HL),D
		LD      BC,R0242-R0238		; 10
		ADD     HL,BC
		LD      (HL),D
		DEC     HL
		LD      (HL),E
		POP     DE
J43D1:		LD      HL,I43F6
J43D4:		LD      C,(HL)
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
J43EC:		LD      A,L
		LD      (BC),A
		INC     BC
		LD      A,H
		LD      (BC),A
		POP     HL
		JR      J43D4
J43F4:		OR      A
		RET

I43F6:		DEFW    SDOSON,PH_SDOSON
		DEFW    SDOSOF,PH_SDOSOF
		DEFW    XFER,PH_XFER
		DEFW    SAUXIN,AUXBOD+0
		DEFW    SAUXOUT,AUXBOD+5
		DEFW    GO_BIOS,PH_GOBIOS
		DEFW    GO_BDOS,PH_GOBDOS
		DEFW    BDOS,PH_BDOS
		DEFW    0

; ------------------------------------------------------------------------------
; *** Paging helper routines loaded in RAM P3 ***
; ------------------------------------------------------------------------------
I4418:
		PHASE  0

; ---------------------------------------------------------
; Subroutine SDOSON: enable disksystem rom on page 1
; ---------------------------------------------------------
PH_SDOSON:	PUSH    AF
		LD      A,I
		PUSH    AF                      ; store IFF2 flag
		LD      A,(MASTER)
		JR      J0010

; ---------------------------------------------------------
; Subroutine SDOSOF: enable dos ram on page 1
; ---------------------------------------------------------
PH_SDOSOF:	PUSH    AF
		LD      A,I
		PUSH    AF                      ; store IFF2 flag
		LD      A,(RAMAD1)
J0010:		PUSH    HL
		PUSH    DE
		PUSH    BC
		LD      H,40H
		CALL    ENASLT
		POP     BC
		POP     DE
		POP     HL
		POP     AF                      ; restore IFF2 flag
R001C:		JP      PE,J0020                ; maskable interrupts were disabled, leave disabled
		EI
J0020:		POP     AF
		RET

; ---------------------------------------------------------
; Subroutine XFER: transfer to/from dos rom
; ---------------------------------------------------------
PH_XFER:	PUSH    AF
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
		PUSH    HL
		PUSH    DE
		PUSH    BC
		LD      H,40H
		CALL    ENASLT			; restore slot in page 1
		POP     BC
		POP     DE
		POP     HL
		POP     AF
		RET

; ---------------------------------------------------------
; Subroutine transfer to/from slot
; ---------------------------------------------------------
PH_RDLDI:	PUSH    DE
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
J0057:		EX      AF,AF'
		LDIR
		LD      A,(MASTER)
		LD      H,40H
		CALL    ENASLT
		EI
		RET

; ---------------------------------------------------------
; Subroutine BDOS diskbasic
; ---------------------------------------------------------
PH_BDOS:	LD      (IX_BDOS),IX
		LD      IY,(MASTER-1)
		LD      IX,(SBDOS)
		JP      CALSLT

; ---------------------------------------------------------
; Subroutine jump to address on pointer (F1E2)
; ---------------------------------------------------------
PH_JPVEC:	EI
		PUSH    DE
		LD      E,(HL)
		INC     HL
		LD      D,(HL)
		EX      DE,HL
		POP     DE
		JP      (HL)

; ---------------------------------------------------------
; Subroutine interslot call with PROMPT routine (F1DF)
; ---------------------------------------------------------
PH_GODRV:	EXX
		EX      AF,AF'
R007D:		CALL    C00BA
		EX      AF,AF'
		EXX
		CALL    CALSLT

C0085:		EXX
		EX      AF,AF'
		LD      A,(SDOSOF)
		LD      (SDOSON),A
R008D:		CALL    C00BF
		EX      AF,AF'
		EXX
		RET

J0093:		POP     AF
R0094:		CALL    C0085
		CALL    GETSLT
		PUSH    AF
R009B:		CALL    PH_GETP2
		PUSH    AF
R009F:		CALL    PH_PUTUS
		LD      IY,(MASTER-1)
		LD      IX,(SPROMPT)
		CALL    CALSLT
R00AD:		CALL    PH_PUTBD
		POP     AF
R00B1:		CALL    PH_PUTP2
		POP     AF
		LD      H,40H
		CALL    ENASLT

C00BA:		LD      A,0C9H
		LD      (SDOSON),A

C00BF:		LD      HL,H_PROM
R00C2:		LD      DE,R00D1
		LD      B,3
J00C7:		LD      C,(HL)
		LD      A,(DE)
		LD      (HL),A
		LD      A,C
		LD      (DE),A
		INC     HL
		INC     DE
		DJNZ    J00C7
		RET

R00D1:		JP      J0093

; ---------------------------------------------------------
; Subroutine call routine in BDOS code segment
; ---------------------------------------------------------
PH_GOBIOS:
R00D4:		CALL    PH_PUTBD	; select BDOS segments
		CALL    CLPRM1+1	; CALL HL
		JR      PH_PUTUS

; ---------------------------------------------------------
; Subroutine call BDOS handler in BDOS code segment
; ---------------------------------------------------------
PH_GOBDOS:
R00DC:		CALL    PH_PUTBD	; select BDOS segments
		CALL    KBDOS


; ---------------------------------------------------------
; Subroutine restore DOS TPA segments (F1FA)
; Default code is for ram p3 in non expanded slot
; ---------------------------------------------------------
PH_PUTUS:	DI			; Patches when ram p3 is in expanded slot:
		PUSH    AF
R00E4: 		JR      J00EF		; NOP (2x)

R00E6: 		LD      A,(SS_BDOS)
R00E9: 		LD      (0),A		; LD (SLTTBL),A
		LD      (DFFFF),A
J00EF:		LD      A,(PS_BDOS)
		OUT     (0A8H),A
		LD      A,(P0_TPA)
R00F7:		CALL    PH_PUTP0
		LD      A,(P2_TPA)
R00FD:		CALL    PH_PUTP2
		POP     AF
		EI
		RET

; ---------------------------------------------------------
; Subroutine Enable BDOS segments (F1F7)
; Default code is for ram p3 in non expanded slot
; ---------------------------------------------------------
PH_PUTBD:	DI			; Patches when ram p3 is in expanded slot:
		PUSH    AF
R0105: 		JR      J0118		; NOP (2x)

R0107: 		LD      A,(DFFFF)
		CPL
		LD      (SS_BDOS),A
		AND     0CCH		; mask page 0 and 2
R0110: 		OR      00H		; 00H patched with slot# ram p3 in bits 3+4 and 7+8
R0112: 		LD      (0),A		; LD (SLTTBL),A
		LD      (DFFFF),A
J0118:		IN      A,(0A8H)
		LD      (PS_BDOS),A
		AND     0CCH
R011F: 		OR      00H		; 00H patched with slot# ram p3 in bits 1+2 and 5+6
		OUT     (0A8H),A
R0123:		CALL    PH_GETP0
		LD      (P0_TPA),A
R0129:		CALL    PH_GETP1
		LD      (P1_TPA),A
R012F:		CALL    PH_GETP2
		LD      (P2_TPA),A
		LD      A,(CODE_S)
R0138:		CALL    PH_PUTP0
		LD      A,(DATA_S)
R013E:		CALL    PH_PUTP2
		POP     AF
		EI
		RET

; ---------------------------------------------------------
; Subroutine transfer to/from segment (F1D6)
; ---------------------------------------------------------
PH_P0LDIR:	PUSH    AF
		DI
R0146:		CALL    PH_PUTP0
		LDIR
		LD      A,(CODE_S)
R014E:		CALL    PH_PUTP0
		EI
		POP     AF
		RET

; ---------------------------------------------------------
; Subroutine interslot call to main bios (F1D9)
; ---------------------------------------------------------
PH_P0CALL:	EX      AF,AF'
		PUSH    AF
R0156:		CALL    C0235
R0159: 		NOP				; May be patched with PUSH AF
		LD      A,(P0_TPA)
R015D:		CALL    PH_PUTP0
		LD      A,(P2_TPA)
R0163:		CALL    PH_PUTP2
		EI
		EX      AF,AF'
		CALL    CLPRM1
		EX      AF,AF'
R016C:		CALL    PH_P0RAM
		LD      A,(CODE_S)
R0172:		CALL    PH_PUTP0
		LD      A,(DATA_S)
R0178:		CALL    PH_PUTP2
R017B: 		JR      J0181			; May be patched with NOP (2x)

		POP     AF
R017E:		CALL    C0225
J0181:		POP     AF
		EX      AF,AF'
		EI
		RET

; ---------------------------------------------------------
; Subroutine print string via CHPUT (F1DC)
; ---------------------------------------------------------
PH_SFLUSH:
R0185:		CALL    C0235
R0188: 		NOP				; May be patched with PUSH AF
		LD      A,(P0_TPA)
R018C:		CALL    PH_PUTP0
J018F:		PUSH    HL
		PUSH    BC
		LD      B,(HL)
		LD      A,(P2_TPA)
R0195:		CALL    PH_PUTP2
		LD      A,B
		EI
		CALL    CHPUT
		DI
		LD      A,(DATA_S)
R01A1:		CALL    PH_PUTP2
		POP     BC
		POP     HL
		INC     HL
		DJNZ    J018F
		LD      A,(CODE_S)
R01AC:		CALL    PH_PUTP0
R01AF:		CALL    PH_P0RAM
R01B2: 		JR      J01B8			; May be patched with NOP (2x)

		POP     AF
R01B5:		CALL    C0225
J01B8:		EI
		RET

; ---------------------------------------------------------
; Subroutine interrupt handler (F1E5)
; ---------------------------------------------------------
PH_SIRQ:	DI
		PUSH    AF
		PUSH    HL
		PUSH    BC
		LD      HL,(IRQ_ST)
		OR      A
		SBC     HL,SP
		JR      C,J01CD
		LD      BC,200
		SBC     HL,BC
		JR      C,R01DE
J01CD:		LD      (SP_IRQ),SP
		LD      SP,(IRQ_ST)
R01D5:		CALL    R01E6
		LD      SP,(SP_IRQ)
		JR      J01E1

R01DE:		CALL    R01E6
J01E1:		POP     BC
		POP     HL
		POP     AF
		EI
		RET

; ---------------------------------------------------------
; Subroutines
; ---------------------------------------------------------

; Subroutine
R01E6:		CALL    C0235
R01E9:		NOP				; May be patched with PUSH AF
R01EA:		CALL    PH_GETP0
R01ED: 		CP      00H			; Value will be P0 code segment at runtime
		JR      NZ,J021E
		PUSH    AF
R01F2:		CALL    PH_GETP1
		PUSH    AF
R01F6:		CALL    PH_GETP2
		PUSH    AF
		LD      A,(P2_TPA)
R01FD:		CALL    PH_PUTP2
		LD      A,(P1_TPA)
R0203:		CALL    PH_PUTP1
		LD      A,(P0_TPA)
R0209:		CALL    PH_PUTP0
		CALL    KEYINT
		DI
		POP     AF
R0211:		CALL    PH_PUTP2
		POP     AF
R0215:		CALL    PH_PUTP1
		POP     AF
R0219:		CALL    PH_PUTP0
		JR      R0221

J021E:		CALL    KEYINT
R0221:		JP      PH_P0RAM		; May be patched with 'CALL PH_P0RAM'

R0224: 		POP     AF

; Subroutine 
C0225:		PUSH    BC
		PUSH    DE
		LD      E,A
		IN      A,(0A8H)
		LD      B,A
		AND     3FH
R022D: 		OR      00H			 ; 00H patched with slot# main bios in bits 7+8
		CALL    SSLOTE
		POP     DE
		POP     BC
		RET

; Subroutine 
; Default code is for main bios in non expanded slot
C0235:		DI			; Patches when main bios is in expanded slot:
R0236:		JR      J0248		; NOP (2x)

R0238: 		LD      A,(0)			; May be patched with LD A,(SLTTBL)
		LD      (SS_TEMP),A
		AND     0FCH
R0240: 		OR      00H		; 00H patched
R0242:		LD      (0),A
R0245: 		CALL    C0225			; May be patched with LD (0FFFFH),A
J0248:		IN      A,(0A8H)
		AND     0FCH
R024C: 		OR      00H			; 00H patched with slot# main bios bits 1+2
		OUT     (0A8H),A
		LD      A,(SS_TEMP)
		RET

; ---------------------------------------------------------
; Subroutine select DOS code on page 0 (F1FD)
; Default code is for ram p3 in non expanded slot
; ---------------------------------------------------------
PH_P0RAM:	DI			; Patches when ram p3 is in expanded slot:
R0255:		JR      J0268		; NOP (2x)

R0257: 		LD      A,(DFFFF)
		CPL
		LD      (SS_TEMP),A
		AND     0FCH
R0260: 		OR      00H		; 00H patched with slot# ram p3 in bits 3+4
R0262: 		LD      (0),A		; LD (SLTTBL),A
		LD      (DFFFF),A
J0268:		IN      A,(0A8H)
		AND     0FCH
R026C: 		OR      00H		; 00H patched with slot# ram p3 bits 1+2
		OUT     (0A8H),A
		LD      A,(SS_TEMP)
		RET

R0274: 		RET			; LD (SLTTBL),A
R0275: 		DEFB    0,0		; "
		LD      (DFFFF),A
		RET

; ---------------------------------------------------------
; Subroutine ALL_SEG handler
; ---------------------------------------------------------
PH_ALLSEG:	PUSH    HL
		LD      HL,ALL_SEG
		JR      J0285

; ---------------------------------------------------------
; Subroutine FRE_SEG handler
; ---------------------------------------------------------
PH_FRESEG:	PUSH    HL
		LD      HL,FRE_SEG
J0285:		PUSH    DE
		CALL    GO_BIOS
		POP     DE
		POP     HL
		RET

; ---------------------------------------------------------
; Subroutine RD_SEG handler
; ---------------------------------------------------------
PH_RDSEG:	DI
		PUSH    HL
		PUSH    BC
		LD      B,A
R0290:		CALL    PH_GETP2
		LD      C,A
		LD      A,B
R0295:		CALL    PH_PUTP2
		RES     6,H
		SET     7,H
		LD      B,(HL)
		LD      A,C
R029E:		CALL    PH_PUTP2
		LD      A,B
		POP     BC
		POP     HL
		RET

; ---------------------------------------------------------
; Subroutine WR_SEG handler
; ---------------------------------------------------------
PH_WRSEG:	DI
		PUSH    HL
		PUSH    BC
		LD      B,A
R02A9:		CALL    PH_GETP2
		LD      C,A
		LD      A,B
R02AE:		CALL    PH_PUTP2
		RES     6,H
		SET     7,H
		LD      (HL),E
		LD      A,C
R02B7:		CALL    PH_PUTP2
		POP     BC
		POP     HL
		RET

; ---------------------------------------------------------
; Subroutine CALLS handler
; ---------------------------------------------------------
PH_CALLS:	EXX
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

; ---------------------------------------------------------
; Subroutine CAL_SEG handler
; ---------------------------------------------------------
PH_CALSEG:	EXX
		EX      AF,AF'
		PUSH    IX
		POP     HL
R02D2:		CALL    PH_GETPH
		PUSH    AF
		PUSH    HL
		PUSH    IY
		POP     AF
R02DA:		CALL    PH_PUTPH
		EX      AF,AF'
		EXX
		CALL    CLPRM1
		EXX
		EX      AF,AF'
		POP     HL
		POP     AF
R02E6:		CALL    PH_PUTPH
		EX      AF,AF'
		EXX
		RET

; ---------------------------------------------------------
; Subroutines PUT / GET PX handlers
; ---------------------------------------------------------

; Subroutine PUT_PH handler
PH_PUTPH:	BIT     7,H
		JR      NZ,J02F6
		BIT     6,H
		JR      Z,PH_PUTP0
		JR      PH_PUTP1

J02F6:		BIT     6,H
		JR      Z,PH_PUTP2
		JR      PH_PUTP3

; Subroutine GET_PH handler
PH_GETPH:	BIT     7,H
		JR      NZ,J0306
		BIT     6,H
		JR      Z,PH_GETP0
		JR      PH_GETP1

J0306:		BIT     6,H
		JR      Z,PH_GETP2
		JR      PH_GETP3

; Subroutine PUT_P0 handler
PH_PUTP0:	LD      (P0_SEG),A
		OUT     (0FCH),A
		RET

; Subroutine GET_P0 handler
PH_GETP0:	LD      A,(P0_SEG)
		RET

; Subroutine PUT_P1 handler
PH_PUTP1:	LD      (P1_SEG),A
		OUT     (0FDH),A
		RET

; Subroutine GET_P1 handler
PH_GETP1:	LD      A,(P1_SEG)
		RET

; Subroutine PUT_P2 handler
PH_PUTP2:	LD      (P2_SEG),A
		OUT     (0FEH),A
		RET

; Subroutine GET_P2 handler
PH_GETP2:	LD      A,(P2_SEG)
		RET

; Subroutine GET_P3 handler
PH_GETP3:	LD      A,(P3_SEG)

; Subroutine PUT_P3 handler
PH_PUTP3:	RET

; ---------------------------------------------------------
; Subroutine RDSLT (F1E8)
; ---------------------------------------------------------
PH_RDSLT:	RES     6,D
		JR      J0334

; ---------------------------------------------------------
; Subroutine WRSLT (F1EB)
; ---------------------------------------------------------
PH_WRSLT:	SET     6,D
J0334:		DI
R0335:		LD      (DS0369),HL
		EX      DE,HL
R0339:		LD      (DS036C),HL
		LD      C,A
		LD      B,D
R033E:		CALL    C0413
		BIT     7,C
		JR      Z,C0361
R0345:		CALL    C03DF
		PUSH    DE
		PUSH    BC
		PUSH    AF
R034B:		CALL    C0361
		LD      C,A
		POP     AF
		POP     DE
		LD      B,D
		EX      (SP),HL
		JR      NZ,J0359
		LD      A,E
		LD      (DFFFF),A
J0359:		CALL    NZ,SSLOTE
		LD      (HL),E
		POP     HL
		LD      A,C
		LD      E,C
		RET

; ---------------------------------------------------------
; Subroutine RDSLT/WRSLT on primary slot
; ---------------------------------------------------------
C0361:		IN      A,(0A8H)
		LD      B,A
		AND     (HL)
		INC     HL
		OR      (HL)
		DI
DS0368: 	LD      HL,0
DS0369  	EQU     DS0368+1
DS036B: 	LD      DE,0
DS036C  	EQU     DS036B+1
		BIT     6,D
		LD      D,B
		JP      NZ,WRPRIM
		CALL    RDPRIM
		LD      A,E
		RET

; ---------------------------------------------------------
; Subroutine CALLF (F1F4)
; ---------------------------------------------------------
PH_CALLF:	EXX
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

; ---------------------------------------------------------
; Subroutine CALSLT (F1EE)
; ---------------------------------------------------------
PH_CASLT:	EXX
		EX      AF,AF'
		PUSH    IX
		POP     BC
		PUSH    IY
		POP     AF
J0390:		LD      C,A
R0391:		CALL    C0413
		BIT     7,C
		JR      Z,C03BF
R0398:		CALL    C03DF
		PUSH    BC
		PUSH    DE
		PUSH    AF
R039E:		CALL    C03BF
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
J03B3:		CALL    NZ,SSLOTE
		LD      (HL),E
		POP     AF                      ; restore IFF2 flag
R03B8:		JP      PO,J03BC                ; maskable interrupts where disabled, leave disabled
		EI
J03BC:		EX      AF,AF'
		EXX
		RET

; ---------------------------------------------------------
; Subroutine CALSLT on primary slot
; ---------------------------------------------------------
C03BF:		IN      A,(0A8H)
		PUSH    AF
		AND     (HL)
		INC     HL
		OR      (HL)
		EXX
		DI
		JP      CLPRIM

; ---------------------------------------------------------
; Subroutine ENASLT (F1F1)
; ---------------------------------------------------------
PH_ENASLT:	PUSH    HL
		LD      C,A
		LD      B,H
R03CD:		CALL    C0413
		BIT     7,C
R03D2:		CALL    NZ,C03DF
		IN      A,(0A8H)
		AND     (HL)
		INC     HL
		OR      (HL)
		DI
		OUT     (0A8H),A
		POP     HL
		RET

; ---------------------------------------------------------
; Subroutine change secondary slot register
; ---------------------------------------------------------
C03DF:		PUSH    HL
		AND     03H
		LD      E,A
		LD      HL,SLTTBL
		ADD     HL,DE
		PUSH    HL
		LD      A,C
		RRCA
		RRCA
		LD      C,A
R03EC:		CALL    C0413
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
J0409:		CALL    NZ,SSLOTL
		LD      A,(SS_TEMP)
		LD      C,A
		LD      A,H
		POP     HL
		RET

; ---------------------------------------------------------
; Subroutine get slot masks
; ---------------------------------------------------------
C0413:		AND     03H
		LD      D,A
		LD      A,B
		RRCA
		RRCA
		RRCA
		RRCA
		AND     0CH
		OR      D
		LD      E,A
		LD      D,00H
R0421:		LD      HL,I0427
		ADD     HL,DE
		ADD     HL,DE
		RET

I0427:		DEFB    0FCH,000H,0FCH,001H,0FCH,002H,0FCH,003H
		DEFB    0F3H,000H,0F3H,004H,0F3H,008H,0F3H,00CH
		DEFB    0CFH,000H,0CFH,010H,0CFH,020H,0CFH,030H
		DEFB    03FH,000H,03FH,040H,03FH,080H,03FH,0C0H

		DEPHASE

; ------------------------------------------------------------------------------
; Table with pointers to jump entries 0F1D3-F22D 
; indirect entry points
; ------------------------------------------------------------------------------
I485F:		DEFW    PH_RDLDI	; F1D3 RD_LDI / JUMPB transfer to/from page 1
		DEFW    PH_P0LDIR	; F1D6 P0_LDIR transfer with page 0
		DEFW    PH_P0CALL	; F1D9 P0_CALL call main-bios
		DEFW    PH_SFLUSH	; F1DC SFLUSH print string via chput
		DEFW    PH_GODRV	; F1DF GO_DRV interslot call with prompt handler
		DEFW    PH_JPVEC	; F1E2 JP_VEC start DOS1 style handler
		DEFW    PH_SIRQ		; F1E5 SIRQ KEYINT handler
		DEFW    PH_RDSLT	; F1E8 RDSLT handler
		DEFW    PH_WRSLT	; F1EB WRSLT handler
		DEFW    PH_CASLT	; F1EE CALSLT handler
		DEFW    PH_ENASLT	; F1F1 ENASLT handler
		DEFW    PH_CALLF	; F1F4 CALLF handler
		DEFW    PH_PUTBD	; F1F7 PUT_BD Enable BDOS segments
		DEFW    PH_PUTUS	; F1FA PUT_US Enable DOS segments
		DEFW    PH_P0RAM	; F1FD P0_RAM Enable DOS RAM on page 0
		DEFW    PH_ALLSEG	; F200 ALL_SEG
		DEFW    PH_FRESEG	; F203 FRE_SEG
		DEFW    PH_RDSEG	; F206 RD_SEG
		DEFW    PH_WRSEG	; F209 WR_SEG
		DEFW    PH_CALSEG	; F20C CAL_SEG
		DEFW    PH_CALLS	; F20F CALLS
		DEFW    PH_PUTPH	; F212 PUT_PH
		DEFW    PH_GETPH 	; F215 GET_PH
		DEFW    PH_PUTP0	; F218 PUT_P0
		DEFW    PH_GETP0	; F21B GET_P0
		DEFW    PH_PUTP1	; F21E PUT_P1
		DEFW    PH_GETP1	; F221 GET_P1
		DEFW    PH_PUTP2	; F224 PUT_P2
		DEFW    PH_GETP2	; F227 GET_P2
		DEFW    PH_PUTP3	; F22A PUT_P3
		DEFW    PH_GETP3	; F22D GET_P3
		DEFW    0


; ------------------------------------------------------------------------------
; Patchtable relocated code
; ------------------------------------------------------------------------------
I489F:		DEFW    R001C+1,R007D+1,R008D+1,R0094+1,R009B+1,R009F+1,R00AD+1,R00B1+1
		DEFW    R00C2+1,R00D1+1,R00D4+1,R00DC+1,R00F7+1,R00FD+1,R0123+1,R0129+1
		DEFW    R012F+1,R0138+1,R013E+1,R0146+1,R014E+1,R0156+1,R015D+1,R0163+1
		DEFW    R016C+1,R0172+1,R0178+1,R017E+1,R0185+1,R018C+1,R0195+1,R01A1+1
		DEFW    R01AC+1,R01AF+1,R01B5+1,R01D5+1,R01DE+1,R01E6+1,R01EA+1,R01F2+1
		DEFW    R01F6+1,R01FD+1,R0203+1,R0209+1,R0211+1,R0215+1,R0219+1,R0221+1
		DEFW    R0245+1,R0290+1,R0295+1,R029E+1,R02A9+1,R02AE+1,R02B7+1,R02D2+1
		DEFW    R02DA+1,R02E6+1,R0335+1,R0339+1,R033E+1,R0345+1,R034B+1,R0391+1
		DEFW    R0398+1,R039E+1,R03B8+1,R03CD+1,R03D2+1,R03EC+1,R0421+1
		DEFW	0
