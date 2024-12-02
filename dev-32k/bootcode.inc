; ------------------------------------------------------------------------------
; bootcode.inc
; MSXDOS 2.2 boot sector code starting at offset 0x1e
; Used in the format routine to initialize a floppy disk
;
; Code Copyrighted by ASCII and maybe others
; Source origin is the msxsyssrc repository by Arjen Zeilemaker
; Restructure, modifications and additional comments by H.J. Berends
;
; Sourcecode supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyrightholders
; ------------------------------------------------------------------------------

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
