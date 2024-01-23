; KANJI.ASM
;
; KANJI (BASIC + Extended BIOS) (special DOS2 version, runs only with DOS2!)
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

	SECTION	KANJI
        ORG     4000H

	INCLUDE "DISK.INC"

	IF HSH = 0
	DS	$6B,0			; rem: misssing code 
	ELSE
	DS	$100,0			; rem: misssing code
	ENDIF

CGTABL  EQU     0004H
VDP_DR  EQU     0006H
RDSLT   EQU     000CH
CALSLT  EQU     001CH
IDBYT2  EQU     002DH
WRTVDP  EQU     0047H
RDVRM   EQU     004AH
WRTVRM  EQU     004DH
FILVRM  EQU     0056H
LDIRMV  EQU     0059H
LDIRVM  EQU     005CH
CHGMOD  EQU     005FH
GICINI  EQU     0090H
CHSNS   EQU     009CH
CHGET   EQU     009FH
CHPUT   EQU     00A2H
LPTOUT  EQU     00A5H
LPTSTT  EQU     00A8H
BREAKX  EQU     00B7H
BEEP    EQU     00C0H
CLS     EQU     00C3H
TOTEXT  EQU     00D2H
PUTCHR  EQU     0139H
SNSMAT  EQU     0141H
KILBUF  EQU     0156H
CALBAS  EQU     0159H
EXTROM  EQU     015FH

INTRET  EQU     0D02H
M0DE3   EQU     0DE3H                   ; Table		keycodes for keys -,^,yen,@,[,;,:,],komma,.,/,_


C4092   EQU     4092H                   ; DOS2 bankswitch

ERROR	EQU     406FH                   ; BASIC error
READYR  EQU     409BH                   ; warm start BASIC
NEWSTT  EQU     4601H                   ; execution loop
CHRGTR  EQU     4666H                   ; get next BASIC character
FRMEVL  EQU     4C64H                   ; evaluate expression
DOCNVF  EQU     517AH                   ; convert DAC to new type
GETBYT  EQU     521CH                   ; evaluate byte operand
BUFLIN  EQU     5284H                   ; decode BASIC line
PTRGET  EQU     5EA4H                   ; locate variable
STRINI  EQU     6627H                   ; allocate temporary string
FRESTR  EQU     67D0H                   ; free temporary string with type check
CLSALL  EQU     6C1CH                   ; close all i/o channels


IF247   EQU     0F247H                  ; unused DOS2 workspace, used because SLTWKR entry already used by DOS2

KANJTA  EQU     0F30FH                  ; double byte character headers
MASTER  EQU     0F348H                  ; slotid disksystem master ROM
HIMSAV  EQU     0F349H                  ; disksystem bottom (lowest address used by the disksystem)
SDOSOF  EQU     0F36BH                  ; enable DOS RAM on page 1

WRPRIM  EQU     0F385H
WRPRIM6	EQU	WRPRIM+6
LINL40  EQU     0F3AEH
LINL32  EQU     0F3AFH
LINLEN  EQU     0F3B0H
CRTCNT  EQU     0F3B1H
CLMLST  EQU     0F3B2H
CLIKSW  EQU     0F3DBH
CSRY    EQU     0F3DCH
CSRX    EQU     0F3DDH
CNSDFG  EQU     0F3DEH
RG0SAV  EQU     0F3DFH
RG1SAV  EQU     0F3E0H
RG2SAV  EQU     0F3E1H
RG7SAV  EQU     0F3E6H
TRGFLG  EQU     0F3E8H
FORCLR  EQU     0F3E9H
BAKCLR  EQU     0F3EAH
SCNCNT  EQU     0F3F6H
REPCNT  EQU     0F3F7H
PUTPNT  EQU     0F3F8H
GETPNT  EQU     0F3FAH
RAWPRT  EQU     0F418H
CURLIN  EQU     0F41CH
BUFMIN  EQU     0F55DH
BUF     EQU     0F55EH
TTYPOS  EQU     0F661H
VALTYP  EQU     0F663H
TXTTAB  EQU     0F676H
DSCTMP  EQU     0F698H
AUTFLG  EQU     0F6AAH
OLDLIN  EQU     0F6BEH
OLDTXT  EQU     0F6C0H
SWPTMP  EQU     0F7BCH
DECCNT  EQU     0F7F4H
DAC     EQU     0F7F6H
HOLD8   EQU     0F806H
MAXFIL  EQU     0F85FH
FILTAB  EQU     0F860H
FNKSTR  EQU     0F87FH
ACPAGE  EQU     0FAF6H
EXBRSA  EQU     0FAF8H
CHRCNT  EQU     0FAF9H
MODE    EQU     0FAFCH
NORUSE  EQU     0FAFDH
HOKVLD  EQU     0FB20H
ENSTOP  EQU     0FBB0H
BASROM  EQU     0FBB1H
LINTTB  EQU     0FBB2H
FSTPOS  EQU     0FBCAH
FNKSWI  EQU     0FBCDH
FNKFLG  equ     0FBCEH
ONGSBF  EQU     0FBD8H
CLIKFL  EQU     0FBD9H
OLDKEY  EQU     0FBDAH
NEWKEY  EQU     0FBE5H
KEYBUF  EQU     0FBF0H
LINWRK  EQU     0FC18H
PATWRK  EQU     0FC40H
HIMEM   EQU     0FC4AH
TRPTBL  EQU     0FC4CH
INTFLG  EQU     0FC9BH
ESCCNT  EQU     0FCA7H
INSFLG  EQU     0FCA8H
CSRSW   EQU     0FCA9H
CSTYLE  EQU     0FCAAH
CAPST   EQU     0FCABH
KANAST  EQU     0FCACH
KANAMD  EQU     0FCADH
SCRMOD  EQU     0FCAFH
OLDSCR  EQU     0FCB0H
EXPTBL  EQU     0FCC1H
SLTWRK  EQU     0FD09H
PROCNM  EQU     0FD89H

H_TIMI  equ     0FD9FH
H_CHPU  equ     0FDA4H
H_DSPC  equ     0FDA9H
H_ERAC  equ     0FDAEH
H_DSPF  equ     0FDB3H
H_ERAF  equ     0FDB8H
H_TOTE  equ     0FDBDH
H_CHGE  EQU     0FDC2H
H_KEYC  EQU     0FDCCH
H_KEYA  EQU     0FDD1H
H_PINL  equ     0FDDBH
H_INLI  equ     0FDE5H
H_WIDT  EQU     0FF84H
H_PHYD  EQU     0FFA7H
H_LPTO  EQU     0FFB6H
H_SCRE  EQU     0FFC0H
EXTBIO  EQU     0FFCAH
RG8SAV  EQU     0FFE7H
RG9SAV  EQU     0FFE8H
RG23SA  EQU     0FFF6H
RG25SA  EQU     0FFFAH


C4100:  JP      J45CC			; TOTEXT handler
C4103:  JP      J47C3			; CHPUT handler
C4106:  JP      J4908			; display cursor handler
C4109:  JP      J491F			; erase cursor handler
C410C:  JP      J4AF3			; erase function keys handler
C410F:  JP      J4B17			; display function keys handler
C4112:  JP      J4D56			; PINLIN handler
C4115:  JP      J4D6C			; INLIN handler
C4118:  JP      J6BEF			; LPTOUT handler
C411B:  JP      J45AD			; CHGET handler
C411E:  JP      J44E0			; WIDTH handler
C4121:  JP      J453C			; SCREEN handler
C4124:  JP      J412D			; statement handler
C4127:  JP      J414B			; extended bios handler
C412A:  JP      J418C			; timer interrupt handler

;         Subroutine STATEMENT handler
;            Inputs  ________________________
;            Outputs ________________________


J412D:  LD      A,(IDBYT2)
        CP      02H                     ; MSX2+ or higher ?
        CCF
        RET     C                       ; yep, quit
        LD      A,(H_PHYD+0)
        CP      0C9H                    ; disk environment ?
        SCF
        RET     Z                       ; nope, quit
        PUSH    HL
        PUSH    BC
        CALL    C456A                   ; GETSLT
        POP     BC
        LD      HL,MASTER
        CP      (HL)                    ; I am the disksystem master ROM ?
        POP     HL
        SCF
        RET     NZ                      ; nope, quit statement handler
        JP      J422B

;         Subroutine EXTBIO handler
;            Inputs  ________________________
;            Outputs ________________________


J414B:  PUSH    AF
        LD      A,D
        CP      11H                     ; KANJI EXTBIO ?
        JR      NZ,J4181                ; nope, quit
        CALL    C41F9                   ; get KANJI workspace
        LD      A,(IX+0)
        OR      A                       ; KANJI enabled ?
        JR      Z,J4181                 ; nope, quit
        LD      A,E
        OR      A                       ; function 0 ?
        JR      NZ,J416E
        POP     AF
        XOR     A
        BIT     5,(IX+1)                ; in KANJI mode ?
        RET     Z                       ; nope, quit (mode 0, ANK)
        LD      A,(IX+0)
        RLCA
        RLCA
        AND     03H                     ; KANJI mode
        INC     A
        RET

J416E:  DEC     A                       ; function 1 ?
        JR      NZ,J4181                ; nope, quit
        POP     AF
        CP      4+1
        RET     NC
        PUSH    AF
        PUSH    BC
        PUSH    DE
        PUSH    HL
        CALL    C4183                   ; select KANJI mode
        POP     HL
        POP     DE
        POP     BC
        POP     AF
        RET

J4181:  POP     AF
        RET

;         Subroutine Select KANJI mode
;            Inputs  A = mode (0= ANK, 1= ...)
;            Outputs ________________________


C4183:  DEC     A                       ; ANK mode ?
        JP      M,C447E                 ; yep,
        RRCA
        RRCA
        JP      J42EA                   ; select KANJI mode

;         Subroutine H_TIMI handler
;            Inputs  ________________________
;            Outputs ________________________


J418C:  PUSH    AF
        CALL    C41F9                   ; get KANJI workspace
        BIT     5,(IX+1)                ; in KANJI mode ?
        JR      Z,J419D                 ; nope, quit
        CALL    C541B                   ; keyboard scan
        LD      HL,SCNCNT
        INC     (HL)
J419D:  POP     AF
        RET

;         Subroutine initialize hooks
;            Inputs  ________________________
;            Outputs ________________________


C419F:  PUSH    HL
        CALL    C456A                   ; GETSLT
        LD      C,A
        POP     HL
        LD      IX,C4100-040H
J41A9:  LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        LD      A,E
        OR      D
        RET     Z
        LD      A,0F7H
        LD      (DE),A
        INC     DE
        LD      A,C
        LD      (DE),A
        INC     DE
        PUSH    IX
        EX      (SP),HL
        LD      A,L
        LD      (DE),A
        INC     DE
        LD      A,H
        LD      (DE),A
        INC     DE
        INC     HL
        INC     HL
        INC     HL
        EX      (SP),HL
        POP     IX
        LD      A,0C9H
        LD      (DE),A
        JR      J41A9

;         Subroutine clear hooks
;            Inputs  ________________________
;            Outputs ________________________


C41CB:  LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        LD      A,E
        OR      D
        RET     Z
        LD      A,0C9H
        LD      (DE),A
        INC     DE
        LD      (DE),A
        INC     DE
        LD      (DE),A
        INC     DE
        LD      (DE),A
        INC     DE
        LD      (DE),A
        JR      C41CB

I41DF:	DEFW	H_TOTE
        DEFW	H_CHPU
        DEFW	H_DSPC
        DEFW	H_ERAC
        DEFW	H_ERAF
        DEFW	H_DSPF
        DEFW	H_PINL
        DEFW	H_INLI
        DEFW	H_LPTO
        DEFW	H_CHGE
        DEFW	H_WIDT
        DEFW	H_SCRE
        DEFW	0

;         Subroutine get KANJI workspace
;            Inputs  ________________________
;            Outputs ________________________


C41F9:  LD      IX,IF247                ; unused space in DOS2
        RET

;         Subroutine call BASIC routine
;            Inputs  ________________________
;            Outputs ________________________


C41FE:  EXX
        EX      AF,AF'
        LD      HL,-16
        ADD     HL,SP
        LD      SP,HL
        PUSH    HL
        EX      DE,HL
        LD      HL,I421A
        LD      BC,16
        LDIR
        POP     HL
        PUSH    DE
        CALL    C4219
        POP     HL
        LD      SP,HL
        EX      AF,AF'
        EXX
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4219:  JP      (HL)

I421A:  XOR     A
        CALL    C4092
        EXX
        EX      AF,AF'
        CALL    CALBAS
        EX      AF,AF'
        EXX
        LD      A,03H   ; 3 
        JP      C4092


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C422A:  RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


J422B:  EI
        PUSH    HL
        LD      HL,I4254
J4230:  LD      DE,PROCNM
J4233:  LD      A,(DE)
J4234:  CP      (HL)
        JR      NZ,J4247
        INC     DE
        INC     HL
        AND     A
        JR      NZ,J4233
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        POP     HL
        CALL    C4245
        AND     A
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4245:  PUSH    DE
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


J4247:  LD      C,0FFH
        XOR     A
        CPIR
        INC     HL
        INC     HL
        CP      (HL)
        JR      NZ,J4230
        POP     HL
        SCF
        RET

I4254:  DEFB    "PALETTE",0
        DEFW    C71E9
        DEFB    "CLS",0
        DEFW    C479C
        DEFB    "AKCNV",0
        DEFW    C6CCD
        DEFB    "JIS",0
        DEFW    C6E63
        DEFB    "SJIS",0
        DEFW    C6E4C
        DEFB    "KACNV",0
        DEFW    C6DB4
        DEFB    "KEXT",0
        DEFW    C6EA2
        DEFB    "KINSTR",0
        DEFW    C6FA2
        DEFB    "KLEN",0
        DEFW    C7044
        DEFB    "KMID",0
        DEFW    C6EFB
        DEFB    "KNJ",0
        DEFW    C6F6B
        DEFB    "KTYPE",0
        DEFW    C7086
        DEFB    "KANJI",0
        DEFW    C42E0
        DEFB    "KANJI0",0
        DEFW    C42E0
        DEFB    "KANJI1",0
        DEFW    C42E2
        DEFB    "KANJI2",0
        DEFW    C42E5
        DEFB    "KANJI3",0
        DEFW    C42E8
        DEFB    "ANK",0
        DEFW    C447E
        DEFB    0
 
;         Subroutine _KANJI / _KANJI0
;            Inputs  ________________________
;            Outputs ________________________


C42E0:  XOR     A
        DEFB    001H

;         Subroutine _KANJI1
;            Inputs  ________________________
;            Outputs ________________________


C42E2:  LD      A,040H
        DEFB    001H

;         Subroutine _KANJI2
;            Inputs  ________________________
;            Outputs ________________________


C42E5:  LD      A,080H
        DEFB    001H

;         Subroutine _KANJI3
;            Inputs  ________________________
;            Outputs ________________________


C42E8:  LD      A,0C0H

;         Subroutine Select KANJI mode
;            Inputs  A = mode (b7,b6)
;            Outputs ________________________


J42EA:  PUSH    HL
        PUSH    AF                      ; store mode
        LD      HL,I41DF
        CALL    C419F                   ; initialize hooks
        CALL    C41F9                   ; get KANJI workspace
        SET     5,(IX+1)                ; flag in KANJI mode
        POP     BC                      ; restore mode
        LD      A,(IX+0)
        AND     A                       ; KANJI already enabled ?
        PUSH    AF
        LD      (IX+0),B
        LD      A,24
        LD      (LINTTB+0),A
        XOR     A
        OUT     (0D8H),A
        LD      A,02H
        OUT     (0D9H),A
        LD      HL,I4476
        LD      B,8
J4313:  IN      A,(0D9H)
        CP      (HL)
        JR      NZ,J431F
        INC     HL
        DJNZ    J4313
        SET     3,(IX+0)                ; JIS1 rom found
J431F:  IN      A,(40H)
        PUSH    AF
        LD      A,0F7H
        OUT     (40H),A                 ; DeviceID KANJI12
        LD      B,1
J4328:  IN      A,(40H)
        CP      08H                     ; Inverted DeviceID KANJI12 ?
        JR      Z,J4357                 ; yep, KANJI12 found
        LD      A,01H
        OUT     (42H),A                 ; ?
        XOR     A
        OUT     (47H),A                 ; row 0
        OUT     (48H),A                 ; col 0
        LD      B,A
        IN      A,(49H)
        LD      C,A
        IN      A,(49H)
        OR      C
        JR      NZ,J4357                ; no KANJI12 JIS1 rom
        LD      A,0EH
        OUT     (47H),A                 ; row 14
        LD      A,5FH
        OUT     (48H),A                 ; col 05FH
        LD      C,00H
        LD      B,8
J434C:  IN      A,(49H)
        ADD     A,C
        LD      C,A
        DJNZ    J434C                   ; calculate checksum
        CP      4CH
        JR      NZ,J4357                ; wrong checksum, no KANJI12 JIS1 rom
        INC     B
J4357:  DJNZ    J435D
        SET     4,(IX+0)                ; kanji 12 JIS1 rom found
J435D:  LD      B,1
        IN      A,(41H)
        CP      08H                     ; Inverted DeviceID KANJI12 ?
        JR      Z,J437F                 ; yep, KANJI12 JIS12 rom
        LD      A,02H
        OUT     (42H),A                 ; ?
        LD      A,25H
        OUT     (47H),A                 ; row 025H
        LD      A,5EH
        OUT     (48H),A                 ; col 05EH
        DEC     B
        IN      A,(49H)
        SUB     41H
        LD      C,A
        IN      A,(49H)
        SUB     42H
        OR      C
        JR      NZ,J437F
        INC     B
J437F:  DJNZ    J4385
        SET     6,(IX+1)                ; kanji 12 JIS2 rom found
J4385:  POP     AF
        CPL
        OUT     (40H),A                 ; select orginal DeviceID again
        LD      A,3EH
        OUT     (0DAH),A
        LD      A,35H
        OUT     (0DBH),A
        LD      BC,256*8+0
J4394:  IN      A,(0DBH)
        ADD     A,C
        LD      C,A
        DJNZ    J4394
        CP      95H
        JR      NZ,J43A3
        LD      HL,MODE
        SET     6,(HL)                  ; JIS2 rom found
J43A3:  LD      A,(H_PHYD+0)
        CP      0C9H                    ; disk environment ?
        JR      Z,J43B6                 ; nope, skip
        LD      HL,0A080H
        LD      (KANJTA+0),HL
        LD      HL,0FDE0H
        LD      (KANJTA+2),HL           ; enable double byte character support
J43B6:  CALL    C442D                   ; calculate and set kanji screenwidth
        CALL    C422A                   ; nothing (was hook H_TIMI)
        POP     AF
        POP     HL
        JR      NZ,C4418                ; KANJI already enabled, reset CHPUT variables
        LD      (SWPTMP+0),HL           ; save BASIC pointer
        CALL    C422A                   ; nothing (was hook EXTBIO)
        LD      IX,CLSALL
        CALL    C41FE                   ; close all i/o channels
        CALL    C6800                   ; initialize MSX-JE (when available)
        CALL    C,C7474                 ; no MSX-JE, initialize own simple japanese input frontend processor (do nothing)
        PUSH    AF                      ; store MSX-JE found
        CALL    C4418                   ; reset CHPUT variables
        POP     AF                      ; restore MSX-JE found
        LD      HL,(SWPTMP+0)           ; restore BASIC pointer
        RET     C                       ; no japanese input frontend processor, quit
        PUSH    HL                      ; store BASIC pointer
        LD      HL,(CURLIN)
        LD      (OLDLIN),HL             ; save current line for CONT
        LD      HL,I445B
        LD      DE,HOLD8
        LD      BC,27
        LDIR                            ; BASIC routine in HOLD8
        LD      A,(MAXFIL)
        LD      (I4460-I445B+HOLD8),A   ; MAXFILES
        POP     HL                      ; restore BASIC pointer
        LD      A,L
        LD      (I4468-I445B+HOLD8),A
        LD      A,H
        LD      (I4470-I445B+HOLD8),A   ; OLDTXT for CONT
        XOR     A                       ; number of user i/o channels = 0
        LD      (MAXFIL),A              ; update user i/o channels
        LD      HL,BUF
        LD      (FILTAB),HL             ; temporary i/o channel table in BUF
        LD      HL,BUF+2
        LD      (BUF+0),HL              ; pointer to system i/o channel
        LD      (HL),A                  ; i/o channel closed
        LD      HL,HOLD8
        LD      IX,NEWSTT
        JP      C41FE                   ; execute BASIC routine in HOLD8 (to reinitialize buffers)


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4418:  XOR     A
        LD      IX,CHPUT
        JP      C4561                  ; call bios routine


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4420:  LD      A,(H_PHYD+0)
        CP      0C9H                    ; disk environment ?
        JR      Z,C442D                 ; nope, continue
        LD      A,(SDOSOF+0)
        CP      0C3H                    ; MSXDOS ?
        RET     Z                       ; yep, quit

;         Subroutine calculate and set kanji screenwidth
;            Inputs  ________________________
;            Outputs ________________________


C442D:  PUSH    HL
        CALL    C44D3                  ; get current screen width
        BIT     6,(IX+0)
        JR      NC,J4443               ; screen 0,
        JR      Z,J4453
        LD      E,A
        ADD     A,A
        ADD     A,A
        ADD     A,E                     ; *5
        RRCA
        RRCA                            ; /4
        AND     3FH
        JR      J4453
J4443:  JR      NZ,J4453
J4445:  LD      L,A
        LD      H,0
        ADD     HL,HL
        ADD     HL,HL                   ; *4
        LD      DE,-5
        LD      A,-1
J444F:  ADD     HL,DE
        INC     A
        JR      C,J444F                ; /5
J4453:  LD      (LINLEN),A
        LD      (IX+6),A
        POP     HL
        RET

I445B:  DEFB    ":"
        DEFB    0CDH                    ; MAX
        DEFB    0B7H                    ; FILES
        DEFB    0EFH                    ; =
        DEFB    00FH                    ; byte number token
I4460:  DEFB    0                       ; 0
        DEFB    ":"                     ; next statement
        DEFB    098H                    ; POKE
        DEFB    00CH                    ; hexadecimal number token
        DEFW    OLDTXT+0                ; &HF6C0
        DEFB    ","
        DEFB    00FH                    ; byte number token
I4468:  DEFB    0                       ; 0
        DEFB    ":"                     ; next statement
        DEFB    098H                    ; POKE
        DEFB    00CH                    ; hexadecimal number token
        DEFW    OLDTXT+1                ; &HF6C1
        DEFB    ","
        DEFB    00FH                    ; byte number token
I4470:  DEFB    0                       ; 0
        DEFB    ":"                     ; next statement
        DEFB    099H                    ; CONT
        DEFB    0                       ; end of line
        DEFW    0                       ; end of program

I4476:  DEFB    000H,040H,020H,010H,008H,004H,002H,001H

;         Subroutine _ANK
;            Inputs  ________________________
;            Outputs ________________________


C447E:  CALL    C41F9                   ; get KANJI workspace
        BIT     5,(IX+1)                ; in KANJI mode ?
        RET     Z                       ; nope, quit
        RES     5,(IX+1)                ; reset flag in KANJI mode
        PUSH    HL
        LD      HL,I41DF
        CALL    C41CB                  ; clear HOOKS
        CALL    C422A
        CALL    C6C92
        CALL    C7E50
        JR      Z,J44A3                ; No MSX-JE
        BIT     0,(IY+8)
        CALL    NZ,C68D3
J44A3:  LD      (IY+8),0
        CALL    C44D3                  ; get current screen width
        LD      (LINLEN),A
        LD      A,24
        LD      (CRTCNT),A
        LD      A,2
        LD      (SCRMOD),A
        LD      IX,TOTEXT
        CALL    C4561                  ; call bios routine
        CALL    C4540                  ; interlaced off, even/odd off, sprites on
        LD      A,(H_PHYD+0)
        CP      0C9H                    ; disk environment ?
        JR      Z,J44D1                ; nope,
        LD      HL,0
        LD      (KANJTA+0),HL
        LD      (KANJTA+2),HL           ; disable double byte character support
J44D1:  POP     HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C44D3:  LD      A,(OLDSCR)
        AND     A
        LD      A,(LINL40)
        RET     Z
        LD      A,(LINL32)
        SCF
        RET

;         Subroutine WIDTH handler
;            Inputs  ________________________
;            Outputs ________________________


J44E0:  EI
        CP      1AH
        JR      C,J4518
        CALL    C41F9                  ; get KANJI workspace
        LD      A,(SCRMOD)
        CP      2
        JR      NC,J4518
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        LD      A,E
        JR      NZ,J4508               ; nope, MSX2 or higher
        BIT     6,(IX+0)
        JR      NZ,J4502
        CP      32+1
        JR      C,J451B
        JR      J4518
J4502:  CP      40+1
        JR      C,J451B
        JR      J4518
J4508:  BIT     6,(IX+0)
        JR      NZ,J4514
        CP      64+1
        JR      C,J451B
        JR      J4518
J4514:  CP      80+1
        JR      C,J451B
J4518:  XOR     A
        LD      E,A
        RET
J451B:  LD      (LINLEN),A
        LD      A,(RG0SAV)
        OR      08H                     ; M5 = 1
        LD      (RG0SAV),A
        CALL    C45E5
        LD      A,(LINLEN)
J452C:  SUB     0EH     ; 14 
        JR      NC,J452C
        ADD     A,1CH
        CPL
        INC     A
        ADD     A,E
        LD      (CLMLST),A
        CALL    C4583                   ; take control from hook caller
        RET

;         Subroutine SCREEN handler
;            Inputs  ________________________
;            Outputs ________________________


J453C:  RET     Z                       ; SCREEN without parameters, quit
        CP      ','
        RET     Z                       ; SCREEN without screennumber, quit
                                        ; interlaced off, even/odd off, sprites on
                                        ; and execute normal SCREEN command

;         Subroutine interlaced off, even/odd off, sprites on
;            Inputs  ________________________
;            Outputs ________________________


C4540:  PUSH    AF
        LD      A,(RG9SAV)
        AND     0F3H                    ; interlaced off, even/odd off
        LD      C,9
        CALL    C4557
        LD      A,(RG8SAV)
        AND     0FDH                    ; sprites on
        LD      C,8
        CALL    C4557
        POP     AF
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4557:  LD      B,A
        LD      A,(EXBRSA)
        AND     A
        RET     Z
        LD      IX,WRTVDP


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4561:  LD      IY,(EXPTBL-1)
        CALL    CALSLT
        EI
        RET

;         Subroutine GETSLT
;            Inputs  ________________________
;            Outputs ________________________


C456A:  IN      A,(0A8H)
        RRCA
        RRCA
        AND     03H
        LD      HL,EXPTBL
        LD      B,0
        LD      C,A
        ADD     HL,BC
        OR      (HL)
        RET     P
        LD      C,A
        INC     HL
        INC     HL
        INC     HL
        INC     HL
        LD      A,(HL)
        AND     0CH
        OR      C
        RET

;         Subroutine take control from hook caller
;            Inputs  ________________________
;            Outputs ________________________


C4583:  PUSH    HL
        PUSH    AF
        LD      A,(H_TOTE+1)
        ADD     A,A
        LD      HL,14
        JR      NC,J4590
        LD      L,14+8
J4590:  ADD     HL,SP
        LD      (HL),WRPRIM6 % 256		; (rem: LOW (WRPRIM+6))
        INC     HL
        LD      (HL),WRPRIM6 / 256		; (rem: HIGH (WRPRIM+6))
        POP     AF
        POP     HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4599:  PUSH    HL
        PUSH    AF
        LD      A,(H_TOTE+1)
        ADD     A,A
        LD      HL,14
        JR      NC,J45A6
        LD      L,14+8
J45A6:  ADD     HL,SP
        LD      (HL),E
        INC     HL
        LD      (HL),D
        POP     AF
        POP     HL
        RET

;         Subroutine CHGET handler
;            Inputs  ________________________
;            Outputs ________________________


J45AD:  EI
        LD      DE,INTRET+11
        CALL    C4599                   ; replace return address
        CALL    C41F9                   ; get KANJI workspace
        XOR     A
        CALL    C45EA


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C45BB:  LD      A,0C9H
        LD      (H_CHGE+0),A
        CALL    C41F9                   ; get KANJI workspace
        CALL    C68F0
        LD      HL,H_CHGE+0
        LD      (HL),0F7H
        RET

;         Subroutine TOTEXT handler
;            Inputs  ________________________
;            Outputs ________________________


J45CC:  EI
        CALL    C4583                   ; take control from hook caller
        CALL    C41F9                   ; get KANJI workspace
        LD      A,(NORUSE)
        BIT     7,A
        RET     NZ
        LD      A,(OLDSCR)
        LD      (SCRMOD),A
        LD      A,(IX+6)
        LD      (LINLEN),A


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C45E5:  LD      A,24
        LD      (LINTTB+0),A


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C45EA:  PUSH    AF
        LD      A,(RG0SAV)
        AND     0EH                     ; M5=0, M4=0, M3=0 ?
        JR      Z,J4606
        CP      04H                     ; M5=0, M4=1, M3=0 ?
        JR      NZ,J45FD
        LD      A,(RG1SAV)
        AND     18H                     ; M1, M2
        JR      J460D
J45FD:  LD      A,(LINTTB+0)
        CP      24
        JR      Z,J4614
        JR      J4621
J4606:  LD      A,(RG1SAV)
        AND     18H                     ; M1=0, M2=0 ?
        JR      Z,J4611                 ; screen 1,
J460D:  CP      10H                     ; M1=1, M2=0 ?
        JR      NZ,J45FD                ; not screen 0,
J4611:  CALL    C4420                   ; calculate and set kanji screenwidth (if not in MSXDOS)
J4614:  LD      A,0FFH
        LD      (LINTTB+0),A
        LD      A,(IX+1)
        OR      0FH
        LD      (IX+1),A
J4621:  PUSH    HL
        LD      A,(SCRMOD)
        CP      1
        JR      NZ,J462A
        XOR     A
J462A:  LD      L,A
        LD      A,(IX+1)
        LD      H,A
        AND     0F0H
        OR      L
        LD      (IX+1),A
        LD      A,H
        AND     0FH
        CP      L
        JP      Z,J4759
        PUSH    DE
        PUSH    BC
        RES     5,(IX+0)
        LD      A,L
        CP      02H
        JR      NC,J4686
        SET     5,(IX+0)
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        LD      B,2
        JR      Z,J4672                 ; yep, use screenmode 2
        BIT     6,(IX+0)
        LD      A,(LINLEN)
        JR      NZ,J4660
        CP      32+1
        JR      J4662
J4660:  CP      40+1
J4662:  LD      B,5
        JR      C,J4672
        LD      A,(MODE)
        AND     06H
        CP      04H                     ; 128 KB VRAM ?
        LD      B,7
        JR      Z,J4672                 ; yep, use screenmode 7
        DEC     B                       ; nope, use screenmode 6
J4672:  PUSH    BC
        LD      A,B
        CALL    CHGMOD
        XOR     A
        LD      (SCRMOD),A
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        LD      A,(RG8SAV)
        SET     1,A                     ; sprites off
        JR      J469F
J4686:  LD      B,A
        LD      A,(IX+1)
        AND     0F0H
        OR      B
        LD      (IX+1),A
        LD      A,0FFH
        LD      (LINLEN),A
        PUSH    BC
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        LD      A,(RG8SAV)
        RES     1,A                     ; sprites on
J469F:  LD      B,A
        LD      C,08H
        CALL    NZ,WRTVDP               ; MSX2 or higher,
        POP     AF
        DEC     A
        LD      C,A
        LD      A,(IX+0)
        AND     0F8H
        OR      C
        LD      (IX+0),A
        RRCA
        RRCA
        AND     30H                     ; KANJI mode
        SLA     C
        OR      C
        LD      C,A
        LD      B,0
        LD      HL,I475C
        ADD     HL,BC
        LD      A,(LINLEN)
        CP      1AH
        JR      NC,J46CB
        LD      A,1AH
        LD      (LINLEN),A
J46CB:  CP      (HL)
        JR      C,J46D2
        LD      A,(HL)
        LD      (LINLEN),A
J46D2:  LD      B,A
        LD      A,(SCRMOD)
        CP      2
        LD      A,B
        JR      NC,C46DE
        LD      (IX+6),A


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C46DE:  INC     HL
        LD      A,(HL)
        LD      (CRTCNT),A
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        JR      Z,J472B                ; yep,
        LD      A,(SCRMOD)
        CP      2
        JR      C,J46F7
        LD      A,(NORUSE)
        BIT     7,A
        JR      Z,J472B
J46F7:  LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        LD      A,(RG2SAV)
        JR      C,J4718
        BIT     7,(IX+0)
        JR      Z,J4718
        OR      20H
        LD      B,A
        LD      C,02H   ; 2 
        CALL    WRTVDP
        LD      A,(RG9SAV)
        OR      0CH                     ; interlaced on, even/odd on
        JR      J4725
J4718:  AND     0DFH
        LD      B,A
        LD      C,02H   ; 2 
        CALL    WRTVDP
        LD      A,(RG9SAV)
        AND     0F3H                    ; interlaced off, even/odd off
J4725:  LD      B,A
        LD      C,09H   ; 9 
        CALL    WRTVDP
J472B:  LD      A,(RG7SAV)
J472E:  AND     0F0H
        LD      HL,BAKCLR
        OR      (HL)
        LD      B,A
        LD      C,07H   ; 7 
        LD      A,(SCRMOD)
        CP      2
        CALL    C,WRTVDP
        LD      HL,0101H
        LD      (CSRY),HL
        POP     BC
        POP     DE
        LD      A,(SCRMOD)
        CP      2
        JR      NC,J4759
        POP     HL
        POP     AF
        PUSH    AF
        PUSH    HL
        CP      0CH
        LD      A,0CH
        CALL    NZ,C47B7                ; KANJI character to screen (registers saved)
J4759:  POP     HL
        POP     AF
        RET

I475C:  DEFB    0,0
        DEFB    32,12
        DEFB    8,3
        DEFB    32,12
        DEFB    32,13
        DEFB    64,13
        DEFB    64,13
        DEFB    32,13
        DEFB    0,0
        DEFB    40,12
        DEFB    10,3
        DEFB    40,12
        DEFB    40,13
        DEFB    80,13
        DEFB    80,13
        DEFB    40,13
        DEFB    0,0
        DEFB    32,12
        DEFB    8,3
        DEFB    32,12
        DEFB    32,24
        DEFB    64,24
        DEFB    64,24
        DEFB    32,24
        DEFB    0,0
        DEFB    40,12
        DEFB    10,3
        DEFB    40,12
        DEFB    40,24
        DEFB    80,24
        DEFB    80,24
        DEFB    40,24

;         Subroutine _CLS
;            Inputs  ________________________
;            Outputs ________________________

C479C:  LD      A,(SCRMOD)
        CP      2                       ; graphic screen ?
        JR      NC,J47B3                ; yep, normal CLS
        CALL    C41F9                   ; get KANJI workspace
        BIT     5,(IX+1)                ; in KANJI mode ?
        JR      Z,J47B3                 ; nope, normal CLS
        LD      A,0CH
J47AE:  CALL    C45EA
        JR      C47B7                   ; KANJI character to screen (registers saved)

J47B3:  XOR     A
        JP      CLS

;         Subroutine KANJI character to screen (registers saved)
;            Inputs  ________________________
;            Outputs ________________________


C47B7:  PUSH    HL
        PUSH    DE
        PUSH    BC
        PUSH    AF
        CALL    C47D2                   ; KANJI character to screen
        POP     AF
        POP     BC
        POP     DE
        POP     HL
        RET

;         Subroutine CHPUT handler
;            Inputs  ________________________
;            Outputs ________________________


J47C3:  EI
        CALL    C41F9                   ; get KANJI workspace
        CALL    C45EA
        LD      C,A
        LD      DE,INTRET+10
        CALL    C4599                   ; replace return address
        LD      A,C

;         Subroutine KANJI character to screen
;            Inputs  ________________________
;            Outputs ________________________


C47D2:  PUSH    AF
        CALL    C4918
        POP     AF
J47D7:  CALL    C47E5
        CALL    C4901
        LD      A,(CSRX)
        DEC     A
        LD      (TTYPOS),A
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C47E5:  LD      C,A
        LD      HL,ESCCNT
J47E9:  LD      A,(HL)
        AND     A
        JP      NZ,J48AF
        LD      A,C
        CP      20H
        JR      C,J4830
        LD      HL,(CSRY)
        CP      7FH
        JP      Z,J4A66
        BIT     7,(IX+2)
        JR      NZ,J4814
        CALL    C4982
        JR      C,J4817
        LD      A,(LINLEN)
        CP      H
        PUSH    BC
        LD      A,0FFH
        CALL    Z,C47E5
        POP     BC
        JP      C4D13
J4814:  CALL    C499D
J4817:  CALL    C4D13
        CALL    C499D
        RET     NZ
        XOR     A
        CALL    C4CE7
        LD      H,01H   ; 1 
C4824:  CALL    C49CE
        RET     NZ
        CALL    C49D6
        LD      L,01H   ; 1 
        JP      C49F5

J4830:  LD      (IX+2),00H
        LD      HL,I484F-2
        LD      C,12

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4839:  INC     HL
        INC     HL
        AND     A
        DEC     C
        RET     M
        CP      (HL)
        INC     HL
        JR      NZ,C4839
        LD      C,(HL)
        INC     HL
        LD      B,(HL)
        LD      HL,(CSRY)
        CALL    C484D
        XOR     A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C484D:  PUSH    BC
        RET

I484F:  DEFB    7
        DEFW    BEEP
        DEFB    8
        DEFW    C49A5
        DEFB    9
        DEFW    C49DE
        DEFB    10
        DEFW    C4824
        DEFB    11
        DEFW    C49EC
        DEFB    12
        DEFW    C4B07
        DEFB    13
        DEFW    C49EE
        DEFB    27
        DEFW    C48A9
        DEFB    28
        DEFW    C49C8
        DEFB    29
        DEFW    C49A5
        DEFB    30
        DEFW    C49C4
        DEFB    31
        DEFW    C49CE

I4873:  DEFB    "j"
        DEFW    C4B07
        DEFB    "E"
        DEFW    C4B07
        DEFB    "K"
        DEFW    C4A75
        DEFB    "J"
        DEFW    C4ACF
        DEFB    "l"
        DEFW    C4A73
        DEFB    "L"
        DEFW    C4A2C
        DEFB    "M"
        DEFW    C49F2
        DEFB    "Y"
        DEFW    C48A6
        DEFB    "A"
        DEFW    C49C4
        DEFB    "B"
        DEFW    C49CE
        DEFB    "C"
        DEFW    C499D
        DEFB    "D"
        DEFW    C49C2
        DEFB    "H"
        DEFW    C49EC
        DEFB    "x"
        DEFW    C48A0
        DEFB    "y"
        DEFW    C48A3

C48A0:  LD      A,1
        DEFB    001H
C48A3:  LD      A,2
        DEFB    001H
C48A6:  LD      A,4
        DEFB    001H
C48A9:  LD      A,0FFH
        LD      (ESCCNT),A
        RET

J48AF:  JP      P,J48BD
        LD      (HL),00H
        LD      A,C
        LD      HL,I4873-2
        LD      C,15
        JP      C4839

J48BD:  DEC     A
        JR      Z,J48DE
        DEC     A
        JR      Z,J48E8
        DEC     A
        LD      (HL),A
        LD      A,(LINLEN)
        LD      DE,CSRX
        JR      Z,J48D3
        LD      (HL),03H        ; 3 
        CALL    C4CEE
        DEC     DE
J48D3:  LD      B,A
        LD      A,C
        SUB     20H     ; " "
        CP      B
        INC     A
        LD      (DE),A
        RET     C
        LD      A,B
        LD      (DE),A
J48DD:  RET
J48DE:  LD      (HL),A
        LD      A,C
        SUB     34H     ; "4"
        JR      Z,J48EF
        DEC     A
        JR      Z,J48F6
        RET
J48E8:  LD      (HL),A
        LD      A,C
        SUB     34H     ; "4"
        JR      NZ,J48F3
        INC     A
J48EF:  LD      (CSTYLE),A
        RET
J48F3:  DEC     A
        RET     NZ
        INC     A
J48F6:  LD      (CSRSW),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C48FA:  LD      A,(CSRSW)
        AND     A
        RET     NZ
        JR      J4926

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4901:  LD      A,(CSRSW)
        AND     A
        RET     Z
        JR      J4926

;         Subroutine display cursor handler
;            Inputs  ________________________
;            Outputs ________________________


J4908:  EI
        CALL    C4583                   ; take control from hook caller
        CALL    C41F9                   ; get KANJI workspace
        JR      J4926


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4911:  LD      A,(CSRSW)
        AND     A
        RET     NZ
        JR      J4926

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4918:  LD      A,(CSRSW)
        AND     A
        RET     Z
        JR      J4926

;         Subroutine erase cursor handler
;            Inputs  ________________________
;            Outputs ________________________


J491F:  EI
        CALL    C4583                   ; take control from hook caller
        CALL    C41F9                   ; get KANJI workspace
J4926:  CALL    C4946
        RET     NC
I492A:  LD      A,(CSTYLE)
        AND     A
        LD      E,00H
        JR      Z,C4934
        LD      E,01H   ; 1 

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4934:  PUSH    DE
        CALL    C4954
        CP      01H     ; 1 
        JR      NZ,J493D
        XOR     A
J493D:  POP     DE
        OR      E
        LD      E,A
        LD      HL,(CSRY)
        JP      C5CED

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4946:  LD      A,(SCRMOD)
        CP      02H     ; 2 
        RET     C
        LD      A,(NORUSE)
        BIT     7,A
        RET     Z
        SCF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4954:  LD      HL,(CSRY)

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4957:  PUSH    HL
        LD      H,01H   ; 1 
        CALL    C4CF8
        POP     AF
        LD      L,A
J495F:  LD      A,L
        CP      H
        JR      Z,J4979
        LD      A,01H   ; 1 
        RET     C
        LD      A,C
        CALL    C4982
        LD      A,(VDP_DR)
        LD      C,A
        IN      C,(C)
        INC     H
        JR      C,J495F
        LD      C,A
        IN      C,(C)
        INC     H
        JR      J495F
J4979:  LD      A,C
        CALL    C4982
        LD      A,02H   ; 2 
        RET     NC
        XOR     A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4982:  CP      81H
        RET     C
        CP      0A0H
        CCF
        RET     NC
        CP      0E0H
        RET     C
        CP      0FDH
        CCF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4990:  CP      40H     ; "@"
        RET     C
        CP      0FDH
        CCF
        RET     C
        CP      7FH
        SCF
        RET     Z
        AND     A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C499D:  LD      A,(LINLEN)
        CP      H
        RET     Z
        INC     H
        JR      C49D6

;         Subroutine BACKSPACE, CURSOR LEFT
;            Inputs  ________________________
;            Outputs ________________________

C49A5:  CALL    C49C2
        RET     NZ
        LD      A,(LINLEN)
        LD      H,A
        DEC     L
        RET     Z
        CALL    C4CF8
        INC     A
        JR      NZ,C49D6
        DEC     H
        JR      C49D6

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49B8:  CALL    C49C2
        RET     NZ
        LD      A,(LINLEN)
        LD      H,A
        JR      C49C4

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49C2:  DEC     H
        DEFB    03EH

C49C4:  DEC     L
        RET     Z
        JR      C49D6

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49C8:  CALL    C499D
        RET     NZ
        LD      H,01H   ; 1 

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49CE:  CALL    C4CEE
        CP      L
        RET     Z
        JR      C,J49DA
        INC     L

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49D6:  LD      (CSRY),HL
        RET
J49DA:  DEC     L
        XOR     A
        JR      C49D6

;         Subroutine TAB
;            Inputs  ________________________
;            Outputs ________________________

C49DE:  LD      A,20H   ; " "
        CALL    C47E5
        LD      A,(CSRX)
        DEC     A
        AND     07H     ; 7 
        JR      NZ,C49DE
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49EC:  LD      L,01H   ; 1 

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49EE:  LD      H,01H   ; 1 
        JR      C49D6

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49F2:  CALL    C49EE

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C49F5:  CALL    C4CEE
        SUB     L
        RET     C
        JP      Z,C4A73
        PUSH    HL
        PUSH    AF
        LD      C,A
        LD      B,00H
        CALL    C4CD9
        LD      L,E
        LD      H,D
        INC     HL
        LDIR
        LD      HL,FSTPOS
        DEC     (HL)
        POP     AF
        POP     HL
        CALL    C5DED
J4A13:  PUSH    AF
        INC     L
        CALL    C4BCE
        DEC     L
        CALL    C4C1D
        INC     L
        CALL    C4BDC
        DEC     L
        CALL    C4C2B
        INC     L
        POP     AF
        DEC     A
        JR      NZ,J4A13
        JP      C4A73

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A2C:  CALL    C49EE

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A2F:  CALL    C4CEE
        LD      H,A
        SUB     L
        RET     C
        JP      Z,C4A73
        LD      L,H
        PUSH    HL

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A3A:  PUSH    AF
        LD      C,A
        LD      B,00H
        CALL    C4CD9
        LD      L,E
        LD      H,D
        PUSH    HL
        DEC     HL
        LDDR
        POP     HL
        LD      (HL),H
        POP     AF
        POP     HL
        CALL    C5F2C
J4A4E:  PUSH    AF
        DEC     L
        CALL    C4BCE
        INC     L
        CALL    C4C1D
        DEC     L
        CALL    C4BDC
        INC     L
        CALL    C4C2B
        DEC     L
        POP     AF
        DEC     A
        JR      NZ,J4A4E
        JR      C4A73
J4A66:  LD      (IX+2),00H
        CALL    C49A5
        RET     Z
        LD      C,20H   ; " "
        JP      C4D13

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A73:  LD      H,01H   ; 1 

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A75:  CALL    C4A7E
        PUSH    HL
        CALL    C600F
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A7E:  CALL    C4CE5
        PUSH    HL
        CALL    C4C6E
        POP     DE
        PUSH    DE
        LD      A,(LINLEN)
        SUB     D
        INC     A
        LD      C,A
        LD      B,00H
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        LD      A,20H   ; " "
        PUSH    AF
        CALL    Z,FILVRM                ; yep,
        POP     AF
        CALL    NZ,C4A9F               ; nope,
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4A9F:  LD      E,A
        LD      A,L
        NEG
        JR      Z,C4AB3
        CP      C
        JR      NC,C4AB3
        PUSH    HL
        PUSH    BC
        LD      C,A
        CALL    C4AB3
        POP     BC
        POP     HL
        CALL    C4ABE
;

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4AB3:  PUSH    BC
        CALL    C4D3A
        POP     HL
        LD      B,L
J4AB9:  OUT     (C),E
        DJNZ    J4AB9
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4ABE:  LD      A,C
        ADD     A,L
        LD      C,A

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4AC1:  LD      L,00H
        INC     H
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      06H     ; 6 
        RET     NC
        RES     7,H
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4ACF:  PUSH    HL
        CALL    C4A75
        POP     HL
        CALL    C4CEE
        CP      L
        RET     C
        RET     Z
        LD      H,01H   ; 1 
        INC     L
        JR      C4ACF

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4ADF:  LD      HL,0101H
        LD      (CSRY),HL
J4AE5:  CALL    C4A7E
        CALL    C4CEE
        CP      L
        RET     C
        RET     Z
        LD      H,01H   ; 1 
        INC     L
        JR      J4AE5

;         Subroutine erase function keys handler
;            Inputs  ________________________
;            Outputs ________________________


J4AF3:  EI
        CALL    C4583                   ; take control from hook caller
        CALL    C41F9                   ; get KANJI workspace
        XOR     A
        LD      (CNSDFG),A
        PUSH    HL
        LD      HL,(CRTCNT)
        CALL    C4A73
        POP     HL
        RET

;         Subroutine screencode CLS
;            Inputs  ________________________
;            Outputs ________________________

C4B07:  CALL    C611A
        CALL    C6ABF
        CALL    C4ADF

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4B10:  LD      A,(CNSDFG)
        AND     A
        RET     Z
        JR      J4B1B

;         Subroutine display function keys handler
;            Inputs  ________________________
;            Outputs ________________________


J4B17:  EI
        CALL    C4583                   ; take control from hook caller
J4B1B:  CALL    C41F9                   ; get KANJI workspace
        CALL    C7E50
        AND     A
        BIT     0,(IY+8)
        CALL    NZ,C6B7B
        RET     C
        LD      A,0FFH
        LD      (CNSDFG),A
        PUSH    HL
        LD      A,(CSRY)
        LD      HL,CRTCNT
        CP      (HL)
        LD      A,0AH
        CALL    Z,C47B7                 ; KANJI character to screen (registers saved)
        LD      A,(NEWKEY+6)
        RRCA
        LD      DE,FNKSTR+5*16
        LD      A,0
        JR      NC,J4B4B
        LD      DE,FNKSTR
        INC     A
J4B4B:  LD      (FNKSWI),A
        LD      HL,(CRTCNT)
        LD      H,1
        JR      NC,J4B60
        CALL    C7E50
        JR      Z,J4B60                ; No MSX-JE,
        BIT     0,(IY+8)
        JR      NZ,J4BB7
J4B60:  LD      A,(LINLEN)
        SUB     4
        JR      C,J4BB7
        LD      B,-1
J4B69:  SUB     05H     ; 5 
        INC     B
        JR      NC,J4B69
        JR      Z,J4BB7
        LD      C,05H   ; 5 
J4B72:  PUSH    BC
        LD      C,0FH   ; 15 
J4B75:  LD      A,(DE)
        AND     A
        JR      Z,J4B98
        CP      21H     ; "!"
        JR      C,J4B9F
        CALL    C4982
        JR      C,J4BA1
        LD      A,B
        DEC     A
        JR      Z,J4B9F
        INC     DE
        LD      A,(DE)
        DEC     DE
        CALL    C4990
        JR      C,J4BBC
        LD      A,(DE)
        CALL    C4BC6
        INC     DE
        DEC     C
        DEC     B
        LD      A,(DE)
        JR      J4BA1
J4B98:  CALL    C4BC4
        DJNZ    J4B98
        JR      J4BA8
J4B9F:  LD      A,20H   ; " "
J4BA1:  CALL    C4BC6
        INC     DE
        DEC     C
        DJNZ    J4B75
J4BA8:  EX      (SP),HL
        LD      A,L
        EX      (SP),HL
        DEC     A
        CALL    NZ,C4BC4
        INC     DE
        EX      DE,HL
        ADD     HL,BC
        EX      DE,HL
J4BB3:  POP     BC
        DEC     C
        JR      NZ,J4B72
J4BB7:  CALL    C600F
        POP     HL
        RET
J4BBC:  CALL    C4BC4
        INC     DE
        DEC     C
        DEC     B
        JR      J4B9F

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4BC4:  LD      A,20H   ; " "

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4BC6:  PUSH    BC
        LD      C,A
        CALL    C58DC
        POP     BC
        INC     H
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4BCE:  PUSH    HL
        LD      H,01H   ; 1 
        LD      A,(LINLEN)
        CP      40+1
        JR      C,J4BE7
        LD      A,28H   ; "("
        JR      J4BE7

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4BDC:  LD      A,(LINLEN)
        CP      40+1
        RET     C
        PUSH    HL
        LD      H,29H   ; ")"
        SUB     28H     ; "("
J4BE7:  CALL    C4BEC
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4BEC:  LD      C,A
        CALL    C4C6E
        LD      B,00H
        LD      DE,LINWRK
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        JP      Z,LDIRMV                ; yep,
        LD      A,L
        NEG
        JR      Z,C4C0F
        CP      C
        JR      NC,C4C0F
        PUSH    HL
        PUSH    BC
        LD      C,A
        CALL    C4C0F
        POP     BC
        POP     HL
        CALL    C4ABE

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C0F:  PUSH    BC
        CALL    C4D31
        POP     HL
        LD      B,L
        EX      DE,HL
J4C16:  INI
        JP      NZ,J4C16
        EX      DE,HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C1D:  PUSH    HL
        LD      H,01H   ; 1 
        LD      A,(LINLEN)
        CP      40+1
        JR      C,J4C36
        LD      A,28H   ; "("
J4C29:  JR      J4C36

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C2B:  LD      A,(LINLEN)
        CP      29H     ; ")"
        RET     C
        PUSH    HL
        LD      H,29H   ; ")"
        SUB     28H     ; "("
J4C36:  CALL    C4C3B
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C3B:  LD      C,A
        CALL    C4C6E
        EX      DE,HL
        LD      B,00H
        LD      HL,LINWRK
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        JP      Z,LDIRVM                ; yep,
        EX      DE,HL
        LD      A,L
        NEG
        JR      Z,C4C60
        CP      C
        JR      NC,C4C60
        PUSH    HL
        PUSH    BC
        LD      C,A
        CALL    C4C60
        POP     BC
        POP     HL
        CALL    C4ABE

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C60:  PUSH    BC
        CALL    C4D3A
        POP     HL
        LD      B,L
        EX      DE,HL
J4C67:  OUTI

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C69:  JP      NZ,J4C67
        EX      DE,HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4C6E:  PUSH    BC
        LD      E,H
        LD      D,00H
        DEC     E
        LD      H,D
        DEC     L
        LD      A,L
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      L,A
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      02H     ; 2 
        LD      BC,0B00H
        JR      Z,J4CC3
        LD      C,L
        LD      B,H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,BC
        LD      BC,1E20H
        DEC     A
        JR      Z,J4CC4
        SUB     02H     ; 2 
        JR      Z,J4CC8
        DEC     A
        LD      BC,06A00H
        JR      Z,J4CA9
        CP      03H     ; 3 
        LD      BC,0D400H
        JR      Z,J4CA9
        ADD     HL,HL
        DEC     A
        JR      NZ,J4CA9
        LD      BC,J6A00
J4CA9:  ADD     HL,DE
        ADD     HL,BC
        LD      A,(RG23SA)
        LD      C,00H
        LD      B,A
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      06H     ; 6 
        JR      NC,J4CC5
        SRL     B
        RR      C
        ADD     HL,BC
        RES     7,H
        POP     BC
        RET

J4CC3:  ADD     HL,HL
J4CC4:  ADD     HL,DE
J4CC5:  ADD     HL,BC
        POP     BC
        RET
J4CC8:  LD      A,H
        AND     A
        LD      BC,1E08H
        JR      NZ,J4CC4
        LD      A,L
        CP      78H     ; "x"
        JR      NC,J4CC4
        LD      BC,1B00H
        JR      J4CC4

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4CD9:  PUSH    HL
        LD      DE,LINTTB-1
        LD      H,0
        ADD     HL,DE
        LD      A,(HL)
        EX      DE,HL
        POP     HL
        AND     A
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4CE5:  DEFB    03EH

C4CE6:  XOR     A


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4CE7:  PUSH    AF
        CALL    C4CD9
        POP     AF
        LD      (DE),A
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4CEE:  LD      A,(CNSDFG)
        PUSH    HL
        LD      HL,CRTCNT
        ADD     A,(HL)
        POP     HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4CF8:  PUSH    DE
        PUSH    HL
        CALL    C4C6E
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        JR      Z,J4D0C                ; yep,
        CALL    C4D31
        IN      A,(C)
        LD      C,A
        POP     HL
        POP     DE
        RET
J4D0C:  CALL    RDVRM
        LD      C,A
        POP     HL
        POP     DE
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4D13:  PUSH    DE
        PUSH    HL
        CALL    C4C6E
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        JR      Z,J4D27                 ; yep,
        LD      B,C
        CALL    C4D3A
        OUT     (C),B
        LD      C,B
        JR      J4D2B
J4D27:  LD      A,C
        CALL    WRTVRM
J4D2B:  POP     HL
        CALL    C58DC
        POP     DE
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4D31:  LD      A,(VDP_DR)
        LD      C,A
        LD      A,H
        RES     6,H
        JR      J4D41

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4D3A:  LD      A,(VDP_DR)
        LD      C,A
        LD      A,H
        SET     6,H
J4D41:  RES     7,H
        RLCA
        RLCA
        AND     03H     ; 3 
        INC     C
        DI
        OUT     (C),A
        LD      A,8EH
        OUT     (C),A
        OUT     (C),L
        EI
        OUT     (C),H
        DEC     C
        RET

;         Subroutine PINLIN handler
;            Inputs  ________________________
;            Outputs ________________________


J4D56:  EI
        CALL    C4583                   ; take control from hook caller
        CALL    C41F9                   ; get KANJI workspace
        LD      A,0FFH
        LD      (DECCNT),A
        LD      A,(AUTFLG)
        AND     A
        JR      NZ,J4D77
        LD      L,00H
        JR      J4D7F

;         Subroutine INLIN handler
;            Inputs  ________________________
;            Outputs ________________________

J4D6C:  EI
        CALL    C4583                   ; take control from hook caller
        CALL    C41F9                   ; get KANJI workspace
        XOR     A
        LD      (DECCNT),A
J4D77:  LD      HL,(CSRY)
        DEC     L
        CALL    NZ,C4CE5
        INC     L
J4D7F:  LD      (FSTPOS),HL
        XOR     A
        LD      (INTFLG),A
J4D86:  CALL    C4D9E
        LD      HL,I4E84-2
        LD      C,14
        CALL    C4839
        PUSH    AF
        CALL    NZ,C4DB1
        POP     AF
        JR      NC,J4D86
        LD      HL,BUFMIN
        RET     Z
        CCF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4D9E:  LD      A,(SCRMOD)
        PUSH    AF
        XOR     A
        LD      (SCRMOD),A
        CALL    C45BB
        POP     BC
        PUSH    AF
        LD      A,B
        LD      (SCRMOD),A
        POP     AF
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4DB1:  PUSH    AF
        CP      09H     ; 9 
        JR      NZ,J4DC5
J4DB6:  POP     AF
J4DB7:  LD      A,20H   ; " "
        CALL    C4DB1
        LD      A,(CSRX)
        DEC     A
        AND     07H     ; 7 
        JR      NZ,J4DB7
        RET
J4DC5:  POP     AF
        LD      HL,INSFLG
        CP      20H     ; " "
        JP      C,J4E71
        INC     (HL)
        DEC     (HL)
        JR      Z,J4E05
        CALL    C4982
        JR      C,J4DF4
        PUSH    AF
        CALL    C4D9E
        PUSH    AF
        CALL    C4954
        DEC     A
        JR      NZ,J4DE7
        LD      A,1DH
        CALL    C47B7                   ; KANJI character to screen (registers saved)
J4DE7:  CALL    C4F6B
        POP     HL
        EX      (SP),HL
        LD      A,H
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        POP     AF
        JP      C47B7                   ; KANJI character to screen (registers saved)

J4DF4:  PUSH    AF
        CALL    C4954
        DEC     A
        LD      A,1DH
        CALL    Z,C47B7                 ; KANJI character to screen (registers saved)
        CALL    C4F84
        POP     AF
        JP      C47B7                   ; KANJI character to screen (registers saved)

J4E05:  CALL    C4982
        JR      C,J4E4F
        PUSH    AF
        CALL    C4D9E
        POP     DE
        LD      E,A
        LD      HL,(CSRY)
        LD      A,(LINLEN)
        INC     H
        INC     A
        CP      H
        JR      NZ,J4E24
        CALL    C4CEE
        CP      L
        JR      Z,C4E39
        INC     L
        LD      H,02H   ; 2 
J4E24:  CALL    C4957
J4E27:  CP      02H     ; 2 
        PUSH    AF
        CALL    C4E39
        POP     AF
        RET     NZ
        LD      A,20H
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        LD      A,1DH
        JP      C47B7                   ; KANJI character to screen (registers saved)


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4E39:  CALL    C4954
        DEC     A
        LD      A,1DH
        CALL    Z,C47B7                 ; KANJI character to screen (registers saved)
        LD      A,20H
        CALL    Z,C47B7                 ; KANJI character to screen (registers saved)
        LD      A,D
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        LD      A,E
        JP      C47B7                   ; KANJI character to screen (registers saved)

J4E4F:  LD      D,A
        CALL    C4954
        PUSH    AF
        DEC     A
        LD      A,1DH
        CALL    Z,C47B7                 ; KANJI character to screen (registers saved)
        LD      A,20H
        CALL    Z,C47B7                 ; KANJI character to screen (registers saved)
        LD      A,D
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        POP     AF
        CP      02H
        RET     NZ
        LD      A,20H
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        LD      A,1DH
        JP      C47B7                   ; KANJI character to screen (registers saved)
J4E71:  LD      (HL),00H
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        DEFB    03EH
J4E77:  DEFB    03EH

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4E78:  XOR     A
        PUSH    AF
        CALL    C4918
        POP     AF
        LD      (CSTYLE),A
        JP      C4901

I4E84:  DEFB    8
        DEFW    C503F
        DEFB    18
        DEFW    C4F5E
        DEFB    27
        DEFW    WRPRIM+6
        DEFB    2
        DEFW    C51A6
        DEFB    6
        DEFW    C5190
        DEFB    14
        DEFW    C516F
        DEFB    5
        DEFW    C5146
        DEFB    3
        DEFW    C4F2A
        DEFB    13
        DEFW    C4EAE
        DEFB    21
        DEFW    C513B
        DEFB    127
        DEFW    C506C
        DEFB    29
        DEFW    C522C
        DEFB    30
        DEFW    C52C0
        DEFB    31
        DEFW    C5235

C4EAE:  CALL    C5218
        LD      A,(AUTFLG)
        AND     A
        JR      Z,J4EB9
        LD      H,01H   ; 1 
J4EB9:  PUSH    HL
        CALL    C4918
        POP     HL
        LD      DE,BUF
        LD      B,0FEH
        DEC     L
J4EC4:  INC     L
J4EC5:  PUSH    BC
        CALL    C4CF8
        POP     BC
        AND     A
        JR      Z,J4EDC
        CP      0FFH
        JR      Z,J4EDC
        CP      20H     ; " "
        JR      NC,J4ED7
        ADD     A,40H   ; "@"
J4ED7:  LD      (DE),A
        INC     DE
        DEC     B
        JR      Z,J4EEC
J4EDC:  INC     H
        LD      A,(LINLEN)
        CP      H
        JR      NC,J4EC5
        PUSH    DE
        CALL    C4CD9
        POP     DE
        LD      H,01H   ; 1 
        JR      Z,J4EC4
J4EEC:  DEC     DE
        LD      A,(DE)
        CP      20H     ; " "
        JR      Z,J4EEC
        PUSH    HL
        PUSH    DE
        CALL    C4901
        POP     DE
        POP     HL
        INC     DE
        XOR     A
        LD      (DE),A
J4EFC:  LD      A,0DH   ; 13 
        AND     A
J4EFF:  PUSH    AF
        CALL    C4CE5
        CALL    C4F13
        LD      A,0AH
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        XOR     A
        LD      (INSFLG),A
        POP     AF
        SCF
        POP     HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4F13:  LD      A,1BH
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        LD      A,'Y'
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        LD      A,L
        ADD     A,1FH
        CALL    C47B7                   ; KANJI character to screen (registers saved)
        LD      A,H
        ADD     A,1FH
        JP      C47B7                   ; KANJI character to screen (registers saved)

J4F29:  INC     L
C4F2A:  CALL    C4CD9
        JR      Z,J4F29
        CALL    C4E78
        XOR     A
        LD      (BUF+0),A
        LD      H,01H   ; 1 
        PUSH    HL
        CALL    GICINI
        CALL    C4F4A
        POP     HL
        JR      C,J4EFC
        LD      A,(BASROM)
        AND     A
        JR      NZ,J4EFC
        JR      J4EFF


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C4F4A:  LD      A,(TRPTBL+30+0)
        RRCA
        RET     NC
        LD      HL,(TRPTBL+30+1)
        LD      A,H
        OR      L
        RET     Z
        LD      HL,(CURLIN)
        INC     HL
        LD      A,H
        OR      L
        RET     Z
        SCF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4F5E:  LD      HL,INSFLG
        LD      A,(HL)
        XOR     0FFH
        LD      (HL),A
        JP      Z,C4E78
        JP      J4E77

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4F6B:  CALL    C4918
        CALL    C500B
        LD      C,20H   ; " "
        CALL    C5015
        LD      A,(CSRX)
        LD      HL,LINLEN
        CP      (HL)
        LD      C,20H   ; " "
        CALL    Z,C5015
        JR      J4F8A

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C4F84:  CALL    C4918
        CALL    C500B
J4F8A:  LD      C,20H   ; " "
        CALL    C5015
        LD      HL,(CSRY)
J4F92:  PUSH    HL
        AND     A
        PUSH    AF
J4F95:  CALL    C4CF8
        CALL    C5015
        CALL    C5038
        POP     AF
        LD      A,C
        CALL    NC,C4982
        JR      C,J4FAE
        LD      A,(LINLEN)
        CP      H
        LD      C,0FFH
        JR      Z,J4FB1
        AND     A
J4FAE:  CALL    C5027
J4FB1:  CCF
        PUSH    AF
        CALL    C4D13
        LD      A,(LINLEN)
        INC     H
        CP      H
        JR      NC,J4F95
        POP     AF
        LD      HL,(BUF+2)
        LD      A,(BUF+0)
        CP      L
        POP     HL
        JP      Z,C4901
        CALL    C4CD9
        JR      Z,J5006
        CALL    C5038
        LD      A,C
        CP      20H     ; " "
        PUSH    AF
        JR      NZ,J4FE1
        LD      A,(LINLEN)
        CP      H
        JR      Z,J4FE1
        POP     AF
        JP      C4901
J4FE1:  CALL    C4CE6
        INC     L
        PUSH    HL
        CALL    C4CEE
        CP      L
        JR      C,J4FF1
        CALL    C4A2F
        JR      J5000
J4FF1:  LD      HL,CSRY
        DEC     (HL)
        JR      NZ,J4FF8
        INC     (HL)
J4FF8:  LD      L,01H   ; 1 
        CALL    C49F5
        POP     HL
        DEC     L
        PUSH    HL
J5000:  POP     HL
        POP     AF
        JP      Z,C4901
        DEC     L
J5006:  INC     L
        LD      H,01H   ; 1 
        JR      J4F92

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C500B:  LD      HL,BUF+162
        LD      (BUF+0),HL
        LD      (BUF+2),HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5015:  LD      A,C
        INC     A
        RET     Z
        PUSH    HL
        LD      HL,(BUF+0)
        LD      (HL),C
        INC     HL
        LD      A,L
        AND     1FH
        LD      L,A
        LD      (BUF+0),HL
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5027:  PUSH    HL
        PUSH    AF
        LD      HL,(BUF+2)
        LD      C,(HL)
        INC     HL
        LD      A,L
        AND     1FH
        LD      L,A
        LD      (BUF+2),HL
        POP     AF
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5038:  PUSH    HL
        LD      HL,(BUF+2)
        LD      C,(HL)
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C503F:  CALL    C4918
        CALL    C4954
        LD      HL,(CSRY)
        DEC     A
        JR      NZ,J504C
        DEC     H
J504C:  DEC     L
        DEC     H
        JR      NZ,J5066
        INC     H
        LD      A,L
        AND     A
        JR      Z,J5066
        CALL    C4CD9
        JR      NZ,J5066
        LD      A,(LINLEN)
        LD      H,A
        CALL    C4CF8
        INC     A
        JR      NZ,J5065
        DEC     H
J5065:  DEC     L
J5066:  INC     L
        LD      (CSRY),HL
        JR      J506F

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C506C:  CALL    C4918
J506F:  LD      HL,(CSRY)
        CALL    C4CF8
        INC     A
        JR      NZ,J507E
        LD      H,01H   ; 1 
        INC     L
        LD      (CSRY),HL
J507E:  CALL    C4954
        LD      HL,(CSRY)
        CP      01H     ; 1 
        JR      NZ,J508C
        DEC     H
        LD      (CSRY),HL
J508C:  AND     A
        LD      D,01H   ; 1 
        JR      Z,J50C2
        INC     D
        LD      A,H
        DEC     A
        JR      NZ,J50C2
        LD      A,L
        DEC     A
        JR      Z,J50C2
        PUSH    HL
        DEC     L
        LD      A,(LINLEN)
        LD      H,A
        CALL    C4CF8
        INC     A
        POP     HL
        JR      NZ,J50C2
        LD      H,03H   ; 3 
        PUSH    HL
        CALL    C4957
        POP     HL
        LD      H,01H   ; 1 
        AND     A
        JR      NZ,J50C2
        DEC     L
        LD      A,(LINLEN)
        LD      H,A
        LD      (CSRY),HL
        DEC     D
        PUSH    HL
        INC     L
        LD      H,03H   ; 3 
        JR      J50EF
J50C2:  LD      E,00H
J50C4:  ADD     HL,DE
        LD      A,(LINLEN)
        CP      H
        JR      C,J50D7
        CALL    C4CF8
        LD      A,H
        SUB     D
        LD      H,A
        CALL    C4D13
        INC     H
        JR      J50C4
J50D7:  PUSH    DE
        CALL    C4CD9
        POP     DE
        JR      NZ,J5132
        LD      A,C
        ADD     A,01H   ; 1 
        LD      A,D
        ADC     A,00H
        LD      D,A
        LD      A,(LINLEN)
        SUB     D
        INC     A
        LD      H,A
        PUSH    HL
        INC     L
        LD      H,01H   ; 1 
J50EF:  CALL    C4CF8
        CALL    C4982
        LD      A,02H   ; 2 
        SBC     A,00H
        CP      D
        JR      Z,J50FE
        JR      NC,J5122
J50FE:  LD      E,A
        NEG
        ADD     A,D
        LD      D,A
        DEC     E
        JR      Z,J5110
        INC     H
        EX      (SP),HL
        CALL    C4D13
        INC     H
        EX      (SP),HL
        CALL    C4CF8
J5110:  INC     H
        EX      (SP),HL
        CALL    C4D13
        INC     H
        EX      (SP),HL
        LD      A,D
        AND     A
        JR      NZ,J50EF
        POP     AF
        LD      D,H
        DEC     D
        LD      H,01H   ; 1 
        JR      J50C2
J5122:  EX      (SP),HL
        LD      C,0FFH
        CALL    C4D13
        POP     HL
        LD      D,H
        DEC     D
        LD      H,01H   ; 1 
        JR      NZ,J50C2
        JP      C4901
J5132:  LD      A,H
        SUB     D
        LD      H,A
        CALL    C4A75
        JP      C4901

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C513B:  CALL    C4918
        CALL    C5218
        LD      (CSRY),HL
        JR      J5156

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5146:  CALL    C4918
        CALL    C4954
        DEC     A
        LD      HL,(CSRY)
        JR      NZ,J5156
        DEC     H
        LD      (CSRY),HL
J5156:  CALL    C4CD9
        PUSH    AF
        CALL    C4A75
        POP     AF
        JR      NZ,J5165
        LD      H,01H   ; 1 
        INC     L
        JR      J5156
J5165:  CALL    C4901
        XOR     A
        LD      (INSFLG),A
        JP      C4E78

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C516F:  CALL    C4918
        LD      HL,(CSRY)
        DEC     L
J5176:  INC     L
        CALL    C4CD9
        JR      Z,J5176
        LD      A,(LINLEN)
        LD      H,A
        INC     H
J5181:  DEC     H
        JR      Z,J518B
        CALL    C4CF8
        CP      20H     ; " "
        JR      Z,J5181
J518B:  CALL    C49C8
        JR      J5165

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5190:  CALL    C4918
        CALL    C51D6
J5196:  CALL    C51BC
        JR      Z,J5165
        JR      C,J5196
J519D:  CALL    C51BC
        JR      Z,J5165
        JR      NC,J519D
        JR      J5165

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C51A6:  CALL    C4918
J51A9:  CALL    C51D6
        JR      Z,J5165
        JR      NC,J51A9
J51B0:  CALL    C51D6
        JR      Z,J5165
        JR      C,J51B0
        CALL    C49C8
        JR      J5165

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C51BC:  LD      HL,(CSRY)
        CALL    C49C8
        CALL    C4954
        DEC     A
        LD      HL,(CSRY)
        CALL    Z,C49C8
        CALL    C4CEE
        LD      E,A
        LD      A,(LINLEN)
        LD      D,A
        JR      J51E9

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C51D6:  LD      HL,(CSRY)
        CALL    C49A5
        CALL    C4954
        DEC     A
        LD      HL,(CSRY)
        CALL    Z,C49A5
        LD      DE,0101H
J51E9:  LD      HL,(CSRY)
        RST     20H
J51ED:  RET     Z
        LD      DE,C5214
        PUSH    DE
        CALL    C4CF8
        CP      30H     ; "0"
        CCF
        RET     NC
        CP      3AH     ; ":"
        RET     C
        CP      41H     ; "A"
        CCF
        RET     NC
        CP      5BH     ; "["
        RET     C
        CP      61H     ; "a"
        CCF
        RET     NC
        CP      7BH     ; "{"
        RET     C
        CP      81H
        CCF
        RET     NC
        CP      0A0H
        RET     C
        CP      0A6H
        CCF
C5214:  LD      A,0
        INC     A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5218:  DEC     L
        JR      Z,J5220
        CALL    C4CD9
        JR      Z,C5218
J5220:  INC     L
        LD      A,(FSTPOS)
        CP      L
        LD      H,01H   ; 1 
        RET     NZ
        LD      HL,(FSTPOS)
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C522C:  CALL    C4918
        CALL    C49B8
        JP      J5165

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5235:  CALL    C4918
        CALL    C523E
        JP      J5165

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C523E:  CALL    C53E6
        JP      NC,C49CE
        CALL    C4CEE
        LD      HL,(CSRY)
        CP      L
        JP      NZ,C49CE
J524E:  LD      A,L
        DEC     A
        JR      Z,J5259
        DEC     L
        CALL    C4CD9
        JR      Z,J524E
        INC     L
J5259:  CALL    C536F
        JR      C,J5264
        DEC     L
        JR      NZ,J524E
        LD      DE,-1
J5264:  INC     DE
        LD      BC,(TXTTAB)
J5269:  LD      L,C
        LD      H,B
        LD      C,(HL)
        INC     HL
        LD      B,(HL)
        LD      A,C
        OR      B
        RET     Z
        INC     HL
        LD      A,(HL)
        SUB     E
        INC     HL
        LD      A,(HL)
        SBC     A,D
        JR      C,J5269
        DEC     HL
        CALL    C533C
        LD      HL,(CSRY)
        PUSH    HL
        CALL    C4CEE
        CP      B
        JR      C,J5298
        JR      Z,J5298
        LD      L,B
J528A:  CALL    C4CD9
        JR      NZ,J5298
        PUSH    HL
        LD      L,01H   ; 1 
        CALL    C49F2
        POP     HL
        JR      J528A
J5298:  LD      L,01H   ; 1 
        CALL    C49F2
        POP     HL
        PUSH    HL
        LD      H,01H   ; 1 
        LD      (CSRY),HL
        CALL    C52AC
        POP     HL
        LD      (CSRY),HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C52AC:  LD      HL,SWPTMP
        CALL    C52B5
        LD      HL,BUF


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C52B5:  LD      A,(HL)
        AND     A
        RET     Z
        PUSH    HL
        CALL    C47E5
        POP     HL
        INC     HL
        JR      C52B5

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C52C0:  CALL    C4918
        CALL    C52C9
        JP      J5165

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C52C9:  CALL    C53E6
        JP      NC,C49C4
        LD      A,(CSRY)
        DEC     A
        JP      NZ,C49C4
        LD      L,00H
J52D8:  INC     L
        CALL    C536F
        JR      C,J52EF
J52DE:  CALL    C4CEE
        CP      L
        JR      Z,J52EC
        CALL    C4CD9
        JR      NZ,J52D8
        INC     L
        JR      J52DE
J52EC:  LD      DE,-1
J52EF:  LD      BC,0
        LD      HL,(TXTTAB)
J52F5:  LD      A,(HL)
        INC     HL
        OR      (HL)
        JR      Z,J530C
        INC     HL
        LD      A,(HL)
        SUB     E
        INC     HL
        LD      A,(HL)
        SBC     A,D
        JR      NC,J530C
        DEC     HL
        LD      C,L
        LD      B,H
        DEC     HL
        LD      A,(HL)
        DEC     HL
        LD      L,(HL)
        LD      H,A
        JR      J52F5
J530C:  LD      A,C
        OR      B
        RET     Z
        LD      L,C
        LD      H,B
        CALL    C533C
        CALL    C4CEE
        SUB     B
        JR      Z,J5326
        JR      C,J5326
        LD      L,A
J531D:  CALL    C4CD9
        JR      NZ,J5326
        INC     B
        DEC     L
        JR      NZ,J531D
J5326:  LD      HL,(CSRY)
        PUSH    HL
        CALL    C49EC
J532D:  PUSH    BC
        CALL    C4A2C
        POP     BC
        DJNZ    J532D
        CALL    C52AC
        POP     HL
        LD      (CSRY),HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C533C:  LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        PUSH    HL
        EX      DE,HL
        CALL    C53AA
        POP     HL
        PUSH    IX
        LD      IX,BUFLIN
        CALL    C41FE                  ; decode BASIC line
        POP     IX
        LD      HL,SWPTMP-1
        LD      C,0FEH
        XOR     A
J5357:  INC     HL
        INC     C
        CP      (HL)
        JR      NZ,J5357
        LD      HL,BUF-1
J535F:  INC     HL
        INC     C
        CP      (HL)
        JR      NZ,J535F
        LD      B,A
        LD      A,(LINLEN)
        LD      E,A
        LD      A,C
J536A:  INC     B
        SUB     E
        JR      NC,J536A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C536F:  LD      H,01H   ; 1 
        LD      A,(LINLEN)
        LD      B,A
J5375:  CALL    C4CF8
        CP      20H     ; " "
        JR      NZ,J5380
        INC     H
        DJNZ    J5375
        RET
J5380:  SUB     30H     ; "0"
        CCF
        RET     NC
        CP      0AH     ; 10 
        RET     NC
        LD      DE,0
J538A:  CALL    C4CF8
        CP      20H     ; " "
        JR      Z,J53A5
        SUB     30H     ; "0"
        RET     C
        CP      0AH     ; 10 
        CCF
        RET     C
        PUSH    HL
        LD      L,E
        LD      H,D
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,DE
        ADD     HL,HL
        LD      E,A
        LD      D,00H
        ADD     HL,DE
        EX      DE,HL
        POP     HL
J53A5:  INC     H
        DJNZ    J538A
        SCF
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C53AA:  LD      DE,SWPTMP
        XOR     A
        LD      BC,-10000
        CALL    C53D2
        LD      BC,-1000
        CALL    C53D2
        LD      BC,-100
        CALL    C53D2
        LD      BC,-10
        CALL    C53D2
        LD      A,L
        OR      '0'
        LD      (DE),A
        INC     DE
        LD      A,' '
        LD      (DE),A
        INC     DE
        XOR     A
        LD      (DE),A
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C53D2:  PUSH    AF
        LD      A,-1
J53D5:  INC     A
        ADD     HL,BC
        JR      C,J53D5
        SBC     HL,BC
        POP     BC
        INC     B
        DJNZ    J53E1
        AND     A
        RET     Z
J53E1:  OR      '0'
        LD      (DE),A
        INC     DE
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C53E6:  LD      A,(NORUSE)
        AND     40H
        RET     Z
        LD      A,(DECCNT)
        AND     A
        RET     Z
        LD      A,(NEWKEY+6)
        RRCA
        CCF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________
;            Remark  Not used, orginal KANJI H_TIMI handler

Q53F7:  EI
        PUSH    AF
        LD      A,(H_TOTE+0)
        CP      0C9H
        JR      Z,J5407
        CALL    C541B                   ; keyboard scan
        LD      HL,SCNCNT
        INC     (HL)
J5407:  LD      A,(SLTWRK+4)
        CP      0C9H
        JR      Z,J5419
        PUSH    AF
        POP     IY
        LD      IX,(SLTWRK+5)
        POP     AF
        JP      CALSLT

J5419:  POP     AF
        RET

;         Subroutine keyboard scan
;            Inputs  ________________________
;            Outputs ________________________

C541B:  LD      HL,SCNCNT
        DEC     (HL)                    ; update keyboard scan count
        RET     NZ                      ; not time for keyboard scan, quit
        LD      (HL),2                  ; keyboard scan count = 2
        LD      HL,TRPTBL+12*3
        LD      A,(HL)                  ; TRIG0
        LD      HL,TRPTBL+13*3
        OR      (HL)                    ; or TRIG1
        LD      HL,TRPTBL+14*3
        OR      (HL)                    ; or TRIG2
        LD      HL,TRPTBL+15*3
        OR      (HL)                    ; or TRIG3
        LD      HL,TRPTBL+16*3
        OR      (HL)                    ; or TRIG4
        RRCA
        JR      NC,J5482
        XOR     A                       ; port = 0
        CALL    C5840                   ; read joystick port
        AND     30H                     ; only fire buttons
        PUSH    AF                      ; store fire button status port 0
        LD      A,1                     ; port = 1
        CALL    C5840                   ; read joystick port
        AND     30H                     ; only fire buttons
        RLCA
        RLCA                            ; to b7/b6
        POP     BC                      ; restore fire button status port 0
        OR      B                       ; combine with fire button status port 1
        PUSH    AF                      ; store fire button status
        CALL    C57F2                   ; read keyboard row 8
        AND     01H                     ; spacebar status
        POP     BC                      ; restore fire button status
        OR      B                       ; combine with spacebar status
        LD      C,A
        LD      HL,TRGFLG
        XOR     (HL)
        AND     (HL)
        LD      (HL),C                  ; update trigger status
        LD      C,A
        RRCA
        LD      HL,TRPTBL+12*3
        CALL    C,C5674                 ; raise trap
        RL      C
        LD      HL,TRPTBL+16*3
        CALL    C,C5674                 ; raise trap
        RL      C
        LD      HL,TRPTBL+14*3
        CALL    C,C5674                 ; raise trap
        RL      C
        LD      HL,TRPTBL+15*3
        CALL    C,C5674                 ; raise trap
        RL      C
        LD      HL,TRPTBL+13*3
        CALL    C,C5674                 ; raise trap
J5482:  XOR     A
        LD      (CLIKFL),A
        CALL    C54A1
        RET     NZ
        LD      HL,REPCNT
        DEC     (HL)
        RET     NZ
        LD      (HL),1
        LD      HL,OLDKEY+0
        LD      DE,OLDKEY+1
        LD      BC,11-1
        LD      (HL),0FFH
        LDIR
        JP      J54DD

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C54A1:  IN      A,(0AAH)
        AND     0F0H
        LD      C,A
        LD      B,11
        LD      HL,NEWKEY
J54AB:  LD      A,C
        OUT     (0AAH),A
        IN      A,(0A9H)
        LD      (HL),A
        INC     C
        INC     HL
        DJNZ    J54AB
        LD      A,(ENSTOP)
        AND     A
        JR      Z,J54C9
        LD      A,(NEWKEY+6)
        CP      0E8H
        JR      NZ,J54C9
        LD      IX,READYR
        JP      CALBAS

J54C9:  LD      DE,NEWKEY
        LD      B,11
J54CE:  DEC     DE
        DEC     HL
        LD      A,(DE)
        CP      (HL)
        JR      NZ,J54D8
        DJNZ    J54CE
        JR      J54DD
J54D8:  LD      A,20
        LD      (REPCNT),A
J54DD:  LD      B,11
        LD      HL,OLDKEY
        LD      DE,NEWKEY
J54E5:  LD      A,(DE)
        LD      C,A
        XOR     (HL)
        AND     (HL)
        LD      (HL),C
        CALL    NZ,C54F9
        INC     DE
        INC     HL
        DJNZ    J54E5
        LD      HL,(GETPNT)
        LD      A,(PUTPNT)
        SUB     L
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C54F9:  PUSH    HL
        PUSH    DE
        PUSH    BC
        PUSH    AF
        LD      A,11
        SUB     B
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      C,A
        LD      B,8
        POP     AF
J5507:  RRA
        PUSH    BC
        PUSH    AF
        CALL    C,C559F
        POP     AF
        POP     BC
        INC     C
        DJNZ    J5507
        POP     BC
        POP     DE
        POP     HL
        RET

I5516:  DEFB    00AH
        DEFW    C55C9
        DEFB    016H
        DEFW    C5603
        DEFB    030H
        DEFW    C55E0
        DEFB    033H
        DEFW    C5693
        DEFB    034H
        DEFW    C56B1
        DEFB    035H
        DEFW    C57FF
        DEFB    03AH
        DEFW    C561D
        DEFB    03CH
        DEFW    C5693
        DEFB    03DH
        DEFW    C56C1
        DEFB    041H
        DEFW    C5693
        DEFB    042H
        DEFW    C5689
        DEFB    048H
        DEFW    C5693
        DEFB    0FFH
        DEFW    C56A2

I553D:  DEFB    0FFH
        DEFB    "!",022H,"#$%&'()"
 
I5547:  DEFW    C56D0                   ; put in keyboardbuffer
        DEFW    C56D0                   ; put in keyboardbuffer
        DEFW    C55F5
        DEFW    C55F7

I554F:  DEFW    I557B-00AH
        DEFW    I556F-00AH
        DEFW    I5563-00AH
        DEFW    I5557-00AH

;	Table		keycodes for keys -,^,yen,@,[,;,:,],komma,.,/,_

I5557:  DEFB    "-^\\@[;:],./",0FFH	; (rem: c-escape for \)

;	Table		keycodes for keys -,^,yen,@,[,;,:,],komma,.,/,_ +SHIFT

I5563:  DEFB    "=~|`{+*}<>?_"

;	Table		keycodes for keys -,^,yen,@,[,;,:,],komma,.,/,_ +CTRL

I556F:  DEFB    "-",01EH,01CH,0,01BH,";:",01DH,",./",0FFH

;	Table		keycodes for keys -,^,yen,@,[,;,:,],komma,.,/,_ +CTRL+SHIFT

I557B:  DEFB    "=",01EH,01CH,0,01BH,"+*",01DH,"<>?",01FH

I5587:  DEFB	0,0,0,0,0,0,0,0
        DEFB	0,0,01BH,009H,0,008H,018H,00DH
        DEFB	" ",00CH,012H,07FH,01DH,01EH,01FH,01CH

;	Subroutine	KEYCOD
;	Inputs		C = scancode
;	Outputs		________________________

C559F:  LD      A,C
        CP      0FFH
        RET     Z
        LD      HL,I5516
        CALL    H_KEYC
        CP      30H
        JR      NC,J55BE
        LD      A,(NEWKEY+6)
        RRCA
        RRCA
        JR      NC,J55BD
        RRCA
        RET     NC
        LD      A,(KANAST)
        AND     A
        JP      NZ,J56FD
J55BD:  LD      A,C
J55BE:  CP      (HL)
        INC     HL
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        PUSH    DE
        RET     C
        POP     DE
        JR      J55BE

;	Subroutine	handler keys 0,1,2,3,4,5,6,7,8,9
;	Inputs		A = C = scancode (000H-009H)
;	Outputs		________________________

C55C9:  ADD     A,30H
        LD      B,A
        LD      A,(NEWKEY+6)
        RRCA
        LD      A,B
        JR      C,C55DD                 ; put in keyboardbuffer and quit
        LD      B,0
        LD      HL,I553D
        ADD     HL,BC
        LD      A,(HL)
        CP      0FFH
        RET     Z

;         Subroutine put in keyboardbuffer
;            Inputs  ________________________
;            Outputs ________________________

C55DD:  JP      C56D0                   ; put in keyboardbuffer and quit

;	Subroutine	handler keys a-z
;	Inputs		C = scancode (016H-02FH)
;	Outputs		________________________

C55E0:  LD      A,(NEWKEY+6)
        AND     03H
        ADD     A,A
        LD      E,A
        LD      D,00H
        LD      HL,I5547
        ADD     HL,DE
        LD      A,(HL)
        INC     HL
        LD      H,(HL)
        LD      L,A
        LD      A,C
        SUB     15H
        JP      (HL)

;	Subroutine	handler keys a-z, SHIFT pressed, CTRL not pressed
;	Inputs		A = lettercode (001H-01AH)
;	Outputs		________________________

C55F5:  ADD     A,20H

;	Subroutine	handler keys a-z, SHIFT and CTRL not pressed
;	Inputs		A = lettercode (001H-01AH)
;	Outputs		________________________

C55F7:  LD      B,A
        LD      A,(CAPST)
        CPL
        AND     20H     ; " "
        XOR     B
        ADD     A,40H   ; "@"
        JR      C55DD                   ; put in keyboardbuffer and quit

;	Subroutine	handler keys -,^,yen,@,[,;,:,],komma,.,/,_
;	Inputs		C = scancode (00AH-015H)
;	Outputs		________________________

C5603:  LD      HL,I554F
        LD      A,(NEWKEY+6)
        AND     03H
        ADD     A,A
        LD      E,A
        LD      D,0
        ADD     HL,DE
        LD      A,(HL)
        INC     HL
        LD      H,(HL)
        LD      L,A
        LD      E,C
        ADD     HL,DE
        LD      A,(HL)
        CP      0FFH
        JP      NZ,C56D0                ; put in keyboardbuffer and quit
        RET

;	Subroutine	handler functionkeys
;	Inputs		C = scancode (035H-039H)
;	Outputs		________________________

C561D:  LD      A,(NEWKEY+6)
        RRCA
        JR      C,J5627
        LD      A,C
        ADD     A,5
        LD      C,A
J5627:  LD      E,C
        LD      D,0
        LD      HL,FNKFLG-035H
        ADD     HL,DE
        LD      A,(HL)
        AND     A
        JR      NZ,J5666
J5632:  EX      DE,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      DE,FNKSTR-035H*16
        ADD     HL,DE
        EX      DE,HL
        DEC     DE
J563D:  INC     DE
J563E:  LD      A,(DE)
        INC     DE
        AND     A
        RET     Z
        CALL    C4982
        JR      C,J565D
        LD      B,A
        LD      A,(DE)
        AND     A
        RET     Z
        CALL    C4990
        JR      C,J563D
        LD      A,(NORUSE)
        BIT     5,A
        JR      NZ,J563D
        LD      A,B
        CALL    C5662                   ; put in keyboardbuffer
        LD      A,(DE)
        INC     DE
J565D:  CALL    C5662                   ; put in keyboardbuffer
        JR      J563E


;         Subroutine put in keyboardbuffer
;            Inputs  ________________________
;            Outputs ________________________

C5662:  CALL    C56D0                   ; put in keyboardbuffer
        RET

J5666:  LD      HL,(CURLIN)
        INC     HL
        LD      A,H
        OR      L
        JR      Z,J5632
        LD      HL,TRPTBL+0*3-035H*3
        ADD     HL,DE
        ADD     HL,DE
        ADD     HL,DE

;         Subroutine raise trap
;            Inputs  ________________________
;            Outputs ________________________

C5674:  LD      A,(HL)
        AND     01H
        RET     Z
        LD      A,(HL)
        OR      04H
        CP      (HL)
        RET     Z
        LD      (HL),A
        XOR     05H
        RET     NZ
        LD      A,(ONGSBF)
        INC     A
        LD      (ONGSBF),A
        RET

;	Subroutine	handler HOME key
;	Inputs		-
;	Outputs		________________________

C5689:  LD      A,(NEWKEY+6)
        RRCA
        LD      A,0CH
        SBC     A,0
        JR      C56D0                   ; put in keyboardbuffer and quit

;	Subroutine	handler easily converted keys
;	Inputs		A = scancode (030H-015H)
;	Outputs		________________________

C5693:  CALL    H_KEYA
        LD      E,A
        LD      D,00H
        LD      HL,I5587-030H
        ADD     HL,DE
        LD      A,(HL)
J569E:  AND     A
        RET     Z
        JR      C56D0                   ; put in keyboardbuffer and quit

;	Subroutine	handler easily converted keys
;	Inputs		A = scancode
;	Outputs		________________________

C56A2:  LD      E,A
        LD      D,0
        LD      HL,M0DE3
        ADD     HL,DE
        LD      A,(EXPTBL+0)
        CALL    RDSLT
        JR      J569E

;	Subroutine	handler CAPS key
;	Inputs		-
;	Outputs		________________________

C56B1:  LD      HL,CAPST
        LD      A,(HL)
        CPL
        LD      (HL),A
        CPL
        AND     A
        LD      A,0CH
        JR      Z,J56BE
        INC     A
J56BE:  OUT     (0ABH),A
        RET

;	Subroutine	handler STOP key
;	Inputs		-
;	Outputs		________________________

C56C1:  LD      A,(NEWKEY+6)
        RRCA
        RRCA
        LD      A,03H   ; 3 
        JR      NC,J56CB
        INC     A
J56CB:  LD      (INTFLG),A
        JR      C,C56DE                 ; keyboard click and quit

;         Subroutine put in keyboardbuffer
;            Inputs  ________________________
;            Outputs ________________________

C56D0:  POP     HL
        PUSH    HL
        LD      BC,C5662+3
        OR      A
        SBC     HL,BC                   ; called from C5662 ?
        PUSH    DE
        CALL    C585E
        POP     DE
        RET

;         Subroutine keyboard click
;            Inputs  ________________________
;            Outputs ________________________

C56DE:  LD      A,(CLIKSW)
        AND     A
        RET     Z
        LD      A,(CLIKFL)
        AND     A
        RET     NZ
        LD      A,0FH
        LD      (CLIKFL),A
        OUT     (0ABH),A
        LD      A,10
J56F1:  DEC     A
        JR      NZ,J56F1
        AND     A
        LD      A,0EH
        JR      Z,J56FA
        INC     A
J56FA:  OUT     (0ABH),A
        RET

;	Subroutine	handle scancodes when KANA is on (and GRAPH and CTRL are not pressed)
;	Inputs		C = scancode (000H-02FH)
;	Outputs		________________________

J56FD:  LD      A,(KANAMD)
        AND     A
        LD      A,(NEWKEY+6)
        RRCA
        JR      Z,J5711
        LD      HL,I5792
        JR      C,J5719
        LD      HL,I57C2
        JR      J5719
J5711:  LD      HL,I5732
        JR      C,J5719
        LD      HL,I5762
J5719:  LD      B,00H
        ADD     HL,BC
        LD      BC,C56D0
        PUSH    BC                      ; put in keyboardbuffer
        LD      A,(CAPST)
        AND     A
        LD      A,(HL)
        RET     NZ
        CP      0A6H
        RET     C
        CP      0B0H
        RET     Z
        CP      0DEH
        RET     NC
        XOR     20H
        RET


;	Table		ANSI layout, without SHIFT (based on CAPS on)

I5732:	DEFB	0C9H,0B1H,0B2H,0B3H,0B4H,0B5H,0C5H,0C6H
        DEFB	0C7H,0C8H,0D7H,0D8H,0D9H,0DAH,0DBH,0D3H
        DEFB	0DEH,0DFH,0D6H,0DCH,0A6H,0DDH,0BBH,0C4H
        DEFB	0C2H,0BDH,0B8H,0BEH,0BFH,0CFH,0CCH,0D0H
        DEFB	0D1H,0D2H,0D5H,0D4H,0CDH,0CEH,0B6H,0B9H
        DEFB	0BCH,0BAH,0CBH,0C3H,0B7H,0C1H,0CAH,0C0H

;	Table		ANSI layout, with SHIFT (based on CAPS on)

I5762:	DEFB	0C9H,0A7H,0A8H,0A9H,0AAH,0ABH,0C5H,0C6H
        DEFB	0C7H,0C8H,0D7H,0D8H,0D9H,0DAH,0A2H,0D3H
        DEFB	0B0H,0A3H,0AEH,0A4H,0A1H,0A5H,0BBH,0C4H
        DEFB	0AFH,0BDH,0B8H,0BEH,0BFH,0CFH,0CCH,0D0H
        DEFB	0D1H,0D2H,0ADH,0ACH,0CDH,0CEH,0B6H,0B9H
        DEFB	0BCH,0BAH,0CBH,0C3H,0B7H,0C1H,0CAH,0C0H

;	Table		JIS layout, without SHIFT (based on CAPS on)

I5792:	DEFB	0DCH,0C7H,0CCH,0B1H,0B3H,0B4H,0B5H,0D4H
        DEFB	0D5H,0D6H,0CEH,0CDH,0B0H,0DEH,0DFH,0DAH
        DEFB	0B9H,0D1H,0C8H,0D9H,0D2H,0DBH,0C1H,0BAH
        DEFB	0BFH,0BCH,0B2H,0CAH,0B7H,0B8H,0C6H,0CFH
        DEFB	0C9H,0D8H,0D3H,0D0H,0D7H,0BEH,0C0H,0BDH
        DEFB	0C4H,0B6H,0C5H,0CBH,0C3H,0BBH,0DDH,0C2H

;	Table		JIS layout, with SHIFT (based on CAPS on)

I57C2:	DEFB	0A6H,0C7H,0CCH,0A7H,0A9H,0AAH,0ABH,0ACH
        DEFB	0ADH,0AEH,0CEH,0CDH,0B0H,0DEH,0A2H,0DAH
        DEFB	0B9H,0A3H,0A4H,0A1H,0A5H,0DBH,0C1H,0BAH
        DEFB	0BFH,0BCH,0A8H,0CAH,0B7H,0B8H,0C6H,0CFH
        DEFB	0C9H,0D8H,0D3H,0D0H,0D7H,0BEH,0C0H,0BDH
        DEFB	0C4H,0B6H,0C5H,0CBH,0C3H,0BBH,0DDH,0AFH

;         Subroutine read keyboard row 8
;            Inputs  ________________________
;            Outputs ________________________

C57F2:  DI
        IN      A,(0AAH)
        AND     0F0H
        ADD     A,8
        OUT     (0AAH),A
        IN      A,(0A9H)
        EI
        RET

C57FF:  LD      HL,MODE
        BIT     0,(HL)			; in SHIFT KANA mode ?
        JR      Z,J580B                 ; nope,
        XOR     A
        RES     0,(HL)                  ; leave SHIFT KANA mode
        JR      J582B                   ; update KANA led
J580B:  LD      A,(KANAST)
        INC     A			; KANA ON ?
        JR      Z,J5828                 ; yep, KANA OFF
        LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        JR      Z,J5826                 ; yep, KANA ON
        LD      A,(NEWKEY+6)
        RRCA
        JR      C,J5826                 ; KANA ON
        XOR     A
        LD      (CHRCNT),A
        INC     A
        SET     0,(HL)                  ; enter SHIFT KANA mode
        JR      J582B                   ; update KANA led
J5826:  LD      A,0FFH
J5828:  LD      (KANAST),A

;         Subroutine update KANA led
;            Inputs  ________________________
;            Outputs ________________________

J582B:  PUSH    AF
        LD      A,0FH
        OUT     (0A0H),A
        IN      A,(0A2H)
        AND     7FH
        LD      B,A
        POP     AF
        OR      A
        LD      A,80H
        JR      Z,J583C
        XOR     A
J583C:  OR      B
        OUT     (0A1H),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5840:  LD      B,A
        LD      A,15
        DI
        OUT     (0A0H),A
        IN      A,(0A2H)
        DJNZ    J5850
        AND     0DFH
        OR      4CH
        JR      J5854
J5850:  AND     0AFH
        OR      03H
J5854:  OUT     (0A1H),A
        LD      A,14
        OUT     (0A0H),A
        EI
        IN      A,(0A2H)
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C585E:  PUSH    AF                      ; store flag
        LD      A,(MODE)
        AND     01H
        JR      Z,J58A7
        POP     AF                      ; restore flag
        PUSH    AF                      ; store flag
        LD      HL,(PUTPNT)
        PUSH    HL
        LD      IX,PUTCHR
        CALL    EXTROM
        POP     HL
        POP     AF                      ; restore flag
        RET     Z
        LD      B,0
        LD      DE,(PUTPNT)
        LD      (PUTPNT),HL
J587F:  LD      A,L
        CP      E
        JR      Z,J588B
        LD      A,(HL)
        PUSH    AF
        INC     B
        CALL    C58D3
        JR      J587F
J588B:  LD      A,B
        AND     A
        RET     Z
        LD      E,B
        LD      L,B
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,SP
        INC     HL
J5896:  LD      A,(HL)
        PUSH    HL
        PUSH    BC
        CALL    C58AD
        POP     BC
        POP     HL
        DEC     HL
        DEC     HL
        DJNZ    J5896
        LD      B,E
J58A3:  POP     AF
        DJNZ    J58A3
        RET
J58A7:  CALL    C56DE                   ; keyboard click
        POP     AF
        JR      Z,C58C3


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C58AD:  LD      HL,NORUSE
        BIT     5,(HL)
        JR      NZ,C58C3


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C58B4:  CALL    C75A4
        JR      C,C58C3
        LD      B,24H   ; "$"
        CALL    C7104
        LD      A,B
        CALL    C58C3
        LD      A,C


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C58C3:  LD      HL,(PUTPNT)
        LD      (HL),A
        CALL    C58D3
        LD      A,(GETPNT)
        CP      L
        RET     Z
        LD      (PUTPNT),HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C58D3:  INC     HL
        LD      A,L
        CP      18H
        RET     NZ
        LD      HL,KEYBUF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C58DC:  PUSH    HL
        PUSH    DE

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C58DE:  PUSH    BC
        PUSH    AF
        CALL    C58E8
        POP     AF
        POP     BC
        POP     DE
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C58E8:  CALL    C6386
        ADD     A,H
        LD      H,A
        LD      A,(IX+2)
        LD      (IX+2),00H
        AND     A
        LD      B,A
        JP      NZ,J5AAD
        LD      A,C
        CALL    C4982
        JR      C,J5974
        LD      (IX+2),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5903:  LD      A,(IDBYT2)
        CP      02H                     ; MSX2+ or higher ?
        RET     C                       ; nope, quit
        LD      A,(RG25SA)
        AND     18H
        CP      18H
        RET     NZ
        LD      A,(MODE)
        AND     20H     ; " "
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C5917:  CALL    C5903
        RET     NZ
        POP     IY
        LD      A,(NORUSE)
        PUSH    AF
        AND     0FH
        LD      DE,(FORCLR)
        PUSH    DE
        JR      NZ,J5953
        LD      DE,0F0FH
        LD      (FORCLR),DE
        LD      A,(NORUSE)
        AND     0F0H
        OR      01H
        LD      (NORUSE),A
        PUSH    IY
        PUSH    BC
        PUSH    HL
        CALL    C5972
        POP     HL
        POP     BC
        POP     IY
        LD      A,(NORUSE)
J5949:  AND     0F0H
        OR      02H
        LD      (NORUSE),A
        POP     DE
        PUSH    DE
        SCF
J5953:  PUSH    AF
        LD      A,E
        RLA
        RLA
        RLA
        RLA
        LD      E,A
        POP     AF
        LD      A,D
        RLA
        RLA
        RLA
        RLA
        LD      D,A
        LD      (FORCLR),DE
        CALL    C5972
        POP     DE
        LD      (FORCLR),DE
        POP     AF
        LD      (NORUSE),A
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C5972:  JP      (IY)

J5974:  CALL    C5917
        LD      A,C
        CP      0FFH
        JR      NZ,J597F
        LD      C,20H   ; " "
        LD      A,C
J597F:  CP      20H     ; " "
        JR      NZ,J598F
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        LD      A,01H   ; 1 
        JP      NC,J6028
J598F:  LD      A,(IX+0)
        AND     50H     ; "P"
        CP      50H     ; "P"
        JR      NZ,J59A2
        PUSH    HL
        LD      L,C
        LD      H,00H
        CALL    C6250
        POP     HL
        JR      J59D5
J59A2:  LD      A,(IX+0)
        AND     48H     ; "H"
        CP      08H     ; 8 
        JR      Z,J59B0
        CALL    C62E8
        JR      J59D5
J59B0:  LD      A,C
        AND     A
        JP      M,J59BB
        SUB     20H     ; " "
        LD      B,00H
        JR      J59BF
J59BB:  SUB     80H
        LD      B,0DH   ; 13 
J59BF:  CP      40H     ; "@"
        JR      C,J59C6
        SUB     40H     ; "@"
        INC     B
J59C6:  OUT     (0D8H),A
        LD      A,B
        OUT     (0D9H),A
        PUSH    HL
        LD      HL,HOLD8
        LD      BC,18D9H
        INIR
        POP     HL
J59D5:  LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        JP      C,J63BF
        LD      A,(IX+0)
        ADD     A,A
        JR      C,J5A2F
        JP      P,J59FA
        PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        LD      E,L
        LD      D,H
        ADD     HL,HL
        ADD     HL,DE
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,6
        JR      J5A08
J59FA:  PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,8
J5A08:  LD      (HOLD8+40),HL
        LD      A,L
        LD      (HOLD8+45),A
        POP     HL
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+38),HL
        LD      HL,16
        LD      (HOLD8+42),HL
        LD      A,08H   ; 8 
        LD      (HOLD8+46),A
        LD      A,01H   ; 1 
        LD      (HOLD8+47),A
        LD      DE,HOLD8
        JR      C5A81
J5A2F:  JP      P,J5A44
        PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        LD      E,L
        LD      D,H
        ADD     HL,HL
        ADD     HL,DE
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,6
        JR      J5A52
J5A44:  PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,8
J5A52:  LD      (HOLD8+40),HL
        LD      A,L
        LD      (HOLD8+45),A
        POP     HL
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+38),HL
        LD      HL,8
        LD      (HOLD8+42),HL
        LD      A,04H   ; 4 
        LD      (HOLD8+46),A
        LD      A,02H   ; 2 
        LD      (HOLD8+47),A
        LD      DE,HOLD8
        CALL    C5A81
        LD      HL,HOLD8+39
        SET     0,(HL)
        LD      DE,HOLD8+1

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5A81:  CALL    C619B
        CALL    C5A8B
        LD      A,08H   ; 8 
        ADD     A,L
        LD      L,A

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5A8B:  LD      A,(HOLD8+46)
J5A8E:  PUSH    AF
        LD      A,(HL)
J5A90:  RLA
        JP      NC,J5A9B
        OUT     (C),E
        DJNZ    J5A90
        JP      J5A9F
J5A9B:  OUT     (C),D
        DJNZ    J5A90
J5A9F:  LD      A,(HOLD8+45)
        LD      B,A
        LD      A,(HOLD8+47)
        ADD     A,L
        LD      L,A
        POP     AF
        DEC     A
        JR      NZ,J5A8E
        RET
J5AAD:  LD      A,C
        CALL    C4990
        JR      C,J5AC1
        LD      A,B
        CP      98H
        JR      NZ,J5AC4


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C5AB8:  LD      A,C
        CP      80H
        JR      C,J5AC4
        CP      9FH
        JR      NC,J5AC4
J5AC1:  LD      BC,8140H
J5AC4:  CALL    C5917
        DEC     H
        EX      DE,HL
        LD      HL,8140H
        AND     A
        SBC     HL,BC
        EX      DE,HL
        JR      NZ,J5ADE
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        LD      A,02H   ; 2 
        JP      NC,J6028
J5ADE:  PUSH    HL
        LD      HL,989EH
        AND     A
        SBC     HL,BC
        POP     HL
        BIT     6,(IX+0)
        JR      Z,J5B0A
        BIT     4,(IX+0)
        JR      NC,J5AF6
        BIT     6,(IX+1)
J5AF6:  JR      Z,J5B0A
        PUSH    HL
        LD      A,B
        AND     3FH     ; "?"
        ADD     A,0DH   ; 13 
        LD      H,A
        LD      A,C
        SUB     40H     ; "@"
        LD      L,A
        CALL    C6250
        POP     HL
        JP      J5BF1
J5B0A:  BIT     3,(IX+0)                ; JIS1 rom ?
        JR      NC,J5B15
        LD      A,(MODE)
        AND     40H     ; "@"
J5B15:  JP      NZ,J5BB4                ; JIS1 rom,
        PUSH    HL
        LD      HL,8140H
        AND     A
        SBC     HL,BC
        JR      NZ,J5B31
        LD      HL,HOLD8+0
        LD      DE,HOLD8+1
        LD      BC,32-1
        LD      (HL),0
        LDIR
        JP      J5BB1
J5B31:  CALL    C621D
        CP      01H     ; 1 
        JR      NZ,J5B51
        LD      A,C
        ADD     A,20H   ; " "
        LD      HL,I7420+42-1
        LD      BC,42
        CPDR
        JR      NZ,J5B4C
        LD      HL,I744A
        ADD     HL,BC
        LD      A,(HL)
        JR      J5B58
J5B4C:  SUB     20H     ; " "
        LD      C,A
        LD      A,01H   ; 1 
J5B51:  CP      03H     ; 3 
        JR      NZ,J5B5E
        LD      A,C
        ADD     A,20H   ; " "
J5B58:  LD      HL,I5B68
        CP      A
        JR      J5B70
J5B5E:  CP      04H     ; 4 
        JR      Z,J5B66
        CP      05H     ; 5 
        JR      NZ,J5BA5
J5B66:  CP      05H     ; 5 
I5B68:  LD      B,00H
        LD      HL,I7372
        ADD     HL,BC
        ADD     HL,BC
        LD      A,(HL)
J5B70:  PUSH    AF
        JR      Z,J5B75
        XOR     20H     ; " "
J5B75:  LD      C,A
        CALL    C62E8
        POP     AF
        JR      Z,J5B94
        BIT     6,(IX+0)
        JR      Z,J5B94
        INC     HL
        LD      A,(HL)
        AND     A
        DEC     HL
        JR      NZ,J5B8D
        CALL    C6359
        JR      J5BB1
J5B8D:  LD      B,18H
        PUSH    HL
        CALL    C623E
        POP     HL
J5B94:  INC     HL
        LD      A,(HL)
        AND     A
        PUSH    AF
        CALL    Z,C6310
        POP     AF
        LD      C,A
        LD      DE,HOLD8+8
        CALL    NZ,C62EB
        JR      J5BB1
J5BA5:  PUSH    BC
        LD      DE,HOLD8
        CALL    C62CC
        POP     BC
        LD      A,C
        CALL    C62CC
J5BB1:  POP     HL
        JR      J5BF1
J5BB4:  PUSH    HL
        CALL    C621D
        LD      E,C
        LD      D,00H
        LD      L,A
        LD      H,D
        LD      C,L
        LD      B,H
        ADD     HL,HL
        ADD     HL,BC
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,DE
        CP      30H     ; "0"
        JR      C,J5BD3
        LD      A,H
        SUB     12H     ; 18 
        LD      H,A
        LD      C,0DAH
        JR      J5BDB
J5BD3:  CP      10H     ; 16 
        JR      C,J5BD9
        DEC     H
        DEC     H
J5BD9:  LD      C,0D8H
J5BDB:  OUT     (C),L
        ADD     HL,HL
        ADD     HL,HL
        INC     C
        OUT     (C),H
        LD      HL,HOLD8
        LD      B,20H   ; " "
        INIR
        BIT     6,(IX+0)
        CALL    NZ,C623C
        POP     HL
J5BF1:  LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        JP      C,J63B6
        LD      A,(IX+0)
        ADD     A,A
        JR      C,J5C51
        JP      P,J5C1B
        PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        LD      E,L
        LD      D,H
        ADD     HL,HL
        ADD     HL,DE
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,12
        LD      (HOLD8+40),HL
        LD      A,06H   ; 6 
        JR      J5C2E
J5C1B:  PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,16
        LD      (HOLD8+40),HL
        LD      A,08H   ; 8 
J5C2E:  LD      (HOLD8+45),A
        POP     HL
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+38),HL
        LD      HL,16
        LD      (HOLD8+42),HL
        LD      A,08H   ; 8 
        LD      (HOLD8+46),A
        LD      A,0F9H
        LD      (HOLD8+47),A
        LD      DE,HOLD8
        JR      C5CA9
J5C51:  JP      P,J5C6B
        PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        LD      E,L
        LD      D,H
        ADD     HL,HL
        ADD     HL,DE
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,12
        LD      (HOLD8+40),HL
        LD      A,06H   ; 6 
        JR      J5C7E
J5C6B:  PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,16
        LD      (HOLD8+40),HL
        LD      A,08H   ; 8 
J5C7E:  LD      (HOLD8+45),A
        POP     HL
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+38),HL
        LD      HL,8
        LD      (HOLD8+42),HL
        LD      A,04H   ; 4 
        LD      (HOLD8+46),A
        LD      A,0FAH
        LD      (HOLD8+47),A
        LD      DE,HOLD8
        CALL    C5CA9
        LD      HL,HOLD8+39
        SET     0,(HL)
        LD      DE,HOLD8+1

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5CA9:  CALL    C619B
        CALL    C5CB3
        LD      A,08H   ; 8 
        ADD     A,L
        LD      L,A

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5CB3:  LD      A,(HOLD8+46)
J5CB6:  PUSH    AF
        LD      A,(HL)
J5CB8:  RLA
        JP      NC,J5CC3
        OUT     (C),E
        DJNZ    J5CB8
        JP      J5CC7
J5CC3:  OUT     (C),D
        DJNZ    J5CB8
J5CC7:  LD      A,(HOLD8+45)
        LD      B,A
        LD      A,08H   ; 8 
        ADD     A,L
        LD      L,A
J5CCF:  LD      A,(HL)
J5CD0:  RLA
        JP      NC,J5CDB
        OUT     (C),E
        DJNZ    J5CD0
        JP      J5CDF
J5CDB:  OUT     (C),D
        DJNZ    J5CD0
J5CDF:  LD      A,(HOLD8+45)
        LD      B,A
        LD      A,(HOLD8+47)
        ADD     A,L
        LD      L,A
        POP     AF
        DEC     A
        JR      NZ,J5CB6
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5CED:  PUSH    HL
        CALL    C5CF3
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5CF3:  CALL    C6386
        ADD     A,H
        LD      H,A
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        JP      C,J655D
        LD      A,(IX+0)
        ADD     A,A
        JP      C,J5D54
        JP      P,J5D1E
        PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        LD      C,L
        LD      B,H
        ADD     HL,HL
        ADD     HL,BC
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,6
        JR      J5D2C
J5D1E:  PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,8
J5D2C:  BIT     1,E
        JR      Z,J5D31
        ADD     HL,HL
J5D31:  LD      (HOLD8+40),HL
        POP     HL
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      D,00H
        BIT     0,E
        LD      E,00H
        JR      Z,J5D46
        LD      E,0AH   ; 10 
J5D46:  ADD     HL,DE
        LD      (HOLD8+38),HL
        LD      HL,16
        SBC     HL,DE
        LD      (HOLD8+42),HL
        JR      C5DA7
J5D54:  BIT     6,(IX+0)
        JR      Z,J5D6C
        PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        LD      C,L
        LD      B,H
        ADD     HL,HL
        ADD     HL,BC
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,6
        JR      J5D7A
J5D6C:  PUSH    HL
        LD      L,H
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      (HOLD8+36),HL
        LD      HL,8
J5D7A:  BIT     1,E
        JR      Z,J5D7F
        ADD     HL,HL
J5D7F:  LD      (HOLD8+40),HL
        POP     HL
        DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      D,00H
        BIT     0,E
        LD      E,00H
        JR      Z,J5D93
        LD      E,05H   ; 5 
J5D93:  ADD     HL,DE
        LD      (HOLD8+38),HL
        LD      HL,8
        SBC     HL,DE
        LD      (HOLD8+42),HL
        CALL    C5DA7
        LD      HL,HOLD8+39
        SET     0,(HL)

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5DA7:  CALL    C616D
        INC     C
        INC     C
        LD      HL,(HOLD8+36)
        OUT     (C),L
        OUT     (C),H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5DB3:  LD      HL,(HOLD8+38)
        LD      A,(RG23SA)
        ADD     A,L
        OUT     (C),A
        LD      A,(ACPAGE)
        ADD     A,H
        OUT     (C),A
        LD      HL,(HOLD8+40)
        OUT     (C),L
        OUT     (C),H
        LD      HL,(HOLD8+42)
        OUT     (C),L
        OUT     (C),H
        LD      A,(FORCLR)
        LD      B,A
        LD      A,(BAKCLR)
        XOR     B
        LD      B,A
        CALL    C5903
        LD      A,B
        JR      NZ,J5DE3
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
J5DE3:  OUT     (C),A
        XOR     A
        OUT     (C),A
        LD      A,83H
        OUT     (C),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5DED:  PUSH    HL
        PUSH    AF
        CALL    C5DF5
        POP     AF
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5DF5:  LD      D,A
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        JP      C,J669B
        CALL    C5E06
        JP      C617E

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5E06:  BIT     5,(IX+0)
        JP      Z,C5E8E
        LD      A,L
        DEC     A
        JP      NZ,C5E8E
        BIT     7,(IX+0)
        LD      E,10H   ; 16 
        JR      Z,J5E1C
        LD      E,08H   ; 8 
J5E1C:  PUSH    AF
        LD      HL,00E2H
        LD      A,0FH   ; 15 
        CALL    C5FDD
        CALL    C5F1C
        POP     AF
        JR      NZ,J5E64
        LD      A,D
        AND     A
        JR      Z,C5E50
        LD      HL,00C0H
        LD      E,33H   ; "3"
        LD      A,04H   ; 4 
        CALL    C5EB0
        LD      HL,00C4H
        LD      E,10H   ; 16 
        LD      A,0CH   ; 12 
        CALL    C5EB0
        CALL    C5E50
        LD      HL,00E3H
        LD      E,0DDH
        LD      A,04H   ; 4 
        JP      C5EB0

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5E50:  CALL    C5EE1
        LD      C,10H   ; 16 

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5E55:  LD      A,(RG23SA)
        ADD     A,C
        LD      B,A
        LD      C,17H
        PUSH    BC
        CALL    C617E
        POP     BC
        JP      WRTVDP

J5E64:  LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      A,D
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      E,38H   ; "8"
        PUSH    AF
        PUSH    HL
        CALL    NZ,C5EA5
        POP     HL
        PUSH    HL
        LD      A,0DCH
        SUB     L
        CALL    C5EFD
        LD      C,08H   ; 8 
        CALL    C5E55
        POP     HL
        POP     AF
        LD      DE,48
        ADD     HL,DE
        LD      E,0D0H
        AND     A
        CALL    NZ,C5EA5
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5E8E:  LD      A,D
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     A,A
        ADD     A,A
        ADD     A,A
        BIT     7,(IX+0)
        LD      E,0F8H
        JR      NZ,C5EA5
        ADD     HL,HL
        ADD     A,A
        LD      E,0F0H
        JR      C5EB0


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C5EA5:  PUSH    HL
        PUSH    DE
        PUSH    AF
        CALL    C5EB0
        POP     AF
        POP     DE
        POP     HL
        SET     0,H


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C5EB0:  LD      B,A
        LD      A,(RG23SA)
        ADD     A,L
        LD      L,A
        LD      A,E
        ADD     A,L
        LD      E,A
        LD      D,H
J5EBA:  LD      A,E
        CP      L
        JR      NC,J5EBF
        LD      A,L
J5EBF:  NEG
        CP      B
        JR      C,J5EC5
        LD      A,B
J5EC5:  PUSH    AF
        CALL    C5ED7
        POP     AF
        LD      C,A
        LD      A,L
        ADD     A,C
        LD      L,A
        LD      A,E
        ADD     A,C
        LD      E,A
        LD      A,B
        SUB     C
        LD      B,A
        JR      NZ,J5EBA
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C5ED7:  CALL    C61F3
        OUT     (C),A
        LD      A,0E0H
        OUT     (C),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5EE1:  CALL    C5F1C
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      A,10H   ; 16 
        CALL    C5F06
        LD      HL,00E0H
        LD      A,04H   ; 4 
        JR      C5F06

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5EF6:  LD      HL,00F0H
        LD      A,10H   ; 16 
        JR      C5F06

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5EFD:  PUSH    HL
        PUSH    AF
        CALL    C5F06
        POP     AF
        POP     HL
        SET     0,H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C5F06:  PUSH    AF
        LD      C,00H
        LD      A,(IX+0)
        INC     A
        AND     02H     ; 2 
        CP      01H     ; 1 
        ADC     A,C
        LD      B,A
        LD      E,C
        LD      D,C
        POP     AF
        CALL    C6080
        JP      C617E


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5F1C:  CALL    C4CEE
        LD      L,A
        LD      A,(CNSDFG)
        AND     01H     ; 1 
        LD      D,A
        CALL    C6B7B
        RET     C
        INC     D
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5F2C:  PUSH    HL
        PUSH    AF
        CALL    C5F34
        POP     AF
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5F34:  LD      D,A
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        JP      C,J66CF
        CALL    C5F45
        JP      C617E

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5F45:  BIT     5,(IX+0)
        JR      Z,J5FB8
        LD      A,L
        SUB     D
        DEC     A
        JR      NZ,J5FB8
        BIT     7,(IX+0)
        JR      NZ,J5F79
        CALL    C5EF6
        LD      C,0F0H
        CALL    C5E55
        CALL    C5F1C
        INC     L
        LD      A,D
        AND     A
        CALL    NZ,C5E8E
        LD      HL,00D0H
        LD      A,04H   ; 4 
        CALL    C5F06
        LD      E,0F0H
        LD      HL,00E4H
        LD      A,0FH   ; 15 
        JP      C5EB0
J5F79:  CALL    C5F1C
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      A,D
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      E,30H   ; "0"
        PUSH    AF
        PUSH    HL
        CALL    NZ,C5EA5
        POP     HL
        PUSH    HL
        LD      A,0CCH
        SUB     L
        CALL    C5EFD
        LD      HL,00F8H
        LD      A,08H   ; 8 
        CALL    C5EFD
        LD      C,0F8H
        CALL    C5E55
        LD      E,0F8H
        LD      HL,00DCH
        LD      A,0FH   ; 15 
        CALL    C5EB0
        POP     HL
        POP     AF
        LD      DE,56
        ADD     HL,DE
        LD      E,0C8H
        AND     A
        CALL    NZ,C5EA5
        RET
J5FB8:  LD      A,D
        LD      H,00H
        DEC     L
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        ADD     A,A
        ADD     A,A
        ADD     A,A
        BIT     7,(IX+0)
        JR      NZ,J5FCF
        ADD     HL,HL
        DEC     HL
        ADD     A,A
        LD      E,10H   ; 16 
        JR      C5FDD
J5FCF:  DEC     HL
        LD      E,08H   ; 8 
        PUSH    HL
        PUSH    DE
        PUSH    AF
        CALL    C5FDD
J5FD8:  POP     AF
        POP     DE
        POP     HL
        SET     0,H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C5FDD:  LD      B,A
        LD      A,(RG23SA)
        ADD     A,L
        LD      L,A
        LD      A,E
        ADD     A,L
        LD      E,A
        LD      D,H
J5FE7:  LD      A,E
        CP      L
        JR      C,J5FEC
        LD      A,L
J5FEC:  INC     A
        CP      B
        JR      C,J5FF1
        LD      A,B
J5FF1:  PUSH    AF
        CALL    C6003
        POP     AF
        LD      C,A
        LD      A,L
        SUB     C
        LD      L,A
        LD      A,E
        SUB     C
        LD      E,A
        LD      A,B
        SUB     C
        LD      B,A
        JR      NZ,J5FE7
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6003:  CALL    C61F3
        LD      A,08H   ; 8 
        OUT     (C),A
        LD      A,0E0H
        OUT     (C),A
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C600F:  CALL    C6386
        ADD     A,H
        LD      H,A
        LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        JP      C,J6705
        CALL    C6024
        JP      C617E


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6024:  CALL    C5917
        SCF
J6028:  PUSH    HL
        PUSH    AF
        LD      L,H
        LD      H,00H
        DEC     L
        ADD     HL,HL
        BIT     6,(IX+0)
        JR      Z,J603F
        LD      E,L
        LD      D,H
        ADD     HL,HL
        ADD     HL,DE
        EX      DE,HL
        LD      BC,0C06H
        JR      J6045
J603F:  ADD     HL,HL
        ADD     HL,HL
        EX      DE,HL
        LD      BC,1008H
J6045:  POP     AF
        JR      C,J6051
        DEC     A
        JR      Z,J604C
        LD      C,B
J604C:  LD      B,00H
        POP     HL
        JR      J6064
J6051:  LD      L,00H
        LD      A,(IX+0)
        INC     A
        AND     02H     ; 2 
        CP      01H     ; 1 
        ADC     A,L
        LD      H,A
        SBC     HL,DE
        LD      C,L
        LD      B,H
        POP     HL
        RET     Z
        RET     C
J6064:  DEC     L
        LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        BIT     7,(IX+0)
        JR      NZ,J6075
        ADD     HL,HL
        LD      A,10H   ; 16 
        JR      C6080
J6075:  PUSH    BC
        PUSH    HL
        CALL    C607E
        POP     HL
        POP     BC
        SET     0,H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C607E:  LD      A,08H   ; 8 

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6080:  PUSH    AF
        LD      A,(RG23SA)
        ADD     A,L
        LD      L,A
        POP     AF
        NEG
        CP      L
        JR      NC,C6099
        PUSH    HL
        PUSH    BC
        PUSH    AF
        LD      A,L
        CALL    C6099
        POP     AF
        POP     BC
        POP     HL
        SUB     L
        LD      L,00H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6099:  NEG
        PUSH    AF
        PUSH    BC
        CALL    C616D
        INC     C
        INC     C
        OUT     (C),E
        OUT     (C),D
        OUT     (C),L
        LD      A,(ACPAGE)
        ADD     A,H
        OUT     (C),A
        POP     HL
        OUT     (C),L
        OUT     (C),H
        POP     AF
        OUT     (C),A
        XOR     A
        OUT     (C),A
        LD      A,(NORUSE)
        AND     0FH
        JR      NZ,J60EF
        LD      A,(IX+0)
        BIT     0,A
        JR      Z,J60D8
        BIT     1,A
        JR      Z,J60E5
        LD      A,(BAKCLR)
J60CE:  OUT     (C),A
        XOR     A
        OUT     (C),A
        LD      A,0C0H
        OUT     (C),A
        RET
J60D8:  BIT     0,E
        JR      NZ,J60EF
        BIT     0,L
J60DE:  JR      NZ,J60EF
        CALL    C6101
        JR      J60CE
J60E5:  LD      A,E
        AND     03H     ; 3 
        JR      NZ,J60EF
        LD      A,L
        AND     03H     ; 3 
        JR      J60DE
J60EF:  LD      A,(BAKCLR)
        OUT     (C),A
        XOR     A
        OUT     (C),A
        LD      A,(NORUSE)
        AND     0FH
        OR      80H
        OUT     (C),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6101:  LD      A,(BAKCLR)
        BIT     0,(IX+0)
        JR      Z,J6113
        BIT     1,(IX+0)
        RET     NZ
        LD      B,A
        ADD     A,A
        ADD     A,A
        OR      B
J6113:  LD      B,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        OR      B
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C611A:  LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        JP      C,J67AD
        LD      H,00H
        CALL    C6130
        BIT     7,(IX+0)
        RET     Z
        LD      H,01H   ; 1 

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6130:  LD      A,(SCRMOD)
        CP      02H     ; 2 
        LD      DE,256
        JR      C,J613D
        LD      DE,212
J613D:  CALL    C616D
        INC     C
        INC     C
        XOR     A
        OUT     (C),A
        OUT     (C),A
        OUT     (C),A
        LD      A,(ACPAGE)
        ADD     A,H
        OUT     (C),A
        OUT     (C),A
        LD      A,(IX+0)
        INC     A
        AND     02H     ; 2 
        CP      01H     ; 1 
        ADC     A,00H
        OUT     (C),A
        OUT     (C),E
        OUT     (C),D
        CALL    C6101
        OUT     (C),A
        XOR     A
        OUT     (C),A
        LD      A,0C0H
        JR      J617C

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C616D:  LD      A,24H   ; "$"

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C616F:  PUSH    AF
        LD      A,(VDP_DR)
        LD      C,A
        INC     C
        POP     AF
        DI
        OUT     (C),A
        LD      A,91H
        EI
J617C:  OUT     (C),A

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C617E:  LD      A,(VDP_DR)
        LD      C,A
        INC     C
J6183:  LD      A,02H   ; 2 
        DI
        OUT     (C),A
        LD      A,8FH
        OUT     (C),A
        IN      A,(C)
        RRA
        LD      A,00H
        OUT     (C),A
        LD      A,8FH
        EI
        OUT     (C),A
        JR      C,J6183
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C619B:  CALL    C616D
        INC     C
        INC     C
        LD      HL,(HOLD8+36)
        OUT     (C),L
        OUT     (C),H
        LD      HL,(HOLD8+38)
        LD      A,(RG23SA)
        ADD     A,L
        OUT     (C),A
        LD      A,(ACPAGE)
        ADD     A,H
        OUT     (C),A
        LD      HL,(HOLD8+40)
        OUT     (C),L
        OUT     (C),H
        LD      HL,(HOLD8+42)
        OUT     (C),L
        OUT     (C),H
        EX      DE,HL
        LD      DE,(FORCLR)
        RL      (HL)
        JP      NC,J61D1
        OUT     (C),E
        DEFB    0D2H
J61D1:  OUT     (C),D
        XOR     A
        OUT     (C),A
        LD      A,(NORUSE)
        AND     0FH
        OR      0B0H
        OUT     (C),A
        DEC     C
        DEC     C
        LD      A,0ACH
        DI
        OUT     (C),A
        LD      A,91H
        EI
        OUT     (C),A
        INC     C
        INC     C
        LD      A,(HOLD8+45)
        DEC     A
        LD      B,A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C61F3:  PUSH    AF
        LD      A,22H   ; """
        CALL    C616F
        INC     C
        INC     C
        OUT     (C),L
        LD      A,(ACPAGE)
        ADD     A,H
        OUT     (C),A
        XOR     A
        OUT     (C),A
        OUT     (C),A
        OUT     (C),E
        LD      A,(ACPAGE)
        ADD     A,D
        OUT     (C),A
        OUT     (C),A
        OUT     (C),A
        POP     AF
        OUT     (C),A
        XOR     A
        OUT     (C),A
        OUT     (C),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C621D:  LD      A,C
        CP      80H
        ADC     A,0FFH
        CP      9EH
        PUSH    AF
        JR      C,J6229
        SUB     5EH     ; "^"
J6229:  SUB     3FH     ; "?"
        LD      C,A
        LD      A,B
        CP      0E0H
        JR      C,J6233
        SUB     40H     ; "@"
J6233:  SUB     81H
        LD      B,A
        POP     AF
        CCF
        LD      A,B
        RLA
        INC     A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C623C:  LD      B,20H   ; " "

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C623E:  LD      HL,HOLD8
        LD      D,0
J6243:  LD      E,(HL)
        PUSH    HL
        LD      HL,I7274
        ADD     HL,DE
        LD      A,(HL)
        POP     HL
        LD      (HL),A
        INC     HL
        DJNZ    J6243
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6250:  IN      A,(40H)
        PUSH    AF
        LD      A,0F7H
        OUT     (40H),A
        PUSH    HL
        LD      DE,253FH
        OR      A
        SBC     HL,DE
        POP     HL
        IN      A,(41H)
        JR      NC,J6265
        IN      A,(40H)
J6265:  CP      08H     ; 8 
        JR      Z,J626D
        LD      A,03H   ; 3 
        OUT     (42H),A
J626D:  CALL    C6275
        POP     AF
        CPL
        OUT     (40H),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6275:  LD      A,H
        OUT     (47H),A
        LD      A,L
        OUT     (48H),A
        LD      HL,0
        LD      (HOLD8+0),HL
        LD      (HOLD8+8),HL
        LD      (HOLD8+22),HL
        LD      (HOLD8+30),HL
        LD      IY,HOLD8+2
        CALL    C6295
        LD      IY,HOLD8+16


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6295:  LD      B,03H   ; 3 
J6297:  IN      A,(49H)
        LD      (IY+0),A
        RRCA
        RRCA
        AND     0C0H
        LD      C,A
        IN      A,(49H)
        LD      E,A
        RRCA
        RRCA
        AND     3FH     ; "?"
        OR      C
        LD      (IY+8),A
        LD      A,E
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      C,A
        IN      A,(49H)
        LD      E,A
        RRCA
        RRCA
        RRCA
        RRCA
        AND     0FH     ; 15 
        OR      C
        LD      (IY+1),A
        LD      A,E
        ADD     A,A
        ADD     A,A
        LD      (IY+9),A
        INC     IY
        INC     IY
        DJNZ    J6297
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C62CC:  ADD     A,20H   ; " "
        PUSH    AF
        RRCA
        RRCA
        RRCA
        RRCA
        CALL    C62D7
        POP     AF

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C62D7:  AND     0FH     ; 15 
        CP      0AH     ; 10 
        SBC     A,69H   ; "i"
        DAA
        LD      L,A
        CALL    C6305
        LD      BC,8
        LDIR
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C62E8:  LD      DE,HOLD8

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C62EB:  PUSH    HL
        LD      L,C
        CALL    C6305
        LD      C,02H   ; 2 
J62F2:  LD      B,04H   ; 4 
J62F4:  LD      A,(HL)
        INC     HL
        LD      (DE),A
        INC     DE
        LD      (DE),A
        INC     DE
        DJNZ    J62F4
        LD      A,08H   ; 8 
        ADD     A,E
        LD      E,A
        DEC     C
        JR      NZ,J62F2
        POP     HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6305:  LD      H,00H
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        LD      BC,(CGTABL)
        ADD     HL,BC
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6310:  LD      IY,HOLD8
        CALL    C631B
        LD      IY,HOLD8+16

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C631B:  LD      D,04H   ; 4 
J631D:  LD      E,02H   ; 2 
        LD      A,(IY+0)
J6322:  LD      B,04H   ; 4 
        BIT     6,(IX+0)
        JR      Z,J632B
        DEC     B
J632B:  LD      C,00H
J632D:  ADD     A,A
        RL      C
        RRC     C
        RL      C
        RL      C
        DJNZ    J632D
        BIT     6,(IX+0)
        JR      Z,J6342
        RL      C
        RL      C
J6342:  LD      (IY+0),C
        LD      (IY+1),C
        LD      BC,8
        ADD     IY,BC
        DEC     E
        JR      NZ,J6322
        LD      BC,-14
        ADD     IY,BC
        DEC     D
        JR      NZ,J631D
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6359:  LD      IY,HOLD8
        CALL    C6364
        LD      IY,HOLD8+16

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6364:  LD      B,04H   ; 4 
J6366:  LD      A,(IY+0)
        RRCA
        RRCA
        LD      C,A
        AND     3CH     ; "<"
        LD      (IY+0),A
        LD      (IY+1),A
        LD      A,C
        RRCA
        RRCA
        AND     0F0H
        LD      (IY+8),A
        LD      (IY+9),A
        INC     IY
        INC     IY
        DJNZ    J6366
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6386:  PUSH    BC
        CALL    C6395
        LD      A,(LINLEN)
        NEG
        ADD     A,B
        INC     A
        AND     A
        RRA
        POP     BC
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6395:  LD      A,(IX+0)
        AND     07H     ; 7 
        LD      BC,080AH
        CP      02H     ; 2 
        JR      Z,J63AF
        LD      BC,4050H
        CP      05H     ; 5 
        JR      Z,J63AF
        CP      06H     ; 6 
        JR      Z,J63AF
        LD      BC,2028H
J63AF:  BIT     6,(IX+0)
        RET     Z
        LD      B,C
        RET
J63B6:  CP      02H     ; 2 
        JP      Z,J64D4
        LD      E,10H   ; 16 
        JR      J63C6
J63BF:  CP      02H     ; 2 
        JP      Z,J64D8
        LD      E,08H   ; 8 
J63C6:  LD      A,L
        DEC     A
        ADD     A,A
        LD      L,A
        LD      A,H
        DEC     A
        ADD     A,A
        BIT     6,(IX+0)
        JR      NZ,J6405
        ADD     A,A
        ADD     A,A
        LD      H,L
        LD      L,A
        LD      A,(VDP_DR)
        LD      C,A
        INC     C
        LD      A,E
        LD      E,L
        LD      D,H
        CALL    C67CF
        LD      HL,HOLD8
        LD      B,A
        DEC     C
J63E7:  OUTI
        JP      NZ,J63E7
        INC     C
        LD      L,E
        LD      H,D
        INC     H
        CALL    C67CF
        LD      HL,HOLD8+16
        LD      B,A
        DEC     C
J63F8:  OUTI
        JP      NZ,J63F8
        INC     C
        EX      DE,HL
        SET     5,H
        LD      B,A
        JP      J6746
J6405:  LD      B,A
        ADD     A,A
        ADD     A,B
        LD      H,L
        LD      L,A
        LD      A,(VDP_DR)
        LD      C,A
        INC     C
        LD      A,E
        CP      08H     ; 8 
        LD      IY,HOLD8
        JR      Z,C6426
        PUSH    HL
        CALL    C6426
        POP     HL
        LD      A,06H   ; 6 
        ADD     A,L
        LD      L,A
        LD      IY,HOLD8+8
        INC     C

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6426:  LD      A,L
        RES     0,L
        RES     1,L
        RES     2,L
        AND     07H     ; 7 
        LD      D,03H   ; 3 
        JR      Z,C6476
        CP      02H     ; 2 
        JR      Z,J646D
        CP      06H     ; 6 
        JR      Z,J6454
        LD      D,04H   ; 4 
        PUSH    HL
        CALL    C64AD
        POP     HL
        LD      D,0F0H
        PUSH    IY
        CALL    C6476
        POP     IY
        INC     C
        LD      DE,0DF08H
        ADD     HL,DE
        LD      D,3FH   ; "?"
        JR      C6476
J6454:  LD      D,02H   ; 2 
        PUSH    HL
        CALL    C64AD
        POP     HL
        LD      D,0FCH
        PUSH    IY
        CALL    C6476
        POP     IY
        INC     C
        LD      DE,0DF08H
        ADD     HL,DE
        LD      D,0FH   ; 15 
        JR      C6476
J646D:  LD      D,0FEH
        PUSH    HL
        CALL    C64AD
        POP     HL
        LD      D,0C0H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6476:  PUSH    BC
        CALL    C648C
        LD      BC,8
        ADD     IY,BC
        POP     BC
        INC     H
        CALL    C648C
        DEC     H
        SET     5,H
J6487:  LD      B,08H   ; 8 
        JP      J6746

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C648C:  PUSH    HL
        CALL    C67D1
        DEC     C
        LD      HL,PATWRK
        LD      B,08H   ; 8 
J6496:  IN      A,(C)
        AND     D
        LD      E,A
        LD      A,D
        CPL
        AND     (IY+0)
        OR      E
        LD      (HL),A
        INC     HL
        INC     IY
        DJNZ    J6496
        POP     HL
        PUSH    HL
        LD      B,08H   ; 8 
        JP      J6623

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C64AD:  PUSH    IY
        POP     HL
        CALL    C64B7
        LD      A,08H   ; 8 
        ADD     A,L
        LD      L,A

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C64B7:  LD      E,08H   ; 8 
        BIT     7,D
        JR      NZ,J64C8
J64BD:  LD      B,D
        LD      A,(HL)
J64BF:  RLCA
        DJNZ    J64BF
        LD      (HL),A
        INC     HL
        DEC     E
        JR      NZ,J64BD
        RET
J64C8:  LD      B,D
        LD      A,(HL)
J64CA:  RRCA
        INC     B
        JR      NZ,J64CA
        LD      (HL),A
        INC     HL
        DEC     E
        JR      NZ,J64C8
        RET
J64D4:  LD      E,02H   ; 2 
        JR      J64DA
J64D8:  LD      E,01H   ; 1 
J64DA:  LD      A,L
        DEC     A
        ADD     A,A
        LD      L,A
        LD      A,H
        DEC     A
        CALL    C679F
        LD      H,L
        LD      L,A
        LD      A,(VDP_DR)
        LD      C,A
        DEC     E
        LD      DE,HOLD8
        JR      Z,C6504
        PUSH    HL
        CALL    C6504
        POP     HL
        BIT     6,(IX+0)
        LD      DE,32
        JR      Z,J6500
        LD      DE,24
J6500:  ADD     HL,DE
        LD      DE,HOLD8+8

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6504:  PUSH    HL
        CALL    C650F
        LD      DE,16
        ADD     HL,DE
        EX      DE,HL
        POP     HL
        INC     H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C650F:  INC     C
        CALL    C67CF
        EX      DE,HL
        DEC     C
        BIT     6,(IX+0)
        LD      E,04H   ; 4 
        JR      Z,J651F
        LD      E,03H   ; 3 
J651F:  PUSH    HL
        LD      B,08H   ; 8 
J6522:  RL      (HL)
        LD      A,(BAKCLR)
        JP      NC,J652D
        LD      A,(FORCLR)
J652D:  ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      D,A
        RL      (HL)
        LD      A,(BAKCLR)
        JP      NC,J653D
        LD      A,(FORCLR)
J653D:  AND     0FH     ; 15 
        OR      D
        OUT     (C),A
        INC     HL
        DJNZ    J6522
        POP     HL
        DEC     E
        JR      NZ,J651F
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C654A:  LD      A,(BAKCLR)
        JR      J6552

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C654F:  LD      A,(FORCLR)
J6552:  PUSH    HL
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      HL,BAKCLR
        OR      (HL)
        POP     HL
        RET
J655D:  CP      02H     ; 2 
        JP      Z,J662D
        LD      A,L
        DEC     A
        ADD     A,A
        LD      L,A
        LD      A,H
        DEC     A
        ADD     A,A
        BIT     6,(IX+0)
        JR      NZ,J65AE
        ADD     A,A
        ADD     A,A
        LD      H,L
        LD      L,A
        LD      A,(VDP_DR)
        LD      C,A
        INC     C
        BIT     0,E
        JR      NZ,J659F
        BIT     1,E
        LD      B,08H   ; 8 
        JR      Z,J6584
        LD      B,10H   ; 16 
J6584:  PUSH    BC
        PUSH    HL
        CALL    C658C
        POP     HL
        POP     BC
        INC     H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C658C:  CALL    C67D1
        DEC     C
        IN      A,(C)
        INC     C
        CPL
        CALL    C67CF
        DEC     C
        OUT     (C),A
        INC     C
        INC     HL
        DJNZ    C658C
        RET
J659F:  INC     H
        LD      B,06H   ; 6 
        CALL    C65A8
        BIT     1,E
        RET     Z
;

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C65A8:  INC     L
        INC     L
        LD      B,06H   ; 6 
        JR      C658C
J65AE:  LD      B,A
        ADD     A,A
        ADD     A,B
        LD      H,L
        LD      L,A
        LD      A,(VDP_DR)
        LD      C,A
        INC     C
        BIT     1,E
        JR      Z,C65C5
        PUSH    HL
        CALL    C65C5
        POP     HL
        LD      A,06H   ; 6 
        ADD     A,L
        LD      L,A

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C65C5:  LD      A,L
        RES     1,L
        RES     2,L
        AND     07H     ; 7 
        LD      D,0FCH
        JR      Z,C65F8
        CP      02H     ; 2 
        JR      Z,J65F6
        CP      06H     ; 6 
        JR      Z,J65E7
        LD      D,0FH   ; 15 
        CALL    C65F8
        LD      A,E
        LD      DE,-248
        ADD     HL,DE
        LD      E,A
        LD      D,0C0H
        JR      C65F8
J65E7:  LD      D,03H   ; 3 
        CALL    C65F8
        LD      A,E
        LD      DE,-248
        ADD     HL,DE
        LD      E,A
        LD      D,0F0H
        JR      C65F8
J65F6:  LD      D,3FH   ; "?"

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C65F8:  BIT     0,E
        CALL    Z,C65FE
        INC     H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C65FE:  PUSH    HL
        LD      B,08H   ; 8 
        BIT     0,E
        JR      Z,J6609
        INC     HL
        INC     HL
        LD      B,06H   ; 6 
J6609:  CALL    C67D1
        DEC     C
        LD      HL,PATWRK
J6610:  IN      A,(C)
        XOR     D
        LD      (HL),A
        INC     HL
        DJNZ    J6610
        POP     HL
        PUSH    HL
        LD      B,08H   ; 8 
        BIT     0,E
        JR      Z,J6623
        INC     HL
        INC     HL
        LD      B,06H   ; 6 
J6623:  INC     C
        CALL    C67CF
        LD      HL,PATWRK
        JP      J67F7
J662D:  LD      A,L
        DEC     A
        ADD     A,A
        LD      L,A
        LD      A,H
        DEC     A
        CALL    C679F
        LD      H,L
        LD      L,A
        LD      A,(VDP_DR)
        LD      C,A
        INC     C
        LD      A,(FORCLR)
        LD      D,A
        LD      A,(BAKCLR)
        XOR     D
        LD      D,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        OR      D
        LD      D,A
        BIT     0,E
        JR      NZ,J667E
        BIT     1,E
        LD      B,20H   ; " "
        LD      E,18H
        JR      Z,J665C
        LD      B,40H   ; "@"
        LD      E,30H   ; "0"
J665C:  BIT     6,(IX+0)
        JR      Z,J6663
        LD      B,E
J6663:  PUSH    BC
        PUSH    HL
        CALL    C666B
        POP     HL
        POP     BC
        INC     H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C666B:  CALL    C67D1
        DEC     C
        IN      A,(C)
        INC     C
        XOR     D
        CALL    C67CF
        DEC     C
        OUT     (C),A
        INC     C
        INC     HL
        DJNZ    C666B
        RET
J667E:  INC     H
        PUSH    DE
        CALL    C6687
        POP     DE
        BIT     1,E
        RET     Z
;

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6687:  LD      E,04H   ; 4 
        BIT     6,(IX+0)
        JR      Z,J6690
        DEC     E
J6690:  INC     L
        INC     L
        LD      B,06H   ; 6 
        CALL    C666B
        DEC     E
        JR      NZ,J6690
        RET
J669B:  PUSH    AF
        LD      A,D
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      E,A
        LD      A,L
        ADD     A,A
        LD      H,A
        LD      L,00H
        LD      A,(VDP_DR)
        LD      C,A
        INC     C
        POP     AF
        CP      02H     ; 2 
        JR      Z,C66BA
        PUSH    DE
        PUSH    HL
        SET     5,H
        CALL    C66BA
        POP     HL
        POP     DE

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C66BA:  CALL    C67DC
        DEC     H
        DEC     H
        CALL    C67EE
        INC     H
        INC     H
        LD      A,L
        ADD     A,20H   ; " "
        LD      L,A
        JR      NC,J66CB
        INC     H
J66CB:  DEC     E
        JR      NZ,C66BA
        RET
J66CF:  PUSH    AF
        LD      A,D
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      E,A
        LD      A,L
        DEC     A
        ADD     A,A
        DEC     A
        LD      H,A
        LD      L,0E0H
        LD      A,(VDP_DR)
        LD      C,A
        INC     C
        POP     AF
        CP      02H     ; 2 
        JR      Z,C66F0
        PUSH    DE
        PUSH    HL
        SET     5,H
        CALL    C66F0
        POP     HL
        POP     DE

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C66F0:  CALL    C67DC
        INC     H
        INC     H
        CALL    C67EE
        DEC     H
        DEC     H
        LD      A,L
        SUB     20H     ; " "
        LD      L,A
        JR      NC,J6701
        DEC     H
J6701:  DEC     E
        JR      NZ,C66F0
        RET
J6705:  LD      BC,(VDP_DR)
        INC     C
        CP      02H     ; 2 
        JR      Z,J674B
        LD      A,L
        DEC     A
        ADD     A,A
        LD      L,A
        LD      A,H
        DEC     A
        ADD     A,A
        BIT     6,(IX+0)
        JR      Z,J6734
        LD      B,A
        ADD     A,A
        ADD     A,B
        LD      H,L
        LD      L,A
        AND     07H     ; 7 
        JR      Z,J6739
        RES     1,L
        RES     2,L
        PUSH    HL
        CALL    C676C
        INC     C
        POP     HL
        LD      A,L
        ADD     A,08H   ; 8 
        LD      L,A
        JR      J6739
J6734:  ADD     A,A
        ADD     A,A
        RET     C
        LD      H,L
        LD      L,A
J6739:  LD      A,L
        NEG
        LD      B,A
        PUSH    BC
        XOR     A
        CALL    C675D
        POP     BC
        DEC     H
        SET     5,H
J6746:  CALL    C654F
        JR      C675D
J674B:  LD      A,L
        DEC     A
        ADD     A,A
        LD      L,A
        LD      A,H
        DEC     A
        CALL    C679F
        RET     C
        LD      H,L
        LD      L,A
        NEG
        LD      B,A
        CALL    C654A
;

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C675D:  PUSH    BC
        CALL    C6763
        POP     BC
        INC     H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6763:  CALL    C67CF
        DEC     C
J6767:  OUT     (C),A
        DJNZ    J6767
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C676C:  LD      D,0C0H
        CP      02H     ; 2 
        JR      Z,J677A
        LD      D,0FCH
        CP      06H     ; 6 
        JR      Z,J677A
        LD      D,0F0H
J677A:  CALL    C6787
        INC     H
        CALL    C6787
        DEC     H
        SET     5,H
        JP      J6487

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6787:  PUSH    HL
        LD      B,08H   ; 8 
        CALL    C67D1
        DEC     C
        LD      HL,PATWRK
J6791:  IN      A,(C)
        AND     D
        LD      (HL),A
        INC     HL
        DJNZ    J6791
        POP     HL
        PUSH    HL
        LD      B,08H   ; 8 
        JP      J6623

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C679F:  ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      B,A
        ADD     A,A
        BIT     6,(IX+0)
        JR      NZ,J67AB
        LD      B,A
J67AB:  ADD     A,B
        RET
J67AD:  CP      02H     ; 2 
        JR      Z,J67C3
        XOR     A
        LD      L,A
J67B3:  LD      H,A
        CALL    C67BD
        CALL    C654F
        LD      HL,2000H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C67BD:  LD      BC,1800H
        JP      FILVRM

J67C3:  CALL    C654A
        LD      HL,0
        LD      BC,0800H
        JP      FILVRM


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C67CF:  SET     6,H


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C67D1:  DI
        OUT     (C),L
        EI
        OUT     (C),H
        EX      (SP),HL
        EX      (SP),HL
        RES     6,H
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C67DC:  PUSH    HL
        CALL    C67D1
        LD      HL,HOLD8
        LD      B,20H   ; " "
        DEC     C
J67E6:  INI
        JP      NZ,J67E6
        INC     C
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C67EE:  PUSH    HL
        CALL    C67CF
        LD      HL,HOLD8
        LD      B,20H   ; " "
J67F7:  DEC     C
J67F8:  OUTI
        JP      NZ,J67F8
        INC     C
        POP     HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6800:  LD      A,(HOKVLD)
        RRCA
        CCF                             ; EXTBIO valid ?
        RET     C                       ; nope, quit
        LD      HL,-64
        ADD     HL,SP
        LD      A,H
        CP      0C0H			; (rem: HIGH 0C000H) stack in page 3 ?
        RET     C                       ; nope, quit
        LD      SP,HL
        PUSH    HL                      ; workspace on stack
        IN      A,(0A8H)
        AND     0C0H
        RLCA
        RLCA
        LD      HL,EXPTBL
        LD      C,A
        LD      B,0
        ADD     HL,BC
        LD      A,(HL)
        AND     80H
        JR      Z,J682E
        INC     HL
        INC     HL
        INC     HL
        INC     HL
        LD      A,(HL)
        AND     0C0H
        RRCA
        RRCA
        RRCA
        SCF
        RRA
J682E:  OR      C
        LD      B,A
        LD      DE,1000H
        POP     HL
        PUSH    HL
        CALL    EXTBIO                  ; MSX-JE function 0 ()
        POP     DE
        OR      A
        SBC     HL,DE                   ; found MSX-JE ?
        JR      Z,J684E                ; nope, quit
        SRA     L
        SRA     L
        LD      B,L                     ; number of MSX-JE
J6843:  LD      A,(DE)
        INC     DE
        AND     07H                     ; MSX-JE with virtual terminal input interface AND dictonary interface
        JR      Z,J6851                ; yep,
        INC     DE
        INC     DE
        INC     DE
        DJNZ    J6843                  ; next MSX-JE
J684E:  SCF                             ; no MSX-JE found
        JR      J6858

J6851:  EX      DE,HL
        LD      A,(HL)                  ; slotid MSX-JE
        INC     HL
        LD      E,(HL)
        INC     HL
        LD      D,(HL)                  ; jump table MSX-JE
        OR      A                       ; MSX-JE found
J6858:  PUSH    AF
        LD      HL,64+2
        ADD     HL,SP
        POP     AF
        LD      SP,HL
        RET     C                       ; no MSX-JE, quit
        PUSH    AF
        PUSH    DE
        PUSH    AF
        POP     IY
        PUSH    DE
        POP     IX
        LD      A,1
        CALL    CALSLT                  ; MSX-JE INQUIRY
        PUSH    BC
        LD      HL,353
        ADD     HL,BC
        EX      DE,HL                   ; MIN2 + 353
        LD      HL,(HIMEM)
        AND     A
        SBC     HL,DE
        LD      (HIMEM),HL              ; allocate memory
        LD      A,(H_PHYD+0)
        CP      0C9H                    ; disk environment ?
        JR      Z,J6886
        LD      (HIMSAV),HL             ; yep, adjust disksystem bottom (lowest address used by the disksystem)
J6886:  POP     BC                      ; MSX-JE MIN2
        POP     DE                      ; MSX-JE jumptable
        POP     AF                      ; MSX-JE slotid
        POP     IX
        LD      SP,HL
        PUSH    IX
        PUSH    BC
        PUSH    AF
        PUSH    DE
        CALL    C41F9                  ; get KANJI workspace
        LD      (IX+4),L
        LD      (IX+5),H                ; workarea
        PUSH    HL
        POP     IY
        LD      (IY+0),21H
        LD      (IY+3),0F7H
        LD      (IY+7),0C9H
        CALL    C69EA
        XOR     A
        LD      (IY+8),A
        LD      (IY+9),A
        LD      (IY+10),A
        LD      DE,353
        ADD     HL,DE
        LD      (IY+1),L
        LD      (IY+2),H                ; MSX-JE workarea
        POP     DE
        POP     AF
        LD      (IY+4),A                ; slotid MSX-JE
        LD      (IY+5),E
        LD      (IY+6),D                ; MSX-JE entry
        POP     DE
        LD      (IY+15),E
        LD      (IY+16),D               ; MSX-JE MIN2
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C68D3:  CALL    C68E2
        DI
        LDIR
        EI
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C68DB:  CALL    C68E2
        EX      DE,HL
        LDIR
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C68E2:  PUSH    IY
        POP     HL
        LD      DE,17
        ADD     HL,DE
        LD      DE,FNKSTR
        LD      BC,80
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C68F0:  CALL    C48FA
        CALL    C68FC
        PUSH    AF
        CALL    C4911
        POP     AF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C68FC:  CALL    C7E50
        JP      Z,J7476                ; No MSX-JE,
        LD      L,(IY+9)
        LD      H,(IY+10)
        LD      A,L
        OR      H
        JP      NZ,J6994
J690D:  BIT     1,(IY+8)
        JR      NZ,J6965
        CALL    C69AE
        JR      C,J694C
        BIT     0,(IY+8)
        JR      NZ,J6931
        LD      A,3
        CALL    C6A74                   ; MSX-JE release
        CALL    C68D3
        CALL    C6A27
        CALL    NC,C6B21
J692C:  CALL    C6A36
        JR      J690D
J6931:  CALL    C68DB
        LD      E,(IY+15)
        LD      D,(IY+16)
        LD      A,2
        CALL    C6A74                   ; MSX-JE invoke
        LD      A,4
        CALL    C6A74                   ; MSX-JE clear
        CALL    C6A43
        CALL    C6ACD
        JR      J692C
J694C:  BIT     0,(IY+8)
        JR      Z,J69AB
        CP      01H     ; 1 
        JR      Z,J695E
        CP      20H     ; " "
        JR      C,J69AB
        CP      7FH
        JR      Z,J69AB
J695E:  CALL    C6B94
        SET     1,(IY+8)
J6965:  LD      A,6
        CALL    C6A74                   ; MSX-JE dispatch
        BIT     2,A
        JR      Z,J6975
        CALL    C69EA
        RES     1,(IY+8)
J6975:  RRCA
        PUSH    AF
        CALL    C,C6AA2
        POP     AF
        RRCA
        JP      NC,J690D
        LD      A,7
        CALL    C6A74                   ; MSX-JE get result
        LD      A,(HL)
        AND     A
        JP      Z,J690D
        LD      A,(GETPNT)
        LD      (IY+14),A
        LD      A,80H
        LD      (GETPNT),A
J6994:  LD      B,(HL)
        INC     HL
        LD      A,(HL)
        AND     A
        JR      NZ,J69A3
        LD      A,(IY+14)
        LD      (GETPNT),A
        LD      HL,0
J69A3:  LD      (IY+9),L
        LD      (IY+10),H
        LD      A,B
        RET

J69AB:  JP      CHGET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C69AE:  PUSH    IY
        PUSH    IX
        CALL    CHSNS
        POP     IX
        POP     IY
        JR      Z,C69AE
        LD      HL,(GETPNT)
        LD      A,(HL)
        CP      20H     ; " "
        JR      NZ,J69CE
        LD      A,06H   ; 6 
        CALL    SNSMAT
        AND     02H     ; 2 
        JR      Z,J69DD
        LD      A,20H   ; " "
J69CE:  CP      18H
        SCF
        RET     NZ
        LD      A,06H   ; 6 
        CALL    SNSMAT
        AND     04H     ; 4 
        LD      A,18H
        SCF
        RET     NZ
J69DD:  CALL    KILBUF
        LD      A,(IY+8)
        XOR     01H     ; 1 
        AND     0FDH
        LD      (IY+8),A


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C69EA:  PUSH    HL
        PUSH    AF
        PUSH    IY
        POP     HL
        LD      BC,97
        ADD     HL,BC
        EX      DE,HL
        PUSH    DE
        LD      HL,NORUSE
        DI
        RES     5,(HL)
        LD      HL,(GETPNT)
        LD      B,0
J6A00:  LD      A,(PUTPNT)
        CP      L
        JR      Z,J6A0F
        LD      A,(HL)
        LD      (DE),A
        CALL    C58D3
        INC     DE
        INC     B
        JR      J6A00
J6A0F:  POP     DE
        LD      A,B
        AND     A
        JR      Z,J6A23
        LD      HL,(GETPNT)
        LD      (PUTPNT),HL
J6A1A:  LD      A,(DE)
        PUSH    BC
        CALL    C58B4
        POP     BC
        INC     DE
        DJNZ    J6A1A
J6A23:  EI
        POP     AF
        POP     HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6A27:  CALL    C6B84
        RET     NC
        BIT     2,(IY+8)
        JR      NZ,C6A36
        XOR     A
        LD      (CNSDFG),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6A36:  PUSH    IY
        PUSH    IX
        CALL    C4B10
        POP     IX
        POP     IY
        SCF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6A43:  SET     2,(IY+8)
        LD      A,(CNSDFG)
        AND     A
        RET     NZ
        CALL    C6B84
        RET     NC
        RES     2,(IY+8)
        LD      A,(CSRY)
        LD      B,A
        LD      A,(CRTCNT)
        CP      B
        JR      NZ,J6A6E
        LD      L,01H   ; 1 
        PUSH    IY
        CALL    C49F5
        POP     IY
        LD      A,(CSRY)
        DEC     A
        LD      (CSRY),A
J6A6E:  LD      A,0FFH
        LD      (CNSDFG),A
        RET

;         Subroutine execute MSX-JE function
;            Inputs  A = functionnumber
;            Outputs ________________________

C6A74:  PUSH    IY
        PUSH    IX
        CALL    C6A81
        EI
        POP     IX
        POP     IY
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6A81:  EXX
        EX      AF,AF'
        LD      HL,0
        ADD     HL,SP
        LD      A,H
        CP      0C1H			; (rem: HIGH 0C100H)
        JR      NC,C6A9E
        EX      DE,HL
        PUSH    IY
        POP     HL
        LD      BC,353
        ADD     HL,BC
        LD      SP,HL
        PUSH    DE
        CALL    C6A9E
        EXX
        POP     HL
        LD      SP,HL
        EXX
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6A9E:  EX      AF,AF'
        EXX
        JP      (IY)


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6AA2:  LD      A,(IX+2)
        PUSH    AF
        LD      (IX+2),00H
        CALL    C6AB2
        POP     AF
        LD      (IX+2),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6AB2:  CALL    C6AD0
J6AB5:  LD      A,(HL)
        INC     HL
        AND     A
        JR      Z,C6AD0
        CALL    C6ADF
        JR      J6AB5

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6ABF:  CALL    C7E50
        BIT     0,(IY+8)
        RET     Z
        CALL    C7E50
        JP      Z,C7753                ; No MSX-JE,
;

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6ACD:  CALL    C6B21
;

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6AD0:  PUSH    HL
        CALL    C6B6E
        LD      E,00H
        PUSH    IY
        CALL    C5CED
        POP     IY
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6ADF:  CP      0CH     ; 12 
        JR      Z,C6B21
        CP      05H     ; 5 
        JR      Z,J6B25
        CP      08H     ; 8 
        JR      Z,J6B32
        CP      12H     ; 18 
        JR      Z,J6B44
        CP      18H
        JR      Z,J6B47
        CP      1AH
        JR      Z,J6B4D
        CP      10H     ; 16 
        RET     C
        CP      20H     ; " "
        INC     HL
        RET     C
        DEC     HL
        PUSH    HL
        LD      HL,(FORCLR)
        PUSH    HL
        LD      L,(IY+12)
        LD      H,(IY+13)
        LD      (FORCLR),HL
        LD      C,A
        CALL    C6B6E
        PUSH    IY
        CALL    C58DC
        POP     IY
        INC     (IY+11)
        POP     HL
        LD      (FORCLR),HL
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6B21:  LD      (IY+11),01H     ; 1 
J6B25:  PUSH    HL
        CALL    C6B6E
        PUSH    IY
        CALL    C600F
        POP     IY
        POP     HL
        RET
J6B32:  PUSH    HL
        DEC     (IY+11)
        CALL    C6B6E
        LD      C,20H   ; " "
        PUSH    IY
        CALL    C58DC
        POP     IY
        POP     HL
        RET
J6B44:  LD      A,(HL)
        INC     HL
        RET
J6B47:  LD      A,(HL)
        INC     HL
        LD      (IY+11),A
        RET
J6B4D:  LD      A,(IX+0)
        AND     07H     ; 7 
        CP      07H     ; 7 
        LD      A,(HL)
        JR      Z,J6B65
        AND     0FH     ; 15 
        LD      (IY+13),A
        LD      A,(HL)
        RRCA
        RRCA
        RRCA
        RRCA
        AND     0FH     ; 15 
        JR      J6B69
J6B65:  LD      (IY+13),A
        CPL
J6B69:  LD      (IY+12),A
        INC     HL
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6B6E:  LD      HL,(CRTCNT)
        LD      H,(IY+11)
        CALL    C6B84
        RET     C
        LD      L,19H
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6B7B:  CALL    C7E50
        BIT     0,(IY+8)
        SCF
        RET     Z


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6B84:  LD      A,(IX+0)
        AND     07H     ; 7 
        CP      04H     ; 4 
        RET     C
        LD      A,(IX+0)
        BIT     7,A
        RET     NZ
        SCF
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6B94:  PUSH    IY
        POP     HL
        LD      BC,97
        ADD     HL,BC
        EX      DE,HL
        PUSH    DE
        LD      HL,NORUSE
        DI
        SET     5,(HL)
        LD      HL,(GETPNT)
        LD      B,00H
J6BA8:  LD      A,(PUTPNT)
        CP      L
        JR      Z,J6BB7
        LD      A,(HL)
        LD      (DE),A
        CALL    C58D3
        INC     DE
        INC     B
        JR      J6BA8
J6BB7:  POP     DE
        LD      A,B
        AND     A
        JR      Z,J6BED
        LD      HL,(GETPNT)
J6BBF:  LD      A,(DE)
        CALL    C4982
        JR      C,J6BE3
        DEC     B
        JR      Z,J6BEA
        PUSH    BC
        LD      B,A
        INC     DE
        LD      A,(DE)
        LD      C,A
        CALL    C621D
        CP      04H     ; 4 
        LD      A,C
        POP     BC
        JR      NZ,J6BE7
        PUSH    HL
        PUSH    BC
        LD      C,A
        LD      B,0
        LD      HL,I7372
        ADD     HL,BC
        ADD     HL,BC
        LD      A,(HL)
        POP     BC
        POP     HL
J6BE3:  LD      (HL),A
        CALL    C58D3
J6BE7:  INC     DE
        DJNZ    J6BBF
J6BEA:  LD      (PUTPNT),HL
J6BED:  EI
        RET

;         Subroutine LPTOUT hander
;            Inputs  ________________________
;            Outputs ________________________


J6BEF:  EI
        PUSH    HL
        LD      HL,RAWPRT
        INC     (HL)
        DEC     (HL)
        POP     HL
        RET     NZ
        PUSH    AF
        PUSH    AF
        CALL    C41F9                  ; get KANJI workspace
        LD      A,(IX+3)
        AND     A
        LD      (IX+3),00H
        JR      NZ,J6C29
        POP     AF
        CP      0DH     ; 13 
        JR      Z,J6C1B
        CP      20H     ; " "
        CCF
        JR      NC,J6C1E
        CALL    C4982
        JR      C,J6C23
        LD      (IX+3),A
        JR      J6C44
J6C1B:  CALL    C6C65
J6C1E:  CALL    NC,C6C7F
        JR      J6C44
J6C23:  PUSH    HL
        PUSH    BC
        LD      C,A
        XOR     A
        JR      J6C38
J6C29:  EX      (SP),HL
        PUSH    BC
        LD      B,A
        LD      C,H
        CALL    C621D
        LD      B,A
        LD      A,C
        ADD     A,20H   ; " "
        LD      C,A
        LD      A,B
        ADD     A,20H   ; " "
J6C38:  CALL    C6C4B
        CALL    NC,C6C7F
        LD      A,C
        CALL    NC,C6C7F
        POP     BC
        POP     HL
J6C44:  EX      (SP),HL
        LD      A,H
        POP     HL
        CALL    C4583                   ; take control from hook caller
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6C4B:  AND     A
        BIT     4,(IX+1)
        RET     NZ
        PUSH    BC
        LD      B,A
        LD      A,1BH
        CALL    C6C7F
        LD      A,4BH   ; "K"
        CALL    NC,C6C7F
        LD      A,B
        POP     BC
        RET     C
        SET     4,(IX+1)
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6C65:  AND     A
        BIT     4,(IX+1)
        RET     Z
        PUSH    BC
        LD      B,A
        LD      A,1BH
        CALL    C6C7F
        LD      A,48H   ; "H"
        CALL    NC,C6C7F
        LD      A,B
        POP     BC
        RET     C
        RES     4,(IX+1)
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6C7F:  PUSH    AF
        LD      A,0C9H
        LD      (H_LPTO+0),A
        POP     AF
        CALL    LPTOUT
        CALL    C,C6C92
        LD      A,0F7H
        LD      (H_LPTO+0),A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6C92:  BIT     4,(IX+1)
        SCF
        RET     Z
J6C98:  LD      IX,BREAKX
        CALL    C6CCA                  ; call bios routine
        JR      C,J6C98
        LD      IX,LPTSTT
        CALL    C6CCA                  ; call bios routine
        SCF
        RET     Z
        LD      A,0DH   ; 13 
        CALL    C6CC6
        EI
        JR      C,J6C98
        LD      A,1BH
        CALL    C6CC6
        JR      C,J6C98
        LD      A,48H   ; "H"
        CALL    C6CC6
        JR      C,J6C98
        RES     4,(IX+1)
        SCF
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6CC6:  LD      IX,LPTOUT


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C6CCA:  JP      C4561                  ; call bios routine

;         Subroutine _AKCNV
;            Inputs  ________________________
;            Outputs ________________________


C6CCD:  CALL    C7126
        PUSH    DE
        CALL    C7174
        PUSH    DE
        LD      C,00H
        LD      A,B
        AND     A
        JR      Z,J6D17
        LD      DE,BUF
        PUSH    HL
        POP     IX
J6CE1:  LD      A,(IX+0)
        INC     IX
        CALL    C4982
        JP      NC,J6D01
        CALL    C6D23
        JR      C,J6D0F
        PUSH    BC
        LD      B,H
        LD      C,L
        CALL    C7104
        LD      H,B
        LD      L,C
        POP     BC
        LD      A,H
        LD      (DE),A
        INC     DE
        INC     C
        LD      A,L
        JR      J6D0F
J6D01:  LD      (DE),A
        INC     DE
        INC     C
        JP      Z,J712D
        DEC     B
        JR      Z,J6D17
        LD      A,(IX+0)
        INC     IX
J6D0F:  LD      (DE),A
        INC     DE
        INC     C
        JP      Z,J712D
        DJNZ    J6CE1
J6D17:  POP     HL
        EX      (SP),HL
        LD      A,C
        CALL    C7188
        POP     HL
        CALL    C7152
        DEFB    ")"
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6D23:  CP      0A6H
        JR      C,C6D7F
        CP      0B0H
        JR      Z,C6D7F
        CP      0DEH
        JR      NC,C6D7F
        CP      0DCH
        LD      HL,256FH
        RET     Z
        CP      0B3H
        JR      NZ,J6D4E
        LD      A,B
        DEC     A
        LD      HL,2526H
        RET     Z
        LD      A,(IX+0)
        CP      0DEH
        SCF
        CCF
        RET     NZ
        INC     IX
        DEC     B
        LD      HL,2574H
        RET
J6D4E:  PUSH    DE
        LD      E,20H   ; " "
        LD      HL,I7372
J6D54:  INC     E
        INC     HL
        INC     HL
        CP      (HL)
        JR      NZ,J6D54
        PUSH    DE
        LD      A,B
        DEC     A
        JR      Z,J6D79
        LD      A,(IX+0)
        CP      0DEH
        JR      Z,J6D6D
        CP      0DFH
        JR      NZ,J6D79
        INC     E
        INC     HL
        INC     HL
J6D6D:  INC     E
        INC     HL
        INC     HL
        INC     HL
        CP      (HL)
        JR      NZ,J6D79
        POP     AF
        DEC     B
        INC     IX
        PUSH    DE
J6D79:  POP     HL
        LD      H,25H   ; "%"
        POP     DE
        AND     A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C6D7F:  CP      30H     ; "0"
        JR      C,J6D87
        CP      3AH     ; ":"
        JR      C,J6D97
J6D87:  CP      41H     ; "A"
        JR      C,J6D8F
        CP      5BH     ; "["
        JR      C,J6D97
J6D8F:  CP      61H     ; "a"
        JR      C,J6D9C
        CP      7BH     ; "{"
        JR      NC,J6D9C
J6D97:  LD      H,23H   ; "#"
        LD      L,A
        AND     A
        RET
J6D9C:  PUSH    BC
        LD      HL,I744A+42-1
        LD      BC,42
        CPDR
        LD      L,C
        LD      H,B
        POP     BC
        SCF
        RET     NZ
        PUSH    BC
        LD      BC,I7420
        ADD     HL,BC
        POP     BC
        LD      L,(HL)
        LD      H,21H   ; "!"
        RET

;         Subroutine _KACNV
;            Inputs  ________________________
;            Outputs ________________________


C6DB4:  CALL    C7126
        PUSH    DE
        CALL    C7174
        PUSH    DE
        LD      C,00H
        LD      A,B
        AND     A
        JP      Z,J6E40
        LD      DE,BUF
        PUSH    HL
        POP     IX
J6DC9:  LD      A,(IX+0)
        INC     IX
        CALL    C4982
        JR      C,J6E3B
        LD      H,A
        LD      A,B
        DEC     A
        LD      A,H
        JR      Z,J6E3B
        LD      L,(IX+0)
        INC     IX
        DEC     B
        PUSH    BC
        LD      B,H
        LD      C,L
        CALL    C70F9
        LD      H,B
        LD      L,C
        POP     BC
        LD      A,H
        CP      21H     ; "!"
        JR      NZ,J6E07
        LD      A,L
        PUSH    HL
        PUSH    BC
        LD      HL,I7420+42-1
        LD      BC,42
        CPDR
        JR      NZ,J6E03
        LD      HL,I744A
        ADD     HL,BC
        LD      A,(HL)
        POP     BC
        POP     HL
        JR      J6E0C
J6E03:  POP     BC
        POP     HL
        JR      J6E2D
J6E07:  CP      23H     ; "#"
        JR      NZ,J6E11
        LD      A,L
J6E0C:  LD      HL,I6E1A
        JR      J6E23
J6E11:  CP      24H     ; "$"
        JR      Z,J6E19
        CP      25H     ; "%"
        JR      NZ,J6E2D
J6E19:  PUSH    BC
I6E1A:  LD      H,0
        ADD     HL,HL
        LD      BC,I7332
        ADD     HL,BC
        POP     BC
        LD      A,(HL)
J6E23:  LD      (DE),A
        INC     DE
        INC     C
        INC     HL
        LD      A,(HL)
        AND     A
        JR      NZ,J6E3B
        JR      J6E3E
J6E2D:  PUSH    BC
        LD      B,H
        LD      C,L
        CALL    C7104
        LD      H,B
        LD      L,C
        POP     BC
        LD      A,H
        LD      (DE),A
        INC     DE
        INC     C
        LD      A,L
J6E3B:  LD      (DE),A
        INC     DE
        INC     C
J6E3E:  DJNZ    J6DC9
J6E40:  POP     HL
        EX      (SP),HL
        LD      A,C
        CALL    C7188
        POP     HL
        CALL    C7152
        DEFB    ")"
        RET

;         Subroutine _SJIS
;            Inputs  ________________________
;            Outputs ________________________


C6E4C:  CALL    C7126
        PUSH    DE
        CALL    C7174
        PUSH    DE
        LD      A,(HL)
        CALL    C4982
        JR      C,J6E71
        LD      A,B
        CP      02H     ; 2 
        JP      C,J7130
        SCF
        JR      J6E80

;         Subroutine _JIS
;            Inputs  ________________________
;            Outputs ________________________

C6E63:  CALL    C7126
        PUSH    DE
        CALL    C7174
        PUSH    DE
        LD      A,(HL)
        CALL    C4982
        JR      NC,J6E7A
J6E71:  DEC     B
        INC     B
        JP      Z,J7130
        LD      C,02H   ; 2 
        JR      J6E90
J6E7A:  LD      A,B
        CP      02H     ; 2 
        JP      C,J7130
J6E80:  LD      B,(HL)
        INC     HL
        LD      C,(HL)
        CALL    NC,C70F9
        LD      A,C
        CALL    C71AC
        LD      (BUF+2),HL
        LD      C,04H   ; 4 
        LD      A,B
J6E90:  CALL    C71AC
        LD      (BUF+0),HL
        POP     HL
        EX      (SP),HL
        LD      A,C
        CALL    C7188
        POP     HL
        CALL    C7152
        DEFB    ")"
        RET

;         Subroutine _KEXT
;            Inputs  ________________________
;            Outputs ________________________


C6EA2:  CALL    C7126
        PUSH    DE
        CALL    C7174
        PUSH    BC
        PUSH    HL
        EX      DE,HL
        CALL    C7152
        DEFB    ","
        CALL    C716B                  ; evaluate byte operand
        CP      02H     ; 2 
        JP      NC,J7130
        POP     IX
        POP     BC
        PUSH    HL
        LD      L,A
        LD      C,00H
        LD      A,B
        AND     A
        JR      Z,J6EEF
        LD      DE,BUF
J6EC6:  LD      A,(IX+0)
        INC     IX
        CALL    C4982
        BIT     0,L
        JR      Z,J6EE1
        JR      C,J6EED
        LD      (DE),A
        INC     DE
        INC     C
        DEC     B
        JR      Z,J6EEF
        LD      A,(IX+0)
        INC     IX
        JR      J6EEA
J6EE1:  JR      C,J6EEA
        DEC     B
        JR      Z,J6EEF
        INC     IX
        JR      J6EED
J6EEA:  LD      (DE),A
        INC     DE
        INC     C
J6EED:  DJNZ    J6EC6
J6EEF:  POP     HL
        EX      (SP),HL
        LD      A,C
        CALL    C7188
        POP     HL
        CALL    C7152
        DEFB    ")"
        RET

;         Subroutine _KMID
;            Inputs  ________________________
;            Outputs ________________________


C6EFB:  CALL    C7126
        PUSH    DE
        CALL    C7174
        PUSH    HL
        PUSH    BC
        EX      DE,HL
        CALL    C7152
        DEFB    ","
        CALL    C716B                  ; evaluate byte operand
        AND     A
        JP      Z,J7130
        PUSH    AF
        LD      A,(HL)
        CP      2CH     ; ","
        LD      E,0FFH
        JR      NZ,J6F1E
        CALL    C7159                  ; get next BASIC character
        CALL    C716B                  ; evaluate byte operand
J6F1E:  POP     AF
        LD      D,A
        POP     BC
        EX      (SP),HL
        LD      C,00H
        LD      A,B
        AND     A
        JR      Z,J6F5F
        LD      A,E
        AND     A
        JR      Z,J6F5F
J6F2C:  DEC     D
        JR      Z,J6F3E
        LD      A,(HL)
        INC     HL
        CALL    C4982
        JR      C,J6F3A
        DEC     B
        JR      Z,J6F5F
        INC     HL
J6F3A:  DJNZ    J6F2C
        JR      J6F5F
J6F3E:  LD      IX,BUF
J6F42:  LD      A,(HL)
        LD      (IX+0),A
        INC     HL
        INC     IX
        INC     C
        CALL    C4982
        JR      C,J6F5A
        DEC     B
        JR      Z,J6F5F
        LD      A,(HL)
        LD      (IX+0),A
        INC     HL
        INC     IX
        INC     C
J6F5A:  DEC     E
        JR      Z,J6F5F
        DJNZ    J6F42
J6F5F:  POP     HL
        EX      (SP),HL
        LD      A,C
        CALL    C7188
        POP     HL
        CALL    C7152
        DEFB    ")"
        RET

;         Subroutine _KNJ
;            Inputs  ________________________
;            Outputs ________________________


C6F6B:  CALL    C7126
        PUSH    DE
        CALL    C7174
        PUSH    DE
        LD      A,B
        CP      04H     ; 4 
        JP      NZ,J7130
        LD      D,(HL)
        INC     HL
        LD      E,(HL)
        INC     HL
        CALL    C71BF
        LD      B,A
        LD      D,(HL)
        INC     HL
        LD      E,(HL)
        CALL    C71BF
        LD      C,A
        BIT     7,B
        CALL    P,C7104
        LD      A,B
        LD      (BUF+0),A
        LD      A,C
        LD      (BUF+1),A
        POP     HL
        EX      (SP),HL
        LD      A,02H   ; 2 
        CALL    C7188
        POP     HL
        CALL    C7152
        DEFB    ")"
        RET

;         Subroutine _KINSTR
;            Inputs  ________________________
;            Outputs ________________________


C6FA2:  CALL    C7120
        PUSH    DE
        PUSH    AF
        CALL    C7165                  ; evaluate expression
        LD      A,(VALTYP)
        CP      03H     ; 3 
        LD      A,01H   ; 1 
        JR      Z,J6FCF
        INC     A
        PUSH    HL
        CALL    C715F                  ; convert DAC to new type
        LD      HL,(DAC+2)
        LD      A,H
        AND     A
        JP      NZ,J7130
        LD      A,L
        AND     A
        JP      Z,J7130
        POP     HL
        PUSH    AF
        CALL    C7152
        DEFB    ","
        CALL    C7165                  ; evaluate expression
        POP     AF
J6FCF:  PUSH    AF
        CALL    C7177
        PUSH    HL
        PUSH    BC
        EX      DE,HL
        CALL    C7152
        DEFB    ","
        CALL    C7174
        POP     IX
        POP     IY
        POP     AF
        PUSH    DE
        PUSH    HL
        PUSH    BC
        PUSH    IX
        POP     BC
        PUSH    IY
        POP     HL
        LD      D,A
        LD      C,00H
        LD      A,B
        AND     A
        JR      Z,J7038
J6FF2:  INC     C
        DEC     D
        JR      Z,J7005
        LD      A,(HL)
        INC     HL
        CALL    C4982
        JR      C,J7001
        DEC     B
        JR      Z,J7038
        INC     HL
J7001:  DJNZ    J6FF2
        JR      J7038
J7005:  POP     AF
        POP     DE
        PUSH    BC
        EX      (SP),HL
        LD      H,L
        EX      (SP),HL
        LD      C,A
J700C:  PUSH    BC
        PUSH    DE
        PUSH    HL
J700F:  LD      A,C
        AND     A
        JR      Z,J703E
        LD      A,B
        AND     A
        JR      Z,J7036
        LD      A,(DE)
        CP      (HL)
        JR      NZ,J7021
        INC     DE
        INC     HL
        DEC     B
        DEC     C
        JR      J700F
J7021:  POP     HL
        POP     DE
        POP     BC
        POP     AF
        INC     A
        PUSH    AF
        LD      A,(HL)
        INC     HL
        CALL    C4982
        JR      C,J7032
        DEC     B
        JR      Z,J7039
        INC     HL
J7032:  DJNZ    J700C
        JR      J7039
J7036:  POP     AF
        POP     AF
J7038:  POP     AF
J7039:  POP     AF
        XOR     A
        JP      J70BD
J703E:  POP     AF
        POP     AF
        POP     AF
        POP     AF
        JR      J70BD

;         Subroutine _KLEN
;            Inputs  ________________________
;            Outputs ________________________


C7044:  CALL    C7120
        PUSH    DE
        PUSH    AF
        CALL    C7174
        PUSH    HL
        PUSH    BC
        EX      DE,HL
        LD      A,(HL)
        CP      2CH     ; ","
        LD      A,00H
        JR      NZ,J705C
        CALL    C7159                  ; get next BASIC character
        CALL    C716B                  ; evaluate byte operand
J705C:  CP      03H     ; 3 
        JR      NC,J70B2
        POP     BC
        EX      (SP),HL
        LD      D,A
        LD      C,00H
        LD      E,00H
        LD      A,B
        AND     A
        JR      Z,J707A
J706B:  LD      A,(HL)
        INC     HL
        INC     C
        CALL    C4982
        JR      C,J7078
        DEC     B
        JR      Z,J707A
        INC     E
        INC     HL
J7078:  DJNZ    J706B
J707A:  DEC     D
        LD      A,C
        JP      M,J70BD
        LD      A,E
        JR      NZ,J70BD
        LD      A,C
        SUB     E
        JR      J70BD

;         Subroutine _KTYPE
;            Inputs  ________________________
;            Outputs ________________________


C7086:  CALL    C7120
        PUSH    DE
        PUSH    AF
        CALL    C7174
        LD      A,B
        AND     A
        JR      Z,J70B2
        PUSH    HL
        PUSH    BC
        EX      DE,HL
        CALL    C7152
        DEFB    ","
        CALL    C716B                  ; evaluate byte operand
        AND     A
        JR      Z,J70B2
        POP     BC
        EX      (SP),HL
        LD      C,A
J70A2:  DEC     C
        JR      Z,J70B5
        LD      A,(HL)
        INC     HL
        CALL    C4982
        JR      C,J70B0
        DEC     B
        JR      Z,J70B2
        INC     HL
J70B0:  DJNZ    J70A2
J70B2:  JP      J7130
J70B5:  LD      A,(HL)
        CALL    C4982
        CCF
        LD      A,00H
        RLA
J70BD:  LD      C,A
        POP     HL
        POP     AF
        EX      (SP),HL
        LD      (VALTYP),A
        LD      B,00H
        LD      (DAC+2),BC
        CALL    C70D3
        POP     HL
        CALL    C7152
        DEFB    ")"
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C70D3:  PUSH    HL
        LD      HL,VALTYP
        LD      A,(HL)
        LD      C,A
        LD      (HL),02H        ; 2 
        LD      HL,DAC+2
        CP      02H     ; 2 
        JR      Z,J70F3
        CP      04H     ; 4 
        JR      Z,J70EB
        CP      08H     ; 8 
        JP      NZ,J712A
J70EB:  PUSH    BC
        CALL    C715F                  ; convert DAC to new type
        POP     BC
        LD      HL,DAC
J70F3:  LD      B,00H
        POP     DE
        LDIR
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C70F9:  CALL    C621D
        ADD     A,20H   ; " "
        LD      B,A
        LD      A,C
        ADD     A,20H   ; " "
        LD      C,A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7104:  LD      A,B
        DEC     A
        SRL     A
        ADD     A,71H   ; "q"
        CP      0A0H
        JR      C,J7110
        ADD     A,40H   ; "@"
J7110:  BIT     0,B
        LD      B,A
        LD      A,C
        JR      NZ,J7118
        ADD     A,5EH   ; "^"
J7118:  ADD     A,1FH
        LD      C,A
        CP      7FH
        RET     C
J711E:  INC     C
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C7120:  CALL    C713C
        RET     NZ
        JR      J712A


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C7126:  CALL    C713C
        RET     Z

J712A:  LD      E,13
        DEFB    001H
J712D:  LD      E,15
        DEFB    001H
J7130:  LD      E,5
        DEFB    001H
J7133:  LD      E,2
        LD      IX,ERROR
        JP      C41FE                  ; BASIC error


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C713C:  CALL    C7152
        DEFB    "("
        LD      IX,PTRGET
        CALL    C41FE                  ; locate variable
        EI
        CALL    C7152
        DEFB    ","
        LD      A,(VALTYP)
        CP      3
        RET

;         Subroutine check if BASIC character
;            Inputs  ________________________
;            Outputs ________________________


C7152:  LD      A,(HL)
        EX      (SP),HL
        CP      (HL)
        INC     HL
        EX      (SP),HL
        JR      NZ,J7133

;         Subroutine get next BASIC character
;            Inputs  ________________________
;            Outputs ________________________


C7159:  LD      IX,CHRGTR
        JR      J716F

;         Subroutine convert DAC to new type
;            Inputs  ________________________
;            Outputs ________________________


C715F:  LD      IX,DOCNVF
        JR      J716F

;         Subroutine evaluate expression
;            Inputs  ________________________
;            Outputs ________________________


C7165:  LD      IX,FRMEVL
        JR      J716F

;         Subroutine evaluate byte operand
;            Inputs  ________________________
;            Outputs ________________________


C716B:  LD      IX,GETBYT
J716F:  CALL    C41FE
        EI
        RET


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C7174:  CALL    C7165                  ; evaluate expression


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________


C7177:  PUSH    HL
        LD      IX,FRESTR
        CALL    C41FE                  ; free temporary string with type check
        EI
        LD      B,(HL)
        INC     HL
        LD      A,(HL)
        INC     HL
        LD      H,(HL)
        LD      L,A
        POP     DE
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7188:  PUSH    HL
        PUSH    AF
        LD      IX,STRINI
        CALL    C41FE                  ; allocate temporary string
        EI
        LD      DE,(DSCTMP+1)
        LD      HL,BUF
        POP     AF
        AND     A
        JR      Z,J71A2
        LD      C,A
        LD      B,00H
        LDIR
J71A2:  POP     DE
        LD      HL,DSCTMP
        LD      BC,3
        LDIR
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C71AC:  PUSH    AF
        RRCA
        RRCA
        RRCA
        RRCA
        CALL    C71B6
        LD      L,A
        POP     AF

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C71B6:  AND     0FH     ; 15 


;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C71B8:  CP      0AH     ; 10 
        SBC     A,69H   ; "i"
        DAA
        LD      H,A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C71BF:  LD      A,D
        CALL    C71CE
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        LD      D,A
        LD      A,E
        CALL    C71CE
        OR      D
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C71CE:  SUB     30H     ; "0"
        JR      C,J7231
        CP      0AH     ; 10 
        RET     C
        SUB     07H     ; 7 
        CP      0AH     ; 10 
        JR      C,J7231
        CP      10H     ; 16 
        RET     C
J71DE:  SUB     20H     ; " "
        CP      0AH     ; 10 
        JR      C,J7231
        CP      10H     ; 16 
        RET     C
        JR      J7231

;         Subroutine _PALETTE
;            Inputs  ________________________
;            Outputs ________________________


C71E9:  LD      A,(IDBYT2)
        AND     A                       ; MSX1 ?
        JR      Z,J7231                ; yep,
        DEC     HL
        CALL    C7159                  ; get next BASIC character
        JR      Z,J7234
        CALL    C7152
        DEFB    "("
        LD      A,00FH
        CALL    C7229
        PUSH    AF
        CALL    C7223
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        PUSH    AF
        CALL    C7223
        POP     BC
        LD      C,A
        PUSH    BC
        CALL    C7223
        POP     DE
        OR      D
        LD      D,A
        CALL    C7152
        DEFB    ")"
        POP     BC
        CALL    C7248
        OUT     (C),D
        EX      (SP),HL
        EX      (SP),HL
        EI
        OUT     (C),E
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7223:  CALL    C7152
        DEFB    ","
        LD      A,07H   ; 7 

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7229:  PUSH    AF
        CALL    C716B                  ; evaluate byte operand
        POP     AF
        CP      E
        LD      A,E
        RET     NC
J7231:  JP      J7130
J7234:  PUSH    HL
        LD      B,00H
        CALL    C7248
        LD      HL,I7254
        LD      B,32
J723F:  OUTI
        EX      (SP),HL
        EX      (SP),HL
        JR      NZ,J723F
        EI
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7248:  LD      C,10H   ; 16 
        CALL    WRTVDP
        LD      A,(VDP_DR)
        ADD     A,02H   ; 2 
        LD      C,A
        RET

I7254:	DEFB    00H,00H,00H,00H,11H,06H,33H,07H
        DEFB	17H,01H,27H,03H,51H,01H,27H,06H
	DEFB	71H,01H,73H,03H,61H,06H,64H,06H
	DEFB	11H,04H,65H,02H,55H,05H,77H,07H

I7274:	DEFB	00H,04H,08H,0CH,08H,0CH,08H,0CH
        DEFB	10H,14H,18H,1CH,18H,1CH,18H,1CH
	DEFB	20H,24H,28H,2CH,28H,2CH,28H,2CH
	DEFB	30H,34H,38H,3CH,38H,3CH,38H,3CH
	DEFB	40H,44H,48H,4CH,48H,4CH,48H,4CH
	DEFB	50H,54H,58H,5CH,58H,5CH,58H,5CH
	DEFB	60H,64H,68H,6CH,68H,6CH,68H,6CH
	DEFB	70H,74H,78H,7CH,78H,7CH,78H,7CH
	DEFB	40H,44H,48H,4CH,48H,4CH,48H,4CH
	DEFB	50H,54H,58H,5CH,58H,5CH,58H,5CH
	DEFB	60H,64H,68H,6CH,68H,6CH,68H,6CH
	DEFB	70H,74H,78H,7CH,78H,7CH,78H,7CH
	DEFB	40H,44H,48H,4CH,48H,4CH,48H,4CH
	DEFB	50H
        DEFB    54H,58H,5CH,58H,5CH,58H,5CH
	DEFB	60H,64H,68H,6CH,68H,6CH,68H,6CH
	DEFB	70H,74H,78H,7CH,78H,7CH,78H,7CH
	DEFB	80H,84H,88H,8CH,88H,8CH,88H,8CH
	DEFB	90H,94H,98H,9CH,98H,9CH,98H,9CH
	DEFB	0A0H,0A4H,0A8H,0ACH,0A8H,0ACH,0A8H,0ACH
	DEFB	0B0H,0B4H,0B8H,0BCH,0B8H,0BCH,0B8H,0BCH
	DEFB	0C0H,0C4H,0C8H,0CCH,0C8H,0CCH,0C8H,0CCH
	DEFB	0D0H,0D4H,0D8H,0DCH,0D8H,0DCH,0D8H,0DCH
	DEFB	0E0H,0E4H,0E8H,0ECH,0E8H,0ECH,0E8H,0ECH
	DEFB	0F0H,0F4H,0F8H,0FCH,0F8H,0FCH

I7332:  DEFB    0F8H,0FCH
	DEFB	0C0H,0C4H,0C8H,0CCH,0C8H,0CCH,0C8H,0CCH
	DEFB	0D0H,0D4H,0D8H,0DCH,0D8H,0DCH,0D8H,0DCH
	DEFB	0E0H,0E4H,0E8H,0ECH,0E8H,0ECH,0E8H,0ECH
	DEFB	0F0H,0F4H,0F8H,0FCH,0F8H,0FCH,0F8H,0FCH
	DEFB	0C0H,0C4H,0C8H,0CCH,0C8H,0CCH,0C8H,0CCH
	DEFB	0D0H,0D4H,0D8H,0DCH,0D8H,0DCH,0D8H,0DCH
	DEFB	0E0H,0E4H,0E8H,0ECH,0E8H,0ECH,0E8H,0ECH
	DEFB	0F0H,0F4H,0F8H,0FCH,0F8H,0FCH

I7372:  DEFB    0F8H,0FCH

I7374:	DEFB	0A7H,00H,0B1H,00H,0A8H,00H,0B2H,00H
	DEFB	0A9H,00H,0B3H,00H,0AAH,00H,0B4H,00H
	DEFB	0ABH,00H,0B5H,00H,0B6H,00H,0B6H,0DEH
	DEFB	0B7H,00H,0B7H,0DEH,0B8H,00H,0B8H,0DEH
	DEFB	0B9H,00H,0B9H,0DEH,0BAH,00H,0BAH,0DEH
	DEFB	0BBH,00H,0BBH,0DEH,0BCH,00H,0BCH,0DEH
	DEFB	0BDH,00H,0BDH,0DEH,0BEH,00H,0BEH,0DEH
	DEFB	0BFH,00H,0BFH,0DEH,0C0H,00H,0C0H,0DEH
	DEFB	0C1H,00H,0C1H,0DEH,0AFH,00H,0C2H,00H
	DEFB	0C2H,0DEH,0C3H,00H,0C3H,0DEH,0C4H,00H
	DEFB	0C4H,0DEH,0C5H,00H,0C6H,00H,0C7H,00H
	DEFB	0C8H,00H,0C9H,00H,0CAH,00H,0CAH,0DEH
	DEFB	0CAH,0DFH,0CBH,00H,0CBH,0DEH,0CBH,0DFH
	DEFB	0CCH,00H,0CCH,0DEH,0CCH,0DFH,0CDH,00H
	DEFB	0CDH,0DEH,0CDH,0DFH,0CEH,00H,0CEH,0DEH
	DEFB	0CEH,0DFH,0CFH,00H,0D0H,00H,0D1H,00H
	DEFB	0D2H,00H,0D3H,00H,0ACH,00H,0D4H,00H
	DEFB	0ADH,00H,0D5H,00H,0AEH,00H,0D6H,00H
	DEFB	0D7H,00H,0D8H,00H,0D9H,00H,0DAH,00H
	DEFB	0DBH,00H,0DCH,00H,0DCH,00H,0B2H,00H
	DEFB	0B4H,00H,0A6H,00H,0DDH,00H,0CDH,0DEH
	DEFB	0B6H,00H,0B9H
        DEFB    00H

I7420:  DEFB    21H,2AH,49H,74H
	DEFB	70H,73H,75H,47H,4AH,4BH,76H,5CH
	DEFB	24H,3DH,25H,3FH,27H,28H,63H,61H
	DEFB	64H,29H,77H,4EH,6FH,4FH,30H,32H
	DEFB	46H,50H,43H,51H,41H,23H,56H,57H
	DEFB	22H,26H,3CH,5DH,2BH
        DEFB    2CH

I744A:  DEFB    20H,21H
	DEFB	22H,23H,24H,25H,26H,27H,28H,29H
	DEFB	2AH,2BH,2CH,2DH,2EH,2FH,3AH,3BH
	DEFB	3CH,3DH,3EH,3FH,40H,5BH,5CH,5DH
	DEFB	5EH,5FH,60H,7BH,7CH,7DH,7EH,0A1H
	DEFB	0A2H,0A3H,0A4H,0A5H,0B0H,2DH,0DEH,0DFH

;         Subroutine initialize own MSX-JE
;            Inputs  ________________________
;            Outputs ________________________

C7474:  SCF                             ; does not have own MSX-JE
        RET
J7476:  LD      A,(IX+4)
        LD      (IX+4),00H
J747D:  AND     A
        JR      Z,J7489
        PUSH    AF
        LD      A,(IX+7)
        LD      (GETPNT),A
        POP     AF
        RET
J7489:  CALL    C69AE
        JR      C,J74A7
        BIT     0,(IY+8)
        JR      NZ,J749F
        CALL    C6A27
        LD      A,(BAKCLR)
        CALL    NC,C775C
        JR      J7489
J749F:  CALL    C6A43
J74A2:  CALL    C7753
        JR      J7489
J74A7:  BIT     0,(IY+8)
        JP      Z,C7642
        CALL    C7642
        CALL    C75C5
        JP      NC,J7561
J74B7:  CP      20H     ; " "
        RET     C
        CP      7FH
        RET     Z
        AND     A
        JP      P,J7541
        CALL    C4982
        JR      NC,J74DA
        CP      80H
        RET     Z
        CP      0A6H
        JR      C,J7541
        CP      0B0H
        JR      Z,J7541
        CP      0DEH
        JR      C,J7547
        CP      0E0H
        JR      C,J7541
        RET
J74DA:  PUSH    AF
        CALL    C7642
;

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C74DE:  CP      9FH
        JR      C,J74FD
        CP      0F2H
        JR      NC,J74FD
        LD      C,A
        POP     AF
        CP      82H
        JR      Z,J7505
        LD      (IX+4),C

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C74EF:  PUSH    AF
        LD      A,(GETPNT)
        LD      (IX+7),A
        LD      A,80H
        LD      (GETPNT),A
        POP     AF
        RET
J74FD:  LD      (IX+4),A
        CALL    C74EF
        POP     AF
        RET

J7505:  LD      A,C
        CALL    C762C
        CALL    C76C2
        CALL    C75F5
J750F:  LD      C,00H
        LD      B,C
J7512:  CP      03H     ; 3 
        JR      NZ,J751E
        LD      A,(INTFLG)
        CP      03H     ; 3 
        JP      Z,C7753
J751E:  CP      1BH
        JR      Z,J74A2
        CP      0DH     ; 13 
        JR      Z,J754E
        CALL    C7653
        AND     A
        JR      Z,J753C
        DEC     A
        PUSH    AF
        PUSH    BC
        LD      C,B
        CALL    Z,C76E2
        POP     BC
        LD      B,C
        POP     AF
        CALL    NZ,C7707
        CALL    C76E2
J753C:  CALL    C7642
        JR      J7512
J7541:  CALL    C6D7F
        RET     C
        JR      J7554
J7547:  CALL    C75B4
        LD      B,25H   ; "%"
        JR      J7556
J754E:  CALL    C76A9
        CALL    C7753
J7554:  LD      C,L
        LD      B,H
J7556:  CALL    C7104
        LD      (IX+4),C
        CALL    C74EF
        LD      A,B
        RET
J7561:  LD      HL,SWPTMP+0
        LD      (HL),A
        INC     HL
        LD      B,03H   ; 3 
J7568:  CALL    C7642
        CP      1BH
        JP      Z,J7489
        CALL    C75C5
        JP      C,J74B7
        LD      (HL),A
        INC     HL
        DJNZ    J7568
        LD      HL,SWPTMP+0
        LD      A,(HL)
        AND     7FH
        INC     HL
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        OR      (HL)
        INC     HL
        LD      B,A
        LD      A,(HL)
        AND     7FH
        INC     HL
        ADD     A,A
        ADD     A,A
        ADD     A,A
        ADD     A,A
        OR      (HL)
        LD      L,A
        LD      H,B
        CALL    C76C2
        CALL    C7707
        LD      C,00H
        CALL    C76E2
        CALL    C7642
        JP      J750F

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C75A4:  CP      86H
        RET     C
        CP      0A0H
        JR      C,J75B2
        CP      0E0H
        RET     C
        CP      0FEH
        CCF
        RET     C
J75B2:  XOR     20H     ; " "

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C75B4:  CP      0DCH
        LD      C,6FH   ; "o"
        RET     Z
        LD      C,20H   ; " "
        LD      HL,I7374
J75BE:  INC     C
        CP      (HL)
        INC     HL
        INC     HL
        JR      NZ,J75BE
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C75C5:  AND     A
        SCF
        RET     Z
        CP      07H     ; 7 
        JR      C,J75E3
        CP      30H     ; "0"
        RET     C
        CP      3AH     ; ":"
        CCF
        RET     C
        PUSH    BC
        LD      B,A
        LD      A,06H   ; 6 
        CALL    SNSMAT
        AND     02H     ; 2 
        LD      A,B
        POP     BC
        SCF
        RET     NZ
        SUB     30H     ; "0"
        RET
J75E3:  ADD     A,09H   ; 9 
        CP      0CH     ; 12 
        SCF
        CCF
        RET     NZ
        LD      A,(INTFLG)
        CP      03H     ; 3 
        SCF
        RET     Z
        LD      A,0CH   ; 12 
        AND     A
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C75F5:  PUSH    IY
        LD      IY,SWPTMP
        LD      C,04H   ; 4 
J75FD:  INC     C
        DEC     C
        JR      Z,J7618
        LD      (IY+0),A
        INC     IY
        LD      (IY+0),00H
        DEC     C
        CALL    C7787
        CALL    C7707
        PUSH    BC
        LD      C,00H
        CALL    C76E2
        POP     BC
J7618:  CALL    C7622
        CP      21H     ; "!"
        JR      NC,J75FD
        POP     IY
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7622:  CALL    C7642
        CALL    C4982
        RET     C
        CALL    C7642
;

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C762C:  PUSH    BC
        PUSH    HL
        LD      C,A
        LD      B,00H
        LD      HL,I7374-2*09FH
        ADD     HL,BC
        ADD     HL,BC
        LD      A,(HL)
        POP     HL
        POP     BC
        CP      0ACH
        RET     C
        CP      0AFH
        RET     NC
        ADD     A,28H   ; "("
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7642:  PUSH    IX
        PUSH    IY
J7646:  CALL    CHSNS
        JR      Z,J7646
        CALL    CHGET
        POP     IY
        POP     IX
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7653:  CP      20H     ; " "
        JR      Z,J766D
        CP      1CH
        JR      Z,J766D
        CP      1DH
        JR      Z,J767E
        CP      08H     ; 8 
        JR      Z,J767E
        CP      1EH
        JR      Z,J7684
        CP      1FH
        JR      Z,J7675
        XOR     A
        RET
J766D:  INC     C
        LD      A,C
        CP      E
        LD      A,01H   ; 1 
        RET     C
        LD      C,00H
J7675:  PUSH    BC
        LD      C,E
        CALL    C76A9
        POP     BC
        LD      A,02H   ; 2 
        RET
J767E:  DEC     C
        LD      A,01H   ; 1 
        RET     P
        LD      C,E
        DEC     C
J7684:  PUSH    BC
        LD      C,E
        CALL    C768D
        POP     BC
        LD      A,02H   ; 2 
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C768D:  LD      B,00H
        AND     A
        SBC     HL,BC
        LD      A,L
        SUB     21H     ; "!"
        RET     NC
        LD      L,7FH
        ADD     A,L
        LD      L,A
        DEC     H
        LD      A,H
        CP      2FH     ; "/"
        JR      NZ,J76A3
        LD      H,28H   ; "("
        RET
J76A3:  CP      20H     ; " "
        RET     NZ
        LD      H,74H   ; "t"
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C76A9:  LD      B,00H
        ADD     HL,BC
        LD      A,L
        SUB     7FH
        RET     C
        ADD     A,21H   ; "!"
        LD      L,A
        INC     H
        LD      A,H
        CP      29H     ; ")"
        JR      NZ,J76BC
        LD      H,30H   ; "0"
        RET
J76BC:  CP      75H     ; "u"
        RET     NZ
        LD      H,21H   ; "!"
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C76C2:  PUSH    AF
        LD      E,0FFH
        LD      A,(LINLEN)
J76C8:  INC     E
        SUB     03H     ; 3 
        JR      NC,J76C8
        LD      A,E
        CP      0BH     ; 11 
        LD      D,01H   ; 1 
        JR      C,J76E0
        LD      D,0FDH
        LD      A,(LINLEN)
J76D9:  INC     D
        SUB     0AH     ; 10 
        JR      NC,J76D9
        LD      E,0AH   ; 10 
J76E0:  POP     AF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C76E2:  PUSH    HL
        PUSH    DE
        PUSH    BC
        PUSH    IY
        PUSH    IX
        CALL    C6B6E
        LD      A,D
        ADD     A,02H   ; 2 
        LD      H,A
        XOR     A
        INC     C
J76F2:  DEC     C
        JR      Z,J76F8
        ADD     A,H
        JR      J76F2
J76F8:  LD      H,A
        INC     H
        LD      E,02H   ; 2 
        CALL    C5CED
        POP     IX
        POP     IY
        POP     BC
        POP     DE
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7707:  PUSH    HL
        PUSH    DE
        PUSH    BC
        PUSH    IY
        PUSH    IX
        LD      BC,(FORCLR)
        PUSH    BC
        LD      A,B
        LD      B,C
        LD      C,A
        LD      (FORCLR),BC
        LD      C,L
        LD      B,H
        CALL    C6B6E
        LD      H,01H   ; 1 
J7721:  PUSH    BC
        CALL    C7104
        LD      A,C
        LD      C,B
        CALL    C58DC
        INC     H
        LD      C,A
        CALL    C58DC
        INC     H
        LD      C,20H   ; " "
        PUSH    DE
J7733:  CALL    C58DC
        INC     H
        DEC     D
        JR      NZ,J7733
        POP     DE
        EX      (SP),HL
        LD      C,01H   ; 1 
        CALL    C76A9
        LD      C,L
        LD      B,H
        POP     HL
        DEC     E
        JR      NZ,J7721
        POP     HL
        LD      (FORCLR),HL
        POP     IX
        POP     IY
        POP     BC
        POP     DE
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7753:  PUSH    AF
        LD      A,(FORCLR)
        CALL    C775C
        POP     AF
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C775C:  PUSH    HL
        PUSH    IY
        PUSH    IX
        LD      L,A
        LD      A,(BAKCLR)
        PUSH    AF
        LD      A,L
        LD      (BAKCLR),A
        CALL    C6B6E
        LD      H,01H   ; 1 
        CALL    C600F
        POP     AF
        LD      (BAKCLR),A
        CALL    C6B6E
        LD      A,(LINLEN)
        INC     A
        LD      H,A
        CALL    C600F
        POP     IX
        POP     IY
        POP     HL
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7787:  PUSH    DE
        LD      HL,SWPTMP
        LD      DE,2121H
        PUSH    DE
        LD      DE,I77E1
J7792:  LD      A,(DE)
        AND     A
        JR      Z,J77AD
        CALL    C77B0
        JR      Z,J779D
        JR      NC,J77AD
J779D:  POP     AF
J779E:  INC     DE
        LD      A,(DE)
        ADD     A,A
        JR      C,J779E
        PUSH    HL
        EX      DE,HL
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        EX      DE,HL
        EX      (SP),HL
        JR      J7792
J77AD:  POP     HL
        POP     DE
        RET

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C77B0:  PUSH    DE
        PUSH    HL
        DEC     DE
        DEC     HL
J77B4:  INC     DE
        INC     HL
        LD      A,(DE)
        AND     A
        JP      P,J77DA
        CP      (HL)
        JR      Z,J77B4
        CP      0DEH
        JR      NZ,J77CB
        INC     DE
        CALL    C77B0
        POP     HL
        POP     DE
        RET     NZ
        INC     A
        RET

J77CB:  LD      A,(HL)
        CP      0DEH
        JR      NZ,J77DC
        INC     HL
        CALL    C77B0
        POP     HL
        POP     DE
        RET     NZ
        SUB     01H
        RET

J77DA:  XOR     A
        DEFB    020H
J77DC:  LD      A,(DE)
        SUB     (HL)
        POP     HL
        POP     DE
        RET

I77E1:	DEFB	0B1H,21H,30H,0B1H,0B2H,25H,30H,0B1H
	DEFB	0B5H,2AH,30H,0B1H,0B8H,2DH,30H,0B1H
	DEFB	0BBH,30H,30H,0B1H,0C2H,35H,30H,0B1H
	DEFB	0C3H,38H,30H,0B1H,0D4H,3CH,30H,0B1H
	DEFB	0D5H,3EH,30H,0B1H,0DDH,42H,30H,0B2H
	DEFB	4AH,30H,0B2H,0B7H,68H,30H,0B2H,0B8H
	DEFB	69H,30H,0B2H,0BFH,6BH,30H,0B2H,0C1H
	DEFB	6CH,30H,0B2H,0C2H,6EH,30H,0B2H,0C8H
	DEFB	70H,30H,0B2H,0DDH,74H,30H,0B3H,26H
	DEFB	31H,0B3H,0B6H,2EH,31H,0B3H,0BDH,30H
	DEFB	31H,0B3H,0BDH,0DEH,32H,31H,0B3H,0BFH
	DEFB	33H,31H,0B3H,0C0H,34H,31H,0B3H,0C2H
	DEFB	35H,31H,0B3H,0C5H,37H,31H,0B3H,0D7H
	DEFB	3AH,31H,0B3H,0D8H,3BH,31H,0B3H,0D9H
	DEFB	3CH,31H,0B3H,0DCH,3DH,31H,0B3H,0DDH
	DEFB	3EH,31H,0B4H,41H,31H,0B4H,0B2H,43H
	DEFB	31H,0B4H,0B7H,55H,31H,0B4H,0C2H,59H
	DEFB	31H,0B4H,0C9H,5DH,31H,0B4H,0DDH,5EH
	DEFB	31H,0B5H,77H,31H,0B5H,0B2H,79H,31H
	DEFB	0B5H,0B3H,7AH,31H,0B5H,0B6H,2CH,32H
	DEFB	0B5H,0B7H,2DH,32H,0B5H,0B7H,0DEH,2EH
	DEFB	32H,0B5H,0B8H,2FH,32H,0B5H,0B9H,33H
	DEFB	32H,0B5H,0BDH,34H,32H,0B5H,0C2H,35H
	DEFB	32H,0B5H,0DAH,36H,32H,0B5H,0DBH,37H
	DEFB	32H,0B5H,0DDH,38H,32H,0B6H,3CH,32H
	DEFB	0B6H,0DEH,64H,32H,0B6H,0B2H,70H,32H
	DEFB	0B6H,0DEH,0B2H,2EH,33H,0B6H,0B2H,0D8H
	DEFB	3DH,33H,0B6H,0B7H,40H,33H,0B6H,0B7H
	DEFB	0DEH,43H,33H,0B6H,0B8H,44H,33H,0B6H
	DEFB	0DEH,0B8H,58H,33H,0B6H,0B9H,5DH,33H
	DEFB	0B6H,0BBH,5EH,33H,0B6H,0BCH,5FH,33H
	DEFB	0B6H,0BCH,0DEH,61H,33H,0B6H,0C0H,63H
	DEFB	33H,0B6H,0C2H,64H,33H,0B6H,0C5H,70H
	DEFB	33H,0B6H,0CAH,0DEH,71H,33H,0B6H,0CCH
	DEFB	0DEH,74H,33H,0B6H,0CFH,76H,33H,0B6H
	DEFB	0D0H,7AH,33H,0B6H,0D3H,7BH,33H,0B6H
	DEFB	0D4H,7CH,33H,0B6H,0D5H,21H,34H,0B6H
	DEFB	0D8H,22H,34H,0B6H,0DDH,25H,34H,0B6H
	DEFB	0DEH,0DDH,5DH,34H,0B7H,6BH,34H,0B7H
	DEFB	0DEH,36H,35H,0B7H,0B8H,45H,35H,0B7H
	DEFB	0C1H,48H,35H,0B7H,0C2H,49H,35H,0B7H
	DEFB	0C7H,4EH,35H,0B7H,0D4H,51H,35H,0B7H
	DEFB	0DEH,0D4H,54H,35H,0B7H,0D5H,56H,35H
	DEFB	0B7H,0DEH,0D5H,6DH,35H,0B7H,0D6H,6EH
	DEFB	35H,0B7H,0DEH,0D6H,79H,35H,0B7H,0D6H
	DEFB	0B3H,7DH,35H,0B7H,0DEH,0D6H,0B3H,44H
	DEFB	36H,0B7H,0D6H,0B8H,49H,36H,0B7H,0DEH
	DEFB	0D6H,0B8H,4CH,36H,0B7H,0D8H,4DH,36H
	DEFB	0B7H,0DDH,4FH,36H,0B7H,0DEH,0DDH,63H
	DEFB	36H,0B8H,65H,36H,0B8H,0DEH,71H,36H
	DEFB	0B8H,0B2H,74H,36H,0B8H,0B3H,75H,36H
	DEFB	0B8H,0DEH,0B3H,76H,36H,0B8H,0BCH,7AH
	DEFB	36H,0B8H,0C2H,7EH,36H,0B8H,0CFH,27H
	DEFB	37H,0B8H,0D8H,2AH,37H,0B8H,0DDH,2EH
	DEFB	37H,0B8H,0DEH,0DDH,32H,37H,0B9H,35H
	DEFB	37H,0B9H,0B2H,38H,37H,0B9H,0DEH,0B2H
	DEFB	5DH,37H,0B9H,0DEH,0B7H,60H,37H,0B9H
	DEFB	0C0H,65H,37H,0B9H,0C2H,66H,37H,0B9H
	DEFB	0DEH,0C2H,6EH,37H,0B9H,0DDH,6FH,37H
	DEFB	0B9H,0DEH,0DDH,35H,38H,0BAH,43H,38H
	DEFB	0BAH,0DEH,5EH,38H,0BAH,0B2H,70H,38H
	DEFB	0BAH,0B3H,72H,38H,0BAH,0DEH,0B3H,64H
	DEFB	39H,0BAH,0B8H,6EH,39H,0BAH,0DEH,0B8H
	DEFB	76H,39H,0BAH,0BCH,77H,39H,0BAH,0C2H
	DEFB	7AH,39H,0BAH,0CFH,7DH,39H,0BAH,0DBH
	DEFB	22H,3AH,0BAH,0DDH,23H,3AH,0BBH,33H
	DEFB	3AH,0BBH,0DEH,41H,3AH,0BBH,0B2H,44H
	DEFB	3AH,0BBH,0DEH,0B2H,5EH,3AH,0BBH,0B4H
	DEFB	63H,3AH,0BBH,0B6H,64H,3AH,0BBH,0B7H
	DEFB	69H,3AH,0BBH,0B7H,0DEH,6DH,3AH,0BBH
	DEFB	0B8H,6EH,3AH,0BBH,0B9H,7AH,3AH,0BBH
	DEFB	0BBH,7BH,3AH,0BBH,0BCH,0DEH,7CH,3AH
	DEFB	0BBH,0C2H,7DH,3AH,0BBH,0DEH,0C2H,28H
	DEFB	3BH,0BBH,0CAH,0DEH,2AH,3BH,0BBH,0CBH
	DEFB	0DEH,2CH,3BH,0BBH,0D2H,2DH,3BH,0BBH
	DEFB	0D7H,2EH,3BH,0BBH,0DDH,30H,3BH,0BBH
	DEFB	0DEH,0DDH,42H,3BH,0BCH,45H,3BH,0BCH
	DEFB	0DEH,76H,3BH,0BCH,0B5H,2EH,3CH,0BCH
	DEFB	0B6H,2FH,3CH,0BCH,0B7H,30H,3CH,0BCH
	DEFB	0B7H,0DEH,32H,3CH,0BCH,0DEH,0B8H,33H
	DEFB	3CH,0BCH,0BCH,35H,3CH,0BCH,0C1H,37H
	DEFB	3CH,0BCH,0C2H,38H,3CH,0BCH,0DEH,0C2H
	DEFB	42H,3CH,0BCH,0C9H,44H,3CH,0BCH,0CAH
	DEFB	0DEH,46H,3CH,0BCH,0CFH,4AH,3CH,0BCH
	DEFB	0D4H,4BH,3CH,0BCH,0DEH,0D4H,58H,3CH
	DEFB	0BCH,0D4H,0B8H,5AH,3CH,0BCH,0DEH,0D4H
	DEFB	0B8H,63H,3CH,0BCH,0D5H,67H,3CH,0BCH
	DEFB	0DEH,0D5H,74H,3CH,0BCH,0D5H,0B3H,7CH
	DEFB	3CH,0BCH,0DEH,0D5H,0B3H,3AH,3DH,0BCH
	DEFB	0D5H,0B8H,47H,3DH,0BCH,0DEH,0D5H,0B8H
	DEFB	4EH,3DH,0BCH,0D5H,0C2H,50H,3DH,0BCH
	DEFB	0DEH,0D5H,0C2H,51H,3DH,0BCH,0D5H,0DDH
	DEFB	53H,3DH,0BCH,0DEH,0D5H,0DDH,5AH,3DH
	DEFB	0BCH,0D6H,68H,3DH,0BCH,0DEH,0D6H,75H
	DEFB	3DH,0BCH,0D6H,0B3H,7DH,3DH,0BCH,0DEH
	DEFB	0D6H,0B3H,65H,3EH,0BCH,0D6H,0B8H,7CH
	DEFB	3EH,0BCH,0DEH,0D6H,0B8H,2BH,3FH,0BCH
	DEFB	0D8H,2CH,3FH,0BCH,0DDH,2DH,3FH,0BCH
	DEFB	0DEH,0DDH,4DH,3FH,0BDH,5AH,3FH,0BDH
	DEFB	0DEH,5EH,3FH,0BDH,0B2H,61H,3FH,0BDH
	DEFB	0DEH,0B2H,6FH,3FH,0BDH,0B3H,72H,3FH
	DEFB	0BDH,0B7H,0DEH,79H,3FH,0BDH,0B9H,0DEH
	DEFB	7BH,3FH,0BDH,0BDH,0DEH,7DH,3FH,0BEH
	DEFB	24H,40H,0BEH,0B2H,28H,40H,0BEH,0DEH
	DEFB	0B2H,47H,40H,0BEH,0B7H,49H,40H,0BEH
	DEFB	0C2H,5AH,40H,0BEH,0DEH,0C2H,64H,40H
	DEFB	0BEH,0DDH,67H,40H,0BEH,0DEH,0DDH,30H
	DEFB	41H,0BEH,0DDH,0C1H,38H,41H,0BFH,39H
	DEFB	41H,0BFH,0B3H,4EH,41H,0BFH,0DEH,0B3H
	DEFB	7CH,41H,0BFH,0B8H,25H,42H,0BFH,0DEH
	DEFB	0B8H,2FH,42H,0BFH,0C2H,34H,42H,0BFH
	DEFB	0DDH,38H,42H,0C0H,3EH,42H,0C0H,0DEH
	DEFB	43H,42H,0C0H,0B2H,4EH,42H,0C0H,0DEH
	DEFB	0B2H,65H,42H,0C0H,0B6H,6BH,42H,0C0H
	DEFB	0B8H,6EH,42H,0C0H,0B9H,7BH,42H,0C0H
	DEFB	0C2H,23H,43H,0C0H,0C3H,28H,43H,0C0H
	DEFB	0C6H,2BH,43H,0C0H,0DEH,0DAH,2FH,43H
	DEFB	0C0H,0DDH,30H,43H,0C0H,0DEH,0DDH,44H
	DEFB	43H,0C1H,4DH,43H,0C1H,0B8H,5BH,43H
	DEFB	0C1H,0C2H,61H,43H,0C1H,0D4H,63H,43H
	DEFB	0C1H,0D5H,66H,43H,0C1H,0D6H,74H,43H
	DEFB	0C1H,0D6H,0B3H,7AH,43H,0C1H,0D6H,0B8H
	DEFB	3CH,44H,0C1H,0DDH,3FH,44H,0C2H,45H
	DEFB	44H,0C2H,0B2H,46H,44H,0C2H,0B3H,4BH
	DEFB	44H,0C2H,0B6H,4DH,44H,0C2H,0BCH,0DEH
	DEFB	54H,44H,0C2H,0CEH,0DEH,5AH,44H,0C2H
	DEFB	0CFH,5CH,44H,0C2H,0D8H,5FH,44H,0C3H
	DEFB	62H,44H,0C3H,0DEH,25H,45H,0C3H,0B7H
	DEFB	26H,45H,0C3H,0C2H,2FH,45H,0C3H,0DDH
	DEFB	35H,45H,0C3H,0DEH,0DDH,41H,45H,0C4H
	DEFB	46H,45H,0C4H,0DEH,58H,45H,0C4H,0B3H
	DEFB	5DH,45H,0C4H,0DEH,0B3H,2FH,46H,0C4H
	DEFB	0B3H,0B9H,0DEH,3DH,46H,0C4H,0B8H,3FH
	DEFB	46H,0C4H,0DEH,0B8H,47H,46H,0C4H,0C1H
	DEFB	4AH,46H,0C4H,0C2H,4CH,46H,0C4H,0C4H
	DEFB	0DEH,4EH,46H,0C4H,0D7H,52H,46H,0C4H
	DEFB	0DDH,55H,46H,0C4H,0DEH,0DDH,5DH,46H
	DEFB	0C5H,60H,46H,0C5H,0B2H,62H,46H,0C5H
	DEFB	0B6H,0DEH,0D7H,63H,46H,0C5H,0BFH,0DEH
	DEFB	66H,46H,0C5H,0C2H,68H,46H,0C5H,0DDH
	DEFB	6EH,46H,0C6H,73H,46H,0C6H,0B5H,77H
	DEFB	46H,0C6H,0B8H,79H,46H,0C6H,0C1H,7CH
	DEFB	46H,0C6H,0D5H,7DH,46H,0C6H,0D6H,21H
	DEFB	47H,0C6H,0DDH,24H,47H,0C7H,28H,47H
	DEFB	0C8H,29H,47H,0C8H,0BAH,2DH,47H,0C8H
	DEFB	0C2H,2EH,47H,0C8H,0DDH,2FH,47H,0C9H
	DEFB	35H,47H,0C9H,0B3H,39H,47H,0C9H,0BFH
	DEFB	0DEH,41H,47H,0CAH,43H,47H,0CAH,0DEH
	DEFB	4CH,47H,0CAH,0B2H,50H,47H,0CAH,0DEH
	DEFB	0B2H,5CH,47H,0CAH,0B3H,67H,47H,0CAH
	DEFB	0B7H,0DEH,6BH,47H,0CAH,0B8H,6CH,47H
	DEFB	0CAH,0DEH,0B8H,78H,47H,0CAH,0BAH,21H
	DEFB	48H,0CAH,0BCH,24H,48H,0CAH,0C0H,28H
	DEFB	48H,0CAH,0C1H,2CH,48H,0CAH,0C2H,2EH
	DEFB	48H,0CAH,0DEH,0C2H,32H,48H,0CAH,0C4H
	DEFB	37H,48H,0CAH,0DDH,3CH,48H,0CAH,0DEH
	DEFB	0DDH,54H,48H,0CBH,5BH,48H,0CBH,0DEH
	DEFB	77H,48H,0CBH,0B2H,22H,49H,0CBH,0B7H
	DEFB	24H,49H,0CBH,0BAH,27H,49H,0CBH,0BCH
	DEFB	29H,49H,0CBH,0C2H,2BH,49H,0CBH,0C9H
	DEFB	30H,49H,0CBH,0D2H,31H,49H,0CBH,0D4H
	DEFB	34H,49H,0CBH,0D6H,36H,49H,0CBH,0DEH
	DEFB	0D6H,40H,49H,0CBH,0D9H,47H,49H,0CBH
	DEFB	0DDH,4AH,49H,0CBH,0DEH,0DDH,52H,49H
	DEFB	0CCH,54H,49H,0CCH,0DEH,6EH,49H,0CCH
	DEFB	0B3H,75H,49H,0CCH,0B7H,78H,49H,0CCH
	DEFB	0B8H,7AH,49H,0CCH,0C1H,25H,4AH,0CCH
	DEFB	0DDH,2CH,4AH,0CCH,0DEH,0DDH,38H,4AH
	DEFB	0CDH,3AH,4AH,0CDH,0DEH,46H,4AH,0CDH
	DEFB	0B7H,48H,4AH,0CDH,0DEH,0C2H,4CH,4AH
	DEFB	0CDH,0DDH,50H,4AH,0CDH,0DEH,0DDH,58H
	DEFB	4AH,0CEH,5DH,4AH,0CEH,0DEH,67H,4AH
	DEFB	0CEH,0B3H,6FH,4AH,0CEH,0DEH,0B3H,33H
	DEFB	4BH,0CEH,0B4H,4AH,4BH,0CEH,0B8H,4CH
	DEFB	4BH,0CEH,0DEH,0B8H,4DH,4BH,0CEH,0DEH
	DEFB	0C2H,56H,4BH,0CEH,0D8H,59H,4BH,0CEH
	DEFB	0DDH,5BH,4BH,0CEH,0DEH,0DDH,5EH,4BH
	DEFB	0CFH,60H,4BH,0CFH,0B2H,64H,4BH,0CFH
	DEFB	0B7H,6AH,4BH,0CFH,0BBH,6FH,4BH,0CFH
	DEFB	0C0H,72H,4BH,0CFH,0C2H,75H,4BH,0CFH
	DEFB	0C3H,0DEH,78H,4BH,0CFH,0DDH,7CH,4BH
	DEFB	0D0H,23H,4CH,0D0H,0C2H,29H,4CH,0D0H
	DEFB	0C5H,0C4H,2BH,4CH,0D0H,0D4H,0B8H,2EH
	DEFB	4CH,0D0H,0D6H,0B3H,2FH,4CH,0D0H,0DDH
	DEFB	31H,4CH,0D1H,33H,4CH,0D1H,0B8H,3AH
	DEFB	4CH,0D2H,3DH,4CH,0D2H,0BDH,46H,4CH
	DEFB	0D2H,0DDH,48H,4CH,0D3H,4EH,4CH,0D3H
	DEFB	0B3H,51H,4CH,0D3H,0B8H,5AH,4CH,0D3H
	DEFB	0C1H,5EH,4CH,0D3H,0C4H,0DEH,61H,4CH
	DEFB	0D3H,0DDH,64H,4CH,0D4H,69H,4CH,0D4H
	DEFB	0B8H,71H,4CH,0D4H,0BDH,77H,4CH,0D5H
	DEFB	7BH,4CH,0D5H,0B3H,24H,4DH,0D6H,3DH
	DEFB	4DH,0D6H,0B3H,43H,4DH,0D6H,0B8H,5DH
	DEFB	4DH,0D7H,65H,4DH,0D7H,0B2H,68H,4DH
	DEFB	0D7H,0B8H,6CH,4DH,0D7H,0DDH,70H,4DH
	DEFB	0D8H,78H,4DH,0D8H,0C2H,27H,4EH,0D8H
	DEFB	0D4H,2BH,4EH,0D8H,0D5H,2DH,4EH,0D8H
	DEFB	0D6H,37H,4EH,0D8H,0D6H,0B3H,3BH,4EH
	DEFB	0D8H,0D6H,0B8H,4FH,4EH,0D8H,0DDH,51H
	DEFB	4EH,0D9H,5CH,4EH,0DAH,61H,4EH,0DAH
	DEFB	0B7H,71H,4EH,0DAH,0C2H,73H,4EH,0DAH
	DEFB	0DDH,77H,4EH,0DBH,24H,4FH,0DBH,0B3H
	DEFB	2BH,4FH,0DBH,0B8H,3BH,4FH,0DBH,0DDH
	DEFB	40H,4FH,0DCH,41H,4FH,0DCH,0B2H,44H
	DEFB	4FH,0DCH,0B7H,46H,4FH,0DCH,0CBH,0DEH
	DEFB	4DH,4FH,0DCH,0DDH,50H,4FH,00H

;         Subroutine __________________________
;            Inputs  ________________________
;            Outputs ________________________

C7E50:  PUSH    DE
        LD      E,(IX+4)
        LD      D,(IX+5)
        BIT     7,D                     ; MSX-JE ?
        PUSH    DE
        POP     IY
        POP     DE
        RET     NZ                      ; yep, quit
        PUSH    IX
        POP     IY
        DEC     IY
        DEC     IY
        DEC     IY
        RET

        DEFS    04000H-$,0FFH	; (rem: modified for $ org offset -04000H)

        .END
