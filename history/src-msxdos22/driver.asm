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
;


BRSLREG EQU     0138H                   ; use BRSLREG instead of RSLREG to avoid name clash with disk.mac

DEFDPB	EQU	$-1
MYSIZE	EQU	0
SECLEN  EQU     512

;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________

        IF HSH = 0

INIHRD:

        ENDIF

C6EFA:	RET

;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________

DRIVES:
C6EFB:	LD	L,0                     ; number of drives of this interface = 0
        RET

;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________

DSKIO:
DSKCHG:
GETDPB:
J6EFE:	LD	A,12                    ; other error
        SCF

;	  Subroutine __________________________
;	     Inputs  ________________________
;	     Outputs ________________________

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

OEMSTA:
J6F0B:	PUSH	HL                      ; store BASIC pointer
        LD	DE,PROCNM
        LD	HL,I72C7
J6F12:	LD	A,(DE)
        CP	(HL)
        INC	DE
        INC	HL
        JR	Z,J6F1B
        POP	HL                      ; restore BASIC pointer
        SCF                             ; statement not recognized
        RET

J6F1B:	AND	A                       ; end of statement name ?
        JR	NZ,J6F12                ; nope, continue
        CALL	C728E                   ; embedded string to screen
        DEFB	12
        DEFB	"**********************************",13,10
        DEFB	"*                                *",13,10

        IF      HSH = 0
; ASCII DOS2 has bad spelling
        DEFB	"* MSX-DOS2 Cartrige Self Checker *",13,10
        ELSE
; HSH DOS2 had good spelling
        DEFB	"* MSXDOS2 Cartridge Self Checker *",13,10
        ENDIF

        DEFB	"*                                *",13,10
        DEFB	"*       W A R N I N G ! !        *",13,10
        DEFB	"*                                *",13,10
        DEFB	"* The system will go into a halt *",13,10
        DEFB	"* state after the check.         *",13,10
        DEFB	"*                                *",13,10
        DEFB	"**********************************",13,10
        DEFB	13,10
        DEFB	" Proceed? (Y/N) "
        DEFB	0
J709D:	CALL	CHGET                   ; get key
        CALL	C7294   	        ; CR/LF to screen
        AND	0DFH                    ; to upper
        CP	'N'                     ; abort ?
        POP	HL                      ; restore BASIC pointer
        RET	Z                       ; yep, quit
        CALL	C7294   	        ; CR/LF to screen
        LD	HL,I7101
        LD	DE,0C000H
        LD	BC,L7101
        LDIR
        LD	HL,(C4092+1)		; bank switch address
        LD	(CC024+1),HL
        XOR	A			; bank 0
        LD	H,A
        LD	L,A			; current checksum 0
        PUSH	HL                      ; store checksum
J70C1:	CALL	CC016			; calculate checksum
        EX	DE,HL
        POP	HL                      ; restore checksum
        ADD	HL,DE                   ; update checksum
        PUSH	HL                      ; store checksum
        EX	DE,HL
        CALL	C7139			; print checksum
        INC	A
        CP	3+1			; next bank ?
        JR	C,J70C1		        ; yep,
        POP	HL                      ; restore checksum
        CALL	C7139			; print checksum (total)
        LD	A,(C4092+2)
        CP	7FH                     ; bank switch address 07FxxH (ASCII DOS2 mapper) ?
        CALL	Z,C717D                 ; yep, check MSX-JE and memory mapper
        CALL	C728E                   ; embedded string to screen
        DEFB	13
        DEFB	"Check finished. Please RESET"
        DEFB	0
J70FE:	DI
        JR	J70FE                   ; halt system

I7101:
        PHASE	0C000H

CC000:	CALL	C402D			; GETSLT
        PUSH	AF			; store current slot id page 1
        LD	A,E
        LD	H,40H			; (rem: HIGH 4000H)
        CALL	ENASLT			; select slot on page 1
        CALL	CC028			; calculate checksum
        POP	AF                      ; restore slot id
        PUSH	HL                      ; store checksum
        LD	H,40H			; (rem: HIGH 4000H)
        CALL	ENASLT			; restore slot page 1
        POP	HL                      ; restore checksum
        RET

CC016:	DI
        PUSH	AF                      ; store bank
        CALL	CC024			; select bank
        CALL	CC028			; calculate checksum
        XOR	A                       ; bank = 0
        CALL	CC024			; select bank
        POP	AF                      ; restore bank
        RET

;	  Subroutine select bank
;	     Inputs  ________________________
;	     Outputs ________________________

CC024:	LD	(0),A
        RET

;	  Subroutine calculate checksum
;	     Inputs  ________________________
;	     Outputs ________________________

CC028:	LD	HL,0                    ; checksum = 0
        LD	B,H                     ; high byte = 0
        LD	DE,04000H
J7130:	LD	A,(DE)
        INC	DE
        LD	C,A
        ADD	HL,BC                   ; update checksum
        BIT	7,D                     ; bank finished ?
        JR	Z,J7130                 ; nope, continue
        RET

        DEPHASE

L7101	EQU	$-I7101

;	  Subroutine print checksum
;	     Inputs  A = bank (4 = total, 5 = kanji)
;	     Outputs ________________________

C7139:	CALL	C728E                   ; embedded string to screen
        DEFB	"Check sum of "
        DEFB	0
        PUSH	HL
        LD	HL,I716C
        CP	3+1                     ; valid bank ?
        JR	C,J715A                 ; yep,
        LD	HL,I7171
        JR	Z,J715A                 ; total
        LD	HL,I7177                ; KANJI bank
J715A:	CALL	C7297                   ; string to screen
        POP	HL
        CALL	C,C72BA		        ; if bank, output nibble in HEX to screen
        CALL	C728E                   ; embedded string to screen
        DEFB	"="
        DEFB	0
        CALL	C72A6                   ; output word in HEX to screen
        JP	C7294   	        ; CR/LF to screen

I716C:	DEFB	"BANK"
        DEFB	0

I7171:	DEFB	"Total"
        DEFB	0

I7177:	DEFB	"KANJI"
        DEFB	0

;	  Subroutine check MSX-JE and memory mapper
;	     Inputs  ________________________
;	     Outputs ________________________

C717D:	CALL	BRSLREG                 ; read primairy slot register
        AND	0CH
        RRCA
        RRCA                            ; primairy slot page 1
        OR	88H
        LD	E,A			; slotid MSX-JE ROM (secundairy slot 2)
        CALL	CC000			; caculate checksum of MSX-JE ROM
        LD	A,5                     ; bank = 5 (kanji)
        CALL	C7139			; print checksum
        DI
        CALL	BRSLREG                 ; read primairy slot register
        AND	0CH
        RRCA
        RRCA                            ; primairy slot page 1
        OR	84H			; slotid memory mapper (secundairy slot 1)
        LD	H,80H			; (rem: HIGH 8000H)
        CALL	ENASLT			; select memory mapper in page 2
        XOR	A
        OUT	(0FEH),A		; select segment 0 in page 2
        LD	HL,0C000H
        LD	DE,08000H
        LD	BC,04000H
        LDIR				; duplicate page 3 content system RAM to memory mapper of interface
        CALL	BRSLREG                 ; read primairy slot register
        AND	0CH
        RRCA
        RRCA                            ; primairy slot page 1
        OR	84H			; slotid memory mapper (secundairy slot 1)
        LD	HL,0C000H
        CALL	ENASLT			; select memory mapper in page 3

	IF (NUMSEG = 0) || (DOS201)

        CALL	C71E3                   ; determine number of segments in memory mapper
        LD	C,A                     ; store number of RAM segments
        LD	B,C                     ; number of RAM segments

	ELSE

	LD	B,NUMSEG                ; fixed number of RAM segments

	ENDIF

J71C0:	LD	A,B                     ; segment
        CALL	C7209                   ; write segment with pattern
        DJNZ	J71C0                   ; next segment

	IF (NUMSEG = 0) || (DOS201)

        LD	B,C                     ; number of RAM segments

	ELSE

	LD	B,NUMSEG                ; fixed number of RAM segments

	ENDIF

J71C7:	LD	A,B                     ; segment
        CALL	C7232                   ; check segment for pattern
        DJNZ	J71C7                   ; next segment
        LD	HL,0C000H
        LD	DE,08000H
        LD	BC,04000H
        LDIR				; duplicate page 3 content of memory mapper of interface back to system RAM
        OUT	(0FFH),A
        XOR	A                       ; segment 0
        CALL	C7209                   ; write segment with pattern
        XOR	A
        CALL	C7232                   ; check segment for pattern
        RET

	IF (NUMSEG = 0) || (DOS201)

;	  Subroutine determine number of segments in memory mapper
;	     Inputs  ________________________
;	     Outputs ________________________

C71E3:	PUSH	BC
        PUSH	DE
        PUSH	HL
        LD	HL,08000H+03FFFH
        XOR	A                       ; segment = 0
        LD	BC,0AA55H
J71ED:	OUT	(0FEH),A                ; select segment
        LD	(HL),B                  ; write RAM
        INC	A
        JR	NZ,J71ED                ; next segment
        LD	D,A                     ; segment = 0
J71F4:	LD	A,D
        OUT	(0FEH),A                ; select segment
        LD	A,(HL)
        CP	B                       ; correct value read back from RAM ?
        JR	NZ,J7203                ; nope,
        LD	(HL),C                  ; write RAM
        LD	A,(HL)
        CP	C                       ; correct value read back from RAM ?
        JR	NZ,J7203                ; nope,
        INC	D
        JR	NZ,J71F4                ; next segment
J7203:	DEC	D
        LD	A,D
        POP	HL
        POP	DE
        POP	BC
        RET

	ENDIF

;	  Subroutine write segment with pattern
;	     Inputs  ________________________
;	     Outputs ________________________

C7209:	OUT	(0FEH),A
        CALL	C728E                   ; embedded string to screen
        DEFB	13
        DEFB	"Writing segment "
        DEFB	0
        CALL	C72B1                   ; output byte in HEX to screen
        LD	HL,08000H
J7226:	LD	(HL),A
        INC	HL
        INC	A
        INC	A
        JR	Z,J722D
        DEC	A
J722D:	BIT	6,H
        JR	Z,J7226
        RET

;	  Subroutine check segment for pattern
;	     Inputs  ________________________
;	     Outputs ________________________

C7232:	LD	D,1                     ; no error
        OUT	(0FEH),A
        CALL	C728E                   ; embedded string to screen
        DEFB	13
        DEFB	"Reading segment "
        DEFB	0
        CALL	C72B1                   ; output byte in HEX to screen
        LD	HL,08000H
J7251:	LD	E,(HL)                  ; read
        CP	E                       ; correct value ?
        CALL	NZ,C7265                ; nope, print address, expected value and actual value
        INC	HL
        INC	A
        INC	A
        JR	Z,J725C
        DEC	A
J725C:	BIT	6,H                     ; segment finished ?
        JR	Z,J7251                 ; nope, continue
        DEC	D                       ; error ?
        CALL	NZ,C7294                ; yep, CR/LF to screen
        RET

;	  Subroutine print address, expected value and actual value
;	     Inputs  A = expected value, E = actual value
;	     Outputs ________________________

C7265:	PUSH	AF                      ; store expected value
        CALL	C728E                   ; embedded string to screen
        DEFB	13,10
        DEFB	" "
        DEFB	0
J726D:	RES	7,H                     ; address in segment
        CALL	C72A6                   ; output word in HEX to screen
        SET	7,H                     ; address
        CALL	C728E                   ; embedded string to screen
        DEFB	"  W:"
        DEFB	0
        CALL	C72B1                   ; output byte in HEX to screen
        CALL	C728E                   ; embedded string to screen
        DEFB	" R:"
        DEFB	0
        LD	A,E                     ; actual value
        CALL	C72B1                   ; output byte in HEX to screen
        POP	AF                      ; restore expected value
J728B:	LD	D,2                     ; error
        RET

;	  Subroutine embedded string to screen
;	     Inputs  ________________________
;	     Outputs ________________________

C728E:	EX	(SP),HL
        CALL	C7297                   ; string to screen
        EX	(SP),HL
        RET

;	  Subroutine CR/LF to screen
;	     Inputs  ________________________
;	     Outputs ________________________

C7294:	LD	HL,I72D2

;	  Subroutine string to screen
;	     Inputs  ________________________
;	     Outputs ________________________

C7297:	PUSH	AF                      ; store
J7298:	LD	A,(HL)
        AND	A                       ; end of string ?
        JR	Z,J72A4                 ; yep, quit
        INC	HL
        PUSH	HL                      ; store string pointer
        CALL	CHPUT                   ; print character
        POP	HL                      ; restore string pointer
        JR	J7298                   ; next

J72A4:	POP	AF                      ; restore
        RET

;	  Subroutine output word in HEX to screen
;	     Inputs  ________________________
;	     Outputs ________________________

C72A6:	PUSH	AF
        LD	A,H
        CALL	C72B1                   ; output byte in HEX to screen
        LD	A,L
        CALL	C72B1                   ; output byte in HEX to screen
        POP	AF
        RET

;	  Subroutine output byte in HEX to screen
;	     Inputs  ________________________
;	     Outputs ________________________

C72B1:	PUSH	AF
        RRCA
        RRCA
        RRCA
        RRCA
J72B6:	CALL	C72BA                   ; output nibble in HEX to screen
        POP	AF

;	  Subroutine output nibble in HEX to screen
;	     Inputs  ________________________
;	     Outputs ________________________

C72BA:	PUSH	AF
        AND	0FH
        CP	0AH
        SBC	A,69H
        DAA
        CALL	CHPUT
        POP	AF
        RET

I72C7:	DEFB	"DOS2MEMCHK"
        DEFB	0

I72D2:	DEFB	13,10
        DEFB	0

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

