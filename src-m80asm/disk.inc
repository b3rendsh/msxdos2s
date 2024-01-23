; DOS2 compile options

; OPTM
; 0 = orginal code
; 1 = optimized code
OPTM	EQU	0

DOS201	EQU	1		; include DiskBasic 2.01 fixes
DOS2	EQU	1		; DOS2 flag for diskdriver
USESBF	EQU	0		; use buffer for screen output
MSXJE	EQU	1		; KANJI BASIC in DOS2 bank 3
SLTEXP	EQU	0		; DOS2 has a slot expander: memory mapper in slot 1, KANJI BASIC in secundairy slot 2
NUMSEG	EQU	0		; memory mapper of NUMSEG pages in secundairy slot 1
DOS1	EQU	0		; DOS1 in DOS2 bank 3
HSH	EQU	0		; HSH style 0: no DOS2 skip key, 1: DOS2 skip key (SELECT or CODE+GRAPH), 2: DOS2 skip key (INSERT)


BNKADR	EQU	07FFEH
BNKTRL	EQU	0		; translate bank numbers

MBNKTL	MACRO

	DEFB	001H		; ROM segment 0, enable HDC
	DEFB	081H		; ROM segment 1, enable HDC
	DEFB	040H		; ROM segment 2, disable HDC
	DEFB	0C0H		; ROM segment 3, disable HDC
	DEFB	020H		; ROM segment 4, disable HDC
	DEFB	0A0H		; ROM segment 5, disable HDC
	DEFB	060H		; ROM segment 6, disable HDC
	DEFB	0E0H		; ROM segment 7, disable HDC

	ENDM

BNKCHG	MACRO

	LD	(BNKADR),A

	ENDM
