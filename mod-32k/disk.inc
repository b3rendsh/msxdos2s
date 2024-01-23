; DOS2 compile options

; OPTM
; 0 = orginal code
; 1 = optimized code
IFDEF TEST
OPTM	EQU	0
ELSE
OPTM	EQU	1
ENDIF

DOS201	EQU	1		; include DiskBasic 2.01 fixes
DOS2	EQU	1		; DOS2 flag for diskdriver
USESBF	EQU	0		; use buffer for screen output
SLTEXP	EQU	0		; DOS2 has a slot expander: memory mapper in slot 1, KANJI BASIC in secundairy slot 2
NUMSEG	EQU	0		; memory mapper of NUMSEG pages in secundairy slot 1
DOS1	EQU	0		; DOS1 in DOS2 bank 3
HSH	EQU	0		; HSH style 0: no DOS2 skip key, 1: DOS2 skip key (SELECT or CODE+GRAPH), 2: DOS2 skip key (INSERT)

; Mod: following option is removed
; MSXJE	EQU	0		; KANJI BASIC in DOS2 bank 3


; Mod: Bank definition removed
