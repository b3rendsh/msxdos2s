; Target build options:
; ROM		Standard DOS 2.2 32K ROM
; TEST		Used for testing purposes
; FAT16		Include FAT16 patch for the DOS 2 kernel


; DOS2 compile options
; OPTM
; 0 = orginal code
; 1 = optimized code
; Optimization goal is first smaller code size and then improved performance


IFDEF ROM
	OPTM	EQU	0
	MOD1	EQU	's'
	MOD2	EQU	' '
ENDIF

IFDEF TEST
	OPTM	EQU	1
	MOD1	EQU	't'
	MOD2	EQU	' '
ENDIF

IFDEF FAT16
	OPTM	EQU	1
	MOD1	EQU	's'
	MOD2	EQU	'x'
ENDIF

;Mod: following options are fixed choice
;HSH	EQU	2	HSH style 2 DOS2 skip key is INS
;DOS201	EQU	1	include DiskBasic 2.01 fixes
;DOS2	EQU	1	DOS2 flag for diskdriver
;MSXJE	EQU	0	KANJI BASIC in DOS2 bank 3
;DOS1	EQU	0	DOS1 in DOS2 bank 3
;NUMSEG	EQU	0	memory mapper of NUMSEG pages in secundairy slot 1
;SLTEXP	EQU	0	DOS2 has a slot expander: memory mapper in slot 1, KANJI BASIC in secundairy slot 2
;USESBF	EQU	0	use buffer for screen output

; Mod: Bank definition removed



