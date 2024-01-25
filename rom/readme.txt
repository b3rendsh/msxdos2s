The ROM's in this folder have been assembled with the following build options.

MSXD22S.ROM (mod-32k):

OPTM	EQU	1	; Optimized code
DOS201	EQU	1	; include DiskBasic 2.01 fixes
DOS2	EQU	1	; DOS2 flag for diskdriver
USESBF	EQU	0	; use buffer for screen output
SLTEXP	EQU	0	; DOS2 has a slot expander: memory mapper in slot 1, KANJI BASIC in secundairy slot 2
NUMSEG	EQU	0	; memory mapper of NUMSEG pages in secundairy slot 1
HSH	EQU	0	; HSH style 0: no DOS2 skip key, 1: DOS2 skip key (SELECT or CODE+GRAPH), 2: DOS2 skip key (INSERT)


MSXD22.ROM (src-z80asm):

OPTM	EQU	0	; Optimized code
DOS201	EQU	1	; include DiskBasic 2.01 fixes
DOS2	EQU	1	; DOS2 flag for diskdriver
USESBF	EQU	0	; use buffer for screen output
MSXJE	EQU	0	; KANJI BASIC in DOS2 bank 3
SLTEXP	EQU	0	; DOS2 has a slot expander: memory mapper in slot 1, KANJI BASIC in secundairy slot 2
NUMSEG	EQU	0	; memory mapper of NUMSEG pages in secundairy slot 1
DOS1	EQU	0	; DOS1 in DOS2 bank 3
HSH	EQU	0	; HSH style 0: no DOS2 skip key, 1: DOS2 skip key (SELECT or CODE+GRAPH), 2: DOS2 skip key (INSERT)
BNKADR	EQU	07FFEH
BNKTRL	EQU	0	; translate bank numbers

