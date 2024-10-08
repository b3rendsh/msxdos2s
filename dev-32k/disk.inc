; Target build options:
; ROM		Standard DOS 2.2 32K ROM
; TEST		Used for testing purposes


; DOS2 compile options:
;
; OPTM
; 0 = orginal code
; 1 = optimized code
; Optimization goal is first smaller code size and then improved performance
; The fat16.com patch program won't work with optimized code.
;
; FAT16		
; Include FAT16 patch for the DOS 2 kernel
;
; The BEER IDE driver will be included by default


IFDEF ROM
	OPTM	EQU	0
	MOD1	EQU	's'	; Small
	MOD2	EQU	'i'	; Ide
ENDIF

IFDEF BEER232
	DEFINE	FAT16
	OPTM	EQU	1
	MOD1	EQU	'x'	; eXtended
	MOD2	EQU	'i'	; Ide
ENDIF

IFDEF TEST
	DEFINE	FAT16
	OPTM	EQU	1
	MOD1	EQU	't'	; Test
	MOD2	EQU	'i'	; Ide
ENDIF

