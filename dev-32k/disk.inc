; Target build options
; --------------------
; CFIDE		Include 8-bit CF IDE driver
; PPIDE		Include PPI 8255 IDE driver
; CORE		Include dummy IDE driver
; IDEDOS1	MSX-DOS 1 IDE
; TEST		Used for testing purposes


; DOS1 compile options
; --------------------
;
; FAT16DOS1
; Include FAT16 for the DOS 1 kernel


; DOS2 compile options
; --------------------
;
; OPTM
; 0 = orginal code
; 1 = optimized code
; Optimization goal is first smaller code size and then improved performance
; The fat16.com patch program won't work with optimized code.
;
; FAT16		
; Include FAT16 patch for the DOS 2 kernel


IF CFIDE || PPIDE
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	OPTM	EQU	1
	MOD1	EQU	'x'	; eXtended
	MOD2	EQU	'i'	; Ide
ENDIF

IFDEF CORE
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	OPTM	EQU	1
	MOD1	EQU	'x'	; eXtended
	MOD2	EQU	' '	;
ENDIF

IFDEF TEST
	OPTM	EQU	1
	MOD1	EQU	't'	; Test
	MOD2	EQU	' '	; 
ENDIF

