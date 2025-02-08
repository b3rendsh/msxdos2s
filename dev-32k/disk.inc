; Target build options
; --------------------
; CFIDE		Include 8-bit CF IDE driver
; PPIDE		Include PPI 8255 IDE driver
; GPIO		Include GPIO 115K2 / 57K6 driver
; UART		Include UART 1655x serial driver
; CORE		Include dummy IDE driver
; IDEDOS1	MSX-DOS 1 IDE
; TEST		Used for testing purposes


; DOS1 compile options
; --------------------
;
; FAT16DOS1	Include FAT16 for the DOS 1 kernel


; DOS2 compile options
; --------------------
;
; OPTM
; 0 = original code
; 1 = optimized code
; Optimization goal is first smaller code size and then improved performance
; The fat16.com patch program won't work with optimized code.
;
; FAT16		Include FAT16 for the DOS 2 kernel
; NODOS1	Remove DOS 1 msxdos.sys entry points to free rom space
; FASTALLOC	Calculate max 4K free clusters for large drives in ALLOC function to avoid slow DIR command

; Apply to all targets
	OPTM	EQU	1	

IF PPIDE || GPIO || UART
	DEFINE	FASTALLOC
ENDIF

IF CFIDE || PPIDE
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	MOD1	EQU	'x'	; eXtended
	MOD2	EQU	'i'	; Ide
ENDIF

IFDEF GPIO
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	DEFINE	NODOS1
	MOD1	EQU	'x'	; eXended
	MOD2	EQU	'r'	; gpio Remote disk
ENDIF

IFDEF UART
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	DEFINE	NODOS1
	MOD1	EQU	'x'	; eXended
	MOD2	EQU	'u'	; UART
ENDIF

IFDEF CORE
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	MOD1	EQU	'x'	; eXtended
	MOD2	EQU	' '	;
ENDIF

IFDEF TEST
	MOD1	EQU	't'	; Test
	MOD2	EQU	' '	; 
ENDIF
