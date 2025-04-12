; Target build options
; --------------------
; CFIDE		Include 8-bit CF IDE driver
; PPIDE		Include PPI 8255 IDE driver
; JIO		Include JIO 115K2 driver
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
	OPTM	EQU	1	; use optimized code
	BTWAIT	EQU	$5000	; boot menu wait time-out (default $5000 is appr. 3 sec for MSX/3.58Mhz)


IF PPIDE || JIO || UART
	DEFINE	FASTALLOC
ENDIF

IF CFIDE || PPIDE
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	DEFINE	BOOTCHOICE	; show boot menu / drive choice
	MOD1	EQU	'x'	; eXtended
	MOD2	EQU	'i'	; Ide
ENDIF

IFDEF JIO
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	MOD1	EQU	'x'	; eXended
	MOD2	EQU	'r'	; JIO Remote disk
ENDIF

IFDEF UART
	DEFINE	FAT16
	DEFINE	FAT16DOS1
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
