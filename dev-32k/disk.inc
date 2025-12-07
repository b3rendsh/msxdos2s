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
; FAT16DOS1	Include FAT16 for the DOS 1 kernel (mandatory for JIO driver)
; MAXENT	Use 16-bit maximum directory entries value (FAT16)
; FASTALLOC	Cutoff free disk space count at 2048 free clusters


; DOS2 compile options
; --------------------
; OPTM		0 = original code, 1 = optimized code
; FAT16		Include FAT16 for the DOS 2 kernel
; NODOS1	Remove DOS 1 msxdos.sys entry points to free rom space
; FASTALLOC	Calculate max 2K free clusters for large drives in ALLOC function to avoid slow DIR command
; ROM16K	Split 32K DOS ROM in 16K ROM and load 16K kernel code from designated area on disk
; DOSV231	Include DOS v2.31 changes
; TURBOR	Include turbo R specific DOS v2.31 changes
; DIRTYBIT	Enable undelete flag for DOS1 FAT12 and FAT16 partitions (slow on large FAT16 partitions!)


; IDE driver compile options
; --------------------------
; BOOTCODE	Check for bootloader signature in the MBR and if present execute the boot code
; BOOTCHOICE	Show boot menu / drive choice
; BTWAIT	Boot menu wait time-out (default $5000 is appr. 3 sec for MSX/3.58Mhz)


; Apply to all targets
	OPTM	EQU	1
	BTWAIT	EQU	$5000

IF PPIDE || JIO || UART || IDEDOS1
	DEFINE	FASTALLOC
	DEFINE	MAXENT
ENDIF

IF CFIDE || PPIDE
	DEFINE	TURBOR
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	DEFINE	BOOTCODE
	DEFINE	BOOTCHOICE
	MOD1	EQU	'x'	; eXtended
	MOD2	EQU	'i'	; Ide
ENDIF

IFDEF JIO
	DEFINE	TURBOR
	DEFINE	FAT16
	DEFINE	FAT16DOS1
	MOD1	EQU	'x'	; eXended
	MOD2	EQU	'r'	; JIO Remote disk
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

IFDEF TURBOR
	DEFINE	DOSV231
ENDIF
