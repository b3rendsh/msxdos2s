; ------------------------------------------------------------------------------
; driver.asm
;
; Copyright (C) 2025 H.J. Berends*
; * Part of the code is based on the BEER-202 driver by SOLiD and other
; PPI 8255 and 8-BIT CF IDE solutions publicly shared on the internet.
; 
; You can freely use, distribute or modify this program.
; It is provided freely and "as it is" in the hope that it will be useful, 
; but without any warranty of any kind, either expressed or implied.
; ------------------------------------------------------------------------------
; Universal driver for MSX-DOS:
; + Maps up to 8 partitions to drives
; + Extended partition support
; + Boot drive selection (boot menu)
; The driver is split in a DOS section (this file) and hardware interface driver
; ------------------------------------------------------------------------------

	        INCLUDE "disk.inc"	; Assembler directives
		INCLUDE	"msx.inc"	; MSX constants and definitions

		SECTION	DRIVER

		; Mandatory symbols defined by the DOS driver
		PUBLIC	INIHRD
		PUBLIC	DRIVES
		PUBLIC	GETDPB
		PUBLIC	CHOICE
		PUBLIC	DSKFMT
		PUBLIC	MTOFF
		PUBLIC	OEMSTA
		PUBLIC	DEFDPB
		PUBLIC	MYSIZE
		PUBLIC	SECLEN

		; Additional symbols defined by the DOS driver
		PUBLIC	BOOTMENU
		PUBLIC	W_CURDRV
		PUBLIC	W_BOOTDRV
		PUBLIC	W_DRIVES
		PUBLIC	DRVSIZE
		PUBLIC	PART_BUF
		PUBLIC	PrintMsg
		PUBLIC	PrintCRLF
		PUBLIC	PrintString
		PUBLIC	MakeDec

		; Hardware interface symbols used by the DOS driver
		EXTERN	DRVMEM
		EXTERN	DRVINIT
		EXTERN	DSKIO
		EXTERN	READSEC

		; Disk routines used by driver
		EXTERN	GETWRK		; Get address of disk driver's work area
		EXTERN	GETSLT		; Get slot of this interface

MYSIZE		equ	DRVSIZE+DRVMEM
SECLEN		equ	512

; ------------------------------------------------------------------------------
; *** DOS driver routines ***
; ------------------------------------------------------------------------------

; Structure of DOS driver work area:
; $00	First absolute sector of partition #1
; $04	Partition type of partition #1
; ...
; $23	First absolute sector of partition #8
; $27	Partition type of partition #8
; 
; $28 - $2A work variables:
W_CURDRV	equ	$28	; Current drive
W_BOOTDRV	equ	$29	; Boot drive (partition)
W_DRIVES	equ	$2a	; Number of drives (partitions) on disk
;
DRVSIZE		equ	$2b	; DOS driver workarea size

; ------------------------------------------
; INIHRD - Initialize the disk
; Input : None
; Output: None
; May corrupt: AF,BC,DE,HL,IX,IY
; Note: the workbuffer is not available yet so most of the initialization is moved to the DRIVES routine.
; ------------------------------------------
INIHRD:		ld	a,$06
		call	SNSMAT			; Check if CTRL key is pressed
		and	2
		jr	z,r101			; z=yes: exit disk init
		xor	a
		ret
r101:		inc	sp
		inc	sp
		ret

; ------------------------------------------
; DRIVES - Get number of drives connected
; A maximum of 8 partitions (drives) is supported.
; Input:
;   F = The zero flag is reset if one physical drive must act as two logical drives.
; Output:
;   L = Number of drives connected. A value of 0 is not allowed for DOS 1.
; May corrupt: F,HL,IX,IY
;
; The DRIVES routine will also initialize the work environment
; ------------------------------------------

; Temporary RAM variables:
PART_BUF	equ	$C000		; Copy of disk info / Master Boot Record
PART_BUFX	equ	PART_BUF+$200	; Copy of extended partition boot record
PART_NEXT	equ	PART_BUF+$400	; Pointer to next partition in work area
PART_EXTLO	equ	PART_BUF+$402	; First extended partition offset low word
PART_EXTHI	equ	PART_BUF+$404	; First extended partition offset high word
PART_EBRLO	equ	PART_BUF+$406	; Extended partition boot record offset low word
PART_EBRHI	equ	PART_BUF+$408	; Extended partition boot record offset high word

DRIVES:		push	af
		push	bc
		push	de

		; initialize work buffer
		call	GETWRK			; HL and IX point to work buffer
		push	hl
		ld	d,h
		ld	e,l
		inc	de
		ld	(hl),$00
		ld	bc,MYSIZE-1
		ldir

		; probe hardware, display info and validate MBR
		call	DRVINIT
		jr	nz,r207			; nz=interface not detected / canceled
		jr	c,r207			; c=interface time-out
		ld	hl,PART_BUF		; Buffer address
		xor	a
		ld	e,a
		ld	d,a
		ld	c,a			; MBR sector address = 0 (24 bits)
		call	READSEC			; read master boot record
		jr	c,r207
		ld	a,(PART_BUF+$01fe)	; Validate boot signature (AA 55)
		cp	$55
		jr	nz,r207
		ld	a,(PART_BUF+$01ff)
		cp	$aa
		jr	nz,r207

		pop	de			; Pointer to work bufer, initialided with zeros
		ld	hl,PART_BUF+$01be	; Start of partition table
		ld	b,$04			; max 4 primary partitions
r201:		ld	c,(hl)			; Save status byte (active/inactive)
		push	bc
		inc	hl			; Skip CHS address information of first sector
		inc	hl
		inc	hl
		inc	hl
		ld	a,(hl)			; Load partition type
		call	PartitionType
		jr	z,r202
		call	PartitionExt
		call	z,xpart
		ld	bc,$000c
		add	hl,bc			; Move to next partition
		pop	bc
		jr	r204
r202:		inc	hl			; Skip CHS address information of last sector
		inc	hl
		inc	hl
		inc	hl
		ldi				; Get LBA - first absolute sector in partition (4 bytes)
		ldi
		ldi
		ldi
		ld	(de),a			; Partition type
		inc	de
		inc	hl			; Skip LBA number of sectors (4 bytes) / move to next partition
		inc	hl
		inc	hl
		inc	hl
		pop	bc
		ld	a,c			; Restore status byte
		cp	$80			; Is active partition (and valid type)?
		jr	nz,r203
		ld	a,(ix+W_DRIVES)
		ld	(ix+W_BOOTDRV),a	; Update boot drive
r203:		inc	(ix+W_DRIVES)		; Increase partition count
r204:		ld	a,(ix+W_DRIVES)
		cp	$08			; Maximum partitions processed?
		jr	nc,r205
		djnz	r201			; process next primary partition
r205:		ld	a,(ix+W_DRIVES)
IFDEF IDEDOS1
		or	a
		jr	nz,r206
		inc	a			; Return value of 0 drives is not allowed in DOS 1
r206:
ENDIF
		ld	l,a			; set number of drives
		pop	de
		pop	bc
		pop	af
		ret

; Hardware not detected or MBR not valid
r207:		pop	hl
		jr	r205

; Process extended partitions
xpart:		ld	(PART_NEXT),de		; save pointer to next partition in workarea
		push	af
		push	hl
		inc	hl
		inc	hl
		inc	hl
		inc	hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		ld	(PART_EXTLO),de		; save first extended partition offset
		ld	(PART_EXTHI),bc		; "
		call	xpart1
		ld	de,(PART_NEXT)		; Load pointer to next partition in workarea
		pop	hl
		pop	af
		ret

xpart1:		ld	(PART_EBRLO),de		; save extended partition boot record offset
		ld	(PART_EBRHI),bc		; "
		ld	hl,PART_BUFX		; set extended partition boot record buffer
		call	READSEC			; read boot sector
		ret	c
		ld	a,(PART_BUFX+$01fe)	; Validate boot signature (AA 55)
		cp	$55
		ret	nz
		ld	a,(PART_BUFX+$01ff)
		cp	$aa
		ret	nz
		ld	a,(PART_BUFX+$01c2)	; Partition type of first entry
		call	PartitionType
		jr	nz,r222
r221:		ld	hl,PART_BUFX+$01c6	; First sector in partition
		ld	de,(PART_NEXT)		; Load pointer to next partition in workarea
		ld	a,(PART_EBRLO)
		add	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EBRLO+1)
		adc	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EBRHI)
		adc	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EBRHI+1)
		adc	a,(hl)
		ld	(de),a
		inc	de
		ld	a,(PART_BUFX+$01c2)	; load partition type
		ld	(de),a		
		inc	de
		ld	(PART_NEXT),de		; Save pointer to next partition in workarea
		inc	(ix+W_DRIVES)		; Increase partition counter
		ld	a,(ix+W_DRIVES)
		cp	$08			; Maximum partitions processed?
		ret	z			; z=yes
r222:		ld	a,(PART_BUFX+$01d2)	; Partition type of 2nd entry
		call	PartitionExt
		ret	nz			; End of chain
		ld	hl,PART_BUFX+$01d6	; pointer to sector number of next extended partition
		ld	de,(PART_EXTLO)		; value is relative to first extended partition sector
		ld	bc,(PART_EXTHI)
		ld	a,(hl)
		add	a,e
		ld	e,a
		inc	hl
		ld	a,(hl)
		adc	a,d
		ld	d,a
		inc	hl
		ld	a,(hl)
		adc	a,c
		ld	c,a
		inc	hl
		ld	a,(hl)
		adc	a,b
		ld	b,a
		jr	xpart1

; Validate partition type
PartitionType:	cp	$01			; FAT12
		ret	z
IFNDEF FAT16DOS1
		push	af
		ld      a,(DOSVER)		; 15=BEER-DOS1
		cp      $20			; Master disk system is DOS 2 or higher?
		jr	c,r223
		pop	af
ENDIF
		cp	$04			; FAT16 (<=32MB)
		ret	z
		cp	$06			; FAT16B (>32MB)
		ret	z
		cp	$0e			; FAT16B with LBA
		ret
r223:		pop	af
		ret


; Validate extended partition type
PartitionExt:	cp	$05			; Extended partition (CHS,LBA)
		ret	z
		cp	$0f			; Extended partition (LBA)
		ret

; ------------------------------------------
; GETDPB - Set DPB using sector 0 / bootsector of partition
; Called by DOS 1 only, not used by DOS 2.2
; Input:
;   A  = Drive number
;   B  = First byte of FAT
;   C  = Media descriptor
;   HL = Base address of DPB
; Output:
;   [HL+1] .. [HL+18] = DPB fo the specified drive
; ------------------------------------------
GETDPB:		ei
		push	hl
		ld	de,0			; first logical sector
		ld	hl,(SSECBUF)		; transfer address
		ld	b,1			; number of sectors is 1
		or	a			; carry flag cleared ==> read sector
		call	DSKIO
		pop	iy
		ret	c
		ld	ix,(SSECBUF)
		ld	a,(ix+$15)		; Media ID
		ld	(iy+$01),a
		ld	(iy+$02),$00		; Sector size is 0200h
		ld	(iy+$03),$02
		ld	(iy+$04),$0f		; Directory mask 00fh: 512/32-1
		ld	(iy+$05),$04		; Directory shift 004h
		ld	a,(ix+$0d)		; Cluster size (in sectors)
		dec	a
		ld	(iy+$06),a		; Cluster mask
		ld	c,$00
r601:		inc	c
		rra
		jr	c,r601
		ld	(iy+$07),c		; Cluster shift
		ld	l,(ix+$0e)		; Number of unused sectors
		ld	h,(ix+$0f)
		ld	(iy+$08),l		; FIRFAT - first FAT sector
		ld	(iy+$09),h
		ld	e,(ix+$16)		; Size of FAT (in sectors)
		ld	(iy+$10),e		; FATSIZ - Sectors per FAT
		ld	d,$00
		ld	b,(ix+$10)		; Number of FATs
		ld	(iy+$0A),b
r602:		add	hl,de
		djnz	r602
		ld	(iy+$11),l		; FIRDIR - First directory sector
		ld	(iy+$12),h
		ld	a,(ix+$12)		; Number of directory entries (high byte)
		ex	de,hl
		ld	h,a
		ld	l,(ix+$11)		; Number of directory entries (low byte)
		ld	bc,$000f
		add	hl,bc
		add	hl,hl
		add	hl,hl
		add	hl,hl
		add	hl,hl			; 16 directory entries per sector
		ld	l,h
		ld	h,$00
		ex	de,hl
		or	a			; number of directory entries < 256?
		jr	z,r603			; z=yes
		ld	a,$ff			; set max 255 directory entries
		jr	r604 
r603:		ld	a,(ix+$11)		; set max directory entries to directory entries low byte
r604:		ld	(iy+$0b),a		; MAXENT - Max directory entries
		add	hl,de
		ld	(iy+$0c),l		; FIRREC - first data sector
		ld	(iy+$0d),h
		ex	de,hl
		ld	l,(ix+$13)		; Total number of sectors
		ld	h,(ix+$14)
		ld	bc,$0000
		ld	a,l
		or	h
		jr	nz,r605
		ld	l,(ix+$20)
		ld	h,(ix+$21)
		ld	c,(ix+$22)
		ld	b,(ix+$23)
r605: 		or	a
		sbc	hl,de
		jr	nc,r606
		dec	bc
r606:		ld	a,(iy+$07)
r607:	  	dec	a
		jr	z,r608
		srl	b
		rr	c
		rr	h
		rr	l
		jr	r607
r608:		inc	hl
		ld	(iy+$0e),l		; MAXCLUS - number of clusters + 1
		ld 	(iy+$0f),h
		xor	a
		ret

; ------------------------------------------
; CHOICE - Choice for FORMAT 
; Input : None
; Output: HL = pointer to string, terminated by 0
; ------------------------------------------
CHOICE:		
IFDEF IDEDOS1	
; No choice: HL = 0
		xor	a
		ld	l,a
		ld	h,a
		ret
ELSE
; Cannot format this drive: (HL) = 0
		ld	hl,choice_txt
		ret
choice_txt:	db	$00
ENDIF

; ------------------------------------------
; DSKFMT - Format not implemented
; MTOFF - Motors off not implemented
; ------------------------------------------
DSKFMT:		
IFDEF IDEDOS1
; This routine will be called by DOS1 only
; Error $0c = Bad parameter
		ld	a,$0c
		scf
ENDIF
MTOFF:		ret

; -------------------------------------------
; OEMSTATEMENT - BASIC System statement expansion
; -------------------------------------------
OEMSTA:		scf
		ret

; ------------------------------------------
; Default DPB pattern (DOS 1)
; ------------------------------------------
DEFDPB:		db	$00		; +00 DRIVE	Drive number
		db	$f9		; +01 MEDIA	Media type
		dw	$0200		; +02 SECSIZ	Sector size
		db	$0f		; +04 DIRMSK	Directory mask
		db	$04		; +05 DIRSHFT	Directory shift
		db	$03		; +06 CLUSMSK	Cluster mask
		db	$03		; +07 CLUSSFT	Cluster shift
		dw	$0001		; +08 FIRFAT	First FAT sector
		db	$02		; +0A FATCNT	Number of FATs
		db	$70		; +0B MAXENT	Number of directory entries
		dw	$000e		; +0C FIRREC	First data sector
		dw	$02ca		; +0E MAXCLUS	Number of clusters+1
		db	$03		; +10 FATSIZ	Sectors per FAT
		dw	$0007		; +11 FIRDIR	First directory sector
		dw	$0000		; +12 FATPTR	FAT pointer

; ------------------------------------------
; Boot MSX-DOS from selected partition, to be used in a modified MSX-DOS boot process.
; MSX-DOS 1:
; 	The default boot drive is the last primary partition that is flagged active.
;	If DOS1 can't boot from the specified drive the machine will restart or start BASIC.
; MSX-DOS 2:
; 	The default boot drive is the first drive with a valid MSX-DOS 2 boot loader.
;	If DOS2 can't boot from the  specified drive the machine will boot from the first drive or start BASIC.
; ------------------------------------------
BOOTMENU:	ei
		call	GETWRK
		xor	a
		or	(ix+W_DRIVES)		; are there any IDE drives?
IFDEF IDEDOS1
		ret	z
		ld	a,(ix+W_BOOTDRV)
		ld	(CURDRV),a		
ELSE
		ld	a,(CUR_DRV)
		ret	z			; z=no IDE drives
		dec	a
ENDIF
		push	af
		call	PrintMsg
		db	"Boot  : ",0
		pop	af
		add	a,'A'
		rst	$18
		call	PrintMsg
		db	13,10,13,10,"Press drive key or [ESC] to cancel.. ",0
		ld	hl,(bootWait)		; get wait time from patch area
boot_r1:	push	hl
		call	SelectDrive
		pop	hl
		ret	c
		push	hl
		ld	hl,SNUMDR		; Number of drives in the system
		cp	(hl)
		pop	hl
		jr	c,boot_valid
		dec	hl
		ld	a,h
		or	l
		jr	nz,boot_r1
IFDEF IDEDOS1
		ld	a,(CURDRV)
boot_valid:	ld	(CURDRV),a
ELSE
		ld	a,(CUR_DRV)
		dec	a
boot_valid:	inc	a
		ld	(CUR_DRV),a
ENDIF
		push	af
		ld	a,$0c			; clear screen
		rst	$18
		ld	hl,$1000		; wait until boot key is released
boot_r2:	dec	hl
		ld	a,h
		or	l
		jr	nz,boot_r2
		call	KILBUF			; clear keyboard buffer
		pop	af
		or	a			; clear carry flag
		ret	

; ------------------------------------------------------------------------------
; *** Print and input subroutines ***
; ------------------------------------------------------------------------------
PrintMsg:	ex      (sp),hl
	        call    PrintString
        	ex      (sp),hl
	        ret

PrintString:	ld      a,(hl)
	        inc     hl
        	and     a
	        ret     z
        	rst	$18			; print character
	        jr      PrintString

; Print CR+LF
PrintCRLF:	ld	a,$0d
		rst	$18
		ld	a,$0a
		rst	$18
		ret

; ------------------------------------------
; Create decimal
; ------------------------------------------
MakeDec:	ld	b,$10
		ld	de,$0000
		xor	a
r731:		add	hl,hl
		ld	a,e
		adc	a,a
		daa
		ld	e,a
		ld	a,d
		adc	a,a
		daa
		ld	d,a
		djnz	r731
		ld	b,$04
		ld	a,d
		call	r732
		ld	a,e
r732:		push	af
		rrca
		rrca
		rrca	
		rrca
		call	r733
		pop	af
r733:		and	$0f
		jr	z,r735
r734:		ld	b,$01
		add	a,$30
		rst	$18			; print character
		ret
r735:		dec	b
		jr	z,r734
		ret

; -----------------------------------------
; Get drive character from keyboard
; Output: A=0..7 or ff if no key pressed
;         carry flag if ESC is pressed
; -----------------------------------------
SelectDrive:	call	CHSNS			; check keyboard buffer
		jr	z,nokey			; z=empty
		ld	a,$01
	        ld	(REPCNT),a		; not to wait until repeat
		call	CHGET           	; get a character (if exists)
		cp	$1b			; [ESC]
		scf
		ret	z
		cp	'A'
		jr	c,nokey
		cp	'I'
		jr	c,setdrive
		cp	'a'
		jr	c,nokey
		cp	'i'
		jr	nc,nokey
		sub	$20
setdrive:	sub	'A'
		ret
nokey:		or	$ff
		ret

; ------------------------------------------------------------------------------
; *** Driver patch area ***
; ------------------------------------------------------------------------------

		SECTION	DRV_PATCH
		ORG	$7FD0

		; Reserved
		ds	$2e,$00

bootWait:	dw	$5000		; 7FFE Boot menu wait time-out (default $5000 is appr. 3 sec for MSX/3.58Mhz)
