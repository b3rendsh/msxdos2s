; ------------------------------------------------------------------------------
; driver-ide.asm
;
; Copyright (C) 2024 H.J. Berends*
; * Part of the code is based on the BEER-202 driver by SOLiD and other
; PPI 8255 and 8-BIT CF IDE solutions publicly shared on the internet.
; 
; You can freely use, distribute or modify this program.
; It is provided freely and "as it is" in the hope that it will be useful, 
; but without any warranty of any kind, either expressed or implied.
; ------------------------------------------------------------------------------
; Universal IDE driver for MSX-DOS:
; + Can be used with PPI 8255 and CF IDE boards (BEER-232, BEER-202, SODA)
; + Extended partitions support
; + Up to 8 drives
; + FAT16 drives up to 2GB (max 255 root directory entries in DOS 1)
; + Boot menu
; + IDE hardware detection, if not present act as dummy driver
;
; Some differences with BEER 2.0 driver:
; + Appr. 20% faster
; + Improved error handling
; + Separate DOS layer and PPI/IDE hardware layer
; x Removed BEER 1.9 legacy dependencies
; ------------------------------------------------------------------------------

	        INCLUDE "disk.inc"	; Assembler directives
		INCLUDE	"msx.inc"	; MSX constants and definitions

		SECTION	DRIVER_IDE

		; Mandatory symbols defined by the disk hardware driver
		PUBLIC	INIHRD
		PUBLIC	DRIVES
		PUBLIC	INIENV
		PUBLIC	DSKIO
		PUBLIC	DSKCHG
		PUBLIC	GETDPB
		PUBLIC	CHOICE
		PUBLIC	DSKFMT
		PUBLIC	MTOFF
		PUBLIC	OEMSTA
		PUBLIC	DEFDPB
		PUBLIC	MYSIZE
		PUBLIC	SECLEN

		; Additional routine 
		PUBLIC	BOOTMENU	; Boot from partition that is flagged active

		; Disk routines used by driver
		EXTERN	GETWRK		; Get address of disk driver's work area
		EXTERN	GETSLT		; Get slot of this interface

MYSIZE		equ	46
SECLEN		equ	512

; ------------------------------------------------------------------------------
; *** DOS driver routines ***
; ------------------------------------------------------------------------------

; Structure of disk interface work area:
; $00	First absolute sector of partition #1
; $04	Partition type of partition #1
; ...
; $23	First absolute sector of partition #8
; $27	Partition type of partition #8
; 
; $28 - $2d work variables:
W_CURDRV	equ	$28	; Current drive
W_BOOTDRV	equ	$29	; Boot drive (partition)
W_DRIVES	equ	$2a	; Number of drives (partitions) on disk
W_RWFLAG	equ	$2b	; Read/Write flag
W_IODATA	equ	$2c	; IDE IO data port/register
W_IOCTL		equ	$2d	; IODE IO control port/register
	
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
PART_EXTLO	equ	PART_BUF+$402	; Extended partition sector offset low word
PART_EXTHI	equ	PART_BUF+$404	; Extended partition sector offset high word

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
		call	ideInit
		jr	nz,r207			; nz=ide hardware not detected
		ld	hl,PART_BUF		; Buffer address
		call	ideInfo
		jr	c,r207			; c=time-out
		call	PrintDiskInfo
		ld	hl,PART_BUF		; Buffer address
		call	ReadMBR
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
		call	xpart1
		ld	de,(PART_NEXT)		; Load pointer to next partition in workarea
		pop	hl
		pop	af
		ret

xpart1:		ld	(PART_EXTLO),de		; save extended partition sector offset
		ld	(PART_EXTHI),bc		; " 
		ld	hl,PART_BUFX		; set extended partition boot record buffer
		call	ReadBootRec
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
		ld	a,(PART_EXTLO)
		add	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EXTLO+1)
		adc	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EXTHI)
		adc	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,(PART_EXTHI+1)
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
		ld	de,(PART_EXTLO)
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
; INIENV - Initialize the work area (environment)
; Input : None
; Output: None
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------
INIENV:		call	GETWRK			; HL and IX point to work buffer
		xor	a
		or	(ix+W_DRIVES)		; number of drives 0?
		ret	z
		ld	(ix+W_CURDRV),$ff	; Init current drive
		call	GETSLT
		ld 	hl,DRVTBL
		ld	b,a			; B = this disk interface slot number
		ld	c,$00
r301:		ld	a,(hl)
		add	a,c
		ld	c,a
		inc	hl
		ld	a,(hl)
		inc	hl
		cp	b			; this interface?
		jr	nz,r301			; nz=no
		dec	hl
		dec	hl
		ld	a,c
		sub	(hl)
		ld	b,(ix+W_BOOTDRV)	; Get boot drive
		add	a,b
		ld	(ix+W_BOOTDRV),a	; Set boot drive
		call	PrintMsg
		db	"Drives: ",0
		ld	a,(ix+W_DRIVES)
		add	a,'0'
		rst	$18
		call	PrintCRLF
		; delay loop to view the driver info
		ld	b,$04			
r302:		ld	hl,$0000
r303:		dec	hl
		ld	a,h
		or	l
		jr	nz,r303
		djnz	r302
		ret

; ------------------------------------------
; DSKIO - IDE Hard Disk Read/Write
; Input:
;   Carry flag = clear ==> read, set ==> write
;   A  = drive number
;   B  = number of sectors to transfer
;   C  = if bit7 is set then media descriptor byte
;        else first logical sector number bit 16..22
;   DE = first logical sector number (bit 0..15)
;   HL = transfer address
; Output:
;   Carry flag = clear ==> successful, set ==> error
;   If error then 
;     A = error code
;     B = remaining sectors
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------
DSKIO:		ei
		push	hl
		push	de
		push	bc
		push	af
		cp	$08			; Max 8 drives (partitions) supported
		jr	nc,r404
		call	GETWRK			; base address of workarea in hl and ix
		pop	af
		jr	c,write_flag
		ld	(ix+W_RWFLAG),$00
		jr	r400
write_flag:	ld	(ix+W_RWFLAG),$01
r400:		ld	e,a
		add	a,a
		add	a,a
		add	a,e
		ld	e,a			; a * 5
		ld	d,$00
		add	hl,de
		push	hl
		pop	iy
		pop	bc
		pop	de
		pop	hl
		xor	a
		or	(iy+$04)		; Test if partition exists (must have nonzero partition type)
		jr	z,r405

		; translate logical to physical sector number
		push	bc			; save sector counter
		push 	hl			; save transfer address
		ex	de,hl
		bit	7,c			; if bit 7 of media descriptor is 0 then use 23-bit sector number
		jr	nz,r401			; nz if 16-bit sector number
		ld	e,c			; bit 16-22 of sector number
		ld	d,$00
		jr	r402
r401:		ld	de,$0000
r402:		ld	c,(iy+$00)
		ld	b,(iy+$01)
		add	hl,bc
		ex	de,hl			; LBA address: de=00..15
		ld	c,(iy+$02)
		ld	b,(iy+$03)
		adc	hl,bc
		ld	c,l			; LBA address: c=16..23
		pop	hl			; restore transfer address
		pop	af			; restore sector counter
		ld	b,a			; "

		; IDE read/write sector command with more than 1 sector is not supported by all disks
rw_loop:	call	ideSetSector
		jr	c,r405
		push	bc
		push	de
		call	rw_sector
		pop	de
		pop	bc
		jr 	c,r405
		inc	e
		jr	nz,r403
		inc	d
		jr	nz,r403
		inc	c
r403:		djnz	rw_loop
		xor	a
		ret

		; Disk i/o error
r404:		pop	af
		pop	bc
		pop	de
		pop	hl
r405:		ld	a,$04			; Error 4 = Data (CRC) error (abort,retry,ignore message)
		scf
		ret

rw_sector:	ld	a,(ix+W_RWFLAG)		; get read/write flag
		or	a
		jr	nz,dosWriteSector

; ------------------------------------------
dosReadSector:	call	ideCmdRead
		jp	nz,ideError
		bit	7,h			; store data in ram page 2 or 3?
		push	af
		push	hl
		jr	nz,rd01			; nz=yes, directly store data in destination
		ld	hl,(SSECBUF)		; init temporary buffer pointer
rd01:		call	ideReadSector
		pop	de
		pop	af
		jr	nz,rd03
		ld	hl,(SSECBUF)
		ld	bc,$0200		; sector size
		call	XFER
		ex	de,hl
rd03:		xor	a			; reset c-flag: no error
		ret

; ------------------------------------------
dosWriteSector:	call	ideCmdWrite
		jp	nz,ideError
		push	hl
		bit	7,h			; read data from ram page 2 or 3?
		jr	nz,wr01			; nz=yes, directly read data from source
		ld	de,(SSECBUF)		; copy source to temporary buffer
		ld	bc,$0200		; sector size
		call	XFER
		ld	hl,(SSECBUF)		; init buffer pointer
wr01:		call	ideWriteSector
		pop	hl
		inc	h
		inc	h
		xor	a			; reset c-flag: no error
		ret

; ------------------------------------------
; DSKCHG - Disk change 
; Input:
;   A  = Drive number
;   B  = 0
;   C  = Media descriptor
;   HL = Base address of DPB
; Output:
;   If successful then
;     Carry flag reset
;     B = Disk change status
;         1= Disk unchanged, 0= Unknown, -1=Disk changed
;   else
;     Carry flag set
;     Error code in A
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------
DSKCHG:		push	af
		call	GETWRK
		pop	af
		cp	(ix+W_CURDRV)		; current drive
		ld	(ix+W_CURDRV),a
		jr	nz,r501
		ld	b,$01			; unchanged
		xor	a
		ret

r501:		ld	b,$FF			; changed
		xor	a
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
; The boot menu time-out is appr. 4 seconds in a MSX/3.58Mhz machine.
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
		ret	z			; z=no IDE drives
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
		ld	hl,$7800		; appr. 4 seconds
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
; *** Driver subroutines ***
; ------------------------------------------------------------------------------

; ------------------------------------------
; Print IDE disk information
; Input: PART_BUF = info record
; ------------------------------------------
PrintDiskInfo:	push	af
		push	hl
		call	PrintMsg
		db	12
IFDEF PPIDE
		db	"BEER  : PPI IDE "
ELSE
		db	"SODA  : CF IDE "
ENDIF
IFDEF IDEDOS1
		db	"DOS 1",13,10,0
ELSE
		db	"DOS 2",13,10,0
ENDIF
		call	PrintMsg
		db	"Rev.  : "
		INCLUDE	"rdate.inc"		; Revision date
		db	13,10,0
		call	PrintMsg
		db	"Master: ",0
		ld	c,$4d
		ld	hl,(PART_BUF+$79)
		ld	a,(PART_BUF+$7b)
		srl	a
		rr	h
		rr	l
		srl	a
		rr	h
		rr	l
		srl	a
		rr	h
		rr	l
		or	a
		jr	z,r721
		ld	c,$47
		ld	l,h
		ld	h,a
		srl	h
		rr	l
		srl	h
		rr	l
r721:		call	MakeDec
		ld	a,c
		rst	$18
		call PrintMsg
		db	13,10,"        ",0
		ld	hl,PART_BUF+$36		; +$2e=firmware +$36=model
		ld	b,$0a
r722:		inc	hl
		ld	a,(hl)
		rst	$18
		dec	hl
		ld	a,(hl)
		rst	$18
		inc	hl
		inc	hl
		djnz	r722
		call	PrintCRLF
		pop	hl
		pop	af
		ret

; ------------------------------------------
; Read Master Boot Record / Boot Sector
; ------------------------------------------
ReadMBR:	xor	a
		ld	e,a
		ld	d,a
		ld	c,a			; sector address = 0 (24 bits) 

ReadBootRec:	call	ideSetSector
		ret	c
		jp	dosReadSector

; ------------------------------------------
; DOS error code handler
; Input: A = IDE error
; ------------------------------------------
dosError:	ld	l,a
		ld	a,$08			; error $08: sector not found
		rr	l
		ret	c			; bit 0: address mark not found --> error $08
		rr	l
		ret	c			; bit 1: track 0 not found --> error $08
		ld	a,$0c			; error $0c: disk error
		rr	l
		ret	c			; bit 2: abort, wrong command --> error $0c
		rr	l
		ld	a,$08
		rr	l
		ret	c			; bit 4: sector id not found --> error $08
		rr	l
		ld	a,$04			; error $04: CRC error
		rr	l
		ret	c			; bit 6: uncorrectable data error --> error $04
		ld	a,$02			; error $02: not ready
		scf
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
; *** IDE hardware interface ***
;
; Mandatory routines:
; 0  ideInit		Initialize disk and set features
; 1  ideInfo		Get information block
; 2  ideSetSector	Set 24-bit sector number
; 3  ideCmdRead		Set command for sector read
; 4  ideReadSector	Read 512 bytes of data
; 5  ideCmdWrite	Set command for sector write
; 6  ideWriteSector	Write 512 bytes of data
; 7  ideWaitReady	Wait for IDE ready
; 8  ideWaitData	Wait for IDE request data
; 9  ideError		IDE error handling
;
; Uses routine:
; dosError		DOS driver error handler
;
; The IX register must point to the driver work buffer for all these routines
; ------------------------------------------------------------------------------
IDE_CMD_READ	equ	$20		; read sector
IDE_CMD_WRITE	equ	$30		; write sector
IDE_CMD_DIAG	equ	$90		; diagnostic test
IDE_CMD_INFO	equ	$ec		; disk info
IDE_CMD_FEATURE	equ	$ef		; set feature

IDE_READ	equ	$40		; /rd=0 /wr=1 /cs=0
IDE_WRITE	equ	$80		; /rd=1 /wr=0 /cs=0
IDE_SET		equ	$c0		; /rd=1 /wr=1 /cs=0 
IDE_IDLE	equ	$e7		; /rd=1 /wr=1 /cs=1 reg=7

; PPI 8255 I/O registers:
PPI_IOA		equ	$30		; A: IDE data low byte
PPI_IOB		equ	$31		; B: IDE data high byte
PPI_IOC		equ	$32		; C: IDE control
PPI_CTL		equ	$33		; PPI control

; PPI 8255 settings:
PPI_INPUT	equ	$92		; Set PPI A+B to input
PPI_OUTPUT	equ	$80		; Set PPI A+B to output

; IDE registers:
REG_DATA	equ	$00	; r/w
REG_ERROR	equ	$01	; r
REG_FEATURE	equ	$01	; w
REG_COUNT	equ	$02	; r/w
REG_LBA0	equ	$03	; r/w
REG_LBA1	equ	$04	; r/w
REG_LBA2	equ	$05	; r/w
REG_LBA3	equ	$06	; r/w
REG_STATUS	equ	$07	; r
REG_COMMAND	equ	$07	; w
REG_CONTROL	equ	$07	; r/w

IFDEF PPIDE
; ------------------------------------------------------------------------------
; *** PPI 8255 IDE routines ***
; ------------------------------------------------------------------------------
; PPI IDE control bit:
; 0	IDE register bit 0
; 1	IDE register bit 1
; 2	IDE register bit 2
; 3	Not used
; 4	Not used
; 5	/CS Select
; 6	/WR Write data
; 7	/RD Read data

		SECTION	PPIDE

; ------------------------------------------
; Initialize disk
; Output: Z-flag set if hardware detected
; probe for PPI 8255 hardware on fixed IO port
; ------------------------------------------
ideInit:	ld	a,PPI_IOA
		ld	(ix+W_IODATA),a
		ld	a,PPI_CTL
		ld	(ix+W_IOCTL),a
		call	ppideOutput
		ld	hl,$00a5		; register=0 value=a5
		call	ppideSetReg
		in	a,(PPI_IOA)
		cp	$a5
		ret

; ------------------------------------------
; Get IDE device information 
; ------------------------------------------
ideInfo:	call	ideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		call	ppideCommand
		call	ideWaitData
		jp 	nz,ideError
		call	ideReadSector
		xor	a
		ret

; ------------------------------------------
; IDE set sector start address and number of sectors
; Input: C,D,E = 24-bit sector number
; ------------------------------------------
ideSetSector:	call	ideWaitReady
		ret	c
		call	ppideOutput
		push	hl
		ld	h,$02			; IDE register 2
		ld	l,$01			; number of sectors is 1
		call	ppideSetReg
		inc	h			; IDE register 3
		ld	l,e			; bit 0..7
		call	ppideSetReg
		inc	h			; IDE register 4
		ld	l,d			; bit 8..15
		call	ppideSetReg
		inc	h			; IDE register 5
		ld	l,c			; bit 16..23
		call	ppideSetReg
		inc	h			; IDE register 6
		ld	l,$e0			; LBA mode
		call	ppideSetReg 
		pop	hl
		xor	a
		ret

; ------------------------------------------
; IDE set command read sector
; ------------------------------------------
ideCmdRead:	push	bc
		ld 	a,IDE_CMD_READ
		call	ppideCommand
		call	ideWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Read Sector
; Input: HL = transfer address
; ------------------------------------------
ideReadSector:	ld	a,PPI_IOA
		ld	b,$80			; counter (decreases by 5 in 4-byte loop)
		ld	c,PPI_IOC
		ld	d,IDE_READ
		ld	e,IDE_SET
		out	(c),e
rdsec_loop:	out	(c),d			; IDE read
		ld	c,a			; PPI port A
		ini				; read low byte, increase bufferpointer, decrease counter
		inc	c			; PPI port B
		ini				; read high byte, increase bufferpointer, decrease counter
		inc	c			; PPI port C
		out	(c),e			; IDE idle
		; repeat 2-byte read
		out	(c),d
		ld	c,a
		ini
		inc	c
		ini
		inc	c
		out	(c),e
		djnz	rdsec_loop		; 640 MOD 5 = 0
		ret

; ------------------------------------------
; IDE set command write sector
; ------------------------------------------
ideCmdWrite:	push	bc
		ld	a,IDE_CMD_WRITE
		call	ppideCommand

		; hardware design flaw: the control signals between ppi and ide should be inverted
		; because changing the ppi mode resets the output ports
		; workaround: small delay
		ld	b,$30
wr_wait:	ex	(sp),hl
		ex	(sp),hl
		djnz	wr_wait
		xor	a

		pop	bc
		ret

; ------------------------------------------
; IDE Write Sector
; Input: HL = transfer address
; ------------------------------------------
ideWriteSector:	ld	a,PPI_IOA
		ld	b,$80			; counter: 128x4=512 bytes
		ld	c,PPI_IOC
		ld	d,IDE_WRITE
		ld	e,IDE_SET
		out	(c),e
wrsec_loop:	ld	c,a
		outi
		inc	c
		outi
		inc 	c
		out	(c),d
		out	(c),e
		; repeat 2-byte write
		ld	c,a
		outi
		inc	c
		outi
		inc 	c
		out	(c),d
		out	(c),e
		djnz	wrsec_loop
		ret

; ------------------------------------------
; Wait for IDE ready or time-out
; ------------------------------------------
ideWaitReady:	push	hl
		push	bc
		call	ppideInput
		ld	b,$14			; time-out after 20 seconds
wait_1:		ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
wait_2:		call	ppideStatus
		and	%11000000
		cp	%01000000		; BUSY=0 RDY=1 ?
		jr	z,wait_end		; z=yes
		dec	hl
		ld	a,h
		or	l
		jr	nz,wait_2
		djnz	wait_1
		scf				; time-out
wait_end:	pop	bc
		pop	hl
		ret

; ------------------------------------------
; Wait for IDE data read/write request
; ------------------------------------------
ideWaitData:	call 	ppideInput
waitdata_1:	call	ppideStatus
		bit	7,a			; IDE busy?
		jr	nz,waitdata_1		; nz=yes
		bit	0,a			; IDE error?
		ret	nz			; nz=yes
		bit	3,a			; IDE data request?
		jr	z,waitdata_1		; z=no
		xor	a			; no error
		ret

; ------------------------------------------
; IDE error handling
; ------------------------------------------
ideError:	ld	a,IDE_SET+REG_ERROR
		call	ppideReadReg
		jp	dosError

; ------------------------------------------
; PPI IDE read status register
; ------------------------------------------
ppideStatus:	ld	a,IDE_SET+REG_STATUS
ppideReadReg:	out	(PPI_IOC),a
		res	7,a			; /rd=0
		out	(PPI_IOC),a
		in	a,(PPI_IOA)		; read register
		ex	af,af'
		ld	a,IDE_IDLE
		out	(PPI_IOC),a
		ex	af,af'
		ret

; ------------------------------------------
; PPI IDE set command
; Input:  A = command
; ------------------------------------------
ppideCommand:	call	ppideOutput
		push	hl
		ld	h,REG_COMMAND
		ld	l,a
		call	ppideSetReg
		pop	hl
		ret

; ------------------------------------------
; PPI IDE set register
; Input:  H = register
;         L = value
; ------------------------------------------
ppideSetReg:	ld	a,IDE_SET
		add	a,h
		out	(PPI_IOC),a
		ld	a,l
		out 	(PPI_IOA),a
		ld 	a,IDE_WRITE
		add	a,h
		out 	(PPI_IOC),a
		ld	a,IDE_IDLE
		out 	(PPI_IOC),a
		ret 

; ------------------------------------------
; PPI IDE set data direction
; Changing the mode on a 8255 will reset ports A,B and C to 0.
; After channging direction the control lines on PPI Port C 
; must be set before any read/write to IDE registers.
; ------------------------------------------
ppideInput:	ex	af,af'
		ld	a,PPI_INPUT		; PPI A+B is input
		out	(PPI_CTL),a
		ex	af,af'
		ret

ppideOutput:	ex	af,af'
		ld	a,PPI_OUTPUT		; PPI A+B is output
		out	(PPI_CTL),a
		ex	af,af'
		ret

; END PPIDE

ELIFDEF CFIDE
; ------------------------------------------------------------------------------
; *** Compact Flash 8-BIT IDE routines ***
; ------------------------------------------------------------------------------

		SECTION	CFIDE

; ------------------------------------------
; Initialize disk
; Output: Z-flag set if hardware detected
; ------------------------------------------
ideInit:	; probe for CF IDE hardware
		ld	hl,idePorts
ideProbe:	ld	a,(hl)
		cp	$ff
		jr	z,ideNotDetected
		call	ideInit1
		ret	z
		inc	hl
		jr	ideProbe

ideInit1:	add	a,$03
		ld	c,a
		ld	a,$aa
		out	(c),a
		ld	a,$55
		inc	c
		out	(c),a
		in	a,(c)
		cp	$55
		ret	nz
		dec	c
		in	a,(c)
		cp	$aa
		ret	nz

		ld	a,(hl)
		ld	(ix+W_IODATA),a		; CF IDE IO Data
		add	a,REG_CONTROL
		ld	(ix+W_IOCTL),a		; CF IDE IO Command/Status


		; Set IDE feature to 8-bit
		call	ideWaitReady
		ret	c			; time-out
		ld	a,(ix+W_IODATA)
		add	a,REG_FEATURE
		ld	c,a
		ld	a,$01
		out	(c),a
		ld	a,(ix+W_IODATA)
		add	a,REG_LBA3
		ld	c,a
		ld	a,$e0			; LBA mode / device 0
		out	(c),a
		ld	a,IDE_CMD_FEATURE
		ld	c,(ix+W_IOCTL)
		out	(c),a
		xor	a			; z=detected
		ret

ideNotDetected:	or	a			; nz=not detected
		ret

; List of ports that are probed, end with $ff
idePorts:	db	$30,$38,$10,$ff

; ------------------------------------------
; Get IDE device information 
; ------------------------------------------
ideInfo:	call	ideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		ld	c,(ix+W_IOCTL)
		out	(c),a
		call	ideWaitData
		jp 	nz,ideError
		call	ideReadSector
		xor	a
		ret

; ------------------------------------------
; IDE set sector start address and number of sectors
; Input: C,D,E = 24-bit sector number
; ------------------------------------------
ideSetSector:	call	ideWaitReady
		ret	c
		push	hl
		push	bc
		ld	a,c
		ex	af,af'
		ld	a,(ix+W_IODATA)
		add	a,$02			; IDE register 2
		ld	c,a
		ld	a,$01			; number of sectors is 1
		out	(c),a
		ex	af,af'
		inc	c			; IDE register 3
		out	(c),e			; bit 0..7
		inc	c			; IDE register 4
		out	(c),d			; bit 8..15
		inc	c			; IDE register 5
		out	(c),a			; bit 16..23
		inc	c			; IDE register 6
		ld	a,$e0			; LBA mode
		out	(c),a
		pop	bc
		pop	hl
		ret

; ------------------------------------------
; IDE set command read sector
; ------------------------------------------
ideCmdRead:	push	bc
		ld 	a,IDE_CMD_READ
		ld	c,(ix+W_IOCTL)
		out	(c),a
		call	ideWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Read Sector
; Input: HL = transfer address
; ------------------------------------------
ideReadSector:	ld	b,$20			; counter: 32x16=512 bytes (decreases by 17 in 16 byte loop)
		ld	c,(ix+W_IODATA)		; IO port
rdsec_loop:	
		REPT 16				; repeat: read 16 bytes
		ini				; 16x ini in a loop is appr.20% faster than inir
		ENDR
		djnz	rdsec_loop		; 2x256+32 = 32x17 (32 and 256+32 are not divisible by 17)
		ret

; ------------------------------------------
; IDE set command write sector
; ------------------------------------------
ideCmdWrite:	push	bc
		ld 	a,IDE_CMD_WRITE
		ld	c,(ix+W_IOCTL)
		out	(c),a
		call	ideWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Write Sector
; Input: HL = transfer address
; ------------------------------------------
ideWriteSector:	ld	b,$20
		ld	c,(ix+W_IODATA)
wrsec_loop:	
		REPT 16
		outi
		ENDR
		djnz	wrsec_loop
		ret

; ------------------------------------------
; Wait for IDE ready or time-out
; ------------------------------------------
ideWaitReady:	push	hl
		push	bc
		ld	b,$14			; time-out after 20 seconds
wait_1:		ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
wait_2:		ld	c,(ix+W_IOCTL)
		in	a,(c)
		and	%11000000
		cp	%01000000		; BUSY=0 RDY=1 ?
		jr	z,wait_end		; z=yes
		dec	hl
		ld	a,h
		or	l
		jr	nz,wait_2
		djnz	wait_1
		scf				; time-out
wait_end:	pop	bc
		pop	hl
		ret

; ------------------------------------------
; Wait for IDE data read/write request
; ------------------------------------------
ideWaitData:	ld	c,(ix+W_IOCTL)
		in	a,(c)
		bit	7,a			; IDE busy?
		jr	nz,ideWaitData		; nz=yes
		bit	0,a			; IDE error?
		ret	nz			; nz=yes
		bit	3,a			; IDE data request?
		jr	z,ideWaitData		; z=no
		xor	a			; no error
		ret

; ------------------------------------------
; IDE error handling
; ------------------------------------------
ideError:	ld	a,(ix+W_IODATA)
		add	a,REG_ERROR
		ld	c,a
		in	a,(c)
		jp	dosError


; END CFIDE

ELSE
; ------------------------------------------------------------------------------
; *** Dummy driver routines ***
; ------------------------------------------------------------------------------
ideInit:	xor	a
		inc	a	; nz=no hardware
ideInfo:
ideSetSector:
ideCmdRead:
ideReadSector:
ideCmdWrite:
ideWriteSector:
ideWaitReady:
ideWaitData:
ideError:	ret

ENDIF 

; ------------------------------------------------------------------------------
; *** IDE hardware driver jump table ***
; ------------------------------------------------------------------------------

IFDEF ATAPI_TABLE
		SECTION	ATAPI_TABLE
		ORG	$7F80

; Structure retrieved from SUNRISE IDE driver sources
; SCSI-BIOS has "HD!" or "HD#"
; Todo:
; What is the specification for these routines (function,input,output)?
; Maybe usefull when implementing atapi support?

		db	"ID"		; IDE-BIOS indentifier
		db	$10		; version 1.0
	        jp	ideInit		; initialise SCSI controller
		db	$c9,$00,$00	; terminate hdd actions
		jp	dosReadSector	; read logical blocks
		jp	dosWriteSector	; write logical blocks
		db	$c9,$00,$00	; request sense
		jp	ideInfo		; inquiry
		db	$c9,$00,$00	; read capacity
		db	$c9,$00,$00	; mode sense
		db	$c9,$00,$00	; mode select
		db	$c9,$00,$00	; format unit
		jp	ideWaitReady	; test unit ready
		db	$c9,$00,$00	; initialise
		db	$c9,$00,$00	; install workspace
		db	$c9,$00,$00	; clear to end of line
		db	$c9,$00,$00	; verify
		db	$c9,$00,$00	; start/stop unit
		db	$c9,$00,$00	; send diagnostic
		db	$001,$00,$00	; IDE-BIOS version 1.00
		db	$c9,$00,$00	; select device for atapi packet
		db	$c9,$00,$00	; issue atapi packet
		db	$c9,$00,$00	; partitionpointer

ENDIF

; IDE jump table (under construction, subject to change!)

		SECTION	IDE_TABLE
		ORG	$7FD0

		jp  	ideInit
		jp  	ideInfo
		jp  	ideSetSector
		jp  	ideCmdRead
		jp  	ideReadSector
		jp  	ideCmdWrite
		jp  	ideWriteSector
		jp	ideWaitReady
		jp	ideWaitData
		jp	ideError

