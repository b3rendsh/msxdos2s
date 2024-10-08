; ------------------------------------------------------------------------------
; DRIVER-BEER232.ASM
;
; Copyright (C) 2024 H.J. Berends*
; * Part of the code is based on the BEER-202 driver by SOLiD.
; 
; You can freely use, distribute or modify this program.
; It is provided freely and "as it is" in the hope that it will be useful, 
; but without any warranty of any kind, either expressed or implied.
; ------------------------------------------------------------------------------
; Alternative driver for the BEER-232 or BEER-202 interface:
; + Extended partitions support
; + Up to 8 drives
; + FAT16 drives up to 2GB (DOS 1 maximum 32MB)
; + Appr. 15% faster
; + Improved error handling
; + Separate DOS layer and PPI/IDE hardware layer
; x Removed BEER 1.9 legacy dependencies
; Wishlist:
; + boot menu
; + master/slave
; + atapi/cdrom?
; + diagnostics?
; ------------------------------------------------------------------------------

		SECTION	DRIVER_BEER232

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

		; Additional routine (for use with MSX-DOS 1) 
		PUBLIC	HDDBOOT		; Boot from partition that is flagged active

		; Disk routines used by driver
		EXTERN	GETWRK		; Get address of disk driver's work area
		EXTERN	GETSLT		; Get slot of this interface
		EXTERN	XFER		; Helper routine to store data in ram page 1

MYSIZE		EQU	44
SECLEN		EQU	512

; MSX labels
SNSMAT		EQU	$0141
PHYDIO		EQU	$0144

CURDRV		EQU	$f247		; default drive
SSECBUF		EQU	$f34d		; pointer to sectorbuffer, can be used by the disk hardware driver
SDIRBU		EQU	$f351		; pointer to directorysectorbuffer
DRVTBL  	EQU     $fb21		; disk interface drive table

; ------------------------------------------------------------------------------
; *** DOS driver routines ***
; ------------------------------------------------------------------------------
;
; Structure of disk interface work area:
; +00,4  $00	First absolute sector of partition #1
; +04	 $04	Partition type of partition #1
; +05,4	 $05	First absolute sector of partition #2
; +09	 $09	Partition type of partition #2
; +10,4	 $0a	First absolute sector of partition #3
; +14	 $0e	Partition type of partition #3
; +15,4	 $0f	First absolute sector of partition #4
; +19 	 $13	Partition type of partition #4
; +20,4	 $14	First absolute sector of partition #5
; +24	 $18	Partition type of partition #1
; +25,4	 $19	First absolute sector of partition #6
; +29	 $1d	Partition type of partition #2
; +30,4	 $1e	First absolute sector of partition #7
; +34	 $22	Partition type of partition #3
; +35,4	 $23	First absolute sector of partition #8
; +39 	 $27	Partition type of partition #4
; +40 	 $28	Current drive
; +41 	 $29	Boot partition
; +42	 $2a	Number of drives on disk
; +43	 $2b	Read/write flag

; ------------------------------------------
; INIHRD - Initialize the disk / output message
; Input : None
; Output: None
; May corrupt: AF,BC,DE,HL,IX,IY 
; ------------------------------------------
INIHRD:		ld	a,$06
		call	SNSMAT			; Check if CTRL key is pressed
		di				
		and	2
		jr	z,r103			; z=yes: exit disk init
		ld	hl,$d000
		call	ideInfo
		jr	c,r103			; c=time-out
		call	OutputLogo
		ld	b,$04			; delay loop to view the message
r101:		ld	hl,$0000
r102:		dec	hl
		ld	a,h
		or	l
		jr	nz,r102
		djnz	r101
		xor	a
		ret
r103:		inc	sp
		inc	sp
		ret

; ------------------------------------------
; DRIVES - Get number of drives connected
; A maximum of 8 partitions (drives) is supported.
; Input:
;   F = The zero flag is reset if one physical drive must act as two logical drives.
; Output:
;   L = Number of drives connected. A value of 0 is not allowed.
; May corrupt: F,HL,IX,IY
;
; The DRIVES routine will also initialize the work environment
; Temporary RAM variables:
; $9000,$0200	Copy of Master Boot Record
; $9200,$0200	Copy of extended partition boot record
; $9400,$0002	Pointer to next partition in work area
; $9402,$0002	Extended partition sector offset low word
; $9404,$0002	Extended partition sector offset high word
; ------------------------------------------
DRIVES:		push	af
		push	bc
		push	de
		ld	hl,$9000	; Buffer address
		call	ReadMBR
		jr	c,r206
		ld	a,($91fe)	; Validate boot signature (AA 55)
		cp	$55
		jr	nz,r206
		ld	a,($91ff)
		cp	$aa
		jr	nz,r206

		; initialize work buffer
		call	GETWRK		; HL and IX point to work buffer
		push	hl
		ld	d,h
		ld	e,l
		inc	de
		ld	(hl),$00
		ld	bc,MYSIZE-1
		ldir
		pop	de		; DE now points to work bufer filled with zeros

		ld	hl,$91be	; Start of partition table
		ld	b,$04		; max 4 primary partitions
r201:		ld	c,(hl)		; Save status byte (active/inactive)
		push	bc
		inc	hl		; Skip CHS address information of first sector
		inc	hl
		inc	hl
		inc	hl
		ld	a,(hl)		; Load partition type
		call	PartitionType
		jr	z,r202
		call	PartitionExt
		call	z,xpart
		ld	bc,$000c
		add	hl,bc		; Move to next partition
		pop	bc
		jr	r204
r202:		inc	hl		; Skip CHS address information of last sector
		inc	hl
		inc	hl
		inc	hl
		ldi			; Get LBA - first absolute sector in partition (4 bytes)
		ldi
		ldi
		ldi
		ld	(de),a		; Partition type
		inc	de
		inc	hl		; Skip LBA number of sectors (4 bytes) / move to next partition
		inc	hl
		inc	hl
		inc	hl
		pop	bc
		ld	a,c		; Restore status byte
		cp	$80		; Is active partition (and valid type)?
		jr	nz,r203
		ld	a,(ix+$2a)
		ld	(ix+$29),a	; Update boot drive
r203:		inc	(ix+$2a)	; Increase partition count
r204:		ld	a,(ix+$2a)
		cp	$08		; Maximum partitions processed?
		jr	nc,r205
		djnz	r201		; process next primary partition

r205:		ld	a,(ix+$2a)
		or	a
		jr	nz,r207
r206:		ld	a,$01		; minimum is 1
r207:		ld	l,a		; set number of drives
		pop	de
		pop	bc
		pop	af
		ret

; Process extended partitions
xpart:		ld	($9400),de	; save pointer to next partition in workarea
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
		ld	de,($9400)	; Load pointer to next partition in workarea
		pop	hl
		pop	af
		ret

xpart1:		ld	($9402),de	; save extended partition sector offset
		ld	($9404),bc	; " 
		ld	hl,$9200	; set extended partition boot record buffer
		call	ReadBootRec
		ret	c
		ld	a,($93fe)	; Validate boot signature (AA 55)
		cp	$55
		ret	nz
		ld	a,($93ff)
		cp	$aa
		ret	nz
		ld	a,($93c2)	; Partition type of first entry
		call	PartitionType
		jr	nz,r222
r221:		ld	hl,$93c6	; First sector in partition
		ld	de,($9400)	; Load pointer to next partition in workarea
		ld	a,($9402)
		add	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,($9403)
		adc	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,($9404)
		adc	a,(hl)
		ld	(de),a
		inc	hl
		inc	de
		ld	a,($9405)
		adc	a,(hl)
		ld	(de),a
		inc	de
		ld	a,($93c2)	; load partition type
		ld	(de),a		
		inc	de
		ld	($9400),de	; Save pointer to next partition in workarea
		inc	(ix+$2a)	; Increase partition counter
		ld	a,(ix+$2a)
		cp	$08		; Maximum partitions processed?
		ret	z		; z=yes
r222:		ld	a,($93d2)	; Partition type of 2nd entry
		call	PartitionExt
		ret	nz		; End of chain
		ld	hl,$93d6	; pointer to sector number of next extended partition
		ld	de,($9402)
		ld	bc,($9404)
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
PartitionType:	cp	$01		; FAT12
		ret	z
		cp	$04		; FAT16 (<=32MB)
		ret	z
		cp	$06		; FAT16B (>32MB)
		ret	z
		cp	$0e		; FAT16B with LBA
		ret

; Validate extended partition type
PartitionExt:	cp	$05		; Extended partition (CHS,LBA)
		ret	z
		cp	$0f		; Extended partition (LBA)
		ret
	
; ------------------------------------------
; INIENV - Initialize the work area (environment)
; Input : None
; Output: None
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------
INIENV:		call	GETWRK		; HL and IX point to work buffer
		ld	(ix+$28),$ff	; Init current drive
		call	GETSLT
		ld 	hl,DRVTBL
		ld	b,a		; B = this disk interface slot number
		ld	c,$00
r301:		ld	a,(hl)
		add	a,c
		ld	c,a
		inc	hl
		ld	a,(hl)
		inc	hl
		cp	b		; this interface?
		jr	nz,r301		; nz=no
		dec	hl
		dec	hl
		ld	a,c
		sub	(hl)
		ld	b,(ix+$29)	; Get boot drive
		add	a,b
		ld	(ix+$29),a	; Set boot drive
		call	PrintMsg
		db	"Drives  : ",0
		ld	a,(ix+$2a)
		add	a,'0'
		rst	$18
		call	PrintCRLF
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
		push	hl
		pop	iy			; save copy of workarea base address
		pop	af
		jr	c,write_flag
		ld	(iy+$2b),$00
		jr	r400
write_flag:	ld	(iy+$2b),$01
r400:		ld	e,a
		add	a,a
		add	a,a
		add	a,e
		ld	e,a
		ld	d,$00
		add	hl,de
		push	hl
		pop	ix
		ld	a,(ix+$00)		; Test if partition exist (must have
		or	(ix+$01)		; nonzero start cylinder)
		or	(ix+$02)
		or	(ix+$03)
		pop	bc
		pop	de
		pop	hl
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
r402:		ld	c,(ix+$00)
		ld	b,(ix+$01)
		add	hl,bc
		ex	de,hl			; LBA address: de=00..15
		ld	c,(ix+$02)
		ld	b,(ix+$03)
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

rw_sector:	ld	a,(iy+$2b)		; get read/write flag
		or	a
		jp	z,ideReadSector
		jp	ideWriteSector

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
		cp	(ix+$28)	; current drive
		ld	(ix+$28),a
		jr	nz,r501
		ld	b,$01		; unchanged
		xor	a
		ret

r501:		ld	b,$FF		; changed
		xor	a
		ret

; ------------------------------------------
; GETDPB - Set DPB using sector 0 / bootsector of partition
; Called by DOS 1 only, not used by DOS 2.2
; Input:
;   A  = Drive number
;   B  = First byte of FAT
;   C  = Media descriptor
;   HL = Base address of BPB
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
		add	hl,hl
		ld	l,h
		ld	h,$00
		ex	de,hl
		jr	z,r603
		ld	a,$ff			; Max 255 directory entries
		jr	r604
r603:		ld	a,(iy+$11)
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
CHOICE:		xor	a
		ld	l,a
		ld	h,a
		ret

; ------------------------------------------
; DSKFMT - Format not implemented
; ------------------------------------------
DSKFMT:		call	PrintMsg
		db	13,"IDE format not implemented",13,10,0
		ret

; ------------------------------------------
; MTOFF - Motors off not implemented
; ------------------------------------------
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

; ------------------------------------------
; Boot MSX-DOS from HDD 
; For use in a modified MSX-DOS boot routine
; ------------------------------------------
HDDBOOT:	di
		ld	a,$07
		call	SNSMAT
		and	$40		; If [SELECT] key pressed then do not boot 
		scf
		ret	z
		call	GETWRK
		ld	a,(ix+$29)
		ld	(CURDRV),a

		; todo: boot menu
		;call	PrintMsg
		;db	"Boot drv: ",0
		;ld	a,(CURDRV)
		;add	a,'A'
		;rst	$18
		;call	PrintCRLF
		;prompt boot choice with countdown 3 seconds
		;input in a
		;ld	(CURDRV),a
		
		ld	bc,$01f8	; B=Number of sectors (1), C=Media ID (F8)
		ld	hl,(SDIRBU)	; Begin address in memory
		ld	de,$0000	; Begin sector (0 is boot sector)
		or	a		; Reset carry flag for read
		call	PHYDIO		; Get the data from disk 
		ld	hl,(SDIRBU)
		ret

; ------------------------------------------------------------------------------
; *** Driver subroutines ***
; ------------------------------------------------------------------------------

; ------------------------------------------
; IDE disk logo output
; ------------------------------------------
OutputLogo:	di
		push	af
		push	hl
		call	PrintMsg
		db	12
IFDEF BEER232
		db	"BEER-232: MSX-DOS 2 IDE",13,10
ELSE
		db	"BEER-232: MSX-DOS 1 IDE",13,10
ENDIF
		db	"Master  : ",0
		ld	c,$4d
		ld	hl,($d079)
		ld	a,($d07b)
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
		ld	a,$20
		rst	$18
		call	PrintCRLF
		ld	hl,$d02e
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
		ld	c,a		; sector address = 0 (24 bits) 

ReadBootRec:	call	ideWaitReady
		ret	c
		call	ideSetSector
		jp	ideReadSector

; ------------------------------------------------------------------------------
; *** Print subroutines ***
; ------------------------------------------------------------------------------
PrintMsg:	ex      (sp),hl
	        call    PrintString
        	ex      (sp),hl
	        ret

PrintString:	ld      a,(hl)
	        inc     hl
        	and     a
	        ret     z
        	rst	$18		; print character
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
		rst	$18		; print character
		ret
r735:		dec	b
		jr	z,r734
		ret

; ------------------------------------------------------------------------------
; *** IDE routines ***
; ------------------------------------------------------------------------------

; ------------------------------------------
; Get IDE device capabilities
; ------------------------------------------
ideInfo:	call	ideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		call	ppideCommand
		call	ideWaitData
		jp 	nz,ideError
		ld	de,IDE_DATA_READ		
		ld	bc,IDE_IO_SECTOR
		out	(c),e
info_loop:	out	(c),d			; IDE read data
		dec	c
		dec	c
		ini				; read low byte, increase bufferpointer
		inc	c
		ini				; read high byte, increase bufferpointer
		inc	c
		out	(c),e			; IDE idle
		djnz	info_loop
		xor	a
		ret

; ------------------------------------------
; IDE set sector start address and number of sectors
; Input: C,D,E = 24-bit sector number
; ------------------------------------------
ideSetSector:	call	ideWaitReady
		ret	c
		jp	ppideParam

; ------------------------------------------
; IDE Read Sector
; Input: HL = transfer address
; ------------------------------------------
ideReadSector:	ld 	a,IDE_CMD_READ
		call	ppideCommand
		call	ideWaitData
		jp	nz,ideError		; nz=error
		bit	7,h			; store data in ram page 2 or 3?
		push	af
		push	hl
		jr	nz,rd01			; nz=yes, directly store data in destination
		ld	hl,(SSECBUF)		; init temporary buffer pointer
rd01:		ld	de,IDE_DATA_READ
		ld	bc,IDE_IO_SECTOR
		out	(c),e
rd02:		out	(c),d			; IDE read data
		dec	c
		dec	c
		ini				; read low byte, increase buffer pointer
		inc	c
		ini				; read high byte, increase buffer pointer
		inc	c
		out	(c),e			; IDE idle
		djnz	rd02
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
; IDE Write Sector
; Input: HL = transfer address
; ------------------------------------------
ideWriteSector:	ld	a,IDE_CMD_WRITE
		call	ppideCommand

		; design flaw: the control signals between ppi and ide should be inverted
		; because changing the ppi mode resets the output ports
		; call	ideWaitData
		; jp	nz,ideError
		; call ppideOutput
		ld	b,$30
wr_wait:	ex	(sp),hl
		ex	(sp),hl
		djnz	wr_wait

		push	hl
		bit	7,h			; read data from ram page 2 or 3?
		jr	nz,wr01			; nz=yes, directly read data from source
		ld	de,(SSECBUF)		; copy source to temporary buffer
		ld	bc,$0200		; sector size
		call	XFER
		ld	hl,(SSECBUF)		; init buffer pointer
wr01:		ld	de,IDE_DATA_WRITE
		ld	bc,IDE_IO_SECTOR
		out	(c),e
wr02:		dec	c
		dec	c
		outi				; write low byte, increase buffer pointer
		inc	c
		outi				; write high byte, increase buffer pointer
		inc 	c
		out	(c),d			; IDE write data
		out	(c),e			; IDE idle
		djnz	wr02
		pop	hl
		inc	h
		inc	h
		xor	a			; reset c-flag: no error
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
ideError:	call	ppideError
		ld	l,a
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
; *** PPI 8255 <==> IDE routines ***
; ------------------------------------------------------------------------------
; PPI 8255 I/O registers:
; $30	A: IDE data low byte
; $31	B: IDE data high byte
; $32	C: IDE control
; $33	PPI control
;
; PPI control:
; $92 = Set PPI A+B to input
; $80 = Set PPI A+B to output
;
; IDE control bit:
; 0	IDE register bit 0
; 1	IDE register bit 1
; 2	IDE register bit 2
; 3	Not used
; 4	Not used
; 5	/CS Select
; 6	/WR Write data
; 7	/RD Read data
; ------------------------------------------
IDE_CMD_READ	equ	$20		; read sector
IDE_CMD_WRITE	equ	$30		; write sector
IDE_CMD_INFO	equ	$ec		; disk info

IDE_DATA_READ	equ	$40c0		; 40=/read  c0=idle
IDE_DATA_WRITE	equ	$80c0		; 80=/write c0=idle

IDE_IO_SECTOR	equ	$0032		; 00=counter 32=port

; ------------------------------------------
; PPI IDE set sector parameters
; Input: C,D,E = 24-bit sector number
; ------------------------------------------
ppideParam:	call	ppideOutput
		ld	a,$c2			; IDE register 2
		out	($32),a
		ld	a,$01			; number of sectors is 1
		out	($30),a
		ld	a,$82
		out	($32),a
		ld	a,$c2
		out	($32),a

		inc	a			; IDE register 3
		out	($32),a
		ld	a,e			; bit 0..7
		out	($30),a
		ld	a,$83
		out	($32),a
		ld	a,$c3
		out	($32),a

		inc	a			; IDE register 4
		out	($32),a
		ld	a,d			; bit 8..15
		out	($30),a
		ld	a,$84
		out	($32),a
		ld	a,$c4
		out	($32),a

		inc	a			; IDE register 5
		out	($32),a
		ld	a,c			; bit 16..23
		out	($30),a
		ld	a,$85
		out	($32),a
		ld	a,$c5
		out	($32),a

		inc	a			; IDE register 6
		out	($32),a
		ld	a,$e0			; LBA mode
		out	($30),a
		ld	a,$86
		out	($32),a
		ld	a,$c6
		out	($32),a
		ret

; ------------------------------------------
; PPI IDE set command
; Input:  A = command
; ------------------------------------------
ppideCommand:	call	ppideOutput
		ex	af,af'
		ld	a,$c7
		out	($32),a
		ex	af,af'
		out 	($30),a
		ld 	a,$87
		out 	($32),a
		ld	a,$c7
		out 	($32),a
		ret

; ------------------------------------------
; PPI IDE read error register
; ------------------------------------------
ppideError:	ld	a,$c1
		out	($32),a
		ld 	a,$41
		out 	($32),a
		in 	a,($30)
		ex	af,af'
		ld	a,$c1
		out	($32),a
		ex	af,af'
		ret

; ------------------------------------------
; PPI IDE read status register
; ------------------------------------------
ppideStatus:	ld	a,$c7
		out	($32),a
		ld 	a,$47
		out 	($32),a
		in 	a,($30)
		ex	af,af'
		ld 	a,$c7
		out	($32),a
		ex 	af,af'
		ret

; ------------------------------------------
; PPI IDE set data direction
; ------------------------------------------
ppideOutput:	ex	af,af'
		ld	a,$80			; PPI A+B is output
		out	($33),a			
		ex	af,af'
		ret

ppideInput:	ex	af,af'
		ld	a,$92			; PPI A+B is input
		out	($33),a
		ex	af,af'
		ret

