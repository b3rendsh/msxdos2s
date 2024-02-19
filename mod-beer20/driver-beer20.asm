; DRIVER-BEER20.ASM
;
; Hard Disk ROM for MSX computer
; Driver for IDE card BEER-202
;
; Copyright SOLiD (2005) for most parts of the low-level IDE interface code.
; ------------------------------------------------------------------------------
; H_J. Berends:
; Based on driver sources for SOLiD BEER IDE v1.8 and disassembled v1.9 rom.
; The goal is to create a simple standard DOS1/DOS2 compatible beer ide driver.
; ------------------------------------------------------------------------------

		SECTION	DRIVER_BEER20

; Symbols which are defined by the disk hardware driver

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

		; Additional routine defined by the beer-ide driver
		PUBLIC	HDDBOOT		; Boot from HDD

; Disk routines used by driver

		EXTERN	GETWRK		; Get address of disk driver's work area
		EXTERN	GETSLT		; Get slot of this interface

MYSIZE		EQU	22
SECLEN		EQU	512

; MSX labels
SNSMAT		EQU	00141H
PHYDIO		EQU	00144H

SSECBUF		EQU	0F34DH		; pointer to sectorbuffer, can be used by the disk hardware driver
SDIRBU		EQU	0F351H		; pointer to directorysectorbuffer
DRVTBL  	EQU     0FB21H		; disk interface drive table
CURDRV		EQU	0F247H		; default drive


; Label translations MSXDOS <--> BEERIDE sources
INIHRD		EQU	InitFileSystem
DRIVES		EQU	GetHardDiskCount
INIENV		EQU	MasterBoot
DSKIO		EQU	HDD_SectorIO
DSKCHG		EQU	HDD_DiskChg
GETDPB		EQU	HDD_SetBPB
CHOICE		EQU	HDD_Choice
DSKFMT		EQU	HDD_Format
MTOFF		EQU	HDD_StopMotor
DEFDPB		EQU	DEFAULT_DPB
GetWorkBuf	EQU	GETWRK
GetMySlot	EQU	GETSLT


; ------------------------------------------
; INIHRD - Initialize the HDD / get sectors & heads
; Input : None
; Output: None
; ------------------------------------------
InitFileSystem:
	ld	a,6
	call	SNSMAT			; Check for CTRL, if pressed, no HDD
	di				; boot performed!
	and	2
	jr	z, ExitDiskInit

	ld	hl,2000h
ifs00:	dec	hl			; slight delay added...
	ld	a,h
	or	l
	jr	nz,ifs00
        call	HDD_StopMotor
	jr	c, ExitDiskInit
;
	ld	hl,0d000h
	call	IDE_Info
	ld	a,(0d006h)
	ld	(0fd0ch),a
	ld	a,(0d00ch)
	ld	(0fd0bh),a 
ifs3:	call	OutputHDDLogo
	ld	b,4			; .. and another delay to view the message
ifs1:	ld	hl,00000h
ifs2:	dec	hl
	ld	a,h
	or	l
ifs4:	jr	nz,ifs2
	djnz	ifs1
	xor	a
	ret

ExitDiskInit:
	INC	SP
	INC	SP
	RET

; ------------------------------------------
; DRIVES - Get number of drives connected
; A maximum of 4 primary partitions is supported, therefore returns fixed value 4.
; Input:
;   F = The zero flag is reset if one physical drive must act as two logical drives.
; Output:
;   L = Number of drives connected.
; ------------------------------------------
; Todo: use partition table to count the actual number of accessible partitions.

GetHardDiskCount:
	ld	l,4
	ret

; ------------------------------------------
; INIENV - Initialize the work area (environment).
; Input : None
; Output: None 
; Beer20: The carry flag is set if initialization failed
; Get partition info from MBR
; ------------------------------------------
MasterBoot:
	ld	hl,9000h	; Buffer address
	call	ReadMBR
	ret	c
	ld	a,(091feh)	; Validate boot signature (AA 55)
	cp	055h
	ret	nz
	ld	a,(091ffh)
	cp	0aah
	ret	nz
	call	GetWorkBuf
	push	hl
	ld	d,h
	ld	e,l
	inc	de
	ld	(hl),000h
	ld	bc,00014h
	ldir
	pop	de
	ld	hl,091beh	; Start of partition table
	ld	bc,00400h	; 4 entries (primary partition)
	ld	a,001h
	ex	af,af'
r612a:	ld	a,(hl)		; Status byte (Active/inactive)
	cp	080h		; Active?
	jr	nz,r612
	ex	af,af'
	ld	c,a
	ex	af,af'
r612:	push	bc
	inc	hl
	inc	hl		; Skip CHS address information of first sector (3 bytes)
	inc	hl
	inc	hl
	ld	a,(hl)		; Partition type
	cp	001h		; FAT12
	jr	z,r614a
	cp	004h		; FAT16 (<=32MB)
	jr	z,r614a
	cp	006h		; FAT16B (>32MB)
	jr	z,r614a
	cp	00eh		; FAT16B with LBA
	jr	z,r614a
IFDEF BEER19_OLD
	cp	082h		; Linux swap space / maybe used by other msx disk interfaces (?)
	jr	z,r614a
ENDIF
	ld	bc,0000ch	
	add	hl,bc		; Move to next partition
	jr	r614b
r614a:	inc	hl
	inc	hl		; Skip CHS address information of last sector (3 bytes)
	inc	hl
	inc	hl
	ldi			; Get LBA - first absolute sector in partition (4 bytes)
	ldi
	ldi
	ldi
	ld	(de),a		; Partition type
	inc	hl		; Skip LBA number of sectors (4 bytes) / move to next partition
	inc	hl
	inc	hl
	inc	hl
	inc	de
	ex	af,af'
	inc	a
	ex	af,af'
r614b:	pop	bc
	djnz	r612a		; process next partition entry (max 4)
	ld	(ix+014h),0ffh
	ex	af,af'
	ld	b,a
	push	ix
	push	bc
	call	GetMySlot
	ld 	hl,DRVTBL
	ld	b,a
	ld	c,000h
r614c:	ld	a,(hl)
	add	a,c
	ld	c,a
	inc	hl
	ld	a,(hl)
	inc	hl
	cp	b
	jr	nz,r614c
	dec	hl
	dec	hl
	ld	a,c
	sub	(hl)
	pop	bc
	ld	(hl),b
	add	a,c
	dec	a
	pop	ix
	ld	(ix+015h),a
	ret

; ------------------------------------------
; DSKIO - IDE Hard Disk (lowest level) Read/Write
; Input:
;   Carry flag = clear ==> read, set ==> write
;   A  = drive number
;   B  = number of sectors to transfer
;   C  = media descriptor byte
;   DE = first logical sector number
;   HL = transfer address
; Output:
;   Carry flag = clear ==> successful, set ==> error
;   If error then 
;     A = error code
;     B = remaining sectors
; ------------------------------------------
HDD_SectorIO:
	ei
	push	hl
	push	de
	push	bc
	push	af
	cp	4
	jr	nc,r581			;Wrong partition number
	call	GetWorkBuf
	pop	af
	push	af
	ld	e,a
	add	a,a
	add	a,a
	add	a,e
	ld	e,a
	ld	d,000h
	add	hl,de
	push	hl
	pop	ix
	ld	a,(ix+00h)		;Test if partition exist (must have
	or	(ix+01h)		;nonzero start cylinder)
	or	(ix+02h)
	or	(ix+03h)
	jp	z,r581
	pop	af
	pop	bc
	pop	de
	pop	hl
r582:	call	Wait_HDD
	push	bc
	push	de
	push	af
	call	SectorTrans
	pop	af
	push	af
	call	XFER_HDD
	jr	c,A764A
	pop	af
	pop	de
	inc	de
	pop	bc
	djnz	r582
	xor	a
	ret
;
r581:	pop	bc
	ld	a,4
	scf
	pop	bc
	pop	de
	pop	hl
	ret

A764A:	pop	bc
	pop	de
	pop	bc
	ret

; ------------------------------------------
; HDD data transfer <=> 
; ------------------------------------------
XFER_HDD:
	jp	c,hdd_wrsec

; Read sector
hdd_rdsec:
	ld	a,20h
	out	(30h),a
	ld	a,87h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,92h
	out	(33h),a
r587:	ld	a,0C7h
	out	(32h),a
	ld	a,47h
	out	(32h),a
	in	a,(30h)
	bit	7,a
	jr	nz,r587
	bit	0,a
	jp	nz,r586
	bit	3,a
	jr	z,r587
	ld	a,0C7h
	out	(32h),a
	ld	a,0C0h
	out	(32h),a
	or	a
	bit	7,h
	push	af
	push	hl
	jr	nz,r588
	ld	hl,(SSECBUF)
r588:	ld	c,30h
	ld	d,0
r589:	ld	a,40h
	ld	b,005h
	out	(32h),a
	ini
	inc	c
	ini
	dec	c
	ld	a,0C0h
	out	(32h),a
	dec	d
	jr	nz,r589
	pop	de
	pop	af
	ret	nz
	ld	hl,(SSECBUF)
	ld	bc,0200h		; sector size
	call	0F1D9h			; Store data in RAM page 1 (swap RAM with the disk ROM)
	ex	de,hl
	or	a
	ret

; Write sector
hdd_wrsec:
	ld	a,30h
	out	(30h),a
	ld	a,87h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ld	b,30h
r590:	ex	(sp),hl
	ex	(sp),hl
	djnz	r590
	ld	a,0C0h
	out	(32h),a
	push	hl
	bit	7,h
	jr	nz,r591
	ld	de,(SSECBUF)
	ld	bc,0200h		; sector size
	call	0F1D9h			; Get data from RAM page 1 (swap RAM with the disk ROM)
	ld	hl,(SSECBUF)
r591:	ld	c,30h
	ld	d,0
r592:	ld	b,005h
	outi
	inc	c
	outi
	dec	c
	ld	a,80h
	out	(32h),a
	ld	a,0C0h
	out	(32h),a
	dec	d
	jr	nz,r592
	pop	hl
	inc	h
	inc	h
	xor	a
	ret

r586:	ld	a,0c1h			; HDD I/O error recovery
	out	(032h),a
	ld	a,041h
	out	(032h),a
	in	a,(030h)
	ld	c,a
	ld	a,0c1h
	out	(032h),a
	ld	a,008h
	rr	c
	ret	c
	rr	c
	ret	c
	ld	a,00Ch
	rr	c
	ret	c
	rr	c
	ld	a,008h
	rr	c
	ret	c
	rr	c
	ld	a,004h
	rr	c
	ret	c
	ld	a,002h
	scf
	ret

; ------------------------------------------
; DSKCHG - Disk change (NO CHANGE)
; Input:
;   A  = Drive number
;   B  = 0
;   C  = Media descriptor
;   HL = Base address of DPB
; Output:
; If successful then
;   Carry flag reset
;   B = Disk change status
;       1= Disk unchanged, 0= Unknown, -1=Disk changed
; else
;   Carry flag set
;   Error code in A
; ------------------------------------------
HDD_DiskChg:
	push	af
	call	GetWorkBuf
	pop	af
	cp	(ix+14h)
	ld	(ix+14h),a
	jr	nz,r601
	ld	b,001h		; unchanged
	xor	a
	ret

r601:	ld	b,0FFh		; changed
	xor	a
	ret

; ------------------------------------------
; Set DPB using sector 0 (bootsector of partition)
; Input:
;   A  = Drive number
;   B  = First byte of FAT
;   C  = Media descriptor
;   HL = Base address of BPB
; Output:
;   [HL+1] .. [HL+18] = DPB fo the specified drive
; ------------------------------------------
HDD_SetBPB:
	ei
	push	hl
	ld	de,0			; first logical sector
	ld	hl,(SSECBUF)		; transfer address
	ld	b,1			; number of sectors is 1
	or	a			; carry flag cleared ==> read sector
	call	HDD_SectorIO
	pop	iy
	ret	c
	ld	ix,(SSECBUF)
	ld	a,(ix+15h)		; Media ID
	ld	(iy+01h),a
	ld	(iy+02h),000h		; Sector size is 0200h
	ld	(iy+03h),002h
	ld	(iy+04h),00fh		; Directory mask 00fh: 512/32-1
	ld	(iy+05h),004h		; Directory shift 004h
	ld	a,(ix+00dh)		; Cluster size (in sectors)
	dec	a
	ld	(iy+06h),a		; Cluster mask
	ld	c,00h
r596:	inc	c
	rra
	jr	c,r596
	ld	(iy+07h),c		; Cluster shift
	ld	l,(ix+0eh)		; Number of unused sectors
	ld	h,(ix+0fh)
	ld	(iy+08h),l		; FIRFAT - first FAT sector
	ld	(iy+09h),h
	ld	e,(ix+16h)		; Size of FAT (in sectors)
	ld	(iy+10h),e		; FATSIZ - Sectors per FAT
	ld	d,00h
	ld	b,(ix+10h)		; Number of FATs
	ld	(iy+0Ah),b
r597:	add	hl,de
	djnz	r597
	ld	(iy+11h),l		; FIRDIR - First directory sector
	ld	(iy+12h),h
	ld	a,(ix+12h)		; Number of directory entries (high byte)
	ex	de,hl
	ld	h,a
	ld	l,(ix+11h)		; Number of directory entries (low byte)
	ld	bc,0000fh
	add	hl,bc
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	hl,hl
	ld	l,h
	ld	h,000h
	ex	de,hl
	jr	z,r598
	ld	a,0ffh			; Max 255 directory entries
	jr	r599
r598:	ld	a,(iy+011h)
r599:	ld	(iy+00bh),a		; MAXENT - Max directory entries
	add	hl,de
IFDEF BEER19_OLD
; Incompatible interpretation of the hidden sectors field. In MS-DOS versions up to 3.0 this is the number of sectors 
; preceding the partition as a 16-bit value, later versions used a 32 bit value. In MSX-DOS it is not useable or used.
; BEER 1.9 adds this value to the offset for the first data sector relative to the start of the partition.
	ld	e,(ix+01ch)		; Number of hidden sectors
	ld	d,(ix+01dh)
	add	hl,de
ENDIF
	ld	(iy+00ch),l		; FIRREC - first data sector
	ld	(iy+00dh),h
	ex	de,hl
	ld	l,(ix+013h)		; Total number of sectors
	ld	h,(ix+014h)
	ld	bc,00000h
	ld	a,l
	or	h
	jr	nz,r600a
	ld	l,(ix+020h)
	ld	h,(ix+021h)
	ld	c,(ix+022h)
	ld	b,(ix+023h)
r600a:  or	a
	sbc	hl,de
	jr	nc,r600b
	dec	bc
r600b:	ld	a,(iy+007h)
r600c:  dec	a
	jr	z,r600d
	srl	b
	rr	c
	rr	h
	rr	l
	jr	r600c
r600d:	inc	hl
	ld	(iy+00eh),l		; MAXCLUS - number of clusters + 1
	ld 	(iy+00fh),h
	xor	a
	ret

; ------------------------------------------
; CHOICE - Choice for FORMAT (no choice)
; Input : None
; Output: HL = pointer to string, terminated by 0
; ------------------------------------------
HDD_Choice:
	xor	a
	ld	l,a
	ld	h,a
	ret

; ------------------------------------------
; DSKFMT - Format not implemented to minimize rom size
; ------------------------------------------
HDD_Format:
	call	PrintMsg
	db	0Dh,"IDE HDD format not implemented",0Dh,0Ah,0
	ret

; ------------------------------------------
; MTOFF - Initialize Hard Disk
; ------------------------------------------
HDD_StopMotor:
	di
	ld	hl,0
	ld	a,92h
	out	(33h),a
r610:	ld	a,0C7h
	out	(32h),a
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	ld	a,47h
	out	(32h),a
	in	a,(30h)
r602:	or	a
	jp	p,r609
	dec	hl
	ld	a,h
	or	l
	jp	nz,r610
	scf
	ei
	ret
;
r609:	ld	a,0E7h
	out	(32h),a
	xor	a
	ei	
	ret

; -------------------------------------------
; OEMSTATEMENT - BASIC System statement expansion
; -------------------------------------------
OEMSTA:	scf
	ret

; ------------------------------------------
; Default DPB pattern
; ------------------------------------------
DEFAULT_DPB:
	db	0F9h		; MEDIA		Media type
	dw	200h		; SECSIZ	Sector size
	db	00Fh		; DIRMSK	Directory mask
	db	004h		; DIRSHFT	Directory shift
	db	003h		; CLUSMSK	Cluster mask
	db	003h		; CLUSSFT	Cluster shift
	dw	00001h		; FIRFAT	First FAT sector
	db	002h		; FATCNT	Number of FATs
	db	070h		; MAXENT	Number of directory entries
	dw	00Eh		; FIRREC	First data sector
	dw	02CAh		; MAXCLUS	Number of clusters+1
	db	003h		; FATSIZ	Sectors per FAT
	dw	00007h		; FIRDIR	First directory sector
	db	0D0h		; ?
	db	002h		; ?

; ------------------------------------------
; Wait for HDD ready
; ------------------------------------------
Wait_HDD:
	push	af
	ld	a,92h
	out	(33h),a
r611:	ld	a,0C7h
	out	(32h),a
	ld	a,47h
	out	(32h),a
	in	a,(30h)
	and	11010000b
	cp	50h
	jp	nz,r611		;loop if BUSY flag set
	ld	a,0F7h
	out	(32h),a
	pop	af
	ret


; ------------------------------------------
; Boot MSX-DOS from HDD (DOS_HDD_BOOT)
; ------------------------------------------
HDDBOOT:
	di
	in	a,(0AAh)	; direct get keyboard in (todo: replace with call SNSMAT)
	and	0F0h
	or	7
	out	(0AAh),a
	nop
	in	a,(0A9h)
	and	40h		; If [SELECT] key pressed then do not boot 
	scf
	ret	z
	call	GetWorkBuf
	ld	a,(ix+15h)
	ld	(CURDRV),a
	ld	bc,01F8h	; B=Number of sectors (1), C=Media ID (F8)
	ld	hl,(SDIRBU)	; Begin address in memory
	ld	de,0		; Begin sector (0 is boot sector)
	or	a		; Reset carry flag for read
	call	PHYDIO		; Get the data from disk 
	ld	hl,(SDIRBU)
	ret

; ------------------------------------------
; Get IDE device capabilities
; ------------------------------------------
IDE_Info:
	call	Wait_HDD
	ld	a,80h
	out	(33h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,0ECh
	out	(30h),a
	ld	a,87h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ld	a,92h
	out	(33h),a
	ld	a,0C7h
	out	(32h),a
r621:	ld	a,47h
	out	(32h),a
	in	a,(30h)
	ld	c,a
	ld	a,0C7h
	out	(32h),a
	bit	7,c
	jr	nz,r621		;wait if BUSY bit set
	bit	0,c
	jr	nz,erexpl	;explain error
	bit	3,c		;DTRQ set? if no -> loop
	jr	z,r621
	ld	c,30h
	ld	d,0
r622:	ld	a,40h
	ld	b,05h
	out	(32h),a
	ini
	inc	c
	ini
	dec	c
	ld	a,0F0h
	out	(32h),a
	dec	d
	jr	nz,r622
	xor	a
	ret
;
erexpl:	ld	a,0C1h
	out	(032h),a
	ld	a,041h
	out	(032h),a
	in	a,(030h)
	ld	c,a
	ld	a,0C1h
	out	(032h),a
	ld	a,c
	scf
	ret

; ------------------------------------------
; Hard disk logo output
; ------------------------------------------
OutputHDDLogo:
	di
	push	af
	push	hl
	ld	hl,S_logo
	call	sputc
	ld	c,04dh
	ld	hl,(0d079h)
	ld	a,(0d07bh)
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
	jr	z,A7A15
	ld	c,047h
	ld	l,h
	ld	h,a
	srl	h
	rr	l
	srl	h
	rr	l
A7A15:	call	MakeDec
	ld	a,c
	call	000a2h
	ld	a,020h
	call	000a2h
	call	A7A3D
	ld	hl,0d02eh
	ld	b,00ah
A7A29:	inc	hl
	ld	a,(hl)
	call	000a2h
	dec	hl
	ld	a,(hl)
	call	000a2h
	inc	hl
	inc	hl
	djnz	A7A29
	call	A7A3D
	pop	hl
	pop	af
	ret

; Print CR+LF
A7A3D:	ld	a,00dh
	call	000a2h
	ld	a,00ah
	jp	000a2h

; ------------------------------------------
; Create decimal
; ------------------------------------------
MakeDec:
	ld	b,010h
	ld	de,00000h
	xor	a
r623:	add	hl,hl
	ld	a,e
	adc	a,a
	daa
	ld	e,a
	ld	a,d
	adc	a,a
	daa
	ld	d,a
	djnz	r623
	ld	b,004h
	ld	a,d
	call	A7A5F
	ld	a,e
A7A5F:	push	af
	rrca
	rrca
	rrca	
	rrca
	call	A7A68
	pop	af
A7A68:	and	0Fh
	jr	z,A7A74
A7A6C:	ld	b,001h
	add	a,030h
	call	000a2h
	ret
A7A74:	dec	b
	jr	z,A7A6C
	ret

sputc:	ld	a,(hl)
	or	a
	ret	z
	call	000A2h
	inc	hl
	jr	sputc
;
S_logo:	db	12,"BEER 202: IDE HDD driver",13,10
	db	13,10,"IDE: ",0

; ------------------------------------------
; Read Master Boot Record (0/0/1)
; ------------------------------------------
ReadMBR:
	call	Wait_HDD
	xor	a
	ld	e,a
	ld	d,a
	ld	c,a
	push	hl
	call	SetHDDParam
	pop	hl
	call	hdd_rdsec
	xor	a
	ret

; ------------------------------------------
; Logical sector -> physical
; CHS addressing used.
; ------------------------------------------
SectorTrans:
	push	hl
	ex	de,hl
	ld	a,h
	and	l
	inc	a
	jr	nz,r901
	ld	hl,(0fd0dh)
	ld	de,(0fd0fh)
	jr	r902
r901:	ld	de,00000h
r902:	ld	c,(ix+00h)
	ld	b,(ix+01h)
	add	hl,bc
	ex	de,hl
	ld	c,(ix+02h)
	ld	b,(ix+03h)
	adc	hl,bc
	ld	a,e
	ld	e,d
	ld	d,l
	ld	c,b
	pop	hl

SetHDDParam:
	ex	af,af'
	ld	a,80h
	out	(33h),a
	ld	a,0C2h
	out	(32h),a
	ld	a,1
	out	(30h),a
	ld	a,82h
	out	(32h),a
	ld	a,0C2h
	out	(32h),a
	inc	a
	out	(32h),a
	ex	af,af'
	push	bc
	ld	bc,00530h
	out	(c),a
	pop	bc
	ld	a,083h
	out	(32h),a
	ld	a,0C3h
	out	(32h),a
	ld	a,0C6h
	out	(32h),a
	ld	a,c
	and	00fh
	or	0e0h
	out	(30h),a
	ld	a,86h
	out	(32h),a
	ld	a,0C6h
	out	(32h),a
	ld	a,0C4h
	out	(32h),a
	ld	bc,00530h
	out	(c),e
	ld	a,84h
	out	(32h),a
	ld	a,0C4h
	out	(32h),a
	inc	a
	out	(32h),a
	out	(c),d
	ld	a,85h
	out	(32h),a
	ld	a,0C5h
	out	(32h),a
	ld	a,0C7h
	out	(32h),a
	ret

; ------------------------------------------------------------------------------
; The PrintMsg routine is also defined in the DOS 1 disk section.
; Duplicated here to maintain compatibility with the interface specification.

PrintMsg:
	ex      (sp),hl
        call    PrintString
        ex      (sp),hl
        ret

PrintString:
	ld      a,(hl)
        inc     hl
        and     a
        ret     z
        call    0408fh		; SOUT: output to screen
        jr      PrintString


; ------------------------------------------------------------------------------
; Intentional no HDD inquiry table at address 7F80 as used by some other controllers. 
; This is not a SCSI interface. 
