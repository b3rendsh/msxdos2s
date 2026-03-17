; ------------------------------------------------------------------------------
; drv_ide.asm
;
; Copyright (C) 2026 H.J. Berends*
; * Part of the code is based on the BEER-202 driver by SOLiD and other
; PPI 8255 and 8-BIT CF IDE solutions publicly shared on the internet.
; 
; You can freely use, distribute or modify this program.
; It is provided freely and "as it is" in the hope that it will be useful, 
; but without any warranty of any kind, either expressed or implied.
; ------------------------------------------------------------------------------
; Note: use build option BEER_CS for optimized IDE control signals

	IF !(CXDOS1 || CXDOS2)
	        INCLUDE "disk.inc"	; Assembler directives
		INCLUDE	"msx.inc"	; MSX constants and definitions
		SECTION	DRV_IDE
	ENDIF

		; Mandatory symbols defined by the disk hardware interface driver
		PUBLIC	DSKIO		; Disk I/O routine
		PUBLIC	DSKCHG		; Disk change routine
		PUBLIC	READSEC		; Read sector (MBR / bootsector) routine
		PUBLIC	DSKABSRW	; Read/write absolute sectors on disk
		PUBLIC	DRVMEM		; Memory for hardware interface variables
		PUBLIC	ideInit
		PUBLIC	ideInfo

		EXTERN	GETWRK		; Get address of disk driver's work area
		EXTERN	DRVSIZE		; DOS driver workarea size (offset)
		EXTERN	W_CURDRV	; Workarea variable defined by the DOS driver

; Hardware driver variables
W_RWFLAG	equ	DRVSIZE+$00	; Read/Write flag
W_IODATA	equ	DRVSIZE+$01	; IDE IO data port/register
W_IOCTL		equ	DRVSIZE+$02	; IDE IO control port/register
;
DRVMEM		equ	$03		; Workarea memory for hardware interface variables

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
DSKCHG:
	IFDEF IDEDOS1
		push	af
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
	ELSE
		; always return unchanged for DOS2 (disks are not hot-pluggable)
		ld	b,$01
		xor	a
		ret
	ENDIF

; ------------------------------------------
; READSEC - Read (boot) sector
; Input:  C,DE = sector number
;         HL   = transfer address
; Output: Carry flag = clear ==> successful, set ==> error
; May corrupt: AF,BC,DE
; ------------------------------------------
READSEC:	call	ideSetSector
		ret	c
		jp	dosReadSector

; ------------------------------------------
; DSKABSRW - IDE disk absolute sector read/write
; Input:
;   Carry flag = clear ==> read, set ==> write
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
; Note: currently only used to load the kernel code with ROM16K option
; ------------------------------------------
DSKABSRW:	push	hl
		push	de
		push	bc
		push	af
		call	GETWRK			; base address of workarea in hl and ix
		pop	af
		ld	(ix+W_RWFLAG),$00
		jr	nc,rwflag_set
		ld	(ix+W_RWFLAG),$01
rwflag_set:	pop	bc
		pop	de
		pop	hl
		jp	rw_loop

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

	IFDEF BEER
; ------------------------------------------------------------------------------
; *** BEER PPI 8255 IDE routines ***
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

IDE_READ	equ	$40		; /rd=0 /wr=1 /cs=0
IDE_WRITE	equ	$80		; /rd=1 /wr=0 /cs=0
IDE_SET		equ	$c0		; /rd=1 /wr=1 /cs=0
	IFDEF BEER_CS
IDE_IDLE	equ	$c7		; /rd=1 /wr=1 /cs=0 reg=7
IDE_OFF		equ	$e7		; /rd=1 /wr=1 /cs=1 reg=7
	ELSE
IDE_IDLE	equ	$e7		; /rd=1 /wr=1 /cs=1 reg=7
	ENDIF

; PPI 8255 I/O registers:
PPI_IOA		equ	$30		; A: IDE data low byte
PPI_IOB		equ	$31		; B: IDE data high byte
PPI_IOC		equ	$32		; C: IDE control
PPI_CTL		equ	$33		; PPI control

; PPI 8255 settings:
PPI_INPUT	equ	$92		; Set PPI A+B to input
PPI_OUTPUT	equ	$80		; Set PPI A+B to output

; ------------------------------------------
; Initialize disk
; Output: Z-flag set if hardware detected
; probe for PPI 8255 hardware on fixed IO port
; ------------------------------------------
ideInit:	ld	a,PPI_IOA
		ld	(ix+W_IODATA),a
		ld	a,PPI_CTL
		ld	(ix+W_IOCTL),a
		call	beerOutput
		ld	hl,$00a5		; register=0 value=a5
		call	beerSetReg
		in	a,(PPI_IOA)
		cp	$a5
		ret

; ------------------------------------------
; Get IDE device information 
; ------------------------------------------
ideInfo:	call	ideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		call	beerCommand
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
		call	beerOutput
		push	hl
		ld	h,$02			; IDE register 2
		ld	l,$01			; number of sectors is 1
		call	beerSetReg
		inc	h			; IDE register 3
		ld	l,e			; bit 0..7
		call	beerSetReg
		inc	h			; IDE register 4
		ld	l,d			; bit 8..15
		call	beerSetReg
		inc	h			; IDE register 5
		ld	l,c			; bit 16..23
		call	beerSetReg
		inc	h			; IDE register 6
		ld	l,$e0			; LBA mode, drive 0
		call	beerSetReg
	IFDEF BEER_CS
		ld	a,IDE_IDLE		; IDE register 7
		out	(PPI_IOC),a		; set address
	ENDIF
		pop	hl
		xor	a
		ret

; ------------------------------------------
; IDE set command read sector
; ------------------------------------------
ideCmdRead:	push	bc
		ld 	a,IDE_CMD_READ
	IFDEF BEER_CS
		call	beer_command_1		; direction already set to output
	ELSE
		call	beerCommand
	ENDIF
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
rdsec_loop:
		REPT 2				;
		out	(c),d			; IDE read
		ld	c,a			; PPI port A
		ini				; read low byte, increase bufferpointer, decrease counter
		inc	c			; PPI port B
		ini				; read high byte, increase bufferpointer, decrease counter
		inc	c			; PPI port C
		out	(c),e			; IDE idle
		ENDR
		djnz	rdsec_loop		; 640 MOD 5 = 0
		ret

; ------------------------------------------
; IDE set command write sector
; ------------------------------------------
ideCmdWrite:	push	bc
		ld	a,IDE_CMD_WRITE
	IFDEF BEER_CS
		call	beer_command_1		; direction already set to output
	ELSE
		call	beerCommand
	ENDIF		

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
wrsec_loop:
		REPT 2
		ld	c,a
		outi
		inc	c
		outi
		inc 	c
		out	(c),d
		out	(c),e
		ENDR
		djnz	wrsec_loop
		ret

; ------------------------------------------
; Wait for IDE ready or time-out
; ------------------------------------------
ideWaitReady:	push	hl
		push	bc
		call	beerInput
		ld	b,$14			; time-out after 20 seconds
wait_1:		ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
wait_2:		call	beerStatus
		and	%11000000
		cp	%01000000		; BUSY=0 RDY=1 ?
		jr	z,wait_end		; z=yes
		dec	hl
		ld	a,h
		or	l
		jr	nz,wait_2
		djnz	wait_1
		scf				; time-out
wait_end:	
	IFDEF BEER_CS
		ld	a,IDE_OFF		; deassert CS / IDE register 7
		out	(PPI_IOC),a		; required for some ide controllers
	ENDIF
		pop	bc
		pop	hl
		ret

; ------------------------------------------
; Wait for IDE data read/write request
; ------------------------------------------
ideWaitData:	call 	beerInput
waitdata_1:	call	beerStatus
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
		call	beerReadReg
		jp	dosError

; ------------------------------------------
; PPI IDE read status register
; ------------------------------------------
beerStatus:	ld	a,IDE_SET+REG_STATUS
beerReadReg:
	IFDEF BEER_CS
		push	bc
		ld	b,a
		ld	c,PPI_IOC
		out	(c),b
		res	7,b			; /rd=0 (assert read)
		out	(c),b
		in	a,(PPI_IOA)		; read register
		set	7,b			; /rd=1 (deassert read)
		out	(c),b
		pop	bc
		ret
	ELSE
		out	(PPI_IOC),a
		res	7,a			; /rd=0
		out	(PPI_IOC),a
		in	a,(PPI_IOA)		; read register
		ex	af,af'
		ld	a,IDE_IDLE
		out	(PPI_IOC),a
		ex	af,af'
		ret
	ENDIF

; ------------------------------------------
; PPI IDE set command
; Input:  A = command
; ------------------------------------------
beerCommand:	call	beerOutput
beer_command_1:	push	hl
		ld	h,REG_COMMAND
		ld	l,a
		call	beerSetReg
		pop	hl
		ret

; ------------------------------------------
; PPI IDE set register
; Input:  H = register
;         L = value
; ------------------------------------------
beerSetReg:	ld	a,IDE_SET
		add	a,h
		out	(PPI_IOC),a		; set register
		ld	a,l
		out 	(PPI_IOA),a		; write value
		ld 	a,IDE_WRITE
		add	a,h
		out 	(PPI_IOC),a		; /wr=0 (assert write)
	IFDEF BEER_CS
		ld	a,IDE_SET
		add	a,h
	ELSE
		ld	a,IDE_IDLE
	ENDIF	
		out 	(PPI_IOC),a		; /wr=1 (deassert write)
		ret 

; ------------------------------------------
; PPI IDE set data direction
; Changing the mode on a 8255 will reset ports A,B and C to 0.
; After channging direction the control lines on PPI Port C 
; must be set before any read/write to IDE registers.
; ------------------------------------------
beerInput:	ex	af,af'
		ld	a,PPI_INPUT		; PPI A+B is input
		out	(PPI_CTL),a
		ex	af,af'
		ret

beerOutput:	ex	af,af'
		ld	a,PPI_OUTPUT		; PPI A+B is output
		out	(PPI_CTL),a
		ex	af,af'
		ret

	; END BEER

	ELIFDEF MALT
; ------------------------------------------------------------------------------
; *** MALT PPI 8255 IDE routines ***
; ------------------------------------------------------------------------------
; PPI IDE control bit:
; 0	IDE register bit 0
; 1	IDE register bit 1
; 2	IDE register bit 2
; 3	/CS0   Chip select 0 (inverted)
; 4	/CS1   Chip select 1 (inverted)
; 5	/WR    Write data    (inverted)
; 6	/RD    Read data     (inverted)
; 7     /RESET Drive reset   (inverted)

IDE_SET		equ	$08		; assert /cs0
IDE_WRITE	equ	$28		; assert /write
IDE_READ	equ	$48		; assert /read
IDE_RESET	equ	$80		; assert /reset
IDE_BITWR	equ	$05
IDE_BITRD	equ	$06


; PPI 8255 settings:
PPI_INPUT	equ	$92		; Set PPI A+B to input
PPI_OUTPUT	equ	$80		; Set PPI A+B to output

; ------------------------------------------
; Initialize disk
; Output: Z-flag set if hardware detected
; probe for PPI 8255 hardware
; ------------------------------------------
ideInit:	ld	hl,idePorts
ideProbe:	ld	a,(hl)
		cp	$ff
		jr	z,ideNotDetected
		call	ideInit1
		ret	z
		inc	hl
		jr	ideProbe

ideInit1:	ld	(ix+W_IODATA),a		; PPIDE Data
		add	a,$03
		ld	(ix+W_IOCTL),a		; PPIDE Control
		call	maltOutput
		push	hl
		ld	hl,$00a5		; register=0 value=a5
		call	maltSetReg
		pop	hl
		ld	c,(ix+W_IODATA)
		in	a,(c)
		cp	$a5
		ret	nz

		call	maltReset		; interface detected, reset disk controller
		xor	a			; z=detected
		ret

ideNotDetected:	or	a			; nz=not detected
		ret

; List of ports that are probed, end with $ff
idePorts:	db	$3c,$34,$1c,$14,$ff


; ------------------------------------------
; Get IDE device information
; ------------------------------------------
ideInfo:	call	ideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		call	maltCommand
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
		call	maltOutput
		pop	bc
		push	bc
		ld	h,$02			; IDE register 2
		ld	l,$01			; number of sectors is 1
		call	maltSetReg
		inc	h			; IDE register 3
		ld	l,e			; bit 0..7
		call	maltSetReg
		inc	h			; IDE register 4
		ld	l,d			; bit 8..15
		call	maltSetReg
		inc	h			; IDE register 5
		ld	l,c			; bit 16..23
		call	maltSetReg
		inc	h			; IDE register 6
		ld	l,$e0			; LBA mode, drive 0
		call	maltSetReg
		pop	bc
		pop	hl
		xor	a
		ret

; ------------------------------------------
; IDE set command read sector
; ------------------------------------------
ideCmdRead:	push	bc
		ld 	a,IDE_CMD_READ
		call	malt_command_1
		call	ideWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Read Sector
; Input: HL = transfer address
; ------------------------------------------
ideReadSector:	ld	a,(ix+W_IODATA)		; select PPI port A
		ld	b,$80			; counter (decreases by 5 in 4-byte loop)
		ld	c,a
		inc	c
		inc	c			; select PPI port C
		ld	d,IDE_READ
		ld	e,IDE_SET
		out	(c),e			; set data register
rdsec_loop:
		REPT 2
		out	(c),d			; assert read
		ld	c,a			; select PPI port A
		ini				; read low byte, increase bufferpointer, decrease counter
		inc	c			; select PPI port B
		ini				; read high byte, increase bufferpointer, decrease counter
		inc	c			; select PPI port C
		out	(c),e			; deassert read
		ENDR
		djnz	rdsec_loop		; 640 MOD 5 = 0
		ret

; ------------------------------------------
; IDE set command write sector
; ------------------------------------------
ideCmdWrite:	push	bc
		ld	a,IDE_CMD_WRITE
		call	malt_command_1
		call	ideWaitData
		call	maltOutput
		pop	bc
		ret

; ------------------------------------------
; IDE Write Sector
; Input: HL = transfer address
; ------------------------------------------
ideWriteSector:	ld	a,(ix+W_IODATA)		; select PPI port A
		ld	b,$80			; counter: 128x4=512 bytes
		ld	c,a
		inc	c
		inc	c			; select PPI port C
		ld	d,IDE_WRITE
		ld	e,IDE_SET
		out	(c),e			; set data sector
wrsec_loop:
		REPT 2
		ld	c,a
		outi				; write low byte
		inc	c
		outi				; write high byte
		inc 	c
		out	(c),d			; assert write
		out	(c),e			; deassert write
		ENDR
		djnz	wrsec_loop
		ret

; ------------------------------------------
; Wait for IDE ready or time-out
; ------------------------------------------
ideWaitReady:	push	hl
		push	bc
		call	maltInput
		ld	b,$14			; time-out after 20 seconds
wait_1:		ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
wait_2:		call	maltStatus
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
ideWaitData:	call 	maltInput
waitdata_1:	call	maltStatus
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
		call	maltReadReg
		push	af
		call	maltReset		; reset disk controller before retry
		pop	af
		jp	dosError		; let DOS handle the error

; ------------------------------------------
; PPI IDE read status register
; ------------------------------------------
maltStatus:	ld	a,IDE_SET+REG_STATUS
maltReadReg:	push	bc
		ld	b,a
		ld	c,(ix+W_IODATA)
		inc	c
		inc	c			; select PPI port C
		out	(c),b
		set	IDE_BITRD,b		; assert read
		out	(c),b
		dec	c
		dec	c			; select PPI port A
		in	a,(c)			; read register
		inc	c
		inc	c			; select PPI port C
		res	IDE_BITRD,b		; deassert read
		out	(c),b
		pop	bc
		ret

; ------------------------------------------
; PPI IDE set command
; Input:  A = command
; ------------------------------------------
maltCommand:	call	maltOutput
malt_command_1:	push	hl
		ld	h,REG_COMMAND
		ld	l,a
		call	maltSetReg
		pop	hl
		ret

; ------------------------------------------
; PPI IDE set register
; Input:  H = register
;         L = value
; ------------------------------------------
maltSetReg:	push	bc
		ld	c,(ix+W_IODATA)
		out	(c),l			; write value
		inc	c
		inc	c
		ld	a,IDE_SET
		add	a,h
		out	(c),a			; set register
		set	IDE_BITWR,a		; assert write
		out	(c),a
		res	IDE_BITWR,a		; deassert write
		out 	(c),a
		pop	bc
		ret

; ------------------------------------------
; PPI IDE reset disk controller
; do hard reset, no soft reset
; ------------------------------------------
maltReset:	ld	c,(ix+W_IODATA)
		inc	c
		inc	c
		ld	a,IDE_RESET
		out	(c),a			; assert reset
		REPT 5				; delay appr.60us, minimum is 25us (90 cycles)
		ex	(sp),hl			; 20 cycles
		ex	(sp),hl
		ENDR
		xor	a			; deassert reset (clear all control signals)
		out	(c),a
		ret

; ------------------------------------------
; PPI IDE set data direction
; ------------------------------------------
maltInput:	ld	b,PPI_INPUT		; PPI A+B is input
		ld	c,(ix+W_IOCTL)
		out	(c),b
		ret

maltOutput:	ld	b,PPI_OUTPUT		; PPI A+B is output
		ld	c,(ix+W_IOCTL)
		out	(c),b
		ret

	; END MALT

	ELIFDEF SODA
; ------------------------------------------------------------------------------
; *** Compact Flash 8-BIT IDE routines ***
; ------------------------------------------------------------------------------

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


	; END SODA

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
