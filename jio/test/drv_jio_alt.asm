; ------------------------------------------------------------------------------
; drv_jio_alt.asm
;
; Copyright (C) 2025 All rights reserved
; JIO MSX-DOS 2 driver by Louthrax
; JIO MSX-DOS 1 driver and CRC routines by H.J. Berends
; 115K2 transmit/receive routines based on code by Nyyrikki
;
; Alternative driver:
; + Optional LPTIO build: use LPT port with 2 stop bits (115200/8N2)
; + Joystick port receive timing alternatives (115200/8N1)
; + UART 1655X at base i/o port 0x80
; ------------------------------------------------------------------------------

IF !(CXDOS1 || CXDOS2)
        INCLUDE	"disk.inc"	; Assembler directives
        INCLUDE	"msx.inc"	; MSX constants and definitions
	DEFINE	DRV_IPL		; Include driver ipl routines
	DEFINE	DRV_SYS		; Include driver system routines
	SECTION	DRV_JIO
ENDIF

; Hardware driver variables

W_CURDRV	equ	$0	; Current drive (DOS1)
W_BOOTDRV	equ	$1	; Boot drive (partition)
W_DRIVES	equ	$2	; Number of drives (partitions) on disk
W_FLAGS		equ	$3
W_COMMAND	equ	$4
W_DRIVE		equ	$5	; DSKIO save drive number (DOS1)
W_DSKCHG	equ	$6	; Partition changed flags
MYSIZE		equ	$7

SECLEN		equ	512
PART_BUF	equ	TMPSTK	; Copy of disk info / Master Boot Record

; UART definitions

ubase		equ	$80

UART_RBR	equ	0		; dlab=0: receiver buffer register (read)
UART_THR	equ	0		; dlab=0: transmitter holding register (write)
UART_IER	equ	1		; dlab=0: interrupt enable register 
UART_IIR	equ	2		; interrupt identifcation register (read)
UART_FCR	equ	2		; fifo control register (write)
UART_LCR	equ	3		; line control register 
UART_MCR	equ	4		; modem control register 
UART_LSR	equ	5		; line status register 
UART_MSR	equ	6		; modem status register 
UART_SCR	equ	7		; scratch register 
UART_DLL	equ	0		; dlab=1: divisor latch (ls)
UART_DLM	equ	1		; dlab=1: divisor latch (ms)
UART_AFR	equ	2		; dlab=1: alternate function register

; ------------------------------------------------------------------------------

        INCLUDE "drv_jio.inc"


IFDEF DRV_IPL
; ------------------------------------------------------------------------------

        ; Mandatory symbols defined by the disk hardware interface driver
        PUBLIC	INIHRD
        PUBLIC	DRIVES		; Initialize hardware interface driver
        PUBLIC	INIENV		; Initialize driver environment
        PUBLIC	CHOICE
        PUBLIC	DSKFMT
        PUBLIC	MTOFF
        PUBLIC	OEMSTA
        PUBLIC	DEFDPB
	PUBLIC	MYSIZE
        PUBLIC	SECLEN
        PUBLIC	BOOTMBR
        PUBLIC	BOOTMENU

        EXTERN	GETWRK		; Get address of disk driver's work area
        EXTERN	GETSLT		; Get slot of this interface
	EXTERN	DoCommand

; ------------------------------------------
; INIHRD - Initialize the disk
; Input : None
; Output: None
; May corrupt: AF,BC,DE,HL,IX,IY
; Note: the workbuffer is not available yet so most of the initialization is moved to the DRIVES routine.
; ------------------------------------------
INIHRD:		ld	a,$06
                call	SNSMAT		; Check if CTRL key is pressed
                and	2
                jr	z,r101		; z=yes: exit disk init
	IFDEF UART
		xor	a
		out	(ubase+UART_IER),a	; set uart interrupts off
		ld	a,$80			; set DLAB=1
		out	(ubase+UART_LCR),a
		ld	a,$01			; set divisor low byte (6=19K2, 3=38K4, 2=57K6, 1=115K2 with 1.84Mhz uart clock)
		out	(ubase+UART_DLL),a
		ld	a,$00			; set divisor high byte
		out	(ubase+UART_DLM),a
		ld	a,$03			; set framing to 8N1 / DLAB=0 (3=8N1 7=8N2)
		out	(ubase+UART_LCR),a
		; ld	a,$07			; enable FIFO buffer and reset it's counters
		ld	a,$00			; disable FIFO buffer (when using 16550 with FIFO bug)
		out	(ubase+UART_FCR),a
	ENDIF          
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
DRIVES:
        push	af
        push	bc
        push	de

        ; initialize work buffer
        call	GETWRK			; HL and IX point to work buffer
        ld	d,h
        ld	e,l
        inc	de
        ld	(hl),$00
        ld	bc,MYSIZE-1
        ldir

        call	PrintMsg
	db	12,"JIO "
IFDEF LPTIO
        db	"LPT "
ELIFDEF UART
	db	"UART "
ENDIF
IFDEF IDEDOS1
        db	"MSX-DOS 1",13,10
ELSE
        db	"MSX-DOS 2",13,10
ENDIF
        db	"Rev.: "
        INCLUDE	"rdate.inc"		; Revision date
        db	13,10
        db	"Waiting for server,",13,10
        db	"press [ESC] to cancel",0


DRIVES_Retry:
        ld	a,7
        call	SNSMAT
        and	4
        jr      z,DRIVES_Exit

        ld	a,'.'
        rst	$18

        ld	(ix+W_FLAGS),FLAG_RX_CRC|FLAG_TX_CRC|FLAG_TIMEOUT
        ld      (ix+W_COMMAND),COMMAND_DRIVE_INFO
        ld      b,1
        ld	hl,PART_BUF
        di
        call	DoCommand
        jr	c,DRIVES_Retry

        ld	hl,PART_BUF

        ld	a,(hl)
        ld	(ix+W_FLAGS),a
        inc     hl
        ld      a,(hl)
        ld	(ix+W_DRIVES),a
        inc     hl
        ld      a,(hl)
        ld	(ix+W_BOOTDRV),a
        inc     hl

        call	PrintString

DRIVES_Exit:
        ld	a,(ix+W_DRIVES)
IFDEF IDEDOS1
        or	a
        jr	nz,r206
        inc	a			; Return value of 0 drives is not allowed in DOS 1
r206:
ENDIF
        ld	l,a
        pop     de
        pop     bc
        pop     af
        ret

; ------------------------------------------------------------------------------
; INIENV - Initialize the work area (environment)
; Input : None
; Output: None
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------------------------------------------

INIENV:	call	GETWRK			; HL and IX point to work buffer
        xor	a
        or	(ix+W_DRIVES)		; number of drives 0?
        ret	z
        ld	(ix+W_CURDRV),$ff	; Init current drive
        ld	(ix+W_DSKCHG),$00	; Init partition changed flags

IFNDEF CXDOS1
        call	GETSLT
        ld 	hl,DRVTBL
        ld	b,a			; B = this disk interface slot number
        ld	c,$00

TestInterface:	ld	a,(hl)
        add	a,c
        ld	c,a
        inc	hl
        ld	a,(hl)
        inc	hl
        cp	b			; this interface?
        jr	nz,TestInterface	; nz=no

        dec	hl
        dec	hl
        ld	a,c
        sub	(hl)
        ld	b,(ix+W_BOOTDRV)	; Get boot drive
        add	a,b
        ld	(ix+W_BOOTDRV),a	; Set boot drive
ENDIF
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
SUBRET:
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
; Check for boot code in the MBR, to be used in a modified MSX-DOS boot process.
; Parameters for boot code:
;   hl,ix = pointer to driver workspace
;   a     = master disk major DOS version (1 or 2)
; Output:
;   Zx set ==> show boot choice (if this option is enabled)
;   Cx set ==> start Disk BASIC
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------

; Not implemented in JIO driver
BOOTMBR:	EQU	SUBRET

; ------------------------------------------
; Boot MSX-DOS from selected partition, to be used in a modified MSX-DOS boot process.
; MSX-DOS 1:
; 	The default boot drive is the last primary partition that is flagged active.
;	If DOS1 can't boot from the specified drive the machine will restart or start BASIC.
; MSX-DOS 2:
; 	The default boot drive is the first drive with a valid MSX-DOS 2 boot loader.
;	If DOS2 can't boot from the  specified drive the machine will boot from the first drive or start BASIC.
; ------------------------------------------
        IFNDEF BOOTCHOICE
BOOTMENU:	EQU	SUBRET
        ELSE
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
                ld	hl,BTWAIT		; wait time is defined in disk.inc
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

        ENDIF ; BOOTCHOICE

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
                rst	$18			; print character
                jr      PrintString

; Print CR+LF
PrintCRLF:	ld	a,$0d
                rst	$18
                ld	a,$0a
                rst	$18
                ret

; ------------------------------------------------------------------------------
ENDIF ; DRV_IPL

IFDEF DRV_SYS
; ------------------------------------------------------------------------------

        PUBLIC	DoCommand
	PUBLIC	DSKIO		; Disk I/O routine
        PUBLIC	DSKCHG		; Disk change routine
        PUBLIC	GETDPB

        EXTERN	GETWRK		; Get address of disk driver's work area

        INCLUDE	"drv_jio_c.asm"
        INCLUDE	"crt.asm"

; ------------------------------------------------------------------------------
; DSKIO - Disk Input / Output
; Input:
;   Carry flag = clear ==> read, set ==> write
;   A  = drive number
;   B  = number of sectors to transfer
;   C  = if bit7 is set then media descriptor byte
;	else first logical sector number bit 16..22
;   DE = first logical sector number (bit 0..15)
;   HL = transfer address
; Output:
;   Carry flag = clear ==> successful, set ==> error
;   If error then
;	 A = error code
;           0 - Write protected disk
;           2 - Drive not ready
;           4 - Data (CRC) error
;           6 - Seek error
;           7 - Record not found
;           10 - Write fault (verify error)
;           12 - Other error
;           new 18 - Not a DOS disk
;           new 20 - Incompatible disk
;           new 22 - Unformatted disk
;           new 24 - Unexpected disk change
;
;	 B = remaining sectors
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------------------------------------------

DSKIO:
        di

        push	hl
        push	bc
        push	af
        call	GETWRK
        pop	af
        pop	bc
        pop	hl

        ld      (ix+W_COMMAND),COMMAND_DRIVE_WRITE
        jr	c,WriteFlag
        ld      (ix+W_COMMAND),COMMAND_DRIVE_READ
WriteFlag:

IF (IDEDOS1 && !CXDOS1) || CXDOS2
        ld	(ix+W_DRIVE),a		; save drive number
rw_loop:
        bit	7,h
        jr	nz,rw_multi
        push	bc
        push	de
        ld	b,1
        ld      a,(ix+W_COMMAND)
        cp	COMMAND_DRIVE_READ
        jr	nz,sec_write
        push	hl
        ld	hl,(SSECBUF)
        ld	a,(ix+W_DRIVE)		; load drive number
        call	DoCommand
        pop	de
        jr	c,sec_err
        ld	hl,(SSECBUF)
        ld	bc,$0200
        call	XFER
        ex	de,hl
        jr	sec_next
sec_write:
        push	hl
        push	de
        push	bc
        ld	de,(SSECBUF)
        ld	bc,$0200
        call	XFER
        pop	bc
        pop	de
        ld	hl,(SSECBUF)
        ld	a,(ix+W_DRIVE)		; load drive number
        call	DoCommand
        pop	hl
        jr	c,sec_err
        inc	h
        inc	h
sec_next:
        xor	a
sec_err:
        pop	de
        pop	bc
        ret     c
        inc	e
        jr	nz,sec_loop
        inc	d
        jr	nz,sec_loop
        inc	c
sec_loop:
        djnz	rw_loop
        xor	a
        ret

rw_multi:
ENDIF
        push    bc
        call	DoCommand
        pop     bc
        ret     c
        ld      b,0
        ret

; ------------------------------------------------------------------------------
; DSKCHG - Disk change
; Input:
;   A  = Drive number
;   B  = 0
;   C  = Media descriptor
;   HL = Base address of DPB
; Output:
;   If successful then
;	 Carry flag reset
;	 B = Disk change status
;	 1= Disk unchanged, 0= Unknown, -1=Disk changed
;   else
;	 Carry flag set
;	 Error code in A
; May corrupt: AF,BC,DE,HL,IX,IY
; ------------------------------------------------------------------------------
DSKCHG:
        di
        ld	b,a			; save drive
	push	bc
	push	hl
        call	GETWRK
        ld      (ix+W_COMMAND),COMMAND_DRIVE_DISK_CHANGED
        call	DoCommand
	pop	hl
	pop	bc
        cp      RESULT_DRIVE_DISK_UNCHANGED-1
        jr	z,DiskNotChanged
        cp      RESULT_DRIVE_DISK_CHANGED-1
	jr	z,DiskChanged

DiskChangeError:
        ld      b,0
        scf
        ret

DiskNotChanged:
	call	GetDriveMask
	and	(ix+W_DSKCHG)		; partition changed?
	jr	nz,PartitionChanged	; nz=yes
IFDEF IDEDOS1
	; In IDEDOS1 whenever the current drive is changed this routine returns that the disk has changed in order
	; to flush the FAT cache, this is a deviation from the original MSX-DOS 1.03 without FAT swapper.
	ld	a,b			; restore drive
        cp	(ix+W_CURDRV)		; current drive changed?
        ld	(ix+W_CURDRV),a		; update current drive
        jr	nz,RetChanged
ENDIF
        ld      b,1
	xor	a
	ret

DiskChanged:
	ld	(ix+W_DSKCHG),0xFF	; set changed flag for all partitions
	call	GetDriveMask

PartitionChanged:
	cpl
	and	(ix+W_DSKCHG)
	ld	(ix+W_DSKCHG),a		; clear changed flag for this partition
IFDEF IDEDOS1
	; In DOS1 the DPB of the drive must be updated if the partition has changed
	ld	a,b
        ld	(ix+W_CURDRV),a		; update current drive
	call	GETDPB			; returns updated DPB pointed to by HL
	jr	c,DiskChangeError	; if cx then error reading boot sector
ENDIF

RetChanged:
        ld	b,0xFF
        xor	a
        ret

GetDriveMask:
	push	hl
	ld	hl,masks
	ld	e,b
	ld	d,0
	add	hl,de
	ld	a,(hl)
	pop	hl
	ret

masks:	db	0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80

; ------------------------------------------------------------------------------
; Input: CDE = Sector
;          B = Length
;         HL = Address
; ------------------------------------------------------------------------------

DoCommand:
        push    ix              ; _pucFlagsAndCommand
        push    hl              ; _pvAddress
        push    bc              ; _uiLength

        ld      b,a             ; _ulSector in BCDE

        call    ucDoCommand
        pop	hl
        pop	hl
        pop	hl

        or      a
        ret     z
        dec     a
        scf
        ret

; ------------------------------------------------------------------------------
; Transmit data on joystick 2 port or lpt port (LPTIO) or 1655X (UART)
; Input: DE = DATA
;        BC = LENGTH
; ------------------------------------------------------------------------------
IFDEF UART
vJIOTransmit:
	ex	de,hl
uart_send:
	;in	a,(ubase+UART_MSR)	; use FIFO buffer
	;bit	4,a			; cts: clear to send?
	in	a,(ubase+UART_LSR)	; don't use FIFO buffer
	bit	5,a			; thr: transmitter ready?
	jr	z,uart_send		; z=no
	ld	a,(hl)
	out	(ubase+UART_THR),a	; send data
	inc	hl
	dec	bc
	ld	a,b
	or	c
	jr	nz,uart_send
	ret

ELSE
vJIOTransmit:
        exx
        push    bc
        push    de
        exx

IFDEF LPTIO
	push	ix
	call	vJIOTransmit2
	pop	ix
ELSE
        call vJIOTransmit2
ENDIF

        exx
        pop     de
        pop     bc
        exx
        ret

vJIOTransmit2:
        ex      de,hl
        inc	bc
        exx
IFDEF LPTIO
	ld	e,1
	ld	d,0
	ld	c,$90
ELSE
        ld	a,15
        out	($a0),a
        in	a,($a2)
        or	4
        ld	e,a
        xor	4
        ld	d,a
        ld	c,$a1
ENDIF

        db	$3e
JIOTxLoop:
	ret	nz
        out	(c),e		; 14
IFDEF LPTIO
	; one more stop bit
	add	ix,ix		; 17
	out	(c),e
ENDIF
        exx
        ld	a,(hl)
        cpi
        ret	po
        exx
        rrca
        out	(c),d		; =0
        ret	nz
        jp	c,Tx10
        out	(c),d		; -0
        rrca
        jp	c,Tx11

; ------------------------------------------------------------------------------

Tx01:	out	(c),d		; -1
        rrca
        jr	c,Tx12
        nop

Tx02:	out	(c),d		; -0
        rrca
        jp	c,Tx13

Tx03:	out	(c),d		; -1
        rrca
        jr	c,Tx14
        nop

Tx04:	out	(c),d		; -0
        rrca
        jp	c,Tx15

Tx05:	out	(c),d		; -1
        rrca
        jr	c,Tx16
        nop

Tx06:	out	(c),d		; -0
        rrca
        jp	c,Tx17

Tx07:	out	(c),d		; -1
        jp	JIOTxLoop

; ------------------------------------------------------------------------------

Tx10:	out	(c),e		; -0
        rrca
        jp	nc,Tx01

Tx11:	out	(c),e		; -1
        rrca
        jr	nc,Tx02
        nop

Tx12:	out	(c),e		; -0
        rrca
        jp	nc,Tx03

Tx13:	out	(c),e		; -1
        rrca
        jr	nc,Tx04
        nop

Tx14:	out	(c),e		; -0
        rrca
        jp	nc,Tx05

Tx15:	out	(c),e		; -1
        rrca
        jr	nc,Tx06
        nop

Tx16:	out	(c),e		; -0
        rrca
        jp	nc,Tx07

Tx17:	out	(c),e		; -1
        jp	JIOTxLoop

ENDIF ; UART

; ------------------------------------------------------------------------------
; Receive data on joystick 2 port or lpt port (LPTIO) or 1655X (UART)
; Input: DE = DATA
;        BC = LENGTH
; ------------------------------------------------------------------------------
IFDEF UART
bJIOReceive:
	ex	de,hl
	in	a,(ubase+UART_MCR)
	or	%00000010		; RTS on
	out	(ubase+UART_MCR),a

uart_sync1:	
	call	rcv_ready
	in	a,(ubase+UART_RBR)	; get header byte
	cp	$ff
	jr	nz,uart_sync1

uart_sync2:	
	call	rcv_ready
	in	a,(ubase+UART_RBR)	; get header byte
	cp	$f0			; sync?
	jr	z,rcv_loop		; nz=no
	cp	$ff
	jr	z,uart_sync2
	jr	uart_sync1
	
rcv_loop:
	call	rcv_ready
	in	a,(ubase+UART_RBR)	; receive data
	ld	(hl),a
	inc	hl
	dec	bc			; data counter
	ld	a,b
	or	c
	jr	nz,rcv_loop
	in	a,(ubase+UART_MCR)
	and	%11111101		; RTS off
	out	(ubase+UART_MCR),a
	ld	a,1
	ret

rcv_ready:
	ld	de,$0000		; time-out counter
ready_loop:
	in	a,(ubase+UART_LSR)
	bit	0,a			; receiver data ready?
	ret	nz
	dec	de
	ld	a,d
	or	e
	jr	nz,ready_loop
	pop	af			; ditch return address
	in	a,(ubase+UART_MCR)
	and	%11111101		; RTS off
	out	(ubase+UART_MCR),a
	xor	a
	ret

ELSE
bJIOReceive:
        ld      h,d
        ld      l,e
        ld      d,b
        ld      e,c

        push	ix
        push	de

        ld      de,0
	dec	hl
        ld	ix,0
        add	ix,sp

IFDEF LPTIO
	; probe the lpt port to determine receive method:
	; 1. unused bits 0,2..7 are last written value on databus or always 0
	; 2. unused bits 0,2..7 are always 1
	ld	c,$90		; lpt i/o port
        out	(c),d		; clear z80 data bus (d=0)
        in	a,(c)
        and	$fd
        jr	z,HeaderPO	; method 1
        cp	$fd
        jr	z,HeaderPE	; method 2
        jr	RxTimeOut	; other, not supported
ELSE
        ld	c,$a2
        ld	a,15		; PSG r15
        out	($a0),a
        in	a,($a2)
        or	64		; bit 6: select joystick 2
        out	($a1),a
        ld	a,14		; PSG r14
        out	($a0),a
        in	a,($a2)
        or	1		; bit 0: rx pin 1
        jp	pe,HeaderPE
ENDIF
        
; ------------------------------------------------------------------------------

HeaderPO:	
	dec	de		;  7
        ld	a,d		;  5
        or	e		;  5
        jr	z,RxTimeOut 	;  8

        in	f,(c)		; 14
        jp	po,HeaderPO	; 11   LOOP=50 (2-CLOCKS)
        rlc	a		; 10
        in	f,(c)		; 14
        jp	po,HeaderPO	; 11   At least 2 clocks needed to be down

WU_PO:	in	f,(c)		; 14
        jp	pe,WU_PO	; 11   LOOP=25
        pop	de		; 11
        push	de		; 11

RX_PO:	in	f,(c)		; 14
        jp	po,RX_PO	; 11   LOOP=25

	;timing alternatives:
	;ret	po		;  6 = 31 CYCLES
        ;ld	sp,hl		;  7  = 32 CYCLES
        ld	b,(hl)		;  8  = 33 CYCLES
	;neg			; 10 =  35 CYCLES
        
        in	a,(c)		; 14   Bit 0
        nop			;  5
        rrca			;  5
        dec	de		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 1
        xor	b		;  5
        rrca			;  5
        inc	hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 2
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 3
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 4
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 5
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 6
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 7
        xor	b		;  5
        rrca			;  5

IFDEF LPTIO
	rrca			;  5 extra rotate because rx is bit 1
        ld	(hl),a		;  8 instruction uses data bus write
        xor	a		;  5 
        out	($90),a		; 12 clear z80 data bus
ELSE
        ld	(hl),a		;  8
ENDIF

	ld	a,d		;  5
	or	e		;  5
	jp	nz,RX_PO	; 11
     
; ------------------------------------------------------------------------------

ReceiveOK:
        ld	sp,ix
        pop	de
        pop	ix
        ld      a,1
        ret

RxTimeOut:
        pop	de
        pop	ix
        xor     a
        ret

; ------------------------------------------------------------------------------

HeaderPE:	
	dec	de		;  7
        ld	a,d		;  5
        or	e		;  5
        jr	z,RxTimeOut	;  8

        in	f,(c)		; 14
        jp	pe,HeaderPE	; 11   LOOP= 50 (2-CLOCKS)
        rlc	a		; 10
        in	f,(c)		; 14
        jp	pe,HeaderPE	; 11   At least 2 clocks needed to be down

WU_PE:	in	f,(c)		; 14
        jp	po,WU_PE	; 11   LOOP=25
        pop	de		; 11
        push	de		; 11

RX_PE:	in	f,(c)		; 14
        jp	pe,RX_PE	; 11   LOOP=25
	
	;timing alternatives:
	;ret	pe		;  6 = 31 CYCLES
        ;ld	sp,hl		;  7  = 32 CYCLES
	ld	b,(hl)		;  8  = 33 CYCLES
	;neg			; 10 =  35 CYCLES

        in	a,(c)		; 14   Bit 0
        cpl			;  5
        rrca			;  5
        dec	de		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 1
        xor	b		;  5
        rrca			;  5
        inc	hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 2
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 3
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 4
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 5
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 6
        xor	b		;  5
        rrca			;  5
        ld	sp,hl		;  7 = 31 CYCLES

        in	b,(c)		; 14   Bit 7
        xor	b		;  5
        rrca			;  5

IFDEF LPTIO
	rrca			;  5 extra rotate because rx is bit 1
ENDIF
	ld	(hl),a		;  8 instruction uses data bus write

	ld	a,d		;  5
        or	e		;  5
        jp	nz,RX_PE	; 11

        jr	ReceiveOK 

ENDIF ; UART

; ------------------------------------------------------------------------------

; Compute xmodem CRC-16
; Input:  DE    = buffer
;         BC    = bytes
;         Stack = CRC-16
; Output: HL    = updated CRC-16

uiXModemCRC16:
        ld	l,c
        ld	h,b
        ld	b,l
        dec	hl
        inc	h
        ld	c,h

        push    ix
        ld      ix,0
        add     ix,sp
        ld      l,(ix+4)
        ld      h,(ix+5)
        pop     ix

        ex      af,af'
        push    af
        ex      af,af'

IF !(CXDOS1 || CXDOS2)
; compute CRC with lookup table
crc16:	ld	a,l
        ex	af,af'
        ld	a,(de)
        inc	de
        xor	h
        ld	h,CrcTab/256
        ld	l,a
        ex	af,af'
        xor	(hl)
        inc	h
        ld	l,(hl)
        ld	h,a
        djnz	crc16
        dec	c
        jp	nz,crc16

ELSE
; compute CRC without lookup table
crc16:	push bc
	ld	a,(de)
	inc	de
	xor     h
	ld      b,a
	ld      c,l
	rrca
	rrca
	rrca
	rrca
	ld      l,a
	and     0fh
	ld      h,a
	xor     b
	ld      b,a
	xor     l
	and     0f0h
	ld      l,a
	xor     c
	add     hl,hl
	xor     h
	ld      h,a
	ld      a,l
	xor     b
	ld      l,a
	pop     bc
        djnz	crc16
        dec	c
        jp	nz,crc16
ENDIF
        ex      af,af'
        pop     af
        ex      af,af'
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
        IFNDEF IDEDOS1
GETDPB:		ret
        ELSE
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
        ENDIF

IF !(CXDOS1 || CXDOS2)
        SECTION	DRV_CRCTAB

        ORG	$7E00	; align to 256-byte page boundary

CrcTab:	; high bytes
        db	000h,010h,020h,030h,040h,050h,060h,070h
        db	081h,091h,0A1h,0B1h,0C1h,0D1h,0E1h,0F1h
        db	012h,002h,032h,022h,052h,042h,072h,062h
        db	093h,083h,0B3h,0A3h,0D3h,0C3h,0F3h,0E3h
        db	024h,034h,004h,014h,064h,074h,044h,054h
        db	0A5h,0B5h,085h,095h,0E5h,0F5h,0C5h,0D5h
        db	036h,026h,016h,006h,076h,066h,056h,046h
        db	0B7h,0A7h,097h,087h,0F7h,0E7h,0D7h,0C7h
        db	048h,058h,068h,078h,008h,018h,028h,038h
        db	0C9h,0D9h,0E9h,0F9h,089h,099h,0A9h,0B9h
        db	05Ah,04Ah,07Ah,06Ah,01Ah,00Ah,03Ah,02Ah
        db	0DBh,0CBh,0FBh,0EBh,09Bh,08Bh,0BBh,0ABh
        db	06Ch,07Ch,04Ch,05Ch,02Ch,03Ch,00Ch,01Ch
        db	0EDh,0FDh,0CDh,0DDh,0ADh,0BDh,08Dh,09Dh
        db	07Eh,06Eh,05Eh,04Eh,03Eh,02Eh,01Eh,00Eh
        db	0FFh,0EFh,0DFh,0CFh,0BFh,0AFh,09Fh,08Fh
        db	091h,081h,0B1h,0A1h,0D1h,0C1h,0F1h,0E1h
        db	010h,000h,030h,020h,050h,040h,070h,060h
        db	083h,093h,0A3h,0B3h,0C3h,0D3h,0E3h,0F3h
        db	002h,012h,022h,032h,042h,052h,062h,072h
        db	0B5h,0A5h,095h,085h,0F5h,0E5h,0D5h,0C5h
        db	034h,024h,014h,004h,074h,064h,054h,044h
        db	0A7h,0B7h,087h,097h,0E7h,0F7h,0C7h,0D7h
        db	026h,036h,006h,016h,066h,076h,046h,056h
        db	0D9h,0C9h,0F9h,0E9h,099h,089h,0B9h,0A9h
        db	058h,048h,078h,068h,018h,008h,038h,028h
        db	0CBh,0DBh,0EBh,0FBh,08Bh,09Bh,0ABh,0BBh
        db	04Ah,05Ah,06Ah,07Ah,00Ah,01Ah,02Ah,03Ah
        db	0FDh,0EDh,0DDh,0CDh,0BDh,0ADh,09Dh,08Dh
        db	07Ch,06Ch,05Ch,04Ch,03Ch,02Ch,01Ch,00Ch
        db	0EFh,0FFh,0CFh,0DFh,0AFh,0BFh,08Fh,09Fh
        db	06Eh,07Eh,04Eh,05Eh,02Eh,03Eh,00Eh,01Eh

        ;low bytes
        db	000h,021h,042h,063h,084h,0A5h,0C6h,0E7h
        db	008h,029h,04Ah,06Bh,08Ch,0ADh,0CEh,0EFh
        db	031h,010h,073h,052h,0B5h,094h,0F7h,0D6h
        db	039h,018h,07Bh,05Ah,0BDh,09Ch,0FFh,0DEh
        db	062h,043h,020h,001h,0E6h,0C7h,0A4h,085h
        db	06Ah,04Bh,028h,009h,0EEh,0CFh,0ACh,08Dh
        db	053h,072h,011h,030h,0D7h,0F6h,095h,0B4h
        db	05Bh,07Ah,019h,038h,0DFh,0FEh,09Dh,0BCh
        db	0C4h,0E5h,086h,0A7h,040h,061h,002h,023h
        db	0CCh,0EDh,08Eh,0AFh,048h,069h,00Ah,02Bh
        db	0F5h,0D4h,0B7h,096h,071h,050h,033h,012h
        db	0FDh,0DCh,0BFh,09Eh,079h,058h,03Bh,01Ah
        db	0A6h,087h,0E4h,0C5h,022h,003h,060h,041h
        db	0AEh,08Fh,0ECh,0CDh,02Ah,00Bh,068h,049h
        db	097h,0B6h,0D5h,0F4h,013h,032h,051h,070h
        db	09Fh,0BEh,0DDh,0FCh,01Bh,03Ah,059h,078h
        db	088h,0A9h,0CAh,0EBh,00Ch,02Dh,04Eh,06Fh
        db	080h,0A1h,0C2h,0E3h,004h,025h,046h,067h
        db	0B9h,098h,0FBh,0DAh,03Dh,01Ch,07Fh,05Eh
        db	0B1h,090h,0F3h,0D2h,035h,014h,077h,056h
        db	0EAh,0CBh,0A8h,089h,06Eh,04Fh,02Ch,00Dh
        db	0E2h,0C3h,0A0h,081h,066h,047h,024h,005h
        db	0DBh,0FAh,099h,0B8h,05Fh,07Eh,01Dh,03Ch
        db	0D3h,0F2h,091h,0B0h,057h,076h,015h,034h
        db	04Ch,06Dh,00Eh,02Fh,0C8h,0E9h,08Ah,0ABh
        db	044h,065h,006h,027h,0C0h,0E1h,082h,0A3h
        db	07Dh,05Ch,03Fh,01Eh,0F9h,0D8h,0BBh,09Ah
        db	075h,054h,037h,016h,0F1h,0D0h,0B3h,092h
        db	02Eh,00Fh,06Ch,04Dh,0AAh,08Bh,0E8h,0C9h
        db	026h,007h,064h,045h,0A2h,083h,0E0h,0C1h
        db	01Fh,03Eh,05Dh,07Ch,09Bh,0BAh,0D9h,0F8h
        db	017h,036h,055h,074h,093h,0B2h,0D1h,0F0h
ENDIF

; ------------------------------------------------------------------------------
ENDIF ; DRV_SYS
