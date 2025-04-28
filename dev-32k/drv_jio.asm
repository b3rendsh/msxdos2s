; ------------------------------------------------------------------------------
; drv_jio.asm
;
; Copyright (C) 2025 All rights reserved
; JIO MSX-DOS 2 driver by Louthrax
; JIO MSX-DOS 1 driver and CRC routines by H.J. Berends
; 115K2 transmit/receive routines based on code by Nyyrikki
; ------------------------------------------------------------------------------
; Work in progress!!!
; ------------------------------------------------------------------------------

        INCLUDE	"disk.inc"	; Assembler directives
        INCLUDE	"msx.inc"	; MSX constants and definitions

; ----------------------------------------
; Included file 'drv_jio.inc'
; ----------------------------------------

#define FLAG_RX_CRC                 (1 << 0)
#define FLAG_TX_CRC                 (1 << 1)
#define FLAG_TIMEOUT                (1 << 2)
#define FLAG_AUTO_RETRY             (1 << 3)

#define COMMAND_DRIVE_REPORT_OK		       0
#define COMMAND_DRIVE_REPORT_WRITE_PROTECTED   0+1
#define COMMAND_DRIVE_REPORT_DRIVE_NOT_READY   2+1
#define COMMAND_DRIVE_REPORT_CRC_ERROR         4+1
#define COMMAND_DRIVE_REPORT_WRITE_FAULT       10+1

#define COMMAND_DRIVE_READ		      16
#define COMMAND_DRIVE_WRITE		      17
#define COMMAND_DRIVE_INFO		      18
#define COMMAND_DRIVE_DISK_CHANGED	      19
#define RESULT_DRIVE_DISK_CHANGED             20
#define RESULT_DRIVE_DISK_UNCHANGED           21

#define DRIVE_ANSWER_WRITE_OK                 0x1111
#define DRIVE_ANSWER_WRITE_FAILED             0x2222
#define DRIVE_ANSWER_WRITE_PROTECTED          0x3333
#define DRIVE_ANSWER_DISK_CHANGED             0x4444
#define DRIVE_ANSWER_DISK_UNCHANGED           0x5555

        SECTION	DRV_JIO

        ; Mandatory symbols defined by the disk hardware interface driver
        PUBLIC	DRIVES	; Initialize hardware interface driver
        PUBLIC	INIENV	; Initialize driver environment
        PUBLIC	DSKIO	; Disk I/O routine
        PUBLIC	DSKCHG	; Disk change routine
        PUBLIC	GETDPB
        PUBLIC	CHOICE
        PUBLIC	DSKFMT
        PUBLIC	MTOFF
        PUBLIC	OEMSTA
        PUBLIC	DEFDPB
        PUBLIC	SECLEN
        PUBLIC	INIHRD
        PUBLIC	BOOTMBR
        PUBLIC	BOOTMENU

        EXTERN	GETWRK	; Get address of disk driver's work area
        EXTERN	GETSLT	; Get slot of this interface
        EXTERN	MYSIZE	; "

; Hardware driver variables

W_CURDRV	equ	$0	; Current drive
W_BOOTDRV	equ	$1	; Boot drive (partition)
W_DRIVES	equ	$2	; Number of drives (partitions) on disk
W_FLAGS		equ	$3
W_COMMAND	equ	$4
W_DRIVE		equ	$5	; DSKIO save drive number (DOS1)
MYSIZE		equ	$6

SECLEN		equ	512
PART_BUF	equ	TMPSTK		; Copy of disk info / Master Boot Record

CHGCPU          equ	$180
GETCPU          equ	$183


; ----------------------------------------
; Included file 'drv_jio_c.asm'
; ----------------------------------------
uiTransmit:
	CALL	_ENT_PARM_DIRECT_L09
	BIT	1,(IX+8)
	JR	Z,_0001
_0000:
	LD	L,(IX+10)
	LD	H,(IX+11)
	PUSH	HL
	CALL	uiXModemCRC16
	POP	AF
	LD	(IX+10),L
	LD	(IX+11),H
_0001:
	LD	C,(IX+4)
	LD	B,(IX+5)
	LD	E,(IX+2)
	LD	D,(IX+3)
	CALL	vJIOTransmit
	XOR	A
	OR	(IX+12)
	JR	Z,_0003
	BIT	1,(IX+8)
	JR	Z,_0003
_0005:
_0004:
_0002:
	LD	BC,2
	LD	HL,10
	ADD	HL,SP
	EX	DE,HL
	CALL	vJIOTransmit
_0003:
	LD	L,(IX+10)
	LD	H,(IX+11)
	JP	_LEAVE_DIRECT_L09
ucReceive:
	CALL	_ENT_PARM_DIRECT_L09
_0058:
_0007:
	LD	C,(IX+4)
	LD	B,(IX+5)
	LD	E,(IX+2)
	LD	D,(IX+3)
	CALL	bJIOReceive
	OR	A
	JR	NZ,_0006
_0008:
	BIT	2,(IX+8)
	JR	Z,_0058
_0009:
	LD	A,3
	JR	_0011
_0010:
_0006:
	XOR	A
_0011:
	JP	_LEAVE_DIRECT_L09
ucDoCommand:
	CALL	_ENT_AUTO_DIRECT_L09
	DEFW	65510
	PUSH	IY
	LD	HL,W_FLAGS
	LD	C,(IX+12)
	LD	B,(IX+13)
	ADD	HL,BC
	LD	D,(HL)
	LD	IYH,D
	LD	HL,W_COMMAND
	ADD	HL,BC
	LD	B,(HL)
	LD	(IX-4),B
	LD	(IX-18),74
	LD	(IX-17),73
	LD	(IX-16),79
	LD	(IX-15),D
	LD	HL,36
	ADD	HL,SP
	INC	HL
	LD	B,(HL)
	DEC	HL
	LD	(HL),B
	LD	C,(IX+4)
	LD	B,(IX+5)
	LD	L,(IX+2)
	LD	H,(IX+3)
	LD	(IX-13),L
	LD	(IX-12),H
	LD	(IX-11),C
	LD	(IX-10),B
	LD	B,(IX+8)
	LD	(IX-9),B
	LD	L,(IX+10)
	LD	H,(IX+11)
	LD	(IX-8),L
	LD	(IX-7),H
	LD	L,B
	LD	H,L
	LD	L,0
	ADD	HL,HL
	LD	(IX-26),L
	LD	(IX-25),H
_0014:
	LD	B,(IX-4)
	LD	(IX-14),B
	LD	A,B
	CP	18
	JR	Z,_0016
	CP	19
	JR	Z,_0016
	XOR	A
	JR	_0017
_0016:
	LD	A,1
_0017:
	LD	C,A
	PUSH	BC
	LD	HL,0
	PUSH	HL
	LD	C,IYH
	PUSH	BC
	LD	BC,5
	LD	L,16
	ADD	HL,SP
	EX	DE,HL
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
	LD	A,(IX-4)
	CP	19
	JR	NZ,_0019
_0018:
	LD	C,IYH
	PUSH	BC
	LD	BC,2
	LD	HL,27
	ADD	HL,SP
	EX	DE,HL
	CALL	ucReceive
	POP	HL
	LD	IYL,A
	OR	A
	JR	NZ,_0027
_0020:
	LD	HL,17476
	LD	C,(IX-3)
	LD	B,(IX-2)
	AND	A
	SBC	HL,BC
	JR	NZ,_0023
_0022:
	LD	IYL,20
	JR	_0027
_0023:
	LD	HL,21845
	AND	A
	SBC	HL,BC
	JR	NZ,_0026
_0025:
	LD	IYL,21
	JR	_0027
_0026:
	LD	IYL,5
_0027:
_0024:
_0021:
	JP	_0051
_0019:
	CP	17
	JR	NZ,_0030
_0029:
	LD	C,0
	PUSH	BC
	PUSH	HL
	LD	C,IYH
	PUSH	BC
	LD	BC,7
	LD	HL,21
	ADD	HL,SP
	EX	DE,HL
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
	LD	C,1
	PUSH	BC
	PUSH	HL
	LD	C,IYH
	PUSH	BC
	LD	C,(IX-26)
	LD	B,(IX-25)
	LD	E,(IX+10)
	LD	D,(IX+11)
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
	LD	C,IYH
	PUSH	BC
	LD	BC,2
	LD	HL,27
	ADD	HL,SP
	EX	DE,HL
	CALL	ucReceive
	POP	HL
	LD	IYL,A
	OR	A
	JR	NZ,_0040
_0031:
	LD	HL,8738
	LD	C,(IX-3)
	LD	B,(IX-2)
	AND	A
	SBC	HL,BC
	JR	NZ,_0034
_0033:
	LD	HL,13107
	AND	A
	SBC	HL,BC
	JR	NZ,_0036
	LD	A,1
	JR	_0037
_0036:
	LD	A,11
_0037:
	LD	IYL,A
	JR	_0051
_0034:
	LD	HL,4369
	AND	A
	SBC	HL,BC
	JR	Z,_0051
_0039:
	LD	IYL,5
_0040:
_0038:
_0032:
	JR	_0051
_0030:
	CP	16
	JR	NZ,_0043
_0042:
	LD	C,1
	PUSH	BC
	PUSH	HL
	LD	C,IYH
	PUSH	BC
	LD	BC,7
	LD	HL,21
	ADD	HL,SP
	EX	DE,HL
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
_0043:
	LD	C,IYH
	PUSH	BC
	LD	C,(IX-26)
	LD	B,(IX-25)
	LD	E,(IX+10)
	LD	D,(IX+11)
	CALL	ucReceive
	POP	HL
	LD	IYL,A
	OR	A
	JR	NZ,_0051
	LD	B,IYH
	BIT	0,B
	JR	Z,_0051
_0047:
_0046:
_0044:
	LD	C,IYH
	PUSH	BC
	LD	BC,2
	LD	HL,6
	ADD	HL,SP
	EX	DE,HL
	CALL	ucReceive
	POP	HL
	LD	IYL,A
	OR	A
	JR	NZ,_0051
_0048:
	LD	L,A
	LD	H,A
	PUSH	HL
	LD	C,(IX-26)
	LD	B,(IX-25)
	LD	E,(IX+10)
	LD	D,(IX+11)
	CALL	uiXModemCRC16
	POP	AF
	LD	C,(IX-24)
	LD	B,(IX-23)
	AND	A
	SBC	HL,BC
	JR	Z,_0051
_0050:
	LD	IYL,5
_0051:
_0049:
_0045:
_0041:
_0028:
	LD	B,IYL
	INC	B
	DEC	B
	JR	Z,_0053
	LD	A,IYL
	CP	20
	JR	Z,_0053
	CP	21
	JR	Z,_0053
_0055:
_0054:
_0052:
	LD	(IX-14),B
	LD	C,1
	PUSH	BC
	LD	HL,0
	PUSH	HL
	LD	C,IYH
	PUSH	BC
	LD	BC,5
	LD	L,16
	ADD	HL,SP
	EX	DE,HL
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
_0053:
	LD	B,IYL
	INC	B
	DEC	B
	JR	Z,_0012
	LD	A,IYL
	CP	20
	JR	Z,_0012
	CP	21
	JR	Z,_0012
	LD	B,IYH
	BIT	3,B
	JP	NZ,_0014
_0012:
	LD	A,IYL
	POP	IY
	JP	_LEAVE_DIRECT_L09

; ----------------------------------------
; Included file 'crt.asm'
; ----------------------------------------

_ENT_AUTO_DIRECT_L09:
        pop     hl
        push    bc
        push    de
        push    ix
        ld      ix,0
        add     ix,sp
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        ex      de,hl
        add     hl,sp
        ld      sp,hl
        ex      de,hl
        jp      (hl)

_LEAVE_DIRECT_L09:
        ld      sp,ix
        pop     ix
        pop     de
        pop     bc
        ret

_ENT_PARM_DIRECT_L09:
        pop     hl
        push    bc
        push    de
        push    ix
        ld      ix,0
        add     ix,sp
        jp      (hl)

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
IFDEF IDEDOS1
        db	12,"JIO MSX-DOS 1",13,10
ELSE
        db	12,"JIO MSX-DOS 2",13,10
ENDIF
        db	"Rev.: "
        INCLUDE	"rdate.inc"	; Revision date
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

;********************************************************************************************************************************
; INIENV - Initialize the work area (environment)
; Input : None
; Output: None
; May corrupt: AF,BC,DE,HL,IX,IY
;********************************************************************************************************************************

INIENV:	call	GETWRK	; HL and IX point to work buffer
        xor	a
        or	(ix+W_DRIVES)	; number of drives 0?
        ret	z
        ld	(ix+W_CURDRV),$ff	; Init current drive
        call	GETSLT
        ld 	hl,DRVTBL
        ld	b,a	; B = this disk interface slot number
        ld	c,$00

TestInterface:	ld	a,(hl)
        add	a,c
        ld	c,a
        inc	hl
        ld	a,(hl)
        inc	hl
        cp	b	; this interface?
        jr	nz,TestInterface	; nz=no

        dec	hl
        dec	hl
        ld	a,c
        sub	(hl)
        ld	b,(ix+W_BOOTDRV)	; Get boot drive
        add	a,b
        ld	(ix+W_BOOTDRV),a	; Set boot drive
        ret

;********************************************************************************************************************************
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
;********************************************************************************************************************************

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

IFDEF IDEDOS1
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

;********************************************************************************************************************************
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
;********************************************************************************************************************************
DSKCHG:
IFDEF IDEDOS1 
	; In IDEDOS1 whenever the current drive is changed this routine returns that the disk has changed in order
	; to flush the FAT cache, this is a deviation from the original MSX-DOS 1.03 without FAT swapper.
	; In case of multiple drives (i.e. fixed disk) it is assumed that the DPB for each drive is never changed.
	; The initial value for ix+W_CURDRV is 0xFF (set in INIENV) to make sure that the FAT cache is flushed at boot.
        di
        ld	b,a			; save drive
	push	bc
	push	hl
        call	GETWRK
	pop	hl
	pop	bc
	ld	a,b			; restore drive
        cp	(ix+W_CURDRV)		; current drive
        ld	(ix+W_CURDRV),a
        jr	nz,DiskChanged

        ld      (ix+W_COMMAND),COMMAND_DRIVE_DISK_CHANGED
	push	bc
	push	hl
        call	DoCommand
	pop	hl
	pop	bc
        cp      RESULT_DRIVE_DISK_UNCHANGED-1
        jr	z,DiskNotChanged
        cp      RESULT_DRIVE_DISK_CHANGED-1
	jr	nz,DiskChangeError

; Update DPB if disk for current drive has changed
UpdateDPB:
	ld	a,b			; restore drive
	call	GETDPB			; returns updated DPB pointed to by HL
	jr	c,DiskChangeError	; if cx then error reading boot sector

DiskChanged:
        ld	b,0xFF			; disk (drive) changed
        xor	a
        ret

DiskNotChanged:
        ld      b,1
	xor	a
	ret

DiskChangeError:
        ld      b,0
        scf
        ret

ELSE
	; DOS 2 uses internal routines to detect disk change by comparing serial numbers and media byte.
        ; Always return unchanged for DOS2 (disks are not hot-pluggable)
        ld	b,$01
        xor	a
        ret
ENDIF


;********************************************************************************************************************************

; CDE : Sector
; B   : Length
; HL  : Address

DoCommand:
        push    ix              ; _pucFlagsAndCommand
        push    hl              ; _pvAddress
        push    bc              ; _uiLength

        ld      b,a             ; _ulSector in BCDE

        call    ucDoCommand
        pop hl
        pop hl
        pop hl

        or      a
        ret     z
        dec     a
        scf
        ret

;********************************************************************************************************************************
; IN:  HL = DATA
;      BC = LENGTH
;********************************************************************************************************************************

vJIOTransmit:
        exx
        push    bc
        push    de
        exx

        call vJIOTransmit2

        exx
        pop     de
        pop     bc
        exx
        ret

vJIOTransmit2:
        ex      de,hl
        inc	bc
        exx
        ld	a,15
        out	($a0),a
        in	a,($a2)
        or	4
        ld	e,a
        xor	4
        ld	d,a
        ld	c,$a1

        db	$3e
JIOTransmitLoop:	ret	nz
        out	(c),e
        exx
        ld	a,(hl)
        cpi
        ret	po
        exx
        rrca
        out	(c),d	; =0
        ret	nz
        jp	c,TRANSMIT10
        out	(c),d	; -0
        rrca
        jp	c,TRANSMIT11
;________________________________________________________________________________________________________________________________

TRANSMIT01:	out	(c),d	; -1
        rrca
        jr	c,TRANSMIT12
        nop

TRANSMIT02:	out	(c),d	; -0
        rrca
        jp	c,TRANSMIT13

TRANSMIT03:	out	(c),d	; -1
        rrca
        jr	c,TRANSMIT14
        nop

TRANSMIT04:	out	(c),d	; -0
        rrca
        jp	c,TRANSMIT15

TRANSMIT05:	out	(c),d	; -1
        rrca
        jr	c,TRANSMIT16
        nop

TRANSMIT06:	out	(c),d	; -0
        rrca
        jp	c,TRANSMIT17

TRANSMIT07:	out	(c),d	; -1
        jp	JIOTransmitLoop
;________________________________________________________________________________________________________________________________

TRANSMIT10:	out	(c),e	; -0
        rrca
        jp	nc,TRANSMIT01

TRANSMIT11:	out	(c),e	; -1
        rrca
        jr	nc,TRANSMIT02
        nop

TRANSMIT12:	out	(c),e	; -0
        rrca
        jp	nc,TRANSMIT03

TRANSMIT13:	out	(c),e	; -1
        rrca
        jr	nc,TRANSMIT04
        nop

TRANSMIT14:	out	(c),e	; -0
        rrca
        jp	nc,TRANSMIT05

TRANSMIT15:	out	(c),e	; -1
        rrca
        jr	nc,TRANSMIT06
        nop

TRANSMIT16:	out	(c),e	; -0
        rrca
        jp	nc,TRANSMIT07

TRANSMIT17:	out	(c),e	; -1
        jp	JIOTransmitLoop

;********************************************************************************************************************************
;********************************************************************************************************************************

bJIOReceive:
        ld      h,d
        ld      l,e
        ld      d,b
        ld      e,c

        push	ix
        push	de

        ld      de,0

        dec	hl
        ld	b,(hl)	; What if HL=0 ?
        ld	c,$a2
        ld	ix,0
        add	ix,sp
        ld	a,15
        out	($a0),a
        in	a,($a2)
        or	64
        out	($a1),a
        ld	a,14
        out	($a0),a
        in	a,($a2)
        or	1
        jp	pe,HeaderPE
;________________________________________________________________________________________________________________________________

HeaderPO:	dec	de	;  7
        ld	a,d	;  5
        or	e	;  5
        jr	z,ReceiveTimeOut	;  8

        in	f,(c)	; 14
        jp	po,HeaderPO	; 11   LOOP=50 (2-CLOCKS)
        rlc	a
        in	f,(c)	; 14
        jp	po,HeaderPO	; 11   At least 2 clocks needed to be down

WU_PO:	in	f,(c)	; 14
        jp	pe,WU_PO	; 11   LOOP=25
        pop	de
        push	de

RX_PO:	in	f,(c)	; 14
        jp	po,RX_PO	; 11   LOOP=25
        ld	(hl),b	;  8  = 33 CYCLES

        in	a,(c)	; 14   Bit 0
        nop		;  5
        rrca		;  5
        dec	de	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 1
        xor	b	;  5
        rrca		;  5
        inc	hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 2
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 3
        xor	b	;  5
        rrca	                             	;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 4
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 5
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 6
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 7
        xor	b	;  5
        rrca		;  5

        ld	b,a	;  5
        ld	a,d	;  5
        or	e	;  5
        jp	nz,RX_PO	; 11
;________________________________________________________________________________________________________________________________

ReceiveOK:
        ld  (hl),b
        ld	sp,ix

        pop	de
        pop	ix
        ld      a,1
        ret

ReceiveTimeOut:
        pop	de
        pop	ix
        xor     a
        ret

;________________________________________________________________________________________________________________________________

HeaderPE:	dec	de	;  7
        ld	a,d	;  5
        or	e	;  5
        jr	z,ReceiveTimeOut	;  8

        in	f,(c)	; 14
        jp	pe,HeaderPE	; 11   LOOP= 50 (2-CLOCKS)
        rlc	a	; 10
        in	f,(c)	; 14
        jp	pe,HeaderPE	; 11   At least 2 clocks needed to be down

WU_PE:	in	f,(c)	; 14
        jp	po,WU_PE	; 11   LOOP=25
        pop	de
        push	de

RX_PE:	in	f,(c)	; 14
        jp	pe,RX_PE	; 11   LOOP=25
        ld	(hl),b	;  8 = 33 CYCLES

        in	a,(c)	; 14   Bit 0
        cpl		;  5
        rrca		;  5
        dec	de	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 1
        xor	b	;  5
        rrca		;  5
        inc	hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 2
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 3
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 4
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 5
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 6
        xor	b	;  5
        rrca		;  5
        ld	sp,hl	;  7 = 31 CYCLES

        in	b,(c)	; 14   Bit 7
        xor	b	;  5
        rrca		;  5

        ld	b,a	;  5
        ld	a,d	;  5
        or	e	;  5
        jp	nz,RX_PE	; 11

        jr	ReceiveOK

;________________________________________________________________________________________________________________________________

; Compute xmodem CRC-16
; Input:  DE    = buffer
;         BC    = bytes
;         Stack = CRC-16
; Output: HL    = updated CRC-16

; compute CRC with lookup table
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

crc16:
        ld	a,l
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
GETDPB:		EQU	SUBRET
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
