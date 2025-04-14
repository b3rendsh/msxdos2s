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

        SECTION	DRV_JIO

        ; Mandatory symbols defined by the disk hardware interface driver
        PUBLIC	DRVINIT	; Initialize hardware interface driver
        PUBLIC	INIENV	; Initialize driver environment
        PUBLIC	DSKIO	; Disk I/O routine
        PUBLIC	DSKCHG	; Disk change routine
        PUBLIC	READSEC	; Read sector (MBR / bootsector) routine
        PUBLIC	DRVMEM	; Memory for hardware interface variables

        EXTERN	GETWRK	; Get address of disk driver's work area
        EXTERN	GETSLT	; Get slot of this interface
        EXTERN	DRVSIZE	; DOS driver workarea size (offset)
        EXTERN	W_CURDRV	; Workarea variable defined by the DOS driver
        EXTERN	W_BOOTDRV	; "
        EXTERN	W_DRIVES	; "

        ; Driver routines, use in DRVINIT/INIENV only
        EXTERN	PART_BUF

        EXTERN	PrintMsg
        EXTERN	PrintCRLF
        EXTERN	PrintString

; Hardware driver variables
W_FLAGS		equ	DRVSIZE
W_COMMAND	equ	DRVSIZE+1
DRVMEM		equ	2

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; #include "flags.inc"
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#define DRIVE_ACKNOWLEDGE_WRITE_OK            0x1111
#define DRIVE_ACKNOWLEDGE_WRITE_FAILED        0x2222
#define DRIVE_ACKNOWLEDGE_WRITE_PROTECTED     0x3333 
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CHGCPU	equ	$180
GETCPU	equ	$183

;********************************************************************************************************************************
; Initialize hardware interface driver
;********************************************************************************************************************************

DRVINIT:
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


DRVINIT_Retry:
        ld	a,7
        call	SNSMAT
        and	4
        ret     z

        ld	a,'.'
        rst	$18

        ld	e,0xFF
        ld	d,e
        ld	c,e

        ld	b,1
        ld	hl,PART_BUF

        ld	(ix+W_FLAGS),FLAG_RX_CRC|FLAG_TX_CRC|FLAG_TIMEOUT
        ld      (ix+W_COMMAND),COMMAND_DRIVE_INFO
        call	ReadOrWriteSectors
        jr	c,DRVINIT_Retry

        ld	hl,PART_BUF
        ld	a,(hl)
        ld	(ix+W_FLAGS),a
        inc	hl
        call	PrintString

        xor	a
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
        ;call	PrintMsg
        ;db	"Drives: ",0
        ;ld	a,(ix+W_DRIVES)
        ;add	a,'0'
        ;rst	$18
        jp	PrintCRLF

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

DSKIO:	push	hl
	push	de
	push	bc

	push	af
	cp	$08	; Max 8 drives (partitions) supported
	jp	nc,r404
	call	GETWRK	; Base address of workarea in hl and ix
	pop	af

        ld      (ix+W_COMMAND),COMMAND_DRIVE_WRITE
	jr	c,WriteFlag
        ld      (ix+W_COMMAND),COMMAND_DRIVE_READ
WriteFlag:

        ld	e,a
        add	a,a
        add	a,a
        add	a,e
        ld	e,a	; a * 5
        ld	d,$00
        add	hl,de

        push	hl
        pop	iy

        pop	bc
        pop	de
        pop	hl

        xor	a
        or	(iy+$04)	; Test if partition exists (must have nonzero partition type)
        jr	z,r405

        ; Translate logical to physical sector number
        push	bc	; Save sector counter
        push 	hl	; Save transfer address
        ex	de,hl
        bit	7,c	; If bit 7 of media descriptor is 0 then use 23-bit sector number
        jr	nz,r401	; nz if 16-bit sector number
        ld	e,c	; Bit 16-22 of sector number
        ld	d,$00
        jr	r402

r401:	ld	de,$0000

r402:	ld	c,(iy+$00)
        ld	b,(iy+$01)
        add	hl,bc
        ex	de,hl	; LBA address: de=00..15
        ld	c,(iy+$02)
        ld	b,(iy+$03)
        adc	hl,bc
	ld	c,l	; LBA address: c=16..23
	pop	hl	; Restore transfer address
	pop	af	; Restore sector counter
	ld	b,a

IFDEF IDEDOS1
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
	call	ReadOrWriteSectors
	pop	de
	jr	nz,sec_err
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
	call	ReadOrWriteSectors
	pop	hl
	jr	nz,sec_err
	inc	h
sec_next:
	xor	a
sec_err:
	pop	de
	pop	bc
	jr	nz,r405
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
        jp	ReadOrWriteSectors

	; Disk i/o error
r404:	pop	af
	pop	bc
	pop	de
	pop	hl

r405:	ld	a,$04	; Error 4 = Data (CRC) error (abort,retry,ignore message)
	scf
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
        push	af
        call	GETWRK
        pop	af
        cp	(ix+W_CURDRV)	; current drive
        ld	(ix+W_CURDRV),a
        jr	nz,DiskChanged
        ld	b,$01	; unchanged
        xor	a
        ret

DiskChanged:	ld	b,$FF	; changed
        xor	a
        ret
ELSE
        ; Always return unchanged for DOS2 (disks are not hot-pluggable)
        ld	b,$01
        xor	a
        ret
ENDIF

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; INCLUDE	"drv_jio_c.asm"
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uiTransmit:
	CALL	_ENT_PARM_DIRECT_L09
	BIT	1,(IX+8)
	JR	Z,_0001
_0000:
	LD	E,2
	CALL	vSetCPU
	LD	L,(IX+10)
	LD	H,(IX+11)
	PUSH	HL
	LD	E,(IX+2)
	LD	D,(IX+3)
	CALL	uiXModemCRC16
	POP	AF
	LD	(IX+10),L
	LD	(IX+11),H
_0001:
	LD	E,0
	CALL	vSetCPU
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
	LD	A,2
	XOR	C
	OR	B
	JR	Z,_0007
_0006:
	LD	E,0
	CALL	vSetCPU
_0007:
_0009:
	LD	C,(IX+4)
	LD	B,(IX+5)
	LD	E,(IX+2)
	LD	D,(IX+3)
	CALL	bJIOReceive
	OR	A
	JR	NZ,_0008
_0010:
	BIT	2,(IX+8)
	JR	Z,_0007
_0011:
	LD	A,3
	JR	_0013
_0012:
_0008:
	XOR	A
_0013:
	JP	_LEAVE_DIRECT_L09
ucReadOrWriteSectors:
	CALL	_ENT_AUTO_DIRECT_L09
	DEFW	65510
	PUSH	IY
	LD	HL,W_FLAGS
	LD	C,(IX+12)
	LD	B,(IX+13)
	ADD	HL,BC
	LD	D,(HL)
	LD	(IX-5),D
	LD	HL,W_COMMAND
	ADD	HL,BC
	LD	B,(HL)
	LD	IYH,B
	CALL	eGetCPU
	LD	(IX-7),A
	LD	(IX-18),74
	LD	(IX-17),73
	LD	(IX-16),79
	LD	(IX-15),D
	LD	B,(IX+2)
	LD	(IX-13),B
	LD	H,(IX+3)
	LD	(IX-12),H
	LD	L,(IX+4)
	LD	BC,0
	LD	(IX-11),L
	LD	B,(IX+8)
	LD	(IX-10),B
	LD	L,(IX+10)
	LD	H,(IX+11)
	LD	(IX-9),L
	LD	(IX-8),H
	LD	H,B
	LD	L,C
	ADD	HL,HL
	LD	(IX-26),L
	LD	(IX-25),H
_0016:
	LD	B,IYH
	LD	(IX-14),B
	LD	A,IYH
	CP	18
	JR	NZ,_0042
	LD	A,1
	JR	_0043
_0042:
	XOR	A
_0043:
	LD	C,A
	PUSH	BC
	LD	HL,0
	PUSH	HL
	LD	L,(IX-5)
	PUSH	HL
	LD	BC,5
	LD	L,16
	ADD	HL,SP
	EX	DE,HL
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
	LD	A,IYH
	CP	17
	JR	NZ,_0018
_0017:
	LD	C,0
	PUSH	BC
	PUSH	HL
	LD	L,(IX-5)
	PUSH	HL
	LD	BC,6
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
	LD	L,(IX-5)
	PUSH	HL
	LD	C,(IX-26)
	LD	B,(IX-25)
	LD	E,(IX+10)
	LD	D,(IX+11)
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
	LD	L,(IX-5)
	PUSH	HL
	LD	BC,2
	LD	HL,27
	ADD	HL,SP
	EX	DE,HL
	CALL	ucReceive
	POP	HL
	LD	IYL,A
	OR	A
	JR	NZ,_0022
_0019:
	LD	HL,4369
	LD	C,(IX-3)
	LD	B,(IX-2)
	AND	A
	SBC	HL,BC
	JR	Z,_0022
_0021:
	LD	HL,13107
	AND	A
	SBC	HL,BC
	JR	NZ,_0024
	LD	A,1
	JR	_0025
_0024:
	LD	A,11
_0025:
	LD	IYL,A
_0022:
_0020:
	JR	_0036
_0018:
	CP	16
	JR	NZ,_0028
_0027:
	LD	C,1
	PUSH	BC
	PUSH	HL
	LD	L,(IX-5)
	PUSH	HL
	LD	BC,6
	LD	HL,21
	ADD	HL,SP
	EX	DE,HL
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
_0028:
	LD	L,(IX-5)
	PUSH	HL
	LD	C,(IX-26)
	LD	B,(IX-25)
	LD	E,(IX+10)
	LD	D,(IX+11)
	CALL	ucReceive
	POP	HL
	LD	IYL,A
	OR	A
	JR	NZ,_0036
	BIT	0,(IX-5)
	JR	Z,_0036
_0032:
_0031:
_0029:
	LD	L,(IX-5)
	PUSH	HL
	LD	BC,2
	LD	HL,6
	ADD	HL,SP
	EX	DE,HL
	CALL	ucReceive
	POP	HL
	LD	IYL,A
	OR	A
	JR	NZ,_0036
_0033:
	LD	E,2
	CALL	vSetCPU
	LD	HL,0
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
	JR	Z,_0036
_0035:
	LD	IYL,5
_0036:
_0034:
_0030:
_0026:
	LD	B,IYL
	INC	B
	DEC	B
	JR	Z,_0038
_0037:
	LD	(IX-14),B
	LD	C,1
	PUSH	BC
	LD	HL,0
	PUSH	HL
	LD	L,(IX-5)
	PUSH	HL
	LD	BC,5
	LD	L,16
	ADD	HL,SP
	EX	DE,HL
	CALL	uiTransmit
	POP	AF
	POP	AF
	POP	AF
_0038:
	LD	B,IYL
	INC	B
	DEC	B
	JR	Z,_0014
	BIT	3,(IX-5)
	JP	NZ,_0016
_0014:
	LD	E,(IX-7)
	CALL	vSetCPU
	LD	A,IYL
	POP	IY
	JP	_LEAVE_DIRECT_L09 

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; INCLUDE	"crt.asm"
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;********************************************************************************************************************************
; Read (boot) sector
; Input: C,DE = sector number
;********************************************************************************************************************************
READSEC:
        ld      (ix+W_COMMAND),COMMAND_DRIVE_READ
        ld	b,1

; CDE : Sector
; B   : Length
; HL  : Address

ReadOrWriteSectors:
        push    ix              ; _pucFlagsAndCommand
        push    hl              ; _pvAddress

        ld      a,c
        ld      c,b             ; _ucLength
        push    bc

        ld      b,0             ; _ulSector in BCDE
        ld      c,a

        call    ucReadOrWriteSectors
        pop hl
        pop hl
        pop hl

        or      a
        ret     z
        dec	a
	scf
        ret

;********************************************************************************************************************************
;********************************************************************************************************************************

eGetCPU:
        ld	a,(GETCPU)
        cp	$C3
        ret     nz
        jp      GETCPU

;********************************************************************************************************************************
;********************************************************************************************************************************

vSetCPU:
        ld	a,(CHGCPU)
        cp	$C3
        ret     nz
        ld      a,e
        jp      CHGCPU

;********************************************************************************************************************************
; IN:  HL = DATA
;      BC = LENGTH
;********************************************************************************************************************************

vJIOTransmit:
        ex      de,hl
        inc	bc
        exx
        ld	a,15
	di
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
	di
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

IFDEF IDEDOS1

uiXModemCRC16:
	ei
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

crc16:
	push bc
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
	djnz    crc16

	dec     c
	jp      nz,crc16
	ret

ELSE

; compute CRC with lookup table
uiXModemCRC16:
	ei
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

ENDIF  
