; ------------------------------------------------------------------------------
; taste.asm
;
; Copyright (C) 2024 H.J. Berends
; 
; You can freely use, distribute or modify this program.
; It is provided freely and "as it is" in the hope that it will be useful, 
; but without any warranty of any kind, either expressed or implied.
; ------------------------------------------------------------------------------
; MSX disk info and test for use with BEER or SODA IDE interface.


		INCLUDE	"msx.inc"

		ORG	$0100

		jp	main

; jump table cf/ppi ide subroutines
cf_info:	jp	0
cf_diag:	jp	0
cf_setsector:	jp	0
cf_cmdread:	jp	0
cf_readsector:	jp	0
cf_readoptm:	jp	0
cf_waitready:	jp	0
cf_waitdata:	jp	0
cf_error:	jp	0

main:		ld	de,t_header
		call	PrintText

		call	cfideInit		; probe cf interface first
		jr	z,main_2
		call	ppideInit
		jr	z,main_2
		ld	de,t_notdetected
		call	PrintText
		jr	main_exit

main_2:		ld      de,cf_info	 	; point to jump table
                ld      bc,9*3			; number of entries
                ldir				; copy hardware interface routines table
		call	info
		jr	c,ide_error
		call	diag
		jr	c,ide_error
		call	z,speedtest
main_exit:	rst	$00	

ide_error:	push	af
		ld	de,t_error
		call	PrintText
		pop	af
		call	PrintHexByte
		call	PrintCRLF
		jr	main_exit

t_header:	db	12
		db	"TASTE: BEER/SODA IDE info and test",13,10
		db	"----------------------------------",13,10,13,10,"$"
t_notdetected:	db	"IDE hardware not detected",13,10,"$"
t_error:	db	13,10,"IDE error $"


; ------------------------------------------------------------------------------
; Compact Flash information
; ------------------------------------------------------------------------------
info:		ld	hl,secbuf
		call	cf_info
		ret	c

		ld	de,t_model
		call	PrintText
		ld	hl,sinfo
		ld	de,secbuf+54
		ld	b,10
		call	PrintInfo
		call	PrintCRLF

		ld	de,t_serial
		call	PrintText
		ld	hl,sinfo
		ld	de,secbuf+20
		ld	b,10
		call	PrintInfo
		call	PrintCRLF

		ld	de,t_firmware
		call	PrintText
		ld	hl,sinfo
		ld	de,secbuf+46
		ld	b,4
		call	PrintInfo
		call	PrintCRLF

		ld	de,t_sectors
		call	PrintText
		ld	de,(secbuf+122)
		call	PrintHexWord
		ld	de,(secbuf+120)
		call	PrintHexWord
		ld	de,t_hex
		call	PrintText
		call	PrintCRLF
		ret

t_model:	db  	"Model    : $"
t_serial:	db	"Serial   : $"
t_firmware:	db	"Firmware : $"
t_sectors: 	db  	"Sectors  : $"
t_hex:		db	" (HEX) $"

; ------------------------------------------------------------------------------
; Compact Flash diagnostics
; ------------------------------------------------------------------------------
diag:		ld	de,t_diag
		call	PrintText
		call	cf_diag
		ret	c
		cp	1
		jr	nz,diag_failed
		ld	de,t_passed
		call	PrintText
		call	PrintCRLF
		xor	a
		ret

diag_failed:	push	af
		ld	de,t_failed
		call	PrintText
		pop	af
		call	PrintHexByte
		call	PrintCRLF

		ld	de,t_dump
		call	PrintText
		ld	hl,secbuf
		call	cf_info
		ret	c
		ld	b,0		; dump first 256 info bytes
		ld	hl,secbuf

diag_dump:	push	bc
		ld	a,(hl)
		cp	$20
		jr	c,dump_hex
		cp	$7e
		jr	nc,dump_hex
		call	PrintC
		jr	dump_next
dump_hex:	call	PrintHexByte
		ld	a,' '
		call	PrintC
dump_next:	inc	hl
		pop	bc
		djnz	diag_dump

		xor	a
		inc	a
		ret

t_diag:		db	13,10,"Diagnostic test $"
t_passed:	db	"passed$"
t_failed:	db	"failed, error $"
t_dump:		db	13,10,"Dump:",13,10,"$"

; ------------------------------------------------------------------------------
; Compact Flash speed test
; ------------------------------------------------------------------------------
speedtest:	ld	a,(EXPTBL)	; Slot number main ROM
		ld	hl,IDBYT0	; ID Byte 0
		call	RDSLT
		ei			; RDSLT disables interrupts
		and	$80		; only interested in bit 7
		ld	(america),a	; 0=60Hz 80=50Hz (good enough detection for MSX 1)
		jr	nz,speedtest1
		ld	a,'6'
		ld	(t_speedHz),a
speedtest1:	ld	de,t_speed
		call	PrintText
		call	diskread
		ret	c
		ld	de,t_optimized
		call	PrintText
		ld	hl,(cf_readoptm+1)
		ld	(cf_readsector+1),hl
		call	diskread
		ret

diskread:	ld	hl,(JIFFY)
		ld	(starttime),hl

		ld	hl,$0800	; read 2048 sectors = 1024 KB
		ld	de,$0000	; start sector bit 0..15
		ld	c,$00		; start sector bit 16..23
		ld	b,$01		; sector count

read_loop:	call	cf_setsector
		ret	c
		call	cf_cmdread
		jp	nz,cf_error
		push	hl
		push	de
		push	bc
		ld	hl,secbuf
		call	cf_readsector
		pop	bc
		pop	de
		pop	hl
		inc	de
		dec	hl
		ld	a,h
		or	l
		jr	nz,read_loop
		
		ld	hl,(JIFFY)
		ld	de,(starttime)
		sbc	hl,de

		ex	de,hl
		;push	de
		;call	PrintHexWord
		;call	PrintCRLF
		;pop	de

		; 50Hz: KB/s = 1024x50/de = 51200/de (51200=$c800)
		; 60Hz: KB/s = 1024x60/de = 61440/de (61440=$f000)
		ld	a,(america)
		or	a
		jr	z,speed60
		ld	a,$c8
		jr	speedset
speed60:	ld	a,$f0
speedset:	ld	c,$00
		call	div_ac_de
		ld	h,a
		ld	l,c
		call	hex2dec
		call	PrintCRLF
		xor	a
		ret	

t_speed:	db	13,10,"Disk read speed (KB/s "
t_speedHz:	db	"50Hz)",13,10,13,10
		db	"Baseline : $"
t_optimized:	db	"Optimized: $"

div_ac_de:	ld	hl,0
		ld	b, 16
r11:		sll	c
		rla
		adc	hl,hl
		sbc	hl,de
		jr	nc,$+4
		add	hl,de
		dec	c
   		djnz	r11
		ret

; ------------------------------------------------------------------------------
; *** Print and input subroutines ***
; ------------------------------------------------------------------------------

; Use BDOS to print character
PrintC:		push	bc
		push	de
		push	hl
		ld	c,6
		ld	e,a
		call	5
		pop	hl
		pop	de
		pop	bc
		ret
		
; Use BDOS to print string terminated with $
PrintText:	push	de
		push	hl
		ld	c,9
		call	5
		pop	hl
		pop	de
		ret

; Use BDOS to move cursor to next line
PrintCRLF:	push	af
		ld	a,13
		call	PrintC
		ld	a,10
		call	PrintC
		pop	af
		ret

; print info with swapping of byte pairs
PrintInfo:	inc	de
		ld	a,(de)
		ld	(hl),a
		inc	hl
		dec	de
		ld	a,(de)
		ld	(hl),a
		inc	hl
		inc	de
		inc	de
		djnz	PrintInfo
		ld	(hl),'$'
		ld	de,sinfo
		call	PrintText
		ret

; Print byte in hex
PrintHexByte:	push	hl
		ld	hl,shex
		call	tohex
		jr	PrintHexS

; Print word in hex
PrintHexWord:	push	hl
		ld	hl,shex
		ld	a,d
		call	tohex
		ld	a,e
		call	tohex
PrintHexS:	ld	(hl),'$'
		pop	hl
		ld	de,shex
		jp	PrintText

; Convert byte to printable hex value
tohex:		ld	b,a
		and	$f0
		rrca
		rrca
		rrca	
		rrca
		add	a,'0'
		cp	'9'+1
		jr	c,digit1
		add	a,7
digit1:		ld	(hl),a
		inc	hl
		ld	a,b
		and	$0f
		add	a,'0'
		cp	'9'+1
		jr	c,digit2
		add	a,7
digit2:		ld	(hl),a
		inc	hl
		ret

; Print value in hl to 4-digit decimal
hex2dec:	ld	bc,-1000
		call	num1
		ld	bc,-100
		call	num1
		ld	c,-10
		call	num1
		ld	c,b
num1:		ld	a,'0'-1
num2:		inc	a
		add	hl,bc
		jr	c,num2
		sbc	hl,bc
		jp	PrintC

; ------------------------------------------------------------------------------
; *** IDE hardware interface ***
; ------------------------------------------------------------------------------
IDE_CMD_READ	equ	$20		; read sector
IDE_CMD_WRITE	equ	$30		; write sector
IDE_CMD_INFO	equ	$ec		; disk info
IDE_CMD_FEATURE	equ	$ef		; set feature
IDE_CMD_DIAG	equ	$90		; diagnostic test

IDE_READ	equ	$40
IDE_WRITE	equ	$80
IDE_IDLE	equ	$c0

; PPI 8255 I/O registers:
PPI_IOA		equ	$30		; A: IDE data low byte
PPI_IOB		equ	$31		; B: IDE data high byte
PPI_IOC		equ	$32		; C: IDE control
PPI_CTL		equ	$33		; PPI control

; CF IDE I/O ports:
CFIO_BASE	equ	$30
CFIO_DATA	equ	CFIO_BASE+$00
CFIO_ERROR	equ	CFIO_BASE+$01
CFIO_FEATURE	equ	CFIO_BASE+$01
CFIO_STATUS	equ	CFIO_BASE+$07
CFIO_COMMAND	equ	CFIO_BASE+$07

; ------------------------------------------------------------------------------
; *** PPI 8255 IDE routines ***
; ------------------------------------------------------------------------------

ppideInit:	; probe for PPI 8255 hardware
		call	ppideOutput
		ld	hl,$00a5	; register=0 value=a5
		call	ppideSetReg
		in	a,(PPI_IOA)
		cp	$a5
		ld	hl,ppide_tab
		ret	

ppide_tab:	jp	ppideInfo
		jp	ppideDiag
		jp	ppideSetSector
		jp	ppideCmdRead
		jp	ppideReadSector
		jp	ppideReadOptm
		jp	ppideWaitReady
		jp	ppideWaitData
		jp	ppideError

; ------------------------------------------
ppideInfo:	call	ppideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		call	ppideCommand
		call	ppideWaitData
		jp 	nz,ppideError
		call	ppideReadSector
		xor	a
		ret

; ------------------------------------------
ppideDiag:	call	ppideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_DIAG
		call	ppideCommand
		call	ppideWaitReady
		ret	c
		jp	ppideError

; ------------------------------------------
ppideSetSector:	call	ppideWaitReady
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
ppideCmdRead:	ld 	a,IDE_CMD_READ
		call	ppideCommand
		jp	ppideWaitData

; ------------------------------------------
; Copy of BEER 1.9 code
ppideReadSector:
		ld	c,PPI_IOA
		ld	d,0
ppi_rd_loop:	ld	a,IDE_READ
		ld	b,005h
		out	(PPI_IOC),a
		ini
		inc	c
		ini
		dec	c
		ld	a,IDE_IDLE
		out	(PPI_IOC),a
		dec	d
		jr	nz,ppi_rd_loop
		ret

; ------------------------------------------
ppideReadOptm:
		ld	a,PPI_IOA
		ld	b,$80
		ld	c,PPI_IOC
		ld	d,IDE_READ
		ld	e,IDE_IDLE
		out	(c),e
ppi_opt_loop:	out	(c),d
		ld	c,a
		ini
		inc	c
		ini
		inc	c
		out	(c),e
		; repeat 2-byte read
		out	(c),d
		ld	c,a
		ini
		inc	c
		ini
		inc	c
		out	(c),e
		djnz	ppi_opt_loop
		ret

; ------------------------------------------
; Wait for IDE ready or time-out
; ------------------------------------------
ppideWaitReady:	push	hl
		push	bc
		call	ppideInput
		ld	b,$14			; time-out after 20 seconds
ppi_wait_1:	ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
ppi_wait_2:	call	ppideStatus
		and	%11000000
		cp	%01000000		; BUSY=0 RDY=1 ?
		jr	z,ppi_wait_end		; z=yes
		dec	hl
		ld	a,h
		or	l
		jr	nz,ppi_wait_2
		djnz	ppi_wait_1
		scf				; time-out
ppi_wait_end:	pop	bc
		pop	hl
		ret

; ------------------------------------------
; Wait for IDE data read/write request
; ------------------------------------------
ppideWaitData:	call 	ppideInput
ppi_waitdata_1:	call	ppideStatus
		bit	7,a			; IDE busy?
		jr	nz,ppi_waitdata_1	; nz=yes
		bit	0,a			; IDE error?
		ret	nz			; nz=yes
		bit	3,a			; IDE data request?
		jr	z,ppi_waitdata_1	; z=no
		xor	a			; no error
		ret

; ------------------------------------------
; IDE error handling
; ------------------------------------------
ppideError:	ld	a,$c1
		out	(PPI_IOC),a
		ld 	a,$41
		out 	(PPI_IOC),a
		in 	a,(PPI_IOA)
		ex	af,af'
		ld	a,$c1
		out	(PPI_IOC),a
		ex	af,af'
		ret

; ------------------------------------------
; PPI IDE set command
; Input:  A = command
; ------------------------------------------
ppideCommand:	call	ppideOutput
		push	hl
		ld	h,$07
		ld	l,a
		call	ppideSetReg
		pop	hl
		ret

; ------------------------------------------
; PPI IDE set register
; Input:  H = register
;         L = value
; ------------------------------------------
ppideSetReg:	ld	a,$c0
		add	a,h
		out	(PPI_IOC),a
		ld	a,l
		out 	(PPI_IOA),a
		ld 	a,$80
		add	a,h
		out 	(PPI_IOC),a
		ld	a,$c0
		add	a,h
		out 	(PPI_IOC),a
		ret

; ------------------------------------------
; PPI IDE read status register
; ------------------------------------------
ppideStatus:	ld	a,$c7
		out	(PPI_IOC),a
		ld 	a,$47
		out 	(PPI_IOC),a
		in 	a,(PPI_IOA)
		ex	af,af'
		ld 	a,$c7
		out	(PPI_IOC),a
		ex 	af,af'
		ret

; ------------------------------------------
; PPI IDE set data direction
; ------------------------------------------
ppideOutput:	ex	af,af'
		ld	a,$80			; PPI A+B is output
		out	(PPI_CTL),a
		ex	af,af'
		ret

ppideInput:	ex	af,af'
		ld	a,$92			; PPI A+B is input
		out	(PPI_CTL),a
		ex	af,af'
		ret

; ------------------------------------------------------------------------------
; *** CF IDE routines ***
; ------------------------------------------------------------------------------

cfideInit:	; probe for CF IDE hardware
		ld	a,$aa
		out	(CFIO_BASE+3),a
		ld	a,$55
		out	(CFIO_BASE+4),a
		in	a,(CFIO_BASE+3)
		cp	$aa
		ret	nz
		in	a,(CFIO_BASE+4)
		cp	$55
		ret	nz

		; Set IDE feature to 8-bit
		call	cfideWaitReady
		ret	c			; time-out
		ld	a,$01			; enable 8-bit
		out	(CFIO_FEATURE),a
		ld	a,$e0			; LBA mode / device 0
		out	(CFIO_BASE+6),a
		ld	a,IDE_CMD_FEATURE
		out	(CFIO_COMMAND),a
		ld	hl,cfide_tab
		xor	a
		ret

cfide_tab:	jp	cfideInfo
		jp	cfideDiag
		jp	cfideSetSector
		jp	cfideCmdRead
		jp	cfideReadSector
		jp	cfideReadOptm
		jp	cfideWaitReady
		jp	cfideWaitData

; ------------------------------------------
cfideInfo:	call	cfideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		out	(CFIO_COMMAND),a
		call	cfideWaitData
		jp 	nz,cfideError
		call	cfideReadSector
		xor	a
		ret

; ------------------------------------------
cfideDiag:	call	cfideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_DIAG
		out	(CFIO_COMMAND),a
		call	cfideWaitReady
		ret	c
		jp	cfideError

; ------------------------------------------
cfideSetSector:	call	cfideWaitReady
		ret	c
		ld	a,$01			; number of sectors is 1
		out	(CFIO_BASE+2),a
		ld	a,e			; bit 0..7
		out	(CFIO_BASE+3),a
		ld	a,d			; bit 8..15
		out	(CFIO_BASE+4),a
		ld	a,c			; bit 16..23
		out	(CFIO_BASE+5),a
		ld	a,$e0			; LBA mode
		out	(CFIO_BASE+6),a
		xor	a
		ret

; ------------------------------------------
cfideCmdRead:	ld 	a,IDE_CMD_READ
		out	(CFIO_COMMAND),a
		jp	cfideWaitData

; ------------------------------------------
cfideReadSector:	
		ld	b,$00
		ld	c,CFIO_DATA
		inir
		inir
		ret

; ------------------------------------------
cfideReadOptm:
		ld	b,$20		; counter: 32x16=512 bytes
		ld	c,CFIO_DATA
cfopt_loop:
		REPT 16			; repeat: read 16 bytes
		ini			; 16x ini in a loop is faster than inir
		ENDR
		djnz	cfopt_loop
		ret

; ------------------------------------------
cfideWaitReady:	push	hl
		push	bc
		ld	b,$14			; time-out after 20 seconds
cf_wait_1:	ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
cf_wait_2:	in	a,(CFIO_STATUS)
		and	%11000000
		cp	%01000000		; BUSY=0 RDY=1 ?
		jr	z,cf_wait_end		; z=yes
		dec	hl
		ld	a,h
		or	l
		jr	nz,cf_wait_2
		djnz	cf_wait_1
		scf				; time-out
cf_wait_end:	pop	bc
		pop	hl
		ret

; ------------------------------------------
; Wait for IDE data read/write request
; ------------------------------------------
cfideWaitData:	in	a,(CFIO_STATUS)
		bit	7,a			; IDE busy?
		jr	nz,cfideWaitData	; nz=yes
		bit	0,a			; IDE error?
		ret	nz			; nz=yes
		bit	3,a			; IDE data request?
		jr	z,cfideWaitData		; z=no
		xor	a			; no error
		ret

; ------------------------------------------
; CF IDE error handling
; ------------------------------------------
cfideError:	in	a,(CFIO_ERROR)
		ret

; ------------------------------------------------------------------------------
; *** Dynamic variables ***
; ------------------------------------------------------------------------------

		ALIGN	$100

secbuf:		ds	512
shex:		ds	6
sinfo:		ds	30
starttime:	ds	2
america:	ds	1
