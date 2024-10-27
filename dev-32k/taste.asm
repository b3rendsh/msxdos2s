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

main:		ld	ix,iobase		; workbuffer for dynamic IO addresses

		ld	de,t_header
		call	PrintText

		; check for interface parameter 
                ld      a,($005d)		; first character of commandline parameters:
		cp	' '
		jp	z,autodetect
		cp	'S'			; S = soda interface
		jr	nz,check1
		call	cfideInit
		ld	a,$01
		jr	z,main_2
		jr	notdetected

check1:		cp	'B'			; B = beer interface
		jr	nz,help
		call	ppideInit
		ld	a,$02
		jr	z,main_2
		jr	notdetected

help:		ld	de,t_help
		call	PrintText
		jr	main_exit		

autodetect:	call	cfideInit		; probe cf interface first
		ld	a,$01
		jr	z,main_2
		call	ppideInit
		ld	a,$02
		jr	z,main_2
notdetected:	ld	de,t_notdetected
		call	PrintText
		jr	main_exit

main_2:		ld	(itype),a
		ld      de,cf_info	 	; point to jump table
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
t_help:		db	"Usage: TASTE [option]",13,10
		db	"Autodetect the interface type",13,10
		db	"if no option is specified.",13,10
		db	"Option:",13,10
		db	"  S = SODA interface",13,10
		db	"  B = BEER interface",13,10,"$"

; ------------------------------------------------------------------------------
; Compact Flash information
; ------------------------------------------------------------------------------
info:		ld	hl,secbuf
		call	cf_info
		ret	c

		ld	de,t_interface
		call	PrintText
		ld	a,(iobase)
		call	PrintHexByte
		ld	a,(itype)
		cp	1
		jr	z,isoda
		ld	de,t_beer
		jr	iprint
isoda:		ld	de,t_soda
iprint:		call	PrintText
		call	PrintCRLF

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

t_interface:	db	"Interface: $"
t_beer:		db	" BEER IDE $"
t_soda:		db	" SODA IDE $"
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
IDE_CMD_DIAG	equ	$90		; diagnostic test
IDE_CMD_INFO	equ	$ec		; disk info
IDE_CMD_FEATURE	equ	$ef		; set feature

IDE_READ	equ	$40		; /rd=0 /wr=1 /cs=0
IDE_WRITE	equ	$80		; /rd=1 /wr=0 /cs=0
IDE_IDLE	equ	$e7		; /rd=1 /wr=1 /cs=1 reg=7

; PPI 8255 I/O registers:
PPI_IOA		equ	$00		; A: IDE data low byte
PPI_IOB		equ	$01		; B: IDE data high byte
PPI_IOC		equ	$02		; C: IDE control
PPI_CTL		equ	$03		; PPI control

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

; ------------------------------------------
; Initialize disk
; Output: Z-flag set if hardware detected
; ------------------------------------------
ppideInit:	; probe for PPI 8255 hardware
		ld	hl,ppidePorts
ppideProbe:	ld	a,(hl)
		cp	$ff
		jr	z,ppideNotDetected
		call	ppideInit1
		ret	z
		inc	hl
		jr	ppideProbe

ppideInit1:	add	a,PPI_CTL
		ld	c,a
		ld	a,PPI_OUTPUT		; PPI A+B is output
		out	(c),a
		dec	c			; PPI_IOC
		ld	a,IDE_IDLE
		out	(c),a
		ld	c,(hl)			; PPI_IOA
		ld	a,$a5			; set test pattern: 1010 0101
		out	(c),a
		in	a,(c)			; read back test pattern
		cp	$a5			
		ret	nz
		ld	a,c
		ld	(ix+0),a		; set PPI IDE IO data address
		ld	(patch1+1),a
		inc	a
		inc	a
		ld	(ix+1),a		; set PPI IDE IO control addres
		ld	(patch2+1),a
		ld	(patch3+1),a
		xor	a			; z=detected
		ld	hl,ppide_tab
		ret

ppideNotDetected:
		or	a			; nz=not detected
		ret

; List of ports that are probed, end with $ff
ppidePorts:	db	$30,$10,$ff

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
; Get IDE device information 
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
; IDE diagnostic 
; ------------------------------------------
ppideDiag:	call	ppideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_DIAG
		call	ppideCommand
		call	ppideWaitReady
		ret	c
		jp	ppideError

; ------------------------------------------
; IDE set sector start address and number of sectors
; Input: C,D,E = 24-bit sector number
; ------------------------------------------
ppideSetSector:	call	ppideWaitReady
		ret	c
		push	hl
		push	bc
		ld	l,c			; set l to bit 16..23
		call	ppideOutput

		ld	b,IDE_WRITE+$02		; IDE register 2
		ld	a,$01			; number of sectors is 1
		ld	c,(ix+0)
		out 	(c),a
		ld	c,(ix+1)
		out 	(c),b
		ld	a,IDE_IDLE
		out 	(c),a

		inc	b			; IDE register 3
		ld	c,(ix+0)
		out	(c),e			; bit 0..7
		ld	c,(ix+1)
		out 	(c),b
		out	(c),a

		inc	b			; IDE register 4
		ld	c,(ix+0)
		out	(c),d			; bit 8..15
		ld	c,(ix+1)
		out 	(c),b
		out	(c),a

		inc	b			; IDE register 5
		ld	c,(ix+0)
		out	(c),l			; bit 16..23
		ld	c,(ix+1)
		out 	(c),b
		out	(c),a

		inc	b			; IDE register 6
		ld	c,(ix+0)
		ld	l,$e0			; LBA mode
		out	(c),l			
		ld	c,(ix+1)
		out 	(c),b
		out	(c),a

		pop	bc
		pop	hl
		ret

; ------------------------------------------
; IDE set command read sector
; ------------------------------------------
ppideCmdRead:	push	bc
		ld 	a,IDE_CMD_READ
		call	ppideCommand
		call	ppideWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Read Sector
; Input: HL = transfer address
; ------------------------------------------

; Copy of BEER 1.9 code
; Ports are patched at runtime
ppideReadSector:
patch1:		ld	c,PPI_IOA
		ld	d,0
ppi_rd_loop:	ld	a,IDE_READ
		ld	b,005h
patch2:		out	(PPI_IOC),a
		ini
		inc	c
		ini
		dec	c
		ld	a,IDE_IDLE
patch3:		out	(PPI_IOC),a
		dec	d
		jr	nz,ppi_rd_loop
		ret

; ------------------------------------------
; throughput optimization: 
; total  93 MSX T-states in 2-byte read loop, 3.580.000Hz/( 93x256x2)=75KB/s
; total 172 MSX T-states in 4-byte read loop, 3.580.000Hz/(172x128x2)=81KB/s
ppideReadOptm:
		ld	a,(ix+0)
		ld	b,$80
		ld	c,(ix+1)
		ld	d,IDE_READ
		ld	e,IDE_IDLE
ppi_opt_loop:	out	(c),d			; 14T
		ld	c,a			; 05T
		ini				; 18T
		inc	c			; 05T
		ini				; 18T
		inc	c			; 05T
		out	(c),e			; 14T
		; repeat 2-byte read before looping to increase throughput:
		; saves 128x14 T-states with 11 extra bytes of code
		out	(c),d
		ld	c,a
		ini
		inc	c
		ini
		inc	c
		out	(c),e
		djnz	ppi_opt_loop		; 14T
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
ppideError:	ld	a,IDE_READ+REG_ERROR
		jr	ppideReadReg

; ------------------------------------------
; PPI IDE read status register
; ------------------------------------------
ppideStatus:	ld	a,IDE_READ+REG_STATUS
ppideReadReg:	ld	c,(ix+1)
		out	(c),a
		ld	c,(ix+0)
		in	a,(c)
		ex	af,af'
		ld	a,IDE_IDLE
		ld	c,(ix+1)
		out	(c),a
		ex	af,af'
		ret

; ------------------------------------------
; PPI IDE set command
; Input:  A = command
; ------------------------------------------
ppideCommand:	call	ppideOutput
		ld	c,(ix+0)
		out	(c),a
		ld	c,(ix+1)
		ld	a,IDE_WRITE+REG_COMMAND
		out	(c),a
		ld	a,IDE_IDLE
		out	(c),a
		ret

; ------------------------------------------
; PPI IDE set data direction
; ------------------------------------------
ppideInput:	ex	af,af'
		ld	c,(ix+1)
		inc	c			; PPI_CTL
		ld	a,PPI_INPUT		; PPI A+B is input
		out	(c),a
		ld	a,IDE_IDLE
		dec	c			; PPIO_IOC
		out	(c),a
		ex	af,af'
		ret

ppideOutput:	ex	af,af'
		ld	c,(ix+1)
		inc	c			; PPI_CTL
		ld	a,PPI_OUTPUT		; PPI A+B is output
		out	(c),a
		ld	a,IDE_IDLE
		dec	c			; PPI_IOC
		out	(c),a
		ex	af,af'
		ret

; ------------------------------------------------------------------------------
; *** Compact Flash 8-BIT IDE routines ***
; ------------------------------------------------------------------------------

; ------------------------------------------
; Initialize disk
; Output: Z-flag set if hardware detected
; ------------------------------------------
cfideInit:	; probe for CF IDE hardware
		ld	hl,cfidePorts
cfideProbe:	ld	a,(hl)
		cp	$ff
		jr	z,cfideNotDetected
		call	cfideInit1
		ret	z
		inc	hl
		jr	cfideProbe

cfideInit1:	add	a,$03
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
		ld	(ix+0),a		; CF IDE IO Data
		add	a,REG_CONTROL
		ld	(ix+1),a		; CF IDE IO Command/Status

		; Set IDE feature to 8-bit
		call	cfideWaitReady
		ret	c			; time-out
		ld	a,(ix+0)
		add	a,REG_FEATURE
		ld	c,a
		ld	a,$01
		out	(c),a
		ld	a,(ix+0)
		add	a,REG_LBA3
		ld	c,a
		ld	a,$e0			; LBA mode / device 0
		out	(c),a
		ld	a,IDE_CMD_FEATURE
		ld	c,(ix+1)
		out	(c),a
		ld	hl,cfide_tab
		xor	a
		ret

cfideNotDetected:
		or	a			; nz=not detected
		ret

; List of ports that are probed, end with $ff
cfidePorts:	db	$30,$38,$10,$ff

cfide_tab:	jp	cfideInfo
		jp	cfideDiag
		jp	cfideSetSector
		jp	cfideCmdRead
		jp	cfideReadSector
		jp	cfideReadOptm
		jp	cfideWaitReady
		jp	cfideWaitData

; ------------------------------------------
; Get IDE device information 
; ------------------------------------------
cfideInfo:	call	cfideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		ld	c,(ix+1)
		out	(c),a
		call	cfideWaitData
		jp 	nz,cfideError
		call	cfideReadSector
		xor	a
		ret

; ------------------------------------------
; IDE diagnostic 
; ------------------------------------------
cfideDiag:	call	cfideWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_DIAG
		ld	c,(ix+1)
		out	(c),a
		call	cfideWaitReady
		ret	c
		jp	cfideError

; ------------------------------------------
; IDE set sector start address and number of sectors
; Input: C,D,E = 24-bit sector number
; ------------------------------------------
cfideSetSector:	call	cfideWaitReady
		ret	c
		push	hl
		push	bc
		ld	a,c
		ex	af,af'
		ld	a,(ix+0)
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
cfideCmdRead:	push	bc
		ld 	a,IDE_CMD_READ
		ld	c,(ix+1)
		out	(c),a
		call	cfideWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Read Sector
; Input: HL = transfer address
; ------------------------------------------

; Baseline
cfideReadSector:	
		ld	b,$00
		ld	c,(ix+0)
		inir
		inir
		ret

; ------------------------------------------
; Optimized
cfideReadOptm:
		ld	b,$20		; counter: 32x16=512 bytes
		ld	c,(ix+0)
cfopt_loop:
		REPT 16			; repeat: read 16 bytes
		ini			; 16x ini in a loop is faster than inir
		ENDR
		djnz	cfopt_loop
		ret

; ------------------------------------------
; Wait for IDE ready or time-out
; ------------------------------------------
cfideWaitReady:	push	hl
		push	bc
		ld	b,$14			; time-out after 20 seconds
cf_wait_1:	ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
cf_wait_2:	ld	c,(ix+1)
		in	a,(c)
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
cfideWaitData:	ld	c,(ix+1)
		in	a,(c)
		bit	7,a			; IDE busy?
		jr	nz,cfideWaitData	; nz=yes
		bit	0,a			; IDE error?
		ret	nz			; nz=yes
		bit	3,a			; IDE data request?
		jr	z,cfideWaitData		; z=no
		xor	a			; no error
		ret

; ------------------------------------------
; IDE error handling
; ------------------------------------------
cfideError:	ld	a,(ix+0)
		add	a,REG_ERROR
		ld	c,a
		in	a,(c)
		ret

; ------------------------------------------------------------------------------
; *** Dynamic variables ***
; ------------------------------------------------------------------------------

shex:		ds	6
sinfo:		ds	30
starttime:	ds	2
america:	ds	1
iobase:		ds	2
itype:		ds	1

		ALIGN	$100
secbuf:		ds	512
