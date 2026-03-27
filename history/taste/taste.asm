; ------------------------------------------------------------------------------
; taste.asm
;
; Copyright (C) 2026 H.J. Berends
; 
; You can freely use, distribute or modify this program.
; It is provided freely and "as it is" in the hope that it will be useful, 
; but without any warranty of any kind, either expressed or implied.
; ------------------------------------------------------------------------------
; MSX disk info and test for use with BEER, SODA or MALT IDE interface.

;DEFINE BASBIN		; build binary option (specified in the make file)
;DEFINE DEBUG		; print debug information
DEFINE BEER_CS		; IDE CS signal always asserted
;DEFINE BEER_TEST	; test alternative driver settings
;DEFINE BEER_TEST_1	; set PIO mode 0 (note that the info record reports max pio mode)
;DEFINE	BEER_TEST_2	; quickly recover from 8255 mode change glitch
;DEFINE BEER_TEST_3	; add read sector delays

		INCLUDE	"msx.inc"

	IFDEF BASBIN
		ORG	BOT32K-7		; 0x8000 - 7 bytes for BIN header
		db	$fe			; ID
		dw	BOT32K			; Start address
		dw	TasteEnd		; End address
		dw	main			; Execution address
	ELSE
		ORG	TBASE			; 0x0100: Start of TPA
		jp	main
	ENDIF

; jump table ide subroutines
cf_info:	jp	0
cf_diag:	jp	0
cf_setsector:	jp	0
cf_cmdread:	jp	0
cf_readsector:	jp	0
cf_readoptm:	jp	0
cf_waitready:	jp	0
cf_waitdata:	jp	0
cf_error:	jp	0
cf_readx:	jp	0
cf_cmdwrite:	jp	0
cf_writesec:	jp	0

main:		ld	ix,iobase		; workbuffer for dynamic IO addresses

		ld	de,t_header
		call	PrintText

	IFNDEF BEER_TEST
	  IFNDEF BASBIN
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
		jr	nz,check2
		call	beerInit
		ld	a,$02
		jr	z,main_2
		jr	notdetected

check2:		cp	'M'			; M = malt interface
		jr	nz,help
		call	maltInit
		ld	a,$03
		jr	z,main_2
		jr	notdetected

help:		ld	de,t_help
		call	PrintText
		jr	main_exit
	  ENDIF ; BASBIN

autodetect:	call	cfideInit		; probe cf interface first
		ld	a,$01
		jr	z,main_2

		call	maltInit
		ld	a,$03
		jr	z,main_2
	ENDIF ; BEER_TEST
		call	beerInit
		ld	a,$02
		jr	z,main_2
notdetected:	ld	de,t_notdetected
		call	PrintText
		jr	main_exit

main_2:		ld	(itype),a
		ld      de,cf_info	 	; point to jump table
                ld      bc,12*3			; number of entries
                ldir				; copy hardware interface routines table
		call	info
		jr	c,ide_error
		call	diag
		jr	c,ide_error
		jr	nz,main_exit
	IFDEF DEBUG
		call	read_mbr
	ELSE
		call	speedtest
		jr	c,main_exit
	  IFNDEF BASBIN
                ld      a,($005e)		; 2nd character of commandline parameters
		cp	'W'
		call	z,writetest
	  ENDIF
	ENDIF
main_exit:
	IFDEF BASBIN
		ret
	ELSE
		rst	$00	
	ENDIF

ide_error:	push	af
		ld	de,t_error
		call	PrintText
		pop	af
		call	PrintHexByte
		call	PrintCRLF
		jr	main_exit

t_header:	db	12
	IFDEF BEER_TEST
		db	"TASTE: BEER IDE alternative timings",13,10
	ELSE
		db	"TASTE: BEER/MALT/SODA info and test",13,10
	ENDIF
		db	"-----------------------------------",13,10,10,"$"
t_notdetected:	db	"IDE hardware not detected",13,10,"$"
t_error:	db	13,10,"IDE error $"
t_help:		db	"Usage: TASTE [option][X|W]",13,10
		db	"Autodetect the interface type",13,10
		db	"if no option is specified.",13,10
		db	"Option:",13,10
		db	"  B = BEER interface",13,10
		db	"  M = MALT interface",13,10
		db	"  S = SODA interface",13,10
		db	"  X = include block read test",13,10
		db	"  W = include write test",13,10,"$"

; ------------------------------------------------------------------------------
; IDE disk information
; ------------------------------------------------------------------------------
info:		; detected interface
		ld	de,t_interface
		call	PrintText
		ld	a,(iobase)
		call	PrintHexByte
		ld	a,(itype)
		cp	1
		jr	z,isoda
		cp	2
		jr	z,ibeer
		ld	de,t_malt
		jr	iprint
ibeer:		ld	de,t_beer
		jr	iprint
isoda:		ld	de,t_soda
iprint:		call	PrintText
		call	PrintCRLF

		; disk controller info
		ld	hl,secbuf
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
		ld	(lastsec+2),de
		call	PrintHexWord
		ld	de,(secbuf+120)
		ld	(lastsec),de
		call	PrintHexWord
		ld	de,t_hex
		call	PrintText
		call	PrintCRLF

		ld	de,t_piomode
		call	PrintText
		ld	de,(secbuf+102)
		call	PrintHexWord
		call	PrintCRLF

		ret

t_interface:	db	"Interface: $"
t_beer:		db	" BEER IDE $"
t_malt:		db	" MALT IDE $"
t_soda:		db	" SODA IDE $"
t_model:	db  	"Model    : $"
t_serial:	db	"Serial   : $"
t_firmware:	db	"Firmware : $"
t_sectors: 	db  	"Sectors  : $"
t_piomode:	db	"PIO mode : $"
t_hex:		db	" (HEX) $"

; ------------------------------------------------------------------------------
; IDE disk diagnostics
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
	IFDEF DEBUG
		call	diag_info
		ld	de,t_inkey
		call	PrintText
		call	GetC
	ENDIF
		xor	a
		ret

diag_failed:	push	af
		ld	de,t_failed
		call	PrintText
		pop	af
		call	PrintHexByte
		call	PrintCRLF

diag_info:	ld	de,t_dump
		call	PrintText
		ld	hl,secbuf
		;call	cf_info
		;ret	c
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
t_inkey:	db	13,10,"press enter to continue",13,10,"$"

; ------------------------------------------------------------------------------
; IDE disk (master) boot records
; ------------------------------------------------------------------------------
read_mbr:	ld	de,$0000	; start sector bit 0..15
		ld	c,$00		; start sector bit 16..23
		call	read_sector

		ld	de,t_inkey
		call	PrintText
		call	GetC

		ld	a,'1'
		ld	(t_boot+9),a
		ld	de,$0001	; start sector bit 0..15
		ld	c,$00		; start sector bit 16..23
		
read_sector:	ld	hl,secbuf
		call	cf_setsector
		ret	c
		call	cf_cmdread
		jp	nz,cf_error
		ld	hl,secbuf
		call	cf_readsector

		; print bootsector data
		ld	de,t_boot
		call	PrintText
		call	sec_dump
		xor	a
		ret

sec_dump:	ld	hl,secbuf
		ld	bc,$0200	; 512 bytes
@loop:		push	bc
		ld	a,(hl)
		call	PrintHexByte
		inc	hl
		pop	bc
		dec	bc
		ld	a,b
		or	c
		jr	nz,@loop
		ret

t_boot:		db	13,10,"SECTOR 0:",13,10,"$"

; ------------------------------------------------------------------------------
; IDE disk read speed test
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

		ld	a,(itype)
		cp	3			; MALT interface
		jr	z,end_optm_test		; z=yes, skip optimized speedtest
		ld	de,t_optimized
		call	PrintText
		ld	hl,(cf_readoptm+1)
		ld	(cf_readsector+1),hl
		call	diskread
		ret	c
end_optm_test:

	IF !(BEER_TEST || BASBIN)
                ld      a,($005e)		; 2nd character of commandline parameters
		cp	'X'
		jr	z,blocktest
		xor	a
		ret
	ENDIF

blocktest:	ld	de,t_block16k
		call	PrintText
		ld	hl,diskreadsec+1
		ld	(hl),$40		; read 64 blocks of 16K = 1024k
		inc	hl
		ld	(hl),$00
		ld	hl,(cf_readx+1)
		ld	(cf_readsector+1),hl
		ld	a,$20			; read 32 sectors
		ld	(beerNsector+1),a
		ld	(maltNsector+1),a
		ld	(cfideNsector+1),a
		ld	(beerReadX+1),a
		ld	(maltReadX+1),a
		ld	(cfideReadX+1),a
		call	diskread
		ret	c

		ld	de,t_block5s
		call	PrintText
		ld	hl,diskreadsec+1
		ld	(hl),$9A		; read 410 blocks of 2.5K = 1025k 
		inc	hl
		ld	(hl),$01
		ld	a,$05			; read 5 sectors
		ld	(beerNsector+1),a
		ld	(maltNsector+1),a
		ld	(cfideNsector+1),a
		ld	(beerReadX+1),a
		ld	(maltReadX+1),a
		ld	(cfideReadX+1),a

diskread:	ld	hl,(JIFFY)
		ld	(starttime),hl

diskreadsec:	ld	hl,$0800	; read 2048 sectors = 1024 KB
		ld	de,$0000	; start sector bit 0..15
		ld	c,$00		; start sector bit 16..23

read_loop:	call	cf_setsector
		ret	c
	IFDEF DEBUG_1
		ld	a,'.'
		call	PrintC
	ENDIF
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
t_speedHz:	db	"50Hz)",13,10,10
		db	"Baseline : $"
t_optimized:	db	"Optimized: $"
t_block16k:	db	"block 16k: $"
t_block5s:	db	"5 sectors: $"

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
; IDE disk write test
; Destroys last 64KB of a disk (up to 4GB)!
; ------------------------------------------------------------------------------
writetest:	ld	de,t_write
		call	PrintText
		ld	bc,(lastsec+2)		; #sec bit 16..31
		ld	a,b
		or	c			; disk size > 32MB?
		jr	z,sec_err
		ld	a,b
		or	a			; disk size > 4GB?
		jr	z,writetest1
		ld	a,$ff
		ld	(lastsec+0),a		; set #sec bit 0..7
		ld	(lastsec+1),a		; set #sec bit 8..15
		ld	(lastsec+2),a		; set #sec bit 16..23
		ld	a,$00
		ld	(lastsec+3),a		; set #sec bit 24..31 (not used)

writetest1:	; create test pattern 1: 0xFF 0xFF 0x00
		ld	de,$aa55
		ld	a,$f0
		call	fill_patbuf
		ld	de,t_pass1
		call	testpat
		ret	c

		; create test pattern 2: 0xFF 0xFF 0x00
		ld	de,$fff0
		ld	a,$00
		call	fill_patbuf
		ld	de,t_pass2
		call	testpat
		ret	c

		xor	a
		ret

sec_err:	ld	de,t_secerr
		call	PrintText
		scf
		ret

; fill sector buffer with test pattern
fill_patbuf:	ld	b,171
		ld	hl,patbuf
@loop:		ld	(hl),d
		inc	hl
		ld	(hl),e
		inc	hl
		ld	(hl),a
		inc	hl
		djnz	@loop
		ret

; test sector write with byte pattern
testpat:	call	PrintText		; print pass

		; write, read and verify sectors
		ld	de,(lastsec)
		ld	a,(lastsec+2)
		ld	c,a
		call	dec_sector		; start at last sector
		ld	b,$80			; test 128 sectors

@wrloop:	call	write_sec
		jr	c,write_fail
		call	read_sec
		jr	c,read_fail

		; verify sector
		push	bc
		push	de
		ld	hl,secbuf		; read back data
		ld	de,patbuf		; written data
		ld	bc,$0200		; verify 512 bytes
@vloop:		ld	a,(de)
		cp	(hl)
		jr	nz,verify_fail
		inc	hl
		inc	de
		dec	bc
		ld	a,b
		or	c
		jr	nz,@vloop
		pop	de
		pop	bc

		; next sector
		call	dec_sector
		djnz	@wrloop

		ld	de,t_ok			; test ok
		call	PrintText
		xor	a
		ret

verify_fail:	pop	de
		pop	bc
		ld	de,t_nok
		call	PrintText
		ld	a,$80+1
		sub	b
		call	PrintHexByte
		call	PrintCRLF
	IFDEF DEBUG_1
		ld	de,t_sector
		call	PrintText
		call	sec_dump
	ENDIF	
		scf
		ret

write_fail:	ld	de,t_write_err
		call	PrintText
		scf
		ret

read_fail:	ld	de,t_read_err
		call	PrintText
		scf
		ret

; -------------------------------------
; test pattern subroutines
; -------------------------------------

; read sector (return on error)
read_sec:	push	bc
		push	de
		ld	hl,secbuf
		ld	b,$0
@loop:		ld	(hl),$0
		inc	hl
		ld	(hl),$0
		djnz	@loop
		call	cf_setsector
		jr	c,disk_err
		call	cf_cmdread
		jr	nz,disk_err
		ld	hl,secbuf
		call	cf_readsector
		pop	de
		pop	bc
		xor	a
		ret

; write sector (return on error)
write_sec:	push	bc
		push	de
		ld	hl,patbuf
		call	cf_setsector
		jr	c,disk_err
		call	cf_cmdwrite
		jr	nz,disk_err
		ld	hl,patbuf
		call	cf_writesec
		pop	de
		pop	bc
		xor	a
		ret

; disk read/write error
disk_err:	pop	de
		pop	bc
		scf
		ret

; decrease 24-bit sector number
dec_sector:	dec	de
		ld	a,d
		and	e
		inc	a
		ret	nz
		dec	c
		ret

t_write:	db	13,10,"Disk write test:",13,10,"$"
t_secerr:	db	"LBA disk size error",13,10,"$"
t_pass1:	db	"pass 1 $"
t_pass2:	db	"pass 2 $"
t_ok:		db	"ok",13,10,"$"
t_nok:		db	"failed, sector 0x$"
t_sector:	db	"sector data:",13,10,"$"
t_write_err:	db	"disk write error",13,10,"$"
t_read_err:	db	"disk read error",13,10,"$"

; ------------------------------------------------------------------------------
; *** Print and input subroutines ***
; ------------------------------------------------------------------------------

; Use BDOS to print character
PrintC:		push	bc
		push	de
		push	hl
	IFDEF BASBIN
		call	CHPUT
	ELSE
		ld	c,6
		ld	e,a
		call	5
	ENDIF
		pop	hl
		pop	de
		pop	bc
		ret
		
; Print string terminated with $
PrintText:	push	de
string_loop:	ld	a,(de)
		inc	de
		cp	'$'
		jr	z,string_end
		call	PrintC
		jr	string_loop
string_end:	pop	de
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

; Use BDOS to get keyboard input
GetC:		push	bc
		push	de
		push	hl
	IFDEF BASBIN
		call	CHGET
	ELSE
		ld	c,1
		ld	e,a
		call	5
	ENDIF
		pop	hl
		pop	de
		pop	bc
		ret

; ------------------------------------------------------------------------------
; *** IDE hardware interface ***
; ------------------------------------------------------------------------------
IDE_CMD_READ	equ	$20		; read sector
IDE_CMD_WRITE	equ	$30		; write sector
IDE_CMD_DIAG	equ	$90		; diagnostic test
IDE_CMD_INFO	equ	$ec		; disk info
IDE_CMD_FEATURE	equ	$ef		; set feature

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

BEER_READ	equ	$40		; /rd=0 /wr=1 /cs=0
BEER_WRITE	equ	$80		; /rd=1 /wr=0 /cs=0
BEER_SET	equ	$c0		; /rd=1 /wr=1 /cs=0
	IFDEF BEER_CS
BEER_IDLE	equ	$c7		; /rd=1 /wr=1 /cs=0 reg=7
BEER_OFF	equ	$e7		; /rd=1 /wr=1 /cs=1 reg=7
	ELSE
BEER_IDLE	equ	$e7		; /rd=1 /wr=1 /cs=1 reg=7
	ENDIF

; PPI 8255 I/O registers:
PPI_IOA		equ	$30		; A: IDE data low byte
PPI_IOB		equ	$31		; B: IDE data high byte
PPI_IOC		equ	$32		; C: IDE control
PPI_CTL		equ	$33		; PPI control

; ------------------------------------------
; Initialize disk
; Output: Z-flag set if hardware detected
; probe for PPI 8255 hardware on fixed IO port
; ------------------------------------------
beerInit:	ld	a,PPI_IOA
		ld	(ix+0),a
		ld	a,PPI_CTL
		ld	(ix+1),a
		call	beerOutput
		ld	hl,$00a5		; register=0 value=a5
		call	beerSetReg
		in	a,(PPI_IOA)
		cp	$a5
		ld	hl,beer_tab
		ret	nz

	IFDEF BEER_TEST_1
		; set PIO mode 0
		call	beerWaitReady
		ret	c
		call	beerOutput
		ld	hl,$0103		; set PIO mode subcommand
		call	beerSetReg
		ld	hl,$0208		; set PIO mode 0
		call	beerSetReg
		ld	hl,$06e0		; LBA mode / drive 0
		call	beerSetReg
		ld	h,REG_COMMAND
		ld	l,IDE_CMD_FEATURE
		call	beerSetReg
	ENDIF
		
		ld	hl,beer_tab
		xor	a
		ret

beer_tab:	jp	beerInfo
		jp	beerDiag
		jp	beerSetSector
		jp	beerCmdRead
		jp	beerReadSector
		jp	beerReadOptm
		jp	beerWaitReady
		jp	beerWaitData
		jp	beerError
		jp	beerReadX
		jp	beerCmdWrite
		jp	beerWriteSec

; ------------------------------------------
; Get IDE device information 
; ------------------------------------------
beerInfo:	call	beerWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		call	beerCommand
		call	beerWaitData
		jp 	nz,beerError
	IFDEF BEER_TEST
		call	beerReadOptm
	ELSE
		call	beerReadSector
	ENDIF
		xor	a
		ret

; ------------------------------------------
; IDE diagnostic 
; ------------------------------------------
beerDiag:	call	beerWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_DIAG
		call	beerCommand
		call	beerWaitReady
		ret	c
		jp	beerError

; ------------------------------------------
; IDE set sector start address and number of sectors
; Input: C,D,E = 24-bit sector number
; ------------------------------------------
beerSetSector:	call	beerWaitReady
		ret	c
		call	beerOutput
		push	hl
		ld	h,$02			; IDE register 2
beerNsector:	ld	l,$01			; number of sectors is N
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
		ld	l,$e0			; LBA mode
		call	beerSetReg
	IFDEF BEER_CS
		ld	a,BEER_IDLE		; IDE register 7
		out	(PPI_IOC),a		; set address
	ENDIF
		pop	hl
		xor	a
		ret

; ------------------------------------------
; IDE set command read sector
; ------------------------------------------
beerCmdRead:	push	bc
		ld 	a,IDE_CMD_READ
		call	beer_command_1		; direction already set to output
		call	beerWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Read Sector
; Input: HL = transfer address
; ------------------------------------------

; Copy of BEER 1.9 code
beerReadSector:	ld	a,BEER_SET+REG_DATA
		out	(PPI_IOC),a
		ld	c,PPI_IOA
		ld	d,0
beer_rd_loop:	ld	a,BEER_READ
		ld	b,005h
		out	(PPI_IOC),a
		ini
		inc	c
		ini
		dec	c
	IFDEF BEER_CS
		ld	a,BEER_SET+REG_DATA
	ELSE
		ld	a,BEER_IDLE
	ENDIF
		out	(PPI_IOC),a
		dec	d
		jr	nz,beer_rd_loop
		ret

; ------------------------------------------
; throughput optimization: 
; total  93 MSX T-states in 2-byte read loop, 3.580.000Hz/( 93x256x2)=75KB/s
; total 172 MSX T-states in 4-byte read loop, 3.580.000Hz/(172x128x2)=81KB/s
beerReadOptm:	ld	a,PPI_IOA
		ld	b,$80
		ld	c,PPI_IOC
		ld	d,BEER_READ
		ld	e,BEER_SET+REG_DATA
		out	(c),e
	IFDEF BEER_TEST_3
		ex	(sp),hl
		ex	(sp),hl
		ex	(sp),hl
		ex	(sp),hl
	ENDIF
beer_opt_loop:	out	(c),d			; 14T
	IFDEF BEER_TEST_3
		nop
		nop
	ENDIF
		ld	c,a			; 05T
		ini				; 18T
		inc	c			; 05T
		ini				; 18T
		inc	c			; 05T
		out	(c),e			; 14T
		; repeat 2-byte read before looping to increase throughput:
		; saves 128x14 T-states with 11 extra bytes of code
	IFDEF BEER_TEST_3
		nop
		nop
		out	(c),d
		nop
		nop
	ELSE
		out	(c),d
	ENDIF
		ld	c,a
		ini
		inc	c
		ini
		inc	c
		out	(c),e
		djnz	beer_opt_loop		; 14T
		ret

; ------------------------------------------
beerReadX:	ld	b,$20
		call	beerInput
beer_outer:	call	beer_waitdata_1
		ret	nz
		push	bc
		ld	a,PPI_IOA
		ld	b,$80
		ld	c,PPI_IOC
		ld	d,BEER_READ
		ld	e,BEER_SET+REG_DATA
		out	(c),e
beer_inner:	out	(c),d
		ld	c,a
		ini	
		inc	c
		ini
		inc	c
		out	(c),e
		; repeat
		out	(c),d
		ld	c,a
		ini
		inc	c
		ini
		inc	c
		out	(c),e
		djnz	beer_inner
	IFDEF DEBUG_1
		ld	a,'-'
		call	PrintC
	ENDIF
		pop	bc
		djnz	beer_outer
		ret

; ------------------------------------------
; IDE set command write sector
; ------------------------------------------
beerCmdWrite:	push	bc
		ld	a,IDE_CMD_WRITE
		call	beer_command_1		; direction already set to output

		ld	b,$30
wr_wait:	ex	(sp),hl
		ex	(sp),hl
		djnz	wr_wait
		xor	a			; no error

		pop	bc
		ret
; ------------------------------------------
; IDE Write Sector
; Input: HL = transfer address
; ------------------------------------------
beerWriteSec:	ld	a,PPI_IOA
		ld	b,$80			; counter: 128x4=512 bytes
		ld	c,PPI_IOC
		ld	d,BEER_WRITE
		ld	e,BEER_SET
		out	(c),e
beer_wr_loop:	ld	c,a
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
		djnz	beer_wr_loop
		ret

; ------------------------------------------
; Wait for IDE ready or time-out
; ------------------------------------------
beerWaitReady:	push	hl
		push	bc
		call	beerInput
		ld	b,$14			; time-out after 20 seconds
beer_wait_1:	ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
beer_wait_2:	call	beerStatus
		and	%11000000
		cp	%01000000		; BUSY=0 RDY=1 ?
		jr	z,beer_wait_end		; z=yes
		dec	hl
		ld	a,h
		or	l
		jr	nz,beer_wait_2
		djnz	beer_wait_1
		scf				; time-out
beer_wait_end:
	IFDEF BEER_CS
		ld	a,BEER_OFF		; deassert CS / IDE register 7
		out	(PPI_IOC),a		; set address
	ENDIF
		pop	bc
		pop	hl
		ret

; ------------------------------------------
; Wait for IDE data read/write request
; ------------------------------------------
beerWaitData:	call 	beerInput
beer_waitdata_1:
	IFDEF DEBUG
		call	beer_dumpreg		; debug
	ENDIF
		call	beerStatus
		bit	7,a			; IDE busy?
		jr	nz,beer_waitdata_1	; nz=yes
		bit	0,a			; IDE error?
		ret	nz			; nz=yes
		bit	3,a			; IDE data request?
		jr	z,beer_waitdata_1	; z=no
		xor	a			; no error
		ret
		
; debug: dump registers
beer_dumpreg:	ld	a,BEER_SET+1
		call	beerReadReg
		ld	(ppireg),a
		ld	a,BEER_SET+2
		call	beerReadReg
		ld	(ppireg+1),a
		ld	a,BEER_SET+3
		call	beerReadReg
		ld	(ppireg+2),a
		ld	a,BEER_SET+4
		call	beerReadReg
		ld	(ppireg+3),a
		ld	a,BEER_SET+5
		call	beerReadReg
		ld	(ppireg+4),a
		ld	a,BEER_SET+6
		call	beerReadReg
		ld	(ppireg+5),a
		ret

; ------------------------------------------
; IDE error handling
; ------------------------------------------
beerError:	ld	a,BEER_SET+REG_ERROR
		jr	beerReadReg

; ------------------------------------------
; PPI IDE read status register
; ------------------------------------------
beerStatus:	ld	a,BEER_SET+REG_STATUS
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
	ELSE
		out	(PPI_IOC),a
		res	7,a			; /rd=0
		out	(PPI_IOC),a
		in	a,(PPI_IOA)		; read register
		ex	af,af'
		ld	a,BEER_IDLE
		out	(PPI_IOC),a
		ex	af,af'
	ENDIF
		ret

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
beerSetReg:	ld	a,BEER_SET
		add	a,h
		out	(PPI_IOC),a
		ld	a,l
		out 	(PPI_IOA),a
		ld 	a,BEER_WRITE
		add	a,h
		out 	(PPI_IOC),a
	IFDEF BEER_CS
		ld	a,BEER_SET
		add	a,h
	ELSE
		ld	a,BEER_IDLE
	ENDIF
		out 	(PPI_IOC),a
		ret 

; ------------------------------------------
; PPI IDE set data direction
; ------------------------------------------
beerInput:
	IFDEF BEER_TEST_2
		; quickly recover from 8255 mode change glitch
		ex	af,af'
		ld	a,PPI_INPUT		; PPI A+B is input
		push	bc
		ld	b,BEER_IDLE
		ld	c,PPI_IOC
		out	(PPI_CTL),a		; PPI output ports are reset: both read and write asserted
		out	(c),b			; restore control: read/write deasserted
		pop	bc
		ex	af,af'
		ret
	ELSE
		ex	af,af'
		ld	a,PPI_INPUT		; PPI A+B is input
		out	(PPI_CTL),a
		ex	af,af'
		ret
	ENDIF

beerOutput:
	IFDEF BEER_TEST_2
		ex	af,af'
		ld	a,PPI_OUTPUT		; PPI A+B is output
		push	bc
		ld	b,BEER_IDLE
		ld	c,PPI_IOC
		out	(PPI_CTL),a		; PPI output ports are reset: both read and write asserted
		out	(c),b			; restore control: read/write deasserted
		pop	bc
		ex	af,af'
		ret
	ELSE
		ex	af,af'
		ld	a,PPI_OUTPUT		; PPI A+B is output
		out	(PPI_CTL),a
		ex	af,af'
		ret
	ENDIF

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

MALT_SET	equ	$08		; assert /cs0
MALT_WRITE	equ	$28		; assert /write
MALT_READ	equ	$48		; assert /read
MALT_RESET	equ	$80		; assert /reset
MALT_BITWR	equ	$05
MALT_BITRD	equ	$06

; ------------------------------------------
; Initialize disk
; Output: Z-flag set if hardware detected
; probe for PPI 8255 hardware on fixed IO port
; ------------------------------------------
maltInit:	ld	hl,maltPorts
maltProbe:	ld	a,(hl)
		cp	$ff
		jr	z,maltNotDetected
		call	maltInit1
		ret	z
		inc	hl
		jr	maltProbe

maltInit1:	ld	(ix+0),a		; PPIDE Data
		add	a,$03
		ld	(ix+1),a		; PPIDE Control
		call	maltOutput
		push	hl
		ld	hl,$00a5		; register=0 value=a5
		call	maltSetReg
		pop	hl
		ld	c,(ix+0)
		in	a,(c)
		cp	$a5
		ret	nz

		call	maltReset
		ld	hl,malt_tab
		xor	a
		ret

maltNotDetected:
		or	a
		ret

; List of ports that are probed, end with $ff
maltPorts:	db	$3c,$34,$1c,$14,$ff

malt_tab:	jp	maltInfo
		jp	maltDiag
		jp	maltSetSector
		jp	maltCmdRead
		jp	maltReadSector
		jp	maltReadSector
		jp	maltWaitReady
		jp	maltWaitData
		jp	maltError
		jp	maltReadX
		jp	maltCmdWrite
		jp	maltWriteSec

; ------------------------------------------
; Get IDE device information
; ------------------------------------------
maltInfo:	call	maltWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_INFO
		call	maltCommand
		call	maltWaitData
		jp 	nz,maltError
		call	maltReadSector
		xor	a
		ret

; ------------------------------------------
; IDE diagnostic
; ------------------------------------------
maltDiag:	call	maltWaitReady
		ret	c			; time-out
		ld	a,IDE_CMD_DIAG
		call	maltCommand
		call	maltWaitReady
		ret	c
		jp	maltError

; ------------------------------------------
; IDE set sector start address and number of sectors
; Input: C,D,E = 24-bit sector number
; ------------------------------------------
maltSetSector:	call	maltWaitReady
		ret	c
		push	hl
		push	bc
		call	maltOutput
		pop	bc
		push	bc
		ld	h,$02			; IDE register 2
maltNsector:	ld	l,$01			; number of sectors is N
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
		ld	l,$e0			; LBA mode
		call	maltSetReg
		pop	bc
		pop	hl
		xor	a
		ret

; ------------------------------------------
; IDE set command read sector
; ------------------------------------------
maltCmdRead:	push	bc
		ld 	a,IDE_CMD_READ
		call	malt_command_1		; direction already set to output
		call	maltWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Read Sector
; Input: HL = transfer address
; ------------------------------------------
maltReadSector:	ld	a,(ix+0)
		ld	b,$80
		ld	c,a
		inc	c
		inc	c
		ld	d,MALT_READ
		ld	e,MALT_SET+REG_DATA
		out	(c),e
malt_rd_loop:
		REPT 2
		out	(c),d
		ld	c,a
		ini
		inc	c
		ini
		inc	c
		out	(c),e
		ENDR
		djnz	malt_rd_loop
		ret

; ------------------------------------------
maltReadX:	ld	b,$20
		push	bc
		call	maltInput
		pop	bc
malt_outer:	push	bc
		call	malt_waitdata_1
		pop	bc
		ret	nz
		push	bc
		ld	a,(ix+0)
		ld	b,$80
		ld	c,a
		inc	c
		inc	c
		ld	d,MALT_READ
		ld	e,MALT_SET+REG_DATA
		out	(c),e
malt_inner:
		REPT 2
		out	(c),d
		ld	c,a
		ini
		inc	c
		ini
		inc	c
		out	(c),e
		ENDR
		djnz	malt_inner
	IFDEF DEBUG_1
		ld	a,'-'
		call	PrintC
	ENDIF
		pop	bc
		djnz	malt_outer
		ret

; ------------------------------------------
; IDE set command write sector
; ------------------------------------------
maltCmdWrite:	push	bc
		ld	a,IDE_CMD_WRITE
		call	malt_command_1		; direction already set to output
		call	maltWaitData
		call	maltOutput
		pop	bc
		ret

; ------------------------------------------
; IDE Write Sector
; Input: HL = transfer address
; ------------------------------------------
maltWriteSec:	ld	a,(ix+0)
		ld	b,$80			; counter: 128x4=512 bytes
		ld	c,a
		inc	c
		inc	c
		ld	d,MALT_WRITE
		ld	e,MALT_SET
		out	(c),e
malt_wr_loop:
		REPT 2
		ld	c,a
		outi
		inc	c
		outi
		inc 	c
		out	(c),d
		out	(c),e
		ENDR
		djnz	malt_wr_loop
		ret

; ------------------------------------------
; Wait for IDE ready or time-out
; ------------------------------------------
maltWaitReady:	push	hl
		push	bc
		call	maltInput
		ld	b,$14			; time-out after 20 seconds
malt_wait_1:	ld	hl,$4000		; wait loop appr. 1 sec for MSX/3.58Mhz
malt_wait_2:	call	maltStatus
		and	%11000000
		cp	%01000000		; BUSY=0 RDY=1 ?
		jr	z,malt_wait_end		; z=yes
		dec	hl
		ld	a,h
		or	l
		jr	nz,malt_wait_2
		djnz	malt_wait_1
		scf				; time-out
malt_wait_end:	pop	bc
		pop	hl
		ret

; ------------------------------------------
; Wait for IDE data read/write request
; ------------------------------------------
maltWaitData:	call 	maltInput
malt_waitdata_1:
		call	maltStatus
		bit	7,a			; IDE busy?
		jr	nz,malt_waitdata_1	; nz=yes
		bit	0,a			; IDE error?
		ret	nz			; nz=yes
		bit	3,a			; IDE data request?
		jr	z,malt_waitdata_1	; z=no
		xor	a			; no error
		ret

; debug: dump registers
malt_dumpreg:	ld	de,t_reg
		call	PrintText
		call	maltInput
		ld	a,MALT_SET+2
		call	maltReadReg
		call	PrintHexByte
		ld	a,MALT_SET+3
		call	maltReadReg
		call	PrintHexByte
		ld	a,MALT_SET+4
		call	maltReadReg
		call	PrintHexByte
		ld	a,MALT_SET+5
		call	maltReadReg
		call	PrintHexByte
		ld	a,MALT_SET+6
		call	maltReadReg
		call	PrintHexByte
		ld	a,MALT_SET+7
		call	maltReadReg
		call	PrintHexByte
		call	PrintCRLF
		ret

t_reg:		db	"Register dump: $"

; ------------------------------------------
; IDE error handling
; ------------------------------------------
maltError:	ld	a,MALT_SET+REG_ERROR
		jr	maltReadReg

; ------------------------------------------
; PPI IDE read status register
; ------------------------------------------
maltStatus:	ld	a,MALT_SET+REG_STATUS
maltReadReg:	push	bc
		ld	b,a
		ld	c,(ix+0)
		inc	c
		inc	c			; select PPI port C
		out	(c),b
		set	MALT_BITRD,b		; assert read
		out	(c),b
		dec	c
		dec	c			; select PPI port A
		in	a,(c)			; read register
		inc	c
		inc	c			; select PPI port C
		res	MALT_BITRD,b		; deassert read
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
		ld	c,(ix+0)
		out	(c),l			; write value
		inc	c
		inc	c
		ld	a,MALT_SET
		add	a,h
		out	(c),a			; set register
		set	MALT_BITWR,a		; assert write
		out	(c),a
		res	MALT_BITWR,a		; deassert write
		out 	(c),a
		pop	bc
		ret

; ------------------------------------------
; PPI IDE reset disk controller
; do hard reset, no soft reset
; ------------------------------------------
maltReset:	ld	c,(ix+0)
		inc	c
		inc	c
		ld	a,MALT_RESET
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
		ld	c,(ix+1)
		out	(c),b
		ret

maltOutput:	ld	b,PPI_OUTPUT		; PPI A+B is output
		ld	c,(ix+1)
		out	(c),b
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
		jp	cfideError
		jp	cfideReadX
		jp	cfideCmdWrite
		jp	cfideWriteSec

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
cfideNsector:	ld	a,$01			; number of sectors is 1
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
; Block read
cfideReadX:	ld	b,$20
cf_outer:	call	cfideWaitData
		ret	nz
		push	bc
		ld	b,$20		; counter: 32x16=512 bytes
		ld	c,(ix+0)
cf_inner:
		REPT 16			; repeat: read 16 bytes
		ini			; 16x ini in a loop is faster than inir
		ENDR
		djnz	cf_inner
		pop	bc
		djnz	cf_outer
		ret


; ------------------------------------------
; IDE set command write sector
; ------------------------------------------
cfideCmdWrite:	push	bc
		ld 	a,IDE_CMD_WRITE
		ld	c,(ix+1)
		out	(c),a
		call	cfideWaitData
		pop	bc
		ret

; ------------------------------------------
; IDE Write Sector
; Input: HL = transfer address
; ------------------------------------------
cfideWriteSec:	ld	b,$20
		ld	c,(ix+0)
cf_wr_loop:
		REPT 16
		outi
		ENDR
		djnz	cf_wr_loop
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
ppireg:		ds	6
lastsec:	ds	4
secbuf:		ds	512
patbuf:		ds	513

; ------------------------------------------------------------------------------
TasteEnd:


