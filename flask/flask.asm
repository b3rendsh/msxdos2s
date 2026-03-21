; -----------------------------------------------------------------------------
; FLASK
; SST simple 64K flash ROM writer
;
; Copyright (C) 2026 H.J. Berends*
;
; *substantial parts are based on WRTSST,
; copyright (C)2022-2026 Takayuki Hara (HRA!)
;
; Permission is hereby granted, free of charge, to any person obtaining a copy of this
; software and associated documentation files (the "Software"), to deal in the Software 
; without restriction, including without limitation the rights to use, copy, modify, merge, 
; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons 
; to whom the Software is furnished to do so, subject to the following conditions:
; The above copyright notice and this permission notice shall be included in all copies or
; substantial portions of the Software.
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
; SOFTWARE.
; -----------------------------------------------------------------------------
; Differences with WRTSST:
; + assemble with z88dk's z80asm
; + default sector erase and chip erase as option
; + cosmetic text changes and code optimizations
; + simplified address option
; + no banked flash rom support
; todo:
; + add verify option
; + add read option
; -----------------------------------------------------------------------------

;DEFINE	TEST

RAMAD0	EQU	$F341
RAMAD1	EQU	$F342
RAMAD2	EQU	$F343
RAMAD3	EQU	$F344
JIFFY	EQU	$FC9E
EXPTBL	EQU	$FCC1
EXPTBLO	EQU	$C1		; low byte of EXPTBL
CALSLT	EQU	$001C
ENASLT	EQU	$0024
BDOS	EQU	$0005
_TERM0	EQU	$00
_DIRIO	EQU	$06
_STROUT	EQU	$09
_FOPEN	EQU	$0F
_FCLOSE	EQU	$10
_SETDTA	EQU	$1A
_RDBLK	EQU	$27

CR	EQU	$0D
LF	EQU	$0A
CTRL_Z	EQU	$1A

NEW_DTA	EQU	$2000

; -----------------------------------------------------------------------------

		ORG	$100

		jp	main
		
		; identification
		db	CR,LF,"MSX FLASK V1.0",CR,LF
	IFDEF TEST
		db	"TEST version",CR,LF
	ENDIF
		db	CTRL_Z

main:		; print title
		ld	de,t_header
		call	puts

		; parse commandline parameters
		call	parse_params
		ld	a,(fcb_fname)
		cp	$20
		jp	z,usage

		; print image filename
		call	dsp_fname
		call	puts_crlf

		; open image file
		call	file_open
		jp	z,puts_and_exit

		; check file size
		call	check_fsize
		jp	nz,puts_and_exit

		; copy subroutines to page 2
		call	transfer_to_page2

		; detect/set flash rom slot
		call	check_slot
		jp	nz,puts_and_exit

		call	restore_slot

		; print flash rom info
		call	dsp_slot
		call	puts_crlf

		; erase flash rom
		ld	a,(chip_erase)
		inc	a
		jr	z,erase_all
		ld	de,t_erase
		call	puts
		call	flash_64k_erase
		jr	erase_done
		
		; erase entire flash ROM
erase_all:	ld	de,t_erase_all		
		call	puts
		call	flash_chip_erase

erase_done:	; prepare block copy
		ld	de,t_ok
		call	puts
		ld	de,t_address
		call	puts
		ld	hl,(file_size)
		push	hl
		call	flash_get_start		; get start block and address
		ld	c,a			; start block
		push	bc
		call	puthex16		; print start address in hl
		call	puts_crlf
		pop	bc
		pop	hl
		push	hl
		srl	h			; hl / 2
		rr	l
		srl	h			; hl / 2
		rr	l
		srl	h			; hl / 2
		rr	l			; hl = filesize / 8KB blocks
		ld	b,l			; max 64 (512KB)
		ld	a,l
		ld	(progress_max),a
		pop	hl

block_wr_loop:	ld	a,c
		push	bc
		ld	(bank_back),a		; set bank (block address)

		ld	a,b
		call	dsp_progress

		; init block data
		ld	hl,NEW_DTA
		ld	de,NEW_DTA+1
		ld	bc,8192-1
		ld	(hl),$ff
		ldir

		; read next 8KB block data from image
		ld	c,_RDBLK
		ld	de,fcb
		ld	hl,8
		call	BDOS

		call	flash_write_8kb
		pop	bc
		ld	de,t_write_error
		jr	c, puts_and_exit
		inc	c			; next bank (block address)
		djnz	block_wr_loop

		ld	a,b
		call	dsp_progress
		call	puts_crlf

		; close the file
		ld	c,_FCLOSE
		ld	de,fcb
		call	BDOS

		; print completed and quit
		ld	de,t_completed

puts_and_exit:	push	de
		call	restore_slot
		pop	de
		call	puts

		ld	c,_TERM0
		jp	BDOS

t_header:	db	"FLASK: write image to SST flash ROM",CR,LF,LF,0
t_address:	db	"Address   : 0x",0
t_erase:	db	"Erase 64K : ",0
t_erase_all:	db	"Erase ROM : ",0
t_ok:		db	"ok",CR,LF,0
t_completed:	db	CR,LF,"Completed.",CR,LF,0
t_not_detected:	db	CR,LF,"Could not detect SST flash ROM.",CR,LF,0
t_too_large:	db	CR,LF,"File too large.",CR,LF,0
t_write_error:	db	CR,LF,LF,"Write error.",CR,LF,0

; -----------------------------------------------------------------------------
; Parse commandline parameters
; -----------------------------------------------------------------------------

get_one:	ld	a,(hl)
		inc	hl
		dec	b
		ret

parse_params:	ld	hl,$0080
		ld	a,(hl)		; Length of command line parameters
		or	a
		jp	z,usage
		ld	b,a
		inc	hl
parse_l1:	call	get_one
		cp	'/'
		jr	z,option
		cp	$20
		jr	nz,file_name
		inc	b
		djnz	parse_l1
		ret

option:		call	get_one
		jp	z,usage

		res	5,A		; to uppercase
		cp	'S'
		jp	z,option_s
		cp	'A'
		jp	z,option_a
		cp	'E'
		jp	z,option_e
		jp	usage

option_s:	call	get_one
		; The slot number is 0 to 3
		sub	'0'
		cp	4
		jp	nc,usage
		ld	(target_slot),a
		ld	a, (hl)
		cp	'-'
		jp	z,usage		; expansion slots not supported
		jr	parse_l1
		
option_a:	ld	a,$04
		ld	(target_block),a
		jr	parse_l1

option_e:	ld	a,$ff
		ld	(chip_erase),a
		jr	parse_l1

file_name:	; If a file name has already been specified, the system displays usage and exits.
		ld	a,(fcb_fname)
		cp	$20
		jp	nz,usage
		ld	c,8
		ld	de,fcb_fname
		dec	hl
		inc	b
fl1:		call	get_one
		cp	'.'
		jp	z,file_ext
		cp	$20
		jp	z,parse_l1
		ld	(de),a
		inc	de

		inc	b
		dec	b
		ret	z

		dec	c
		jr	nz,fl1

		call	get_one
		cp	'.'
		jp	nz,usage

file_ext:	inc	b
		dec	b
		ret	z

		ld	c,3
		ld	de,fcb_fext
fl2:		call	get_one
		cp	$20
		jp	z,parse_l1
		ld	(de),a
		inc	de

		inc	b
		dec	b
		ret	z

		dec	c
		jr	nz,fl2
		jp	parse_l1

; -----------------------------------------------------------------------------
; Display progress bar
; Input: a = progress value ( 0: 100%, progress_max: 0% )
; -----------------------------------------------------------------------------
dsp_progress:	push	af
		; fill '-' into progress.
		ld	hl,progress
		ld	de,progress + 1
		ld	bc,16 - 1
		ld	a,'-'
		ld	(hl), a
		ldir
		pop	af
		; Convert progress value: A=0 is 0%, A=progress_max is 100%
		ld	b,a
		ld	a,(progress_max)
		sub	b
		; D = A * 16 / progress_max
		push	af
		ld	a,(progress_max)
		ld	e,a
		pop	af
		ld	c,a
		xor	a
		ld	d,a
		ld	b,12
divide_loop:	sla	c
		rla
		sub	e
		jr	nc,skip_add
		add	a,e
skip_add:	ccf
		rl	d
		djnz	divide_loop
		; check 0%
		ld	a,d
		or	a
		jr	z,skip_fill
		; fill '#' into progress.
		ld	c,a				; b is already zero.
		ld	hl,progress
		ld	de,progress + 1
		ld	a, '#'
		ld	(hl), a
		dec	c
		jr	z,skip_fill
		ldir
skip_fill:	ld	de,progress_bar
		call	puts
		ret

progress_bar:	db	"Write ROM : |"
progress:	db	$20,$20,$20,$20,$20,$20,$20,$20		; 16 spaces
		db	$20,$20,$20,$20,$20,$20,$20,$20
		db	"|",CR,0

; -----------------------------------------------------------------------------
; Open File
; Output: Zx =  0: success, 1: failed
;         DE = error message (if Zx = 1)
; -----------------------------------------------------------------------------
file_open:	ld	de,fcb
		ld	c, _FOPEN
		call	BDOS
		or	a				; 0: Success, 255: Error
		ld	de,t_cannot_open
		jr	nz, put_error

		; Check file size
		ld	hl,fcb_filsiz
		ld	a,(hl)
		ld	b,a
		inc	hl
		or	(hl)
		inc	hl
		or	(hl)
		inc	hl
		or	(hl)
		ld	de,t_is_zero
		jr	z,put_error

		; calc KB
		ld	a,(fcb_filsiz + 1)
		and	$1F
		or	b
		ld	de,0
		jr	z,is_mult_8kb
		ld	e,8
is_mult_8kb:	ld	hl,(fcb_filsiz + 1)	; HL = (fcb_filesiz) / 256
		srl	h
		rr	l			; HL = HL / 2
		srl	h
		rr	l			; HL = HL / 2
		add	hl,de			; HL = (fcb_filesiz) / 1024
		ld	a,l
		and	$F8
		ld	l,a
		ld	(file_size),hl

		; set DTA
		ld	c, _SETDTA
		ld	de, NEW_DTA
		call	BDOS

		; set record size
		ld	hl,1024
		ld	(fcb_s2),hl
		inc	l			; Zx = 0
		ret

put_error:	xor	a			; Zx = 1
		ret

t_cannot_open:	db	"Cannot open file.",CR,LF,0
t_is_zero:	db	"File is empty.",CR,LF,0

; -----------------------------------------------------------------------------
; Display usage and return to DOS.
; -----------------------------------------------------------------------------
usage:		call	restore_slot
		ld	de,t_usage
		call	puts
		ld	c, _TERM0
		jp	BDOS

t_usage:	db	"Usage: FLASK [options] filename.rom",CR,LF
		db	"  options:",CR,LF
		db	"  /Sx .... Set target slot #x",CR,LF
		db	"  /A ..... Set address to 0x8000",CR,LF
		db	"  /E ..... Erase entire flash ROM",CR,LF
		db	0
		; todo
		db	"  /V ..... Verify ROM image",CR,LF
		db      "  /R ..... Read ROM image",CR,LF
		db	0

; -----------------------------------------------------------------------------
; Display target slot
; -----------------------------------------------------------------------------
dsp_slot:	ld	de,t_slot
		call	puts

		ld	a,(target_slot)
		and	3
		call	puthex_c

		ld	a,(target_slot)
		rlca
		ret	nc

		ld	de,t_bar
		call	puts

		ld	a,(target_slot)
		rra
		rra
		and	3
		jp	puthex_c

t_slot:		db	"Slot#     : ",0
t_bar:		db	"-",0

; -----------------------------------------------------------------------------
; display target file name
; -----------------------------------------------------------------------------
dsp_fname:	ld	de,t_fname
		call	puts

		ld	hl,fcb_fname
		ld	b,8
@l1:		ld	a,(hl)
		inc	hl
		cp	$20
		jr	z,@s1
		push	hl
		push	bc
		ld	e, a
		ld	c,_DIRIO
		call	BDOS
		pop	bc
		pop	hl
		djnz	@l1
@s1:		ld	e,'.'
		ld	c,_DIRIO
		call	BDOS

		ld	hl,fcb_fext
		ld	b,3
@l2:		ld	a,(hl)
		inc	hl
		cp	$20
		ret	z
		push	hl
		push	bc
		ld	e,a
		ld	c, _DIRIO
		call	BDOS
		pop	bc
		pop	hl
		djnz	@l2
		ret

t_fname:	db	"File name : ",0

; -----------------------------------------------------------------------------
; Check file size
; Output: Zx = 0: too large, 1: ok
; -----------------------------------------------------------------------------
check_fsize:	ld	a,(file_size)
		ld	de,t_too_large
		cp	65			; file size <= 64K ?
		jr	nc,too_large
		ld	a,(file_size+1)
too_large:	or	a			; file size <= 64K ?
		ret

; -----------------------------------------------------------------------------
; Check target slot
; Output: Zx = 0: not detected, 1: detected
; -----------------------------------------------------------------------------
check_slot:	ld	a,(target_slot)
		inc	a
		jp	nz,target_param		; If a slot number is specified, return without doing anything.

		ld	hl,EXPTBL
target_l1:	ld	a,l
		sub	EXPTBLO			; low byte of EXPTBL address
		ld	b,a
		ld	a,(hl)
		and	$80
		or	b
		or	a
		jp	m,expanded_slot

basic_slot:	ld	(target_slot),a
		push	hl
		call	detect_target
		pop	hl
		ret	z
		jr	next_slot

expanded_slot:	or	$80
		ld	(target_slot),a
		push	hl
		call	detect_target
		pop	hl
		ret	z
		ld	a,(target_slot)
		add	a,$04
		cp	$90
		jr	c,expanded_slot

next_slot:	ld	a,l
		inc	a
		ld	l,a
		cp	EXPTBLO + 4
		jr	c,target_l1

		xor	a
		inc	a
		ret

target_param:	dec	a
		jp	detect_target

; -----------------------------------------------------------------------------
; restore DOS RAM TPA slots
; -----------------------------------------------------------------------------
restore_slot:	ld	a,(RAMAD1)
		ld	h,$40
		call	ENASLT
		ld	a,(RAMAD2)
		ld	h,$80
		call	ENASLT
		ei
		ret

; -----------------------------------------------------------------------------
; Detect_target
; Input:  (target_slot)
; Output:  Zx = 0: not detected, 1: detected
; -----------------------------------------------------------------------------
detect_target:	ld	a,(target_slot)
		call	is_slot_flash
		ld	de,t_not_detected
		ret	nz			; nz - not detected

		; Print file size
		ld	de,t_file_size
		call	puts
		ld	hl,(file_size)
		call	putdec
		ld	de,t_kb
		call	puts

		; Get/print ROM information
		call	restore_slot
		ld	de,t_device_id
		call	puts
		ld	a,(device_id)
		call	get_device_name
		call	puts
		call	puts_crlf
		ld	de,t_rom_size
		call	puts
		ld	hl,(rom_size)
		call	putdec
		ld	de,t_kb
		call	puts

		xor	a
		ret

t_file_size:	db	"File size : ",0
t_device_id:	db	"ROM info  : ",0
t_rom_size:	db	"ROM size  : ",0
t_kb:		db	"KB",CR,LF,0

; -----------------------------------------------------------------------------
;  DATA
; -----------------------------------------------------------------------------
target_slot:	db	$FF		; 0xFF: auto
file_size:	dw	0		; KB
rom_size:	dw	0		; KB
target_block:	db	0		; 0..7: 8K block number
manufact_id:	db	0
device_id:	db	0
progress_max:	db	0
bank_back:	db	0
chip_erase:	db	0		; 0:sector erase, 255:chip erase
fcb:
fcb_dr:		db	0		; 0: Default Drive, 1: A, 2: B, ... 8: H
fcb_fname:	ds	8,$20
fcb_fext:	ds	3,$20
fcb_ex:		db	0
fcb_s1:		db	0
fcb_s2:		db	0
fcb_rc:		db	0
fcb_filsiz:	dw	0,0
fcb_date:	dw	0
fcb_time:	dw	0
fcb_devid:	db	0
fcb_dirloc:	db	0
fcb_strcls:	dw	0
fcb_clrcls:	dw	0
fcb_clsoff:	dw	0
fcb_cr:		db	0
fcb_rn:		dw	0,0

; -----------------------------------------------------------------------------
; Standard I/O subroutines
; -----------------------------------------------------------------------------

crlf:		db	CR,LF,0
puts_crlf:	ld	de,crlf
puts:		ld	a,(de)
		inc	de
		or	a
		ret	z
		ld	c,_DIRIO
		push	de
		ld	e,a
		call	BDOS
		pop	de
		jr	puts

puthex16:	push	hl
		ld	a,h
		call	puthex8
		pop	hl
		ld	a,l
puthex8:	push	af
		rrca
		rrca
		rrca
		rrca
		call	puthex_c
		pop	af
puthex_c:	and	$0F
		ld	hl,hex_chars
		ld	d,0
		ld	e,a
		add	hl,de
		ld	e,(hl)
		ld	c,_DIRIO
		jp	BDOS
hex_chars:	db	"0123456789ABCDEF"

putdec:		ld	bc,str
		ld	de,10000
		call	count_sub
		ld	(bc),a
		inc	bc

		ld	de,1000
		call	count_sub
		ld	(bc),a
		inc	bc

		ld	de,100
		call	count_sub
		ld	(bc),a
		inc	bc

		ld	de,10
		call	count_sub
		ld	(bc),a
		inc	bc

		ld	de,1
		call	count_sub
		ld	(bc),a

		ld	hl,str
		ld	a,'0'
zero_skip:	cp	(hl)
		jr	nz, zero_skip_exit
		inc	hl
		jr	zero_skip
zero_skip_exit:	ex	de,hl
		jp	puts

count_sub:	xor	a		; Cf = 0, A = 0
		ld	a,'0'
@l1:		sbc	hl,de
		jr	c,@s1
		inc	a
		jr	@l1
@s1:		add	hl,de
		ret

str:		db	"00000",0

; -----------------------------------------------------------------------------
; flash rom subroutines
; -----------------------------------------------------------------------------

MID_SST		EQU	$BF
DID_SST39SF010A	EQU	$B5
DID_SST39SF020A	EQU	$B6
DID_SST39SF040	EQU	$B7

; -----------------------------------------------------------------------------
; Get_manufacturer name
; Input:  a = target manufacturer id
; Output: de = target string address.
;         zx = 0: unknown, 1: matched
; -----------------------------------------------------------------------------
get_manufact_name:
		cp	MID_SST
		ld	de,s_sst
		ret	z
		ld	de,s_unknown
		ret

s_sst:		db	"SST",0
s_unknown:	db	"Unknown",0

; -----------------------------------------------------------------------------
; Get_device name
; Input:  a  = target manufacturer id.
; Output: de = target string address.
;         zx = 0: unknown, 1: matched
; -----------------------------------------------------------------------------
get_device_name:
		ld	hl,512
		ld	(rom_size),hl
		cp	DID_SST39SF040
		ld	de,s_sst39sf040
		ret	z

		ld	hl,256
		ld	(rom_size),hl
		cp	DID_SST39SF020A
		ld	de,s_sst39sf020a
		ret	z

		ld	hl,128
		ld	(rom_size),hl
		cp	DID_SST39SF010A
		ld	de,s_sst39sf010a
		ret	z

		ld	hl,0
		ld	(rom_size),hl
		ld	de,s_unknown
		ret

s_sst39sf010a:	db	"SST39SF010A",0
s_sst39sf020a:	db	"SST39SF020A",0
s_sst39sf040:	db	"SST39SF040",0

; -----------------------------------------------------------------------------
; is_rom
; Input:  hl = target address
; output: zx = 0: ROM, 1: RAM
; -----------------------------------------------------------------------------
is_rom:		ld	a,(hl)
		cpl
		ld	(hl),a
		cp	(hl)
		cpl
		ld	(hl),a
		ret

; -----------------------------------------------------------------------------
; 64K flash ROM subroutines
; -----------------------------------------------------------------------------

CMD_2AAA	EQU	$2AAA
CMD_5555	EQU	$5555
PPI_SLOT	EQU	$A8

; -----------------------------------------------------------------------------
; Is slot flash rom
; Input:  a  = target slot
; Output: zx = 0: not flash, 1: flash
;         (manufact_id) = Manufacturer ID / Device ID
; -----------------------------------------------------------------------------
is_slot_flash:	or	a
		jp	m,not_flash		; extended slot not supported

		; Change to target slot on page1 and page2
		push	af
		ld	h, $40
		call	ENASLT
		pop	af
		ld	h,$80
		call	ENASLT

		; Is ROM, Page1 and Page2?
		ld	hl,$4000
		ld	de,$0100
		ld	b,$80
@l1:		call	is_rom
		jr	z, not_flash
		add	hl,de
		djnz	@l1

		call	restore_slot
		call	p2_get_id

		ld	a,(manufact_id)
		call	get_manufact_name
		ret	nz

		ld	a,(manufact_id + 1)
		call	get_device_name
		ret

not_flash:	xor	a
		inc	a
		ret

; -----------------------------------------------------------------------------
; Get start block and address
; Input:  hl = target size [KB]
; Output: a  = start block
;         hl = start address
; Note: target size is already checked
; -----------------------------------------------------------------------------
flash_get_start:
		ld	a,l
		cp	33
		jr	c,file_32kb

		; file larger than 32kb
		xor	a
		ld	(target_block),a
		ld	hl,$0000
		ret

file_32kb:	; check address option
		ld	a,(target_block)
		cp	4
		jr	z,address_p2

		; use default address p1
		ld	a,2
		ld	(target_block),a
		ld	hl,$4000
		ret		

		; option address p2
address_p2:	ld	hl,$8000
		ret

; -----------------------------------------------------------------------------
; Erase 64k / 16 sectors of 4k
; -----------------------------------------------------------------------------
flash_64k_erase:
		ld	b,16		; 16 sectors of 4KB
		ld	c,$00		; Starting page ($0000)
		ld	a,c
loop_sector:	push	bc
		cp	$c0		; goto page3 when between 0x80 and 0xC0, goto page2 when others
		jr	nc,p2_erase
		cp	$80
		jr	nc,p3_erase
p2_erase:	call	p2_sector_erase
		jr	next_sector
p3_erase:	call	sector_erase
next_sector:	pop	bc
		ld	a,c
		add	a,$10		; Move to next 4KB boundary (High byte + 16)
		ld	c,a
		djnz	loop_sector

sector_erase:	di
		exx
		; backup page3 to page1
		ld	hl,$C000
		ld	de,$4000
		ld	bc,$4000
		ldir
		call	transfer_to_page3
		exx
		ld	(save_sp),sp
		ld	sp,$FFFF
		call	p3_sector_erase
		ld	sp,(save_sp)
		ld	hl,$4000
		ld	de,$C000
		ld	bc,$3FFF		; exclude extended slot register
		ldir
		ei
		ret

; -----------------------------------------------------------------------------
; transfer subroutines to page 2
; -----------------------------------------------------------------------------
transfer_to_page2:
		ld	hl,page2_start
		ld	de,p2_start
		ld	bc,p2_end - p2_start
		ldir
		ret

; -----------------------------------------------------------------------------
; transfer subroutines to page 3
; -----------------------------------------------------------------------------
transfer_to_page3:
		; transfer subroutines for page3
		ld	hl,page3_start
		ld	de,p3_start
		ld	bc,p3_end - p3_start
		ldir
		ret

; -----------------------------------------------------------------------------
; Flash write 8kb
; Output: cx = 0: success, 1: error
;
; Copies the contents of 0x2000-0x3FFF to the area appearing in 0x6000-0x7FFF.
;
; bank_back  address         routine    address(MSB8bit)
;   0        0x0000-0x1FFF   page2      0x00
;   1        0x2000-0x3FFF   page2      0x20
;   2        0x4000-0x5FFF   page2      0x40
;   3        0x6000-0x7FFF   page2      0x60
;   4        0x8000-0x9FFF   page3      0x80
;   5        0xA000-0xBFFF   page3      0xA0
;   6        0xC000-0xDFFF   page2      0xC0
;   7        0xE000-0xFFFF   page2      0xE0
;
; -----------------------------------------------------------------------------
flash_write_8kb:
		ld	a,(bank_back)
		; HL = A * 0x2000
		rrca
		rrca
		rrca
		ld	h,a
		ld	l,0

		cp	$80			; goto page3 when 0x80 or 0xA0, goto page2 when others
		jr	z,page3
		cp	$A0
		jr	z,page3
page2:		call	p2_flash_write_8kb
		ret
page3:		di
		exx
		; backup page3 to page1
		ld	hl,$C000
		ld	de,$4000
		ld	bc,$4000
		ldir
		call	transfer_to_page3
		; copy write data to page3
		ld	hl,$2000
		ld	de,$C000
		ld	bc,$2000
		ldir
		exx

		ld	(save_sp),sp
		ld	sp,$FFFF
		call	p3_flash_write_8kb
		ld	sp,(save_sp)
		; restore page 3
		ld	hl,$4000
		ld	de,$C000
		ld	bc,$3FFF		; exclude extended slot register
		ldir
		ei
		ret

; -----------------------------------------------------------------------------
; DATA
; -----------------------------------------------------------------------------
save_sp:	dw	0

; -----------------------------------------------------------------------------
; Programs to be placed on 0xA000 (PAGE2)
; -----------------------------------------------------------------------------
page2_start:
		PHASE	$A000
p2_start:

; -----------------------------------------------------------------------------
; p2 setup slot
; -----------------------------------------------------------------------------
p2_setup_slot:	; save target slot
		ld	a,(target_slot)
		; primary slot
		and	3
		ld	b,a
		rlca
		rlca
		rlca
		rlca
		or	b
		rlca
		rlca
		or	b
		ld	b,a
		in	a,(PPI_SLOT)
		ld	(p2_save_slot),a
		and	a,%00110000
		or	b
		out	(PPI_SLOT),a
		ret

; -----------------------------------------------------------------------------
; p2 restore slot
; restore primary slot
; -----------------------------------------------------------------------------
p2_restore_slot:
		ld	a,(p2_save_slot)
		out	(PPI_SLOT),a
		ret

; -----------------------------------------------------------------------------
; Get_flash ROM id
; Output: e = Manufacturer ID
;         d = Device ID
; -----------------------------------------------------------------------------
p2_get_id:	di
		; Initialize stack pointer
		ld	(p2_save_sp),sp
		ld	sp,$BFFF
		; Change slot
		call	p2_setup_slot
		; Get Manufacturer ID
		ld	hl,$0000
		ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$90
		ld	(CMD_5555),a
	IFDEF TEST
		ld	e,MID_SST
		ld	d,DID_SST39SF010A
	ELSE
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
	ENDIF

		ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$F0
		ld	(CMD_5555),a
		; Restore slot
		call	p2_restore_slot
		ld	(manufact_id),de
		; Restore stack pointer
		ld	sp,(p2_save_sp)
		ei
		ret

; -----------------------------------------------------------------------------
; p2 flash chip erase
; -----------------------------------------------------------------------------
flash_chip_erase:
		di
		; Initialize stack pointer
		ld	(p2_save_sp),sp
		ld	sp,$BFFF
		; Change slot
		call	p2_setup_slot
		; Erase chip
		ld	hl,$0000
		ld	a, $AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$80
		ld	(CMD_5555),a
		ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$10
		ld	(CMD_5555),a
		; Restore slot
		call	p2_restore_slot
		; Restore stack pointer
		ld	sp,(p2_save_sp)
		; Wait
		ld	hl, JIFFY
		ld	a,(hl)
		add	a,10
		ei
wait_l1:	cp	(hl)
		jr	nz,wait_l1
		ret

; -----------------------------------------------------------------------------
; p2 sector erase
; -----------------------------------------------------------------------------
p2_sector_erase:
		di
		; Initialize stack pointer
		ld	(p2_save_sp),sp
		ld	sp,$BFFF
		; Change slot
		call	p2_setup_slot

		ld	h,c		; Set H to the page start (e.g., $40, $80, etc.)
		ld	l,$00	        ; HL now points to the start of the 4KB sector

		; Erase sector
		ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$80
		ld	(CMD_5555),a
		ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$30		; sector erase command
		ld	(hl),a		; at sector address
p2_wait:	ld	a,(hl)		; read byte from the sector being erased
		xor	(hl)		; compare with another read
		and	$40		; Check toggle bit
		jr	nz,p2_wait	; if bits are still toggling, chip is busy

		; Restore slot
		call	p2_restore_slot
		; Restore stack pointer
		ld	sp,(p2_save_sp)
		ei
		ret

; -----------------------------------------------------------------------------
; p2_flash_write_8kb
; Copies the contents of 0x2000-0x3FFF to the area appearing in 0x6000-0x7FFF.
; Input:  hl = target address
; Output: cx = 0: success, 1: error
; -----------------------------------------------------------------------------
p2_flash_write_8kb:
		di
		; Initialize stack pointer
		ld	(p2_save_sp),sp
		ld	sp,$BFFF
		push	hl
		; Transfer write datas
		ld	hl,$2000
		ld	de,$8000
		ld	bc,$2000
		ldir
		; Change slot
		call	p2_setup_slot
		pop	hl

		ld	de,$8000				; source address
		ld	bc,$2000				; transfer bytes
loop2_of_bc:	ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$A0
		ld	(CMD_5555),a
		ld	a,(de)
		ld	(hl),a

		push	bc
		ld	bc,0					; timeout 65536 count
wait2_for_write_complete:
		nop
		nop
		cp	(hl)
		jr	z,write2_complete
		djnz	wait2_for_write_complete
		dec	c
		jr	nz, wait2_for_write_complete
write2_error:	pop	bc
		call	p2_restore_slot
		ld	sp,(p2_save_sp)
		ei
		scf
		ret

write2_complete:
		pop	bc
		inc	de
		inc	hl
		dec	bc
		ld	a,b
		or	c
		jr	nz,loop2_of_bc

		call	p2_restore_slot
		ld	sp,(p2_save_sp)
		ei
		or	a
		ret

p2_save_sp:	dw	0
p2_save_slot:	db	0

p2_end:
		DEPHASE

; -----------------------------------------------------------------------------
; Programs to be placed on 0xE000 (PAGE3)
; -----------------------------------------------------------------------------
page3_start:
		PHASE	$E000
p3_start:
; -----------------------------------------------------------------------------
; p3 setup slot
; -----------------------------------------------------------------------------
p3_setup_slot:	; save target slot
		ld	a,(target_slot)
		; primary slot
		and	3
		ld	b,a
		rlca
		rlca
		or	b
		rlca
		rlca
		or	b
		ld	b,a
		in	a,(PPI_SLOT)
		ld	(p3_save_slot),a
		and	%11000000
		or	b
		out	(PPI_SLOT),a
		ret

; -----------------------------------------------------------------------------
; p3 restore slot
; restore primary slot
; -----------------------------------------------------------------------------
p3_restore_slot:
		ld	a,(p3_save_slot)
		out	(PPI_SLOT),a
		ret

; -----------------------------------------------------------------------------
; p3 sector erase
; -----------------------------------------------------------------------------
p3_sector_erase:
		; Change slot
		call	p3_setup_slot

		ld	h,c		; Set H to the page start (e.g., $40, $80, etc.)
		ld	l,$00	        ; HL now points to the start of the 4KB sector

		; Sector erase
		ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$80
		ld	(CMD_5555),a
		ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$30		; sector erase command
		ld	(hl),a		; at sector address
p3_wait:	ld	a,(hl)		; read byte from the sector being erased
		xor	(hl)		; compare with another read
		and	$40		; Check toggle bit
		jr	nz,p3_wait	; if bits are still toggling, chip is busy

		jp	p3_restore_slot	; restore slot and return

; -----------------------------------------------------------------------------
; Write flash 8kb page3
; Input:; hl = target address
; output: cx = 0: success, 1: error
; -----------------------------------------------------------------------------
p3_flash_write_8kb:
		; Change slot
		call	p3_setup_slot

		ld	de,$C000		; source address
		ld	bc,$2000		; transfer bytes
loop3_of_bc:	ld	a,$AA
		ld	(CMD_5555),a
		ld	a,$55
		ld	(CMD_2AAA),a
		ld	a,$A0
		ld	(CMD_5555),a
		ld	a,(de)
		ld	(hl),a

		push	bc
		ld	bc,0			; timeout 65536 count

wait3_for_write_complete:
		nop
		nop
		cp	(hl)
		jr	z,write3_complete
		djnz	wait3_for_write_complete
		dec	c
		jr	nz,wait3_for_write_complete
write3_error:	pop	bc
		call	p3_restore_slot
		scf
		ret
write3_complete:
		pop	bc
		inc	de
		inc	hl
		dec	bc
		ld	a,b
		or	c
		jr	nz,loop3_of_bc

		call	p3_restore_slot
		or	a
		ret

p3_save_slot:	db	0

p3_end:
		DEPHASE
