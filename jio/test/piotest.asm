; ------------------------------------------------------------------------------
; test parallel port if it is jio compatible
; ------------------------------------------------------------------------------

		ORG	$0100

main:		ld	de,t_header
		call	PrintText

		ld	hl,test_data
		ld	b,8
		ld	c,$90
do_loop:	push	bc
		push	hl
		di
		out	(c),0			; clear data bus
		in	d,(c)
		ld	a,(hl)			; read random data
		in	e,(c)
		ld	(hl),a
		in	a,(c)
		ei
		push	de
		call	PrintHexByte
		ld	a,' '
		call	PrintC
		pop	de
		ld	a,e
		push	de
		call	PrintHexByte
		ld	a,' '
		call	PrintC
		pop	de
		ld	a,d
		call	PrintHexByte
		call	PrintCRLF
		pop	hl
		pop	bc
		inc	hl
		djnz	do_loop
		rst	$00

t_header:	db	"JIO LPT I/O PORT: ",13,10,"$"
test_data:	db	$11,$22,$33,$44,$55,$66,$77,$88

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

shex:		ds	6
		
