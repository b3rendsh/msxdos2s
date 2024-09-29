; DISK-BEER20.ASM
;
; Hard Disk ROM for MSX computer
; Disk extensions for IDE card BEER-202
;
; Copyright SOLiD (2005) for most parts of the disk ide patch code.
; ------------------------------------------------------------------------------
; H_J. Berends:
; Based on driver sources for SOLiD BEER IDE v1.8 and disassembled v1.9 rom.
; The goal is to make disk ide patches for the beer-202 interface that are
; compliant with MSX DOS 1 and 2 disk interface standards and guidelines.


; beer-ide system variables:
; 0FD09 FAT pointer
; 0FD0D sector number (bit 0..15)
; 0FD0F sector number (bit 16..23)
; 0FFD9 Current drive control byte
; 0FFDA FAT block# in buffer
; 0FFDB	Common buffer indicator


		SECTION	DISK_BEER20

		; Symbols defined by the disk ide module
		PUBLIC	NewGetFAT
		PUBLIC	NewPutFAT
		PUBLIC	NewUpdateFAT
		PUBLIC	FAT_read
		PUBLIC	FAT_write
		PUBLIC	A752B
		PUBLIC	A7570
		PUBLIC	A7584
		PUBLIC	A75BE
		PUBLIC	A75E3
		PUBLIC	A75F2

		; Disk routines used by the disk ide disk module
		EXTERN	GETFAT
		EXTERN	GetFATbuf
		EXTERN	DSK_abs_read
		EXTERN	DSK_abs_write
		EXTERN	OldGetFAT
		EXTERN	PutFAT
		EXTERN	ReadSector_all
		EXTERN	GetDPBptr
		EXTERN	DPB_change_entry
		EXTERN	OldPutFAT

; ------------------------------------------------------------------------------
; Multi-FAT swapper                                          
; ------------------------------------------------------------------------------
FAT_read: 
	ld 	a,h
  	or 	l
  	jp 	nz,GETFAT
  	ld 	hl,(0fd09h)	; see FAT_write
  	ret

NewGetFAT:
	call	0f252h
	call	FAT_Swapper
	ld	a,(ix+00fh)
	cp	010h
	jp	nc,A759D
	jp	OldGetFAT

FAT_write:
	ld	a,h
	or	l
	jr 	nz,NewPutFAT
	ld 	(0fd09h),bc	; see FAT_read
	ret

NewPutFAT:
	call	FAT_Swapper
	dec	de
	ld	a,001h
	ld	(de),a
	inc	de
	jp	PutFAT

FAT_Swapper:
	push	hl
	ld	hl,(0ffdbh)	;Is this FAT in common buffer
	ld	e,(ix+013h)
	ld	d,(ix+014h)
	or	a
	sbc	hl,de
	pop	hl
	ret	nz		;No -> do no swapping
	ld	a,(0ffd9h)	;Is current's drive FAT buffered?
	cp 	(ix+00h)
	jr	z,r569
	call	SWAFAT		;if no -> swap immediately
	jr	r570

r569:	call	A75A5
	push	hl
	ld	hl,0ffdah
	cp 	(hl)
	pop	hl		;compare to block # in buffer
	call	nz,SWAFAT 	;If no match -> swap FATS
r570:	ld	a,(ix+00fh)
	cp	010h
	ld	a,h
	jr	nc,A746F
	and 	003h
	ld	h,a		;mask FAT entry addres to block size
	ret

A746F:	sub	003h
	jr	nc,A746F
	add	a,003h
	ld	h,a
	ret

; ------------------------------------------
; Swapping FAT buffers
; ------------------------------------------
SWAFAT:	push	hl
	call	SaveFATbuf
	pop	hl
ReadFATbuf:
	ld	a,(ix+00h)
	ld	(0ffd9h),a	;FAT's drive
	call	A75A5
	ld	(0ffdah),a	;FAT's block no
	push	hl
	push	de
	push	bc
	call	GetFATbuf
	ld	b,a
	ld	a,(0FFDAh)
	ld	c,a
	add	a,a
	add	a,c
	add	a,e
	ld	e,a
	jr	nc,r571
	inc	d
r571:	push	hl
	push	de
	push	bc
	ld	b,3
	call	DiskReadSect
	pop	bc
	pop	de
	pop	hl
	jr	nc,r572
	ld	a,(ix+10h)
	add	a,e
	ld	e,a
	jr	nc,r573
	inc	d
r573:	djnz	r571
	call	GetFATbuf
	ld	b,3
	call	DSK_abs_read
r572:	pop	bc
	pop	de
	pop	hl
	ret

DiskReadSect:
	call	0F270h
	ld	a,(ix+00h)
	ld	c,(ix+01h)
	jp	ReadSector_all

NewUpdateFAT:
	push	de
	ld	e,(ix+13h)
	ld	d,(ix+14h)
	ld	hl,(0FFDBh)
	or	a
	sbc	hl,de
	pop	de
	jp	nz,r574
SaveFATbuf:
	push	bc
	push	de
	push	ix
	ld	a,(0FFD9h)
	call	GetDPBptr
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	push	bc
	pop	ix
	call	GetFATbuf
	ld	b,a
	dec	hl
	ld	a,(hl)
	ld	(hl),0
	inc	hl
	cp	1
	jp	nz,r575
	ld	a,(0FFDAh)
	ld	c,a
	add	a,a
	add	a,c
	add	a,e
	ld	e,a
	jr	nc,r576
	inc	d
r576:	push	de
	push	bc
	push	hl
	ld	a,(0FFDAh)
	ld	b,a
	add	a,a
	add	a,b
	ld	b,a
	ld	a,(ix+10h)
	sub	b
	jr	z,r575
	cp	3
	ld	b,a
	jr	c,r577
	ld	b,3
r577:	ld	a,(ix+00h)
	call	DSK_abs_write
	pop	hl
	pop	bc
	pop	de
	ld	a,e
	add	a,(ix+10h)
	ld	e,a
	jr	nc,r578
	inc	d
r578:	djnz	r576
r575:	pop	ix
	pop	de
	pop	bc
	ret

r574:	call	GetFATbuf
r579:	push	af
	push	ix
	push	hl
	push	de
	push	bc
	ld	a,(ix+00h)
	ld	c,(ix+01h)
	call	DSK_abs_write
	pop	bc
	pop	de
	ld	a,e
	add	a,b
	ld	e,a
	ld	a,0
	adc	a,d
	ld	d,a
	pop	hl
	pop	ix
	pop	af
	dec	a
	jr	nz,r579
	ret

; ------------------------------------------
A752B:	ld	a,(0FFD9h)
	cp	(ix)
	call	nz,SaveFATbuf
	ld	a,(0F2E1h)
	call	ReadFATbuf
	ld	l,(ix+13h)
	ld	h,(ix+14h)
	dec	hl
	ld	(hl),0
	jp	DPB_change_entry

; ------------------------------------------
; FAT patches

A7570:	ld	a,(ix+00fh)
	cp	010h
	jr	nc,A757F
	ld	a,h
	cp	00fh
A757A:	ret	c
	ld	a,l
	cp	0f7h
	ret     
A757F:	ld	a,h
	cp	0ffh
	jr	A757A

; ------------------------------------------
A7584:	push	de
	ld	e,l
	ld	d,h
	ld	a,(ix+00fh)
	cp	010h
	jp	c,A7596
	add	hl,de
	pop	de
	add	hl,de
	ld	(hl),c
	inc	hl
	ld	(hl),b
	ret
A7596:	ld	a,b
	and	00fh
	ld	b,a
	jp	OldPutFAT

A759D:	add	hl,hl
	add	hl,de
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
	or	h
	ret

; ------------------------------------------
A75A5:	ld	a,(ix+00fh)
	cp	010h
	jr	nc,A75B2
	ld	a,h
	rra
	rra
	and	003h
	ret
A75B2:	ld	a,h
	push	bc
	ld	c,0ffh
A75B6:	inc	c
	sub	003h
	jr	nc,A75B6
	ld	a,c
	pop	bc
	ret

; ------------------------------------------
A75BE:	ld	c,000h
	dec	b
	jr	z,A75C8
A75C3:	add	hl,hl
	rl	c
	djnz	A75C3
A75C8:	or	l
	ld	l,a
	ld	a,c
	ld	c,(ix+00ch)
	ld	b,(ix+00dh)
	add	hl,bc
	adc	a,000h
	pop	bc
	ret	z
	ld	(0fd0dh),hl	; store 24-bit sector number bit 0..15
	ld	l,a
	ld	h,000h
	ld	(0fd0fh),hl	; store 24-bit sector number bit 16..23
	ld	hl,0ffffh	; indicator to use 24-bit sector number in DSKIO routine
	ret

; ------------------------------------------
A75E3:	ld	a,e
	and	d
	inc	a
	jr	z,A75EF
	ld	hl,(0f23fh)	; BUFSEC  - sectornumber in data buffer
	or	a
	sbc	hl,de
	ret

A75EF:	inc	a
	scf
	ret

; ------------------------------------------
A75F2:	jr	nc,A75F8
	ld	a,d
	and	e
	inc	a
	ret	nz
A75F8:	ld	a,0ffh
	ld	(0f241h),a	; BUFDRN - driveid of sector in directory buffer
	ret
