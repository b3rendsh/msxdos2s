# Make z80 binary from asm source with z88dk tools

all:	taste

taste:
	@echo Assembling TASTE program..
	z80asm -DBASBIN -b -m -l -o=TASTE.BIN taste.asm
	z80asm -b -m -l -o=TASTE.COM taste.asm
	@echo done
