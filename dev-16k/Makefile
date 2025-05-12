# Make z80 binary from asm source with z88dk tools
# 16K MSX-DOS 2 Development Edition
# Todo: create build rules

# system agnostic commands
ifdef ComSpec
	RMF	:= del /f /q
	RMD	:=
	SEARCH	:= find
	CP	:= copy /b
	GETDATE	:= powershell get-date -format "{yyyyMMdd}" 
	QUOTE	:= "
	/	:= $(strip \)
else
	RMF	:= rm -f 
	RMD	:= /*
	SEARCH	:= grep
	CP	:= cp
	GETDATE	:= date +'%Y%m%d'
	QUOTE	:= '"'
	/	:= /
endif 

# Note: these must be existing folders
DEV32K	:= ..$/dev-32k

all:	ppide3

rdate:
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc

ppide3:	rdate
	@echo Assembling BEER DOS2 / 16k ppide rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DROM16K -DPPIDE -Oobj -o=beer_161.bin $(DEV32K)/p1_main.asm $(DEV32K)/p3_paging.asm $(DEV32K)/driver.asm $(DEV32K)/drv_ide.asm 
	z80asm -b -d -l -m -DROM16K -DPPIDE -Oobj -o=beer_162.bin $(DEV32K)/p0_kernel.asm
	z88dk-appmake +glue -b obj/beer_161 --filler 0xFF --clean
	z88dk-appmake +glue -b obj/beer_162 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/beer_161__.bin -o beer_16k.rom -s 16384 --org 0
	z88dk-appmake +rom -b obj/beer_162__.bin -o dos2_16k.img -s 16384 --org 0
	@echo done

clean:
	$(RMF) obj$(RMD)
	$(RMF) rdate.inc

