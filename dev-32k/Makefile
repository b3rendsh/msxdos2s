# Make z80 binary from asm source with z88dk tools
# 16K MSX-DOS 1 and 32K MSX-DOS 2 Development Edition
# Universal IDE disk driver

SOURCES1 := dos1x.asm driver-ide.asm
SOURCES2 := p1_main.asm p3_paging.asm driver-ide.asm p0_kernel.asm

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

# Note: the rom folder must be an existing folder
ROM 	:= .$/rom

all:	ppide1 cfide1 ppide2 cfide2 taste

rdate:
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc

ppide1:	rdate
	@echo Assembling BEER DOS1 / 16k ppide rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DIDEDOS1 -DPPIDE -Oobj -o=beer_dos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/beer_dos1 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/beer_dos1__.bin -o $(ROM)/beer_dos1.rom -s 16384 --org 0
	@echo done

cfide1: rdate
	@echo Assembling SODA DOS1 / 16k cfide rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DIDEDOS1 -DCFIDE -Oobj -o=soda_dos1 $(SOURCES1)
	z88dk-appmake +glue -b obj/soda_dos1 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/soda_dos1__.bin -o $(ROM)/soda_dos1.rom -s 16384 --org 0
	@echo done

ppide2:	rdate
	@echo Assembling BEER DOS2 / 32k ppide rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DPPIDE -Oobj -o=beer_dos2.bin $(SOURCES2)
	z88dk-appmake +glue -b obj/beer_dos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/beer_dos2__.bin -o $(ROM)/beer_dos2.rom -s 32768 --org 0
	@echo done

cfide2:	rdate
	@echo Assembling SODA DOS2 / 32k cfide rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DCFIDE -Oobj -o=soda_dos2.bin $(SOURCES2)
	z88dk-appmake +glue -b obj/soda_dos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/soda_dos2__.bin -o $(ROM)/soda_dos2.rom -s 32768 --org 0
	@echo done

taste:
	@echo Assembling taste program..
	z80asm -b -m -Oobj -o=taste.com taste.asm
	$(CP) obj$/taste.com $(ROM)$/TASTE.COM
	@echo done


img:	
	@echo Creating BEER and SODA EEPROM images..
	z80asm -b -d -l -m -Oobj -o=dummy.bin dummy.asm
	z88dk-appmake +rom -b obj/dummy.bin -o $(ROM)/dummy_16k.rom -s 16384 --org 0
	z88dk-appmake +rom -b obj/dummy.bin -o $(ROM)/beer_64k.img -s 65536 --org 0
	z88dk-appmake +inject -b $(ROM)/beer_64k.img -i $(ROM)/beer_dos2.rom -s 00000 -o $(ROM)/beer_64k.img 
	z88dk-appmake +inject -b $(ROM)/beer_64k.img -i $(ROM)/beer_dos1.rom -s 32768 -o $(ROM)/beer_64k.img
	z88dk-appmake +rom -b obj/dummy.bin -o $(ROM)/soda_64k.img -s 65536 --org 0
	z88dk-appmake +inject -b $(ROM)/soda_64k.img -i $(ROM)/soda_dos2.rom -s 00000 -o $(ROM)/soda_64k.img 
	z88dk-appmake +inject -b $(ROM)/soda_64k.img -i $(ROM)/soda_dos1.rom -s 32768 -o $(ROM)/soda_64k.img
	@echo done

test1:	rdate
	@echo Assembling test1 rom..
	z80asm -b -d -l -m -DTEST -Oobjtest -o=test1.bin $(SOURCES1)
	z88dk-appmake +glue -b objtest/test1 --filler 0xFF --clean
	z88dk-appmake +rom -b objtest/test1__.bin -o $(ROM)/test1.rom -s 16384 --org 0
	$(SEARCH) "DOS_" objtest/test1.map > test1_entry_points.txt
	@echo done

test2:	rdate
	@echo Assembling test2 rom..
	z80asm -b -d -l -m -DTEST -Oobjtest -o=test2.bin $(SOURCES2)
	z88dk-appmake +glue -b objtest/test2 --filler 0xFF --clean
	z88dk-appmake +rom -b objtest/test2__.bin -o $(ROM)/test2.rom -s 32768 --org 0
	$(SEARCH) "DOS_" objtest/test2.map > test2_entry_points.txt
	@echo done

clean:
	$(RMF) obj$(RMD)
	$(RMF) objtest$(RMD)
	$(RMF) rdate.inc

