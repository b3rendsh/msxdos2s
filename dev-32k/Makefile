# Make z80 binary from asm source with z88dk tools
# 16K MSX-DOS 1 and 32K MSX-DOS 2 Development Edition
# Universal disk driver
# Todo: create build rules

SOURCES1 := dos1x.asm driver.asm drv_ide.asm 
SOURCES2 := p1_main.asm p3_paging.asm driver.asm drv_ide.asm p0_kernel.asm

# system agnostic commands
ifdef ComSpec
	RMF	:= del /f /q
	RMD	:=
	SEARCH	:= find
	CP	:= copy /b
	GETDATE	:= powershell get-date -format "{yyyy-MM-dd}" 
	QUOTE	:= "
	/	:= $(strip \)
else
	RMF	:= rm -f 
	RMD	:= /*
	SEARCH	:= grep
	CP	:= cp
	GETDATE	:= date +'%Y-%m-%d'
	QUOTE	:= '"'
	/	:= /
endif 

# Note: the rom folder must be an existing folder
ROM 	:= .$/rom

all:	ppide1 cfide1 ppide2 cfide2 core2 jio1 jio2 taste

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

core2:	rdate
	@echo Assembling CORE DOS2 / 32k rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DCORE -Oobj -o=core_dos2.bin $(SOURCES2)
	z88dk-appmake +glue -b obj/core_dos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/core_dos2__.bin -o $(ROM)/core_dos2.rom -s 32768 --org 0
	@echo done

jio1:	rdate
	@echo Assembling JIO DOS1 / 16k jio remote disk rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DIDEDOS1 -DJIO -Oobj -o=jio_dos1.bin dos1x.asm drv_jio.asm
	z88dk-appmake +glue -b obj/jio_dos1 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/jio_dos1__.bin -o $(ROM)/jio_dos1.rom -s 16384 --org 0
	@echo done

jio2:	rdate
	@echo Assembling JIO DOS2 / 32k jio remote disk rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DJIO -Oobj -o=jio_dos2.bin p1_main.asm p3_paging.asm drv_jio.asm p0_kernel.asm
	z88dk-appmake +glue -b obj/jio_dos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/jio_dos2__.bin -o $(ROM)/jio_dos2.rom -s 32768 --org 0
	@echo done

uart2:	rdate
	@echo Assembling UART DOS2 / 32k uart 1655x remote disk rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DUART -Oobj -o=uart_dos2.bin p1_main.asm p3_paging.asm driver.asm drv_uart.asm p0_kernel.asm
	z88dk-appmake +glue -b obj/uart_dos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/uart_dos2__.bin -o $(ROM)/uart_dos2.rom -s 32768 --org 0
	@echo done

taste:
	@echo Assembling taste program..
	z80asm -b -m -Oobj -o=taste.com taste.asm
	$(CP) obj$/taste.com $(ROM)$/TASTE.COM
	@echo done

img:	
	@echo Creating BEER, SODA and JIO EEPROM images..
	z80asm -b -d -l -m -Oobj -o=dummy.bin dummy.asm
	z88dk-appmake +rom -b obj/dummy.bin -o $(ROM)/dummy_16k.rom -s 16384 --org 0
	z88dk-appmake +rom -b obj/dummy.bin -o $(ROM)/beer_64k.img -s 65536 --org 0
	z88dk-appmake +inject -b $(ROM)/beer_64k.img -i $(ROM)/beer_dos2.rom -s 00000 -o $(ROM)/beer_64k.img 
	z88dk-appmake +inject -b $(ROM)/beer_64k.img -i $(ROM)/beer_dos1.rom -s 32768 -o $(ROM)/beer_64k.img
	z88dk-appmake +rom -b obj/dummy.bin -o $(ROM)/soda_64k.img -s 65536 --org 0
	z88dk-appmake +inject -b $(ROM)/soda_64k.img -i $(ROM)/soda_dos2.rom -s 00000 -o $(ROM)/soda_64k.img 
	z88dk-appmake +inject -b $(ROM)/soda_64k.img -i $(ROM)/soda_dos1.rom -s 32768 -o $(ROM)/soda_64k.img
	z88dk-appmake +rom -b obj/dummy.bin -o $(ROM)/jio_64k.img -s 65536 --org 0
	z88dk-appmake +inject -b $(ROM)/jio_64k.img -i $(ROM)/jio_dos2.rom -s 00000 -o $(ROM)/jio_64k.img 
	z88dk-appmake +inject -b $(ROM)/jio_64k.img -i $(ROM)/jio_dos1.rom -s 32768 -o $(ROM)/jio_64k.img

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

