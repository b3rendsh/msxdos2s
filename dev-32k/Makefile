# Make z80 binary from asm source with z88dk tools
# 16K MSX-DOS 1 and 32K MSX-DOS 2 Development Edition

SOURCES1  := dos1x.asm
SOURCES2A := p1_main.asm p3_paging.asm
SOURCES2B := p0_kernel.asm
DRV_IDE   := driver.asm drv_ide.asm
DRV_JIO   := drv_jio.asm

MAKEFLAGS += --no-print-directory

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

all:	beer1 soda1 malt1 jio1 beer2 soda2 malt2 core2 jio2 pio2 uart2 beer2s taste img

rdate:
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc

# build logic for 16K DOS1 ROM
dos1:	rdate
	@echo Assembling 16K MSX-DOS 1 $(INTERFACE) ROM...
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DIDEDOS1 $(DEFINES) -Oobj -o=dos1 $(SOURCES1) $(DRIVER)
	z88dk-appmake +glue -b obj/dos1 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/dos1__.bin -o $(ROM)/$(PREFIX)_dos1.rom -s 16384 --org 0
	@echo done

beer1:
	@$(MAKE) dos1 INTERFACE="BEER IDE" DEFINES=-DBEER PREFIX=beer DRIVER="$(DRV_IDE)"

soda1:
	@$(MAKE) dos1 INTERFACE="SODA IDE" DEFINES=-DSODA PREFIX=soda DRIVER="$(DRV_IDE)"

malt1:
	@$(MAKE) dos1 INTERFACE="MALT IDE" DEFINES=-DMALT PREFIX=malt DRIVER="$(DRV_IDE)"

jio1:
	@$(MAKE) dos1 INTERFACE="JIO" DEFINES=-DJIO PREFIX=jio DRIVER="$(DRV_JIO)"

# build logic for 32K DOS2 ROM
dos2:	rdate
	@echo Assembling 32K MSX-DOS 2 $(INTERFACE) ROM...
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m $(DEFINES) -Oobj -o=dos2 $(SOURCES2A) $(DRIVER) $(SOURCES2B)
	z88dk-appmake +glue -b obj/dos2 --filler 0xFF --clean
	z88dk-appmake +rom -b obj/dos2__.bin -o $(ROM)/$(PREFIX)_dos2.rom -s 32768 --org 0
	@echo done

beer2:
	@$(MAKE) dos2 INTERFACE="BEER IDE" DEFINES=-DBEER PREFIX=beer DRIVER="$(DRV_IDE)"

soda2:
	@$(MAKE) dos2 INTERFACE="SODA IDE" DEFINES=-DSODA PREFIX=soda DRIVER="$(DRV_IDE)"

malt2:
	@$(MAKE) dos2 INTERFACE="MALT IDE" DEFINES=-DMALT PREFIX=malt DRIVER="$(DRV_IDE)"

core2:
	@$(MAKE) dos2 INTEFACE="CORE" DEFINES=-DCORE PREFIX=core DRIVER="$(DRV_IDE)"

jio2:
	@$(MAKE) dos2 INTEFACE="JIO" DEFINES=-DJIO PREFIX=jio DRIVER="$(DRV_JIO)"

pio2:
	@$(MAKE) dos2 INTEFACE="JIO LPT" DEFINES="-DJIO -DLPTIO" PREFIX=pio DRIVER="$(DRV_JIO)"

uart2:
	@$(MAKE) dos2 INTEFACE="JIO UART" DEFINES="-DJIO -DUART" PREFIX=uart DRIVER="$(DRV_JIO)"

beer2s:	rdate
	@echo Assembling 32K BEER IDE image with swapped pages..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DBEER -Oobj -o=beer_161.bin $(SOURCES2A) $(DRV_IDE)
	z80asm -b -d -l -m -DBEER -Oobj -o=beer_162.bin $(SOURCES2B)
	z88dk-appmake +glue -b obj/beer_161 --filler 0xFF --clean
	z88dk-appmake +glue -b obj/beer_162 --filler 0xFF --clean
	z80asm -b -d -l -m -Oobj -o=dummy.bin dummy.asm
	z88dk-appmake +rom -b obj/dummy.bin -o $(ROM)/beer_32k.img -s 32768 --org 0
	z88dk-appmake +inject -b $(ROM)/beer_32k.img -i obj/beer_162__.bin -s 00000 -o $(ROM)/beer_32k.img 
	z88dk-appmake +inject -b $(ROM)/beer_32k.img -i obj/beer_161__.bin -s 16384 -o $(ROM)/beer_32k.img
	@echo done

taste:
	@echo Assembling TASTE program..
	z80asm -b -m -Oobj -o=taste.com taste.asm
	$(CP) obj$/taste.com $(ROM)$/TASTE.COM
	z80asm -DBASBIN -b -m -l -Oobj -o=taste.bin taste.asm
	$(CP) obj$/taste.bin $(ROM)$/TASTE.BIN
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
	z80asm -b -d -l -m -DTEST -Oobjtest -o=test1.bin $(SOURCES1) $(DRV_IDE)
	z88dk-appmake +glue -b objtest/test1 --filler 0xFF --clean
	z88dk-appmake +rom -b objtest/test1__.bin -o $(ROM)/test1.rom -s 16384 --org 0
	$(SEARCH) "DOS_" objtest/test1.map > test1_entry_points.txt
	@echo done

test2:	rdate
	@echo Assembling test2 rom..
	z80asm -b -d -l -m -DTEST -Oobjtest -o=test2.bin $(SOURCES2A) $(DRV_IDE) $(SOURCES2B)
	z88dk-appmake +glue -b objtest/test2 --filler 0xFF --clean
	z88dk-appmake +rom -b objtest/test2__.bin -o $(ROM)/test2.rom -s 32768 --org 0
	$(SEARCH) "DOS_" objtest/test2.map > test2_entry_points.txt
	@echo done

clean:
	$(RMF) obj$(RMD)
	$(RMF) objtest$(RMD)
	$(RMF) rdate.inc

