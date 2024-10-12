# Make z80 binary from asm source with z88dk tools
# 32K MSX 2.2 Development Edition


SOURCES1 := p1_main.asm p3_paging.asm ../mod-beer20/driver-beer20.asm p0_kernel.asm
SOURCES2 := p1_main.asm p3_paging.asm driver-ide.asm p0_kernel.asm

# system agnostic commands
ifdef ComSpec
	RMF	:= del /f /q
	RMD	:=
	SEARCH	:= find
	GETDATE	:= powershell get-date -format "{yyyyMMdd}"
	QUOTE	:= "
else
	RMF	:= rm -f 
	RMD	:= /*
	SEARCH	:= grep
	GETDATE	:= date +'%Y%m%d'
	QUOTE	:= '"'
endif 

all:	rom ppide cfide test

rom:	
	@echo Assembling reference rom..
	$(RMF) obj$(RMD)
	$(RMF) mod-beer20$(RMD)
	z80asm -b -d -l -m -DROM -Oobj -o=msx_d2si.bin $(SOURCES1)
	z88dk-appmake +glue -b obj/msx_d2si --filler 0xFF --clean
	z88dk-appmake +rom -b obj/msx_d2si__.bin -o msx_d2si.rom -s 32768 --org 0
	@echo done

ppide:	
	@echo Assembling BEER ppide rom..
	$(RMF) obj$(RMD)
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc
	z80asm -b -d -l -m -DPPIDE -Oobj -o=beer.bin $(SOURCES2)
	z88dk-appmake +glue -b obj/beer --filler 0xFF --clean
	z88dk-appmake +rom -b obj/beer__.bin -o msx_d2xi_beer.rom -s 32768 --org 0
	$(RMF) rdate.inc
	@echo done

cfide:	
	@echo Assembling SODA cfide rom..
	$(RMF) obj$(RMD)
	@echo db $(QUOTE)$(shell $(GETDATE))$(QUOTE) > rdate.inc
	z80asm -b -d -l -m -DCFIDE -Oobj -o=soda.bin $(SOURCES2)
	z88dk-appmake +glue -b obj/soda --filler 0xFF --clean
	z88dk-appmake +rom -b obj/soda__.bin -o msx_d2xi_soda.rom -s 32768 --org 0
	$(RMF) rdate.inc
	@echo done

test:	
	@echo Assembling test rom..
	z80asm -b -d -l -m -DTEST -Oobjtest -o=msx_d2ti.bin $(SOURCES1)
	z88dk-appmake +glue -b objtest/msx_d2ti --filler 0xFF --clean
	z88dk-appmake +rom -b objtest/msx_d2ti__.bin -o msx_d2ti.rom -s 32768 --org 0
	$(SEARCH) "DOS_" objtest/msx_d2ti.map > TEST_entry_points.txt 
	@echo done

clean:
	$(RMF) obj$(RMD)
	$(RMF) objtest$(RMD)
	$(RMF) mod-beer20$(RMD)

