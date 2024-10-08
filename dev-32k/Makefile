# Make z80 binary from asm source with z88dk tools
# 32K MSX 2.2 Development Edition


SOURCES1 := p1_main.asm ../mod-beer20/driver-beer20.asm p3_paging.asm p0_kernel.asm
SOURCES2 := p1_main.asm driver-beer232.asm p3_paging.asm p0_kernel.asm

# system agnostic commands
ifdef ComSpec
	RMF	:= del /f /q
	RMD	:=
	SEARCH	:= find
else
	RMF	:= rm -f 
	RMD	:= /*
	SEARCH	:= grep
endif 


all:	rom beer232 test

rom:	
	@echo Assembling reference rom..
	$(RMF) obj$(RMD)
	$(RMF) mod-beer20$(RMD)
	z80asm -b -d -l -m -DROM -Oobj -o=msx_d2si.bin $(SOURCES) $(SOURCES1)
	z88dk-appmake +glue -b obj/msx_d2si --filler 0xFF --clean
	z88dk-appmake +rom -b obj/msx_d2si__.bin -o msx_d2si.rom -s 32768 --org 0
	@echo done

beer232:	
	@echo Assembling beer232 rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DBEER232 -Oobj -o=msx_d2xi.bin $(SOURCES2)
	z88dk-appmake +glue -b obj/msx_d2xi --filler 0xFF --clean
	z88dk-appmake +rom -b obj/msx_d2xi__.bin -o msx_d2xi.rom -s 32768 --org 0
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
	

