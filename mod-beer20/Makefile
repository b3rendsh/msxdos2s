# Make z80 binary from asm source with z88dk tools
# 16K MSX BEER 2.0 disk system and driver
#

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


all:	oem beer20

oem:	
	@echo Assembling oem rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DOEM -Oobj -o=OEM disk.asm driver-stub.asm
	z88dk-appmake +glue -b obj/OEM --filler 0x00 --clean
	z88dk-appmake +rom -b obj/OEM__.bin -o OEM_DISK.ROM -s 16384 --org 0 --filler 0x00
	$(SEARCH) "DOS_" obj/OEM.map > OEM_entry_points.txt
	@echo done

beer20:
	@echo Assembling beer20 rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DBEER20 -Oobj -o=BEER20 disk.asm disk-beer20.asm driver-beer20.asm
	z88dk-appmake +glue -b obj/BEER20 --filler 0x00 --clean
	z88dk-appmake +rom -b obj/BEER20__.bin -o BEER20_DISK.ROM -s 16384 --org 0 --filler 0x00
	$(SEARCH) "DOS_" obj/BEER20.map > BEER20_entry_points.txt
	@echo done

clean:
	$(RMF) obj$(RMD)
	

