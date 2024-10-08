# Make z80 binary from asm source with z88dk tools
# 16K MSX BEER 2.0 disk system and driver
#

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


all:	oem beer20 beer232

oem:	
	@echo Assembling oem rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DOEM -Oobj -o=OEM disk.asm driver-stub.asm
	z88dk-appmake +glue -b obj/OEM --filler 0x00 --clean
	z88dk-appmake +rom -b obj/OEM__.bin -o OEM_DISK.ROM -s 16384 --org 0 --filler 0x00
	$(SEARCH) "DOS_" obj/OEM.MAP > OEM_entry_points.txt
	@echo done

beer20:
	@echo Assembling beer20 rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DBEER20 -Oobj -o=BEER20 disk.asm disk-beer20.asm driver-beer20.asm
	z88dk-appmake +glue -b obj/BEER20 --filler 0x00 --clean
	z88dk-appmake +rom -b obj/BEER20__.bin -o BEER20_DISK.ROM -s 16384 --org 0 --filler 0x00
	$(SEARCH) "DOS_" obj/BEER20.MAP > BEER20_entry_points.txt
	@echo done

beer232:
	@echo Assembling 16k beer232 rom..
	$(RMF) obj$(RMD)
	z80asm -b -d -l -m -DBEER20 -Oobj -o=BEER232 disk.asm disk-beer20.asm ../dev-32k/driver-beer232.asm
	z88dk-appmake +glue -b obj/BEER232 --filler 0x00 --clean
	z88dk-appmake +rom -b obj/BEER232__.bin -o BEER232_DOS1.ROM -s 16384 --org 0 --filler 0x00
	$(SEARCH) "DOS_" obj/BEER232.MAP > BEER232_entry_points.txt
	@echo done

clean:
	$(RMF) obj$(RMD)

	

