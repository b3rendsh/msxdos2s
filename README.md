# msxdos2s
MSX DOS 2 sources:  
- Copy of MSX DOS 2.2 sources from sourcefourge (subset)  
- Converted sources to assemble with z88dk's z80asm  
- Modified sources to create a MSX DOS 2.2 32K ROM  

## Information
This repository contains modified MSX DOS 2.2 sources. 
The purpose is to create a 32K ROM that can run from a standard MSX cartridge.  
Kanji support and dos2memchk ("CALL DOS2MEMCHK" in BASIC) are removed to reduce size.  
There is no OEM disk driver included, only the dummy driver from ASCII.  
    
The original 64K ROM came on a cartridge with a ROM mapper. To use that version you need a small logic circuit e.g. a 74LS138 and 74LS74 in addition to a 64K EPROM. With the 32K version this circuit is no longer needed.  
  
Putting the ROM inside an MSX-2 machine can be relatively easy, depending on the machine and slot layout. Note that the internal DOS 1 DISKROM should remain in place because it contains the OEM disk driver. 
    
The source files are provided and modified for study only.  

## References
MSX system sources:  
https://sourceforge.net/projects/msxsyssrc/  
  
MSX DOS 2:  
https://www.msx.org/wiki/MSX-DOS_2  


