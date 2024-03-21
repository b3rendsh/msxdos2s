# msxdos2s
MSX DOS 2 sources:  
- Copy of MSX DOS 2.2 sources from sourceforge (subset)  
- Converted sources to assemble with z88dk's z80asm  
- Modified sources to create a MSX DOS 2.2 32K ROM  
- Optional FAT16 kernel patch  
  
BEER IDE sources:  
- Driver version 2.0, compatible with DOS 2.2 and other disk systems  
- DOS 1.03 disk system with integrated BEER 2.0 driver and disk kernel customization  
  
Development:  
- Restructured MSX DOS 2.2 disk system with integrated BEER 2.0 driver  
- Optional FAT16 kernel patch where the ramdisk function is preserved  

## Information
This repository contains modified MSX DOS 2.2 sources. The purpose is to create a 32K ROM that can run from a standard MSX cartridge or can be used as a disk ROM for the BEER IDE interface.  
  
Kanji support and dos2memchk ("CALL DOS2MEMCHK" in BASIC) are removed to reduce size. The standard version includes the dummy disk driver from ASCII. 
  
The development version includes the BEER 2.0 driver. To use it with existing disks make sure that the boot partition contains the msxdos2.sys and command2.com version 2.20 or compatible. The system won't work reliably with a DOS 1 based shell or other alternative, the same limitations as the original DOS 2.2 apply. Without boot disk the system will start disk basic.  
  
The original 64K ROM came on a cartridge with a ROM mapper. To use that version you need a small logic circuit e.g. a 74LS138 and 74LS74 in addition to a 64K EPROM. With the 32K version this circuit is no longer needed.  
  
Putting the ROM inside an MSX-2 machine can be relatively easy, depending on the machine and slot layout. Note that any internal DOS 1 disk ROM should remain in place because it contains the OEM disk driver. 
    
## Limitations
The source files are provided and modified for study only and the ROM files are provided for testing only. This repository contains mostly a re-composition of the creative work of ASCII, SOLiD, OKEI and maybe others. It is provided freely and "as it is" in the hope that it will be useful, but without any warranty of any kind, either expressed or implied. Use at own risk!  

## References
MSX system sources:  
https://sourceforge.net/projects/msxsyssrc/  
  
MSX DOS 2:  
https://www.msx.org/wiki/MSX-DOS_2  


