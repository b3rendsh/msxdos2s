# msxdos2s
- MSX DOS 2.2 in a 32K ROM package
- BEER-IDE driver 2.0 that is compatible with MSX DOS 2.2 and other disk systems
- Disk hardware interface

## 32K MSX DOS 2.2
This repository contains a recomposition of the MSX DOS 2.2 codebase. The compiled code is a 32K disk ROM that can be put in a 32K EPROM inside the machine,  on a standard MSX ROM cartridge or the BEER-232 IDE interface.  

[ROM binaries](rom/)  
There are two precompiled binaries, either with or without the FAT16 patch applied.

[Source files](mod-32k/)  
The binaries can be created from source with the z88dk tools.

 1. All functions of MSX DOS 2.2 as originally distributed by ASCII have been retained except for Kanji support and dos2memchk ("CALL DOS2MEMCHK" in BASIC) which have been removed to reduce the size of the ROM. 
 2. The binary includes only the dummy disk driver from ASCII and no OEM disk interface driver. 
 3. The original 64K ROM came on a cartridge with a ROM mapper. To use that version you need a small logic circuit e.g. a 74LS138 and 74LS74 in addition to a 64K EPROM. With the 32K version this circuit is no longer needed. 
 4. The ROM works with msxdos2.sys and command2.com version 2.20 or compatible loaded from disk media such as a floppy or IDE disk / compact flash card. 

[Development](dev-32k/)  
This folder contains the latest source code of this project. It is possible to create a 32K ROM that optionally contains the FAT16 patch and/or BEER-IDE 2.0 driver with support for FAT16 partitions. 

## BEER-IDE 2.0
[16K BEER-IDE 2.0 ROM](https://github.com/b3rendsh/msxdos2s/blob/main/rom/BEER20_DISK.ROM)  
[Source files](mod-beer20/)  
The 16K ROM is based on MSX DOS 1.03 and contains the driver 2.0 for the BEER IDE disk interface. 

1. The main improvement compared to the 1.9 driver is that it will work with other disk systems in the machine that have higher DOS versions e.g. a SUNRISE IDE with MSX DOS 2.2 or Nextor.
2. In the source code all the differences between v1.9 and v2.0 are marked and commented in detail.
3. It can be used as the disk ROM with the BEER-202 or BEER-232 hardware. 
4. Works on a MSX1 system with 64K RAM. 
5. Supports one IDE disk typically a CF card with use of a 40 pin CF adapter.
6. The disk system supports up to 4 partitions of 32MB. 

## Hardware 
[BEER-232 IDE interface](hardware/beer-232/)  
The BEER-232 cartridge is derived from the BEER-202 and supports 2x 32K disk ROM  instead of a  single 16K ROM.

## Limitations
The source files are provided and modified for study only and the ROM files are provided for testing only. This repository contains mostly a re-composition of the creative work of ASCII, SOLiD, OKEI and maybe others. It is provided freely and "as it is" in the hope that it will be useful, but without any warranty of any kind, either expressed or implied. Use at own risk!  

## References
MSX system sources:  
https://sourceforge.net/projects/msxsyssrc/  
  
MSX DOS 2:  
https://www.msx.org/wiki/MSX-DOS_2  



