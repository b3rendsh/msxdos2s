# msxdos2s
- MSX DOS 2.2 in a 32K ROM package
- BEER and SODA IDE driver that is compatible with MSX DOS 2.2 and other disk systems
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

## BEER and SODA IDE
[BEER and SODA ROM package](https://github.com/b3rendsh/msxdos2s/blob/main/rom/beer_and_soda.zip)  
[Development](dev-32k/)  
This folder contains the latest source code of this project.

You can build a 16K MSX DOS 1 ROM or 32K MSX DOS 2.2 ROM with a BEER or SODA IDE driver.  
Following DOS enhancements are included:

1. Large disks with a standard Master Boot Record
2. Maximum of 4 primary partitions and/or 1 or more extended partitions.
3. Maximum of 8 partitions
4. FAT12 and FAT16 file systems*
5. Boot drive selection (boot menu)
6. Standard DOS 2 or Nextor partitions / disk format*
   
*The number of root directory entries (files) in DOS 1 is limited to 255.

## BEER-IDE 2.0
[16K BEER-IDE 2.0 ROM](https://github.com/b3rendsh/msxdos2s/blob/main/rom/BEER20_DISK.ROM)  
[Source files](mod-beer20/)  
The 16K ROM is based on MSX DOS 1.03 and contains the driver 2.0 for the BEER IDE disk interface. 

1. The main improvement compared to the 1.9 driver is that it will work with other disk systems in the machine that have higher DOS versions e.g. a SUNRISE IDE with MSX DOS 2.2 or Nextor.
2. In the source code all the differences between v1.9 and v2.0 are marked and commented in detail.
3. It can be used as the disk ROM with the BEER-202 or BEER-232 hardware. 
4. Works on a MSX1 system with 64K RAM. 
5. Supports one IDE disk typically a CF card with use of a 40 pin CF adapter.
6. The disk system supports up to 4 FAT12 or FAT16 partitions.
7. The number of root directory entries (files) in DOS 1 is limited to 255.

## Hardware 
[BEER-232 IDE interface](hardware/beer-232/)  
The BEER-232 cartridge is derived from the BEER-202 and supports 2x 32K disk ROM  instead of a  single 16K ROM.

[SODA IDE interface](https://github.com/Danjovic/Soda-IDE)  
The SODA IDE cartridge design is based on a RCBus Compact Flash module. Danjovic adapted this design for MSX and made the PCB layout.  

## Limitations
The source files are provided and modified for study only and the ROM files are provided for testing only. This repository contains mostly a re-composition of the creative work of ASCII, SOLiD, OKEI and maybe others. It is provided freely and "as it is" in the hope that it will be useful, but without any warranty of any kind, either expressed or implied. Use at own risk!  

## References
MSX system sources:  
https://sourceforge.net/projects/msxsyssrc/  
  
MSX DOS 2:  
https://www.msx.org/wiki/MSX-DOS_2  



