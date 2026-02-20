# msxdos2s
- Enhanced MSX DOS 2.2 / 2.31 in a 32K ROM package
- Enhanced MSX DOS 1
- Optimized BEER and SODA IDE interface drivers
- Disk hardware interface designs
- MSX JIO client

## Introduction
This repository contains a re-composition of the MSX DOS 2 disk ROM that can be used as a 32K ROM without a ROM mapper (or 16K ROM for the BEER interface) and includes enhancements for large disks. 

There is also a version of MSX DOS 1 which contains enhancements that are based on the 16K BEER IDE disk ROM that SOLiD created.

And thirdly it includes a disk driver for MSX-DOS 1 and 2 that can be used with the BEER and SODA disk interfaces.

All functions of MSX DOS 1 and MSX DOS 2 as originally distributed by ASCII have been retained and should work 100% the same except for the changes that are mentioned below.
The disk ROM works with the original msxdos(2).sys and command(2).com loaded from disk media such as a floppy or IDE disk / compact flash card. 
The disk ROM also works if there are other disk interfaces in the machine and it is compliant with the MSX standards to determine which disk interface will be the master.

## Enhanced MSX DOS IDE disk system

[BEER and SODA ROM package](https://github.com/b3rendsh/msxdos2s/blob/main/rom/beer_and_soda.zip)  
[Source code](dev-32k/)  

Following DOS and driver enhancements are included:
1. Support for large disks with a standard Master Boot Record.
2. Maximum of 4 primary partitions and/or 1 or more extended partitions.
3. Maximum of 8 partitions.
4. Support for FAT16 file systems.
5. Boot drive selection (boot menu).
6. Support for standard DOS 2 or Nextor partitions and disk format.

#### MSX DOS 2 changes
1. Added support for [FAT16 partitions](docs/FAT16%20partitions.txt)
2. Kanji and ROM check ("call dos2memchk" in BASIC) are removed to reduce the size of the ROM.
3. The ROM mapper code is removed and all the code is consolidated in a contiguous 32K ROM space.
4. If the turbo R specific changes are included with the DOS v2.31 option then the ROM still works on older machines.

#### MSX DOS 1 changes
1. Added support for [FAT16 partitions](docs/FAT16%20partitions.txt)
2. Added FAT swapper from SOLiD to support partitions larger than 16MB.

#### Universal IDE driver
The IDE driver is split into a common DOS layer and a low level disk interface layer. It currently supports the BEER-202, BEER-232 and SODA disk interface hardware. If the hardware is not detected then a dummy driver is used.
1. The code is optimized to increase the data transfer rate.
2. Detection of hardware and I/O ports for the SODA interface.
3. Includes support for extended partitions and up to 8 drives.
4. Only one master IDE disk is supported. No master/slave or ATAPI (CDROM) drives. The SODA interface works with CF cards only.
5. Optional execution of boot code in the MBR, in addition or as a replacement for the boot menu.

#### TASTE.COM
IDE performance test and diagnostics utility. It works with the BEER and SODA interfaces.

## Disk interfaces 
**BEER-202 IDE interface**  
This is the original BEER interface, there are several designs with small variations that you can build yourself.  
The [16K MSX-DOS 1 disk ROM](rom/) and [16K MSX-DOS 2 disk ROM](dev-16k/) can be used with this interface.  
  
[**BEER-232 IDE interface**](hardware/beer-232/)  
The BEER-232 cartridge is derived from the BEER-202 and supports 2x 32K disk ROM  instead of a  single 16K ROM.
  
[**SODA IDE interface**](https://github.com/Danjovic/Soda-IDE)  
The SODA IDE cartridge design is based on a RCBus Compact Flash module. Danjovic adapted this design for MSX and made the PCB layout.  
  
[**MSX JIO**](https://github.com/louthrax/MSXJIO)  
Remote serial disk solution from Louthrax. It consists of a disk image server on a PC or android phone, a MSX client and a serial connection over the MSX joystick (gpio) port 2 using a ftdi cable or bluetooth adapter. 

[**MSX JIO CART**](https://github.com/herraa1/msx-jio-cart-v1)  
MSX JIO cartridge from Herraa1. Software based serial communnications like MSXJIO but without using joystick port 2.

## Older versions

#### 32K MSX DOS 2.2
[ROM binaries](rom/)  
[Source files](mod-32k/)  
The main changes compared to ASCII's MSXDOS 2.2 are that this ROM works without a ROM mapper and doesn't contain Kanji support. Only the original dummy disk driver from ASCII is included, no OEM disk interface driver. Also the enhancements that are mentioned above are not included. 

#### 16K BEER-IDE 2.0
[BEER-IDE 2.0 ROM](https://github.com/b3rendsh/msxdos2s/blob/main/rom/BEER20_DISK.ROM)  
[Source files](mod-beer20/)  
The main improvement compared to the 1.9 driver is that it will work with other disk systems in the machine that have higher DOS versions e.g. a SUNRISE IDE with MSX DOS 2.2 or Nextor. Some other nasty bugs have been fixed. In the source code all the differences between v1.9 and v2.0 are marked and commented in detail.

To use MiSiX DOS use the 2.0 ROM or compile with 1.9 compatibility options, newer ROM versions don't work with MiSiX.

## Contributions
Although contributions of all kinds are welcome, I have no intention to add any more features or modify the DOS kernel in this repository.

Please create an issue if you want to report a bug or have any requests or suggestions. Pull requests from forks are currently not supported.

## License
Most of the sources are a re-composition of the creative work of ASCII and others. They are shared here for educational and non commercial purposes only. It is believed that these works are being used in accordance with the intentions and/or licensing of their creators.

The Creative Commons license terms apply to all custom source code, drivers and binaries.

The material is provided freely and "as it is" in the hope that it will be useful, but without any warranty of any kind, either expressed or implied. Use at own risk!

## References
MSX system sources:  
https://sourceforge.net/projects/msxsyssrc/  
  
MSX DOS 2:  
https://www.msx.org/wiki/MSX-DOS_2  

Other:
- MSX Technical Data Book for MSX-DOS 1
- MSX DOS 2 System Specification 1986-11-26
- Nextor 2.1 Driver Development Guide (github)
- Microsoft FAT Specification (August 30 2005)
- IDE ata3 specification (Revision 6, 26 October 1995)
- Intel 8255A PPI data sheet (August 1991)


