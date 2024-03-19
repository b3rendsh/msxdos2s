BEER-IDE 2.0
============

This is an update for the BEER-IDE disk interface.

The goal is to improve compatibility and enable future enhancements 
and support by the MSX development community by providing restructured 
and documented open sources for the disk interface rom.

The BEER-IDE disk interface codebase consists of following modules:
1. Standard MSX-DOS 1.03 disk system.
2. Disk system enhancements i.e. FAT and floppy drive handling.
3. Low level IDE disk driver for use with the BEER-202 hardware.


Improvements
------------
Main improvement is that it now works with other disk interfaces, DOS 2
or Nextor in the same system. If the DOS version of the other disk 
interface is 2.2 or higher then that one will become the master disk 
interface. If it is a DOS 1 interface (e.g. floppy) then the BEER-IDE
will become the master disk interface.

The low-level module can now also be used as a driver in a MSX-DOS 2.2 
based disk system.

The disk system will dynamically assign drives for the number of defined 
and accessible primary partitions on the disk e.g. if there are 2 primary 
partitions on a disk then 2 drives are assigned not 4. This prevents
the creation of phantom drive letters.



Limitations
-----------
1. Most features of the last version 1.9 by SOLiD are preserved, as are 
the limitations regarding the format of the disk. For compatibility 
with DOS 2.2 FAT12 it is recommended to flash a CF card with the 
existing msxmenu-v1.68-beer.dsk or other with the same disk structure.
2. Look for the BEER19_OLD and FLOPPY labels in the source code to see 
the details what changed compared to version 1.9. Most notably the drive
assignment to internal floppy disks is no longer fixed to A and B but 
according to MSX-DOS default behaviour.


Work in progress
----------------
1. Device based driver for Nextor.
2. Improve standard disk format compatibility with MS-DOS.
3. Hardware update BEER-202 with 32K DOS 2.2 and DOS1/DOS2 switch.
4. DOS 2.2 BEER-IDE for MSX-1/64K without mapper (feasibility study).


Disclaimer
----------
This is an experimental driver for the BEER-IDE interface, it has been
tested in OpenMSX and with a real BEER-202 card in a Philips VG8010
MSX-1 and NMS8255 MSX-2 computer. It is provided freely and "as it is"
in the hope that it will be useful, but without any warranty of any 
kind, either expressed or implied. Use at own risk!




