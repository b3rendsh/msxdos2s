BEER-IDE 2.0
============

This is an update for the BEER-IDE disk interface.

The goal is to improve compatibility and enable future enhancements 
and support by the MSX development community by providing restructured 
and documented open sources for the disk interface rom.

Improvement
-----------
Main improvement is that it now works with other disk interfaces, DOS 2
or Nextor in the same system. If the DOS version of the other disk 
interface is 2.2 or higher then that one will become the master disk 
interface. If it is a DOS 1 interface (e.g. floppy) then the BEER-IDE
will become the master disk interface.

Limitations
-----------
1. Most features of the last version 1.9 by SOLiD are preserved, as are 
the limitations regarding the format of the disk. For compatibility 
with DOS 2.2 FAT12 it is recommended to flash a CF card with the 
existing msxmenu-v1.68-beer.dsk or other with the same disk structure.
2. When using BEER-IDE with another DOS version 2.2 or higher make sure
that DOS 2.2 is loaded before the BEER-IDE. Eg. DOS 2+ interface in 
slot 1 and BEER-IDE in slot 2. If not then the system may halt with 
the message "Not enough memory".

Work in progress
----------------
1. Integrate driver and FAT16 patch in a 32K DOS 2.2 rom.
2. Dynamic drive assignment.
3. Device based driver for Nextor.
4. Improve standard disk format compatibility with MS-DOS.
5. Hardware update BEER-202 with 32K DOS 2.2 and DOS1/DOS2 switch.
6. DOS 2.2 BEER-IDE for MSX-1/64K without mapper (feasibility study).

Disclaimer
----------
This is an expiremental driver for the BEER-IDE interface, it has been
tested in OpenMSX and with a real BEER-202 card in a Philips VG8010
MSX-1 and NMS8255 MSX-2 computer. It is provided freely and "as it is"
in the hope that it will be useful, but without any warranty of any 
kind, either expressed or implied. Use at own risk!




