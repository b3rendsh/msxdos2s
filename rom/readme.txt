ROM Images
==========

This folder contains the following ROM images:

BEER20_DISK.ROM
---------------
16K ROM with driver version 2.0.x for the BEER IDE disk interface. It can be
used on a BEER202 or BEER232 PCB. The included DOS version is 1.03 so it works
on a MSX1 system with 64K RAM. With this ROM you can use the BEER IDE cartridge
also together with other cartridges that contain higher DOS versions e.g. the
SUNRISE IDE or a separate 32K EEPROM cartridge with a DOS v2.2 ROM below.
Sources in mod-beer20 folder.


MSXD22S.ROM
-----------
32K ROM that is mostly the same as the original ASCII DOS v2.2 but without the
use of a ROM mapper. It contains a dummy disk driver. This ROM can be used on
a 32K EEPROM PCB together with other disk interfaces in the system e.g.
internal floppy drive. The patch16.com can be applied to get FAT16 support but
you can't undo it with patch16 /r. The MSX2 check at init is removed so it also
works on a MSX1 with a 128K or more RAM mapper.
Sources in mod-32k folder.


MSXD22SX.ROM
------------
32K ROM same as the MSXD22S ROM with the FAT16 patch already applied.
This also means that format and ramdisk functions are disabled. Some code is
optimized to free space for the extra FAT16 code. For FAT16 the command2.com
must still be patched once with compatch.com.
Sources in mod-32k folder.


BEER_AND_SODA.ZIP
-----------------
ZIP archive with ROM and EEPROM images for the BEER and SODA IDE interfaces.
The ROM images include the latest DOS enhancements and IDE disk drivers.
Sources in dev-32k folder.


DEVELOPMENT
-----------
There is a build script (Makefile) so you can create a custom ROM with the
latest changes from source with the Z88DK toolkit on a Windows PC or Linux PC.
Sources in dev-32k folder.



