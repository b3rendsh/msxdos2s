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


DEVELOPMENT
-----------
Newer development versions are not provided as a ROM in this repository. There
is a build script (makefile) so you can create the binary ROM from source code
with the Z88DK toolkit on a Windows PC or Linux PC. You can choose the target
build options that are most suitable e.g. to create a ROM with DOS v2.2 that
includes the BEER driver and FAT16 patch. The 32K ROM can be used on a BEER232
PCB but not on a BEER202 PCB.
Sources in dev-32k folder.
