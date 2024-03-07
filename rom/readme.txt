ROM Images
==========

This folder contains the following ROM images. 
See disk.inc in source folder for build options.


MSXD22S.ROM
-----------
32K ROM version that is mostly the same as the original ASCII DOS v2.2.
No optimization so this version may work with patch16.com (fat16 /r will fail).
Kanji, rom mapper and self check code is removed to reduce rom size.
MSX-2 check at init is removed so it may work on MSX-1 with ram mapper.
Sources in mod-32k folder.


MSXD22SX.ROM
------------
32K ROM version that includes the FAT16 patch.
This also means that format and ramdisk functions are disabled.
The optimized code option is required to free space for the extra fat16 code.
For FAT16 the command2.com must still be patched once with compatch.com.
Sources in mod-32k folder.


MSXD22.ROM
----------
Original ASCII ROM version 2.2 with DiskBasic 2.01 fixes.
KANJI bank 3 is mostly equal to HSH version, some source code is missing here.
Sources in src-z80asm folder.


BEER20_DISK.ROM
---------------
New experimental driver version 2.0 for the BEER-IDE disk interface.
The included DOS version is 1.03. 
With this rom you can use the beer-ide together with higher DOS versions.
Also the MS-DOS file system compatibility is improved.
Sources in mod-beer20 folder.


DEVELOPMENT
-----------
Newer development versions will not be provided as a ROM in this repository.
There will be a build script so you can create the ROM from source code.
You can choose the target build options that are most suitable for you.
Sources in dev-32k folder.





