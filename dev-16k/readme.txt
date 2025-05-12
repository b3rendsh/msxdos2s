DEV-16K
=======

Special test version of the smaller 32K DOS2 with BEER IDE driver,
for use with openMSX or BEER-202 interface. The ROM file is
further reduced to 16K and the 16K DOS 2 kernel code segment
will be loaded from a designated area on the disk. 
This version only works with converted disk images!


ROM images
----------
The 32K ROM image is split in following two 16KB files:
beer_16k.rom	ROM file that can be used as the BEER IDE ROM
dos2_16k.img	DOS2 kernel image file that is loaded from disk


DISK image
----------
Convert a standard disk image (with MBR) using the dsk_dos2.py script.
It will insert the content of the dos2 kernel file in the disk image after
the MBR before the first partition, and the partition table entries are
updated with their new starting sector.
To update the kernel code in a disk image use the upd_dos2.py script.


Floppy image
------------
You can convert a 720k floppy image to a BEER disk image by inserting the
mbr720k image file before the floppy image file e.g. from a command prompt:
copy /b mbr720k.img + floppy.dsk harddisk.dsk".
Do this before using the dsk_dos2.py script above.


Notes
-----
If you try to boot from a disk that doesn't contain the 16K kernel then
the system will halt with a "No kernel on disk" message.
The "test720k_dos2_16k.dsk" file contains a bootable DOS2 disk image ready
for use with the 16K DOS2 rom.