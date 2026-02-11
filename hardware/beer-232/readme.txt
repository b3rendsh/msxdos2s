BEER-232
========

This hardware design is a modified version of the BEER-202 interface:

The 16KB EPROM is replaced with a 64KB EPROM with a jumper to select 
between 2 different 32K ROM images.

It's intended use is to host a 32K MSXDOS2.2 + BEER 2.0 DRIVER DISK ROM.
You can select between two different versions of this ROM or use the 2nd 32K
to host the 16K DOS 1.03 based BEER 2.03 ROM or legacy BEER 1.9 DISK ROM.

The PCB design is derived work from the MSX Makers! BEER-202 v1.1 design.
Besides the mentioned changes there are also small differences in the 
footprints and layout of the PCB due to migration to a newer Kicad version.

Alternatives
------------

1. Modify the BEER-202 PCB to support a 32KB EPROM:
- At the EPROM socket cut the traces to pin 22 and pin 27
- Solder a wire from EPROM socket pin 22 (/OE) to slot connector pad 3 (/CS12)
- Solder a wire from EPROM socket pin 27 (A14) to slot connector pad 18 (A15)
Note: modify at own risk and make sure your PCB version supports this mod.

2. Use the 16KB MSX-DOS 2 firmware version
The 16KB MSX-DOS 2 firmware works the same as the 32KB version except that you
need to prepare the disk format for each IDE disk that is used with this ROM.

Hardware test
-------------

You can test the 8255 chip on the cartridge with the t8255.bas program.
It should produce following output:
PORT    A    B    C
        AA   AA   AA
        0    0    0

Depending on the 8255 manufacturer the line with zero's may show different
values.

If the ROM is flashed correctly with the latest firmware and the system
boots into BASIC it should display a line with "Disk BASIC version .."

You can test compatibility with an IDE disk / CF card with the TASTE program,
even if the disk is not formatted yet.


License
-------

The GPL 3.0 license is applicable for this hardware design only.




