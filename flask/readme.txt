FLASK
=====

Write image to simple 64K SST flash ROM.

The intended use is for updating the disk system ROM in the MALT IDE cartridge.
It also works with other cartridges that have a SST flash ROM but banked ROM is
not supported.

This program is based on WRTSST from HRA!

The main difference is that by default FLASK only erases the 64K flash memory
that is currently visible in the Z80 address space and not the entire ROM chip.

Commandline options:

/Sx	Set the target slot to slot #x
	If this option is omitted then the program will search for the first
	slot with a SST flash ROM. Extended slots are not supported.

/A	Set the start address to 0x8000
	The default start address for ROM images up to 32KB is 0x4000 and larger
	ROM images up to 64KB will be loaded at address 0x0000.

/E	Erase the entire flash ROM memory instead of the 64K sector erase.




