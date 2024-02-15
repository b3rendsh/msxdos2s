@echo  off
rem Make z80 binary from asm source with z88dk tools

if "%1"=="oem" goto oem
if "%1"=="beer20" goto beer20
if "%1"=="test" goto test
if "%1"=="clean" goto clean

echo Usage: make [oem^|beer20^|test^|clean]
goto end

:oem
echo Assembling oem rom..
set obj=.\obj\
set target=OEM
del /q %obj%
z80asm -b -d -l -m -D%target% -O%obj% -o=%target% disk.asm driver-stub.asm
z88dk-appmake +glue -b %obj%%target% --filler 0x00 --clean
z88dk-appmake +rom -b %obj%%target%__.bin -o %target%_DISK.ROM -s 16384 --org 0 --filler 0x00
findstr /b "DOS_" %obj%%target%.MAP > %target%_entry_points.txt
goto end

:beer20
echo Assembling beer20 rom..
set obj=.\obj\
set target=BEER20
del /q %obj%
z80asm -b -d -l -m -D%target% -O%obj% -o=%target% disk.asm disk-beer20.asm driver-beer20.asm
z88dk-appmake +glue -b %obj%%target% --filler 0x00 --clean
z88dk-appmake +rom -b %obj%%target%__.bin -o %target%_DISK.ROM -s 16384 --org 0 --filler 0x00
findstr /b "DOS_" %obj%%target%.MAP > %target%_entry_points.txt
goto end

:test
echo Compiling test..
set obj=.\objtest\
set target=TEST
z80asm -b -d -l -m -D%target% -O%obj% -o=%target% disk.asm
echo done
goto end

:clean
del /q obj
del /q objtest
echo Cleanup done

:end

