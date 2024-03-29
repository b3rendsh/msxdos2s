@echo  off
rem Make z80 binary from asm source with z88dk tools

if "%1"=="rom" goto rom
if "%1"=="fat16" goto fat16
if "%1"=="test" goto test
if "%1"=="clean" goto clean

echo Usage: make [rom^|fat16^|test^|clean]
goto end

:rom
echo Assembling rom..
set obj=.\obj\
set target=ROM
del /q %obj%
z80asm -b -d -l -m -D%target% -O%obj% -o=msxd22s.bin msxd22s0.asm driver.asm msxd22s1.asm msxd22s2.asm
z88dk-appmake +glue -b %obj%msxd22s --filler 0xFF --clean
z88dk-appmake +rom -b %obj%msxd22s__.bin -o msxd22s.rom -s 32768 --org 0
echo done
goto end

:fat16
echo Compiling fat16..
set obj=.\obj\
set target=FAT16
del /q %obj%
z80asm -b -d -l -m -D%target% -O%obj% -o=msxd22s.bin msxd22s0.asm driver.asm msxd22s1.asm msxd22s2.asm
z88dk-appmake +glue -b %obj%msxd22s --filler 0xFF --clean
z88dk-appmake +rom -b %obj%msxd22s__.bin -o msxd22sx.rom -s 32768 --org 0
echo done
goto end


:test
echo Compiling test..
set obj=.\objtest\
set app=.\app\
set target=TEST
z80asm -b -d -l -m -D%target% -O%obj% -o=msxd22s.bin msxd22s0.asm driver.asm msxd22s1.asm msxd22s2.asm
z88dk-appmake +glue -b %obj%msxd22s --filler 0xFF --clean
z88dk-appmake +rom -b %obj%msxd22s__.bin -o msxd22t.rom -s 32768 --org 0
findstr /b "DOS_" %obj%msxd22s.map > %target%_entry_points.txt
echo done
goto end

:clean
del /q obj
del /q objtest
echo Cleanup done

:end

