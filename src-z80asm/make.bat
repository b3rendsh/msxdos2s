@echo  off
rem Make z80 binary from asm source with z88dk tools

if "%1"=="rom" goto rom
if "%1"=="test" goto test
if "%1"=="clean" goto clean

echo Usage: make [rom^|test^|clean]
goto end

:rom
echo Assembling rom..
set obj=.\obj\
set target=ROM
del /q %obj%
z80asm -b -d -l -m -D%target% -O%obj% -o=msxd22 disks0.asm disks1.asm disks2.asm kanji.asm
copy / b %obj%msxd22_bank0.bin + %obj%msxd22_bank1.bin + %obj%msxd22_bank2.bin + %obj%msxd22_kanji.bin msxd22.rom
echo done
goto end


:test
echo Compiling test..
set obj=.\objtest\
set app=.\app\
set target=TEST
z80asm -b -d -l -m -D%target% -O%obj% -o=msxd22 disks0.asm disks1.asm disks2.asm kanji.asm
copy / b %obj%msxd22_bank0.bin + %obj%msxd22_bank1.bin + %obj%msxd22_bank2.bin + %obj%msxd22_kanji.bin msxd22-test.rom
echo done
goto end

:clean
del /q obj
del /q objtest
echo Cleanup done

:end

