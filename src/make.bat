@echo off
mkdir Temp 2> nul
mkdir Temp\Debug 2> nul
mkdir Temp\Release 2> nul
mkdir Temp\Profile 2> nul

set mode=%1
if "%1" == "" set mode=debug

set flag=
if %mode% == debug set flag=.exe -O0
if %mode% == release set flag=.exe -O1
if %mode% == profile set flag=_prof.exe -prof -auto-all
if %mode% == drift goto drift

if "%flag%" == "" goto bad_flags

set comp=
if exist C:\Neil\yhc-devel\src\compiler98 set comp=C:\Neil\yhc-devel\src\compiler98
if exist D:\sources\yhc\11_sep_2006\yhc-devel\src\compiler98 set comp=D:\sources\yhc\11_sep_2006\yhc-devel\src\compiler98
if exist C:\Documents\Uni\yhc\15_sep_2006\yhc-devel\src\compiler98 set comp=C:\Documents\Uni\yhc\15_sep_2006\yhc-devel\src\compiler98

echo Compiling for %mode%
ghc --make Main -o catch%flag% -odir Temp\%mode% -hidir Temp\%mode% -i%comp%
goto finish


:bad_flags
echo Bad flag specified
goto finish


:drift
echo module Hite.Binary where > Hite\Binary.hs
echo import General.Binary >> Hite\Binary.hs
echo import Hite.Type; import Hite.TypeType; import Hite.DataType >> Hite\Binary.hs
drift -r Hite\Type.hs >> Hite\Binary.hs
drift -r Hite\TypeType.hs >> Hite\Binary.hs
drift -r Hite\DataType.hs >> Hite\Binary.hs
goto finish


:finish
