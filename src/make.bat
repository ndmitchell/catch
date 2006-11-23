@echo off

set out=..\obj

mkdir %out% 2> nul
mkdir %out%\Debug 2> nul
mkdir %out%\Release 2> nul
mkdir %out%\Profile 2> nul

set mode=%1
if "%1" == "" set mode=debug

set flag=
if %mode% == debug set flag=.exe -O0
if %mode% == release set flag=.exe -O1
if %mode% == profile set flag=_prof.exe -prof -auto-all
if %mode% == drift goto drift

if "%flag%" == "" goto bad_flags

set comp=
if exist C:\Neil\yhc set comp=C:\Neil\yhc
if exist D:\sources\yhc\11_sep_2006\yhc-devel set comp=D:\sources\yhc\11_sep_2006\yhc-devel
if exist C:\Documents\Uni\yhc\15_sep_2006\yhc-devel set comp=C:\Documents\Uni\yhc\15_sep_2006\yhc-devel

echo Compiling for %mode%
ghc --make Main -o catch%flag% -odir %out%\%mode% -hidir %out%\%mode% -i%comp%\src\libraries\core -i%comp%\src\libraries\general -i..\proposition
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

echo module Hill.Binary where > Hill\Binary.hs
echo import General.Binary >> Hill\Binary.hs
echo import Hill.Type; import Hite.Binary >> Hill\Binary.hs
drift -r Hill\Type.hs >> Hill\Binary.hs

goto finish


:finish
