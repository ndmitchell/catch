@echo off
mkdir Temp 2> nul
mkdir Temp\Debug 2> nul
mkdir Temp\Release 2> nul
mkdir Temp\Profile 2> nul

set mode=%1
if "%1" == "" set mode=debug

set flag=
if %mode% == debug set flag= -O0
if %mode% == release set flag= -O1
if %mode% == profile set flag=_prof -prof -auto-all

if "%flag%" == "" goto bad_flags

echo Compiling for %mode%
ghc --make Main -o catch%flag% -odir Temp\%mode% -hidir Temp\%mode%
goto finish


:bad_flags
echo Bad flag specified
goto finish


:finish
