@echo off

if exist C:\Neil\yhc set comp=C:\Neil\yhc
if exist C:\Documents\Uni\yhc\current set comp=C:\Documents\Uni\yhc\current
if exist D:\sources\yhc\current set comp=D:\sources\yhc\current

if "%1" == "prof" goto make_prof
if "%1" == "opt" goto make_opt
if "%1" == "opt2" goto make_opt2


:make_norm
set flags=
set objdir=obj\norm
set exe=catch
set msg=
goto make

:make_opt
set flags=-O
set objdir=obj\opt
set exe=catch
set msg=
goto make

:make_opt2
set flags=-O2
set objdir=obj\opt2
set exe=catch
set msg=
goto make

:make_prof
set flags=-prof -auto-all
set objdir=obj\prof
set exe=catch_prof
set msg=Run with catch_prof +RTS -p
goto make


:make
mkdir obj 2> nul
mkdir %objdir% 2> nul
@echo on
ghc %flags% --make Main -o %exe%.exe -odir %objdir% -hidir %objdir% -i%comp%\src\libraries\core -i%comp%\src\libraries\general -i..\proposition
@echo off


echo Message: %msg%
