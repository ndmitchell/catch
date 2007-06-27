@echo off

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
set packages= -hide-all-packages -package base -package mtl -package uniplate -package filepath -i%YHC_BASE_PATH%\..\src\libraries\core -i%YHC_BASE_PATH%\..\src\libraries\general -i%YHC_BASE_PATH%\..\depends\play -i..\proposition
@echo on
ghc %flags% --make Main -o %exe%.exe -odir %objdir% -hidir %objdir% %packages%
@echo off


echo Message: %msg%
