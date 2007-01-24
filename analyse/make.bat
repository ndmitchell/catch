@echo off

if exist C:\Neil\yhc set comp=C:\Neil\yhc
if exist C:\Documents\Uni\yhc\current set comp=C:\Documents\Uni\yhc\current
if exist D:\sources\yhc\current set comp=D:\sources\yhc\current

if "%1" == "test" goto make_test

mkdir obj 2> nul
ghc --make Main -o analyse.exe -odir obj -hidir obj -i%comp%\src\libraries\core -i%comp%\src\libraries\general -i..\proposition
goto finish


:make_test
mkdir oobj 2> nul
ghc --make PathCtorTest -o tester.exe -O2 -odir oobj -hidir oobj -i%comp%\src\libraries\core -i%comp%\src\libraries\general -i..\proposition



:finish


