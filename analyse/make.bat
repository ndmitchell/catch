@echo off
mkdir obj 2> nul

if exist C:\Neil\yhc set comp=C:\Neil\yhc
if exist C:\Documents\Uni\yhc\current set comp=C:\Documents\Uni\yhc\current
if exist D:\sources\yhc\current set comp=D:\sources\yhc\current

ghc --make Main -o analyse.exe -odir obj -hidir obj -i%comp%\src\libraries\core -i%comp%\src\libraries\general -i..\proposition
