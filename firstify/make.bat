@echo off
mkdir obj 2> nul

if exist C:\Neil\yhc set comp=C:\Neil\yhc
if exist D:\sources\yhc\11_sep_2006\yhc-devel set comp=D:\sources\yhc\11_sep_2006\yhc-devel
if exist D:\sources\yhc\h.o\yhc set comp=D:\sources\yhc\h.o\yhc
if exist C:\Documents\Uni\yhc\15_sep_2006\yhc-devel set comp=C:\Documents\Uni\yhc\15_sep_2006\yhc-devel
if exist C:\Documents\Uni\yhc\current set comp=C:\Documents\Uni\yhc\current

ghc --make Main -o firstify -odir obj -hidir obj -i%comp%\src\libraries\core -i%comp%\src\libraries\general
