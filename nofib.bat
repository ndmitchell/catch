
set file=%2
if not "%1"=="" goto %1

set tests=DigitsOfE1 DigitsOfE1_Safe DigitsOfE2 Exp3_8 Gen_Regexps Gen_Regexps_Safe Integrate Primes Queens Rfib Tak X2n1

if exist C:\Neil\yhc set comp=C:\\Neil\\yhc
if exist C:\Documents\Uni\yhc\current set comp=C:\\Documents\\Uni\\yhc\\current
if exist D:\sources\yhc\current set comp=D:\\sources\\yhc\\current
set comp=%comp%\\src\\libraries\\*

set hugs=runhugs -98 -P".;..;..\\..;{Hugs}\\packages\\*;C:\\Program Files\\Haskell\\hugs\\packages\\*;%comp%;..\\..\\proposition" Main.hs

cd firstify
call make
cd ..

cd gadget
call make opt
cd ..


echo RUNNING NOFIB TESTS > nofib.txt
for %%i in (%tests%) do call %0 build %%i

cls
echo BEGIN DIFFERENCES
diff nofib_oracle.txt nofib.txt --unified
echo END DIFFERENCES
goto finish


:build
echo FAILED TO RUN CATCH > logs\%file%\summary.log
cd prepare
%hugs% %file%
cd ..
cd firstify
firstify %file% 2> nul > nul
cd ..
cd letelim
%hugs% %file% > nul
cd ..
cd gadget
gadget %file%
cd ..
echo TEST: %file% >> nofib.txt
type logs\%file%\summary.log >> nofib.txt
echo. >> nofib.txt

goto finish


:finish
