@echo off
set file=%2
if not "%1"=="" goto %1

set tests=Bernoulli Bernoulli_Safe DigitsOfE1 DigitsOfE1_Safe DigitsOfE2 Exp3_8 Gen_Regexps Gen_Regexps_Safe Integrate Paraffins Paraffins_Safe Primes Queens Rfib Tak X2n1

cd catch_1
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
echo TEST: %file%

if exist logs\%file%\summary.log echo FAILED TO RUN CATCH > logs\%file%\summary.log

cd catch_1
catch %file% 2> nul > nul
if errorlevel 1 goto finish
cd ..

cd gadget
gadget %file%
cd ..

echo TEST: %file% >> nofib.txt
type logs\%file%\summary.log >> nofib.txt
echo. >> nofib.txt

goto finish


:finish
