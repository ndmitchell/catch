@echo off
set file=%2
if not "%1"=="" goto %1

set tests=Bernoulli Bernoulli_Safe DigitsOfE1 DigitsOfE1_Safe DigitsOfE2 Exp3_8 Gen_Regexps Gen_Regexps_Safe Integrate Paraffins Paraffins_Safe Primes Queens Rfib Tak X2n1

cd catch_1
call make opt
cd ..

cd gadget
call make opt
cd ..


echo RUNNING NOFIB TESTS > nofib.txt
echo RUNNING NOFIB TESTS > timings.txt
for %%i in (%tests%) do call %0 build %%i

cls
echo BEGIN DIFFERENCES
diff nofib_oracle.txt nofib.txt --unified
echo END DIFFERENCES
goto finish


:build
echo TEST: %file%
echo TEST: %file% >> nofib.txt
echo TEST: %file% >> timings.txt

if exist logs\%file%\summary.log echo FAILED TO RUN CATCH > logs\%file%\summary.log

cd catch_1
unix_time catch %file% +RTS -t 2> nul > catch.time
tail catch.stat --lines=1 >> ..\timings.txt
tail catch.time --lines=1 >> ..\timings.txt
echo . >> ..\timings.txt
cd ..

cd gadget
unix_time gadget %file% +RTS -t 2> nul > gadget.time
tail gadget.stat --lines=1 >> ..\timings.txt
tail gadget.time --lines=1 >> ..\timings.txt
echo . >> ..\timings.txt
cd ..

type logs\%file%\summary.log >> nofib.txt
echo. >> nofib.txt

goto finish


:finish
