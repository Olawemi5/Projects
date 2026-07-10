@echo off
set RSCRIPT="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
cd /d "%~dp0"
%RSCRIPT% -e "source('tests/testthat.R')"

