@echo off
if (%1) == () nmake -f makefile.nt all
if not (%1) == () nmake -f makefile.nt %1 %2 %3 %4 %5 %6 %7 %8 %9
