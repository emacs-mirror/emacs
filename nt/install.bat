@echo off
if (%1) == (speed) set BUILD_TYPE=spd
if (%1) == (speed) shift
if not (%1) == () set INSTALL_DIR=%1
if not (%1) == () shift
nmake -f makefile.nt install %1 %2 %3 %4 %5 %6 %7 %8 %9
set INSTALL_DIR=
set BUILD_TYPE=
