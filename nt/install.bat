@echo off
if (%1) == (speed) set BUILD_TYPE=spd
if (%1) == (speed) shift
if not (%1) == () set INSTALL_DIR=%1
nmake -f makefile.nt install
set INSTALL_DIR=
set BUILD_TYPE=
