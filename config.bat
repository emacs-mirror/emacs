@echo off
rem   ----------------------------------------------------------------------
rem   Configuration script for MSDOS
rem   Copyright (C) 1994 Free Software Foundation, Inc.

rem   This file is part of GNU Emacs.

rem   GNU Emacs is free software; you can redistribute it and/or modify
rem   it under the terms of the GNU General Public License as published by
rem   the Free Software Foundation; either version 2, or (at your option)
rem   any later version.

rem   GNU Emacs is distributed in the hope that it will be useful,
rem   but WITHOUT ANY WARRANTY; without even the implied warranty of
rem   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem   GNU General Public License for more details.

rem   You should have received a copy of the GNU General Public License
rem   along with GNU Emacs; see the file COPYING.  If not, write to
rem   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
rem   ----------------------------------------------------------------------
rem   YOU'LL NEED THE FOLLOWING UTILITIES TO MAKE EMACS:
rem
rem   + msdos version 3 or better.
rem   + djgpp version 1,11 maint 4 or better.
rem   + make utility that allows breaking of the 128 chars limit on
rem     command lines.  ndmake (as of version 4.5) won't work due to a
rem     line length limit.
rem   + rm, mv, chmod (From GNU file utilities).
rem   + sed.
rem
rem   You must install in directory c:/emacs or change this script, the
rem   files msdos/sed*.inp, and lisp/dos-fns.el.  (The latter must be
rem   recompiled.)
rem   ----------------------------------------------------------------------
if not "%2" == "" goto usage
if "%1" == "msdos" goto msdos
if "%1" == "msdos-X11" goto msdos11
:usage
echo Usage: config msdos
rem echo    or  config msdos-X11 -- don't even think about it
echo [Read the script before you run it; also check that you have all the
echo necessary utilities.]
goto end
rem   ----------------------------------------------------------------------
:msdos11
set X11=y
goto msdoscommon
rem   ----------------------------------------------------------------------
:msdos
set X11=
:msdoscommon
rem   Change to the Emacs root -- assume we are there
rem cd c:\emacs
rem   ----------------------------------------------------------------------
Echo Configuring the source directory...
cd src
set PATHSH=paths-h.in
if exist %PATHSH% goto src1
set PATHSH=paths.h-in
if exist %PATHSH% goto src1
echo config: *** The file originally called "src/paths.h.in" cannot be found.
cd ..
goto end
:src1
set CONFIGH=config-h.in
if exist %CONFIGH% goto src2
set CONFIGH=config.h-in
if exist %CONFIGH% goto src2
echo config: *** The file originally called "src/config.h.in" cannot be found.
cd ..
goto end
:src2
set MAKEFILEIN=makefile.in-in
if exist %MAKEFILEIN% goto src3
set MAKEFILEIN=makefile-in.in
if exist %MAKEFILEIN% goto src3
echo makefile: *** The file originally called "src/makefile.in.in" cannot be found.
cd ..
goto end
:src3

rem   Create "paths.h"
rm -f paths.h
sed -e "s!/lib/emacs!!" -e "s!/usr/local!c:/emacs!" -e "s!/data!/etc!" <%PATHSH% >paths.h

rem   Create "config.h"
rm -f config.h config.tmp
cp %CONFIGH% config.tmp
if "%X11%" == "" goto src4
sed -f ../msdos/sed4.inp <%CONFIGH% >config.tmp
:src4
sed -f ../msdos/sed2.inp <config.tmp >config.h
rm -f config.tmp

rem   On my system dir.h gets in the way.  It's a VMS file so who cares.
if exist dir.h ren dir.h vmsdir.h

rem   Create "makefile" from "makefile.in.in" using a context patch.
rm -f makefile junk.c
sed -e "1,/cpp stuff/s@^# .*$@@" <%MAKEFILEIN% >junk.c
gcc -E junk.c | sed -f ../msdos/sed1.inp >makefile
rm -f junk.c
cd ..
rem   ----------------------------------------------------------------------
Echo Configuring the library source directory...
cd lib-src
set MAKEFILEIN=makefile.in-in
if exist %MAKEFILEIN% goto libsrc1
set MAKEFILEIN=makefile-in.in
if exist %MAKEFILEIN% goto libsrc1
echo makefile: *** The file originally called "lib-src/Makefile.in.in" cannot be found.
cd ..
goto end
:libsrc1
rem   Create "makefile" from "makefile.in".
sed -e "1,/cpp stuff/s@^# .*$@@" <%MAKEFILEIN% >junk.c
gcc -E -I. -I../src junk.c | sed -e "s/^ /	/" -e "/^#/d" -e "/^[ 	]*$/d" >Makefile.new
sed -f ../msdos/sed3.inp <makefile.new >makefile
cd ..
rem   ----------------------------------------------------------------------
Echo Configuring the main directory...
copy msdos\mainmake makefile >nul
rem   ----------------------------------------------------------------------
:end
set X11=
set MAKEFILEIN=
set PATHSH=
set CONFIGH=

