@echo off
rem   ----------------------------------------------------------------------
rem   Configuration script for MSDOS
rem   Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2001
rem   Free Software Foundation, Inc.

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
rem   along with GNU Emacs; see the file COPYING.  If not, write to the
rem   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
rem   Boston, MA 02111-1307, USA.
rem   ----------------------------------------------------------------------
rem   YOU'LL NEED THE FOLLOWING UTILITIES TO MAKE EMACS:
rem
rem   + msdos version 3 or better.
rem   + djgpp version 1.12maint1 or later (version 2.0 or later recommended).
rem   + make utility that allows breaking of the 128 chars limit on
rem     command lines.  ndmake (as of version 4.5) won't work due to a
rem     line length limit.  The make that comes with djgpp does work.
rem   + rm and mv (from GNU file utilities).
rem   + sed (you can use the port that comes with DJGPP).
rem
rem   You should be able to get all the above utilities from any SimTel
rem   repository, e.g. ftp.simtel.net, in the directory
rem   "pub/simtelnet/gnu/djgpp/v2gnu".  As usual, please use your local
rem   mirroring site to reduce trans-Atlantic traffic.
rem   ----------------------------------------------------------------------
set X11=
set nodebug=
set djgpp_ver=
if "%1" == "" goto usage
rem   ----------------------------------------------------------------------
rem   See if their environment is large enough.  We need 28 bytes.
set $foo$=789012345678901234567
if not "%$foo$%" == "789012345678901234567" goto SmallEnv
set $foo$=
:again
if "%1" == "" goto usage
if "%1" == "--with-x" goto withx
if "%1" == "--no-debug" goto nodebug
if "%1" == "msdos" goto msdos
:usage
echo Usage: config [--with-x] [--no-debug] msdos
echo [Read the script before you run it.]
goto end
rem   ----------------------------------------------------------------------
:withx
set X11=Y
shift
goto again
rem   ----------------------------------------------------------------------
:nodebug
set nodebug=Y
shift
goto again
rem   ----------------------------------------------------------------------
:msdos
Echo Checking whether 'sed' is available...
sed -e "w junk.$$$" <Nul
If Exist junk.$$$ Goto sedOk
Echo To configure 'Emacs' you need to have 'sed'!
Goto End
:sedOk
Echo Checking whether 'rm' is available...
rm -f junk.$$$
If Not Exist junk.$$$ Goto rmOk
Echo To configure 'Emacs' you need to have 'rm'!
Goto End
:rmOk
Echo Checking whether 'mv' is available...
rm -f junk.1 junk.2
echo foo >junk.1
mv junk.1 ./junk.2
If Exist junk.2 Goto mvOk
Echo To configure 'Emacs' you need to have 'mv'!
rm -f junk.1
Goto End
:mvOk
rm -f junk.2
Echo Checking whether 'gcc' is available...
echo main(){} >junk.c
gcc -c junk.c
if exist junk.o goto gccOk
Echo To configure 'Emacs' you need to have 'gcc'!
rm -f junk.c
Goto End
:gccOk
rm -f junk.c junk.o junk junk.exe
Echo Checking what version of DJGPP is installed...
If Not "%DJGPP%" == "" goto djgppOk
Echo To compile 'Emacs' under MS-DOS you MUST have DJGPP installed!
Goto End
:djgppOk
echo int main()           >junk.c
echo #ifdef __DJGPP__    >>junk.c
echo {return (__DJGPP__)*10;} >>junk.c
echo #else               >>junk.c
echo #ifdef __GO32__     >>junk.c
echo {return 10;}         >>junk.c
echo #else               >>junk.c
echo {return 0;}         >>junk.c
echo #endif              >>junk.c
echo #endif              >>junk.c
gcc -o junk junk.c
if not exist junk.exe coff2exe junk
junk
If ErrorLevel 10 Goto go32Ok
rm -f junk.c junk junk.exe
Echo To compile 'Emacs' under MS-DOS you MUST have DJGPP installed!
Goto End
:go32Ok
set djgpp_ver=1
If ErrorLevel 20 set djgpp_ver=2
rm -f junk.c junk junk.exe
rem DJECHO is used by the top-level Makefile
Echo Checking whether 'djecho' is available...
redir -o Nul -eo djecho -o junk.$$$ foo
If Exist junk.$$$ Goto djechoOk
Echo To build 'Emacs' you need the 'djecho.exe' program!
Echo 'djecho.exe' is part of 'djdevNNN.zip' basic DJGPP development kit.
Echo Versions of DJGPP before 2.02 called this program 'echo.exe'.
Echo Either unpack 'djecho.exe' from the 'djdevNNN.zip' archive,
Echo or, if you have 'echo.exe', copy it to 'djecho.exe'.
Echo Then run CONFIG.BAT again with the same arguments you did now.
Goto End
:djechoOk
rm -f junk.$$$
Echo Configuring for DJGPP Version %DJGPP_VER% ...
Rem   ----------------------------------------------------------------------
Echo Configuring the source directory...
cd src

rem   Create "epaths.h"
sed -f ../msdos/sed4.inp <epaths.in >epaths.tmp
update epaths.tmp epaths.h >nul
rm -f epaths.tmp

rem   Create "config.h"
rm -f config.h2 config.tmp
sed -e '' config.in > config.tmp
if "%X11%" == "" goto src4
sed -f ../msdos/sed2x.inp <config.in >config.tmp
:src4
if "%DJGPP_VER%" == "2" Goto src41
sed -f ../msdos/sed2.inp <config.tmp >config.h2
goto src42
:src41
sed -f ../msdos/sed2v2.inp <config.tmp >config.h2
:src42
update config.h2 config.h >nul
rm -f config.tmp config.h2

rem   On my system dir.h gets in the way.  It's a VMS file so who cares.
if exist dir.h ren dir.h vmsdir.h

rem   Create "makefile" from "makefile.in".
rm -f Makefile junk.c
sed -e "1,/== start of cpp stuff ==/s@^# .*$@@" <Makefile.in >junk.c
If "%DJGPP_VER%" == "1" Goto mfV1
gcc -E -traditional junk.c | sed -f ../msdos/sed1v2.inp >Makefile
goto mfDone
:mfV1
gcc -E -traditional junk.c | sed -f ../msdos/sed1.inp >Makefile
:mfDone
rm -f junk.c

if "%X11%" == "" goto src5
mv Makefile makefile.tmp
sed -f ../msdos/sed1x.inp <makefile.tmp >Makefile
rm -f makefile.tmp
:src5

if "%nodebug%" == "" goto src6
sed -e "/^CFLAGS *=/s/ *-gcoff//" <Makefile >makefile.tmp
sed -e "/^LDFLAGS *=/s/=/=-s/" <makefile.tmp >Makefile
rm -f makefile.tmp
:src6
cd ..
rem   ----------------------------------------------------------------------
Echo Configuring the library source directory...
cd lib-src
rem   Create "makefile" from "makefile.in".
sed -e "1,/== start of cpp stuff ==/s@^# .*$@@" <Makefile.in >junk.c
gcc -E -traditional -I. -I../src junk.c | sed -e "s/^ /	/" -e "/^#/d" -e "/^[ 	]*$/d" >makefile.new
If "%DJGPP_VER%" == "2" goto libsrc-v2
sed -f ../msdos/sed3.inp <makefile.new >Makefile
Goto libsrc2
:libsrc-v2
sed -f ../msdos/sed3v2.inp <makefile.new >Makefile
:libsrc2
rm -f makefile.new junk.c
if "%nodebug%" == "" goto libsrc3
sed -e "/^CFLAGS *=/s/ *-gcoff//" <Makefile >makefile.tmp
sed -e "/^ALL_CFLAGS *=/s/=/= -s/" <makefile.tmp >Makefile
rm -f makefile.tmp
:libsrc3
cd ..
rem   ----------------------------------------------------------------------
if "%X11%" == "" goto oldx1
Echo Configuring the oldxmenu directory...
cd oldxmenu
sed -f ../msdos/sed5x.inp <Makefile.in >Makefile
if "%nodebug%" == "" goto oldx2
sed -e "/^CFLAGS *=/s/ *-gcoff//" <Makefile >makefile.tmp
mv -f makefile.tmp Makefile
:oldx2
cd ..
:oldx1
rem   ----------------------------------------------------------------------
Echo Configuring the manual directory...
cd man
sed -f ../msdos/sed6.inp < Makefile.in > Makefile
cd ..
rem   ----------------------------------------------------------------------
Echo Configuring the ELisp manual directory...
cd lispref
sed -f ../msdos/sed6.inp < Makefile.in > Makefile
cd ..
rem   ----------------------------------------------------------------------
Echo Configuring the ELisp Introduction manual directory...
cd lispintro
sed -f ../msdos/sed6.inp < Makefile.in > Makefile
cd ..
rem   ----------------------------------------------------------------------
Echo Configuring the lisp directory...
cd lisp
sed -f ../msdos/sedlisp.inp < Makefile.in > Makefile
cd ..
rem   ----------------------------------------------------------------------
If not Exist leim\quail\latin-pre.el goto maindir
Echo Configuring the leim directory...
cd leim
sed -f ../msdos/sedleim.inp < Makefile.in > Makefile
cd ..
rem   ----------------------------------------------------------------------
:maindir
Echo Configuring the main directory...
If "%DJGPP_VER%" == "1" goto mainv1
Echo Looking for the GDB init file...
If Exist src\.gdbinit update src/.gdbinit src/_gdbinit
If Exist src\_gdbinit goto gdbinitOk
Echo ERROR:
Echo I cannot find the GDB init file.  It was called ".gdbinit" in
Echo the Emacs distribution, but was probably renamed to some other
Echo name without the leading dot when you untarred the archive.
Echo It should be in the "src/" subdirectory.  Please make sure this
Echo file exists and is called "_gdbinit" with a leading underscore.
Echo Then run CONFIG.BAT again with the same arguments you did now.
goto End
:gdbinitOk
Echo Looking for the GDB init file...found
copy msdos\mainmake.v2 Makefile >nul
:mainv1
If "%DJGPP_VER%" == "1" copy msdos\mainmake Makefile >nul
rem   ----------------------------------------------------------------------
goto End
:SmallEnv
echo Your environment size is too small.  Please enlarge it and run me again.
echo For example, type "command.com /e:2048" to have 2048 bytes available.
set $foo$=
:end
set X11=
set nodebug=
set djgpp_ver=
