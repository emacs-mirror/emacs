@echo off
rem   ----------------------------------------------------------------------
rem   Configuration script for MS Windows 95/98 and NT/2000
rem   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
rem   + MS Windows 95/98 or NT/2000
rem   + either MSVC 2.x or later, or gcc-2.95 or later (with gmake 3.75
rem     or later) and the Mingw32 and W32 API headers and libraries
rem
rem For reference, here is a list of which builds of gmake are known to
rem work or not, and whether they work in the presence and/or absence of
rem sh.exe.
rem  
rem                                       sh exists     no sh
rem  cygwin b20.1 make (3.75):            okay[1]       fails[2]
rem  MSVC compiled gmake 3.77:            okay          okay
rem  MSVC compiled gmake 3.78.1:          okay          okay
rem  MSVC compiled gmake 3.79.1:          okay          okay
rem  mingw32/gcc-2.92.2 make (3.77):      okay          okay
rem  cygwin compiled gmake 3.77:          okay[1]       fails[2]
rem  cygwin compiled gmake 3.78.1:        okay          fails[2]
rem  cygwin compiled gmake 3.79.1:        couldn't build make[3]
rem
rem [1] doesn't cope with makefiles with DOS line endings, so must mount
rem     emacs source with text!=binary.
rem [2] fails when needs to invoke shell commands; okay invoking gcc etc.
rem [3] requires LC_MESSAGES support to build; maybe 2.95.x update to
rem     cygwin provides this?
rem

rem ----------------------------------------------------------------------
rem   See if the environment is large enough.  We need 43 (?) bytes.
set $foo$=123456789_123456789_123456789_123456789_123
if not "%$foo$%" == "123456789_123456789_123456789_123456789_123" goto SmallEnv
set $foo$=

rem ----------------------------------------------------------------------
rem   Make sure we are running in the nt subdir
if exist configure.bat goto start
echo You must run configure from the nt subdirectory.
goto end

:start
rem ----------------------------------------------------------------------
rem   Default settings.
set prefix=
set nodebug=N
set noopt=N
set nocygwin=N
set COMPILER=
set usercflags=
set userldflags=
set sep1=
set sep2=

rem ----------------------------------------------------------------------
rem   Handle arguments.
:again
if "%1" == "-h" goto usage
if "%1" == "--help" goto usage
if "%1" == "--prefix" goto setprefix
if "%1" == "--with-gcc" goto withgcc
if "%1" == "--with-msvc" goto withmsvc
if "%1" == "--no-debug" goto nodebug
if "%1" == "--no-opt" goto noopt
if "%1" == "--no-cygwin" goto nocygwin
if "%1" == "--cflags" goto usercflags
if "%1" == "--ldflags" goto userldflags
if "%1" == "" goto checkutils
:usage
echo Usage: configure [options]
echo Options:
echo.   --prefix PREFIX         install Emacs in directory PREFIX
echo.   --with-gcc              use GCC to compile Emacs
echo.   --with-msvc             use MSVC to compile Emacs
echo.   --no-debug              exclude debug info from executables
echo.   --no-opt                disable optimization
echo.   --no-cygwin             use -mno-cygwin option with GCC
echo.   --cflags FLAG           pass FLAG to compiler
echo.   --ldflags FLAG          pass FLAG to compiler when linking
goto end
rem ----------------------------------------------------------------------
:setprefix
shift
set prefix=%1
shift
goto again
rem ----------------------------------------------------------------------
:withgcc
set COMPILER=gcc
shift
goto again
rem ----------------------------------------------------------------------
:withmsvc
set COMPILER=cl
shift
goto again
rem ----------------------------------------------------------------------
:nodebug
set nodebug=Y
shift
goto again
rem ----------------------------------------------------------------------
:noopt
set noopt=Y
shift
goto again
rem ----------------------------------------------------------------------
:nocygwin
set nocygwin=Y
shift
goto again
rem ----------------------------------------------------------------------
:usercflags
shift
set usercflags=%usercflags%%sep1%%1
set sep1= %nothing%
shift
goto again
rem ----------------------------------------------------------------------
:userldflags
shift
set userldflags=%userldflags%%sep2%%1
set sep2= %nothing%
shift
goto again

rem ----------------------------------------------------------------------
rem    Check that necessary utilities (cp and rm) are present.
:checkutils
echo Checking for 'cp'...
cp configure.bat junk.bat
if not exist junk.bat goto needcp
echo Checking for 'rm'...
rm junk.bat
if exist junk.bat goto needrm
goto checkcompiler
:needcp
echo You need 'cp' (the Unix file copy program) to build Emacs.
goto end
:needrm
del junk.bat
echo You need 'rm' (the Unix file delete program) to build Emacs.
goto end

rem ----------------------------------------------------------------------
rem   Auto-detect compiler if not specified, and validate GCC if chosen.
:checkcompiler
if (%COMPILER%)==(cl) goto genmakefiles
if (%COMPILER%)==(gcc) goto checkgcc

echo Checking whether 'cl' is available...
echo main(){} >junk.c
cl -nologo -c junk.c
if exist junk.obj goto clOK

echo Checking whether 'gcc' is available...
gcc -c junk.c
if not exist junk.o goto nocompiler
del junk.o

:checkgcc
if (%nocygwin%) == (Y) goto checkw32api
echo Checking whether gcc requires '-mno-cygwin'...
echo #include "cygwin/version.h" >junk.c
echo main(){} >>junk.c
gcc -c junk.c
if not exist junk.o goto checkw32api
gcc -mno-cygwin -c junk.c
if exist junk.o set nocygwin=Y
rm -f junk.c junk.o

:checkw32api
rem ----------------------------------------------------------------------
rem   Older versions of the Windows API headers either don't have any of
rem   the IMAGE_xxx definitions (the headers that come with Cygwin b20.1
rem   are like this), or have a typo in the definition of
rem   IMAGE_FIRST_SECTION (the headers with gcc/mingw32 2.95 have this
rem   problem).  The gcc/mingw32 2.95.2 headers are okay, as are distros
rem   of w32api-xxx.zip from Anders Norlander since 1999-11-18 at least.
rem
echo Checking whether W32 API headers are too old...
echo #include "windows.h" >junk.c
echo test(PIMAGE_NT_HEADERS pHeader)>>junk.c
echo {PIMAGE_SECTION_HEADER pSection = IMAGE_FIRST_SECTION(pHeader);}>>junk.c
gcc -c junk.c
if exist junk.o goto gccOk

:nocompiler
echo.
echo Configure failed.
echo To configure Emacs for Windows, you need to have either
echo gcc-2.95 or later with Mingw32 and the W32 API headers,
echo or MSVC 2.x or later.
del junk.c
goto end

:gccOk
set COMPILER=gcc
rm -f junk.c junk.o
echo Using 'gcc'
goto genmakefiles

:clOk
set COMPILER=cl
rm -f junk.c junk.obj
echo Using 'MSVC'
goto genmakefiles

rem ----------------------------------------------------------------------
:genmakefiles
echo Generating makefiles
if %COMPILER% == gcc set MAKECMD=gmake
if %COMPILER% == cl set MAKECMD=nmake

rem   Pass on chosen settings to makefiles.
echo # Start of settings from configure.bat >config.settings
echo COMPILER=%COMPILER% >>config.settings
if (%nodebug%) == (Y) echo NODEBUG=1>>config.settings
if (%noopt%) == (Y) echo NOOPT=1>>config.settings
if (%nocygwin%) == (Y) echo NOCYGWIN=1>>config.settings
if not "(%prefix%)" == "()" echo INSTALL_DIR=%prefix%>>config.settings
if not "(%usercflags%)" == "()" echo USER_CFLAGS=%usercflags%>>config.settings
if not "(%userldflags%)" == "()" echo USER_LDFLAGS=%userldflags%>>config.settings
echo # End of settings from configure.bat>>config.settings
echo. >>config.settings

copy config.nt ..\src\config.h
if not "(%usercflags%)" == "()" echo #define USER_CFLAGS " %usercflags%">>..\src\config.h
if not "(%userldflags%)" == "()" echo #define USER_LDFLAGS " %userldflags%">>..\src\config.h
copy paths.h ..\src\epaths.h

copy /b config.settings+%MAKECMD%.defs+..\nt\makefile.w32-in ..\nt\makefile
copy /b config.settings+%MAKECMD%.defs+..\lib-src\makefile.w32-in ..\lib-src\makefile
copy /b config.settings+%MAKECMD%.defs+..\src\makefile.w32-in ..\src\makefile
if not exist ..\lisp\Makefile.unix rename ..\lisp\Makefile.in Makefile.unix
if exist ..\lisp\makefile del /f ..\lisp\makefile
copy /b config.settings+%MAKECMD%.defs+..\lisp\makefile.w32-in ..\lisp\makefile
rem   Use the default (no-op) Makefile.in if the nt version is not present.
if exist ..\leim\makefile.w32-in copy /b config.settings+%MAKECMD%.defs+..\leim\makefile.w32-in ..\leim\makefile
if not exist ..\leim\makefile.w32-in copy /b config.settings+%MAKECMD%.defs+..\leim\Makefile.in ..\leim\makefile
del config.settings

echo.
echo Emacs successfully configured.
echo Run `%MAKECMD%' to build, then run `%MAKECMD% install' to install.
goto end

:SmallEnv
echo Your environment size is too small.  Please enlarge it and rerun configure.
echo For example, type "command.com /e:2048" to have 2048 bytes available.
set $foo$=
:end
set prefix=
set nodebug=
set noopt=
set nocygwin=
set COMPILER=
set MAKECMD=
set usercflags=
set userldflags=
