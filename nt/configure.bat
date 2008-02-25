@echo off
rem   ----------------------------------------------------------------------
rem   Configuration script for MS Windows 95/98/Me and NT/2000/XP
rem   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005,
rem      2006, 2007, 2008 Free Software Foundation, Inc.

rem   This file is part of GNU Emacs.

rem   GNU Emacs is free software; you can redistribute it and/or modify
rem   it under the terms of the GNU General Public License as published by
rem   the Free Software Foundation; either version 3, or (at your option)
rem   any later version.

rem   GNU Emacs is distributed in the hope that it will be useful,
rem   but WITHOUT ANY WARRANTY; without even the implied warranty of
rem   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem   GNU General Public License for more details.

rem   You should have received a copy of the GNU General Public License
rem   along with GNU Emacs; see the file COPYING.  If not, write to the
rem   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
rem   Boston, MA 02110-1301, USA.
rem   ----------------------------------------------------------------------
rem   YOU'LL NEED THE FOLLOWING UTILITIES TO MAKE EMACS:
rem
rem   + MS Windows 95/98/Me or NT/2000/XP
rem   + either MSVC 2.x or later, or gcc-2.95 or later (with gmake 3.75
rem     or later) and the Mingw32 and W32 API headers and libraries.
rem   + Visual Studio 2005 is not supported at this time.
rem
rem For reference, here is a list of which builds of gmake are known to
rem work or not, and whether they work in the presence and/or absence of
rem sh.exe.
rem
rem                                       sh exists     no sh
rem  cygwin b20.1 make (3.75):            fails[1,5]    fails[2,5]
rem  MSVC compiled gmake 3.77:            okay          okay
rem  MSVC compiled gmake 3.78.1:          okay          okay
rem  MSVC compiled gmake 3.79.1:          okay          okay
rem  mingw32/gcc-2.92.2 make (3.77):      okay          okay[4]
rem  cygwin compiled gmake 3.77:          fails[1,5]    fails[2,5]
rem  cygwin compiled gmake 3.78.1:        fails[5]      fails[2,5]
rem  cygwin compiled gmake 3.79.1:        fails[3,5]    fails[2?,5]
rem  cygwin compiled make 3.80:           okay[6]       fails?[7]
rem  cygwin compiled make 3.81:           fails         fails?[7]
rem  mingw32 compiled make 3.79.1:        okay          okay
rem  mingw32 compiled make 3.80:          okay          okay?[7]
rem  mingw32 compiled make 3.81:          okay          okay[8]
rem
rem [1] doesn't cope with makefiles with DOS line endings, so must mount
rem     emacs source with text!=binary.
rem [2] fails when needs to invoke shell commands; okay invoking gcc etc.
rem [3] requires LC_MESSAGES support to build; cannot build with early
rem     versions of cygwin.
rem [4] may fail on Windows 9X and Windows ME; if so, install Bash.
rem [5] fails when building leim due to the use of cygwin style paths.
rem     May work if building emacs without leim.
rem [6] need to uncomment 3 lines in nt/gmake.defs that invoke `cygpath';
rem    	look for "cygpath" near line 85 of gmake.defs.
rem [7] not recommended; please report if you try this combination.
rem [8] tested only on Windows XP.
rem

if exist config.log del config.log

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
set docflags=
set userldflags=
set doldflags=
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
if "%1" == "--without-png" goto withoutpng
if "%1" == "--without-jpeg" goto withoutjpeg
if "%1" == "--without-gif" goto withoutgif
if "%1" == "--without-tiff" goto withouttiff
if "%1" == "--without-xpm" goto withoutxpm
if "%1" == "--enable-font-backend" goto withfont
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
echo.   --without-png           do not use PNG library even if it is installed
echo.   --without-jpeg          do not use JPEG library even if it is installed
echo.   --without-gif           do not use GIF library even if it is installed
echo.   --without-tiff          do not use TIFF library even if it is installed
echo.   --without-xpm           do not use XPM library even if it is installed
echo.   --enable-font-backend   build with font backend support
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

:withoutpng
set pngsupport=N
set HAVE_PNG=
shift
goto again

rem ----------------------------------------------------------------------

:withoutjpeg
set jpegsupport=N
set HAVE_JPEG=
shift
goto again

rem ----------------------------------------------------------------------

:withoutgif
set gifsupport=N
set HAVE_GIF=
shift
goto again

rem ----------------------------------------------------------------------

:withouttiff
set tiffsupport=N
set HAVE_TIFF=
shift
goto again

rem ----------------------------------------------------------------------

:withoutxpm
set xpmsupport=N
set HAVE_XPM=
shift
goto again

:withfont
set usercflags=%usercflags%%sep1%-DUSE_FONT_BACKEND
set sep1= %nothing%
set usefontbackend=Y
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
if (%COMPILER%)==(cl) goto compilercheckdone
if (%COMPILER%)==(gcc) goto checkgcc

echo Checking whether 'gcc' is available...
echo main(){} >junk.c
gcc -c junk.c
if exist junk.o goto checkgcc

echo Checking whether 'cl' is available...
cl -nologo -c junk.c
if exist junk.obj goto clOK
goto nocompiler

:checkgcc
if exist junk.o del junk.o
Rem WARNING -- COMMAND.COM on some systems only looks at the first
Rem            8 characters of a label.  So do NOT be tempted to change
Rem            chkapi* into something fancier like checkw32api
Rem You HAVE been warned!
if (%nocygwin%) == (Y) goto chkapiN
echo Checking whether gcc requires '-mno-cygwin'...
echo #include "cygwin/version.h" >junk.c
echo main(){} >>junk.c
echo gcc -c junk.c >>config.log
gcc -c junk.c >>config.log 2>&1
if not exist junk.o goto chkapi
echo gcc -mno-cygwin -c junk.c >>config.log
gcc -mno-cygwin -c junk.c >>config.log 2>&1
if exist junk.o set nocygwin=Y

:chkapi
echo The failed program was: >>config.log
type junk.c >>config.log
:chkapiN
rm -f junk.c junk.o
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
echo test(PIMAGE_NT_HEADERS pHeader) >>junk.c
echo {PIMAGE_SECTION_HEADER pSection = IMAGE_FIRST_SECTION(pHeader);} >>junk.c
if (%nocygwin%) == (Y) goto chkapi1
set cf=%usercflags%
goto chkapi2
:chkapi1
set cf=%usercflags% -mno-cygwin
:chkapi2
echo on
gcc %cf% -c junk.c
@echo off
@echo gcc %cf% -c junk.c >>config.log
gcc %cf% -c junk.c >>config.log 2>&1
set cf=
if exist junk.o goto gccOk
echo The failed program was: >>config.log
type junk.c >>config.log

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
echo Using 'gcc'
rm -f junk.c junk.o
Rem It is not clear what GCC version began supporting -mtune
Rem and pentium4 on x86, so check this explicitly.
echo main(){} >junk.c
echo gcc -c -O2 -mtune=pentium4 junk.c >>config.log
gcc -c -O2 -mtune=pentium4 junk.c >>config.log 2>&1
if not errorlevel 1 goto gccMtuneOk
echo The failed program was: >>config.log
type junk.c >>config.log
set mf=-mcpu=i686
rm -f junk.c junk.o
goto compilercheckdone
:gccMtuneOk
echo GCC supports -mtune=pentium4 >>config.log
set mf=-mtune=pentium4
rm -f junk.c junk.o
goto compilercheckdone

:clOk
set COMPILER=cl
rm -f junk.c junk.obj
echo Using 'MSVC'

:compilercheckdone

rem ----------------------------------------------------------------------
rem   Check for external image libraries. Since they are loaded
rem   dynamically, the libraries themselves do not need to be present
rem   at compile time, but the header files are required.

set mingwflag=

if (%nocygwin%) == (N) goto flagsOK
set mingwflag=-mno-cygwin

:flagsOK

if (%pngsupport%) == (N) goto pngDone

echo Checking for libpng...
echo #include "png.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto havePng

echo ...png.h not found, building without PNG support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_PNG=
goto :pngDone

:havePng
echo ...PNG header available, building with PNG support.
set HAVE_PNG=1

:pngDone
rm -f junk.c junk.obj

if (%jpegsupport%) == (N) goto jpegDone

echo Checking for jpeg-6b...
echo #include "jconfig.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveJpeg

echo ...jconfig.h not found, building without JPEG support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_JPEG=
goto :jpegDone

:haveJpeg
echo ...JPEG header available, building with JPEG support.
set HAVE_JPEG=1

:jpegDone
rm -f junk.c junk.obj

if (%gifsupport%) == (N) goto gifDone

echo Checking for libgif...
echo #include "gif_lib.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveGif

echo ...gif_lib.h not found, building without GIF support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_GIF=
goto :gifDone

:haveGif
echo ...GIF header available, building with GIF support.
set HAVE_GIF=1

:gifDone
rm -f junk.c junk.obj

if (%tiffsupport%) == (N) goto tiffDone

echo Checking for tiff...
echo #include "tiffio.h" >junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveTiff

echo ...tiffio.h not found, building without TIFF support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_TIFF=
goto :tiffDone

:haveTiff
echo ...TIFF header available, building with TIFF support.
set HAVE_TIFF=1

:tiffDone
rm -f junk.c junk.obj

if (%xpmsupport%) == (N) goto xpmDone

echo Checking for libXpm...
echo #define FOR_MSW 1 >junk.c
echo #include "X11/xpm.h" >>junk.c
echo main (){} >>junk.c
rem   -o option is ignored with cl, but allows result to be consistent.
echo %COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >>config.log
%COMPILER% %usercflags% %mingwflag% -c junk.c -o junk.obj >junk.out 2>>config.log
if exist junk.obj goto haveXpm

echo ...X11/xpm.h not found, building without XPM support.
echo The failed program was: >>config.log
type junk.c >>config.log
set HAVE_XPM=
goto :xpmDone

:haveXpm
echo ...XPM header available, building with XPM support.
set HAVE_XPM=1

:xpmDone
rm -f junk.c junk.obj junk.err junk.out

rem ----------------------------------------------------------------------
:genmakefiles
echo Generating makefiles
if %COMPILER% == gcc set MAKECMD=gmake
if %COMPILER% == cl set MAKECMD=nmake

rem   Pass on chosen settings to makefiles.
rem   NB. Be very careful to not have a space before redirection symbols
rem   except when there is a preceding digit, when a space is required.
rem
echo # Start of settings from configure.bat >config.settings
echo COMPILER=%COMPILER%>>config.settings
if not "(%mf%)" == "()" echo MCPU_FLAG=%mf%>>config.settings
if (%nodebug%) == (Y) echo NODEBUG=1 >>config.settings
if (%noopt%) == (Y) echo NOOPT=1 >>config.settings
if (%nocygwin%) == (Y) echo NOCYGWIN=1 >>config.settings
if not "(%prefix%)" == "()" echo INSTALL_DIR=%prefix%>>config.settings
rem We go thru docflags because usercflags could be "-DFOO=bar" -something
rem and the if command cannot cope with this
for %%v in (%usercflags%) do if not (%%v)==() set docflags=Y
if (%docflags%)==(Y) echo USER_CFLAGS=%usercflags%>>config.settings
for %%v in (%userldflags%) do if not (%%v)==() set doldflags=Y
if (%doldflags%)==(Y) echo USER_LDFLAGS=%userldflags%>>config.settings
if (%usefontbackend%) == (Y) echo USE_FONTBACKEND=1 >>config.settings
echo # End of settings from configure.bat>>config.settings
echo. >>config.settings

copy config.nt config.tmp
echo. >>config.tmp
echo /* Start of settings from configure.bat.  */ >>config.tmp
if (%docflags%) == (Y) echo #define USER_CFLAGS " %usercflags%">>config.tmp
if (%doldflags%) == (Y) echo #define USER_LDFLAGS " %userldflags%">>config.tmp
if not "(%HAVE_PNG%)" == "()" echo #define HAVE_PNG 1 >>config.tmp
if not "(%HAVE_JPEG%)" == "()" echo #define HAVE_JPEG 1 >>config.tmp
if not "(%HAVE_GIF%)" == "()" echo #define HAVE_GIF 1 >>config.tmp
if not "(%HAVE_TIFF%)" == "()" echo #define HAVE_TIFF 1 >>config.tmp
if not "(%HAVE_XPM%)" == "()" echo #define HAVE_XPM 1 >>config.tmp
echo /* End of settings from configure.bat.  */ >>config.tmp

Rem See if fc.exe returns a meaningful exit status.  If it does, we
Rem might as well avoid unnecessary overwriting of config.h and epaths.h,
Rem since this forces recompilation of every source file.
if exist foo.bar del foo.bar
fc /b foo.bar foo.bar >nul 2>&1
if not errorlevel 2 goto doCopy
fc /b config.tmp ..\src\config.h >nul 2>&1
if errorlevel 1 goto doCopy
fc /b paths.h ..\src\epaths.h >nul 2>&1
if errorlevel 0 goto dontCopy
:doCopy
copy config.tmp ..\src\config.h
copy paths.h ..\src\epaths.h

:dontCopy
if exist config.tmp del config.tmp
copy /b config.settings+%MAKECMD%.defs+..\nt\makefile.w32-in ..\nt\makefile
if exist ..\admin\unidata copy /b config.settings+%MAKECMD%.defs+..\admin\unidata\makefile.w32-in ..\admin\unidata\makefile
copy /b config.settings+%MAKECMD%.defs+..\lib-src\makefile.w32-in ..\lib-src\makefile
copy /b config.settings+%MAKECMD%.defs+..\src\makefile.w32-in ..\src\makefile
copy /b config.settings+%MAKECMD%.defs+..\doc\emacs\makefile.w32-in ..\doc\emacs\makefile
copy /b config.settings+%MAKECMD%.defs+..\doc\misc\makefile.w32-in ..\doc\misc\makefile
copy /b config.settings+%MAKECMD%.defs+..\doc\lispref\makefile.w32-in ..\doc\lispref\makefile
copy /b config.settings+%MAKECMD%.defs+..\doc\lispintro\makefile.w32-in ..\doc\lispintro\makefile
if exist ..\lisp\makefile rm -f ../lisp/[Mm]akefile
copy /b config.settings+%MAKECMD%.defs+..\lisp\makefile.w32-in ..\lisp\makefile
rem   Use the default (no-op) Makefile.in if the nt version is not present.
if exist ..\leim\makefile.w32-in copy /b config.settings+%MAKECMD%.defs+..\leim\makefile.w32-in ..\leim\makefile
if not exist ..\leim\makefile.w32-in copy /b config.settings+%MAKECMD%.defs+..\leim\Makefile.in ..\leim\makefile
del config.settings

Rem Some people use WinZip which doesn't create empty directories!
if not exist ..\site-lisp\nul mkdir ..\site-lisp\
Rem Update subdirs.el only if it is different or fc.exe doesn't work.
if exist foo.bar del foo.bar
fc /b foo.bar foo.bar >nul 2>&1
if not errorlevel 2 goto doUpdateSubdirs
fc /b subdirs.el ..\site-lisp\subdirs.el >nul 2>&1
if not errorlevel 1 goto dontUpdateSubdirs
:doUpdateSubdirs
if exist ..\site-lisp\subdirs.el del ..\site-lisp\subdirs.el
copy subdirs.el ..\site-lisp\subdirs.el

:dontUpdateSubdirs
echo.

rem check that we have all the libraries we need.
set libsOK=1

if not "(%HAVE_XPM%)" == "()" goto checkpng
if (%xpmsupport%) == (N) goto checkpng
 set libsOK=0
 echo XPM support is missing. It is required for color icons in the toolbar.
 echo   Install libXpm development files or use --without-xpm

:checkpng
if not "(%HAVE_PNG%)" == "()" goto checkjpeg
if (%pngsupport%) == (N) goto checkjpeg
 set libsOK=0
 echo PNG support is missing.
 echo   Install libpng development files or use --without-png

:checkjpeg
if not "(%HAVE_JPEG%)" == "()" goto checktiff
if (%jpegsupport%) == (N) goto checktiff
 set libsOK=0
 echo JPEG support is missing.
 echo   Install jpeg development files or use --without-jpeg

:checktiff
if not "(%HAVE_TIFF%)" == "()" goto checkgif
if (%tiffsupport%) == (N) goto checkgif
 set libsOK=0
 echo TIFF support is missing.
 echo   Install libtiff development files or use --without-tiff

:checkgif
if not "(%HAVE_GIF%)" == "()" goto donelibchecks
if (%gifsupport%) == (N) goto donelibchecks
 set libsOK=0
 echo GIF support is missing.
 echo   Install giflib or libungif development files or use --without-gif

:donelibchecks
if (%libsOK%) == (1) goto success
echo.
echo Important libraries are missing. Fix these issues before running make.
goto end

:success
echo Emacs successfully configured.
echo Emacs successfully configured. >>config.log
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
set docflags=
set userldflags=
set doldflags=
set mingwflag=
set mf=

goto skipArchTag
   arch-tag: 300d20a4-1675-4e75-b615-7ce1a8c5376c
:skipArchTag
