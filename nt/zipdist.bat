@echo off
rem Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
rem   Free Software Foundation, Inc.

rem Author: Christoph Scholtes cschol2112 at gmail.com

rem This file is part of GNU Emacs.

rem GNU Emacs is free software: you can redistribute it and/or modify
rem it under the terms of the GNU General Public License as published by
rem the Free Software Foundation, either version 3 of the License, or
rem (at your option) any later version.

rem GNU Emacs is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem GNU General Public License for more details.

rem You should have received a copy of the GNU General Public License
rem along with GNU Emacs.  If not, see http://www.gnu.org/licenses/.

SETLOCAL
rem arg 1: full path to Emacs root directory
set ARG_PATH="%~f1"
rem Path separator cannot be parsed correctly, substitute
set ARG_PATH=%ARG_PATH:\=;%

rem arg 2: Emacs version number
set EMACS_VER=%2

rem Parse out last directory from passed in full path (arg 1)
for /f "tokens=* delims=;" %%G in (%ARG_PATH%) do call :PARSE_PATH %%G
goto :EXIT

:PARSE_PATH
if "%1"=="" (
  goto :ZIP_CHECK
)
set ROOT_DIR=%1
SHIFT
goto :PARSE_PATH

rem Check, if 7zip is installed and available on path
:ZIP_CHECK
7z
if %ERRORLEVEL% NEQ 0 goto :ZIP_ERROR
goto ZIP_DIST

:ZIP_ERROR
echo.
echo ERROR: Make sure 7zip is installed and available on the Windows Path!
goto EXIT

rem Build distributions
:ZIP_DIST
pushd ..\..
rem Build and verify full distribution
7z a -bd -tZIP -mx=9 -x!.bzrignore -x!.gitignore -xr!emacs.mdp -xr!*.pdb -xr!*.opt -xr!*~ -xr!CVS -xr!.arch-inventory emacs-%EMACS_VER%-bin-i386.zip %ROOT_DIR%/BUGS %ROOT_DIR%/COPYING %ROOT_DIR%/README %ROOT_DIR%/README.W32 %ROOT_DIR%/INSTALL %ROOT_DIR%/bin %ROOT_DIR%/etc %ROOT_DIR%/info %ROOT_DIR%/lisp %ROOT_DIR%/leim %ROOT_DIR%/site-lisp
7z t emacs-%EMACS_VER%-bin-i386.zip
rem Build and verify binary only distribution
7z a -bd -tZIP -mx=9 emacs-%EMACS_VER%-barebin-i386.zip %ROOT_DIR%/README.W32 %ROOT_DIR%/bin %ROOT_DIR%/etc/DOC-X %ROOT_DIR%/COPYING
7z t emacs-%EMACS_VER%-barebin-i386.zip
popd
goto EXIT

:EXIT
