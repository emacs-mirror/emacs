@echo off

rem Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
rem   Free Software Foundation, Inc.

rem Cannot use brackets in andrewi's email below because
rem older Windows shells will treat that as redirection.

rem Author: Andrew Innes andrewi@gnu.org

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


if (%3) == () goto usage
if not (%4) == () goto %4

:bin

echo Create full bin distribution
copy %3\README.W32 emacs-%1\README.W32
rem Info-ZIP zip seems to be broken on Windows.
rem It always writes to zip.zip and treats the zipfile argument as one
rem of the files to go in it.
rem zip -9 -r %2-bin-i386 emacs-%1/BUGS emacs-%1/COPYING emacs-%1/README emacs-%1/README.W32 emacs-%1/INSTALL emacs-%1/bin emacs-%1/etc emacs-%1/info emacs-%1/lisp emacs-%1/leim -x emacs.mdp *.pdb *.opt *~ CVS
7z a -tZIP -mx=9 -xr!emacs.mdp -xr!*.pdb -xr!*.opt -xr!*~ -xr!CVS -xr!.arch-inventory %2-bin-i386.zip emacs-%1/BUGS emacs-%1/COPYING emacs-%1/README emacs-%1/README.W32 emacs-%1/INSTALL emacs-%1/bin emacs-%1/etc emacs-%1/info emacs-%1/lisp emacs-%1/leim emacs-%1/site-lisp
del emacs-%1\README.W32
if not (%4) == () goto end

:barebin
echo Create archive with just the basic binaries and generated files
echo (the user needs to unpack the full source distribution for
echo  everything else)
copy %3\README.W32 emacs-%1\README.W32
copy %3\dump.bat emacs-%1\bin\dump.bat
rem Info-ZIP zip seems to be broken on Windows.
rem It always writes to zip.zip and treats the zipfile argument as one
rem of the files to go in it.
rem zip -9 -r %2-barebin-i386.zip emacs-%1/README.W32 emacs-%1/bin emacs-%1/etc/DOC-X emacs-%1/COPYING
7z a -tZIP -mx=9 %2-barebin-i386.zip emacs-%1/README.W32 emacs-%1/bin emacs-%1/etc/DOC-X emacs-%1/COPYING
del emacs-%1\README.W32
if not (%4) == () goto end

goto end

:usage
echo Generate source and binary distributions of emacs.
echo Usage: %0 emacs-version dist-basename distfiles [bin,barebin]
echo   (e.g., %0 19.34 emacs-19.34.5 d:\andrewi\distfiles)
:end

goto skipArchTag
   arch-tag: 6e2ddd92-c1c9-4992-b6b5-207aaab72f68
:skipArchTag
