@echo off

rem Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
rem   Free Software Foundation, Inc.
rem
rem Cannot use brackets in andrewi's email below because
rem older Windows shells will treat that as redirection.
rem
rem Author: Andrew Innes andrewi@gnu.org
rem
rem This file is part of GNU Emacs.
rem
rem GNU Emacs is free software; you can redistribute it and/or modify
rem it under the terms of the GNU General Public License as published by
rem the Free Software Foundation; either version 2, or (at your option)
rem any later version.
rem
rem GNU Emacs is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem GNU General Public License for more details.
rem
rem You should have received a copy of the GNU General Public License
rem along with GNU Emacs; see the file COPYING.  If not, write to the
rem Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
rem Boston, MA 02110-1301, USA.

set ZIP=zip

if (%3) == () goto usage
if not (%4) == () goto %4

:bin

echo Create full bin distribution
copy %3\README.W32 emacs-%1\README.W32

%ZIP% -x emacs.mdp -x *.pdb -x *.opt -x *~ -x CVS -9 emacs-%1/BUGS emacs-%1/README emacs-%1/README.W32 emacs-%1/bin emacs-%1/etc emacs-%1/info emacs-%1/lisp %2-bin-i386.zip
del emacs-%1\README.W32
if not (%4) == () goto end

:barebin

echo Create archive with just the basic binaries and generated files
echo (the user needs to unpack the full source distribution for
echo  everything else)
copy %3\README.W32 emacs-%1\README.W32
%ZIP% -9 emacs-%1/README.W32 emacs-%1/bin emacs-%1/etc/DOC emacs-%1/etc/DOC-X %2-barebin-i386.zip
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
