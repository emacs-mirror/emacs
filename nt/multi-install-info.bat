@echo off

rem  Hack to run install-info with multiple info files on the command
rem  line on the Windows platform.
rem
rem  Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
rem
rem  This file is part of GNU Emacs.
rem
rem  GNU Emacs is free software; you can redistribute it and/or modify
rem  it under the terms of the GNU General Public License as published by
rem  the Free Software Foundation; either version 2, or (at your option)
rem  any later version.
rem
rem  GNU Emacs is distributed in the hope that it will be useful,
rem  but WITHOUT ANY WARRANTY; without even the implied warranty of
rem  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem  GNU General Public License for more details.
rem
rem  You should have received a copy of the GNU General Public License
rem  along with GNU Emacs; see the file COPYING.  If not, write to
rem  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
rem  Boston, MA 02110-1301, USA.
rem
rem
rem  Usage:
rem   multi-install-info <switch passed to install-info> FILE1 FILE2 ...
rem
rem  By Peter 'Luna' Runestig <peter@runestig.com> 2003

set INSTALL_INFO=install-info
set II_SWITCH=%1=%2
rem Eat the install-info switch:
shift

:Loop
shift
if .%1% == . goto EndLoop
%INSTALL_INFO% %II_SWITCH% %1
goto Loop
:EndLoop

goto skipArchTag
   arch-tag: 4f590862-8ead-497a-a71c-fb4b0e5d50db
:skipArchTag
