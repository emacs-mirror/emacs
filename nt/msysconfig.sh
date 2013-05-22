#! /bin/sh
### msysconfig.sh - Run the top-level 'configure' script as appropriate
### for the MinGW/MSYS build of a native MS-Windows port of Emacs.

## Copyright (C) 2013 Free Software Foundation, Inc.

## Author: Eli Zaretskii <eliz@gnu.org>

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## The Posix 'configure' script needs a few tweaks to produce desired
## results when running under MSYS with the purpose of configuring
## Emacs for the MinGW MS-Windows build.  Rather than asking users to
## type these tweaks every time they configure the package, we provide
## this helper script which takes care of the mundane things.

### Code:

srcdir=`dirname "$0"`
parent=`dirname "$srcdir"`

## The nt/mingw-cfg.site file provides various autoconf variables that
## are needed for a successful MinGW build.
CONFIG_SITE="$srcdir/mingw-cfg.site" $parent/configure "$@"
