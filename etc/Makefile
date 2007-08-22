### Makefile for Emacs etc/ directory

## Copyright (C) 1993, 1994, 2001, 2002, 2003, 2004, 2005, 2006,
##   2007 Free Software Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs; see the file COPYING.  If not, write to the
## Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301, USA.


.PHONY: mostlyclean clean distclean maintainer-clean unlock relock


mostlyclean clean distclean maintainer-clean:
	-rm -f DOC*

SOURCES = [0-9A-QS-Z]* README *.[ch16] emacs.* ms-* \
	spook-lines termcap.*

unlock:
	chmod u+w $(SOURCES)

relock:
	chmod u-w $(SOURCES)

# ${etcdir}/e/eterm-color is used by ../lisp/term.el.
# TERMINFO systems use terminfo files compiled by the Terminfo Compiler (tic).
# These files are binary, and depend on the version of tic, but they seem
# to be system-independent and backwardly compatible.
# So there should be no need to recompile the distributed binary version.
TIC=tic
e/eterm-color: e/eterm-color.ti
	TERMINFO=`pwd`; export TERMINFO; $(TIC) e/eterm-color.ti


## arch-tag: 4261f003-cf77-4478-a10a-5284e9d8f797
### Makefile ends here
