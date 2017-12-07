# test.make --- Semantic unit test for Make -*- makefile -*-

# Copyright (C) 2001-2002, 2010-2017 Free Software Foundation, Inc.

# Author: Eric M. Ludlam <eric@siege-engine.com>

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

top=
ede_FILES=Project.ede Makefile

example_MISC=semantic-skel.el skeleton.bnf
init_LISP=semantic-load.el
DISTDIR=$(top)semantic-$(VERSION)

# really goofy & variables tabs
A=      B
A       =B
A=B     C
A=B\
        C

A=	http://${B} \
	ftp://${B}
B=	test

all: example semantic Languages tools senator semantic.info

test ${B}: foo bar
	@echo ${A}

example:
	@

init: $(init_LISP)
	@echo "(add-to-list 'load-path nil)" > $@-compile-script
	@if test ! -z "${LOADPATH}" ; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> $@-compile-script; \
	    done;\
	fi
	@echo "(setq debug-on-error t)" >> $@-compile-script
	$(EMACS) -batch -l $@-compile-script -f batch-byte-compile $^

include tesset.mk tusset.mk
include oneset.mk

ifdef SOME_SYMBOL
  VAR1 = foo
else
  VAR1 = bar
endif

ifndef SOME_OTHER_SYMBOL
  VAR1 = baz
endif

ifeq ($(VAR1), foo)
  VAR2 = gleep
else
  ifneq ($(VAR1), foo)
    VAR2 = glop
  endif
endif

# End of Makefile
