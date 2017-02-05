# Test GNU Emacs modules.

# Copyright 2015-2017 Free Software Foundation, Inc.

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
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

ROOT = ../..
EMACS = $(ROOT)/src/emacs

CC      = gcc
LD      = gcc
LDFLAGS =

# On MS-Windows, say "make SO=dll" to build the module
SO      = so
# -fPIC is a no-op on Windows, but causes a compiler warning
ifeq ($(SO),dll)
CFLAGS  = -std=gnu99 -ggdb3 -Wall
else
CFLAGS  = -std=gnu99 -ggdb3 -Wall -fPIC
endif

all: mod-test.$(SO)

%.$(SO): %.o
	$(LD) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -I$(ROOT)/src -c $<

check:
	$(EMACS) -batch -l ert -l test.el -f ert-run-tests-batch-and-exit
