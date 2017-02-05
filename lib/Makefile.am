# Copyright 2016-2017 Free Software Foundation, Inc.

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

# GNU Emacs assumes GNU Make.
AUTOMAKE_OPTIONS = -Wno-portability

BUILT_SOURCES =
CLEANFILES =
EXTRA_DIST =
MOSTLYCLEANDIRS =
MOSTLYCLEANFILES =
noinst_LIBRARIES =
SUFFIXES =

AM_CFLAGS = $(PROFILING_CFLAGS) $(GNULIB_WARN_CFLAGS) $(WERROR_CFLAGS)
DEFAULT_INCLUDES = -I. -I$(top_srcdir)/lib -I../src -I$(top_srcdir)/src

if BUILDING_FOR_WINDOWSNT
include ../nt/gnulib.mk
else
include gnulib.mk

libgnu_a_SOURCES += openat-die.c save-cwd.c
endif

if HYBRID_MALLOC_LIB
noinst_LIBRARIES += libegnu.a
endif

libegnu_a_SOURCES = $(libgnu_a_SOURCES)
libegnu_a_LIBADD = $(patsubst %.o,e-%.o,$(libgnu_a_LIBADD))
EXTRA_libegnu_a_SOURCES = $(EXTRA_libgnu_a_SOURCES)
libegnu_a_SHORTNAME = e
libegnu_a_CPPFLAGS = -Demacs
MOSTLYCLEANFILES += libegnu.a

.PHONY: bootstrap-clean

bootstrap-clean: maintainer-clean
