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

.PHONY: bootstrap-clean

bootstrap-clean: maintainer-clean
