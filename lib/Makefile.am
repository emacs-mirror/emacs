BUILT_SOURCES =
EXTRA_DIST =
MOSTLYCLEANDIRS =
MOSTLYCLEANFILES =
noinst_LIBRARIES =

AM_CFLAGS = $(PROFILING_CFLAGS) $(GNULIB_WARN_CFLAGS) $(WERROR_CFLAGS)
DEFAULT_INCLUDES = -I. -I$(top_srcdir)/lib -I../src -I$(top_srcdir)/src

include gnulib.mk

libgnu_a_SOURCES += openat-die.c save-cwd.c
