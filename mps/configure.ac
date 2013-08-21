# configure.ac -- autoconf configuration for the MPS    -*- Autoconf -*-
#
# $Id$
# Copyright (C) 2012-2013 Ravenbrook Limited.  See end of file for license.
#
# YOU DON'T NEED AUTOCONF TO BUILD THE MPS
# This is just here for people who want or expect a configure script.
# See [Building the Memory Pool System](manual/build.txt) for how best
# to build and integrate the MPS.
#
# Generate the configure script with
#
#     autoreconf -vif
#

AC_PREREQ([2.50])
# http://www.gnu.org/savannah-checkouts/gnu/autoconf/manual/autoconf-2.69/html_node/Initializing-configure.html#Initializing-configure
AC_INIT([Memory Pool System Kit],
        [master],
        [mps-questions@ravenbrook.com],
        [mps-kit],
        [http://www.ravenbrook.com/project/mps/])
AC_CONFIG_AUX_DIR(tool/autoconf/build-aux)
AC_CONFIG_SRCDIR([code/mps.c])

# Checks for programs.
AC_PROG_CC
AC_LANG_C
AC_PROG_INSTALL

# Check for Clang/LLVM.
AC_CHECK_DECL([__clang__],[CLANG=yes],[CLANG=no])

# These flags aren't used for building the MPS, but for sample programs.
CFLAGS_GC="-ansi -pedantic -Wall -Werror -Wpointer-arith \
           -Wstrict-prototypes -Wmissing-prototypes \
           -Winline -Waggregate-return -Wnested-externs \
           -Wcast-qual -Wstrict-aliasing=2 -O -g3 -pthread"
CFLAGS_LL="$CFLAGS_GC -Wno-extended-offsetof"

AC_CANONICAL_HOST
AC_MSG_CHECKING([target platform])
BUILD_TARGET=build-via-make
CLEAN_TARGET=clean-make-build
INSTALL_TARGET=install-make-build
TEST_TARGET=test-make-build
case $host/$CLANG in
  i*86-*-linux*/no)
      AC_MSG_RESULT([Linux x86])
      MPS_TARGET_NAME=lii3gc
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  x86_64-*-linux*/no)
      AC_MSG_RESULT([Linux x86_64])
      MPS_TARGET_NAME=lii6gc
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  i*86-*-darwin*/*)
      AC_MSG_RESULT([Mac OS X x86])
      MPS_TARGET_NAME=xci3ll
      BUILD_TARGET=build-via-xcode
      CLEAN_TARGET=clean-xcode-build
      INSTALL_TARGET=install-xcode-build
      TEST_TARGET=test-xcode-build
      PFMCFLAGS="$CFLAGS_LL"
    ;;
  x86_64-apple-darwin*/*)
      AC_MSG_RESULT([Mac OS X x86_64])
      MPS_TARGET_NAME=xci6ll
      BUILD_TARGET=build-via-xcode
      CLEAN_TARGET=clean-xcode-build
      INSTALL_TARGET=install-xcode-build
      TEST_TARGET=test-xcode-build
      PFMCFLAGS="$CFLAGS_LL"
    ;;
  i*86-*-freebsd*/no)
      AC_MSG_RESULT([FreeBSD x86])
      MPS_TARGET_NAME=fri3gc
      # Need /usr/local/include in order to find sqlite3.h
      CFLAGS="-I/usr/local/include"
      CPP="$CC -I/usr/local/include -E"
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  amd64-*-freebsd*/no | x86_64-*-freebsd*/no)
      AC_MSG_RESULT([FreeBSD x86_64])
      MPS_TARGET_NAME=fri6gc
      # Need /usr/local/include in order to find sqlite3.h
      CFLAGS="-I/usr/local/include"
      CPP="$CC -I/usr/local/include -E"
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  *)
    AC_MSG_ERROR([MPS does not support this platform out of the box.  See manual/build.txt])
esac

AC_CHECK_PROGS([MAKE],[gnumake gmake make],[AC_MSG_ERROR([Unable to find a make program.])])
if ! $MAKE --version | grep -q "GNU" 2> /dev/null; then
  AC_MSG_ERROR([MPS requires GNU make to build from configure, but see manual/build.txt])
fi

EXTRA_TARGETS="mpseventcnv mpseventtxt"
AC_CHECK_HEADER([sqlite3.h], [EXTRA_TARGETS="$EXTRA_TARGETS mpseventsql"])

# Put platform compiler flags like -ansi -pedantic into CFLAGS only
# after checking for sqlite3.h -- that header doesn't compile with
# those flags.
CFLAGS="$CFLAGS $PFMCFLAGS"

AC_SUBST(MPS_TARGET_NAME)
AC_SUBST(BUILD_TARGET)
AC_SUBST(CLEAN_TARGET)
AC_SUBST(INSTALL_TARGET)
AC_SUBST(TEST_TARGET)
AC_SUBST(EXTRA_TARGETS)
AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(CPPFLAGS)
AC_CONFIG_FILES(Makefile example/scheme/Makefile)

AC_OUTPUT

echo 1>&2 "CONFIGURE/MAKE IS NOT THE BEST WAY TO BUILD THE MPS -- see <manual/build.txt>"
