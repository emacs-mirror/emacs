# configure.ac -- autoconf configuration for the MPS    -*- Autoconf -*-
#
# $Id$
# Copyright (C) 2012-2018 Ravenbrook Limited.  See end of file for license.
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
# http://www.gnu.org/software/autoconf/manual/autoconf.html#Initializing-configure
AC_INIT([Memory Pool System Kit],
        m4_esyscmd_s([sed -n '/^#define MPS_RELEASE "\(.*\)"$/{s/.*"\(.*\)"/\1/;p;}' code/version.c]),
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
      MPS_OS_NAME=li
      MPS_ARCH_NAME=i3
      MPS_BUILD_NAME=gc
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  x86_64-*-linux*/no)
      AC_MSG_RESULT([Linux x86_64])
      MPS_OS_NAME=li
      MPS_ARCH_NAME=i6
      MPS_BUILD_NAME=gc
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  x86_64-*-linux*/yes)
      AC_MSG_RESULT([Linux x86_64])
      MPS_OS_NAME=li
      MPS_ARCH_NAME=i6
      MPS_BUILD_NAME=ll
      PFMCFLAGS="$CFLAGS_LL"
    ;;
  i*86-*-darwin*/*)
      AC_MSG_RESULT([Mac OS X x86])
      MPS_OS_NAME=xc
      MPS_ARCH_NAME=i3
      MPS_BUILD_NAME=ll
      BUILD_TARGET=build-via-xcode
      CLEAN_TARGET=clean-xcode-build
      INSTALL_TARGET=install-xcode-build
      TEST_TARGET=test-xcode-build
      PFMCFLAGS="$CFLAGS_LL"
    ;;
  x86_64-apple-darwin*/*)
      AC_MSG_RESULT([Mac OS X x86_64])
      MPS_OS_NAME=xc
      MPS_ARCH_NAME=i6
      MPS_BUILD_NAME=ll
      BUILD_TARGET=build-via-xcode
      CLEAN_TARGET=clean-xcode-build
      INSTALL_TARGET=install-xcode-build
      TEST_TARGET=test-xcode-build
      PFMCFLAGS="$CFLAGS_LL"
    ;;
  i*86-*-freebsd*/no)
      AC_MSG_RESULT([FreeBSD x86])
      MPS_OS_NAME=fr
      MPS_ARCH_NAME=i3
      MPS_BUILD_NAME=gc
      # Need /usr/local/include in order to find sqlite3.h
      CFLAGS="-I/usr/local/include"
      CPP="$CC -I/usr/local/include -E"
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  amd64-*-freebsd*/yes | x86_64-*-freebsd*/yes)
      AC_MSG_RESULT([FreeBSD x86_64])
      MPS_OS_NAME=fr
      MPS_ARCH_NAME=i6
      MPS_BUILD_NAME=ll
      # Need /usr/local/include in order to find sqlite3.h
      CFLAGS="-I/usr/local/include"
      CPP="$CC -I/usr/local/include -E"
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  amd64-*-freebsd*/no | x86_64-*-freebsd*/no)
      AC_MSG_RESULT([FreeBSD x86_64])
      MPS_OS_NAME=fr
      MPS_ARCH_NAME=i6
      MPS_BUILD_NAME=gc
      # Need /usr/local/include in order to find sqlite3.h
      CFLAGS="-I/usr/local/include"
      CPP="$CC -I/usr/local/include -E"
      PFMCFLAGS="$CFLAGS_GC"
    ;;
  i*86-*-freebsd*/yes)
      AC_MSG_RESULT([FreeBSD x86])
      MPS_OS_NAME=fr
      MPS_ARCH_NAME=i3
      MPS_BUILD_NAME=ll
      # Need /usr/local/include in order to find sqlite3.h
      CFLAGS="-I/usr/local/include"
      CPP="$CC -I/usr/local/include -E"
      PFMCFLAGS="$CFLAGS_LL"
    ;;
  amd64-*-freebsd*/no | x86_64-*-freebsd*/no)
      AC_MSG_RESULT([FreeBSD x86_64])
      MPS_OS_NAME=fr
      MPS_ARCH_NAME=i6
      MPS_BUILD_NAME=ll
      # Need /usr/local/include in order to find sqlite3.h
      CFLAGS="-I/usr/local/include"
      CPP="$CC -I/usr/local/include -E"
      PFMCFLAGS="$CFLAGS_LL"
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

AC_SUBST(MPS_OS_NAME)
AC_SUBST(MPS_ARCH_NAME)
AC_SUBST(MPS_BUILD_NAME)
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

echo 1>&2 "CONFIGURE/MAKE IS NOT THE BEST WAY TO BUILD THE MPS
-- see <manual/build.txt> for alternative approaches"


# C. COPYRIGHT AND LICENSE
#
# Copyright (C) 2012-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
# All rights reserved.  This is an open source license.  Contact
# Ravenbrook for commercial licensing options.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 
# 3. Redistributions in any form must be accompanied by information on how
# to obtain complete source code for this software and any accompanying
# software that uses this software.  The source code must either be
# included in the distribution or be available for no more than the cost
# of distribution plus a nominal fee, and must be freely redistributable
# under reasonable conditions.  For an executable file, complete source
# code means the source code for all modules it contains. It does not
# include source code for modules or files that typically accompany the
# major components of the operating system on which the executable file
# runs.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
# USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
