# configure.ac -- autoconf configuration for the MPS    -*- Autoconf -*-
#
# $Id$
# Copyright (C) 2012 Ravenbrook Limited.  See end of file for license.
#
# YOU DON"T NEED AUTOCONF TO BUILD THE MPS
# This is just here for people who want or expect a configure script.
# See [Building the Memory Pool System](manual/build.txt) for how best
# to build and integrate the MPS.
#
# Generate the configure script with
#
#     autoreconf -vif
#

AC_PREREQ([2.50])
AC_INIT([Memory Pool System], [master], [mps-questions@ravenbrook.com])
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_SRCDIR([code/mps.c])

# Checks for programs.
AC_PROG_CC

AC_CANONICAL_HOST
AC_MSG_CHECKING([target platform])
case $host in
  i*86-*-linux*)
      AC_MSG_RESULT([Linux x86])
      MPS_TARGET_PF=MPS_PF_LI3C
      MPS_TARGET_NAME=lii3gc
    ;;
  x86_64-*-linux*)
      AC_MSG_RESULT([Linux x86_64])
      MPS_TARGET_PF=MPS_PF_LII6GC
      MPS_TARGET_NAME=lii6gc
    ;;
  i*86-*-darwin*)
      AC_MSG_RESULT([Mac OS X x86])
      MPS_TARGET_PF=MPS_PF_XCI3LL
      BUILD_TARGET=build-via-xcode
      CLEAN_TARGET=clean-xcode-build
    ;;
  x86_64-apple-darwin*)
      AC_MSG_RESULT([Mac OS X x86_64])
      MPS_TARGET_PF=MPS_PF_XCI6LL
      BUILD_TARGET=build-via-xcode
      CLEAN_TARGET=clean-xcode-build
    ;;
  i*86-*-freebsd*)
      AC_MSG_RESULT([FreeBSD x86])
      MPS_TARGET_PF=MPS_PF_FRI3GC
      MPS_TARGET_NAME=fri3gc
    ;;
  amd64-*-freebsd*)
      AC_MSG_RESULT([FreeBSD x86_64])
      MPS_TARGET_PF=MPS_PF_FRI6GC
      MPS_TARGET_NAME=fri6gc
    ;;
  *)
    AC_MSG_ERROR([MPS does not yet support this platform.])
esac

AC_SUBST(MPS_TARGET_PF)
AC_SUBST(MPS_TARGET_NAME)
AC_CONFIG_FILES(Makefile)

AC_OUTPUT
