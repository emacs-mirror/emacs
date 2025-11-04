# sig2str.m4
# serial 9
dnl Copyright (C) 2002, 2005-2006, 2009-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_SIG2STR],
[
  AC_REQUIRE([gl_SIGNAL_H_DEFAULTS])
  gl_CHECK_FUNCS_ANDROID([sig2str], [[#include <signal.h>]])
  gl_CHECK_FUNCS_ANDROID([str2sig], [[#include <signal.h>]])
  if test $ac_cv_func_sig2str = no; then
    HAVE_SIG2STR=0
  fi
  if test $ac_cv_func_str2sig = no; then
    HAVE_STR2SIG=0
  fi
])

# Prerequisites of lib/sig2str.c.
AC_DEFUN([gl_PREREQ_SIG2STR], [
  :
])
