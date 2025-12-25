# lchmod.m4
# serial 11
dnl Copyright (C) 2005-2006, 2008-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl From Paul Eggert.
dnl Provide a replacement for lchmod on hosts that lack a working version.

AC_DEFUN([gl_FUNC_LCHMOD],
[
  AC_REQUIRE([gl_SYS_STAT_H_DEFAULTS])

  dnl Persuade glibc <sys/stat.h> to declare lchmod().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  gl_CHECK_FUNCS_ANDROID([lchmod], [[#include <sys/stat.h>]])
  if test "$ac_cv_func_lchmod" = no; then
    HAVE_LCHMOD=0
  fi
])

# Prerequisites of lib/lchmod.c.
AC_DEFUN([gl_PREREQ_LCHMOD],
[
  :
])
