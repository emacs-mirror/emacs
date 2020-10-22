#serial 8

dnl Copyright (C) 2005-2006, 2008-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Paul Eggert.
dnl Provide a replacement for lchmod on hosts that lack a working version.

AC_DEFUN([gl_FUNC_LCHMOD],
[
  AC_REQUIRE([gl_SYS_STAT_H_DEFAULTS])

  dnl Persuade glibc <sys/stat.h> to declare lchmod().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles

  AC_CHECK_FUNCS_ONCE([lchmod lstat])
  if test "$ac_cv_func_lchmod" = no; then
    HAVE_LCHMOD=0
  fi
])

# Prerequisites of lib/lchmod.c.
AC_DEFUN([gl_PREREQ_LCHMOD],
[
  :
])
