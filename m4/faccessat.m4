# faccessat.m4
# serial 12
dnl Copyright (C) 2009-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# See if we need to provide faccessat replacement.

# Written by Eric Blake.

AC_DEFUN([gl_FUNC_FACCESSAT],
[
  AC_REQUIRE([gl_UNISTD_H_DEFAULTS])
  AC_REQUIRE([gl_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK])

  dnl Persuade glibc <unistd.h> to declare faccessat().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  gl_CHECK_FUNCS_ANDROID([faccessat], [[#include <unistd.h>]])
  if test $ac_cv_func_faccessat = no; then
    HAVE_FACCESSAT=0
    case "$gl_cv_onwards_func_faccessat" in
      future*) REPLACE_FACCESSAT=1 ;;
    esac
  else
    case $gl_cv_func_lstat_dereferences_slashed_symlink in
      *yes) ;;
      *)    REPLACE_FACCESSAT=1 ;;
    esac
  fi
])

# Prerequisites of lib/faccessat.c.
AC_DEFUN([gl_PREREQ_FACCESSAT],
[
  AC_CHECK_FUNCS([access])
])
