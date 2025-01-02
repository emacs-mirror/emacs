# memset_explicit.m4
# serial 3
dnl Copyright 2022-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_MEMSET_EXPLICIT],
[
  AC_REQUIRE([gl_STRING_H_DEFAULTS])
  dnl Persuade OpenSolaris derivatives' <string.h> to declare memset_s().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  gl_CHECK_FUNCS_ANDROID([memset_explicit], [[#include <string.h>]])
  if test $ac_cv_func_memset_explicit = no; then
    HAVE_MEMSET_EXPLICIT=0
    case "$gl_cv_onwards_func_memset_explicit" in
      future*) REPLACE_MEMSET_EXPLICIT=1 ;;
    esac
  fi
])

AC_DEFUN([gl_PREREQ_MEMSET_EXPLICIT],
[
  AC_CHECK_FUNCS([explicit_memset])
  AC_CHECK_FUNCS_ONCE([memset_s])
])
