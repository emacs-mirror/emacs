dnl Copyright 2017-2018 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_EXPLICIT_BZERO],
[
  AC_REQUIRE([gl_HEADER_STRING_H_DEFAULTS])

  dnl Persuade glibc <string.h> to declare explicit_bzero.
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_CHECK_FUNCS_ONCE([explicit_bzero])
  if test $ac_cv_func_explicit_bzero = no; then
    HAVE_EXPLICIT_BZERO=0
  fi
])

AC_DEFUN([gl_PREREQ_EXPLICIT_BZERO],
[
  AC_CHECK_FUNCS([explicit_memset])
])
