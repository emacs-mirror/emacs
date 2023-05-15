# printf-posix.m4 serial 5
dnl Copyright (C) 2007-2023 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_PRINTF_POSIX],
[
  AC_REQUIRE([gl_FUNC_PRINTF_IS_POSIX])
  if test $gl_cv_func_printf_posix = no; then
    gl_PREREQ_VASNPRINTF_WITH_POSIX_EXTRAS
    gl_REPLACE_VASNPRINTF
    gl_REPLACE_PRINTF
  fi
])

dnl Test whether printf is POSIX compliant.
dnl Result is gl_cv_func_printf_posix.
AC_DEFUN([gl_FUNC_PRINTF_IS_POSIX],
[
  AC_REQUIRE([gl_FUNC_VFPRINTF_IS_POSIX])
  gl_cv_func_printf_posix="$gl_cv_func_vfprintf_posix"
])

AC_DEFUN([gl_REPLACE_PRINTF],
[
  AC_REQUIRE([gl_STDIO_H_DEFAULTS])
  AC_REQUIRE([gl_ASM_SYMBOL_PREFIX])
  AC_LIBOBJ([printf])
  REPLACE_PRINTF=1
  AC_DEFINE([REPLACE_PRINTF_POSIX], [1],
    [Define if printf is overridden by a POSIX compliant gnulib implementation.])
  gl_PREREQ_PRINTF
])

AC_DEFUN([gl_PREREQ_PRINTF], [:])
