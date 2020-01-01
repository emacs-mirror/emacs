# count-leading-zeros.m4 serial 2
dnl Copyright (C) 2012-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_COUNT_LEADING_ZEROS],
[
  dnl We don't need (and can't compile) count_leading_zeros_ll
  dnl unless the type 'unsigned long long int' exists.
  AC_REQUIRE([AC_TYPE_UNSIGNED_LONG_LONG_INT])
])
