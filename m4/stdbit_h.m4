# stdbit_h.m4
# serial 2
dnl Copyright 2024-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl A placeholder for <stdbit.h>, for platforms that have issues.

AC_DEFUN_ONCE([gl_STDBIT_H],
[
  AC_REQUIRE([gl_BIGENDIAN])

  AC_CHECK_HEADERS_ONCE([stdbit.h])
  if test $ac_cv_header_stdbit_h = yes; then
    GL_GENERATE_STDBIT_H=false
  else
    GL_GENERATE_STDBIT_H=true
  fi

  dnl We don't use gl_MODULE_INDICATOR_INIT_VARIABLE here, because stdbit.in.h
  dnl does not use #include_next.
  GL_STDC_LEADING_ZEROS=0;       AC_SUBST([GL_STDC_LEADING_ZEROS])
  GL_STDC_LEADING_ONES=0;        AC_SUBST([GL_STDC_LEADING_ONES])
  GL_STDC_TRAILING_ZEROS=0;      AC_SUBST([GL_STDC_TRAILING_ZEROS])
  GL_STDC_TRAILING_ONES=0;       AC_SUBST([GL_STDC_TRAILING_ONES])
  GL_STDC_FIRST_LEADING_ZERO=0;  AC_SUBST([GL_STDC_FIRST_LEADING_ZERO])
  GL_STDC_FIRST_LEADING_ONE=0;   AC_SUBST([GL_STDC_FIRST_LEADING_ONE])
  GL_STDC_FIRST_TRAILING_ZERO=0; AC_SUBST([GL_STDC_FIRST_TRAILING_ZERO])
  GL_STDC_FIRST_TRAILING_ONE=0;  AC_SUBST([GL_STDC_FIRST_TRAILING_ONE])
  GL_STDC_COUNT_ZEROS=0;         AC_SUBST([GL_STDC_COUNT_ZEROS])
  GL_STDC_COUNT_ONES=0;          AC_SUBST([GL_STDC_COUNT_ONES])
  GL_STDC_HAS_SINGLE_BIT=0;      AC_SUBST([GL_STDC_HAS_SINGLE_BIT])
  GL_STDC_BIT_WIDTH=0;           AC_SUBST([GL_STDC_BIT_WIDTH])
  GL_STDC_BIT_FLOOR=0;           AC_SUBST([GL_STDC_BIT_FLOOR])
  GL_STDC_BIT_CEIL=0;            AC_SUBST([GL_STDC_BIT_CEIL])
])
