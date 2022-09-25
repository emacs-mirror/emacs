# Configure ieee754-h module

dnl Copyright 2018-2022 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_IEEE754_H],
[
  AC_REQUIRE([AC_C_BIGENDIAN])
  AC_CHECK_HEADERS_ONCE([ieee754.h])
  if test $ac_cv_header_ieee754_h = yes; then
    GL_GENERATE_IEEE754_H=false
  else
    GL_GENERATE_IEEE754_H=true
    AC_DEFINE([_GL_REPLACE_IEEE754_H], 1,
              [Define to 1 if <ieee754.h> is missing.])
  fi
])
