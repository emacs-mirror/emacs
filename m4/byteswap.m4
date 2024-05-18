# byteswap.m4
# serial 6
dnl Copyright (C) 2005, 2007, 2009-2024 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl Written by Oskar Liljeblad.

AC_DEFUN([gl_BYTESWAP],
[
  dnl Prerequisites of lib/byteswap.in.h.
  AC_CHECK_HEADERS_ONCE([byteswap.h])
  if test $ac_cv_header_byteswap_h = yes; then
    AC_CACHE_CHECK([for working bswap_16, bswap_32, bswap_64],
      [gl_cv_header_working_byteswap_h],
      [gl_cv_header_working_byteswap_h=no
       AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <byteswap.h>
            ]],
            [[int value_16 = bswap_16 (0.0);
              int value_32 = bswap_32 (0.0);
              int value_64 = bswap_64 (0.0);
              return !(value_16 + value_32 + value_64);
            ]])
         ],
         [gl_cv_header_working_byteswap_h=yes],
         [gl_cv_header_working_byteswap_h=no])
      ])
  fi
  if test $gl_cv_header_working_byteswap_h = yes; then
    GL_GENERATE_BYTESWAP_H=false
  else
    GL_GENERATE_BYTESWAP_H=true
  fi
])
