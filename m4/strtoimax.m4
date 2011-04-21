# strtoimax.m4 serial 8
dnl Copyright (C) 2002-2004, 2006, 2009-2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_STRTOIMAX],
[
  AC_CACHE_CHECK([whether <inttypes.h> defines strtoimax as a macro],
    gl_cv_func_strtoimax_macro,
    [AC_EGREP_CPP([inttypes_h_defines_strtoimax], [#include <inttypes.h>
#ifdef strtoimax
 inttypes_h_defines_strtoimax
#endif],
       gl_cv_func_strtoimax_macro=yes,
       gl_cv_func_strtoimax_macro=no)])

  if test "$gl_cv_func_strtoimax_macro" != yes; then
    AC_REPLACE_FUNCS([strtoimax])
    if test $ac_cv_func_strtoimax = no; then
      gl_PREREQ_STRTOIMAX
    fi
  fi
])

# Prerequisites of lib/strtoimax.c.
AC_DEFUN([gl_PREREQ_STRTOIMAX], [
  AC_CHECK_DECLS([strtoll])
  AC_REQUIRE([AC_TYPE_LONG_LONG_INT])
])
