# stdbit_h.m4
# serial 14
dnl Copyright 2024-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl A placeholder for <stdbit.h>, for platforms that have issues.

AC_DEFUN_ONCE([gl_STDBIT_H],
[
  AC_REQUIRE([gl_BIGENDIAN])

  gl_CHECK_NEXT_HEADERS([stdbit.h])
  if test "$ac_cv_header_stdbit_h" = yes; then
    HAVE_STDBIT_H=1
  else
    HAVE_STDBIT_H=0
  fi
  AC_SUBST([HAVE_STDBIT_H])
  AM_CONDITIONAL([GL_HAVE_STDBIT_H], [test "$ac_cv_header_stdbit_h" = yes])

  if test "$ac_cv_header_stdbit_h" = yes; then
    dnl We may have a stdbit.h without C2y features.
    AC_CHECK_DECLS([stdc_rotate_left_uc], , , [[#include <stdbit.h>]])
    if test "$ac_cv_have_decl_stdc_rotate_left_uc" = no; then
      GL_GENERATE_STDBIT_H=true
    else
      GL_GENERATE_STDBIT_H=false
    fi
  else
    GL_GENERATE_STDBIT_H=true
  fi
])

# gl_STDBIT_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_STDBIT_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_STDBIT_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_STDBIT_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_STDBIT_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_LEADING_ZEROS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_LEADING_ONES])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_TRAILING_ZEROS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_TRAILING_ONES])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_FIRST_LEADING_ZERO])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_FIRST_LEADING_ONE])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_FIRST_TRAILING_ZERO])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_FIRST_TRAILING_ONE])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_COUNT_ZEROS])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_COUNT_ONES])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_HAS_SINGLE_BIT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_BIT_WIDTH])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_BIT_FLOOR])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_BIT_CEIL])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_ROTATE_LEFT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_ROTATE_RIGHT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_MEMREVERSE8])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_MEMREVERSE8U])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_LOAD8_ALIGNED])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_LOAD8])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_STORE8_ALIGNED])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_STDC_STORE8])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_STDBIT_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_STDBIT_H_DEFAULTS])
])

AC_DEFUN([gl_STDBIT_H_DEFAULTS],
[
])
