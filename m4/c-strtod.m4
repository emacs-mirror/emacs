# c-strtod.m4 serial 16

# Copyright (C) 2004-2006, 2009-2019 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Written by Paul Eggert.

AC_DEFUN([gl_C99_STRTOLD],
[
  AC_CACHE_CHECK([whether strtold conforms to C99],
    [gl_cv_func_c99_strtold],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
          [[/* On HP-UX before 11.23, strtold returns a struct instead of
                long double.  Reject implementations like that, by requiring
                compatibility with the C99 prototype.  */
             #include <stdlib.h>
             static long double (*p) (char const *, char **) = strtold;
             static long double
             test (char const *nptr, char **endptr)
             {
               long double r;
               r = strtold (nptr, endptr);
               return r;
             }]],
           [[return test ("1.0", NULL) != 1 || p ("1.0", NULL) != 1;]])],
       [gl_cv_func_c99_strtold=yes],
       [gl_cv_func_c99_strtold=no])])
  if test $gl_cv_func_c99_strtold = yes; then
    AC_DEFINE([HAVE_C99_STRTOLD], [1], [Define to 1 if strtold conforms to C99.])
  fi
])

dnl Prerequisites of lib/c-strtod.c.
AC_DEFUN([gl_C_STRTOD],
[
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  AC_CHECK_HEADERS_ONCE([xlocale.h])
  dnl We can't use AC_CHECK_FUNC here, because strtod_l() is defined as a
  dnl static inline function when compiling for Android 7.1 or older.
  AC_CACHE_CHECK([for strtod_l], [gl_cv_func_strtod_l],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <stdlib.h>
            #include <locale.h>
            #if HAVE_XLOCALE_H
            # include <xlocale.h>
            #endif
            locale_t loc;
          ]],
          [[char *end;
            return strtod_l("0",&end,loc) < 0.0;
          ]])
       ],
       [gl_cv_func_strtod_l=yes],
       [gl_cv_func_strtod_l=no])
    ])
  if test $gl_cv_func_strtod_l = yes; then
    HAVE_STRTOD_L=1
  else
    HAVE_STRTOD_L=0
  fi
  AC_DEFINE_UNQUOTED([HAVE_STRTOD_L], [$HAVE_STRTOD_L],
    [Define to 1 if the system has the 'strtod_l' function.])
])

dnl Prerequisites of lib/c-strtold.c.
AC_DEFUN([gl_C_STRTOLD],
[
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  AC_REQUIRE([gl_C99_STRTOLD])
  AC_CHECK_FUNCS([strtold_l])
])
