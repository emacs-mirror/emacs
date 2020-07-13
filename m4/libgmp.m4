# Configure the GMP library or a replacement.

dnl Copyright 2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_LIBGMP],
[
  AC_ARG_WITH([libgmp],
    [AS_HELP_STRING([--without-libgmp],
       [do not use the GNU Multiple Precision (GMP) library;
        this is the default on systems lacking libgmp.])])

  AC_CHECK_HEADERS_ONCE([gmp.h])
  GMP_H=gmp.h
  LIB_GMP=

  case $with_libgmp in
    no) ;;
    yes) GMP_H= LIB_GMP=-lgmp;;
    *) if test "$ac_cv_header_gmp_h" = yes; then
         gl_saved_LIBS=$LIBS
         AC_SEARCH_LIBS([__gmpz_roinit_n], [gmp])
         LIBS=$gl_saved_LIBS
         case $ac_cv_search___gmpz_roinit_n in
           'none needed')
             GMP_H=;;
           -*)
             GMP_H= LIB_GMP=$ac_cv_search___gmpz_roinit_n;;
         esac
       fi;;
  esac

  if test -z "$GMP_H"; then
    AC_DEFINE([HAVE_GMP], 1,
      [Define to 1 if you have the GMP library instead of just the
       mini-gmp replacement.])
  fi

  AC_SUBST([LIB_GMP])
  AC_SUBST([GMP_H])
  AM_CONDITIONAL([GL_GENERATE_GMP_H], [test -n "$GMP_H"])
])
