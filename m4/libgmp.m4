# libgmp.m4 serial 7
# Configure the GMP library or a replacement.
dnl Copyright 2020-2022 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl gl_LIBGMP
dnl Search for an installed libgmp.
dnl If found, set and AC_SUBST HAVE_LIBGMP=yes and the LIBGMP and LTLIBGMP
dnl variables, and augment the CPPFLAGS variable, and #define HAVE_LIBGMP to 1.
dnl Otherwise, set and AC_SUBST HAVE_LIBGMP=no and LIBGMP and LTLIBGMP to
dnl empty.

AC_DEFUN([gl_LIBGMP],
[
  AC_ARG_WITH([libgmp],
    [AS_HELP_STRING([--without-libgmp],
       [do not use the GNU Multiple Precision (GMP) library;
        this is the default on systems lacking libgmp.])])
  HAVE_LIBGMP=no
  LIBGMP=
  LTLIBGMP=
  AS_IF([test "$with_libgmp" != no],
    [AC_CHECK_HEADERS([gmp.h gmp/gmp.h], [break])
     dnl Prefer AC_LIB_HAVE_LINKFLAGS if the havelib module is also in use.
     AS_IF([test "$ac_cv_header_gmp_h" = yes ||
            test "$ac_cv_header_gmp_gmp_h" = yes],
       [m4_ifdef([gl_HAVE_MODULE_HAVELIB],
          [AC_LIB_HAVE_LINKFLAGS([gmp], [],
             [#if HAVE_GMP_H
              # include <gmp.h>
              #else
              # include <gmp/gmp.h>
              #endif],
             [static const mp_limb_t x[2] = { 0x73, 0x55 };
              mpz_t tmp;
              mpz_roinit_n (tmp, x, 2);
             ],
             [no])],
          [gl_saved_LIBS=$LIBS
           AC_SEARCH_LIBS([__gmpz_roinit_n], [gmp])
           LIBS=$gl_saved_LIBS
           case $ac_cv_search___gmpz_roinit_n in
             'none needed')
               HAVE_LIBGMP=yes;;
             -*)
               HAVE_LIBGMP=yes
               LIBGMP=$ac_cv_search___gmpz_roinit_n
               LTLIBGMP=$LIBGMP;;
           esac
           AC_SUBST([HAVE_LIBGMP])
           AC_SUBST([LIBGMP])
           AC_SUBST([LTLIBGMP])])])
     if test "$with_libgmp,$HAVE_LIBGMP" = yes,no; then
       AC_MSG_ERROR(
         [GMP not found, although --with-libgmp was specified.m4_ifdef(
            [AC_LIB_HAVE_LINKFLAGS],
            [ Try specifying --with-libgmp-prefix=DIR.])])
     fi])
  if test $HAVE_LIBGMP = yes && test "$ac_cv_header_gmp_h" = yes; then
    GL_GENERATE_GMP_H=false
  else
    GL_GENERATE_GMP_H=true
  fi
  gl_CONDITIONAL([GL_GENERATE_MINI_GMP_H],
    [test $HAVE_LIBGMP != yes])
  gl_CONDITIONAL([GL_GENERATE_GMP_GMP_H],
    [test $HAVE_LIBGMP = yes && test "$ac_cv_header_gmp_h" != yes])
])
