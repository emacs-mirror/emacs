# inttypes.m4 serial 19
dnl Copyright (C) 2006-2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Derek Price, Bruno Haible.
dnl Test whether <inttypes.h> is supported or must be substituted.

AC_DEFUN([gl_INTTYPES_H],
[
  AC_REQUIRE([gl_STDINT_H])

  dnl Override <inttypes.h> always, so that the portability warnings work.
  AC_REQUIRE([gl_INTTYPES_H_DEFAULTS])
  gl_CHECK_NEXT_HEADERS([inttypes.h])

  dnl Ensure that <stdint.h> defines the limit macros, since gnulib's
  dnl <inttypes.h> relies on them.  This macro is only needed when a
  dnl C++ compiler is in use; it has no effect for a C compiler.
  dnl Also be careful to define __STDC_LIMIT_MACROS only when gnulib's
  dnl <inttypes.h> is going to be created, and to avoid redefinition warnings
  dnl if the __STDC_LIMIT_MACROS is already defined through the CPPFLAGS.
  AC_DEFINE([GL_TRIGGER_STDC_LIMIT_MACROS], [1],
    [Define to make the limit macros in <stdint.h> visible.])
  AH_VERBATIM([__STDC_LIMIT_MACROS_ZZZ],
[/* Ensure that <stdint.h> defines the limit macros, since gnulib's
   <inttypes.h> relies on them.  */
#if defined __cplusplus && !defined __STDC_LIMIT_MACROS && GL_TRIGGER_STDC_LIMIT_MACROS
# define __STDC_LIMIT_MACROS 1
#endif
])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use.
  gl_WARN_ON_USE_PREPARE([[#include <inttypes.h>
    ]], [imaxabs imaxdiv strtoimax strtoumax])
])

AC_DEFUN([gl_INTTYPES_MODULE_INDICATOR],
[
  dnl Use AC_REQUIRE here, so that the default settings are expanded once only.
  AC_REQUIRE([gl_INTTYPES_H_DEFAULTS])
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
])

AC_DEFUN([gl_INTTYPES_H_DEFAULTS],
[
  GNULIB_IMAXABS=0;      AC_SUBST([GNULIB_IMAXABS])
  GNULIB_IMAXDIV=0;      AC_SUBST([GNULIB_IMAXDIV])
  GNULIB_STRTOIMAX=0;    AC_SUBST([GNULIB_STRTOIMAX])
  GNULIB_STRTOUMAX=0;    AC_SUBST([GNULIB_STRTOUMAX])
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_DECL_IMAXABS=1;   AC_SUBST([HAVE_DECL_IMAXABS])
  HAVE_DECL_IMAXDIV=1;   AC_SUBST([HAVE_DECL_IMAXDIV])
  HAVE_DECL_STRTOIMAX=1; AC_SUBST([HAVE_DECL_STRTOIMAX])
  HAVE_DECL_STRTOUMAX=1; AC_SUBST([HAVE_DECL_STRTOUMAX])
  INT64_MAX_EQ_LONG_MAX='defined _LP64';  AC_SUBST([INT64_MAX_EQ_LONG_MAX])
  PRI_MACROS_BROKEN=0;   AC_SUBST([PRI_MACROS_BROKEN])
  PRIPTR_PREFIX=__PRIPTR_PREFIX;  AC_SUBST([PRIPTR_PREFIX])
  UINT64_MAX_EQ_ULONG_MAX='defined _LP64';  AC_SUBST([UINT64_MAX_EQ_ULONG_MAX])
])
