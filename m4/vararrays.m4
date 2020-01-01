# Check for variable-length arrays.

# serial 5

# From Paul Eggert

# Copyright (C) 2001, 2009-2020 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This is a copy of AC_C_VARARRAYS from a recent development version
# of Autoconf.  It replaces Autoconf's version, or for pre-2.61 autoconf
# it defines the macro that Autoconf lacks.
AC_DEFUN([AC_C_VARARRAYS],
[
  AC_CACHE_CHECK([for variable-length arrays],
    ac_cv_c_vararrays,
    [AC_EGREP_CPP([defined],
       [#ifdef __STDC_NO_VLA__
        defined
        #endif
       ],
       [ac_cv_c_vararrays='no: __STDC_NO_VLA__ is defined'],
       [AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM(
             [[/* Test for VLA support.  This test is partly inspired
                  from examples in the C standard.  Use at least two VLA
                  functions to detect the GCC 3.4.3 bug described in:
                  https://lists.gnu.org/r/bug-gnulib/2014-08/msg00014.html
                  */
               #ifdef __STDC_NO_VLA__
                syntax error;
               #else
                 extern int n;
                 int B[100];
                 int fvla (int m, int C[m][m]);

                 int
                 simple (int count, int all[static count])
                 {
                   return all[count - 1];
                 }

                 int
                 fvla (int m, int C[m][m])
                 {
                   typedef int VLA[m][m];
                   VLA x;
                   int D[m];
                   static int (*q)[m] = &B;
                   int (*s)[n] = q;
                   return C && &x[0][0] == &D[0] && &D[0] == s[0];
                 }
               #endif
               ]])],
          [ac_cv_c_vararrays=yes],
          [ac_cv_c_vararrays=no])])])
  if test "$ac_cv_c_vararrays" = yes; then
    dnl This is for compatibility with Autoconf 2.61-2.69.
    AC_DEFINE([HAVE_C_VARARRAYS], 1,
      [Define to 1 if C supports variable-length arrays.])
  elif test "$ac_cv_c_vararrays" = no; then
    AC_DEFINE([__STDC_NO_VLA__], 1,
      [Define to 1 if C does not support variable-length arrays, and
       if the compiler does not already define this.])
  fi
])
