# pthread_sigmask.m4 serial 2
dnl Copyright (C) 2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_PTHREAD_SIGMASK],
[
  m4_ifdef([gl_THREADLIB], [
    AC_REQUIRE([gl_THREADLIB])
    if test "$gl_threads_api" = posix; then
      gl_save_LIBS="$LIBS"
      LIBS="$LIBS $LIBMULTITHREAD"
      AC_CHECK_FUNCS([pthread_sigmask])
      LIBS="$gl_save_LIBS"
    else
      ac_cv_func_pthread_sigmask=no
    fi
  ], [
    AC_CHECK_FUNCS_ONCE([pthread_sigmask])
  ])

  if test $ac_cv_func_pthread_sigmask = no; then
    REPLACE_PTHREAD_SIGMASK=1
  fi
])
