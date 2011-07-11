# pthread_sigmask.m4 serial 7-emacs1
dnl Copyright (C) 2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_PTHREAD_SIGMASK],
[
  AC_CHECK_FUNCS_ONCE([pthread_sigmask])
  LIB_PTHREAD_SIGMASK=
    dnl gl_THREADLIB is not in use.  Assume the application wants
    dnl POSIX semantics.
    if test $ac_cv_func_pthread_sigmask != yes; then
      gl_save_LIBS=$LIBS
      AC_SEARCH_LIBS([pthread_sigmask], [pthread c_r])
      LIBS=$gl_save_LIBS
      if test "$ac_cv_search_pthread_sigmask" = no; then
        HAVE_PTHREAD_SIGMASK=0
      elif test "$ac_cv_search_pthread_sigmask" != 'none required'; then
        LIB_PTHREAD_SIGMASK=$ac_cv_search_pthread_sigmask
      fi
    fi
  AC_SUBST([LIB_PTHREAD_SIGMASK])
  dnl We don't need a variable LTLIB_PTHREAD_SIGMASK, because when
  dnl "$gl_threads_api" = posix, $LTLIBMULTITHREAD and $LIBMULTITHREAD are the
  dnl same: either both empty or both "-lpthread".
])
