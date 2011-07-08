# pthread_sigmask.m4 serial 7
dnl Copyright (C) 2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_PTHREAD_SIGMASK],
[
  AC_CHECK_FUNCS_ONCE([pthread_sigmask])
  LIB_PTHREAD_SIGMASK=
  m4_ifdef([gl_THREADLIB], [
    AC_REQUIRE([gl_THREADLIB])
    if test "$gl_threads_api" = posix; then
      if test $ac_cv_func_pthread_sigmask = yes; then
        dnl pthread_sigmask is available without -lpthread.
        :
      else
        if test -n "$LIBMULTITHREAD"; then
          AC_CACHE_CHECK([for pthread_sigmask in $LIBMULTITHREAD],
            [gl_cv_func_pthread_sigmask_in_LIBMULTITHREAD],
            [gl_save_LIBS="$LIBS"
             LIBS="$LIBS $LIBMULTITHREAD"
             AC_LINK_IFELSE(
               [AC_LANG_PROGRAM(
                  [[#include <pthread.h>
                    #include <signal.h>
                  ]],
                  [[return pthread_sigmask (0, (sigset_t *) 0, (sigset_t *) 0);]])
               ],
               [gl_cv_func_pthread_sigmask_in_LIBMULTITHREAD=yes],
               [gl_cv_func_pthread_sigmask_in_LIBMULTITHREAD=no])
             LIBS="$gl_save_LIBS"
            ])
          if test $gl_cv_func_pthread_sigmask_in_LIBMULTITHREAD = yes; then
            dnl pthread_sigmask is available with -lpthread.
            LIB_PTHREAD_SIGMASK="$LIBMULTITHREAD"
          else
            dnl pthread_sigmask is not available at all.
            HAVE_PTHREAD_SIGMASK=0
          fi
        else
          dnl pthread_sigmask is not available at all.
          HAVE_PTHREAD_SIGMASK=0
        fi
      fi
    else
      dnl pthread_sigmask may exist but does not interoperate with the chosen
      dnl multithreading facility.
      dnl If "$gl_threads_api" = pth, we could use the function pth_sigmask,
      dnl but it is equivalent to sigprocmask, so we choose to emulate
      dnl pthread_sigmask with sigprocmask also in this case. This yields fewer
      dnl link dependencies.
      if test $ac_cv_func_pthread_sigmask = yes; then
        REPLACE_PTHREAD_SIGMASK=1
      else
        HAVE_PTHREAD_SIGMASK=0
      fi
    fi
  ] ,[
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
  ])
  AC_SUBST([LIB_PTHREAD_SIGMASK])
  dnl We don't need a variable LTLIB_PTHREAD_SIGMASK, because when
  dnl "$gl_threads_api" = posix, $LTLIBMULTITHREAD and $LIBMULTITHREAD are the
  dnl same: either both empty or both "-lpthread".
])
