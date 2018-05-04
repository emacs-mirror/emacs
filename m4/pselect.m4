# pselect.m4 serial 6
dnl Copyright (C) 2011-2018 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_PSELECT],
[
  AC_REQUIRE([gl_HEADER_SYS_SELECT])
  AC_REQUIRE([AC_C_RESTRICT])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CHECK_FUNCS_ONCE([pselect])

  if test $ac_cv_func_pselect = yes; then
    AC_CACHE_CHECK([whether signature of pselect conforms to POSIX],
      [gl_cv_sig_pselect],
      [AC_LINK_IFELSE(
         [AC_LANG_PROGRAM(
              [[#include <sys/select.h>
                ]],
              [[int (*p) (int, fd_set *, fd_set *, fd_set *restrict,
                          struct timespec const *restrict,
                          sigset_t const *restrict) = pselect;
                return !p;]])],
         [gl_cv_sig_pselect=yes],
         [gl_cv_sig_pselect=no])])

    dnl On FreeBSD 8.2, pselect() doesn't always reject bad fds.
    AC_CACHE_CHECK([whether pselect detects invalid fds],
      [gl_cv_func_pselect_detects_ebadf],
      [
        AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <sys/types.h>
#include <sys/time.h>
#if HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif
#include <unistd.h>
#include <errno.h>
]],[[
  fd_set set;
  dup2(0, 16);
  FD_ZERO(&set);
  FD_SET(16, &set);
  close(16);
  struct timespec timeout;
  timeout.tv_sec = 0;
  timeout.tv_nsec = 5000;
  return pselect (17, &set, NULL, NULL, &timeout, NULL) != -1 || errno != EBADF;
]])], [gl_cv_func_pselect_detects_ebadf=yes],
      [gl_cv_func_pselect_detects_ebadf=no],
          [
           case "$host_os" in
                           # Guess yes on glibc systems.
            *-gnu* | gnu*) gl_cv_func_pselect_detects_ebadf="guessing yes" ;;
                           # If we don't know, assume the worst.
            *)             gl_cv_func_pselect_detects_ebadf="guessing no" ;;
           esac
          ])
      ])
    case $gl_cv_func_pselect_detects_ebadf in
      *yes) ;;
      *) REPLACE_PSELECT=1 ;;
    esac
  fi

  if test $ac_cv_func_pselect = no || test $gl_cv_sig_pselect = no; then
    REPLACE_PSELECT=1
  fi
])
