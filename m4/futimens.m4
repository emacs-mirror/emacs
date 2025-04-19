# futimens.m4
# serial 12
dnl Copyright (C) 2009-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# See if we need to provide futimens replacement.

# Written by Eric Blake.

AC_DEFUN([gl_FUNC_FUTIMENS],
[
  AC_REQUIRE([gl_SYS_STAT_H_DEFAULTS])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  gl_CHECK_FUNCS_ANDROID([futimens], [[#include <sys/stat.h>]])
  if test $ac_cv_func_futimens = no; then
    HAVE_FUTIMENS=0
    case "$gl_cv_onwards_func_futimens" in
      future*) REPLACE_FUTIMENS=1 ;;
    esac
  else
    AC_CACHE_CHECK([whether futimens works],
      [gl_cv_func_futimens_works],
      [AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
]GL_MDA_DEFINES],
     [[struct timespec ts[2];
      int fd = creat ("conftest.file", 0600);
      int result = 0;
      struct stat st;
      if (fd < 0)
        return 1;
      ts[0].tv_sec = 1;
      ts[0].tv_nsec = UTIME_OMIT;
      ts[1].tv_sec = 1;
      ts[1].tv_nsec = UTIME_NOW;
      errno = 0;
      if (futimens (AT_FDCWD, NULL) == 0 || errno != EBADF)
        result |= 2;
      if (futimens (fd, ts))
        result |= 4;
      sleep (1);
      ts[0].tv_nsec = UTIME_NOW;
      ts[1].tv_nsec = UTIME_OMIT;
      if (futimens (fd, ts))
        result |= 8;
      if (fstat (fd, &st))
        result |= 16;
      if (st.st_ctime < st.st_atime)
        result |= 32;
      enum
      {
        BILLION = 1000 * 1000 * 1000,
        /* Bogus positive and negative tv_nsec values closest to valid
           range, but without colliding with UTIME_NOW or UTIME_OMIT.  */
        UTIME_BOGUS_POS = BILLION + ((UTIME_NOW == BILLION || UTIME_OMIT == BILLION)
                                     ? (1 + (UTIME_NOW == BILLION + 1)
                                        + (UTIME_OMIT == BILLION + 1))
                                     : 0)
      };
      ts[0].tv_sec = 1;
      ts[0].tv_nsec = UTIME_BOGUS_POS;
      ts[1].tv_sec = 1;
      ts[1].tv_nsec = 0;
      if (futimens (fd, ts) == 0)
        result |= 64;
      return result;
      ]])],
         [gl_cv_func_futimens_works=yes],
         [gl_cv_func_futimens_works=no],
         [case "$host_os" in
                           # Guess no on glibc systems.
            *-gnu* | gnu*) gl_cv_func_futimens_works="guessing no" ;;
                           # Guess no on musl systems.
            *-musl*)       gl_cv_func_futimens_works="guessing no" ;;
                           # Guess yes otherwise.
            *)             gl_cv_func_futimens_works="guessing yes" ;;
          esac
         ])
      rm -f conftest.file])
    case "$gl_cv_func_futimens_works" in
      *yes) ;;
      *)
        REPLACE_FUTIMENS=1
        ;;
    esac
  fi
])
