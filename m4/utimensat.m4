# utimensat.m4
# serial 14
dnl Copyright (C) 2009-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# See if we need to provide utimensat replacement.

# Written by Eric Blake.

AC_DEFUN([gl_FUNC_UTIMENSAT],
[
  AC_REQUIRE([gl_SYS_STAT_H_DEFAULTS])
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  gl_CHECK_FUNCS_ANDROID([utimensat], [[#include <sys/stat.h>]])
  if test $ac_cv_func_utimensat = no; then
    HAVE_UTIMENSAT=0
    case "$gl_cv_onwards_func_utimensat" in
      future*) REPLACE_UTIMENSAT=1 ;;
    esac
  else
    AC_CACHE_CHECK([whether utimensat works],
      [gl_cv_func_utimensat_works],
      [AC_RUN_IFELSE(
         [AC_LANG_PROGRAM([[
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
]GL_MDA_DEFINES],
            [[int result = 0;
              const char *f = "conftest.file";
              if (close (creat (f, 0600)))
                return 1;
              /* Test whether a trailing slash is handled correctly.
                 This fails on AIX 7.2.  */
              {
                struct timespec ts[2];
                ts[0].tv_sec = 345183300; ts[0].tv_nsec = 0;
                ts[1] = ts[0];
                if (utimensat (AT_FDCWD, "conftest.file/", ts, 0) == 0)
                  result |= 2;
              }
              /* Test whether the AT_SYMLINK_NOFOLLOW flag is supported.  */
              {
                if (utimensat (AT_FDCWD, f, NULL, AT_SYMLINK_NOFOLLOW))
                  result |= 4;
              }
              /* Test whether UTIME_NOW and UTIME_OMIT work.  */
              {
                struct timespec ts[2];
                ts[0].tv_sec = 1;
                ts[0].tv_nsec = UTIME_OMIT;
                ts[1].tv_sec = 1;
                ts[1].tv_nsec = UTIME_NOW;
                if (utimensat (AT_FDCWD, f, ts, 0))
                  result |= 8;
              }
              sleep (1);
              {
                struct stat st;
                struct timespec ts[2];
                ts[0].tv_sec = 1;
                ts[0].tv_nsec = UTIME_NOW;
                ts[1].tv_sec = 1;
                ts[1].tv_nsec = UTIME_OMIT;
                if (utimensat (AT_FDCWD, f, ts, 0))
                  result |= 8;
                if (stat (f, &st))
                  result |= 8;
                else if (st.st_ctime < st.st_atime)
                  result |= 16;
              }
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
              {
                struct timespec ts[2];
                ts[0].tv_sec = 1;
                ts[0].tv_nsec = UTIME_BOGUS_POS;
                ts[1].tv_sec = 1;
                ts[1].tv_nsec = 0;
                if (utimensat (AT_FDCWD, f, ts, 0) == 0)
                  result |= 32;
              }
              return result;
            ]])],
         [gl_cv_func_utimensat_works=yes],
         [case $? in
            2) gl_cv_func_utimensat_works='nearly' ;;
            *) gl_cv_func_utimensat_works=no ;;
          esac
         ],
         [case "$host_os" in
            # Guess yes on Linux or glibc systems.
            linux*)
              gl_cv_func_utimensat_works="guessing yes" ;;
            # Guess no on GNU/Hurd.
            gnu*)
              gl_cv_func_utimensat_works="guessing no" ;;
            # Guess yes on systems that emulate the Linux system calls.
            midipix*)
              gl_cv_func_utimensat_works="guessing yes" ;;
            # Guess 'nearly' on AIX.
            aix*)
              gl_cv_func_utimensat_works="guessing nearly" ;;
            # If we don't know, obey --enable-cross-guesses.
            *)
              gl_cv_func_utimensat_works="$gl_cross_guess_normal" ;;
          esac
         ])
      ])
    case "$gl_cv_func_utimensat_works" in
      *yes)
        ;;
      *nearly)
        AC_DEFINE([HAVE_NEARLY_WORKING_UTIMENSAT], [1],
          [Define to 1 if utimensat works, except for the trailing slash handling.])
        REPLACE_UTIMENSAT=1
        ;;
      *)
        REPLACE_UTIMENSAT=1
        ;;
    esac
  fi
])
