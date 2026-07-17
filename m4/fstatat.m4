# fstatat.m4
# serial 8
dnl Copyright (C) 2004-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Written by Jim Meyering.

# If we have the fstatat function, and it has the bug (in AIX 7.1)
# that it does not fill in st_size correctly, use the replacement function.
AC_DEFUN([gl_FUNC_FSTATAT],
[
  AC_REQUIRE([gl_SYS_STAT_H_DEFAULTS])
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  AC_REQUIRE([gl_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK])
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_CHECK_FUNCS_ONCE([fstatat])

  if test $ac_cv_func_fstatat = no; then
    HAVE_FSTATAT=0
  else
    dnl Test for an AIX 7.1 bug; see
    dnl <https://lists.gnu.org/r/bug-tar/2011-09/msg00015.html>.
    AC_CACHE_CHECK([whether fstatat (..., 0) works],
      [gl_cv_func_fstatat_zero_flag],
      [AC_RUN_IFELSE(
         [AC_LANG_SOURCE(
            [[
              #include <fcntl.h>
              #include <sys/stat.h>
              int
              main (void)
              {
                struct stat a;
                return fstatat (AT_FDCWD, ".", &a, 0) != 0;
              }
            ]])],
         [gl_cv_func_fstatat_zero_flag=yes],
         [gl_cv_func_fstatat_zero_flag=no],
         [case "$host_os" in
            aix*) gl_cv_func_fstatat_zero_flag="guessing no";;
            *)    gl_cv_func_fstatat_zero_flag="guessing yes";;
          esac
         ])
      ])
    AS_CASE([$gl_cv_func_fstatat_zero_flag],
      [*yes],
        [AC_DEFINE([HAVE_WORKING_FSTATAT_ZERO_FLAG], [1],
           [Define to 1 if fstatat (..., 0) works.
            For example, it does not work in AIX 7.1.])])

    case $gl_cv_func_fstatat_zero_flag+$gl_cv_func_lstat_dereferences_slashed_symlink in
    *yes+*yes) ;;
    *) REPLACE_FSTATAT=1 ;;
    esac

    case $host_os in
      darwin* | solaris*)
        REPLACE_FSTATAT=1 ;;
    esac

    dnl Check for the AT_EMPTY_PATH compatibility issue with null pointers
    dnl only if not already replacing fstatat.
    dnl There is no need to AC_DEFINE anything here, as the Gnulib
    dnl replacement works around the compatibility bug even if the bug
    dnl is not present, and it is not worth the trouble to tune this.
    AS_CASE([$REPLACE_FSTATAT],
      [0],
        [AC_CACHE_CHECK([for no AT_EMPTY_PATH or fstatat with null file],
           [gl_cv_func_fstatat_null_file],
           [AC_RUN_IFELSE(
              [AC_LANG_PROGRAM(
                 [[#include <stddef.h>
                   #include <fcntl.h>
                   #include <sys/stat.h>
                   #ifndef AT_EMPTY_PATH
                    #define AT_EMPTY_PATH 0
                   #endif
                   /* Don't check via -Wnonnull, as the problem could in
                      theory exist with compilers lacking -Wnonnull.  */
                   #if __GLIBC__ && ! (2 < __GLIBC__ + (41 <= __GLIBC_MINOR__))
                    #error "glibc 2.40 and earlier can fail with null file"
                   #endif
                 ]],
                 [[struct stat st;
                   if (!AT_EMPTY_PATH)
                     return 0; /* No need to replace fstatat.  */
                   if (fstatat (AT_FDCWD, NULL, &st, AT_EMPTY_PATH) < 0)
                     return 1;
                   int fd = open (".", O_RDONLY);
                   if (fd < 0)
                     return 1; /* Play it safe.  */
                   return fstatat (fd, NULL, &st, AT_EMPTY_PATH) < 0;
                 ]])],
              [gl_cv_func_fstatat_null_file=yes],
              [gl_cv_func_fstatat_null_file=no],
              [gl_cv_func_fstatat_null_file="guessing no"])])
         AS_CASE([$gl_cv_func_fstatat_null_file],
           [*no],
             [REPLACE_FSTATAT=1])])
  fi
])
