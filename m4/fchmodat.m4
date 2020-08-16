# fchmodat.m4 serial 5
dnl Copyright (C) 2004-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Written by Jim Meyering.

AC_DEFUN([gl_FUNC_FCHMODAT],
[
  AC_REQUIRE([gl_SYS_STAT_H_DEFAULTS])
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CHECK_FUNCS_ONCE([fchmodat lchmod])
  if test $ac_cv_func_fchmodat != yes; then
    HAVE_FCHMODAT=0
  else
    AC_CACHE_CHECK(
      [whether fchmodat+AT_SYMLINK_NOFOLLOW works on non-symlinks],
      [gl_cv_func_fchmodat_works],
      [dnl This test fails on GNU/Linux with glibc 2.31 (but not on
       dnl GNU/kFreeBSD nor GNU/Hurd) and Cygwin 2.9.
       AC_RUN_IFELSE(
         [AC_LANG_PROGRAM(
            [
              AC_INCLUDES_DEFAULT[
              #include <fcntl.h>
              #ifndef S_IRUSR
               #define S_IRUSR 0400
              #endif
              #ifndef S_IWUSR
               #define S_IWUSR 0200
              #endif
              #ifndef S_IRWXU
               #define S_IRWXU 0700
              #endif
              #ifndef S_IRWXG
               #define S_IRWXG 0070
              #endif
              #ifndef S_IRWXO
               #define S_IRWXO 0007
              #endif
            ]GL_MDA_DEFINES],
            [[
              int permissive = S_IRWXU | S_IRWXG | S_IRWXO;
              int desired = S_IRUSR | S_IWUSR;
              static char const f[] = "conftest.fchmodat";
              struct stat st;
              if (creat (f, permissive) < 0)
                return 1;
              if (fchmodat (AT_FDCWD, f, desired, AT_SYMLINK_NOFOLLOW) != 0)
                return 1;
              if (stat (f, &st) != 0)
                return 1;
              return ! ((st.st_mode & permissive) == desired);
            ]])],
         [gl_cv_func_fchmodat_works=yes],
         [gl_cv_func_fchmodat_works=no],
         [case "$host_os" in
            dnl Guess no on Linux with glibc and Cygwin, yes otherwise.
            linux-gnu* | cygwin*) gl_cv_func_fchmodat_works="guessing no" ;;
            *)                    gl_cv_func_fchmodat_works="$gl_cross_guess_normal" ;;
          esac
         ])
       rm -f conftest.fchmodat])
    case $gl_cv_func_fchmodat_works in
      *yes) ;;
      *)
        AC_DEFINE([NEED_FCHMODAT_NONSYMLINK_FIX], [1],
          [Define to 1 if fchmodat+AT_SYMLINK_NOFOLLOW does not work right on non-symlinks.])
        REPLACE_FCHMODAT=1
        ;;
    esac
  fi
])

# Prerequisites of lib/fchmodat.c.
AC_DEFUN([gl_PREREQ_FCHMODAT],
[
  AC_CHECK_FUNCS_ONCE([lchmod])
  :
])
