# fchmodat.m4 serial 7
dnl Copyright (C) 2004-2022 Free Software Foundation, Inc.
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
      [whether fchmodat works],
      [gl_cv_func_fchmodat_works],
      [AC_RUN_IFELSE(
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
              int result = 0;
              #define file "conftest.fchmodat"
              struct stat st;
              if (creat (file, permissive) < 0)
                return 1;
              /* Test whether fchmodat rejects a trailing slash on a non-directory.
                 This test fails on AIX 7.2.  */
              if (fchmodat (AT_FDCWD, file "/", desired, 0) == 0)
                result |= 2;
              /* Test whether fchmodat+AT_SYMLINK_NOFOLLOW works on non-symlinks.
                 This test fails on GNU/Linux with glibc 2.31 (but not on
                 GNU/kFreeBSD nor GNU/Hurd) and Cygwin 2.9.  */
              if (fchmodat (AT_FDCWD, file, desired, AT_SYMLINK_NOFOLLOW) != 0)
                result |= 4;
              if (stat (file, &st) != 0)
                return 1;
              if ((st.st_mode & permissive) != desired)
                result |= 4;
              return result;
            ]])],
         [gl_cv_func_fchmodat_works=yes],
         [case $? in
            2) gl_cv_func_fchmodat_works='nearly' ;;
            *) gl_cv_func_fchmodat_works=no ;;
          esac
         ],
         [case "$host_os" in
                                  # Guess no on Linux with glibc and Cygwin.
            linux-gnu* | cygwin*) gl_cv_func_fchmodat_works="guessing no" ;;
                                  # Guess 'nearly' on AIX.
            aix*)                 gl_cv_func_fchmodat_works="guessing nearly" ;;
                                  # If we don't know, obey --enable-cross-guesses.
            *)                    gl_cv_func_fchmodat_works="$gl_cross_guess_normal" ;;
          esac
         ])
       rm -f conftest.fchmodat])
    case "$gl_cv_func_fchmodat_works" in
      *yes) ;;
      *nearly)
        AC_DEFINE([HAVE_NEARLY_WORKING_FCHMODAT], [1],
          [Define to 1 if fchmodat works, except for the trailing slash handling.])
        REPLACE_FCHMODAT=1
        ;;
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
  AC_CHECK_FUNCS_ONCE([readlinkat])
  :
])
