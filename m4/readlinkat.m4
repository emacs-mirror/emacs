# readlinkat.m4
# serial 10
dnl Copyright (C) 2009-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# See if we need to provide readlinkat replacement.

# Written by Eric Blake.

AC_DEFUN([gl_FUNC_READLINKAT],
[
  AC_REQUIRE([gl_UNISTD_H_DEFAULTS])
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  gl_CHECK_FUNCS_ANDROID([readlinkat], [[#include <unistd.h>]])
  AC_REQUIRE([gl_FUNC_READLINK])
  if test $ac_cv_func_readlinkat = no; then
    HAVE_READLINKAT=0
    case "$gl_cv_onwards_func_readlinkat" in
      future*) REPLACE_READLINKAT=1 ;;
    esac
  else
    AC_CACHE_CHECK([whether readlinkat signature is correct],
      [gl_cv_decl_readlinkat_works],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
           [[#include <unistd.h>
             /* Check whether original declaration has correct type.  */
             ssize_t readlinkat (int, char const *, char *, size_t);
           ]])
         ],
         [gl_cv_decl_readlinkat_works=yes],
         [gl_cv_decl_readlinkat_works=no])
      ])
    # Assume readlinkat has the same bugs as readlink,
    # as is the case on macOS 14 with trailing slashes.
    case $gl_cv_decl_readlinkat_works,$gl_cv_func_readlink_trailing_slash,$gl_cv_func_readlink_truncate in
      *yes,*yes,*yes)
        ;;
      *)
        REPLACE_READLINKAT=1
        ;;
    esac

    dnl On Cygwin 3.3.6, readlinkat(AT_FDCWD,"/dev/null") returns
    dnl "\\Device\\Null", which is unusable.
    case "$host_os" in
      cygwin*)
        REPLACE_READLINKAT=1
        ;;
    esac
  fi
])
