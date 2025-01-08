# memset_explicit.m4
# serial 4
dnl Copyright 2022-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_MEMSET_EXPLICIT],
[
  AC_REQUIRE([gl_STRING_H_DEFAULTS])
  dnl Persuade OpenSolaris derivatives' <string.h> to declare memset_s().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  gl_CHECK_FUNCS_ANDROID([memset_explicit], [[#include <string.h>]])
  if test $ac_cv_func_memset_explicit = no; then
    HAVE_MEMSET_EXPLICIT=0
    case "$gl_cv_onwards_func_memset_explicit" in
      future*) REPLACE_MEMSET_EXPLICIT=1 ;;
    esac
  fi
])

AC_DEFUN([gl_PREREQ_MEMSET_EXPLICIT],
[
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CHECK_FUNCS([explicit_memset])
  AC_CHECK_FUNCS_ONCE([memset_s])
  if test $ac_cv_func_memset_s = yes; then
    AC_CACHE_CHECK([for working memset_s],
      [gl_cv_func_memset_s_works],
      [AC_RUN_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <string.h>
              #include <stddef.h>
            ]], [[
              (void) memset_s (NULL, 0, '?', 0);
            ]])
         ],
         [gl_cv_func_memset_s_works=yes],
         [gl_cv_func_memset_s_works=no],
         [case "$host_os" in
            # Guess no on Solaris.
            solaris*)
              gl_cv_func_memset_s_works="guessing no" ;;
            *)
              gl_cv_func_memset_s_works="guessing yes" ;;
          esac
         ])
      ])
    case "$gl_cv_func_memset_s_works" in
      *yes)
        AC_DEFINE([HAVE_MEMSET_S_SUPPORTS_ZERO], [1],
          [Define to 1 if memset_s support zero-length operations.])
        ;;
    esac
  fi
])
