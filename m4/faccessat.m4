# serial 9
# See if we need to provide faccessat replacement.

dnl Copyright (C) 2009-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Written by Eric Blake.

AC_DEFUN([gl_FUNC_FACCESSAT_EOVERFLOW],
[
  AC_CHECK_FUNCS_ONCE([faccessat])
  if test "$ac_cv_func_faccessat" = yes; then
    AC_CACHE_CHECK([whether faccessat works when stat would EOVERFLOW],
      [gl_cv_func_faccessat_never_eoverflows],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([],
            [[#ifdef __linux__
               #include <linux/version.h>
               #if (! (KERNEL_VERSION (5, 8, 0) <= LINUX_VERSION_CODE \
                    && 2 < (__GLIBC__ + (33 <= __GLIBC_MINOR__))))
                #error "faccessat might fail with EOVERFLOW"
               #endif
              #endif
            ]])],
         [gl_cv_func_faccessat_never_eoverflows=yes],
         [gl_cv_func_faccessat_never_eoverflows=no])])
    if test "$gl_cv_func_faccessat_never_eoverflows" = yes; then
      AC_DEFINE([FACCESSAT_NEVER_EOVERFLOWS], 1,
        [Define to 1 if faccessat is EOVERFLOW-free.])
    fi
  fi
])

AC_DEFUN([gl_FUNC_FACCESSAT],
[
  AC_REQUIRE([gl_UNISTD_H_DEFAULTS])
  AC_REQUIRE([gl_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK])

  dnl Persuade glibc <unistd.h> to declare faccessat().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  AC_REQUIRE([gl_FUNC_FACCESSAT_EOVERFLOW])

  AC_CHECK_FUNCS_ONCE([faccessat])
  if test $ac_cv_func_faccessat = no; then
    HAVE_FACCESSAT=0
  else
    case $gl_cv_func_lstat_dereferences_slashed_symlink,$gl_cv_func_faccessat_never_eoverflows in
      *yes,*yes) ;;
      *)    REPLACE_FACCESSAT=1 ;;
    esac
  fi
])

# Prerequisites of lib/faccessat.c.
AC_DEFUN([gl_PREREQ_FACCESSAT],
[
  AC_CHECK_FUNCS([access])
])
