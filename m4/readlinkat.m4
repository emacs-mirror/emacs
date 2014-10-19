# serial 4
# See if we need to provide readlinkat replacement.

dnl Copyright (C) 2009-2014 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Written by Eric Blake.

AC_DEFUN([gl_FUNC_READLINKAT],
[
  AC_REQUIRE([gl_UNISTD_H_DEFAULTS])
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  AC_CHECK_FUNCS_ONCE([readlinkat])
  if test $ac_cv_func_readlinkat = no; then
    HAVE_READLINKAT=0
  else
    AC_CACHE_CHECK([whether readlinkat signature is correct],
      [gl_cv_decl_readlinkat_works],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
           [[#include <unistd.h>
             /* Check whether original declaration has correct type.  */
             ssize_t readlinkat (int, char const *, char *, size_t);]])],
         [gl_cv_decl_readlinkat_works=yes],
         [gl_cv_decl_readlinkat_works=no])])
    if test "$gl_cv_decl_readlink_works" != yes; then
      REPLACE_READLINKAT=1
    fi
  fi
])
