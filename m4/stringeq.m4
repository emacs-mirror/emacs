# stringeq.m4
# serial 1
dnl Copyright (C) 2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_STREQ],
[
  AC_REQUIRE([gl_STRING_H_DEFAULTS])
  AC_CHECK_DECLS_ONCE([streq])
  if test $ac_cv_have_decl_streq != no; then
    HAVE_DECL_STREQ=1
  fi
])

AC_DEFUN([gl_FUNC_MEMEQ],
[
  AC_REQUIRE([gl_STRING_H_DEFAULTS])
  AC_CHECK_DECLS_ONCE([memeq])
  if test $ac_cv_have_decl_memeq != no; then
    HAVE_DECL_MEMEQ=1
  fi
])
