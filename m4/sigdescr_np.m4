# sigdescr_np.m4 serial 2
dnl Copyright (C) 2020-2022 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_SIGDESCR_NP],
[
  dnl Persuade glibc <string.h> to declare sigdescr_np().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_REQUIRE([gl_STRING_H_DEFAULTS])
  AC_CHECK_FUNCS([sigdescr_np])
  if test $ac_cv_func_sigdescr_np = no; then
    HAVE_SIGDESCR_NP=0
  fi
])
