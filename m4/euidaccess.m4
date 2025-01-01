# euidaccess.m4
# serial 17
dnl Copyright (C) 2002-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_NONREENTRANT_EUIDACCESS],
[
  AC_REQUIRE([gl_FUNC_EUIDACCESS])
  AC_CHECK_DECLS([setregid])
  AC_DEFINE([PREFER_NONREENTRANT_EUIDACCESS], [1],
    [Define this if you prefer euidaccess to return the correct result
     even if this would make it nonreentrant.  Define this only if your
     entire application is safe even if the uid or gid might temporarily
     change.  If your application uses signal handlers or threads it
     is probably not safe.])
])

AC_DEFUN([gl_FUNC_EUIDACCESS],
[
  AC_REQUIRE([gl_UNISTD_H_DEFAULTS])

  dnl Persuade glibc <unistd.h> to declare euidaccess().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_CHECK_FUNCS([euidaccess])
  if test $ac_cv_func_euidaccess = no; then
    HAVE_EUIDACCESS=0
  fi
])

# Prerequisites of lib/euidaccess.c.
AC_DEFUN([gl_PREREQ_EUIDACCESS], [
  dnl Prefer POSIX faccessat over non-standard euidaccess.
  gl_CHECK_FUNCS_ANDROID([faccessat], [[#include <unistd.h>]])
  dnl Try various other non-standard fallbacks.
  AC_CHECK_HEADERS([libgen.h])
  AC_FUNC_GETGROUPS

  # Solaris 9 and 10 need -lgen to get the eaccess function.
  # Save and restore LIBS so -lgen isn't added to it.  Otherwise, *all*
  # programs in the package would end up linked with that potentially-shared
  # library, inducing unnecessary run-time overhead.
  EUIDACCESS_LIBGEN=
  AC_SUBST([EUIDACCESS_LIBGEN])
  gl_saved_libs=$LIBS
    AC_SEARCH_LIBS([eaccess], [gen],
                   [test "$ac_cv_search_eaccess" = "none required" ||
                    EUIDACCESS_LIBGEN=$ac_cv_search_eaccess])
    AC_CHECK_FUNCS([eaccess])
  LIBS=$gl_saved_libs
  # For backward compatibility.
  LIB_EACCESS="$EUIDACCESS_LIBGEN"
  AC_SUBST([LIB_EACCESS])
])
