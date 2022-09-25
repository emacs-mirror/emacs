# Check for GNU-style execinfo.h.

dnl Copyright 2012-2022 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_EXECINFO_H],
[
  AC_CHECK_HEADERS_ONCE([execinfo.h])

  LIB_EXECINFO=''
  GL_GENERATE_EXECINFO_H=true

  if test $ac_cv_header_execinfo_h = yes; then
    gl_saved_libs=$LIBS
      AC_SEARCH_LIBS([backtrace_symbols_fd], [execinfo],
        [test "$ac_cv_search_backtrace_symbols_fd" = "none required" ||
         LIB_EXECINFO=$ac_cv_search_backtrace_symbols_fd])
    LIBS=$gl_saved_libs
    if test "$ac_cv_search_backtrace_symbols_fd" != no; then
      GL_GENERATE_EXECINFO_H=false
    fi
  fi

  AC_SUBST([LIB_EXECINFO])
])
