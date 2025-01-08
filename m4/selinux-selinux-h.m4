# selinux-selinux-h.m4
# serial 10   -*- Autoconf -*-
dnl Copyright (C) 2006-2007, 2009-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# From Jim Meyering
# Provide <selinux/selinux.h>, if necessary.
# If it is already present, provide wrapper functions to guard against
# misbehavior from getfilecon, lgetfilecon, and fgetfilecon.

AC_DEFUN([gl_HEADERS_SELINUX_SELINUX_H],
[
  AC_REQUIRE([gl_CHECK_HEADER_SELINUX_SELINUX_H])
  if test "$with_selinux" != no; then
    if test "$ac_cv_header_selinux_selinux_h" = yes; then
      # We do have <selinux/selinux.h>, so do compile getfilecon.c
      # and arrange to use its wrappers.
      gl_CHECK_NEXT_HEADERS([selinux/selinux.h])
      AC_DEFINE([getfilecon], [rpl_getfilecon],
                [Always use our getfilecon wrapper.])
      AC_DEFINE([getfilecon_raw], [rpl_getfilecon_raw],
                [Always use our getfilecon_raw wrapper.])
      AC_DEFINE([lgetfilecon], [rpl_lgetfilecon],
                [Always use our lgetfilecon wrapper.])
      AC_DEFINE([lgetfilecon_raw], [rpl_lgetfilecon_raw],
                [Always use our lgetfilecon_raw wrapper.])
      AC_DEFINE([fgetfilecon], [rpl_fgetfilecon],
                [Always use our fgetfilecon wrapper.])
      AC_DEFINE([fgetfilecon_raw], [rpl_fgetfilecon_raw],
                [Always use our fgetfilecon_raw wrapper.])
    fi
  fi
])

# Check for <selinux/selinux.h>, if necessary.

AC_DEFUN([gl_CHECK_HEADER_SELINUX_SELINUX_H],
[
  AC_REQUIRE([gl_LIBSELINUX])
  if test "$with_selinux" != no; then
    AC_CHECK_HEADERS_ONCE([selinux/selinux.h])

    if test $ac_cv_header_selinux_selinux_h = yes; then
      USE_SELINUX_SELINUX_H=1
    else
      USE_SELINUX_SELINUX_H=0
    fi

    case "$ac_cv_search_setfilecon:$ac_cv_header_selinux_selinux_h" in
      no:*) # already warned
        ;;
      *:no)
        AC_MSG_WARN([libselinux was found but selinux/selinux.h is missing.])
        AC_MSG_WARN([AC_PACKAGE_NAME will be compiled without SELinux support.])
        ;;
    esac
  else
    # Do as if <selinux/selinux.h> does not exist, even if
    # AC_CHECK_HEADERS_ONCE has already determined that it exists.
    USE_SELINUX_SELINUX_H=0
  fi
  AC_SUBST([USE_SELINUX_SELINUX_H])
  AC_DEFINE_UNQUOTED([USE_SELINUX_SELINUX_H], [$USE_SELINUX_SELINUX_H],
    [Define to 1 if <selinux/selinux.h> should be used, to 0 otherwise.])
])

AC_DEFUN([gl_LIBSELINUX],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([AC_CANONICAL_BUILD])

  AC_ARG_WITH([selinux],
    AS_HELP_STRING([[--without-selinux]], [do not use SELinux, even on systems with SELinux]),
    [], [with_selinux=maybe])

  LIB_SELINUX=
  if test "$with_selinux" != no; then
    gl_saved_LIBS=$LIBS
    AC_SEARCH_LIBS([setfilecon], [selinux],
                   [test "$ac_cv_search_setfilecon" = "none required" ||
                    LIB_SELINUX=$ac_cv_search_setfilecon])
    LIBS=$gl_saved_LIBS
  fi
  AC_SUBST([LIB_SELINUX])

  # Warn if SELinux is found but libselinux is absent;
  if test "$ac_cv_search_setfilecon" = no; then
    if test "$host" = "$build" && test -d /selinux; then
      AC_MSG_WARN([This system supports SELinux but libselinux is missing.])
      AC_MSG_WARN([AC_PACKAGE_NAME will be compiled without SELinux support.])
    fi
    with_selinux=no
  fi
])
