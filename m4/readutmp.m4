# readutmp.m4
# serial 32
dnl Copyright (C) 2002-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_READUTMP],
[
  AC_REQUIRE([gl_SYSTEMD_CHOICE])

  dnl Set READUTMP_LIB to '-lsystemd' or '', depending on whether use of
  dnl systemd APIs is possible and desired (only the systemd login API, here).
  dnl AC_LIB_LINKFLAGS_BODY would be overkill here, since few people install
  dnl libsystemd in non-system directories.
  READUTMP_LIB=
  if test "$SYSTEMD_CHOICE" = yes; then
    AC_CHECK_HEADER([systemd/sd-login.h])
    if test $ac_cv_header_systemd_sd_login_h = yes; then
      AC_CACHE_CHECK([for libsystemd version >= 254],
        [gl_cv_lib_readutmp_systemd],
        [gl_saved_LIBS="$LIBS"
         LIBS="$LIBS -lsystemd"
         AC_LINK_IFELSE(
           [AC_LANG_PROGRAM([[
              #include <stdint.h>
              #include <systemd/sd-login.h>
              ]], [[
              uint64_t st;
              sd_session_get_start_time ("1", &st);
              ]])
           ],
           [gl_cv_lib_readutmp_systemd=yes],
           [gl_cv_lib_readutmp_systemd=no])
         LIBS="$gl_saved_LIBS"
        ])
      if test $gl_cv_lib_readutmp_systemd = yes; then
        AC_DEFINE([READUTMP_USE_SYSTEMD], [1],
          [Define if the readutmp module should use the systemd login API.])
        READUTMP_LIB='-lsystemd'
      fi
    fi
  fi
  AC_SUBST([READUTMP_LIB])

  gl_PREREQ_READUTMP_H
])

# Prerequisites of readutmp.h and boot-time-aux.h.
AC_DEFUN_ONCE([gl_PREREQ_READUTMP_H],
[
  dnl Persuade utmpx.h to declare utmpxname
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_CHECK_HEADERS_ONCE([utmp.h utmpx.h])
  if test $ac_cv_header_utmp_h = yes || test $ac_cv_header_utmpx_h = yes; then
    dnl Prerequisites of lib/readutmp.h and lib/readutmp.c.
    AC_CHECK_FUNCS_ONCE([utmpname utmpxname])
    AC_CHECK_DECLS([endutent],,,[[
/* <sys/types.h> is a prerequisite of <utmp.h> on FreeBSD 8.0, OpenBSD 4.6.  */
#include <sys/types.h>
#ifdef HAVE_UTMP_H
# include <utmp.h>
#endif
]])
    utmp_includes="\
AC_INCLUDES_DEFAULT
#ifdef HAVE_UTMPX_H
# include <utmpx.h>
#endif
#ifdef HAVE_UTMP_H
# if defined _THREAD_SAFE && defined UTMP_DATA_INIT
   /* When including both utmp.h and utmpx.h on AIX 4.3, with _THREAD_SAFE
      defined, work around the duplicate struct utmp_data declaration.  */
#  define utmp_data gl_aix_4_3_workaround_utmp_data
# endif
# include <utmp.h>
#endif
"
    AC_CHECK_MEMBERS([struct utmpx.ut_user],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_user],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmpx.ut_name],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_name],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmpx.ut_type],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_type],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmpx.ut_pid],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_pid],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_tv],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmpx.ut_host],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_host],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmpx.ut_id],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_id],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmpx.ut_session],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_session],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmpx.ut_exit],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_exit],,,[$utmp_includes])

    AC_CHECK_MEMBERS([struct utmpx.ut_exit.e_exit],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_exit.e_exit],,,[$utmp_includes])

    AC_CHECK_MEMBERS([struct utmpx.ut_exit.e_termination],,,[$utmp_includes])
    AC_CHECK_MEMBERS([struct utmp.ut_exit.e_termination],,,[$utmp_includes])
  fi

  AC_CHECK_DECLS([sysinfo],,,[[
    #include <sys/sysinfo.h>
    ]])

  AC_CHECK_HEADERS_ONCE([sys/param.h])
  dnl <sys/sysctl.h> requires <sys/param.h> on OpenBSD 4.0.
  AC_CHECK_HEADERS([sys/sysctl.h],,,
    [AC_INCLUDES_DEFAULT
     #if HAVE_SYS_PARAM_H
     # include <sys/param.h>
     #endif
    ])
  AC_CHECK_FUNCS([sysctl])

  AC_CHECK_HEADERS_ONCE([OS.h])
])
