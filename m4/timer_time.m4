# timer_time.m4 serial 2
dnl Copyright (C) 2011-2013 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Check for timer_settime, and set LIB_TIMER_TIME.

AC_DEFUN([gl_TIMER_TIME],
[
  dnl Based on clock_time.m4. See details there.

  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  AC_REQUIRE([gl_THREADLIB])

  LIB_TIMER_TIME=
  AC_SUBST([LIB_TIMER_TIME])
  gl_saved_libs=$LIBS
    AC_SEARCH_LIBS([timer_settime], [rt posix4],
                   [test "$ac_cv_search_timer_settime" = "none required" ||
                    LIB_TIMER_TIME=$ac_cv_search_timer_settime])
    dnl GLIBC uses threads to emulate posix timers when kernel support
    dnl is not available (like Linux < 2.6 or when used with kFreeBSD)
    dnl Now the pthread lib is linked automatically in the normal case,
    dnl but when linking statically, it needs to be explicitly specified.
    AC_EGREP_CPP([Thread],
      [
#include <features.h>
#ifdef __GNU_LIBRARY__
 #if ((__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2) || (__GLIBC__ > 2)) \
     && !defined __UCLIBC__
  Thread emulation available
 #endif
#endif
      ],
      [LIB_TIMER_TIME="$LIB_TIMER_TIME $LIBMULTITHREAD"])
    AC_CHECK_FUNCS([timer_settime])
  LIBS=$gl_saved_libs
])
