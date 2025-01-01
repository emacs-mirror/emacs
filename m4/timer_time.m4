# timer_time.m4
# serial 6
dnl Copyright (C) 2011-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Check for timer_settime, and set TIMER_TIME_LIB.

AC_DEFUN([gl_TIMER_TIME],
[
  dnl Based on clock_time.m4. See details there.

  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  dnl Test whether the gnulib module 'threadlib' is in use.
  dnl Some packages like Emacs use --avoid=threadlib.
  dnl Write the symbol in such a way that it does not cause 'aclocal' to pick
  dnl the threadlib.m4 file that is installed in $PREFIX/share/aclocal/.
  m4_ifdef([gl_][PTHREADLIB], [AC_REQUIRE([gl_][PTHREADLIB])])

  AC_CHECK_DECL([timer_settime], [], [],
                [[#include <time.h>
                ]])
  TIMER_TIME_LIB=
  AC_SUBST([TIMER_TIME_LIB])
  AS_IF([test "$ac_cv_have_decl_timer_settime" = yes], [
    gl_saved_libs=$LIBS
    AC_SEARCH_LIBS([timer_settime], [rt posix4],
                   [test "$ac_cv_search_timer_settime" = "none required" ||
                    TIMER_TIME_LIB=$ac_cv_search_timer_settime])
    m4_ifdef([gl_][PTHREADLIB],
      [dnl GLIBC uses threads to emulate posix timers when kernel support
       dnl is not available (like Linux < 2.6 or when used with kFreeBSD)
       dnl Now the pthread lib is linked automatically in the normal case,
       dnl but when linking statically, it needs to be explicitly specified.
       AC_EGREP_CPP([Thread],
         [#include <features.h>
          #ifdef __GNU_LIBRARY__
           #if ((__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2) || (__GLIBC__ > 2)) \
               && !(__UCLIBC__ && __HAS_NO_THREADS__)
            Thread emulation available
           #endif
          #endif
         ],
         [TIMER_TIME_LIB="$TIMER_TIME_LIB $LIBPMULTITHREAD"])])
    AC_CHECK_FUNCS([timer_settime])
    LIBS=$gl_saved_libs
  ])
  dnl For backward compatibility.
  LIB_TIMER_TIME="$TIMER_TIME_LIB"
  AC_SUBST([LIB_TIMER_TIME])
])
