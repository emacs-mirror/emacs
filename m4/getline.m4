# getline.m4
# serial 35

dnl Copyright (C) 1998-2003, 2005-2007, 2009-2025 Free Software Foundation,
dnl Inc.
dnl
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_PREREQ([2.59])

dnl See if there's a working, system-supplied version of the getline function.
dnl We can't just do AC_REPLACE_FUNCS([getline]) because some systems
dnl have a function by that name in -linet that doesn't have anything
dnl to do with the function we need.
AC_DEFUN([gl_FUNC_GETLINE],
[
  AC_REQUIRE([gl_STDIO_H_DEFAULTS])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles

  dnl Persuade glibc <stdio.h> to declare getline().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_CHECK_DECLS_ONCE([getline])

  gl_CHECK_FUNCS_ANDROID([getline], [[#include <stdio.h>]])
  if test $ac_cv_func_getline = yes; then
    dnl Found it in some library.  Verify that it works.
    AC_CACHE_CHECK([for working getline function],
      [am_cv_func_working_getline],
      [echo fooNbarN | tr -d '\012' | tr N '\012' > conftest.data
       touch conftest.empty
       AC_RUN_IFELSE([AC_LANG_SOURCE([[
#    include <stdio.h>
#    include <stdlib.h>
#    include <string.h>
    int main ()
    {
      FILE *in = fopen ("./conftest.data", "r");
      int result = 0;
      if (!in)
        return 1;
      {
        /* Test result for a NULL buffer and a zero size.
           Based on a test program from Karl Heuer.  */
        char *line = NULL;
        size_t siz = 0;
        int len = getline (&line, &siz, in);
        if (!(len == 4 && line && strcmp (line, "foo\n") == 0))
          result |= 2;
        free (line);
      }
      {
        /* Test result for a NULL buffer and a non-zero size.
           This crashes on FreeBSD 8.0.  */
        char *line = NULL;
        size_t siz = (size_t)(~0) / 4;
        if (getline (&line, &siz, in) == -1)
          result |= 4;
        free (line);
      }
      fclose (in);
      {
        /* Test that reading EOF as the first character sets the first byte
           in the buffer to NUL.  This fails on glibc 2.42 and earlier.  */
        in = fopen ("./conftest.empty", "r");
        if (!in)
          return 1;
        char *line = malloc (1);
        line[0] = 'A';
        size_t siz = 1;
        if (getline (&line, &siz, in) != -1 || line[0] != '\0')
          result |= 8;
        free (line);
      }
      fclose (in);
      return result;
    }
    ]])],
         [am_cv_func_working_getline=yes],
         [am_cv_func_working_getline=no],
         [case "$host_os" in
                                # Guess yes on musl.
            *-musl* | midipix*) am_cv_func_working_getline="guessing yes" ;;
                                # Guess no on glibc.
            *-gnu* | gnu*)      am_cv_func_working_getline="guessing no" ;;
            *)                  am_cv_func_working_getline="$gl_cross_guess_normal" ;;
          esac
         ])
       rm -f conftest.data conftest.empty
      ])
  else
    am_cv_func_working_getline=no
    case "$gl_cv_onwards_func_getline" in
      future*) REPLACE_GETLINE=1 ;;
    esac
  fi

  if test $ac_cv_have_decl_getline = no; then
    HAVE_DECL_GETLINE=0
  fi

  case "$am_cv_func_working_getline" in
    *yes) ;;
    *)
      dnl Set REPLACE_GETLINE always: Even if we have not found the broken
      dnl getline function among $LIBS, it may exist in libinet and the
      dnl executable may be linked with -linet.
      REPLACE_GETLINE=1
      ;;
  esac
])

# Prerequisites of lib/getline.c.
AC_DEFUN([gl_PREREQ_GETLINE],
[
  :
])
