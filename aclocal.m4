dnl The following are from prerelease autoconf 2.14a.  When 2.14 is
dnl released, we should be able to zap them and AC_PREREQ(2.14).


# AC_PROG_CC_STDC
# ---------------
# If the C compiler in not in ANSI C mode by default, try to add an
# option to output variable @code{CC} to make it so.  This macro tries
# various options that select ANSI C on some system or another.  It
# considers the compiler to be in ANSI C mode if it handles function
# prototypes correctly.
AC_DEFUN(AC_PROG_CC_STDC,
[AC_REQUIRE([AC_PROG_CC])dnl
AC_BEFORE([$0], [AC_C_INLINE])dnl
AC_BEFORE([$0], [AC_C_CONST])dnl
dnl Force this before AC_PROG_CPP.  Some cpp's, eg on HPUX, require
dnl a magic option to avoid problems with ANSI preprocessor commands
dnl like #elif.
dnl FIXME: can't do this because then AC_AIX won't work due to a
dnl circular dependency.
dnl AC_BEFORE([$0], [AC_PROG_CPP])
AC_MSG_CHECKING(for ${CC-cc} option to accept ANSI C)
AC_CACHE_VAL(ac_cv_prog_cc_stdc,
[ac_cv_prog_cc_stdc=no
ac_save_CC="$CC"
# Don't try gcc -ansi; that turns off useful extensions and
# breaks some systems' header files.
# AIX			-qlanglvl=ansi
# Ultrix and OSF/1	-std1
# HP-UX 10.20 and later	-Ae
# HP-UX older versions	-Aa -D_HPUX_SOURCE
# SVR4			-Xc -D__EXTENSIONS__
for ac_arg in "" -qlanglvl=ansi -std1 -Ae "-Aa -D_HPUX_SOURCE" "-Xc -D__EXTENSIONS__"
do
  CC="$ac_save_CC $ac_arg"
  AC_TRY_COMPILE(
[#include <stdarg.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
/* Most of the following tests are stolen from RCS 5.7's src/conf.sh.  */
struct buf { int x; };
FILE * (*rcsopen) (struct buf *, struct stat *, int);
static char *e (p, i)
     char **p;
     int i;
{
  return p[i];
}
static char *f (char * (*g) (char **, int), char **p, ...)
{
  char *s;
  va_list v;
  va_start (v,p);
  s = g (p, va_arg (v,int));
  va_end (v);
  return s;
}
int test (int i, double x);
struct s1 {int (*f) (int a);};
struct s2 {int (*f) (double a);};
int pairnames (int, char **, FILE *(*)(struct buf *, struct stat *, int), int, int);
int argc;
char **argv;],
[return f (e, argv, 0) != argv[0]  ||  f (e, argv, 1) != argv[1];],
[ac_cv_prog_cc_stdc="$ac_arg"; break])
done
CC="$ac_save_CC"
])
case "x$ac_cv_prog_cc_stdc" in
  x|xno)
    AC_MSG_RESULT([none needed]) ;;
  *)
    AC_MSG_RESULT($ac_cv_prog_cc_stdc)
    CC="$CC $ac_cv_prog_cc_stdc" ;;
esac
])# AC_PROG_CC_STDC

# AC_C_VOLATILE
# -------------
# Note that, unlike const, #defining volatile to be the empty string can
# actually turn a correct program into an incorrect one, since removing
# uses of volatile actually grants the compiler permission to perform
# optimizations that could break the user's code.  So, do not #define
# volatile away unless it is really necessary to allow the user's code
# to compile cleanly.  Benign compiler failures should be tolerated.
AC_DEFUN(AC_C_VOLATILE,
[AC_REQUIRE([AC_PROG_CC_STDC])dnl
AC_CACHE_CHECK([for working volatile], ac_cv_c_volatile,
[AC_TRY_COMPILE(,[
volatile int x;
int * volatile y;],
ac_cv_c_volatile=yes, ac_cv_c_volatile=no)])
if test $ac_cv_c_volatile = no; then
  AC_DEFINE(volatile,,
            [Define to empty if the keyword `volatile' does not work.
             Warning: valid code using `volatile' can become incorrect
             without.  Disable with care.])
fi
])

# AC_C_PROTOTYPES
# ---------------
# Check if the C compiler supports prototypes, included if it needs
# options.
AC_DEFUN(AC_C_PROTOTYPES,
[AC_REQUIRE([AC_PROG_CC_STDC])dnl
AC_REQUIRE([AC_PROG_CPP])dnl
AC_MSG_CHECKING([for function prototypes])
if test "$ac_cv_prog_cc_stdc" != no; then
  AC_MSG_RESULT(yes)
  AC_DEFINE(PROTOTYPES, 1,
            [Define if the compiler supports function prototypes.])
else
  AC_MSG_RESULT(no)
fi
])# AC_C_PROTOTYPES
