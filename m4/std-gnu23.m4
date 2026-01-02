# std-gnu23.m4
# serial 5

# Prefer GNU C23 to earlier versions.

# This implementation is taken from GNU Autoconf lib/autoconf/c.m4
# commit d5d33c599143f6c36406eb860571fced1da49ea4
# dated Sun Nov 17 09:00:49 2024 -0800
# This implementation will be obsolete once we can assume Autoconf 2.73
# or later is installed everywhere a Gnulib program might be developed.

m4_version_prereq([2.73], [], [


# Copyright (C) 2001-2026 Free Software Foundation, Inc.

# This file is part of Autoconf.  This program is free
# software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the Autoconf Configure Script Exception,
# version 3.0, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License
# and a copy of the Autoconf Configure Script Exception along with
# this program; see the files COPYINGv3 and COPYING.EXCEPTION
# respectively.  If not, see <https://www.gnu.org/licenses/> and
# <https://git.savannah.gnu.org/gitweb/?p=autoconf.git;a=blob_plain;f=COPYING.EXCEPTION>.

# Written by David MacKenzie, with help from
# Akim Demaille, Paul Eggert,
# François Pinard, Karl Berry, Richard Pixley, Ian Lance Taylor,
# Roland McGrath, Noah Friedman, david d zuhn, and many others.


# AC_PROG_CC([COMPILER ...])
# --------------------------
# COMPILER ... is a space separated list of C compilers to search for.
# This just gives the user an opportunity to specify an alternative
# search list for the C compiler.
AC_DEFUN([AC_PROG_CC],
[AC_LANG_PUSH(C)dnl
AC_ARG_VAR([CC],     [C compiler command])dnl
AC_ARG_VAR([CFLAGS], [C compiler flags])dnl
_AC_ARG_VAR_LDFLAGS()dnl
_AC_ARG_VAR_LIBS()dnl
_AC_ARG_VAR_CPPFLAGS()dnl
m4_ifval([$1],
      [AC_CHECK_TOOLS(CC, [$1])],
[AC_CHECK_TOOL(CC, gcc)
if test -z "$CC"; then
  dnl Here we want:
  dnl	AC_CHECK_TOOL(CC, cc)
  dnl but without the check for a tool without the prefix.
  dnl Until the check is removed from there, copy the code:
  if test -n "$ac_tool_prefix"; then
    AC_CHECK_PROG(CC, [${ac_tool_prefix}cc], [${ac_tool_prefix}cc])
  fi
fi
if test -z "$CC"; then
  AC_CHECK_PROG(CC, cc, cc, , , /usr/ucb/cc)
fi
if test -z "$CC"; then
  AC_CHECK_TOOLS(CC, cl.exe)
fi
if test -z "$CC"; then
  AC_CHECK_TOOL(CC, clang)
fi
])

test -z "$CC" && AC_MSG_FAILURE([no acceptable C compiler found in \$PATH])

# Provide some information about the compiler.
_AS_ECHO_LOG([checking for _AC_LANG compiler version])
set X $ac_compile
ac_compiler=$[2]
for ac_option in --version -v -V -qversion -version; do
  _AC_DO_LIMIT([$ac_compiler $ac_option >&AS_MESSAGE_LOG_FD])
done

m4_expand_once([_AC_COMPILER_EXEEXT])[]dnl
m4_expand_once([_AC_COMPILER_OBJEXT])[]dnl
_AC_LANG_COMPILER_GNU
if test $ac_compiler_gnu = yes; then
  GCC=yes
else
  GCC=
fi
_AC_PROG_CC_G
_AC_PROG_CC_STDC_EDITION
AC_LANG_POP(C)dnl
])# AC_PROG_CC

AC_DEFUN([_AC_C_C89_TEST_GLOBALS],
[m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C89 (global declarations)
ac_c_conftest_c89_globals='
/* Do not test the value of __STDC__, because some compilers define it to 0
   or do not define it, while otherwise adequately conforming.  */

#include <stddef.h>
#include <stdarg.h>
struct stat;
/* Most of the following tests are stolen from RCS 5.7 src/conf.sh.  */
struct buf { int x; };
struct buf * (*rcsopen) (struct buf *, struct stat *, int);
static char *e (char **p, int i)
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

/* C89 style stringification. */
#define noexpand_stringify(a) #a
const char *stringified = noexpand_stringify(arbitrary+token=sequence);

/* C89 style token pasting.  Exercises some of the corner cases that
   e.g. old MSVC gets wrong, but not very hard. */
#define noexpand_concat(a,b) a##b
#define expand_concat(a,b) noexpand_concat(a,b)
extern int vA;
extern int vbee;
#define aye A
#define bee B
int *pvA = &expand_concat(v,aye);
int *pvbee = &noexpand_concat(v,bee);

/* OSF 4.0 Compaq cc is some sort of almost-ANSI by default.  It has
   function prototypes and stuff, but not \xHH hex character constants.
   These do not provoke an error unfortunately, instead are silently treated
   as an "x".  The following induces an error, until -std is added to get
   proper ANSI mode.  Curiously \x00 != x always comes out true, for an
   array size at least.  It is necessary to write \x00 == 0 to get something
   that is true only with -std.  */
int osf4_cc_array ['\''\x00'\'' == 0 ? 1 : -1];

/* IBM C 6 for AIX is almost-ANSI by default, but it replaces macro parameters
   inside strings and character constants.  */
#define FOO(x) '\''x'\''
int xlc6_cc_array[FOO(a) == '\''x'\'' ? 1 : -1];

int test (int i, double x);
struct s1 {int (*f) (int a);};
struct s2 {int (*f) (double a);};
int pairnames (int, char **, int *(*)(struct buf *, struct stat *, int),
               int, int);'
]])])

AC_DEFUN([_AC_C_C89_TEST_MAIN],
[m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C89 (body of main).
ac_c_conftest_c89_main='
ok |= (argc == 0 || f (e, argv, 0) != argv[0] || f (e, argv, 1) != argv[1]);
'
]])])

AC_DEFUN([_AC_C_C99_TEST_GLOBALS],
[m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C99 (global declarations)
ac_c_conftest_c99_globals='
/* Does the compiler advertise C99 conformance? */
#if !defined __STDC_VERSION__ || __STDC_VERSION__ < 199901L
# error "Compiler does not advertise C99 conformance"
#endif

// See if C++-style comments work.

#include <stdbool.h>
extern int puts (const char *);
extern int printf (const char *, ...);
extern int dprintf (int, const char *, ...);
extern void *malloc (size_t);
extern void free (void *);

// Check varargs macros.  These examples are taken from C99 6.10.3.5.
// dprintf is used instead of fprintf to avoid needing to declare
// FILE and stderr.
#define debug(...) dprintf (2, __VA_ARGS__)
#define showlist(...) puts (#__VA_ARGS__)
#define report(test,...) ((test) ? puts (#test) : printf (__VA_ARGS__))
static void
test_varargs_macros (void)
{
  int x = 1234;
  int y = 5678;
  debug ("Flag");
  debug ("X = %d\n", x);
  showlist (The first, second, and third items.);
  report (x>y, "x is %d but y is %d", x, y);
}

// Check long long types.
#define BIG64 18446744073709551615ull
#define BIG32 4294967295ul
#define BIG_OK (BIG64 / BIG32 == 4294967297ull && BIG64 % BIG32 == 0)
#if !BIG_OK
  #error "your preprocessor is broken"
#endif
#if BIG_OK
#else
  #error "your preprocessor is broken"
#endif
static long long int bignum = -9223372036854775807LL;
static unsigned long long int ubignum = BIG64;

struct incomplete_array
{
  int datasize;
  double data[];
};

struct named_init {
  int number;
  const wchar_t *name;
  double average;
};

typedef const char *ccp;

static inline int
test_restrict (ccp restrict text)
{
  // Iterate through items via the restricted pointer.
  // Also check for declarations in for loops.
  for (unsigned int i = 0; *(text+i) != '\''\0'\''; ++i)
    continue;
  return 0;
}

// Check varargs and va_copy.
static bool
test_varargs (const char *format, ...)
{
  va_list args;
  va_start (args, format);
  va_list args_copy;
  va_copy (args_copy, args);

  const char *str = "";
  int number = 0;
  float fnumber = 0;

  while (*format)
    {
      switch (*format++)
	{
	case '\''s'\'': // string
	  str = va_arg (args_copy, const char *);
	  break;
	case '\''d'\'': // int
	  number = va_arg (args_copy, int);
	  break;
	case '\''f'\'': // float
	  fnumber = va_arg (args_copy, double);
	  break;
	default:
	  break;
	}
    }
  va_end (args_copy);
  va_end (args);

  return *str && number && fnumber;
}
'
]])])

AC_DEFUN([_AC_C_C99_TEST_MAIN],
[m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C99 (body of main).
ac_c_conftest_c99_main='
  // Check bool.
  _Bool success = false;
  success |= (argc != 0);

  // Check restrict.
  if (test_restrict ("String literal") == 0)
    success = true;
  char *restrict newvar = "Another string";

  // Check varargs.
  success &= test_varargs ("s, d'\'' f .", "string", 65, 34.234);
  test_varargs_macros ();

  // Check flexible array members.
  struct incomplete_array *ia =
    malloc (sizeof (struct incomplete_array) + (sizeof (double) * 10));
  ia->datasize = 10;
  for (int i = 0; i < ia->datasize; ++i)
    ia->data[i] = i * 1.234;
  // Work around memory leak warnings.
  free (ia);

  // Check named initializers.
  struct named_init ni = {
    .number = 34,
    .name = L"Test wide string",
    .average = 543.34343,
  };

  ni.number = 58;

  // Do not test for VLAs, as some otherwise-conforming compilers lack them.
  // C code should instead use __STDC_NO_VLA__; see Autoconf manual.

  // work around unused variable warnings
  ok |= (!success || bignum == 0LL || ubignum == 0uLL || newvar[0] == '\''x'\''
	 || ni.number != 58);
'
]])])

AC_DEFUN([_AC_C_C11_TEST_GLOBALS],
[m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C11 (global declarations)
ac_c_conftest_c11_globals='
/* Does the compiler advertise C11 conformance? */
#if !defined __STDC_VERSION__ || __STDC_VERSION__ < 201112L
# error "Compiler does not advertise C11 conformance"
#endif

// Check _Alignas.
char _Alignas (double) aligned_as_double;
char _Alignas (0) no_special_alignment;
extern char aligned_as_int;
char _Alignas (0) _Alignas (int) aligned_as_int;

// Check _Alignof.
enum
{
  int_alignment = _Alignof (int),
  int_array_alignment = _Alignof (int[100]),
  char_alignment = _Alignof (char)
};
_Static_assert (0 < -_Alignof (int), "_Alignof is signed");

// Check _Noreturn.
int _Noreturn does_not_return (void) { for (;;) continue; }

// Check _Static_assert.
struct test_static_assert
{
  int x;
  _Static_assert (sizeof (int) <= sizeof (long int),
                  "_Static_assert does not work in struct");
  long int y;
};

// Check UTF-8 literals.
#define u8 syntax error!
char const utf8_literal[] = u8"happens to be ASCII" "another string";

// Check duplicate typedefs.
typedef long *long_ptr;
typedef long int *long_ptr;
typedef long_ptr long_ptr;

// Anonymous structures and unions -- taken from C11 6.7.2.1 Example 1.
struct anonymous
{
  union {
    struct { int i; int j; };
    struct { int k; long int l; } w;
  };
  int m;
} v1;
'
]])])

AC_DEFUN([_AC_C_C11_TEST_MAIN],
[m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C11 (body of main).
ac_c_conftest_c11_main='
  _Static_assert ((offsetof (struct anonymous, i)
		   == offsetof (struct anonymous, w.k)),
		  "Anonymous union alignment botch");
  v1.i = 2;
  v1.w.k = 5;
  ok |= v1.i != 5;
'
]])])

AC_DEFUN([_AC_C_C23_TEST_GLOBALS],
[m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C23 (global declarations)
ac_c_conftest_c23_globals='
/* Does the compiler advertise conformance to C17 or earlier?
   Although GCC 14 does not do that, even with -std=gnu23,
   it is close enough, and defines __STDC_VERSION == 202000L.  */
#if !defined __STDC_VERSION__ || __STDC_VERSION__ <= 201710L
# error "Compiler advertises conformance to C17 or earlier"
#endif

// Check alignas.
char alignas (double) c23_aligned_as_double;
char alignas (0) c23_no_special_alignment;
extern char c23_aligned_as_int;
char alignas (0) alignas (int) c23_aligned_as_int;

// Check alignof.
enum
{
  c23_int_alignment = alignof (int),
  c23_int_array_alignment = alignof (int[100]),
  c23_char_alignment = alignof (char)
};
static_assert (0 < -alignof (int), "alignof is signed");

int function_with_unnamed_parameter (int) { return 0; }

void c23_noreturn ();

/* Test parsing of string and char UTF-8 literals (including hex escapes).
   The parens pacify GCC 15.  */
bool use_u8 = (!sizeof u8"\xFF") == (!u8'\''x'\'');

bool check_that_bool_works = true | false | !nullptr;
#if !true
# error "true does not work in #if"
#endif
#if false
#elifdef __STDC_VERSION__
#else
# error "#elifdef does not work"
#endif

#ifndef __has_c_attribute
# error "__has_c_attribute not defined"
#endif

#ifndef __has_include
# error "__has_include not defined"
#endif

#define LPAREN() (
#define FORTY_TWO(x) 42
#define VA_OPT_TEST(r, x, ...) __VA_OPT__ (FORTY_TWO r x))
static_assert (VA_OPT_TEST (LPAREN (), 0, <:-) == 42);

static_assert (0b101010 == 42);
static_assert (0B101010 == 42);
static_assert (0xDEAD'\''BEEF == 3'\''735'\''928'\''559);
static_assert (0.500'\''000'\''000 == 0.5);

enum unsignedish : unsigned int { uione = 1 };
static_assert (0 < -uione);

#include <stddef.h>
constexpr nullptr_t null_pointer = nullptr;

static typeof (1 + 1L) two () { return 2; }
static long int three () { return 3; }
'
]])])

AC_DEFUN([_AC_C_C23_TEST_MAIN],
[m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C23 (body of main).
ac_c_conftest_c23_main='
  {
    label_before_declaration:
      int arr[10] = {};
      if (arr[0])
        goto label_before_declaration;
      if (!arr[0])
        goto label_at_end_of_block;
    label_at_end_of_block:
  }
  ok |= !null_pointer;
  ok |= two != three;
'
]])])

AC_DEFUN([_AC_C_C89_TEST_PROGRAM],
[AC_REQUIRE([_AC_C_C89_TEST_GLOBALS])dnl
AC_REQUIRE([_AC_C_C89_TEST_MAIN])dnl
m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C89 (complete).
ac_c_conftest_c89_program="${ac_c_conftest_c89_globals}

int
main (int argc, char **argv)
{
  int ok = 0;
  ${ac_c_conftest_c89_main}
  return ok;
}
"
]])])

AC_DEFUN([_AC_C_C99_TEST_PROGRAM],
[AC_REQUIRE([_AC_C_C89_TEST_GLOBALS])dnl
AC_REQUIRE([_AC_C_C89_TEST_MAIN])dnl
AC_REQUIRE([_AC_C_C99_TEST_GLOBALS])dnl
AC_REQUIRE([_AC_C_C99_TEST_MAIN])dnl
m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C99 (complete).
ac_c_conftest_c99_program="${ac_c_conftest_c89_globals}
${ac_c_conftest_c99_globals}

int
main (int argc, char **argv)
{
  int ok = 0;
  ${ac_c_conftest_c89_main}
  ${ac_c_conftest_c99_main}
  return ok;
}
"
]])])

AC_DEFUN([_AC_C_C11_TEST_PROGRAM],
[AC_REQUIRE([_AC_C_C89_TEST_GLOBALS])dnl
AC_REQUIRE([_AC_C_C89_TEST_MAIN])dnl
AC_REQUIRE([_AC_C_C99_TEST_GLOBALS])dnl
AC_REQUIRE([_AC_C_C99_TEST_MAIN])dnl
AC_REQUIRE([_AC_C_C11_TEST_GLOBALS])dnl
AC_REQUIRE([_AC_C_C11_TEST_MAIN])dnl
m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C11 (complete).
ac_c_conftest_c11_program="${ac_c_conftest_c89_globals}
${ac_c_conftest_c99_globals}
${ac_c_conftest_c11_globals}

int
main (int argc, char **argv)
{
  int ok = 0;
  ${ac_c_conftest_c89_main}
  ${ac_c_conftest_c99_main}
  ${ac_c_conftest_c11_main}
  return ok;
}
"
]])])

AC_DEFUN([_AC_C_C23_TEST_PROGRAM],
[AC_REQUIRE([_AC_C_C23_TEST_GLOBALS])dnl
AC_REQUIRE([_AC_C_C23_TEST_MAIN])dnl
m4_divert_text([INIT_PREPARE],
[[# Test code for whether the C compiler supports C23 (complete).
ac_c_conftest_c23_program="${ac_c_conftest_c23_globals}

int
main (int, char **)
{
  int ok = 0;
  ${ac_c_conftest_c23_main}
  return ok;
}
"
]])])


# _AC_C_C89_OPTIONS
# -----------------
# Whitespace-separated list of options that might put the C compiler
# into a mode conforming to ISO C1990 with extensions.  Do not try
# "strictly conforming" modes (e.g. gcc's -std=c90); they break some
# systems' header files.  If more than one option is needed, put
# shell quotes around the group.
#
# AIX circa 2003         -qlanglvl=extc89
# old AIX                -qlanglvl=ansi
# Ultrix, OSF/1, Tru64   -std
# HP-UX 10.20 and later  -Ae
# HP-UX older versions   -Aa -D_HPUX_SOURCE
# SVR4                   -Xc -D__EXTENSIONS__
m4_define([_AC_C_C89_OPTIONS], [
    -qlanglvl=extc89
    -qlanglvl=ansi
    -std
    -Ae
    "-Aa -D_HPUX_SOURCE"
    "-Xc -D__EXTENSIONS__"
])


# _AC_C_C99_OPTIONS
# -----------------
# Whitespace-separated list of options that might put the C compiler
# into a mode conforming to ISO C1999 with extensions.  Do not try
# "strictly conforming" modes (e.g. gcc's -std=c99); they break some
# systems' header files.  If more than one option is needed, put
# shell quotes around the group.
#
# GCC, Clang    -std=gnu99
# Intel ICC     -std=c99, -c99 (deprecated)
#   Note: because -std=c99 puts GCC in strictly conforming mode,
#   this option must be tested *after* -std=gnu99.
# IRIX          -c99
# Tru64         -c99
# IBM XL C      -qlanglvl=extc1x (V12.1; does not pass C11 test)
# IBM XL C      -qlanglvl=extc99 (pre-V12.1)
# HP cc         -AC99
# Solaris       -D_STDC_C99=
#   Note: acc's -xc99 option uses linker magic to define the external
#   symbol __xpg4 as if by "int __xpg4 = 1;", which enables C99
#   behavior for C library functions.  This is not wanted here,
#   because it means that a single module compiled with -xc99 alters
#   C runtime behavior for the entire program, not for just the
#   module.  Instead, define the (private) symbol _STDC_C99, which
#   suppresses a bogus failure in <stdbool.h>.  The resulting compiler
#   passes the test case here, and that's good enough.
#   For more, please see the thread starting at:
#   https://lists.gnu.org/archive/html/autoconf/2010-12/msg00059.html
m4_define([_AC_C_C99_OPTIONS], [
    -std=gnu99
    -std=c99
    -c99
    -qlanglvl=extc1x
    -qlanglvl=extc99
    -AC99
    -D_STDC_C99=
])


# _AC_C_C11_OPTIONS
# -----------------
# Whitespace-separated list of options that might put the C compiler
# into a mode conforming to ISO C2011 with extensions.  Do not try
# "strictly conforming" modes (e.g. gcc's -std=c11); they break some
# systems' header files.  If more than one option is needed, put
# shell quotes around the group.
#
# GCC, Clang    -std=gnu11
# MSVC          -std:c11
#
# For IBM XL C for AIX V16.1 or later, '-std=gnu11' should work if
# the user configured with CC='xlclang'.  Otherwise, do not try
# -qlanglvl=extc1x as xlc with IBM XL C V16.1 (the latest version as
# of August 2020) does not pass the C11 test.  Instead, try extc1x when
# compiling the C99 test instead, since it enables _Static_assert and
# _Noreturn, which is a win.
m4_define([_AC_C_C11_OPTIONS], [
    -std=gnu11
    -std:c11
])

# _AC_C_C23_OPTIONS
# -----------------
# Whitespace-separated list of options that might put the C compiler
# into a mode conforming to ISO C 2023 with extensions.  Do not try
# "strictly conforming" modes (e.g. gcc's -std=c23); they break some
# systems' header files.  If more than one option is needed, put
# shell quotes around the group.
#
# GCC, Clang    -std=gnu23
m4_define([_AC_C_C23_OPTIONS], [
    -std=gnu23
])


# _AC_PROG_CC_STDC_EDITION_TRY(EDITION)
# -------------------------------------
# Subroutine of _AC_PROG_CC_STDC_EDITION.  Not to be called directly.
#
# Check whether the C compiler accepts features of EDITION of the
# C standard.  EDITION should be a two-digit year (e.g. 89, 99, 11, 23).
# (FIXME: Switch to four-digit years for futureproofing.)
# This is done by compiling the test program defined by
# _AC_C_C{EDITION}_TEST_PROGRAM, first with no additional
# command-line options, and then with each of the options
# in the space-separated list defined by _AC_C_C{EDITION}_OPTIONS.
#
# If we find a way to make the test program compile, set cache variable
# ac_cv_prog_cc_cEDITION to the options required (if any), and add those
# options to $CC.  Set shell variable ac_prog_cc_stdc to 'cEDITION',
# and set shell variable ac_cv_prog_cc_stdc to the options required.
# (Neither of these variables is AC_SUBSTed.  ac_cv_prog_cc_stdc used
# to be a cache variable and is preserved with this name for backward
# compatibility.)  Otherwise, ac_cv_prog_cc_cEDITION is set to 'no'
# and the other variables are not changed.
#
# If ac_prog_cc_stdc is already set to a value other than 'no',
# the shell code produced by this macro does nothing.  This is so
# _AC_PROG_CC_STDC_EDITION can use m4_map to iterate through
# all the editions.
AC_DEFUN([_AC_PROG_CC_STDC_EDITION_TRY],
[AC_LANG_ASSERT([C])]dnl
[AC_REQUIRE([_AC_C_C$1_TEST_PROGRAM])]dnl
[AS_IF([test x$ac_prog_cc_stdc = xno],
[AC_MSG_CHECKING([for $CC option to enable C$1 features])
AC_CACHE_VAL([ac_cv_prog_cc_c$1],
[ac_cv_prog_cc_c$1=no
ac_save_CC=$CC
AC_LANG_CONFTEST([AC_LANG_DEFINES_PROVIDED][$][ac_c_conftest_c$1_program])
for ac_arg in '' m4_normalize(m4_defn([_AC_C_C$1_OPTIONS]))
do
  CC="$ac_save_CC $ac_arg"
  _AC_COMPILE_IFELSE([], [ac_cv_prog_cc_c$1=$ac_arg])
  test "x$ac_cv_prog_cc_c$1" != "xno" && break
done
rm -f conftest.$ac_ext
CC=$ac_save_CC])
AS_IF([test "x$ac_cv_prog_cc_c$1" = xno],
  [AC_MSG_RESULT([unsupported])],
  [AS_IF([test "x$ac_cv_prog_cc_c$1" = x],
    [AC_MSG_RESULT([none needed])],
    [AC_MSG_RESULT([$ac_cv_prog_cc_c$1])
     CC="$CC $ac_cv_prog_cc_c$1"])
  ac_cv_prog_cc_stdc=$ac_cv_prog_cc_c$1
  ac_prog_cc_stdc=c$1])])
])


# _AC_PROG_CC_STDC_EDITION
# ------------------------
# Detect the most recent edition of the ISO C standard that is
# supported by the C compiler.  Add command-line options to $CC, if
# necessary, to enable support for this edition.  Set the shell
# variable ac_prog_cc_stdc to indicate the edition.
AC_DEFUN([_AC_PROG_CC_STDC_EDITION],
[ac_prog_cc_stdc=no
m4_map([_AC_PROG_CC_STDC_EDITION_TRY], [[23], [11], [99], [89]])])

# AC_C_PROTOTYPES
# ---------------
# Check if the C compiler supports prototypes.
# Obsolete - new code should assume C89 compliance.
AC_DEFUN([AC_C_PROTOTYPES],
[AC_REQUIRE([AC_PROG_CC])dnl
if test "$ac_prog_cc_stdc" != no; then
  AC_DEFINE(PROTOTYPES, 1,
	    [Define to 1 if the C compiler supports function prototypes.])
  AC_DEFINE(__PROTOTYPES, 1,
	    [Define like PROTOTYPES; this can be used by system headers.])
fi
])# AC_C_PROTOTYPES

])# m4_version_prereq
