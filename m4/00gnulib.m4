# 00gnulib.m4
# serial 11
dnl Copyright (C) 2009-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl This file must be named something that sorts before all other
dnl gnulib-provided .m4 files.  It is needed until the clang fix has
dnl been included in Autoconf.

# The following definitions arrange to use a compiler option
# -Werror=implicit-function-declaration in AC_CHECK_DECL, when the
# compiler is clang.  Without it, clang implicitly declares "known"
# library functions in C mode, but not in C++ mode, which would cause
# Gnulib to omit a declaration and thus later produce an error in C++
# mode.  As of clang 9.0, these "known" functions are identified through
# LIBBUILTIN invocations in the LLVM source file
# llvm/tools/clang/include/clang/Basic/Builtins.def.
# It's not possible to AC_REQUIRE the extra tests from AC_CHECK_DECL,
# because AC_CHECK_DECL, like other Autoconf built-ins, is not supposed
# to AC_REQUIRE anything: some configure.ac files have their first
# AC_CHECK_DECL executed conditionally.  Therefore append the extra tests
# to AC_PROG_CC.
AC_DEFUN([gl_COMPILER_CLANG],
[
dnl AC_REQUIRE([AC_PROG_CC])
  AC_CACHE_CHECK([whether the compiler is clang],
    [gl_cv_compiler_clang],
    [dnl Use _AC_COMPILE_IFELSE instead of AC_EGREP_CPP, to avoid error
     dnl "circular dependency of AC_LANG_COMPILER(C)" if AC_PROG_CC has
     dnl not yet been invoked.
     _AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
           #ifdef __clang__
           barfbarf
           #endif
           ]],[[]])
        ],
        [gl_cv_compiler_clang=no],
        [gl_cv_compiler_clang=yes])
    ])
])
AC_DEFUN([gl_COMPILER_PREPARE_CHECK_DECL],
[
dnl AC_REQUIRE([AC_PROG_CC])
dnl AC_REQUIRE([gl_COMPILER_CLANG])
  AC_CACHE_CHECK([for compiler option needed when checking for declarations],
    [gl_cv_compiler_check_decl_option],
    [if test $gl_cv_compiler_clang = yes; then
       dnl Test whether the compiler supports the option
       dnl '-Werror=implicit-function-declaration'.
       saved_ac_compile="$ac_compile"
       ac_compile="$ac_compile -Werror=implicit-function-declaration"
       dnl Use _AC_COMPILE_IFELSE instead of AC_COMPILE_IFELSE, to avoid a
       dnl warning "AC_COMPILE_IFELSE was called before AC_USE_SYSTEM_EXTENSIONS".
       _AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],[[]])],
         [gl_cv_compiler_check_decl_option='-Werror=implicit-function-declaration'],
         [gl_cv_compiler_check_decl_option=none])
       ac_compile="$saved_ac_compile"
     else
       gl_cv_compiler_check_decl_option=none
     fi
    ])
  if test "x$gl_cv_compiler_check_decl_option" != xnone; then
    ac_compile_for_check_decl="$ac_compile $gl_cv_compiler_check_decl_option"
  else
    ac_compile_for_check_decl="$ac_compile"
  fi
])
dnl Redefine _AC_CHECK_DECL_BODY so that it references ac_compile_for_check_decl
dnl instead of ac_compile.  If, for whatever reason, the override of AC_PROG_CC
dnl in zzgnulib.m4 is inactive, use the original ac_compile.
m4_define([_AC_CHECK_DECL_BODY],
[  ac_saved_ac_compile="$ac_compile"
  if test -n "$ac_compile_for_check_decl"; then
    ac_compile="$ac_compile_for_check_decl"
  fi]
m4_defn([_AC_CHECK_DECL_BODY])[  ac_compile="$ac_saved_ac_compile"
])

# This implementation is taken from GNU Autoconf lib/autoconf/c.m4
# commit cba29368f2d37483990c317617b6c762b0a43c39 dated 2026-10-19 21:14:13.
# This implementation will be obsolete once we can assume Autoconf 2.74
# or later is installed everywhere a Gnulib program might be developed.

m4_version_prereq([2.74], [], [

# AC_LANG_FUNC_LINK_TRY(C)(FUNCTION)
# ----------------------------------
# Don't include <ctype.h> because on OSF/1 3.0 it includes
# <sys/types.h> which includes <sys/select.h> which contains a
# prototype for select.  Similarly for bzero.
#
# This test used to merely assign f=$1 in main(), but that was
# optimized away by HP unbundled cc A.05.36 for ia64 under +O3,
# presumably on the basis that there's no need to do that store if the
# program is about to exit.  Conversely, the AIX linker optimizes an
# unused external declaration that initializes f=$1.  So this test
# program has both an external initialization of f, and a use of f in
# main that affects the exit status.
#
m4_define([AC_LANG_FUNC_LINK_TRY(C)],
[AC_LANG_PROGRAM(
[/* Define $1 to an innocuous variant, in case <limits.h> declares $1.
   For example, HP-UX 11i <limits.h> declares gettimeofday.  */
#define $1 innocuous_$1

/* System header to define __stub macros and hopefully few prototypes,
   which can conflict with char $1 (void); below.  */
#include <limits.h>
#undef $1

/* Tell Clang to allow disagreement with declaration of builtin $1.  */
#if defined __clang__ && 4 <= __clang_major__
# pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#endif

#ifdef _MSC_VER

/* Tell MSVC to link the function without checking its type.  */
# ifdef _M_IX86 /* x86 */
#  pragma comment (linker, "/include:_$1")
# else /* x86_64, arm, arm64 */
#  pragma comment (linker, "/include:$1")
# endif

#else

/* Override any GCC internal prototype to avoid an error.
   Use char because int might match the return type of a GCC
   builtin and then its argument prototype would still apply.  */
# ifdef __cplusplus
extern "C"
# endif
char $1 (void);
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
# if defined __stub_$1 || defined __stub___$1
choke me
# endif

#endif
], [
#ifndef _MSC_VER
return $1 ();
#endif
])])

])# m4_version_prereq

# gl_00GNULIB
# -----------
# Witness macro that this file has been included.  Needed to force
# Automake to include this file prior to all other gnulib .m4 files.
AC_DEFUN([gl_00GNULIB])
