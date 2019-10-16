# manywarnings.m4 serial 18
dnl Copyright (C) 2008-2019 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Simon Josefsson

# gl_MANYWARN_COMPLEMENT(OUTVAR, LISTVAR, REMOVEVAR)
# --------------------------------------------------
# Copy LISTVAR to OUTVAR except for the entries in REMOVEVAR.
# Elements separated by whitespace.  In set logic terms, the function
# does OUTVAR = LISTVAR \ REMOVEVAR.
AC_DEFUN([gl_MANYWARN_COMPLEMENT],
[
  gl_warn_set=
  set x $2; shift
  for gl_warn_item
  do
    case " $3 " in
      *" $gl_warn_item "*)
        ;;
      *)
        gl_warn_set="$gl_warn_set $gl_warn_item"
        ;;
    esac
  done
  $1=$gl_warn_set
])

# gl_MANYWARN_ALL_GCC(VARIABLE)
# -----------------------------
# Add all documented GCC warning parameters to variable VARIABLE.
# Note that you need to test them using gl_WARN_ADD if you want to
# make sure your gcc understands it.
#
# The effects of this macro depend on the current language (_AC_LANG).
AC_DEFUN([gl_MANYWARN_ALL_GCC],
[_AC_LANG_DISPATCH([$0], _AC_LANG, $@)])

# Specialization for _AC_LANG = C.
# Use of m4_defun rather than AC_DEFUN works around a bug in autoconf < 2.63b.
m4_defun([gl_MANYWARN_ALL_GCC(C)],
[
  AC_LANG_PUSH([C])

  dnl First, check for some issues that only occur when combining multiple
  dnl gcc warning categories.
  AC_REQUIRE([AC_PROG_CC])
  if test -n "$GCC"; then

    dnl Check if -W -Werror -Wno-missing-field-initializers is supported
    dnl with the current $CC $CFLAGS $CPPFLAGS.
    AC_CACHE_CHECK([whether -Wno-missing-field-initializers is supported],
      [gl_cv_cc_nomfi_supported],
      [gl_save_CFLAGS="$CFLAGS"
       CFLAGS="$CFLAGS -W -Werror -Wno-missing-field-initializers"
       AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([[]], [[]])],
         [gl_cv_cc_nomfi_supported=yes],
         [gl_cv_cc_nomfi_supported=no])
       CFLAGS="$gl_save_CFLAGS"
      ])

    if test "$gl_cv_cc_nomfi_supported" = yes; then
      dnl Now check whether -Wno-missing-field-initializers is needed
      dnl for the { 0, } construct.
      AC_CACHE_CHECK([whether -Wno-missing-field-initializers is needed],
        [gl_cv_cc_nomfi_needed],
        [gl_save_CFLAGS="$CFLAGS"
         CFLAGS="$CFLAGS -W -Werror"
         AC_COMPILE_IFELSE(
           [AC_LANG_PROGRAM(
              [[int f (void)
                {
                  typedef struct { int a; int b; } s_t;
                  s_t s1 = { 0, };
                  return s1.b;
                }
              ]],
              [[]])],
           [gl_cv_cc_nomfi_needed=no],
           [gl_cv_cc_nomfi_needed=yes])
         CFLAGS="$gl_save_CFLAGS"
        ])
    fi

    dnl Next, check if -Werror -Wuninitialized is useful with the
    dnl user's choice of $CFLAGS; some versions of gcc warn that it
    dnl has no effect if -O is not also used
    AC_CACHE_CHECK([whether -Wuninitialized is supported],
      [gl_cv_cc_uninitialized_supported],
      [gl_save_CFLAGS="$CFLAGS"
       CFLAGS="$CFLAGS -Werror -Wuninitialized"
       AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([[]], [[]])],
         [gl_cv_cc_uninitialized_supported=yes],
         [gl_cv_cc_uninitialized_supported=no])
       CFLAGS="$gl_save_CFLAGS"
      ])

  fi

  # List all gcc warning categories.
  # To compare this list to your installed GCC's, run this Bash command:
  #
  # comm -3 \
  #  <((sed -n 's/^  *\(-[^ 0-9][^ ]*\) .*/\1/p' manywarnings.m4; \
  #     awk '/^[^#]/ {print $1}' ../build-aux/gcc-warning.spec) | sort) \
  #  <(LC_ALL=C gcc --help=warnings | sed -n 's/^  \(-[^ ]*\) .*/\1/p' | sort)

  gl_manywarn_set=
  for gl_manywarn_item in -fno-common \
    -W \
    -Wabsolute-value \
    -Waddress \
    -Waddress-of-packed-member \
    -Waggressive-loop-optimizations \
    -Wall \
    -Wattribute-warning \
    -Wattributes \
    -Wbad-function-cast \
    -Wbool-compare \
    -Wbool-operation \
    -Wbuiltin-declaration-mismatch \
    -Wbuiltin-macro-redefined \
    -Wcannot-profile \
    -Wcast-align \
    -Wcast-align=strict \
    -Wcast-function-type \
    -Wchar-subscripts \
    -Wclobbered \
    -Wcomment \
    -Wcomments \
    -Wcoverage-mismatch \
    -Wcpp \
    -Wdangling-else \
    -Wdate-time \
    -Wdeprecated \
    -Wdeprecated-declarations \
    -Wdesignated-init \
    -Wdisabled-optimization \
    -Wdiscarded-array-qualifiers \
    -Wdiscarded-qualifiers \
    -Wdiv-by-zero \
    -Wdouble-promotion \
    -Wduplicated-branches \
    -Wduplicated-cond \
    -Wduplicate-decl-specifier \
    -Wempty-body \
    -Wendif-labels \
    -Wenum-compare \
    -Wexpansion-to-defined \
    -Wextra \
    -Wformat-contains-nul \
    -Wformat-extra-args \
    -Wformat-nonliteral \
    -Wformat-security \
    -Wformat-signedness \
    -Wformat-y2k \
    -Wformat-zero-length \
    -Wframe-address \
    -Wfree-nonheap-object \
    -Whsa \
    -Wif-not-aligned \
    -Wignored-attributes \
    -Wignored-qualifiers \
    -Wimplicit \
    -Wimplicit-function-declaration \
    -Wimplicit-int \
    -Wincompatible-pointer-types \
    -Winit-self \
    -Winline \
    -Wint-conversion \
    -Wint-in-bool-context \
    -Wint-to-pointer-cast \
    -Winvalid-memory-model \
    -Winvalid-pch \
    -Wlogical-not-parentheses \
    -Wlogical-op \
    -Wmain \
    -Wmaybe-uninitialized \
    -Wmemset-elt-size \
    -Wmemset-transposed-args \
    -Wmisleading-indentation \
    -Wmissing-attributes \
    -Wmissing-braces \
    -Wmissing-declarations \
    -Wmissing-field-initializers \
    -Wmissing-include-dirs \
    -Wmissing-parameter-type \
    -Wmissing-profile \
    -Wmissing-prototypes \
    -Wmultichar \
    -Wmultistatement-macros \
    -Wnarrowing \
    -Wnested-externs \
    -Wnonnull \
    -Wnonnull-compare \
    -Wnull-dereference \
    -Wodr \
    -Wold-style-declaration \
    -Wold-style-definition \
    -Wopenmp-simd \
    -Woverflow \
    -Woverlength-strings \
    -Woverride-init \
    -Wpacked \
    -Wpacked-bitfield-compat \
    -Wpacked-not-aligned \
    -Wparentheses \
    -Wpointer-arith \
    -Wpointer-compare \
    -Wpointer-sign \
    -Wpointer-to-int-cast \
    -Wpragmas \
    -Wpsabi \
    -Wrestrict \
    -Wreturn-local-addr \
    -Wreturn-type \
    -Wscalar-storage-order \
    -Wsequence-point \
    -Wshadow \
    -Wshift-count-negative \
    -Wshift-count-overflow \
    -Wshift-negative-value \
    -Wsizeof-array-argument \
    -Wsizeof-pointer-div \
    -Wsizeof-pointer-memaccess \
    -Wstack-protector \
    -Wstrict-aliasing \
    -Wstrict-overflow \
    -Wstrict-prototypes \
    -Wstringop-truncation \
    -Wsuggest-attribute=cold \
    -Wsuggest-attribute=const \
    -Wsuggest-attribute=format \
    -Wsuggest-attribute=malloc \
    -Wsuggest-attribute=noreturn \
    -Wsuggest-attribute=pure \
    -Wsuggest-final-methods \
    -Wsuggest-final-types \
    -Wswitch \
    -Wswitch-bool \
    -Wswitch-unreachable \
    -Wsync-nand \
    -Wsystem-headers \
    -Wtautological-compare \
    -Wtrampolines \
    -Wtrigraphs \
    -Wtype-limits \
    -Wuninitialized \
    -Wunknown-pragmas \
    -Wunsafe-loop-optimizations \
    -Wunused \
    -Wunused-but-set-parameter \
    -Wunused-but-set-variable \
    -Wunused-function \
    -Wunused-label \
    -Wunused-local-typedefs \
    -Wunused-macros \
    -Wunused-parameter \
    -Wunused-result \
    -Wunused-value \
    -Wunused-variable \
    -Wvarargs \
    -Wvariadic-macros \
    -Wvector-operation-performance \
    -Wvla \
    -Wvolatile-register-var \
    -Wwrite-strings \
    \
    ; do
    gl_manywarn_set="$gl_manywarn_set $gl_manywarn_item"
  done

  # gcc --help=warnings outputs an unusual form for these options; list
  # them here so that the above 'comm' command doesn't report a false match.
  # Would prefer "min (PTRDIFF_MAX, SIZE_MAX)", but it must be a literal.
  # Also, AC_COMPUTE_INT requires it to fit in a long; it is 2**63 on
  # the only platforms where it does not fit in a long, so make that
  # a special case.
  AC_MSG_CHECKING([max safe object size])
  AC_COMPUTE_INT([gl_alloc_max],
    [LONG_MAX < (PTRDIFF_MAX < (size_t) -1 ? PTRDIFF_MAX : (size_t) -1)
     ? -1
     : PTRDIFF_MAX < (size_t) -1 ? (long) PTRDIFF_MAX : (long) (size_t) -1],
    [[#include <limits.h>
      #include <stddef.h>
      #include <stdint.h>
    ]],
    [gl_alloc_max=2147483647])
  case $gl_alloc_max in
    -1) gl_alloc_max=9223372036854775807;;
  esac
  AC_MSG_RESULT([$gl_alloc_max])
  gl_manywarn_set="$gl_manywarn_set -Walloc-size-larger-than=$gl_alloc_max"
  gl_manywarn_set="$gl_manywarn_set -Warray-bounds=2"
  gl_manywarn_set="$gl_manywarn_set -Wattribute-alias=2"
  gl_manywarn_set="$gl_manywarn_set -Wformat-overflow=2"
  gl_manywarn_set="$gl_manywarn_set -Wformat-truncation=2"
  gl_manywarn_set="$gl_manywarn_set -Wimplicit-fallthrough=5"
  gl_manywarn_set="$gl_manywarn_set -Wnormalized=nfc"
  gl_manywarn_set="$gl_manywarn_set -Wshift-overflow=2"
  gl_manywarn_set="$gl_manywarn_set -Wstringop-overflow=2"
  gl_manywarn_set="$gl_manywarn_set -Wunused-const-variable=2"
  gl_manywarn_set="$gl_manywarn_set -Wvla-larger-than=4031"

  # These are needed for older GCC versions.
  if test -n "$GCC"; then
    case `($CC --version) 2>/dev/null` in
      'gcc (GCC) '[[0-3]].* | \
      'gcc (GCC) '4.[[0-7]].*)
        gl_manywarn_set="$gl_manywarn_set -fdiagnostics-show-option"
        gl_manywarn_set="$gl_manywarn_set -funit-at-a-time"
          ;;
    esac
  fi

  # Disable specific options as needed.
  if test "$gl_cv_cc_nomfi_needed" = yes; then
    gl_manywarn_set="$gl_manywarn_set -Wno-missing-field-initializers"
  fi

  if test "$gl_cv_cc_uninitialized_supported" = no; then
    gl_manywarn_set="$gl_manywarn_set -Wno-uninitialized"
  fi

  $1=$gl_manywarn_set

  AC_LANG_POP([C])
])

# Specialization for _AC_LANG = C++.
# Use of m4_defun rather than AC_DEFUN works around a bug in autoconf < 2.63b.
m4_defun([gl_MANYWARN_ALL_GCC(C++)],
[
  gl_MANYWARN_ALL_GCC_CXX_IMPL([$1])
])
