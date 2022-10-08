# Check for alignas and alignof that conform to C23.

dnl Copyright 2011-2022 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Prepare for substituting <stdalign.h> if it is not supported.

AC_DEFUN([gl_STDALIGN_H],
[
  AC_CACHE_CHECK([for alignas and alignof],
    [gl_cv_header_working_stdalign_h],
    [gl_save_CFLAGS=$CFLAGS
     for gl_working in "yes, keywords" "yes, <stdalign.h> macros"; do
      AS_CASE([$gl_working],
        [*stdalign.h*], [CFLAGS="$gl_save_CFLAGS -DINCLUDE_STDALIGN_H"])
      AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <stdint.h>
            #ifdef INCLUDE_STDALIGN_H
             #include <stdalign.h>
            #endif
            #include <stddef.h>

            /* Test that alignof yields a result consistent with offsetof.
               This catches GCC bug 52023
               <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=52023>.  */
            #ifdef __cplusplus
               template <class t> struct alignof_helper { char a; t b; };
            # define ao(type) offsetof (alignof_helper<type>, b)
            #else
            # define ao(type) offsetof (struct { char a; type b; }, b)
            #endif
            char test_double[ao (double) % _Alignof (double) == 0 ? 1 : -1];
            char test_long[ao (long int) % _Alignof (long int) == 0 ? 1 : -1];
            char test_alignof[alignof (double) == _Alignof (double) ? 1 : -1];

            /* Test alignas only on platforms where gnulib can help.  */
            #if \
                ((defined __cplusplus && 201103 <= __cplusplus) \
                 || (__TINYC__ && defined __attribute__) \
                 || (defined __APPLE__ && defined __MACH__ \
                     ? 4 < __GNUC__ + (1 <= __GNUC_MINOR__) \
                     : __GNUC__) \
                 || (__ia64 && (61200 <= __HP_cc || 61200 <= __HP_aCC)) \
                 || __ICC || 0x590 <= __SUNPRO_C || 0x0600 <= __xlC__ \
                 || 1300 <= _MSC_VER)
              struct alignas_test { char c; char alignas (8) alignas_8; };
              char test_alignas[offsetof (struct alignas_test, alignas_8) == 8
                                ? 1 : -1];
            #endif
          ]])],
       [gl_cv_header_working_stdalign_h=$gl_working],
       [gl_cv_header_working_stdalign_h=no])

      CFLAGS=$gl_save_CFLAGS
      test "$gl_cv_header_working_stdalign_h" != no && break
     done])

  GL_GENERATE_STDALIGN_H=false
  AS_CASE([$gl_cv_header_working_stdalign_h],
    [no],
      [GL_GENERATE_STDALIGN_H=true],
    [yes*keyword*],
      [AC_DEFINE([HAVE_C_ALIGNASOF], [1],
         [Define to 1 if the alignas and alignof keywords work.])])

  AC_CHECK_HEADERS_ONCE([stdalign.h])

  dnl The "zz" puts this toward config.h's end, to avoid potential
  dnl collisions with other definitions.
  AH_VERBATIM([zzalignas],
[#if !defined HAVE_C_ALIGNASOF && __cplusplus < 201103 && !defined alignof
# if HAVE_STDALIGN_H
#  include <stdalign.h>
# else
   /* Substitute.  Keep consistent with gnulib/lib/stdalign.in.h.  */
#  ifndef _GL_STDALIGN_H
#   define _GL_STDALIGN_H
#   undef _Alignas
#   undef _Alignof
#   if (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112 \
        || (defined __GNUC__ && __GNUC__ < 4 + (__GNUC_MINOR__ < 9) \
            && !defined __clang__) \
        || (defined __clang__ && __clang_major__ < 8))
#    ifdef __cplusplus
#     if (201103 <= __cplusplus || defined _MSC_VER)
#      define _Alignof(type) alignof (type)
#     else
       template <class __t> struct __alignof_helper { char __a; __t __b; };
#      define _Alignof(type) offsetof (__alignof_helper<type>, __b)
#      define _GL_STDALIGN_NEEDS_STDDEF 1
#     endif
#    else
#     define _Alignof(type) offsetof (struct { char __a; type __b; }, __b)
#     define _GL_STDALIGN_NEEDS_STDDEF 1
#    endif
#   endif
#   if ! (defined __cplusplus && (201103 <= __cplusplus || defined _MSC_VER))
#    define alignof _Alignof
#   endif
#   define __alignof_is_defined 1
#   if !defined __STDC_VERSION__ || __STDC_VERSION__ < 201112
#    if defined __cplusplus && (201103 <= __cplusplus || defined _MSC_VER)
#     define _Alignas(a) alignas (a)
#    elif (!defined __attribute__ \
           && ((defined __APPLE__ && defined __MACH__ \
                ? 4 < __GNUC__ + (1 <= __GNUC_MINOR__) \
                : __GNUC__ && !defined __ibmxl__) \
               || (4 <= __clang_major__) \
               || (__ia64 && (61200 <= __HP_cc || 61200 <= __HP_aCC)) \
               || __ICC || 0x590 <= __SUNPRO_C || 0x0600 <= __xlC__))
#     define _Alignas(a) __attribute__ ((__aligned__ (a)))
#    elif 1300 <= _MSC_VER
#     define _Alignas(a) __declspec (align (a))
#    endif
#   endif
#   if ((defined _Alignas \
         && !(defined __cplusplus && (201103 <= __cplusplus || defined _MSC_VER))) \
        || (defined __STDC_VERSION__ && 201112 <= __STDC_VERSION__))
#    define alignas _Alignas
#   endif
#   if (defined alignas \
        || (defined __cplusplus && (201103 <= __cplusplus || defined _MSC_VER)))
#    define __alignas_is_defined 1
#   endif
#   if _GL_STDALIGN_NEEDS_STDDEF
#    include <stddef.h>
#   endif
#  endif /* _GL_STDALIGN_H */
# endif
#endif])
])
