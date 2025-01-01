# endian_h.m4
# serial 6
dnl Copyright 2024-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl A placeholder for <endian.h>, for platforms that have issues.

AC_DEFUN_ONCE([gl_ENDIAN_H],
[
  AC_REQUIRE([gl_BIGENDIAN])

  AC_CHECK_HEADERS_ONCE([endian.h])
  gl_CHECK_NEXT_HEADERS([endian.h])
  if test $ac_cv_header_endian_h = yes; then
    HAVE_ENDIAN_H=1
    dnl Check if endian.h defines uint16_t, uint32_t, and uint64_t.
    AC_CACHE_CHECK([if endian.h defines stdint types],
      [gl_cv_header_endian_h_stdint_types],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <endian.h>
            ]],
            [[uint16_t t1 = 0;
              uint32_t t2 = 0;
              uint64_t t3 = 0;
              return !(t1 + t2 + t3);
            ]])],
      [gl_cv_header_endian_h_stdint_types=yes],
      [gl_cv_header_endian_h_stdint_types=no])
    ])
    AC_CACHE_CHECK([if endian.h defines functions and macros],
      [gl_cv_header_working_endian_h],
      [gl_cv_header_working_endian_h=no
       AC_LINK_IFELSE(
         [AC_LANG_PROGRAM(
[[
#include <endian.h>
]],
[[
#if LITTLE_ENDIAN == BIG_ENDIAN
# error "Endian macros not unique."
#endif
#if BYTE_ORDER != LITTLE_ENDIAN && BYTE_ORDER != BIG_ENDIAN
# error "Byte order not defined."
#endif

/* Big endian to host.  */
int value16_1 = be16toh (0.0);
int value32_1 = be32toh (0.0);
int value64_1 = be64toh (0.0);

/* Host to big endian.  */
int value16_2 = htobe16 (0.0);
int value32_2 = htobe32 (0.0);
int value64_2 = htobe64 (0.0);

/* Little endian to host.  */
int value16_3 = le16toh (0.0);
int value32_3 = le32toh (0.0);
int value64_3 = le64toh (0.0);

/* Host to little endian.  */
int value16_4 = htole16 (0.0);
int value32_4 = htole32 (0.0);
int value64_4 = htole64 (0.0);

/* Make sure the variables get used.  */
return !(value16_1 + value32_1 + value64_1
         + value16_2 + value32_2 + value64_2
         + value16_3 + value32_3 + value64_3
         + value16_4 + value32_4 + value64_4);
]])],
         [gl_cv_header_working_endian_h=yes],
         [gl_cv_header_working_endian_h=no])
      ])
  else
    HAVE_ENDIAN_H=0
  fi

  AC_CHECK_HEADERS_ONCE([sys/endian.h])
  if test $ac_cv_header_sys_endian_h = yes; then
    HAVE_SYS_ENDIAN_H=1
    dnl Check if sys/endian.h defines uint16_t, uint32_t, and uint64_t.
    dnl Note: We don't use the result of this test for now.
    AC_CACHE_CHECK([if sys/endian.h defines stdint types],
      [gl_cv_header_sys_endian_h_stdint_types],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <sys/endian.h>
            ]],
            [[uint16_t t1 = 0;
              uint32_t t2 = 0;
              uint64_t t3 = 0;
              return !(t1 + t2 + t3);
            ]])],
      [gl_cv_header_sys_endian_h_stdint_types=yes],
      [gl_cv_header_sys_endian_h_stdint_types=no])
    ])
    dnl Note: We don't use the result of this test for now.
    AC_CACHE_CHECK([if sys/endian.h defines functions and macros],
      [gl_cv_header_working_sys_endian_h],
      [gl_cv_header_working_sys_endian_h=no
       AC_LINK_IFELSE(
         [AC_LANG_PROGRAM(
[[
#include <sys/endian.h>
]],
[[
#if LITTLE_ENDIAN == BIG_ENDIAN
# error "Endian macros not unique."
#endif
#if BYTE_ORDER != LITTLE_ENDIAN && BYTE_ORDER != BIG_ENDIAN
# error "Byte order not defined."
#endif

/* Big endian to host.  */
int value16_1 = be16toh (0.0);
int value32_1 = be32toh (0.0);
int value64_1 = be64toh (0.0);

/* Host to big endian.  */
int value16_2 = htobe16 (0.0);
int value32_2 = htobe32 (0.0);
int value64_2 = htobe64 (0.0);

/* Little endian to host.  */
int value16_3 = le16toh (0.0);
int value32_3 = le32toh (0.0);
int value64_3 = le64toh (0.0);

/* Host to little endian.  */
int value16_4 = htole16 (0.0);
int value32_4 = htole32 (0.0);
int value64_4 = htole64 (0.0);

/* Make sure the variables get used.  */
return !(value16_1 + value32_1 + value64_1
         + value16_2 + value32_2 + value64_2
         + value16_3 + value32_3 + value64_3
         + value16_4 + value32_4 + value64_4);
]])],
         [gl_cv_header_working_sys_endian_h=yes],
         [gl_cv_header_working_sys_endian_h=no])
      ])
  else
    HAVE_SYS_ENDIAN_H=0
  fi

  dnl Check if endian.h should be generated.
  if test "$gl_cv_header_endian_h_stdint_types" = yes \
     && test "$gl_cv_header_working_endian_h" = yes; then
    GL_GENERATE_ENDIAN_H=false
  else
    GL_GENERATE_ENDIAN_H=true
  fi

  dnl Check if endian.h works but is missing types from stdint.h.
  if test $GL_GENERATE_ENDIAN_H; then
    if test "$gl_cv_header_working_endian_h" = yes; then
      ENDIAN_H_JUST_MISSING_STDINT=1
    else
      ENDIAN_H_JUST_MISSING_STDINT=0
    fi
  else
    ENDIAN_H_JUST_MISSING_STDINT=0
  fi

  AC_SUBST([HAVE_ENDIAN_H])
  AC_SUBST([HAVE_SYS_ENDIAN_H])
  AC_SUBST([ENDIAN_H_JUST_MISSING_STDINT])
])
