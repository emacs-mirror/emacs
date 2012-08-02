dnl 'extern inline' a la ISO C99.

dnl Copyright 2012 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_EXTERN_INLINE],
[
  AC_REQUIRE([AC_C_INLINE])
  AH_VERBATIM([extern_inline],
[/* _GL_INLINE is a portable alternative to ISO C99 plain 'inline'.
   _GL_EXTERN_INLINE is a portable alternative to 'extern inline'.
   _GL_INLINE_HEADER_BEGIN contains useful stuff to put
     in an include file, before uses of _GL_INLINE.
     It suppresses GCC's bogus "no previous prototype for 'FOO'" diagnostic,
     when FOO is an inline function in the header; see
     <http://gcc.gnu.org/bugzilla/show_bug.cgi?id=54113>.
   _GL_INLINE_HEADER_END contains useful stuff to put
     in the same include file, after uses of _GL_INLINE.  */
#if __GNUC__ ? __GNUC_STDC_INLINE__ : 199901L <= __STDC_VERSION__
# define _GL_INLINE inline
# define _GL_EXTERN_INLINE extern inline
# if (__GNUC__ == 4 && 6 <= __GNUC_MINOR__) || 4 < __GNUC__
#  define _GL_INLINE_HEADER_BEGIN \
     _Pragma ("GCC diagnostic push") \
     _Pragma ("GCC diagnostic ignored \"-Wmissing-prototypes\"")
#  define _GL_INLINE_HEADER_END \
     _Pragma ("GCC diagnostic pop")
# endif
#else
# define _GL_INLINE static inline
# define _GL_EXTERN_INLINE static inline
#endif

#ifndef _GL_INLINE_HEADER_BEGIN
# define _GL_INLINE_HEADER_BEGIN
# define _GL_INLINE_HEADER_END
#endif])
])
