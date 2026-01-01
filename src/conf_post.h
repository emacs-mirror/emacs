/* conf_post.h --- configure.ac includes this via AH_BOTTOM

Copyright (C) 1988, 1993-1994, 1999-2002, 2004-2026 Free Software
Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/* Put the code here rather than in configure.ac using AH_BOTTOM.
   This way, the code does not get processed by autoheader.  For
   example, undefs here are not commented out.  */

/* Disable 'assert' unless enabling checking.  Do this early, in
   case some misguided implementation depends on NDEBUG in some
   include file other than assert.h.  */
#if !defined ENABLE_CHECKING && !defined NDEBUG
# define NDEBUG
#endif

/* To help make dependencies clearer elsewhere, this file typically
   does not #include other files.  The exception is ms-w32.h (DOS_NT
   only) because it historically was included here and changing that
   would take some work.  */

#if defined WINDOWSNT && !defined DEFER_MS_W32_H
# include <ms-w32.h>
#endif

/* GNUC_PREREQ (V, W, X) is true if this is GNU C version V.W.X or later.
   It can be used in a preprocessor expression.  */
#ifndef __GNUC_MINOR__
# define GNUC_PREREQ(v, w, x) false
#elif ! defined __GNUC_PATCHLEVEL__
# define GNUC_PREREQ(v, w, x) \
    ((v) < __GNUC__ + ((w) < __GNUC_MINOR__ + ((x) == 0))
#else
# define GNUC_PREREQ(v, w, x) \
    ((v) < __GNUC__ + ((w) < __GNUC_MINOR__ + ((x) <= __GNUC_PATCHLEVEL__)))
#endif

/* The type of bool bitfields.  Needed to compile Objective-C with
   standard GCC, and to make sure adjacent bool_bf fields are packed
   into the same 1-, 2-, or 4-byte allocation unit in the MinGW
   builds.  It was also needed to port to pre-C99 compilers, although
   we don't care about that any more.  */
#if NS_IMPL_GNUSTEP || defined __MINGW32__
typedef unsigned int bool_bf;
#else
typedef bool bool_bf;
#endif

/* A substitute for __has_attribute on compilers that lack it.
   It is used only on arguments like cleanup that are handled here.
   This macro should be used only in #if expressions, as Oracle
   Studio 12.5's __has_attribute does not work in plain code.  */
#if (defined __has_attribute \
     && (!defined __clang_minor__ \
         || 3 < __clang_major__ + (5 <= __clang_minor__)))
# define HAS_ATTRIBUTE(a) __has_attribute (__##a##__)
#else
# define HAS_ATTRIBUTE(a) HAS_ATTR_##a
# define HAS_ATTR_cleanup GNUC_PREREQ (3, 4, 0)
# define HAS_ATTR_no_address_safety_analysis false
# define HAS_ATTR_no_sanitize false
# define HAS_ATTR_no_sanitize_address GNUC_PREREQ (4, 8, 0)
# define HAS_ATTR_no_sanitize_undefined GNUC_PREREQ (4, 9, 0)
#endif

/* A substitute for __has_feature on compilers that lack it.  It is used only
   to define ADDRESS_SANITIZER below.  */
#ifdef __has_feature
# define HAS_FEATURE(a) __has_feature (a)
#else
# define HAS_FEATURE(a) false
#endif

/* True if addresses are being sanitized.  */
#if defined __SANITIZE_ADDRESS__ || HAS_FEATURE (address_sanitizer)
# define ADDRESS_SANITIZER true
#else
# define ADDRESS_SANITIZER false
#endif

/* We have to go this route, rather than the old hpux9 approach of
   renaming the functions via macros.  The system's stdlib.h has fully
   prototyped declarations, which yields a conflicting definition of
   srand48; it tries to redeclare what was once srandom to be srand48.
   So we go with HAVE_LRAND48 being defined.  */
#ifdef HPUX
#undef srandom
#undef random
#undef HAVE_RANDOM
#undef HAVE_RINT
#endif  /* HPUX */

#ifdef MSDOS
#ifndef __DJGPP__
You lose; /* Emacs for DOS must be compiled with DJGPP */
#endif
#define _NAIVE_DOS_REGS

/* Start of gnulib-related stuff  */

/* lib/ftoastr.c wants strtold, but DJGPP only has _strtold.  DJGPP >
   2.03 has it, but it also has _strtold as a stub that jumps to
   strtold, so use _strtold in all versions.  */
#define strtold _strtold

#if __DJGPP__ > 2 || __DJGPP_MINOR__ > 3
# define HAVE_LSTAT 1
#else
# define lstat stat
/* DJGPP 2.03 and older don't have the next two.  */
# define EOVERFLOW ERANGE
# define SIZE_MAX  4294967295U
#endif

/* Things that lib/reg* wants.  */

#define mbrtowc(pwc, s, n, ps) mbtowc (pwc, s, n)
#define wcrtomb(s, wc, ps) wctomb (s, wc)
#define btowc(b) ((wchar_t) (b))
#define towupper(chr) toupper (chr)
#define towlower(chr) tolower (chr)
#define iswalnum(chr) isalnum (chr)
#define wctype(name) ((wctype_t) 0)
#define iswctype(wc, type) false
#define mbsinit(ps) 1

/* Some things that lib/at-func.c wants.  */
#define GNULIB_SUPPORT_ONLY_AT_FDCWD

/* Needed by lib/lchmod.c.  */
#define EOPNOTSUPP EINVAL

/* We must intercept 'opendir' calls to stash away the directory name,
   so we could reuse it in readlinkat; see msdos.c.  */
#define opendir sys_opendir

/* End of gnulib-related stuff.  */

#define emacs_raise(sig) msdos_fatal_signal (sig)

/* DATA_START is needed by vm-limit.c. */
#define DATA_START (&etext + 1)
#endif  /* MSDOS */

#if defined HAVE_NTGUI && !defined DebPrint
# ifdef EMACSDEBUG
extern void _DebPrint (const char *fmt, ...);
#  define DebPrint(stuff) _DebPrint stuff
# else
#  define DebPrint(stuff) ((void) 0)
# endif
#endif

#if defined CYGWIN && defined HAVE_NTGUI
# define NTGUI_UNICODE /* Cygwin runs only on UNICODE-supporting systems */
# define _WIN32_WINNT 0x500 /* Win2k */
/* The following was in /usr/include/string.h prior to Cygwin 1.7.33.  */
#ifndef strnicmp
#define strnicmp strncasecmp
#endif
#endif

#ifdef emacs /* Don't do this for lib-src.  */
/* Tell regex.c to use a type compatible with Emacs.  */
#define RE_TRANSLATE_TYPE Lisp_Object
#define RE_TRANSLATE(TBL, C) char_table_translate (TBL, C)
#define RE_TRANSLATE_P(TBL) (!BASE_EQ (TBL, make_fixnum (0)))
#endif

/* Tell time_rz.c to use Emacs's getter and setter for TZ.
   Only Emacs uses time_rz so this is OK.  */
#define getenv_TZ emacs_getenv_TZ
#define setenv_TZ emacs_setenv_TZ
extern char *emacs_getenv_TZ (void);
extern int emacs_setenv_TZ (char const *);

#define NO_INLINE _GL_ATTRIBUTE_NOINLINE
#define EXTERNALLY_VISIBLE _GL_ATTRIBUTE_EXTERNALLY_VISIBLE

#if GNUC_PREREQ (4, 4, 0) && defined __GLIBC_MINOR__
# define PRINTF_ARCHETYPE __gnu_printf__
#elif GNUC_PREREQ (4, 4, 0) && defined __MINGW32__
# ifdef MINGW_W64
/* When __USE_MINGW_ANSI_STDIO is non-zero (as set by config.h),
   MinGW64 replaces printf* with its own versions that are
   __gnu_printf__ compatible, and emits warnings for MS native %I64d
   format spec.  */
#  if __USE_MINGW_ANSI_STDIO
#   define PRINTF_ARCHETYPE __gnu_printf__
#  else
#   define PRINTF_ARCHETYPE __ms_printf__
#  endif
# else	/* mingw.org's MinGW */
/* Starting from runtime v5.0.0, mingw.org's MinGW with GCC 6 and
   later turns on __USE_MINGW_ANSI_STDIO by default, replaces printf*
   with its own __mingw_printf__ version, which still recognizes
   %I64d.  */
#  if GNUC_PREREQ (6, 0, 0) && __MINGW32_MAJOR_VERSION >= 5
#   define PRINTF_ARCHETYPE __mingw_printf__
#  else  /* __MINGW32_MAJOR_VERSION < 5 */
#   define PRINTF_ARCHETYPE __ms_printf__
#  endif  /* __MINGW32_MAJOR_VERSION < 5 */
# endif	 /* MinGW */
#else
# define PRINTF_ARCHETYPE __printf__
#endif
#define ATTRIBUTE_FORMAT_PRINTF(string_index, first_to_check) \
  _GL_ATTRIBUTE_FORMAT ((PRINTF_ARCHETYPE, string_index, first_to_check))

#define ARG_NONNULL _GL_ATTRIBUTE_NONNULL

/* Declare NAME to be a pointer to an object of type TYPE, initialized
   to the address ADDR, which may be of a different type.  Accesses
   via NAME may alias with other accesses with the traditional
   behavior, even if options like gcc -fstrict-aliasing are used.  */

#define DECLARE_POINTER_ALIAS(name, type, addr) \
  type _GL_ATTRIBUTE_MAY_ALIAS *name = (type *) (addr)

#if 3 <= __GNUC__
# define ATTRIBUTE_SECTION(name) __attribute__ ((section (name)))
#else
# define ATTRIBUTE_SECTION(name)
#endif

#define ATTRIBUTE_MALLOC_SIZE(args) \
  _GL_ATTRIBUTE_MALLOC _GL_ATTRIBUTE_ALLOC_SIZE (args)

/* Work around GCC bug 59600: when a function is inlined, the inlined
   code may have its addresses sanitized even if the function has the
   no_sanitize_address attribute.  This bug is fixed in GCC 4.9.0 and
   clang 3.4.  */
#if (! ADDRESS_SANITIZER \
     || (GNUC_PREREQ (4, 9, 0) \
	 || 3 < __clang_major__ + (4 <= __clang_minor__)))
# define ADDRESS_SANITIZER_WORKAROUND /* No workaround needed.  */
#else
# define ADDRESS_SANITIZER_WORKAROUND NO_INLINE
#endif

/* Attribute of functions whose code should not have addresses
   sanitized.  */

#if HAS_ATTRIBUTE (no_sanitize_address)
# define ATTRIBUTE_NO_SANITIZE_ADDRESS \
    __attribute__ ((no_sanitize_address)) ADDRESS_SANITIZER_WORKAROUND
#elif HAS_ATTRIBUTE (no_address_safety_analysis)
# define ATTRIBUTE_NO_SANITIZE_ADDRESS \
    __attribute__ ((no_address_safety_analysis)) ADDRESS_SANITIZER_WORKAROUND
#else
# define ATTRIBUTE_NO_SANITIZE_ADDRESS
#endif

/* Attribute of functions whose undefined behavior should not be sanitized.  */

#if HAS_ATTRIBUTE (no_sanitize_undefined)
# define ATTRIBUTE_NO_SANITIZE_UNDEFINED __attribute__ ((no_sanitize_undefined))
#elif HAS_ATTRIBUTE (no_sanitize)
# define ATTRIBUTE_NO_SANITIZE_UNDEFINED \
    __attribute__ ((no_sanitize ("undefined")))
#else
# define ATTRIBUTE_NO_SANITIZE_UNDEFINED
#endif

/* gcc -fsanitize=address does not work with vfork in Fedora 28 x86-64.  See:
   https://lists.gnu.org/r/emacs-devel/2017-05/msg00464.html
   For now, assume that this problem occurs on all platforms.  */
#if ADDRESS_SANITIZER && !defined vfork
# define vfork fork
#endif

/* vfork is deprecated on at least macOS 11.6 and later, but it still works
   and is faster than fork, so silence the warning as if we knew what we
   are doing.  */
#ifdef DARWIN_OS
#define VFORK()								\
  (_Pragma("clang diagnostic push")					\
   _Pragma("clang diagnostic ignored \"-Wdeprecated-declarations\"")	\
   vfork ()								\
   _Pragma("clang diagnostic pop"))
#else
#define VFORK() vfork ()
#endif

#if ! (defined __FreeBSD__ || defined GNU_LINUX || defined __MINGW32__)
# undef PROFILING
#endif

/* Some versions of GNU/Linux define noinline in their headers.  */
#ifdef noinline
#undef noinline
#endif

/* INLINE marks functions defined in Emacs-internal C headers.
   INLINE is implemented via C99-style 'extern inline' if Emacs is built
   with -DEMACS_EXTERN_INLINE; otherwise it is implemented via 'static'.
   EMACS_EXTERN_INLINE is no longer the default, as 'static' seems to
   have better performance with GCC.

   An include file foo.h should prepend INLINE to function
   definitions, with the following overall pattern:

      [#include any other .h files first.]
      ...
      INLINE_HEADER_BEGIN
      ...
      INLINE int
      incr (int i)
      {
        return i + 1;
      }
      ...
      INLINE_HEADER_END

   For every executable, exactly one file that includes the header
   should do this:

      #define INLINE EXTERN_INLINE

   before including config.h or any other .h file.
   Other .c files should not define INLINE.
   For Emacs, this is done by having emacs.c first '#define INLINE
   EXTERN_INLINE' and then include every .h file that uses INLINE.

   The INLINE_HEADER_BEGIN and INLINE_HEADER_END macros suppress bogus
   warnings in some GCC versions; see ../m4/extern-inline.m4.  */

#ifdef EMACS_EXTERN_INLINE

/* Use Gnulib's extern-inline module for extern inline functions.

   C99 compilers compile functions like 'incr' as C99-style extern
   inline functions.  Buggy GCC implementations do something similar with
   GNU-specific keywords.  Buggy non-GCC compilers use static
   functions, which bloats the code but is good enough.  */

# ifndef INLINE
#  define INLINE _GL_INLINE
# endif
# define EXTERN_INLINE _GL_EXTERN_INLINE
# define INLINE_HEADER_BEGIN _GL_INLINE_HEADER_BEGIN
# define INLINE_HEADER_END _GL_INLINE_HEADER_END

#else

/* Use 'static inline' instead of 'extern inline' because 'static inline'
   has much better performance for Emacs when compiled with 'gcc -Og'.  */

# ifndef INLINE
#  define INLINE EXTERN_INLINE
# endif
# define EXTERN_INLINE static inline
# define INLINE_HEADER_BEGIN
# define INLINE_HEADER_END

#endif

/* 'int x UNINIT;' is equivalent to 'int x;', except it cajoles GCC
   into not warning incorrectly about use of an uninitialized variable.  */
#if defined GCC_LINT || defined lint
# define UNINIT = {0,}
#else
# define UNINIT /* empty */
#endif

/* Emacs needs neither glibc strftime behavior for AM and PM indicators,
   nor Gnulib strftime support for non-Gregorian calendars.  */
#define REQUIRE_GNUISH_STRFTIME_AM_PM false
#define SUPPORT_NON_GREG_CALENDARS_IN_STRFTIME false

#ifdef MSDOS
/* These are required by file-has-acl.c but defined in dirent.h and
   errno.h, which are not generated on DOS.  */
#define _GL_DT_NOTDIR 0x100   /* Not a directory */
#define ENOTSUP ENOSYS
# define IFTODT(mode) \
   (S_ISREG (mode) ? DT_REG : S_ISDIR (mode) ? DT_DIR \
    : S_ISLNK (mode) ? DT_LNK : S_ISBLK (mode) ? DT_BLK \
    : S_ISCHR (mode) ? DT_CHR : S_ISFIFO (mode) ? DT_FIFO \
    : S_ISSOCK (mode) ? DT_SOCK : DT_UNKNOWN)
#endif /* MSDOS */

#if defined WINDOWSNT && !(defined OMIT_CONSOLESAFE && OMIT_CONSOLESAFE == 1)
# if !defined _UCRT
#  include <stdarg.h>
#  include <stdio.h>
#  include <stddef.h>

/* Workarounds for MSVCRT bugs.

   The functions below are in Gnulib, but their prototypes and
   redirections must be here because the MS-Windows build omits the
   Gnulib stdio-h module, which does the below in Gnulib's stdio.h
   file, which is not used by the MS-Windows build.  */

extern size_t gl_consolesafe_fwrite (const void *ptr, size_t size,
				     size_t nmemb, FILE *fp)
  ARG_NONNULL ((1, 4));
extern int gl_consolesafe_fprintf (FILE *restrict fp,
				   const char *restrict format, ...)
  ATTRIBUTE_FORMAT_PRINTF (2, 3)
  ARG_NONNULL ((1, 2));
extern int gl_consolesafe_printf (const char *restrict format, ...)
  ATTRIBUTE_FORMAT_PRINTF (1, 2)
  ARG_NONNULL ((1));
extern int gl_consolesafe_vfprintf (FILE *restrict fp,
				    const char *restrict format, va_list args)
  ATTRIBUTE_FORMAT_PRINTF (2, 0)
  ARG_NONNULL ((1, 2));
extern int gl_consolesafe_vprintf (const char *restrict format, va_list args)
  ATTRIBUTE_FORMAT_PRINTF (1, 0)
  ARG_NONNULL ((1));
#  define fwrite gl_consolesafe_fwrite
#  define fprintf gl_consolesafe_fprintf
#  define printf gl_consolesafe_printf
#  define vfprintf gl_consolesafe_vfprintf
#  define vprintf gl_consolesafe_vprintf
# endif	/* !_UCRT */
#endif	/* WINDOWSNT */
