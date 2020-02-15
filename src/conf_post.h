/* conf_post.h --- configure.ac includes this via AH_BOTTOM

Copyright (C) 1988, 1993-1994, 1999-2002, 2004-2020 Free Software
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
   does not #include other files.  The exceptions are first stdbool.h
   because it is unlikely to interfere with configuration and bool is
   such a core part of the C language, and second ms-w32.h (DOS_NT
   only) because it historically was included here and changing that
   would take some work.  */

#include <stdbool.h>

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

/* Simulate __has_attribute on compilers that lack it.  It is used only
   on arguments like alloc_size that are handled in this simulation.
   __has_attribute should be used only in #if expressions, as Oracle
   Studio 12.5's __has_attribute does not work in plain code.  */
#ifndef __has_attribute
# define __has_attribute(a) __has_attribute_##a
# define __has_attribute_alloc_size GNUC_PREREQ (4, 3, 0)
# define __has_attribute_cleanup GNUC_PREREQ (3, 4, 0)
# define __has_attribute_cold GNUC_PREREQ (4, 3, 0)
# define __has_attribute_externally_visible GNUC_PREREQ (4, 1, 0)
# define __has_attribute_no_address_safety_analysis false
# define __has_attribute_no_sanitize_address GNUC_PREREQ (4, 8, 0)
# define __has_attribute_no_sanitize_undefined GNUC_PREREQ (4, 9, 0)
# define __has_attribute_warn_unused_result GNUC_PREREQ (3, 4, 0)
#endif

/* Simulate __has_feature on compilers that lack it.  It is used only
   to define ADDRESS_SANITIZER below.  */
#ifndef __has_feature
# define __has_feature(a) false
#endif

/* True if addresses are being sanitized.  */
#if defined __SANITIZE_ADDRESS__ || __has_feature (address_sanitizer)
# define ADDRESS_SANITIZER true
#else
# define ADDRESS_SANITIZER false
#endif

#if defined DARWIN_OS && defined emacs && defined HAVE_UNEXEC
# define malloc unexec_malloc
# define realloc unexec_realloc
# define free unexec_free
#endif

/* If HYBRID_MALLOC is defined (e.g., on Cygwin), emacs will use
   gmalloc before dumping and the system malloc after dumping.
   hybrid_malloc and friends, defined in gmalloc.c, are wrappers that
   accomplish this.  */
#ifdef HYBRID_MALLOC
#ifdef emacs
#define malloc hybrid_malloc
#define realloc hybrid_realloc
#define aligned_alloc hybrid_aligned_alloc
#define calloc hybrid_calloc
#define free hybrid_free
#endif
#endif	/* HYBRID_MALLOC */

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

/* We must intercept 'opendir' calls to stash away the directory name,
   so we could reuse it in readlinkat; see msdos.c.  */
#define opendir sys_opendir

/* End of gnulib-related stuff.  */

#define emacs_raise(sig) msdos_fatal_signal (sig)

/* DATA_START is needed by vm-limit.c and unexcoff.c. */
#define DATA_START (&etext + 1)

/* Define one of these for easier conditionals.  */
#ifdef HAVE_X_WINDOWS
/* We need a little extra space, see ../../lisp/loadup.el and the
   commentary below, in the non-X branch.  The 140KB number was
   measured on GNU/Linux and on MS-Windows.  */
#define SYSTEM_PURESIZE_EXTRA (-170000+140000)
#else
/* We need a little extra space, see ../../lisp/loadup.el.
   As of 20091024, DOS-specific files use up 62KB of pure space.  But
   overall, we end up wasting 130KB of pure space, because
   BASE_PURESIZE starts at 1.47MB, while we need only 1.3MB (including
   non-DOS specific files and load history; the latter is about 55K,
   but depends on the depth of the top-level Emacs directory in the
   directory tree).  Given the unknown policy of different DPMI
   hosts regarding loading of untouched pages, I'm not going to risk
   enlarging Emacs footprint by another 100+ KBytes.  */
#define SYSTEM_PURESIZE_EXTRA (-170000+90000)
#endif
#endif  /* MSDOS */

/* macOS / GNUstep need a bit more pure memory.  Of the existing knobs,
   SYSTEM_PURESIZE_EXTRA seems like the least likely to cause problems.  */
#ifdef HAVE_NS
#if defined NS_IMPL_GNUSTEP
#  define SYSTEM_PURESIZE_EXTRA 30000
#elif defined DARWIN_OS
#  define SYSTEM_PURESIZE_EXTRA 200000
#endif
#endif

#ifdef CYGWIN
#define SYSTEM_PURESIZE_EXTRA 50000
#endif

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
#define RE_TRANSLATE_P(TBL) (!EQ (TBL, make_fixnum (0)))
#endif

/* Tell time_rz.c to use Emacs's getter and setter for TZ.
   Only Emacs uses time_rz so this is OK.  */
#define getenv_TZ emacs_getenv_TZ
#define setenv_TZ emacs_setenv_TZ
extern char *emacs_getenv_TZ (void);
extern int emacs_setenv_TZ (char const *);

/* Avoid __attribute__ ((cold)) on MinGW; see thread starting at
   <https://lists.gnu.org/r/emacs-devel/2019-04/msg01152.html>. */
#if __has_attribute (cold) && !defined __MINGW32__
# define ATTRIBUTE_COLD __attribute__ ((cold))
#else
# define ATTRIBUTE_COLD
#endif

#if __GNUC__ >= 3  /* On GCC 3.0 we might get a warning.  */
#define NO_INLINE __attribute__((noinline))
#else
#define NO_INLINE
#endif

#if __has_attribute (externally_visible)
#define EXTERNALLY_VISIBLE __attribute__((externally_visible))
#else
#define EXTERNALLY_VISIBLE
#endif

#if GNUC_PREREQ (2, 7, 0)
# define ATTRIBUTE_FORMAT(spec) __attribute__ ((__format__ spec))
#else
# define ATTRIBUTE_FORMAT(spec) /* empty */
#endif

#if GNUC_PREREQ (7, 0, 0)
# define FALLTHROUGH __attribute__ ((__fallthrough__))
#else
# define FALLTHROUGH ((void) 0)
#endif

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
  ATTRIBUTE_FORMAT ((PRINTF_ARCHETYPE, string_index, first_to_check))

#define ARG_NONNULL _GL_ARG_NONNULL
#define ATTRIBUTE_CONST _GL_ATTRIBUTE_CONST
#define ATTRIBUTE_UNUSED _GL_UNUSED

#if GNUC_PREREQ (3, 3, 0) && !defined __ICC
# define ATTRIBUTE_MAY_ALIAS __attribute__ ((__may_alias__))
#else
# define ATTRIBUTE_MAY_ALIAS
#endif

/* Declare NAME to be a pointer to an object of type TYPE, initialized
   to the address ADDR, which may be of a different type.  Accesses
   via NAME may alias with other accesses with the traditional
   behavior, even if options like gcc -fstrict-aliasing are used.  */

#define DECLARE_POINTER_ALIAS(name, type, addr) \
  type ATTRIBUTE_MAY_ALIAS *name = (type *) (addr)

#if 3 <= __GNUC__
# define ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
# define ATTRIBUTE_SECTION(name) __attribute__((section (name)))
#else
# define ATTRIBUTE_MALLOC
#define ATTRIBUTE_SECTION(name)
#endif

#if __has_attribute (alloc_size)
# define ATTRIBUTE_ALLOC_SIZE(args) __attribute__ ((__alloc_size__ args))
#else
# define ATTRIBUTE_ALLOC_SIZE(args)
#endif

#define ATTRIBUTE_MALLOC_SIZE(args) ATTRIBUTE_MALLOC ATTRIBUTE_ALLOC_SIZE (args)

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

#if __has_attribute (no_sanitize_address)
# define ATTRIBUTE_NO_SANITIZE_ADDRESS \
    __attribute__ ((no_sanitize_address)) ADDRESS_SANITIZER_WORKAROUND
#elif __has_attribute (no_address_safety_analysis)
# define ATTRIBUTE_NO_SANITIZE_ADDRESS \
    __attribute__ ((no_address_safety_analysis)) ADDRESS_SANITIZER_WORKAROUND
#else
# define ATTRIBUTE_NO_SANITIZE_ADDRESS
#endif

/* Attribute of functions whose undefined behavior should not be sanitized.  */

#if __has_attribute (no_sanitize_undefined)
# define ATTRIBUTE_NO_SANITIZE_UNDEFINED __attribute__ ((no_sanitize_undefined))
#elif __has_attribute (no_sanitize)
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

/* Use 'static' instead of 'extern inline' because 'static' typically
   has better performance for Emacs.  Do not use the 'inline' keyword,
   as modern compilers inline automatically.  ATTRIBUTE_UNUSED
   pacifies gcc -Wunused-function.  */

# ifndef INLINE
#  define INLINE EXTERN_INLINE
# endif
# define EXTERN_INLINE static ATTRIBUTE_UNUSED
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
