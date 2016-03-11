/* conf_post.h --- configure.ac includes this via AH_BOTTOM

Copyright (C) 1988, 1993-1994, 1999-2002, 2004-2016 Free Software
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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/* Commentary:

   Rather than writing this code directly in AH_BOTTOM, we include it
   via this file.  This is so that it does not get processed by
   autoheader.  Eg, any undefs here would otherwise be commented out.
*/

/* Code: */

/* Include any platform specific configuration file.  */
#ifdef config_opsysfile
# include config_opsysfile
#endif

#include <stdbool.h>

/* The type of bool bitfields.  Needed to compile Objective-C with
   standard GCC.  It was also needed to port to pre-C99 compilers,
   although we don't care about that any more.  */
#if NS_IMPL_GNUSTEP
typedef unsigned int bool_bf;
#else
typedef bool bool_bf;
#endif

#ifndef WINDOWSNT
/* On AIX 3 this must be included before any other include file.  */
#include <alloca.h>
#if ! HAVE_ALLOCA
# error "alloca not available on this machine"
#endif
#endif

/* Simulate __has_attribute on compilers that lack it.  It is used only
   on arguments like alloc_size that are handled in this simulation.  */
#ifndef __has_attribute
# define __has_attribute(a) __has_attribute_##a
# define __has_attribute_alloc_size (4 < __GNUC__ + (3 <= __GNUC_MINOR__))
# define __has_attribute_cleanup (3 < __GNUC__ + (4 <= __GNUC_MINOR__))
# define __has_attribute_externally_visible \
    (4 < __GNUC__ + (1 <= __GNUC_MINOR__))
# define __has_attribute_no_address_safety_analysis false
# define __has_attribute_no_sanitize_address \
    (4 < __GNUC__ + (8 <= __GNUC_MINOR__))
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

#ifdef DARWIN_OS
#ifdef emacs
#define malloc unexec_malloc
#define realloc unexec_realloc
#define free unexec_free
#endif
/* The following solves the problem that Emacs hangs when evaluating
   (make-comint "test0" "/nodir/nofile" nil "") when /nodir/nofile
   does not exist.  Also, setsid is not allowed in the vfork child's
   context as of Darwin 9/Mac OS X 10.5.  */
#undef HAVE_WORKING_VFORK
#define vfork fork
#endif  /* DARWIN_OS */

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

/* Mac OS X / GNUstep need a bit more pure memory.  Of the existing knobs,
   SYSTEM_PURESIZE_EXTRA seems like the least likely to cause problems.  */
#ifdef HAVE_NS
#if defined NS_IMPL_GNUSTEP
#  define SYSTEM_PURESIZE_EXTRA 30000
#elif defined DARWIN_OS
#  define SYSTEM_PURESIZE_EXTRA 200000
#endif
#endif

#ifdef CYGWIN
#define SYSTEM_PURESIZE_EXTRA 10000
#endif

#if defined HAVE_NTGUI && !defined DebPrint
# ifdef EMACSDEBUG
extern void _DebPrint (const char *fmt, ...);
#  define DebPrint(stuff) _DebPrint stuff
# else
#  define DebPrint(stuff)
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
#define RE_TRANSLATE_P(TBL) (!EQ (TBL, make_number (0)))
#endif

/* Tell time_rz.c to use Emacs's getter and setter for TZ.
   Only Emacs uses time_rz so this is OK.  */
#define getenv_TZ emacs_getenv_TZ
#define setenv_TZ emacs_setenv_TZ
extern char *emacs_getenv_TZ (void);
extern int emacs_setenv_TZ (char const *);

#include <string.h>
#include <stdlib.h>

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

#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7)
# define ATTRIBUTE_FORMAT(spec) __attribute__ ((__format__ spec))
#else
# define ATTRIBUTE_FORMAT(spec) /* empty */
#endif

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)
# define ATTRIBUTE_FORMAT_PRINTF(formatstring_parameter, first_argument) \
   ATTRIBUTE_FORMAT ((__gnu_printf__, formatstring_parameter, first_argument))
#else
# define ATTRIBUTE_FORMAT_PRINTF(formatstring_parameter, first_argument) \
   ATTRIBUTE_FORMAT ((__printf__, formatstring_parameter, first_argument))
#endif

#define ATTRIBUTE_CONST _GL_ATTRIBUTE_CONST
#define ATTRIBUTE_UNUSED _GL_UNUSED

#if 3 <= __GNUC__
# define ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
#else
# define ATTRIBUTE_MALLOC
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
     || ((4 < __GNUC__ + (9 <= __GNUC_MINOR__)) \
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

/* Some versions of GNU/Linux define noinline in their headers.  */
#ifdef noinline
#undef noinline
#endif

/* Use Gnulib's extern-inline module for extern inline functions.
   An include file foo.h should prepend FOO_INLINE to function
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

   The INLINE_HEADER_BEGIN and INLINE_HEADER_END suppress bogus
   warnings in some GCC versions; see ../m4/extern-inline.m4.

   C99 compilers compile functions like 'incr' as C99-style extern
   inline functions.  Buggy GCC implementations do something similar with
   GNU-specific keywords.  Buggy non-GCC compilers use static
   functions, which bloats the code but is good enough.  */

#ifndef INLINE
# define INLINE _GL_INLINE
#endif
#define EXTERN_INLINE _GL_EXTERN_INLINE
#define INLINE_HEADER_BEGIN _GL_INLINE_HEADER_BEGIN
#define INLINE_HEADER_END _GL_INLINE_HEADER_END

/* To use the struct hack with N elements, declare the struct like this:
     struct s { ...; t name[FLEXIBLE_ARRAY_MEMBER]; };
   and allocate (offsetof (struct s, name) + N * sizeof (t)) bytes.
   IBM xlc 12.1 claims to do C99 but mishandles flexible array members.  */
#ifdef __IBMC__
# define FLEXIBLE_ARRAY_MEMBER 1
#else
# define FLEXIBLE_ARRAY_MEMBER
#endif

/* Use this to suppress gcc's `...may be used before initialized' warnings. */
#ifdef lint
/* Use CODE only if lint checking is in effect.  */
# define IF_LINT(Code) Code
#else
# define IF_LINT(Code) /* empty */
#endif

/* conf_post.h ends here */
