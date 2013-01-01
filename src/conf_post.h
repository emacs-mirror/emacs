/* conf_post.h --- configure.ac includes this via AH_BOTTOM

Copyright (C) 1988, 1993-1994, 1999-2002, 2004-2013 Free Software
Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

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

#ifndef WINDOWSNT
/* On AIX 3 this must be included before any other include file.  */
#include <alloca.h>
#if ! HAVE_ALLOCA
# error "alloca not available on this machine"
#endif
#endif

/* This silences a few compilation warnings on FreeBSD.  */
#ifdef BSD_SYSTEM_AHB
#undef BSD_SYSTEM_AHB
#undef BSD_SYSTEM
#if __FreeBSD__ == 1
#define BSD_SYSTEM 199103
#elif __FreeBSD__ == 2
#define BSD_SYSTEM 199306
#elif __FreeBSD__ >= 3
#define BSD_SYSTEM 199506
#endif
#endif

#ifdef DARWIN_OS
#ifdef emacs
#define malloc unexec_malloc
#define realloc unexec_realloc
#define free unexec_free
/* Don't use posix_memalign because it is not compatible with unexmacosx.c.  */
#undef HAVE_POSIX_MEMALIGN
#endif
/* The following solves the problem that Emacs hangs when evaluating
   (make-comint "test0" "/nodir/nofile" nil "") when /nodir/nofile
   does not exist.  Also, setsid is not allowed in the vfork child's
   context as of Darwin 9/Mac OS X 10.5.  */
#undef HAVE_WORKING_VFORK
#define vfork fork
#endif  /* DARWIN_OS */

/* We have to go this route, rather than the old hpux9 approach of
   renaming the functions via macros.  The system's stdlib.h has fully
   prototyped declarations, which yields a conflicting definition of
   srand48; it tries to redeclare what was once srandom to be srand48.
   So we go with HAVE_LRAND48 being defined.  */
#ifdef HPUX
#undef srandom
#undef random
/* We try to avoid checking for random and rint on hpux in
   configure.ac, but some other configure test might check for them as
   a dependency, so to be safe we also undefine them here.
 */
#undef HAVE_RANDOM
#undef HAVE_RINT
#endif  /* HPUX */

#ifdef IRIX6_5
#ifdef emacs
char *_getpty();
#endif

#endif /* IRIX6_5 */

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
/* The "portable" definition of _GL_INLINE on config.h does not work
   with DJGPP GCC 3.4.4: it causes unresolved externals in sysdep.c,
   although lib/execinfo.h is included and the inline functions there
   are visible.  */
#if __GNUC__ < 4
# define _GL_EXECINFO_INLINE inline
#endif
/* End of gnulib-related stuff.  */

#define emacs_raise(sig) msdos_fatal_signal (sig)

#ifndef HAVE_SETPGID
# ifdef USG
#  define setpgid(pid, pgid) setpgrp ()
# else
#  define setpgid(pid, pgid) setpgrp (pid, pgid)
# endif
#endif

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
#define SYSTEM_PURESIZE_EXTRA (-170000+65000)
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
#endif

#ifdef emacs /* Don't do this for lib-src.  */
/* Tell regex.c to use a type compatible with Emacs.  */
#define RE_TRANSLATE_TYPE Lisp_Object
#define RE_TRANSLATE(TBL, C) char_table_translate (TBL, C)
#ifdef make_number
/* If make_number is a macro, use it.  */
#define RE_TRANSLATE_P(TBL) (!EQ (TBL, make_number (0)))
#else
/* If make_number is a function, avoid it.  */
#define RE_TRANSLATE_P(TBL) (!(INTEGERP (TBL) && XINT (TBL) == 0))
#endif
#endif

#include <string.h>
#include <stdlib.h>

#if __GNUC__ >= 3  /* On GCC 3.0 we might get a warning.  */
#define NO_INLINE __attribute__((noinline))
#else
#define NO_INLINE
#endif

#if (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1))
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

/* Some versions of GNU/Linux define noinline in their headers.  */
#ifdef noinline
#undef noinline
#endif

#define INLINE _GL_INLINE
#define EXTERN_INLINE _GL_EXTERN_INLINE
#define INLINE_HEADER_BEGIN _GL_INLINE_HEADER_BEGIN
#define INLINE_HEADER_END _GL_INLINE_HEADER_END

/* Use this to suppress gcc's `...may be used before initialized' warnings. */
#ifdef lint
/* Use CODE only if lint checking is in effect.  */
# define IF_LINT(Code) Code
/* Assume that the expression COND is true.  This differs in intent
   from 'assert', as it is a message from the programmer to the compiler.  */
# define lint_assume(cond) ((cond) ? (void) 0 : abort ())
#else
# define IF_LINT(Code) /* empty */
# define lint_assume(cond) ((void) (0 && (cond)))
#endif

/* conf_post.h ends here */
