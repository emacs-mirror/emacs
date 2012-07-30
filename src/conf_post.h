/* conf_post.h --- configure.ac includes this via AH_BOTTOM

Copyright (C) 1988, 1993-1994, 1999-2002, 2004-2012
  Free Software Foundation, Inc.

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

/* On AIX 3 this must be included before any other include file.  */
#include <alloca.h>
#if ! HAVE_ALLOCA
# error "alloca not available on this machine"
#endif

#ifdef SIGNAL_H_AHB
#undef SIGNAL_H_AHB
#include <signal.h>
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

/* Define AMPERSAND_FULL_NAME if you use the convention
   that & in the full name stands for the login id.  */
/* Turned on June 1996 supposing nobody will mind it.  */
#define AMPERSAND_FULL_NAME

/* `subprocesses' should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   Only MSDOS does not support this (it overrides
   this in its config_opsysfile below).  */

#define subprocesses

/* Include the os dependent file.  */
#ifdef config_opsysfile
# include config_opsysfile
#endif

/* Mac OS X / GNUstep need a bit more pure memory.  Of the existing knobs,
   SYSTEM_PURESIZE_EXTRA seems like the least likely to cause problems.  */
#ifdef HAVE_NS
#if defined NS_IMPL_GNUSTEP
#  define SYSTEM_PURESIZE_EXTRA 30000
#elif defined DARWIN_OS
#  define SYSTEM_PURESIZE_EXTRA 200000
#endif
#endif

#ifdef emacs /* Don't do this for lib-src.  */
/* Tell regex.c to use a type compatible with Emacs.  */
#define RE_TRANSLATE_TYPE Lisp_Object
#define RE_TRANSLATE(TBL, C) CHAR_TABLE_TRANSLATE (TBL, C)
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

/* conf_post.h ends here */
