/* Includes for memory limit warnings.
   Copyright (C) 1990, 1993, 1994, 1995, 1996, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifdef MSDOS
#include <dpmi.h>
extern int etext;
#endif

/* Some systems need this before <sys/resource.h>.  */
#include <sys/types.h>

#ifdef _LIBC

#include <sys/resource.h>
#define BSD4_2			/* Tell code below to use getrlimit.  */

/* Old Linux startup code won't define __data_start.  */
extern int etext, __data_start; weak_extern (__data_start)
#define start_of_data()	(&__data_start ?: &etext)

#else /* not _LIBC */

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#else
# if HAVE_SYS_VLIMIT_H
#  include <sys/vlimit.h>	/* Obsolete, says glibc */
# endif
#endif

#ifdef __bsdi__
#define BSD4_2
#endif

#ifdef CYGWIN
#define BSD4_2
#endif

#ifndef BSD4_2
#ifndef USG
#ifndef MSDOS
#ifndef WINDOWSNT
#include <sys/vlimit.h>
#endif /* not WINDOWSNT */
#endif /* not MSDOS */
#endif /* not USG */
#else /* if BSD4_2 */
#include <sys/time.h>
#include <sys/resource.h>
#endif /* BSD4_2 */

#ifdef emacs
/* The important properties of this type are that 1) it's a pointer, and
   2) arithmetic on it should work as if the size of the object pointed
   to has a size of 1.  */
typedef POINTER_TYPE *POINTER;

typedef unsigned long SIZE;

#ifdef NULL
#undef NULL
#endif
#define NULL ((POINTER) 0)

extern POINTER start_of_data ();
#if defined USE_LSB_TAG
#define EXCEEDS_LISP_PTR(ptr) 0
#elif defined DATA_SEG_BITS
#define EXCEEDS_LISP_PTR(ptr) \
  (((EMACS_UINT) (ptr) & ~DATA_SEG_BITS) >> VALBITS)
#else
#define EXCEEDS_LISP_PTR(ptr) ((EMACS_UINT) (ptr) >> VALBITS)
#endif

#ifdef DATA_START
#define start_of_data() ((char *)DATA_START)
#endif

#ifdef BSD_SYSTEM
#ifndef DATA_SEG_BITS
#ifndef DATA_START
extern char etext;
#define start_of_data() &etext
#endif
#endif
#endif

#else  /* not emacs */
extern char etext;
#define start_of_data() &etext
#endif /* not emacs */

#endif /* not _LIBC */


/* arch-tag: fe39244e-e54f-4208-b7aa-02556f7841c5
   (do not change this comment) */
