/* Includes for memory limit warnings.
   Copyright (C) 1990, 1993, 1994, 1995 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef MSDOS
#include <dpmi.h>
#endif

/* Some systems need this before <sys/resource.h>.  */
#include <sys/types.h>

#ifdef _LIBC

#include <sys/resource.h>
#define BSD4_2			/* Tell code below to use getrlimit.  */

extern int __data_start;
#define start_of_data()	&__data_start

#else

#if defined (__osf__) && (defined (__mips) || defined (mips) || defined(__alpha))
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef __bsdi__
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
#ifdef __STDC__
typedef void *POINTER;
#else
typedef char *POINTER;
#endif

typedef unsigned long SIZE;

#ifdef NULL
#undef NULL
#endif
#define NULL ((POINTER) 0)

extern POINTER start_of_data ();
#ifdef DATA_SEG_BITS
#define EXCEEDS_LISP_PTR(ptr) \
  (((EMACS_UINT) (ptr) & ~DATA_SEG_BITS) >> VALBITS)
#else
#define EXCEEDS_LISP_PTR(ptr) ((EMACS_UINT) (ptr) >> VALBITS)
#endif

#ifdef BSD
#ifndef DATA_SEG_BITS
extern char etext;
#define start_of_data() &etext
#endif
#endif

#else  /* Not emacs */ 
extern char etext;
#define start_of_data() &etext
#endif /* Not emacs */

#endif /* _LIBC */


/* start of data space; can be changed by calling malloc_init */
static POINTER data_space_start;

/* Number of bytes of writable memory we can expect to be able to get */
static unsigned int lim_data;

#ifdef NO_LIM_DATA
static void
get_lim_data ()
{
  lim_data = -1;
}
#else /* not NO_LIM_DATA */

#ifdef USG

static void
get_lim_data ()
{
  extern long ulimit ();

  lim_data = -1;

  /* Use the ulimit call, if we seem to have it.  */
#if !defined (ULIMIT_BREAK_VALUE) || defined (LINUX)
  lim_data = ulimit (3, 0);
#endif

  /* If that didn't work, just use the macro's value.  */
#ifdef ULIMIT_BREAK_VALUE
  if (lim_data == -1)
    lim_data = ULIMIT_BREAK_VALUE;
#endif

  lim_data -= (long) data_space_start;
}

#else /* not USG */
#ifdef WINDOWSNT

static void
get_lim_data ()
{
  extern unsigned long data_region_size;
  lim_data = data_region_size;
}

#else
#if !defined (BSD4_2) && !defined (__osf__)

#ifdef MSDOS
void
get_lim_data ()
{
  _go32_dpmi_meminfo info;

  _go32_dpmi_get_free_memory_information (&info);
  lim_data = info.available_memory;
}
#else /* not MSDOS */
static void
get_lim_data ()
{
  lim_data = vlimit (LIM_DATA, -1);
}
#endif /* not MSDOS */

#else /* BSD4_2 */

static void
get_lim_data ()
{
  struct rlimit XXrlimit;

  getrlimit (RLIMIT_DATA, &XXrlimit);
#ifdef RLIM_INFINITY
  lim_data = XXrlimit.rlim_cur & RLIM_INFINITY; /* soft limit */
#else
  lim_data = XXrlimit.rlim_cur;	/* soft limit */
#endif
}
#endif /* BSD4_2 */
#endif /* not WINDOWSNT */
#endif /* not USG */
#endif /* not NO_LIM_DATA */
