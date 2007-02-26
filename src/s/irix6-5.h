/* Definitions file for GNU Emacs running on Silicon Graphics Irix system 6.5.

Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
  2007  Free Software Foundation, Inc.

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


#define IRIX6_5			/* used in m/iris4d */
#include "irix5-0.h"

#if _MIPS_SZLONG == 64		/* -mabi=64 (gcc) or -64 (MIPSpro) */
#define _LP64			/* lisp.h takes care of the rest */
#endif /* _MIPS_SZLONG */

/* This macro definition, which we inherited from irix5-0.h,
   is needed in configure on Irix 5, but gets in the way there
   on Irix 6.  So get rid of it except in Makefile.in where we need it.  */
#ifndef THIS_IS_MAKEFILE
#undef C_SWITCH_SYSTEM
#endif

/* The only supported 32-bit configuration of GCC under IRIX6.x produces
   n32 MIPS ABI binaries and also supports -g. */
#ifdef __GNUC__
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g
#else
/* Optimize, inaccurate debugging, increase limit on size of what's
   optimized.

   This should also be applicable other than on Irix 6.5, but I don't
   know for which compiler versions.  -- fx */
#define C_DEBUG_SWITCH -g3 -O -OPT:Olimit=3500
#endif

#undef SA_RESTART

/* Cancel the #define that is in irix5-0.h.  */
#undef ospeed

#undef TIOCSIGSEND		/* defined in usg5-4.h */

/* Tested on Irix 6.5.  SCM worked on earlier versions.  */
#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS

/* arch-tag: d7ad9ec2-54ad-4b2f-adf2-0070c5c63e83
   (do not change this comment) */
