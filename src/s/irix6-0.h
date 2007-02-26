/* Definitions file for GNU Emacs running on Silicon Graphics Irix system 6.0.

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


#include "irix5-0.h"

/* Irix 6 tries to do 64 bits, but doesn't do it fully,
   so inhibit that.  */
#define IRIX_FORCE_32_BITS

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -32
#endif

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
#endif

#undef SA_RESTART

/* It turns out that the #define in irix5-0.h is needed in Irix 6 as well.  */
#if 0
/* Cancel the #define that is in irix5-0.h.  */
#undef ospeed
#endif

#undef TIOCSIGSEND

/* Tested on Irix 6.5.  SCM worked on earlier versions.  */
#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS

/* arch-tag: a775e465-a619-4655-a58f-5982aad0c624
   (do not change this comment) */
