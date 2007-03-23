/* machine description file for Mips running RISCOS version 4.

   Copyright (C) 1992, 1999, 2001, 2002, 2003, 2004, 2005, 2006,
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


#include "mips.h"

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Use -opsystem=usg5-2-2 normally, or -opsystem=bsd4-3 with the BSD
world.
NOTE-END  */

#if 0
/* Define MIPS2 if you have an R6000 or R4000.  */
#define MIPS2
#endif

#ifdef __GNUC__
#define C_DEBUG_SWITCH -g -O
#else
/* We used to have  -systype bsd43, but a configure change
   now takes care of that option.  */
#ifdef MIPS2
#define C_DEBUG_SWITCH -DMips -g3 -Wf,-XNd4000 -O -Olimit 2000 -mips2
#else
#define C_DEBUG_SWITCH -DMips -g3 -Wf,-XNd4000 -O -Olimit 2000
#endif
#endif

#ifdef TERMINFO
#undef TERMINFO
#endif

#define START_FILES pre-crt0.o /lib/crt1.o
/* Used to have -lisode, but jlp@math.byu.edu says remove it
   (for RISCOS 4.52).  */
/* ethanb@ptolemy.astro.washington.edu says crtn.o uses _ctype
   and therefore we must search libc again after crtn.o.
   The -L is used to force second -lc to find the sysv version
   of libc.a, which is needed because the BSD libc.a
   doesn't have _ctype.  */
#define LIB_STANDARD -lmld -lc /lib/crtn.o -L/usr/lib -lc


#define COFF
#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -systype bsd43 -g3 -D 800000

#define NO_MODE_T

/* These are needed on Riscos 4.0.
   It appears that's the only system which uses mips4.h and defines BSD.  */
#ifdef BSD_SYSTEM
#undef HAVE_STRERROR
#undef HAVE_XRMSETDATABASE
#undef HAVE_XSCREENRESOURCESTRING
#undef HAVE_SETSID
#endif

/* arch-tag: 56050454-0df5-4de9-b1b7-0c6ab400313c
   (do not change this comment) */
