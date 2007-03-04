/* news-risc.h is for the "RISC News".

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

#ifdef NEWSOS5

/* NEWS-OS 5.0.2 */

#define LIBS_MACHINE -lmld

#ifdef __GNUC__
#define LD_SWITCH_MACHINE -g -Xlinker -D -Xlinker 800000
#else
#define C_DEBUG_SWITCH -g3
#define C_OPTIMIZE_SWITCH -g3
#define LD_SWITCH_MACHINE -g3 -D 800000 -non_shared
#endif

#else /* not NEWSOS5 */

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="bsd4-3"  */

#define COFF
#undef LD_SWITCH_MACHINE
#ifdef __GNUC__
#define LD_SWITCH_MACHINE -Xlinker -x -Xlinker -D -Xlinker 800000
#else
#define LD_SWITCH_MACHINE -x -D 800000
#endif

/* #define C_OPTIMIZE_SWITCH -O2 */
#define C_OPTIMIZE_SWITCH -O

#ifndef __GNUC__
#define C_DEBUG_SWITCH -g3
#endif

#undef TERMINFO

/* We have no mode_t.  */
#define NO_MODE_T

/* Don't use the definitions in m/mips.h.  */
#undef LINKER
#define LINKER $(CC) -nostdlib
#undef LIBS_MACHINE
#define LIBS_MACHINE -lmld

#undef KERNEL_FILE
#define KERNEL_FILE "/vmunix"

/* System's malloc, realloc, calloc and so on have bad prototypes,
   using char * instead of void *, so tell gmalloc not to use the
   prototypes.  */
#define BROKEN_PROTOTYPES

#endif /* not NEWSOS5 */

/* arch-tag: cf17300c-dd34-4b9d-a657-2de718469662
   (do not change this comment) */
