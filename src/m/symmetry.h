/* machine description file for SEQUENT SYMMETRY machines
   Copyright (C) 1985, 1986, 2002 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* CHANGE: [Eric H. Herrin II; eric@ms.uky.edu - 15 Sept 1988] 
 * Modified the sequent.h file for the Sequent Symmetry machine.   
 * Biggest change was to align the sections in the a.out to 4K 
 * boundaries (this is the page size).
 */


/* NOTICE: this file works for DYNIX release 3.0.12 on Sequent Symmetry
 * (Intel 80386) machines.  Hasn't been tested on anything else.
 */ 

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="bsd4-3"  */

#include "intel386.h"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE unsigned long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define	FSCALE	1000.0
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Name of file the to look in
   for the kernel symbol table (for load average) */

#undef KERNEL_FILE
#define KERNEL_FILE "/dynix"

/* Avoids a compiler bug */

#define TAHOE_REGISTER_BUG

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  Furthermore, the value written
   in the a_text in the file must have N_ADDRADJ added to it.  */

#define A_TEXT_OFFSET(HDR) (sizeof (HDR) + N_ADDRADJ (HDR))

/* This is the offset of the executable's text, from the start of the file.  */

#define A_TEXT_SEEK(HDR) (N_TXTOFF (hdr) + sizeof (hdr))

/* The file sections in the Symmetry a.out must be on 4K boundaries.
 */
#define SEGSIZ 4096
#define SECTION_ALIGNMENT	(SEGSIZ-1)

/* (short) negative-int doesn't sign-extend correctly */
#define SHORT_CAST_BUG

/* Cause compilations to be done in parallel in ymakefile.  */
#define MAKE_PARALLEL &

/* Define how to search all pty names.
   This is for Dynix 3.0; delete next 5 definitions for older systems.  */

#define PTY_MAJOR "pqrstuvwPQRSTUVW"
#define PTY_MINOR "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define PTY_ITERATION					\
  register int ma, mi;					\
  for (ma = 0; ma < sizeof(PTY_MAJOR) - 1; ma++)	\
    for (mi = 0; mi < sizeof(PTY_MINOR) - 1; mi++)
#define PTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/pty%c%c", PTY_MAJOR[ma], PTY_MINOR[mi]);
#define PTY_TTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/tty%c%c", PTY_MAJOR[ma], PTY_MINOR[mi]);
