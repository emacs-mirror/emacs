/* machine description file for Sun 68000's
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Sun 1, 2 and 3 (-machine=sun1, -machine=sun2, -machine=sun3;
                -opsystem=bsd4-2 or -opsystem=sunos4)

  Whether you should use sun1, sun2 or sun3 depends on the
		   VERSION OF THE OPERATING SYSTEM
  you have.  There are three machine types for different versions of
  SunOS.  All are derived from Berkeley 4.2, meaning that you should
  use -opsystem=bsd4-2.  Emacs 17 has run on all of them.  You will
  need to use sun3 on Sun 2's running SunOS release 3.

  For SunOS release 4 on a Sun 3, use -machine=sun3 and
  -opsystem=sunos4.  See the file share-lib/SUNBUG for how to solve
  problems caused by bugs in the "export" version of SunOS 4.
NOTE-END  */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* 68000 has lowest-numbered byte as most significant */

#define BIG_ENDIAN

/* Say this machine is a 68000 */

#define m68000

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Sun can't write competent compilers */
#define COMPILER_REGISTER_BUG

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Must use the system's termcap.  It does special things.  */

#define LIBS_TERMCAP -ltermcap

/* Mask for address bits within a memory segment */

#define SEGMENT_MASK (SEGSIZ - 1)
