/* machine description file for Sony's NEWS workstations, NEWS-OS 3.0.
   Copyright (C) 1985, 1986, 1989 Free Software Foundation, Inc.

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
Use -opsystem=bsd4-2, or -opsystem=bsd4-3 for system release 3.
NOTE-END  */

/* Say this machine is a 68000 */

#ifndef m68000
#define m68000
#endif

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* 68000 has lowest-numbered byte as most significant */

#define BIG_ENDIAN

/* One CRT0 Dummy variable */

#define CRT0_DUMMIES one_dummy,

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* The News machine has alloca. */

#define HAVE_ALLOCA

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* Must use the system's termcap.  It does special things.  */

#define LIBS_TERMCAP -ltermcap

