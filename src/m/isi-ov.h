/* machine description file for ISI 68000's
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


/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Use `-opsystem=bsd4-2' or `-opsystem=bsd4-3'.
NOTE-END */

#define ISI68K

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Say this machine is a 68000 */

#define m68000

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#ifdef BSD4_3
#define LOAD_AVE_TYPE long
#else
#define LOAD_AVE_TYPE double
#endif BSD4_3

/* Convert that into an integer that is 100 for a load average of 1.0  */

#ifdef BSD4_3
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)
#else
#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))
#endif

/* Mask for address bits within a memory segment */

#define SEGMENT_MASK 0x1ffff

/* use the -20 switch to get the 68020 code */
/* #define C_SWITCH_MACHINE -20 */

/* Use the version of the library for the 68020
   because the standard library requires some special hacks in crt0
   which the GNU crt0 does not have.  */

#define LIB_STANDARD -lmc

/* macros to make unexec work right */

#define A_TEXT_OFFSET(HDR) sizeof(HDR)
#define A_TEXT_SEEK(HDR) sizeof(HDR)

/* A few changes for the newer systems.  */

#ifdef BSD4_3
/* The following line affects crt0.c.  */
#undef m68k

#undef LIB_STANDARD
#define LIB_STANDARD -lmc -lc
#define C_DEBUG_SWITCH -20 -O -X23
#endif
