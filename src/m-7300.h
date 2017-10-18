/* m- file for AT&T UNIX PC model 7300
   Copyright (C) 1986 Free Software Foundation, Inc.
   Modified for this machine by mtxinu!rtech!gonzo!daveb

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This machine does not have flexnames.  Yuk */

/* # define SHORTNAMES */


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define BIG_ENDIAN

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000 are the ones defined so far.  */

# ifndef mc68k
# define mc68k
# endif
#ifndef m68k
#define m68k
#endif

/* Cause crt0.c to define errno.  */

#define NEED_ERRNO

/* Data type of load average, as read out of kmem.  */
/* These are commented out since it is not supported by this machine.  */
  
/* #define LOAD_AVE_TYPE long */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0) */

#define SWITCH_ENUM_BUG

/* These three lines were new in 18.50.  They were said to permit
   a demand-paged executable, but someone else says they don't work.
   Someone else says they do.  They didn't work because errno was an
   initialized variable in crt0.c, and because of %splimit (also therein),
   both of which have been fixed now. */
#define SECTION_ALIGNMENT 0x03ff
#define SEGMENT_MASK 0xffff
#define LD_SWITCH_MACHINE -z
