/* m- file for Mips machines.
   Copyright (C) 1987, 1992, 1999, 2001, 2002, 2003, 2004, 2005, 2006,
                 2007, 2008, 2009, 2010, 2011, 2012  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
This is used on GNU/Linux and netbsd.
NOTE-END  */

/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */

#if ! (defined (__MIPSEL__) || defined (MIPSEL) || defined (_MIPSEL))
#define WORDS_BIG_ENDIAN
#endif

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   This flag only matters if you use USE_LISP_UNION_TYPE.  */

#define EXPLICIT_SIGN_EXTEND

/* Describe layout of the address space in an executing process.  */

#define TEXT_START      0x00400000


#if 0 /* These definitions were advantageous when not using
	 USE_LSB_TAG.  With that, they get ignored but cause errors.  */

#define DATA_SEG_BITS	0x10000000

/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short.  */

#define XUINT(a) (((unsigned)(a) << (BITS_PER_INT-VALBITS)) >> (BITS_PER_INT-VALBITS))

#define XSET(var, type, ptr)						\
  ((var) =								\
   ((int)(type) << VALBITS)						\
   + (((unsigned) (ptr) << (BITS_PER_INT-VALBITS)) >> (BITS_PER_INT-VALBITS)))

/* arch-tag: 8fd020ee-78a7-4d87-96ce-6129f52f7bee
   (do not change this comment) */

#endif /* 0 */

