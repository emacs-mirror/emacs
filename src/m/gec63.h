/* machine description file for gec63
   Copyright (C) 1986 Free Software Foundation, Inc.

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
   USUAL-OPSYS="usg5-2"  */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Say this machine is a 68000 */

#define gec63

/* Use an int to represent Lisp_Object */

#define NO_UNION_TYPE

/* GEC63 has alloca in the PW/ux63 library.  */
#define LIB_STANDARD -lPW -lc
#define HAVE_ALLOCA

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no /dev/kmem */

#undef ADDR_CORRECT(x)
#define NO_ARG_ARRAY

#undef TERMCAP
#define TERMINFO

/* Define sizes of portions of a Lisp_Object.  */
#define VALBITS 24

#define VALAMASK (((1<<VALBITS) - 1)| 0xF0000000L)

#define XTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & GCTYPEMASK))
#define XSETTYPE(a, b) ((a)  =  ((a) & VALAMASK)  +  ((int)(b) << VALBITS))

#define XPNTR(a) ((a) & VALAMASK)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + ((int) (ptr) & VALAMASK))

/* Move some garbage-collector flag bits to different bit positions.  */
#define ARRAY_MARK_FLAG (1 << 27)

#define NO_REMAP
