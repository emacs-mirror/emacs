/* machine description file for Hitachi SR2001/SR2201 machines.
   Copyright (C) 1996, 2002 Free Software Foundation, Inc.

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
   USUAL-OPSYS="hpux"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#undef WORD_MACHINE

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef hp9000s800
#	define hp9000s800
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short. */
   

#define XUINT(a) (((unsigned)(a) << BITS_PER_INT-VALBITS) >> BITS_PER_INT-VALBITS)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + (((unsigned) (ptr) << BITS_PER_INT-VALBITS) >> BITS_PER_INT-VALBITS))

#define XMARKBIT(a) ((a) < 0)
#define XSETMARKBIT(a,b) ((a) = ((b) ? (a)|MARKBIT : (a) & ~MARKBIT))

#if 0  /* Loses when sign bit of type field is set.  */
#define XUNMARK(a) ((a) = (((a) << BITS_PER_INT-GCTYPEBITS-VALBITS) >> BITS_PER_INT-GCTYPEBITS-VALBITS))
#endif

/* Define the BSTRING functions in terms of the sysV functions. */
/* On HPUX 8.05, including types.h can include strings.h
   which declares these as functions.  Hence the #ifndef.  */

#ifndef HAVE_BCOPY
#define bcopy(a,b,s)	memcpy (b,a,s)
#define bzero(a,s)	memset (a,0,s)
#define bcmp		memcmp
#endif

/* #ifdef __hpux */
/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#ifndef hp9000s800
#     define hp9000s800
#endif


/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) (x * 100.0))


/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

#define VIRT_ADDR_VARIES

/* the data segment on this machine always starts at address 0x40000000. */

#define DATA_SEG_BITS 0x40000000

#define DATA_START    0x40000000
#define TEXT_START    0x00000000

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#define UNEXEC unexhp9k800.o

#define LIBS_MACHINE
#define LIBS_DEBUG

/* Include the file bsdtty.h, since this machine has job control.  */
/* #define NEED_BSDTTY */

/* The symbol in the kernel where the load average is found
   is named _avenrun.  At this time there are two major flavors
   of hp-ux (there is the s800 and s300 (s200) flavors).  The
   differences are thusly moved to the corresponding machine description file.
*/

/* no underscore please */
#define LDAV_SYMBOL "avenrun"

#if 0   /* Supposedly no longer true.  */
/* In hpux, for unknown reasons, S_IFLNK is defined even though
   symbolic links do not exist.
   Make sure our conditionals based on S_IFLNK are not confused.

   Here we assume that stat.h is included before config.h
   so that we can override it here.  */

#undef S_IFLNK
#endif

/* On USG systems these have different names. */

#define index strchr
#define rindex strrchr

/* #endif */
