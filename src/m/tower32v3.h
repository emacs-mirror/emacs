/* machine description file for the NCR Tower 32 running System V.3.
   Copyright (C) 1986, 2002 Free Software Foundation, Inc.

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
   USUAL-OPSYS="usg5-3"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

/* #define NO_ARG_ARRAY */

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */
#define m68000

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

/* #define LOAD_AVE_TYPE long */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE) */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

#ifdef __GNUC__
#define alloca __builtin_alloca
#define C_OPTIMIZE_SWITCH -O -fstrength-reduce -fomit-frame-pointer
#define LIB_STANDARD -lc /lib/crtn.o
#else
/* This section is correct if you do *not* change src/ymakefile so that
   CFLAGS includes C_OPTIMIZE_SWITCH rather than C_DEBUG_SWITCH.  */
#define C_DEBUG_SWITCH -g -O0
#define LIB_STANDARD -lc -lPW /lib/crtn.o
/* This section is correct if you do enable C_OPTIMIZE_SWITCH.  */
/* #define C_OPTIMIZE_SWITCH -O2 */
/* #define LIB_STANDARD -lc /lib/crtn.o */
#endif

/* The OS maps the data section far away from the text section.  */
#define NO_REMAP
#define TEXT_START 0
#undef static
#define START_FILES pre-crt0.o /lib/crt1.o

/* The OS has an implementation of symlinks that is semantically different
   from BSD, but for some silly reason it partly has the same syntax.  */
#undef S_IFLNK

/* The OS needs stream.h+ptem.h included in sysdep.c.  */
#define NO_SIOCTL_H
#define NEED_PTEM_H
