/* amdahl machine description file 
   Copyright (C) 1987, 1999, 2002 Free Software Foundation, Inc.

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

/*
This file for amdahl_uts created by modifying the template.h
by Jishnu Mukerji 3/1/87

The following line tells the configuration script what sort of
operating system this machine is likely to run.
USUAL-OPSYS="usg5-2-2"

This file works with the Amdahl uts native C compiler. The 5.2u370
compiler is so brain damaged that it is not even worth trying to use it.
*/

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#undef NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#define WORD_MACHINE /* not actually used anywhere yet! */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */

/* uts gets defined automatically */
/* However for clarity define amdahl_uts */
#define amdahl_uts

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

/* #define LOAD_AVE_TYPE long*/

/* Convert that into an integer that is 100 for a load average of 1.0  */

/*#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0)*/

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES*/

#ifdef HAVE_ALLOCA
#define LIB_STANDARD -lPW -lc
#endif

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

/*#define NO_REMAP*/

#define TERMINFO

/* The usual definition of XINT, which involves shifting, does not
   sign-extend properly on this machine.  */

#define XINT(i) (((sign_extend_temp=(i)) & 0x00800000) \
		 ? (sign_extend_temp | 0xFF000000) \
		 : (sign_extend_temp & 0x00FFFFFF))

#ifdef emacs /* Don't do this when making xmakefile! */
extern int sign_extend_temp;
#endif

/* The following needed to load the proper crt0.o and to get the
   proper declaration of data_start in the #undef NO_REMAP case */

#ifndef NO_REMAP
#define START_FILES pre-crt0.o /lib/crt0.o
#endif

/* Perhaps this means that the optimizer isn't safe to use.  */

#define C_OPTIMIZE_SWITCH

/* Put text and data on non-segment boundary; makes image smaller */

#define LD_SWITCH_MACHINE	-N 

/* When writing the 'xemacs' file, make text segment ro */
#define EXEC_MAGIC	0410

/* Mask for address bits within a memory segment */
#define SEGSIZ 0x10000		/* Should this not be defined elsewhere ? */
#define SEGMENT_MASK (SEGSIZ - 1)

/* Compensate for error in signal.h.  */
#define NSIG_MINIMUM 20
