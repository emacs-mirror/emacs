/* Machine-dependent configuration for GNU Emacs for AT&T 3b machines.
   Copyright (C) 1986 Free Software Foundation, Inc.

   Modified by David Robinson (daver@csvax.caltech.edu) 6/6/86

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
   USUAL-OPSYS="usg5-2-2"  */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16            /* Number of bits in a short */

#define INTBITS 32              /* Number of bits in an int */

#define LONGBITS 32             /* Number of bits in a long */

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
   does not define it automatically */
#define ATT3B

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

/* #define VIRT_ADDR_VARIES */  /* Karl Kleinpaste says this isn't needed.  */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* SysV has alloca in the PW library */

#define LIB_STANDARD -lPW -lc
#define HAVE_ALLOCA

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* #define LD_SWITCH_MACHINE -N */

/* Use Terminfo, not Termcap.  */

#define TERMINFO

/* -O has been observed to make correct C code in Emacs not work.
   So don't try to use it.  */

#if u3b2 || u3b5 || u3b15
#define C_OPTIMIZE_SWITCH
#endif

/* Define our page size.  */

#define NBPC 2048

/* The usual definition of XINT, which involves shifting, does not
   sign-extend properly on this machine.  */

#define XINT(i) (((sign_extend_temp=(i)) & 0x00800000) \
		 ? (sign_extend_temp | 0xFF000000) \
		 : (sign_extend_temp & 0x00FFFFFF))

#ifdef emacs /* Don't do this when making xmakefile! */
extern int sign_extend_temp;
#endif

#if u3b2 || u3b5 || u3b15

/* On 3b2/5/15, data space has high order bit on. */
#define VALBITS 27
#define VALMASK (((1<<VALBITS) - 1) | (1 << 31))
#define XTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & GCTYPEMASK))

#endif /* 3b2, 3b5 or 3b15 */

#define TEXT_START 0


/* For alloca.c (not actually used, since HAVE_ALLOCA) */
#define STACK_DIRECTION 1

/* (short) negative-int doesn't sign-extend correctly */
#define SHORT_CAST_BUG

/* 3B2s with WIN/3B have winsize defined in ptem.h */
#if u3b2
#define NEED_PTEM_H
#endif /* u3b2 */

/* 3b2 does not have memmove, I'm told.  */
/* It is safe to have no parens around the args in the safe_bcopy call,
   and parens would screw up the prototype decl for memmove.  */
#define	memmove(d, s, n) safe_bcopy (s, d, n)

/* This affects filemode.c.  */
#define NO_MODE_T
