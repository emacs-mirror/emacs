/* machine description file for WICAT machines.
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
   USUAL-OPSYS="usg5-2"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#undef NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#undef WORD_MACHINE

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000 are the ones defined so far.  */

#ifndef m68000
#define m68000
#endif

/* This flag is used only in alloca.s.  */
#define WICAT

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#undef LOAD_AVE_TYPE

/* Convert that into an integer that is 100 for a load average of 1.0  */

#undef LOAD_AVE_CVT

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

#undef VIRT_ADDR_VARIES

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#undef NO_REMAP

/* For WICAT, define TAHOE_REGISTER_BUG if you have a pre-4.2 C compiler */

#define TAHOE_REGISTER_BUG

/* pagesize definition */

#define EXEC_PAGESIZE	0x1000

/* Delete this for WICAT sys V releases before 2.0.  */

#define	LIB_STANDARD -lc-nofp

/* Special magic number */

#define EXEC_MAGIC	MC68ROMAGIC

/* Special switches to give to ld.  */

#define LD_SWITCH_MACHINE -e __start -N

/* Sigh...cannot define this for WICAT cuz 0 length memcpy blows chunks */

#undef BSTRING

#ifdef BSTRING
#undef bcopy
#undef bzero
#undef bcmp

#define bcopy(a,b,s)	memcpy(b,a,s)
#define bzero(a,s)	memset(a,0,s)
#define bcmp		memcmp
#endif

/*
 * Define optimflags if you want to optimize.
 *	- Set to null string for pre-4.2 C compiler
 *	- Set to "-O -Wopt,-O-f" for 4.2
 */

#define C_OPTIMIZE_SWITCH /* -O -Wopt,-O-f */

/* For WICAT version supporting PTYs and select (currently internal only) */

#ifdef HAVE_PTYS
#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'q'
#endif

/* there is a select() in libcurses.a that causes a conflict so use termlib */
#ifdef HAVE_SELECT
#undef TERMINFO
#define LIBS_TERMCAP select.o -ltermlib
#endif
