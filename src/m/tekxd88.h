/* Configuration file for the Tektronix XD88 running UTekV 3.2e,
   contributed by Kaveh Ghazi  (ghazi@caip.rutgers.edu)  1/15/93.
   You probably need to use gnu make (version 3.63 or higher.)
   Copyright (C) 1993, 2002 Free Software Foundation, Inc.

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


/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */
#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */
/* #define WORD_MACHINE */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef m88000     /* Some 88k C compilers already define this */
#define m88000
#endif

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */
#define SIGN_EXTEND_CHAR(c) (c)

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */
#define NO_UNION_TYPE 

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */
/* #define EXPLICIT_SIGN_EXTEND */

/* Data type of load average, as read out of kmem.  */
/* #define LOAD_AVE_TYPE double */	/* No load average on XD88. */
/* Convert that into an integer that is 100 for a load average of 1.0  */
/* #define LOAD_AVE_CVT(x) ((int) ((x) * 100.0)) */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */
/*#define CANNOT_DUMP*/

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */
/* #define VIRT_ADDR_VARIES */ 

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */
#define NO_REMAP

#ifdef __GNUC__
#  define alloca __builtin_alloca	/* Use the gcc builtin alloca() ... */
#else /* not __GNUC__ */
#  define C_OPTIMIZE_SWITCH -O
#endif /* __GNUC__ */

#undef NOMULTIPLEJOBS	/* we have job control */
#define HAVE_SOCKETS	/* sockets are available */
#define BROKEN_FIONREAD /* is this needed ? */
#define BSTRING		/* its in libc but not declared in any header file. */
#undef sigsetmask	/* XD88 has sigsetmask() */

#undef LIB_X11_LIB	/* Don't use shared libraries defined in usg5-3.h */
#undef LIBX11_SYSTEM

#define HAVE_TERMIOS	/* We have termios. */
#undef HAVE_TERMIO	/* Make sure termios ifdef code is used, not termio. */
#define NO_TERMIO	/* Don't include both termios.h and termio.h */
#define HAVE_PTYS	/* XD88 SysV has PTYs. */
#define SYSV_PTYS	/* Requires <termios.h> */

#ifdef ghs  /* Stands for "Green Hills Software", defined only in /bin/cc */ 
/* -X18 means do not allocate programmer-defined local variables to a
   register unless they are declared register.  (Copied from perl-4.036
   Green Hills C hints file.  Might be needed for setjmp, I don't know.) */
#  define C_SWITCH_MACHINE -X18
/* We need /lib/default.ld so that /bin/ld can read its link directives. */
#  define LD_SWITCH_SYSTEM /lib/default.ld
#endif /* ghs */

/* We need this to get dumping to work */
#define KEEP_OLD_TEXT_SCNPTR
