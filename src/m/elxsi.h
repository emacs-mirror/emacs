/* machine description file for Elxsi machine (running enix).
   Copyright (C) 1986, 1992, 2002 Free Software Foundation, Inc.
   Adapted by John Salmon

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

/* This file was modified by Matt Crawford <matt@tank.uchicago.edu>
   to work under Elxsi's 12.0 release of BSD unix. */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#undef WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

/*#define NO_ARG_ARRAY */

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/*#define WORD_MACHINE */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */

#ifndef elxsi
#define elxsi
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND


/* Name of kernel load average variable */

#undef LDAV_SYMBOL
#define LDAV_SYMBOL "avenrun"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) ((x) * 100.0)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.

   Earlier versions couldn't dump.
   Changes for 12.0 release are in 19.1.
   Dumping should work now.  */

/* #define CANNOT_DUMP */

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
/*#define NO_REMAP*/

/* This is a guess for an alternate solution to whatever
   problem motivated defining _sobuf in sysdep,c with extern char *_sobuf.  */
#define _sobuf xsobuf

/* Address of start of text segment as loaded.  */

#define TEXT_START	0x800

/* Tell crt0.c not to define environ.  */

#define DONT_NEED_ENVIRON

/* The elxsi has no debugger, so might as well optimize instead
   of trying to make a symbol table.  */

#define C_DEBUG_SWITCH -O

/* Elxsi uses COFF under both Sys V and BSD environments */

#define COFF

#define ADJUST_EXEC_HEADER {\
extern int _init_brk;\
_init_brk = bss_start;\
}
