/* machine description file for the IA-64 architecture.
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.
     Contributed by David Mosberger <davidm@hpl.hp.com>

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define BITS_PER_LONG		64
#define BITS_PER_EMACS_INT	64

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#undef WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
   group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
   to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

/* __ia64__ defined automatically */


/* Use type EMACS_INT rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define the type to use.  */
#define EMACS_INT		long
#define EMACS_UINT		unsigned long
#define SPECIAL_EMACS_INT

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE		long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */
#if 0
#define CANNOT_DUMP
#endif

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

/* Define the following if GNU malloc and the relocating allocator do
   not work together with X.  */

/* #define SYSTEM_MALLOC */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

/* #define NO_REMAP */

/* Some really obscure 4.2-based systems (like Sequent DYNIX) do not
   support asynchronous I/O (using SIGIO) on sockets, even though it
   works fine on tty's.  If you have one of these systems, define the
   following, and then use it in config.h (or elsewhere) to decide
   when (not) to use SIGIO.

   You'd think this would go in an operating-system description file,
   but since it only occurs on some, but not all, BSD systems, the
   reasonable place to select for it is in the machine description
   file.  */

/* #define NO_SOCK_SIGIO */

#ifdef __ELF__
#undef UNEXEC
#define UNEXEC unexelf.o
#endif

#define PNTR_COMPARISON_TYPE unsigned long

/* On the 64 bit architecture, we can use 60 bits for addresses */

#define VALBITS         60

/* This definition of MARKBIT is necessary because of the comparison of
   ARRAY_MARK_FLAG and MARKBIT in an #if in lisp.h, which cpp doesn't like. */

#define MARKBIT         0x8000000000000000L

/* Define XINT and XUINT so that they can take arguments of type int */

#define XINT(a)  (((long) (a) << (BITS_PER_LONG - VALBITS)) >> (BITS_PER_LONG - VALBITS))
#define XUINT(a) ((long) (a) & VALMASK)

/* Declare malloc and realloc in a way that is clean.
   But not in makefiles!  */

#ifndef NOT_C_CODE
/* We need these because pointers are larger than the default ints.  */
# if !defined(__NetBSD__) && !defined(__OpenBSD__)
#  include <alloca.h>
# else
#  include <stdlib.h>
# endif

/* We need to prototype these for the lib-src programs even if we don't
   use the system malloc for the Emacs proper.  */
#ifdef _MALLOC_INTERNAL
/* These declarations are designed to match the ones in gmalloc.c.  */
#if defined (__STDC__) && __STDC__
extern void *malloc (), *realloc (), *calloc ();
#else
extern char *malloc (), *realloc (), *calloc ();
#endif
#else /* not _MALLOC_INTERNAL */
extern void *malloc (), *realloc (), *calloc ();
#endif /* not _MALLOC_INTERNAL */

#ifdef REL_ALLOC
#ifndef _MALLOC_INTERNAL
/* "char *" because ralloc.c defines it that way.  gmalloc.c thinks it
   is allowed to prototype these as "void *" so we don't prototype in
   that case.  You're right: it stinks!  */
extern char *r_alloc (), *r_re_alloc ();
extern void r_alloc_free ();
#endif /* not _MALLOC_INTERNAL */
#endif /* REL_ALLOC */

#endif /* not NOT_C_CODE */

#define DATA_SEG_BITS	0x6000000000000000

#define HAVE_TEXT_START
