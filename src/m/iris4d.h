/* machine description file for Iris-4D machines.  Use with s/irix*.h.
   Copyright (C) 1987, 2002 Free Software Foundation, Inc.

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

#undef WORD_MACHINE

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#ifndef mips
#define mips
#endif

#ifndef IRIS_4D
#define IRIS_4D
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* jg@genmagic.genmagic.com (John Giannandrea) says this is unnecessary.  */
#if 0
/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long	/* This doesn't quite work on the 4D */

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int)(((double)(x)*100)/1024.0)

/* s-iris3-6.h uses /vmunix */

#undef KERNEL_FILE
#define KERNEL_FILE "/unix"
#endif

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#undef CANNOT_DUMP

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

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#ifdef USG5_4
#undef UNEXEC
#define UNEXEC unexelf.o
#else
#define UNEXEC unexmips.o
#endif

#define TEXT_START 0x400000

/*
 * DATA_SEG_BITS forces extra bits to be or'd in with any pointers which
 * were stored in a Lisp_Object (as Emacs uses fewer than 32 bits for
 * the value field of a LISP_OBJECT).
 */

#define DATA_START 0x10000000
#define DATA_SEG_BITS	0x10000000

#undef LIBS_MACHINE
/* -lsun in case using Yellow Pages for passwords.  */
#if defined(__GNUC__) && defined(_ABIN32)
#define LIBS_MACHINE
#else
#ifndef IRIX6_5
#define LIBS_MACHINE -lmld
#else
#define LIBS_MACHINE
#endif
#endif
#define LIBS_DEBUG

/* Define this if you have a fairly recent system,
   in which crt1.o and crt1.n should be used.  */
#define HAVE_CRTN

#ifndef USG5_4
#ifdef HAVE_CRTN
/* Must define START-FILES so that the linker can find /usr/lib/crt0.o.  */
#define START_FILES pre-crt0.o /usr/lib/crt1.o
#define LIB_STANDARD -lc /usr/lib/crtn.o
#else
#define START_FILES pre-crt0.o /usr/lib/crt0.o
/* The entry-point label (start of text segment) is `start', not `__start'.  */
#define DEFAULT_ENTRY_ADDRESS start
#define LIB_STANDARD -lc
#endif
#endif

/* Use terminfo instead of termcap.  */

#define TERMINFO

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'q'

#ifndef _LP64
/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short.  */

#define XUINT(a) (((unsigned)(a) << BITS_PER_INT-VALBITS) >> BITS_PER_INT-VALBITS)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + (((unsigned) (ptr) << BITS_PER_INT-VALBITS) >> BITS_PER_INT-VALBITS))

#define XUNMARK(a) ((a) = (((unsigned)(a) << BITS_PER_INT-GCTYPEBITS-VALBITS) >> BITS_PER_INT-GCTYPEBITS-VALBITS))
#endif /* _LP64 */

#ifndef __GNUC__
/* Turn off some "helpful" error checks for type mismatches
   that we can't fix without breaking other machines.  */
#ifdef IRIX_FORCE_32_BITS
#ifdef THIS_IS_MAKEFILE
#ifndef IRIX6_5
#define C_SWITCH_MACHINE -32
#else
#define C_SWITCH_MACHINE -n32
#endif
#endif
#endif

#endif /* not __GNUC__ */
