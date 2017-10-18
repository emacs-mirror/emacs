/* m- file for the Motorola delta running System V.3.
   tested on sysV68 (mvme147 - based system).  Use with s-usg5-3.h.
   Copyright (C) 1986 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define mot_delta

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

/* #define NO_ARG_ARRAY */

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE */

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */
#define m68000
#define mot_delta

/* Don't try to dump part of data space as pure.  */

#define NO_REMAP

/* We have system V ipc.  */

#define HAVE_SYSVIPC

/* We have ptys.  */

#define HAVE_PTYS
#define SYSV_PTYS

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE 
#define SWITCH_ENUM_BUG
/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
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

#define USG_SYS_TIME
#define SYSV_SYSTEM_DIR
#define BSTRING
#define CLASH_DETECTION		/* starting from emacs 18.59 */

#undef KERNEL_FILE
#define KERNEL_FILE "/sysV68"
#undef LOAD_AVE_TYPE
#define HAVE_DUP2

/* -lbsd gets sigblock and sigsetmask. */
#define LIBS_SYSTEM -lbsd
#undef sigsetmask

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#ifdef __GNUC__
/* easy. use builtin one. also be sure that no other ones are tried out. */
# define alloca __builtin_alloca
# define HAVE_ALLOCA
# undef C_ALLOCA
#else /* not __GNUC__, use the one in alloca.s. */
/* the alloca in -lPW is broken, at least until R3V6
 -riku@field.fi
 -pot@cnuce.cnr.IT */
#undef HAVE_ALLOCA
#undef C_ALLOCA
#endif /* __GNUC__ */

#define LIBS_TERMCAP -lcurses

#define HAVE_TIMEVAL
#define HAVE_GETTIMEOFDAY

#define HAVE_SELECT
#define HAVE_SOCKETS		/***** only if NSE has been installed *****/

#define USE_UTIME

/* Required only for use with Green Hills compiler:
	-ga because alloca relies on stack frames. This option forces
	    the Green Hills compiler to create stack frames even for
	    functions with few local variables. */

/* #define C_SWITCH_MACHINE -ga */

/* People used to say this was necessary.  Maybe it no longer is,
   because the system may not define FIONREAD.  But this can't hurt.  */
#define BROKEN_FIONREAD

/* Send signals to subprocesses by typing interrupt characters at them.  */
#define SIGNALS_VIA_CHARACTERS
