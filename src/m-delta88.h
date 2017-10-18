/* m- file for Motorola System V/88 machines
   Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */


/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE 

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

/* #define EXPLICIT_SIGN_EXTEND */

/* Data type of load average, as read out of kmem.  */
/* No load average on Motorola machines. */
/* #define LOAD_AVE_TYPE double */

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* #define LOAD_AVE_CVT(x) ((int) ((x) * 100.0)) */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP  */

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

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* BEM:  Distributed asm alloca doesn't work.  Don't know about libPW.a.
   C ALLOCA is safe and fast enough for now. */

#define C_ALLOCA
#define	STACK_DIRECTION	-1  /* grows towards lower addresses. */

/* Motorola SysV has PTYs.  Not all usg3-5 systems do, so this is defined
   here. */

#define HAVE_PTYS 
#define SYSV_PTYS

/* Ditto for IPC. */


/* 
 * we now have job control in R32V1
 */
#undef NOMULTIPLEJOBS

/*
 * we have bcopy, bzero, bcmp in libc.a (what isn't in libc.a?)
 */
#define BSTRING

/*
 * sockets are in R32V1
 */
#define HAVE_SOCKETS

/*
 * we have the wrong name for networking libs
 */
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lnsl -lbsd

/* 
 * we have Berkeley style <sys/time.h>
 */
#define HAVE_TIMEVAL
#define HAVE_GETTIMEOFDAY

/* SysV88 has select(). */
#define HAVE_SELECT
#define BROKEN_FIONREAD

/*
 * don't use utimes, we ain't got one - use utime() instead
 */
#define USE_UTIME

#define LIBS_SYSTEM -lbsd -lg

#define NEED_TERMIOS

#define NO_SIOCTL_H
