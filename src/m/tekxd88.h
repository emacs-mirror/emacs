/* m- file for Tektronix XD88 running UTekV 3.2e to be used with s-usg5-3.h,
   contributed by Kaveh Ghazi  (ghazi@caip.rutgers.edu)  1/15/93.
   You probably need to use gnu make (version 3.63 or higher.)
   Copyright (C) 1993 Free Software Foundation, Inc.

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
/* No load average on XD88 machines. */
/* #define LOAD_AVE_TYPE double */

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* #define LOAD_AVE_CVT(x) ((int) ((x) * 100.0)) */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */
#define CANNOT_DUMP	/* oh well, maybe someday ... */

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
#ifdef __GNUC__
#  define alloca __builtin_alloca	/* Use the gcc builtin alloca() ... */
#  define HAVE_ALLOCA	/* ... and be sure that no other ones are tried out. */
#  undef C_ALLOCA
#  define C_OPTIMIZE_SWITCH -O2
#else /* not __GNUC__ */
#  undef HAVE_ALLOCA
#  define C_ALLOCA	/* Use the alloca() supplied in alloca.c. */
#  define STACK_DIRECTION -1  /* The stack grows towards lower addresses. */
#  define C_OPTIMIZE_SWITCH -O
#endif /* __GNUC__ */

/*#define C_DEBUG_SWITCH C_OPTIMIZE_SWITCH*/  /* Uncomment this to optimize */

/* XD88 SysV has PTYs.  Not all usg3-5 systems do, so this is defined here. */
#define HAVE_PTYS 
#define SYSV_PTYS	/* Requires <termios.h> */

/* we have job control */
#undef NOMULTIPLEJOBS

/*
 * sockets are available
 */
#define HAVE_SOCKETS

/* 
 * we have Berkeley style <sys/time.h>
 */
#define HAVE_TIMEVAL

/* XD88 has select(). */
#define HAVE_SELECT
#define BROKEN_FIONREAD  /* is this needed ? */

/*
 * don't use utimes, we ain't got one - use utime() instead
 */
#define USE_UTIME

#define NO_SIOCTL_H

/* We need HAVE_TCATTR to prevent Ctrl-Z from suspending Emacs before
   suspend-emacs has been called. */
/*#define HAVE_TCATTR*/
/* TCATTR gives bogus baud rates.  Use the following for OSPEED instead. */
/*#define OSPEED(str) (cfgetospeed(&(str)))*/
#define HAVE_TERMIOS
#undef HAVE_TERMIO

#define BSTRING /* its in libc but not declared in any <*.h> file. */
#define HAVE_TZSET
#define HAVE_SETSID
#define HAVE_RENAME

#ifdef ghs	/* Stands for "Green Hills Software", defined in /bin/cc */ 
/* Only required for use with the Green Hills compiler:
	-X18 Do not allocate programmer-defined local variables to a
	     register unless they are declared register.  (From building
	     perl-4.036 Green Hills hints.  Might be needed for setjmp.)
	*/
#define C_SWITCH_MACHINE -X18
/* We need /lib/default.ld so the bundled ld can read its link directives. */
#define LD_SWITCH_SYSTEM /lib/default.ld
#endif /* ghs */

/* XD88 does not have the random() and srandom() calls in the base system,
   but they exist in libX11.a.  So, if you are building with X11 then you
   will need to define HAVE_RANDOM. */
#ifdef HAVE_X_WINDOWS
#define HAVE_RANDOM
#undef LIB_X11_LIB  /* don't use the shared library default from usg5-3.h */
#undef LIBX11_SYSTEM
#endif /* HAVE_X_WINDOWS */

/*#define SYSTEM_MALLOC*/

#ifndef UTEKV
#define UTEKV  /* system specific symbol */
#endif /* !UTEKV */

/* stuff to hopefully someday get dumping working ... */
/*#define SECTION_ALIGNMENT 0x1ff*/
/*#define SEGMENT_MASK 0xff*/
/*#define A_TEXT_OFFSET(HDR) sizeof(HDR)*/
