/* machine description file for the Motorola delta running System V.3.
   tested on sys1147 (mvme147 - based system).
   Copyright (C) 1986 Free Software Foundation, Inc.

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


/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="usg5-3"  */

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */
#define m68000
#define NO_REMAP

#define HAVE_SYSVIPC

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

   Otherwise Emacs assumes that data space precedes text space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/*#define C_ALLOCA */
/*#define HAVE_ALLOCA */

#ifdef __GNUC__
/* easy. use builtin one. also be sure that no other ones are tried out. */
# define alloca __builtin_alloca
# define HAVE_ALLOCA
# undef C_ALLOCA
#else
# ifdef C_ALLOCA
#  define STACK_DIRECTION (-1)	 /* C_ALLOCA needs to know about stack. */
# else /* C_ALLOCA */
#  ifndef HAVE_ALLOCA
#   define BAT_ALLOCA            /* if not in library, alloca.s needs this. */
#  endif /* HAVE_ALLOCA */
# endif /* C_ALLOCA */
#endif /* __GNUC__ */

/* The standard C library is -lcieee, not -lc.
   Also use the PW library, which contains alloca.
   DO NOT USE -lPW. That version of alloca is broken, at last until version
   SVR3V5.1 . -riku@field.fi */

#define LIB_STANDARD -lc

#define LIBS_TERMCAP -lcurses

/* define this if you want to use X11 */
#undef HAVE_X_WINDOWS

#ifdef HAVE_X_WINDOWS
/* debug switches enabled because of some difficulties w/X11 */
# define C_DEBUG_SWITCH -g
# define OBJECTS_MACHINE -lg
# define C_OPTIMIZE_SWITCH
# define CANNOT_DUMP
/*# define XDEBUG*/
# define X11
/* X library implements these. */
# define BSTRING
/* X library is in 'nonstandard' location. */
# define LD_SWITCH_MACHINE -L/usr/lib/X11/
#else
/* No sufficient justification for this.  */
/* # define C_DEBUG_SWITCH */
# define C_OPTIMIZE_SWITCH -O
#endif /* HAVE_X_WINDOWS */

/* enable batdevice-dependent code to compile. */
#define BAT68K

#define HAVE_SOCKETS

/* crt0.c should use the vax-bsd style of entry, with no dummy args.  */


/* emacs's magic number isn't temacs's;
   temacs is writeable text (the default!).  */
