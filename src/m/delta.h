/* Machine description file for the Motorola Delta.
   Tested on mvme147 board using R3V7 without X.  Tested with gcc.
   Tested on mvme167 board using R3V7 without X.  Tested with cc, gnucc, gcc.
   Copyright (C) 1986, 1993, 1994 Free Software Foundation, Inc.

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
   USUAL-OPSYS="usg5-3"  */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

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
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#define m68000
#define MOTOROLA_DELTA

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

   Otherwise Emacs assumes that data space precedes text space,
   numerically.  */

/* #define VIRT_ADDR_VARIES */

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* #define C_ALLOCA */
/* #define HAVE_ALLOCA */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Some really obscure 4.2-based systems (like Sequent DYNIX)
 * do not support asynchronous I/O (using SIGIO) on sockets,
 * even though it works fine on tty's.  If you have one of
 * these systems, define the following, and then use it in
 * config.h (or elsewhere) to decide when (not) to use SIGIO.
 *
 * You'd think this would go in an operating-system description file,
 * but since it only occurs on some, but not all, BSD systems, the
 * reasonable place to select for it is in the machine description
 * file.
 */

/* #define NO_SOCK_SIGIO */


/* Undefine this if you don't want the machine slow down when a buffer
   is modified. */

#define CLASH_DETECTION

/* Machine specific stuff */
#define HAVE_PTYS
#define SYSV_PTYS
#ifdef HAVE_INET_SOCKETS	/* this comes from autoconf  */
# define HAVE_SOCKETS		/* NSE may or may not have been installed */
#endif
#define SIGNALS_VIA_CHARACTERS
#define BROKEN_CLOSEDIR		/* builtin closedir is interruptible */
#undef HAVE_BCOPY		/* b* functions are just stubs to mem* ones */
#define bcopy(from,to,bytes)	memcpy(to,from,bytes)
#define bzero(to,bytes)		memset(to,0,bytes)
#define bcmp memcmp
#define memmove(t,f,s) safe_bcopy(f,t,s) /* for overlapping copies */
#undef KERNEL_FILE
#define KERNEL_FILE "/sysv68"
#undef LDAV_SYMBOL
#ifdef SIGIO
 /* R3V7 has SIGIO, but interrupt input does not work yet.
    Let's go on with cbreak code. */
/* # define INTERRUPT_INPUT */
#endif

/* The standard C library is -lc881, not -lc.
   -lbsd brings sigblock and sigsetmask.
   DO NOT USE -lPW. That version of alloca is broken in versions R3V5,
   R3V6, R3V7. -riku@field.fi -pot@cnuce.cnr.it. */

#define LIB_STANDARD -lc881
#define LIB_MATH -lm881
#define LIBS_TERMCAP -lcurses
#define LIBS_SYSTEM -lbsd
#undef sigsetmask

#ifdef HAVE_X_WINDOWS
/* I have not tested X, but I think these are obsolete, so let's
   commment them -pot@cnuce.cnr.it */
/* debug switches enabled because of some difficulties w/X11
# define C_DEBUG_SWITCH -g
# define OBJECTS_MACHINE -lg
# define C_OPTIMIZE_SWITCH
# define CANNOT_DUMP
# define XDEBUG */
/* X library is in 'nonstandard' location. */
/* This should be taken care of by configure -pot@cnuce.cnr.it
# define LD_SWITCH_MACHINE -L/usr/lib/X11/ */
# define HAVE_RANDOM
# define BROKEN_FIONREAD	/* pearce@ll.mit.edu says this is needed. */
# define HAVE_XSCREENNUMBEROFSCREEN
# undef LIB_X11_LIB		/* no shared libraries */
# define LIB_X11_LIB -lX11
# undef USG_SHARED_LIBRARIES    /* once again, no shared libs */
# undef LIBX11_SYSTEM		/* no -lpt as usg5-3.h expects */
# define LIBX11_SYSTEM -lnls -lnsl_s
#endif /* HAVE_X_WINDOWS */

#ifdef __GNUC__
 /* Use builtin alloca. Also be sure that no other ones are tried out. */
# define alloca __builtin_alloca
# define HAVE_ALLOCA
 /* Union lisp objects do not yet work as of 19.15. */
/* # undef NO_UNION_TYPE */

/* There are three ways to use the gnucc provided with R3V7.  Either
   link /bin/ccd/cc to /bin/cc and then configure (supposing that CC
   is unset or set to cc).  Or configure like this: `CC=/bin/ccd/cc
   configure', or else configure like this: `CC=gnucc configure'. */

# ifdef __STDC__
 /* Compiling with gnucc (not through ccd).  This means -traditional is
    not set.  Let us set it, because gmalloc.c includes <stddef.h>,
    and we don't have that (as of SYSV68 R3V7). */
#  define C_SWITCH_MACHINE -mfp0ret -m68881 -traditional -Dconst= -fdelayed-branch -fstrength-reduce -fno-inline -fcaller-saves
#  define LIB_GCC /lib/gnulib881
# endif /* __STDC__ */

#else
 /* Not __GNUC__, use the alloca in alloca.s. */

 /* Try to guess if we are using the Green Hills Compiler */
# if defined mc68000 && defined MC68000
 /* Required only for use with Green Hills compiler:
	-ga	 Because alloca relies on stack frames. This option forces
		 the Green Hills compiler to create stack frames even for
		 functions with few local variables. */
#  define C_SWITCH_MACHINE -ga -O
#  define GAP_USE_BCOPY		/* *++to = *++from  is inefficient */
#  define BCOPY_UPWARD_SAFE 0
#  define BCOPY_DOWNWARD_SAFE 1	/* bcopy does: mov.b (%a1)+,(%a0)+ */
# else
 /* We are using the standard AT&T Portable C Compiler */
#  define SWITCH_ENUM_BUG
# endif

#endif /* not __GNUC__ */
