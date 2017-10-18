/* NAME:
 *      m-dpx2.h - machine description for Bull DPX/2 range
 *
 * SYNOPSIS:
 *      #include "s-usg5-3.h"
 *      #include "m-dpx2.h"
 *
 * DESCRIPTION:
 *      This header is part of GNU Emacs, and therefore 
 *      falls under the FSF GPL.  There should be a copy of it 
 *      somewhere on your system (see below).
 *      
 *      This header file, and a _minor_ change to 
 *      config.h-dist are all that are needed to build GNU 
 *      Emacs-18.58 on a Bull DPX/2 model 200 or 300.
 *      The DPX/2 500 should probably use a mips based config.
 *
 *      The only change _needed_ to config.h (copied from 
 *      config.h-dist) is to define the appropriate ncl_{el,mr} 
 *      pre-processor constant.  Hopefully in a future release 
 *      the C compiler will define this automagically :-)
 *      
 *	If you have INET and X11 loaded, define HAVE_X_WINDOWS
 *	before you include this file.  If you have INET but not
 *	X11 loaded define HAVE_SOCKETS in config.h before you
 *	include this file.
 *
 *      If you make changes to this file, please be sure to send 
 *      an update to me:  <sjg@melb.bull.oz.au>
 *      
 *      Please read through this file before attempting to build 
 *      Emacs.  You may need to set SRC_COMPAT=_SYSV in your
 *	environment or in Makefile. 
 *
 *
 * AMENDED:
 *      92/02/06  12:53:06  (sjg)
 *
 * RELEASED:
 *      92/02/06  12:53:07  v1.7
 *
 * BUGS:
 *	There is a problem when running Emacs with its own X
 *	window in that if the invoking process terminates,
 *	Emacs will terminate shortly afterwards.  The following
 *	exercises the bug:
 *		$ ksh
 *		$ emacs &
 *		$ exit
 *	The emacs process will die within a few minutes of the
 *	invoking ksh's termination.  Solution?  Don't do the
 *	above :-)
 *
 * SCCSID:
 *      @(#)m-dpx2.h  1.7  92/02/06  12:53:06  (sjg)
 */

/* 
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

/*
 * You need to either un-comment one of these lines, or copy one 
 * of them to config.h before you include this file.
 * Note that some simply define a constant and others set a value.
 */

/* #define ncl_el	/* DPX/2 210,220 etc */
/* #define ncl_mr 1	/* DPX/2 320,340 (and 360,380 ?) */


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

/* #define WORD_MACHINE /**/

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

/* /bin/cc on ncl_el and ncl_mr define m68k and mc68000 */

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define FSCALE 1000.0
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP /**/

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/* #define VIRT_ADDR_VARIES /**/

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
/* #define HAVE_ALLOCA /**/

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/*
 * end of the standard macro's
 */

/*
 * a neat identifier to handle source mods (if needed)
 */
#define DPX2

/*
 * if we use X11, libX11.a has these...
 */
#ifdef HAVE_X_WINDOWS
# undef LIBX11_SYSTEM
# define LIBX11_SYSTEM -lnsl
# define BSTRING
# define HAVE_GETWD
# define HAVE_GETTIMEOFDAY
/*
 * Bull's X11R3 and X11R4 libs contain random and friends.
 * The MIT X11R5 lib does not.
 */
# define HAVE_RANDOM
/*
 * we must have INET loaded so we have sockets
 */
# define HAVE_SOCKETS
#endif /* HAVE_X_WINDOWS */

/*
 * useful if you have INET loaded
 */
#ifdef HAVE_SOCKETS
# define LIBS_MACHINE -linet
#endif


#if (defined(ncl_mr) || defined(ncl_el)) && !defined (NBPC)
# define NBPC 4096
#endif

/*
 * if SIGIO is defined, much of the emacs
 * code assumes we are BSD !!
 */
#ifdef SIGIO
# undef SIGIO
#endif

/*
 * a good idea on multi-user systems :-)
 */
#define CLASH_DETECTION		/* probably a good idea */
#define NO_FCHMOD  /* Use chmod instead.  */


#ifdef SIGTSTP
/*
 * sysdep.c(sys_suspend) works fine with emacs-18.58
 * and BOS 02.00.45, if you have an earler version
 * of Emacs and/or BOS, or have problems, or just prefer
 * to start a sub-shell rather than suspend-emacs,
 * un-comment out the next line.
 */
/* # undef SIGTSTP /* make suspend-emacs spawn a sub-shell */
# ifdef NOMULTIPLEJOBS
#   undef NOMULTIPLEJOBS
# endif
#endif
/*
 * no we don't want this at all
 */
#ifdef USG_JOBCTRL
# undef USG_JOBCTRL
#endif


/* 
 * X support _needs_ this
 */
#define HAVE_SELECT
/*
 * and select requires these
 */
#define HAVE_TIMEVAL
#define USE_UTIME

/* select also needs this header file--but not in ymakefile.  */
#ifndef NO_SHORTNAMES
#include <sys/types.h>
#include <sys/select.h>
#endif

#define TEXT_START 0
/* 
 * Define the direction of stack growth. 
 */
#define STACK_DIRECTION -1

/*
 * define _SYSV
 * this is needed by a lot of the B.O.S include files so that 
 * what emacs wants is really included.
 * Causes the POSIX stuff to be skipped.
 */
#ifndef _SYSV
# define _SYSV
#endif

/* end of m-dpx2.h */
