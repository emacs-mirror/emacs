/* machine description for Bull DPX/2 range 
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE /**/

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

/*#define CANNOT_DUMP /**/

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
#ifndef DPX2
#define DPX2
#endif

/* Disable support for shared libraries in unexec.  */

#undef USG_SHARED_LIBRARIES

/*
 * if we use X11, libX11.a has these...
 */
# undef LIB_X11_LIB
# define LIB_X11_LIB -lX11
# undef LIBX11_SYSTEM
# define LIBX11_SYSTEM -lmalloc -lnsl
# define BSTRING
# define HAVE_GETWD

/*
 * we must have INET loaded so we have sockets
 */
# define HAVE_SOCKETS

/*
 * useful if you have INET loaded
 */
# define LIBS_MACHINE -linet


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
 * but we have that
*/
#define GETPGRP_NO_ARG

/* select also needs this header file--but not in ymakefile.  */
#ifndef NOT_C_CODE
#include <sys/types.h>
#include <sys/select.h>
#endif

#define TEXT_START 0

/* 
 * Define the direction of stack growth. 
 */

#define STACK_DIRECTION -1

/* we have termios */
#undef HAVE_TERMIO
#define HAVE_TERMIOS
#define HAVE_TCATTR

/* we also have this */
#define HAVE_PTYS
#define SYSV_PTYS

/* It doesn't seem we have sigpause */
#undef HAVE_SYSV_SIGPAUSE

#define POSIX_SIGNALS

/* We don't need the definition from usg5-3.h with POSIX_SIGNALS.  */
#undef sigsetmask


/* on bos2.00.45 there is a bug that makes the F_SETOWN fcntl() call
   enters in an infinite loop. Avoid calling it  */
#define F_SETOWN_BUG

/* system closedir sometimes complains about wrong descriptor
   for no apparent reasons. Use the provided closedir in sysdep.c instead */
#ifdef HAVE_CLOSEDIR
#undef HAVE_CLOSEDIR
#endif

/* Send signals to subprocesses by "typing" signal chars at them.  */
#define SIGNALS_VIA_CHARACTERS

/* This is to prevent memory clobbering on the DPX/2 200.  */
#define LD_SWITCH_MACHINE -N -T32

 /* end of dpx2.h */


