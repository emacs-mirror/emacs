/* Machine description file for Motorola System V/88 machines
   Copyright (C) 1985, 2002 Free Software Foundation, Inc.

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
   USUAL-OPSYS="usg5-3"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

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
#ifdef USG5_4
/* rms: not needed; LIB_X11_LIB deals with this.  */
/* #define LIBX11_SYSTEM -lX11 */
#else
#undef LIB_X11_LIB /* We don't have the shared libs as assumed in usg5-3.h. */
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lnsl -lbsd
#endif /* USG5_4 */

#define BROKEN_FIONREAD

/* previously defined in usg5-4, if we choose to use that.  */
#ifndef LIBS_SYSTEM
#ifdef USG5_4
#define LIBS_SYSTEM -lsocket -lnsl 
#else
#define LIBS_SYSTEM -lbsd -lg
#endif /* USG5_4 */
#endif

#define HAVE_TERMIOS
#undef HAVE_TERMIO
#define NO_TERMIO
#undef sigsetmask

#define NO_SIOCTL_H

#ifdef USG5_4
#ifdef HAVE_X_WINDOWS
#else
#undef BSTRING
#endif /* HAVE_X_WINDOWS */
#endif /* USG5_4 */

#define NO_PTY_H

#define USE_GETOBAUD
