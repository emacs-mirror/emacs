/* machine description file for ibm ps/2 aix386.
   Copyright (C) 1989, 2002 Free Software Foundation, Inc.

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
   USUAL-OPSYS="note"

NOTE-START
  Use -opsystem=usg5-3 on AIX 1.2.
  -opsystem=usg5-2-2 should work on either AIX 1.1 or 1.2, but may not
  work with certain new X window managers, and may be suboptimal.
NOTE-END */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#undef WORDS_BIG_ENDIAN

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

#define INTEL386
#define aix386

#define IBMAIX

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* crt0.c, if it is used, should use the i386-bsd style of entry.
   with no extra dummy args.  On USG and XENIX,
   NO_REMAP says this isn't used. */

#define CRT0_DUMMIES bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

/* USG systems do not actually support the load average,
so disable it for them.  */

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

/* Define addresses, macros, change some setup for dump */

#define NO_REMAP
#undef static
  /* Since NO_REMAP, problem with statics doesn't exist */

#ifdef USG5_3
#define TEXT_START 0x00000000
#else
#define TEXT_START 0x00400000
#define TEXT_END 0
#define DATA_START 0x00800000
#define DATA_END 0

/* The data segment in this machine always starts at address 0x00800000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.  */

#define DATA_SEG_BITS 0x00800000
#endif

#if 0 /* I refuse to promulgate a recommendation that would make
         users unable to debug - RMS.  */
/* delete the following line to foil optimization, enable debugging */
#define C_DEBUG_SWITCH -O
#endif

#define BSTRING
#undef  HAVE_TERMIO
#define HAVE_TERMIOS

/* Send signals to subprocesses by "typing" special chars at them.  */

#define SIGNALS_VIA_CHARACTERS

/*
 * 	Define SYSV_SYSTEM_DIR to use the V.3 getdents/readir
 *	library functions.  Almost, but not quite the same as
 *	the 4.2 functions
 */
#define SYSV_SYSTEM_DIR

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */
#undef  NONSYSTEM_DIR_LIBRARY

/* AIX utimes allegedly causes SIGSEGV.  */
#undef HAVE_UTIMES /* override configuration decision */

/* AIX defines FIONREAD, but it does not work.  */
#define BROKEN_FIONREAD

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long	/* For AIX (sysV) */

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)/65535.0) * 100.0)

/* This page was added in June 1990.  It may be incorrect for some versions
   of aix, so delete it if it causes trouble.  */

/* AIX has sigsetmask() */
#undef sigsetmask

/* AIX386 has BSD4.3 PTYs */

#define HAVE_PTYS

/* AIX has IPC. It also has sockets, and either can be used for client/server.
   I would suggest the client/server code be changed to use HAVE_SOCKETS rather
   than BSD as the conditional if sockets provide any advantages. */

#define HAVE_SYSVIPC

/* AIX has sockets */

#define HAVE_SOCKETS
/* #define SKTPAIR */ /* SKTPAIR works, but what is advantage over pipes? */

/* Specify the font for X to use.  */

#define X_DEFAULT_FONT "8x13"

/* AIX has a wait.h.  */

#define HAVE_WAIT_HEADER

/* sioctl.h should not be included, says bytheway@cs.utah.edu.  */
#undef NEED_SIOCTL
/* I'm guessing that that means it doesn't want ptem.h either.  */
#undef NEED_PTEM_H

/* aix has `union wait'  */
#define HAVE_UNION_WAIT

/* Here override various assumptions in ymakefile */

#ifdef __GNUC__
#define alloca(n) __builtin_alloca(n)
#if __GNUC__ < 2
#define LIB_STANDARD /usr/local/lib/gcc-gnulib -lbsd -lrts -lc 
#endif
/* -g fails to work, so it is omitted.  */
/* tranle says that -fstrength-reduce does not help.  */
#define C_DEBUG_SWITCH
#else
#define LIBS_MACHINE -lbsd -lrts
#endif

#define OBJECTS_MACHINE hftctl.o
#define LD_SWITCH_MACHINE -T0x00400000 -K -e start
#define LIBS_DEBUG		/* no -lg on aix ps/2 */

#ifdef USG5_3
#define XICCC
#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -T0x0 -K -e start

/* Things defined in s-usg5-3.h that need to be overridden.  */
#undef NOMULTIPLEJOBS
#undef BROKEN_TIOCGETC
#undef BROKEN_TIOCGWINSZ
#undef LIBX10_SYSTEM
#undef LIBX11_SYSTEM
#undef LIB_X11_LIB
#endif

/* Shared libraries are supported in a patch release of ps/2 1.2.1.
   If the system has them, the user can turn them on, and this code
   will make them work.  */
#define USG_SHARED_LIBRARIES /* Assume that by 19's release everyone has this.  */

#ifdef USG_SHARED_LIBRARIES
#define ORDINARY_LINK
#undef LIB_STANDARD
#undef LD_SWITCH_MACHINE
#if __GNUC__ > 1
#define LD_SWITCH_MACHINE -shlib
#endif
#endif
