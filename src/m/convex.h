/* machine description file for Convex (all models).
   Copyright (C) 1987, 1994, 2002 Free Software Foundation, Inc.

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
   USUAL-OPSYS="bsd4-3"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.
 * Maybe it would be better to simply correct the code. */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

/* #define WORD_MACHINE */
  
/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */
#ifndef convex  /* The compiler doesn't always do this.  */
#define convex
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

#ifndef __GNUC__ /* David M. Cooke <dcooke@haven.larc.nasa.gov>
		    and Ralph Sobek <Ralph.Sobek@cerfacs.fr> agree
		    must ignore one arg when compiled with convex compiler.  */
#define CRT0_DUMMIES ignore,
#else 
#define CRT0_DUMMIES
#endif

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) ((x) * 100.0)

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

/*#define VIRT_ADDR_VARIES*/

/* Must use the system's termcap.  It does special things.  */
  
#define LIBS_TERMCAP -ltermcap
  
/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

/* #define NO_REMAP */

/* Addresses on the Convex have the high bit set.  */
#define DATA_SEG_BITS (1 << (BITS_PER_INT-1))

/* Right shift is logical shift.
   And the usual way of handling such machines, which involves
   copying the number into sign_extend_temp, does not work
   for reasons as yet unknown.  */

#define XINT(a)  sign_extend_lisp_int (a)

/* Convex uses a special version of unexec.  */

#define UNEXEC unexconvex.o

/* you gotta define 'COFF' for post 6.1 unexec. */

#define COFF
#define TEXT_START 0x80001000

/* Posix stuff for Convex OS 8.1 and up. */

#define LD_SWITCH_MACHINE \
    -e__start -L /usr/lib \
    '-A__iob=___ap$$iob' '-A_use_libc_sema=___ap$$use_libc_sema'

/* Use <dirent.h>. */
#define SYSV_SYSTEM_DIR

#ifdef _POSIX_SOURCE

/* These symbols have been undefined to advance the state of the art. */

#define S_IFMT _S_IFMT
#define S_IFDIR _S_IFDIR

#define S_IREAD _S_IREAD
#define S_IWRITE _S_IWRITE
#define S_IEXEC _S_IEXEC

#endif

/* Ptys may start below ptyp0; call a routine to hunt for where. */

#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER first_pty_letter()

#if 0
/*
 * Force a K&R compilation and libraries with the Convex V 4.0 C compiler
 */
#define C_SWITCH_MACHINE -pcc
#define LIB_STANDARD -lc_old
#define LIBS_MACHINE -lC2_old
#define LD_SWITCH_MACHINE -X -NL -fn -Enoposix -A__iob=___ap\$$iob \
 -A_use_libc_sema=___ap\$$use_libc_sema -L /usr/lib
#endif

/* Avoid error in xrdb.c - d.m.cooke@larc.nasa.gov.  */
#define DECLARE_GETPWUID_WITH_UID_T

/* Call getpgrp properly.  */
#define GETPGRP_NO_ARG

/* Tested for both Convex C and GNUC by d.m.cooke@larc.nasa.gov.  */
#define LIBS_MACHINE -lC2

/* Avoid error in getloadavg.c.  */
#define NLIST_NAME_UNION  1

#if 0  /* This is supposed to be an improvement.
	  It would be good for people to try enabling this code
	  and report the results.  */
/* gcc -nostdlib prevents some math symbols from being included.
   So we have to use -nostartfiles instead. */
#define LINKER $(CC) -nostartfiles

#define ORDINARY_LINK

#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE \
    -L /usr/lib \
    '-A__iob=___ap$$iob' '-A_use_libc_sema=___ap$$use_libc_sema'
#endif

/* There is some indication that the convex has sys/wait.h
   but it does not work right.  */
#undef HAVE_SYS_WAIT_H
