/* Machine description file for intel 386.
   Copyright (C) 1987, 2002 Free Software Foundation, Inc.

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
Intel 386 (-machine=intel386 or -machine=is386.h)

  The possibilities for -opsystem are: bsd4-2, usg5-2-2, usg5-3,
  isc2-2, 386-ix, esix, linux, sco3.2v4, and xenix.

  18.58 should support a wide variety of operating systems.
  Use isc2-2 for Interactive 386/ix version 2.2.
  Use 386ix for prior versions.
  Use esix for Esix.
  Use linux for Linux.
  It isn't clear what to do on an SCO system.

  -machine=is386 is used for an Integrated Solutions 386 machine.
  It may also be correct for Microport systems.

Cubix QBx/386 (-machine=intel386 -opsystem=usg5-3)

  Changes merged in 19.1.  Systems before 2/A/0 may fail to compile etags.c
  due to a compiler bug.

Prime EXL (-machine=intel386 -opsystem=usg5-3)

  Minor changes merged in 19.1.
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

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* crt0.c, if it is used, should use the i386-bsd style of entry.
   with no extra dummy args.  On USG and XENIX,
   NO_REMAP says this isn't used. */

#define CRT0_DUMMIES bogus_fp,

/* crt0.c should define a symbol `start' and do .globl with a dot.  */

#define DOT_GLOBAL_START

#ifdef XENIX
/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE short

/* Convert that into an integer that is 100 for a load average of 1.0  */
#define LOAD_AVE_CVT(x) (((double) (x)) * 100.0 / FSCALE)

#define FSCALE 256.0         /* determined by experimentation...  */
#endif


#ifdef SOLARIS2
/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */
#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))

/* J.W.Hawtin@lut.ac.uk say Solaris 2.4 as well as Solaris 2.1 on X86
   requires -lkvm as well.
   And handa@etl.gov.jp says that -lkvm needs -llelf, at least on 2.5.  */
#define LIBS_MACHINE -lkvm -lelf

#ifndef SOLARIS2_4
/* J.W.hawtin@lut.ac.uk says Solaris 2.1 on the X86 has FSCALE defined in a
   system header. */
#else /* SOLARIS2_4 */
#ifndef __GNUC__
#if 0 /* wisner@gryphon.com says this screws up cpp */
#define C_SWITCH_MACHINE -Xa
#endif
#ifndef NOT_C_CODE
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */
#endif /* not NOT_C_CODE */
#endif /* not __GNUC__ */
#endif /* SOLARIS2_4 */

/* configure thinks solaris X86 has gethostname, but it does not work,
   so undefine it.  */
#undef HAVE_GETHOSTNAME

#else /* not SOLARIS2 */
#ifdef USG5_4 /* Older USG systems do not support the load average.  */
/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */

#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))
#define FSCALE 256.0
#endif
#endif /* not SOLARIS2 */

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

#ifdef XENIX
/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Since cannot purify, use standard Xenix 386 startup code. */

#define START_FILES	/lib/386/Sseg.o pre-crt0.o /lib/386/Scrt0.o

/* These really use terminfo.  */

#define LIBS_TERMCAP /lib/386/Slibcurses.a  \
   /lib/386/Slibtinfo.a /lib/386/Slibx.a

/* Standard libraries for this machine.  Since `-l' doesn't work in `ld'.  */
/* '__fltused' is unresolved w/o Slibcfp.a */
#define LIB_STANDARD /lib/386/Slibcfp.a /lib/386/Slibc.a
#else /* not XENIX */

/* this brings in alloca() if we're using cc */
#ifdef USG
#ifndef LIB_STANDARD
#ifdef USG5_4
#define LIB_STANDARD -lc
#else /* not USG5_4 */
#define LIB_STANDARD -lPW -lc
#endif /* not USG5_4 */
#endif /* LIB_STANDARD */

#define NO_REMAP 
#define TEXT_START 0
#endif /* USG */
#endif /* not XENIX */

/* If compiling with GCC, let GCC implement alloca.  */
#if defined(__GNUC__) && !defined(alloca)
#define alloca(n) __builtin_alloca(n)
#endif

#ifdef USG5_4
#define DATA_SEG_BITS 0x08000000
#endif

#ifdef MSDOS
#define NO_REMAP
#endif

#ifdef WINDOWSNT
#define VIRT_ADDR_VARIES
#define DATA_END 	get_data_end ()
#define DATA_START 	get_data_start ()
#define NO_ARG_ARRAY
#endif

#ifdef linux
/* libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared library, */
/* we cannot get the maximum address for brk */
#define ULIMIT_BREAK_VALUE (32*1024*1024)

#define SEGMENT_MASK ((SEGMENT_SIZE)-1)
#endif
