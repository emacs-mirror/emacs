/* machine description file For the powerpc Macintosh.
   Copyright (C) 1994, 2001, 2002 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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

/* Use type EMACS_INT rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

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

#if defined(__OpenBSD__)
#define ORDINARY_LINK
#endif

#define UNEXEC unexelf.o

#define NO_TERMIO

#if defined (LINUX) || defined (__NetBSD__) || defined (__OpenBSD__)
# define TEXT_END ({ extern int _etext; &_etext; })
#endif

#if (defined (__NetBSD__) || defined (__OpenBSD__)) && defined (__ELF__)
#define HAVE_TEXT_START
#endif

/* NAKAJI Hiroyuki <nakaji@tutrp.tut.ac.jp> says this is needed
   For MkLinux/LinuxPPC.  */

#ifdef LINUX
#define LINKER $(CC) -nostdlib
#define LD_SWITCH_MACHINE -Xlinker -m -Xlinker elf32ppc
/* s/gnu-linux.h defines this to `-z nocombreloc' which does not work here
   because prefix-args is not used.  */
#undef LD_SWITCH_SYSTEM_TEMACS
#define LD_SWITCH_MACHINE_TEMACS -Xlinker -znocombreloc
#endif

#if 0  /* This breaks things on PPC GNU/Linux ecept for Yellowdog,
	  even with identical GCC, as, ld.  Let's take it out until we
	  know what's really going on here.  */
/* GCC 2.95 and newer on GNU/Linux PPC changed the load address to
   0x10000000.  */
#if defined __linux__
#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 95)
#define DATA_SEG_BITS  0x10000000
#endif
#endif
#endif /* 0 */
