/* R2 AIX machine/system dependent defines
   Copyright (C) 1988 Free Software Foundation, Inc.

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
   USUAL-OPSYS="aix3-1"  */

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#define IBMR2AIX

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.	 */

#define NO_UNION_TYPE

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */
/* #define CANNOT_DUMP */

#define UNEXEC unexaix.o

/* Define addresses, macros, change some setup for dump */

#define NO_REMAP

#define TEXT_START 0x10000000
#define TEXT_END 0
#define DATA_START 0x20000000
#define DATA_END 0

/* The data segment in this machine always starts at address 0x20000000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.	*/

#define DATA_SEG_BITS 0x20000000

#ifdef CANNOT_DUMP
/* Define shared memory segment symbols */

#define PURE_SEG_BITS 0x30000000

/* Use shared memory.  */
/* This is turned off because it does not always work.	See etc/AIX.DUMP.  */
/* #define HAVE_SHM */
#define SHMKEY 5305035		/* used for shared memory code segments */
#endif /* CANNOT_DUMP */

#define N_BADMAG(x) BADMAG(x)
#define N_TXTOFF(x) A_TEXTPOS(x)
#define N_SYMOFF(x) A_SYMPOS(x)
#define A_TEXT_OFFSET(HDR) sizeof(HDR)
/* #define ADJUST_EXEC_HEADER \
    unexec_text_start += sizeof(hdr); \
    unexec_data_start = ohdr.a_dbase
*/
#undef ADDR_CORRECT
#define ADDR_CORRECT(x) ((int)(x))

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.	 */

/* Note: aix3-2.h defines HAVE_ALLOCA; aix3-1.h doesn't.  */
#ifndef HAVE_ALLOCA
#define C_ALLOCA
#define STACK_DIRECTION -1 /* tell alloca.c which way it grows */
#endif

/* Specify the font for X to use.
   This used to be Rom14.500; that's nice on the X server shipped with
   the RS/6000, but it's not available on other servers.  */
#define X_DEFAULT_FONT "fixed"

/* Here override various assumptions in ymakefile */

#ifdef AIXHFT
#define OBJECTS_MACHINE hftctl.o
#endif

#define C_SWITCH_MACHINE -D_BSD

#ifdef AIX3_2
/* IBM's X11R5 use -lIM and -liconv in AIX 3.2.2.  */
#define LIBS_MACHINE -lrts -lIM -liconv
#else
#define LIBS_MACHINE -lIM
#endif

#define START_FILES
#define HAVE_SYSVIPC
#define HAVE_GETWD
/*** BUILD 9008 - FIONREAD problem still exists in X-Windows. ***/
#define BROKEN_FIONREAD

/* Don't try to include sioctl.h or ptem.h.  */
#undef NEED_SIOCTL
#undef NEED_PTEM_H

#define ORDINARY_LINK
/* sfreed@unm.edu says add -bI:/usr/lpp/X11/bin/smt.exp for AIX 3.2.4.  */
/* marc@sti.com (Marc Pawliger) says ibmrs6000.inp is needed to avoid
   linker error for updated X11R5 libraries, which references pthread library
   which most machines don't have.  We use the name .inp instead of .imp
   because .inp is a better convention to use in make-dist for naming
   random input files.  */
#ifdef HAVE_AIX_SMT_EXP
#define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:$(srcdir)/m/ibmrs6000.inp,-bI:/usr/lpp/X11/bin/smt.exp
#else
#define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:$(srcdir)/m/ibmrs6000.inp
#endif

/* AIX supposedly doesn't use this interface, but on the RS/6000
   it apparently does.  */
#define NLIST_STRUCT
