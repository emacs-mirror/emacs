/* R2 AIX machine/system dependent defines
   Copyright (C) 1988, 2002 Free Software Foundation, Inc.

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
   USUAL-OPSYS="aix3-1"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#ifdef USG5_4
#undef WORDS_BIG_ENDIAN
#else
#define WORDS_BIG_ENDIAN
#endif

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
#ifdef USG5_4
#define CANNOT_DUMP
#endif

#ifndef UNEXEC
#define UNEXEC unexaix.o
#endif

/* Define addresses, macros, change some setup for dump */

#define NO_REMAP

#ifndef USG5_4
#define TEXT_START 0x10000000
#define TEXT_END 0
#define DATA_START 0x20000000
#define DATA_END 0
#endif

/* The data segment in this machine always starts at address 0x20000000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.	*/

#ifndef USG5_4
#define DATA_SEG_BITS 0x20000000
#else
#define DATA_SEG_BITS 0
#endif

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

/* Specify the font for X to use.
   This used to be Rom14.500; that's nice on the X server shipped with
   the RS/6000, but it's not available on other servers.  */
#define X_DEFAULT_FONT "fixed"

/* Here override various assumptions in ymakefile */

#ifdef AIXHFT
#define OBJECTS_MACHINE hftctl.o
#endif

#ifndef USG5_4
#define C_SWITCH_MACHINE -D_BSD
#endif

#ifdef AIX3_2
/* -lpthreads seems to be necessary for Xlib in X11R6, and should be harmless
   on older versions of X where it happens to exist.  */
#ifdef HAVE_LIBPTHREADS
#define LIBS_MACHINE -lrts -lIM -liconv -lpthreads
#else
/* IBM's X11R5 use -lIM and -liconv in AIX 3.2.2.  */
#define LIBS_MACHINE -lrts -lIM -liconv
#endif
#else
#ifdef USG5_4
#define LIBS_MACHINE
#else
#define LIBS_MACHINE -lIM
#endif
#endif

#define START_FILES
#define HAVE_SYSVIPC
/*** BUILD 9008 - FIONREAD problem still exists in X-Windows. ***/
#define BROKEN_FIONREAD
/* As we define BROKEN_FIONREAD, SIGIO will be undefined in systty.h.
   But, on AIX, SIGAIO, SIGPTY, and SIGPOLL are defined as SIGIO,
   which causes compilation error at init_signals in sysdep.c.  So, we
   define these macros so that syssignal.h detects them and undefine
   SIGAIO, SIGPTY and SIGPOLL.  */
#define BROKEN_SIGAIO
#define BROKEN_SIGPTY
#define BROKEN_SIGPOLL

/* Don't try to include sioctl.h or ptem.h.  */
#undef NEED_SIOCTL
#undef NEED_PTEM_H

#define ORDINARY_LINK

#ifndef USG5_4
/* sfreed@unm.edu says add -bI:/usr/lpp/X11/bin/smt.exp for AIX 3.2.4.  */
/* marc@sti.com (Marc Pawliger) says ibmrs6000.inp is needed to avoid
   linker error for updated X11R5 libraries, which references pthread library
   which most machines don't have.  We use the name .inp instead of .imp
   because .inp is a better convention to use in make-dist for naming
   random input files.  */
#ifdef THIS_IS_MAKEFILE /* Don't use this in configure.  */
#ifdef AIX4
#define LD_SWITCH_MACHINE -Wl,-bnodelcsect
#else /* not AIX4 */
#ifdef HAVE_AIX_SMT_EXP
#define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:$(srcdir)/m/ibmrs6000.inp,-bI:/usr/lpp/X11/bin/smt.exp
#else
#define LD_SWITCH_MACHINE -Wl,-bnso,-bnodelcsect,-bI:/lib/syscalls.exp,-bI:$(srcdir)/m/ibmrs6000.inp
#endif
#endif /* not AIX4 */
#endif /* THIS_IS_MAKEFILE */

/* Avoid gcc 2.7.x collect2 bug by using /bin/ld instead.  */
#if __GNUC__ == 2 && __GNUC_MINOR__ == 7
#define LD_SWITCH_SITE -B/bin/
#endif

#ifndef NLIST_STRUCT
/* AIX supposedly doesn't use this interface, but on the RS/6000
   it apparently does.  */
#define NLIST_STRUCT
#endif
#endif /* USG5_4 */
