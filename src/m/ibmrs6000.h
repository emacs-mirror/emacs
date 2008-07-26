/* R2 AIX machine/system dependent defines
   Copyright (C) 1988, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="aix3-1"  */

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#define IBMR2AIX

#ifndef UNEXEC
#define UNEXEC unexaix.o
#endif

/* Define addresses, macros, change some setup for dump */

#define NO_REMAP

/* The data segment in this machine always starts at address 0x20000000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.  */

#ifndef USG5_4
#define TEXT_START 0x10000000
#define DATA_START 0x20000000
#define WORDS_BIG_ENDIAN
#define DATA_SEG_BITS 0x20000000

/* sfreed@unm.edu says add -bI:/usr/lpp/X11/bin/smt.exp for AIX 3.2.4.  */
/* marc@sti.com (Marc Pawliger) says ibmrs6000.inp is needed to avoid
   linker error for updated X11R5 libraries, which references pthread library
   which most machines don't have.  We use the name .inp instead of .imp
   because .inp is a better convention to use in make-dist for naming
   random input files.  */
#ifdef THIS_IS_MAKEFILE /* Don't use this in configure.  */
#define LD_SWITCH_MACHINE -Wl,-bnodelcsect
#endif /* THIS_IS_MAKEFILE */

#ifndef NLIST_STRUCT
/* AIX supposedly doesn't use this interface, but on the RS/6000
   it apparently does.  */
#define NLIST_STRUCT
#endif

/* -lpthreads seems to be necessary for Xlib in X11R6, and should be harmless
   on older versions of X where it happens to exist.  */
#ifdef HAVE_LIBPTHREADS
#define LIBS_MACHINE -lrts -lIM -liconv -lpthreads
#else
/* IBM's X11R5 use -lIM and -liconv in AIX 3.2.2.  */
#define LIBS_MACHINE -lrts -lIM -liconv
#endif

#else /* USG5_4 */
#undef WORDS_BIG_ENDIAN
#define DATA_SEG_BITS 0
#define LIBS_MACHINE
#endif /* USG5_4 */

#define START_FILES
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

#define ORDINARY_LINK

/* arch-tag: 028318ee-a7ae-4a08-804d-cc1e6588d003
   (do not change this comment) */
