/* RTPC AIX machine/system dependent defines
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
   USUAL-OPSYS="usg5-2-2"  */

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#ifndef IBMAIX
#define IBMAIX
#endif

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* No load average information appears in the AIX kernel.  VRM has this
   info, and if anyone desires they should fix fns.c to get it out of VRM */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */

/* Define addresses, macros, change some setup for dump */

#undef COFF
#define NO_REMAP
#undef static
  /* Since NO_REMAP, problem with statics doesn't exist */

#define TEXT_START 0x10000000
#define TEXT_END 0
#define DATA_START 0x20000000
#define DATA_END 0

/* The data segment in this machine always starts at address 0x20000000.
   An address of data cannot be stored correctly in a Lisp object;
   we always lose the high bits.  We must tell XPNTR to add them back.  */

#define DATA_SEG_BITS 0x20000000

#define N_BADMAG(x) BADMAG(x)
#define N_TXTOFF(x) A_TEXTPOS(x)
#define N_SYMOFF(x) A_SYMPOS(x)
#define A_TEXT_OFFSET(HDR) sizeof(HDR)
#define ADJUST_EXEC_HEADER \
    unexec_text_start += sizeof(hdr); \
    unexec_data_start = ohdr.a_dbase
#undef ADDR_CORRECT
#define ADDR_CORRECT(x) ((int)(x))

/* This is the offset of the executable's text, from the start of the file.  */

#define A_TEXT_SEEK(HDR) (N_TXTOFF (hdr) + sizeof (hdr))

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#define C_ALLOCA
#define STACK_DIRECTION -1 /* tell alloca.c which way it grows */

/* AIX has PTYs, so define here, along with macros needed to make them work. */

#define HAVE_PTYS
#define PTY_ITERATION for (i=0; i<256; i++)
#define PTY_NAME_SPRINTF sprintf (ptyname, "/dev/ptc%d", i);

#define PTY_TTY_NAME_SPRINTF				\
{ /* Check that server side not already open */		\
  if ((ioctl (*ptyv, PTYSTATUS, 0) & 0xFFFF) != 0)	\
    {							\
      close (*ptyv);					\
      continue;						\
    }							\
  /* And finally to be sure we can open it later */	\
  sprintf (ptyname, "/dev/pts%d", i);			\
  signal (SIGHUP,SIG_IGN);				\
}     /* ignore hangup at process end */

/* TIOCNOTTY doesn't occur on AIX, but the rest
   of the conditionalized code in process.c does
   the right thing if we fake this out.  */
#define TIOCNOTTY IOCTYPE

/* AIX has IPC. It also has sockets, and either can be used for client/server.
   I would suggest the client/server code be changed to use HAVE_SOCKETS rather
   than BSD as the conditional if sockets provide any advantages. */

#define HAVE_SYSVIPC

/* AIX has sockets */

#define HAVE_SOCKETS
/* #define SKTPAIR */ /* SKTPAIR works, but what is advantage over pipes? */

/* Specify the font for X to use.  */

#define X_DEFAULT_FONT "Rom14.500"

/* Here override various assumptions in ymakefile */

/* On AIX 2.2.1, use these definitions instead
#define C_SWITCH_MACHINE -I/usr/include -Nn2000
#define LIBS_MACHINE -lX -lrts
*/

#define C_SWITCH_MACHINE -I/usr/include -I/usr/include/bsd -Nn2000
#define LIBS_MACHINE -lXMenu -lX -lsock -lbsd -lrts

#define OBJECTS_MACHINE hftctl.o
#define START_FILES /lib/crt0.o
/* -lXMenu, -lX must precede -lsock, -lbsd */
#define LD_SWITCH_MACHINE -n -T0x10000000 -K -e start

#if 0 /* I refuse to promulgate a recommendation that would make
         users unable to debug - RMS.  */
/* delete the following line to foil optimization, enable debugging */
#define C_DEBUG_SWITCH -O
#endif


/* Setup to do some things BSD way - these won't work previous to AIX 2.1.2 */

#include </usr/include/bsd/BSDtoAIX.h>
#define BSTRING
#define HAVE_GETTIMEOFDAY
#define HAVE_SELECT
#define HAVE_TIMEVAL
#define HAVE_VFORK

/* But don't use utimes() -- it causes SIGSEGV!  Use utime() instead. */
#define USE_UTIME

/* AIX defines FIONREAD, but it does not work.  */
#define BROKEN_FIONREAD

/* rocky@watson.ibm.com says this is needed.  */
#define HAVE_FTIME
