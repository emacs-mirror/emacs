/* machine description file for Apollo machine.
   Copyright (C) 1985, 1986, 1994, Free Software Foundation, Inc.

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
   USUAL-OPSYS="bsd4-3"  */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Say this machine is a 68000 */

/* #define m68000 */   /* Done by the C compiler */

#define APOLLO

/* Assume we use s-bsd4-3.h for system version 10.  */

#ifdef BSD4_3
#define APOLLO_SR10
#endif

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Do not define LOAD_AVE_TYPE or LOAD_AVE_CVT
   since there is no /dev/kmem */

/* Undefine VIRT_ADDR_VARIES because the virtual addresses of
   pure and impure space as loaded do not vary.  */

#undef VIRT_ADDR_VARIES

/* Define HAVE_ALLOCA because we use the system's version of alloca.  */

#define HAVE_ALLOCA

/* Prevent -lg from being used for debugging.  Not needed.  */

#define LIBS_DEBUG

/* Can't use the system's termcap.  It has compressed data sections that
   interfere with dumping.  That means we won't automatically get a vt100
   when we start up emacs in a dm pad (a dubious feature at best anyway). */

#undef LIBS_TERMCAP

/* Must use the system's malloc and alloca.  */

#define SYSTEM_MALLOC

/* Define the file we use for UNEXEC. */

#define UNEXEC unexapollo.o

/* The Apollo linker does not recognize the -X switch, so we remove it here. */

#define LD_SWITCH_SYSTEM

/* Define C_SWITCH_MACHINE to compile for 68020/68030 or PRISM.
   Define LD_SWITCH_MACHINE to save space by stripping symbols
   and use X11 libraries. */

#if _ISP__A88K
#define C_SWITCH_MACHINE -W0,-ncompress -W0,-opt,2 -A cpu,a88k -A sys,any -A run,bsd4.3
#define LD_SWITCH_MACHINE -A cpu,a88k -A sys,any -A run,bsd4.3
#else
#define C_SWITCH_MACHINE -W0,-ncompress -W0,-opt,2 -A cpu,3000 -A sys,any -A run,bsd4.3
#define LD_SWITCH_MACHINE -A cpu,m68k -A sys,any -A run,bsd4.3
#endif

#define OLDXMENU_OPTIONS ${C_SWITCH_MACHINE}
