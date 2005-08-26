/* machine description file for Bull SPS-7.
   Copyright (C) 1986, 1999 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="usg5-2"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Say this machine is a 68000 */

#ifndef m68000
#define m68000
#endif

#define sps7

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */
/* Suspect there is something weird about this machine, so turn it off.  */

/* #define LOAD_AVE_TYPE long */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0) */

#define SMX
#define	V3x

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

#define HAVE_SOCKETS

/*	Have the socketpair call
*/

#define SKTPAIR

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#define CLASH_DETECTION

/* Use Berkeley style interface to nlist */

#define NLIST_STRUCT

/* Define this to cause -N to be passed to ld.  This is needed
 *  in uniplus because of its funny memory space layout.
 * SMX--If you are using 32 bit (COFF) use "-N", else don't use anything.
 */

#define LD_SWITCH_MACHINE -N -T32 -e __start

/* If you are compiling for a 68020, then use -lc32 else use -lc */

#define LIB_STANDARD -lc32

/* Fore 16 bit, -linet, for 32 bit -linet32 (be sure you have it!). */

#define LIBS_MACHINE -linet32

/* Use -T32 for 68020, -T16 otherwise */

#define C_SWITCH_MACHINE -T32

#define BROKEN_SIGIO

/* Other than 68020 use ld16, as32, or undefine (defaults ld and as). */

#define ASS as32

#ifdef V3x
#define EXEC_MAGIC 0x10b
#define SEGMENT_MASK (NBPS-1)
#define	ADJUST_EXEC_HEADER f_hdr.f_magic = SMROMAGIC;\
 f_ohdr.stsize = 0;
#endif

/* arch-tag: 2240f71c-6f3b-4a82-80fc-4d56c682d7ad
   (do not change this comment) */
