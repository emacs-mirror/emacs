/* machine description file for ns16000.
   Copyright (C) 1985 Free Software Foundation, Inc.

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
   USUAL-OPSYS="note"

NOTE-START
For the Encore, use `-opsystem=umax'.
For a Tektronix 16000 box (a 6130, perhaps?), use `-opsystem=bsd4-2'.
Use `-machine=ns16000' for both.
NOTE-END  */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* 16000 is little-endian (lowest-numbered byte is least significant) */

/* #define BIG_ENDIAN */  /* So don't define this.  */

/* Say this machine is a 16000 */

#define ns16000 1

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Data type of load average, as read out of kmem.  */

#ifndef USG
#define LOAD_AVE_TYPE double
#endif

/* Convert that into an integer that is 100 for a load average of 1.0  */

#ifndef USG
#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))
#endif

#ifdef USG

/* Control assembler syntax used in alloca.s.  */
#define NS5

/* On early NS systems ulimit was buggy. If set emacs uses this value
 * for the maximum sbrk value instead of getting it from ulimit.
 */
#define ULIMIT_BREAK_VALUE 0x7E0000

/* Early NS compilers have this bug. I believe it has been fixed in later
 * releases.
 */
#define SHORT_CAST_BUG

#define SEGMENT_MASK (NBPS - 1)

/* Variables to get crt0.c to come out correctly */
#define CRT0_DUMMIES bogus_fp,
#define DOT_GLOBAL_START

/* Control how emacsclient communicates.  */
#define HAVE_SYSVIPC

/* Set this to /bin/mail unless you have a better mail posting program */
#define MAIL_PROGRAM_NAME "/usr/local/bin/remail"

/* Tell sysdep.c not to define bzero, etc.  */
#undef BSTRING
#define BSTRING

/* Macro definitions to emulate BSD functions with SysV ones */
#undef bcopy
#undef bzero
#undef bcmp

#define bcopy(a,b,s)	memcpy(b,a,s)
#define bzero(a,s)	memset(a,0,s)
#define bcmp		memcmp

/* This avoids problems with uninitialized static variables going in .data.  */
#define static

#endif /* USG */
