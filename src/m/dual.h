/* machine description file for Dual machines using unisoft port.
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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Dual running System V (-machine=dual -opsystem=usg5-2)

  As of 17.46, this works except for a few changes
  needed in unexec.c.

Dual running Uniplus (-machine=dual -opsystem=unipl5-2)

  Works, as of 17.51.
NOTE-END */


/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000 are the ones defined so far.  */

#ifndef m68000
#define m68000
#endif

/* Data type of load average, as read out of kmem.  */
/* These are commented out since it does not really work in uniplus */

/* #define LOAD_AVE_TYPE long */

/* Convert that into an integer that is 100 for a load average of 1.0  */

/* #define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0) */

/* Change some things to avoid bugs in compiler */

#define SWITCH_ENUM_BUG 1

/* arch-tag: 7208d63c-9a23-469e-a9b1-908ac912c743
   (do not change this comment) */
