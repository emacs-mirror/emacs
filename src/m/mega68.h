/* machine description file for Megatest 68000's.
   Copyright (C) 1985, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
   USUAL-OPSYS="bsd4-2"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Say this machine is a 68000 */

#define m68000

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* arch-tag: 61e6deb4-a93e-444b-bbd5-9887a83f1bbc
   (do not change this comment) */
