/* machine description file for pyramid.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#ifdef __GNUC__
#define NO_ARG_ARRAY
#endif

/* XINT must explicitly sign extend */

#define EXPLICIT_SIGN_EXTEND

/* pyramid preprocessor defines "pyr", however the following is clearer */
#define pyramid

/* Don't use the union types any more.  They were used until Emacs 17.45.  */

#define NO_UNION_TYPE

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* Don't use the ordinary -g for debugging in cc */

#define C_DEBUG_SWITCH -gx

/* Reenable this #define for old versions of the Pyramid system.  */

/* #define PYRAMID_OLD */

/* arch-tag: b9097bc9-92be-46d4-acb1-13c2b966d581
   (do not change this comment) */
