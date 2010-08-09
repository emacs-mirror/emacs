/* machine description file for Sun 4 SPARC.

Copyright (C) 1987, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
  2009, 2010  Free Software Foundation, Inc.

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

/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */
#define WORDS_BIG_ENDIAN

/* __sparc__ is defined by the compiler by default.  */

/* XINT must explicitly sign-extend
   This flag only matters if you use USE_LISP_UNION_TYPE.  */
#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

#ifdef __arch64__		/* GCC, 64-bit ABI.  */

#define BITS_PER_LONG 64

#ifndef _LP64
#define _LP64 /* Done on Alpha -- not sure if it should be here.  -- fx */
#endif

#endif  /* __arch64__ */

/* arch-tag: 0a6f7882-33fd-4811-9832-7466c51e50f7
   (do not change this comment) */
