/* Machine description file for the alpha chip.

Copyright (C) 1994, 1997, 1999, 2001, 2002, 2003, 2004, 2005, 2006,
  2007, 2008, 2009, 2010  Free Software Foundation, Inc.

Author: Rainer Schoepf
(according to authors.el)

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

#ifndef _LP64
#define _LP64 /* This doesn't appear to be necessary on OSF 4/5  -- fx.  */
#endif

/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */
#undef WORDS_BIG_ENDIAN

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */
/* __alpha defined automatically */


/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   This flag only matters if you use USE_LISP_UNION_TYPE.  */
#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

#ifdef __ELF__

#if !defined(GNU_LINUX) && !defined(__NetBSD__)
#define DATA_START    0x140000000
#endif

#if (defined (__NetBSD__) || defined (__OpenBSD__))
#define HAVE_TEXT_START
#endif

#else  /* not __ELF__ */

/* Describe layout of the address space in an executing process.  */
#define TEXT_START    0x120000000
#define DATA_START    0x140000000

#endif /* __ELF__ */

/* On the Alpha it's best to avoid including TERMIO since struct
   termio and struct termios are mutually incompatible.  */
#define NO_TERMIO

#if defined (GNU_LINUX) || defined (__NetBSD__) || defined (__OpenBSD__)
# ifndef __ELF__
#  define COFF
# endif /* notdef __ELF__ */
#endif

/* Many Alpha implementations (e.g. gas 2.8) can't handle DBL_MIN:
   they generate code that uses a signaling NaN instead of DBL_MIN.
   Define DBL_MIN_REPLACEMENT to be the next value larger than DBL_MIN:
   this avoids the assembler bug.  */
#define DBL_MIN_REPLACEMENT 2.2250738585072019e-308

/* arch-tag: 978cb578-1e25-4a60-819b-adae0972aa78
   (do not change this comment) */
