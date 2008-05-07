/* machine description file for Sun 4 SPARC.
   Copyright (C) 1987, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
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
   USUAL-OPSYS="note"

NOTE-START
Use -opsystem=sunos4 for operating system version 4, and
-opsystem=bsd4-2 for earlier versions.
NOTE-END  */

/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Say this machine is a sparc */

#ifndef sparc
#define sparc
#endif

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Mask for address bits within a memory segment */

#define SEGMENT_MASK (SEGSIZ - 1)

#if !defined (__NetBSD__) && !defined (__linux__) && !defined (__OpenBSD__)
/* This really belongs in s/sun.h.  */

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

#define A_TEXT_OFFSET(HDR) sizeof (HDR)

/* This is the offset of the executable's text, from the start of the file.  */

#define A_TEXT_SEEK(HDR) (N_TXTOFF (hdr) + sizeof (hdr))

#endif /* not __NetBSD__ and not __linux__ and not __OpenBSD__ */

#ifdef __arch64__		/* GCC, 64-bit ABI.  */
#define BITS_PER_LONG 64
#ifdef __linux__
#undef START_FILES
#define START_FILES pre-crt0.o /usr/lib64/crt1.o /usr/lib64/crti.o

/* The duplicate -lgcc is intentional in the definition of LIB_STANDARD.
   The reason is that some functions in libgcc.a call functions from libc.a,
   and some libc.a functions need functions from libgcc.a.  Since most
   versions of ld are one-pass linkers, we need to mention -lgcc twice,
   or else we risk getting unresolved externals.  */
#undef LIB_STANDARD
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib64/crtn.o
#endif

#ifndef _LP64
#define _LP64			/* Done on Alpha -- not sure if it
				   should be here.  -- fx */
#endif
#endif

/* arch-tag: 0a6f7882-33fd-4811-9832-7466c51e50f7
   (do not change this comment) */
