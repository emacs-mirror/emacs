/* machine description file for IBM S390 in 64-bit mode
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008
     Free Software Foundation, Inc.

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

/* This file was made by copying the significant parts of amdx86-64.h
   into ibms390.h.  */


/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="<name of system .h file here, without the s- or .h>"

NOTE-START
IBM s390 64 bits (-machine=ibms390x64)

  The possibilities for -opsystem are: gnu-linux.

NOTE-END */

#define BITS_PER_LONG 64
#define BITS_PER_EMACS_INT 64

/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define the type to use.  */
#define EMACS_INT long
#define EMACS_UINT unsigned long

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   This flag only matters if you use USE_LISP_UNION_TYPE.  */

#undef EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

#define VIRT_ADDR_VARIES

/* Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.  Undefine it if an
   assembler-language alloca in the file alloca.s should be used. */

#define HAVE_ALLOCA

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#undef NO_REMAP

/* On the 64 bit architecture, we can use 60 bits for addresses */

#define VALBITS         60

#define LINKER $(CC) -nostdlib

/* Define XPNTR to avoid or'ing with DATA_SEG_BITS */

#define XPNTR(a) XUINT (a)

#undef START_FILES
#ifdef HAVE_LIB64_DIR
#define START_FILES pre-crt0.o /usr/lib64/crt1.o /usr/lib64/crti.o
#else
#define START_FILES pre-crt0.o /usr/lib/crt1.o /usr/lib/crti.o
#endif

#undef LIB_STANDARD
#ifdef HAVE_LIB64_DIR
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib64/crtn.o
#else
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtn.o
#endif

/* arch-tag: 4b87653c-6add-4663-8691-7d9dc17b5519
   (do not change this comment) */
