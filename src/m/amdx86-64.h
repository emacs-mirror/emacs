/* machine description file for AMD x86-64.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
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

#ifdef i386
/* Although we're running on an amd64 kernel, we're actually compiling for
   the x86 architecture.  The user should probably have provided an
   explicit --build to `configure', but if everything else than the kernel
   is running in i386 mode, then the bug is really ours: we should have
   guessed better.  */
#include "m/intel386.h"
#else

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="linux"  */

#define BITS_PER_LONG           64
#define BITS_PER_EMACS_INT      64

/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */

#undef WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
/* __x86_64 defined automatically.  */

/* Define the type to use.  */
#define EMACS_INT               long
#define EMACS_UINT              unsigned long

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   This flag only matters if you use USE_LISP_UNION_TYPE.  */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define XPNTR to avoid or'ing with DATA_SEG_BITS */
#undef DATA_SEG_BITS

#ifdef __FreeBSD__

/* The libraries for binaries native to the build host's architecture are
   installed under /usr/lib in FreeBSD, and the ones that need special paths
   are 32-bit compatibility libraries (installed under /usr/lib32).  To build
   a native binary of Emacs on FreeBSD/amd64 we can just point to /usr/lib.  */

#undef START_FILES
#define START_FILES pre-crt0.o $(CRT_DIR)/crt1.o $(CRT_DIR)/crti.o

/* The duplicate -lgcc is intentional in the definition of LIB_STANDARD.
   The reason is that some functions in libgcc.a call functions from libc.a,
   and some libc.a functions need functions from libgcc.a.  Since most
   versions of ld are one-pass linkers, we need to mention -lgcc twice,
   or else we risk getting unresolved externals.  */
#undef LIB_STANDARD
#define LIB_STANDARD -lgcc -lc -lgcc $(CRT_DIR)/crtn.o

#elif defined(__OpenBSD__)

#undef START_FILES
#define START_FILES pre-crt0.o $(CRT_DIR)/crt0.o $(CRT_DIR)/crtbegin.o
#undef LIB_STANDARD
#define LIB_STANDARD -lgcc -lc -lgcc $(CRT_DIR)/crtend.o

#elif defined(__NetBSD__)

/* LIB_STANDARD and START_FILES set correctly in s/netbsd.h */

#elif defined(SOLARIS2)

#undef START_FILES
#undef LIB_STANDARD

#elif defined(__APPLE__)

/* LIB_STANDARD and START_FILES set correctly in s/darwin.h */

#else /* !__OpenBSD__ && !__FreeBSD__ && !__NetBSD__ && !SOLARIS2
         && !__APPLE__ */
/* The duplicate -lgcc is intentional in the definition of LIB_STANDARD.
   The reason is that some functions in libgcc.a call functions from libc.a,
   and some libc.a functions need functions from libgcc.a.  Since most
   versions of ld are one-pass linkers, we need to mention -lgcc twice,
   or else we risk getting unresolved externals.  */
#undef START_FILES
#undef LIB_STANDARD
#define START_FILES pre-crt0.o $(CRT_DIR)/crt1.o $(CRT_DIR)/crti.o
#define LIB_STANDARD -lgcc -lc -lgcc $(CRT_DIR)/crtn.o

#endif /* __FreeBSD__ */
#endif /* !i386 */

/* arch-tag: 8a5e001d-e12e-4692-a3a6-0b15ba271c6e
   (do not change this comment) */
