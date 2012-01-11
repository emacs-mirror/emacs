/* machine description file For the powerpc Macintosh.
   Copyright (C) 1994, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2010, 2011, 2012  Free Software Foundation, Inc.

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

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

#if (defined (__NetBSD__) || defined (__OpenBSD__)) && defined (__ELF__)
#define HAVE_TEXT_START
#endif

/* NAKAJI Hiroyuki <nakaji@tutrp.tut.ac.jp> says this is needed
   For MkLinux/LinuxPPC.  */

#ifdef GNU_LINUX
#define LINKER $(CC) -nostdlib
/* s/gnu-linux.h defines this to `-z nocombreloc' which does not work here
   because prefix-args is not used.  */
#undef LD_SWITCH_SYSTEM_TEMACS
#define LD_SWITCH_MACHINE_TEMACS -Xlinker -znocombreloc
#ifdef _ARCH_PPC64
#undef START_FILES
#define START_FILES pre-crt0.o /usr/lib64/crt1.o /usr/lib64/crti.o
#undef LIB_STANDARD
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib64/crtn.o
#endif
#endif

#ifdef _ARCH_PPC64
#ifndef _LP64
#define _LP64
#endif
#endif

/* arch-tag: 41913e4e-e7d1-4023-aadb-210cc31712ed
   (do not change this comment) */
