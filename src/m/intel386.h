/* Machine description file for intel 386.
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
Intel 386 (-machine=intel386)

  The possibilities for -opsystem are: bsd4-2, usg5-2-2, usg5-3,
  isc2-2, 386-ix, and linux.

  18.58 should support a wide variety of operating systems.
  Use linux for Linux.
  It isn't clear what to do on an SCO system.

NOTE-END */

/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */

#undef WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

/* #define NO_ARG_ARRAY */

/* crt0.c, if it is used, should use the i386-bsd style of entry.
   with no extra dummy args.  On USG and XENIX,
   NO_REMAP says this isn't used. */

#define CRT0_DUMMIES bogus_fp,

#ifdef SOLARIS2
/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */
#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))

/* J.W.Hawtin@lut.ac.uk say Solaris 2.4 as well as Solaris 2.1 on X86
   requires -lkvm as well.
   And handa@etl.gov.jp says that -lkvm needs -llelf, at least on 2.5.  */
#define LIBS_MACHINE -lkvm -lelf

/* configure thinks solaris X86 has gethostname, but it does not work,
   so undefine it.  */
#undef HAVE_GETHOSTNAME

#else /* not SOLARIS2 */
#ifdef USG5_4 /* Older USG systems do not support the load average.  */
/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* This is totally uncalibrated. */

#define LOAD_AVE_CVT(x) ((int) (((double) (x)) * 100.0 / FSCALE))
#define FSCALE 256.0
#endif /* USG5_4 */
#endif /* not SOLARIS2 */

#ifdef USG
#define TEXT_START 0
#endif /* USG */

#ifdef MSDOS
#define NO_REMAP
#endif

#ifdef WINDOWSNT
#define VIRT_ADDR_VARIES
#define DATA_START 	get_data_start ()
#define NO_ARG_ARRAY
#endif

#ifdef GNU_LINUX
/* libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared library, */
/* we cannot get the maximum address for brk */
#define ULIMIT_BREAK_VALUE (32*1024*1024)

#define SEGMENT_MASK ((SEGMENT_SIZE)-1)
#endif

#if defined (DARWIN_OS)
#ifdef _LP64
/* For Intel Mac, with CC='gcc -arch x86_64'.  */
#define NO_ARG_ARRAY
#endif
#endif

/* arch-tag: 746338f0-cb7b-4f49-a98c-cb50817cf2ec
   (do not change this comment) */
