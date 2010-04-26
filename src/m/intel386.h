/* Machine description file for intel 386.
   Copyright (C) 1987, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2010  Free Software Foundation, Inc.

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

#ifdef USG
#define TEXT_START 0
#endif /* USG */

#ifdef WINDOWSNT
#define VIRT_ADDR_VARIES
#define DATA_START 	get_data_start ()
#endif

#ifdef GNU_LINUX
/* libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared library, */
/* we cannot get the maximum address for brk */
#define ULIMIT_BREAK_VALUE (32*1024*1024)

#define SEGMENT_MASK ((SEGMENT_SIZE)-1)
#endif

/* arch-tag: 746338f0-cb7b-4f49-a98c-cb50817cf2ec
   (do not change this comment) */
