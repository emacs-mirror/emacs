/* altos machine description file	Altos 3068 Unix System V Release 2
   Copyright (C) 1985, 1986, 2002 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="usg5-2"  */

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

#define EXPLICIT_SIGN_EXTEND

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

#define LIB_STANDARD -lc

#define SWITCH_ENUM_BUG

#define NO_REMAP

#undef TERMINFO

#undef CANNOT_DUMP
#undef SHORTNAMES
#define TERMCAP

#define LIBS_TERMCAP -ltermlib
#define PURESIZE 220000
#define ALTOS

#ifdef __GNUC__
#define COFF_ENCAPSULATE
#endif
