/* altos machine description file	Altos 3068 Unix System V Release 2
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="usg5-2"  */

/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Vax is not big-endian: lowest numbered byte is least significant,
   but 68000's are. */

#define BIG_ENDIAN

#define EXPLICIT_SIGN_EXTEND

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

#define LIB_STANDARD -lc

#ifdef __GNUC__
#define alloca __builtin_alloca
#define HAVE_ALLOCA
#else
#define C_ALLOCA		/* we have -lPW and alloca but it's broken!
				   <vsedev!ron> */
#endif

#define SWITCH_ENUM_BUG

#define NO_REMAP
#define STACK_DIRECTION -1

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
