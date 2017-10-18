/* m-altos	Altos 3068 Unix System V Release 2
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Vax is not big-endian: lowest numbered byte is least significant,
   but 68000's are. */

#define BIG_ENDIAN

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

#define EXPLICIT_SIGN_EXTEND

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

#define LIB_STANDARD -lc
#define C_ALLOCA		/* we have -lPW and alloca but it's broken!
				   <vsedev!ron> */
#define SWITCH_ENUM_BUG

#define NO_REMAP
#define STACK_DIRECTION -1

#define TERMINFO

#undef CANNOT_DUMP
#undef SHORTNAMES
#undef TERMCAP
