/* Machine description file for ARM-based non-RISCiX machines.
   Copyright (C) 1994, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007  Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */



/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#undef WORDS_BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  We can't
 * do this on the arm with gcc, since the first 4 args are in registers.  */

#ifdef __GNUC__
#define NO_ARG_ARRAY
#else
#undef NO_ARG_ARRAY
#endif

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#undef WORD_MACHINE

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (((int)(c) << 24) >> 24)

#define NO_UNION_TYPE

#define NO_REMAP

/* arch-tag: 07856f0c-f0c8-4bd8-99af-0b7fa1e5ee42
   (do not change this comment) */
