/* machine description file for Iris-4D machines.  Use with s/irix*.h.
   Copyright (C) 1987, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
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

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the bit field into an int.  In other words, if bit fields
   are always unsigned.

   This flag only matters if you use USE_LISP_UNION_TYPE.  */

#define EXPLICIT_SIGN_EXTEND

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#undef UNEXEC
#define UNEXEC unexelf.o

#define TEXT_START 0x400000

/*
 * DATA_SEG_BITS forces extra bits to be or'd in with any pointers which
 * were stored in a Lisp_Object (as Emacs uses fewer than 32 bits for
 * the value field of a LISP_OBJECT).
 */

#define DATA_START 0x10000000
#define DATA_SEG_BITS	0x10000000

#undef LIBS_MACHINE
#define LIBS_MACHINE
#define LIBS_DEBUG

/* Use terminfo instead of termcap.  */

#define TERMINFO

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'q'

#if _MIPS_SZLONG != 64
/* fixme: should there be 64-bit definitions?  (The ones below aren't OK.)  */

/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short.  */

#define XUINT(a) (((unsigned)(a) << BITS_PER_INT-VALBITS) >> BITS_PER_INT-VALBITS)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + (((unsigned) (ptr) << BITS_PER_INT-VALBITS) >> BITS_PER_INT-VALBITS))
#endif /* _LP64 */

/* arch-tag: fff5e139-9ae0-465d-afec-837c41ea0aa6
   (do not change this comment) */
