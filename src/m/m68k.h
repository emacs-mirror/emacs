/* Machine description file for generic Motorola 68k.
   Copyright (C) 1985, 1995, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
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

/* Define WORDS_BIG_ENDIAN if lowest-numbered byte in a word
   is the most significant byte.  */

#define WORDS_BIG_ENDIAN

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */

#ifndef m68k
#define m68k
#endif

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   This flag only matters if you use USE_LISP_UNION_TYPE.  */

#define EXPLICIT_SIGN_EXTEND

#ifdef GNU_LINUX
#ifdef __ELF__
#define DATA_SEG_BITS 0x80000000
#endif

#define NO_REMAP
#define TEXT_START 0
#endif

/* arch-tag: 4eadd161-b4e8-4b82-82a1-e4ce7f42969d
   (do not change this comment) */
