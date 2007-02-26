/* Machine description file for Tensilica Xtensa.
   Copyright (C) 2007 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#define NO_ARG_ARRAY
#define NO_UNION_TYPE

#ifdef __LITTLE_ENDIAN
#undef WORDS_BIG_ENDIAN
#else
#define WORDS_BIG_ENDIAN
#endif

/* arch-tag: fe5872de-d565-4d81-8fe0-ea19865b3e6a
   (do not change this comment) */
