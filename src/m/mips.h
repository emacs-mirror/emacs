/* m- file for Mips machines.

Copyright (C) 1987, 1992, 1999, 2001, 2002, 2003, 2004, 2005, 2006,
  2007, 2008, 2009, 2010  Free Software Foundation, Inc.

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

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   This flag only matters if you use USE_LISP_UNION_TYPE.  */
#define EXPLICIT_SIGN_EXTEND

/* arch-tag: 8fd020ee-78a7-4d87-96ce-6129f52f7bee
   (do not change this comment) */
