/* Declare __fpending.

   Copyright (C) 2000, 2003, 2005-2006, 2009-2019 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Written by Jim Meyering.  */

#include <stddef.h>
#include <stdio.h>
#if HAVE_STDIO_EXT_H
# include <stdio_ext.h>
#endif

#if !HAVE_DECL___FPENDING
size_t __fpending (FILE *) _GL_ATTRIBUTE_PURE;
#endif
