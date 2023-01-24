/* A substitute for ISO C11 <stdalign.h>.

   Copyright 2011-2023 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert and Bruno Haible.  */

/* Define two obsolescent C11 macros, assuming alignas and alignof are
   either keywords or alignasof-defined macros.  */

#ifndef _GL_STDALIGN_H
#define _GL_STDALIGN_H

#if (defined alignas \
     || (defined __STDC_VERSION__ && 202311 <= __STDC_VERSION__) \
     || (defined __cplusplus && (201103 <= __cplusplus || defined _MSC_VER)))
# define __alignas_is_defined 1
#endif

#define __alignof_is_defined 1

#endif /* _GL_STDALIGN_H */
