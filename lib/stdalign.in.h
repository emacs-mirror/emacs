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

#ifndef _@GUARD_PREFIX@_STDALIGN_H

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

/* We need to include the system's <stdalign.h> when it exists, because it might
   define 'alignof' as a macro when it's not a keyword or compiler built-in.  */
#if @HAVE_STDALIGN_H@
/* The include_next requires a split double-inclusion guard.  */
# @INCLUDE_NEXT@ @NEXT_STDALIGN_H@
#endif

#ifndef _@GUARD_PREFIX@_STDALIGN_H
#define _@GUARD_PREFIX@_STDALIGN_H

#if (defined alignas \
     || (defined __STDC_VERSION__ && 202311 <= __STDC_VERSION__) \
     || (defined __cplusplus && (201103 <= __cplusplus || defined _MSC_VER)))
# define __alignas_is_defined 1
#endif

#define __alignof_is_defined 1

#endif /* _@GUARD_PREFIX@_STDALIGN_H */
#endif /* _@GUARD_PREFIX@_STDALIGN_H */
