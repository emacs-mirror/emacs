/* Replacement stdint.h file for building GNU Emacs on Windows.

Copyright (C) 2011  Free Software Foundation, Inc.

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

#ifndef _NT_STDINT_H_
#define _NT_STDINT_H_

#ifdef __GNUC__
# include_next <stdint.h> /* use stdint.h if available */
#else	/* !__GNUC__ */

/* Minimum definitions to allow compilation with tool chains where
   stdint.h is not available, e.g. Microsoft Visual Studio.  */

typedef unsigned int uint32_t;
#define INT32_MAX 2147483647
/* "i64" is the non-standard suffix used by MSVC for 64-bit constants.  */
#define INT64_MAX 9223372036854775807i64

#ifdef _WIN64
  typedef __int64 intptr_t;
#define INTPTR_MAX INT64_MAX
#else
  typedef int intptr_t;
#define INTPTR_MAX INT32_MAX
#endif

#define uintmax_t unsigned __int64
#define PTRDIFF_MAX INTPTR_MAX

#endif	/* !__GNUC__ */

#endif /* _NT_STDINT_H_ */
