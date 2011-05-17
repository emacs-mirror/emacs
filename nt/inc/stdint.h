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

#ifdef _WIN64
  typedef __int64 intptr_t;
#else
  typedef int intptr_t;
#endif

#define uintmax_t unsigned __int64

#endif	/* !__GNUC__ */

#endif /* _NT_STDINT_H_ */
