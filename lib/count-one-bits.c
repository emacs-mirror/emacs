/* Count the number of 1-bits in a word.

   Copyright (C) 2012-2022 Free Software Foundation, Inc.

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

#include <config.h>

#define COUNT_ONE_BITS_INLINE _GL_EXTERN_INLINE
#include "count-one-bits.h"

#if 1500 <= _MSC_VER && (defined _M_IX86 || defined _M_X64)
int popcount_support = -1;
#endif
