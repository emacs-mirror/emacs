/* stdc_count_ones_* functions.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

#define _GL_STDC_COUNT_ONES_INLINE _GL_EXTERN_INLINE
#include <config.h>
#include <stdbit.h>

#if 1500 <= _MSC_VER && (defined _M_IX86 || defined _M_X64)
signed char __gl_stdbit_popcount_support;
#endif
