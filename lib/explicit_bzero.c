/* Erasure of sensitive data, generic implementation.
   Copyright (C) 2016-2022 Free Software Foundation, Inc.

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

#include <string.h>

/* Set LEN bytes of S to 0.  The compiler will not delete a call to
   this function, even if S is dead after the call.  */
void
explicit_bzero (void *s, size_t len)
{
  memset_explicit (s, 0, len);
}
