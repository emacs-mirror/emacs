/* Erasure of sensitive data, generic implementation.
   Copyright (C) 2016-2020 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */

/* An assembler implementation of explicit_bzero can be created as an
   assembler alias of an optimized bzero implementation.
   Architecture-specific implementations also need to define
   __explicit_bzero_chk.  */

#if !_LIBC
# include <config.h>
#endif

/* memset_s need this define */
#if HAVE_MEMSET_S
# define __STDC_WANT_LIB_EXT1__ 1
#endif

#include <string.h>

#if defined _WIN32 && !defined __CYGWIN__
# define  WIN32_LEAN_AND_MEAN
# include <windows.h>
#endif

#if _LIBC
/* glibc-internal users use __explicit_bzero_chk, and explicit_bzero
   redirects to that.  */
# undef explicit_bzero
#endif

/* Set LEN bytes of S to 0.  The compiler will not delete a call to
   this function, even if S is dead after the call.  */
void
explicit_bzero (void *s, size_t len)
{
#if defined _WIN32 && !defined __CYGWIN__
  (void) SecureZeroMemory (s, len);
#elif HAVE_EXPLICIT_MEMSET
  explicit_memset (s, '\0', len);
#elif HAVE_MEMSET_S
  (void) memset_s (s, len, '\0', len);
#else
  memset (s, '\0', len);
# if defined __GNUC__ && !defined __clang__
  /* Compiler barrier.  */
  asm volatile ("" ::: "memory");
# endif
#endif
}
