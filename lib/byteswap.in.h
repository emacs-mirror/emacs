/* byteswap.h - Byte swapping
   Copyright (C) 2005, 2007, 2009-2026 Free Software Foundation, Inc.
   Written by Oskar Liljeblad <oskar@osk.mine.nu>, 2005.

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

#ifndef _GL_BYTESWAP_H
#define _GL_BYTESWAP_H 1

/* This file uses _GL_INLINE.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

/* Define this now, rather than after including stdbit.h, in case stdbit.h
   recursively includes us via stdint.h.  This is for Gnulib endian.h.  */
#ifndef _GL_BYTESWAP_INLINE
# define _GL_BYTESWAP_INLINE _GL_INLINE
#endif

#include <stdbit.h> /* for stdc_memreverse8u* */
#include <stdint.h> /* for UINT_LEAST64_MAX */

_GL_INLINE_HEADER_BEGIN

#ifdef __cplusplus
extern "C" {
#endif

/* Given an unsigned 16-bit argument X, return the value corresponding to
   X with reversed byte order.  */
_GL_BYTESWAP_INLINE uint_least16_t
bswap_16 (uint_least16_t x)
{
  return stdc_memreverse8u16 (x);
}

/* Given an unsigned 32-bit argument X, return the value corresponding to
   X with reversed byte order.  */
_GL_BYTESWAP_INLINE uint_least32_t
bswap_32 (uint_least32_t x)
{
  return stdc_memreverse8u32 (x);
}

#ifdef UINT_LEAST64_MAX
/* Given an unsigned 64-bit argument X, return the value corresponding to
   X with reversed byte order.  */
_GL_BYTESWAP_INLINE uint_least64_t
bswap_64 (uint_least64_t x)
{
  return stdc_memreverse8u64 (x);
}
#endif

#ifdef __cplusplus
}
#endif

_GL_INLINE_HEADER_END

#endif /* _GL_BYTESWAP_H */
