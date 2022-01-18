/* count-one-bits.h -- counts the number of 1-bits in a word.
   Copyright (C) 2007-2022 Free Software Foundation, Inc.

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

/* Written by Ben Pfaff.  */

#ifndef COUNT_ONE_BITS_H
#define COUNT_ONE_BITS_H 1

#include <limits.h>
#include <stdlib.h>

#ifndef _GL_INLINE_HEADER_BEGIN
 #error "Please include config.h first."
#endif
_GL_INLINE_HEADER_BEGIN
#ifndef COUNT_ONE_BITS_INLINE
# define COUNT_ONE_BITS_INLINE _GL_INLINE
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Assuming the GCC builtin is GCC_BUILTIN and the MSC builtin is MSC_BUILTIN,
   expand to code that computes the number of 1-bits of the local
   variable 'x' of type TYPE (an unsigned integer type) and return it
   from the current function.  */
#if (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)) \
    || (__clang_major__ >= 4)
# define COUNT_ONE_BITS(GCC_BUILTIN, MSC_BUILTIN, TYPE) \
    return GCC_BUILTIN (x)
#else

/* Compute and return the number of 1-bits set in the least
   significant 32 bits of X. */
COUNT_ONE_BITS_INLINE int
count_one_bits_32 (unsigned int x)
{
  x = ((x & 0xaaaaaaaaU) >> 1) + (x & 0x55555555U);
  x = ((x & 0xccccccccU) >> 2) + (x & 0x33333333U);
  x = (x >> 16) + (x & 0xffff);
  x = ((x & 0xf0f0) >> 4) + (x & 0x0f0f);
  return (x >> 8) + (x & 0x00ff);
}

/* Expand to code that computes the number of 1-bits of the local
   variable 'x' of type TYPE (an unsigned integer type) and return it
   from the current function.  */
# define COUNT_ONE_BITS_GENERIC(TYPE)                                   \
    do                                                                  \
      {                                                                 \
        int count = 0;                                                  \
        int bits;                                                       \
        for (bits = 0; bits < sizeof (TYPE) * CHAR_BIT; bits += 32)     \
          {                                                             \
            count += count_one_bits_32 (x);                             \
            x = x >> 31 >> 1;                                           \
          }                                                             \
        return count;                                                   \
      }                                                                 \
    while (0)

# if 1500 <= _MSC_VER && (defined _M_IX86 || defined _M_X64)

/* While gcc falls back to its own generic code if the machine
   on which it's running doesn't support popcount, with Microsoft's
   compiler we need to detect and fallback ourselves.  */

#  if 0
#   include <intrin.h>
#  else
    /* Don't pollute the namespace with too many MSVC intrinsics.  */
#   pragma intrinsic (__cpuid)
#   pragma intrinsic (__popcnt)
#   if defined _M_X64
#    pragma intrinsic (__popcnt64)
#   endif
#  endif

#  if !defined _M_X64
static inline __popcnt64 (unsigned long long x)
{
  return __popcnt ((unsigned int) (x >> 32)) + __popcnt ((unsigned int) x);
}
#  endif

/* Return nonzero if popcount is supported.  */

/* 1 if supported, 0 if not supported, -1 if unknown.  */
extern int popcount_support;

COUNT_ONE_BITS_INLINE int
popcount_supported (void)
{
  if (popcount_support < 0)
    {
      /* Do as described in
         <https://docs.microsoft.com/en-us/cpp/intrinsics/popcnt16-popcnt-popcnt64> */
      int cpu_info[4];
      __cpuid (cpu_info, 1);
      popcount_support = (cpu_info[2] >> 23) & 1;
    }
  return popcount_support;
}

#  define COUNT_ONE_BITS(GCC_BUILTIN, MSC_BUILTIN, TYPE) \
     do                                                  \
       {                                                 \
         if (popcount_supported ())                      \
           return MSC_BUILTIN (x);                       \
         else                                            \
           COUNT_ONE_BITS_GENERIC (TYPE);                \
       }                                                 \
     while (0)

# else

#  define COUNT_ONE_BITS(GCC_BUILTIN, MSC_BUILTIN, TYPE) \
     COUNT_ONE_BITS_GENERIC (TYPE)

# endif
#endif

/* Compute and return the number of 1-bits set in X. */
COUNT_ONE_BITS_INLINE int
count_one_bits (unsigned int x)
{
  COUNT_ONE_BITS (__builtin_popcount, __popcnt, unsigned int);
}

/* Compute and return the number of 1-bits set in X. */
COUNT_ONE_BITS_INLINE int
count_one_bits_l (unsigned long int x)
{
  COUNT_ONE_BITS (__builtin_popcountl, __popcnt, unsigned long int);
}

/* Compute and return the number of 1-bits set in X. */
COUNT_ONE_BITS_INLINE int
count_one_bits_ll (unsigned long long int x)
{
  COUNT_ONE_BITS (__builtin_popcountll, __popcnt64, unsigned long long int);
}

#ifdef __cplusplus
}
#endif

_GL_INLINE_HEADER_END

#endif /* COUNT_ONE_BITS_H */
