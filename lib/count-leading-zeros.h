/* count-leading-zeros.h -- counts the number of leading 0 bits in a word.
   Copyright (C) 2012-2018 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Eric Blake.  */

#ifndef COUNT_LEADING_ZEROS_H
#define COUNT_LEADING_ZEROS_H 1

#include <limits.h>
#include <stdlib.h>

#ifndef _GL_INLINE_HEADER_BEGIN
 #error "Please include config.h first."
#endif
_GL_INLINE_HEADER_BEGIN
#ifndef COUNT_LEADING_ZEROS_INLINE
# define COUNT_LEADING_ZEROS_INLINE _GL_INLINE
#endif

/* Assuming the GCC builtin is BUILTIN and the MSC builtin is MSC_BUILTIN,
   expand to code that computes the number of leading zeros of the local
   variable 'x' of type TYPE (an unsigned integer type) and return it
   from the current function.  */
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
# define COUNT_LEADING_ZEROS(BUILTIN, MSC_BUILTIN, TYPE)                \
  return x ? BUILTIN (x) : CHAR_BIT * sizeof x;
#elif _MSC_VER
# pragma intrinsic _BitScanReverse
# pragma intrinsic _BitScanReverse64
# define COUNT_LEADING_ZEROS(BUILTIN, MSC_BUILTIN, TYPE)                \
    do                                                                  \
      {                                                                 \
        unsigned long result;                                           \
        return MSC_BUILTIN (&result, x) ? result : CHAR_BIT * sizeof x; \
      }                                                                 \
    while (0)
#else
# define COUNT_LEADING_ZEROS(BUILTIN, MSC_BUILTIN, TYPE)                \
    do                                                                  \
      {                                                                 \
        int count;                                                      \
        unsigned int leading_32;                                        \
        if (! x)                                                        \
          return CHAR_BIT * sizeof x;                                   \
        for (count = 0;                                                 \
             (leading_32 = ((x >> (sizeof (TYPE) * CHAR_BIT - 32))      \
                            & 0xffffffffU),                             \
              count < CHAR_BIT * sizeof x - 32 && !leading_32);         \
             count += 32)                                               \
          x = x << 31 << 1;                                             \
        return count + count_leading_zeros_32 (leading_32);             \
      }                                                                 \
    while (0)

/* Compute and return the number of leading zeros in X,
   where 0 < X < 2**32.  */
COUNT_LEADING_ZEROS_INLINE int
count_leading_zeros_32 (unsigned int x)
{
  /* <https://github.com/gibsjose/BitHacks>
     <http://www.fit.vutbr.cz/~ibarina/pub/bithacks.pdf> */
  static const char de_Bruijn_lookup[32] = {
    31, 22, 30, 21, 18, 10, 29, 2, 20, 17, 15, 13, 9, 6, 28, 1,
    23, 19, 11, 3, 16, 14, 7, 24, 12, 4, 8, 25, 5, 26, 27, 0
  };

  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return de_Bruijn_lookup[((x * 0x07c4acddU) & 0xffffffffU) >> 27];
}
#endif

/* Compute and return the number of leading zeros in X. */
COUNT_LEADING_ZEROS_INLINE int
count_leading_zeros (unsigned int x)
{
  COUNT_LEADING_ZEROS (__builtin_clz, _BitScanReverse, unsigned int);
}

/* Compute and return the number of leading zeros in X. */
COUNT_LEADING_ZEROS_INLINE int
count_leading_zeros_l (unsigned long int x)
{
  COUNT_LEADING_ZEROS (__builtin_clzl, _BitScanReverse, unsigned long int);
}

#if HAVE_UNSIGNED_LONG_LONG_INT
/* Compute and return the number of leading zeros in X. */
COUNT_LEADING_ZEROS_INLINE int
count_leading_zeros_ll (unsigned long long int x)
{
  COUNT_LEADING_ZEROS (__builtin_clzll, _BitScanReverse64,
                       unsigned long long int);
}
#endif

_GL_INLINE_HEADER_END

#endif /* COUNT_LEADING_ZEROS_H */
