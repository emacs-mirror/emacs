/* count-trailing-zeros.h -- counts the number of trailing 0 bits in a word.
   Copyright 2013-2022 Free Software Foundation, Inc.

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

/* Written by Paul Eggert.  */

#ifndef COUNT_TRAILING_ZEROS_H
#define COUNT_TRAILING_ZEROS_H 1

#include <limits.h>
#include <stdlib.h>

#ifndef _GL_INLINE_HEADER_BEGIN
 #error "Please include config.h first."
#endif
_GL_INLINE_HEADER_BEGIN
#ifndef COUNT_TRAILING_ZEROS_INLINE
# define COUNT_TRAILING_ZEROS_INLINE _GL_INLINE
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Assuming the GCC builtin is BUILTIN and the MSC builtin is MSC_BUILTIN,
   expand to code that computes the number of trailing zeros of the local
   variable 'x' of type TYPE (an unsigned integer type) and return it
   from the current function.  */
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4) \
    || (__clang_major__ >= 4)
# define COUNT_TRAILING_ZEROS(BUILTIN, MSC_BUILTIN, TYPE)               \
  return x ? BUILTIN (x) : CHAR_BIT * sizeof x;
#elif _MSC_VER
# pragma intrinsic _BitScanForward
# pragma intrinsic _BitScanForward64
# define COUNT_TRAILING_ZEROS(BUILTIN, MSC_BUILTIN, TYPE)               \
    do                                                                  \
      {                                                                 \
        unsigned long result;                                           \
        return MSC_BUILTIN (&result, x) ? result : CHAR_BIT * sizeof x; \
      }                                                                 \
    while (0)
#else
# define COUNT_TRAILING_ZEROS(BUILTIN, MSC_BUILTIN, TYPE)               \
    do                                                                  \
      {                                                                 \
        int count = 0;                                                  \
        if (! x)                                                        \
          return CHAR_BIT * sizeof x;                                   \
        for (count = 0;                                                 \
             (count < CHAR_BIT * sizeof x - 32                          \
              && ! (x & 0xffffffffU));                                  \
             count += 32)                                               \
          x = x >> 31 >> 1;                                             \
        return count + count_trailing_zeros_32 (x);                     \
      }                                                                 \
    while (0)

/* Compute and return the number of trailing zeros in the least
   significant 32 bits of X.  One of these bits must be nonzero.  */
COUNT_TRAILING_ZEROS_INLINE int
count_trailing_zeros_32 (unsigned int x)
{
  /* <https://github.com/gibsjose/BitHacks>
     <https://www.fit.vutbr.cz/~ibarina/pub/bithacks.pdf> */
  static const char de_Bruijn_lookup[32] = {
    0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
  };
  return de_Bruijn_lookup[(((x & -x) * 0x077cb531U) & 0xffffffffU) >> 27];
}
#endif

/* Compute and return the number of trailing zeros in X. */
COUNT_TRAILING_ZEROS_INLINE int
count_trailing_zeros (unsigned int x)
{
  COUNT_TRAILING_ZEROS (__builtin_ctz, _BitScanForward, unsigned int);
}

/* Compute and return the number of trailing zeros in X. */
COUNT_TRAILING_ZEROS_INLINE int
count_trailing_zeros_l (unsigned long int x)
{
  COUNT_TRAILING_ZEROS (__builtin_ctzl, _BitScanForward, unsigned long int);
}

/* Compute and return the number of trailing zeros in X. */
COUNT_TRAILING_ZEROS_INLINE int
count_trailing_zeros_ll (unsigned long long int x)
{
  COUNT_TRAILING_ZEROS (__builtin_ctzll, _BitScanForward64,
                        unsigned long long int);
}

#ifdef __cplusplus
}
#endif

_GL_INLINE_HEADER_END

#endif
