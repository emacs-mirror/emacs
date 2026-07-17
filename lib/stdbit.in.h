/* stdbit.h - C23 bit and byte utilities for non-C23 platforms

   Copyright 2024-2026 Free Software Foundation, Inc.

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

#ifndef _@GUARD_PREFIX@_STDBIT_H

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

/* The include_next requires a split double-inclusion guard.  */
#if @HAVE_STDBIT_H@
/* The FreeBSD 15.1 <stdbit.h> uses the _Bool type.  */
# if defined __FreeBSD__ && defined __cplusplus
#  define _Bool bool
# endif
# @INCLUDE_NEXT@ @NEXT_STDBIT_H@
#endif

#ifndef _@GUARD_PREFIX@_STDBIT_H
#define _@GUARD_PREFIX@_STDBIT_H

/* This file uses _GL_INLINE, WORDS_BIGENDIAN.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

/* If needed for APIs, get size_t, avoiding namespace pollution on GNU.  */
#if @GNULIB_STDC_MEMREVERSE8@ && !defined __STDC_VERSION_STDBIT_H__
# define __need_size_t
# include <stddef.h>
#endif

/* If needed for APIs, get intN_t, uintN_t, int_leastN_t,
   uint_leastN_t, and (for internal use) get equivalents of
   uint_fast{16,32,64}_t.  Avoid namespace pollution on GNU.  */
#if (@GNULIB_STDC_MEMREVERSE8U@ \
     || @GNULIB_STDC_LOAD8@ || @GNULIB_STDC_LOAD8_ALIGNED@ \
     || @GNULIB_STDC_STORE8@ || @GNULIB_STDC_STORE8_ALIGNED@)
# if !(defined __STDC_VERSION_STDBIT_H__ && defined __UINT_FAST64_TYPE__)
#  include <stdint.h>
#  define _GL_STDBIT_UINT_FAST16 uint_fast16_t
#  define _GL_STDBIT_UINT_FAST32 uint_fast32_t
#  define _GL_STDBIT_UINT_FAST64 uint_fast64_t
# else
#  define _GL_STDBIT_UINT_FAST16 __UINT_FAST16_TYPE__
#  define _GL_STDBIT_UINT_FAST32 __UINT_FAST32_TYPE__
#  define _GL_STDBIT_UINT_FAST64 __UINT_FAST64_TYPE__
# endif
#endif

#if @GNULIB_STDC_MEMREVERSE8U@ || @GNULIB_STDC_LOAD8_ALIGNED@ || @GNULIB_STDC_STORE8_ALIGNED@

/* Determine whether the compiler supports the __builtin_bswap{16,32,64}
   builtins.  */
# if defined __GNUC__ && 4 < __GNUC__ + (8 <= __GNUC_MINOR__)
#  define _GL_STDBIT_HAS_BUILTIN_BSWAP16 1
# elif defined __has_builtin
#  if __has_builtin (__builtin_bswap16)
#   define _GL_STDBIT_HAS_BUILTIN_BSWAP16 1
#  endif
# endif
# if defined __GNUC__ && 4 < __GNUC__ + (3 <= __GNUC_MINOR__)
#  define _GL_STDBIT_HAS_BUILTIN_BSWAP32 1
#  define _GL_STDBIT_HAS_BUILTIN_BSWAP64 1
# elif defined __has_builtin
#  if __has_builtin (__builtin_bswap32)
#   define _GL_STDBIT_HAS_BUILTIN_BSWAP32 1
#  endif
#  if __has_builtin (__builtin_bswap64)
#   define _GL_STDBIT_HAS_BUILTIN_BSWAP64 1
#  endif
# endif

#endif

#if @GNULIB_STDC_LOAD8_ALIGNED@ || @GNULIB_STDC_STORE8_ALIGNED@

/* Get memcpy, but keep namespace clean on GNU.  */
# ifdef __has_builtin
#  if __has_builtin (__builtin_memcpy)
#   define _GL_STDBIT_MEMCPY(dest, src, n) __builtin_memcpy (dest, src, n)
#  endif
# endif
# ifndef _GL_STDBIT_MEMCPY
#  include <string.h>
#  define _GL_STDBIT_MEMCPY(dest, src, n) memcpy (dest, src, n)
# endif

#endif

_GL_INLINE_HEADER_BEGIN

#ifndef _GL_STDC_LEADING_ZEROS_INLINE
# define _GL_STDC_LEADING_ZEROS_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_LEADING_ONES_INLINE
# define _GL_STDC_LEADING_ONES_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_TRAILING_ZEROS_INLINE
# define _GL_STDC_TRAILING_ZEROS_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_TRAILING_ONES_INLINE
# define _GL_STDC_TRAILING_ONES_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_FIRST_LEADING_ZERO_INLINE
# define _GL_STDC_FIRST_LEADING_ZERO_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_FIRST_LEADING_ONE_INLINE
# define _GL_STDC_FIRST_LEADING_ONE_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_FIRST_TRAILING_ZERO_INLINE
# define _GL_STDC_FIRST_TRAILING_ZERO_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_FIRST_TRAILING_ONE_INLINE
# define _GL_STDC_FIRST_TRAILING_ONE_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_COUNT_ZEROS_INLINE
# define _GL_STDC_COUNT_ZEROS_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_COUNT_ONES_INLINE
# define _GL_STDC_COUNT_ONES_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_HAS_SINGLE_BIT_INLINE
# define _GL_STDC_HAS_SINGLE_BIT_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_BIT_WIDTH_INLINE
# define _GL_STDC_BIT_WIDTH_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_BIT_FLOOR_INLINE
# define _GL_STDC_BIT_FLOOR_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_BIT_CEIL_INLINE
# define _GL_STDC_BIT_CEIL_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_ROTATE_LEFT_INLINE
# define _GL_STDC_ROTATE_LEFT_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_ROTATE_RIGHT_INLINE
# define _GL_STDC_ROTATE_RIGHT_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_MEMREVERSE8_INLINE
# define _GL_STDC_MEMREVERSE8_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_MEMREVERSE8U_INLINE
# define _GL_STDC_MEMREVERSE8U_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_LOAD8_ALIGNED_INLINE
# define _GL_STDC_LOAD8_ALIGNED_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_LOAD8_INLINE
# define _GL_STDC_LOAD8_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_STORE8_ALIGNED_INLINE
# define _GL_STDC_STORE8_ALIGNED_INLINE _GL_INLINE
#endif
#ifndef _GL_STDC_STORE8_INLINE
# define _GL_STDC_STORE8_INLINE _GL_INLINE
#endif

/* An expression, preferably with the type of A, that has the value of B.  */
#if ((defined __GNUC__ && 2 <= __GNUC__) \
     || (defined __clang_major__ && 4 <= __clang_major__) \
     || (defined __IBMC__ && 1210 <= __IBMC__ && defined __IBM__TYPEOF__) \
     || (defined __SUNPRO_C && 0x5110 <= __SUNPRO_C && !__STDC__) \
     || (defined _MSC_VER && 1939 <= _MSC_VER && !defined __cplusplus))
# define _GL_STDBIT_TYPEOF_CAST(a, b) ((__typeof__ (a)) (b))
#elif 202311 <= __STDC_VERSION__
# define _GL_STDBIT_TYPEOF_CAST(a, b) ((typeof (a)) (b))
#else
/* This platform is so old that it lacks typeof, so _Generic is likely
   missing or unreliable.  The C23 standard seems to allow yielding B
   (which is always unsigned long long int), so do that.  */
# define _GL_STDBIT_TYPEOF_CAST(a, b) (b)
#endif


#ifdef __cplusplus
extern "C" {
#endif


/* Some systems are only missing C2y features in stdbit.h.  */
#ifndef __STDC_VERSION_STDBIT_H__

/* ISO C 23 § 7.18.2 Endian  */

#define __STDC_ENDIAN_BIG__ 4321
#define __STDC_ENDIAN_LITTLE__ 1234
#ifdef WORDS_BIGENDIAN
# define __STDC_ENDIAN_NATIVE__ __STDC_ENDIAN_BIG__
#else
# define __STDC_ENDIAN_NATIVE__ __STDC_ENDIAN_LITTLE__
#endif

#endif /* !__STDC_VERSION_STDBIT_H__ */


/* Some systems are only missing C2y features in stdbit.h.  */
#ifndef __STDC_VERSION_STDBIT_H__

/* ISO C 23 § 7.18.3 Count Leading Zeros  */

#if @GNULIB_STDC_LEADING_ZEROS@

# if 3 < __GNUC__ + (4 <= __GNUC_MINOR__) || 4 <= __clang_major__
#  define _GL_STDBIT_HAS_BUILTIN_CLZ true
# elif defined __has_builtin
#  if (__has_builtin (__builtin_clz) \
       && __has_builtin (__builtin_clzl) \
       && __has_builtin (__builtin_clzll))
#   define _GL_STDBIT_HAS_BUILTIN_CLZ true
#  endif
# endif

/* Count leading 0 bits of N, even if N is 0.  */
# if !GNULIB_defined_clz_functions
#  ifdef _GL_STDBIT_HAS_BUILTIN_CLZ
_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clz (unsigned int n)
{
  return n ? __builtin_clz (n) : 8 * sizeof n;
}
_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clzl (unsigned long int n)
{
  return n ? __builtin_clzl (n) : 8 * sizeof n;
}
_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clzll (unsigned long long int n)
{
  return n ? __builtin_clzll (n) : 8 * sizeof n;
}
#  elif defined _MSC_VER

/* Declare the few MSVC intrinsics that we need.  We prefer not to include
   <intrin.h> because it would pollute the namespace.  */
extern unsigned char _BitScanReverse (unsigned long *, unsigned long);
#   pragma intrinsic (_BitScanReverse)
#   ifdef _M_X64
extern unsigned char _BitScanReverse64 (unsigned long *, unsigned long long);
#    pragma intrinsic (_BitScanReverse64)
#   endif

_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clzl (unsigned long int n)
{
  unsigned long int r;
  return 8 * sizeof n - (_BitScanReverse (&r, n) ? r + 1 : 0);
}
_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clz (unsigned int n)
{
  return _gl_stdbit_clzl (n) - 8 * (sizeof 0ul - sizeof n);
}
_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clzll (unsigned long long int n)
{
#   ifdef _M_X64
  unsigned long int r;
  return 8 * sizeof n - (_BitScanReverse64 (&r, n) ? r + 1 : 0);
#   else
  unsigned long int hi = n >> 32;
  return _gl_stdbit_clzl (hi ? hi : n) + (hi ? 0 : 32);
#   endif
}

#  else /* !_MSC_VER */

_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clzll (unsigned long long int n)
{
  int r = 0;
  for (int i = 8 * sizeof n >> 1; 1 << 6 <= i; i >>= 1)
    {
      int a = (1ull << i <= n) * i; n >>= a; r += a;
    }
  int a5 = (0x00000000ffffffff < n) << 5; n >>= a5; r += a5;
  int a4 = (0x000000000000ffff < n) << 4; n >>= a4; r += a4;
  int a3 = (0x00000000000000ff < n) << 3; n >>= a3; r += a3;
  int a2 = (0x000000000000000f < n) << 2; n >>= a2; r += a2;
  return (8 * sizeof n - (1 << 2) - r) + ((0x11112234ull >> (n << 2)) & 0xf);
}
_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clz (unsigned int n)
{
  return _gl_stdbit_clzll (n) - 8 * (sizeof 0ull - sizeof 0u);
}
_GL_STDC_LEADING_ZEROS_INLINE int
_gl_stdbit_clzl (unsigned long int n)
{
  return _gl_stdbit_clzll (n) - 8 * (sizeof 0ull - sizeof 0ul);
}
#  endif

#  define GNULIB_defined_clz_functions 1
# endif

# if !GNULIB_defined_stdc_leading_zeros_functions

_GL_STDC_LEADING_ZEROS_INLINE unsigned int
stdc_leading_zeros_ui (unsigned int n)
{
  return _gl_stdbit_clz (n);
}

_GL_STDC_LEADING_ZEROS_INLINE unsigned int
stdc_leading_zeros_uc (unsigned char n)
{
  return stdc_leading_zeros_ui (n) - 8 * (sizeof 0u - sizeof n);
}

_GL_STDC_LEADING_ZEROS_INLINE unsigned int
stdc_leading_zeros_us (unsigned short int n)
{
  return stdc_leading_zeros_ui (n) - 8 * (sizeof 0u - sizeof n);
}

_GL_STDC_LEADING_ZEROS_INLINE unsigned int
stdc_leading_zeros_ul (unsigned long int n)
{
  return _gl_stdbit_clzl (n);
}

_GL_STDC_LEADING_ZEROS_INLINE unsigned int
stdc_leading_zeros_ull (unsigned long long int n)
{
  return _gl_stdbit_clzll (n);
}

#  define GNULIB_defined_stdc_leading_zeros_functions 1
# endif

# define stdc_leading_zeros(n) \
  (sizeof (n) == 1 ? stdc_leading_zeros_uc (n)	\
   : sizeof (n) == sizeof (unsigned short int) ? stdc_leading_zeros_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_leading_zeros_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_leading_zeros_ul (n) \
   : stdc_leading_zeros_ull (n))

#endif


/* ISO C 23 § 7.18.4 Count Leading Ones  */

#if @GNULIB_STDC_LEADING_ONES@

# if !GNULIB_defined_stdc_leading_ones_functions

_GL_STDC_LEADING_ONES_INLINE unsigned int
stdc_leading_ones_uc (unsigned char n)
{
  return stdc_leading_zeros_uc (~n);
}

_GL_STDC_LEADING_ONES_INLINE unsigned int
stdc_leading_ones_us (unsigned short int n)
{
  return stdc_leading_zeros_us (~n);
}

_GL_STDC_LEADING_ONES_INLINE unsigned int
stdc_leading_ones_ui (unsigned int n)
{
  return stdc_leading_zeros_ui (~n);
}

_GL_STDC_LEADING_ONES_INLINE unsigned int
stdc_leading_ones_ul (unsigned long int n)
{
  return stdc_leading_zeros_ul (~n);
}

_GL_STDC_LEADING_ONES_INLINE unsigned int
stdc_leading_ones_ull (unsigned long long int n)
{
  return stdc_leading_zeros_ull (~n);
}

#  define GNULIB_defined_stdc_leading_ones_functions 1
# endif

# define stdc_leading_ones(n) \
  (sizeof (n) == 1 ? stdc_leading_ones_uc (n)	\
   : sizeof (n) == sizeof (unsigned short int) ? stdc_leading_ones_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_leading_ones_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_leading_ones_ul (n) \
   : stdc_leading_ones_ull (n))

#endif


/* ISO C 23 § 7.18.5 Count Trailing Zeros  */

#if @GNULIB_STDC_TRAILING_ZEROS@

# if 3 < __GNUC__ + (4 <= __GNUC_MINOR__) || 4 <= __clang_major__
#  define _GL_STDBIT_HAS_BUILTIN_CTZ true
# elif defined __has_builtin
#  if (__has_builtin (__builtin_ctz) \
       && __has_builtin (__builtin_ctzl) \
       && __has_builtin (__builtin_ctzll))
#   define _GL_STDBIT_HAS_BUILTIN_CTZ true
#  endif
# endif

/* Count trailing 0 bits of N, even if N is 0.  */
# if !GNULIB_defined_ctz_functions
#  ifdef _GL_STDBIT_HAS_BUILTIN_CTZ
_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctz (unsigned int n)
{
  return n ? __builtin_ctz (n) : 8 * sizeof n;
}
_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctzl (unsigned long int n)
{
  return n ? __builtin_ctzl (n) : 8 * sizeof n;
}
_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctzll (unsigned long long int n)
{
  return n ? __builtin_ctzll (n) : 8 * sizeof n;
}
#  elif defined _MSC_VER

/* Declare the few MSVC intrinsics that we need.  We prefer not to include
   <intrin.h> because it would pollute the namespace.  */
extern unsigned char _BitScanForward (unsigned long *, unsigned long);
#   pragma intrinsic (_BitScanForward)
#   ifdef _M_X64
extern unsigned char _BitScanForward64 (unsigned long *, unsigned long long);
#    pragma intrinsic (_BitScanForward64)
#   endif

_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctzl (unsigned long int n)
{
  unsigned long int r;
  return _BitScanForward (&r, n) ? r : 8 * sizeof n;
}
_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctz (unsigned int n)
{
  return _gl_stdbit_ctzl (n | (1ul << (8 * sizeof n - 1) << 1));
}
_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctzll (unsigned long long int n)
{
#   ifdef _M_X64
  unsigned long int r;
  return _BitScanForward64 (&r, n) ? r : 8 * sizeof n;
#   else
  unsigned int lo = n;
  return _gl_stdbit_ctzl (lo ? lo : n >> 32) + (lo ? 0 : 32);
#   endif
}

#  else /* !_MSC_VER */

_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctz (unsigned int n)
{
  return 8 * sizeof n - (n ? _gl_stdbit_clz (n & -n) + 1 : 0);
}
_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctzl (unsigned long int n)
{
  return 8 * sizeof n - (n ? _gl_stdbit_clzl (n & -n) + 1 : 0);
}
_GL_STDC_TRAILING_ZEROS_INLINE int
_gl_stdbit_ctzll (unsigned long long int n)
{
  return 8 * sizeof n - (n ? _gl_stdbit_clzll (n & -n) + 1 : 0);
}
#  endif

#  define GNULIB_defined_ctz_functions 1
# endif

# if !GNULIB_defined_stdc_trailing_zeros_functions

_GL_STDC_TRAILING_ZEROS_INLINE unsigned int
stdc_trailing_zeros_ui (unsigned int n)
{
  return _gl_stdbit_ctz (n);
}

_GL_STDC_TRAILING_ZEROS_INLINE unsigned int
stdc_trailing_zeros_uc (unsigned char n)
{
  return stdc_trailing_zeros_ui (n | (1 + (unsigned char) -1));
}

_GL_STDC_TRAILING_ZEROS_INLINE unsigned int
stdc_trailing_zeros_us (unsigned short int n)
{
  return stdc_trailing_zeros_ui (n | (1 + (unsigned short int) -1));
}

_GL_STDC_TRAILING_ZEROS_INLINE unsigned int
stdc_trailing_zeros_ul (unsigned long int n)
{
  return _gl_stdbit_ctzl (n);
}

_GL_STDC_TRAILING_ZEROS_INLINE unsigned int
stdc_trailing_zeros_ull (unsigned long long int n)
{
  return _gl_stdbit_ctzll (n);
}

#  define GNULIB_defined_stdc_trailing_zeros_functions 1
# endif

# define stdc_trailing_zeros(n) \
  (sizeof (n) == 1 ? stdc_trailing_zeros_uc (n)	\
   : sizeof (n) == sizeof (unsigned short int) ? stdc_trailing_zeros_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_trailing_zeros_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_trailing_zeros_ul (n) \
   : stdc_trailing_zeros_ull (n))

#endif


/* ISO C 23 § 7.18.6 Count Trailing Ones  */

#if @GNULIB_STDC_TRAILING_ONES@

# if !GNULIB_defined_stdc_trailing_ones_functions

_GL_STDC_TRAILING_ONES_INLINE unsigned int
stdc_trailing_ones_uc (unsigned char n)
{
  return stdc_trailing_zeros_uc (~n);
}

_GL_STDC_TRAILING_ONES_INLINE unsigned int
stdc_trailing_ones_us (unsigned short int n)
{
  return stdc_trailing_zeros_us (~n);
}

_GL_STDC_TRAILING_ONES_INLINE unsigned int
stdc_trailing_ones_ui (unsigned int n)
{
  return stdc_trailing_zeros_ui (~n);
}

_GL_STDC_TRAILING_ONES_INLINE unsigned int
stdc_trailing_ones_ul (unsigned long int n)
{
  return stdc_trailing_zeros_ul (~n);
}

_GL_STDC_TRAILING_ONES_INLINE unsigned int
stdc_trailing_ones_ull (unsigned long long int n)
{
  return stdc_trailing_zeros_ull (~n);
}

#  define GNULIB_defined_stdc_trailing_ones_functions 1
# endif

# define stdc_trailing_ones(n) \
  (sizeof (n) == 1 ? stdc_trailing_ones_uc (n)	\
   : sizeof (n) == sizeof (unsigned short int) ? stdc_trailing_ones_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_trailing_ones_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_trailing_ones_ul (n) \
   : stdc_trailing_ones_ull (n))

#endif


/* ISO C 23 § 7.18.7 First Leading Zero  */

#if @GNULIB_STDC_FIRST_LEADING_ZERO@

# if !GNULIB_defined_stdc_first_leading_zero_functions

_GL_STDC_FIRST_LEADING_ZERO_INLINE unsigned int
stdc_first_leading_zero_uc (unsigned char n)
{
  unsigned int count = stdc_leading_ones_uc (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_LEADING_ZERO_INLINE unsigned int
stdc_first_leading_zero_us (unsigned short int n)
{
  unsigned int count = stdc_leading_ones_us (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_LEADING_ZERO_INLINE unsigned int
stdc_first_leading_zero_ui (unsigned int n)
{
  unsigned int count = stdc_leading_ones_ui (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_LEADING_ZERO_INLINE unsigned int
stdc_first_leading_zero_ul (unsigned long int n)
{
  unsigned int count = stdc_leading_ones_ul (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_LEADING_ZERO_INLINE unsigned int
stdc_first_leading_zero_ull (unsigned long long int n)
{
  unsigned int count = stdc_leading_ones_ull (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

#  define GNULIB_defined_stdc_first_leading_zero_functions 1
# endif

# define stdc_first_leading_zero(n) \
  (sizeof (n) == 1 ? stdc_first_leading_zero_uc (n) \
   : sizeof (n) == sizeof (unsigned short) ? stdc_first_leading_zero_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_first_leading_zero_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_first_leading_zero_ul (n) \
   : stdc_first_leading_zero_ull (n))

#endif


/* ISO C 23 § 7.18.8 First Leading One  */

#if @GNULIB_STDC_FIRST_LEADING_ONE@

# if !GNULIB_defined_stdc_first_leading_one_functions

_GL_STDC_FIRST_LEADING_ONE_INLINE unsigned int
stdc_first_leading_one_uc (unsigned char n)
{
  unsigned int count = stdc_leading_zeros_uc (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_LEADING_ONE_INLINE unsigned int
stdc_first_leading_one_us (unsigned short int n)
{
  unsigned int count = stdc_leading_zeros_us (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_LEADING_ONE_INLINE unsigned int
stdc_first_leading_one_ui (unsigned int n)
{
  unsigned int count = stdc_leading_zeros_ui (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_LEADING_ONE_INLINE unsigned int
stdc_first_leading_one_ul (unsigned long int n)
{
  unsigned int count = stdc_leading_zeros_ul (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_LEADING_ONE_INLINE unsigned int
stdc_first_leading_one_ull (unsigned long long int n)
{
  unsigned int count = stdc_leading_zeros_ull (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

#  define GNULIB_defined_stdc_first_leading_one_functions 1
# endif

# define stdc_first_leading_one(n) \
  (sizeof (n) == 1 ? stdc_first_leading_one_uc (n) \
   : sizeof (n) == sizeof (unsigned short) ? stdc_first_leading_one_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_first_leading_one_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_first_leading_one_ul (n) \
   : stdc_first_leading_one_ull (n))

#endif


/* ISO C 23 § 7.18.9 First Trailing Zero  */

#if @GNULIB_STDC_FIRST_TRAILING_ZERO@

# if !GNULIB_defined_stdc_first_trailing_zero_functions

_GL_STDC_FIRST_TRAILING_ZERO_INLINE unsigned int
stdc_first_trailing_zero_uc (unsigned char n)
{
  unsigned int count = stdc_trailing_ones_uc (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_TRAILING_ZERO_INLINE unsigned int
stdc_first_trailing_zero_us (unsigned short int n)
{
  unsigned int count = stdc_trailing_ones_us (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_TRAILING_ZERO_INLINE unsigned int
stdc_first_trailing_zero_ui (unsigned int n)
{
  unsigned int count = stdc_trailing_ones_ui (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_TRAILING_ZERO_INLINE unsigned int
stdc_first_trailing_zero_ul (unsigned long int n)
{
  unsigned int count = stdc_trailing_ones_ul (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_TRAILING_ZERO_INLINE unsigned int
stdc_first_trailing_zero_ull (unsigned long long int n)
{
  unsigned int count = stdc_trailing_ones_ull (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

#  define GNULIB_defined_stdc_first_trailing_zero_functions 1
# endif

# define stdc_first_trailing_zero(n) \
  (sizeof (n) == 1 ? stdc_first_trailing_zero_uc (n) \
   : sizeof (n) == sizeof (unsigned short) ? stdc_first_trailing_zero_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_first_trailing_zero_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_first_trailing_zero_ul (n) \
   : stdc_first_trailing_zero_ull (n))

#endif


/* ISO C 23 § 7.18.10 First Trailing One  */

#if @GNULIB_STDC_FIRST_TRAILING_ONE@

# if !GNULIB_defined_stdc_first_trailing_one_functions

_GL_STDC_FIRST_TRAILING_ONE_INLINE unsigned int
stdc_first_trailing_one_uc (unsigned char n)
{
  unsigned int count = stdc_trailing_zeros_uc (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_TRAILING_ONE_INLINE unsigned int
stdc_first_trailing_one_us (unsigned short int n)
{
  unsigned int count = stdc_trailing_zeros_us (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_TRAILING_ONE_INLINE unsigned int
stdc_first_trailing_one_ui (unsigned int n)
{
  unsigned int count = stdc_trailing_zeros_ui (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_TRAILING_ONE_INLINE unsigned int
stdc_first_trailing_one_ul (unsigned long int n)
{
  unsigned int count = stdc_trailing_zeros_ul (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

_GL_STDC_FIRST_TRAILING_ONE_INLINE unsigned int
stdc_first_trailing_one_ull (unsigned long long int n)
{
  unsigned int count = stdc_trailing_zeros_ull (n);
  unsigned int bits = 8 * sizeof n;
  return count % bits + (count < bits);
}

#  define GNULIB_defined_stdc_first_trailing_one_functions 1
# endif

# define stdc_first_trailing_one(n) \
  (sizeof (n) == 1 ? stdc_first_trailing_one_uc (n) \
   : sizeof (n) == sizeof (unsigned short) ? stdc_first_trailing_one_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_first_trailing_one_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_first_trailing_one_ul (n) \
   : stdc_first_trailing_one_ull (n))

#endif


/* ISO C 23 § 7.18.12 Count Ones  */

#if @GNULIB_STDC_COUNT_ONES@

# if 3 < __GNUC__ + (4 <= __GNUC_MINOR__) || 4 <= __clang_major__
#  define _GL_STDBIT_HAS_BUILTIN_POPCOUNT true
# elif defined __has_builtin
#  if (__has_builtin (__builtin_popcount) \
       && __has_builtin (__builtin_popcountl) \
       && __has_builtin (__builtin_popcountll))
#   define _GL_STDBIT_HAS_BUILTIN_POPCOUNT true
#  endif
# endif

/* Count 1 bits in N.  */
# if !GNULIB_defined_popcount_functions
#  ifdef _GL_STDBIT_HAS_BUILTIN_POPCOUNT
#   define _gl_stdbit_popcount __builtin_popcount
#   define _gl_stdbit_popcountl __builtin_popcountl
#   define _gl_stdbit_popcountll __builtin_popcountll
#  else
_GL_STDC_COUNT_ONES_INLINE int
_gl_stdbit_popcount_wide (unsigned long long int n)
{
  if (sizeof n & (sizeof n - 1))
    {
      /* Use a simple O(log N) loop on theoretical platforms where N's
         width is not a power of 2.  */
      int count = 0;
      for (int i = 0; i < 8 * sizeof n; i++, n >>= 1)
        count += n & 1;
      return count;
    }
  else
    {
      /* N's width is a power of 2; count in parallel.  */
      unsigned long long int
        max = -1ull,
        x555555 = max / (1 << 1 | 1),	/* 0x555555... */
        x333333 = max / (1 << 2 | 1),	/* 0x333333... */
        x0f0f0f = max / (1 << 4 | 1),	/* 0x0f0f0f... */
        x010101 = max / ((1 << 8) - 1),	/* 0x010101... */
        x000_7f = max / 0xffffffffffffffffLL * 0x7f; /* 0x000000000000007f... */
      n -= (n >> 1) & x555555;
      n = (n & x333333) + ((n >> 2) & x333333);
      n = (n + (n >> 4)) & x0f0f0f;

      /* If the popcount always fits in 8 bits, multiply so that the
         popcount is in the leading 8 bits of the product; these days
         this is typically faster than the alternative below.  */
      if (8 * sizeof n < 1 << 8)
        return n * x010101 >> 8 * (sizeof n - 1);

      /* N is at least 256 bits wide!  Fall back on an O(log log N)
         loop that a compiler could unroll.  Unroll the first three
         iterations by hand, to skip some division and masking.  This
         is the most we can easily do without hassling with constants
         that a typical-platform compiler would reject.  */
      n += n >> (1 << 3);
      n += n >> (1 << 4);
      n += n >> (1 << 5);
      n &= x000_7f;
      for (int i = 64; i < 8 * sizeof n; i <<= 1)
        n = (n + (n >> i)) & max / (1ull << i | 1);
      return n;
    }
}

#   ifdef _MSC_VER
#    if 1500 <= _MSC_VER && (defined _M_IX86 || defined _M_X64)
/* Declare the few MSVC intrinsics that we need.  We prefer not to include
   <intrin.h> because it would pollute the namespace.  */
extern void __cpuid (int[4], int);
#     pragma intrinsic (__cpuid)
extern unsigned int __popcnt (unsigned int);
#     pragma intrinsic (__popcnt)
#     ifdef _M_X64
extern unsigned long long __popcnt64 (unsigned long long);
#      pragma intrinsic (__popcnt64)
#     else
_GL_STDC_COUNT_ONES_INLINE int
__popcnt64 (unsigned long long int n)
{
  return __popcnt (n >> 32) + __popcnt (n);
}
#     endif
#    endif

/* 1 if supported, -1 if not, 0 if unknown.  */
extern signed char _gl_stdbit_popcount_support;

_GL_STDC_COUNT_ONES_INLINE bool
_gl_stdbit_popcount_supported (void)
{
  if (!_gl_stdbit_popcount_support)
    {
      /* Do as described in
         <https://docs.microsoft.com/en-us/cpp/intrinsics/popcnt16-popcnt-popcnt64>
         Although Microsoft started requiring POPCNT in MS-Windows 11 24H2,
         we'll be more cautious.  */
      int cpu_info[4];
      __cpuid (cpu_info, 1);
      _gl_stdbit_popcount_support = cpu_info[2] & 1 << 23 ? 1 : -1;
    }
  return 0 < _gl_stdbit_popcount_support;
}
_GL_STDC_COUNT_ONES_INLINE int
_gl_stdbit_popcount (unsigned int n)
{
  return (_gl_stdbit_popcount_supported ()
          ? __popcnt (n)
          : _gl_stdbit_popcount_wide (n));
}
_GL_STDC_COUNT_ONES_INLINE int
_gl_stdbit_popcountl (unsigned long int n)
{
  return (_gl_stdbit_popcount_supported ()
          ? __popcnt (n)
          : _gl_stdbit_popcount_wide (n));
}
_GL_STDC_COUNT_ONES_INLINE int
_gl_stdbit_popcountll (unsigned long long int n)
{
  return (_gl_stdbit_popcount_supported ()
          ? __popcnt64 (n)
          : _gl_stdbit_popcount_wide (n));
}
#   else /* !_MSC_VER */
#    define _gl_stdbit_popcount _gl_stdbit_popcount_wide
#    define _gl_stdbit_popcountl _gl_stdbit_popcount_wide
#    define _gl_stdbit_popcountll _gl_stdbit_popcount_wide
#   endif
#  endif
#  define GNULIB_defined_popcount_functions 1
# endif

# if !GNULIB_defined_stdc_count_ones_functions

_GL_STDC_COUNT_ONES_INLINE unsigned int
stdc_count_ones_ui (unsigned int n)
{
  return _gl_stdbit_popcount (n);
}

_GL_STDC_COUNT_ONES_INLINE unsigned int
stdc_count_ones_uc (unsigned char n)
{
  return stdc_count_ones_ui (n);
}

_GL_STDC_COUNT_ONES_INLINE unsigned int
stdc_count_ones_us (unsigned short int n)
{
  return stdc_count_ones_ui (n);
}

_GL_STDC_COUNT_ONES_INLINE unsigned int
stdc_count_ones_ul (unsigned long int n)
{
  return _gl_stdbit_popcountl (n);
}

_GL_STDC_COUNT_ONES_INLINE unsigned int
stdc_count_ones_ull (unsigned long long int n)
{
  return _gl_stdbit_popcountll (n);
}

#  define GNULIB_defined_stdc_count_ones_functions 1
# endif

# define stdc_count_ones(n) \
  (sizeof (n) == 1 ? stdc_count_ones_uc (n) \
   : sizeof (n) == sizeof (unsigned short int) ? stdc_count_ones_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_count_ones_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_count_ones_ul (n) \
   : stdc_count_ones_ull (n))

#endif


/* ISO C 23 § 7.18.11 Count Zeros  */

#if @GNULIB_STDC_COUNT_ZEROS@

# if !GNULIB_defined_stdc_count_zeros_functions

_GL_STDC_COUNT_ZEROS_INLINE unsigned int
stdc_count_zeros_uc (unsigned char n)
{
  return stdc_count_ones_uc (~n);
}

_GL_STDC_COUNT_ZEROS_INLINE unsigned int
stdc_count_zeros_us (unsigned short int n)
{
  return stdc_count_ones_us (~n);
}

_GL_STDC_COUNT_ZEROS_INLINE unsigned int
stdc_count_zeros_ui (unsigned int n)
{
  return stdc_count_ones_ui (~n);
}

_GL_STDC_COUNT_ZEROS_INLINE unsigned int
stdc_count_zeros_ul (unsigned long int n)
{
  return stdc_count_ones_ul (~n);
}

_GL_STDC_COUNT_ZEROS_INLINE unsigned int
stdc_count_zeros_ull (unsigned long long int n)
{
  return stdc_count_ones_ull (~n);
}

#  define GNULIB_defined_stdc_count_zeros_functions 1
# endif

# define stdc_count_zeros(n) \
  (sizeof (n) == 1 ? stdc_count_zeros_uc (n) \
   : sizeof (n) == sizeof (unsigned short int) ? stdc_count_zeros_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_count_zeros_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_count_zeros_ul (n) \
   : stdc_count_zeros_ull (n))

#endif


/* ISO C 23 § 7.18.13 Single-bit Check  */

#if @GNULIB_STDC_HAS_SINGLE_BIT@

# if !GNULIB_defined_stdc_has_single_bit_functions

_GL_STDC_HAS_SINGLE_BIT_INLINE bool
stdc_has_single_bit_uc (unsigned char n)
{
  unsigned char n_1 = n - 1, nx = n_1 ^ n;
  return n_1 < nx;
}

_GL_STDC_HAS_SINGLE_BIT_INLINE bool
stdc_has_single_bit_us (unsigned short int n)
{
  unsigned short int n_1 = n - 1, nx = n_1 ^ n;
  return n_1 < nx;
}

_GL_STDC_HAS_SINGLE_BIT_INLINE bool
stdc_has_single_bit_ui (unsigned int n)
{
  unsigned int n_1 = n - 1, nx = n_1 ^ n;
  return n_1 < nx;
}

_GL_STDC_HAS_SINGLE_BIT_INLINE bool
stdc_has_single_bit_ul (unsigned long int n)
{
  unsigned long int n_1 = n - 1, nx = n_1 ^ n;
  return n_1 < nx;
}

_GL_STDC_HAS_SINGLE_BIT_INLINE bool
stdc_has_single_bit_ull (unsigned long long int n)
{
  unsigned long long int n_1 = n - 1, nx = n_1 ^ n;
  return n_1 < nx;
}

#  define GNULIB_defined_stdc_has_single_bit_functions 1
# endif

# define stdc_has_single_bit(n) \
  ((bool) \
   (sizeof (n) == 1 ? stdc_has_single_bit_uc (n) \
    : sizeof (n) == sizeof (unsigned short int) ? stdc_has_single_bit_us (n) \
    : sizeof (n) == sizeof 0u ? stdc_has_single_bit_ui (n) \
    : sizeof (n) == sizeof 0ul ? stdc_has_single_bit_ul (n) \
    : stdc_has_single_bit_ull (n)))

#endif


/* ISO C 23 § 7.18.14 Bit Width  */

#if @GNULIB_STDC_BIT_WIDTH@

# if !GNULIB_defined_stdc_bit_width_functions

_GL_STDC_BIT_WIDTH_INLINE unsigned int
stdc_bit_width_uc (unsigned char n)
{
  return 8 * sizeof n - stdc_leading_zeros_uc (n);
}

_GL_STDC_BIT_WIDTH_INLINE unsigned int
stdc_bit_width_us (unsigned short int n)
{
  return 8 * sizeof n - stdc_leading_zeros_us (n);
}

_GL_STDC_BIT_WIDTH_INLINE unsigned int
stdc_bit_width_ui (unsigned int n)
{
  return 8 * sizeof n - stdc_leading_zeros_ui (n);
}

_GL_STDC_BIT_WIDTH_INLINE unsigned int
stdc_bit_width_ul (unsigned long int n)
{
  return 8 * sizeof n - stdc_leading_zeros_ul (n);
}

_GL_STDC_BIT_WIDTH_INLINE unsigned int
stdc_bit_width_ull (unsigned long long int n)
{
  return 8 * sizeof n - stdc_leading_zeros_ull (n);
}

#  define GNULIB_defined_stdc_bit_width_functions 1
# endif

# define stdc_bit_width(n) \
  (sizeof (n) == 1 ? stdc_bit_width_uc (n) \
   : sizeof (n) == sizeof (unsigned short int) ? stdc_bit_width_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_bit_width_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_bit_width_ul (n) \
   : stdc_bit_width_ull (n))

#endif

#endif /* !__STDC_VERSION_STDBIT_H__ */


/* ISO C 23 § 7.18.15 Bit Floor  */

#if @GNULIB_STDC_BIT_FLOOR@

# if !defined __STDC_VERSION_STDBIT_H__
#  if !GNULIB_defined_stdc_bit_floor_functions

_GL_STDC_BIT_FLOOR_INLINE unsigned char
stdc_bit_floor_uc (unsigned char n)
{
  return n ? 1u << (stdc_bit_width_uc (n) - 1) : 0;
}

_GL_STDC_BIT_FLOOR_INLINE unsigned short int
stdc_bit_floor_us (unsigned short int n)
{
  return n ? 1u << (stdc_bit_width_us (n) - 1) : 0;
}

_GL_STDC_BIT_FLOOR_INLINE unsigned int
stdc_bit_floor_ui (unsigned int n)
{
  return n ? 1u << (stdc_bit_width_ui (n) - 1) : 0;
}

_GL_STDC_BIT_FLOOR_INLINE unsigned long int
stdc_bit_floor_ul (unsigned long int n)
{
  return n ? 1ul << (stdc_bit_width_ul (n) - 1) : 0;
}

_GL_STDC_BIT_FLOOR_INLINE unsigned long long int
stdc_bit_floor_ull (unsigned long long int n)
{
  return n ? 1ull << (stdc_bit_width_ull (n) - 1) : 0;
}

#   define GNULIB_defined_stdc_bit_floor_functions 1
#  endif
# endif

# if !defined __STDC_VERSION_STDBIT_H__ \
     || (defined __sun && defined _SYS_STDBIT_H)
#  undef stdc_bit_floor
#  define stdc_bit_floor(n) \
     (_GL_STDBIT_TYPEOF_CAST \
      (n, \
       (sizeof (n) == 1 ? stdc_bit_floor_uc (n) \
        : sizeof (n) == sizeof (unsigned short int) ? stdc_bit_floor_us (n) \
        : sizeof (n) == sizeof 0u ? stdc_bit_floor_ui (n) \
        : sizeof (n) == sizeof 0ul ? stdc_bit_floor_ul (n) \
        : stdc_bit_floor_ull (n))))
# endif

#endif


/* ISO C 23 § 7.18.16 Bit Ceiling  */

#if @GNULIB_STDC_BIT_CEIL@

# if !defined __STDC_VERSION_STDBIT_H__
#  if !GNULIB_defined_stdc_bit_ceil_functions

_GL_STDC_BIT_CEIL_INLINE unsigned char
stdc_bit_ceil_uc (unsigned char n)
{
  return n <= 1 ? 1 : 2u << (stdc_bit_width_uc (n - 1) - 1);
}

_GL_STDC_BIT_CEIL_INLINE unsigned short int
stdc_bit_ceil_us (unsigned short int n)
{
  return n <= 1 ? 1 : 2u << (stdc_bit_width_us (n - 1) - 1);
}

_GL_STDC_BIT_CEIL_INLINE unsigned int
stdc_bit_ceil_ui (unsigned int n)
{
  return n <= 1 ? 1 : 2u << (stdc_bit_width_ui (n - 1) - 1);
}

_GL_STDC_BIT_CEIL_INLINE unsigned long int
stdc_bit_ceil_ul (unsigned long int n)
{
  return n <= 1 ? 1 : 2ul << (stdc_bit_width_ul (n - 1) - 1);
}

_GL_STDC_BIT_CEIL_INLINE unsigned long long int
stdc_bit_ceil_ull (unsigned long long int n)
{
  return n <= 1 ? 1 : 2ull << (stdc_bit_width_ull (n - 1) - 1);
}

#   define GNULIB_defined_stdc_bit_ceil_functions 1
#  endif
# endif

# if !defined __STDC_VERSION_STDBIT_H__ \
     || (defined __sun && defined _SYS_STDBIT_H)
#  undef stdc_bit_ceil
#  define stdc_bit_ceil(n) \
     (_GL_STDBIT_TYPEOF_CAST \
      (n, \
       (sizeof (n) == 1 ? stdc_bit_ceil_uc (n) \
        : sizeof (n) == sizeof (unsigned short int) ? stdc_bit_ceil_us (n) \
        : sizeof (n) == sizeof 0u ? stdc_bit_ceil_ui (n) \
        : sizeof (n) == sizeof 0ul ? stdc_bit_ceil_ul (n) \
        : stdc_bit_ceil_ull (n))))
# endif

#endif


/* ISO C2y § 7.18.17 Rotate Left  */

#if @GNULIB_STDC_ROTATE_LEFT@

# ifdef __has_builtin
#  if __has_builtin (__builtin_stdc_rotate_left)
#   define _gl_stdc_rotate_left __builtin_stdc_rotate_left
#   define stdc_rotate_left __builtin_stdc_rotate_left
#  endif
# endif

# ifndef _gl_stdc_rotate_left
#  define _gl_stdc_rotate_left(v, c)                \
  (((v) << ((c) & (sizeof (v) * 8 - 1)))            \
   | ((v) >> (-(c) & (sizeof (v) * 8 - 1))))
# endif

# if !GNULIB_defined_stdc_rotate_left_functions

_GL_STDC_ROTATE_LEFT_INLINE unsigned char
stdc_rotate_left_uc (unsigned char v, unsigned int c)
{
  return _gl_stdc_rotate_left (v, c);
}

_GL_STDC_ROTATE_LEFT_INLINE unsigned short int
stdc_rotate_left_us (unsigned short int v, unsigned int c)
{
  return _gl_stdc_rotate_left (v, c);
}

_GL_STDC_ROTATE_LEFT_INLINE unsigned int
stdc_rotate_left_ui (unsigned int v, unsigned int c)
{
  return _gl_stdc_rotate_left (v, c);
}

_GL_STDC_ROTATE_LEFT_INLINE unsigned long int
stdc_rotate_left_ul (unsigned long int v, unsigned int c)
{
  return _gl_stdc_rotate_left (v, c);
}

_GL_STDC_ROTATE_LEFT_INLINE unsigned long long int
stdc_rotate_left_ull (unsigned long long int v, unsigned int c)
{
  return _gl_stdc_rotate_left (v, c);
}

#  define GNULIB_defined_stdc_rotate_left_functions 1
# endif

# ifndef stdc_rotate_left
#  define stdc_rotate_left(v, c)                                        \
  (_GL_STDBIT_TYPEOF_CAST                                               \
   (v,                                                                  \
    (sizeof (v) == 1 ? stdc_rotate_left_uc (v, c)                       \
     : sizeof (v) == sizeof (unsigned short int) ? stdc_rotate_left_us (v, c) \
     : sizeof (v) == sizeof 0u ? stdc_rotate_left_ui (v, c)             \
     : sizeof (v) == sizeof 0ul ? stdc_rotate_left_ul (v, c)            \
     : stdc_rotate_left_ull (v, c))))
# endif

#endif


/* ISO C2y § 7.18.18 Rotate Right  */

#if @GNULIB_STDC_ROTATE_RIGHT@

# ifdef __has_builtin
#  if __has_builtin (__builtin_stdc_rotate_right)
#   define _gl_stdc_rotate_right __builtin_stdc_rotate_right
#   define stdc_rotate_right __builtin_stdc_rotate_right
#  endif
# endif

# ifndef _gl_stdc_rotate_right
#  define _gl_stdc_rotate_right(v, c)               \
  (((v) >> ((c) & (sizeof (v) * 8 - 1)))            \
   | ((v) << (-(c) & (sizeof (v) * 8 - 1))))
# endif

# if !GNULIB_defined_stdc_rotate_right_functions

_GL_STDC_ROTATE_RIGHT_INLINE unsigned char
stdc_rotate_right_uc (unsigned char v, unsigned int c)
{
  return _gl_stdc_rotate_right (v, c);
}

_GL_STDC_ROTATE_RIGHT_INLINE unsigned short int
stdc_rotate_right_us (unsigned short int v, unsigned int c)
{
  return _gl_stdc_rotate_right (v, c);
}

_GL_STDC_ROTATE_RIGHT_INLINE unsigned int
stdc_rotate_right_ui (unsigned int v, unsigned int c)
{
  return _gl_stdc_rotate_right (v, c);
}

_GL_STDC_ROTATE_RIGHT_INLINE unsigned long int
stdc_rotate_right_ul (unsigned long int v, unsigned int c)
{
  return _gl_stdc_rotate_right (v, c);
}

_GL_STDC_ROTATE_RIGHT_INLINE unsigned long long int
stdc_rotate_right_ull (unsigned long long int v, unsigned int c)
{
  return _gl_stdc_rotate_right (v, c);
}

#  define GNULIB_defined_stdc_rotate_right_functions 1
# endif

# ifndef stdc_rotate_right
#  define stdc_rotate_right(v, c)                                       \
  (_GL_STDBIT_TYPEOF_CAST                                               \
   (v,                                                                  \
    (sizeof (v) == 1 ? stdc_rotate_right_uc (v, c)                      \
     : sizeof (v) == sizeof (unsigned short int) ? stdc_rotate_right_us (v, c) \
     : sizeof (v) == sizeof 0u ? stdc_rotate_right_ui (v, c)            \
     : sizeof (v) == sizeof 0ul ? stdc_rotate_right_ul (v, c)           \
     : stdc_rotate_right_ull (v, c))))
# endif

#endif


/* ISO C2y § 7.18.19 8-bit Memory Reversal  */

#if @GNULIB_STDC_MEMREVERSE8@

# if !GNULIB_defined_stdc_memreverse8

_GL_STDC_MEMREVERSE8_INLINE void
stdc_memreverse8 (size_t n, unsigned char *ptr)
{
  if (n > 0)
    {
      /* There is no need to optimize the cases N == 1, N == 2, N == 4
         specially using __builtin_constant_p, because GCC does the possible
         optimizations already, taking into account the alignment of PTR:
         GCC >= 3 for N == 1, GCC >= 8 for N == 2, GCC >= 13 for N == 4.
         (Whereas clang >= 3, <= 22 optimizes only the case N == 1.)  */
      size_t i, j;
      for (i = 0, j = n-1; i < j; i++, j--)
        {
          unsigned char xi = ptr[i];
          unsigned char xj = ptr[j];
          ptr[j] = xi;
          ptr[i] = xj;
        }
    }
}

#  define GNULIB_defined_stdc_memreverse8 1
# endif

#endif


/* ISO C2y § 7.18.20 Exact-width 8-bit Memory Reversal  */

#if @GNULIB_STDC_MEMREVERSE8U@

/* Note: ISO C defines these functions with argument and return type uintN_t.
   We do it here with argument and return type uint_leastN_t.  This is a
   generalization that does not contradict ISO C: When uintN_t exists, it is
   known that uint_leastN_t is the same type as uintN_t.  */

# if !GNULIB_defined_stdc_memreverse8u_functions

_GL_STDC_MEMREVERSE8U_INLINE uint_least8_t
stdc_memreverse8u8 (uint_least8_t value)
{
  return value;
}

_GL_STDC_MEMREVERSE8U_INLINE uint_least16_t
stdc_memreverse8u16 (uint_least16_t value)
{
#  ifdef _GL_STDBIT_HAS_BUILTIN_BSWAP16
  return __builtin_bswap16 (value);
#  else
  _GL_STDBIT_UINT_FAST16 mask = 0xFFU;
  return (  (value & (mask << (8 * 1))) >> (8 * 1)
          | (value & (mask << (8 * 0))) << (8 * 1));
#  endif
}

_GL_STDC_MEMREVERSE8U_INLINE uint_least32_t
stdc_memreverse8u32 (uint_least32_t value)
{
#  ifdef _GL_STDBIT_HAS_BUILTIN_BSWAP32
  return __builtin_bswap32 (value);
#  else
  _GL_STDBIT_UINT_FAST32 mask = 0xFFU;
  return (  (value & (mask << (8 * 3))) >> (8 * 3)
          | (value & (mask << (8 * 2))) >> (8 * 1)
          | (value & (mask << (8 * 1))) << (8 * 1)
          | (value & (mask << (8 * 0))) << (8 * 3));
#  endif
}

_GL_STDC_MEMREVERSE8U_INLINE uint_least64_t
stdc_memreverse8u64 (uint_least64_t value)
{
#  ifdef _GL_STDBIT_HAS_BUILTIN_BSWAP64
  return __builtin_bswap64 (value);
#  else
  _GL_STDBIT_UINT_FAST64 mask = 0xFFU;
  return (  (value & (mask << (8 * 7))) >> (8 * 7)
          | (value & (mask << (8 * 6))) >> (8 * 5)
          | (value & (mask << (8 * 5))) >> (8 * 3)
          | (value & (mask << (8 * 4))) >> (8 * 1)
          | (value & (mask << (8 * 3))) << (8 * 1)
          | (value & (mask << (8 * 2))) << (8 * 3)
          | (value & (mask << (8 * 1))) << (8 * 5)
          | (value & (mask << (8 * 0))) << (8 * 7));
#  endif
}

#  define GNULIB_defined_stdc_memreverse8u_functions 1
# endif

#endif


/* ISO C2y § 7.18.21 Endian-Aware 8-Bit Load  */

/* On hosts where _GL_STDBIT_OPTIMIZE_VIA_MEMCPY (see below) might be useful,
   we need to avoid type-punning, because the compiler's aliasing
   analysis would frequently produce incorrect code, and requiring the
   option '-fno-strict-aliasing' is no viable solution.
   So, this definition won't work:

     uint_least16_t
     load16 (const unsigned char ptr[2])
     {
       return *(const uint_least16_t *)ptr;
     }

   Instead, the following definitions are candidates:

     // Trick from Lasse Collin: use memcpy and __builtin_assume_aligned.
     uint_least16_t
     load16_a (const unsigned char ptr[2])
     {
       uint_least16_t value;
       memcpy (&value, __builtin_assume_aligned (ptr, 2), 2);
       return value;
     }

     // Use __builtin_assume_aligned, without memcpy.
     uint_least16_t
     load16_b (const unsigned char ptr[2])
     {
       const unsigned char *aptr =
         (const unsigned char *) __builtin_assume_aligned (ptr, 2);
       return (_GL_STDBIT_BIGENDIAN
               ? ((uint_least16_t) aptr [0] << 8) | (uint_least16_t) aptr [1]
               : (uint_least16_t) aptr [0] | ((uint_least16_t) aptr [1] << 8));
     }

     // Use memcpy and __assume.
     uint_least16_t
     load16_c (const unsigned char ptr[2])
     {
       __assume (((uintptr_t) ptr & (2 - 1)) == 0);
       uint_least16_t value;
       memcpy (&value, __builtin_assume_aligned (ptr, 2), 2);
       return value;
     }

     // Use __assume, without memcpy.
     uint_least16_t
     load16_d (const unsigned char ptr[2])
     {
       __assume (((uintptr_t) ptr & (2 - 1)) == 0);
       return (_GL_STDBIT_BIGENDIAN
               ? ((uint_least16_t) ptr [0] << 8) | (uint_least16_t) ptr [1]
               : (uint_least16_t) ptr [0] | ((uint_least16_t) ptr [1] << 8));
     }

     // Use memcpy, without __builtin_assume_aligned or __assume.
     uint_least16_t
     load16_e (const unsigned char ptr[2])
     {
       uint_least16_t value;
       memcpy (&value, ptr, 2);
       return value;
     }

     // Use the code for the unaligned case.
     uint_least16_t
     load16_f (const unsigned char ptr[2])
     {
       return (_GL_STDBIT_BIGENDIAN
               ? ((uint_least16_t) ptr [0] << 8) | (uint_least16_t) ptr [1]
               : (uint_least16_t) ptr [0] | ((uint_least16_t) ptr [1] << 8));
     }

   Portability constraints:
     - __builtin_assume_aligned works only in GCC >= 4.7 and clang >= 4.
     - __assume works only with MSVC (_MSC_VER >= 1200).

   Which variant produces the best code?
     - memcpy is inlined only in gcc >= 3.4, g++ >= 4.9, clang >= 4.
     - MSVC's __assume has no effect.
     - With gcc 13:
       On armelhf, arm64, i686, powerpc, powerpc64, powerpc64le, s390x, x86_64:
       All of a,b,e,f are equally good.
       On alpha, arm, hppa, mips, mips64, riscv64, sh4, sparc64:
       Only a,b are good; f medium; e worst.
     - With older gcc versions on x86_64:
       gcc >= 10: All of a,b,e,f are equally good.
       gcc < 10: Only a,e are good; b,f medium.
     - With MSVC 14: Only c,e are good; d,f medium.

   So, we use the following heuristic for getting good code:
     - gcc >= 4.7, g++ >= 4.9, clang >= 4, or any other platform
       with __builtin_assume_aligned: Use variant a.
     - MSVC: Use variant e.
     - Otherwise: Use variant f.
 */
#if (defined __clang__ ? __clang_major__ >= 4 : \
     (defined __GNUC__ \
      && (defined __cplusplus \
          ? __GNUC__ + (__GNUC_MINOR__ >= 9) > 4 \
          : __GNUC__ + (__GNUC_MINOR__ >= 7) > 4)))
# define _GL_HAS_BUILTIN_ASSUME_ALIGNED 1
#elif defined __has_builtin
# if __has_builtin (__builtin_assume_aligned)
#  define _GL_HAS_BUILTIN_ASSUME_ALIGNED 1
# endif
#endif
#ifdef _GL_HAS_BUILTIN_ASSUME_ALIGNED
# define _GL_STDBIT_ASSUME_ALIGNED(ptr, align) \
    __builtin_assume_aligned (ptr, align)
#else
# define _GL_STDBIT_ASSUME_ALIGNED(ptr, align) (ptr)
#endif

#if defined _GL_HAS_BUILTIN_ASSUME_ALIGNED || defined _MSC_VER
/* The _GL_STDBIT_OPTIMIZE_VIA_MEMCPY trick works on typical hosts
   where CHAR_BIT == 8 and uint_leastN_t types have minimal sizes.
   Check to be safe and to document the assumption.  */
# define _GL_STDBIT_OPTIMIZE_VIA_MEMCPY \
   ((unsigned char) -1 == 0xFF \
    && sizeof (uint_least16_t) == 2 \
    && sizeof (uint_least32_t) == 4 \
    && sizeof (uint_least64_t) == 8)
#endif

#ifndef _GL_STDBIT_OPTIMIZE_VIA_MEMCPY
# define _GL_STDBIT_OPTIMIZE_VIA_MEMCPY 0
#endif

#define _GL_STDBIT_BIGENDIAN (__STDC_ENDIAN_NATIVE__ == __STDC_ENDIAN_BIG__)

#if @GNULIB_STDC_LOAD8@

# if !GNULIB_defined_stdc_load8_functions

_GL_STDC_LOAD8_INLINE uint_least8_t
stdc_load8_beu8 (const unsigned char ptr[1])
{
  return ptr[0];
}

_GL_STDC_LOAD8_INLINE uint_least16_t
stdc_load8_beu16 (const unsigned char ptr[2])
{
  _GL_STDBIT_UINT_FAST16 v0 = ptr[0];
  _GL_STDBIT_UINT_FAST16 v1 = ptr[1];
  return (v0 << (8 * 1)) | (v1 << (8 * 0));
}

_GL_STDC_LOAD8_INLINE uint_least32_t
stdc_load8_beu32 (const unsigned char ptr[4])
{
  _GL_STDBIT_UINT_FAST32 v0 = ptr[0];
  _GL_STDBIT_UINT_FAST32 v1 = ptr[1];
  _GL_STDBIT_UINT_FAST32 v2 = ptr[2];
  _GL_STDBIT_UINT_FAST32 v3 = ptr[3];
  return (v0 << (8 * 3)) | (v1 << (8 * 2)) | (v2 << (8 * 1)) | (v3 << (8 * 0));
}

_GL_STDC_LOAD8_INLINE uint_least64_t
stdc_load8_beu64 (const unsigned char ptr[8])
{
  _GL_STDBIT_UINT_FAST64 v0 = ptr[0];
  _GL_STDBIT_UINT_FAST64 v1 = ptr[1];
  _GL_STDBIT_UINT_FAST64 v2 = ptr[2];
  _GL_STDBIT_UINT_FAST64 v3 = ptr[3];
  _GL_STDBIT_UINT_FAST64 v4 = ptr[4];
  _GL_STDBIT_UINT_FAST64 v5 = ptr[5];
  _GL_STDBIT_UINT_FAST64 v6 = ptr[6];
  _GL_STDBIT_UINT_FAST64 v7 = ptr[7];
  return ((v0 << (8 * 7)) | (v1 << (8 * 6))
          | (v2 << (8 * 5)) | (v3 << (8 * 4))
          | (v4 << (8 * 3)) | (v5 << (8 * 2))
          | (v6 << (8 * 1)) | (v7 << (8 * 0)));
}

_GL_STDC_LOAD8_INLINE uint_least8_t
stdc_load8_leu8 (const unsigned char ptr[1])
{
  return ptr[0];
}

_GL_STDC_LOAD8_INLINE uint_least16_t
stdc_load8_leu16 (const unsigned char ptr[2])
{
  _GL_STDBIT_UINT_FAST16 v0 = ptr[0];
  _GL_STDBIT_UINT_FAST16 v1 = ptr[1];
  return (v0 << (8 * 0)) | (v1 << (8 * 1));
}

_GL_STDC_LOAD8_INLINE uint_least32_t
stdc_load8_leu32 (const unsigned char ptr[4])
{
  _GL_STDBIT_UINT_FAST32 v0 = ptr[0];
  _GL_STDBIT_UINT_FAST32 v1 = ptr[1];
  _GL_STDBIT_UINT_FAST32 v2 = ptr[2];
  _GL_STDBIT_UINT_FAST32 v3 = ptr[3];
  return (v0 << (8 * 0)) | (v1 << (8 * 1)) | (v2 << (8 * 2)) | (v3 << (8 * 3));
}

_GL_STDC_LOAD8_INLINE uint_least64_t
stdc_load8_leu64 (const unsigned char ptr[8])
{
  _GL_STDBIT_UINT_FAST64 v0 = ptr[0];
  _GL_STDBIT_UINT_FAST64 v1 = ptr[1];
  _GL_STDBIT_UINT_FAST64 v2 = ptr[2];
  _GL_STDBIT_UINT_FAST64 v3 = ptr[3];
  _GL_STDBIT_UINT_FAST64 v4 = ptr[4];
  _GL_STDBIT_UINT_FAST64 v5 = ptr[5];
  _GL_STDBIT_UINT_FAST64 v6 = ptr[6];
  _GL_STDBIT_UINT_FAST64 v7 = ptr[7];
  return ((v0 << (8 * 0)) | (v1 << (8 * 1))
          | (v2 << (8 * 2)) | (v3 << (8 * 3))
          | (v4 << (8 * 4)) | (v5 << (8 * 5))
          | (v6 << (8 * 6)) | (v7 << (8 * 7)));
}

_GL_STDC_LOAD8_INLINE int_least8_t
stdc_load8_bes8 (const unsigned char ptr[1])
{
  return stdc_load8_beu8 (ptr);
}

_GL_STDC_LOAD8_INLINE int_least16_t
stdc_load8_bes16 (const unsigned char ptr[2])
{
  return stdc_load8_beu16 (ptr);
}

_GL_STDC_LOAD8_INLINE int_least32_t
stdc_load8_bes32 (const unsigned char ptr[4])
{
  return stdc_load8_beu32 (ptr);
}

_GL_STDC_LOAD8_INLINE int_least64_t
stdc_load8_bes64 (const unsigned char ptr[8])
{
  return stdc_load8_beu64 (ptr);
}

_GL_STDC_LOAD8_INLINE int_least8_t
stdc_load8_les8 (const unsigned char ptr[1])
{
  return stdc_load8_leu8 (ptr);
}

_GL_STDC_LOAD8_INLINE int_least16_t
stdc_load8_les16 (const unsigned char ptr[2])
{
  return stdc_load8_leu16 (ptr);
}

_GL_STDC_LOAD8_INLINE int_least32_t
stdc_load8_les32 (const unsigned char ptr[4])
{
  return stdc_load8_leu32 (ptr);
}

_GL_STDC_LOAD8_INLINE int_least64_t
stdc_load8_les64 (const unsigned char ptr[8])
{
  return stdc_load8_leu64 (ptr);
}

#  define GNULIB_defined_stdc_load8_functions 1
# endif

#endif

#if @GNULIB_STDC_LOAD8_ALIGNED@

# if !GNULIB_defined_stdc_load8_aligned_functions

_GL_STDC_LOAD8_ALIGNED_INLINE uint_least8_t
stdc_load8_aligned_beu8 (const unsigned char ptr[1])
{
  return stdc_load8_beu8 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE uint_least16_t
stdc_load8_aligned_beu16 (const unsigned char ptr[2])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      uint_least16_t value;
      _GL_STDBIT_MEMCPY (&value, _GL_STDBIT_ASSUME_ALIGNED (ptr, 2), 2);
      if (!_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u16 (value);
      return value;
    }
  else
    return stdc_load8_beu16 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE uint_least32_t
stdc_load8_aligned_beu32 (const unsigned char ptr[4])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      uint_least32_t value;
      _GL_STDBIT_MEMCPY (&value, _GL_STDBIT_ASSUME_ALIGNED (ptr, 4), 4);
      if (!_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u32 (value);
      return value;
    }
  else
    return stdc_load8_beu32 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE uint_least64_t
stdc_load8_aligned_beu64 (const unsigned char ptr[8])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      uint_least64_t value;
      _GL_STDBIT_MEMCPY (&value, _GL_STDBIT_ASSUME_ALIGNED (ptr, 8), 8);
      if (!_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u64 (value);
      return value;
    }
  else
    return stdc_load8_beu64 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE uint_least8_t
stdc_load8_aligned_leu8 (const unsigned char ptr[1])
{
  return stdc_load8_leu8 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE uint_least16_t
stdc_load8_aligned_leu16 (const unsigned char ptr[2])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      uint_least16_t value;
      _GL_STDBIT_MEMCPY (&value, _GL_STDBIT_ASSUME_ALIGNED (ptr, 2), 2);
      if (_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u16 (value);
      return value;
    }
  else
    return stdc_load8_leu16 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE uint_least32_t
stdc_load8_aligned_leu32 (const unsigned char ptr[4])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      uint_least32_t value;
      _GL_STDBIT_MEMCPY (&value, _GL_STDBIT_ASSUME_ALIGNED (ptr, 4), 4);
      if (_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u32 (value);
      return value;
    }
  else
    return stdc_load8_leu32 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE uint_least64_t
stdc_load8_aligned_leu64 (const unsigned char ptr[8])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      uint_least64_t value;
      _GL_STDBIT_MEMCPY (&value, _GL_STDBIT_ASSUME_ALIGNED (ptr, 8), 8);
      if (_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u64 (value);
      return value;
    }
  else
    return stdc_load8_leu64 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE int_least8_t
stdc_load8_aligned_bes8 (const unsigned char ptr[1])
{
  return stdc_load8_aligned_beu8 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE int_least16_t
stdc_load8_aligned_bes16 (const unsigned char ptr[2])
{
  return stdc_load8_aligned_beu16 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE int_least32_t
stdc_load8_aligned_bes32 (const unsigned char ptr[4])
{
  return stdc_load8_aligned_beu32 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE int_least64_t
stdc_load8_aligned_bes64 (const unsigned char ptr[8])
{
  return stdc_load8_aligned_beu64 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE int_least8_t
stdc_load8_aligned_les8 (const unsigned char ptr[1])
{
  return stdc_load8_aligned_leu8 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE int_least16_t
stdc_load8_aligned_les16 (const unsigned char ptr[2])
{
  return stdc_load8_aligned_leu16 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE int_least32_t
stdc_load8_aligned_les32 (const unsigned char ptr[4])
{
  return stdc_load8_aligned_leu32 (ptr);
}

_GL_STDC_LOAD8_ALIGNED_INLINE int_least64_t
stdc_load8_aligned_les64 (const unsigned char ptr[8])
{
  return stdc_load8_aligned_leu64 (ptr);
}

#  define GNULIB_defined_stdc_load8_aligned_functions 1
# endif

#endif


/* ISO C2y § 7.18.22 Endian-Aware 8-Bit Store  */

#if @GNULIB_STDC_STORE8@

# if !GNULIB_defined_stdc_store8_functions

_GL_STDC_STORE8_INLINE void
stdc_store8_beu8 (uint_least8_t value, unsigned char ptr[1])
{
  ptr[0] = value;
}

_GL_STDC_STORE8_INLINE void
stdc_store8_beu16 (uint_least16_t value, unsigned char ptr[2])
{
  ptr[0] = (value >> 8) & 0xFFU;
  ptr[1] = value & 0xFFU;
}

_GL_STDC_STORE8_INLINE void
stdc_store8_beu32 (uint_least32_t value, unsigned char ptr[4])
{
  ptr[0] = (value >> 24) & 0xFFU;
  ptr[1] = (value >> 16) & 0xFFU;
  ptr[2] = (value >> 8) & 0xFFU;
  ptr[3] = value & 0xFFU;
}

_GL_STDC_STORE8_INLINE void
stdc_store8_beu64 (uint_least64_t value, unsigned char ptr[8])
{
  ptr[0] = (value >> 56) & 0xFFU;
  ptr[1] = (value >> 48) & 0xFFU;
  ptr[2] = (value >> 40) & 0xFFU;
  ptr[3] = (value >> 32) & 0xFFU;
  ptr[4] = (value >> 24) & 0xFFU;
  ptr[5] = (value >> 16) & 0xFFU;
  ptr[6] = (value >> 8) & 0xFFU;
  ptr[7] = value & 0xFFU;
}

_GL_STDC_STORE8_INLINE void
stdc_store8_leu8 (uint_least8_t value, unsigned char ptr[1])
{
  ptr[0] = value;
}

_GL_STDC_STORE8_INLINE void
stdc_store8_leu16 (uint_least16_t value, unsigned char ptr[2])
{
  ptr[0] = value & 0xFFU;
  ptr[1] = (value >> 8) & 0xFFU;
}

_GL_STDC_STORE8_INLINE void
stdc_store8_leu32 (uint_least32_t value, unsigned char ptr[4])
{
  ptr[0] = value & 0xFFU;
  ptr[1] = (value >> 8) & 0xFFU;
  ptr[2] = (value >> 16) & 0xFFU;
  ptr[3] = (value >> 24) & 0xFFU;
}

_GL_STDC_STORE8_INLINE void
stdc_store8_leu64 (uint_least64_t value, unsigned char ptr[8])
{
  ptr[0] = value & 0xFFU;
  ptr[1] = (value >> 8) & 0xFFU;
  ptr[2] = (value >> 16) & 0xFFU;
  ptr[3] = (value >> 24) & 0xFFU;
  ptr[4] = (value >> 32) & 0xFFU;
  ptr[5] = (value >> 40) & 0xFFU;
  ptr[6] = (value >> 48) & 0xFFU;
  ptr[7] = (value >> 56) & 0xFFU;
}

_GL_STDC_STORE8_INLINE void
stdc_store8_bes8 (int_least8_t value, unsigned char ptr[1])
{
  stdc_store8_beu8 (value, ptr);
}

_GL_STDC_STORE8_INLINE void
stdc_store8_bes16 (int_least16_t value, unsigned char ptr[2])
{
  stdc_store8_beu16 (value, ptr);
}

_GL_STDC_STORE8_INLINE void
stdc_store8_bes32 (int_least32_t value, unsigned char ptr[4])
{
  stdc_store8_beu32 (value, ptr);
}

_GL_STDC_STORE8_INLINE void
stdc_store8_bes64 (int_least64_t value, unsigned char ptr[8])
{
  stdc_store8_beu64 (value, ptr);
}

_GL_STDC_STORE8_INLINE void
stdc_store8_les8 (int_least8_t value, unsigned char ptr[1])
{
  stdc_store8_leu8 (value, ptr);
}

_GL_STDC_STORE8_INLINE void
stdc_store8_les16 (int_least16_t value, unsigned char ptr[2])
{
  stdc_store8_leu16 (value, ptr);
}

_GL_STDC_STORE8_INLINE void
stdc_store8_les32 (int_least32_t value, unsigned char ptr[4])
{
  stdc_store8_leu32 (value, ptr);
}

_GL_STDC_STORE8_INLINE void
stdc_store8_les64 (int_least64_t value, unsigned char ptr[8])
{
  stdc_store8_leu64 (value, ptr);
}

#  define GNULIB_defined_stdc_store8_functions 1
# endif

#endif

#if @GNULIB_STDC_STORE8_ALIGNED@

# if !GNULIB_defined_stdc_store8_aligned_functions

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_beu8 (uint_least8_t value, unsigned char ptr[1])
{
  stdc_store8_beu8 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_beu16 (uint_least16_t value, unsigned char ptr[2])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      if (!_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u16 (value);
      _GL_STDBIT_MEMCPY (_GL_STDBIT_ASSUME_ALIGNED (ptr, 2), &value, 2);
    }
  else
    stdc_store8_beu16 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_beu32 (uint_least32_t value, unsigned char ptr[4])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      if (!_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u32 (value);
      _GL_STDBIT_MEMCPY (_GL_STDBIT_ASSUME_ALIGNED (ptr, 4), &value, 4);
    }
  else
    stdc_store8_beu32 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_beu64 (uint_least64_t value, unsigned char ptr[8])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      if (!_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u64 (value);
      _GL_STDBIT_MEMCPY (_GL_STDBIT_ASSUME_ALIGNED (ptr, 8), &value, 8);
    }
  else
    stdc_store8_beu64 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_leu8 (uint_least8_t value, unsigned char ptr[1])
{
  stdc_store8_leu8 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_leu16 (uint_least16_t value, unsigned char ptr[2])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      if (_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u16 (value);
      _GL_STDBIT_MEMCPY (_GL_STDBIT_ASSUME_ALIGNED (ptr, 2), &value, 2);
    }
  else
    stdc_store8_leu16 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_leu32 (uint_least32_t value, unsigned char ptr[4])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      if (_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u32 (value);
      _GL_STDBIT_MEMCPY (_GL_STDBIT_ASSUME_ALIGNED (ptr, 4), &value, 4);
    }
  else
    stdc_store8_leu32 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_leu64 (uint_least64_t value, unsigned char ptr[8])
{
  if (_GL_STDBIT_OPTIMIZE_VIA_MEMCPY)
    {
      if (_GL_STDBIT_BIGENDIAN)
        value = stdc_memreverse8u64 (value);
      _GL_STDBIT_MEMCPY (_GL_STDBIT_ASSUME_ALIGNED (ptr, 8), &value, 8);
    }
  else
    stdc_store8_leu64 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_bes8 (int_least8_t value, unsigned char ptr[1])
{
  stdc_store8_aligned_beu8 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_bes16 (int_least16_t value, unsigned char ptr[2])
{
  stdc_store8_aligned_beu16 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_bes32 (int_least32_t value, unsigned char ptr[4])
{
  stdc_store8_aligned_beu32 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_bes64 (int_least64_t value, unsigned char ptr[8])
{
  stdc_store8_aligned_beu64 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_les8 (int_least8_t value, unsigned char ptr[1])
{
  stdc_store8_aligned_leu8 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_les16 (int_least16_t value, unsigned char ptr[2])
{
  stdc_store8_aligned_leu16 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_les32 (int_least32_t value, unsigned char ptr[4])
{
  stdc_store8_aligned_leu32 (value, ptr);
}

_GL_STDC_STORE8_ALIGNED_INLINE void
stdc_store8_aligned_les64 (int_least64_t value, unsigned char ptr[8])
{
  stdc_store8_aligned_leu64 (value, ptr);
}

#  define GNULIB_defined_stdc_store8_aligned_functions 1
# endif

#endif


#ifdef __cplusplus
}
#endif

_GL_INLINE_HEADER_END

/* ISO C 23 § 7.18.1 General  */
#ifndef __STDC_VERSION_STDBIT_H__
# define __STDC_VERSION_STDBIT_H__ 202311L
#endif

#endif /* _@GUARD_PREFIX@_STDBIT_H */
#endif /* _@GUARD_PREFIX@_STDBIT_H */
