/* stdbit.h - C23 bit and byte utilities for non-C23 platforms

   Copyright 2024-2025 Free Software Foundation, Inc.

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

#ifndef STDBIT_H
#define STDBIT_H 1

/* This file uses _GL_INLINE, WORDS_BIGENDIAN.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

_GL_INLINE_HEADER_BEGIN

#ifndef _GL_STDBIT_INLINE
# define _GL_STDBIT_INLINE _GL_INLINE
#endif
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

/* An expression, preferably with the type of A, that has the value of B.  */
#if ((defined __GNUC__ && 2 <= __GNUC__) \
     || (defined __clang_major__ && 4 <= __clang_major__) \
     || (defined __IBMC__ && 1210 <= __IBMC__ && defined __IBM__TYPEOF__) \
     || (defined __SUNPRO_C && 0x5110 <= __SUNPRO_C && !__STDC__) \
     || (defined _MSC_VER && 1939 <= _MSC_VER))
# define _GL_STDBIT_TYPEOF_CAST(a, b) ((__typeof__ (a)) (b))
#elif 202311 <= __STDC_VERSION__
# define _GL_STDBIT_TYPEOF_CAST(a, b) ((typeof (a)) (b))
#else
/* This platform is so old that it lacks typeof, so _Generic is likely
   missing or unreliable.  The C23 standard seems to allow yielding B
   (which is always unsigned long long int), so do that.  */
# define _GL_STDBIT_TYPEOF_CAST(a, b) (b)
#endif


/* ISO C 23 § 7.18.1 General  */

#define __STDC_VERSION_STDBIT_H__ 202311L


/* ISO C 23 § 7.18.2 Endian  */

#define __STDC_ENDIAN_BIG__ 4321
#define __STDC_ENDIAN_LITTLE__ 1234
#ifdef WORDS_BIGENDIAN
# define __STDC_ENDIAN_NATIVE__ __STDC_ENDIAN_BIG__
#else
# define __STDC_ENDIAN_NATIVE__ __STDC_ENDIAN_LITTLE__
#endif


#ifdef __cplusplus
extern "C" {
#endif

#if 3 < __GNUC__ + (4 <= __GNUC_MINOR__) || 4 <= __clang_major__
# define _GL_STDBIT_HAS_BUILTIN_CLZ true
# define _GL_STDBIT_HAS_BUILTIN_CTZ true
# define _GL_STDBIT_HAS_BUILTIN_POPCOUNT true
#elif defined __has_builtin
# if (__has_builtin (__builtin_clz) \
      && __has_builtin (__builtin_clzl) \
      && __has_builtin (__builtin_clzll))
#  define _GL_STDBIT_HAS_BUILTIN_CLZ true
# endif
# if (__has_builtin (__builtin_ctz) \
      && __has_builtin (__builtin_ctzl) \
      && __has_builtin (__builtin_ctzll))
#  define _GL_STDBIT_HAS_BUILTIN_CTZ true
# endif
# if (__has_builtin (__builtin_popcount) \
      && __has_builtin (__builtin_popcountl) \
      && __has_builtin (__builtin_popcountll))
#  define _GL_STDBIT_HAS_BUILTIN_POPCOUNT true
# endif
#endif

/* Count leading 0 bits of N, even if N is 0.  */
#ifdef _GL_STDBIT_HAS_BUILTIN_CLZ
_GL_STDBIT_INLINE int
__gl_stdbit_clz (unsigned int n)
{
  return n ? __builtin_clz (n) : 8 * sizeof n;
}
_GL_STDBIT_INLINE int
__gl_stdbit_clzl (unsigned long int n)
{
  return n ? __builtin_clzl (n) : 8 * sizeof n;
}
_GL_STDBIT_INLINE int
__gl_stdbit_clzll (unsigned long long int n)
{
  return n ? __builtin_clzll (n) : 8 * sizeof n;
}
#elif defined _MSC_VER

/* Declare the few MSVC intrinsics that we need.  We prefer not to include
   <intrin.h> because it would pollute the namespace.  */
extern unsigned char _BitScanReverse (unsigned long *, unsigned long);
# pragma intrinsic (_BitScanReverse)
# ifdef _M_X64
extern unsigned char _BitScanReverse64 (unsigned long *, unsigned long long);
#  pragma intrinsic (_BitScanReverse64)
# endif

_GL_STDBIT_INLINE int
__gl_stdbit_clzl (unsigned long int n)
{
  unsigned long int r;
  return 8 * sizeof n - (_BitScanReverse (&r, n) ? r + 1 : 0);
}
_GL_STDBIT_INLINE int
__gl_stdbit_clz (unsigned int n)
{
  return __gl_stdbit_clzl (n) - 8 * (sizeof 0ul - sizeof n);
}
_GL_STDBIT_INLINE int
__gl_stdbit_clzll (unsigned long long int n)
{
# ifdef _M_X64
  unsigned long int r;
  return 8 * sizeof n - (_BitScanReverse64 (&r, n) ? r + 1 : 0);
# else
  unsigned long int hi = n >> 32;
  return __gl_stdbit_clzl (hi ? hi : n) + (hi ? 0 : 32);
# endif
}

#else /* !_MSC_VER */

_GL_STDBIT_INLINE int
__gl_stdbit_clzll (unsigned long long int n)
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
_GL_STDBIT_INLINE int
__gl_stdbit_clz (unsigned int n)
{
  return __gl_stdbit_clzll (n) - 8 * (sizeof 0ull - sizeof 0u);
}
_GL_STDBIT_INLINE int
__gl_stdbit_clzl (unsigned long int n)
{
  return __gl_stdbit_clzll (n) - 8 * (sizeof 0ull - sizeof 0ul);
}
#endif

/* Count trailing 0 bits of N, even if N is 0.  */
#ifdef _GL_STDBIT_HAS_BUILTIN_CTZ
_GL_STDBIT_INLINE int
__gl_stdbit_ctz (unsigned int n)
{
  return n ? __builtin_ctz (n) : 8 * sizeof n;
}
_GL_STDBIT_INLINE int
__gl_stdbit_ctzl (unsigned long int n)
{
  return n ? __builtin_ctzl (n) : 8 * sizeof n;
}
_GL_STDBIT_INLINE int
__gl_stdbit_ctzll (unsigned long long int n)
{
  return n ? __builtin_ctzll (n) : 8 * sizeof n;
}
#elif defined _MSC_VER

/* Declare the few MSVC intrinsics that we need.  We prefer not to include
   <intrin.h> because it would pollute the namespace.  */
extern unsigned char _BitScanForward (unsigned long *, unsigned long);
# pragma intrinsic (_BitScanForward)
# ifdef _M_X64
extern unsigned char _BitScanForward64 (unsigned long *, unsigned long long);
#  pragma intrinsic (_BitScanForward64)
# endif

_GL_STDBIT_INLINE int
__gl_stdbit_ctzl (unsigned long int n)
{
  unsigned long int r;
  return _BitScanForward (&r, n) ? r : 8 * sizeof n;
}
_GL_STDBIT_INLINE int
__gl_stdbit_ctz (unsigned int n)
{
  return __gl_stdbit_ctzl (n | (1ul << (8 * sizeof n - 1) << 1));
}
_GL_STDBIT_INLINE int
__gl_stdbit_ctzll (unsigned long long int n)
{
# ifdef _M_X64
  unsigned long int r;
  return _BitScanForward64 (&r, n) ? r : 8 * sizeof n;
# else
  unsigned int lo = n;
  return __gl_stdbit_ctzl (lo ? lo : n >> 32) + (lo ? 0 : 32);
# endif
}

#else /* !_MSC_VER */

_GL_STDBIT_INLINE int
__gl_stdbit_ctz (unsigned int n)
{
  return 8 * sizeof n - (n ? __gl_stdbit_clz (n & -n) + 1 : 0);
}
_GL_STDBIT_INLINE int
__gl_stdbit_ctzl (unsigned long int n)
{
  return 8 * sizeof n - (n ? __gl_stdbit_clzl (n & -n) + 1 : 0);
}
_GL_STDBIT_INLINE int
__gl_stdbit_ctzll (unsigned long long int n)
{
  return 8 * sizeof n - (n ? __gl_stdbit_clzll (n & -n) + 1 : 0);
}
#endif

#if @GL_STDC_COUNT_ONES@
/* Count 1 bits in N.  */
# ifdef _GL_STDBIT_HAS_BUILTIN_POPCOUNT
#  define __gl_stdbit_popcount __builtin_popcount
#  define __gl_stdbit_popcountl __builtin_popcountl
#  define __gl_stdbit_popcountll __builtin_popcountll
# else
_GL_STDC_COUNT_ONES_INLINE int
__gl_stdbit_popcount_wide (unsigned long long int n)
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

#  ifdef _MSC_VER
#   if 1500 <= _MSC_VER && (defined _M_IX86 || defined _M_X64)
/* Declare the few MSVC intrinsics that we need.  We prefer not to include
   <intrin.h> because it would pollute the namespace.  */
extern void __cpuid (int[4], int);
#    pragma intrinsic (__cpuid)
extern unsigned int __popcnt (unsigned int);
#    pragma intrinsic (__popcnt)
#    ifdef _M_X64
extern unsigned long long __popcnt64 (unsigned long long);
#     pragma intrinsic (__popcnt64)
#    else
_GL_STDC_COUNT_ONES_INLINE int
__popcnt64 (unsigned long long int n)
{
  return __popcnt (n >> 32) + __popcnt (n);
}
#    endif
#   endif

/* 1 if supported, -1 if not, 0 if unknown.  */
extern signed char __gl_stdbit_popcount_support;

_GL_STDC_COUNT_ONES_INLINE bool
__gl_stdbit_popcount_supported (void)
{
  if (!__gl_stdbit_popcount_support)
    {
      /* Do as described in
         <https://docs.microsoft.com/en-us/cpp/intrinsics/popcnt16-popcnt-popcnt64>
         Although Microsoft started requiring POPCNT in MS-Windows 11 24H2,
         we'll be more cautious.  */
      int cpu_info[4];
      __cpuid (cpu_info, 1);
      __gl_stdbit_popcount_support = cpu_info[2] & 1 << 23 ? 1 : -1;
    }
  return 0 < __gl_stdbit_popcount_support;
}
_GL_STDC_COUNT_ONES_INLINE int
__gl_stdbit_popcount (unsigned int n)
{
  return (__gl_stdbit_popcount_supported ()
          ? __popcnt (n)
          : __gl_stdbit_popcount_wide (n));
}
_GL_STDC_COUNT_ONES_INLINE int
__gl_stdbit_popcountl (unsigned long int n)
{
  return (__gl_stdbit_popcount_supported ()
          ? __popcnt (n)
          : __gl_stdbit_popcount_wide (n));
}
_GL_STDC_COUNT_ONES_INLINE int
__gl_stdbit_popcountll (unsigned long long int n)
{
  return (__gl_stdbit_popcount_supported ()
          ? __popcnt64 (n)
          : __gl_stdbit_popcount_wide (n));
}
#  else /* !_MSC_VER */
#   define __gl_stdbit_popcount __gl_stdbit_popcount_wide
#   define __gl_stdbit_popcountl __gl_stdbit_popcount_wide
#   define __gl_stdbit_popcountll __gl_stdbit_popcount_wide
#  endif
# endif
#endif


/* ISO C 23 § 7.18.3 Count Leading Zeros  */

#if @GL_STDC_LEADING_ZEROS@

_GL_STDC_LEADING_ZEROS_INLINE unsigned int
stdc_leading_zeros_ui (unsigned int n)
{
  return __gl_stdbit_clz (n);
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
  return __gl_stdbit_clzl (n);
}

_GL_STDC_LEADING_ZEROS_INLINE unsigned int
stdc_leading_zeros_ull (unsigned long long int n)
{
  return __gl_stdbit_clzll (n);
}

# define stdc_leading_zeros(n) \
  (sizeof (n) == 1 ? stdc_leading_zeros_uc (n)	\
   : sizeof (n) == sizeof (unsigned short int) ? stdc_leading_zeros_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_leading_zeros_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_leading_zeros_ul (n) \
   : stdc_leading_zeros_ull (n))

#endif


/* ISO C 23 § 7.18.4 Count Leading Ones  */

#if @GL_STDC_LEADING_ONES@

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

# define stdc_leading_ones(n) \
  (sizeof (n) == 1 ? stdc_leading_ones_uc (n)	\
   : sizeof (n) == sizeof (unsigned short int) ? stdc_leading_ones_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_leading_ones_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_leading_ones_ul (n) \
   : stdc_leading_ones_ull (n))

#endif


/* ISO C 23 § 7.18.5 Count Trailing Zeros  */

#if @GL_STDC_TRAILING_ZEROS@

_GL_STDC_TRAILING_ZEROS_INLINE unsigned int
stdc_trailing_zeros_ui (unsigned int n)
{
  return __gl_stdbit_ctz (n);
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
  return __gl_stdbit_ctzl (n);
}

_GL_STDC_TRAILING_ZEROS_INLINE unsigned int
stdc_trailing_zeros_ull (unsigned long long int n)
{
  return __gl_stdbit_ctzll (n);
}

# define stdc_trailing_zeros(n) \
  (sizeof (n) == 1 ? stdc_trailing_zeros_uc (n)	\
   : sizeof (n) == sizeof (unsigned short int) ? stdc_trailing_zeros_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_trailing_zeros_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_trailing_zeros_ul (n) \
   : stdc_trailing_zeros_ull (n))

#endif


/* ISO C 23 § 7.18.6 Count Trailing Ones  */

#if @GL_STDC_TRAILING_ONES@

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

# define stdc_trailing_ones(n) \
  (sizeof (n) == 1 ? stdc_trailing_ones_uc (n)	\
   : sizeof (n) == sizeof (unsigned short int) ? stdc_trailing_ones_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_trailing_ones_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_trailing_ones_ul (n) \
   : stdc_trailing_ones_ull (n))

#endif


/* ISO C 23 § 7.18.7 First Leading Zero  */

#if @GL_STDC_FIRST_LEADING_ZERO@

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

# define stdc_first_leading_zero(n) \
  (sizeof (n) == 1 ? stdc_first_leading_zero_uc (n) \
   : sizeof (n) == sizeof (unsigned short) ? stdc_first_leading_zero_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_first_leading_zero_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_first_leading_zero_ul (n) \
   : stdc_first_leading_zero_ull (n))

#endif


/* ISO C 23 § 7.18.8 First Leading One  */

#if @GL_STDC_FIRST_LEADING_ONE@

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

# define stdc_first_leading_one(n) \
  (sizeof (n) == 1 ? stdc_first_leading_one_uc (n) \
   : sizeof (n) == sizeof (unsigned short) ? stdc_first_leading_one_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_first_leading_one_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_first_leading_one_ul (n) \
   : stdc_first_leading_one_ull (n))

#endif


/* ISO C 23 § 7.18.9 First Trailing Zero  */

#if @GL_STDC_FIRST_TRAILING_ZERO@

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

# define stdc_first_trailing_zero(n) \
  (sizeof (n) == 1 ? stdc_first_trailing_zero_uc (n) \
   : sizeof (n) == sizeof (unsigned short) ? stdc_first_trailing_zero_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_first_trailing_zero_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_first_trailing_zero_ul (n) \
   : stdc_first_trailing_zero_ull (n))

#endif


/* ISO C 23 § 7.18.10 First Trailing One  */

#if @GL_STDC_FIRST_TRAILING_ONE@

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

#define stdc_first_trailing_one(n) \
  (sizeof (n) == 1 ? stdc_first_trailing_one_uc (n) \
   : sizeof (n) == sizeof (unsigned short) ? stdc_first_trailing_one_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_first_trailing_one_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_first_trailing_one_ul (n) \
   : stdc_first_trailing_one_ull (n))

#endif


/* ISO C 23 § 7.18.12 Count Ones  */

#if @GL_STDC_COUNT_ONES@

_GL_STDC_COUNT_ONES_INLINE unsigned int
stdc_count_ones_ui (unsigned int n)
{
  return __gl_stdbit_popcount (n);
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
  return __gl_stdbit_popcountl (n);
}

_GL_STDC_COUNT_ONES_INLINE unsigned int
stdc_count_ones_ull (unsigned long long int n)
{
  return __gl_stdbit_popcountll (n);
}

# define stdc_count_ones(n) \
  (sizeof (n) == 1 ? stdc_count_ones_uc (n) \
   : sizeof (n) == sizeof (unsigned short int) ? stdc_count_ones_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_count_ones_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_count_ones_ul (n) \
   : stdc_count_ones_ull (n))

#endif


/* ISO C 23 § 7.18.11 Count Zeros  */

#if @GL_STDC_COUNT_ZEROS@

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

# define stdc_count_zeros(n) \
  (sizeof (n) == 1 ? stdc_count_zeros_uc (n) \
   : sizeof (n) == sizeof (unsigned short int) ? stdc_count_zeros_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_count_zeros_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_count_zeros_ul (n) \
   : stdc_count_zeros_ull (n))

#endif


/* ISO C 23 § 7.18.13 Single-bit Check  */

#if @GL_STDC_HAS_SINGLE_BIT@

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

# define stdc_has_single_bit(n) \
  ((bool) \
   (sizeof (n) == 1 ? stdc_has_single_bit_uc (n) \
    : sizeof (n) == sizeof (unsigned short int) ? stdc_has_single_bit_us (n) \
    : sizeof (n) == sizeof 0u ? stdc_has_single_bit_ui (n) \
    : sizeof (n) == sizeof 0ul ? stdc_has_single_bit_ul (n) \
    : stdc_has_single_bit_ull (n)))

#endif


/* ISO C 23 § 7.18.14 Bit Width  */

#if @GL_STDC_BIT_WIDTH@

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

# define stdc_bit_width(n) \
  (sizeof (n) == 1 ? stdc_bit_width_uc (n) \
   : sizeof (n) == sizeof (unsigned short int) ? stdc_bit_width_us (n) \
   : sizeof (n) == sizeof 0u ? stdc_bit_width_ui (n) \
   : sizeof (n) == sizeof 0ul ? stdc_bit_width_ul (n) \
   : stdc_bit_width_ull (n))

#endif


/* ISO C 23 § 7.18.15 Bit Floor  */

#if @GL_STDC_BIT_FLOOR@

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

# define stdc_bit_floor(n) \
  (_GL_STDBIT_TYPEOF_CAST \
   (n, \
    (sizeof (n) == 1 ? stdc_bit_floor_uc (n) \
     : sizeof (n) == sizeof (unsigned short int) ? stdc_bit_floor_us (n) \
     : sizeof (n) == sizeof 0u ? stdc_bit_floor_ui (n) \
     : sizeof (n) == sizeof 0ul ? stdc_bit_floor_ul (n) \
     : stdc_bit_floor_ull (n))))

#endif


/* ISO C 23 § 7.18.16 Bit Ceiling  */

#if @GL_STDC_BIT_CEIL@

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

# define stdc_bit_ceil(n) \
  (_GL_STDBIT_TYPEOF_CAST \
   (n, \
    (sizeof (n) == 1 ? stdc_bit_ceil_uc (n) \
     : sizeof (n) == sizeof (unsigned short int) ? stdc_bit_ceil_us (n) \
     : sizeof (n) == sizeof 0u ? stdc_bit_ceil_ui (n) \
     : sizeof (n) == sizeof 0ul ? stdc_bit_ceil_ul (n) \
     : stdc_bit_ceil_ull (n))))

#endif


#ifdef __cplusplus
}
#endif

_GL_INLINE_HEADER_END

#endif /* STDBIT_H */
