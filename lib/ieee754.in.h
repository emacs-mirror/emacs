/* Copyright (C) 1992-2026 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */

#ifndef _IEEE754_H
#define _IEEE754_H 1

#ifndef _GL_GNULIB_HEADER
/* Ordinary glibc usage.  */
# include <features.h>
# include <bits/endian.h>
# define _IEEE754_BYTE_ORDER __BYTE_ORDER
# define _IEEE754_BIG_ENDIAN __BIG_ENDIAN
# define _IEEE754_LITTLE_ENDIAN __LITTLE_ENDIAN
# define _IEEE754_FLOAT_WORD_ORDER __FLOAT_WORD_ORDER
#else
/* Gnulib usage.  */
# include <endian.h>
# define _IEEE754_BYTE_ORDER BYTE_ORDER
# define _IEEE754_BIG_ENDIAN BIG_ENDIAN
# define _IEEE754_LITTLE_ENDIAN LITTLE_ENDIAN
# define _IEEE754_FLOAT_WORD_ORDER BYTE_ORDER
#endif

#ifdef __cplusplus
extern "C" {
#endif

union ieee754_float
  {
    float f;

    /* This is the IEEE 754 single-precision format.  */
    struct
      {
#if	_IEEE754_BYTE_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int mantissa:23;
#endif				/* Big endian.  */
#if	_IEEE754_BYTE_ORDER == _IEEE754_LITTLE_ENDIAN
	unsigned int mantissa:23;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif				/* Little endian.  */
      } ieee;

    /* This format makes it easier to see if a NaN is a signalling NaN.  */
    struct
      {
#if	_IEEE754_BYTE_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int quiet_nan:1;
	unsigned int mantissa:22;
#endif				/* Big endian.  */
#if	_IEEE754_BYTE_ORDER == _IEEE754_LITTLE_ENDIAN
	unsigned int mantissa:22;
	unsigned int quiet_nan:1;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif				/* Little endian.  */
      } ieee_nan;
  };

#define IEEE754_FLOAT_BIAS	0x7f /* Added to exponent.  */


union ieee754_double
  {
    double d;

    /* This is the IEEE 754 double-precision format.  */
    struct
      {
#if	_IEEE754_BYTE_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:11;
	/* Together these comprise the mantissa.  */
	unsigned int mantissa0:20;
	unsigned int mantissa1:32;
#endif				/* Big endian.  */
#if	_IEEE754_BYTE_ORDER == _IEEE754_LITTLE_ENDIAN
# if	_IEEE754_FLOAT_WORD_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
	unsigned int mantissa1:32;
# else
	/* Together these comprise the mantissa.  */
	unsigned int mantissa1:32;
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
# endif
#endif				/* Little endian.  */
      } ieee;

    /* This format makes it easier to see if a NaN is a signalling NaN.  */
    struct
      {
#if	_IEEE754_BYTE_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:11;
	unsigned int quiet_nan:1;
	/* Together these comprise the mantissa.  */
	unsigned int mantissa0:19;
	unsigned int mantissa1:32;
#else
# if	_IEEE754_FLOAT_WORD_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int mantissa0:19;
	unsigned int quiet_nan:1;
	unsigned int exponent:11;
	unsigned int negative:1;
	unsigned int mantissa1:32;
# else
	/* Together these comprise the mantissa.  */
	unsigned int mantissa1:32;
	unsigned int mantissa0:19;
	unsigned int quiet_nan:1;
	unsigned int exponent:11;
	unsigned int negative:1;
# endif
#endif
      } ieee_nan;
  };

#define IEEE754_DOUBLE_BIAS	0x3ff /* Added to exponent.  */


union ieee854_long_double
  {
    long double d;

    /* This is the IEEE 854 double-extended-precision format.  */
    struct
      {
#if	_IEEE754_BYTE_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:15;
	unsigned int empty:16;
	unsigned int mantissa0:32;
	unsigned int mantissa1:32;
#endif
#if	_IEEE754_BYTE_ORDER == _IEEE754_LITTLE_ENDIAN
# if	_IEEE754_FLOAT_WORD_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int exponent:15;
	unsigned int negative:1;
	unsigned int empty:16;
	unsigned int mantissa0:32;
	unsigned int mantissa1:32;
# else
	unsigned int mantissa1:32;
	unsigned int mantissa0:32;
	unsigned int exponent:15;
	unsigned int negative:1;
	unsigned int empty:16;
# endif
#endif
      } ieee;

    /* This is for NaNs in the IEEE 854 double-extended-precision format.  */
    struct
      {
#if	_IEEE754_BYTE_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:15;
	unsigned int empty:16;
	unsigned int one:1;
	unsigned int quiet_nan:1;
	unsigned int mantissa0:30;
	unsigned int mantissa1:32;
#endif
#if	_IEEE754_BYTE_ORDER == _IEEE754_LITTLE_ENDIAN
# if	_IEEE754_FLOAT_WORD_ORDER == _IEEE754_BIG_ENDIAN
	unsigned int exponent:15;
	unsigned int negative:1;
	unsigned int empty:16;
	unsigned int mantissa0:30;
	unsigned int quiet_nan:1;
	unsigned int one:1;
	unsigned int mantissa1:32;
# else
	unsigned int mantissa1:32;
	unsigned int mantissa0:30;
	unsigned int quiet_nan:1;
	unsigned int one:1;
	unsigned int exponent:15;
	unsigned int negative:1;
	unsigned int empty:16;
# endif
#endif
      } ieee_nan;
  };

#define IEEE854_LONG_DOUBLE_BIAS 0x3fff

#ifdef __cplusplus
}
#endif

#endif /* ieee754.h */
