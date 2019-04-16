/* Big numbers for Emacs.

Copyright 2018-2019 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include "bignum.h"

#include "lisp.h"

#include <math.h>
#include <stdlib.h>

/* mpz global temporaries.  Making them global saves the trouble of
   properly using mpz_init and mpz_clear on temporaries even when
   storage is exhausted.  Admittedly this is not ideal.  An mpz value
   in a temporary is made permanent by mpz_swapping it with a bignum's
   value.  Although typically at most two temporaries are needed,
   time_arith, rounddiv_q and rounding_driver each need four.  */

mpz_t mpz[4];

static void *
xrealloc_for_gmp (void *ptr, size_t ignore, size_t size)
{
  return xrealloc (ptr, size);
}

static void
xfree_for_gmp (void *ptr, size_t ignore)
{
  xfree (ptr);
}

void
init_bignum (void)
{
  eassert (mp_bits_per_limb == GMP_NUMB_BITS);
  integer_width = 1 << 16;
  mp_set_memory_functions (xmalloc, xrealloc_for_gmp, xfree_for_gmp);

  for (int i = 0; i < ARRAYELTS (mpz); i++)
    mpz_init (mpz[i]);
}

/* Return the value of the Lisp bignum N, as a double.  */
double
bignum_to_double (Lisp_Object n)
{
  return mpz_get_d_rounded (XBIGNUM (n)->value);
}

/* Return D, converted to a Lisp integer.  Discard any fraction.
   Signal an error if D cannot be converted.  */
Lisp_Object
double_to_integer (double d)
{
  if (!isfinite (d))
    overflow_error ();
  mpz_set_d (mpz[0], d);
  return make_integer_mpz ();
}

/* Return a Lisp integer equal to mpz[0], which has BITS bits and which
   must not be in fixnum range.  Set mpz[0] to a junk value.  */
static Lisp_Object
make_bignum_bits (size_t bits)
{
  /* The documentation says integer-width should be nonnegative, so
     a single comparison suffices even though 'bits' is unsigned.  */
  if (integer_width < bits)
    overflow_error ();

  struct Lisp_Bignum *b = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Bignum,
						       PVEC_BIGNUM);
  mpz_init (b->value);
  mpz_swap (b->value, mpz[0]);
  return make_lisp_ptr (b, Lisp_Vectorlike);
}

/* Return a Lisp integer equal to mpz[0], which must not be in fixnum range.
   Set mpz[0] to a junk value.  */
static Lisp_Object
make_bignum (void)
{
  return make_bignum_bits (mpz_sizeinbase (mpz[0], 2));
}

/* Return a Lisp integer equal to N, which must not be in fixnum range.  */
Lisp_Object
make_bigint (intmax_t n)
{
  eassert (FIXNUM_OVERFLOW_P (n));
  mpz_set_intmax (mpz[0], n);
  return make_bignum ();
}
Lisp_Object
make_biguint (uintmax_t n)
{
  eassert (FIXNUM_OVERFLOW_P (n));
  mpz_set_uintmax (mpz[0], n);
  return make_bignum ();
}

/* Return a Lisp integer equal to -N, which must not be in fixnum range.  */
Lisp_Object
make_neg_biguint (uintmax_t n)
{
  eassert (-MOST_NEGATIVE_FIXNUM < n);
  mpz_set_uintmax (mpz[0], n);
  mpz_neg (mpz[0], mpz[0]);
  return make_bignum ();
}

/* Return a Lisp integer with value taken from mpz[0].
   Set mpz[0] to a junk value.  */
Lisp_Object
make_integer_mpz (void)
{
  size_t bits = mpz_sizeinbase (mpz[0], 2);

  if (bits <= FIXNUM_BITS)
    {
      EMACS_INT v = 0;
      int i = 0, shift = 0;

      do
	{
	  EMACS_INT limb = mpz_getlimbn (mpz[0], i++);
	  v += limb << shift;
	  shift += GMP_NUMB_BITS;
	}
      while (shift < bits);

      if (mpz_sgn (mpz[0]) < 0)
	v = -v;

      if (!FIXNUM_OVERFLOW_P (v))
	return make_fixnum (v);
    }

  return make_bignum_bits (bits);
}

/* Set RESULT to V.  This code is for when intmax_t is wider than long.  */
void
mpz_set_intmax_slow (mpz_t result, intmax_t v)
{
  int maxlimbs = (INTMAX_WIDTH + GMP_NUMB_BITS - 1) / GMP_NUMB_BITS;
  mp_limb_t *limb = mpz_limbs_write (result, maxlimbs);
  int n = 0;
  uintmax_t u = v;
  bool negative = v < 0;
  if (negative)
    {
      uintmax_t two = 2;
      u = -u & ((two << (UINTMAX_WIDTH - 1)) - 1);
    }

  do
    {
      limb[n++] = u;
      u = GMP_NUMB_BITS < UINTMAX_WIDTH ? u >> GMP_NUMB_BITS : 0;
    }
  while (u != 0);

  mpz_limbs_finish (result, negative ? -n : n);
}
void
mpz_set_uintmax_slow (mpz_t result, uintmax_t v)
{
  int maxlimbs = (UINTMAX_WIDTH + GMP_NUMB_BITS - 1) / GMP_NUMB_BITS;
  mp_limb_t *limb = mpz_limbs_write (result, maxlimbs);
  int n = 0;

  do
    {
      limb[n++] = v;
      v = GMP_NUMB_BITS < INTMAX_WIDTH ? v >> GMP_NUMB_BITS : 0;
    }
  while (v != 0);

  mpz_limbs_finish (result, n);
}

/* If Z fits into *PI, store its value there and return true.
   Return false otherwise.  */
bool
mpz_to_intmax (mpz_t const z, intmax_t *pi)
{
  ptrdiff_t bits = mpz_sizeinbase (z, 2);
  bool negative = mpz_sgn (z) < 0;

  if (bits < INTMAX_WIDTH)
    {
      intmax_t v = 0;
      int i = 0, shift = 0;

      do
	{
	  intmax_t limb = mpz_getlimbn (z, i++);
	  v += limb << shift;
	  shift += GMP_NUMB_BITS;
	}
      while (shift < bits);

      *pi = negative ? -v : v;
      return true;
    }
  if (bits == INTMAX_WIDTH && INTMAX_MIN < -INTMAX_MAX && negative
      && mpz_scan1 (z, 0) == INTMAX_WIDTH - 1)
    {
      *pi = INTMAX_MIN;
      return true;
    }
  return false;
}
bool
mpz_to_uintmax (mpz_t const z, uintmax_t *pi)
{
  if (mpz_sgn (z) < 0)
    return false;
  ptrdiff_t bits = mpz_sizeinbase (z, 2);
  if (UINTMAX_WIDTH < bits)
    return false;

  uintmax_t v = 0;
  int i = 0, shift = 0;

  do
    {
      uintmax_t limb = mpz_getlimbn (z, i++);
      v += limb << shift;
      shift += GMP_NUMB_BITS;
    }
  while (shift < bits);

  *pi = v;
  return true;
}

/* Return the value of the bignum X if it fits, 0 otherwise.
   A bignum cannot be zero, so 0 indicates failure reliably.  */
intmax_t
bignum_to_intmax (Lisp_Object x)
{
  intmax_t i;
  return mpz_to_intmax (XBIGNUM (x)->value, &i) ? i : 0;
}
uintmax_t
bignum_to_uintmax (Lisp_Object x)
{
  uintmax_t i;
  return mpz_to_uintmax (XBIGNUM (x)->value, &i) ? i : 0;
}

/* Yield an upper bound on the buffer size needed to contain a C
   string representing the NUM in base BASE.  This includes any
   preceding '-' and the terminating NUL.  */
static ptrdiff_t
mpz_bufsize (mpz_t const num, int base)
{
  return mpz_sizeinbase (num, base) + 2;
}
ptrdiff_t
bignum_bufsize (Lisp_Object num, int base)
{
  return mpz_bufsize (XBIGNUM (num)->value, base);
}

/* Convert NUM to a nearest double, as opposed to mpz_get_d which
   truncates toward zero.  */
double
mpz_get_d_rounded (mpz_t const num)
{
  ptrdiff_t size = mpz_bufsize (num, 10);

  /* Use mpz_get_d as a shortcut for a bignum so small that rounding
     errors cannot occur, which is possible if EMACS_INT (not counting
     sign) has fewer bits than a double significand.  */
  if (! ((FLT_RADIX == 2 && DBL_MANT_DIG <= FIXNUM_BITS - 1)
	 || (FLT_RADIX == 16 && DBL_MANT_DIG * 4 <= FIXNUM_BITS - 1))
      && size <= DBL_DIG + 2)
    return mpz_get_d (num);

  USE_SAFE_ALLOCA;
  char *buf = SAFE_ALLOCA (size);
  mpz_get_str (buf, 10, num);
  double result = strtod (buf, NULL);
  SAFE_FREE ();
  return result;
}

/* Store into BUF (of size SIZE) the value of NUM as a base-BASE string.
   If BASE is negative, use upper-case digits in base -BASE.
   Return the string's length.
   SIZE must equal bignum_bufsize (NUM, abs (BASE)).  */
ptrdiff_t
bignum_to_c_string (char *buf, ptrdiff_t size, Lisp_Object num, int base)
{
  eassert (bignum_bufsize (num, abs (base)) == size);
  mpz_get_str (buf, base, XBIGNUM (num)->value);
  ptrdiff_t n = size - 2;
  return !buf[n - 1] ? n - 1 : n + !!buf[n];
}

/* Convert NUM to a base-BASE Lisp string.
   If BASE is negative, use upper-case digits in base -BASE.  */

Lisp_Object
bignum_to_string (Lisp_Object num, int base)
{
  ptrdiff_t size = bignum_bufsize (num, abs (base));
  USE_SAFE_ALLOCA;
  char *str = SAFE_ALLOCA (size);
  ptrdiff_t len = bignum_to_c_string (str, size, num, base);
  Lisp_Object result = make_unibyte_string (str, len);
  SAFE_FREE ();
  return result;
}

/* Create a bignum by scanning NUM, with digits in BASE.
   NUM must consist of an optional '-', a nonempty sequence
   of base-BASE digits, and a terminating NUL byte, and
   the represented number must not be in fixnum range.  */

Lisp_Object
make_bignum_str (char const *num, int base)
{
  struct Lisp_Bignum *b = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Bignum,
						       PVEC_BIGNUM);
  mpz_init (b->value);
  int check = mpz_set_str (b->value, num, base);
  eassert (check == 0);
  return make_lisp_ptr (b, Lisp_Vectorlike);
}
