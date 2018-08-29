/* Big numbers for Emacs.

Copyright 2018 Free Software Foundation, Inc.

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

/* Return the value of the Lisp bignum N, as a double.  */
double
bignum_to_double (Lisp_Object n)
{
  return mpz_get_d (XBIGNUM (n)->value);
}

/* Return D, converted to a bignum.  Discard any fraction.  */
Lisp_Object
double_to_bignum (double d)
{
  mpz_t z;
  mpz_init_set_d (z, d);
  Lisp_Object result = make_integer (z);
  mpz_clear (z);
  return result;
}

/* Return a Lisp integer equal to OP, which has BITS bits and which
   must not be in fixnum range.  */
static Lisp_Object
make_bignum_bits (mpz_t const op, size_t bits)
{
  /* The documentation says integer-width should be nonnegative, so
     a single comparison suffices even though 'bits' is unsigned.  */
  if (integer_width < bits)
    range_error ();

  struct Lisp_Bignum *b = ALLOCATE_PSEUDOVECTOR (struct Lisp_Bignum, value,
						 PVEC_BIGNUM);
  /* We could mpz_init + mpz_swap here, to avoid a copy, but the
     resulting API seemed possibly confusing.  */
  mpz_init_set (b->value, op);

  return make_lisp_ptr (b, Lisp_Vectorlike);
}

/* Return a Lisp integer equal to OP, which must not be in fixnum range.  */
static Lisp_Object
make_bignum (mpz_t const op)
{
  return make_bignum_bits (op, mpz_sizeinbase (op, 2));
}

static void mpz_set_uintmax_slow (mpz_t, uintmax_t);

/* Set RESULT to V.  */
static void
mpz_set_uintmax (mpz_t result, uintmax_t v)
{
  if (v <= ULONG_MAX)
    mpz_set_ui (result, v);
  else
    mpz_set_uintmax_slow (result, v);
}

/* Return a Lisp integer equal to N, which must not be in fixnum range.  */
Lisp_Object
make_bigint (intmax_t n)
{
  eassert (FIXNUM_OVERFLOW_P (n));
  mpz_t z;
  mpz_init (z);
  mpz_set_intmax (z, n);
  Lisp_Object result = make_bignum (z);
  mpz_clear (z);
  return result;
}
Lisp_Object
make_biguint (uintmax_t n)
{
  eassert (FIXNUM_OVERFLOW_P (n));
  mpz_t z;
  mpz_init (z);
  mpz_set_uintmax (z, n);
  Lisp_Object result = make_bignum (z);
  mpz_clear (z);
  return result;
}

/* Return a Lisp integer with value taken from OP.  */
Lisp_Object
make_integer (mpz_t const op)
{
  size_t bits = mpz_sizeinbase (op, 2);

  if (bits <= FIXNUM_BITS)
    {
      EMACS_INT v = 0;
      int i = 0, shift = 0;

      do
	{
	  EMACS_INT limb = mpz_getlimbn (op, i++);
	  v += limb << shift;
	  shift += GMP_NUMB_BITS;
	}
      while (shift < bits);

      if (mpz_sgn (op) < 0)
	v = -v;

      if (!FIXNUM_OVERFLOW_P (v))
	return make_fixnum (v);
    }

  return make_bignum_bits (op, bits);
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
static void
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

/* Return the value of the bignum X if it fits, 0 otherwise.
   A bignum cannot be zero, so 0 indicates failure reliably.  */
intmax_t
bignum_to_intmax (Lisp_Object x)
{
  ptrdiff_t bits = mpz_sizeinbase (XBIGNUM (x)->value, 2);
  bool negative = mpz_sgn (XBIGNUM (x)->value) < 0;

  if (bits < INTMAX_WIDTH)
    {
      intmax_t v = 0;
      int i = 0, shift = 0;

      do
	{
	  intmax_t limb = mpz_getlimbn (XBIGNUM (x)->value, i++);
	  v += limb << shift;
	  shift += GMP_NUMB_BITS;
	}
      while (shift < bits);

      return negative ? -v : v;
    }
  return ((bits == INTMAX_WIDTH && INTMAX_MIN < -INTMAX_MAX && negative
	   && mpz_scan1 (XBIGNUM (x)->value, 0) == INTMAX_WIDTH - 1)
	  ? INTMAX_MIN : 0);
}
uintmax_t
bignum_to_uintmax (Lisp_Object x)
{
  uintmax_t v = 0;
  if (0 <= mpz_sgn (XBIGNUM (x)->value))
    {
      ptrdiff_t bits = mpz_sizeinbase (XBIGNUM (x)->value, 2);
      if (bits <= UINTMAX_WIDTH)
	{
	  int i = 0, shift = 0;

	  do
	    {
	      uintmax_t limb = mpz_getlimbn (XBIGNUM (x)->value, i++);
	      v += limb << shift;
	      shift += GMP_NUMB_BITS;
	    }
	  while (shift < bits);
	}
    }
  return v;
}

/* Convert NUM to a base-BASE Lisp string.  */

Lisp_Object
bignum_to_string (Lisp_Object num, int base)
{
  ptrdiff_t n = mpz_sizeinbase (XBIGNUM (num)->value, base) - 1;
  USE_SAFE_ALLOCA;
  char *str = SAFE_ALLOCA (n + 3);
  mpz_get_str (str, base, XBIGNUM (num)->value);
  while (str[n])
    n++;
  Lisp_Object result = make_unibyte_string (str, n);
  SAFE_FREE ();
  return result;
}

/* Create a bignum by scanning NUM, with digits in BASE.
   NUM must consist of an optional '-', a nonempty sequence
   of base-BASE digits, and a terminating null byte, and
   the represented number must not be in fixnum range.  */

Lisp_Object
make_bignum_str (char const *num, int base)
{
  struct Lisp_Bignum *b = ALLOCATE_PSEUDOVECTOR (struct Lisp_Bignum, value,
						 PVEC_BIGNUM);
  mpz_init (b->value);
  int check = mpz_set_str (b->value, num, base);
  eassert (check == 0);
  return make_lisp_ptr (b, Lisp_Vectorlike);
}
