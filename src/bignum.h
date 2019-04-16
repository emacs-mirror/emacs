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

/* Include this header only if access to bignum internals is needed.  */

#ifndef BIGNUM_H
#define BIGNUM_H

#ifdef HAVE_GMP
# include <gmp.h>
#else
# include "mini-gmp.h"
#endif

#include "lisp.h"

/* Number of data bits in a limb.  */
#ifndef GMP_NUMB_BITS
enum { GMP_NUMB_BITS = TYPE_WIDTH (mp_limb_t) };
#endif

struct Lisp_Bignum
{
  union vectorlike_header header;
  mpz_t value;
} GCALIGNED_STRUCT;

extern mpz_t mpz[4];

extern void init_bignum (void);
extern Lisp_Object make_integer_mpz (void);
extern bool mpz_to_intmax (mpz_t const, intmax_t *) ARG_NONNULL ((1, 2));
extern bool mpz_to_uintmax (mpz_t const, uintmax_t *) ARG_NONNULL ((1, 2));
extern void mpz_set_intmax_slow (mpz_t, intmax_t) ARG_NONNULL ((1));
extern void mpz_set_uintmax_slow (mpz_t, uintmax_t) ARG_NONNULL ((1));
extern double mpz_get_d_rounded (mpz_t const);

INLINE_HEADER_BEGIN

INLINE struct Lisp_Bignum *
XBIGNUM (Lisp_Object a)
{
  eassert (BIGNUMP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Bignum);
}

INLINE void ARG_NONNULL ((1))
mpz_set_intmax (mpz_t result, intmax_t v)
{
  /* mpz_set_si works in terms of long, but Emacs may use a wider
     integer type, and so sometimes will have to construct the mpz_t
     by hand.  */
  if (LONG_MIN <= v && v <= LONG_MAX)
    mpz_set_si (result, v);
  else
    mpz_set_intmax_slow (result, v);
}
INLINE void ARG_NONNULL ((1))
mpz_set_uintmax (mpz_t result, uintmax_t v)
{
  if (v <= ULONG_MAX)
    mpz_set_ui (result, v);
  else
    mpz_set_uintmax_slow (result, v);
}

/* Return a pointer to an mpz_t that is equal to the Lisp integer I.
   If I is a bignum this returns a pointer to I's representation;
   otherwise this sets *TMP to I's value and returns TMP.  */
INLINE mpz_t *
bignum_integer (mpz_t *tmp, Lisp_Object i)
{
  if (FIXNUMP (i))
    {
      mpz_set_intmax (*tmp, XFIXNUM (i));
      return tmp;
    }
  return &XBIGNUM (i)->value;
}

INLINE_HEADER_END

#endif /* BIGNUM_H */
