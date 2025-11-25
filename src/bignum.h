/* Big numbers for Emacs.

Copyright 2018-2025 Free Software Foundation, Inc.

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

#include <gmp.h>
#include "lisp.h"

/* Compile with -DFASTER_BIGNUM=0 to disable common optimizations and
   allow easier testing of some slow-path code.  */
#ifndef FASTER_BIGNUM
# define FASTER_BIGNUM 1
#endif

/* Number of data bits in a limb.  */
#ifndef GMP_NUMB_BITS
enum { GMP_NUMB_BITS = TYPE_WIDTH (mp_limb_t) };
#endif

/* Define FLAT_BIGNUMS to use the flat bignum representation.  */
#ifdef HAVE_MPS
# define FLAT_BIGNUMS
#endif

struct Lisp_Bignum
{
  struct vectorlike_header header;
#ifdef FLAT_BIGNUMS
  mp_size_t sign_and_size;
  mp_limb_t limbs[FLEXIBLE_ARRAY_MEMBER];
#else
  mpz_t value;
#endif
} GCALIGNED_STRUCT;

extern mpz_t mpz[5];

extern void init_bignum (void);
extern Lisp_Object make_integer_mpz (void);
extern bool mpz_to_intmax (mpz_t const, intmax_t *) ARG_NONNULL ((1, 2));
extern bool mpz_to_uintmax (mpz_t const, uintmax_t *) ARG_NONNULL ((1, 2));
extern void mpz_set_intmax_slow (mpz_t, intmax_t) ARG_NONNULL ((1));
extern void mpz_set_uintmax_slow (mpz_t, uintmax_t) ARG_NONNULL ((1));
extern void emacs_mpz_mul (mpz_t, mpz_t const, mpz_t const)
  ARG_NONNULL ((1, 2, 3));
extern void emacs_mpz_mul_2exp (mpz_t, mpz_t const, EMACS_INT)
  ARG_NONNULL ((1, 2));
extern void emacs_mpz_pow_ui (mpz_t, mpz_t const, unsigned long)
  ARG_NONNULL ((1, 2));
extern double mpz_get_d_rounded (mpz_t const);
extern Lisp_Object get_random_bignum (struct Lisp_Bignum const *);

/* defined in alloc.c  */
extern void init_gmp_memory_functions (void);

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
  long int i;
  if (FASTER_BIGNUM && !ckd_add (&i, v, 0))
    mpz_set_si (result, i);
  else
    mpz_set_intmax_slow (result, v);
}
INLINE void ARG_NONNULL ((1))
mpz_set_uintmax (mpz_t result, uintmax_t v)
{
  unsigned long int i;
  if (FASTER_BIGNUM && !ckd_add (&i, v, 0))
    mpz_set_ui (result, i);
  else
    mpz_set_uintmax_slow (result, v);
}

/* To convert a bignum to an mpz_t, either use BIGNUM_VAL or bignum_val.
   BIGNUM_VAL can be used to initialize an mpz_t.  E.g.:

     mpz_t const z = BIGNUM_VAL (bignum)

   The typical use for bignum_val is in argument context:

     mpz_neg (foo, bignum_val.z (bignum))

   In both cases, the mpz_t should not me mutated as it shares memory
   with the bignum.  */

#ifdef FLAT_BIGNUMS

#define BIGNUM_VAL(b) \
  MPZ_ROINIT_N ((mp_limb_t *) b->limbs, b->sign_and_size)

struct bignum_val
{
  const mpz_t z;
};

INLINE struct bignum_val
bignum_val (struct Lisp_Bignum const *i)
{
  struct bignum_val val = { BIGNUM_VAL (i) };
  return val;
}

INLINE mpz_t const *
bignum_integer (mpz_t *tmp, Lisp_Object i)
{
  if (FIXNUMP (i))
    mpz_set_intmax (*tmp, XFIXNUM (i));
  else
    mpz_set (*tmp, bignum_val (XBIGNUM (i)).z);
  return tmp;
}

#else

#define BIGNUM_VAL(b)                                          \
  MPZ_ROINIT_N ((mp_limb_t *) mpz_limbs_read (b->value),       \
		(mpz_sgn (b->value) < 0 ? -mpz_size (b->value) \
					: mpz_size (b->value)))
struct bignum_val
{
  mpz_srcptr z;
};

INLINE struct bignum_val
bignum_val (struct Lisp_Bignum const *i)
{
  struct bignum_val val = { i->value };
  return val;
}

/* Return a pointer to an mpz_t that is equal to the Lisp integer I.
   If I is a bignum this returns a pointer to I's representation;
   otherwise this sets *TMP to I's value and returns TMP.  */
INLINE mpz_t const *
bignum_integer (mpz_t *tmp, Lisp_Object i)
{
  if (FIXNUMP (i))
    {
      mpz_set_intmax (*tmp, XFIXNUM (i));
      /* The unnecessary cast pacifies a buggy GCC 4.8.5.  */
      return (mpz_t const *) tmp;
    }
  return &XBIGNUM (i)->value;
}
#endif

#define XBIGNUM_VAL(b) BIGNUM_VAL (XBIGNUM (b))

INLINE struct bignum_val
xbignum_val (Lisp_Object i)
{
  return bignum_val (XBIGNUM (i));
}

/* Set RESULT to the value stored in the Lisp integer I.  If I is a
   big integer, copy it to RESULT.  RESULT must already be
   initialized.  */
INLINE void
mpz_set_integer (mpz_t result, Lisp_Object i)
{
  if (FIXNUMP (i))
    mpz_set_intmax (result, XFIXNUM (i));
  else
    mpz_set (result, xbignum_val (i).z);
}

INLINE_HEADER_END

#endif /* BIGNUM_H */
