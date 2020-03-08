/* Timestamp functions for Emacs

Copyright (C) 1985-1987, 1989, 1993-2020 Free Software Foundation, Inc.

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

#include "systime.h"

#include "blockinput.h"
#include "bignum.h"
#include "coding.h"
#include "lisp.h"
#include "pdumper.h"

#include <strftime.h>

#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_TIMEZONE_T
# include <sys/param.h>
# if defined __NetBSD_Version__ && __NetBSD_Version__ < 700000000
#  define HAVE_TZALLOC_BUG true
# endif
#endif
#ifndef HAVE_TZALLOC_BUG
# define HAVE_TZALLOC_BUG false
#endif

enum { TM_YEAR_BASE = 1900 };

#ifndef HAVE_TM_GMTOFF
# define HAVE_TM_GMTOFF false
#endif

#ifndef TIME_T_MIN
# define TIME_T_MIN TYPE_MINIMUM (time_t)
#endif
#ifndef TIME_T_MAX
# define TIME_T_MAX TYPE_MAXIMUM (time_t)
#endif

/* Compile with -DFASTER_TIMEFNS=0 to disable common optimizations and
   allow easier testing of some slow-path code.  */
#ifndef FASTER_TIMEFNS
# define FASTER_TIMEFNS 1
#endif

/* Whether to warn about Lisp timestamps (TICKS . HZ) that may be
   instances of obsolete-format timestamps (HI . LO) where HI is
   the high-order bits and LO the low-order 16 bits.  Currently this
   is true, but it should change to false in a future version of
   Emacs.  Compile with -DWARN_OBSOLETE_TIMESTAMPS=0 to see what the
   future will be like.  */
#ifndef WARN_OBSOLETE_TIMESTAMPS
enum { WARN_OBSOLETE_TIMESTAMPS = true };
#endif

/* Although current-time etc. generate list-format timestamps
   (HI LO US PS), the plan is to change these functions to generate
   frequency-based timestamps (TICKS . HZ) in a future release.
   To try this now, compile with -DCURRENT_TIME_LIST=0.  */
#ifndef CURRENT_TIME_LIST
enum { CURRENT_TIME_LIST = true };
#endif

#if FIXNUM_OVERFLOW_P (1000000000)
static Lisp_Object timespec_hz;
#else
# define timespec_hz make_fixnum (TIMESPEC_HZ)
#endif

#define TRILLION 1000000000000
#if FIXNUM_OVERFLOW_P (TRILLION)
static Lisp_Object trillion;
# define ztrillion (*xbignum_val (trillion))
#else
# define trillion make_fixnum (TRILLION)
# if ULONG_MAX < TRILLION || !FASTER_TIMEFNS
mpz_t ztrillion;
# endif
#endif

/* True if the nonzero Lisp integer HZ divides evenly into a trillion.  */
static bool
trillion_factor (Lisp_Object hz)
{
  if (FASTER_TIMEFNS)
    {
      if (FIXNUMP (hz))
	return TRILLION % XFIXNUM (hz) == 0;
      if (!FIXNUM_OVERFLOW_P (TRILLION))
	return false;
    }
  verify (TRILLION <= INTMAX_MAX);
  intmax_t ihz;
  return integer_to_intmax (hz, &ihz) && TRILLION % ihz == 0;
}

/* Return a struct timeval that is roughly equivalent to T.
   Use the least timeval not less than T.
   Return an extremal value if the result would overflow.  */
struct timeval
make_timeval (struct timespec t)
{
  struct timeval tv;
  tv.tv_sec = t.tv_sec;
  tv.tv_usec = t.tv_nsec / 1000;

  if (t.tv_nsec % 1000 != 0)
    {
      if (tv.tv_usec < 999999)
	tv.tv_usec++;
      else if (tv.tv_sec < TIME_T_MAX)
	{
	  tv.tv_sec++;
	  tv.tv_usec = 0;
	}
    }

  return tv;
}

/* Yield A's UTC offset, or an unspecified value if unknown.  */
static long int
tm_gmtoff (struct tm *a)
{
#if HAVE_TM_GMTOFF
  return a->tm_gmtoff;
#else
  return 0;
#endif
}

/* Yield A - B, measured in seconds.
   This function is copied from the GNU C Library.  */
static int
tm_diff (struct tm *a, struct tm *b)
{
  /* Compute intervening leap days correctly even if year is negative.
     Take care to avoid int overflow in leap day calculations,
     but it's OK to assume that A and B are close to each other.  */
  int a4 = (a->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (a->tm_year & 3);
  int b4 = (b->tm_year >> 2) + (TM_YEAR_BASE >> 2) - ! (b->tm_year & 3);
  int a100 = a4 / 25 - (a4 % 25 < 0);
  int b100 = b4 / 25 - (b4 % 25 < 0);
  int a400 = a100 >> 2;
  int b400 = b100 >> 2;
  int intervening_leap_days = (a4 - b4) - (a100 - b100) + (a400 - b400);
  int years = a->tm_year - b->tm_year;
  int days = (365 * years + intervening_leap_days
	      + (a->tm_yday - b->tm_yday));
  return (60 * (60 * (24 * days + (a->tm_hour - b->tm_hour))
		+ (a->tm_min - b->tm_min))
	  + (a->tm_sec - b->tm_sec));
}

enum { tzeqlen = sizeof "TZ=" - 1 };

/* Time zones equivalent to current local time and to UTC, respectively.  */
static timezone_t local_tz;
static timezone_t const utc_tz = 0;

static struct tm *
emacs_localtime_rz (timezone_t tz, time_t const *t, struct tm *tm)
{
  tm = localtime_rz (tz, t, tm);
  if (!tm && errno == ENOMEM)
    memory_full (SIZE_MAX);
  return tm;
}

static AVOID
invalid_time_zone_specification (Lisp_Object zone)
{
  xsignal2 (Qerror, build_string ("Invalid time zone specification"), zone);
}

/* Free a timezone, except do not free the time zone for local time.
   Freeing utc_tz is also a no-op.  */
static void
xtzfree (timezone_t tz)
{
  if (tz != local_tz)
    tzfree (tz);
}

/* Convert the Lisp time zone rule ZONE to a timezone_t object.
   The returned value either is 0, or is LOCAL_TZ, or is newly allocated.
   If SETTZ, set Emacs local time to the time zone rule; otherwise,
   the caller should eventually pass the returned value to xtzfree.  */
static timezone_t
tzlookup (Lisp_Object zone, bool settz)
{
  static char const tzbuf_format[] = "<%+.*"pI"d>%s%"pI"d:%02d:%02d";
  char const *trailing_tzbuf_format = tzbuf_format + sizeof "<%+.*"pI"d" - 1;
  char tzbuf[sizeof tzbuf_format + 2 * INT_STRLEN_BOUND (EMACS_INT)];
  char const *zone_string;
  timezone_t new_tz;

  if (NILP (zone))
    return local_tz;
  else if (EQ (zone, Qt) || EQ (zone, make_fixnum (0)))
    {
      zone_string = "UTC0";
      new_tz = utc_tz;
    }
  else
    {
      bool plain_integer = FIXNUMP (zone);

      if (EQ (zone, Qwall))
	zone_string = 0;
      else if (STRINGP (zone))
	zone_string = SSDATA (ENCODE_SYSTEM (zone));
      else if (plain_integer || (CONSP (zone) && FIXNUMP (XCAR (zone))
				 && CONSP (XCDR (zone))))
	{
	  Lisp_Object abbr UNINIT;
	  if (!plain_integer)
	    {
	      abbr = XCAR (XCDR (zone));
	      zone = XCAR (zone);
	    }

	  EMACS_INT abszone = eabs (XFIXNUM (zone)), hour = abszone / (60 * 60);
	  int hour_remainder = abszone % (60 * 60);
	  int min = hour_remainder / 60, sec = hour_remainder % 60;

	  if (plain_integer)
	    {
	      int prec = 2;
	      EMACS_INT numzone = hour;
	      if (hour_remainder != 0)
		{
		  prec += 2, numzone = 100 * numzone + min;
		  if (sec != 0)
		    prec += 2, numzone = 100 * numzone + sec;
		}
	      sprintf (tzbuf, tzbuf_format, prec,
		       XFIXNUM (zone) < 0 ? -numzone : numzone,
		       &"-"[XFIXNUM (zone) < 0], hour, min, sec);
	      zone_string = tzbuf;
	    }
	  else
	    {
	      AUTO_STRING (leading, "<");
	      AUTO_STRING_WITH_LEN (trailing, tzbuf,
				    sprintf (tzbuf, trailing_tzbuf_format,
					     &"-"[XFIXNUM (zone) < 0],
					     hour, min, sec));
	      zone_string = SSDATA (concat3 (leading, ENCODE_SYSTEM (abbr),
					     trailing));
	    }
	}
      else
	invalid_time_zone_specification (zone);

      new_tz = tzalloc (zone_string);

      if (HAVE_TZALLOC_BUG && !new_tz && errno != ENOMEM && plain_integer
	  && XFIXNUM (zone) % (60 * 60) == 0)
	{
	  /* tzalloc mishandles POSIX strings; fall back on tzdb if
	     possible (Bug#30738).  */
	  sprintf (tzbuf, "Etc/GMT%+"pI"d", - (XFIXNUM (zone) / (60 * 60)));
	  new_tz = tzalloc (zone_string);
	}

      if (!new_tz)
	{
	  if (errno == ENOMEM)
	    memory_full (SIZE_MAX);
	  invalid_time_zone_specification (zone);
	}
    }

  if (settz)
    {
      block_input ();
      emacs_setenv_TZ (zone_string);
      tzset ();
      timezone_t old_tz = local_tz;
      local_tz = new_tz;
      tzfree (old_tz);
      unblock_input ();
    }

  return new_tz;
}

void
init_timefns (void)
{
#ifdef HAVE_UNEXEC
  /* A valid but unlikely setting for the TZ environment variable.
     It is OK (though a bit slower) if the user chooses this value.  */
  static char dump_tz_string[] = "TZ=UtC0";

  /* When just dumping out, set the time zone to a known unlikely value
     and skip the rest of this function.  */
  if (will_dump_with_unexec_p ())
    {
      xputenv (dump_tz_string);
      tzset ();
      return;
    }
#endif

  char *tz = getenv ("TZ");

#ifdef HAVE_UNEXEC
  /* If the execution TZ happens to be the same as the dump TZ,
     change it to some other value and then change it back,
     to force the underlying implementation to reload the TZ info.
     This is needed on implementations that load TZ info from files,
     since the TZ file contents may differ between dump and execution.  */
  if (tz && strcmp (tz, &dump_tz_string[tzeqlen]) == 0)
    {
      ++*tz;
      tzset ();
      --*tz;
    }
#endif

  /* Set the time zone rule now, so that the call to putenv is done
     before multiple threads are active.  */
  tzlookup (tz ? build_string (tz) : Qwall, true);
}

/* Report that a time value is out of range for Emacs.  */
void
time_overflow (void)
{
  error ("Specified time is not representable");
}

static AVOID
time_error (int err)
{
  switch (err)
    {
    case ENOMEM: memory_full (SIZE_MAX);
    case EOVERFLOW: time_overflow ();
    default: error ("Invalid time specification");
    }
}

static AVOID
invalid_hz (Lisp_Object hz)
{
  xsignal2 (Qerror, build_string ("Invalid time frequency"), hz);
}

/* Return the upper part of the time T (everything but the bottom 16 bits).  */
static Lisp_Object
hi_time (time_t t)
{
  return INT_TO_INTEGER (t >> LO_TIME_BITS);
}

/* Return the bottom bits of the time T.  */
static Lisp_Object
lo_time (time_t t)
{
  return make_fixnum (t & ((1 << LO_TIME_BITS) - 1));
}

/* When converting a double to a fraction TICKS / HZ, HZ is equal to
   FLT_RADIX * P where 0 <= P < FLT_RADIX_POWER_SIZE.  The tiniest
   nonzero double uses the maximum P.  */
enum { flt_radix_power_size = DBL_MANT_DIG - DBL_MIN_EXP + 1 };

/* A integer vector of size flt_radix_power_size.  The Pth entry
   equals FLT_RADIX**P.  */
static Lisp_Object flt_radix_power;

/* Convert T into an Emacs time *RESULT, truncating toward minus infinity.
   Return zero if successful, an error number otherwise.  */
static int
decode_float_time (double t, struct lisp_time *result)
{
  Lisp_Object ticks, hz;
  if (t == 0)
    {
      ticks = make_fixnum (0);
      hz = make_fixnum (1);
    }
  else
    {
      int scale = double_integer_scale (t);

      if (scale < 0)
	{
	 /* T is finite but so large that HZ would be less than 1 if
	    T's precision were represented exactly.  SCALE must be
	    nonnegative, as the (TICKS . HZ) representation requires
	    HZ to be at least 1.  So use SCALE = 0, which converts T to
	    (T . 1), which is the exact numeric value with too-large HZ,
	    which is typically better than signaling overflow.  */
	  scale = 0;
	}
      else if (flt_radix_power_size <= scale)
	return isnan (t) ? EDOM : EOVERFLOW;

      /* Compute TICKS, HZ such that TICKS / HZ exactly equals T, where HZ is
	 T's frequency or 1, whichever is greater.  Here, “frequency” means
	 1/precision.  Cache HZ values in flt_radix_power.  */
      double scaled = scalbn (t, scale);
      eassert (trunc (scaled) == scaled);
      ticks = double_to_integer (scaled);
      hz = AREF (flt_radix_power, scale);
      if (NILP (hz))
	{
	  mpz_ui_pow_ui (mpz[0], FLT_RADIX, scale);
	  hz = make_integer_mpz ();
	  ASET (flt_radix_power, scale, hz);
	}
    }
  result->ticks = ticks;
  result->hz = hz;
  return 0;
}

/* Make a 4-element timestamp (HI LO US PS) from TICKS and HZ.
   Drop any excess precision.  */
static Lisp_Object
ticks_hz_list4 (Lisp_Object ticks, Lisp_Object hz)
{
  /* mpz[0] = floor ((ticks * trillion) / hz).  */
  mpz_t const *zticks = bignum_integer (&mpz[0], ticks);
#if FASTER_TIMEFNS && TRILLION <= ULONG_MAX
  mpz_mul_ui (mpz[0], *zticks, TRILLION);
#else
  mpz_mul (mpz[0], *zticks, ztrillion);
#endif
  mpz_fdiv_q (mpz[0], mpz[0], *bignum_integer (&mpz[1], hz));

  /* mpz[0] = floor (mpz[0] / trillion), with US = the high six digits of the
     12-digit remainder, and PS = the low six digits.  */
#if FASTER_TIMEFNS && TRILLION <= ULONG_MAX
  unsigned long int fullps = mpz_fdiv_q_ui (mpz[0], mpz[0], TRILLION);
  int us = fullps / 1000000;
  int ps = fullps % 1000000;
#else
  mpz_fdiv_qr (mpz[0], mpz[1], mpz[0], ztrillion);
  int ps = mpz_fdiv_q_ui (mpz[1], mpz[1], 1000000);
  int us = mpz_get_ui (mpz[1]);
#endif

  /* mpz[0] = floor (mpz[0] / 1 << LO_TIME_BITS), with lo = remainder.  */
  unsigned long ulo = mpz_get_ui (mpz[0]);
  if (mpz_sgn (mpz[0]) < 0)
    ulo = -ulo;
  int lo = ulo & ((1 << LO_TIME_BITS) - 1);
  mpz_fdiv_q_2exp (mpz[0], mpz[0], LO_TIME_BITS);

  return list4 (make_integer_mpz (), make_fixnum (lo),
		make_fixnum (us), make_fixnum (ps));
}

/* Set ROP to T.  */
static void
mpz_set_time (mpz_t rop, time_t t)
{
  if (EXPR_SIGNED (t))
    mpz_set_intmax (rop, t);
  else
    mpz_set_uintmax (rop, t);
}

/* Store into mpz[0] a clock tick count for T, assuming a
   TIMESPEC_HZ-frequency clock.  Use mpz[1] as a temp.  */
static void
timespec_mpz (struct timespec t)
{
  /* mpz[0] = sec * TIMESPEC_HZ + nsec.  */
  mpz_set_ui (mpz[0], t.tv_nsec);
  mpz_set_time (mpz[1], t.tv_sec);
  mpz_addmul_ui (mpz[0], mpz[1], TIMESPEC_HZ);
}

/* Convert T to a Lisp integer counting TIMESPEC_HZ ticks.  */
static Lisp_Object
timespec_ticks (struct timespec t)
{
  /* For speed, use intmax_t arithmetic if it will do.  */
  intmax_t accum;
  if (FASTER_TIMEFNS
      && !INT_MULTIPLY_WRAPV (t.tv_sec, TIMESPEC_HZ, &accum)
      && !INT_ADD_WRAPV (t.tv_nsec, accum, &accum))
    return make_int (accum);

  /* Fall back on bignum arithmetic.  */
  timespec_mpz (t);
  return make_integer_mpz ();
}

/* Convert T to a Lisp integer counting HZ ticks, taking the floor.
   Assume T is valid, but check HZ.  */
static Lisp_Object
lisp_time_hz_ticks (struct lisp_time t, Lisp_Object hz)
{
  /* The idea is to return the floor of ((T.ticks * HZ) / T.hz).  */

  /* For speed, just return T.ticks if T.hz == HZ.  */
  if (FASTER_TIMEFNS && EQ (t.hz, hz))
    return t.ticks;

  /* Check HZ for validity.  */
  if (FIXNUMP (hz))
    {
      if (XFIXNUM (hz) <= 0)
	invalid_hz (hz);

      /* For speed, use intmax_t arithmetic if it will do.  */
      intmax_t ticks;
      if (FASTER_TIMEFNS && FIXNUMP (t.ticks) && FIXNUMP (t.hz)
	  && !INT_MULTIPLY_WRAPV (XFIXNUM (t.ticks), XFIXNUM (hz), &ticks))
	return make_int (ticks / XFIXNUM (t.hz)
			 - (ticks % XFIXNUM (t.hz) < 0));
    }
  else if (! (BIGNUMP (hz) && 0 < mpz_sgn (*xbignum_val (hz))))
    invalid_hz (hz);

  /* Fall back on bignum arithmetic.  */
  mpz_mul (mpz[0],
	   *bignum_integer (&mpz[0], t.ticks),
	   *bignum_integer (&mpz[1], hz));
  mpz_fdiv_q (mpz[0], mpz[0], *bignum_integer (&mpz[1], t.hz));
  return make_integer_mpz ();
}

/* Convert T to a Lisp integer counting seconds, taking the floor.  */
static Lisp_Object
lisp_time_seconds (struct lisp_time t)
{
  /* The idea is to return the floor of T.ticks / T.hz.  */

  if (!FASTER_TIMEFNS)
    return lisp_time_hz_ticks (t, make_fixnum (1));

  /* For speed, use EMACS_INT arithmetic if it will do.  */
  if (FIXNUMP (t.ticks) && FIXNUMP (t.hz))
    return make_fixnum (XFIXNUM (t.ticks) / XFIXNUM (t.hz)
			- (XFIXNUM (t.ticks) % XFIXNUM (t.hz) < 0));

  /* For speed, inline what lisp_time_hz_ticks would do.  */
  mpz_fdiv_q (mpz[0],
	      *bignum_integer (&mpz[0], t.ticks),
	      *bignum_integer (&mpz[1], t.hz));
  return make_integer_mpz ();
}

/* Convert T to a Lisp timestamp.  */
Lisp_Object
make_lisp_time (struct timespec t)
{
  if (CURRENT_TIME_LIST)
    {
      time_t s = t.tv_sec;
      int ns = t.tv_nsec;
      return list4 (hi_time (s), lo_time (s),
		    make_fixnum (ns / 1000), make_fixnum (ns % 1000 * 1000));
    }
  else
    return timespec_to_lisp (t);
}

/* Return (TICKS . HZ) for time T.  */
Lisp_Object
timespec_to_lisp (struct timespec t)
{
  return Fcons (timespec_ticks (t), timespec_hz);
}

/* Return NUMERATOR / DENOMINATOR, rounded to the nearest double.
   Arguments must be Lisp integers, and DENOMINATOR must be nonzero.  */
static double
frac_to_double (Lisp_Object numerator, Lisp_Object denominator)
{
  intmax_t intmax_numerator;
  if (FASTER_TIMEFNS && EQ (denominator, make_fixnum (1))
      && integer_to_intmax (numerator, &intmax_numerator))
    return intmax_numerator;

  /* Compute number of base-FLT_RADIX digits in numerator and denominator.  */
  mpz_t const *n = bignum_integer (&mpz[0], numerator);
  mpz_t const *d = bignum_integer (&mpz[1], denominator);
  ptrdiff_t nbits = mpz_sizeinbase (*n, 2);
  ptrdiff_t dbits = mpz_sizeinbase (*d, 2);
  eassume (0 < nbits);
  eassume (0 < dbits);
  ptrdiff_t ndig = (nbits + LOG2_FLT_RADIX - 1) / LOG2_FLT_RADIX;
  ptrdiff_t ddig = (dbits + LOG2_FLT_RADIX - 1) / LOG2_FLT_RADIX;

  /* Scale with SCALE when doing integer division.  That is, compute
     (N * FLT_RADIX**SCALE) / D [or, if SCALE is negative, N / (D *
     FLT_RADIX**-SCALE)] as a bignum, convert the bignum to double,
     then divide the double by FLT_RADIX**SCALE.  First scale N
     (or scale D, if SCALE is negative) ...  */
  ptrdiff_t scale = ddig - ndig + DBL_MANT_DIG + 1;
  if (scale < 0)
    {
      mpz_mul_2exp (mpz[1], *d, - (scale * LOG2_FLT_RADIX));
      d = &mpz[1];
    }
  else
    {
      /* min so we don't scale tiny numbers as if they were normalized.  */
      scale = min (scale, flt_radix_power_size - 1);

      mpz_mul_2exp (mpz[0], *n, scale * LOG2_FLT_RADIX);
      n = &mpz[0];
    }
  /* ... and then divide, with quotient Q and remainder R.  */
  mpz_t *q = &mpz[2];
  mpz_t *r = &mpz[3];
  mpz_tdiv_qr (*q, *r, *n, *d);

  /* The amount to add to the absolute value of Q so that truncating
     it to double will round correctly.  */
  int incr;

  /* Round the quotient before converting it to double.
     If the quotient is less than FLT_RADIX ** DBL_MANT_DIG,
     round to the nearest integer; otherwise, it is less than
     FLT_RADIX ** (DBL_MANT_DIG + 1) and round it to the nearest
     multiple of FLT_RADIX.  Break ties to even.  */
  if (mpz_sizeinbase (*q, 2) < DBL_MANT_DIG * LOG2_FLT_RADIX)
    {
      /* Converting to double will use the whole quotient so add 1 to
	 its absolute value as per round-to-even; i.e., if the doubled
	 remainder exceeds the denominator, or exactly equals the
	 denominator and adding 1 would make the quotient even.  */
      mpz_mul_2exp (*r, *r, 1);
      int cmp = mpz_cmpabs (*r, *d);
      incr = cmp > 0 || (cmp == 0 && (FASTER_TIMEFNS && FLT_RADIX == 2
				      ? mpz_odd_p (*q)
				      : mpz_tdiv_ui (*q, FLT_RADIX) & 1));
    }
  else
    {
      /* Converting to double will discard the quotient's low-order digit,
	 so add FLT_RADIX to its absolute value as per round-to-even.  */
      int lo_2digits = mpz_tdiv_ui (*q, FLT_RADIX * FLT_RADIX);
      eassume (0 <= lo_2digits && lo_2digits < FLT_RADIX * FLT_RADIX);
      int lo_digit = lo_2digits % FLT_RADIX;
      incr = ((lo_digit > FLT_RADIX / 2
	       || (lo_digit == FLT_RADIX / 2 && FLT_RADIX % 2 == 0
		   && ((lo_2digits / FLT_RADIX) & 1
		       || mpz_sgn (*r) != 0)))
	      ? FLT_RADIX : 0);
    }

  /* Increment the absolute value of the quotient by INCR.  */
  if (!FASTER_TIMEFNS || incr != 0)
    (mpz_sgn (*n) < 0 ? mpz_sub_ui : mpz_add_ui) (*q, *q, incr);

  /* Rescale the integer Q back to double.  This step does not round.  */
  return scalbn (mpz_get_d (*q), -scale);
}

/* From a valid timestamp (TICKS . HZ), generate the corresponding
   time values.

   If RESULT is not null, store into *RESULT the converted time.
   Otherwise, store into *DRESULT the number of seconds since the
   start of the POSIX Epoch.

   Return zero, which indicates success.  */
static int
decode_ticks_hz (Lisp_Object ticks, Lisp_Object hz,
		 struct lisp_time *result, double *dresult)
{
  if (result)
    {
      result->ticks = ticks;
      result->hz = hz;
    }
  else
    *dresult = frac_to_double (ticks, hz);
  return 0;
}

/* Lisp timestamp classification.  */
enum timeform
  {
   TIMEFORM_INVALID = 0,
   TIMEFORM_HI_LO, /* seconds in the form (HI << LO_TIME_BITS) + LO.  */
   TIMEFORM_HI_LO_US, /* seconds plus microseconds (HI LO US) */
   TIMEFORM_NIL, /* current time in nanoseconds */
   TIMEFORM_HI_LO_US_PS, /* seconds plus micro and picoseconds (HI LO US PS) */
   TIMEFORM_FLOAT, /* time as a float */
   TIMEFORM_TICKS_HZ /* fractional time: HI is ticks, LO is ticks per second */
  };

/* From the valid form FORM and the time components HIGH, LOW, USEC
   and PSEC, generate the corresponding time value.  If LOW is
   floating point, the other components should be zero and FORM should
   not be TIMEFORM_TICKS_HZ.

   If RESULT is not null, store into *RESULT the converted time.
   Otherwise, store into *DRESULT the number of seconds since the
   start of the POSIX Epoch.  Unsuccessful calls may or may not store
   results.

   Return zero if successful, an error number otherwise.  */
static int
decode_time_components (enum timeform form,
			Lisp_Object high, Lisp_Object low,
			Lisp_Object usec, Lisp_Object psec,
			struct lisp_time *result, double *dresult)
{
  switch (form)
    {
    case TIMEFORM_INVALID:
      return EINVAL;

    case TIMEFORM_TICKS_HZ:
      if (INTEGERP (high)
	  && (!NILP (Fnatnump (low)) && !EQ (low, make_fixnum (0))))
	return decode_ticks_hz (high, low, result, dresult);
      return EINVAL;

    case TIMEFORM_FLOAT:
      {
	double t = XFLOAT_DATA (low);
	if (result)
	  return decode_float_time (t, result);
	else
	  {
	    *dresult = t;
	    return 0;
	  }
      }

    case TIMEFORM_NIL:
      return decode_ticks_hz (timespec_ticks (current_timespec ()),
			      timespec_hz, result, dresult);

    default:
      break;
    }

  if (! (INTEGERP (high) && INTEGERP (low)
	 && FIXNUMP (usec) && FIXNUMP (psec)))
    return EINVAL;
  EMACS_INT us = XFIXNUM (usec);
  EMACS_INT ps = XFIXNUM (psec);

  /* Normalize out-of-range lower-order components by carrying
     each overflow into the next higher-order component.  */
  us += ps / 1000000 - (ps % 1000000 < 0);
  mpz_set_intmax (mpz[0], us / 1000000 - (us % 1000000 < 0));
  mpz_add (mpz[0], mpz[0], *bignum_integer (&mpz[1], low));
  mpz_addmul_ui (mpz[0], *bignum_integer (&mpz[1], high), 1 << LO_TIME_BITS);
  ps = ps % 1000000 + 1000000 * (ps % 1000000 < 0);
  us = us % 1000000 + 1000000 * (us % 1000000 < 0);

  if (result)
    {
      switch (form)
	{
	case TIMEFORM_HI_LO:
	  /* Floats and nil were handled above, so it was an integer.  */
	  result->hz = make_fixnum (1);
	  break;

	case TIMEFORM_HI_LO_US:
	  mpz_mul_ui (mpz[0], mpz[0], 1000000);
	  mpz_add_ui (mpz[0], mpz[0], us);
	  result->hz = make_fixnum (1000000);
	  break;

	case TIMEFORM_HI_LO_US_PS:
	  mpz_mul_ui (mpz[0], mpz[0], 1000000);
	  mpz_add_ui (mpz[0], mpz[0], us);
	  mpz_mul_ui (mpz[0], mpz[0], 1000000);
	  mpz_add_ui (mpz[0], mpz[0], ps);
	  result->hz = trillion;
	  break;

	default:
	  eassume (false);
	}
      result->ticks = make_integer_mpz ();
    }
  else
    *dresult = mpz_get_d (mpz[0]) + (us * 1e6L + ps) / 1e12L;

  return 0;
}

enum { DECODE_SECS_ONLY = WARN_OBSOLETE_TIMESTAMPS + 1 };

/* Decode a Lisp timestamp SPECIFIED_TIME that represents a time.

   FLAGS specifies conversion flags.  If FLAGS & DECODE_SECS_ONLY,
   ignore and do not validate any sub-second components of an
   old-format SPECIFIED_TIME.  If FLAGS & WARN_OBSOLETE_TIMESTAMPS,
   diagnose what could be obsolete (HIGH . LOW) timestamps.

   If RESULT is not null, store into *RESULT the converted time;
   otherwise, store into *DRESULT the number of seconds since the
   start of the POSIX Epoch.  Unsuccessful calls may or may not store
   results.

   Return the form of SPECIFIED-TIME.  Signal an error if unsuccessful.  */
static enum timeform
decode_lisp_time (Lisp_Object specified_time, int flags,
		  struct lisp_time *result, double *dresult)
{
  Lisp_Object high = make_fixnum (0);
  Lisp_Object low = specified_time;
  Lisp_Object usec = make_fixnum (0);
  Lisp_Object psec = make_fixnum (0);
  enum timeform form = TIMEFORM_HI_LO;

  if (NILP (specified_time))
    form = TIMEFORM_NIL;
  else if (FLOATP (specified_time))
    form = TIMEFORM_FLOAT;
  else if (CONSP (specified_time))
    {
      high = XCAR (specified_time);
      low = XCDR (specified_time);
      if (CONSP (low))
	{
	  Lisp_Object low_tail = XCDR (low);
	  low = XCAR (low);
	  if (! (flags & DECODE_SECS_ONLY))
	    {
	      if (CONSP (low_tail))
		{
		  usec = XCAR (low_tail);
		  low_tail = XCDR (low_tail);
		  if (CONSP (low_tail))
		    {
		      psec = XCAR (low_tail);
		      form = TIMEFORM_HI_LO_US_PS;
		    }
		  else
		    form = TIMEFORM_HI_LO_US;
		}
	      else if (!NILP (low_tail))
		{
		  usec = low_tail;
		  form = TIMEFORM_HI_LO_US;
		}
	    }
	}
      else
	{
	  if (flags & WARN_OBSOLETE_TIMESTAMPS
	      && RANGED_FIXNUMP (0, low, (1 << LO_TIME_BITS) - 1))
	    message ("obsolete timestamp with cdr %"pI"d", XFIXNUM (low));
	  form = TIMEFORM_TICKS_HZ;
	}

      /* Require LOW to be an integer, as otherwise the computation
	 would be considerably trickier.  */
      if (! INTEGERP (low))
	form = TIMEFORM_INVALID;
    }

  int err = decode_time_components (form, high, low, usec, psec,
				    result, dresult);
  if (err)
    time_error (err);
  return form;
}

/* Convert Z to time_t, returning true if it fits.  */
static bool
mpz_time (mpz_t const z, time_t *t)
{
  if (TYPE_SIGNED (time_t))
    {
      intmax_t i;
      if (! (mpz_to_intmax (z, &i) && TIME_T_MIN <= i && i <= TIME_T_MAX))
	return false;
      *t = i;
    }
  else
    {
      uintmax_t i;
      if (! (mpz_to_uintmax (z, &i) && i <= TIME_T_MAX))
	return false;
      *t = i;
    }
  return true;
}

/* Convert T to struct timespec, returning an invalid timespec
   if T does not fit.  */
static struct timespec
lisp_to_timespec (struct lisp_time t)
{
  struct timespec result = invalid_timespec ();
  int ns;
  mpz_t *q = &mpz[0];
  mpz_t const *qt = q;

  /* Floor-divide (T.ticks * TIMESPEC_HZ) by T.hz,
     yielding quotient Q (tv_sec) and remainder NS (tv_nsec).
     Return an invalid timespec if Q does not fit in time_t.
     For speed, prefer fixnum arithmetic if it works.  */
  if (FASTER_TIMEFNS && EQ (t.hz, timespec_hz))
    {
      if (FIXNUMP (t.ticks))
	{
	  EMACS_INT s = XFIXNUM (t.ticks) / TIMESPEC_HZ;
	  ns = XFIXNUM (t.ticks) % TIMESPEC_HZ;
	  if (ns < 0)
	    s--, ns += TIMESPEC_HZ;
	  if ((TYPE_SIGNED (time_t) ? TIME_T_MIN <= s : 0 <= s)
	      && s <= TIME_T_MAX)
	    {
	      result.tv_sec = s;
	      result.tv_nsec = ns;
	    }
	  return result;
	}
      else
	ns = mpz_fdiv_q_ui (*q, *xbignum_val (t.ticks), TIMESPEC_HZ);
    }
  else if (FASTER_TIMEFNS && EQ (t.hz, make_fixnum (1)))
    {
      ns = 0;
      if (FIXNUMP (t.ticks))
	{
	  EMACS_INT s = XFIXNUM (t.ticks);
	  if ((TYPE_SIGNED (time_t) ? TIME_T_MIN <= s : 0 <= s)
	      && s <= TIME_T_MAX)
	    {
	      result.tv_sec = s;
	      result.tv_nsec = ns;
	    }
	  return result;
	}
      else
	qt = xbignum_val (t.ticks);
    }
  else
    {
      mpz_mul_ui (*q, *bignum_integer (q, t.ticks), TIMESPEC_HZ);
      mpz_fdiv_q (*q, *q, *bignum_integer (&mpz[1], t.hz));
      ns = mpz_fdiv_q_ui (*q, *q, TIMESPEC_HZ);
    }

  /* Check that Q fits in time_t, not merely in T.tv_sec.  With some versions
     of MinGW, tv_sec is a 64-bit type, whereas time_t is a 32-bit type.  */
  time_t sec;
  if (mpz_time (*qt, &sec))
    {
      result.tv_sec = sec;
      result.tv_nsec = ns;
    }
  return result;
}

/* Convert (HIGH LOW USEC PSEC) to struct timespec.
   Return true if successful.  */
bool
list4_to_timespec (Lisp_Object high, Lisp_Object low,
		   Lisp_Object usec, Lisp_Object psec,
		   struct timespec *result)
{
  struct lisp_time t;
  if (decode_time_components (TIMEFORM_HI_LO_US_PS, high, low, usec, psec,
			      &t, 0))
    return false;
  *result = lisp_to_timespec (t);
  return timespec_valid_p (*result);
}

/* Decode a Lisp list SPECIFIED_TIME that represents a time.
   If SPECIFIED_TIME is nil, use the current time.
   Signal an error if SPECIFIED_TIME does not represent a time.
   If PFORM, store the time's form into *PFORM.  */
static struct lisp_time
lisp_time_struct (Lisp_Object specified_time, enum timeform *pform)
{
  struct lisp_time t;
  enum timeform form
    = decode_lisp_time (specified_time, WARN_OBSOLETE_TIMESTAMPS, &t, 0);
  if (pform)
    *pform = form;
  return t;
}

/* Decode a Lisp list SPECIFIED_TIME that represents a time.
   Discard any low-order (sub-ns) resolution.
   If SPECIFIED_TIME is nil, use the current time.
   Signal an error if SPECIFIED_TIME does not represent a timespec.  */
struct timespec
lisp_time_argument (Lisp_Object specified_time)
{
  struct lisp_time lt = lisp_time_struct (specified_time, 0);
  struct timespec t = lisp_to_timespec (lt);
  if (! timespec_valid_p (t))
    time_overflow ();
  return t;
}

/* Like lisp_time_argument, except decode only the seconds part, and
   do not check the subseconds part.  */
static time_t
lisp_seconds_argument (Lisp_Object specified_time)
{
  int flags = WARN_OBSOLETE_TIMESTAMPS | DECODE_SECS_ONLY;
  struct lisp_time lt;
  decode_lisp_time (specified_time, flags, &lt, 0);
  struct timespec t = lisp_to_timespec (lt);
  if (! timespec_valid_p (t))
    time_overflow ();
  return t.tv_sec;
}

/* Return the sum of the Lisp integers A and B.
   Subtract instead of adding if SUBTRACT.
   This function is tuned for small B.  */
static Lisp_Object
lispint_arith (Lisp_Object a, Lisp_Object b, bool subtract)
{
  bool mpz_done = false;

  if (FASTER_TIMEFNS && FIXNUMP (b))
    {
      if (EQ (b, make_fixnum (0)))
	return a;

      /* For speed, use EMACS_INT arithmetic if it will do.  */
      if (FIXNUMP (a))
	return make_int (subtract
			 ? XFIXNUM (a) - XFIXNUM (b)
			 : XFIXNUM (a) + XFIXNUM (b));

      /* For speed, use mpz_add_ui/mpz_sub_ui if it will do.  */
      if (eabs (XFIXNUM (b)) <= ULONG_MAX)
	{
	  ((XFIXNUM (b) < 0) == subtract ? mpz_add_ui : mpz_sub_ui)
	    (mpz[0], *xbignum_val (a), eabs (XFIXNUM (b)));
	  mpz_done = true;
	}
    }

  /* Fall back on bignum arithmetic if necessary.  */
  if (!mpz_done)
    (subtract ? mpz_sub : mpz_add) (mpz[0],
				    *bignum_integer (&mpz[0], a),
				    *bignum_integer (&mpz[1], b));
  return make_integer_mpz ();
}

/* Given Lisp operands A and B, add their values, and return the
   result as a Lisp timestamp.  Subtract instead of adding if SUBTRACT.  */
static Lisp_Object
time_arith (Lisp_Object a, Lisp_Object b, bool subtract)
{
  if (FLOATP (a) && !isfinite (XFLOAT_DATA (a)))
    {
      double da = XFLOAT_DATA (a);
      double db = XFLOAT_DATA (Ffloat_time (b));
      return make_float (subtract ? da - db : da + db);
    }
  enum timeform aform, bform;
  struct lisp_time ta = lisp_time_struct (a, &aform);

  if (FLOATP (b) && !isfinite (XFLOAT_DATA (b)))
    return subtract ? make_float (-XFLOAT_DATA (b)) : b;

  /* Subtract nil from nil correctly, and handle other eq values
     quicker while we're at it.  Compare here rather than earlier, to
     handle NaNs and check formats.  */
  struct lisp_time tb;
  if (EQ (a, b))
    bform = aform, tb = ta;
  else
    tb = lisp_time_struct (b, &bform);

  Lisp_Object ticks, hz;

  if (FASTER_TIMEFNS && EQ (ta.hz, tb.hz))
    {
      hz = ta.hz;
      ticks = lispint_arith (ta.ticks, tb.ticks, subtract);
    }
  else
    {
      /* The plan is to decompose ta into na/da and tb into nb/db.
	 Start by computing da and db, their minimum (which will be
	 needed later) and the iticks temporary that will become
	 available once only their minimum is needed.  */
      mpz_t const *da = bignum_integer (&mpz[1], ta.hz);
      mpz_t const *db = bignum_integer (&mpz[2], tb.hz);
      bool da_lt_db = mpz_cmp (*da, *db) < 0;
      mpz_t const *hzmin = da_lt_db ? da : db;
      mpz_t *iticks = &mpz[da_lt_db + 1];

      /* The plan is to compute (na * (db/g) + nb * (da/g)) / lcm (da, db)
	 where g = gcd (da, db).  Start by computing g.  */
      mpz_t *g = &mpz[3];
      mpz_gcd (*g, *da, *db);

      /* fa = da/g, fb = db/g.  */
      mpz_t *fa = &mpz[4], *fb = &mpz[3];
      mpz_divexact (*fa, *da, *g);
      mpz_divexact (*fb, *db, *g);

      /* ihz = fa * db.  This is equal to lcm (da, db).  */
      mpz_t *ihz = &mpz[0];
      mpz_mul (*ihz, *fa, *db);

      /* When warning about obsolete timestamps, if the smaller
	 denominator comes from a non-(TICKS . HZ) timestamp and could
	 generate a (TICKS . HZ) timestamp that would look obsolete,
	 arrange for the result to have a higher HZ to avoid a
	 spurious warning by a later consumer of this function's
	 returned value.  */
      verify (1 << LO_TIME_BITS <= ULONG_MAX);
      if (WARN_OBSOLETE_TIMESTAMPS
	  && (da_lt_db ? aform : bform) == TIMEFORM_FLOAT
	  && (da_lt_db ? bform : aform) != TIMEFORM_TICKS_HZ
	  && mpz_cmp_ui (*hzmin, 1) > 0
	  && mpz_cmp_ui (*hzmin, 1 << LO_TIME_BITS) < 0)
	{
	  mpz_t *hzmin1 = &mpz[2 - da_lt_db];
	  mpz_set_ui (*hzmin1, 1 << LO_TIME_BITS);
	  hzmin = hzmin1;
	}

      /* iticks = (fb * na) OP (fa * nb), where OP is + or -.  */
      mpz_t const *na = bignum_integer (iticks, ta.ticks);
      mpz_mul (*iticks, *fb, *na);
      mpz_t const *nb = bignum_integer (&mpz[3], tb.ticks);
      (subtract ? mpz_submul : mpz_addmul) (*iticks, *fa, *nb);

      /* Normalize iticks/ihz by dividing both numerator and
	 denominator by ig = gcd (iticks, ihz).  For speed, though,
	 skip this division if ihz = 1.  */
      mpz_t *ig = &mpz[3];
      mpz_gcd (*ig, *iticks, *ihz);
      if (!FASTER_TIMEFNS || mpz_cmp_ui (*ig, 1) > 0)
	{
	  mpz_divexact (*iticks, *iticks, *ig);
	  mpz_divexact (*ihz, *ihz, *ig);

	  /* However, if dividing the denominator by ig would cause the
	     denominator to become less than hzmin, rescale the denominator
	     upwards by multiplying the normalized numerator and denominator
	     so that the resulting denominator becomes at least hzmin.
	     This rescaling avoids returning a timestamp that is less precise
	     than both a and b, or a timestamp that looks obsolete when that
	     might be a problem.  */
	  if (!FASTER_TIMEFNS || mpz_cmp (*ihz, *hzmin) < 0)
	    {
	      /* Rescale straightforwardly.  Although this might not
		 yield the minimal denominator that preserves numeric
		 value and is at least hzmin, calculating such a
		 denominator would be too expensive because it would
		 require testing multisets of factors of lcm (da, db).  */
	      mpz_t *rescale = &mpz[3];
	      mpz_cdiv_q (*rescale, *hzmin, *ihz);
	      mpz_mul (*iticks, *iticks, *rescale);
	      mpz_mul (*ihz, *ihz, *rescale);
	    }
	}

      /* mpz[0] and iticks now correspond to the (HZ . TICKS) pair.  */
      hz = make_integer_mpz ();
      mpz_swap (mpz[0], *iticks);
      ticks = make_integer_mpz ();
    }

  /* Return an integer if the timestamp resolution is 1,
     otherwise the (TICKS . HZ) form if !CURRENT_TIME_LIST or if
     either input used (TICKS . HZ) form or the result can't be expressed
     exactly in (HI LO US PS) form, otherwise the (HI LO US PS) form
     for backward compatibility.  */
  return (EQ (hz, make_fixnum (1))
	  ? ticks
	  : (!CURRENT_TIME_LIST
	     || aform == TIMEFORM_TICKS_HZ
	     || bform == TIMEFORM_TICKS_HZ
	     || !trillion_factor (hz))
	  ? Fcons (ticks, hz)
	  : ticks_hz_list4 (ticks, hz));
}

DEFUN ("time-add", Ftime_add, Stime_add, 2, 2, 0,
       doc: /* Return the sum of two time values A and B, as a time value.
See `format-time-string' for the various forms of a time value.
For example, nil stands for the current time.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return time_arith (a, b, false);
}

DEFUN ("time-subtract", Ftime_subtract, Stime_subtract, 2, 2, 0,
       doc: /* Return the difference between two time values A and B, as a time value.
You can use `float-time' to convert the difference into elapsed seconds.
See `format-time-string' for the various forms of a time value.
For example, nil stands for the current time.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return time_arith (a, b, true);
}

/* Return negative, 0, positive if a < b, a == b, a > b respectively.
   Return positive if either a or b is a NaN; this is good enough
   for the current callers.  */
static int
time_cmp (Lisp_Object a, Lisp_Object b)
{
  if ((FLOATP (a) && !isfinite (XFLOAT_DATA (a)))
      || (FLOATP (b) && !isfinite (XFLOAT_DATA (b))))
    {
      double da = FLOATP (a) ? XFLOAT_DATA (a) : 0;
      double db = FLOATP (b) ? XFLOAT_DATA (b) : 0;
      return da < db ? -1 : da != db;
    }

  struct lisp_time ta = lisp_time_struct (a, 0);

  /* Compare nil to nil correctly, and handle other eq values quicker
     while we're at it.  Compare here rather than earlier, to handle
     NaNs and check formats.  */
  if (EQ (a, b))
    return 0;

  /* Compare (ATICKS . AZ) to (BTICKS . BHZ) by comparing
     ATICKS * BHZ to BTICKS * AHZ.  */
  struct lisp_time tb = lisp_time_struct (b, 0);
  mpz_t const *za = bignum_integer (&mpz[0], ta.ticks);
  mpz_t const *zb = bignum_integer (&mpz[1], tb.ticks);
  if (! (FASTER_TIMEFNS && EQ (ta.hz, tb.hz)))
    {
      /* This could be sped up by looking at the signs, sizes, and
	 number of bits of the two sides; see how GMP does mpq_cmp.
	 It may not be worth the trouble here, though.  */
      mpz_mul (mpz[0], *za, *bignum_integer (&mpz[2], tb.hz));
      mpz_mul (mpz[1], *zb, *bignum_integer (&mpz[2], ta.hz));
      za = &mpz[0];
      zb = &mpz[1];
    }
  return mpz_cmp (*za, *zb);
}

DEFUN ("time-less-p", Ftime_less_p, Stime_less_p, 2, 2, 0,
       doc: /* Return non-nil if time value A is less than time value B.
See `format-time-string' for the various forms of a time value.
For example, nil stands for the current time.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return time_cmp (a, b) < 0 ? Qt : Qnil;
}

DEFUN ("time-equal-p", Ftime_equal_p, Stime_equal_p, 2, 2, 0,
       doc: /* Return non-nil if A and B are equal time values.
See `format-time-string' for the various forms of a time value.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return time_cmp (a, b) == 0 ? Qt : Qnil;
}


DEFUN ("float-time", Ffloat_time, Sfloat_time, 0, 1, 0,
       doc: /* Return the current time, as a float number of seconds since the epoch.
If SPECIFIED-TIME is given, it is a time value to convert to float
instead of the current time.  See `format-time-string' for the various
forms of a time value.

WARNING: Since the result is floating point, it may not be exact.
If precise time stamps are required, use either `encode-time',
or (if you need time as a string) `format-time-string'.  */)
  (Lisp_Object specified_time)
{
  double t;
  decode_lisp_time (specified_time, 0, 0, &t);
  return make_float (t);
}

/* Write information into buffer S of size MAXSIZE, according to the
   FORMAT of length FORMAT_LEN, using time information taken from *TP.
   Use the time zone specified by TZ.
   Use NS as the number of nanoseconds in the %N directive.
   Return the number of bytes written, not including the terminating
   '\0'.  If S is NULL, nothing will be written anywhere; so to
   determine how many bytes would be written, use NULL for S and
   ((size_t) -1) for MAXSIZE.

   This function behaves like nstrftime, except it allows NUL
   bytes in FORMAT and it does not support nanoseconds.  */
static size_t
emacs_nmemftime (char *s, size_t maxsize, const char *format,
		 size_t format_len, const struct tm *tp, timezone_t tz, int ns)
{
  size_t total = 0;

  /* Loop through all the NUL-terminated strings in the format
     argument.  Normally there's just one NUL-terminated string, but
     there can be arbitrarily many, concatenated together, if the
     format contains '\0' bytes.  nstrftime stops at the first
     '\0' byte so we must invoke it separately for each such string.  */
  for (;;)
    {
      size_t len;
      size_t result;

      if (s)
	s[0] = '\1';

      result = nstrftime (s, maxsize, format, tp, tz, ns);

      if (s)
	{
	  if (result == 0 && s[0] != '\0')
	    return 0;
	  s += result + 1;
	}

      maxsize -= result + 1;
      total += result;
      len = strlen (format);
      if (len == format_len)
	return total;
      total++;
      format += len + 1;
      format_len -= len + 1;
    }
}

static Lisp_Object
format_time_string (char const *format, ptrdiff_t formatlen,
		    struct timespec t, Lisp_Object zone, struct tm *tmp)
{
  char buffer[4000];
  char *buf = buffer;
  ptrdiff_t size = sizeof buffer;
  size_t len;
  int ns = t.tv_nsec;
  USE_SAFE_ALLOCA;

  timezone_t tz = tzlookup (zone, false);
  /* On some systems, like 32-bit MinGW, tv_sec of struct timespec is
     a 64-bit type, but time_t is a 32-bit type.  emacs_localtime_rz
     expects a pointer to time_t value.  */
  time_t tsec = t.tv_sec;
  tmp = emacs_localtime_rz (tz, &tsec, tmp);
  if (! tmp)
    {
      int localtime_errno = errno;
      xtzfree (tz);
      time_error (localtime_errno);
    }
  synchronize_system_time_locale ();

  while (true)
    {
      buf[0] = '\1';
      len = emacs_nmemftime (buf, size, format, formatlen, tmp, tz, ns);
      if ((0 < len && len < size) || (len == 0 && buf[0] == '\0'))
	break;

      /* Buffer was too small, so make it bigger and try again.  */
      len = emacs_nmemftime (NULL, SIZE_MAX, format, formatlen, tmp, tz, ns);
      if (STRING_BYTES_BOUND <= len)
	{
	  xtzfree (tz);
	  string_overflow ();
	}
      size = len + 1;
      buf = SAFE_ALLOCA (size);
    }

  xtzfree (tz);
  AUTO_STRING_WITH_LEN (bufstring, buf, len);
  Lisp_Object result = code_convert_string_norecord (bufstring,
						     Vlocale_coding_system, 0);
  SAFE_FREE ();
  return result;
}

DEFUN ("format-time-string", Fformat_time_string, Sformat_time_string, 1, 3, 0,
       doc: /* Use FORMAT-STRING to format the time value TIME.
A time value that is omitted or nil stands for the current time,
a number stands for that many seconds, an integer pair (TICKS . HZ)
stands for TICKS/HZ seconds, and an integer list (HI LO US PS) stands
for HI*2**16 + LO + US/10**6 + PS/10**12 seconds.  This function
treats seconds as time since the epoch of 1970-01-01 00:00:00 UTC.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.

The value is a copy of FORMAT-STRING, but with certain constructs replaced
by text that describes the specified date and time in TIME:

%Y is the year, %y within the century, %C the century.
%G is the year corresponding to the ISO week, %g within the century.
%m is the numeric month.
%b and %h are the locale's abbreviated month name, %B the full name.
 (%h is not supported on MS-Windows.)
%d is the day of the month, zero-padded, %e is blank-padded.
%u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
%a is the locale's abbreviated name of the day of week, %A the full name.
%U is the week number starting on Sunday, %W starting on Monday,
 %V according to ISO 8601.
%j is the day of the year.

%H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
 only blank-padded, %l is like %I blank-padded.
%p is the locale's equivalent of either AM or PM.
%q is the calendar quarter (1–4).
%M is the minute (00-59).
%S is the second (00-59; 00-60 on platforms with leap seconds)
%s is the number of seconds since 1970-01-01 00:00:00 +0000.
%N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
%Z is the time zone abbreviation, %z is the numeric form.

%c is the locale's date and time format.
%x is the locale's "preferred" date format.
%D is like "%m/%d/%y".
%F is the ISO 8601 date format (like "%+4Y-%m-%d").

%R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
%X is the locale's "preferred" time format.

Finally, %n is a newline, %t is a tab, %% is a literal %, and
unrecognized %-sequences stand for themselves.

A %-sequence can contain optional flags, field width, and a modifier
(in that order) after the `%'.  The flags are:

`-' Do not pad the field.
`_' Pad with spaces.
`0' Pad with zeros.
`+' Pad with zeros and put `+' before nonnegative year numbers with >4 digits.
`^' Use upper case characters if possible.
`#' Use opposite case characters if possible.

A field width N is an unsigned decimal integer with a leading digit
nonzero.  %NX is like %X, but takes up at least N positions.  The
field width is (on GNU/Linux and some other systems) in measured in
bytes, not characters.  It depends on the locale what the width (in
characters) %NX will end up being, especially when there are non-ASCII
characters in %X.

The modifiers are:

`E' Use the locale's alternative version.
`O' Use the locale's number symbols.

For example, to produce full ISO 8601 format, use "%FT%T%z".

usage: (format-time-string FORMAT-STRING &optional TIME ZONE)  */)
  (Lisp_Object format_string, Lisp_Object timeval, Lisp_Object zone)
{
  struct timespec t = lisp_time_argument (timeval);
  struct tm tm;

  CHECK_STRING (format_string);
  format_string = code_convert_string_norecord (format_string,
						Vlocale_coding_system, 1);
  return format_time_string (SSDATA (format_string), SBYTES (format_string),
			     t, zone, &tm);
}

DEFUN ("decode-time", Fdecode_time, Sdecode_time, 0, 3, 0,
       doc: /* Decode a time value as (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF).
The optional TIME is the time value to convert.  See
`format-time-string' for the various forms of a time value.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (the UTC offset in seconds) applied
without consideration for daylight saving time.

The optional FORM specifies the form of the SEC member.  If `integer',
SEC is an integer; if t, SEC is an integer or (TICKS . HZ) timestamp
with the same precision as TIME.  An omitted or nil FORM is currently
treated like `integer', but this may change in future Emacs versions.

To access (or alter) the elements in the time value, the
`decoded-time-second', `decoded-time-minute', `decoded-time-hour',
`decoded-time-day', `decoded-time-month', `decoded-time-year',
`decoded-time-weekday', `decoded-time-dst' and `decoded-time-zone'
accessors can be used.

The list has the following nine members: SEC is an integer or
Lisp timestamp representing a nonnegative value less than 60
\(or less than 61 if the operating system supports leap seconds).
MINUTE is an integer between 0 and 59.  HOUR is an integer
between 0 and 23.  DAY is an integer between 1 and 31.  MONTH is an
integer between 1 and 12.  YEAR is the year number, an integer; 0
represents 1 BC.  DOW is the day of week, an integer between 0 and 6,
where 0 is Sunday.  DST is t if daylight saving time is in effect,
nil if it is not in effect, and -1 if daylight saving information is
not available.  UTCOFF is an integer indicating the UTC offset in
seconds, i.e., the number of seconds east of Greenwich.  (Note that
Common Lisp has different meanings for DOW and UTCOFF, and its
SEC is always an integer between 0 and 59.)

usage: (decode-time &optional TIME ZONE FORM)  */)
  (Lisp_Object specified_time, Lisp_Object zone, Lisp_Object form)
{
  /* Compute broken-down local time LOCAL_TM from SPECIFIED_TIME and ZONE.  */
  struct lisp_time lt = lisp_time_struct (specified_time, 0);
  struct timespec ts = lisp_to_timespec (lt);
  if (! timespec_valid_p (ts))
    time_overflow ();
  time_t time_spec = ts.tv_sec;
  struct tm local_tm, gmt_tm;
  timezone_t tz = tzlookup (zone, false);
  struct tm *tm = emacs_localtime_rz (tz, &time_spec, &local_tm);
  int localtime_errno = errno;
  xtzfree (tz);

  if (!tm)
    time_error (localtime_errno);

  /* Let YEAR = LOCAL_TM.tm_year + TM_YEAR_BASE.  */
  Lisp_Object year;
  if (FASTER_TIMEFNS
      && MOST_NEGATIVE_FIXNUM - TM_YEAR_BASE <= local_tm.tm_year
      && local_tm.tm_year <= MOST_POSITIVE_FIXNUM - TM_YEAR_BASE)
    {
      /* Avoid overflow when INT_MAX - TM_YEAR_BASE < local_tm.tm_year.  */
      EMACS_INT tm_year_base = TM_YEAR_BASE;
      year = make_fixnum (local_tm.tm_year + tm_year_base);
    }
  else
    {
      mpz_set_si (mpz[0], local_tm.tm_year);
      mpz_add_ui (mpz[0], mpz[0], TM_YEAR_BASE);
      year = make_integer_mpz ();
    }

  /* Compute SEC from LOCAL_TM.tm_sec and HZ.  */
  Lisp_Object hz = lt.hz, sec;
  if (EQ (hz, make_fixnum (1)) || !EQ (form, Qt))
    sec = make_fixnum (local_tm.tm_sec);
  else
    {
      /* Let TICKS = HZ * LOCAL_TM.tm_sec + mod (LT.ticks, HZ)
	 and SEC = (TICKS . HZ).  */
      Lisp_Object ticks;
      intmax_t n;
      if (FASTER_TIMEFNS && FIXNUMP (lt.ticks) && FIXNUMP (hz)
	  && !INT_MULTIPLY_WRAPV (XFIXNUM (hz), local_tm.tm_sec, &n)
	  && ! (INT_ADD_WRAPV
		(n, (XFIXNUM (lt.ticks) % XFIXNUM (hz)
		     + (XFIXNUM (lt.ticks) % XFIXNUM (hz) < 0
			? XFIXNUM (hz) : 0)),
		 &n)))
	ticks = make_int (n);
      else
	{
	  mpz_fdiv_r (mpz[0],
		      *bignum_integer (&mpz[0], lt.ticks),
		      *bignum_integer (&mpz[1], hz));
	  mpz_addmul_ui (mpz[0], *bignum_integer (&mpz[1], hz),
			 local_tm.tm_sec);
	  ticks = make_integer_mpz ();
	}
      sec = Fcons (ticks, hz);
    }

  return CALLN (Flist,
		sec,
		make_fixnum (local_tm.tm_min),
		make_fixnum (local_tm.tm_hour),
		make_fixnum (local_tm.tm_mday),
		make_fixnum (local_tm.tm_mon + 1),
		year,
		make_fixnum (local_tm.tm_wday),
		(local_tm.tm_isdst < 0 ? make_fixnum (-1)
		 : local_tm.tm_isdst == 0 ? Qnil : Qt),
		(HAVE_TM_GMTOFF
		 ? make_fixnum (tm_gmtoff (&local_tm))
		 : gmtime_r (&time_spec, &gmt_tm)
		 ? make_fixnum (tm_diff (&local_tm, &gmt_tm))
		 : Qnil));
}

/* Return OBJ - OFFSET, checking that OBJ is a valid integer and that
   the result is representable as an int.  0 <= OFFSET <= TM_YEAR_BASE.  */
static int
check_tm_member (Lisp_Object obj, int offset)
{
  if (FASTER_TIMEFNS && INT_MAX <= MOST_POSITIVE_FIXNUM - TM_YEAR_BASE)
    {
      CHECK_FIXNUM (obj);
      EMACS_INT n = XFIXNUM (obj);
      int i;
      if (INT_SUBTRACT_WRAPV (n, offset, &i))
	time_overflow ();
      return i;
    }
  else
    {
      CHECK_INTEGER (obj);
      mpz_sub_ui (mpz[0], *bignum_integer (&mpz[0], obj), offset);
      intmax_t i;
      if (! (mpz_to_intmax (mpz[0], &i) && INT_MIN <= i && i <= INT_MAX))
	time_overflow ();
      return i;
    }
}

DEFUN ("encode-time", Fencode_time, Sencode_time, 1, MANY, 0,
       doc: /* Convert TIME to a timestamp.

TIME is a list (SECOND MINUTE HOUR DAY MONTH YEAR IGNORED DST ZONE).
in the style of `decode-time', so that (encode-time (decode-time ...)) works.
In this list, ZONE can be nil for Emacs local time, t for Universal
Time, `wall' for system wall clock time, or a string as in the TZ
environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.  If ZONE specifies a
time zone with daylight-saving transitions, DST is t for daylight
saving time, nil for standard time, and -1 to cause the daylight
saving flag to be guessed.

As an obsolescent calling convention, if this function is called with
6 or more arguments, the first 6 arguments are SECOND, MINUTE, HOUR,
DAY, MONTH, and YEAR, and specify the components of a decoded time,
where DST assumed to be -1 and FORM is omitted.  If there are more
than 6 arguments the *last* argument is used as ZONE and any other
extra arguments are ignored, so that (apply #\\='encode-time
(decode-time ...)) works.  In this obsolescent convention, DST and
ZONE default to -1 and nil respectively.

Years before 1970 are not guaranteed to work.  On some systems,
year values as low as 1901 do work.

usage: (encode-time TIME &rest OBSOLESCENT-ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  struct tm tm;
  Lisp_Object zone = Qnil;
  Lisp_Object a = args[0];
  Lisp_Object secarg, minarg, hourarg, mdayarg, monarg, yeararg;
  tm.tm_isdst = -1;

  if (nargs == 1)
    {
      Lisp_Object tail = a;
      for (int i = 0; i < 9; i++, tail = XCDR (tail))
	CHECK_CONS (tail);
      secarg = XCAR (a); a = XCDR (a);
      minarg = XCAR (a); a = XCDR (a);
      hourarg = XCAR (a); a = XCDR (a);
      mdayarg = XCAR (a); a = XCDR (a);
      monarg = XCAR (a); a = XCDR (a);
      yeararg = XCAR (a); a = XCDR (a);
      a = XCDR (a);
      Lisp_Object dstflag = XCAR (a); a = XCDR (a);
      zone = XCAR (a);
      if (SYMBOLP (dstflag) && !FIXNUMP (zone) && !CONSP (zone))
	tm.tm_isdst = !NILP (dstflag);
    }
  else if (nargs < 6)
    xsignal2 (Qwrong_number_of_arguments, Qencode_time, make_fixnum (nargs));
  else
    {
      if (6 < nargs)
	zone = args[nargs - 1];
      secarg = a;
      minarg = args[1];
      hourarg = args[2];
      mdayarg = args[3];
      monarg = args[4];
      yeararg = args[5];
    }

  /* Let SEC = floor (LT.ticks / HZ), with SUBSECTICKS the remainder.  */
  struct lisp_time lt;
  decode_lisp_time (secarg, 0, &lt, 0);
  Lisp_Object hz = lt.hz, sec, subsecticks;
  if (FASTER_TIMEFNS && EQ (hz, make_fixnum (1)))
    {
      sec = lt.ticks;
      subsecticks = make_fixnum (0);
    }
  else
    {
      mpz_fdiv_qr (mpz[0], mpz[1],
		   *bignum_integer (&mpz[0], lt.ticks),
		   *bignum_integer (&mpz[1], hz));
      sec = make_integer_mpz ();
      mpz_swap (mpz[0], mpz[1]);
      subsecticks = make_integer_mpz ();
    }
  tm.tm_sec  = check_tm_member (sec, 0);
  tm.tm_min  = check_tm_member (minarg, 0);
  tm.tm_hour = check_tm_member (hourarg, 0);
  tm.tm_mday = check_tm_member (mdayarg, 0);
  tm.tm_mon  = check_tm_member (monarg, 1);
  tm.tm_year = check_tm_member (yeararg, TM_YEAR_BASE);

  timezone_t tz = tzlookup (zone, false);
  tm.tm_wday = -1;
  time_t value = mktime_z (tz, &tm);
  int mktime_errno = errno;
  xtzfree (tz);

  if (tm.tm_wday < 0)
    time_error (mktime_errno);

  if (EQ (hz, make_fixnum (1)))
    return (CURRENT_TIME_LIST
	    ? list2 (hi_time (value), lo_time (value))
	    : INT_TO_INTEGER (value));
  else
    {
      struct lisp_time val1 = { INT_TO_INTEGER (value), make_fixnum (1) };
      Lisp_Object secticks = lisp_time_hz_ticks (val1, hz);
      Lisp_Object ticks = lispint_arith (secticks, subsecticks, false);
      return Fcons (ticks, hz);
    }
}

DEFUN ("time-convert", Ftime_convert, Stime_convert, 1, 2, 0,
       doc: /* Convert TIME value to a Lisp timestamp.
With optional FORM, convert to that timestamp form.
Truncate the returned value toward minus infinity.

If FORM is nil (the default), return the same form as `current-time'.
If FORM is a positive integer, return a pair of integers (TICKS . FORM),
where TICKS is the number of clock ticks and FORM is the clock frequency
in ticks per second.  (Currently the positive integer should be at least
65536 if the returned value is expected to be given to standard functions
expecting Lisp timestamps.)  If FORM is t, return (TICKS . PHZ), where
PHZ is a suitable clock frequency in ticks per second.  If FORM is
`integer', return an integer count of seconds.  If FORM is `list',
return an integer list (HIGH LOW USEC PSEC), where HIGH has the most
significant bits of the seconds, LOW has the least significant 16
bits, and USEC and PSEC are the microsecond and picosecond counts.  */)
     (Lisp_Object time, Lisp_Object form)
{
  struct lisp_time t;
  enum timeform input_form = decode_lisp_time (time, 0, &t, 0);
  if (NILP (form))
    form = CURRENT_TIME_LIST ? Qlist : Qt;
  if (EQ (form, Qlist))
    return ticks_hz_list4 (t.ticks, t.hz);
  if (EQ (form, Qinteger))
    return FASTER_TIMEFNS && INTEGERP (time) ? time : lisp_time_seconds (t);
  if (EQ (form, Qt))
    form = t.hz;
  if (FASTER_TIMEFNS
      && input_form == TIMEFORM_TICKS_HZ && EQ (form, XCDR (time)))
    return time;
  return Fcons (lisp_time_hz_ticks (t, form), form);
}

DEFUN ("current-time", Fcurrent_time, Scurrent_time, 0, 0, 0,
       doc: /* Return the current time, as the number of seconds since 1970-01-01 00:00:00.
The time is returned as a list of integers (HIGH LOW USEC PSEC).
HIGH has the most significant bits of the seconds, while LOW has the
least significant 16 bits.  USEC and PSEC are the microsecond and
picosecond counts.

In a future Emacs version, the format of the returned timestamp is
planned to change.  Use `time-convert' if you need a particular
timestamp form; for example, (time-convert nil \\='integer) returns
the current time in seconds.  */)
  (void)
{
  return make_lisp_time (current_timespec ());
}

DEFUN ("current-time-string", Fcurrent_time_string, Scurrent_time_string,
       0, 2, 0,
       doc: /* Return the current local time, as a human-readable string.
Programs can use this function to decode a time,
since the number of columns in each field is fixed
if the year is in the range 1000-9999.
The format is `Sun Sep 16 01:03:52 1973'.
However, see also the functions `decode-time' and `format-time-string'
which provide a much more powerful and general facility.

If SPECIFIED-TIME is given, it is the time value to format instead of
the current time.  See `format-time-string' for the various forms of a
time value.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.  */)
  (Lisp_Object specified_time, Lisp_Object zone)
{
  time_t value = lisp_seconds_argument (specified_time);
  timezone_t tz = tzlookup (zone, false);

  /* Convert to a string in ctime format, except without the trailing
     newline, and without the 4-digit year limit.  Don't use asctime
     or ctime, as they might dump core if the year is outside the
     range -999 .. 9999.  */
  struct tm tm;
  struct tm *tmp = emacs_localtime_rz (tz, &value, &tm);
  int localtime_errno = errno;
  xtzfree (tz);
  if (! tmp)
    time_error (localtime_errno);

  static char const wday_name[][4] =
    { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
  static char const mon_name[][4] =
    { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
  intmax_t year_base = TM_YEAR_BASE;
  char buf[sizeof "Mon Apr 30 12:49:17 " + INT_STRLEN_BOUND (int) + 1];
  int len = sprintf (buf, "%s %s%3d %02d:%02d:%02d %"PRIdMAX,
		     wday_name[tm.tm_wday], mon_name[tm.tm_mon], tm.tm_mday,
		     tm.tm_hour, tm.tm_min, tm.tm_sec,
		     tm.tm_year + year_base);

  return make_unibyte_string (buf, len);
}

DEFUN ("current-time-zone", Fcurrent_time_zone, Scurrent_time_zone, 0, 2, 0,
       doc: /* Return the offset and name for the local time zone.
This returns a list of the form (OFFSET NAME).
OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
    A negative value means west of Greenwich.
NAME is a string giving the name of the time zone.
If SPECIFIED-TIME is given, the time zone offset is determined from it
instead of using the current time.  The argument should be a Lisp
time value; see `format-time-string' for the various forms of a time
value.

The optional ZONE is omitted or nil for Emacs local time, t for
Universal Time, `wall' for system wall clock time, or a string as in
the TZ environment variable.  It can also be a list (as from
`current-time-zone') or an integer (as from `decode-time') applied
without consideration for daylight saving time.

Some operating systems cannot provide all this information to Emacs;
in this case, `current-time-zone' returns a list containing nil for
the data it can't find.  */)
  (Lisp_Object specified_time, Lisp_Object zone)
{
  struct timespec value;
  struct tm local_tm, gmt_tm;
  Lisp_Object zone_offset, zone_name;

  zone_offset = Qnil;
  value = make_timespec (lisp_seconds_argument (specified_time), 0);
  zone_name = format_time_string ("%Z", sizeof "%Z" - 1, value,
				  zone, &local_tm);

  /* gmtime_r expects a pointer to time_t, but tv_sec of struct
     timespec on some systems (MinGW) is a 64-bit field.  */
  time_t tsec = value.tv_sec;
  if (HAVE_TM_GMTOFF || gmtime_r (&tsec, &gmt_tm))
    {
      long int offset = (HAVE_TM_GMTOFF
			 ? tm_gmtoff (&local_tm)
			 : tm_diff (&local_tm, &gmt_tm));
      zone_offset = make_fixnum (offset);
      if (SCHARS (zone_name) == 0)
	{
	  /* No local time zone name is available; use numeric zone instead.  */
	  long int hour = offset / 3600;
	  int min_sec = offset % 3600;
	  int amin_sec = min_sec < 0 ? - min_sec : min_sec;
	  int min = amin_sec / 60;
	  int sec = amin_sec % 60;
	  int min_prec = min_sec ? 2 : 0;
	  int sec_prec = sec ? 2 : 0;
	  char buf[sizeof "+0000" + INT_STRLEN_BOUND (long int)];
	  zone_name = make_formatted_string (buf, "%c%.2ld%.*d%.*d",
					     (offset < 0 ? '-' : '+'),
					     hour, min_prec, min, sec_prec, sec);
	}
    }

  return list2 (zone_offset, zone_name);
}

DEFUN ("set-time-zone-rule", Fset_time_zone_rule, Sset_time_zone_rule, 1, 1, 0,
       doc: /* Set the Emacs local time zone using TZ, a string specifying a time zone rule.
If TZ is nil or `wall', use system wall clock time; this differs from
the usual Emacs convention where nil means current local time.  If TZ
is t, use Universal Time.  If TZ is a list (as from
`current-time-zone') or an integer (as from `decode-time'), use the
specified time zone without consideration for daylight saving time.

Instead of calling this function, you typically want something else.
To temporarily use a different time zone rule for just one invocation
of `decode-time', `encode-time', or `format-time-string', pass the
function a ZONE argument.  To change local time consistently
throughout Emacs, call (setenv "TZ" TZ): this changes both the
environment of the Emacs process and the variable
`process-environment', whereas `set-time-zone-rule' affects only the
former.  */)
  (Lisp_Object tz)
{
  tzlookup (NILP (tz) ? Qwall : tz, true);
  return Qnil;
}

/* A buffer holding a string of the form "TZ=value", intended
   to be part of the environment.  If TZ is supposed to be unset,
   the buffer string is "tZ=".  */
 static char *tzvalbuf;

/* Get the local time zone rule.  */
char *
emacs_getenv_TZ (void)
{
  return tzvalbuf[0] == 'T' ? tzvalbuf + tzeqlen : 0;
}

/* Set the local time zone rule to TZSTRING, which can be null to
   denote wall clock time.  Do not record the setting in LOCAL_TZ.

   This function is not thread-safe, in theory because putenv is not,
   but mostly because of the static storage it updates.  Other threads
   that invoke localtime etc. may be adversely affected while this
   function is executing.  */

int
emacs_setenv_TZ (const char *tzstring)
{
  static ptrdiff_t tzvalbufsize;
  ptrdiff_t tzstringlen = tzstring ? strlen (tzstring) : 0;
  char *tzval = tzvalbuf;
  bool new_tzvalbuf = tzvalbufsize <= tzeqlen + tzstringlen;

  if (new_tzvalbuf)
    {
      /* Do not attempt to free the old tzvalbuf, since another thread
	 may be using it.  In practice, the first allocation is large
	 enough and memory does not leak.  */
      tzval = xpalloc (NULL, &tzvalbufsize,
		       tzeqlen + tzstringlen - tzvalbufsize + 1, -1, 1);
      tzvalbuf = tzval;
      tzval[1] = 'Z';
      tzval[2] = '=';
    }

  if (tzstring)
    {
      /* Modify TZVAL in place.  Although this is dicey in a
	 multithreaded environment, we know of no portable alternative.
	 Calling putenv or setenv could crash some other thread.  */
      tzval[0] = 'T';
      strcpy (tzval + tzeqlen, tzstring);
    }
  else
    {
      /* Turn 'TZ=whatever' into an empty environment variable 'tZ='.
	 Although this is also dicey, calling unsetenv here can crash Emacs.
	 See Bug#8705.  */
      tzval[0] = 't';
      tzval[tzeqlen] = 0;
    }


#ifndef WINDOWSNT
  /* Modifying *TZVAL merely requires calling tzset (which is the
     caller's responsibility).  However, modifying TZVAL requires
     calling putenv; although this is not thread-safe, in practice this
     runs only on startup when there is only one thread.  */
  bool need_putenv = new_tzvalbuf;
#else
  /* MS-Windows 'putenv' copies the argument string into a block it
     allocates, so modifying *TZVAL will not change the environment.
     However, the other threads run by Emacs on MS-Windows never call
     'xputenv' or 'putenv' or 'unsetenv', so the original cause for the
     dicey in-place modification technique doesn't exist there in the
     first place.  */
  bool need_putenv = true;
#endif
  if (need_putenv)
    xputenv (tzval);

  return 0;
}

#if (ULONG_MAX < TRILLION || !FASTER_TIMEFNS) && !defined ztrillion
# define NEED_ZTRILLION_INIT 1
#endif

#ifdef NEED_ZTRILLION_INIT
static void
syms_of_timefns_for_pdumper (void)
{
  mpz_init_set_ui (ztrillion, 1000000);
  mpz_mul_ui (ztrillion, ztrillion, 1000000);
}
#endif

void
syms_of_timefns (void)
{
#ifndef timespec_hz
  timespec_hz = make_int (TIMESPEC_HZ);
  staticpro (&timespec_hz);
#endif
#ifndef trillion
  trillion = make_int (1000000000000);
  staticpro (&trillion);
#endif

  DEFSYM (Qencode_time, "encode-time");

  defsubr (&Scurrent_time);
  defsubr (&Stime_convert);
  defsubr (&Stime_add);
  defsubr (&Stime_subtract);
  defsubr (&Stime_less_p);
  defsubr (&Stime_equal_p);
  defsubr (&Sformat_time_string);
  defsubr (&Sfloat_time);
  defsubr (&Sdecode_time);
  defsubr (&Sencode_time);
  defsubr (&Scurrent_time_string);
  defsubr (&Scurrent_time_zone);
  defsubr (&Sset_time_zone_rule);

  flt_radix_power = make_vector (flt_radix_power_size, Qnil);
  staticpro (&flt_radix_power);

#ifdef NEED_ZTRILLION_INIT
  pdumper_do_now_and_after_load (syms_of_timefns_for_pdumper);
#endif
}
