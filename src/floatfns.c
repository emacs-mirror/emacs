/* Primitive operations on floating point for GNU Emacs Lisp interpreter.
   Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* ANSI C requires only these float functions:
   acos, asin, atan, atan2, ceil, cos, cosh, exp, fabs, floor, fmod,
   frexp, ldexp, log, log10, modf, pow, sin, sinh, sqrt, tan, tanh.

   Define HAVE_INVERSE_HYPERBOLIC if you have acosh, asinh, and atanh.
   Define HAVE_CBRT if you have cbrt.
   Define HAVE_RINT if you have rint.
   If you don't define these, then the appropriate routines will be simulated.

   Define HAVE_MATHERR if on a system supporting the SysV matherr callback.
   (This should happen automatically.)

   Define FLOAT_CHECK_ERRNO if the float library routines set errno.
   This has no effect if HAVE_MATHERR is defined.

   Define FLOAT_CATCH_SIGILL if the float library routines signal SIGILL.
   (What systems actually do this?  Please let us know.)

   Define FLOAT_CHECK_DOMAIN if the float library doesn't handle errors by
   either setting errno, or signalling SIGFPE/SIGILL.  Otherwise, domain and
   range checking will happen before calling the float routines.  This has
   no effect if HAVE_MATHERR is defined (since matherr will be called when
   a domain error occurs.)
 */

#include <signal.h>

#include <config.h>
#include "lisp.h"
#include "syssignal.h"

Lisp_Object Qarith_error;

#ifdef LISP_FLOAT_TYPE

#ifdef MSDOS
/* These are redefined (correctly, but differently) in values.h.  */
#undef INTBITS
#undef LONGBITS
#undef SHORTBITS
#endif

/* Work around a problem that happens because math.h on hpux 7
   defines two static variables--which, in Emacs, are not really static,
   because `static' is defined as nothing.  The problem is that they are
   defined both here and in lread.c.
   These macros prevent the name conflict.  */
#if defined (HPUX) && !defined (HPUX8)
#define _MAXLDBL floatfns_maxldbl
#define _NMAXLDBL floatfns_nmaxldbl
#endif

#include <math.h>

/* This declaration is omitted on some systems, like Ultrix.  */
#if !defined (HPUX) && defined (HAVE_LOGB) && !defined (logb)
extern double logb ();
#endif /* not HPUX and HAVE_LOGB and no logb macro */

#if defined(DOMAIN) && defined(SING) && defined(OVERFLOW)
    /* If those are defined, then this is probably a `matherr' machine. */
# ifndef HAVE_MATHERR
#  define HAVE_MATHERR
# endif
#endif

#ifdef NO_MATHERR
#undef HAVE_MATHERR
#endif

#ifdef HAVE_MATHERR
# ifdef FLOAT_CHECK_ERRNO
#  undef FLOAT_CHECK_ERRNO
# endif
# ifdef FLOAT_CHECK_DOMAIN
#  undef FLOAT_CHECK_DOMAIN
# endif
#endif

#ifndef NO_FLOAT_CHECK_ERRNO
#define FLOAT_CHECK_ERRNO
#endif

#ifdef FLOAT_CHECK_ERRNO
# include <errno.h>

extern int errno;
#endif

/* Avoid traps on VMS from sinh and cosh.
   All the other functions set errno instead.  */

#ifdef VMS
#undef cosh
#undef sinh
#define cosh(x) ((exp(x)+exp(-x))*0.5)
#define sinh(x) ((exp(x)-exp(-x))*0.5)
#endif /* VMS */

#ifndef HAVE_RINT
#define rint(x) (floor((x)+0.5))
#endif

static SIGTYPE float_error ();

/* Nonzero while executing in floating point.
   This tells float_error what to do.  */

static int in_float;

/* If an argument is out of range for a mathematical function,
   here is the actual argument value to use in the error message.  */

static Lisp_Object float_error_arg, float_error_arg2;

static char *float_error_fn_name;

/* Evaluate the floating point expression D, recording NUM
   as the original argument for error messages.
   D is normally an assignment expression.
   Handle errors which may result in signals or may set errno.

   Note that float_error may be declared to return void, so you can't
   just cast the zero after the colon to (SIGTYPE) to make the types
   check properly.  */

#ifdef FLOAT_CHECK_ERRNO
#define IN_FLOAT(d, name, num)				\
  do {							\
    float_error_arg = num;				\
    float_error_fn_name = name;				\
    in_float = 1; errno = 0; (d); in_float = 0;		\
    switch (errno) {					\
    case 0: break;					\
    case EDOM:	 domain_error (float_error_fn_name, float_error_arg);	\
    case ERANGE: range_error (float_error_fn_name, float_error_arg);	\
    default:	 arith_error (float_error_fn_name, float_error_arg);	\
    }							\
  } while (0)
#define IN_FLOAT2(d, name, num, num2)			\
  do {							\
    float_error_arg = num;				\
    float_error_arg2 = num2;				\
    float_error_fn_name = name;				\
    in_float = 1; errno = 0; (d); in_float = 0;		\
    switch (errno) {					\
    case 0: break;					\
    case EDOM:	 domain_error (float_error_fn_name, float_error_arg);	\
    case ERANGE: range_error (float_error_fn_name, float_error_arg);	\
    default:	 arith_error (float_error_fn_name, float_error_arg);	\
    }							\
  } while (0)
#else
#define IN_FLOAT(d, name, num) (in_float = 1, (d), in_float = 0)
#define IN_FLOAT2(d, name, num, num2) (in_float = 1, (d), in_float = 0)
#endif

/* Convert float to Lisp_Int if it fits, else signal a range error
   using the given arguments.  */
#define FLOAT_TO_INT(x, i, name, num)					\
  do									\
    {									\
      if ((x) >= (((EMACS_INT) 1) << (VALBITS-1)) ||			\
	  (x) <= - (((EMACS_INT) 1) << (VALBITS-1)) - 1)		\
	range_error (name, num);					\
      XSETINT (i,  (EMACS_INT)(x));					\
    }									\
  while (0)
#define FLOAT_TO_INT2(x, i, name, num1, num2)				\
  do									\
    {									\
      if ((x) >= (((EMACS_INT) 1) << (VALBITS-1)) ||			\
	  (x) <= - (((EMACS_INT) 1) << (VALBITS-1)) - 1)		\
	range_error2 (name, num1, num2);				\
      XSETINT (i,  (EMACS_INT)(x));					\
    }									\
  while (0)

#define arith_error(op,arg) \
  Fsignal (Qarith_error, Fcons (build_string ((op)), Fcons ((arg), Qnil)))
#define range_error(op,arg) \
  Fsignal (Qrange_error, Fcons (build_string ((op)), Fcons ((arg), Qnil)))
#define range_error2(op,a1,a2) \
  Fsignal (Qrange_error, Fcons (build_string ((op)), \
				Fcons ((a1), Fcons ((a2), Qnil))))
#define domain_error(op,arg) \
  Fsignal (Qdomain_error, Fcons (build_string ((op)), Fcons ((arg), Qnil)))
#define domain_error2(op,a1,a2) \
  Fsignal (Qdomain_error, Fcons (build_string ((op)), \
				 Fcons ((a1), Fcons ((a2), Qnil))))

/* Extract a Lisp number as a `double', or signal an error.  */

double
extract_float (num)
     Lisp_Object num;
{
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (FLOATP (num))
    return XFLOAT (num)->data;
  return (double) XINT (num);
}

/* Trig functions.  */

DEFUN ("acos", Facos, Sacos, 1, 1, 0,
  "Return the inverse cosine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("acos", arg);
#endif
  IN_FLOAT (d = acos (d), "acos", arg);
  return make_float (d);
}

DEFUN ("asin", Fasin, Sasin, 1, 1, 0,
  "Return the inverse sine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("asin", arg);
#endif
  IN_FLOAT (d = asin (d), "asin", arg);
  return make_float (d);
}

DEFUN ("atan", Fatan, Satan, 1, 1, 0,
  "Return the inverse tangent of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = atan (d), "atan", arg);
  return make_float (d);
}

DEFUN ("cos", Fcos, Scos, 1, 1, 0,
  "Return the cosine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = cos (d), "cos", arg);
  return make_float (d);
}

DEFUN ("sin", Fsin, Ssin, 1, 1, 0,
  "Return the sine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = sin (d), "sin", arg);
  return make_float (d);
}

DEFUN ("tan", Ftan, Stan, 1, 1, 0,
  "Return the tangent of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  double c = cos (d);
#ifdef FLOAT_CHECK_DOMAIN
  if (c == 0.0)
    domain_error ("tan", arg);
#endif
  IN_FLOAT (d = sin (d) / c, "tan", arg);
  return make_float (d);
}

#if 0 /* Leave these out unless we find there's a reason for them.  */

DEFUN ("bessel-j0", Fbessel_j0, Sbessel_j0, 1, 1, 0,
  "Return the bessel function j0 of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = j0 (d), "bessel-j0", arg);
  return make_float (d);
}

DEFUN ("bessel-j1", Fbessel_j1, Sbessel_j1, 1, 1, 0,
  "Return the bessel function j1 of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = j1 (d), "bessel-j1", arg);
  return make_float (d);
}

DEFUN ("bessel-jn", Fbessel_jn, Sbessel_jn, 2, 2, 0,
  "Return the order N bessel function output jn of ARG.\n\
The first arg (the order) is truncated to an integer.")
  (arg1, arg2)
     register Lisp_Object arg1, arg2;
{
  int i1 = extract_float (arg1);
  double f2 = extract_float (arg2);

  IN_FLOAT (f2 = jn (i1, f2), "bessel-jn", arg1);
  return make_float (f2);
}

DEFUN ("bessel-y0", Fbessel_y0, Sbessel_y0, 1, 1, 0,
  "Return the bessel function y0 of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = y0 (d), "bessel-y0", arg);
  return make_float (d);
}

DEFUN ("bessel-y1", Fbessel_y1, Sbessel_y1, 1, 1, 0,
  "Return the bessel function y1 of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = y1 (d), "bessel-y0", arg);
  return make_float (d);
}

DEFUN ("bessel-yn", Fbessel_yn, Sbessel_yn, 2, 2, 0,
  "Return the order N bessel function output yn of ARG.\n\
The first arg (the order) is truncated to an integer.")
  (arg1, arg2)
     register Lisp_Object arg1, arg2;
{
  int i1 = extract_float (arg1);
  double f2 = extract_float (arg2);

  IN_FLOAT (f2 = yn (i1, f2), "bessel-yn", arg1);
  return make_float (f2);
}

#endif

#if 0 /* Leave these out unless we see they are worth having.  */

DEFUN ("erf", Ferf, Serf, 1, 1, 0,
  "Return the mathematical error function of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = erf (d), "erf", arg);
  return make_float (d);
}

DEFUN ("erfc", Ferfc, Serfc, 1, 1, 0,
  "Return the complementary error function of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = erfc (d), "erfc", arg);
  return make_float (d);
}

DEFUN ("log-gamma", Flog_gamma, Slog_gamma, 1, 1, 0,
  "Return the log gamma of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = lgamma (d), "log-gamma", arg);
  return make_float (d);
}

DEFUN ("cube-root", Fcube_root, Scube_root, 1, 1, 0,
  "Return the cube root of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef HAVE_CBRT
  IN_FLOAT (d = cbrt (d), "cube-root", arg);
#else
  if (d >= 0.0)
    IN_FLOAT (d = pow (d, 1.0/3.0), "cube-root", arg);
  else
    IN_FLOAT (d = -pow (-d, 1.0/3.0), "cube-root", arg);
#endif
  return make_float (d);
}

#endif

DEFUN ("exp", Fexp, Sexp, 1, 1, 0,
  "Return the exponential base e of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 709.7827)   /* Assume IEEE doubles here */
    range_error ("exp", arg);
  else if (d < -709.0)
    return make_float (0.0);
  else
#endif
    IN_FLOAT (d = exp (d), "exp", arg);
  return make_float (d);
}

DEFUN ("expt", Fexpt, Sexpt, 2, 2, 0,
  "Return the exponential X ** Y.")
  (arg1, arg2)
     register Lisp_Object arg1, arg2;
{
  double f1, f2;

  CHECK_NUMBER_OR_FLOAT (arg1, 0);
  CHECK_NUMBER_OR_FLOAT (arg2, 0);
  if (INTEGERP (arg1)     /* common lisp spec */
      && INTEGERP (arg2)) /* don't promote, if both are ints */
    {				/* this can be improved by pre-calculating */
      int acc, x, y;		/* some binary powers of x then accumulating */
      Lisp_Object val;

      x = XINT (arg1);
      y = XINT (arg2);
      acc = 1;
      
      if (y < 0)
	{
	  if (x == 1)
	    acc = 1;
	  else if (x == -1)
	    acc = (y & 1) ? -1 : 1;
	  else
	    acc = 0;
	}
      else
	{
	  while (y > 0)
	    {
	      if (y & 1)
		acc *= x;
	      x *= x;
	      y = (unsigned)y >> 1;
	    }
	}
      XSETINT (val, acc);
      return val;
    }
  f1 = FLOATP (arg1) ? XFLOAT (arg1)->data : XINT (arg1);
  f2 = FLOATP (arg2) ? XFLOAT (arg2)->data : XINT (arg2);
  /* Really should check for overflow, too */
  if (f1 == 0.0 && f2 == 0.0)
    f1 = 1.0;
#ifdef FLOAT_CHECK_DOMAIN
  else if ((f1 == 0.0 && f2 < 0.0) || (f1 < 0 && f2 != floor(f2)))
    domain_error2 ("expt", arg1, arg2);
#endif
  IN_FLOAT2 (f1 = pow (f1, f2), "expt", arg1, arg2);
  return make_float (f1);
}

DEFUN ("log", Flog, Slog, 1, 2, 0,
  "Return the natural logarithm of ARG.\n\
If second optional argument BASE is given, return log ARG using that base.")
  (arg, base)
     register Lisp_Object arg, base;
{
  double d = extract_float (arg);

#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error2 ("log", arg, base);
#endif
  if (NILP (base))
    IN_FLOAT (d = log (d), "log", arg);
  else
    {
      double b = extract_float (base);

#ifdef FLOAT_CHECK_DOMAIN
      if (b <= 0.0 || b == 1.0)
	domain_error2 ("log", arg, base);
#endif
      if (b == 10.0)
	IN_FLOAT2 (d = log10 (d), "log", arg, base);
      else
	IN_FLOAT2 (d = log (d) / log (b), "log", arg, base);
    }
  return make_float (d);
}

DEFUN ("log10", Flog10, Slog10, 1, 1, 0,
  "Return the logarithm base 10 of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error ("log10", arg);
#endif
  IN_FLOAT (d = log10 (d), "log10", arg);
  return make_float (d);
}

DEFUN ("sqrt", Fsqrt, Ssqrt, 1, 1, 0,
  "Return the square root of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 0.0)
    domain_error ("sqrt", arg);
#endif
  IN_FLOAT (d = sqrt (d), "sqrt", arg);
  return make_float (d);
}

#if 0 /* Not clearly worth adding.  */

DEFUN ("acosh", Facosh, Sacosh, 1, 1, 0,
  "Return the inverse hyperbolic cosine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 1.0)
    domain_error ("acosh", arg);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = acosh (d), "acosh", arg);
#else
  IN_FLOAT (d = log (d + sqrt (d*d - 1.0)), "acosh", arg);
#endif
  return make_float (d);
}

DEFUN ("asinh", Fasinh, Sasinh, 1, 1, 0,
  "Return the inverse hyperbolic sine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = asinh (d), "asinh", arg);
#else
  IN_FLOAT (d = log (d + sqrt (d*d + 1.0)), "asinh", arg);
#endif
  return make_float (d);
}

DEFUN ("atanh", Fatanh, Satanh, 1, 1, 0,
  "Return the inverse hyperbolic tangent of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d >= 1.0 || d <= -1.0)
    domain_error ("atanh", arg);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = atanh (d), "atanh", arg);
#else
  IN_FLOAT (d = 0.5 * log ((1.0 + d) / (1.0 - d)), "atanh", arg);
#endif
  return make_float (d);
}

DEFUN ("cosh", Fcosh, Scosh, 1, 1, 0,
  "Return the hyperbolic cosine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("cosh", arg);
#endif
  IN_FLOAT (d = cosh (d), "cosh", arg);
  return make_float (d);
}

DEFUN ("sinh", Fsinh, Ssinh, 1, 1, 0,
  "Return the hyperbolic sine of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("sinh", arg);
#endif
  IN_FLOAT (d = sinh (d), "sinh", arg);
  return make_float (d);
}

DEFUN ("tanh", Ftanh, Stanh, 1, 1, 0,
  "Return the hyperbolic tangent of ARG.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = tanh (d), "tanh", arg);
  return make_float (d);
}
#endif

DEFUN ("abs", Fabs, Sabs, 1, 1, 0,
  "Return the absolute value of ARG.")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER_OR_FLOAT (arg, 0);

  if (FLOATP (arg))
    IN_FLOAT (arg = make_float (fabs (XFLOAT (arg)->data)), "abs", arg);
  else if (XINT (arg) < 0)
    XSETINT (arg, - XINT (arg));

  return arg;
}

DEFUN ("float", Ffloat, Sfloat, 1, 1, 0,
  "Return the floating point number equal to ARG.")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER_OR_FLOAT (arg, 0);

  if (INTEGERP (arg))
    return make_float ((double) XINT (arg));
  else				/* give 'em the same float back */
    return arg;
}

DEFUN ("logb", Flogb, Slogb, 1, 1, 0,
  "Returns largest integer <= the base 2 log of the magnitude of ARG.\n\
This is the same as the exponent of a float.")
     (arg)
     Lisp_Object arg;
{
  Lisp_Object val;
  EMACS_INT value;
  double f = extract_float (arg);

  if (f == 0.0)
    value = -(VALMASK >> 1);
  else
    {
#ifdef HAVE_LOGB
      IN_FLOAT (value = logb (f), "logb", arg);
#else
#ifdef HAVE_FREXP
      int ivalue;
      IN_FLOAT (frexp (f, &ivalue), "logb", arg);
      value = ivalue - 1;
#else
      int i;
      double d;
      if (f < 0.0)
	f = -f;
      value = -1;
      while (f < 0.5)
	{
	  for (i = 1, d = 0.5; d * d >= f; i += i)
	    d *= d;
	  f /= d;
	  value -= i;
	}
      while (f >= 1.0)
	{
	  for (i = 1, d = 2.0; d * d <= f; i += i)
	    d *= d;
	  f /= d;
	  value += i;
	}
#endif
#endif
    }
  XSETINT (val, value);
  return val;
}

/* the rounding functions  */

DEFUN ("ceiling", Fceiling, Sceiling, 1, 1, 0,
  "Return the smallest integer no less than ARG.  (Round toward +inf.)")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER_OR_FLOAT (arg, 0);

  if (FLOATP (arg))
    {
      double d;

      IN_FLOAT (d = ceil (XFLOAT (arg)->data), "ceiling", arg);
      FLOAT_TO_INT (d, arg, "ceiling", arg);
    }

  return arg;
}

#endif /* LISP_FLOAT_TYPE */


DEFUN ("floor", Ffloor, Sfloor, 1, 2, 0,
  "Return the largest integer no greater than ARG.  (Round towards -inf.)\n\
With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR.")
  (arg, divisor)
     register Lisp_Object arg, divisor;
{
  CHECK_NUMBER_OR_FLOAT (arg, 0);

  if (! NILP (divisor))
    {
      int i1, i2;

      CHECK_NUMBER_OR_FLOAT (divisor, 1);

#ifdef LISP_FLOAT_TYPE
      if (FLOATP (arg) || FLOATP (divisor))
	{
	  double f1, f2;

	  f1 = FLOATP (arg) ? XFLOAT (arg)->data : XINT (arg);
	  f2 = (FLOATP (divisor) ? XFLOAT (divisor)->data : XINT (divisor));
	  if (f2 == 0)
	    Fsignal (Qarith_error, Qnil);

	  IN_FLOAT2 (f1 = floor (f1 / f2), "floor", arg, divisor);
	  FLOAT_TO_INT2 (f1, arg, "floor", arg, divisor);
	  return arg;
	}
#endif

      i1 = XINT (arg);
      i2 = XINT (divisor);

      if (i2 == 0)
	Fsignal (Qarith_error, Qnil);

      /* With C's /, the result is implementation-defined if either operand
	 is negative, so use only nonnegative operands.  */
      i1 = (i2 < 0
	    ? (i1 <= 0  ?  -i1 / -i2  :  -1 - ((i1 - 1) / -i2))
	    : (i1 < 0  ?  -1 - ((-1 - i1) / i2)  :  i1 / i2));

      XSETINT (arg, i1);
      return arg;
    }

#ifdef LISP_FLOAT_TYPE
  if (FLOATP (arg))
    {
      double d;
      IN_FLOAT (d = floor (XFLOAT (arg)->data), "floor", arg);
      FLOAT_TO_INT (d, arg, "floor", arg);
    }
#endif

  return arg;
}

#ifdef LISP_FLOAT_TYPE

DEFUN ("round", Fround, Sround, 1, 1, 0,
  "Return the nearest integer to ARG.")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER_OR_FLOAT (arg, 0);

  if (FLOATP (arg))
    {
      double d;

      /* Screw the prevailing rounding mode.  */
      IN_FLOAT (d = rint (XFLOAT (arg)->data), "round", arg);
      FLOAT_TO_INT (d, arg, "round", arg);
    }

  return arg;
}

DEFUN ("truncate", Ftruncate, Struncate, 1, 1, 0,
       "Truncate a floating point number to an int.\n\
Rounds the value toward zero.")
  (arg)
     register Lisp_Object arg;
{
  CHECK_NUMBER_OR_FLOAT (arg, 0);

  if (FLOATP (arg))
    {
      double d;

      d = XFLOAT (arg)->data;
      FLOAT_TO_INT (d, arg, "truncate", arg);
    }

  return arg;
}

/* It's not clear these are worth adding.  */

DEFUN ("fceiling", Ffceiling, Sfceiling, 1, 1, 0,
  "Return the smallest integer no less than ARG, as a float.\n\
\(Round toward +inf.\)")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = ceil (d), "fceiling", arg);
  return make_float (d);
}

DEFUN ("ffloor", Fffloor, Sffloor, 1, 1, 0,
  "Return the largest integer no greater than ARG, as a float.\n\
\(Round towards -inf.\)")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = floor (d), "ffloor", arg);
  return make_float (d);
}

DEFUN ("fround", Ffround, Sfround, 1, 1, 0,
  "Return the nearest integer to ARG, as a float.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  IN_FLOAT (d = rint (d), "fround", arg);
  return make_float (d);
}

DEFUN ("ftruncate", Fftruncate, Sftruncate, 1, 1, 0,
       "Truncate a floating point number to an integral float value.\n\
Rounds the value toward zero.")
  (arg)
     register Lisp_Object arg;
{
  double d = extract_float (arg);
  if (d >= 0.0)
    IN_FLOAT (d = floor (d), "ftruncate", arg);
  else
    IN_FLOAT (d = ceil (d), "ftruncate", arg);
  return make_float (d);
}

#ifdef FLOAT_CATCH_SIGILL
static SIGTYPE
float_error (signo)
     int signo;
{
  if (! in_float)
    fatal_error_signal (signo);

#ifdef BSD
#ifdef BSD4_1
  sigrelse (SIGILL);
#else /* not BSD4_1 */
  sigsetmask (SIGEMPTYMASK);
#endif /* not BSD4_1 */
#else
  /* Must reestablish handler each time it is called.  */
  signal (SIGILL, float_error);
#endif /* BSD */

  in_float = 0;

  Fsignal (Qarith_error, Fcons (float_error_arg, Qnil));
}

/* Another idea was to replace the library function `infnan'
   where SIGILL is signaled.  */

#endif /* FLOAT_CATCH_SIGILL */

#ifdef HAVE_MATHERR
int 
matherr (x)
     struct exception *x;
{
  Lisp_Object args;
  if (! in_float)
    /* Not called from emacs-lisp float routines; do the default thing. */
    return 0;
  if (!strcmp (x->name, "pow"))
    x->name = "expt";

  args
    = Fcons (build_string (x->name),
	     Fcons (make_float (x->arg1),
		    ((!strcmp (x->name, "log") || !strcmp (x->name, "pow"))
		     ? Fcons (make_float (x->arg2), Qnil)
		     : Qnil)));
  switch (x->type)
    {
    case DOMAIN:	Fsignal (Qdomain_error, args);		break;
    case SING:		Fsignal (Qsingularity_error, args);	break;
    case OVERFLOW:	Fsignal (Qoverflow_error, args);	break;
    case UNDERFLOW:	Fsignal (Qunderflow_error, args);	break;
    default:		Fsignal (Qarith_error, args);		break;
    }
  return (1);	/* don't set errno or print a message */
}
#endif /* HAVE_MATHERR */

init_floatfns ()
{
#ifdef FLOAT_CATCH_SIGILL
  signal (SIGILL, float_error);
#endif 
  in_float = 0;
}

#else /* not LISP_FLOAT_TYPE */

init_floatfns ()
{}

#endif /* not LISP_FLOAT_TYPE */

syms_of_floatfns ()
{
#ifdef LISP_FLOAT_TYPE
  defsubr (&Sacos);
  defsubr (&Sasin);
  defsubr (&Satan);
  defsubr (&Scos);
  defsubr (&Ssin);
  defsubr (&Stan);
#if 0
  defsubr (&Sacosh);
  defsubr (&Sasinh);
  defsubr (&Satanh);
  defsubr (&Scosh);
  defsubr (&Ssinh);
  defsubr (&Stanh);
  defsubr (&Sbessel_y0);
  defsubr (&Sbessel_y1);
  defsubr (&Sbessel_yn);
  defsubr (&Sbessel_j0);
  defsubr (&Sbessel_j1);
  defsubr (&Sbessel_jn);
  defsubr (&Serf);
  defsubr (&Serfc);
  defsubr (&Slog_gamma);
  defsubr (&Scube_root);
#endif
  defsubr (&Sfceiling);
  defsubr (&Sffloor);
  defsubr (&Sfround);
  defsubr (&Sftruncate);
  defsubr (&Sexp);
  defsubr (&Sexpt);
  defsubr (&Slog);
  defsubr (&Slog10);
  defsubr (&Ssqrt);

  defsubr (&Sabs);
  defsubr (&Sfloat);
  defsubr (&Slogb);
  defsubr (&Sceiling);
  defsubr (&Sround);
  defsubr (&Struncate);
#endif /* LISP_FLOAT_TYPE */
  defsubr (&Sfloor);
}
