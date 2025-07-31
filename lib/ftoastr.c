/* floating point to accurate string

   Copyright (C) 2010-2025 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert.  */

/* This code can misbehave on some buggy or older platforms, when
   operating on arguments on floating types other than 'double', or
   when given unusual combinations of options.  Gnulib's
   snprintf-posix module works around many of these problems.

   This code relies on sprintf, strtod, etc. operating accurately;
   otherwise, the resulting strings could be inaccurate or too long.  */

#include <config.h>

#include "ftoastr.h"

#include <float.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef C_LOCALE
# include "c-snprintf.h"
# include "c-strtod.h"
# define PREFIX(name) c_ ## name
#else
# define PREFIX(name) name
#endif

#if LENGTH == 3
# define FLOAT long double
# define FLOAT_DIG LDBL_DIG
# define FLOAT_MIN LDBL_MIN
# define FLOAT_PREC_BOUND _GL_LDBL_PREC_BOUND
# define FTOASTR PREFIX (ldtoastr)
# define PROMOTED_FLOAT long double
# define STRTOF PREFIX (strtold)
#elif LENGTH == 2
# define FLOAT double
# define FLOAT_DIG DBL_DIG
# define FLOAT_MIN DBL_MIN
# define FLOAT_PREC_BOUND _GL_DBL_PREC_BOUND
# define FTOASTR PREFIX (dtoastr)
# define PROMOTED_FLOAT double
#else
# define LENGTH 1
# define FLOAT float
# define FLOAT_DIG FLT_DIG
# define FLOAT_MIN FLT_MIN
# define FLOAT_PREC_BOUND _GL_FLT_PREC_BOUND
# define FTOASTR PREFIX (ftoastr)
# define PROMOTED_FLOAT double
# if HAVE_STRTOF
#  define STRTOF strtof
# endif
#endif

/* On pre-C99 hosts, approximate strtof with strtod.  This
   may generate one or two extra digits, but that's better than not
   working at all.  */
#ifndef STRTOF
# define STRTOF PREFIX (strtod)
#endif

/* On hosts where it's not known that snprintf works, use sprintf to
   implement the subset needed here.  Typically BUFSIZE is big enough
   and there's little or no performance hit.  */
#ifdef C_LOCALE
# undef snprintf
# define snprintf c_snprintf
#elif ! GNULIB_SNPRINTF
# undef snprintf
# define snprintf ftoastr_snprintf
static int
ftoastr_snprintf (char *buf, size_t bufsize, char const *format,
                  int width, int prec, FLOAT x)
{
  PROMOTED_FLOAT promoted_x = x;
  char width_0_buffer[LENGTH == 1 ? FLT_BUFSIZE_BOUND
                      : LENGTH == 2 ? DBL_BUFSIZE_BOUND
                      : LDBL_BUFSIZE_BOUND];
  int n = width;
  if (bufsize < sizeof width_0_buffer)
    {
      n = sprintf (width_0_buffer, format, 0, prec, promoted_x);
      if (n < 0)
        return n;
      if (n < width)
        n = width;
    }
  if (n < bufsize)
    n = sprintf (buf, format, width, prec, promoted_x);
  return n;
}
#endif

int
FTOASTR (char *buf, size_t bufsize, int flags, int width, FLOAT x)
{
  /* The following method is simple but slow.
     For ideas about speeding things up, please see:

     Jeon J. Dragonbox: a new floating-point binary-to-decimal
     conversion algorithm. 2024. <https://github.com/jk-jeon/dragonbox/>.
     Used in {fmt} <https://github.com/fmtlib/fmt>.

     Adams U. Ryū: fast float-to-string conversion.
     PLDI 2018. 270–282. <https://doi.org/10.1145/3192366.3192369>.  */

  PROMOTED_FLOAT promoted_x = x;
  char format[sizeof "%-+ 0*.*Lg"];
  FLOAT abs_x = x < 0 ? -x : x;
  int prec;

  char *p = format;
  *p++ = '%';

  /* Support flags that generate output parsable by strtof.  */
  *p = '-'; p += (flags & FTOASTR_LEFT_JUSTIFY  ) != 0;
  *p = '+'; p += (flags & FTOASTR_ALWAYS_SIGNED ) != 0;
  *p = ' '; p += (flags & FTOASTR_SPACE_POSITIVE) != 0;
  *p = '0'; p += (flags & FTOASTR_ZERO_PAD      ) != 0;

  *p++ = '*';
  *p++ = '.';
  *p++ = '*';
  *p = 'L'; p += 2 < LENGTH;
  *p++ = flags & FTOASTR_UPPER_E ? 'G' : 'g';
  *p = '\0';

  for (prec = abs_x < FLOAT_MIN ? 1 : FLOAT_DIG; ; prec++)
    {
      int n = snprintf (buf, bufsize, format, width, prec, promoted_x);
      if (n < 0
          || FLOAT_PREC_BOUND <= prec
          || (n < bufsize && STRTOF (buf, NULL) == x))
        return n;
    }
}
