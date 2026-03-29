/* Output like sprintf to a buffer of specified size.    -*- coding: utf-8 -*-
   Also takes args differently: pass one pointer to the end
   of the format string in addition to the format string itself.
   Copyright (C) 1985, 2001-2026 Free Software Foundation, Inc.

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

/* If you think about replacing this with some similar standard C function of
   the printf family (such as vsnprintf), please note that this function
   supports the following Emacs-specific features:

   . For %c conversions, it produces a string with the multibyte representation
     of the ('int') argument, suitable for display in an Emacs buffer.

   . For %s and %c, when field width is specified (e.g., %25s), it accounts for
     the display width of each character, according to char-width-table.  That
     is, it does not assume that each character takes one column on display.
     Nor does it assume that each character is a single byte.

   . If the size of the buffer is not enough to produce the formatted string in
     its entirety, it makes sure that truncation does not chop the last
     character in the middle of its multibyte sequence, producing an invalid
     sequence.

   . It accepts a pointer to the end of the format string, so the format string
     could include embedded null characters.

   . It signals an error if the length of the formatted string is about to
     overflow ptrdiff_t or size_t, to avoid producing strings longer than what
     Emacs can handle.

   On the other hand, this function supports only a small subset of the
   standard C formatted output facilities.  E.g., precision is ignored
   in %s and %c conversions, and %lld does not necessarily work and
   code should use something like %"pM"d with intmax_t instead.
   (See below for the detailed documentation of what is supported.)
   However, this is okay, as this function is supposed to be called
   from 'error' and similar C functions, and thus does not need to
   support all the features of 'Fformat_message', which is used by the
   Lisp 'error' function.  */

/* In the FORMAT argument this function supports ` and ' as directives
   that output left and right quotes as per ‘text-quoting style’.  It
   also supports the following %-sequences:

   %s means print a string argument.
   %S is treated as %s, for loose compatibility with 'Fformat_message'.
   %d means print a 'signed int' argument in decimal.
   %o means print an 'unsigned int' argument in octal.
   %u means print an 'unsigned int' argument in decimal.
   %x means print an 'unsigned int' argument in lower-case hex.
   %X means print an 'unsigned int' argument in upper-case hex.
   %e means print a 'double' argument in exponential notation.
   %f means print a 'double' argument in decimal-point notation.
   %g means print a 'double' argument in exponential notation
      or in decimal-point notation, depending on the value;
      this is often (though not always) the shorter of the two notations.
   %c means print a 'signed int' argument as a single character.
   %% means produce a literal % character.

   A %-sequence other than %% may contain optional flags, width, precision,
   and length, as follows:

     %<flags><width><precision><length>character

   where flags is [+ -0], width is [0-9]+, precision is .[0-9]+, and length
   is empty or l or the value of the pD or pI or PRIdMAX (sans "d") macros.
   A % that does not introduce a valid %-sequence causes undefined behavior.
   Bytes in FORMAT other than % are copied through as-is.

   The + flag character inserts a + before any positive number, while a space
   inserts a space before any positive number; these flags only affect %d, %o,
   %x, %e, %f, and %g sequences.  The - and 0 flags affect the width specifier,
   as described below.  For signed numerical arguments only, the ' ' (space)
   flag causes the result to be prefixed with a space character if it does not
   start with a sign (+ or -).

   The l (lower-case letter ell) length modifier is a 'long' data type
   modifier: it is supported for %d, %o, and %x conversions of integral
   arguments, must immediately precede the conversion specifier, and means that
   the respective argument is to be treated as 'long int' or 'unsigned long
   int'.  Similarly, the value of the pD macro means to use ptrdiff_t,
   the value of the pI macro means to use EMACS_INT or EMACS_UINT, the
   value of the PRIdMAX etc. macros means to use intmax_t or uintmax_t,
   and the empty length modifier means 'int' or 'unsigned int'.

   The width specifier supplies a lower limit for the length of the printed
   representation.  The padding, if any, normally goes on the left, but it goes
   on the right if the - flag is present.  The padding character is normally a
   space, but (for numerical arguments only) it is 0 if the 0 flag is present.
   The - flag takes precedence over the 0 flag.

   For %e, %f, and %g sequences, the number after the "." in the precision
   specifier says how many decimal places to show; if zero, the decimal point
   itself is omitted.  For %d, %o, and %x sequences, the precision specifies
   the minimum number of digits to appear.  Precision specifiers are
   not supported for other %-sequences.  */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <unistd.h>
#include <limits.h>

#include "lisp.h"

/* Since we use the macro CHAR_HEAD_P, we have to include this, but
   don't have to include others because CHAR_HEAD_P does not contains
   another macro.  */
#include "character.h"

/* Enough to handle floating point formats with large numbers.  */
enum { SIZE_BOUND_EXTRA = DBL_MAX_10_EXP + 50 };

/* Parse FMT as an unsigned decimal integer, putting its value into *VALUE.
   Return the address of the first byte after the integer.
   If FMT is not an integer, return FMT and store zero into *VALUE.  */
static char const *
parse_format_integer (char const *fmt, int *value)
{
  int n = 0;
  bool overflow = false;
  for (; '0' <= *fmt && *fmt <= '9'; fmt++)
    {
      overflow |= ckd_mul (&n, n, 10);
      overflow |= ckd_add (&n, n, *fmt - '0');
    }
  if (overflow || min (PTRDIFF_MAX, SIZE_MAX) - SIZE_BOUND_EXTRA < n)
    error ("Format width or precision too large");
  *value = n;
  return fmt;
}

/* Like doprnt, except FORMAT_END must be non-null.
   Although this function is never exercised in current Emacs,
   it is retained in case some future Emacs version
   contains doprnt callers that need such formats.
   Having a separate function helps GCC optimize doprnt better.  */
static ptrdiff_t
doprnt_non_null_end (char *buffer, ptrdiff_t bufsize, char const *format,
		     char const *format_end, va_list ap)
{
  USE_SAFE_ALLOCA;
  ptrdiff_t fmtlen = format_end - format;
  char *fmt = SAFE_ALLOCA (fmtlen + 1);
  memcpy (fmt, format, fmtlen);
  fmt[fmtlen] = 0;
  ptrdiff_t nbytes = doprnt (buffer, bufsize, fmt, NULL, ap);
  SAFE_FREE ();
  return nbytes;
}

/* Format to BUFFER (of positive size BUFSIZE) data formatted by FORMAT,
   terminated at either the first NUL or (if FORMAT_END is non-null
   and there are no NUL bytes between FORMAT and FORMAT_END)
   terminated at position FORMAT_END.  AP specifies format arguments.
   (*FORMAT_END is not part of the format, but must exist and be readable.)
   If the output does not fit, truncate it to fit and return BUFSIZE - 1;
   if this truncates a multibyte sequence,
   store '\0' into the sequence's first byte.
   Return the number of bytes stored into BUFFER, excluding
   the terminating null byte.  Output is always null-terminated.
   String arguments are passed as C strings.
   Integers are passed as C integers.

   FIXME: If FORMAT_END is not at a character boundary
   doprnt_non_null_end will cut the string in the middle of the
   character and the returned string will have an incomplete character
   sequence at the end.  We may prefer to cut at a character
   boundary.  */

ptrdiff_t
doprnt (char *buffer, ptrdiff_t bufsize, const char *format,
	const char *format_end, va_list ap)
{
  if (format_end)
    return doprnt_non_null_end (buffer, bufsize, format, format_end, ap);

  const char *fmt = format;	/* Pointer into format string.  */
  char *bufptr = buffer;	/* Pointer into output buffer.  */

  /* Use this for sprintf unless we need something really big.  */
  char tembuf[SIZE_BOUND_EXTRA + 50];

  /* Size of sprintf_buffer.  */
  ptrdiff_t size_allocated = sizeof (tembuf);

  /* Buffer to use for sprintf.  Either tembuf or same as BIG_BUFFER.  */
  char *sprintf_buffer = tembuf;

  /* Buffer we have got with malloc.  */
  char *big_buffer = NULL;

  Lisp_Object quoting_style = Ftext_quoting_style ();

  bufsize--;

  /* Loop until end of format string or buffer full. */
  while (*fmt && bufsize > 0)
    {
      char const *fmt0 = fmt;
      char fmtchar = *fmt++;
      if (fmtchar == '%')
	{
	  ptrdiff_t width;  /* Columns occupied by STRING on display.  */
	  enum {
	    pDlen = sizeof pD - 1,
	    pIlen = sizeof pI - 1,
	    pMlen = sizeof PRIdMAX - 2,
	    maxmlen = max (max (1, pDlen), max (pIlen, pMlen))
	  };
	  enum {
	    no_modifier, long_modifier, pD_modifier, pI_modifier, pM_modifier
	  } length_modifier = no_modifier;
	  static char const modifier_len[] = { 0, 1, pDlen, pIlen, pMlen };
	  int mlen;
	  char charbuf[MAX_MULTIBYTE_LENGTH + 1];	/* Used for %c.  */

	  /* Width and precision specified by this %-sequence.  */
	  int wid = 0, prec = -1;

	  /* FMTSTAR will be a "%*.*X"-like version of this %-sequence.
	     Start by putting '%' into FMTSTAR.  */
	  char fmtstar[sizeof "%-+ 0*.*d" + maxmlen];
	  char *string = fmtstar;
	  *string++ = '%';

	  /* Copy at most one instance of each flag into FMTSTAR.  */
	  bool minusflag = false, plusflag = false, zeroflag = false,
	    spaceflag = false;
	  for (;; fmt++)
	    {
	      *string = *fmt;
	      switch (*fmt)
		{
		case '-': string += !minusflag; minusflag = true; continue;
		case '+': string += !plusflag; plusflag = true; continue;
		case ' ': string += !spaceflag; spaceflag = true; continue;
		case '0': string += !zeroflag; zeroflag = true; continue;
		}
	      break;
	    }

	  /* Parse width and precision, putting "*.*" into FMTSTAR.  */
	  if ('1' <= *fmt && *fmt <= '9')
	    fmt = parse_format_integer (fmt, &wid);
	  if (*fmt == '.')
	    fmt = parse_format_integer (fmt + 1, &prec);
	  *string++ = '*';
	  *string++ = '.';
	  *string++ = '*';

	  /* Check for the length modifiers in textual length order, so
	     that longer modifiers override shorter ones.  */
	  for (mlen = 1; mlen <= maxmlen; mlen++)
	    {
	      if (mlen == 1 && *fmt == 'l')
		length_modifier = long_modifier;
	      if (mlen == pDlen && strncmp (fmt, pD, pDlen) == 0)
		length_modifier = pD_modifier;
	      if (mlen == pIlen && strncmp (fmt, pI, pIlen) == 0)
		length_modifier = pI_modifier;
	      if (mlen == pMlen && strncmp (fmt, PRIdMAX, pMlen) == 0)
		length_modifier = pM_modifier;
	    }

	  /* Copy optional length modifier and conversion specifier
	     character into FMTSTAR, and append a NUL.  */
	  mlen = modifier_len[length_modifier];
	  string = mempcpy (string, fmt, mlen + 1);
	  fmt += mlen;
	  *string = 0;

	  /* An idea of how much space we might need.
	     This might be a field width or a precision; e.g.
	     %1.1000f and %1000.1f both might need 1000+ bytes.
	     Make it large enough to handle floating point formats
	     with large numbers.  */
	  ptrdiff_t size_bound = max (wid, prec) + SIZE_BOUND_EXTRA;

	  /* Make sure we have that much.  */
	  if (size_bound > size_allocated)
	    {
	      if (big_buffer)
		xfree (big_buffer);
	      big_buffer = xmalloc (size_bound);
	      sprintf_buffer = big_buffer;
	      size_allocated = size_bound;
	    }
	  int minlen = 0;
	  ptrdiff_t tem;
	  switch (*fmt++)
	    {
	    default:
	      error ("Invalid format operation %s", fmt0);

	    case 'd':
	      switch (length_modifier)
		{
		case no_modifier:
		  {
		    int v = va_arg (ap, int);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		case long_modifier:
		  {
		    long v = va_arg (ap, long);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		case pD_modifier:
		signed_pD_modifier:
		  {
		    ptrdiff_t v = va_arg (ap, ptrdiff_t);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		case pI_modifier:
		  {
		    EMACS_INT v = va_arg (ap, EMACS_INT);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		case pM_modifier:
		  {
		    intmax_t v = va_arg (ap, intmax_t);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		default:
		  eassume (false);
		}
	      /* Now copy into final output, truncating as necessary.  */
	      string = sprintf_buffer;
	      goto doit;

	    case 'o':
	    case 'u':
	    case 'x': case 'X':
	      switch (length_modifier)
		{
		case no_modifier:
		  {
		    unsigned v = va_arg (ap, unsigned);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		case long_modifier:
		  {
		    unsigned long v = va_arg (ap, unsigned long);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		case pD_modifier:
		  goto signed_pD_modifier;
		case pI_modifier:
		  {
		    EMACS_UINT v = va_arg (ap, EMACS_UINT);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		case pM_modifier:
		  {
		    uintmax_t v = va_arg (ap, uintmax_t);
		    tem = sprintf (sprintf_buffer, fmtstar, wid, prec, v);
		  }
		  break;
		default:
		  eassume (false);
		}
	      /* Now copy into final output, truncating as necessary.  */
	      string = sprintf_buffer;
	      goto doit;

	    case 'f':
	    case 'e':
	    case 'g':
	      {
		double d = va_arg (ap, double);
		tem = sprintf (sprintf_buffer, fmtstar, wid, prec, d);
		/* Now copy into final output, truncating as necessary.  */
		string = sprintf_buffer;
		goto doit;
	      }

	    case 'S':
	    case 's':
	      minlen = minusflag ? -wid : wid;
	      string = va_arg (ap, char *);
	      tem = strnlen (string, STRING_BYTES_BOUND + 1);
	      if (tem == STRING_BYTES_BOUND + 1)
		error ("String for %%s or %%S format is too long");
	      width = strwidth (string, tem);
	      goto doit1;

	      /* Copy string into final output, truncating if no room.  */
	    doit:
	      eassert (0 <= tem);
	      /* Coming here means STRING contains ASCII only.  */
	      if (STRING_BYTES_BOUND < tem)
		error ("Format width or precision too large");
	      width = tem;
	    doit1:
	      /* We have already calculated:
		 TEM -- length of STRING,
		 WIDTH -- columns occupied by STRING when displayed, and
		 MINLEN -- minimum columns of the output.  */
	      if (minlen > 0)
		{
		  while (minlen > width && bufsize > 0)
		    {
		      *bufptr++ = ' ';
		      bufsize--;
		      minlen--;
		    }
		  minlen = 0;
		}
	      if (tem > bufsize)
		{
		  /* Truncate the string at character boundary.  */
		  tem = bufsize;
		  do
		    {
		      tem--;
		      if (CHAR_HEAD_P (string[tem]))
			{
			  if (BYTES_BY_CHAR_HEAD (string[tem]) <= bufsize - tem)
			    tem = bufsize;
			  break;
			}
		    }
		  while (tem != 0);

		  memcpy (bufptr, string, tem);
		  while (tem < bufsize)
		    bufptr[tem++] = 0;
		  /* Trigger exit from the loop, but make sure we
		     return to the caller a value which will indicate
		     that the buffer was too small.  */
		  bufptr += bufsize;
		  bufsize = 0;
		  continue;
		}
	      memcpy (bufptr, string, tem);
	      bufptr += tem;
	      bufsize -= tem;
	      if (minlen < 0)
		{
		  while (minlen < - width && bufsize > 0)
		    {
		      *bufptr++ = ' ';
		      bufsize--;
		      minlen++;
		    }
		  minlen = 0;
		}
	      continue;

	    case 'c':
	      {
		int chr = va_arg (ap, int);
		tem = CHAR_STRING (chr, (unsigned char *) charbuf);
		string = charbuf;
		string[tem] = 0;
		width = strwidth (string, tem);
		minlen = minusflag ? -wid : wid;
		goto doit1;
	      }

	    case '%':
	      /* Treat this '%' as normal.  */
	      break;
	    }
	}

      char const *src;
      ptrdiff_t srclen;
      if (EQ (quoting_style, Qcurve) && fmtchar == '`')
	src = uLSQM, srclen = sizeof uLSQM - 1;
      else if (EQ (quoting_style, Qcurve) && fmtchar == '\'')
	src = uRSQM, srclen = sizeof uRSQM - 1;
      else if (! LEADING_CODE_P (fmtchar))
	{
	  if (EQ (quoting_style, Qstraight) && fmtchar == '`')
	    fmtchar = '\'';

	  *bufptr++ = fmtchar;
	  bufsize--;
	  continue;
	}
      else
        {
          int charlen = BYTES_BY_CHAR_HEAD (fmtchar);
          src = fmt0;

          /* If the format string ends in the middle of a multibyte
             character we don't want to skip over the NUL byte.  */
          for (srclen = 1 ; *(src + srclen) != 0 && srclen < charlen ; srclen++);

          fmt = src + srclen;
        }

      if (bufsize < srclen)
	{
	  /* Truncate, but return value that will signal to caller
	     that the buffer was too small.  */
	  do
	    *bufptr++ = '\0';
	  while (--bufsize != 0);
	}
      else
	{
	  do
	    {
	      *bufptr++ = *src++;
	      bufsize--;
	    }
	  while (--srclen != 0);
	}
    }

  /* If we had to malloc something, free it.  */
  xfree (big_buffer);

  *bufptr = 0;		/* Make sure our string ends with a '\0' */
  return bufptr - buffer;
}

/* Format to an unbounded buffer BUF.  This is like sprintf, except it
   is not limited to returning an 'int' so it doesn't have a silly 2
   GiB limit on typical 64-bit hosts.  However, it is limited to the
   Emacs-style formats that doprnt supports, and it requotes ` and '
   as per ‘text-quoting-style’.

   Return the number of bytes put into BUF, excluding the terminating
   '\0'.  */
ptrdiff_t
esprintf (char *buf, char const *format, ...)
{
  ptrdiff_t nbytes;
  va_list ap;
  va_start (ap, format);
  nbytes = doprnt (buf, TYPE_MAXIMUM (ptrdiff_t), format, 0, ap);
  va_end (ap);
  return nbytes;
}

#if defined HAVE_X_WINDOWS && defined USE_X_TOOLKIT

/* Format to buffer *BUF of positive size *BUFSIZE, reallocating *BUF
   and updating *BUFSIZE if the buffer is too small, and otherwise
   behaving line esprintf.  When reallocating, free *BUF unless it is
   equal to NONHEAPBUF, and if BUFSIZE_MAX is nonnegative then signal
   memory exhaustion instead of growing the buffer size past
   BUFSIZE_MAX.  */
ptrdiff_t
exprintf (char **buf, ptrdiff_t *bufsize,
	  char *nonheapbuf, ptrdiff_t bufsize_max,
	  char const *format, ...)
{
  ptrdiff_t nbytes;
  va_list ap;
  va_start (ap, format);
  nbytes = evxprintf (buf, bufsize, nonheapbuf, bufsize_max, format, ap);
  va_end (ap);
  return nbytes;
}

#endif

/* Act like exprintf, except take a va_list.  */
ptrdiff_t
evxprintf (char **buf, ptrdiff_t *bufsize,
	   char *nonheapbuf, ptrdiff_t bufsize_max,
	   char const *format, va_list ap)
{
  for (;;)
    {
      ptrdiff_t nbytes;
      va_list ap_copy;
      va_copy (ap_copy, ap);
      nbytes = doprnt (*buf, *bufsize, format, 0, ap_copy);
      va_end (ap_copy);
      if (nbytes < *bufsize - 1)
	return nbytes;
      if (*buf != nonheapbuf)
	{
	  xfree (*buf);
	  *buf = NULL;
	}
      *buf = xpalloc (NULL, bufsize, 1, bufsize_max, 1);
    }
}
