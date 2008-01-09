/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
   Copyright (C) 1985, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007, 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include <config.h>
#include <stdio.h>
#include <ctype.h>

#ifdef STDC_HEADERS
#include <float.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "lisp.h"

#ifndef DBL_MAX_10_EXP
#define DBL_MAX_10_EXP 308 /* IEEE double */
#endif

/* Since we use the macro CHAR_HEAD_P, we have to include this, but
   don't have to include others because CHAR_HEAD_P does not contains
   another macro.  */
#include "character.h"

static int doprnt1 ();

/* Generate output from a format-spec FORMAT,
   terminated at position FORMAT_END.
   Output goes in BUFFER, which has room for BUFSIZE chars.
   If the output does not fit, truncate it to fit.
   Returns the number of bytes stored into BUFFER.
   ARGS points to the vector of arguments, and NARGS says how many.
   A double counts as two arguments.
   String arguments are passed as C strings.
   Integers are passed as C integers.  */

int
doprnt (buffer, bufsize, format, format_end, nargs, args)
     char *buffer;
     register int bufsize;
     char *format;
     char *format_end;
     int nargs;
     char **args;
{
  return doprnt1 (0, buffer, bufsize, format, format_end, nargs, args);
}

/* Like doprnt except that strings in ARGS are passed
   as Lisp_Object.  */

int
doprnt_lisp (buffer, bufsize, format, format_end, nargs, args)
     char *buffer;
     register int bufsize;
     char *format;
     char *format_end;
     int nargs;
     char **args;
{
  return doprnt1 (1, buffer, bufsize, format, format_end, nargs, args);
}

static int
doprnt1 (lispstrings, buffer, bufsize, format, format_end, nargs, args)
     int lispstrings;
     char *buffer;
     register int bufsize;
     char *format;
     char *format_end;
     int nargs;
     char **args;
{
  int cnt = 0;			/* Number of arg to gobble next */
  register char *fmt = format;	/* Pointer into format string */
  register char *bufptr = buffer; /* Pointer into output buffer.. */

  /* Use this for sprintf unless we need something really big.  */
  char tembuf[DBL_MAX_10_EXP + 100];

  /* Size of sprintf_buffer.  */
  unsigned size_allocated = sizeof (tembuf);

  /* Buffer to use for sprintf.  Either tembuf or same as BIG_BUFFER.  */
  char *sprintf_buffer = tembuf;

  /* Buffer we have got with malloc.  */
  char *big_buffer = 0;

  register int tem;
  unsigned char *string;
  char fixed_buffer[20];	/* Default buffer for small formatting. */
  char *fmtcpy;
  int minlen;
  unsigned char charbuf[5];	/* Used for %c.  */

  if (format_end == 0)
    format_end = format + strlen (format);

  if ((format_end - format + 1) < sizeof (fixed_buffer))
    fmtcpy = fixed_buffer;
  else
    fmtcpy = (char *) alloca (format_end - format + 1);

  bufsize--;

  /* Loop until end of format string or buffer full. */
  while (fmt != format_end && bufsize > 0)
    {
      if (*fmt == '%')	/* Check for a '%' character */
	{
	  unsigned size_bound = 0;
	  int width;		/* Columns occupied by STRING.  */

	  fmt++;
	  /* Copy this one %-spec into fmtcpy.  */
	  string = (unsigned char *) fmtcpy;
	  *string++ = '%';
	  while (1)
	    {
	      *string++ = *fmt;
	      if ('0' <= *fmt && *fmt <= '9')
		{
		  /* Get an idea of how much space we might need.
		     This might be a field width or a precision; e.g.
		     %1.1000f and %1000.1f both might need 1000+ bytes.
		     Parse the width or precision, checking for overflow.  */
		  unsigned n = *fmt - '0';
		  while ('0' <= fmt[1] && fmt[1] <= '9')
		    {
		      if (n * 10 / 10 != n
			  || (n = n * 10 + (fmt[1] - '0')) < n)
			error ("Format width or precision too large");
		      *string++ = *++fmt;
		    }

		  if (size_bound < n)
		    size_bound = n;
		}
	      else if (*fmt == '-' || *fmt == ' ' || *fmt == '.' || *fmt == '+')
		;
	      else
		break;
	      fmt++;
	    }
	  *string = 0;

	  /* Make the size bound large enough to handle floating point formats
	     with large numbers.  */
	  if (size_bound + DBL_MAX_10_EXP + 50 < size_bound)
	    error ("Format width or precision too large");
	  size_bound += DBL_MAX_10_EXP + 50;

	  /* Make sure we have that much.  */
	  if (size_bound > size_allocated)
	    {
	      if (big_buffer)
		big_buffer = (char *) xrealloc (big_buffer, size_bound);
	      else
		big_buffer = (char *) xmalloc (size_bound);
	      sprintf_buffer = big_buffer;
	      size_allocated = size_bound;
	    }
	  minlen = 0;
	  switch (*fmt++)
	    {
	    default:
	      error ("Invalid format operation %%%c", fmt[-1]);

/*	    case 'b': */
	    case 'd':
	    case 'o':
	    case 'x':
	      if (cnt == nargs)
		error ("Not enough arguments for format string");
	      if (sizeof (int) == sizeof (EMACS_INT))
		;
	      else if (sizeof (long) == sizeof (EMACS_INT))
		/* Insert an `l' the right place.  */
		string[1] = string[0],
		string[0] = string[-1],
		string[-1] = 'l',
		string++;
	      else
		abort ();
	      sprintf (sprintf_buffer, fmtcpy, args[cnt++]);
	      /* Now copy into final output, truncating as nec.  */
	      string = (unsigned char *) sprintf_buffer;
	      goto doit;

	    case 'f':
	    case 'e':
	    case 'g':
	      {
		union { double d; char *half[2]; } u;
		if (cnt + 1 == nargs)
		  error ("Not enough arguments for format string");
		u.half[0] = args[cnt++];
		u.half[1] = args[cnt++];
		sprintf (sprintf_buffer, fmtcpy, u.d);
		/* Now copy into final output, truncating as nec.  */
		string = (unsigned char *) sprintf_buffer;
		goto doit;
	      }

	    case 'S':
	      string[-1] = 's';
	    case 's':
	      if (cnt == nargs)
		error ("Not enough arguments for format string");
	      if (fmtcpy[1] != 's')
		minlen = atoi (&fmtcpy[1]);
	      if (lispstrings)
		{
		  string = ((struct Lisp_String *) args[cnt])->data;
		  tem = STRING_BYTES ((struct Lisp_String *) args[cnt]);
		  cnt++;
		}
	      else
		{
		  string = (unsigned char *) args[cnt++];
		  tem = strlen (string);
		}
	      width = strwidth (string, tem);
	      goto doit1;

	      /* Copy string into final output, truncating if no room.  */
	    doit:
	      /* Coming here means STRING contains ASCII only.  */
	      width = tem = strlen (string);
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
		  while (!CHAR_HEAD_P (string[tem - 1])) tem--;
		  bcopy (string, bufptr, tem);
		  /* We must calculate WIDTH again.  */
		  width = strwidth (bufptr, tem);
		}
	      else
		bcopy (string, bufptr, tem);
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
	      if (cnt == nargs)
		error ("Not enough arguments for format string");
	      tem = CHAR_STRING ((int) (EMACS_INT) args[cnt], charbuf);
	      string = charbuf;
	      cnt++;
	      string[tem] = 0;
	      width = strwidth (string, tem);
	      if (fmtcpy[1] != 'c')
		minlen = atoi (&fmtcpy[1]);
	      goto doit1;

	    case '%':
	      fmt--;    /* Drop thru and this % will be treated as normal */
	    }
	}

      {
	/* Just some character; Copy it if the whole multi-byte form
	   fit in the buffer.  */
	char *save_bufptr = bufptr;

	do { *bufptr++ = *fmt++; }
	while (--bufsize > 0 && !CHAR_HEAD_P (*fmt));
	if (!CHAR_HEAD_P (*fmt))
	  {
	    bufptr = save_bufptr;
	    break;
	  }
      }
    };

  /* If we had to malloc something, free it.  */
  if (big_buffer)
    xfree (big_buffer);

  *bufptr = 0;		/* Make sure our string end with a '\0' */
  return bufptr - buffer;
}

/* arch-tag: aa0ab528-7c5f-4c73-894c-aa2526a1efb3
   (do not change this comment) */
