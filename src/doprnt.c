/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
   Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <config.h>
#include <stdio.h>
#include <ctype.h>

extern long *xmalloc (), *xrealloc ();

/* Generate output from a format-spec FORMAT,
   terminated at position FORMAT_END.
   Output goes in BUFFER, which has room for BUFSIZE chars.
   If the output does not fit, truncate it to fit.
   Returns the number of characters stored into BUFFER.
   ARGS points to the vector of arguments, and NARGS says how many.
   A double counts as two arguments.  */

doprnt (buffer, bufsize, format, format_end, nargs, args)
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
  char tembuf[100];

  /* Size of sprintf_buffer.  */
  int size_allocated = 100;

  /* Buffer to use for sprintf.  Either tembuf or same as BIG_BUFFER.  */
  char *sprintf_buffer = tembuf;

  /* Buffer we have got with malloc.  */
  char *big_buffer = 0;

  register int tem;
  char *string;
  char fixed_buffer[20];	/* Default buffer for small formatting. */
  char *fmtcpy;
  int minlen;
  int size;			/* Field width factor; e.g., %90d */
  char charbuf[2];		/* Used for %c.  */

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
	  int size_bound;

	  fmt++;
	  /* Copy this one %-spec into fmtcpy.  */
	  string = fmtcpy;
	  *string++ = '%';
	  while (1)
	    {
	      *string++ = *fmt;
	      if (! (*fmt >= '0' && *fmt <= '9')
		  && *fmt != '-' && *fmt != ' '&& *fmt != '.')
		break;
	      fmt++;
	    }
	  *string = 0;
	  /* Get an idea of how much space we might need.  */
	  size_bound = atoi (&fmtcpy[1]);

	  /* Avoid pitfall of negative "size" parameter ("%-200d"). */
	  if (size_bound < 0)
	    size_bound = -size_bound;
	  size_bound += 50;

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
		error ("not enough arguments for format string");
	      sprintf (sprintf_buffer, fmtcpy, args[cnt++]);
	      /* Now copy into final output, truncating as nec.  */
	      string = sprintf_buffer;
	      goto doit;

	    case 'f':
	    case 'e':
	    case 'g':
	      {
		union { double d; char *half[2]; } u;
		if (cnt + 1 == nargs)
		  error ("not enough arguments for format string");
		u.half[0] = args[cnt++];
		u.half[1] = args[cnt++];
		sprintf (sprintf_buffer, fmtcpy, u.d);
		/* Now copy into final output, truncating as nec.  */
		string = sprintf_buffer;
		goto doit;
	      }

	    case 'S':
	      string[-1] = 's';
	    case 's':
	      if (cnt == nargs)
		error ("not enough arguments for format string");
	      string = args[cnt++];
	      if (fmtcpy[1] != 's')
		minlen = atoi (&fmtcpy[1]);
	      /* Copy string into final output, truncating if no room.  */
	    doit:
	      tem = strlen (string);
	    doit1:
	      if (minlen > 0)
		{
		  while (minlen > tem && bufsize > 0)
		    {
		      *bufptr++ = ' ';
		      bufsize--;
		      minlen--;
		    }
		  minlen = 0;
		}
	      if (tem > bufsize)
		tem = bufsize;
	      strncpy (bufptr, string, tem);
	      bufptr += tem;
	      bufsize -= tem;
	      if (minlen < 0)
		{
		  while (minlen < - tem && bufsize > 0)
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
		error ("not enough arguments for format string");
	      *charbuf = (EMACS_INT) args[cnt++];
	      string = charbuf;
	      tem = 1;
	      if (fmtcpy[1] != 'c')
		minlen = atoi (&fmtcpy[1]);
	      goto doit1;

	    case '%':
	      fmt--;    /* Drop thru and this % will be treated as normal */
	    }
	}
      *bufptr++ = *fmt++;	/* Just some characters; Copy 'em */
      bufsize--;
    };

  /* If we had to malloc something, free it.  */
  if (big_buffer)
    xfree (big_buffer);

  *bufptr = 0;		/* Make sure our string end with a '\0' */
  return bufptr - buffer;
}
