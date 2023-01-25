/* Formatted output to a stream.
   Copyright (C) 2004, 2006-2023 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/* Specification.  */
#include <stdio.h>

#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdlib.h>

#include "fseterr.h"
#include "vasnprintf.h"

/* Print formatted output to the stream FP.
   Return string length of formatted string.  On error, return a negative
   value.  */
int
vfprintf (FILE *fp, const char *format, va_list args)
{
  char buf[2000];
  char *output;
  size_t len;
  size_t lenbuf = sizeof (buf);

  output = vasnprintf (buf, &lenbuf, format, args);
  len = lenbuf;

  if (!output)
    {
      fseterr (fp);
      return -1;
    }

  if (fwrite (output, 1, len, fp) < len)
    {
      if (output != buf)
        free (output);
      return -1;
    }

  if (output != buf)
    free (output);

  if (len > INT_MAX)
    {
      errno = EOVERFLOW;
      fseterr (fp);
      return -1;
    }

  return len;
}
