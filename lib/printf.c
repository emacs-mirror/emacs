/* Formatted output to a stream.
   Copyright (C) 2007, 2010-2023 Free Software Foundation, Inc.

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

#include <stdarg.h>

/* Print formatted output to standard output.
   Return string length of formatted string.  On error, return a negative
   value.  */
int
printf (const char *format, ...)
{
  int retval;
  va_list args;

  va_start (args, format);
  retval = vfprintf (stdout, format, args);
  va_end (args);

  return retval;
}
