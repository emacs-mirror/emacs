/* declarations for strftime.c

   Copyright (C) 2002, 2004, 2008-2022 Free Software Foundation, Inc.

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

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Just like strftime, but with two more arguments:
   POSIX requires that strftime use the local timezone information.
   Use the timezone __TZ instead.  Use __NS as the number of
   nanoseconds in the %N directive.

   On error, set errno and return 0.  Otherwise, return the number of
   bytes generated (not counting the trailing NUL), preserving errno
   if the number is 0.  This errno behavior is in draft POSIX 202x
   plus some requested changes to POSIX.  */
size_t nstrftime (char *restrict, size_t, char const *, struct tm const *,
                  timezone_t __tz, int __ns);

#ifdef __cplusplus
}
#endif
