/* declarations for strftime.c

   Copyright (C) 2002, 2004, 2008-2025 Free Software Foundation, Inc.

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

#include <stddef.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Format the broken-down time *__TP, with additional __NS nanoseconds,
   into the buffer __S of size __MAXSIZE, according to the rules of the
   LC_TIME category of the current locale.

   Use the time zone __TZ.
   If *__TP represents local time, __TZ should be set to
     tzalloc (getenv ("TZ")).
   If *__TP represents universal time (a.k.a. GMT), __TZ should be set to
     (timezone_t) 0.

   The format string __FORMAT, including GNU extensions, is described in
   the GNU libc's strftime() documentation:
   <https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html>
   Additionally, the following conversion is supported:
     %N   The number of nanoseconds, passed as __NS argument.
   Here's a summary of the available conversions (= format directives):
     literal characters      %n %t %%
     date:
       century               %C
       year                  %Y %y
       week-based year       %G %g
       month (in year)       %m %B %b %h
       week in year          %U %W %V
       day in year           %j
       day (in month)        %d %e
       day in week           %u %w %A %a
       year, month, day      %x %F %D
     time:
       half-day              %p %P
       hour                  %H %k %I %l
       minute (in hour)      %M
       hour, minute          %R
       second (in minute)    %S
       hour, minute, second  %r %T %X
       second (since epoch)  %s
     date and time:          %c
     time zone:              %z %Z
     nanosecond              %N
   In locales with non-Gregorian calendars, the following conversions don't
   apply in the expected way:
     date:
       century               %C
       year                  %y
       week-based year       %G %g
       week in year          %U %W %V
       day in year           %j
       year, month, day      %D

   Store the result, as a string with a trailing NUL character, at the
   beginning of the array __S[0..__MAXSIZE-1] and return the length of
   that string, not counting the trailing NUL.
   If unsuccessful, possibly change the array __S, set errno, and return -1;
   errno == ERANGE means the string didn't fit.

   As a glibc extension if __S is null, do not store anything, and
   return the value that would have been returned had __S been non-null.

   A __MAXSIZE greater than PTRDIFF_MAX is silently treated as if
   it were PTRDIFF_MAX, so that the caller can safely add 1 to
   any return value without overflow.

   This function is like strftime, but with two more arguments:
     * __TZ instead of the local timezone information,
     * __NS as the number of nanoseconds in the %N directive,
   and on success it does not preserve errno,
   and on failure it returns -1 not 0.
 */
ptrdiff_t nstrftime (char *restrict __s, size_t __maxsize,
                     char const *__format,
                     struct tm const *__tp, timezone_t __tz, int __ns);

/* Like nstrftime, except that it uses the "C" locale instead of the
   current locale.  */
ptrdiff_t c_nstrftime (char *restrict __s, size_t __maxsize,
                       char const *__format,
                       struct tm const *__tp, timezone_t __tz, int __ns);

#ifdef __cplusplus
}
#endif
