/* Sanitizing the TZ environment variable on native Windows.
   Copyright (C) 2017-2026 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Bruno Haible.  */

#ifndef _TZSANITIZE_H
#define _TZSANITIZE_H

#ifdef __cplusplus
extern "C" {
#endif

#if defined _WIN32 && !defined __CYGWIN__

/* Rectify the value of the environment variable TZ.
   There are four possible kinds of such values:
     - Traditional US time zone names, e.g. "PST8PDT".  Syntax: see
       <https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/tzset>
     - Time zone names based on geography, that contain one or more
       slashes, e.g. "Europe/Moscow".
     - Time zone names based on geography, without slashes, e.g.
       "Singapore".
     - Time zone names that contain explicit DST rules.  Syntax: see
       <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html#tag_08_03>
   The Microsoft CRT understands only the first kind.  It produces incorrect
   results if the value of TZ is of the other kinds.
   But in a Cygwin environment, /etc/profile.d/tzset.sh sets TZ to a value
   of the second kind for most geographies, or of the first kind in a few
   other geographies.  If it is of the second kind, neutralize it.  For the
   Microsoft CRT, an absent or empty TZ means the time zone that the user
   has set in the Windows Control Panel.
   If the value of TZ is of the third or fourth kind -- Cygwin programs
   understand these syntaxes as well --, it does not matter whether we
   neutralize it or not, since these values occur only when a Cygwin user
   has set TZ explicitly; this case is 1. rare and 2. under the user's
   responsibility.  */

/* Applications need to make sure that tzsanitize() gets invoked (at least
   once) *before* the first invocation of __tzset() in the Microsoft runtime
   library.  This is because
     - __tzset looks at the value of the environment variable "TZ".
     - The first invocation of __tzset sets some cached variables.
       Further invocations of __tzset do nothing.

   The gnulib overrides of
     localtime, mktime, tzset, strftime, wcsftime, ctime
   do call tzsanitize(); therefore no explicit tzsanitize() invocations are
   needed in simple programs.  But some other functions do call __tzset()
   implicitly:
     - In the Microsoft runtime library:
       stat = _stat64, fstat = _fstat64.
       MSVCRT: _stat64, _fstat64 -> __loctotime64_t -> __tzset.
       UCRT: _fstat64 -> common_fstat -> common_stat_handle_file_opened
             -> convert_large_integer_time_to_time_t
             -> loctotime -> common_loctotime_t -> __tzset.
     - In GNU libintl:
       [d]gettext -> dcigettext -> _nl_find_domain -> _nl_load_domain -> fstat.

   Therefore programs that are linked to such libraries need to call
   tzsanitize() early during their initialization.  */

extern void tzsanitize (void);

#endif

#ifdef __cplusplus
}
#endif

#endif /* _TZSANITIZE_H */
