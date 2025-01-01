/* Time internal interface

   Copyright 2015-2025 Free Software Foundation, Inc.

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

/* Written by Paul Eggert.  */

/* This file is for Gnulib internal use only.
   Applications should not use it.  */

/* A time zone rule.  */
struct tm_zone
{
  /* More abbreviations, should they be needed.  Their TZ_IS_SET
     members are zero.  */
  struct tm_zone *next;

  /* If nonzero, the rule represents the TZ environment variable set
     to the first "abbreviation" (this may be the empty string).
     Otherwise, it represents an unset TZ.  */
  char tz_is_set;

  /* A sequence of null-terminated strings packed next to each other.
     The strings are followed by an extra null byte.  If TZ_IS_SET,
     there must be at least one string and the first string (which is
     actually a TZ environment value) may be empty.  Otherwise all
     strings must be nonempty.

     Abbreviations are stored here even on platforms with tm_zone, because
     otherwise tm_zone values would be dead after changing TZ and calling
     tzset.  Abbreviations never move once allocated, and are live
     until tzfree is called.  */
  char abbrs[FLEXIBLE_ARRAY_MEMBER];
};

timezone_t set_tz (timezone_t);
bool revert_tz (timezone_t);

/* Magic cookie timezone_t value, for local time.  It differs from
   NULL and from all other timezone_t values.  Only the address
   matters; the pointer is never dereferenced.  */
#define local_tz ((timezone_t) 1)
