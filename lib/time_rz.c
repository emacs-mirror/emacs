/* Time zone functions such as tzalloc and localtime_rz

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

/* Although this module is not thread-safe, any races should be fairly
   rare and reasonably benign.  For complete thread-safety, use a C
   library with a working timezone_t type, so that this module is not
   needed.  */

#include <config.h>

/* Specification.  */
#include <time.h>

#include <errno.h>

#if HAVE_TZALLOC
# if NEED_TIMEZONE_NULL_SUPPORT          /* Android API level >= 35 */

struct tm *
localtime_rz (timezone_t tz, time_t const *t, struct tm *tm)
# undef localtime_rz
{
  if (!tz)
    return gmtime_r (t, tm);
  else
    return localtime_rz (tz, t, tm);
}

time_t
mktime_z (timezone_t tz, struct tm *tm)
# undef mktime_z
{
  if (!tz)
    return timegm (tm);
  else
    return mktime_z (tz, tm);
}
# endif

void
tzfree (timezone_t tz)
# undef tzfree
{
  int err = errno;
  tzfree (tz);
  errno = err;
}

#else

# include <stddef.h>
# include <stdlib.h>
# include <string.h>

# include "flexmember.h"
# include "idx.h"
# include "time-internal.h"

/* The approximate size to use for small allocation requests.  This is
   the largest "small" request for the GNU C library malloc.  */
enum { DEFAULT_MXFAST = 64 * sizeof (size_t) / 4 };

/* Minimum size of the ABBRS member of struct tm_zone.  ABBRS is larger
   only in the unlikely case where an abbreviation longer than this is
   used.  */
enum { ABBR_SIZE_MIN = DEFAULT_MXFAST - offsetof (struct tm_zone, abbrs) };

/* Copy to ABBRS the abbreviation at ABBR with size ABBR_SIZE (this
   includes its trailing null byte).  Append an extra null byte to
   mark the end of ABBRS.  */
static void
extend_abbrs (char *abbrs, char const *abbr, size_t abbr_size)
{
  memcpy (abbrs, abbr, abbr_size);
  abbrs[abbr_size] = '\0';
}

/* Return a newly allocated time zone for NAME, or NULL on failure.
   A null NAME stands for wall clock time (which is like unset TZ).  */
timezone_t
tzalloc (char const *name)
{
  size_t name_size = name ? strlen (name) + 1 : 0;
  size_t abbr_size = name_size < ABBR_SIZE_MIN ? ABBR_SIZE_MIN : name_size + 1;
  timezone_t tz = malloc (FLEXSIZEOF (struct tm_zone, abbrs, abbr_size));
  if (tz)
    {
      tz->next = NULL;
      tz->tz_is_set = !!name;
      tz->abbrs[0] = '\0';
      if (name)
        extend_abbrs (tz->abbrs, name, name_size);
    }
  return tz;
}

/* If HAVE_STRUCT_TM_TM_ZONE, save into TZ any nontrivial time zone
   abbreviation used by TM, and update *TM to contain the saved abbreviation.
   Return true if successful, false (setting errno) otherwise.  */
static bool
save_abbr (timezone_t tz, struct tm *tm)
{
# if HAVE_STRUCT_TM_TM_ZONE
  char const *zone = tm->tm_zone;
  char *zone_copy = (char *) "";

  /* No need to replace null zones, or zones within the struct tm.  */
  if (!zone || ((char *) tm <= zone && zone < (char *) (tm + 1)))
    return true;

  if (*zone)
    {
      zone_copy = tz->abbrs;

      while (!streq (zone_copy, zone))
        {
          if (! (*zone_copy || (zone_copy == tz->abbrs && tz->tz_is_set)))
            {
              idx_t zone_size = strlen (zone) + 1;
              if (zone_size < tz->abbrs + ABBR_SIZE_MIN - zone_copy)
                extend_abbrs (zone_copy, zone, zone_size);
              else
                {
                  tz = tz->next = tzalloc (zone);
                  if (!tz)
                    return false;
                  tz->tz_is_set = 0;
                  zone_copy = tz->abbrs;
                }
              break;
            }

          zone_copy += strlen (zone_copy) + 1;
          if (!*zone_copy && tz->next)
            {
              tz = tz->next;
              zone_copy = tz->abbrs;
            }
        }
    }

  /* Replace the zone name so that its lifetime matches that of TZ.  */
  tm->tm_zone = zone_copy;
# endif

  return true;
}

/* Free a time zone.  */
void
tzfree (timezone_t tz)
{
  if (tz != local_tz)
    while (tz)
      {
        timezone_t next = tz->next;
        free (tz);
        tz = next;
      }
}

/* Get and set the TZ environment variable.  These functions can be
   overridden by programs like Emacs that manage their own environment.  */

# ifndef getenv_TZ
static char *
getenv_TZ (void)
{
  return getenv ("TZ");
}
# endif

# ifndef setenv_TZ
static int
setenv_TZ (char const *tz)
{
  return tz ? setenv ("TZ", tz, 1) : unsetenv ("TZ");
}
# endif

/* Change the environment to match the specified timezone_t value.
   Return true if successful, false (setting errno) otherwise.  */
static bool
change_env (timezone_t tz)
{
  if (setenv_TZ (tz->tz_is_set ? tz->abbrs : NULL) != 0)
    return false;
  tzset ();
  return true;
}

/* Temporarily set the time zone to TZ, which must not be null.
   Return LOCAL_TZ if the time zone setting is already correct.
   Otherwise return a newly allocated time zone representing the old
   setting, or NULL (setting errno) on failure.  */
timezone_t
set_tz (timezone_t tz)
{
  char *env_tz = getenv_TZ ();
  if (env_tz
      ? tz->tz_is_set && streq (tz->abbrs, env_tz)
      : !tz->tz_is_set)
    return local_tz;
  else
    {
      timezone_t old_tz = tzalloc (env_tz);
      if (!old_tz)
        return old_tz;
      if (! change_env (tz))
        {
          tzfree (old_tz);
          return NULL;
        }
      return old_tz;
    }
}

/* Restore an old setting returned by set_tz.  It must not be null.
   Return true (preserving errno) if successful, false (setting errno)
   otherwise.  */
bool
revert_tz (timezone_t tz)
{
  if (tz == local_tz)
    return true;
  else
    {
      int saved_errno = errno;
      bool ok = change_env (tz);
      if (!ok)
        saved_errno = errno;
      tzfree (tz);
      errno = saved_errno;
      return ok;
    }
}

/* Use time zone TZ to compute localtime_r (T, TM).  */
struct tm *
localtime_rz (timezone_t tz, time_t const *t, struct tm *tm)
{
# ifdef HAVE_LOCALTIME_INFLOOP_BUG
  /* The -67768038400665599 comes from:
     https://lists.gnu.org/r/bug-gnulib/2017-07/msg00142.html
     On affected platforms the greatest POSIX-compatible time_t value
     that could return nonnull is 67768036191766798 (when
     TZ="XXX24:59:59" it resolves to the year 2**31 - 1 + 1900, on
     12-31 at 23:59:59), so test for that too while we're in the
     neighborhood.  */
  if (! (-67768038400665599 <= *t && *t <= 67768036191766798))
    {
      errno = EOVERFLOW;
      return NULL;
    }
# endif

  if (!tz)
    return gmtime_r (t, tm);
  else
    {
      timezone_t old_tz = set_tz (tz);
      if (old_tz)
        {
          bool abbr_saved = localtime_r (t, tm) && save_abbr (tz, tm);
          if (revert_tz (old_tz) && abbr_saved)
            return tm;
        }
      return NULL;
    }
}

/* Use time zone TZ to compute mktime (TM).  */
time_t
mktime_z (timezone_t tz, struct tm *tm)
{
  if (!tz)
    return timegm (tm);
  else
    {
      timezone_t old_tz = set_tz (tz);
      if (old_tz)
        {
          struct tm tm_1;
          tm_1.tm_sec = tm->tm_sec;
          tm_1.tm_min = tm->tm_min;
          tm_1.tm_hour = tm->tm_hour;
          tm_1.tm_mday = tm->tm_mday;
          tm_1.tm_mon = tm->tm_mon;
          tm_1.tm_year = tm->tm_year;
          tm_1.tm_yday = -1;
          tm_1.tm_isdst = tm->tm_isdst;
          time_t t = mktime (&tm_1);
          bool ok = 0 <= tm_1.tm_yday;
          ok = ok && save_abbr (tz, &tm_1);
          if (revert_tz (old_tz) && ok)
            {
              *tm = tm_1;
              return t;
            }
        }
      return -1;
    }
}

#endif
