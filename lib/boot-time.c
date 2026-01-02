/* Determine the time when the machine last booted.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, either version 3 of the License,
   or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Bruno Haible <bruno@clisp.org>.  */

#include <config.h>

/* Specification.  */
#include "boot-time.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined __linux__ || defined __ANDROID__
# include <sys/sysinfo.h>
# include <time.h>
#endif

#if HAVE_SYS_SYSCTL_H && !(defined __GLIBC__ && defined __linux__) && !defined __minix
# if HAVE_SYS_PARAM_H
#  include <sys/param.h>
# endif
# include <sys/sysctl.h>
#endif

#if HAVE_OS_H
# include <OS.h>
#endif

#if defined _WIN32 && ! defined __CYGWIN__
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
# include <sys/time.h>
#endif

#include "idx.h"
#include "readutmp.h"
#include "stat-time.h"

/* Each of the FILE streams in this file is only used in a single thread.  */
#include "unlocked-io.h"

/* Some helper functions.  */
#include "boot-time-aux.h"

/* The following macros describe the 'struct UTMP_STRUCT_NAME',
   *not* 'struct gl_utmp'.  */
#undef UT_USER

/* Accessor macro for the member named ut_user or ut_name.  */
#if (HAVE_UTMPX_H ? HAVE_STRUCT_UTMPX_UT_NAME \
     : HAVE_UTMP_H && HAVE_STRUCT_UTMP_UT_NAME)
# define UT_USER(UT) ((UT)->ut_name)
#else
# define UT_USER(UT) ((UT)->ut_user)
#endif

#if !HAVE_UTMPX_H && HAVE_UTMP_H && defined UTMP_NAME_FUNCTION
# if !HAVE_DECL_ENDUTENT /* Android */
void endutent (void);
# endif
#endif

#if defined __linux__ || HAVE_UTMPX_H || HAVE_UTMP_H || defined __CYGWIN__ || defined _WIN32

static int
get_boot_time_uncached (struct timespec *p_boot_time)
{
  struct timespec found_boot_time = {0};

# if (HAVE_UTMPX_H ? HAVE_STRUCT_UTMPX_UT_TYPE : HAVE_STRUCT_UTMP_UT_TYPE)

  /* Try to find the boot time in the /var/run/utmp file.  */

#  if defined UTMP_NAME_FUNCTION /* glibc, musl, macOS, FreeBSD, NetBSD, Minix, AIX, Solaris, Cygwin, Android */

  /* Ignore the return value for now.
     Solaris' utmpname returns 1 upon success -- which is contrary
     to what the GNU libc version does.  In addition, older GNU libc
     versions are actually void.   */
  UTMP_NAME_FUNCTION ((char *) UTMP_FILE);

  SET_UTMP_ENT ();

#   if (defined __linux__ && !defined __ANDROID__) || defined __minix
  /* Timestamp of the "runlevel" entry, if any.  */
  struct timespec runlevel_ts = {0};
#   endif

  void const *entry;

  while ((entry = GET_UTMP_ENT ()) != NULL)
    {
      struct UTMP_STRUCT_NAME const *ut = (struct UTMP_STRUCT_NAME const *) entry;

      struct timespec ts =
        #if (HAVE_UTMPX_H ? 1 : HAVE_STRUCT_UTMP_UT_TV)
        { .tv_sec = ut->ut_tv.tv_sec, .tv_nsec = ut->ut_tv.tv_usec * 1000 };
        #else
        { .tv_sec = ut->ut_time, .tv_nsec = 0 };
        #endif

      if (ut->ut_type == BOOT_TIME)
        found_boot_time = ts;

#   if defined __linux__ && !defined __ANDROID__
      if (memeq (UT_USER (ut), "runlevel", strlen ("runlevel") + 1)
          && memeq (ut->ut_line, "~", strlen ("~") + 1))
        runlevel_ts = ts;
#   endif
#   if defined __minix
      if (UT_USER (ut)[0] == '\0'
          && memeq (ut->ut_line, "run-level ", strlen ("run-level ")))
        runlevel_ts = ts;
#   endif
    }

  END_UTMP_ENT ();

#   if defined __linux__ && !defined __ANDROID__
  /* On Raspbian, which runs on hardware without a real-time clock, during boot,
       1. the clock gets set to 1970-01-01 00:00:00,
       2. an entry gets written into /var/run/utmp, with ut_type = BOOT_TIME,
          ut_user = "reboot", ut_line = "~", time = 1970-01-01 00:00:05 or so,
       3. the clock gets set to a correct value through NTP,
       4. an entry gets written into /var/run/utmp, with
          ut_user = "runlevel", ut_line = "~", time = correct value.
     In this case, get the time from the "runlevel" entry.  */

  /* Workaround for Raspbian:  */
  if (found_boot_time.tv_sec <= 60 && runlevel_ts.tv_sec != 0)
    found_boot_time = runlevel_ts;
  if (found_boot_time.tv_sec == 0)
    {
      /* Workaround for Alpine Linux:  */
      get_linux_boot_time_fallback (&found_boot_time);
    }
#   endif

#   if defined __ANDROID__
  if (found_boot_time.tv_sec == 0)
    {
      /* Workaround for Android:  */
      get_android_boot_time (&found_boot_time);
    }
#   endif

#   if defined __minix
  /* On Minix, during boot,
       1. an entry gets written into /var/run/utmp, with ut_type = BOOT_TIME,
          ut_user = "", ut_line = "system boot", time = 1970-01-01 00:00:00,
       2. an entry gets written into /var/run/utmp, with
          ut_user = "", ut_line = "run-level m", time = correct value.
     In this case, copy the time from the "run-level m" entry to the
     "system boot" entry.  */
  if (found_boot_time.tv_sec <= 60 && runlevel_ts.tv_sec != 0)
    found_boot_time = runlevel_ts;
#   endif

#  else /* HP-UX, Haiku */

  FILE *f = fopen (UTMP_FILE, "re");

  if (f != NULL)
    {
      for (;;)
        {
          struct UTMP_STRUCT_NAME ut;

          if (fread (&ut, sizeof ut, 1, f) == 0)
            break;

          struct timespec ts =
            #if (HAVE_UTMPX_H ? 1 : HAVE_STRUCT_UTMP_UT_TV)
            { .tv_sec = ut.ut_tv.tv_sec, .tv_nsec = ut.ut_tv.tv_usec * 1000 };
            #else
            { .tv_sec = ut.ut_time, .tv_nsec = 0 };
            #endif

          if (ut.ut_type == BOOT_TIME)
            found_boot_time = ts;
        }

      fclose (f);
    }

#  endif

#  if defined __linux__ && !defined __ANDROID__
  if (found_boot_time.tv_sec == 0)
    {
      get_linux_boot_time_final_fallback (&found_boot_time);
    }
#  endif

# else /* Adélie Linux, old FreeBSD, OpenBSD, native Windows */

#  if defined __linux__ && !defined __ANDROID__
  /* Workaround for Adélie Linux:  */
  get_linux_boot_time_fallback (&found_boot_time);
  if (found_boot_time.tv_sec == 0)
    get_linux_boot_time_final_fallback (&found_boot_time);
#  endif

#  if defined __OpenBSD__
  /* Workaround for OpenBSD:  */
  get_openbsd_boot_time (&found_boot_time);
#  endif

# endif

# if HAVE_SYS_SYSCTL_H && HAVE_SYSCTL \
     && defined CTL_KERN && defined KERN_BOOTTIME \
     && !defined __minix
  if (found_boot_time.tv_sec == 0)
    {
      get_bsd_boot_time_final_fallback (&found_boot_time);
    }
# endif

# if defined __HAIKU__
  if (found_boot_time.tv_sec == 0)
    {
      get_haiku_boot_time (&found_boot_time);
    }
# endif

# if HAVE_OS_H
  if (found_boot_time.tv_sec == 0)
    {
      get_haiku_boot_time_final_fallback (&found_boot_time);
    }
# endif

# if defined __CYGWIN__ || defined _WIN32
  if (found_boot_time.tv_sec == 0)
    {
      /* Workaround for Windows:  */
      get_windows_boot_time (&found_boot_time);
#  ifndef __CYGWIN__
      if (found_boot_time.tv_sec == 0)
        get_windows_boot_time_fallback (&found_boot_time);
#  endif
    }
# endif

  if (found_boot_time.tv_sec != 0)
    {
      *p_boot_time = found_boot_time;
      return 0;
    }
  else
    return -1;
}

int
get_boot_time (struct timespec *p_boot_time)
{
  /* Cache the result from get_boot_time_uncached.  */
  static int volatile cached_result = -1;
  static struct timespec volatile cached_boot_time;

  if (cached_result < 0)
    {
      struct timespec boot_time;
      int result = get_boot_time_uncached (&boot_time);
      cached_boot_time = boot_time;
      cached_result = result;
    }

  if (cached_result == 0)
    {
      *p_boot_time = cached_boot_time;
      return 0;
    }
  else
    return -1;
}

#else

int
get_boot_time (struct timespec *p_boot_time)
{
  return -1;
}

#endif
