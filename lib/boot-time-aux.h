/* Auxiliary functions for determining the time when the machine last booted.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

#define SIZEOF(a) (sizeof(a)/sizeof(a[0]))

#if defined __linux__ || defined __ANDROID__

/* Store the uptime counter, as managed by the Linux kernel, in *P_UPTIME.
   Return 0 upon success, -1 upon failure.  */
_GL_ATTRIBUTE_MAYBE_UNUSED
static int
get_linux_uptime (struct timespec *p_uptime)
{
  /* The clock_gettime facility returns the uptime with a resolution of 1 µsec.
     It is available with glibc >= 2.14, Android, or musl libc.
     In glibc < 2.17 it required linking with librt.  */
# if !defined __GLIBC__ || 2 < __GLIBC__ + (17 <= __GLIBC_MINOR__)
  if (clock_gettime (CLOCK_BOOTTIME, p_uptime) >= 0)
    return 0;
# endif

  /* /proc/uptime contains the uptime with a resolution of 0.01 sec.
     But it does not have read permissions on Android.  */
# if !defined __ANDROID__
  FILE *fp = fopen ("/proc/uptime", "re");
  if (fp != NULL)
    {
      char buf[32 + 1];
      size_t n = fread (buf, 1, sizeof (buf) - 1, fp);
      fclose (fp);
      if (n > 0)
        {
          buf[n] = '\0';
          /* buf now contains two values: the uptime and the idle time.  */
          time_t s = 0;
          char *p;
          for (p = buf; '0' <= *p && *p <= '9'; p++)
            s = 10 * s + (*p - '0');
          if (buf < p)
            {
              long ns = 0;
              if (*p++ == '.')
                for (int i = 0; i < 9; i++)
                  ns = 10 * ns + ('0' <= *p && *p <= '9' ? *p++ - '0' : 0);
              p_uptime->tv_sec = s;
              p_uptime->tv_nsec = ns;
              return 0;
            }
        }
    }
# endif

# if HAVE_DECL_SYSINFO /* not available in Android API < 9 */
  /* The sysinfo call returns the uptime with a resolution of 1 sec only.  */
  struct sysinfo info;
  if (sysinfo (&info) >= 0)
    {
      p_uptime->tv_sec = info.uptime;
      p_uptime->tv_nsec = 0;
      return 0;
    }
# endif

  return -1;
}

#endif

#if defined __linux__ && !defined __ANDROID__

static int
get_linux_boot_time_fallback (struct timespec *p_boot_time)
{
  /* On Alpine Linux, UTMP_FILE is not filled.  It is always empty.
     So, get the time stamp of a file that gets touched only during the
     boot process.  */

  const char * const boot_touched_files[] =
    {
      "/var/lib/systemd/random-seed", /* seen on distros with systemd */
      "/var/run/utmp",                /* seen on distros with OpenRC */
      "/var/lib/random-seed"          /* seen on older distros */
    };
  for (idx_t i = 0; i < SIZEOF (boot_touched_files); i++)
    {
      const char *filename = boot_touched_files[i];
      struct stat statbuf;
      if (stat (filename, &statbuf) >= 0)
        {
          *p_boot_time = get_stat_mtime (&statbuf);
          return 0;
        }
    }
  return -1;
}

/* The following approach is only usable as a fallback, because it is of
   the form
     boot_time = (time now) - (kernel's ktime_get_boottime[_ts64] ())
   and therefore produces wrong values after the date has been bumped in the
   running system, which happens frequently if the system is running in a
   virtual machine and this VM has been put into "saved" or "sleep" state
   and then resumed.  */
static int
get_linux_boot_time_final_fallback (struct timespec *p_boot_time)
{
  struct timespec uptime;
  if (get_linux_uptime (&uptime) >= 0)
    {
      struct timespec result;
# if !defined __GLIBC__ || 2 < __GLIBC__ + (16 <= __GLIBC_MINOR__)
      /* Better than:
      if (0 <= clock_gettime (CLOCK_REALTIME, &result))
         because timespec_get does not need -lrt in glibc 2.16.
      */
      if (! timespec_get (&result, TIME_UTC))
        return -1;
#  else
      /* Fall back on lower-res approach that does not need -lrt.
         This is good enough; on these hosts UPTIME is even lower-res.  */
      struct timeval tv;
      int r = gettimeofday (&tv, NULL);
      if (r < 0)
        return r;
      result.tv_sec = tv.tv_sec;
      result.tv_nsec = tv.tv_usec * 1000;
#  endif

      if (result.tv_nsec < uptime.tv_nsec)
        {
          result.tv_nsec += 1000000000;
          result.tv_sec -= 1;
        }
      result.tv_sec -= uptime.tv_sec;
      result.tv_nsec -= uptime.tv_nsec;
      *p_boot_time = result;
      return 0;
    }
  return -1;
}

#endif

#if defined __ANDROID__

static int
get_android_boot_time (struct timespec *p_boot_time)
{
  /* On Android, there is no /var, and normal processes don't have access
     to system files.  Therefore use the kernel's uptime counter, although
     it produces wrong values after the date has been bumped in the running
     system.  */
  struct timespec uptime;
  if (get_linux_uptime (&uptime) >= 0)
    {
      struct timespec result;
      if (clock_gettime (CLOCK_REALTIME, &result) >= 0)
        {
          if (result.tv_nsec < uptime.tv_nsec)
            {
              result.tv_nsec += 1000000000;
              result.tv_sec -= 1;
            }
          result.tv_sec -= uptime.tv_sec;
          result.tv_nsec -= uptime.tv_nsec;
          *p_boot_time = result;
          return 0;
        }
    }
  return -1;
}

#endif

#if defined __OpenBSD__

static int
get_openbsd_boot_time (struct timespec *p_boot_time)
{
  /* On OpenBSD, UTMP_FILE is not filled.  It contains only dummy entries.
     So, get the time stamp of a file that gets touched only during the
     boot process.  */
  const char * const boot_touched_files[] =
    {
      "/var/db/host.random",
      "/var/run/utmp"
    };
  for (idx_t i = 0; i < SIZEOF (boot_touched_files); i++)
    {
      const char *filename = boot_touched_files[i];
      struct stat statbuf;
      if (stat (filename, &statbuf) >= 0)
        {
          *p_boot_time = get_stat_mtime (&statbuf);
          return 0;
        }
    }
  return -1;
}

#endif

#if HAVE_SYS_SYSCTL_H && HAVE_SYSCTL \
    && defined CTL_KERN && defined KERN_BOOTTIME \
    && !defined __minix
/* macOS, FreeBSD, GNU/kFreeBSD, NetBSD, OpenBSD */
/* On Minix 3.3 this sysctl produces garbage results.  Therefore avoid it.  */

/* The following approach is only usable as a fallback, because it produces
   wrong values after the date has been bumped in the running system, which
   happens frequently if the system is running in a virtual machine and this
   VM has been put into "saved" or "sleep" state and then resumed.  */
static int
get_bsd_boot_time_final_fallback (struct timespec *p_boot_time)
{
  static int request[2] = { CTL_KERN, KERN_BOOTTIME };
  struct timeval result;
  size_t result_len = sizeof result;

  if (sysctl (request, 2, &result, &result_len, NULL, 0) >= 0)
    {
      p_boot_time->tv_sec = result.tv_sec;
      p_boot_time->tv_nsec = result.tv_usec * 1000;
      return 0;
    }
  return -1;
}

#endif

#if defined __HAIKU__

static int
get_haiku_boot_time (struct timespec *p_boot_time)
{
  /* On Haiku, /etc/utmp does not exist.  During boot,
       1. the current time is restored, but possibly with a wrong time zone,
          that is, with an offset of a few hours,
       2. some symlinks and files get created,
       3. the various devices are brought up, in particular the network device,
       4. the correct date and time is set,
       5. some more device nodes get created.
     The boot time can be retrieved by looking at a directory created during
     phase 5, such as /dev/input.  */
  const char * const boot_touched_file = "/dev/input";
  struct stat statbuf;
  if (stat (boot_touched_file, &statbuf) >= 0)
    {
      *p_boot_time = get_stat_mtime (&statbuf);
      return 0;
    }
  return -1;
}

#endif

#if HAVE_OS_H /* BeOS, Haiku */

/* The following approach is only usable as a fallback, because it produces
   wrong values after the date has been bumped in the running system, which
   happens frequently if the system is running in a virtual machine and this
   VM has been put into "saved" or "sleep" state and then resumed.  */
static int
get_haiku_boot_time_final_fallback (struct timespec *p_boot_time)
{
  system_info si;

  get_system_info (&si);
  p_boot_time->tv_sec = si.boot_time / 1000000;
  p_boot_time->tv_nsec = (si.boot_time % 1000000) * 1000;
  return 0;
}

#endif

#if defined __CYGWIN__ || defined _WIN32

static int
get_windows_boot_time (struct timespec *p_boot_time)
{
  /* On Cygwin, /var/run/utmp is empty.
     On native Windows, <utmpx.h> and <utmp.h> don't exist.
     Instead, on Windows, the boot time can be retrieved by looking at the
     time stamp of a file that (normally) gets touched only during the boot
     process, namely C:\pagefile.sys.  */
  const char * const boot_touched_file =
    #if defined __CYGWIN__ && !defined _WIN32
    "/cygdrive/c/pagefile.sys"
    #else
    "C:\\pagefile.sys"
    #endif
    ;
  struct stat statbuf;
  if (stat (boot_touched_file, &statbuf) >= 0)
    {
      *p_boot_time = get_stat_mtime (&statbuf);
      return 0;
    }
  return -1;
}

#endif
