/* Detect the number of processors.

   Copyright (C) 2009-2025 Free Software Foundation, Inc.

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

/* Written by Glen Lenker and Bruno Haible.  */

#include <config.h>
#include "nproc.h"

#include <errno.h>
#include <limits.h>
#if HAVE_SETMNTENT
# include <mntent.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#if HAVE_PTHREAD_GETAFFINITY_NP && 0
# include <pthread.h>
# include <sched.h>
#endif
#if HAVE_SCHED_GETAFFINITY_LIKE_GLIBC || HAVE_SCHED_GETAFFINITY_NP
# include <sched.h>
#endif

#include <sys/types.h>

#if HAVE_SYS_PSTAT_H
# include <sys/pstat.h>
#endif

#if HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif

#if HAVE_SYS_SYSCTL_H && !(defined __GLIBC__ && defined __linux__)
# include <sys/sysctl.h>
#endif

#if defined _WIN32 && ! defined __CYGWIN__
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
#endif

#include "c-ctype.h"

#include "minmax.h"

#define ARRAY_SIZE(a) (sizeof (a) / sizeof ((a)[0]))

#define NPROC_MINIMUM 1

/* Return the number of processors available to the current process, based
   on a modern system call that returns the "affinity" between the current
   process and each CPU.  Return 0 if unknown or if such a system call does
   not exist.  */
static unsigned long
num_processors_via_affinity_mask (void)
{
  /* glibc >= 2.3.3 with NPTL and NetBSD 5 have pthread_getaffinity_np,
     but with different APIs.  Also it requires linking with -lpthread.
     Therefore this code is not enabled.
     glibc >= 2.3.4 has sched_getaffinity whereas NetBSD 5 has
     sched_getaffinity_np.  */
#if HAVE_PTHREAD_GETAFFINITY_NP && defined __GLIBC__ && 0
  {
    cpu_set_t set;

    if (pthread_getaffinity_np (pthread_self (), sizeof (set), &set) == 0)
      {
        unsigned long count;

# ifdef CPU_COUNT
        /* glibc >= 2.6 has the CPU_COUNT macro.  */
        count = CPU_COUNT (&set);
# else
        size_t i;

        count = 0;
        for (i = 0; i < CPU_SETSIZE; i++)
          if (CPU_ISSET (i, &set))
            count++;
# endif
        if (count > 0)
          return count;
      }
  }
#elif HAVE_PTHREAD_GETAFFINITY_NP && defined __NetBSD__ && 0
  {
    cpuset_t *set;

    set = cpuset_create ();
    if (set != NULL)
      {
        unsigned long count = 0;

        if (pthread_getaffinity_np (pthread_self (), cpuset_size (set), set)
            == 0)
          {
            cpuid_t i;

            for (i = 0;; i++)
              {
                int ret = cpuset_isset (i, set);
                if (ret < 0)
                  break;
                if (ret > 0)
                  count++;
              }
          }
        cpuset_destroy (set);
        if (count > 0)
          return count;
      }
  }
#elif HAVE_SCHED_GETAFFINITY_LIKE_GLIBC /* glibc >= 2.3.4 */
  /* There are two ways to use the sched_getaffinity() function:
       - With a statically-sized cpu_set_t.
       - With a dynamically-sized cpu_set_t.
     Documentation:
     <https://www.kernel.org/doc/man-pages/online/pages/man2/sched_getaffinity.2.html>
     <https://www.kernel.org/doc/man-pages/online/pages/man3/CPU_SET.3.html>
     The second way has the advantage that it works on systems with more than
     1024 CPUs.  The first way has the advantage that it works also when memory
     is tight.  */
# if defined CPU_ALLOC_SIZE /* glibc >= 2.6 */
  {
    unsigned int alloc_count = 1024;
    for (;;)
      {
        cpu_set_t *set = CPU_ALLOC (alloc_count);
        if (set == NULL)
          /* Out of memory.  */
          break;
        unsigned int size = CPU_ALLOC_SIZE (alloc_count);
        if (sched_getaffinity (0, size, set) == 0)
          {
            unsigned int count = CPU_COUNT_S (size, set);
            CPU_FREE (set);
            return count;
          }
        if (errno != EINVAL)
          {
            /* Some other error.  */
            CPU_FREE (set);
            return 0;
          }
        CPU_FREE (set);
        /* Retry with some larger cpu_set_t.  */
        alloc_count *= 2;
        if (alloc_count == 0)
          /* Integer overflow.  Avoid an endless loop.  */
          return 0;
      }
  }
# endif
  {
    cpu_set_t set;

    if (sched_getaffinity (0, sizeof (set), &set) == 0)
      {
        unsigned long count;

# ifdef CPU_COUNT
        /* glibc >= 2.6 has the CPU_COUNT macro.  */
        count = CPU_COUNT (&set);
# else
        size_t i;

        count = 0;
        for (i = 0; i < CPU_SETSIZE; i++)
          if (CPU_ISSET (i, &set))
            count++;
# endif
        if (count > 0)
          return count;
      }
  }
#elif HAVE_SCHED_GETAFFINITY_NP /* NetBSD >= 5 */
  {
    cpuset_t *set;

    set = cpuset_create ();
    if (set != NULL)
      {
        unsigned long count = 0;

        if (sched_getaffinity_np (getpid (), cpuset_size (set), set) == 0)
          {
            cpuid_t i;

            for (i = 0;; i++)
              {
                int ret = cpuset_isset (i, set);
                if (ret < 0)
                  break;
                if (ret > 0)
                  count++;
              }
          }
        cpuset_destroy (set);
        if (count > 0)
          return count;
      }
  }
#endif

#if defined _WIN32 && ! defined __CYGWIN__
  { /* This works on native Windows platforms.  */
    DWORD_PTR process_mask;
    DWORD_PTR system_mask;

    if (GetProcessAffinityMask (GetCurrentProcess (),
                                &process_mask, &system_mask))
      {
        DWORD_PTR mask = process_mask;
        unsigned long count = 0;

        for (; mask != 0; mask = mask >> 1)
          if (mask & 1)
            count++;
        if (count > 0)
          return count;
      }
  }
#endif

  return 0;
}


/* Return the total number of processors.  Here QUERY must be one of
   NPROC_ALL, NPROC_CURRENT.  The result is guaranteed to be at least 1.  */
static unsigned long int
num_processors_available (enum nproc_query query)
{
  /* On systems with a modern affinity mask system call, we have
         sysconf (_SC_NPROCESSORS_CONF)
            >= sysconf (_SC_NPROCESSORS_ONLN)
               >= num_processors_via_affinity_mask ()
     The first number is the number of CPUs configured in the system.
     The second number is the number of CPUs available to the scheduler.
     The third number is the number of CPUs available to the current process.

     Note! On Linux systems with glibc, the first and second number come from
     the /sys and /proc file systems (see
     glibc/sysdeps/unix/sysv/linux/getsysstats.c).
     In some situations these file systems are not mounted, and the sysconf call
     returns 1 or 2 (<https://sourceware.org/PR21542>),
     which does not reflect the reality.  */

  if (query == NPROC_CURRENT)
    {
      /* Try the modern affinity mask system call.  */
      {
        unsigned long nprocs = num_processors_via_affinity_mask ();

        if (nprocs > 0)
          return nprocs;
      }

#if defined _SC_NPROCESSORS_ONLN
      { /* This works on glibc, Mac OS X 10.5, FreeBSD, AIX, Solaris, Cygwin,
           Haiku.  */
        long int nprocs = sysconf (_SC_NPROCESSORS_ONLN);
        if (nprocs > 0)
          return nprocs;
      }
#endif
    }
  else /* query == NPROC_ALL */
    {
#if defined _SC_NPROCESSORS_CONF
      { /* This works on glibc, Mac OS X 10.5, FreeBSD, AIX, Solaris, Cygwin,
           Haiku.  */
        long int nprocs = sysconf (_SC_NPROCESSORS_CONF);

# if __GLIBC__ >= 2 && defined __linux__
        /* On Linux systems with glibc, this information comes from the /sys and
           /proc file systems (see glibc/sysdeps/unix/sysv/linux/getsysstats.c).
           In some situations these file systems are not mounted, and the
           sysconf call returns 1 or 2.  But we wish to guarantee that
           num_processors (NPROC_ALL) >= num_processors (NPROC_CURRENT).  */
        if (nprocs == 1 || nprocs == 2)
          {
            unsigned long nprocs_current = num_processors_via_affinity_mask ();

            if (/* nprocs_current > 0 && */ nprocs_current > nprocs)
              nprocs = nprocs_current;
          }
# endif

        if (nprocs > 0)
          return nprocs;
      }
#endif
    }

#if HAVE_PSTAT_GETDYNAMIC
  { /* This works on HP-UX.  */
    struct pst_dynamic psd;
    if (pstat_getdynamic (&psd, sizeof psd, 1, 0) >= 0)
      {
        /* The field psd_proc_cnt contains the number of active processors.
           In newer releases of HP-UX 11, the field psd_max_proc_cnt includes
           deactivated processors.  */
        if (query == NPROC_CURRENT)
          {
            if (psd.psd_proc_cnt > 0)
              return psd.psd_proc_cnt;
          }
        else
          {
            if (psd.psd_max_proc_cnt > 0)
              return psd.psd_max_proc_cnt;
          }
      }
  }
#endif

  /* Finally, as fallback, use the APIs that don't distinguish between
     NPROC_CURRENT and NPROC_ALL.  */

#if HAVE_SYSCTL && !(defined __GLIBC__ && defined __linux__) && defined HW_NCPU
  { /* This works on macOS, FreeBSD, NetBSD, OpenBSD.
       macOS 10.14 does not allow mib to be const.  */
    int nprocs;
    size_t len = sizeof (nprocs);
    static int mib[][2] = {
# ifdef HW_NCPUONLINE
      { CTL_HW, HW_NCPUONLINE },
# endif
      { CTL_HW, HW_NCPU }
    };
    for (int i = 0; i < ARRAY_SIZE (mib); i++)
      {
        if (sysctl (mib[i], ARRAY_SIZE (mib[i]), &nprocs, &len, NULL, 0) == 0
            && len == sizeof (nprocs)
            && 0 < nprocs)
          return nprocs;
      }
  }
#endif

#if defined _WIN32 && ! defined __CYGWIN__
  { /* This works on native Windows platforms.  */
    SYSTEM_INFO system_info;
    GetSystemInfo (&system_info);
    if (0 < system_info.dwNumberOfProcessors)
      return system_info.dwNumberOfProcessors;
  }
#endif

  return NPROC_MINIMUM;
}

#if defined __linux__ || defined __ANDROID__
/* Identify the cgroup2 mount point,
   initially at the usual location for efficiency,
   resorting to searching mount points otherwise.
   Return NULL if the mount point is not found.
   The returned string can be freed.  */
static char *
cgroup2_mount (void)
{
  FILE *fp;
  char *ret = NULL;

  /* Check the usual location first.  */
  if (access ("/sys/fs/cgroup/cgroup.controllers", F_OK) == 0)
    return strdup ("/sys/fs/cgroup");

#if HAVE_SETMNTENT
  /* Otherwise look for the mount point.  */
  struct mntent *mnt;
  if (! (fp = setmntent ("/proc/mounts", "r")))
    return NULL;
  while ((mnt = getmntent (fp)) != NULL)
    {
      if (streq (mnt->mnt_type, "cgroup2"))
        {
          ret = strdup (mnt->mnt_dir);
          break;
        }
    }
  endmntent (fp);
#endif

  return ret;
}

/* Return the minimum configured cgroupv2 CPU quota for the current process.
   Return ULONG_MAX if quota can't be read.
   Returned value will be >= 1.  */
static unsigned long int
get_cgroup2_cpu_quota (void)
{
  unsigned long int cpu_quota = ULONG_MAX;
  FILE *fp;

  fp = fopen ("/proc/self/cgroup", "r");
  if (! fp)
    return cpu_quota;

  /* Get our cgroupv2 (unififed) hierarchy.  */
  char *cgroup = NULL;
  char *cgroup_str = NULL;
  size_t cgroup_size = 0;
  ssize_t read;
  while ((read = getline (&cgroup_str, &cgroup_size, fp)) != -1)
    {
      if (strncmp (cgroup_str, "0::/", 4) == 0)
        {
          char *end = cgroup_str + read - 1;
          if (*end == '\n')
            *end = '\0';
          cgroup = cgroup_str + 3;
          break;
        }
    }
  fclose (fp);

  char *mount = NULL;
  if (cgroup && ! (mount = cgroup2_mount ()))
    cgroup = NULL;

  /* Find the lowest quota in the hierarchy.  */
  char *quota_str = NULL;
  size_t quota_size = 0;
  while (cgroup && *cgroup)
    {
      /* Walk back up the nested cgroup hierarchy
         to find the lowest cpu quota as defined in a cpu.max file.
         Note this file may not be present if the cpu controller
         is not enabled for that part of the hierarchy.  */

      char cpu_max_file[PATH_MAX];
      snprintf (cpu_max_file, sizeof (cpu_max_file),
                "%s%s/cpu.max", mount, cgroup);

      if ((fp = fopen (cpu_max_file, "r"))
          && getline (&quota_str, &quota_size, fp) != -1
          && strncmp (quota_str, "max", 3) != 0)
        {
          long quota, period;
          if (sscanf (quota_str, "%ld %ld", &quota, &period) == 2 && period)
            {
              double ncpus = (double)quota / period;
              if (cpu_quota == ULONG_MAX || ncpus < cpu_quota)
                {
                  cpu_quota = MAX (1, (long)(ncpus + 0.5));
                  /* nproc will return 1 minimum, so no point going lower */
                  if (cpu_quota == 1)
                    *cgroup = '\0';
                }
            }
        }

      if (fp)
        fclose (fp);

      char *last_sep = strrchr (cgroup, '/');
      if (! last_sep)
        break;
      if (last_sep == cgroup && *(cgroup + 1))
        *(cgroup + 1) = '\0';  /* Iterate on "/" also.  */
      else
        *last_sep = '\0';
    }

  free (quota_str);
  free (mount);
  free (cgroup_str);

  return cpu_quota;
}
#endif


/* Return the cgroupv2 CPU quota if the current scheduler honors it.
   Otherwise return ULONG_MAX.
   Returned value will be >= 1.  */
static unsigned long int
cpu_quota (void)
{
  unsigned long int quota = ULONG_MAX;

#if defined __linux__ || defined __ANDROID__
# if HAVE_SCHED_GETAFFINITY_LIKE_GLIBC && defined SCHED_DEADLINE
  /* We've a new enough sched.h  */
  switch (sched_getscheduler (0))
    {
      case -1:
      case SCHED_FIFO:
      case SCHED_RR:
      case SCHED_DEADLINE:
        quota = ULONG_MAX;
        break;
      default:
        quota = get_cgroup2_cpu_quota ();
        break;
    }
# endif
#endif

  return quota;
}

/* Parse OMP environment variables without dependence on OMP.
   Return 0 for invalid values.  */
static unsigned long int
parse_omp_threads (char const* threads)
{
  unsigned long int ret = 0;

  if (threads == NULL)
    return ret;

  /* The OpenMP spec says that the value assigned to the environment variables
     "may have leading and trailing white space".  */
  while (*threads != '\0' && c_isspace (*threads))
    threads++;

  /* Convert it from positive decimal to 'unsigned long'.  */
  if (c_isdigit (*threads))
    {
      char *endptr;
      unsigned long int value = strtoul (threads, &endptr, 10);
      while (*endptr != '\0' && c_isspace (*endptr))
        endptr++;
      if (*endptr == '\0')
        return value;
      /* Also accept the first value in a nesting level,
         since we can't determine the nesting level from env vars.  */
      else if (*endptr == ',')
        return value;
    }

  return ret;
}

unsigned long int
num_processors (enum nproc_query query)
{
  unsigned long int nproc_limit = ULONG_MAX;

  /* Honor the OpenMP environment variables, recognized also by all
     programs that are based on OpenMP.  */
  if (query == NPROC_CURRENT_OVERRIDABLE)
    {
      unsigned long int omp_env_threads, omp_env_limit;
      omp_env_threads = parse_omp_threads (getenv ("OMP_NUM_THREADS"));
      omp_env_limit = parse_omp_threads (getenv ("OMP_THREAD_LIMIT"));
      if (! omp_env_limit)
        omp_env_limit = ULONG_MAX;

      if (omp_env_threads)
        return MIN (omp_env_threads, omp_env_limit);

      nproc_limit = omp_env_limit;
      query = NPROC_CURRENT;
    }

  /* Honor any CPU quotas.  */
  if (query == NPROC_CURRENT && nproc_limit > NPROC_MINIMUM)
    {
      unsigned long int quota = cpu_quota ();
      nproc_limit = MIN (quota, nproc_limit);
    }

  if (nproc_limit > NPROC_MINIMUM)
    {
      unsigned long nprocs = num_processors_available (query);
      nproc_limit = MIN (nprocs, nproc_limit);
    }

  return nproc_limit;
}
