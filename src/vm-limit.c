/* Functions for memory limit warnings.
   Copyright (C) 1990, 1992, 2001-2018 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>
#include <unistd.h> /* for 'environ', on AIX */
#include "lisp.h"

#ifdef MSDOS
#include "dosfns.h"
extern int etext;
#endif

/* Some systems need this before <sys/resource.h>.  */
#include <sys/types.h>

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#else
# if HAVE_SYS_VLIMIT_H
#  include <sys/vlimit.h>	/* Obsolete, says glibc */
# endif
#endif

/* Start of data.  It is OK if this is approximate; it's used only as
   a heuristic.  */
#ifdef DATA_START
# define data_start ((char *) DATA_START)
#else
extern char data_start[];
# ifndef HAVE_DATA_START
/* Initialize to nonzero, so that it's put into data and not bss.
   Link this file's object code first, so that this symbol is near the
   start of data.  */
char data_start[1] = { 1 };
# endif
#endif

#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif
#ifndef DOUG_LEA_MALLOC
# ifndef __MALLOC_HOOK_VOLATILE
#  define __MALLOC_HOOK_VOLATILE volatile
# endif
extern void *(*__morecore) (ptrdiff_t);
extern void (*__MALLOC_HOOK_VOLATILE __after_morecore_hook) (void);
#endif

/* From ralloc.c.  */
#ifdef REL_ALLOC
extern void *(*real_morecore) (ptrdiff_t);
#endif

/*
  Level number of warnings already issued.
  0 -- no warnings issued.
  1 -- 75% warning already issued.
  2 -- 85% warning already issued.
  3 -- 95% warning issued; keep warning frequently.
*/
enum warnlevel { not_warned, warned_75, warned_85, warned_95 };
static enum warnlevel warnlevel;

/* Function to call to issue a warning;
   0 means don't issue them.  */
static void (*warn_function) (const char *);

/* Start of data space; can be changed by calling memory_warnings.  */
static char *data_space_start;

/* Number of bytes of writable memory we can expect to be able to get.  */
static size_t lim_data;

#ifdef HAVE_GETRLIMIT

# ifndef RLIMIT_AS
#  define RLIMIT_AS RLIMIT_DATA
# endif

static void
get_lim_data (void)
{
  /* Set LIM_DATA to the minimum of the maximum object size and the
     maximum address space.  Don't bother to check for values like
     RLIM_INFINITY since in practice they are not much less than SIZE_MAX.  */
  struct rlimit rlimit;
  lim_data
    = (getrlimit (RLIMIT_AS, &rlimit) == 0 && rlimit.rlim_cur <= SIZE_MAX
       ? rlimit.rlim_cur
       : SIZE_MAX);
}

#elif defined WINDOWSNT

#include "w32heap.h"

static void
get_lim_data (void)
{
  extern size_t reserved_heap_size;
  lim_data = reserved_heap_size;
}

#elif defined MSDOS

void
get_lim_data (void)
{
  unsigned long totalram, freeram, totalswap, freeswap;

  dos_memory_info (&totalram, &freeram, &totalswap, &freeswap);
  lim_data = freeram;
  /* Don't believe they will give us more that 0.5 GB.   */
  if (lim_data > 512U * 1024U * 1024U)
    lim_data = 512U * 1024U * 1024U;
}

unsigned long
ret_lim_data (void)
{
  get_lim_data ();
  return lim_data;
}
#else
# error "get_lim_data not implemented on this machine"
#endif

/* Verify amount of memory available, complaining if we're near the end. */

static void
check_memory_limits (void)
{
#ifndef REL_ALLOC
  void *(*real_morecore) (ptrdiff_t) = 0;
#endif

  char *cp;
  size_t five_percent;
  size_t data_size;
  enum warnlevel new_warnlevel;

  if (lim_data == 0)
    get_lim_data ();
  five_percent = lim_data / 20;

  /* Find current end of memory and issue warning if getting near max */
  cp = (real_morecore ? real_morecore : __morecore) (0);
  data_size = cp - data_space_start;

  if (!warn_function)
    return;

  /* What level of warning does current memory usage demand?  */
  new_warnlevel
    = (data_size > five_percent * 19) ? warned_95
    : (data_size > five_percent * 17) ? warned_85
    : (data_size > five_percent * 15) ? warned_75
    : not_warned;

  /* If we have gone up a level, give the appropriate warning.  */
  if (new_warnlevel > warnlevel || new_warnlevel == warned_95)
    {
      warnlevel = new_warnlevel;
      static char const *const warn_diagnostic[] =
	{
	  "Warning: past 75% of memory limit",
	  "Warning: past 85% of memory limit",
	  "Warning: past 95% of memory limit"
	};
      warn_function (warn_diagnostic[warnlevel - 1]);
    }
  /* Handle going down in usage levels, with some hysteresis.  */
  else
    {
      /* If we go down below 70% full, issue another 75% warning
	 when we go up again.  */
      if (data_size < five_percent * 14)
	warnlevel = not_warned;
      /* If we go down below 80% full, issue another 85% warning
	 when we go up again.  */
      else if (warnlevel > warned_75 && data_size < five_percent * 16)
	warnlevel = warned_75;
      /* If we go down below 90% full, issue another 95% warning
	 when we go up again.  */
      else if (warnlevel > warned_85 && data_size < five_percent * 18)
	warnlevel = warned_85;
    }
}

/* Enable memory usage warnings.
   START says where the end of pure storage is.
   WARNFUN specifies the function to call to issue a warning.  */

void
memory_warnings (void *start, void (*warnfun) (const char *))
{
  data_space_start = start ? start : data_start;

  warn_function = warnfun;
  __after_morecore_hook = check_memory_limits;

  /* Force data limit to be recalculated on each run.  */
  lim_data = 0;
}
