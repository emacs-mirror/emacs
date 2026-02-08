/* Haiku subroutines that are general to the Haiku operating system.
   Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

#include "lisp.h"
#include "process.h"
#include "coding.h"

#include <kernel/OS.h>

#include <pwd.h>
#include <stdlib.h>

Lisp_Object
list_system_processes (void)
{
  team_info info;
  int32 cookie = 0;
  Lisp_Object lval = Qnil;

  while (get_next_team_info (&cookie, &info) == B_OK)
    lval = Fcons (make_fixnum (info.team), lval);

  return lval;
}

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  CHECK_FIXNUM (pid);

  team_info info;
  Lisp_Object lval = Qnil;
  thread_info inf;
  area_info area;
  team_id id = (team_id) XFIXNUM (pid);
  struct passwd *g;
  size_t mem = 0;

  if (get_team_info (id, &info) != B_OK)
    return Qnil;

  bigtime_t everything = 0, vsample = 0;
  bigtime_t cpu_eaten = 0, esample = 0;

  lval = Fcons (Fcons (Qeuid, make_fixnum (info.uid)), lval);
  lval = Fcons (Fcons (Qegid, make_fixnum (info.gid)), lval);
  lval = Fcons (Fcons (Qthcount, make_fixnum (info.thread_count)), lval);
  lval = Fcons (Fcons (Qcomm, build_string_from_utf8 (info.args)), lval);

  g = getpwuid (info.uid);

  if (g && g->pw_name)
    lval = Fcons (Fcons (Quser, build_string (g->pw_name)), lval);

  /* FIXME: Calculating this makes Emacs show up as using 100% CPU! */

  for (int32 team_cookie = 0;
       get_next_team_info (&team_cookie, &info) == B_OK;)
    for (int32 thread_cookie = 0;
	 get_next_thread_info (info.team, &thread_cookie, &inf) == B_OK;)
      {
	if (inf.team == id && strncmp (inf.name, "idle thread ", 12))
	  cpu_eaten += inf.user_time + inf.kernel_time;
	everything += inf.user_time + inf.kernel_time;
      }

  sleep (0.05);

  for (int32 team_cookie = 0;
       get_next_team_info (&team_cookie, &info) == B_OK;)
    for (int32 thread_cookie = 0;
	 get_next_thread_info (info.team, &thread_cookie, &inf) == B_OK;)
      {
	if (inf.team == id && strncmp (inf.name, "idle thread ", 12))
	  esample += inf.user_time + inf.kernel_time;
	vsample += inf.user_time + inf.kernel_time;
      }

  cpu_eaten = esample - cpu_eaten;
  everything = vsample - everything;

  if (everything)
    lval = Fcons (Fcons (Qpcpu, make_float (((double) (cpu_eaten) /
					     (double) (everything)) * 100)),
		  lval);
  else
    lval = Fcons (Fcons (Qpcpu, make_float (0.0)), lval);

  for (ssize_t area_cookie = 0;
       get_next_area_info (id, &area_cookie, &area) == B_OK;)
    mem += area.ram_size;

  system_info sinfo;
  get_system_info (&sinfo);
  int64 max = (int64) sinfo.max_pages * B_PAGE_SIZE;

  lval = Fcons (Fcons (Qpmem, make_float (((double) mem /
					   (double) max) * 100)),
		lval);
  lval = Fcons (Fcons (Qrss, make_fixnum (mem / 1024)), lval);

  return lval;
}


/* Borrowed from w32 implementation.  */

struct load_sample
{
  time_t sample_time;
  bigtime_t idle;
  bigtime_t kernel;
  bigtime_t user;
};

/* We maintain 1-sec samples for the last 16 minutes in a circular buffer.  */
static struct load_sample samples[16*60];
static int first_idx = -1, last_idx = -1;
static int max_idx = ARRAYELTS (samples);
static unsigned num_of_processors = 0;

static int
buf_next (int from)
{
  int next_idx = from + 1;

  if (next_idx >= max_idx)
    next_idx = 0;

  return next_idx;
}

static int
buf_prev (int from)
{
  int prev_idx = from - 1;

  if (prev_idx < 0)
    prev_idx = max_idx - 1;

  return prev_idx;
}

static double
getavg (int which)
{
  double retval = -1.0;
  double tdiff;
  int idx;
  double span = (which == 0 ? 1.0 : (which == 1 ? 5.0 : 15.0)) * 60;
  time_t now = samples[last_idx].sample_time;

  if (first_idx != last_idx)
    {
      for (idx = buf_prev (last_idx); ; idx = buf_prev (idx))
	{
	  tdiff = difftime (now, samples[idx].sample_time);
	  if (tdiff >= span - 2 * DBL_EPSILON * now)
	    {
	      long double sys =
		(samples[last_idx].kernel + samples[last_idx].user) -
		(samples[idx].kernel + samples[idx].user);
	      long double idl = samples[last_idx].idle - samples[idx].idle;

	      retval = (idl / (sys + idl)) * num_of_processors;
	      break;
	    }
	  if (idx == first_idx)
	    break;
	}
    }

  return retval;
}

static void
sample_sys_load (bigtime_t *idle, bigtime_t *system, bigtime_t *user)
{
  bigtime_t i = 0, s = 0, u = 0;
  team_info info;
  thread_info inf;

  for (int32 team_cookie = 0;
       get_next_team_info (&team_cookie, &info) == B_OK;)
    for (int32 thread_cookie = 0;
	 get_next_thread_info (info.team, &thread_cookie, &inf) == B_OK;)
      {
	if (!strncmp (inf.name, "idle thread ", 12))
	  i += inf.user_time + inf.kernel_time;
	else
	  s += inf.kernel_time, u += inf.user_time;
      }

  *idle = i;
  *system = s;
  *user = u;
}

int
getloadavg (double loadavg[], int nelem)
{
  int elem;
  bigtime_t idle, kernel, user;
  time_t now = time (NULL);

  if (num_of_processors <= 0)
    {
      system_info i;
      if (get_system_info (&i) == B_OK)
	num_of_processors = i.cpu_count;
    }

  /* If system time jumped back for some reason, delete all samples
     whose time is later than the current wall-clock time.  This
     prevents load average figures from becoming frozen for prolonged
     periods of time, when system time is reset backwards.  */
  if (last_idx >= 0)
    {
      while (difftime (now, samples[last_idx].sample_time) < -1.0)
	{
	  if (last_idx == first_idx)
	    {
	      first_idx = last_idx = -1;
	      break;
	    }
	  last_idx = buf_prev (last_idx);
	}
    }

  /* Store another sample.  We ignore samples that are less than 1 sec
     apart.  */
  if (last_idx < 0
      || (difftime (now, samples[last_idx].sample_time)
	  >= 1.0 - 2 * DBL_EPSILON * now))
    {
      sample_sys_load (&idle, &kernel, &user);
      last_idx = buf_next (last_idx);
      samples[last_idx].sample_time = now;
      samples[last_idx].idle = idle;
      samples[last_idx].kernel = kernel;
      samples[last_idx].user = user;
      /* If the buffer has more that 15 min worth of samples, discard
	 the old ones.  */
      if (first_idx == -1)
	first_idx = last_idx;
      while (first_idx != last_idx
	     && (difftime (now, samples[first_idx].sample_time)
	         >= 15.0 * 60 + 2 * DBL_EPSILON * now))
	first_idx = buf_next (first_idx);
    }

  for (elem = 0; elem < nelem; elem++)
    {
      double avg = getavg (elem);

      if (avg < 0)
	break;
      loadavg[elem] = avg;
    }

  /* Always return at least one element, otherwise load-average
     returns nil, and Lisp programs might decide we cannot measure
     system load.  For example, jit-lock-stealth-load's defcustom
     might decide that feature is "unsupported".  */
  if (elem == 0)
    loadavg[elem++] = 0.09;	/* < display-time-load-average-threshold */

  return elem;
}
