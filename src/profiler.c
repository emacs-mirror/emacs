/* Profiler implementation.

Copyright (C) 2012-2026 Free Software Foundation, Inc.

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
#include "syssignal.h"
#include "systime.h"
#include "pdumper.h"

/* Return A + B, but return the maximum fixnum if the result would overflow.
   Assume A and B are nonnegative and in fixnum range.  */

static EMACS_INT
saturated_add (EMACS_INT a, EMACS_INT b)
{
  return min (a + b, MOST_POSITIVE_FIXNUM);
}

/* Logs.  */

/* A fully associative cache of size SIZE, mapping vectors of DEPTH
   Lisp objects to counts.  */
typedef struct {
  /* We use `int' throughout for table indices because anything bigger
     is overkill.  (Maybe we should make a typedef, but int is short.)  */
  int size;		        /* number of entries */
  int depth;			/* elements in each key vector */
  int index_size;	        /* size of index */
  Lisp_Object *trace;		/* working trace, `depth' elements */
  int *index;		        /* `index_size' indices or -1 if nothing */
  int *next;		        /* `size' indices to next bucket or -1 */
  EMACS_UINT *hash;	        /* `size' hash values */
  Lisp_Object *keys;		/* `size' keys of `depth' objects each */
  EMACS_INT *counts;		/* `size' entries, 0 indicates unused entry */
  int next_free;		/* next free entry, -1 if all taken */
} log_t;

static void
mark_log (log_t *log)
{
  if (log == NULL)
    return;
  int size = log->size;
  int depth = log->depth;
  for (int i = 0; i < size; i++)
    if (log->counts[i] > 0)	/* Only mark valid keys.  */
      mark_objects (log->keys + i * depth, depth);
}

static log_t *
make_log (int size, int depth)
{
  log_t *log = xmalloc (sizeof *log);
  log->size = size;
  log->depth = depth;

  /* The index size is arbitrary but for there to be any point it should be
     bigger than SIZE.  FIXME: make it a power of 2 or a (pseudo)prime.  */
  int index_size = size * 2 + 1;
  log->index_size = index_size;

  log->trace = xmalloc (depth * sizeof *log->trace);

  log->index = xmalloc (index_size * sizeof *log->index);
  for (int i = 0; i < index_size; i++)
    log->index[i] = -1;

  log->next = xmalloc (size * sizeof *log->next);
  for (int i = 0; i < size - 1; i++)
    log->next[i] = i + 1;
  log->next[size - 1] = -1;
  log->next_free = 0;

  log->hash = xmalloc (size * sizeof *log->hash);
  log->keys = xzalloc (size * depth * sizeof *log->keys);
  log->counts = xzalloc (size * sizeof *log->counts);

  return log;
}

static void
free_log (log_t *log)
{
  xfree (log->trace);
  xfree (log->index);
  xfree (log->next);
  xfree (log->hash);
  xfree (log->keys);
  xfree (log->counts);
  xfree (log);
}

static inline EMACS_INT
get_log_count (log_t *log, int idx)
{
  eassume (idx >= 0 && idx < log->size);
  return log->counts[idx];
}

static inline void
set_log_count (log_t *log, int idx, EMACS_INT val)
{
  eassume (idx >= 0 && idx < log->size && val >= 0);
  log->counts[idx] = val;
}

static inline Lisp_Object *
get_key_vector (log_t *log, int idx)
{
  eassume (idx >= 0 && idx < log->size);
  return log->keys + idx * log->depth;
}

static inline int
log_hash_index (log_t *log, EMACS_UINT hash)
{
  /* FIXME: avoid division.  */
  return hash % log->index_size;
}

static void
remove_log_entry (log_t *log, int idx)
{
  eassume (idx >= 0 && idx < log->size);
  /* Remove from index.  */
  int hidx = log_hash_index (log, log->hash[idx]);
  int *p = &log->index[hidx];
  while (*p != idx)
    {
      eassert (*p >= 0 && *p < log->size);
      p = &log->next[*p];
    }
  *p = log->next[*p];
  /* Invalidate entry and put it on the free list.  */
  log->counts[idx] = 0;
  log->next[idx] = log->next_free;
  log->next_free = idx;
}

static bool
trace_equal (Lisp_Object *bt1, Lisp_Object *bt2, int depth)
{
  for (int i = 0; i < depth; i++)
    if (!BASE_EQ (bt1[i], bt2[i]) && NILP (Ffunction_equal (bt1[i], bt2[i])))
      return false;
  return true;
}

static EMACS_UINT
trace_hash (Lisp_Object *trace, int depth)
{
  EMACS_UINT hash = 0;
  for (int i = 0; i < depth; i++)
    {
      Lisp_Object f = trace[i];
      EMACS_UINT hash1
	= (CLOSUREP (f) ? XHASH (AREF (f, CLOSURE_CODE)) : XHASH (f));
      hash = sxhash_combine (hash, hash1);
    }
  return hash;
}

struct profiler_log {
  log_t *log;
  EMACS_INT gc_count;  /* Samples taken during GC.  */
  EMACS_INT discarded; /* Samples evicted during table overflow.  */
};

static Lisp_Object export_log (struct profiler_log *);

static struct profiler_log
make_profiler_log (void)
{
  int size = clip_to_bounds (0, profiler_log_size,
			     min (MOST_POSITIVE_FIXNUM, INT_MAX));
  int max_stack_depth = clip_to_bounds (0, profiler_max_stack_depth, INT_MAX);
  return (struct profiler_log){make_log (size, max_stack_depth), 0, 0};
}

static void
free_profiler_log (struct profiler_log *plog)
{
  free_log (plog->log);
  plog->log = NULL;
}


/* Evict the least used half of the hash_table.

   When the table is full, we have to evict someone.
   The easiest and most efficient is to evict the value we're about to add
   (i.e. once the table is full, stop sampling).

   We could also pick the element with the lowest count and evict it,
   but finding it is O(N) and for that amount of work we get very
   little in return: for the next sample, this latest sample will have
   count==1 and will hence be a prime candidate for eviction :-(

   So instead, we take O(N) time to eliminate more or less half of the
   entries (the half with the lowest counts).  So we get an amortized
   cost of O(1) and we get O(N) time for a new entry to grow larger
   than the other least counts before a new round of eviction.  */

static EMACS_INT
approximate_median (log_t *log, int start, int size)
{
  eassert (size > 0);
  if (size < 2)
    return get_log_count (log, start);
  if (size < 3)
    /* Not an actual median, but better for our application than
       choosing either of the two numbers.  */
    return ((get_log_count (log, start)
	     + get_log_count (log, start + 1))
	    / 2);
  else
    {
      int newsize = size / 3;
      int start2 = start + newsize;
      EMACS_INT i1 = approximate_median (log, start, newsize);
      EMACS_INT i2 = approximate_median (log, start2, newsize);
      EMACS_INT i3 = approximate_median (log, start2 + newsize,
					 size - 2 * newsize);
      return (i1 < i2
	      ? (i2 < i3 ? i2 : max (i1, i3))
	      : (i1 < i3 ? i1 : max (i2, i3)));
    }
}

static void
evict_lower_half (struct profiler_log *plog)
{
  log_t *log = plog->log;
  int size = log->size;
  EMACS_INT median = approximate_median (log, 0, size);

  for (int i = 0; i < size; i++)
    {
      EMACS_INT count = get_log_count (log, i);
      /* Evict not only values smaller but also values equal to the median,
	 so as to make sure we evict something no matter what.  */
      if (count <= median)
	{
	  plog->discarded = saturated_add (plog->discarded, count);
	  remove_log_entry (log, i);
	}
    }
}

/* Record the current backtrace in LOG.  COUNT is the weight of this
   current backtrace: interrupt counts for CPU, and the allocation
   size for memory.  */

static void
record_backtrace (struct profiler_log *plog, EMACS_INT count)
{
  log_t *log = plog->log;
  get_backtrace (log->trace, log->depth);
  EMACS_UINT hash = trace_hash (log->trace, log->depth);
  int hidx = log_hash_index (log, hash);
  int idx = log->index[hidx];
  while (idx >= 0)
    {
      if (log->hash[idx] == hash
	  && trace_equal (log->trace, get_key_vector (log, idx), log->depth))
	{
	  /* Found existing entry.  */
	  set_log_count (log, idx,
			 saturated_add (get_log_count (log, idx), count));
	  return;
	}
      idx = log->next[idx];
    }

  /* Add new entry.  */
  if (log->next_free < 0)
    evict_lower_half (plog);
  idx = log->next_free;
  eassert (idx >= 0);
  log->next_free = log->next[idx];
  log->next[idx] = log->index[hidx];
  log->index[hidx] = idx;
  eassert (log->counts[idx] == 0);
  log->hash[idx] = hash;
  memcpy (get_key_vector (log, idx), log->trace,
	  log->depth * sizeof *log->trace);
  log->counts[idx] = count;

  /* FIXME: If the hash-table is almost full, we should set
     some global flag so that some Elisp code can offload its
     data elsewhere, so as to avoid the eviction code.
     There are 2 ways to do that:
     - Set a flag checked in maybe_quit, such that maybe_quit can then
     call Fprofiler_cpu_log and stash the full log for later use.
     - Set a flag check in post-gc-hook, so that Elisp code can call
     profiler-cpu-log.  That gives us more flexibility since that
     Elisp code can then do all kinds of fun stuff like write
     the log to disk.  Or turn it right away into a call tree.
     Of course, using Elisp is generally preferable, but it may
     take longer until we get a chance to run the Elisp code, so
     there's more risk that the table will get full before we
     get there.  */
}

/* Sampling profiler.  */

/* Signal handler for sampling profiler.  */

static void
add_sample (struct profiler_log *plog, EMACS_INT count)
{
  if (BASE_EQ (backtrace_top_function (), QAutomatic_GC)) /* bug#60237 */
    /* Special case the time-count inside GC because the hash-table
       code is not prepared to be used while the GC is running.
       More specifically it uses ASIZE at many places where it does
       not expect the ARRAY_MARK_FLAG to be set.  We could try and
       harden the hash-table code, but it doesn't seem worth the
       effort.  */
    plog->gc_count = saturated_add (plog->gc_count, count);
  else
    record_backtrace (plog, count);
}

#ifdef PROFILER_CPU_SUPPORT

/* The sampling interval specified.  */
static Lisp_Object profiler_cpu_interval = LISPSYM_INITIALLY (Qnil);

/* The profiler timer and whether it was properly initialized, if
   POSIX timers are available.  */
#ifdef HAVE_ITIMERSPEC
static timer_t profiler_timer;
static bool profiler_timer_ok;
#endif

/* Status of sampling profiler.  */
static enum profiler_cpu_running
  { NOT_RUNNING,
#ifdef HAVE_ITIMERSPEC
    TIMER_SETTIME_RUNNING,
#endif
    SETITIMER_RUNNING
  }
  profiler_cpu_running;

/* Hash-table log of CPU profiler.  */
static struct profiler_log cpu;

/* The current sampling interval in nanoseconds.  */
static EMACS_INT current_sampling_interval;

static void
handle_profiler_signal (int signal)
{
  EMACS_INT count = 1;
#if defined HAVE_ITIMERSPEC && defined HAVE_TIMER_GETOVERRUN
  if (profiler_timer_ok)
    {
      int overruns = timer_getoverrun (profiler_timer);
      eassert (overruns >= 0);
      count += overruns;
    }
#endif
  add_sample (&cpu, count);
}

static void
deliver_profiler_signal (int signal)
{
  deliver_process_signal (signal, handle_profiler_signal);
}

static int
setup_cpu_timer (Lisp_Object sampling_interval)
{
  int billion = 1000000000;

  if (! RANGED_FIXNUMP (1, sampling_interval,
			 (TYPE_MAXIMUM (time_t) < EMACS_INT_MAX / billion
			  ? ((EMACS_INT) TYPE_MAXIMUM (time_t) * billion
			     + (billion - 1))
			  : EMACS_INT_MAX)))
    return -1;

  current_sampling_interval = XFIXNUM (sampling_interval);
  struct timespec interval
    = make_timespec (current_sampling_interval / billion,
		     current_sampling_interval % billion);
  struct sigaction action;
  emacs_sigaction_init (&action, deliver_profiler_signal);
  sigaction (SIGPROF, &action, 0);

#ifdef HAVE_ITIMERSPEC
  if (! profiler_timer_ok)
    {
      /* System clocks to try, in decreasing order of desirability.  */
      static clockid_t const system_clock[] = {
#ifdef CLOCK_THREAD_CPUTIME_ID
	CLOCK_THREAD_CPUTIME_ID,
#endif
#ifdef CLOCK_PROCESS_CPUTIME_ID
	CLOCK_PROCESS_CPUTIME_ID,
#endif
#ifdef CLOCK_MONOTONIC
	CLOCK_MONOTONIC,
#endif
	CLOCK_REALTIME
      };
      struct sigevent sigev;
      sigev.sigev_value.sival_ptr = &profiler_timer;
      sigev.sigev_signo = SIGPROF;
      sigev.sigev_notify = SIGEV_SIGNAL;

      for (int i = 0; i < ARRAYELTS (system_clock); i++)
	if (timer_create (system_clock[i], &sigev, &profiler_timer) == 0)
	  {
	    profiler_timer_ok = true;
	    break;
	  }
    }

  if (profiler_timer_ok)
    {
      struct itimerspec ispec;
      ispec.it_value = ispec.it_interval = interval;
      if (timer_settime (profiler_timer, 0, &ispec, 0) == 0)
	return TIMER_SETTIME_RUNNING;
    }
#endif

#ifdef HAVE_SETITIMER
  struct itimerval timer;
  timer.it_value = timer.it_interval = make_timeval (interval);
  if (setitimer (ITIMER_PROF, &timer, 0) == 0)
    return SETITIMER_RUNNING;
#endif

  return NOT_RUNNING;
}

DEFUN ("profiler-cpu-start", Fprofiler_cpu_start, Sprofiler_cpu_start,
       1, 1, 0,
       doc: /* Start or restart the cpu profiler.
It takes call-stack samples each SAMPLING-INTERVAL nanoseconds, approximately.
See also `profiler-log-size' and `profiler-max-stack-depth'.  */)
  (Lisp_Object sampling_interval)
{
  if (profiler_cpu_running)
    error ("CPU profiler is already running");

  if (cpu.log == NULL)
    cpu = make_profiler_log ();

  int status = setup_cpu_timer (sampling_interval);
  if (status < 0)
    {
      profiler_cpu_running = NOT_RUNNING;
      error ("Invalid sampling interval");
    }
  else
    {
      profiler_cpu_interval = sampling_interval;
      profiler_cpu_running = status;
      if (! profiler_cpu_running)
	error ("Unable to start profiler timer");
    }

  return Qt;
}

DEFUN ("profiler-cpu-stop", Fprofiler_cpu_stop, Sprofiler_cpu_stop,
       0, 0, 0,
       doc: /* Stop the cpu profiler.  The profiler log is not affected.
Return non-nil if the profiler was running.  */)
  (void)
{
  switch (profiler_cpu_running)
    {
    case NOT_RUNNING:
      return Qnil;

#ifdef HAVE_ITIMERSPEC
    case TIMER_SETTIME_RUNNING:
      {
	struct itimerspec disable = { 0, };
	timer_settime (profiler_timer, 0, &disable, 0);
      }
      break;
#endif

#ifdef HAVE_SETITIMER
    case SETITIMER_RUNNING:
      {
	struct itimerval disable = { 0, };
	setitimer (ITIMER_PROF, &disable, 0);
      }
      break;
#endif
    }

  signal (SIGPROF, SIG_IGN);
  profiler_cpu_running = NOT_RUNNING;
  return Qt;
}

DEFUN ("profiler-cpu-running-p",
       Fprofiler_cpu_running_p, Sprofiler_cpu_running_p,
       0, 0, 0,
       doc: /* Return non-nil if cpu profiler is running.  */)
  (void)
{
  return profiler_cpu_running ? Qt : Qnil;
}

DEFUN ("profiler-cpu-log", Fprofiler_cpu_log, Sprofiler_cpu_log,
       0, 0, 0,
       doc: /* Return the current cpu profiler log.
The log is a hash-table mapping backtraces to counters which represent
the amount of time spent at those points.  Every backtrace is a vector
of functions, where the last few elements may be nil.

If the profiler has not run since the last invocation of
`profiler-cpu-log' (or was never run at all), return nil.  If the
profiler is currently running, allocate a new log for future samples
before returning.  */)
  (void)
{
  /* Temporarily stop profiling to avoid it interfering with our data
     access.  */
  bool prof_cpu = profiler_cpu_running;
  if (prof_cpu)
    Fprofiler_cpu_stop ();

  Lisp_Object ret = export_log (&cpu);

  if (prof_cpu)
    Fprofiler_cpu_start (profiler_cpu_interval);

  return ret;
}
#endif /* PROFILER_CPU_SUPPORT */

/* Extract log data to a Lisp hash table.  The log data is then erased.  */
static Lisp_Object
export_log (struct profiler_log *plog)
{
  if (!plog->log)
    return Qnil;

  log_t *log = plog->log;
  /* The returned hash table uses `equal' as key equivalence predicate
     which is more discriminating than the `function-equal' used by
     the log but close enough, and will never confuse two distinct
     keys in the log.  */
  Lisp_Object h = make_hash_table (&hashtest_equal, DEFAULT_HASH_SIZE,
				   Weak_None);
  for (int i = 0; i < log->size; i++)
    {
      int count = get_log_count (log, i);
      if (count > 0)
	Fputhash (Fvector (log->depth, get_key_vector (log, i)),
		  make_fixnum (count), h);
    }
  if (plog->gc_count)
    Fputhash (CALLN (Fvector, QAutomatic_GC, Qnil),
	      make_fixnum (plog->gc_count),
	      h);
  if (plog->discarded)
    Fputhash (CALLN (Fvector, QDiscarded_Samples, Qnil),
	      make_fixnum (plog->discarded),
	      h);
  free_profiler_log (plog);
  return h;
}

/* Memory profiler.  */

/* Hash-table log of Memory profiler.  */
static struct profiler_log memory;

/* True if memory profiler is running.  */
bool profiler_memory_running;

DEFUN ("profiler-memory-start", Fprofiler_memory_start, Sprofiler_memory_start,
       0, 0, 0,
       doc: /* Start/restart the memory profiler.
The memory profiler will take samples of the call-stack whenever a new
allocation takes place.  Note that most small allocations only trigger
the profiler occasionally.
See also `profiler-log-size' and `profiler-max-stack-depth'.  */)
  (void)
{
  if (profiler_memory_running)
    error ("Memory profiler is already running");

  if (memory.log == NULL)
    memory = make_profiler_log ();

  profiler_memory_running = true;

  return Qt;
}

DEFUN ("profiler-memory-stop",
       Fprofiler_memory_stop, Sprofiler_memory_stop,
       0, 0, 0,
       doc: /* Stop the memory profiler.  The profiler log is not affected.
Return non-nil if the profiler was running.  */)
  (void)
{
  if (!profiler_memory_running)
    return Qnil;
  profiler_memory_running = false;
  return Qt;
}

DEFUN ("profiler-memory-running-p",
       Fprofiler_memory_running_p, Sprofiler_memory_running_p,
       0, 0, 0,
       doc: /* Return non-nil if memory profiler is running.  */)
  (void)
{
  return profiler_memory_running ? Qt : Qnil;
}

DEFUN ("profiler-memory-log",
       Fprofiler_memory_log, Sprofiler_memory_log,
       0, 0, 0,
       doc: /* Return the current memory profiler log.
The log is a hash-table mapping backtraces to counters which represent
the amount of memory allocated at those points.  Every backtrace is a vector
of functions, where the last few elements may be nil.

If the profiler has not run since the last invocation of
`profiler-memory-log' (or was never run at all), return nil.  If the
profiler is currently running, allocate a new log for future samples
before returning.  */)
  (void)
{
  bool prof_mem = profiler_memory_running;
  if (prof_mem)
    Fprofiler_memory_stop ();

  Lisp_Object ret = export_log (&memory);

  if (prof_mem)
    Fprofiler_memory_start ();

  return ret;
}


/* Signals and probes.  */

/* Record that the current backtrace allocated SIZE bytes.  */
void
malloc_probe (size_t size)
{
  add_sample (&memory, min (size, MOST_POSITIVE_FIXNUM));
}

DEFUN ("function-equal", Ffunction_equal, Sfunction_equal, 2, 2, 0,
       doc: /* Return non-nil if F1 and F2 come from the same source.
Used to determine if different closures are just different instances of
the same lambda expression, or are really unrelated function.  */)
     (Lisp_Object f1, Lisp_Object f2)
{
  bool res;
  if (EQ (f1, f2))
    res = true;
  else if (CLOSUREP (f1) && CLOSUREP (f2))
    res = EQ (AREF (f1, CLOSURE_CODE), AREF (f2, CLOSURE_CODE));
  else
    res = false;
  return res ? Qt : Qnil;
}

void
mark_profiler (void)
{
#ifdef PROFILER_CPU_SUPPORT
  mark_log (cpu.log);
#endif
  mark_log (memory.log);
}

void
syms_of_profiler (void)
{
  DEFVAR_INT ("profiler-max-stack-depth", profiler_max_stack_depth,
	      doc: /* Number of elements from the call-stack recorded in the log.  */);
  profiler_max_stack_depth = 16;
  DEFVAR_INT ("profiler-log-size", profiler_log_size,
	      doc: /* Number of distinct call-stacks that can be recorded in a profiler log.
If the log gets full, some of the least-seen call-stacks will be evicted
to make room for new entries.  */);
  profiler_log_size = 10000;

  DEFSYM (QDiscarded_Samples, "Discarded Samples");

  defsubr (&Sfunction_equal);

#ifdef PROFILER_CPU_SUPPORT
  profiler_cpu_running = NOT_RUNNING;
  defsubr (&Sprofiler_cpu_start);
  defsubr (&Sprofiler_cpu_stop);
  defsubr (&Sprofiler_cpu_running_p);
  defsubr (&Sprofiler_cpu_log);
#endif
  profiler_memory_running = false;
  defsubr (&Sprofiler_memory_start);
  defsubr (&Sprofiler_memory_stop);
  defsubr (&Sprofiler_memory_running_p);
  defsubr (&Sprofiler_memory_log);
}
