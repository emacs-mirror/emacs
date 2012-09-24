/* Profiler implementation.

Copyright (C) 2012 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <stdio.h>
#include <limits.h>
#include <sys/time.h>
#include <signal.h>
#include <setjmp.h>
#include "lisp.h"

/* True if sampling profiler is running.  */

bool sample_profiler_running;

/* True if memory profiler is running.  */

bool memory_profiler_running;

static void sigprof_handler (int, siginfo_t *, void *);


/* Logs.  */

typedef struct Lisp_Hash_Table log_t;

static Lisp_Object
make_log (int heap_size, int max_stack_depth)
{
  /* We use a standard Elisp hash-table object, but we use it in
     a special way.  This is OK as long as the object is not exposed
     to Elisp, i.e. until it is returned by *-profiler-log, after which
     it can't be used any more.  */
  Lisp_Object log = make_hash_table (Qequal, make_number (heap_size),
				     make_float (DEFAULT_REHASH_SIZE),
				     make_float (DEFAULT_REHASH_THRESHOLD),
				     Qnil, Qnil, Qnil);
  struct Lisp_Hash_Table *h = XHASH_TABLE (log);

  /* What is special about our hash-tables is that the keys are pre-filled
     with the vectors we'll put in them.  */
  int i = ASIZE (h->key_and_value) / 2;
  while (0 < i)
    set_hash_key_slot (h, --i,
		       Fmake_vector (make_number (max_stack_depth), Qnil));
  return log;
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

static EMACS_INT approximate_median (log_t *log,
				     ptrdiff_t start, ptrdiff_t size)
{
  eassert (size > 0);
  if (size < 2)
    return XINT (HASH_VALUE (log, start));
  if (size < 3)
    /* Not an actual median, but better for our application than
       choosing either of the two numbers.  */
    return ((XINT (HASH_VALUE (log, start))
	     + XINT (HASH_VALUE (log, start + 1)))
	    / 2);
  else
    {
      ptrdiff_t newsize = size / 3;
      ptrdiff_t start2 = start + newsize;
      EMACS_INT i1 = approximate_median (log, start, newsize);
      EMACS_INT i2 = approximate_median (log, start2, newsize);
      EMACS_INT i3 = approximate_median (log, start2 + newsize,
					 size - 2 * newsize);
      return (i1 < i2
	      ? (i2 < i3 ? i2 : (i1 < i3 ? i3 : i1))
	      : (i1 < i3 ? i1 : (i2 < i3 ? i3 : i2)));
    }
}

static void evict_lower_half (log_t *log)
{
  ptrdiff_t size = ASIZE (log->key_and_value) / 2;
  EMACS_INT median = approximate_median (log, 0, size);
  ptrdiff_t i;

  for (i = 0; i < size; i++)
    /* Evict not only values smaller but also values equal to the median,
       so as to make sure we evict something no matter what.  */
    if (XINT (HASH_VALUE (log, i)) <= median)
      {
	Lisp_Object key = HASH_KEY (log, i);
	{ /* FIXME: we could make this more efficient.  */
	  Lisp_Object tmp;
	  XSET_HASH_TABLE (tmp, log); /* FIXME: Use make_lisp_ptr.  */
	  Fremhash (key, tmp);
	}
	eassert (EQ (log->next_free, make_number (i)));
	{
	  int j;
	  eassert (VECTORP (key));
	  for (j = 0; j < ASIZE (key); j++)
	    ASET (key, i, Qnil);
	}
	set_hash_key_slot (log, i, key);
      }
}

/* Record the current backtrace in LOG. BASE is a special name for
   describing which the backtrace come from. BASE can be nil. COUNT is
   a number how many times the profiler sees the backtrace at the
   time.  ELAPSED is a elapsed time in millisecond that the backtrace
   took.  */

static void
record_backtrace (log_t *log, size_t count)
{
  struct backtrace *backlist = backtrace_list;
  Lisp_Object backtrace;
  ptrdiff_t index, i = 0;
  ptrdiff_t asize;

  if (!INTEGERP (log->next_free))
    evict_lower_half (log);
  index = XINT (log->next_free);

  /* Get a "working memory" vector.  */
  backtrace = HASH_KEY (log, index);
  asize = ASIZE (backtrace);

  /* Copy the backtrace contents into working memory.  */
  for (; i < asize && backlist; i++, backlist = backlist->next)
    ASET (backtrace, i, *backlist->function);

  /* Make sure that unused space of working memory is filled with nil.  */
  for (; i < asize; i++)
    ASET (backtrace, i, Qnil);

  { /* We basically do a `gethash+puthash' here, except that we have to be
       careful to avoid memory allocation since we're in a signal
       handler, and we optimize the code to try and avoid computing the
       hash+lookup twice.  See fns.c:Fputhash for reference.  */
    EMACS_UINT hash;
    ptrdiff_t j = hash_lookup (log, backtrace, &hash);
    if (j >= 0)
      set_hash_value_slot (log, j,
			   make_number (count + XINT (HASH_VALUE (log, j))));
    else
      { /* BEWARE!  hash_put in general can allocate memory.
	   But currently it only does that if log->next_free is nil.  */
	int j;
	eassert (!NILP (log->next_free));
	j = hash_put (log, backtrace, make_number (count), hash);
	/* Let's make sure we've put `backtrace' right where it
	   already was to start with.  */
	eassert (index == j);

	/* FIXME: If the hash-table is almost full, we should set
	   some global flag so that some Elisp code can offload its
	   data elsewhere, so as to avoid the eviction code.  */
      }
  }
}

/* Sample profiler.  */

static Lisp_Object cpu_log;
/* Separate counter for the time spent in the GC.  */
static EMACS_INT cpu_gc_count;

/* The current sample interval in millisecond.  */

static int current_sample_interval;

DEFUN ("sample-profiler-start", Fsample_profiler_start, Ssample_profiler_start,
       1, 1, 0,
       doc: /* Start or restart sample profiler.  Sample profiler will
take samples each SAMPLE-INTERVAL in millisecond.  See also
`profiler-slot-heap-size' and `profiler-max-stack-depth'.  */)
  (Lisp_Object sample_interval)
{
  struct sigaction sa;
  struct itimerval timer;

  if (sample_profiler_running)
    error ("Sample profiler is already running");

  if (NILP (cpu_log))
    {
      cpu_gc_count = 0;
      cpu_log = make_log (profiler_slot_heap_size,
			  profiler_max_stack_depth);
    }

  current_sample_interval = XINT (sample_interval);

  sa.sa_sigaction = sigprof_handler;
  sa.sa_flags = SA_RESTART | SA_SIGINFO;
  sigemptyset (&sa.sa_mask);
  sigaction (SIGPROF, &sa, 0);

  timer.it_interval.tv_sec = 0;
  timer.it_interval.tv_usec = current_sample_interval * 1000;
  timer.it_value = timer.it_interval;
  setitimer (ITIMER_PROF, &timer, 0);

  sample_profiler_running = 1;

  return Qt;
}

DEFUN ("sample-profiler-stop", Fsample_profiler_stop, Ssample_profiler_stop,
       0, 0, 0,
       doc: /* Stop sample profiler.  Profiler log will be kept.  */)
  (void)
{
  if (!sample_profiler_running)
    error ("Sample profiler is not running");
  sample_profiler_running = 0;

  setitimer (ITIMER_PROF, 0, 0);

  return Qt;
}

DEFUN ("sample-profiler-running-p",
       Fsample_profiler_running_p, Ssample_profiler_running_p,
       0, 0, 0,
       doc: /* Return t if sample profiler is running.  */)
  (void)
{
  return sample_profiler_running ? Qt : Qnil;
}

DEFUN ("sample-profiler-log",
       Fsample_profiler_log, Ssample_profiler_log,
       0, 0, 0,
       doc: /* Return sample profiler log.  The data is a list of
(sample nil TIMESTAMP SLOTS), where TIMESTAMP is a timestamp when the
log is collected and SLOTS is a list of slots.  */)
  (void)
{
  Lisp_Object result = cpu_log;
  /* Here we're making the log visible to Elisp , so it's not safe any
     more for our use afterwards since we can't rely on its special
     pre-allocated keys anymore.  So we have to allocate a new one.  */
  cpu_log = (sample_profiler_running
	     ? make_log (profiler_slot_heap_size, profiler_max_stack_depth)
	     : Qnil);
  Fputhash (Fmake_vector (make_number (1), Qautomatic_gc),
	    make_number (cpu_gc_count),
	    result);
  cpu_gc_count = 0;
  return result;
}


/* Memory profiler.  */

static Lisp_Object memory_log;

DEFUN ("memory-profiler-start", Fmemory_profiler_start, Smemory_profiler_start,
       0, 0, 0,
       doc: /* Start/restart memory profiler.  See also
`profiler-slot-heap-size' and `profiler-max-stack-depth'.  */)
  (void)
{
  if (memory_profiler_running)
    error ("Memory profiler is already running");

  if (NILP (memory_log))
    memory_log = make_log (profiler_slot_heap_size,
			   profiler_max_stack_depth);

  memory_profiler_running = 1;

  return Qt;
}

DEFUN ("memory-profiler-stop",
       Fmemory_profiler_stop, Smemory_profiler_stop,
       0, 0, 0,
       doc: /* Stop memory profiler.  Profiler log will be kept.  */)
  (void)
{
  if (!memory_profiler_running)
    error ("Memory profiler is not running");
  memory_profiler_running = 0;

  return Qt;
}

DEFUN ("memory-profiler-running-p",
       Fmemory_profiler_running_p, Smemory_profiler_running_p,
       0, 0, 0,
       doc: /* Return t if memory profiler is running.  */)
  (void)
{
  return memory_profiler_running ? Qt : Qnil;
}

DEFUN ("memory-profiler-log",
       Fmemory_profiler_log, Smemory_profiler_log,
       0, 0, 0,
       doc: /* Return memory profiler log.  The data is a list of
(memory nil TIMESTAMP SLOTS), where TIMESTAMP is a timestamp when the
log is collected and SLOTS is a list of slots.  */)
  (void)
{
  Lisp_Object result = memory_log;
  /* Here we're making the log visible to Elisp , so it's not safe any
     more for our use afterwards since we can't rely on its special
     pre-allocated keys anymore.  So we have to allocate a new one.  */
  memory_log = (memory_profiler_running
		? make_log (profiler_slot_heap_size, profiler_max_stack_depth)
		: Qnil);
  return result;
}


/* Signals and probes.  */

/* Signal handler for sample profiler.  */

static void
sigprof_handler (int signal, siginfo_t *info, void *ctx)
{
  eassert (HASH_TABLE_P (cpu_log));
  if (backtrace_list && EQ (*backtrace_list->function, Qautomatic_gc))
    /* Special case the time-count inside GC because the hash-table
       code is not prepared to be used while the GC is running.
       More specifically it uses ASIZE at many places where it does
       not expect the ARRAY_MARK_FLAG to be set.  We could try and
       harden the hash-table code, but it doesn't seem worth the
       effort.  */
    cpu_gc_count += current_sample_interval;
  else
    record_backtrace (XHASH_TABLE (cpu_log), current_sample_interval);
}

/* Record that the current backtrace allocated SIZE bytes.  */
/* FIXME: Inline it everywhere!  */
void
malloc_probe (size_t size)
{
  if (HASH_TABLE_P (memory_log))
    record_backtrace (XHASH_TABLE (memory_log), size);
}

void
syms_of_profiler (void)
{
  DEFVAR_INT ("profiler-max-stack-depth", profiler_max_stack_depth,
	      doc: /* FIXME */);
  profiler_max_stack_depth = 16;
  DEFVAR_INT ("profiler-slot-heap-size", profiler_slot_heap_size,
	      doc: /* FIXME */);
  profiler_slot_heap_size = 10000;

  cpu_log = memory_log = Qnil;
  staticpro (&cpu_log);
  staticpro (&memory_log);

  /* FIXME: Rename things to start with "profiler-", to use "cpu" instead of
     "sample", and to make them sound like they're internal or something.  */
  defsubr (&Ssample_profiler_start);
  defsubr (&Ssample_profiler_stop);
  defsubr (&Ssample_profiler_running_p);
  defsubr (&Ssample_profiler_log);

  defsubr (&Smemory_profiler_start);
  defsubr (&Smemory_profiler_stop);
  defsubr (&Smemory_profiler_running_p);
  defsubr (&Smemory_profiler_log);
}
