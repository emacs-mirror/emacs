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

/* True during tracing.  */

bool is_in_trace;

/* Tag for GC entry.  */

Lisp_Object Qgc;

static void sigprof_handler (int, siginfo_t *, void *);
static void block_sigprof (void);
static void unblock_sigprof (void);


/* Pattern matching.  */

enum pattern_type
{
  pattern_exact,		/* foo */
  pattern_body_exact,		/* *foo* */
  pattern_pre_any,		/* *foo */
  pattern_post_any,		/* foo* */
  pattern_body_any		/* foo*bar */
};

struct pattern
{
  enum pattern_type type;
  char *exact;
  char *extra;
  int exact_length;
  int extra_length;
};

static struct pattern *
parse_pattern (const char *pattern)
{
  int length = strlen (pattern);
  enum pattern_type type;
  char *exact;
  char *extra = 0;
  struct pattern *pat =
    (struct pattern *) xmalloc (sizeof (struct pattern));

  if (length > 1
      && *pattern == '*'
      && pattern[length - 1] == '*')
    {
      type = pattern_body_exact;
      exact = xstrdup (pattern + 1);
      exact[length - 2] = 0;
    }
  else if (*pattern == '*')
    {
      type = pattern_pre_any;
      exact = xstrdup (pattern + 1);
    }
  else if (pattern[length - 1] == '*')
    {
      type = pattern_post_any;
      exact = xstrdup (pattern);
      exact[length - 1] = 0;
    }
  else if (strchr (pattern, '*'))
    {
      type = pattern_body_any;
      exact = xstrdup (pattern);
      extra = strchr (exact, '*');
      *extra++ = 0;
    }
  else
    {
      type = pattern_exact;
      exact = xstrdup (pattern);
    }

  pat->type = type;
  pat->exact = exact;
  pat->extra = extra;
  pat->exact_length = strlen (exact);
  pat->extra_length = extra ? strlen (extra) : 0;

  return pat;
}

static void
free_pattern (struct pattern *pattern)
{
  xfree (pattern->exact);
  xfree (pattern);
}

static int
pattern_match_1 (enum pattern_type type,
		 const char *exact,
		 int exact_length,
		 const char *string,
		 int length)
{
  if (exact_length > length)
    return 0;
  switch (type)
    {
    case pattern_exact:
      return exact_length == length && !strncmp (exact, string, length);
    case pattern_body_exact:
      return strstr (string, exact) != 0;
    case pattern_pre_any:
      return !strncmp (exact, string + (length - exact_length), exact_length);
    case pattern_post_any:
      return !strncmp (exact, string, exact_length);
    case pattern_body_any:
      return 0;
    }
}

static int
pattern_match (struct pattern *pattern, const char *string)
{
  int length = strlen (string);
  switch (pattern->type)
    {
    case pattern_body_any:
      if (pattern->exact_length + pattern->extra_length > length)
	return 0;
      return pattern_match_1 (pattern_post_any,
			      pattern->exact,
			      pattern->exact_length,
			      string, length)
	&&   pattern_match_1 (pattern_pre_any,
			      pattern->extra,
			      pattern->extra_length,
			      string, length);
    default:
      return pattern_match_1 (pattern->type,
			      pattern->exact,
			      pattern->exact_length,
			      string, length);
    }
}

#if 0
static int
match (const char *pattern, const char *string)
{
  int res;
  struct pattern *pat = parse_pattern (pattern);
  res = pattern_match (pat, string);
  free_pattern (pat);
  return res;
}

static void
should_match (const char *pattern, const char *string)
{
  putchar (match (pattern, string) ? '.' : 'F');
}

static void
should_not_match (const char *pattern, const char *string)
{
  putchar (match (pattern, string) ? 'F' : '.');
}

static void
pattern_match_tests (void)
{
  should_match ("", "");
  should_not_match ("", "a");
  should_match ("a", "a");
  should_not_match ("a", "ab");
  should_not_match ("ab", "a");
  should_match ("*a*", "a");
  should_match ("*a*", "ab");
  should_match ("*a*", "ba");
  should_match ("*a*", "bac");
  should_not_match ("*a*", "");
  should_not_match ("*a*", "b");
  should_match ("*", "");
  should_match ("*", "a");
  should_match ("a*", "a");
  should_match ("a*", "ab");
  should_not_match ("a*", "");
  should_not_match ("a*",  "ba");
  should_match ("*a", "a");
  should_match ("*a", "ba");
  should_not_match ("*a", "");
  should_not_match ("*a", "ab");
  should_match ("a*b", "ab");
  should_match ("a*b", "acb");
  should_match ("a*b", "aab");
  should_match ("a*b", "abb");
  should_not_match ("a*b", "");
  should_not_match ("a*b", "");
  should_not_match ("a*b", "abc");
  puts ("");
}
#endif


/* Filters.  */

static struct pattern *filter_pattern;

/* Set the current filter pattern.  If PATTERN is null, unset the
   current filter pattern instead.  */

static void
set_filter_pattern (const char *pattern)
{
  if (sample_profiler_running)
    block_sigprof ();

  if (filter_pattern)
    {
      free_pattern (filter_pattern);
      filter_pattern = 0;
    }
  if (pattern)
    filter_pattern = parse_pattern (pattern);

  if (sample_profiler_running)
    unblock_sigprof ();
}

/* Return true if the current filter pattern is matched with FUNCTION.
   FUNCTION should be a symbol or a subroutine, otherwise return
   false.  */

static int
apply_filter_1 (Lisp_Object function)
{
  const char *name;

  if (!filter_pattern)
    return 1;

  if (SYMBOLP (function))
    name = SDATA (SYMBOL_NAME (function));
  else if (SUBRP (function))
    name = XSUBR (function)->symbol_name;
  else
    return 0;

  return pattern_match (filter_pattern, name);
}

/* Return true if the current filter pattern is matched with at least
   one entry in BACKLIST.  */

static int
apply_filter (struct backtrace *backlist)
{
  while (backlist)
    {
      if (apply_filter_1 (*backlist->function))
	return 1;
      backlist = backlist->next;
    }
  return 0;
}

DEFUN ("profiler-set-filter-pattern",
       Fprofiler_set_filter_pattern, Sprofiler_set_filter_pattern,
       1, 1, "sPattern: ",
       doc: /* Set the current filter pattern.  PATTERN can contain
one or two wildcards (*) as follows:

- foo
- *foo
- foo*
- *foo*
- foo*bar

If PATTERN is nil or an empty string, then unset the current filter
pattern.  */)
  (Lisp_Object pattern)
{
  if (NILP (pattern)
      || (STRINGP (pattern) && !SREF (pattern, 0)))
    {
      set_filter_pattern (0);
      message ("Profiler filter pattern unset");
      return Qt;
    }
  else if (!STRINGP (pattern))
    error ("Invalid type of profiler filter pattern");

  set_filter_pattern (SDATA (pattern));

  return Qt;
}


/* Backtraces.  */


static Lisp_Object
make_backtrace (int size)
{
  return Fmake_vector (make_number (size), Qnil);
}

static EMACS_UINT
backtrace_hash (Lisp_Object backtrace)
{
  int i;
  EMACS_UINT hash = 0;
  for (i = 0; i < ASIZE (backtrace); i++)
    /* FIXME */
    hash = SXHASH_COMBINE (XUINT (AREF (backtrace, i)), hash);
  return hash;
}

static int
backtrace_equal (Lisp_Object a, Lisp_Object b)
{
  int i, j;

  for (i = 0, j = 0;; i++, j++)
    {
      Lisp_Object x = i < ASIZE (a) ? AREF (a, i) : Qnil;
      Lisp_Object y = j < ASIZE (b) ? AREF (b, j) : Qnil;
      if (NILP (x) && NILP (y))
	break;
      else if (!EQ (x, y))
	return 0;
    }

  return 1;
}

static Lisp_Object
backtrace_object_1 (Lisp_Object backtrace, int i)
{
  if (i >= ASIZE (backtrace) || NILP (AREF (backtrace, i)))
    return Qnil;
  else
    return Fcons (AREF (backtrace, i), backtrace_object_1 (backtrace, i + 1));
}

/* Convert BACKTRACE to a list.  */

static Lisp_Object
backtrace_object (Lisp_Object backtrace)
{
  backtrace_object_1 (backtrace, 0);
}


/* Slots.  */

/* Slot data structure.  */

struct slot
{
  /* Point to next free slot or next hash table link.  */
  struct slot *next;
  /* Point to previous hash table link.  */
  struct slot *prev;
  /* Backtrace object with fixed size.  */
  Lisp_Object backtrace;
  /* How many times a profiler sees the slot, or how much resouce
     allocated during profiling.  */
  size_t count;
  /* How long the slot takes to execute.  */
  size_t elapsed;
  /* True in used.  */
  unsigned char used : 1;
};

static void
mark_slot (struct slot *slot)
{
  mark_object (slot->backtrace);
}

/* Convert SLOT to a list.  */

static Lisp_Object
slot_object (struct slot *slot)
{
  return list3 (backtrace_object (slot->backtrace),
		make_number (slot->count),
		make_number (slot->elapsed));
}



/* Slot heaps.  */

struct slot_heap
{
  /* Number of slots allocated to the heap.  */
  unsigned int size;
  /* Actual data area.  */
  struct slot *data;
  /* Free list.  */
  struct slot *free_list;
};

static void
clear_slot_heap (struct slot_heap *heap)
{
  int i;
  struct slot *data;
  struct slot *free_list;

  data = heap->data;

  /* Mark all slots unsused.  */
  for (i = 0; i < heap->size; i++)
    data[i].used = 0;

  /* Rebuild a free list.  */
  free_list = heap->free_list = heap->data;
  for (i = 1; i < heap->size; i++)
    {
      free_list->next = &data[i];
      free_list = free_list->next;
    }
  free_list->next = 0;
}

/* Make a slot heap with SIZE.  MAX_STACK_DEPTH is a fixed size of
   allocated slots.  */

static struct slot_heap *
make_slot_heap (unsigned int size, int max_stack_depth)
{
  int i;
  struct slot_heap *heap;
  struct slot *data;

  data = (struct slot *) xmalloc (sizeof (struct slot) * size);
  for (i = 0; i < size; i++)
    data[i].backtrace = make_backtrace (max_stack_depth);

  heap = (struct slot_heap *) xmalloc (sizeof (struct slot_heap));
  heap->size = size;
  heap->data = data;
  clear_slot_heap (heap);

  return heap;
}

static void
free_slot_heap (struct slot_heap *heap)
{
  int i;
  struct slot *data = heap->data;
  for (i = 0; i < heap->size; i++)
    data[i].backtrace = Qnil;
  xfree (data);
  xfree (heap);
}

static void
mark_slot_heap (struct slot_heap *heap)
{
  int i;
  for (i = 0; i < heap->size; i++)
    mark_slot (&heap->data[i]);
}

/* Allocate one slot from HEAP.  Return 0 if no free slot in HEAP.  */

static struct slot *
allocate_slot (struct slot_heap *heap)
{
  struct slot *slot;
  if (!heap->free_list)
    return 0;
  slot = heap->free_list;
  slot->count = 0;
  slot->elapsed = 0;
  slot->used = 1;
  heap->free_list = heap->free_list->next;
  return slot;
}

static void
free_slot (struct slot_heap *heap, struct slot *slot)
{
  eassert (slot->used);
  slot->used = 0;
  slot->next = heap->free_list;
  heap->free_list = slot;
}

/* Return a minimal slot from HEAP.  "Minimal" means that such a slot
   is meaningless for profiling.  */

static struct slot *
min_slot (struct slot_heap *heap)
{
  int i;
  struct slot *min = 0;
  for (i = 0; i < heap->size; i++)
    {
      struct slot *slot = &heap->data[i];
      if (!min || (slot->used && slot->count < min->count))
	min = slot;
    }
  return min;
}


/* Slot hash tables.  */

struct slot_table
{
  /* Number of slot buckets.  */
  unsigned int size;
  /* Buckets data area.  */
  struct slot **data;
};

static void
clear_slot_table (struct slot_table *table)
{
  int i;
  for (i = 0; i < table->size; i++)
    table->data[i] = 0;
}

static struct slot_table *
make_slot_table (int size)
{
  struct slot_table *table
    = (struct slot_table *) xmalloc (sizeof (struct slot_table));
  table->size = size;
  table->data = (struct slot **) xmalloc (sizeof (struct slot *) * size);
  clear_slot_table (table);
  return table;
}

static void
free_slot_table (struct slot_table *table)
{
  xfree (table->data);
  xfree (table);
}

static void
remove_slot (struct slot_table *table, struct slot *slot)
{
  if (slot->prev)
    slot->prev->next = slot->next;
  else
    {
      EMACS_UINT hash = backtrace_hash (slot->backtrace);
      table->data[hash % table->size] = slot->next;
    }
  if (slot->next)
    slot->next->prev = slot->prev;
}


/* Logs.  */

struct log
{
  /* Type of log in symbol.  `sample' or `memory'.  */
  Lisp_Object type;
  /* Backtrace for working.  */
  Lisp_Object backtrace;
  struct slot_heap *slot_heap;
  struct slot_table *slot_table;
  size_t others_count;
  size_t others_elapsed;
};

static struct log *
make_log (const char *type, int heap_size, int max_stack_depth)
{
  struct log *log =
    (struct log *) xmalloc (sizeof (struct log));
  log->type = intern (type);
  log->backtrace = make_backtrace (max_stack_depth);
  log->slot_heap = make_slot_heap (heap_size, max_stack_depth);
  /* Number of buckets of hash table will be 10% of HEAP_SIZE.  */
  log->slot_table = make_slot_table (max (256, heap_size) / 10);
  log->others_count = 0;
  log->others_elapsed = 0;
  return log;
}

static void
free_log (struct log *log)
{
  log->backtrace = Qnil;
  free_slot_heap (log->slot_heap);
  free_slot_table (log->slot_table);
}

static void
mark_log (struct log *log)
{
  mark_object (log->type);
  mark_object (log->backtrace);
  mark_slot_heap (log->slot_heap);
}

static void
clear_log (struct log *log)
{
  clear_slot_heap (log->slot_heap);
  clear_slot_table (log->slot_table);
  log->others_count = 0;
  log->others_elapsed = 0;
}

/* Evint SLOT from LOG and accumulate the slot counts into others
   counts.  */

static void
evict_slot (struct log *log, struct slot *slot)
{
  log->others_count += slot->count;
  log->others_elapsed += slot->elapsed;
  remove_slot (log->slot_table, slot);
  free_slot (log->slot_heap, slot);
}

/* Evict a minimal slot from LOG.  */

static void
evict_min_slot (struct log *log)
{
  struct slot *min = min_slot (log->slot_heap);
  if (min)
    evict_slot (log, min);
}

/* Allocate a new slot for BACKTRACE from LOG.  The returen value must
   be a valid pointer to the slot.  */

static struct slot *
new_slot (struct log *log, Lisp_Object backtrace)
{
  int i;
  struct slot *slot = allocate_slot (log->slot_heap);

  /* If failed to allocate a slot, free some slots to make a room in
     heap.  */
  if (!slot)
    {
      evict_min_slot (log);
      slot = allocate_slot (log->slot_heap);
      /* Must be allocated.  */
      eassert (slot);
    }

  slot->prev = 0;
  slot->next = 0;

  /* Assign BACKTRACE to the slot.  */
  for (i = 0; i < ASIZE (backtrace); i++)
    ASET (slot->backtrace, i, AREF (backtrace, i));

  return slot;
}

/* Make sure that a slot for BACKTRACE is in LOG and return the
   slot. The return value must be a valid pointer to the slot.  */

static struct slot *
ensure_slot (struct log *log, Lisp_Object backtrace)
{
  EMACS_UINT hash = backtrace_hash (backtrace);
  int index = hash % log->slot_table->size;
  struct slot *slot = log->slot_table->data[index];
  struct slot *prev = slot;

  /* Looking up in hash table bucket.  */
  while (slot)
    {
      if (backtrace_equal (backtrace, slot->backtrace))
	goto found;
      prev = slot;
      slot = slot->next;
    }

  /* If not found, allocate a new slot for BACKTRACE from LOG and link
     it with bucket chain.  */
  slot = new_slot (log, backtrace);
  if (prev)
    {
      slot->prev = prev;
      prev->next = slot;
    }
  else
    log->slot_table->data[index] = slot;

 found:
  return slot;
}

/* Record the current backtrace in LOG. BASE is a special name for
   describing which the backtrace come from. BASE can be nil. COUNT is
   a number how many times the profiler sees the backtrace at the
   time.  ELAPSED is a elapsed time in millisecond that the backtrace
   took.  */

static void
record_backtrace_under (struct log *log, Lisp_Object base,
			size_t count, size_t elapsed)
{
  int i = 0;
  Lisp_Object backtrace = log->backtrace;
  struct backtrace *backlist = backtrace_list;

  /* First of all, apply filter on the bactkrace.  */
  if (!apply_filter (backlist)) return;

  /* Record BASE if necessary.  */
  if (!NILP (base) && ASIZE (backtrace) > 0)
    ASET (backtrace, i++, base);

  /* Copy the backtrace contents into working memory.  */
  for (; i < ASIZE (backtrace) && backlist; backlist = backlist->next)
    {
      Lisp_Object function = *backlist->function;
      if (FUNCTIONP (function))
	ASET (backtrace, i++, function);
    }
  /* Make sure that unused space of working memory is filled with
     nil.  */
  for (; i < ASIZE (backtrace); i++)
    ASET (backtrace, i, Qnil);

  /* If the backtrace is not empty, */
  if (!NILP (AREF (backtrace, 0)))
    {
      /* then record counts.  */
      struct slot *slot = ensure_slot (log, backtrace);
      slot->count += count;
      slot->elapsed += elapsed;
    }
}

static void
record_backtrace (struct log *log, size_t count, size_t elapsed)
{
  record_backtrace_under (log, Qnil, count, elapsed);
}

/* Convert LOG to a list.  */

static Lisp_Object
log_object (struct log *log)
{
  int i;
  Lisp_Object slots = Qnil;

  if (log->others_count != 0 || log->others_elapsed != 0)
    {
      /* Add others slot.  */
      Lisp_Object others_slot
	= list3 (list1 (Qt),
		 make_number (log->others_count),
		 make_number (log->others_elapsed));
      slots = list1 (others_slot);
    }

  for (i = 0; i < log->slot_heap->size; i++)
    {
      struct slot *s = &log->slot_heap->data[i];
      if (s->used)
	{
	  Lisp_Object slot = slot_object (s);
	  slots = Fcons (slot, slots);
	}
    }

  return list4 (log->type, Qnil, Fcurrent_time (), slots);
}


/* Sample profiler.  */

static struct log *sample_log;

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

  if (!sample_log)
    sample_log = make_log ("sample",
			   profiler_slot_heap_size,
			   profiler_max_stack_depth);

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

DEFUN ("sample-profiler-reset", Fsample_profiler_reset, Ssample_profiler_reset,
       0, 0, 0,
       doc: /* Clear sample profiler log.  */)
  (void)
{
  if (sample_log)
    {
      if (sample_profiler_running)
	{
	  block_sigprof ();
	  clear_log (sample_log);
	  unblock_sigprof ();
	}
      else
	{
	  free_log (sample_log);
	  sample_log = 0;
	}
    }
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
  int i;
  Lisp_Object result = Qnil;

  if (sample_log)
    {
      if (sample_profiler_running)
	{
	  block_sigprof ();
	  result = log_object (sample_log);
	  unblock_sigprof ();
	}
      else
	result = log_object (sample_log);
    }

  return result;
}


/* Memory profiler.  */

static struct log *memory_log;

DEFUN ("memory-profiler-start", Fmemory_profiler_start, Smemory_profiler_start,
       0, 0, 0,
       doc: /* Start/restart memory profiler.  See also
`profiler-slot-heap-size' and `profiler-max-stack-depth'.  */)
  (void)
{
  if (memory_profiler_running)
    error ("Memory profiler is already running");

  if (!memory_log)
    memory_log = make_log ("memory",
			   profiler_slot_heap_size,
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

DEFUN ("memory-profiler-reset",
       Fmemory_profiler_reset, Smemory_profiler_reset,
       0, 0, 0,
       doc: /* Clear memory profiler log.  */)
  (void)
{
  if (memory_log)
    {
      if (memory_profiler_running)
	clear_log (memory_log);
      else
	{
	  free_log (memory_log);
	  memory_log = 0;
	}
    }
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
  Lisp_Object result = Qnil;

  if (memory_log)
    result = log_object (memory_log);

  return result;
}


/* Signals and probes.  */

/* Signal handler for sample profiler.  */

static void
sigprof_handler (int signal, siginfo_t *info, void *ctx)
{
  if (!is_in_trace && sample_log)
    record_backtrace (sample_log, 1, current_sample_interval);
}

static void
block_sigprof (void)
{
  sigset_t sigset;
  sigemptyset (&sigset);
  sigaddset (&sigset, SIGPROF);
  sigprocmask (SIG_BLOCK, &sigset, 0);
}

static void
unblock_sigprof (void)
{
  sigset_t sigset;
  sigemptyset (&sigset);
  sigaddset (&sigset, SIGPROF);
  sigprocmask (SIG_UNBLOCK, &sigset, 0);
}

/* Record that the current backtrace allocated SIZE bytes.  */

void
malloc_probe (size_t size)
{
  if (memory_log)
    record_backtrace (memory_log, size, 0);
}

/* Record that GC happened in the current backtrace.  */

void
gc_probe (size_t size, size_t elapsed)
{
  if (sample_log)
    record_backtrace_under (sample_log, Qgc, 1, elapsed);
  if (memory_log)
    record_backtrace_under (memory_log, Qgc, size, elapsed);
}



void
mark_profiler (void)
{
  if (sample_log)
    {
      if (sample_profiler_running)
	{
	  block_sigprof ();
          mark_log (sample_log);
	  unblock_sigprof ();
	}
      else
	mark_log (sample_log);	
    }
  if (memory_log)
    mark_log (memory_log);
}

void
syms_of_profiler (void)
{
  DEFSYM (Qgc, "gc");

  DEFVAR_INT ("profiler-max-stack-depth", profiler_max_stack_depth,
	      doc: /* FIXME */);
  profiler_max_stack_depth = 16;
  DEFVAR_INT ("profiler-slot-heap-size", profiler_slot_heap_size,
	      doc: /* FIXME */);
  profiler_slot_heap_size = 10000;

  defsubr (&Sprofiler_set_filter_pattern);

  defsubr (&Ssample_profiler_start);
  defsubr (&Ssample_profiler_stop);
  defsubr (&Ssample_profiler_reset);
  defsubr (&Ssample_profiler_running_p);
  defsubr (&Ssample_profiler_log);

  defsubr (&Smemory_profiler_start);
  defsubr (&Smemory_profiler_stop);
  defsubr (&Smemory_profiler_reset);
  defsubr (&Smemory_profiler_running_p);
  defsubr (&Smemory_profiler_log);
}
