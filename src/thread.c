/* Threading code.
   Copyright (C) 2012, 2013 Free Software Foundation, Inc.

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
#include <setjmp.h>
#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "process.h"
#include "coding.h"

static struct thread_state primary_thread;

struct thread_state *current_thread = &primary_thread;

static struct thread_state *all_threads = &primary_thread;

static sys_mutex_t global_lock;

extern int poll_suppress_count;
extern volatile int interrupt_input_blocked;



/* m_specpdl is set when the thread is created and cleared when the
   thread dies.  */
#define thread_alive_p(STATE) ((STATE)->m_specpdl != NULL)



static void
release_global_lock (void)
{
  sys_mutex_unlock (&global_lock);
}

/* You must call this after acquiring the global lock.
   acquire_global_lock does it for you.  */
static void
post_acquire_global_lock (struct thread_state *self)
{
  Lisp_Object buffer;
  struct thread_state *prev_thread = current_thread;

  /* Do this early on, so that code below could signal errors (e.g.,
     unbind_for_thread_switch might) correctly, because we are already
     running in the context of the thread pointed by SELF.  */
  current_thread = self;

  if (prev_thread != current_thread)
    {
      /* PREV_THREAD is NULL if the previously current thread
	 exited.  In this case, there is no reason to unbind, and
	 trying will crash.  */
      if (prev_thread != NULL)
	unbind_for_thread_switch (prev_thread);
      rebind_for_thread_switch ();
    }

  /* We need special handling to re-set the buffer.  */
  XSETBUFFER (buffer, self->m_current_buffer);
  self->m_current_buffer = 0;
  set_buffer_internal (XBUFFER (buffer));

  if (!NILP (current_thread->error_symbol))
    {
      Lisp_Object sym = current_thread->error_symbol;
      Lisp_Object data = current_thread->error_data;

      current_thread->error_symbol = Qnil;
      current_thread->error_data = Qnil;
      Fsignal (sym, data);
    }
}

static void
acquire_global_lock (struct thread_state *self)
{
  sys_mutex_lock (&global_lock);
  post_acquire_global_lock (self);
}



static void
lisp_mutex_init (lisp_mutex_t *mutex)
{
  mutex->owner = NULL;
  mutex->count = 0;
  sys_cond_init (&mutex->condition);
}

static int
lisp_mutex_lock (lisp_mutex_t *mutex, int new_count)
{
  struct thread_state *self;

  if (mutex->owner == NULL)
    {
      mutex->owner = current_thread;
      mutex->count = new_count == 0 ? 1 : new_count;
      return 0;
    }
  if (mutex->owner == current_thread)
    {
      eassert (new_count == 0);
      ++mutex->count;
      return 0;
    }

  self = current_thread;
  self->wait_condvar = &mutex->condition;
  while (mutex->owner != NULL && (new_count != 0
				  || NILP (self->error_symbol)))
    sys_cond_wait (&mutex->condition, &global_lock);
  self->wait_condvar = NULL;

  if (new_count == 0 && !NILP (self->error_symbol))
    return 1;

  mutex->owner = self;
  mutex->count = new_count == 0 ? 1 : new_count;

  return 1;
}

static int
lisp_mutex_unlock (lisp_mutex_t *mutex)
{
  if (mutex->owner != current_thread)
    error ("Cannot unlock mutex owned by another thread");

  if (--mutex->count > 0)
    return 0;

  mutex->owner = NULL;
  sys_cond_broadcast (&mutex->condition);

  return 1;
}

static unsigned int
lisp_mutex_unlock_for_wait (lisp_mutex_t *mutex)
{
  unsigned int result = mutex->count;

  /* Ensured by condvar code.  */
  eassert (mutex->owner == current_thread);

  mutex->count = 0;
  mutex->owner = NULL;
  sys_cond_broadcast (&mutex->condition);

  return result;
}

static void
lisp_mutex_destroy (lisp_mutex_t *mutex)
{
  sys_cond_destroy (&mutex->condition);
}

static int
lisp_mutex_owned_p (lisp_mutex_t *mutex)
{
  return mutex->owner == current_thread;
}



DEFUN ("make-mutex", Fmake_mutex, Smake_mutex, 0, 1, 0,
       doc: /* Create a mutex.
A mutex provides a synchronization point for threads.
Only one thread at a time can hold a mutex.  Other threads attempting
to acquire it will block until the mutex is available.

A thread can acquire a mutex any number of times.

NAME, if given, is used as the name of the mutex.  The name is
informational only.  */)
  (Lisp_Object name)
{
  struct Lisp_Mutex *mutex;
  Lisp_Object result;

  if (!NILP (name))
    CHECK_STRING (name);

  mutex = ALLOCATE_PSEUDOVECTOR (struct Lisp_Mutex, mutex, PVEC_MUTEX);
  memset ((char *) mutex + offsetof (struct Lisp_Mutex, mutex),
	  0, sizeof (struct Lisp_Mutex) - offsetof (struct Lisp_Mutex,
						    mutex));
  mutex->name = name;
  lisp_mutex_init (&mutex->mutex);

  XSETMUTEX (result, mutex);
  return result;
}

static void
mutex_lock_callback (void *arg)
{
  struct Lisp_Mutex *mutex = arg;
  struct thread_state *self = current_thread;

  if (lisp_mutex_lock (&mutex->mutex, 0))
    post_acquire_global_lock (self);
}

static void
do_unwind_mutex_lock (void)
{
  current_thread->event_object = Qnil;
}

DEFUN ("mutex-lock", Fmutex_lock, Smutex_lock, 1, 1, 0,
       doc: /* Acquire a mutex.
If the current thread already owns MUTEX, increment the count and
return.
Otherwise, if no thread owns MUTEX, make the current thread own it.
Otherwise, block until MUTEX is available, or until the current thread
is signalled using `thread-signal'.
Note that calls to `mutex-lock' and `mutex-unlock' must be paired.  */)
  (Lisp_Object mutex)
{
  struct Lisp_Mutex *lmutex;
  ptrdiff_t count = SPECPDL_INDEX ();

  CHECK_MUTEX (mutex);
  lmutex = XMUTEX (mutex);

  current_thread->event_object = mutex;
  record_unwind_protect_void (do_unwind_mutex_lock);
  flush_stack_call_func (mutex_lock_callback, lmutex);
  return unbind_to (count, Qnil);
}

static void
mutex_unlock_callback (void *arg)
{
  struct Lisp_Mutex *mutex = arg;
  struct thread_state *self = current_thread;

  if (lisp_mutex_unlock (&mutex->mutex))
    post_acquire_global_lock (self);
}

DEFUN ("mutex-unlock", Fmutex_unlock, Smutex_unlock, 1, 1, 0,
       doc: /* Release the mutex.
If this thread does not own MUTEX, signal an error.
Otherwise, decrement the mutex's count.  If the count is zero,
release MUTEX.   */)
  (Lisp_Object mutex)
{
  struct Lisp_Mutex *lmutex;

  CHECK_MUTEX (mutex);
  lmutex = XMUTEX (mutex);

  flush_stack_call_func (mutex_unlock_callback, lmutex);
  return Qnil;
}

DEFUN ("mutex-name", Fmutex_name, Smutex_name, 1, 1, 0,
       doc: /* Return the name of MUTEX.
If no name was given when MUTEX was created, return nil.  */)
  (Lisp_Object mutex)
{
  struct Lisp_Mutex *lmutex;

  CHECK_MUTEX (mutex);
  lmutex = XMUTEX (mutex);

  return lmutex->name;
}

void
finalize_one_mutex (struct Lisp_Mutex *mutex)
{
  lisp_mutex_destroy (&mutex->mutex);
}



DEFUN ("make-condition-variable",
       Fmake_condition_variable, Smake_condition_variable,
       1, 2, 0,
       doc: /* Make a condition variable associated with MUTEX.
A condition variable provides a way for a thread to sleep while
waiting for a state change.

MUTEX is the mutex associated with this condition variable.
NAME, if given, is the name of this condition variable.  The name is
informational only.  */)
  (Lisp_Object mutex, Lisp_Object name)
{
  struct Lisp_CondVar *condvar;
  Lisp_Object result;

  CHECK_MUTEX (mutex);
  if (!NILP (name))
    CHECK_STRING (name);

  condvar = ALLOCATE_PSEUDOVECTOR (struct Lisp_CondVar, cond, PVEC_CONDVAR);
  memset ((char *) condvar + offsetof (struct Lisp_CondVar, cond),
	  0, sizeof (struct Lisp_CondVar) - offsetof (struct Lisp_CondVar,
						      cond));
  condvar->mutex = mutex;
  condvar->name = name;
  sys_cond_init (&condvar->cond);

  XSETCONDVAR (result, condvar);
  return result;
}

static void
condition_wait_callback (void *arg)
{
  struct Lisp_CondVar *cvar = arg;
  struct Lisp_Mutex *mutex = XMUTEX (cvar->mutex);
  struct thread_state *self = current_thread;
  unsigned int saved_count;
  Lisp_Object cond;

  XSETCONDVAR (cond, cvar);
  self->event_object = cond;
  saved_count = lisp_mutex_unlock_for_wait (&mutex->mutex);
  /* If we were signalled while unlocking, we skip the wait, but we
     still must reacquire our lock.  */
  if (NILP (self->error_symbol))
    {
      self->wait_condvar = &cvar->cond;
      sys_cond_wait (&cvar->cond, &global_lock);
      self->wait_condvar = NULL;
    }
  lisp_mutex_lock (&mutex->mutex, saved_count);
  self->event_object = Qnil;
  post_acquire_global_lock (self);
}

DEFUN ("condition-wait", Fcondition_wait, Scondition_wait, 1, 1, 0,
       doc: /* Wait for the condition variable COND to be notified.
COND is the condition variable to wait on.

The mutex associated with COND must be held when this is called.
It is an error if it is not held.

This releases the mutex and waits for COND to be notified or for
this thread to be signalled with `thread-signal'.  When
`condition-wait' returns, COND's mutex will again be locked by
this thread.  */)
  (Lisp_Object cond)
{
  struct Lisp_CondVar *cvar;
  struct Lisp_Mutex *mutex;

  CHECK_CONDVAR (cond);
  cvar = XCONDVAR (cond);

  mutex = XMUTEX (cvar->mutex);
  if (!lisp_mutex_owned_p (&mutex->mutex))
    error ("Condition variable's mutex is not held by current thread");

  flush_stack_call_func (condition_wait_callback, cvar);

  return Qnil;
}

/* Used to communicate argumnets to condition_notify_callback.  */
struct notify_args
{
  struct Lisp_CondVar *cvar;
  int all;
};

static void
condition_notify_callback (void *arg)
{
  struct notify_args *na = arg;
  struct Lisp_Mutex *mutex = XMUTEX (na->cvar->mutex);
  struct thread_state *self = current_thread;
  unsigned int saved_count;
  Lisp_Object cond;

  XSETCONDVAR (cond, na->cvar);
  saved_count = lisp_mutex_unlock_for_wait (&mutex->mutex);
  if (na->all)
    sys_cond_broadcast (&na->cvar->cond);
  else
    sys_cond_signal (&na->cvar->cond);
  lisp_mutex_lock (&mutex->mutex, saved_count);
  post_acquire_global_lock (self);
}

DEFUN ("condition-notify", Fcondition_notify, Scondition_notify, 1, 2, 0,
       doc: /* Notify COND, a condition variable.
This wakes a thread waiting on COND.
If ALL is non-nil, all waiting threads are awoken.

The mutex associated with COND must be held when this is called.
It is an error if it is not held.

This releases COND's mutex when notifying COND.  When
`condition-notify' returns, the mutex will again be locked by this
thread.  */)
  (Lisp_Object cond, Lisp_Object all)
{
  struct Lisp_CondVar *cvar;
  struct Lisp_Mutex *mutex;
  struct notify_args args;

  CHECK_CONDVAR (cond);
  cvar = XCONDVAR (cond);

  mutex = XMUTEX (cvar->mutex);
  if (!lisp_mutex_owned_p (&mutex->mutex))
    error ("Condition variable's mutex is not held by current thread");

  args.cvar = cvar;
  args.all = !NILP (all);
  flush_stack_call_func (condition_notify_callback, &args);

  return Qnil;
}

DEFUN ("condition-mutex", Fcondition_mutex, Scondition_mutex, 1, 1, 0,
       doc: /* Return the mutex associated with condition variable COND.  */)
  (Lisp_Object cond)
{
  struct Lisp_CondVar *cvar;

  CHECK_CONDVAR (cond);
  cvar = XCONDVAR (cond);

  return cvar->mutex;
}

DEFUN ("condition-name", Fcondition_name, Scondition_name, 1, 1, 0,
       doc: /* Return the name of condition variable COND.
If no name was given when COND was created, return nil.  */)
  (Lisp_Object cond)
{
  struct Lisp_CondVar *cvar;

  CHECK_CONDVAR (cond);
  cvar = XCONDVAR (cond);

  return cvar->name;
}

void
finalize_one_condvar (struct Lisp_CondVar *condvar)
{
  sys_cond_destroy (&condvar->cond);
}



struct select_args
{
  select_func *func;
  int max_fds;
  fd_set *rfds;
  fd_set *wfds;
  fd_set *efds;
  struct timespec *timeout;
  sigset_t *sigmask;
  int result;
};

static void
really_call_select (void *arg)
{
  struct select_args *sa = arg;
  struct thread_state *self = current_thread;

  release_global_lock ();
  sa->result = (sa->func) (sa->max_fds, sa->rfds, sa->wfds, sa->efds,
			   sa->timeout, sa->sigmask);
  acquire_global_lock (self);
}

int
thread_select (select_func *func, int max_fds, fd_set *rfds,
	       fd_set *wfds, fd_set *efds, struct timespec *timeout,
	       sigset_t *sigmask)
{
  struct select_args sa;

  sa.func = func;
  sa.max_fds = max_fds;
  sa.rfds = rfds;
  sa.wfds = wfds;
  sa.efds = efds;
  sa.timeout = timeout;
  sa.sigmask = sigmask;
  flush_stack_call_func (really_call_select, &sa);
  return sa.result;
}



static void
mark_one_thread (struct thread_state *thread)
{
  struct handler *handler;
  Lisp_Object tem;

  mark_specpdl (thread->m_specpdl, thread->m_specpdl_ptr);

  mark_stack (thread->m_stack_bottom, thread->stack_top);

  for (handler = thread->m_handlerlist; handler; handler = handler->next)
    {
      mark_object (handler->tag_or_ch);
      mark_object (handler->val);
    }

  if (thread->m_current_buffer)
    {
      XSETBUFFER (tem, thread->m_current_buffer);
      mark_object (tem);
    }

  mark_object (thread->m_last_thing_searched);

  if (!NILP (thread->m_saved_last_thing_searched))
    mark_object (thread->m_saved_last_thing_searched);
}

static void
mark_threads_callback (void *ignore)
{
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    {
      Lisp_Object thread_obj;

      XSETTHREAD (thread_obj, iter);
      mark_object (thread_obj);
      mark_one_thread (iter);
    }
}

void
mark_threads (void)
{
  flush_stack_call_func (mark_threads_callback, NULL);
}

void
unmark_threads (void)
{
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    if (iter->m_byte_stack_list)
      relocate_byte_stack (iter->m_byte_stack_list);
}



static void
yield_callback (void *ignore)
{
  struct thread_state *self = current_thread;

  release_global_lock ();
  sys_thread_yield ();
  acquire_global_lock (self);
}

DEFUN ("thread-yield", Fthread_yield, Sthread_yield, 0, 0, 0,
       doc: /* Yield the CPU to another thread.  */)
     (void)
{
  flush_stack_call_func (yield_callback, NULL);
  return Qnil;
}

static Lisp_Object
invoke_thread_function (void)
{
  int count = SPECPDL_INDEX ();

  Ffuncall (1, &current_thread->function);
  return unbind_to (count, Qnil);
}

static Lisp_Object
do_nothing (Lisp_Object whatever)
{
  return whatever;
}

static void *
run_thread (void *state)
{
  char stack_pos;
  struct thread_state *self = state;
  struct thread_state **iter;

  self->m_stack_bottom = &stack_pos;
  self->stack_top = &stack_pos;
  self->thread_id = sys_thread_self ();

  acquire_global_lock (self);

  { /* Put a dummy catcher at top-level so that handlerlist is never NULL.
       This is important since handlerlist->nextfree holds the freelist
       which would otherwise leak every time we unwind back to top-level.   */
    handlerlist_sentinel = xzalloc (sizeof (struct handler));
    handlerlist = handlerlist_sentinel->nextfree = handlerlist_sentinel;
    struct handler *c = push_handler (Qunbound, CATCHER);
    eassert (c == handlerlist_sentinel);
    handlerlist_sentinel->nextfree = NULL;
    handlerlist_sentinel->next = NULL;
  }

  /* It might be nice to do something with errors here.  */
  internal_condition_case (invoke_thread_function, Qt, do_nothing);

  update_processes_for_thread_death (Fcurrent_thread ());

  xfree (self->m_specpdl - 1);
  self->m_specpdl = NULL;
  self->m_specpdl_ptr = NULL;
  self->m_specpdl_size = 0;

  {
    struct handler *c, *c_next;
    for (c = handlerlist_sentinel; c; c = c_next)
      {
	c_next = c->nextfree;
	xfree (c);
      }
  }

  current_thread = NULL;
  sys_cond_broadcast (&self->thread_condvar);

  /* Unlink this thread from the list of all threads.  Note that we
     have to do this very late, after broadcasting our death.
     Otherwise the GC may decide to reap the thread_state object,
     leading to crashes.  */
  for (iter = &all_threads; *iter != self; iter = &(*iter)->next_thread)
    ;
  *iter = (*iter)->next_thread;

  release_global_lock ();

  return NULL;
}

void
finalize_one_thread (struct thread_state *state)
{
  sys_cond_destroy (&state->thread_condvar);
}

DEFUN ("make-thread", Fmake_thread, Smake_thread, 1, 2, 0,
       doc: /* Start a new thread and run FUNCTION in it.
When the function exits, the thread dies.
If NAME is given, it must be a string; it names the new thread.  */)
  (Lisp_Object function, Lisp_Object name)
{
  sys_thread_t thr;
  struct thread_state *new_thread;
  Lisp_Object result;
  const char *c_name = NULL;
  size_t offset = offsetof (struct thread_state, m_byte_stack_list);

  /* Can't start a thread in temacs.  */
  if (!initialized)
    emacs_abort ();

  if (!NILP (name))
    CHECK_STRING (name);

  new_thread = ALLOCATE_PSEUDOVECTOR (struct thread_state, m_byte_stack_list,
				      PVEC_THREAD);
  memset ((char *) new_thread + offset, 0,
	  sizeof (struct thread_state) - offset);

  new_thread->function = function;
  new_thread->name = name;
  new_thread->m_last_thing_searched = Qnil; /* copy from parent? */
  new_thread->m_saved_last_thing_searched = Qnil;
  new_thread->m_current_buffer = current_thread->m_current_buffer;
  new_thread->error_symbol = Qnil;
  new_thread->error_data = Qnil;
  new_thread->event_object = Qnil;

  new_thread->m_specpdl_size = 50;
  new_thread->m_specpdl = xmalloc ((1 + new_thread->m_specpdl_size)
				   * sizeof (union specbinding));
  /* Skip the dummy entry.  */
  ++new_thread->m_specpdl;
  new_thread->m_specpdl_ptr = new_thread->m_specpdl;

  sys_cond_init (&new_thread->thread_condvar);

  /* We'll need locking here eventually.  */
  new_thread->next_thread = all_threads;
  all_threads = new_thread;

  if (!NILP (name))
    c_name = SSDATA (ENCODE_UTF_8 (name));

  if (! sys_thread_create (&thr, c_name, run_thread, new_thread))
    {
      /* Restore the previous situation.  */
      all_threads = all_threads->next_thread;
      error ("Could not start a new thread");
    }

  /* FIXME: race here where new thread might not be filled in?  */
  XSETTHREAD (result, new_thread);
  return result;
}

DEFUN ("current-thread", Fcurrent_thread, Scurrent_thread, 0, 0, 0,
       doc: /* Return the current thread.  */)
  (void)
{
  Lisp_Object result;
  XSETTHREAD (result, current_thread);
  return result;
}

DEFUN ("thread-name", Fthread_name, Sthread_name, 1, 1, 0,
       doc: /* Return the name of the THREAD.
The name is the same object that was passed to `make-thread'.  */)
     (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  return tstate->name;
}

static void
thread_signal_callback (void *arg)
{
  struct thread_state *tstate = arg;
  struct thread_state *self = current_thread;

  sys_cond_broadcast (tstate->wait_condvar);
  post_acquire_global_lock (self);
}

DEFUN ("thread-signal", Fthread_signal, Sthread_signal, 3, 3, 0,
       doc: /* Signal an error in a thread.
This acts like `signal', but arranges for the signal to be raised
in THREAD.  If THREAD is the current thread, acts just like `signal'.
This will interrupt a blocked call to `mutex-lock', `condition-wait',
or `thread-join' in the target thread.  */)
  (Lisp_Object thread, Lisp_Object error_symbol, Lisp_Object data)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  if (tstate == current_thread)
    Fsignal (error_symbol, data);

  /* What to do if thread is already signalled?  */
  /* What if error_symbol is Qnil?  */
  tstate->error_symbol = error_symbol;
  tstate->error_data = data;

  if (tstate->wait_condvar)
    flush_stack_call_func (thread_signal_callback, tstate);

  return Qnil;
}

DEFUN ("thread-alive-p", Fthread_alive_p, Sthread_alive_p, 1, 1, 0,
       doc: /* Return t if THREAD is alive, or nil if it has exited.  */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  return thread_alive_p (tstate) ? Qt : Qnil;
}

DEFUN ("thread--blocker", Fthread_blocker, Sthread_blocker, 1, 1, 0,
       doc: /* Return the object that THREAD is blocking on.
If THREAD is blocked in `thread-join' on a second thread, return that
thread.
If THREAD is blocked in `mutex-lock', return the mutex.
If THREAD is blocked in `condition-wait', return the condition variable.
Otherwise, if THREAD is not blocked, return nil.  */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  return tstate->event_object;
}

static void
thread_join_callback (void *arg)
{
  struct thread_state *tstate = arg;
  struct thread_state *self = current_thread;
  Lisp_Object thread;

  XSETTHREAD (thread, tstate);
  self->event_object = thread;
  self->wait_condvar = &tstate->thread_condvar;
  while (thread_alive_p (tstate) && NILP (self->error_symbol))
    sys_cond_wait (self->wait_condvar, &global_lock);

  self->wait_condvar = NULL;
  self->event_object = Qnil;
  post_acquire_global_lock (self);
}

DEFUN ("thread-join", Fthread_join, Sthread_join, 1, 1, 0,
       doc: /* Wait for THREAD to exit.
This blocks the current thread until THREAD exits or until
the current thread is signaled.
It is an error for a thread to try to join itself.  */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  if (tstate == current_thread)
    error ("Cannot join current thread");

  if (thread_alive_p (tstate))
    flush_stack_call_func (thread_join_callback, tstate);

  return Qnil;
}

DEFUN ("all-threads", Fall_threads, Sall_threads, 0, 0, 0,
       doc: /* Return a list of all the live threads.  */)
  (void)
{
  Lisp_Object result = Qnil;
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    {
      if (thread_alive_p (iter))
	{
	  Lisp_Object thread;

	  XSETTHREAD (thread, iter);
	  result = Fcons (thread, result);
	}
    }

  return result;
}



bool
thread_check_current_buffer (struct buffer *buffer)
{
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    {
      if (iter == current_thread)
	continue;

      if (iter->m_current_buffer == buffer)
	return true;
    }

  return false;
}



static void
init_primary_thread (void)
{
  primary_thread.header.size
    = PSEUDOVECSIZE (struct thread_state, m_byte_stack_list);
  XSETPVECTYPE (&primary_thread, PVEC_THREAD);
  primary_thread.m_last_thing_searched = Qnil;
  primary_thread.m_saved_last_thing_searched = Qnil;
  primary_thread.name = Qnil;
  primary_thread.function = Qnil;
  primary_thread.error_symbol = Qnil;
  primary_thread.error_data = Qnil;
  primary_thread.event_object = Qnil;
}

void
init_threads_once (void)
{
  init_primary_thread ();
}

void
init_threads (void)
{
  init_primary_thread ();
  sys_cond_init (&primary_thread.thread_condvar);
  sys_mutex_init (&global_lock);
  sys_mutex_lock (&global_lock);
  current_thread = &primary_thread;
  primary_thread.thread_id = sys_thread_self ();
}

void
syms_of_threads (void)
{
#ifndef THREADS_ENABLED
  if (0)
#endif
    {
      defsubr (&Sthread_yield);
      defsubr (&Smake_thread);
      defsubr (&Scurrent_thread);
      defsubr (&Sthread_name);
      defsubr (&Sthread_signal);
      defsubr (&Sthread_alive_p);
      defsubr (&Sthread_join);
      defsubr (&Sthread_blocker);
      defsubr (&Sall_threads);
      defsubr (&Smake_mutex);
      defsubr (&Smutex_lock);
      defsubr (&Smutex_unlock);
      defsubr (&Smutex_name);
      defsubr (&Smake_condition_variable);
      defsubr (&Scondition_wait);
      defsubr (&Scondition_notify);
      defsubr (&Scondition_mutex);
      defsubr (&Scondition_name);
    }

  DEFSYM (Qthreadp, "threadp");
  DEFSYM (Qmutexp, "mutexp");
  DEFSYM (Qcondition_variable_p, "condition-variable-p");
}
