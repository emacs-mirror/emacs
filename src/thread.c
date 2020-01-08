/* Threading code.
Copyright (C) 2012-2020 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


#include <config.h>
#include <setjmp.h>
#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "process.h"
#include "coding.h"
#include "syssignal.h"
#include "pdumper.h"
#include "keyboard.h"

union aligned_thread_state
{
  struct thread_state s;
  GCALIGNED_UNION_MEMBER
};
verify (GCALIGNED (union aligned_thread_state));

static union aligned_thread_state main_thread
  = {{
      .header.size = PVECHEADERSIZE (PVEC_THREAD,
				     PSEUDOVECSIZE (struct thread_state,
						    event_object),
				     VECSIZE (struct thread_state)),
      .m_last_thing_searched = LISPSYM_INITIALLY (Qnil),
      .m_saved_last_thing_searched = LISPSYM_INITIALLY (Qnil),
      .name = LISPSYM_INITIALLY (Qnil),
      .function = LISPSYM_INITIALLY (Qnil),
      .result = LISPSYM_INITIALLY (Qnil),
      .error_symbol = LISPSYM_INITIALLY (Qnil),
      .error_data = LISPSYM_INITIALLY (Qnil),
      .event_object = LISPSYM_INITIALLY (Qnil),
    }};

struct thread_state *current_thread = &main_thread.s;

static struct thread_state *all_threads = &main_thread.s;

static sys_mutex_t global_lock;

extern volatile int interrupt_input_blocked;



/* m_specpdl is set when the thread is created and cleared when the
   thread dies.  */
#define thread_live_p(STATE) ((STATE)->m_specpdl != NULL)



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

       /* Set the new thread's current buffer.  This needs to be done
	  even if it is the same buffer as that of the previous thread,
	  because of thread-local bindings.  */
      set_buffer_internal_2 (current_buffer);
    }

   /* We could have been signaled while waiting to grab the global lock
      for the first time since this thread was created, in which case
      we didn't yet have the opportunity to set up the handlers.  Delay
      raising the signal in that case (it will be actually raised when
      the thread comes here after acquiring the lock the next time).  */
  if (!NILP (current_thread->error_symbol) && handlerlist)
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

/* This is called from keyboard.c when it detects that SIGINT was
   delivered to the main thread and interrupted thread_select before
   the main thread could acquire the lock.  We must acquire the lock
   to prevent a thread from running without holding the global lock,
   and to avoid repeated calls to sys_mutex_unlock, which invokes
   undefined behavior.  */
void
maybe_reacquire_global_lock (void)
{
  /* SIGINT handler is always run on the main thread, see
     deliver_process_signal, so reflect that in our thread-tracking
     variables.  */
  current_thread = &main_thread.s;

  if (current_thread->not_holding_lock)
    {
      struct thread_state *self = current_thread;

      acquire_global_lock (self);
      current_thread->not_holding_lock = 0;
    }
}



static void
lisp_mutex_init (lisp_mutex_t *mutex)
{
  mutex->owner = NULL;
  mutex->count = 0;
  sys_cond_init (&mutex->condition);
}

/* Lock MUTEX for thread LOCKER, setting its lock count to COUNT, if
   non-zero, or to 1 otherwise.

   If MUTEX is locked by LOCKER, COUNT must be zero, and the MUTEX's
   lock count will be incremented.

   If MUTEX is locked by another thread, this function will release
   the global lock, giving other threads a chance to run, and will
   wait for the MUTEX to become unlocked; when MUTEX becomes unlocked,
   and will then re-acquire the global lock.

   Return value is 1 if the function waited for the MUTEX to become
   unlocked (meaning other threads could have run during the wait),
   zero otherwise.  */
static int
lisp_mutex_lock_for_thread (lisp_mutex_t *mutex, struct thread_state *locker,
			    int new_count)
{
  struct thread_state *self;

  if (mutex->owner == NULL)
    {
      mutex->owner = locker;
      mutex->count = new_count == 0 ? 1 : new_count;
      return 0;
    }
  if (mutex->owner == locker)
    {
      eassert (new_count == 0);
      ++mutex->count;
      return 0;
    }

  self = locker;
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
lisp_mutex_lock (lisp_mutex_t *mutex, int new_count)
{
  return lisp_mutex_lock_for_thread (mutex, current_thread, new_count);
}

/* Decrement MUTEX's lock count.  If the lock count becomes zero after
   decrementing it, meaning the mutex is now unlocked, broadcast that
   to all the threads that might be waiting to lock the mutex.  This
   function signals an error if MUTEX is locked by a thread other than
   the current one.  Return value is 1 if the mutex becomes unlocked,
   zero otherwise.  */
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

/* Like lisp_mutex_unlock, but sets MUTEX's lock count to zero
   regardless of its value.  Return the previous lock count.  */
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
  if (!NILP (name))
    CHECK_STRING (name);

  struct Lisp_Mutex *mutex
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_Mutex, name, PVEC_MUTEX);
  mutex->name = name;
  lisp_mutex_init (&mutex->mutex);

  Lisp_Object result;
  XSETMUTEX (result, mutex);
  return result;
}

static void
mutex_lock_callback (void *arg)
{
  struct Lisp_Mutex *mutex = arg;
  struct thread_state *self = current_thread;

  /* Calling lisp_mutex_lock might yield to other threads while this
     one waits for the mutex to become unlocked, so we need to
     announce us as the current thread by calling
     post_acquire_global_lock.  */
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
is signaled using `thread-signal'.
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
    post_acquire_global_lock (self); /* FIXME: is this call needed? */
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
  CHECK_MUTEX (mutex);
  if (!NILP (name))
    CHECK_STRING (name);

  struct Lisp_CondVar *condvar
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_CondVar, name, PVEC_CONDVAR);
  condvar->mutex = mutex;
  condvar->name = name;
  sys_cond_init (&condvar->cond);

  Lisp_Object result;
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
  /* If signaled while unlocking, skip the wait but reacquire the lock.  */
  if (NILP (self->error_symbol))
    {
      self->wait_condvar = &cvar->cond;
      /* This call could switch to another thread.  */
      sys_cond_wait (&cvar->cond, &global_lock);
      self->wait_condvar = NULL;
    }
  self->event_object = Qnil;
  /* Since sys_cond_wait could switch threads, we need to lock the
     mutex for the thread which was the current when we were called,
     otherwise lisp_mutex_lock will record the wrong thread as the
     owner of the mutex lock.  */
  lisp_mutex_lock_for_thread (&mutex->mutex, self, saved_count);
  /* Calling lisp_mutex_lock_for_thread might yield to other threads
     while this one waits for the mutex to become unlocked, so we need
     to announce us as the current thread by calling
     post_acquire_global_lock.  */
  post_acquire_global_lock (self);
}

DEFUN ("condition-wait", Fcondition_wait, Scondition_wait, 1, 1, 0,
       doc: /* Wait for the condition variable COND to be notified.
COND is the condition variable to wait on.

The mutex associated with COND must be held when this is called.
It is an error if it is not held.

This releases the mutex and waits for COND to be notified or for
this thread to be signaled with `thread-signal'.  When
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

/* Used to communicate arguments to condition_notify_callback.  */
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
  /* Calling lisp_mutex_lock might yield to other threads while this
     one waits for the mutex to become unlocked, so we need to
     announce us as the current thread by calling
     post_acquire_global_lock.  */
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
  sigset_t oldset;

  block_interrupt_signal (&oldset);
  self->not_holding_lock = 1;
  release_global_lock ();
  restore_signal_mask (&oldset);

  sa->result = (sa->func) (sa->max_fds, sa->rfds, sa->wfds, sa->efds,
			   sa->timeout, sa->sigmask);

  block_interrupt_signal (&oldset);
  /* If we were interrupted by C-g while inside sa->func above, the
     signal handler could have called maybe_reacquire_global_lock, in
     which case we are already holding the lock and shouldn't try
     taking it again, or else we will hang forever.  */
  if (self->not_holding_lock)
    {
      acquire_global_lock (self);
      self->not_holding_lock = 0;
    }
  restore_signal_mask (&oldset);
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
  /* Get the stack top now, in case mark_specpdl changes it.  */
  void const *stack_top = thread->stack_top;

  mark_specpdl (thread->m_specpdl, thread->m_specpdl_ptr);

  mark_stack (thread->m_stack_bottom, stack_top);

  for (struct handler *handler = thread->m_handlerlist;
       handler; handler = handler->next)
    {
      mark_object (handler->tag_or_ch);
      mark_object (handler->val);
    }

  if (thread->m_current_buffer)
    {
      Lisp_Object tem;
      XSETBUFFER (tem, thread->m_current_buffer);
      mark_object (tem);
    }

  /* No need to mark Lisp_Object members like m_last_thing_searched,
     as mark_threads_callback does that by calling mark_object.  */
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
unmark_main_thread (void)
{
  main_thread.s.header.size &= ~ARRAY_MARK_FLAG;
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
  ptrdiff_t count = SPECPDL_INDEX ();

  current_thread->result = Ffuncall (1, &current_thread->function);
  return unbind_to (count, Qnil);
}

static Lisp_Object last_thread_error;

static Lisp_Object
record_thread_error (Lisp_Object error_form)
{
  last_thread_error = error_form;
  return error_form;
}

static void *
run_thread (void *state)
{
  /* Make sure stack_top and m_stack_bottom are properly aligned as GC
     expects.  */
  max_align_t stack_pos;

  struct thread_state *self = state;
  struct thread_state **iter;

  self->m_stack_bottom = self->stack_top = (char *) &stack_pos;
  self->thread_id = sys_thread_self ();

  if (self->thread_name)
    sys_thread_set_name (self->thread_name);

  acquire_global_lock (self);

  /* Put a dummy catcher at top-level so that handlerlist is never NULL.
     This is important since handlerlist->nextfree holds the freelist
     which would otherwise leak every time we unwind back to top-level.   */
  handlerlist_sentinel = xzalloc (sizeof (struct handler));
  handlerlist = handlerlist_sentinel->nextfree = handlerlist_sentinel;
  struct handler *c = push_handler (Qunbound, CATCHER);
  eassert (c == handlerlist_sentinel);
  handlerlist_sentinel->nextfree = NULL;
  handlerlist_sentinel->next = NULL;

  /* It might be nice to do something with errors here.  */
  internal_condition_case (invoke_thread_function, Qt, record_thread_error);

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

  xfree (self->thread_name);

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

static void
free_search_regs (struct re_registers *regs)
{
  if (regs->num_regs != 0)
    {
      xfree (regs->start);
      xfree (regs->end);
    }
}

void
finalize_one_thread (struct thread_state *state)
{
  free_search_regs (&state->m_search_regs);
  free_search_regs (&state->m_saved_search_regs);
  sys_cond_destroy (&state->thread_condvar);
}

DEFUN ("make-thread", Fmake_thread, Smake_thread, 1, 2, 0,
       doc: /* Start a new thread and run FUNCTION in it.
When the function exits, the thread dies.
If NAME is given, it must be a string; it names the new thread.  */)
  (Lisp_Object function, Lisp_Object name)
{
  /* Can't start a thread in temacs.  */
  if (!initialized)
    emacs_abort ();

  if (!NILP (name))
    CHECK_STRING (name);

  struct thread_state *new_thread
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct thread_state, event_object,
				    PVEC_THREAD);
  new_thread->function = function;
  new_thread->name = name;
  /* Perhaps copy m_last_thing_searched from parent?  */
  new_thread->m_current_buffer = current_thread->m_current_buffer;

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

  char const *c_name = !NILP (name) ? SSDATA (ENCODE_SYSTEM (name)) : NULL;
  if (c_name)
    new_thread->thread_name = xstrdup (c_name);
  else
    new_thread->thread_name = NULL;
  sys_thread_t thr;
  if (! sys_thread_create (&thr, run_thread, new_thread))
    {
      /* Restore the previous situation.  */
      all_threads = all_threads->next_thread;
#ifdef THREADS_ENABLED
      error ("Could not start a new thread");
#else
      error ("Concurrency is not supported in this configuration");
#endif
    }

  /* FIXME: race here where new thread might not be filled in?  */
  Lisp_Object result;
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
or `thread-join' in the target thread.
If THREAD is the main thread, just the error message is shown.  */)
  (Lisp_Object thread, Lisp_Object error_symbol, Lisp_Object data)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  if (tstate == current_thread)
    Fsignal (error_symbol, data);

#ifdef THREADS_ENABLED
  if (main_thread_p (tstate))
    {
      /* Construct an event.  */
      struct input_event event;
      EVENT_INIT (event);
      event.kind = THREAD_EVENT;
      event.frame_or_window = Qnil;
      event.arg = list3 (Fcurrent_thread (), error_symbol, data);

      /* Store it into the input event queue.  */
      kbd_buffer_store_event (&event);
    }

  else
#endif
    {
      /* What to do if thread is already signaled?  */
      /* What if error_symbol is Qnil?  */
      tstate->error_symbol = error_symbol;
      tstate->error_data = data;

      if (tstate->wait_condvar)
	flush_stack_call_func (thread_signal_callback, tstate);
    }

  return Qnil;
}

DEFUN ("thread-live-p", Fthread_live_p, Sthread_live_p, 1, 1, 0,
       doc: /* Return t if THREAD is alive, or nil if it has exited.  */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  return thread_live_p (tstate) ? Qt : Qnil;
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
  while (thread_live_p (tstate) && NILP (self->error_symbol))
    sys_cond_wait (self->wait_condvar, &global_lock);

  self->wait_condvar = NULL;
  self->event_object = Qnil;
  post_acquire_global_lock (self);
}

DEFUN ("thread-join", Fthread_join, Sthread_join, 1, 1, 0,
       doc: /* Wait for THREAD to exit.
This blocks the current thread until THREAD exits or until the current
thread is signaled.  It returns the result of the THREAD function.  It
is an error for a thread to try to join itself.  */)
  (Lisp_Object thread)
{
  struct thread_state *tstate;
  Lisp_Object error_symbol, error_data;

  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  if (tstate == current_thread)
    error ("Cannot join current thread");

  error_symbol = tstate->error_symbol;
  error_data = tstate->error_data;

  if (thread_live_p (tstate))
    flush_stack_call_func (thread_join_callback, tstate);

  if (!NILP (error_symbol))
    Fsignal (error_symbol, error_data);

  return tstate->result;
}

DEFUN ("all-threads", Fall_threads, Sall_threads, 0, 0, 0,
       doc: /* Return a list of all the live threads.  */)
  (void)
{
  Lisp_Object result = Qnil;
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next_thread)
    {
      if (thread_live_p (iter))
	{
	  Lisp_Object thread;

	  XSETTHREAD (thread, iter);
	  result = Fcons (thread, result);
	}
    }

  return result;
}

DEFUN ("thread-last-error", Fthread_last_error, Sthread_last_error, 0, 1, 0,
       doc: /* Return the last error form recorded by a dying thread.
If CLEANUP is non-nil, remove this error form from history.  */)
     (Lisp_Object cleanup)
{
  Lisp_Object result = last_thread_error;

  if (!NILP (cleanup))
    last_thread_error = Qnil;

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



bool
main_thread_p (const void *ptr)
{
  return ptr == &main_thread.s;
}

bool
in_current_thread (void)
{
  if (current_thread == NULL)
    return false;
  return sys_thread_equal (sys_thread_self (), current_thread->thread_id);
}

void
init_threads (void)
{
  sys_cond_init (&main_thread.s.thread_condvar);
  sys_mutex_init (&global_lock);
  sys_mutex_lock (&global_lock);
  current_thread = &main_thread.s;
  main_thread.s.thread_id = sys_thread_self ();
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
      defsubr (&Sthread_live_p);
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
      defsubr (&Sthread_last_error);

      staticpro (&last_thread_error);
      last_thread_error = Qnil;

      Fdefalias (intern_c_string ("thread-alive-p"),
		 intern_c_string ("thread-live-p"), Qnil);

      Fprovide (intern_c_string ("threads"), Qnil);
    }

  DEFSYM (Qthreadp, "threadp");
  DEFSYM (Qmutexp, "mutexp");
  DEFSYM (Qcondition_variable_p, "condition-variable-p");

  DEFVAR_LISP ("main-thread", Vmain_thread,
    doc: /* The main thread of Emacs.  */);
#ifdef THREADS_ENABLED
  XSETTHREAD (Vmain_thread, &main_thread.s);
#else
  Vmain_thread = Qnil;
#endif
}
