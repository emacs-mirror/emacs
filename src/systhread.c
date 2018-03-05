/* System thread definitions
Copyright (C) 2012-2018 Free Software Foundation, Inc.

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

#ifdef HAVE_NS
#include "nsterm.h"
#endif

#ifndef THREADS_ENABLED

void
sys_mutex_init (sys_mutex_t *m)
{
  *m = 0;
}

void
sys_mutex_lock (sys_mutex_t *m)
{
}

void
sys_mutex_unlock (sys_mutex_t *m)
{
}

void
sys_cond_init (sys_cond_t *c)
{
  *c = 0;
}

void
sys_cond_wait (sys_cond_t *c, sys_mutex_t *m)
{
}

void
sys_cond_signal (sys_cond_t *c)
{
}

void
sys_cond_broadcast (sys_cond_t *c)
{
}

void
sys_cond_destroy (sys_cond_t *c)
{
}

sys_thread_t
sys_thread_self (void)
{
  return 0;
}

bool
sys_thread_equal (sys_thread_t t, sys_thread_t u)
{
  return t == u;
}

int
sys_thread_create (sys_thread_t *t, const char *name,
		   thread_creation_function *func, void *datum)
{
  return 0;
}

void
sys_thread_yield (void)
{
}

#elif defined (HAVE_PTHREAD)

#include <sched.h>

#ifdef HAVE_SYS_PRCTL_H
#include <sys/prctl.h>
#endif

void
sys_mutex_init (sys_mutex_t *mutex)
{
  pthread_mutex_init (mutex, NULL);
}

void
sys_mutex_lock (sys_mutex_t *mutex)
{
  pthread_mutex_lock (mutex);
}

void
sys_mutex_unlock (sys_mutex_t *mutex)
{
  pthread_mutex_unlock (mutex);
}

void
sys_cond_init (sys_cond_t *cond)
{
  pthread_cond_init (cond, NULL);
}

void
sys_cond_wait (sys_cond_t *cond, sys_mutex_t *mutex)
{
  pthread_cond_wait (cond, mutex);
}

void
sys_cond_signal (sys_cond_t *cond)
{
  pthread_cond_signal (cond);
}

void
sys_cond_broadcast (sys_cond_t *cond)
{
  pthread_cond_broadcast (cond);
#ifdef HAVE_NS
  /* Send an app defined event to break out of the NS run loop.
     It seems that if ns_select is running the NS run loop, this
     broadcast has no effect until the loop is done, breaking a couple
     of tests in thread-tests.el. */
  ns_run_loop_break ();
#endif
}

void
sys_cond_destroy (sys_cond_t *cond)
{
  pthread_cond_destroy (cond);
}

sys_thread_t
sys_thread_self (void)
{
  return pthread_self ();
}

bool
sys_thread_equal (sys_thread_t t, sys_thread_t u)
{
  return pthread_equal (t, u);
}

int
sys_thread_create (sys_thread_t *thread_ptr, const char *name,
		   thread_creation_function *func, void *arg)
{
  pthread_attr_t attr;
  int result = 0;

  if (pthread_attr_init (&attr))
    return 0;

  /* Avoid crash on macOS with deeply nested GC (Bug#30364).  */
  size_t stack_size;
  size_t required_stack_size = sizeof (void *) * 1024 * 1024;
  if (pthread_attr_getstacksize (&attr, &stack_size) == 0
      && stack_size < required_stack_size)
    pthread_attr_setstacksize (&attr, required_stack_size);

  if (!pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED))
    {
      result = pthread_create (thread_ptr, &attr, func, arg) == 0;
#if defined (HAVE_SYS_PRCTL_H) && defined (HAVE_PRCTL) && defined (PR_SET_NAME)
      if (result && name != NULL)
	prctl (PR_SET_NAME, name);
#endif
    }

  pthread_attr_destroy (&attr);

  return result;
}

void
sys_thread_yield (void)
{
  sched_yield ();
}

#elif defined (WINDOWSNT)

#include <w32term.h>

/* Cannot include <process.h> because of the local header by the same
   name, sigh.  */
uintptr_t _beginthread (void (__cdecl *)(void *), unsigned, void *);

/* Mutexes are implemented as critical sections, because they are
   faster than Windows mutex objects (implemented in userspace), and
   satisfy the requirements, since we only need to synchronize within a
   single process.  */
void
sys_mutex_init (sys_mutex_t *mutex)
{
  InitializeCriticalSection ((LPCRITICAL_SECTION)mutex);
}

void
sys_mutex_lock (sys_mutex_t *mutex)
{
  /* FIXME: What happens if the owning thread exits without releasing
     the mutex?  According to MSDN, the result is undefined behavior.  */
  EnterCriticalSection ((LPCRITICAL_SECTION)mutex);
}

void
sys_mutex_unlock (sys_mutex_t *mutex)
{
  LeaveCriticalSection ((LPCRITICAL_SECTION)mutex);
}

void
sys_cond_init (sys_cond_t *cond)
{
  cond->initialized = false;
  cond->wait_count = 0;
  /* Auto-reset event for signal.  */
  cond->events[CONDV_SIGNAL] = CreateEvent (NULL, FALSE, FALSE, NULL);
  /* Manual-reset event for broadcast.  */
  cond->events[CONDV_BROADCAST] = CreateEvent (NULL, TRUE, FALSE, NULL);
  if (!cond->events[CONDV_SIGNAL] || !cond->events[CONDV_BROADCAST])
    return;
  InitializeCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);
  cond->initialized = true;
}

void
sys_cond_wait (sys_cond_t *cond, sys_mutex_t *mutex)
{
  DWORD wait_result;
  bool last_thread_waiting;

  if (!cond->initialized)
    return;

  /* Increment the wait count avoiding race conditions.  */
  EnterCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);
  cond->wait_count++;
  LeaveCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);

  /* Release the mutex and wait for either the signal or the broadcast
     event.  */
  LeaveCriticalSection ((LPCRITICAL_SECTION)mutex);
  wait_result = WaitForMultipleObjects (2, cond->events, FALSE, INFINITE);

  /* Decrement the wait count and see if we are the last thread
     waiting on the condition variable.  */
  EnterCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);
  cond->wait_count--;
  last_thread_waiting =
    wait_result == WAIT_OBJECT_0 + CONDV_BROADCAST
    && cond->wait_count == 0;
  LeaveCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);

  /* Broadcast uses a manual-reset event, so when the last thread is
     released, we must manually reset that event.  */
  if (last_thread_waiting)
    ResetEvent (cond->events[CONDV_BROADCAST]);

  /* Per the API, re-acquire the mutex.  */
  EnterCriticalSection ((LPCRITICAL_SECTION)mutex);
}

void
sys_cond_signal (sys_cond_t *cond)
{
  bool threads_waiting;

  if (!cond->initialized)
    return;

  EnterCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);
  threads_waiting = cond->wait_count > 0;
  LeaveCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);

  if (threads_waiting)
    SetEvent (cond->events[CONDV_SIGNAL]);
}

void
sys_cond_broadcast (sys_cond_t *cond)
{
  bool threads_waiting;

  if (!cond->initialized)
    return;

  EnterCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);
  threads_waiting = cond->wait_count > 0;
  LeaveCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);

  if (threads_waiting)
    SetEvent (cond->events[CONDV_BROADCAST]);
}

void
sys_cond_destroy (sys_cond_t *cond)
{
  if (cond->events[CONDV_SIGNAL])
    CloseHandle (cond->events[CONDV_SIGNAL]);
  if (cond->events[CONDV_BROADCAST])
    CloseHandle (cond->events[CONDV_BROADCAST]);

  if (!cond->initialized)
    return;

  /* FIXME: What if wait_count is non-zero, i.e. there are still
     threads waiting on this condition variable?  */
  DeleteCriticalSection ((LPCRITICAL_SECTION)&cond->wait_count_lock);
}

sys_thread_t
sys_thread_self (void)
{
  return (sys_thread_t) GetCurrentThreadId ();
}

bool
sys_thread_equal (sys_thread_t t, sys_thread_t u)
{
  return t == u;
}

static thread_creation_function *thread_start_address;

/* _beginthread wants a void function, while we are passed a function
   that returns a pointer.  So we use a wrapper.  See the command in
   w32term.h about the need for ALIGN_STACK attribute.  */
static void ALIGN_STACK
w32_beginthread_wrapper (void *arg)
{
  (void)thread_start_address (arg);
}

int
sys_thread_create (sys_thread_t *thread_ptr, const char *name,
		   thread_creation_function *func, void *arg)
{
  /* FIXME: Do threads that run Lisp require some minimum amount of
     stack?  Zero here means each thread will get the same amount as
     the main program.  On GNU/Linux, it seems like the stack is 2MB
     by default, overridden by RLIMIT_STACK at program start time.
     Not sure what to do with this.  See also the comment in
     w32proc.c:new_child.  */
  const unsigned stack_size = 0;
  uintptr_t thandle;

  thread_start_address = func;

  /* We use _beginthread rather than CreateThread because the former
     arranges for the thread handle to be automatically closed when
     the thread exits, thus preventing handle leaks and/or the need to
     track all the threads and close their handles when they exit.
     Also, MSDN seems to imply that code which uses CRT _must_ call
     _beginthread, although if that is true, we already violate that
     rule in many places...  */
  thandle = _beginthread (w32_beginthread_wrapper, stack_size, arg);
  if (thandle == (uintptr_t)-1L)
    return 0;

  /* Kludge alert!  We use the Windows thread ID, an unsigned 32-bit
     number, as the sys_thread_t type, because that ID is the only
     unique identifier of a thread on Windows.  But _beginthread
     returns a handle of the thread, and there's no easy way of
     getting the thread ID given a handle (GetThreadId is available
     only since Vista, so we cannot use it portably).  Fortunately,
     the value returned by sys_thread_create is not used by its
     callers; instead, run_thread, which runs in the context of the
     new thread, calls sys_thread_self and uses its return value;
     sys_thread_self in this implementation calls GetCurrentThreadId.
     Therefore, we return some more or less arbitrary value of the
     thread ID from this function. */
  *thread_ptr = thandle & 0xFFFFFFFF;
  return 1;
}

void
sys_thread_yield (void)
{
  Sleep (0);
}

#else

#error port me

#endif
