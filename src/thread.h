/* Thread definitions
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

#ifndef THREAD_H
#define THREAD_H

#include "regex-emacs.h"

#ifdef WINDOWSNT
#include <sys/socket.h>
#endif

#ifdef MSDOS
#include <signal.h>		/* sigset_t */
#endif

#include "sysselect.h"		/* FIXME */
#include "systhread.h"

struct thread_state
{
  union vectorlike_header header;

  /* The buffer in which the last search was performed, or
     Qt if the last search was done in a string;
     Qnil if no searching has been done yet.  */
  Lisp_Object m_last_thing_searched;
#define last_thing_searched (current_thread->m_last_thing_searched)

  Lisp_Object m_saved_last_thing_searched;
#define saved_last_thing_searched (current_thread->m_saved_last_thing_searched)

  /* The thread's name.  */
  Lisp_Object name;

  /* The thread's function.  */
  Lisp_Object function;

  /* The thread's result, if function has finished.  */
  Lisp_Object result;

  /* If non-nil, this thread has been signaled.  */
  Lisp_Object error_symbol;
  Lisp_Object error_data;

  /* If we are waiting for some event, this holds the object we are
     waiting on.  */
  Lisp_Object event_object;
  /* event_object must be the last Lisp field.  */

  /* An address near the bottom of the stack.
     Tells GC how to save a copy of the stack.  */
  char const *m_stack_bottom;
#define stack_bottom (current_thread->m_stack_bottom)

  /* The address of an object near the C stack top, used to determine
     which words need to be scanned by the garbage collector.  This is
     also used to detect heuristically whether segmentation violation
     address indicates stack overflow, as opposed to some internal
     error in Emacs.  If the C function F calls G which calls H which
     calls ... F, then at least one of the functions in the chain
     should set this to the address of a local variable.  */
  void const *stack_top;

  struct catchtag *m_catchlist;
#define catchlist (current_thread->m_catchlist)

  /* Chain of condition handlers currently in effect.
     The elements of this chain are contained in the stack frames
     of Fcondition_case and internal_condition_case.
     When an error is signaled (by calling Fsignal),
     this chain is searched for an element that applies.  */
  struct handler *m_handlerlist;
#define handlerlist (current_thread->m_handlerlist)

  struct handler *m_handlerlist_sentinel;
#define handlerlist_sentinel (current_thread->m_handlerlist_sentinel)

  /* Current number of specbindings allocated in specpdl.  */
  ptrdiff_t m_specpdl_size;
#define specpdl_size (current_thread->m_specpdl_size)

  /* Pointer to beginning of specpdl.  */
  union specbinding *m_specpdl;
#define specpdl (current_thread->m_specpdl)

  /* Pointer to first unused element in specpdl.  */
  union specbinding *m_specpdl_ptr;
#define specpdl_ptr (current_thread->m_specpdl_ptr)

  /* Depth in Lisp evaluations and function calls.  */
  intmax_t m_lisp_eval_depth;
#define lisp_eval_depth (current_thread->m_lisp_eval_depth)

  /* This points to the current buffer.  */
  struct buffer *m_current_buffer;
#define current_buffer (current_thread->m_current_buffer)

  /* Every call to re_search, etc., must pass &search_regs as the regs
     argument unless you can show it is unnecessary (i.e., if re_search
     is certainly going to be called again before region-around-match
     can be called).

     Since the registers are now dynamically allocated, we need to make
     sure not to refer to the Nth register before checking that it has
     been allocated by checking search_regs.num_regs.

     The regex code keeps track of whether it has allocated the search
     buffer using bits in the re_pattern_buffer.  This means that whenever
     you compile a new pattern, it completely forgets whether it has
     allocated any registers, and will allocate new registers the next
     time you call a searching or matching function.  Therefore, we need
     to call re_set_registers after compiling a new pattern or after
     setting the match registers, so that the regex functions will be
     able to free or re-allocate it properly.  */
  struct re_registers m_search_regs;
#define search_regs (current_thread->m_search_regs)

  struct re_registers m_saved_search_regs;
#define saved_search_regs (current_thread->m_saved_search_regs)

  /* This member is different from waiting_for_input.
     It is used to communicate to a lisp process-filter/sentinel (via the
     function Fwaiting_for_user_input_p) whether Emacs was waiting
     for user-input when that process-filter was called.
     waiting_for_input cannot be used as that is by definition 0 when
     lisp code is being evalled.
     This is also used in record_asynch_buffer_change.
     For that purpose, this must be 0
     when not inside wait_reading_process_output.  */
  int m_waiting_for_user_input_p;
#define waiting_for_user_input_p (current_thread->m_waiting_for_user_input_p)

  /* True while doing kbd input.  */
  bool m_waiting_for_input;
#define waiting_for_input (current_thread->m_waiting_for_input)

  /* For longjmp to where kbd input is being done.  This is per-thread
     so that if more than one thread calls read_char, they don't
     clobber each other's getcjmp, which will cause
     quit_throw_to_read_char crash due to using a wrong stack.  */
  sys_jmp_buf m_getcjmp;
#define getcjmp (current_thread->m_getcjmp)

  /* The OS identifier for this thread.  */
  sys_thread_t thread_id;

  /* The condition variable for this thread.  This is associated with
     the global lock.  This thread broadcasts to it when it exits.  */
  sys_cond_t thread_condvar;

  /* This thread might be waiting for some condition.  If so, this
     points to the condition.  If the thread is interrupted, the
     interrupter should broadcast to this condition.  */
  sys_cond_t *wait_condvar;

  /* Thread's name in the locale encoding.  */
  char *thread_name;

  /* This thread might have released the global lock.  If so, this is
     non-zero.  When a thread runs outside thread_select with this
     flag non-zero, it means it has been interrupted by SIGINT while
     in thread_select, and didn't have a chance of acquiring the lock.
     It must do so ASAP.  */
  int not_holding_lock;

  /* Threads are kept on a linked list.  */
  struct thread_state *next_thread;
} GCALIGNED_STRUCT;

INLINE bool
THREADP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_THREAD);
}

INLINE void
CHECK_THREAD (Lisp_Object x)
{
  CHECK_TYPE (THREADP (x), Qthreadp, x);
}

INLINE struct thread_state *
XTHREAD (Lisp_Object a)
{
  eassert (THREADP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct thread_state);
}

/* A mutex in lisp is represented by a system condition variable.
   The system mutex associated with this condition variable is the
   global lock.

   Using a condition variable lets us implement interruptibility for
   lisp mutexes.  */
typedef struct
{
  /* The owning thread, or NULL if unlocked.  */
  struct thread_state *owner;
  /* The lock count.  */
  unsigned int count;
  /* The underlying system condition variable.  */
  sys_cond_t condition;
} lisp_mutex_t;

/* A mutex as a lisp object.  */
struct Lisp_Mutex
{
  union vectorlike_header header;

  /* The name of the mutex, or nil.  */
  Lisp_Object name;

  /* The lower-level mutex object.  */
  lisp_mutex_t mutex;
} GCALIGNED_STRUCT;

INLINE bool
MUTEXP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_MUTEX);
}

INLINE void
CHECK_MUTEX (Lisp_Object x)
{
  CHECK_TYPE (MUTEXP (x), Qmutexp, x);
}

INLINE struct Lisp_Mutex *
XMUTEX (Lisp_Object a)
{
  eassert (MUTEXP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Mutex);
}

/* A condition variable as a lisp object.  */
struct Lisp_CondVar
{
  union vectorlike_header header;

  /* The associated mutex.  */
  Lisp_Object mutex;

  /* The name of the condition variable, or nil.  */
  Lisp_Object name;

  /* The lower-level condition variable object.  */
  sys_cond_t cond;
} GCALIGNED_STRUCT;

INLINE bool
CONDVARP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_CONDVAR);
}

INLINE void
CHECK_CONDVAR (Lisp_Object x)
{
  CHECK_TYPE (CONDVARP (x), Qcondition_variable_p, x);
}

INLINE struct Lisp_CondVar *
XCONDVAR (Lisp_Object a)
{
  eassert (CONDVARP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_CondVar);
}

extern struct thread_state *current_thread;

extern void finalize_one_thread (struct thread_state *state);
extern void finalize_one_mutex (struct Lisp_Mutex *);
extern void finalize_one_condvar (struct Lisp_CondVar *);
extern void maybe_reacquire_global_lock (void);

extern void init_threads (void);
extern void syms_of_threads (void);
extern bool main_thread_p (const void *);
extern bool in_current_thread (void);

typedef int select_func (int, fd_set *, fd_set *, fd_set *,
			 const struct timespec *, const sigset_t *);

int thread_select  (select_func *func, int max_fds, fd_set *rfds,
		    fd_set *wfds, fd_set *efds, struct timespec *timeout,
		    sigset_t *sigmask);

bool thread_check_current_buffer (struct buffer *);

#endif /* THREAD_H */
