
#include <config.h>
#include "lisp.h"
#include <pthread.h>

void mark_byte_stack P_ ((struct byte_stack *));
void mark_backtrace P_ ((struct backtrace *));
void mark_catchlist P_ ((struct catchtag *));
void mark_stack P_ ((char *, char *));
void flush_stack_call_func P_ ((void (*) (char *)));


static struct thread_state primary_thread;

static struct thread_state *all_threads = &primary_thread;

__thread struct thread_state *current_thread = &primary_thread;

static pthread_mutex_t global_lock;

static void
mark_one_thread (struct thread_state *thread)
{
  register struct specbinding *bind;
  struct handler *handler;

  for (bind = thread->m_specpdl; bind != thread->m_specpdl_ptr; bind++)
    {
      mark_object (bind->symbol);
      mark_object (bind->old_value);
    }

#if (GC_MARK_STACK == GC_MAKE_GCPROS_NOOPS \
     || GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS)
  mark_stack (thread->stack_bottom, thread->stack_top);
#else
  {
    register struct gcpro *tail;
    for (tail = thread->m_gcprolist; tail; tail = tail->next)
      for (i = 0; i < tail->nvars; i++)
	mark_object (tail->var[i]);
  }
#endif

  mark_byte_stack (thread->m_byte_stack_list);

  mark_catchlist (thread->m_catchlist);

  for (handler = thread->m_handlerlist; handler; handler = handler->next)
    {
      mark_object (handler->handler);
      mark_object (handler->var);
    }

  mark_backtrace (thread->m_backtrace_list);

  if (thread->func)
    mark_object (thread->func);
}

static void
mark_threads_continuation (char *end)
{
  struct thread_state *iter;

  current_thread->stack_top = end;
  for (iter = all_threads; iter; iter = iter->next)
    mark_one_thread (iter);
}

void
mark_threads (void)
{
  flush_stack_call_func (mark_threads_continuation);
}

void
unmark_threads (void)
{
  struct thread_state *iter;

  for (iter = all_threads; iter; iter = iter->next)
    unmark_byte_stack (iter->m_byte_stack_list);
}

static void
thread_yield_continuation (char *end)
{
  current_thread->stack_top = end;
  pthread_mutex_unlock (&global_lock);
  sched_yield ();
  pthread_mutex_lock (&global_lock);
}

void
thread_yield (void)
{
  /* Note: currently it is safe to check this here, but eventually it
     will require a lock to ensure non-racy operation.  */
  /* Only yield if there is another thread to yield to.  */
  if (all_threads->next)
    flush_stack_call_func (thread_yield_continuation);
}

DEFUN ("yield", Fyield, Syield, 0, 0, 0,
       doc: /* Yield to the next thread.  */)
     (void)
{
  thread_yield ();
}

static void *
run_thread (void *state)
{
  char stack_bottom_variable;
  struct thread_state *self = state;
  struct thread_state **iter;

  self->stack_bottom = &stack_bottom_variable;

  self->m_specpdl_size = 50;
  self->m_specpdl = xmalloc (self->m_specpdl_size
			     * sizeof (struct specbinding));
  self->m_specpdl_ptr = self->m_specpdl;

  /* Thread-local assignment.  */
  current_thread = self;

  pthread_mutex_lock (&global_lock);

  /* FIXME: unwind protect here.  */
  Ffuncall (1, &self->func);

  /* Unlink this thread from the list of all threads.  */
  for (iter = &all_threads; *iter != self; iter = &(*iter)->next)
    ;
  *iter = (*iter)->next;

  xfree (self->m_specpdl);
  /* FIXME: other cleanups here.  */
  xfree (self);

  pthread_mutex_unlock (&global_lock);

  return NULL;
}

DEFUN ("run-in-thread", Frun_in_thread, Srun_in_thread, 1, 1, 0,
       doc: /* Start a new thread and run FUNCTION in it.
When the function exits, the thread dies.  */)
     (function)
     Lisp_Object function;
{
  pthread_t thr;
  struct thread_state *new_thread;

  /* Can't start a thread in temacs.  */
  if (!initialized)
    abort ();

  new_thread = xmalloc (sizeof (struct thread_state));
  memset (new_thread, 0, sizeof (struct thread_state));

  new_thread->func = function;

  /* We'll need locking here.  */
  new_thread->next = all_threads;
  all_threads = new_thread;

  /* FIXME check result */
  pthread_create (&thr, NULL, run_thread, new_thread);
}

void
init_threads (void)
{
  pthread_mutex_init (&global_lock, NULL);
  pthread_mutex_lock (&global_lock);
}

void
syms_of_threads (void)
{
  defsubr (&Srun_in_thread);
  defsubr (&Syield);
}
