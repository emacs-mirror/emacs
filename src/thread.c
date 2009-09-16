
#include <config.h>
#include "lisp.h"
#include <pthread.h>

void mark_byte_stack P_ ((struct byte_stack *));
void mark_backtrace P_ ((struct backtrace *));
void mark_catchlist P_ ((struct catchtag *));
void mark_stack P_ ((char *, char *));
void flush_stack_call_func P_ ((void (*) (char *, void *), void *));


static struct thread_state primary_thread;

static struct thread_state *all_threads = &primary_thread;

__thread struct thread_state *current_thread = &primary_thread;

pthread_mutex_t global_lock;

static void
mark_one_thread (struct thread_state *thread)
{
  register struct specbinding *bind;
  struct handler *handler;
  Lisp_Object tem;

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

  if (thread->m_current_buffer)
    {
      XSETBUFFER (tem, thread->m_current_buffer);
      mark_object (tem);
    }
}

static void
mark_threads_callback (char *end, void *ignore)
{
  struct thread_state *iter;

  current_thread->stack_top = end;
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
    unmark_byte_stack (iter->m_byte_stack_list);
}

static void
thread_yield_callback (char *end, void *ignore)
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
  if (all_threads->next_thread)
    flush_stack_call_func (thread_yield_callback, NULL);
}

DEFUN ("yield", Fyield, Syield, 0, 0, 0,
       doc: /* Yield to the next thread.  */)
     (void)
{
  thread_yield ();
}

static Lisp_Object
invoke_thread_function (void)
{
  Lisp_Object iter;

  int count = SPECPDL_INDEX ();

  /* Set up specpdl.  */
  for (iter = current_thread->initial_specpdl;
       !EQ (iter, Qnil);
       iter = XCDR (iter))
    {
      /* We may bind a variable twice -- but it doesn't matter because
	 there is no way to undo these bindings without exiting the
	 thread.  */
      specbind (XCAR (XCAR (iter)), XCDR (XCAR (iter)));
    }
  current_thread->initial_specpdl = Qnil;

  Feval (current_thread->func);
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
  struct thread_state *self = state;
  struct thread_state **iter;
  struct gcpro gcpro1;
  Lisp_Object buffer;
  char stack_pos;

  self->stack_top = self->stack_bottom = &stack_pos;

  self->m_specpdl_size = 50;
  self->m_specpdl = xmalloc (self->m_specpdl_size
			     * sizeof (struct specbinding));
  self->m_specpdl_ptr = self->m_specpdl;

  /* Thread-local assignment.  */
  current_thread = self;

  pthread_mutex_lock (&global_lock);

  /* We need special handling to set the initial buffer.  Our parent
     thread is very likely to be using this same buffer so we will
     typically wait for the parent thread to release it first.  */
  XSETBUFFER (buffer, self->m_current_buffer);
  GCPRO1 (buffer);
  self->m_current_buffer = 0;
  set_buffer_internal (XBUFFER (buffer));

  /* It might be nice to do something with errors here.  */
  internal_condition_case (invoke_thread_function, Qt, do_nothing);

  /* Unlink this thread from the list of all threads.  */
  for (iter = &all_threads; *iter != self; iter = &(*iter)->next_thread)
    ;
  *iter = (*iter)->next_thread;

  release_buffer (self);
  xfree (self->m_specpdl);

  pthread_mutex_unlock (&global_lock);

  return NULL;
}

DEFUN ("run-in-thread", Frun_in_thread, Srun_in_thread, 1, 1, 0,
       doc: /* Start a new thread and run FUNCTION in it.
When the function exits, the thread dies.  */)
     (function)
     Lisp_Object function;
{
  char stack_pos;
  pthread_t thr;
  struct thread_state *new_thread;
  struct specbinding *p;

  /* Can't start a thread in temacs.  */
  if (!initialized)
    abort ();

  new_thread = (struct thread_state *) allocate_pseudovector (VECSIZE (struct thread_state),
							      2, PVEC_THREAD);
  memset ((char *) new_thread + OFFSETOF (struct thread_state, m_gcprolist),
	  0, sizeof (struct thread_state) - OFFSETOF (struct thread_state,
						      m_gcprolist));

  new_thread->func = function;
  new_thread->initial_specpdl = Qnil;
  new_thread->m_current_buffer = current_thread->m_current_buffer;
  new_thread->stack_bottom = &stack_pos;

  for (p = specpdl; p != specpdl_ptr; ++p)
    {
      if (!p->func)
	{
	  Lisp_Object sym = p->symbol;
	  if (!SYMBOLP (sym))
	    sym = XCAR (sym);
	  new_thread->initial_specpdl
	    = Fcons (Fcons (sym, find_symbol_value (sym)),
		     new_thread->initial_specpdl);
	}
    }

  /* We'll need locking here.  */
  new_thread->next_thread = all_threads;
  all_threads = new_thread;

  if (pthread_create (&thr, NULL, run_thread, new_thread))
    {
      /* Restore the previous situation.  */
      all_threads = all_threads->next_thread;
    }

  return Qnil;
}

/* Get the current thread as a lisp object.  */
Lisp_Object
get_current_thread (void)
{
  Lisp_Object result;
  XSETTHREAD (result, current_thread);
  return result;
}

/* Get the main thread as a lisp object.  */
Lisp_Object
get_main_thread (void)
{
  Lisp_Object result;
  XSETTHREAD (result, &primary_thread);
  return result;
}

int
other_threads_p (void)
{
  return all_threads->next_thread != NULL;
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
