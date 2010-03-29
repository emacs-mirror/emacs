
#include <config.h>
#include <setjmp.h>
#include "lisp.h"
#include "buffer.h"
#include "blockinput.h"
#include <pthread.h>
#include "systime.h"
#include "sysselect.h"


void mark_byte_stack P_ ((struct byte_stack *));
void mark_backtrace P_ ((struct backtrace *));
void mark_catchlist P_ ((struct catchtag *));
void mark_stack P_ ((char *, char *));
void flush_stack_call_func P_ ((void (*) (char *, void *), void *));

/* Get the next thread as in circular buffer.  */
#define NEXT_THREAD(x)(x->next_thread ? x->next_thread : all_threads)

/* condition var .. w/ global lock */

static pthread_cond_t thread_cond;

static struct thread_state primary_thread;

static struct thread_state *all_threads = &primary_thread;

__thread struct thread_state *current_thread = &primary_thread;

pthread_mutex_t global_lock;

/* Used internally by the scheduler, it is the next that will be executed.  */
static pthread_t next_thread;

Lisp_Object minibuffer_mutex;

/* Choose the next thread to be executed.  */
static void
thread_schedule ()
{
  struct thread_state *it = current_thread;
  do
    {
      it = NEXT_THREAD (it);
    }
  while (it->blocked && it != current_thread);

  next_thread = it->pthread_id;
}

/* Schedule a new thread and block the caller until it is not scheduled
   again.  */
static inline void
reschedule (char *end, int wait)
{
  current_thread->stack_top = end;
  thread_schedule ();

  if (next_thread != current_thread->pthread_id)
    pthread_cond_broadcast (&thread_cond);

  if (!wait)
    return;

  pthread_mutex_unlock (&global_lock);

  pthread_mutex_lock (&global_lock);

  while (current_thread->pthread_id != next_thread)
    pthread_cond_wait (&thread_cond, &global_lock);
}

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

  if (thread->m_byte_stack_list)
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

  mark_object (thread->m_last_thing_searched);

  if (thread->m_saved_last_thing_searched)
    mark_object (thread->m_saved_last_thing_searched);
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
    if (iter->m_byte_stack_list)
      unmark_byte_stack (iter->m_byte_stack_list);
}

static void
thread_yield_callback (char *end, void *ignore)
{
  reschedule (end, 1);
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
  return other_threads_p () ? Qt : Qnil;
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

  Ffuncall (1, &current_thread->func);
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
  self->pthread_id = pthread_self ();

  /* Thread-local assignment.  */
  current_thread = self;

  /* We need special handling to set the initial buffer.  Our parent
     thread is very likely to be using this same buffer so we will
     typically wait for the parent thread to release it first.  */
  XSETBUFFER (buffer, self->m_current_buffer);
  GCPRO1 (buffer);
  self->m_current_buffer = 0;

  pthread_mutex_lock (&global_lock);

  set_buffer_internal (XBUFFER (buffer));

  /* It might be nice to do something with errors here.  */
  internal_condition_case (invoke_thread_function, Qt, do_nothing);

  blocal_unbind_thread (get_current_thread ());

  /* Unlink this thread from the list of all threads.  */
  for (iter = &all_threads; *iter != self; iter = &(*iter)->next_thread)
    ;
  *iter = (*iter)->next_thread;

  thread_schedule ();
  pthread_cond_broadcast (&thread_cond);

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

  new_thread = ALLOCATE_PSEUDOVECTOR (struct thread_state, m_gcprolist,
				      PVEC_THREAD);
  memset ((char *) new_thread + OFFSETOF (struct thread_state, m_gcprolist),
	  0, sizeof (struct thread_state) - OFFSETOF (struct thread_state,
						      m_gcprolist));

  new_thread->func = function;
  new_thread->blocked = 0;
  new_thread->initial_specpdl = Qnil;
  new_thread->m_last_thing_searched = Qnil; /* copy from parent? */
  new_thread->m_saved_last_thing_searched = Qnil;
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
      error ("Could not start a new thread");
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

/* Is the current an user thread.  */
int
user_thread_p (void)
{
  struct thread_state *it = all_threads;
  pthread_t self = pthread_self ();
  do
    {
      if (it->pthread_id == self)
	return 1;
    }
  while (it = it->next_thread);

  return 0;
}

DEFUN ("make-mutex", Fmake_mutex, Smake_mutex, 0, 1, 0,
       doc: /* Make a mutex.  If RECURSIVE is nil the mutex is not recursive
             and can be locked once.  */)
     (recursive)
     Lisp_Object recursive;
{
  Lisp_Object ret;
  struct Lisp_Mutex *mutex = allocate_mutex ();
  mutex->owner = 0;
  mutex->rec_counter = 0;
  mutex->recursive = ! NILP (recursive);
  XSETMUTEX (ret, mutex);
  return ret;
}

DEFUN ("mutex-lock", Fmutex_lock, Smutex_lock, 1, 1, 0,
       doc: /* Lock a mutex.  */)
     (val)
     Lisp_Object val;
{
  struct Lisp_Mutex *mutex = XMUTEX (val);
  while (1)
    {
      if (mutex->owner == 0
          || (mutex->recursive && mutex->owner == pthread_self ()))
        {
          mutex->owner = pthread_self ();
          mutex->rec_counter++;
          return Qt;
        }

      thread_yield ();
    }

  return Qt;
}

DEFUN ("mutex-unlock", Fmutex_unlock, Smutex_unlock, 1, 1, 0,
       doc: /* Unlock a mutex.  */)
     (val)
     Lisp_Object val;
{
  struct Lisp_Mutex *mutex = XMUTEX (val);

  if (mutex->owner != pthread_self () || mutex->rec_counter == 0)
    return Qnil;

  mutex->rec_counter--;

  if (mutex->rec_counter == 0)
    mutex->owner = 0;

  return Qt;
}

int
thread_select (n, rfd, wfd, xfd, tmo)
  int n;
  SELECT_TYPE *rfd, *wfd, *xfd;
  EMACS_TIME *tmo;
{
  char end;
  int ret;
  current_thread->blocked = 1;

  reschedule (&end, 0);

  pthread_mutex_unlock (&global_lock);

  ret = select (n, rfd, wfd, xfd, tmo);
  current_thread->blocked = 0;

  pthread_mutex_lock (&global_lock);
  pthread_cond_broadcast (&thread_cond);

  while (current_thread->pthread_id != next_thread)
    pthread_cond_wait (&thread_cond, &global_lock);

  return ret;
}

int
other_threads_p (void)
{
  return all_threads->next ? 1 : 0;
}

Lisp_Object
thread_notify_kill_buffer (register struct buffer *b)
{
  register Lisp_Object tem;
  struct thread_state *it = all_threads;
  for (; it; it = it->next_thread)
    {
      if (b == it->m_current_buffer)
        {
          register Lisp_Object buf;
          XSETBUFFER (buf, it->m_current_buffer);
          tem = Fother_buffer (buf, Qnil, Qnil);
          it->m_current_buffer = XBUFFER (tem);
          if (b == it->m_current_buffer)
            return Qnil;
        }
    }

  return Qt;
}

void
init_threads_once (void)
{
  primary_thread.size = PSEUDOVECSIZE (struct thread_state, m_gcprolist);
  primary_thread.next = NULL;
  primary_thread.func = Qnil;
  primary_thread.initial_specpdl = Qnil;
  XSETPVECTYPE (&primary_thread, PVEC_THREAD);
  minibuffer_mutex = Fmake_mutex (Qnil);
}

void
init_threads (void)
{
  pthread_mutex_init (&global_lock, NULL);
  pthread_cond_init (&thread_cond, NULL);
  pthread_mutex_lock (&global_lock);

  primary_thread.pthread_id = pthread_self ();
  primary_thread.blocked = 0;
  primary_thread.m_last_thing_searched = Qnil;
  next_thread = primary_thread.pthread_id;
}

void
syms_of_threads (void)
{
  DEFVAR_LISP ("minibuffer-mutex", &minibuffer_mutex,
	       doc: /* Mutex for the minibuffer.  */);

  defsubr (&Srun_in_thread);
  defsubr (&Syield);
  defsubr (&Smake_mutex);
  defsubr (&Smutex_lock);
  defsubr (&Smutex_unlock);
}
