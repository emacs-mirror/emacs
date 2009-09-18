
struct thread_state
{
  EMACS_UINT size;
  struct Lisp_Vector *next;

  /* The function we are evaluating, or 0 in the main thread.  */
  Lisp_Object func;

  /* An alias of symbols and values that we use to populate the
     initial specpdl.  */
  Lisp_Object initial_specpdl;

  /* Recording what needs to be marked for gc.  */
  struct gcpro *m_gcprolist;
#define gcprolist (current_thread->m_gcprolist)

  /* A list of currently active byte-code execution value stacks.
     Fbyte_code adds an entry to the head of this list before it starts
     processing byte-code, and it removed the entry again when it is
     done.  Signalling an error truncates the list analoguous to
     gcprolist.  */
  struct byte_stack *m_byte_stack_list;
#define byte_stack_list (current_thread->m_byte_stack_list)

  /* An address near the bottom of the stack.
     Tells GC how to save a copy of the stack.  */
  char *stack_bottom;

  /* An address near the top of the stack.  */
  char *stack_top;

  struct backtrace *m_backtrace_list;
#define backtrace_list (current_thread->m_backtrace_list)

  struct catchtag *m_catchlist;
#define catchlist (current_thread->m_catchlist)

  /* Chain of condition handlers currently in effect.
     The elements of this chain are contained in the stack frames
     of Fcondition_case and internal_condition_case.
     When an error is signaled (by calling Fsignal, below),
     this chain is searched for an element that applies.  */
  struct handler *m_handlerlist;
#define handlerlist (current_thread->m_handlerlist)

  /* Count levels of GCPRO to detect failure to UNGCPRO.  */
  int m_gcpro_level;
#define gcpro_level (current_thread->m_gcpro_level)

  /* Current number of specbindings allocated in specpdl.  */
  int m_specpdl_size;
#define specpdl_size (current_thread->m_specpdl_size)

  /* Pointer to beginning of specpdl.  */
  struct specbinding *m_specpdl;
#define specpdl (current_thread->m_specpdl)

  /* Pointer to first unused element in specpdl.  */
  struct specbinding *m_specpdl_ptr;
#define specpdl_ptr (current_thread->m_specpdl_ptr)

  /* Depth in Lisp evaluations and function calls.  */
  int m_lisp_eval_depth;
#define lisp_eval_depth (current_thread->m_lisp_eval_depth)

  /* This points to the current buffer.  */
  struct buffer *m_current_buffer;
#define current_buffer (current_thread->m_current_buffer)

  struct thread_state *next_thread;

  pthread_t pthread_id;

  /* If nonzero the thread access a buffer without lock it.  */
  int nolock;
};

extern __thread struct thread_state *current_thread;

extern void init_threads P_ ((void));

extern void thread_yield P_ ((void));

extern void syms_of_threads P_ ((void));

extern Lisp_Object get_current_thread P_ ((void));

extern Lisp_Object get_main_thread P_ ((void));

extern pthread_mutex_t global_lock;

extern int other_threads_p P_ ((void));

extern int user_thread_p P_ ((void));

EXFUN (Finhibit_yield, 1);

extern int thread_inhibit_yield_p  P_ ((void));
