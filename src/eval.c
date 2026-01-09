/* Evaluator for GNU Emacs Lisp interpreter.

Copyright (C) 1985-1987, 1993-1995, 1999-2026 Free Software Foundation,
Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


#include <config.h>
#include <limits.h>
#include <stdlib.h>
#include "lisp.h"
#include "blockinput.h"
#include "commands.h"
#include "keyboard.h"
#include "dispextern.h"
#include "buffer.h"
#include "pdumper.h"
#include "atimer.h"

/* Non-nil means record all fset's and provide's, to be undone
   if the file being autoloaded is not fully loaded.
   They are recorded by being consed onto the front of Vautoload_queue:
   (FUN . ODEF) for a defun, (0 . OFEATURES) for a provide.  */

Lisp_Object Vautoload_queue;

/* This holds either the symbol `run-hooks' or nil.
   It is nil at an early stage of startup, and when Emacs
   is shutting down.  */
Lisp_Object Vrun_hooks;

/* The function from which the last `signal' was called.  Set in
   Fsignal.  */
/* FIXME: We should probably get rid of this!  */
Lisp_Object Vsignaling_function;

/* These would ordinarily be static, but they need to be visible to GDB.  */
bool backtrace_p (union specbinding *) EXTERNALLY_VISIBLE;
union specbinding *backtrace_next (union specbinding *) EXTERNALLY_VISIBLE;
union specbinding *backtrace_top (void) EXTERNALLY_VISIBLE;

static Lisp_Object funcall_lambda (Lisp_Object, ptrdiff_t, Lisp_Object *);
static Lisp_Object apply_lambda (Lisp_Object, Lisp_Object, specpdl_ref);
static Lisp_Object lambda_arity (Lisp_Object);

static Lisp_Object
specpdl_symbol (union specbinding *pdl)
{
  eassert (pdl->kind >= SPECPDL_LET);
  return pdl->let.symbol;
}

static enum specbind_tag
specpdl_kind (union specbinding *pdl)
{
  eassert (pdl->kind >= SPECPDL_LET);
  return pdl->let.kind;
}

static Lisp_Object
specpdl_old_value (union specbinding *pdl)
{
  eassert (pdl->kind >= SPECPDL_LET);
  return pdl->let.old_value;
}

static void
set_specpdl_old_value (union specbinding *pdl, Lisp_Object val)
{
  eassert (pdl->kind >= SPECPDL_LET);
  pdl->let.old_value = val;
}

static Lisp_Object
specpdl_where (union specbinding *pdl)
{
  eassert (pdl->kind > SPECPDL_LET);
  return pdl->let.where.buf;
}

static KBOARD *
specpdl_kboard (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_LET);
  return pdl->let.where.kbd;
}

static Lisp_Object
specpdl_arg (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_UNWIND);
  return pdl->unwind.arg;
}

/* To work around GDB bug 32313
   <https://sourceware.org/bugzilla/show_bug.cgi?id=32313> make
   backtrace_* functions visible-to-GDB pointers instead of merely
   being an externally visible functions themselves.  Declare the
   pointer first to pacify gcc -Wmissing-variable-declarations.  */
#define GDB_FUNCPTR(func, resulttype, params) \
  extern resulttype (*const func) params EXTERNALLY_VISIBLE; \
  resulttype (*const func) params = func##_body

static Lisp_Object
backtrace_function_body (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  return pdl->bt.function;
}
GDB_FUNCPTR (backtrace_function, Lisp_Object, (union specbinding *));

static ptrdiff_t
backtrace_nargs (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  return pdl->bt.nargs;
}

static Lisp_Object *
backtrace_args_body (union specbinding *pdl)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  return pdl->bt.args;
}
GDB_FUNCPTR (backtrace_args, Lisp_Object *, (union specbinding *));

/* Functions to modify slots of backtrace records.  */

static void
set_backtrace_args (union specbinding *pdl, Lisp_Object *args, ptrdiff_t nargs)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  pdl->bt.args = args;
  pdl->bt.nargs = nargs;
}

static void
set_backtrace_debug_on_exit (union specbinding *pdl, bool doe)
{
  eassert (pdl->kind == SPECPDL_BACKTRACE);
  pdl->bt.debug_on_exit = doe;
}

/* Helper functions to scan the backtrace.  */

bool
backtrace_p (union specbinding *pdl)
{
  if (current_thread && specpdl && pdl)
    return pdl >= specpdl;
  return false;
}

static bool
backtrace_thread_p (struct thread_state *tstate, union specbinding *pdl)
{ return pdl >= tstate->m_specpdl; }

union specbinding *
backtrace_top (void)
{
  /* This is so "xbacktrace" doesn't crash in pdumped Emacs if they
     invoke the command before init_eval_once_for_pdumper initializes
     specpdl machinery.  See also backtrace_p above.  */
  if (!current_thread || !specpdl)
    return NULL;

  union specbinding *pdl = specpdl_ptr - 1;
  while (backtrace_p (pdl) && pdl->kind != SPECPDL_BACKTRACE)
    pdl--;
  return pdl;
}

static union specbinding *
backtrace_thread_top (struct thread_state *tstate)
{
  union specbinding *pdl = tstate->m_specpdl_ptr - 1;
  while (backtrace_thread_p (tstate, pdl) && pdl->kind != SPECPDL_BACKTRACE)
    pdl--;
  return pdl;
}

union specbinding *
backtrace_next (union specbinding *pdl)
{
  pdl--;
  while (backtrace_p (pdl) && pdl->kind != SPECPDL_BACKTRACE)
    pdl--;
  return pdl;
}

static void init_eval_once_for_pdumper (void);

static union specbinding *
backtrace_thread_next (struct thread_state *tstate, union specbinding *pdl)
{
  pdl--;
  while (backtrace_thread_p (tstate, pdl) && pdl->kind != SPECPDL_BACKTRACE)
    pdl--;
  return pdl;
}

void
init_eval_once (void)
{
  /* Don't forget to update docs (lispref node "Eval").  */
  Vrun_hooks = Qnil;
  pdumper_do_now_and_after_load (init_eval_once_for_pdumper);
}

static void
init_eval_once_for_pdumper (void)
{
  enum { size = 50 };
  union specbinding *pdlvec = malloc ((size + 1) * sizeof *specpdl);
  specpdl = specpdl_ptr = pdlvec + 1;
  specpdl_end = specpdl + size;
}

void
init_eval (void)
{
  specpdl_ptr = specpdl;
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
  Vquit_flag = Qnil;
  debug_on_next_call = 0;
  lisp_eval_depth = 0;
  /* This is less than the initial value of num_nonmacro_input_events.  */
  when_entered_debugger = -1;
}

static void
restore_stack_limits (Lisp_Object data)
{
  intmax_t old_depth;
  integer_to_intmax (data, &old_depth);
  lisp_eval_depth_reserve += max_lisp_eval_depth - old_depth;
  max_lisp_eval_depth = old_depth;
}

/* Try and ensure that we have at least B dpeth available.  */

static void
max_ensure_room (intmax_t b)
{
  intmax_t sum = ckd_add (&sum, lisp_eval_depth, b) ? INTMAX_MAX : sum;
  intmax_t diff = min (sum - max_lisp_eval_depth, lisp_eval_depth_reserve);
  if (diff <= 0)
    return;
  intmax_t old_depth = max_lisp_eval_depth;
  max_lisp_eval_depth += diff;
  lisp_eval_depth_reserve -= diff;
  /* Restore limits after leaving the debugger.  */
  record_unwind_protect (restore_stack_limits, make_int (old_depth));
}

/* Call the Lisp debugger, giving it argument ARG.  */

Lisp_Object
call_debugger (Lisp_Object arg)
{
  bool debug_while_redisplaying;
  specpdl_ref count = SPECPDL_INDEX ();
  Lisp_Object val;

  /* The debugger currently requires 77 additional frames to print lists
     nested 8 deep (the value of print-level used in the debugger) using
     cl-prin1 (bug#31919), with a margin to be on the safe side.  */
  max_ensure_room (200);

#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  debug_on_next_call = 0;
  when_entered_debugger = num_nonmacro_input_events;

  /* Resetting redisplaying_p to 0 makes sure that debug output is
     displayed if the debugger is invoked during redisplay.  */
  debug_while_redisplaying = redisplaying_p;
  int redisplay_counter_before = redisplay_counter;
  redisplaying_p = 0;
  specbind (Qdebugger_may_continue,
	    debug_while_redisplaying ? Qnil : Qt);
  specbind (Qinhibit_redisplay, Qnil);
  specbind (Qinhibit_debugger, Qt);

  /* If we are debugging an error while `inhibit-changing-match-data'
     is bound to non-nil (e.g., within a call to `string-match-p'),
     then make sure debugger code can still use match data.  */
  specbind (Qinhibit_changing_match_data, Qnil);

#if 0 /* Binding this prevents execution of Lisp code during
	 redisplay, which necessarily leads to display problems.  */
  specbind (Qinhibit_eval_during_redisplay, Qt);
#endif

  val = apply1 (Vdebugger, arg);

  /* Interrupting redisplay and resuming it later is not safe under
     all circumstances.  So, when the debugger returns, abort the
     interrupted redisplay by going back to the top-level.  */
  if (debug_while_redisplaying
      && redisplay_counter_before != redisplay_counter)
    /* FIXME: Rather than jump all the way to `top-level`
       we should exit only the current redisplay.  */
    Ftop_level ();

  return unbind_to (count, val);
}

void
do_debug_on_call (Lisp_Object code, specpdl_ref count)
{
  debug_on_next_call = 0;
  set_backtrace_debug_on_exit (specpdl_ref_to_ptr (count), true);
  call_debugger (list1 (code));
}


DEFUN ("or", For, Sor, 0, UNEVALLED, 0,
       doc: /* Eval args until one of them yields non-nil, then return that value.
The remaining args are not evalled at all.
If all args return nil, return nil.
usage: (or CONDITIONS...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = Qnil;

  while (CONSP (args))
    {
      Lisp_Object arg = XCAR (args);
      args = XCDR (args);
      val = eval_sub (arg);
      if (!NILP (val))
	break;
    }

  return val;
}

DEFUN ("and", Fand, Sand, 0, UNEVALLED, 0,
       doc: /* Eval args until one of them yields nil, then return nil.
The remaining args are not evalled at all.
If no arg yields nil, return the last arg's value.
usage: (and CONDITIONS...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = Qt;

  while (CONSP (args))
    {
      Lisp_Object arg = XCAR (args);
      args = XCDR (args);
      val = eval_sub (arg);
      if (NILP (val))
	break;
    }

  return val;
}

DEFUN ("if", Fif, Sif, 2, UNEVALLED, 0,
       doc: /* If COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.
usage: (if COND THEN ELSE...)  */)
  (Lisp_Object args)
{
  Lisp_Object cond;

  cond = eval_sub (XCAR (args));

  if (!NILP (cond))
    return eval_sub (Fcar (XCDR (args)));
  return Fprogn (Fcdr (XCDR (args)));
}

DEFUN ("cond", Fcond, Scond, 0, UNEVALLED, 0,
       doc: /* Try each clause until one succeeds.
Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is the value of the cond-form.
If a clause has one element, as in (CONDITION), then the cond-form
returns CONDITION's value, if that is non-nil.
If no clause succeeds, cond returns nil.
usage: (cond CLAUSES...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = args;

  while (CONSP (args))
    {
      Lisp_Object clause = XCAR (args);
      val = eval_sub (Fcar (clause));
      if (!NILP (val))
	{
	  if (!NILP (XCDR (clause)))
	    val = Fprogn (XCDR (clause));
	  break;
	}
      args = XCDR (args);
    }

  return val;
}

DEFUN ("progn", Fprogn, Sprogn, 0, UNEVALLED, 0,
       doc: /* Eval BODY forms sequentially and return value of last one.
usage: (progn BODY...)  */)
  (Lisp_Object body)
{
  Lisp_Object val = Qnil;

  while (CONSP (body))
    {
      Lisp_Object form = XCAR (body);
      body = XCDR (body);
      val = eval_sub (form);
    }

  return val;
}

/* Evaluate BODY sequentially, discarding its value.  */

void
prog_ignore (Lisp_Object body)
{
  Fprogn (body);
}

DEFUN ("prog1", Fprog1, Sprog1, 1, UNEVALLED, 0,
       doc: /* Eval FIRST and BODY sequentially; return value from FIRST.
The value of FIRST is saved during the evaluation of the remaining args,
whose values are discarded.
usage: (prog1 FIRST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = eval_sub (XCAR (args));
  prog_ignore (XCDR (args));
  return val;
}

DEFUN ("setq", Fsetq, Ssetq, 0, UNEVALLED, 0,
       doc: /* Set each SYM to the value of its VAL.
The symbols SYM are variables; they are literal (not evaluated).
The values VAL are expressions; they are evaluated.
Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
The second VAL is not computed until after the first SYM is set, and so on;
each VAL can use the new value of variables set earlier in the `setq'.
The return value of the `setq' form is the value of the last VAL.
usage: (setq [SYM VAL]...)  */)
  (Lisp_Object args)
{
  Lisp_Object val = args, tail = args;

  for (EMACS_INT nargs = 0; CONSP (tail); nargs += 2)
    {
      Lisp_Object sym = XCAR (tail);
      tail = XCDR (tail);
      if (!CONSP (tail))
	xsignal2 (Qwrong_number_of_arguments, Qsetq, make_fixnum (nargs + 1));
      Lisp_Object arg = XCAR (tail);
      tail = XCDR (tail);
      val = eval_sub (arg);
      /* Like for eval_sub, we do not check declared_special here since
	 it's been done when let-binding.  */
      Lisp_Object lex_binding
	= (SYMBOLP (sym)
	   ? Fassq (sym, Vinternal_interpreter_environment)
	   : Qnil);
      if (!NILP (lex_binding))
	XSETCDR (lex_binding, val); /* SYM is lexically bound.  */
      else
	Fset (sym, val);	/* SYM is dynamically bound.  */
    }

  return val;
}

DEFUN ("quote", Fquote, Squote, 1, UNEVALLED, 0,
       doc: /* Return the argument, without evaluating it.  `(quote x)' yields `x'.
Warning: `quote' does not construct its return value, but just returns
the value that was pre-constructed by the Lisp reader (see info node
`(elisp)Printed Representation').
This means that \\='(a . b) is not identical to (cons \\='a \\='b): the former
does not cons.  Quoting should be reserved for constants that will
never be modified by side-effects, unless you like self-modifying code.
See the common pitfall in info node `(elisp)Rearrangement' for an example
of unexpected results when a quoted object is modified.
usage: (quote ARG)  */)
  (Lisp_Object args)
{
  if (!NILP (XCDR (args)))
    xsignal2 (Qwrong_number_of_arguments, Qquote, Flength (args));
  return XCAR (args);
}

DEFUN ("make-interpreted-closure", Fmake_interpreted_closure,
       Smake_interpreted_closure, 3, 5, 0,
       doc: /* Make an interpreted closure.
ARGS should be the list of formal arguments.
BODY should be a non-empty list of forms.
ENV should be a lexical environment, like the second argument of `eval'.
IFORM if non-nil should be of the form (interactive ...).  */)
  (Lisp_Object args, Lisp_Object body, Lisp_Object env,
   Lisp_Object docstring, Lisp_Object iform)
{
  Lisp_Object ifcdr, value, slots[6];

  CHECK_CONS (body);          /* Make sure it's not confused with byte-code! */
  CHECK_LIST (args);
  CHECK_LIST (iform);
  ifcdr = CDR (iform);
  if (NILP (CDR (ifcdr)))
    value = CAR (ifcdr);
  else
    value = CALLN (Fvector, XCAR (ifcdr), XCDR (ifcdr));
  slots[0] = args;
  slots[1] = body;
  slots[2] = env;
  slots[3] = Qnil;
  slots[4] = docstring;
  slots[5] = value;
  /* Adjusting the size is indispensable since, as for byte-code objects,
     we distinguish interactive functions by the presence or absence of the
     iform slot.  */
  Lisp_Object val
    = Fvector (!NILP (iform) ? 6 : !NILP (docstring) ? 5 : 3, slots);
  XSETPVECTYPE (XVECTOR (val), PVEC_CLOSURE);
  return val;
}

DEFUN ("function", Ffunction, Sfunction, 1, UNEVALLED, 0,
       doc: /* Like `quote', but preferred for objects which are functions.
In byte compilation, `function' causes its argument to be handled by
the byte compiler.  Similarly, when expanding macros and expressions,
ARG can be examined and possibly expanded.  If `quote' is used
instead, this doesn't happen.

usage: (function ARG)  */)
  (Lisp_Object args)
{
  Lisp_Object quoted = XCAR (args);

  if (!NILP (XCDR (args)))
    xsignal2 (Qwrong_number_of_arguments, Qfunction, Flength (args));

  if (CONSP (quoted)
      && EQ (XCAR (quoted), Qlambda))
    { /* This is a lambda expression within a lexical environment;
	 return an interpreted closure instead of a simple lambda.  */
      Lisp_Object cdr = XCDR (quoted);
      Lisp_Object args = Fcar (cdr);
      cdr = Fcdr (cdr);
      Lisp_Object docstring = Qnil, iform = Qnil;
      if (CONSP (cdr))
        {
          docstring = XCAR (cdr);
          if (STRINGP (docstring))
            {
              Lisp_Object tmp = XCDR (cdr);
              if (!NILP (tmp))
                cdr = tmp;
              else     /* It's not a docstring, it's a return value.  */
                docstring = Qnil;
            }
          /* Handle the special (:documentation <form>) to build the docstring
	     dynamically.  */
          else if (CONSP (docstring)
                   && EQ (QCdocumentation, XCAR (docstring))
                   && (docstring = eval_sub (Fcar (XCDR (docstring))),
                       true))
            cdr = XCDR (cdr);
          else
            docstring = Qnil;   /* Not a docstring after all.  */
        }
      if (CONSP (cdr))
        {
          iform = XCAR (cdr);
          if (CONSP (iform)
              && EQ (Qinteractive, XCAR (iform)))
            cdr = XCDR (cdr);
          else
            iform = Qnil;   /* Not an interactive-form after all.  */
        }
      if (NILP (cdr))
        cdr = Fcons (Qnil, Qnil); /* Make sure the body is never empty! */

      if (NILP (Vinternal_interpreter_environment)
          || NILP (Vinternal_make_interpreted_closure_function))
        return Fmake_interpreted_closure
            (args, cdr, Vinternal_interpreter_environment, docstring, iform);
      else
        return calln (Vinternal_make_interpreted_closure_function,
                      args, cdr, Vinternal_interpreter_environment,
                      docstring, iform);
    }
  else
    /* Simply quote the argument.  */
    return quoted;
}


DEFUN ("defvaralias", Fdefvaralias, Sdefvaralias, 2, 3, 0,
       doc: /* Make NEW-ALIAS a variable alias for symbol BASE-VARIABLE.
Aliased variables always have the same value; setting one sets the other.
Third arg DOCSTRING, if non-nil, is documentation for NEW-ALIAS.  If it is
omitted or nil, NEW-ALIAS gets the documentation string of BASE-VARIABLE,
or of the variable at the end of the chain of aliases, if BASE-VARIABLE is
itself an alias.  If NEW-ALIAS is bound, and BASE-VARIABLE is not,
then the value of BASE-VARIABLE is set to that of NEW-ALIAS.
The return value is BASE-VARIABLE.

If the resulting chain of variable definitions would contain a loop,
signal a `cyclic-variable-indirection' error.  */)
  (Lisp_Object new_alias, Lisp_Object base_variable, Lisp_Object docstring)
{
  CHECK_SYMBOL (new_alias);
  CHECK_SYMBOL (base_variable);

  if (SYMBOL_CONSTANT_P (new_alias))
    /* Making it an alias effectively changes its value.  */
    error ("Cannot make a constant an alias: %s",
	   SDATA (SYMBOL_NAME (new_alias)));

  struct Lisp_Symbol *sym = XSYMBOL (new_alias);

  /* Ensure non-circularity.  */
  struct Lisp_Symbol *s = XSYMBOL (base_variable);
  for (;;)
    {
      if (s == sym)
	xsignal1 (Qcyclic_variable_indirection, base_variable);
      if (s->u.s.redirect != SYMBOL_VARALIAS)
	break;
      s = SYMBOL_ALIAS (s);
    }

  switch (sym->u.s.redirect)
    {
    case SYMBOL_FORWARDED:
      error ("Cannot make a built-in variable an alias: %s",
	     SDATA (SYMBOL_NAME (new_alias)));
    case SYMBOL_LOCALIZED:
      error ("Don't know how to make a buffer-local variable an alias: %s",
	     SDATA (SYMBOL_NAME (new_alias)));
    case SYMBOL_PLAINVAL:
    case SYMBOL_VARALIAS:
      break;
    default:
      emacs_abort ();
    }

  /* https://lists.gnu.org/r/emacs-devel/2008-04/msg00834.html
     If n_a is bound, but b_v is not, set the value of b_v to n_a,
     so that old-code that affects n_a before the aliasing is setup
     still works.  */
  if (NILP (Fboundp (base_variable)))
    set_internal (base_variable, find_symbol_value (new_alias),
                  Qnil, SET_INTERNAL_BIND);
  else if (!NILP (Fboundp (new_alias))
           && !EQ (find_symbol_value (new_alias),
                   find_symbol_value (base_variable)))
    {
      Lisp_Object message, formatted;

      message = build_string ("Overwriting value of `%s' by aliasing"
			      " to `%s'");
      formatted = CALLN (Fformat_message, message,
			 new_alias, base_variable);
      calln (Qdisplay_warning,
	     list3 (Qdefvaralias, Qlosing_value, new_alias),
	     formatted);
    }

  {
    union specbinding *p;

    for (p = specpdl_ptr; p > specpdl; )
      if ((--p)->kind >= SPECPDL_LET
	  && (EQ (new_alias, specpdl_symbol (p))))
	error ("Don't know how to make a let-bound variable an alias: %s",
	       SDATA (SYMBOL_NAME (new_alias)));
  }

  if (sym->u.s.trapped_write == SYMBOL_TRAPPED_WRITE)
    notify_variable_watchers (new_alias, base_variable, Qdefvaralias, Qnil);

  sym->u.s.declared_special = true;
  XSYMBOL (base_variable)->u.s.declared_special = true;
  sym->u.s.redirect = SYMBOL_VARALIAS;
  SET_SYMBOL_ALIAS (sym, XSYMBOL (base_variable));
  sym->u.s.trapped_write = XSYMBOL (base_variable)->u.s.trapped_write;
  LOADHIST_ATTACH (new_alias);
  /* Even if docstring is nil: remove old docstring.  */
  Fput (new_alias, Qvariable_documentation, docstring);

  return base_variable;
}

DEFUN ("internal-delete-indirect-variable", Finternal_delete_indirect_variable, Sinternal_delete_indirect_variable,
       1, 1, 0,
       doc: /* Undeclare SYMBOL as variable alias, then unbind it.
Return SYMBOL.
Internal use only.  */)
  (register Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (XSYMBOL (symbol)->u.s.redirect != SYMBOL_VARALIAS)
    xsignal2 (Qerror,
	      build_string ("Cannot undeclare a variable that is not an alias"),
	      symbol);
  XSYMBOL (symbol)->u.s.redirect = SYMBOL_PLAINVAL;
  Fput (symbol, Qvariable_documentation, Qnil);
  Fset (symbol, Qunbound);
  return symbol;
}

static union specbinding *
default_toplevel_binding (Lisp_Object symbol)
{
  for (union specbinding *pdl = specpdl; pdl < specpdl_ptr; ++pdl)
    {
      switch (pdl->kind)
	{
	case SPECPDL_LET_DEFAULT:
	case SPECPDL_LET:
	  if (EQ (specpdl_symbol (pdl), symbol))
	    return pdl;
	  break;

	default: break;
	}
    }
  return NULL;
}

static union specbinding *
local_toplevel_binding (Lisp_Object symbol, Lisp_Object buf)
{
  for (union specbinding *pdl = specpdl; pdl < specpdl_ptr; ++pdl)
    {
      switch (pdl->kind)
	{
	case SPECPDL_LET_LOCAL:
	  if (BASE_EQ (specpdl_where (pdl), buf)
	      && EQ (specpdl_symbol (pdl), symbol))
	    return pdl;
	  break;

	default: break;
	}
    }
  return NULL;
}

/* Look for a lexical-binding of SYMBOL somewhere up the stack.
   This will only find bindings created with interpreted code, since once
   compiled names of lexical variables are basically gone anyway.  */
static bool
lexbound_p (Lisp_Object symbol)
{
  union specbinding *pdl = specpdl_ptr;
  while (pdl > specpdl)
    {
      switch ((--pdl)->kind)
	{
	case SPECPDL_LET_DEFAULT:
	case SPECPDL_LET:
	  if (BASE_EQ (specpdl_symbol (pdl), Qinternal_interpreter_environment))
	    {
	      Lisp_Object env = specpdl_old_value (pdl);
	      if (CONSP (env) && !NILP (Fassq (symbol, env)))
	        return true;
	    }
	  break;

	default: break;
	}
    }
  return false;
}

DEFUN ("default-toplevel-value", Fdefault_toplevel_value, Sdefault_toplevel_value, 1, 1, 0,
       doc: /* Return SYMBOL's toplevel default value.
"Toplevel" means outside of any let binding.  */)
  (Lisp_Object symbol)
{
  union specbinding *binding = default_toplevel_binding (symbol);
  Lisp_Object value
    = binding ? specpdl_old_value (binding) : Fdefault_value (symbol);
  if (!BASE_EQ (value, Qunbound))
    return value;
  xsignal1 (Qvoid_variable, symbol);
}

DEFUN ("set-default-toplevel-value", Fset_default_toplevel_value,
       Sset_default_toplevel_value, 2, 2, 0,
       doc: /* Set SYMBOL's toplevel default value to VALUE.
"Toplevel" means outside of any let binding.  */)
     (Lisp_Object symbol, Lisp_Object value)
{
  union specbinding *binding = default_toplevel_binding (symbol);
  if (binding)
    set_specpdl_old_value (binding, value);
  else
    Fset_default (symbol, value);
  return Qnil;
}

DEFUN ("buffer-local-toplevel-value",
       Fbuffer_local_toplevel_value,
       Sbuffer_local_toplevel_value, 1, 2, 0,
       doc: /* Return SYMBOL's toplevel buffer-local value in BUFFER.
"Toplevel" means outside of any let binding.
BUFFER defaults to the current buffer.
If SYMBOL has no local value in BUFFER, signals an error.  */)
  (Lisp_Object symbol, Lisp_Object buffer)
{
  if (NILP (buffer))
    buffer = Fcurrent_buffer ();
  if (NILP (Flocal_variable_p (symbol, buffer)))
    xsignal1 (Qvoid_variable, symbol);
  union specbinding *binding = local_toplevel_binding (symbol, buffer);
  return binding
    ? specpdl_old_value (binding)
    : Fbuffer_local_value (symbol, buffer);
}

DEFUN ("set-buffer-local-toplevel-value",
       Fset_buffer_local_toplevel_value,
       Sset_buffer_local_toplevel_value, 2, 3, 0,
       doc: /* Set SYMBOL's toplevel buffer-local value in BUFFER to VALUE.
"Toplevel" means outside of any let-binding.
BUFFER defaults to the current buffer.
Makes SYMBOL buffer-local in BUFFER if it was not already.  */)
     (Lisp_Object symbol, Lisp_Object value, Lisp_Object buffer)
{
  Lisp_Object buf = !NILP (buffer) ? buffer : Fcurrent_buffer ();
  union specbinding *binding = local_toplevel_binding (symbol, buf);

  if (binding)
    set_specpdl_old_value (binding, value);
  else if (NILP (buffer))
    Fset (Fmake_local_variable (symbol), value);
  else
    {
      specpdl_ref count = SPECPDL_INDEX ();
      record_unwind_current_buffer ();
      Fset_buffer (buffer);
      Fset (Fmake_local_variable (symbol), value);
      unbind_to (count, Qnil);
    }

  return Qnil;
}

DEFUN ("internal--define-uninitialized-variable",
       Finternal__define_uninitialized_variable,
       Sinternal__define_uninitialized_variable, 1, 2, 0,
       doc: /* Define SYMBOL as a variable, with DOC as its docstring.
This is like `defvar' and `defconst' but without affecting the variable's
value.  */)
  (Lisp_Object symbol, Lisp_Object doc)
{
  if (!XSYMBOL (symbol)->u.s.declared_special
      && lexbound_p (symbol))
    /* This test tries to catch the situation where we do
       (let ((<foo-var> ...)) ...(<foo-function> ...)....)
       and where the `foo` package only gets loaded when <foo-function>
       is called, so the outer `let` incorrectly made the binding lexical
       because the <foo-var> wasn't yet declared as dynamic at that point.  */
    xsignal2 (Qerror,
	      build_string ("Defining as dynamic an already lexical var"),
	      symbol);

  XSYMBOL (symbol)->u.s.declared_special = true;
  if (!NILP (doc))
    {
      Fput (symbol, Qvariable_documentation, doc);
    }
  LOADHIST_ATTACH (symbol);
  return Qnil;
}

static Lisp_Object
defvar (Lisp_Object sym, Lisp_Object initvalue, Lisp_Object docstring, bool eval)
{
  Lisp_Object tem;

  CHECK_SYMBOL (sym);

  tem = Fdefault_boundp (sym);

  /* Do it before evaluating the initial value, for self-references.  */
  Finternal__define_uninitialized_variable (sym, docstring);

  if (NILP (tem))
    Fset_default (sym, eval ? eval_sub (initvalue) : initvalue);
  else
    { /* Check if there is really a global binding rather than just a let
	     binding that shadows the global unboundness of the var.  */
      union specbinding *binding = default_toplevel_binding (sym);
      if (binding && BASE_EQ (specpdl_old_value (binding), Qunbound))
	{
	  set_specpdl_old_value (binding,
	                         eval ? eval_sub (initvalue) : initvalue);
	}
    }
  return sym;
}

DEFUN ("defvar", Fdefvar, Sdefvar, 1, UNEVALLED, 0,
       doc: /* Define SYMBOL as a variable, and return SYMBOL.
You are not required to define a variable in order to use it, but
defining it lets you supply an initial value and documentation, which
can be referred to by the Emacs help facilities and other programming
tools.

If SYMBOL's value is void and the optional argument INITVALUE is
provided, INITVALUE is evaluated and the result used to set SYMBOL's
value.  If SYMBOL is buffer-local, its default value is what is set;
buffer-local values are not affected.  If INITVALUE is missing,
SYMBOL's value is not set.

If INITVALUE is provided, the `defvar' form also declares the variable
as \"special\", so that it is always dynamically bound even if
`lexical-binding' is t.  If INITVALUE is missing, the form marks the
variable \"special\" locally (i.e., within the current
lexical scope, or the current file, if the form is at top-level),
and does nothing if `lexical-binding' is nil.

If SYMBOL is let-bound, then this form does not affect the local let
binding but the toplevel default binding instead, like
`set-toplevel-default-binding`.
(`defcustom' behaves similarly in this respect.)

The optional argument DOCSTRING is a documentation string for the
variable.

To define a user option, use `defcustom' instead of `defvar'.

To define a buffer-local variable, use `defvar-local'.
usage: (defvar SYMBOL &optional INITVALUE DOCSTRING)  */)
  (Lisp_Object args)
{
  Lisp_Object sym, tail;

  sym = XCAR (args);
  tail = XCDR (args);

  CHECK_SYMBOL (sym);

  if (!NILP (tail))
    {
      if (!NILP (XCDR (tail)) && !NILP (XCDR (XCDR (tail))))
	error ("Too many arguments");
      Lisp_Object exp = XCAR (tail);
      tail = XCDR (tail);
      return defvar (sym, exp, CAR (tail), true);
    }
  else if (!NILP (Vinternal_interpreter_environment)
	   && (SYMBOLP (sym) && !XSYMBOL (sym)->u.s.declared_special))
    /* A simple (defvar foo) with lexical scoping does "nothing" except
       declare that var to be dynamically scoped *locally* (i.e. within
       the current file or let-block).  */
    Vinternal_interpreter_environment
      = Fcons (sym, Vinternal_interpreter_environment);
  else
    {
      /* Simple (defvar <var>) should not count as a definition at all.
	 It could get in the way of other definitions, and unloading this
	 package could try to make the variable unbound.  */
    }

  return sym;
}

DEFUN ("defvar-1", Fdefvar_1, Sdefvar_1, 2, 3, 0,
       doc: /* Like `defvar' but as a function.
More specifically behaves like (defvar SYM \\='INITVALUE DOCSTRING).  */)
  (Lisp_Object sym, Lisp_Object initvalue, Lisp_Object docstring)
{
  return defvar (sym, initvalue, docstring, false);
}

DEFUN ("defconst", Fdefconst, Sdefconst, 2, UNEVALLED, 0,
       doc: /* Define SYMBOL as a constant variable.
This declares that neither programs nor users should ever change the
value.  This constancy is not actually enforced by Emacs Lisp, but
SYMBOL is marked as a special variable so that it is never lexically
bound.

The `defconst' form always sets the value of SYMBOL to the result of
evalling INITVALUE.  If SYMBOL is buffer-local, its default value is
what is set; buffer-local values are not affected.  If SYMBOL has a
local binding, then this form sets the local binding's value.
However, you should normally not make local bindings for variables
defined with this form.

The optional DOCSTRING specifies the variable's documentation string.
usage: (defconst SYMBOL INITVALUE [DOCSTRING])  */)
  (Lisp_Object args)
{
  Lisp_Object sym, tem;

  sym = XCAR (args);
  CHECK_SYMBOL (sym);
  Lisp_Object docstring = Qnil;
  if (!NILP (XCDR (XCDR (args))))
    {
      if (!NILP (XCDR (XCDR (XCDR (args)))))
	error ("Too many arguments");
      docstring = XCAR (XCDR (XCDR (args)));
    }
  tem = eval_sub (XCAR (XCDR (args)));
  return Fdefconst_1 (sym, tem, docstring);
}

DEFUN ("defconst-1", Fdefconst_1, Sdefconst_1, 2, 3, 0,
       doc: /* Like `defconst' but as a function.
More specifically, behaves like (defconst SYM \\='INITVALUE DOCSTRING).  */)
  (Lisp_Object sym, Lisp_Object initvalue, Lisp_Object docstring)
{
  CHECK_SYMBOL (sym);
  Lisp_Object tem = initvalue;
  Finternal__define_uninitialized_variable (sym, docstring);
  Fset_default (sym, tem);      /* FIXME: set-default-toplevel-value? */
  Fput (sym, Qrisky_local_variable, Qt); /* FIXME: Why?  */
  return sym;
}

/* Make SYMBOL lexically scoped.  */
DEFUN ("internal-make-var-non-special", Fmake_var_non_special,
       Smake_var_non_special, 1, 1, 0,
       doc: /* Internal function.  */)
     (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  XSYMBOL (symbol)->u.s.declared_special = false;
  return Qnil;
}


DEFUN ("let*", FletX, SletX, 1, UNEVALLED, 0,
       doc: /* Bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
Each VALUEFORM can refer to the symbols already bound by this VARLIST.
usage: (let* VARLIST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object var, val, elt, lexenv;
  specpdl_ref count = SPECPDL_INDEX ();

  lexenv = Vinternal_interpreter_environment;

  Lisp_Object varlist = XCAR (args);
  FOR_EACH_TAIL (varlist)
    {
      elt = XCAR (varlist);
      if (SYMBOLP (elt))
	{
	  var = elt;
	  val = Qnil;
	}
      else
	{
	  var = Fcar (elt);
	  if (! NILP (Fcdr (XCDR (elt))))
	    signal_error ("`let' bindings can have only one value-form", elt);
	  val = eval_sub (Fcar (XCDR (elt)));
	}

      var = maybe_remove_pos_from_symbol (var);
      CHECK_TYPE (BARE_SYMBOL_P (var), Qsymbolp, var);
      if (!NILP (lexenv) && !XBARE_SYMBOL (var)->u.s.declared_special
	  && NILP (Fmemq (var, Vinternal_interpreter_environment)))
	/* Lexically bind VAR by adding it to the interpreter's binding
	   alist.  */
	{
	  Lisp_Object newenv
	    = Fcons (Fcons (var, val), Vinternal_interpreter_environment);
	  if (BASE_EQ (Vinternal_interpreter_environment, lexenv))
	    /* Save the old lexical environment on the specpdl stack,
	       but only for the first lexical binding, since we'll never
	       need to revert to one of the intermediate ones.  */
	    specbind (Qinternal_interpreter_environment, newenv);
	  else
	    Vinternal_interpreter_environment = newenv;
	}
      else
	specbind (var, val);
    }
  CHECK_LIST_END (varlist, XCAR (args));

  val = Fprogn (XCDR (args));
  return unbind_to (count, val);
}

DEFUN ("let", Flet, Slet, 1, UNEVALLED, 0,
       doc: /* Bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
All the VALUEFORMs are evalled before any symbols are bound.
usage: (let VARLIST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object *temps, tem, lexenv;
  Lisp_Object elt;
  specpdl_ref count = SPECPDL_INDEX ();
  ptrdiff_t argnum;
  USE_SAFE_ALLOCA;

  Lisp_Object varlist = XCAR (args);

  /* Make space to hold the values to give the bound variables.  */
  EMACS_INT varlist_len = list_length (varlist);
  SAFE_ALLOCA_LISP (temps, varlist_len);
  ptrdiff_t nvars = varlist_len;

  /* Compute the values and store them in `temps'.  */

  for (argnum = 0; argnum < nvars && CONSP (varlist); argnum++)
    {
      maybe_quit ();
      elt = XCAR (varlist);
      varlist = XCDR (varlist);
      if (SYMBOLP (elt))
	temps[argnum] = Qnil;
      else if (! NILP (Fcdr (Fcdr (elt))))
	signal_error ("`let' bindings can have only one value-form", elt);
      else
	temps[argnum] = eval_sub (Fcar (Fcdr (elt)));
    }
  nvars = argnum;

  lexenv = Vinternal_interpreter_environment;

  varlist = XCAR (args);
  for (argnum = 0; argnum < nvars && CONSP (varlist); argnum++)
    {
      elt = XCAR (varlist);
      varlist = XCDR (varlist);
      Lisp_Object var = maybe_remove_pos_from_symbol (SYMBOLP (elt) ? elt
						      : Fcar (elt));
      CHECK_TYPE (BARE_SYMBOL_P (var), Qsymbolp, var);
      tem = temps[argnum];

      if (!NILP (lexenv) && !XBARE_SYMBOL (var)->u.s.declared_special
	  && NILP (Fmemq (var, Vinternal_interpreter_environment)))
	/* Lexically bind VAR by adding it to the lexenv alist.  */
	lexenv = Fcons (Fcons (var, tem), lexenv);
      else
	/* Dynamically bind VAR.  */
	specbind (var, tem);
    }

  if (!BASE_EQ (lexenv, Vinternal_interpreter_environment))
    /* Instantiate a new lexical environment.  */
    specbind (Qinternal_interpreter_environment, lexenv);

  elt = Fprogn (XCDR (args));
  return SAFE_FREE_UNBIND_TO (count, elt);
}

DEFUN ("while", Fwhile, Swhile, 1, UNEVALLED, 0,
       doc: /* If TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil.

The value of a `while' form is always nil.

usage: (while TEST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object test, body;

  test = XCAR (args);
  body = XCDR (args);
  while (!NILP (eval_sub (test)))
    {
      maybe_quit ();
      prog_ignore (body);
    }

  return Qnil;
}

static void
with_delayed_message_display (struct atimer *timer)
{
  message3 (build_string (timer->client_data));
}

static void
with_delayed_message_cancel (void *timer)
{
  xfree (((struct atimer *) timer)->client_data);
  cancel_atimer (timer);
}

DEFUN ("funcall-with-delayed-message",
       Ffuncall_with_delayed_message, Sfuncall_with_delayed_message,
       3, 3, 0,
       doc: /* Like `funcall', but display MESSAGE if FUNCTION takes longer than TIMEOUT.
TIMEOUT is a number of seconds, and can be an integer or a floating
point number.

If FUNCTION takes less time to execute than TIMEOUT seconds, MESSAGE
is not displayed.  */)
  (Lisp_Object timeout, Lisp_Object message, Lisp_Object function)
{
  specpdl_ref count = SPECPDL_INDEX ();

  CHECK_NUMBER (timeout);
  CHECK_STRING (message);

  /* Set up the atimer.  */
  struct timespec interval = dtotimespec (XFLOATINT (timeout));
  struct atimer *timer = start_atimer (ATIMER_RELATIVE, interval,
				       with_delayed_message_display,
				       xstrdup (SSDATA (message)));
  record_unwind_protect_ptr (with_delayed_message_cancel, timer);

  Lisp_Object result = calln (function);

  return unbind_to (count, result);
}

DEFUN ("macroexpand", Fmacroexpand, Smacroexpand, 1, 2, 0,
       doc: /* Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation.  */)
  (Lisp_Object form, Lisp_Object environment)
{
  /* With cleanups from Hallvard Furuseth.  */
  register Lisp_Object expander, sym, def, tem;

  while (1)
    {
      /* Come back here each time we expand a macro call,
	 in case it expands into another macro call.  */
      if (!CONSP (form))
	break;
      /* Set SYM, give DEF and TEM right values in case SYM is not a symbol. */
      def = sym = XCAR (form);
      tem = Qnil;
      /* Trace symbols aliases to other symbols
	 until we get a symbol that is not an alias.  */
      while (SYMBOLP (def))
	{
	  maybe_quit ();
	  sym = def;
	  tem = Fassq (sym, environment);
	  if (NILP (tem))
	    {
	      def = XSYMBOL (sym)->u.s.function;
	      if (!NILP (def))
		continue;
	    }
	  break;
	}
      /* Right now TEM is the result from SYM in ENVIRONMENT,
	 and if TEM is nil then DEF is SYM's function definition.  */
      if (NILP (tem))
	{
	  /* SYM is not mentioned in ENVIRONMENT.
	     Look at its function definition.  */
	  def = Fautoload_do_load (def, sym, Qmacro);
	  if (!CONSP (def))
	    /* Not defined or definition not suitable.  */
	    break;
	  if (!EQ (XCAR (def), Qmacro))
	    break;
	  else expander = XCDR (def);
	}
      else
	{
	  expander = XCDR (tem);
	  if (NILP (expander))
	    break;
	}
      {
	Lisp_Object newform = apply1 (expander, XCDR (form));
	if (EQ (form, newform))
	  break;
	else
	  form = newform;
      }
    }
  return form;
}

DEFUN ("catch", Fcatch, Scatch, 1, UNEVALLED, 0,
       doc: /* Eval BODY allowing nonlocal exits using `throw'.
TAG is evalled to get the tag to use; it must not be nil.

Then the BODY is executed.
Within BODY, a call to `throw' with the same TAG exits BODY and this `catch'.
If no throw happens, `catch' returns the value of the last BODY form.
If a throw happens, it specifies the value to return from `catch'.
usage: (catch TAG BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object tag = eval_sub (XCAR (args));
  return internal_catch (tag, Fprogn, XCDR (args));
}

/* Assert that E is true, but do not evaluate E.  Use this instead of
   eassert (E) when E contains variables that might be clobbered by a
   longjmp.  */

#define clobbered_eassert(E) static_assert (sizeof (E) != 0)

void
pop_handler (void)
{
  handlerlist = handlerlist->next;
}

/* Set up a catch, then call C function FUNC on argument ARG.
   FUNC should return a Lisp_Object.
   This is how catches are done from within C code.  */

Lisp_Object
internal_catch (Lisp_Object tag,
		Lisp_Object (*func) (Lisp_Object), Lisp_Object arg)
{
  /* This structure is made part of the chain `catchlist'.  */
  struct handler *c = push_handler (tag, CATCHER);

  /* Call FUNC.  */
  if (! sys_setjmp (c->jmp))
    {
      Lisp_Object val = func (arg);
      eassert (handlerlist == c);
      handlerlist = c->next;
      return val;
    }
  else
    { /* Throw works by a longjmp that comes right here.  */
      Lisp_Object val = handlerlist->val;
      clobbered_eassert (handlerlist == c);
      handlerlist = handlerlist->next;
      return val;
    }
}

/* Unwind the specbind, catch, and handler stacks back to CATCH, and
   jump to that CATCH, returning VALUE as the value of that catch.

   This is the guts of Fthrow and Fsignal; they differ only in the way
   they choose the catch tag to throw to.  A catch tag for a
   condition-case form has a TAG of Qnil.

   Before each catch is discarded, unbind all special bindings and
   execute all unwind-protect clauses made above that catch.  Unwind
   the handler stack as we go, so that the proper handlers are in
   effect for each unwind-protect clause we run.  At the end, restore
   some static info saved in CATCH, and longjmp to the location
   specified there.

   This is used for correct unwinding in Fthrow and Fsignal.  */

static AVOID
unwind_to_catch (struct handler *catch, enum nonlocal_exit type,
                 Lisp_Object value)
{
  bool last_time;

  eassert (catch->next);

  /* Save the value in the tag.  */
  catch->nonlocal_exit = type;
  catch->val = value;

  /* Restore certain special C variables.  */
  set_poll_suppress_count (catch->poll_suppress_count);
  unblock_input_to (catch->interrupt_input_blocked);

#ifdef HAVE_X_WINDOWS
  /* Restore the X error handler stack.  This is important because
     otherwise a display disconnect won't unwind the stack of error
     traps to the right depth.  */
  x_unwind_errors_to (catch->x_error_handler_depth);
#endif

  do
    {
      /* Unwind the specpdl stack, and then restore the proper set of
	 handlers.  */
      unbind_to (handlerlist->pdlcount, Qnil);
      last_time = handlerlist == catch;
      if (! last_time)
	handlerlist = handlerlist->next;
    }
  while (! last_time);

  eassert (handlerlist == catch);

  lisp_eval_depth = catch->f_lisp_eval_depth;
  set_act_rec (current_thread, catch->act_rec);

  sys_longjmp (catch->jmp, 1);
}

DEFUN ("throw", Fthrow, Sthrow, 2, 2, 0,
       doc: /* Throw to the catch for TAG and return VALUE from it.
Both TAG and VALUE are evalled.  */
       attributes: noreturn)
  (register Lisp_Object tag, Lisp_Object value)
{
  struct handler *c;

  if (!NILP (tag))
    for (c = handlerlist; c; c = c->next)
      {
	if (c->type == CATCHER_ALL)
          unwind_to_catch (c, NONLOCAL_EXIT_THROW, Fcons (tag, value));
        if (c->type == CATCHER && EQ (c->tag_or_ch, tag))
	  unwind_to_catch (c, NONLOCAL_EXIT_THROW, value);
      }
  xsignal2 (Qno_catch, tag, value);
}


DEFUN ("unwind-protect", Funwind_protect, Sunwind_protect, 1, UNEVALLED, 0,
       doc: /* Do BODYFORM, protecting with UNWINDFORMS.
If BODYFORM completes normally, its value is returned
after executing the UNWINDFORMS.
If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.
usage: (unwind-protect BODYFORM UNWINDFORMS...)  */)
  (Lisp_Object args)
{
  Lisp_Object val;
  specpdl_ref count = SPECPDL_INDEX ();

  record_unwind_protect (prog_ignore, XCDR (args));
  val = eval_sub (XCAR (args));
  return unbind_to (count, val);
}

DEFUN ("condition-case", Fcondition_case, Scondition_case, 2, UNEVALLED, 0,
       doc: /* Regain control when an error is signaled.
Executes BODYFORM and returns its value if no error happens.
Each element of HANDLERS looks like (CONDITION-NAME BODY...)
or (:success BODY...), where the BODY is made of Lisp expressions.

A handler is applicable to an error if CONDITION-NAME is one of the
error's condition names.  Handlers may also apply when non-error
symbols are signaled (e.g., `quit').  A CONDITION-NAME of t applies to
any symbol, including non-error symbols.  If multiple handlers are
applicable, only the first one runs.

The car of a handler may be a list of condition names instead of a
single condition name; then it handles all of them.  If the special
condition name `debug' is present in this list, it allows another
condition in the list to run the debugger if `debug-on-error' and the
other usual mechanisms say it should (otherwise, `condition-case'
suppresses the debugger).

When a handler handles an error, control returns to the `condition-case'
and it executes the handler's BODY...
with VAR bound to (ERROR-SYMBOL . SIGNAL-DATA) from the error.
\(If VAR is nil, the handler can't access that information.)
Then the value of the last BODY form is returned from the `condition-case'
expression.

The special handler (:success BODY...) is invoked if BODYFORM terminated
without signaling an error.  BODY is then evaluated with VAR bound to
the value returned by BODYFORM.

See also the function `signal' for more info.
usage: (condition-case VAR BODYFORM &rest HANDLERS)  */)
  (Lisp_Object args)
{
  Lisp_Object var = XCAR (args);
  Lisp_Object bodyform = XCAR (XCDR (args));
  Lisp_Object handlers = XCDR (XCDR (args));

  return internal_lisp_condition_case (var, bodyform, handlers);
}

void
push_handler_bind (Lisp_Object conditions, Lisp_Object handler, int skip)
{
  if (!CONSP (conditions))
    conditions = Fcons (conditions, Qnil);
  struct handler *c = push_handler (conditions, HANDLER_BIND);
  c->val = handler;
  c->bytecode_dest = skip;
}

DEFUN ("handler-bind-1", Fhandler_bind_1, Shandler_bind_1, 1, MANY, 0,
       doc: /* Set up error handlers around execution of BODYFUN.
BODYFUN should be a function and it is called with no arguments.
CONDITIONS should be a list of condition names (symbols).
When an error is signaled during execution of BODYFUN, if that
error matches one of CONDITIONS, then the associated HANDLER is
called with the error as argument.
HANDLER should either transfer the control via a non-local exit,
or return normally.
If it returns normally, the search for an error handler continues
from where it left off.

usage: (handler-bind BODYFUN [CONDITIONS HANDLER]...)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  eassert (nargs >= 1);
  Lisp_Object bodyfun = args[0];
  int count = 0;
  if (nargs % 2 == 0)
    error ("Trailing CONDITIONS without HANDLER in `handler-bind`");
  for (ptrdiff_t i = nargs - 2; i > 0; i -= 2)
    {
      Lisp_Object conditions = args[i], handler = args[i + 1];
      if (NILP (conditions))
        continue;
      push_handler_bind (conditions, handler, count++);
    }
  Lisp_Object ret = call0 (bodyfun);
  for (; count > 0; count--)
    pop_handler ();
  return ret;
}

/* Like Fcondition_case, but the args are separate
   rather than passed in a list.  Used by Fbyte_code.  */

Lisp_Object
internal_lisp_condition_case (Lisp_Object var, Lisp_Object bodyform,
			      Lisp_Object handlers)
{
  struct handler *oldhandlerlist = handlerlist;

  /* The number of non-success handlers, plus 1 for a sentinel.  */
  ptrdiff_t clausenb = 1;

  var = maybe_remove_pos_from_symbol (var);
  CHECK_TYPE (BARE_SYMBOL_P (var), Qsymbolp, var);

  Lisp_Object success_handler = Qnil;

  for (Lisp_Object tail = handlers; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object tem = XCAR (tail);
      if (! (NILP (tem)
	     || (CONSP (tem)
		 && (SYMBOLP (XCAR (tem))
		     || CONSP (XCAR (tem))))))
	error ("Invalid condition handler: %s",
	       SDATA (Fprin1_to_string (tem, Qt, Qnil)));
      if (CONSP (tem) && EQ (XCAR (tem), QCsuccess))
	success_handler = tem;
      else
	clausenb++;
    }

  /* The first clause is the one that should be checked first, so it
     should be added to handlerlist last.  So build in CLAUSES a table
     that contains HANDLERS but in reverse order.  CLAUSES is pointer
     to volatile to avoid issues with setjmp and local storage.
     SAFE_ALLOCA won't work here due to the setjmp, so impose a
     MAX_ALLOCA limit.  */
  if (MAX_ALLOCA / word_size < clausenb)
    memory_full (SIZE_MAX);
  Lisp_Object volatile *clauses = alloca (clausenb * sizeof *clauses);
  clauses += clausenb;
  *--clauses = make_fixnum (0);
  for (Lisp_Object tail = handlers; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object tem = XCAR (tail);
      if (!(CONSP (tem) && EQ (XCAR (tem), QCsuccess)))
	*--clauses = tem;
    }
  Lisp_Object volatile var_volatile = var;
  Lisp_Object val, handler_body;
  for (Lisp_Object volatile *pcl = clauses; ; pcl++)
    {
      if (BASE_EQ (*pcl, make_fixnum (0)))
	{
	  val = eval_sub (bodyform);
	  handlerlist = oldhandlerlist;
	  if (NILP (success_handler))
	    return val;
#if GCC_LINT && __GNUC__ && !__clang__
	  /* This useless assignment pacifies GCC 14.2.1 x86-64
	     <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=21161>.  */
	  var = var_volatile;
#endif
	  handler_body = XCDR (success_handler);
	  break;
	}
      Lisp_Object clause = *pcl;
      Lisp_Object condition = CONSP (clause) ? XCAR (clause) : Qnil;
      if (!CONSP (condition))
	condition = list1 (condition);
      struct handler *c = push_handler (condition, CONDITION_CASE);
      if (sys_setjmp (c->jmp))
	{
	  var = var_volatile;
	  val = handlerlist->val;
	  Lisp_Object volatile *chosen_clause = clauses;
	  struct handler *oldh = oldhandlerlist;
	  for (struct handler *h = handlerlist->next; h != oldh; h = h->next)
	    chosen_clause++;
	  handler_body = XCDR (*chosen_clause);
	  handlerlist = oldh;

	  /* Whoever longjumped to us unwound the stack to C->pdlcount before
	     throwing, so the unbind_to will undo just this binding.  */
	  break;
	}
    }

  if (NILP (var))
    return Fprogn (handler_body);

  if (!NILP (Vinternal_interpreter_environment))
    {
      val = Fcons (Fcons (var, val),
		   Vinternal_interpreter_environment);
      var = Qinternal_interpreter_environment;
    }

  specpdl_ref count = SPECPDL_INDEX ();
  specbind (var, val);
  return unbind_to (count, Fprogn (handler_body));
}

/* Call the function BFUN with no arguments, catching errors within it
   according to HANDLERS.  If there is an error, call HFUN with
   one argument which is the data that describes the error:
   (SIGNALNAME . DATA)

   HANDLERS can be a list of conditions to catch.
   If HANDLERS is Qt, catch all errors.
   If HANDLERS is Qerror, catch all errors
   but allow the debugger to run if that is enabled.  */

Lisp_Object
internal_condition_case (Lisp_Object (*bfun) (void), Lisp_Object handlers,
			 Lisp_Object (*hfun) (Lisp_Object))
{
  struct handler *c = push_handler (handlers, CONDITION_CASE);
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = handlerlist->val;
      clobbered_eassert (handlerlist == c);
      handlerlist = handlerlist->next;
      return hfun (val);
    }
  else
    {
      Lisp_Object val = bfun ();
      eassert (handlerlist == c);
      handlerlist = c->next;
      return val;
    }
}

/* Like internal_condition_case but call BFUN with ARG as its argument.  */

Lisp_Object
internal_condition_case_1 (Lisp_Object (*bfun) (Lisp_Object), Lisp_Object arg,
			   Lisp_Object handlers,
			   Lisp_Object (*hfun) (Lisp_Object))
{
  struct handler *c = push_handler (handlers, CONDITION_CASE);
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = handlerlist->val;
      clobbered_eassert (handlerlist == c);
      handlerlist = handlerlist->next;
      return hfun (val);
    }
  else
    {
      Lisp_Object val = bfun (arg);
      eassert (handlerlist == c);
      handlerlist = c->next;
      return val;
    }
}

/* Like internal_condition_case_1 but call BFUN with ARG1 and ARG2 as
   its arguments.  */

Lisp_Object
internal_condition_case_2 (Lisp_Object (*bfun) (Lisp_Object, Lisp_Object),
			   Lisp_Object arg1,
			   Lisp_Object arg2,
			   Lisp_Object handlers,
			   Lisp_Object (*hfun) (Lisp_Object))
{
  struct handler *c = push_handler (handlers, CONDITION_CASE);
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = handlerlist->val;
      clobbered_eassert (handlerlist == c);
      handlerlist = handlerlist->next;
      return hfun (val);
    }
  else
    {
      Lisp_Object val = bfun (arg1, arg2);
      eassert (handlerlist == c);
      handlerlist = c->next;
      return val;
    }
}

/* Like internal_condition_case but call BFUN with NARGS as first,
   and ARGS as second argument.  */

Lisp_Object
internal_condition_case_n (Lisp_Object (*bfun) (ptrdiff_t, Lisp_Object *),
			   ptrdiff_t nargs,
			   Lisp_Object *args,
			   Lisp_Object handlers,
			   Lisp_Object (*hfun) (Lisp_Object err,
						ptrdiff_t nargs,
						Lisp_Object *args))
{
  struct handler *c = push_handler (handlers, CONDITION_CASE);
  if (sys_setjmp (c->jmp))
    {
      Lisp_Object val = handlerlist->val;
      clobbered_eassert (handlerlist == c);
      handlerlist = handlerlist->next;
      return hfun (val, nargs, args);
    }
  else
    {
      Lisp_Object val = bfun (nargs, args);
      eassert (handlerlist == c);
      handlerlist = c->next;
      return val;
    }
}

static Lisp_Object Qcatch_all_memory_full;

/* Like a combination of internal_condition_case_1 and internal_catch.
   Catches all signals and throws.  Never exits nonlocally; returns
   Qcatch_all_memory_full if no handler could be allocated.  */

Lisp_Object
internal_catch_all (Lisp_Object (*function) (void *), void *argument,
                    Lisp_Object (*handler) (enum nonlocal_exit, Lisp_Object))
{
  struct handler *c = push_handler_nosignal (Qt, CATCHER_ALL);
  if (c == NULL)
    return Qcatch_all_memory_full;

  if (sys_setjmp (c->jmp) == 0)
    {
      Lisp_Object val = function (argument);
      eassert (handlerlist == c);
      handlerlist = c->next;
      return val;
    }
  else
    {
      eassert (handlerlist == c);
      enum nonlocal_exit type = c->nonlocal_exit;
      Lisp_Object val = c->val;
      handlerlist = c->next;
      return handler (type, val);
    }
}

struct handler *
push_handler (Lisp_Object tag_ch_val, enum handlertype handlertype)
{
  struct handler *c = push_handler_nosignal (tag_ch_val, handlertype);
  if (!c)
    memory_full (sizeof *c);
  return c;
}

struct handler *
push_handler_nosignal (Lisp_Object tag_ch_val, enum handlertype handlertype)
{
  struct handler *c = handlerlist->nextfree;
  if (!c)
    {
      c = malloc (sizeof *c);
      if (!c)
	return c;
      if (profiler_memory_running)
	malloc_probe (sizeof *c);
      c->nextfree = NULL;
      handlerlist->nextfree = c;
    }
  c->type = handlertype;
  c->tag_or_ch = tag_ch_val;
  c->val = Qnil;
  c->next = handlerlist;
  c->f_lisp_eval_depth = lisp_eval_depth;
  c->pdlcount = SPECPDL_INDEX ();
  c->act_rec = get_act_rec (current_thread);
  c->poll_suppress_count = poll_suppress_count;
  c->interrupt_input_blocked = interrupt_input_blocked;
#ifdef HAVE_X_WINDOWS
  c->x_error_handler_depth = x_error_message_count;
#endif
  handlerlist = c;
  return c;
}


static Lisp_Object signal_or_quit (Lisp_Object, Lisp_Object, bool);
static Lisp_Object find_handler_clause (Lisp_Object, Lisp_Object);
static bool maybe_call_debugger (Lisp_Object conditions, Lisp_Object error);

static void
process_quit_flag (void)
{
  Lisp_Object flag = Vquit_flag;
  Vquit_flag = Qnil;
  if (EQ (flag, Qkill_emacs))
    Fkill_emacs (Qnil, Qnil);
  if (EQ (Vthrow_on_input, flag))
    Fthrow (Vthrow_on_input, Qt);
  quit ();
}

void
probably_quit (void)
{
  specpdl_ref gc_count = inhibit_garbage_collection ();
  if (!NILP (Vquit_flag) && NILP (Vinhibit_quit))
    process_quit_flag ();
  else if (pending_signals)
    process_pending_signals ();
  unbind_to (gc_count, Qnil);
}

DEFUN ("signal", Fsignal, Ssignal, 2, 2, 0,
       doc: /* Signal an error.  Args are ERROR-SYMBOL and associated DATA.
This function does not return.

When `noninteractive' is non-nil (in particular, in batch mode), an
unhandled error calls `kill-emacs', which terminates the Emacs
session with a non-zero exit code.

An error symbol is a symbol with an `error-conditions' property
that is a list of condition names.  The symbol should be non-nil.
A handler for any of those names will get to handle this signal.
The symbol `error' should normally be one of them.

DATA should be a list.  Its elements are printed as part of the error message.
See Info anchor `(elisp)Definition of signal' for some details on how this
error message is constructed.
If the signal is handled, DATA is made available to the handler.
See also the function `condition-case'.  */
       attributes: noreturn)
  (Lisp_Object error_symbol, Lisp_Object data)
{
  /* If they call us with nonsensical arguments, produce "peculiar error".  */
  if (NILP (error_symbol) && !CONSP (data))
    error_symbol = Qerror;
  signal_or_quit (error_symbol, data, false);
  eassume (false);
}

/* Quit, in response to a keyboard quit request.  */
Lisp_Object
quit (void)
{
  return signal_or_quit (Qquit, Qnil, true);
}

/* Signal an error, or quit.  ERROR_SYMBOL and DATA are as with Fsignal.
   If CONTINUABLE, the caller allows this function to return
   (presumably after calling the debugger);
   Otherwise this function is like Fsignal and does not return.  */

static Lisp_Object
signal_or_quit (Lisp_Object error_symbol, Lisp_Object data, bool continuable)
{
  /* When memory is full, ERROR-SYMBOL is nil,
     and DATA is (REAL-ERROR-SYMBOL . REAL-DATA).
     That is a special case--don't do this in other situations.  */
  bool oom = NILP (error_symbol);
  Lisp_Object error             /* The error object.  */
    = oom ? data
      : (!SYMBOLP (error_symbol) && NILP (data)) ? error_symbol
      : Fcons (error_symbol, data);
  Lisp_Object conditions;
  Lisp_Object string;
  Lisp_Object real_error_symbol
    = CONSP (error) ? XCAR (error) : error_symbol;
  Lisp_Object clause = Qnil;
  struct handler *h;
  int skip;

  if (gc_in_progress || waiting_for_input)
    emacs_abort ();

  /* This hook is used by edebug.  */
  if (! NILP (Vsignal_hook_function)
      && !oom)
    {
      specpdl_ref count = SPECPDL_INDEX ();
      max_ensure_room (20);
      /* FIXME: 'handler-bind' makes `signal-hook-function' obsolete?  */
      /* FIXME: Here we still "split" the error object
         into its error-symbol and its error-data?  */
      calln (Vsignal_hook_function, error_symbol, data);
      unbind_to (count, Qnil);
    }

  conditions = Fget (real_error_symbol, Qerror_conditions);
  if (NILP (conditions))
    signal_error ("Invalid error symbol", error_symbol);

  /* Remember from where signal was called.  Skip over the frame for
     `signal' itself.  If a frame for `error' follows, skip that,
     too.  Don't do this when ERROR_SYMBOL is nil, because that
     is a memory-full error.  */
  Vsignaling_function = Qnil;
  if (!oom)
    {
      union specbinding *pdl = backtrace_next (backtrace_top ());
      if (backtrace_p (pdl) && EQ (backtrace_function (pdl), Qerror))
	pdl = backtrace_next (pdl);
      if (backtrace_p (pdl))
	Vsignaling_function = backtrace_function (pdl);
    }

  for (skip = 0, h = handlerlist; h; skip++, h = h->next)
    {
      switch (h->type)
        {
        case CATCHER_ALL:
          clause = Qt;
          break;
	case CATCHER:
	  continue;
        case CONDITION_CASE:
          clause = find_handler_clause (h->tag_or_ch, conditions);
	  break;
	case HANDLER_BIND:
	  {
	    if (!NILP (find_handler_clause (h->tag_or_ch, conditions)))
	      {
	        specpdl_ref count = SPECPDL_INDEX ();
		/* Add some room in case this is for debugging, as in
		   call_debugger.  */
	        max_ensure_room (200);
	        push_handler (make_fixnum (skip + h->bytecode_dest),
	                      SKIP_CONDITIONS);
	        calln (h->val, error);
	        unbind_to (count, Qnil);
	        pop_handler ();
	      }
	    continue;
	  }
	case SKIP_CONDITIONS:
	  {
	    int toskip = XFIXNUM (h->tag_or_ch);
	    while (toskip-- >= 0)
	      h = h->next;
	    continue;
	  }
	default:
	  abort ();
	}
      if (!NILP (clause))
	break;
    }

  if (/* Don't run the debugger for a memory-full error.
	 (There is no room in memory to do that!)  */
      !oom
      && (!NILP (Vdebug_on_signal)
	  /* If no handler is present now, try to run the debugger.  */
	  || NILP (clause)
	  /* A `debug' symbol in the handler list disables the normal
	     suppression of the debugger.  */
	  || (CONSP (clause) && !NILP (Fmemq (Qdebug, clause)))
	  /* Special handler that means "print a message and run debugger
	     if requested".  */
	  || EQ (clause, Qerror)))
    {
      bool debugger_called
	= maybe_call_debugger (conditions, error);
      /* We can't return values to code which signaled an error, but we
	 can continue code which has signaled a quit.  */
      if (continuable && debugger_called)
	return Qnil;
    }

  if (!NILP (clause))
    unwind_to_catch (h, NONLOCAL_EXIT_SIGNAL, error);
  else if (handlerlist != handlerlist_sentinel)
    /* FIXME: This will come right back here if there's no `top-level'
       catcher.  A better solution would be to abort here, and instead
       add a catch-all condition handler so we never come here.  */
    Fthrow (Qtop_level, Qt);

  string = Ferror_message_string (error);
  fatal ("%s", SDATA (string));
}

/* Like xsignal, but takes 0, 1, 2, or 3 args instead of a list.  */

void
xsignal0 (Lisp_Object error_symbol)
{
  xsignal (error_symbol, Qnil);
}

void
xsignal1 (Lisp_Object error_symbol, Lisp_Object arg)
{
  xsignal (error_symbol, list1 (arg));
}

void
xsignal2 (Lisp_Object error_symbol, Lisp_Object arg1, Lisp_Object arg2)
{
  xsignal (error_symbol, list2 (arg1, arg2));
}

void
xsignal3 (Lisp_Object error_symbol, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  xsignal (error_symbol, list3 (arg1, arg2, arg3));
}

/* Signal `error' with message S, and additional arg ARG.
   If ARG is not a proper list, make it a one-element list.  */

void
signal_error (const char *s, Lisp_Object arg)
{
  if (NILP (Fproper_list_p (arg)))
    arg = list1 (arg);

  xsignal (Qerror, Fcons (build_string (s), arg));
}

/* Simplified version of 'define-error'.  */

void
define_error (Lisp_Object name, const char *message, Lisp_Object parent)
{
  eassert (SYMBOLP (name));
  eassert (SYMBOLP (parent));
  Lisp_Object parent_conditions = Fget (parent, Qerror_conditions);
  eassert (CONSP (parent_conditions));
  eassert (!NILP (Fmemq (parent, parent_conditions)));
  eassert (NILP (Fmemq (name, parent_conditions)));
  Fput (name, Qerror_conditions, Fcons (name, parent_conditions));
  Fput (name, Qerror_message, build_string (message));
}

/* Use this for arithmetic overflow, e.g., when an integer result is
   too large even for a bignum.  */
void
overflow_error (void)
{
  xsignal0 (Qoverflow_error);
}


/* Return true if LIST is a non-nil atom or
   a list containing one of CONDITIONS.  */

static bool
wants_debugger (Lisp_Object list, Lisp_Object conditions)
{
  if (NILP (list))
    return 0;
  if (! CONSP (list))
    return 1;

  while (CONSP (conditions))
    {
      Lisp_Object this, tail;
      this = XCAR (conditions);
      for (tail = list; CONSP (tail); tail = XCDR (tail))
	if (EQ (XCAR (tail), this))
	  return 1;
      conditions = XCDR (conditions);
    }
  return 0;
}

/* Return true if an error with condition-symbols CONDITIONS,
   and described by SIGNAL-DATA, should skip the debugger
   according to debugger-ignored-errors.  */

static bool
skip_debugger (Lisp_Object conditions, Lisp_Object data)
{
  Lisp_Object tail;
  bool first_string = 1;
  Lisp_Object error_message;

  error_message = Qnil;
  for (tail = Vdebug_ignored_errors; CONSP (tail); tail = XCDR (tail))
    {
      if (STRINGP (XCAR (tail)))
	{
	  if (first_string)
	    {
	      error_message = Ferror_message_string (data);
	      first_string = 0;
	    }

	  if (fast_string_match (XCAR (tail), error_message) >= 0)
	    return 1;
	}
      else
	{
	  Lisp_Object contail;

	  for (contail = conditions; CONSP (contail); contail = XCDR (contail))
	    if (EQ (XCAR (tail), XCAR (contail)))
	      return 1;
	}
    }

  return 0;
}

/* Say whether SIGNAL is a `quit' error (or inherits from it).  */
bool
signal_quit_p (Lisp_Object error)
{
  Lisp_Object signal = CONSP (error) ? XCAR (error) : Qnil;
  Lisp_Object list;

  return EQ (signal, Qquit)
    || (SYMBOLP (signal)
	&& CONSP (list = Fget (signal, Qerror_conditions))
	&& !NILP (Fmemq (Qquit, list)));
}

/* Call the debugger if calling it is currently enabled for CONDITIONS.
   SIG and DATA describe the signal.  There are two ways to pass them:
    = SIG is the error symbol, and DATA is the rest of the data.
    = SIG is nil, and DATA is (SYMBOL . REST-OF-DATA).
      This is for memory-full errors only.  */
static bool
maybe_call_debugger (Lisp_Object conditions, Lisp_Object error)
{
  if (
      /* Don't try to run the debugger with interrupts blocked.
	 The editing loop would return anyway.  */
      ! input_blocked_p ()
      && NILP (Vinhibit_debugger)
      /* Does user want to enter debugger for this kind of error?  */
      && (signal_quit_p (error)
	  ? debug_on_quit
	  : wants_debugger (Vdebug_on_error, conditions))
      && ! skip_debugger (conditions, error)
      /* See commentary on definition of
         `internal-when-entered-debugger'.  */
      && when_entered_debugger < num_nonmacro_input_events)
    {
      call_debugger (list2 (Qerror, error));
      return 1;
    }

  return 0;
}

static Lisp_Object
find_handler_clause (Lisp_Object handlers, Lisp_Object conditions)
{
  register Lisp_Object h;

  /* t is used by handlers for all conditions, set up by C code.  */
  /* error is used similarly, but means print an error message
     and run the debugger if that is enabled.  */
  if (!CONSP (handlers))
    return handlers;

  for (h = handlers; CONSP (h); h = XCDR (h))
    {
      Lisp_Object handler = XCAR (h);
      if (!NILP (Fmemq (handler, conditions))
          /* t is also used as a catch-all by Lisp code.  */
          || EQ (handler, Qt))
	return handlers;
    }

  return Qnil;
}


/* Format and return a string; called like vprintf.  */
Lisp_Object
vformat_string (const char *m, va_list ap)
{
  char buf[4000];
  ptrdiff_t size = sizeof buf;
  ptrdiff_t size_max = STRING_BYTES_BOUND + 1;
  char *buffer = buf;
  ptrdiff_t used;
  Lisp_Object string;

  used = evxprintf (&buffer, &size, buf, size_max, m, ap);
  string = make_string (buffer, used);
  if (buffer != buf)
    xfree (buffer);

  return string;
}

/* Dump an error message; called like vprintf.  */
void
verror (const char *m, va_list ap)
{
  xsignal1 (Qerror, vformat_string (m, ap));
}


/* Dump an error message; called like printf.  */

void
error (const char *m, ...)
{
  va_list ap;
  va_start (ap, m);
  verror (m, ap);
}

DEFUN ("commandp", Fcommandp, Scommandp, 1, 2, 0,
       doc: /* Non-nil if FUNCTION makes provisions for interactive calling.
This means it contains a description for how to read arguments to give it.
The value is nil for an invalid function or a symbol with no function
definition.

Interactively callable functions include strings and vectors (treated
as keyboard macros), lambda-expressions that contain a top-level call
to `interactive', autoload definitions made by `autoload' with non-nil
fourth argument, and some of the built-in functions of Lisp.

Also, a symbol satisfies `commandp' if its function definition does so.

If the optional argument FOR-CALL-INTERACTIVELY is non-nil,
then strings and vectors are not accepted.  */)
  (Lisp_Object function, Lisp_Object for_call_interactively)
{
  register Lisp_Object fun;
  bool genfun = false; /* If true, we should consult `interactive-form'.  */

  fun = function;

  fun = indirect_function (fun);
  if (NILP (fun))
    return Qnil;

  /* Emacs primitives are interactive if their DEFUN specifies an
     interactive spec.  */
  if (SUBRP (fun))
    {
      if (XSUBR (fun)->intspec.string)
        return Qt;
    }
  /* Bytecode objects are interactive if they are long enough to
     have an element whose index is CLOSURE_INTERACTIVE, which is
     where the interactive spec is stored.  */
  else if (CLOSUREP (fun))
    {
      if (PVSIZE (fun) > CLOSURE_INTERACTIVE)
        return Qt;
      else if (PVSIZE (fun) > CLOSURE_DOC_STRING)
        {
          Lisp_Object doc = AREF (fun, CLOSURE_DOC_STRING);
          /* An invalid "docstring" is a sign that we have an OClosure.  */
          genfun = !(NILP (doc) || VALID_DOCSTRING_P (doc));
        }
    }

#ifdef HAVE_MODULES
  /* Module functions are interactive if their `interactive_form'
     field is non-nil. */
  else if (MODULE_FUNCTIONP (fun))
    {
      if (!NILP (module_function_interactive_form (XMODULE_FUNCTION (fun))))
        return Qt;
    }
#endif

  /* Strings and vectors are keyboard macros.  */
  else if (STRINGP (fun) || VECTORP (fun))
    return (NILP (for_call_interactively) ? Qt : Qnil);

  /* Lists may represent commands.  */
  else if (!CONSP (fun))
    return Qnil;
  else
    {
      Lisp_Object funcar = XCAR (fun);
      if (EQ (funcar, Qautoload))
        {
          if (!NILP (Fcar (Fcdr (Fcdr (XCDR (fun))))))
            return Qt;
        }
      else
        {
          Lisp_Object body = CDR_SAFE (XCDR (fun));
          if (!EQ (funcar, Qlambda))
	    return Qnil;
	  if (!NILP (Fassq (Qinteractive, body)))
	    return Qt;
	  else
	    return Qnil;
	}
    }

  /* By now, if it's not a function we already returned nil.  */

  /* Check an `interactive-form' property if present, analogous to the
     function-documentation property.  */
  fun = function;
  while (SYMBOLP (fun))
    {
      Lisp_Object tmp = Fget (fun, Qinteractive_form);
      if (!NILP (tmp))
	error ("Found an 'interactive-form' property!");
      fun = Fsymbol_function (fun);
    }

  /* If there's no immediate interactive form but it's an OClosure,
     then delegate to the generic-function in case it has
     a type-specific interactive-form.  */
  if (genfun)
    {
      Lisp_Object iform = calln (Qinteractive_form, fun);
      return NILP (iform) ? Qnil : Qt;
    }
  else
    return Qnil;
}

DEFUN ("autoload", Fautoload, Sautoload, 2, 5, 0,
       doc: /* Define FUNCTION to autoload from FILE.
FUNCTION is a symbol; FILE is a file name string to pass to `load'.

Third arg DOCSTRING is documentation for the function.

Fourth arg INTERACTIVE if non-nil says function can be called
interactively.  If INTERACTIVE is a list, it is interpreted as a list
of modes the function is applicable for.

Fifth arg TYPE indicates the type of the object:
   nil or omitted says FUNCTION is a function,
   `keymap' says FUNCTION is really a keymap, and
   `macro' or t says FUNCTION is really a macro.

Third through fifth args give info about the real definition.
They default to nil.

If FUNCTION is already defined other than as an autoload,
this does nothing and returns nil.  */)
  (Lisp_Object function, Lisp_Object file, Lisp_Object docstring, Lisp_Object interactive, Lisp_Object type)
{
  CHECK_SYMBOL (function);
  CHECK_STRING (file);

  /* If function is defined and not as an autoload, don't override.  */
  if (!NILP (XSYMBOL (function)->u.s.function)
      && !AUTOLOADP (XSYMBOL (function)->u.s.function))
    return Qnil;

  return Fdefalias (function,
		    list5 (Qautoload, file, docstring, interactive, type),
		    Qnil);
}

static void
un_autoload (Lisp_Object oldqueue)
{
  /* Queue to unwind is current value of Vautoload_queue.
     oldqueue is the shadowed value to leave in Vautoload_queue.  */
  Lisp_Object queue = Vautoload_queue;
  Vautoload_queue = oldqueue;
  while (CONSP (queue))
    {
      Lisp_Object first = XCAR (queue);
      if (CONSP (first) && BASE_EQ (XCAR (first), make_fixnum (0)))
	Vfeatures = XCDR (first);
      else
	Ffset (first, Fcar (Fcdr (Fget (first, Qfunction_history))));
      queue = XCDR (queue);
    }
}

Lisp_Object
load_with_autoload_queue
  (Lisp_Object file, Lisp_Object noerror, Lisp_Object nomessage,
   Lisp_Object nosuffix, Lisp_Object must_suffix)
{
  specpdl_ref count = SPECPDL_INDEX ();

  /* If autoloading gets an error (which includes the error of failing
     to define the function being called), we use Vautoload_queue
     to undo function definitions and `provide' calls made by
     the function.  We do this in the specific case of autoloading
     because autoloading is not an explicit request "load this file",
     but rather a request to "call this function".

     The value saved here is to be restored into Vautoload_queue.  */
  record_unwind_protect (un_autoload, Vautoload_queue);
  Vautoload_queue = Qt;
  Lisp_Object tem
    = save_match_data_load (file, noerror, nomessage, nosuffix, must_suffix);

  /* Once loading finishes, don't undo it.  */
  Vautoload_queue = Qt;
  unbind_to (count, Qnil);
  return tem;
}

/* Load an autoloaded function.
   FUNNAME is the symbol which is the function's name.
   FUNDEF is the autoload definition (a list).  */

DEFUN ("autoload-do-load", Fautoload_do_load, Sautoload_do_load, 1, 3, 0,
       doc: /* Load FUNDEF which should be an autoload.
If non-nil, FUNNAME should be the symbol whose function value is FUNDEF,
in which case the function returns the new autoloaded function value.
If equal to `macro', MACRO-ONLY specifies that FUNDEF should only be loaded if
it defines a macro.  */)
  (Lisp_Object fundef, Lisp_Object funname, Lisp_Object macro_only)
{
  if (!CONSP (fundef) || !EQ (Qautoload, XCAR (fundef)))
    return fundef;

  Lisp_Object kind = Fnth (make_fixnum (4), fundef);
  if (EQ (macro_only, Qmacro)
      && !(EQ (kind, Qt) || EQ (kind, Qmacro)))
    return fundef;

  /* This is to make sure that loadup.el gives a clear picture
     of what files are preloaded and when.  */
  if (will_dump_p () && !will_bootstrap_p ())
    {
      /* Avoid landing here recursively while outputting the
	 backtrace from the error.  */
      gflags.will_dump = false;
      error ("Attempt to autoload %s while preparing to dump",
	     SDATA (SYMBOL_NAME (funname)));
    }

  CHECK_SYMBOL (funname);

  /* If `macro_only' is set and fundef isn't a macro, assume this autoload to
     be a "best-effort" (e.g. to try and find a compiler macro),
     so don't signal an error if autoloading fails.  */
  Lisp_Object ignore_errors
    = (EQ (kind, Qt) || EQ (kind, Qmacro)) ? Qnil : macro_only;
  load_with_autoload_queue (Fcar (Fcdr (fundef)), ignore_errors, Qt, Qnil, Qt);

  if (NILP (funname) || !NILP (ignore_errors))
    return Qnil;
  else
    {
      Lisp_Object fun = Findirect_function (funname, Qnil);

      if (!NILP (Fequal (fun, fundef)))
	error ("Autoloading file %s failed to define function %s",
	       SDATA (Fcar (Fcar (Vload_history))),
	       SDATA (SYMBOL_NAME (funname)));
      else
	return fun;
    }
}


static Lisp_Object list_of_t;  /* Never-modified constant containing (t).  */

DEFUN ("eval", Feval, Seval, 1, 2, 0,
       doc: /* Evaluate FORM and return its value.
If LEXICAL is `t', evaluate using lexical binding by default.
This is the recommended value.

If absent or `nil', use dynamic scoping only.

LEXICAL can also represent an actual lexical environment; see the Info
node `(elisp)Eval' for details.  */)
  (Lisp_Object form, Lisp_Object lexical)
{
  specpdl_ref count = SPECPDL_INDEX ();
  specbind (Qinternal_interpreter_environment,
	    CONSP (lexical) || NILP (lexical) ? lexical : list_of_t);
  return unbind_to (count, eval_sub (form));
}

void
grow_specpdl_allocation (void)
{
  eassert (specpdl_ptr == specpdl_end);

  specpdl_ref count = SPECPDL_INDEX ();
  ptrdiff_t max_size = PTRDIFF_MAX - 1000;
  union specbinding *pdlvec = specpdl - 1;
  ptrdiff_t size = specpdl_end - specpdl;
  ptrdiff_t pdlvecsize = size + 1;
  eassert (max_size > size);
  pdlvec = xpalloc (pdlvec, &pdlvecsize, 1, max_size + 1, sizeof *specpdl);
  specpdl = pdlvec + 1;
  specpdl_end = specpdl + pdlvecsize - 1;
  specpdl_ptr = specpdl_ref_to_ptr (count);
}

/* Eval a sub-expression of the current expression (i.e. in the same
   lexical scope).  */
Lisp_Object
eval_sub (Lisp_Object form)
{
  if (SYMBOLP (form))
    {
      /* Look up its binding in the lexical environment.
	 We do not pay attention to the declared_special flag here, since we
	 already did that when let-binding the variable.  */
      Lisp_Object lex_binding
	= Fassq (form, Vinternal_interpreter_environment);
      return !NILP (lex_binding) ? XCDR (lex_binding) : Fsymbol_value (form);
    }

  if (!CONSP (form))
    return form;

  maybe_quit ();

  maybe_gc ();

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	xsignal1 (Qexcessive_lisp_nesting, make_fixnum (lisp_eval_depth));
    }

  Lisp_Object original_fun = XCAR (form);
  Lisp_Object original_args = XCDR (form);
  CHECK_LIST (original_args);

  /* This also protects them from gc.  */
  specpdl_ref count
    = record_in_backtrace (original_fun, &original_args, UNEVALLED);

  if (debug_on_next_call)
    do_debug_on_call (Qt, count);

  Lisp_Object fun, val, funcar;
  /* Declare here, as this array may be accessed by call_debugger near
     the end of this function.  See Bug#21245.  */
  Lisp_Object argvals[8];
  /* The stack overflow detection probably isn't worth the effort any more
     but this may be the least bad spot to feed it.  */
  current_thread->stack_top = argvals;

 retry:

  /* Optimize for no indirection.  */
  fun = original_fun;
  if (!SYMBOLP (fun))
    fun = Ffunction (list1 (fun));
  else if (!NILP (fun) && (fun = XSYMBOL (fun)->u.s.function, SYMBOLP (fun)))
    fun = indirect_function (fun);

  if (SUBRP (fun) && !NATIVE_COMP_FUNCTION_DYNP (fun))
    {
      Lisp_Object args_left = original_args;
      ptrdiff_t numargs = list_length (args_left);

      if (numargs < XSUBR (fun)->min_args
	  || (XSUBR (fun)->max_args >= 0
	      && XSUBR (fun)->max_args < numargs))
	xsignal2 (Qwrong_number_of_arguments, original_fun,
		  make_fixnum (numargs));

      else if (XSUBR (fun)->max_args == UNEVALLED)
	val = (XSUBR (fun)->function.aUNEVALLED) (args_left);
      else if (XSUBR (fun)->max_args == MANY
	       || XSUBR (fun)->max_args > 8)

	{
	  /* Pass a vector of evaluated arguments.  */
	  Lisp_Object *vals;
	  ptrdiff_t argnum = 0;
	  USE_SAFE_ALLOCA;

	  SAFE_ALLOCA_LISP (vals, numargs);

	  while (CONSP (args_left) && argnum < numargs)
	    {
	      Lisp_Object arg = XCAR (args_left);
	      args_left = XCDR (args_left);
	      vals[argnum++] = eval_sub (arg);
	    }

	  set_backtrace_args (specpdl_ref_to_ptr (count), vals, argnum);

	  val = XSUBR (fun)->function.aMANY (argnum, vals);

	  lisp_eval_depth--;
	  /* Do the debug-on-exit now, while VALS still exists.  */
	  if (backtrace_debug_on_exit (specpdl_ref_to_ptr (count)))
	    val = call_debugger (list2 (Qexit, val));
	  SAFE_FREE ();
	  specpdl_ptr--;
	  return val;
	}
      else
	{
	  int i, maxargs = XSUBR (fun)->max_args;

	  for (i = 0; i < maxargs; i++)
	    {
	      argvals[i] = eval_sub (Fcar (args_left));
	      args_left = Fcdr (args_left);
	    }

	  set_backtrace_args (specpdl_ref_to_ptr (count), argvals, numargs);

	  switch (i)
	    {
	    case 0:
	      val = (XSUBR (fun)->function.a0 ());
	      break;
	    case 1:
	      val = (XSUBR (fun)->function.a1 (argvals[0]));
	      break;
	    case 2:
	      val = (XSUBR (fun)->function.a2 (argvals[0], argvals[1]));
	      break;
	    case 3:
	      val = (XSUBR (fun)->function.a3
		     (argvals[0], argvals[1], argvals[2]));
	      break;
	    case 4:
	      val = (XSUBR (fun)->function.a4
		     (argvals[0], argvals[1], argvals[2], argvals[3]));
	      break;
	    case 5:
	      val = (XSUBR (fun)->function.a5
		     (argvals[0], argvals[1], argvals[2], argvals[3],
		      argvals[4]));
	      break;
	    case 6:
	      val = (XSUBR (fun)->function.a6
		     (argvals[0], argvals[1], argvals[2], argvals[3],
		      argvals[4], argvals[5]));
	      break;
	    case 7:
	      val = (XSUBR (fun)->function.a7
		     (argvals[0], argvals[1], argvals[2], argvals[3],
		      argvals[4], argvals[5], argvals[6]));
	      break;

	    case 8:
	      val = (XSUBR (fun)->function.a8
		     (argvals[0], argvals[1], argvals[2], argvals[3],
		      argvals[4], argvals[5], argvals[6], argvals[7]));
	      break;

	    default:
	      /* Someone has created a subr that takes more arguments than
		 is supported by this code.  We need to either rewrite the
		 subr to use a different argument protocol, or add more
		 cases to this switch.  */
	      emacs_abort ();
	    }
	}
    }
  else if (CLOSUREP (fun)
	   || NATIVE_COMP_FUNCTION_DYNP (fun)
	   || MODULE_FUNCTIONP (fun))
    return apply_lambda (fun, original_args, count);
  else
    {
      if (NILP (fun))
	xsignal1 (Qvoid_function, original_fun);
      if (!CONSP (fun))
	xsignal1 (Qinvalid_function, original_fun);
      funcar = XCAR (fun);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, original_fun);
      if (EQ (funcar, Qautoload))
	{
	  Fautoload_do_load (fun, original_fun, Qnil);
	  goto retry;
	}
      if (EQ (funcar, Qmacro))
	{
	  specpdl_ref count1 = SPECPDL_INDEX ();
	  Lisp_Object exp;
	  /* Bind lexical-binding during expansion of the macro, so the
	     macro can know reliably if the code it outputs will be
	     interpreted using lexical-binding or not.  */
	  specbind (Qlexical_binding,
		    NILP (Vinternal_interpreter_environment) ? Qnil : Qt);

	  /* Make the macro aware of any defvar declarations in scope. */
	  Lisp_Object dynvars = Vmacroexp__dynvars;
	  for (Lisp_Object p = Vinternal_interpreter_environment;
	       !NILP (p); p = XCDR(p))
	    {
	      Lisp_Object e = XCAR (p);
	      if (SYMBOLP (e))
		dynvars = Fcons(e, dynvars);
	    }
	  if (!EQ (dynvars, Vmacroexp__dynvars))
	    specbind (Qmacroexp__dynvars, dynvars);

	  exp = apply1 (Fcdr (fun), original_args);
	  exp = unbind_to (count1, exp);
	  val = eval_sub (exp);
	}
      else if (EQ (funcar, Qlambda))
	return apply_lambda (fun, original_args, count);
      else
	xsignal1 (Qinvalid_function, original_fun);
    }

  lisp_eval_depth--;
  if (backtrace_debug_on_exit (specpdl_ref_to_ptr (count)))
    val = call_debugger (list2 (Qexit, val));
  specpdl_ptr--;

  return val;
}

DEFUN ("apply", Fapply, Sapply, 1, MANY, 0,
       doc: /* Call FUNCTION with our remaining args, using our last arg as list of args.
Then return the value FUNCTION returns.
With a single argument, call the argument's first element using the
other elements as args.
Thus, (apply \\='+ 1 2 \\='(3 4)) returns 10.
usage: (apply FUNCTION &rest ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i, funcall_nargs;
  Lisp_Object *funcall_args = NULL;
  Lisp_Object spread_arg = args[nargs - 1];
  Lisp_Object fun = args[0];
  USE_SAFE_ALLOCA;

  ptrdiff_t numargs = list_length (spread_arg);

  if (numargs == 0)
    return Ffuncall (max (1, nargs - 1), args);
  else if (numargs == 1)
    {
      args [nargs - 1] = XCAR (spread_arg);
      return Ffuncall (nargs, args);
    }

  numargs += nargs - 2;

  /* Optimize for no indirection.  */
  if (SYMBOLP (fun) && !NILP (fun)
      && (fun = XSYMBOL (fun)->u.s.function, SYMBOLP (fun)))
    {
      fun = indirect_function (fun);
      if (NILP (fun))
	/* Let funcall get the error.  */
	fun = args[0];
    }

  if (SUBRP (fun) && XSUBR (fun)->max_args > numargs
      /* Don't hide an error by adding missing arguments.  */
      && numargs >= XSUBR (fun)->min_args)
    {
      /* Avoid making funcall cons up a yet another new vector of arguments
	 by explicitly supplying nil's for optional values.  */
      SAFE_ALLOCA_LISP (funcall_args, 1 + XSUBR (fun)->max_args);
      memclear (funcall_args + numargs + 1,
		(XSUBR (fun)->max_args - numargs) * word_size);
      funcall_nargs = 1 + XSUBR (fun)->max_args;
    }
  else
    { /* We add 1 to numargs because funcall_args includes the
	 function itself as well as its arguments.  */
      SAFE_ALLOCA_LISP (funcall_args, 1 + numargs);
      funcall_nargs = 1 + numargs;
    }

  memcpy (funcall_args, args, nargs * word_size);
  /* Spread the last arg we got.  Its first element goes in
     the slot that it used to occupy, hence this value of I.  */
  i = nargs - 1;
  while (!NILP (spread_arg))
    {
      funcall_args [i++] = XCAR (spread_arg);
      spread_arg = XCDR (spread_arg);
    }

  Lisp_Object retval = Ffuncall (funcall_nargs, funcall_args);

  SAFE_FREE ();
  return retval;
}

/* Run hook variables in various ways.  */

static Lisp_Object
funcall_nil (ptrdiff_t nargs, Lisp_Object *args)
{
  Ffuncall (nargs, args);
  return Qnil;
}

DEFUN ("run-hooks", Frun_hooks, Srun_hooks, 0, MANY, 0,
       doc: /* Run each hook in HOOKS.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

Major modes should not use this function directly to run their mode
hook; they should use `run-mode-hooks' instead.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hooks &rest HOOKS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;

  for (i = 0; i < nargs; i++)
    run_hook (args[i]);

  return Qnil;
}

DEFUN ("run-hook-with-args", Frun_hook_with_args,
       Srun_hook_with_args, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS.  The final return value
is unspecified.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, funcall_nil);
}

/* NB this one still documents a specific non-nil return value.
   (As did run-hook-with-args and run-hook-with-args-until-failure
   until they were changed in 24.1.)  */
DEFUN ("run-hook-with-args-until-success", Frun_hook_with_args_until_success,
       Srun_hook_with_args_until_success, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS, stopping at the first
one that returns non-nil, and return that value.  Otherwise (if
all functions return nil, or if there are no functions to call),
return nil.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args-until-success HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, Ffuncall);
}

static Lisp_Object
funcall_not (ptrdiff_t nargs, Lisp_Object *args)
{
  return NILP (Ffuncall (nargs, args)) ? Qt : Qnil;
}

DEFUN ("run-hook-with-args-until-failure", Frun_hook_with_args_until_failure,
       Srun_hook_with_args_until_failure, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  The value of HOOK
may be nil, a function, or a list of functions.  Call each
function in order with arguments ARGS, stopping at the first
one that returns nil, and return nil.  Otherwise (if all functions
return non-nil, or if there are no functions to call), return non-nil
\(do not rely on the precise return value in this case).

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args-until-failure HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return NILP (run_hook_with_args (nargs, args, funcall_not)) ? Qt : Qnil;
}

static Lisp_Object
run_hook_wrapped_funcall (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object tmp = args[0], ret;
  args[0] = args[1];
  args[1] = tmp;
  ret = Ffuncall (nargs, args);
  args[1] = args[0];
  args[0] = tmp;
  return ret;
}

DEFUN ("run-hook-wrapped", Frun_hook_wrapped, Srun_hook_wrapped, 2, MANY, 0,
       doc: /* Run HOOK, passing each function through WRAP-FUNCTION.
I.e. instead of calling each function FUN directly with arguments ARGS,
it calls WRAP-FUNCTION with arguments FUN and ARGS.
As soon as a call to WRAP-FUNCTION returns non-nil, `run-hook-wrapped'
aborts and returns that value.
usage: (run-hook-wrapped HOOK WRAP-FUNCTION &rest ARGS)  */)
     (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, run_hook_wrapped_funcall);
}

/* ARGS[0] should be a hook symbol.
   Call each of the functions in the hook value, passing each of them
   as arguments all the rest of ARGS (all NARGS - 1 elements).
   FUNCALL specifies how to call each function on the hook.  */

Lisp_Object
run_hook_with_args (ptrdiff_t nargs, Lisp_Object *args,
		    Lisp_Object (*funcall) (ptrdiff_t nargs, Lisp_Object *args))
{
  Lisp_Object sym, val, ret = Qnil;

  /* If we are dying or still initializing,
     don't do anything--it would probably crash if we tried.  */
  if (NILP (Vrun_hooks))
    return Qnil;

  sym = args[0];
  val = find_symbol_value (sym);

  if (BASE_EQ (val, Qunbound) || NILP (val))
    return ret;
  else if (!CONSP (val) || FUNCTIONP (val))
    {
      args[0] = val;
      return funcall (nargs, args);
    }
  else
    {
      Lisp_Object global_vals = Qnil;

      for (;
	   CONSP (val) && NILP (ret);
	   val = XCDR (val))
	{
	  if (EQ (XCAR (val), Qt))
	    {
	      /* t indicates this hook has a local binding;
		 it means to run the global binding too.  */
	      global_vals = Fdefault_value (sym);
	      if (NILP (global_vals)) continue;

	      if (!CONSP (global_vals) || EQ (XCAR (global_vals), Qlambda))
		{
		  args[0] = global_vals;
		  ret = funcall (nargs, args);
		}
	      else
		{
		  for (;
		       CONSP (global_vals) && NILP (ret);
		       global_vals = XCDR (global_vals))
		    {
		      args[0] = XCAR (global_vals);
		      /* In a global value, t should not occur.  If it does, we
			 must ignore it to avoid an endless loop.  */
		      if (!EQ (args[0], Qt))
			ret = funcall (nargs, args);
		    }
		}
	    }
	  else
	    {
	      args[0] = XCAR (val);
	      ret = funcall (nargs, args);
	    }
	}

      return ret;
    }
}

/* Run the hook HOOK, giving each function no args.  */

void
run_hook (Lisp_Object hook)
{
  Frun_hook_with_args (1, &hook);
}

/* Run the hook HOOK, giving each function the two args ARG1 and ARG2.  */

void
run_hook_with_args_2 (Lisp_Object hook, Lisp_Object arg1, Lisp_Object arg2)
{
  CALLN (Frun_hook_with_args, hook, arg1, arg2);
}

/* Apply fn to arg.  */
Lisp_Object
apply1 (Lisp_Object fn, Lisp_Object arg)
{
  return NILP (arg) ? calln (fn) : CALLN (Fapply, fn, arg);
}

DEFUN ("functionp", Ffunctionp, Sfunctionp, 1, 1, 0,
       doc: /* Return t if OBJECT is a function.

An object is a function if it is callable via `funcall'; this includes
symbols with function bindings, but excludes macros and special forms.

Ordinarily return nil if OBJECT is not a function, although t might be
returned in rare cases.  */)
     (Lisp_Object object)
{
  if (FUNCTIONP (object))
    return Qt;
  return Qnil;
}

bool
FUNCTIONP (Lisp_Object object)
{
  if (SYMBOLP (object) && !NILP (Ffboundp (object)))
    {
      object = Findirect_function (object, Qt);

      if (CONSP (object) && EQ (XCAR (object), Qautoload))
	{
	  /* Autoloaded symbols are functions, except if they load
	     macros or keymaps.  */
	  for (int i = 0; i < 4 && CONSP (object); i++)
	    object = XCDR (object);

	  return ! (CONSP (object) && !NILP (XCAR (object)));
	}
    }

  if (SUBRP (object))
    return XSUBR (object)->max_args != UNEVALLED;
  else if (CLOSUREP (object) || MODULE_FUNCTIONP (object))
    return true;
  else if (CONSP (object))
    {
      Lisp_Object car = XCAR (object);
      return EQ (car, Qlambda);
    }
  else
    return false;
}

DEFUN ("debugger-trap", Fdebugger_trap, Sdebugger_trap, 0, 0, "",
       doc: /* Stop Emacs and hand over control to GDB.
The Emacs source file src/.gdbinit sets a breakpoint in this function.

This function does nothing.  It is not called by Emacs otherwise, and
exists so that calling it or invoking it interactively will cause
GDB to kick in.

For Lisp debugging see `debug', as well as `edebug', in the manual:
"(elisp) Debugging".  */)
  (void)
{
  return Qnil;
}

Lisp_Object
funcall_general (Lisp_Object fun, ptrdiff_t numargs, Lisp_Object *args)
{
  Lisp_Object original_fun = fun;
 retry:
  if (SYMBOLP (fun) && !NILP (fun)
      && (fun = XSYMBOL (fun)->u.s.function, SYMBOLP (fun)))
    fun = indirect_function (fun);

  if (SUBRP (fun) && !NATIVE_COMP_FUNCTION_DYNP (fun))
    return funcall_subr (XSUBR (fun), numargs, args);
  else if (CLOSUREP (fun)
	   || NATIVE_COMP_FUNCTION_DYNP (fun)
	   || MODULE_FUNCTIONP (fun))
    return funcall_lambda (fun, numargs, args);
  else
    {
      if (NILP (fun))
	xsignal1 (Qvoid_function, original_fun);
      if (!CONSP (fun))
	xsignal1 (Qinvalid_function, original_fun);
      Lisp_Object funcar = XCAR (fun);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, original_fun);
      if (EQ (funcar, Qlambda))
	return funcall_lambda (fun, numargs, args);
      else if (EQ (funcar, Qautoload))
	{
	  Fautoload_do_load (fun, original_fun, Qnil);
	  fun = original_fun;
	  goto retry;
	}
      else
	xsignal1 (Qinvalid_function, original_fun);
    }
}

DEFUN ("funcall", Ffuncall, Sfuncall, 1, MANY, 0,
       doc: /* Call first argument as a function, passing remaining arguments to it.
Return the value that function returns.
Thus, (funcall \\='cons \\='x \\='y) returns (x . y).
usage: (funcall FUNCTION &rest ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count;

  maybe_quit ();

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	xsignal1 (Qexcessive_lisp_nesting, make_fixnum (lisp_eval_depth));
    }

  count = record_in_backtrace (args[0], &args[1], nargs - 1);

  maybe_gc ();

  if (debug_on_next_call)
    do_debug_on_call (Qlambda, count);

  Lisp_Object val = funcall_general (args[0], nargs - 1, args + 1);

  lisp_eval_depth--;
  if (backtrace_debug_on_exit (specpdl_ref_to_ptr (count)))
    val = call_debugger (list2 (Qexit, val));
  specpdl_ptr--;
  return val;
}


static Lisp_Object
safe_eval_handler (Lisp_Object arg, ptrdiff_t nargs, Lisp_Object *args)
{
  add_to_log ("Error muted by safe_call: %S signaled %S",
	      Flist (nargs, args), arg);
  return Qnil;
}

Lisp_Object
safe_funcall (ptrdiff_t nargs, Lisp_Object *args)
{
  specpdl_ref count = SPECPDL_INDEX ();
  /* FIXME: This function started its life in 'xdisp.c' for use internally
     by the redisplay.  So it was important to inhibit redisplay.
     Not clear if we still need this 'specbind' now that 'xdisp.c' has its
     own version of this code.  */
  specbind (Qinhibit_redisplay, Qt);
  /* Use Qt to ensure debugger does not run.  */
  Lisp_Object val = internal_condition_case_n (Ffuncall, nargs, args, Qt,
				               safe_eval_handler);
  return unbind_to (count, val);
}

Lisp_Object
safe_eval (Lisp_Object sexp)
{
  return safe_calln (Qeval, sexp, Qt);
}

/* Apply a C subroutine SUBR to the NUMARGS evaluated arguments in ARG_VECTOR
   and return the result of evaluation.  */

Lisp_Object
funcall_subr (struct Lisp_Subr *subr, ptrdiff_t numargs, Lisp_Object *args)
{
  eassume (numargs >= 0);
  if (numargs >= subr->min_args)
    {
      /* Conforming call to finite-arity subr.  */
      ptrdiff_t maxargs = subr->max_args;
      if (numargs <= maxargs && maxargs <= 8)
	{
	  Lisp_Object argbuf[8];
	  Lisp_Object *a;
	  if (numargs < maxargs)
	    {
	      eassume (maxargs <= ARRAYELTS (argbuf));
	      a = argbuf;
	      memcpy (a, args, numargs * word_size);
	      memclear (a + numargs, (maxargs - numargs) * word_size);
	    }
	  else
	    a = args;
	  switch (maxargs)
	    {
	    case 0:
	      return subr->function.a0 ();
	    case 1:
	      return subr->function.a1 (a[0]);
	    case 2:
	      return subr->function.a2 (a[0], a[1]);
	    case 3:
	      return subr->function.a3 (a[0], a[1], a[2]);
	    case 4:
	      return subr->function.a4 (a[0], a[1], a[2], a[3]);
	    case 5:
	      return subr->function.a5 (a[0], a[1], a[2], a[3], a[4]);
	    case 6:
	      return subr->function.a6 (a[0], a[1], a[2], a[3], a[4], a[5]);
	    case 7:
	      return subr->function.a7 (a[0], a[1], a[2], a[3], a[4], a[5],
					a[6]);
	    case 8:
	      return subr->function.a8 (a[0], a[1], a[2], a[3], a[4], a[5],
					a[6], a[7]);
	    }
	  eassume (false);	/* In case the compiler is too stupid.  */
	}

      /* Call to n-adic subr.  */
      if (maxargs == MANY || maxargs > 8)
	return subr->function.aMANY (numargs, args);
    }

  /* Anything else is an error.  */
  Lisp_Object fun;
  XSETSUBR (fun, subr);
  if (subr->max_args == UNEVALLED)
    xsignal1 (Qinvalid_function, fun);
  else
    xsignal2 (Qwrong_number_of_arguments, fun, make_fixnum (numargs));
}

static Lisp_Object
apply_lambda (Lisp_Object fun, Lisp_Object args, specpdl_ref count)
{
  Lisp_Object *arg_vector;
  Lisp_Object tem;
  USE_SAFE_ALLOCA;

  ptrdiff_t numargs = list_length (args);
  SAFE_ALLOCA_LISP (arg_vector, numargs);
  Lisp_Object args_left = args;

  for (ptrdiff_t i = 0; i < numargs; i++)
    {
      tem = Fcar (args_left), args_left = Fcdr (args_left);
      tem = eval_sub (tem);
      arg_vector[i] = tem;
    }

  set_backtrace_args (specpdl_ref_to_ptr (count), arg_vector, numargs);
  tem = funcall_lambda (fun, numargs, arg_vector);

  lisp_eval_depth--;
  /* Do the debug-on-exit now, while arg_vector still exists.  */
  if (backtrace_debug_on_exit (specpdl_ref_to_ptr (count)))
    tem = call_debugger (list2 (Qexit, tem));
  SAFE_FREE ();
  specpdl_ptr--;
  return tem;
}

/* Apply a Lisp function FUN to the NARGS evaluated arguments in ARG_VECTOR
   and return the result of evaluation.
   FUN must be either a lambda-expression, a compiled-code object,
   or a module function.  */

static Lisp_Object
funcall_lambda (Lisp_Object fun, ptrdiff_t nargs, Lisp_Object *arg_vector)
{
  Lisp_Object syms_left, lexenv;

  if (CONSP (fun))
    {
      lexenv = Qnil;
      syms_left = XCDR (fun);
      if (CONSP (syms_left))
	syms_left = XCAR (syms_left);
      else
	xsignal1 (Qinvalid_function, fun);
    }
  else if (CLOSUREP (fun))
    {
      syms_left = AREF (fun, CLOSURE_ARGLIST);
      /* Bytecode objects using lexical binding have an integral
	 ARGLIST slot value: pass the arguments to the byte-code
	 engine directly.  */
      if (FIXNUMP (syms_left))
	return exec_byte_code (fun, XFIXNUM (syms_left), nargs, arg_vector);
      /* Otherwise the closure either is interpreted
	 or uses dynamic binding and the ARGLIST slot contains a standard
	 formal argument list whose variables are bound dynamically below.  */
      lexenv = CONSP (AREF (fun, CLOSURE_CODE))
               ? AREF (fun, CLOSURE_CONSTANTS)
               : Qnil;
    }
#ifdef HAVE_MODULES
  else if (MODULE_FUNCTIONP (fun))
    return funcall_module (fun, nargs, arg_vector);
#endif
#ifdef HAVE_NATIVE_COMP
  else if (NATIVE_COMP_FUNCTION_DYNP (fun))
    {
      syms_left = XSUBR (fun)->lambda_list;
      lexenv = Qnil;
    }
#endif
  else
    emacs_abort ();

  specpdl_ref count = SPECPDL_INDEX ();
  ptrdiff_t i = 0;
  bool optional = false;
  bool rest = false;
  bool previous_rest = false;
  for (; CONSP (syms_left); syms_left = XCDR (syms_left))
    {
      maybe_quit ();

      Lisp_Object next = maybe_remove_pos_from_symbol (XCAR (syms_left));
      if (!BARE_SYMBOL_P (next))
	xsignal1 (Qinvalid_function, fun);

      if (BASE_EQ (next, Qand_rest))
        {
          if (rest || previous_rest)
            xsignal1 (Qinvalid_function, fun);
          rest = 1;
	  previous_rest = true;
        }
      else if (BASE_EQ (next, Qand_optional))
        {
          if (optional || rest || previous_rest)
            xsignal1 (Qinvalid_function, fun);
          optional = 1;
        }
      else
	{
	  Lisp_Object arg;
	  if (rest)
	    {
	      arg = Flist (nargs - i, &arg_vector[i]);
	      i = nargs;
	    }
	  else if (i < nargs)
	    arg = arg_vector[i++];
	  else if (!optional)
	    xsignal2 (Qwrong_number_of_arguments, fun, make_fixnum (nargs));
	  else
	    arg = Qnil;

	  /* Bind the argument.  */
	  if (!NILP (lexenv))
	    /* Lexically bind NEXT by adding it to the lexenv alist.  */
	    lexenv = Fcons (Fcons (next, arg), lexenv);
	  else
	    /* Dynamically bind NEXT.  */
	    specbind (next, arg);
	  previous_rest = false;
	}
    }

  if (!NILP (syms_left) || previous_rest)
    xsignal1 (Qinvalid_function, fun);
  else if (i < nargs)
    xsignal2 (Qwrong_number_of_arguments, fun, make_fixnum (nargs));

  if (!BASE_EQ (lexenv, Vinternal_interpreter_environment))
    /* Instantiate a new lexical environment.  */
    specbind (Qinternal_interpreter_environment, lexenv);

  Lisp_Object val;
  if (CONSP (fun))
    val = Fprogn (XCDR (XCDR (fun)));
  else if (NATIVE_COMP_FUNCTIONP (fun))
    {
      eassert (NATIVE_COMP_FUNCTION_DYNP (fun));
      /* No need to use funcall_subr as we have zero arguments by
	 construction.  */
      val = XSUBR (fun)->function.a0 ();
    }
  else
    {
      eassert (CLOSUREP (fun));
      val = CONSP (AREF (fun, CLOSURE_CODE))
            /* Interpreted function.  */
            ? Fprogn (AREF (fun, CLOSURE_CODE))
            /* Dynbound bytecode.  */
            : exec_byte_code (fun, 0, 0, NULL);
    }

  return unbind_to (count, val);
}

DEFUN ("func-arity", Ffunc_arity, Sfunc_arity, 1, 1, 0,
       doc: /* Return minimum and maximum number of args allowed for FUNCTION.
FUNCTION must be a function of some kind.
The returned value is a cons cell (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number, or the symbol `many', for a
function with `&rest' args, or `unevalled' for a special form.

Note that this function might return inaccurate results in some cases,
such as with functions defined using `apply-partially', functions
advised using `advice-add', and functions that determine their arg
list dynamically.  */)
  (Lisp_Object function)
{
  Lisp_Object original;
  Lisp_Object funcar;
  Lisp_Object result;

  original = function;

 retry:

  /* Optimize for no indirection.  */
  function = original;
  if (SYMBOLP (function) && !NILP (function))
    {
      function = XSYMBOL (function)->u.s.function;
      if (SYMBOLP (function))
	function = indirect_function (function);
    }

  if (CONSP (function) && EQ (XCAR (function), Qmacro))
    function = XCDR (function);

  if (SUBRP (function))
    result = Fsubr_arity (function);
  else if (CLOSUREP (function))
    result = lambda_arity (function);
#ifdef HAVE_MODULES
  else if (MODULE_FUNCTIONP (function))
    result = module_function_arity (XMODULE_FUNCTION (function));
#endif
  else
    {
      if (NILP (function))
	xsignal1 (Qvoid_function, original);
      if (!CONSP (function))
	xsignal1 (Qinvalid_function, original);
      funcar = XCAR (function);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, original);
      if (EQ (funcar, Qlambda))
	result = lambda_arity (function);
      else if (EQ (funcar, Qautoload))
	{
	  Fautoload_do_load (function, original, Qnil);
	  goto retry;
	}
      else
	xsignal1 (Qinvalid_function, original);
    }
  return result;
}

/* FUN must be either a lambda-expression or a compiled-code object.  */
static Lisp_Object
lambda_arity (Lisp_Object fun)
{
  Lisp_Object syms_left;

  if (CONSP (fun))
    {
      syms_left = XCDR (fun);
      if (CONSP (syms_left))
	syms_left = XCAR (syms_left);
      else
	xsignal1 (Qinvalid_function, fun);
    }
  else if (CLOSUREP (fun))
    {
      syms_left = AREF (fun, CLOSURE_ARGLIST);
      if (FIXNUMP (syms_left))
        return get_byte_code_arity (syms_left);
    }
  else
    emacs_abort ();

  EMACS_INT minargs = 0, maxargs = 0;
  bool optional = false;
  for (; CONSP (syms_left); syms_left = XCDR (syms_left))
    {
      Lisp_Object next = XCAR (syms_left);
      if (!SYMBOLP (next))
	xsignal1 (Qinvalid_function, fun);

      if (EQ (next, Qand_rest))
	return Fcons (make_fixnum (minargs), Qmany);
      else if (EQ (next, Qand_optional))
	optional = true;
      else
	{
          if (!optional)
            minargs++;
          maxargs++;
        }
    }

  if (!NILP (syms_left))
    xsignal1 (Qinvalid_function, fun);

  return Fcons (make_fixnum (minargs), make_fixnum (maxargs));
}


/* Return true if SYMBOL's default currently has a let-binding
   which was made in the buffer that is now current.  */

bool
let_shadows_buffer_binding_p (struct Lisp_Symbol *symbol)
{
  union specbinding *p;
  Lisp_Object buf = Fcurrent_buffer ();

  for (p = specpdl_ptr; p > specpdl; )
    if ((--p)->kind > SPECPDL_LET)
      {
	struct Lisp_Symbol *let_bound_symbol = XSYMBOL (specpdl_symbol (p));
	eassert (let_bound_symbol->u.s.redirect != SYMBOL_VARALIAS);
	if (symbol == let_bound_symbol
	    && p->kind != SPECPDL_LET_LOCAL /* bug#62419 */
	    && BASE_EQ (specpdl_where (p), buf))
	  return 1;
      }

  return 0;
}

static void
do_specbind (struct Lisp_Symbol *sym, union specbinding *bind,
             Lisp_Object value, enum Set_Internal_Bind bindflag)
{
  switch (sym->u.s.redirect)
    {
    case SYMBOL_PLAINVAL:
      if (!sym->u.s.trapped_write)
	SET_SYMBOL_VAL (sym, value);
      else
        set_internal (specpdl_symbol (bind), value, Qnil, bindflag);
      break;

    case SYMBOL_FORWARDED:
      if (BUFFER_OBJFWDP (SYMBOL_FWD (sym))
	  && specpdl_kind (bind) == SPECPDL_LET_DEFAULT)
	{
          set_default_internal (specpdl_symbol (bind), value, bindflag,
				NULL);
	  return;
	}
      FALLTHROUGH;
    case SYMBOL_LOCALIZED:
      set_internal (specpdl_symbol (bind), value, Qnil, bindflag);
      break;

    default:
      emacs_abort ();
    }
}

/* `specpdl_ptr' describes which variable is
   let-bound, so it can be properly undone when we unbind_to.
   It can be either a plain SPECPDL_LET or a SPECPDL_LET_LOCAL/DEFAULT.
   - SYMBOL is the variable being bound.  Note that it should not be
     aliased (i.e. when let-binding V1 that's aliased to V2, we want
     to record V2 here).
   - WHERE tells us in which buffer the binding took place.
     This is used for SPECPDL_LET_LOCAL bindings (i.e. bindings to a
     buffer-local variable) as well as for SPECPDL_LET_DEFAULT bindings,
     i.e. bindings to the default value of a variable which can be
     buffer-local.  */

void
specbind (Lisp_Object symbol, Lisp_Object value)
{
  /* The caller must ensure that the SYMBOL argument is a bare symbol.  */
  struct Lisp_Symbol *sym = XBARE_SYMBOL (symbol);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS:
      sym = SYMBOL_ALIAS (sym); XSETSYMBOL (symbol, sym); goto start;
    case SYMBOL_PLAINVAL:
      /* The most common case is that of a non-constant symbol with a
	 trivial value.  Make that as fast as we can.  */
      specpdl_ptr->let.kind = SPECPDL_LET;
      specpdl_ptr->let.symbol = symbol;
      specpdl_ptr->let.old_value = SYMBOL_VAL (sym);
      specpdl_ptr->let.where.kbd = NULL;
      break;
    case SYMBOL_LOCALIZED:
    case SYMBOL_FORWARDED:
      {
	Lisp_Object ovalue = find_symbol_value (symbol);
	specpdl_ptr->let.kind = SPECPDL_LET_LOCAL;
	specpdl_ptr->let.symbol = symbol;
	specpdl_ptr->let.old_value = ovalue;
	specpdl_ptr->let.where.buf = Fcurrent_buffer ();

	eassert (sym->u.s.redirect != SYMBOL_LOCALIZED
		 || (BASE_EQ (SYMBOL_BLV (sym)->where, Fcurrent_buffer ())));

	if (sym->u.s.redirect == SYMBOL_LOCALIZED)
	  {
	    if (!blv_found (SYMBOL_BLV (sym)))
	      specpdl_ptr->let.kind = SPECPDL_LET_DEFAULT;
	  }
	else if (BUFFER_OBJFWDP (SYMBOL_FWD (sym)))
	  {
	    /* If SYMBOL is a per-buffer variable which doesn't have a
	       buffer-local value here, make the `let' change the global
	       value by changing the value of SYMBOL in all buffers not
	       having their own value.  This is consistent with what
	       happens with other buffer-local variables.  */
	    if (NILP (Flocal_variable_p (symbol, Qnil)))
	      specpdl_ptr->let.kind = SPECPDL_LET_DEFAULT;
	  }
	else if (KBOARD_OBJFWDP (SYMBOL_FWD (sym)))
	  {
	    specpdl_ptr->let.where.kbd = kboard_for_bindings ();
	    specpdl_ptr->let.kind = SPECPDL_LET;
	  }
	else
	  specpdl_ptr->let.kind = SPECPDL_LET;

	break;
      }
    default: emacs_abort ();
    }
  grow_specpdl ();
  do_specbind (sym, specpdl_ptr - 1, value, SET_INTERNAL_BIND);
}

/* Push unwind-protect entries of various types.  */

void
record_unwind_protect (void (*function) (Lisp_Object), Lisp_Object arg)
{
  specpdl_ptr->unwind.kind = SPECPDL_UNWIND;
  specpdl_ptr->unwind.func = function;
  specpdl_ptr->unwind.arg = arg;
  specpdl_ptr->unwind.eval_depth = lisp_eval_depth;
  grow_specpdl ();
}

void
record_unwind_protect_array (Lisp_Object *array, ptrdiff_t nelts)
{
  specpdl_ptr->unwind_array.kind = SPECPDL_UNWIND_ARRAY;
  specpdl_ptr->unwind_array.array = array;
  specpdl_ptr->unwind_array.nelts = nelts;
  grow_specpdl ();
}

void
record_unwind_protect_ptr (void (*function) (void *), void *arg)
{
  specpdl_ptr->unwind_ptr.kind = SPECPDL_UNWIND_PTR;
  specpdl_ptr->unwind_ptr.func = function;
  specpdl_ptr->unwind_ptr.arg = arg;
  specpdl_ptr->unwind_ptr.mark = NULL;
  grow_specpdl ();
}

/* Like `record_unwind_protect_ptr', but also specifies a function
   for GC-marking Lisp objects only reachable through ARG.  */
void
record_unwind_protect_ptr_mark (void (*function) (void *), void *arg,
				void (*mark) (void *))
{
  specpdl_ptr->unwind_ptr.kind = SPECPDL_UNWIND_PTR;
  specpdl_ptr->unwind_ptr.func = function;
  specpdl_ptr->unwind_ptr.arg = arg;
  specpdl_ptr->unwind_ptr.mark = mark;
  grow_specpdl ();
}

void
record_unwind_protect_int (void (*function) (int), int arg)
{
  specpdl_ptr->unwind_int.kind = SPECPDL_UNWIND_INT;
  specpdl_ptr->unwind_int.func = function;
  specpdl_ptr->unwind_int.arg = arg;
  grow_specpdl ();
}

void
record_unwind_protect_intmax (void (*function) (intmax_t), intmax_t arg)
{
  specpdl_ptr->unwind_intmax.kind = SPECPDL_UNWIND_INTMAX;
  specpdl_ptr->unwind_intmax.func = function;
  specpdl_ptr->unwind_intmax.arg = arg;
  grow_specpdl ();
}

void
record_unwind_protect_excursion (void)
{
  specpdl_ptr->unwind_excursion.kind = SPECPDL_UNWIND_EXCURSION;
  save_excursion_save (specpdl_ptr);
  grow_specpdl ();
}

void
record_unwind_protect_void (void (*function) (void))
{
  specpdl_ptr->unwind_void.kind = SPECPDL_UNWIND_VOID;
  specpdl_ptr->unwind_void.func = function;
  grow_specpdl ();
}

void
record_unwind_protect_module (enum specbind_tag kind, void *ptr)
{
  specpdl_ptr->kind = kind;
  specpdl_ptr->unwind_ptr.func = NULL;
  specpdl_ptr->unwind_ptr.arg = ptr;
  specpdl_ptr->unwind_ptr.mark = NULL;
  grow_specpdl ();
}

static void
do_one_unbind (union specbinding *this_binding, bool unwinding,
               enum Set_Internal_Bind bindflag)
{
  KBOARD *kbdwhere = NULL;

  eassert (unwinding || this_binding->kind >= SPECPDL_LET);
  switch (this_binding->kind)
    {
    case SPECPDL_UNWIND:
      lisp_eval_depth = this_binding->unwind.eval_depth;
      this_binding->unwind.func (this_binding->unwind.arg);
      break;
    case SPECPDL_UNWIND_ARRAY:
      xfree (this_binding->unwind_array.array);
      break;
    case SPECPDL_UNWIND_PTR:
      this_binding->unwind_ptr.func (this_binding->unwind_ptr.arg);
      break;
    case SPECPDL_UNWIND_INT:
      this_binding->unwind_int.func (this_binding->unwind_int.arg);
      break;
    case SPECPDL_UNWIND_INTMAX:
      this_binding->unwind_intmax.func (this_binding->unwind_intmax.arg);
      break;
    case SPECPDL_UNWIND_VOID:
      this_binding->unwind_void.func ();
      break;
    case SPECPDL_UNWIND_EXCURSION:
      save_excursion_restore (this_binding->unwind_excursion.marker,
			      this_binding->unwind_excursion.window);
      break;
    case SPECPDL_BACKTRACE:
    case SPECPDL_NOP:
      break;
#ifdef HAVE_MODULES
    case SPECPDL_MODULE_RUNTIME:
      finalize_runtime_unwind (this_binding->unwind_ptr.arg);
      break;
    case SPECPDL_MODULE_ENVIRONMENT:
      finalize_environment_unwind (this_binding->unwind_ptr.arg);
      break;
#endif
    case SPECPDL_LET:
      { /* If variable has a trivial value (no forwarding), and isn't
	   trapped, we can just set it.  */
	Lisp_Object sym = specpdl_symbol (this_binding);
	if (SYMBOLP (sym) && XSYMBOL (sym)->u.s.redirect == SYMBOL_PLAINVAL)
	  {
	    if (XSYMBOL (sym)->u.s.trapped_write == SYMBOL_UNTRAPPED_WRITE)
	      SET_SYMBOL_VAL (XSYMBOL (sym), specpdl_old_value (this_binding));
	    else
	      set_internal (sym, specpdl_old_value (this_binding),
                            Qnil, bindflag);
	    break;
	  }
      }
      /* Come here only if make_local_foo was used for the first time
	 on this var within this let or the symbol is not a plainval.  */
      kbdwhere = specpdl_kboard (this_binding);
      FALLTHROUGH;
    case SPECPDL_LET_DEFAULT:
      set_default_internal (specpdl_symbol (this_binding),
                            specpdl_old_value (this_binding),
                            bindflag, kbdwhere);
      break;
    case SPECPDL_LET_LOCAL:
      {
	Lisp_Object symbol = specpdl_symbol (this_binding);
	Lisp_Object where = specpdl_where (this_binding);
	Lisp_Object old_value = specpdl_old_value (this_binding);
	eassert (BUFFERP (where));

	/* If this was a local binding, reset the value in the appropriate
	   buffer, but only if that buffer's binding still exists.  */
	if (!NILP (Flocal_variable_p (symbol, where)))
          set_internal (symbol, old_value, where, bindflag);
      }
      break;
    }
}

static void
do_nothing (void)
{}

/* Push an unwind-protect entry that does nothing, so that
   set_unwind_protect_ptr can overwrite it later.  */

void
record_unwind_protect_nothing (void)
{
  record_unwind_protect_void (do_nothing);
}

/* Clear the unwind-protect entry COUNT, so that it does nothing.
   It need not be at the top of the stack.  */

void
clear_unwind_protect (specpdl_ref count)
{
  union specbinding *p = specpdl_ref_to_ptr (count);
  p->unwind_void.kind = SPECPDL_UNWIND_VOID;
  p->unwind_void.func = do_nothing;
}

/* Set the unwind-protect entry COUNT so that it invokes FUNC (ARG).
   It need not be at the top of the stack.  Discard the entry's
   previous value without invoking it.  */

void
set_unwind_protect (specpdl_ref count, void (*func) (Lisp_Object),
		    Lisp_Object arg)
{
  union specbinding *p = specpdl_ref_to_ptr (count);
  p->unwind.kind = SPECPDL_UNWIND;
  p->unwind.func = func;
  p->unwind.arg = arg;
  p->unwind.eval_depth = lisp_eval_depth;
}

void
set_unwind_protect_ptr (specpdl_ref count, void (*func) (void *), void *arg)
{
  union specbinding *p = specpdl_ref_to_ptr (count);
  p->unwind_ptr.kind = SPECPDL_UNWIND_PTR;
  p->unwind_ptr.func = func;
  p->unwind_ptr.arg = arg;
  p->unwind_ptr.mark = NULL;
}

/* Pop and execute entries from the unwind-protect stack until the
   depth COUNT is reached.  Return VALUE.  */

Lisp_Object
unbind_to (specpdl_ref count, Lisp_Object value)
{
  Lisp_Object quitf = Vquit_flag;

  Vquit_flag = Qnil;

  while (specpdl_ptr != specpdl_ref_to_ptr (count))
    {
      /* Copy the binding, and decrement specpdl_ptr, before we do
	 the work to unbind it.  We decrement first
	 so that an error in unbinding won't try to unbind
	 the same entry again, and we copy the binding first
	 in case more bindings are made during some of the code we run.  */

      union specbinding this_binding;
      this_binding = *--specpdl_ptr;

      do_one_unbind (&this_binding, true, SET_INTERNAL_UNBIND);
    }

  if (NILP (Vquit_flag) && !NILP (quitf))
    Vquit_flag = quitf;

  return value;
}

DEFUN ("special-variable-p", Fspecial_variable_p, Sspecial_variable_p, 1, 1, 0,
       doc: /* Return non-nil if SYMBOL's global binding has been declared special.
A special variable is one that will be bound dynamically, even in a
context where binding is lexical by default.  */)
  (Lisp_Object symbol)
{
   CHECK_SYMBOL (symbol);
   return XSYMBOL (symbol)->u.s.declared_special ? Qt : Qnil;
}


static union specbinding *
get_backtrace_starting_at (Lisp_Object base)
{
  union specbinding *pdl = backtrace_top ();

  if (!NILP (base))
    { /* Skip up to `base'.  */
      int offset = 0;
      if (CONSP (base) && FIXNUMP (XCAR (base)))
        {
          offset = XFIXNUM (XCAR (base));
          base = XCDR (base);
        }
      base = Findirect_function (base, Qt);
      while (backtrace_p (pdl)
             && !EQ (base, Findirect_function (backtrace_function (pdl), Qt)))
        pdl = backtrace_next (pdl);
      while (backtrace_p (pdl) && offset-- > 0)
        pdl = backtrace_next (pdl);
    }

  return pdl;
}

static union specbinding *
get_backtrace_frame (Lisp_Object nframes, Lisp_Object base)
{
  register EMACS_INT i;

  CHECK_FIXNAT (nframes);
  union specbinding *pdl = get_backtrace_starting_at (base);

  /* Find the frame requested.  */
  for (i = XFIXNAT (nframes); i > 0 && backtrace_p (pdl); i--)
    pdl = backtrace_next (pdl);

  return pdl;
}

static Lisp_Object
backtrace_frame_apply (Lisp_Object function, union specbinding *pdl)
{
  if (!backtrace_p (pdl))
    return Qnil;

  Lisp_Object flags = Qnil;
  if (backtrace_debug_on_exit (pdl))
    flags = list2 (QCdebug_on_exit, Qt);

  if (backtrace_nargs (pdl) == UNEVALLED)
    return calln (function, Qnil, backtrace_function (pdl), *backtrace_args (pdl), flags);
  else
    {
      Lisp_Object tem = Flist (backtrace_nargs (pdl), backtrace_args (pdl));
      return calln (function, Qt, backtrace_function (pdl), tem, flags);
    }
}

DEFUN ("backtrace-debug", Fbacktrace_debug, Sbacktrace_debug, 2, 3, 0,
       doc: /* Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.
LEVEL and BASE specify the activation frame to use, as in `backtrace-frame'.
The debugger is entered when that frame exits, if the flag is non-nil.  */)
  (Lisp_Object level, Lisp_Object flag, Lisp_Object base)
{
  CHECK_FIXNUM (level);
  union specbinding *pdl = get_backtrace_frame (level, base);

  if (backtrace_p (pdl))
    set_backtrace_debug_on_exit (pdl, !NILP (flag));

  return flag;
}

DEFUN ("mapbacktrace", Fmapbacktrace, Smapbacktrace, 1, 2, 0,
       doc: /* Call FUNCTION for each frame in backtrace.
If BASE is non-nil, it should be a function and iteration will start
from its nearest activation frame.
FUNCTION is called with 4 arguments: EVALD, FUNC, ARGS, and FLAGS.  If
a frame has not evaluated its arguments yet or is a special form,
EVALD is nil and ARGS is a list of forms.  If a frame has evaluated
its arguments and called its function already, EVALD is t and ARGS is
a list of values.
FLAGS is a plist of properties of the current frame: currently, the
only supported property is :debug-on-exit.  `mapbacktrace' always
returns nil.  */)
     (Lisp_Object function, Lisp_Object base)
{
  union specbinding *pdl = get_backtrace_starting_at (base);

  while (backtrace_p (pdl))
    {
      ptrdiff_t i = pdl - specpdl;
      backtrace_frame_apply (function, pdl);
      /* Beware! PDL is no longer valid here because FUNCTION might
         have caused grow_specpdl to reallocate pdlvec.  We must use
         the saved index, cf. Bug#27258.  */
      pdl = backtrace_next (&specpdl[i]);
    }

  return Qnil;
}

DEFUN ("backtrace-frame--internal", Fbacktrace_frame_internal,
       Sbacktrace_frame_internal, 3, 3, NULL,
       doc: /* Call FUNCTION on stack frame NFRAMES away from BASE.
Return the result of FUNCTION, or nil if no matching frame could be found. */)
     (Lisp_Object function, Lisp_Object nframes, Lisp_Object base)
{
  return backtrace_frame_apply (function, get_backtrace_frame (nframes, base));
}

DEFUN ("backtrace--frames-from-thread", Fbacktrace_frames_from_thread,
       Sbacktrace_frames_from_thread, 1, 1, NULL,
       doc: /* Return the list of backtrace frames from current execution point in THREAD.
If a frame has not evaluated the arguments yet (or is a special form),
the value of the list element is (nil FUNCTION ARG-FORMS...).
If a frame has evaluated its arguments and called its function already,
the value of the list element is (t FUNCTION ARG-VALUES...).
A &rest arg is represented as the tail of the list ARG-VALUES.
FUNCTION is whatever was supplied as car of evaluated list,
or a lambda expression for macro calls.  */)
     (Lisp_Object thread)
{
  struct thread_state *tstate;
  CHECK_THREAD (thread);
  tstate = XTHREAD (thread);

  union specbinding *pdl = backtrace_thread_top (tstate);
  Lisp_Object list = Qnil;

  while (backtrace_thread_p (tstate, pdl))
    {
      Lisp_Object frame;
      if (backtrace_nargs (pdl) == UNEVALLED)
	frame = Fcons (Qnil,
		      Fcons (backtrace_function (pdl), *backtrace_args (pdl)));
      else
	{
	  Lisp_Object tem = Flist (backtrace_nargs (pdl), backtrace_args (pdl));
	  frame = Fcons (Qt, Fcons (backtrace_function (pdl), tem));
	}
      list = Fcons (frame, list);
      pdl = backtrace_thread_next (tstate, pdl);
    }
  return Fnreverse (list);
}

/* For backtrace-eval, we want to temporarily unwind the last few elements of
   the specpdl stack, and then rewind them.  We store the pre-unwind values
   directly in the pre-existing specpdl elements (i.e. we swap the current
   value and the old value stored in the specpdl), kind of like the inplace
   pointer-reversal trick.  As it turns out, the rewind does the same as the
   unwind, except it starts from the other end of the specpdl stack, so we use
   the same function for both unwind and rewind.
   This same code is used when switching threads, except in that case
   we unwind/rewind the whole specpdl of the threads.  */
void
specpdl_unrewind (union specbinding *pdl, int distance, bool vars_only)
{
  union specbinding *tmp = pdl;
  int step = -1;
  KBOARD *kbdwhere;

  if (distance < 0)
    { /* It's a rewind rather than unwind.  */
      tmp += distance - 1;
      step = 1;
      distance = -distance;
    }

  for (; distance > 0; distance--)
    {
      tmp += step;
      kbdwhere = NULL;

      switch (tmp->kind)
	{
	  /* FIXME: Ideally we'd like to "temporarily unwind" (some of) those
	     unwind_protect, but the problem is that we don't know how to
	     rewind them afterwards.  */
	case SPECPDL_UNWIND:
	  if (vars_only)
	    break;
	  if (tmp->unwind.func == set_buffer_if_live)
	    {
	      Lisp_Object oldarg = tmp->unwind.arg;
	      tmp->unwind.arg = Fcurrent_buffer ();
	      set_buffer_if_live (oldarg);
	    }
	  break;
	case SPECPDL_UNWIND_EXCURSION:
	  if (vars_only)
	    break;
	  {
	    Lisp_Object marker = tmp->unwind_excursion.marker;
	    Lisp_Object window = tmp->unwind_excursion.window;
	    save_excursion_save (tmp);
	    save_excursion_restore (marker, window);
	  }
	  break;
	case SPECPDL_LET:
	  { /* If variable has a trivial value (no forwarding), we can
	       just set it.  No need to check for constant symbols here,
	       since that was already done by specbind.  */
	    Lisp_Object sym = specpdl_symbol (tmp);
	    if (SYMBOLP (sym)
		&& XSYMBOL (sym)->u.s.redirect == SYMBOL_PLAINVAL)
	      {
		Lisp_Object old_value = specpdl_old_value (tmp);
		set_specpdl_old_value (tmp, SYMBOL_VAL (XSYMBOL (sym)));
		SET_SYMBOL_VAL (XSYMBOL (sym), old_value);
		break;
	      }
	  }
	  /* Come here only if make_local_foo was used for the first
	     time on this var within this let or the symbol is forwarded.  */
	  kbdwhere = specpdl_kboard (tmp);
	  FALLTHROUGH;
	case SPECPDL_LET_DEFAULT:
	  {
	    Lisp_Object sym = specpdl_symbol (tmp);
	    Lisp_Object old_value = specpdl_old_value (tmp);
	    set_specpdl_old_value (tmp, default_value (sym));
	    set_default_internal (sym, old_value, SET_INTERNAL_THREAD_SWITCH,
				  kbdwhere);
	  }
	  break;
	case SPECPDL_LET_LOCAL:
	  {
	    Lisp_Object symbol = specpdl_symbol (tmp);
	    Lisp_Object where = specpdl_where (tmp);
	    Lisp_Object old_value = specpdl_old_value (tmp);
	    eassert (BUFFERP (where));

	    /* If this was a local binding, reset the value in the appropriate
	       buffer, but only if that buffer's binding still exists.  */
	    if (!NILP (Flocal_variable_p (symbol, where)))
	      {
		set_specpdl_old_value
		  (tmp, buffer_local_value (symbol, where));
                set_internal (symbol, old_value, where,
                              SET_INTERNAL_THREAD_SWITCH);
	      }
	    else
	      /* If the var is not local any more, it can't be undone nor
                 redone, so just zap it.
                 This is important in case the buffer re-gains a local value
                 before we unrewind again, in which case we'd risk applying
                 this entry in the wrong direction.  */
	      tmp->kind = SPECPDL_NOP;
	  }
	  break;

	default: break;
	}
    }
}

static void
backtrace_eval_unrewind (int distance)
{
  specpdl_unrewind (specpdl_ptr, distance, false);
}

DEFUN ("backtrace-eval", Fbacktrace_eval, Sbacktrace_eval, 2, 3, NULL,
       doc: /* Evaluate EXP in the context of some activation frame.
NFRAMES and BASE specify the activation frame to use, as in `backtrace-frame'.  */)
     (Lisp_Object exp, Lisp_Object nframes, Lisp_Object base)
{
  union specbinding *pdl = get_backtrace_frame (nframes, base);
  specpdl_ref count = SPECPDL_INDEX ();
  ptrdiff_t distance = specpdl_ptr - pdl;
  eassert (distance >= 0);

  if (!backtrace_p (pdl))
    error ("Activation frame not found!");

  backtrace_eval_unrewind (distance);
  record_unwind_protect_int (backtrace_eval_unrewind, -distance);

  /* Use eval_sub rather than Feval since the main motivation behind
     backtrace-eval is to be able to get/set the value of lexical variables
     from the debugger.  */
  return unbind_to (count, eval_sub (exp));
}

DEFUN ("backtrace--locals", Fbacktrace__locals, Sbacktrace__locals, 1, 2, NULL,
       doc: /* Return names and values of local variables of a stack frame.
NFRAMES and BASE specify the activation frame to use, as in `backtrace-frame'.  */)
  (Lisp_Object nframes, Lisp_Object base)
{
  union specbinding *frame = get_backtrace_frame (nframes, base);
  union specbinding *prevframe
    = get_backtrace_frame (make_fixnum (XFIXNAT (nframes) - 1), base);
  ptrdiff_t distance = specpdl_ptr - frame;
  Lisp_Object result = Qnil;
  eassert (distance >= 0);

  if (!backtrace_p (prevframe))
    error ("Activation frame not found!");
  if (!backtrace_p (frame))
    error ("Activation frame not found!");

  /* The specpdl entries normally contain the symbol being bound along with its
     `old_value', so it can be restored.  The new value to which it is bound is
     available in one of two places: either in the current value of the
     variable (if it hasn't been rebound yet) or in the `old_value' slot of the
     next specpdl entry for it.
     `backtrace_eval_unrewind' happens to swap the role of `old_value'
     and "new value", so we abuse it here, to fetch the new value.
     It's ugly (we'd rather not modify global data) and a bit inefficient,
     but it does the job for now.  */
  backtrace_eval_unrewind (distance);

  /* Grab values.  */
  {
    union specbinding *tmp = prevframe;
    for (; tmp > frame; tmp--)
      {
	switch (tmp->kind)
	  {
	  case SPECPDL_LET:
	  case SPECPDL_LET_DEFAULT:
	  case SPECPDL_LET_LOCAL:
	    {
	      Lisp_Object sym = specpdl_symbol (tmp);
	      Lisp_Object val = specpdl_old_value (tmp);
	      if (BASE_EQ (sym, Qinternal_interpreter_environment))
		{
		  Lisp_Object env = val;
		  for (; CONSP (env); env = XCDR (env))
		    {
		      Lisp_Object binding = XCAR (env);
		      if (CONSP (binding))
			result = Fcons (Fcons (XCAR (binding),
					       XCDR (binding)),
					result);
		    }
		}
	      else
		result = Fcons (Fcons (sym, val), result);
	    }
	    break;

	  default: break;
	  }
      }
  }

  /* Restore values from specpdl to original place.  */
  backtrace_eval_unrewind (-distance);

  return result;
}


void
mark_specpdl (union specbinding *first, union specbinding *ptr)
{
  union specbinding *pdl;
  for (pdl = first; pdl != ptr; pdl++)
    {
      switch (pdl->kind)
        {
	case SPECPDL_UNWIND:
	  mark_object (specpdl_arg (pdl));
	  break;

	case SPECPDL_UNWIND_ARRAY:
	  mark_objects (pdl->unwind_array.array, pdl->unwind_array.nelts);
	  break;

	case SPECPDL_UNWIND_EXCURSION:
	  mark_object (pdl->unwind_excursion.marker);
	  mark_object (pdl->unwind_excursion.window);
	  break;

	case SPECPDL_BACKTRACE:
	  {
	    ptrdiff_t nargs = backtrace_nargs (pdl);
	    mark_object (backtrace_function (pdl));
	    if (nargs == UNEVALLED)
	      nargs = 1;
	    mark_objects (backtrace_args (pdl), nargs);
	  }
	  break;

#ifdef HAVE_MODULES
        case SPECPDL_MODULE_RUNTIME:
          break;
        case SPECPDL_MODULE_ENVIRONMENT:
          mark_module_environment (pdl->unwind_ptr.arg);
          break;
#endif

	case SPECPDL_LET_DEFAULT:
	case SPECPDL_LET_LOCAL:
	  mark_object (specpdl_where (pdl));
	  FALLTHROUGH;
	case SPECPDL_LET:
	  mark_object (specpdl_symbol (pdl));
	  mark_object (specpdl_old_value (pdl));
	  break;

	case SPECPDL_UNWIND_PTR:
	  if (pdl->unwind_ptr.mark)
	    pdl->unwind_ptr.mark (pdl->unwind_ptr.arg);
	  break;

	case SPECPDL_UNWIND_INT:
	case SPECPDL_UNWIND_INTMAX:
        case SPECPDL_UNWIND_VOID:
	case SPECPDL_NOP:
	  break;

	/* While other loops that scan the specpdl use "default: break;"
	   for simplicity, here we explicitly list all cases and abort
	   if we find an unexpected value, as a sanity check.  */
	default:
	  emacs_abort ();
	}
    }
}

/* Fill ARRAY of size SIZE with backtrace entries, most recent call first.
   Truncate the backtrace if longer than SIZE; pad with nil if shorter.  */
void
get_backtrace (Lisp_Object *array, ptrdiff_t size)
{
  /* Copy the backtrace contents into working memory.  */
  union specbinding *pdl = backtrace_top ();
  ptrdiff_t i = 0;
  for (; i < size && backtrace_p (pdl); i++, pdl = backtrace_next (pdl))
    array[i] = backtrace_function (pdl);
  for (; i < size; i++)
    array[i] = Qnil;
}

Lisp_Object backtrace_top_function (void)
{
  union specbinding *pdl = backtrace_top ();
  return (backtrace_p (pdl) ? backtrace_function (pdl) : Qnil);
}

void
syms_of_eval (void)
{
  DEFVAR_INT ("max-lisp-eval-depth", max_lisp_eval_depth,
	      doc: /* Limit on depth in `eval', `apply' and `funcall' before error.

This limit serves to catch infinite recursions for you before they cause
actual stack overflow in C, which would be fatal for Emacs.
You can safely make it considerably larger than its default value,
if that proves inconveniently small.  However, if you increase it too far,
Emacs could overflow the real C stack, and crash.  */);
  max_lisp_eval_depth = 1600;

  DEFVAR_INT ("lisp-eval-depth-reserve", lisp_eval_depth_reserve,
	      doc: /* Extra depth that can be allocated to handle errors.
This is the max depth that the system will add to `max-lisp-eval-depth'
when calling debuggers or `handler-bind' handlers.  */);
  lisp_eval_depth_reserve = 200;

  DEFVAR_LISP ("quit-flag", Vquit_flag,
	       doc: /* Non-nil causes `eval' to abort, unless `inhibit-quit' is non-nil.
If the value is t, that means do an ordinary quit.
If the value equals `throw-on-input', that means quit by throwing
to the tag specified in `throw-on-input'; it's for handling `while-no-input'.
Typing C-g sets `quit-flag' to t, regardless of `inhibit-quit',
but `inhibit-quit' non-nil prevents anything from taking notice of that.  */);
  Vquit_flag = Qnil;

  DEFVAR_LISP ("inhibit-quit", Vinhibit_quit,
	       doc: /* Non-nil inhibits C-g quitting from happening immediately.
Note that `quit-flag' will still be set by typing C-g,
so a quit will be signaled as soon as `inhibit-quit' is nil.
To prevent this happening, set `quit-flag' to nil
before making `inhibit-quit' nil.  */);
  Vinhibit_quit = Qnil;

  DEFSYM (Qsetq, "setq");
  DEFSYM (Qinhibit_quit, "inhibit-quit");
  DEFSYM (Qautoload, "autoload");
  DEFSYM (Qinhibit_debugger, "inhibit-debugger");
  DEFSYM (Qmacro, "macro");

  /* Note that the process handling also uses Qexit, but we don't want
     to staticpro it twice, so we just do it here.  */
  DEFSYM (Qexit, "exit");

  DEFSYM (Qinteractive, "interactive");
  DEFSYM (Qcommandp, "commandp");
  DEFSYM (Qand_rest, "&rest");
  DEFSYM (Qand_optional, "&optional");
  DEFSYM (QCdocumentation, ":documentation");
  DEFSYM (Qdebug, "debug");
  DEFSYM (Qdebug_early, "debug-early");
  DEFSYM (Qdebug_early__handler, "debug-early--handler");
  DEFSYM (Qdebugger_may_continue, "debugger-may-continue");
  DEFSYM (Qdisplay_warning, "display-warning");
  DEFSYM (Qlosing_value, "losing-value");

  DEFVAR_LISP ("inhibit-debugger", Vinhibit_debugger,
	       doc: /* Non-nil means never enter the debugger.
Normally set while the debugger is already active, to avoid recursive
invocations.  */);
  Vinhibit_debugger = Qnil;

  DEFVAR_LISP ("debug-on-error", Vdebug_on_error,
	       doc: /* Non-nil means enter debugger if an error is signaled.
Does not apply to errors handled by `condition-case' or those
matched by `debug-ignored-errors'.
If the value is a list, an error only means to enter the debugger
if one of its condition symbols appears in the list.
When you evaluate an expression interactively, this variable
is temporarily non-nil if `eval-expression-debug-on-error' is non-nil.
The command `toggle-debug-on-error' toggles this.
See also the variable `debug-on-quit' and `inhibit-debugger'.  */);
  Vdebug_on_error = Qnil;

  DEFVAR_LISP ("debug-ignored-errors", Vdebug_ignored_errors,
    doc: /* List of errors for which the debugger should not be called.
Each element may be a condition-name or a regexp that matches error messages.
If any element applies to a given error, that error skips the debugger
and just returns to top level.
If you invoke Emacs with --debug-init, and want to remove some
elements from the default value of this variable, use `setq' to
change the value of the variable to a new list, rather than `delq'
to remove some errors from the list.
This overrides the variable `debug-on-error'.
It does not apply to errors handled by `condition-case'.  */);
  Vdebug_ignored_errors = Qnil;

  DEFVAR_BOOL ("debug-on-quit", debug_on_quit,
    doc: /* Non-nil means enter debugger if quit is signaled (C-g, for example).
Does not apply if quit is handled by a `condition-case'.  */);
  debug_on_quit = 0;

  DEFVAR_BOOL ("debug-on-next-call", debug_on_next_call,
	       doc: /* Non-nil means enter debugger before next `eval', `apply' or `funcall'.  */);

  DEFVAR_BOOL ("backtrace-on-redisplay-error", backtrace_on_redisplay_error,
	       doc: /* Non-nil means create a backtrace if a lisp error occurs in redisplay.
The backtrace is written to buffer *Redisplay-trace*.  */);
  backtrace_on_redisplay_error = false;

  DEFVAR_BOOL ("debugger-may-continue", debugger_may_continue,
	       doc: /* Non-nil means debugger may continue execution.
This is nil when the debugger is called under circumstances where it
might not be safe to continue.  */);
  debugger_may_continue = 1;

  DEFVAR_BOOL ("debugger-stack-frame-as-list", debugger_stack_frame_as_list,
	       doc: /* Non-nil means display call stack frames as lists. */);
  debugger_stack_frame_as_list = 0;

  DEFSYM (Qdebugger, "debugger");
  DEFVAR_LISP ("debugger", Vdebugger,
	       doc: /* Function to call to invoke debugger.
If due to frame exit, arguments are `exit' and the value being returned;
 this function's value will be returned instead of that.
If due to error, arguments are `error' and a list of arguments to `signal'.
If due to `apply' or `funcall' entry, one argument, `lambda'.
If due to `eval' entry, one argument, t.
IF the desired entry point of the debugger is higher in the call stack,
it can be specified with the keyword argument `:backtrace-base', whose
format should be the same as the BASE argument of `backtrace-frame'.  */);
  Vdebugger = Qdebug_early;

  DEFVAR_LISP ("signal-hook-function", Vsignal_hook_function,
	       doc: /* If non-nil, this is a function for `signal' to call.
It receives the same arguments that `signal' was given.
The Edebug package uses this to regain control.  */);
  Vsignal_hook_function = Qnil;

  DEFVAR_LISP ("debug-on-signal", Vdebug_on_signal,
	       doc: /* Non-nil means call the debugger regardless of condition handlers.
Note that `debug-on-error', `debug-on-quit' and friends
still determine whether to handle the particular condition.  */);
  Vdebug_on_signal = Qnil;

  DEFVAR_BOOL ("backtrace-on-error-noninteractive",
               backtrace_on_error_noninteractive,
               doc: /* Non-nil means print backtrace on error in batch mode.
If this is nil, errors in batch mode will just print the error
message upon encountering an unhandled error, without showing
the Lisp backtrace.  */);
  backtrace_on_error_noninteractive = true;

  /* The value of num_nonmacro_input_events as of the last time we
   started to enter the debugger.  If we decide to enter the debugger
   again when this is still equal to num_nonmacro_input_events, then we
   know that the debugger itself has an error, and we should just
   signal the error instead of entering an infinite loop of debugger
   invocations.  */
  DEFSYM (Qinternal_when_entered_debugger, "internal-when-entered-debugger");
  DEFVAR_INT ("internal-when-entered-debugger", when_entered_debugger,
              doc: /* The number of keyboard events as of last time `debugger' was called.
Used to avoid infinite loops if the debugger itself has an error.
Don't set this unless you're sure that can't happen.  */);

  /* When lexical binding is being used,
   Vinternal_interpreter_environment is non-nil, and contains an alist
   of lexically-bound variable, or (t), indicating an empty
   environment.  The lisp name of this variable would be
   `internal-interpreter-environment' if it weren't hidden.
   Every element of this list can be either a cons (VAR . VAL)
   specifying a lexical binding, or a single symbol VAR indicating
   that this variable should use dynamic scoping.  */
  DEFSYM (Qinternal_interpreter_environment,
	  "internal-interpreter-environment");
  DEFVAR_LISP ("internal-interpreter-environment",
		Vinternal_interpreter_environment,
	       doc: /* If non-nil, the current lexical environment of the lisp interpreter.
When lexical binding is not being used, this variable is nil.
A value of `(t)' indicates an empty environment, otherwise it is an
alist of active lexical bindings.  */);
  Vinternal_interpreter_environment = Qnil;
  /* Don't export this variable to Elisp, so no one can mess with it
     (Just imagine if someone makes it buffer-local).  */
  Funintern (Qinternal_interpreter_environment, Qnil);

  DEFVAR_LISP ("internal-make-interpreted-closure-function",
	       Vinternal_make_interpreted_closure_function,
	       doc: /* Function to filter the env when constructing a closure.  */);
  Vinternal_make_interpreted_closure_function = Qnil;

  Vrun_hooks = intern_c_string ("run-hooks");
  staticpro (&Vrun_hooks);

  staticpro (&Vautoload_queue);
  Vautoload_queue = Qnil;
  staticpro (&Vsignaling_function);
  Vsignaling_function = Qnil;

  staticpro (&Qcatch_all_memory_full);
  /* Make sure Qcatch_all_memory_full is a unique object.  We could
     also use something like Fcons (Qnil, Qnil), but json.c treats any
     cons cell as error data, so use an uninterned symbol instead.  */
  Qcatch_all_memory_full
    = Fmake_symbol (build_string ("catch-all-memory-full"));

  staticpro (&list_of_t);
  list_of_t = list1 (Qt);

  defsubr (&Sor);
  defsubr (&Sand);
  defsubr (&Sif);
  defsubr (&Scond);
  defsubr (&Sprogn);
  defsubr (&Sprog1);
  defsubr (&Ssetq);
  defsubr (&Squote);
  defsubr (&Sfunction);
  defsubr (&Smake_interpreted_closure);
  defsubr (&Sdefault_toplevel_value);
  defsubr (&Sset_default_toplevel_value);
  defsubr (&Sbuffer_local_toplevel_value);
  defsubr (&Sset_buffer_local_toplevel_value);
  defsubr (&Sdefvar);
  defsubr (&Sdefvar_1);
  defsubr (&Sdefvaralias);
  DEFSYM (Qdefvaralias, "defvaralias");
  defsubr (&Sinternal_delete_indirect_variable);
  defsubr (&Sdefconst);
  defsubr (&Sdefconst_1);
  defsubr (&Sinternal__define_uninitialized_variable);
  defsubr (&Smake_var_non_special);
  defsubr (&Slet);
  defsubr (&SletX);
  defsubr (&Swhile);
  defsubr (&Sfuncall_with_delayed_message);
  defsubr (&Smacroexpand);
  defsubr (&Scatch);
  defsubr (&Sthrow);
  defsubr (&Sunwind_protect);
  defsubr (&Scondition_case);
  defsubr (&Shandler_bind_1);
  DEFSYM (QCsuccess, ":success");
  defsubr (&Ssignal);
  defsubr (&Scommandp);
  defsubr (&Sautoload);
  defsubr (&Sautoload_do_load);
  defsubr (&Seval);
  defsubr (&Sapply);
  defsubr (&Sfuncall);
  defsubr (&Sfunc_arity);
  defsubr (&Srun_hooks);
  defsubr (&Srun_hook_with_args);
  defsubr (&Srun_hook_with_args_until_success);
  defsubr (&Srun_hook_with_args_until_failure);
  defsubr (&Srun_hook_wrapped);
  defsubr (&Sbacktrace_debug);
  DEFSYM (QCdebug_on_exit, ":debug-on-exit");
  defsubr (&Smapbacktrace);
  defsubr (&Sbacktrace_frame_internal);
  defsubr (&Sbacktrace_frames_from_thread);
  defsubr (&Sbacktrace_eval);
  defsubr (&Sbacktrace__locals);
  defsubr (&Sspecial_variable_p);
  DEFSYM (Qfunctionp, "functionp");
  defsubr (&Sfunctionp);
  defsubr (&Sdebugger_trap);
}
