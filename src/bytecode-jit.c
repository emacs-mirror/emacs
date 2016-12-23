/* JIT compilation of byte code produced by bytecomp.el.
   Copyright (C) 2016 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_LIBJIT
#include "bytecode.h"
#include "lisp.h"
#include "blockinput.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "syntax.h"
#include "window.h"

#include <jit.h>

/* Fetch the next byte from the bytecode stream.  */

#define FETCH *pc++

/* Fetch two bytes from the bytecode stream and make a 16-bit number
   out of them.  */

#define FETCH2 (op = FETCH, op + (FETCH << 8))

/* Push x onto the execution stack.  This used to be #define PUSH(x)
   (*++stackp = (x)) This oddity is necessary because Alliant can't be
   bothered to compile the preincrement operator properly, as of 4/91.
   -JimB */

#define PUSH(x) (top++, *top = (x))

/* Pop a value off the execution stack.  */

#define POP (*top--)

/* Discard n values from the execution stack.  */

#define DISCARD(n) (top -= (n))

/* Get the value which is at the top of the execution stack, but don't
   pop it.  */

#define TOP (*top)

#undef BEFORE_POTENTIAL_GC
#undef AFTER_POTENTIAL_GC
#define BEFORE_POTENTIAL_GC() ((void )0)
#define AFTER_POTENTIAL_GC() ((void )0)

/* Check for jumping out of range.  */

#if BYTE_CODE_SAFE

#define CHECK_RANGE(ARG) \
  if (ARG >= bytestr_length) emacs_abort ()

#else /* not BYTE_CODE_SAFE */

#define CHECK_RANGE(ARG)

#endif /* not BYTE_CODE_SAFE */

/* A version of the QUIT macro which makes sure that the stack top is
   set before signaling `quit'.  */

#define BYTE_CODE_QUIT					\
  do {							\
    if (!NILP (Vquit_flag) && NILP (Vinhibit_quit))	\
      {							\
        Lisp_Object flag = Vquit_flag;			\
	Vquit_flag = Qnil;				\
        BEFORE_POTENTIAL_GC ();				\
	if (EQ (Vthrow_on_input, flag))			\
	  Fthrow (Vthrow_on_input, Qt);			\
	Fsignal (Qquit, Qnil);				\
	AFTER_POTENTIAL_GC ();				\
      }							\
    else if (pending_signals)				\
      process_pending_signals ();			\
  } while (0)

/* Global jit context */
jit_context_t jit_context = NULL;

#define jit_type_Lisp_Object jit_type_nuint

jit_type_t native_varref_sig;
static Lisp_Object
native_varref (Lisp_Object v1)
{
  Lisp_Object v2;

  if (SYMBOLP (v1))
    {
      if (XSYMBOL (v1)->redirect != SYMBOL_PLAINVAL
	  || (v2 = SYMBOL_VAL (XSYMBOL (v1)),
	      EQ (v2, Qunbound)))
	{
	  v2 = Fsymbol_value (v1);
	}
    }
  else
    {
      v2 = Fsymbol_value (v1);
    }
  return v2;
}

jit_type_t native_ifnil_sig;
static bool
native_ifnil (Lisp_Object v1)
{
  maybe_gc ();
  if (NILP (v1))
    {
      BYTE_CODE_QUIT;
      return true;
    }
  else
    return false;
}

jit_type_t native_ifnonnil_sig;
static bool
native_ifnonnil (Lisp_Object v1)
{
  maybe_gc ();
  if (!NILP (v1))
    {
      BYTE_CODE_QUIT;
      return true;
    }
  else
    return false;

}

jit_type_t native_car_sig;
static Lisp_Object
native_car (Lisp_Object v1)
{
  if (CONSP (v1))
    return XCAR (v1);
  else if (NILP (v1))
    return Qnil;
  else
    {
      wrong_type_argument (Qlistp, v1);
    }
}

jit_type_t native_eq_sig;
static Lisp_Object
native_eq (Lisp_Object v1, Lisp_Object v2)
{
  return EQ (v1, v2) ? Qt : Qnil;
}

jit_type_t native_memq_sig;
static Lisp_Object
native_memq (Lisp_Object v1, Lisp_Object v2)
{
  v1 = Fmemq (v1, v2);
  return v1;
}

jit_type_t native_cdr_sig;
static Lisp_Object
native_cdr (Lisp_Object v1)
{
  if (CONSP (v1))
    return XCDR (v1);
  else if (NILP (v1))
    return Qnil;
  else
    {
      wrong_type_argument (Qlistp, v1);
    }
}

jit_type_t native_varset_sig;
static void
native_varset (Lisp_Object sym, Lisp_Object val)
{
  /* Inline the most common case.  */
  if (SYMBOLP (sym)
      && !EQ (val, Qunbound)
      && !XSYMBOL (sym)->redirect
      && !SYMBOL_CONSTANT_P (sym))
    SET_SYMBOL_VAL (XSYMBOL (sym), val);
  else
    {
      set_internal (sym, val, Qnil, 0);
    }
}

jit_type_t specbind_sig;
jit_type_t Ffuncall_sig;

jit_type_t native_unbind_to_sig;
static Lisp_Object
native_unbind_to (ptrdiff_t x, Lisp_Object q)
{
  return unbind_to (SPECPDL_INDEX () - x, q);
}

jit_type_t unbind_to_sig;

jit_type_t byte_code_quit_sig;
static void
byte_code_quit (void)
{
  maybe_gc ();
  BYTE_CODE_QUIT;
}

jit_type_t native_save_excursion_sig;
static void
native_save_excursion (void)
{
  record_unwind_protect (save_excursion_restore,
			 save_excursion_save ());
}

jit_type_t native_save_restriction_sig;
static void
native_save_restriction (void)
{
  record_unwind_protect (save_restriction_restore,
			 save_restriction_save ());
}


jit_type_t native_save_window_excursion_sig;
static Lisp_Object
native_save_window_excursion (Lisp_Object v1)
{
  ptrdiff_t count1 = SPECPDL_INDEX ();
  record_unwind_protect (restore_window_configuration,
			 Fcurrent_window_configuration (Qnil));
  v1 = Fprogn (v1);
  unbind_to (count1, v1);
  return v1;
}

jit_type_t native_catch_sig;
static Lisp_Object
native_catch (Lisp_Object v2, Lisp_Object v1)
{
  return internal_catch (v2, eval_sub, v1);
}

jit_type_t native_pophandler_sig;
static void
native_pophandler (void)
{
  handlerlist = handlerlist->next;
}

jit_type_t native_pushhandler1_sig;
static void *
native_pushhandler1 (Lisp_Object **stack, Lisp_Object tag,
		     int type)
{
  struct handler *c = push_handler (tag, type);
  c->stack = *stack;
  return c->jmp;
}

jit_type_t native_pushhandler2_sig;
static void
native_pushhandler2 (Lisp_Object **stack)
{
  struct handler *c = handlerlist;
  native_pophandler ();
  *stack = c->stack;
  (*stack)++;
  **stack = c->val;
}

jit_type_t native_unwind_protect_sig;
static void
native_unwind_protect (Lisp_Object handler)
{
  record_unwind_protect (NILP (Ffunctionp (handler))
			 ? unwind_body : bcall0,
			 handler);
}

jit_type_t native_temp_output_buffer_setup_sig;
static Lisp_Object
native_temp_output_buffer_setup (Lisp_Object x)
{
  CHECK_STRING (x);
  temp_output_buffer_setup (SSDATA (x));
  return Vstandard_output;
}

jit_type_t native_nth_sig;
static Lisp_Object
native_nth (Lisp_Object v1, Lisp_Object v2)
{
  EMACS_INT n;
  CHECK_NUMBER (v1);
  n = XINT (v1);
  immediate_quit = 1;
  while (--n >= 0 && CONSP (v2))
    v2 = XCDR (v2);
  immediate_quit = 0;
  return CAR (v2);
}

jit_type_t native_symbolp_sig;
jit_type_t native_consp_sig;
jit_type_t native_stringp_sig;
jit_type_t native_listp_sig;
jit_type_t native_not_sig;
static Lisp_Object
native_symbolp (Lisp_Object v1)
{
  return SYMBOLP (v1) ? Qt : Qnil;
}
static Lisp_Object
native_consp (Lisp_Object v1)
{
  return CONSP (v1) ? Qt : Qnil;
}
static Lisp_Object
native_stringp (Lisp_Object v1)
{
  return STRINGP (v1) ? Qt : Qnil;
}
static Lisp_Object
native_listp (Lisp_Object v1)
{
  return CONSP (v1) || NILP (v1) ? Qt : Qnil;
}
static Lisp_Object
native_not (Lisp_Object v1)
{
  return NILP (v1) ? Qt : Qnil;
}

jit_type_t native_add1_sig;
static Lisp_Object
native_add1 (Lisp_Object v1, bool add)
{
  if (INTEGERP (v1))
    {
      XSETINT (v1, XINT (v1) + (add ? 1 : -1));
      return v1;
    }
  else if (add)
    return Fadd1 (v1);
  else
    return Fsub1 (v1);
}

jit_type_t native_eqlsign_sig;
static Lisp_Object
native_eqlsign (Lisp_Object v1, Lisp_Object v2)
{
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (v1);
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (v2);
  if (FLOATP (v1) || FLOATP (v2))
    {
      double f1, f2;

      f1 = (FLOATP (v1) ? XFLOAT_DATA (v1) : XINT (v1));
      f2 = (FLOATP (v2) ? XFLOAT_DATA (v2) : XINT (v2));
      return (f1 == f2 ? Qt : Qnil);
    }
  else
    return (XINT (v1) == XINT (v2) ? Qt : Qnil);
}

jit_type_t arithcompare_sig;
jit_type_t native_negate_sig;
static Lisp_Object
native_negate (Lisp_Object v)
{
  if (INTEGERP (v))
    {
      XSETINT (v, - XINT (v));
      return v;
    }
  else
    return Fminus (1, &v);
}

jit_type_t native_point_sig;
static Lisp_Object
native_point (void)
{
  Lisp_Object v1;
  XSETFASTINT (v1, PT);
  return v1;
}

jit_type_t native_point_max_sig;
static Lisp_Object
native_point_max (void)
{
  Lisp_Object v1;
  XSETFASTINT (v1, ZV);
  return v1;
}

jit_type_t native_point_min_sig;
static Lisp_Object
native_point_min (void)
{
  Lisp_Object v1;
  XSETFASTINT (v1, BEGV);
  return v1;
}

jit_type_t native_current_column_sig;
static Lisp_Object
native_current_column (void)
{
  Lisp_Object v1;
  XSETFASTINT (v1, current_column ());
  return v1;
}

jit_type_t native_interactive_p_sig;
static Lisp_Object
native_interactive_p (void)
{
  return call0 (intern ("interactive-p"));
}

jit_type_t native_char_syntax_sig;
static Lisp_Object
native_char_syntax (Lisp_Object v)
{
  int c;

  CHECK_CHARACTER (v);
  c = XFASTINT (v);
  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    MAKE_CHAR_MULTIBYTE (c);
  XSETFASTINT (v, syntax_code_spec[SYNTAX (c)]);
  return v;
}

jit_type_t native_elt_sig;
static Lisp_Object
native_elt (Lisp_Object v1, Lisp_Object v2)
{
  if (CONSP (v2))
    {
      /* Exchange args and then do nth.  */
      EMACS_INT n;
      CHECK_NUMBER (v2);
      n = XINT (v2);
      immediate_quit = 1;
      while (--n >= 0 && CONSP (v1))
	v1 = XCDR (v1);
      immediate_quit = 0;
      return CAR (v1);
    }
  else
    return Felt (v1, v2);
}

jit_type_t native_car_safe_sig;
static Lisp_Object
native_car_safe (Lisp_Object v)
{
  return CAR_SAFE (v);
}
jit_type_t native_cdr_safe_sig;
static Lisp_Object
native_cdr_safe (Lisp_Object v)
{
  return CDR_SAFE (v);
}

jit_type_t native_number_p_sig;
static Lisp_Object
native_number_p (Lisp_Object v)
{
  return NUMBERP (v) ? Qt : Qnil;
}
jit_type_t native_integer_p_sig;
static Lisp_Object
native_integer_p (Lisp_Object v)
{
  return INTEGERP (v) ? Qt : Qnil;
}

jit_type_t setjmp_sig;

static void
emacs_jit_init (void)
{
#define JIT_SIG_(f, ret, params)		\
  do {						\
    f##_sig =					\
      jit_type_create_signature (		\
        jit_abi_cdecl,				\
        ret,					\
        params,					\
        sizeof (params) / sizeof (params[0]),	\
        1);					\
  } while (0)
#define JIT_SIG(f, ret, ...)			\
  do {						\
    jit_type_t params[] =			\
      {						\
        __VA_ARGS__				\
      };					\
    JIT_SIG_ (f, ret, params);			\
  } while (0)

  jit_context = jit_context_create();

  do {
    jit_type_t params[] =
      {
	jit_type_void_ptr,
#if !defined (HAVE__SETJMP) && defined (HAVE_SIGSETJMP)
	jit_type_sys_int
#endif
      };
    setjmp_sig = jit_type_create_signature (jit_abi_cdecl,
					    jit_type_sys_int, params,
#if !defined (HAVE__SETJMP) && defined (HAVE_SIGSETJMP)
					    2,
#else
					    1,
#endif
					    1);
  } while (0);

  JIT_SIG (native_varref, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_ifnil, jit_type_sys_bool, jit_type_Lisp_Object);
  JIT_SIG (native_ifnonnil, jit_type_sys_bool, jit_type_Lisp_Object);
  JIT_SIG (native_car, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_eq, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_memq, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_cdr, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_varset, jit_type_void, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (specbind, jit_type_void, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (Ffuncall, jit_type_Lisp_Object, jit_type_nuint, jit_type_void_ptr);
  JIT_SIG (native_unbind_to, jit_type_Lisp_Object, jit_type_nuint, jit_type_Lisp_Object);
  JIT_SIG (unbind_to, jit_type_Lisp_Object, jit_type_nuint, jit_type_Lisp_Object);
  JIT_SIG (byte_code_quit, jit_type_void);
  JIT_SIG (native_save_excursion, jit_type_void);
  JIT_SIG (native_save_window_excursion, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_save_restriction, jit_type_void);
  JIT_SIG (native_catch, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_pophandler, jit_type_void);
  JIT_SIG (native_pushhandler1, jit_type_void_ptr, jit_type_create_pointer (jit_type_void_ptr, 1), jit_type_Lisp_Object, jit_type_nint);
  JIT_SIG (native_pushhandler2, jit_type_void, jit_type_create_pointer (jit_type_void_ptr, 1));
  JIT_SIG (native_unwind_protect, jit_type_void, jit_type_Lisp_Object);
  JIT_SIG (native_temp_output_buffer_setup, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_nth, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_symbolp, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_consp, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_stringp, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_listp, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_not, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_add1, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_sys_bool);
  JIT_SIG (native_eqlsign, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (arithcompare, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_nuint);
  JIT_SIG (native_negate, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_point, jit_type_Lisp_Object);
  JIT_SIG (native_point_max, jit_type_Lisp_Object);
  JIT_SIG (native_point_min, jit_type_Lisp_Object);
  JIT_SIG (native_current_column, jit_type_Lisp_Object);
  JIT_SIG (native_interactive_p, jit_type_Lisp_Object);
  JIT_SIG (native_char_syntax, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_elt, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_car_safe, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_cdr_safe, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_number_p, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (native_integer_p, jit_type_Lisp_Object, jit_type_Lisp_Object);
}

Lisp_Object
jit_exec (Lisp_Object byte_code, Lisp_Object args_template, ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object *top;
  Lisp_Object maxdepth = XVECTOR (byte_code)->contents[COMPILED_STACK_DEPTH];

  CHECK_NATNUM (maxdepth);
  if (MAX_ALLOCA / word_size <= XFASTINT (maxdepth))
    memory_full (SIZE_MAX);
  top = alloca ((XFASTINT (maxdepth) + 1) * sizeof *top);

  if (INTEGERP (args_template))
    {
      ptrdiff_t at = XINT (args_template);
      bool rest = (at & 128) != 0;
      int mandatory = at & 127;
      ptrdiff_t nonrest = at >> 8;
      eassert (mandatory <= nonrest);
      if (nargs <= nonrest)
	{
	  ptrdiff_t i;
	  for (i = 0 ; i < nargs; i++, args++)
	    PUSH (*args);
	  if (nargs < mandatory)
	    /* Too few arguments.  */
	    Fsignal (Qwrong_number_of_arguments,
		     list2 (Fcons (make_number (mandatory),
				   rest ? Qand_rest : make_number (nonrest)),
			    make_number (nargs)));
	  else
	    {
	      for (; i < nonrest; i++)
		PUSH (Qnil);
	      if (rest)
		PUSH (Qnil);
	    }
	}
      else if (rest)
	{
	  ptrdiff_t i;
	  for (i = 0 ; i < nonrest; i++, args++)
	    PUSH (*args);
	  PUSH (Flist (nargs - nonrest, args));
	}
      else
	/* Too many arguments.  */
	Fsignal (Qwrong_number_of_arguments,
		 list2 (Fcons (make_number (mandatory), make_number (nonrest)),
			make_number (nargs)));
    }
  else if (! NILP (args_template))
    /* We should push some arguments on the stack.  */
    {
      error ("Unknown args template!");
    }

  {
    Lisp_Object (*func)(Lisp_Object *) =
      (Lisp_Object (*)(Lisp_Object *))AREF (byte_code, COMPILED_JIT_ID);
    /* We don't actually need to use this structure to keep track of a
       stack, since our stack isn't GCed.  We just need to use it as a
       placeholder in `byte_stack_list' to facilitate proper unwinding. */
    struct byte_stack stack = {};
    stack.next = byte_stack_list;
    byte_stack_list = &stack;
    Lisp_Object ret = func (top);
    byte_stack_list = byte_stack_list->next;
    return ret;
  }
}

void
jit_byte_code__ (Lisp_Object byte_code)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  int op;
  Lisp_Object *vectorp;
#if BYTE_CODE_SAFE
  ptrdiff_t const_length;
  Lisp_Object *stacke;
#endif
  ptrdiff_t bytestr_length;
  Lisp_Object bytestr;
  Lisp_Object vector;
  Lisp_Object maxdepth;
  Lisp_Object *top;
  enum handlertype type;

  unsigned char *byte_string_start, *pc;

  /* jit-specific variables */
  jit_function_t this_func;
  jit_type_t params[1];
  jit_type_t signature;
  jit_label_t *labels;
  jit_value_t stackv;

  /* ensure this is a byte-coded function _before_ doing anything else */
  CHECK_COMPILED (byte_code);

  /* check if function has already been compiled */
  if (XVECTOR (byte_code)->contents[COMPILED_JIT_ID])
    {
      return;
    }
  else if (!jit_context)
    {
      /* jit is not yet initialized */
      emacs_jit_init ();
    }

  bytestr = XVECTOR (byte_code)->contents[COMPILED_BYTECODE];
  vector = XVECTOR (byte_code)->contents[COMPILED_CONSTANTS];
  maxdepth = XVECTOR (byte_code)->contents[COMPILED_STACK_DEPTH];
  CHECK_STRING (bytestr);
  CHECK_VECTOR (vector);
  CHECK_NATNUM (maxdepth);

#if BYTE_CODE_SAFE
  const_length = ASIZE (vector);
#endif

  if (STRING_MULTIBYTE (bytestr))
    /* BYTESTR must have been produced by Emacs 20.2 or the earlier
       because they produced a raw 8-bit string for byte-code and now
       such a byte-code string is loaded as multibyte while raw 8-bit
       characters converted to multibyte form.  Thus, now we must
       convert them back to the originally intended unibyte form.  */
    bytestr = Fstring_as_unibyte (bytestr);

  bytestr_length = SBYTES (bytestr);
  vectorp = XVECTOR (vector)->contents;

  pc = byte_string_start = SDATA (bytestr);
  if (MAX_ALLOCA / word_size <= XFASTINT (maxdepth))
    memory_full (SIZE_MAX);

  /* prepare for jit */
  jit_context_build_start (jit_context);
  params[0] = jit_type_void_ptr;
  signature = jit_type_create_signature (jit_abi_cdecl, jit_type_nuint, params, 1, 1);
  this_func = jit_function_create (jit_context, signature);
  jit_function_set_optimization_level (this_func,
				       jit_function_get_max_optimization_level ());
  stackv = jit_value_get_param (this_func, 0);
  labels = alloca (sizeof (*labels) * SBYTES (bytestr));
  {
    /* give each instruction a label.  the labels won't be initialized
       until we attach code to them, but they work as a placeholder. */
    int i;
    for (i = 0; i < SBYTES (bytestr); i++)
      labels[i] = jit_label_undefined;
  }

  while (pc < byte_string_start + bytestr_length)
    {
#ifndef BYTE_CODE_THREADED
      op = FETCH;
#endif

      /* The interpreter can be compiled one of two ways: as an
	 ordinary switch-based interpreter, or as a threaded
	 interpreter.  The threaded interpreter relies on GCC's
	 computed goto extension, so it is not available everywhere.
	 Threading provides a performance boost.  These macros are how
	 we allow the code to be compiled both ways.  */
#ifdef BYTE_CODE_THREADED
      /* The CASE macro introduces an instruction's body.  It is
	 either a label or a case label.  */
#define CASE(OP) insn_ ## OP
      /* NEXT is invoked at the end of an instruction to go to the
	 next instruction.  It is either a computed goto, or a
	 plain break.  */
#define NEXT								\
      do {								\
	if (pc >= byte_string_start + bytestr_length)			\
	  goto exit;							\
	else								\
	  {								\
	    /* Create a new block and attach a label to it.  */		\
	    /* Since fetching the instruction incrememnts pc, do  */	\
	    /* this before we fetch the instruction, so pc is right. */	\
	    jit_insn_label (this_func, &labels[JIT_PC]);		\
	    op = FETCH;							\
	    goto *(targets[op]);					\
	  }								\
      } while (0)
      /* FIRST is like NEXT, but is only used at the start of the
	 interpreter body.  In the switch-based interpreter it is the
	 switch, so the threaded definition must include a semicolon.  */
#define FIRST NEXT;
      /* Most cases are labeled with the CASE macro, above.
	 CASE_DEFAULT is one exception; it is used if the interpreter
	 being built requires a default case.  The threaded
	 interpreter does not, because the dispatch table is
	 completely filled.  */
#define CASE_DEFAULT
      /* This introduces an instruction that is known to call abort.  */
#define CASE_ABORT CASE (Bstack_ref): CASE (default)
#else
      /* See above for the meaning of the various defines.  */
#define CASE(OP) case OP
#define NEXT break
#define FIRST switch (op)
#define CASE_DEFAULT case 255: default:
#define CASE_ABORT case 0
#endif

#ifdef BYTE_CODE_THREADED

      /* A convenience define that saves us a lot of typing and makes
	 the table clearer.  */
#define LABEL(OP) [OP] = &&insn_ ## OP

#if 4 < __GNUC__ + (6 <= __GNUC_MINOR__)
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Woverride-init"
#elif defined __clang__
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Winitializer-overrides"
#endif

      /* This is the dispatch table for the threaded interpreter.  */
      static const void *const targets[256] =
	{
	  [0 ... (Bconstant - 1)] = &&insn_default,
	  [Bconstant ... 255] = &&insn_Bconstant,

#define DEFINE(name, value) LABEL (name) ,
	  BYTE_CODES
#undef DEFINE
	};

#if 4 < __GNUC__ + (6 <= __GNUC_MINOR__) || defined __clang__
# pragma GCC diagnostic pop
#endif

#endif

#define JIT_PC (pc - byte_string_start)
#define JIT_NEED_STACK jit_value_ref (this_func, stackv)
#define JIT_NEXT				\
      do {					\
        if (!jit_insn_branch (			\
              this_func,		        \
	      &labels[JIT_PC]))			\
	  emacs_abort ();			\
      } while (0)

#define JIT_INC(v, n)				\
  do {						\
    jit_value_t i =				\
      jit_insn_add_relative (			\
        this_func,		                \
        v,					\
        (jit_nint )n);				\
    if (!i)					\
      emacs_abort ();				\
    else if (!jit_insn_store (			\
                this_func,			\
		v,				\
		i))				\
      emacs_abort ();				\
  } while (0)

#define JIT_PUSH(v)				\
      do {					\
	JIT_INC (stackv, sizeof (Lisp_Object));	\
	if (!jit_insn_store_relative (		\
	      this_func,			\
	      stackv,				\
	      (jit_nint )0, 			\
	      v))				\
	  emacs_abort ();			\
      } while (0)

#define JIT_TOP(v)				\
      do {					\
	v = jit_insn_load_relative (		\
	      this_func,			\
	      stackv,				\
	      (jit_nint )0,			\
	      jit_type_Lisp_Object);		\
	if (!v)					\
	  emacs_abort ();			\
      } while (0)

#define JIT_POP(v)					\
      do {						\
	JIT_TOP (v);					\
	JIT_INC (stackv, -sizeof (Lisp_Object));	\
      } while (0)

#define JIT_CALL(f, args, n)				\
      jit_insn_call_native (				\
	this_func,					\
	#f,						\
	(void*)&f,					\
	f##_sig,					\
	args,						\
	n,						\
	JIT_CALL_NOTHROW)

#define JIT_CALL_ARGS(r, f, ...)			\
      do {						\
	jit_value_t params[] =				\
	  {						\
	    __VA_ARGS__					\
	  };						\
	r = JIT_CALL (					\
	      f,				        \
	      params,					\
	      sizeof (params) / sizeof (params[0]));	\
      } while (0)

#define JIT_CONSTANT(t, v)			\
      jit_value_create_nint_constant (		\
	this_func,				\
	t,					\
	v)

#define JIT_CALL_WITH_STACK_N(f, n)			\
      do {						\
	jit_type_t params[n];				\
	jit_value_t args[n];				\
	jit_value_t ret;				\
	jit_type_t f##_sig;				\
	int i;						\
	for (i = 0; i < n; i++)				\
	  params[i] = jit_type_Lisp_Object;		\
	JIT_SIG_ (f, jit_type_Lisp_Object, params);	\
	JIT_NEED_STACK;					\
	for (i = 1; i <= n; i++)			\
	  JIT_POP (args[n-i]);				\
	ret = JIT_CALL (f, args, n);			\
	JIT_PUSH (ret);					\
      } while (0)

#define JIT_CALL_WITH_STACK_MANY(f, n)				\
      do {							\
	jit_value_t ret;					\
	jit_type_t f##_sig;					\
	JIT_SIG (						\
	  f,						        \
	  jit_type_Lisp_Object,					\
	  jit_type_nuint,					\
	  jit_type_void_ptr);					\
	JIT_NEED_STACK;						\
	JIT_INC (stackv, -(n - 1) * sizeof (Lisp_Object));	\
	JIT_CALL_ARGS (						\
	  ret,					                \
	  f,							\
	  JIT_CONSTANT (jit_type_nuint, n),			\
	  stackv);						\
	JIT_INC (stackv, -sizeof (Lisp_Object));		\
	JIT_PUSH (ret);						\
      } while (0)

#ifndef BYTE_CODE_THREADED
      /* create a new block and attach a label to it */
      jit_insn_label (this_func, &labels[JIT_PC]);
#endif

      FIRST
	{
	CASE (Bvarref7):
	  op = FETCH2;
	  goto varref;

	CASE (Bvarref):
	CASE (Bvarref1):
	CASE (Bvarref2):
	CASE (Bvarref3):
	CASE (Bvarref4):
	CASE (Bvarref5):
	  op = op - Bvarref;
	  goto varref;

	/* This seems to be the most frequently executed byte-code
	   among the Bvarref's, so avoid a goto here.  */
	CASE (Bvarref6):
	  op = FETCH;
	varref:
	  {
	    jit_value_t v1, v2;
	    JIT_NEED_STACK;
	    v1 = JIT_CONSTANT (jit_type_nuint, vectorp[op]);
	    JIT_CALL_ARGS (v2, native_varref, v1);
	    JIT_PUSH (v2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcar):
	  {
	    JIT_CALL_WITH_STACK_N (native_car, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Beq):
	  {
	    JIT_CALL_WITH_STACK_N (native_eq, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bmemq):
	  {
	    JIT_CALL_WITH_STACK_N (native_memq, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcdr):
	  {
	    JIT_CALL_WITH_STACK_N (native_cdr, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bvarset):
	CASE (Bvarset1):
	CASE (Bvarset2):
	CASE (Bvarset3):
	CASE (Bvarset4):
	CASE (Bvarset5):
	  op -= Bvarset;
	  goto varset;

	CASE (Bvarset7):
	  op = FETCH2;
	  goto varset;

	CASE (Bvarset6):
	  op = FETCH;
	varset:
	  {
	    jit_value_t sym, val, x;
	    JIT_NEED_STACK;
	    sym = JIT_CONSTANT (jit_type_Lisp_Object, vectorp[op]);
	    JIT_POP (val);
	    JIT_CALL_ARGS (x, native_varset, sym, val);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bdup):
	  {
	    jit_value_t x;
	    JIT_NEED_STACK;
	    JIT_TOP (x);
	    JIT_PUSH (x);
	    JIT_NEXT;
	    NEXT;
	  }

	/* ------------------ */

	CASE (Bvarbind6):
	  op = FETCH;
	  goto varbind;

	CASE (Bvarbind7):
	  op = FETCH2;
	  goto varbind;

	CASE (Bvarbind):
	CASE (Bvarbind1):
	CASE (Bvarbind2):
	CASE (Bvarbind3):
	CASE (Bvarbind4):
	CASE (Bvarbind5):
	  op -= Bvarbind;
	varbind:
	  {
	    jit_value_t v1, v2, x;
	    JIT_NEED_STACK;
	    v1 = JIT_CONSTANT (jit_type_Lisp_Object, vectorp[op]);
	    JIT_POP (v2);
	    JIT_CALL_ARGS (x, specbind, v1, v2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcall6):
	  op = FETCH;
	  goto docall;

	CASE (Bcall7):
	  op = FETCH2;
	  goto docall;

	CASE (Bcall):
	CASE (Bcall1):
	CASE (Bcall2):
	CASE (Bcall3):
	CASE (Bcall4):
	CASE (Bcall5):
	  op -= Bcall;
	docall:
	  {
	    JIT_NEED_STACK;
	    JIT_CALL_WITH_STACK_MANY (Ffuncall, op + 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bunbind6):
	  op = FETCH;
	  goto dounbind;

	CASE (Bunbind7):
	  op = FETCH2;
	  goto dounbind;

	CASE (Bunbind):
	CASE (Bunbind1):
	CASE (Bunbind2):
	CASE (Bunbind3):
	CASE (Bunbind4):
	CASE (Bunbind5):
	  op -= Bunbind;
	dounbind:
	  {
	    jit_value_t args[] =
	      {
		JIT_CONSTANT (jit_type_nuint, op),
		JIT_CONSTANT (jit_type_Lisp_Object, Qnil)
	      };
	    JIT_CALL (native_unbind_to, args, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bunbind_all):	/* Obsolete.  Never used.  */
	  /* To unbind back to the beginning of this frame.  Not used yet,
	     but will be needed for tail-recursion elimination.  */
	  {
	    jit_value_t args[] =
	      {
		JIT_CONSTANT (jit_type_nuint, count),
		JIT_CONSTANT (jit_type_Lisp_Object, Qnil)
	      };
	    JIT_CALL (unbind_to, args, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bgoto):
	  {
	    jit_value_t v;
	    op = FETCH2;
	    CHECK_RANGE (op);
	    JIT_CALL (byte_code_quit, NULL, 0);
	    jit_insn_branch (
	      this_func,
	      &labels[op]);
	    NEXT;
	  }

	CASE (Bgotoifnil):
	CASE (BRgotoifnil):
	CASE (Bgotoifnonnil):
	CASE (BRgotoifnonnil):
	CASE (Bgotoifnilelsepop):
	CASE (BRgotoifnilelsepop):
	CASE (Bgotoifnonnilelsepop):
	CASE (BRgotoifnonnilelsepop):
	  {
	    jit_value_t v2, v3;
	    int insn = op;
	    if (insn >= Bgotoifnil && insn <= Bgotoifnonnilelsepop)
	      op = FETCH2;
	    else
	      {
		op = FETCH - 128;
		op += (pc - byte_string_start);
	      }
	    CHECK_RANGE (op);
	    JIT_NEED_STACK;
	    JIT_POP (v2);
	    if (insn == Bgotoifnil || insn == BRgotoifnil
		|| insn == Bgotoifnilelsepop || insn == BRgotoifnilelsepop)
	      JIT_CALL_ARGS (v3, native_ifnil, v2);
	    else
	      JIT_CALL_ARGS (v3, native_ifnonnil, v2);
	    if (insn == Bgotoifnilelsepop || insn == Bgotoifnonnilelsepop
		|| insn == BRgotoifnilelsepop || insn == BRgotoifnonnilelsepop)
	      JIT_PUSH (v2);
	    jit_insn_branch_if (
	      this_func,
	      v3,
	      &labels[op]);
	    if (insn == Bgotoifnilelsepop || insn == Bgotoifnonnilelsepop
		|| insn == BRgotoifnilelsepop || insn == BRgotoifnonnilelsepop)
	      JIT_INC (stackv, -sizeof (Lisp_Object));
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (BRgoto):
	  {
	    op = FETCH - 128;
	    const int dest = (pc - byte_string_start) + op;
	    JIT_CALL (byte_code_quit, NULL, 0);
	    jit_insn_branch (
	      this_func,
	      &labels[dest]);
	    NEXT;
	  }

	CASE (Breturn):
	  {
	    jit_value_t v;
	    JIT_NEED_STACK;
	    JIT_POP (v);
	    jit_insn_return (this_func, v);
	    NEXT;
	  }

	CASE (Bdiscard):
	  {
	    JIT_NEED_STACK;
	    JIT_INC (stackv, -sizeof (Lisp_Object));
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bconstant2):
	  {
	    jit_value_t v = JIT_CONSTANT (jit_type_Lisp_Object, vectorp[FETCH2]);
	    JIT_NEED_STACK;
	    JIT_PUSH (v);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsave_excursion):
	  {
	    JIT_CALL (native_save_excursion, NULL, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsave_current_buffer): /* Obsolete since ??.  */
	CASE (Bsave_current_buffer_1):
	  {
	    jit_type_t record_unwind_current_buffer_sig;
	    JIT_SIG (record_unwind_current_buffer, jit_type_void);
	    JIT_CALL (record_unwind_current_buffer, NULL, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsave_window_excursion): /* Obsolete since 24.1.  */
	  {
	    JIT_CALL_WITH_STACK_N (native_save_window_excursion, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsave_restriction):
	  JIT_CALL (native_save_restriction, NULL, 0);
	  JIT_NEXT;
	  NEXT;

	CASE (Bcatch):		/* Obsolete since 24.4.  */
	  {
	    JIT_CALL_WITH_STACK_N (native_catch, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bpushcatch):	/* New in 24.4.  */
	  type = CATCHER;
	  goto pushhandler;
	CASE (Bpushconditioncase): /* New in 24.4.  */
	  type = CONDITION_CASE;
	pushhandler:
	  {
	    jit_label_t new_label;
	    jit_value_t tag, stackp, jmp, result, result2, typev;
	    int dest = FETCH2;
	    JIT_NEED_STACK;
	    JIT_POP (tag);
	    stackp = jit_insn_address_of (this_func, stackv);
	    typev = JIT_CONSTANT (jit_type_nint, type);
	    JIT_CALL_ARGS (jmp, native_pushhandler1, stackp, tag, typev);
	    do {
	      void *f;
	      int n;
	      jit_value_t args[2] = { jmp };
#ifdef HAVE__SETJMP
	      f = (void *)&_setjmp;
	      n = 1;
#elif defined HAVE_SIGSETJMP
	      f = (void *)&sigsetjmp;
	      n = 2;
	      args[1] = JIT_CONSTANT (jit_type_sys_int, 0);
#else
	      f = (void *)&setjmp;
	      n = 1;
#endif
	      result = jit_insn_call_native (this_func, "setjmp", f,
					     setjmp_sig, args, n,
					     JIT_CALL_NOTHROW);
	    } while (0);
	    jit_insn_branch_if_not (this_func, result, &labels[JIT_PC]);
	    JIT_CALL (native_pushhandler2, &stackp, 1);
	    jit_insn_branch (this_func, &labels[dest]);
	    NEXT;
	  }

	CASE (Bpophandler):	/* New in 24.4.  */
	  {
	    JIT_CALL (native_pophandler, NULL, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bunwind_protect):	/* FIXME: avoid closure for lexbind.  */
	  {
	    jit_value_t handler;
	    JIT_NEED_STACK;
	    JIT_POP (handler);
	    JIT_CALL (native_unwind_protect, &handler, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcondition_case):		/* Obsolete since 24.4.  */
	  {
	    JIT_CALL_WITH_STACK_N (internal_lisp_condition_case, 3);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Btemp_output_buffer_setup): /* Obsolete since 24.1.  */
	  {
	    JIT_NEED_STACK;
	    JIT_CALL_WITH_STACK_N (native_temp_output_buffer_setup, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Btemp_output_buffer_show): /* Obsolete since 24.1.  */
	  {
	    jit_type_t temp_output_buffer_show_sig;
	    jit_value_t v1, v2, c, q, x;
	    JIT_NEED_STACK;
	    JIT_SIG (temp_output_buffer_show,
		     jit_type_void,
		     jit_type_Lisp_Object);
	    JIT_POP (v1);
	    JIT_POP (v2);
	    JIT_CALL (temp_output_buffer_show, &v2, 1);
	    JIT_PUSH (v1);
	    c = JIT_CONSTANT (jit_type_nuint, 1);
	    q = JIT_CONSTANT (jit_type_Lisp_Object, Qnil);
	    JIT_CALL_ARGS (x, native_unbind_to, c, q);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bnth):
	  {
	    JIT_CALL_WITH_STACK_N (native_nth, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsymbolp):
	CASE (Bconsp):
	CASE (Bstringp):
	CASE (Blistp):
	CASE (Bnot):
	  {
	    jit_value_t v1, v2;
	    JIT_NEED_STACK;
	    JIT_POP (v1);
	    switch (op)
	      {
	      case Bsymbolp:
		JIT_CALL_ARGS (v2, native_symbolp, v1);
		break;
	      case Bconsp:
		JIT_CALL_ARGS (v2, native_consp, v1);
		break;
	      case Bstringp:
		JIT_CALL_ARGS (v2, native_stringp, v1);
		break;
	      case Blistp:
		JIT_CALL_ARGS (v2, native_listp, v1);
		break;
	      case Bnot:
	      default:
		JIT_CALL_ARGS (v2, native_not, v1);
		break;
	      }
	    JIT_PUSH (v2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcons):
	  {
	    JIT_CALL_WITH_STACK_N (Fcons, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Blist1):
	  {
	    JIT_CALL_WITH_STACK_N (list1, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Blist2):
	  {
	    JIT_CALL_WITH_STACK_N (list2, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Blist3):
	CASE (Blist4):
	CASE (BlistN):
	  {
	    size_t temp;
	    if (op == BlistN)
	      {
		temp = FETCH;
	      }
	    else
	      {
		if (op == Blist3)
		  temp = 3;
		else
		  temp = 4;
	      }
	    JIT_CALL_WITH_STACK_MANY (Flist, temp);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Blength):
	  {
	    JIT_CALL_WITH_STACK_N (Flength, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Baref):
	  {
	    JIT_CALL_WITH_STACK_N (Faref, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Baset):
	  {
	    JIT_CALL_WITH_STACK_N (Faset, 3);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsymbol_value):
	  {
	    JIT_CALL_WITH_STACK_N (Fsymbol_value, 1);
	    JIT_NEXT;
	    NEXT;
	  }
	CASE (Bsymbol_function):
	  {
	    JIT_CALL_WITH_STACK_N (Fsymbol_function, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bset):
	  {
	    JIT_CALL_WITH_STACK_N (Fset, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bfset):
	  {
	    JIT_CALL_WITH_STACK_N (Ffset, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bget):
	  {
	    JIT_CALL_WITH_STACK_N (Fget, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsubstring):
	  {
	    JIT_CALL_WITH_STACK_N (Fsubstring, 3);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bconcat2):
	CASE (Bconcat3):
	CASE (Bconcat4):
	CASE (BconcatN):
	  {
	    size_t n;
	    if (op == BconcatN)
	      n = FETCH;
	    else
	      n = op - Bconcat2 + 2;
	    JIT_CALL_WITH_STACK_MANY (Fconcat, n);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsub1):
	  {
	    jit_value_t v1, v2;
	    JIT_NEED_STACK;
	    JIT_POP (v1);
	    JIT_CALL_ARGS (v2, native_add1, v1, JIT_CONSTANT (jit_type_sys_bool, 0));
	    JIT_PUSH (v2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Badd1):
	  {
	    jit_value_t v1, v2;
	    JIT_NEED_STACK;
	    JIT_POP (v1);
	    JIT_CALL_ARGS (v2, native_add1, v1, JIT_CONSTANT (jit_type_sys_bool, 1));
	    JIT_PUSH (v2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Beqlsign):
	  {
	    JIT_CALL_WITH_STACK_N (native_eqlsign, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bgtr):
	CASE (Blss):
	CASE (Bleq):
	CASE (Bgeq):
	  {
	    jit_value_t v1, v2, v3, c;
	    enum Arith_Comparison v[] =
	      {
		ARITH_GRTR,
		ARITH_LESS,
		ARITH_LESS_OR_EQUAL,
		ARITH_GRTR_OR_EQUAL
	      };
	    JIT_NEED_STACK;
	    c = JIT_CONSTANT (jit_type_nuint, v[op-Bgtr]);
	    JIT_POP (v2);
	    JIT_POP (v1);
	    JIT_CALL_ARGS (v3, arithcompare, v1, v2, c);
	    JIT_PUSH (v3);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bdiff):
	  {
	    JIT_CALL_WITH_STACK_MANY (Fminus, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bnegate):
	  {
	    JIT_CALL_WITH_STACK_N (native_negate, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bplus):
	  {
	    JIT_CALL_WITH_STACK_MANY (Fplus, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bmax):
	  {
	    JIT_CALL_WITH_STACK_MANY (Fmax, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bmin):
	  {
	    JIT_CALL_WITH_STACK_MANY (Fmin, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bmult):
	  {
	    JIT_CALL_WITH_STACK_MANY (Ftimes, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bquo):
	  {
	    JIT_CALL_WITH_STACK_MANY (Fquo, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Brem):
	  {
	    JIT_CALL_WITH_STACK_N (Frem, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bpoint):
	  {
	    JIT_CALL_WITH_STACK_N (native_point, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bgoto_char):
	  {
	    JIT_CALL_WITH_STACK_N (Fgoto_char, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Binsert):
	  {
	    JIT_CALL_WITH_STACK_MANY (Finsert, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (BinsertN):
	  {
	    /* FETCH must not appear in the macro reference below, otherwise
	       the macro expansion will contain multiple instances OF FETCH */
	    Lisp_Object n = FETCH;
	    JIT_CALL_WITH_STACK_MANY (Finsert, n);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bpoint_max):
	  {
	    JIT_CALL_WITH_STACK_N (native_point_max, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bpoint_min):
	  {
	    JIT_CALL_WITH_STACK_N (native_point_min, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bchar_after):
	  {
	    JIT_CALL_WITH_STACK_N (Fchar_after, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bfollowing_char):
	  {
	    JIT_CALL_WITH_STACK_N (Ffollowing_char, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bpreceding_char):
	  {
	    JIT_CALL_WITH_STACK_N (Fprevious_char, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcurrent_column):
	  {
	    JIT_CALL_WITH_STACK_N (native_current_column, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bindent_to):
	  {
	    JIT_PUSH (JIT_CONSTANT (jit_type_Lisp_Object, Qnil));
	    JIT_CALL_WITH_STACK_N (Findent_to, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Beolp):
	  {
	    JIT_CALL_WITH_STACK_N (Feolp, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Beobp):
	  {
	    JIT_CALL_WITH_STACK_N (Feobp, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bbolp):
	  {
	    JIT_CALL_WITH_STACK_N (Fbolp, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bbobp):
	  {
	    JIT_CALL_WITH_STACK_N (Fbobp, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcurrent_buffer):
	  {
	    JIT_CALL_WITH_STACK_N (Fcurrent_buffer, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bset_buffer):
	  {
	    JIT_CALL_WITH_STACK_N (Fset_buffer, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Binteractive_p):	/* Obsolete since 24.1.  */
	  {
	    JIT_CALL_WITH_STACK_N (native_interactive_p, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bforward_char):
	  {
	    JIT_CALL_WITH_STACK_N (Fforward_char, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bforward_word):
	  {
	    JIT_CALL_WITH_STACK_N (Fforward_word, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bskip_chars_forward):
	  {
	    JIT_CALL_WITH_STACK_N (Fskip_chars_forward, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bskip_chars_backward):
	  {
	    JIT_CALL_WITH_STACK_N (Fskip_chars_backward, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bforward_line):
	  {
	    JIT_CALL_WITH_STACK_N (Fforward_line, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bchar_syntax):
	  {
	    JIT_CALL_WITH_STACK_N (native_char_syntax, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bbuffer_substring):
	  {
	    JIT_CALL_WITH_STACK_N (Fbuffer_substring, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bdelete_region):
	  {
	    JIT_CALL_WITH_STACK_N (Fdelete_region, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bnarrow_to_region):
	  {
	    JIT_CALL_WITH_STACK_N (Fnarrow_to_region, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bwiden):
	  {
	    JIT_CALL_WITH_STACK_N (Fwiden, 0);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bend_of_line):
	  {
	    JIT_CALL_WITH_STACK_N (Fend_of_line, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bset_marker):
	  {
	    JIT_CALL_WITH_STACK_N (Fset_marker, 3);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bmatch_beginning):
	  {
	    JIT_CALL_WITH_STACK_N (Fmatch_beginning, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bmatch_end):
	  {
	    JIT_CALL_WITH_STACK_N (Fmatch_end, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bupcase):
	  {
	    JIT_CALL_WITH_STACK_N (Fupcase, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bdowncase):
	  {
	    JIT_CALL_WITH_STACK_N (Fdowncase, 1);
	    JIT_NEXT;
	    NEXT;
	  }

      CASE (Bstringeqlsign):
	  {
	    JIT_CALL_WITH_STACK_N (Fstring_equal, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bstringlss):
	  {
	    JIT_CALL_WITH_STACK_N (Fstring_lessp, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bequal):
	  {
	    JIT_CALL_WITH_STACK_N (Fequal, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bnthcdr):
	  {
	    JIT_CALL_WITH_STACK_N (Fnthcdr, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Belt):
	  {
	    JIT_CALL_WITH_STACK_N (native_elt, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bmember):
	  {
	    JIT_CALL_WITH_STACK_N (Fmember, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bassq):
	  {
	    JIT_CALL_WITH_STACK_N (Fassq, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bnreverse):
	  {
	    JIT_CALL_WITH_STACK_N (Fnreverse, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsetcar):
	  {
	    JIT_CALL_WITH_STACK_N (Fsetcar, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsetcdr):
	  {
	    JIT_CALL_WITH_STACK_N (Fsetcdr, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcar_safe):
	  {
	    JIT_CALL_WITH_STACK_N (native_car_safe, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bcdr_safe):
	  {
	    JIT_CALL_WITH_STACK_N (native_cdr_safe, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bnconc):
	  {
	    JIT_CALL_WITH_STACK_MANY (Fnconc, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bnumberp):
	  {
	    JIT_CALL_WITH_STACK_N (native_number_p, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bintegerp):
	  {
	    JIT_CALL_WITH_STACK_N (native_integer_p, 1);
	    JIT_NEXT;
	    NEXT;
	  }

#if BYTE_CODE_SAFE
	  /* These are intentionally written using 'case' syntax,
	     because they are incompatible with the threaded
	     interpreter.  */

	case Bset_mark:
	  error ("set-mark is an obsolete bytecode");
	  break;
	case Bscan_buffer:
	  error ("scan-buffer is an obsolete bytecode");
	  break;
#endif

	CASE_ABORT:
	  /* Actually this is Bstack_ref with offset 0, but we use Bdup
	     for that instead.  */
	  /* CASE (Bstack_ref): */
	  call3 (Qerror,
		 build_string ("Invalid byte opcode: op=%s, ptr=%d"),
		 make_number (op),
		 make_number (pc - 1 - byte_string_start));

	  /* Handy byte-codes for lexical binding.  */
	CASE (Bstack_ref1):
	CASE (Bstack_ref2):
	CASE (Bstack_ref3):
	CASE (Bstack_ref4):
	CASE (Bstack_ref5):
	CASE (Bstack_ref6):
	CASE (Bstack_ref7):
	  {
	    jit_value_t v1;
	    int offs = op - Bstack_ref;
	    if (offs == 6)
	      offs = FETCH;
	    else if (offs == 7)
	      offs = FETCH2;

	    JIT_NEED_STACK;
	    JIT_INC (stackv, -offs * sizeof (Lisp_Object));
	    JIT_TOP (v1);
	    JIT_INC (stackv, offs * sizeof (Lisp_Object));
	    JIT_PUSH (v1);
	    JIT_NEXT;
	    NEXT;
	  }
	CASE (Bstack_set):
	CASE (Bstack_set2):
	  /* stack-set-0 = discard; stack-set-1 = discard-1-preserve-tos.  */
	  {
	    jit_value_t v1;
	    int offs = (op == Bstack_set) ? FETCH : FETCH2;
	    JIT_NEED_STACK;
	    JIT_TOP (v1);
	    if (offs != 0)
	      JIT_INC (stackv, -(offs + 1) * sizeof (Lisp_Object));
	    JIT_PUSH (v1);
	    JIT_INC (stackv, (offs - 1) * sizeof (Lisp_Object));
	    JIT_NEXT;
	    NEXT;
	  }
	CASE (BdiscardN):
	  {
	    op = FETCH;
	    JIT_NEED_STACK;
	    if (op & 0x80)
	      {
		jit_value_t v1;
		op &= 0x7F;
		JIT_TOP (v1);
		JIT_INC (stackv, -(op + 1) * sizeof (Lisp_Object));
		JIT_PUSH (v1);
	      }
	    else
	      JIT_INC (stackv, -op * sizeof (Lisp_Object));
	    JIT_NEXT;
	    NEXT;
	  }

	CASE_DEFAULT
	CASE (Bconstant):
	  {
	    jit_value_t c;
#if BYTE_CODE_SAFE
	    if (op < Bconstant)
	      {
		emacs_abort ();
	      }
	    if ((op -= Bconstant) >= const_length)
	      {
		emacs_abort ();
	      }
#endif
	    c = JIT_CONSTANT (jit_type_Lisp_Object, vectorp[op - Bconstant]);
	    JIT_PUSH (c);
	    JIT_NEXT;
	    NEXT;
	  }
	}
    }

 exit:

  {
    int err = !jit_function_compile (this_func);
    jit_context_build_end (jit_context);
    if (err)
      emacs_abort ();
    ASET (byte_code, COMPILED_JIT_ID, (Lisp_Object )jit_function_to_closure (this_func));
  }
}

DEFUN ("jit-compile", Fjit_compile, Sjit_compile, 1, 1, 0,
       doc: /* Function used internally in byte-compiled code.
	       The first argument, BYTECODE, is a compiled byte code object. */)
  (Lisp_Object byte_code)
{
  jit_byte_code__ (byte_code);
  return byte_code;
}

void
syms_of_bytecode_jit (void)
{
  defsubr (&Sjit_compile);
  DEFVAR_BOOL ("byte-code-jit-on", byte_code_jit_on,
	       doc: /* If non-nil, compile byte-code to machine code
		       before execution. */);
  byte_code_jit_on = 0;
}
#endif  /* HAVE_LIBJIT */
