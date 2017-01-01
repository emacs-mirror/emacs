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

#include <stdarg.h>
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

struct emacs_jit_context
{
  jit_context_t libjit_ctxt;
  jit_function_t func;
  jit_type_t stack_many;
  jit_type_t stack_n[4];
  jit_value_t stack;
};

static bool jit_initialized = false;

static void
emacs_jit_start (struct emacs_jit_context *ctxt)
{
#define JIT_SIG_(f, ret, params)					\
  f = jit_type_create_signature (jit_abi_cdecl, ret, params,		\
				 sizeof (params) / sizeof (params[0]), 1);
#define JIT_SIG(f, ret, ...)			\
  do {						\
    jit_type_t params[] =			\
      {						\
        __VA_ARGS__				\
      };					\
    JIT_SIG_ (f, ret, params);			\
  } while (0)

  ctxt->libjit_ctxt = jit_context_create ();
  jit_type_t jit_type_Lisp_Object_ptr =
    jit_type_create_pointer (jit_type_Lisp_Object, 1);
  jit_type_t func_sig;
  JIT_SIG (func_sig, jit_type_Lisp_Object, jit_type_Lisp_Object_ptr);
  ctxt->func = jit_function_create (ctxt->libjit_ctxt, func_sig);
  jit_function_set_optimization_level (ctxt->func,
				       jit_function_get_max_optimization_level ());
  ctxt->stack = jit_value_get_param (ctxt->func, 0);

  JIT_SIG (ctxt->stack_many, jit_type_Lisp_Object,
	   jit_type_nuint, jit_type_Lisp_Object_ptr);

  jit_type_t params[] = {
    jit_type_Lisp_Object,
    jit_type_Lisp_Object,
    jit_type_Lisp_Object,
    jit_type_Lisp_Object
  };
  int i;
  for (i = 0; i < sizeof (params) / sizeof (params[0]); i++)
    {
      ctxt->stack_n[i] =
	jit_type_create_signature (jit_abi_cdecl, jit_type_Lisp_Object,
				   params, i, 1);
    }
#undef JIT_SIG
#undef JIT_SIG_
}

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
  JIT_SIG (native_ifnil, jit_type_sys_bool, jit_type_Lisp_Object);
  JIT_SIG (native_ifnonnil, jit_type_sys_bool, jit_type_Lisp_Object);
  JIT_SIG (native_varset, jit_type_void, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (specbind, jit_type_void, jit_type_Lisp_Object, jit_type_Lisp_Object);
  JIT_SIG (Ffuncall, jit_type_Lisp_Object, jit_type_nuint, jit_type_void_ptr);
  JIT_SIG (byte_code_quit, jit_type_void);
  JIT_SIG (native_save_excursion, jit_type_void);
  JIT_SIG (native_save_restriction, jit_type_void);
  JIT_SIG (native_pophandler, jit_type_void);
  JIT_SIG (native_pushhandler1, jit_type_void_ptr, jit_type_create_pointer (jit_type_void_ptr, 1), jit_type_Lisp_Object, jit_type_nint);
  JIT_SIG (native_pushhandler2, jit_type_void, jit_type_create_pointer (jit_type_void_ptr, 1));
  JIT_SIG (native_unwind_protect, jit_type_void, jit_type_Lisp_Object);
  JIT_SIG (native_add1, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_sys_bool);
  JIT_SIG (arithcompare, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_Lisp_Object, jit_type_nuint);

  jit_initialized = true;
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
    /* We don't actually need to use this structure to keep track of a
       stack, since our stack isn't GCed.  We just need to use it as a
       placeholder in `byte_stack_list' to facilitate proper unwinding. */
    struct byte_stack stack = {};
    stack.next = byte_stack_list;
    byte_stack_list = &stack;
    Lisp_Object (*func)(Lisp_Object *) =
      (Lisp_Object (*)(Lisp_Object *))jit_function_to_closure ((void *)AREF (byte_code, COMPILED_JIT_ID));
    Lisp_Object ret = func (top);
    byte_stack_list = byte_stack_list->next;
    return ret;
  }
}

static inline
void jit_inc (struct emacs_jit_context *ctxt, jit_value_t v, long n)
{
  jit_value_t i = jit_insn_add_relative (ctxt->func, v, (jit_nint )n);
  if (!i || !jit_insn_store (ctxt->func, v, i))
    emacs_abort ();
}

static inline
void jit_push (struct emacs_jit_context *ctxt, jit_value_t v)
{
  jit_inc (ctxt, ctxt->stack, sizeof (Lisp_Object));
  if (!jit_insn_store_relative (ctxt->func, ctxt->stack, (jit_nint )0, v))
    emacs_abort ();
}

static inline
jit_value_t jit_top (struct emacs_jit_context *ctxt)
{
  jit_value_t v = jit_insn_load_relative (ctxt->func, ctxt->stack,
					  (jit_nint )0, jit_type_Lisp_Object);
  if (!v)
    emacs_abort ();
  return v;
}

static inline
jit_value_t jit_pop (struct emacs_jit_context *ctxt)
{
  jit_value_t v = jit_top (ctxt);
  jit_inc (ctxt, ctxt->stack, -sizeof (Lisp_Object));
  return v;
}

static inline
jit_value_t jit_call (struct emacs_jit_context *ctxt, void *f,
		      const char *name, jit_type_t sig, jit_value_t *args,
		      size_t nargs)
{
  return jit_insn_call_native (ctxt->func, name, f, sig, args, nargs,
			       JIT_CALL_NOTHROW);
}

static inline
jit_value_t jit_call_vaarg (struct emacs_jit_context *ctxt, void *f,
			    const char *name, jit_type_t sig, ...)
{
  jit_value_t *args;
  int i, count;
  va_list ap;

  /* Determine the number of passed arguments. */
  va_start (ap, sig);
  for (count = 0; va_arg (ap, jit_value_t) != NULL; count++);
  va_end (ap);

  /* Collect args and setup the call */
  args = (count > 0) ? alloca (count * sizeof (*args)) : NULL;
  va_start (ap, sig);
  for (i = 0; i < count; i++)
    args[i] = va_arg (ap, jit_value_t);
  va_end (ap);

  return jit_call (ctxt, f, name, sig, args, count);
}

static inline
void jit_call_with_stack_n (struct emacs_jit_context *ctxt, void *f,
			    const char *name, int n)
{
  jit_value_t *args = (n > 0) ? alloca (n * sizeof (*args)) : NULL;
  int i;

  for (i = 1; i <= n; i++)
    args[n-i] = jit_pop (ctxt);
  jit_push (ctxt, jit_call (ctxt, f, name, ctxt->stack_n[n], args, n));
}

#define JIT_CONSTANT(f, t, v)			\
  jit_value_create_nint_constant (f, t, v)

static inline
void jit_call_with_stack_many (struct emacs_jit_context *ctxt, void *f,
			       const char *name, int n)
{
  jit_inc (ctxt, ctxt->stack, -(n - 1) * sizeof (Lisp_Object));
  jit_insn_store_relative (ctxt->func, ctxt->stack, (jit_nint )0,
			   jit_call_vaarg (ctxt, f, name, ctxt->stack_many,
					   JIT_CONSTANT (ctxt->func,
							 jit_type_nuint, n),
					   ctxt->stack, NULL));
}

#undef JIT_CONSTANT

struct {
  void * const ptr;
  const char * const name;
  int n;
} static const functions[256] = {
#undef DEFINE_FIXED
#define DEFINE_FIXED(bname, value, fname, num) \
  [value] = { .ptr = (void *)(&fname), .name = #fname, .n = num },
#define DEFINE(bname, value)

  BYTE_CODES

#undef DEFINE_FIXED
#undef DEFINE
#define DEFINE_FIXED(bname, value, fname, num) DEFINE (bname, value)
};

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
  enum handlertype type;

  unsigned char *byte_string_start, *pc;

  /* jit-specific variables */
  struct emacs_jit_context ctxt;
  jit_label_t *labels;

  /* ensure this is a byte-coded function _before_ doing anything else */
  CHECK_COMPILED (byte_code);

  /* check if function has already been compiled */
  if (XVECTOR (byte_code)->contents[COMPILED_JIT_ID])
    return;
  if (!jit_initialized)
    emacs_jit_init ();

  emacs_jit_start (&ctxt);

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
  jit_context_build_start (ctxt.libjit_ctxt);
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
	    jit_insn_label (ctxt.func, &labels[JIT_PC]);		\
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
#define JIT_NEED_STACK jit_value_ref (ctxt.func, ctxt.stack)
#define JIT_NEXT				\
      do {					\
        if (!jit_insn_branch (			\
              ctxt.func,		        \
	      &labels[JIT_PC]))			\
	  emacs_abort ();			\
      } while (0)

#define JIT_INC(v, n)				\
      jit_inc (&ctxt, v, n)

#define JIT_PUSH(v)				\
      jit_push (&ctxt, v)

#define JIT_TOP()				\
      jit_top (&ctxt)

#define JIT_POP()					\
      jit_pop (&ctxt)

#define JIT_CALL(f, args, n)				\
      jit_call (&ctxt, (void *)&f, #f, f##_sig, args, n)

#define JIT_CALL_ARGS(f, ...)					\
      jit_call_vaarg (&ctxt, (void *)&f, #f, f##_sig, __VA_ARGS__, NULL)

#define JIT_CONSTANT(t, v)			\
      jit_value_create_nint_constant (		\
	ctxt.func,				\
	t,					\
	v)

#define JIT_CALL_WITH_STACK_N(f, n)			\
      jit_call_with_stack_n (&ctxt, (void *)&f, #f, n)

#define JIT_CALL_WITH_STACK_MANY(f, n)				\
      jit_call_with_stack_many (&ctxt, (void *)&f, #f, n)

#ifndef BYTE_CODE_THREADED
      /* create a new block and attach a label to it */
      jit_insn_label (ctxt.func, &labels[JIT_PC]);
#endif

      FIRST
	{
	CASE (Bcall):
	CASE (Bcall1):
	CASE (Bcall2):
	CASE (Bcall3):
	CASE (Bcall4):
	CASE (Bcall5):
	CASE (Bcall6):
	CASE (Bcall7):
	CASE (Blist3):
	CASE (Blist4):
	CASE (BlistN):
	CASE (Bconcat2):
	CASE (Bconcat3):
	CASE (Bconcat4):
	CASE (BconcatN):
	CASE (Bdiff):
	CASE (Bplus):
	CASE (Bmax):
	CASE (Bmin):
	CASE (Bmult):
	CASE (Bquo):
	CASE (Binsert):
	CASE (BinsertN):
	CASE (Bnconc):
	  {
	    int args = functions[op].n;
	    if (args < 0)
	      {
		if (args == -1)
		  args = FETCH;
		else if (args == -2)
		  args = FETCH2;
		if (op == Bcall6 || op == Bcall7)
		  args += 1;
	      }
	    JIT_NEED_STACK;
	    jit_call_with_stack_many (&ctxt, functions[op].ptr,
				      functions[op].name, args);
	    JIT_NEXT;
	    NEXT;
	  }
	CASE (Blist1):
	CASE (Blist2):
	CASE (Bcar):
	CASE (Beq):
	CASE (Bmemq):
	CASE (Bcdr):
	CASE (Bsave_window_excursion): /* Obsolete since 24.1.  */
	CASE (Bcatch):		/* Obsolete since 24.4.  */
	CASE (Bcondition_case):		/* Obsolete since 24.4.  */
	CASE (Btemp_output_buffer_setup): /* Obsolete since 24.1.  */
	CASE (Bnth):
	CASE (Bsymbolp):
	CASE (Bconsp):
	CASE (Bstringp):
	CASE (Blistp):
	CASE (Bnot):
	CASE (Bcons):
	CASE (Blength):
	CASE (Baref):
	CASE (Baset):
	CASE (Bsymbol_value):
	CASE (Bsymbol_function):
	CASE (Bset):
	CASE (Bfset):
	CASE (Bget):
	CASE (Bsubstring):
	CASE (Beqlsign):
	CASE (Bnegate):
	CASE (Brem):
	CASE (Bpoint):
	CASE (Bgoto_char):
	CASE (Bpoint_max):
	CASE (Bpoint_min):
	CASE (Bchar_after):
	CASE (Bfollowing_char):
	CASE (Bpreceding_char):
	CASE (Bcurrent_column):
	CASE (Beolp):
	CASE (Beobp):
	CASE (Bbolp):
	CASE (Bbobp):
	CASE (Bcurrent_buffer):
	CASE (Bset_buffer):
	CASE (Binteractive_p):	/* Obsolete since 24.1.  */
	CASE (Bforward_char):
	CASE (Bforward_word):
	CASE (Bskip_chars_forward):
	CASE (Bskip_chars_backward):
	CASE (Bforward_line):
	CASE (Bchar_syntax):
	CASE (Bbuffer_substring):
	CASE (Bdelete_region):
	CASE (Bnarrow_to_region):
	CASE (Bwiden):
	CASE (Bend_of_line):
	CASE (Bset_marker):
	CASE (Bmatch_beginning):
	CASE (Bmatch_end):
	CASE (Bupcase):
	CASE (Bdowncase):
	CASE (Bstringeqlsign):
	CASE (Bstringlss):
	CASE (Bequal):
	CASE (Bnthcdr):
	CASE (Belt):
	CASE (Bmember):
	CASE (Bassq):
	CASE (Bnreverse):
	CASE (Bsetcar):
	CASE (Bsetcdr):
	CASE (Bcar_safe):
	CASE (Bcdr_safe):
	CASE (Bnumberp):
	CASE (Bintegerp):
	  {
	    JIT_NEED_STACK;
	    jit_call_with_stack_n (&ctxt, functions[op].ptr,
				   functions[op].name, functions[op].n);
	    JIT_NEXT;
	    NEXT;
	  }

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
	    JIT_NEED_STACK;
	    JIT_PUSH (JIT_CONSTANT (jit_type_nuint, vectorp[op]));
	    JIT_CALL_WITH_STACK_N (native_varref, 1);
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
	    JIT_NEED_STACK;
	    JIT_CALL_ARGS (native_varset, JIT_CONSTANT (jit_type_Lisp_Object,
							vectorp[op]),
			   JIT_POP ());

	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bdup):
	  {
	    JIT_NEED_STACK;
	    JIT_PUSH (JIT_TOP ());
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
	    JIT_NEED_STACK;
	    JIT_CALL_ARGS (specbind,
			   JIT_CONSTANT (jit_type_Lisp_Object,
					 vectorp[op]),
			   JIT_POP ());
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
	    jit_call (&ctxt, (void *)&native_unbind_to, "native_unbind_to",
		      ctxt.stack_many, args, 2);
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
	    jit_call (&ctxt, (void *)&unbind_to, "unbind_to",
		      ctxt.stack_many, args, 2);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bgoto):
	  {
	    op = FETCH2;
	    CHECK_RANGE (op);
	    JIT_CALL (byte_code_quit, NULL, 0);
	    jit_insn_branch (
	      ctxt.func,
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
	    v2 = JIT_POP ();
	    if (insn == Bgotoifnil || insn == BRgotoifnil
		|| insn == Bgotoifnilelsepop || insn == BRgotoifnilelsepop)
	      v3 = JIT_CALL_ARGS (native_ifnil, v2);
	    else
	      v3 = JIT_CALL_ARGS (native_ifnonnil, v2);
	    if (insn == Bgotoifnilelsepop || insn == Bgotoifnonnilelsepop
		|| insn == BRgotoifnilelsepop || insn == BRgotoifnonnilelsepop)
	      JIT_PUSH (v2);
	    jit_insn_branch_if (
	      ctxt.func,
	      v3,
	      &labels[op]);
	    if (insn == Bgotoifnilelsepop || insn == Bgotoifnonnilelsepop
		|| insn == BRgotoifnilelsepop || insn == BRgotoifnonnilelsepop)
	      JIT_INC (ctxt.stack, -sizeof (Lisp_Object));
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (BRgoto):
	  {
	    op = FETCH - 128;
	    const int dest = (pc - byte_string_start) + op;
	    JIT_CALL (byte_code_quit, NULL, 0);
	    jit_insn_branch (
	      ctxt.func,
	      &labels[dest]);
	    NEXT;
	  }

	CASE (Breturn):
	  {
	    JIT_NEED_STACK;
	    jit_insn_return (ctxt.func, JIT_POP ());
	    NEXT;
	  }

	CASE (Bdiscard):
	  {
	    JIT_NEED_STACK;
	    JIT_INC (ctxt.stack, -sizeof (Lisp_Object));
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bconstant2):
	  {
	    JIT_NEED_STACK;
	    JIT_PUSH (JIT_CONSTANT (jit_type_Lisp_Object, vectorp[FETCH2]));
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

	CASE (Bsave_restriction):
	  JIT_CALL (native_save_restriction, NULL, 0);
	  JIT_NEXT;
	  NEXT;

	CASE (Bpushcatch):	/* New in 24.4.  */
	  type = CATCHER;
	  goto pushhandler;
	CASE (Bpushconditioncase): /* New in 24.4.  */
	  type = CONDITION_CASE;
	pushhandler:
	  {
	    jit_value_t stackp, jmp, result, result2;
	    int dest = FETCH2;
	    JIT_NEED_STACK;
	    stackp = jit_insn_address_of (ctxt.func, ctxt.stack);
	    jmp = JIT_CALL_ARGS (native_pushhandler1, stackp, JIT_POP (),
				 JIT_CONSTANT (jit_type_nint, type));
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
	      result = jit_insn_call_native (ctxt.func, "setjmp", f,
					     setjmp_sig, args, n,
					     JIT_CALL_NOTHROW);
	    } while (0);
	    jit_insn_branch_if_not (ctxt.func, result, &labels[JIT_PC]);
	    JIT_CALL (native_pushhandler2, &stackp, 1);
	    jit_insn_branch (ctxt.func, &labels[dest]);
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
	    handler = JIT_POP ();
	    JIT_CALL (native_unwind_protect, &handler, 1);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Btemp_output_buffer_show): /* Obsolete since 24.1.  */
	  {
	    jit_type_t temp_output_buffer_show_sig;
	    jit_value_t v1, v2, c, q;
	    JIT_NEED_STACK;
	    JIT_SIG (temp_output_buffer_show,
		     jit_type_void,
		     jit_type_Lisp_Object);
	    v1 = JIT_POP ();
	    v2 = JIT_POP ();
	    JIT_CALL (temp_output_buffer_show, &v2, 1);
	    JIT_PUSH (v1);
	    c = JIT_CONSTANT (jit_type_nuint, 1);
	    q = JIT_CONSTANT (jit_type_Lisp_Object, Qnil);
	    JIT_CALL_ARGS (native_unbind_to, c, q);
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bsub1):
	  {
	    JIT_NEED_STACK;
	    JIT_PUSH (JIT_CALL_ARGS (native_add1, JIT_POP (),
				     JIT_CONSTANT (jit_type_sys_bool, 0)));
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Badd1):
	  {
	    JIT_NEED_STACK;
	    JIT_PUSH (JIT_CALL_ARGS (native_add1, JIT_POP (),
				     JIT_CONSTANT (jit_type_sys_bool, 1)));
	    JIT_NEXT;
	    NEXT;
	  }

	CASE (Bgtr):
	CASE (Blss):
	CASE (Bleq):
	CASE (Bgeq):
	  {
	    jit_value_t v1, v2, c;
	    enum Arith_Comparison v[] =
	      {
		ARITH_GRTR,
		ARITH_LESS,
		ARITH_LESS_OR_EQUAL,
		ARITH_GRTR_OR_EQUAL
	      };
	    JIT_NEED_STACK;
	    c = JIT_CONSTANT (jit_type_nuint, v[op-Bgtr]);
	    v2 = JIT_POP ();
	    v1 = JIT_POP ();
	    JIT_PUSH (JIT_CALL_ARGS (arithcompare, v1, v2, c));
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
	    JIT_INC (ctxt.stack, -offs * sizeof (Lisp_Object));
	    v1 = JIT_TOP ();
	    JIT_INC (ctxt.stack, offs * sizeof (Lisp_Object));
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
	    v1 = JIT_TOP ();
	    if (offs != 0)
	      JIT_INC (ctxt.stack, -(offs + 1) * sizeof (Lisp_Object));
	    JIT_PUSH (v1);
	    JIT_INC (ctxt.stack, (offs - 1) * sizeof (Lisp_Object));
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
		v1 = JIT_TOP ();
		JIT_INC (ctxt.stack, -(op + 1) * sizeof (Lisp_Object));
		JIT_PUSH (v1);
	      }
	    else
	      JIT_INC (ctxt.stack, -op * sizeof (Lisp_Object));
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
    int err = !jit_function_compile (ctxt.func);
    jit_context_build_end (ctxt.libjit_ctxt);
    if (err)
      emacs_abort ();
    ASET (byte_code, COMPILED_JIT_ID, (Lisp_Object )ctxt.func);
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
