/* Compile byte code produced by bytecomp.el into native code.
   Copyright (C) 2019 Free Software Foundation, Inc.

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

#ifdef HAVE_LIBGCCJIT

#include <stdlib.h>
#include <stdio.h>
#include <libgccjit.h>

#include "lisp.h"
#include "puresize.h"
#include "buffer.h"
#include "bytecode.h"
#include "atimer.h"
#include "window.h"

#define DEFAULT_SPEED 2 /* See comp-speed var.  */

#define COMP_DEBUG 1

#define SAFE_ALLOCA_BLOCK(ptr, func, name)			\
do {								\
  (ptr) = SAFE_ALLOCA (sizeof (basic_block_t));			\
  (ptr)->gcc_bb = gcc_jit_function_new_block ((func), (name));	\
  (ptr)->terminated = false;					\
  (ptr)->top = NULL;						\
 } while (0)

#define STR(s) #s

#define DECL_AND_SAFE_ALLOCA_BLOCK(name, func)	\
  basic_block_t *(name);			\
  SAFE_ALLOCA_BLOCK ((name), (func), STR(name))

/* Element of the meta stack.  */
typedef struct {
  gcc_jit_lvalue *gcc_lval;
  enum Lisp_Type type; /* -1 if not set.  */
  Lisp_Object constant; /* This is used for constant propagation.   */
  bool const_set;
} stack_el_t;

typedef struct {
  gcc_jit_block *gcc_bb;
  /* When non zero indicates a stack pointer restart.  */
  stack_el_t *top;
  bool terminated;
} basic_block_t;

/* The compiler context	 */

typedef struct {
  gcc_jit_context *ctxt;
  gcc_jit_type *void_type;
  gcc_jit_type *bool_type;
  gcc_jit_type *char_type;
  gcc_jit_type *int_type;
  gcc_jit_type *unsigned_type;
  gcc_jit_type *long_type;
  gcc_jit_type *unsigned_long_type;
  gcc_jit_type *long_long_type;
  gcc_jit_type *unsigned_long_long_type;
  gcc_jit_type *emacs_int_type;
  gcc_jit_type *void_ptr_type;
  gcc_jit_type *char_ptr_type;
  gcc_jit_type *ptrdiff_type;
  gcc_jit_type *uintptr_type;
  gcc_jit_type *lisp_obj_type;
  gcc_jit_type *lisp_obj_ptr_type;
  gcc_jit_field *lisp_obj_as_ptr;
  gcc_jit_field *lisp_obj_as_num;
  /* struct Lisp_Cons */
  gcc_jit_struct *lisp_cons_s;
  gcc_jit_field *lisp_cons_u;
  gcc_jit_field *lisp_cons_u_s;
  gcc_jit_field *lisp_cons_u_s_car;
  gcc_jit_field *lisp_cons_u_s_u;
  gcc_jit_field *lisp_cons_u_s_u_cdr;
  gcc_jit_type *lisp_cons_type;
  gcc_jit_type *lisp_cons_ptr_type;
  /* struct jmp_buf.  */
  gcc_jit_struct *jmp_buf_s;
  /* struct handler.  */
  gcc_jit_struct *handler_s;
  gcc_jit_field *handler_jmp_field;
  gcc_jit_field *handler_val_field;
  gcc_jit_field *handler_next_field;
  gcc_jit_type *handler_ptr_type;
  /* struct thread_state.  */
  gcc_jit_struct *thread_state_s;
  gcc_jit_field *m_handlerlist;
  gcc_jit_type *thread_state_ptr_type;
  gcc_jit_rvalue *current_thread;
  /* other globals */
  gcc_jit_rvalue *pure;
  /* libgccjit has really limited support for casting therefore this union will
     be used for the scope.  */
  gcc_jit_type *cast_union_type;
  gcc_jit_field *cast_union_as_ll;
  gcc_jit_field *cast_union_as_ull;
  gcc_jit_field *cast_union_as_l;
  gcc_jit_field *cast_union_as_ul;
  gcc_jit_field *cast_union_as_u;
  gcc_jit_field *cast_union_as_i;
  gcc_jit_field *cast_union_as_b;
  gcc_jit_field *cast_union_as_uintptr;
  gcc_jit_field *cast_union_as_ptrdiff;
  gcc_jit_field *cast_union_as_c_p;
  gcc_jit_field *cast_union_as_v_p;
  gcc_jit_field *cast_union_as_lisp_cons_ptr;
  gcc_jit_field *cast_union_as_lisp_obj;
  gcc_jit_field *cast_union_as_lisp_obj_ptr;
  gcc_jit_function *func; /* Current function being compiled  */
  gcc_jit_rvalue *most_positive_fixnum;
  gcc_jit_rvalue *most_negative_fixnum;
  gcc_jit_rvalue *one;
  gcc_jit_rvalue *inttypebits;
  gcc_jit_rvalue *lisp_int0;
  gcc_jit_function *pseudovectorp;
  gcc_jit_function *bool_to_lisp_obj;
  gcc_jit_function *car;
  gcc_jit_function *cdr;
  gcc_jit_function *setcar;
  gcc_jit_function *setcdr;
  gcc_jit_function *check_type;
  gcc_jit_function *check_impure;
  basic_block_t *block; /* Current basic block	 */
  Lisp_Object func_hash; /* f_name -> gcc_func	*/
} comp_t;

static comp_t comp;

FILE *logfile = NULL;

/* The result of one function compilation.  */

typedef struct {
  gcc_jit_result *gcc_res;
  short min_args, max_args;
} comp_f_res_t;

void emacs_native_compile (const char *lisp_f_name, const char *c_f_name,
			   Lisp_Object func, int opt_level, bool dump_asm);

static char * ATTRIBUTE_FORMAT_PRINTF (1, 2)
format_string (const char *format, ...)
{
  static char scratch_area[512];
  va_list va;
  va_start (va, format);
  int res = vsnprintf (scratch_area, sizeof (scratch_area), format, va);
  if (res >= sizeof (scratch_area))
    error ("Truncating string");
  va_end (va);
  return scratch_area;
}

static void
bcall0 (Lisp_Object f)
{
  Ffuncall (1, &f);
}

INLINE static gcc_jit_field *
type_to_cast_field (gcc_jit_type *type)
{
  gcc_jit_field *field;

  if (type == comp.long_long_type)
    field = comp.cast_union_as_ll;
  else if (type == comp.unsigned_long_long_type)
    field = comp.cast_union_as_ull;
  else if (type == comp.long_type)
    field = comp.cast_union_as_l;
  else if (type == comp.unsigned_long_type)
    field = comp.cast_union_as_ul;
  else if (type == comp.unsigned_type)
    field = comp.cast_union_as_u;
  else if (type == comp.int_type)
    field = comp.cast_union_as_i;
  else if (type == comp.bool_type)
    field = comp.cast_union_as_b;
  else if (type == comp.void_ptr_type)
    field = comp.cast_union_as_v_p;
  else if (type == comp.uintptr_type)
    field = comp.cast_union_as_uintptr;
  else if (type == comp.ptrdiff_type)
    field = comp.cast_union_as_ptrdiff;
  else if (type == comp.char_ptr_type)
    field = comp.cast_union_as_c_p;
  else if (type == comp.lisp_cons_ptr_type)
    field = comp.cast_union_as_lisp_cons_ptr;
  else if (type == comp.lisp_obj_type)
    field = comp.cast_union_as_lisp_obj;
  else if (type == comp.lisp_obj_ptr_type)
    field = comp.cast_union_as_lisp_obj_ptr;
  else
    error ("unsupported cast\n");

  return field;
}

INLINE static void
emit_comment (const char *str)
{
  if (COMP_DEBUG)
    gcc_jit_block_add_comment (comp.block->gcc_bb,
			       NULL,
			       str);
}


/* Assignments to the meta-stack slots should be emitted usign this to always */
/* reset annotation fields.   */

static void
emit_assign_to_stack_slot (basic_block_t *block, stack_el_t *slot,
			   gcc_jit_rvalue *val)
{
  gcc_jit_block_add_assignment (block->gcc_bb,
				NULL,
				slot->gcc_lval,
				val);
  slot->type = -1;
  slot->const_set = false;
}

/* Declare a function with all args being Lisp_Object and returning a
   Lisp_Object.  */

static gcc_jit_function *
emit_func_declare (const char *f_name, gcc_jit_type *ret_type,
		   unsigned nargs, gcc_jit_rvalue **args,
		   enum	 gcc_jit_function_kind kind, bool reusable)
{
  gcc_jit_param *param[nargs];
  gcc_jit_type *type[nargs];

  /* If args are passed types are extracted from that otherwise assume params */
  /* are all lisp objs.	 */
  if (args)
    for (unsigned i = 0; i < nargs; i++)
      type[i] = gcc_jit_rvalue_get_type (args[i]);
  else
    for (unsigned i = 0; i < nargs; i++)
      type[i] = comp.lisp_obj_type;

  for (int i = nargs - 1; i >= 0; i--)
    param[i] = gcc_jit_context_new_param(comp.ctxt,
					 NULL,
					 type[i],
					 format_string ("par_%d", i));

  gcc_jit_function *func =
    gcc_jit_context_new_function(comp.ctxt, NULL,
				 kind,
				 ret_type,
				 f_name,
				 nargs,
				 param,
				 0);

  if (reusable)
    {
      Lisp_Object value;
      Lisp_Object key = make_string (f_name, strlen (f_name));
      value = make_pointer_integer (XPL (func));

      EMACS_UINT hash = 0;
      struct Lisp_Hash_Table *ht = XHASH_TABLE (comp.func_hash);
      ptrdiff_t i = hash_lookup (ht, key, &hash);
      /* Don't want to declare the same function two times */
      eassert (i == -1);
      hash_put (ht, key, value, hash);
    }

  return func;
}

static gcc_jit_rvalue *
emit_call (const char *f_name, gcc_jit_type *ret_type, unsigned nargs,
	   gcc_jit_rvalue **args)
{
  Lisp_Object key = make_string (f_name, strlen (f_name));
  EMACS_UINT hash = 0;
  struct Lisp_Hash_Table *ht = XHASH_TABLE (comp.func_hash);
  ptrdiff_t i = hash_lookup (ht, key, &hash);

  if (i == -1)
    {
      emit_func_declare(f_name, ret_type, nargs, args, GCC_JIT_FUNCTION_IMPORTED,
			true);
      i = hash_lookup (ht, key, &hash);
      eassert (i != -1);
    }

  Lisp_Object value = HASH_VALUE (ht, hash_lookup (ht, key, &hash));
  gcc_jit_function *func = (gcc_jit_function *) XFIXNUMPTR (value);

  return gcc_jit_context_new_call(comp.ctxt,
				  NULL,
				  func,
				  nargs,
				  args);
}

/* Close current basic block emitting a conditional.  */

INLINE static void
emit_cond_jump (gcc_jit_rvalue *test,
		basic_block_t *then_target, basic_block_t *else_target)
{
  if (gcc_jit_rvalue_get_type (test) == comp.bool_type)
    gcc_jit_block_end_with_conditional (comp.block->gcc_bb,
				      NULL,
				      test,
				      then_target->gcc_bb,
				      else_target->gcc_bb);
  else
    /* In case test is not bool we do a logical negation to obtain a bool as
       result.  */
    gcc_jit_block_end_with_conditional (
      comp.block->gcc_bb,
      NULL,
      gcc_jit_context_new_unary_op (comp.ctxt,
				    NULL,
				    GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
				    comp.bool_type,
				    test),
      else_target->gcc_bb,
      then_target->gcc_bb);

  comp.block->terminated = true;
}

/* Close current basic block emitting a comparison between two rval.  */

static gcc_jit_rvalue *
emit_comparison_jump (enum gcc_jit_comparison op,
		     gcc_jit_rvalue *a, gcc_jit_rvalue *b,
		     basic_block_t *then_target, basic_block_t *else_target)
{
  gcc_jit_rvalue *test = gcc_jit_context_new_comparison (comp.ctxt,
							 NULL,
							 op,
							 a, b);

  emit_cond_jump (test, then_target, else_target);

  return test;
}

static gcc_jit_rvalue *
emit_cast (gcc_jit_type *new_type, gcc_jit_rvalue *obj)
{
  static unsigned i;

  gcc_jit_field *orig_field =
    type_to_cast_field (gcc_jit_rvalue_get_type (obj));
  gcc_jit_field *dest_field = type_to_cast_field (new_type);

  gcc_jit_lvalue *tmp_u =
    gcc_jit_function_new_local (comp.func,
				NULL,
				comp.cast_union_type,
				format_string ("union_cast_%u", i++));
  gcc_jit_block_add_assignment (comp.block->gcc_bb,
				NULL,
				gcc_jit_lvalue_access_field (tmp_u,
							     NULL,
							     orig_field),
				obj);

  return gcc_jit_rvalue_access_field ( gcc_jit_lvalue_as_rvalue (tmp_u),
				       NULL,
				       dest_field);
}

/*
   Emit the equivalent of

   (typeof_ptr) ((uintptr) ptr + size_of_ptr_ref * i)
*/

static gcc_jit_rvalue *
emit_ptr_arithmetic (gcc_jit_rvalue *ptr, gcc_jit_type *ptr_type,
		     int size_of_ptr_ref, gcc_jit_rvalue *i)
{
  emit_comment ("ptr_arithmetic");

  gcc_jit_rvalue *offset =
    gcc_jit_context_new_binary_op (
      comp.ctxt,
      NULL,
      GCC_JIT_BINARY_OP_MULT,
      comp.uintptr_type,
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.uintptr_type,
					   size_of_ptr_ref),
      emit_cast (comp.uintptr_type, i));

  return
    emit_cast (
      ptr_type,
      gcc_jit_context_new_binary_op (
        comp.ctxt,
	NULL,
	GCC_JIT_BINARY_OP_PLUS,
	comp.uintptr_type,
	emit_cast (comp.uintptr_type, ptr),
	offset));
}

INLINE static gcc_jit_rvalue *
emit_XLI (gcc_jit_rvalue *obj)
{
  emit_comment ("XLI");

  return gcc_jit_rvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_num);
}

INLINE static gcc_jit_lvalue *
emit_lval_XLI (gcc_jit_lvalue *obj)
{
  emit_comment ("lval_XLI");

  return gcc_jit_lvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_num);
}

INLINE static gcc_jit_rvalue *
emit_XLP (gcc_jit_rvalue *obj)
{
  emit_comment ("XLP");

  return gcc_jit_rvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_ptr);
}

INLINE static gcc_jit_lvalue *
emit_lval_XLP (gcc_jit_lvalue *obj)
{
  emit_comment ("lval_XLP");

  return gcc_jit_lvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_ptr);
}

static gcc_jit_rvalue *
emit_XUNTAG (gcc_jit_rvalue *a, gcc_jit_type *type, unsigned lisp_word_tag)
{
  /* #define XUNTAG(a, type, ctype) ((ctype *)
     ((char *) XLP (a) - LISP_WORD_TAG (type))) */
  emit_comment ("XUNTAG");

  return emit_cast (gcc_jit_type_get_pointer (type),
	   gcc_jit_context_new_binary_op (
	     comp.ctxt,
	     NULL,
	     GCC_JIT_BINARY_OP_MINUS,
	     comp.emacs_int_type,
	     emit_XLI (a),
	     gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						  comp.emacs_int_type,
						  lisp_word_tag)));
}

static gcc_jit_rvalue *
emit_XCONS (gcc_jit_rvalue *a)
{
  emit_comment ("XCONS");

  return emit_XUNTAG (a,
		      gcc_jit_struct_as_type (comp.lisp_cons_s),
		      LISP_WORD_TAG (Lisp_Cons));
}

static gcc_jit_rvalue *
emit_EQ (gcc_jit_rvalue *x, gcc_jit_rvalue *y)
{
  emit_comment ("EQ");

  return gcc_jit_context_new_comparison (
	   comp.ctxt,
	   NULL,
	   GCC_JIT_COMPARISON_EQ,
	   emit_XLI (x),
	   emit_XLI (y));
}

static gcc_jit_rvalue *
emit_TAGGEDP (gcc_jit_rvalue *obj, unsigned tag)
{
   /* (! (((unsigned) (XLI (a) >> (USE_LSB_TAG ? 0 : VALBITS)) \
	- (unsigned) (tag)) \
	& ((1 << GCTYPEBITS) - 1))) */
  emit_comment ("TAGGEDP");

  gcc_jit_rvalue *sh_res =
    gcc_jit_context_new_binary_op (
      comp.ctxt,
      NULL,
      GCC_JIT_BINARY_OP_RSHIFT,
      comp.emacs_int_type,
      emit_XLI (obj),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.emacs_int_type,
					   (USE_LSB_TAG ? 0 : VALBITS)));

  gcc_jit_rvalue *minus_res =
    gcc_jit_context_new_binary_op (comp.ctxt,
				   NULL,
				   GCC_JIT_BINARY_OP_MINUS,
				   comp.unsigned_type,
				   emit_cast (comp.unsigned_type, sh_res),
				   gcc_jit_context_new_rvalue_from_int (
				     comp.ctxt,
				     comp.unsigned_type,
				     tag));

  gcc_jit_rvalue *res =
   gcc_jit_context_new_unary_op (
     comp.ctxt,
     NULL,
     GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
     comp.int_type,
     gcc_jit_context_new_binary_op (comp.ctxt,
				    NULL,
				    GCC_JIT_BINARY_OP_BITWISE_AND,
				    comp.unsigned_type,
				    minus_res,
				    gcc_jit_context_new_rvalue_from_int (
				      comp.ctxt,
				      comp.unsigned_type,
				      ((1 << GCTYPEBITS) - 1))));

  return res;
}

static gcc_jit_rvalue *
emit_VECTORLIKEP (gcc_jit_rvalue *obj)
{
  emit_comment ("VECTORLIKEP");

  return emit_TAGGEDP (obj, Lisp_Vectorlike);
}

static gcc_jit_rvalue *
emit_CONSP (gcc_jit_rvalue *obj)
{
  emit_comment ("CONSP");

  return emit_TAGGEDP (obj, Lisp_Cons);
}

static gcc_jit_rvalue *
emit_FLOATP (gcc_jit_rvalue *obj)
{
  emit_comment ("FLOATP");

  return emit_TAGGEDP (obj, Lisp_Float);
}

static gcc_jit_rvalue *
emit_BIGNUMP (gcc_jit_rvalue *obj)
{
  /* PSEUDOVECTORP (x, PVEC_BIGNUM); */
  emit_comment ("BIGNUMP");

  gcc_jit_rvalue *args[2] = {
    obj,
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.int_type,
					 PVEC_BIGNUM) };

  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.pseudovectorp,
				   2,
				   args);
}

static gcc_jit_rvalue *
emit_FIXNUMP (gcc_jit_rvalue *obj)
{
  /* (! (((unsigned) (XLI (x) >> (USE_LSB_TAG ? 0 : FIXNUM_BITS))
	- (unsigned) (Lisp_Int0 >> !USE_LSB_TAG))
	& ((1 << INTTYPEBITS) - 1)))  */
  emit_comment ("FIXNUMP");

  gcc_jit_rvalue *sh_res =
    gcc_jit_context_new_binary_op (
      comp.ctxt,
      NULL,
      GCC_JIT_BINARY_OP_RSHIFT,
      comp.emacs_int_type,
      emit_XLI (obj),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.emacs_int_type,
					   (USE_LSB_TAG ? 0 : FIXNUM_BITS)));

  gcc_jit_rvalue *minus_res =
    gcc_jit_context_new_binary_op (comp.ctxt,
				   NULL,
				   GCC_JIT_BINARY_OP_MINUS,
				   comp.unsigned_type,
				   emit_cast (comp.unsigned_type, sh_res),
				   gcc_jit_context_new_rvalue_from_int (
				     comp.ctxt,
				     comp.unsigned_type,
				     (Lisp_Int0 >> !USE_LSB_TAG)));

  gcc_jit_rvalue *res =
   gcc_jit_context_new_unary_op (
     comp.ctxt,
     NULL,
     GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
     comp.int_type,
     gcc_jit_context_new_binary_op (comp.ctxt,
				    NULL,
				    GCC_JIT_BINARY_OP_BITWISE_AND,
				    comp.unsigned_type,
				    minus_res,
				    gcc_jit_context_new_rvalue_from_int (
				      comp.ctxt,
				      comp.unsigned_type,
				      ((1 << INTTYPEBITS) - 1))));

  return res;
}

static gcc_jit_rvalue *
emit_XFIXNUM (gcc_jit_rvalue *obj)
{
  emit_comment ("XFIXNUM");

  return gcc_jit_context_new_binary_op (comp.ctxt,
					NULL,
					GCC_JIT_BINARY_OP_RSHIFT,
					comp.emacs_int_type,
					emit_XLI (obj),
					comp.inttypebits);
}

static gcc_jit_rvalue *
emit_INTEGERP (gcc_jit_rvalue *obj)
{
  emit_comment ("INTEGERP");

  return gcc_jit_context_new_binary_op (comp.ctxt,
					NULL,
					GCC_JIT_BINARY_OP_LOGICAL_OR,
					comp.bool_type,
					emit_cast (comp.bool_type,
						   emit_FIXNUMP (obj)),
					emit_BIGNUMP (obj));
}

static gcc_jit_rvalue *
emit_NUMBERP (gcc_jit_rvalue *obj)
{
  emit_comment ("NUMBERP");

  return gcc_jit_context_new_binary_op (comp.ctxt,
					NULL,
					GCC_JIT_BINARY_OP_LOGICAL_OR,
					comp.bool_type,
					emit_INTEGERP(obj),
					emit_cast (comp.bool_type,
						   emit_FLOATP (obj)));
}

static gcc_jit_rvalue *
emit_make_fixnum (gcc_jit_rvalue *obj)
{
  emit_comment ("make_fixnum");

  gcc_jit_rvalue *tmp =
    gcc_jit_context_new_binary_op (comp.ctxt,
				   NULL,
				   GCC_JIT_BINARY_OP_LSHIFT,
				   comp.emacs_int_type,
				   obj,
				   comp.inttypebits);

  tmp = gcc_jit_context_new_binary_op (comp.ctxt,
				       NULL,
				       GCC_JIT_BINARY_OP_PLUS,
				       comp.emacs_int_type,
				       tmp,
				       comp.lisp_int0);

  gcc_jit_lvalue *res = gcc_jit_function_new_local (comp.func,
						    NULL,
						    comp.lisp_obj_type,
						    "lisp_obj_fixnum");

  gcc_jit_block_add_assignment (comp.block->gcc_bb,
				NULL,
				emit_lval_XLI (res),
				tmp);

  return gcc_jit_lvalue_as_rvalue (res);
}

/* Construct fill and return a lisp object form a raw pointer.	*/
static gcc_jit_rvalue *
emit_lisp_obj_from_ptr (void *p)
{
  static unsigned i;
  emit_comment ("lisp_obj_from_ptr");

  gcc_jit_lvalue *lisp_obj =
    gcc_jit_function_new_local (comp.func,
				NULL,
				comp.lisp_obj_type,
				format_string ("lisp_obj_from_ptr_%u", i++));
  gcc_jit_rvalue *void_ptr =
    gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
					comp.void_ptr_type,
					p);

  if (SYMBOLP (p))
    emit_comment (
      format_string ("Symbol %s",
		     (char *) SDATA (SYMBOL_NAME (p))));

  gcc_jit_block_add_assignment (comp.block->gcc_bb,
				NULL,
				emit_lval_XLP (lisp_obj),
				void_ptr);

  return gcc_jit_lvalue_as_rvalue (lisp_obj);
}

static gcc_jit_rvalue *
emit_NILP (gcc_jit_rvalue *x)
{
  emit_comment ("NILP");

  return emit_EQ (x, emit_lisp_obj_from_ptr (Qnil));
}

static gcc_jit_rvalue *
emit_XCAR (gcc_jit_rvalue *c)
{
  emit_comment ("XCAR");

  /* XCONS (c)->u.s.car */
  return
    gcc_jit_rvalue_access_field (
      /* XCONS (c)->u.s */
      gcc_jit_rvalue_access_field (
	/* XCONS (c)->u */
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_rvalue_dereference_field (
	    emit_XCONS (c),
	    NULL,
	    comp.lisp_cons_u)),
	NULL,
	comp.lisp_cons_u_s),
      NULL,
      comp.lisp_cons_u_s_car);
}

static gcc_jit_lvalue *
emit_lval_XCAR (gcc_jit_rvalue *c)
{
  emit_comment ("lval_XCAR");

  /* XCONS (c)->u.s.car */
  return
    gcc_jit_lvalue_access_field (
      /* XCONS (c)->u.s */
      gcc_jit_lvalue_access_field (
	/* XCONS (c)->u */
	gcc_jit_rvalue_dereference_field (
	  emit_XCONS (c),
	  NULL,
	  comp.lisp_cons_u),
	NULL,
	comp.lisp_cons_u_s),
      NULL,
      comp.lisp_cons_u_s_car);
}

static gcc_jit_rvalue *
emit_XCDR (gcc_jit_rvalue *c)
{
  emit_comment ("XCDR");
  /* XCONS (c)->u.s.u.cdr */
  return
    gcc_jit_rvalue_access_field (
      /* XCONS (c)->u.s.u */
      gcc_jit_rvalue_access_field (
	/* XCONS (c)->u.s */
	gcc_jit_rvalue_access_field (
	  /* XCONS (c)->u */
	  gcc_jit_lvalue_as_rvalue (
	    gcc_jit_rvalue_dereference_field (
	      emit_XCONS (c),
	      NULL,
	      comp.lisp_cons_u)),
	  NULL,
	  comp.lisp_cons_u_s),
	NULL,
	comp.lisp_cons_u_s_u),
      NULL,
      comp.lisp_cons_u_s_u_cdr);
}

static gcc_jit_lvalue *
emit_lval_XCDR (gcc_jit_rvalue *c)
{
  emit_comment ("lval_XCDR");

  /* XCONS (c)->u.s.u.cdr */
  return
    gcc_jit_lvalue_access_field (
      /* XCONS (c)->u.s.u */
      gcc_jit_lvalue_access_field (
	/* XCONS (c)->u.s */
	gcc_jit_lvalue_access_field (
	  /* XCONS (c)->u */
	  gcc_jit_rvalue_dereference_field (
	    emit_XCONS (c),
	    NULL,
	    comp.lisp_cons_u),
	  NULL,
	  comp.lisp_cons_u_s),
	NULL,
	comp.lisp_cons_u_s_u),
      NULL,
      comp.lisp_cons_u_s_u_cdr);
}

static void
emit_CHECK_CONS (gcc_jit_rvalue *x)
{
  emit_comment ("CHECK_CONS");

  gcc_jit_rvalue *args[] =
    { emit_CONSP (x),
      emit_lisp_obj_from_ptr (Qconsp),
      x };

  gcc_jit_block_add_eval (
    comp.block->gcc_bb,
    NULL,
    gcc_jit_context_new_call (comp.ctxt,
			      NULL,
			      comp.check_type,
			      3,
			      args));
}

static gcc_jit_rvalue *
emit_car_addr (gcc_jit_rvalue *c)
{
  emit_comment ("car_addr");

  return gcc_jit_lvalue_get_address (emit_lval_XCAR (c), NULL);
}

static gcc_jit_rvalue *
emit_cdr_addr (gcc_jit_rvalue *c)
{
  emit_comment ("cdr_addr");

  return gcc_jit_lvalue_get_address (emit_lval_XCDR (c), NULL);
}

static void
emit_XSETCAR (gcc_jit_rvalue *c, gcc_jit_rvalue *n)
{
  emit_comment ("XSETCAR");

  gcc_jit_block_add_assignment(
    comp.block->gcc_bb,
    NULL,
    gcc_jit_rvalue_dereference (
      emit_car_addr (c),
      NULL),
    n);
}

static void
emit_XSETCDR (gcc_jit_rvalue *c, gcc_jit_rvalue *n)
{
  emit_comment ("XSETCDR");

  gcc_jit_block_add_assignment(
    comp.block->gcc_bb,
    NULL,
    gcc_jit_rvalue_dereference (
      emit_cdr_addr (c),
      NULL),
    n);
}

static gcc_jit_rvalue *
emit_PURE_P (gcc_jit_rvalue *ptr)
{

  emit_comment ("PURE_P");

  return
    gcc_jit_context_new_comparison (
      comp.ctxt,
      NULL,
      GCC_JIT_COMPARISON_LE,
      gcc_jit_context_new_binary_op (
	comp.ctxt,
	NULL,
	GCC_JIT_BINARY_OP_MINUS,
	comp.uintptr_type,
	emit_cast (comp.uintptr_type, ptr),
	emit_cast (comp.uintptr_type, comp.pure)),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.uintptr_type,
					   PURESIZE));
}

static gcc_jit_rvalue *
emit_call_n_ref (const char *f_name, unsigned nargs,
		 gcc_jit_lvalue *base_arg)
{
  gcc_jit_rvalue *args[] =
    { gcc_jit_context_new_rvalue_from_int(comp.ctxt,
					  comp.ptrdiff_type,
					  nargs),
      gcc_jit_lvalue_get_address (base_arg, NULL) };
  return emit_call (f_name, comp.lisp_obj_type, 2, args);
}

/* struct Lisp_Cons definition.  */

static void
define_lisp_cons (void)
{
  /*
    union cdr_u
    {
      Lisp_Object cdr;
      struct Lisp_Cons *chain;
    };

    struct cons_s
    {
      Lisp_Object car;
      union cdr_u u;
    };

    union cons_u
    {
      struct cons_s s;
      char align_pad[sizeof (struct Lisp_Cons)];
    };

    struct Lisp_Cons
    {
      union cons_u u;
    };
  */

  comp.lisp_cons_s =
    gcc_jit_context_new_opaque_struct (comp.ctxt,
				       NULL,
				       "comp_Lisp_Cons");
  comp.lisp_cons_type =
    gcc_jit_struct_as_type (comp.lisp_cons_s);
  comp.lisp_cons_ptr_type =
    gcc_jit_type_get_pointer (comp.lisp_cons_type);

  comp.lisp_cons_u_s_u_cdr =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.lisp_obj_type,
			       "cdr");

  gcc_jit_field *cdr_u_fields[] =
    { comp.lisp_cons_u_s_u_cdr,
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.lisp_cons_ptr_type,
				 "chain") };

  gcc_jit_type *cdr_u =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "comp_cdr_u",
				    sizeof (cdr_u_fields)
				    / sizeof (*cdr_u_fields),
				    cdr_u_fields);

  comp.lisp_cons_u_s_car = gcc_jit_context_new_field (comp.ctxt,
					    NULL,
					    comp.lisp_obj_type,
					    "car");
  comp.lisp_cons_u_s_u = gcc_jit_context_new_field (comp.ctxt,
						    NULL,
						    cdr_u,
						    "u");
  gcc_jit_field *cons_s_fields[] =
    { comp.lisp_cons_u_s_car,
      comp.lisp_cons_u_s_u };

  gcc_jit_struct *cons_s =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "comp_cons_s",
				     sizeof (cons_s_fields)
				     / sizeof (*cons_s_fields),
				     cons_s_fields);

  comp.lisp_cons_u_s = gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 gcc_jit_struct_as_type (cons_s),
				 "s");

  gcc_jit_field *cons_u_fields[] =
    { comp.lisp_cons_u_s,
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					sizeof (struct Lisp_Cons)),
	"align_pad") };

  gcc_jit_type *lisp_cons_u_type =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "comp_cons_u",
				    sizeof (cons_u_fields)
				    / sizeof (*cons_u_fields),
				    cons_u_fields);

  comp.lisp_cons_u =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       lisp_cons_u_type,
			       "u");
  gcc_jit_struct_set_fields (comp.lisp_cons_s,
			     NULL, 1, &comp.lisp_cons_u);

}

/* opaque jmp_buf definition.  */

static void
define_jmp_buf (void)
{
  gcc_jit_field *field =
    gcc_jit_context_new_field (
      comp.ctxt,
      NULL,
      gcc_jit_context_new_array_type (comp.ctxt,
				      NULL,
				      comp.char_type,
				      sizeof (jmp_buf)),
      "stuff");
  comp.jmp_buf_s =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "comp_jmp_buf",
				     1, &field);
}

/* struct handler definition  */

static void
define_handler_struct (void)
{
  comp.handler_s =
    gcc_jit_context_new_opaque_struct (comp.ctxt, NULL, "comp_handler");
  comp.handler_ptr_type =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.handler_s));

  comp.handler_jmp_field = gcc_jit_context_new_field (comp.ctxt,
						      NULL,
						      gcc_jit_struct_as_type (
							comp.jmp_buf_s),
						      "jmp");
  comp.handler_val_field = gcc_jit_context_new_field (comp.ctxt,
						      NULL,
						      comp.lisp_obj_type,
						      "val");
  comp.handler_next_field = gcc_jit_context_new_field (comp.ctxt,
						       NULL,
						       comp.handler_ptr_type,
						       "next");
  gcc_jit_field *fields[] =
    { gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					offsetof (struct handler, val)),
	"pad0"),
      comp.handler_val_field,
      comp.handler_next_field,
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					offsetof (struct handler, jmp)
					- offsetof (struct handler, next)
					- sizeof (((struct handler *) 0)->next)),
	"pad1"),
      comp.handler_jmp_field,
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					sizeof (struct handler)
					- offsetof (struct handler, jmp)
					- sizeof (((struct handler *) 0)->jmp)),
	"pad2") };
  gcc_jit_struct_set_fields (comp.handler_s,
			     NULL,
			     sizeof (fields) / sizeof (*fields),
			     fields);

}

static void
define_thread_state_struct (void)
{
  /* Partially opaque definition for `thread_state'.
     Because we need to access just m_handlerlist hopefully this is requires
     less manutention then the full deifnition.	 */

  comp.m_handlerlist = gcc_jit_context_new_field (comp.ctxt,
						  NULL,
						  comp.handler_ptr_type,
						  "m_handlerlist");
  gcc_jit_field *fields[] =
    { gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					offsetof (struct thread_state,
						  m_handlerlist)),
	"pad0"),
      comp.m_handlerlist,
      gcc_jit_context_new_field (
	comp.ctxt,
	NULL,
	gcc_jit_context_new_array_type (
	  comp.ctxt,
	  NULL,
	  comp.char_type,
	  sizeof (struct thread_state)
	  - offsetof (struct thread_state,
		      m_handlerlist)
	  - sizeof (((struct thread_state *) 0)->m_handlerlist)),
	"pad1") };

  comp.thread_state_s =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "comp_thread_state",
				     sizeof (fields) / sizeof (*fields),
				     fields);
  comp.thread_state_ptr_type =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.thread_state_s));
}

static void
define_cast_union (void)
{

  comp.cast_union_as_ll =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.long_long_type,
			       "ll");
  comp.cast_union_as_ull =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.unsigned_long_long_type,
			       "ull");
  comp.cast_union_as_l =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.long_type,
			       "l");
  comp.cast_union_as_ul =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.unsigned_long_type,
			       "ul");
  comp.cast_union_as_u =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.unsigned_type,
			       "u");
  comp.cast_union_as_i =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.int_type,
			       "i");
  comp.cast_union_as_b =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.bool_type,
			       "b");
  comp.cast_union_as_uintptr =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.uintptr_type,
			       "uintptr");
  comp.cast_union_as_ptrdiff =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.ptrdiff_type,
			       "ptrdiff");
  comp.cast_union_as_c_p =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.char_ptr_type,
			       "c_p");
  comp.cast_union_as_v_p =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.void_ptr_type,
			       "v_p");
  comp.cast_union_as_lisp_cons_ptr =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.lisp_cons_ptr_type,
			       "cons_ptr");
  comp.cast_union_as_lisp_obj =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.lisp_obj_type,
			       "lisp_obj");
  comp.cast_union_as_lisp_obj_ptr =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.lisp_obj_ptr_type,
			       "lisp_obj_ptr");


  gcc_jit_field *cast_union_fields[] =
    { comp.cast_union_as_ll,
      comp.cast_union_as_ull,
      comp.cast_union_as_l,
      comp.cast_union_as_ul,
      comp.cast_union_as_u,
      comp.cast_union_as_i,
      comp.cast_union_as_b,
      comp.cast_union_as_uintptr,
      comp.cast_union_as_ptrdiff,
      comp.cast_union_as_c_p,
      comp.cast_union_as_v_p,
      comp.cast_union_as_lisp_cons_ptr,
      comp.cast_union_as_lisp_obj,
      comp.cast_union_as_lisp_obj_ptr };
  comp.cast_union_type =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "cast_union",
				    sizeof (cast_union_fields)
				    / sizeof (*cast_union_fields),
				    cast_union_fields);
}

static void
define_CHECK_TYPE (void)
{
  USE_SAFE_ALLOCA;
  gcc_jit_param *param[] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "ok"),
      gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "predicate"),
      gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "x") };
  comp.check_type =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_ALWAYS_INLINE,
				  comp.void_type,
				  "CHECK_TYPE",
				  3,
				  param,
				  0);
  gcc_jit_rvalue *ok = gcc_jit_param_as_rvalue (param[0]);
  gcc_jit_rvalue *predicate = gcc_jit_param_as_rvalue (param[1]);
  gcc_jit_rvalue *x = gcc_jit_param_as_rvalue (param[2]);

  DECL_AND_SAFE_ALLOCA_BLOCK (init_block, comp.check_type);
  DECL_AND_SAFE_ALLOCA_BLOCK (ok_block, comp.check_type);
  DECL_AND_SAFE_ALLOCA_BLOCK (not_ok_block, comp.check_type);

  comp.block = init_block;
  comp.func = comp.check_type;

  emit_cond_jump (ok, ok_block, not_ok_block);

  gcc_jit_block_end_with_void_return (ok_block->gcc_bb, NULL);

  comp.block = not_ok_block;

  gcc_jit_rvalue *wrong_type_args[] = { predicate, x };

  gcc_jit_block_add_eval (comp.block->gcc_bb,
			  NULL,
			  emit_call ("wrong_type_argument",
				     comp.lisp_obj_type, 2, wrong_type_args));

  gcc_jit_block_end_with_void_return (not_ok_block->gcc_bb, NULL);

  SAFE_FREE ();
}


/* Declare a substitute for CAR as always inlined function.  */

static void
define_CAR_CDR (void)
{
  USE_SAFE_ALLOCA;

  gcc_jit_param *car_param =
	gcc_jit_context_new_param (comp.ctxt,
				   NULL,
				   comp.lisp_obj_type,
				   "c");
  comp.car =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_ALWAYS_INLINE,
				  comp.lisp_obj_type,
				  "CAR",
				  1,
				  &car_param,
				  0);
  gcc_jit_param *cdr_param =
	gcc_jit_context_new_param (comp.ctxt,
				   NULL,
				   comp.lisp_obj_type,
				   "c");
  comp.cdr =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_ALWAYS_INLINE,
				  comp.lisp_obj_type,
				  "CDR",
				  1,
				  &cdr_param,
				  0);

  gcc_jit_function *f = comp.car;
  gcc_jit_param *param = car_param;

  for (int i = 0; i < 2; i++)
    {
      gcc_jit_rvalue *c = gcc_jit_param_as_rvalue (param);
      DECL_AND_SAFE_ALLOCA_BLOCK (init_block, f);
      DECL_AND_SAFE_ALLOCA_BLOCK (is_cons_b, f);
      DECL_AND_SAFE_ALLOCA_BLOCK (not_a_cons_b, f);

      comp.block = init_block;
      comp.func = f;

      emit_cond_jump (emit_CONSP (c), is_cons_b, not_a_cons_b);

      comp.block = is_cons_b;

      if (f == comp.car)
	gcc_jit_block_end_with_return (comp.block->gcc_bb,
				       NULL,
				       emit_XCAR (c));
      else
	gcc_jit_block_end_with_return (comp.block->gcc_bb,
				       NULL,
				       emit_XCDR (c));

      comp.block = not_a_cons_b;

      DECL_AND_SAFE_ALLOCA_BLOCK (is_nil_b, f);
      DECL_AND_SAFE_ALLOCA_BLOCK (not_nil_b, f);

      emit_cond_jump (emit_NILP (c), is_nil_b, not_nil_b);

      comp.block = is_nil_b;
      gcc_jit_block_end_with_return (comp.block->gcc_bb,
				     NULL,
				     emit_lisp_obj_from_ptr (Qnil));

      comp.block = not_nil_b;
      gcc_jit_rvalue *wrong_type_args[] =
	{ emit_lisp_obj_from_ptr (Qlistp), c };

      gcc_jit_block_add_eval (comp.block->gcc_bb,
			      NULL,
			      emit_call ("wrong_type_argument",
					 comp.lisp_obj_type, 2, wrong_type_args));
      gcc_jit_block_end_with_return (comp.block->gcc_bb,
				     NULL,
				     emit_lisp_obj_from_ptr (Qnil));
      f = comp.cdr;
      param = cdr_param;
    }

  SAFE_FREE ();
}

static void
define_setcar_setcdr (void)
{
  USE_SAFE_ALLOCA;

  char const *f_name[] = {"setcar", "setcdr"};
  char const *par_name[] = {"new_car", "new_cdr"};

  for (int i = 0; i < 2; i++)
    {
      gcc_jit_param *cell =
	gcc_jit_context_new_param (comp.ctxt,
				   NULL,
				   comp.lisp_obj_type,
				   "cell");
      gcc_jit_param *new_el =
	gcc_jit_context_new_param (comp.ctxt,
				   NULL,
				   comp.lisp_obj_type,
				   par_name[i]);

      gcc_jit_param *param[] = { cell, new_el };

      gcc_jit_function **f_ref = !i ? &comp.setcar : &comp.setcdr;
      *f_ref = gcc_jit_context_new_function (comp.ctxt, NULL,
					     GCC_JIT_FUNCTION_ALWAYS_INLINE,
					     comp.lisp_obj_type,
					     f_name[i],
					     2,
					     param,
					     0);
      DECL_AND_SAFE_ALLOCA_BLOCK (init_block, *f_ref);
      comp.func = *f_ref;
      comp.block = init_block;

      /* CHECK_CONS (cell); */
      emit_CHECK_CONS (gcc_jit_param_as_rvalue (cell));

      /* CHECK_IMPURE (cell, XCONS (cell)); */
      gcc_jit_rvalue *args[] =
	{ gcc_jit_param_as_rvalue (cell),
	  emit_XCONS (gcc_jit_param_as_rvalue (cell)) };

      gcc_jit_block_add_eval (
			      init_block->gcc_bb,
			      NULL,
			      gcc_jit_context_new_call (comp.ctxt,
							NULL,
							comp.check_impure,
							2,
							args));

      /* XSETCDR (cell, newel); */
      if (!i)
	emit_XSETCAR (gcc_jit_param_as_rvalue (cell),
		      gcc_jit_param_as_rvalue (new_el));
      else
	emit_XSETCDR (gcc_jit_param_as_rvalue (cell),
		      gcc_jit_param_as_rvalue (new_el));

      /* return newel; */
      gcc_jit_block_end_with_return (init_block->gcc_bb,
				     NULL,
				     gcc_jit_param_as_rvalue (new_el));
    }
  SAFE_FREE ();
}

/* Declare a substitute for PSEUDOVECTORP as always inlined function.  */

static void
define_PSEUDOVECTORP (void)
{
  USE_SAFE_ALLOCA;

  gcc_jit_param *param[] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "a"),
      gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "code") };

  comp.pseudovectorp =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_ALWAYS_INLINE,
				  comp.bool_type,
				  "PSEUDOVECTORP",
				  2,
				  param,
				  0);

  DECL_AND_SAFE_ALLOCA_BLOCK (init_block, comp.pseudovectorp);
  DECL_AND_SAFE_ALLOCA_BLOCK (ret_false_b, comp.pseudovectorp);
  DECL_AND_SAFE_ALLOCA_BLOCK (call_pseudovector_typep_b, comp.pseudovectorp);

  comp.block = init_block;
  comp.func = comp.pseudovectorp;

  emit_cond_jump (emit_VECTORLIKEP (gcc_jit_param_as_rvalue (param[0])),
		  call_pseudovector_typep_b,
		  ret_false_b);

  comp.block = ret_false_b;
  gcc_jit_block_end_with_return (ret_false_b->gcc_bb,
				 NULL,
				 gcc_jit_context_new_rvalue_from_int(
				   comp.ctxt,
				   comp.bool_type,
				   false));

  gcc_jit_rvalue *args[2] =
    { gcc_jit_param_as_rvalue (param[0]),
      gcc_jit_param_as_rvalue (param[1]) };
  comp.block = call_pseudovector_typep_b;
  /* FIXME use XUNTAG now that's available.  */
  gcc_jit_block_end_with_return (call_pseudovector_typep_b->gcc_bb
				 ,
				 NULL,
				 emit_call ("helper_PSEUDOVECTOR_TYPEP_XUNTAG",
					    comp.bool_type,
					    2,
					    args));
  SAFE_FREE ();
}

static void
define_CHECK_IMPURE (void)
{
  USE_SAFE_ALLOCA;

  gcc_jit_param *param[] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "obj"),
      gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.void_ptr_type,
				 "ptr") };
  comp.check_impure =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_ALWAYS_INLINE,
				  comp.void_type,
				  "CHECK_IMPURE",
				  2,
				  param,
				  0);

    DECL_AND_SAFE_ALLOCA_BLOCK (init_block, comp.check_impure);
    DECL_AND_SAFE_ALLOCA_BLOCK (err_block, comp.check_impure);
    DECL_AND_SAFE_ALLOCA_BLOCK (ok_block, comp.check_impure);

    comp.block = init_block;
    comp.func = comp.check_impure;

    emit_cond_jump (emit_PURE_P (gcc_jit_param_as_rvalue (param[0])), /* FIXME */
		    err_block,
		    ok_block);
    gcc_jit_block_end_with_void_return (ok_block->gcc_bb, NULL);

    gcc_jit_rvalue *pure_write_error_arg =
      gcc_jit_param_as_rvalue (param[0]);

    comp.block = err_block;
    gcc_jit_block_add_eval (comp.block->gcc_bb,
			    NULL,
			    emit_call ("pure_write_error",
				       comp.void_type, 1,
				       &pure_write_error_arg));

    gcc_jit_block_end_with_void_return (err_block->gcc_bb, NULL);

    SAFE_FREE ();}

/* Declare a function to convert boolean into t or nil */

static void
define_bool_to_lisp_obj (void)
{
  USE_SAFE_ALLOCA;

  /* x ? Qt : Qnil */
  gcc_jit_param *param = gcc_jit_context_new_param (comp.ctxt,
						    NULL,
						    comp.bool_type,
						    "x");
  comp.bool_to_lisp_obj =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_ALWAYS_INLINE,
				  comp.lisp_obj_type,
				  "bool_to_lisp_obj",
				  1,
				  &param,
				  0);
  DECL_AND_SAFE_ALLOCA_BLOCK (init_block, comp.bool_to_lisp_obj);
  DECL_AND_SAFE_ALLOCA_BLOCK (ret_t_block, comp.bool_to_lisp_obj);
  DECL_AND_SAFE_ALLOCA_BLOCK (ret_nil_block, comp.bool_to_lisp_obj);
  comp.block = init_block;
  comp.func = comp.bool_to_lisp_obj;

  emit_cond_jump (gcc_jit_param_as_rvalue (param),
		  ret_t_block,
		  ret_nil_block);

  comp.block = ret_t_block;
  gcc_jit_block_end_with_return (ret_t_block->gcc_bb,
				 NULL,
				 emit_lisp_obj_from_ptr (Qt));

  comp.block = ret_nil_block;
  gcc_jit_block_end_with_return (ret_nil_block->gcc_bb,
				 NULL,
				 emit_lisp_obj_from_ptr (Qnil));

  SAFE_FREE ();
}

DEFUN ("comp-init-ctxt", Fcomp_init_ctxt, Scomp_init_ctxt,
       0, 0, 0,
       doc: /* Initialize the native compiler context. Return t on success.  */)
     (void)
{
  if (comp.ctxt)
    {
      error ("Compiler context already taken.");
      return Qnil;
    }
  comp.ctxt = gcc_jit_context_acquire();

  if (COMP_DEBUG)
    {
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_DEBUGINFO,
				       1);
    }
  if (COMP_DEBUG > 1)
    {
      logfile = fopen ("libgccjit.log", "w");
      gcc_jit_context_set_logfile (comp.ctxt,
				   logfile,
				   0, 0);
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES,
				       1);
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING,
				       1);
      gcc_jit_context_dump_reproducer_to_file (comp.ctxt, "comp_reproducer.c");

    }

  /* Do not inline within a compilation unit.  */
  gcc_jit_context_add_command_line_option (comp.ctxt, "-fno-inline");

  comp.void_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_VOID);
  comp.void_ptr_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_VOID_PTR);
  comp.bool_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_BOOL);
  comp.char_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_CHAR);
  comp.int_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_INT);
  comp.unsigned_type = gcc_jit_context_get_type (comp.ctxt,
						 GCC_JIT_TYPE_UNSIGNED_INT);
  comp.long_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_LONG);
  comp.unsigned_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_UNSIGNED_LONG);
  comp.long_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_LONG_LONG);
  comp.unsigned_long_long_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_UNSIGNED_LONG_LONG);
  comp.char_ptr_type = gcc_jit_type_get_pointer (comp.char_type);

#if EMACS_INT_MAX <= LONG_MAX
  /* 32-bit builds without wide ints, 64-bit builds on Posix hosts.  */
  comp.lisp_obj_as_ptr = gcc_jit_context_new_field (comp.ctxt,
						    NULL,
						    comp.void_ptr_type,
						    "obj");
#else
  /* 64-bit builds on MS-Windows, 32-bit builds with wide ints.	 */
  comp.lisp_obj_as_ptr = gcc_jit_context_new_field (comp.ctxt,
						    NULL,
						    comp.long_long_type,
						    "obj");
#endif

  comp.emacs_int_type = gcc_jit_context_get_int_type (comp.ctxt,
						      sizeof (EMACS_INT),
						      true);

  comp.lisp_obj_as_num =  gcc_jit_context_new_field (comp.ctxt,
						     NULL,
						     comp.emacs_int_type,
						     "num");

  gcc_jit_field *lisp_obj_fields[] = { comp.lisp_obj_as_ptr,
				       comp.lisp_obj_as_num };
  comp.lisp_obj_type = gcc_jit_context_new_union_type (comp.ctxt,
						       NULL,
						       "comp_Lisp_Object",
						       sizeof (lisp_obj_fields)
						       / sizeof (*lisp_obj_fields),
						       lisp_obj_fields);
  comp.lisp_obj_ptr_type = gcc_jit_type_get_pointer (comp.lisp_obj_type);

  comp.most_positive_fixnum =
    gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					  comp.emacs_int_type,
					  MOST_POSITIVE_FIXNUM);
  comp.most_negative_fixnum =
    gcc_jit_context_new_rvalue_from_long (comp.ctxt,
					  comp.emacs_int_type,
					  MOST_NEGATIVE_FIXNUM);
  comp.one =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.emacs_int_type,
					 1);
  comp.inttypebits =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.emacs_int_type,
					 INTTYPEBITS);

  comp.lisp_int0 =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.emacs_int_type,
					 Lisp_Int0);

  comp.ptrdiff_type = gcc_jit_context_get_int_type (comp.ctxt,
						    sizeof (void *),
						    true);

  comp.uintptr_type = gcc_jit_context_get_int_type (comp.ctxt,
						    sizeof (void *),
						    false);

  comp.func_hash = CALLN (Fmake_hash_table, QCtest, Qequal, QCweakness, Qt);

  /* Define data structures.  */

  define_lisp_cons ();
  define_jmp_buf ();
  define_handler_struct ();
  define_thread_state_struct ();
  define_cast_union ();

  comp.current_thread =
    gcc_jit_context_new_rvalue_from_ptr (comp.ctxt,
					 comp.thread_state_ptr_type,
					 current_thread);
  comp.pure =
    gcc_jit_context_new_rvalue_from_ptr (comp.ctxt,
					 comp.void_ptr_type,
					 pure);

  /* Define inline functions.  */

  define_CAR_CDR();
  define_PSEUDOVECTORP ();
  define_CHECK_TYPE ();
  define_CHECK_IMPURE ();
  define_bool_to_lisp_obj ();
  define_setcar_setcdr();

  return Qt;
}

DEFUN ("comp-release-ctxt", Fcomp_release_ctxt, Scomp_release_ctxt,
       0, 0, 0,
       doc: /* Release the native compiler context.  */)
     (void)
{
  if (comp.ctxt)
    gcc_jit_context_release(comp.ctxt);

  if (logfile)
    fclose (logfile);
  comp.ctxt = NULL;

  return Qt;
}

DEFUN ("comp-add-func-to-ctxt", Fcomp_add_func_to_ctxt, Scomp_add_func_to_ctxt,
       1, 1, 0,
       doc: /* Add limple FUNC to the current compilation context.  */)
     (Lisp_Object func)
{
  char *c_name =
    (char *) SDATA (CALLN (Ffuncall, intern ("comp-func-c-func-name"), func));

  return Qt;
}

DEFUN ("comp-compile-and-load-ctxt", Fcomp_compile_and_load_ctxt,
       Scomp_compile_and_load_ctxt,
       0, 1, 0,
       doc: /* Compile as native code the current context and load its
	       functions.  */)
     (Lisp_Object disassemble)
{
  gcc_jit_context_set_int_option (comp.ctxt,
				  GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
				  comp_speed);
  return Qt;
}

void
syms_of_comp (void)
{
  defsubr (&Scomp_init_ctxt);
  defsubr (&Scomp_release_ctxt);
  defsubr (&Scomp_add_func_to_ctxt);
  defsubr (&Scomp_compile_and_load_ctxt);
  comp.func_hash = Qnil;
  staticpro (&comp.func_hash);

  DEFVAR_INT ("comp-speed", comp_speed,
	      doc: /* From 0 to 3.  */);
  comp_speed = DEFAULT_SPEED;

}

/******************************************************************************/
/* Helper functions called from the runtime.				      */
/* These can't be statics till shared mechanism is used to solve relocations. */
/******************************************************************************/

Lisp_Object helper_save_window_excursion (Lisp_Object v1);

void helper_unwind_protect (Lisp_Object handler);

Lisp_Object helper_temp_output_buffer_setup (Lisp_Object x);

Lisp_Object helper_unbind_n (int val);

bool helper_PSEUDOVECTOR_TYPEP_XUNTAG (const union vectorlike_header *a,
				       enum pvec_type code);
Lisp_Object
helper_save_window_excursion (Lisp_Object v1)
{
  ptrdiff_t count1 = SPECPDL_INDEX ();
  record_unwind_protect (restore_window_configuration,
			 Fcurrent_window_configuration (Qnil));
  v1 = Fprogn (v1);
  unbind_to (count1, v1);
  return v1;
}

void helper_unwind_protect (Lisp_Object handler)
{
  /* Support for a function here is new in 24.4.  */
  record_unwind_protect (FUNCTIONP (handler) ? bcall0 : prog_ignore,
			 handler);
}

Lisp_Object
helper_temp_output_buffer_setup (Lisp_Object x)
{
  CHECK_STRING (x);
  temp_output_buffer_setup (SSDATA (x));
  return Vstandard_output;
}

Lisp_Object
helper_unbind_n (int val)
{
  return unbind_to (SPECPDL_INDEX () - val, Qnil);
}

bool
helper_PSEUDOVECTOR_TYPEP_XUNTAG (const union vectorlike_header *a,
				  enum pvec_type code)
{
  return PSEUDOVECTOR_TYPEP (XUNTAG (a, Lisp_Vectorlike,
				     union vectorlike_header),
			     code);
}

#endif /* HAVE_LIBGCCJIT */
