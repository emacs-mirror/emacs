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
#include "buffer.h"
#include "bytecode.h"
#include "atimer.h"
#include "window.h"

#define DEFAULT_SPEED 2 /* From 0 to 3 map to gcc -O */

#define COMP_DEBUG 1

#define MAX_FUN_NAME 256

/* Max number of args we are able to handle while emitting function calls.  */

#define MAX_ARGS 16

#define DISASS_FILE_NAME "emacs-asm.s"

#define CHECK_STACK					\
  eassert (stack >= stack_base && stack < stack_over)

#define PUSH_LVAL(obj)							\
  do {									\
    CHECK_STACK;							\
    gcc_jit_block_add_assignment (comp.bblock->gcc_bb,			\
				  NULL,					\
				  *stack,				\
				  gcc_jit_lvalue_as_rvalue(obj));	\
    stack++;								\
  } while (0)

#define PUSH_RVAL(obj)					\
  do {							\
    CHECK_STACK;					\
    gcc_jit_block_add_assignment (comp.bblock->gcc_bb,	\
				  NULL,			\
				  *stack,		\
				  (obj));		\
    stack++;						\
  } while (0)

/* This always happens in the first basic block.  */

#define PUSH_PARAM(obj)							\
  do {									\
    CHECK_STACK;							\
    gcc_jit_block_add_assignment (prologue_bb,				\
				  NULL,					\
				  *stack,				\
				  gcc_jit_param_as_rvalue(obj));	\
    stack++;								\
  } while (0)

#define TOS (*(stack - 1))

#define POP0

#define POP1						\
  do {							\
    stack--;						\
    CHECK_STACK;					\
    args[0] = gcc_jit_lvalue_as_rvalue (*stack);	\
  } while (0)

#define POP2						\
  do {							\
    stack--;						\
    CHECK_STACK;					\
    args[1] = gcc_jit_lvalue_as_rvalue (*stack);	\
    stack--;						\
    args[0] = gcc_jit_lvalue_as_rvalue (*stack);	\
  } while (0)

#define POP3						\
  do {							\
    stack--;						\
    CHECK_STACK;					\
    args[2] = gcc_jit_lvalue_as_rvalue (*stack);	\
    stack--;						\
    args[1] = gcc_jit_lvalue_as_rvalue (*stack);	\
    stack--;						\
    args[0] = gcc_jit_lvalue_as_rvalue (*stack);	\
  } while (0)

/* Fetch the next byte from the bytecode stream.  */

#define FETCH (bytestr_data[pc++])

/* Fetch two bytes from the bytecode stream and make a 16-bit number
   out of them.	 */

#define FETCH2 (op = FETCH, op + (FETCH << 8))

#define STR(s) #s

/* With most of the ops we need to do the same stuff so this macros are meant
   to save some typing.	 */

#define CASE(op)					\
  case op :						\
  if (COMP_DEBUG)					\
    gcc_jit_block_add_comment (comp.bblock->gcc_bb,	\
			       NULL,			\
			       "Opcode " STR(op));

/* Pop from the meta-stack, emit the call and push the result */

#define EMIT_CALL_N(name, nargs)					\
  do {									\
    POP##nargs;								\
    res = emit_call (name, comp.lisp_obj_type, nargs, args);		\
    PUSH_RVAL (res);							\
  } while (0)

/* Generate appropriate case and emit call to function. */

#define CASE_CALL_NARGS(name, nargs)					\
  CASE (B##name)							\
  EMIT_CALL_N (STR(F##name), nargs);					\
  break

/* Emit calls to functions with prototype (ptrdiff_t nargs, Lisp_Object *args)
   This is done aggregating args into the scratch_call_area.  */

#define EMIT_SCRATCH_CALL_N(name, nargs)		\
  do {							\
    pop (nargs, &stack, args);				\
    res = emit_scratch_callN (name, nargs, args);	\
    PUSH_RVAL (res);					\
  } while (0)

#define EMIT_ARITHCOMPARE(comparison)					\
  do {									\
    POP2;								\
    args[2] = gcc_jit_context_new_rvalue_from_int (comp.ctxt,		\
						   comp.int_type,	\
						   comparison);		\
    res = emit_call ("arithcompare", comp.lisp_obj_type, 3, args);	\
    PUSH_RVAL (res);							\
  } while (0)

typedef struct {
  gcc_jit_block *gcc_bb;
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
  gcc_jit_type *long_long_type;
  gcc_jit_type *emacs_int_type;
  gcc_jit_type *void_ptr_type;
  gcc_jit_type *ptrdiff_type;
  gcc_jit_type *jmp_buf_type;
  gcc_jit_type *lisp_obj_type;
  gcc_jit_type *lisp_obj_ptr_type;
  gcc_jit_field *lisp_obj_as_ptr;
  gcc_jit_field *lisp_obj_as_num;
  /* struct handler.  */
  gcc_jit_struct *handler;
  gcc_jit_field *handler_jmp_field;
  gcc_jit_field *handler_val_field;
  gcc_jit_field *handler_next_field;
  gcc_jit_type *handler_ptr_type;
  /* struct thread_state.  */
  gcc_jit_struct *thread_state;
  gcc_jit_field *m_handlerlist;
  gcc_jit_type *thread_state_ptr_type;
  gcc_jit_rvalue *current_thread;
  /* libgccjit has really limited support for casting therefore this union will
     be used for the scope.  */
  gcc_jit_type *cast_union_type;
  gcc_jit_field *cast_union_as_ll;
  gcc_jit_field *cast_union_as_l;
  gcc_jit_field *cast_union_as_u;
  gcc_jit_field *cast_union_as_i;
  gcc_jit_field *cast_union_as_b;
  gcc_jit_function *func; /* Current function being compiled  */
  gcc_jit_rvalue *scratch; /* Will point to scratch_call_area  */
  gcc_jit_rvalue *most_positive_fixnum;
  gcc_jit_rvalue *most_negative_fixnum;
  gcc_jit_rvalue *one;
  gcc_jit_rvalue *inttypebits;
  gcc_jit_rvalue *lisp_int0;
  gcc_jit_function *pseudovectorp;
  gcc_jit_function *bool_to_lisp_obj;
  basic_block_t *bblock; /* Current basic block	 */
  Lisp_Object func_hash; /* f_name -> gcc_func	*/
} comp_t;

static comp_t comp;

Lisp_Object scratch_call_area[MAX_ARGS];

FILE *logfile = NULL;

/* The result of one function compilation.  */

typedef struct {
  gcc_jit_result *gcc_res;
  short min_args, max_args;
} comp_f_res_t;

void emacs_native_compile (const char *lisp_f_name, const char *c_f_name,
			   Lisp_Object func, int opt_level, bool dump_asm);


static void
bcall0 (Lisp_Object f)
{
  Ffuncall (1, &f);
}

/* Pop form the main evaluation stack and place the elements in args in reversed
   order.  */

INLINE static void
pop (unsigned n, gcc_jit_lvalue ***stack_ref, gcc_jit_rvalue *args[])
{
  gcc_jit_lvalue **stack = *stack_ref;

  while (n--)
    {
      stack--;
      args[n] = gcc_jit_lvalue_as_rvalue (*stack);
    }

  *stack_ref = stack;
}

INLINE static gcc_jit_field *
type_to_cast_field (gcc_jit_type *type)
{
  gcc_jit_field *field;

  if (type == comp.long_long_type)
    field = comp.cast_union_as_ll;
  else if (type == comp.long_type)
    field = comp.cast_union_as_l;
  else if (type == comp.unsigned_type)
    field = comp.cast_union_as_u;
  else if (type == comp.int_type)
    field = comp.cast_union_as_i;
  else if (type == comp.bool_type)
    field = comp.cast_union_as_b;
  else
    error ("unsopported cast\n");

  return field;
}

static gcc_jit_function *
emit_func_declare (const char *f_name, gcc_jit_type *ret_type,
		   unsigned nargs, gcc_jit_rvalue **args,
		   enum	 gcc_jit_function_kind kind, bool reusable)
{
  gcc_jit_param *param[4];
  gcc_jit_type *type[4];

  /* If args are passed types are extracted from that otherwise assume params */
  /* are all lisp objs.	 */
  if (args)
    for (int i = 0; i < nargs; i++)
      type[i] = gcc_jit_rvalue_get_type (args[i]);
  else
    for (int i = 0; i < nargs; i++)
      type[i] = comp.lisp_obj_type;

  switch (nargs) {
  case 4:
    param[3] = gcc_jit_context_new_param(comp.ctxt,
					 NULL,
					 type[3],
					 "c");
    /* Fall through */
    FALLTHROUGH;
  case 3:
    param[2] = gcc_jit_context_new_param(comp.ctxt,
					 NULL,
					 type[2],
					 "c");
    /* Fall through */
    FALLTHROUGH;
  case 2:
    param[1] = gcc_jit_context_new_param(comp.ctxt,
					 NULL,
					 type[1],
					 "b");
    /* Fall through */
    FALLTHROUGH;
  case 1:
    param[0] = gcc_jit_context_new_param(comp.ctxt,
					 NULL,
					 type[0],
					 "a");
    /* Fall through */
    FALLTHROUGH;
  case 0:
    break;
  default:
    /* Argnum not supported  */
    eassert (0);
  }

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
		gcc_jit_block *then_target, gcc_jit_block *else_target)
{
  gcc_jit_block_end_with_conditional (comp.bblock->gcc_bb,
				      NULL,
				      test,
				      then_target,
				      else_target);
  comp.bblock->terminated = true;
}

/* Close current basic block emitting a comparison between two rval.  */

static gcc_jit_rvalue *
emit_comparison_jump (enum gcc_jit_comparison op, /* TODO add basick block as param */
		     gcc_jit_rvalue *a, gcc_jit_rvalue *b,
		     gcc_jit_block *then_target, gcc_jit_block *else_target)
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
  gcc_jit_field *orig_field =
    type_to_cast_field (gcc_jit_rvalue_get_type (obj));
  gcc_jit_field *dest_field = type_to_cast_field (new_type);

  gcc_jit_lvalue *tmp_u =
    gcc_jit_function_new_local (comp.func,
				NULL,
				comp.cast_union_type,
				"union_cast");
  gcc_jit_block_add_assignment (comp.bblock->gcc_bb,
				NULL,
				gcc_jit_lvalue_access_field (tmp_u,
							     NULL,
							     orig_field),
				obj);

  return gcc_jit_rvalue_access_field ( gcc_jit_lvalue_as_rvalue (tmp_u),
				       NULL,
				       dest_field);
}

INLINE static gcc_jit_rvalue *
emit_rval_XLI (gcc_jit_rvalue *obj)
{
  return gcc_jit_rvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_num);
}

INLINE static gcc_jit_lvalue *
emit_lval_XLI (gcc_jit_lvalue *obj)
{
  return gcc_jit_lvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_num);
}

INLINE static gcc_jit_rvalue *
emit_rval_XLP (gcc_jit_rvalue *obj)
{
  return gcc_jit_rvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_ptr);
}

INLINE static gcc_jit_lvalue *
emit_lval_XLP (gcc_jit_lvalue *obj)
{
  return gcc_jit_lvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_ptr);
}

static gcc_jit_rvalue *
emit_TAGGEDP (gcc_jit_rvalue *obj, unsigned tag)
{
   /* (! (((unsigned) (XLI (a) >> (USE_LSB_TAG ? 0 : VALBITS)) \
	- (unsigned) (tag)) \
	& ((1 << GCTYPEBITS) - 1))) */

  gcc_jit_rvalue *sh_res =
    gcc_jit_context_new_binary_op (
      comp.ctxt,
      NULL,
      GCC_JIT_BINARY_OP_RSHIFT,
      comp.emacs_int_type,
      emit_rval_XLI (obj),
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
  return emit_TAGGEDP (obj, Lisp_Vectorlike);
}

static gcc_jit_rvalue *
emit_CONSP (gcc_jit_rvalue *obj)
{
  return emit_TAGGEDP (obj, Lisp_Cons);
}

static gcc_jit_rvalue *
emit_FLOATP (gcc_jit_rvalue *obj)
{
  return emit_TAGGEDP (obj, Lisp_Float);
}

static gcc_jit_rvalue *
emit_BIGNUMP (gcc_jit_rvalue *obj)
{
  /* PSEUDOVECTORP (x, PVEC_BIGNUM); */
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

  gcc_jit_rvalue *sh_res =
    gcc_jit_context_new_binary_op (
      comp.ctxt,
      NULL,
      GCC_JIT_BINARY_OP_RSHIFT,
      comp.emacs_int_type,
      emit_rval_XLI (obj),
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
  return gcc_jit_context_new_binary_op (comp.ctxt,
					NULL,
					GCC_JIT_BINARY_OP_RSHIFT,
					comp.emacs_int_type,
					emit_rval_XLI (obj),
					comp.inttypebits);
}

static gcc_jit_rvalue *
emit_INTEGERP (gcc_jit_rvalue *obj)
{
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
  return gcc_jit_context_new_binary_op (comp.ctxt,
					NULL,
					GCC_JIT_BINARY_OP_LOGICAL_OR,
					comp.bool_type,
					emit_INTEGERP(obj),
					emit_cast (comp.bool_type,
						   emit_FLOATP (obj)));
}

static gcc_jit_rvalue *
emit_make_fixnum (gcc_jit_block *block, gcc_jit_rvalue *obj)
{
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

  gcc_jit_block_add_assignment (block,
				NULL,
				emit_lval_XLI (res),
				tmp);

  return gcc_jit_lvalue_as_rvalue (res);
}

/* Construct fill and return a lisp object form a raw pointer.	*/
/* TODO should we pass the bb?	*/
static gcc_jit_rvalue *
emit_lisp_obj_from_ptr (basic_block_t *bblock, void *p)
{
  static unsigned i;
  char scratch[256];

  int res = snprintf (scratch, sizeof (scratch),
		      "lisp_obj_from_ptr_%u", i++);
  if (res >= sizeof (scratch))
    error ("Internal error, truncating temporary variable");

  gcc_jit_lvalue *lisp_obj = gcc_jit_function_new_local (comp.func,
							 NULL,
							 comp.lisp_obj_type,
							 scratch);
  gcc_jit_rvalue *void_ptr =
    gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
					comp.void_ptr_type,
					p);

  if (SYMBOLP (p))
    {
      snprintf (scratch, sizeof (scratch),
		"Symbol %s", (char *) SDATA (SYMBOL_NAME (p)));
      gcc_jit_block_add_comment (bblock->gcc_bb,
				 NULL,
				 scratch);
    }

  gcc_jit_block_add_assignment (bblock->gcc_bb,
				NULL,
				emit_lval_XLP (lisp_obj),
				void_ptr);
  return gcc_jit_lvalue_as_rvalue (lisp_obj);
}

static gcc_jit_rvalue *
emit_scratch_callN (const char *f_name, unsigned nargs, gcc_jit_rvalue **args)
{
  char tmp_str[256];

  /* Here we set all the pointers into the scratch call area.  */
  /* TODO: distinguish primitives for faster calling convention.  */

  /*
    Lisp_Object *p;
    p = scratch_call_area;

    p[0] = nargs;
    p[1] = 0x...;
    .
    .
    .
    p[n] = 0x...;
  */

  snprintf (tmp_str, sizeof (tmp_str), "calling %s", f_name);

  gcc_jit_block_add_comment (comp.bblock->gcc_bb,
			     NULL,
			     tmp_str);

  gcc_jit_lvalue *p =
    gcc_jit_function_new_local(comp.func,
			       NULL,
			       gcc_jit_type_get_pointer (comp.lisp_obj_type),
			       "p");

  gcc_jit_block_add_assignment(comp.bblock->gcc_bb, NULL,
			       p,
			       comp.scratch);

  for (int i = 0; i < nargs; i++) {
    gcc_jit_rvalue *idx =
      gcc_jit_context_new_rvalue_from_int (
	comp.ctxt,
	gcc_jit_context_get_type(comp.ctxt,
				 GCC_JIT_TYPE_UNSIGNED_INT),
	i);
    gcc_jit_block_add_assignment (
      comp.bblock->gcc_bb,
      NULL,
      gcc_jit_context_new_array_access (comp.ctxt,
					NULL,
					gcc_jit_lvalue_as_rvalue(p),
					idx),
      args[i]);
  }

  args[0] = gcc_jit_context_new_rvalue_from_int(comp.ctxt,
						comp.ptrdiff_type,
						nargs);
  args[1] = comp.scratch;

  return emit_call (f_name, comp.lisp_obj_type, 2, args);
}

/* struct handler definition  */

static void
define_handler_struct (void)
{
  comp.handler = gcc_jit_context_new_opaque_struct (comp.ctxt, NULL, "handler");
  comp.handler_ptr_type =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.handler));

  comp.handler_jmp_field = gcc_jit_context_new_field (comp.ctxt,
						      NULL,
						      comp.jmp_buf_type,
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
    { gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "type"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "tag_or_ch"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "nonlocal_exit"),
      comp.handler_val_field,
      comp.handler_next_field,
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.handler_ptr_type,
				 "nextfree"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.lisp_obj_ptr_type,
				 "bytecode_top"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "bytecode_dest"),
      comp.handler_jmp_field,
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.emacs_int_type,
				 "f_lisp_eval_depth"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.ptrdiff_type,
				 "pdlcount"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "poll_suppress_count"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.int_type,
				 "interrupt_input_blocked") };
  gcc_jit_struct_set_fields (comp.handler,
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
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					sizeof (struct thread_state)
					- offsetof (struct thread_state,
						    m_handlerlist)
					- sizeof (struct handler *)),
	"pad1") };

  comp.thread_state =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "thread_state",
				     sizeof (fields) / sizeof (*fields),
				     fields);
  comp.thread_state_ptr_type =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.thread_state));
}

/* Declare a substitute for PSEUDOVECTORP as inline function.  */

static void
define_PSEUDOVECTORP (void)
{
  gcc_jit_param *param[2] =
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

  gcc_jit_block *initial_block =
    gcc_jit_function_new_block (comp.pseudovectorp, "PSEUDOVECTORP_initial_block");

  gcc_jit_block *ret_false_b =
    gcc_jit_function_new_block (comp.pseudovectorp, "ret_false");

  gcc_jit_block *call_pseudovector_typep_b =
    gcc_jit_function_new_block (comp.pseudovectorp, "call_pseudovector");

  /* Set current context as needed */
  basic_block_t bblock = { .gcc_bb = initial_block,
			   .terminated = false };
  comp.bblock = &bblock;
  comp.func = comp.pseudovectorp;

  emit_cond_jump (
    emit_cast (comp.bool_type,
	       emit_VECTORLIKEP (gcc_jit_param_as_rvalue (param[0]))),
    call_pseudovector_typep_b,
    ret_false_b);

  comp.bblock->gcc_bb = ret_false_b;
  gcc_jit_block_end_with_return (ret_false_b,
				 NULL,
				 gcc_jit_context_new_rvalue_from_int(
				   comp.ctxt,
				   comp.bool_type,
				   false));

  gcc_jit_rvalue *args[2] =
    { gcc_jit_param_as_rvalue (param[0]),
      gcc_jit_param_as_rvalue (param[1]) };
  comp.bblock->gcc_bb = call_pseudovector_typep_b;
  /* FIXME XUNTAG missing here. */
  gcc_jit_block_end_with_return (call_pseudovector_typep_b,
				 NULL,
				 emit_call ("helper_PSEUDOVECTOR_TYPEP_XUNTAG",
					    comp.bool_type,
					    2,
					    args));
}

/* Declare a function to convert boolean into t or nil */

static void
define_bool_to_lisp_obj (void)
{
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
  gcc_jit_block *initial_block =
    gcc_jit_function_new_block (comp.bool_to_lisp_obj,
				"bool_to_lisp_obj_initial_block");
  gcc_jit_block *ret_t_block =
    gcc_jit_function_new_block (comp.bool_to_lisp_obj,
				"ret_t");
  gcc_jit_block *ret_nil_block =
    gcc_jit_function_new_block (comp.bool_to_lisp_obj,
				"ret_nil");
  /* Set current context as needed */
  basic_block_t bblock = { .gcc_bb = initial_block,
			   .terminated = false };
  comp.bblock = &bblock;
  comp.func = comp.bool_to_lisp_obj;

  emit_cond_jump (gcc_jit_param_as_rvalue (param),
		  ret_t_block,
		  ret_nil_block);
  bblock.gcc_bb = ret_t_block;
  gcc_jit_block_end_with_return (ret_t_block,
				 NULL,
				 emit_lisp_obj_from_ptr (&bblock, Qt));
  bblock.gcc_bb = ret_nil_block;
  gcc_jit_block_end_with_return (ret_nil_block,
				 NULL,
				 emit_lisp_obj_from_ptr (&bblock, Qnil));
}

static int
ucmp(const void *a, const void *b)
{
#define _I(x) *(const int*)x
  return _I(a) < _I(b) ? -1 : _I(a) > _I(b);
#undef _I
}

/* Compute and initialize all basic blocks.  */
static basic_block_t *
compute_bblocks (ptrdiff_t bytestr_length, unsigned char *bytestr_data)
{
  ptrdiff_t pc = 0;
  unsigned op;
  bool new_bb = true;
  basic_block_t *bb_map = xmalloc (bytestr_length * sizeof (basic_block_t));
  unsigned *bb_start_pc = xmalloc (bytestr_length * sizeof (unsigned));
  unsigned bb_n = 0;

  while (pc < bytestr_length)
    {
      if (new_bb)
	{
	  bb_start_pc[bb_n++] = pc;
	  new_bb = false;
	}

      op = FETCH;
      switch (op)
	{
	  /* 3 byte non branch ops */
	case Bvarref7:
	case Bvarset7:
	case Bvarbind7:
	case Bcall7:
	case Bunbind7:
	case Bstack_ref7:
	case Bstack_set2:
	  pc += 2;
	  break;
	  /* 2 byte non branch ops */
	case Bvarref6:
	case Bvarset6:
	case Bvarbind6:
	case Bcall6:
	case Bunbind6:
	case Bconstant2:
	case BlistN:
	case BconcatN:
	case BinsertN:
	case Bstack_ref6:
	case Bstack_set:
	case BdiscardN:
	  ++pc;
	  break;
	  /* Absolute branches */
	case Bgoto:
	case Bgotoifnil:
	case Bgotoifnonnil:
	case Bgotoifnilelsepop:
	case Bgotoifnonnilelsepop:
	  op = FETCH2;
	  bb_start_pc[bb_n++] = op;
	  new_bb = true;
	  break;
	  /* PC relative branches */
	case BRgoto:
	case BRgotoifnil:
	case BRgotoifnonnil:
	case BRgotoifnilelsepop:
	case BRgotoifnonnilelsepop:
	  op = FETCH - 128;
	  bb_start_pc[bb_n++] = op;
	  new_bb = true;
	  break;
	  /* Other ops changing bb */
	case Bpushcatch:
	case Bpushconditioncase:
	case Bsub1:
	case Badd1:
	case Bnegate:
	case Breturn:
	  new_bb = true;
	  break;
	default:
	  break;
	}
    }

  /* Sort and remove possible duplicates.  */
  qsort (bb_start_pc, bb_n, sizeof(unsigned), ucmp);
  {
    unsigned i, j;
    for (i = j = 0; i < bb_n; i++)
      if (bb_start_pc[i] != bb_start_pc[j])
	bb_start_pc[++j] = bb_start_pc[i];
    bb_n = j + 1;
  }

  basic_block_t curr_bb;
  char block_name[256];
  for (int i = 0, pc = 0; pc < bytestr_length; pc++)
    {
      if (i < bb_n && pc == bb_start_pc[i])
	{
	  ++i;
	  snprintf (block_name, sizeof (block_name), "bb_%d", i);
	  curr_bb.gcc_bb = gcc_jit_function_new_block (comp.func, block_name);
	  curr_bb.terminated = false;
	}
      bb_map[pc] = curr_bb;
    }

  xfree (bb_start_pc);

  return bb_map;
}

static void
init_comp (int opt_level)
{
  comp.ctxt = gcc_jit_context_acquire();

  if (COMP_DEBUG)
    {
      logfile = fopen ("libgccjit.log", "w");
      gcc_jit_context_set_logfile (comp.ctxt,
				   logfile,
				   0, 0);
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING,
				       1);
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES,
				       1);
    }
  if (COMP_DEBUG > 1)
    {
      gcc_jit_context_set_bool_option (comp.ctxt,
				       GCC_JIT_BOOL_OPTION_DEBUGINFO,
				       1);

      gcc_jit_context_dump_reproducer_to_file (comp.ctxt, "comp_reproducer.c");

    }

  gcc_jit_context_set_int_option (comp.ctxt,
				  GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
				  opt_level);

  comp.void_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_VOID);
  comp.void_ptr_type =
    gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_VOID_PTR);
  comp.char_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_CHAR);
  comp.int_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_INT);
  comp.unsigned_type = gcc_jit_context_get_type (comp.ctxt,
						 GCC_JIT_TYPE_UNSIGNED_INT);
  comp.bool_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_BOOL);
  comp.long_type = gcc_jit_context_get_type (comp.ctxt, GCC_JIT_TYPE_LONG);
  comp.long_long_type = gcc_jit_context_get_type (comp.ctxt,
						  GCC_JIT_TYPE_LONG_LONG);

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

  if (sizeof (EMACS_INT) == sizeof (long))
    comp.emacs_int_type = comp.long_type;
  else if (sizeof (EMACS_INT) == sizeof (long long))
    comp.emacs_int_type = comp.long_long_type;
  else
    error ("Unexpected EMACS_INT size.");

  comp.lisp_obj_as_num =  gcc_jit_context_new_field (comp.ctxt,
						     NULL,
						     comp.emacs_int_type,
						     "num");

  gcc_jit_field *lisp_obj_fields[] = { comp.lisp_obj_as_ptr,
				       comp.lisp_obj_as_num };
  comp.lisp_obj_type = gcc_jit_context_new_union_type (comp.ctxt,
						       NULL,
						       "LispObj",
						       sizeof (lisp_obj_fields)
						       / sizeof (*lisp_obj_fields),
						       lisp_obj_fields);
  comp.lisp_obj_ptr_type = gcc_jit_type_get_pointer (comp.lisp_obj_type);

  comp.cast_union_as_ll =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.long_long_type,
			       "ll");
  comp.cast_union_as_l =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       comp.long_type,
			       "l");
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

  gcc_jit_field *cast_union_fields[] =
    { comp.cast_union_as_ll,
      comp.cast_union_as_l,
      comp.cast_union_as_u,
      comp.cast_union_as_i,
      comp.cast_union_as_b,};
  comp.cast_union_type =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "cast_union",
				    sizeof (cast_union_fields)
				    / sizeof (*cast_union_fields),
				    cast_union_fields);
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

  enum gcc_jit_types ptrdiff_t_gcc;
  if (sizeof (ptrdiff_t) == sizeof (int))
    ptrdiff_t_gcc = GCC_JIT_TYPE_INT;
  else if (sizeof (ptrdiff_t) == sizeof (long int))
    ptrdiff_t_gcc = GCC_JIT_TYPE_LONG;
  else if (sizeof (ptrdiff_t) == sizeof (long long int))
    ptrdiff_t_gcc = GCC_JIT_TYPE_LONG_LONG;
  else
    eassert ("ptrdiff_t size not handled.");

  comp.ptrdiff_type = gcc_jit_context_get_type (comp.ctxt, ptrdiff_t_gcc);

  /* Opaque definition for jmp_buf.  */
  comp.jmp_buf_type = gcc_jit_context_new_array_type (comp.ctxt,
						      NULL,
						      comp.char_type,
						      sizeof (jmp_buf));
  comp.scratch =
    gcc_jit_lvalue_get_address(
      gcc_jit_context_new_global (comp.ctxt, NULL,
				  GCC_JIT_GLOBAL_IMPORTED,
				  comp.lisp_obj_type,
				  "scratch_call_area"),
      NULL);

  comp.func_hash = CALLN (Fmake_hash_table, QCtest, Qequal, QCweakness, Qt);

  define_handler_struct ();
  define_thread_state_struct ();
  comp.current_thread =
    gcc_jit_context_new_rvalue_from_ptr (comp.ctxt,
					 comp.thread_state_ptr_type,
					 current_thread);
  define_PSEUDOVECTORP ();
  define_bool_to_lisp_obj ();
}

static void
release_comp (void)
{
  if (COMP_DEBUG)
    gcc_jit_context_dump_to_file (comp.ctxt, "gcc-ctxt-dump.c", 1);
  if (comp.ctxt)
    gcc_jit_context_release(comp.ctxt);

  if (logfile)
    fclose (logfile);
}

static comp_f_res_t
compile_f (const char *f_name, ptrdiff_t bytestr_length,
	   unsigned char *bytestr_data,
	   EMACS_INT stack_depth, Lisp_Object *vectorp,
	   ptrdiff_t vector_size, Lisp_Object args_template)
{
  gcc_jit_rvalue *res;
  comp_f_res_t comp_res = { NULL, 0, 0 };
  ptrdiff_t pc = 0;
  gcc_jit_rvalue *args[4];
  unsigned op;
  char scratch_name[256];
  unsigned pushhandler_n  = 0;

  /* Meta-stack we use to flat the bytecode written for push and pop
     Emacs VM.*/
  gcc_jit_lvalue **stack_base, **stack, **stack_over;
  stack_base = stack =
    (gcc_jit_lvalue **) xmalloc (stack_depth * sizeof (gcc_jit_lvalue *));
  stack_over = stack_base + stack_depth;

  if (FIXNUMP (args_template))
    {
      ptrdiff_t at = XFIXNUM (args_template);
      bool rest = (at & 128) != 0;
      int mandatory = at & 127;
      ptrdiff_t nonrest = at >> 8;

      comp_res.min_args = mandatory;

      eassert (!rest);

      if (!rest && nonrest < SUBR_MAX_ARGS)
	comp_res.max_args = nonrest;
    }
  else if (CONSP (args_template))
    /* FIXME */
    comp_res.min_args = comp_res.max_args = XFIXNUM (Flength (args_template));

  else
    eassert (SYMBOLP (args_template) && args_template == Qnil);


  /* Current function being compiled.  */
  comp.func = emit_func_declare (f_name, comp.lisp_obj_type, comp_res.max_args,
				 NULL, GCC_JIT_FUNCTION_EXPORTED, false);

  for (int i = 0; i < stack_depth; ++i)
    {
      snprintf (scratch_name, sizeof (scratch_name), "local_%d", i);
      stack[i] = gcc_jit_function_new_local (comp.func,
					     NULL,
					     comp.lisp_obj_type,
					     scratch_name);
    }

  gcc_jit_block *prologue_bb =
    gcc_jit_function_new_block (comp.func, "prologue");

  basic_block_t *bb_map = compute_bblocks (bytestr_length, bytestr_data);

  for (ptrdiff_t i = 0; i < comp_res.max_args; ++i)
    PUSH_PARAM (gcc_jit_function_get_param (comp.func, i));
  gcc_jit_block_end_with_jump (prologue_bb, NULL, bb_map[0].gcc_bb);

  gcc_jit_rvalue *nil = emit_lisp_obj_from_ptr (&bb_map[0], Qnil);

  comp.bblock = NULL;

  while (pc < bytestr_length)
    {
      enum handlertype type;

      /* If we are changing BB and the last was one wasn't terminated
	 terminate it with a fall through.  */
      if (comp.bblock && comp.bblock->gcc_bb != bb_map[pc].gcc_bb &&
	  !comp.bblock->terminated)
	{
	  gcc_jit_block_end_with_jump (comp.bblock->gcc_bb, NULL, bb_map[pc].gcc_bb);
	  comp.bblock->terminated = true;
	}
      comp.bblock = &bb_map[pc];
      op = FETCH;

      switch (op)
	{
	CASE (Bstack_ref1)
	  goto stack_ref;
	CASE (Bstack_ref2)
	  goto stack_ref;
	CASE (Bstack_ref3)
	  goto stack_ref;
	CASE (Bstack_ref4)
	  goto stack_ref;
	CASE (Bstack_ref5)
	  stack_ref:
	  PUSH_LVAL (stack_base[(stack - stack_base) - (op - Bstack_ref) - 1]);
	  break;

	CASE (Bstack_ref6)
	  PUSH_LVAL (stack_base[(stack - stack_base) - FETCH - 1]);
	  break;

	CASE (Bstack_ref7)
	  PUSH_LVAL (stack_base[(stack - stack_base) - FETCH2 - 1]);
	  break;

	CASE (Bvarref7)
	  op = FETCH2;
	  goto varref;

	CASE (Bvarref)
	  goto varref_count;
	CASE (Bvarref1)
	  goto varref_count;
	CASE (Bvarref2)
	  goto varref_count;
	CASE (Bvarref3)
	  goto varref_count;
	CASE (Bvarref4)
	  goto varref_count;
	CASE (Bvarref5)
	  varref_count:
	  op -= Bvarref;
	  goto varref;

	CASE (Bvarref6)
	  op = FETCH;
	varref:
	  {
	    args[0] = emit_lisp_obj_from_ptr (comp.bblock, vectorp[op]);
	    res = emit_call ("Fsymbol_value", comp.lisp_obj_type, 1, args);
	    PUSH_RVAL (res);
	    break;
	  }

	CASE (Bvarset)
	  goto varset_count;
	CASE (Bvarset1)
	  goto varset_count;
	CASE (Bvarset2)
	  goto varset_count;
	CASE (Bvarset3)
	  goto varset_count;
	CASE (Bvarset4)
	  goto varset_count;
	CASE (Bvarset5)
	  varset_count:
	  op -= Bvarset;
	  goto varset;

	CASE (Bvarset7)
	  op = FETCH2;
	  goto varset;

	CASE (Bvarset6)
	  op = FETCH;
	varset:
	  {
	    POP1;
	    args[1] = args[0];
	    args[0] = emit_lisp_obj_from_ptr (comp.bblock, vectorp[op]);
	    args[2] = nil;
	    args[3] = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
							   comp.int_type,
							   SET_INTERNAL_SET);
	    res = emit_call ("set_internal", comp.lisp_obj_type, 4, args);
	    PUSH_RVAL (res);
	  }
	  break;

	CASE (Bvarbind6)
	  op = FETCH;
	  goto varbind;

	CASE (Bvarbind7)
	  op = FETCH2;
	  goto varbind;

	CASE (Bvarbind)
	  goto varbind_count;
	CASE (Bvarbind1)
	  goto varbind_count;
	CASE (Bvarbind2)
	  goto varbind_count;
	CASE (Bvarbind3)
	  goto varbind_count;
	CASE (Bvarbind4)
	  goto varbind_count;
	CASE (Bvarbind5)
	  varbind_count:
	  op -= Bvarbind;
	varbind:
	  {
	    args[0] = emit_lisp_obj_from_ptr (comp.bblock, vectorp[op]);
	    pop (1, &stack, &args[1]);
	    res = emit_call ("specbind", comp.lisp_obj_type, 2, args);
	    PUSH_RVAL (res);
	    break;
	  }

	CASE (Bcall6)
	  op = FETCH;
	  goto docall;

	CASE (Bcall7)
	  op = FETCH2;
	  goto docall;

	CASE (Bcall)
	  goto docall_count;
	CASE (Bcall1)
	  goto docall_count;
	CASE (Bcall2)
	  goto docall_count;
	CASE (Bcall3)
	  goto docall_count;
	CASE (Bcall4)
	  goto docall_count;
	CASE (Bcall5)
	docall_count:
	  op -= Bcall;
	docall:
	  {
	    ptrdiff_t nargs = op + 1;
	    pop (nargs, &stack, args);
	    res = emit_scratch_callN ("Ffuncall", nargs, args);
	    PUSH_RVAL (res);
	    break;
	  }

	CASE (Bunbind6)
	  op = FETCH;
	  goto dounbind;

	CASE (Bunbind7)
	  op = FETCH2;
	  goto dounbind;

	CASE (Bunbind)
	  goto dounbind_count;
	CASE (Bunbind1)
	  goto dounbind_count;
	CASE (Bunbind2)
	  goto dounbind_count;
	CASE (Bunbind3)
	  goto dounbind_count;
	CASE (Bunbind4)
	  goto dounbind_count;
	CASE (Bunbind5)
	dounbind_count:
	  op -= Bunbind;
	dounbind:
	  {
	    args[0] = gcc_jit_context_new_rvalue_from_int(comp.ctxt,
							  comp.ptrdiff_type,
							  op);

	    emit_call ("helper_unbind_n", comp.lisp_obj_type, 1, args);
	  }
	  break;

	CASE (Bpophandler)
	  {
	  /* current_thread->m_handlerlist =
	       current_thread->m_handlerlist->next;  */
	    gcc_jit_lvalue *m_handlerlist =
	      gcc_jit_rvalue_dereference_field (comp.current_thread,
						NULL,
						comp.m_handlerlist);

	    gcc_jit_block_add_assignment(
	      comp.bblock->gcc_bb,
	      NULL,
	      m_handlerlist,
	      gcc_jit_lvalue_as_rvalue (
		gcc_jit_rvalue_dereference_field (
			gcc_jit_lvalue_as_rvalue (m_handlerlist),
			NULL,
			comp.handler_next_field)));
	    break;
	  }

	CASE (Bpushconditioncase) /* New in 24.4.  */
	  type = CATCHER;
	  goto pushhandler;

	CASE (Bpushcatch)	/* New in 24.4.	 */
	  type = CONDITION_CASE;;
	pushhandler:
	  {
	    /* struct handler *c = push_handler (POP, type); */
	    int handler_pc = FETCH2;
	    snprintf (scratch_name, sizeof (scratch_name), "c_%u",
		      pushhandler_n);
	    gcc_jit_lvalue *c =
	      gcc_jit_function_new_local (comp.func,
					  NULL,
					  comp.handler_ptr_type,
					  scratch_name);
	    POP1;
	    args[1] = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
							   comp.int_type,
							   type);
	    gcc_jit_block_add_assignment (
	      comp.bblock->gcc_bb,
	      NULL,
	      c,
	      emit_call ("push_handler", comp.handler_ptr_type, 2, args));

	    args[0] =
	      gcc_jit_lvalue_get_address (
		gcc_jit_rvalue_dereference_field (
		  gcc_jit_lvalue_as_rvalue (c),
		  NULL,
		  comp.handler_jmp_field),
		NULL);
#ifdef HAVE__SETJMP
	    res = emit_call ("_setjmp", comp.int_type, 1, args);
#else
	    res = emit_call ("setjmp", comp.int_type, 1, args);
#endif
	    snprintf (scratch_name, sizeof (scratch_name), "push_h_val_%u",
		      pushhandler_n);
	    gcc_jit_block *push_h_val_block =
	      gcc_jit_function_new_block (comp.func, scratch_name);
	    emit_cond_jump (
	      /* This negation is just to have a bool.  */
	      gcc_jit_context_new_unary_op (comp.ctxt,
					    NULL,
					    GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
					    comp.bool_type,
					    res),
	      bb_map[pc].gcc_bb,
	      push_h_val_block);

	    basic_block_t bb_orig = *comp.bblock;
	    comp.bblock->gcc_bb = push_h_val_block;
	    /* current_thread->m_handlerlist = c->next; */
	    gcc_jit_lvalue *m_handlerlist =
	      gcc_jit_rvalue_dereference_field (comp.current_thread,
						NULL,
						comp.m_handlerlist);
	    gcc_jit_block_add_assignment(comp.bblock->gcc_bb,
					 NULL,
					 m_handlerlist,
					 gcc_jit_lvalue_as_rvalue(
					   gcc_jit_rvalue_dereference_field (
					     gcc_jit_lvalue_as_rvalue (c),
					     NULL,
					     comp.handler_next_field)));
	    /* PUSH (c->val); */
	    PUSH_LVAL (
	      gcc_jit_rvalue_dereference_field (gcc_jit_lvalue_as_rvalue (c),
						NULL,
						comp.handler_val_field));
	    *comp.bblock = bb_orig;

	    gcc_jit_block_end_with_jump (push_h_val_block, NULL,
					 bb_map[handler_pc].gcc_bb);
	    ++pushhandler_n;
	  }
	  break;

	CASE_CALL_NARGS (nth, 2);
	CASE_CALL_NARGS (symbolp, 1);

	CASE (Bconsp)
	  POP1;
	  res = emit_cast (comp.bool_type,
			   emit_CONSP (args[0]));
	  res = gcc_jit_context_new_call (comp.ctxt,
					  NULL,
					  comp.bool_to_lisp_obj,
					  1, &res);
	  PUSH_RVAL (res);
	  break;

	CASE_CALL_NARGS (stringp, 1);
	CASE_CALL_NARGS (listp, 1);
	CASE_CALL_NARGS (eq, 2);
	CASE_CALL_NARGS (memq, 1);
	CASE_CALL_NARGS (not, 1);
	CASE_CALL_NARGS (car, 1);
	CASE_CALL_NARGS (cdr, 1);
	CASE_CALL_NARGS (cons, 2);

	CASE (BlistN)
	  op = FETCH;
	  goto make_list;

	CASE (Blist1)
	  goto make_list_count;
	CASE (Blist2)
	  goto make_list_count;
	CASE (Blist3)
	  goto make_list_count;
	CASE (Blist4)
	make_list_count:
	  op = op - Blist1;
	make_list:
	  {
	    POP1;
	    args[1] = nil;
	    res = emit_call ("Fcons", comp.lisp_obj_type, 2, args);
	    PUSH_RVAL (res);
	    for (int i = 0; i < op; ++i)
	      {
		POP2;
		res = emit_call ("Fcons", comp.lisp_obj_type, 2, args);
		PUSH_RVAL (res);
	      }
	    break;
	  }

	CASE_CALL_NARGS (length, 1);
	CASE_CALL_NARGS (aref, 2);
	CASE_CALL_NARGS (aset, 3);
	CASE_CALL_NARGS (symbol_value, 1);
	CASE_CALL_NARGS (symbol_function, 1);
	CASE_CALL_NARGS (set, 2);
	CASE_CALL_NARGS (fset, 2);
	CASE_CALL_NARGS (get, 2);
	CASE_CALL_NARGS (substring, 3);

	CASE (Bconcat2)
	  EMIT_SCRATCH_CALL_N ("Fconcat", 2);
	  break;
	CASE (Bconcat3)
	  EMIT_SCRATCH_CALL_N ("Fconcat", 3);
	  break;
	CASE (Bconcat4)
	  EMIT_SCRATCH_CALL_N ("Fconcat", 4);
	  break;
	CASE (BconcatN)
	  op = FETCH;
	  EMIT_SCRATCH_CALL_N ("Fconcat", op);
	  break;

	CASE (Bsub1)
	  {

	    /* (FIXNUMP (TOP) && XFIXNUM (TOP) != MOST_NEGATIVE_FIXNUM
		 ? make_fixnum (XFIXNUM (TOP) - 1)
		 : Fsub1 (TOP)) */

	    gcc_jit_block *sub1_inline_block =
		 gcc_jit_function_new_block (comp.func, "inline_sub1");
	    gcc_jit_block *sub1_fcall_block =
		 gcc_jit_function_new_block (comp.func, "fcall_sub1");

	    gcc_jit_rvalue *tos_as_num =
	      emit_XFIXNUM (gcc_jit_lvalue_as_rvalue (TOS));

	    emit_cond_jump (
	      gcc_jit_context_new_binary_op (
		comp.ctxt,
		NULL,
		GCC_JIT_BINARY_OP_LOGICAL_AND,
		comp.bool_type,
		emit_cast (comp.bool_type,
			   emit_FIXNUMP (gcc_jit_lvalue_as_rvalue (TOS))),
		gcc_jit_context_new_comparison (comp.ctxt,
						NULL,
						GCC_JIT_COMPARISON_NE,
						tos_as_num,
						comp.most_negative_fixnum)),
	      sub1_inline_block,
	      sub1_fcall_block);

	    gcc_jit_rvalue *sub1_inline_res =
	      gcc_jit_context_new_binary_op (comp.ctxt,
					     NULL,
					     GCC_JIT_BINARY_OP_MINUS,
					     comp.emacs_int_type,
					     tos_as_num,
					     comp.one);

	    gcc_jit_block_add_assignment (sub1_inline_block,
					  NULL,
					  TOS,
					  emit_make_fixnum (sub1_inline_block,
							    sub1_inline_res));
	    basic_block_t bb_orig = *comp.bblock;

	    comp.bblock->gcc_bb = sub1_fcall_block;
	    POP1;
	    res = emit_call ("Fsub1", comp.lisp_obj_type, 1, args);
	    PUSH_RVAL (res);

	    *comp.bblock = bb_orig;

	    gcc_jit_block_end_with_jump (sub1_inline_block, NULL,
					 bb_map[pc].gcc_bb);
	    gcc_jit_block_end_with_jump (sub1_fcall_block, NULL,
					 bb_map[pc].gcc_bb);
	  }

	  break;
	CASE (Badd1)
	  {

	    /* (FIXNUMP (TOP) && XFIXNUM (TOP) != MOST_POSITIVE_FIXNUM
		 ? make_fixnum (XFIXNUM (TOP) + 1)
		 : Fadd (TOP)) */

	    gcc_jit_block *add1_inline_block =
		 gcc_jit_function_new_block (comp.func, "inline_add1");
	    gcc_jit_block *add1_fcall_block =
		 gcc_jit_function_new_block (comp.func, "fcall_add1");

	    gcc_jit_rvalue *tos_as_num =
	      emit_XFIXNUM (gcc_jit_lvalue_as_rvalue (TOS));

	    emit_cond_jump (
	      gcc_jit_context_new_binary_op (
		comp.ctxt,
		NULL,
		GCC_JIT_BINARY_OP_LOGICAL_AND,
		comp.bool_type,
		emit_cast (comp.bool_type,
			   emit_FIXNUMP (gcc_jit_lvalue_as_rvalue (TOS))),
		gcc_jit_context_new_comparison (comp.ctxt,
						NULL,
						GCC_JIT_COMPARISON_NE,
						tos_as_num,
						comp.most_positive_fixnum)),
	      add1_inline_block,
	      add1_fcall_block);

	    gcc_jit_rvalue *add1_inline_res =
	      gcc_jit_context_new_binary_op (comp.ctxt,
					     NULL,
					     GCC_JIT_BINARY_OP_PLUS,
					     comp.emacs_int_type,
					     tos_as_num,
					     comp.one);

	    gcc_jit_block_add_assignment (add1_inline_block,
					  NULL,
					  TOS,
					  emit_make_fixnum (add1_inline_block,
							    add1_inline_res));
	    basic_block_t bb_orig = *comp.bblock;

	    comp.bblock->gcc_bb = add1_fcall_block;
	    POP1;
	    res = emit_call ("Fadd1", comp.lisp_obj_type, 1, args);
	    PUSH_RVAL (res);

	    *comp.bblock = bb_orig;

	    gcc_jit_block_end_with_jump (add1_inline_block, NULL,
					 bb_map[pc].gcc_bb);
	    gcc_jit_block_end_with_jump (add1_fcall_block, NULL,
					 bb_map[pc].gcc_bb);
	  }
	  break;

	CASE (Beqlsign)
	  EMIT_ARITHCOMPARE (ARITH_EQUAL);
	  break;

	CASE (Bgtr)
	  EMIT_ARITHCOMPARE (ARITH_GRTR);
	  break;

	CASE (Blss)
	  EMIT_ARITHCOMPARE (ARITH_LESS);
	  break;

	CASE (Bleq)
	  EMIT_ARITHCOMPARE (ARITH_LESS_OR_EQUAL);
	  break;

	CASE (Bgeq)
	  EMIT_ARITHCOMPARE (ARITH_GRTR_OR_EQUAL);
	  break;

	CASE (Bdiff)
	  EMIT_SCRATCH_CALL_N ("Fminus", 2);
	  break;

	CASE (Bnegate)
	  {

	    /* (FIXNUMP (TOP) && XFIXNUM (TOP) != MOST_NEGATIVE_FIXNUM
		 ? make_fixnum (- XFIXNUM (TOP))
		 : Fminus (1, &TOP)) */

	    gcc_jit_block *negate_inline_block =
		 gcc_jit_function_new_block (comp.func, "inline_negate");
	    gcc_jit_block *negate_fcall_block =
		 gcc_jit_function_new_block (comp.func, "fcall_negate");

	    gcc_jit_rvalue *tos_as_num =
	      emit_XFIXNUM (gcc_jit_lvalue_as_rvalue (TOS));

	    emit_cond_jump (
	      gcc_jit_context_new_binary_op (
		comp.ctxt,
		NULL,
		GCC_JIT_BINARY_OP_LOGICAL_AND,
		comp.bool_type,
		emit_cast (comp.bool_type,
			   emit_FIXNUMP (gcc_jit_lvalue_as_rvalue (TOS))),
		gcc_jit_context_new_comparison (comp.ctxt,
						NULL,
						GCC_JIT_COMPARISON_NE,
						tos_as_num,
						comp.most_negative_fixnum)),
	      negate_inline_block,
	      negate_fcall_block);

	    gcc_jit_rvalue *negate_inline_res =
	      gcc_jit_context_new_unary_op (comp.ctxt,
					    NULL,
					    GCC_JIT_UNARY_OP_MINUS,
					    comp.emacs_int_type,
					    tos_as_num);

	    gcc_jit_block_add_assignment (negate_inline_block,
					  NULL,
					  TOS,
					  emit_make_fixnum (negate_inline_block,
							    negate_inline_res));
	    basic_block_t bb_orig = *comp.bblock;

	    comp.bblock->gcc_bb = negate_fcall_block;
	    EMIT_SCRATCH_CALL_N ("Fminus", 1);
	    *comp.bblock = bb_orig;

	    gcc_jit_block_end_with_jump (negate_inline_block, NULL,
					 bb_map[pc].gcc_bb);
	    gcc_jit_block_end_with_jump (negate_fcall_block, NULL,
					 bb_map[pc].gcc_bb);
	  }
	  break;
	CASE (Bplus)
	  EMIT_SCRATCH_CALL_N ("Fplus", 2);
	  break;
	CASE (Bmax)
	  EMIT_SCRATCH_CALL_N ("Fmax", 2);
	  break;
	CASE (Bmin)
	  EMIT_SCRATCH_CALL_N ("Fmin", 2);
	  break;
	CASE (Bmult)
	  EMIT_SCRATCH_CALL_N ("Ftimes", 2);
	  break;
	CASE (Bpoint)
	  args[0] =
	    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						 comp.ptrdiff_type,
						 PT);
	  res = emit_call ("make_fixed_natnum",
			   comp.lisp_obj_type,
			   1,
			   args);
	  PUSH_RVAL (res);
	  break;

	CASE_CALL_NARGS (goto_char, 1);

	CASE (Binsert)
	  EMIT_SCRATCH_CALL_N ("Finsert", 1);
	  break;

	CASE (Bpoint_max)
	  args[0] =
	    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						 comp.ptrdiff_type,
						 ZV);
	  res = emit_call ("make_fixed_natnum",
			   comp.lisp_obj_type,
			   1,
			   args);
	  PUSH_RVAL (res);
	  break;

	CASE (Bpoint_min)
	  args[0] =
	    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						 comp.ptrdiff_type,
						 BEGV);
	  res = emit_call ("make_fixed_natnum",
			   comp.lisp_obj_type,
			   1,
			   args);
	  PUSH_RVAL (res);
	  break;

	CASE_CALL_NARGS (char_after, 1);
	CASE_CALL_NARGS (following_char, 0);

	CASE (Bpreceding_char)
	  res = emit_call ("Fprevious_char", comp.lisp_obj_type, 0, args);
	  PUSH_RVAL (res);
	  break;

	CASE_CALL_NARGS (current_column, 0);

	CASE (Bindent_to)
	  POP1;
	  args[1] = nil;
	  res = emit_call ("Findent_to", comp.lisp_obj_type, 2, args);
	  PUSH_RVAL (res);
	  break;

	CASE_CALL_NARGS (eolp, 0);
	CASE_CALL_NARGS (eobp, 0);
	CASE_CALL_NARGS (bolp, 0);
	CASE_CALL_NARGS (bobp, 0);
	CASE_CALL_NARGS (current_buffer, 0);
	CASE_CALL_NARGS (set_buffer, 1);

	CASE (Bsave_current_buffer) /* Obsolete since ??.  */
	  goto save_current;
	CASE (Bsave_current_buffer_1)
	  save_current:
	  emit_call ("record_unwind_current_buffer",
		     comp.void_type, 0, NULL);
	  break;

	CASE (Binteractive_p)	/* Obsolete since 24.1.	 */
	  PUSH_RVAL (emit_lisp_obj_from_ptr (comp.bblock,
					     intern ("interactive-p")));
	  res = emit_call ("call0", comp.lisp_obj_type, 1, args);
	  PUSH_RVAL (res);
	  break;

	CASE_CALL_NARGS (forward_char, 1);
	CASE_CALL_NARGS (forward_word, 1);
	CASE_CALL_NARGS (skip_chars_forward, 2);
	CASE_CALL_NARGS (skip_chars_backward, 2);
	CASE_CALL_NARGS (forward_line, 1);
	CASE_CALL_NARGS (char_syntax, 1);
	CASE_CALL_NARGS (buffer_substring, 2);
	CASE_CALL_NARGS (delete_region, 2);
	CASE_CALL_NARGS (narrow_to_region, 2);
	CASE_CALL_NARGS (widen, 0);
	CASE_CALL_NARGS (end_of_line, 1);

	CASE (Bconstant2)
	  goto do_constant;
	  break;

	CASE (Bgoto)
	  op = FETCH2;
	  gcc_jit_block_end_with_jump (comp.bblock->gcc_bb,
				       NULL,
				       bb_map[op].gcc_bb);
	  comp.bblock->terminated = true;
	  break;

	CASE (Bgotoifnil)
	  op = FETCH2;
	  POP1;
	  emit_comparison_jump (GCC_JIT_COMPARISON_EQ, args[0], nil,
				bb_map[op].gcc_bb, bb_map[pc].gcc_bb);
	  break;

	CASE (Bgotoifnonnil)
	  op = FETCH2;
	  POP1;
	  emit_comparison_jump (GCC_JIT_COMPARISON_NE, args[0], nil,
				bb_map[op].gcc_bb, bb_map[pc].gcc_bb);
	  break;

	CASE (Bgotoifnilelsepop)
	  op = FETCH2;
	  emit_comparison_jump (GCC_JIT_COMPARISON_EQ,
				gcc_jit_lvalue_as_rvalue (TOS),
				nil,
				bb_map[op].gcc_bb, bb_map[pc].gcc_bb);
	  POP1;
	  break;

	CASE (Bgotoifnonnilelsepop)
	  op = FETCH2;
	  emit_comparison_jump (GCC_JIT_COMPARISON_NE,
				gcc_jit_lvalue_as_rvalue (TOS),
				nil,
				bb_map[op].gcc_bb, bb_map[pc].gcc_bb);
	  POP1;
	  break;

	CASE (Breturn)
	  POP1;
	  gcc_jit_block_end_with_return(comp.bblock->gcc_bb,
					NULL,
					args[0]);
	  comp.bblock->terminated = true;
	  break;

	CASE (Bdiscard)
	  POP1;
	  break;

	CASE (Bdup)
	  PUSH_LVAL (TOS);
	  break;

	CASE (Bsave_excursion)
	  res = emit_call ("record_unwind_protect_excursion",
				comp.void_type, 0, args);
	  break;

	CASE (Bsave_window_excursion) /* Obsolete since 24.1.  */
	  EMIT_CALL_N ("helper_save_window_excursion", 1);
	  break;

	CASE (Bsave_restriction)
	  args[0] = emit_lisp_obj_from_ptr (comp.bblock,
					    save_restriction_restore);
	  args[1] = emit_call ("save_restriction_save",
			       comp.lisp_obj_type,
			       0,
			       NULL);
	  emit_call ("record_unwind_protect", comp.void_ptr_type, 2, args);
	  break;

	CASE (Bcatch)		/* Obsolete since 24.4.	 */
	  POP2;
	  args[2] = args[1];
	  args[1] = emit_lisp_obj_from_ptr (comp.bblock, eval_sub);
	  emit_call ("internal_catch", comp.void_ptr_type, 3, args);
	  break;

	CASE (Bunwind_protect)	/* FIXME: avoid closure for lexbind.  */
	  POP1;
	  emit_call ("helper_unwind_protect", comp.void_type, 1, args);
	  break;

	CASE (Bcondition_case)		/* Obsolete since 24.4.	 */
	  POP3;
	  emit_call ("internal_lisp_condition_case",
		     comp.lisp_obj_type, 3, args);
	  break;

	CASE (Btemp_output_buffer_setup) /* Obsolete since 24.1.	 */
	  EMIT_CALL_N ("helper_temp_output_buffer_setup", 1);
	  break;

	CASE (Btemp_output_buffer_show) /* Obsolete since 24.1.	*/
	  POP2;
	  emit_call ("temp_output_buffer_show", comp.void_type, 1,
		     &args[1]);
	  PUSH_RVAL (args[0]);
	  emit_call ("helper_unbind_n", comp.lisp_obj_type, 1, args);

	  break;
	CASE (Bunbind_all)	/* Obsolete.  Never used.  */
	  /* To unbind back to the beginning of this frame.  Not used yet,
	     but will be needed for tail-recursion elimination.	 */
	  error ("Bunbind_all not supported");
	  break;

	CASE_CALL_NARGS (set_marker, 3);
	CASE_CALL_NARGS (match_beginning, 1);
	CASE_CALL_NARGS (match_end, 1);
	CASE_CALL_NARGS (upcase, 1);
	CASE_CALL_NARGS (downcase, 1);

	CASE (Bstringeqlsign)
	  EMIT_CALL_N ("Fstring_equal", 2);
	  break;

	CASE (Bstringlss)
	  EMIT_CALL_N ("Fstring_lessp", 2);
	  break;

	CASE_CALL_NARGS (equal, 2);
	CASE_CALL_NARGS (nthcdr, 2);
	CASE_CALL_NARGS (elt, 2);
	CASE_CALL_NARGS (member, 2);
	CASE_CALL_NARGS (assq, 2);
	CASE_CALL_NARGS (setcar, 2);
	CASE_CALL_NARGS (setcdr, 2);

	CASE (Bcar_safe)
	  EMIT_CALL_N ("CAR_SAFE", 1);
	  break;

	CASE (Bcdr_safe)
	  EMIT_CALL_N ("CDR_SAFE", 1);
	  break;

	CASE (Bnconc)
	  EMIT_SCRATCH_CALL_N ("Fnconc", 2);
	  break;

	CASE (Bquo)
	  EMIT_SCRATCH_CALL_N ("Fquo", 2);
	  break;

	CASE_CALL_NARGS (rem, 2);

	CASE (Bnumberp)
	  POP1;
	  res = emit_NUMBERP (args[0]);
	  res = gcc_jit_context_new_call (comp.ctxt,
					  NULL,
					  comp.bool_to_lisp_obj,
					  1, &res);
	  PUSH_RVAL (res);
	  break;

	CASE (Bintegerp)
	  POP1;
	  res = emit_INTEGERP(args[0]);
	  res = gcc_jit_context_new_call (comp.ctxt,
					  NULL,
					  comp.bool_to_lisp_obj,
					  1, &res);
	  PUSH_RVAL (res);
	  break;

	CASE (BRgoto)
	  op = FETCH - 128;
	  op += pc;
	  gcc_jit_block_end_with_jump (comp.bblock->gcc_bb,
				       NULL,
				       bb_map[op].gcc_bb);
	  comp.bblock->terminated = true;
	  break;

	CASE (BRgotoifnil)
	  op = FETCH - 128;
	  op += pc;
	  POP1;
	  emit_comparison_jump (GCC_JIT_COMPARISON_EQ, args[0], nil,
				bb_map[op].gcc_bb, bb_map[pc].gcc_bb);
	  break;

	CASE (BRgotoifnonnil)
	  op = FETCH - 128;
	  op += pc;
	  POP1;
	  emit_comparison_jump (GCC_JIT_COMPARISON_NE, args[0], nil,
				bb_map[op].gcc_bb, bb_map[pc].gcc_bb);
	  break;

	CASE (BRgotoifnilelsepop)
	  op = FETCH - 128;
	  op += pc;
	  emit_comparison_jump (GCC_JIT_COMPARISON_EQ,
				gcc_jit_lvalue_as_rvalue (TOS),
				nil,
				bb_map[op].gcc_bb, bb_map[pc].gcc_bb);
	  POP1;
	  break;

	CASE (BRgotoifnonnilelsepop)
	  op = FETCH - 128;
	  op += pc;
	  emit_comparison_jump (GCC_JIT_COMPARISON_NE,
				gcc_jit_lvalue_as_rvalue (TOS),
				nil,
				bb_map[op].gcc_bb, bb_map[pc].gcc_bb);
	  POP1;
	  break;

	CASE (BinsertN)
	  op = FETCH;
	  EMIT_SCRATCH_CALL_N ("Finsert", op);
	  break;

	CASE (Bstack_set)
	  /* stack-set-0 = discard; stack-set-1 = discard-1-preserve-tos.  */
	  op = FETCH;
	  POP1;
	  if (op > 0)
	    gcc_jit_block_add_assignment (comp.bblock->gcc_bb,
					  NULL,
					  *(stack - op),
					  args[0]);
	  break;

	CASE (Bstack_set2)
	  op = FETCH2;
	  POP1;
	  gcc_jit_block_add_assignment (comp.bblock->gcc_bb,
					NULL,
					*(stack - op),
					args[0]);
	  break;

	CASE (BdiscardN)
	  op = FETCH;
	  if (op & 0x80)
	    {
	      op &= 0x7F;
	      POP1;
	      gcc_jit_block_add_assignment (comp.bblock->gcc_bb,
					    NULL,
					    *(stack - op - 1),
					    args[0]);
	    }

	  stack -= op;
	  break;
	CASE (Bswitch)
	  error ("Bswitch not supported");
	  /* The cases of Bswitch that we handle (which in theory is
	     all of them) are done in Bconstant, below.	 This is done
	     due to a design issue with Bswitch -- it should have
	     taken a constant pool index inline, but instead looks for
	     a constant on the stack.  */
	  goto fail;
	  break;

	default:
	CASE (Bconstant)
	  {
	    if (op < Bconstant || op > Bconstant + vector_size)
	      goto fail;

	    op -= Bconstant;
	  do_constant:

	    /* See the Bswitch case for commentary.  */
	    if (pc >= bytestr_length || bytestr_data[pc] != Bswitch)
	      {
		gcc_jit_rvalue *c =
		  emit_lisp_obj_from_ptr (comp.bblock, vectorp[op]);
		PUSH_RVAL (c);
		break;
	      }

	    /* We're compiling Bswitch instead.	 */
	    ++pc;
	    break;
	  }
	}
    }

  comp_res.gcc_res = gcc_jit_context_compile(comp.ctxt);

  goto exit;

 fail:
  error ("Something went wrong");

 exit:
  xfree (stack_base);
  xfree (bb_map);
  return comp_res;
}

void
emacs_native_compile (const char *lisp_f_name, const char *c_f_name,
		      Lisp_Object func, int opt_level, bool dump_asm)
{
  init_comp (opt_level);
  Lisp_Object bytestr = AREF (func, COMPILED_BYTECODE);
  CHECK_STRING (bytestr);

  if (STRING_MULTIBYTE (bytestr))
    /* BYTESTR must have been produced by Emacs 20.2 or the earlier
       because they produced a raw 8-bit string for byte-code and now
       such a byte-code string is loaded as multibyte while raw 8-bit
       characters converted to multibyte form.	Thus, now we must
       convert them back to the originally intended unibyte form.  */
    bytestr = Fstring_as_unibyte (bytestr);

  ptrdiff_t bytestr_length = SBYTES (bytestr);

  Lisp_Object vector = AREF (func, COMPILED_CONSTANTS);
  CHECK_VECTOR (vector);
  Lisp_Object *vectorp = XVECTOR (vector)->contents;

  Lisp_Object maxdepth = AREF (func, COMPILED_STACK_DEPTH);
  CHECK_FIXNAT (maxdepth);

  /* Gcc doesn't like being interrupted.  */
  sigset_t oldset;
  block_atimers (&oldset);

  comp_f_res_t comp_res = compile_f (c_f_name, bytestr_length, SDATA (bytestr),
				     XFIXNAT (maxdepth) + 1,
				     vectorp, ASIZE (vector),
				     AREF (func, COMPILED_ARGLIST));

  union Aligned_Lisp_Subr *x = xmalloc (sizeof (union Aligned_Lisp_Subr));

  x->s.header.size = PVEC_SUBR << PSEUDOVECTOR_AREA_BITS;
  x->s.function.a0 = gcc_jit_result_get_code(comp_res.gcc_res, c_f_name);
  eassert (x->s.function.a0);
  x->s.min_args = comp_res.min_args;
  x->s.max_args = comp_res.max_args;
  x->s.symbol_name = lisp_f_name;
  defsubr(x);

  if (dump_asm)
    {
      gcc_jit_context_compile_to_file(comp.ctxt,
				      GCC_JIT_OUTPUT_KIND_ASSEMBLER,
				      DISASS_FILE_NAME);
    }
  unblock_atimers (&oldset);
  release_comp ();
}

DEFUN ("native-compile", Fnative_compile, Snative_compile,
       1, 3, 0,
       doc: /* Compile as native code function FUNC and load it.  */) /* FIXME doc */
     (Lisp_Object func, Lisp_Object speed, Lisp_Object disassemble)
{
  static char c_f_name[MAX_FUN_NAME];
  char *lisp_f_name;

  if (!SYMBOLP (func))
    error ("Not a symbol.");

  lisp_f_name = (char *) SDATA (SYMBOL_NAME (func));

  int res = snprintf (c_f_name, MAX_FUN_NAME, "Fnative_comp_%s", lisp_f_name);

  if (res >= MAX_FUN_NAME)
    error ("Function name too long");

  /* FIXME how many other characters are not allowed in C?
     This will introduce name clashs too. */
  char *c = c_f_name;
  while (*c)
    {
      if (*c == '-' ||
	  *c == '+')
	*c = '_';
      ++c;
    }

  func = indirect_function (func);
  if (!COMPILEDP (func))
    error ("Not a byte-compiled function");

  if (speed != Qnil &&
      (!FIXNUMP (speed) ||
       !(XFIXNUM (speed) >= 0 &&
	 XFIXNUM (speed) <= 3)))
    error ("opt-level must be number between 0 and 3");

  int opt_level;
  if (speed == Qnil)
    opt_level = DEFAULT_SPEED;
  else
    opt_level = XFIXNUM (speed);

  emacs_native_compile (lisp_f_name, c_f_name, func, opt_level,
			disassemble != Qnil);

  if (disassemble)
    {
      FILE *fd;
      Lisp_Object str;

      if ((fd = fopen (DISASS_FILE_NAME, "r")))
	{
	  fseek (fd , 0L, SEEK_END);
	  long int size = ftell (fd);
	  fseek (fd , 0L, SEEK_SET);
	  char *buffer = xmalloc (size + 1);
	  ptrdiff_t nread = fread (buffer, 1, size, fd);
	  if (nread > 0)
	    {
	      size = nread;
	      buffer[size] = '\0';
	      str = make_string (buffer, size);
	      fclose (fd);
	    }
	  else
	    str = empty_unibyte_string;
	  xfree (buffer);
	  return str;
	}
      else
	{
	  error ("disassemble file could not be found");
	}
    }

  return Qnil;
}

void
syms_of_comp (void)
{
  defsubr (&Snative_compile);
  comp.func_hash = Qnil;
  staticpro (&comp.func_hash);
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
