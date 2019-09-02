/* Compile byte code produced by bytecomp.el into native code.
   Copyright (C) 2019 Free Software Foundation, Inc.

Author: Andrea Corallo <akrl@sdf.org>

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
#include <signal.h>
#include <libgccjit.h>

#include "lisp.h"
#include "puresize.h"
#include "window.h"
#include "dynlib.h"
#include "buffer.h"

#define DEFAULT_SPEED 2 /* See comp-speed var.  */

#define COMP_DEBUG 1

/*
  If 1 always favorite the emission of direct constants when these are know
  instead of the corresponding frame slot access.
  This has to prove to have some perf advantage but certainly makes the
  generated code C-like code more bloated.
*/

#define CONST_PROP_MAX 0

/* C symbols emited for the load relocation mechanism.  */
#define DATA_RELOC_SYM "d_reloc"
#define IMPORTED_FUNC_RELOC_SYM "f_reloc"
#define TEXT_DATA_RELOC_SYM "text_data_reloc"
#define TEXT_EXPORTED_FUNC_RELOC_SYM "text_exported_funcs"
#define TEXT_IMPORTED_FUNC_RELOC_SYM "text_imported_funcs"

#define STR_VALUE(s) #s
#define STR(s) STR_VALUE (s)

#define FIRST(x)				\
  XCAR(x)
#define SECOND(x)				\
  XCAR (XCDR (x))
#define THIRD(x)				\
  XCAR (XCDR (XCDR (x)))
#define FORTH(x)				\
  XCAR (XCDR (XCDR (XCDR (x))))

#define FUNCALL1(fun, arg)			\
  CALLN (Ffuncall, intern_c_string (STR(fun)), arg)

#define DECL_BLOCK(name, func)				\
  gcc_jit_block *(name) =				\
    gcc_jit_function_new_block ((func), STR(name))

#ifdef HAVE__SETJMP
#define SETJMP _setjmp
#else
#define SETJMP setjmp
#endif
#define SETJMP_NAME STR (SETJMP)

/* C side of the compiler context. */

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
  gcc_jit_function *func; /* Current function being compiled.  */
  gcc_jit_block *block;  /* Current basic block being compiled.  */
  gcc_jit_lvalue **frame; /* Frame for the current function.  */
  gcc_jit_rvalue *most_positive_fixnum;
  gcc_jit_rvalue *most_negative_fixnum;
  gcc_jit_rvalue *one;
  gcc_jit_rvalue *inttypebits;
  gcc_jit_rvalue *lisp_int0;
  gcc_jit_function *pseudovectorp;
  gcc_jit_function *bool_to_lisp_obj;
  gcc_jit_function *add1;
  gcc_jit_function *sub1;
  gcc_jit_function *negate;
  gcc_jit_function *car;
  gcc_jit_function *cdr;
  gcc_jit_function *setcar;
  gcc_jit_function *setcdr;
  gcc_jit_function *check_type;
  gcc_jit_function *check_impure;
  Lisp_Object func_blocks; /* blk_name -> gcc_block.  */
  Lisp_Object func_hash; /* subr_name -> reloc_field.  */
  Lisp_Object emitter_dispatcher;
  gcc_jit_rvalue *data_relocs; /* Synthesized struct holding data relocs.  */
  gcc_jit_lvalue *func_relocs; /* Synthesized struct holding func relocs.  */
} comp_t;

static comp_t comp;

FILE *logfile = NULL;


/*
   Helper functions called by the runtime.
*/
Lisp_Object helper_save_window_excursion (Lisp_Object v1);
void helper_unwind_protect (Lisp_Object handler);
Lisp_Object helper_temp_output_buffer_setup (Lisp_Object x);
Lisp_Object helper_unbind_n (Lisp_Object n);
bool helper_PSEUDOVECTOR_TYPEP_XUNTAG (const union vectorlike_header *a,
				       enum pvec_type code);
void helper_emit_save_restriction (void);
void helper_set_data_relocs (Lisp_Object *d_relocs_vec, char const *relocs);


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
    error ("Unsupported cast");

  return field;
}

static gcc_jit_block *
retrive_block (Lisp_Object block_name)
{
  Lisp_Object value = Fgethash (block_name, comp.func_blocks, Qnil);
  if (NILP (value))
    error ("LIMPLE basic block inconsistency");

  return (gcc_jit_block *) xmint_pointer (value);
}

static void
declare_block (Lisp_Object block_name)
{
  char *name_str = (char *) SDATA (SYMBOL_NAME (block_name));
  gcc_jit_block *block = gcc_jit_function_new_block (comp.func, name_str);
  Lisp_Object value = make_mint_ptr (block);
  if (!NILP (Fgethash (block_name, comp.func_blocks, Qnil)))
    error ("LIMPLE basic block inconsistency");
  Fputhash (block_name, value, comp.func_blocks);
}

static void
register_emitter (Lisp_Object key, void *func)
{
  Lisp_Object value = make_mint_ptr (func);
  Fputhash (key, value, comp.emitter_dispatcher);
}

INLINE static void
emit_comment (const char *str)
{
  if (COMP_DEBUG)
    gcc_jit_block_add_comment (comp.block,
			       NULL,
			       str);
}

/*
  Declare an imported function.
  When nargs is MANY (ptrdiff_t nargs, Lisp_Object *args) signature is assumed.
  When types is NULL types is assumed to be all Lisp_Objects.
*/
static gcc_jit_field *
declare_imported_func (Lisp_Object subr_sym, gcc_jit_type *ret_type,
		       int nargs, gcc_jit_type **types)
{
  /* Don't want to declare the same function two times.  */
  eassert (NILP (Fgethash (subr_sym, comp.func_hash, Qnil)));

  if (nargs == MANY)
    {
      nargs = 2;
      types = alloca (nargs * sizeof (* types));
      types[0] = comp.ptrdiff_type;
      types[1] = comp.lisp_obj_ptr_type;
    }
  else if (!types)
    {
      types = alloca (nargs * sizeof (* types));
      for (unsigned i = 0; i < nargs; i++)
	types[i] = comp.lisp_obj_type;
    }

  eassert (types);

  /* String containing the function ptr name. */
  Lisp_Object f_ptr_name =
    CALLN (Ffuncall, intern_c_string (STR (comp-c-func-name)),
	   subr_sym, make_string("R", 1));

  gcc_jit_type *f_ptr_type =
    gcc_jit_context_new_function_ptr_type (comp.ctxt,
					   NULL,
					   ret_type,
					   nargs,
					   types,
					   0);
  gcc_jit_field *field =
    gcc_jit_context_new_field (comp.ctxt,
			       NULL,
			       f_ptr_type,
			       SSDATA (f_ptr_name));

  Fputhash (subr_sym, make_mint_ptr (field), comp.func_hash);
  return field;
}

static void
fill_declaration_types (gcc_jit_type **type, gcc_jit_rvalue **args,
			unsigned nargs)
{
  /* If args are passed types are extracted from that otherwise assume params */
  /* are all lisp objs.	 */
  if (args)
    for (unsigned i = 0; i < nargs; i++)
      type[i] = gcc_jit_rvalue_get_type (args[i]);
  else
    for (unsigned i = 0; i < nargs; i++)
      type[i] = comp.lisp_obj_type;
}

static gcc_jit_function *
declare_exported_func (const char *f_name, gcc_jit_type *ret_type,
		       unsigned nargs, gcc_jit_rvalue **args)
{
  gcc_jit_type *type[nargs];

  fill_declaration_types (type, args, nargs);

  gcc_jit_param *param[nargs];
  for (int i = nargs - 1; i >= 0; i--)
    param[i] = gcc_jit_context_new_param(comp.ctxt,
					 NULL,
					 type[i],
					 format_string ("par_%d", i));
  return gcc_jit_context_new_function(comp.ctxt, NULL,
				      GCC_JIT_GLOBAL_EXPORTED,
				      ret_type,
				      f_name,
				      nargs,
				      param,
				      0);
}

static gcc_jit_rvalue *
emit_call (Lisp_Object subr_sym, gcc_jit_type *ret_type, unsigned nargs,
	   gcc_jit_rvalue **args)
{
  Lisp_Object value = Fgethash (subr_sym, comp.func_hash, Qnil);
  eassert (!NILP (value));

  gcc_jit_lvalue *f_ptr =
    gcc_jit_lvalue_access_field (comp.func_relocs,
				 NULL,
				 (gcc_jit_field *) xmint_pointer (value));
  if (!f_ptr)
    error ("Undeclared function relocation.");

  emit_comment (format_string ("calling subr: %s",
			       SSDATA (SYMBOL_NAME (subr_sym))));
  return gcc_jit_context_new_call_through_ptr(comp.ctxt,
					      NULL,
					      gcc_jit_lvalue_as_rvalue (f_ptr),
					      nargs,
					      args);
}

static gcc_jit_rvalue *
emit_call_ref (Lisp_Object subr_sym, unsigned nargs,
	       gcc_jit_lvalue *base_arg)
{
  gcc_jit_rvalue *args[] =
    { gcc_jit_context_new_rvalue_from_int(comp.ctxt,
					  comp.ptrdiff_type,
					  nargs),
      gcc_jit_lvalue_get_address (base_arg, NULL) };
  return emit_call (subr_sym, comp.lisp_obj_type, 2, args);
}

/* Close current basic block emitting a conditional.  */

INLINE static void
emit_cond_jump (gcc_jit_rvalue *test,
		gcc_jit_block *then_target, gcc_jit_block *else_target)
{
  if (gcc_jit_rvalue_get_type (test) == comp.bool_type)
    gcc_jit_block_end_with_conditional (comp.block,
				      NULL,
				      test,
				      then_target,
				      else_target);
  else
    /* In case test is not bool we do a logical negation to obtain a bool as
       result.  */
    gcc_jit_block_end_with_conditional (
      comp.block,
      NULL,
      gcc_jit_context_new_unary_op (comp.ctxt,
				    NULL,
				    GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
				    comp.bool_type,
				    test),
      else_target,
      then_target);

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
  gcc_jit_block_add_assignment (comp.block,
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

  gcc_jit_block_add_assignment (comp.block,
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

  gcc_jit_block_add_assignment (comp.block,
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
    comp.block,
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
    comp.block,
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
    comp.block,
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


/*************************************/
/* Code emitted by LIMPLE statemes.  */
/*************************************/

/* Emit an r-value from an mvar meta variable.
   In case this is a constant that was propagated return it otherwise load it
   from frame.  */

static gcc_jit_rvalue *
emit_mvar_val (Lisp_Object mvar)
{
  if (CONST_PROP_MAX)
    {
      if (NILP (FUNCALL1 (comp-mvar-const-vld, mvar)))
	return
	  gcc_jit_lvalue_as_rvalue(
	    comp.frame[XFIXNUM (FUNCALL1 (comp-mvar-slot, mvar))]);
      else
	return emit_lisp_obj_from_ptr (FUNCALL1 (comp-mvar-constant, mvar));
    }
  else
    {
      if (NILP (FUNCALL1 (comp-mvar-slot, mvar)))
	{
	  /* If the slot is not specified this must be a constant.  */
	  eassert (!NILP (FUNCALL1 (comp-mvar-const-vld, mvar)));
	  return emit_lisp_obj_from_ptr (FUNCALL1 (comp-mvar-constant, mvar));
	}
      return
	gcc_jit_lvalue_as_rvalue(
	  comp.frame[XFIXNUM (FUNCALL1 (comp-mvar-slot, mvar))]);
    }
}

static gcc_jit_rvalue *
emit_set_internal (Lisp_Object args)
{
  /*
    Ex: (call set_internal
              #s(comp-mvar 7 nil t xxx nil)
	      #s(comp-mvar 6 1 t 3 nil))
  */
  /* TODO: Inline the most common case.  */
  eassert (list_length (args) == 3);
  args = XCDR (args);
  int i = 0;
  gcc_jit_rvalue *gcc_args[4];
  FOR_EACH_TAIL (args)
    gcc_args[i++] = emit_mvar_val (XCAR (args));
  gcc_args[2] = emit_lisp_obj_from_ptr (Qnil);
  gcc_args[3] = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						     comp.int_type,
						     SET_INTERNAL_SET);
  return emit_call (intern_c_string ("set_internal"), comp.void_type , 4,
		    gcc_args);
}

/* This is for a regular function with arguments as m-var.   */

static gcc_jit_rvalue *
emit_simple_limple_call (Lisp_Object args, gcc_jit_type *ret_type)
{
  int i = 0;
  Lisp_Object callee = FIRST (args);
  args = XCDR (args);
  ptrdiff_t nargs = list_length (args);
  gcc_jit_rvalue *gcc_args[nargs];
  FOR_EACH_TAIL (args)
    gcc_args[i++] = emit_mvar_val (XCAR (args));

  return emit_call (callee, ret_type, nargs, gcc_args);
}

static gcc_jit_rvalue *
emit_simple_limple_call_lisp_ret (Lisp_Object args)
{
  /*
    Ex: (call Fcar #s(comp-mvar 4 0 nil nil nil))

    Ex: (call Fcons #s(comp-mvar 3 0 t 1 nil)
                    #s(comp-mvar 4 nil t nil nil))
  */
  return emit_simple_limple_call (args, comp.lisp_obj_type);
}

static gcc_jit_rvalue *
emit_simple_limple_call_void_ret (Lisp_Object args)
{
  return emit_simple_limple_call (args, comp.void_type);
}

/* Entry point to dispatch emitting (call fun ...).  */

static gcc_jit_rvalue *
emit_limple_call (Lisp_Object insn)
{
  Lisp_Object callee_sym = FIRST (insn);
  Lisp_Object emitter = Fgethash (callee_sym, comp.emitter_dispatcher, Qnil);

  if (!NILP (emitter))
    {
      gcc_jit_rvalue * (* emitter_ptr) (Lisp_Object) = xmint_pointer (emitter);
      return emitter_ptr (insn);
    }

  return emit_simple_limple_call_lisp_ret (insn);
}

static gcc_jit_rvalue *
emit_limple_call_ref (Lisp_Object insn)
{
  /* Ex: (callref Fplus 2 0).  */

  Lisp_Object callee = FIRST (insn);
  EMACS_UINT nargs = XFIXNUM (SECOND (insn));
  EMACS_UINT base_ptr = XFIXNUM (THIRD (insn));
  return emit_call_ref (callee, nargs, comp.frame[base_ptr]);
}

/* Register an handler for a non local exit.  */

static void
emit_limple_push_handler (gcc_jit_rvalue *handler, gcc_jit_rvalue *handler_type,
			  gcc_jit_block *handler_bb, gcc_jit_block *guarded_bb,
			  EMACS_UINT clobber_slot)
{
  /* Ex: (push-handler #s(comp-mvar 6 0 t (arith-error) nil) 1 bb_3 bb_2).  */

  static unsigned pushhandler_n; /* FIXME move at ctxt or func level.  */

  /* struct handler *c = push_handler (POP, type); */
  gcc_jit_lvalue *c =
    gcc_jit_function_new_local (comp.func,
				NULL,
				comp.handler_ptr_type,
				format_string ("c_%u",
					       pushhandler_n));

  gcc_jit_rvalue *args[] = { handler, handler_type };
  gcc_jit_block_add_assignment (
	      comp.block,
	      NULL,
	      c,
	      emit_call (intern_c_string ("push_handler"), comp.handler_ptr_type, 2, args));

  args[0] =
    gcc_jit_lvalue_get_address (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (c),
	  NULL,
	  comp.handler_jmp_field),
	NULL);

  gcc_jit_rvalue *res;
  res = emit_call (intern_c_string (SETJMP_NAME), comp.int_type, 1, args);
  emit_cond_jump (res, handler_bb, guarded_bb);

  /* This emit the handler part.  */

  comp.block = handler_bb;
  gcc_jit_lvalue *m_handlerlist =
    gcc_jit_rvalue_dereference_field (comp.current_thread,
				      NULL,
				      comp.m_handlerlist);
  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    m_handlerlist,
    gcc_jit_lvalue_as_rvalue(
      gcc_jit_rvalue_dereference_field (gcc_jit_lvalue_as_rvalue (c),
					NULL,
					comp.handler_next_field)));
  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    comp.frame[clobber_slot],
    gcc_jit_lvalue_as_rvalue(
      gcc_jit_rvalue_dereference_field (gcc_jit_lvalue_as_rvalue (c),
					NULL,
					comp.handler_val_field)));
  ++pushhandler_n;
}

static void
emit_limple_insn (Lisp_Object insn)
{
  Lisp_Object op = XCAR (insn);
  Lisp_Object args = XCDR (insn);
  Lisp_Object arg0;
  gcc_jit_rvalue *res;

  if (CONSP (args))
    arg0 = XCAR (args);

  if (EQ (op, Qjump))
    {
      /* Unconditional branch.	*/
      gcc_jit_block *target = retrive_block (arg0);
      gcc_jit_block_end_with_jump (comp.block, NULL, target);
    }
  else if (EQ (op, Qcond_jump))
    {
      /* Conditional branch.  */
      gcc_jit_rvalue *a = emit_mvar_val (arg0);
      gcc_jit_rvalue *b =  emit_mvar_val (SECOND (args));
      gcc_jit_block *target1 = retrive_block (THIRD (args));
      gcc_jit_block *target2 = retrive_block (FORTH (args));

      emit_cond_jump (emit_EQ (a, b), target2, target1);
    }
  else if (EQ (op, Qcond_jump_narg_leq))
    {
      /*
	 Limple: (cond-jump-narg-less 2 entry_2 entry_fallback_2)
	 C: if (nargs < 2) goto entry2_fallback; else goto entry_2;
      */
      gcc_jit_lvalue *nargs =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 0));
      gcc_jit_rvalue *n =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.ptrdiff_type,
					     XFIXNUM (arg0));
      gcc_jit_block *target1 = retrive_block (SECOND (args));
      gcc_jit_block *target2 = retrive_block (THIRD (args));
      gcc_jit_rvalue *test = gcc_jit_context_new_comparison (
			       comp.ctxt,
			       NULL,
			       GCC_JIT_COMPARISON_LE,
			       gcc_jit_lvalue_as_rvalue (nargs),
			       n);
      emit_cond_jump (test, target2, target1);
    }
  else if (EQ (op, Qpush_handler))
    {
      EMACS_UINT clobber_slot = XFIXNUM (FUNCALL1 (comp-mvar-slot, arg0));
      gcc_jit_rvalue *handler = emit_mvar_val (arg0);
      int h_num;
      if (EQ (SECOND (args), Qcatcher))
	h_num = CATCHER;
      else if (EQ (SECOND (args), Qcondition_case))
	h_num = CONDITION_CASE;
      else
	eassert (false);
      gcc_jit_rvalue *handler_type =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.int_type,
					     h_num);
      gcc_jit_block *handler_bb = retrive_block (THIRD (args));
      gcc_jit_block *guarded_bb = retrive_block (FORTH (args));
      emit_limple_push_handler (handler, handler_type, handler_bb, guarded_bb,
				clobber_slot);
    }
  else if (EQ (op, Qpop_handler))
    {
      /*
	C: current_thread->m_handlerlist =
	     current_thread->m_handlerlist->next;
      */
      gcc_jit_lvalue *m_handlerlist =
	gcc_jit_rvalue_dereference_field (comp.current_thread,
					  NULL,
					  comp.m_handlerlist);

      gcc_jit_block_add_assignment(
	comp.block,
	NULL,
	m_handlerlist,
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_rvalue_dereference_field (
	    gcc_jit_lvalue_as_rvalue (m_handlerlist),
	    NULL,
	    comp.handler_next_field)));

    }
  else if (EQ (op, Qcall))
    {
      gcc_jit_block_add_eval (comp.block,
			      NULL,
			      emit_limple_call (args));
    }
  else if (EQ (op, Qset))
    {
      EMACS_UINT slot_n = XFIXNUM (FUNCALL1 (comp-mvar-slot, arg0));
      Lisp_Object arg1 = SECOND (args);

      if (EQ (Ftype_of (arg1), Qcomp_mvar))
	res = emit_mvar_val (arg1);
      else if (EQ (FIRST (arg1), Qcall))
	res = emit_limple_call (XCDR (arg1));
      else if (EQ (FIRST (arg1), Qcallref))
	res = emit_limple_call_ref (XCDR (arg1));
      else
	error ("LIMPLE inconsistent arg1 for op =");
      eassert (res);
      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    comp.frame[slot_n],
				    res);
    }
  else if (EQ (op, Qset_par_to_local))
    {
      /* Ex: (setpar #s(comp-mvar 2 0 nil nil nil) 0).  */
      EMACS_UINT slot_n = XFIXNUM (FUNCALL1 (comp-mvar-slot, arg0));
      EMACS_UINT param_n = XFIXNUM (SECOND (args));
      gcc_jit_rvalue *param =
	gcc_jit_param_as_rvalue (gcc_jit_function_get_param (comp.func,
							     param_n));
      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    comp.frame[slot_n],
				    param);
    }
  else if (EQ (op, Qset_args_to_local))
    {
      /*
	Limple: (set-args-to-local 1)
	C: local[1] = *args;
      */
      gcc_jit_rvalue *gcc_args =
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 1)));

      gcc_jit_rvalue *res =
	gcc_jit_lvalue_as_rvalue (gcc_jit_rvalue_dereference (gcc_args, NULL));

      EMACS_UINT slot_n = XFIXNUM (arg0);
      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    comp.frame[slot_n],
				    res);
    }
  else if (EQ (op, Qset_rest_args_to_local))
    {
      /*
        Limple: (set-rest-args-to-local 3)
        C: local[3] = list (nargs - 3, args);
      */
      gcc_jit_rvalue *n =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.ptrdiff_type,
					     XFIXNUM (arg0));
      gcc_jit_lvalue *nargs =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 0));
      gcc_jit_lvalue *args =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 1));

      gcc_jit_rvalue *list_args[] =
	{ gcc_jit_context_new_binary_op (comp.ctxt,
					 NULL,
					 GCC_JIT_BINARY_OP_MINUS,
					 comp.ptrdiff_type,
					 gcc_jit_lvalue_as_rvalue (nargs),
					 n),
	  gcc_jit_lvalue_as_rvalue (args) };

      res = emit_call (Qlist, comp.lisp_obj_type, 2,
		       list_args);

      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    comp.frame[XFIXNUM (arg0)],
				    res);
    }
  else if (EQ (op, Qinc_args))
    {
      /*
	Limple: (inc-args)
	C: ++args;
      */
      gcc_jit_lvalue *args =
	gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 1));

      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    args,
				    emit_ptr_arithmetic (
				      gcc_jit_lvalue_as_rvalue (args),
				      comp.lisp_obj_ptr_type,
				      sizeof (Lisp_Object),
				      comp.one));
    }
  else if (EQ (op, Qsetimm))
    {
      /* EX: (=imm #s(comp-mvar 9 1 t 3 nil) 3 a).  */
      EMACS_UINT slot_n = XFIXNUM (FUNCALL1 (comp-mvar-slot, arg0));
      gcc_jit_rvalue *reloc_n =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.int_type,
					     XFIXNUM (SECOND (args)));
      emit_comment (SSDATA (Fprin1_to_string (THIRD (args), Qnil)));
      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    comp.frame[slot_n],
				    gcc_jit_lvalue_as_rvalue (
				      gcc_jit_context_new_array_access (
				        comp.ctxt,
				        NULL,
				        comp.data_relocs,
				        reloc_n)));
    }
  else if (EQ (op, Qcomment))
    {
      /* Ex: (comment "Function: foo").	 */
      emit_comment((char *) SDATA (arg0));
    }
  else if (EQ (op, Qreturn))
    {
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_mvar_val (arg0));
    }
  else
    {
      error ("LIMPLE op inconsistent");
    }
}


/**************/
/* Inliners.  */
/**************/

static gcc_jit_rvalue *
emit_add1 (Lisp_Object insn)
{
  gcc_jit_rvalue *n = emit_mvar_val (SECOND (insn));
  return gcc_jit_context_new_call (comp.ctxt, NULL, comp.add1, 1, &n);
}

static gcc_jit_rvalue *
emit_sub1 (Lisp_Object insn)
{
  gcc_jit_rvalue *n = emit_mvar_val (SECOND (insn));
  return gcc_jit_context_new_call (comp.ctxt, NULL, comp.sub1, 1, &n);
}

static gcc_jit_rvalue *
emit_negate (Lisp_Object insn)
{
  gcc_jit_rvalue *n = emit_mvar_val (SECOND (insn));
  return gcc_jit_context_new_call (comp.ctxt, NULL, comp.negate, 1, &n);
}

static gcc_jit_rvalue *
emit_consp (Lisp_Object insn)
{
  gcc_jit_rvalue *x = emit_mvar_val (SECOND (insn));
  gcc_jit_rvalue *res = emit_cast (comp.bool_type,
				   emit_CONSP (x));
  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.bool_to_lisp_obj,
				   1, &res);
}

static gcc_jit_rvalue *
emit_car (Lisp_Object insn)
{
  gcc_jit_rvalue *x = emit_mvar_val (SECOND (insn));
  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.car,
				   1, &x);
}

static gcc_jit_rvalue *
emit_cdr (Lisp_Object insn)
{
  gcc_jit_rvalue *x = emit_mvar_val (SECOND (insn));
  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.cdr,
				   1, &x);
}

static gcc_jit_rvalue *
emit_setcar (Lisp_Object insn)
{
  gcc_jit_rvalue *args[] =
    { emit_mvar_val (SECOND (insn)),
      emit_mvar_val (THIRD (insn)) };
  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.setcar,
				   2, args);
}

static gcc_jit_rvalue *
emit_setcdr (Lisp_Object insn)
{
  gcc_jit_rvalue *args[] =
    { emit_mvar_val (SECOND (insn)),
      emit_mvar_val (THIRD (insn)) };
  return gcc_jit_context_new_call (comp.ctxt,
				   NULL,
				   comp.setcdr,
				   2, args);
}

static gcc_jit_rvalue *
emit_numperp (Lisp_Object insn)
{
  gcc_jit_rvalue *x = emit_mvar_val (SECOND (insn));
  gcc_jit_rvalue *res = emit_NUMBERP (x);
  return gcc_jit_context_new_call (comp.ctxt, NULL, comp.bool_to_lisp_obj, 1,
				   &res);
}

static gcc_jit_rvalue *
emit_integerp (Lisp_Object insn)
{
  gcc_jit_rvalue *x = emit_mvar_val (SECOND (insn));
  gcc_jit_rvalue *res = emit_INTEGERP (x);
  return gcc_jit_context_new_call (comp.ctxt, NULL, comp.bool_to_lisp_obj, 1,
				   &res);
}

/*
  Is not possibile to initilize static data in libgccjit therfore will create
  the following:

  char *str_name (void)
  {
    return "payload here";
  }
*/

static void
emit_literal_string_func (const char *str_name, const char *str)
{
  if (0) /* FIXME: somehow check gcc version here.  */
    {
      gcc_jit_function *f =
	gcc_jit_context_new_function (comp.ctxt, NULL,
				      GCC_JIT_FUNCTION_EXPORTED,
				      comp.char_ptr_type,
				      str_name,
				      0, NULL, 0);
      DECL_BLOCK (block, f);
      gcc_jit_rvalue *res = gcc_jit_context_new_string_literal (comp.ctxt, str);
      gcc_jit_block_end_with_return (block, NULL, res);
    } else
    {
      /* Horrible workaround for a funny bug:
	 https://gcc.gnu.org/ml/jit/2019-q3/msg00013.html
	 This will have to be used for all gccs pre gcc10 era. */
      size_t len = strlen (str);
      gcc_jit_type *a_type =
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.char_type,
					len + 1);
      gcc_jit_function *f =
	gcc_jit_context_new_function (comp.ctxt, NULL,
				      GCC_JIT_FUNCTION_EXPORTED,
				      gcc_jit_type_get_pointer (a_type),
				      str_name,
				      0, NULL, 0);
      DECL_BLOCK (block, f);
      gcc_jit_lvalue *arr =
	gcc_jit_context_new_global (comp.ctxt,
				    NULL,
				    GCC_JIT_GLOBAL_INTERNAL,
				    a_type,
				    format_string ("arr_%s", str_name));
      for (ptrdiff_t i = 0; i <= len; i++, str++)
	{
	  char c = i != len ? *str : 0;

	  gcc_jit_block_add_assignment (
	    block,
	    NULL,
	    gcc_jit_context_new_array_access (
	      comp.ctxt,
	      NULL,
	      gcc_jit_lvalue_as_rvalue (arr),
	      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						   comp.ptrdiff_type,
						   i)),
	    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						 comp.char_type,
						 c));
	}
      gcc_jit_rvalue *res = gcc_jit_lvalue_get_address (arr, NULL);
      gcc_jit_block_end_with_return (block, NULL, res);
    }
}

/*
  Declare as imported all the functions that are requested from the runtime.
  These are either subrs or not.
*/
static Lisp_Object
declare_runtime_imported (void)
{
  /* For subr imported by the runtime we rely on the standard mechanism in place
     for functions imported by lisp code. */
  FUNCALL1 (comp-add-subr-to-relocs, intern_c_string ("1+"));
  FUNCALL1 (comp-add-subr-to-relocs, intern_c_string ("1-"));
  FUNCALL1 (comp-add-subr-to-relocs, Qplus);
  FUNCALL1 (comp-add-subr-to-relocs, Qminus);
  FUNCALL1 (comp-add-subr-to-relocs, Qlist);

  Lisp_Object field_list = Qnil;
#define ADD_IMPORTED(f_name, ret_type, nargs, args)			       \
  {									       \
    Lisp_Object name = intern_c_string (f_name);			       \
    Lisp_Object field =							       \
      make_mint_ptr (declare_imported_func (name, ret_type, nargs, args));     \
    Lisp_Object el = Fcons (name, field);				       \
    field_list = Fcons (el, field_list);				       \
  } while (0)

  gcc_jit_type *args[4];

  ADD_IMPORTED ("wrong_type_argument", comp.void_type, 2, NULL);

  args[0] = comp.lisp_obj_type;
  args[1] = comp.int_type;
  ADD_IMPORTED ("helper_PSEUDOVECTOR_TYPEP_XUNTAG", comp.bool_type, 2, args);

  ADD_IMPORTED ("pure_write_error", comp.void_type, 1, NULL);

  args[0] = comp.lisp_obj_type;
  args[1] = comp.int_type;
  ADD_IMPORTED ("push_handler", comp.handler_ptr_type, 2, args);

  args[0] = gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.jmp_buf_s));
  ADD_IMPORTED (SETJMP_NAME, comp.int_type, 1, args);

  ADD_IMPORTED ("record_unwind_protect_excursion", comp.void_type, 0, NULL);

  args[0] = comp.lisp_obj_type;
  ADD_IMPORTED ("helper_unbind_n", comp.lisp_obj_type, 1, args);

  ADD_IMPORTED ("record_unwind_current_buffer", comp.void_type, 0, NULL);

  args[0] = args[1] = args[2] = comp.lisp_obj_type;
  args[3] = comp.int_type;
  ADD_IMPORTED ("set_internal", comp.void_type, 4, args);

#undef ADD_IMPORTED

  return field_list;
}

/*
This emit the code needed by every compilation unit to be loaded.
*/
static void
emit_ctxt_code (void)
{
  /* Imported objects.  */

  const char *d_reloc = SSDATA (FUNCALL1 (comp-ctxt-data-relocs, Vcomp_ctxt));
  EMACS_UINT d_reloc_len =
    XFIXNUM (FUNCALL1 (hash-table-count,
		       FUNCALL1 (comp-ctxt-data-relocs-idx, Vcomp_ctxt)));

  comp.data_relocs =
    gcc_jit_lvalue_as_rvalue(
      gcc_jit_context_new_global (
	comp.ctxt,
	NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	gcc_jit_context_new_array_type (comp.ctxt,
			    NULL,
			    comp.lisp_obj_type,
			    d_reloc_len),
	DATA_RELOC_SYM));

  emit_literal_string_func (TEXT_DATA_RELOC_SYM, d_reloc);

  /* Imported functions from non Lisp code.  */
  Lisp_Object f_runtime = declare_runtime_imported ();
  EMACS_UINT f_reloc_len = XFIXNUM (Flength (f_runtime));

  /* Imported subrs. */
  Lisp_Object f_subr = FUNCALL1 (comp-ctxt-func-relocs-l, Vcomp_ctxt);
  f_reloc_len += XFIXNUM (Flength (f_subr));

  gcc_jit_field *fields[f_reloc_len];
  Lisp_Object f_reloc_list = Qnil;
  int n_frelocs = 0;

  FOR_EACH_TAIL (f_runtime)
    {
      Lisp_Object el = XCAR (f_runtime);
      fields[n_frelocs++] = xmint_pointer( XCDR (el));
      f_reloc_list = Fcons (XCAR (el), f_reloc_list);
    }

  FOR_EACH_TAIL (f_subr)
    {
      Lisp_Object subr_sym = XCAR (f_subr);
      /* Ignore inliners. This are not real functions to be imported.  */
      if (NILP (Fgethash (subr_sym, comp.emitter_dispatcher, Qnil)))
	{
	  Lisp_Object subr = Fsymbol_function (subr_sym);
	  Lisp_Object maxarg = XCDR (Fsubr_arity (subr));
	  gcc_jit_field *field =
	    declare_imported_func (subr_sym, comp.lisp_obj_type,
				   FIXNUMP (maxarg) ? XFIXNUM (maxarg) : MANY, NULL);
	  fields [n_frelocs++] = field;
	  f_reloc_list = Fcons (subr_sym, f_reloc_list);
      }
    }

  Lisp_Object f_reloc_vec = make_vector (n_frelocs, Qnil);
  f_reloc_list = Freverse (f_reloc_list);
  ptrdiff_t i = 0;
  FOR_EACH_TAIL (f_reloc_list)
    {
      ASET (f_reloc_vec, i++, XCAR (f_reloc_list));
    }
  emit_literal_string_func (TEXT_IMPORTED_FUNC_RELOC_SYM,
			     (SSDATA (Fprin1_to_string (f_reloc_vec, Qnil))));

  gcc_jit_struct *f_reloc_struct =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "function_reloc_struct",
				     n_frelocs, fields);
  comp.func_relocs =
    gcc_jit_context_new_global (
      comp.ctxt,
      NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_struct_as_type (f_reloc_struct),
      IMPORTED_FUNC_RELOC_SYM);

  /* Exported functions info.  */
  const char *func_list = SSDATA (FUNCALL1 (comp-ctxt-funcs, Vcomp_ctxt));
  emit_literal_string_func (TEXT_EXPORTED_FUNC_RELOC_SYM, func_list);
}


/****************************************************************/
/* Inline function definition and lisp data structure follows.  */
/****************************************************************/

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

/* Opaque jmp_buf definition.  */

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

  DECL_BLOCK (entry_block, comp.check_type);
  DECL_BLOCK (ok_block, comp.check_type);
  DECL_BLOCK (not_ok_block, comp.check_type);

  comp.block = entry_block;
  comp.func = comp.check_type;

  emit_cond_jump (ok, ok_block, not_ok_block);

  gcc_jit_block_end_with_void_return (ok_block, NULL);

  comp.block = not_ok_block;

  gcc_jit_rvalue *wrong_type_args[] = { predicate, x };

  gcc_jit_block_add_eval (comp.block,
			  NULL,
			  emit_call (intern_c_string ("wrong_type_argument"),
				     comp.void_type, 2, wrong_type_args));

  gcc_jit_block_end_with_void_return (not_ok_block, NULL);
}

/* Define a substitute for CAR as always inlined function.  */

static void
define_CAR_CDR (void)
{
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
      DECL_BLOCK (entry_block, f);
      DECL_BLOCK (is_cons_b, f);
      DECL_BLOCK (not_a_cons_b, f);

      comp.block = entry_block;
      comp.func = f;

      emit_cond_jump (emit_CONSP (c), is_cons_b, not_a_cons_b);

      comp.block = is_cons_b;

      if (f == comp.car)
	gcc_jit_block_end_with_return (comp.block,
				       NULL,
				       emit_XCAR (c));
      else
	gcc_jit_block_end_with_return (comp.block,
				       NULL,
				       emit_XCDR (c));

      comp.block = not_a_cons_b;

      DECL_BLOCK (is_nil_b, f);
      DECL_BLOCK (not_nil_b, f);

      emit_cond_jump (emit_NILP (c), is_nil_b, not_nil_b);

      comp.block = is_nil_b;
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_lisp_obj_from_ptr (Qnil));

      comp.block = not_nil_b;
      gcc_jit_rvalue *wrong_type_args[] =
	{ emit_lisp_obj_from_ptr (Qlistp), c };

      gcc_jit_block_add_eval (comp.block,
			      NULL,
			      emit_call (intern_c_string ("wrong_type_argument"),
					 comp.void_type, 2, wrong_type_args));
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_lisp_obj_from_ptr (Qnil));
      f = comp.cdr;
      param = cdr_param;
    }
}

static void
define_setcar_setcdr (void)
{
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
      DECL_BLOCK (entry_block, *f_ref);
      comp.func = *f_ref;
      comp.block = entry_block;

      /* CHECK_CONS (cell); */
      emit_CHECK_CONS (gcc_jit_param_as_rvalue (cell));

      /* CHECK_IMPURE (cell, XCONS (cell)); */
      gcc_jit_rvalue *args[] =
	{ gcc_jit_param_as_rvalue (cell),
	  emit_XCONS (gcc_jit_param_as_rvalue (cell)) };

      gcc_jit_block_add_eval (
			      entry_block,
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
      gcc_jit_block_end_with_return (entry_block,
				     NULL,
				     gcc_jit_param_as_rvalue (new_el));
    }
}

/*
   Define a substitute for Fadd1 Fsub1.
   Currently expose just fixnum arithmetic.
*/

static void
define_add1_sub1 (void)
{
  gcc_jit_block *bb_orig = comp.block;

  gcc_jit_function *func[2];
  char const *f_name[] = {"add1", "sub1"};
  char const *fall_back_func[] = {"1+", "1-"};
  gcc_jit_rvalue *compare[] =
    { comp.most_positive_fixnum, comp.most_negative_fixnum };
  enum gcc_jit_binary_op op[] =
    { GCC_JIT_BINARY_OP_PLUS, GCC_JIT_BINARY_OP_MINUS };
  for (int i = 0; i < 2; i++)
    {
      gcc_jit_param *param = gcc_jit_context_new_param (comp.ctxt,
							NULL,
							comp.lisp_obj_type,
							"n");
      comp.func = func[i] =
	gcc_jit_context_new_function (comp.ctxt, NULL,
				      GCC_JIT_FUNCTION_ALWAYS_INLINE,
				      comp.lisp_obj_type,
				      f_name[i],
				      1,
				      &param,
				      0);
      DECL_BLOCK (entry_block, func[i]);
      DECL_BLOCK (inline_block, func[i]);
      DECL_BLOCK (fcall_block, func[i]);

      comp.block = entry_block;

      /* (FIXNUMP (n) && XFIXNUM (n) != MOST_POSITIVE_FIXNUM
	 ? (XFIXNUM (n) + 1)
	 : Fadd1 (n)) */

      gcc_jit_rvalue *n = gcc_jit_param_as_rvalue (param);
      gcc_jit_rvalue *n_fixnum = emit_XFIXNUM (n);

      emit_cond_jump (
	gcc_jit_context_new_binary_op (
	  comp.ctxt,
	  NULL,
	  GCC_JIT_BINARY_OP_LOGICAL_AND,
	  comp.bool_type,
	  emit_cast (comp.bool_type,
		 emit_FIXNUMP (n)),
	  gcc_jit_context_new_comparison (comp.ctxt,
					  NULL,
					  GCC_JIT_COMPARISON_NE,
					  n_fixnum,
					  compare[i])),
	inline_block,
	fcall_block);

      comp.block = inline_block;
      gcc_jit_rvalue *inline_res =
	gcc_jit_context_new_binary_op (comp.ctxt,
				       NULL,
				       op[i],
				       comp.emacs_int_type,
				       n_fixnum,
				       comp.one);

      gcc_jit_block_end_with_return (inline_block,
				     NULL,
				     emit_make_fixnum (inline_res));

      comp.block = fcall_block;
      gcc_jit_rvalue *call_res = emit_call (intern_c_string (fall_back_func[i]),
					    comp.lisp_obj_type, 1, &n);
      gcc_jit_block_end_with_return (fcall_block,
				     NULL,
				     call_res);
    }
  comp.block = bb_orig;
  comp.add1 = func[0];
  comp.sub1 = func[1];
}

static void
define_negate (void)
{
  gcc_jit_block *bb_orig = comp.block;

  gcc_jit_param *param[] =
    { gcc_jit_context_new_param (comp.ctxt,
				 NULL,
				 comp.lisp_obj_type,
				 "n") };

  comp.func = comp.negate =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_ALWAYS_INLINE,
				  comp.lisp_obj_type,
				  "negate",
				  1,
				  param,
				  0);

  DECL_BLOCK (entry_block, comp.negate);
  DECL_BLOCK (inline_block, comp.negate);
  DECL_BLOCK (fcall_block, comp.negate);

  comp.block = entry_block;

  /* (FIXNUMP (TOP) && XFIXNUM (TOP) != MOST_NEGATIVE_FIXNUM
		 ? make_fixnum (- XFIXNUM (TOP))
		 : Fminus (1, &TOP)) */

  gcc_jit_lvalue *n = gcc_jit_param_as_lvalue (param[0]);
  gcc_jit_rvalue *n_fixnum =
    emit_XFIXNUM (gcc_jit_lvalue_as_rvalue (n));

  emit_cond_jump (
    gcc_jit_context_new_binary_op (
      comp.ctxt,
      NULL,
      GCC_JIT_BINARY_OP_LOGICAL_AND,
      comp.bool_type,
      emit_cast (comp.bool_type,
		 emit_FIXNUMP (gcc_jit_lvalue_as_rvalue (n))),
      gcc_jit_context_new_comparison (comp.ctxt,
				      NULL,
				      GCC_JIT_COMPARISON_NE,
				      n_fixnum,
				      comp.most_negative_fixnum)),
    inline_block,
    fcall_block);

  comp.block = inline_block;
  gcc_jit_rvalue *inline_res =
    gcc_jit_context_new_unary_op (comp.ctxt,
				  NULL,
				  GCC_JIT_UNARY_OP_MINUS,
				  comp.emacs_int_type,
				  n_fixnum);

  gcc_jit_block_end_with_return (inline_block,
				 NULL,
				 emit_make_fixnum (inline_res));

  comp.block = fcall_block;
  gcc_jit_rvalue *call_res = emit_call_ref (Qminus, 1, n);
  gcc_jit_block_end_with_return (fcall_block,
				 NULL,
				 call_res);
  comp.block = bb_orig;
}

/* Define a substitute for PSEUDOVECTORP as always inlined function.  */

static void
define_PSEUDOVECTORP (void)
{
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

  DECL_BLOCK (entry_block, comp.pseudovectorp);
  DECL_BLOCK (ret_false_b, comp.pseudovectorp);
  DECL_BLOCK (call_pseudovector_typep_b, comp.pseudovectorp);

  comp.block = entry_block;
  comp.func = comp.pseudovectorp;

  emit_cond_jump (emit_VECTORLIKEP (gcc_jit_param_as_rvalue (param[0])),
		  call_pseudovector_typep_b,
		  ret_false_b);

  comp.block = ret_false_b;
  gcc_jit_block_end_with_return (ret_false_b,
				 NULL,
				 gcc_jit_context_new_rvalue_from_int(
				   comp.ctxt,
				   comp.bool_type,
				   false));

  gcc_jit_rvalue *args[] =
    { gcc_jit_param_as_rvalue (param[0]),
      gcc_jit_param_as_rvalue (param[1]) };
  comp.block = call_pseudovector_typep_b;
  /* FIXME use XUNTAG now that's available.  */
  gcc_jit_block_end_with_return (
    call_pseudovector_typep_b,
    NULL,
    emit_call (intern_c_string ("helper_PSEUDOVECTOR_TYPEP_XUNTAG"),
	       comp.bool_type,
	       2,
	       args));
}

static void
define_CHECK_IMPURE (void)
{
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

    DECL_BLOCK (entry_block, comp.check_impure);
    DECL_BLOCK (err_block, comp.check_impure);
    DECL_BLOCK (ok_block, comp.check_impure);

    comp.block = entry_block;
    comp.func = comp.check_impure;

    emit_cond_jump (emit_PURE_P (gcc_jit_param_as_rvalue (param[0])), /* FIXME */
		    err_block,
		    ok_block);
    gcc_jit_block_end_with_void_return (ok_block, NULL);

    gcc_jit_rvalue *pure_write_error_arg =
      gcc_jit_param_as_rvalue (param[0]);

    comp.block = err_block;
    gcc_jit_block_add_eval (comp.block,
			    NULL,
			    emit_call (intern_c_string ("pure_write_error"),
				       comp.void_type, 1,
				       &pure_write_error_arg));

    gcc_jit_block_end_with_void_return (err_block, NULL);
}

/* Define a function to convert boolean into t or nil */

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
  DECL_BLOCK (entry_block, comp.bool_to_lisp_obj);
  DECL_BLOCK (ret_t_block, comp.bool_to_lisp_obj);
  DECL_BLOCK (ret_nil_block, comp.bool_to_lisp_obj);
  comp.block = entry_block;
  comp.func = comp.bool_to_lisp_obj;

  emit_cond_jump (gcc_jit_param_as_rvalue (param),
		  ret_t_block,
		  ret_nil_block);

  comp.block = ret_t_block;
  gcc_jit_block_end_with_return (ret_t_block,
				 NULL,
				 emit_lisp_obj_from_ptr (Qt));

  comp.block = ret_nil_block;
  gcc_jit_block_end_with_return (ret_nil_block,
				 NULL,
				 emit_lisp_obj_from_ptr (Qnil));

}

static void
compile_function (Lisp_Object func)
{
  char *c_name = (char *) SDATA (FUNCALL1 (comp-func-c-func-name, func));
  Lisp_Object args = FUNCALL1 (comp-func-args, func);
  EMACS_INT frame_size = XFIXNUM (FUNCALL1 (comp-func-frame-size, func));
  bool ncall = (FUNCALL1 (comp-nargs-p, args));

  if (!ncall)
    {
      EMACS_INT max_args = XFIXNUM (FUNCALL1 (comp-args-max, args));
      comp.func
	= declare_exported_func (c_name, comp.lisp_obj_type, max_args, NULL);
    }
  else
    {
      gcc_jit_param *param[] =
	{ gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.ptrdiff_type,
				     "nargs"),
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.lisp_obj_ptr_type,
				     "args") };
      comp.func =
	gcc_jit_context_new_function (comp.ctxt,
				      NULL,
				      GCC_JIT_FUNCTION_EXPORTED,
				      comp.lisp_obj_type,
				      c_name, 2, param, 0);
    }

  gcc_jit_lvalue *frame_array =
    gcc_jit_function_new_local (
      comp.func,
      NULL,
      gcc_jit_context_new_array_type (comp.ctxt,
				      NULL,
				      comp.lisp_obj_type,
				      frame_size),
      "local");

  gcc_jit_lvalue *frame[frame_size];
  for (int i = 0; i < frame_size; ++i)
    frame[i] =
      gcc_jit_context_new_array_access (
        comp.ctxt,
	NULL,
	gcc_jit_lvalue_as_rvalue (frame_array),
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.int_type,
					     i));
  comp.frame = frame;

  comp.func_blocks = CALLN (Fmake_hash_table);

  /* Pre declare all basic blocks to gcc.
     The "entry" block must be declared as first.  */
  declare_block (Qentry);
  Lisp_Object blocks = FUNCALL1 (comp-func-blocks, func);
  Lisp_Object entry_block = Fgethash (Qentry, blocks, Qnil);
  struct Lisp_Hash_Table *ht = XHASH_TABLE (blocks);
  for (ptrdiff_t i = 0; i < ht->count; i++)
    {
      Lisp_Object block = HASH_VALUE (ht, i);
      if (!EQ (block, entry_block))
	declare_block (HASH_KEY (ht, i));
    }

  for (ptrdiff_t i = 0; i < ht->count; i++)
    {
      Lisp_Object block_name = HASH_KEY (ht, i);
      Lisp_Object block = HASH_VALUE (ht, i);
      Lisp_Object insns = FUNCALL1 (comp-block-insns, block);

      comp.block = retrive_block (block_name);
      while (CONSP (insns))
	{
	  Lisp_Object insn = XCAR (insns);
	  emit_limple_insn (insn);
	  insns = XCDR (insns);
	}
    }
}


/**********************************/
/* Entry points exposed to lisp.  */
/**********************************/

DEFUN ("comp--init-ctxt", Fcomp__init_ctxt, Scomp__init_ctxt,
       0, 0, 0,
       doc: /* Initialize the native compiler context. Return t on success.  */)
     (void)
{
  if (comp.ctxt)
    {
      error ("Compiler context already taken");
      return Qnil;
    }

  if (NILP (comp.emitter_dispatcher))
    {
      /* Move this into syms_of_comp the day will be dumpable.   */
      comp.emitter_dispatcher = CALLN (Fmake_hash_table);
      register_emitter (Qset_internal, emit_set_internal);
      register_emitter (Qhelper_unbind_n, emit_simple_limple_call_lisp_ret);
      register_emitter (Qhelper_unwind_protect,
			emit_simple_limple_call_void_ret);
      register_emitter (Qrecord_unwind_current_buffer,
			emit_simple_limple_call_lisp_ret);
      register_emitter (Qrecord_unwind_protect_excursion,
			emit_simple_limple_call_void_ret);
      register_emitter (Qhelper_save_restriction,
			emit_simple_limple_call_void_ret);
      /* Inliners.  */
      register_emitter (QFadd1, emit_add1);
      register_emitter (QFsub1, emit_sub1);
      register_emitter (QFconsp, emit_consp);
      register_emitter (QFcar, emit_car);
      register_emitter (QFcdr, emit_cdr);
      register_emitter (QFsetcar, emit_setcar);
      register_emitter (QFsetcdr, emit_setcdr);
      register_emitter (Qnegate, emit_negate);
      register_emitter (QFnumberp, emit_numperp);
      register_emitter (QFintegerp, emit_integerp);
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

  comp.lisp_obj_as_num = gcc_jit_context_new_field (comp.ctxt,
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

  /*
    Always reinitialize this cause old function definitions are garbage collected
    by libgccjit when the ctxt is released.
  */
  comp.func_hash = CALLN (Fmake_hash_table);

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

  return Qt;
}

DEFUN ("comp--release-ctxt", Fcomp__release_ctxt, Scomp__release_ctxt,
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

DEFUN ("comp--compile-ctxt-to-file", Fcomp__compile_ctxt_to_file,
       Scomp__compile_ctxt_to_file,
       1, 1, 0,
       doc: /* Compile as native code the current context to file.  */)
     (Lisp_Object ctxtname)
{
  CHECK_STRING (ctxtname);

  gcc_jit_context_set_int_option (comp.ctxt,
				  GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
				  comp_speed);
  /* Gcc doesn't like being interrupted at all.  */
  sigset_t oldset;
  sigset_t blocked;
  sigemptyset (&blocked);
  sigaddset (&blocked, SIGALRM);
  sigaddset (&blocked, SIGINT);
  sigaddset (&blocked, SIGIO);
  pthread_sigmask (SIG_BLOCK, &blocked, &oldset);

  emit_ctxt_code ();

  /* Define inline functions.  */
  define_CAR_CDR();
  define_PSEUDOVECTORP ();
  define_CHECK_TYPE ();
  define_CHECK_IMPURE ();
  define_bool_to_lisp_obj ();
  define_setcar_setcdr ();
  define_add1_sub1 ();
  define_negate ();

  /* Compile all functions. Can't be done before because the
     relocation structs has to be already defined.  */
  struct Lisp_Hash_Table *func_h
    = XHASH_TABLE (FUNCALL1 (comp-ctxt-funcs-h, Vcomp_ctxt));
  for (ptrdiff_t i = 0; i < func_h->count; i++)
    compile_function (HASH_VALUE (func_h, i));

  /* FIXME use format_string here  */
  if (COMP_DEBUG)
    {
      AUTO_STRING (dot_c, ".c");
      const char *filename =
	(const char *) SDATA (CALLN (Fconcat, ctxtname, dot_c));
      gcc_jit_context_dump_to_file (comp.ctxt, filename, 1);
    }

  AUTO_STRING (dot_so, NATIVE_ELISP_SUFFIX);
  const char *filename =
    (const char *) SDATA (CALLN (Fconcat, ctxtname, dot_so));

  gcc_jit_context_compile_to_file (comp.ctxt,
				   GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY,
				   filename);

  pthread_sigmask (SIG_SETMASK, &oldset, 0);

  return Qt;
}

/* DEFUN ("comp-compile-and-load-ctxt", Fcomp_compile_and_load_ctxt, */
/*        Scomp_compile_and_load_ctxt, */
/*        0, 1, 0, */
/*        doc: /\* Compile as native code the current context and load its */
/* 	       functions.  *\/) */
/*      (Lisp_Object disassemble) */
/* { */
/*   gcc_jit_context_set_int_option (comp.ctxt, */
/* 				  GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, */
/* 				  comp_speed); */
/*   /\* Gcc doesn't like being interrupted at all.  *\/ */
/*   sigset_t oldset; */
/*   sigset_t blocked; */
/*   sigemptyset (&blocked); */
/*   sigaddset (&blocked, SIGALRM); */
/*   sigaddset (&blocked, SIGINT); */
/*   sigaddset (&blocked, SIGIO); */
/*   pthread_sigmask (SIG_BLOCK, &blocked, &oldset); */

/*   if (COMP_DEBUG) */
/*     gcc_jit_context_dump_to_file (comp.ctxt, "gcc-ctxt-dump.c", 1); */
/*   gcc_jit_result *gcc_res = gcc_jit_context_compile(comp.ctxt); */

/*   if (!NILP (disassemble)) */
/*     gcc_jit_context_compile_to_file (comp.ctxt, */
/* 				     GCC_JIT_OUTPUT_KIND_ASSEMBLER, */
/* 				     "gcc-ctxt-dump.s"); */

/*   while (CONSP (comp.funcs)) */
/*     { */
/*       union Aligned_Lisp_Subr *x = xmalloc (sizeof (union Aligned_Lisp_Subr)); */
/*       Lisp_Object func = XCAR (comp.funcs); */
/*       Lisp_Object args = FUNCALL1 (comp-func-args, func); */
/*       char *symbol_name = */
/* 	(char *) SDATA (SYMBOL_NAME (FUNCALL1 (comp-func-symbol-name, func))); */
/*       char *c_name = (char *) SDATA (FUNCALL1 (comp-func-c-func-name, func)); */

/*       x->s.header.size = PVEC_SUBR << PSEUDOVECTOR_AREA_BITS; */
/*       x->s.function.a0 = gcc_jit_result_get_code(gcc_res, c_name); */
/*       eassert (x->s.function.a0); */
/*       x->s.min_args = XFIXNUM (FUNCALL1 (comp-args-base-min, args)); */
/*       if (FUNCALL1 (comp-args-p, args)) */
/* 	x->s.max_args = XFIXNUM (FUNCALL1 (comp-args-max, args)); */
/*       else */
/* 	x->s.max_args = MANY; */
/*       x->s.symbol_name = symbol_name; */
/*       defsubr(x); */

/*       comp.funcs = XCDR (comp.funcs); */
/*     } */

/*   pthread_sigmask (SIG_SETMASK, &oldset, 0); */

/*   return Qt; */
/* } */


/******************************************************************************/
/* Helper functions called from the runtime.				      */
/* These can't be statics till shared mechanism is used to solve relocations. */
/* Note: this are all potentially definable directly to gcc and are here just */
/* for lazyness. Change this if a performance impact is measured.             */
/******************************************************************************/

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

void
helper_unwind_protect (Lisp_Object handler)
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
helper_unbind_n (Lisp_Object n)
{
  return unbind_to (SPECPDL_INDEX () - XFIXNUM (n), Qnil);
}

bool
helper_PSEUDOVECTOR_TYPEP_XUNTAG (const union vectorlike_header *a,
				  enum pvec_type code)
{
  return PSEUDOVECTOR_TYPEP (XUNTAG (a, Lisp_Vectorlike,
				     union vectorlike_header),
			     code);
}

void
helper_emit_save_restriction (void)
{
  record_unwind_protect (save_restriction_restore,
			 save_restriction_save ());
}

void
helper_set_data_relocs (Lisp_Object *d_relocs_vec, char const *relocs)
{
}


/*********************************/
/* Native elisp load functions.  */
/*********************************/

static Lisp_Object Vnative_elisp_refs_hash;

typedef char *(*comp_litt_str_func) (void);

static void
prevent_gc (Lisp_Object obj)
{
  Fputhash (obj, Qt, Vnative_elisp_refs_hash);
}

static Lisp_Object
retrive_literal_obj (dynlib_handle_ptr handle, const char *str_name)
{
  comp_litt_str_func f = dynlib_sym (handle, str_name);
  eassert (f);
  char *res = f();
  return Fread (build_string (res));
}

static int
load_comp_unit (dynlib_handle_ptr handle)
{
  /* Imported data.  */
  Lisp_Object *data_relocs = dynlib_sym (handle, DATA_RELOC_SYM);

  Lisp_Object d_vec = retrive_literal_obj (handle, TEXT_DATA_RELOC_SYM);
  EMACS_UINT d_vec_len = XFIXNUM (Flength (d_vec));

  for (EMACS_UINT i = 0; i < d_vec_len; i++)
    {
      data_relocs[i] = AREF (d_vec, i);
      prevent_gc (data_relocs[i]);
    }

  /* Imported functions.  */
  Lisp_Object (**f_relocs)(void) =
    dynlib_sym (handle, IMPORTED_FUNC_RELOC_SYM);
  Lisp_Object f_vec =
    retrive_literal_obj (handle, TEXT_IMPORTED_FUNC_RELOC_SYM);
  EMACS_UINT f_vec_len = XFIXNUM (Flength (f_vec));
    for (EMACS_UINT i = 0; i < f_vec_len; i++)
    {
      Lisp_Object f_sym = AREF (f_vec, i);
      char *f_str = SSDATA (SYMBOL_NAME (f_sym));
      Lisp_Object subr = Fsymbol_function (f_sym);
      if (!NILP (subr))
	{
	  eassert (SUBRP (subr));
	  f_relocs[i] = XSUBR (subr)->function.a0;
	} else if (!strcmp (f_str, "wrong_type_argument"))
	{
	  f_relocs[i] = (void *) wrong_type_argument;
	} else if (!strcmp (f_str, "helper_PSEUDOVECTOR_TYPEP_XUNTAG"))
	{
	  f_relocs[i] = (void *) helper_PSEUDOVECTOR_TYPEP_XUNTAG;
	} else if (!strcmp (f_str, "pure_write_error"))
	{
	  f_relocs[i] = (void *) pure_write_error;
	} else if (!strcmp (f_str, "push_handler"))
	{
	  f_relocs[i] = (void *) push_handler;
	} else if (!strcmp (f_str, SETJMP_NAME))
	{
	  f_relocs[i] = (void *) SETJMP;
	} else if (!strcmp (f_str, "record_unwind_protect_excursion"))
	{
	  f_relocs[i] = (void *) record_unwind_protect_excursion;
	} else if (!strcmp (f_str, "helper_unbind_n"))
	{
	  f_relocs[i] = (void *) helper_unbind_n;
	} else if (!strcmp (f_str, "record_unwind_current_buffer"))
	{
	  f_relocs[i] = (void *) record_unwind_current_buffer;
	} else if (!strcmp (f_str, "set_internal"))
	{
	  f_relocs[i] = (void *) set_internal;
	} else
	{
	  error ("Unexpected function relocation %s", f_str);
	}
    }

  /* Exported functions.  */
  Lisp_Object func_list = retrive_literal_obj (handle, TEXT_EXPORTED_FUNC_RELOC_SYM);

  while (func_list)
    {
      Lisp_Object el = XCAR (func_list);
      Lisp_Object Qsym = AREF (el, 0);
      char *c_func_name = SSDATA (AREF (el, 1));
      Lisp_Object args = AREF (el, 2);
      ptrdiff_t minargs = XFIXNUM (XCAR (args));
      ptrdiff_t maxargs = FIXNUMP (XCDR (args)) ? XFIXNUM (XCDR (args)) : MANY;
      /* char *doc = SSDATA (AREF (el, 3)); */
      void *func = dynlib_sym (handle, c_func_name);
      eassert (func);

      union Aligned_Lisp_Subr *x = xmalloc (sizeof (union Aligned_Lisp_Subr));
      x->s.header.size = PVEC_SUBR << PSEUDOVECTOR_AREA_BITS;
      x->s.function.a0 = func;
      x->s.min_args = minargs;
      x->s.max_args = maxargs;
      x->s.symbol_name = SSDATA (Fsymbol_name (Qsym));
      defsubr(x);

      func_list = XCDR (func_list);
    }

  return 0;
}

/* Load related routines. */
DEFUN ("native-elisp-load", Fnative_elisp_load, Snative_elisp_load, 1, 1, 0,
       doc: /* Load native elisp code FILE.  */)
  (Lisp_Object file)
{
  dynlib_handle_ptr handle;

  CHECK_STRING (file);
  handle = dynlib_open (SSDATA (file));
  if (!handle)
    xsignal2 (Qcomp_unit_open_failed, file, build_string (dynlib_error ()));

  int r = load_comp_unit (handle);

  if (r != 0)
    xsignal2 (Qcomp_unit_init_failed, file, INT_TO_INTEGER (r));

  return Qt;
}


void
syms_of_comp (void)
{
  staticpro (&Vnative_elisp_refs_hash);
  Vnative_elisp_refs_hash
    = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
		       DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
		       Qnil, false);

  /* Limple instruction set.  */
  DEFSYM (Qcomment, "comment");
  DEFSYM (Qjump, "jump");
  DEFSYM (Qcall, "call");
  DEFSYM (Qcallref, "callref");
  DEFSYM (Qncall, "ncall");
  DEFSYM (Qsetimm, "setimm");
  DEFSYM (Qreturn, "return");
  DEFSYM (Qcomp_mvar, "comp-mvar");
  DEFSYM (Qcond_jump, "cond-jump");
  /* In use for prologue emission.  */
  DEFSYM (Qset_par_to_local, "set-par-to-local");
  DEFSYM (Qset_args_to_local, "set-args-to-local");
  DEFSYM (Qset_rest_args_to_local, "set-rest-args-to-local");
  DEFSYM (Qinc_args, "inc-args");
  DEFSYM (Qcond_jump_narg_leq, "cond-jump-narg-leq");
  /* Others.  */
  DEFSYM (Qpush_handler, "push-handler");
  DEFSYM (Qpop_handler, "pop-handler");
  DEFSYM (Qcondition_case, "condition-case");
  /* call operands.  */
  DEFSYM (Qcatcher, "catcher"); /* FIXME use these allover.  */
  DEFSYM (Qentry, "entry");
  DEFSYM (Qset_internal, "set_internal");
  DEFSYM (Qrecord_unwind_current_buffer, "record_unwind_current_buffer");
  DEFSYM (Qrecord_unwind_protect_excursion, "record_unwind_protect_excursion");
  DEFSYM (Qhelper_unbind_n, "helper_unbind_n");
  DEFSYM (Qhelper_unwind_protect, "helper_unwind_protect");
  DEFSYM (Qhelper_save_restriction, "helper_save_restriction");
  /* Inliners.  */
  DEFSYM (QFadd1, "Fadd1");
  DEFSYM (QFsub1, "Fsub1");
  DEFSYM (QFconsp, "Fconsp");
  DEFSYM (QFcar, "Fcar");
  DEFSYM (QFcdr, "Fcdr");
  DEFSYM (QFsetcar, "Fsetcar");
  DEFSYM (QFsetcdr, "Fsetcdr");
  DEFSYM (Qnegate, "negate");
  DEFSYM (QFnumberp, "Fnumberp");
  DEFSYM (QFintegerp, "Fintegerp");
  /* Returned values.  */
  DEFSYM (Qcomp_unit_open_failed, "comp-unit-open-failed");
  DEFSYM (Qcomp_unit_init_failed, "comp-unit-init-failed");

  defsubr (&Scomp__init_ctxt);
  defsubr (&Scomp__release_ctxt);
  defsubr (&Scomp__compile_ctxt_to_file);
  defsubr (&Snative_elisp_load);

  staticpro (&comp.func_hash);
  comp.func_hash = Qnil;
  staticpro (&comp.func_blocks);
  staticpro (&comp.emitter_dispatcher);
  comp.emitter_dispatcher = Qnil;

  DEFVAR_INT ("comp-speed", comp_speed,
	      doc: /* From 0 to 3.  */);
  DEFVAR_LISP ("comp-ctxt", Vcomp_ctxt,
	       doc: /*
		     The compiler context. */);
  Vcomp_ctxt = Qnil;

  comp_speed = DEFAULT_SPEED;
}

#endif /* HAVE_LIBGCCJIT */
