/* Compile elisp into native code.
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

#ifdef HAVE_NATIVE_COMP

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <libgccjit.h>

#include <sys/types.h> /* For getpid.  */
#include <unistd.h>
#include <sys/stat.h> /* For O_RDONLY.  */
#include <fcntl.h>
/* FIXME non portable.  */
#include <sys/mman.h> /* For memfd_create.  */

#include "lisp.h"
#include "puresize.h"
#include "window.h"
#include "dynlib.h"
#include "buffer.h"
#include "blockinput.h"

/* C symbols emitted for the load relocation mechanism.  */
#define CURRENT_THREAD_RELOC_SYM "current_thread_reloc"
#define PURE_RELOC_SYM "pure_reloc"
#define DATA_RELOC_SYM "d_reloc"
#define IMPORTED_FUNC_LINK_TABLE "freloc_link_table"
#define TEXT_DATA_RELOC_SYM "text_data_reloc"

#define SPEED XFIXNUM (Fsymbol_value (Qcomp_speed))
#define COMP_DEBUG XFIXNUM (Fsymbol_value (Qcomp_debug))

#define STR_VALUE(s) #s
#define STR(s) STR_VALUE (s)

#define FIRST(x)				\
  XCAR(x)
#define SECOND(x)				\
  XCAR (XCDR (x))
#define THIRD(x)				\
  XCAR (XCDR (XCDR (x)))

/* Like call1 but stringify and intern.  */
#define CALL1I(fun, arg)				\
  CALLN (Ffuncall, intern_c_string (STR (fun)), arg)

#define DECL_BLOCK(name, func)				\
  gcc_jit_block *(name) =				\
    gcc_jit_function_new_block ((func), STR (name))

#ifdef HAVE__SETJMP
#define SETJMP _setjmp
#else
#define SETJMP setjmp
#endif
#define SETJMP_NAME SETJMP

/* Max number function importable by native compiled code.  */
#define F_RELOC_MAX_SIZE 1500

typedef struct {
  void *link_table[F_RELOC_MAX_SIZE];
  ptrdiff_t size;
} f_reloc_t;

static f_reloc_t freloc;

/* C side of the compiler context.  */

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
  gcc_jit_lvalue *loc_handler;
  /* struct thread_state.  */
  gcc_jit_struct *thread_state_s;
  gcc_jit_field *m_handlerlist;
  gcc_jit_type *thread_state_ptr_type;
  gcc_jit_rvalue *current_thread_ref;
  /* Other globals.  */
  gcc_jit_rvalue *pure_ref;
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
  gcc_jit_lvalue **f_frame; /* "Floating" frame for the current function.  */
  gcc_jit_lvalue *scratch; /* Used as scratch slot for some code sequence (switch).  */
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
  Lisp_Object func_blocks_h; /* blk_name -> gcc_block.  */
  Lisp_Object exported_funcs_h; /* subr_name -> gcc_jit_function *.  */
  Lisp_Object imported_funcs_h; /* subr_name -> gcc_jit_field *reloc_field.  */
  Lisp_Object emitter_dispatcher;
  gcc_jit_rvalue *data_relocs; /* Synthesized struct holding data relocs.  */
  gcc_jit_lvalue *func_relocs; /* Synthesized struct holding func relocs.  */
} comp_t;

static comp_t comp;

FILE *logfile = NULL;

/* This is used for serialized objects by the reload mechanism.  */
typedef struct {
  ptrdiff_t len;
  const char data[];
} static_obj_t;


/*
   Helper functions called by the run-time.
*/
Lisp_Object helper_save_window_excursion (Lisp_Object v1);
void helper_unwind_protect (Lisp_Object handler);
Lisp_Object helper_temp_output_buffer_setup (Lisp_Object x);
Lisp_Object helper_unbind_n (Lisp_Object n);
void helper_save_restriction (void);
bool helper_PSEUDOVECTOR_TYPEP_XUNTAG (Lisp_Object a, enum pvec_type code);

void *helper_link_table[] =
  { wrong_type_argument,
    helper_PSEUDOVECTOR_TYPEP_XUNTAG,
    pure_write_error,
    push_handler,
    SETJMP_NAME,
    record_unwind_protect_excursion,
    helper_unbind_n,
    helper_save_restriction,
    record_unwind_current_buffer,
    set_internal,
    helper_unwind_protect,
    specbind };


static char * ATTRIBUTE_FORMAT_PRINTF (1, 2)
format_string (const char *format, ...)
{
  static char scratch_area[512];
  va_list va;
  va_start (va, format);
  int res = vsnprintf (scratch_area, sizeof (scratch_area), format, va);
  if (res >= sizeof (scratch_area))
    {
      scratch_area[sizeof (scratch_area) - 4] = '.';
      scratch_area[sizeof (scratch_area) - 3] = '.';
      scratch_area[sizeof (scratch_area) - 2] = '.';
    }
  va_end (va);
  return scratch_area;
}

static void
bcall0 (Lisp_Object f)
{
  Ffuncall (1, &f);
}

static gcc_jit_field *
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
    xsignal1 (Qnative_ice, build_string ("unsupported cast"));

  return field;
}

static gcc_jit_block *
retrive_block (Lisp_Object block_name)
{
  Lisp_Object value = Fgethash (block_name, comp.func_blocks_h, Qnil);

  if (NILP (value))
    xsignal1 (Qnative_ice, build_string ("missing basic block"));

  return (gcc_jit_block *) xmint_pointer (value);
}

static void
declare_block (Lisp_Object block_name)
{
  char *name_str = SSDATA (SYMBOL_NAME (block_name));
  gcc_jit_block *block = gcc_jit_function_new_block (comp.func, name_str);
  Lisp_Object value = make_mint_ptr (block);

  if (!NILP (Fgethash (block_name, comp.func_blocks_h, Qnil)))
    xsignal1 (Qnative_ice, build_string ("double basic block declaration"));

  Fputhash (block_name, value, comp.func_blocks_h);
}

static gcc_jit_lvalue *
get_slot (Lisp_Object mvar)
{
  Lisp_Object mvar_slot = CALL1I (comp-mvar-slot, mvar);

  if (EQ (mvar_slot, Qscratch))
    {
      if (!comp.scratch)
	comp.scratch = gcc_jit_function_new_local (comp.func,
						   NULL,
						   comp.lisp_obj_type,
						   "scratch");
      return comp.scratch;
    }
  EMACS_INT slot_n = XFIXNUM (mvar_slot);
  gcc_jit_lvalue **frame =
    (CALL1I (comp-mvar-ref, mvar) || SPEED < 2)
    ? comp.frame : comp.f_frame;
  return frame[slot_n];
}

static void
register_emitter (Lisp_Object key, void *func)
{
  Lisp_Object value = make_mint_ptr (func);
  Fputhash (key, value, comp.emitter_dispatcher);
}

static void
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
  When types is NULL args are assumed to be all Lisp_Objects.
*/
static gcc_jit_field *
declare_imported_func (Lisp_Object subr_sym, gcc_jit_type *ret_type,
		       int nargs, gcc_jit_type **types)
{
  USE_SAFE_ALLOCA;
  /* Don't want to declare the same function two times.  */
  if (!NILP (Fgethash (subr_sym, comp.imported_funcs_h, Qnil)))
    xsignal2 (Qnative_ice,
	      build_string ("unexpected double function declaration"),
	      subr_sym);

  if (nargs == MANY)
    {
      nargs = 2;
      types = SAFE_ALLOCA (nargs * sizeof (* types));
      types[0] = comp.ptrdiff_type;
      types[1] = comp.lisp_obj_ptr_type;
    }
  else if (nargs == UNEVALLED)
    {
      nargs = 1;
      types = SAFE_ALLOCA (nargs * sizeof (* types));
      types[0] = comp.lisp_obj_type;
    }
  else if (!types)
    {
      types = SAFE_ALLOCA (nargs * sizeof (* types));
      for (ptrdiff_t i = 0; i < nargs; i++)
	types[i] = comp.lisp_obj_type;
    }

  /* String containing the function ptr name.  */
  Lisp_Object f_ptr_name =
    CALLN (Ffuncall, intern_c_string (STR (comp-c-func-name)),
	   subr_sym, make_string ("R", 1));

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

  Fputhash (subr_sym, make_mint_ptr (field), comp.imported_funcs_h);
  SAFE_FREE ();
  return field;
}

/* Emit calls fetching from existing declarations.  */
static gcc_jit_rvalue *
emit_call (Lisp_Object subr_sym, gcc_jit_type *ret_type, ptrdiff_t nargs,
	   gcc_jit_rvalue **args, bool direct)
{
  Lisp_Object func =
    Fgethash (subr_sym, direct ? comp.exported_funcs_h: comp.imported_funcs_h,
	      Qnil);
  if (NILP (func))
      xsignal2 (Qnative_ice,
		build_string ("missing function declaration"),
		subr_sym);

  if (direct)
    {
      emit_comment (format_string ("direct call to subr: %s",
				   SSDATA (SYMBOL_NAME (subr_sym))));
      return gcc_jit_context_new_call (comp.ctxt,
				       NULL,
				       xmint_pointer (func),
				       nargs,
				       args);
    }
  else
    {
      gcc_jit_lvalue *f_ptr =
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (comp.func_relocs),
	  NULL,
	  (gcc_jit_field *) xmint_pointer (func));

      if (!f_ptr)
	xsignal2 (Qnative_ice,
		  build_string ("missing function relocation"),
		  subr_sym);
      emit_comment (format_string ("calling subr: %s",
				   SSDATA (SYMBOL_NAME (subr_sym))));
      return gcc_jit_context_new_call_through_ptr (comp.ctxt,
						   NULL,
						   gcc_jit_lvalue_as_rvalue (f_ptr),
						   nargs,
						   args);
    }
}

static gcc_jit_rvalue *
emit_call_ref (Lisp_Object subr_sym, ptrdiff_t nargs,
	       gcc_jit_lvalue *base_arg, bool direct)
{
  gcc_jit_rvalue *args[] =
    { gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.ptrdiff_type,
					   nargs),
      gcc_jit_lvalue_get_address (base_arg, NULL) };
  return emit_call (subr_sym, comp.lisp_obj_type, 2, args, direct);
}

/* Close current basic block emitting a conditional.  */

static void
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
  static ptrdiff_t i;

  gcc_jit_field *orig_field =
    type_to_cast_field (gcc_jit_rvalue_get_type (obj));
  gcc_jit_field *dest_field = type_to_cast_field (new_type);

  gcc_jit_lvalue *tmp_u =
    gcc_jit_function_new_local (comp.func,
				NULL,
				comp.cast_union_type,
				format_string ("union_cast_%td", i++));
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
   Emit the equivalent of:
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

static gcc_jit_rvalue *
emit_XLI (gcc_jit_rvalue *obj)
{
  emit_comment ("XLI");

  return gcc_jit_rvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_num);
}

static gcc_jit_lvalue *
emit_lval_XLI (gcc_jit_lvalue *obj)
{
  emit_comment ("lval_XLI");

  return gcc_jit_lvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_num);
}

/*
static gcc_jit_rvalue *
emit_XLP (gcc_jit_rvalue *obj)
{
  emit_comment ("XLP");

  return gcc_jit_rvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_ptr);
}

static gcc_jit_lvalue *
emit_lval_XLP (gcc_jit_lvalue *obj)
{
  emit_comment ("lval_XLP");

  return gcc_jit_lvalue_access_field (obj,
				      NULL,
				      comp.lisp_obj_as_ptr);
} */
static gcc_jit_rvalue *
emit_XUNTAG (gcc_jit_rvalue *a, gcc_jit_type *type, ptrdiff_t lisp_word_tag)
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
emit_TAGGEDP (gcc_jit_rvalue *obj, ptrdiff_t tag)
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

  gcc_jit_rvalue *args[] =
    { obj,
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
					emit_INTEGERP (obj),
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

static gcc_jit_rvalue *
emit_const_lisp_obj (Lisp_Object obj)
{
  emit_comment (format_string ("const lisp obj: %s",
			       SSDATA (Fprin1_to_string (obj, Qnil))));

  if (Qnil == NULL && EQ (obj, Qnil))
    return emit_cast (comp.lisp_obj_type,
		      gcc_jit_context_new_rvalue_from_ptr (comp.ctxt,
							   comp.void_ptr_type,
							   NULL));

  Lisp_Object d_reloc_idx = CALL1I (comp-ctxt-data-relocs-idx, Vcomp_ctxt);
  ptrdiff_t reloc_fixn = XFIXNUM (Fgethash (obj, d_reloc_idx, Qnil));
  gcc_jit_rvalue *reloc_n =
    gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					 comp.ptrdiff_type,
					 reloc_fixn);
  return
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_array_access (comp.ctxt,
					NULL,
					comp.data_relocs,
					reloc_n));
}

static gcc_jit_rvalue *
emit_NILP (gcc_jit_rvalue *x)
{
  emit_comment ("NILP");
  return emit_EQ (x, emit_const_lisp_obj (Qnil));
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
      emit_const_lisp_obj (Qconsp),
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

  gcc_jit_block_add_assignment (
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

  gcc_jit_block_add_assignment (
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
	emit_cast (comp.uintptr_type,
		   gcc_jit_lvalue_as_rvalue (
		     gcc_jit_rvalue_dereference (comp.pure_ref, NULL)))),
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
  Lisp_Object const_vld = CALL1I (comp-mvar-const-vld, mvar);
  Lisp_Object constant = CALL1I (comp-mvar-constant, mvar);

  if (!NILP (const_vld))
    {
      if (FIXNUMP (constant))
	{
	  /* We can still emit directly objects that are self-contained in a
	     word (read fixnums).  */
	  emit_comment (SSDATA (Fprin1_to_string (constant, Qnil)));
	  gcc_jit_rvalue *word =
	    gcc_jit_context_new_rvalue_from_ptr (comp.ctxt,
						 comp.void_ptr_type,
						 constant);
	  return emit_cast (comp.lisp_obj_type, word);
	}
      /* Other const objects are fetched from the reloc array.  */
      return emit_const_lisp_obj (constant);
    }

  return gcc_jit_lvalue_as_rvalue (get_slot (mvar));
}

static void
emit_frame_assignment (Lisp_Object dst_mvar, gcc_jit_rvalue *val)
{

  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    get_slot (dst_mvar),
    val);
}

static gcc_jit_rvalue *
emit_set_internal (Lisp_Object args)
{
  /*
    Ex: (set_internal #s(comp-mvar nil nil t comp-test-up-val nil nil)
                      #s(comp-mvar 1 4 t nil symbol nil)).
  */
  /* TODO: Inline the most common case.  */
  if (list_length (args) != 3)
    xsignal2 (Qnative_ice,
	      build_string ("unexpected arg length for insns"),
	      args);

  args = XCDR (args);
  int i = 0;
  gcc_jit_rvalue *gcc_args[4];
  FOR_EACH_TAIL (args)
    gcc_args[i++] = emit_mvar_val (XCAR (args));
  gcc_args[2] = emit_const_lisp_obj (Qnil);
  gcc_args[3] = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
						     comp.int_type,
						     SET_INTERNAL_SET);
  return emit_call (intern_c_string ("set_internal"), comp.void_type , 4,
		    gcc_args, false);
}

/* This is for a regular function with arguments as m-var.  */

static gcc_jit_rvalue *
emit_simple_limple_call (Lisp_Object args, gcc_jit_type *ret_type, bool direct)
{
  USE_SAFE_ALLOCA;
  int i = 0;
  Lisp_Object callee = FIRST (args);
  args = XCDR (args);
  ptrdiff_t nargs = list_length (args);
  gcc_jit_rvalue **gcc_args = SAFE_ALLOCA (nargs * sizeof (*gcc_args));
  FOR_EACH_TAIL (args)
    gcc_args[i++] = emit_mvar_val (XCAR (args));

  SAFE_FREE ();
  return emit_call (callee, ret_type, nargs, gcc_args, direct);
}

static gcc_jit_rvalue *
emit_simple_limple_call_lisp_ret (Lisp_Object args)
{
  /*
    Ex: (call Fcons #s(comp-mvar 3 0 t 1 nil) #s(comp-mvar 4 nil t nil nil)).
  */
  return emit_simple_limple_call (args, comp.lisp_obj_type, false);
}

static gcc_jit_rvalue *
emit_simple_limple_call_void_ret (Lisp_Object args)
{
  return emit_simple_limple_call (args, comp.void_type, false);
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
emit_limple_call_ref (Lisp_Object insn, bool direct)
{
  /* Ex: (funcall #s(comp-mvar 1 5 t eql symbol t)
                  #s(comp-mvar 2 6 nil nil nil t)
		  #s(comp-mvar 3 7 t 0 fixnum t)).  */

  Lisp_Object callee = FIRST (insn);
  EMACS_INT nargs = XFIXNUM (Flength (CDR (insn)));
  EMACS_INT base_ptr = 0;
  if (nargs)
    base_ptr = XFIXNUM (CALL1I (comp-mvar-slot, SECOND (insn)));
  return emit_call_ref (callee, nargs, comp.frame[base_ptr], direct);
}

/* Register an handler for a non local exit.  */

static void
emit_limple_push_handler (gcc_jit_rvalue *handler, gcc_jit_rvalue *handler_type,
			  gcc_jit_block *handler_bb, gcc_jit_block *guarded_bb,
			  Lisp_Object clobbered_mvar)
{
   /* struct handler *c = push_handler (POP, type);  */

  gcc_jit_rvalue *args[] = { handler, handler_type };
  gcc_jit_block_add_assignment (
    comp.block,
    NULL,
    comp.loc_handler,
    emit_call (intern_c_string ("push_handler"),
	       comp.handler_ptr_type, 2, args, false));

  args[0] =
    gcc_jit_lvalue_get_address (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (comp.loc_handler),
	  NULL,
	  comp.handler_jmp_field),
	NULL);

  gcc_jit_rvalue *res;
  res =
    emit_call (intern_c_string (STR (SETJMP_NAME)), comp.int_type, 1, args, false);
  emit_cond_jump (res, handler_bb, guarded_bb);
}

static void
emit_limple_insn (Lisp_Object insn)
{
  Lisp_Object op = XCAR (insn);
  Lisp_Object args = XCDR (insn);
  gcc_jit_rvalue *res;
  Lisp_Object arg[6];

  Lisp_Object p = XCDR (insn);
  ptrdiff_t i = 0;
  FOR_EACH_TAIL (p)
    {
      if (i == sizeof (arg) / sizeof (Lisp_Object))
	break;
      arg[i++] = XCAR (p);
    }

  if (EQ (op, Qjump))
    {
      /* Unconditional branch.  */
      gcc_jit_block *target = retrive_block (arg[0]);
      gcc_jit_block_end_with_jump (comp.block, NULL, target);
    }
  else if (EQ (op, Qcond_jump))
    {
      /* Conditional branch.  */
      gcc_jit_rvalue *a = emit_mvar_val (arg[0]);
      gcc_jit_rvalue *b =  emit_mvar_val (arg[1]);
      gcc_jit_block *target1 = retrive_block (arg[2]);
      gcc_jit_block *target2 = retrive_block (arg[3]);

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
					     XFIXNUM (arg[0]));
      gcc_jit_block *target1 = retrive_block (arg[1]);
      gcc_jit_block *target2 = retrive_block (arg[2]);
      gcc_jit_rvalue *test = gcc_jit_context_new_comparison (
			       comp.ctxt,
			       NULL,
			       GCC_JIT_COMPARISON_LE,
			       gcc_jit_lvalue_as_rvalue (nargs),
			       n);
      emit_cond_jump (test, target2, target1);
    }
  else if (EQ (op, Qphi))
    {
      /* Nothing to do for phis into the backend.  */
    }
  else if (EQ (op, Qpush_handler))
    {
      /* (push-handler condition-case #s(comp-mvar 0 3 t (arith-error) cons nil) 1 bb_2 bb_1) */
      int h_num UNINIT;
      Lisp_Object handler_spec = arg[0];
      gcc_jit_rvalue *handler = emit_mvar_val (arg[1]);
      if (EQ (handler_spec, Qcatcher))
	h_num = CATCHER;
      else if (EQ (handler_spec, Qcondition_case))
	h_num = CONDITION_CASE;
      else
	xsignal2 (Qnative_ice, build_string ("incoherent insn"), insn);
      gcc_jit_rvalue *handler_type =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.int_type,
					     h_num);
      gcc_jit_block *handler_bb = retrive_block (arg[2]);
      gcc_jit_block *guarded_bb = retrive_block (arg[3]);
      emit_limple_push_handler (handler, handler_type, handler_bb, guarded_bb,
				arg[0]);
    }
  else if (EQ (op, Qpop_handler))
    {
      /*
	C: current_thread->m_handlerlist =
	     current_thread->m_handlerlist->next;
      */
      gcc_jit_lvalue *m_handlerlist =
	gcc_jit_rvalue_dereference_field (
          gcc_jit_lvalue_as_rvalue (
	    gcc_jit_rvalue_dereference (comp.current_thread_ref, NULL)),
	  NULL,
	  comp.m_handlerlist);

      gcc_jit_block_add_assignment (
	comp.block,
	NULL,
	m_handlerlist,
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_rvalue_dereference_field (
	    gcc_jit_lvalue_as_rvalue (m_handlerlist),
	    NULL,
	    comp.handler_next_field)));

    }
  else if (EQ (op, Qfetch_handler))
    {
      gcc_jit_lvalue *m_handlerlist =
        gcc_jit_rvalue_dereference_field (
	  gcc_jit_lvalue_as_rvalue (
	    gcc_jit_rvalue_dereference (comp.current_thread_ref, NULL)),
	  NULL,
	  comp.m_handlerlist);
      gcc_jit_block_add_assignment (comp.block,
				    NULL,
				    comp.loc_handler,
				    gcc_jit_lvalue_as_rvalue (m_handlerlist));

      gcc_jit_block_add_assignment (
        comp.block,
        NULL,
        m_handlerlist,
        gcc_jit_lvalue_as_rvalue (
          gcc_jit_rvalue_dereference_field (
	    gcc_jit_lvalue_as_rvalue (comp.loc_handler),
	    NULL,
	    comp.handler_next_field)));
      emit_frame_assignment (
        arg[0],
        gcc_jit_lvalue_as_rvalue (
          gcc_jit_rvalue_dereference_field (
	    gcc_jit_lvalue_as_rvalue (comp.loc_handler),
	    NULL,
	    comp.handler_val_field)));
    }
  else if (EQ (op, Qcall))
    {
      gcc_jit_block_add_eval (comp.block, NULL,
			      emit_limple_call (args));
    }
  else if (EQ (op, Qcallref))
    {
      gcc_jit_block_add_eval (comp.block, NULL,
			      emit_limple_call_ref (args, false));
    }
  else if (EQ (op, Qdirect_call))
    {
      gcc_jit_block_add_eval (
        comp.block, NULL,
	emit_simple_limple_call (XCDR (insn), comp.lisp_obj_type, true));
    }
  else if (EQ (op, Qdirect_callref))
    {
      gcc_jit_block_add_eval (comp.block, NULL,
			      emit_limple_call_ref (XCDR (insn), true));
    }
  else if (EQ (op, Qset))
    {
      Lisp_Object arg1 = arg[1];

      if (EQ (Ftype_of (arg1), Qcomp_mvar))
	res = emit_mvar_val (arg1);
      else if (EQ (FIRST (arg1), Qcall))
	res = emit_limple_call (XCDR (arg1));
      else if (EQ (FIRST (arg1), Qcallref))
	res = emit_limple_call_ref (XCDR (arg1), false);
      else if (EQ (FIRST (arg1), Qdirect_call))
	res = emit_simple_limple_call (XCDR (arg1), comp.lisp_obj_type, true);
      else if (EQ (FIRST (arg1), Qdirect_callref))
	res = emit_limple_call_ref (XCDR (arg1), true);
      else
	xsignal2 (Qnative_ice,
		  build_string ("LIMPLE inconsistent arg1 for insn"),
		  insn);

      if (!res)
	xsignal1 (Qnative_ice,
		  build_string (gcc_jit_context_get_first_error (comp.ctxt)));

      emit_frame_assignment (arg[0], res);
    }
  else if (EQ (op, Qset_par_to_local))
    {
      /* Ex: (set-par-to-local #s(comp-mvar 0 3 nil nil nil nil) 0).  */
      EMACS_INT param_n = XFIXNUM (arg[1]);
      gcc_jit_rvalue *param =
	gcc_jit_param_as_rvalue (gcc_jit_function_get_param (comp.func,
							     param_n));
      emit_frame_assignment (arg[0], param);
    }
  else if (EQ (op, Qset_args_to_local))
    {
      /*
	Ex: (set-args-to-local #s(comp-mvar 1 6 nil nil nil nil))
	C: local[1] = *args;
      */
      gcc_jit_rvalue *gcc_args =
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_param_as_lvalue (gcc_jit_function_get_param (comp.func, 1)));

      gcc_jit_rvalue *res =
	gcc_jit_lvalue_as_rvalue (gcc_jit_rvalue_dereference (gcc_args, NULL));

      emit_frame_assignment (arg[0], res);
    }
  else if (EQ (op, Qset_rest_args_to_local))
    {
      /*
        Ex: (set-rest-args-to-local #s(comp-mvar 2 9 nil nil nil nil))
        C: local[2] = list (nargs - 2, args);
      */

      EMACS_INT slot_n = XFIXNUM (CALL1I (comp-mvar-slot, arg[0]));
      gcc_jit_rvalue *n =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.ptrdiff_type,
					     slot_n);
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
		       list_args, false);

      emit_frame_assignment (arg[0], res);
    }
  else if (EQ (op, Qinc_args))
    {
      /*
	Ex: (inc-args)
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
      /* Ex: (setimm #s(comp-mvar 9 1 t 3 nil) 3 a).  */
      gcc_jit_rvalue *reloc_n =
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.int_type,
					     XFIXNUM (arg[1]));
      emit_comment (SSDATA (Fprin1_to_string (arg[2], Qnil)));
      emit_frame_assignment (
	arg[0],
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_context_new_array_access (comp.ctxt,
					    NULL,
					    comp.data_relocs,
					    reloc_n)));
    }
  else if (EQ (op, Qcomment))
    {
      /* Ex: (comment "Function: foo").  */
      emit_comment (SSDATA (arg[0]));
    }
  else if (EQ (op, Qreturn))
    {
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_mvar_val (arg[0]));
    }
  else
    {
      xsignal2 (Qnative_ice,
		build_string ("LIMPLE op inconsistent"),
		op);
    }
}


/**************/
/* Inliners.  */
/**************/

static gcc_jit_rvalue *
emit_call_with_type_hint (gcc_jit_function *func, Lisp_Object insn,
			  Lisp_Object type)
{
  bool type_hint = EQ (CALL1I (comp-mvar-type, SECOND (insn)), type);
  gcc_jit_rvalue *args[] =
    { emit_mvar_val (SECOND (insn)),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.bool_type,
					   type_hint) };

  return gcc_jit_context_new_call (comp.ctxt, NULL, func, 2, args);
}

/* Same as before but with two args. The type hint is on the 2th.  */
static gcc_jit_rvalue *
emit_call2_with_type_hint (gcc_jit_function *func, Lisp_Object insn,
			   Lisp_Object type)
{
  bool type_hint = EQ (CALL1I (comp-mvar-type, SECOND (insn)), type);
  gcc_jit_rvalue *args[] =
    { emit_mvar_val (SECOND (insn)),
      emit_mvar_val (THIRD (insn)),
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   comp.bool_type,
					   type_hint) };

  return gcc_jit_context_new_call (comp.ctxt, NULL, func, 3, args);
}


static gcc_jit_rvalue *
emit_add1 (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.add1, insn, Qfixnum);
}

static gcc_jit_rvalue *
emit_sub1 (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.sub1, insn, Qfixnum);
}

static gcc_jit_rvalue *
emit_negate (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.negate, insn, Qfixnum);
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
  return emit_call_with_type_hint (comp.car, insn, Qcons);
}

static gcc_jit_rvalue *
emit_cdr (Lisp_Object insn)
{
  return emit_call_with_type_hint (comp.cdr, insn, Qcons);
}

static gcc_jit_rvalue *
emit_setcar (Lisp_Object insn)
{
  return emit_call2_with_type_hint (comp.setcar, insn, Qcons);
}

static gcc_jit_rvalue *
emit_setcdr (Lisp_Object insn)
{
  return emit_call2_with_type_hint (comp.setcdr, insn, Qcons);
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

/* This is in charge of serializing an object and export a function to
   retrieve it at load time.  */
static void
emit_static_object (const char *name, Lisp_Object obj)
{
  /* libgccjit has no support for initialized static data.
     The mechanism below is certainly not aesthetic but I assume the bottle neck
     in terms of performance at load time will still be the reader.
     NOTE: we can not relay on libgccjit even for valid NULL terminated C
     strings cause of this funny bug that will affect all pre gcc10 era gccs:
     https://gcc.gnu.org/ml/jit/2019-q3/msg00013.html  */

  Lisp_Object str = Fprin1_to_string (obj, Qnil);
  ptrdiff_t len = SBYTES (str);
  const char *p = SSDATA (str);

  gcc_jit_type *a_type =
    gcc_jit_context_new_array_type (comp.ctxt,
				    NULL,
				    comp.char_type,
				    len + 1);
  gcc_jit_field *fields[] =
    { gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 comp.ptrdiff_type,
				 "len"),
      gcc_jit_context_new_field (comp.ctxt,
				 NULL,
				 a_type,
				 "data") };

  gcc_jit_type *data_struct_t =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (comp.ctxt,
				       NULL,
				       format_string ("%s_struct", name),
				       2, fields));

  gcc_jit_lvalue *data_struct =
    gcc_jit_context_new_global (comp.ctxt,
				NULL,
				GCC_JIT_GLOBAL_INTERNAL,
				data_struct_t,
				format_string ("%s_s", name));

  gcc_jit_function *f =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  gcc_jit_type_get_pointer (data_struct_t),
				  name,
				  0, NULL, 0);
  DECL_BLOCK (block, f);

  /* NOTE this truncates if the data has some zero byte before termination.  */
  gcc_jit_block_add_comment (block, NULL, p);

  gcc_jit_lvalue *arr =
      gcc_jit_lvalue_access_field (data_struct, NULL, fields[1]);

  for (ptrdiff_t i = 0; i < len; i++, p++)
    {
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
					     *p));
    }
  gcc_jit_block_add_assignment (
	block,
	NULL,
	gcc_jit_lvalue_access_field (data_struct, NULL, fields[0]),
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.ptrdiff_type,
					     len));
  gcc_jit_rvalue *res = gcc_jit_lvalue_get_address (data_struct, NULL);
  gcc_jit_block_end_with_return (block, NULL, res);
}

static void
declare_runtime_imported_data (void)
{
  /* Imported symbols by inliner functions.  */
  CALL1I (comp-add-const-to-relocs, Qnil);
  CALL1I (comp-add-const-to-relocs, Qt);
  CALL1I (comp-add-const-to-relocs, Qconsp);
  CALL1I (comp-add-const-to-relocs, Qlistp);
}

/*
  Declare as imported all the functions that are requested from the runtime.
  These are either subrs or not.
*/
static Lisp_Object
declare_runtime_imported_funcs (void)
{
  Lisp_Object field_list = Qnil;

#define ADD_IMPORTED(f_name, ret_type, nargs, args)			       \
  {									       \
    Lisp_Object name = intern_c_string (STR (f_name));			       \
    Lisp_Object field =							       \
      make_mint_ptr (declare_imported_func (name, ret_type, nargs, args));     \
    Lisp_Object el = Fcons (name, field);				       \
    field_list = Fcons (el, field_list);				       \
  } while (0)

  gcc_jit_type *args[4];

  ADD_IMPORTED (wrong_type_argument, comp.void_type, 2, NULL);

  args[0] = comp.lisp_obj_type;
  args[1] = comp.int_type;
  ADD_IMPORTED (helper_PSEUDOVECTOR_TYPEP_XUNTAG, comp.bool_type, 2, args);

  ADD_IMPORTED (pure_write_error, comp.void_type, 1, NULL);

  args[0] = comp.lisp_obj_type;
  args[1] = comp.int_type;
  ADD_IMPORTED (push_handler, comp.handler_ptr_type, 2, args);

  args[0] = gcc_jit_type_get_pointer (gcc_jit_struct_as_type (comp.jmp_buf_s));
  ADD_IMPORTED (SETJMP_NAME, comp.int_type, 1, args);

  ADD_IMPORTED (record_unwind_protect_excursion, comp.void_type, 0, NULL);

  args[0] = comp.lisp_obj_type;
  ADD_IMPORTED (helper_unbind_n, comp.lisp_obj_type, 1, args);

  ADD_IMPORTED (helper_save_restriction, comp.void_type, 0, NULL);

  ADD_IMPORTED (record_unwind_current_buffer, comp.void_type, 0, NULL);

  args[0] = args[1] = args[2] = comp.lisp_obj_type;
  args[3] = comp.int_type;
  ADD_IMPORTED (set_internal, comp.void_type, 4, args);

  args[0] = comp.lisp_obj_type;
  ADD_IMPORTED (helper_unwind_protect, comp.void_type, 1, args);

  args[0] = args[1] = comp.lisp_obj_type;
  ADD_IMPORTED (specbind, comp.void_type, 2, args);

#undef ADD_IMPORTED

  return Freverse (field_list);
}

/*
  This emit the code needed by every compilation unit to be loaded.
*/
static void
emit_ctxt_code (void)
{
  comp.current_thread_ref =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (
        comp.ctxt,
        NULL,
        GCC_JIT_GLOBAL_EXPORTED,
        gcc_jit_type_get_pointer (comp.thread_state_ptr_type),
        CURRENT_THREAD_RELOC_SYM));

  comp.pure_ref =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (
        comp.ctxt,
        NULL,
        GCC_JIT_GLOBAL_EXPORTED,
        gcc_jit_type_get_pointer (comp.void_ptr_type),
        PURE_RELOC_SYM));

  declare_runtime_imported_data ();
  /* Imported objects.  */
  EMACS_INT d_reloc_len =
    XFIXNUM (CALL1I (hash-table-count,
		       CALL1I (comp-ctxt-data-relocs-idx, Vcomp_ctxt)));
  Lisp_Object d_reloc = Fnreverse (CALL1I (comp-ctxt-data-relocs-l, Vcomp_ctxt));
  d_reloc = Fvconcat (1, &d_reloc);

  comp.data_relocs =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_context_new_global (
	comp.ctxt,
	NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	gcc_jit_context_new_array_type (comp.ctxt,
					NULL,
					comp.lisp_obj_type,
					d_reloc_len),
	DATA_RELOC_SYM));

  emit_static_object (TEXT_DATA_RELOC_SYM, d_reloc);

  /* Functions imported from Lisp code.  */

  gcc_jit_field **fields = xmalloc (freloc.size * sizeof (*fields));
  ptrdiff_t n_frelocs = 0;
  Lisp_Object f_runtime = declare_runtime_imported_funcs ();
  FOR_EACH_TAIL (f_runtime)
    {
      Lisp_Object el = XCAR (f_runtime);
      eassert (n_frelocs < freloc.size);
      fields[n_frelocs++] = xmint_pointer (XCDR (el));
    }

  Lisp_Object subr_l = Vsubr_list;
  FOR_EACH_TAIL (subr_l)
    {
      struct Lisp_Subr *subr = XSUBR (XCAR (subr_l));
      Lisp_Object subr_sym = intern_c_string (subr->symbol_name);
      eassert (n_frelocs < freloc.size);
      fields[n_frelocs++] = declare_imported_func (subr_sym, comp.lisp_obj_type,
						   subr->max_args, NULL);
    }

  gcc_jit_struct *f_reloc_struct =
    gcc_jit_context_new_struct_type (comp.ctxt,
				     NULL,
				     "freloc_link_table",
				     n_frelocs, fields);
  comp.func_relocs =
    gcc_jit_context_new_global (
      comp.ctxt,
      NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_pointer (gcc_jit_struct_as_type (f_reloc_struct)),
      IMPORTED_FUNC_LINK_TABLE);

  xfree (fields);
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
				    ARRAYELTS (cdr_u_fields),
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
				     ARRAYELTS (cons_s_fields),
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
				    ARRAYELTS (cons_u_fields),
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
			     ARRAYELTS (fields),
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
				     ARRAYELTS (fields),
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
				    ARRAYELTS (cast_union_fields),
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
				     comp.void_type, 2, wrong_type_args,
				     false));

  gcc_jit_block_end_with_void_return (not_ok_block, NULL);
}

/* Define a substitute for CAR as always inlined function.  */

static void
define_CAR_CDR (void)
{
  gcc_jit_function *func[2];
  char const *f_name[] = { "CAR", "CDR" };
  for (int i = 0; i < 2; i++)
    {
      gcc_jit_param *param[] =
	{ gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.lisp_obj_type,
				     "c"),
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.bool_type,
				     "cert_cons") };
      /* TODO: understand why after ipa-prop pass gcc is less keen on inlining
	 and as consequence can refuse to compile these. (see dhrystone.el)
	 Flag this and all the one involved in ipa-prop as
	 GCC_JIT_FUNCTION_INTERNAL not to fail compilation in case.
	 This seems at least to have no perf downside.  */
      func[i] =
	gcc_jit_context_new_function (comp.ctxt, NULL,
				      GCC_JIT_FUNCTION_INTERNAL,
				      comp.lisp_obj_type,
				      f_name[i],
				      2, param, 0);

      gcc_jit_rvalue *c = gcc_jit_param_as_rvalue (param[0]);
      DECL_BLOCK (entry_block, func[i]);
      DECL_BLOCK (is_cons_b, func[i]);
      DECL_BLOCK (not_a_cons_b, func[i]);
      comp.block = entry_block;
      comp.func = func[i];
      emit_cond_jump (
	gcc_jit_context_new_binary_op (comp.ctxt,
				       NULL,
				       GCC_JIT_BINARY_OP_LOGICAL_OR,
				       comp.bool_type,
				       gcc_jit_param_as_rvalue (param[1]),
				       emit_cast (comp.bool_type,
						  emit_CONSP (c))),
	is_cons_b,
	not_a_cons_b);
      comp.block = is_cons_b;
      if (i == 0)
	gcc_jit_block_end_with_return (comp.block, NULL, emit_XCAR (c));
      else
	gcc_jit_block_end_with_return (comp.block, NULL, emit_XCDR (c));

      comp.block = not_a_cons_b;

      DECL_BLOCK (is_nil_b, func[i]);
      DECL_BLOCK (not_nil_b, func[i]);

      emit_cond_jump (emit_NILP (c), is_nil_b, not_nil_b);

      comp.block = is_nil_b;
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_const_lisp_obj (Qnil));

      comp.block = not_nil_b;
      gcc_jit_rvalue *wrong_type_args[] =
	{ emit_const_lisp_obj (Qlistp), c };

      gcc_jit_block_add_eval (comp.block,
			      NULL,
			      emit_call (intern_c_string ("wrong_type_argument"),
					 comp.void_type, 2, wrong_type_args,
					 false));
      gcc_jit_block_end_with_return (comp.block,
				     NULL,
				     emit_const_lisp_obj (Qnil));
    }
  comp.car = func[0];
  comp.cdr = func[1];
}

static void
define_setcar_setcdr (void)
{
  char const *f_name[] = { "setcar", "setcdr" };
  char const *par_name[] = { "new_car", "new_cdr" };

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

      gcc_jit_param *param[] =
	{ cell,
	  new_el,
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.bool_type,
				     "cert_cons") };

      gcc_jit_function **f_ref = !i ? &comp.setcar : &comp.setcdr;
      *f_ref = gcc_jit_context_new_function (comp.ctxt, NULL,
					     GCC_JIT_FUNCTION_INTERNAL,
					     comp.lisp_obj_type,
					     f_name[i],
					     3, param, 0);
      DECL_BLOCK (entry_block, *f_ref);
      comp.func = *f_ref;
      comp.block = entry_block;

      /* CHECK_CONS (cell);  */
      emit_CHECK_CONS (gcc_jit_param_as_rvalue (cell));

      /* CHECK_IMPURE (cell, XCONS (cell));  */
      gcc_jit_rvalue *args[] =
	{ gcc_jit_param_as_rvalue (cell),
	  emit_XCONS (gcc_jit_param_as_rvalue (cell)) };

      gcc_jit_block_add_eval (entry_block,
			      NULL,
			      gcc_jit_context_new_call (comp.ctxt,
							NULL,
							comp.check_impure,
							2,
							args));

      /* XSETCDR (cell, newel);  */
      if (!i)
	emit_XSETCAR (gcc_jit_param_as_rvalue (cell),
		      gcc_jit_param_as_rvalue (new_el));
      else
	emit_XSETCDR (gcc_jit_param_as_rvalue (cell),
		      gcc_jit_param_as_rvalue (new_el));

      /* return newel;  */
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
  char const *f_name[] = { "add1", "sub1" };
  char const *fall_back_func[] = { "1+", "1-" };
  gcc_jit_rvalue *compare[] =
    { comp.most_positive_fixnum, comp.most_negative_fixnum };
  enum gcc_jit_binary_op op[] =
    { GCC_JIT_BINARY_OP_PLUS, GCC_JIT_BINARY_OP_MINUS };
  for (ptrdiff_t i = 0; i < 2; i++)
    {
      gcc_jit_param *param[] =
	{ gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.lisp_obj_type,
				     "n"),
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.bool_type,
				     "cert_fixnum") };
      comp.func = func[i] =
	gcc_jit_context_new_function (comp.ctxt, NULL,
				      GCC_JIT_FUNCTION_INTERNAL,
				      comp.lisp_obj_type,
				      f_name[i],
				      2,
				      param, 0);
      DECL_BLOCK (entry_block, func[i]);
      DECL_BLOCK (inline_block, func[i]);
      DECL_BLOCK (fcall_block, func[i]);

      comp.block = entry_block;

      /* cert_fixnum ||
	 ((FIXNUMP (n) && XFIXNUM (n) != MOST_POSITIVE_FIXNUM
	 ? (XFIXNUM (n) + 1)
	 : Fadd1 (n)) */

      gcc_jit_rvalue *n = gcc_jit_param_as_rvalue (param[0]);
      gcc_jit_rvalue *n_fixnum = emit_XFIXNUM (n);
      gcc_jit_rvalue *sure_fixnum =
	gcc_jit_context_new_binary_op (
	  comp.ctxt,
	  NULL,
	  GCC_JIT_BINARY_OP_LOGICAL_OR,
	  comp.bool_type,
	  gcc_jit_param_as_rvalue (param[1]),
	  emit_cast (comp.bool_type,
		     emit_FIXNUMP (n)));

      emit_cond_jump (
	gcc_jit_context_new_binary_op (
	  comp.ctxt,
	  NULL,
	  GCC_JIT_BINARY_OP_LOGICAL_AND,
	  comp.bool_type,
	  sure_fixnum,
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
					    comp.lisp_obj_type, 1, &n, false);
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
				     "n"),
	  gcc_jit_context_new_param (comp.ctxt,
				     NULL,
				     comp.bool_type,
				     "cert_fixnum") };

  comp.func = comp.negate =
    gcc_jit_context_new_function (comp.ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  comp.lisp_obj_type,
				  "negate",
				  2, param, 0);

  DECL_BLOCK (entry_block, comp.negate);
  DECL_BLOCK (inline_block, comp.negate);
  DECL_BLOCK (fcall_block, comp.negate);

  comp.block = entry_block;

  /* (cert_fixnum || FIXNUMP (TOP)) && XFIXNUM (TOP) != MOST_NEGATIVE_FIXNUM
     ? make_fixnum (- XFIXNUM (TOP)) : Fminus (1, &TOP))  */

  gcc_jit_lvalue *n = gcc_jit_param_as_lvalue (param[0]);
  gcc_jit_rvalue *n_fixnum = emit_XFIXNUM (gcc_jit_lvalue_as_rvalue (n));
  gcc_jit_rvalue *sure_fixnum =
	gcc_jit_context_new_binary_op (
	  comp.ctxt,
	  NULL,
	  GCC_JIT_BINARY_OP_LOGICAL_OR,
	  comp.bool_type,
	  gcc_jit_param_as_rvalue (param[1]),
	  emit_cast (comp.bool_type,
		     emit_FIXNUMP (gcc_jit_lvalue_as_rvalue (n))));

  emit_cond_jump (
    gcc_jit_context_new_binary_op (
      comp.ctxt,
      NULL,
      GCC_JIT_BINARY_OP_LOGICAL_AND,
      comp.bool_type,
      sure_fixnum,
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
  gcc_jit_rvalue *call_res = emit_call_ref (Qminus, 1, n, false);
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
				 gcc_jit_context_new_rvalue_from_int (
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
	       comp.bool_type, 2, args, false));
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
				       comp.void_type, 1,&pure_write_error_arg,
				       false));

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
				 emit_const_lisp_obj (Qt));

  comp.block = ret_nil_block;
  gcc_jit_block_end_with_return (ret_nil_block,
				 NULL,
				 emit_const_lisp_obj (Qnil));

}

/* Declare a function being compiled and add it to comp.exported_funcs_h.  */

static void
declare_function (Lisp_Object func)
{
  gcc_jit_function *gcc_func;
  char *c_name = SSDATA (CALL1I (comp-func-c-name, func));
  Lisp_Object args = CALL1I (comp-func-args, func);
  bool nargs = (CALL1I (comp-nargs-p, args));
  USE_SAFE_ALLOCA;

  if (!nargs)
    {
      EMACS_INT max_args = XFIXNUM (CALL1I (comp-args-max, args));
      gcc_jit_type **type = SAFE_ALLOCA (max_args * sizeof (*type));
      for (ptrdiff_t i = 0; i < max_args; i++)
	type[i] = comp.lisp_obj_type;

      gcc_jit_param **param = SAFE_ALLOCA (max_args * sizeof (*param));
      for (int i = 0; i < max_args; ++i)
	param[i] = gcc_jit_context_new_param (comp.ctxt,
					      NULL,
					      type[i],
					      format_string ("par_%d", i));
      gcc_func = gcc_jit_context_new_function (comp.ctxt, NULL,
					       GCC_JIT_FUNCTION_EXPORTED,
					       comp.lisp_obj_type,
					       c_name,
					       max_args,
					       param,
					       0);
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
      gcc_func =
	gcc_jit_context_new_function (comp.ctxt,
				      NULL,
				      GCC_JIT_FUNCTION_EXPORTED,
				      comp.lisp_obj_type,
				      c_name, 2, param, 0);
    }

  Fputhash (CALL1I (comp-func-name, func),
	    make_mint_ptr (gcc_func),
	    comp.exported_funcs_h);

  SAFE_FREE ();
}

static void
compile_function (Lisp_Object func)
{
  USE_SAFE_ALLOCA;
  EMACS_INT frame_size = XFIXNUM (CALL1I (comp-func-frame-size, func));

  comp.func = xmint_pointer (Fgethash (CALL1I (comp-func-name, func),
				       comp.exported_funcs_h, Qnil));

  gcc_jit_lvalue *frame_array =
    gcc_jit_function_new_local (
      comp.func,
      NULL,
      gcc_jit_context_new_array_type (comp.ctxt,
				      NULL,
				      comp.lisp_obj_type,
				      frame_size),
      "local");
  comp.frame = SAFE_ALLOCA (frame_size * sizeof (*comp.frame));
  for (EMACS_INT i = 0; i < frame_size; ++i)
    comp.frame[i] =
      gcc_jit_context_new_array_access (
        comp.ctxt,
	NULL,
	gcc_jit_lvalue_as_rvalue (frame_array),
	gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					     comp.int_type,
					     i));

  /*
     The floating frame is a copy of the normal frame that can be used to store
     locals if the are not going to be used in a nargs call.
     This has two advantages:
     - Enable gcc for better reordering (frame array is clobbered every time is
       passed as parameter being involved into an nargs function call).
     - Allow gcc to trigger other optimizations that are prevented by memory
       referencing.
  */
  if (SPEED >= 2)
    {
      comp.f_frame = SAFE_ALLOCA (frame_size * sizeof (*comp.f_frame));
      for (ptrdiff_t i = 0; i < frame_size; ++i)
	comp.f_frame[i] =
	  gcc_jit_function_new_local (comp.func,
				      NULL,
				      comp.lisp_obj_type,
				      format_string ("local%td", i));
    }

  comp.scratch = NULL;

  comp.loc_handler =  gcc_jit_function_new_local (comp.func,
						  NULL,
						  comp.handler_ptr_type,
						  "c");

  comp.func_blocks_h = CALLN (Fmake_hash_table);

  /* Pre-declare all basic blocks to gcc.
     The "entry" block must be declared as first.  */
  declare_block (Qentry);
  Lisp_Object blocks = CALL1I (comp-func-blocks, func);
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
      Lisp_Object insns = CALL1I (comp-block-insns, block);
      if (NILP (block) || NILP (insns))
	xsignal1 (Qnative_ice,
		  build_string ("basic block is missing or empty"));

      comp.block = retrive_block (block_name);
      while (CONSP (insns))
	{
	  Lisp_Object insn = XCAR (insns);
	  emit_limple_insn (insn);
	  insns = XCDR (insns);
	}
    }
  const char *err =  gcc_jit_context_get_first_error (comp.ctxt);
  if (err)
    xsignal3 (Qnative_ice,
	      build_string ("failing to compile function"),
	      CALL1I (comp-func-name, func),
	      build_string (err));

  SAFE_FREE ();
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
      xsignal1 (Qnative_ice,
		build_string ("compiler context already taken"));
      return Qnil;
    }

  if (NILP (comp.emitter_dispatcher))
    {
      /* Move this into syms_of_comp the day will be dumpable.  */
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
      register_emitter (Qadd1, emit_add1);
      register_emitter (Qsub1, emit_sub1);
      register_emitter (Qconsp, emit_consp);
      register_emitter (Qcar, emit_car);
      register_emitter (Qcdr, emit_cdr);
      register_emitter (Qsetcar, emit_setcar);
      register_emitter (Qsetcdr, emit_setcdr);
      register_emitter (Qnegate, emit_negate);
      register_emitter (Qnumberp, emit_numperp);
      register_emitter (Qintegerp, emit_integerp);
    }

  comp.ctxt = gcc_jit_context_acquire ();

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
    }

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
  comp.lisp_obj_type =
    gcc_jit_context_new_union_type (comp.ctxt,
				    NULL,
				    "comp_Lisp_Object",
				    ARRAYELTS (lisp_obj_fields),
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

  comp.exported_funcs_h = CALLN (Fmake_hash_table);
  /*
    Always reinitialize this cause old function definitions are garbage
    collected by libgccjit when the ctxt is released.
  */
  comp.imported_funcs_h = CALLN (Fmake_hash_table);

  /* Define data structures.  */

  define_lisp_cons ();
  define_jmp_buf ();
  define_handler_struct ();
  define_thread_state_struct ();
  define_cast_union ();

  return Qt;
}

DEFUN ("comp--release-ctxt", Fcomp__release_ctxt, Scomp__release_ctxt,
       0, 0, 0,
       doc: /* Release the native compiler context.  */)
     (void)
{
  if (comp.ctxt)
    gcc_jit_context_release (comp.ctxt);

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
				  SPEED);
  /* Gcc doesn't like being interrupted at all.  */
  block_input ();
  sigset_t oldset;
  sigset_t blocked;
  sigemptyset (&blocked);
  sigaddset (&blocked, SIGALRM);
  sigaddset (&blocked, SIGINT);
  sigaddset (&blocked, SIGIO);
  pthread_sigmask (SIG_BLOCK, &blocked, &oldset);

  emit_ctxt_code ();

  /* Define inline functions.  */
  define_CAR_CDR ();
  define_PSEUDOVECTORP ();
  define_CHECK_TYPE ();
  define_CHECK_IMPURE ();
  define_bool_to_lisp_obj ();
  define_setcar_setcdr ();
  define_add1_sub1 ();
  define_negate ();

  struct Lisp_Hash_Table *func_h
    = XHASH_TABLE (CALL1I (comp-ctxt-funcs-h, Vcomp_ctxt));
  for (ptrdiff_t i = 0; i < func_h->count; i++)
    declare_function (HASH_VALUE (func_h, i));
  /* Compile all functions. Can't be done before because the
     relocation structs has to be already defined.  */
  for (ptrdiff_t i = 0; i < func_h->count; i++)
    compile_function (HASH_VALUE (func_h, i));

  if (COMP_DEBUG)
      gcc_jit_context_dump_to_file (comp.ctxt,
				    format_string ("%s.c", SSDATA (ctxtname)),
				    1);
  if (COMP_DEBUG > 2)
    gcc_jit_context_dump_reproducer_to_file (comp.ctxt, "comp_reproducer.c");

  AUTO_STRING (dot_so, NATIVE_ELISP_SUFFIX);

  Lisp_Object out_file = CALLN (Fconcat, ctxtname, dot_so);

  /* Remove the old eln before creating the new one to get a new inode and
     prevent crashes in case the old one is currently loaded.  */
  if (!NILP (Ffile_exists_p (out_file)))
    Fdelete_file (out_file, Qnil);

  gcc_jit_context_compile_to_file (comp.ctxt,
				   GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY,
				   SSDATA (out_file));

  pthread_sigmask (SIG_SETMASK, &oldset, 0);
  unblock_input ();

  return out_file;
}


void
fill_freloc (void)
{
  if (ARRAYELTS (helper_link_table) > F_RELOC_MAX_SIZE)
    goto overflow;
  memcpy (freloc.link_table, helper_link_table, sizeof (freloc.link_table));
  freloc.size = ARRAYELTS (helper_link_table);

  Lisp_Object subr_l = Vsubr_list;
  FOR_EACH_TAIL (subr_l)
    {
      if (freloc.size == F_RELOC_MAX_SIZE)
	goto overflow;
      struct Lisp_Subr *subr = XSUBR (XCAR (subr_l));
      freloc.link_table[freloc.size] = subr->function.a0;
      freloc.size++;
    }
  return;

 overflow:
  fatal ("Overflowing function relocation table, increase F_RELOC_MAX_SIZE");
}

/******************************************************************************/
/* Helper functions called from the run-time.				      */
/* These can't be statics till shared mechanism is used to solve relocations. */
/* Note: this are all potentially definable directly to gcc and are here just */
/* for laziness. Change this if a performance impact is measured.             */
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

void
helper_save_restriction (void)
{
  record_unwind_protect (save_restriction_restore,
			 save_restriction_save ());
}

bool
helper_PSEUDOVECTOR_TYPEP_XUNTAG (Lisp_Object a, enum pvec_type code)
{
  return PSEUDOVECTOR_TYPEP (XUNTAG (a, Lisp_Vectorlike,
				     union vectorlike_header),
			     code);
}


/**************************************/
/* Functions used to load eln files.  */
/**************************************/

static Lisp_Object Vnative_elisp_refs_hash;

typedef char *(*comp_lit_str_func) (void);

/* Deserialize read and return static object.  */
static Lisp_Object
load_static_obj (dynlib_handle_ptr handle, const char *name)
{
  static_obj_t *(*f)(void) = dynlib_sym (handle, name);
  eassert (f);
  static_obj_t *res = f ();
  return Fread (make_string (res->data, res->len));
}

static void
load_comp_unit (Lisp_Object comp_u_obj, Lisp_Object file)
{
  struct Lisp_Native_Comp_Unit *comp_u = XNATIVE_COMP_UNIT (comp_u_obj);
  dynlib_handle_ptr handle = comp_u->handle;
  struct thread_state ***current_thread_reloc =
    dynlib_sym (handle, CURRENT_THREAD_RELOC_SYM);
  EMACS_INT ***pure_reloc = dynlib_sym (handle, PURE_RELOC_SYM);
  Lisp_Object *data_relocs = dynlib_sym (handle, DATA_RELOC_SYM);
  void **freloc_link_table = dynlib_sym (handle, IMPORTED_FUNC_LINK_TABLE);
  void (*top_level_run)(Lisp_Object) = dynlib_sym (handle, "top_level_run");

  if (!(current_thread_reloc
	&& pure_reloc
	&& data_relocs
	&& freloc_link_table
	&& top_level_run))
    xsignal1 (Qnative_lisp_file_inconsistent, file);

  *current_thread_reloc = &current_thread;
  *pure_reloc = (EMACS_INT **)&pure;

  /* Imported data.  */
  Lisp_Object d_vec = load_static_obj (handle, TEXT_DATA_RELOC_SYM);
  EMACS_INT d_vec_len = XFIXNUM (Flength (d_vec));

  for (EMACS_INT i = 0; i < d_vec_len; i++)
      data_relocs[i] = AREF (d_vec, i);

  comp_u->data_vec = d_vec;
  /* Imported functions.  */
  *freloc_link_table = freloc.link_table;

  /* Executing this will perform all the expected environment modification.  */
  top_level_run (comp_u_obj);

  return;
}

DEFUN ("comp--register-subr", Fcomp__register_subr, Scomp__register_subr,
       7, 7, 0,
       doc: /* This gets called by top_level_run during load phase to register
	       each exported subr.  */)
     (Lisp_Object name, Lisp_Object minarg, Lisp_Object maxarg,
      Lisp_Object c_name, Lisp_Object doc, Lisp_Object intspec,
      Lisp_Object comp_u)
{
  dynlib_handle_ptr handle = XNATIVE_COMP_UNIT (comp_u)->handle;
  if (!handle)
    xsignal0 (Qwrong_register_subr_call);

  void *func = dynlib_sym (handle, SSDATA (c_name));
  eassert (func);

  union Aligned_Lisp_Subr *x =
    (union Aligned_Lisp_Subr *) allocate_pseudovector (
				  VECSIZE (union Aligned_Lisp_Subr),
				  0, VECSIZE (union Aligned_Lisp_Subr),
				  PVEC_SUBR);
  x->s.function.a0 = func;
  x->s.min_args = XFIXNUM (minarg);
  x->s.max_args = FIXNUMP (maxarg) ? XFIXNUM (maxarg) : MANY;
  x->s.symbol_name = xstrdup (SSDATA (Fsymbol_name (name)));
  x->s.native_intspec = intspec;
  x->s.native_doc = doc;
  x->s.native_comp_u = comp_u;
  Lisp_Object tem;
  XSETSUBR (tem, &x->s);
  set_symbol_function (name, tem);

  LOADHIST_ATTACH (Fcons (Qdefun, name));

  return Qnil;
}

/* Load related routines.  */
DEFUN ("native-elisp-load", Fnative_elisp_load, Snative_elisp_load, 1, 1, 0,
       doc: /* Load native elisp code FILE.  */)
  (Lisp_Object file)
{
  CHECK_STRING (file);

  if (!freloc.link_table[0])
    xsignal2 (Qnative_lisp_load_failed, file,
	      build_string ("Empty relocation table"));

  /* FIXME non portable.  */
  /* We copy the content of the file to be loaded in a memory mapped
     file.  We then keep track of this in the struct
     Lisp_Native_Comp_Unit.  In case this will be overwritten
     or delete we'll dump the right data.  */
  int fd_in = emacs_open (SSDATA (file), O_RDONLY, 0);
  int fd_out = memfd_create (SSDATA (file), 0);
  if (fd_in < 0 || fd_out < 0)
    xsignal2 (Qnative_lisp_load_failed, file,
	      build_string ("Failing to get file descriptor"));
  struct stat st;
  if (fstat (fd_in, &st) != 0)
    report_file_error ("Input file status", file);
  copy_file_fd (fd_out, fd_in, &st, Qnil, file);
  dynlib_handle_ptr handle =
    dynlib_open (format_string ("/proc/%d/fd/%d", getpid (), fd_out));
  Lisp_Object comp_u = make_native_comp_u (fd_in, handle);
  if (!handle)
    xsignal2 (Qnative_lisp_load_failed, file, build_string (dynlib_error ()));

  load_comp_unit (comp_u, file);

  return Qt;
}


void
syms_of_comp (void)
{
  /* Compiler control customizes.  */
  DEFSYM (Qcomp_speed, "comp-speed");
  DEFSYM (Qcomp_debug, "comp-debug");

  /* Limple instruction set.  */
  DEFSYM (Qcomment, "comment");
  DEFSYM (Qjump, "jump");
  DEFSYM (Qcall, "call");
  DEFSYM (Qcallref, "callref");
  DEFSYM (Qdirect_call, "direct-call");
  DEFSYM (Qdirect_callref, "direct-callref");
  DEFSYM (Qsetimm, "setimm");
  DEFSYM (Qreturn, "return");
  DEFSYM (Qcomp_mvar, "comp-mvar");
  DEFSYM (Qcond_jump, "cond-jump");
  DEFSYM (Qphi, "phi");
  /* Ops in use for prologue emission.  */
  DEFSYM (Qset_par_to_local, "set-par-to-local");
  DEFSYM (Qset_args_to_local, "set-args-to-local");
  DEFSYM (Qset_rest_args_to_local, "set-rest-args-to-local");
  DEFSYM (Qinc_args, "inc-args");
  DEFSYM (Qcond_jump_narg_leq, "cond-jump-narg-leq");
  /* Others.  */
  DEFSYM (Qpush_handler, "push-handler");
  DEFSYM (Qpop_handler, "pop-handler");
  DEFSYM (Qfetch_handler, "fetch-handler");
  DEFSYM (Qcondition_case, "condition-case");
  /* call operands.  */
  DEFSYM (Qcatcher, "catcher");
  DEFSYM (Qentry, "entry");
  DEFSYM (Qset_internal, "set_internal");
  DEFSYM (Qrecord_unwind_current_buffer, "record_unwind_current_buffer");
  DEFSYM (Qrecord_unwind_protect_excursion, "record_unwind_protect_excursion");
  DEFSYM (Qhelper_unbind_n, "helper_unbind_n");
  DEFSYM (Qhelper_unwind_protect, "helper_unwind_protect");
  DEFSYM (Qhelper_save_restriction, "helper_save_restriction");
  /* Inliners.  */
  DEFSYM (Qadd1, "1+");
  DEFSYM (Qsub1, "1-");
  DEFSYM (Qconsp, "consp");
  DEFSYM (Qcar, "car");
  DEFSYM (Qcdr, "cdr");
  DEFSYM (Qsetcar, "setcar");
  DEFSYM (Qsetcdr, "setcdr");
  DEFSYM (Qnegate, "negate");
  DEFSYM (Qnumberp, "numberp");
  DEFSYM (Qintegerp, "integerp");

  /* Others.  */
  DEFSYM (Qfixnum, "fixnum");
  DEFSYM (Qscratch, "scratch");

  /* To be signaled by the compiler.  */
  DEFSYM (Qnative_compiler_error, "native-compiler-error");
  Fput (Qnative_compiler_error, Qerror_conditions,
	pure_list (Qnative_compiler_error, Qerror));
  Fput (Qnative_compiler_error, Qerror_message,
        build_pure_c_string ("Native compiler error"));

  DEFSYM (Qnative_ice, "native-ice");
  Fput (Qnative_ice, Qerror_conditions,
	pure_list (Qnative_ice, Qnative_compiler_error, Qerror));
  Fput (Qnative_ice, Qerror_message,
        build_pure_c_string ("Internal native compiler error"));

  /* By the load machinery.  */
  DEFSYM (Qnative_lisp_load_failed, "native-lisp-load-failed");
  Fput (Qnative_lisp_load_failed, Qerror_conditions,
	pure_list (Qnative_lisp_load_failed, Qerror));
  Fput (Qnative_lisp_load_failed, Qerror_message,
        build_pure_c_string ("Native elisp load failed"));

  DEFSYM (Qnative_lisp_wrong_reloc, "native-lisp-wrong-reloc");
  Fput (Qnative_lisp_wrong_reloc, Qerror_conditions,
	pure_list (Qnative_lisp_wrong_reloc, Qnative_lisp_load_failed, Qerror));
  Fput (Qnative_lisp_wrong_reloc, Qerror_message,
        build_pure_c_string ("Primitive redefined or wrong relocation"));

  DEFSYM (Qwrong_register_subr_call, "wrong-register-subr-call");
  Fput (Qwrong_register_subr_call, Qerror_conditions,
	pure_list (Qwrong_register_subr_call, Qnative_lisp_load_failed, Qerror));
  Fput (Qwrong_register_subr_call, Qerror_message,
        build_pure_c_string ("comp--register-subr can only be called during "
			    "native lisp load phase."));

  DEFSYM (Qnative_lisp_file_inconsistent, "native-lisp-file-inconsistent");
  Fput (Qnative_lisp_file_inconsistent, Qerror_conditions,
	pure_list (Qnative_lisp_file_inconsistent, Qnative_lisp_load_failed, Qerror));
  Fput (Qnative_lisp_file_inconsistent, Qerror_message,
        build_pure_c_string ("inconsistent eln file"));

  defsubr (&Scomp__init_ctxt);
  defsubr (&Scomp__release_ctxt);
  defsubr (&Scomp__compile_ctxt_to_file);
  defsubr (&Scomp__register_subr);
  defsubr (&Snative_elisp_load);

  staticpro (&comp.exported_funcs_h);
  comp.exported_funcs_h = Qnil;
  staticpro (&comp.imported_funcs_h);
  comp.imported_funcs_h = Qnil;
  staticpro (&comp.func_blocks_h);
  staticpro (&comp.emitter_dispatcher);
  comp.emitter_dispatcher = Qnil;

  DEFVAR_LISP ("comp-ctxt", Vcomp_ctxt,
	       doc: /* The compiler context.  */);
  Vcomp_ctxt = Qnil;

  /* FIXME should be initialized but not here... */
  DEFVAR_LISP ("comp-subr-list", Vsubr_list,
	       doc: /* List of all defined subrs.  */);

  /* Load mechanism.  */
  staticpro (&Vnative_elisp_refs_hash);
  Vnative_elisp_refs_hash
    = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
		       DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
		       Qnil, false);
}

#endif /* HAVE_NATIVE_COMP */
