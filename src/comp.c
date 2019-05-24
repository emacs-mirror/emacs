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

#define COMP_DEBUG 0

#define MAX_FUN_NAME 256

/* Max number of args we are able to handle while emitting function calls.  */

#define MAX_ARGS 16

#define DISASS_FILE_NAME "emacs-asm.s"

#define CHECK_STACK				\
  eassert (stack >= stack_base && stack < stack_over)

#define PUSH(obj)				\
  do {						\
    CHECK_STACK;				\
    *stack = obj;				\
    stack++;					\
  } while (0)

#define POP0

#define POP1					\
  do {						\
    stack--;					\
    CHECK_STACK;				\
    args[0] = *stack;				\
  } while (0)

#define POP2					\
  do {						\
    stack--;					\
    CHECK_STACK;				\
    args[1] = *stack;				\
    stack--;					\
    args[0] = *stack;				\
  } while (0)

#define POP3					\
  do {						\
    stack--;					\
    CHECK_STACK;				\
    args[2] = *stack;				\
    stack--;					\
    args[1] = *stack;				\
    stack--;					\
    args[0] = *stack;				\
  } while (0)

/* Fetch the next byte from the bytecode stream.  */

#define FETCH (bytestr_data[pc++])

/* Fetch two bytes from the bytecode stream and make a 16-bit number
   out of them.  */

#define FETCH2 (op = FETCH, op + (FETCH << 8))

/* Discard n values from the stack.  */

#define DISCARD(n) (stack -= (n))

#define STR(s) #s

/* With most of the ops we need to do the same stuff so this macros are meant
   to save some typing.  */

/* Generate appropriate case and emit convential calls to function. */

#define CASE_CALL_NARGS(name, nargs)					\
  case B##name:								\
  POP##nargs;								\
  res = jit_emit_call (STR(F##name), comp.lisp_obj_type, nargs, args);	\
  PUSH (gcc_jit_lvalue_as_rvalue (res));				\
  break

/* Emit calls to functions with prototype (ptrdiff_t nargs, Lisp_Object *args)
   This is done aggregating args into the scratch_call_area.  */

#define EMIT_SCRATCH_CALL_N(name, nargs)		\
  pop (nargs, &stack, args);				\
  res = jit_emit_callN (name, nargs, args);		\
  PUSH (gcc_jit_lvalue_as_rvalue (res))

/* The compiler context  */

typedef struct {
  gcc_jit_context *ctxt;
  gcc_jit_type *void_type;
  gcc_jit_type *int_type;
  gcc_jit_type *void_ptr_type;
  gcc_jit_type *ptrdiff_type;
  gcc_jit_type *lisp_obj_type;
  gcc_jit_function *func; /* Current function being compiled  */
  gcc_jit_rvalue *nil;
  gcc_jit_rvalue *scratch; /* Will point to scratch_call_area  */
  gcc_jit_block *block; /* Current basic block  */
  Lisp_Object func_hash; /* f_name -> gcc_func  */
} comp_t;

static comp_t comp;

Lisp_Object scratch_call_area[MAX_ARGS];

FILE *logfile;

/* The result of one function compilation.  */

typedef struct {
  gcc_jit_result *gcc_res;
  short min_args, max_args;
} comp_f_res_t;

INLINE static void pop (unsigned n, gcc_jit_rvalue ***stack_ref,
			gcc_jit_rvalue *args[]);

static gcc_jit_function *jit_func_declare (const char *f_name,
					   gcc_jit_type *ret_type,
					   unsigned nargs,
					   gcc_jit_rvalue **args,
					   enum gcc_jit_function_kind kind,
					   bool reusable);

void emacs_native_compile (const char *lisp_f_name, const char *c_f_name,
			   Lisp_Object func, bool dump_asm);

/* Pop form the main evaluation stack and place the elements in args in reversed
 order.  */

INLINE static void
pop (unsigned n, gcc_jit_rvalue ***stack_ref, gcc_jit_rvalue *args[])
{
  gcc_jit_rvalue **stack = *stack_ref;

  while (n--)
    {
      stack--;
      args[n] = *stack;
    }

  *stack_ref = stack;
}

static gcc_jit_function *
jit_func_declare (const char *f_name, gcc_jit_type *ret_type,
		  unsigned nargs, gcc_jit_rvalue **args,
		  enum  gcc_jit_function_kind kind, bool reusable)
{
  gcc_jit_param *param[4];
  gcc_jit_type *type[4];

  /* If args are passed types are extracted from that otherwise assume params */
  /* are all lisp objs.  */
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
				 comp.lisp_obj_type,
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

static gcc_jit_lvalue *
jit_emit_call (const char *f_name, gcc_jit_type *ret_type, unsigned nargs,
	       gcc_jit_rvalue **args)
{
  Lisp_Object key = make_string (f_name, strlen (f_name));
  EMACS_UINT hash = 0;
  struct Lisp_Hash_Table *ht = XHASH_TABLE (comp.func_hash);
  ptrdiff_t i = hash_lookup (ht, key, &hash);

  if (i == -1)
    {
      jit_func_declare(f_name, ret_type, nargs, args, GCC_JIT_FUNCTION_IMPORTED,
		       true);
      i = hash_lookup (ht, key, &hash);
      eassert (i != -1);
    }

  Lisp_Object value = HASH_VALUE (ht, hash_lookup (ht, key, &hash));
  gcc_jit_function *func = (gcc_jit_function *) XFIXNUMPTR (value);

  gcc_jit_lvalue *res = gcc_jit_function_new_local(comp.func,
						   NULL,
						   ret_type,
						   "res");
  gcc_jit_block_add_assignment(comp.block, NULL,
			       res,
			       gcc_jit_context_new_call(comp.ctxt,
							NULL,
							func,
							nargs,
							args));
  return res;
}

static gcc_jit_lvalue *
jit_emit_callN (const char *f_name, unsigned nargs, gcc_jit_rvalue **args)
{
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

  gcc_jit_lvalue *p =
    gcc_jit_function_new_local(comp.func,
			       NULL,
			       gcc_jit_type_get_pointer (comp.lisp_obj_type),
			       "p");

  gcc_jit_block_add_assignment(comp.block, NULL,
			       p,
			       comp.scratch);

  for (int i = 0; i < nargs; i++) {
    gcc_jit_rvalue *idx =
      gcc_jit_context_new_rvalue_from_int (comp.ctxt,
					   gcc_jit_context_get_type(comp.ctxt,
								    GCC_JIT_TYPE_UNSIGNED_INT),
					   i);
    gcc_jit_block_add_assignment (comp.block, NULL,
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

  return jit_emit_call (f_name, comp.lisp_obj_type, 2, args);
}

static comp_f_res_t
compile_f (const char *f_name, ptrdiff_t bytestr_length,
	   unsigned char *bytestr_data,
	   EMACS_INT stack_depth, Lisp_Object *vectorp,
	   ptrdiff_t vector_size, Lisp_Object args_template)
{
  gcc_jit_lvalue *res;
  comp_f_res_t comp_res = { NULL, 0, 0 };
  ptrdiff_t pc = 0;
  gcc_jit_rvalue *args[4];
  unsigned op;

  /* This is the stack we use to flat the bytecode written for push and pop
     Emacs VM.*/
  gcc_jit_rvalue **stack_base, **stack, **stack_over;
  stack_base = stack =
    (gcc_jit_rvalue **) xmalloc (stack_depth * sizeof (gcc_jit_rvalue *));
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


  /* Current function being compiled. Return a lips obj. */
  comp.func = jit_func_declare (f_name, comp.lisp_obj_type, comp_res.max_args,
				NULL, GCC_JIT_FUNCTION_EXPORTED, false);

  for (ptrdiff_t i = 0; i < comp_res.max_args; ++i)
    PUSH (gcc_jit_param_as_rvalue (gcc_jit_function_get_param (comp.func, i)));

  comp.block = gcc_jit_function_new_block(comp.func, "foo_blk");

  while (pc < bytestr_length)
    {
      op = FETCH;

      switch (op)
	{
	case Bstack_ref1:
	case Bstack_ref2:
	case Bstack_ref3:
	case Bstack_ref4:
	case Bstack_ref5:
	  {
	    PUSH (stack_base[(stack - stack_base) - (op - Bstack_ref) - 1]);
	    break;
	  }
	case Bstack_ref6:
	  {
	    PUSH (stack_base[(stack - stack_base) - FETCH - 1]);
	    break;
	  }
	case Bstack_ref7:
	  {
	    PUSH (stack_base[(stack - stack_base) - FETCH2 - 1]);
	    break;
	  }

	case Bvarref7:
	  op = FETCH2;
	  goto varref;

	case Bvarref:
	case Bvarref1:
	case Bvarref2:
	case Bvarref3:
	case Bvarref4:
	case Bvarref5:
	  op -= Bvarref;
	  goto varref;

	case Bvarref6:
	  op = FETCH;
	varref:
	  {
	    args[0] = gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
							  comp.lisp_obj_type,
							  vectorp[op]);
	    res = jit_emit_call ("Fsymbol_value", comp.lisp_obj_type, 1, args);
	    PUSH (gcc_jit_lvalue_as_rvalue (res));
	    break;
	  }

	case Bvarset:
	case Bvarset1:
	case Bvarset2:
	case Bvarset3:
	case Bvarset4:
	case Bvarset5:
	  op -= Bvarset;
	  goto varset;

	case Bvarset7:
	  op = FETCH2;
	  goto varset;

	case Bvarset6:
	  op = FETCH;
	varset:
	  {
	    POP1;
	    args[1] = args[0];
	    args[0] = gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
							  comp.lisp_obj_type,
							  vectorp[op]);
	    args[2] = comp.nil;
	    args[3] = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
							   comp.int_type,
							   SET_INTERNAL_SET);
	    res = jit_emit_call ("set_internal", comp.lisp_obj_type, 4, args);
	    PUSH (gcc_jit_lvalue_as_rvalue (res));
	  }
	  break;

	case Bvarbind6:
	  op = FETCH;
	  goto varbind;

	case Bvarbind7:
	  op = FETCH2;
	  goto varbind;

	case Bvarbind:
	case Bvarbind1:
	case Bvarbind2:
	case Bvarbind3:
	case Bvarbind4:
	case Bvarbind5:
	  op -= Bvarbind;
	varbind:
	  {
	    args[0] = gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
							  comp.lisp_obj_type,
							  vectorp[op]);
	    pop (1, &stack, &args[1]);
	    res = jit_emit_call ("specbind", comp.lisp_obj_type, 2, args);
	    PUSH (gcc_jit_lvalue_as_rvalue (res));
	    break;
	  }

	case Bcall6:
	  op = FETCH;
	  goto docall;

	case Bcall7:
	  op = FETCH2;
	  goto docall;

	case Bcall:
	case Bcall1:
	case Bcall2:
	case Bcall3:
	case Bcall4:
	case Bcall5:
	  op -= Bcall;
	docall:
	  {
	    ptrdiff_t nargs = op + 1;
	    pop (nargs, &stack, args);
	    res = jit_emit_callN ("Ffuncall", nargs, args);
	    PUSH (gcc_jit_lvalue_as_rvalue (res));
	    break;
	  }

	case Bunbind6:
	  op = FETCH;
	  goto dounbind;

	case Bunbind7:
	  op = FETCH2;
	  goto dounbind;

	case Bunbind:
	case Bunbind1:
	case Bunbind2:
	case Bunbind3:
	case Bunbind4:
	case Bunbind5:
	  op -= Bunbind;
	dounbind:
	  {
	    args[0] = gcc_jit_context_new_rvalue_from_int(comp.ctxt,
							  comp.ptrdiff_type,
							  op);

	    res = jit_emit_call ("unbind_n", comp.lisp_obj_type, 1, args);
	  }
	  break;
	case Bpophandler:
	  error ("Bpophandler\n");
	  break;
	case Bpushconditioncase:
	  error ("Bpushconditioncase\n");
	  break;
	case Bpushcatch:
	  error ("Bpushcatch\n");
	  break;

	CASE_CALL_NARGS (nth, 2);
	CASE_CALL_NARGS (symbolp, 1);
	CASE_CALL_NARGS (consp, 1);
	CASE_CALL_NARGS (stringp, 1);
	CASE_CALL_NARGS (listp, 1);
	CASE_CALL_NARGS (eq, 2);
	CASE_CALL_NARGS (memq, 1);
	CASE_CALL_NARGS (not, 1);
	CASE_CALL_NARGS (car, 1);
	CASE_CALL_NARGS (cdr, 1);
	CASE_CALL_NARGS (cons, 2);

	case BlistN:
	  op = FETCH;
	  goto make_list;

	case Blist1:
	case Blist2:
	case Blist3:
	case Blist4:
	  op = op - Blist1;
	make_list:
	  {
	    POP1;
	    args[1] = gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
							  comp.lisp_obj_type,
							  Qnil);
	    res = jit_emit_call ("Fcons", comp.lisp_obj_type, 2, args);
	    PUSH (gcc_jit_lvalue_as_rvalue (res));
	    for (int i = 0; i < op; ++i)
	      {
		POP2;
		res = jit_emit_call ("Fcons", comp.lisp_obj_type, 2, args);
		PUSH (gcc_jit_lvalue_as_rvalue (res));
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

	case Bconcat2:
	  EMIT_SCRATCH_CALL_N ("Fconcat", 2);
	  break;
	case Bconcat3:
	  EMIT_SCRATCH_CALL_N ("Fconcat", 3);
	  break;
	case Bconcat4:
	  EMIT_SCRATCH_CALL_N ("Fconcat", 4);
	  break;
	case BconcatN:
	  op = FETCH;
	  EMIT_SCRATCH_CALL_N ("Fconcat", op);
	  break;

	case Bsub1:
	  error ("Bsub1\n");
	  break;
	case Badd1:
	  error ("Badd1\n");
	  break;
	case Beqlsign:
	  error ("Beqlsign\n");
	  break;
	case Bgtr:
	  error ("Bgtr\n");
	  break;
	case Blss:
	  error ("Blss\n");
	  break;
	case Bleq:
	  error ("Bleq\n");
	  break;
	case Bgeq:
	  error ("Bgeq\n");
	  break;
	case Bdiff:
	  EMIT_SCRATCH_CALL_N ("Fminus", 2);
	  break;
	case Bnegate:
	  error ("Bnegate\n");
	  break;
	case Bplus:
	  EMIT_SCRATCH_CALL_N ("Fplus", 2);
	  break;
	case Bmax:
	  EMIT_SCRATCH_CALL_N ("Fmax", 2);
	  break;
	case Bmin:
	  EMIT_SCRATCH_CALL_N ("Fmin", 2);
	  break;
	case Bmult:
	  EMIT_SCRATCH_CALL_N ("Ftimes", 2);
	  break;
	case Bpoint:
	  error ("Bpoint\n");
	  break;

	CASE_CALL_NARGS (goto_char, 1);

	case Binsert:
	  EMIT_SCRATCH_CALL_N ("Finsert", 1);
	  break;

	case Bpoint_max:
	  error ("Bpoint_max\n");
	  break;
	case Bpoint_min:
	  error ("Bpoint_min\n");
	  break;

	CASE_CALL_NARGS (char_after, 1);
	CASE_CALL_NARGS (following_char, 0);

	case Bpreceding_char:
	  res = jit_emit_call ("Fprevious_char", comp.lisp_obj_type, 0, args);
	  PUSH (gcc_jit_lvalue_as_rvalue (res));
	  break;

	CASE_CALL_NARGS (current_column, 0);

	case Bindent_to:
	  POP1;
	  args[1] = comp.nil;
	  res = jit_emit_call ("Findent_to", comp.lisp_obj_type, 2, args);
	  break;

	CASE_CALL_NARGS (eolp, 0);

	case Beobp:
	  error ("Beobp\n");
	  break;

	CASE_CALL_NARGS (bolp, 0);

	case Bbobp:
	  error ("Bbobp\n");
	  break;

	CASE_CALL_NARGS (current_buffer, 0);
	CASE_CALL_NARGS (set_buffer, 1);

	case Bsave_current_buffer: /* Obsolete since ??.  */
	case Bsave_current_buffer_1:
	  jit_emit_call ("record_unwind_current_buffer",
			 comp.void_type, 0, NULL);
	  break;

	case Binteractive_p:
	  error ("Binteractive_p\n");
	  break;
	case Bforward_char:
	  error ("Bforward_char\n");
	  break;
	case Bforward_word:
	  error ("Bforward_word\n");
	  break;
	case Bskip_chars_forward:
	  error ("Bskip_chars_forward\n");
	  break;
	case Bskip_chars_backward:
	  error ("Bskip_chars_backward\n");
	  break;
	case Bforward_line:
	  error ("Bforward_line\n");
	  break;
	case Bchar_syntax:
	  error ("Bchar_syntax\n");
	  break;
	case Bbuffer_substring:
	  error ("Bbuffer_substring\n");
	  break;
	case Bdelete_region:
	  error ("Bdelete_region\n");
	  break;
	case Bnarrow_to_region:
	  error ("Bnarrow_to_region\n");
	  break;
	case Bwiden:
	  error ("Bwiden\n");
	  break;
	case Bend_of_line:
	  error ("Bend_of_line\n");
	  break;

	case Bconstant2:
	  goto do_constant;
	  break;

	case Bgoto:
	  error ("Bgoto\n");
	  break;
	case Bgotoifnil:
	  error ("Bgotoifnil\n");
	  break;
	case Bgotoifnonnil:
	  error ("Bgotoifnonnil\n");
	  break;
	case Bgotoifnilelsepop:
	  error ("Bgotoifnilelsepop\n");
	  break;
	case Bgotoifnonnilelsepop:
	  error ("Bgotoifnonnilelsepop\n");
	  break;

	case Breturn:
	  POP1;
	  gcc_jit_block_end_with_return(comp.block,
					NULL,
					args[0]);
	  break;

	case Bdiscard:
	  DISCARD (1);
	  break;

	case Bdup:
	  PUSH (*(stack - 1));
	  break;

	case Bsave_excursion:
	  error ("Bsave_excursion\n");
	  break;
	case Bsave_window_excursion:
	  error ("Bsave_window_excursion\n");
	  break;
	case Bsave_restriction:
	  error ("Bsave_restriction\n");
	  break;
	case Bcatch:
	  error ("Bcatch\n");
	  break;
	case Bunwind_protect:
	  error ("Bunwind_protect\n");
	  break;
	case Bcondition_case:
	  error ("Bcondition_case\n");
	  break;
	case Btemp_output_buffer_setup:
	  error ("Btemp_output_buffer_setup\n");
	  break;
	case Btemp_output_buffer_show:
	  error ("Btemp_output_buffer_show\n");
	  break;
	case Bunbind_all:
	  error ("Bunbind_all\n");
	  break;
	case Bset_marker:
	  error ("Bset_marker\n");
	  break;
	case Bmatch_beginning:
	  error ("Bmatch_beginning\n");
	  break;
	case Bmatch_end:
	  error ("Bmatch_end\n");
	  break;
	case Bupcase:
	  error ("Bupcase\n");
	  break;
	case Bdowncase:
	  error ("Bdowncase\n");
	  break;
	case Bstringeqlsign:
	  error ("Bstringeqlsign\n");
	  break;
	case Bstringlss:
	  error ("Bstringlss\n");
	  break;
	case Bequal:
	  error ("Bequal\n");
	  break;
	case Bnthcdr:
	  error ("Bnthcdr\n");
	  break;
	case Belt:
	  error ("Belt\n");
	  break;
	case Bmember:
	  error ("Bmember\n");
	  break;
	case Bassq:
	  error ("Bassq\n");
	  break;
	case Bnreverse:
	  error ("Bnreverse\n");
	  break;
	case Bsetcar:
	  error ("Bsetcar\n");
	  break;
	case Bsetcdr:
	  error ("Bsetcdr\n");
	  break;
	case Bcar_safe:
	  error ("Bcar_safe\n");
	  break;
	case Bcdr_safe:
	  error ("Bcdr_safe\n");
	  break;
	case Bnconc:
	  error ("Bnconc\n");
	  break;
	case Bquo:
	  error ("Bquo\n");
	  break;
	case Brem:
	  error ("Brem\n");
	  break;
	case Bnumberp:
	  error ("Bnumberp\n");
	  break;
	case Bintegerp:
	  error ("Bintegerp\n");
	  break;
	case BRgoto:
	  error ("BRgoto\n");
	  break;
	case BRgotoifnil:
	  error ("BRgotoifnil\n");
	  break;
	case BRgotoifnonnil:
	  error ("BRgotoifnonnil\n");
	  break;
	case BRgotoifnilelsepop:
	  error ("BRgotoifnilelsepop\n");
	  break;
	case BRgotoifnonnilelsepop:
	  error ("BRgotoifnonnilelsepop\n");
	  break;
	case BinsertN:
	  error ("BinsertN\n");
	  break;
	case Bstack_set:
	  error ("Bstack_set\n");
	  break;
	case Bstack_set2:
	  error ("Bstack_set2\n");
	  break;
	case BdiscardN:
	  error ("BdiscardN\n");
	  break;
	case Bswitch:
	  error ("Bswitch\n");
	  /* The cases of Bswitch that we handle (which in theory is
	     all of them) are done in Bconstant, below.  This is done
	     due to a design issue with Bswitch -- it should have
	     taken a constant pool index inline, but instead looks for
	     a constant on the stack.  */
	  goto fail;
	  break;
	default:
	case Bconstant:
	  {
	    if (op < Bconstant || op > Bconstant + vector_size)
	      goto fail;

	    op -= Bconstant;
	  do_constant:

	    /* See the Bswitch case for commentary.  */
	    if (pc >= bytestr_length || bytestr_data[pc] != Bswitch)
	      {
		gcc_jit_rvalue *c =
		  gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
						      comp.lisp_obj_type,
						      vectorp[op]);
		PUSH (c);
		/* Fprint(vectorp[op], Qnil); */
		break;
	      }

	    /* We're compiling Bswitch instead.  */
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
  return comp_res;
}

void
emacs_native_compile (const char *lisp_f_name, const char *c_f_name,
		      Lisp_Object func, bool dump_asm)
{
  Lisp_Object bytestr = AREF (func, COMPILED_BYTECODE);
  CHECK_STRING (bytestr);

  if (STRING_MULTIBYTE (bytestr))
    /* BYTESTR must have been produced by Emacs 20.2 or the earlier
       because they produced a raw 8-bit string for byte-code and now
       such a byte-code string is loaded as multibyte while raw 8-bit
       characters converted to multibyte form.  Thus, now we must
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
}

DEFUN ("native-compile", Fnative_compile, Snative_compile,
       1, 2, 0,
       doc: /* Compile as native code function FUNC and load it.  */) /* FIXME doc */
     (Lisp_Object func, Lisp_Object disassemble)
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
  for (int i; i < strlen(c_f_name); i++)
    if (c_f_name[i] == '-')
      c_f_name[i] = '_';

  func = indirect_function (func);
  if (!COMPILEDP (func))
    error ("Not a byte-compiled function");

  emacs_native_compile (lisp_f_name, c_f_name, func, disassemble != Qnil);

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
init_comp (void)
{
  comp.ctxt = gcc_jit_context_acquire();

#if EMACS_INT_MAX <= LONG_MAX
  /* 32-bit builds without wide ints, 64-bit builds on Posix hosts.  */
  comp.lisp_obj_type = gcc_jit_context_get_type(comp.ctxt, GCC_JIT_TYPE_VOID_PTR);
#else
  /* 64-bit builds on MS-Windows, 32-bit builds with wide ints.  */
  comp.lisp_obj_type = gcc_jit_context_get_type(comp.ctxt, GCC_JIT_TYPE_LONG_LONG);
#endif

  comp.void_type = gcc_jit_context_get_type(comp.ctxt, GCC_JIT_TYPE_VOID);
  comp.int_type = gcc_jit_context_get_type(comp.ctxt, GCC_JIT_TYPE_INT);
  comp.void_ptr_type =
    gcc_jit_context_get_type(comp.ctxt, GCC_JIT_TYPE_VOID_PTR);

  enum gcc_jit_types ptrdiff_t_gcc;
  if (sizeof (ptrdiff_t) == sizeof (int))
    ptrdiff_t_gcc = GCC_JIT_TYPE_INT;
  else if (sizeof (ptrdiff_t) == sizeof (long int))
    ptrdiff_t_gcc = GCC_JIT_TYPE_LONG;
  else if (sizeof (ptrdiff_t) == sizeof (long long int))
    ptrdiff_t_gcc = GCC_JIT_TYPE_LONG_LONG;
  else
    eassert ("ptrdiff_t size not handled.");

  comp.ptrdiff_type = gcc_jit_context_get_type(comp.ctxt, ptrdiff_t_gcc);

  comp.nil = gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
						 comp.lisp_obj_type,
						 Qnil);

  comp.scratch =
    gcc_jit_lvalue_get_address(
    gcc_jit_context_new_global (comp.ctxt, NULL,
				GCC_JIT_GLOBAL_IMPORTED,
				comp.lisp_obj_type,
				"scratch_call_area"),
    NULL);

  comp.func_hash = CALLN (Fmake_hash_table, QCtest, Qequal, QCweakness, Qt);

  if (COMP_DEBUG) {
    logfile = fopen ("libjit.log", "w");
    gcc_jit_context_set_logfile (comp.ctxt,
				 logfile,
				 0, 0);
    gcc_jit_context_set_bool_option (comp.ctxt,
				     GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING,
				     1);
  }

  gcc_jit_context_set_bool_option (comp.ctxt,
				   GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES,
				   1);
}

void
release_comp (void)
{
  if (comp.ctxt)
    gcc_jit_context_release(comp.ctxt);

  if (COMP_DEBUG)
    fclose (logfile);
}

void
syms_of_comp (void)
{
  defsubr (&Snative_compile);
  comp.func_hash = Qnil;
  staticpro (&comp.func_hash);
}

#endif /* HAVE_LIBJIT */
