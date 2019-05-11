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

#define MAX_FUN_NAME 256

#define DISASS_FILE_NAME "emacs-asm.s"

#define CHECK_STACK				\
  eassert (stack >= stack_base && stack < stack_over)

#define PUSH(obj)				\
  do {						\
    CHECK_STACK;				\
    *stack = obj;				\
    stack++;					\
  } while (0)

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

/* The compiler context  */

typedef struct {
  gcc_jit_context *ctxt;
  gcc_jit_type *lisp_obj;
  gcc_jit_type *int_type;
  gcc_jit_function *func; /* Current function being compiled  */
  gcc_jit_block *block; /* Current basic block  */
  Lisp_Object func_hash; /* f_name -> gcc_func  */
} comp_t;

static comp_t comp;

/* The result of one function compilation.  */

typedef struct {
  gcc_jit_result *gcc_res;
  short min_args, max_args;
} comp_f_res_t;

static gcc_jit_function *jit_func_declare (const char *f_name, unsigned nargs,
					   gcc_jit_rvalue **args,
					   enum gcc_jit_function_kind kind,
					   bool reusable);

void emacs_native_compile (const char *lisp_f_name, const char *c_f_name,
			   Lisp_Object func, bool dump_asm);

static gcc_jit_function *
jit_func_declare (const char *f_name, unsigned nargs, gcc_jit_rvalue **args,
		  enum  gcc_jit_function_kind kind,
		  bool reusable)
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
      type[i] = comp.lisp_obj;

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
				 comp.lisp_obj,
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
jit_emit_call (const char *f_name, unsigned nargs, gcc_jit_rvalue **args)
{
  Lisp_Object key = make_string (f_name, strlen (f_name));
  EMACS_UINT hash = 0;
  struct Lisp_Hash_Table *ht = XHASH_TABLE (comp.func_hash);
  ptrdiff_t i = hash_lookup (ht, key, &hash);

  if (i == -1)
    {
      jit_func_declare(f_name, nargs, args, GCC_JIT_FUNCTION_IMPORTED,
		       true);
      i = hash_lookup (ht, key, &hash);
      eassert (i != -1);
    }

  Lisp_Object value = HASH_VALUE (ht, hash_lookup (ht, key, &hash));
  gcc_jit_function *func = (gcc_jit_function *) XFIXNUMPTR (value);

  gcc_jit_lvalue *res = gcc_jit_function_new_local(comp.func,
						   NULL,
						   comp.lisp_obj,
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
  comp.func = jit_func_declare (f_name, comp_res.max_args, NULL,
				GCC_JIT_FUNCTION_EXPORTED, false);

  for (ptrdiff_t i = 0; i < comp_res.max_args; ++i)
    PUSH (gcc_jit_param_as_rvalue (gcc_jit_function_get_param (comp.func, i)));

  comp.block = gcc_jit_function_new_block(comp.func, "foo_blk");

  while (pc < bytestr_length)
    {
      op = FETCH;
      printf ("pc %td\t%ud\n", pc, op);
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
							  comp.lisp_obj,
							  vectorp[op]);
	    res = jit_emit_call ("Fsymbol_value", 1, args);
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
							  comp.lisp_obj,
							  vectorp[op]);
	    args[2] = gcc_jit_context_new_rvalue_from_ptr(comp.ctxt,
							  comp.lisp_obj,
							  Qnil);
	    args[3] = gcc_jit_context_new_rvalue_from_int (comp.ctxt,
							   comp.int_type,
							   SET_INTERNAL_SET);
	    res = jit_emit_call ("set_internal", 4, args);
	    PUSH (gcc_jit_lvalue_as_rvalue (res));
	  }
	  break;

	case Bvarbind:
	  printf("Bvarbind\n");
	  break;
	case Bvarbind1:
	  printf("Bvarbind1\n");
	  break;
	case Bvarbind2:
	  printf("Bvarbind2\n");
	  break;
	case Bvarbind3:
	  printf("Bvarbind3\n");
	  break;
	case Bvarbind4:
	  printf("Bvarbind4\n");
	  break;
	case Bvarbind5:
	  printf("Bvarbind5\n");
	  break;
	case Bvarbind6:
	  printf("Bvarbind6\n");
	  break;
	case Bvarbind7:
	  printf("Bvarbind7\n");
	  break;
	case Bcall:
	  printf("Bcall\n");
	  break;
	case Bcall1:
	  printf("Bcall1\n");
	  break;
	case Bcall2:
	  printf("Bcall2\n");
	  break;
	case Bcall3:
	  printf("Bcall3\n");
	  break;
	case Bcall4:
	  printf("Bcall4\n");
	  break;
	case Bcall5:
	  printf("Bcall5\n");
	  break;
	case Bcall6:
	  printf("Bcall6\n");
	  break;
	case Bcall7:
	  printf("Bcall7\n");
	  break;
	case Bunbind:
	  printf("Bunbind\n");
	  break;
	case Bunbind1:
	  printf("Bunbind1\n");
	  break;
	case Bunbind2:
	  printf("Bunbind2\n");
	  break;
	case Bunbind3:
	  printf("Bunbind3\n");
	  break;
	case Bunbind4:
	  printf("Bunbind4\n");
	  break;
	case Bunbind5:
	  printf("Bunbind5\n");
	  break;
	case Bunbind6:
	  printf("Bunbind6\n");
	  break;
	case Bunbind7:
	  printf("Bunbind7\n");
	  break;
	case Bpophandler:
	  printf("Bpophandler\n");
	  break;
	case Bpushconditioncase:
	  printf("Bpushconditioncase\n");
	  break;
	case Bpushcatch:
	  printf("Bpushcatch\n");
	  break;
	case Bnth:
	  printf("Bnth\n");
	  break;
	case Bsymbolp:
	  printf("Bsymbolp\n");
	  break;
	case Bconsp:
	  printf("Bconsp\n");
	  break;
	case Bstringp:
	  printf("Bstringp\n");
	  break;
	case Blistp:
	  printf("Blistp\n");
	  break;
	case Beq:
	  POP2;
	  res = jit_emit_call ("Feq", 2, args);
	  PUSH (gcc_jit_lvalue_as_rvalue (res));
	  break;
	case Bmemq:
	  POP1;
	  res = jit_emit_call ("Fmemq", 1, args);
	  PUSH (gcc_jit_lvalue_as_rvalue (res));
	  break;
	  break;
	case Bnot:
	  printf("Bnot\n");
	  break;
	case Bcar:
	  POP1;
	  res = jit_emit_call ("Fcar", 1, args);
	  PUSH (gcc_jit_lvalue_as_rvalue (res));
	  break;
	case Bcdr:
	  POP1;
	  res = jit_emit_call ("Fcdr", 1, args);
	  PUSH (gcc_jit_lvalue_as_rvalue (res));
	  break;
	case Bcons:
	  POP2;
	  res = jit_emit_call ("Fcons", 2, args);
	  PUSH (gcc_jit_lvalue_as_rvalue (res));
	  break;

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
							  comp.lisp_obj,
							  Qnil);
	    res = jit_emit_call ("Fcons", 2, args);
	    PUSH (gcc_jit_lvalue_as_rvalue (res));
	    for (int i = 0; i < op; ++i)
	      {
		POP2;
		res = jit_emit_call ("Fcons", 2, args);
		PUSH (gcc_jit_lvalue_as_rvalue (res));
	      }
	    break;
	  }

	case Blength:
	  printf("Blength\n");
	  break;
	case Baref:
	  printf("Baref\n");
	  break;
	case Baset:
	  printf("Baset\n");
	  break;
	case Bsymbol_value:
	  printf("Bsymbol_value\n");
	  break;
	case Bsymbol_function:
	  printf("Bsymbol_function\n");
	  break;
	case Bset:
	  printf("Bset\n");
	  break;
	case Bfset:
	  printf("Bfset\n");
	  break;
	case Bget:
	  printf("Bget\n");
	  break;
	case Bsubstring:
	  printf("Bsubstring\n");
	  break;
	case Bconcat2:
	  printf("Bconcat2\n");
	  break;
	case Bconcat3:
	  printf("Bconcat3\n");
	  break;
	case Bconcat4:
	  printf("Bconcat4\n");
	  break;
	case Bsub1:
	  printf("Bsub1\n");
	  break;
	case Badd1:
	  printf("Badd1\n");
	  break;
	case Beqlsign:
	  printf("Beqlsign\n");
	  break;
	case Bgtr:
	  printf("Bgtr\n");
	  break;
	case Blss:
	  printf("Blss\n");
	  break;
	case Bleq:
	  printf("Bleq\n");
	  break;
	case Bgeq:
	  printf("Bgeq\n");
	  break;
	case Bdiff:
	  printf("Bdiff\n");
	  break;
	case Bnegate:
	  printf("Bnegate\n");
	  break;
	case Bplus:
	  printf("Bplus\n");
	  break;
	case Bmax:
	  printf("Bmax\n");
	  break;
	case Bmin:
	  printf("Bmin\n");
	  break;
	case Bmult:
	  printf("Bmult\n");
	  break;
	case Bpoint:
	  printf("Bpoint\n");
	  break;
	case Bsave_current_buffer:
	  printf("Bsave_current_buffer\n");
	  break;
	case Bgoto_char:
	  printf("Bgoto_char\n");
	  break;
	case Binsert:
	  printf("Binsert\n");
	  break;
	case Bpoint_max:
	  printf("Bpoint_max\n");
	  break;
	case Bpoint_min:
	  printf("Bpoint_min\n");
	  break;
	case Bchar_after:
	  printf("Bchar_after\n");
	  break;
	case Bfollowing_char:
	  printf("Bfollowing_char\n");
	  break;
	case Bpreceding_char:
	  printf("Bpreceding_char\n");
	  break;
	case Bcurrent_column:
	  printf("Bcurrent_column\n");
	  break;
	case Bindent_to:
	  printf("Bindent_to\n");
	  break;
	case Beolp:
	  printf("Beolp\n");
	  break;
	case Beobp:
	  printf("Beobp\n");
	  break;
	case Bbolp:
	  printf("Bbolp\n");
	  break;
	case Bbobp:
	  printf("Bbobp\n");
	  break;
	case Bcurrent_buffer:
	  printf("Bcurrent_buffer\n");
	  break;
	case Bset_buffer:
	  printf("Bset_buffer\n");
	  break;
	case Bsave_current_buffer_1:
	  printf("Bsave_current_buffer_1\n");
	  break;
	case Binteractive_p:
	  printf("Binteractive_p\n");
	  break;
	case Bforward_char:
	  printf("Bforward_char\n");
	  break;
	case Bforward_word:
	  printf("Bforward_word\n");
	  break;
	case Bskip_chars_forward:
	  printf("Bskip_chars_forward\n");
	  break;
	case Bskip_chars_backward:
	  printf("Bskip_chars_backward\n");
	  break;
	case Bforward_line:
	  printf("Bforward_line\n");
	  break;
	case Bchar_syntax:
	  printf("Bchar_syntax\n");
	  break;
	case Bbuffer_substring:
	  printf("Bbuffer_substring\n");
	  break;
	case Bdelete_region:
	  printf("Bdelete_region\n");
	  break;
	case Bnarrow_to_region:
	  printf("Bnarrow_to_region\n");
	  break;
	case Bwiden:
	  printf("Bwiden\n");
	  break;
	case Bend_of_line:
	  printf("Bend_of_line\n");
	  break;
	case Bconstant2:
	  printf("Bconstant2\n");
	  goto do_constant;
	  break;
	case Bgoto:
	  printf("Bgoto\n");
	  break;
	case Bgotoifnil:
	  printf("Bgotoifnil\n");
	  break;
	case Bgotoifnonnil:
	  printf("Bgotoifnonnil\n");
	  break;
	case Bgotoifnilelsepop:
	  printf("Bgotoifnilelsepop\n");
	  break;
	case Bgotoifnonnilelsepop:
	  printf("Bgotoifnonnilelsepop\n");
	  break;
	case Breturn:
	  printf("Breturn\n");
	  break;
	case Bdiscard:
	  printf("Bdiscard\n");
	  break;
	case Bdup:
	  printf("Bdup\n");
	  break;
	case Bsave_excursion:
	  printf("Bsave_excursion\n");
	  break;
	case Bsave_window_excursion:
	  printf("Bsave_window_excursion\n");
	  break;
	case Bsave_restriction:
	  printf("Bsave_restriction\n");
	  break;
	case Bcatch:
	  printf("Bcatch\n");
	  break;
	case Bunwind_protect:
	  printf("Bunwind_protect\n");
	  break;
	case Bcondition_case:
	  printf("Bcondition_case\n");
	  break;
	case Btemp_output_buffer_setup:
	  printf("Btemp_output_buffer_setup\n");
	  break;
	case Btemp_output_buffer_show:
	  printf("Btemp_output_buffer_show\n");
	  break;
	case Bunbind_all:
	  printf("Bunbind_all\n");
	  break;
	case Bset_marker:
	  printf("Bset_marker\n");
	  break;
	case Bmatch_beginning:
	  printf("Bmatch_beginning\n");
	  break;
	case Bmatch_end:
	  printf("Bmatch_end\n");
	  break;
	case Bupcase:
	  printf("Bupcase\n");
	  break;
	case Bdowncase:
	  printf("Bdowncase\n");
	  break;
	case Bstringeqlsign:
	  printf("Bstringeqlsign\n");
	  break;
	case Bstringlss:
	  printf("Bstringlss\n");
	  break;
	case Bequal:
	  printf("Bequal\n");
	  break;
	case Bnthcdr:
	  printf("Bnthcdr\n");
	  break;
	case Belt:
	  printf("Belt\n");
	  break;
	case Bmember:
	  printf("Bmember\n");
	  break;
	case Bassq:
	  printf("Bassq\n");
	  break;
	case Bnreverse:
	  printf("Bnreverse\n");
	  break;
	case Bsetcar:
	  printf("Bsetcar\n");
	  break;
	case Bsetcdr:
	  printf("Bsetcdr\n");
	  break;
	case Bcar_safe:
	  printf("Bcar_safe\n");
	  break;
	case Bcdr_safe:
	  printf("Bcdr_safe\n");
	  break;
	case Bnconc:
	  printf("Bnconc\n");
	  break;
	case Bquo:
	  printf("Bquo\n");
	  break;
	case Brem:
	  printf("Brem\n");
	  break;
	case Bnumberp:
	  printf("Bnumberp\n");
	  break;
	case Bintegerp:
	  printf("Bintegerp\n");
	  break;
	case BRgoto:
	  printf("BRgoto\n");
	  break;
	case BRgotoifnil:
	  printf("BRgotoifnil\n");
	  break;
	case BRgotoifnonnil:
	  printf("BRgotoifnonnil\n");
	  break;
	case BRgotoifnilelsepop:
	  printf("BRgotoifnilelsepop\n");
	  break;
	case BRgotoifnonnilelsepop:
	  printf("BRgotoifnonnilelsepop\n");
	  break;
	case BconcatN:
	  printf("BconcatN\n");
	  break;
	case BinsertN:
	  printf("BinsertN\n");
	  break;
	case Bstack_set:
	  printf("Bstack_set\n");
	  break;
	case Bstack_set2:
	  printf("Bstack_set2\n");
	  break;
	case BdiscardN:
	  printf("BdiscardN\n");
	  break;
	case Bswitch:
	  printf("Bswitch\n");
	  /* The cases of Bswitch that we handle (which in theory is
	     all of them) are done in Bconstant, below.  This is done
	     due to a design issue with Bswitch -- it should have
	     taken a constant pool index inline, but instead looks for
	     a constant on the stack.  */
	  goto fail;
	  break;
	default:
	case Bconstant:
	  printf("Bconstant ");
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
						      comp.lisp_obj,
						      vectorp[op]);
		PUSH (c);
		Fprint(vectorp[op], Qnil);
		break;
	      }

	    /* We're compiling Bswitch instead.  */
	    ++pc;
	    break;
	  }
	}
    }

  stack--;
  gcc_jit_block_end_with_return(comp.block,
				NULL,
				*stack);
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
  comp.lisp_obj = gcc_jit_context_get_type(comp.ctxt, GCC_JIT_TYPE_VOID_PTR);
#else
  /* 64-bit builds on MS-Windows, 32-bit builds with wide ints.  */
  comp.lisp_obj = gcc_jit_context_get_type(comp.ctxt, GCC_JIT_TYPE_LONG_LONG);
#endif

  comp.int_type = gcc_jit_context_get_type(comp.ctxt, GCC_JIT_TYPE_INT);
  comp.func_hash = CALLN (Fmake_hash_table, QCtest, Qequal, QCweakness, Qt);

  /* gcc_jit_context_set_bool_option(comp.ctxt, */
  /* 				  GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING, */
  /* 				  1); */

  gcc_jit_context_set_bool_option(comp.ctxt,
				  GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES,
				  1);
}

void
release_comp (void)
{
  if (comp.ctxt)
    gcc_jit_context_release(comp.ctxt);
}

void
syms_of_comp (void)
{
  defsubr (&Snative_compile);
  comp.func_hash = Qnil;
  staticpro (&comp.func_hash);
}

#endif /* HAVE_LIBJIT */
