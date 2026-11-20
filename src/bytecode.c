/* Execution of byte code produced by bytecomp.el.
   Copyright (C) 1985-1988, 1993, 2000-2026 Free Software Foundation,
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

#include "lisp.h"
#include "sysstdio.h"
#include "buffer.h"
#include "window.h"

/* Define BYTE_CODE_SAFE true to enable some minor sanity checking,
   useful for debugging the byte compiler.  It defaults to false.  */

#ifndef BYTE_CODE_SAFE
# define BYTE_CODE_SAFE false
#endif

/* Define BYTE_CODE_METER to generate a byte-op usage histogram.  */
/* #define BYTE_CODE_METER */

/* If BYTE_CODE_THREADED is defined, then the interpreter will be
   indirect threaded, using GCC's computed goto extension.  This code,
   as currently implemented, is incompatible with BYTE_CODE_SAFE and
   BYTE_CODE_METER.  */
#if (defined __GNUC__ && !defined __STRICT_ANSI__ \
     && !BYTE_CODE_SAFE && !defined BYTE_CODE_METER)
#define BYTE_CODE_THREADED
#endif


#ifdef BYTE_CODE_METER

#define METER_2(code1, code2) \
  (*aref_addr (AREF (Vbyte_code_meter, code1), code2))
#define METER_1(code) METER_2 (0, code)

#define METER_CODE(last_code, this_code)				\
{									\
  if (byte_metering_on)							\
    {									\
      if (XFIXNAT (METER_1 (this_code)) < MOST_POSITIVE_FIXNUM)	\
        XSETFASTINT (METER_1 (this_code),				\
		     XFIXNAT (METER_1 (this_code)) + 1);		\
      if (last_code							\
	  && (XFIXNAT (METER_2 (last_code, this_code))			\
	      < MOST_POSITIVE_FIXNUM))					\
        XSETFASTINT (METER_2 (last_code, this_code),			\
		     XFIXNAT (METER_2 (last_code, this_code)) + 1);	\
    }									\
}

#endif /* BYTE_CODE_METER */


/*  Byte codes: */

#define BYTE_CODES							\
DEFINE (Bstack_ref, 0) /* Actually, Bstack_ref+0 is not implemented: use dup.  */ \
DEFINE (Bstack_ref1, 1)							\
DEFINE (Bstack_ref2, 2)							\
DEFINE (Bstack_ref3, 3)							\
DEFINE (Bstack_ref4, 4)							\
DEFINE (Bstack_ref5, 5)							\
DEFINE (Bstack_ref6, 6)							\
DEFINE (Bstack_ref7, 7)							\
DEFINE (Bvarref, 010)							\
DEFINE (Bvarref1, 011)							\
DEFINE (Bvarref2, 012)							\
DEFINE (Bvarref3, 013)							\
DEFINE (Bvarref4, 014)							\
DEFINE (Bvarref5, 015)							\
DEFINE (Bvarref6, 016)							\
DEFINE (Bvarref7, 017)							\
DEFINE (Bvarset, 020)							\
DEFINE (Bvarset1, 021)							\
DEFINE (Bvarset2, 022)							\
DEFINE (Bvarset3, 023)							\
DEFINE (Bvarset4, 024)							\
DEFINE (Bvarset5, 025)							\
DEFINE (Bvarset6, 026)							\
DEFINE (Bvarset7, 027)							\
DEFINE (Bvarbind, 030)							\
DEFINE (Bvarbind1, 031)							\
DEFINE (Bvarbind2, 032)							\
DEFINE (Bvarbind3, 033)							\
DEFINE (Bvarbind4, 034)							\
DEFINE (Bvarbind5, 035)							\
DEFINE (Bvarbind6, 036)							\
DEFINE (Bvarbind7, 037)							\
DEFINE (Bcall, 040)							\
DEFINE (Bcall1, 041)							\
DEFINE (Bcall2, 042)							\
DEFINE (Bcall3, 043)							\
DEFINE (Bcall4, 044)							\
DEFINE (Bcall5, 045)							\
DEFINE (Bcall6, 046)							\
DEFINE (Bcall7, 047)							\
DEFINE (Bunbind, 050)							\
DEFINE (Bunbind1, 051)							\
DEFINE (Bunbind2, 052)							\
DEFINE (Bunbind3, 053)							\
DEFINE (Bunbind4, 054)							\
DEFINE (Bunbind5, 055)							\
DEFINE (Bunbind6, 056)							\
DEFINE (Bunbind7, 057)							\
									\
DEFINE (Bpophandler, 060)						\
DEFINE (Bpushconditioncase, 061)					\
DEFINE (Bpushcatch, 062)						\
									\
DEFINE (Bnth, 070)							\
DEFINE (Bsymbolp, 071)							\
DEFINE (Bconsp, 072)							\
DEFINE (Bstringp, 073)							\
DEFINE (Blistp, 074)							\
DEFINE (Beq, 075)							\
DEFINE (Bmemq, 076)							\
DEFINE (Bnot, 077)							\
DEFINE (Bcar, 0100)							\
DEFINE (Bcdr, 0101)							\
DEFINE (Bcons, 0102)							\
DEFINE (Blist1, 0103)							\
DEFINE (Blist2, 0104)							\
DEFINE (Blist3, 0105)							\
DEFINE (Blist4, 0106)							\
DEFINE (Blength, 0107)							\
DEFINE (Baref, 0110)							\
DEFINE (Baset, 0111)							\
DEFINE (Bsymbol_value, 0112)						\
DEFINE (Bsymbol_function, 0113)						\
DEFINE (Bset, 0114)							\
DEFINE (Bfset, 0115)							\
DEFINE (Bget, 0116)							\
DEFINE (Bsubstring, 0117)						\
DEFINE (Bconcat2, 0120)							\
DEFINE (Bconcat3, 0121)							\
DEFINE (Bconcat4, 0122)							\
DEFINE (Bsub1, 0123)							\
DEFINE (Badd1, 0124)							\
DEFINE (Beqlsign, 0125)							\
DEFINE (Bgtr, 0126)							\
DEFINE (Blss, 0127)							\
DEFINE (Bleq, 0130)							\
DEFINE (Bgeq, 0131)							\
DEFINE (Bdiff, 0132)							\
DEFINE (Bnegate, 0133)							\
DEFINE (Bplus, 0134)							\
DEFINE (Bmax, 0135)							\
DEFINE (Bmin, 0136)							\
DEFINE (Bmult, 0137)							\
									\
DEFINE (Bpoint, 0140)							\
/* 0141 was Bmark in v17, Bsave_current_buffer in 18-19.  */		\
DEFINE (Bsave_current_buffer_OBSOLETE, 0141)  /* Obsolete since 20. */	\
DEFINE (Bgoto_char, 0142)						\
DEFINE (Binsert, 0143)							\
DEFINE (Bpoint_max, 0144)						\
DEFINE (Bpoint_min, 0145)						\
DEFINE (Bchar_after, 0146)						\
DEFINE (Bfollowing_char, 0147)						\
DEFINE (Bpreceding_char, 0150)						\
DEFINE (Bcurrent_column, 0151)						\
DEFINE (Bindent_to, 0152)						\
/* 0153 was Bscan_buffer in v17.  */                                    \
DEFINE (Beolp, 0154)							\
DEFINE (Beobp, 0155)							\
DEFINE (Bbolp, 0156)							\
DEFINE (Bbobp, 0157)							\
DEFINE (Bcurrent_buffer, 0160)						\
DEFINE (Bset_buffer, 0161)						\
DEFINE (Bsave_current_buffer, 0162)					\
/* 0163 was Bset_mark in v17.  */                                       \
DEFINE (Binteractive_p, 0164) /* Obsolete since Emacs-24.1.  */		\
									\
DEFINE (Bforward_char, 0165)						\
DEFINE (Bforward_word, 0166)						\
DEFINE (Bskip_chars_forward, 0167)					\
DEFINE (Bskip_chars_backward, 0170)					\
DEFINE (Bforward_line, 0171)						\
DEFINE (Bchar_syntax, 0172)						\
DEFINE (Bbuffer_substring, 0173)					\
DEFINE (Bdelete_region, 0174)						\
DEFINE (Bnarrow_to_region, 0175)					\
DEFINE (Bwiden, 0176)							\
DEFINE (Bend_of_line, 0177)						\
									\
DEFINE (Bconstant2, 0201)						\
DEFINE (Bgoto, 0202)							\
DEFINE (Bgotoifnil, 0203)						\
DEFINE (Bgotoifnonnil, 0204)						\
DEFINE (Bgotoifnilelsepop, 0205)					\
DEFINE (Bgotoifnonnilelsepop, 0206)					\
DEFINE (Breturn, 0207)							\
DEFINE (Bdiscard, 0210)							\
DEFINE (Bdup, 0211)							\
									\
DEFINE (Bsave_excursion, 0212)						\
DEFINE (Bsave_window_excursion, 0213) /* Obsolete since Emacs-24.1.  */	\
DEFINE (Bsave_restriction, 0214)					\
DEFINE (Bcatch, 0215)		/* Obsolete since Emacs-25.  */         \
									\
DEFINE (Bunwind_protect, 0216)						\
DEFINE (Bcondition_case, 0217)	/* Obsolete since Emacs-25.  */         \
DEFINE (Btemp_output_buffer_setup, 0220) /* Obsolete since Emacs-24.1.  */ \
DEFINE (Btemp_output_buffer_show, 0221)  /* Obsolete since Emacs-24.1.  */ \
									\
/* 0222 was Bunbind_all, never used. */                                 \
									\
DEFINE (Bset_marker, 0223)						\
DEFINE (Bmatch_beginning, 0224)						\
DEFINE (Bmatch_end, 0225)						\
DEFINE (Bupcase, 0226)							\
DEFINE (Bdowncase, 0227)						\
									\
DEFINE (Bstringeqlsign, 0230)						\
DEFINE (Bstringlss, 0231)						\
DEFINE (Bequal, 0232)							\
DEFINE (Bnthcdr, 0233)							\
DEFINE (Belt, 0234)							\
DEFINE (Bmember, 0235)							\
DEFINE (Bassq, 0236)							\
DEFINE (Bnreverse, 0237)						\
DEFINE (Bsetcar, 0240)							\
DEFINE (Bsetcdr, 0241)							\
DEFINE (Bcar_safe, 0242)						\
DEFINE (Bcdr_safe, 0243)						\
DEFINE (Bnconc, 0244)							\
DEFINE (Bquo, 0245)							\
DEFINE (Brem, 0246)							\
DEFINE (Bnumberp, 0247)							\
DEFINE (Bintegerp, 0250)						\
									\
/* 0252-0256 were relative jumps, apparently never used.  */            \
									\
DEFINE (BlistN, 0257)							\
DEFINE (BconcatN, 0260)							\
DEFINE (BinsertN, 0261)							\
									\
/* Bstack_ref is code 0.  */						\
DEFINE (Bstack_set,  0262)						\
DEFINE (Bstack_set2, 0263)						\
DEFINE (BdiscardN,   0266)						\
									\
DEFINE (Bswitch, 0267)                                                  \
                                                                        \
DEFINE (Bconstant, 0300)

enum byte_code_op
{
#define DEFINE(name, value) name = value,
    BYTE_CODES
#undef DEFINE
};

/* Fetch the next byte from the bytecode stream.  */

#define FETCH (*pc++)

/* Fetch two bytes from the bytecode stream and make a 16-bit number
   out of them.  */

#define FETCH2 (pc += 2, pc[-2] | pc[-1] << 8)

/* Push X onto the execution stack.  The expression X should not
   contain TOP, to avoid competing side effects.  */

#define PUSH(x) (*++top = (x))

/* Pop a value off the execution stack.  */

#define POP (*top--)

/* Discard n values from the execution stack.  */

#define DISCARD(n) (top -= (n))

/* Get the value which is at the top of the execution stack, but don't
   pop it.  */

#define TOP (*top)

DEFUN ("byte-code", Fbyte_code, Sbyte_code, 3, 3, 0,
       doc: /* Function used internally in byte-compiled code.
The first argument, BYTESTR, is a string of byte code;
the second, VECTOR, a vector of constants;
the third, MAXDEPTH, the maximum stack depth used in this function.
If the third argument is incorrect, Emacs may crash.  */)
  (Lisp_Object bytestr, Lisp_Object vector, Lisp_Object maxdepth)
{
  if (! (STRINGP (bytestr) && VECTORP (vector) && FIXNATP (maxdepth)))
    error ("Invalid byte-code");

  if (STRING_MULTIBYTE (bytestr))
    {
      /* BYTESTR must have been produced by Emacs 20.2 or earlier
	 because it produced a raw 8-bit string for byte-code and now
	 such a byte-code string is loaded as multibyte with raw 8-bit
	 characters converted to multibyte form.  Convert them back to
	 the original unibyte form.  */
      bytestr = Fstring_as_unibyte (bytestr);
    }
  Lisp_Object fun = CALLN (Fmake_byte_code, Qnil, bytestr, vector, maxdepth);
  return exec_byte_code (fun, 0, 0, NULL);
}

static void
bcall0 (Lisp_Object f)
{
  calln (f);
}

/* The bytecode stack size in bytes.
   This is a fairly generous amount, but:
   - if users need more, we could allocate more, or just reserve the address
     space and allocate on demand
   - if threads are used more, then it might be a good idea to reduce the
     per-thread overhead in time and space
   - for maximum flexibility but a small runtime penalty, we could allocate
     the stack in smaller chunks as needed
*/
#define BC_STACK_SIZE (512 * 1024 * sizeof (Lisp_Object))

/* Bytecode interpreter stack:

           |--------------|         --
           |fun           |           |                   ^ stack growth
           |saved_pc      |           |                   | direction
           |saved_top    -------      |
     fp--->|saved_fp     ----   |     | current frame
           |--------------|  |  |     | (called from bytecode in this example)
           |   (free)     |  |  |     |
     top-->| ...stack...  |  |  |     |
           : ...          :  |  |     |
           |incoming args |  |  |     |
           |--------------|  |  |   --
           |fun           |  |  |     |
           |saved_pc      |  |  |     |
           |saved_top     |  |  |     |
           |saved_fp      |<-   |     | previous frame
           |--------------|     |     |
           |   (free)     |     |     |
           | ...stack...  |<----      |
           : ...          :           |
           |incoming args |           |
           |--------------|         --
           :              :
*/

/* bytecode stack frame header (footer, actually) */
struct bc_frame {
  struct bc_frame *saved_fp;        /* previous frame pointer,
                                       NULL if bottommost frame */

  /* In a frame called directly from C, the following two members are NULL.  */
  Lisp_Object *saved_top;           /* previous stack pointer */
  const unsigned char *saved_pc;    /* previous program counter */

  Lisp_Object fun;                  /* current function object */

  Lisp_Object next_stack[];	    /* data stack of next frame */
};

void
init_bc_thread (struct bc_thread_state *bc)
{
  bc->stack = xmalloc (BC_STACK_SIZE);
  bc->stack_end = bc->stack + BC_STACK_SIZE;
  /* Put a dummy header at the bottom to indicate the first free location.  */
  bc->fp = (struct bc_frame *)bc->stack;
  memset (bc->fp, 0, sizeof *bc->fp);
}

void
free_bc_thread (struct bc_thread_state *bc)
{
  xfree (bc->stack);
}

void
mark_bytecode (struct bc_thread_state *bc)
{
  struct bc_frame *fp = bc->fp;
  Lisp_Object *top = NULL;     /* stack pointer of topmost frame not known */
  for (;;)
    {
      struct bc_frame *next_fp = fp->saved_fp;
      /* Only the dummy frame at the bottom has saved_fp = NULL.  */
      if (!next_fp)
	break;
      mark_object (fp->fun);
      Lisp_Object *frame_base = next_fp->next_stack;
      if (top)
	{
	  /* The stack pointer of a frame is known: mark the part of the stack
	     above it conservatively.  This includes any outgoing arguments.  */
	  mark_memory (top + 1, fp);
	  /* Mark the rest of the stack precisely.  */
	  mark_objects (frame_base, top + 1 - frame_base);
	}
      else
	{
	  /* The stack pointer is unknown -- mark everything conservatively.  */
	  mark_memory (frame_base, fp);
	}
      top = fp->saved_top;
      fp = next_fp;
    }
}

DEFUN ("internal-stack-stats", Finternal_stack_stats, Sinternal_stack_stats,
       0, 0, 0,
       doc: /* internal */)
  (void)
{
  struct bc_thread_state *bc = &current_thread->bc;
  int nframes = 0;
  int nruns = 0;
  for (struct bc_frame *fp = bc->fp; fp; fp = fp->saved_fp)
    {
      nframes++;
      if (fp->saved_top == NULL)
	nruns++;
    }
  fprintf (stderr, "%d stack frames, %d runs\n", nframes, nruns);
  return Qnil;
}

/* Whether a stack pointer is valid in the current frame.  */
static bool
valid_sp (struct bc_thread_state *bc, Lisp_Object *sp)
{
  struct bc_frame *fp = bc->fp;
  return sp < (Lisp_Object *)fp && sp + 1 >= fp->saved_fp->next_stack;
}

/* GCC seems to have difficulty putting important variables in
   registers, so give it some heavy-handed assistance by specifying
   which ones to use.  Use callee-saved registers to reduce spill/fill.  */
#if __GNUC__ && !__clang__ && defined __x86_64__
#define BC_REG_TOP asm ("rbx")
#define BC_REG_PC asm ("r12")
#elif __GNUC__ && !__clang__ && defined __aarch64__
#define BC_REG_TOP asm ("x19")
#define BC_REG_PC asm ("x20")
#else
#define BC_REG_TOP
#define BC_REG_PC
#endif

/* It seems difficult to avoid spurious -Wclobbered diagnostics from GCC
   in exec_byte_code, so turn the warning off around that function.
   See <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=21161>.  */
#if __GNUC__ && !__clang__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wclobbered"
#endif

/* Execute the byte-code in FUN.  ARGS_TEMPLATE is the function arity
   encoded as an integer (the one in FUN is ignored), and ARGS, of
   size NARGS, should be a vector of the actual arguments.  The
   arguments in ARGS are pushed on the stack according to
   ARGS_TEMPLATE before executing FUN.  */

Lisp_Object
exec_byte_code (Lisp_Object fun, ptrdiff_t args_template,
		ptrdiff_t nargs, Lisp_Object *args)
{
#ifdef BYTE_CODE_METER
  int volatile this_op = 0;
#endif
  unsigned char quitcounter = 1;
  struct bc_thread_state *bc = &current_thread->bc;

  /* Values used for the first stack record when called from C.  */
  register Lisp_Object *top BC_REG_TOP = NULL;
  register unsigned char const *pc BC_REG_PC = NULL;

  Lisp_Object bytestr = AREF (fun, CLOSURE_CODE);

 setup_frame: ;
  eassert (!STRING_MULTIBYTE (bytestr));
  eassert (string_immovable_p (bytestr));
  /* FIXME: in debug mode (!NDEBUG, BYTE_CODE_SAFE or enabled checking),
     save the specpdl index on function entry and check that it is the same
     when returning, to detect unwind imbalances.  This would require adding
     a field to the frame header.  */

  Lisp_Object vector = AREF (fun, CLOSURE_CONSTANTS);
  Lisp_Object maxdepth = AREF (fun, CLOSURE_STACK_DEPTH);
  ptrdiff_t const_length = ASIZE (vector);
  ptrdiff_t bytestr_length = SCHARS (bytestr);
  Lisp_Object *vectorp = XVECTOR (vector)->contents;

  EMACS_INT max_stack = XFIXNAT (maxdepth);
  Lisp_Object *frame_base = bc->fp->next_stack;
  struct bc_frame *fp = (struct bc_frame *)(frame_base + max_stack);

  if ((char *)fp->next_stack > bc->stack_end)
    error ("Bytecode stack overflow");

  /* Save the function object so that the bytecode and vector are
     held from removal by the GC. */
  fp->fun = fun;
  /* Save previous stack pointer and pc in the new frame.  If we came
     directly from outside, these will be NULL.  */
  fp->saved_top = top;
  fp->saved_pc = pc;
  fp->saved_fp = bc->fp;
  bc->fp = fp;

  top = frame_base - 1;
  unsigned char const *bytestr_data = SDATA (bytestr);
  pc = bytestr_data;

  /* ARGS_TEMPLATE is composed of bit fields:
     bits 0..6    minimum number of arguments
     bits 7       1 iff &rest argument present
     bits 8..14   maximum number of arguments */
  bool rest = (args_template & 128) != 0;
  int mandatory = args_template & 127;
  ptrdiff_t nonrest = args_template >> 8;
  if (! (mandatory <= nargs && (rest || nargs <= nonrest)))
    Fsignal (Qwrong_number_of_arguments,
	     list2 (Fcons (make_fixnum (mandatory), make_fixnum (nonrest)),
		    make_fixnum (nargs)));
  ptrdiff_t pushedargs = min (nonrest, nargs);
  for (ptrdiff_t i = 0; i < pushedargs; i++, args++)
    PUSH (*args);
  if (nonrest < nargs)
    PUSH (Flist (nargs - nonrest, args));
  else
    for (ptrdiff_t i = nargs - rest; i < nonrest; i++)
      PUSH (Qnil);

  while (true)
    {
      ptrdiff_t op;
      ptrdiff_t arg;
      enum handlertype type;

      if (BYTE_CODE_SAFE && !valid_sp (bc, top))
	emacs_abort ();

#ifdef BYTE_CODE_METER
      int prev_op = this_op;
      this_op = op = FETCH;
      METER_CODE (prev_op, op);
#elif !defined BYTE_CODE_THREADED
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
#define NEXT goto *(targets[op = FETCH])
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

      /* This is the dispatch table for the threaded interpreter.  */
      static const void *const targets[256] =
	{
	  [0 ... (Bconstant - 1)] = &&insn_default,
	  [Bconstant ... 255] = &&insn_Bconstant,

#define DEFINE(name, value) [name] = &&insn_ ## name,
	  BYTE_CODES
#undef DEFINE
	};

#endif


      FIRST
	{
	CASE (Bvarref7):
	  arg = FETCH2;
	  goto varref;

	CASE (Bvarref):
	CASE (Bvarref1):
	CASE (Bvarref2):
	CASE (Bvarref3):
	CASE (Bvarref4):
	CASE (Bvarref5):
	  arg = op - Bvarref;
	  goto varref;

	/* This seems to be the most frequently executed byte-code
	   among the Bvarref's, so avoid a goto here.  */
	CASE (Bvarref6):
	  arg = FETCH;
	varref:
	  {
	    Lisp_Object v1 = vectorp[arg], v2;
	    if (XBARE_SYMBOL (v1)->u.s.redirect != SYMBOL_PLAINVAL
		|| (v2 = XBARE_SYMBOL (v1)->u.s.val.value,
		    BASE_EQ (v2, Qunbound)))
	      v2 = Fsymbol_value (v1);
	    PUSH (v2);
	    NEXT;
	  }

	CASE (Bgotoifnil):
	  {
	    Lisp_Object v1 = POP;
	    arg = FETCH2;
	    if (NILP (v1))
	      goto op_branch;
	    NEXT;
	  }

	CASE (Bcar):
	  if (CONSP (TOP))
	    TOP = XCAR (TOP);
	  else if (!NILP (TOP))
	    {
	      record_in_backtrace (Qcar, &TOP, 1);
	      wrong_type_argument (Qlistp, TOP);
	    }
	  NEXT;

	CASE (Beq):
	  {
	    Lisp_Object v1 = POP;
	    TOP = EQ (v1, TOP) ? Qt : Qnil;
	    NEXT;
	  }

	CASE (Bmemq):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fmemq (TOP, v1);
	    NEXT;
	  }

	CASE (Bcdr):
	  {
	    if (CONSP (TOP))
	      TOP = XCDR (TOP);
	    else if (!NILP (TOP))
	      {
		record_in_backtrace (Qcdr, &TOP, 1);
		wrong_type_argument (Qlistp, TOP);
	      }
	    NEXT;
	  }

	CASE (Bvarset):
	CASE (Bvarset1):
	CASE (Bvarset2):
	CASE (Bvarset3):
	CASE (Bvarset4):
	CASE (Bvarset5):
	  arg = op - Bvarset;
	  goto varset;

	CASE (Bvarset7):
	  arg = FETCH2;
	  goto varset;

	CASE (Bvarset6):
	  arg = FETCH;
	varset:
	  {
	    Lisp_Object sym = vectorp[arg];
	    Lisp_Object val = POP;
	    if (XBARE_SYMBOL (sym)->u.s.redirect == SYMBOL_PLAINVAL
		&& !XBARE_SYMBOL (sym)->u.s.trapped_write)
	      SET_SYMBOL_VAL (XBARE_SYMBOL (sym), val);
	    else
              set_internal (sym, val, Qnil, SET_INTERNAL_SET);
	  }
	  NEXT;

	CASE (Bdup):
	  {
	    Lisp_Object v1 = TOP;
	    PUSH (v1);
	    NEXT;
	  }

	/* ------------------ */

	CASE (Bvarbind6):
	  arg = FETCH;
	  goto varbind;

	CASE (Bvarbind7):
	  arg = FETCH2;
	  goto varbind;

	CASE (Bvarbind):
	CASE (Bvarbind1):
	CASE (Bvarbind2):
	CASE (Bvarbind3):
	CASE (Bvarbind4):
	CASE (Bvarbind5):
	  arg = op - Bvarbind;
	varbind:
	  /* Specbind can signal and thus GC.  */
	  specbind (vectorp[arg], POP);
	  NEXT;

	CASE (Bcall6):
	  arg = FETCH;
	  goto docall;

	CASE (Bcall7):
	  arg = FETCH2;
	  goto docall;

	CASE (Bcall):
	CASE (Bcall1):
	CASE (Bcall2):
	CASE (Bcall3):
	CASE (Bcall4):
	CASE (Bcall5):
	  arg = op - Bcall;
	docall:
	  {
	    DISCARD (arg);
#ifdef BYTE_CODE_METER
	    if (byte_metering_on && SYMBOLP (TOP))
	      {
		Lisp_Object v1 = TOP;
		Lisp_Object v2 = Fget (v1, Qbyte_code_meter);
		if (FIXNUMP (v2)
		    && XFIXNUM (v2) < MOST_POSITIVE_FIXNUM)
		  {
		    XSETINT (v2, XFIXNUM (v2) + 1);
		    Fput (v1, Qbyte_code_meter, v2);
		  }
	      }
#endif
	    maybe_quit ();

	    if (++lisp_eval_depth > max_lisp_eval_depth)
	      {
		if (max_lisp_eval_depth < 100)
		  max_lisp_eval_depth = 100;
		if (lisp_eval_depth > max_lisp_eval_depth)
		  error ("Lisp nesting exceeds `max-lisp-eval-depth'");
	      }

	    ptrdiff_t call_nargs = arg;
	    Lisp_Object call_fun = TOP;
	    Lisp_Object *call_args = &TOP + 1;

	    specpdl_ref count1 = record_in_backtrace (call_fun,
						      call_args, call_nargs);
	    maybe_gc ();
	    if (debug_on_next_call)
	      do_debug_on_call (Qlambda, count1);

	    Lisp_Object original_fun = call_fun;
	    /* Calls to symbols-with-pos don't need to be on the fast path.  */
	    if (BARE_SYMBOL_P (call_fun))
	      call_fun = XBARE_SYMBOL (call_fun)->u.s.function;
	    if (CLOSUREP (call_fun))
	      {
		Lisp_Object template = AREF (call_fun, CLOSURE_ARGLIST);
		if (FIXNUMP (template))
		  {
		    /* Fast path for lexbound functions.  */
		    fun = call_fun;
		    bytestr = AREF (call_fun, CLOSURE_CODE),
		    args_template = XFIXNUM (template);
		    nargs = call_nargs;
		    args = call_args;
		    goto setup_frame;
		  }
	      }

	    Lisp_Object val;
	    if (SUBRP (call_fun) && !NATIVE_COMP_FUNCTION_DYNP (call_fun))
	      val = funcall_subr (XSUBR (call_fun), call_nargs, call_args);
	    else
	      val = funcall_general (original_fun, call_nargs, call_args);

	    lisp_eval_depth--;
	    if (backtrace_debug_on_exit (specpdl_ptr - 1))
	      val = call_debugger (list2 (Qexit, val));
	    specpdl_ptr--;

	    TOP = val;
	    NEXT;
	  }

	CASE (Bunbind6):
	  arg = FETCH;
	  goto dounbind;

	CASE (Bunbind7):
	  arg = FETCH2;
	  goto dounbind;

	CASE (Bunbind):
	CASE (Bunbind1):
	CASE (Bunbind2):
	CASE (Bunbind3):
	CASE (Bunbind4):
	CASE (Bunbind5):
	  arg = op - Bunbind;
	dounbind:
	  unbind_to (specpdl_ref_add (SPECPDL_INDEX (), -arg), Qnil);
	  NEXT;

	CASE (Bgoto):
	  arg = FETCH2;
	op_branch:
	  {
	    if (BYTE_CODE_SAFE && !(arg >= 0 && arg < bytestr_length))
	      emacs_abort ();
	    const unsigned char *new_pc = bytestr_data + arg;
	    quitcounter += new_pc < pc;
	    if (!quitcounter)
	      {
		quitcounter = 1;
		maybe_gc ();
		maybe_quit ();
	      }
	    pc = new_pc;
	    NEXT;
	  }

	CASE (Bgotoifnonnil):
	  arg = FETCH2;
	  if (!NILP (POP))
	    goto op_branch;
	  NEXT;

	CASE (Bgotoifnilelsepop):
	  arg = FETCH2;
	  if (NILP (TOP))
	    goto op_branch;
	  DISCARD (1);
	  NEXT;

	CASE (Bgotoifnonnilelsepop):
	  arg = FETCH2;
	  if (!NILP (TOP))
	    goto op_branch;
	  DISCARD (1);
	  NEXT;

	CASE (Breturn):
	  {
	    Lisp_Object *saved_top = bc->fp->saved_top;
	    if (saved_top)
	      {
		Lisp_Object val = TOP;

		lisp_eval_depth--;
		if (backtrace_debug_on_exit (specpdl_ptr - 1))
		  val = call_debugger (list2 (Qexit, val));
		specpdl_ptr--;

		top = saved_top;
		pc = bc->fp->saved_pc;
		struct bc_frame *fp = bc->fp->saved_fp;
		bc->fp = fp;

		Lisp_Object fun = fp->fun;
		Lisp_Object bytestr = AREF (fun, CLOSURE_CODE);
		Lisp_Object vector = AREF (fun, CLOSURE_CONSTANTS);
		bytestr_data = SDATA (bytestr);
		vectorp = XVECTOR (vector)->contents;
		if (BYTE_CODE_SAFE)
		  {
		    /* Only required for checking, not for execution.  */
		    const_length = ASIZE (vector);
		    bytestr_length = SCHARS (bytestr);
		  }

		TOP = val;
		NEXT;
	      }
	    else
	      goto exit;
	  }

	CASE (Bdiscard):
	  DISCARD (1);
	  NEXT;

	CASE (Bconstant2):
	  PUSH (vectorp[FETCH2]);
	  NEXT;

	CASE (Bsave_excursion):
	  record_unwind_protect_excursion ();
	  NEXT;

	CASE (Bsave_current_buffer_OBSOLETE): /* Obsolete since 20.  */
	CASE (Bsave_current_buffer):
	  record_unwind_current_buffer ();
	  NEXT;

	CASE (Bsave_window_excursion): /* Obsolete since 24.1.  */
	  {
	    specpdl_ref count1 = SPECPDL_INDEX ();
	    record_unwind_protect (restore_window_configuration,
				   Fcurrent_window_configuration (Qnil));
	    TOP = Fprogn (TOP);
	    unbind_to (count1, TOP);
	    NEXT;
	  }

	CASE (Bsave_restriction):
	  record_unwind_protect (save_restriction_restore,
				 save_restriction_save ());
	  NEXT;

	CASE (Bcatch):		/* Obsolete since 25.  */
	  {
	    Lisp_Object v1 = POP;
	    TOP = internal_catch (TOP, eval_sub, v1);
	    NEXT;
	  }

	CASE (Bpushcatch):	/* New in 24.4.  */
	  type = CATCHER;
	  goto pushhandler;
	CASE (Bpushconditioncase): /* New in 24.4.  */
	  type = CONDITION_CASE;
	pushhandler:
	  {
	    struct handler *c = push_handler (POP, type);
	    c->bytecode_dest = FETCH2;
	    c->bytecode_top = top;

	    if (sys_setjmp (c->jmp))
	      {
		/* No need to restore old quitcounter; just check at the next
		   backward branch.  */
		quitcounter = (unsigned char)-1;
		struct handler *c = handlerlist;
		handlerlist = c->next;
		top = c->bytecode_top;
		arg = c->bytecode_dest;
		bc = &current_thread->bc;
		struct bc_frame *fp = bc->fp;

		Lisp_Object fun = fp->fun;
		Lisp_Object bytestr = AREF (fun, CLOSURE_CODE);
		Lisp_Object vector = AREF (fun, CLOSURE_CONSTANTS);
		bytestr_data = SDATA (bytestr);
		vectorp = XVECTOR (vector)->contents;
		if (BYTE_CODE_SAFE)
		  {
		    /* Only required for checking, not for execution.  */
		    const_length = ASIZE (vector);
		    bytestr_length = SCHARS (bytestr);
		  }
		pc = bytestr_data;
		PUSH (c->val);
		goto op_branch;
	      }

	    NEXT;
	  }

	CASE (Bpophandler):	/* New in 24.4.  */
	  handlerlist = handlerlist->next;
	  NEXT;

	CASE (Bunwind_protect):	/* FIXME: avoid closure for lexbind.  */
	  {
	    Lisp_Object handler = POP;
	    /* Support for a function here is new in 24.4.  */
	    record_unwind_protect (FUNCTIONP (handler) ? bcall0 : prog_ignore,
				   handler);
	    NEXT;
	  }

	CASE (Bcondition_case):		/* Obsolete since 25.  */
	  {
	    Lisp_Object handlers = POP, body = POP;
	    TOP = internal_lisp_condition_case (TOP, body, handlers);
	    NEXT;
	  }

	CASE (Btemp_output_buffer_setup): /* Obsolete since 24.1.  */
	  CHECK_STRING (TOP);
	  temp_output_buffer_setup (SSDATA (TOP));
	  TOP = Vstandard_output;
	  NEXT;

	CASE (Btemp_output_buffer_show): /* Obsolete since 24.1.  */
	  {
	    Lisp_Object v1 = POP;
	    temp_output_buffer_show (TOP);
	    TOP = v1;
	    /* pop binding of standard-output */
	    unbind_to (specpdl_ref_add (SPECPDL_INDEX (), -1), Qnil);
	    NEXT;
	  }

	CASE (Bnth):
	  {
	    Lisp_Object v2 = POP, v1 = TOP;
	    if (RANGED_FIXNUMP (0, v1, SMALL_LIST_LEN_MAX))
	      {
		for (EMACS_INT n = XFIXNUM (v1); 0 < n && CONSP (v2); n--)
		  v2 = XCDR (v2);
		if (CONSP (v2))
		  TOP = XCAR (v2);
		else if (NILP (v2))
		  TOP = Qnil;
		else
		  {
		    record_in_backtrace (Qnth, &TOP, 2);
		    wrong_type_argument (Qlistp, v2);
		  }
	      }
	    else
	      TOP = Fnth (v1, v2);
	    NEXT;
	  }

	CASE (Bsymbolp):
	  TOP = SYMBOLP (TOP) ? Qt : Qnil;
	  NEXT;

	CASE (Bconsp):
	  TOP = CONSP (TOP) ? Qt : Qnil;
	  NEXT;

	CASE (Bstringp):
	  TOP = STRINGP (TOP) ? Qt : Qnil;
	  NEXT;

	CASE (Blistp):
	  TOP = CONSP (TOP) || NILP (TOP) ? Qt : Qnil;
	  NEXT;

	CASE (Bnot):
	  TOP = NILP (TOP) ? Qt : Qnil;
	  NEXT;

	CASE (Bcons):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fcons (TOP, v1);
	    NEXT;
	  }

	CASE (Blist1):
	  TOP = list1 (TOP);
	  NEXT;

	CASE (Blist2):
	  {
	    Lisp_Object v1 = POP;
	    TOP = list2 (TOP, v1);
	    NEXT;
	  }

	CASE (Blist3):
	  DISCARD (2);
	  TOP = list3 (TOP, top[1], top[2]);
	  NEXT;

	CASE (Blist4):
	  DISCARD (3);
	  TOP = list4 (TOP, top[1], top[2], top[3]);
	  NEXT;

	CASE (BlistN):
	  {
	    ptrdiff_t n = FETCH;
	    DISCARD (n - 1);
	    TOP = Flist (n, &TOP);
	    NEXT;
	  }

	CASE (Blength):
	  TOP = Flength (TOP);
	  NEXT;

	CASE (Baref):
	  {
	    Lisp_Object idxval = POP;
	    Lisp_Object arrayval = TOP;
	    if (!FIXNUMP (idxval))
	      {
		record_in_backtrace (Qaref, &TOP, 2);
		wrong_type_argument (Qfixnump, idxval);
	      }
	    ptrdiff_t size;
	    if (((VECTORP (arrayval) && (size = ASIZE (arrayval), true))
		 || (RECORDP (arrayval) && (size = PVSIZE (arrayval), true))))
	      {
		ptrdiff_t idx = XFIXNUM (idxval);
		if (idx >= 0 && idx < size)
		  TOP = AREF (arrayval, idx);
		else
		  {
		    record_in_backtrace (Qaref, &TOP, 2);
		    args_out_of_range (arrayval, idxval);
		  }
	      }
	    else
	      TOP = Faref (arrayval, idxval);
	    NEXT;
	  }

	CASE (Baset):
	  {
	    Lisp_Object newelt = POP;
	    Lisp_Object idxval = POP;
	    Lisp_Object arrayval = TOP;
	    if (!FIXNUMP (idxval))
	      {
		record_in_backtrace (Qaset, &TOP, 3);
		wrong_type_argument (Qfixnump, idxval);
	      }
	    ptrdiff_t size;
	    if (((VECTORP (arrayval) && (size = ASIZE (arrayval), true))
		 || (RECORDP (arrayval) && (size = PVSIZE (arrayval), true))))
	      {
		ptrdiff_t idx = XFIXNUM (idxval);
		if (idx >= 0 && idx < size)
		  {
		    ASET (arrayval, idx, newelt);
		    TOP = newelt;
		  }
		else
		  {
		    record_in_backtrace (Qaset, &TOP, 3);
		    args_out_of_range (arrayval, idxval);
		  }
	      }
	    else
	      TOP = Faset (arrayval, idxval, newelt);
	    NEXT;
	  }

	CASE (Bsymbol_value):
	  TOP = Fsymbol_value (TOP);
	  NEXT;

	CASE (Bsymbol_function):
	  TOP = Fsymbol_function (TOP);
	  NEXT;

	CASE (Bset):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fset (TOP, v1);
	    NEXT;
	  }

	CASE (Bfset):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Ffset (TOP, v1);
	    NEXT;
	  }

	CASE (Bget):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fget (TOP, v1);
	    NEXT;
	  }

	CASE (Bsubstring):
	  {
	    Lisp_Object v2 = POP, v1 = POP;
	    TOP = Fsubstring (TOP, v1, v2);
	    NEXT;
	  }

	CASE (Bconcat2):
	  DISCARD (1);
	  TOP = Fconcat (2, &TOP);
	  NEXT;

	CASE (Bconcat3):
	  DISCARD (2);
	  TOP = Fconcat (3, &TOP);
	  NEXT;

	CASE (Bconcat4):
	  DISCARD (3);
	  TOP = Fconcat (4, &TOP);
	  NEXT;

	CASE (BconcatN):
	  {
	    ptrdiff_t n = FETCH;
	    DISCARD (n - 1);
	    TOP = Fconcat (n, &TOP);
	    NEXT;
	  }

	CASE (Bsub1):
	  TOP = (FIXNUMP (TOP) && XFIXNUM (TOP) != MOST_NEGATIVE_FIXNUM
		 ? make_fixnum (XFIXNUM (TOP) - 1)
		 : Fsub1 (TOP));
	  NEXT;

	CASE (Badd1):
	  TOP = (FIXNUMP (TOP) && XFIXNUM (TOP) != MOST_POSITIVE_FIXNUM
		 ? make_fixnum (XFIXNUM (TOP) + 1)
		 : Fadd1 (TOP));
	  NEXT;

	CASE (Beqlsign):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    if (FIXNUMP (v1) && FIXNUMP (v2))
	      TOP = BASE_EQ (v1, v2) ? Qt : Qnil;
	    else
	      TOP = arithcompare (v1, v2) & Cmp_EQ ? Qt : Qnil;
	    NEXT;
	  }

	CASE (Bgtr):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    if (FIXNUMP (v1) && FIXNUMP (v2))
	      TOP = XFIXNUM (v1) > XFIXNUM (v2) ? Qt : Qnil;
	    else
	      TOP = arithcompare (v1, v2) & Cmp_GT ? Qt : Qnil;
	    NEXT;
	  }

	CASE (Blss):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    if (FIXNUMP (v1) && FIXNUMP (v2))
	      TOP = XFIXNUM (v1) < XFIXNUM (v2) ? Qt : Qnil;
	    else
	      TOP = arithcompare (v1, v2) & Cmp_LT ? Qt : Qnil;
	    NEXT;
	  }

	CASE (Bleq):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    if (FIXNUMP (v1) && FIXNUMP (v2))
	      TOP = XFIXNUM (v1) <= XFIXNUM (v2) ? Qt : Qnil;
	    else
	      TOP = arithcompare (v1, v2) & (Cmp_LT | Cmp_EQ) ? Qt : Qnil;
	    NEXT;
	  }

	CASE (Bgeq):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    if (FIXNUMP (v1) && FIXNUMP (v2))
	      TOP = XFIXNUM (v1) >= XFIXNUM (v2) ? Qt : Qnil;
	    else
	      TOP = arithcompare (v1, v2) & (Cmp_GT | Cmp_EQ) ? Qt : Qnil;
	    NEXT;
	  }

	CASE (Bdiff):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    EMACS_INT res;
	    if (FIXNUMP (v1) && FIXNUMP (v2)
		&& (res = XFIXNUM (v1) - XFIXNUM (v2),
		    !FIXNUM_OVERFLOW_P (res)))
	      TOP = make_fixnum (res);
	    else
	      TOP = Fminus (2, &TOP);
	    NEXT;
	  }

	CASE (Bnegate):
	  TOP = (FIXNUMP (TOP) && XFIXNUM (TOP) != MOST_NEGATIVE_FIXNUM
		 ? make_fixnum (- XFIXNUM (TOP))
		 : Fminus (1, &TOP));
	  NEXT;

	CASE (Bplus):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    EMACS_INT res;
	    if (FIXNUMP (v1) && FIXNUMP (v2)
		&& (res = XFIXNUM (v1) + XFIXNUM (v2),
		    !FIXNUM_OVERFLOW_P (res)))
	      TOP = make_fixnum (res);
	    else
	      TOP = Fplus (2, &TOP);
	    NEXT;
	  }

	CASE (Bmax):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    if (FIXNUMP (v1) && FIXNUMP (v2))
	      {
		if (XFIXNUM (v2) > XFIXNUM (v1))
		  TOP = v2;
	      }
	    else
	      TOP = Fmax (2, &TOP);
	    NEXT;
	  }

	CASE (Bmin):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    if (FIXNUMP (v1) && FIXNUMP (v2))
	      {
		if (XFIXNUM (v2) < XFIXNUM (v1))
		  TOP = v2;
	      }
	    else
	      TOP = Fmin (2, &TOP);
	    NEXT;
	  }

	CASE (Bmult):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    intmax_t res;
	    if (FIXNUMP (v1) && FIXNUMP (v2)
		&& !ckd_mul (&res, XFIXNUM (v1), XFIXNUM (v2))
		&& !FIXNUM_OVERFLOW_P (res))
	      TOP = make_fixnum (res);
	    else
	      TOP = Ftimes (2, &TOP);
	    NEXT;
	  }

	CASE (Bquo):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    EMACS_INT res;
	    if (FIXNUMP (v1) && FIXNUMP (v2) && XFIXNUM (v2) != 0
		&& (res = XFIXNUM (v1) / XFIXNUM (v2),
		    !FIXNUM_OVERFLOW_P (res)))
	      TOP = make_fixnum (res);
	    else
	      TOP = Fquo (2, &TOP);
	    NEXT;
	  }

	CASE (Brem):
	  {
	    Lisp_Object v2 = POP;
	    Lisp_Object v1 = TOP;
	    if (FIXNUMP (v1) && FIXNUMP (v2) && XFIXNUM (v2) != 0)
	      TOP = make_fixnum (XFIXNUM (v1) % XFIXNUM (v2));
	    else
	      TOP = Frem (v1, v2);
	    NEXT;
	  }

	CASE (Bpoint):
	  PUSH (make_fixed_natnum (PT));
	  NEXT;

	CASE (Bgoto_char):
	  TOP = Fgoto_char (TOP);
	  NEXT;

	CASE (Binsert):
	  TOP = Finsert (1, &TOP);
	  NEXT;

	CASE (BinsertN):
	  {
	    ptrdiff_t n = FETCH;
	    DISCARD (n - 1);
	    TOP = Finsert (n, &TOP);
	    NEXT;
	  }

	CASE (Bpoint_max):
	  PUSH (make_fixed_natnum (ZV));
	  NEXT;

	CASE (Bpoint_min):
	  PUSH (make_fixed_natnum (BEGV));
	  NEXT;

	CASE (Bchar_after):
	  TOP = Fchar_after (TOP);
	  NEXT;

	CASE (Bfollowing_char):
	  PUSH (Ffollowing_char ());
	  NEXT;

	CASE (Bpreceding_char):
	  PUSH (Fprevious_char ());
	  NEXT;

	CASE (Bcurrent_column):
	  PUSH (make_fixed_natnum (current_column ()));
	  NEXT;

	CASE (Bindent_to):
	  TOP = Findent_to (TOP, Qnil);
	  NEXT;

	CASE (Beolp):
	  PUSH (Feolp ());
	  NEXT;

	CASE (Beobp):
	  PUSH (Feobp ());
	  NEXT;

	CASE (Bbolp):
	  PUSH (Fbolp ());
	  NEXT;

	CASE (Bbobp):
	  PUSH (Fbobp ());
	  NEXT;

	CASE (Bcurrent_buffer):
	  PUSH (Fcurrent_buffer ());
	  NEXT;

	CASE (Bset_buffer):
	  TOP = Fset_buffer (TOP);
	  NEXT;

	CASE (Binteractive_p):	/* Obsolete since 24.1.  */
	  PUSH (call0 (Qinteractive_p));
	  NEXT;

	CASE (Bforward_char):
	  TOP = Fforward_char (TOP);
	  NEXT;

	CASE (Bforward_word):
	  TOP = Fforward_word (TOP);
	  NEXT;

	CASE (Bskip_chars_forward):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fskip_chars_forward (TOP, v1);
	    NEXT;
	  }

	CASE (Bskip_chars_backward):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fskip_chars_backward (TOP, v1);
	    NEXT;
	  }

	CASE (Bforward_line):
	  TOP = Fforward_line (TOP);
	  NEXT;

	CASE (Bchar_syntax):
	  TOP = Fchar_syntax (TOP);
	  NEXT;

	CASE (Bbuffer_substring):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fbuffer_substring (TOP, v1);
	    NEXT;
	  }

	CASE (Bdelete_region):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fdelete_region (TOP, v1);
	    NEXT;
	  }

	CASE (Bnarrow_to_region):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fnarrow_to_region (TOP, v1);
	    NEXT;
	  }

	CASE (Bwiden):
	  PUSH (Fwiden ());
	  NEXT;

	CASE (Bend_of_line):
	  TOP = Fend_of_line (TOP);
	  NEXT;

	CASE (Bset_marker):
	  {
	    Lisp_Object v2 = POP, v1 = POP;
	    TOP = Fset_marker (TOP, v1, v2);
	    NEXT;
	  }

	CASE (Bmatch_beginning):
	  TOP = Fmatch_beginning (TOP);
	  NEXT;

	CASE (Bmatch_end):
	  TOP = Fmatch_end (TOP);
	  NEXT;

	CASE (Bupcase):
	  TOP = Fupcase (TOP);
	  NEXT;

	CASE (Bdowncase):
	  TOP = Fdowncase (TOP);
	  NEXT;

	CASE (Bstringeqlsign):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fstring_equal (TOP, v1);
	    NEXT;
	  }

	CASE (Bstringlss):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fstring_lessp (TOP, v1);
	    NEXT;
	  }

	CASE (Bequal):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fequal (TOP, v1);
	    NEXT;
	  }

	CASE (Bnthcdr):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fnthcdr (TOP, v1);
	    NEXT;
	  }

	CASE (Belt):
	  {
	    Lisp_Object v2 = POP, v1 = TOP;
	    if (CONSP (v1) && RANGED_FIXNUMP (0, v2, SMALL_LIST_LEN_MAX))
	      {
		/* Like the fast case for Bnth, but with args reversed.  */
		for (EMACS_INT n = XFIXNUM (v2); 0 < n && CONSP (v1); n--)
		  v1 = XCDR (v1);
		if (CONSP (v1))
		  TOP = XCAR (v1);
		else if (NILP (v1))
		  TOP = Qnil;
		else
		  {
		    record_in_backtrace (Qelt, &TOP, 2);
		    wrong_type_argument (Qlistp, v1);
		  }
	      }
	    else
	      TOP = Felt (v1, v2);
	    NEXT;
	  }

	CASE (Bmember):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fmember (TOP, v1);
	    NEXT;
	  }

	CASE (Bassq):
	  {
	    Lisp_Object v1 = POP;
	    TOP = Fassq (TOP, v1);
	    NEXT;
	  }

	CASE (Bnreverse):
	  TOP = Fnreverse (TOP);
	  NEXT;

	CASE (Bsetcar):
	  {
	    Lisp_Object newval = POP;
	    Lisp_Object cell = TOP;
	    if (!CONSP (cell))
	      {
		record_in_backtrace (Qsetcar, &TOP, 2);
		wrong_type_argument (Qconsp, cell);
	      }
	    XSETCAR (cell, newval);
	    TOP = newval;
	    NEXT;
	  }

	CASE (Bsetcdr):
	  {
	    Lisp_Object newval = POP;
	    Lisp_Object cell = TOP;
	    if (!CONSP (cell))
	      {
		record_in_backtrace (Qsetcdr, &TOP, 2);
		wrong_type_argument (Qconsp, cell);
	      }
	    XSETCDR (cell, newval);
	    TOP = newval;
	    NEXT;
	  }

	CASE (Bcar_safe):
	  TOP = CAR_SAFE (TOP);
	  NEXT;

	CASE (Bcdr_safe):
	  TOP = CDR_SAFE (TOP);
	  NEXT;

	CASE (Bnconc):
	  DISCARD (1);
	  TOP = Fnconc (2, &TOP);
	  NEXT;

	CASE (Bnumberp):
	  TOP = NUMBERP (TOP) ? Qt : Qnil;
	  NEXT;

	CASE (Bintegerp):
	  TOP = INTEGERP (TOP) ? Qt : Qnil;
	  NEXT;

	CASE_ABORT:
	  /* Actually this is Bstack_ref with offset 0, but we use Bdup
	     for that instead.  */
	  /* CASE (Bstack_ref): */
	  error ("Invalid byte opcode: op=%d, ptr=%"pD"d",
		 pc[-1], pc - 1 - bytestr_data);

	  /* Handy byte-codes for lexical binding.  */
	CASE (Bstack_ref1):
	CASE (Bstack_ref2):
	CASE (Bstack_ref3):
	CASE (Bstack_ref4):
	CASE (Bstack_ref5):
	  {
	    Lisp_Object v1 = top[Bstack_ref - op];
	    PUSH (v1);
	    NEXT;
	  }
	CASE (Bstack_ref6):
	  {
	    Lisp_Object v1 = top[- FETCH];
	    PUSH (v1);
	    NEXT;
	  }
	CASE (Bstack_ref7):
	  {
	    Lisp_Object v1 = top[- FETCH2];
	    PUSH (v1);
	    NEXT;
	  }
	CASE (Bstack_set):
	  /* stack-set-0 = discard; stack-set-1 = discard-1-preserve-tos.  */
	  {
	    Lisp_Object *ptr = top - FETCH;
	    *ptr = POP;
	    NEXT;
	  }
	CASE (Bstack_set2):
	  {
	    Lisp_Object *ptr = top - FETCH2;
	    *ptr = POP;
	    NEXT;
	  }
	CASE (BdiscardN):
	  {
	    ptrdiff_t n = FETCH;
	    if (n & 0x80)
	      {
		n &= 0x7F;
		top[-n] = TOP;
	      }
	    DISCARD (n);
	    NEXT;
	  }

        CASE (Bswitch):
          {
            /* TODO: Perhaps introduce another byte-code for switch when the
	       number of cases is less, which uses a simple vector for linear
	       search as the jump table.  */

	    /* TODO: Instead of pushing the table in a separate
	       Bconstant op, use an immediate argument (maybe separate
	       switch opcodes for 1-byte and 2-byte constant indices).
	       This would also get rid of some hacks that assume each
	       Bswitch to be preceded by a Bconstant.  */
            Lisp_Object jmp_table = POP;
	    if (BYTE_CODE_SAFE && !HASH_TABLE_P (jmp_table))
              emacs_abort ();
            Lisp_Object v1 = POP;
            struct Lisp_Hash_Table *h = XHASH_TABLE (jmp_table);
	    /* Do a linear search if there are few cases and the test is `eq'.
	       (The table is assumed to be sized exactly; all entries are
	       consecutive at the beginning.)
	       FIXME: 5 is arbitrarily chosen.  */
            if (h->count <= 5 && !h->test->cmpfn && !symbols_with_pos_enabled)
              {
		eassume (h->count >= 2);
		for (ptrdiff_t i = h->count - 1; i >= 0; i--)
		  if (BASE_EQ (v1, HASH_KEY (h, i)))
		    {
		      arg = XFIXNUM (HASH_VALUE (h, i));
		      goto op_branch;
		    }
              }
            else
	      {
		ptrdiff_t i = hash_find (h, v1);
		if (i >= 0)
		  {
		    arg = XFIXNUM (HASH_VALUE (h, i));
		    goto op_branch;
		  }
	      }
          }
          NEXT;

	CASE_DEFAULT
	CASE (Bconstant):
	  if (BYTE_CODE_SAFE
	      && ! (Bconstant <= op && op < Bconstant + const_length))
	    emacs_abort ();
	  PUSH (vectorp[op - Bconstant]);
	  NEXT;
	}
    }

 exit:

  bc->fp = bc->fp->saved_fp;

  Lisp_Object result = TOP;
  return result;
}

#if __GNUC__ && !__clang__
#pragma GCC diagnostic pop
#endif


/* `args_template' has the same meaning as in exec_byte_code() above.  */
Lisp_Object
get_byte_code_arity (Lisp_Object args_template)
{
  eassert (FIXNATP (args_template));
  EMACS_INT at = XFIXNUM (args_template);
  bool rest = (at & 128) != 0;
  int mandatory = at & 127;
  EMACS_INT nonrest = at >> 8;

  return Fcons (make_fixnum (mandatory),
		rest ? Qmany : make_fixnum (nonrest));
}

void
syms_of_bytecode (void)
{
  DEFSYM (Qinteractive_p, "interactive-p");

  defsubr (&Sbyte_code);
  defsubr (&Sinternal_stack_stats);

#ifdef BYTE_CODE_METER

  DEFVAR_LISP ("byte-code-meter", Vbyte_code_meter,
	       doc: /* A vector of vectors which holds a histogram of byte-code usage.
\(aref (aref byte-code-meter 0) CODE) indicates how many times the byte
opcode CODE has been executed.
\(aref (aref byte-code-meter CODE1) CODE2), where CODE1 is not 0,
indicates how many times the byte opcodes CODE1 and CODE2 have been
executed in succession.  */);

  DEFVAR_BOOL ("byte-metering-on", byte_metering_on,
	       doc: /* If non-nil, keep profiling information on byte code usage.
The variable byte-code-meter indicates how often each byte opcode is used.
If a symbol has a property named `byte-code-meter' whose value is an
integer, it is incremented each time that symbol's function is called.  */);

  byte_metering_on = false;
  Vbyte_code_meter = make_nil_vector (256);
  DEFSYM (Qbyte_code_meter, "byte-code-meter");
  for (int i = 0; i < 256; i++)
    ASET (Vbyte_code_meter, i, make_vector (256, make_fixnum (0)));
#endif
}
