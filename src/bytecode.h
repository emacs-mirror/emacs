/* Shared definitions for src/bytecode{,-jit}.c
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

#include "lisp.h"

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
#if (defined __GNUC__ && !defined __STRICT_ANSI__ && !defined __CHKP__ \
     && !BYTE_CODE_SAFE && !defined BYTE_CODE_METER)
#define BYTE_CODE_THREADED
#endif

/*  Byte codes: */

#define BYTE_CODES							\
  DEFINE (Bstack_ref, 0) /* Actually, Bstack_ref+0 is not implemented: use dup.  */ \
  DEFINE (Bstack_ref1, 1)						\
  DEFINE (Bstack_ref2, 2)						\
  DEFINE (Bstack_ref3, 3)						\
  DEFINE (Bstack_ref4, 4)						\
  DEFINE (Bstack_ref5, 5)						\
  DEFINE (Bstack_ref6, 6)						\
  DEFINE (Bstack_ref7, 7)						\
  DEFINE (Bvarref, 010)							\
  DEFINE (Bvarref1, 011)						\
  DEFINE (Bvarref2, 012)						\
  DEFINE (Bvarref3, 013)						\
  DEFINE (Bvarref4, 014)						\
  DEFINE (Bvarref5, 015)						\
  DEFINE (Bvarref6, 016)						\
  DEFINE (Bvarref7, 017)						\
  DEFINE (Bvarset, 020)							\
  DEFINE (Bvarset1, 021)						\
  DEFINE (Bvarset2, 022)						\
  DEFINE (Bvarset3, 023)						\
  DEFINE (Bvarset4, 024)						\
  DEFINE (Bvarset5, 025)						\
  DEFINE (Bvarset6, 026)						\
  DEFINE (Bvarset7, 027)						\
  DEFINE (Bvarbind, 030)						\
  DEFINE (Bvarbind1, 031)						\
  DEFINE (Bvarbind2, 032)						\
  DEFINE (Bvarbind3, 033)						\
  DEFINE (Bvarbind4, 034)						\
  DEFINE (Bvarbind5, 035)						\
  DEFINE (Bvarbind6, 036)						\
  DEFINE (Bvarbind7, 037)						\
  DEFINE_MANY (Bcall, 040, Ffuncall, 1)					\
  DEFINE_MANY (Bcall1, 041, Ffuncall, 2)				\
  DEFINE_MANY (Bcall2, 042, Ffuncall, 3)				\
  DEFINE_MANY (Bcall3, 043, Ffuncall, 4)				\
  DEFINE_MANY (Bcall4, 044, Ffuncall, 5)				\
  DEFINE_MANY (Bcall5, 045, Ffuncall, 6)				\
  DEFINE_MANY (Bcall6, 046, Ffuncall, -1)				\
  DEFINE_MANY (Bcall7, 047, Ffuncall, -2)				\
  DEFINE (Bunbind, 050)							\
  DEFINE (Bunbind1, 051)						\
  DEFINE (Bunbind2, 052)						\
  DEFINE (Bunbind3, 053)						\
  DEFINE (Bunbind4, 054)						\
  DEFINE (Bunbind5, 055)						\
  DEFINE (Bunbind6, 056)						\
  DEFINE (Bunbind7, 057)						\
									\
  DEFINE (Bpophandler, 060)						\
  DEFINE (Bpushconditioncase, 061)					\
  DEFINE (Bpushcatch, 062)						\
									\
  DEFINE_FIXED (Bnth, 070, native_nth, 2)				\
  DEFINE_FIXED (Bsymbolp, 071, native_symbolp, 1)			\
  DEFINE_FIXED (Bconsp, 072, native_consp, 1)				\
  DEFINE_FIXED (Bstringp, 073, native_stringp, 1)			\
  DEFINE_FIXED (Blistp, 074, native_listp, 1)				\
  DEFINE_FIXED (Beq, 075, native_eq, 2)					\
  DEFINE_FIXED (Bmemq, 076, native_memq, 2)				\
  DEFINE_FIXED (Bnot, 077, native_not, 1)				\
  DEFINE_FIXED (Bcar, 0100, native_car, 1)				\
  DEFINE_FIXED (Bcdr, 0101, native_cdr, 1)				\
  DEFINE_FIXED (Bcons, 0102, Fcons, 2)					\
  DEFINE_FIXED (Blist1, 0103, list1, 1)					\
  DEFINE_FIXED (Blist2, 0104, list2, 2)					\
  DEFINE_MANY (Blist3, 0105, Flist, 3)					\
  DEFINE_MANY (Blist4, 0106, Flist, 4)					\
  DEFINE_FIXED (Blength, 0107, Flength, 1)				\
  DEFINE_FIXED (Baref, 0110, Faref, 2)					\
  DEFINE_FIXED (Baset, 0111, Faset, 3)					\
  DEFINE_FIXED (Bsymbol_value, 0112, Fsymbol_value, 1)			\
  DEFINE_FIXED (Bsymbol_function, 0113, Fsymbol_function, 1)		\
  DEFINE_FIXED (Bset, 0114, Fset, 2)					\
  DEFINE_FIXED (Bfset, 0115, Ffset, 2)					\
  DEFINE_FIXED (Bget, 0116, Fget, 2)					\
  DEFINE_FIXED (Bsubstring, 0117, Fsubstring, 3)			\
  DEFINE_MANY (Bconcat2, 0120, Fconcat, 2)				\
  DEFINE_MANY (Bconcat3, 0121, Fconcat, 3)				\
  DEFINE_MANY (Bconcat4, 0122, Fconcat, 4)				\
  DEFINE (Bsub1, 0123)							\
  DEFINE (Badd1, 0124)							\
  DEFINE_FIXED (Beqlsign, 0125, native_eqlsign, 2)			\
  DEFINE (Bgtr, 0126)							\
  DEFINE (Blss, 0127)							\
  DEFINE (Bleq, 0130)							\
  DEFINE (Bgeq, 0131)							\
  DEFINE_MANY (Bdiff, 0132, Fminus, 2)					\
  DEFINE_FIXED (Bnegate, 0133, native_negate, 1)			\
  DEFINE_MANY (Bplus, 0134, Fplus, 2)					\
  DEFINE_MANY (Bmax, 0135, Fmax, 2)					\
  DEFINE_MANY (Bmin, 0136, Fmin, 2)					\
  DEFINE_MANY (Bmult, 0137, Ftimes, 2)					\
									\
  DEFINE_FIXED (Bpoint, 0140, native_point, 0)				\
  /* Was Bmark in v17.  */						\
  DEFINE (Bsave_current_buffer, 0141) /* Obsolete.  */			\
  DEFINE_FIXED (Bgoto_char, 0142, Fgoto_char, 1)			\
  DEFINE_MANY (Binsert, 0143, Finsert, 1)				\
  DEFINE_FIXED (Bpoint_max, 0144, native_point_max, 0)			\
  DEFINE_FIXED (Bpoint_min, 0145, native_point_min, 0)			\
  DEFINE_FIXED (Bchar_after, 0146, Fchar_after, 1)			\
  DEFINE_FIXED (Bfollowing_char, 0147, Ffollowing_char, 0)		\
  DEFINE_FIXED (Bpreceding_char, 0150, Fprevious_char, 0)		\
  DEFINE_FIXED (Bcurrent_column, 0151, native_current_column, 0)	\
  DEFINE (Bindent_to, 0152)						\
  DEFINE_FIXED (Beolp, 0154, Feolp, 0)					\
  DEFINE_FIXED (Beobp, 0155, Feobp, 0)					\
  DEFINE_FIXED (Bbolp, 0156, Fbolp, 0)					\
  DEFINE_FIXED (Bbobp, 0157, Fbobp, 0)					\
  DEFINE_FIXED (Bcurrent_buffer, 0160, Fcurrent_buffer, 0)		\
  DEFINE_FIXED (Bset_buffer, 0161, Fset_buffer, 1)			\
  DEFINE (Bsave_current_buffer_1, 0162) /* Replacing Bsave_current_buffer.  */ \
  DEFINE_FIXED (Binteractive_p, 0164, native_interactive_p, 0) /* Obsolete since Emacs-24.1.  */ \
									\
  DEFINE_FIXED (Bforward_char, 0165, Fforward_char, 1)			\
  DEFINE_FIXED (Bforward_word, 0166, Fforward_word, 1)			\
  DEFINE_FIXED (Bskip_chars_forward, 0167, Fskip_chars_forward, 2)	\
  DEFINE_FIXED (Bskip_chars_backward, 0170, Fskip_chars_backward, 2)	\
  DEFINE_FIXED (Bforward_line, 0171, Fforward_line, 1)			\
  DEFINE_FIXED (Bchar_syntax, 0172, native_char_syntax, 1)		\
  DEFINE_FIXED (Bbuffer_substring, 0173, Fbuffer_substring, 2)		\
  DEFINE_FIXED (Bdelete_region, 0174, Fdelete_region, 2)			\
  DEFINE_FIXED (Bnarrow_to_region, 0175, Fnarrow_to_region, 2)		\
  DEFINE_FIXED (Bwiden, 0176, Fwiden, 0)					\
  DEFINE_FIXED (Bend_of_line, 0177, Fend_of_line, 1)			\
									\
  DEFINE (Bconstant2, 0201)						\
  DEFINE (Bgoto, 0202)							\
  DEFINE (Bgotoifnil, 0203)						\
  DEFINE (Bgotoifnonnil, 0204)						\
  DEFINE (Bgotoifnilelsepop, 0205)					\
  DEFINE (Bgotoifnonnilelsepop, 0206)					\
  DEFINE (Breturn, 0207)						\
  DEFINE (Bdiscard, 0210)						\
  DEFINE (Bdup, 0211)							\
									\
  DEFINE (Bsave_excursion, 0212)					\
  DEFINE_FIXED (Bsave_window_excursion, 0213, native_save_window_excursion, 1) /* Obsolete since Emacs-24.1.  */ \
  DEFINE (Bsave_restriction, 0214)					\
  DEFINE_FIXED (Bcatch, 0215, native_catch, 2)				\
									\
  DEFINE (Bunwind_protect, 0216)					\
  DEFINE_FIXED (Bcondition_case, 0217, internal_lisp_condition_case, 3) \
  DEFINE_FIXED (Btemp_output_buffer_setup, 0220, native_temp_output_buffer_setup, 1) /* Obsolete since Emacs-24.1.  */ \
  DEFINE (Btemp_output_buffer_show, 0221)  /* Obsolete since Emacs-24.1.  */ \
									\
  DEFINE (Bunbind_all, 0222)	/* Obsolete.  Never used.  */		\
									\
  DEFINE_FIXED (Bset_marker, 0223, Fset_marker, 3)			\
  DEFINE_FIXED (Bmatch_beginning, 0224, Fmatch_beginning, 1)		\
  DEFINE_FIXED (Bmatch_end, 0225, Fmatch_end, 1)				\
  DEFINE_FIXED (Bupcase, 0226, Fupcase, 1)				\
  DEFINE_FIXED (Bdowncase, 0227, Fdowncase, 1)				\
									\
  DEFINE_FIXED (Bstringeqlsign, 0230, Fstring_equal, 2)			\
  DEFINE_FIXED (Bstringlss, 0231, Fstring_lessp, 2)			\
  DEFINE_FIXED (Bequal, 0232, Fequal, 2)					\
  DEFINE_FIXED (Bnthcdr, 0233, Fnthcdr, 2)				\
  DEFINE_FIXED (Belt, 0234, native_elt, 2)				\
  DEFINE_FIXED (Bmember, 0235, Fmember, 2)				\
  DEFINE_FIXED (Bassq, 0236, Fassq, 2)					\
  DEFINE_FIXED (Bnreverse, 0237, Fnreverse, 1)				\
  DEFINE_FIXED (Bsetcar, 0240, Fsetcar, 2)				\
  DEFINE_FIXED (Bsetcdr, 0241, Fsetcdr, 2)				\
  DEFINE_FIXED (Bcar_safe, 0242, native_car_safe, 1)			\
  DEFINE_FIXED (Bcdr_safe, 0243, native_cdr_safe, 1)			\
  DEFINE_MANY (Bnconc, 0244, Fnconc, 2)					\
  DEFINE_MANY (Bquo, 0245, Fquo, 2)					\
  DEFINE_FIXED (Brem, 0246, Frem, 2)					\
  DEFINE_FIXED (Bnumberp, 0247, native_number_p, 1)			\
  DEFINE_FIXED (Bintegerp, 0250, native_integer_p, 1)			\
									\
  DEFINE (BRgoto, 0252)							\
  DEFINE (BRgotoifnil, 0253)						\
  DEFINE (BRgotoifnonnil, 0254)						\
  DEFINE (BRgotoifnilelsepop, 0255)					\
  DEFINE (BRgotoifnonnilelsepop, 0256)					\
									\
  DEFINE_MANY (BlistN, 0257, Flist, -1)					\
  DEFINE_MANY (BconcatN, 0260, Fconcat, -1)				\
  DEFINE_MANY (BinsertN, 0261, Finsert, -1)				\
									\
  /* Bstack_ref is code 0.  */						\
  DEFINE (Bstack_set,  0262)						\
  DEFINE (Bstack_set2, 0263)						\
  DEFINE (BdiscardN,   0266)						\
									\
  DEFINE (Bconstant, 0300)

enum byte_code_op
{
#define DEFINE_FIXED(bname, value, fname, num) DEFINE (bname, value)
#define DEFINE_MANY(bname, value, fname, num) \
  DEFINE_FIXED (bname, value, fname, num)
#define DEFINE(name, value) name = value,
    BYTE_CODES
#undef DEFINE

#if BYTE_CODE_SAFE
    Bscan_buffer = 0153, /* No longer generated as of v18.  */
    Bset_mark = 0163, /* this loser is no longer generated as of v18 */
#endif
};

/* Whether to maintain a `top' and `bottom' field in the stack frame.  */
#define BYTE_MAINTAIN_TOP BYTE_CODE_SAFE

/* Structure describing a value stack used during byte-code execution
   in Fbyte_code.  */

struct byte_stack
{
  /* Program counter.  This points into the byte_string below
     and is relocated when that string is relocated.  */
  const unsigned char *pc;

  /* Top and bottom of stack.  The bottom points to an area of memory
     allocated with alloca in Fbyte_code.  */
#if BYTE_MAINTAIN_TOP
  Lisp_Object *top, *bottom;
#endif

  /* The string containing the byte-code, and its current address.
     Storing this here protects it from GC because mark_byte_stack
     marks it.  */
  Lisp_Object byte_string;
  const unsigned char *byte_string_start;

  /* Next entry in byte_stack_list.  */
  struct byte_stack *next;
};

/* A list of currently active byte-code execution value stacks.
   Fbyte_code adds an entry to the head of this list before it starts
   processing byte-code, and it removes the entry again when it is
   done.  Signaling an error truncates the list.

   byte_stack_list is a macro defined in thread.h.  */
/* struct byte_stack *byte_stack_list; */

/* Actions that must be performed before and after calling a function
   that might GC.  */

#if !BYTE_MAINTAIN_TOP
#define BEFORE_POTENTIAL_GC()	((void)0)
#define AFTER_POTENTIAL_GC()	((void)0)
#else
#define BEFORE_POTENTIAL_GC()	stack.top = top
#define AFTER_POTENTIAL_GC()	stack.top = NULL
#endif

/* Garbage collect if we have consed enough since the last time.
   We do this at every branch, to avoid loops that never GC.  */

#define MAYBE_GC()		\
  do {				\
   BEFORE_POTENTIAL_GC ();	\
   maybe_gc ();			\
   AFTER_POTENTIAL_GC ();	\
 } while (0)

extern void
bcall0 (Lisp_Object f);

extern Lisp_Object
exec_byte_code__ (Lisp_Object, Lisp_Object, Lisp_Object,
		  Lisp_Object, ptrdiff_t, Lisp_Object *);

#ifdef HAVE_LIBJIT
extern void
jit_byte_code__ (Lisp_Object);

extern Lisp_Object
jit_exec (Lisp_Object, Lisp_Object, ptrdiff_t, Lisp_Object *);
#endif
