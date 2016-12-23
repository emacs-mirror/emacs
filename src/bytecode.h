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
/* Was Bmark in v17.  */						\
DEFINE (Bsave_current_buffer, 0141) /* Obsolete.  */			\
DEFINE (Bgoto_char, 0142)						\
DEFINE (Binsert, 0143)							\
DEFINE (Bpoint_max, 0144)						\
DEFINE (Bpoint_min, 0145)						\
DEFINE (Bchar_after, 0146)						\
DEFINE (Bfollowing_char, 0147)						\
DEFINE (Bpreceding_char, 0150)						\
DEFINE (Bcurrent_column, 0151)						\
DEFINE (Bindent_to, 0152)						\
DEFINE (Beolp, 0154)							\
DEFINE (Beobp, 0155)							\
DEFINE (Bbolp, 0156)							\
DEFINE (Bbobp, 0157)							\
DEFINE (Bcurrent_buffer, 0160)						\
DEFINE (Bset_buffer, 0161)						\
DEFINE (Bsave_current_buffer_1, 0162) /* Replacing Bsave_current_buffer.  */ \
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
DEFINE (Bcatch, 0215)							\
									\
DEFINE (Bunwind_protect, 0216)						\
DEFINE (Bcondition_case, 0217)						\
DEFINE (Btemp_output_buffer_setup, 0220) /* Obsolete since Emacs-24.1.  */ \
DEFINE (Btemp_output_buffer_show, 0221)  /* Obsolete since Emacs-24.1.  */ \
									\
DEFINE (Bunbind_all, 0222)	/* Obsolete.  Never used.  */		\
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
DEFINE (BRgoto, 0252)							\
DEFINE (BRgotoifnil, 0253)						\
DEFINE (BRgotoifnonnil, 0254)						\
DEFINE (BRgotoifnilelsepop, 0255)					\
DEFINE (BRgotoifnonnilelsepop, 0256)					\
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
DEFINE (Bconstant, 0300)

enum byte_code_op
{
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
