/* Execution of byte code produced by bytecomp.el.
   Copyright (C) 1985, 1986, 1987, 1988, 1993 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

hacked on by jwz@lucid.com 17-jun-91
  o  added a compile-time switch to turn on simple sanity checking;
  o  put back the obsolete byte-codes for error-detection;
  o  added a new instruction, unbind_all, which I will use for 
     tail-recursion elimination;
  o  made temp_output_buffer_show be called with the right number
     of args;
  o  made the new bytecodes be called with args in the right order;
  o  added metering support.

by Hallvard:
  o  added relative jump instructions;
  o  all conditionals now only do QUIT if they jump.
 */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "syntax.h"

/*
 * define BYTE_CODE_SAFE to enable some minor sanity checking (useful for 
 * debugging the byte compiler...)
 *
 * define BYTE_CODE_METER to enable generation of a byte-op usage histogram. 
 */
/* #define BYTE_CODE_SAFE */
/* #define BYTE_CODE_METER */


#ifdef BYTE_CODE_METER

Lisp_Object Vbyte_code_meter, Qbyte_code_meter;
int byte_metering_on;

#define METER_2(code1, code2) \
  XFASTINT (XVECTOR (XVECTOR (Vbyte_code_meter)->contents[(code1)]) \
	    ->contents[(code2)])

#define METER_1(code) METER_2 (0, (code))

#define METER_CODE(last_code, this_code)			\
{								\
  if (byte_metering_on)						\
    {								\
      if (METER_1 (this_code) != ((1<<VALBITS)-1))		\
        METER_1 (this_code)++;					\
      if (last_code						\
	  && METER_2 (last_code, this_code) != ((1<<VALBITS)-1))\
        METER_2 (last_code, this_code)++;			\
    }								\
}

#else /* no BYTE_CODE_METER */

#define METER_CODE(last_code, this_code)

#endif /* no BYTE_CODE_METER */


Lisp_Object Qbytecode;

/*  Byte codes: */

#define Bvarref 010
#define Bvarset 020
#define Bvarbind 030
#define Bcall 040
#define Bunbind 050

#define Bnth 070
#define Bsymbolp 071
#define Bconsp 072
#define Bstringp 073
#define Blistp 074
#define Beq 075
#define Bmemq 076
#define Bnot 077
#define Bcar 0100
#define Bcdr 0101
#define Bcons 0102
#define Blist1 0103
#define Blist2 0104
#define Blist3 0105
#define Blist4 0106
#define Blength 0107
#define Baref 0110
#define Baset 0111
#define Bsymbol_value 0112
#define Bsymbol_function 0113
#define Bset 0114
#define Bfset 0115
#define Bget 0116
#define Bsubstring 0117
#define Bconcat2 0120
#define Bconcat3 0121
#define Bconcat4 0122
#define Bsub1 0123
#define Badd1 0124
#define Beqlsign 0125
#define Bgtr 0126
#define Blss 0127
#define Bleq 0130
#define Bgeq 0131
#define Bdiff 0132
#define Bnegate 0133
#define Bplus 0134
#define Bmax 0135
#define Bmin 0136
#define Bmult 0137

#define Bpoint 0140
#define Bmark 0141 /* no longer generated as of v18 */
#define Bgoto_char 0142
#define Binsert 0143
#define Bpoint_max 0144
#define Bpoint_min 0145
#define Bchar_after 0146
#define Bfollowing_char 0147
#define Bpreceding_char 0150
#define Bcurrent_column 0151
#define Bindent_to 0152
#define Bscan_buffer 0153 /* No longer generated as of v18 */
#define Beolp 0154
#define Beobp 0155
#define Bbolp 0156
#define Bbobp 0157
#define Bcurrent_buffer 0160
#define Bset_buffer 0161
#define Bread_char 0162 /* No longer generated as of v19 */
#define Bset_mark 0163 /* this loser is no longer generated as of v18 */
#define Binteractive_p 0164 /* Needed since interactive-p takes unevalled args */

#define Bforward_char 0165
#define Bforward_word 0166
#define Bskip_chars_forward 0167
#define Bskip_chars_backward 0170
#define Bforward_line 0171
#define Bchar_syntax 0172
#define Bbuffer_substring 0173
#define Bdelete_region 0174
#define Bnarrow_to_region 0175
#define Bwiden 0176
#define Bend_of_line 0177

#define Bconstant2 0201
#define Bgoto 0202
#define Bgotoifnil 0203
#define Bgotoifnonnil 0204
#define Bgotoifnilelsepop 0205
#define Bgotoifnonnilelsepop 0206
#define Breturn 0207
#define Bdiscard 0210
#define Bdup 0211

#define Bsave_excursion 0212
#define Bsave_window_excursion 0213
#define Bsave_restriction 0214
#define Bcatch 0215

#define Bunwind_protect 0216
#define Bcondition_case 0217
#define Btemp_output_buffer_setup 0220
#define Btemp_output_buffer_show 0221

#define Bunbind_all 0222

#define Bset_marker 0223
#define Bmatch_beginning 0224
#define Bmatch_end 0225
#define Bupcase 0226
#define Bdowncase 0227

#define Bstringeqlsign 0230
#define Bstringlss 0231
#define Bequal 0232
#define Bnthcdr 0233
#define Belt 0234
#define Bmember 0235
#define Bassq 0236
#define Bnreverse 0237
#define Bsetcar 0240
#define Bsetcdr 0241
#define Bcar_safe 0242
#define Bcdr_safe 0243
#define Bnconc 0244
#define Bquo 0245
#define Brem 0246
#define Bnumberp 0247
#define Bintegerp 0250

#define BRgoto 0252
#define BRgotoifnil 0253
#define BRgotoifnonnil 0254
#define BRgotoifnilelsepop 0255
#define BRgotoifnonnilelsepop 0256

#define BlistN 0257
#define BconcatN 0260
#define BinsertN 0261

#define Bconstant 0300
#define CONSTANTLIM 0100

/* Fetch the next byte from the bytecode stream */

#define FETCH *pc++

/* Fetch two bytes from the bytecode stream
 and make a 16-bit number out of them */

#define FETCH2 (op = FETCH, op + (FETCH << 8))

/* Push x onto the execution stack. */

/* This used to be #define PUSH(x) (*++stackp = (x))
   This oddity is necessary because Alliant can't be bothered to
   compile the preincrement operator properly, as of 4/91.  -JimB  */
#define PUSH(x) (stackp++, *stackp = (x))

/* Pop a value off the execution stack.  */

#define POP (*stackp--)

/* Discard n values from the execution stack.  */

#define DISCARD(n) (stackp -= (n))

/* Get the value which is at the top of the execution stack, but don't pop it. */

#define TOP (*stackp)

DEFUN ("byte-code", Fbyte_code, Sbyte_code, 3, 3, 0,
  "Function used internally in byte-compiled code.\n\
The first argument is a string of byte code; the second, a vector of constants;\n\
the third, the maximum stack depth used in this function.\n\
If the third argument is incorrect, Emacs may crash.")
  (bytestr, vector, maxdepth)
     Lisp_Object bytestr, vector, maxdepth;
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  int count = specpdl_ptr - specpdl;
#ifdef BYTE_CODE_METER
  int this_op = 0;
  int prev_op;
#endif
  register int op;
  unsigned char *pc;
  Lisp_Object *stack;
  register Lisp_Object *stackp;
  Lisp_Object *stacke;
  register Lisp_Object v1, v2;
  register Lisp_Object *vectorp = XVECTOR (vector)->contents;
#ifdef BYTE_CODE_SAFE
  register int const_length = XVECTOR (vector)->size;
#endif
  /* Copy of BYTESTR, saved so we can tell if BYTESTR was relocated.  */
  Lisp_Object string_saved;
  /* Cached address of beginning of string,
     valid if BYTESTR equals STRING_SAVED.  */
  register unsigned char *strbeg;

  CHECK_STRING (bytestr, 0);
  if (!VECTORP (vector))
    vector = wrong_type_argument (Qvectorp, vector);
  CHECK_NUMBER (maxdepth, 2);

  stackp = (Lisp_Object *) alloca (XFASTINT (maxdepth) * sizeof (Lisp_Object));
  bzero (stackp, XFASTINT (maxdepth) * sizeof (Lisp_Object));
  GCPRO3 (bytestr, vector, *stackp);
  gcpro3.nvars = XFASTINT (maxdepth);

  --stackp;
  stack = stackp;
  stacke = stackp + XFASTINT (maxdepth);

  /* Initialize the saved pc-pointer for fetching from the string.  */
  string_saved = bytestr;
  pc = XSTRING (string_saved)->data;

  while (1)
    {
#ifdef BYTE_CODE_SAFE
      if (stackp > stacke)
	error ("Byte code stack overflow (byte compiler bug), pc %d, depth %d",
	       pc - XSTRING (string_saved)->data, stacke - stackp);
      if (stackp < stack)
	error ("Byte code stack underflow (byte compiler bug), pc %d",
	       pc - XSTRING (string_saved)->data);
#endif

      if (! EQ (string_saved, bytestr))
	{
	  pc = pc - XSTRING (string_saved)->data + XSTRING (bytestr)->data;
	  string_saved = bytestr;
	}

#ifdef BYTE_CODE_METER
      prev_op = this_op;
      this_op = op = FETCH;
      METER_CODE (prev_op, op);
      switch (op)
#else
      switch (op = FETCH)
#endif
	{
	case Bvarref+6:
	  op = FETCH;
	  goto varref;

	case Bvarref+7:
	  op = FETCH2;
	  goto varref;

	case Bvarref: case Bvarref+1: case Bvarref+2: case Bvarref+3:
	case Bvarref+4: case Bvarref+5:
	  op = op - Bvarref;
	varref:
	  v1 = vectorp[op];
	  if (!SYMBOLP (v1))
	    v2 = Fsymbol_value (v1);
	  else
	    {
	      v2 = XSYMBOL (v1)->value;
	      if (MISCP (v2) || EQ (v2, Qunbound))
		v2 = Fsymbol_value (v1);
	    }
	  PUSH (v2);
	  break;

	case Bvarset+6:
	  op = FETCH;
	  goto varset;

	case Bvarset+7:
	  op = FETCH2;
	  goto varset;

	case Bvarset: case Bvarset+1: case Bvarset+2: case Bvarset+3:
	case Bvarset+4: case Bvarset+5:
	  op -= Bvarset;
	varset:
	  Fset (vectorp[op], POP);
	  break;

	case Bvarbind+6:
	  op = FETCH;
	  goto varbind;

	case Bvarbind+7:
	  op = FETCH2;
	  goto varbind;

	case Bvarbind: case Bvarbind+1: case Bvarbind+2: case Bvarbind+3:
	case Bvarbind+4: case Bvarbind+5:
	  op -= Bvarbind;
	varbind:
	  specbind (vectorp[op], POP);
	  break;

	case Bcall+6:
	  op = FETCH;
	  goto docall;

	case Bcall+7:
	  op = FETCH2;
	  goto docall;

	case Bcall: case Bcall+1: case Bcall+2: case Bcall+3:
	case Bcall+4: case Bcall+5:
	  op -= Bcall;
	docall:
	  DISCARD (op);
#ifdef BYTE_CODE_METER
	  if (byte_metering_on && SYMBOLP (TOP))
	    {
	      v1 = TOP;
	      v2 = Fget (v1, Qbyte_code_meter);
	      if (INTEGERP (v2)
		  && XINT (v2) != ((1<<VALBITS)-1))
		{
		  XSETINT (v2, XINT (v2) + 1);
		  Fput (v1, Qbyte_code_meter, v2);
		}
	    }
#endif
	  TOP = Ffuncall (op + 1, &TOP);
	  break;

	case Bunbind+6:
	  op = FETCH;
	  goto dounbind;

	case Bunbind+7:
	  op = FETCH2;
	  goto dounbind;

	case Bunbind: case Bunbind+1: case Bunbind+2: case Bunbind+3:
	case Bunbind+4: case Bunbind+5:
	  op -= Bunbind;
	dounbind:
	  unbind_to (specpdl_ptr - specpdl - op, Qnil);
	  break;

	case Bunbind_all:
	  /* To unbind back to the beginning of this frame.  Not used yet,
	     but will be needed for tail-recursion elimination.  */
	  unbind_to (count, Qnil);
	  break;

	case Bgoto:
	  QUIT;
	  op = FETCH2;    /* pc = FETCH2 loses since FETCH2 contains pc++ */
	  pc = XSTRING (string_saved)->data + op;
	  break;

	case Bgotoifnil:
	  op = FETCH2;
	  if (NILP (POP))
	    {
	      QUIT;
	      pc = XSTRING (string_saved)->data + op;
	    }
	  break;

	case Bgotoifnonnil:
	  op = FETCH2;
	  if (!NILP (POP))
	    {
	      QUIT;
	      pc = XSTRING (string_saved)->data + op;
	    }
	  break;

	case Bgotoifnilelsepop:
	  op = FETCH2;
	  if (NILP (TOP))
	    {
	      QUIT;
	      pc = XSTRING (string_saved)->data + op;
	    }
	  else DISCARD (1);
	  break;

	case Bgotoifnonnilelsepop:
	  op = FETCH2;
	  if (!NILP (TOP))
	    {
	      QUIT;
	      pc = XSTRING (string_saved)->data + op;
	    }
	  else DISCARD (1);
	  break;

	case BRgoto:
	  QUIT;
	  pc += *pc - 127;
	  break;

	case BRgotoifnil:
	  if (NILP (POP))
	    {
	      QUIT;
	      pc += *pc - 128;
	    }
	  pc++;
	  break;

	case BRgotoifnonnil:
	  if (!NILP (POP))
	    {
	      QUIT;
	      pc += *pc - 128;
	    }
	  pc++;
	  break;

	case BRgotoifnilelsepop:
	  op = *pc++;
	  if (NILP (TOP))
	    {
	      QUIT;
	      pc += op - 128;
	    }
	  else DISCARD (1);
	  break;

	case BRgotoifnonnilelsepop:
	  op = *pc++;
	  if (!NILP (TOP))
	    {
	      QUIT;
	      pc += op - 128;
	    }
	  else DISCARD (1);
	  break;

	case Breturn:
	  v1 = POP;
	  goto exit;

	case Bdiscard:
	  DISCARD (1);
	  break;

	case Bdup:
	  v1 = TOP;
	  PUSH (v1);
	  break;

	case Bconstant2:
	  PUSH (vectorp[FETCH2]);
	  break;

	case Bsave_excursion:
	  record_unwind_protect (save_excursion_restore, save_excursion_save ());
	  break;

	case Bsave_window_excursion:
	  TOP = Fsave_window_excursion (TOP);
	  break;

	case Bsave_restriction:
	  record_unwind_protect (save_restriction_restore, save_restriction_save ());
	  break;

	case Bcatch:
	  v1 = POP;
	  TOP = internal_catch (TOP, Feval, v1);
	  break;

	case Bunwind_protect:
	  record_unwind_protect (0, POP);
	  (specpdl_ptr - 1)->symbol = Qnil;
	  break;

	case Bcondition_case:
	  v1 = POP;
	  v1 = Fcons (POP, v1);
	  TOP = Fcondition_case (Fcons (TOP, v1));
	  break;

	case Btemp_output_buffer_setup:
	  temp_output_buffer_setup (XSTRING (TOP)->data);
	  TOP = Vstandard_output;
	  break;

	case Btemp_output_buffer_show:
	  v1 = POP;
	  temp_output_buffer_show (TOP);
	  TOP = v1;
	  /* pop binding of standard-output */
	  unbind_to (specpdl_ptr - specpdl - 1, Qnil);
	  break;

	case Bnth:
	  v1 = POP;
	  v2 = TOP;
	nth_entry:
	  CHECK_NUMBER (v2, 0);
	  op = XINT (v2);
	  immediate_quit = 1;
	  while (--op >= 0)
	    {
	      if (CONSP (v1))
		v1 = XCONS (v1)->cdr;
	      else if (!NILP (v1))
		{
		  immediate_quit = 0;
		  v1 = wrong_type_argument (Qlistp, v1);
		  immediate_quit = 1;
		  op++;
		}
	    }
	  immediate_quit = 0;
	  goto docar;

	case Bsymbolp:
	  TOP = SYMBOLP (TOP) ? Qt : Qnil;
	  break;

	case Bconsp:
	  TOP = CONSP (TOP) ? Qt : Qnil;
	  break;

	case Bstringp:
	  TOP = STRINGP (TOP) ? Qt : Qnil;
	  break;

	case Blistp:
	  TOP = CONSP (TOP) || NILP (TOP) ? Qt : Qnil;
	  break;

	case Beq:
	  v1 = POP;
	  TOP = EQ (v1, TOP) ? Qt : Qnil;
	  break;

	case Bmemq:
	  v1 = POP;
	  TOP = Fmemq (TOP, v1);
	  break;

	case Bnot:
	  TOP = NILP (TOP) ? Qt : Qnil;
	  break;

	case Bcar:
	  v1 = TOP;
	docar:
	  if (CONSP (v1)) TOP = XCONS (v1)->car;
	  else if (NILP (v1)) TOP = Qnil;
	  else Fcar (wrong_type_argument (Qlistp, v1));
	  break;

	case Bcdr:
	  v1 = TOP;
	  if (CONSP (v1)) TOP = XCONS (v1)->cdr;
	  else if (NILP (v1)) TOP = Qnil;
	  else Fcdr (wrong_type_argument (Qlistp, v1));
	  break;

	case Bcons:
	  v1 = POP;
	  TOP = Fcons (TOP, v1);
	  break;

	case Blist1:
	  TOP = Fcons (TOP, Qnil);
	  break;

	case Blist2:
	  v1 = POP;
	  TOP = Fcons (TOP, Fcons (v1, Qnil));
	  break;

	case Blist3:
	  DISCARD (2);
	  TOP = Flist (3, &TOP);
	  break;

	case Blist4:
	  DISCARD (3);
	  TOP = Flist (4, &TOP);
	  break;

	case BlistN:
	  op = FETCH;
	  DISCARD (op - 1);
	  TOP = Flist (op, &TOP);
	  break;

	case Blength:
	  TOP = Flength (TOP);
	  break;

	case Baref:
    	  v1 = POP;
	  TOP = Faref (TOP, v1);
	  break;

	case Baset:
	  v2 = POP; v1 = POP;
	  TOP = Faset (TOP, v1, v2);
	  break;

	case Bsymbol_value:
	  TOP = Fsymbol_value (TOP);
	  break;

	case Bsymbol_function:
	  TOP = Fsymbol_function (TOP);
	  break;

	case Bset:
	  v1 = POP;
	  TOP = Fset (TOP, v1);
	  break;

	case Bfset:
	  v1 = POP;
	  TOP = Ffset (TOP, v1);
	  break;

	case Bget:
	  v1 = POP;
	  TOP = Fget (TOP, v1);
	  break;

	case Bsubstring:
	  v2 = POP; v1 = POP;
	  TOP = Fsubstring (TOP, v1, v2);
	  break;

	case Bconcat2:
	  DISCARD (1);
	  TOP = Fconcat (2, &TOP);
	  break;

	case Bconcat3:
	  DISCARD (2);
	  TOP = Fconcat (3, &TOP);
	  break;

	case Bconcat4:
	  DISCARD (3);
	  TOP = Fconcat (4, &TOP);
	  break;

	case BconcatN:
	  op = FETCH;
	  DISCARD (op - 1);
	  TOP = Fconcat (op, &TOP);
	  break;

	case Bsub1:
	  v1 = TOP;
	  if (INTEGERP (v1))
	    {
	      XSETINT (v1, XINT (v1) - 1);
	      TOP = v1;
	    }
	  else
	    TOP = Fsub1 (v1);
	  break;

	case Badd1:
	  v1 = TOP;
	  if (INTEGERP (v1))
	    {
	      XSETINT (v1, XINT (v1) + 1);
	      TOP = v1;
	    }
	  else
	    TOP = Fadd1 (v1);
	  break;

	case Beqlsign:
	  v2 = POP; v1 = TOP;
	  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (v1, 0);
	  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (v2, 0);
	  TOP = (XFLOATINT (v1) == XFLOATINT (v2)) ? Qt : Qnil;
	  break;

	case Bgtr:
	  v1 = POP;
	  TOP = Fgtr (TOP, v1);
	  break;

	case Blss:
	  v1 = POP;
	  TOP = Flss (TOP, v1);
	  break;

	case Bleq:
	  v1 = POP;
	  TOP = Fleq (TOP, v1);
	  break;

	case Bgeq:
	  v1 = POP;
	  TOP = Fgeq (TOP, v1);
	  break;

	case Bdiff:
	  DISCARD (1);
	  TOP = Fminus (2, &TOP);
	  break;

	case Bnegate:
	  v1 = TOP;
	  if (INTEGERP (v1))
	    {
	      XSETINT (v1, - XINT (v1));
	      TOP = v1;
	    }
	  else
	    TOP = Fminus (1, &TOP);
	  break;

	case Bplus:
	  DISCARD (1);
	  TOP = Fplus (2, &TOP);
	  break;

	case Bmax:
	  DISCARD (1);
	  TOP = Fmax (2, &TOP);
	  break;

	case Bmin:
	  DISCARD (1);
	  TOP = Fmin (2, &TOP);
	  break;

	case Bmult:
	  DISCARD (1);
	  TOP = Ftimes (2, &TOP);
	  break;

	case Bquo:
	  DISCARD (1);
	  TOP = Fquo (2, &TOP);
	  break;

	case Brem:
	  v1 = POP;
	  TOP = Frem (TOP, v1);
	  break;

	case Bpoint:
	  XSETFASTINT (v1, point);
	  PUSH (v1);
	  break;

	case Bgoto_char:
	  TOP = Fgoto_char (TOP);
	  break;

	case Binsert:
	  TOP = Finsert (1, &TOP);
	  break;

	case BinsertN:
	  op = FETCH;
	  DISCARD (op - 1);
	  TOP = Finsert (op, &TOP);
	  break;

	case Bpoint_max:
	  XSETFASTINT (v1, ZV);
	  PUSH (v1);
	  break;

	case Bpoint_min:
	  XSETFASTINT (v1, BEGV);
	  PUSH (v1);
	  break;

	case Bchar_after:
	  TOP = Fchar_after (TOP);
	  break;

	case Bfollowing_char:
	  v1 = Ffollowing_char ();
	  PUSH (v1);
	  break;

	case Bpreceding_char:
	  v1 = Fprevious_char ();
	  PUSH (v1);
	  break;

	case Bcurrent_column:
	  XSETFASTINT (v1, current_column ());
	  PUSH (v1);
	  break;

	case Bindent_to:
	  TOP = Findent_to (TOP, Qnil);
	  break;

	case Beolp:
	  PUSH (Feolp ());
	  break;

	case Beobp:
	  PUSH (Feobp ());
	  break;

	case Bbolp:
	  PUSH (Fbolp ());
	  break;

	case Bbobp:
	  PUSH (Fbobp ());
	  break;

	case Bcurrent_buffer:
	  PUSH (Fcurrent_buffer ());
	  break;

	case Bset_buffer:
	  TOP = Fset_buffer (TOP);
	  break;

	case Bread_char:
	  PUSH (Fread_char ());
	  QUIT;
	  break;

	case Binteractive_p:
	  PUSH (Finteractive_p ());
	  break;

	case Bforward_char:
	  TOP = Fforward_char (TOP);
	  break;

	case Bforward_word:
	  TOP = Fforward_word (TOP);
	  break;

	case Bskip_chars_forward:
	  v1 = POP;
	  TOP = Fskip_chars_forward (TOP, v1);
	  break;

	case Bskip_chars_backward:
	  v1 = POP;
	  TOP = Fskip_chars_backward (TOP, v1);
	  break;

	case Bforward_line:
	  TOP = Fforward_line (TOP);
	  break;

	case Bchar_syntax:
	  CHECK_NUMBER (TOP, 0);
	  XSETFASTINT (TOP,
		       syntax_code_spec[(int) SYNTAX (XINT (TOP))]);
	  break;

	case Bbuffer_substring:
	  v1 = POP;
	  TOP = Fbuffer_substring (TOP, v1);
	  break;

	case Bdelete_region:
	  v1 = POP;
	  TOP = Fdelete_region (TOP, v1);
	  break;

	case Bnarrow_to_region:
	  v1 = POP;
	  TOP = Fnarrow_to_region (TOP, v1);
	  break;

	case Bwiden:
	  PUSH (Fwiden ());
	  break;

	case Bend_of_line:
	  TOP = Fend_of_line (TOP);
	  break;

	case Bset_marker:
	  v1 = POP;
	  v2 = POP;
	  TOP = Fset_marker (TOP, v2, v1);
	  break;

	case Bmatch_beginning:
	  TOP = Fmatch_beginning (TOP);
	  break;

	case Bmatch_end:
	  TOP = Fmatch_end (TOP);
	  break;

	case Bupcase:
	  TOP = Fupcase (TOP);
	  break;

	case Bdowncase:
	  TOP = Fdowncase (TOP);
	break;

	case Bstringeqlsign:
	  v1 = POP;
	  TOP = Fstring_equal (TOP, v1);
	  break;

	case Bstringlss:
	  v1 = POP;
	  TOP = Fstring_lessp (TOP, v1);
	  break;

	case Bequal:
	  v1 = POP;
	  TOP = Fequal (TOP, v1);
	  break;

	case Bnthcdr:
	  v1 = POP;
	  TOP = Fnthcdr (TOP, v1);
	  break;

	case Belt:
	  if (CONSP (TOP))
	    {
	      /* Exchange args and then do nth.  */
	      v2 = POP;
	      v1 = TOP;
	      goto nth_entry;
	    }
	  v1 = POP;
	  TOP = Felt (TOP, v1);
	  break;

	case Bmember:
	  v1 = POP;
	  TOP = Fmember (TOP, v1);
	  break;

	case Bassq:
	  v1 = POP;
	  TOP = Fassq (TOP, v1);
	  break;

	case Bnreverse:
	  TOP = Fnreverse (TOP);
	  break;

	case Bsetcar:
	  v1 = POP;
	  TOP = Fsetcar (TOP, v1);
	  break;

	case Bsetcdr:
	  v1 = POP;
	  TOP = Fsetcdr (TOP, v1);
	  break;

	case Bcar_safe:
	  v1 = TOP;
	  if (CONSP (v1))
	    TOP = XCONS (v1)->car;
	  else
	    TOP = Qnil;
	  break;

	case Bcdr_safe:
	  v1 = TOP;
	  if (CONSP (v1))
	    TOP = XCONS (v1)->cdr;
	  else
	    TOP = Qnil;
	  break;

	case Bnconc:
	  DISCARD (1);
	  TOP = Fnconc (2, &TOP);
	  break;

	case Bnumberp:
	  TOP = (NUMBERP (TOP) ? Qt : Qnil);
	  break;

	case Bintegerp:
	  TOP = INTEGERP (TOP) ? Qt : Qnil;
	  break;

#ifdef BYTE_CODE_SAFE
	case Bset_mark:
	  error ("set-mark is an obsolete bytecode");
	  break;
	case Bscan_buffer:
	  error ("scan-buffer is an obsolete bytecode");
	  break;
	case Bmark:
	  error ("mark is an obsolete bytecode");
	  break;
#endif

	default:
#ifdef BYTE_CODE_SAFE
	  if (op < Bconstant)
	    error ("unknown bytecode %d (byte compiler bug)", op);
	  if ((op -= Bconstant) >= const_length)
	    error ("no constant number %d (byte compiler bug)", op);
	  PUSH (vectorp[op]);
#else
	  PUSH (vectorp[op - Bconstant]);
#endif
	}
    }

 exit:
  UNGCPRO;
  /* Binds and unbinds are supposed to be compiled balanced.  */
  if (specpdl_ptr - specpdl != count)
#ifdef BYTE_CODE_SAFE
    error ("binding stack not balanced (serious byte compiler bug)");
#else
    abort ();
#endif
  return v1;
}

syms_of_bytecode ()
{
  Qbytecode = intern ("byte-code");
  staticpro (&Qbytecode);

  defsubr (&Sbyte_code);

#ifdef BYTE_CODE_METER

  DEFVAR_LISP ("byte-code-meter", &Vbyte_code_meter,
   "A vector of vectors which holds a histogram of byte-code usage.\n\
(aref (aref byte-code-meter 0) CODE) indicates how many times the byte\n\
opcode CODE has been executed.\n\
(aref (aref byte-code-meter CODE1) CODE2), where CODE1 is not 0,\n\
indicates how many times the byte opcodes CODE1 and CODE2 have been\n\
executed in succession.");
  DEFVAR_BOOL ("byte-metering-on", &byte_metering_on,
   "If non-nil, keep profiling information on byte code usage.\n\
The variable byte-code-meter indicates how often each byte opcode is used.\n\
If a symbol has a property named `byte-code-meter' whose value is an\n\
integer, it is incremented each time that symbol's function is called.");

  byte_metering_on = 0;
  Vbyte_code_meter = Fmake_vector (make_number (256), make_number (0));
  Qbyte_code_meter = intern ("byte-code-meter");
  staticpro (&Qbyte_code_meter);
  {
    int i = 256;
    while (i--)
      XVECTOR (Vbyte_code_meter)->contents[i] =
	Fmake_vector (make_number (256), make_number (0));
  }
#endif
}
