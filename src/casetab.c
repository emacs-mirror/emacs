/* GNU Emacs routines to deal with case tables.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Written by Howard Gayle.  See chartab.c for details. */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "charset.h"

Lisp_Object Qcase_table_p, Qcase_table;
Lisp_Object Vascii_downcase_table, Vascii_upcase_table;
Lisp_Object Vascii_canon_table, Vascii_eqv_table;

static void compute_trt_inverse ();

DEFUN ("case-table-p", Fcase_table_p, Scase_table_p, 1, 1, 0,
  "Return t iff OBJECT is a case table.\n\
See `set-case-table' for more information on these data structures.")
  (object)
     Lisp_Object object;
{
  Lisp_Object up, canon, eqv;

  if (! CHAR_TABLE_P (object))
    return Qnil;
  if (! EQ (XCHAR_TABLE (object)->purpose, Qcase_table))
    return Qnil;

  up = XCHAR_TABLE (object)->extras[0];
  canon = XCHAR_TABLE (object)->extras[1];
  eqv = XCHAR_TABLE (object)->extras[2];

  return ((NILP (up) || CHAR_TABLE_P (up))
	  && ((NILP (canon) && NILP (eqv))
 	      || (CHAR_TABLE_P (canon)
		  && (NILP (eqv) || CHAR_TABLE_P (eqv))))
	  ? Qt : Qnil);
}

static Lisp_Object
check_case_table (obj)
     Lisp_Object obj;
{
  register Lisp_Object tem;

  while (tem = Fcase_table_p (obj), NILP (tem))
    obj = wrong_type_argument (Qcase_table_p, obj);
  return (obj);
}   

DEFUN ("current-case-table", Fcurrent_case_table, Scurrent_case_table, 0, 0, 0,
  "Return the case table of the current buffer.")
  ()
{
  return current_buffer->downcase_table;
}

DEFUN ("standard-case-table", Fstandard_case_table, Sstandard_case_table, 0, 0, 0,
  "Return the standard case table.\n\
This is the one used for new buffers.")
  ()
{
  return Vascii_downcase_table;
}

static Lisp_Object set_case_table ();

DEFUN ("set-case-table", Fset_case_table, Sset_case_table, 1, 1, 0,
  "Select a new case table for the current buffer.\n\
A case table is a char-table which maps characters\n\
to their lower-case equivalents.  It also has three \"extra\" slots\n\
which may be additional char-tables or nil.\n\
These slots are called UPCASE, CANONICALIZE and EQUIVALENCES.\n\
UPCASE maps each character to its upper-case equivalent;\n\
 if lower and upper case characters are in 1-1 correspondence,\n\
 you may use nil and the upcase table will be deduced from DOWNCASE.\n\
CANONICALIZE maps each character to a canonical equivalent;\n\
 any two characters that are related by case-conversion have the same\n\
 canonical equivalent character; it may be nil, in which case it is\n\
 deduced from DOWNCASE and UPCASE.\n\
EQUIVALENCES is a map that cyclicly permutes each equivalence class\n\
 (of characters with the same canonical equivalent); it may be nil,\n\
 in which case it is deduced from CANONICALIZE.")
  (table)
     Lisp_Object table;
{
  return set_case_table (table, 0);
}

DEFUN ("set-standard-case-table", Fset_standard_case_table, Sset_standard_case_table, 1, 1, 0,
  "Select a new standard case table for new buffers.\n\
See `set-case-table' for more info on case tables.")
  (table)
     Lisp_Object table;
{
  return set_case_table (table, 1);
}

static Lisp_Object
set_case_table (table, standard)
     Lisp_Object table;
     int standard;
{
  Lisp_Object up, canon, eqv;

  check_case_table (table);

  up = XCHAR_TABLE (table)->extras[0];
  canon = XCHAR_TABLE (table)->extras[1];
  eqv = XCHAR_TABLE (table)->extras[2];

  if (NILP (up))
    {
      up = Fmake_char_table (Qcase_table, Qnil);
      compute_trt_inverse (table, up);
      XCHAR_TABLE (table)->extras[0] = up;
    }

  if (NILP (canon))
    {
      register int i;
      Lisp_Object *upvec = XCHAR_TABLE (up)->contents;
      Lisp_Object *downvec = XCHAR_TABLE (table)->contents;

      canon = Fmake_char_table (Qcase_table, Qnil);

      /* Set up the CANON vector; for each character,
	 this sequence of upcasing and downcasing ought to
	 get the "preferred" lowercase equivalent.  */
      for (i = 0; i < CHAR_TABLE_SINGLE_BYTE_SLOTS; i++)
	XCHAR_TABLE (canon)->contents[i] = downvec[upvec[downvec[i]]];
      XCHAR_TABLE (table)->extras[1] = canon;
    }

  if (NILP (eqv))
    {
      eqv = Fmake_char_table (Qcase_table, Qnil);
      compute_trt_inverse (canon, eqv);
      XCHAR_TABLE (table)->extras[2] = eqv;
    }

  if (standard)
    Vascii_downcase_table = table;
  else
    {
      current_buffer->downcase_table = table;
      current_buffer->upcase_table = up;
      current_buffer->case_canon_table = canon;
      current_buffer->case_eqv_table = eqv;
    }

  return table;
}

/* Using the scratch array at BYTES of which the first DEPTH elements
   are already set, and using the multi-byte structure inherited from
   TRT, make INVERSE be an identity mapping.  That is, for each slot
   that's indexed by a single byte, store that byte in INVERSE.
   Where TRT has a subtable, make a corresponding subtable in INVERSE
   and recursively initialize that subtable so that its elements are
   the multi-byte characters that correspond to the index bytes.
   This is the first step in generating an inverse mapping.  */

static void
compute_trt_identity (bytes, depth, trt, inverse)
     unsigned char *bytes;
     int depth;
     struct Lisp_Char_Table *trt, *inverse;
{
  register int i;
  int lim = (depth == 0 ? CHAR_TABLE_ORDINARY_SLOTS : SUB_CHAR_TABLE_ORDINARY_SLOTS);

  for (i = 0; i < lim; i++)
    {
      if (NATNUMP (trt->contents[i]))
	{
	  bytes[depth] = i;
	  XSETFASTINT (inverse->contents[i],
		       (depth == 0 && i < CHAR_TABLE_SINGLE_BYTE_SLOTS ? i
			: MAKE_NON_ASCII_CHAR (bytes[0], bytes[1], bytes[2])));
	}
      else if (SUB_CHAR_TABLE_P (trt->contents[i]))
	{
	  bytes[depth] = i - 128;
	  inverse->contents[i] = make_sub_char_table (Qnil);
	  compute_trt_identity (bytes, depth + 1,
				XCHAR_TABLE (trt->contents[i]),
				XCHAR_TABLE (inverse->contents[i]));
	}
      else /* must be Qnil or Qidentity */
	inverse->contents[i] = trt->contents[i];
    }
}

/* Using the scratch array at BYTES of which the first DEPTH elements
   are already set, permute the elements of INVERSE (which is initially
   an identity mapping) so that it has one cycle for each equivalence
   class induced by the translation table TRT.  IBASE is the lispy
   version of the outermost (depth 0) instance of INVERSE.  */

static void
compute_trt_shuffle (bytes, depth, ibase, trt, inverse)
     unsigned char *bytes;
     int depth;
     Lisp_Object ibase;
     struct Lisp_Char_Table *trt, *inverse;
{
  register int i;
  Lisp_Object j, tem, q;
  int lim = (depth == 0 ? CHAR_TABLE_ORDINARY_SLOTS : SUB_CHAR_TABLE_ORDINARY_SLOTS);

  for (i = 0; i < lim; i++)
    {
      bytes[depth] = i;
      XSETFASTINT (j,
		   (depth == 0 && i < CHAR_TABLE_SINGLE_BYTE_SLOTS ? i
		    : MAKE_NON_ASCII_CHAR (bytes[0], bytes[1], bytes[2])));
      q = trt->contents[i];
      if (NATNUMP (q) && XFASTINT (q) != XFASTINT (j))
	{
	  tem = Faref (ibase, q);
	  Faset (ibase, q, j);
	  Faset (ibase, j, tem);
	}
      else if (SUB_CHAR_TABLE_P (q))
	{
	  bytes[depth] = i - 128;
	  compute_trt_shuffle (bytes, depth + 1, ibase,
			       XCHAR_TABLE (trt->contents[i]),
			       XCHAR_TABLE (inverse->contents[i]));
	}
    }
}

/* Given a translate table TRT, store the inverse mapping into INVERSE.
   Since TRT is not one-to-one, INVERSE is not a simple mapping.
   Instead, it divides the space of characters into equivalence classes.
   All characters in a given class form one circular list, chained through
   the elements of INVERSE.  */

static void
compute_trt_inverse (trt, inv)
     Lisp_Object trt, inv;
{
  unsigned char bytes[3];
  compute_trt_identity (bytes, 0, XCHAR_TABLE (trt), XCHAR_TABLE (inv));
  compute_trt_shuffle (bytes, 0, inv, XCHAR_TABLE (trt), XCHAR_TABLE (inv));
}

init_casetab_once ()
{
  register int i;
  Lisp_Object down, up;
  Qcase_table = intern ("case-table");
  staticpro (&Qcase_table);

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern ("char-table-extra-slots");

  /* Now we are ready to set up this property, so we can
     create char tables.  */
  Fput (Qcase_table, Qchar_table_extra_slots, make_number (3));

  down = Fmake_char_table (Qcase_table, Qnil);
  Vascii_downcase_table = down;
  XCHAR_TABLE (down)->purpose = Qcase_table;

  for (i = 0; i < CHAR_TABLE_SINGLE_BYTE_SLOTS; i++)
    XSETFASTINT (XCHAR_TABLE (down)->contents[i],
		 (i >= 'A' && i <= 'Z') ? i + ('a' - 'A') : i);

  XCHAR_TABLE (down)->extras[1] = Fcopy_sequence (down);

  up = Fmake_char_table (Qcase_table, Qnil);
  XCHAR_TABLE (down)->extras[0] = up;

  for (i = 0; i < CHAR_TABLE_SINGLE_BYTE_SLOTS; i++)
    XSETFASTINT (XCHAR_TABLE (up)->contents[i],
		 ((i >= 'A' && i <= 'Z')
		  ? i + ('a' - 'A')
		  : ((i >= 'a' && i <= 'z')
		     ? i + ('A' - 'a')
		     : i)));

  XCHAR_TABLE (down)->extras[2] = Fcopy_sequence (up);
}

syms_of_casetab ()
{
  Qcase_table_p = intern ("case-table-p");
  staticpro (&Qcase_table_p);

  staticpro (&Vascii_canon_table);
  staticpro (&Vascii_downcase_table);
  staticpro (&Vascii_eqv_table);
  staticpro (&Vascii_upcase_table);

  defsubr (&Scase_table_p);
  defsubr (&Scurrent_case_table);
  defsubr (&Sstandard_case_table);
  defsubr (&Sset_case_table);
  defsubr (&Sset_standard_case_table);
}
