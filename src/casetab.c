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
      compute_trt_inverse (XCHAR_TABLE (table), XCHAR_TABLE (up));
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
      for (i = 0; i < 256; i++)
	XCHAR_TABLE (canon)->contents[i] = downvec[upvec[downvec[i]]];
      XCHAR_TABLE (table)->extras[1] = canon;
    }

  if (NILP (eqv))
    {
      eqv = Fmake_char_table (Qcase_table, Qnil);
      compute_trt_inverse (XCHAR_TABLE (canon), XCHAR_TABLE (eqv));
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

/* Given a translate table TRT, store the inverse mapping into INVERSE.
   Since TRT is not one-to-one, INVERSE is not a simple mapping.
   Instead, it divides the space of characters into equivalence classes.
   All characters in a given class form one circular list, chained through
   the elements of INVERSE.  */

static void
compute_trt_inverse (trt, inverse)
     struct Lisp_Char_Table *trt, *inverse;
{
  register int i = 0400;
  register unsigned char c, q;

  while (i--)
    inverse->contents[i] = i;
  i = 0400;
  while (i--)
    {
      if ((q = trt->contents[i]) != (unsigned char) i)
	{
	  c = inverse->contents[q];
	  inverse->contents[q] = i;
	  inverse->contents[i] = c;
	}
    }
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

  for (i = 0; i < 256; i++)
    XCHAR_TABLE (down)->contents[i] = (i >= 'A' && i <= 'Z') ? i + 040 : i;

  XCHAR_TABLE (down)->extras[1] = Fcopy_sequence (down);

  up = Fmake_char_table (Qcase_table, Qnil);
  XCHAR_TABLE (down)->extras[0] = up;

  for (i = 0; i < 256; i++)
    XCHAR_TABLE (up)->contents[i]
      = ((i >= 'A' && i <= 'Z')
	 ? i + ('a' - 'A')
	 : ((i >= 'a' && i <= 'z')
	    ? i + ('A' - 'a')
	    : i));

  XCHAR_TABLE (down)->extras[2] = Fcopy_sequence (up);
}

syms_of_casetab ()
{
  Qcase_table_p = intern ("case-table-p");
  staticpro (&Qcase_table_p);

  staticpro (&Vascii_downcase_table);

  defsubr (&Scase_table_p);
  defsubr (&Scurrent_case_table);
  defsubr (&Sstandard_case_table);
  defsubr (&Sset_case_table);
  defsubr (&Sset_standard_case_table);
}
