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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Howard Gayle.  See chartab.c for details. */

#include <config.h>
#include "lisp.h"
#include "buffer.h"

Lisp_Object Qcase_table_p;
Lisp_Object Vascii_downcase_table, Vascii_upcase_table;
Lisp_Object Vascii_canon_table, Vascii_eqv_table;

void compute_trt_inverse ();

DEFUN ("case-table-p", Fcase_table_p, Scase_table_p, 1, 1, 0,
  "Return t iff ARG is a case table.\n\
See `set-case-table' for more information on these data structures.")
  (table)
     Lisp_Object table;
{
  Lisp_Object down, up, canon, eqv;
  down = Fcar_safe (table);
  up = Fcar_safe (Fcdr_safe (table));
  canon = Fcar_safe (Fcdr_safe (Fcdr_safe (table)));
  eqv = Fcar_safe (Fcdr_safe (Fcdr_safe (Fcdr_safe (table))));

#define STRING256_P(obj) (STRINGP (obj) && XSTRING (obj)->size == 256)

  return (STRING256_P (down)
	  && (NILP (up) || STRING256_P (up))
	  && ((NILP (canon) && NILP (eqv))
 	      || (STRING256_P (canon)
		  && (NILP (eqv) || STRING256_P (eqv))))
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
  Lisp_Object down, up, canon, eqv;
  
  down = current_buffer->downcase_table;
  up = current_buffer->upcase_table;
  canon = current_buffer->case_canon_table;
  eqv = current_buffer->case_eqv_table;

  return Fcons (down, Fcons (up, Fcons (canon, Fcons (eqv, Qnil))));
}

DEFUN ("standard-case-table", Fstandard_case_table, Sstandard_case_table, 0, 0, 0,
  "Return the standard case table.\n\
This is the one used for new buffers.")
  ()
{
  return Fcons (Vascii_downcase_table,
		Fcons (Vascii_upcase_table,
		       Fcons (Vascii_canon_table,
			      Fcons (Vascii_eqv_table, Qnil))));
}

static Lisp_Object set_case_table ();

DEFUN ("set-case-table", Fset_case_table, Sset_case_table, 1, 1, 0,
  "Select a new case table for the current buffer.\n\
A case table is a list (DOWNCASE UPCASE CANONICALIZE EQUIVALENCES)\n\
 where each element is either nil or a string of length 256.\n\
DOWNCASE maps each character to its lower-case equivalent.\n\
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
  Lisp_Object down, up, canon, eqv;

  check_case_table (table);

  down = Fcar_safe (table);
  up = Fcar_safe (Fcdr_safe (table));
  canon = Fcar_safe (Fcdr_safe (Fcdr_safe (table)));
  eqv = Fcar_safe (Fcdr_safe (Fcdr_safe (Fcdr_safe (table))));

  if (NILP (up))
    {
      up = Fmake_string (make_number (256), make_number (0));
      compute_trt_inverse (XSTRING (down)->data, XSTRING (up)->data);
    }

  if (NILP (canon))
    {
      register int i;
      unsigned char *upvec = XSTRING (up)->data;
      unsigned char *downvec = XSTRING (down)->data;

      canon = Fmake_string (make_number (256), make_number (0));

      /* Set up the CANON vector; for each character,
	 this sequence of upcasing and downcasing ought to
	 get the "preferred" lowercase equivalent.  */
      for (i = 0; i < 256; i++)
	XSTRING (canon)->data[i] = downvec[upvec[downvec[i]]];
    }

  if (NILP (eqv))
    {
      eqv = Fmake_string (make_number (256), make_number (0));

      compute_trt_inverse (XSTRING (canon)->data, XSTRING (eqv)->data);
    }

  if (standard)
    {
      Vascii_downcase_table = down;
      Vascii_upcase_table = up;
      Vascii_canon_table = canon;
      Vascii_eqv_table = eqv;
    }
  else
    {
      current_buffer->downcase_table = down;
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

void
compute_trt_inverse (trt, inverse)
     register unsigned char *trt;
     register unsigned char *inverse;
{
  register int i = 0400;
  register unsigned char c, q;

  while (i--)
    inverse[i] = i;
  i = 0400;
  while (i--)
    {
      if ((q = trt[i]) != (unsigned char) i)
	{
	  c = inverse[q];
	  inverse[q] = i;
	  inverse[i] = c;
	}
    }
}

init_casetab_once ()
{
  register int i;
  Lisp_Object tem;

  tem = Fmake_string (make_number (256), make_number (0));
  Vascii_downcase_table = tem;
  Vascii_canon_table = tem;

  for (i = 0; i < 256; i++)
    XSTRING (tem)->data[i] = (i >= 'A' && i <= 'Z') ? i + 040 : i;

  tem = Fmake_string (make_number (256), make_number (0));
  Vascii_upcase_table = tem;
  Vascii_eqv_table = tem;

  for (i = 0; i < 256; i++)
    XSTRING (tem)->data[i]
      = ((i >= 'A' && i <= 'Z')
	 ? i + ('a' - 'A')
	 : ((i >= 'a' && i <= 'z')
	    ? i + ('A' - 'a')
	    : i));
}

syms_of_casetab ()
{
  Qcase_table_p = intern ("case-table-p");
  staticpro (&Qcase_table_p);
  staticpro (&Vascii_downcase_table);
  staticpro (&Vascii_upcase_table);
  staticpro (&Vascii_canon_table);
  staticpro (&Vascii_eqv_table);

  defsubr (&Scase_table_p);
  defsubr (&Scurrent_case_table);
  defsubr (&Sstandard_case_table);
  defsubr (&Sset_case_table);
  defsubr (&Sset_standard_case_table);

#if 0
  DEFVAR_LISP ("ascii-downcase-table", &Vascii_downcase_table,
	       "String mapping ASCII characters to lowercase equivalents.");
  DEFVAR_LISP ("ascii-upcase-table", &Vascii_upcase_table,
	       "String mapping ASCII characters to uppercase equivalents.");
#endif
}
