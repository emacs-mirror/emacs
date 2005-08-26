/* GNU Emacs routines to deal with case tables.
   Copyright (C) 1993, 1994, 2002, 2003, 2004, 
                 2005 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Written by Howard Gayle.  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "character.h"

Lisp_Object Qcase_table_p, Qcase_table;
Lisp_Object Vascii_downcase_table, Vascii_upcase_table;
Lisp_Object Vascii_canon_table, Vascii_eqv_table;

/* Used as a temporary in DOWNCASE and other macros in lisp.h.  No
   need to mark it, since it is used only very temporarily.  */
int case_temp1;
Lisp_Object case_temp2;

static void set_canon ();
static void set_identity ();
static void shuffle ();

DEFUN ("case-table-p", Fcase_table_p, Scase_table_p, 1, 1, 0,
       doc: /* Return t iff OBJECT is a case table.
See `set-case-table' for more information on these data structures.  */)
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
       doc: /* Return the case table of the current buffer.  */)
     ()
{
  return current_buffer->downcase_table;
}

DEFUN ("standard-case-table", Fstandard_case_table, Sstandard_case_table, 0, 0, 0,
       doc: /* Return the standard case table.
This is the one used for new buffers.  */)
     ()
{
  return Vascii_downcase_table;
}

static Lisp_Object set_case_table ();

DEFUN ("set-case-table", Fset_case_table, Sset_case_table, 1, 1, 0,
       doc: /* Select a new case table for the current buffer.
A case table is a char-table which maps characters
to their lower-case equivalents.  It also has three \"extra\" slots
which may be additional char-tables or nil.
These slots are called UPCASE, CANONICALIZE and EQUIVALENCES.
UPCASE maps each character to its upper-case equivalent;
 if lower and upper case characters are in 1-1 correspondence,
 you may use nil and the upcase table will be deduced from DOWNCASE.
CANONICALIZE maps each character to a canonical equivalent;
 any two characters that are related by case-conversion have the same
 canonical equivalent character; it may be nil, in which case it is
 deduced from DOWNCASE and UPCASE.
EQUIVALENCES is a map that cyclicly permutes each equivalence class
 (of characters with the same canonical equivalent); it may be nil,
 in which case it is deduced from CANONICALIZE.  */)
     (table)
     Lisp_Object table;
{
  return set_case_table (table, 0);
}

DEFUN ("set-standard-case-table", Fset_standard_case_table, Sset_standard_case_table, 1, 1, 0,
       doc: /* Select a new standard case table for new buffers.
See `set-case-table' for more info on case tables.  */)
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
      map_char_table (set_identity, Qnil, table, up);
      map_char_table (shuffle, Qnil, table, up);
      XCHAR_TABLE (table)->extras[0] = up;
    }

  if (NILP (canon))
    {
      canon = Fmake_char_table (Qcase_table, Qnil);
      XCHAR_TABLE (table)->extras[1] = canon;
      map_char_table (set_canon, Qnil, table, table);
    }

  if (NILP (eqv))
    {
      eqv = Fmake_char_table (Qcase_table, Qnil);
      map_char_table (set_identity, Qnil, canon, eqv);
      map_char_table (shuffle, Qnil, canon, eqv);
      XCHAR_TABLE (table)->extras[2] = eqv;
    }

  /* This is so set_image_of_range_1 in regex.c can find the EQV table.  */
  XCHAR_TABLE (canon)->extras[2] = eqv;

  if (standard)
    {
      Vascii_downcase_table = table;
      Vascii_upcase_table = up;
      Vascii_canon_table = canon;
      Vascii_eqv_table = eqv;
    }
  else
    {
      current_buffer->downcase_table = table;
      current_buffer->upcase_table = up;
      current_buffer->case_canon_table = canon;
      current_buffer->case_eqv_table = eqv;
    }

  return table;
}

/* The following functions are called in map_char_table.  */

/* Set CANON char-table element for characters in RANGE to a
   translated ELT by UP and DOWN char-tables.  This is done only when
   ELT is a character.  The char-tables CANON, UP, and DOWN are in
   CASE_TABLE.  */

static void
set_canon (case_table, range, elt)
     Lisp_Object case_table, range, elt;
{
  Lisp_Object up = XCHAR_TABLE (case_table)->extras[0];
  Lisp_Object canon = XCHAR_TABLE (case_table)->extras[1];

  if (NATNUMP (elt))
    Fset_char_table_range (canon, range, Faref (case_table, Faref (up, elt)));
}

/* Set elements of char-table TABLE for C to C itself.  C may be a
   cons specifying a character range.  In that case, set characters in
   that range to themselves.  This is done only when ELT is a
   character.  This is called in map_char_table.  */

static void
set_identity (table, c, elt)
     Lisp_Object table, c, elt;
{
  if (NATNUMP (elt))
    {
      int from, to;

      if (CONSP (c))
	{
	  from = XINT (XCAR (c));
	  to = XINT (XCDR (c));
	}
      else
	from = to = XINT (c);
      for (; from <= to; from++)
	CHAR_TABLE_SET (table, from, make_number (from));
    }
}

/* Permute the elements of TABLE (which is initially an identity
   mapping) so that it has one cycle for each equivalence class
   induced by the translation table on which map_char_table is
   operated.  */

static void
shuffle (table, c, elt)
     Lisp_Object table, c, elt;
{
  if (NATNUMP (elt))
    {
      Lisp_Object tem = Faref (table, elt);
      int from, to;

      if (CONSP (c))
	{
	  from = XINT (XCAR (c));
	  to = XINT (XCDR (c));
	}
      else
	from = to = XINT (c);

      for (; from <= to; from++)
	if (from != XINT (elt))
	  {
	    Faset (table, elt, make_number (from));
	    Faset (table, make_number (from), tem);
	  }
    }
}

void
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

  for (i = 0; i < 128; i++)
    {
      int c = (i >= 'A' && i <= 'Z') ? i + ('a' - 'A') : i;
      CHAR_TABLE_SET (down, i, make_number (c));
    }

  XCHAR_TABLE (down)->extras[1] = Fcopy_sequence (down);

  up = Fmake_char_table (Qcase_table, Qnil);
  XCHAR_TABLE (down)->extras[0] = up;

  for (i = 0; i < 128; i++)
    {
      int c = ((i >= 'A' && i <= 'Z') ? i + ('a' - 'A')
	       : ((i >= 'a' && i <= 'z') ? i + ('A' - 'a')
		  : i));;
      CHAR_TABLE_SET (up, i, make_number (c));
    }

  XCHAR_TABLE (down)->extras[2] = Fcopy_sequence (up);
}

void
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

/* arch-tag: e06388ad-99fe-40ec-ba67-9d010fcc4916
   (do not change this comment) */
