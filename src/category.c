/* GNU Emacs routines to deal with category tables.

Copyright (C) 1998, 2001-2020 Free Software Foundation, Inc.
Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
  2005, 2006, 2007, 2008, 2009, 2010, 2011
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H14PRO021
Copyright (C) 2003
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H13PRO009

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


/* Here we handle three objects: category, category set, and category
   table.  Read comments in the file category.h to understand them.  */

#include <config.h>

#include "lisp.h"
#include "character.h"
#include "buffer.h"
#include "category.h"

/* This setter is used only in this file, so it can be private.  */
static void
bset_category_table (struct buffer *b, Lisp_Object val)
{
  b->category_table_ = val;
}


/* Category set staff.  */

static Lisp_Object
hash_get_category_set (Lisp_Object table, Lisp_Object category_set)
{
  if (NILP (XCHAR_TABLE (table)->extras[1]))
    set_char_table_extras
      (table, 1,
       make_hash_table (hashtest_equal, DEFAULT_HASH_SIZE,
			DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
			Qnil, false));
  struct Lisp_Hash_Table *h = XHASH_TABLE (XCHAR_TABLE (table)->extras[1]);
  Lisp_Object hash;
  ptrdiff_t i = hash_lookup (h, category_set, &hash);
  if (i >= 0)
    return HASH_KEY (h, i);
  hash_put (h, category_set, Qnil, hash);
  return category_set;
}

/* Make CATEGORY_SET include (if VAL) or exclude (if !VAL) CATEGORY.  */

static void
set_category_set (Lisp_Object category_set, EMACS_INT category, bool val)
{
  bool_vector_set (category_set, category, val);
}

DEFUN ("make-category-set", Fmake_category_set, Smake_category_set, 1, 1, 0,
       doc: /* Return a newly created category-set which contains CATEGORIES.
CATEGORIES is a string of category mnemonics.
The value is a bool-vector which has t at the indices corresponding to
those categories.  */)
  (Lisp_Object categories)
{
  Lisp_Object val;
  ptrdiff_t len;

  CHECK_STRING (categories);
  val = MAKE_CATEGORY_SET;

  if (STRING_MULTIBYTE (categories))
    error ("Multibyte string in `make-category-set'");

  len = SCHARS (categories);
  while (--len >= 0)
    {
      unsigned char cat = SREF (categories, len);
      Lisp_Object category = make_fixnum (cat);

      CHECK_CATEGORY (category);
      set_category_set (val, cat, 1);
    }
  return val;
}


/* Category staff.  */

static Lisp_Object check_category_table (Lisp_Object table);

DEFUN ("define-category", Fdefine_category, Sdefine_category, 2, 3, 0,
       doc: /* Define CATEGORY as a category which is described by DOCSTRING.
CATEGORY should be an ASCII printing character in the range ` ' to `~'.
DOCSTRING is the documentation string of the category.  The first line
should be a terse text (preferably less than 16 characters),
and the rest lines should be the full description.
The category is defined only in category table TABLE, which defaults to
the current buffer's category table.  */)
  (Lisp_Object category, Lisp_Object docstring, Lisp_Object table)
{
  CHECK_CATEGORY (category);
  CHECK_STRING (docstring);
  table = check_category_table (table);

  if (!NILP (CATEGORY_DOCSTRING (table, XFIXNAT (category))))
    error ("Category `%c' is already defined", (int) XFIXNAT (category));
  if (!NILP (Vpurify_flag))
    docstring = Fpurecopy (docstring);
  SET_CATEGORY_DOCSTRING (table, XFIXNAT (category), docstring);

  return Qnil;
}

DEFUN ("category-docstring", Fcategory_docstring, Scategory_docstring, 1, 2, 0,
       doc: /* Return the documentation string of CATEGORY, as defined in TABLE.
TABLE should be a category table and defaults to the current buffer's
category table.  */)
  (Lisp_Object category, Lisp_Object table)
{
  CHECK_CATEGORY (category);
  table = check_category_table (table);

  return CATEGORY_DOCSTRING (table, XFIXNAT (category));
}

DEFUN ("get-unused-category", Fget_unused_category, Sget_unused_category,
       0, 1, 0,
       doc: /* Return a category which is not yet defined in TABLE.
If no category remains available, return nil.
The optional argument TABLE specifies which category table to modify;
it defaults to the current buffer's category table.  */)
  (Lisp_Object table)
{
  int i;

  table = check_category_table (table);

  for (i = ' '; i <= '~'; i++)
    if (NILP (CATEGORY_DOCSTRING (table, i)))
      return make_fixnum (i);

  return Qnil;
}


/* Category-table staff.  */

DEFUN ("category-table-p", Fcategory_table_p, Scategory_table_p, 1, 1, 0,
       doc: /* Return t if ARG is a category table.  */)
  (Lisp_Object arg)
{
  if (CHAR_TABLE_P (arg)
      && EQ (XCHAR_TABLE (arg)->purpose, Qcategory_table))
    return Qt;
  return Qnil;
}

/* If TABLE is nil, return the current category table.  If TABLE is
   not nil, check the validity of TABLE as a category table.  If
   valid, return TABLE itself, but if not valid, signal an error of
   wrong-type-argument.  */

static Lisp_Object
check_category_table (Lisp_Object table)
{
  if (NILP (table))
    return BVAR (current_buffer, category_table);
  CHECK_TYPE (!NILP (Fcategory_table_p (table)), Qcategory_table_p, table);
  return table;
}

DEFUN ("category-table", Fcategory_table, Scategory_table, 0, 0, 0,
       doc: /* Return the current category table.
This is the one specified by the current buffer.  */)
  (void)
{
  return BVAR (current_buffer, category_table);
}

DEFUN ("standard-category-table", Fstandard_category_table,
   Sstandard_category_table, 0, 0, 0,
       doc: /* Return the standard category table.
This is the one used for new buffers.  */)
  (void)
{
  return Vstandard_category_table;
}


static void
copy_category_entry (Lisp_Object table, Lisp_Object c, Lisp_Object val)
{
  val = Fcopy_sequence (val);
  if (CONSP (c))
    char_table_set_range (table, XFIXNUM (XCAR (c)), XFIXNUM (XCDR (c)), val);
  else
    char_table_set (table, XFIXNUM (c), val);
}

/* Return a copy of category table TABLE.  We can't simply use the
   function copy-sequence because no contents should be shared between
   the original and the copy.  This function is called recursively by
   binding TABLE to a sub char table.  */

static Lisp_Object
copy_category_table (Lisp_Object table)
{
  table = copy_char_table (table);

  if (! NILP (XCHAR_TABLE (table)->defalt))
    set_char_table_defalt (table,
			   Fcopy_sequence (XCHAR_TABLE (table)->defalt));
  set_char_table_extras
    (table, 0, Fcopy_sequence (XCHAR_TABLE (table)->extras[0]));
  map_char_table (copy_category_entry, Qnil, table, table);

  return table;
}

DEFUN ("copy-category-table", Fcopy_category_table, Scopy_category_table,
       0, 1, 0,
       doc: /* Construct a new category table and return it.
It is a copy of the TABLE, which defaults to the standard category table.  */)
  (Lisp_Object table)
{
  if (!NILP (table))
    check_category_table (table);
  else
    table = Vstandard_category_table;

  return copy_category_table (table);
}

DEFUN ("make-category-table", Fmake_category_table, Smake_category_table,
       0, 0, 0,
       doc: /* Construct a new and empty category table and return it.  */)
  (void)
{
  Lisp_Object val;
  int i;

  val = Fmake_char_table (Qcategory_table, Qnil);
  set_char_table_defalt (val, MAKE_CATEGORY_SET);
  for (i = 0; i < (1 << CHARTAB_SIZE_BITS_0); i++)
    set_char_table_contents (val, i, MAKE_CATEGORY_SET);
  Fset_char_table_extra_slot (val, make_fixnum (0), make_nil_vector (95));
  return val;
}

DEFUN ("set-category-table", Fset_category_table, Sset_category_table, 1, 1, 0,
       doc: /* Specify TABLE as the category table for the current buffer.
Return TABLE.  */)
  (Lisp_Object table)
{
  int idx;
  table = check_category_table (table);
  bset_category_table (current_buffer, table);
  /* Indicate that this buffer now has a specified category table.  */
  idx = PER_BUFFER_VAR_IDX (category_table);
  SET_PER_BUFFER_VALUE_P (current_buffer, idx, 1);
  return table;
}


Lisp_Object
char_category_set (int c)
{
  return CHAR_TABLE_REF (BVAR (current_buffer, category_table), c);
}

DEFUN ("char-category-set", Fchar_category_set, Schar_category_set, 1, 1, 0,
       doc: /* Return the category set of CHAR.
usage: (char-category-set CHAR)  */)
  (Lisp_Object ch)
{
  CHECK_CHARACTER (ch);
  return CATEGORY_SET (XFIXNAT (ch));
}

DEFUN ("category-set-mnemonics", Fcategory_set_mnemonics,
       Scategory_set_mnemonics, 1, 1, 0,
       doc: /* Return a string containing mnemonics of the categories in CATEGORY-SET.
CATEGORY-SET is a bool-vector, and the categories \"in\" it are those
that are indexes where t occurs in the bool-vector.
The return value is a string containing those same categories.  */)
  (Lisp_Object category_set)
{
  int i, j;
  char str[96];

  CHECK_CATEGORY_SET (category_set);

  j = 0;
  for (i = 32; i < 127; i++)
    if (CATEGORY_MEMBER (i, category_set))
      str[j++] = i;
  str[j] = '\0';

  return build_string (str);
}

DEFUN ("modify-category-entry", Fmodify_category_entry,
       Smodify_category_entry, 2, 4, 0,
       doc: /* Modify the category set of CHARACTER by adding CATEGORY to it.
The category is changed only for table TABLE, which defaults to
the current buffer's category table.
CHARACTER can be either a single character or a cons representing the
lower and upper ends of an inclusive character range to modify.
CATEGORY must be a category name (a character between ` ' and `~').
Use `describe-categories' to see existing category names.
If optional fourth argument RESET is non-nil,
then delete CATEGORY from the category set instead of adding it.  */)
  (Lisp_Object character, Lisp_Object category, Lisp_Object table, Lisp_Object reset)
{
  bool set_value;	/* Actual value to be set in category sets.  */
  Lisp_Object category_set;
  int start, end;
  int from, to;

  if (FIXNUMP (character))
    {
      CHECK_CHARACTER (character);
      start = end = XFIXNAT (character);
    }
  else
    {
      CHECK_CONS (character);
      CHECK_CHARACTER_CAR (character);
      CHECK_CHARACTER_CDR (character);
      start = XFIXNAT (XCAR (character));
      end = XFIXNAT (XCDR (character));
    }

  CHECK_CATEGORY (category);
  table = check_category_table (table);

  if (NILP (CATEGORY_DOCSTRING (table, XFIXNAT (category))))
    error ("Undefined category: %c", (int) XFIXNAT (category));

  set_value = NILP (reset);

  while (start <= end)
    {
      from = start, to = end;
      category_set = char_table_ref_and_range (table, start, &from, &to);
      if (CATEGORY_MEMBER (XFIXNAT (category), category_set) != NILP (reset))
	{
	  category_set = Fcopy_sequence (category_set);
	  set_category_set (category_set, XFIXNAT (category), set_value);
	  category_set = hash_get_category_set (table, category_set);
	  char_table_set_range (table, start, to, category_set);
	}
      start = to + 1;
    }

  return Qnil;
}

/* Return true if there is a word boundary between two word-constituent
   characters C1 and C2 if they appear in this order.
   Use the macro WORD_BOUNDARY_P instead of calling this function
   directly.  */

bool
word_boundary_p (int c1, int c2)
{
  Lisp_Object category_set1, category_set2;
  Lisp_Object tail;
  bool default_result;

  if (EQ (CHAR_TABLE_REF (Vchar_script_table, c1),
	  CHAR_TABLE_REF (Vchar_script_table, c2)))
    {
      tail = Vword_separating_categories;
      default_result = 0;
    }
  else
    {
      tail = Vword_combining_categories;
      default_result = 1;
    }

  category_set1 = CATEGORY_SET (c1);
  if (NILP (category_set1))
    return default_result;
  category_set2 = CATEGORY_SET (c2);
  if (NILP (category_set2))
    return default_result;

  for (; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt = XCAR (tail);

      if (CONSP (elt)
	  && (NILP (XCAR (elt))
	      || (CATEGORYP (XCAR (elt))
		  && CATEGORY_MEMBER (XFIXNAT (XCAR (elt)), category_set1)
		  && ! CATEGORY_MEMBER (XFIXNAT (XCAR (elt)), category_set2)))
	  && (NILP (XCDR (elt))
	      || (CATEGORYP (XCDR (elt))
		  && ! CATEGORY_MEMBER (XFIXNAT (XCDR (elt)), category_set1)
		  && CATEGORY_MEMBER (XFIXNAT (XCDR (elt)), category_set2))))
	return !default_result;
    }
  return default_result;
}


void
init_category_once (void)
{
  /* This has to be done here, before we call Fmake_char_table.  */
  DEFSYM (Qcategory_table, "category-table");
  Fput (Qcategory_table, Qchar_table_extra_slots, make_fixnum (2));

  Vstandard_category_table = Fmake_char_table (Qcategory_table, Qnil);
  /* Set a category set which contains nothing to the default.  */
  set_char_table_defalt (Vstandard_category_table, MAKE_CATEGORY_SET);
  Fset_char_table_extra_slot (Vstandard_category_table, make_fixnum (0),
			      make_nil_vector (95));
}

void
syms_of_category (void)
{
  DEFSYM (Qcategoryp, "categoryp");
  DEFSYM (Qcategorysetp, "categorysetp");
  DEFSYM (Qcategory_table_p, "category-table-p");

  DEFVAR_LISP ("word-combining-categories", Vword_combining_categories,
	       doc: /* List of pair (cons) of categories to determine word boundary.

Emacs treats a sequence of word constituent characters as a single
word (i.e. finds no word boundary between them) only if they belong to
the same script.  But, exceptions are allowed in the following cases.

\(1) The case that characters are in different scripts is controlled
by the variable `word-combining-categories'.

Emacs finds no word boundary between characters of different scripts
if they have categories matching some element of this list.

More precisely, if an element of this list is a cons of category CAT1
and CAT2, and a multibyte character C1 which has CAT1 is followed by
C2 which has CAT2, there's no word boundary between C1 and C2.

For instance, to tell that Han characters followed by Hiragana
characters can form a single word, the element `(?C . ?H)' should be
in this list.

\(2) The case that character are in the same script is controlled by
the variable `word-separating-categories'.

Emacs finds a word boundary between characters of the same script
if they have categories matching some element of this list.

More precisely, if an element of this list is a cons of category CAT1
and CAT2, and a multibyte character C1 which has CAT1 but not CAT2 is
followed by C2 which has CAT2 but not CAT1, there's a word boundary
between C1 and C2.

For instance, to tell that there's a word boundary between Hiragana
and Katakana (both are in the same script `kana'),
the element `(?H . ?K)' should be in this list.  */);

  Vword_combining_categories = Qnil;

  DEFVAR_LISP ("word-separating-categories", Vword_separating_categories,
	       doc: /* List of pair (cons) of categories to determine word boundary.
See the documentation of the variable `word-combining-categories'.  */);

  Vword_separating_categories = Qnil;

  defsubr (&Smake_category_set);
  defsubr (&Sdefine_category);
  defsubr (&Scategory_docstring);
  defsubr (&Sget_unused_category);
  defsubr (&Scategory_table_p);
  defsubr (&Scategory_table);
  defsubr (&Sstandard_category_table);
  defsubr (&Scopy_category_table);
  defsubr (&Smake_category_table);
  defsubr (&Sset_category_table);
  defsubr (&Schar_category_set);
  defsubr (&Scategory_set_mnemonics);
  defsubr (&Smodify_category_entry);
}
