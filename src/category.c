/* GNU Emacs routines to deal with category tables.
   Copyright (C) 1995, 1997 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.
   Copyright (C) 2001, 2002
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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


/* Here we handle three objects: category, category set, and category
   table.  Read comments in the file category.h to understand them.  */

#include <config.h>
#include <ctype.h>
#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "category.h"
#include "keymap.h"

/* The version number of the latest category table.  Each category
   table has a unique version number.  It is assigned a new number
   also when it is modified.  When a regular expression is compiled
   into the struct re_pattern_buffer, the version number of the
   category table (of the current buffer) at that moment is also
   embedded in the structure.

   For the moment, we are not using this feature.  */
static int category_table_version;

Lisp_Object Qcategory_table, Qcategoryp, Qcategorysetp, Qcategory_table_p;

/* Variables to determine word boundary.  */
Lisp_Object Vword_combining_categories, Vword_separating_categories;

/* Temporary internal variable used in macro CHAR_HAS_CATEGORY.  */
Lisp_Object _temp_category_set;


/* Category set staff.  */

DEFUN ("make-category-set", Fmake_category_set, Smake_category_set, 1, 1, 0,
       doc: /* Return a newly created category-set which contains CATEGORIES.
CATEGORIES is a string of category mnemonics.
The value is a bool-vector which has t at the indices corresponding to
those categories.  */)
     (categories)
     Lisp_Object categories;
{
  Lisp_Object val;
  int len;

  CHECK_STRING (categories);
  val = MAKE_CATEGORY_SET;

  if (STRING_MULTIBYTE (categories))
    error ("Multibyte string in make-category-set");

  len = XSTRING (categories)->size;
  while (--len >= 0)
    {
      Lisp_Object category;

      XSETFASTINT (category, XSTRING (categories)->data[len]);
      CHECK_CATEGORY (category);
      SET_CATEGORY_SET (val, category, Qt);
    }
  return val;
}


/* Category staff.  */

Lisp_Object check_category_table ();

DEFUN ("define-category", Fdefine_category, Sdefine_category, 2, 3, 0,
       doc: /* Define CHAR as a category which is described by DOCSTRING.
CHAR should be an ASCII printing character in the range ` ' to `~'.
DOCSTRING is a documentation string of the category.
The category is defined only in category table TABLE, which defaults to
 the current buffer's category table.  */)
     (category, docstring, table)
     Lisp_Object category, docstring, table;
{
  CHECK_CATEGORY (category);
  CHECK_STRING (docstring);
  table = check_category_table (table);

  if (!NILP (CATEGORY_DOCSTRING (table, XFASTINT (category))))
    error ("Category `%c' is already defined", XFASTINT (category));
  CATEGORY_DOCSTRING (table, XFASTINT (category)) = docstring;

  return Qnil;
}

DEFUN ("category-docstring", Fcategory_docstring, Scategory_docstring, 1, 2, 0,
       doc: /* Return the documentation string of CATEGORY, as defined in CATEGORY-TABLE.  */)
     (category, table)
     Lisp_Object category, table;
{
  CHECK_CATEGORY (category);
  table = check_category_table (table);

  return CATEGORY_DOCSTRING (table, XFASTINT (category));
}

DEFUN ("get-unused-category", Fget_unused_category, Sget_unused_category,
       0, 1, 0,
       doc: /* Return a category which is not yet defined in CATEGORY-TABLE.
If no category remains available, return nil.
The optional argument CATEGORY-TABLE
specifies which category table to modify;
it defaults to the current buffer's category table.  */)
     (table)
     Lisp_Object table;
{
  int i;

  table = check_category_table (table);

  for (i = ' '; i <= '~'; i++)
    if (NILP (CATEGORY_DOCSTRING (table, i)))
      return make_number (i);

  return Qnil;
}


/* Category-table staff.  */

DEFUN ("category-table-p", Fcategory_table_p, Scategory_table_p, 1, 1, 0,
       doc: /* Return t if ARG is a category table.  */)
     (arg)
     Lisp_Object arg;
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

Lisp_Object
check_category_table (table)
     Lisp_Object table;
{
  register Lisp_Object tem;
  if (NILP (table))
    return current_buffer->category_table;
  while (tem = Fcategory_table_p (table), NILP (tem))
    table = wrong_type_argument (Qcategory_table_p, table);
  return table;
}   

DEFUN ("category-table", Fcategory_table, Scategory_table, 0, 0, 0,
       doc: /* Return the current category table.
This is the one specified by the current buffer.  */)
     ()
{
  return current_buffer->category_table;
}

DEFUN ("standard-category-table", Fstandard_category_table,
   Sstandard_category_table, 0, 0, 0,
       doc: /* Return the standard category table.
This is the one used for new buffers.  */)
     ()
{
  return Vstandard_category_table;
}


static void
copy_category_entry (table, range, val)
     Lisp_Object table, range, val;
{
  char_table_set_range (table, XINT (XCAR (range)), XINT (XCDR (range)),
			Fcopy_sequence (val));
}

/* Return a copy of category table TABLE.  We can't simply use the
   function copy-sequence because no contents should be shared between
   the original and the copy.  This function is called recursively by
   binding TABLE to a sub char table.  */

Lisp_Object
copy_category_table (table)
     Lisp_Object table;
{
  table = copy_char_table (table);

  if (! NILP (XCHAR_TABLE (table)->defalt))
    XCHAR_TABLE (table)->defalt
      = Fcopy_sequence (XCHAR_TABLE (table)->defalt);
  XCHAR_TABLE (table)->extras[0]
    = Fcopy_sequence (XCHAR_TABLE (table)->extras[0]);

  map_char_table (copy_category_entry, Qnil, table, table, 0, NULL);

  return table;
}

DEFUN ("copy-category-table", Fcopy_category_table, Scopy_category_table,
       0, 1, 0,
       doc: /* Construct a new category table and return it.
It is a copy of the TABLE, which defaults to the standard category table.  */)
     (table)
     Lisp_Object table;
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
     ()
{
  Lisp_Object val;
  int i;

  val = Fmake_char_table (Qcategory_table, Qnil);
  XCHAR_TABLE (val)->defalt = MAKE_CATEGORY_SET;
  for (i = 0; i < (1 << CHARTAB_SIZE_BITS_0); i++)  
    XCHAR_TABLE (val)->contents[i] = MAKE_CATEGORY_SET;
  Fset_char_table_extra_slot (val, make_number (0),
			      Fmake_vector (make_number (95), Qnil));
  return val;
}

DEFUN ("set-category-table", Fset_category_table, Sset_category_table, 1, 1, 0,
       doc: /* Specify TABLE as the category table for the current buffer.  */)
     (table)
     Lisp_Object table;
{
  int idx;
  table = check_category_table (table);
  current_buffer->category_table = table;
  /* Indicate that this buffer now has a specified category table.  */
  idx = PER_BUFFER_VAR_IDX (category_table);
  SET_PER_BUFFER_VALUE_P (current_buffer, idx, 1);
  return table;
}


Lisp_Object
char_category_set (c)
     int c;
{
  return CHAR_TABLE_REF (current_buffer->category_table, c);
}

DEFUN ("char-category-set", Fchar_category_set, Schar_category_set, 1, 1, 0,
       doc: /* Return the category set of CHAR.  */)
     (ch)
     Lisp_Object ch;
{
  CHECK_NUMBER (ch);
  return CATEGORY_SET (XFASTINT (ch));
}

DEFUN ("category-set-mnemonics", Fcategory_set_mnemonics,
       Scategory_set_mnemonics, 1, 1, 0,
       doc: /* Return a string containing mnemonics of the categories in CATEGORY-SET.
CATEGORY-SET is a bool-vector, and the categories \"in\" it are those
that are indexes where t occurs the bool-vector.
The return value is a string containing those same categories.  */)
     (category_set)
     Lisp_Object category_set;
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

void
set_category_set (category_set, category, val)
     Lisp_Object category_set, category, val;
{
  do {
    int idx = XINT (category) / 8;
    unsigned char bits = 1 << (XINT (category) % 8);

    if (NILP (val))
      XCATEGORY_SET (category_set)->data[idx] &= ~bits;
    else
      XCATEGORY_SET (category_set)->data[idx] |= bits;
  } while (0);
}

DEFUN ("modify-category-entry", Fmodify_category_entry,
       Smodify_category_entry, 2, 4, 0,
       doc: /* Modify the category set of CHARACTER by adding CATEGORY to it.
The category is changed only for table TABLE, which defaults to
the current buffer's category table.
CHARACTER can be either a single character or a cons representing the
lower and upper ends of an inclusive character range to modify.
If optional fourth argument RESET is non-nil,
then delete CATEGORY from the category set instead of adding it.  */)
     (character, category, table, reset)
     Lisp_Object character, category, table, reset;
{
  Lisp_Object set_value;	/* Actual value to be set in category sets.  */
  Lisp_Object category_set;
  int start, end;
  int from, to;

  if (INTEGERP (character))
    {
      CHECK_CHARACTER (character);
      start = end = XFASTINT (character);
    }
  else
    {
      CHECK_CONS (character);
      CHECK_CHARACTER (XCAR (character));
      CHECK_CHARACTER (XCDR (character));
      start = XFASTINT (XCAR (character));
      end = XFASTINT (XCDR (character));
    }

  CHECK_CATEGORY (category);
  table = check_category_table (table);

  if (NILP (CATEGORY_DOCSTRING (table, XFASTINT (category))))
    error ("Undefined category: %c", XFASTINT (category));
  
  set_value = NILP (reset) ? Qt : Qnil;

  while (start <= end)
    {
      category_set = char_table_ref_and_range (table, start, &from, &to);
      if (CATEGORY_MEMBER (category, category_set) != NILP (reset))
	{
	  category_set = Fcopy_sequence (category_set);
	  SET_CATEGORY_SET (category_set, category, set_value);
	  if (to > end)
	    char_table_set_range (table, start, end, category_set);
	  else
	    char_table_set_range (table, start, to, category_set);
	}
      start = to + 1;
    }
  return Qnil;
}

/* Return 1 if there is a word boundary between two word-constituent
   characters C1 and C2 if they appear in this order, else return 0.
   Use the macro WORD_BOUNDARY_P instead of calling this function
   directly.  */

int
word_boundary_p (c1, c2)
     int c1, c2;
{
  Lisp_Object category_set1, category_set2;
  Lisp_Object tail;
  int default_result;

  if (CHAR_CHARSET (c1) == CHAR_CHARSET (c2))
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
	  && CATEGORYP (XCAR (elt))
	  && CATEGORYP (XCDR (elt))
	  && CATEGORY_MEMBER (XFASTINT (XCAR (elt)), category_set1)
	  && CATEGORY_MEMBER (XFASTINT (XCDR (elt)), category_set2))
	return !default_result;
    }
  return default_result;
}


void
init_category_once ()
{
  /* This has to be done here, before we call Fmake_char_table.  */
  Qcategory_table = intern ("category-table");
  staticpro (&Qcategory_table);

  /* Intern this now in case it isn't already done.
     Setting this variable twice is harmless.
     But don't staticpro it here--that is done in alloc.c.  */
  Qchar_table_extra_slots = intern ("char-table-extra-slots");

  /* Now we are ready to set up this property, so we can
     create category tables.  */
  Fput (Qcategory_table, Qchar_table_extra_slots, make_number (2));

  Vstandard_category_table = Fmake_char_table (Qcategory_table, Qnil);
  /* Set a category set which contains nothing to the default.  */ 
  XCHAR_TABLE (Vstandard_category_table)->defalt = MAKE_CATEGORY_SET;
  Fset_char_table_extra_slot (Vstandard_category_table, make_number (0),
			      Fmake_vector (make_number (95), Qnil));
}

void
syms_of_category ()
{
  Qcategoryp = intern ("categoryp");
  staticpro (&Qcategoryp);
  Qcategorysetp = intern ("categorysetp");
  staticpro (&Qcategorysetp);
  Qcategory_table_p = intern ("category-table-p");
  staticpro (&Qcategory_table_p);

  DEFVAR_LISP ("word-combining-categories", &Vword_combining_categories,
	       doc: /* List of pair (cons) of categories to determine word boundary.

Emacs treats a sequence of word constituent characters as a single
word (i.e. finds no word boundary between them) iff they belongs to
the same charset.  But, exceptions are allowed in the following cases.

\(1) The case that characters are in different charsets is controlled
by the variable `word-combining-categories'.

Emacs finds no word boundary between characters of different charsets
if they have categories matching some element of this list.

More precisely, if an element of this list is a cons of category CAT1
and CAT2, and a multibyte character C1 which has CAT1 is followed by
C2 which has CAT2, there's no word boundary between C1 and C2.

For instance, to tell that ASCII characters and Latin-1 characters can
form a single word, the element `(?l . ?l)' should be in this list
because both characters have the category `l' (Latin characters).

\(2) The case that character are in the same charset is controlled by
the variable `word-separating-categories'.

Emacs find a word boundary between characters of the same charset
if they have categories matching some element of this list.

More precisely, if an element of this list is a cons of category CAT1
and CAT2, and a multibyte character C1 which has CAT1 is followed by
C2 which has CAT2, there's a word boundary between C1 and C2.

For instance, to tell that there's a word boundary between Japanese
Hiragana and Japanese Kanji (both are in the same charset), the
element `(?H . ?C) should be in this list.  */);

  Vword_combining_categories = Qnil;

  DEFVAR_LISP ("word-separating-categories", &Vword_separating_categories,
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

  category_table_version = 0;
}
