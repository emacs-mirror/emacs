/* Declarations having to do with Emacs category tables.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
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

#ifndef EMACS_CATEGORY_H
#define EMACS_CATEGORY_H

/* We introduce here three types of object: category, category set,
   and category table.

   A category is like syntax but differs in the following points:

   o A category is represented by a mnemonic character of the range
   ` '(32)..`~'(126) (printable ASCII characters).

   o A category is not exclusive, i.e. a character has multiple
   categories (category set).  Of course, there's a case that a
   category set is empty, i.e. the character has no category.

   o In addition to the predefined categories, a user can define new
   categories.  Total number of categories is limited to 95.

   A category set is a set of categories represented by Lisp
   bool-vector of length 128 (only elements of 31th through 126th
   are used).

   A category table is like syntax-table, represented by a Lisp
   char-table.  The contents are category sets or nil.  It has two
   extra slots, for a vector of doc string of each category and a
   version number.

   The first extra slot is a vector of doc strings of categories, the
   length is 95.  The Nth element corresponding to the category N+32.

   The second extra slot is a version number of the category table.
   But, for the moment, we are not using this slot.  */

#include "lisp.h"

INLINE_HEADER_BEGIN

#define CATEGORYP(x) RANGED_FIXNUMP (0x20, x, 0x7E)

#define CHECK_CATEGORY(x) \
  CHECK_TYPE (CATEGORYP (x), Qcategoryp, x)

#define CATEGORY_SET_P(x) \
  (BOOL_VECTOR_P (x) && bool_vector_size (x) == 128)

/* Return a new empty category set.  */
#define MAKE_CATEGORY_SET (Fmake_bool_vector (make_fixnum (128), Qnil))

#define CHECK_CATEGORY_SET(x) \
  CHECK_TYPE (CATEGORY_SET_P (x), Qcategorysetp, x)

/* Return the category set of character C in the current category table.  */
#define CATEGORY_SET(c) char_category_set (c)

/* Return true if CATEGORY_SET contains CATEGORY.
   Faster than '!NILP (Faref (category_set, make_fixnum (category)))'.  */
INLINE bool
CATEGORY_MEMBER (EMACS_INT category, Lisp_Object category_set)
{
  return bool_vector_bitref (category_set, category);
}

/* Return true if category set of CH contains CATEGORY.  */
INLINE bool
CHAR_HAS_CATEGORY (int ch, int category)
{
  Lisp_Object category_set = CATEGORY_SET (ch);
  return CATEGORY_MEMBER (category, category_set);
}

/* The standard category table is stored where it will automatically
   be used in all new buffers.  */
#define Vstandard_category_table BVAR (&buffer_defaults, category_table)

/* Return the doc string of CATEGORY in category table TABLE.  */
#define CATEGORY_DOCSTRING(table, category)				\
  AREF (Fchar_table_extra_slot (table, make_fixnum (0)), ((category) - ' '))

/* Set the doc string of CATEGORY to VALUE in category table TABLE.  */
#define SET_CATEGORY_DOCSTRING(table, category, value)			\
  ASET (Fchar_table_extra_slot (table, make_fixnum (0)), ((category) - ' '), value)

/* Return the version number of category table TABLE.  Not used for
   the moment.  */
#define CATEGORY_TABLE_VERSION (table) \
  Fchar_table_extra_slot (table, make_fixnum (1))

/* Return true if there is a word boundary between two
   word-constituent characters C1 and C2 if they appear in this order.
   There is no word boundary between two word-constituent ASCII and
   Latin-1 characters.  */
#define WORD_BOUNDARY_P(c1, c2)					\
  (!(SINGLE_BYTE_CHAR_P (c1) && SINGLE_BYTE_CHAR_P (c2))	\
   && word_boundary_p (c1, c2))

extern bool word_boundary_p (int, int);

INLINE_HEADER_END

#endif /* EMACS_CATEGORY_H */
