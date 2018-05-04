/* Fontset handler.

Copyright (C) 2001-2018 Free Software Foundation, Inc.
Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
  2005, 2006, 2007, 2008, 2009, 2010, 2011
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H14PRO021
Copyright (C) 2003, 2006
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

#include <config.h>
#include <stdio.h>
#include <stdlib.h>

#include "lisp.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */
#include "font.h"

/* FONTSET

   A fontset is a collection of font related information to give
   similar appearance (style, etc) of characters.  A fontset has two
   roles.  One is to use for the frame parameter `font' as if it is an
   ASCII font.  In that case, Emacs uses the font specified for
   `ascii' script for the frame's default font.

   Another role, the more important one, is to provide information
   about which font to use for each non-ASCII character.

   There are two kinds of fontsets; base and realized.  A base fontset
   is created by `new-fontset' from Emacs Lisp explicitly.  A realized
   fontset is created implicitly when a face is realized for ASCII
   characters.  A face is also realized for non-ASCII characters based
   on an ASCII face.  All of non-ASCII faces based on the same ASCII
   face share the same realized fontset.

   A fontset object is implemented by a char-table whose default value
   and parent are always nil.

   An element of a base fontset is a vector of FONT-DEFs which themselves
   are vectors of the form [ FONT-SPEC ENCODING REPERTORY ].

   An element of a realized fontset is nil, t, 0, or a cons that has
   this from:

	(CHARSET-ORDERED-LIST-TICK . FONT-GROUP)

   CHARSET_ORDERED_LIST_TICK is the same as charset_ordered_list_tick or -1.

   FONT-GROUP is a vector of elements that have this form:

	[ RFONT-DEF0 RFONT-DEF1 ... ]

   Each RFONT-DEFn (i.e. Realized FONT-DEF) has this form:

	[ FACE-ID FONT-DEF FONT-OBJECT SORTING-SCORE ]

   RFONT-DEFn are automatically reordered considering the current
   charset priority list, the current language environment, and
   priorities determined by font-backends.

   RFONT-DEFn may not be a vector in the following cases.

   The value nil means that we have not yet generated the above vector
   from the base of the fontset.

   The value t means that no font is available for the corresponding
   range of characters.

   The value 0 means that no font is available for the corresponding
   range of characters in this fontset, but may be available in the
   fallback font-group or in the default fontset.

   A fontset has 8 extra slots.

   The 1st slot:
	base: the ID number of the fontset
	realized: Likewise

   The 2nd slot:
	base: the name of the fontset
	realized: nil

   The 3rd slot:
	base: the font name for ASCII characters
	realized: nil

   The 4th slot:
	base: nil
	realized: the base fontset

   The 5th slot:
	base: nil
	realized: the frame that the fontset belongs to

   The 6th slot:
	base: nil
	realized: the ID number of a face to use for characters that
		  has no font in a realized fontset.

   The 7th slot:
	base: nil
	realized: If the base is not the default fontset, a fontset
		  realized from the default fontset, else nil.

   The 8th slot:
	base: Same as element value (but for fallback fonts).
	realized: Likewise.

   All fontsets are recorded in the vector Vfontset_table.


   DEFAULT FONTSET

   There's a special base fontset named `default fontset' which
   defines the default font specifications.  When a base fontset
   doesn't specify a font for a specific character, the corresponding
   value in the default fontset is used.

   The parent of a realized fontset created for such a face that has
   no fontset is the default fontset.


   These structures are hidden from the other codes than this file.
   The other codes handle fontsets only by their ID numbers.  They
   usually use the variable name `fontset' for IDs.  But, in this
   file, we always use variable name `id' for IDs, and name `fontset'
   for an actual fontset object, i.e., char-table.

*/

/********** VARIABLES and FUNCTION PROTOTYPES **********/

/* Vector containing all fontsets.  */
static Lisp_Object Vfontset_table;

/* Next possibly free fontset ID.  Usually this keeps the minimum
   fontset ID not yet used.  */
static int next_fontset_id;

/* The default fontset.  This gives default FAMILY and REGISTRY of
   font for each character.  */
static Lisp_Object Vdefault_fontset;

/* Prototype declarations for static functions.  */
static Lisp_Object make_fontset (Lisp_Object, Lisp_Object, Lisp_Object);

/* Return true if ID is a valid fontset id.
   Optimized away if ENABLE_CHECKING is not defined.  */

static bool
fontset_id_valid_p (int id)
{
  return (id >= 0 && id < ASIZE (Vfontset_table) - 1);
}



/********** MACROS AND FUNCTIONS TO HANDLE FONTSET **********/

/* Return the fontset with ID.  No check of ID's validness.  */
#define FONTSET_FROM_ID(id) AREF (Vfontset_table, id)

/* Access special values of FONTSET.  */

#define FONTSET_ID(fontset) XCHAR_TABLE (fontset)->extras[0]
static void
set_fontset_id (Lisp_Object fontset, Lisp_Object id)
{
  set_char_table_extras (fontset, 0, id);
}

/* Access special values of (base) FONTSET.  */

#define FONTSET_NAME(fontset) XCHAR_TABLE (fontset)->extras[1]
static void
set_fontset_name (Lisp_Object fontset, Lisp_Object name)
{
  set_char_table_extras (fontset, 1, name);
}

#define FONTSET_ASCII(fontset) XCHAR_TABLE (fontset)->extras[2]
static void
set_fontset_ascii (Lisp_Object fontset, Lisp_Object ascii)
{
  set_char_table_extras (fontset, 2, ascii);
}

/* Access special values of (realized) FONTSET.  */

#define FONTSET_BASE(fontset) XCHAR_TABLE (fontset)->extras[3]
static void
set_fontset_base (Lisp_Object fontset, Lisp_Object base)
{
  set_char_table_extras (fontset, 3, base);
}

#define FONTSET_FRAME(fontset) XCHAR_TABLE (fontset)->extras[4]
static void
set_fontset_frame (Lisp_Object fontset, Lisp_Object frame)
{
  set_char_table_extras (fontset, 4, frame);
}

#define FONTSET_NOFONT_FACE(fontset) XCHAR_TABLE (fontset)->extras[5]
static void
set_fontset_nofont_face (Lisp_Object fontset, Lisp_Object face)
{
  set_char_table_extras (fontset, 5, face);
}

#define FONTSET_DEFAULT(fontset) XCHAR_TABLE (fontset)->extras[6]
static void
set_fontset_default (Lisp_Object fontset, Lisp_Object def)
{
  set_char_table_extras (fontset, 6, def);
}

/* For both base and realized fontset.  */

#define FONTSET_FALLBACK(fontset) XCHAR_TABLE (fontset)->extras[7]
static void
set_fontset_fallback (Lisp_Object fontset, Lisp_Object fallback)
{
  set_char_table_extras (fontset, 7, fallback);
}

#define BASE_FONTSET_P(fontset) (NILP (FONTSET_BASE (fontset)))

/* Macros for FONT-DEF and RFONT-DEF of fontset.  */
#define FONT_DEF_NEW(font_def, font_spec, encoding, repertory)	\
  do {								\
    (font_def) = make_uninit_vector (3);			\
    ASET ((font_def), 0, font_spec);				\
    ASET ((font_def), 1, encoding);				\
    ASET ((font_def), 2, repertory);				\
  } while (0)

#define FONT_DEF_SPEC(font_def) AREF (font_def, 0)
#define FONT_DEF_ENCODING(font_def) AREF (font_def, 1)
#define FONT_DEF_REPERTORY(font_def) AREF (font_def, 2)

#define RFONT_DEF_FACE(rfont_def) AREF (rfont_def, 0)
#define RFONT_DEF_SET_FACE(rfont_def, face_id)	\
  ASET ((rfont_def), 0, make_number (face_id))
#define RFONT_DEF_FONT_DEF(rfont_def) AREF (rfont_def, 1)
#define RFONT_DEF_SPEC(rfont_def) FONT_DEF_SPEC (AREF (rfont_def, 1))
#define RFONT_DEF_OBJECT(rfont_def) AREF (rfont_def, 2)
#define RFONT_DEF_SET_OBJECT(rfont_def, object)	\
  ASET ((rfont_def), 2, (object))
/* Score of RFONT_DEF is an integer value; the lowest 8 bits represent
   the order of listing by font backends, the higher bits represents
   the order given by charset priority list.  The smaller value is
   preferable.  */
#define RFONT_DEF_SCORE(rfont_def) XINT (AREF (rfont_def, 3))
#define RFONT_DEF_SET_SCORE(rfont_def, score) \
  ASET ((rfont_def), 3, make_number (score))
#define RFONT_DEF_NEW(rfont_def, font_def)		\
  do {							\
    (rfont_def) = Fmake_vector (make_number (4), Qnil);	\
    ASET ((rfont_def), 1, (font_def));			\
    RFONT_DEF_SET_SCORE ((rfont_def), 0);		\
  } while (0)


/* Return the element of FONTSET for the character C.  If FONTSET is a
   base fontset other then the default fontset and FONTSET doesn't
   contain information for C, return the information in the default
   fontset.  */

#define FONTSET_REF(fontset, c)		\
  (EQ (fontset, Vdefault_fontset)	\
   ? CHAR_TABLE_REF (fontset, c)	\
   : fontset_ref ((fontset), (c)))

static Lisp_Object
fontset_ref (Lisp_Object fontset, int c)
{
  Lisp_Object elt;

  elt = CHAR_TABLE_REF (fontset, c);
  if (NILP (elt) && ! EQ (fontset, Vdefault_fontset)
      /* Don't check Vdefault_fontset for a realized fontset.  */
      && NILP (FONTSET_BASE (fontset)))
    elt = CHAR_TABLE_REF (Vdefault_fontset, c);
  return elt;
}

/* Set elements of FONTSET for characters in RANGE to the value ELT.
   RANGE is a cons (FROM . TO), where FROM and TO are character codes
   specifying a range.  */

#define FONTSET_SET(fontset, range, elt)	\
  Fset_char_table_range ((fontset), (range), (elt))


/* Modify the elements of FONTSET for characters in RANGE by replacing
   with ELT or adding ELT.  RANGE is a cons (FROM . TO), where FROM
   and TO are character codes specifying a range.  If ADD is nil,
   replace with ELT, if ADD is `prepend', prepend ELT, otherwise,
   append ELT.  */

#define FONTSET_ADD(fontset, range, elt, add)				\
  (NILP (add)								\
   ? (NILP (range)							\
      ? (set_fontset_fallback						\
	 (fontset, Fmake_vector (make_number (1), (elt))))		\
      : ((void)								\
	 Fset_char_table_range (fontset, range,				\
				Fmake_vector (make_number (1), elt))))	\
   : fontset_add ((fontset), (range), (elt), (add)))

static void
fontset_add (Lisp_Object fontset, Lisp_Object range, Lisp_Object elt, Lisp_Object add)
{
  Lisp_Object args[2];
  int idx = (EQ (add, Qappend) ? 0 : 1);

  args[1 - idx] = Fmake_vector (make_number (1), elt);

  if (CONSP (range))
    {
      int from = XINT (XCAR (range));
      int to = XINT (XCDR (range));
      int from1, to1;

      do {
	from1 = from, to1 = to;
	args[idx] = char_table_ref_and_range (fontset, from, &from1, &to1);
	char_table_set_range (fontset, from, to1,
			      (NILP (args[idx]) ? args[1 - idx]
			       : CALLMANY (Fvconcat, args)));
	from = to1 + 1;
      } while (from < to);
    }
  else
    {
      args[idx] = FONTSET_FALLBACK (fontset);
      set_fontset_fallback (fontset,
			    (NILP (args[idx]) ? args[1 - idx]
			     : CALLMANY (Fvconcat, args)));
    }
}

static int
fontset_compare_rfontdef (const void *val1, const void *val2)
{
  return (RFONT_DEF_SCORE (*(Lisp_Object *) val1)
	  - RFONT_DEF_SCORE (*(Lisp_Object *) val2));
}

/* Update a cons cell which has this form:
	(CHARSET-ORDERED-LIST-TICK . FONT-GROUP)
   where FONT-GROUP is of the form
	[ PREFERRED-RFONT-DEF RFONT-DEF0 RFONT-DEF1 ... ]
   Reorder RFONT-DEFs according to the current language, and update
   CHARSET-ORDERED-LIST-TICK.  */

static void
reorder_font_vector (Lisp_Object font_group, struct font *font)
{
  Lisp_Object vec, font_object;
  int size;
  int i;
  bool score_changed = false;

  if (font)
    XSETFONT (font_object, font);
  else
    font_object = Qnil;

  vec = XCDR (font_group);
  size = ASIZE (vec);
  /* Exclude the tailing nil element from the reordering.  */
  if (NILP (AREF (vec, size - 1)))
    size--;

  for (i = 0; i < size; i++)
    {
      Lisp_Object rfont_def = AREF (vec, i);
      Lisp_Object font_def = RFONT_DEF_FONT_DEF (rfont_def);
      Lisp_Object font_spec = FONT_DEF_SPEC (font_def);
      int score = RFONT_DEF_SCORE (rfont_def) & 0xFF;
      Lisp_Object otf_spec = Ffont_get (font_spec, QCotf);

      if (! NILP (otf_spec))
	/* A font-spec with :otf is preferable regardless of encoding
	   and language..  */
	;
      else if (! font_match_p (font_spec, font_object))
	{
	  Lisp_Object encoding = FONT_DEF_ENCODING (font_def);

	  if (! NILP (encoding))
	    {
	      /* This spec specifies an encoding by a charset set
		 name.  Reflect the preference order of that charset
		 in the upper bits of SCORE.  */
	      Lisp_Object tail;

	      for (tail = Vcharset_ordered_list;
		   ! EQ (tail, Vcharset_non_preferred_head) && CONSP (tail);
		   tail = XCDR (tail))
		if (EQ (encoding, XCAR (tail)))
		  break;
		else if (score <= min (INT_MAX, MOST_POSITIVE_FIXNUM) - 0x100)
		  score += 0x100;
	    }
	  else
	    {
	      /* This spec does not specify an encoding.  If the spec
		 specifies a language, and the language is not for the
		 current language environment, make the score
		 larger.  */
	      Lisp_Object lang = Ffont_get (font_spec, QClang);

	      if (! NILP (lang)
		  && ! EQ (lang, Vcurrent_iso639_language)
		  && (! CONSP (Vcurrent_iso639_language)
		      || NILP (Fmemq (lang, Vcurrent_iso639_language))))
		score |= 0x100;
	    }
	}
      if (RFONT_DEF_SCORE (rfont_def) != score)
	{
	  RFONT_DEF_SET_SCORE (rfont_def, score);
	  score_changed = true;
	}
    }

  if (score_changed)
    qsort (XVECTOR (vec)->contents, size, word_size,
	   fontset_compare_rfontdef);
  EMACS_INT low_tick_bits = charset_ordered_list_tick & MOST_POSITIVE_FIXNUM;
  XSETCAR (font_group, make_number (low_tick_bits));
}

/* Return a font-group (actually a cons (CHARSET_ORDERED_LIST_TICK
   . FONT-GROUP)) for character C or a fallback font-group in the
   realized fontset FONTSET.  The elements of FONT-GROUP are
   RFONT-DEFs.  The value may not be a cons.  See the comment at the
   head of this file for the detail of the return value.  */

static Lisp_Object
fontset_get_font_group (Lisp_Object fontset, int c)
{
  Lisp_Object font_group;
  Lisp_Object base_fontset;
  int from = 0, to = MAX_CHAR, i;

  eassert (! BASE_FONTSET_P (fontset));
  if (c >= 0)
    font_group = CHAR_TABLE_REF (fontset, c);
  else
    font_group = FONTSET_FALLBACK (fontset);
  if (! NILP (font_group))
    /* We have already realized FONT-DEFs of this font group for C or
       for fallback (FONT_GROUP is a cons), or we have already found
       that no appropriate font was found (FONT_GROUP is t or 0).  */
    return font_group;
  base_fontset = FONTSET_BASE (fontset);
  if (NILP (base_fontset))
    /* Actually we never come here because FONTSET is a realized one,
       and thus it should have a base.  */
    font_group = Qnil;
  else if (c >= 0)
    font_group = char_table_ref_and_range (base_fontset, c, &from, &to);
  else
    font_group = FONTSET_FALLBACK (base_fontset);

  /* FONT_GROUP not being a vector means that no fonts are specified
     for C, or the fontset does not have fallback fonts.  */
  if (NILP (font_group))
    {
      font_group = make_number (0);
      if (c >= 0)
	/* Record that FONTSET does not specify fonts for C.  As
	   there's a possibility that a font is found in a fallback
	   font group, we set 0 at the moment.  */
	char_table_set_range (fontset, from, to, font_group);
      return font_group;
    }
  if (!VECTORP (font_group))
    return font_group;

  /* Now realize FONT-DEFs of this font group, and update the realized
     fontset FONTSET. */
  font_group = Fcopy_sequence (font_group);
  for (i = 0; i < ASIZE (font_group); i++)
    if (! NILP (AREF (font_group, i)))
      {
	Lisp_Object rfont_def;

	RFONT_DEF_NEW (rfont_def, AREF (font_group, i));
	/* Remember the original order.  */
	RFONT_DEF_SET_SCORE (rfont_def, i);
	ASET (font_group, i, rfont_def);
      }
  font_group = Fcons (make_number (-1), font_group);
  if (c >= 0)
    char_table_set_range (fontset, from, to, font_group);
  else
    set_fontset_fallback (fontset, font_group);
  return font_group;
}

/* Return RFONT-DEF (vector) in the realized fontset FONTSET for the
   character C.  If no font is found, return Qnil or 0 if there's a
   possibility that the default fontset or the fallback font groups
   have a proper font, and return Qt if not.

   If a font is found but is not yet opened, open it (if FACE is not
   NULL) or return Qnil (if FACE is NULL).

   CHARSET_ID is a charset-id that must be preferred, or -1 meaning no
   preference.

   If FALLBACK, search only fallback fonts.  */

static Lisp_Object
fontset_find_font (Lisp_Object fontset, int c, struct face *face,
		   int charset_id, bool fallback)
{
  Lisp_Object vec, font_group;
  int i, charset_matched = 0, found_index;
  struct frame *f = (FRAMEP (FONTSET_FRAME (fontset))
		     ? XFRAME (FONTSET_FRAME (fontset))
		     : XFRAME (selected_frame));
  Lisp_Object rfont_def;

  font_group = fontset_get_font_group (fontset, fallback ? -1 : c);
  if (! CONSP (font_group))
    return font_group;
  vec = XCDR (font_group);
  if (ASIZE (vec) == 0)
    return Qnil;

  if (ASIZE (vec) > 1)
    {
      if (XINT (XCAR (font_group)) != charset_ordered_list_tick)
	/* We have just created the font-group,
	   or the charset priorities were changed.  */
	reorder_font_vector (font_group, face->ascii_face->font);
      if (charset_id >= 0)
	/* Find a spec matching with CHARSET_ID to try it at
	   first.  */
	for (i = 0; i < ASIZE (vec); i++)
	  {
	    Lisp_Object repertory;

	    rfont_def = AREF (vec, i);
	    if (NILP (rfont_def))
	      break;
	    repertory = FONT_DEF_REPERTORY (RFONT_DEF_FONT_DEF (rfont_def));

	    if (XINT (repertory) == charset_id)
	      {
		charset_matched = i;
		break;
	      }
	  }
    }

  /* Find the first available font in the vector of RFONT-DEF.  If
     CHARSET_MATCHED > 0, try the corresponding RFONT-DEF first, then
     try the rest.  */
  for (i = 0; i < ASIZE (vec); i++)
    {
      Lisp_Object font_def;
      Lisp_Object font_entity, font_object;

      found_index = i;
      if (i == 0)
	{
	  if (charset_matched > 0)
	    {
	      /* Try the element matching with CHARSET_ID at first.  */
	      found_index = charset_matched;
	      /* Make this negative so that we don't come here in the
		 next loop.  */
	      charset_matched = - charset_matched;
	      /* We must try the first element in the next loop.  */
	      i = -1;
	    }
	}
      else if (i == - charset_matched)
	{
	  /* We have already tried this element and the followings
	     that have the same font specifications in the first
	     iteration.  So, skip them all.  */
	  rfont_def = AREF (vec, i);
	  font_def = RFONT_DEF_FONT_DEF (rfont_def);
	  for (; i + 1 < ASIZE (vec); i++)
	    {
	      rfont_def = AREF (vec, i + 1);
	      if (NILP (rfont_def))
		break;
	      if (! EQ (RFONT_DEF_FONT_DEF (rfont_def), font_def))
		break;
	    }
	  continue;
	}

      rfont_def = AREF (vec, found_index);
      if (NILP (rfont_def))
	{
	  if (i < 0)
	    continue;
	  /* This is a sign of not to try the other fonts.  */
	  return Qt;
	}
      if (INTEGERP (RFONT_DEF_FACE (rfont_def))
	  && XINT (RFONT_DEF_FACE (rfont_def)) < 0)
	/* We couldn't open this font last time.  */
	continue;

      font_object = RFONT_DEF_OBJECT (rfont_def);
      if (NILP (font_object))
	{
	  font_def = RFONT_DEF_FONT_DEF (rfont_def);

	  if (! face)
	    /* We have not yet opened the font.  */
	    return Qnil;
	  /* Find a font best-matching with the spec without checking
	     the support of the character C.  That checking is costly,
	     and even without the checking, the found font supports C
	     in high possibility.  */
	  font_entity = font_find_for_lface (f, face->lface,
					     FONT_DEF_SPEC (font_def), -1);
	  if (NILP (font_entity))
	    {
	      /* Record that no font matches the spec.  */
	      RFONT_DEF_SET_FACE (rfont_def, -1);
	      continue;
	    }
	  font_object = font_open_for_lface (f, font_entity, face->lface,
					     FONT_DEF_SPEC (font_def));
	  if (NILP (font_object))
	    {
	      /* Something strange happened, perhaps because of a
		 Font-backend problem.  To avoid crashing, record
		 that this spec is unusable.  It may be better to find
		 another font of the same spec, but currently we don't
		 have such an API in font-backend.  */
	      RFONT_DEF_SET_FACE (rfont_def, -1);
	      continue;
	    }
	  RFONT_DEF_SET_OBJECT (rfont_def, font_object);
	}

      if (font_has_char (f, font_object, c))
	goto found;

      /* Find a font already opened, matching with the current spec,
	 and supporting C. */
      font_def = RFONT_DEF_FONT_DEF (rfont_def);
      for (; found_index + 1 < ASIZE (vec); found_index++)
	{
	  rfont_def = AREF (vec, found_index + 1);
	  if (NILP (rfont_def))
	    break;
	  if (! EQ (RFONT_DEF_FONT_DEF (rfont_def), font_def))
	    break;
	  font_object = RFONT_DEF_OBJECT (rfont_def);
	  if (! NILP (font_object) && font_has_char (f, font_object, c))
	    {
	      found_index++;
	      goto found;
	    }
	}

      /* Find a font-entity with the current spec and supporting C.  */
      font_entity = font_find_for_lface (f, face->lface,
					 FONT_DEF_SPEC (font_def), c);
      if (! NILP (font_entity))
	{
	  /* We found a font.  Open it and insert a new element for
	     that font in VEC.  */
	  Lisp_Object new_vec;
	  int j;

	  font_object = font_open_for_lface (f, font_entity, face->lface,
					     Qnil);
	  if (NILP (font_object))
	    continue;
	  RFONT_DEF_NEW (rfont_def, font_def);
	  RFONT_DEF_SET_OBJECT (rfont_def, font_object);
	  RFONT_DEF_SET_SCORE (rfont_def, RFONT_DEF_SCORE (rfont_def));
	  new_vec = Fmake_vector (make_number (ASIZE (vec) + 1), Qnil);
	  found_index++;
	  for (j = 0; j < found_index; j++)
	    ASET (new_vec, j, AREF (vec, j));
	  ASET (new_vec, j, rfont_def);
	  for (j++; j < ASIZE (new_vec); j++)
	    ASET (new_vec, j, AREF (vec, j - 1));
	  XSETCDR (font_group, new_vec);
	  vec = new_vec;
	  goto found;
	}
      if (i >= 0)
	i = found_index;
    }

  /* Record that no font in this font group supports C.  */
  FONTSET_SET (fontset, make_number (c), make_number (0));
  return Qnil;

 found:
  if (fallback && found_index > 0)
    {
      /* The order of fonts in the fallback font-group is not that
	 important, and it is better to move the found font to the
	 first of the group so that the next try will find it
	 quickly. */
      for (i = found_index; i > 0; i--)
	ASET (vec, i, AREF (vec, i - 1));
      ASET (vec, 0, rfont_def);
    }
  return rfont_def;
}


/* Return RFONT-DEF (vector) corresponding to the font for character
   C.  The value is not a vector if no font is found for C.  */

static Lisp_Object
fontset_font (Lisp_Object fontset, int c, struct face *face, int id)
{
  Lisp_Object rfont_def;
  Lisp_Object default_rfont_def UNINIT;
  Lisp_Object base_fontset;

  /* Try a font-group of FONTSET. */
  FONT_DEFERRED_LOG ("current fontset: font for", make_number (c), Qnil);
  rfont_def = fontset_find_font (fontset, c, face, id, 0);
  if (VECTORP (rfont_def))
    return rfont_def;
  if (NILP (rfont_def))
    FONTSET_SET (fontset, make_number (c), make_number (0));

  /* Try a font-group of the default fontset. */
  base_fontset = FONTSET_BASE (fontset);
  if (! EQ (base_fontset, Vdefault_fontset))
    {
      if (NILP (FONTSET_DEFAULT (fontset)))
	set_fontset_default
	  (fontset,
	   make_fontset (FONTSET_FRAME (fontset), Qnil, Vdefault_fontset));
      FONT_DEFERRED_LOG ("default fontset: font for", make_number (c), Qnil);
      default_rfont_def
	= fontset_find_font (FONTSET_DEFAULT (fontset), c, face, id, 0);
      if (VECTORP (default_rfont_def))
	return default_rfont_def;
      if (NILP (default_rfont_def))
	FONTSET_SET (FONTSET_DEFAULT (fontset), make_number (c),
		     make_number (0));
    }

  /* Try a fallback font-group of FONTSET. */
  if (! EQ (rfont_def, Qt))
    {
      FONT_DEFERRED_LOG ("current fallback: font for", make_number (c), Qnil);
      rfont_def = fontset_find_font (fontset, c, face, id, 1);
      if (VECTORP (rfont_def))
	return rfont_def;
      /* Remember that FONTSET has no font for C.  */
      FONTSET_SET (fontset, make_number (c), Qt);
    }

  /* Try a fallback font-group of the default fontset. */
  if (! EQ (base_fontset, Vdefault_fontset)
      && ! EQ (default_rfont_def, Qt))
    {
      FONT_DEFERRED_LOG ("default fallback: font for", make_number (c), Qnil);
      rfont_def = fontset_find_font (FONTSET_DEFAULT (fontset), c, face, id, 1);
      if (VECTORP (rfont_def))
	return rfont_def;
      /* Remember that the default fontset has no font for C.  */
      FONTSET_SET (FONTSET_DEFAULT (fontset), make_number (c), Qt);
    }

  return Qnil;
}

/* Return a newly created fontset with NAME.  If BASE is nil, make a
   base fontset.  Otherwise make a realized fontset whose base is
   BASE.  */

static Lisp_Object
make_fontset (Lisp_Object frame, Lisp_Object name, Lisp_Object base)
{
  Lisp_Object fontset;
  int size = ASIZE (Vfontset_table);
  int id = next_fontset_id;

  /* Find a free slot in Vfontset_table.  Usually, next_fontset_id is
     the next available fontset ID.  So it is expected that this loop
     terminates quickly.  In addition, as the last element of
     Vfontset_table is always nil, we don't have to check the range of
     id.  */
  while (!NILP (AREF (Vfontset_table, id))) id++;

  if (id + 1 == size)
    Vfontset_table = larger_vector (Vfontset_table, 1, -1);

  fontset = Fmake_char_table (Qfontset, Qnil);

  set_fontset_id (fontset, make_number (id));
  if (NILP (base))
    set_fontset_name (fontset, name);
  else
    {
      set_fontset_name (fontset, Qnil);
      set_fontset_frame (fontset, frame);
      set_fontset_base (fontset, base);
    }

  ASET (Vfontset_table, id, fontset);
  next_fontset_id = id + 1;
  return fontset;
}


/********** INTERFACES TO xfaces.c, xfns.c, and dispextern.h **********/

/* Return the name of the fontset who has ID.  */

Lisp_Object
fontset_name (int id)
{
  Lisp_Object fontset;

  fontset = FONTSET_FROM_ID (id);
  return FONTSET_NAME (fontset);
}


/* Return the ASCII font name of the fontset who has ID.  */

Lisp_Object
fontset_ascii (int id)
{
  Lisp_Object fontset, elt;

  fontset= FONTSET_FROM_ID (id);
  elt = FONTSET_ASCII (fontset);
  if (CONSP (elt))
    elt = XCAR (elt);
  return elt;
}

/* Free fontset of FACE defined on frame F.  Called from
   free_realized_face.  */

void
free_face_fontset (struct frame *f, struct face *face)
{
  Lisp_Object fontset;

  fontset = FONTSET_FROM_ID (face->fontset);
  if (NILP (fontset))
    return;
  eassert (! BASE_FONTSET_P (fontset));
  eassert (f == XFRAME (FONTSET_FRAME (fontset)));
  ASET (Vfontset_table, face->fontset, Qnil);
  if (face->fontset < next_fontset_id)
    next_fontset_id = face->fontset;
  if (! NILP (FONTSET_DEFAULT (fontset)))
    {
      int id = XINT (FONTSET_ID (FONTSET_DEFAULT (fontset)));

      fontset = AREF (Vfontset_table, id);
      eassert (!NILP (fontset) && ! BASE_FONTSET_P (fontset));
      eassert (f == XFRAME (FONTSET_FRAME (fontset)));
      ASET (Vfontset_table, id, Qnil);
      if (id < next_fontset_id)
	next_fontset_id = face->fontset;
    }
  face->fontset = -1;
}

/* Return ID of face suitable for displaying character C at buffer position
   POS on frame F.  FACE must be realized for ASCII characters in advance.
   Called from the macro FACE_FOR_CHAR.  */

int
face_for_char (struct frame *f, struct face *face, int c,
	       ptrdiff_t pos, Lisp_Object object)
{
  Lisp_Object fontset, rfont_def, charset;
  int face_id;
  int id;

  eassert (fontset_id_valid_p (face->fontset));

  if (ASCII_CHAR_P (c) || CHAR_BYTE8_P (c))
    return face->ascii_face->id;

  if (use_default_font_for_symbols  /* let the user disable this feature */
      && c > 0 && EQ (CHAR_TABLE_REF (Vchar_script_table, c), Qsymbol))
    {
      /* Fonts often have characters for punctuation and other
         symbols, even if they don't match the 'symbol' script.  So
         check if the character is present in the current ASCII face
         first, and if so, use the same font as used by that face.
         This avoids unnecessarily switching to another font when the
         frame's default font will do.  We only do this for symbols so
         that users could still setup fontsets to force Emacs to use
         specific fonts for characters from other scripts, because
         choice of fonts is frequently affected by cultural
         preferences and font features, not by font coverage.
         However, these considerations are unlikely to be relevant to
         punctuation and other symbols, since the latter generally
         aren't specific to any culture, and don't require
         sophisticated OTF features.  */
      Lisp_Object font_object;

      if (face->ascii_face->font)
	{
	  XSETFONT (font_object, face->ascii_face->font);
	  if (font_has_char (f, font_object, c))
	    return face->ascii_face->id;
	}

#if 0
      /* Try the current face.  Disabled because it can cause
	 counter-intuitive results, whereby the font used for some
	 character depends on the characters that precede it on
	 display.  See the discussion of bug #15138.  Note that the
	 original bug reported in #15138 was in a situation where face
	 == face->ascii_face, so the above code solves that situation
	 without risking the undesirable consequences.  */
      if (face->font)
	{
	  XSETFONT (font_object, face->font);
	  if (font_has_char (f, font_object, c)) return face->id;
	}
#endif
    }

  fontset = FONTSET_FROM_ID (face->fontset);
  eassert (!BASE_FONTSET_P (fontset));

  if (pos < 0)
    {
      id = -1;
      charset = Qnil;
    }
  else
    {
      charset = Fget_char_property (make_number (pos), Qcharset, object);
      if (CHARSETP (charset))
	{
	  Lisp_Object val;

	  val = assq_no_quit (charset, Vfont_encoding_charset_alist);
	  if (CONSP (val) && CHARSETP (XCDR (val)))
	    charset = XCDR (val);
	  id = XINT (CHARSET_SYMBOL_ID (charset));
	}
      else
	id = -1;
    }

  rfont_def = fontset_font (fontset, c, face, id);
  if (VECTORP (rfont_def))
    {
      if (INTEGERP (RFONT_DEF_FACE (rfont_def)))
	face_id = XINT (RFONT_DEF_FACE (rfont_def));
      else
	{
	  Lisp_Object font_object;

	  font_object = RFONT_DEF_OBJECT (rfont_def);
	  face_id = face_for_font (f, font_object, face);
	  RFONT_DEF_SET_FACE (rfont_def, face_id);
	}
    }
  else
    {
      if (INTEGERP (FONTSET_NOFONT_FACE (fontset)))
	face_id = XINT (FONTSET_NOFONT_FACE (fontset));
      else
	{
	  face_id = face_for_font (f, Qnil, face);
	  set_fontset_nofont_face (fontset, make_number (face_id));
	}
    }
  eassert (face_id >= 0);
  return face_id;
}


Lisp_Object
font_for_char (struct face *face, int c, ptrdiff_t pos, Lisp_Object object)
{
  Lisp_Object fontset, rfont_def, charset;
  int id;

  if (ASCII_CHAR_P (c))
    {
      Lisp_Object font_object;

      XSETFONT (font_object, face->ascii_face->font);
      return font_object;
    }

  eassert (fontset_id_valid_p (face->fontset));
  fontset = FONTSET_FROM_ID (face->fontset);
  eassert (!BASE_FONTSET_P (fontset));
  if (pos < 0)
    {
      id = -1;
      charset = Qnil;
    }
  else
    {
      charset = Fget_char_property (make_number (pos), Qcharset, object);
      if (CHARSETP (charset))
	{
	  Lisp_Object val;

	  val = assq_no_quit (charset, Vfont_encoding_charset_alist);
	  if (CONSP (val) && CHARSETP (XCDR (val)))
	    charset = XCDR (val);
	  id = XINT (CHARSET_SYMBOL_ID (charset));
	}
      else
	id = -1;
    }

  rfont_def = fontset_font (fontset, c, face, id);
  return (VECTORP (rfont_def)
	  ? RFONT_DEF_OBJECT (rfont_def)
	  : Qnil);
}


/* Make a realized fontset for ASCII face FACE on frame F from the
   base fontset BASE_FONTSET_ID.  If BASE_FONTSET_ID is -1, use the
   default fontset as the base.  Value is the id of the new fontset.
   Called from realize_x_face.  */

int
make_fontset_for_ascii_face (struct frame *f, int base_fontset_id, struct face *face)
{
  Lisp_Object base_fontset, fontset, frame;

  XSETFRAME (frame, f);
  if (base_fontset_id >= 0)
    {
      base_fontset = FONTSET_FROM_ID (base_fontset_id);
      if (!BASE_FONTSET_P (base_fontset))
	base_fontset = FONTSET_BASE (base_fontset);
      eassert (BASE_FONTSET_P (base_fontset));
    }
  else
    base_fontset = Vdefault_fontset;

  fontset = make_fontset (frame, Qnil, base_fontset);
  return XINT (FONTSET_ID (fontset));
}



/* Cache data used by fontset_pattern_regexp.  The car part is a
   pattern string containing at least one wild card, the cdr part is
   the corresponding regular expression.  */
static Lisp_Object Vcached_fontset_data;

#define CACHED_FONTSET_NAME SSDATA (XCAR (Vcached_fontset_data))
#define CACHED_FONTSET_REGEX (XCDR (Vcached_fontset_data))

/* If fontset name PATTERN contains any wild card, return regular
   expression corresponding to PATTERN.  */

static Lisp_Object
fontset_pattern_regexp (Lisp_Object pattern)
{
  if (!strchr (SSDATA (pattern), '*')
      && !strchr (SSDATA (pattern), '?'))
    /* PATTERN does not contain any wild cards.  */
    return Qnil;

  if (!CONSP (Vcached_fontset_data)
      || strcmp (SSDATA (pattern), CACHED_FONTSET_NAME))
    {
      /* We must at first update the cached data.  */
      unsigned char *regex, *p0, *p1;
      int ndashes = 0, nstars = 0, nescs = 0;

      for (p0 = SDATA (pattern); *p0; p0++)
	{
	  if (*p0 == '-')
	    ndashes++;
	  else if (*p0 == '*')
	    nstars++;
	  else if (*p0 == '['
		   || *p0 == '.' || *p0 == '\\'
		   || *p0 == '+' || *p0 == '^'
		   || *p0 == '$')
	    nescs++;
	}

      /* If PATTERN is not full XLFD we convert "*" to ".*".  Otherwise
	 we convert "*" to "[^-]*" which is much faster in regular
	 expression matching.  */
      ptrdiff_t regexsize = (SBYTES (pattern)
			     + (ndashes < 14 ? 2 : 5) * nstars
			     + 2 * nescs + 3);
      USE_SAFE_ALLOCA;
      p1 = regex = SAFE_ALLOCA (regexsize);

      *p1++ = '^';
      for (p0 = SDATA (pattern); *p0; p0++)
	{
	  if (*p0 == '*')
	    {
	      if (ndashes < 14)
		*p1++ = '.';
	      else
		*p1++ = '[', *p1++ = '^', *p1++ = '-', *p1++ = ']';
	      *p1++ = '*';
	    }
	  else if (*p0 == '?')
	    *p1++ = '.';
	  else if (*p0 == '['
		   || *p0 == '.' || *p0 == '\\'
		   || *p0 == '+' || *p0 == '^'
		   || *p0 == '$')
	    *p1++ = '\\', *p1++ = *p0;
	  else
	    *p1++ = *p0;
	}
      *p1++ = '$';
      *p1++ = 0;

      Vcached_fontset_data = Fcons (build_string (SSDATA (pattern)),
				    build_string ((char *) regex));
      SAFE_FREE ();
    }

  return CACHED_FONTSET_REGEX;
}

/* Return ID of the base fontset named NAME.  If there's no such
   fontset, return -1.  NAME_PATTERN specifies how to treat NAME as this:
     0: pattern containing '*' and '?' as wildcards
     1: regular expression
     2: literal fontset name
*/

int
fs_query_fontset (Lisp_Object name, int name_pattern)
{
  Lisp_Object tem;
  int i;

  name = Fdowncase (name);
  if (name_pattern != 1)
    {
      tem = Frassoc (name, Vfontset_alias_alist);
      if (NILP (tem))
	tem = Fassoc (name, Vfontset_alias_alist, Qnil);
      if (CONSP (tem) && STRINGP (XCAR (tem)))
	name = XCAR (tem);
      else if (name_pattern == 0)
	{
	  tem = fontset_pattern_regexp (name);
	  if (STRINGP (tem))
	    {
	      name = tem;
	      name_pattern = 1;
	    }
	}
    }

  for (i = 0; i < ASIZE (Vfontset_table); i++)
    {
      Lisp_Object fontset, this_name;

      fontset = FONTSET_FROM_ID (i);
      if (NILP (fontset)
	  || !BASE_FONTSET_P (fontset))
	continue;

      this_name = FONTSET_NAME (fontset);
      if (name_pattern == 1
	  ? fast_string_match_ignore_case (name, this_name) >= 0
	  : !xstrcasecmp (SSDATA (name), SSDATA (this_name)))
	return i;
    }
  return -1;
}


DEFUN ("query-fontset", Fquery_fontset, Squery_fontset, 1, 2, 0,
       doc: /* Return the name of a fontset that matches PATTERN.
The value is nil if there is no matching fontset.
PATTERN can contain `*' or `?' as a wildcard
just as X font name matching algorithm allows.
If REGEXPP is non-nil, PATTERN is a regular expression.  */)
  (Lisp_Object pattern, Lisp_Object regexpp)
{
  Lisp_Object fontset;
  int id;

  check_window_system (NULL);

  CHECK_STRING (pattern);

  if (SCHARS (pattern) == 0)
    return Qnil;

  id = fs_query_fontset (pattern, !NILP (regexpp));
  if (id < 0)
    return Qnil;

  fontset = FONTSET_FROM_ID (id);
  return FONTSET_NAME (fontset);
}

/* Return a list of base fontset names matching PATTERN on frame F.  */

Lisp_Object
list_fontsets (struct frame *f, Lisp_Object pattern, int size)
{
  Lisp_Object frame, regexp, val;
  int id;

  XSETFRAME (frame, f);

  regexp = fontset_pattern_regexp (pattern);
  val = Qnil;

  for (id = 0; id < ASIZE (Vfontset_table); id++)
    {
      Lisp_Object fontset, name;

      fontset = FONTSET_FROM_ID (id);
      if (NILP (fontset)
	  || !BASE_FONTSET_P (fontset)
	  || !EQ (frame, FONTSET_FRAME (fontset)))
	continue;
      name = FONTSET_NAME (fontset);

      if (STRINGP (regexp)
	  ? (fast_string_match (regexp, name) < 0)
	  : strcmp (SSDATA (pattern), SSDATA (name)))
	continue;

      val = Fcons (Fcopy_sequence (FONTSET_NAME (fontset)), val);
    }

  return val;
}


/* Free all realized fontsets whose base fontset is BASE.  */

static void
free_realized_fontsets (Lisp_Object base)
{
  int id;

#if 0
  /* For the moment, this doesn't work because free_realized_face
     doesn't remove FACE from a cache.  Until we find a solution, we
     suppress this code, and simply use Fclear_face_cache even though
     that is not efficient.  */
  block_input ();
  for (id = 0; id < ASIZE (Vfontset_table); id++)
    {
      Lisp_Object this = AREF (Vfontset_table, id);

      if (EQ (FONTSET_BASE (this), base))
	{
	  Lisp_Object tail;

	  for (tail = FONTSET_FACE_ALIST (this); CONSP (tail);
	       tail = XCDR (tail))
	    {
	      struct frame *f = XFRAME (FONTSET_FRAME (this));
	      int face_id = XINT (XCDR (XCAR (tail)));
	      struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);

	      /* Face THIS itself is also freed by the following call.  */
	      free_realized_face (f, face);
	    }
	}
    }
  unblock_input ();
#else  /* not 0 */
  /* But, we don't have to call Fclear_face_cache if no fontset has
     been realized from BASE.  */
  for (id = 0; id < ASIZE (Vfontset_table); id++)
    {
      Lisp_Object this = AREF (Vfontset_table, id);

      if (CHAR_TABLE_P (this) && EQ (FONTSET_BASE (this), base))
	{
	  Fclear_face_cache (Qt);
	  /* This is in case some Lisp calls this function and then
	     proceeds with calling some other function, like font-at,
	     which needs the basic faces.  */
	  recompute_basic_faces (XFRAME (FONTSET_FRAME (this)));
	  break;
	}
    }
#endif /* not 0 */
}


/* Check validity of NAME as a fontset name and return the
   corresponding fontset.  If not valid, signal an error.

   If NAME is t, return Vdefault_fontset.  If NAME is nil, return the
   fontset of *FRAME.

   Set *FRAME to the actual frame.  */

static Lisp_Object
check_fontset_name (Lisp_Object name, Lisp_Object *frame)
{
  int id;
  struct frame *f = decode_live_frame (*frame);

  XSETFRAME (*frame, f);

  if (EQ (name, Qt))
    return Vdefault_fontset;
  if (NILP (name))
    id = FRAME_FONTSET (f);
  else
    {
      CHECK_STRING (name);
      /* First try NAME as literal.  */
      id = fs_query_fontset (name, 2);
      if (id < 0)
	/* For backward compatibility, try again NAME as pattern.  */
	id = fs_query_fontset (name, 0);
      if (id < 0)
	error ("Fontset `%s' does not exist", SDATA (name));
    }
  return FONTSET_FROM_ID (id);
}

static void
accumulate_script_ranges (Lisp_Object arg, Lisp_Object range, Lisp_Object val)
{
  if (EQ (XCAR (arg), val))
    {
      if (CONSP (range))
	XSETCDR (arg, Fcons (Fcons (XCAR (range), XCDR (range)), XCDR (arg)));
      else
	XSETCDR (arg, Fcons (Fcons (range, range), XCDR (arg)));
    }
}


/* Callback function for map_charset_chars in Fset_fontset_font.
   ARG is a vector [ FONTSET FONT_DEF ADD ASCII SCRIPT_RANGE_LIST ].

   In FONTSET, set FONT_DEF in a fashion specified by ADD for
   characters in RANGE and ranges in SCRIPT_RANGE_LIST before RANGE.
   The consumed ranges are popped up from SCRIPT_RANGE_LIST, and the
   new SCRIPT_RANGE_LIST is stored in ARG.

   If ASCII is nil, don't set FONT_DEF for ASCII characters.  It is
   assured that SCRIPT_RANGE_LIST doesn't contain ASCII in that
   case.  */

static void
set_fontset_font (Lisp_Object arg, Lisp_Object range)
{
  Lisp_Object fontset, font_def, add, ascii, script_range_list;
  int from = XINT (XCAR (range)), to = XINT (XCDR (range));

  fontset = AREF (arg, 0);
  font_def = AREF (arg, 1);
  add = AREF (arg, 2);
  ascii = AREF (arg, 3);
  script_range_list = AREF (arg, 4);

  if (NILP (ascii) && from < 0x80)
    {
      if (to < 0x80)
	return;
      from = 0x80;
      range = Fcons (make_number (0x80), XCDR (range));
    }

#define SCRIPT_FROM XINT (XCAR (XCAR (script_range_list)))
#define SCRIPT_TO XINT (XCDR (XCAR (script_range_list)))
#define POP_SCRIPT_RANGE() script_range_list = XCDR (script_range_list)

  for (; CONSP (script_range_list) && SCRIPT_TO < from; POP_SCRIPT_RANGE ())
    FONTSET_ADD (fontset, XCAR (script_range_list), font_def, add);
  if (CONSP (script_range_list))
    {
      if (SCRIPT_FROM < from)
	range = Fcons (make_number (SCRIPT_FROM), XCDR (range));
      while (CONSP (script_range_list) && SCRIPT_TO <= to)
	POP_SCRIPT_RANGE ();
      if (CONSP (script_range_list) && SCRIPT_FROM <= to)
	XSETCAR (XCAR (script_range_list), make_number (to + 1));
    }

  FONTSET_ADD (fontset, range, font_def, add);
  ASET (arg, 4, script_range_list);
}

static void update_auto_fontset_alist (Lisp_Object, Lisp_Object);


DEFUN ("set-fontset-font", Fset_fontset_font, Sset_fontset_font, 3, 5, 0,
       doc: /*
Modify fontset NAME to use FONT-SPEC for TARGET characters.

NAME is a fontset name string, nil for the fontset of FRAME, or t for
the default fontset.

TARGET may be a single character to use FONT-SPEC for.

Target may be a cons (FROM . TO), where FROM and TO are characters.
In that case, use FONT-SPEC for all characters in the range FROM
and TO (inclusive).

TARGET may be a script name symbol.  In that case, use FONT-SPEC for
all characters that belong to the script.

TARGET may be a charset.  In that case, use FONT-SPEC for all
characters in the charset.

TARGET may be nil.  In that case, use FONT-SPEC for any characters for
that no FONT-SPEC is specified.

FONT-SPEC may one of these:
 * A font-spec object made by the function `font-spec' (which see).
 * A cons (FAMILY . REGISTRY), where FAMILY is a font family name and
   REGISTRY is a font registry name.  FAMILY may contain foundry
   name, and REGISTRY may contain encoding name.
 * A font name string.
 * nil, which explicitly specifies that there's no font for TARGET.

Optional 4th argument FRAME is a frame or nil for the selected frame
that is concerned in the case that NAME is nil.

Optional 5th argument ADD, if non-nil, specifies how to add FONT-SPEC
to the font specifications for TARGET previously set.  If it is
`prepend', FONT-SPEC is prepended.  If it is `append', FONT-SPEC is
appended.  By default, FONT-SPEC overrides the previous settings.  */)
  (Lisp_Object name, Lisp_Object target, Lisp_Object font_spec, Lisp_Object frame, Lisp_Object add)
{
  Lisp_Object fontset;
  Lisp_Object font_def, registry, family;
  Lisp_Object range_list;
  struct charset *charset = NULL;
  Lisp_Object fontname;
  bool ascii_changed = 0;

  fontset = check_fontset_name (name, &frame);

  fontname = Qnil;
  if (CONSP (font_spec))
    {
      Lisp_Object spec = Ffont_spec (0, NULL);

      font_parse_family_registry (XCAR (font_spec), XCDR (font_spec), spec);
      font_spec = spec;
      fontname = Ffont_xlfd_name (font_spec, Qnil);
    }
  else if (STRINGP (font_spec))
    {
      fontname = font_spec;
      font_spec = CALLN (Ffont_spec, QCname, fontname);
    }
  else if (FONT_SPEC_P (font_spec))
    fontname = Ffont_xlfd_name (font_spec, Qnil);
  else if (! NILP (font_spec))
    Fsignal (Qfont, list2 (build_string ("Invalid font-spec"), font_spec));

  if (! NILP (font_spec))
    {
      Lisp_Object encoding, repertory;

      family = AREF (font_spec, FONT_FAMILY_INDEX);
      if (! NILP (family) )
	family = SYMBOL_NAME (family);
      registry = AREF (font_spec, FONT_REGISTRY_INDEX);
      if (! NILP (registry))
	registry = Fdowncase (SYMBOL_NAME (registry));
      AUTO_STRING (dash, "-");
      encoding = find_font_encoding (concat3 (family, dash, registry));
      if (NILP (encoding))
	encoding = Qascii;

      if (SYMBOLP (encoding))
	{
	  CHECK_CHARSET (encoding);
	  encoding = repertory = CHARSET_SYMBOL_ID (encoding);
	}
      else
	{
	  repertory = XCDR (encoding);
	  encoding = XCAR (encoding);
	  CHECK_CHARSET (encoding);
	  encoding = CHARSET_SYMBOL_ID (encoding);
	  if (! NILP (repertory) && SYMBOLP (repertory))
	    {
	      CHECK_CHARSET (repertory);
	      repertory = CHARSET_SYMBOL_ID (repertory);
	    }
	}
      FONT_DEF_NEW (font_def, font_spec, encoding, repertory);
    }
  else
    font_def = Qnil;

  if (CHARACTERP (target))
    {
      if (XFASTINT (target) < 0x80)
	error ("Can't set a font for partial ASCII range");
      range_list = list1 (Fcons (target, target));
    }
  else if (CONSP (target))
    {
      Lisp_Object from, to;

      from = Fcar (target);
      to = Fcdr (target);
      CHECK_CHARACTER (from);
      CHECK_CHARACTER (to);
      if (XFASTINT (from) < 0x80)
	{
	  if (XFASTINT (from) != 0 || XFASTINT (to) < 0x7F)
	    error ("Can't set a font for partial ASCII range");
	  ascii_changed = 1;
	}
      range_list = list1 (target);
    }
  else if (SYMBOLP (target) && !NILP (target))
    {
      Lisp_Object script_list;
      Lisp_Object val;

      range_list = Qnil;
      script_list = XCHAR_TABLE (Vchar_script_table)->extras[0];
      if (! NILP (Fmemq (target, script_list)))
	{
	  if (EQ (target, Qlatin))
	    ascii_changed = 1;
	  val = list1 (target);
	  map_char_table (accumulate_script_ranges, Qnil, Vchar_script_table,
			  val);
	  range_list = Fnreverse (XCDR (val));
	}
      if (CHARSETP (target))
	{
	  CHECK_CHARSET_GET_CHARSET (target, charset);
	  if (charset->ascii_compatible_p)
	    ascii_changed = 1;
	}
      else if (NILP (range_list))
	error ("Invalid script or charset name: %s",
	       SDATA (SYMBOL_NAME (target)));
    }
  else if (NILP (target))
    range_list = list1 (Qnil);
  else
    error ("Invalid target for setting a font");

  if (ascii_changed)
    {
      Lisp_Object val;

      if (NILP (font_spec))
	error ("Can't set ASCII font to nil");
      val = CHAR_TABLE_REF (fontset, 0);
      if (! NILP (val) && EQ (add, Qappend))
	/* We are going to change just an additional font for ASCII.  */
	ascii_changed = 0;
    }

  if (charset)
    {
      Lisp_Object arg;

      arg = make_uninit_vector (5);
      ASET (arg, 0, fontset);
      ASET (arg, 1, font_def);
      ASET (arg, 2, add);
      ASET (arg, 3, ascii_changed ? Qt : Qnil);
      ASET (arg, 4, range_list);

      map_charset_chars (set_fontset_font, Qnil, arg, charset,
			 CHARSET_MIN_CODE (charset),
			 CHARSET_MAX_CODE (charset));
      range_list = AREF (arg, 4);
    }
  for (; CONSP (range_list); range_list = XCDR (range_list))
    FONTSET_ADD (fontset, XCAR (range_list), font_def, add);

  if (ascii_changed)
    {
      Lisp_Object tail, fr;
      int fontset_id = XINT (FONTSET_ID (fontset));

      set_fontset_ascii (fontset, fontname);
      name = FONTSET_NAME (fontset);
      FOR_EACH_FRAME (tail, fr)
	{
	  struct frame *f = XFRAME (fr);
	  Lisp_Object font_object;
	  struct face *face;

	  if (FRAME_INITIAL_P (f) || FRAME_TERMCAP_P (f))
	    continue;
	  if (fontset_id != FRAME_FONTSET (f))
	    continue;
	  face = FACE_FROM_ID_OR_NULL (f, DEFAULT_FACE_ID);
	  if (face)
	    font_object = font_load_for_lface (f, face->lface, font_spec);
	  else
	    font_object = font_open_by_spec (f, font_spec);
	  if (! NILP (font_object))
	    {
	      update_auto_fontset_alist (font_object, fontset);
	      AUTO_FRAME_ARG (arg, Qfont, Fcons (name, font_object));
	      Fmodify_frame_parameters (fr, arg);
	    }
	}
    }

  /* Free all realized fontsets whose base is FONTSET.  This way, the
     specified character(s) are surely redisplayed by a correct
     font.  */
  free_realized_fontsets (fontset);

  return Qnil;
}


DEFUN ("new-fontset", Fnew_fontset, Snew_fontset, 2, 2, 0,
       doc: /* Create a new fontset NAME from font information in FONTLIST.

FONTLIST is an alist of scripts vs the corresponding font specification list.
Each element of FONTLIST has the form (SCRIPT FONT-SPEC ...), where a
character of SCRIPT is displayed by a font that matches one of
FONT-SPEC.

SCRIPT is a symbol that appears in the first extra slot of the
char-table `char-script-table'.

FONT-SPEC is a vector, a cons, or a string.  See the documentation of
`set-fontset-font' for the meaning.  */)
  (Lisp_Object name, Lisp_Object fontlist)
{
  Lisp_Object fontset, tail;
  int id;

  CHECK_STRING (name);

  name = Fdowncase (name);
  id = fs_query_fontset (name, 0);
  if (id < 0)
    {
      Lisp_Object font_spec = Ffont_spec (0, NULL);
      Lisp_Object short_name;
      char xlfd[256];
      int len;

      if (font_parse_xlfd (SSDATA (name), SBYTES (name), font_spec) < 0)
	error ("Fontset name must be in XLFD format");
      short_name = AREF (font_spec, FONT_REGISTRY_INDEX);
      if (strncmp (SSDATA (SYMBOL_NAME (short_name)), "fontset-", 8)
	  || SBYTES (SYMBOL_NAME (short_name)) < 9)
	error ("Registry field of fontset name must be \"fontset-*\"");
      Vfontset_alias_alist = Fcons (Fcons (name, SYMBOL_NAME (short_name)),
				    Vfontset_alias_alist);
      ASET (font_spec, FONT_REGISTRY_INDEX, Qiso8859_1);
      fontset = make_fontset (Qnil, name, Qnil);
      len = font_unparse_xlfd (font_spec, 0, xlfd, 256);
      if (len < 0)
	error ("Invalid fontset name (perhaps too long): %s", SDATA (name));
      set_fontset_ascii (fontset, make_unibyte_string (xlfd, len));
    }
  else
    {
      fontset = FONTSET_FROM_ID (id);
      free_realized_fontsets (fontset);
      Fset_char_table_range (fontset, Qt, Qnil);
    }

  for (tail = fontlist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt, script;

      elt = XCAR (tail);
      script = Fcar (elt);
      elt = Fcdr (elt);
      if (CONSP (elt) && (NILP (XCDR (elt)) || CONSP (XCDR (elt))))
	for (; CONSP (elt); elt = XCDR (elt))
	  Fset_fontset_font (name, script, XCAR (elt), Qnil, Qappend);
      else
	Fset_fontset_font (name, script, elt, Qnil, Qappend);
    }
  CHECK_LIST_END (tail, fontlist);
  return name;
}


/* Alist of automatically created fontsets.  Each element is a cons
   (FONT-SPEC . FONTSET-ID).  */
static Lisp_Object auto_fontset_alist;

/* Number of automatically created fontsets.  */
static ptrdiff_t num_auto_fontsets;

/* Return a fontset synthesized from FONT-OBJECT.  This is called from
   x_new_font when FONT-OBJECT is used for the default ASCII font of a
   frame, and the returned fontset is used for the default fontset of
   that frame.  The fontset specifies a font of the same registry as
   FONT-OBJECT for all characters in the repertory of the registry
   (see Vfont_encoding_alist).  If the repertory is not known, the
   fontset specifies the font for all Latin characters assuming that a
   user intends to use FONT-OBJECT for Latin characters.  */

int
fontset_from_font (Lisp_Object font_object)
{
  Lisp_Object font_name = font_get_name (font_object);
  Lisp_Object font_spec = copy_font_spec (font_object);
  Lisp_Object registry = AREF (font_spec, FONT_REGISTRY_INDEX);
  Lisp_Object fontset_spec, alias, name, fontset;
  Lisp_Object val;

  val = assoc_no_quit (font_spec, auto_fontset_alist);
  if (CONSP (val))
    return XINT (FONTSET_ID (XCDR (val)));
  if (num_auto_fontsets++ == 0)
    alias = intern ("fontset-startup");
  else
    {
      char temp[sizeof "fontset-auto" + INT_STRLEN_BOUND (ptrdiff_t)];

      sprintf (temp, "fontset-auto%"pD"d", num_auto_fontsets - 1);
      alias = intern (temp);
    }
  fontset_spec = copy_font_spec (font_spec);
  ASET (fontset_spec, FONT_REGISTRY_INDEX, alias);
  name = Ffont_xlfd_name (fontset_spec, Qnil);
  eassert (!NILP (name));
  fontset = make_fontset (Qnil, name, Qnil);
  Vfontset_alias_alist = Fcons (Fcons (name, SYMBOL_NAME (alias)),
				Vfontset_alias_alist);
  alias = Fdowncase (AREF (font_object, FONT_NAME_INDEX));
  Vfontset_alias_alist = Fcons (Fcons (name, alias), Vfontset_alias_alist);
  auto_fontset_alist = Fcons (Fcons (font_spec, fontset), auto_fontset_alist);
  font_spec = Ffont_spec (0, NULL);
  ASET (font_spec, FONT_REGISTRY_INDEX, registry);
  {
    Lisp_Object target = find_font_encoding (SYMBOL_NAME (registry));

    if (CONSP (target))
      target = XCDR (target);
    if (! CHARSETP (target))
      target = Qlatin;
    Fset_fontset_font (name, target, font_spec, Qnil, Qnil);
    Fset_fontset_font (name, Qnil, font_spec, Qnil, Qnil);
  }

  set_fontset_ascii (fontset, font_name);

  return XINT (FONTSET_ID (fontset));
}


/* Update auto_fontset_alist for FONTSET.  When an ASCII font of
   FONTSET is changed, we delete an entry of FONTSET if any from
   auto_fontset_alist so that FONTSET is not re-used by
   fontset_from_font.  */

static void
update_auto_fontset_alist (Lisp_Object font_object, Lisp_Object fontset)
{
  Lisp_Object prev, tail;

  for (prev = Qnil, tail = auto_fontset_alist; CONSP (tail);
       prev = tail, tail = XCDR (tail))
    if (EQ (fontset, XCDR (XCAR (tail))))
      {
	if (NILP (prev))
	  auto_fontset_alist = XCDR (tail);
	else
	  XSETCDR (prev, XCDR (tail));
	break;
      }
}


DEFUN ("fontset-info", Ffontset_info, Sfontset_info, 1, 2, 0,
       doc: /* Return information about a fontset FONTSET on frame FRAME.

FONTSET is a fontset name string, nil for the fontset of FRAME, or t
for the default fontset.  FRAME nil means the selected frame.

The value is a char-table whose elements have this form:

    ((FONT OPENED-FONT ...) ...)

FONT is a name of font specified for a range of characters.

OPENED-FONT is a name of a font actually opened.

The char-table has one extra slot.  If FONTSET is not the default
fontset, the value the extra slot is a char-table containing the
information about the derived fonts from the default fontset.  The
format is the same as above.  */)
  (Lisp_Object fontset, Lisp_Object frame)
{
  Lisp_Object *realized[2], fontsets[2], tables[2];
  Lisp_Object val, elt;
  int c, i, j, k;

  check_window_system (NULL);
  fontset = check_fontset_name (fontset, &frame);

  /* Recode fontsets realized on FRAME from the base fontset FONTSET
     in the table `realized'.  */
  USE_SAFE_ALLOCA;
  SAFE_ALLOCA_LISP (realized[0], 2 * ASIZE (Vfontset_table));
  realized[1] = realized[0] + ASIZE (Vfontset_table);
  for (i = j = 0; i < ASIZE (Vfontset_table); i++)
    {
      elt = FONTSET_FROM_ID (i);
      if (!NILP (elt)
	  && EQ (FONTSET_BASE (elt), fontset)
	  && EQ (FONTSET_FRAME (elt), frame))
	realized[0][j++] = elt;
    }
  realized[0][j] = Qnil;

  for (i = j = 0; ! NILP (realized[0][i]); i++)
    {
      elt = FONTSET_DEFAULT (realized[0][i]);
      if (! NILP (elt))
	realized[1][j++] = elt;
    }
  realized[1][j] = Qnil;

  tables[0] = Fmake_char_table (Qfontset_info, Qnil);
  fontsets[0] = fontset;
  if (!EQ (fontset, Vdefault_fontset))
    {
      tables[1] = Fmake_char_table (Qnil, Qnil);
      set_char_table_extras (tables[0], 0, tables[1]);
      fontsets[1] = Vdefault_fontset;
    }

  /* Accumulate information of the fontset in TABLE.  The format of
     each element is ((FONT-SPEC OPENED-FONT ...) ...).  */
  for (k = 0; k <= 1; k++)
    {
      for (c = 0; c <= MAX_CHAR; )
	{
	  int from = c, to = MAX_5_BYTE_CHAR;

	  if (c <= MAX_5_BYTE_CHAR)
	    {
	      val = char_table_ref_and_range (fontsets[k], c, &from, &to);
	    }
	  else
	    {
	      val = FONTSET_FALLBACK (fontsets[k]);
	      to = MAX_CHAR;
	    }
	  if (VECTORP (val))
	    {
	      Lisp_Object alist;

	      /* At first, set ALIST to ((FONT-SPEC) ...).  */
	      for (alist = Qnil, i = 0; i < ASIZE (val); i++)
		if (! NILP (AREF (val, i)))
		  alist = Fcons (Fcons (FONT_DEF_SPEC (AREF (val, i)), Qnil),
				 alist);
	      alist = Fnreverse (alist);

	      /* Then store opened font names to cdr of each elements.  */
	      for (i = 0; ! NILP (realized[k][i]); i++)
		{
		  if (c <= MAX_5_BYTE_CHAR)
		    val = FONTSET_REF (realized[k][i], c);
		  else
		    val = FONTSET_FALLBACK (realized[k][i]);
		  if (! CONSP (val) || ! VECTORP (XCDR (val)))
		    continue;
		  /* VAL: (int . [[FACE-ID FONT-DEF FONT-OBJECT int] ... ])  */
		  val = XCDR (val);
		  for (j = 0; j < ASIZE (val); j++)
		    {
		      elt = AREF (val, j);
		      if (!NILP (elt) && FONT_OBJECT_P (RFONT_DEF_OBJECT (elt)))
			{
			  Lisp_Object font_object = RFONT_DEF_OBJECT (elt);
			  Lisp_Object slot, name;

			  slot = Fassq (RFONT_DEF_SPEC (elt), alist);
			  name = AREF (font_object, FONT_NAME_INDEX);
			  if (NILP (Fmember (name, XCDR (slot))))
			    nconc2 (slot, list1 (name));
			}
		    }
		}

	      /* Store ALIST in TBL for characters C..TO.  */
	      if (c <= MAX_5_BYTE_CHAR)
		char_table_set_range (tables[k], c, to, alist);
	      else
		set_char_table_defalt (tables[k], alist);

	      /* At last, change each elements to font names.  */
	      for (; CONSP (alist); alist = XCDR (alist))
		{
		  elt = XCAR (alist);
		  XSETCAR (elt, Ffont_xlfd_name (XCAR (elt), Qnil));
		}
	    }
	  c = to + 1;
	}
      if (EQ (fontset, Vdefault_fontset))
	break;
    }

  SAFE_FREE ();
  return tables[0];
}


DEFUN ("fontset-font", Ffontset_font, Sfontset_font, 2, 3, 0,
       doc: /* Return a font name pattern for character CH in fontset NAME.
If NAME is t, find a pattern in the default fontset.
If NAME is nil, find a pattern in the fontset of the selected frame.

The value has the form (FAMILY . REGISTRY), where FAMILY is a font
family name and REGISTRY is a font registry name.  This is actually
the first font name pattern for CH in the fontset or in the default
fontset.

If the 2nd optional arg ALL is non-nil, return a list of all font name
patterns.  */)
  (Lisp_Object name, Lisp_Object ch, Lisp_Object all)
{
  int c;
  Lisp_Object fontset, elt, list, repertory, val;
  int i, j;
  Lisp_Object frame;

  frame = Qnil;
  fontset = check_fontset_name (name, &frame);

  CHECK_CHARACTER (ch);
  c = XINT (ch);
  list = Qnil;
  while (1)
    {
      for (i = 0, elt = FONTSET_REF (fontset, c); i < 2;
	   i++, elt = FONTSET_FALLBACK (fontset))
	if (VECTORP (elt))
	  for (j = 0; j < ASIZE (elt); j++)
	    {
	      Lisp_Object family, registry;

	      val = AREF (elt, j);
	      if (NILP (val))
		return Qnil;
	      repertory = AREF (val, 1);
	      if (INTEGERP (repertory))
		{
		  struct charset *charset = CHARSET_FROM_ID (XINT (repertory));

		  if (! CHAR_CHARSET_P (c, charset))
		    continue;
		}
	      else if (CHAR_TABLE_P (repertory))
		{
		  if (NILP (CHAR_TABLE_REF (repertory, c)))
		    continue;
		}
	      val = AREF (val, 0);
	      /* VAL is a FONT-SPEC */
	      family = AREF (val, FONT_FAMILY_INDEX);
	      if (! NILP (family))
		family = SYMBOL_NAME (family);
	      registry = AREF (val, FONT_REGISTRY_INDEX);
	      if (! NILP (registry))
		registry = SYMBOL_NAME (registry);
	      val = Fcons (family, registry);
	      if (NILP (all))
		return val;
	      list = Fcons (val, list);
	    }
      if (EQ (fontset, Vdefault_fontset))
	break;
      fontset = Vdefault_fontset;
    }
  return (Fnreverse (list));
}

DEFUN ("fontset-list", Ffontset_list, Sfontset_list, 0, 0, 0,
       doc: /* Return a list of all defined fontset names.  */)
  (void)
{
  Lisp_Object fontset, list;
  int i;

  list = Qnil;
  for (i = 0; i < ASIZE (Vfontset_table); i++)
    {
      fontset = FONTSET_FROM_ID (i);
      if (!NILP (fontset)
	  && BASE_FONTSET_P (fontset))
	list = Fcons (FONTSET_NAME (fontset), list);
    }

  return list;
}


#ifdef ENABLE_CHECKING

Lisp_Object dump_fontset (Lisp_Object) EXTERNALLY_VISIBLE;

Lisp_Object
dump_fontset (Lisp_Object fontset)
{
  Lisp_Object vec;

  vec = Fmake_vector (make_number (3), Qnil);
  ASET (vec, 0, FONTSET_ID (fontset));

  if (BASE_FONTSET_P (fontset))
    {
      ASET (vec, 1, FONTSET_NAME (fontset));
    }
  else
    {
      Lisp_Object frame;

      frame = FONTSET_FRAME (fontset);
      if (FRAMEP (frame))
	{
	  struct frame *f = XFRAME (frame);

	  if (FRAME_LIVE_P (f))
	    ASET (vec, 1,
		  Fcons (FONTSET_NAME (FONTSET_BASE (fontset)),
			 f->name));
	  else
	    ASET (vec, 1,
		  Fcons (FONTSET_NAME (FONTSET_BASE (fontset)), Qnil));
	}
      if (!NILP (FONTSET_DEFAULT (fontset)))
	ASET (vec, 2, FONTSET_ID (FONTSET_DEFAULT (fontset)));
    }
  return vec;
}

DEFUN ("fontset-list-all", Ffontset_list_all, Sfontset_list_all, 0, 0, 0,
       doc: /* Return a brief summary of all fontsets for debug use.  */)
  (void)
{
  Lisp_Object val;
  int i;

  for (i = 0, val = Qnil; i < ASIZE (Vfontset_table); i++)
    if (! NILP (AREF (Vfontset_table, i)))
      val = Fcons (dump_fontset (AREF (Vfontset_table, i)), val);
  return (Fnreverse (val));
}
#endif	/* ENABLE_CHECKING */

void
syms_of_fontset (void)
{
  DEFSYM (Qfontset, "fontset");
  Fput (Qfontset, Qchar_table_extra_slots, make_number (8));
  DEFSYM (Qfontset_info, "fontset-info");
  Fput (Qfontset_info, Qchar_table_extra_slots, make_number (1));

  DEFSYM (Qappend, "append");
  DEFSYM (Qlatin, "latin");

  Vcached_fontset_data = Qnil;
  staticpro (&Vcached_fontset_data);

  Vfontset_table = Fmake_vector (make_number (32), Qnil);
  staticpro (&Vfontset_table);

  Vdefault_fontset = Fmake_char_table (Qfontset, Qnil);
  staticpro (&Vdefault_fontset);
  set_fontset_id (Vdefault_fontset, make_number (0));
  set_fontset_name
    (Vdefault_fontset,
     build_pure_c_string ("-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default"));
  ASET (Vfontset_table, 0, Vdefault_fontset);
  next_fontset_id = 1;

  auto_fontset_alist = Qnil;
  staticpro (&auto_fontset_alist);

  DEFVAR_LISP ("font-encoding-charset-alist", Vfont_encoding_charset_alist,
	       doc: /*
Alist of charsets vs the charsets to determine the preferred font encoding.
Each element looks like (CHARSET . ENCODING-CHARSET),
where ENCODING-CHARSET is a charset registered in the variable
`font-encoding-alist' as ENCODING.

When a text has a property `charset' and the value is CHARSET, a font
whose encoding corresponds to ENCODING-CHARSET is preferred.  */);
  Vfont_encoding_charset_alist = Qnil;

  DEFVAR_LISP ("use-default-ascent", Vuse_default_ascent,
	       doc: /*
Char table of characters whose ascent values should be ignored.
If an entry for a character is non-nil, the ascent value of the glyph
is assumed to be specified by _MULE_DEFAULT_ASCENT property of a font.

This affects how a composite character which contains
such a character is displayed on screen.  */);
  Vuse_default_ascent = Qnil;

  DEFVAR_BOOL ("use-default-font-for-symbols", use_default_font_for_symbols,
	       doc: /*
If non-nil, use the default face's font for symbols and punctuation.

By default, Emacs will try to use the default face's font for
displaying symbol and punctuation characters, disregarding the
fontsets, if the default font can display the character.
Set this to nil to make Emacs honor the fontsets instead.  */);
  use_default_font_for_symbols = 1;

  DEFVAR_LISP ("ignore-relative-composition", Vignore_relative_composition,
	       doc: /*
Char table of characters which are not composed relatively.
If an entry for a character is non-nil, a composition sequence
which contains that character is displayed so that
the glyph of that character is put without considering
an ascent and descent value of a previous character.  */);
  Vignore_relative_composition = Qnil;

  DEFVAR_LISP ("alternate-fontname-alist", Valternate_fontname_alist,
	       doc: /* Alist of fontname vs list of the alternate fontnames.
When a specified font name is not found, the corresponding
alternate fontnames (if any) are tried instead.  */);
  Valternate_fontname_alist = Qnil;

  DEFVAR_LISP ("fontset-alias-alist", Vfontset_alias_alist,
	       doc: /* Alist of fontset names vs the aliases.  */);
  Vfontset_alias_alist
    = list1 (Fcons (FONTSET_NAME (Vdefault_fontset),
		    build_pure_c_string ("fontset-default")));

  DEFVAR_LISP ("vertical-centering-font-regexp",
	       Vvertical_centering_font_regexp,
	       doc: /* Regexp matching font names that require vertical centering on display.
When a character is displayed with such fonts, the character is displayed
at the vertical center of lines.  */);
  Vvertical_centering_font_regexp = Qnil;

  DEFVAR_LISP ("otf-script-alist", Votf_script_alist,
	       doc: /* Alist of OpenType script tags vs the corresponding script names.  */);
  Votf_script_alist = Qnil;

  defsubr (&Squery_fontset);
  defsubr (&Snew_fontset);
  defsubr (&Sset_fontset_font);
  defsubr (&Sfontset_info);
  defsubr (&Sfontset_font);
  defsubr (&Sfontset_list);
#ifdef ENABLE_CHECKING
  defsubr (&Sfontset_list_all);
#endif
}
