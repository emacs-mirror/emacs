/* Fontset handler.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
     Free Software Foundation, Inc.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003, 2006
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009
 
This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* #define FONTSET_DEBUG */

#include <config.h>

#ifdef FONTSET_DEBUG
#include <stdio.h>
#endif

#include "lisp.h"
#include "blockinput.h"
#include "buffer.h"
#include "character.h"
#include "charset.h"
#include "ccl.h"
#include "keyboard.h"
#include "frame.h"
#include "dispextern.h"
#include "intervals.h"
#include "fontset.h"
#include "window.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif
#ifdef WINDOWSNT
#include "w32term.h"
#endif
#ifdef MAC_OS
#include "macterm.h"
#endif
#include "termhooks.h"

#include "font.h"

#undef xassert
#ifdef FONTSET_DEBUG
#define xassert(X)	do {if (!(X)) abort ();} while (0)
#undef INLINE
#define INLINE
#else   /* not FONTSET_DEBUG */
#define xassert(X)	(void) 0
#endif	/* not FONTSET_DEBUG */

EXFUN (Fclear_face_cache, 1);

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

   An element of a base fontset is a vector of FONT-DEFs which itself
   is a vector [ FONT-SPEC ENCODING REPERTORY ].

   An element of a realized fontset is nil, t, or a vector of this form:

	[ CHARSET-ORDERED-LIST-TICK PREFERRED-RFONT-DEF
	  RFONT-DEF0 RFONT-DEF1 ... ]

   RFONT-DEFn (i.e. Realized FONT-DEF) has this form:

	[ FACE-ID FONT-DEF FONT-OBJECT SORTING-SCORE ]

   RFONT-DEFn are automatically reordered by the current charset
   priority list.

   The value nil means that we have not yet generated the above vector
   from the base of the fontset.

   The value t means that no font is available for the corresponding
   range of characters.


   A fontset has 9 extra slots.

   The 1st slot: the ID number of the fontset

   The 2nd slot:
	base: the name of the fontset
	realized: nil

   The 3rd slot:
	base: nil
	realized: the base fontset

   The 4th slot:
	base: nil
	realized: the frame that the fontset belongs to

   The 5th slot:
	base: the font name for ASCII characters
	realized: nil

   The 6th slot:
	base: nil
	realized: the ID number of a face to use for characters that
		  has no font in a realized fontset.

   The 7th slot:
	base: nil
	realized: Alist of font index vs the corresponding repertory
	char-table.
	
   The 8th slot:
	base: nil
	realized: If the base is not the default fontset, a fontset
	realized from the default fontset, else nil.

   The 9th slot:
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
   file, we always use varialbe name `id' for IDs, and name `fontset'
   for an actual fontset object, i.e., char-table.

*/

/********** VARIABLES and FUNCTION PROTOTYPES **********/

extern Lisp_Object Qfont;
static Lisp_Object Qfontset;
static Lisp_Object Qfontset_info;
static Lisp_Object Qprepend, Qappend;
static Lisp_Object Qlatin;

/* Vector containing all fontsets.  */
static Lisp_Object Vfontset_table;

/* Next possibly free fontset ID.  Usually this keeps the minimum
   fontset ID not yet used.  */
static int next_fontset_id;

/* The default fontset.  This gives default FAMILY and REGISTRY of
   font for each character.  */
static Lisp_Object Vdefault_fontset;

Lisp_Object Vfont_encoding_alist;
Lisp_Object Vfont_encoding_charset_alist;
Lisp_Object Vuse_default_ascent;
Lisp_Object Vignore_relative_composition;
Lisp_Object Valternate_fontname_alist;
Lisp_Object Vfontset_alias_alist;
Lisp_Object Vvertical_centering_font_regexp;
Lisp_Object Votf_script_alist;

/* Check if any window system is used now.  */
void (*check_window_system_func) P_ ((void));


/* Prototype declarations for static functions.  */
static Lisp_Object fontset_add P_ ((Lisp_Object, Lisp_Object, Lisp_Object,
				    Lisp_Object));
static Lisp_Object fontset_find_font P_ ((Lisp_Object, int, struct face *,
					  int, int));
static void reorder_font_vector P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object fontset_font P_ ((Lisp_Object, int, struct face *, int));
static Lisp_Object make_fontset P_ ((Lisp_Object, Lisp_Object, Lisp_Object));
static Lisp_Object fontset_pattern_regexp P_ ((Lisp_Object));
static void accumulate_script_ranges P_ ((Lisp_Object, Lisp_Object,
					  Lisp_Object));
Lisp_Object find_font_encoding P_ ((Lisp_Object));

static void set_fontset_font P_ ((Lisp_Object, Lisp_Object));

#ifdef FONTSET_DEBUG

/* Return 1 if ID is a valid fontset id, else return 0.  */

static int
fontset_id_valid_p (id)
     int id;
{
  return (id >= 0 && id < ASIZE (Vfontset_table) - 1);
}

#endif



/********** MACROS AND FUNCTIONS TO HANDLE FONTSET **********/

/* Return the fontset with ID.  No check of ID's validness.  */
#define FONTSET_FROM_ID(id) AREF (Vfontset_table, id)

/* Macros to access special values of FONTSET.  */
#define FONTSET_ID(fontset)		XCHAR_TABLE (fontset)->extras[0]

/* Macros to access special values of (base) FONTSET.  */
#define FONTSET_NAME(fontset)		XCHAR_TABLE (fontset)->extras[1]
#define FONTSET_ASCII(fontset)		XCHAR_TABLE (fontset)->extras[4]
#define FONTSET_SPEC(fontset)		XCHAR_TABLE (fontset)->extras[5]

/* Macros to access special values of (realized) FONTSET.  */
#define FONTSET_BASE(fontset)		XCHAR_TABLE (fontset)->extras[2]
#define FONTSET_FRAME(fontset)		XCHAR_TABLE (fontset)->extras[3]
#define FONTSET_OBJLIST(fontset)	XCHAR_TABLE (fontset)->extras[4]
#define FONTSET_NOFONT_FACE(fontset)	XCHAR_TABLE (fontset)->extras[5]
#define FONTSET_REPERTORY(fontset)	XCHAR_TABLE (fontset)->extras[6]
#define FONTSET_DEFAULT(fontset)	XCHAR_TABLE (fontset)->extras[7]

/* For both base and realized fontset.  */
#define FONTSET_FALLBACK(fontset)	XCHAR_TABLE (fontset)->extras[8]

#define BASE_FONTSET_P(fontset)		(NILP (FONTSET_BASE (fontset)))


/* Macros for FONT-DEF and RFONT-DEF of fontset.  */
#define FONT_DEF_NEW(font_def, font_spec, encoding, repertory)	\
  do {								\
    (font_def) = Fmake_vector (make_number (3), (font_spec));	\
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
#define RFONT_DEF_REPERTORY(rfont_def) FONT_DEF_REPERTORY (AREF (rfont_def, 1))
#define RFONT_DEF_OBJECT(rfont_def) AREF (rfont_def, 2)
#define RFONT_DEF_SET_OBJECT(rfont_def, object)	\
  ASET ((rfont_def), 2, (object))
#define RFONT_DEF_SCORE(rfont_def) XINT (AREF (rfont_def, 3))
#define RFONT_DEF_SET_SCORE(rfont_def, score) \
  ASET ((rfont_def), 3, make_number (score))
#define RFONT_DEF_NEW(rfont_def, font_def)		\
  do {							\
    (rfont_def) = Fmake_vector (make_number (4), Qnil);	\
    ASET ((rfont_def), 1, (font_def));		\
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
fontset_ref (fontset, c)
     Lisp_Object fontset;
     int c;
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

#define FONTSET_ADD(fontset, range, elt, add)				     \
  (NILP (add)								     \
   ? (NILP (range)							     \
      ? (FONTSET_FALLBACK (fontset) = Fmake_vector (make_number (1), (elt))) \
      : Fset_char_table_range ((fontset), (range),			     \
			       Fmake_vector (make_number (1), (elt))))	     \
   : fontset_add ((fontset), (range), (elt), (add)))

static Lisp_Object
fontset_add (fontset, range, elt, add)
     Lisp_Object fontset, range, elt, add;
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
	args[idx] = char_table_ref_and_range (fontset, from, &from1, &to1);
	if (to < to1)
	  to1 = to;
	char_table_set_range (fontset, from, to1,
			      NILP (args[idx]) ? args[1 - idx]
			      : Fvconcat (2, args));
	from = to1 + 1;
      } while (from < to);
    }
  else
    {
      args[idx] = FONTSET_FALLBACK (fontset);
      FONTSET_FALLBACK (fontset)
	= NILP (args[idx]) ? args[1 - idx] : Fvconcat (2, args);
    }
  return Qnil;
}

static int
fontset_compare_rfontdef (val1, val2)
     const void *val1, *val2;
{
  return (RFONT_DEF_SCORE (*(Lisp_Object *) val2)
	  - RFONT_DEF_SCORE (*(Lisp_Object *) val1));
}

/* Update FONT-GROUP which has this form:
	[ CHARSET-ORDERED-LIST-TICK PREFERRED-RFONT-DEF
	  RFONT-DEF0 RFONT-DEF1 ... ]
   Reorder RFONT-DEFs according to the current langauge, and update
   CHARSET-ORDERED-LIST-TICK.

   If PREFERRED_FAMILY is not nil, that family has the higher priority
   if the encoding charsets or languages in font-specs are the same.  */

extern Lisp_Object Fassoc_string ();

static void
reorder_font_vector (font_group, preferred_family)
     Lisp_Object font_group;
     Lisp_Object preferred_family;
{
  int size;
  int i;
  int score_changed = 0;

  size = ASIZE (font_group);
  /* Exclude the tailing nil elements from the reordering.  */
  while (NILP (AREF (font_group, size - 1))) size--;
  size -= 2;

  for (i = 0; i < size; i++)
    {
      Lisp_Object rfont_def = AREF (font_group, i + 2);
      Lisp_Object font_def = RFONT_DEF_FONT_DEF (rfont_def);
      Lisp_Object font_spec = FONT_DEF_SPEC (font_def);
      Lisp_Object lang = Ffont_get (font_spec, QClang);
      Lisp_Object family = AREF (font_spec, FONT_FAMILY_INDEX);
      Lisp_Object repertory = FONT_DEF_REPERTORY (font_def);
      int score = 0;

      if (! NILP (repertory))
	{  
	  Lisp_Object tail;

	  for (score = 0xFFFF, tail = Vcharset_ordered_list;
	       ! EQ (tail, Vcharset_non_preferred_head) && CONSP (tail);
	       score--, tail = XCDR (tail))
	    if (EQ (repertory, XCAR (tail)))
	      break;
	  if (EQ (tail, Vcharset_non_preferred_head))
	    score = 0;
	}
      else if (! NILP (lang))
	{
	  if (EQ (lang, Vcurrent_iso639_language))
	    score = 0xFFFF;
	  else if (CONSP (Vcurrent_iso639_language))
	    score = ! NILP (Fmemq (lang, Vcurrent_iso639_language));
	}

      if (! NILP (preferred_family) && ! NILP (family))
	{
	  if (fast_string_match_ignore_case (preferred_family,
					     SYMBOL_NAME (family)) < 0)
	    score |= 0x10000;
	}
      if (RFONT_DEF_SCORE (rfont_def) != score)
	{
	  RFONT_DEF_SET_SCORE (rfont_def, score);
	  score_changed = 1;
	}
    }

  if (score_changed)
    qsort (XVECTOR (font_group)->contents + 2, size, sizeof (Lisp_Object),
	   fontset_compare_rfontdef);

  ASET (font_group, 0, make_number (charset_ordered_list_tick));
}

/* Return RFONT-DEF (vector) in the realized fontset FONTSET for the
   character C.  If no font is found, return Qnil if there's a
   possibility that the default fontset or the fallback font groups
   have a proper font, and return Qt if not.

   If a font is found but is not yet opened, open it (if FACE is not
   NULL) or return Qnil (if FACE is NULL).

   ID is a charset-id that must be preferred, or -1 meaning no
   preference.

   If FALLBACK is nonzero, search only fallback fonts.  */

static Lisp_Object
fontset_find_font (fontset, c, face, id, fallback)
     Lisp_Object fontset;
     int c;
     struct face *face;
     int id, fallback;
{
  Lisp_Object base_fontset, elt, vec;
  int i, from, to;
  FRAME_PTR f = XFRAME (FONTSET_FRAME (fontset));

  base_fontset = FONTSET_BASE (fontset);
  if (! fallback)
    vec = CHAR_TABLE_REF (fontset, c);
  else
    vec = FONTSET_FALLBACK (fontset);

  if (NILP (vec))
    {
      Lisp_Object range;

      /* We have not yet decided a font for C.  */
      if (! face)
	return Qnil;
      if (! fallback)
	{
	  elt = char_table_ref_and_range (base_fontset, c, &from, &to);
	  range = Fcons (make_number (from), make_number (to));
	}
      else
	{
	  elt = FONTSET_FALLBACK (base_fontset);
	}
      if (NILP (elt))
	{
	  /* This fontset doesn't specify any font for C. */
	  vec = make_number (0);
	}
      else if (ASIZE (elt) == 1 && NILP (AREF (elt, 0)))
	{
	  /* Explicitly specified no font.  */
	  vec = Qt;
	}
      else
	{
	  /* Build a font-group vector [ -1 nil RFONT-DEF0 RFONT-DEF1 ... ],
	     where the first -1 is to force reordering of RFONT-DEFs.  */
	  int size = ASIZE (elt);
	  int j;

	  vec = Fmake_vector (make_number (size + 2), Qnil);
	  ASET (vec, 0, make_number (-1));
	  for (i = j = 0; i < size; i++)
	    if (! NILP (AREF (elt, i)))
	      {
		Lisp_Object rfont_def;

		RFONT_DEF_NEW (rfont_def, AREF (elt, i));
		ASET (vec, j + 2, rfont_def);
		j++;
	      }
	}
      /* Then store it in the fontset.  */
      if (! fallback)
	FONTSET_SET (fontset, range, vec);
      else
	FONTSET_FALLBACK (fontset) = vec;

    }
  if (! VECTORP (vec))
    return (EQ (vec, Qt) ? Qt : Qnil);

  if (ASIZE (vec) > 4		/* multiple RFONT-DEFs */
      && XINT (AREF (vec, 0)) != charset_ordered_list_tick)
    /* We have just created VEC,
       or the charset priorities were changed.  */
    reorder_font_vector (vec, face->lface[LFACE_FAMILY_INDEX]);
  i = 2;
  if (id >= 0)
    {
      elt = AREF (vec, 1);
      if (! NILP (elt)
	  && EQ (make_number (id), RFONT_DEF_REPERTORY (elt)))
	i = 1;
      else
	{
	  for (; i < ASIZE (vec); i++)
	    {
	      elt = AREF (vec, i);
	      if (! NILP (elt)
		  && EQ (make_number (id), RFONT_DEF_REPERTORY (elt)))
		break;
	    }
	  if (i < ASIZE (vec))
	    {
	      ASET (vec, 1, elt);
	      i = 1;
	    }
	  else
	    {
	      ASET (vec, 1, Qnil);
	      i = 2;
	    }
	}
    }

  /* Find the first available font in the vector of RFONT-DEF.  */
  for (; i < ASIZE (vec); i++)
    {
      Lisp_Object font_def, font_entity, font_object;

      elt = AREF (vec, i);
      if (NILP (elt))
	/* This is the sign of not to try fallback fonts.  */
	return Qt;
      if (id >= 0 && i > 1 && EQ (AREF (vec, 1), elt))
	/* This is already checked.  */
	continue;
      if (INTEGERP (RFONT_DEF_FACE (elt))
	  && XINT (AREF (elt, 1)) < 0)
	/* We couldn't open this font last time.  */
	continue;

      font_object = RFONT_DEF_OBJECT (elt);
      if (NILP (font_object))
	{
	  Lisp_Object font_def = RFONT_DEF_FONT_DEF (elt);

	  if (! face)
	    /* We have not yet opened the font.  */
	    return Qnil;
	  font_entity = font_find_for_lface (f, face->lface,
					     FONT_DEF_SPEC (font_def), -1);
	  if (NILP (font_entity))
	    {
	      /* Record that no font matches the spec.  */
	      RFONT_DEF_SET_FACE (elt, -1);
	      continue;
	    }
	  font_object = font_open_for_lface (f, font_entity, face->lface, Qnil);
	  if (NILP (font_object))
	    {
	      /* Record that the font is unsable.  */
	      RFONT_DEF_SET_FACE (elt, -1);
	      continue;
	    }
	  RFONT_DEF_SET_OBJECT (elt, font_object);
	}
      
      if (font_has_char (f, font_object, c))
	return elt;

#if 0
      /* The following code makes Emacs to find a font for C by fairly
	 exhausitive search.  But, that takes long time especially for
	 X font backend.  */

      /* Try to find the different font maching with the current spec
	 and support C. */
      font_def = RFONT_DEF_FONT_DEF (elt);
      for (i++; i < ASIZE (vec); i++)
	{
	  if (! EQ (RFONT_DEF_FONT_DEF (AREF (vec, i)), font_def))
	    break;
	  if (font_has_char (f, RFONT_DEF_OBJECT (AREF (vec, i)), c))
	    return AREF (vec, i);
	}
      /* Find an font-entity that support C.  */
      font_entity = font_find_for_lface (f, face->lface,
					 FONT_DEF_SPEC (font_def), c);
      if (! NILP (font_entity))
	{
	  Lisp_Object rfont_def, new_vec;
	  int j;

	  font_object = font_open_for_lface (f, font_entity, face->lface,
					     Qnil);
	  RFONT_DEF_NEW (rfont_def, font_def);
	  RFONT_DEF_SET_OBJECT (rfont_def, font_object);
	  RFONT_DEF_SET_SCORE (rfont_def, RFONT_DEF_SCORE (elt));
	  new_vec = Fmake_vector (make_number (ASIZE (vec) + 1), Qnil);
	  for (j = 0; j < i; j++)
	    ASET (new_vec, j, AREF (vec, j));
	  ASET (new_vec, j, rfont_def);
	  for (j++; j < ASIZE (new_vec); j++)
	    ASET (new_vec, j, AREF (vec, j - 1));
	  vec = new_vec;
	  return rfont_def;
	}
      i--;
#endif	/* 0 */
    }

  FONTSET_SET (fontset, make_number (c), make_number (0));
  return Qnil;
}


static Lisp_Object
fontset_font (fontset, c, face, id)
     Lisp_Object fontset;
     int c;
     struct face *face;
     int id;
{
  Lisp_Object rfont_def;
  Lisp_Object base_fontset;

  /* Try a font-group for C. */
  rfont_def = fontset_find_font (fontset, c, face, id, 0);
  if (VECTORP (rfont_def))
    return rfont_def;
  if (EQ (rfont_def, Qt))
    return Qnil;
  base_fontset = FONTSET_BASE (fontset);
  /* Try a font-group for C of the default fontset. */
  if (! EQ (base_fontset, Vdefault_fontset))
    {
      if (NILP (FONTSET_DEFAULT (fontset)))
	FONTSET_DEFAULT (fontset)
	  = make_fontset (FONTSET_FRAME (fontset), Qnil, Vdefault_fontset);
      rfont_def = fontset_find_font (FONTSET_DEFAULT (fontset), c, face, id, 0);
      if (VECTORP (rfont_def))
	return (rfont_def);
    }

  /* Try a fallback font-group. */
  rfont_def = fontset_find_font (fontset, c, face, id, 1);
  if (! VECTORP (rfont_def)
      && ! EQ (base_fontset, Vdefault_fontset))
    /* Try a fallback font-group of the default fontset . */
    rfont_def = fontset_find_font (FONTSET_DEFAULT (fontset), c, face, id, 1);

  if (! VECTORP (rfont_def))
    /* Remeber that we have no font for C.  */
    FONTSET_SET (fontset, make_number (c), Qt);

  return rfont_def;
}

/* Return a newly created fontset with NAME.  If BASE is nil, make a
   base fontset.  Otherwise make a realized fontset whose base is
   BASE.  */

static Lisp_Object
make_fontset (frame, name, base)
     Lisp_Object frame, name, base;
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
    Vfontset_table = larger_vector (Vfontset_table, size + 32, Qnil);

  fontset = Fmake_char_table (Qfontset, Qnil);

  FONTSET_ID (fontset) = make_number (id);
  if (NILP (base))
    {
      FONTSET_NAME (fontset) = name;
    }
  else
    {
      FONTSET_NAME (fontset) = Qnil;
      FONTSET_FRAME (fontset) = frame;
      FONTSET_BASE (fontset) = base;
    }

  ASET (Vfontset_table, id, fontset);
  next_fontset_id = id + 1;
  return fontset;
}


/* Set the ASCII font of the default fontset to FONTNAME if that is
   not yet set.  */
void
set_default_ascii_font (fontname)
     Lisp_Object fontname;
{
  if (! STRINGP (FONTSET_ASCII (Vdefault_fontset)))
    {
      int id = fs_query_fontset (fontname, 2);

      if (id >= 0)
	fontname = FONTSET_ASCII (FONTSET_FROM_ID (id));
      FONTSET_ASCII (Vdefault_fontset)= fontname;
    }
}


/********** INTERFACES TO xfaces.c, xfns.c, and dispextern.h **********/

/* Return the name of the fontset who has ID.  */

Lisp_Object
fontset_name (id)
     int id;
{
  Lisp_Object fontset;

  fontset = FONTSET_FROM_ID (id);
  return FONTSET_NAME (fontset);
}


/* Return the ASCII font name of the fontset who has ID.  */

Lisp_Object
fontset_ascii (id)
     int id;
{
  Lisp_Object fontset, elt;

  fontset= FONTSET_FROM_ID (id);
  elt = FONTSET_ASCII (fontset);
  if (CONSP (elt))
    elt = XCAR (elt);
  return elt;
}

void
free_realized_fontset (f, fontset)
     FRAME_PTR f;
     Lisp_Object fontset;
{
  Lisp_Object tail;

  return;
  for (tail = FONTSET_OBJLIST (fontset); CONSP (tail); tail = XCDR (tail))
    {
      xassert (FONT_OBJECT_P (XCAR (tail)));
      font_close_object (f, XCAR (tail));
    }
}

/* Free fontset of FACE defined on frame F.  Called from
   free_realized_face.  */

void
free_face_fontset (f, face)
     FRAME_PTR f;
     struct face *face;
{
  Lisp_Object fontset;

  fontset = FONTSET_FROM_ID (face->fontset);
  if (NILP (fontset))
    return;
  xassert (! BASE_FONTSET_P (fontset));
  xassert (f == XFRAME (FONTSET_FRAME (fontset)));
  free_realized_fontset (f, fontset);
  ASET (Vfontset_table, face->fontset, Qnil);
  if (face->fontset < next_fontset_id)
    next_fontset_id = face->fontset;
  if (! NILP (FONTSET_DEFAULT (fontset)))
    {
      int id = XINT (FONTSET_ID (FONTSET_DEFAULT (fontset)));
      
      fontset = AREF (Vfontset_table, id);
      xassert (!NILP (fontset) && ! BASE_FONTSET_P (fontset));
      xassert (f == XFRAME (FONTSET_FRAME (fontset)));
      free_realized_fontset (f, fontset);
      ASET (Vfontset_table, id, Qnil);
      if (id < next_fontset_id)
	next_fontset_id = face->fontset;
    }
  face->fontset = -1;
}


/* Return 1 if FACE is suitable for displaying character C.
   Otherwise return 0.  Called from the macro FACE_SUITABLE_FOR_CHAR_P
   when C is not an ASCII character.  */

int
face_suitable_for_char_p (face, c)
     struct face *face;
     int c;
{
  Lisp_Object fontset, rfont_def;

  fontset = FONTSET_FROM_ID (face->fontset);
  rfont_def = fontset_font (fontset, c, NULL, -1);
  return (VECTORP (rfont_def)
	  && INTEGERP (RFONT_DEF_FACE (rfont_def))
	  && face->id == XINT (RFONT_DEF_FACE (rfont_def)));
}


/* Return ID of face suitable for displaying character C on frame F.
   FACE must be reazlied for ASCII characters in advance.  Called from
   the macro FACE_FOR_CHAR.  */

int
face_for_char (f, face, c, pos, object)
     FRAME_PTR f;
     struct face *face;
     int c, pos;
     Lisp_Object object;
{
  Lisp_Object fontset, rfont_def;
  int face_id;
  int id;

  if (ASCII_CHAR_P (c))
    return face->ascii_face->id;

  xassert (fontset_id_valid_p (face->fontset));
  fontset = FONTSET_FROM_ID (face->fontset);
  xassert (!BASE_FONTSET_P (fontset));
  if (pos < 0)
    id = -1;
  else
    {
      Lisp_Object charset;

      charset = Fget_char_property (make_number (pos), Qcharset, object);
      if (NILP (charset))
	id = -1;
      else if (CHARSETP (charset))
	{
	  Lisp_Object val;

	  val = assoc_no_quit (charset, Vfont_encoding_charset_alist);
	  if (CONSP (val) && CHARSETP (XCDR (val)))
	    charset = XCDR (val);
	  id = XINT (CHARSET_SYMBOL_ID (charset));
	}
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
	  FONTSET_NOFONT_FACE (fontset) = make_number (face_id);
	}
    }
  xassert (face_id >= 0);
  return face_id;
}


/* Make a realized fontset for ASCII face FACE on frame F from the
   base fontset BASE_FONTSET_ID.  If BASE_FONTSET_ID is -1, use the
   default fontset as the base.  Value is the id of the new fontset.
   Called from realize_x_face.  */

int
make_fontset_for_ascii_face (f, base_fontset_id, face)
     FRAME_PTR f;
     int base_fontset_id;
     struct face *face;
{
  Lisp_Object base_fontset, fontset, frame;

  XSETFRAME (frame, f);
  if (base_fontset_id >= 0)
    {
      base_fontset = FONTSET_FROM_ID (base_fontset_id);
      if (!BASE_FONTSET_P (base_fontset))
	base_fontset = FONTSET_BASE (base_fontset);
      if (! BASE_FONTSET_P (base_fontset))
	abort ();
    }
  else
    base_fontset = Vdefault_fontset;

  fontset = make_fontset (frame, Qnil, base_fontset);
  return XINT (FONTSET_ID (fontset));
}


/* Return ENCODING or a cons of ENCODING and REPERTORY of the font
   FONTNAME.  ENCODING is a charset symbol that specifies the encoding
   of the font.  REPERTORY is a charset symbol or nil.  */

Lisp_Object
find_font_encoding (fontname)
     Lisp_Object fontname;
{
  Lisp_Object tail, elt;

  for (tail = Vfont_encoding_alist; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      if (CONSP (elt)
	  && STRINGP (XCAR (elt))
	  && fast_string_match_ignore_case (XCAR (elt), fontname) >= 0
	  && (SYMBOLP (XCDR (elt))
	      ? CHARSETP (XCDR (elt))
	      : CONSP (XCDR (elt)) && CHARSETP (XCAR (XCDR (elt)))))
	return (XCDR (elt));
    }
  /* We don't know the encoding of this font.  Let's assume `ascii'.  */
  return Qascii;
}


/* Cache data used by fontset_pattern_regexp.  The car part is a
   pattern string containing at least one wild card, the cdr part is
   the corresponding regular expression.  */
static Lisp_Object Vcached_fontset_data;

#define CACHED_FONTSET_NAME (SDATA (XCAR (Vcached_fontset_data)))
#define CACHED_FONTSET_REGEX (XCDR (Vcached_fontset_data))

/* If fontset name PATTERN contains any wild card, return regular
   expression corresponding to PATTERN.  */

static Lisp_Object
fontset_pattern_regexp (pattern)
     Lisp_Object pattern;
{
  if (!index (SDATA (pattern), '*')
      && !index (SDATA (pattern), '?'))
    /* PATTERN does not contain any wild cards.  */
    return Qnil;

  if (!CONSP (Vcached_fontset_data)
      || strcmp (SDATA (pattern), CACHED_FONTSET_NAME))
    {
      /* We must at first update the cached data.  */
      unsigned char *regex, *p0, *p1;
      int ndashes = 0, nstars = 0;

      for (p0 = SDATA (pattern); *p0; p0++)
	{
	  if (*p0 == '-')
	    ndashes++;
	  else if (*p0 == '*')
	    nstars++;
	}

      /* If PATTERN is not full XLFD we conert "*" to ".*".  Otherwise
	 we convert "*" to "[^-]*" which is much faster in regular
	 expression matching.  */
      if (ndashes < 14)
	p1 = regex = (unsigned char *) alloca (SBYTES (pattern) + 2 * nstars + 1);
      else
	p1 = regex = (unsigned char *) alloca (SBYTES (pattern) + 5 * nstars + 1);

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
	  else
	    *p1++ = *p0;
	}
      *p1++ = '$';
      *p1++ = 0;

      Vcached_fontset_data = Fcons (build_string (SDATA (pattern)),
				    build_string (regex));
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
fs_query_fontset (name, name_pattern)
     Lisp_Object name;
     int name_pattern;
{
  Lisp_Object tem;
  int i;

  name = Fdowncase (name);
  if (name_pattern != 1)
    {
      tem = Frassoc (name, Vfontset_alias_alist);
      if (NILP (tem))
	tem = Fassoc (name, Vfontset_alias_alist);
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
	  : !strcasecmp (SDATA (name), SDATA (this_name)))
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
     (pattern, regexpp)
     Lisp_Object pattern, regexpp;
{
  Lisp_Object fontset;
  int id;

  (*check_window_system_func) ();

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
list_fontsets (f, pattern, size)
     FRAME_PTR f;
     Lisp_Object pattern;
     int size;
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
	  : strcmp (SDATA (pattern), SDATA (name)))
	continue;

      val = Fcons (Fcopy_sequence (FONTSET_NAME (fontset)), val);
    }

  return val;
}


/* Free all realized fontsets whose base fontset is BASE.  */

static void
free_realized_fontsets (base)
     Lisp_Object base;
{
  int id;

#if 0
  /* For the moment, this doesn't work because free_realized_face
     doesn't remove FACE from a cache.  Until we find a solution, we
     suppress this code, and simply use Fclear_face_cache even though
     that is not efficient.  */
  BLOCK_INPUT;
  for (id = 0; id < ASIZE (Vfontset_table); id++)
    {
      Lisp_Object this = AREF (Vfontset_table, id);

      if (EQ (FONTSET_BASE (this), base))
	{
	  Lisp_Object tail;

	  for (tail = FONTSET_FACE_ALIST (this); CONSP (tail);
	       tail = XCDR (tail))
	    {
	      FRAME_PTR f = XFRAME (FONTSET_FRAME (this));
	      int face_id = XINT (XCDR (XCAR (tail)));
	      struct face *face = FACE_FROM_ID (f, face_id);

	      /* Face THIS itself is also freed by the following call.  */
	      free_realized_face (f, face);
	    }
	}
    }
  UNBLOCK_INPUT;
#else  /* not 0 */
  /* But, we don't have to call Fclear_face_cache if no fontset has
     been realized from BASE.  */
  for (id = 0; id < ASIZE (Vfontset_table); id++)
    {
      Lisp_Object this = AREF (Vfontset_table, id);

      if (CHAR_TABLE_P (this) && EQ (FONTSET_BASE (this), base))
	{
	  Fclear_face_cache (Qt);
	  break;
	}
    }
#endif /* not 0 */
}


/* Check validity of NAME as a fontset name and return the
   corresponding fontset.  If not valid, signal an error.
   If NAME is t, return Vdefault_fontset.  */

static Lisp_Object
check_fontset_name (name)
     Lisp_Object name;
{
  int id;

  if (EQ (name, Qt))
    return Vdefault_fontset;

  CHECK_STRING (name);
  /* First try NAME as literal.  */
  id = fs_query_fontset (name, 2);
  if (id < 0)
    /* For backward compatibility, try again NAME as pattern.  */
    id = fs_query_fontset (name, 0);
  if (id < 0)
    error ("Fontset `%s' does not exist", SDATA (name));
  return FONTSET_FROM_ID (id);
}

static void
accumulate_script_ranges (arg, range, val)
     Lisp_Object arg, range, val;
{
  if (EQ (XCAR (arg), val))
    {
      if (CONSP (range))
	XSETCDR (arg, Fcons (Fcons (XCAR (range), XCDR (range)), XCDR (arg)));
      else
	XSETCDR (arg, Fcons (Fcons (range, range), XCDR (arg)));
    }
}


/* Return an ASCII font name generated from fontset name NAME and
   font-spec ASCII_SPEC.  NAME is a string conforming to XLFD.  */

static INLINE Lisp_Object
generate_ascii_font_name (name, ascii_spec)
     Lisp_Object name, ascii_spec;
{
  Lisp_Object font_spec = Ffont_spec (0, NULL);
  Lisp_Object vec;
  int i;
  char xlfd[256];

  if (font_parse_xlfd (SDATA (name), font_spec) < 0)
    error ("Not an XLFD font name: %s", SDATA (name));
  for (i = FONT_FOUNDRY_INDEX; i < FONT_EXTRA_INDEX; i++)
    if (! NILP (AREF (ascii_spec, i)))
      ASET (font_spec, i, AREF (ascii_spec, i));
  i = font_unparse_xlfd (font_spec, 0, xlfd, 256);
  if (i < 0)
    error ("Not an XLFD font name: %s", SDATA (name));
  return make_unibyte_string (xlfd, i);
}

/* Variables referred in set_fontset_font.  They are set before
   map_charset_chars is called in Fset_fontset_font.  */
static Lisp_Object font_def_arg, add_arg;
static int from_arg, to_arg;

/* Callback function for map_charset_chars in Fset_fontset_font.  In
   FONTSET, set font_def_arg in a fashion specified by add_arg for
   characters in RANGE while ignoring the range between from_arg and
   to_arg.  */

static void
set_fontset_font (fontset, range)
     Lisp_Object fontset, range;
{
  if (from_arg < to_arg)
    {
      int from = XINT (XCAR (range)), to = XINT (XCDR (range));

      if (from < from_arg)
	{
	  if (to > to_arg)
	    {
	      Lisp_Object range2;

	      range2 = Fcons (make_number (to_arg), XCDR (range));
	      FONTSET_ADD (fontset, range, font_def_arg, add_arg);
	      to = to_arg;
	    }
	  if (to > from_arg)
	    range = Fcons (XCAR (range), make_number (from_arg));
	}
      else if (to <= to_arg)
	return;
      else
	{
	  if (from < to_arg)
	    range = Fcons (make_number (to_arg), XCDR (range));
	}
    }
  FONTSET_ADD (fontset, range, font_def_arg, add_arg);
}

extern Lisp_Object QCfamily, QCregistry;

DEFUN ("set-fontset-font", Fset_fontset_font, Sset_fontset_font, 3, 5, 0,
       doc: /*
Modify fontset NAME to use FONT-SPEC for TARGET characters.

TARGET may be a cons; (FROM . TO), where FROM and TO are characters.
In that case, use FONT-SPEC for all characters in the range FROM and
TO (inclusive).

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

Optional 4th argument FRAME, if non-nil, is a frame.  This argument is
kept for backward compatibility and has no meaning.

Optional 5th argument ADD, if non-nil, specifies how to add FONT-SPEC
to the font specifications for TARGET previously set.  If it is
`prepend', FONT-SPEC is prepended.  If it is `append', FONT-SPEC is
appended.  By default, FONT-SPEC overrides the previous settings.  */)
     (name, target, font_spec, frame, add)
     Lisp_Object name, target, font_spec, frame, add;
{
  Lisp_Object fontset;
  Lisp_Object font_def, registry, family;
  Lisp_Object range_list;
  struct charset *charset = NULL;

  fontset = check_fontset_name (name);

  /* The arg FRAME is kept for backward compatibility.  We only check
     the validity.  */
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame);

  if (CONSP (font_spec))
    {
      Lisp_Object spec = Ffont_spec (0, NULL);

      font_parse_family_registry (XCAR (font_spec), XCDR (font_spec), spec);
      font_spec = spec;
    }
  else if (STRINGP (font_spec))
    {
      Lisp_Object args[2];
      extern Lisp_Object QCname;

      args[0] = QCname;
      args[1] = font_spec;
      font_spec = Ffont_spec (2, args);
    }
  else if (! NILP (font_spec) && ! FONT_SPEC_P (font_spec))
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
      encoding = find_font_encoding (concat3 (family, build_string ("-"),
					      registry));
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
    range_list = Fcons (Fcons (target, target), Qnil);
  else if (CONSP (target))
    {
      Lisp_Object from, to;

      from = Fcar (target);
      to = Fcdr (target);
      CHECK_CHARACTER (from);
      CHECK_CHARACTER (to);
      range_list = Fcons (target, Qnil);
    }
  else if (SYMBOLP (target) && !NILP (target))
    {
      Lisp_Object script_list;
      Lisp_Object val;

      range_list = Qnil;
      script_list = XCHAR_TABLE (Vchar_script_table)->extras[0];
      if (! NILP (Fmemq (target, script_list)))
	{
	  val = Fcons (target, Qnil);
	  map_char_table (accumulate_script_ranges, Qnil, Vchar_script_table,
			  val);
	  range_list = XCDR (val);
	  if (EQ (target, Qlatin) && NILP (FONTSET_ASCII (fontset)))
	    {
	      if (VECTORP (font_spec))
		val = generate_ascii_font_name (FONTSET_NAME (fontset),
						font_spec);
	      else
		val = font_spec;
	      FONTSET_ASCII (fontset) = val;
	    }
	}
      if (CHARSETP (target))
	{
	  if (EQ (target, Qascii) && NILP (FONTSET_ASCII (fontset)))
	    {
	      if (VECTORP (font_spec))
		font_spec = generate_ascii_font_name (FONTSET_NAME (fontset),
						      font_spec);
	      FONTSET_ASCII (fontset) = font_spec;
	      range_list = Fcons (Fcons (make_number (0), make_number (127)),
				  Qnil);
	    }
	  else
	    {
	      CHECK_CHARSET_GET_CHARSET (target, charset);
	    }
	}
      else if (NILP (range_list))
	error ("Invalid script or charset name: %s",
	       SDATA (SYMBOL_NAME (target)));
    }
  else if (NILP (target))
    range_list = Fcons (Qnil, Qnil);
  else
    error ("Invalid target for setting a font");


  if (charset)
    {
      font_def_arg = font_def;
      add_arg = add;
      if (NILP (range_list))
	from_arg = to_arg = 0;
      else
	from_arg = XINT (XCAR (XCAR (range_list))),
	  to_arg = XINT (XCDR (XCAR (range_list)));

      map_charset_chars (set_fontset_font, Qnil, fontset, charset,
			 CHARSET_MIN_CODE (charset),
			 CHARSET_MAX_CODE (charset));
    }
  for (; CONSP (range_list); range_list = XCDR (range_list))
    FONTSET_ADD (fontset, XCAR (range_list), font_def, add);

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
  (name, fontlist)
     Lisp_Object name, fontlist;
{
  Lisp_Object fontset;
  Lisp_Object val;
  int id;

  CHECK_STRING (name);
  CHECK_LIST (fontlist);

  name = Fdowncase (name);
  id = fs_query_fontset (name, 0);
  if (id < 0)
    {
      Lisp_Object font_spec = Ffont_spec (0, NULL);
      Lisp_Object short_name;
      char *xlfd;
      int len;

      if (font_parse_xlfd (SDATA (name), font_spec) < 0)
	error ("Fontset name must be in XLFD format");
      short_name = AREF (font_spec, FONT_REGISTRY_INDEX);
      if (strncmp (SDATA (SYMBOL_NAME (short_name)), "fontset-", 8)
	  || SBYTES (SYMBOL_NAME (short_name)) < 9)
	error ("Registry field of fontset name must be \"fontset-*\"");
      Vfontset_alias_alist = Fcons (Fcons (name, SYMBOL_NAME (short_name)),
				    Vfontset_alias_alist);
      ASET (font_spec, FONT_REGISTRY_INDEX, Qiso8859_1);
      fontset = make_fontset (Qnil, name, Qnil);
      xlfd = alloca (SBYTES (name) + 1);
      len = font_unparse_xlfd (font_spec, 0, xlfd, SBYTES (name) + 1);
      FONTSET_ASCII (fontset) = make_unibyte_string (xlfd, len);
    }
  else
    {
      fontset = FONTSET_FROM_ID (id);;
      free_realized_fontsets (fontset);
      Fset_char_table_range (fontset, Qt, Qnil);
    }

  for (; ! NILP (fontlist); fontlist = Fcdr (fontlist))
    {
      Lisp_Object elt, script;

      elt = Fcar (fontlist);
      script = Fcar (elt);
      elt = Fcdr (elt);
      if (CONSP (elt) && (NILP (XCDR (elt)) || CONSP (XCDR (elt))))
	for (; CONSP (elt); elt = XCDR (elt))
	  Fset_fontset_font (name, script, XCAR (elt), Qnil, Qappend);
      else
	Fset_fontset_font (name, script, elt, Qnil, Qappend);
    }
  return name;
}


/* Alist of automatically created fontsets.  Each element is a cons
   (FONT-SPEC . FONTSET-ID).  */
static Lisp_Object auto_fontset_alist;

int
fontset_from_font (font_object)
     Lisp_Object font_object;
{
  Lisp_Object font_name = font_get_name (font_object);
  Lisp_Object font_spec = Fcopy_font_spec (font_object);
  Lisp_Object fontset_spec, alias, name, fontset;
  Lisp_Object val;
  int i;

  val = assoc_no_quit (font_spec, auto_fontset_alist);
  if (CONSP (val))
    return XINT (FONTSET_ID (XCDR (val)));
  if (NILP (auto_fontset_alist))
    alias = intern ("fontset-startup");
  else
    {
      char temp[32];
      int len = XINT (Flength (auto_fontset_alist));

      sprintf (temp, "fontset-auto%d", len);
      alias = intern (temp);
    }
  fontset_spec = Fcopy_font_spec (font_spec);
  ASET (fontset_spec, FONT_REGISTRY_INDEX, alias);
  name = Ffont_xlfd_name (fontset_spec);
  if (NILP (name))
    abort ();
  fontset = make_fontset (Qnil, name, Qnil);
  Vfontset_alias_alist = Fcons (Fcons (name, SYMBOL_NAME (alias)),
				Vfontset_alias_alist);
  alias = Fdowncase (AREF (font_object, FONT_NAME_INDEX));
  Vfontset_alias_alist = Fcons (Fcons (name, alias), Vfontset_alias_alist);
  auto_fontset_alist = Fcons (Fcons (font_spec, fontset), auto_fontset_alist);
  FONTSET_ASCII (fontset) = font_name;
  ASET (font_spec, FONT_FOUNDRY_INDEX, Qnil);
  ASET (font_spec, FONT_ADSTYLE_INDEX, Qnil);
  for (i = FONT_WEIGHT_INDEX; i < FONT_EXTRA_INDEX; i++)
    ASET (font_spec, i, Qnil);
  Fset_fontset_font (name, Qlatin, font_spec, Qnil, Qnil);
  font_spec = Fcopy_font_spec (font_spec);
  ASET (font_spec, FONT_REGISTRY_INDEX, Qiso10646_1);
  Fset_fontset_font (name, Qnil, font_spec, Qnil, Qnil);
  return XINT (FONTSET_ID (fontset));
}

DEFUN ("font-info", Ffont_info, Sfont_info, 1, 2, 0,
       doc: /* Return information about a font named NAME on frame FRAME.
If FRAME is omitted or nil, use the selected frame.
The returned value is a vector of OPENED-NAME, FULL-NAME, CHARSET, SIZE,
  HEIGHT, BASELINE-OFFSET, RELATIVE-COMPOSE, and DEFAULT-ASCENT,
where
  OPENED-NAME is the name used for opening the font,
  FULL-NAME is the full name of the font,
  SIZE is the maximum bound width of the font,
  HEIGHT is the height of the font,
  BASELINE-OFFSET is the upward offset pixels from ASCII baseline,
  RELATIVE-COMPOSE and DEFAULT-ASCENT are the numbers controlling
    how to compose characters.
If the named font is not yet loaded, return nil.  */)
     (name, frame)
     Lisp_Object name, frame;
{
  FRAME_PTR f;
  struct font *font;
  Lisp_Object info;
  Lisp_Object font_object;

  (*check_window_system_func) ();

  CHECK_STRING (name);
  name = Fdowncase (name);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  font_object = font_open_by_name (f, SDATA (name));
  if (NILP (font_object))
    return Qnil;
  font = XFONT_OBJECT (font_object);

  info = Fmake_vector (make_number (7), Qnil);
  XVECTOR (info)->contents[0] = AREF (font_object, FONT_NAME_INDEX);
  XVECTOR (info)->contents[1] = AREF (font_object, FONT_NAME_INDEX);
  XVECTOR (info)->contents[2] = make_number (font->pixel_size);
  XVECTOR (info)->contents[3] = make_number (font->height);
  XVECTOR (info)->contents[4] = make_number (font->baseline_offset);
  XVECTOR (info)->contents[5] = make_number (font->relative_compose);
  XVECTOR (info)->contents[6] = make_number (font->default_ascent);

  font_close_object (f, font_object);
  return info;
}


/* Return a cons (FONT-NAME . GLYPH-CODE).
   FONT-NAME is the font name for the character at POSITION in the current
   buffer.  This is computed from all the text properties and overlays
   that apply to POSITION.  POSTION may be nil, in which case,
   FONT-NAME is the font name for display the character CH with the
   default face.

   GLYPH-CODE is the glyph code in the font to use for the character.

   If the 2nd optional arg CH is non-nil, it is a character to check
   the font instead of the character at POSITION.

   It returns nil in the following cases:

   (1) The window system doesn't have a font for the character (thus
   it is displayed by an empty box).

   (2) The character code is invalid.

   (3) If POSITION is not nil, and the current buffer is not displayed
   in any window.

   In addition, the returned font name may not take into account of
   such redisplay engine hooks as what used in jit-lock-mode if
   POSITION is currently not visible.  */


DEFUN ("internal-char-font", Finternal_char_font, Sinternal_char_font, 1, 2, 0,
       doc: /* For internal use only.  */)
     (position, ch)
     Lisp_Object position, ch;
{
  EMACS_INT pos, pos_byte, dummy;
  int face_id;
  int c;
  struct frame *f;
  struct face *face;
  Lisp_Object rfont_def;
  int cs_id;

  if (NILP (position))
    {
      CHECK_CHARACTER (ch);
      c = XINT (ch);
      f = XFRAME (selected_frame);
      face_id = DEFAULT_FACE_ID;
      pos = -1;
      cs_id = -1;
    }
  else
    {
      Lisp_Object window, charset;
      struct window *w;

      CHECK_NUMBER_COERCE_MARKER (position);
      pos = XINT (position);
      if (pos < BEGV || pos >= ZV)
	args_out_of_range_3 (position, make_number (BEGV), make_number (ZV));
      pos_byte = CHAR_TO_BYTE (pos);
      if (NILP (ch))
	c = FETCH_CHAR (pos_byte);
      else
	{
	  CHECK_NATNUM (ch);
	  c = XINT (ch);
	}
      window = Fget_buffer_window (Fcurrent_buffer (), Qnil);
      if (NILP (window))
	return Qnil;
      w = XWINDOW (window);
      f = XFRAME (w->frame);
      face_id = face_at_buffer_position (w, pos, -1, -1, &dummy, pos + 100, 0);
      charset = Fget_char_property (position, Qcharset, Qnil);
      if (CHARSETP (charset))
	cs_id = XINT (CHARSET_SYMBOL_ID (charset));
      else
	cs_id = -1;
    }
  if (! CHAR_VALID_P (c, 0))
    return Qnil;
  face_id = FACE_FOR_CHAR (f, FACE_FROM_ID (f, face_id), c, pos, Qnil);
  face = FACE_FROM_ID (f, face_id);
  if (face->font)
    {
      struct font *font = face->font;
      unsigned code = font->driver->encode_char (font, c);
      Lisp_Object fontname = font->props[FONT_NAME_INDEX];
      /* Assignment to EMACS_INT stops GCC whining about limited range
	 of data type.  */
      EMACS_INT cod = code;

      if (code == FONT_INVALID_CODE)
	return Qnil;
      if (cod <= MOST_POSITIVE_FIXNUM)
	return Fcons (fontname, make_number (code));
      return Fcons (fontname, Fcons (make_number (code >> 16),
				     make_number (code & 0xFFFF)));
    }
  return Qnil;
}


DEFUN ("fontset-info", Ffontset_info, Sfontset_info, 1, 2, 0,
       doc: /* Return information about a fontset FONTSET on frame FRAME.
The value is a char-table of which elements has this form.

    ((FONT-PATTERN OPENED-FONT ...) ...)

FONT-PATTERN is a vector:

	[ FAMILY WEIGHT SLANT SWIDTH ADSTYLE REGISTRY ]

or a string of font name pattern.

OPENED-FONT is a name of a font actually opened.

The char-table has one extra slot.  The value is a char-table
containing the information about the derived fonts from the default
fontset.  The format is the same as abobe.  */)
     (fontset, frame)
     Lisp_Object fontset, frame;
{
  FRAME_PTR f;
  Lisp_Object *realized[2], fontsets[2], tables[2];
  Lisp_Object val, elt;
  int c, i, j, k;

  (*check_window_system_func) ();

  fontset = check_fontset_name (fontset);

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* Recode fontsets realized on FRAME from the base fontset FONTSET
     in the table `realized'.  */
  realized[0] = (Lisp_Object *) alloca (sizeof (Lisp_Object)
					* ASIZE (Vfontset_table));
  for (i = j = 0; i < ASIZE (Vfontset_table); i++)
    {
      elt = FONTSET_FROM_ID (i);
      if (!NILP (elt)
	  && EQ (FONTSET_BASE (elt), fontset)
	  && EQ (FONTSET_FRAME (elt), frame))
	realized[0][j++] = elt;
    }
  realized[0][j] = Qnil;

  realized[1] = (Lisp_Object *) alloca (sizeof (Lisp_Object)
					* ASIZE (Vfontset_table));
  for (i = j = 0; ! NILP (realized[0][i]); i++)
    {
      elt = FONTSET_DEFAULT (realized[0][i]);
      if (! NILP (elt))
	realized[1][j++] = elt;
    }
  realized[1][j] = Qnil;

  tables[0] = Fmake_char_table (Qfontset_info, Qnil);
  tables[1] = Fmake_char_table (Qnil, Qnil);
  XCHAR_TABLE (tables[0])->extras[0] = tables[1];
  fontsets[0] = fontset;
  fontsets[1] = Vdefault_fontset;

  /* Accumulate information of the fontset in TABLE.  The format of
     each element is ((FONT-SPEC OPENED-FONT ...) ...).  */
  for (k = 0; k <= 1; k++)
    {
      for (c = 0; c <= MAX_CHAR; )
	{
	  int from, to;

	  if (c <= MAX_5_BYTE_CHAR)
	    {
	      val = char_table_ref_and_range (fontsets[k], c, &from, &to);
	      if (to > MAX_5_BYTE_CHAR)
		to = MAX_5_BYTE_CHAR;
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

	      /* Then store opend font names to cdr of each elements.  */
	      for (i = 0; ! NILP (realized[k][i]); i++)
		{
		  if (c <= MAX_5_BYTE_CHAR)
		    val = FONTSET_REF (realized[k][i], c);
		  else
		    val = FONTSET_FALLBACK (realized[k][i]);
		  if (! VECTORP (val))
		    continue;
		  /* VAL: [int ? [FACE-ID FONT-DEF FONT-OBJECT int] ... ]  */
		  for (j = 2; j < ASIZE (val); j++)
		    {
		      elt = AREF (val, j);
		      if (FONT_OBJECT_P (RFONT_DEF_OBJECT (elt)))
			{
			  Lisp_Object font_object = RFONT_DEF_OBJECT (elt);
			  Lisp_Object slot, name;

			  slot = Fassq (RFONT_DEF_SPEC (elt), alist);
			  name = AREF (font_object, FONT_NAME_INDEX);
			  if (NILP (Fmember (name, XCDR (slot))))
			    nconc2 (slot, Fcons (name, Qnil));
			}
		    }
		}

	      /* Store ALIST in TBL for characters C..TO.  */
	      if (c <= MAX_5_BYTE_CHAR)
		char_table_set_range (tables[k], c, to, alist);
	      else
		XCHAR_TABLE (tables[k])->defalt = alist;

	      /* At last, change each elements to font names.  */
	      for (; CONSP (alist); alist = XCDR (alist))
		{
		  elt = XCAR (alist);
		  XSETCAR (elt, Ffont_xlfd_name (XCAR (elt)));
		}
	    }
	  c = to + 1;
	}
    }

  return tables[0];
}


DEFUN ("fontset-font", Ffontset_font, Sfontset_font, 2, 3, 0,
       doc: /* Return a font name pattern for character CH in fontset NAME.
If NAME is t, find a pattern in the default fontset.

The value has the form (FAMILY . REGISTRY), where FAMILY is a font
family name and REGISTRY is a font registry name.  This is actually
the first font name pattern for CH in the fontset or in the default
fontset.

If the 2nd optional arg ALL is non-nil, return a list of all font name
patterns.  */)
  (name, ch, all)
     Lisp_Object name, ch, all;
{
  int c;
  Lisp_Object fontset, elt, list, repertory, val;
  int i, j;

  fontset = check_fontset_name (name);

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
	      val = AREF (elt, j);
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
	      val = Fcons (AREF (val, 0), AREF (val, 5));
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
     ()
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


#ifdef FONTSET_DEBUG

Lisp_Object
dump_fontset (fontset)
     Lisp_Object fontset;
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
	  FRAME_PTR f = XFRAME (frame);

	  if (FRAME_LIVE_P (f))
	    ASET (vec, 1,
		  Fcons (FONTSET_NAME (FONTSET_BASE (fontset)), f->name));
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
     ()
{
  Lisp_Object val;
  int i;

  for (i = 0, val = Qnil; i < ASIZE (Vfontset_table); i++)
    if (! NILP (AREF (Vfontset_table, i)))
      val = Fcons (dump_fontset (AREF (Vfontset_table, i)), val);
  return (Fnreverse (val));
}
#endif	/* FONTSET_DEBUG */

void
syms_of_fontset ()
{
  DEFSYM (Qfontset, "fontset");
  Fput (Qfontset, Qchar_table_extra_slots, make_number (9));
  DEFSYM (Qfontset_info, "fontset-info");
  Fput (Qfontset_info, Qchar_table_extra_slots, make_number (1));

  DEFSYM (Qprepend, "prepend");
  DEFSYM (Qappend, "append");
  DEFSYM (Qlatin, "latin");

  Vcached_fontset_data = Qnil;
  staticpro (&Vcached_fontset_data);

  Vfontset_table = Fmake_vector (make_number (32), Qnil);
  staticpro (&Vfontset_table);

  Vdefault_fontset = Fmake_char_table (Qfontset, Qnil);
  staticpro (&Vdefault_fontset);
  FONTSET_ID (Vdefault_fontset) = make_number (0);
  FONTSET_NAME (Vdefault_fontset)
    = build_string ("-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default");
  ASET (Vfontset_table, 0, Vdefault_fontset);
  next_fontset_id = 1;

  auto_fontset_alist = Qnil;
  staticpro (&auto_fontset_alist);

  DEFVAR_LISP ("font-encoding-alist", &Vfont_encoding_alist,
	       doc: /*
Alist of fontname patterns vs the corresponding encoding and repertory info.
Each element looks like (REGEXP . (ENCODING . REPERTORY)),
where ENCODING is a charset or a char-table,
and REPERTORY is a charset, a char-table, or nil.

If ENCDING and REPERTORY are the same, the element can have the form
\(REGEXP . ENCODING).

ENCODING is for converting a character to a glyph code of the font.
If ENCODING is a charset, encoding a character by the charset gives
the corresponding glyph code.  If ENCODING is a char-table, looking up
the table by a character gives the corresponding glyph code.

REPERTORY specifies a repertory of characters supported by the font.
If REPERTORY is a charset, all characters beloging to the charset are
supported.  If REPERTORY is a char-table, all characters who have a
non-nil value in the table are supported.  It REPERTORY is nil, Emacs
gets the repertory information by an opened font and ENCODING.  */);
  Vfont_encoding_alist = Qnil;

  DEFVAR_LISP ("font-encoding-charset-alist", &Vfont_encoding_charset_alist,
	       doc: /*
Alist of charsets vs the charsets to determine the preferred font encoding.
Each element looks like (CHARSET . ENCDOING-CHARSET),
where ENCODING-CHARSET is a charset registered in the variable
`font-encoding-alist' as ENCODING.

When a text has a property `charset' and the value is CHARSET, a font
whose encoding corresponds to ENCODING-CHARSET is preferred.  */);
  Vfont_encoding_charset_alist = Qnil;

  DEFVAR_LISP ("use-default-ascent", &Vuse_default_ascent,
	       doc: /*
Char table of characters whose ascent values should be ignored.
If an entry for a character is non-nil, the ascent value of the glyph
is assumed to be what specified by _MULE_DEFAULT_ASCENT property of a font.

This affects how a composite character which contains
such a character is displayed on screen.  */);
  Vuse_default_ascent = Qnil;

  DEFVAR_LISP ("ignore-relative-composition", &Vignore_relative_composition,
	       doc: /*
Char table of characters which is not composed relatively.
If an entry for a character is non-nil, a composition sequence
which contains that character is displayed so that
the glyph of that character is put without considering
an ascent and descent value of a previous character.  */);
  Vignore_relative_composition = Qnil;

  DEFVAR_LISP ("alternate-fontname-alist", &Valternate_fontname_alist,
	       doc: /* Alist of fontname vs list of the alternate fontnames.
When a specified font name is not found, the corresponding
alternate fontnames (if any) are tried instead.  */);
  Valternate_fontname_alist = Qnil;

  DEFVAR_LISP ("fontset-alias-alist", &Vfontset_alias_alist,
	       doc: /* Alist of fontset names vs the aliases.  */);
  Vfontset_alias_alist = Fcons (Fcons (FONTSET_NAME (Vdefault_fontset),
				       build_string ("fontset-default")),
				Qnil);

  DEFVAR_LISP ("vertical-centering-font-regexp",
	       &Vvertical_centering_font_regexp,
	       doc: /* *Regexp matching font names that require vertical centering on display.
When a character is displayed with such fonts, the character is displayed
at the vertical center of lines.  */);
  Vvertical_centering_font_regexp = Qnil;

  DEFVAR_LISP ("otf-script-alist", &Votf_script_alist,
	       doc: /* Alist of OpenType script tags vs the corresponding script names.  */);
  Votf_script_alist = Qnil;

  defsubr (&Squery_fontset);
  defsubr (&Snew_fontset);
  defsubr (&Sset_fontset_font);
  defsubr (&Sfont_info);
  defsubr (&Sinternal_char_font);
  defsubr (&Sfontset_info);
  defsubr (&Sfontset_font);
  defsubr (&Sfontset_list);
#ifdef FONTSET_DEBUG
  defsubr (&Sfontset_list_all);
#endif
}

/* arch-tag: ea861585-2f5f-4e5b-9849-d04a9c3a3537
   (do not change this comment) */
