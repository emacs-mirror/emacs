/* Fontset handler.
   Copyright (C) 1995, 1997, 2000 Electrotechnical Laboratory, JAPAN.
     Licensed to the Free Software Foundation.
   Copyright (C) 2003
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
#include "fontset.h"
#include "window.h"

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

   FONT-SPEC is:
	[ FAMILY WEIGHT SLANT SWIDTH ADSTYLE REGISTRY ]
   or
	FONT-NAME
   where FAMILY, WEIGHT, SLANT, SWIDTH, ADSTYLE, REGISTRY, and
   FONT-NAME are strings.

   ENCODING is a charset ID or a char-table that can convert
   characters to glyph codes of the corresponding font.

   REPERTORY is a charset ID or nil.  If REPERTORY is a charset ID,
   the repertory of the charset exactly matches with that of the font.
   If REPERTORY is nil, we consult with the font itself to get the
   repertory.

   ENCODING and REPERTORY are extracted from the variable
   Vfont_encoding_alist by using a font name generated form FONT-SPEC
   (if it is a vector) or FONT-NAME as a key.


   An element of a realized fontset is nil or t, or has this form:

	( CHARSET-PRIORITY-LIST-TICK . FONT-VECTOR )

   FONT-VECTOR is a vector whose elements have this form:

	[ FACE-ID FONT-INDEX FONT-DEF ]

   FONT-VECTOR is automatically reordered by the current charset
   priority list.

   The value nil means that we have not yet generated FONT-VECTOR from
   the base of the fontset.

   The value t means that no font is available for the corresponding
   range of characters.


   A fontset has 8 extra slots.

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

/* Vector containing all fontsets.  */
static Lisp_Object Vfontset_table;

/* Next possibly free fontset ID.  Usually this keeps the minimum
   fontset ID not yet used.  */
static int next_fontset_id;

/* The default fontset.  This gives default FAMILY and REGISTRY of
   font for each character.  */
static Lisp_Object Vdefault_fontset;

Lisp_Object Vfont_encoding_alist;
Lisp_Object Vuse_default_ascent;
Lisp_Object Vignore_relative_composition;
Lisp_Object Valternate_fontname_alist;
Lisp_Object Vfontset_alias_alist;
Lisp_Object Vvertical_centering_font_regexp;

/* The following six are declarations of callback functions depending
   on window system.  See the comments in src/fontset.h for more
   detail.  */

/* Return a pointer to struct font_info of font FONT_IDX of frame F.  */
struct font_info *(*get_font_info_func) P_ ((FRAME_PTR f, int font_idx));

/* Return a list of font names which matches PATTERN.  See the documentation
   of `x-list-fonts' for more details.  */
Lisp_Object (*list_fonts_func) P_ ((struct frame *f,
				    Lisp_Object pattern,
				    int size,
				    int maxnames));

/* Load a font named NAME for frame F and return a pointer to the
   information of the loaded font.  If loading is failed, return 0.  */
struct font_info *(*load_font_func) P_ ((FRAME_PTR f, char *name, int));

/* Return a pointer to struct font_info of a font named NAME for frame F.  */
struct font_info *(*query_font_func) P_ ((FRAME_PTR f, char *name));

/* Additional function for setting fontset or changing fontset
   contents of frame F.  */
void (*set_frame_fontset_func) P_ ((FRAME_PTR f, Lisp_Object arg,
				    Lisp_Object oldval));

/* To find a CCL program, fs_load_font calls this function.
   The argument is a pointer to the struct font_info.
   This function set the member `encoder' of the structure.  */
void (*find_ccl_program_func) P_ ((struct font_info *));

Lisp_Object (*get_font_repertory_func) P_ ((struct frame *,
					    struct font_info *));

/* Check if any window system is used now.  */
void (*check_window_system_func) P_ ((void));


/* Prototype declarations for static functions.  */
static Lisp_Object fontset_add P_ ((Lisp_Object, Lisp_Object, Lisp_Object,
				    Lisp_Object));
static Lisp_Object make_fontset P_ ((Lisp_Object, Lisp_Object, Lisp_Object));
static Lisp_Object fontset_pattern_regexp P_ ((Lisp_Object));
static void accumulate_script_ranges P_ ((Lisp_Object, Lisp_Object,
					  Lisp_Object));
static Lisp_Object find_font_encoding P_ ((char *));

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

/* Macros to access special values of (realized) FONTSET.  */
#define FONTSET_BASE(fontset)		XCHAR_TABLE (fontset)->extras[2]
#define FONTSET_FRAME(fontset)		XCHAR_TABLE (fontset)->extras[3]
#define FONTSET_NOFONT_FACE(fontset)	XCHAR_TABLE (fontset)->extras[5]
#define FONTSET_REPERTORY(fontset)	XCHAR_TABLE (fontset)->extras[6]
#define FONTSET_FALLBACK(fontset)	XCHAR_TABLE (fontset)->extras[7]

#define BASE_FONTSET_P(fontset)		(NILP (FONTSET_BASE (fontset)))


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


/* Return the element of FONTSET for the character C, set FROM and TO
   to the range of characters around C that have the same value as C.
   If FONTSET is a base fontset other then the default fontset and
   FONTSET doesn't contain information for C, return the information
   in the default fontset.  */

#define FONTSET_REF_AND_RANGE(fontset, c, form, to)	\
  (EQ (fontset, Vdefault_fontset)			\
   ? char_table_ref_and_range (fontset, c, &from, &to)	\
   : fontset_ref_and_range (fontset, c, &from, &to))

static Lisp_Object
fontset_ref_and_range (fontset, c, from, to)
     Lisp_Object fontset;
     int c;
     int *from, *to;
{
  Lisp_Object elt;

  elt = char_table_ref_and_range (fontset, c, from, to);
  if (NILP (elt) && ! EQ (fontset, Vdefault_fontset)
      /* Don't check Vdefault_fontset for a realized fontset.  */
      && NILP (FONTSET_BASE (fontset)))
    {
      int from1, to1;

      elt = char_table_ref_and_range (Vdefault_fontset, c, &from1, &to1);
      if (*from < from1)
	*from = from1;
      if (*to > to1)
	*to = to1;
    }
  return elt;
}


/* Set elements of FONTSET for characters in RANGE to the value ELT.
   RANGE is a cons (FROM . TO), where FROM and TO are character codes
   specifying a range.  */

#define FONTSET_SET(fontset, range, elt)	\
  Fset_char_table_range ((fontset), (range), (elt))


/* Modify the elements of FONTSET for characters in RANGE by replacing
   with ELT or adding ETL.  RANGE is a cons (FROM . TO), where FROM
   and TO are character codes specifying a range.  If ADD is nil,
   replace with ELT, if ADD is `prepend', prepend ELT, otherwise,
   append ELT.  */

#define FONTSET_ADD(fontset, range, elt, add)				\
  (NILP (add)								\
   ? Fset_char_table_range ((fontset), (range),				\
			    Fmake_vector (make_number (1), (elt)))	\
   : fontset_add ((fontset), (range), (elt), (add)))

static Lisp_Object
fontset_add (fontset, range, elt, add)
     Lisp_Object fontset, range, elt, add;
{
  int from, to, from1, to1;
  Lisp_Object elt1;

  from = XINT (XCAR (range));
  to = XINT (XCDR (range));
  do {
    elt1 = char_table_ref_and_range (fontset, from, &from1, &to1);
    if (to < to1)
      to1 = to;
    if (NILP (elt1))
      elt1 = Fmake_vector (make_number (1), elt);
    else
      {
	int i, i0 = 1, i1 = ASIZE (elt1) + 1;
	Lisp_Object new;

	new = Fmake_vector (make_number (i1), elt);
	if (EQ (add, Qappend))
	  i0--, i1--;
	for (i = 0; i0 < i1; i++, i0++)
	  ASET (new, i0, AREF (elt1, i));
	elt1 = new;
      }
    char_table_set_range (fontset, from, to1, elt1);
    from = to1 + 1;
  } while (from < to);
  return Qnil;
}


/* Update FONTSET_ELEMENT which has this form:
	( CHARSET-PRIORITY-LIST-TICK . FONT-VECTOR).
   Reorder FONT-VECTOR according to the current order of charset
   (Vcharset_ordered_list), and update CHARSET-PRIORITY-LIST-TICK to
   the latest value.  */

static void
reorder_font_vector (fontset_element)
     Lisp_Object fontset_element;
{
  Lisp_Object vec, list, *new_vec;
  int size;
  int *charset_id_table;
  int i, idx;

  XSETCAR (fontset_element, make_number (charset_ordered_list_tick));
  vec = XCDR (fontset_element);
  size = ASIZE (vec);
  if (size < 2)
    /* No need of reordering VEC.  */
    return;
  charset_id_table = (int *) alloca (sizeof (int) * size);
  new_vec = (Lisp_Object *) alloca (sizeof (Lisp_Object) * size);
  /* At first, extract ENCODING (a chaset ID) from VEC.  VEC has this
     form:
	[[FACE-ID FONT-INDEX [ FONT-SPEC ENCODING REPERTORY ]] ...] */
  for (i = 0; i < size; i++)
    charset_id_table[i] = XINT (AREF (AREF (AREF (vec, i), 2), 1));

  /* Then, store the elements of VEC in NEW_VEC in the correct
     order.  */
  idx = 0;
  for (list = Vcharset_ordered_list; CONSP (list); list = XCDR (list))
    {
      for (i = 0; i < size; i++)
	if (charset_id_table[i] == XINT (XCAR (list)))
	  new_vec[idx++] = AREF (vec, i);
      if (idx == size)
	break;
    }

  /* At last, update VEC.  */
  for (i = 0; i < size; i++)
    ASET (vec, i, new_vec[i]);
}


/* Load a font matching the font related attributes in FACE->lface and
   font pattern in FONT_DEF of FONTSET, and return an index of the
   font.  FONT_DEF has this form:
	[ FONT-SPEC ENCODING REPERTORY ]
   If REPERTORY is nil, generate a char-table representing the font
   repertory by looking into the font itself.  */

static int
load_font_get_repertory (f, face, font_def, fontset)
     FRAME_PTR f;
     struct face *face;
     Lisp_Object font_def;
     Lisp_Object fontset;
{
  char *font_name;
  struct font_info *font_info;
  int charset;

  font_name = choose_face_font (f, face->lface, AREF (font_def, 0), NULL);
  if (NATNUMP (AREF (font_def, 1)))
    charset = XINT (AREF (font_def, 1));
  else
    charset = -1;
  if (! (font_info = fs_load_font (f, font_name, charset)))
    return -1;

  if (NILP (AREF (font_def, 2))
      && NILP (Fassq (make_number (font_info->font_idx),
		      FONTSET_REPERTORY (fontset))))
    {
      /* We must look into the font to get the correct repertory as a
	 char-table.  */
      Lisp_Object repertory;

      repertory = (*get_font_repertory_func) (f, font_info);
      FONTSET_REPERTORY (fontset)
	= Fcons (Fcons (make_number (font_info->font_idx), repertory),
		 FONTSET_REPERTORY (fontset));
    }
	
  return font_info->font_idx;
}


/* Return a face ID registerd in the realized fontset FONTSET for the
   character C.  If FACE is NULL, return -1 if a face is not yet
   set.  Otherwise, realize a proper face from FACE and return it.  */

static int
fontset_face (fontset, c, face)
     Lisp_Object fontset;
     int c;
     struct face *face;
{
  Lisp_Object base_fontset, elt, vec;
  int i, from, to;
  int font_idx;
  FRAME_PTR f = XFRAME (FONTSET_FRAME (fontset));

  base_fontset = FONTSET_BASE (fontset);
  elt = CHAR_TABLE_REF (fontset, c);

  if (EQ (elt, Qt))
    goto try_default;

  if (NILP (elt))
    {
      /* We have not yet decided a face for C.  */
      Lisp_Object range;

      if (! face)
	return -1;
      elt = FONTSET_REF_AND_RANGE (base_fontset, c, from, to);
      range = Fcons (make_number (from), make_number (to));
      if (NILP (elt))
	{
	  /* Record that we have no font for characters of this
	     range.  */
	  FONTSET_SET (fontset, range, Qt);
	  goto try_default;
	}
      elt = Fcopy_sequence (elt);
      /* Now ELT is a vector of FONT-DEFs.  We at first change it to
	 FONT-VECTOR, a vector of [ nil nil FONT-DEF ].  */
      for (i = 0; i < ASIZE (elt); i++)
	{
	  Lisp_Object tmp;

	  tmp = Fmake_vector (make_number (3), Qnil);
	  ASET (tmp, 2, AREF (elt, i));
	  ASET (elt, i, tmp);
	}
      /* Then store (-1 . FONT-VECTOR) in the fontset.  -1 is to force
	 reordering of FONT-VECTOR.  */
      elt = Fcons (make_number (-1), elt);
      FONTSET_SET (fontset, range, elt);
    }

  if (XINT (XCAR (elt)) != charset_ordered_list_tick)
    /* The priority of charsets is changed after we selected a face
       for C last time.  */
    reorder_font_vector (elt);

  vec = XCDR (elt);
  /* Find the first available font in the font vector VEC.  */
  for (i = 0; i < ASIZE (vec); i++)
    {
      Lisp_Object font_def;

      elt = AREF (vec, i);
      /* ELT == [ FACE-ID FONT-INDEX [ FONT-SPEC ENCODING REPERTORY ] ] */
      font_def = AREF (elt, 2);
      if (INTEGERP (AREF (elt, 1)) && XINT (AREF (elt, 1)) < 0)
	/* We couldn't open this font last time.  */
	continue;

      if (!face && (NILP (AREF (elt, 1)) || NILP (AREF (elt, 0))))
	/* We have not yet opened the font, or we have not yet made a
	   realized face for the font.  */
	return -1;

      if (INTEGERP (AREF (font_def, 2)))
	{
	  /* The repertory is specified by charset ID.  */
	  struct charset *charset
	    = CHARSET_FROM_ID (XINT (AREF (font_def, 2)));

	  if (! CHAR_CHARSET_P (c, charset))
	    /* This font can't display C.  */
	    continue;
	}
      else
	{
	  Lisp_Object slot;

	  if (! INTEGERP (AREF (elt, 1)))
	    {
	      /* We have not yet opened a font matching this spec.
		 Open the best matching font now and register the
		 repertory.  */
	      font_idx = load_font_get_repertory (f, face, font_def, fontset);
	      ASET (elt, 1, make_number (font_idx));
	      if (font_idx < 0)
		/* This means that we couldn't find a font matching
		   FONT_DEF.  */
		continue;
	    }

	  slot = Fassq (AREF (elt, 1), FONTSET_REPERTORY (fontset));
	  if (! CONSP (slot))
	    abort ();
	  if (NILP (CHAR_TABLE_REF (XCDR (slot), c)))
	    /* This fond can't display C.  */
	    continue;
	}

      /* Now we have decided to use this font spec to display C.  */
      if (INTEGERP (AREF (elt, 1)))
	font_idx = XINT (AREF (elt, 1));
      else
	{
	  /* But not yet opened the best matching font.  */
	  font_idx = load_font_get_repertory (f, face, font_def, fontset);
	  ASET (elt, 1, make_number (font_idx));
	  if (font_idx < 0)
	    continue;
	}

      /* Now we have the opened font.  */
      if (NILP (AREF (elt, 0)))
	{
	  /* But not yet made a realized face that uses this font.  */
	  int face_id = lookup_non_ascii_face (f, font_idx, face);

	  ASET (elt, 0, make_number (face_id));
	}

      /* Ok, this face can display C.  */
      return XINT (AREF (elt, 0));
    }

 try_default:
  if (! EQ (base_fontset, Vdefault_fontset))
    {
      if (NILP (FONTSET_FALLBACK (fontset)))
	FONTSET_FALLBACK (fontset)
	  = make_fontset (FONTSET_FRAME (fontset), Qnil, Vdefault_fontset);
      return fontset_face (FONTSET_FALLBACK (fontset), c, face);
    }

  /* We have tried all the fonts for C, but none of them can be opened
     nor can display C.  */
  if (NILP (FONTSET_NOFONT_FACE (fontset)))
    {
      int face_id;

      if (! face)
	return -1;
      face_id = lookup_non_ascii_face (f, -1, face);
      FONTSET_NOFONT_FACE (fontset) = make_number (face_id);
    }
  return XINT (FONTSET_NOFONT_FACE (fontset));
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
    {
      /* We must grow Vfontset_table.  */
      Lisp_Object tem;
      int i;

      tem = Fmake_vector (make_number (size + 32), Qnil);
      for (i = 0; i < size; i++)
	AREF (tem, i) = AREF (Vfontset_table, i);
      Vfontset_table = tem;
    }

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
  /* It is assured that ELT is always a string (i.e. fontname
     pattern).  */
  return elt;
}


/* Free fontset of FACE defined on frame F.  Called from
   free_realized_face.  */

void
free_face_fontset (f, face)
     FRAME_PTR f;
     struct face *face;
{
  Lisp_Object fontset;

  fontset = AREF (Vfontset_table, face->fontset);
  xassert (!NILP (fontset) && ! BASE_FONTSET_P (fontset));
  xassert (f == XFRAME (FONTSET_FRAME (fontset)));
  ASET (Vfontset_table, face->fontset, Qnil);
  if (face->fontset < next_fontset_id)
    next_fontset_id = face->fontset;
  if (! NILP (FONTSET_FALLBACK (fontset)))
    {
      int id = FONTSET_ID (FONTSET_FALLBACK (fontset));
      
      fontset = AREF (Vfontset_table, id);
      xassert (!NILP (fontset) && ! BASE_FONTSET_P (fontset));
      xassert (f == XFRAME (FONTSET_FRAME (fontset)));
      ASET (Vfontset_table, id, Qnil);
      if (id < next_fontset_id)
	next_fontset_id = face->fontset;
    }
}


/* Return 1 iff FACE is suitable for displaying character C.
   Otherwise return 0.  Called from the macro FACE_SUITABLE_FOR_CHAR_P
   when C is not an ASCII character.  */

int
face_suitable_for_char_p (face, c)
     struct face *face;
     int c;
{
  Lisp_Object fontset;

  fontset = FONTSET_FROM_ID (face->fontset);
  return (face->id == fontset_face (fontset, c, NULL));
}


/* Return ID of face suitable for displaying character C on frame F.
   FACE must be reazlied for ASCII characters in advance.  Called from
   the macro FACE_FOR_CHAR.  */

int
face_for_char (f, face, c)
     FRAME_PTR f;
     struct face *face;
     int c;
{
  Lisp_Object fontset;

  if (ASCII_CHAR_P (c))
    return face->ascii_face->id;

  xassert (fontset_id_valid_p (face->fontset));
  fontset = FONTSET_FROM_ID (face->fontset);
  xassert (!BASE_FONTSET_P (fontset));
  return fontset_face (fontset, c, face);
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
      xassert (BASE_FONTSET_P (base_fontset));
      if (! BASE_FONTSET_P (base_fontset))
	abort ();
    }
  else
    base_fontset = Vdefault_fontset;

  fontset = make_fontset (frame, Qnil, base_fontset);
  {
    Lisp_Object elt;

    elt = FONTSET_REF (base_fontset, 0);
    elt = Fmake_vector (make_number (3), AREF (elt, 0));
    ASET (elt, 0, make_number (face->id));
    ASET (elt, 1, make_number (face->font_info_id));
    elt = Fcons (make_number (charset_ordered_list_tick),
		 Fmake_vector (make_number (1), elt));
    char_table_set_range (fontset, 0, 127, elt);
  }
  return XINT (FONTSET_ID (fontset));
}


#if defined(WINDOWSNT) && defined (_MSC_VER)
#pragma optimize("", off)
#endif

/* Load a font named FONTNAME on frame F.  Return a pointer to the
   struct font_info of the loaded font.  If loading fails, return
   NULL.  CHARSET is an ID of charset to encode characters for this
   font.  If it is -1, find one from Vfont_encoding_alist.  */

struct font_info *
fs_load_font (f, fontname, charset)
     FRAME_PTR f;
     char *fontname;
     int charset;
{
  struct font_info *fontp;

  if (!fontname)
    /* No way to get fontname.  */
    return NULL;

  fontp = (*load_font_func) (f, fontname, 0);
  if (! fontp || fontp->charset >= 0)
    return fontp;

  fontname = fontp->full_name;

  if (charset < 0)
    {
      Lisp_Object charset_symbol;

      charset_symbol = find_font_encoding (fontname);
      if (CONSP (charset_symbol))
	charset_symbol = XCAR (charset_symbol);
      charset = XINT (CHARSET_SYMBOL_ID (charset_symbol));
    }
  fontp->charset = charset;
  fontp->vertical_centering = 0;
  fontp->font_encoder = NULL;

  if (charset != charset_ascii)
    {
      fontp->vertical_centering
	= (STRINGP (Vvertical_centering_font_regexp)
	   && (fast_c_string_match_ignore_case
	       (Vvertical_centering_font_regexp, fontname) >= 0));

      if (find_ccl_program_func)
	(*find_ccl_program_func) (fontp);
    }

  return fontp;
}

#if defined(WINDOWSNT) && defined (_MSC_VER)
#pragma optimize("", on)
#endif


/* Return ENCODING or a cons of ENCODING and REPERTORY of the font
   FONTNAME.  ENCODING is a charset symbol that specifies the encoding
   of the font.  REPERTORY is a charset symbol or nil.  */


static Lisp_Object
find_font_encoding (fontname)
     char *fontname;
{
  Lisp_Object tail, elt;

  for (tail = Vfont_encoding_alist; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      if (CONSP (elt)
	  && STRINGP (XCAR (elt))
	  && fast_c_string_match_ignore_case (XCAR (elt), fontname) >= 0
	  && (SYMBOLP (XCDR (elt))
	      ? CHARSETP (XCDR (elt))
	      : CONSP (XCDR (elt)) && CHARSETP (XCAR (XCDR (elt)))))
	return (XCDR (elt));
    }
  /* We don't know the encoding of this font.  Let's assume Unicode
     encoding.  */
  return Qunicode;
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
      char *regex = (char *) alloca (SCHARS (pattern) * 2 + 3);
      char *p0, *p1 = regex;

      /* Convert "*" to ".*", "?" to ".".  */
      *p1++ = '^';
      for (p0 = (char *) SDATA (pattern); *p0; p0++)
	{
	  if (*p0 == '*')
	    {
	      *p1++ = '.';
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
   fontset, return -1.  */

int
fs_query_fontset (name, regexpp)
     Lisp_Object name;
     int regexpp;
{
  Lisp_Object tem;
  int i;

  name = Fdowncase (name);
  if (!regexpp)
    {
      tem = Frassoc (name, Vfontset_alias_alist);
      if (CONSP (tem) && STRINGP (XCAR (tem)))
	name = XCAR (tem);
      else
	{
	  tem = fontset_pattern_regexp (name);
	  if (STRINGP (tem))
	    {
	      name = tem;
	      regexpp = 1;
	    }
	}
    }

  for (i = 0; i < ASIZE (Vfontset_table); i++)
    {
      Lisp_Object fontset;
      unsigned char *this_name;

      fontset = FONTSET_FROM_ID (i);
      if (NILP (fontset)
	  || !BASE_FONTSET_P (fontset))
	continue;

      this_name = SDATA (FONTSET_NAME (fontset));
      if (regexpp
	  ? fast_c_string_match_ignore_case (name, this_name) >= 0
	  : !strcmp (SDATA (name), this_name))
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
      Lisp_Object fontset;
      unsigned char *name;

      fontset = FONTSET_FROM_ID (id);
      if (NILP (fontset)
	  || !BASE_FONTSET_P (fontset)
	  || !EQ (frame, FONTSET_FRAME (fontset)))
	continue;
      name = SDATA (FONTSET_NAME (fontset));

      if (STRINGP (regexp)
	  ? (fast_c_string_match_ignore_case (regexp, name) < 0)
	  : strcmp (SDATA (pattern), name))
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
#if 0
  int id;

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
  Fclear_face_cache (Qt);
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
   ASCII font specification ASCII_SPEC.  NAME is a string conforming
   to XLFD.  ASCII_SPEC is a vector:
	[FAMILY WEIGHT SLANT SWIDTH ADSTYLE REGISTRY].  */

static INLINE Lisp_Object
generate_ascii_font_name (name, ascii_spec)
     Lisp_Object name, ascii_spec;
{
  Lisp_Object vec;
  int i;

  vec = split_font_name_into_vector (name);
  for (i = FONT_SPEC_FAMILY_INDEX; i <= FONT_SPEC_ADSTYLE_INDEX; i++)
    if (! NILP (AREF (ascii_spec, i)))
      ASET (vec, 1 + i, AREF (ascii_spec, i));
  if (! NILP (AREF (ascii_spec, FONT_SPEC_REGISTRY_INDEX)))
    ASET (vec, 12, AREF (ascii_spec, FONT_SPEC_REGISTRY_INDEX));
  return build_font_name_from_vector (vec);
}

static void
set_fontset_font (range, arg)
     Lisp_Object range, arg;
{
  Lisp_Object fontset, font_def, add;

  fontset = XCAR (arg);
  font_def = XCAR (XCDR (arg));
  add = XCAR (XCDR (XCDR (arg)));
  FONTSET_ADD (fontset, range, font_def, add);
  free_realized_fontsets (fontset);
}


DEFUN ("set-fontset-font", Fset_fontset_font, Sset_fontset_font, 3, 5, 0,
       doc: /*
Modify fontset NAME to use FONT-SPEC for CHARACTER.

CHARACTER may be a cons; (FROM . TO), where FROM and TO are
characters.  In that case, use FONT-SPEC for all characters in the
range FROM and TO (inclusive).

CHARACTER may be a script name symbol.  In that case, use FONT-SPEC
for all characters that belong to the script.

CHARACTER may be a charset.  In that case, use FONT-SPEC for all
characters in the charset.

FONT-SPEC may be:
 * A vector [ FAMILY WEIGHT SLANT WIDTH ADSTYLE REGISTRY ].
   See the documentation of `set-face-attribute' for the detail of
   these vector elements;
 * A cons (FAMILY . REGISTRY), where FAMILY is a font family name and
   REGISTRY is a font registry name;
 * A font name string.

Optional 4th argument FRAME, if non-nil, is a frame.  This argument is
kept for backward compatibility and has no meaning.

Optional 5th argument ADD, if non-nil, specifies how to add FONT-SPEC
to the font specifications for RANGE previously set.  If it is
`prepend', FONT-SPEC is prepended.  If it is `append', FONT-SPEC is
appended.  By default, FONT-SPEC overrides the previous settings.  */)
     (name, character, font_spec, frame, add)
     Lisp_Object name, character, font_spec, frame, add;
{
  Lisp_Object fontset;
  Lisp_Object font_def, registry;
  Lisp_Object encoding, repertory;
  Lisp_Object range_list;

  fontset = check_fontset_name (name);

  /* The arg FRAME is kept for backward compatibility.  We only check
     the validity.  */
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame);

  if (VECTORP (font_spec))
    {
      int j;

      if (ASIZE (font_spec) != FONT_SPEC_MAX_INDEX)
	args_out_of_range (make_number (FONT_SPEC_MAX_INDEX),
			   make_number (ASIZE (font_spec)));

      font_spec = Fcopy_sequence (font_spec);
      for (j = 0; j < FONT_SPEC_MAX_INDEX - 1; j++)
	if (! NILP (AREF (font_spec, j)))
	  {
	    CHECK_STRING (AREF (font_spec, j));
	    ASET (font_spec, j, Fdowncase (AREF (font_spec, j)));
	  }
      /* REGISTRY should not be omitted.  */
      CHECK_STRING (AREF (font_spec, FONT_SPEC_REGISTRY_INDEX));
      registry = Fdowncase (AREF (font_spec, FONT_SPEC_REGISTRY_INDEX));
      ASET (font_spec, FONT_SPEC_REGISTRY_INDEX, registry);

    }
  else if (CONSP (font_spec))
    {
      Lisp_Object family;

      family = XCAR (font_spec);
      registry = XCDR (font_spec);

      if (! NILP (family))
	{
	  CHECK_STRING (family);
	  family = Fdowncase (family);
	}
      CHECK_STRING (registry);
      registry = Fdowncase (registry);
      font_spec = Fmake_vector (make_number (FONT_SPEC_MAX_INDEX), Qnil);
      ASET (font_spec, FONT_SPEC_FAMILY_INDEX, family);
      ASET (font_spec, FONT_SPEC_REGISTRY_INDEX, registry);
    }
  else
    {
      CHECK_STRING (font_spec);
      font_spec = Fdowncase (font_spec);
      registry = split_font_name_into_vector (font_spec);
      if (NILP (registry))
	error ("No XLFD: %s", SDATA (font_spec));
      if (NILP (AREF (registry, 12))
	  || NILP (AREF (registry, 13)))
	error ("Registry must be specified");
      registry = concat2 (concat2 (AREF (registry, 12), build_string ("-")),
			  AREF (registry, 13));
    }

  if (STRINGP (font_spec))
    encoding = find_font_encoding ((char *) SDATA (font_spec));
  else
    encoding = find_font_encoding ((char *) SDATA (registry));
  if (SYMBOLP (encoding))
    encoding = repertory = CHARSET_SYMBOL_ID (encoding);
  else
    {
      repertory = XCDR (encoding);
      encoding = CHARSET_SYMBOL_ID (XCAR (encoding));
    }
  font_def = Fmake_vector (make_number (3), font_spec);
  ASET (font_def, 1, encoding);
  ASET (font_def, 2, repertory);

  if (CHARACTERP (character))
    range_list = Fcons (Fcons (character, character), Qnil);
  else if (CONSP (character))
    {
      Lisp_Object from, to;

      from = Fcar (character);
      to = Fcdr (character);
      CHECK_CHARACTER (from);
      CHECK_CHARACTER (to);
      range_list = Fcons (character, Qnil);
    }
  else
    {
      Lisp_Object script_list;
      Lisp_Object val;

      CHECK_SYMBOL (character);
      range_list = Qnil;
      script_list = XCHAR_TABLE (Vchar_script_table)->extras[0];
      if (! NILP (Fmemq (character, script_list)))
	{
	  val = Fcons (character, Qnil);
	  map_char_table (accumulate_script_ranges, Qnil, Vchar_script_table,
			  val);
	  range_list = XCDR (val);
	}
      else if (CHARSETP (character))
	{
	  struct charset *charset;

	  CHECK_CHARSET_GET_CHARSET (character, charset);
	  if (EQ (character, Qascii))
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
	      map_charset_chars (set_fontset_font, Qnil,
				 list3 (fontset, font_def, add), charset,
				 CHARSET_MIN_CODE (charset),
				 CHARSET_MAX_CODE (charset));
	      return Qnil;
	    }
	}

      if (NILP (range_list))
	error ("Invalid script or charset name: %s",
	       SDATA (SYMBOL_NAME (character)));
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

  id = fs_query_fontset (name, 0);
  if (id < 0)
    {
      name = Fdowncase (name);
      val = split_font_name_into_vector (name);
      if (NILP (val) || NILP (AREF (val, 12)) || NILP (AREF (val, 13)))
	error ("Fontset name must be in XLFD format");
      if (strcmp (SDATA (AREF (val, 12)), "fontset"))
	error ("Registry field of fontset name must be \"fontset\"");
      Vfontset_alias_alist
	= Fcons (Fcons (name,
			concat2 (concat2 (AREF (val, 12), build_string ("-")),
				 AREF (val, 13))),
		 Vfontset_alias_alist);
      ASET (val, 12, build_string ("iso8859-1"));
      fontset = make_fontset (Qnil, name, Qnil);
      FONTSET_ASCII (fontset) = build_font_name_from_vector (val);
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
   (FONTNAME . FONTSET-ID).  */
static Lisp_Object auto_fontset_alist;

int
new_fontset_from_font_name (Lisp_Object fontname)
{
  Lisp_Object val;
  Lisp_Object name;
  Lisp_Object vec;
  int id;

  fontname = Fdowncase (fontname);
  val = Fassoc (fontname, auto_fontset_alist);
  if (CONSP (val))
    return XINT (XCDR (val));

  vec = split_font_name_into_vector (fontname);
  if ( NILP (vec))
    vec = Fmake_vector (make_number (14), build_string (""));
  ASET (vec, 12, build_string ("fontset"));
  if (NILP (auto_fontset_alist))
    {
      ASET (vec, 13, build_string ("startup"));
      name = build_font_name_from_vector (vec);
    }
  else
    {
      char temp[20];
      int len = Flength (auto_fontset_alist);

      sprintf (temp, "auto%d", len);
      ASET (vec, 13, build_string (temp));
      name = build_font_name_from_vector (vec);
    }
  name = Fnew_fontset (name, Fcons (Fcons (Qascii, Fcons (fontname, Qnil)),
				    Qnil));
  id = fs_query_fontset (name, 0);
  auto_fontset_alist
    = Fcons (Fcons (fontname, make_number (id)), auto_fontset_alist);
  return id;
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
  struct font_info *fontp;
  Lisp_Object info;

  (*check_window_system_func) ();

  CHECK_STRING (name);
  name = Fdowncase (name);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  if (!query_font_func)
    error ("Font query function is not supported");

  fontp = (*query_font_func) (f, SDATA (name));
  if (!fontp)
    return Qnil;

  info = Fmake_vector (make_number (7), Qnil);

  XVECTOR (info)->contents[0] = build_string (fontp->name);
  XVECTOR (info)->contents[1] = build_string (fontp->full_name);
  XVECTOR (info)->contents[2] = make_number (fontp->size);
  XVECTOR (info)->contents[3] = make_number (fontp->height);
  XVECTOR (info)->contents[4] = make_number (fontp->baseline_offset);
  XVECTOR (info)->contents[5] = make_number (fontp->relative_compose);
  XVECTOR (info)->contents[6] = make_number (fontp->default_ascent);

  return info;
}


/* Return the font name for the character at POSITION in the current
   buffer.  This is computed from all the text properties and overlays
   that apply to POSITION.  It returns nil in the following cases:

   (1) The window system doesn't have a font for the character (thus
   it is displayed by an empty box).

   (2) The character code is invalid.

   (3) The current buffer is not displayed in any window.

   In addition, the returned font name may not take into account of
   such redisplay engine hooks as what used in jit-lock-mode if
   POSITION is currently not visible.  */


DEFUN ("internal-char-font", Finternal_char_font, Sinternal_char_font, 1, 1, 0,
       doc: /* For internal use only.  */)
     (position)
     Lisp_Object position;
{
  int pos, pos_byte, dummy;
  int face_id;
  int c;
  Lisp_Object window;
  struct window *w;
  struct frame *f;
  struct face *face;

  CHECK_NUMBER_COERCE_MARKER (position);
  pos = XINT (position);
  if (pos < BEGV || pos >= ZV)
    args_out_of_range_3 (position, make_number (BEGV), make_number (ZV));
  pos_byte = CHAR_TO_BYTE (pos);
  c = FETCH_CHAR (pos_byte);
  window = Fget_buffer_window (Fcurrent_buffer (), Qnil);
  if (NILP (window))
    return Qnil;
  w = XWINDOW (window);
  f = XFRAME (w->frame);
  face_id = face_at_buffer_position (w, pos, -1, -1, &dummy, pos + 100, 0);
  face_id = FACE_FOR_CHAR (f, FACE_FROM_ID (f, face_id), c);
  face = FACE_FROM_ID (f, face_id);
  return (face->font && face->font_name
	  ? build_string (face->font_name)
	  : Qnil);
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
  Lisp_Object table, val, elt;
  Lisp_Object *realized;
  int n_realized = 0;
  int fallback;
  int c, i, j;

  (*check_window_system_func) ();

  fontset = check_fontset_name (fontset);

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* Recode fontsets realized on FRAME from the base fontset FONTSET
     in the table `realized'.  */
  realized = (Lisp_Object *) alloca (sizeof (Lisp_Object)
				     * ASIZE (Vfontset_table));
  for (i = 0; i < ASIZE (Vfontset_table); i++)
    {
      elt = FONTSET_FROM_ID (i);
      if (!NILP (elt)
	  && EQ (FONTSET_BASE (elt), fontset)
	  && EQ (FONTSET_FRAME (elt), frame))
	realized[n_realized++] = elt;
    }


  table = Fmake_char_table (Qfontset_info, Qnil);
  XCHAR_TABLE (table)->extras[0] = Fmake_char_table (Qnil, Qnil);
  /* Accumulate information of the fontset in TABLE.  The format of
     each element is ((FONT-SPEC OPENED-FONT ...) ...).  */
  for (fallback = 0; fallback <= 1; fallback++)
    {
      Lisp_Object this_fontset, this_table;

      if (! fallback)
	{
	  this_fontset = fontset;
	  this_table = table;
	}
      else
	{
	  this_fontset = Vdefault_fontset;
	  this_table = XCHAR_TABLE (table)->extras[0];
#if 0
	  for (i = 0; i < n_realized; i++)
	    realized[i] = FONTSET_FALLBACK (realized[i]);
#endif
	}
      for (c = 0; c <= MAX_5_BYTE_CHAR; )
	{
	  int from, to;

	  val = char_table_ref_and_range (this_fontset, c, &from, &to);
	  if (VECTORP (val))
	    {
	      Lisp_Object alist;

	      /* At first, set ALIST to ((FONT-SPEC) ...).  */
	      for (alist = Qnil, i = 0; i < ASIZE (val); i++)
		alist = Fcons (Fcons (AREF (AREF (val, i), 0), Qnil), alist);
	      alist = Fnreverse (alist);

	      /* Then store opend font names to cdr of each elements.  */
	      for (i = 0; i < n_realized; i++)
		{
		  if (NILP (realized[i]))
		    continue;
		  val = FONTSET_REF (realized[i], c);
		  if (NILP (val))
		    continue;
		  val = XCDR (val);
		  /* Now VAL is [[FACE-ID FONT-INDEX FONT-DEF] ...].
		     If a font of an element is already opened,
		     FONT-INDEX of the element is integer.  */
		  for (j = 0; j < ASIZE (val); j++)
		    if (INTEGERP (AREF (AREF (val, j), 0)))
		      {
			Lisp_Object font_idx;

			font_idx = AREF (AREF (val, j), 1);
			elt = Fassq (AREF (AREF (AREF (val, j), 2), 0), alist);
			if (CONSP (elt)
			    && NILP (Fmemq (font_idx, XCDR(elt))))
			  nconc2 (elt, Fcons (font_idx, Qnil));
		      }
		}
	      for (val = alist; CONSP (val); val = XCDR (val))
		for (elt = XCDR (XCAR (val)); CONSP (elt); elt = XCDR (elt))
		  {
		    struct font_info *font_info
		      = (*get_font_info_func) (f, XINT (XCAR (elt)));
		    XSETCAR (elt, build_string (font_info->full_name));
		  }

	      /* Store ALIST in TBL for characters C..TO.  */
	      char_table_set_range (this_table, c, to, alist);
	    }
	  c = to + 1;
	}
    }

  return table;
}


DEFUN ("fontset-font", Ffontset_font, Sfontset_font, 2, 2, 0,
       doc: /* Return a font name pattern for character CH in fontset NAME.
If NAME is t, find a font name pattern in the default fontset.  */)
     (name, ch)
     Lisp_Object name, ch;
{
  int c;
  Lisp_Object fontset, elt;

  fontset = check_fontset_name (name);

  CHECK_CHARACTER (ch);
  c = XINT (ch);
  elt = FONTSET_REF (fontset, c);
  return Fcopy_sequence (elt);
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
	    ASET (vec, 1, f->name);
	  else
	    ASET (vec, 1, Qt);
	}
      if (!NILP (FONTSET_FALLBACK (fontset)))
	ASET (vec, 2, FONTSET_ID (FONTSET_FALLBACK (fontset)));
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
  if (!load_font_func)
    /* Window system initializer should have set proper functions.  */
    abort ();

  DEFSYM (Qfontset, "fontset");
  Fput (Qfontset, Qchar_table_extra_slots, make_number (8));
  DEFSYM (Qfontset_info, "fontset-info");
  Fput (Qfontset_info, Qchar_table_extra_slots, make_number (1));

  DEFSYM (Qprepend, "prepend");
  DEFSYM (Qappend, "append");

  Vcached_fontset_data = Qnil;
  staticpro (&Vcached_fontset_data);

  Vfontset_table = Fmake_vector (make_number (32), Qnil);
  staticpro (&Vfontset_table);

  Vdefault_fontset = Fmake_char_table (Qfontset, Qnil);
  staticpro (&Vdefault_fontset);
  FONTSET_ID (Vdefault_fontset) = make_number (0);
  FONTSET_NAME (Vdefault_fontset)
    = build_string ("-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default");
  {
    Lisp_Object default_ascii_font;

#if defined (macintosh)
    default_ascii_font
      = build_string ("-apple-monaco-medium-r-*--*-120-*-*-*-*-mac-roman");
#elif defined (WINDOWSNT)
    default_ascii_font
      = build_string ("-*-courier new-normal-r-*-*-*-100-*-*-*-*-iso8859-1");
#else
    default_ascii_font
      = build_string ("-adobe-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1");
#endif
    FONTSET_ASCII (Vdefault_fontset) = default_ascii_font;
  }
  AREF (Vfontset_table, 0) = Vdefault_fontset;
  next_fontset_id = 1;

  auto_fontset_alist = Qnil;
  staticpro (&auto_fontset_alist);

  DEFVAR_LISP ("font-encoding-alist", &Vfont_encoding_alist,
	       doc: /*
Alist of fontname patterns vs the corresponding encoding and repertory info.
Each element looks like (REGEXP . (ENCODING . REPERTORY)),
where ENCODING is a charset or a char-table,
and REPERTORY is a charset, a char-table, or nil.

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
