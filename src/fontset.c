/* Fontset handler.
   Copyright (C) 1995, 1997, 2000 Electrotechnical Laboratory, JAPAN.
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

#ifdef FONTSET_DEBUG
#undef xassert
#define xassert(X)	do {if (!(X)) abort ();} while (0)
#undef INLINE
#define INLINE
#endif

EXFUN (Fclear_face_cache, 1);

/* FONTSET

   A fontset is a collection of font related information to give
   similar appearance (style, etc) of characters.  There are two kinds
   of fontsets; base and realized.  A base fontset is created by
   `new-fontset' from Emacs Lisp explicitly.  A realized fontset is
   created implicitly when a face is realized for ASCII characters.  A
   face is also realized for non-ASCII characters based on an ASCII
   face.  All of non-ASCII faces based on the same ASCII face share
   the same realized fontset.
   
   A fontset object is implemented by a char-table whose default value
   and parent are always nil.

   An element of a base fontset is a font specification of the form:
	[ FAMILY WEIGHT SLANT SWIDTH REGISTRY ] (vector of size 5)
   or
	FONT-NAME (strig)

   FAMILY and REGISTRY are strings.

   WEIGHT, SLANT, and SWIDTH must be symbols that set-face-attribute
   accepts as attribute values for :weight, :slant, :swidth
   respectively.


   A fontset has 7 extra slots.

   The 1st slot is an ID number of the fontset.

   The 2nd slot is a name of the fontset in a base fontset, and nil in
   a realized fontset.

   The 3rd slot is nil in a base fontset, and a base fontset in a
   realized fontset.

   The 4th slot is a frame that the fontset belongs to.  This is nil
   in a base fontset.

   The 5th slot is a cons of 0 and fontname for ASCII characters in a
   base fontset, and nil in a realized face.

   The 6th slot is an alist of a charset vs. the corresponding font
   specification.

   The 7th slot is an alist of a font specification vs. the
   corresponding face ID.  In a base fontset, the face IDs are all
   nil.

   All fontsets are recorded in Vfontset_table.


   DEFAULT FONTSET

   There's a special fontset named `default fontset' which defines the
   default font specifications.  When a base fontset doesn't specify a
   font for a specific character, the corresponding value in the
   default fontset is used.  The format is the same as a base fontset.

   The parent of a realized fontset created for such a face that has
   no fontset is the default fontset.


   These structures are hidden from the other codes than this file.
   The other codes handle fontsets only by their ID numbers.  They
   usually use the variable name `fontset' for IDs.  But, in this
   file, we always use varialbe name `id' for IDs, and name `fontset'
   for the actual fontset objects (i.e. char-table objects).

*/

/********** VARIABLES and FUNCTION PROTOTYPES **********/

extern Lisp_Object Qfont;
Lisp_Object Qfontset;

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

/* Check if any window system is used now.  */
void (*check_window_system_func) P_ ((void));


/* Prototype declarations for static functions.  */
static Lisp_Object make_fontset P_ ((Lisp_Object, Lisp_Object, Lisp_Object));
static int fontset_id_valid_p P_ ((int));
static Lisp_Object fontset_pattern_regexp P_ ((Lisp_Object));


/********** MACROS AND FUNCTIONS TO HANDLE FONTSET **********/

/* Return the fontset with ID.  No check of ID's validness.  */
#define FONTSET_FROM_ID(id) AREF (Vfontset_table, id)

/* Macros to access special values of FONTSET.  */
#define FONTSET_ID(fontset)		XCHAR_TABLE (fontset)->extras[0]

/* Macros to access special values of (base) FONTSET.  */
#define FONTSET_NAME(fontset)		XCHAR_TABLE (fontset)->extras[1]
#define FONTSET_ASCII(fontset)		XCHAR_TABLE (fontset)->extras[4]

#define BASE_FONTSET_P(fontset)		STRINGP (FONTSET_NAME (fontset))

/* Macros to access special values of (realized) FONTSET.  */
#define FONTSET_BASE(fontset)		XCHAR_TABLE (fontset)->extras[2]
#define FONTSET_FRAME(fontset)		XCHAR_TABLE (fontset)->extras[3]
#define FONTSET_CHARSET_ALIST(fontset)	XCHAR_TABLE (fontset)->extras[5]
#define FONTSET_FACE_ALIST(fontset)	XCHAR_TABLE (fontset)->extras[6]


/* Return the element of FONTSET (char-table) at index C (character).  */

#define FONTSET_REF(fontset, c, etl)	((elt) = fontset_ref ((fontset), (c)))

static Lisp_Object
fontset_ref (fontset, c)
     Lisp_Object fontset;
     int c;
{
  Lisp_Object elt;

  while (1)
    {
      elt = CHAR_TABLE_REF (fontset, c);
      if (NILP (elt) && ASCII_CHAR_P (c))
	elt = FONTSET_ASCII (fontset);
      if (NILP (elt))
	{
	  Lisp_Object tail;
	  struct charset *charset;

	  for (tail = FONTSET_CHARSET_ALIST (fontset);
	       CONSP (tail);  tail = XCDR (tail))
	    {
	      charset = CHARSET_FROM_ID (XCAR (XCAR (tail)));
	      if (ENCODE_CHAR (charset, c) != CHARSET_INVALID_CODE (charset))
		{
		  elt = XCDR (XCAR (tail));
		  break;
		}
	    }
	}
      if (! NILP (elt) || EQ (fontset, Vdefault_fontset))
	break;
      fontset = Vdefault_fontset;
    }
  return elt;
}


/* Set the element of FONTSET at index IDX to the value ELT.  IDX may
   be a character or a charset.  */

#define FONTSET_SET(fontset, c, newelt) fontset_set(fontset, c, newelt)

static void
fontset_set (fontset, idx, elt)
     Lisp_Object fontset, idx, elt;
{
  if (SYMBOLP (idx))
    {
      Lisp_Object id, slot, tail;
      
      id = make_number (CHARSET_SYMBOL_ID (idx));
      if (id == charset_ascii)
	Fset_char_table_range (fontset,
			       Fcons (make_number (0), make_number (127)),
			       elt);
      else
	{
	  slot = Fassq (id, FONTSET_CHARSET_ALIST (fontset));
	  if (CONSP (slot))
	    XCDR (slot) = elt;
	  else if (CONSP (FONTSET_CHARSET_ALIST (fontset)))
	    {
	      for (tail = FONTSET_CHARSET_ALIST (fontset);
		   CONSP (XCDR (tail)); tail = XCDR (tail));
	      XCDR (tail) = Fcons (Fcons (id, elt), Qnil);
	    }
	  else
	    FONTSET_CHARSET_ALIST (fontset) = Fcons (Fcons (id, elt), Qnil);
	}
    }
  else
    {
      int from = XINT (XCAR (idx));
      int to = XINT (XCDR (idx));

      if (from == to)
	CHAR_TABLE_SET (fontset, from, elt);
      else
	Fset_char_table_range (fontset, idx, elt);
    }
}


/* Return a face registerd in the realized fontset FONTSET for the
   character C.  Return -1 if a face ID is not yet set.  */

static struct face *
fontset_face (fontset, c)
     Lisp_Object fontset;
     int c;
{
  Lisp_Object base, elt;
  int id;
  struct face *face;

  base = FONTSET_BASE (fontset);
  FONTSET_REF (base, c, elt);

  if (NILP (elt))
    return NULL;

  elt = Fassoc (elt, FONTSET_FACE_ALIST (fontset));
  if (! CONSP (elt))
    return NULL;
  id = XINT (XCDR (elt));
  face = FACE_FROM_ID (XFRAME (FONTSET_FRAME (fontset)), id);
  return face;
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



/********** INTERFACES TO xfaces.c and dispextern.h **********/

/* Return name of the fontset with ID.  */

Lisp_Object
fontset_name (id)
     int id;
{
  Lisp_Object fontset;

  fontset = FONTSET_FROM_ID (id);
  return FONTSET_NAME (fontset);
}


/* Return ASCII font name of the fontset with ID.  */

Lisp_Object
fontset_ascii (id)
     int id;
{
  Lisp_Object fontset;

  fontset= FONTSET_FROM_ID (id);
  return FONTSET_ASCII (fontset);
}


/* Free fontset of FACE defined on frame F.  Called from
   free_realized_face.  */

void
free_face_fontset (f, face)
     FRAME_PTR f;
     struct face *face;
{
  AREF (Vfontset_table, face->fontset) = Qnil;
  if (face->fontset < next_fontset_id)
    next_fontset_id = face->fontset;
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
  return (face == fontset_face (fontset, c));
}


/* Return ID of face suitable for displaying character C on frame F.
   The selection of face is done based on the fontset of FACE.  FACE
   must be reazlied for ASCII characters in advance.  Called from the
   macro FACE_FOR_CHAR when C is not an ASCII character.  */

int
face_for_char (f, face, c)
     FRAME_PTR f;
     struct face *face;
     int c;
{
  Lisp_Object fontset;
  struct face *new_face;

  xassert (fontset_id_valid_p (face->fontset));
  fontset = FONTSET_FROM_ID (face->fontset);
  xassert (!BASE_FONTSET_P (fontset));

  new_face = fontset_face (fontset, c);
  if (new_face)
    return new_face->id;

  /* No face is recorded for C in the fontset of FACE.  Make a new
     realized face for C that has the same fontset.  */
  return lookup_face (f, face->lface, c, face);
}


/* Make a realized fontset for ASCII face FACE on frame F from the
   base fontset BASE_FONTSET_ID.  If BASE_FONTSET_ID is -1, use the
   default fontset as the base.  Value is the id of the new fontset.
   Called from realize_x_face.  */

int
make_fontset_for_ascii_face (f, base_fontset_id)
     FRAME_PTR f;
     int base_fontset_id;
{
  Lisp_Object base_fontset, fontset, frame;

  XSETFRAME (frame, f);
  if (base_fontset_id >= 0)
    {
      base_fontset = FONTSET_FROM_ID (base_fontset_id);
      if (!BASE_FONTSET_P (base_fontset))
	base_fontset = FONTSET_BASE (base_fontset);
      xassert (BASE_FONTSET_P (base_fontset));
    }
  else
    base_fontset = Vdefault_fontset;

  fontset = make_fontset (frame, Qnil, base_fontset);
  return XINT (FONTSET_ID (fontset));
}


/* Return FONT-SPEC recorded in the fontset of FACE for character C.
   If FACE is null, or the fontset doesn't contain information about
   C, get the font name pattern from the default fontset.  Called from
   choose_face_font.  */

Lisp_Object
fontset_font_pattern (f, face, c)
     FRAME_PTR f;
     struct face *face;
     int c;
{
  Lisp_Object fontset, base, elt;
  int id = face ? face->fontset : -1;

  if (id >= 0)
    {
      fontset = FONTSET_FROM_ID (id);
      xassert (!BASE_FONTSET_P (fontset));
      base = FONTSET_BASE (fontset);
    }
  else
    {
      base = Vdefault_fontset;
    }

  FONTSET_REF (base, c, elt);
  if (face && ! NILP (elt))
    {
      Lisp_Object slot;

      slot = Fassoc (elt, FONTSET_FACE_ALIST (fontset));
      if (CONSP (slot))
	XSETCDR (slot, make_number (face->id));
      FONTSET_FACE_ALIST (fontset)      
	= Fcons (Fcons (elt, make_number (face->id)),
		 FONTSET_FACE_ALIST (fontset));
    }
  return elt;
}


#if defined(WINDOWSNT) && defined (_MSC_VER)
#pragma optimize("", off)
#endif

/* Load a font named FONTNAME on frame F.  Return a pointer to the
   struct font_info of the loaded font.  If loading fails, return
   NULL.  */

struct font_info *
fs_load_font (f, fontname)
     FRAME_PTR f;
     char *fontname;
{
  Lisp_Object tail, elt;
  struct font_info *fontp;

  if (!fontname)
    /* No way to get fontname.  */
    return 0;

  fontp = (*load_font_func) (f, fontname, 0);
  if (!fontp)
    return NULL;

  fontname = fontp->full_name;
  /* Fill in members (charset, vertical_centering, encoding, etc) of
     font_info structure that are not set by (*load_font_func).  */
  for (tail = Vfont_encoding_alist; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      if (STRINGP (XCAR (elt)) && CHARSETP (XCDR (elt))
	  && fast_c_string_match_ignore_case (XCAR (elt), fontname) >= 0)
	{
	  fontp->charset = CHARSET_SYMBOL_ID (XCDR (elt));
	  break;
	}
    }
  if (! CONSP (tail))
    return NULL;

  fontp->vertical_centering
    = (STRINGP (Vvertical_centering_font_regexp)
       && (fast_c_string_match_ignore_case
	   (Vvertical_centering_font_regexp, fontname) >= 0));

  fontp->font_encoder = NULL;

  if (find_ccl_program_func)
    (*find_ccl_program_func) (fontp);

  return fontp;
}

#if defined(WINDOWSNT) && defined (_MSC_VER)
#pragma optimize("", on)
#endif


/* Cache data used by fontset_pattern_regexp.  The car part is a
   pattern string containing at least one wild card, the cdr part is
   the corresponding regular expression.  */
static Lisp_Object Vcached_fontset_data;

#define CACHED_FONTSET_NAME (XSTRING (XCAR (Vcached_fontset_data))->data)
#define CACHED_FONTSET_REGEX (XCDR (Vcached_fontset_data))

/* If fontset name PATTERN contains any wild card, return regular
   expression corresponding to PATTERN.  */

static Lisp_Object
fontset_pattern_regexp (pattern)
     Lisp_Object pattern;
{
  if (!index (XSTRING (pattern)->data, '*')
      && !index (XSTRING (pattern)->data, '?'))
    /* PATTERN does not contain any wild cards.  */
    return Qnil;

  if (!CONSP (Vcached_fontset_data)
      || strcmp (XSTRING (pattern)->data, CACHED_FONTSET_NAME))
    {
      /* We must at first update the cached data.  */
      char *regex = (char *) alloca (XSTRING (pattern)->size * 2 + 3);
      char *p0, *p1 = regex;

      /* Convert "*" to ".*", "?" to ".".  */
      *p1++ = '^';
      for (p0 = (char *) XSTRING (pattern)->data; *p0; p0++)
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

      Vcached_fontset_data = Fcons (build_string (XSTRING (pattern)->data),
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

      this_name = XSTRING (FONTSET_NAME (fontset))->data;
      if (regexpp
	  ? fast_c_string_match_ignore_case (name, this_name) >= 0
	  : !strcmp (XSTRING (name)->data, this_name))
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

  if (XSTRING (pattern)->size == 0)
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
      name = XSTRING (FONTSET_NAME (fontset))->data;

      if (!NILP (regexp)
	  ? (fast_c_string_match_ignore_case (regexp, name) < 0)
	  : strcmp (XSTRING (pattern)->data, name))
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
    error ("Fontset `%s' does not exist", XSTRING (name)->data);
  return FONTSET_FROM_ID (id);
}

DEFUN ("set-fontset-font", Fset_fontset_font, Sset_fontset_font, 3, 4, 0,
       doc: /* Modify fontset NAME to use FONT-SPEC for characters of CHARSETS.

CHARSET may be a cons; (FROM . TO), where FROM and TO are characters.
In that case, use FONT-SPEC for all characters in the range FROM and
TO (inclusive).

FONT-SPEC is be a vector; [ FAMILY WEIGHT SLANT WIDTH ADSTYLE REGISTRY ]

FONT-SPEC may be a cons; (FAMILY . REGISTRY), where FAMILY is a family
name of a font, REGSITRY is a registry name of a font.

FONT-SPEC may be a font name string.  */)
     (name, charset, font_spec, frame)
     Lisp_Object name, charset, font_spec, frame;
{
  Lisp_Object fontset;
  Lisp_Object family, registry;

  fontset = check_fontset_name (name);

  if (VECTORP (font_spec))
    {
      int i;
      Lisp_Object val;

      font_spec = Fcopy_sequence (font_spec);
      for (i = 0; i < 5; i++)
	{
	  val = Faref (font_spec, make_number (i));
	  if (! NILP (val))
	    {
	      CHECK_STRING (val);
	      ASET (font_spec, i, Fdowncase (val));
	    }
	}
      val = Faref (font_spec, make_number (5));
      CHECK_STRING (val);
      ASET (font_spec, 5, Fdowncase (val));
    }
  else if (STRINGP (font_spec))
    font_spec = Fdowncase (font_spec);
  else if (CONSP (font_spec))
    {
      CHECK_CONS (font_spec);
      family = XCAR (font_spec);
      registry = XCDR (font_spec);
      font_spec = Fmake_vector (make_number (6), Qnil);
      if (!NILP (family))
	{
	  CHECK_STRING (family);
	  ASET (font_spec, 0, Fdowncase (family));
	}
      CHECK_STRING (registry);
      ASET (font_spec, 5, Fdowncase (registry));
    }

  if (SYMBOLP (charset))
    {
      CHECK_CHARSET (charset);
    }
  else
    {
      Lisp_Object from, to;

      /* CHARSET should be (FROM . TO).  */
      from = Fcar (charset);
      to = Fcdr (charset);
      CHECK_CHARACTER (from);
      CHECK_CHARACTER (to);
    }

  /* The arg FRAME is kept for backward compatibility.  We only check
     the validity.  */
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame);

  FONTSET_SET (fontset, charset, font_spec);

  /* Free all realized fontsets whose base is FONTSET.  This way, the
     specified character(s) are surely redisplayed by a correct
     font.  */
  free_realized_fontsets (fontset);

  return Qnil;
}


DEFUN ("new-fontset", Fnew_fontset, Snew_fontset, 2, 2, 0,
       doc: /* Create a new fontset NAME from font information in FONTLIST.

FONTLIST is an alist of charsets vs corresponding font specifications.
Each element of FONTLIST has the form (CHARSET . FONT-SPEC), where
a character of CHARSET is displayed by a font that matches FONT-SPEC.

FONT-SPEC is a vector [ FAMILY WEIGHT SLANT WIDTH ADSTYLE REGISTRY ], where
FAMILY is a string specifying the font family,
WEIGHT is a string specifying the weight of the font,
SLANT is a string specifying the slant of the font,
WIDTH is a string specifying the width of the font,
ADSTYLE is a string specifying the adstyle of the font,
REGISTRY is a string specifying the charset-registry of the font.

See also the documentation of `set-face-attribute' for the detail of
these vector elements.

FONT-SPEC may be a font name (string).  */)
  (name, fontlist)
     Lisp_Object name, fontlist;
{
  Lisp_Object fontset, ascii_font;
  Lisp_Object tem, tail;

  CHECK_STRING (name);
  CHECK_LIST (fontlist);

  name = Fdowncase (name);
  tem = Fquery_fontset (name, Qnil);
  if (! NILP (tem))
    free_realized_fontsets (tem);

  fontset = make_fontset (Qnil, name, Qnil);

  /* Check the validity of FONTLIST.  */
  ascii_font = Fcdr (Fassq (Qascii, fontlist));
  if (NILP (ascii_font))
    error ("No ascii font specified");
  if (! STRINGP (ascii_font))
    ascii_font = generate_ascii_font (name, ascii_font);

  fontlist = Fcopy_sequence (fontlist);
  for (tail = fontlist; ! NILP (tail); tail = Fcdr (tail))
    Fset_fontset_font (name, Fcar (Fcar (tail)), Fcdr (Fcar (tail)), Qnil);

  FONTSET_ASCII (fontset) = ascii_font;

  return name;
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

  fontp = (*query_font_func) (f, XSTRING (name)->data);
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


#if 0				/* unused */
/* Called from Ffontset_info via map_char_table on each leaf of
   fontset.  ARG is a list (LAST FONT-INFO ...), where LAST is `(last
   ARG)' and FONT-INFOs have this form:
	(CHAR FONT-SPEC) or ((FROM . TO) FONT-SPEC)
   The current leaf is indexed by CHARACTER and has value ELT.  This
   function add the information of the current leaf to ARG by
   appending a new element or modifying the last element..  */

static void
accumulate_font_info (arg, character, elt)
     Lisp_Object arg, character, elt;
{
  Lisp_Object last, last_char, last_elt;

  if (!CONSP (elt) && !SINGLE_BYTE_CHAR_P (XINT (character)))
    FONTSET_REF (Vdefault_fontset, XINT (character), elt);
  if (!CONSP (elt))
    return;
  last = XCAR (arg);
  last_char = XCAR (XCAR (last));
  last_elt = XCAR (XCDR (XCAR (last)));
  elt = XCDR (elt);
  if (!NILP (Fequal (elt, last_elt)))
    {
      struct charset *this_charset = CHAR_CHARSET (XINT (character));

      if (CONSP (last_char))	/* LAST_CHAR == (FROM . TO)  */
	{
	  if (this_charset == CHAR_CHARSET (XINT (XCAR (last_char))))
	    {
	      XSETCDR (last_char, character);
	      return;
	    }
	}
      else if (XINT (last_char) == XINT (character))
	return;
      else if (this_charset == CHAR_CHARSET (XINT (last_char)))
	{
	  XSETCAR (XCAR (last), Fcons (last_char, character));
	  return;
	}
    }
  XSETCDR (last, Fcons (Fcons (character, Fcons (elt, Qnil)), Qnil));
  XSETCAR (arg, XCDR (last));
}
#endif /* 0 */

DEFUN ("fontset-info", Ffontset_info, Sfontset_info, 1, 2, 0,
       doc: /* Return information about a fontset named NAME on frame FRAME.
The value is a vector:
  [ SIZE HEIGHT ((CHARSET-OR-RANGE FONT-SPEC OPENED ...) ...) ],
where,
  SIZE is the maximum bound width of ASCII font in the fontset,
  HEIGHT is the maximum bound height of ASCII font in the fontset,
  CHARSET-OR-RANGE is a charset or a cons of two characters specifying
    the range of characters.
  FONT-SPEC is a fontname pattern string or a vector
    [ FAMILY WEIGHT SLANT WIDTH ADSTYLE REGISTRY ].
    See the documentation of `new-fontset' for the meanings those elements.
  OPENEDs are names of fonts actually opened.
If the ASCII font is not yet opened, SIZE and HEIGHT are 0.
If FRAME is omitted, it defaults to the currently selected frame.  */)
     (name, frame)
     Lisp_Object name, frame;
{
  Lisp_Object fontset;
  FRAME_PTR f;
  Lisp_Object val, tail, elt;
  Lisp_Object *realized;
  struct font_info *fontp = NULL;
  int n_realized = 0;
  int i;

  (*check_window_system_func) ();

  fontset = check_fontset_name (name);

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* Recode realized fontsets whose base is FONTSET in the table
     `realized'.  */
  realized = (Lisp_Object *) alloca (sizeof (Lisp_Object)
				     * ASIZE (Vfontset_table));
  for (i = 0; i < ASIZE (Vfontset_table); i++)
    {
      elt = FONTSET_FROM_ID (i);
      if (!NILP (elt)
	  && EQ (FONTSET_BASE (elt), fontset))
	realized[n_realized++] = elt;
    }

  /* Accumulate information of the fontset in VAL.  The format is
     (LAST FONT-INFO FONT-INFO ...), where FONT-INFO is (CHAR-OR-RANGE
     FONT-SPEC).  See the comment for accumulate_font_info for the
     detail.  */
  val = Fcons (Fcons (Qascii, Fcons (FONTSET_ASCII (fontset), Qnil)), Qnil);
  val = Fcons (val, val);
  for (i = 128; i <= MAX_CHAR; )
    {
      Lisp_Object elt;
      int from, to;

      elt = char_table_ref_and_range (fontset, i, &from, &to);
      if (! NILP (elt))
	{
	  elt = Fcons (Fcons (make_number (from), make_number (to)),
		       Fcons (elt, Qnil));
	  XSETCDR (XCAR (val), Fcons (elt, Qnil));
	  XSETCAR (val, XCDR (XCAR (val)));
	}
      i = to + 1;
    }

  for (tail = FONTSET_CHARSET_ALIST (fontset);
       CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      elt = Fcons ((INTEGERP (XCAR (elt))
		    ? CHARSET_NAME (CHARSET_FROM_ID (XCAR (elt)))
		    : XCAR (elt)),
		   Fcons (XCDR (elt), Qnil));
      XSETCDR (XCAR (val), Fcons (elt, Qnil));
      XSETCAR (val, XCDR (XCAR (val)));
    }

  val = XCDR (val);

  /* If fonts are opened for FONT-SPEC, append the names of the fonts to
     FONT-SPEC.  */
  for (tail = val; CONSP (tail); tail = XCDR (tail))
    {
      elt = XCAR (tail);
      for (i = 0; i < n_realized; i++)
	{
	  Lisp_Object face_list, fontname;

	  for (face_list = FONTSET_FACE_ALIST (realized[i]);
	       CONSP (face_list); face_list = XCDR (face_list))
	    {
	      int face_id = XINT (XCDR (XCAR (face_list)));
	      struct face *face = FACE_FROM_ID (f, face_id);

	      if (face->font && face->font_name)
		{
		  fontname = build_string (face->font_name);
		  if (NILP (Fmember (fontname, XCDR (XCDR (elt)))))
		    XSETCDR (XCDR (elt), Fcons (fontname, XCDR (XCDR (elt))));
		}
	    }
	}
    }

  elt = XCDR (XCDR (XCAR (val)));
  if (CONSP (elt))
    fontp = (*query_font_func) (f, XSTRING (XCAR (elt))->data);
  val = Fmake_vector (make_number (3), val);
  AREF (val, 0) = fontp ? make_number (fontp->size) : make_number (0);
  AREF (val, 1) = fontp ? make_number (fontp->height) : make_number (0);
  return val;
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
  FONTSET_REF (fontset, c, elt);
  return elt;
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

void
syms_of_fontset ()
{
  if (!load_font_func)
    /* Window system initializer should have set proper functions.  */
    abort ();

  Qfontset = intern ("fontset");
  staticpro (&Qfontset);
  Fput (Qfontset, Qchar_table_extra_slots, make_number (7));

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

  DEFVAR_LISP ("font-encoding-alist", &Vfont_encoding_alist,
	       doc: /* Alist of fontname patterns vs corresponding encoding info.
Each element looks like (REGEXP . CHARSET), where CHARSET is an
Emacs charset symbol.  */);
  Vfont_encoding_alist = Qnil;

  DEFVAR_LISP ("use-default-ascent", &Vuse_default_ascent,
	       doc: /* Char table of characters whose ascent values should be ignored.
If an entry for a character is non-nil, the ascent value of the glyph
is assumed to be what specified by _MULE_DEFAULT_ASCENT property of a font.

This affects how a composite character which contains
such a character is displayed on screen.  */);
  Vuse_default_ascent = Qnil;

  DEFVAR_LISP ("ignore-relative-composition", &Vignore_relative_composition,
	       doc: /* Char table of characters which is not composed relatively.
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
}
