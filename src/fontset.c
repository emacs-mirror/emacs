/* Fontset handler.
   Copyright (C) 1995, 1997, 2000 Electrotechnical Laboratory, JAPAN.
   Licensed to the Free Software Foundation.

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
#include "buffer.h"
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


/* FONTSET

   A fontset is a collection of font related information to give
   similar appearance (style, size, etc) of characters.  There are two
   kinds of fontsets; base and realized.  A base fontset is created by
   new-fontset from Emacs Lisp explicitly.  A realized fontset is
   created implicitly when a face is realized for ASCII characters.  A
   face is also realized for multibyte characters based on an ASCII
   face.  All of the multibyte faces based on the same ASCII face
   share the same realized fontset.

   A fontset object is implemented by a char-table.

   An element of a base fontset is:
	(INDEX . FONTNAME) or
	(INDEX . (FOUNDRY . REGISTRY ))
   FONTNAME is a font name pattern for the corresponding character.
   FOUNDRY and REGISTRY are respectively foundry and registry fields of
   a font name for the corresponding character.  INDEX specifies for
   which character (or generic character) the element is defined.  It
   may be different from an index to access this element.  For
   instance, if a fontset defines some font for all characters of
   charset `japanese-jisx0208', INDEX is the generic character of this
   charset.  REGISTRY is the

   An element of a realized fontset is FACE-ID which is a face to use
   for displaying the corresponding character.

   All single byte characters (ASCII and 8bit-unibyte) share the same
   element in a fontset.  The element is stored in the first element
   of the fontset.

   To access or set each element, use macros FONTSET_REF and
   FONTSET_SET respectively for efficiency.

   A fontset has 3 extra slots.

   The 1st slot is an ID number of the fontset.

   The 2nd slot is a name of the fontset.  This is nil for a realized
   face.

   The 3rd slot is a frame that the fontset belongs to.  This is nil
   for a default face.

   A parent of a base fontset is nil.  A parent of a realized fontset
   is a base fontset.

   All fontsets are recorded in Vfontset_table.


   DEFAULT FONTSET

   There's a special fontset named `default fontset' which defines a
   default fontname pattern.  When a base fontset doesn't specify a
   font for a specific character, the corresponding value in the
   default fontset is used.  The format is the same as a base fontset.

   The parent of a realized fontset created for such a face that has
   no fontset is the default fontset.


   These structures are hidden from the other codes than this file.
   The other codes handle fontsets only by their ID numbers.  They
   usually use variable name `fontset' for IDs.  But, in this file, we
   always use variable name `id' for IDs, and name `fontset' for the
   actual fontset objects.

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
   font for each characters.  */
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
static Lisp_Object fontset_ref P_ ((Lisp_Object, int));
static void fontset_set P_ ((Lisp_Object, int, Lisp_Object));
static Lisp_Object make_fontset P_ ((Lisp_Object, Lisp_Object, Lisp_Object));
static int fontset_id_valid_p P_ ((int));
static Lisp_Object fontset_pattern_regexp P_ ((Lisp_Object));
static Lisp_Object font_family_registry P_ ((Lisp_Object, int));


/********** MACROS AND FUNCTIONS TO HANDLE FONTSET **********/

/* Return the fontset with ID.  No check of ID's validness.  */
#define FONTSET_FROM_ID(id) AREF (Vfontset_table, id)

/* Macros to access special values of FONTSET.  */
#define FONTSET_ID(fontset)		XCHAR_TABLE (fontset)->extras[0]
#define FONTSET_NAME(fontset)		XCHAR_TABLE (fontset)->extras[1]
#define FONTSET_FRAME(fontset)		XCHAR_TABLE (fontset)->extras[2]
#define FONTSET_ASCII(fontset)		XCHAR_TABLE (fontset)->contents[0]
#define FONTSET_BASE(fontset)		XCHAR_TABLE (fontset)->parent

#define BASE_FONTSET_P(fontset)		NILP (FONTSET_BASE(fontset))


/* Return the element of FONTSET (char-table) at index C (character).  */

#define FONTSET_REF(fontset, c)	fontset_ref (fontset, c)

static Lisp_Object
fontset_ref (fontset, c)
     Lisp_Object fontset;
     int c;
{
  int charset, c1, c2;
  Lisp_Object elt, defalt;

  if (SINGLE_BYTE_CHAR_P (c))
    return FONTSET_ASCII (fontset);

  SPLIT_CHAR (c, charset, c1, c2);
  elt = XCHAR_TABLE (fontset)->contents[charset + 128];
  if (!SUB_CHAR_TABLE_P (elt))
    return elt;
  defalt = XCHAR_TABLE (elt)->defalt;
  if (c1 < 32
      || (elt = XCHAR_TABLE (elt)->contents[c1],
	  NILP (elt)))
    return defalt;
  if (!SUB_CHAR_TABLE_P (elt))
    return elt;
  defalt = XCHAR_TABLE (elt)->defalt;
  if (c2 < 32
      || (elt = XCHAR_TABLE (elt)->contents[c2],
	  NILP (elt)))
    return defalt;
  return elt;
}


#define FONTSET_REF_VIA_BASE(fontset, c) fontset_ref_via_base (fontset, &c)

static Lisp_Object
fontset_ref_via_base (fontset, c)
     Lisp_Object fontset;
     int *c;
{
  int charset, c1, c2;
  Lisp_Object elt;

  if (SINGLE_BYTE_CHAR_P (*c))
    return FONTSET_ASCII (fontset);

  elt = FONTSET_REF (FONTSET_BASE (fontset), *c);
  if (NILP (elt) && ! EQ (fontset, Vdefault_fontset))
    elt = FONTSET_REF (Vdefault_fontset, *c);
  if (NILP (elt))
    return Qnil;

  *c = XINT (XCAR (elt));
  SPLIT_CHAR (*c, charset, c1, c2);
  elt = XCHAR_TABLE (fontset)->contents[charset + 128];
  if (c1 < 32)
    return (SUB_CHAR_TABLE_P (elt) ? XCHAR_TABLE (elt)->defalt : elt);
  if (!SUB_CHAR_TABLE_P (elt))
    return Qnil;
  elt = XCHAR_TABLE (elt)->contents[c1];
  if (c2 < 32)
    return (SUB_CHAR_TABLE_P (elt) ? XCHAR_TABLE (elt)->defalt : elt);
  if (!SUB_CHAR_TABLE_P (elt))
    return Qnil;
  elt = XCHAR_TABLE (elt)->contents[c2];
  return elt;
}


/* Store into the element of FONTSET at index C the value NEWELT.  */
#define FONTSET_SET(fontset, c, newelt) fontset_set(fontset, c, newelt)

static void
fontset_set (fontset, c, newelt)
     Lisp_Object fontset;
     int c;
     Lisp_Object newelt;
{
  int charset, code[3];
  Lisp_Object *elt;
  int i;

  if (SINGLE_BYTE_CHAR_P (c))
    {
      FONTSET_ASCII (fontset) = newelt;
      return;
    }

  SPLIT_CHAR (c, charset, code[0], code[1]);
  code[2] = 0;			/* anchor */
  elt = &XCHAR_TABLE (fontset)->contents[charset + 128];
  for (i = 0; code[i] > 0; i++)
    {
      if (!SUB_CHAR_TABLE_P (*elt))
	*elt = make_sub_char_table (*elt);
      elt = &XCHAR_TABLE (*elt)->contents[code[i]];
    }
  if (SUB_CHAR_TABLE_P (*elt))
    XCHAR_TABLE (*elt)->defalt = newelt;
  else
    *elt = newelt;
}


/* Return a newly created fontset with NAME.  If BASE is nil, make a
   base fontset.  Otherwise make a realized fontset whose parent is
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

      tem = Fmake_vector (make_number (size + 8), Qnil);
      for (i = 0; i < size; i++)
	AREF (tem, i) = AREF (Vfontset_table, i);
      Vfontset_table = tem;
    }

  fontset = Fmake_char_table (Qfontset, Qnil);

  FONTSET_ID (fontset) = make_number (id);
  FONTSET_NAME (fontset) = name;
  FONTSET_FRAME (fontset) = frame;
  FONTSET_BASE (fontset) = base;

  AREF (Vfontset_table, id) = fontset;
  next_fontset_id = id + 1;
  return fontset;
}


/* Return 1 if ID is a valid fontset id, else return 0.  */

static INLINE int
fontset_id_valid_p (id)
     int id;
{
  return (id >= 0 && id < ASIZE (Vfontset_table) - 1);
}


/* Extract `family' and `registry' string from FONTNAME and a cons of
   them.  Actually, `family' may also contain `foundry', `registry'
   may also contain `encoding' of FONTNAME.  But, if FONTNAME doesn't
   conform to XLFD nor explicitely specifies the other fields
   (i.e. not using wildcard `*'), return FONTNAME.  If FORCE is
   nonzero, specifications of the other fields are ignored, and return
   a cons as far as FONTNAME conform to XLFD.  */

static Lisp_Object
font_family_registry (fontname, force)
     Lisp_Object fontname;
     int force;
{
  Lisp_Object family, registry;
  char *p = XSTRING (fontname)->data;
  char *sep[15];
  int i = 0;

  while (*p && i < 15)
    if (*p++ == '-')
      {
	if (!force && i >= 2 && i <= 11 && *p != '*' && p[1] != '-')
	  return fontname;
	sep[i++] = p;
      }
  if (i != 14)
    return fontname;

  family = make_unibyte_string (sep[0], sep[2] - 1 - sep[0]);
  registry = make_unibyte_string (sep[12], p - sep[12]);
  return Fcons (family, registry);
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
  Lisp_Object fontset, elt;
  fontset= FONTSET_FROM_ID (id);
  elt = FONTSET_ASCII (fontset);
  return XCDR (elt);
}


/* Free fontset of FACE.  Called from free_realized_face.  */

void
free_face_fontset (f, face)
     FRAME_PTR f;
     struct face *face;
{
  if (fontset_id_valid_p (face->fontset))
    {
      AREF (Vfontset_table, face->fontset) = Qnil;
      if (face->fontset < next_fontset_id)
	next_fontset_id = face->fontset;
    }
}


/* Return 1 iff FACE is suitable for displaying character C.
   Otherwise return 0.  Called from the macro FACE_SUITABLE_FOR_CHAR_P
   when C is not a single byte character..  */

int
face_suitable_for_char_p (face, c)
     struct face *face;
     int c;
{
  Lisp_Object fontset, elt;

  if (SINGLE_BYTE_CHAR_P (c))
    return (face == face->ascii_face);

  xassert (fontset_id_valid_p (face->fontset));
  fontset = FONTSET_FROM_ID (face->fontset);
  xassert (!BASE_FONTSET_P (fontset));

  elt = FONTSET_REF_VIA_BASE (fontset, c);
  return (!NILP (elt) && face->id == XFASTINT (elt));
}


/* Return ID of face suitable for displaying character C on frame F.
   The selection of face is done based on the fontset of FACE.  FACE
   should already have been realized for ASCII characters.  Called
   from the macro FACE_FOR_CHAR when C is not a single byte character.  */

int
face_for_char (f, face, c)
     FRAME_PTR f;
     struct face *face;
     int c;
{
  Lisp_Object fontset, elt;
  int face_id;

  xassert (fontset_id_valid_p (face->fontset));
  fontset = FONTSET_FROM_ID (face->fontset);
  xassert (!BASE_FONTSET_P (fontset));

  elt = FONTSET_REF_VIA_BASE (fontset, c);
  if (!NILP (elt))
    return XINT (elt);

  /* No face is recorded for C in the fontset of FACE.  Make a new
     realized face for C that has the same fontset.  */
  face_id = lookup_face (f, face->lface, c, face);

  /* Record the face ID in FONTSET at the same index as the
     information in the base fontset.  */
  FONTSET_SET (fontset, c, make_number (face_id));
  return face_id;
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


/* Return the font name pattern for C that is recorded in the fontset
   with ID.  If a font name pattern is specified (instead of a cons of
   family and registry), check if a font can be opened by that pattern
   to get the fullname.  If a font is opened, return that name.
   Otherwise, return nil.  If ID is -1, or the fontset doesn't contain
   information about C, get the registry and encoding of C from the
   default fontset.  Called from choose_face_font.  */

Lisp_Object
fontset_font_pattern (f, id, c)
     FRAME_PTR f;
     int id, c;
{
  Lisp_Object fontset, elt;
  struct font_info *fontp;

  elt = Qnil;
  if (fontset_id_valid_p (id))
    {
      fontset = FONTSET_FROM_ID (id);
      xassert (!BASE_FONTSET_P (fontset));
      fontset = FONTSET_BASE (fontset);
      elt = FONTSET_REF (fontset, c);
    }
  if (NILP (elt))
    elt = FONTSET_REF (Vdefault_fontset, c);

  if (!CONSP (elt))
    return Qnil;
  if (CONSP (XCDR (elt)))
    return XCDR (elt);

  /* The fontset specifies only a font name pattern (not cons of
     family and registry).  If a font can be opened by that pattern,
     return the name of opened font.  Otherwise return nil.  The
     exception is a font for single byte characters.  In that case, we
     return a cons of FAMILY and REGISTRY extracted from the opened
     font name.  */
  elt = XCDR (elt);
  xassert (STRINGP (elt));
  fontp = FS_LOAD_FONT (f, c, XSTRING (elt)->data, -1);
  if (!fontp)
    return Qnil;

  return font_family_registry (build_string (fontp->full_name),
			       SINGLE_BYTE_CHAR_P (c));
}


#if defined(WINDOWSNT) && defined (_MSC_VER)
#pragma optimize("", off)
#endif

/* Load a font named FONTNAME to display character C on frame F.
   Return a pointer to the struct font_info of the loaded font.  If
   loading fails, return NULL.  If FACE is non-zero and a fontset is
   assigned to it, record FACE->id in the fontset for C.  If FONTNAME
   is NULL, the name is taken from the fontset of FACE or what
   specified by ID.  */

struct font_info *
fs_load_font (f, c, fontname, id, face)
     FRAME_PTR f;
     int c;
     char *fontname;
     int id;
     struct face *face;
{
  Lisp_Object fontset;
  Lisp_Object list, elt;
  int size = 0;
  struct font_info *fontp;
  int charset = CHAR_CHARSET (c);

  if (face)
    id = face->fontset;
  if (id < 0)
    fontset = Qnil;
  else
    fontset = FONTSET_FROM_ID (id);

  if (!NILP (fontset)
      && !BASE_FONTSET_P (fontset))
    {
      elt = FONTSET_REF_VIA_BASE (fontset, c);
      if (!NILP (elt))
	{
	  /* A suitable face for C is already recorded, which means
	     that a proper font is already loaded.  */
	  int face_id = XINT (elt);

	  xassert (face_id == face->id);
	  face = FACE_FROM_ID (f, face_id);
	  return (*get_font_info_func) (f, face->font_info_id);
	}

      if (!fontname && charset == CHARSET_ASCII)
	{
	  elt = FONTSET_ASCII (fontset);
	  fontname = XSTRING (XCDR (elt))->data;
	}
    }

  if (!fontname)
    /* No way to get fontname.  */
    return 0;

  fontp = (*load_font_func) (f, fontname, size);
  if (!fontp)
    return 0;

  /* Fill in members (charset, vertical_centering, encoding, etc) of
     font_info structure that are not set by (*load_font_func).  */
  fontp->charset = charset;

  fontp->vertical_centering
    = (STRINGP (Vvertical_centering_font_regexp)
       && (fast_c_string_match_ignore_case
	   (Vvertical_centering_font_regexp, fontp->full_name) >= 0));

  if (fontp->encoding[1] != FONT_ENCODING_NOT_DECIDED)
    {
      /* The font itself tells which code points to be used.  Use this
	 encoding for all other charsets.  */
      int i;

      fontp->encoding[0] = fontp->encoding[1];
      for (i = MIN_CHARSET_OFFICIAL_DIMENSION1; i <= MAX_CHARSET; i++)
	fontp->encoding[i] = fontp->encoding[1];
    }
  else
    {
      /* The font itself doesn't have information about encoding.  */
      int i;

      fontname = fontp->full_name;
      /* By default, encoding of ASCII chars is 0 (i.e. 0x00..0x7F),
	 others is 1 (i.e. 0x80..0xFF).  */
      fontp->encoding[0] = 0;
      for (i = MIN_CHARSET_OFFICIAL_DIMENSION1; i <= MAX_CHARSET; i++)
	fontp->encoding[i] = 1;
      /* Then override them by a specification in Vfont_encoding_alist.  */
      for (list = Vfont_encoding_alist; CONSP (list); list = XCDR (list))
	{
	  elt = XCAR (list);
	  if (CONSP (elt)
	      && STRINGP (XCAR (elt)) && CONSP (XCDR (elt))
	      && (fast_c_string_match_ignore_case (XCAR (elt), fontname)
		  >= 0))
	    {
	      Lisp_Object tmp;

	      for (tmp = XCDR (elt); CONSP (tmp); tmp = XCDR (tmp))
		if (CONSP (XCAR (tmp))
		    && ((i = get_charset_id (XCAR (XCAR (tmp))))
			>= 0)
		    && INTEGERP (XCDR (XCAR (tmp)))
		    && XFASTINT (XCDR (XCAR (tmp))) < 4)
		  fontp->encoding[i]
		    = XFASTINT (XCDR (XCAR (tmp)));
	    }
	}
    }

  fontp->font_encoder = (struct ccl_program *) 0;

  if (find_ccl_program_func)
    (*find_ccl_program_func) (fontp);

  /* If we loaded a font for a face that has fontset, record the face
     ID in the fontset for C.  */
  if (face
      && !NILP (fontset)
      && !BASE_FONTSET_P (fontset))
    FONTSET_SET (fontset, c, make_number (face->id));
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

/* Return a list of base fontset names matching PATTERN on frame F.
   If SIZE is not 0, it is the size (maximum bound width) of fontsets
   to be listed.  */

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

      if (size)
	{
	  struct font_info *fontp;
	  fontp = FS_LOAD_FONT (f, 0, NULL, id);
	  if (!fontp || size != fontp->size)
	    continue;
	}
      val = Fcons (Fcopy_sequence (FONTSET_NAME (fontset)), val);
    }

  return val;
}

DEFUN ("new-fontset", Fnew_fontset, Snew_fontset, 2, 2, 0,
       doc: /* Create a new fontset NAME that contains font information in FONTLIST.
FONTLIST is an alist of charsets vs corresponding font name patterns.  */)
     (name, fontlist)
     Lisp_Object name, fontlist;
{
  Lisp_Object fontset, elements, ascii_font;
  Lisp_Object tem, tail, elt;

  (*check_window_system_func) ();

  CHECK_STRING (name);
  CHECK_LIST (fontlist);

  name = Fdowncase (name);
  tem = Fquery_fontset (name, Qnil);
  if (!NILP (tem))
    error ("Fontset `%s' matches the existing fontset `%s'",
	   XSTRING (name)->data, XSTRING (tem)->data);

  /* Check the validity of FONTLIST while creating a template for
     fontset elements.  */
  elements = ascii_font = Qnil;
  for (tail = fontlist; CONSP (tail); tail = XCDR (tail))
    {
      int c, charset;

      tem = XCAR (tail);
      if (!CONSP (tem)
	  || (charset = get_charset_id (XCAR (tem))) < 0
	  || (!STRINGP (XCDR (tem)) && !CONSP (XCDR (tem))))
	error ("Elements of fontlist must be a cons of charset and font name pattern");

      tem = XCDR (tem);
      if (STRINGP (tem))
	tem = Fdowncase (tem);
      else
	tem = Fcons (Fdowncase (Fcar (tem)), Fdowncase (Fcdr (tem)));
      if (charset == CHARSET_ASCII)
	ascii_font = tem;
      else
	{
	  c = MAKE_CHAR (charset, 0, 0);
	  elements = Fcons (Fcons (make_number (c), tem), elements);
	}
    }

  if (NILP (ascii_font))
    error ("No ASCII font in the fontlist");

  fontset = make_fontset (Qnil, name, Qnil);
  FONTSET_ASCII (fontset) = Fcons (make_number (0), ascii_font);
  for (; CONSP (elements); elements = XCDR (elements))
    {
      elt = XCAR (elements);
      tem = XCDR (elt);
      if (STRINGP (tem))
	tem = font_family_registry (tem, 0);
      tem = Fcons (XCAR (elt), tem);
      FONTSET_SET (fontset, XINT (XCAR (elt)), tem);
    }

  return Qnil;
}


/* Clear all elements of FONTSET for multibyte characters.  */

static void
clear_fontset_elements (fontset)
     Lisp_Object fontset;
{
  int i;

  for (i = CHAR_TABLE_SINGLE_BYTE_SLOTS; i < CHAR_TABLE_ORDINARY_SLOTS; i++)
    XCHAR_TABLE (fontset)->contents[i] = Qnil;
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
       doc: /* Modify fontset NAME to use FONTNAME for CHARACTER.

CHARACTER may be a cons; (FROM . TO), where FROM and TO are
non-generic characters.  In that case, use FONTNAME
for all characters in the range FROM and TO (inclusive).
CHARACTER may be a charset.   In that case, use FONTNAME
for all character in the charsets.

FONTNAME may be a cons; (FAMILY . REGISTRY), where FAMILY is a family
name of a font, REGISTRY is a registry name of a font.  */)
     (name, character, fontname, frame)
     Lisp_Object name, character, fontname, frame;
{
  Lisp_Object fontset, elt;
  Lisp_Object realized;
  int from, to;
  int id;
  Lisp_Object family, registry;

  fontset = check_fontset_name (name);

  if (CONSP (character))
    {
      /* CH should be (FROM . TO) where FROM and TO are non-generic
	 characters.  */
      CHECK_NUMBER_CAR (character);
      CHECK_NUMBER_CDR (character);
      from = XINT (XCAR (character));
      to = XINT (XCDR (character));
      if (!char_valid_p (from, 0) || !char_valid_p (to, 0))
	error ("Character range should be by non-generic characters.");
      if (!NILP (name)
	  && (SINGLE_BYTE_CHAR_P (from) || SINGLE_BYTE_CHAR_P (to)))
	error ("Can't change font for a single byte character");
    }
  else if (SYMBOLP (character))
    {
      elt = Fget (character, Qcharset);
      if (!VECTORP (elt) || ASIZE (elt) < 1 || !NATNUMP (AREF (elt, 0)))
	error ("Invalid charset: %s", (XSTRING (SYMBOL_NAME (character)))->data);
      from = MAKE_CHAR (XINT (AREF (elt, 0)), 0, 0);
      to = from;
    }
  else
    {
      CHECK_NUMBER (character);
      from = XINT (character);
      to = from;
    }
  if (!char_valid_p (from, 1))
    invalid_character (from);
  if (SINGLE_BYTE_CHAR_P (from))
    error ("Can't change font for a single byte character");
  if (from < to)
    {
      if (!char_valid_p (to, 1))
	invalid_character (to);
      if (SINGLE_BYTE_CHAR_P (to))
	error ("Can't change font for a single byte character");
    }

  if (STRINGP (fontname))
    {
      fontname = Fdowncase (fontname);
      elt = Fcons (make_number (from), font_family_registry (fontname, 0));
    }
  else
    {
      CHECK_CONS (fontname);
      family = XCAR (fontname);
      registry = XCDR (fontname);
      if (!NILP (family))
	{
	  CHECK_STRING (family);
	  family = Fdowncase (family);
	}
      if (!NILP (registry))
	{
	  CHECK_STRING (registry);
	  registry = Fdowncase (registry);
	}
      elt = Fcons (make_number (from), Fcons (family, registry));
    }

  /* The arg FRAME is kept for backward compatibility.  We only check
     the validity.  */
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame);

  for (; from <= to; from++)
    FONTSET_SET (fontset, from, elt);
  Foptimize_char_table (fontset);

  /* If there's a realized fontset REALIZED whose parent is FONTSET,
     clear all the elements of REALIZED and free all multibyte faces
     whose fontset is REALIZED.  This way, the specified character(s)
     are surely redisplayed by a correct font.  */
  for (id = 0; id < ASIZE (Vfontset_table); id++)
    {
      realized = AREF (Vfontset_table, id);
      if (!NILP (realized)
	  && !BASE_FONTSET_P (realized)
	  && EQ (FONTSET_BASE (realized), fontset))
	{
	  FRAME_PTR f = XFRAME (FONTSET_FRAME (realized));
	  clear_fontset_elements (realized);
	  free_realized_multibyte_face (f, id);
	}
    }

  return Qnil;
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
  if (! CHAR_VALID_P (c, 0))
    return Qnil;
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
    elt = FONTSET_REF (Vdefault_fontset, XINT (character));
  if (!CONSP (elt))
    return;
  last = XCAR (arg);
  last_char = XCAR (XCAR (last));
  last_elt = XCAR (XCDR (XCAR (last)));
  elt = XCDR (elt);
  if (!NILP (Fequal (elt, last_elt)))
    {
      int this_charset = CHAR_CHARSET (XINT (character));

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


DEFUN ("fontset-info", Ffontset_info, Sfontset_info, 1, 2, 0,
       doc: /* Return information about a fontset named NAME on frame FRAME.
The value is a vector:
  [ SIZE HEIGHT ((CHARSET-OR-RANGE FONT-SPEC OPENED ...) ...) ],
where,
  SIZE is the maximum bound width of ASCII font in the fontset,
  HEIGHT is the maximum bound height of ASCII font in the fontset,
  CHARSET-OR-RANGE is a charset, a character (may be a generic character)
    or a cons of two characters specifying the range of characters.
  FONT-SPEC is a fontname pattern string or a cons (FAMILY . REGISTRY),
    where FAMILY is a `FAMILY' field of a XLFD font name,
    REGISTRY is a `CHARSET_REGISTRY' field of a XLFD font name.
    FAMILY may contain a `FOUNDRY' field at the head.
    REGISTRY may contain a `CHARSET_ENCODING' field at the tail.
  OPENEDs are names of fonts actually opened.
If the ASCII font is not yet opened, SIZE and HEIGHT are 0.
If FRAME is omitted, it defaults to the currently selected frame.  */)
     (name, frame)
     Lisp_Object name, frame;
{
  Lisp_Object fontset;
  FRAME_PTR f;
  Lisp_Object indices[3];
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
  val = Fcons (Fcons (make_number (0),
		      Fcons (XCDR (FONTSET_ASCII (fontset)), Qnil)),
	       Qnil);
  val = Fcons (val, val);
  map_char_table (accumulate_font_info, Qnil, fontset, val, 0, indices);
  val = XCDR (val);

  /* For each FONT-INFO, if CHAR_OR_RANGE (car part) is a generic
     character for a charset, replace it with the charset symbol.  If
     fonts are opened for FONT-SPEC, append the names of the fonts to
     FONT-SPEC.  */
  for (tail = val; CONSP (tail); tail = XCDR (tail))
    {
      int c;
      elt = XCAR (tail);
      if (INTEGERP (XCAR (elt)))
	{
	  int charset, c1, c2;
	  c = XINT (XCAR (elt));
	  SPLIT_CHAR (c, charset, c1, c2);
	  if (c1 == 0)
	    XSETCAR (elt, CHARSET_SYMBOL (charset));
	}
      else
	c = XINT (XCAR (XCAR (elt)));
      for (i = 0; i < n_realized; i++)
	{
	  Lisp_Object face_id, font;
	  struct face *face;

	  face_id = FONTSET_REF_VIA_BASE (realized[i], c);
	  if (INTEGERP (face_id))
	    {
	      face = FACE_FROM_ID (f, XINT (face_id));
	      if (face && face->font && face->font_name)
		{
		  font = build_string (face->font_name);
		  if (NILP (Fmember (font, XCDR (XCDR (elt)))))
		    XSETCDR (XCDR (elt), Fcons (font, XCDR (XCDR (elt))));
		}
	    }
	}
    }

  elt = Fcdr (Fcdr (Fassq (CHARSET_SYMBOL (CHARSET_ASCII), val)));
  if (CONSP (elt))
    {
      elt = XCAR (elt);
      fontp = (*query_font_func) (f, XSTRING (elt)->data);
    }
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

  CHECK_NUMBER (ch);
  c = XINT (ch);
  if (!char_valid_p (c, 1))
    invalid_character (c);

  elt = FONTSET_REF (fontset, c);
  if (CONSP (elt))
    elt = XCDR (elt);

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
  Fput (Qfontset, Qchar_table_extra_slots, make_number (3));

  Vcached_fontset_data = Qnil;
  staticpro (&Vcached_fontset_data);

  Vfontset_table = Fmake_vector (make_number (32), Qnil);
  staticpro (&Vfontset_table);

  Vdefault_fontset = Fmake_char_table (Qfontset, Qnil);
  staticpro (&Vdefault_fontset);
  FONTSET_ID (Vdefault_fontset) = make_number (0);
  FONTSET_NAME (Vdefault_fontset)
    = build_string ("-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default");
#if defined (MAC_OS)
  FONTSET_ASCII (Vdefault_fontset)
    = Fcons (make_number (0),
	     build_string ("-ETL-fixed-medium-r-*--*-160-*-*-*-*-iso8859-1"));
#elif defined (WINDOWSNT)
  FONTSET_ASCII (Vdefault_fontset)
    = Fcons (make_number (0),
	     build_string ("-*-courier new-normal-r-*-*-*-100-*-*-*-*-iso8859-1"));
#else
  FONTSET_ASCII (Vdefault_fontset)
    = Fcons (make_number (0),
	     build_string ("-adobe-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1"));
#endif
  AREF (Vfontset_table, 0) = Vdefault_fontset;
  next_fontset_id = 1;

  DEFVAR_LISP ("font-encoding-alist", &Vfont_encoding_alist,
	       doc: /* Alist of fontname patterns vs corresponding encoding info.
Each element looks like (REGEXP . ENCODING-INFO),
 where ENCODING-INFO is an alist of CHARSET vs ENCODING.
ENCODING is one of the following integer values:
	0: code points 0x20..0x7F or 0x2020..0x7F7F are used,
	1: code points 0xA0..0xFF or 0xA0A0..0xFFFF are used,
	2: code points 0x20A0..0x7FFF are used,
	3: code points 0xA020..0xFF7F are used.  */);
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
