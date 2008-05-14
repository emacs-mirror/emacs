/* xfont.c -- X core font driver.
   Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.
   Copyright (C) 2006, 2007, 2008
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>

#include "lisp.h"
#include "dispextern.h"
#include "xterm.h"
#include "frame.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "fontset.h"
#include "font.h"
#include "ccl.h"


/* X core font driver.  */

struct xfont_info
{
  struct font font;
  Display *display;
  XFontStruct *xfont;
};

/* Prototypes of support functions.  */
extern void x_clear_errors P_ ((Display *));

static XCharStruct *xfont_get_pcm P_ ((XFontStruct *, XChar2b *));
static void xfont_find_ccl_program P_ ((struct font *));
static int xfont_registry_charsets P_ ((Lisp_Object, struct charset **,
					struct charset **));


/* Get metrics of character CHAR2B in XFONT.  Value is null if CHAR2B
   is not contained in the font.  */

static XCharStruct *
xfont_get_pcm (xfont, char2b)
     XFontStruct *xfont;
     XChar2b *char2b;
{
  /* The result metric information.  */
  XCharStruct *pcm = NULL;

  xassert (xfont && char2b);

  if (xfont->per_char != NULL)
    {
      if (xfont->min_byte1 == 0 && xfont->max_byte1 == 0)
	{
	  /* min_char_or_byte2 specifies the linear character index
	     corresponding to the first element of the per_char array,
	     max_char_or_byte2 is the index of the last character.  A
	     character with non-zero CHAR2B->byte1 is not in the font.
	     A character with byte2 less than min_char_or_byte2 or
	     greater max_char_or_byte2 is not in the font.  */
	  if (char2b->byte1 == 0
	      && char2b->byte2 >= xfont->min_char_or_byte2
	      && char2b->byte2 <= xfont->max_char_or_byte2)
	    pcm = xfont->per_char + char2b->byte2 - xfont->min_char_or_byte2;
	}
      else
	{
	  /* If either min_byte1 or max_byte1 are nonzero, both
	     min_char_or_byte2 and max_char_or_byte2 are less than
	     256, and the 2-byte character index values corresponding
	     to the per_char array element N (counting from 0) are:

	     byte1 = N/D + min_byte1
	     byte2 = N\D + min_char_or_byte2

	     where:

	     D = max_char_or_byte2 - min_char_or_byte2 + 1
	     / = integer division
	     \ = integer modulus  */
	  if (char2b->byte1 >= xfont->min_byte1
	      && char2b->byte1 <= xfont->max_byte1
	      && char2b->byte2 >= xfont->min_char_or_byte2
	      && char2b->byte2 <= xfont->max_char_or_byte2)
	    pcm = (xfont->per_char
		   + ((xfont->max_char_or_byte2 - xfont->min_char_or_byte2 + 1)
		      * (char2b->byte1 - xfont->min_byte1))
		   + (char2b->byte2 - xfont->min_char_or_byte2));
	}
    }
  else
    {
      /* If the per_char pointer is null, all glyphs between the first
	 and last character indexes inclusive have the same
	 information, as given by both min_bounds and max_bounds.  */
      if (char2b->byte2 >= xfont->min_char_or_byte2
	  && char2b->byte2 <= xfont->max_char_or_byte2)
	pcm = &xfont->max_bounds;
    }

  return ((pcm == NULL
	   || (pcm->width == 0 && (pcm->rbearing - pcm->lbearing) == 0))
	  ? NULL : pcm);
}

/* Find a CCL program for a font specified by FONTP, and set the member
 `encoder' of the structure.  */

static void
xfont_find_ccl_program (font)
     struct font *font;
{
  Lisp_Object list, elt;

  elt = Qnil;
  for (list = Vfont_ccl_encoder_alist; CONSP (list); list = XCDR (list))
    {
      elt = XCAR (list);
      if (CONSP (elt)
	  && STRINGP (XCAR (elt))
	  && ((fast_string_match_ignore_case (XCAR (elt),
					      font->props[FONT_NAME_INDEX])
	       >= 0)
	      || (fast_string_match_ignore_case (XCAR (elt),
						 font->props[FONT_FULLNAME_INDEX])
		  >= 0)))
	break;
    }

  if (! NILP (list))
    {
      struct ccl_program *ccl
	= (struct ccl_program *) xmalloc (sizeof (struct ccl_program));

      if (setup_ccl_program (ccl, XCDR (elt)) < 0)
	xfree (ccl);
      else
	font->font_encoder = ccl;
    }
}

static Lisp_Object xfont_get_cache P_ ((FRAME_PTR));
static Lisp_Object xfont_list P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object xfont_match P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object xfont_list_family P_ ((Lisp_Object));
static Lisp_Object xfont_open P_ ((FRAME_PTR, Lisp_Object, int));
static void xfont_close P_ ((FRAME_PTR, struct font *));
static int xfont_prepare_face P_ ((FRAME_PTR, struct face *));
static int xfont_has_char P_ ((Lisp_Object, int));
static unsigned xfont_encode_char P_ ((struct font *, int));
static int xfont_text_extents P_ ((struct font *, unsigned *, int,
				   struct font_metrics *));
static int xfont_draw P_ ((struct glyph_string *, int, int, int, int, int));
static int xfont_check P_ ((FRAME_PTR, struct font *));

struct font_driver xfont_driver =
  {
    0,				/* Qx */
    0,				/* case insensitive */
    xfont_get_cache,
    xfont_list,
    xfont_match,
    xfont_list_family,
    NULL,
    xfont_open,
    xfont_close,
    xfont_prepare_face,
    NULL,
    xfont_has_char,
    xfont_encode_char,
    xfont_text_extents,
    xfont_draw,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    xfont_check
  };

extern Lisp_Object QCname;

static Lisp_Object
xfont_get_cache (f)
     FRAME_PTR f;
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  return (dpyinfo->name_list_element);
}

extern Lisp_Object Vface_alternative_font_registry_alist;

static int
compare_font_names (const void *name1, const void *name2)
{
  return strcasecmp (*(const char **) name1, *(const char **) name2);
}

static Lisp_Object xfont_list_pattern P_ ((Lisp_Object, Display *, char *));

static Lisp_Object
xfont_list_pattern (frame, display, pattern)
     Lisp_Object frame;
     Display *display;
     char *pattern;
{
  Lisp_Object list = Qnil;
  int i, limit, num_fonts;
  char **names;

  BLOCK_INPUT;
  x_catch_errors (display);

  for (limit = 512; ; limit *= 2)
    {
      names = XListFonts (display, pattern, limit, &num_fonts);
      if (x_had_errors_p (display))
	{
	  /* This error is perhaps due to insufficient memory on X
	     server.  Let's just ignore it.  */
	  x_clear_errors (display);
	  num_fonts = 0;
	  break;
	}
      if (num_fonts < limit)
	break;
      XFreeFontNames (names);
    }

  if (num_fonts > 0)
    {
      char **indices = alloca (sizeof (char *) * num_fonts);

      for (i = 0; i < num_fonts; i++)
	indices[i] = names[i];
      qsort (indices, num_fonts, sizeof (char *), compare_font_names);

      for (i = 0; i < num_fonts; i++)
	{
	  Lisp_Object entity;
	  int result;

	  if (i > 0 && strcasecmp (indices[i - 1], indices[i]) == 0)
	    continue;

	  entity = font_make_entity ();
	  ASET (entity, FONT_TYPE_INDEX, Qx);

	  result = font_parse_xlfd (indices[i], entity);
	  if (result < 0)
	    {
	      /* This may be an alias name.  Try to get the full XLFD name
		 from XA_FONT property of the font.  */
	      XFontStruct *font = XLoadQueryFont (display, indices[i]);
	      unsigned long value;

	      if (! font)
		continue;
	      if (XGetFontProperty (font, XA_FONT, &value))
		{
		  char *name = (char *) XGetAtomName (display, (Atom) value);
		  int len = strlen (name);

		  /* If DXPC (a Differential X Protocol Compressor)
		     Ver.3.7 is running, XGetAtomName will return null
		     string.  We must avoid such a name.  */
		  if (len > 0)
		    result = font_parse_xlfd (name, entity);
		  XFree (name);
		}
	      XFreeFont (display, font);
	    }

	  if (result == 0
	      /* Avoid auto-scaled fonts.  */
	      && (XINT (AREF (entity, FONT_DPI_INDEX)) == 0
		  || XINT (AREF (entity, FONT_AVGWIDTH_INDEX)) > 0))
	    list = Fcons (entity, list);
	}
    }

  x_uncatch_errors ();
  UNBLOCK_INPUT;

  return list;
}

static Lisp_Object
xfont_list (frame, spec)
     Lisp_Object frame, spec;
{
  FRAME_PTR f = XFRAME (frame);
  Display *display = FRAME_X_DISPLAY_INFO (f)->display;
  Lisp_Object registry, list, val, extra, font_name;
  Lisp_Object dpi, avgwidth;
  int len;
  char name[256];
  
  extra = AREF (spec, FONT_EXTRA_INDEX);
  if (CONSP (extra))
    {
      val = assq_no_quit (QCotf, extra);
      if (! NILP (val))
	return Qnil;
      val = assq_no_quit (QCscript, extra);
      if (! NILP (val))
	return Qnil;
      val = assq_no_quit (QClang, extra);
      if (! NILP (val))
	return Qnil;
    }

  registry = AREF (spec, FONT_REGISTRY_INDEX);
  if (NILP (registry))
    ASET (spec, FONT_REGISTRY_INDEX, Qiso8859_1);
  len = font_unparse_xlfd (spec, 0, name, 256);
  ASET (spec, FONT_REGISTRY_INDEX, registry);
  if (len < 0)
    return Qnil;
  list = xfont_list_pattern (frame, display, name);
  if (NILP (list) && NILP (registry))
    {
      /* Try iso10646-1 */
      char *r = name + len - 9;	/* 9 == strlen (iso8859-1) */

      if (r - name + 10 < 256)	/* 10 == strlen (iso10646-1) */
	{
	  strcpy (r, "iso10646-1");
	  list = xfont_list_pattern (frame, display, name);
	}
    }
  if (NILP (list) && ! NILP (registry))
    {
      Lisp_Object alter;

      if ((alter = Fassoc (SYMBOL_NAME (registry),
			   Vface_alternative_font_registry_alist),
	   CONSP (alter)))
	{
	  /* Pointer to REGISTRY-ENCODING field.  */
	  char *r = name + len - SBYTES (SYMBOL_NAME (registry));

	  for (alter = XCDR (alter); CONSP (alter); alter = XCDR (alter))
	    if (STRINGP (XCAR (alter))
		&& ((r - name) + SBYTES (XCAR (alter))) < 256)
	      {
		strcpy (r, (char *) SDATA (XCAR (alter)));
		list = xfont_list_pattern (frame, display, name);
		if (! NILP (list))
		  break;
	      }
	}
    }

  return list;
}

static Lisp_Object
xfont_match (frame, spec)
     Lisp_Object frame, spec;
{
  FRAME_PTR f = XFRAME (frame);
  Display *display = FRAME_X_DISPLAY_INFO (f)->display;
  Lisp_Object extra, val, entity;
  char buf[256], *name;
  XFontStruct *xfont;
  unsigned long value;

  extra = AREF (spec, FONT_EXTRA_INDEX);
  val = assq_no_quit (QCname, extra);
  if (! CONSP (val) || ! STRINGP (XCDR (val)))
    {
      if (font_unparse_xlfd (spec, 0, buf, 256) < 0)
	return Qnil;
      name = buf;
    }
  else
    name = (char *) SDATA (XCDR (val));

  BLOCK_INPUT;
  entity = Qnil;
  xfont = XLoadQueryFont (display, name);
  if (xfont)
    {
      if (XGetFontProperty (xfont, XA_FONT, &value))
	{
	  int len;

	  name = (char *) XGetAtomName (display, (Atom) value);
	  len = strlen (name);

	  /* If DXPC (a Differential X Protocol Compressor)
	     Ver.3.7 is running, XGetAtomName will return null
	     string.  We must avoid such a name.  */
	  if (len > 0)
	    {
	      entity = font_make_entity ();
	      ASET (entity, FONT_TYPE_INDEX, Qx);
	      if (font_parse_xlfd (name, entity) < 0)
		entity = Qnil;
	    }
	  XFree (name);
	}
      XFreeFont (display, xfont);
    }
  UNBLOCK_INPUT;

  return entity;
}

static int
memq_no_quit (elt, list)
     Lisp_Object elt, list;
{
  while (CONSP (list) && ! EQ (XCAR (list), elt))
    list = XCDR (list);
  return (CONSP (list));
}

static Lisp_Object
xfont_list_family (frame)
     Lisp_Object frame;
{
  FRAME_PTR f = XFRAME (frame);
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  char **names;
  int num_fonts, i;
  Lisp_Object list;
  char *last_family;
  int last_len;

  BLOCK_INPUT;
  x_catch_errors (dpyinfo->display);
  names = XListFonts (dpyinfo->display, "-*-*-*-*-*-*-*-*-*-*-*-*-*-*",
		      0x8000, &num_fonts);
  if (x_had_errors_p (dpyinfo->display))
    {
      /* This error is perhaps due to insufficient memory on X server.
	 Let's just ignore it.  */
      x_clear_errors (dpyinfo->display);
      num_fonts = 0;
    }

  list = Qnil;
  for (i = 0, last_len = 0; i < num_fonts; i++)
    {
      char *p0 = names[i], *p1;
      Lisp_Object family;

      p0++;			/* skip the leading '-' */
      while (*p0 && *p0 != '-') p0++; /* skip foundry */
      if (! *p0)
	continue;
      p1 = ++p0;
      while (*p1 && *p1 != '-') p1++; /* find the end of family */
      if (! *p1 || p1 == p0)
	continue;
      if (last_len == p1 - p0
	  && bcmp (last_family, p0, last_len) == 0)
	continue;
      last_len = p1 - p0;
      last_family = p0;
      family = make_unibyte_string (p0, last_len);
      if (NILP (Fassoc_string (family, list, Qt)))
	list = Fcons (family, list);
    }

  XFreeFontNames (names);
  x_uncatch_errors ();
  UNBLOCK_INPUT;

  return list;
}

extern Lisp_Object QCavgwidth;

static Lisp_Object
xfont_open (f, entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object entity;
     int pixel_size;
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Display *display = dpyinfo->display;
  char name[256];
  int len;
  unsigned long value;
  Lisp_Object registry;
  struct charset *encoding, *repertory;
  Lisp_Object font_object, fullname;
  struct font *font;
  XFontStruct *xfont;
  int i;

  /* At first, check if we know how to encode characters for this
     font.  */
  registry = AREF (entity, FONT_REGISTRY_INDEX);
  if (font_registry_charsets (registry, &encoding, &repertory) < 0)
    return Qnil;

  if (XINT (AREF (entity, FONT_SIZE_INDEX)) != 0)
    pixel_size = XINT (AREF (entity, FONT_SIZE_INDEX));
  else if (pixel_size == 0)
    {
      if (FRAME_FONT (f))
	pixel_size = FRAME_FONT (f)->pixel_size;
      else
	pixel_size = 14;
    }
  len = font_unparse_xlfd (entity, pixel_size, name, 256);
  if (len <= 0)
    return Qnil;

  BLOCK_INPUT;
  x_catch_errors (display);
  xfont = XLoadQueryFont (display, name);
  if (x_had_errors_p (display))
    {
      /* This error is perhaps due to insufficient memory on X server.
	 Let's just ignore it.  */
      x_clear_errors (display);
      xfont = NULL;
    }
  fullname = Qnil;
  /* Try to get the full name of FONT.  */
  if (xfont && XGetFontProperty (xfont, XA_FONT, &value))
    {
      char *p0, *p;
      int dashes = 0;

      p0 = p = (char *) XGetAtomName (FRAME_X_DISPLAY (f), (Atom) value);;
      /* Count the number of dashes in the "full name".
	 If it is too few, this isn't really the font's full name,
	 so don't use it.
	 In X11R4, the fonts did not come with their canonical names
	 stored in them.  */
      while (*p)
	{
	  if (*p == '-')
	    dashes++;
	  p++;
	}

      if (dashes >= 13)
	fullname = Fdowncase (make_unibyte_string (p0, p - p0));
      XFree (p0);
    }
  x_uncatch_errors ();
  UNBLOCK_INPUT;

  if (! xfont)
    return Qnil;

  font_object = font_make_object (VECSIZE (struct xfont_info));
  ASET (font_object, FONT_TYPE_INDEX, Qx);
  if (STRINGP (fullname))
    font_parse_xlfd (SDATA (fullname), font_object);
  for (i = 1; i < FONT_ENTITY_MAX; i++)
    ASET (font_object, i, AREF (entity, i));
  ASET (font_object, FONT_SIZE_INDEX, make_number (pixel_size));
  if (STRINGP (fullname))
    ASET (font_object, FONT_NAME_INDEX, fullname);
  else
    ASET (font_object, FONT_NAME_INDEX, make_unibyte_string (name, len));
  ASET (font_object, FONT_FULLNAME_INDEX, fullname);
  ASET (font_object, FONT_FILE_INDEX, Qnil);
  ASET (font_object, FONT_FORMAT_INDEX, Qx);
  font = XFONT_OBJECT (font_object);
  ((struct xfont_info *) font)->xfont = xfont;
  ((struct xfont_info *) font)->display = FRAME_X_DISPLAY (f);
  font->pixel_size = pixel_size;
  font->driver = &xfont_driver;
  font->encoding_charset = encoding->id;
  font->repertory_charset = repertory ? repertory->id : -1;
  font->ascent = xfont->ascent;
  font->descent = xfont->descent;
  font->height = font->ascent + font->descent;
  font->min_width = xfont->min_bounds.width;
  if (xfont->min_bounds.width == xfont->max_bounds.width)
    {
      /* Fixed width font.  */
      font->average_width = font->space_width = xfont->min_bounds.width;
    }
  else
    {
      XCharStruct *pcm;
      XChar2b char2b;
      Lisp_Object val;

      char2b.byte1 = 0x00, char2b.byte2 = 0x20;
      pcm = xfont_get_pcm (xfont, &char2b);
      if (pcm)
	font->space_width = pcm->width;
      else
	font->space_width = 0;

      val = Ffont_get (font_object, QCavgwidth);
      if (INTEGERP (val))
	font->average_width = XINT (val);
      if (font->average_width < 0)
	font->average_width = - font->average_width;
      if (font->average_width == 0
	  && encoding->ascii_compatible_p)
	{
	  int width = font->space_width, n = pcm != NULL;

	  for (char2b.byte2 = 33; char2b.byte2 <= 126; char2b.byte2++)
	    if ((pcm = xfont_get_pcm (xfont, &char2b)) != NULL)
	      width += pcm->width, n++;
	  font->average_width = width / n;
	}
    }

  BLOCK_INPUT;
  font->underline_thickness
    = (XGetFontProperty (xfont, XA_UNDERLINE_THICKNESS, &value)
       ? (long) value : 0);
  font->underline_position
    = (XGetFontProperty (xfont, XA_UNDERLINE_POSITION, &value)
       ? (long) value : -1);
  font->baseline_offset
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_BASELINE_OFFSET, &value)
       ? (long) value : 0);
  font->relative_compose
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_RELATIVE_COMPOSE, &value)
       ? (long) value : 0);
  font->default_ascent
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_DEFAULT_ASCENT, &value)
       ? (long) value : 0);
  UNBLOCK_INPUT;

  if (NILP (fullname))
    fullname = AREF (font_object, FONT_NAME_INDEX);
  font->vertical_centering
    = (STRINGP (Vvertical_centering_font_regexp)
       && (fast_string_match_ignore_case
	   (Vvertical_centering_font_regexp, fullname) >= 0));

  return font_object;
}

static void
xfont_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  BLOCK_INPUT;
  XFreeFont (FRAME_X_DISPLAY (f), ((struct xfont_info *) font)->xfont);
  UNBLOCK_INPUT;
}

static int
xfont_prepare_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  BLOCK_INPUT;
  XSetFont (FRAME_X_DISPLAY (f), face->gc,
	    ((struct xfont_info *) face->font)->xfont->fid);
  UNBLOCK_INPUT;

  return 0;
}

static int
xfont_has_char (entity, c)
     Lisp_Object entity;
     int c;
{
  Lisp_Object registry = AREF (entity, FONT_REGISTRY_INDEX);
  struct charset *repertory;

  if (font_registry_charsets (registry, NULL, &repertory) < 0)
    return -1;
  if (! repertory)
    return -1;
  return (ENCODE_CHAR (repertory, c) != CHARSET_INVALID_CODE (repertory));
}

static unsigned
xfont_encode_char (font, c)
     struct font *font;
     int c;
{
  XFontStruct *xfont = ((struct xfont_info *) font)->xfont;
  struct charset *charset;
  unsigned code;
  XChar2b char2b;

  charset = CHARSET_FROM_ID (font->encoding_charset);
  code = ENCODE_CHAR (charset, c);
  if (code == CHARSET_INVALID_CODE (charset))
    return FONT_INVALID_CODE;
  if (font->repertory_charset >= 0)
    {
      charset = CHARSET_FROM_ID (font->repertory_charset);
      return (ENCODE_CHAR (charset, c) != CHARSET_INVALID_CODE (charset)
	      ? code : FONT_INVALID_CODE);
    }
  char2b.byte1 = code >> 8;
  char2b.byte2 = code & 0xFF;
  return (xfont_get_pcm (xfont, &char2b) ? code : FONT_INVALID_CODE);
}

static int
xfont_text_extents (font, code, nglyphs, metrics)
     struct font *font;
     unsigned *code;
     int nglyphs;
     struct font_metrics *metrics;
{
  XFontStruct *xfont = ((struct xfont_info *) font)->xfont;
  int width = 0;
  int i, x;

  if (metrics)
    bzero (metrics, sizeof (struct font_metrics));
  for (i = 0, x = 0; i < nglyphs; i++)
    {
      XChar2b char2b;
      static XCharStruct *pcm;

      if (code[i] >= 0x10000)
	continue;
      char2b.byte1 = code[i] >> 8, char2b.byte2 = code[i] & 0xFF;
      pcm = xfont_get_pcm (xfont, &char2b);
      if (! pcm)
	continue;
      if (metrics->lbearing > width + pcm->lbearing)
	metrics->lbearing = width + pcm->lbearing;
      if (metrics->rbearing < width + pcm->rbearing)
	metrics->rbearing = width + pcm->rbearing;
      if (metrics->ascent < pcm->ascent)
	metrics->ascent = pcm->ascent;
      if (metrics->descent < pcm->descent)
	metrics->descent = pcm->descent;
      width += pcm->width;
    }
  if (metrics)
    metrics->width = width;
  return width;
}

static int
xfont_draw (s, from, to, x, y, with_background)
     struct glyph_string *s;
     int from, to, x, y, with_background;
{
  XFontStruct *xfont = ((struct xfont_info *) s->font)->xfont;
  int len = to - from;
  GC gc = s->gc;
  int i;

  if (s->gc != s->face->gc)
    {
      BLOCK_INPUT;
      XSetFont (s->display, gc, xfont->fid);
      UNBLOCK_INPUT;
    }

  if (xfont->min_byte1 == 0 && xfont->max_byte1 == 0)
    {
      char *str;
      USE_SAFE_ALLOCA;

      SAFE_ALLOCA (str, char *, len);
      for (i = 0; i < len ; i++)
	str[i] = XCHAR2B_BYTE2 (s->char2b + from + i);
      BLOCK_INPUT;
      if (with_background > 0)
	{
	  if (s->padding_p)
	    for (i = 0; i < len; i++)
	      XDrawImageString (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
				gc, x + i, y, str + i, 1);
	  else
	    XDrawImageString (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			      gc, x, y, str, len);
	}
      else
	{
	  if (s->padding_p)
	    for (i = 0; i < len; i++)
	      XDrawString (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			   gc, x + i, y, str + i, 1);
	  else
	    XDrawString (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			 gc, x, y, str, len);
	}
      UNBLOCK_INPUT;
      SAFE_FREE ();
      return s->nchars;
    }

  BLOCK_INPUT;
  if (with_background > 0)
    {
      if (s->padding_p)
	for (i = 0; i < len; i++)
	  XDrawImageString16 (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			      gc, x + i, y, s->char2b + from + i, 1);
      else
	XDrawImageString16 (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			    gc, x, y, s->char2b + from, len);
    }
  else
    {
      if (s->padding_p)
	for (i = 0; i < len; i++)
	  XDrawString16 (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
			 gc, x + i, y, s->char2b + from + i, 1);
      else
	XDrawString16 (FRAME_X_DISPLAY (s->f), FRAME_X_WINDOW (s->f),
		       gc, x, y, s->char2b + from, len);
    }
  UNBLOCK_INPUT;

  return len;
}

static int
xfont_check (f, font)
     FRAME_PTR f;
     struct font *font;
{
  struct xfont_info *xfont = (struct xfont_info *) font;

  return (FRAME_X_DISPLAY (f) == xfont->display ? 0 : -1);
}


void
syms_of_xfont ()
{
  xfont_driver.type = Qx;
  register_font_driver (&xfont_driver, NULL);
}

/* arch-tag: 23c5f366-a5ee-44b7-a3b7-90d6da7fd749
   (do not change this comment) */
