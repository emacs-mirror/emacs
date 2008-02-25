/* xfont.c -- X core font driver.
   Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.
   Copyright (C) 2006, 2007, 2008
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

#include <config.h>
#include <stdio.h>
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


/* X core font driver.  */

/* Prototypes of support functions.  */
extern void x_clear_errors P_ ((Display *));

static char *xfont_query_font P_ ((Display *, char *, Lisp_Object));
static XCharStruct *xfont_get_pcm P_ ((XFontStruct *, XChar2b *));
static int xfont_registry_charsets P_ ((Lisp_Object, struct charset **,
					struct charset **));

static char *
xfont_query_font (display, name, spec)
     Display *display;
     char *name;
     Lisp_Object spec;
{
  XFontStruct *font;

  BLOCK_INPUT;
  x_catch_errors (display);
  font = XLoadQueryFont (display, name);
  name = NULL;
  if (x_had_errors_p (display))
    {
      /* This error is perhaps due to insufficient memory on X
	 server.  Let's just ignore it.  */
      x_clear_errors (display);
    }
  else if (font)
    {
      unsigned long value;

      if (XGetFontProperty (font, XA_FONT, &value))
	{
	  char *n = (char *) XGetAtomName (display, (Atom) value);

	  if (font_parse_xlfd (n, spec) >= 0)
	    name = n;
	  else
	    XFree (n);
	}
      XFreeFont (display, font);
    }
  x_uncatch_errors ();
  UNBLOCK_INPUT;

  return name;
}


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

static Lisp_Object xfont_get_cache P_ ((FRAME_PTR));
static Lisp_Object xfont_list P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object xfont_match P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object xfont_list_family P_ ((Lisp_Object));
static struct font *xfont_open P_ ((FRAME_PTR, Lisp_Object, int));
static void xfont_close P_ ((FRAME_PTR, struct font *));
static int xfont_prepare_face P_ ((FRAME_PTR, struct face *));
#if 0
static void xfont_done_face P_ ((FRAME_PTR, struct face *));
#endif
static int xfont_has_char P_ ((Lisp_Object, int));
static unsigned xfont_encode_char P_ ((struct font *, int));
static int xfont_text_extents P_ ((struct font *, unsigned *, int,
				   struct font_metrics *));
static int xfont_draw P_ ((struct glyph_string *, int, int, int, int, int));

struct font_driver xfont_driver =
  {
    0,				/* Qx */
    xfont_get_cache,
    xfont_list,
    xfont_match,
    xfont_list_family,
    NULL,
    xfont_open,
    xfont_close,
    xfont_prepare_face,
    NULL /*xfont_done_face*/,
    xfont_has_char,
    xfont_encode_char,
    xfont_text_extents,
    xfont_draw
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

  for (i = 0; i < num_fonts; i++)
    {
      Lisp_Object entity = Fmake_vector (make_number (FONT_ENTITY_MAX), Qnil);
      int result;

      ASET (entity, FONT_TYPE_INDEX, Qx);
      ASET (entity, FONT_FRAME_INDEX, frame);

      result = font_parse_xlfd (names[i], entity);
      if (result < 0)
	{
	  /* This may be an alias name.  Try to get the full XLFD name
	     from XA_FONT property of the font.  */
	  XFontStruct *font = XLoadQueryFont (display, names[i]);
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

      if (result == 0)
	{
	  Lisp_Object val = AREF (entity, FONT_EXTRA_INDEX);
	  char *p = (char *) SDATA (SYMBOL_NAME (val));

	  /* P == "RESX-RESY-SPACING-AVGWIDTH.  We rejust this font if
	     it's an autoscaled one (i.e. RESX > 0 && AVGWIDTH == 0).  */
	  if (atoi (p) > 0)
	    {
	      p += SBYTES (SYMBOL_NAME (val));
	      while (p[-1] != '-') p--;
	      if (atoi (p) == 0)
		continue;
	    }
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
  Lisp_Object list, val, extra, font_name;
  int len;
  char name[256];
  
  extra = AREF (spec, FONT_EXTRA_INDEX);
  font_name = Qnil;
  if (CONSP (extra))
    {
      val = assq_no_quit (QCotf, extra);
      if (! NILP (val))
	return null_vector;
      val = assq_no_quit (QCscript, extra);
      if (! NILP (val))
	return null_vector;
      val = assq_no_quit (QClanguage, extra);
      if (! NILP (val))
	return null_vector;
      val = assq_no_quit (QCname, extra);
      if (CONSP (val))
	font_name = XCDR (val);
    }

  if (STRINGP (font_name)
      && ! strchr ((char *) SDATA (font_name), ':'))
    list = xfont_list_pattern (frame, display, (char *) SDATA (font_name));
  else if ((len = font_unparse_xlfd (spec, 0, name, 256)) < 0)
    return null_vector;
  else
    {
      list = xfont_list_pattern (frame, display, name);
      if (NILP (list))
	{
	  Lisp_Object registry = AREF (spec, FONT_REGISTRY_INDEX);
	  Lisp_Object alter;

	  if (! NILP (registry)
	      && (alter = Fassoc (SYMBOL_NAME (registry),
				  Vface_alternative_font_registry_alist),
		  CONSP (alter)))
	    {
	      /* Pointer to REGISTRY-ENCODING field.  */
	      char *r = name + len - SBYTES (SYMBOL_NAME (registry));

	      for (alter = XCDR (alter); CONSP (alter); alter = XCDR (alter))
		if (STRINGP (XCAR (alter))
		    && ((r - name) + SBYTES (XCAR (alter))) < 255)
		  {
		    strcpy (r, (char *) SDATA (XCAR (alter)));
		    list = xfont_list_pattern (frame, display, name);
		    if (! NILP (list))
		      break;
		  }
	    }
	}
    }

  return (NILP (list) ? null_vector : Fvconcat (1, &list));
}

static Lisp_Object
xfont_match (frame, spec)
     Lisp_Object frame, spec;
{
  FRAME_PTR f = XFRAME (frame);
  Display *display = FRAME_X_DISPLAY_INFO (f)->display;
  Lisp_Object extra, val, entity;
  char *name;
  XFontStruct *xfont;
  unsigned long value;

  extra = AREF (spec, FONT_EXTRA_INDEX);
  val = assq_no_quit (QCname, extra);
  if (! CONSP (val) || ! STRINGP (XCDR (val)))
    return Qnil;

  BLOCK_INPUT;
  entity = Qnil;
  name = (char *) SDATA (XCDR (val));
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
	      entity = Fmake_vector (make_number (FONT_ENTITY_MAX), Qnil);
	      ASET (entity, FONT_TYPE_INDEX, Qx);
	      ASET (entity, FONT_FRAME_INDEX, frame);
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
      family = intern_downcase (p0, last_len);
      if (! memq_no_quit (family, list))
	list = Fcons (family, list);
    }

  XFreeFontNames (names);
  x_uncatch_errors ();
  UNBLOCK_INPUT;

  return list;
}

static struct font *
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
  struct font *font;
  XFontStruct *xfont;

  /* At first, check if we know how to encode characters for this
     font.  */
  registry = AREF (entity, FONT_REGISTRY_INDEX);
  if (font_registry_charsets (registry, &encoding, &repertory) < 0)
    return NULL;

  if (XINT (AREF (entity, FONT_SIZE_INDEX)) != 0)
    pixel_size = XINT (AREF (entity, FONT_SIZE_INDEX));
  len = font_unparse_xlfd (entity, pixel_size, name, 256);
  if (len <= 0)
    return NULL;

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
  x_uncatch_errors ();
  UNBLOCK_INPUT;

  if (! xfont)
    return NULL;
  font = malloc (sizeof (struct font));
  font->format = Qx;
  font->font.font = xfont;
  font->entity = entity;
  font->pixel_size = pixel_size;
  font->driver = &xfont_driver;
  font->font.name = malloc (len + 1);
  if (! font->font.name)
    {
      XFreeFont (display, xfont);
      free (font);
      return NULL;
    }
  bcopy (name, font->font.name, len + 1);
  font->font.charset = encoding->id;
  font->encoding_charset = encoding->id;
  font->repertory_charset = repertory ? repertory->id : -1;
  font->ascent = xfont->ascent;
  font->descent = xfont->descent;

  if (xfont->min_bounds.width == xfont->max_bounds.width)
    {
      /* Fixed width font.  */
      font->font.average_width = font->font.space_width
	= xfont->min_bounds.width;
    }
  else
    {
      XChar2b char2b;
      XCharStruct *pcm;

      char2b.byte1 = 0x00, char2b.byte2 = 0x20;
      pcm = xfont_get_pcm (xfont, &char2b);
      if (pcm)
	font->font.space_width = pcm->width;
      else
	font->font.space_width = xfont->max_bounds.width;

      font->font.average_width
	= (XGetFontProperty (xfont, dpyinfo->Xatom_AVERAGE_WIDTH, &value)
	   ? (long) value / 10 : 0);
      if (font->font.average_width < 0)
	font->font.average_width = - font->font.average_width;
      if (font->font.average_width == 0)
	{
	  if (pcm)
	    {
	      int width = pcm->width;
	      for (char2b.byte2 = 33; char2b.byte2 <= 126; char2b.byte2++)
		if ((pcm = xfont_get_pcm (xfont, &char2b)) != NULL)
		  width += pcm->width;
	      font->font.average_width = width / 95;
	    }
	  else
	    font->font.average_width = xfont->max_bounds.width;
	}
    }
  font->min_width = xfont->min_bounds.width;
  if (font->min_width <= 0)
    font->min_width = font->font.space_width;

  BLOCK_INPUT;
  /* Try to get the full name of FONT.  Put it in FULL_NAME.  */
  if (XGetFontProperty (xfont, XA_FONT, &value))
    {
      char *full_name = NULL, *p0, *p;
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
	{
	  full_name = (char *) malloc (p - p0 + 1);
	  if (full_name)
	    bcopy (p0, full_name, p - p0 + 1);
	}
      XFree (p0);

      if (full_name)
	font->font.full_name = full_name;
      else
	font->font.full_name = font->font.name;
    }
  font->file_name = NULL;

  font->font.size = xfont->max_bounds.width;
  font->font.height = xfont->ascent + xfont->descent;
  font->font.baseline_offset
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_BASELINE_OFFSET, &value)
       ? (long) value : 0);
  font->font.relative_compose
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_RELATIVE_COMPOSE, &value)
       ? (long) value : 0);
  font->font.default_ascent
    = (XGetFontProperty (xfont, dpyinfo->Xatom_MULE_DEFAULT_ASCENT, &value)
       ? (long) value : 0);
  font->font.vertical_centering
    = (STRINGP (Vvertical_centering_font_regexp)
       && (fast_c_string_match_ignore_case
	   (Vvertical_centering_font_regexp, font->font.full_name) >= 0));

  UNBLOCK_INPUT;

  dpyinfo->n_fonts++;

  /* Set global flag fonts_changed_p to non-zero if the font loaded
     has a character with a smaller width than any other character
     before, or if the font loaded has a smaller height than any other
     font loaded before.  If this happens, it will make a glyph matrix
     reallocation necessary.  */
  if (dpyinfo->n_fonts == 1)
    {
      dpyinfo->smallest_font_height = font->font.height;
      dpyinfo->smallest_char_width = font->min_width;
      fonts_changed_p = 1;
    }
  else
    {
      if (dpyinfo->smallest_font_height > font->font.height)
	dpyinfo->smallest_font_height = font->font.height, fonts_changed_p |= 1;
      if (dpyinfo->smallest_char_width > font->min_width)
	dpyinfo->smallest_char_width = font->min_width, fonts_changed_p |= 1;
    }

  return font;
}

static void
xfont_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  BLOCK_INPUT;
  XFreeFont (FRAME_X_DISPLAY (f), font->font.font);
  UNBLOCK_INPUT;

  if (font->font.name != font->font.full_name)
    free (font->font.full_name);
  free (font->font.name);
  free (font);
  FRAME_X_DISPLAY_INFO (f)->n_fonts--;
}

static int
xfont_prepare_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  BLOCK_INPUT;
  XSetFont (FRAME_X_DISPLAY (f), face->gc, face->font->fid);
  UNBLOCK_INPUT;

  return 0;
}

#if 0
static void
xfont_done_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  if (face->extra)
    {
      BLOCK_INPUT;
      XFreeGC (FRAME_X_DISPLAY (f), (GC) face->extra);
      UNBLOCK_INPUT;
      face->extra = NULL;
    }
}
#endif	/* 0 */

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
  return (xfont_get_pcm (font->font.font, &char2b) ? code : FONT_INVALID_CODE);
}

static int
xfont_text_extents (font, code, nglyphs, metrics)
     struct font *font;
     unsigned *code;
     int nglyphs;
     struct font_metrics *metrics;
{
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
      pcm = xfont_get_pcm (font->font.font, &char2b);
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
  XFontStruct *xfont = s->face->font;
  int len = to - from;
  GC gc = s->gc;
  int i;

  if (gc != s->face->gc)
    {
      XGCValues xgcv;
      Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (s->f);

      BLOCK_INPUT;
      XGetGCValues (s->display, gc, GCFont, &xgcv);
      if (xgcv.font != xfont->fid)
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


void
syms_of_xfont ()
{
  xfont_driver.type = Qx;
  register_font_driver (&xfont_driver, NULL);
}

/* arch-tag: 23c5f366-a5ee-44b7-a3b7-90d6da7fd749
   (do not change this comment) */
