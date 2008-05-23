/* xftfont.c -- XFT font driver.
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
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>

#include "lisp.h"
#include "dispextern.h"
#include "xterm.h"
#include "frame.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "fontset.h"
#include "font.h"
#include "ftfont.h"

/* Xft font driver.  */

static Lisp_Object Qxft;

/* The actual structure for Xft font that can be casted to struct
   font.  */

struct xftfont_info
{
  struct font font;
  Display *display;
  int screen;
  XftFont *xftfont;
#ifdef HAVE_LIBOTF
  int maybe_otf;	  /* Flag to tell if this may be OTF or not.  */
  OTF *otf;
#endif	/* HAVE_LIBOTF */
};

/* Structure pointed by (struct face *)->extra  */

struct xftface_info
{
  XftColor xft_fg;		/* color for face->foreground */
  XftColor xft_bg;		/* color for face->background */
};

static void xftfont_get_colors P_ ((FRAME_PTR, struct face *, GC gc,
				    struct xftface_info *,
				    XftColor *fg, XftColor *bg));


/* Setup foreground and background colors of GC into FG and BG.  If
   XFTFACE_INFO is not NULL, reuse the colors in it if possible.  BG
   may be NULL.  */

static void
xftfont_get_colors (f, face, gc, xftface_info, fg, bg)
     FRAME_PTR f;
     struct face *face;
     GC gc;
     struct xftface_info *xftface_info;
     XftColor *fg, *bg;
{
  if (xftface_info && face->gc == gc)
    {
      *fg = xftface_info->xft_fg;
      if (bg)
	*bg = xftface_info->xft_bg;
    }
  else
    {
      XGCValues xgcv;
      int fg_done = 0, bg_done = 0;

      BLOCK_INPUT;
      XGetGCValues (FRAME_X_DISPLAY (f), gc,
		    GCForeground | GCBackground, &xgcv);
      if (xftface_info)
	{
	  if (xgcv.foreground == face->foreground)
	    *fg = xftface_info->xft_fg, fg_done = 1;
	  else if (xgcv.foreground == face->background)
	    *fg = xftface_info->xft_bg, fg_done = 1;
	  if (! bg)
	    bg_done = 1;
	  else if (xgcv.background == face->background)
	    *bg = xftface_info->xft_bg, bg_done = 1;
	  else if (xgcv.background == face->foreground)
	    *bg = xftface_info->xft_fg, bg_done = 1;
	}

      if (fg_done + bg_done < 2)
	{
	  XColor colors[2];

	  colors[0].pixel = fg->pixel = xgcv.foreground;
	  if (bg)
	    colors[1].pixel = bg->pixel = xgcv.background;
	  XQueryColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), colors,
			bg ? 2 : 1);
	  fg->color.alpha = 0xFFFF;
	  fg->color.red = colors[0].red;
	  fg->color.green = colors[0].green;
	  fg->color.blue = colors[0].blue;
	  if (bg)
	    {
	      bg->color.alpha = 0xFFFF;
	      bg->color.red = colors[1].red;
	      bg->color.green = colors[1].green;
	      bg->color.blue = colors[1].blue;
	    }
	}
      UNBLOCK_INPUT;
    }
}


static Lisp_Object xftfont_list P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object xftfont_match P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object xftfont_open P_ ((FRAME_PTR, Lisp_Object, int));
static void xftfont_close P_ ((FRAME_PTR, struct font *));
static int xftfont_prepare_face P_ ((FRAME_PTR, struct face *));
static void xftfont_done_face P_ ((FRAME_PTR, struct face *));
static unsigned xftfont_encode_char P_ ((struct font *, int));
static int xftfont_text_extents P_ ((struct font *, unsigned *, int,
				     struct font_metrics *));
static int xftfont_draw P_ ((struct glyph_string *, int, int, int, int, int));

static int xftfont_anchor_point P_ ((struct font *, unsigned, int,
				     int *, int *));
static int xftfont_end_for_frame P_ ((FRAME_PTR f));

struct font_driver xftfont_driver;

static Lisp_Object
xftfont_list (frame, spec)
     Lisp_Object frame;
     Lisp_Object spec;
{
  Lisp_Object list = ftfont_driver.list (frame, spec), tail;
  
  for (tail = list; CONSP (tail); tail = XCDR (tail))
    ASET (XCAR (tail), FONT_TYPE_INDEX, Qxft);
  return list;
}

static Lisp_Object
xftfont_match (frame, spec)
     Lisp_Object frame;
     Lisp_Object spec;
{
  Lisp_Object entity = ftfont_driver.match (frame, spec);

  if (! NILP (entity))
    ASET (entity, FONT_TYPE_INDEX, Qxft);
  return entity;
}

extern Lisp_Object ftfont_font_format P_ ((FcPattern *));

static FcChar8 ascii_printable[95];

static Lisp_Object
xftfont_open (f, entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object entity;
     int pixel_size;
{
  Display *display = FRAME_X_DISPLAY (f);
  Lisp_Object val, filename, cache, font_object;
  FcPattern *pat = NULL;
  struct xftfont_info *xftfont_info = NULL;
  struct font *font;
  double size = 0;
  XftFont *xftfont = NULL;
  int spacing;
  char name[256];
  int len, i;
  XGlyphInfo extents;
  FT_Face ft_face;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  if (! CONSP (val))
    return Qnil;
  val = XCDR (val);
  filename = XCAR (val);
  size = XINT (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;
  pat = FcPatternCreate ();
  FcPatternAddString (pat, FC_FILE, (FcChar8 *) SDATA (filename));
  FcPatternAddDouble (pat, FC_PIXEL_SIZE, pixel_size);
  /*FcPatternAddBool (pat, FC_ANTIALIAS, FcTrue);*/
  val = AREF (entity, FONT_FAMILY_INDEX);
  if (! NILP (val))
    FcPatternAddString (pat, FC_FAMILY, (FcChar8 *) SDATA (SYMBOL_NAME (val)));
  FcConfigSubstitute (NULL, pat, FcMatchPattern);

  BLOCK_INPUT;
  XftDefaultSubstitute (display, FRAME_X_SCREEN_NUMBER (f), pat);
  xftfont = XftFontOpenPattern (display, pat);
  UNBLOCK_INPUT;
  if (! xftfont)
    return Qnil;
  /* We should not destroy PAT here because it is kept in XFTFONT and
     destroyed automatically when XFTFONT is closed.  */
  font_object = font_make_object (VECSIZE (struct xftfont_info));
  ASET (font_object, FONT_TYPE_INDEX, Qxft);
  for (i = 1; i < FONT_ENTITY_MAX; i++)
    ASET (font_object, i, AREF (entity, i));
  ASET (font_object, FONT_SIZE_INDEX, make_number (size));
  len = font_unparse_xlfd (entity, size, name, 256);
  if (len > 0)
    ASET (font_object, FONT_NAME_INDEX, make_unibyte_string (name, len));
  len = font_unparse_fcname (entity, size, name, 256);
  if (len > 0)
    ASET (font_object, FONT_FULLNAME_INDEX, make_unibyte_string (name, len));
  else
    ASET (font_object, FONT_FULLNAME_INDEX,
	  AREF (font_object, FONT_NAME_INDEX));
  ASET (font_object, FONT_FILE_INDEX, filename);
  ASET (font_object, FONT_FORMAT_INDEX,
	ftfont_font_format (xftfont->pattern));
  font = XFONT_OBJECT (font_object);
  font->pixel_size = pixel_size;
  font->driver = &xftfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  xftfont_info = (struct xftfont_info *) font;
  xftfont_info->display = display;
  xftfont_info->screen = FRAME_X_SCREEN_NUMBER (f);
  xftfont_info->xftfont = xftfont;
  font->pixel_size = size;
  font->driver = &xftfont_driver;
  if (INTEGERP (AREF (entity, FONT_SPACING_INDEX)))
    spacing = XINT (AREF (entity, FONT_SPACING_INDEX));
  else
    spacing = FC_PROPORTIONAL;
  if (! ascii_printable[0])
    {
      int i;
      for (i = 0; i < 95; i++)
	ascii_printable[i] = ' ' + i;
    }
  BLOCK_INPUT;
  if (spacing != FC_PROPORTIONAL)
    {
      font->min_width = font->average_width = font->space_width
	= xftfont->max_advance_width;
      XftTextExtents8 (display, xftfont, ascii_printable + 1, 94, &extents);
    }
  else
    {
      XftTextExtents8 (display, xftfont, ascii_printable, 1, &extents);
      font->space_width = extents.xOff;
      if (font->space_width <= 0)
	/* dirty workaround */
	font->space_width = pixel_size;	
      XftTextExtents8 (display, xftfont, ascii_printable + 1, 94, &extents);
      font->average_width = (font->space_width + extents.xOff) / 95;
    }
  UNBLOCK_INPUT;

  font->ascent = xftfont->ascent;
  if (font->ascent < extents.y)
    font->ascent = extents.y;
  font->descent = xftfont->descent;
  if (font->descent < extents.height - extents.y)
    font->descent = extents.height - extents.y;
  font->height = font->ascent + font->descent;

  ft_face = XftLockFace (xftfont);
  if (XINT (AREF (entity, FONT_SIZE_INDEX)) == 0)
    {
      int upEM = ft_face->units_per_EM;

      font->underline_position = -ft_face->underline_position * size / upEM;
      font->underline_thickness = -ft_face->underline_thickness * size / upEM;
    }
  else
    {
      font->underline_position = -1;
      font->underline_thickness = 0;
    }
#ifdef HAVE_LIBOTF
  xftfont_info->maybe_otf = ft_face->face_flags & FT_FACE_FLAG_SFNT;
  xftfont_info->otf = NULL;
#endif	/* HAVE_LIBOTF */
  XftUnlockFace (xftfont);

  /* Unfortunately Xft doesn't provide a way to get minimum char
     width.  So, we use space_width instead.  */
  font->min_width = font->space_width;

  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = 0;
  font->vertical_centering = 0;

  return font_object;
}

static void
xftfont_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  struct xftfont_info *xftfont_info = (struct xftfont_info *) font;

#ifdef HAVE_LIBOTF
  if (xftfont_info->otf)
    OTF_close (xftfont_info->otf);
#endif
  BLOCK_INPUT;
  XftFontClose (xftfont_info->display, xftfont_info->xftfont);
  UNBLOCK_INPUT;
}

static int
xftfont_prepare_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  struct xftface_info *xftface_info;

#if 0
  /* This doesn't work if face->ascii_face doesn't use an Xft font. */
  if (face != face->ascii_face)
    {
      face->extra = face->ascii_face->extra;
      return 0;
    }
#endif

  xftface_info = malloc (sizeof (struct xftface_info));
  if (! xftface_info)
    return -1;
  xftfont_get_colors (f, face, face->gc, NULL,
		      &xftface_info->xft_fg, &xftface_info->xft_bg);
  face->extra = xftface_info;
  return 0;
}

static void
xftfont_done_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  struct xftface_info *xftface_info;
  
#if 0
  /* This doesn't work if face->ascii_face doesn't use an Xft font. */
  if (face != face->ascii_face
      || ! face->extra)
    return;
#endif

  xftface_info = (struct xftface_info *) face->extra;
  if (xftface_info)
    {
      free (xftface_info);
      face->extra = NULL;
    }
}

static unsigned
xftfont_encode_char (font, c)
     struct font *font;
     int c;
{
  struct xftfont_info *xftfont_info = (struct xftfont_info *) font;
  unsigned code = XftCharIndex (xftfont_info->display, xftfont_info->xftfont,
				(FcChar32) c);
  
  return (code ? code : FONT_INVALID_CODE);
}

static int
xftfont_text_extents (font, code, nglyphs, metrics)
     struct font *font;
     unsigned *code;
     int nglyphs;
     struct font_metrics *metrics;
{
  struct xftfont_info *xftfont_info = (struct xftfont_info *) font;
  XGlyphInfo extents;

  BLOCK_INPUT;
  XftGlyphExtents (xftfont_info->display, xftfont_info->xftfont, code, nglyphs,
		   &extents);
  UNBLOCK_INPUT;
  if (metrics)
    {
      metrics->lbearing = - extents.x;
      metrics->rbearing = - extents.x + extents.width;
      metrics->width = extents.xOff;
      metrics->ascent = extents.y;
      metrics->descent = extents.height - extents.y;
    }
  return extents.xOff;
}

static XftDraw *
xftfont_get_xft_draw (f)
     FRAME_PTR f;
{
  XftDraw *xft_draw = font_get_frame_data (f, &xftfont_driver);;

  if (! xft_draw)
    {
      BLOCK_INPUT;
      xft_draw= XftDrawCreate (FRAME_X_DISPLAY (f),
			       FRAME_X_WINDOW (f),
			       FRAME_X_VISUAL (f),
			       FRAME_X_COLORMAP (f));
      UNBLOCK_INPUT;
      if (! xft_draw)
	abort ();
      font_put_frame_data (f, &xftfont_driver, xft_draw);
    }
  return xft_draw;
}

static int
xftfont_draw (s, from, to, x, y, with_background)
     struct glyph_string *s;
     int from, to, x, y, with_background;
{
  FRAME_PTR f = s->f;
  struct face *face = s->face;
  struct xftfont_info *xftfont_info = (struct xftfont_info *) s->font;
  struct xftface_info *xftface_info = NULL;
  XftDraw *xft_draw = xftfont_get_xft_draw (f);
  FT_UInt *code;
  XftColor fg, bg;
  int len = to - from;
  int i;

  if (s->font == face->font)
    xftface_info = (struct xftface_info *) face->extra;
  xftfont_get_colors (f, face, s->gc, xftface_info,
		      &fg, with_background ? &bg : NULL);
  BLOCK_INPUT;
  if (s->num_clips > 0)
    XftDrawSetClipRectangles (xft_draw, 0, 0, s->clip, s->num_clips);
  else
    XftDrawSetClip (xft_draw, NULL);

  if (with_background)
    XftDrawRect (xft_draw, &bg,
		 x, y - face->font->ascent, s->width, face->font->height);
  code = alloca (sizeof (FT_UInt) * len);
  for (i = 0; i < len; i++)
    code[i] = ((XCHAR2B_BYTE1 (s->char2b + from + i) << 8)
	       | XCHAR2B_BYTE2 (s->char2b + from + i));

  if (s->padding_p)
    for (i = 0; i < len; i++)
      XftDrawGlyphs (xft_draw, &fg, xftfont_info->xftfont,
		     x + i, y, code + i, 1);
  else
    XftDrawGlyphs (xft_draw, &fg, xftfont_info->xftfont,
		   x, y, code, len);
  UNBLOCK_INPUT;

  return len;
}

static int
xftfont_anchor_point (font, code, index, x, y)
     struct font *font;
     unsigned code;
     int index;
     int *x, *y;
{
  struct xftfont_info *xftfont_info = (struct xftfont_info *) font;
  FT_Face ft_face = XftLockFace (xftfont_info->xftfont);
  int result;

  if (FT_Load_Glyph (ft_face, code, FT_LOAD_DEFAULT) != 0)
    result = -1;
  else if (ft_face->glyph->format != FT_GLYPH_FORMAT_OUTLINE)
    result = -1;
  else if (index >= ft_face->glyph->outline.n_points)
    result = -1;
  else
    {
      *x = ft_face->glyph->outline.points[index].x;
      *y = ft_face->glyph->outline.points[index].y;
    }
  XftUnlockFace (xftfont_info->xftfont);
  return result;
}

static int
xftfont_end_for_frame (f)
     FRAME_PTR f;
{
  XftDraw *xft_draw = font_get_frame_data (f, &xftfont_driver);

  if (xft_draw)
    {
      BLOCK_INPUT;
      XftDrawDestroy (xft_draw);
      UNBLOCK_INPUT;
      font_put_frame_data (f, &xftfont_driver, NULL);
    }
  return 0;
}

#ifdef HAVE_LIBOTF
#ifdef HAVE_M17N_FLT
static Lisp_Object
xftfont_shape (lgstring)
     Lisp_Object lgstring;
{
  struct font *font;
  struct xftfont_info *xftfont_info;
  Lisp_Object result;
  FT_Face ft_face;

  CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring), font);
  xftfont_info = (struct xftfont_info *) font;
  if (! xftfont_info->maybe_otf)
    return Qnil;
  ft_face = XftLockFace (xftfont_info->xftfont);
  if (! xftfont_info->otf)
    {
      OTF *otf = OTF_open_ft_face (ft_face);

      if (! otf || OTF_get_table (otf, "head") < 0)
	{
	  if (otf)
	    OTF_close (otf);
	  xftfont_info->maybe_otf = 0;
	  XftUnlockFace (xftfont_info->xftfont);
	  return Qnil;
	}
      xftfont_info->otf = otf;
    }

  result = ftfont_shape_by_flt (lgstring, font, ft_face, xftfont_info->otf);
  XftUnlockFace (xftfont_info->xftfont);
  return result;
}
#endif	/* HAVE_M17N_FLT */
#endif	/* HAVE_LIBOTF */

void
syms_of_xftfont ()
{
  DEFSYM (Qxft, "xft");

  xftfont_driver = ftfont_driver;
  xftfont_driver.type = Qxft;
  xftfont_driver.get_cache = xfont_driver.get_cache;
  xftfont_driver.list = xftfont_list;
  xftfont_driver.match = xftfont_match;
  xftfont_driver.open = xftfont_open;
  xftfont_driver.close = xftfont_close;
  xftfont_driver.prepare_face = xftfont_prepare_face;
  xftfont_driver.done_face = xftfont_done_face;
  xftfont_driver.encode_char = xftfont_encode_char;
  xftfont_driver.text_extents = xftfont_text_extents;
  xftfont_driver.draw = xftfont_draw;
  xftfont_driver.anchor_point = xftfont_anchor_point;
  xftfont_driver.end_for_frame = xftfont_end_for_frame;
#ifdef HAVE_LIBOTF
#ifdef HAVE_M17N_FLT
  xftfont_driver.shape = xftfont_shape;
#endif	/* HAVE_M17N_FLT */
#endif	/* HAVE_LIBOTF */

  register_font_driver (&xftfont_driver, NULL);
}

/* arch-tag: 64ec61bf-7c8e-4fe6-b953-c6a85d5e1605
   (do not change this comment) */
