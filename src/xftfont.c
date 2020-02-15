/* xftfont.c -- XFT font driver.
   Copyright (C) 2006-2020 Free Software Foundation, Inc.
   Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
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
#include <X11/Xlib.h>
#include <X11/Xft/Xft.h>

#include "lisp.h"
#include "xterm.h"
#include "frame.h"
#include "blockinput.h"
#include "charset.h"
#include "composite.h"
#include "font.h"
#include "ftfont.h"
#include "pdumper.h"

/* Xft font driver.  */

/* Structure pointed by (struct face *)->extra  */

struct xftface_info
{
  XftColor xft_fg;		/* color for face->foreground */
  XftColor xft_bg;		/* color for face->background */
};

/* Setup foreground and background colors of GC into FG and BG.  If
   XFTFACE_INFO is not NULL, reuse the colors in it if possible.  BG
   may be NULL.  */

static void
xftfont_get_colors (struct frame *f, struct face *face, GC gc,
		    struct xftface_info *xftface_info,
		    XftColor *fg, XftColor *bg)
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
      bool fg_done = false, bg_done = false;

      block_input ();
      XGetGCValues (FRAME_X_DISPLAY (f), gc,
		    GCForeground | GCBackground, &xgcv);
      if (xftface_info)
	{
	  if (xgcv.foreground == face->foreground)
	    *fg = xftface_info->xft_fg, fg_done = true;
	  else if (xgcv.foreground == face->background)
	    *fg = xftface_info->xft_bg, fg_done = true;
	  if (! bg)
	    bg_done = true;
	  else if (xgcv.background == face->background)
	    *bg = xftface_info->xft_bg, bg_done = true;
	  else if (xgcv.background == face->foreground)
	    *bg = xftface_info->xft_fg, bg_done = true;
	}

      if (! (fg_done & bg_done))
	{
	  XColor colors[2];

	  colors[0].pixel = fg->pixel = xgcv.foreground;
	  if (bg)
	    colors[1].pixel = bg->pixel = xgcv.background;
	  x_query_colors (f, colors, bg ? 2 : 1);
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
      unblock_input ();
    }
}

static Lisp_Object
xftfont_list (struct frame *f, Lisp_Object spec)
{
  return ftfont_list2 (f, spec, Qxft);
}

static Lisp_Object
xftfont_match (struct frame *f, Lisp_Object spec)
{
  return ftfont_match2 (f, spec, Qxft);
}

static FcChar8 ascii_printable[95];

static Lisp_Object
xftfont_open (struct frame *f, Lisp_Object entity, int pixel_size)
{
  FcResult result;
  Display *display = FRAME_X_DISPLAY (f);
  Lisp_Object val, filename, font_object;
  FcPattern *pat = NULL, *match;
  struct font_info *xftfont_info = NULL;
  struct font *font;
  double size = 0;
  XftFont *xftfont = NULL;
  int spacing;
  XGlyphInfo extents;
  FT_Face ft_face;
  FcMatrix *matrix;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  if (! CONSP (val))
    return Qnil;
  val = XCDR (val);
  filename = XCAR (val);
  size = XFIXNUM (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;

  block_input ();

  pat = ftfont_entity_pattern (entity, pixel_size);
  /* Substitute in values from X resources and XftDefaultSet.  */
  XftDefaultSubstitute (display, FRAME_X_SCREEN_NUMBER (f), pat);
  match = XftFontMatch (display, FRAME_X_SCREEN_NUMBER (f), pat, &result);
  ftfont_fix_match (pat, match);

  FcPatternDestroy (pat);
  xftfont = XftFontOpenPattern (display, match);
  if (!xftfont)
    {
      unblock_input ();
      XftPatternDestroy (match);
      return Qnil;
    }
  ft_face = XftLockFace (xftfont);
  unblock_input ();

  /* We should not destroy PAT here because it is kept in XFTFONT and
     destroyed automatically when XFTFONT is closed.  */
  font_object = font_build_object (VECSIZE (struct font_info),
				   AREF (entity, FONT_TYPE_INDEX),
				   entity, size);
  ASET (font_object, FONT_FILE_INDEX, filename);
  font = XFONT_OBJECT (font_object);
  font->pixel_size = size;
#ifdef HAVE_HARFBUZZ
  if (EQ (AREF (font_object, FONT_TYPE_INDEX), Qxfthb))
    font->driver = &xfthbfont_driver;
  else
#endif	/* HAVE_HARFBUZZ */
  font->driver = &xftfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  xftfont_info = (struct font_info *) font;
  xftfont_info->display = display;
  xftfont_info->xftfont = xftfont;
  xftfont_info->x_display_id = FRAME_DISPLAY_INFO (f)->x_id;
  /* This means that there's no need of transformation.  */
  xftfont_info->matrix.xx = 0;
  if (FcPatternGetMatrix (xftfont->pattern, FC_MATRIX, 0, &matrix)
      == FcResultMatch)
    {
      xftfont_info->matrix.xx = 0x10000L * matrix->xx;
      xftfont_info->matrix.yy = 0x10000L * matrix->yy;
      xftfont_info->matrix.xy = 0x10000L * matrix->xy;
      xftfont_info->matrix.yx = 0x10000L * matrix->yx;
    }
  if (FIXNUMP (AREF (entity, FONT_SPACING_INDEX)))
    spacing = XFIXNUM (AREF (entity, FONT_SPACING_INDEX));
  else
    spacing = FC_PROPORTIONAL;
  if (! ascii_printable[0])
    {
      int ch;
      for (ch = 0; ch < 95; ch++)
	ascii_printable[ch] = ' ' + ch;
    }
  block_input ();

  /* Unfortunately Xft doesn't provide a way to get minimum char
     width.  So, we set min_width to space_width.  */

  if (spacing != FC_PROPORTIONAL
#ifdef FC_DUAL
      && spacing != FC_DUAL
#endif	/* FC_DUAL */
#ifdef FC_CHARCELL
      && spacing != FC_CHARCELL
#endif	/* FC_CHARCELL */
      )
    {
      font->min_width = font->max_width = font->average_width
	= font->space_width = xftfont->max_advance_width;
      XftTextExtents8 (display, xftfont, ascii_printable + 1, 94, &extents);
    }
  else
    {
      XftTextExtents8 (display, xftfont, ascii_printable, 1, &extents);
      font->min_width = font->max_width = font->space_width
	= extents.xOff;
      if (font->space_width <= 0)
	/* dirty workaround */
	font->space_width = pixel_size;
      XftTextExtents8 (display, xftfont, ascii_printable + 1, 94, &extents);
      font->average_width = (font->space_width + extents.xOff) / 95;
    }
  unblock_input ();

  font->ascent = xftfont->ascent;
  font->descent = xftfont->descent;
  /* The following workaround is unnecessary on most systems, and
     causes annoying differences in glyph height between regular and
     bold fonts (see bug#22383).  However, with some fonts, such as
     monaco, removing the workaround results in overlapping vertical
     space of a line, see bug#23360.  As long as the way to reconcile
     these opposites is not known, we provide a user option to work
     around the problem.  */
  if (pixel_size >= 5
      && xft_font_ascent_descent_override)
    {
      /* The above condition is a dirty workaround because
	 XftTextExtents8 behaves strangely for some fonts
	 (e.g. "Dejavu Sans Mono") when pixel_size is less than 5. */
      if (font->ascent < extents.y)
	font->ascent = extents.y;
      if (font->descent < extents.height - extents.y)
	font->descent = extents.height - extents.y;
    }
  font->height = font->ascent + font->descent;

  if (XFIXNUM (AREF (entity, FONT_SIZE_INDEX)) == 0)
    {
      int upEM = ft_face->units_per_EM;

      font->underline_position = -ft_face->underline_position * size / upEM;
      font->underline_thickness = ft_face->underline_thickness * size / upEM;
      if (font->underline_thickness > 2)
	font->underline_position -= font->underline_thickness / 2;
    }
  else
    {
      font->underline_position = -1;
      font->underline_thickness = 0;
    }
#ifdef HAVE_LIBOTF
  xftfont_info->maybe_otf = (ft_face->face_flags & FT_FACE_FLAG_SFNT) != 0;
  xftfont_info->otf = NULL;
#endif	/* HAVE_LIBOTF */
#ifdef HAVE_HARFBUZZ
  xftfont_info->hb_font = NULL;
#endif	/* HAVE_HARFBUZZ */
  xftfont_info->ft_size = ft_face->size;

  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = 0;
  font->vertical_centering = false;
#ifdef FT_BDF_H
  if (! (ft_face->face_flags & FT_FACE_FLAG_SFNT))
    {
      BDF_PropertyRec rec;

      if (FT_Get_BDF_Property (ft_face, "_MULE_BASELINE_OFFSET", &rec) == 0
	  && rec.type == BDF_PROPERTY_TYPE_INTEGER)
	font->baseline_offset = rec.u.integer;
      if (FT_Get_BDF_Property (ft_face, "_MULE_RELATIVE_COMPOSE", &rec) == 0
	  && rec.type == BDF_PROPERTY_TYPE_INTEGER)
	font->relative_compose = rec.u.integer;
      if (FT_Get_BDF_Property (ft_face, "_MULE_DEFAULT_ASCENT", &rec) == 0
	  && rec.type == BDF_PROPERTY_TYPE_INTEGER)
	font->default_ascent = rec.u.integer;
    }
#endif

  return font_object;
}

static void
xftfont_close (struct font *font)
{
  struct x_display_info *xdi;
  struct font_info *xftfont_info = (struct font_info *) font;

#ifdef HAVE_LIBOTF
  if (xftfont_info->otf)
    {
      OTF_close (xftfont_info->otf);
      xftfont_info->otf = NULL;
    }
#endif
#ifdef HAVE_HARFBUZZ
  if (xftfont_info->hb_font)
    {
      hb_font_destroy (xftfont_info->hb_font);
      xftfont_info->hb_font = NULL;
    }
#endif

  /* See comment in xfont_close.  */
  if (xftfont_info->xftfont
      && ((xdi = x_display_info_for_display (xftfont_info->display))
	  && xftfont_info->x_display_id == xdi->x_id))
    {
      block_input ();
      XftUnlockFace (xftfont_info->xftfont);
      XftFontClose (xftfont_info->display, xftfont_info->xftfont);
      unblock_input ();
      xftfont_info->xftfont = NULL;
    }
}

static void
xftfont_prepare_face (struct frame *f, struct face *face)
{
  struct xftface_info *xftface_info;

#if false
  /* This doesn't work if face->ascii_face doesn't use an Xft font. */
  if (face != face->ascii_face)
    {
      face->extra = face->ascii_face->extra;
      return;
    }
#endif

  xftface_info = xmalloc (sizeof *xftface_info);
  xftfont_get_colors (f, face, face->gc, NULL,
		      &xftface_info->xft_fg, &xftface_info->xft_bg);
  face->extra = xftface_info;
}

static void
xftfont_done_face (struct frame *f, struct face *face)
{
  struct xftface_info *xftface_info;

#if false
  /* This doesn't work if face->ascii_face doesn't use an Xft font. */
  if (face != face->ascii_face
      || ! face->extra)
    return;
#endif

  xftface_info = (struct xftface_info *) face->extra;
  if (xftface_info)
    {
      xfree (xftface_info);
      face->extra = NULL;
    }
}

static int
xftfont_has_char (Lisp_Object font, int c)
{
  struct font_info *xftfont_info;
  struct charset *cs = NULL;

  if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qja)
      && charset_jisx0208 >= 0)
    cs = CHARSET_FROM_ID (charset_jisx0208);
  else if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qko)
      && charset_ksc5601 >= 0)
    cs = CHARSET_FROM_ID (charset_ksc5601);
  if (cs)
    return (ENCODE_CHAR (cs, c) != CHARSET_INVALID_CODE (cs));

  if (FONT_ENTITY_P (font))
    return ftfont_has_char (font, c);
  xftfont_info = (struct font_info *) XFONT_OBJECT (font);
  return (XftCharExists (xftfont_info->display, xftfont_info->xftfont,
			 (FcChar32) c) == FcTrue);
}

static unsigned
xftfont_encode_char (struct font *font, int c)
{
  struct font_info *xftfont_info = (struct font_info *) font;
  unsigned code = XftCharIndex (xftfont_info->display, xftfont_info->xftfont,
				(FcChar32) c);

  return (code ? code : FONT_INVALID_CODE);
}

static void
xftfont_text_extents (struct font *font, const unsigned int *code,
		      int nglyphs, struct font_metrics *metrics)
{
  struct font_info *xftfont_info = (struct font_info *) font;
  XGlyphInfo extents;

  block_input ();
  XftGlyphExtents (xftfont_info->display, xftfont_info->xftfont, code, nglyphs,
		   &extents);
  unblock_input ();

  metrics->lbearing = - extents.x;
  metrics->rbearing = - extents.x + extents.width;
  metrics->width = extents.xOff;
  metrics->ascent = extents.y;
  metrics->descent = extents.height - extents.y;
}

static XftDraw *
xftfont_get_xft_draw (struct frame *f)
{
  XftDraw *xft_draw = font_get_frame_data (f, Qxft);

  if (! xft_draw)
    {
      block_input ();
      xft_draw= XftDrawCreate (FRAME_X_DISPLAY (f),
                               FRAME_X_DRAWABLE (f),
			       FRAME_X_VISUAL (f),
			       FRAME_X_COLORMAP (f));
      unblock_input ();
      eassert (xft_draw != NULL);
      font_put_frame_data (f, Qxft, xft_draw);
    }
  return xft_draw;
}

static int
xftfont_draw (struct glyph_string *s, int from, int to, int x, int y,
              bool with_background)
{
  block_input ();

  struct frame *f = s->f;
  struct face *face = s->face;
  struct font_info *xftfont_info = (struct font_info *) s->font;
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
  if (s->num_clips > 0)
    XftDrawSetClipRectangles (xft_draw, 0, 0, s->clip, s->num_clips);
  else
    XftDrawSetClip (xft_draw, NULL);

  if (with_background)
    {
      int height = FONT_HEIGHT (s->font), ascent = FONT_BASE (s->font);

      /* Font's global height and ascent values might be
	 preposterously large for some fonts.  We fix here the case
	 when those fonts are used for display of glyphless
	 characters, because drawing background with font dimensions
	 in those cases makes the display illegible.  There's only one
	 more call to the draw method with with_background set to
	 true, and that's in x_draw_glyph_string_foreground, when
	 drawing the cursor, where we have no such heuristics
	 available.  FIXME.  */
      if (s->first_glyph->type == GLYPHLESS_GLYPH
	  && (s->first_glyph->u.glyphless.method == GLYPHLESS_DISPLAY_HEX_CODE
	      || s->first_glyph->u.glyphless.method == GLYPHLESS_DISPLAY_ACRONYM))
	height = ascent =
	  s->first_glyph->slice.glyphless.lower_yoff
	  - s->first_glyph->slice.glyphless.upper_yoff;
      XftDrawRect (xft_draw, &bg, x, y - ascent, s->width, height);
    }
  code = alloca (sizeof (FT_UInt) * len);
  for (i = 0; i < len; i++)
    code[i] = s->char2b[from + i];

  if (s->padding_p)
    for (i = 0; i < len; i++)
      XftDrawGlyphs (xft_draw, &fg, xftfont_info->xftfont,
		     x + i, y, code + i, 1);
  else
    XftDrawGlyphs (xft_draw, &fg, xftfont_info->xftfont,
                   x, y, code, len);
  /* Need to explicitly mark the frame dirty because we didn't call
     FRAME_X_DRAWABLE in order to draw: we cached the drawable in the
     XftDraw structure.  */
  x_mark_frame_dirty (f);
  unblock_input ();
  return len;
}

#if defined HAVE_M17N_FLT && defined HAVE_LIBOTF
static Lisp_Object
xftfont_shape (Lisp_Object lgstring, Lisp_Object direction)
{
  struct font *font = CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring));
  struct font_info *xftfont_info = (struct font_info *) font;
  FT_Face ft_face = XftLockFace (xftfont_info->xftfont);
  xftfont_info->ft_size = ft_face->size;
  Lisp_Object val = ftfont_shape (lgstring, direction);
  XftUnlockFace (xftfont_info->xftfont);
  return val;
}
#endif

static int
xftfont_end_for_frame (struct frame *f)
{
  block_input ();
  XftDraw *xft_draw;

  xft_draw = font_get_frame_data (f, Qxft);
  if (xft_draw)
    {
      block_input ();
      XftDrawDestroy (xft_draw);
      unblock_input ();
      font_put_frame_data (f, Qxft, NULL);
    }
  unblock_input ();
  return 0;
}

/* When using X double buffering, the XftDraw structure we build
   seems to be useless once a frame is resized, so recreate it on
   ConfigureNotify and in some other cases.  */

static void
xftfont_drop_xrender_surfaces (struct frame *f)
{
  block_input ();
  if (FRAME_X_DOUBLE_BUFFERED_P (f))
    xftfont_end_for_frame (f);
  unblock_input ();
}

static bool
xftfont_cached_font_ok (struct frame *f, Lisp_Object font_object,
                        Lisp_Object entity)
{
  struct font_info *info = (struct font_info *) XFONT_OBJECT (font_object);
  FcPattern *oldpat = info->xftfont->pattern;
  Display *display = FRAME_X_DISPLAY (f);
  FcPattern *pat = FcPatternCreate ();
  FcBool b1, b2;
  bool ok = false;
  int i1, i2, r1, r2;

  ftfont_add_rendering_parameters (pat, entity);
  XftDefaultSubstitute (display, FRAME_X_SCREEN_NUMBER (f), pat);

  r1 = FcPatternGetBool (pat, FC_ANTIALIAS, 0, &b1);
  r2 = FcPatternGetBool (oldpat, FC_ANTIALIAS, 0, &b2);
  if (r1 != r2 || b1 != b2) goto out;
  r1 = FcPatternGetBool (pat, FC_HINTING, 0, &b1);
  r2 = FcPatternGetBool (oldpat, FC_HINTING, 0, &b2);
  if (r1 != r2 || b1 != b2) goto out;
  r1 = FcPatternGetBool (pat, FC_AUTOHINT, 0, &b1);
  r2 = FcPatternGetBool (oldpat, FC_AUTOHINT, 0, &b2);
  if (r1 != r2 || b1 != b2) goto out;
#ifdef FC_EMBOLDEN
  r1 = FcPatternGetBool (pat, FC_EMBOLDEN, 0, &b1);
  r2 = FcPatternGetBool (oldpat, FC_EMBOLDEN, 0, &b2);
  if (r1 != r2 || b1 != b2) goto out;
#endif
  r1 = FcPatternGetInteger (pat, FC_HINT_STYLE, 0, &i1);
  r2 = FcPatternGetInteger (oldpat, FC_HINT_STYLE, 0, &i2);
  if (r1 != r2 || i1 != i2) goto out;
  r1 = FcPatternGetInteger (pat, FC_LCD_FILTER, 0, &i1);
  r2 = FcPatternGetInteger (oldpat, FC_LCD_FILTER, 0, &i2);
  if (r1 != r2 || i1 != i2) goto out;
  r1 = FcPatternGetInteger (pat, FC_RGBA, 0, &i1);
  r2 = FcPatternGetInteger (oldpat, FC_RGBA, 0, &i2);
  if (r1 != r2 || i1 != i2) goto out;

  ok = true;
 out:
  FcPatternDestroy (pat);
  return ok;
}

#ifdef HAVE_HARFBUZZ

static Lisp_Object
xfthbfont_list (struct frame *f, Lisp_Object spec)
{
  return ftfont_list2 (f, spec, Qxfthb);
}

static Lisp_Object
xfthbfont_match (struct frame *f, Lisp_Object spec)
{
  return ftfont_match2 (f, spec, Qxfthb);
}

static hb_font_t *
xfthbfont_begin_hb_font (struct font *font, double *position_unit)
{
  struct font_info *xftfont_info = (struct font_info *) font;
  FT_Face ft_face = XftLockFace (xftfont_info->xftfont);

  xftfont_info->ft_size = ft_face->size;

  return fthbfont_begin_hb_font (font, position_unit);
}

static void
xfthbfont_end_hb_font (struct font *font, hb_font_t *hb_font)
{
  struct font_info *xftfont_info = (struct font_info *) font;

  XftUnlockFace (xftfont_info->xftfont);
}

#endif	/* HAVE_HARFBUZZ */

static void syms_of_xftfont_for_pdumper (void);

struct font_driver const xftfont_driver =
  {
    /* We can't draw a text without device dependent functions.  */
  .type = LISPSYM_INITIALLY (Qxft),
  .get_cache = xfont_get_cache,
  .list = xftfont_list,
  .match = xftfont_match,
  .list_family = ftfont_list_family,
  .open_font = xftfont_open,
  .close_font = xftfont_close,
  .prepare_face = xftfont_prepare_face,
  .done_face = xftfont_done_face,
  .has_char = xftfont_has_char,
  .encode_char = xftfont_encode_char,
  .text_extents = xftfont_text_extents,
  .draw = xftfont_draw,
  .get_bitmap = ftfont_get_bitmap,
  .anchor_point = ftfont_anchor_point,
#ifdef HAVE_LIBOTF
  .otf_capability = ftfont_otf_capability,
#endif
  .end_for_frame = xftfont_end_for_frame,
#if defined HAVE_M17N_FLT && defined HAVE_LIBOTF
  .shape = xftfont_shape,
#endif
#if defined HAVE_OTF_GET_VARIATION_GLYPHS || defined HAVE_FT_FACE_GETCHARVARIANTINDEX
  .get_variation_glyphs = ftfont_variation_glyphs,
#endif
  .filter_properties = ftfont_filter_properties,
  .cached_font_ok = xftfont_cached_font_ok,
  .combining_capability = ftfont_combining_capability,
  .drop_xrender_surfaces = xftfont_drop_xrender_surfaces,
  };
#ifdef HAVE_HARFBUZZ
struct font_driver xfthbfont_driver;
#endif	/* HAVE_HARFBUZZ */

void
syms_of_xftfont (void)
{
  DEFSYM (Qxft, "xft");
#ifdef HAVE_HARFBUZZ
  DEFSYM (Qxfthb, "xfthb");
  Fput (Qxft, Qfont_driver_superseded_by, Qxfthb);
#endif	/* HAVE_HARFBUZZ */

  DEFVAR_BOOL ("xft-font-ascent-descent-override",
	       xft_font_ascent_descent_override,
	       doc:  /* Non-nil means override the ascent and descent values for Xft font driver.
This is needed with some fonts to correct vertical overlap of glyphs.  */);
  xft_font_ascent_descent_override = 0;

  pdumper_do_now_and_after_load (syms_of_xftfont_for_pdumper);
}

static void
syms_of_xftfont_for_pdumper (void)
{
  register_font_driver (&xftfont_driver, NULL);
#ifdef HAVE_HARFBUZZ
  xfthbfont_driver = xftfont_driver;
  xfthbfont_driver.type = Qxfthb;
  xfthbfont_driver.list = xfthbfont_list;
  xfthbfont_driver.match = xfthbfont_match;
  xfthbfont_driver.otf_capability = hbfont_otf_capability;
  xfthbfont_driver.shape = hbfont_shape;
  xfthbfont_driver.combining_capability = hbfont_combining_capability;
  xfthbfont_driver.begin_hb_font = xfthbfont_begin_hb_font;
  xfthbfont_driver.end_hb_font = xfthbfont_end_hb_font;
  register_font_driver (&xfthbfont_driver, NULL);
#endif	/* HAVE_HARFBUZZ */
}
