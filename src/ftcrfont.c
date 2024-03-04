/* ftcrfont.c -- FreeType font driver on cairo.
   Copyright (C) 2015-2024 Free Software Foundation, Inc.

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
#include <math.h>
#include <cairo-ft.h>

#include "lisp.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#elif HAVE_HAIKU
#include "haikuterm.h"
#include "haiku_support.h"
#include "termchar.h"
#else
#include "pgtkterm.h"
#endif
#include "blockinput.h"
#include "charset.h"
#include "composite.h"
#include "font.h"
#include "ftfont.h"
#include "pdumper.h"
#ifdef HAVE_PGTK
#include "xsettings.h"
#endif

#ifdef USE_BE_CAIRO
#define RED_FROM_ULONG(color)	(((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color)	(((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color)	((color) & 0xff)
#endif

#define METRICS_NCOLS_PER_ROW	(128)

enum metrics_status
  {
    METRICS_INVALID = -1,    /* metrics entry is invalid */
  };

#define METRICS_STATUS(metrics)	((metrics)->ascent + (metrics)->descent)
#define METRICS_SET_STATUS(metrics, status) \
  ((metrics)->ascent = 0, (metrics)->descent = (status))

static int
ftcrfont_glyph_extents (struct font *font,
                        unsigned glyph,
                        struct font_metrics *metrics)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;
  int row, col;
  struct font_metrics *cache;

  row = glyph / METRICS_NCOLS_PER_ROW;
  col = glyph % METRICS_NCOLS_PER_ROW;
  if (row >= ftcrfont_info->metrics_nrows)
    {
      ftcrfont_info->metrics =
	xrealloc (ftcrfont_info->metrics,
		  sizeof (struct font_metrics *) * (row + 1));
      memset (ftcrfont_info->metrics + ftcrfont_info->metrics_nrows, 0,
	      (sizeof (struct font_metrics *)
	       * (row + 1 - ftcrfont_info->metrics_nrows)));
      ftcrfont_info->metrics_nrows = row + 1;
    }
  if (ftcrfont_info->metrics[row] == NULL)
    {
      struct font_metrics *new;
      int i;

      new = xmalloc (sizeof (struct font_metrics) * METRICS_NCOLS_PER_ROW);
      for (i = 0; i < METRICS_NCOLS_PER_ROW; i++)
	METRICS_SET_STATUS (new + i, METRICS_INVALID);
      ftcrfont_info->metrics[row] = new;
    }
  cache = ftcrfont_info->metrics[row] + col;

  if (METRICS_STATUS (cache) == METRICS_INVALID)
    {
      cairo_glyph_t cr_glyph = {.index = glyph};
      cairo_text_extents_t extents;

      cairo_scaled_font_glyph_extents (ftcrfont_info->cr_scaled_font,
				       &cr_glyph, 1, &extents);
      cache->lbearing = floor (extents.x_bearing);
      cache->rbearing = ceil (extents.width + extents.x_bearing);
      cache->width = lround (extents.x_advance);
      /* The subtraction of a small number is to avoid rounding up due
	 to floating-point inaccuracies with some fonts, which then
	 could cause unpleasant effects while scrolling (see bug
	 #44284), since we then think that a glyph row's ascent is too
	 small to accommodate a glyph with a higher phys_ascent.  */
      cache->ascent = ceil (- extents.y_bearing - 1.0 / 256);
      cache->descent = ceil (extents.height + extents.y_bearing);
    }

  if (metrics)
    *metrics = *cache;

  return cache->width;
}

static Lisp_Object
ftcrfont_list (struct frame *f, Lisp_Object spec)
{
  return ftfont_list2 (f, spec, Qftcr);
}

static Lisp_Object
ftcrfont_match (struct frame *f, Lisp_Object spec)
{
  return ftfont_match2 (f, spec, Qftcr);
}

static Lisp_Object
ftcrfont_open (struct frame *f, Lisp_Object entity, int pixel_size)
{
  FcResult result;
  Lisp_Object val, filename, font_object;
  FcPattern *pat, *match;
  struct font_info *ftcrfont_info;
  struct font *font;
  double size = 0;
  cairo_font_face_t *font_face;
  cairo_font_extents_t extents;
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
  FcConfigSubstitute (NULL, pat, FcMatchPattern);
  FcDefaultSubstitute (pat);
  match = FcFontMatch (NULL, pat, &result);
  ftfont_fix_match (pat, match);

  FcPatternDestroy (pat);
  font_face = cairo_ft_font_face_create_for_pattern (match);
  if (!font_face
      || cairo_font_face_status (font_face) != CAIRO_STATUS_SUCCESS)
    {
      unblock_input ();
      FcPatternDestroy (match);
      return Qnil;
    }
  cairo_matrix_t font_matrix, ctm;
  cairo_matrix_init_scale (&font_matrix, pixel_size, pixel_size);
  cairo_matrix_init_identity (&ctm);

#ifdef HAVE_PGTK
  cairo_font_options_t *options = xsettings_get_font_options ();
#else
  cairo_font_options_t *options = cairo_font_options_create ();
#endif
#ifdef USE_BE_CAIRO
  if (be_use_subpixel_antialiasing ())
    cairo_font_options_set_antialias (options, CAIRO_ANTIALIAS_SUBPIXEL);
#endif
  cairo_scaled_font_t *scaled_font
    = cairo_scaled_font_create (font_face, &font_matrix, &ctm, options);
  cairo_font_face_destroy (font_face);
  cairo_font_options_destroy (options);
  unblock_input ();
  if (!scaled_font
      || cairo_scaled_font_status (scaled_font) != CAIRO_STATUS_SUCCESS)
    {
      FcPatternDestroy (match);
      return Qnil;
    }
  ft_face = cairo_ft_scaled_font_lock_face (scaled_font);
  if (!ft_face)
    {
      FcPatternDestroy (match);
      return Qnil;
    }

  font_object = font_build_object (VECSIZE (struct font_info),
				   AREF (entity, FONT_TYPE_INDEX),
				   entity, size);
  ASET (font_object, FONT_FILE_INDEX, filename);
  font = XFONT_OBJECT (font_object);
  font->pixel_size = size;
#ifdef HAVE_HARFBUZZ
  if (EQ (AREF (font_object, FONT_TYPE_INDEX), Qftcrhb))
    font->driver = &ftcrhbfont_driver;
  else
#endif	/* HAVE_HARFBUZZ */
  font->driver = &ftcrfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  ftcrfont_info = (struct font_info *) font;
  ftcrfont_info->cr_scaled_font = scaled_font;

  /* This means that there's no need of transformation.  */
  ftcrfont_info->matrix.xx = 0;
  if (FcPatternGetMatrix (match, FC_MATRIX, 0, &matrix) == FcResultMatch)
    {
      ftcrfont_info->matrix.xx = 0x10000L * matrix->xx;
      ftcrfont_info->matrix.yy = 0x10000L * matrix->yy;
      ftcrfont_info->matrix.xy = 0x10000L * matrix->xy;
      ftcrfont_info->matrix.yx = 0x10000L * matrix->yx;
    }

  ftcrfont_info->metrics = NULL;
  ftcrfont_info->metrics_nrows = 0;

  block_input ();
  cairo_glyph_t stack_glyph;
  font->min_width = font->max_width = 0;
  font->average_width = font->space_width = 0;
  int n = 0;
  for (char c = 32; c < 127; c++)
    {
      cairo_glyph_t *glyphs = &stack_glyph;
      int num_glyphs = 1;
      cairo_status_t status =
	cairo_scaled_font_text_to_glyphs (ftcrfont_info->cr_scaled_font,
					  0, 0, &c, 1, &glyphs, &num_glyphs,
					  NULL, NULL, NULL);

      /* In order to simulate the Xft behavior, we use metrics of
	 glyph ID 0 if there is no glyph for an ASCII printable.  */
      if (status != CAIRO_STATUS_SUCCESS)
	stack_glyph.index = 0;
      else if (glyphs != &stack_glyph)
	{
	  cairo_glyph_free (glyphs);
	  stack_glyph.index = 0;
	}
      int this_width = ftcrfont_glyph_extents (font, stack_glyph.index, NULL);
      if (this_width > 0)
	{
	  if (! font->min_width || font->min_width > this_width)
	    font->min_width = this_width;
	  if (this_width > font->max_width)
	    font->max_width = this_width;
	  if (c == 32)
	    font->space_width = this_width;
	  font->average_width += this_width;
	  n++;
	}
    }
  if (n)
    font->average_width /= n;

  cairo_scaled_font_extents (ftcrfont_info->cr_scaled_font, &extents);
  font->ascent = lround (extents.ascent);
  val = assq_no_quit (QCminspace, AREF (entity, FONT_EXTRA_INDEX));
  if (!(CONSP (val) && NILP (XCDR (val))))
    {
      font->descent = lround (extents.descent);
      font->height = font->ascent + font->descent;
    }
  else
    {
      font->height = lround (extents.height);
      font->descent = font->height - font->ascent;
    }

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
  ftcrfont_info->maybe_otf = (ft_face->face_flags & FT_FACE_FLAG_SFNT) != 0;
  ftcrfont_info->otf = NULL;
#endif	/* HAVE_LIBOTF */
#ifdef HAVE_HARFBUZZ
  ftcrfont_info->hb_font = NULL;
#endif	/* HAVE_HARFBUZZ */
  if (ft_face->units_per_EM)
    ftcrfont_info->bitmap_position_unit = 0;
  else
    ftcrfont_info->bitmap_position_unit = (extents.height
					   / ft_face->size->metrics.height);
  cairo_ft_scaled_font_unlock_face (scaled_font);
  ftcrfont_info->ft_size = NULL;
  unblock_input ();

  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = 0;
  font->vertical_centering = false;
  eassert (font->max_width < 512 * 1024 * 1024);

  return font_object;
}

static void
ftcrfont_close (struct font *font)
{
  if (font_data_structures_may_be_ill_formed ())
    return;

  struct font_info *ftcrfont_info = (struct font_info *) font;

  block_input ();
#ifdef HAVE_LIBOTF
  if (ftcrfont_info->otf)
    {
      OTF_close (ftcrfont_info->otf);
      ftcrfont_info->otf = NULL;
    }
#endif
#ifdef HAVE_HARFBUZZ
  if (ftcrfont_info->hb_font)
    {
      hb_font_destroy (ftcrfont_info->hb_font);
      ftcrfont_info->hb_font = NULL;
    }
#endif
  for (int i = 0; i < ftcrfont_info->metrics_nrows; i++)
    if (ftcrfont_info->metrics[i])
      xfree (ftcrfont_info->metrics[i]);
  if (ftcrfont_info->metrics)
    xfree (ftcrfont_info->metrics);
  cairo_scaled_font_destroy (ftcrfont_info->cr_scaled_font);
  unblock_input ();
}

static int
ftcrfont_has_char (Lisp_Object font, int c)
{
  if (FONT_ENTITY_P (font))
    return ftfont_has_char (font, c);

  struct charset *cs = NULL;

  if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qja)
      && charset_jisx0208 >= 0)
    cs = CHARSET_FROM_ID (charset_jisx0208);
  else if (EQ (AREF (font, FONT_ADSTYLE_INDEX), Qko)
      && charset_ksc5601 >= 0)
    cs = CHARSET_FROM_ID (charset_ksc5601);
  if (cs)
    return (ENCODE_CHAR (cs, c) != CHARSET_INVALID_CODE (cs));

  return -1;
}

static unsigned
ftcrfont_encode_char (struct font *font, int c)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;
  unsigned code = FONT_INVALID_CODE;
  unsigned char utf8[MAX_MULTIBYTE_LENGTH];
  int utf8len = CHAR_STRING (c, utf8);
  cairo_glyph_t stack_glyph;
  cairo_glyph_t *glyphs = &stack_glyph;
  int num_glyphs = 1;

  if (cairo_scaled_font_text_to_glyphs (ftcrfont_info->cr_scaled_font, 0, 0,
					(char *) utf8, utf8len,
					&glyphs, &num_glyphs,
					NULL, NULL, NULL)
      == CAIRO_STATUS_SUCCESS)
    {
      if (glyphs != &stack_glyph)
	cairo_glyph_free (glyphs);
      else if (stack_glyph.index)
	code = stack_glyph.index;
    }

  return code;
}

static void
ftcrfont_text_extents (struct font *font,
                       const unsigned *code,
                       int nglyphs,
                       struct font_metrics *metrics)
{
  int width, i;

  block_input ();
  width = ftcrfont_glyph_extents (font, code[0], metrics);
  for (i = 1; i < nglyphs; i++)
    {
      struct font_metrics m;
      int w = ftcrfont_glyph_extents (font, code[i], metrics ? &m : NULL);

      if (metrics)
	{
	  if (width + m.lbearing < metrics->lbearing)
	    metrics->lbearing = width + m.lbearing;
	  if (width + m.rbearing > metrics->rbearing)
	    metrics->rbearing = width + m.rbearing;
	  if (m.ascent > metrics->ascent)
	    metrics->ascent = m.ascent;
	  if (m.descent > metrics->descent)
	    metrics->descent = m.descent;
	}
      width += w;
    }
  unblock_input ();

  if (metrics)
    metrics->width = width;
}

static int
ftcrfont_get_bitmap (struct font *font, unsigned int code,
		     struct font_bitmap *bitmap, int bits_per_pixel)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;

  if (ftcrfont_info->bitmap_position_unit)
    return -1;

  cairo_scaled_font_t *scaled_font = ftcrfont_info->cr_scaled_font;
  FT_Face ft_face = cairo_ft_scaled_font_lock_face (scaled_font);

  ftcrfont_info->ft_size = ft_face->size;
  int result = ftfont_get_bitmap (font, code, bitmap, bits_per_pixel);
  cairo_ft_scaled_font_unlock_face (scaled_font);
  ftcrfont_info->ft_size = NULL;

  return result;
}

static int
ftcrfont_anchor_point (struct font *font, unsigned int code, int idx,
		       int *x, int *y)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;

  if (ftcrfont_info->bitmap_position_unit)
    return -1;

  cairo_scaled_font_t *scaled_font = ftcrfont_info->cr_scaled_font;
  FT_Face ft_face = cairo_ft_scaled_font_lock_face (scaled_font);

  ftcrfont_info->ft_size = ft_face->size;
  int result = ftfont_anchor_point (font, code, idx, x, y);
  cairo_ft_scaled_font_unlock_face (scaled_font);
  ftcrfont_info->ft_size = NULL;

  return result;
}

#ifdef HAVE_LIBOTF
static Lisp_Object
ftcrfont_otf_capability (struct font *font)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;
  cairo_scaled_font_t *scaled_font = ftcrfont_info->cr_scaled_font;
  FT_Face ft_face = cairo_ft_scaled_font_lock_face (scaled_font);

  ftcrfont_info->ft_size = ft_face->size;
  Lisp_Object result = ftfont_otf_capability (font);
  cairo_ft_scaled_font_unlock_face (scaled_font);
  ftcrfont_info->ft_size = NULL;

  return result;
}
#endif

#if defined HAVE_M17N_FLT && defined HAVE_LIBOTF
static Lisp_Object
ftcrfont_shape (Lisp_Object lgstring, Lisp_Object direction)
{
  struct font *font = CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring));
  struct font_info *ftcrfont_info = (struct font_info *) font;

  if (ftcrfont_info->bitmap_position_unit)
    return make_fixnum (0);

  cairo_scaled_font_t *scaled_font = ftcrfont_info->cr_scaled_font;
  FT_Face ft_face = cairo_ft_scaled_font_lock_face (scaled_font);

  ftcrfont_info->ft_size = ft_face->size;
  Lisp_Object result = ftfont_shape (lgstring, direction);
  cairo_ft_scaled_font_unlock_face (scaled_font);
  ftcrfont_info->ft_size = NULL;

  return result;
}
#endif

#if defined HAVE_OTF_GET_VARIATION_GLYPHS || defined HAVE_FT_FACE_GETCHARVARIANTINDEX
static int
ftcrfont_variation_glyphs (struct font *font, int c, unsigned variations[256])
{
  struct font_info *ftcrfont_info = (struct font_info *) font;
  cairo_scaled_font_t *scaled_font = ftcrfont_info->cr_scaled_font;
  FT_Face ft_face = cairo_ft_scaled_font_lock_face (scaled_font);

  ftcrfont_info->ft_size = ft_face->size;
  int result = ftfont_variation_glyphs (font, c, variations);
  cairo_ft_scaled_font_unlock_face (scaled_font);
  ftcrfont_info->ft_size = NULL;

  return result;
}
#endif	/* HAVE_OTF_GET_VARIATION_GLYPHS || HAVE_FT_FACE_GETCHARVARIANTINDEX */

static int
ftcrfont_draw (struct glyph_string *s,
               int from, int to, int x, int y, bool with_background)
{
  struct frame *f = s->f;
  struct font_info *ftcrfont_info = (struct font_info *) s->font;
  cairo_t *cr;
  cairo_glyph_t *glyphs;
  int len = to - from;
  int i;
#ifdef USE_BE_CAIRO
  unsigned long be_foreground, be_background;

  if (s->hl != DRAW_CURSOR)
    {
      be_foreground = s->face->foreground;
      be_background = s->face->background;
    }
  else
    haiku_merge_cursor_foreground (s, &be_foreground,
				   &be_background);
#endif

  block_input ();

#ifndef USE_BE_CAIRO
#ifdef HAVE_X_WINDOWS
  cr = x_begin_cr_clip (f, s->gc);
#else
  cr = pgtk_begin_cr_clip (f);
#endif
#else
  /* Presumably the draw lock is already held by
     haiku_draw_glyph_string.  */
  EmacsWindow_begin_cr_critical_section (FRAME_HAIKU_WINDOW (f));
  cr = haiku_begin_cr_clip (f, s);
  if (!cr)
    {
      EmacsWindow_end_cr_critical_section (FRAME_HAIKU_WINDOW (f));
      unblock_input ();
      return 0;
    }
  BView_cr_dump_clipping (FRAME_HAIKU_DRAWABLE (f), cr);
#endif

  if (with_background)
    {
#ifndef USE_BE_CAIRO
#ifdef HAVE_X_WINDOWS
      x_set_cr_source_with_gc_background (f, s->gc, s->hl != DRAW_CURSOR);
#else
      pgtk_set_cr_source_with_color (f, s->xgcv.background,
				     s->hl != DRAW_CURSOR);
#endif
#else
      uint32_t col = be_background;

      cairo_set_source_rgb (cr, RED_FROM_ULONG (col) / 255.0,
			    GREEN_FROM_ULONG (col) / 255.0,
			    BLUE_FROM_ULONG (col) / 255.0);
#endif
      cairo_rectangle (cr, x, y - FONT_BASE (s->font),
		       s->width, FONT_HEIGHT (s->font));
      cairo_fill (cr);
    }

  glyphs = alloca (sizeof (cairo_glyph_t) * len);
  for (i = 0; i < len; i++)
    {
      glyphs[i].index = s->char2b[from + i];
      glyphs[i].x = x;
      glyphs[i].y = y;
      x += (s->padding_p ? 1 : ftcrfont_glyph_extents (s->font,
                                                       glyphs[i].index,
                                                       NULL));
    }
#ifndef USE_BE_CAIRO
#ifdef HAVE_X_WINDOWS
  x_set_cr_source_with_gc_foreground (f, s->gc, false);
#else
  pgtk_set_cr_source_with_color (f, s->xgcv.foreground, false);
#endif
#else
  uint32_t col = be_foreground;

  cairo_set_source_rgb (cr, RED_FROM_ULONG (col) / 255.0,
			GREEN_FROM_ULONG (col) / 255.0,
			BLUE_FROM_ULONG (col) / 255.0);
#endif
  cairo_set_scaled_font (cr, ftcrfont_info->cr_scaled_font);
  cairo_show_glyphs (cr, glyphs, len);
#ifndef USE_BE_CAIRO
#ifdef HAVE_X_WINDOWS
  x_end_cr_clip (f);
#else
  pgtk_end_cr_clip (f);
#endif
#else
  haiku_end_cr_clip (cr);
  EmacsWindow_end_cr_critical_section (FRAME_HAIKU_WINDOW (f));
#endif
  unblock_input ();

  return len;
}

#ifdef HAVE_PGTK
/* Determine if FONT_OBJECT is a valid cached font for ENTITY by
   comparing the options used to open it with the user's current
   preferences specified via GSettings.  */
static bool
ftcrfont_cached_font_ok (struct frame *f, Lisp_Object font_object,
			 Lisp_Object entity)
{
  struct font_info *info = (struct font_info *) XFONT_OBJECT (font_object);

  cairo_font_options_t *options = cairo_font_options_create ();
  cairo_scaled_font_get_font_options (info->cr_scaled_font, options);
  cairo_font_options_t *gsettings_options = xsettings_get_font_options ();

  bool equal = cairo_font_options_equal (options, gsettings_options);
  cairo_font_options_destroy (options);
  cairo_font_options_destroy (gsettings_options);

  return equal;
}
#endif

#ifdef HAVE_HARFBUZZ

static Lisp_Object
ftcrhbfont_list (struct frame *f, Lisp_Object spec)
{
  return ftfont_list2 (f, spec, Qftcrhb);
}

static Lisp_Object
ftcrhbfont_match (struct frame *f, Lisp_Object spec)
{
  return ftfont_match2 (f, spec, Qftcrhb);
}

static hb_font_t *
ftcrhbfont_begin_hb_font (struct font *font, double *position_unit)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;
  cairo_scaled_font_t *scaled_font = ftcrfont_info->cr_scaled_font;
  FT_Face ft_face = cairo_ft_scaled_font_lock_face (scaled_font);

  ftcrfont_info->ft_size = ft_face->size;
  hb_font_t *hb_font = fthbfont_begin_hb_font (font, position_unit);
  /* HarfBuzz 5 correctly scales bitmap-only fonts without position
     unit adjustment.
     (https://github.com/harfbuzz/harfbuzz/issues/489)

     Update: HarfBuzz 5.2.0 no longer does this for an hb_font_t font
     object created from a given FT_Face.
     (https://github.com/harfbuzz/harfbuzz/issues/3788) */
  if ((hb_version_atleast (5, 2, 0) || !hb_version_atleast (5, 0, 0))
      && ftcrfont_info->bitmap_position_unit)
    *position_unit = ftcrfont_info->bitmap_position_unit;

  return hb_font;
}

static void
ftcrhbfont_end_hb_font (struct font *font, hb_font_t *hb_font)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;
  cairo_scaled_font_t *scaled_font = ftcrfont_info->cr_scaled_font;

  cairo_ft_scaled_font_unlock_face (scaled_font);
  ftcrfont_info->ft_size = NULL;
}

#endif	/* HAVE_HARFBUZZ */


static void syms_of_ftcrfont_for_pdumper (void);

struct font_driver const ftcrfont_driver =
  {
  .type = LISPSYM_INITIALLY (Qftcr),
  .get_cache = ftfont_get_cache,
  .list = ftcrfont_list,
  .match = ftcrfont_match,
  .list_family = ftfont_list_family,
  .open_font = ftcrfont_open,
  .close_font = ftcrfont_close,
  .has_char = ftcrfont_has_char,
  .encode_char = ftcrfont_encode_char,
  .text_extents = ftcrfont_text_extents,
  .draw = ftcrfont_draw,
  .get_bitmap = ftcrfont_get_bitmap,
  .anchor_point = ftcrfont_anchor_point,
#ifdef HAVE_LIBOTF
  .otf_capability = ftcrfont_otf_capability,
#endif
#if defined HAVE_M17N_FLT && defined HAVE_LIBOTF
  .shape = ftcrfont_shape,
#endif
#if defined HAVE_OTF_GET_VARIATION_GLYPHS || defined HAVE_FT_FACE_GETCHARVARIANTINDEX
  .get_variation_glyphs = ftcrfont_variation_glyphs,
#endif
  .filter_properties = ftfont_filter_properties,
  .combining_capability = ftfont_combining_capability,
#ifdef HAVE_PGTK
  .cached_font_ok = ftcrfont_cached_font_ok,
#endif
  };
#ifdef HAVE_HARFBUZZ
struct font_driver ftcrhbfont_driver;
#endif	/* HAVE_HARFBUZZ */

void
syms_of_ftcrfont (void)
{
  DEFSYM (Qftcr, "ftcr");
#ifdef HAVE_HARFBUZZ
  DEFSYM (Qftcrhb, "ftcrhb");
  Fput (Qftcr, Qfont_driver_superseded_by, Qftcrhb);
#endif	/* HAVE_HARFBUZZ */
  pdumper_do_now_and_after_load (syms_of_ftcrfont_for_pdumper);
}

#ifdef HAVE_X_WINDOWS

/* Place the default font options used by Cairo on the given display
   in OPTIONS.  */

void
ftcrfont_get_default_font_options (struct x_display_info *dpyinfo,
				   cairo_font_options_t *options)
{
  Pixmap drawable;
  cairo_surface_t *surface;

  /* Cairo doesn't allow fetching the default font options for a
     display, so the only option is to create a drawable, and an Xlib
     surface for that drawable, and to get the font options from there
     instead.  */

  drawable = XCreatePixmap (dpyinfo->display, dpyinfo->root_window,
			    1, 1, dpyinfo->n_planes);
  surface = cairo_xlib_surface_create (dpyinfo->display, drawable,
				       dpyinfo->visual, 1, 1);

  if (!surface)
    {
      XFreePixmap (dpyinfo->display, drawable);
      return;
    }

  cairo_surface_get_font_options (surface, options);
  XFreePixmap (dpyinfo->display, drawable);
  cairo_surface_destroy (surface);
  return;
}

#endif

static void
syms_of_ftcrfont_for_pdumper (void)
{
  register_font_driver (&ftcrfont_driver, NULL);
#ifdef HAVE_HARFBUZZ
  ftcrhbfont_driver = ftcrfont_driver;
  ftcrhbfont_driver.type = Qftcrhb;
  ftcrhbfont_driver.list = ftcrhbfont_list;
  ftcrhbfont_driver.match = ftcrhbfont_match;
  ftcrhbfont_driver.otf_capability = hbfont_otf_capability;
  ftcrhbfont_driver.shape = hbfont_shape;
  ftcrhbfont_driver.combining_capability = hbfont_combining_capability;
  ftcrhbfont_driver.begin_hb_font = ftcrhbfont_begin_hb_font;
  ftcrhbfont_driver.end_hb_font = ftcrhbfont_end_hb_font;
  register_font_driver (&ftcrhbfont_driver, NULL);
#endif	/* HAVE_HARFBUZZ */
}
