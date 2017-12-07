/* ftcrfont.c -- FreeType font driver on cairo.
   Copyright (C) 2015-2017 Free Software Foundation, Inc.

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
#include <cairo-ft.h>

#include "lisp.h"
#include "xterm.h"
#include "blockinput.h"
#include "font.h"
#include "ftfont.h"

/* FTCR font driver.  */

/* The actual structure for FTCR font.  A pointer to this structure
   can be cast to struct font *.  */

struct ftcrfont_info
{
  struct font font;
  /* The following six members must be here in this order to be
     compatible with struct ftfont_info (in ftfont.c).  */
#ifdef HAVE_LIBOTF
  bool maybe_otf;	  /* Flag to tell if this may be OTF or not.  */
  OTF *otf;
#endif	/* HAVE_LIBOTF */
  FT_Size ft_size;
  int index;
  FT_Matrix matrix;

  cairo_font_face_t *cr_font_face;
  /* To prevent cairo from cluttering the activated FT_Size maintained
     in ftfont.c, we activate this special FT_Size before drawing.  */
  FT_Size ft_size_draw;
  /* Font metrics cache.  */
  struct font_metrics **metrics;
  short metrics_nrows;
};

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
  struct ftcrfont_info *ftcrfont_info = (struct ftcrfont_info *) font;
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
    ftfont_text_extents (font, &glyph, 1, cache);

  if (metrics)
    *metrics = *cache;

  return cache->width;
}

static Lisp_Object
ftcrfont_list (struct frame *f, Lisp_Object spec)
{
  Lisp_Object list = ftfont_list (f, spec), tail;

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    ASET (XCAR (tail), FONT_TYPE_INDEX, Qftcr);
  return list;
}

static Lisp_Object
ftcrfont_match (struct frame *f, Lisp_Object spec)
{
  Lisp_Object entity = ftfont_match (f, spec);

  if (VECTORP (entity))
    ASET (entity, FONT_TYPE_INDEX, Qftcr);
  return entity;
}

static Lisp_Object
ftcrfont_open (struct frame *f, Lisp_Object entity, int pixel_size)
{
  Lisp_Object font_object;
  struct font *font;
  struct ftcrfont_info *ftcrfont_info;
  FT_Face ft_face;
  FT_UInt size;

  block_input ();
  size = XINT (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;
  font_object = font_build_object (VECSIZE (struct ftcrfont_info),
				   Qftcr, entity, size);
  font_object = ftfont_open2 (f, entity, pixel_size, font_object);
  if (NILP (font_object)) return Qnil;

  font = XFONT_OBJECT (font_object);
  font->driver = &ftcrfont_driver;
  ftcrfont_info = (struct ftcrfont_info *) font;
  ft_face = ftcrfont_info->ft_size->face;
  FT_New_Size (ft_face, &ftcrfont_info->ft_size_draw);
  FT_Activate_Size (ftcrfont_info->ft_size_draw);
  FT_Set_Pixel_Sizes (ft_face, 0, font->pixel_size);
  ftcrfont_info->cr_font_face =
    cairo_ft_font_face_create_for_ft_face (ft_face, 0);
  ftcrfont_info->metrics = NULL;
  ftcrfont_info->metrics_nrows = 0;
  unblock_input ();

  return font_object;
}

static void
ftcrfont_close (struct font *font)
{
  struct ftcrfont_info *ftcrfont_info = (struct ftcrfont_info *) font;
  int i;

  block_input ();
  for (i = 0; i < ftcrfont_info->metrics_nrows; i++)
    if (ftcrfont_info->metrics[i])
      xfree (ftcrfont_info->metrics[i]);
  if (ftcrfont_info->metrics)
    xfree (ftcrfont_info->metrics);
  FT_Done_Size (ftcrfont_info->ft_size_draw);
  cairo_font_face_destroy (ftcrfont_info->cr_font_face);
  unblock_input ();

  ftfont_close (font);
}

static void
ftcrfont_text_extents (struct font *font,
                       unsigned *code,
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
ftcrfont_draw (struct glyph_string *s,
               int from, int to, int x, int y, bool with_background)
{
  struct frame *f = s->f;
  struct face *face = s->face;
  struct ftcrfont_info *ftcrfont_info = (struct ftcrfont_info *) s->font;
  cairo_t *cr;
  cairo_glyph_t *glyphs;
  cairo_surface_t *surface;
  cairo_surface_type_t surface_type;
  int len = to - from;
  int i;

  block_input ();

  cr = x_begin_cr_clip (f, s->gc);

  if (with_background)
    {
      x_set_cr_source_with_gc_background (f, s->gc);
      cairo_rectangle (cr, x, y - FONT_BASE (face->font),
		       s->width, FONT_HEIGHT (face->font));
      cairo_fill (cr);
    }

  glyphs = alloca (sizeof (cairo_glyph_t) * len);
  for (i = 0; i < len; i++)
    {
      unsigned code = ((XCHAR2B_BYTE1 (s->char2b + from + i) << 8)
		       | XCHAR2B_BYTE2 (s->char2b + from + i));

      glyphs[i].index = code;
      glyphs[i].x = x;
      glyphs[i].y = y;
      x += (s->padding_p ? 1 : ftcrfont_glyph_extents (s->font, code, NULL));
    }

  x_set_cr_source_with_gc_foreground (f, s->gc);
  cairo_set_font_face (cr, ftcrfont_info->cr_font_face);
  cairo_set_font_size (cr, s->font->pixel_size);
  /* cairo_set_font_matrix */
  /* cairo_set_font_options */

  FT_Activate_Size (ftcrfont_info->ft_size_draw);
  cairo_show_glyphs (cr, glyphs, len);
  surface = cairo_get_target (cr);
  /* XXX: It used to be necessary to flush when exporting.  It might
     be the case that this is no longer necessary.  */
  surface_type = cairo_surface_get_type (surface);
  if (surface_type != CAIRO_SURFACE_TYPE_XLIB
      && (surface_type != CAIRO_SURFACE_TYPE_IMAGE
	  || cairo_image_surface_get_format (surface) != CAIRO_FORMAT_ARGB32))
    cairo_surface_flush (surface);

  x_end_cr_clip (f);

  unblock_input ();

  return len;
}



struct font_driver const ftcrfont_driver =
  {
  .type = LISPSYM_INITIALLY (Qftcr),
  .get_cache = ftfont_get_cache,
  .list = ftcrfont_list,
  .match = ftcrfont_match,
  .list_family = ftfont_list_family,
  .open = ftcrfont_open,
  .close = ftcrfont_close,
  .has_char = ftfont_has_char,
  .encode_char = ftfont_encode_char,
  .text_extents = ftcrfont_text_extents,
  .draw = ftcrfont_draw,
  .get_bitmap = ftfont_get_bitmap,
  .anchor_point = ftfont_anchor_point,
#ifdef HAVE_LIBOTF
  .otf_capability = ftfont_otf_capability,
#endif
#if defined HAVE_M17N_FLT && defined HAVE_LIBOTF
  .shape = ftfont_shape,
#endif
#ifdef HAVE_OTF_GET_VARIATION_GLYPHS
  .get_variation_glyphs = ftfont_variation_glyphs,
#endif
  .filter_properties = ftfont_filter_properties,
  .combining_capability = ftfont_combining_capability,
  };

void
syms_of_ftcrfont (void)
{
  if (ftfont_info_size != offsetof (struct ftcrfont_info, cr_font_face))
    abort ();

  DEFSYM (Qftcr, "ftcr");
  register_font_driver (&ftcrfont_driver, NULL);
}
