/* ftcrfont.c -- FreeType font driver on cairo.
   Copyright (C) 2015-2020 Free Software Foundation, Inc.

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
#include "xterm.h"
#include "blockinput.h"
#include "charset.h"
#include "composite.h"
#include "font.h"
#include "ftfont.h"
#include "pdumper.h"

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
      cache->ascent = ceil (- extents.y_bearing);
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
  if (!font_face)
    {
      unblock_input ();
      FcPatternDestroy (match);
      return Qnil;
    }
  cairo_matrix_t font_matrix, ctm;
  cairo_matrix_init_scale (&font_matrix, pixel_size, pixel_size);
  cairo_matrix_init_identity (&ctm);
  cairo_font_options_t *options = cairo_font_options_create ();
  cairo_scaled_font_t *scaled_font
    = cairo_scaled_font_create (font_face, &font_matrix, &ctm, options);
  cairo_font_face_destroy (font_face);
  cairo_font_options_destroy (options);
  unblock_input ();

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
      if (this_width > 0
	  && (! font->min_width
	      || font->min_width > this_width))
	font->min_width = this_width;
      if (this_width > font->max_width)
	font->max_width = this_width;
      if (c == 32)
	font->space_width = this_width;
      font->average_width += this_width;
    }
  font->average_width /= 95;

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

  ft_face = cairo_ft_scaled_font_lock_face (scaled_font);
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

static float* create_kernel(float radius, float deviation) {
  static float last_radius = -1.0;
  static float last_dev = -1.0;
  static float *kernel = NULL;

  if (last_radius == radius && last_dev == deviation)
    return kernel;

  if (kernel) free(kernel);

  int size = 2 * (int)(radius) + 1;
  kernel = (float*)(malloc(sizeof(float) * (size + 1)));
  float radiusf = fabs(radius) + 1.0f;
  float value = -radius;
  float sum = 0.0f;
  int i;

  if(!kernel) return 0;

  if(deviation == 0.0f)
    deviation = sqrt(-(radiusf * radiusf) / (2.0f * log(1.0f / 255.0f)));

  kernel[0] = size;

  for(i = 0; i < size; i++) {
    kernel[1 + i] =
      1.0f / (2.506628275f * deviation) *
      exp(-((value * value) / (2.0f * (deviation * deviation))))
      ;

    sum += kernel[1 + i];
    value += 1.0f;
  }

  for(i = 0; i < size; i++) kernel[1 + i] /= sum;

  return kernel;
}


/* FIXME: this is WAYYYYYYYY TOOOOO SLOOOWW */
cairo_bool_t cairou_gaussian_blur(
                                  cairo_surface_t* surface,
                                  float radius,
                                  float deviation
                                  ) {
  float* horzBlur = 0;
  float* vertBlur = 0;
  float* kernel = 0;
  unsigned char* data = 0;
  cairo_format_t format;
  int width;
  int height;
  int stride;
  unsigned int channels;
  int iY;
  int iX;

  if(cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS)
    return false;

  data = cairo_image_surface_get_data(surface);
  format = cairo_image_surface_get_format(surface);
  width = cairo_image_surface_get_width(surface);
  height = cairo_image_surface_get_height(surface);
  stride = cairo_image_surface_get_stride(surface);
  channels = 4;

  horzBlur = (float*)(malloc(sizeof(float) * height * stride));
  vertBlur = (float*)(malloc(sizeof(float) * height * stride));
  kernel = create_kernel(radius, deviation);

  if(!horzBlur || !vertBlur || !kernel) return false;

  /* Horizontal pass. */
  for(iY = 0; iY < height; iY++) {
    for(iX = 0; iX < width; iX++) {
      float red = 0.0f;
      float green = 0.0f;
      float blue = 0.0f;
      float alpha = 0.0f;
      int offset = (int)(kernel[0]) / -2;
      int baseOffset;
      int i;

      for(i = 0; i < (int)(kernel[0]); i++) {
        unsigned char* dataPtr = 0;
        int x = iX + offset;
        float kernip1;

        if(x < 0 || x >= width) continue;

        dataPtr = &data[iY * stride + x * channels];
        kernip1 = kernel[i + 1];

        if(channels == 1) alpha += kernip1 * dataPtr[0];

        else {
          if(channels == 4) alpha += kernip1 * dataPtr[3];

          red += kernip1 * dataPtr[2];
          green += kernip1 * dataPtr[1];
          blue+= kernip1 * dataPtr[0];
        }

        offset++;
      }

      baseOffset = iY * stride + iX * channels;

      if(channels == 1) horzBlur[baseOffset] = alpha;

      else {
        if(channels == 4) horzBlur[baseOffset + 3] = alpha;

        horzBlur[baseOffset + 2] = red;
        horzBlur[baseOffset + 1] = green;
        horzBlur[baseOffset] = blue;
      }
    }
  }

  /* Vertical pass. */
  for(iY = 0; iY < height; iY++) {
    for(iX = 0; iX < width; iX++) {
      float red = 0.0f;
      float green = 0.0f;
      float blue = 0.0f;
      float alpha = 0.0f;
      int offset = (int)(kernel[0]) / -2;
      int baseOffset;
      int i;

      for(i = 0; i < (int)(kernel[0]); i++) {
        float* dataPtr = 0;
        int y = iY + offset;
        float kernip1;

        if(y < 0 || y >= height) {
          offset++;

          continue;
        }

        dataPtr = &horzBlur[y * stride + iX * channels];
        kernip1 = kernel[i + 1];

        if(channels == 1) alpha += kernip1 * dataPtr[0];

        else {
          if(channels == 4) alpha += kernip1 * dataPtr[3];

          red += kernip1 * dataPtr[2];
          green += kernip1 * dataPtr[1];
          blue += kernip1 * dataPtr[0];
        }

        offset++;
      }

      baseOffset = iY * stride + iX * channels;

      if(channels == 1) vertBlur[baseOffset] = alpha;

      else {
        if(channels == 4) vertBlur[baseOffset + 3] = alpha;

        vertBlur[baseOffset + 2] = red;
        vertBlur[baseOffset + 1] = green;
        vertBlur[baseOffset] = blue;
      }
    }
  }

  for(iY = 0; iY < height; iY++) {
    for(iX = 0; iX < width; iX++) {
      int i = iY * stride + iX * channels;

      if(channels == 1) data[i] = (unsigned char)(vertBlur[i]);

      else {
        if(channels == 4) data[i + 3] = (unsigned char)(
                                                        vertBlur[i + 3]
                                                        );

        data[i + 2] = (unsigned char)(vertBlur[i + 2]);
        data[i + 1] = (unsigned char)(vertBlur[i + 1]);
        data[i] = (unsigned char)(vertBlur[i]);
      }
    }
  }

  free(horzBlur);
  free(vertBlur);

  return true;
}

inline uint32_t
plus4(uint32_t x, uint32_t y)
{
  uint8_t a, b, c, d;
  a = (x >> 24) & 0xff;
  b = (x >> 16) & 0xff;
  c = (x >> 8 ) & 0xff;
  d = x         & 0xff;
  a += (y >> 24) & 0xff;
  b += (y >> 16) & 0xff;
  c += (y >> 8 ) & 0xff;
  d += y         & 0xff;
  return (a << 24) | (b << 16) | (c << 8) | d;
}

inline uint32_t
minus4(uint32_t x, uint32_t y)
{
  uint8_t a, b, c, d;
  a = (x >> 24) & 0xff;
  b = (x >> 16) & 0xff;
  c = (x >> 8 ) & 0xff;
  d = x         & 0xff;
  a -= (y >> 24) & 0xff;
  b -= (y >> 16) & 0xff;
  c -= (y >> 8 ) & 0xff;
  d -= y         & 0xff;
  return (a << 24) | (b << 16) | (c << 8) | d;
}

inline uint32_t
div4(uint32_t x, uint32_t y)
{
  uint8_t a, b, c, d;
  a = (x >> 24) & 0xff;
  b = (x >> 16) & 0xff;
  c = (x >> 8 ) & 0xff;
  d = x         & 0xff;
  return (a / y << 24) | (b / y << 16) | (c / y << 8) | d / y;
}

static void
box_blur (cairo_surface_t *surface, int iteration)
{
  uint8_t *data, *blur;
  int width, height, stride, x, y;

  if (cairo_surface_status (surface) != CAIRO_STATUS_SUCCESS)
    return;
  eassert (cairo_surface_get_type (surface) == CAIRO_SURFACE_TYPE_IMAGE);
  eassert (cairo_image_surface_get_format (surface) == CAIRO_FORMAT_ARGB32);

  if (iteration <= 0)
    return;

  cairo_surface_flush (surface);
  width = cairo_image_surface_get_width (surface);
  height = cairo_image_surface_get_height (surface);
  stride = cairo_image_surface_get_stride (surface);
  blur = malloc (height * width * sizeof (uint32_t));
  memset (blur, 0, sizeof (height * width * sizeof (uint32_t)));

  while (--iteration >= 0)
    {
      uint32_t *src, *dst;
      data = cairo_image_surface_get_data (surface);

      // Horizontal, data -> blur
      for (y = 0; y < height; y++)
        {
          uint32_t acc = 0;
          src = (uint32_t *)&data[y * stride];
          dst = (uint32_t *)&blur[y * stride];
          if (width < 3)
            break;
          acc = plus4(src[0], src[1]);
          for (x = 2; x < width; x++)
            {
              acc = plus4(acc, src[x]);
              dst[x-1] = div4(acc, 3);
              acc = minus4(acc, src[x-2]);
            }
        }

      // Vertical, blur -> data
      if (height < 2)
        break;
      uint32_t *row = (uint32_t *) malloc(width * sizeof(uint32_t));
      memcpy (row, blur, width * sizeof(uint32_t));
      for (x = 0; x < width; x++)
        row[x] = plus4(row[x], *(uint32_t*)(blur+stride+x));
      for (y = 2; y < height; y++)
        {
          uint32_t *src = (uint32_t *)(blur + y * stride);
          uint32_t *dst1 = (uint32_t *)(data + (y-1) * stride);
          uint32_t *src2 = (uint32_t *)(blur + (y-2) * stride);
          for (x = 0; x < width; x++)
            {
              row[x] = plus4(row[x], src[x]);
              dst1[x] = div4(row[x], 3);
              row[x] = minus4(row[x], src2[x]);
            }
        }
      free (row);
    }

  cairo_surface_mark_dirty (surface);
  free (blur);
}

static int
ftcrfont_draw (struct glyph_string *s,
               int from, int to, int x, int y, bool with_background)
{
  struct frame *f = s->f;
  struct face *face = s->face;
  struct font_info *ftcrfont_info = (struct font_info *) s->font;
  cairo_t *cr;
  cairo_glyph_t *glyphs;
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
      glyphs[i].index = s->char2b[from + i];
      glyphs[i].x = x;
      glyphs[i].y = y;
      x += (s->padding_p ? 1 : ftcrfont_glyph_extents (s->font,
                                                       glyphs[i].index,
                                                       NULL));
    }

  if (face->shadow_p)
    {
      double x1, y1, w1, h1;
      cairo_surface_t *surface;
      cairo_t *dc;

      cairo_clip_extents (cr, &x1, &y1, &w1, &h1);
      surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, w1, h1);
      dc = cairo_create (surface);
      cairo_translate (dc, -x1 + face->shadow_offset.x,
                       -y1 + face->shadow_offset.y);
      cairo_set_source_rgb (dc, 0, 0, 0);
      cairo_set_scaled_font (dc, ftcrfont_info->cr_scaled_font);
      cairo_show_glyphs (dc, glyphs, len);
      box_blur (surface, ceil (face->shadow_blur));
      if (face->shadow_color_defaulted_p)
        x_set_cr_source_with_gc_foreground (f, s->gc);
      else
        {
          // TODO: get color RGB
          x_set_cr_source_with_gc_foreground (f, s->gc);
        }
      cairo_mask_surface (cr, surface, x1, y1);

      cairo_destroy (dc);
      cairo_surface_destroy (surface);
    }

  x_set_cr_source_with_gc_foreground (f, s->gc);
  cairo_set_scaled_font (cr, ftcrfont_info->cr_scaled_font);
  cairo_show_glyphs (cr, glyphs, len);

  x_end_cr_clip (f);

  unblock_input ();

  return len;
}


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
  if (ftcrfont_info->bitmap_position_unit)
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
