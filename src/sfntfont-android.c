/* sfnt format font driver for Android.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
#include <dirent.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __aarch64__
#include <arm_neon.h>
#endif

#include <android/api-level.h>
#include <android/log.h>

#include "androidterm.h"
#include "sfntfont.h"
#include "pdumper.h"
#include "blockinput.h"
#include "android.h"

/* Structure describing a temporary buffer.  */

struct sfntfont_android_scanline_buffer
{
  /* Size of this buffer.  */
  size_t buffer_size;

  /* Pointer to the buffer data.  */
  void *buffer_data;
};

/* Array of directories to search for system fonts.  */
static char *system_font_directories[] =
  {
    (char *) "/system/fonts",
    (char *) "/product/fonts",
    /* This should be filled in by init_sfntfont_android.  */
    (char[PATH_MAX]) { },
  };

/* The font cache.  */
static Lisp_Object font_cache;

/* The scanline buffer.  */
static struct sfntfont_android_scanline_buffer scanline_buffer;

/* The largest size of the scanline buffer since the last window
   update.  */
static size_t max_scanline_buffer_size;



/* Return a temporary buffer for storing scan lines.
   Set BUFFER to the buffer upon success.  */

#ifndef __aarch64__

#define GET_SCANLINE_BUFFER(buffer, height, stride)		\
  do								\
    {								\
      size_t _size;						\
								\
      if (ckd_mul (&_size, height, stride))			\
	memory_full (SIZE_MAX);					\
								\
      if (_size < MAX_ALLOCA)					\
	(buffer) = alloca (_size);				\
      else							\
	{							\
	  if (_size > scanline_buffer.buffer_size)		\
	    {							\
	      (buffer)						\
		= scanline_buffer.buffer_data			\
		= xrealloc (scanline_buffer.buffer_data, 	\
			    _size);				\
	      scanline_buffer.buffer_size = _size;		\
	    }							\
	  else if (_size <= scanline_buffer.buffer_size)	\
	    (buffer) = scanline_buffer.buffer_data;		\
	  /* This is unreachable but clang says it is.  */	\
	  else							\
	    emacs_abort ();					\
								\
	  max_scanline_buffer_size				\
	    = max (_size, max_scanline_buffer_size);		\
	}							\
    } while (false);

#else

#define GET_SCANLINE_BUFFER(buffer, height, stride)		\
  do								\
    {								\
      size_t _size;						\
      void *_temp;						\
								\
      if (ckd_mul (&_size, height, stride))			\
	memory_full (SIZE_MAX);					\
								\
      if (_size > scanline_buffer.buffer_size)			\
	{							\
	  if (posix_memalign (&_temp, 16, _size))		\
	    memory_full (_size);				\
	  free (scanline_buffer.buffer_data);			\
	  (buffer)						\
	    = scanline_buffer.buffer_data			\
	    = _temp;						\
	  scanline_buffer.buffer_size = _size;			\
	}							\
      else if (_size <= scanline_buffer.buffer_size)		\
	(buffer) = scanline_buffer.buffer_data;			\
      /* This is unreachable but clang says it is.  */		\
      else							\
	emacs_abort ();						\
								\
      max_scanline_buffer_size					\
      = max (_size, max_scanline_buffer_size);			\
    } while (false);

#endif



/* Scale each of the four packed bytes in P in the low 16 bits of P by
   SCALE.  Return the result.

   SCALE is an integer between 0 and 256.  */

static unsigned int
sfntfont_android_scale32 (unsigned int scale, unsigned int p)
{
  uint32_t ag, rb;
  uint32_t scaled_ag, scaled_rb;

  ag = (p & 0xFF00FF00) >> 8;
  rb = (p & 0x00FF00FF);

  scaled_ag = (scale * ag) & 0xFF00FF00;
  scaled_rb = (scale * rb) >> 8 & 0x00FF00FF;

  return scaled_ag | scaled_rb;
}

static unsigned int
sfntfont_android_mul8x2 (unsigned int a8, unsigned int b32)
{
  unsigned int i;

  b32 &= 0xff00ff;
  i = a8 * b32 + 0x800080;

  return (i + ((i >> 8) & 0xff00ff)) >> 8 & 0xff00ff;
}

#define U255TO256(x) ((unsigned short) (x) + ((x) >> 7))

/* Blend two pixels SRC and DST without utilizing any control flow.
   Both SRC and DST are expected to be in premultiplied ABGB8888
   format.  Value is returned in premultiplied ARGB8888 format.  */

static unsigned int
sfntfont_android_blend (unsigned int src, unsigned int dst)
{
  unsigned int a, br_part, ag_part, both;

  a = (src >> 24);
  br_part = sfntfont_android_mul8x2 (255 - a, dst);
  ag_part = sfntfont_android_mul8x2 (255 - a, dst >> 8) << 8;

  both = ag_part | br_part;

  /* This addition need not be saturating because both has already
     been multiplied by 255 - a.  */
  return both + src;
}

#ifdef __aarch64__

/* Like U255TO256, but operates on vectors.  */

static uint16x8_t
sfntfont_android_u255to256 (uint8x8_t in)
{
  return vaddl_u8 (vshr_n_u8 (in, 7), in);
}

/* Use processor features to efficiently composite four pixels at SRC
   to DST.  */

static void
sfntfont_android_over_8888_1 (unsigned int *src, unsigned int *dst)
{
  uint8x8_t alpha;
  uint16x8_t alpha_c16, v1, v3, v4;
  uint8x8_t b, g, r, a, v2, v5;
  uint8x8x4_t _src, _dst;

  /* Pull in src and dst.

     This loads bytes, not words, so little endian ABGR becomes
     RGBA.  */
  _src = vld4_u8 ((const uint8_t *) src);
  _dst = vld4_u8 ((const uint8_t *) dst);

  /* Load constants.  */
  v4 = vdupq_n_u16 (256);
  v5 = vdup_n_u8 (0);

  /* Load src alpha.  */
  alpha = _src.val[3];

  /* alpha_c16 = 256 - 255TO256 (alpha).  */
  alpha_c16 = sfntfont_android_u255to256 (alpha);
  alpha_c16 = vsubq_u16 (v4, alpha_c16);

  /* Cout = Csrc + Cdst * alpha_c.  */
  v1 = vaddl_u8 (_dst.val[2], v5);
  v2 = _src.val[2];
  v3 = vmulq_u16 (v1, alpha_c16);
  b = vqadd_u8 (v2, vshrn_n_u16 (v3, 8));

  v1 = vaddl_u8 (_dst.val[1], v5);
  v2 = _src.val[1];
  v3 = vmulq_u16 (v1, alpha_c16);
  g = vqadd_u8 (v2, vshrn_n_u16 (v3, 8));

  v1 = vaddl_u8 (_dst.val[0], v5);
  v2 = _src.val[0];
  v3 = vmulq_u16 (v1, alpha_c16);
  r = vqadd_u8 (v2, vshrn_n_u16 (v3, 8));

#if 0
  /* Aout = Asrc + Adst * alpha_c.  */
  v1 = vaddl_u8 (_dst.val[3], v5);
  v2 = _src.val[3];
  v3 = vmulq_u16 (v1, alpha_c16);
  a = vqadd_u8 (v2, vshrn_n_u16 (v3, 8));
#else
  /* We know that Adst is always 1, so Asrc + Adst * (1 - Asrc) is
     always 1.  */
  a = vdup_n_u8 (255);
#endif

  /* Store back in dst.  */
  _dst.val[0] = r;
  _dst.val[1] = g;
  _dst.val[2] = b;
  _dst.val[3] = a;
  vst4_u8 ((uint8_t *) dst, _dst);
}

/* Use processor features to efficiently composite the buffer at SRC
   to DST.  Composite at most MAX - SRC words.

   If either SRC or DST are not yet properly aligned, value is 1.
   Otherwise, value is 0, and *X is incremented to the start of any
   trailing data which could not be composited due to data alignment
   constraints.  */

static int
sfntfont_android_over_8888 (unsigned int *src, unsigned int *dst,
			    unsigned int *max, unsigned int *x)
{
  size_t i;
  ptrdiff_t how_much;
  void *s, *d;

  /* Figure out how much can be composited by this loop.  */
  how_much = (max - src) & ~7;

  /* Return if there is not enough to vectorize.  */
  if (!how_much)
    return 1;

  /* Now increment *X by that much so the containing loop can process
     the remaining pixels one-by-one.  */

  *x += how_much;

  for (i = 0; i < how_much; i += 8)
    {
      s = (src + i);
      d = (dst + i);

      sfntfont_android_over_8888_1 (s, d);
    }

  return 0;
}

#endif

/* Composite the bitmap described by BUFFER, STRIDE and TEXT_RECTANGLE
   onto the native-endian ABGR8888 bitmap described by DEST and
   BITMAP_INFO.  RECT is the subset of the bitmap to composite.  */

static void
sfntfont_android_composite_bitmap (unsigned char *restrict buffer,
				   size_t stride,
				   unsigned char *restrict dest,
				   AndroidBitmapInfo *bitmap_info,
				   struct android_rectangle *text_rectangle,
				   struct android_rectangle *rect)
{
  unsigned int *src_row;
  unsigned int *dst_row;
  unsigned int i, src_y, x, src_x, max_x, dst_x;
#ifdef __aarch64__
  unsigned int lim_x;
#endif

  if ((intptr_t) dest & 3 || bitmap_info->stride & 3)
    /* This shouldn't be possible as Android is supposed to align the
       bitmap to at least a 4 byte boundary.  */
    emacs_abort ();
  else
    {
      for (i = 0; i < rect->height; ++i)
	{
	  if (i + rect->y >= bitmap_info->height)
	    /* Done.  */
	    return;

	  src_y = i + (rect->y - text_rectangle->y);

	  if (src_y > text_rectangle->height)
	    /* Huh? */
	    return;

	  src_row = (unsigned int *) ((buffer + src_y * stride));
	  dst_row = (unsigned int *) (dest + ((i + rect->y)
					      * bitmap_info->stride));

	  /* Figure out where the loop below should end.  */
	  max_x = min (rect->width, bitmap_info->width - rect->x);

	  /* Keep this loop simple! */
	  for (x = 0; x < max_x; ++x)
	    {
	      src_x = x + (rect->x - text_rectangle->x);
	      dst_x = x + rect->x;

#ifdef __aarch64__
	      /* This is the largest value of src_x.  */
	      lim_x = max_x + (rect->x - text_rectangle->x);

	      if (!sfntfont_android_over_8888 (src_row + src_x,
					       dst_row + dst_x,
					       src_row + lim_x,
					       &x))
		{
		  /* Decrement X by one so the for loop can increment
		     it again.  */
		  x--;
		  continue;
		}
#endif
		dst_row[dst_x]
		  = sfntfont_android_blend (src_row[src_x],
					    dst_row[dst_x]);
	    }
	}
    }
}

/* Calculate the union containing both A and B, both boxes.  Place the
   result in RESULT.  */

static void
sfntfont_android_union_boxes (struct gui_box a, struct gui_box b,
			      struct gui_box *result)
{
  result->x1 = min (a.x1, b.x1);
  result->y1 = min (a.y1, b.y1);
  result->x2 = max (a.x2, b.x2);
  result->y2 = max (a.y2, b.y2);
}

/* Draw the specified glyph rasters from FROM to TO on behalf of S,
   using S->gc.  Fill the background if WITH_BACKGROUND is true.

   See init_sfntfont_vendor and sfntfont_draw for more details.  */

static void
sfntfont_android_put_glyphs (struct glyph_string *s, int from,
			     int to, int x, int y, bool with_background,
			     struct sfnt_raster **rasters,
			     int *x_coords)
{
  struct android_rectangle background, text_rectangle, rect;
  struct gui_box text, character;
  unsigned int *buffer, *row;
  unsigned char *restrict raster_row;
  size_t stride, i;
  AndroidBitmapInfo bitmap_info;
  unsigned char *bitmap_data;
  jobject bitmap;
  int left, top, temp_y;
  unsigned int prod, raster_y;
  unsigned long foreground, back_pixel, rb;

  if (!s->gc->num_clip_rects)
    /* Clip region is empty.  */
    return;

  if (from == to)
    /* Nothing to draw.  */
    return;

  /* Swizzle the foreground and background in s->gc into BGR, then add
     an alpha channel.  */
  foreground = s->gc->foreground;
  back_pixel = s->gc->background;
  rb = foreground & 0x00ff00ff;
  foreground &= ~0x00ff00ff;
  foreground |= rb >> 16 | rb << 16 | 0xff000000;
  rb = back_pixel & 0x00ff00ff;
  back_pixel &= ~0x00ff00ff;
  back_pixel |= rb >> 16 | rb << 16 | 0xff000000;

  prepare_face_for_display (s->f, s->face);

  /* Build the scanline buffer.  Figure out the bounds of the
     background.  */
  memset (&background, 0, sizeof background);

  if (with_background)
    {
      background.x = x;
      background.y = y - FONT_BASE (s->font);
      background.width = s->width;
      background.height = FONT_HEIGHT (s->font);
    }

  /* Now figure out the bounds of the text.  */

  if (rasters[0])
    {
      text.x1 = x_coords[0] + rasters[0]->offx;
      text.x2 = text.x1 + rasters[0]->width;
      text.y1 = y - rasters[0]->height - rasters[0]->offy;
      text.y2 = y - rasters[0]->offy;
    }
  else
    memset (&text, 0, sizeof text);

  for (i = 1; i < to - from; ++i)
    {
      /* See if text has to be extended.  */

      if (!rasters[i])
	continue;

      character.x1 = x_coords[i] + rasters[i]->offx;
      character.x2 = character.x1 + rasters[i]->width;
      character.y1 = y - rasters[i]->height - rasters[i]->offy;
      character.y2 = y - rasters[i]->offy;

      sfntfont_android_union_boxes (text, character, &text);
    }

  /* Union the background rect with the text rectangle.  */
  text_rectangle.x = text.x1;
  text_rectangle.y = text.y1;
  text_rectangle.width = text.x2 - text.x1;
  text_rectangle.height = text.y2 - text.y1;
  gui_union_rectangles (&background, &text_rectangle,
			&text_rectangle);

  /* Allocate enough to hold text_rectangle.height, aligned to 8 (or
     16) bytes.  Then fill it with the background.  */
#ifndef __aarch64__
  stride = ((text_rectangle.width * sizeof *buffer) + 7) & ~7;
#else
  stride = ((text_rectangle.width * sizeof *buffer) + 15) & ~15;
#endif
  GET_SCANLINE_BUFFER (buffer, text_rectangle.height, stride);

  /* Try to optimize out this memset if the background rectangle
     contains the whole text rectangle.  */

  if (!with_background || memcmp (&background, &text_rectangle,
				  sizeof text_rectangle))
    memset (buffer, 0, text_rectangle.height * stride);

  if (with_background)
    {
      /* The background should have been filled in advance if a stipple
	 is enabled.  */
      eassert (s->gc->fill_style != ANDROID_FILL_OPAQUE_STIPPLED);

      /* Fill the background.  First, offset the background rectangle
	 to become relative from text_rectangle.x,
	 text_rectangle.y.  */
      background.x = background.x - text_rectangle.x;
      background.y = background.y - text_rectangle.y;
      eassert (background.x >= 0 && background.y >= 0);

      for (temp_y = background.y; (temp_y
				   < (background.y
				      + background.height));
	   ++temp_y)
	{
	  row = (unsigned int *) ((unsigned char *) buffer
				  + stride * temp_y);

	  for (x = background.x; x < background.x + background.width; ++x)
	    row[x] = back_pixel;
	}
    }

  /* Draw all the rasters onto the buffer.  */
  for (i = 0; i < to - from; ++i)
    {
      if (!rasters[i])
	continue;

      /* Figure out the top and left of the raster relative to
	 text_rectangle.  */
      left = x_coords[i] + rasters[i]->offx - text_rectangle.x;

      /* Note that negative offy represents the part of the text that
	 lies below the baseline.  */
      top = (y - (rasters[i]->height + rasters[i]->offy)
	     - text_rectangle.y);
      eassert (left >= 0 && top >= 0);

      /* Draw the raster onto the temporary bitmap using the
	 foreground color scaled by the alpha map.  */

      for (raster_y = 0; raster_y < rasters[i]->height; ++raster_y)
	{
	  row = (unsigned int *) ((unsigned char *) buffer
				  + stride * (raster_y + top));
	  raster_row = &rasters[i]->cells[raster_y * rasters[i]->stride];

	  for (x = 0; x < rasters[i]->width; ++x)
	    {
	      prod
		= sfntfont_android_scale32 (U255TO256 (raster_row[x]),
					    foreground);
	      row[left + x]
		= sfntfont_android_blend (prod, row[left + x]);
	    }
	}
    }

  /* Lock the bitmap.  It must be unlocked later.  */
  bitmap_data = android_lock_bitmap (FRAME_ANDROID_DRAWABLE (s->f),
				     &bitmap_info, &bitmap);

  /* If locking the bitmap fails, just discard the data that was
     allocated.  */
  if (!bitmap_data)
    return;

  /* Loop over each clip rect in the GC.  */
  eassert (bitmap_info.format == ANDROID_BITMAP_FORMAT_RGBA_8888);

  if (s->gc->num_clip_rects > 0)
    {
      for (i = 0; i < s->gc->num_clip_rects; ++i)
	{
	  if (!gui_intersect_rectangles (&s->gc->clip_rects[i],
					 &text_rectangle, &rect))
	    /* Outside the clip region.  */
	    continue;

	  /* Composite the intersection onto the buffer.  */
	  sfntfont_android_composite_bitmap ((unsigned char *) buffer,
					     stride, bitmap_data,
					     &bitmap_info,
					     &text_rectangle, &rect);
	}
    }
  else /* gc->num_clip_rects < 0 */
    sfntfont_android_composite_bitmap ((unsigned char *) buffer,
				       stride, bitmap_data,
				       &bitmap_info,
				       &text_rectangle,
				       &text_rectangle);

  /* Release the bitmap.  */
  AndroidBitmap_unlockPixels (android_java_env, bitmap);
  ANDROID_DELETE_LOCAL_REF (bitmap);

  /* Damage the window by the text rectangle.  */
  android_damage_window (FRAME_ANDROID_DRAWABLE (s->f),
			 &text_rectangle);

#undef MAX_ALLOCA
}



/* Shrink the scanline buffer after a window update.  If
   max_scanline_buffer_size is not zero, and is less than
   scanline_buffer.buffer_size / 2, then resize the scanline buffer to
   max_scanline_buffer_size.  */

void
sfntfont_android_shrink_scanline_buffer (void)
{
  if (!max_scanline_buffer_size)
    return;

  if (max_scanline_buffer_size
      < scanline_buffer.buffer_size / 2)
    {
      scanline_buffer.buffer_size
	= max_scanline_buffer_size;
      scanline_buffer.buffer_data
	= xrealloc (scanline_buffer.buffer_data,
		    max_scanline_buffer_size);
    }

  max_scanline_buffer_size = 0;
}



/* Font driver definition.  */

/* Return the font cache for this font driver.  F is ignored.  */

static Lisp_Object
sfntfont_android_get_cache (struct frame *f)
{
  return font_cache;
}

/* The Android sfntfont driver.  */
const struct font_driver android_sfntfont_driver =
  {
    .type = LISPSYM_INITIALLY (Qsfnt_android),
    .case_sensitive = true,
    .get_cache = sfntfont_android_get_cache,
    .list = sfntfont_list,
    .match = sfntfont_match,
    .draw = sfntfont_draw,
    .open_font = sfntfont_open,
    .close_font = sfntfont_close,
    .encode_char = sfntfont_encode_char,
    .text_extents = sfntfont_text_extents,
    .list_family = sfntfont_list_family,
    .get_variation_glyphs = sfntfont_get_variation_glyphs,

#ifdef HAVE_HARFBUZZ
    /* HarfBuzz support is enabled transparently on Android without
       using a separate font driver.  */
    .begin_hb_font = sfntfont_begin_hb_font,
    .combining_capability = hbfont_combining_capability,
    .shape = hbfont_shape,
    .otf_capability = hbfont_otf_capability,
#endif /* HAVE_HARFBUZZ */
  };



/* This is an ugly hack that should go away, but I can't think of
   how.  */

DEFUN ("android-enumerate-fonts", Fandroid_enumerate_fonts,
       Sandroid_enumerate_fonts, 0, 0, 0,
       doc: /* Enumerate fonts present on the system.

Signal an error if fonts have already been enumerated.  This would
normally have been done in C, but reading fonts require Lisp to be
loaded before character sets are made available.  */)
  (void)
{
  DIR *dir;
  int i;
  struct dirent *dirent;
  char name[PATH_MAX * 2];
  static bool enumerated;

  if (enumerated)
    error ("Fonts have already been enumerated");
  enumerated = true;

  block_input ();

  /* Scan through each of the system font directories.  Enumerate each
     font that looks like a TrueType font.  */
  for (i = 0; i < ARRAYELTS (system_font_directories); ++i)
    {
      dir = opendir (system_font_directories[i]);

      __android_log_print (ANDROID_LOG_VERBOSE, __func__,
			   "Loading fonts from: %s",
			   system_font_directories[i]);

      if (!dir)
	continue;

      while ((dirent = readdir (dir)))
	{
	  /* If it contains (not ends with!) with .ttf or .ttc, then
	     enumerate it.  */

	  if ((strstr (dirent->d_name, ".ttf")
	       || strstr (dirent->d_name, ".ttc"))
	      /* Ignore the non-variable Roboto font.  */
	      && (i != 0 || strcmp (dirent->d_name,
				    "RobotoStatic-Regular.ttf")))
	    {
	      sprintf (name, "%s/%s", system_font_directories[i],
		       dirent->d_name);
	      sfnt_enum_font (name);
	    }
	}

      closedir (dir);
    }

  unblock_input ();

  return Qnil;
}



static void
syms_of_sfntfont_android_for_pdumper (void)
{
  init_sfntfont_vendor (Qsfnt_android, &android_sfntfont_driver,
			sfntfont_android_put_glyphs);
  register_font_driver (&android_sfntfont_driver, NULL);
}

void
init_sfntfont_android (void)
{
  int api_level;

  if (!android_init_gui)
    return;

  api_level = android_get_current_api_level ();

  /* Make sure to pick the proper Sans Serif and Serif fonts for the
     version of Android the device is running.  */

  if (api_level >= 21)
    /* Android 5.0 and later distribute Noto Serif in lieu of Droid
       Serif.  */
    Vsfnt_default_family_alist
      = list4 (Fcons (build_string ("Monospace"),
		      build_string ("Droid Sans Mono")),
	       /* Android doesn't come with a Monospace Serif font, so
		  this will have to do.  */
	       Fcons (build_string ("Monospace Serif"),
		      build_string ("Droid Sans Mono")),
	       Fcons (build_string ("Sans Serif"),
		      build_string ("Roboto")),
	       Fcons (build_string ("DejaVu Serif"),
		      build_string ("Noto Serif")));
  else if (api_level >= 14)
    /* Android 4.0 and later distribute Roboto in lieu of Droid
       Sans.  */
    Vsfnt_default_family_alist
      = list4 (Fcons (build_string ("Monospace"),
		      build_string ("Droid Sans Mono")),
	       /* Android doesn't come with a Monospace Serif font, so
		  this will have to do.  */
	       Fcons (build_string ("Monospace Serif"),
		      build_string ("Droid Sans Mono")),
	       Fcons (build_string ("Sans Serif"),
		      build_string ("Roboto")),
	       Fcons (build_string ("DejaVu Serif"),
		      build_string ("Droid Serif")));
  else
    Vsfnt_default_family_alist
      = list4 (Fcons (build_string ("Monospace"),
		      build_string ("Droid Sans Mono")),
	       Fcons (build_string ("Monospace Serif"),
		      build_string ("Droid Sans Mono")),
	       Fcons (build_string ("Sans Serif"),
		      build_string ("Droid Sans")),
	       Fcons (build_string ("DejaVu Serif"),
		      build_string ("Droid Serif")));

  /* Set up the user fonts directory.  This directory is ``fonts'' in
     the Emacs files directory.  */
  snprintf (system_font_directories[2], PATH_MAX, "%s/fonts",
	    android_get_home_directory ());
}

void
syms_of_sfntfont_android (void)
{
  DEFSYM (Qsfnt_android, "sfnt-android");
  DEFSYM (Qandroid_enumerate_fonts, "android-enumerate-fonts");
  Fput (Qandroid, Qfont_driver_superseded_by, Qsfnt_android);

  font_cache = list (Qnil);
  staticpro (&font_cache);

  defsubr (&Sandroid_enumerate_fonts);

  pdumper_do_now_and_after_load (syms_of_sfntfont_android_for_pdumper);
}
