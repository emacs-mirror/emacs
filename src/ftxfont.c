/* ftxfont.c -- FreeType font driver on X (without using XFT).
   Copyright (C) 2006-2018 Free Software Foundation, Inc.
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
#include <stdio.h>
#include <X11/Xlib.h>

#include "lisp.h"
#include "xterm.h"
#include "frame.h"
#include "blockinput.h"
#include "font.h"

/* FTX font driver.  */

struct ftxfont_frame_data
{
  /* Background and foreground colors.  */
  XColor colors[2];
  /* GCs interpolating the above colors.  gcs[0] is for a color
   closest to BACKGROUND, and gcs[5] is for a color closest to
   FOREGROUND.  */
  GC gcs[6];
  struct ftxfont_frame_data *next;
};


/* Return an array of 6 GCs for antialiasing.  */

static GC *
ftxfont_get_gcs (struct frame *f, unsigned long foreground, unsigned long background)
{
  XColor color;
  XGCValues xgcv;
  int i;
  struct ftxfont_frame_data *data = font_get_frame_data (f, Qftx);
  struct ftxfont_frame_data *prev = NULL, *this = NULL, *new;

  if (data)
    {
      for (this = data; this; prev = this, this = this->next)
	{
	  if (this->colors[0].pixel < background)
	    continue;
	  if (this->colors[0].pixel > background)
	    break;
	  if (this->colors[1].pixel < foreground)
	    continue;
	  if (this->colors[1].pixel > foreground)
	    break;
	  return this->gcs;
	}
    }

  new = xmalloc (sizeof *new);
  new->next = this;
  if (prev)
      prev->next = new;
  font_put_frame_data (f, Qftx, new);

  new->colors[0].pixel = background;
  new->colors[1].pixel = foreground;

  block_input ();
  XQueryColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), new->colors, 2);
  for (i = 1; i < 7; i++)
    {
      /* Interpolate colors linearly.  Any better algorithm?  */
      color.red
	= (new->colors[1].red * i + new->colors[0].red * (8 - i)) / 8;
      color.green
	= (new->colors[1].green * i + new->colors[0].green * (8 - i)) / 8;
      color.blue
	= (new->colors[1].blue * i + new->colors[0].blue * (8 - i)) / 8;
      if (! x_alloc_nearest_color (f, FRAME_X_COLORMAP (f), &color))
	break;
      xgcv.foreground = color.pixel;
      new->gcs[i - 1] = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
				   GCForeground, &xgcv);
    }
  unblock_input ();

  if (i < 7)
    {
      block_input ();
      for (i--; i >= 0; i--)
	XFreeGC (FRAME_X_DISPLAY (f), new->gcs[i]);
      unblock_input ();
      if (prev)
	prev->next = new->next;
      else if (data)
	font_put_frame_data (f, Qftx, new->next);
      xfree (new);
      return NULL;
    }
  return new->gcs;
}

static int
ftxfont_draw_bitmap (struct frame *f, GC gc_fore, GC *gcs, struct font *font,
                     unsigned int code, int x, int y, XPoint *p, int size,
                     int *n, bool flush)
{
  struct font_bitmap bitmap;
  unsigned char *b;
  int i, j;

  if (ftfont_get_bitmap (font, code, &bitmap, size > 0x100 ? 1 : 8) < 0)
    return 0;
  if (size > 0x100)
    {
      for (i = 0, b = bitmap.buffer; i < bitmap.rows;
	   i++, b += bitmap.pitch)
	{
	  for (j = 0; j < bitmap.width; j++)
	    if (b[j / 8] & (1 << (7 - (j % 8))))
	      {
		p[n[0]].x = x + bitmap.left + j;
		p[n[0]].y = y - bitmap.top + i;
		if (++n[0] == size)
		  {
                    XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
				 gc_fore, p, size, CoordModeOrigin);
		    n[0] = 0;
		  }
	      }
	}
      if (flush && n[0] > 0)
        XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
		     gc_fore, p, n[0], CoordModeOrigin);
    }
  else
    {
      for (i = 0, b = bitmap.buffer; i < bitmap.rows;
	   i++, b += bitmap.pitch)
	{
	  for (j = 0; j < bitmap.width; j++)
	    {
	      int idx = (bitmap.bits_per_pixel == 1
			 ? ((b[j / 8] & (1 << (7 - (j % 8)))) ? 6 : -1)
			 : (b[j] >> 5) - 1);

	      if (idx >= 0)
		{
		  XPoint *pp = p + size * idx;

		  pp[n[idx]].x = x + bitmap.left + j;
		  pp[n[idx]].y = y - bitmap.top + i;
		  if (++(n[idx]) == size)
		    {
                      XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
				   idx == 6 ? gc_fore : gcs[idx], pp, size,
				   CoordModeOrigin);
		      n[idx] = 0;
		    }
		}
	    }
	}
      if (flush)
	{
	  for (i = 0; i < 6; i++)
	    if (n[i] > 0)
              XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
			   gcs[i], p + 0x100 * i, n[i], CoordModeOrigin);
	  if (n[6] > 0)
            XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
			 gc_fore, p + 0x600, n[6], CoordModeOrigin);
	}
    }

  /* There is no ftfont_free_bitmap, so do not try to free BITMAP.  */

  return bitmap.advance;
}

static void
ftxfont_draw_background (struct frame *f, struct font *font, GC gc, int x, int y,
			 int width)
{
  XGCValues xgcv;

  XGetGCValues (FRAME_X_DISPLAY (f), gc,
		GCForeground | GCBackground, &xgcv);
  XSetForeground (FRAME_X_DISPLAY (f), gc, xgcv.background);
  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f), gc,
		  x, y - FONT_BASE (font), width, FONT_HEIGHT (font));
  XSetForeground (FRAME_X_DISPLAY (f), gc, xgcv.foreground);
}

static Lisp_Object
ftxfont_list (struct frame *f, Lisp_Object spec)
{
  Lisp_Object list = ftfont_list (f, spec), tail;

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    ASET (XCAR (tail), FONT_TYPE_INDEX, Qftx);
  return list;
}

static Lisp_Object
ftxfont_match (struct frame *f, Lisp_Object spec)
{
  Lisp_Object entity = ftfont_match (f, spec);

  if (VECTORP (entity))
    ASET (entity, FONT_TYPE_INDEX, Qftx);
  return entity;
}

static Lisp_Object
ftxfont_open (struct frame *f, Lisp_Object entity, int pixel_size)
{
  Lisp_Object font_object = ftfont_open (f, entity, pixel_size);
  if (NILP (font_object))
    return Qnil;
  struct font *font = XFONT_OBJECT (font_object);
  font->driver = &ftxfont_driver;
  return font_object;
}

static void
ftxfont_close (struct font *font)
{
  ftfont_close (font);
}

static int
ftxfont_draw (struct glyph_string *s, int from, int to, int x, int y,
              bool with_background)
{
  struct frame *f = s->f;
  struct face *face = s->face;
  struct font *font = s->font;
  XPoint p[0x700];
  int n[7];
  unsigned *code;
  int len = to - from;
  int i;
  GC *gcs;
  int xadvance;

  n[0] = n[1] = n[2] = n[3] = n[4] = n[5] = n[6] = 0;

  USE_SAFE_ALLOCA;
  SAFE_NALLOCA (code, 1, len);
  block_input ();
  if (with_background)
    ftxfont_draw_background (f, font, s->gc, x, y, s->width);
  for (i = 0; i < len; i++)
    code[i] = ((XCHAR2B_BYTE1 (s->char2b + from + i) << 8)
	       | XCHAR2B_BYTE2 (s->char2b + from + i));

  if (face->gc == s->gc)
    {
      gcs = ftxfont_get_gcs (f, face->foreground, face->background);
    }
  else
    {
      XGCValues xgcv;
      unsigned long mask = GCForeground | GCBackground;

      XGetGCValues (FRAME_X_DISPLAY (f), s->gc, mask, &xgcv);
      gcs = ftxfont_get_gcs (f, xgcv.foreground, xgcv.background);
    }

  if (gcs)
    {
      if (s->num_clips)
	for (i = 0; i < 6; i++)
	  XSetClipRectangles (FRAME_X_DISPLAY (f), gcs[i], 0, 0,
			      s->clip, s->num_clips, Unsorted);

      for (i = 0; i < len; i++)
	{
	  xadvance = ftxfont_draw_bitmap (f, s->gc, gcs, font, code[i], x, y,
					  p, 0x100, n, i + 1 == len);
	  x += (s->padding_p ? 1 : xadvance);
	}
      if (s->num_clips)
	for (i = 0; i < 6; i++)
	  XSetClipMask (FRAME_X_DISPLAY (f), gcs[i], None);
    }
  else
    {
      /* We can't draw with antialiasing.
	 s->gc should already have a proper clipping setting. */
      for (i = 0; i < len; i++)
	{
	  xadvance = ftxfont_draw_bitmap (f, s->gc, NULL, font, code[i], x, y,
					  p, 0x700, n, i + 1 == len);
	  x += (s->padding_p ? 1 : xadvance);
	}
    }

  unblock_input ();
  SAFE_FREE ();

  return len;
}

static int
ftxfont_end_for_frame (struct frame *f)
{
  struct ftxfont_frame_data *data = font_get_frame_data (f, Qftx);

  block_input ();
  while (data)
    {
      struct ftxfont_frame_data *next = data->next;
      int i;

      for (i = 0; i < 6; i++)
	XFreeGC (FRAME_X_DISPLAY (f), data->gcs[i]);
      xfree (data);
      data = next;
    }
  unblock_input ();
  font_put_frame_data (f, Qftx, NULL);
  return 0;
}



struct font_driver const ftxfont_driver =
  {
  /* We can't draw a text without device dependent functions.  */
  .type = LISPSYM_INITIALLY (Qftx),
  .get_cache = ftfont_get_cache,
  .list = ftxfont_list,
  .match = ftxfont_match,
  .list_family = ftfont_list_family,
  .open = ftxfont_open,
  .close = ftxfont_close,
  .has_char = ftfont_has_char,
  .encode_char = ftfont_encode_char,
  .text_extents = ftfont_text_extents,
  .draw = ftxfont_draw,
  .get_bitmap = ftfont_get_bitmap,
  .anchor_point = ftfont_anchor_point,
#ifdef HAVE_LIBOTF
  .otf_capability = ftfont_otf_capability,
#endif
  .end_for_frame = ftxfont_end_for_frame,
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
syms_of_ftxfont (void)
{
  DEFSYM (Qftx, "ftx");
  register_font_driver (&ftxfont_driver, NULL);
}
