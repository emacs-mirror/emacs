/* ftxfont.c -- FreeType font driver on X (without using XFT).
   Copyright (C) 2006 Free Software Foundation, Inc.
   Copyright (C) 2006
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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

/* FTX font driver.  */

static Lisp_Object Qftx;

/* Prototypes for helper function.  */
static int ftxfont_draw_bitmap P_ ((FRAME_PTR, GC *, struct font *, unsigned,
				    int, int, XPoint *, int, int *n));
static void ftxfont_draw_backgrond P_ ((FRAME_PTR, struct font *, GC,
					int, int, int));

static int
ftxfont_draw_bitmap (f, gc, font, code, x, y, p, size, n)
     FRAME_PTR f;
     GC *gc;
     struct font *font;
     unsigned code;
     int x, y;
     XPoint *p;
     int size, *n;
{
  struct font_bitmap bitmap;
  unsigned char *b;
  int i, j;

  if (ftfont_driver.get_bitmap (font, code, &bitmap, 1) < 0)
    return 0;
  for (i = 0, b = bitmap.buffer; i < bitmap.rows;
       i++, b += bitmap.pitch)
    {
      if (size > 0x100)
	{
	  for (j = 0; j < bitmap.width; j++)
	    if (b[j / 8] & (1 << (7 - (j % 8))))
	      {
		p[n[0]].x = x + bitmap.left + j;
		p[n[0]].y = y - bitmap.top + i;
		if (++n[0] == 0x400)
		  {
		    XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				 gc[0], p, size, CoordModeOrigin);
		    n[0] = 0;
		  }
	      }
	}
      else
	{
	  for (j = 0; j < bitmap.width; j++)
	    {
	      int idx = (b[j] >> 5) - 1;

	      if (idx >= 0)
		{
		  XPoint *pp = p + size * idx;

		  pp[n[idx]].x = x + bitmap.left + j;
		  pp[n[idx]].y = y - bitmap.top + i;
		  if (++(n[idx]) == 0x100)
		    {
		      XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				   gc[idx], pp, size, CoordModeOrigin);
		      n[idx] = 0;
		    }
		}
	    }
	}
    }

  if (ftfont_driver.free_bitmap)
    ftfont_driver.free_bitmap (font, &bitmap);

  return bitmap.advance;
}

static void
ftxfont_draw_backgrond (f, font, gc, x, y, width)
     FRAME_PTR f;
     struct font *font;
     GC gc;
     int x, y, width;
{
  XGCValues xgcv;

  XGetGCValues (FRAME_X_DISPLAY (f), gc,
		GCForeground | GCBackground, &xgcv);
  XSetForeground (FRAME_X_DISPLAY (f), gc, xgcv.background);
  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		  x, y - font->ascent, width, font->font.height);
  XSetForeground (FRAME_X_DISPLAY (f), gc, xgcv.foreground);
}

/* Prototypes for font-driver methods.  */
static Lisp_Object ftxfont_list P_ ((Lisp_Object, Lisp_Object));
static struct font *ftxfont_open P_ ((FRAME_PTR, Lisp_Object, int));
static void ftxfont_close P_ ((FRAME_PTR, struct font *));
static int ftxfont_prepare_face (FRAME_PTR, struct face *);
static void ftxfont_done_face (FRAME_PTR, struct face *);

static int ftxfont_draw P_ ((struct glyph_string *, int, int, int, int, int));

struct font_driver ftxfont_driver;

static Lisp_Object
ftxfont_list (frame, spec)
     Lisp_Object frame;
     Lisp_Object spec;
{
  Lisp_Object val = ftfont_driver.list (frame, spec);
  
  if (! NILP (val))
    {
      int i;

      for (i = 0; i < ASIZE (val); i++)
	ASET (AREF (val, i), FONT_TYPE_INDEX, Qftx);
    }
  return val;
}

static struct font *
ftxfont_open (f, entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object entity;
     int pixel_size;
{
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  struct font *font;
  XFontStruct *xfont = malloc (sizeof (XFontStruct));
  
  if (! xfont)
    return NULL;
  font = ftfont_driver.open (f, entity, pixel_size);
  if (! font)
    {
      free (xfont);
      return NULL;
    }

  xfont->fid = FRAME_FONT (f)->fid;
  xfont->ascent = font->ascent;
  xfont->descent = font->descent;
  xfont->max_bounds.width = font->font.size;
  xfont->min_bounds.width = font->min_width;
  font->font.font = xfont;
  font->driver = &ftxfont_driver;

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
ftxfont_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  ftfont_driver.close (f, font);
  FRAME_X_DISPLAY_INFO (f)->n_fonts--;
}

static int
ftxfont_prepare_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  GC gc[6];
  XColor colors[3];
  XGCValues xgcv;
  unsigned long mask = GCForeground | GCBackground | GCGraphicsExposures;
  int i;

  face->extra = NULL;

  /* Here, we create 6 more GCs to simulate anti-aliasing.  */
  BLOCK_INPUT;
  XGetGCValues (FRAME_X_DISPLAY (f), face->gc, mask, &xgcv);
  colors[0].pixel = face->foreground;
  colors[1].pixel = face->background;
  XQueryColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), colors, 2);
  for (i = 1; i < 7; i++)
    {
      colors[2].red = (colors[0].red * i + colors[1].red * (7 - i)) / 7;
      colors[2].green = (colors[0].green * i + colors[1].green * (7 - i)) / 7;
      colors[2].blue = (colors[0].blue * i + colors[1].blue * (7 - i)) / 7;
      if (! x_alloc_nearest_color (f, FRAME_X_COLORMAP (f), &colors[2]))
	break;
      xgcv.foreground = colors[2].pixel;
      gc[i - 1] = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			     mask, &xgcv);
    }
  UNBLOCK_INPUT;

  if (i < 7)
    return -1;
  face->extra = malloc (sizeof (GC) * 7);
  if (! face->extra)
    return -1;
  for (i = 0; i < 6; i++)
    ((GC *) face->extra)[i] = gc[i];
  ((GC *) face->extra)[i] = face->gc;
  return 0;
}

static void
ftxfont_done_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  if (face->extra)
    {
      int i;

      BLOCK_INPUT;
      for (i = 0; i < 7; i++)
	XFreeGC (FRAME_X_DISPLAY (f), ((GC *) face->extra)[i]);
      UNBLOCK_INPUT;
      free (face->extra);
      face->extra = NULL;
    }
}

static int
ftxfont_draw (s, from, to, x, y, with_background)
     struct glyph_string *s;
     int from, to, x, y, with_background;
{
  FRAME_PTR f = s->f;
  struct face *face = s->face;
  struct font *font = (struct font *) face->font;
  XPoint p[0x700];
  int n[7];
  unsigned *code;
  int len = to - from;
  int i;

  n[0] = n[1] = n[2] = n[3] = n[4] = n[5] = n[6] = 0;

  BLOCK_INPUT;

  if (with_background)
    ftxfont_draw_backgrond (f, font, s->gc, x, y, s->width);
  code = alloca (sizeof (unsigned) * len);
  for (i = 0; i < len; i++)
    code[i] = ((XCHAR2B_BYTE1 (s->char2b + from + i) << 8)
	       | XCHAR2B_BYTE2 (s->char2b + from + i));

  if (! face->extra)
    {
      for (i = 0; i < len; i++)
	x += ftxfont_draw_bitmap (f, &face->gc, font, code[i], x, y,
				  p, 0x700, n);
      if (n[0] > 0)
	XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     face->gc, p, n[0], CoordModeOrigin);
    }
  else
    {
      GC *gc = face->extra;

      for (i = 0; i < len; i++)
	x += ftxfont_draw_bitmap (f, &face->gc, font, code[i], x, y,
				  p, 0x100, n);
      for (i = 0; i < 7; i++)
	if (n[i] > 0)
	  XDrawPoints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		       gc[i], p + 0x100 * i, n[i], CoordModeOrigin);
    }

  UNBLOCK_INPUT;

  return len;
}



void
syms_of_ftxfont ()
{
  DEFSYM (Qftx, "ftx");

  ftxfont_driver = ftfont_driver;
  ftxfont_driver.type = Qftx;
  ftxfont_driver.list = ftxfont_list;
  ftxfont_driver.open = ftxfont_open;
  ftxfont_driver.close = ftxfont_close;
  ftxfont_driver.prepare_face = ftxfont_prepare_face;
  ftxfont_driver.done_face = ftxfont_done_face;
  ftxfont_driver.draw = ftxfont_draw;

  register_font_driver (&ftxfont_driver, NULL);
}
