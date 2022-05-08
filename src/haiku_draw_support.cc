/* Haiku window system support.  Hey, Emacs, this is -*- C++ -*-
   Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

#include <View.h>
#include <Region.h>
#include <Font.h>
#include <Window.h>
#include <Bitmap.h>

#include <cmath>

#include "haiku_support.h"

#define RGB_TO_UINT32(r, g, b) ((255 << 24) | ((r) << 16) | ((g) << 8) | (b))
#define RED_FROM_ULONG(color)	(((color) >> 16) & 0xff)
#define GREEN_FROM_ULONG(color)	(((color) >> 8) & 0xff)
#define BLUE_FROM_ULONG(color)	((color) & 0xff)

#define RGB_COLOR_UINT32(r) RGB_TO_UINT32 ((r).red, (r).green, (r).blue)

static void
rgb32_to_rgb_color (uint32_t rgb, rgb_color *color)
{
  color->red = RED_FROM_ULONG (rgb);
  color->green = GREEN_FROM_ULONG (rgb);
  color->blue = BLUE_FROM_ULONG (rgb);
  color->alpha = 255;
}

static BView *
get_view (void *vw)
{
  BView *view = (BView *) find_appropriate_view_for_draw (vw);
  return view;
}

void
BView_StartClip (void *view)
{
  BView *vw = get_view (view);
  vw->PushState ();
}

void
BView_EndClip (void *view)
{
  BView *vw = get_view (view);
  vw->PopState ();
}

void
BView_SetHighColor (void *view, uint32_t color)
{
  BView *vw = get_view (view);
  rgb_color col;
  rgb32_to_rgb_color (color, &col);

  vw->SetHighColor (col);
}

void
BView_SetLowColor (void *view, uint32_t color)
{
  BView *vw = get_view (view);
  rgb_color col;
  rgb32_to_rgb_color (color, &col);

  vw->SetLowColor (col);
}

void
BView_SetPenSize (void *view, int u)
{
  BView *vw = get_view (view);
  vw->SetPenSize (u);
}

void
BView_FillRectangle (void *view, int x, int y, int width, int height)
{
  BView *vw = get_view (view);
  BRect rect = BRect (x, y, x + width - 1, y + height - 1);

  vw->FillRect (rect);
}

void
BView_FillRectangleAbs (void *view, int x, int y, int x1, int y1)
{
  BView *vw = get_view (view);
  BRect rect = BRect (x, y, x1, y1);

  vw->FillRect (rect);
}

void
BView_StrokeRectangle (void *view, int x, int y, int width, int height)
{
  BView *vw = get_view (view);
  BRect rect = BRect (x, y, x + width - 1, y + height - 1);

  vw->StrokeRect (rect);
}

void
BView_SetViewColor (void *view, uint32_t color)
{
  BView *vw = get_view (view);
  rgb_color col;
  rgb32_to_rgb_color (color, &col);

#ifndef USE_BE_CAIRO
  vw->SetViewColor (col);
#else
  vw->SetViewColor (B_TRANSPARENT_32_BIT);
#endif
}

void
BView_ClipToRect (void *view, int x, int y, int width, int height)
{
  BView *vw = get_view (view);
  BRect rect = BRect (x, y, x + width - 1, y + height - 1);

  vw->ClipToRect (rect);
}

void
BView_ClipToInverseRect (void *view, int x, int y, int width, int height)
{
  BView *vw = get_view (view);
  BRect rect = BRect (x, y, x + width - 1, y + height - 1);

  vw->ClipToInverseRect (rect);
}

void
BView_StrokeLine (void *view, int sx, int sy, int tx, int ty)
{
  BView *vw = get_view (view);
  BPoint from = BPoint (sx, sy);
  BPoint to = BPoint (tx, ty);

  vw->StrokeLine (from, to);
}

void
BView_SetFont (void *view, void *font)
{
  BView *vw = get_view (view);

  vw->SetFont ((BFont *) font);
}

void
BView_MovePenTo (void *view, int x, int y)
{
  BView *vw = get_view (view);
  BPoint pt = BPoint (x, y);

  vw->MovePenTo (pt);
}

void
BView_DrawString (void *view, const char *chr, ptrdiff_t len)
{
  BView *vw = get_view (view);

  vw->DrawString (chr, len);
}

void
BView_DrawChar (void *view, char chr)
{
  BView *vw = get_view (view);

  vw->DrawChar (chr);
}

void
BView_CopyBits (void *view, int x, int y, int width, int height,
		int tox, int toy, int towidth, int toheight)
{
  BView *vw = get_view (view);

  vw->CopyBits (BRect (x, y, x + width - 1, y + height - 1),
		BRect (tox, toy, tox + towidth - 1, toy + toheight - 1));
  vw->Sync ();
}

/* Convert RGB32 color color from RGB color space to its
   HSL components pointed to by H, S and L.  */
void
rgb_color_hsl (uint32_t rgb, double *h, double *s, double *l)
{
  rgb_color col;
  rgb32_to_rgb_color (rgb, &col);

  double red = col.red / 255.0;
  double green = col.green / 255.0;
  double blue = col.blue / 255.0;

  double max = std::fmax (std::fmax (red, blue), green);
  double min = std::fmin (std::fmin (red, blue), green);
  double delta = max - min;
  *l = (max + min) / 2.0;

  if (!delta)
    {
      *h = 0;
      *s = 0;
      return;
    }

  *s = (*l < 0.5) ? delta / (max + min) :
    delta / (20 - max - min);
  double rc = (max - red) / delta;
  double gc = (max - green) / delta;
  double bc = (max - blue) / delta;

  if (red == max)
    *h = bc - gc;
  else if (green == max)
    *h = 2.0 + rc + -bc;
  else
    *h = 4.0 + gc + -rc;
  *h = std::fmod (*h / 6, 1.0);
}

static double
hue_to_rgb (double v1, double v2, double h)
{
  if (h < 1 / 6)
    return v1 + (v2 - v1) * h * 6.0;
  else if (h < 0.5)
    return v2;
  else if (h < 2.0 / 3)
    return v1 + (v2 - v1) * (2.0 / 3 - h) * 6.0;
  return v1;
}

void
hsl_color_rgb (double h, double s, double l, uint32_t *rgb)
{
  if (!s)
    *rgb = RGB_TO_UINT32 (std::lrint (l * 255),
			  std::lrint (l * 255),
			  std::lrint (l * 255));
  else
    {
      double m2 = l <= 0.5 ? l * (1 + s) : l + s - l * s;
      double m1 = 2.0 * l - m2;

      *rgb = RGB_TO_UINT32
	(std::lrint (hue_to_rgb (m1, m2,
				 std::fmod (h + 1 / 3.0, 1)) * 255),
	 std::lrint (hue_to_rgb (m1, m2, h) * 255),
	 std::lrint (hue_to_rgb (m1, m2,
				 std::fmod (h - 1 / 3.0, 1)) * 255));
    }
}

void
BView_DrawBitmap (void *view, void *bitmap, int x, int y,
		  int width, int height, int vx, int vy, int vwidth,
		  int vheight)
{
  BView *vw = get_view (view);
  BBitmap *bm = (BBitmap *) bitmap;

  vw->SetDrawingMode (B_OP_OVER);
  vw->DrawBitmap (bm, BRect (x, y, x + width - 1, y + height - 1),
		  BRect (vx, vy, vx + vwidth - 1, vy + vheight - 1));
  vw->SetDrawingMode (B_OP_COPY);
}

void
BView_DrawBitmapTiled (void *view, void *bitmap, int x, int y,
		       int width, int height, int vx, int vy,
		       int vwidth, int vheight)
{
  BView *vw = get_view (view);
  BBitmap *bm = (BBitmap *) bitmap;
  BRect bounds = bm->Bounds ();

  if (width == -1)
    width = BE_RECT_WIDTH (bounds);

  if (height == -1)
    height = BE_RECT_HEIGHT (bounds);

  vw->SetDrawingMode (B_OP_OVER);
  vw->DrawBitmap (bm, BRect (x, y, x + width - 1, y + height - 1),
		  BRect (vx, vy, vx + vwidth - 1, vy + vheight - 1),
		  B_TILE_BITMAP);
  vw->SetDrawingMode (B_OP_COPY);
}

void
BView_DrawBitmapWithEraseOp (void *view, void *bitmap, int x,
			     int y, int width, int height)
{
  BView *vw = get_view (view);
  BBitmap *bm = (BBitmap *) bitmap;
  BBitmap bc (bm->Bounds (), B_RGBA32);
  BRect rect (x, y, x + width - 1, y + height - 1);
  uint32_t *bits;
  size_t stride;
  rgb_color low_color;
  BRect bounds;

  if (bc.InitCheck () != B_OK || bc.ImportBits (bm) != B_OK)
    return;

  bits = (uint32_t *) bc.Bits ();
  stride = bc.BytesPerRow ();

  if (bm->ColorSpace () == B_GRAY1)
    {
      low_color = vw->LowColor ();
      bounds = bc.Bounds ();

      for (int y = 0; y < BE_RECT_HEIGHT (bounds); ++y)
	{
	  for (int x = 0; x < BE_RECT_WIDTH (bounds); ++x)
	    {
	      if (bits[y * (stride / 4) + x] == 0xFF000000)
		bits[y * (stride / 4) + x] = RGB_COLOR_UINT32 (low_color);
	      else
		bits[y * (stride / 4) + x] = 0;
	    }
	}
    }

  vw->SetDrawingMode ((bm->ColorSpace ()
		       == B_GRAY1)
		      ? B_OP_OVER : B_OP_ERASE);
  vw->DrawBitmap (&bc, rect);
  vw->SetDrawingMode (B_OP_COPY);
}

void
BView_DrawMask (void *src, void *view,
		int x, int y, int width, int height,
		int vx, int vy, int vwidth, int vheight,
		uint32_t color)
{
  BBitmap *source = (BBitmap *) src;
  BBitmap bm (source->Bounds (), B_RGBA32);
  BRect bounds = bm.Bounds ();

  if (bm.InitCheck () != B_OK)
    return;
  for (int y = 0; y < BE_RECT_HEIGHT (bounds); ++y)
    {
      for (int x = 0; x < BE_RECT_WIDTH (bounds); ++x)
	{
	  int bit = haiku_get_pixel ((void *) source, x, y);

	  if (!bit)
	    haiku_put_pixel ((void *) &bm, x, y, ((uint32_t) 255 << 24) | color);
	  else
	    haiku_put_pixel ((void *) &bm, x, y, 0);
	}
    }
  BView *vw = get_view (view);
  vw->SetDrawingMode (B_OP_OVER);
  vw->DrawBitmap (&bm, BRect (x, y, x + width - 1, y + height - 1),
		  BRect (vx, vy, vx + vwidth - 1, vy + vheight - 1));
  vw->SetDrawingMode (B_OP_COPY);
}

static BBitmap *
rotate_bitmap_270 (BBitmap *bmp)
{
  BRect r = bmp->Bounds ();
  BBitmap *bm = new BBitmap (BRect (r.top, r.left, r.bottom, r.right),
			     bmp->ColorSpace (), true);
  if (bm->InitCheck () != B_OK)
    gui_abort ("Failed to init bitmap for rotate");
  int w = BE_RECT_WIDTH (r);
  int h = BE_RECT_HEIGHT (r);

  for (int y = 0; y < h; ++y)
    for (int x = 0; x < w; ++x)
      haiku_put_pixel ((void *) bm, y, w - x - 1,
		       haiku_get_pixel ((void *) bmp, x, y));

  return bm;
}

static BBitmap *
rotate_bitmap_90 (BBitmap *bmp)
{
  BRect r = bmp->Bounds ();
  BBitmap *bm = new BBitmap (BRect (r.top, r.left, r.bottom, r.right),
			     bmp->ColorSpace (), true);
  if (bm->InitCheck () != B_OK)
    gui_abort ("Failed to init bitmap for rotate");
  int w = BE_RECT_WIDTH (r);
  int h = BE_RECT_HEIGHT (r);

  for (int y = 0; y < h; ++y)
    for (int x = 0; x < w; ++x)
      haiku_put_pixel ((void *) bm, h - y - 1, x,
		       haiku_get_pixel ((void *) bmp, x, y));

  return bm;
}

void *
BBitmap_transform_bitmap (void *bitmap, void *mask, uint32_t m_color,
			  double rot, int desw, int desh)
{
  BBitmap *bm = (BBitmap *) bitmap;
  BBitmap *mk = (BBitmap *) mask;
  int copied_p = 0;

  if (rot == 90)
    {
      copied_p = 1;
      bm = rotate_bitmap_90 (bm);
      if (mk)
	mk = rotate_bitmap_90 (mk);
    }

  if (rot == 270)
    {
      copied_p = 1;
      bm = rotate_bitmap_270 (bm);
      if (mk)
	mk = rotate_bitmap_270 (mk);
    }

  BRect n = BRect (0, 0, desw - 1, desh - 1);
  BView vw (n, NULL, B_FOLLOW_NONE, 0);
  BBitmap *dst = new BBitmap (n, bm->ColorSpace (), true);
  if (dst->InitCheck () != B_OK)
    if (bm->InitCheck () != B_OK)
      gui_abort ("Failed to init bitmap for scale");
  dst->AddChild (&vw);

  if (!vw.LockLooper ())
    gui_abort ("Failed to lock offscreen view for scale");

  if (rot != 90 && rot != 270)
    {
      BAffineTransform tr;
      tr.RotateBy (BPoint (desw / 2, desh / 2), rot * M_PI / 180.0);
      vw.SetTransform (tr);
    }

  vw.MovePenTo (0, 0);
  vw.DrawBitmap (bm, n);
  if (mk)
    {
      BRect k = mk->Bounds ();
      BView_DrawMask ((void *) mk, (void *) &vw,
		      0, 0, BE_RECT_WIDTH (k),
		      BE_RECT_HEIGHT (k),
		      0, 0, desw, desh, m_color);
    }
  vw.Sync ();
  vw.RemoveSelf ();

  if (copied_p)
    delete bm;
  if (copied_p && mk)
    delete mk;
  return dst;
}

void
BView_FillTriangle (void *view, int x1, int y1,
		    int x2, int y2, int x3, int y3)
{
  BView *vw = get_view (view);
  vw->FillTriangle (BPoint (x1, y1), BPoint (x2, y2),
		    BPoint (x3, y3));
}

void
BView_InvertRect (void *view, int x, int y, int width, int height)
{
  BView *vw = get_view (view);

  vw->InvertRect (BRect (x, y, x + width - 1, y + height - 1));
}
