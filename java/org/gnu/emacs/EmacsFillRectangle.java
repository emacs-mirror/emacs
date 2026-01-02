/* Communication module for Android terminals.  -*- c-file-style: "GNU" -*-

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

package org.gnu.emacs;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.ColorFilter;
import android.graphics.ColorMatrixColorFilter;
import android.graphics.Paint;
import android.graphics.Rect;

import android.util.Log;

public final class EmacsFillRectangle
{
  /* Color filter that inverts colors from the source.  */
  private static final ColorFilter invertFilter;

  static
  {
    invertFilter = new ColorMatrixColorFilter (new float[] {
	-1f, 0f, 0f, 0f, 255f,
	0f, -1f, 0f, 0f, 255f,
	0f, 0f, -1f, 0f, 255f,
	0f, 0f, 0f, 1f, 0f,
      });
  };

  public static void
  perform (EmacsDrawable drawable, EmacsGC gc,
	   int x, int y, int width, int height)
  {
    Paint paint;
    Rect rect;
    Canvas canvas;
    Bitmap invertBitmap;

    canvas = drawable.lockCanvas (gc);

    /* Clip masks are not respected or implemented when specified with
       this request.  */
    if (canvas == null || gc.clip_mask != null)
      return;

    rect = new Rect (x, y, x + width, y + height);

    if (gc.function != EmacsGC.GC_INVERT)
      {
	paint = gc.gcPaint;
	paint.setStyle (Paint.Style.FILL);

	if (gc.fill_style != EmacsGC.GC_FILL_OPAQUE_STIPPLED)
	  canvas.drawRect (rect, paint);
	else
	  gc.blitOpaqueStipple (canvas, rect);
      }
    else
      {
	paint = new Paint ();

	/* Simply invert the destination, which is only implemented for
	   this request.  As Android doesn't permit copying a bitmap to
	   itself, a copy of the source must be procured beforehand.  */
	invertBitmap = Bitmap.createBitmap (drawable.getBitmap (),
					    x, y, width, height);
        paint.setColorFilter (invertFilter);
	canvas.drawBitmap (invertBitmap, null, rect, paint);
        paint.setColorFilter (null);
	invertBitmap.recycle ();
      }

    drawable.damageRect (rect);
  }
};
