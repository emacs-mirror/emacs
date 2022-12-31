/* Communication module for Android terminals.  -*- c-file-style: "GNU" -*-

Copyright (C) 2023 Free Software Foundation, Inc.

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

import java.lang.Math;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.PorterDuff.Mode;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.graphics.Xfermode;

public class EmacsDrawLine implements EmacsPaintReq
{
  private int x, y, x2, y2;
  private EmacsDrawable drawable;
  private EmacsGC immutableGC;
  private static Xfermode xorAlu, srcInAlu;

  static
  {
    xorAlu = new PorterDuffXfermode (Mode.XOR);
    srcInAlu = new PorterDuffXfermode (Mode.SRC_IN);
  };

  public
  EmacsDrawLine (EmacsDrawable drawable, int x, int y,
		 int x2, int y2, EmacsGC immutableGC)
  {
    this.drawable = drawable;
    this.x = x;
    this.y = y;
    this.x2 = x2;
    this.y2 = y2;
    this.immutableGC = immutableGC;
  }

  @Override
  public Rect
  getRect ()
  {
    return new Rect (Math.min (x, x2 + 1),
		     Math.min (y, y2 + 1),
		     Math.max (x2 + 1, x),
		     Math.max (y2 + 1, y));
  }

  @Override
  public EmacsDrawable
  getDrawable ()
  {
    return drawable;
  }

  @Override
  public EmacsGC
  getGC ()
  {
    return immutableGC;
  }

  @Override
  public void
  paintTo (Canvas canvas, Paint paint, EmacsGC immutableGC)
  {
    int alu;
    Paint maskPaint;
    Canvas maskCanvas;
    Bitmap maskBitmap;
    Rect rect, srcRect;
    int width, height;

    /* TODO implement stippling.  */
    if (immutableGC.fill_style == EmacsGC.GC_FILL_OPAQUE_STIPPLED)
      return;

    alu = immutableGC.function;
    rect = getRect ();
    width = rect.width ();
    height = rect.height ();

    paint.setStyle (Paint.Style.STROKE);

    if (alu == EmacsGC.GC_COPY)
      paint.setXfermode (null);
    else
      paint.setXfermode (xorAlu);

    if (immutableGC.clip_mask == null)
      {
        paint.setColor (immutableGC.foreground | 0xff000000);
	canvas.drawLine ((float) x, (float) y,
			 (float) x2, (float) y2,
			 paint);
      }
    else
      {
	maskPaint = new Paint ();
	maskBitmap
	  = immutableGC.clip_mask.bitmap.copy (Bitmap.Config.ARGB_8888,
					       true);

	if (maskBitmap == null)
	  return;

	maskPaint.setXfermode (srcInAlu);
	maskPaint.setColor (immutableGC.foreground | 0xff000000);
	maskCanvas = new Canvas (maskBitmap);
	srcRect = new Rect (0, 0, maskBitmap.getWidth (),
			    maskBitmap.getHeight ());
	maskCanvas.drawLine (0.0f, 0.0f, (float) Math.abs (x - x2),
			     (float) Math.abs (y - y2), maskPaint);
	canvas.drawBitmap (maskBitmap, srcRect, rect, paint);
      }

    paint.setXfermode (null);
  }
}
