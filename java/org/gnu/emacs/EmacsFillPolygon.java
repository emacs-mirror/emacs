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
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.PorterDuff.Mode;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.graphics.RectF;
import android.graphics.Xfermode;

public class EmacsFillPolygon implements EmacsPaintReq
{
  private EmacsDrawable drawable;
  private EmacsGC immutableGC;
  private Path path;

  private static Xfermode xorAlu, srcInAlu;

  static
  {
    xorAlu = new PorterDuffXfermode (Mode.XOR);
    srcInAlu = new PorterDuffXfermode (Mode.SRC_IN);
  };

  public
  EmacsFillPolygon (EmacsDrawable drawable, Point points[],
		    EmacsGC immutableGC)
  {
    int i;

    this.drawable = drawable;
    this.immutableGC = immutableGC;

    /* Build the path from the given array of points.  */
    path = new Path ();

    if (points.length >= 1)
      {
	path.moveTo (points[0].x, points[0].y);

	for (i = 1; i < points.length; ++i)
	  path.lineTo (points[i].x, points[i].y);

	path.close ();
      }
  }

  @Override
  public Rect
  getRect ()
  {
    RectF rect;

    rect = new RectF (0, 0, 0, 0);
    path.computeBounds (rect, true);

    return new Rect ((int) Math.floor (rect.left),
		     (int) Math.floor (rect.top),
		     (int) Math.ceil (rect.right),
		     (int) Math.ceil (rect.bottom));
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
    Rect rect;

    /* TODO implement stippling.  */
    if (immutableGC.fill_style == EmacsGC.GC_FILL_OPAQUE_STIPPLED)
      return;

    alu = immutableGC.function;
    rect = getRect ();

    if (alu == EmacsGC.GC_COPY)
      paint.setXfermode (null);
    else
      paint.setXfermode (xorAlu);

    paint.setStyle (Paint.Style.FILL);

    if (immutableGC.clip_mask == null)
      {
	paint.setColor (immutableGC.foreground | 0xff000000);
	canvas.drawPath (path, paint);
      }
    else
      {
	maskPaint = new Paint ();
	maskBitmap = immutableGC.clip_mask.bitmap;
	maskBitmap = maskBitmap.copy (Bitmap.Config.ARGB_8888,
				      true);

	if (maskBitmap == null)
	  return;

	maskPaint.setXfermode (srcInAlu);
	maskPaint.setColor (immutableGC.foreground | 0xff000000);
	maskCanvas = new Canvas (maskBitmap);
	path.offset (-rect.left, -rect.top, null);
	maskCanvas.drawPath (path, maskPaint);
	canvas.drawBitmap (maskBitmap, new Rect (0, 0, rect.width (),
						 rect.height ()),
			   rect, paint);
      }
  }
}
