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

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.PorterDuff.Mode;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.graphics.Xfermode;

public class EmacsCopyArea implements EmacsPaintReq
{
  private int src_x, src_y, dest_x, dest_y, width, height;
  private EmacsDrawable destination, source;
  private EmacsGC immutableGC;
  private static Xfermode xorAlu, srcInAlu;

  static
  {
    xorAlu = new PorterDuffXfermode (Mode.XOR);
    srcInAlu = new PorterDuffXfermode (Mode.SRC_IN);
  };

  public
  EmacsCopyArea (EmacsDrawable destination, EmacsDrawable source,
		 int src_x, int src_y, int width, int height,
		 int dest_x, int dest_y, EmacsGC immutableGC)
  {
    this.destination = destination;
    this.source = source;
    this.src_x = src_x;
    this.src_y = src_y;
    this.width = width;
    this.height = height;
    this.dest_x = dest_x;
    this.dest_y = dest_y;
    this.immutableGC = immutableGC;
  }

  @Override
  public Rect
  getRect ()
  {
    return new Rect (dest_x, dest_y, dest_x + width,
		     dest_y + height);
  }

  @Override
  public EmacsDrawable
  getDrawable ()
  {
    return destination;
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
    Bitmap bitmap;
    Paint maskPaint;
    Canvas maskCanvas;
    Bitmap maskBitmap;
    Rect rect, srcRect;

    /* TODO implement stippling.  */
    if (immutableGC.fill_style == EmacsGC.GC_FILL_OPAQUE_STIPPLED)
      return;

    alu = immutableGC.function;
    rect = getRect ();
    bitmap = source.getBitmap ();

    if (alu == EmacsGC.GC_COPY)
      paint.setXfermode (null);
    else
      paint.setXfermode (xorAlu);

    if (immutableGC.clip_mask == null)
      canvas.drawBitmap (bitmap, new Rect (src_x, src_y,
					   src_x + width,
					   src_y + height),
			 rect, paint);
    else
      {
	maskPaint = new Paint ();
	srcRect = new Rect (0, 0, rect.width (),
			    rect.height ());
	maskBitmap
	  = immutableGC.clip_mask.bitmap.copy (Bitmap.Config.ARGB_8888,
					       true);

	if (maskBitmap == null)
	  return;

	maskPaint.setXfermode (srcInAlu);
	maskCanvas = new Canvas (maskBitmap);
	maskCanvas.drawBitmap (bitmap, new Rect (src_x, src_y,
						 src_x + width,
						 src_y + height),
			       srcRect, maskPaint);
	canvas.drawBitmap (maskBitmap, srcRect, rect, paint);
      }
  }
}
