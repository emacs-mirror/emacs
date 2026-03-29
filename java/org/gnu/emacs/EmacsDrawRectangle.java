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
import android.graphics.Paint;
import android.graphics.PorterDuff.Mode;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.graphics.RectF;
import android.graphics.Xfermode;

import android.util.Log;

public final class EmacsDrawRectangle
{
  private static final Xfermode srcInAlu;

  static
  {
    srcInAlu = new PorterDuffXfermode (Mode.SRC_IN);
  };

  public static void
  perform (EmacsDrawable drawable, EmacsGC gc,
	   int x, int y, int width, int height)
  {
    Paint maskPaint, paint;
    Canvas maskCanvas;
    Bitmap maskBitmap;
    Rect maskRect, dstRect;
    Canvas canvas;
    Bitmap clipBitmap;

    /* TODO implement stippling for this request.  */
    if (gc.fill_style == EmacsGC.GC_FILL_OPAQUE_STIPPLED
	/* And GC_INVERT also.  */
	|| gc.fill_style == EmacsGC.GC_INVERT)
      return;

    canvas = drawable.lockCanvas (gc);

    if (canvas == null)
      return;

    paint = gc.gcPaint;
    paint.setStyle (Paint.Style.STROKE);

    /* This graphics request, in contrast to X, does not presently
       respect the GC's line style.  */

    if (gc.clip_mask == null)
      /* Use canvas.drawRect with a RectF.  That seems to reliably
	 get PostScript behavior.  */
      canvas.drawRect (new RectF (x + 0.5f, y + 0.5f,
				  x + width + 0.5f,
				  y + height + 0.5f),
		       paint);
    else
      {
	/* Drawing with a clip mask involves calculating the
	   intersection of the clip mask with the dst rect, and
	   extrapolating the corresponding part of the src rect.  */
	clipBitmap = gc.clip_mask.bitmap;
	dstRect = new Rect (x, y, x + width, y + height);
	maskRect = new Rect (gc.clip_x_origin,
			     gc.clip_y_origin,
			     (gc.clip_x_origin
			      + clipBitmap.getWidth ()),
			     (gc.clip_y_origin
			      + clipBitmap.getHeight ()));

	if (!maskRect.setIntersect (dstRect, maskRect))
	  /* There is no intersection between the clip mask and the
	     dest rect.  */
	  return;

	/* Finally, create a temporary bitmap that is the size of
	   maskRect.  */

	maskBitmap
	  = Bitmap.createBitmap (maskRect.width (), maskRect.height (),
				 Bitmap.Config.ARGB_8888);

	/* Draw the mask onto the maskBitmap.  */
	maskCanvas = new Canvas (maskBitmap);
	maskRect.offset (-gc.clip_x_origin,
			 -gc.clip_y_origin);
	maskCanvas.drawBitmap (gc.clip_mask.bitmap,
			       maskRect, new Rect (0, 0,
						   maskRect.width (),
						   maskRect.height ()),
			       paint);
	maskRect.offset (gc.clip_x_origin,
			 gc.clip_y_origin);

	/* Set the transfer mode to SRC_IN to preserve only the parts
	   of the source that overlap with the mask.  */
	maskPaint = new Paint ();
	maskPaint.setXfermode (srcInAlu);
	maskPaint.setStyle (Paint.Style.STROKE);

	/* Draw the source.  */
	maskCanvas.drawRect (maskRect, maskPaint);

	/* Finally, draw the mask bitmap to the destination.  */
	paint.setXfermode (null);
	canvas.drawBitmap (maskBitmap, null, maskRect, paint);

	/* Recycle this unused bitmap.  */
	maskBitmap.recycle ();
      }

    drawable.damageRect (x, y, x + width + 1, y + height + 1);
  }
}
