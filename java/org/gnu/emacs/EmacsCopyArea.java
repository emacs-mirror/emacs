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

public class EmacsCopyArea
{
  private static Xfermode overAlu;

  static
  {
    overAlu = new PorterDuffXfermode (Mode.SRC_OVER);
  };

  private static void
  insetRectBy (Rect rect, int left, int top, int right,
	       int bottom)
  {
    rect.left += left;
    rect.top += top;
    rect.right -= right;
    rect.bottom -= bottom;
  }

  public static void
  perform (EmacsDrawable source, EmacsGC gc,
	   EmacsDrawable destination,
	   int src_x, int src_y, int width, int height,
	   int dest_x, int dest_y)
  {
    int i;
    Bitmap bitmap;
    Paint maskPaint, paint;
    Canvas maskCanvas, canvas;
    Bitmap srcBitmap, maskBitmap, clipBitmap;
    Rect rect, maskRect, srcRect, dstRect, maskDestRect;
    boolean needFill;

    /* TODO implement stippling.  */
    if (gc.fill_style == EmacsGC.GC_FILL_OPAQUE_STIPPLED)
      return;

    paint = gc.gcPaint;

    canvas = destination.lockCanvas ();

    if (canvas == null)
      return;

    canvas.save ();

    if (gc.real_clip_rects != null)
      {
	for (i = 0; i < gc.real_clip_rects.length; ++i)
	  canvas.clipRect (gc.real_clip_rects[i]);
      }

    /* A copy must be created or drawBitmap could end up overwriting
       itself.  */
    srcBitmap = source.getBitmap ();

    /* If srcBitmap is out of bounds, then adjust the source rectangle
       to be within bounds.  Note that tiling on windows with
       backgrounds is unimplemented.  */

    if (src_x < 0)
      {
	width += src_x;
	dest_x -= src_x;
	src_x = 0;
      }

    if (src_y < 0)
      {
	height += src_y;
	dest_y -= src_y;
	src_y = 0;
      }

    if (src_x + width > srcBitmap.getWidth ())
      width = srcBitmap.getWidth () - src_x;

    if (src_y + height > srcBitmap.getHeight ())
      height = srcBitmap.getHeight () - src_y;

    rect = new Rect (dest_x, dest_y, dest_x + width,
		     dest_y + height);

    if (gc.clip_mask == null)
      {
	bitmap = Bitmap.createBitmap (srcBitmap,
				      src_x, src_y, width,
				      height);
	canvas.drawBitmap (bitmap, null, rect, paint);
	bitmap.recycle ();
      }
    else
      {
	/* Drawing with a clip mask involves calculating the
	   intersection of the clip mask with the dst rect, and
	   extrapolating the corresponding part of the src rect.  */
	clipBitmap = gc.clip_mask.bitmap;
	dstRect = new Rect (dest_x, dest_y,
			    dest_x + width,
			    dest_y + height);
	maskRect = new Rect (gc.clip_x_origin,
			     gc.clip_y_origin,
			     (gc.clip_x_origin
			      + clipBitmap.getWidth ()),
			     (gc.clip_y_origin
			      + clipBitmap.getHeight ()));
	clipBitmap = gc.clip_mask.bitmap;

	if (!maskRect.setIntersect (dstRect, maskRect))
	  /* There is no intersection between the clip mask and the
	     dest rect.  */
	  return;

	/* Now figure out which part of the source corresponds to
	   maskRect and return it relative to srcBitmap.  */
	srcRect = new Rect (src_x, src_y, src_x + width,
			    src_y + height);
	insetRectBy (srcRect, maskRect.left - dstRect.left,
		     maskRect.top - dstRect.top,
		     maskRect.right - dstRect.right,
		     maskRect.bottom - dstRect.bottom);

	/* Finally, create a temporary bitmap that is the size of
	   maskRect.  */

	maskBitmap
	  = Bitmap.createBitmap (maskRect.width (), maskRect.height (),
				 Bitmap.Config.ARGB_8888);

	/* Draw the mask onto the maskBitmap.  */
	maskCanvas = new Canvas (maskBitmap);
	maskPaint = new Paint ();
	maskRect.offset (-gc.clip_x_origin,
			 -gc.clip_y_origin);
	maskCanvas.drawBitmap (gc.clip_mask.bitmap,
			       maskRect,
			       new Rect (0, 0,
					 maskRect.width (),
					 maskRect.height ()),
			       maskPaint);
	maskRect.offset (gc.clip_x_origin,
			 gc.clip_y_origin);

	/* Set the transfer mode to SRC_IN to preserve only the parts
	   of the source that overlap with the mask.  */
	maskPaint.setXfermode (EmacsGC.srcInAlu);

	/* Draw the source.  */
	maskDestRect = new Rect (0, 0, srcRect.width (),
				 srcRect.height ());
	maskCanvas.drawBitmap (srcBitmap, srcRect, maskDestRect,
			       maskPaint);

	/* Finally, draw the mask bitmap to the destination.  */
	paint.setXfermode (overAlu);
	canvas.drawBitmap (maskBitmap, null, maskRect, paint);
	gc.resetXfermode ();

	/* Recycle this unused bitmap.  */
	maskBitmap.recycle ();
      }

    canvas.restore ();
    destination.damageRect (rect);
  }
}
