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

public class EmacsDrawLine
{
  public static void
  perform (EmacsDrawable drawable, EmacsGC gc,
	   int x, int y, int x2, int y2)
  {
    Rect rect;
    Canvas canvas;
    Paint paint;
    int i;

    /* TODO implement stippling.  */
    if (gc.fill_style == EmacsGC.GC_FILL_OPAQUE_STIPPLED)
      return;

    paint = gc.gcPaint;
    rect = new Rect (Math.min (x, x2 + 1),
		     Math.min (y, y2 + 1),
		     Math.max (x2 + 1, x),
		     Math.max (y2 + 1, y));
    canvas = drawable.lockCanvas ();

    if (canvas == null)
      return;

    canvas.save ();

    if (gc.real_clip_rects != null)
      {
	for (i = 0; i < gc.real_clip_rects.length; ++i)
	  canvas.clipRect (gc.real_clip_rects[i]);
      }

    paint.setStyle (Paint.Style.STROKE);

    if (gc.clip_mask == null)
      canvas.drawLine ((float) x, (float) y,
		       (float) x2, (float) y2,
		       paint);

    /* DrawLine with clip mask not implemented; it is not used by
       Emacs.  */
    canvas.restore ();
    drawable.damageRect (rect);
  }
}
