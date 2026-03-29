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

import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.Rect;
import android.graphics.RectF;

import android.os.Build;

public final class EmacsFillPolygon
{
  @SuppressWarnings ("deprecation") /* computeBounds (IZ) */
  public static void
  perform (EmacsDrawable drawable, EmacsGC gc, Point points[])
  {
    Canvas canvas;
    Path path;
    Paint paint;
    Rect rect;
    RectF rectF;
    int i;

    canvas = drawable.lockCanvas (gc);

    if (canvas == null)
      return;

    paint = gc.gcPaint;

    /* Build the path from the given array of points.  */
    path = new Path ();

    if (points.length >= 1)
      {
	path.moveTo (points[0].x, points[0].y);

	for (i = 1; i < points.length; ++i)
	  path.lineTo (points[i].x, points[i].y);

	path.close ();
      }

    /* Compute the damage rectangle.  */
    rectF = new RectF (0, 0, 0, 0);

    /* computeBounds (IZ) is deprecated but the incompetence of
       Android's release management has caused its replacement to be
       omitted from published header files.  */

    /* if (Build.VERSION.SDK_INT < Build.VERSION_CODES.VANILLA_ICE_CREAM) */
      path.computeBounds (rectF, true);
    /* else
       path.computeBounds (rectF); */

    rect = new Rect ((int) Math.floor (rectF.left),
		     (int) Math.floor (rectF.top),
		     (int) Math.ceil (rectF.right),
		     (int) Math.ceil (rectF.bottom));

    paint.setStyle (Paint.Style.FILL);

    if (gc.clip_mask == null)
      canvas.drawPath (path, paint);

    drawable.damageRect (rect);

    /* FillPolygon with clip mask not implemented; it is not used by
       Emacs.  */
  }
}
