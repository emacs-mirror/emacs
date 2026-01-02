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
import android.graphics.Rect;

public final class EmacsDrawLine
{
  /* Return the normalized slope and magnitude of a line whose extrema
     are DX and DY removed, on the X and Y axes respectively, from its
     origin point.  */

  private static float[]
  measureLine (float dx, float dy)
  {
    float hypot;

    if (dx == 0f && dy == 0f)
      return new float[] { 0f, 0f, 0f, };

    if (dx == 0f)
      return new float[] { 0f, dy > 0f ? 1f : -1f, Math.abs (dy), };
    else if (dy == 0f)
      return new float[] { dx > 0f ? 1f : -1f, 0f, Math.abs (dx), };
    else
      {
	hypot = (float) Math.hypot (dx, dy);
	return new float[] { dx / hypot, dy / hypot, hypot, };
      }
  }

  private static void
  polyDashPattern (EmacsGC gc, Canvas canvas, Paint paint, float x0,
		   float y0, float x1, float y1)
  {
    int patternTotal, i, offset;
    float dx, dy, mag, dash_mag, rem, lx1, ly1;
    float[] measured;
    boolean which;

    /* Compute the total length of this pattern.  */
    patternTotal = 0;
    for (i = 0; i < gc.dashes.length; ++i)
      patternTotal += gc.dashes[i];
    if ((gc.dashes.length & 1) != 0)
      patternTotal += patternTotal;

    /* Subtract as much of the offset as does not contribute to the
       phase at the first pixel of the line.  */
    offset = gc.dash_offset % patternTotal;

    /* Set I to the first dash that ought to be drawn and WHICH to its
       phase.  */
    i = 0;
    which = true;
    while (offset >= gc.dashes[i])
      {
	offset -= gc.dashes[i++];
	if (i >= gc.dashes.length)
	  i = 0;
	which = !which;
      }

    /* Compute the length of the first visible segment.  */
    dash_mag = gc.dashes[i] - offset;

    /* Compute the slope of the line.  */
    dx = x1 - x0;
    dy = y1 - y0;
    measured = measureLine (dx, dy);
    dx = measured[0];
    dy = measured[1];
    rem = mag = measured[2];
    lx1 = x0;
    ly1 = y0;

    while (rem > 0f)
      {
	dash_mag = Math.min (dash_mag, rem);
	rem -= dash_mag;

	/* End of this segment.  */
	x1 = (mag - rem) * dx + x0;
	y1 = (mag - rem) * dy + y0;

	if (which)
	  canvas.drawLine (lx1, ly1, x1, y1, paint);
	which = !which;

	/* Start of the next segment.  */
	lx1 = x1;
	ly1 = y1;
	i++;
	if (i >= gc.dashes.length)
	  i = 0;
	dash_mag = gc.dashes[i];
      }
  }

  public static void
  perform (EmacsDrawable drawable, EmacsGC gc,
	   int x, int y, int x2, int y2)
  {
    Canvas canvas;
    Paint paint;
    int x0, x1, y0, y1;

    /* TODO implement stippling.  */
    if (gc.fill_style == EmacsGC.GC_FILL_OPAQUE_STIPPLED)
      return;

    /* Calculate the leftmost and rightmost points.  */

    x0 = Math.min (x, x2 + 1);
    x1 = Math.max (x, x2 + 1);
    y0 = Math.min (y, y2 + 1);
    y1 = Math.max (y, y2 + 1);

    /* And the clip rectangle.  */

    paint = gc.gcPaint;
    canvas = drawable.lockCanvas (gc);

    if (canvas == null)
      return;

    if (gc.clip_mask == null)
      {
	if (gc.line_style != EmacsGC.GC_LINE_ON_OFF_DASH)
	  canvas.drawLine ((float) x, (float) y, (float) x2, (float) y2,
			   paint);
	else
	  polyDashPattern (gc, canvas, paint, (float) x, (float) y,
			   (float) x2, (float) y2);
      }

    /* DrawLine with clip mask not implemented; it is not used by
       Emacs.  */
    drawable.damageRect (x0, y0, x1, y1);
  }
}
