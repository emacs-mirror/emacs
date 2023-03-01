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

import android.graphics.Rect;
import android.graphics.Paint;

import android.graphics.PorterDuff.Mode;
import android.graphics.PorterDuffXfermode;
import android.graphics.Xfermode;

/* X like graphics context structures.  Keep the enums in synch with
   androidgui.h! */

public final class EmacsGC extends EmacsHandleObject
{
  public static final int GC_COPY = 0;
  public static final int GC_XOR  = 1;

  public static final int GC_FILL_SOLID			= 0;
  public static final int GC_FILL_OPAQUE_STIPPLED	= 1;

  public static final Xfermode xorAlu, srcInAlu;

  public int function, fill_style;
  public int foreground, background;
  public int clip_x_origin, clip_y_origin;
  public int ts_origin_x, ts_origin_y;
  public Rect clip_rects[], real_clip_rects[];
  public EmacsPixmap clip_mask, stipple;
  public Paint gcPaint;

  /* ID incremented every time the clipping rectangles of any GC
     changes.  */
  private static long clip_serial;

  /* The value of clipRectID after the last time this GCs clip
     rectangles changed.  0 if there are no clip rectangles.  */
  public long clipRectID;

  static
  {
    xorAlu = new PorterDuffXfermode (Mode.XOR);
    srcInAlu = new PorterDuffXfermode (Mode.SRC_IN);
  }

  /* The following fields are only set on immutable GCs.  */

  public
  EmacsGC (short handle)
  {
    /* For historical reasons the C code has an extra layer of
       indirection above this GC handle.  struct android_gc is the GC
       used by Emacs code, while android_gcontext is the type of the
       handle.  */
    super (handle);

    fill_style = GC_FILL_SOLID;
    function = GC_COPY;
    foreground = 0;
    background = 0xffffff;
    gcPaint = new Paint ();
  }

  /* Mark this GC as dirty.  Apply parameters to the paint and
     recompute real_clip_rects.  */

  public void
  markDirty (boolean clipRectsChanged)
  {
    int i;

    if (clipRectsChanged)
      {
	if ((ts_origin_x != 0 || ts_origin_y != 0)
	    && clip_rects != null)
	  {
	    real_clip_rects = new Rect[clip_rects.length];

	    for (i = 0; i < clip_rects.length; ++i)
	      {
		real_clip_rects[i] = new Rect (clip_rects[i]);
		real_clip_rects[i].offset (ts_origin_x, ts_origin_y);
	      }
	  }
	else
	  real_clip_rects = clip_rects;

	clipRectID = ++clip_serial;
      }

    gcPaint.setStrokeWidth (1f);
    gcPaint.setColor (foreground | 0xff000000);
    gcPaint.setXfermode (function == GC_XOR
			 ? xorAlu : srcInAlu);
  }

  public void
  resetXfermode ()
  {
    gcPaint.setXfermode (function == GC_XOR
			 ? xorAlu : srcInAlu);
  }
};
