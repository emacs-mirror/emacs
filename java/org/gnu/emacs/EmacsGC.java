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

import android.graphics.Rect;
import android.graphics.Paint;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.ColorFilter;
import android.graphics.PorterDuff.Mode;
import android.graphics.PorterDuffColorFilter;
import android.graphics.Shader.TileMode;

import android.os.Build;

/* X like graphics context structures.  Keep the enums in synch with
   androidgui.h! */

public final class EmacsGC extends EmacsHandleObject
{
  public static final int GC_COPY    = 0;
  public static final int GC_INVERT  = 1;

  public static final int GC_FILL_SOLID			= 0;
  public static final int GC_FILL_OPAQUE_STIPPLED	= 1;

  public static final int GC_LINE_SOLID			= 0;
  public static final int GC_LINE_ON_OFF_DASH		= 1;

  public int function, fill_style;
  public int foreground, background;
  public int clip_x_origin, clip_y_origin;
  public int ts_origin_x, ts_origin_y;
  public int line_style, line_width;
  public int dashes[], dash_offset;
  public Rect clip_rects[], real_clip_rects[];
  public EmacsPixmap clip_mask, stipple;
  public Paint gcPaint;

  /* Drawable object for rendering the stipple bitmap.  */
  public EmacsTileObject tileObject;

  /* ID incremented every time the clipping rectangles of any GC
     changes.  */
  private static long clip_serial;

  /* The value of clipRectID after the last time this GCs clip
     rectangles changed.  0 if there are no clip rectangles.  */
  public long clipRectID;

  /* The following fields are only set on immutable GCs.  */

  public
  EmacsGC ()
  {
    /* For historical reasons the C code has an extra layer of
       indirection above this GC handle.  struct android_gc is the GC
       used by Emacs code, while android_gcontext is the type of the
       handle.  */
    super ();

    fill_style = GC_FILL_SOLID;
    function = GC_COPY;
    foreground = 0;
    background = 0xffffff;
    gcPaint = new Paint ();

    /* Android S and above enable anti-aliasing unless explicitly told
       otherwise.  */
    gcPaint.setAntiAlias (false);
  }

  /* Mark this GC as dirty.  Apply parameters to the paint and
     recompute real_clip_rects.  */

  public void
  markDirty (boolean clipRectsChanged)
  {
    int i;
    Bitmap stippleBitmap;

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

    /* A line_width of 0 is equivalent to that of 1.  */
    gcPaint.setStrokeWidth (line_width < 1 ? 1 : line_width);
    gcPaint.setColor (foreground | 0xff000000);

    /* Update the stipple object with the new stipple bitmap, or delete
       it if the stipple has been cleared on systems too old to support
       modifying such objects.  */

    if (stipple != null)
      {
	stippleBitmap = stipple.getBitmap ();

	/* Allocate a new tile object if none is already present or it
	   cannot be reconfigured.  */
	if (tileObject == null)
	  {
	    tileObject = new EmacsTileObject (stippleBitmap);
	    tileObject.setTileModeXY (TileMode.REPEAT, TileMode.REPEAT);
	  }
	else
	  /* Otherwise, update the existing tile object with the new
	     bitmap.  */
	  tileObject.setBitmap (stippleBitmap);
      }
    else if (tileObject != null)
      tileObject.setBitmap (null);
  }

  /* Prepare the tile object to draw a stippled image onto a section of
     a drawable defined by RECT.  It is an error to call this function
     unless the `stipple' field of the GContext is set.  */

  private void
  prepareStipple (Rect rect)
  {
    int sx, sy; /* Stipple origin.  */
    int bw, bh; /* Stipple size.  */
    Bitmap bitmap;
    Rect boundsRect;

    /* Retrieve the dimensions of the stipple bitmap, which doubles as
       the unit of advance for this stipple.  */
    bitmap = tileObject.getBitmap ();
    bw     = bitmap.getWidth ();
    bh     = bitmap.getHeight ();

    /* Align the lower left corner of the bounds rectangle to the
       initial position of the stipple.  */
    sx = (rect.left % bw) * -1 + (-ts_origin_x % bw) * -1;
    sy = (rect.top  % bh) * -1 + (-ts_origin_y % bh) * -1;
    boundsRect = new Rect (rect.left + sx, rect.top + sy,
			   rect.right, rect.bottom);
    tileObject.setBounds (boundsRect);
  }

  /* Fill the rectangle BOUNDS in the provided CANVAS with the stipple
     pattern defined for this GContext, in the foreground color where
     the pattern is on, and in the background color where off.  */

  protected void
  blitOpaqueStipple (Canvas canvas, Rect rect)
  {
    ColorFilter filter;

    prepareStipple (rect);
    filter = new PorterDuffColorFilter (foreground | 0xff000000,
					Mode.SRC_IN);
    tileObject.setColorFilter (filter);

    canvas.save ();
    canvas.clipRect (rect);

    tileObject.draw (canvas);
    filter = new PorterDuffColorFilter (background | 0xff000000,
					Mode.SRC_OUT);
    tileObject.setColorFilter (filter);
    tileObject.draw (canvas);
    canvas.restore ();
  }
};
