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

/* X like graphics context structures.  Keep the enums in synch with
   androidgui.h! */

public class EmacsGC extends EmacsHandleObject
{
  public static final int GC_COPY = 0;
  public static final int GC_XOR  = 1;

  public static final int GC_FILL_SOLID			= 0;
  public static final int GC_FILL_OPAQUE_STIPPLED	= 1;

  public int function, fill_style;
  public int foreground, background;
  public int clip_x_origin, clip_y_origin;
  public int ts_origin_x, ts_origin_y;
  public Rect clip_rects[];
  public EmacsPixmap clip_mask, stipple;
  private boolean dirty;
  private EmacsGC immutableGC;

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
    background = 0xffffffff;
  }

  public
  EmacsGC (EmacsGC source)
  {
    super ((short) 0);

    int i;

    function = source.function;
    fill_style = source.fill_style;
    foreground = source.foreground;
    background = source.background;
    clip_x_origin = source.clip_x_origin;
    clip_y_origin = source.clip_y_origin;
    clip_rects = source.clip_rects;
    clip_mask = source.clip_mask;
    stipple = source.stipple;
    ts_origin_x = source.ts_origin_x;
    ts_origin_y = source.ts_origin_y;

    /* Offset all the clip rects by ts_origin_x and ts_origin_y.  */

    if ((ts_origin_x != 0 || ts_origin_y != 0)
	&& clip_rects != null)
      {
	clip_rects = new Rect[clip_rects.length];

	for (i = 0; i < clip_rects.length; ++i)
	  {
	    clip_rects[i] = new Rect (source.clip_rects[i]);
	    clip_rects[i].offset (ts_origin_x,
				  ts_origin_y);
	  }
      }
  }

  /* Mark this GC as dirty.  This means immutableGC will return a new
     copy of this GC the next time it is called.  */

  public void
  markDirty ()
  {
    dirty = true;
  }

  public EmacsGC
  immutableGC ()
  {
    if (immutableGC == null || dirty)
      {
	immutableGC = new EmacsGC (this);
	dirty = false;
      }

    return immutableGC;
  };
};
