/* Communication module for Android terminals.  -*- c-file-style: "GNU" -*-

Copyright (C) 2024-2026 Free Software Foundation, Inc.

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
import android.graphics.BitmapShader;
import android.graphics.Canvas;
import android.graphics.ColorFilter;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.Shader.TileMode;

/* This is a crude facsimile of the BitmapDrawable class implementing
   just enough of its functionality to support displaying stipples in
   EmacsGC.  */

public final class EmacsTileObject
{
  /* Bitmap object set by EmacsGC.  */
  private Bitmap bitmap;

  /* Tiling modes on either axis.  */
  private TileMode xTile, yTile;

  /* Destination rectangle.  */
  private Rect boundsRect;

  /* Paint providing graphics properties for drawBitmap.  */
  private final Paint paint;



  public
  EmacsTileObject (Bitmap stippleBitmap)
  {
    bitmap = stippleBitmap;
    paint  = new Paint ();
  }

  public void
  setBitmap (Bitmap newBitmap)
  {
    bitmap = newBitmap;
  }

  public void
  setBounds (Rect bounds)
  {
    boundsRect = bounds;
  }

  public void
  setTileModeXY (TileMode newXTile, TileMode newYTile)
  {
    xTile = newXTile;
    yTile = newYTile;
  }

  public void
  setColorFilter (ColorFilter filterObject)
  {
    paint.setColorFilter (filterObject);
  }

  public Bitmap
  getBitmap ()
  {
    return bitmap;
  }

  /* Replicate `bitmap' over CANVAS so that boundsRect is covered with
     copies thereof on the X axis, if xTile is REPEAT, and also on the Y
     axis, if yTile is a like value.  */

  public void
  draw (Canvas canvas)
  {
    paint.setShader (new BitmapShader (bitmap, xTile, yTile));
    canvas.drawRect (boundsRect, paint);
  }
};
