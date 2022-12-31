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

import java.lang.IllegalArgumentException;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Rect;

/* Drawable backed by bitmap.  */

public class EmacsPixmap extends EmacsHandleObject
  implements EmacsDrawable
{
  /* The depth of the bitmap.  This is not actually used, just defined
     in order to be consistent with X.  */
  public int depth, width, height;

  /* The bitmap itself.  */
  public Bitmap bitmap;

  /* The canvas used to draw to BITMAP.  */
  public Canvas canvas;

  public
  EmacsPixmap (short handle, int colors[], int width,
	       int height, int depth)
  {
    super (handle);

    if (depth != 1 && depth != 24)
      throw new IllegalArgumentException ("Invalid depth specified"
					  + " for pixmap: " + depth);

    switch (depth)
      {
      case 1:
	bitmap = Bitmap.createBitmap (colors, width, height,
				      Bitmap.Config.ALPHA_8);
	break;

      case 24:
	bitmap = Bitmap.createBitmap (colors, width, height,
				      Bitmap.Config.ARGB_8888);
	bitmap.setHasAlpha (false);
	break;
      }

    this.width = width;
    this.height = height;
    this.depth = depth;
  }

  @Override
  public Canvas
  lockCanvas ()
  {
    if (canvas == null)
      canvas = new Canvas (bitmap);

    return canvas;
  }

  @Override
  public void
  unlockCanvas ()
  {

  }

  @Override
  public void
  damageRect (Rect damageRect)
  {

  }

  @Override
  public Bitmap
  getBitmap ()
  {
    return bitmap;
  }
};
