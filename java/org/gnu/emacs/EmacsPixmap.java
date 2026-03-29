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

import java.lang.IllegalArgumentException;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Rect;

import android.os.Build;

/* Drawable backed by bitmap.  */

public final class EmacsPixmap extends EmacsHandleObject
  implements EmacsDrawable
{
  /* The depth of the bitmap.  This is not actually used, just defined
     in order to be consistent with X.  */
  public final int depth, width, height;

  /* The bitmap itself.  */
  public Bitmap bitmap;

  /* The canvas used to draw to BITMAP.  */
  public Canvas canvas;

  /* Whether or not GC should be explicitly triggered upon
     release.  */
  private final boolean needCollect;

  /* ID used to determine whether or not the GC clip rects
     changed.  */
  private long gcClipRectID;

  public
  EmacsPixmap (int width, int height, int depth)
  {
    super ();

    if (depth != 1 && depth != 24)
      throw new IllegalArgumentException ("Invalid depth specified"
					  + " for pixmap: " + depth);

    switch (depth)
      {
      case 1:
	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
	  bitmap = Bitmap.createBitmap (width, height,
					Bitmap.Config.ALPHA_8,
					false);
	else
	  bitmap = Bitmap.createBitmap (width, height,
					Bitmap.Config.ALPHA_8);
	break;

      case 24:

	/* Emacs doesn't just use the first kind of `createBitmap'
	   because the latter allows specifying that the pixmap is
	   always opaque, which really increases efficiency.  */
	if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O)
	  bitmap = Bitmap.createBitmap (width, height,
					Bitmap.Config.ARGB_8888);
	else
	  bitmap = Bitmap.createBitmap (width, height,
					Bitmap.Config.ARGB_8888,
					false);
	break;
      }

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB_MR1)
      /* On these old versions of Android, Bitmap.recycle frees bitmap
	 contents immediately.  */
      needCollect = false;
    else if (Build.VERSION.SDK_INT < Build.VERSION_CODES.KITKAT)
      needCollect = (bitmap.getByteCount ()
		     >= 1024 * 512);
    else
      needCollect = (bitmap.getAllocationByteCount ()
		     >= 1024 * 512);

    bitmap.eraseColor (0xff000000);

    this.width = width;
    this.height = height;
    this.depth = depth;
  }

  @Override
  public Canvas
  lockCanvas (EmacsGC gc)
  {
    int i;

    if (canvas == null)
      {
	canvas = new Canvas (bitmap);
	canvas.save ();
      }

    /* Now see if clipping has to be redone.  */
    if (gc.clipRectID == gcClipRectID)
      return canvas;

    /* It does have to be redone.  Reapply gc.real_clip_rects.  */
    canvas.restore ();
    canvas.save ();

    if (gc.real_clip_rects != null)
      {
	for (i = 0; i < gc.real_clip_rects.length; ++i)
	  canvas.clipRect (gc.real_clip_rects[i]);
      }

    /* Save the clip rect ID again.  */
    gcClipRectID = gc.clipRectID;
    return canvas;
  }

  @Override
  public void
  damageRect (Rect damageRect)
  {

  }

  @Override
  public void
  damageRect (int left, int top, int right, int bottom)
  {

  }

  @Override
  public Bitmap
  getBitmap ()
  {
    return bitmap;
  }

  @Override
  public void
  destroyHandle ()
  {
    bitmap.recycle ();
    bitmap = null;

    /* Collect the bitmap storage if the bitmap is big.  */
    if (needCollect)
      Runtime.getRuntime ().gc ();
  }
};
