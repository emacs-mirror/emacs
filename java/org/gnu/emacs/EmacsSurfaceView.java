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

import android.view.View;

import android.os.Build;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Rect;
import android.graphics.Paint;

import java.lang.ref.WeakReference;

/* This originally extended SurfaceView.  However, doing so proved to
   be too slow, and Android's surface view keeps up to three of its
   own back buffers, which use too much memory (up to 96 MB for a
   single frame.) */

public final class EmacsSurfaceView extends View
{
  private static final String TAG = "EmacsSurfaceView";

  /* The complete buffer contents at the time of the last draw.  */
  private Bitmap frontBuffer;

  /* Whether frontBuffer has been updated since the last call to
     `onDraw'.  */
  private boolean bitmapChanged;

  /* Canvas representing the front buffer.  */
  private Canvas bitmapCanvas;

  /* Reference to the last bitmap copied to the front buffer.  */
  private WeakReference<Bitmap> bitmap;

  /* Paint objects used on the main and UI threads, respectively.  */
  private static final Paint bitmapPaint, uiThreadPaint;

  static
  {
    /* Create two different Paint objects; one is used on the main
       thread for buffer swaps, while the other is used from the UI
       thread in `onDraw'.  This is necessary because Paint objects
       are not thread-safe, even if their uses are interlocked.  */

    bitmapPaint = new Paint ();
    uiThreadPaint = new Paint ();
  };

  public
  EmacsSurfaceView (EmacsView view)
  {
    super (view.getContext ());

    this.bitmap = new WeakReference<Bitmap> (null);
  }

  private void
  copyToFrontBuffer (Bitmap bitmap, Rect damageRect)
  {
    EmacsService.checkEmacsThread ();

    if (Build.VERSION.SDK_INT != Build.VERSION_CODES.O
	&& Build.VERSION.SDK_INT != Build.VERSION_CODES.O_MR1
	&& Build.VERSION.SDK_INT != Build.VERSION_CODES.N_MR1
	&& Build.VERSION.SDK_INT != Build.VERSION_CODES.N)
      {
	/* If `drawBitmap' can safely be used while a bitmap is locked
	   by another thread, continue here... */

	if (damageRect != null)
	  bitmapCanvas.drawBitmap (bitmap, damageRect, damageRect,
				   bitmapPaint);
	else
	  bitmapCanvas.drawBitmap (bitmap, 0f, 0f, bitmapPaint);
      }
    else
      {
	/* But if it can not, as on Android 7.0 through 8.1, then use
	   a replacement function.  */

	if (damageRect != null)
	  EmacsNative.blitRect (bitmap, frontBuffer,
				damageRect.left,
				damageRect.top,
				damageRect.right,
				damageRect.bottom);
	else
	  EmacsNative.blitRect (bitmap, frontBuffer, 0, 0,
				bitmap.getWidth (),
				bitmap.getHeight ());
      }

    /* See the large comment inside `onDraw'.  */
    bitmapChanged = true;
  }

  private void
  reconfigureFrontBuffer (Bitmap bitmap)
  {
    /* First, remove the old front buffer.  */

    if (frontBuffer != null)
      {
	frontBuffer.recycle ();
	frontBuffer = null;
	bitmapCanvas = null;
      }

    this.bitmap = new WeakReference<Bitmap> (bitmap);

    /* Next, create the new front buffer if necessary.  */

    if (bitmap != null && frontBuffer == null)
      {
	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
	  frontBuffer = Bitmap.createBitmap (bitmap.getWidth (),
					     bitmap.getHeight (),
					     Bitmap.Config.ARGB_8888,
					     false);
	else
	  frontBuffer = Bitmap.createBitmap (bitmap.getWidth (),
					     bitmap.getHeight (),
					     Bitmap.Config.ARGB_8888);

	bitmapCanvas = new Canvas (frontBuffer);

	/* And copy over the bitmap contents.  */
	copyToFrontBuffer (bitmap, null);
      }
    else if (bitmap != null)
      /* Just copy over the bitmap contents.  */
      copyToFrontBuffer (bitmap, null);
  }

  public synchronized void
  setBitmap (Bitmap bitmap, Rect damageRect)
  {
    if (bitmap != this.bitmap.get ())
      reconfigureFrontBuffer (bitmap);
    else if (bitmap != null)
      copyToFrontBuffer (bitmap, damageRect);

    if (bitmap != null)
      {
	/* In newer versions of Android, the invalid rectangle is
	   supposedly internally calculated by the system.  How that
	   is done is unknown, but calling `invalidateRect' is now
	   deprecated.

	   Fortunately, nobody has deprecated the version of
	   `postInvalidate' that accepts a dirty rectangle.  */

	if (damageRect != null)
	  postInvalidate (damageRect.left, damageRect.top,
			  damageRect.right, damageRect.bottom);
	else
	  postInvalidate ();
      }
  }

  @Override
  public synchronized void
  onDraw (Canvas canvas)
  {
    /* Paint the view's bitmap; the bitmap might be recycled right
       now.  */

    if (frontBuffer != null)
      {
	/* The first time the bitmap is drawn after a buffer swap,
	   mark its contents as having changed.  This increments the
	   ``generation ID'' used by Android to avoid uploading buffer
	   textures for unchanged bitmaps.

	   When a buffer swap takes place, the bitmap is initially
	   updated from the Emacs thread, resulting in the generation
	   ID being increased.  If the render thread is texturizing
	   the bitmap while the swap takes place, it might record the
	   generation ID after the update for a texture containing the
	   contents of the bitmap prior to the swap, leaving the
	   texture tied to the bitmap partially updated.

	   Android never calls `onDraw' if the render thread is still
	   processing the bitmap.  Update the generation ID here to
	   ensure that a new texture will be uploaded if the bitmap
	   has changed.

	   Uploading the bitmap contents to the GPU uses an excessive
	   amount of memory, as the entire bitmap is placed into the
	   graphics command queue, but this memory is actually shared
	   among all other applications and reclaimed by the system
	   when necessary.  */

	if (bitmapChanged)
	  {
	    EmacsNative.notifyPixelsChanged (frontBuffer);
	    bitmapChanged = false;
	  }

	canvas.drawBitmap (frontBuffer, 0f, 0f, uiThreadPaint);
      }
  }
};
