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

import android.content.res.ColorStateList;

import android.view.ContextMenu;
import android.view.View;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.ViewGroup;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Rect;
import android.graphics.Region;
import android.graphics.Paint;

import android.os.Build;
import android.util.Log;

/* This is an Android view which has a back and front buffer.  When
   swapBuffers is called, the back buffer is swapped to the front
   buffer, and any damage is invalidated.  frontBitmap and backBitmap
   are modified and used both from the UI and the Emacs thread.  As a
   result, there is a lock held during all drawing operations.

   It is also a ViewGroup, as it also lays out children.  */

public class EmacsView extends ViewGroup
{
  public static final String TAG = "EmacsView";

  /* The associated EmacsWindow.  */
  public EmacsWindow window;

  /* The buffer bitmap.  */
  public Bitmap bitmap;

  /* The associated canvases.  */
  public Canvas canvas;

  /* The damage region.  */
  public Region damageRegion;

  /* The paint.  */
  public Paint paint;

  /* The associated surface view.  */
  private EmacsSurfaceView surfaceView;

  /* Whether or not a configure event must be sent for the next layout
     event regardless of what changed.  */
  public boolean mustReportLayout;

  /* If non-null, whether or not bitmaps must be recreated upon the
     next call to getBitmap.  */
  private Rect bitmapDirty;

  /* Whether or not a popup is active.  */
  private boolean popupActive;

  /* The current context menu.  */
  private EmacsContextMenu contextMenu;

  public
  EmacsView (EmacsWindow window)
  {
    super (EmacsService.SERVICE);

    this.window = window;
    this.damageRegion = new Region ();
    this.paint = new Paint ();

    setFocusable (true);
    setFocusableInTouchMode (true);

    /* Create the surface view.  */
    this.surfaceView = new EmacsSurfaceView (this);
    this.surfaceView.setZOrderMediaOverlay (true);
    addView (this.surfaceView);

    /* Not sure exactly what this does but it makes things magically
       work.  Why is something as simple as XRaiseWindow so involved
       on Android? */
    setChildrenDrawingOrderEnabled (true);

    /* Get rid of the foreground and background tint.  */
    setBackgroundTintList (null);
    setForegroundTintList (null);

    /* Get rid of the default focus highlight.  */
    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.O)
      setDefaultFocusHighlightEnabled (false);
  }

  private void
  handleDirtyBitmap ()
  {
    Bitmap oldBitmap;

    /* Save the old bitmap.  */
    oldBitmap = bitmap;

    /* Recreate the front and back buffer bitmaps.  */
    bitmap
      = Bitmap.createBitmap (bitmapDirty.width (),
			     bitmapDirty.height (),
			     Bitmap.Config.ARGB_8888);
    bitmap.eraseColor (0xffffffff);

    /* And canvases.  */
    canvas = new Canvas (bitmap);

    /* Copy over the contents of the old bitmap.  */
    if (oldBitmap != null)
      canvas.drawBitmap (oldBitmap, 0f, 0f, new Paint ());

    bitmapDirty = null;
  }

  public synchronized void
  explicitlyDirtyBitmap (Rect rect)
  {
    if (bitmapDirty == null
	&& (bitmap == null
	    || rect.width () != bitmap.getWidth ()
	    || rect.height () != bitmap.getHeight ()))
      bitmapDirty = rect;
  }

  public synchronized Bitmap
  getBitmap ()
  {
    if (bitmapDirty != null)
      handleDirtyBitmap ();

    return bitmap;
  }

  public synchronized Canvas
  getCanvas ()
  {
    if (bitmapDirty != null)
      handleDirtyBitmap ();

    return canvas;
  }

  @Override
  protected void
  onMeasure (int widthMeasureSpec, int heightMeasureSpec)
  {
    Rect measurements;
    int width, height;

    /* Return the width and height of the window regardless of what
       the parent says.  */
    measurements = window.getGeometry ();

    width = measurements.width ();
    height = measurements.height ();

    /* Now apply any extra requirements in widthMeasureSpec and
       heightMeasureSpec.  */

    if (MeasureSpec.getMode (widthMeasureSpec) == MeasureSpec.EXACTLY)
      width = MeasureSpec.getSize (widthMeasureSpec);
    else if (MeasureSpec.getMode (widthMeasureSpec) == MeasureSpec.AT_MOST
	     && width > MeasureSpec.getSize (widthMeasureSpec))
      width = MeasureSpec.getSize (widthMeasureSpec);

    if (MeasureSpec.getMode (heightMeasureSpec) == MeasureSpec.EXACTLY)
      height = MeasureSpec.getSize (heightMeasureSpec);
    else if (MeasureSpec.getMode (heightMeasureSpec) == MeasureSpec.AT_MOST
	     && height > MeasureSpec.getSize (heightMeasureSpec))
      height = MeasureSpec.getSize (heightMeasureSpec);

    super.setMeasuredDimension (width, height);
  }

  @Override
  protected synchronized void
  onLayout (boolean changed, int left, int top, int right,
	    int bottom)
  {
    int count, i;
    View child;
    Rect windowRect;

    count = getChildCount ();

    if (changed || mustReportLayout)
      {
	mustReportLayout = false;
	window.viewLayout (left, top, right, bottom);
      }

    if (changed
	/* Check that a change has really happened.  */
	&& (bitmapDirty == null
	    || bitmapDirty.width () != right - left
	    || bitmapDirty.height () != bottom - top))
      bitmapDirty = new Rect (left, top, right, bottom);

    for (i = 0; i < count; ++i)
      {
	child = getChildAt (i);

	Log.d (TAG, "onLayout: " + child);

	if (child == surfaceView)
	  /* The child is the surface view, so give it the entire
	     view.  */
	  child.layout (0, 0, right - left, bottom - top);
	else if (child.getVisibility () != GONE)
	  {
	    if (!(child instanceof EmacsView))
	      continue;

	    /* What to do: lay out the view precisely according to its
	       window rect.  */
	    windowRect = ((EmacsView) child).window.getGeometry ();
	    child.layout (windowRect.left, windowRect.top,
			  windowRect.right, windowRect.bottom);
	  }
      }
  }

  public void
  damageRect (Rect damageRect)
  {
    synchronized (damageRegion)
      {
	damageRegion.union (damageRect);
      }
  }

  /* This method is called from both the UI thread and the Emacs
     thread.  */

  public void
  swapBuffers (boolean force)
  {
    Canvas canvas;
    Rect damageRect;
    Bitmap bitmap;

    /* Code must always take damageRegion, and then surfaceChangeLock,
       never the other way around! */

    synchronized (damageRegion)
      {
	if (damageRegion.isEmpty ())
	  return;

	bitmap = getBitmap ();

	/* Emacs must take the following lock to ensure the access to the
	   canvas occurs with the surface created.  Otherwise, Android
	   will throttle calls to lockCanvas.  */

	synchronized (surfaceView.surfaceChangeLock)
	  {
	    damageRect = damageRegion.getBounds ();

	    if (!surfaceView.isCreated ())
	      return;

	    if (bitmap == null)
	      return;

	    /* Lock the canvas with the specified damage.  */
	    canvas = surfaceView.lockCanvas (damageRect);

	    /* Return if locking the canvas failed.  */
	    if (canvas == null)
	      return;

	    /* Copy from the back buffer to the canvas.  If damageRect was
	       made empty, then draw the entire back buffer.  */

	    if (damageRect.isEmpty ())
	      canvas.drawBitmap (bitmap, 0f, 0f, paint);
	    else
	      canvas.drawBitmap (bitmap, damageRect, damageRect, paint);

	    /* Unlock the canvas and clear the damage.  */
	    surfaceView.unlockCanvasAndPost (canvas);
	    damageRegion.setEmpty ();
	  }
      }
  }

  public void
  swapBuffers ()
  {
    swapBuffers (false);
  }

  @Override
  public boolean
  onKeyDown (int keyCode, KeyEvent event)
  {
    window.onKeyDown (keyCode, event);
    return true;
  }

  @Override
  public boolean
  onKeyMultiple (int keyCode, int repeatCount, KeyEvent event)
  {
    window.onKeyDown (keyCode, event);
    return true;
  }

  @Override
  public boolean
  onKeyUp (int keyCode, KeyEvent event)
  {
    window.onKeyUp (keyCode, event);
    return true;
  }

  @Override
  public void
  onFocusChanged (boolean gainFocus, int direction,
		  Rect previouslyFocusedRect)
  {
    window.onFocusChanged (gainFocus);
    super.onFocusChanged (gainFocus, direction,
			  previouslyFocusedRect);
  }

  @Override
  public boolean
  onGenericMotionEvent (MotionEvent motion)
  {
    return window.onSomeKindOfMotionEvent (motion);
  }

  @Override
  public boolean
  onTouchEvent (MotionEvent motion)
  {
    return window.onTouchEvent (motion);
  }

  private void
  moveChildToBack (View child)
  {
    int index;

    index = indexOfChild (child);

    if (index > 0)
      {
	detachViewFromParent (index);

	/* The view at 0 is the surface view.  */
	attachViewToParent (child, 1,
			    child.getLayoutParams());
      }
  }


  /* The following two functions must not be called if the view has no
     parent, or is parented to an activity.  */

  public void
  raise ()
  {
    EmacsView parent;

    parent = (EmacsView) getParent ();

    Log.d (TAG, "raise: parent " + parent);

    if (parent.indexOfChild (this)
	== parent.getChildCount () - 1)
      return;

    parent.bringChildToFront (this);

    /* Yes, all of this is really necessary! */
    parent.requestLayout ();
    parent.invalidate ();
    requestLayout ();
    invalidate ();

    /* The surface view must be destroyed and recreated.  */
    removeView (surfaceView);
    addView (surfaceView, 0);
  }

  public void
  lower ()
  {
    EmacsView parent;

    parent = (EmacsView) getParent ();

    Log.d (TAG, "lower: parent " + parent);

    if (parent.indexOfChild (this) == 1)
      return;

    parent.moveChildToBack (this);

    /* Yes, all of this is really necessary! */
    parent.requestLayout ();
    parent.invalidate ();
    requestLayout ();
    invalidate ();

    /* The surface view must be removed and attached again.  */
    removeView (surfaceView);
    addView (surfaceView, 0);
  }

  @Override
  protected void
  onCreateContextMenu (ContextMenu menu)
  {
    if (contextMenu == null)
      return;

    contextMenu.expandTo (menu);
  }

  public boolean
  popupMenu (EmacsContextMenu menu, int xPosition,
	     int yPosition)
  {
    if (popupActive)
      return false;

    contextMenu = menu;

    /* On API 21 or later, use showContextMenu (float, float).  */
    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.LOLLIPOP)
      return showContextMenu ((float) xPosition, (float) yPosition);
    else
      return showContextMenu ();
  }

  public void
  cancelPopupMenu ()
  {
    if (!popupActive)
      throw new IllegalStateException ("cancelPopupMenu called without"
				       + " popupActive set");

    contextMenu = null;
  }
};
