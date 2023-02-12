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

import android.content.Context;
import android.content.res.ColorStateList;

import android.text.InputType;

import android.view.ContextMenu;
import android.view.View;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.ViewGroup;

import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputMethodManager;

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

  /* Whether or not bitmaps must be recreated upon the next call to
     getBitmap.  */
  private boolean bitmapDirty;

  /* Whether or not a popup is active.  */
  private boolean popupActive;

  /* The current context menu.  */
  private EmacsContextMenu contextMenu;

  /* The last measured width and height.  */
  private int measuredWidth, measuredHeight;

  /* The serial of the last clip rectangle change.  */
  private long lastClipSerial;

  /* The InputMethodManager for this view's context.  */
  public InputMethodManager imManager;

  /* Whether or not this view is attached to a window.  */
  public boolean isAttachedToWindow;

  /* Whether or not this view should have the on screen keyboard
     displayed whenever possible.  */
  public boolean isCurrentlyTextEditor;

  public
  EmacsView (EmacsWindow window)
  {
    super (EmacsService.SERVICE);

    Object tem;

    this.window = window;
    this.damageRegion = new Region ();
    this.paint = new Paint ();

    setFocusable (true);
    setFocusableInTouchMode (true);

    /* Create the surface view.  */
    this.surfaceView = new EmacsSurfaceView (this);
    addView (this.surfaceView);

    /* Get rid of the default focus highlight.  */
    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.O)
      setDefaultFocusHighlightEnabled (false);

    /* Obtain the input method manager.  */
    tem = getContext ().getSystemService (Context.INPUT_METHOD_SERVICE);
    imManager = (InputMethodManager) tem;
  }

  private void
  handleDirtyBitmap ()
  {
    Bitmap oldBitmap;

    if (measuredWidth == 0 || measuredHeight == 0)
      return;

    if (!isAttachedToWindow)
      return;

    /* If bitmap is the same width and height as the measured width
       and height, there is no need to do anything.  Avoid allocating
       the extra bitmap.  */
    if (bitmap != null
	&& (bitmap.getWidth () == measuredWidth
	    && bitmap.getHeight () == measuredHeight))
      {
	bitmapDirty = false;
	return;
      }

    /* Save the old bitmap.  */
    oldBitmap = bitmap;

    /* Recreate the front and back buffer bitmaps.  */
    bitmap
      = Bitmap.createBitmap (measuredWidth,
			     measuredHeight,
			     Bitmap.Config.ARGB_8888);
    bitmap.eraseColor (0xffffffff);

    /* And canvases.  */
    canvas = new Canvas (bitmap);
    canvas.save ();

    /* Since the clip rectangles have been cleared, clear the clip
       rectangle ID.  */
    lastClipSerial = 0;

    /* Copy over the contents of the old bitmap.  */
    if (oldBitmap != null)
      canvas.drawBitmap (oldBitmap, 0f, 0f, new Paint ());

    bitmapDirty = false;

    /* Explicitly free the old bitmap's memory.  */

    if (oldBitmap != null)
      {
	oldBitmap.recycle ();

	/* Make sure to set the view's bitmap to the new bitmap, or
	   ugly flicker can result.  */
	surfaceView.setBitmap (bitmap, null);
      }

    /* Some Android versions still don't free the bitmap until the
       next GC.  */
    Runtime.getRuntime ().gc ();
  }

  public synchronized void
  explicitlyDirtyBitmap ()
  {
    bitmapDirty = true;
  }

  public synchronized Bitmap
  getBitmap ()
  {
    if (bitmapDirty || bitmap == null)
      handleDirtyBitmap ();

    return bitmap;
  }

  public synchronized Canvas
  getCanvas (EmacsGC gc)
  {
    int i;

    if (bitmapDirty || bitmap == null)
      handleDirtyBitmap ();

    if (canvas == null)
      return null;

    /* Update clip rectangles if necessary.  */
    if (gc.clipRectID != lastClipSerial)
      {
	canvas.restore ();
	canvas.save ();

	if (gc.real_clip_rects != null)
	  {
	    for (i = 0; i < gc.real_clip_rects.length; ++i)
	      canvas.clipRect (gc.real_clip_rects[i]);
	  }

	lastClipSerial = gc.clipRectID;
      }

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

  /* Note that the monitor lock for the window must never be held from
     within the lock for the view, because the window also locks the
     other way around.  */

  @Override
  protected void
  onLayout (boolean changed, int left, int top, int right,
	    int bottom)
  {
    int count, i;
    View child;
    Rect windowRect;
    int wantedWidth, wantedHeight;

    count = getChildCount ();

    measuredWidth = right - left;
    measuredHeight = bottom - top;

    /* Dirty the back buffer.  */

    if (changed)
      explicitlyDirtyBitmap ();

    for (i = 0; i < count; ++i)
      {
	child = getChildAt (i);

	Log.d (TAG, "onLayout: " + child);

	if (child == surfaceView)
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

    /* Now report the layout change to the window.  */

    if (changed || mustReportLayout)
      {
	mustReportLayout = false;
	window.viewLayout (left, top, right, bottom);
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
  swapBuffers ()
  {
    Canvas canvas;
    Rect damageRect;
    Bitmap bitmap;

    damageRect = null;

    synchronized (damageRegion)
      {
	if (damageRegion.isEmpty ())
	  return;

	bitmap = getBitmap ();

	/* Transfer the bitmap to the surface view, then invalidate
	   it.  */
        surfaceView.setBitmap (bitmap, damageRect);
      }
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
    popupActive = true;

    /* Use showContextMenu (float, float) on N to get actual popup
       behavior.  */
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N)
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
    popupActive = false;
  }

  @Override
  public synchronized void
  onDetachedFromWindow ()
  {
    isAttachedToWindow = false;

    synchronized (this)
      {
	/* Recycle the bitmap and call GC.  */
	bitmap.recycle ();
	bitmap = null;
	canvas = null;
	surfaceView.setBitmap (null, null);

	/* Collect the bitmap storage; it could be large.  */
	Runtime.getRuntime ().gc ();
      }

    super.onDetachedFromWindow ();
  }

  @Override
  public synchronized void
  onAttachedToWindow ()
  {
    isAttachedToWindow = true;

    /* Dirty the bitmap, as it was destroyed when onDetachedFromWindow
       was called.  */
    bitmapDirty = true;

    /* Now expose the view contents again.  */
    EmacsNative.sendExpose (this.window.handle, 0, 0,
			    measuredWidth, measuredHeight);

    super.onAttachedToWindow ();
  }

  public void
  showOnScreenKeyboard ()
  {
    /* Specifying no flags at all tells the system the user asked for
       the input method to be displayed.  */
    imManager.showSoftInput (this, 0);
    isCurrentlyTextEditor = true;
  }

  public void
  hideOnScreenKeyboard ()
  {
    imManager.hideSoftInputFromWindow (this.getWindowToken (),
				       0);
    isCurrentlyTextEditor = false;
  }

  @Override
  public InputConnection
  onCreateInputConnection (EditorInfo info)
  {
    /* Make sure the input method never displays a full screen input
       box that obscures Emacs.  */
    info.imeOptions = EditorInfo.IME_FLAG_NO_FULLSCREEN;

    /* Set a reasonable inputType.  */
    info.inputType = InputType.TYPE_NULL;

    return null;
  }

  @Override
  public boolean
  onCheckIsTextEditor ()
  {
    /* If value is true, then the system will display the on screen
       keyboard.  */
    return isCurrentlyTextEditor;
  }
};
