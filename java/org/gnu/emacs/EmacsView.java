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

import android.content.Context;

import android.text.InputType;

import android.view.ContextMenu;
import android.view.ContextThemeWrapper;
import android.view.DragEvent;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.view.WindowInsets;

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

import java.util.Arrays;

/* This is an Android view which has a back and front buffer.  When
   swapBuffers is called, the back buffer is swapped to the front
   buffer, and any damage is invalidated.  A front buffer bitmap defined
   in EmacsSurfaceView, and the write buffer in this file, are modified
   and used both from the UI and the Emacs thread.  As a result, there
   is a lock held during all drawing operations.

   It is also a ViewGroup, so that it may also manage the layout of its
   children.  */

public final class EmacsView extends ViewGroup
  implements ViewTreeObserver.OnGlobalLayoutListener
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

  /* Whether the back buffer has been updated since the last swap.  */
  private boolean unswapped;

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

  /* The associated input connection.  */
  private EmacsInputConnection inputConnection;

  /* The current IC mode.  See `android_reset_ic' for more
     details.  */
  private int icMode;

  /* The number of calls to `resetIC' to have taken place the last
     time an InputConnection was created.  */
  public long icSerial;

  /* The number of calls to `recetIC' that have taken place.  */
  public volatile long icGeneration;

  public
  EmacsView (EmacsWindow window)
  {
    /* This is required to guarantee that popup menus respect the
       default style.  */
    super (new ContextThemeWrapper (EmacsService.SERVICE,
				    R.style.EmacsStyle));
    {
      Object tem;
      Context context;

      this.window = window;
      this.damageRegion = new Region ();

      setFocusable (true);
      setFocusableInTouchMode (true);

      /* Create the surface view.  */
      this.surfaceView = new EmacsSurfaceView (this);
      addView (this.surfaceView);

      /* Get rid of the default focus highlight.  */
      if (Build.VERSION.SDK_INT > Build.VERSION_CODES.O)
	setDefaultFocusHighlightEnabled (false);

      /* Obtain the input method manager.  */
      context = getContext ();
      tem = context.getSystemService (Context.INPUT_METHOD_SERVICE);
      imManager = (InputMethodManager) tem;

      /* Add this view as its own global layout listener.  */
      getViewTreeObserver ().addOnGlobalLayoutListener (this);
    }
  }

  private void
  handleDirtyBitmap ()
  {
    Bitmap oldBitmap;
    int measuredWidth, measuredHeight;

    /* Load measuredWidth and measuredHeight.  */
    measuredWidth = this.measuredWidth;
    measuredHeight = this.measuredHeight;

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

    /* Recreate the back buffer bitmap.  */
    bitmap
      = Bitmap.createBitmap (measuredWidth,
			     measuredHeight,
			     Bitmap.Config.ARGB_8888);
    bitmap.eraseColor (window.background | 0xff000000);

    /* And canvases.  */
    canvas = new Canvas (bitmap);
    canvas.save ();

    /* Since the clip rectangles have been cleared, clear the clip
       rectangle ID.  */
    lastClipSerial = 0;

    /* Clear the bitmap reallocation flag.  */
    bitmapDirty = false;

    /* Explicitly free the old bitmap's memory.  The bitmap might
       continue to be referenced by canvas or JNI objects returned by
       getBitmap or getCanvas, but the underlying storage will not be
       released until such references disappear.  See
       BitmapWrapper::freePixels in hwui/jni/Bitmap.cpp.  */
    if (oldBitmap != null)
      oldBitmap.recycle ();

    /* Some Android versions still refuse to release the bitmap until
       the next GC.  */
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
    /* Never alter the bitmap if modifications have been received that
       are still to be copied to the front buffer, as this indicates
       that redisplay is in the process of copying matrix contents to
       the glass, and such events as generally prompt a complete
       regeneration of the frame's contents might not be processed.  */
    if (!unswapped && (bitmapDirty || bitmap == null))
      handleDirtyBitmap ();
    unswapped = true;

    return bitmap;
  }

  public synchronized Canvas
  getCanvas (EmacsGC gc)
  {
    int i;

    if (!unswapped && (bitmapDirty || bitmap == null))
      handleDirtyBitmap ();

    if (canvas == null)
      return null;
    unswapped = true;

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

    /* This is strictly necessary to propagate layout requests to
       children.  */
    this.measureChildren (widthMeasureSpec, heightMeasureSpec);
    super.setMeasuredDimension (width, height);
  }

  /* Return whether this view's window is focused.  This is made
     necessary by Android 11's unreliable dispatch of
     onWindowFocusChanged prior to gesture navigation away from a
     frame.  */

  public boolean
  checkWindowFocus ()
  {
    EmacsActivity activity;
    Object consumer;

    consumer = window.getAttachedConsumer ();

    if (!(consumer instanceof EmacsActivity))
      return false;

    activity = (EmacsActivity) consumer;
    return activity.hasWindowFocus ();
  }

  /* Note that the monitor lock for the window must never be held from
     within that for the view, because the window acquires locks in the
     opposite direction.  */

  @Override
  protected void
  onLayout (boolean changed, int left, int top, int right,
	    int bottom)
  {
    int count, i, oldMeasuredWidth, oldMeasuredHeight;
    View child;
    Rect windowRect;
    boolean needExpose;
    WindowInsets rootWindowInsets;

    count = getChildCount ();
    needExpose = false;

    synchronized (this)
      {
	/* Load measuredWidth and measuredHeight.  */
	oldMeasuredWidth = measuredWidth;
	oldMeasuredHeight = measuredHeight;

	/* Set measuredWidth and measuredHeight.  */
	measuredWidth = right - left;
	measuredHeight = bottom - top;

	/* If oldMeasuredHeight or oldMeasuredWidth are wrong, set
	   changed to true as well.  */

	if (right - left != oldMeasuredWidth
	    || bottom - top != oldMeasuredHeight)
	  changed = true;

	/* Dirty the back buffer if the layout change resulted in the view
	   being resized.  */

	if (changed)
	  {
	    /* Expose the window upon a change in the view's size that
	       prompts the creation of a new bitmap.  */
	    bitmapDirty = needExpose = true;

	    /* This might return NULL if this view is not attached.  */
	    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R)
	      {
		/* If a toplevel view is focused and
		   isCurrentlyTextEditor is enabled when the IME is
		   hidden, clear isCurrentlyTextEditor so it isn't shown
		   again if the user dismisses Emacs before
		   returning.  */
		rootWindowInsets = getRootWindowInsets ();

		if (isCurrentlyTextEditor
		    && rootWindowInsets != null
		    && isAttachedToWindow
		    && !rootWindowInsets.isVisible (WindowInsets.Type.ime ())
		    /* N.B. that the keyboard is dismissed during
		       gesture navigation under Android 30, but the
		       system is quite temperamental regarding whether
		       the window is focused at that point.  Ideally
		       isCurrentlyTextEditor shouldn't be reset in that
		       case, but detecting that situation appears to be
		       impossible.  Sigh.  */
		    && (window == EmacsActivity.focusedWindow
			&& hasWindowFocus ()))
		  isCurrentlyTextEditor = false;
	      }
	  }
      }

    for (i = 0; i < count; ++i)
      {
	child = getChildAt (i);

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

    if (needExpose && isAttachedToWindow)
      EmacsNative.sendExpose (this.window.handle, 0, 0,
			      right - left, bottom - top);
  }

  public void
  damageRect (Rect damageRect)
  {
    EmacsService.checkEmacsThread ();
    damageRegion.union (damageRect);
  }

  /* This function enables damage to be recorded without consing a new
     Rect object.  */

  public void
  damageRect (int left, int top, int right, int bottom)
  {
    EmacsService.checkEmacsThread ();
    damageRegion.op (left, top, right, bottom, Region.Op.UNION);
  }

  /* Complete deferred reconfiguration of the front buffer after a
     buffer swap completes, and generate Expose events for the same.  */

  private void
  postSwapBuffers ()
  {
    if (!unswapped)
      return;

    unswapped = false;

    /* If the bitmap is dirty, reconfigure the bitmap and
       generate an Expose event to produce its contents.  */

    if ((bitmapDirty || bitmap == null)
	/* Do not generate Expose events if handleDirtyBitmap will
	   not create a valid bitmap, or the consequent buffer swap
	   will produce another event, ad infinitum.  */
	&& isAttachedToWindow && measuredWidth != 0
	&& measuredHeight != 0)
      {
	handleDirtyBitmap ();
	EmacsNative.sendExpose (this.window.handle, 0, 0,
				measuredWidth, measuredHeight);
      }
  }

  public void
  swapBuffers ()
  {
    Canvas canvas;
    Rect damageRect;

    /* Make sure this function is called only from the Emacs
       thread.  */
    EmacsService.checkEmacsThread ();

    damageRect = null;

    /* Now see if there is a damage region.  */

    if (damageRegion.isEmpty ())
      {
	postSwapBuffers ();
	return;
      }

    /* And extract and clear the damage region.  */

    damageRect = damageRegion.getBounds ();
    damageRegion.setEmpty ();

    synchronized (this)
      {
	/* Transfer the bitmap to the surface view, then invalidate
	   it.  */
	surfaceView.setBitmap (bitmap, damageRect);
	postSwapBuffers ();
      }
  }

  @Override
  public boolean
  onKeyPreIme (int keyCode, KeyEvent event)
  {
    /* Several Android systems intercept key events representing
       C-SPC.  Avert this by detecting C-SPC events here and relaying
       them directly to onKeyDown.

       Make this optional though, since some input methods also
       leverage C-SPC as a shortcut for switching languages.  */

    if ((keyCode == KeyEvent.KEYCODE_SPACE
	 && (window.eventModifiers (event)
	     & KeyEvent.META_CTRL_MASK) != 0)
	&& !EmacsNative.shouldForwardCtrlSpace ())
      return onKeyDown (keyCode, event);

    return super.onKeyPreIme (keyCode, event);
  }

  @Override
  public boolean
  onKeyDown (int keyCode, KeyEvent event)
  {
    if (((keyCode == KeyEvent.KEYCODE_VOLUME_UP
	  || keyCode == KeyEvent.KEYCODE_VOLUME_DOWN
	  || keyCode == KeyEvent.KEYCODE_VOLUME_MUTE)
	 && !EmacsNative.shouldForwardMultimediaButtons ())
	|| keyCode == KeyEvent.KEYCODE_SCROLL_LOCK
	|| keyCode == KeyEvent.KEYCODE_NUM_LOCK)
      return super.onKeyDown (keyCode, event);

    return window.onKeyDown (keyCode, event);
  }

  @Override
  public boolean
  onKeyMultiple (int keyCode, int repeatCount, KeyEvent event)
  {
    if (((keyCode == KeyEvent.KEYCODE_VOLUME_UP
	  || keyCode == KeyEvent.KEYCODE_VOLUME_DOWN
	  || keyCode == KeyEvent.KEYCODE_VOLUME_MUTE)
	 && !EmacsNative.shouldForwardMultimediaButtons ())
	|| keyCode == KeyEvent.KEYCODE_SCROLL_LOCK
	|| keyCode == KeyEvent.KEYCODE_NUM_LOCK)
      return super.onKeyMultiple (keyCode, repeatCount, event);

    return window.onKeyDown (keyCode, event);
  }

  @Override
  public boolean
  onKeyUp (int keyCode, KeyEvent event)
  {
    if (((keyCode == KeyEvent.KEYCODE_VOLUME_UP
	  || keyCode == KeyEvent.KEYCODE_VOLUME_DOWN
	  || keyCode == KeyEvent.KEYCODE_VOLUME_MUTE)
	 && !EmacsNative.shouldForwardMultimediaButtons ())
	|| keyCode == KeyEvent.KEYCODE_SCROLL_LOCK
	|| keyCode == KeyEvent.KEYCODE_NUM_LOCK)
      return super.onKeyUp (keyCode, event);

    return window.onKeyUp (keyCode, event);
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
    return window.onGenericMotionEvent (motion);
  }

  @Override
  public boolean
  onTouchEvent (MotionEvent motion)
  {
    return window.onTouchEvent (motion);
  }

  @Override
  public boolean
  onDragEvent (DragEvent drag)
  {
    /* Inter-program drag and drop isn't supported under Android 23
       and earlier.  */

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N)
      return false;

    return window.onDragEvent (drag);
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
	attachViewToParent (child, 1, child.getLayoutParams ());
      }
  }

  /* The following four functions must not be called if the view has
     no parent, or is parented to an activity.  */

  public void
  raise ()
  {
    EmacsView parent;

    parent = (EmacsView) getParent ();

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

    if (parent.indexOfChild (this) == 1)
      return;

    parent.moveChildToBack (this);
  }

  public void
  moveAbove (EmacsView view)
  {
    EmacsView parent;
    int index;

    parent = (EmacsView) getParent ();

    if (parent != view.getParent ())
      throw new IllegalStateException ("Moving view above non-sibling");

    index = parent.indexOfChild (this);
    parent.detachViewFromParent (index);
    index = parent.indexOfChild (view);
    parent.attachViewToParent (this, index + 1, getLayoutParams ());
  }

  public void
  moveBelow (EmacsView view)
  {
    EmacsView parent;
    int index;

    parent = (EmacsView) getParent ();

    if (parent != view.getParent ())
      throw new IllegalStateException ("Moving view above non-sibling");

    index = parent.indexOfChild (this);
    parent.detachViewFromParent (index);
    index = parent.indexOfChild (view);
    parent.attachViewToParent (this, index, getLayoutParams ());
  }

  @Override
  protected void
  onCreateContextMenu (ContextMenu menu)
  {
    if (contextMenu == null)
      return;

    contextMenu.expandTo (menu, this);
  }

  public boolean
  popupMenu (EmacsContextMenu menu, int xPosition,
	     int yPosition, boolean force)
  {
    ContextThemeWrapper context;

    if (popupActive && !force)
      return false;

    /* Android will permanently cease to display any popup menus at
       all if the list of menu items is empty.  Prevent this by
       promptly returning if there are no menu items.  */

    if (menu.menuItems.isEmpty ())
      return false;

    contextMenu = menu;
    popupActive = true;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.VANILLA_ICE_CREAM)
      {
	context = (ContextThemeWrapper) getContext ();
	/* It is necessary to reload the current theme before attempting
	   to display a new popup menu, or any previously applied system
	   theme will continue to apply to it.  */
	context.setTheme (R.style.EmacsStyleOpen);
	context.setTheme (R.style.EmacsStyle);
      }

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

    /* It is not possible to know with 100% certainty which activity is
       currently displaying the context menu.  Loop over each activity
       and call `closeContextMenu' instead.  */

    for (EmacsWindowManager.WindowConsumer consumer
	   : EmacsWindowManager.MANAGER.consumers)
      {
	if (consumer instanceof EmacsActivity)
	  ((EmacsActivity) consumer).closeContextMenu ();
      }
  }

  @Override
  public synchronized void
  onDetachedFromWindow ()
  {
    Bitmap savedBitmap;

    savedBitmap = bitmap;
    isAttachedToWindow = false;
    bitmap = null;
    canvas = null;

    surfaceView.setBitmap (null, null);

    /* Recycle the bitmap and call GC.  */

    if (savedBitmap != null)
      savedBitmap.recycle ();

    /* Collect the bitmap storage; it could be large.  */
    Runtime.getRuntime ().gc ();
    super.onDetachedFromWindow ();
  }

  @Override
  public void
  onAttachedToWindow ()
  {
    synchronized (this)
      {
	isAttachedToWindow = true;

	/* Dirty the bitmap, as it was destroyed when
	   onDetachedFromWindow was called.  */
	bitmapDirty = true;

	/* Rather than unconditionally generating an exposure event upon
	   window attachment, avoid delivering successive Exposure
	   events if the size of the window has changed but is still to
	   be reported by clearing the measured width and height, and
	   requesting another layout computation.  */
	measuredWidth = measuredHeight = 0;
      }

    requestLayout ();
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
    int mode;
    int[] selection;

    /* Figure out what kind of IME behavior Emacs wants.  */
    mode = getICMode ();

    /* Make sure the input method never displays a full screen input
       box that obscures Emacs.  */
    info.imeOptions = EditorInfo.IME_FLAG_NO_FULLSCREEN;
    info.imeOptions |= EditorInfo.IME_FLAG_NO_EXTRACT_UI;

    /* Set a reasonable inputType.  */
    info.inputType = InputType.TYPE_CLASS_TEXT;

    /* If this fails or ANDROID_IC_MODE_NULL was requested, then don't
       initialize the input connection.  */

    if (mode == EmacsService.IC_MODE_NULL)
      {
	info.inputType = InputType.TYPE_NULL;
	return null;
      }

    /* Set icSerial.  If icSerial < icGeneration, the input connection
       has been reset, and future input should be ignored until a new
       connection is created.  */

    icSerial = icGeneration;

    /* Reset flags set by the previous input method.  */

    EmacsNative.clearInputFlags (window.handle);

    /* Obtain the current position of point and set it as the
       selection.  Don't do this under one specific situation: if
       `android_update_ic' is being called in the main thread, trying
       to synchronize with it can cause a dead lock in the IM manager.
       See icBeginSynchronous in EmacsService.java for more
       details.  */

    selection = EmacsService.viewGetSelection (window.handle);

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, ("onCreateInputConnection: "
		   + (selection != null
		      ? Arrays.toString (selection)
		      : "(unavailable)")));

    if (selection == null)
      {
	/* If the selection could not be obtained, return 0 by 0.
	   However, ask for the selection position to be updated as
	   soon as possible.  */

	selection = new int[] { 0, 0, };
	EmacsNative.requestSelectionUpdate (window.handle);
      }

    if (mode == EmacsService.IC_MODE_ACTION
	|| mode == EmacsService.IC_MODE_PASSWORD)
      info.imeOptions |= EditorInfo.IME_ACTION_DONE;

    if (mode == EmacsService.IC_MODE_PASSWORD)
      info.inputType  |= InputType.TYPE_TEXT_VARIATION_PASSWORD;

    /* Set the initial selection fields.  */
    info.initialSelStart = selection[0];
    info.initialSelEnd = selection[1];

    /* Create the input connection if necessary.  */

    if (inputConnection == null)
      inputConnection = new EmacsInputConnection (this);
    else
      /* Clear several pieces of state in the input connection.  */
      inputConnection.reset ();

    /* Return the input connection.  */
    return inputConnection;
  }

  @Override
  public synchronized boolean
  onCheckIsTextEditor ()
  {
    /* If value is true, then the system will display the on screen
       keyboard.  */
    return isCurrentlyTextEditor;
  }

  @Override
  public boolean
  isOpaque ()
  {
    /* Returning true here allows the system to not draw the contents
       of windows underneath this view, thereby improving
       performance.  */
    return true;
  }

  public void
  setICMode (int icMode)
  {
    this.icMode = icMode;
  }

  public int
  getICMode ()
  {
    return icMode;
  }

  @Override
  public void
  onGlobalLayout ()
  {
    int[] locations;

    /* Get the absolute offset of this view and specify its left and
       top position in subsequent ConfigureNotify events.  */

    locations = new int[2];
    getLocationInWindow (locations);
    window.notifyContentRectPosition (locations[0],
				      locations[1]);
  }

  @Override
  public WindowInsets
  onApplyWindowInsets (WindowInsets insets)
  {
    WindowInsets rootWindowInsets;

    /* This function is called when window insets change, which
       encompasses input method visibility changes under Android 30
       and later.  If a toplevel view is focused and
       isCurrentlyTextEditor is enabled when the IME is hidden, clear
       isCurrentlyTextEditor so it isn't shown again if the user
       dismisses Emacs before returning.  */

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.R)
      return super.onApplyWindowInsets (insets);

    /* This might return NULL if this view is not attached.  */
    rootWindowInsets = getRootWindowInsets ();

    if (isCurrentlyTextEditor
	&& rootWindowInsets != null
	&& isAttachedToWindow
	&& !rootWindowInsets.isVisible (WindowInsets.Type.ime ())
	&& window == EmacsActivity.focusedWindow)
      isCurrentlyTextEditor = false;

    return super.onApplyWindowInsets (insets);
  }
};
