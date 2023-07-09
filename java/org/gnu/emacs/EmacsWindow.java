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

import java.lang.IllegalStateException;
import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import android.content.Context;

import android.graphics.Rect;
import android.graphics.Canvas;
import android.graphics.Bitmap;
import android.graphics.PixelFormat;

import android.view.View;
import android.view.ViewManager;
import android.view.Gravity;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.InputDevice;
import android.view.WindowManager;

import android.util.Log;

import android.os.Build;

/* This defines a window, which is a handle.  Windows represent a
   rectangular subset of the screen with their own contents.

   Windows either have a parent window, in which case their views are
   attached to the parent's view, or are "floating", in which case
   their views are attached to the parent activity (if any), else
   nothing.

   Views are also drawables, meaning they can accept drawing
   requests.  */

public final class EmacsWindow extends EmacsHandleObject
  implements EmacsDrawable
{
  private static final String TAG = "EmacsWindow";

  private static class Coordinate
  {
    /* Integral coordinate.  */
    int x, y;

    public
    Coordinate (int x, int y)
    {
      this.x = x;
      this.y = y;
    }
  };

  /* The view associated with the window.  */
  public EmacsView view;

  /* The geometry of the window.  */
  private Rect rect;

  /* The parent window, or null if it is the root window.  */
  public EmacsWindow parent;

  /* List of all children in stacking order.  This must be kept
     consistent with their Z order!  */
  public ArrayList<EmacsWindow> children;

  /* Map between pointer identifiers and last known position.  Used to
     compute which pointer changed upon a touch event.  */
  private HashMap<Integer, Coordinate> pointerMap;

  /* The window consumer currently attached, if it exists.  */
  private EmacsWindowAttachmentManager.WindowConsumer attached;

  /* The window background scratch GC.  foreground is always the
     window background.  */
  private EmacsGC scratchGC;

  /* The button state and keyboard modifier mask at the time of the
     last button press or release event.  */
  public int lastButtonState;

  /* Whether or not the window is mapped.  */
  private volatile boolean isMapped;

  /* Whether or not to ask for focus upon being mapped.  */
  private boolean dontFocusOnMap;

  /* Whether or not the window is override-redirect.  An
     override-redirect window always has its own system window.  */
  private boolean overrideRedirect;

  /* The window manager that is the parent of this window.  NULL if
     there is no such window manager.  */
  private WindowManager windowManager;

  /* The time of the last KEYCODE_VOLUME_DOWN release.  This is used
     to quit Emacs upon two rapid clicks of the volume down
     button.  */
  private long lastVolumeButtonRelease;

  /* Linked list of character strings which were recently sent as
     events.  */
  public LinkedHashMap<Integer, String> eventStrings;

  /* Whether or not this window is fullscreen.  */
  public boolean fullscreen;

  /* The window background pixel.  This is used by EmacsView when
     creating new bitmaps.  */
  public volatile int background;

  /* The position of this window relative to the root window.  */
  public int xPosition, yPosition;

  public
  EmacsWindow (short handle, final EmacsWindow parent, int x, int y,
	       int width, int height, boolean overrideRedirect)
  {
    super (handle);

    rect = new Rect (x, y, x + width, y + height);
    pointerMap = new HashMap<Integer, Coordinate> ();

    /* Create the view from the context's UI thread.  The window is
       unmapped, so the view is GONE.  */
    view = EmacsService.SERVICE.getEmacsView (this, View.GONE,
					      parent == null);
    this.parent = parent;
    this.overrideRedirect = overrideRedirect;

    /* Create the list of children.  */
    children = new ArrayList<EmacsWindow> ();

    if (parent != null)
      {
	parent.children.add (this);
        EmacsService.SERVICE.runOnUiThread (new Runnable () {
	    @Override
	    public void
	    run ()
	    {
	      parent.view.addView (view);
	    }
	  });
      }

    scratchGC = new EmacsGC ((short) 0);

    /* Create the map of input method-committed strings.  Keep at most
       ten strings in the map.  */

    eventStrings
      = new LinkedHashMap<Integer, String> () {
	  @Override
	  protected boolean
	  removeEldestEntry (Map.Entry<Integer, String> entry)
	  {
	    return size () > 10;
	  }
	};
  }

  public void
  changeWindowBackground (int pixel)
  {
    /* scratchGC is used as the argument to a FillRectangles req.  */
    scratchGC.foreground = pixel;
    scratchGC.markDirty (false);

    /* Make the background known to the view as well.  */
    background = pixel;
  }

  public synchronized Rect
  getGeometry ()
  {
    return new Rect (rect);
  }

  @Override
  public synchronized void
  destroyHandle () throws IllegalStateException
  {
    if (parent != null)
      parent.children.remove (this);

    EmacsActivity.invalidateFocus ();

    if (!children.isEmpty ())
      throw new IllegalStateException ("Trying to destroy window with "
				       + "children!");

    /* Remove the view from its parent and make it invisible.  */
    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	public void
	run ()
	{
	  ViewManager parent;
	  EmacsWindowAttachmentManager manager;

	  if (EmacsActivity.focusedWindow == EmacsWindow.this)
	    EmacsActivity.focusedWindow = null;

	  manager = EmacsWindowAttachmentManager.MANAGER;
	  view.setVisibility (View.GONE);

	  /* If the window manager is set, use that instead.  */
	  if (windowManager != null)
	    parent = windowManager;
	  else
	    parent = (ViewManager) view.getParent ();
	  windowManager = null;

	  if (parent != null)
	    parent.removeView (view);

	  manager.detachWindow (EmacsWindow.this);
	}
      });

    super.destroyHandle ();
  }

  public void
  setConsumer (EmacsWindowAttachmentManager.WindowConsumer consumer)
  {
    attached = consumer;
  }

  public EmacsWindowAttachmentManager.WindowConsumer
  getAttachedConsumer ()
  {
    return attached;
  }

  public synchronized long
  viewLayout (int left, int top, int right, int bottom)
  {
    int rectWidth, rectHeight;

    rect.left = left;
    rect.top = top;
    rect.right = right;
    rect.bottom = bottom;

    rectWidth = right - left;
    rectHeight = bottom - top;

    /* If parent is null, use xPosition and yPosition instead of the
       geometry rectangle positions.  */

    if (parent == null)
      {
	left = xPosition;
	top = yPosition;
      }

    return EmacsNative.sendConfigureNotify (this.handle,
					    System.currentTimeMillis (),
					    left, top, rectWidth,
					    rectHeight);
  }

  public void
  requestViewLayout ()
  {
    view.explicitlyDirtyBitmap ();

    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  if (overrideRedirect)
	    /* Set the layout parameters again.  */
	    view.setLayoutParams (getWindowLayoutParams ());

	  view.mustReportLayout = true;
	  view.requestLayout ();
	}
      });
  }

  public synchronized void
  resizeWindow (int width, int height)
  {
    rect.right = rect.left + width;
    rect.bottom = rect.top + height;

    requestViewLayout ();
  }

  public synchronized void
  moveWindow (int x, int y)
  {
    int width, height;

    width = rect.width ();
    height = rect.height ();

    rect.left = x;
    rect.top = y;
    rect.right = x + width;
    rect.bottom = y + height;

    requestViewLayout ();
  }

  private WindowManager.LayoutParams
  getWindowLayoutParams ()
  {
    WindowManager.LayoutParams params;
    int flags, type;
    Rect rect;

    flags = 0;
    rect = getGeometry ();
    flags |= WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE;
    flags |= WindowManager.LayoutParams.FLAG_NOT_TOUCHABLE;
    type = WindowManager.LayoutParams.TYPE_APPLICATION_ATTACHED_DIALOG;

    params
      = new WindowManager.LayoutParams (rect.width (), rect.height (),
					rect.left, rect.top,
					type, flags,
					PixelFormat.RGBA_8888);
    params.gravity = Gravity.TOP | Gravity.LEFT;
    return params;
  }

  private Context
  findSuitableActivityContext ()
  {
    /* Find a recently focused activity.  */
    if (!EmacsActivity.focusedActivities.isEmpty ())
      return EmacsActivity.focusedActivities.get (0);

    /* Return the service context, which probably won't work.  */
    return EmacsService.SERVICE;
  }

  public synchronized void
  mapWindow ()
  {
    final int width, height;

    if (isMapped)
      return;

    isMapped = true;
    width = rect.width ();
    height = rect.height ();

    if (parent == null)
      {
        EmacsService.SERVICE.runOnUiThread (new Runnable () {
	    @Override
	    public void
	    run ()
	    {
	      EmacsWindowAttachmentManager manager;
	      WindowManager windowManager;
	      Context ctx;
	      Object tem;
	      WindowManager.LayoutParams params;

	      /* Make the view visible, first of all.  */
	      view.setVisibility (View.VISIBLE);

	      if (!overrideRedirect)
		{
		  manager = EmacsWindowAttachmentManager.MANAGER;

		  /* If parent is the root window, notice that there are new
		     children available for interested activites to pick
		     up.  */
		  manager.registerWindow (EmacsWindow.this);

		  if (!getDontFocusOnMap ())
		    /* Eventually this should check no-focus-on-map.  */
		    view.requestFocus ();
		}
	      else
		{
		  /* But if the window is an override-redirect window,
		     then:

		     - Find an activity that is currently active.

		     - Map the window as a panel on top of that
                       activity using the system window manager.  */

		  ctx = findSuitableActivityContext ();
		  tem = ctx.getSystemService (Context.WINDOW_SERVICE);
		  windowManager = (WindowManager) tem;

		  /* Calculate layout parameters.  */
		  params = getWindowLayoutParams ();
		  view.setLayoutParams (params);

		  /* Attach the view.  */
		  try
		    {
		      view.prepareForLayout (width, height);
		      windowManager.addView (view, params);

		      /* Record the window manager being used in the
			 EmacsWindow object.  */
		      EmacsWindow.this.windowManager = windowManager;
		    }
		  catch (Exception e)
		    {
		      Log.w (TAG,
			     "failed to attach override-redirect window, " + e);
		    }
		}
	    }
	  });
      }
    else
      {
	/* Do the same thing as above, but don't register this
	   window.  */
        EmacsService.SERVICE.runOnUiThread (new Runnable () {
	    @Override
	    public void
	    run ()
	    {
	      /* Prior to mapping the view, set its measuredWidth and
		 measuredHeight to some reasonable value, in order to
		 avoid excessive bitmap dirtying.  */

	      view.prepareForLayout (width, height);
	      view.setVisibility (View.VISIBLE);

	      if (!getDontFocusOnMap ())
		view.requestFocus ();
	    }
	  });
      }
  }

  public synchronized void
  unmapWindow ()
  {
    if (!isMapped)
      return;

    isMapped = false;

    view.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  EmacsWindowAttachmentManager manager;

	  manager = EmacsWindowAttachmentManager.MANAGER;

	  view.setVisibility (View.GONE);

	  /* Detach the view from the window manager if possible.  */
	  if (windowManager != null)
	    windowManager.removeView (view);
	  windowManager = null;

	  /* Now that the window is unmapped, unregister it as
	     well.  */
	  manager.detachWindow (EmacsWindow.this);
	}
      });
  }

  @Override
  public Canvas
  lockCanvas (EmacsGC gc)
  {
    return view.getCanvas (gc);
  }

  @Override
  public void
  damageRect (Rect damageRect)
  {
    view.damageRect (damageRect);
  }

  public void
  swapBuffers ()
  {
    view.swapBuffers ();
  }

  public void
  clearWindow ()
  {
    EmacsService.SERVICE.fillRectangle (this, scratchGC,
					0, 0, rect.width (),
					rect.height ());
  }

  public void
  clearArea (int x, int y, int width, int height)
  {
    EmacsService.SERVICE.fillRectangle (this, scratchGC,
					x, y, width, height);
  }

  @Override
  public Bitmap
  getBitmap ()
  {
    return view.getBitmap ();
  }

  /* event.getCharacters is used because older input methods still
     require it.  */
  @SuppressWarnings ("deprecation")
  public int
  getEventUnicodeChar (KeyEvent event, int state)
  {
    String characters;

    if (event.getUnicodeChar (state) != 0)
      return event.getUnicodeChar (state);

    characters = event.getCharacters ();

    if (characters != null && characters.length () == 1)
      return characters.charAt (0);

    return characters == null ? 0 : -1;
  }

  public void
  saveUnicodeString (int serial, String string)
  {
    eventStrings.put (serial, string);
  }



  /* Return the modifier mask associated with the specified keyboard
     input EVENT.  Replace bits corresponding to Left or Right keys
     with their corresponding general modifier bits.  */

  private int
  eventModifiers (KeyEvent event)
  {
    int state;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR2)
      state = event.getModifiers ();
    else
      {
	/* Replace this with getMetaState and manual
	   normalization.  */
	state = event.getMetaState ();

	/* Normalize the state by setting the generic modifier bit if
	   either a left or right modifier is pressed.  */

	if ((state & KeyEvent.META_ALT_LEFT_ON) != 0
	    || (state & KeyEvent.META_ALT_RIGHT_ON) != 0)
	  state |= KeyEvent.META_ALT_MASK;

	if ((state & KeyEvent.META_CTRL_LEFT_ON) != 0
	    || (state & KeyEvent.META_CTRL_RIGHT_ON) != 0)
	  state |= KeyEvent.META_CTRL_MASK;
      }

    return state;
  }

  /* Return the modifier mask associated with the specified motion
     EVENT.  Replace bits corresponding to Left or Right keys with
     their corresponding general modifier bits.  */

  private int
  motionEventModifiers (MotionEvent event)
  {
    int state;

    state = event.getMetaState ();

    /* Normalize the state by setting the generic modifier bit if
       either a left or right modifier is pressed.  */

    if ((state & KeyEvent.META_ALT_LEFT_ON) != 0
	|| (state & KeyEvent.META_ALT_RIGHT_ON) != 0)
      state |= KeyEvent.META_ALT_MASK;

    if ((state & KeyEvent.META_CTRL_LEFT_ON) != 0
	|| (state & KeyEvent.META_CTRL_RIGHT_ON) != 0)
      state |= KeyEvent.META_CTRL_MASK;

    return state;
  }

  /* event.getCharacters is used because older input methods still
     require it.  */
  @SuppressWarnings ("deprecation")
  public void
  onKeyDown (int keyCode, KeyEvent event)
  {
    int state, state_1;
    long serial;
    String characters;

    state = eventModifiers (event);

    /* Ignore meta-state understood by Emacs for now, or Ctrl+C will
       not be recognized as an ASCII key press event.  */
    state_1
      = state & ~(KeyEvent.META_ALT_MASK | KeyEvent.META_CTRL_MASK);

    synchronized (eventStrings)
      {
	serial
	  = EmacsNative.sendKeyPress (this.handle,
				      event.getEventTime (),
				      state, keyCode,
				      getEventUnicodeChar (event,
							   state_1));

	characters = event.getCharacters ();

	if (characters != null && characters.length () > 1)
	  saveUnicodeString ((int) serial, characters);
      }
  }

  public void
  onKeyUp (int keyCode, KeyEvent event)
  {
    int state, state_1;
    long time;

    /* Compute the event's modifier mask.  */
    state = eventModifiers (event);

    /* Ignore meta-state understood by Emacs for now, or Ctrl+C will
       not be recognized as an ASCII key press event.  */
    state_1
      = state & ~(KeyEvent.META_ALT_MASK | KeyEvent.META_CTRL_MASK);

    EmacsNative.sendKeyRelease (this.handle,
				event.getEventTime (),
				state, keyCode,
				getEventUnicodeChar (event,
						     state_1));

    if (keyCode == KeyEvent.KEYCODE_VOLUME_DOWN)
      {
	/* Check if this volume down press should quit Emacs.
	   Most Android devices have no physical keyboard, so it
	   is unreasonably hard to press C-g.  */

	time = event.getEventTime ();

	if (time - lastVolumeButtonRelease < 350)
	  EmacsNative.quit ();

	lastVolumeButtonRelease = time;
      }
  }

  public void
  onFocusChanged (boolean gainFocus)
  {
    EmacsActivity.invalidateFocus ();
  }

  /* Notice that the activity has been detached or destroyed.

     ISFINISHING is set if the activity is not the main activity, or
     if the activity was not destroyed in response to explicit user
     action.  */

  public void
  onActivityDetached (boolean isFinishing)
  {
    /* Destroy the associated frame when the activity is detached in
       response to explicit user action.  */

    if (isFinishing)
      EmacsNative.sendWindowAction (this.handle, 0);
  }

  /* Look through the button state to determine what button EVENT was
     generated from.  DOWN is true if EVENT is a button press event,
     false otherwise.  Value is the X number of the button.  */

  private int
  whatButtonWasIt (MotionEvent event, boolean down)
  {
    int eventState, notIn;

    if (Build.VERSION.SDK_INT
	< Build.VERSION_CODES.ICE_CREAM_SANDWICH)
      /* Earlier versions of Android only support one mouse
	 button.  */
      return 1;

    eventState = event.getButtonState ();
    notIn = (down ? eventState & ~lastButtonState
	     : lastButtonState & ~eventState);

    if ((notIn & MotionEvent.BUTTON_PRIMARY) != 0)
      return 1;

    if ((notIn & MotionEvent.BUTTON_SECONDARY) != 0)
      return 3;

    if ((notIn & MotionEvent.BUTTON_TERTIARY) != 0)
      return 2;

    /* Not a real value.  */
    return 4;
  }

  /* Return the ID of the pointer which changed in EVENT.  Value is -1
     if it could not be determined, else the pointer that changed, or
     -2 if -1 would have been returned, but there is also a pointer
     that is a mouse.  */

  private int
  figureChange (MotionEvent event)
  {
    int pointerID, i, truncatedX, truncatedY, pointerIndex;
    Coordinate coordinate;
    boolean mouseFlag;

    /* pointerID is always initialized but the Java compiler is too
       dumb to know that.  */
    pointerID = -1;
    mouseFlag = false;

    switch (event.getActionMasked ())
      {
      case MotionEvent.ACTION_DOWN:
	/* Primary pointer pressed with index 0.  */

	/* Detect mice.  If this is a mouse event, give it to
	   onSomeKindOfMotionEvent.  */
	if ((Build.VERSION.SDK_INT
	     >= Build.VERSION_CODES.ICE_CREAM_SANDWICH)
	    && event.getToolType (0) == MotionEvent.TOOL_TYPE_MOUSE)
	  return -2;

	pointerID = event.getPointerId (0);
	pointerMap.put (pointerID,
			new Coordinate ((int) event.getX (0),
					(int) event.getY (0)));
	break;

      case MotionEvent.ACTION_UP:

	/* Detect mice.  If this is a mouse event, give it to
	   onSomeKindOfMotionEvent.  */
	if ((Build.VERSION.SDK_INT
	     >= Build.VERSION_CODES.ICE_CREAM_SANDWICH)
	    && event.getToolType (0) == MotionEvent.TOOL_TYPE_MOUSE)
	  return -2;

	/* Primary pointer released with index 0.  */
	pointerID = event.getPointerId (0);
	pointerMap.remove (pointerID);
	break;

      case MotionEvent.ACTION_POINTER_DOWN:
	/* New pointer.  Find the pointer ID from the index and place
	   it in the map.  */
	pointerIndex = event.getActionIndex ();
	pointerID = event.getPointerId (pointerIndex);
	pointerMap.put (pointerID,
			new Coordinate ((int) event.getX (pointerIndex),
					(int) event.getY (pointerIndex)));
	break;

      case MotionEvent.ACTION_POINTER_UP:
	/* Pointer removed.  Remove it from the map.  */
	pointerIndex = event.getActionIndex ();
	pointerID = event.getPointerId (pointerIndex);
	pointerMap.remove (pointerID);
	break;

      default:

	/* Loop through each pointer in the event.  */
	for (i = 0; i < event.getPointerCount (); ++i)
	  {
	    pointerID = event.getPointerId (i);

	    /* Look up that pointer in the map.  */
	    coordinate = pointerMap.get (pointerID);

	    if (coordinate != null)
	      {
		/* See if coordinates have changed.  */
		truncatedX = (int) event.getX (i);
		truncatedY = (int) event.getY (i);

		if (truncatedX != coordinate.x
		    || truncatedY != coordinate.y)
		  {
		    /* The pointer changed.  Update the coordinate and
		       break out of the loop.  */
		    coordinate.x = truncatedX;
		    coordinate.y = truncatedY;

		    break;
		  }
	      }

	    /* See if this is a mouse.  If so, set the mouseFlag.  */
	    if ((Build.VERSION.SDK_INT
		 >= Build.VERSION_CODES.ICE_CREAM_SANDWICH)
		&& event.getToolType (i) == MotionEvent.TOOL_TYPE_MOUSE)
	      mouseFlag = true;
	  }

	/* Set the pointer ID to -1 if the loop failed to find any
	   changed pointer.  If a mouse pointer was found, set it to
	   -2.  */
	if (i == event.getPointerCount ())
	  pointerID = (mouseFlag ? -2 : -1);
      }

    /* Return the pointer ID.  */
    return pointerID;
  }

  public boolean
  onTouchEvent (MotionEvent event)
  {
    int pointerID, index;

    /* Extract the ``touch ID'' (or in Android, the ``pointer
       ID''.) */
    pointerID = figureChange (event);

    if (pointerID < 0)
      {
	/* If this is a mouse event, give it to
	   onSomeKindOfMotionEvent.  */
	if (pointerID == -2)
	  return onSomeKindOfMotionEvent (event);

	return false;
      }

    /* Find the pointer index corresponding to the event.  */
    index = event.findPointerIndex (pointerID);

    switch (event.getActionMasked ())
      {
      case MotionEvent.ACTION_DOWN:
      case MotionEvent.ACTION_POINTER_DOWN:
	/* Touch down event.  */
	EmacsNative.sendTouchDown (this.handle, (int) event.getX (index),
				   (int) event.getY (index),
				   event.getEventTime (), pointerID);
	return true;

      case MotionEvent.ACTION_UP:
      case MotionEvent.ACTION_POINTER_UP:
      case MotionEvent.ACTION_CANCEL:
	/* Touch up event.  Android documentation says ACTION_CANCEL
	   should be treated as more or less equivalent to ACTION_UP,
	   so that is what is done here.  */
	EmacsNative.sendTouchUp (this.handle, (int) event.getX (index),
				 (int) event.getY (index),
				 event.getEventTime (), pointerID);
	return true;

      case MotionEvent.ACTION_MOVE:
	/* Pointer motion event.  */
	EmacsNative.sendTouchMove (this.handle, (int) event.getX (index),
				   (int) event.getY (index),
				   event.getEventTime (), pointerID);
	return true;
      }

    return false;
  }

  public boolean
  onSomeKindOfMotionEvent (MotionEvent event)
  {
    /* isFromSource is not available until API level 18.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2)
      {
	if (!event.isFromSource (InputDevice.SOURCE_CLASS_POINTER))
	  return false;
      }
    else if (event.getSource () != InputDevice.SOURCE_CLASS_POINTER)
      return false;

    switch (event.getAction ())
      {
      case MotionEvent.ACTION_HOVER_ENTER:
	EmacsNative.sendEnterNotify (this.handle, (int) event.getX (),
				     (int) event.getY (),
				     event.getEventTime ());
	return true;

      case MotionEvent.ACTION_MOVE:
      case MotionEvent.ACTION_HOVER_MOVE:
	EmacsNative.sendMotionNotify (this.handle, (int) event.getX (),
				      (int) event.getY (),
				      event.getEventTime ());
	return true;

      case MotionEvent.ACTION_HOVER_EXIT:

	/* If the exit event comes from a button press, its button
	   state will have extra bits compared to the last known
	   button state.  Since the exit event will interfere with
	   tool bar button presses, ignore such splurious events.  */

	if ((event.getButtonState () & ~lastButtonState) == 0)
	  EmacsNative.sendLeaveNotify (this.handle, (int) event.getX (),
				       (int) event.getY (),
				       event.getEventTime ());

	return true;

      case MotionEvent.ACTION_BUTTON_PRESS:
	/* Find the button which was pressed.  */
	EmacsNative.sendButtonPress (this.handle, (int) event.getX (),
				     (int) event.getY (),
				     event.getEventTime (),
				     motionEventModifiers (event),
				     whatButtonWasIt (event, true));

	if (Build.VERSION.SDK_INT
	    < Build.VERSION_CODES.ICE_CREAM_SANDWICH)
	  return true;

	lastButtonState = event.getButtonState ();
	return true;

      case MotionEvent.ACTION_BUTTON_RELEASE:
	/* Find the button which was released.  */
	EmacsNative.sendButtonRelease (this.handle, (int) event.getX (),
				       (int) event.getY (),
				       event.getEventTime (),
				       motionEventModifiers (event),
				       whatButtonWasIt (event, false));

	if (Build.VERSION.SDK_INT
	    < Build.VERSION_CODES.ICE_CREAM_SANDWICH)
	  return true;

	lastButtonState = event.getButtonState ();
	return true;

      case MotionEvent.ACTION_DOWN:
	/* Emacs must return true even though touch events are not
	   handled here, because the value of this function is used by
	   the system to decide whether or not Emacs gets ACTION_MOVE
	   events.  */
	return true;

      case MotionEvent.ACTION_UP:
	/* However, if ACTION_UP reports a different button state from
	   the last known state, look up which button was released and
	   send a ButtonRelease event; this is to work around a bug in
	   the framework where real ACTION_BUTTON_RELEASE events are
	   not delivered.  */

	if (Build.VERSION.SDK_INT
	    < Build.VERSION_CODES.ICE_CREAM_SANDWICH)
	  return true;

	if (event.getButtonState () == 0 && lastButtonState != 0)
	  {
	    EmacsNative.sendButtonRelease (this.handle, (int) event.getX (),
					   (int) event.getY (),
					   event.getEventTime (),
					   motionEventModifiers (event),
					   whatButtonWasIt (event, false));
	    lastButtonState = event.getButtonState ();
	  }

	return true;

      case MotionEvent.ACTION_SCROLL:
	/* Send a scroll event with the specified deltas.  */
	EmacsNative.sendWheel (this.handle, (int) event.getX (),
			       (int) event.getY (),
			       event.getEventTime (),
			       motionEventModifiers (event),
			       event.getAxisValue (MotionEvent.AXIS_HSCROLL),
			       event.getAxisValue (MotionEvent.AXIS_VSCROLL));
	return true;
      }

    return false;
  }

  public synchronized void
  reparentTo (final EmacsWindow otherWindow, int x, int y)
  {
    int width, height;

    /* Reparent this window to the other window.  */

    if (parent != null)
      parent.children.remove (this);

    if (otherWindow != null)
      otherWindow.children.add (this);

    parent = otherWindow;

    /* Move this window to the new location.  */
    width = rect.width ();
    height = rect.height ();
    rect.left = x;
    rect.top = y;
    rect.right = x + width;
    rect.bottom = y + height;

    /* Now do the work necessary on the UI thread to reparent the
       window.  */
    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  EmacsWindowAttachmentManager manager;
	  ViewManager parent;

	  /* First, detach this window if necessary.  */
	  manager = EmacsWindowAttachmentManager.MANAGER;
	  manager.detachWindow (EmacsWindow.this);

	  /* Also unparent this view.  */

	  /* If the window manager is set, use that instead.  */
	  if (windowManager != null)
	    parent = windowManager;
	  else
	    parent = (ViewManager) view.getParent ();
	  windowManager = null;

	  if (parent != null)
	    parent.removeView (view);

	  /* Next, either add this window as a child of the new
	     parent's view, or make it available again.  */
	  if (otherWindow != null)
	    otherWindow.view.addView (view);
	  else if (EmacsWindow.this.isMapped)
	    manager.registerWindow (EmacsWindow.this);

	  /* Request relayout.  */
	  view.requestLayout ();
	}
      });
  }

  public void
  makeInputFocus (long time)
  {
    /* TIME is currently ignored.  Request the input focus now.  */

    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  view.requestFocus ();
	}
      });
  }

  public synchronized void
  raise ()
  {
    /* This does nothing here.  */
    if (parent == null)
      return;

    /* Remove and add this view again.  */
    parent.children.remove (this);
    parent.children.add (this);

    /* Request a relayout.  */
    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  view.raise ();
	}
      });
  }

  public synchronized void
  lower ()
  {
    /* This does nothing here.  */
    if (parent == null)
      return;

    /* Remove and add this view again.  */
    parent.children.remove (this);
    parent.children.add (this);

    /* Request a relayout.  */
    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  view.lower ();
	}
      });
  }

  public synchronized int[]
  getWindowGeometry ()
  {
    int[] array;

    array = new int[4];

    array[0] = parent != null ? rect.left : xPosition;
    array[1] = parent != null ? rect.top : yPosition;
    array[2] = rect.width ();
    array[3] = rect.height ();

    return array;
  }

  public void
  noticeIconified ()
  {
    EmacsNative.sendIconified (this.handle);
  }

  public void
  noticeDeiconified ()
  {
    EmacsNative.sendDeiconified (this.handle);
  }

  public synchronized void
  setDontAcceptFocus (final boolean dontAcceptFocus)
  {
    /* Update the view's focus state.  */
    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  view.setFocusable (!dontAcceptFocus);
	  view.setFocusableInTouchMode (!dontAcceptFocus);
	}
      });
  }

  public synchronized void
  setDontFocusOnMap (final boolean dontFocusOnMap)
  {
    this.dontFocusOnMap = dontFocusOnMap;
  }

  public synchronized boolean
  getDontFocusOnMap ()
  {
    return dontFocusOnMap;
  }

  public int[]
  translateCoordinates (int x, int y)
  {
    int[] array;

    /* This is supposed to translate coordinates to the root
       window.  */
    array = new int[2];
    EmacsService.SERVICE.getLocationOnScreen (view, array);

    /* Now, the coordinates of the view should be in array.  Offset X
       and Y by them.  */
    array[0] += x;
    array[1] += y;

    /* Return the resulting coordinates.  */
    return array;
  }

  public void
  toggleOnScreenKeyboard (final boolean on)
  {
    /* Even though InputMethodManager functions are thread safe,
       `showOnScreenKeyboard' etc must be called from the UI thread in
       order to avoid deadlocks if the calls happen in tandem with a
       call to a synchronizing function within
       `onCreateInputConnection'.  */

    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  if (on)
	    view.showOnScreenKeyboard ();
	  else
	    view.hideOnScreenKeyboard ();	  
	}
      });
  }

  public String
  lookupString (int eventSerial)
  {
    String any;

    synchronized (eventStrings)
      {
	any = eventStrings.remove (eventSerial);
      }

    return any;
  }

  public void
  setFullscreen (final boolean isFullscreen)
  {
    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  EmacsActivity activity;
	  Object tem;

	  fullscreen = isFullscreen;
	  tem = getAttachedConsumer ();

	  if (tem != null)
	    {
	      activity = (EmacsActivity) getAttachedConsumer ();
	      activity.syncFullscreenWith (EmacsWindow.this);
	    }
	}
      });
  }

  public void
  defineCursor (final EmacsCursor cursor)
  {
    /* Don't post this message if pointer icons aren't supported.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N)
      view.post (new Runnable () {
	  @Override
	  public void
	  run ()
	  {
	    if (cursor != null)
	      view.setPointerIcon (cursor.icon);
	    else
	      view.setPointerIcon (null);
	  }
	});
  }

  public synchronized void
  notifyContentRectPosition (int xPosition, int yPosition)
  {
    Rect geometry;

    /* Ignore these notifications if not a child of the root
       window.  */
    if (parent != null)
      return;

    /* xPosition and yPosition are the position of this window
       relative to the screen.  Set them and request a ConfigureNotify
       event.  */

    if (this.xPosition != xPosition
	|| this.yPosition != yPosition)
      {
	this.xPosition = xPosition;
	this.yPosition = yPosition;

	EmacsNative.sendConfigureNotify (this.handle,
					 System.currentTimeMillis (),
					 xPosition, yPosition,
					 rect.width (), rect.height ());
      }
  }
};
