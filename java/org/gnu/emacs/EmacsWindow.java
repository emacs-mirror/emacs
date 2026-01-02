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

import java.lang.IllegalStateException;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;

import android.app.Activity;

import android.content.ClipData;
import android.content.ClipDescription;
import android.content.ContentResolver;
import android.content.Context;

import android.graphics.Rect;
import android.graphics.Canvas;
import android.graphics.Bitmap;
import android.graphics.PixelFormat;

import android.net.Uri;

import android.view.DragEvent;
import android.view.Gravity;
import android.view.InputDevice;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewManager;
import android.view.WindowManager;

import android.util.SparseArray;
import android.util.Log;

import android.os.Build;
import android.os.SystemClock;

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

  /* Whether any windows have yet been created in this session.  */
  private static boolean initialWindowCreated;

  private static class Coordinate
  {
    /* Integral coordinate.  */
    int x, y;

    /* Button associated with the coordinate, or 0 if it is a touch
       event.  */
    int button;

    /* Pointer ID associated with the coordinate.  */
    int id;

    public
    Coordinate (int x, int y, int button, int id)
    {
      this.x = x;
      this.y = y;
      this.button = button;
      this.id = id;
    }
  };

  /* The view associated with the window.  */
  public EmacsView view;

  /* The geometry of the window.  */
  private Rect rect;

  /* The parent window, or null if it is the root window.  */
  public EmacsWindow parent;

  /* List of all children in stacking order.  This must be kept
     consistent with their Z order!

     Synchronize access to this list with itself.  */
  public ArrayList<EmacsWindow> children;

  /* Map between pointer identifiers and last known position.  Used to
     compute which pointer changed upon a touch event.  */
  private SparseArray<Coordinate> pointerMap;

  /* The window consumer currently attached, if it exists.  */
  private EmacsWindowManager.WindowConsumer attached;

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

  /* The time of the last release of the quit keycode, generally
     KEYCODE_VOLUME_DOWN.  This is used to signal quit upon two rapid
     presses of such key.  */
  private long lastQuitKeyRelease;

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

  /* The position of the last drag and drop event received; both
     values are -1 if no drag and drop operation is under way.  */
  private int dndXPosition, dndYPosition;

  /* Identifier binding this window to the activity created for it, or
     -1 if the window should be attached to system-created activities
     (i.e. the activity launched by the system at startup).  Value is
     meaningless under API level 29 and earlier.  */
  public long attachmentToken;

  /* Whether this window should be preserved during window pruning,
     and whether this window has previously been attached to a task.  */
  public boolean preserve, previouslyAttached;

  /* The window manager name of this window, which supplies the name of
     activities in which it is displayed as a toplevel window, or
     NULL.  */
  public String wmName;

  public
  EmacsWindow (final EmacsWindow parent, int x, int y,
	       int width, int height, boolean overrideRedirect)
  {
    rect = new Rect (x, y, x + width, y + height);
    pointerMap = new SparseArray<Coordinate> ();

    /* Create the view from the context's UI thread.  The window is
       unmapped, so the view is GONE.  */
    view = EmacsService.SERVICE.getEmacsView (this, View.GONE,
					      parent == null);
    this.parent = parent;
    this.overrideRedirect = overrideRedirect;

    /* The initial frame should always be bound to the startup
       activity.  */
    if (!initialWindowCreated)
      {
	this.attachmentToken = -1;
        initialWindowCreated = true;
      }

    /* Create the list of children.  */
    children = new ArrayList<EmacsWindow> ();

    if (parent != null)
      {
	synchronized (parent.children)
	  {
	    parent.children.add (this);
	  }

        EmacsService.SERVICE.runOnUiThread (new Runnable () {
	    @Override
	    public void
	    run ()
	    {
	      parent.view.addView (view);
	    }
	  });
      }

    scratchGC = new EmacsGC ();

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

    dndXPosition = -1;
    dndYPosition = -1;
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
      {
	synchronized (parent.children)
	  {
	    parent.children.remove (this);
	  }
      }

    /* This is just a sanity test and is not reliable since `children'
       may be modified between isEmpty and handle destruction.  */
    if (!children.isEmpty ())
      throw new IllegalStateException ("Trying to destroy window with "
				       + "children!");

    /* Remove the view from its parent and make it invisible.  */
    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  ViewManager parent;
	  EmacsWindowManager manager;

	  /* Invalidate the focus; this should transfer the input focus
	     to the next eligible window as this window is no longer
	     present in parent.children.  */
	  EmacsActivity.invalidateFocus (4);

	  if (EmacsActivity.focusedWindow == EmacsWindow.this)
	    EmacsActivity.focusedWindow = null;

	  manager = EmacsWindowManager.MANAGER;
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
  setConsumer (EmacsWindowManager.WindowConsumer consumer)
  {
    attached = consumer;
  }

  public EmacsWindowManager.WindowConsumer
  getAttachedConsumer ()
  {
    return attached;
  }

  public synchronized long
  viewLayout (int left, int top, int right, int bottom)
  {
    int rectWidth, rectHeight;

    /* If this is an override-redirect window, don't ever modify
       rect.left and rect.top, as its WM window will always have been
       moved in unison with itself.  */

    if (overrideRedirect)
      {
	rect.right = rect.left + (right - left);
	rect.bottom = rect.top + (bottom - top);
      }
    /* If parent is null, use xPosition and yPosition instead of the
       geometry rectangle positions.  */
    else if (parent == null)
      {
	rect.left = xPosition;
	rect.top = yPosition;
	rect.right = rect.left + (right - left);
	rect.bottom = rect.top + (bottom - top);
      }
    /* Otherwise accept the new position offered by the toolkit.  FIXME:
       isn't there a potential race condition here if the toolkit lays
       out EmacsView after a child frame's rect is set but before it
       calls onLayout to read the modified rect?  */
    else
      {
	rect.left = left;
	rect.top = top;
	rect.right = right;
	rect.bottom = bottom;
      }

    rectWidth = right - left;
    rectHeight = bottom - top;

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
	    {
	      WindowManager.LayoutParams params;

	      /* Set the layout parameters again.  */
	      params = getWindowLayoutParams ();
	      view.setLayoutParams (params);

	      /* Announce this update to the window server.  */
	      if (windowManager != null)
		windowManager.updateViewLayout (view, params);
	    }

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

  public synchronized void
  moveResizeWindow (int x, int y, int width, int height)
  {
    rect.left = x;
    rect.top = y;
    rect.right = x + width;
    rect.bottom = y + height;
    requestViewLayout ();
  }

  /* Return WM layout parameters for an override redirect window with
     the geometry provided here.  */

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
    type = WindowManager.LayoutParams.TYPE_APPLICATION_PANEL;

    params
      = new WindowManager.LayoutParams (rect.width (), rect.height (),
					rect.left, rect.top,
					type, flags,
					PixelFormat.RGBA_8888);
    params.gravity = Gravity.TOP | Gravity.LEFT;
    return params;
  }

  private Activity
  findSuitableActivityContext ()
  {
    /* Find a recently focused activity.  */
    if (!EmacsActivity.focusedActivities.isEmpty ())
      return EmacsActivity.focusedActivities.get (0);

    /* Resort to the last activity to be focused.  */
    return EmacsActivity.lastFocusedActivity;
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
	      EmacsWindowManager manager;
	      WindowManager windowManager;
	      Activity ctx;
	      Object tem;
	      WindowManager.LayoutParams params;

	      /* Make the view visible, first of all.  */
	      view.setVisibility (View.VISIBLE);

	      if (!overrideRedirect)
		{
		  manager = EmacsWindowManager.MANAGER;

		  /* If parent is the root window, notice that there are new
		     children available for interested activities to pick
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

		  if (ctx == null)
		    {
		      Log.w (TAG, "failed to attach override-redirect window"
			     + " for want of activity");
		      return;
		    }

		  tem = ctx.getSystemService (Context.WINDOW_SERVICE);
		  windowManager = (WindowManager) tem;

		  /* Calculate layout parameters and propagate the
		     activity's token into it.  */

		  params = getWindowLayoutParams ();
		  params.token = (ctx.findViewById (android.R.id.content)
				  .getWindowToken ());
		  view.setLayoutParams (params);

		  /* Attach the view.  */
		  try
		    {
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
	  EmacsWindowManager manager;

	  manager = EmacsWindowManager.MANAGER;

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
    view.damageRect (damageRect.left,
		     damageRect.top,
		     damageRect.right,
		     damageRect.bottom);
  }

  @Override
  public void
  damageRect (int left, int top, int right, int bottom)
  {
    view.damageRect (left, top, right, bottom);
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
     input EVENT.  Replace bits representing Left or Right keys with
     their corresponding general modifier bits.  */

  public static int
  eventModifiers (KeyEvent event)
  {
    int state;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR2)
      state = KeyEvent.normalizeMetaState (event.getMetaState ());
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

  /* event.getCharacters is used because older input methods still
     require it.  */
  @SuppressWarnings ("deprecation")
  public boolean
  onKeyDown (int keyCode, KeyEvent event)
  {
    int state, state_1, extra_ignored, unicode_char;
    long serial;
    String characters;

    if (keyCode == KeyEvent.KEYCODE_BACK)
      {
	/* New Android systems display Back navigation buttons on a
	   row of virtual buttons at the bottom of the screen.  These
	   buttons function much as physical buttons do, in that key
	   down events are produced when a finger taps them, even if
	   the finger is not ultimately released after the OS's
	   gesture navigation is activated.

	   Deliver onKeyDown events in onKeyUp instead, so as not to
	   navigate backwards during gesture navigation.  */

	return true;
      }

    state = eventModifiers (event);

    /* Meta isn't supported by systems older than Android 3.0.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB)
      extra_ignored = KeyEvent.META_META_MASK;
    else
      extra_ignored = 0;

    /* Ignore meta-state understood by Emacs for now, or key presses
       such as Ctrl+C and Meta+C will not be recognized as ASCII key
       press events.  */

    state_1
      = state & ~(KeyEvent.META_ALT_MASK | KeyEvent.META_CTRL_MASK
		  | KeyEvent.META_SYM_ON | extra_ignored);

    /* There's no distinction between Right Alt and Alt Gr on Android,
       so restore META_ALT_RIGHT_ON if set in state to enable composing
       characters.  (bug#69321) */

    if ((state & KeyEvent.META_ALT_RIGHT_ON) != 0)
      {
	state_1 |= KeyEvent.META_ALT_ON | KeyEvent.META_ALT_RIGHT_ON;

	/* If Alt is also not depressed, remove its bit from the mask
	   reported to Emacs.  */
	if ((state & KeyEvent.META_ALT_LEFT_ON) == 0)
	  state &= ~KeyEvent.META_ALT_MASK;
      }

    unicode_char = getEventUnicodeChar (event, state_1);

    /* If a NUMPAD_ key is detected for which no character is returned,
       return false without sending the key event, as this will prompt
       the system to send an event with the corresponding action
       key.  */

    if (keyCode >= KeyEvent.KEYCODE_NUMPAD_0
	&& keyCode <= KeyEvent.KEYCODE_NUMPAD_RIGHT_PAREN
	&& unicode_char == 0)
      return false;

    synchronized (eventStrings)
      {
	serial
	  = EmacsNative.sendKeyPress (this.handle,
				      event.getEventTime (),
				      state, keyCode,
				      unicode_char);

	characters = event.getCharacters ();

	if (characters != null && characters.length () > 1)
	  saveUnicodeString ((int) serial, characters);
      }

    return true;
  }

  public boolean
  onKeyUp (int keyCode, KeyEvent event)
  {
    int state, state_1, unicode_char, extra_ignored;
    long time;

    /* Compute the event's modifier mask.  */
    state = eventModifiers (event);

    /* Meta isn't supported by systems older than Android 3.0.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB)
      extra_ignored = KeyEvent.META_META_MASK;
    else
      extra_ignored = 0;

    /* Ignore meta-state understood by Emacs for now, or key presses
       such as Ctrl+C and Meta+C will not be recognized as ASCII key
       press events.  */

    state_1
      = state & ~(KeyEvent.META_ALT_MASK | KeyEvent.META_CTRL_MASK
		  | KeyEvent.META_SYM_ON | extra_ignored);

    /* There's no distinction between Right Alt and Alt Gr on Android,
       so restore META_ALT_RIGHT_ON if set in state to enable composing
       characters.  */

    if ((state & KeyEvent.META_ALT_RIGHT_ON) != 0)
      {
	state_1 |= KeyEvent.META_ALT_ON | KeyEvent.META_ALT_RIGHT_ON;

	/* If Alt is also not depressed, remove its bit from the mask
	   reported to Emacs.  */
	if ((state & KeyEvent.META_ALT_LEFT_ON) == 0)
	  state &= ~KeyEvent.META_ALT_MASK;
      }

    unicode_char = getEventUnicodeChar (event, state_1);

    if (keyCode == KeyEvent.KEYCODE_BACK)
      {
	/* If the key press's been canceled, return immediately.  */

	if ((event.getFlags () & KeyEvent.FLAG_CANCELED) != 0)
	  return true;

	/* Dispatch the key press event that was deferred till now.  */
	EmacsNative.sendKeyPress (this.handle, event.getEventTime (),
				  state, keyCode, unicode_char);
      }
    /* If a NUMPAD_ key is detected for which no character is returned,
       return false without sending the key event, as this will prompt
       the system to send an event with the corresponding action
       key.  */
    else if (keyCode >= KeyEvent.KEYCODE_NUMPAD_0
	     && keyCode <= KeyEvent.KEYCODE_NUMPAD_RIGHT_PAREN
	     && unicode_char == 0)
      return false;

    EmacsNative.sendKeyRelease (this.handle, event.getEventTime (),
				state, keyCode, unicode_char);

    if (keyCode == EmacsNative.getQuitKeycode ())
      {
	/* Check if this volume down press should quit Emacs.
	   Most Android devices have no physical keyboard, so it
	   is unreasonably hard to press C-g.  */

	time = event.getEventTime ();

	if (time - lastQuitKeyRelease < 350)
	  EmacsNative.quit ();

        lastQuitKeyRelease = time;
      }

    return true;
  }

  public void
  onFocusChanged (boolean gainFocus)
  {
    EmacsActivity.invalidateFocus (gainFocus ? 6 : 5);
  }

  /* Notice that the activity (or its task) has been detached or
     destroyed by explicit user action.  */

  public void
  onActivityDetached ()
  {
    EmacsNative.sendWindowAction (this.handle, 0);
  }

  /* Dispatch a back gesture invocation as a KeyPress event.  Lamentably
     the platform does not appear to support reporting keyboard
     modifiers with these events.  */

  public void
  onBackInvoked ()
  {
    long time = SystemClock.uptimeMillis ();
    EmacsNative.sendKeyPress (this.handle, time, 0,
			      KeyEvent.KEYCODE_BACK, 0);
    EmacsNative.sendKeyRelease (this.handle, time, 0,
				KeyEvent.KEYCODE_BACK, 0);
  }



  /* Mouse and touch event handling.

     Android does not conceptually distinguish between mouse events
     (those coming from a device whose movement affects the on-screen
     pointer image) and touch screen events.  Each click or touch
     starts a single pointer gesture sequence, and subsequent motion
     of the device will result in updates being reported relative to
     that sequence until the mouse button or touch is released.

     When a touch, click, or pointer motion takes place, several kinds
     of event can be sent:

     ACTION_DOWN or ACTION_POINTER_DOWN is sent with a new coordinate
     and an associated ``pointer ID'' identifying the event and its
     gesture sequence when a click or touch takes place.  Emacs is
     responsible for recording both the position and pointer ID of
     this click for the purpose of determining future changes to its
     position.

     ACTION_UP or ACTION_POINTER_UP is sent with a pointer ID when the
     click associated with a previous ACTION_DOWN event is released.

     ACTION_CANCEL (or ACTION_POINTER_UP with FLAG_CANCELED) is sent
     if a similar situation transpires: the window system has chosen
     to grab the click, and future changes to its position will no
     longer be reported to Emacs.

     ACTION_MOVE is sent if a coordinate tied to a click that has not
     been released changes.  Emacs processes this event by comparing
     each of the coordinates within the event with its recollection of
     those contained within prior ACTION_DOWN and ACTION_MOVE events;
     the pointer ID of the differing coordinate is then reported
     within a touch or pointer motion event along with its new
     position.

     The events described above are all sent for both touch and mouse
     click events.  Determining whether an ACTION_DOWN event is
     associated with a button event is performed by inspecting the
     mouse button state associated with that event.  If it contains
     any mouse buttons that were not contained in the button state at
     the time of the last ACTION_DOWN or ACTION_UP event, the
     coordinate contained within is assumed to be a mouse click,
     leading to it and associated motion or ACTION_UP events being
     reported as mouse button or motion events.  Otherwise, those
     events are reported as touch screen events, with the touch ID set
     to the pointer ID.

     In addition to the events illustrated above, Android also sends
     several other types of event upon select types of activity from a
     mouse device:

     ACTION_HOVER_MOVE is sent with the coordinate of the mouse
     pointer if it moves above a frame prior to any click taking
     place.  Emacs sends a mouse motion event containing the
     coordinate.

     ACTION_HOVER_ENTER and ACTION_HOVER_LEAVE are respectively sent
     when the mouse pointer enters and leaves a frame.  Moreover,
     ACTION_HOVER_LEAVE events are sent immediately before an
     ACTION_DOWN event associated with a mouse click.  These
     extraneous events are distinct in that their button states always
     contain an additional button compared to the button state
     recorded at the time of the last ACTION_UP event.

     On Android 6.0 and later, ACTION_BUTTON_PRESS is sent with the
     coordinate of the mouse pointer if a mouse click occurs,
     alongside a ACTION_DOWN event.  ACTION_BUTTON_RELEASE is sent
     with the same information upon a mouse click being released, also
     accompanying an ACTION_UP event.

     However, both types of button events are implemented in a buggy
     fashion and cannot be used to report button events.  */

  /* Look through the button state to determine what button EVENT was
     generated from.  DOWN is true if EVENT is a button press event,
     false otherwise.  Value is the X number of the button.  */

  private int
  whatButtonWasIt (MotionEvent event, boolean down)
  {
    int eventState, notIn;

    /* Obtain the new button state.  */
    eventState = event.getButtonState ();

    /* Compute which button is now set or no longer set.  */

    notIn = (down ? eventState & ~lastButtonState
	     : lastButtonState & ~eventState);

    if ((notIn & (MotionEvent.BUTTON_PRIMARY
		  | MotionEvent.BUTTON_SECONDARY
		  | MotionEvent.BUTTON_TERTIARY)) == 0)
      /* No buttons have been pressed, so this is a touch event.  */
      return 0;

    if ((notIn & MotionEvent.BUTTON_PRIMARY) != 0)
      return 1;

    if ((notIn & MotionEvent.BUTTON_SECONDARY) != 0)
      return 3;

    if ((notIn & MotionEvent.BUTTON_TERTIARY) != 0)
      return 2;

    /* Buttons 4, 5, 6 and 7 are actually scroll wheels under X.
       Thus, report additional buttons starting at 8.  */

    if ((notIn & MotionEvent.BUTTON_BACK) != 0)
      return 8;

    if ((notIn & MotionEvent.BUTTON_FORWARD) != 0)
      return 9;

    /* Report stylus events as touch screen events.  */

    if ((notIn & MotionEvent.BUTTON_STYLUS_PRIMARY) != 0)
      return 0;

    if ((notIn & MotionEvent.BUTTON_STYLUS_SECONDARY) != 0)
      return 0;

    /* Not a real value.  */
    return 11;
  }

  /* Return the mouse button associated with the specified ACTION_DOWN
     or ACTION_POINTER_DOWN EVENT.

     Value is 0 if no mouse button was pressed, or the X number of
     that mouse button.  */

  private int
  buttonForEvent (MotionEvent event)
  {
    /* ICS and earlier don't support true mouse button events, so
       treat all down events as touch screen events.  */

    if (Build.VERSION.SDK_INT
	< Build.VERSION_CODES.ICE_CREAM_SANDWICH)
      return 0;

    return whatButtonWasIt (event, true);
  }

  /* Return the coordinate object associated with the specified
     EVENT, or null if it is not known.  */

  private Coordinate
  figureChange (MotionEvent event)
  {
    int i, truncatedX, truncatedY, pointerIndex, pointerID, count;
    Coordinate coordinate;

    /* Initialize this variable now.  */
    coordinate = null;

    switch (event.getActionMasked ())
      {
      case MotionEvent.ACTION_DOWN:
	/* Primary pointer pressed with index 0.  */

	pointerID = event.getPointerId (0);
	coordinate = new Coordinate ((int) event.getX (0),
				     (int) event.getY (0),
				     buttonForEvent (event),
				     pointerID);
	pointerMap.put (pointerID, coordinate);
	break;

      case MotionEvent.ACTION_UP:
      case MotionEvent.ACTION_CANCEL:
	/* Primary pointer released with index 0.  */
	pointerID = event.getPointerId (0);
	coordinate = pointerMap.get (pointerID);
	pointerMap.delete (pointerID);
	break;

      case MotionEvent.ACTION_POINTER_DOWN:
	/* New pointer.  Find the pointer ID from the index and place
	   it in the map.  */
	pointerIndex = event.getActionIndex ();
	pointerID = event.getPointerId (pointerIndex);
	coordinate = new Coordinate ((int) event.getX (pointerIndex),
				     (int) event.getY (pointerIndex),
				     buttonForEvent (event),
				     pointerID);
	pointerMap.put (pointerID, coordinate);
	break;

      case MotionEvent.ACTION_POINTER_UP:
	/* Pointer removed.  Remove it from the map.  */
	pointerIndex = event.getActionIndex ();
	pointerID = event.getPointerId (pointerIndex);
	coordinate = pointerMap.get (pointerID);
	pointerMap.delete (pointerID);
	break;

      default:

	/* Loop through each pointer in the event.  */

	count = event.getPointerCount ();
	for (i = 0; i < count; ++i)
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
	  }

	/* Set coordinate to NULL if the loop failed to find any
	   matching pointer.  */

	if (i == count)
	  coordinate = null;
      }

    /* Return the pointer ID.  */
    return coordinate;
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

  /* Process a single ACTION_DOWN, ACTION_POINTER_DOWN, ACTION_UP,
     ACTION_POINTER_UP, ACTION_CANCEL, or ACTION_MOVE event.

     Ascertain which coordinate changed and send an appropriate mouse
     or touch screen event.  */

  private void
  motionEvent (MotionEvent event)
  {
    Coordinate coordinate;
    int modifiers;
    long time;

    /* Find data associated with this event's pointer.  Namely, its
       current location, whether or not a change has taken place, and
       whether or not it is a button event.  */

    coordinate = figureChange (event);

    if (coordinate == null)
      return;

    time = event.getEventTime ();

    if (coordinate.button != 0)
      {
	/* This event is tied to a mouse click, so report mouse motion
	   and button events.  */

	modifiers = motionEventModifiers (event);

	switch (event.getAction ())
	  {
	  case MotionEvent.ACTION_POINTER_DOWN:
	  case MotionEvent.ACTION_DOWN:
	    EmacsNative.sendButtonPress (this.handle, coordinate.x,
					 coordinate.y, time, modifiers,
					 coordinate.button);
	    break;

	  case MotionEvent.ACTION_POINTER_UP:
	  case MotionEvent.ACTION_UP:
	  case MotionEvent.ACTION_CANCEL:
	    EmacsNative.sendButtonRelease (this.handle, coordinate.x,
					   coordinate.y, time, modifiers,
					   coordinate.button);
	    break;

	  case MotionEvent.ACTION_MOVE:
	    EmacsNative.sendMotionNotify (this.handle, coordinate.x,
					  coordinate.y, time);
	    break;
	  }
      }
    else
      {
	/* This event is a touch event, and the touch ID is the
	   pointer ID.  */

	switch (event.getActionMasked ())
	  {
	  case MotionEvent.ACTION_DOWN:
	  case MotionEvent.ACTION_POINTER_DOWN:
	    /* Touch down event.  */
	    EmacsNative.sendTouchDown (this.handle, coordinate.x,
				       coordinate.y, time,
				       coordinate.id, 0);
	    break;

	  case MotionEvent.ACTION_UP:
	  case MotionEvent.ACTION_POINTER_UP:
	    /* Touch up event.  */
	    EmacsNative.sendTouchUp (this.handle, coordinate.x,
				     coordinate.y, time,
				     coordinate.id, 0);
	    break;

	  case MotionEvent.ACTION_CANCEL:
	    /* Touch sequence cancellation event.  */
	    EmacsNative.sendTouchUp (this.handle, coordinate.x,
				     coordinate.y, time,
				     coordinate.id,
				     1 /* ANDROID_TOUCH_SEQUENCE_CANCELED */);
	    break;

	  case MotionEvent.ACTION_MOVE:
	    /* Pointer motion event.  */
	    EmacsNative.sendTouchMove (this.handle, coordinate.x,
				       coordinate.y, time,
				       coordinate.id, 0);
	    break;
	  }
      }

    if (Build.VERSION.SDK_INT
	< Build.VERSION_CODES.ICE_CREAM_SANDWICH)
      return;

    /* Now update the button state.  */
    lastButtonState = event.getButtonState ();
    return;
  }

  public boolean
  onTouchEvent (MotionEvent event)
  {
    switch (event.getActionMasked ())
      {
      case MotionEvent.ACTION_DOWN:
      case MotionEvent.ACTION_POINTER_DOWN:
      case MotionEvent.ACTION_UP:
      case MotionEvent.ACTION_POINTER_UP:
      case MotionEvent.ACTION_CANCEL:
      case MotionEvent.ACTION_MOVE:
	motionEvent (event);
	return true;
      }

    return false;
  }

  public boolean
  onGenericMotionEvent (MotionEvent event)
  {
    switch (event.getAction ())
      {
      case MotionEvent.ACTION_HOVER_ENTER:
	EmacsNative.sendEnterNotify (this.handle, (int) event.getX (),
				     (int) event.getY (),
				     event.getEventTime ());
	return true;

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

      case MotionEvent.ACTION_DOWN:
      case MotionEvent.ACTION_POINTER_DOWN:
      case MotionEvent.ACTION_UP:
      case MotionEvent.ACTION_POINTER_UP:
      case MotionEvent.ACTION_CANCEL:
      case MotionEvent.ACTION_MOVE:
	/* MotionEvents may either be sent to onGenericMotionEvent or
	   onTouchEvent depending on if Android thinks it is a mouse
	   event or not, but we detect them ourselves.  */
	motionEvent (event);
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
      {
	synchronized (parent.children)
	  {
	    parent.children.remove (this);
	  }
      }

    if (otherWindow != null)
      {
	synchronized (otherWindow.children)
	  {
	    otherWindow.children.add (this);
	  }
      }

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
	  EmacsWindowManager manager;
	  ViewManager parent;

	  /* Invalidate the focus; this should transfer the input focus
	     to the next eligible window as this window is no longer
	     present in parent.children.  */
	  EmacsActivity.invalidateFocus (7);

	  /* First, detach this window if necessary.  */
	  manager = EmacsWindowManager.MANAGER;
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

    synchronized (parent.children)
      {
	/* Remove and add this view again.  */
	parent.children.remove (this);
	parent.children.add (this);
      }

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

    synchronized (parent.children)
      {
	/* Remove and add this view again.  */
	parent.children.remove (this);
	parent.children.add (this);
      }

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

  public synchronized void
  reconfigure (final EmacsWindow window, final int stackMode)
  {
    ListIterator<EmacsWindow> iterator;
    EmacsWindow object;

    /* This does nothing here.  */
    if (parent == null)
      return;

    /* If window is NULL, call lower or upper subject to
       stackMode.  */

    if (window == null)
      {
	if (stackMode == 1) /* ANDROID_BELOW */
	  lower ();
	else
	  raise ();

	return;
      }

    /* Otherwise, if window.parent is distinct from this, return.  */
    if (window.parent != this.parent)
      return;

    /* Synchronize with the parent's child list.  Iterate over each
       item until WINDOW is encountered, before moving this window to
       the location prescribed by STACKMODE.  */

    synchronized (parent.children)
      {
	/* Remove this window from parent.children, for it will be
	   reinserted before or after WINDOW.  */
	parent.children.remove (this);

	/* Create an iterator.  */
	iterator = parent.children.listIterator ();

	while (iterator.hasNext ())
	  {
	    object = iterator.next ();

	    if (object == window)
	      {
		/* Now place this before or after the cursor of the
		   iterator.  */

		if (stackMode == 0) /* ANDROID_ABOVE */
		  iterator.add (this);
		else
		  {
		    iterator.previous ();
		    iterator.add (this);
		  }

		/* Effect the same adjustment upon the view
		   hierarchy.  */

		EmacsService.SERVICE.runOnUiThread (new Runnable () {
		    @Override
		    public void
		    run ()
		    {
		      if (stackMode == 0)
			view.moveAbove (window.view);
		      else
			view.moveBelow (window.view);
		    }
		  });
	      }
	  }

	/* parent.children does not list WINDOW, which should never
	   transpire.  */
	EmacsNative.emacsAbort ();
      }
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

  public void
  setWmName (final String wmName)
  {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP)
      return;

    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  EmacsActivity activity;
	  Object tem;

	  EmacsWindow.this.wmName = wmName;

	  /* If an activity is already attached, replace its task
	     description.  */

	  tem = getAttachedConsumer ();

	  if (tem != null && tem instanceof EmacsActivity)
	    {
	      activity = (EmacsActivity) tem;
	      activity.updateWmName ();
	    }
	}
      });
  }

  public int[]
  translateCoordinates (int x, int y)
  {
    int[] array;

    /* This is supposed to translate coordinates to the root window,
       whose origin point, in this context, is that of the toplevel
       activity host to this view.  */
    array = new int[2];
    EmacsService.SERVICE.getLocationInWindow (view, array);

    /* Now, the coordinates of the view should be in array.  Offset X
       and Y by them.  */
    array[0] += x;
    array[1] += y;

    /* In the case of an override redirect window, the WM window's
       extents and position match the Emacs window exactly.  */

    if (overrideRedirect)
      {
	synchronized (this)
	  {
	    array[0] += rect.left;
	    array[1] += rect.top;
	  }
      }

    /* Return the resulting coordinates.  */
    return array;
  }

  public void
  toggleOnScreenKeyboard (final boolean on)
  {
    FutureTask<Void> task;

    /* Even though InputMethodManager functions are thread safe,
       `showOnScreenKeyboard' etc must be called from the UI thread in
       order to avoid deadlocks if the calls happen in tandem with a
       call to a synchronizing function within
       `onCreateInputConnection'.  */

    task = new FutureTask<Void> (new Callable<Void> () {
	@Override
	public Void
	call ()
	{
	  if (on)
	    view.showOnScreenKeyboard ();
	  else
	    view.hideOnScreenKeyboard ();
	  return null;
	}
      });

    /* Block Lisp until this request to display the on-screen keyboard
       is registered by the UI thread, or updates arising from a
       redisplay that are reported between the two events will be liable
       to run afoul of the IMM's cache of selection positions and never
       reach the input method, if it is currently hidden, as input
       methods receive outdated selection information reported during
       the previous call to `onCreateInputConnection' when first
       displayed.

       Chances are this is a long-standing bug in the system.  */
    EmacsService.<Void>syncRunnable (task);
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

	  if (tem != null && tem instanceof EmacsActivity)
	    {
	      activity = (EmacsActivity) tem;
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



  /* Drag and drop.

     Android 7.0 and later permit multiple windows to be juxtaposed
     on-screen, consequently enabling items selected from one window
     to be dragged onto another.  Data is transferred across program
     boundaries using ClipData items, much the same way clipboard data
     is transferred.

     When an item is dropped, Emacs must ascertain whether the clip
     data represents plain text, a content URI incorporating a file,
     or some other data.  This is implemented by examining the clip
     data's ``description'', which enumerates each of the MIME data
     types the clip data is capable of providing data in.

     If the clip data represents plain text, then that text is copied
     into a string and conveyed to Lisp code.  Otherwise, Emacs must
     solicit rights to access the URI from the system, absent which it
     is accounted plain text and reinterpreted as such, to cue the
     user that something has gone awry.

     Moreover, events are regularly sent as the item being dragged
     travels across the frame, even if it might not be dropped.  This
     facilitates cursor motion and scrolling in response, as provided
     by the options dnd-indicate-insertion-point and
     dnd-scroll-margin.  */

  /* Register the drag and drop event EVENT.  */

  public boolean
  onDragEvent (DragEvent event)
  {
    ClipData data;
    ClipDescription description;
    int i, j, x, y, itemCount;
    String type, uriString;
    Uri uri;
    EmacsActivity activity;
    StringBuilder builder;
    ContentResolver resolver;

    x = (int) event.getX ();
    y = (int) event.getY ();

    switch (event.getAction ())
      {
      case DragEvent.ACTION_DRAG_STARTED:
	/* Return true to continue the drag and drop operation.  */
	return true;

      case DragEvent.ACTION_DRAG_LOCATION:
	/* Send this drag motion event to Emacs.  Skip this when the
	   integer position hasn't changed, for Android sends events
	   even if the movement from the previous position of the drag
	   is less than 1 pixel on either axis.  */

	if (x != dndXPosition || y != dndYPosition)
	  {
	    EmacsNative.sendDndDrag (handle, x, y);
	    dndXPosition = x;
	    dndYPosition = y;
	  }

	return true;

      case DragEvent.ACTION_DROP:
	/* Reset this view's record of the previous drag and drop
	   event's position.  */
	dndXPosition = -1;
	dndYPosition = -1;

	/* Judge whether this is plain text, or if it's a file URI for
	   which permissions must be requested.  */

	data = event.getClipData ();
	description = data.getDescription ();
	itemCount = data.getItemCount ();

	/* If there are insufficient items within the clip data,
	   return false.  */

	if (itemCount < 1)
	  return false;

	/* Search for plain text data within the clipboard.  */

	for (i = 0; i < description.getMimeTypeCount (); ++i)
	  {
	    type = description.getMimeType (i);

	    if (type.equals (ClipDescription.MIMETYPE_TEXT_PLAIN)
		|| type.equals (ClipDescription.MIMETYPE_TEXT_HTML))
	      {
		/* The data being dropped is plain text; encode it
		   suitably and send it to the main thread.  */
		type = (data.getItemAt (0).coerceToText (EmacsService.SERVICE)
			.toString ());
		EmacsNative.sendDndText (handle, x, y, type);
		return true;
	      }
	    else if (type.equals (ClipDescription.MIMETYPE_TEXT_URILIST))
	      {
		/* The data being dropped is a list of URIs; encode it
		   suitably and send it to the main thread.  */
		type = (data.getItemAt (0).coerceToText (EmacsService.SERVICE)
			.toString ());
		EmacsNative.sendDndUri (handle, x, y, type);
		return true;
	      }
	  }

	/* There's no plain text data within this clipboard item, so
	   each item within should be treated as a content URI
	   designating a file.  */

	/* Collect the URIs into a string with each suffixed
	   by newlines, much as in a text/uri-list.  */
	builder = new StringBuilder ();

	for (i = 0; i < itemCount; ++i)
	  {
	    /* If the item dropped is a URI, send it to the
	       main thread.  */

	    uri = data.getItemAt (i).getUri ();

	    /* Attempt to acquire permissions for this URI;
	       failing which, insert it as text instead.  */

	    if (uri != null
		&& uri.getScheme () != null
		&& uri.getScheme ().equals ("content")
		&& (activity = EmacsActivity.lastFocusedActivity) != null)
	      {
		if ((activity.requestDragAndDropPermissions (event) == null))
		  uri = null;
		else
		  {
		    resolver = activity.getContentResolver ();

		    /* Substitute a content file name for the URI, if
		       possible.  */
		    uriString = EmacsService.buildContentName (uri, resolver);

		    if (uriString != null)
		      {
			builder.append (uriString).append ("\n");
			continue;
		      }
		  }
	      }

	    if (uri != null)
	      builder.append (uri.toString ()).append ("\n");
	    else
	      {
		/* Treat each URI that Emacs cannot secure
		   permissions for as plain text.  */
		type = (data.getItemAt (i)
			.coerceToText (EmacsService.SERVICE)
			.toString ());
		EmacsNative.sendDndText (handle, x, y, type);
	      }
	  }

	/* Now send each URI to Emacs.  */

	if (builder.length () > 0)
	  EmacsNative.sendDndUri (handle, x, y, builder.toString ());
	return true;

      default:
	/* Reset this view's record of the previous drag and drop
	   event's position.  */
	dndXPosition = -1;
	dndYPosition = -1;
      }

    return true;
  }



  /* Miscellaneous functions for debugging graphics code.  */

  /* Recreate the activity to which this window is attached, if any.
     This is nonfunctional on Android 2.3.7 and earlier.  */

  public void
  recreateActivity ()
  {
    final EmacsWindowManager.WindowConsumer attached;

    attached = this.attached;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB)
      return;

    view.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  if (attached instanceof EmacsActivity)
	    ((EmacsActivity) attached).recreate ();
	}
      });
  }
};
