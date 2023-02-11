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
import java.util.List;
import java.util.ArrayList;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.Build;
import android.util.Log;
import android.widget.FrameLayout;
import android.widget.FrameLayout.LayoutParams;
import android.view.Menu;

public class EmacsActivity extends Activity
  implements EmacsWindowAttachmentManager.WindowConsumer
{
  public static final String TAG = "EmacsActivity";

  /* The currently attached EmacsWindow, or null if none.  */
  private EmacsWindow window;

  /* The frame layout associated with the activity.  */
  private FrameLayout layout;

  /* List of activities with focus.  */
  public static List<EmacsActivity> focusedActivities;

  /* The last activity to have been focused.  */
  public static EmacsActivity lastFocusedActivity;

  /* The currently focused window.  */
  public static EmacsWindow focusedWindow;

  /* Whether or not this activity is paused.  */
  private boolean isPaused;

  static
  {
    focusedActivities = new ArrayList<EmacsActivity> ();
  };

  public static void
  invalidateFocus1 (EmacsWindow window)
  {
    if (window.view.isFocused ())
      focusedWindow = window;

    for (EmacsWindow child : window.children)
      invalidateFocus1 (child);
  }

  public static void
  invalidateFocus ()
  {
    EmacsWindow oldFocus;

    /* Walk through each focused activity and assign the window focus
       to the bottom-most focused window within.  Record the old focus
       as well.  */
    oldFocus = focusedWindow;
    focusedWindow = null;

    for (EmacsActivity activity : focusedActivities)
      {
	if (activity.window != null)
	  invalidateFocus1 (activity.window);
      }

    /* Send focus in- and out- events to the previous and current
       focus.  */

    if (oldFocus != null)
      EmacsNative.sendFocusOut (oldFocus.handle,
				System.currentTimeMillis ());

    if (focusedWindow != null)
      EmacsNative.sendFocusIn (focusedWindow.handle,
			       System.currentTimeMillis ());
  }

  @Override
  public void
  detachWindow ()
  {
    if (window == null)
      Log.w (TAG, "detachWindow called, but there is no window");
    else
      {
	/* Clear the window's pointer to this activity and remove the
	   window's view.  */
	window.setConsumer (null);

	/* The window can't be iconified any longer.  */
	window.noticeDeiconified ();
	layout.removeView (window.view);
	window = null;

	invalidateFocus ();
      }
  }

  @Override
  public void
  attachWindow (EmacsWindow child)
  {
    Log.d (TAG, "attachWindow: " + child);

    if (window != null)
      throw new IllegalStateException ("trying to attach window when one"
				       + " already exists");

    /* Record and attach the view.  */

    window = child;
    layout.addView (window.view);
    child.setConsumer (this);

    /* If the window isn't no-focus-on-map, focus its view.  */
    if (!child.getDontFocusOnMap ())
      window.view.requestFocus ();

    /* If the activity is iconified, send that to the window.  */
    if (isPaused)
      window.noticeIconified ();

    /* Invalidate the focus.  */
    invalidateFocus ();
  }

  @Override
  public void
  destroy ()
  {
    finish ();
  }

  @Override
  public EmacsWindow
  getAttachedWindow ()
  {
    return window;
  }

  @Override
  public void
  onCreate (Bundle savedInstanceState)
  {
    FrameLayout.LayoutParams params;
    Intent intent;

    /* See if Emacs should be started with -Q.  */
    intent = getIntent ();
    EmacsService.needDashQ
      = intent.getBooleanExtra ("org.gnu.emacs.START_DASH_Q",
				false);

    /* Set the theme to one without a title bar.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH)
      setTheme (android.R.style.Theme_DeviceDefault_NoActionBar);
    else
      setTheme (android.R.style.Theme_NoTitleBar);

    params = new FrameLayout.LayoutParams (LayoutParams.MATCH_PARENT,
					   LayoutParams.MATCH_PARENT);

    /* Make the frame layout.  */
    layout = new FrameLayout (this);
    layout.setLayoutParams (params);

    /* Set it as the content view.  */
    setContentView (layout);

    /* Maybe start the Emacs service if necessary.  */
    EmacsService.startEmacsService (this);

    /* Add this activity to the list of available activities.  */
    EmacsWindowAttachmentManager.MANAGER.registerWindowConsumer (this);

    super.onCreate (savedInstanceState);
  }

  @Override
  public void
  onDestroy ()
  {
    EmacsWindowAttachmentManager manager;
    boolean isMultitask;

    manager = EmacsWindowAttachmentManager.MANAGER;

    /* The activity will die shortly hereafter.  If there is a window
       attached, close it now.  */
    Log.d (TAG, "onDestroy " + this);
    isMultitask = this instanceof EmacsMultitaskActivity;
    manager.removeWindowConsumer (this, isMultitask || isFinishing ());
    focusedActivities.remove (this);
    invalidateFocus ();

    /* Remove this activity from the static field, lest it leak.  */
    if (lastFocusedActivity == this)
      lastFocusedActivity = null;

    super.onDestroy ();
  }

  @Override
  public void
  onWindowFocusChanged (boolean isFocused)
  {
    if (isFocused && !focusedActivities.contains (this))
      {
	focusedActivities.add (this);
	lastFocusedActivity = this;
      }
    else
      focusedActivities.remove (this);

    invalidateFocus ();
  }

  @Override
  public void
  onPause ()
  {
    isPaused = true;

    EmacsWindowAttachmentManager.MANAGER.noticeIconified (this);
    super.onPause ();
  }

  @Override
  public void
  onResume ()
  {
    isPaused = false;

    EmacsWindowAttachmentManager.MANAGER.noticeDeiconified (this);
    super.onResume ();
  }

  @Override
  public void
  onContextMenuClosed (Menu menu)
  {
    Log.d (TAG, "onContextMenuClosed: " + menu);

    /* See the comment inside onMenuItemClick.  */
    if (EmacsContextMenu.wasSubmenuSelected
	&& menu.toString ().contains ("ContextMenuBuilder"))
      return;

    /* Send a context menu event given that no menu item has already
       been selected.  */
    if (!EmacsContextMenu.itemAlreadySelected)
      EmacsNative.sendContextMenu ((short) 0, 0);

    super.onContextMenuClosed (menu);
  }
};
