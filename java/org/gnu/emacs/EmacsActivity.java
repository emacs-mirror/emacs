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
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.Window;
import android.view.WindowInsets;
import android.view.WindowInsetsController;
import android.widget.FrameLayout.LayoutParams;
import android.widget.FrameLayout;

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

  /* Whether or not this activity is fullscreen.  */
  private boolean isFullscreen;

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
  public final void
  detachWindow ()
  {
    syncFullscreenWith (null);

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
  public final void
  attachWindow (EmacsWindow child)
  {
    Log.d (TAG, "attachWindow: " + child);

    if (window != null)
      throw new IllegalStateException ("trying to attach window when one"
				       + " already exists");

    syncFullscreenWith (child);

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
  public final void
  destroy ()
  {
    finish ();
  }

  @Override
  public final EmacsWindow
  getAttachedWindow ()
  {
    return window;
  }

  @Override
  public final void
  onCreate (Bundle savedInstanceState)
  {
    FrameLayout.LayoutParams params;
    Intent intent;

    /* See if Emacs should be started with -Q.  */
    intent = getIntent ();
    EmacsService.needDashQ
      = intent.getBooleanExtra ("org.gnu.emacs.START_DASH_Q",
				false);

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
  public final void
  onWindowFocusChanged (boolean isFocused)
  {
    Log.d (TAG, ("onWindowFocusChanged: "
		 + (isFocused ? "YES" : "NO")));

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
  public final void
  onPause ()
  {
    isPaused = true;

    EmacsWindowAttachmentManager.MANAGER.noticeIconified (this);
    super.onPause ();
  }

  @Override
  public final void
  onResume ()
  {
    isPaused = false;

    /* Update the window insets.  */
    syncFullscreenWith (window);

    EmacsWindowAttachmentManager.MANAGER.noticeDeiconified (this);
    super.onResume ();
  }

  @Override
  public final void
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

  @SuppressWarnings ("deprecation")
  public final void
  syncFullscreenWith (EmacsWindow emacsWindow)
  {
    WindowInsetsController controller;
    Window window;
    int behavior, flags;
    View view;

    if (emacsWindow != null)
      isFullscreen = emacsWindow.fullscreen;
    else
      isFullscreen = false;

    /* On Android 11 or later, use the window insets controller to
       control whether or not the view is fullscreen.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R)
      {
	window = getWindow ();

	/* If there is no attached window, return immediately.  */
	if (window == null)
	  return;

	behavior = WindowInsetsController.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE;
	controller = window.getInsetsController ();
	controller.setSystemBarsBehavior (behavior);

	if (isFullscreen)
	  controller.hide (WindowInsets.Type.statusBars ()
			   | WindowInsets.Type.navigationBars ());
	else
	  controller.show (WindowInsets.Type.statusBars ()
			   | WindowInsets.Type.navigationBars ());

	return;
      }

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)
      {
	/* On Android 4.1 or later, use `setSystemUiVisibility'.  */

	window = getWindow ();

	if (window == null)
	  return;

	view = window.getDecorView ();

	if (isFullscreen)
	  {
	    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.KITKAT)
	      /* This flag means that Emacs will be full screen, but
		 the system will cancel the full screen state upon
		 switching to another program.  */
	      view.setSystemUiVisibility (View.SYSTEM_UI_FLAG_FULLSCREEN);
	    else
	      {
		/* These flags means that Emacs will be full screen as
		   long as the state flag is set.  */
		flags = 0;
		flags |= View.SYSTEM_UI_FLAG_FULLSCREEN;
		flags |= View.SYSTEM_UI_FLAG_HIDE_NAVIGATION;
		flags |= View.SYSTEM_UI_FLAG_IMMERSIVE;
		flags |= View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY;
		view.setSystemUiVisibility (flags);
	      }
	  }
	else
	  view.setSystemUiVisibility (View.SYSTEM_UI_FLAG_VISIBLE);
      }
  }

  @Override
  public final void
  onAttachedToWindow ()
  {
    super.onAttachedToWindow ();

    /* Update the window insets.  */
    syncFullscreenWith (window);
  }
};
