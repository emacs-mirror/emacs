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
import android.util.Log;
import android.widget.FrameLayout;
import android.widget.FrameLayout.LayoutParams;

public class EmacsActivity extends Activity
  implements EmacsWindowAttachmentManager.WindowConsumer
{
  public static final String TAG = "EmacsActivity";

  /* The currently attached EmacsWindow, or null if none.  */
  private EmacsWindow window;

  /* The frame layout associated with the activity.  */
  private FrameLayout layout;

  /* List of activities with focus.  */
  private static List<EmacsActivity> focusedActivities;

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

    /* Set the theme to one without a title bar.  */
    setTheme (android.R.style.Theme_NoTitleBar);

    params = new FrameLayout.LayoutParams (LayoutParams.MATCH_PARENT,
					   LayoutParams.MATCH_PARENT);

    /* Make the frame layout.  */
    layout = new FrameLayout (this);
    layout.setLayoutParams (params);

    /* Set it as the content view.  */
    setContentView (layout);

    if (EmacsService.SERVICE == null)
      /* Start the Emacs service now.  */
      startService (new Intent (this, EmacsService.class));

    /* Add this activity to the list of available activities.  */
    EmacsWindowAttachmentManager.MANAGER.registerWindowConsumer (this);

    super.onCreate (savedInstanceState);
  }

  @Override
  public void
  onDestroy ()
  {
    /* The activity will die shortly hereafter.  If there is a window
       attached, close it now.  */
    Log.d (TAG, "onDestroy " + this);
    EmacsWindowAttachmentManager.MANAGER.removeWindowConsumer (this);
    focusedActivities.remove (this);
    invalidateFocus ();
    super.onDestroy ();
  }

  @Override
  public void
  onWindowFocusChanged (boolean isFocused)
  {
    if (isFocused && !focusedActivities.contains (this))
      focusedActivities.add (this);
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
    super.onResume ();
  }

  @Override
  public void
  onResume ()
  {
    isPaused = false;

    EmacsWindowAttachmentManager.MANAGER.noticeDeiconified (this);
    super.onResume ();
  }
};
