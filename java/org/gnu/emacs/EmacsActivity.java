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

import java.util.List;
import java.util.ArrayList;

import java.util.concurrent.TimeUnit;

import android.app.Activity;
import android.app.ActivityManager.TaskDescription;

import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;

import android.os.Build;
import android.os.Bundle;
import android.os.SystemClock;

import android.util.Log;

import android.net.Uri;

import android.view.Menu;
import android.view.View;
import android.view.ViewTreeObserver;
import android.view.Window;
import android.view.WindowInsets;
import android.view.WindowInsetsController;

import android.widget.FrameLayout;

import android.window.BackEvent;
import android.window.OnBackAnimationCallback;
import android.window.OnBackInvokedCallback;
import android.window.OnBackInvokedDispatcher;

public class EmacsActivity extends Activity
  implements EmacsWindowManager.WindowConsumer,
  ViewTreeObserver.OnGlobalLayoutListener
{
  public static final String TAG = "EmacsActivity";

  /* Key of intent value providing extra startup argument.  */
  public static final String EXTRA_STARTUP_ARGUMENTS;

  /* ID for URIs from a granted document tree.  */
  public static final int ACCEPT_DOCUMENT_TREE = 1;

  /* The currently attached EmacsWindow, or null if none.  */
  private EmacsWindow window;

  /* The frame layout associated with the activity.  */
  private FrameLayout layout;

  /* List of activities with focus.  */
  public static final List<EmacsActivity> focusedActivities;

  /* The last activity to have been focused.  */
  public static EmacsActivity lastFocusedActivity;

  /* The currently focused window.  */
  public static EmacsWindow focusedWindow;

  /* Whether or not this activity is paused.  */
  private boolean isStopped;

  /* Whether or not this activity is fullscreen.  */
  private boolean isFullscreen;

  /* The last context menu to be closed.  */
  private static Menu lastClosedMenu;

  /* The time of the most recent call to onStop.  */
  private static long timeOfLastInteraction;

  static
  {
    focusedActivities = new ArrayList<EmacsActivity> ();
    EXTRA_STARTUP_ARGUMENTS = "org.gnu.emacs.STARTUP_ARGUMENTS";
  };

  public static void
  invalidateFocus1 (EmacsWindow window, boolean resetWhenChildless)
  {
    if (window.view.isFocused ())
      focusedWindow = window;

    synchronized (window.children)
      {
	for (EmacsWindow child : window.children)
	  invalidateFocus1 (child, false);

	/* If no focused window was previously detected among WINDOW's
	   children and RESETWHENCHILDLESS is set (implying it is a
	   toplevel window), request that it be focused, to avoid
	   creating a situation where no windows exist focused or can be
	   transferred the input focus by user action.  */
	if (focusedWindow == null && resetWhenChildless)
	  {
	    window.view.requestFocus ();
	    focusedWindow = window;
	  }
      }
  }

  public static void
  invalidateFocus (int whence)
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
	  invalidateFocus1 (activity.window, focusedWindow == null);
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

	/* Reset the WM name.  */
	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
	  updateWmName ();

	invalidateFocus (0);
      }
  }

  @Override
  public final void
  attachWindow (EmacsWindow child)
  {
    FrameLayout.LayoutParams defaultParams;

    if (window != null)
      throw new IllegalStateException ("trying to attach window when one"
				       + " already exists");

    syncFullscreenWith (child);

    /* Record and attach the view.  */

    /* Reset residual LayoutParams that might remain in effect on this
       window, or some distributions of Android (e.g. Huawei HarmonyOS
       4.2) will retain the size of this window as a child frame.  */
    defaultParams
      = new FrameLayout.LayoutParams (FrameLayout.LayoutParams.MATCH_PARENT,
				      FrameLayout.LayoutParams.MATCH_PARENT);
    syncFullscreenWith (child);
    window = child;
    layout.addView (window.view, defaultParams);
    child.setConsumer (this);

    /* If the window isn't no-focus-on-map, focus its view.  */
    if (!child.getDontFocusOnMap ())
      window.view.requestFocus ();

    /* If the activity is iconified, send that to the window.  */
    if (isStopped)
      window.noticeIconified ();

    /* Invalidate the focus.  Since attachWindow may be called from
       either the main or the UI thread, post this to the UI thread.  */

    runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  invalidateFocus (1);
	}
      });

    /* Synchronize the window's window manager name with this activity's
       task in the recents list.  */
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
      updateWmName ();
  }

  @Override
  public final void
  destroy ()
  {
    if (window != null)
      {
	/* Clear the window's pointer to this activity and remove the
	   window's view.  */
	window.setConsumer (null);

	/* The window can't be iconified any longer.  */
	window.noticeDeiconified ();
	layout.removeView (window.view);
	window = null;
      }

    finish ();
  }

  @Override
  public final EmacsWindow
  getAttachedWindow ()
  {
    return window;
  }

  private void
  interceptBackGesture ()
  {
    OnBackInvokedDispatcher dispatcher;
    int priority = OnBackInvokedDispatcher.PRIORITY_DEFAULT;
    OnBackInvokedCallback callback;

    dispatcher = getOnBackInvokedDispatcher ();
    callback = new OnBackAnimationCallback () {
	@Override
	public void
	onBackInvoked ()
	{
	  View view = EmacsActivity.this.getCurrentFocus ();
	  EmacsWindow window;

	  if (view instanceof EmacsView)
	    {
	      window = ((EmacsView) view).window;
	      window.onBackInvoked ();
	    }
	}

	/* The three functions are overridden to prevent a misleading
	   back animation from being displayed, as Emacs intercepts all
	   back gestures and will not return to the home screen.  */

	@Override
	public void
	onBackCancelled ()
	{

	}

	@Override
	public void
	onBackProgressed (BackEvent gestureEvent)
	{

	}

	@Override
	public void
	onBackStarted (BackEvent gestureEvent)
	{

	}
    };
    dispatcher.registerOnBackInvokedCallback (priority, callback);
  }



  @Override
  public void
  onCreate (Bundle savedInstanceState)
  {
    FrameLayout.LayoutParams params;
    Intent intent;
    View decorView;
    ViewTreeObserver observer;
    int matchParent;

    /* See if Emacs should be started with any extra arguments, such
       as `--quick'.  */
    intent = getIntent ();
    EmacsService.extraStartupArguments
      = intent.getStringArrayExtra (EXTRA_STARTUP_ARGUMENTS);

    matchParent = FrameLayout.LayoutParams.MATCH_PARENT;
    params
      = new FrameLayout.LayoutParams (matchParent,
				      matchParent);

    /* Make the frame layout.  */
    layout = new FrameLayout (this);
    layout.setLayoutParams (params);

    /* Set it as the content view.  */
    setContentView (layout);

    /* Android 15 also realigns activity contents to originate beneath
       system windows, e.g. the navigation bar, so request the original
       behavior.  */
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.VANILLA_ICE_CREAM)
      layout.setFitsSystemWindows (true);

    /* Android 16 replaces KEYCODE_BACK with a callback registered at
       the window level.  */
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.BAKLAVA)
      interceptBackGesture ();

    /* Maybe start the Emacs service if necessary.  */
    EmacsService.startEmacsService (this);

    /* Add this activity to the list of available activities.  */
    EmacsWindowManager.MANAGER.registerWindowConsumer (this);

    /* Start observing global layout changes between Jelly Bean and Q.
       This is required to restore the fullscreen state whenever the
       on screen keyboard is displayed, as there is otherwise no way
       to determine when the on screen keyboard becomes visible.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN
	&& Build.VERSION.SDK_INT < Build.VERSION_CODES.R)
      {
	decorView = getWindow ().getDecorView ();
	observer = decorView.getViewTreeObserver ();
	observer.addOnGlobalLayoutListener (this);
      }

    super.onCreate (savedInstanceState);

    /* Call `onWindowFocusChanged' to read the focus state, which fails
       to be called after an activity is recreated.  */
    onWindowFocusChanged (false);
  }

  @Override
  public final void
  onGlobalLayout ()
  {
    syncFullscreenWith (window);
  }

  @Override
  public final void
  onStop ()
  {
    /* Iconification was previously reported in onPause, but that was
       misinformed, as `onStop' is the actual callback activated upon
       changes in an activity's visibility.  */
    isStopped = true;
    EmacsWindowManager.MANAGER.noticeIconified (this);

    timeOfLastInteraction = SystemClock.elapsedRealtime ();
    super.onStop ();
  }

  /* Return whether the task is being finished in response to explicit
     user action.  That is to say, Activity.isFinished, but as
     documented.  */

  public final boolean
  isReallyFinishing ()
  {
    long atime, dtime;
    int hours;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N)
      return isFinishing ();

    /* When the number of tasks retained in the recents list exceeds a
       threshold, Android 7 and later so destroy activities in trimming
       them from recents on the expiry of a timeout that isFinishing
       returns true, in direct contradiction to the documentation.  This
       timeout is generally 6 hours, but admits of customization by
       individual system distributors, so to err on the side of the
       caution, the timeout Emacs applies is a more conservative figure
       of 4 hours.  */

    if (timeOfLastInteraction == 0)
      return isFinishing ();

    atime = timeOfLastInteraction;

    /* Compare atime with the current system time.  */
    dtime = SystemClock.elapsedRealtime () - atime;
    if (dtime + 1000000 < TimeUnit.HOURS.toMillis (4))
      return isFinishing ();

    return false;
  }

  @Override
  public final void
  onDestroy ()
  {
    EmacsWindowManager manager;
    boolean isMultitask, reallyFinishing;

    manager = EmacsWindowManager.MANAGER;

    /* The activity will die shortly hereafter.  If there is a window
       attached, close it now.  */
    isMultitask = this instanceof EmacsMultitaskActivity;
    reallyFinishing = isReallyFinishing ();
    manager.removeWindowConsumer (this, isMultitask || reallyFinishing);
    focusedActivities.remove (this);
    invalidateFocus (2);

    /* Remove this activity from the static field, lest it leak.  */
    if (lastFocusedActivity == this)
      lastFocusedActivity = null;

    super.onDestroy ();
  }

  @Override
  public final void
  onWindowFocusChanged (boolean isFocused)
  {
    /* At times and on certain versions of Android ISFOCUSED does not
       reflect whether the window actually holds focus, so replace it
       with the value of `hasWindowFocus'.  */
    isFocused = hasWindowFocus ();

    if (isFocused)
      {
	if (!focusedActivities.contains (this))
	  focusedActivities.add (this);

	lastFocusedActivity = this;

	/* Update the window insets as the focus change may have
	   changed the window insets as well, and the system does not
	   automatically restore visibility flags.  */

	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN
	    && Build.VERSION.SDK_INT < Build.VERSION_CODES.R
	    && isFullscreen)
	  syncFullscreenWith (window);
      }
    else
      focusedActivities.remove (this);

    invalidateFocus (3);
  }

  @Override
  public final void
  onResume ()
  {
    isStopped = false;
    timeOfLastInteraction = 0;

    EmacsWindowManager.MANAGER.noticeDeiconified (this);
    super.onResume ();
  }

  @Override
  public final void
  onContextMenuClosed (Menu menu)
  {
    int serial;

    /* See the comment inside onMenuItemClick.  */

    if (((EmacsContextMenu.wasSubmenuSelected == -2)
	 || (EmacsContextMenu.wasSubmenuSelected >= 0
	     && ((System.currentTimeMillis ()
		  - EmacsContextMenu.wasSubmenuSelected)
		 <= 300)))
	|| menu == lastClosedMenu)
      {
	EmacsContextMenu.wasSubmenuSelected = -1;
	lastClosedMenu = menu;
	return;
      }

    /* lastClosedMenu is set because Android apparently calls this
       function twice.  */

    lastClosedMenu = null;

    /* Send a context menu event given that no menu item has already
       been selected.  */
    if (!EmacsContextMenu.itemAlreadySelected)
      {
	serial = EmacsContextMenu.lastMenuEventSerial;
	EmacsNative.sendContextMenu (0, 0, serial);
      }

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
	    flags = 0;
	    flags |= View.SYSTEM_UI_FLAG_FULLSCREEN;

	    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
	      {
		/* These flags means that Emacs will be full screen as
		   long as the state flag is set.  */
		flags |= View.SYSTEM_UI_FLAG_HIDE_NAVIGATION;
		flags |= View.SYSTEM_UI_FLAG_IMMERSIVE;
		flags |= View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY;
	      }

	    /* Apply the given flags.  */
	    view.setSystemUiVisibility (flags);
	  }
	else
	  view.setSystemUiVisibility (View.SYSTEM_UI_FLAG_VISIBLE);
      }
  }

  /* Update the name of this activity's task description from the
     current window, or reset the same if no window is attached.  */

  @SuppressWarnings ("deprecation")
  public final void
  updateWmName ()
  {
    String wmName;
    TaskDescription description;

    if (window == null || window.wmName == null)
      wmName = "Emacs";
    else
      wmName = window.wmName;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU)
      description = new TaskDescription (wmName);
    else
      description = (new TaskDescription.Builder ()
		     .setLabel (wmName).build ());
    setTaskDescription (description);
  }

  @Override
  public final void
  onAttachedToWindow ()
  {
    super.onAttachedToWindow ();

    /* Update the window insets.  */
    syncFullscreenWith (window);
  }

  @Override
  public final void
  onNewIntent (Intent intent)
  {
    String tag, action;

    /* This function is called when EmacsActivity is relaunched from a
       notification.  */

    if (intent == null || EmacsService.SERVICE == null)
      return;

    tag = intent.getStringExtra (EmacsDesktopNotification.NOTIFICATION_TAG);
    action
      = intent.getStringExtra (EmacsDesktopNotification.NOTIFICATION_ACTION);

    if (tag == null || action == null)
      return;

    EmacsNative.sendNotificationAction (tag, action);
  }

  @Override
  public long
  getAttachmentToken ()
  {
    return -1; /* This is overridden by EmacsMultitaskActivity.  */
  }



  @Override
  public final void
  onActivityResult (int requestCode, int resultCode, Intent data)
  {
    ContentResolver resolver;
    Uri uri;
    int flags;

    switch (requestCode)
      {
      case ACCEPT_DOCUMENT_TREE:

	/* A document granted through
	   EmacsService.requestDirectoryAccess.  */

	if (resultCode == RESULT_OK)
	  {
	    resolver = getContentResolver ();
	    uri = data.getData ();
	    flags = (Intent.FLAG_GRANT_READ_URI_PERMISSION
		     | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);

	    try
	      {
		if (uri != null)
		  resolver.takePersistableUriPermission (uri, flags);
	      }
	    catch (Exception exception)
	      {
		/* Permission to access URI might've been revoked in
		   between selecting the file and this callback being
		   invoked.  Don't crash in such cases.  */
	      }
	  }

	break;
      }
  }
};
