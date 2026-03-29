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

import java.util.ArrayList;
import java.util.List;

import android.app.ActivityManager.AppTask;
import android.app.ActivityManager.RecentTaskInfo;
import android.app.ActivityManager;
import android.app.ActivityOptions;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;

import android.os.Build;

import android.util.Log;

/* Code to paper over the differences in lifecycles between
   "activities" and windows.

   Four of the five interfaces to be implemented by an instance of this
   class are relevant on all versions of Android:

     registerWindowConsumer (WindowConsumer)
     registerWindow (EmacsWindow)
     removeWindowConsumer (WindowConsumer)
     removeWindow (EmacsWindow)

   A WindowConsumer is expected to allow an EmacsWindow to be attached
   to it, and be created or destroyed.

   Whenever a window is created, registerWindow examines the list of
   window consumers.  If a consumer exists and does not currently have a
   window of its own attached, it gets the new window, while otherwise,
   the window attachment manager starts a new consumer.  Whenever a
   consumer is registered, registerWindowConsumer checks the list of
   available windows.  If a window exists and is not currently attached
   to a consumer, then the consumer gets it.  Finally, every time a
   window is removed, the consumer is destroyed.

     getAttachmentToken ()

   should return a token uniquely identifying a consumer, which, on API
   21 and up, enables attributing the tasks of activities to the windows
   for which they were created, and with that, consistent interaction
   between user-visible window state and their underlying frames.  */

public final class EmacsWindowManager
{
  private static final String TAG = "EmacsWindowManager";
  public  final static String ACTIVITY_TOKEN = "emacs:activity_token";

  /* The single window attachment manager ``object''.  */
  public static final EmacsWindowManager MANAGER;

  /* Monotonically increasing counter from which multitasking activity
     tokens are produced.  */
  private static long nextActivityToken;

  /* The ActivityManager.  */
  private ActivityManager activityManager;

  static
  {
    MANAGER = new EmacsWindowManager ();
  };

  interface WindowConsumer
  {
    public void attachWindow (EmacsWindow window);
    public EmacsWindow getAttachedWindow ();
    public void detachWindow ();
    public void destroy ();
    public long getAttachmentToken ();
  };

  /* List of currently attached window consumers.  */
  public List<WindowConsumer> consumers;

  /* List of currently attached windows.  */
  public List<EmacsWindow> windows;

  public
  EmacsWindowManager ()
  {
    consumers = new ArrayList<WindowConsumer> ();
    windows = new ArrayList<EmacsWindow> ();
  }




  /* Return whether the provided WINDOW should be attached to the window
     consumer CONSUMER.  */

  public static boolean
  isWindowEligible (WindowConsumer consumer, EmacsWindow window)
  {
    return (/* The window has yet to be bound.  */
	    window.attachmentToken == 0
	    /* Or has already been bound to CONSUMER.  */
	    || (window.attachmentToken
		== consumer.getAttachmentToken ()));
  }



  public synchronized void
  registerWindowConsumer (WindowConsumer consumer)
  {
    consumers.add (consumer);
    pruneWindows ();

    for (EmacsWindow window : windows)
      {
	if (window.getAttachedConsumer () == null
	    /* Don't attach this window to CONSUMER if incompatible.  */
	    && isWindowEligible (consumer, window))
	  {
	    /* Permanently bind this window to the consumer.  */
	    window.attachmentToken = consumer.getAttachmentToken ();
	    window.previouslyAttached = true;
	    consumer.attachWindow (window);
	    return;
	  }
      }

    EmacsNative.sendWindowAction (0, 0);
  }

  public synchronized void
  registerWindow (EmacsWindow window)
  {
    Intent intent;
    ActivityOptions options;
    long token;

    if (windows.contains (window))
      /* The window is already registered.  */
      return;

    windows.add (window);

    for (WindowConsumer consumer : consumers)
      {
	if (consumer.getAttachedWindow () == null
	    && isWindowEligible (consumer, window))
	  {
	    /* Permanently bind this window to the consumer.  */
	    window.attachmentToken = consumer.getAttachmentToken ();
	    window.previouslyAttached = true;
	    consumer.attachWindow (window);
	    return;
	  }
      }

    /* Do not create a multitasking activity for the initial frame,
       but arrange to start EmacsActivity.  */
    if (window.attachmentToken == -1)
      {
	intent = new Intent (EmacsService.SERVICE,
			     EmacsActivity.class);
	intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK);

	try
	  {
	    EmacsService.SERVICE.startActivity (intent);
	  }
	catch (Exception e)
	  {
	    Log.w (TAG, "an activity could not be started on behalf"
		   + " of the mapped default window " + window.handle);
	  }

	return;
      }

    intent = new Intent (EmacsService.SERVICE,
			 EmacsMultitaskActivity.class);

    /* FLAG_ACTIVITY_MULTIPLE_TASK would appear appropriate, but that
       is not so: on Android 2.3 and earlier, this flag combined with
       FLAG_ACTIVITY_NEW_TASK prompts the task switcher to create a
       new instance of EmacsMultitaskActivity, rather than return to
       an existing instance, and is entirely redundant, inasmuch as
       only one multitasking task can exist at any given moment.  */
    intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK);

    /* Intent.FLAG_ACTIVITY_NEW_DOCUMENT is lamentably unavailable on
       older systems than Lollipop.  */
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
      {
	intent.addFlags (Intent.FLAG_ACTIVITY_NEW_DOCUMENT
			 | Intent.FLAG_ACTIVITY_MULTIPLE_TASK);

	/* Bind this window to the activity in advance, i.e., before its
	   creation, so that its ID will be recorded in the RecentTasks
	   list.  */
	token = ++nextActivityToken;
      }
    else
      /* APIs required for linking activities to windows are not
	 available in earlier Android versions.  */
      token = -2;

    window.attachmentToken = token;
    intent.putExtra (ACTIVITY_TOKEN, token);

    try
      {
	if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N)
	  EmacsService.SERVICE.startActivity (intent);
	else
	  {
	    /* Specify the desired window size.  */
	    options = ActivityOptions.makeBasic ();
	    options.setLaunchBounds (window.getGeometry ());
	    EmacsService.SERVICE.startActivity (intent, options.toBundle ());
	  }
      }
    catch (Exception e)
      {
	Log.w (TAG, "an activity could not be started on behalf"
	       + " of a mapped window, " + window.handle);
      }

    pruneWindows ();
  }

  public synchronized void
  removeWindowConsumer (WindowConsumer consumer, boolean isFinishing)
  {
    EmacsWindow window;

    window = consumer.getAttachedWindow ();

    if (window != null)
      {
	consumer.detachWindow ();

	/* Though pruneWindows will likely remove the same window(s),
	   call onActivityDetached anyway if isFinishing is set, if
	   CONSUMER not be a multitasking activity, as in obscure
	   circumstances pruneWindows will not remove frames bound to
	   the system-started task.  */
	if (isFinishing
	    && (!(consumer instanceof EmacsMultitaskActivity)
		|| Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP))
	  window.onActivityDetached ();
      }

    pruneWindows ();
    consumers.remove (consumer);
  }

  public synchronized void
  detachWindow (EmacsWindow window)
  {
    WindowConsumer consumer;

    /* Reset window management state.  */
    window.previouslyAttached = false;
    window.attachmentToken = 0;

    /* Remove WINDOW from the list of active windows.  */
    windows.remove (window);

    if ((consumer = window.getAttachedConsumer ()) != null)
      {
	consumers.remove (consumer);
	consumer.destroy ();
      }

    pruneWindows ();
  }

  public void
  noticeIconified (WindowConsumer consumer)
  {
    EmacsWindow window;

    /* If a window is attached, send the appropriate iconification
       events.  */
    window = consumer.getAttachedWindow ();

    if (window != null)
      window.noticeIconified ();
  }

  public void
  noticeDeiconified (WindowConsumer consumer)
  {
    EmacsWindow window;

    /* If a window is attached, send the appropriate iconification
       events.  */
    window = consumer.getAttachedWindow ();

    if (window != null)
      window.noticeDeiconified ();
  }

  public synchronized List<EmacsWindow>
  copyWindows ()
  {
    return new ArrayList<EmacsWindow> (windows);
  }



  /* Return the activity token specified in the intent giving rise to
     TASK, or 0 if absent.  */

  private static long
  getTaskToken (AppTask task)
  {
    RecentTaskInfo info;

    info = task.getTaskInfo ();

    /* baseIntent is a member of info's superclass, TaskInfo, on Android
       10 and later.  Prior to this release, it had been a member of
       RecentTaskInfo since SDK 1, and whatever the misleading
       documentation might suggest, a reference to `baseIntent' through
       TaskInfo is just as good a reference to RecentTaskInfo.  */
    return (info.baseIntent != null
	    ? info.baseIntent.getLongExtra (ACTIVITY_TOKEN, -1l)
	    : 0);
  }

  /* Iterate over each of Emacs's tasks and remove remaining registered
     windows whose tasks no longer exist.  This function should be
     called upon any event that could plausibly indicate changes in the
     task list or as to window management.  */

  private synchronized void
  pruneWindows ()
  {
    Object object;
    List<AppTask> appTasks;
    long taskToken;
    boolean set;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP
	|| EmacsService.SERVICE == null)
      return;

    if (activityManager == null)
      {
	object
	  = EmacsService.SERVICE.getSystemService (Context.ACTIVITY_SERVICE);
	activityManager = (ActivityManager) object;
      }

    appTasks = activityManager.getAppTasks ();

    /* Clear the preserve flag on all toplevel windows.  */

    for (EmacsWindow window : windows)
      window.preserve = false;

    for (AppTask task : appTasks)
      {
	taskToken = getTaskToken (task);
	set = false;

	if (taskToken == 0)
	  continue;

	/* Search for a window with this token.  */
	for (EmacsWindow window : windows)
	  {
	    if (window.attachmentToken == taskToken)
	      {
		window.preserve = true;
		set = true;
	      }
	  }

	if (!set)
	  task.finishAndRemoveTask ();
      }

    /* Now remove toplevel windows without activity tasks.  */

    for (EmacsWindow window : windows)
      {
	if (window.preserve
	    /* This is not the initial window.  */
	    || (window.attachmentToken < 1)
	    /* Nor has it never been attached.  */
	    || !window.previouslyAttached)
	  continue;

	window.onActivityDetached ();
      }
  }

  /* Iterate over each of Emacs's tasks to delete such as belong to a
     previous Emacs session, i.e., tasks created for a previous
     session's non-initial frames.  CONTEXT should be a context from
     which to obtain a reference to the activity manager.  */

  public void
  removeOldTasks (Context context)
  {
    List<AppTask> appTasks;
    RecentTaskInfo info;
    ComponentName name;
    String target;
    Object object;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP)
      return;

    if (activityManager == null)
      {
	object = context.getSystemService (Context.ACTIVITY_SERVICE);
	activityManager = (ActivityManager) object;
      }

    appTasks = activityManager.getAppTasks ();
    target   = ".EmacsMultitaskActivity";

    for (AppTask task : appTasks)
      {
	info = task.getTaskInfo ();

	/* Test whether info is a reference to
	   EmacsMultitaskActivity.  */
	if (info.baseIntent != null
	    && (name = info.baseIntent.getComponent ()) != null
	    && name.getShortClassName ().equals (target))
	  /* Delete the task.  */
	  task.finishAndRemoveTask ();
      }
  }
};
