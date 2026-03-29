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

import android.app.Notification;
import android.app.NotificationManager;
import android.app.NotificationChannel;
import android.app.PendingIntent;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import android.net.Uri;

import android.os.Build;

import android.widget.RemoteViews;



/* Structure designating a single desktop notification.

   New versions of Android also organize notifications into individual
   ``channels'', which are used to implement groups.  Unlike on other
   systems, notification importance is set for each group, not for
   each individual notification.  */



public final class EmacsDesktopNotification
{
  /* Intent tag for notification action data.  */
  public static final String NOTIFICATION_ACTION = "emacs:notification_action";

  /* Intent tag for notification IDs.  */
  public static final String NOTIFICATION_TAG = "emacs:notification_tag";

  /* Action ID assigned to the broadcast receiver which should be
     notified of any notification's being dismissed.  */
  public static final String NOTIFICATION_DISMISSED = "org.gnu.emacs.DISMISSED";

  /* The content of this desktop notification.  */
  public final String content;

  /* The title of this desktop notification.  */
  public final String title;

  /* The notification group.  */
  public final String group;

  /* String identifying this notification for future replacement.
     Typically a string resembling ``XXXX.NNNN.YYYY'', where XXXX is
     the system boot time, NNNN is the PID of this Emacs instance, and
     YYYY is the counter value returned by the notifications display
     function.  */
  public final String tag;

  /* The identifier of this notification's icon.  */
  public final int icon;

  /* The importance of this notification's group.  */
  public final int importance;

  /* Array of actions and their user-facing text to be offered by this
     notification.  */
  public final String[] actions, titles;

  /* Delay in milliseconds after which this notification should be
     automatically dismissed.  */
  public final long delay;

  public
  EmacsDesktopNotification (String title, String content,
			    String group, String tag, int icon,
			    int importance,
			    String[] actions, String[] titles,
			    long delay)
  {
    this.content    = content;
    this.title	    = title;
    this.group	    = group;
    this.tag        = tag;
    this.icon       = icon;
    this.importance = importance;
    this.actions    = actions;
    this.titles     = titles;
    this.delay      = delay;
  }



  /* Functions for displaying desktop notifications.  */

  /* Insert each action in actions and titles into the notification
     builder BUILDER, with pending intents created with CONTEXT holding
     suitable metadata.  */

  @SuppressWarnings ("deprecation")
  private void
  insertActions (Context context, Notification.Builder builder)
  {
    int i;
    PendingIntent pending;
    Intent intent;
    Notification.Action.Builder action;

    if (actions == null)
      return;

    for (i = 0; i < actions.length; ++i)
      {
	/* Actions named default should not be displayed.  */
	if (actions[i].equals ("default"))
	  continue;

	intent = new Intent (context, EmacsActivity.class);
	intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK);

	/* Pending intents are specific to combinations of class, action
	   and data, but not information provided as extras.  In order
	   that its target may be invoked with the action and tag set
	   below, generate a URL from those two elements and specify it
	   as the intent data, which ensures that the intent allocated
	   fully reflects the duo.  */

	intent.setData (new Uri.Builder ().scheme ("action")
			.appendPath (tag).appendPath (actions[i])
			.build ());
	intent.putExtra (NOTIFICATION_ACTION, actions[i]);
	intent.putExtra (NOTIFICATION_TAG, tag);

	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S)
	  pending = PendingIntent.getActivity (context, 0, intent,
					       PendingIntent.FLAG_IMMUTABLE);
	else
	  pending = PendingIntent.getActivity (context, 0, intent, 0);

	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M)
	  {
	    action = new Notification.Action.Builder (0, titles[i], pending);
	    builder.addAction (action.build ());
	  }
	else
	  builder.addAction (0, titles[i], pending);
      }
  }

  /* Internal helper for `display' executed on the main thread.  */

  @SuppressWarnings ("deprecation") /* Notification.Builder (Context).  */
  private void
  display1 (Context context)
  {
    NotificationManager manager;
    NotificationChannel channel;
    Notification notification;
    Object tem;
    RemoteViews contentView;
    Intent intent;
    PendingIntent pending;
    int priority;
    Notification.Builder builder;

    tem = context.getSystemService (Context.NOTIFICATION_SERVICE);
    manager = (NotificationManager) tem;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
      {
	/* Create the notification channel for this group.  If a group
	   already exists with the same name, its linked attributes
	   (such as its importance) will be overridden.  */
        channel = new NotificationChannel (group, group, importance);
	manager.createNotificationChannel (channel);
	builder = new Notification.Builder (context, group);

	/* Create and configure a notification object and display
	   it.  */

	builder.setContentTitle (title);
	builder.setContentText (content);
	builder.setSmallIcon (icon);
	builder.setTimeoutAfter (delay);

	insertActions (context, builder);
	notification = builder.build ();
      }
    else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB)
      {
	/* Android 7.1 and earlier don't segregate notifications into
	   distinct categories, but permit an importance to be
	   assigned to each individual notification.  */

	builder = new Notification.Builder (context);
	builder.setContentTitle (title);
	builder.setContentText (content);
	builder.setSmallIcon (icon);

	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)
	  {
	    switch (importance)
	      {
	      case 2: /* IMPORTANCE_LOW */
	      default:
		priority = Notification.PRIORITY_LOW;
		break;

	      case 3: /* IMPORTANCE_DEFAULT */
		priority = Notification.PRIORITY_DEFAULT;
		break;

	      case 4: /* IMPORTANCE_HIGH */
		priority = Notification.PRIORITY_HIGH;
		break;
	      }

	    builder.setPriority (priority);
	    insertActions (context, builder);
	    notification = builder.build ();
	  }
	else
	  notification = builder.getNotification ();
      }
    else
      {
	notification = new Notification ();
	notification.icon = icon;

	/* This remote widget tree is defined in
	   java/res/layout/sdk8_notifications_view.xml.  */
	notification.contentView
	  = contentView
	  = new RemoteViews ("org.gnu.emacs",
			     R.layout.sdk8_notifications_view);
	contentView.setTextViewText (R.id.sdk8_notifications_title,
				     title);
	contentView.setTextViewText (R.id.sdk8_notifications_content,
				     content);
      }

    /* Provide a content intent which starts Emacs when the
       notification is clicked.  */

    intent = new Intent (context, EmacsActivity.class);
    intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK);
    intent.setData (new Uri.Builder ()
		    .scheme ("action")
		    .appendPath (tag)
		    .build ());
    intent.putExtra (NOTIFICATION_ACTION, "default");
    intent.putExtra (NOTIFICATION_TAG, tag);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S)
      pending = PendingIntent.getActivity (context, 0, intent,
					   PendingIntent.FLAG_IMMUTABLE);
    else
      pending = PendingIntent.getActivity (context, 0, intent, 0);

    notification.contentIntent = pending;

    /* Provide a cancellation intent to respond to notification
       dismissals.  */

    intent = new Intent (context, CancellationReceiver.class);
    intent.setAction (NOTIFICATION_DISMISSED);
    intent.setPackage ("org.gnu.emacs");
    intent.setData (new Uri.Builder ()
		    .scheme ("action")
		    .appendPath (tag)
		    .build ());
    intent.putExtra (NOTIFICATION_TAG, tag);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S)
      pending = PendingIntent.getBroadcast (context, 0, intent,
					    PendingIntent.FLAG_IMMUTABLE);
    else
      pending = PendingIntent.getBroadcast (context, 0, intent, 0);

    notification.deleteIntent = pending;
    manager.notify (tag, 2, notification);
  }

  /* Display this desktop notification.

     Create a notification channel named GROUP or update its
     importance if such a channel is already defined.  */

  public void
  display ()
  {
    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  display1 (EmacsService.SERVICE);
	}
      });
  }



  /* Broadcast receiver.  This is something of a system-wide callback
     arranged to be invoked whenever a notification posted by Emacs is
     dismissed, in order to relay news of its dismissal to
     androidselect.c and run or remove callbacks as appropriate.  */

  public static class CancellationReceiver extends BroadcastReceiver
  {
    @Override
    public void
    onReceive (Context context, Intent intent)
    {
      String tag, action;

      if (intent == null || EmacsService.SERVICE == null)
	return;

      tag = intent.getStringExtra (NOTIFICATION_TAG);

      if (tag == null)
	return;

      EmacsNative.sendNotificationDeleted (tag);
    }
  };
};
