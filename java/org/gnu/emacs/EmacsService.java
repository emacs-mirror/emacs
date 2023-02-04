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

import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

import android.graphics.Canvas;
import android.graphics.Bitmap;
import android.graphics.Point;

import android.view.View;
import android.view.InputDevice;
import android.view.KeyEvent;

import android.annotation.TargetApi;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.NotificationChannel;
import android.app.PendingIntent;
import android.app.Service;

import android.content.ClipboardManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.ApplicationInfoFlags;
import android.content.pm.PackageManager;
import android.content.res.AssetManager;

import android.net.Uri;

import android.os.Build;
import android.os.Looper;
import android.os.IBinder;
import android.os.Handler;
import android.os.Vibrator;
import android.os.VibratorManager;
import android.os.VibrationEffect;

import android.util.Log;
import android.util.DisplayMetrics;

import android.hardware.input.InputManager;

class Holder<T>
{
  T thing;
};

/* EmacsService is the service that starts the thread running Emacs
   and handles requests by that Emacs instance.  */

public class EmacsService extends Service
{
  public static final String TAG = "EmacsService";
  public static final int MAX_PENDING_REQUESTS = 256;
  public static volatile EmacsService SERVICE;
  public static boolean needDashQ;

  private EmacsThread thread;
  private Handler handler;

  /* Display metrics used by font backends.  */
  public DisplayMetrics metrics;

  @Override
  public int
  onStartCommand (Intent intent, int flags, int startId)
  {
    Notification notification;
    NotificationManager manager;
    NotificationChannel channel;
    String infoBlurb;
    Object tem;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
      {
	tem = getSystemService (Context.NOTIFICATION_SERVICE);
	manager = (NotificationManager) tem;
	infoBlurb = ("See (emacs)Android Environment for more"
		     + " details about this notification.");
	channel
	  = new NotificationChannel ("emacs", "Emacs persistent notification",
				     NotificationManager.IMPORTANCE_DEFAULT);
	manager.createNotificationChannel (channel);
	notification = (new Notification.Builder (this, "emacs")
			.setContentTitle ("Emacs")
			.setContentText (infoBlurb)
			.setSmallIcon (android.R.drawable.sym_def_app_icon)
			.build ());
	manager.notify (1, notification);
	startForeground (1, notification);
      }

    return START_NOT_STICKY;
  }

  @Override
  public IBinder
  onBind (Intent intent)
  {
    return null;
  }

  @SuppressWarnings ("deprecation")
  private String
  getApkFile ()
  {
    PackageManager manager;
    ApplicationInfo info;

    manager = getPackageManager ();

    try
      {
	if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU)
	  info = manager.getApplicationInfo ("org.gnu.emacs", 0);
	else
	  info = manager.getApplicationInfo ("org.gnu.emacs",
					     ApplicationInfoFlags.of (0));

	/* Return an empty string upon failure.  */

	if (info.sourceDir != null)
	  return info.sourceDir;

	return "";
      }
    catch (Exception e)
      {
	return "";
      }
  }

  private String
  getLibraryDirectory ()
  {
    int apiLevel;
    Context context;

    context = getApplicationContext ();
    apiLevel = Build.VERSION.SDK_INT;

    if (apiLevel >= Build.VERSION_CODES.GINGERBREAD)
      return context.getApplicationInfo().nativeLibraryDir;
    else if (apiLevel >= Build.VERSION_CODES.DONUT)
      return context.getApplicationInfo().dataDir + "/lib";

    return "/data/data/" + context.getPackageName() + "/lib";
  }

  @Override
  public void
  onCreate ()
  {
    AssetManager manager;
    Context app_context;
    String filesDir, libDir, cacheDir, classPath;
    double pixelDensityX;
    double pixelDensityY;

    SERVICE = this;
    handler = new Handler (Looper.getMainLooper ());
    manager = getAssets ();
    app_context = getApplicationContext ();
    metrics = getResources ().getDisplayMetrics ();
    pixelDensityX = metrics.xdpi;
    pixelDensityY = metrics.ydpi;

    try
      {
	/* Configure Emacs with the asset manager and other necessary
	   parameters.  */
	filesDir = app_context.getFilesDir ().getCanonicalPath ();
	libDir = getLibraryDirectory ();
	cacheDir = app_context.getCacheDir ().getCanonicalPath ();

	/* Now provide this application's apk file, so a recursive
	   invocation of app_process (through android-emacs) can
	   find EmacsNoninteractive.  */
	classPath = getApkFile ();

	Log.d (TAG, "Initializing Emacs, where filesDir = " + filesDir
	       + ", libDir = " + libDir + ", and classPath = " + classPath);

	EmacsNative.setEmacsParams (manager, filesDir, libDir,
				    cacheDir, (float) pixelDensityX,
				    (float) pixelDensityY,
				    classPath, this);

	/* Start the thread that runs Emacs.  */
	thread = new EmacsThread (this, needDashQ);
	thread.start ();
      }
    catch (IOException exception)
      {
	EmacsNative.emacsAbort ();
	return;
      }
  }



  /* Functions from here on must only be called from the Emacs
     thread.  */

  public void
  runOnUiThread (Runnable runnable)
  {
    handler.post (runnable);
  }

  public EmacsView
  getEmacsView (final EmacsWindow window, final int visibility,
		final boolean isFocusedByDefault)
  {
    Runnable runnable;
    final Holder<EmacsView> view;

    view = new Holder<EmacsView> ();

    runnable = new Runnable () {
	public void
	run ()
	{
	  synchronized (this)
	    {
	      view.thing = new EmacsView (window);
	      view.thing.setVisibility (visibility);

	      /* The following function is only present on Android 26
		 or later.  */
	      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
		view.thing.setFocusedByDefault (isFocusedByDefault);

	      notify ();
	    }
	}
      };

    synchronized (runnable)
      {
	runOnUiThread (runnable);

	try
	  {
	    runnable.wait ();
	  }
	catch (InterruptedException e)
	  {
	    EmacsNative.emacsAbort ();
	  }
      }

    return view.thing;
  }

  public void
  getLocationOnScreen (final EmacsView view, final int[] coordinates)
  {
    Runnable runnable;

    runnable = new Runnable () {
	public void
	run ()
	{
	  synchronized (this)
	    {
	      view.getLocationOnScreen (coordinates);
	      notify ();
	    }
	}
      };

    synchronized (runnable)
      {
	runOnUiThread (runnable);

	try
	  {
	    runnable.wait ();
	  }
	catch (InterruptedException e)
	  {
	    EmacsNative.emacsAbort ();
	  }
      }
  }

  public void
  fillRectangle (EmacsDrawable drawable, EmacsGC gc,
		 int x, int y, int width, int height)
  {
    EmacsFillRectangle.perform (drawable, gc, x, y,
				width, height);
  }

  public void
  fillPolygon (EmacsDrawable drawable, EmacsGC gc,
	       Point points[])
  {
    EmacsFillPolygon.perform (drawable, gc, points);
  }

  public void
  drawRectangle (EmacsDrawable drawable, EmacsGC gc,
		 int x, int y, int width, int height)
  {
    EmacsDrawRectangle.perform (drawable, gc, x, y,
				width, height);
  }

  public void
  drawLine (EmacsDrawable drawable, EmacsGC gc,
	    int x, int y, int x2, int y2)
  {
    EmacsDrawLine.perform (drawable, gc, x, y,
			   x2, y2);
  }

  public void
  drawPoint (EmacsDrawable drawable, EmacsGC gc,
	     int x, int y)
  {
    EmacsDrawPoint.perform (drawable, gc, x, y);
  }

  public void
  copyArea (EmacsDrawable srcDrawable, EmacsDrawable dstDrawable,
	    EmacsGC gc,
	    int srcX, int srcY, int width, int height, int destX,
	    int destY)
  {
    EmacsCopyArea.perform (srcDrawable, gc, dstDrawable,
			   srcX, srcY, width, height, destX,
			   destY);
  }

  public void
  clearWindow (EmacsWindow window)
  {
    window.clearWindow ();
  }

  public void
  clearArea (EmacsWindow window, int x, int y, int width,
	     int height)
  {
    window.clearArea (x, y, width, height);
  }

  @SuppressWarnings ("deprecation")
  public void
  ringBell ()
  {
    Vibrator vibrator;
    VibrationEffect effect;
    VibratorManager vibratorManager;
    Object tem;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S)
      {
	tem = getSystemService (Context.VIBRATOR_MANAGER_SERVICE);
	vibratorManager = (VibratorManager) tem;
        vibrator = vibratorManager.getDefaultVibrator ();
      }
    else
      vibrator
	= (Vibrator) getSystemService (Context.VIBRATOR_SERVICE);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
      {
	effect
	  = VibrationEffect.createOneShot (50,
					   VibrationEffect.DEFAULT_AMPLITUDE);
	vibrator.vibrate (effect);
      }
    else
      vibrator.vibrate (50);
  }

  public short[]
  queryTree (EmacsWindow window)
  {
    short[] array;
    List<EmacsWindow> windowList;
    int i;

    if (window == null)
      /* Just return all the windows without a parent.  */
      windowList = EmacsWindowAttachmentManager.MANAGER.copyWindows ();
    else
      windowList = window.children;

    array = new short[windowList.size () + 1];
    i = 1;

    array[0] = (window == null
		? 0 : (window.parent != null
		       ? window.parent.handle : 0));

    for (EmacsWindow treeWindow : windowList)
      array[i++] = treeWindow.handle;

    return array;
  }

  public int
  getScreenWidth (boolean mmWise)
  {
    DisplayMetrics metrics;

    metrics = getResources ().getDisplayMetrics ();

    if (!mmWise)
      return metrics.widthPixels;
    else
      return (int) ((metrics.widthPixels / metrics.xdpi) * 2540.0);
  }

  public int
  getScreenHeight (boolean mmWise)
  {
    DisplayMetrics metrics;

    metrics = getResources ().getDisplayMetrics ();

    if (!mmWise)
      return metrics.heightPixels;
    else
      return (int) ((metrics.heightPixels / metrics.ydpi) * 2540.0);
  }

  public boolean
  detectMouse ()
  {
    InputManager manager;
    InputDevice device;
    int[] ids;
    int i;

    if (Build.VERSION.SDK_INT
	< Build.VERSION_CODES.JELLY_BEAN)
      return false;

    manager = (InputManager) getSystemService (Context.INPUT_SERVICE);
    ids = manager.getInputDeviceIds ();

    for (i = 0; i < ids.length; ++i)
      {
	device = manager.getInputDevice (ids[i]);

	if (device == null)
	  continue;

	if (device.supportsSource (InputDevice.SOURCE_MOUSE))
	  return true;
      }

    return false;
  }

  public String
  nameKeysym (int keysym)
  {
    return KeyEvent.keyCodeToString (keysym);
  }

  public void
  sync ()
  {
    Runnable runnable;

    runnable = new Runnable () {
	public void
	run ()
	{
	  synchronized (this)
	    {
	      notify ();
	    }
	}
      };

    synchronized (runnable)
      {
	runOnUiThread (runnable);

	try
	  {
	    runnable.wait ();
	  }
	catch (InterruptedException e)
	  {
	    EmacsNative.emacsAbort ();
	  }
      }
  }

  

  /* Start the Emacs service if necessary.  On Android 26 and up,
     start Emacs as a foreground service with a notification, to avoid
     it being killed by the system.

     On older systems, simply start it as a normal background
     service.  */

  public static void
  startEmacsService (Context context)
  {
    if (EmacsService.SERVICE == null)
      {
	if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O)
	  /* Start the Emacs service now.  */
	  context.startService (new Intent (context,
					    EmacsService.class));
	else
	  /* Display the permanant notification and start Emacs as a
	     foreground service.  */
	  context.startForegroundService (new Intent (context,
						      EmacsService.class));
      }
  }

  /* Ask the system to open the specified URL.
     Value is NULL upon success, or a string describing the error
     upon failure.  */

  public String
  browseUrl (String url)
  {
    Intent intent;

    try
      {
	intent = new Intent (Intent.ACTION_VIEW, Uri.parse (url));
	intent.setFlags (Intent.FLAG_ACTIVITY_NEW_TASK);
	startActivity (intent);
      }
    catch (Exception e)
      {
	return e.toString ();
      }

    return null;
  }

  /* Get a SDK 11 ClipboardManager.

     Android 4.0.x requires that this be called from the main
     thread.  */

  public ClipboardManager
  getClipboardManager ()
  {
    final Holder<ClipboardManager> manager;
    Runnable runnable;

    manager = new Holder<ClipboardManager> ();

    runnable = new Runnable () {
	public void
	run ()
	{
	  Object tem;

	  synchronized (this)
	    {
	      tem = getSystemService (Context.CLIPBOARD_SERVICE);
	      manager.thing = (ClipboardManager) tem;
	      notify ();
	    }
	}
      };

    synchronized (runnable)
      {
	runOnUiThread (runnable);

	try
	  {
	    runnable.wait ();
	  }
	catch (InterruptedException e)
	  {
	    EmacsNative.emacsAbort ();
	  }
      }

    return manager.thing;
  }

  public void
  restartEmacs ()
  {
    Intent intent;

    intent = new Intent (this, EmacsActivity.class);
    intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK
		     | Intent.FLAG_ACTIVITY_CLEAR_TASK);
    startActivity (intent);
    System.exit (0);
  }
};
