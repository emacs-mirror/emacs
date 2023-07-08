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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import java.util.List;

import java.util.concurrent.atomic.AtomicInteger;

import android.graphics.Matrix;
import android.graphics.Point;

import android.view.InputDevice;
import android.view.KeyEvent;
import android.view.inputmethod.CursorAnchorInfo;
import android.view.inputmethod.ExtractedText;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.NotificationChannel;
import android.app.Service;

import android.content.ClipboardManager;
import android.content.Context;
import android.content.ContentResolver;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.ApplicationInfoFlags;
import android.content.pm.PackageManager;
import android.content.res.AssetManager;

import android.hardware.input.InputManager;

import android.net.Uri;

import android.os.BatteryManager;
import android.os.Build;
import android.os.Looper;
import android.os.IBinder;
import android.os.Handler;
import android.os.ParcelFileDescriptor;
import android.os.Vibrator;
import android.os.VibratorManager;
import android.os.VibrationEffect;

import android.provider.DocumentsContract;

import android.util.Log;
import android.util.DisplayMetrics;

import android.widget.Toast;

/* EmacsService is the service that starts the thread running Emacs
   and handles requests by that Emacs instance.  */

public final class EmacsService extends Service
{
  public static final String TAG = "EmacsService";
  public static volatile EmacsService SERVICE;

  /* If non-NULL, an extra argument to pass to
     `android_emacs_init'.  */
  public static String extraStartupArgument;

  private EmacsThread thread;
  private Handler handler;
  private ContentResolver resolver;

  /* Keep this in synch with androidgui.h.  */
  public static final int IC_MODE_NULL   = 0;
  public static final int IC_MODE_ACTION = 1;
  public static final int IC_MODE_TEXT   = 2;

  /* Display metrics used by font backends.  */
  public DisplayMetrics metrics;

  /* Flag that says whether or not to print verbose debugging
     information when responding to an input method.  */
  public static final boolean DEBUG_IC = false;

  /* Flag that says whether or not to stringently check that only the
     Emacs thread is performing drawing calls.  */
  private static final boolean DEBUG_THREADS = false;

  /* Atomic integer used for synchronization between
     icBeginSynchronous/icEndSynchronous and viewGetSelection.

     Value is 0 if no query is in progress, 1 if viewGetSelection is
     being called, and 2 if icBeginSynchronous was called.  */
  public static final AtomicInteger servicingQuery;

  static
  {
    servicingQuery = new AtomicInteger ();
  };

  /* Return the directory leading to the directory in which native
     library files are stored on behalf of CONTEXT.  */

  public static String
  getLibraryDirectory (Context context)
  {
    int apiLevel;

    apiLevel = Build.VERSION.SDK_INT;

    if (apiLevel >= Build.VERSION_CODES.GINGERBREAD)
      return context.getApplicationInfo ().nativeLibraryDir;

    return context.getApplicationInfo ().dataDir + "/lib";
  }

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
	infoBlurb = ("This notification is displayed to keep Emacs"
		     + " running while it is in the background.  You"
		     + " may disable if you want;"
		     + " see (emacs)Android Environment.");
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

  @Override
  public void
  onCreate ()
  {
    final AssetManager manager;
    Context app_context;
    final String filesDir, libDir, cacheDir, classPath;
    final double pixelDensityX;
    final double pixelDensityY;
    final double scaledDensity;

    SERVICE = this;
    handler = new Handler (Looper.getMainLooper ());
    manager = getAssets ();
    app_context = getApplicationContext ();
    metrics = getResources ().getDisplayMetrics ();
    pixelDensityX = metrics.xdpi;
    pixelDensityY = metrics.ydpi;
    scaledDensity = ((metrics.scaledDensity
		      / metrics.density)
		     * pixelDensityX);
    resolver = getContentResolver ();

    try
      {
	/* Configure Emacs with the asset manager and other necessary
	   parameters.  */
	filesDir = app_context.getFilesDir ().getCanonicalPath ();
	libDir = getLibraryDirectory (this);
	cacheDir = app_context.getCacheDir ().getCanonicalPath ();

	/* Now provide this application's apk file, so a recursive
	   invocation of app_process (through android-emacs) can
	   find EmacsNoninteractive.  */
	classPath = getApkFile ();

	Log.d (TAG, "Initializing Emacs, where filesDir = " + filesDir
	       + ", libDir = " + libDir + ", and classPath = " + classPath
	       + "; fileToOpen = " + EmacsOpenActivity.fileToOpen);

	/* Start the thread that runs Emacs.  */
	thread = new EmacsThread (this, new Runnable () {
	    @Override
	    public void
	    run ()
	    {
	      EmacsNative.setEmacsParams (manager, filesDir, libDir,
					  cacheDir, (float) pixelDensityX,
					  (float) pixelDensityY,
					  (float) scaledDensity,
					  classPath, EmacsService.this);
	    }
	  }, extraStartupArgument,
	  /* If any file needs to be opened, open it now.  */
	  EmacsOpenActivity.fileToOpen);
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
    final EmacsHolder<EmacsView> view;

    view = new EmacsHolder<EmacsView> ();

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

    syncRunnable (runnable);
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

    syncRunnable (runnable);
  }



  public static void
  checkEmacsThread ()
  {
    if (DEBUG_THREADS)
      {
	if (Thread.currentThread () instanceof EmacsThread)
	  return;

	throw new RuntimeException ("Emacs thread function"
				    + " called from other thread!");
      }
  }

  /* These drawing functions must only be called from the Emacs
     thread.  */

  public void
  fillRectangle (EmacsDrawable drawable, EmacsGC gc,
		 int x, int y, int width, int height)
  {
    checkEmacsThread ();
    EmacsFillRectangle.perform (drawable, gc, x, y,
				width, height);
  }

  public void
  fillPolygon (EmacsDrawable drawable, EmacsGC gc,
	       Point points[])
  {
    checkEmacsThread ();
    EmacsFillPolygon.perform (drawable, gc, points);
  }

  public void
  drawRectangle (EmacsDrawable drawable, EmacsGC gc,
		 int x, int y, int width, int height)
  {
    checkEmacsThread ();
    EmacsDrawRectangle.perform (drawable, gc, x, y,
				width, height);
  }

  public void
  drawLine (EmacsDrawable drawable, EmacsGC gc,
	    int x, int y, int x2, int y2)
  {
    checkEmacsThread ();
    EmacsDrawLine.perform (drawable, gc, x, y,
			   x2, y2);
  }

  public void
  drawPoint (EmacsDrawable drawable, EmacsGC gc,
	     int x, int y)
  {
    checkEmacsThread ();
    EmacsDrawPoint.perform (drawable, gc, x, y);
  }

  public void
  clearWindow (EmacsWindow window)
  {
    checkEmacsThread ();
    window.clearWindow ();
  }

  public void
  clearArea (EmacsWindow window, int x, int y, int width,
	     int height)
  {
    checkEmacsThread ();
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
	/* Android 4.0 and earlier don't support mouse input events at
	   all.  */
	< Build.VERSION_CODES.JELLY_BEAN)
      return false;

    manager = (InputManager) getSystemService (Context.INPUT_SERVICE);
    ids = manager.getInputDeviceIds ();

    for (i = 0; i < ids.length; ++i)
      {
	device = manager.getInputDevice (ids[i]);

	if (device == null)
	  continue;

	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
	  {
	    if (device.supportsSource (InputDevice.SOURCE_MOUSE))
	      return true;
	  }
	else
	  {
	    /* `supportsSource' is only present on API level 21 and
	       later, but earlier versions provide a bit mask
	       containing each supported source.  */

	    if ((device.getSources () & InputDevice.SOURCE_MOUSE) != 0)
	      return true;
	  }
      }

    return false;
  }

  public String
  nameKeysym (int keysym)
  {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR1)
      return KeyEvent.keyCodeToString (keysym);

    return String.valueOf (keysym);
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
    Uri uri;

    try
      {
	/* Parse the URI.  */
	uri = Uri.parse (url);

	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
	  {
	    /* On Android 4.4 and later, check if URI is actually a
	       file name.  If so, rewrite it into a content provider
	       URI, so that it can be accessed by other programs.  */

	    if (uri.getScheme ().equals ("file")
		&& uri.getPath () != null)
	      uri
		= DocumentsContract.buildDocumentUri ("org.gnu.emacs",
						      uri.getPath ());
	  }

	Log.d (TAG, ("browseUri: browsing " + url
		     + " --> " + uri.getPath ()
		     + " --> " + uri));

	intent = new Intent (Intent.ACTION_VIEW, uri);
	intent.setFlags (Intent.FLAG_ACTIVITY_NEW_TASK
			 | Intent.FLAG_GRANT_READ_URI_PERMISSION);
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
    final EmacsHolder<ClipboardManager> manager;
    Runnable runnable;

    manager = new EmacsHolder<ClipboardManager> ();

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

    syncRunnable (runnable);
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

  /* Wait synchronously for the specified RUNNABLE to complete in the
     UI thread.  Must be called from the Emacs thread.  */

  public static void
  syncRunnable (Runnable runnable)
  {
    EmacsNative.beginSynchronous ();

    synchronized (runnable)
      {
	SERVICE.runOnUiThread (runnable);

	while (true)
	  {
	    try
	      {
		runnable.wait ();
		break;
	      }
	    catch (InterruptedException e)
	      {
		continue;
	      }
	  }
      }

    EmacsNative.endSynchronous ();
  }



  /* IMM functions such as `updateSelection' holds an internal lock
     that is also taken before `onCreateInputConnection' (in
     EmacsView.java) is called; when that then asks the UI thread for
     the current selection, a dead lock results.  To remedy this,
     reply to any synchronous queries now -- and prohibit more queries
     for the duration of `updateSelection' -- if EmacsView may have
     been asking for the value of the region.  */

  public static void
  icBeginSynchronous ()
  {
    /* Set servicingQuery to 2, so viewGetSelection knows it shouldn't
       proceed.  */

    if (servicingQuery.getAndSet (2) == 1)
      /* But if viewGetSelection is already in progress, answer it
	 first.  */
      EmacsNative.answerQuerySpin ();
  }

  public static void
  icEndSynchronous ()
  {
    if (servicingQuery.getAndSet (0) != 2)
      throw new RuntimeException ("incorrect value of `servicingQuery': "
				  + "likely 1");
  }

  public static int[]
  viewGetSelection (short window)
  {
    int[] selection;

    /* See if a query is already in progress from the other
       direction.  */
    if (!servicingQuery.compareAndSet (0, 1))
      return null;

    /* Now call the regular getSelection.  Note that this can't race
       with answerQuerySpin, as `android_servicing_query' can never be
       2 when icBeginSynchronous is called, so a query will always be
       started.  */
    selection = EmacsNative.getSelection (window);

    /* Finally, clear servicingQuery if its value is still 1.  If a
       query has started from the other side, it ought to be 2.  */

    servicingQuery.compareAndSet (1, 0);
    return selection;
  }



  public void
  updateIC (EmacsWindow window, int newSelectionStart,
	    int newSelectionEnd, int composingRegionStart,
	    int composingRegionEnd)
  {
    if (DEBUG_IC)
      Log.d (TAG, ("updateIC: " + window + " " + newSelectionStart
		   + " " + newSelectionEnd + " "
		   + composingRegionStart + " "
		   + composingRegionEnd));

    icBeginSynchronous ();
    window.view.imManager.updateSelection (window.view,
					   newSelectionStart,
					   newSelectionEnd,
					   composingRegionStart,
					   composingRegionEnd);
    icEndSynchronous ();
  }

  public void
  resetIC (EmacsWindow window, int icMode)
  {
    int oldMode;

    if (DEBUG_IC)
      Log.d (TAG, "resetIC: " + window + ", " + icMode);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU
	&& (oldMode = window.view.getICMode ()) == icMode
	/* Don't do this if there is currently no input
	   connection.  */
	&& oldMode != IC_MODE_NULL)
      {
	if (DEBUG_IC)
	  Log.d (TAG, "resetIC: calling invalidateInput");

	/* Android 33 and later allow the IM reset to be optimized out
	   and replaced by a call to `invalidateInput', which is much
	   faster, as it does not involve resetting the input
	   connection.  */

	icBeginSynchronous ();
	window.view.imManager.invalidateInput (window.view);
	icEndSynchronous ();

	return;
      }

    window.view.setICMode (icMode);

    icBeginSynchronous ();
    window.view.icGeneration++;
    window.view.imManager.restartInput (window.view);
    icEndSynchronous ();
  }

  public void
  updateCursorAnchorInfo (EmacsWindow window, float x,
			  float y, float yBaseline,
			  float yBottom)
  {
    CursorAnchorInfo info;
    CursorAnchorInfo.Builder builder;
    Matrix matrix;
    int[] offsets;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP)
      return;

    offsets = new int[2];
    builder = new CursorAnchorInfo.Builder ();
    matrix = new Matrix (window.view.getMatrix ());
    window.view.getLocationOnScreen (offsets);
    matrix.postTranslate (offsets[0], offsets[1]);
    builder.setMatrix (matrix);
    builder.setInsertionMarkerLocation (x, y, yBaseline, yBottom,
					0);
    info = builder.build ();



    if (DEBUG_IC)
      Log.d (TAG, ("updateCursorAnchorInfo: " + x + " " + y
		   + " " + yBaseline + "-" + yBottom));

    icBeginSynchronous ();
    window.view.imManager.updateCursorAnchorInfo (window.view, info);
    icEndSynchronous ();
  }

  /* Open a content URI described by the bytes BYTES, a non-terminated
     string; make it writable if WRITABLE, and readable if READABLE.
     Truncate the file if TRUNCATE.

     Value is the resulting file descriptor or -1 upon failure.  */

  public int
  openContentUri (byte[] bytes, boolean writable, boolean readable,
		  boolean truncate)
  {
    String name, mode;
    ParcelFileDescriptor fd;
    int i;

    /* Figure out the file access mode.  */

    mode = "";

    if (readable)
      mode += "r";

    if (writable)
      mode += "w";

    if (truncate)
      mode += "t";

    /* Try to open an associated ParcelFileDescriptor.  */

    try
      {
	/* The usual file name encoding question rears its ugly head
	   again.  */
	name = new String (bytes, "UTF-8");
	Log.d (TAG, "openContentUri: " + Uri.parse (name));

	fd = resolver.openFileDescriptor (Uri.parse (name), mode);

	/* Use detachFd on newer versions of Android or plain old
	   dup.  */

	if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR1)
	  {
	    i = fd.detachFd ();
	    fd.close ();

	    return i;
	  }
	else
	  {
	    i = EmacsNative.dup (fd.getFd ());
	    fd.close ();

	    return i;
	  }
      }
    catch (Exception exception)
      {
	return -1;
      }
  }

  public boolean
  checkContentUri (byte[] string, boolean readable, boolean writable)
  {
    String mode, name;
    ParcelFileDescriptor fd;

    /* Decode this into a URI.  */

    try
      {
	/* The usual file name encoding question rears its ugly head
	   again.  */
	name = new String (string, "UTF-8");
	Log.d (TAG, "checkContentUri: " + Uri.parse (name));
      }
    catch (UnsupportedEncodingException exception)
      {
	name = null;
	throw new RuntimeException (exception);
      }

    mode = "r";

    if (writable)
      mode += "w";

    Log.d (TAG, "checkContentUri: checking against mode " + mode);

    try
      {
	fd = resolver.openFileDescriptor (Uri.parse (name), mode);
	fd.close ();

	Log.d (TAG, "checkContentUri: YES");

	return true;
      }
    catch (Exception exception)
      {
	Log.d (TAG, "checkContentUri: NO");
	Log.d (TAG, exception.toString ());
	return false;
      }
  }

  private long[]
  queryBattery19 ()
  {
    IntentFilter filter;
    Intent battery;
    long capacity, chargeCounter, currentAvg, currentNow;
    long status, remaining, plugged, temp;

    filter = new IntentFilter (Intent.ACTION_BATTERY_CHANGED);
    battery = registerReceiver (null, filter);

    if (battery == null)
      return null;

    capacity = battery.getIntExtra (BatteryManager.EXTRA_LEVEL, 0);
    chargeCounter
      = (battery.getIntExtra (BatteryManager.EXTRA_SCALE, 0)
	 / battery.getIntExtra (BatteryManager.EXTRA_LEVEL, 100) * 100);
    currentAvg = 0;
    currentNow = 0;
    status = battery.getIntExtra (BatteryManager.EXTRA_STATUS, 0);
    remaining = -1;
    plugged = battery.getIntExtra (BatteryManager.EXTRA_PLUGGED, 0);
    temp = battery.getIntExtra (BatteryManager.EXTRA_TEMPERATURE, 0);

    return new long[] { capacity, chargeCounter, currentAvg,
			currentNow, remaining, status, plugged,
			temp, };
  }

  /* Return the status of the battery.  See struct
     android_battery_status for the order of the elements
     returned.

     Value may be null upon failure.  */

  public long[]
  queryBattery ()
  {
    Object tem;
    BatteryManager manager;
    long capacity, chargeCounter, currentAvg, currentNow;
    long status, remaining, plugged, temp;
    int prop;
    IntentFilter filter;
    Intent battery;

    /* Android 4.4 or earlier require applications to use a different
       API to query the battery status.  */

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP)
      return queryBattery19 ();

    tem = getSystemService (Context.BATTERY_SERVICE);
    manager = (BatteryManager) tem;
    remaining = -1;

    prop = BatteryManager.BATTERY_PROPERTY_CAPACITY;
    capacity = manager.getLongProperty (prop);
    prop = BatteryManager.BATTERY_PROPERTY_CHARGE_COUNTER;
    chargeCounter = manager.getLongProperty (prop);
    prop = BatteryManager.BATTERY_PROPERTY_CURRENT_AVERAGE;
    currentAvg = manager.getLongProperty (prop);
    prop = BatteryManager.BATTERY_PROPERTY_CURRENT_NOW;
    currentNow = manager.getLongProperty (prop);

    /* Return the battery status.  N.B. that Android 7.1 and earlier
       only return ``charging'' or ``discharging''.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
      status
	= manager.getIntProperty (BatteryManager.BATTERY_PROPERTY_STATUS);
    else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M)
      status = (manager.isCharging ()
		? BatteryManager.BATTERY_STATUS_CHARGING
		: BatteryManager.BATTERY_STATUS_DISCHARGING);
    else
      status = (currentNow > 0
		? BatteryManager.BATTERY_STATUS_CHARGING
		: BatteryManager.BATTERY_STATUS_DISCHARGING);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P)
      remaining = manager.computeChargeTimeRemaining ();

    plugged = -1;
    temp = -1;

    /* Now obtain additional information from the battery manager.  */

    filter = new IntentFilter (Intent.ACTION_BATTERY_CHANGED);
    battery = registerReceiver (null, filter);

    if (battery != null)
      {
	plugged = battery.getIntExtra (BatteryManager.EXTRA_PLUGGED, 0);
	temp = battery.getIntExtra (BatteryManager.EXTRA_TEMPERATURE, 0);

	/* Make status more reliable.  */
	if (Build.VERSION.SDK_INT < Build.VERSION_CODES.M)
	  status = battery.getIntExtra (BatteryManager.EXTRA_STATUS, 0);
      }

    return new long[] { capacity, chargeCounter, currentAvg,
			currentNow, remaining, status, plugged,
			temp, };
  }

  /* Display the specified STRING in a small dialog box on the main
     thread.  */

  public void
  displayToast (final String string)
  {
    runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  Toast toast;

	  toast = Toast.makeText (getApplicationContext (),
				  string, Toast.LENGTH_SHORT);
	  toast.show ();
	}
      });
  }

  public void
  updateExtractedText (EmacsWindow window, ExtractedText text,
		       int token)
  {
    if (DEBUG_IC)
      Log.d (TAG, "updateExtractedText: @" + token + ", " + text);

    window.view.imManager.updateExtractedText (window.view,
					       token, text);
  }
};
