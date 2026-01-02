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

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

import java.util.concurrent.atomic.AtomicInteger;

import android.database.Cursor;

import android.graphics.Matrix;
import android.graphics.Point;

import android.webkit.MimeTypeMap;

import android.view.InputDevice;
import android.view.KeyEvent;
import android.view.inputmethod.CursorAnchorInfo;
import android.view.inputmethod.ExtractedText;

import android.app.AlarmManager;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;

import android.content.ActivityNotFoundException;
import android.content.ClipboardManager;
import android.content.Context;
import android.content.ContentResolver;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.UriPermission;

import android.content.pm.PackageManager;

import android.content.res.AssetManager;
import android.content.res.Configuration;
import android.content.res.Resources;

import android.hardware.input.InputManager;

import android.net.Uri;

import android.os.BatteryManager;
import android.os.Binder;
import android.os.Build;
import android.os.Environment;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.ParcelFileDescriptor;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.os.VibratorManager;

import android.provider.DocumentsContract;
import android.provider.DocumentsContract.Document;
import android.provider.OpenableColumns;
import android.provider.Settings;

import android.util.Log;
import android.util.DisplayMetrics;

import android.widget.Toast;

/* EmacsService is the service that starts the thread running Emacs
   and handles requests by that Emacs instance.  */

public final class EmacsService extends Service
{
  public static final String TAG = "EmacsService";

  /* The started Emacs service object.  */
  public static EmacsService SERVICE;

  /* If non-NULL, an array of extra arguments to pass to
     `android_emacs_init'.  */
  public static String[] extraStartupArguments;

  /* The thread running Emacs C code.  */
  private EmacsThread thread;

  /* Handler used to run tasks on the main thread.  */
  private Handler handler;

  /* Content resolver used to access URIs.  */
  private ContentResolver resolver;

  /* Keep this in synch with androidgui.h.  */
  public static final int IC_MODE_NULL     = 0;
  public static final int IC_MODE_ACTION   = 1;
  public static final int IC_MODE_TEXT     = 2;
  public static final int IC_MODE_PASSWORD = 3;

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

  /* Thread used to query document providers, or null if it hasn't
     been created yet.  */
  private EmacsSafThread storageThread;

  /* The Thread object representing the Android user interface
     thread.  */
  private Thread mainThread;

  /* The display's horizontal and vertical density and that which is
     consulted for font scaling.  */
  private double dpiX, dpiY, dpiScaled;

  /* The display's previously observed UI mode as it relates to the
     system theme.  */
  private int uiMode;

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
		     + " may disable it if you wish;"
		     + " see (emacs)Android Environment.");
	channel
	  = new NotificationChannel ("emacs", "Emacs Background Service",
				     NotificationManager.IMPORTANCE_LOW);
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

  /* Return the display density, adjusted in accord with the user's
     text scaling preferences.  */

  @SuppressWarnings ("deprecation")
  private static float
  getScaledDensity (DisplayMetrics metrics)
  {
    /* The scaled density has been made obsolete by the introduction
       of non-linear text scaling in Android 34, where there is no
       longer a fixed relation between point and pixel sizes, but
       remains useful, considering that Emacs does not support
       non-linear text scaling.  */
    return metrics.scaledDensity;
  }

  @Override
  public void
  onCreate ()
  {
    final AssetManager manager;
    Context app_context;
    final String filesDir, libDir, cacheDir, classPath;
    final float pixelDensityX;
    final float pixelDensityY;
    final float scaledDensity;
    float tempScaledDensity;
    Resources resources;
    DisplayMetrics metrics;
    Configuration configuration;

    super.onCreate ();

    SERVICE = this;
    resources = getResources ();
    handler = new Handler (Looper.getMainLooper ());
    manager = getAssets ();
    app_context = getApplicationContext ();
    metrics = resources.getDisplayMetrics ();
    pixelDensityX = metrics.xdpi;
    pixelDensityY = metrics.ydpi;
    tempScaledDensity = ((getScaledDensity (metrics)
			  / metrics.density)
			 * pixelDensityX);
    configuration = resources.getConfiguration ();
    uiMode = configuration.uiMode & Configuration.UI_MODE_NIGHT_MASK;
    resolver = getContentResolver ();
    mainThread = Thread.currentThread ();

    /* If the density used to compute the text size is smaller than 160,
       there's likely a bug with display density computation.  Reset it
       to 160 in that case.

       Note that Android uses 160 ``dpi'' as the density where 1 point
       corresponds to 1 pixel, not 72 or 96 as used elsewhere.  This
       difference is codified in PT_PER_INCH defined in font.h.  */

    if (tempScaledDensity < 160.0f)
      tempScaledDensity = 160.0f;

    /* scaledDensity is const as required to refer to it from within
       the nested function below.  */
    scaledDensity = tempScaledDensity;

    /* Save these fields for future reference.  */
    dpiX = pixelDensityX;
    dpiY = pixelDensityY;
    dpiScaled = scaledDensity;

    /* Remove all tasks from previous Emacs sessions but the task
       created by the system at startup.  */
    EmacsWindowManager.MANAGER.removeOldTasks (this);

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
	classPath = EmacsApplication.apkFileName;

	Log.d (TAG, "Initializing Emacs, where filesDir = " + filesDir
	       + ", libDir = " + libDir + ", and classPath = " + classPath
	       + "; args = " + (extraStartupArguments != null
				? Arrays.toString (extraStartupArguments)
				: "(none)")
	       + "; display density: " + pixelDensityX + " by "
	       + pixelDensityY + " scaled to " + scaledDensity);

	/* Start the thread that runs Emacs.  */
	thread = new EmacsThread (this, new Runnable () {
	    @Override
	    public void
	    run ()
	    {
	      EmacsNative.setEmacsParams (manager, filesDir, libDir,
					  cacheDir, pixelDensityX,
					  pixelDensityY, scaledDensity,
					  uiMode, classPath,
					  EmacsService.this,
					  Build.VERSION.SDK_INT);
	    }
	  }, extraStartupArguments);
	thread.start ();
      }
    catch (IOException exception)
      {
	EmacsNative.emacsAbort ();
	return;
      }
  }

  /* The native functions the subsequent two functions call do nothing
     in the infrequent case the Emacs thread is awaiting a response
     for the main thread.  Caveat emptor! */

  @Override
  public void
  onDestroy ()
  {
    /* This function is called immediately before the system kills
       Emacs.  In this respect, it is rather akin to a SIGDANGER
       signal, so force an auto-save accordingly.  */

    EmacsNative.shutDownEmacs ();
    super.onDestroy ();
  }

  @Override
  public void
  onLowMemory ()
  {
    EmacsNative.onLowMemory ();
    super.onLowMemory ();
  }

  @Override
  public void
  onConfigurationChanged (Configuration newConfig)
  {
    DisplayMetrics metrics;
    float pixelDensityX, pixelDensityY, scaledDensity;

    metrics = getResources ().getDisplayMetrics ();

    /* The display configuration may have been altered.  Retrieve the
       revised display density and deliver an event if so.  */
    pixelDensityX = metrics.xdpi;
    pixelDensityY = metrics.ydpi;
    scaledDensity = ((getScaledDensity (metrics)
		      / metrics.density) * pixelDensityX);

    /* A density below 160 probably indicates a system bug.  See
       onCreate for more commentary.  */
    if (scaledDensity < 160.0f)
      scaledDensity = 160.0f;

    if (pixelDensityX != dpiX || pixelDensityY != dpiY
	|| scaledDensity != dpiScaled)
      {
	dpiX = pixelDensityX;
	dpiY = pixelDensityY;
	dpiScaled = scaledDensity;
	EmacsNative.sendConfigurationChanged (0, pixelDensityX, pixelDensityY,
					      scaledDensity, 0);
      }

    if ((newConfig.uiMode & Configuration.UI_MODE_NIGHT_MASK) != uiMode)
      {
	uiMode = newConfig.uiMode & Configuration.UI_MODE_NIGHT_MASK;
	EmacsNative.sendConfigurationChanged (1, 0.0f, 0.0f, 0.0f, uiMode);
      }

    super.onConfigurationChanged (newConfig);
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
    FutureTask<EmacsView> task;

    task = new FutureTask<EmacsView> (new Callable<EmacsView> () {
	@Override
	public EmacsView
	call ()
	{
	  EmacsView view;

	  view = new EmacsView (window);
	  view.setVisibility (visibility);

	  /* The following function is only present on Android 26
	     or later.  */
	  if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
	    view.setFocusedByDefault (isFocusedByDefault);

	  return view;
	}
      });

    return EmacsService.<EmacsView>syncRunnable (task);
  }

  public void
  getLocationOnScreen (final EmacsView view, final int[] coordinates)
  {
    FutureTask<Void> task;

    task = new FutureTask<Void> (new Callable<Void> () {
	public Void
	call ()
	{
	  view.getLocationOnScreen (coordinates);
	  return null;
	}
      });

    EmacsService.<Void>syncRunnable (task);
  }

  public void
  getLocationInWindow (final EmacsView view, final int[] coordinates)
  {
    FutureTask<Void> task;

    task = new FutureTask<Void> (new Callable<Void> () {
	public Void
	call ()
	{
	  view.getLocationInWindow (coordinates);
	  return null;
	}
      });

    EmacsService.<Void>syncRunnable (task);
  }



  public static void
  checkEmacsThread ()
  {
    if (DEBUG_THREADS)
      {
	/* When SERVICE is NULL, Emacs is being executed non-interactively.  */
	if (SERVICE == null
	    /* It was previously assumed that only instances of
	       `EmacsThread' were valid for graphics calls, but this is
	       no longer true now that Lisp threads can be attached to
	       the JVM.  */
	    || (Thread.currentThread () != SERVICE.mainThread))
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

  @SuppressWarnings ("deprecation")
  public void
  ringBell (int duration)
  {
    Vibrator vibrator;
    VibrationEffect effect;
    VibratorManager vibratorManager;
    Object tem;
    int amplitude;

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
	amplitude = VibrationEffect.DEFAULT_AMPLITUDE;
	effect
	  = VibrationEffect.createOneShot (duration, amplitude);
	vibrator.vibrate (effect);
      }
    else
      vibrator.vibrate (duration);
  }

  public long[]
  queryTree (EmacsWindow window)
  {
    long[] array;
    List<EmacsWindow> windowList;
    int i;

    if (window == null)
      /* Just return all the windows without a parent.  */
      windowList = EmacsWindowManager.MANAGER.copyWindows ();
    else
      windowList = window.children;

    synchronized (windowList)
      {
	array = new long[windowList.size () + 1];
	i = 1;

	array[0] = (window == null
		    ? 0 : (window.parent != null
			   ? window.parent.handle : 0));

	for (EmacsWindow treeWindow : windowList)
	  array[i++] = treeWindow.handle;
      }

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

  public boolean
  detectKeyboard ()
  {
    Configuration configuration;

    configuration = getResources ().getConfiguration ();
    return configuration.keyboard != Configuration.KEYBOARD_NOKEYS;
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
	  /* Display the permanent notification and start Emacs as a
	     foreground service.  */
	  context.startForegroundService (new Intent (context,
						      EmacsService.class));
      }
  }

  /* Ask the system to open the specified URL in an application that
     understands how to open it.

     If SEND, tell the system to also open applications that can
     ``send'' the URL (through mail, for example), instead of only
     those that can view the URL.

     Value is NULL upon success, or a string describing the error
     upon failure.  */

  public String
  browseUrl (String url, boolean send)
  {
    Intent intent;
    Uri uri;

    try
      {
	/* Parse the URI.  */
	if (!send)
	  {
	    uri = Uri.parse (url);

	    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
	      {
		/* On Android 4.4 and later, check if URI is actually
		   a file name.  If so, rewrite it into a content
		   provider URI, so that it can be accessed by other
		   programs.  */

		if (uri.getScheme ().equals ("file")
		    && uri.getPath () != null)
		  uri
		    = DocumentsContract.buildDocumentUri ("org.gnu.emacs",
							  uri.getPath ());
	      }

	    intent = new Intent (Intent.ACTION_VIEW, uri);

	    /* Set several flags on the Intent prompting the system to
	       permit the recipient to read and edit the URI
	       indefinitely.  */

	    intent.setFlags (Intent.FLAG_ACTIVITY_NEW_TASK
			     | Intent.FLAG_GRANT_READ_URI_PERMISSION
			     | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);

	    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
	      intent.addFlags (Intent.FLAG_GRANT_PERSISTABLE_URI_PERMISSION);
	  }
	else
	  {
	    intent = new Intent (Intent.ACTION_SEND);
	    intent.setType ("text/plain");
	    intent.putExtra (Intent.EXTRA_SUBJECT, "Sharing link");
	    intent.putExtra (Intent.EXTRA_TEXT, url);

	    /* Display a list of programs able to send this URL.  */
	    intent = Intent.createChooser (intent, "Send");

	    /* Apparently flags need to be set after a chooser is
	       created.  */
	    intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK);
	  }

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
    FutureTask<Object> task;

    task = new FutureTask<Object> (new Callable<Object> () {
	public Object
	call ()
	{
	  return getSystemService (Context.CLIPBOARD_SERVICE);
	}
      });

    return (ClipboardManager) EmacsService.<Object>syncRunnable (task);
  }

  public void
  restartEmacs ()
  {
    Intent intent;
    PendingIntent pending;
    AlarmManager manager;

    intent = new Intent (this, EmacsActivity.class);
    intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK
		     | Intent.FLAG_ACTIVITY_CLEAR_TASK);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
      startActivity (intent);
    else
      {
	/* Experimentation has established that Android 4.3 and earlier
	   versions do not attempt to recreate a process when it crashes
	   immediately after requesting that an intent for itself be
	   started.  Schedule an intent to start some time after Emacs
	   exits instead.  */

	pending = PendingIntent.getActivity (this, 0, intent, 0);
	manager = (AlarmManager) getSystemService (Context.ALARM_SERVICE);
	manager.set (AlarmManager.RTC, System.currentTimeMillis () + 100,
		     pending);
      }

    System.exit (0);
  }

  /* Wait synchronously for the specified TASK to complete in the UI
     thread, then return its result.  Must be called from the Emacs
     thread.  */

  public static <V> V
  syncRunnable (FutureTask<V> task)
  {
    V object;

    EmacsNative.beginSynchronous ();
    SERVICE.runOnUiThread (task);

    try
      {
	object = task.get ();
      }
    catch (ExecutionException exception)
      {
	/* Wrap this exception in a RuntimeException and signal it to
	   the caller.  */
	throw new RuntimeException (exception.getCause ());
      }
    catch (InterruptedException exception)
      {
	EmacsNative.emacsAbort ();
	object = null;
      }

    EmacsNative.endSynchronous ();

    return object;
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
  viewGetSelection (long window)
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

    oldMode = window.view.getICMode ();

    /* If it's not necessary to reset the input connection for ICMODE to
       take effect, return immediately.  */
    if (oldMode == IC_MODE_NULL && icMode == IC_MODE_NULL)
      {
	if (DEBUG_IC)
	  Log.d (TAG, "resetIC: redundant invocation ignored");
	return;
      }

    if (oldMode == icMode
	&& Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU)
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
  updateCursorAnchorInfo (final EmacsWindow window, float x,
			  float y, float yBaseline,
			  float yBottom)
  {
    final CursorAnchorInfo info;
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

    EmacsService.SERVICE.runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  window.view.imManager.updateCursorAnchorInfo (window.view, info);
	}
    });
  }



  /* Content provider functions.  */

  /* Open a content URI described by the bytes BYTES, a non-terminated
     string; make it writable if WRITABLE, and readable if READABLE.
     Truncate the file if TRUNCATE.

     Value is the resulting file descriptor, -1, or an exception will be
     raised.  */

  public int
  openContentUri (String uri, boolean writable, boolean readable,
		  boolean truncate)
    throws FileNotFoundException, IOException
  {
    String name, mode;
    ParcelFileDescriptor fd;
    int i;
    Uri uriObject;

    /* Figure out the file access mode.  */

    mode = "";

    if (readable)
      mode += "r";

    if (writable)
      mode += "w";

    if (truncate)
      mode += "t";

    /* Decode the URI.  It might be possible for a perverse user to
       construct a content file name that Android finds unparsable, so
       punt if the result is NULL.  */

    uriObject = Uri.parse (uri);
    if (uriObject == null)
      return -1;

    /* Try to open a corresponding ParcelFileDescriptor.  Though
       `fd.detachFd' is exclusive to Honeycomb and up, this function is
       never called on systems older than KitKat, which is Emacs's
       minimum requirement for access to /content/by-authority.  */

    fd = resolver.openFileDescriptor (uriObject, mode);
    if (fd == null)
      return -1;

    i = fd.detachFd ();
    fd.close ();

    return i;
  }

  /* Return whether Emacs is directly permitted to access the
     content:// URI NAME.  This is not a suitable test for files which
     Emacs can access by virtue of their containing document
     trees.  */

  public boolean
  checkContentUri (String name, boolean readable, boolean writable)
  {
    Uri uri;
    int rc, flags;

    /* Decode the URI.  It might be possible that perverse user should
       construct a content file name that Android finds unparsable, so
       punt if the result is NULL.  */

    uri = Uri.parse (name);
    if (uri == null)
      return false;

    flags = 0;

    if (readable)
      flags |= Intent.FLAG_GRANT_READ_URI_PERMISSION;

    if (writable)
      flags |= Intent.FLAG_GRANT_WRITE_URI_PERMISSION;

    /* checkCallingUriPermission deals with permissions held by callers
       of functions over the Binder IPC mechanism as contrasted with
       Emacs itself, while getCallingPid and getCallingUid, despite the
       class where they reside, return the process credentials against
       which the system will actually test URIs being opened.  */

    rc = checkUriPermission (uri, Binder.getCallingPid (),
			     Binder.getCallingUid (), flags);
    return rc == PackageManager.PERMISSION_GRANTED;
  }

  /* Return a 8 character checksum for the string STRING, after encoding
     as UTF-8 data.  */

  private static String
  getDisplayNameHash (String string)
  {
    byte[] encoded;
    ByteArrayOutputStream stream;
    int i, ch;

    /* Much of the VFS code expects file names to be encoded as modified
       UTF-8 data, but Android's JNI implementation produces (while not
       accepting!) regular UTF-8 sequences for all characters, even
       non-Emoji ones.  With no documentation to this effect, save for
       two comments nestled in the source code of the Java virtual
       machine, it is not sound to assume that this behavior will not be
       revised in future or modified releases of Android, and as such,
       encode STRING into modified UTF-8 by hand, to protect against
       future changes in this respect.  */

    stream = new ByteArrayOutputStream ();

    for (i = 0; i < string.length (); ++i)
      {
	ch = string.charAt (i);

	if (ch != 0 && ch <= 127)
	  stream.write (ch);
	else if (ch <= 2047)
	  {
	    stream.write (0xc0 | (0x1f & (ch >> 6)));
	    stream.write (0x80 | (0x3f & ch));
	  }
	else
	  {
	    stream.write (0xe0 | (0x0f & (ch >> 12)));
	    stream.write (0x80 | (0x3f & (ch >> 6)));
	    stream.write (0x80 | (0x3f & ch));
	  }
      }

    encoded = stream.toByteArray ();

    /* Closing a ByteArrayOutputStream has no effect.
       encoded.close ();  */

    return EmacsNative.displayNameHash (encoded);
  }

  /* Build a content file name for URI.

     Return a file name within the /contents/by-authority
     pseudo-directory that `android_get_content_name' can then
     transform back into an encoded URI.

     If a display name can be requested from URI (using the resolver
     RESOLVER), append it to this file name.

     A content name consists of any number of unencoded path segments
     separated by `/' characters, possibly followed by a question mark
     and an encoded query string.  */

  public static String
  buildContentName (Uri uri, ContentResolver resolver)
  {
    StringBuilder builder;
    String displayName;
    Cursor cursor;
    int column;

    displayName = null;
    cursor      = null;

    try
      {
	cursor = resolver.query (uri, null, null, null, null);

	if (cursor != null)
	  {
	    cursor.moveToFirst ();
	    column
	      = cursor.getColumnIndexOrThrow (OpenableColumns.DISPLAY_NAME);
	    displayName
	      = cursor.getString (column);

	    /* Verify that the display name is valid, i.e. it
	       contains no characters unsuitable for a file name and
	       is nonempty.  */
	    if (displayName.isEmpty () || displayName.contains ("/"))
	      displayName = null;
	  }
      }
    catch (Exception e)
      {
	/* Ignored.  */
      }
    finally
      {
	if (cursor != null)
	  cursor.close ();
      }

    /* If a display name is available, at this point it should be the
       value of displayName.  */

    builder = new StringBuilder (displayName != null
				 ? "/content/by-authority-named/"
				 : "/content/by-authority/");
    builder.append (uri.getAuthority ());

    /* First, append each path segment.  */

    for (String segment : uri.getPathSegments ())
      {
	/* FIXME: what if segment contains a slash character? */
	builder.append ('/');
	builder.append (uri.encode (segment));
      }

    /* Now, append the query string if necessary.  */

    if (uri.getEncodedQuery () != null)
      builder.append ('?').append (uri.getEncodedQuery ());

    /* Append the display name.  */

    if (displayName != null)
      {
	builder.append ('/');
	builder.append (getDisplayNameHash (displayName));
	builder.append ('/');
	builder.append (displayName);
      }

    return builder.toString ();
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
			currentNow, status, remaining, plugged,
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
			currentNow, status, remaining, plugged,
			temp, };
  }

  public void
  updateExtractedText (EmacsWindow window, ExtractedText text,
		       int token)
  {
    if (DEBUG_IC)
      Log.d (TAG, "updateExtractedText: @" + token + ", " + text);

    icBeginSynchronous ();
    window.view.imManager.updateExtractedText (window.view,
					       token, text);
    icEndSynchronous ();
  }



  /* Document tree management functions.  These functions shouldn't be
     called before Android 5.0.  */

  /* Return an array of each document authority providing at least one
     tree URI that Emacs holds the rights to persistently access.  */

  public String[]
  getDocumentAuthorities ()
  {
    List<UriPermission> permissions;
    HashSet<String> allProviders;
    Uri uri;

    permissions = resolver.getPersistedUriPermissions ();
    allProviders = new HashSet<String> ();

    for (UriPermission permission : permissions)
      {
	uri = permission.getUri ();

	if (DocumentsContract.isTreeUri (uri)
	    && permission.isReadPermission ())
	  allProviders.add (uri.getAuthority ());
      }

    return allProviders.toArray (new String[0]);
  }

  /* Start a file chooser activity to request access to a directory
     tree.

     Value is 1 if the activity couldn't be started for some reason,
     and 0 in any other case.  */

  public int
  requestDirectoryAccess ()
  {
    FutureTask<Integer> task;

    /* Return 1 if Android is too old to support this feature.  */

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP)
      return 1;

    task = new FutureTask<Integer> (new Callable<Integer> () {
	@Override
	public Integer
        call ()
	{
	  EmacsActivity activity;
	  Intent intent;
	  int id, rc;

	  /* Try to obtain an activity that will receive the response
	     from the file chooser dialog.  */

	  if (EmacsActivity.focusedActivities.isEmpty ())
	    {
	      /* If focusedActivities is empty then this dialog may
		 have been displayed immediately after another popup
		 dialog was dismissed.  Try the EmacsActivity to be
		 focused.  */

	      activity = EmacsActivity.lastFocusedActivity;

	      if (activity == null)
		/* Still no luck.  Return failure.  */
		return 1;
	    }
	  else
	    activity = EmacsActivity.focusedActivities.get (0);

	  /* Now create the intent.  */
	  intent = new Intent (Intent.ACTION_OPEN_DOCUMENT_TREE);
	  rc = 1;

	  try
	    {
	      id = EmacsActivity.ACCEPT_DOCUMENT_TREE;
	      activity.startActivityForResult (intent, id, null);
	      rc = 0;
	    }
	  catch (Exception e)
	    {
	      e.printStackTrace ();
	    }

	  return rc;
	}
      });

    return EmacsService.<Integer>syncRunnable (task);
  }

  /* Return an array of each tree provided by the document PROVIDER
     that Emacs has permission to access.

     Value is an array if the provider really does exist, NULL
     otherwise.  */

  public String[]
  getDocumentTrees (String provider)
  {
    List<String> treeList;
    List<UriPermission> permissions;
    Uri uri;

    permissions = resolver.getPersistedUriPermissions ();
    treeList = new ArrayList<String> ();

    for (UriPermission permission : permissions)
      {
	uri = permission.getUri ();

	if (DocumentsContract.isTreeUri (uri)
	    && uri.getAuthority ().equals (provider)
	    && permission.isReadPermission ())
	  /* Make sure the tree document ID is encoded.  Refrain from
	     encoding characters such as +:&?#, since they don't
	     conflict with file name separators or other special
	     characters.  */
	  treeList.add (Uri.encode (DocumentsContract.getTreeDocumentId (uri),
				    " +:&?#"));
      }

    /* The empty string array that is ostensibly allocated to provide
       the first argument provides just the type of the array to be
       returned.  */
    return treeList.toArray (new String[0]);
  }

  /* Find the document ID of the file within TREE_URI designated by
     NAME.

     NAME is a ``file name'' comprised of the display names of
     individual files.  Each constituent component prior to the last
     must name a directory file within TREE_URI.

     Upon success, return 0 or 1 (contingent upon whether or not the
     last component within NAME is a directory) and place the document
     ID of the named file in ID_RETURN[0].

     If the designated file can't be located, but each component of
     NAME up to the last component can and is a directory, return -2
     and the ID of the last component located in ID_RETURN[0].

     If the designated file can't be located, return -1, or signal one
     of OperationCanceledException, SecurityException,
     FileNotFoundException, or UnsupportedOperationException.  */

  public int
  documentIdFromName (String tree_uri, String name, String[] id_return)
  {
    /* Start the thread used to run SAF requests if it isn't already
       running.  */

    if (storageThread == null)
      {
	storageThread = new EmacsSafThread (resolver);
	storageThread.start ();
      }

    return storageThread.documentIdFromName (tree_uri, name,
					     id_return);
  }

  /* Return an encoded document URI representing a tree with the
     specified IDENTIFIER supplied by the authority AUTHORITY.

     Return null instead if Emacs does not have permanent access
     to the specified document tree recorded on disk.  */

  public String
  getTreeUri (String tree, String authority)
  {
    Uri uri, grantedUri;
    List<UriPermission> permissions;

    /* First, build the URI.  */
    tree = Uri.decode (tree);
    uri = DocumentsContract.buildTreeDocumentUri (authority, tree);

    /* Now, search for it within the list of persisted URI
       permissions.  */
    permissions = resolver.getPersistedUriPermissions ();

    for (UriPermission permission : permissions)
      {
	/* If the permission doesn't entitle Emacs to read access,
	   skip it.  */

	if (!permission.isReadPermission ())
	  continue;

        grantedUri = permission.getUri ();

	if (grantedUri.equals (uri))
	  return uri.toString ();
      }

    /* Emacs doesn't have permission to access this tree URI.  */
    return null;
  }

  /* Return file status for the document designated by the given
     DOCUMENTID and tree URI.  If DOCUMENTID is NULL, use the document
     ID in URI itself.

     Value is null upon failure, or an array of longs [MODE, SIZE,
     MTIM] upon success, where MODE contains the file type and access
     modes of the file as in `struct stat', SIZE is the size of the
     file in BYTES or -1 if not known, and MTIM is the time of the
     last modification to this file in milliseconds since 00:00,
     January 1st, 1970.

     If NOCACHE, refrain from placing the file status within the
     status cache.

     OperationCanceledException and other typical exceptions may be
     signaled upon receiving async input or other errors.  */

  public long[]
  statDocument (String uri, String documentId, boolean noCache)
  {
    /* Start the thread used to run SAF requests if it isn't already
       running.  */

    if (storageThread == null)
      {
	storageThread = new EmacsSafThread (resolver);
	storageThread.start ();
      }

    return storageThread.statDocument (uri, documentId, noCache);
  }

  /* Find out whether Emacs has access to the document designated by
     the specified DOCUMENTID within the tree URI.  If DOCUMENTID is
     NULL, use the document ID in URI itself.

     If WRITABLE, also check that the file is writable, which is true
     if it is either a directory or its flags contains
     FLAG_SUPPORTS_WRITE.

     Value is 0 if the file is accessible, and one of the following if
     not:

       -1, if the file does not exist.
       -2, if WRITABLE and the file is not writable.
       -3, upon any other error.

     In addition, arbitrary runtime exceptions (such as
     SecurityException or UnsupportedOperationException) may be
     thrown.  */

  public int
  accessDocument (String uri, String documentId, boolean writable)
  {
    /* Start the thread used to run SAF requests if it isn't already
       running.  */

    if (storageThread == null)
      {
	storageThread = new EmacsSafThread (resolver);
	storageThread.start ();
      }

    return storageThread.accessDocument (uri, documentId, writable);
  }

  /* Open a cursor representing each entry within the directory
     designated by the specified DOCUMENTID within the tree URI.

     If DOCUMENTID is NULL, use the document ID within URI itself.
     Value is NULL upon failure.

     In addition, arbitrary runtime exceptions (such as
     SecurityException or UnsupportedOperationException) may be
     thrown.  */

  public Cursor
  openDocumentDirectory (String uri, String documentId)
  {
    /* Start the thread used to run SAF requests if it isn't already
       running.  */

    if (storageThread == null)
      {
	storageThread = new EmacsSafThread (resolver);
	storageThread.start ();
      }

    return storageThread.openDocumentDirectory (uri, documentId);
  }

  /* Read a single directory entry from the specified CURSOR.  Return
     NULL if at the end of the directory stream, and a directory entry
     with `d_name' set to NULL if an error occurs.  */

  public EmacsDirectoryEntry
  readDirectoryEntry (Cursor cursor)
  {
    EmacsDirectoryEntry entry;
    int index;
    String name, type;

    entry = new EmacsDirectoryEntry ();

    while (true)
      {
	if (!cursor.moveToNext ())
	  return null;

	/* First, retrieve the display name.  */
	index = cursor.getColumnIndex (Document.COLUMN_DISPLAY_NAME);

	if (index < 0)
	  /* Return an invalid directory entry upon failure.  */
	  return entry;

	try
	  {
	    name = cursor.getString (index);
	  }
	catch (Exception exception)
	  {
	    return entry;
	  }

	/* Skip this entry if its name cannot be represented.  NAME
	   can still be null here, since some Cursors are permitted to
	   return NULL if INDEX is not a string.  */

	if (name == null || name.equals ("..")
	    || name.equals (".") || name.contains ("/")
	    || name.contains ("\0"))
	  continue;

	/* Now, look for its type.  */

	index = cursor.getColumnIndex (Document.COLUMN_MIME_TYPE);

	if (index < 0)
	  /* Return an invalid directory entry upon failure.  */
	  return entry;

	try
	  {
	    type = cursor.getString (index);
	  }
	catch (Exception exception)
	  {
	    return entry;
	  }

	if (type != null
	    && type.equals (Document.MIME_TYPE_DIR))
	  entry.d_type = 1;
	entry.d_name = name;
	return entry;
      }

    /* Not reached.  */
  }

  /* Open a file descriptor for a file document designated by
     DOCUMENTID within the document tree identified by URI.  If
     TRUNCATE and the document already exists, truncate its contents
     before returning.

     If READ && WRITE, open the file under either the `rw' or `rwt'
     access mode, which implies that the value must be a seekable
     on-disk file.  If TRUNC && WRITE, also truncate the file after it
     is opened.

     If only READ or WRITE is set, value may be a non-seekable FIFO or
     one end of a socket pair.

     Value is NULL upon failure or a parcel file descriptor upon
     success.  Call `ParcelFileDescriptor.close' on this file
     descriptor instead of using the `close' system call.

     FileNotFoundException and/or SecurityException and
     UnsupportedOperationException may be thrown upon failure.  */

  public ParcelFileDescriptor
  openDocument (String uri, String documentId,
		boolean read, boolean write, boolean truncate)
  {
    /* Start the thread used to run SAF requests if it isn't already
       running.  */

    if (storageThread == null)
      {
	storageThread = new EmacsSafThread (resolver);
	storageThread.start ();
      }

    return storageThread.openDocument (uri, documentId, read, write,
				       truncate);
  }

  /* Create a new document with the given display NAME within the
     directory identified by DOCUMENTID inside the document tree
     designated by URI.

     If DOCUMENTID is NULL, create the document inside the root of
     that tree.

     Either FileNotFoundException, SecurityException or
     UnsupportedOperationException may be thrown upon failure.

     Return the document ID of the new file upon success, NULL
     otherwise.  */

  public String
  createDocument (String uri, String documentId, String name)
    throws FileNotFoundException
  {
    String mimeType, separator, mime, extension;
    int index;
    MimeTypeMap singleton;
    Uri treeUri, directoryUri, docUri;

    /* Try to get the MIME type for this document.
       Default to ``application/octet-stream''.  */

    mimeType = "application/octet-stream";

    /* Abuse WebView stuff to get the file's MIME type.  */

    index = name.lastIndexOf ('.');

    if (index > 0)
      {
	singleton = MimeTypeMap.getSingleton ();
	extension = name.substring (index + 1);
	mime = singleton.getMimeTypeFromExtension (extension);

	if (mime != null)
	  mimeType = mime;
      }

    /* Now parse URI.  */
    treeUri = Uri.parse (uri);

    if (documentId == null)
      documentId = DocumentsContract.getTreeDocumentId (treeUri);

    /* And build a file URI referring to the directory.  */

    directoryUri
      = DocumentsContract.buildChildDocumentsUriUsingTree (treeUri,
							   documentId);

    docUri = DocumentsContract.createDocument (resolver,
					       directoryUri,
					       mimeType, name);

    if (docUri == null)
      return null;

    /* Invalidate the file status of the containing directory.  */

    if (storageThread != null)
      storageThread.postInvalidateStat (treeUri, documentId);

    /* Return the ID of the new document.  */
    return DocumentsContract.getDocumentId (docUri);
  }

  /* Like `createDocument', but create a directory instead of an
     ordinary document.  */

  public String
  createDirectory (String uri, String documentId, String name)
    throws FileNotFoundException
  {
    int index;
    Uri treeUri, directoryUri, docUri;

    /* Now parse URI.  */
    treeUri = Uri.parse (uri);

    if (documentId == null)
      documentId = DocumentsContract.getTreeDocumentId (treeUri);

    /* And build a file URI referring to the directory.  */

    directoryUri
      = DocumentsContract.buildChildDocumentsUriUsingTree (treeUri,
							   documentId);

    /* If name ends with a directory separator character, delete
       it.  */

    if (name.endsWith ("/"))
      name = name.substring (0, name.length () - 1);

    /* From Android's perspective, directories are just ordinary
       documents with the `MIME_TYPE_DIR' type.  */

    docUri = DocumentsContract.createDocument (resolver,
					       directoryUri,
					       Document.MIME_TYPE_DIR,
					       name);

    if (docUri == null)
      return null;

    /* Return the ID of the new document, but first invalidate the
       state of the containing directory.  */

    if (storageThread != null)
      storageThread.postInvalidateStat (treeUri, documentId);

    return DocumentsContract.getDocumentId (docUri);
  }

  /* Delete the document identified by ID from the document tree
     identified by URI.  Return 0 upon success and -1 upon
     failure.

     NAME should be the name of the document being deleted, and is
     used to invalidate the cache.  */

  public int
  deleteDocument (String uri, String id, String name)
    throws FileNotFoundException
  {
    Uri uriObject, tree;

    tree = Uri.parse (uri);
    uriObject = DocumentsContract.buildDocumentUriUsingTree (tree, id);

    if (DocumentsContract.deleteDocument (resolver, uriObject))
      {
	if (storageThread != null)
	  storageThread.postInvalidateCache (tree, id, name);

	return 0;
      }

    return -1;
  }

  /* Rename the document designated by DOCID inside the directory tree
     identified by URI, which should be within the directory
     designated by DIR, to NAME.  If the file can't be renamed because
     it doesn't support renaming, return -1, 0 otherwise.  */

  public int
  renameDocument (String uri, String docId, String dir, String name)
    throws FileNotFoundException
  {
    Uri tree, uriObject;

    tree = Uri.parse (uri);
    uriObject = DocumentsContract.buildDocumentUriUsingTree (tree, docId);

    if (DocumentsContract.renameDocument (resolver, uriObject,
					  name)
	!= null)
      {
	/* Invalidate the cache.  */
	if (storageThread != null)
	  storageThread.postInvalidateCacheDir (tree, docId,
						name);
	return 0;
      }

    /* Handle errors specially, so `android_saf_rename_document' can
       return ENXDEV.  */
    return -1;
  }

  /* Move the document designated by DOCID from the directory under
     DIR_NAME designated by SRCID to the directory designated by
     DSTID.  If the ID of the document being moved changes as a
     consequence of the movement, return the new ID, else NULL.

     URI is the document tree containing all three documents.  */

  public String
  moveDocument (String uri, String docId, String dirName,
		String dstId, String srcId)
    throws FileNotFoundException
  {
    Uri uri1, docId1, dstId1, srcId1;
    Uri name;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N)
      throw new UnsupportedOperationException ("Documents aren't capable"
					       + " of being moved on Android"
					       + " versions before 7.0.");

    uri1 = Uri.parse (uri);
    docId1 = DocumentsContract.buildDocumentUriUsingTree (uri1, docId);
    dstId1 = DocumentsContract.buildDocumentUriUsingTree (uri1, dstId);
    srcId1 = DocumentsContract.buildDocumentUriUsingTree (uri1, srcId);

    /* Move the document; this function returns the new ID of the
       document should it change.  */
    name = DocumentsContract.moveDocument (resolver, docId1,
					   srcId1, dstId1);

    /* Now invalidate the caches for both DIRNAME and DOCID.  */

    if (storageThread != null)
      {
	storageThread.postInvalidateCacheDir (uri1, docId, dirName);

	/* Invalidate the stat cache entries for both the source and
	   destination directories, since their contents have
	   changed.  */
	storageThread.postInvalidateStat (uri1, dstId);
	storageThread.postInvalidateStat (uri1, srcId);
      }

    return (name != null
	    ? DocumentsContract.getDocumentId (name)
	    : null);
  }

  /* Return if there is a content provider by the name of AUTHORITY
     supplying at least one tree URI Emacs retains persistent rights
     to access.  */

  public boolean
  validAuthority (String authority)
  {
    List<UriPermission> permissions;
    Uri uri;

    permissions = resolver.getPersistedUriPermissions ();

    for (UriPermission permission : permissions)
      {
	uri = permission.getUri ();

	if (DocumentsContract.isTreeUri (uri)
	    && permission.isReadPermission ()
	    && uri.getAuthority ().equals (authority))
	  return true;
      }

    return false;
  }

  /* Relinquish authorization for read and write access to the provided
     URI, which is generally a reference to a directory tree.  */

  public void
  relinquishUriRights (String uri)
  {
    Uri uri1;
    int flags;

    uri1 = Uri.parse (uri);
    flags = (Intent.FLAG_GRANT_READ_URI_PERMISSION
	     | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
    resolver.releasePersistableUriPermission (uri1, flags);
  }



  /* Functions for detecting and requesting storage permissions.  */

  public boolean
  externalStorageAvailable ()
  {
    final String readPermission;

    readPermission = "android.permission.READ_EXTERNAL_STORAGE";

    return (Build.VERSION.SDK_INT < Build.VERSION_CODES.R
	    ? (checkSelfPermission (readPermission)
	       == PackageManager.PERMISSION_GRANTED)
	    : Environment.isExternalStorageManager ());
  }

  private void
  requestStorageAccess23 ()
  {
    Runnable runnable;

    runnable = new Runnable () {
	@Override
	public void
	run ()
	{
	  EmacsActivity activity;
	  String permission, permission1;

	  permission = "android.permission.READ_EXTERNAL_STORAGE";
	  permission1 = "android.permission.WRITE_EXTERNAL_STORAGE";

	  /* Find an activity that is entitled to display a permission
	     request dialog.  */

	  if (EmacsActivity.focusedActivities.isEmpty ())
	    {
	      /* If focusedActivities is empty then this dialog may
		 have been displayed immediately after another popup
		 dialog was dismissed.  Try the EmacsActivity to be
		 focused.  */

	      activity = EmacsActivity.lastFocusedActivity;

	      if (activity == null)
		{
		  /* Still no luck.  Return failure.  */
		  return;
		}
	    }
	  else
	    activity = EmacsActivity.focusedActivities.get (0);

	  /* Now request these permissions.  */
	  activity.requestPermissions (new String[] { permission,
						      permission1, },
				       0);
	}
      };

    runOnUiThread (runnable);
  }

  private void
  requestStorageAccess30 ()
  {
    Runnable runnable;
    final Intent intent;

    intent
      = new Intent (Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION,
		    Uri.parse ("package:org.gnu.emacs"));

    runnable = new Runnable () {
	@Override
	public void
	run ()
	{
	  EmacsActivity activity;

	  /* Find an activity that is entitled to display a permission
	     request dialog.  */

	  if (EmacsActivity.focusedActivities.isEmpty ())
	    {
	      /* If focusedActivities is empty then this dialog may
		 have been displayed immediately after another popup
		 dialog was dismissed.  Try the EmacsActivity to be
		 focused.  */

	      activity = EmacsActivity.lastFocusedActivity;

	      if (activity == null)
		{
		  /* Still no luck.  Return failure.  */
		  return;
		}
	    }
	  else
	    activity = EmacsActivity.focusedActivities.get (0);

	  /* Now request these permissions.  */

	  try
	    {
	      activity.startActivity (intent);
	    }
	  catch (ActivityNotFoundException exception)
	    {
	      Log.w (TAG, "Failed to request storage access permissions: ");
	      exception.printStackTrace ();
	    }
	}
      };

    runOnUiThread (runnable);
  }

  public void
  requestStorageAccess ()
  {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.R)
      requestStorageAccess23 ();
    else
      requestStorageAccess30 ();
  }



  /* Notification miscellany.  */

  /* Cancel any notification displayed with the tag TAG.  */

  public void
  cancelNotification (final String string)
  {
    Object tem;
    final NotificationManager manager;

    tem = getSystemService (Context.NOTIFICATION_SERVICE);
    manager = (NotificationManager) tem;

    runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  manager.cancel (string, 2);
	}
      });
  }
};
