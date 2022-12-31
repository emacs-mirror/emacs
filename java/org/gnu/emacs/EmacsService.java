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

import java.lang.Runnable;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

import android.graphics.Canvas;
import android.graphics.Bitmap;
import android.graphics.Point;

import android.annotation.TargetApi;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.res.AssetManager;
import android.os.Build;
import android.os.Looper;
import android.os.IBinder;
import android.os.Handler;
import android.util.Log;

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

  private EmacsThread thread;
  private Handler handler;
  private EmacsPaintQueue paintQueue;

  /* List of all EmacsWindows that are available to attach to an
     activity.  */
  public static List<EmacsWindow> availableChildren;

  static
  {
    availableChildren = new ArrayList<EmacsWindow> ();
  };

  @Override
  public int
  onStartCommand (Intent intent, int flags, int startId)
  {
    return START_NOT_STICKY;
  }

  @Override
  public IBinder
  onBind (Intent intent)
  {
    return null;
  }

  @TargetApi (Build.VERSION_CODES.GINGERBREAD)
  String
  getLibraryDirectory ()
  {
    int apiLevel;
    Context context;

    context = getApplicationContext ();
    apiLevel = android.os.Build.VERSION.SDK_INT;

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
    String filesDir, libDir;

    SERVICE = this;
    handler = new Handler (Looper.getMainLooper ());
    manager = getAssets ();
    app_context = getApplicationContext ();

    try
      {
	/* Configure Emacs with the asset manager and other necessary
	   parameters.  */
	filesDir = app_context.getFilesDir ().getCanonicalPath ();
	libDir = getLibraryDirectory ();

	Log.d (TAG, "Initializing Emacs, where filesDir = " + filesDir
	       + " and libDir = " + libDir);

	EmacsNative.setEmacsParams (manager, filesDir, libDir,
				    this);

	/* Start the thread that runs Emacs.  */
	thread = new EmacsThread (this);
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

  void
  runOnUiThread (Runnable runnable)
  {
    handler.post (runnable);
  }

  EmacsView
  getEmacsView (final EmacsWindow window)
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

  /* Notice that a child of the root window named WINDOW is now
     available for attachment to a specific activity.  */

  public void
  noticeAvailableChild (final EmacsWindow window)
  {
    Log.d (TAG, "A new child is available: " + window);

    handler.post (new Runnable () {
	public void
	run ()
	{
	  for (EmacsActivity activity
		 : EmacsActivity.availableActivities)
	    {
	      /* TODO: check if the activity matches.  */
	      activity.attachChild (window);
	      break;
	    }

	  /* Nope, wait for an activity to become available.  */
	  availableChildren.add (window);
	}
      });
  }

  /* Notice that a child of the root window named WINDOW has been
     destroyed.  */

  public void
  noticeChildDestroyed (final EmacsWindow child)
  {
    handler.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  availableChildren.remove (child);
	}
      });
  }

  /* X drawing operations.  These are quite primitive operations.  The
     drawing queue is kept on the Emacs thread, but is periodically
     flushed to the application thread, upon buffers swaps and once it
     gets too big.  */



  private void
  ensurePaintQueue ()
  {
    if (paintQueue == null)
      paintQueue = new EmacsPaintQueue ();
  }

  public void
  flushPaintQueue ()
  {
    final EmacsPaintQueue queue;

    if (paintQueue == null)
      return;

    if (paintQueue.numRequests < 1)
      /* No requests to flush.  */
      return;

    queue = paintQueue;

    handler.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  queue.run ();
	}
      });

    /* Clear the paint queue.  */
    paintQueue = null;
  }

  private void
  checkFlush ()
  {
    if (paintQueue != null
	&& paintQueue.numRequests > MAX_PENDING_REQUESTS)
      flushPaintQueue ();
  }

  public void
  fillRectangle (EmacsDrawable drawable, EmacsGC gc,
		 int x, int y, int width, int height)
  {
    EmacsPaintReq req;

    ensurePaintQueue ();

    req = new EmacsFillRectangle (drawable, x, y,
				  width, height,
				  gc.immutableGC ());
    paintQueue.appendPaintOperation (req);
    checkFlush ();
  }

  public void
  fillPolygon (EmacsDrawable drawable, EmacsGC gc,
	       Point points[])
  {
    EmacsPaintReq req;

    ensurePaintQueue ();

    req = new EmacsFillPolygon (drawable, points,
				gc.immutableGC ());
    paintQueue.appendPaintOperation (req);
    checkFlush ();
  }

  public void
  drawRectangle (EmacsDrawable drawable, EmacsGC gc,
		 int x, int y, int width, int height)
  {
    EmacsPaintReq req;

    ensurePaintQueue ();

    if (gc.clip_rects != null && gc.clip_rects.length >= 1)
      android.util.Log.d ("drawRectangle",
			  gc.clip_rects[0].toString ()
			  + " " + gc.toString ());

    req = new EmacsDrawRectangle (drawable, x, y,
				  width, height,
				  gc.immutableGC ());
    paintQueue.appendPaintOperation (req);
    checkFlush ();
  }

  public void
  drawLine (EmacsDrawable drawable, EmacsGC gc,
	    int x, int y, int x2, int y2)
  {
    EmacsPaintReq req;

    ensurePaintQueue ();

    req = new EmacsDrawLine (drawable, x, y,
			     x2, y2,
			     gc.immutableGC ());
    paintQueue.appendPaintOperation (req);
    checkFlush ();
  }

  public void
  drawPoint (EmacsDrawable drawable, EmacsGC gc,
	     int x, int y)
  {
    EmacsPaintReq req;

    ensurePaintQueue ();

    req = new EmacsDrawPoint (drawable, x, y,
			      gc.immutableGC ());
    paintQueue.appendPaintOperation (req);
    checkFlush ();
  }

  public void
  copyArea (EmacsDrawable srcDrawable, EmacsDrawable dstDrawable,
	    EmacsGC gc,
	    int srcX, int srcY, int width, int height, int destX,
	    int destY)
  {
    EmacsPaintReq req;

    ensurePaintQueue ();

    req = new EmacsCopyArea (srcDrawable, dstDrawable,
			     srcX, srcY, width, height, destX,
			     destY, gc.immutableGC ());
    paintQueue.appendPaintOperation (req);
    checkFlush ();
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
};
