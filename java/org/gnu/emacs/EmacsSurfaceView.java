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

import android.view.SurfaceView;
import android.view.SurfaceHolder;

import android.os.Build;

import android.graphics.Canvas;
import android.graphics.Rect;

import android.util.Log;

public class EmacsSurfaceView extends SurfaceView
{
  private static final String TAG = "EmacsSurfaceView";
  public Object surfaceChangeLock;
  private boolean created;
  private EmacsView view;

  /* This is the callback used on Android 8 to 25.  */

  private class Callback implements SurfaceHolder.Callback
  {
    @Override
    public void
    surfaceChanged (SurfaceHolder holder, int format,
		    int width, int height)
    {
      Log.d (TAG, "surfaceChanged: " + view + ", " + view.pendingConfigure);

      /* Make sure not to swap buffers if there is pending
	 configuration, because otherwise the redraw callback will not
	 run correctly.  */

      if (view.pendingConfigure == 0)
	view.swapBuffers ();
    }

    @Override
    public void
    surfaceCreated (SurfaceHolder holder)
    {
      synchronized (surfaceChangeLock)
	{
	  Log.d (TAG, "surfaceCreated: " + view);
	  created = true;
	}

      /* Drop the lock when doing this, or a deadlock can
	 result.  */
      view.swapBuffers ();
    }

    @Override
    public void
    surfaceDestroyed (SurfaceHolder holder)
    {
      synchronized (surfaceChangeLock)
	{
	  Log.d (TAG, "surfaceDestroyed: " + view);
	  created = false;
	}
    }
  }

  /* And this is the callback used on Android 26 and later.  It is
     used because it can tell the system when drawing completes.  */

  private class Callback2 extends Callback implements SurfaceHolder.Callback2
  {
    @Override
    public void
    surfaceRedrawNeeded (SurfaceHolder holder)
    {
      /* This version is not supported.  */
      return;
    }

    @Override
    public void
    surfaceRedrawNeededAsync (SurfaceHolder holder,
			      Runnable drawingFinished)
    {
      Runnable old;

      Log.d (TAG, "surfaceRedrawNeededAsync: " + view.pendingConfigure);

      /* The system calls this function when it wants to know whether
	 or not Emacs is still configuring itself in response to a
	 resize.

         If the view did not send an outstanding ConfigureNotify
         event, then call drawingFinish immediately.  Else, give it to
         the view to execute after drawing completes.  */

      if (view.pendingConfigure == 0)
	drawingFinished.run ();
      else
	/* And set this runnable to run once drawing completes.  */
	view.drawingFinished = drawingFinished;
    }
  }

  public
  EmacsSurfaceView (final EmacsView view)
  {
    super (view.getContext ());

    this.surfaceChangeLock = new Object ();
    this.view = view;

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O)
      getHolder ().addCallback (new Callback ());
    else
      getHolder ().addCallback (new Callback2 ());
  }

  public boolean
  isCreated ()
  {
    return created;
  }

  public Canvas
  lockCanvas (Rect damage)
  {
    SurfaceHolder holder;

    holder = getHolder ();

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
      {
	damage.setEmpty ();
	return holder.lockHardwareCanvas ();
      }

    return holder.lockCanvas (damage);
  }

  @Override
  protected void
  onLayout (boolean changed, int left, int top, int right,
	    int bottom)
  {
    Log.d (TAG, ("onLayout: " + left + " " + top + " " + right
		 + " " + bottom + " -- " + changed + " visibility "
		 + getVisibility ()));
  }

  /* This method is only used during debugging when it seems damage
     isn't working correctly.  */

  public Canvas
  lockCanvas ()
  {
    SurfaceHolder holder;

    holder = getHolder ();
    return holder.lockCanvas ();
  }

  public void
  unlockCanvasAndPost (Canvas canvas)
  {
    SurfaceHolder holder;

    holder = getHolder ();
    holder.unlockCanvasAndPost (canvas);
  }
};
