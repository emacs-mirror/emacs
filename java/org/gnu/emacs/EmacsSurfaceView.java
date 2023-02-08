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
      Canvas canvas;

      Log.d (TAG, "surfaceChanged: " + view + ", ");

      view.swapBuffers (true);
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
      view.swapBuffers (true);
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

  public
  EmacsSurfaceView (final EmacsView view)
  {
    super (view.getContext ());

    this.surfaceChangeLock = new Object ();
    this.view = view;

    getHolder ().addCallback (new Callback ());
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
