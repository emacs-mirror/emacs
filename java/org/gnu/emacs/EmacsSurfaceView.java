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

public class EmacsSurfaceView extends SurfaceView
{
  public Object surfaceChangeLock;
  private boolean created;

  public
  EmacsSurfaceView (final EmacsView view)
  {
    super (view.getContext ());

    surfaceChangeLock = new Object ();

    getHolder ().addCallback (new SurfaceHolder.Callback () {
	@Override
	public void
	surfaceChanged (SurfaceHolder holder, int format,
			int width, int height)
	{
	  view.swapBuffers ();
	}

	@Override
	public void
	surfaceCreated (SurfaceHolder holder)
	{
	  synchronized (surfaceChangeLock)
	    {
	      created = true;
	      view.swapBuffers ();
	    }
	}

	@Override
	public void
	surfaceDestroyed (SurfaceHolder holder)
	{
	  synchronized (surfaceChangeLock)
	    {
	      created = false;
	    }
	}
      });
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
