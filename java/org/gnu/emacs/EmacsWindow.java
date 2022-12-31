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

import java.lang.IllegalStateException;
import java.util.ArrayList;
import java.util.List;

import android.graphics.Rect;
import android.graphics.Canvas;
import android.graphics.Bitmap;
import android.graphics.Point;

import android.view.View;
import android.view.ViewGroup;
import android.view.KeyEvent;

/* This defines a window, which is a handle.  Windows represent a
   rectangular subset of the screen with their own contents.

   Windows either have a parent window, in which case their views are
   attached to the parent's view, or are "floating", in which case
   their views are attached to the parent activity (if any), else
   nothing.

   Views are also drawables, meaning they can accept drawing
   requests.  */

public class EmacsWindow extends EmacsHandleObject
  implements EmacsDrawable
{
  /* The view associated with the window.  */
  public EmacsView view;

  /* The geometry of the window.  */
  private Rect rect;

  /* The parent window, or null if it is the root window.  */
  private EmacsWindow parent;

  /* List of all children in stacking order.  This must be kept
     consistent!  */
  private ArrayList<EmacsWindow> children;

  /* The EmacsActivity currently attached, if it exists.  */
  private EmacsActivity attached;

  /* The window background scratch GC.  foreground is always the
     window background.  */
  private EmacsGC scratchGC;

  public
  EmacsWindow (short handle, final EmacsWindow parent, int x, int y,
	       int width, int height)
  {
    super (handle);

    rect = new Rect (x, y, x + width, y + height);

    /* Create the view from the context's UI thread.  */
    view = EmacsService.SERVICE.getEmacsView (this);
    this.parent = parent;
    children = new ArrayList<EmacsWindow> ();

    /* The window is unmapped by default.  */
    view.setVisibility (View.GONE);

    /* If parent is the root window, notice that there are new
       children available for interested activites to pick up.  */
    if (parent == null)
      EmacsService.SERVICE.noticeAvailableChild (this);
    else
      {
	/* Otherwise, directly add this window as a child of that
	   window's view.  */
	synchronized (parent)
	  {
	    parent.children.add (this);
	    parent.view.post (new Runnable () {
		@Override
		public void
		run ()
		{
		  parent.view.addView (view);
		}
	      });
	  }
      }

    scratchGC = new EmacsGC ((short) 0);
  }

  public void
  changeWindowBackground (int pixel)
  {
    /* scratchGC is used as the argument to a FillRectangles req.  */
    scratchGC.foreground = pixel;
    scratchGC.markDirty ();
  }

  public Rect
  getGeometry ()
  {
    synchronized (this)
      {
	/* Huh, this is it.  */
	return rect;
      }
  }

  @Override
  public void
  destroyHandle () throws IllegalStateException
  {
    synchronized (this)
      {
	if (!children.isEmpty ())
	  throw new IllegalStateException ("Trying to destroy window with "
					   + "children!");
      }

    /* Notice that the child has been destroyed.  */
    EmacsService.SERVICE.noticeChildDestroyed (this);

    /* Remove the view from its parent and make it invisible.  */
    view.post (new Runnable () {
	public void
	run ()
	{
	  view.setVisibility (View.GONE);

	  if (view.getParent () != null)
	    ((ViewGroup) view.getParent ()).removeView (view);

	  if (attached != null)
	    attached.makeAvailable ();
	}
      });

    super.destroyHandle ();
  }

  public void
  setActivity (EmacsActivity activity)
  {
    synchronized (this)
      {
	activity = activity;
      }
  }

  public void
  viewLayout (int left, int top, int right, int bottom)
  {
    synchronized (this)
      {
	rect.left = left;
	rect.top = top;
	rect.right = right;
	rect.bottom = bottom;

	EmacsNative.sendConfigureNotify (this.handle,
					 System.currentTimeMillis (),
					 left, top, rect.width (),
					 rect.height ());
      }
  }

  public void
  requestViewLayout ()
  {
    view.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  view.requestLayout ();
	}
      });
  }

  public void
  resizeWindow (int width, int height)
  {
    synchronized (this)
      {
	rect.right = rect.left + width;
	rect.bottom = rect.top + height;
      }
  }

  public void
  moveWindow (int x, int y)
  {
    int width, height;

    synchronized (this)
      {
	width = rect.width ();
	height = rect.height ();

	rect.left = x;
	rect.top = y;
	rect.right = x + width;
	rect.bottom = y + height;

	requestViewLayout ();
      }
  }

  public void
  mapWindow ()
  {
    view.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  view.setVisibility (View.VISIBLE);
	}
      });
  }

  public void
  unmapWindow ()
  {
    view.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  view.setVisibility (View.GONE);
	}
      });
  }

  @Override
  public Canvas
  lockCanvas ()
  {
    if (view.canvas != null)
      return view.canvas;

    return null;
  }

  @Override
  public void
  unlockCanvas ()
  {

  }

  @Override
  public void
  damageRect (Rect damageRect)
  {
    view.damageRect (damageRect);
  }

  public void
  swapBuffers ()
  {
    /* Before calling swapBuffers, make sure to flush the paint
       queue.  */
    EmacsService.SERVICE.flushPaintQueue ();
    view.post (new Runnable () {
	@Override
	public void
	run ()
	{
	  view.swapBuffers ();
	}
      });
  }

  public void
  clearWindow ()
  {
    synchronized (this)
      {
	EmacsService.SERVICE.fillRectangle (this, scratchGC,
					    0, 0, rect.width (),
					    rect.height ());
      }
  }

  public void
  clearArea (int x, int y, int width, int height)
  {
    EmacsService.SERVICE.fillRectangle (this, scratchGC,
					x, y, width, height);
  }

  @Override
  public Bitmap
  getBitmap ()
  {
    return view.bitmap;
  }

  public void
  onKeyDown (int keyCode, KeyEvent event)
  {
    EmacsNative.sendKeyPress (this.handle,
			      event.getEventTime (),
			      event.getModifiers (),
			      keyCode);
  }

  public void
  onKeyUp (int keyCode, KeyEvent event)
  {
    EmacsNative.sendKeyRelease (this.handle,
				event.getEventTime (),
				event.getModifiers (),
				keyCode);
  }
};
