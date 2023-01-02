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

import android.content.Intent;

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
  public ArrayList<EmacsWindow> children;

  /* The window consumer currently attached, if it exists.  */
  private EmacsWindowAttachmentManager.WindowConsumer attached;

  /* The window background scratch GC.  foreground is always the
     window background.  */
  private EmacsGC scratchGC;

  public
  EmacsWindow (short handle, final EmacsWindow parent, int x, int y,
	       int width, int height)
  {
    super (handle);

    rect = new Rect (x, y, x + width, y + height);

    /* Create the view from the context's UI thread.  The window is
       unmapped, so the view is GONE.  */
    view = EmacsService.SERVICE.getEmacsView (this, View.GONE,
					      parent == null);
    this.parent = parent;

    /* Create the list of children.  */
    children = new ArrayList<EmacsWindow> ();

    if (parent != null)
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
    else
      EmacsService.SERVICE.runOnUiThread (new Runnable () {
	  @Override
	  public void
	  run ()
	  {
	    EmacsWindowAttachmentManager manager;

	    manager = EmacsWindowAttachmentManager.MANAGER;

	    /* If parent is the root window, notice that there are new
	       children available for interested activites to pick
	       up.  */

	    manager.registerWindow (EmacsWindow.this);
	  }
	});

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
    if (parent != null)
      parent.children.remove (this);

    EmacsActivity.invalidateFocus ();

    if (!children.isEmpty ())
      throw new IllegalStateException ("Trying to destroy window with "
				       + "children!");

    /* Remove the view from its parent and make it invisible.  */
    view.post (new Runnable () {
	public void
	run ()
	{
	  View parent;
	  EmacsWindowAttachmentManager manager;

	  if (EmacsActivity.focusedWindow == EmacsWindow.this)
	    EmacsActivity.focusedWindow = null;

	  manager = EmacsWindowAttachmentManager.MANAGER;
	  view.setVisibility (View.GONE);

	  parent = (View) view.getParent ();

	  if (parent != null && attached == null)
	    ((ViewGroup) parent).removeView (view);

	  manager.detachWindow (EmacsWindow.this);
	}
      });

    super.destroyHandle ();
  }

  public void
  setConsumer (EmacsWindowAttachmentManager.WindowConsumer consumer)
  {
    attached = consumer;
  }

  public EmacsWindowAttachmentManager.WindowConsumer
  getAttachedConsumer ()
  {
    return attached;
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
	  /* Eventually this should check no-focus-on-map.  */
	  view.requestFocus ();
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
    int state;

    state = event.getModifiers ();
    state &= ~(KeyEvent.META_ALT_MASK | KeyEvent.META_CTRL_MASK);

    EmacsNative.sendKeyPress (this.handle,
			      event.getEventTime (),
			      event.getModifiers (),
			      keyCode,
			      /* Ignore meta-state understood by Emacs
				 for now, or Ctrl+C will not be
				 recognized as an ASCII key press
				 event.  */
			      event.getUnicodeChar (state));
  }

  public void
  onKeyUp (int keyCode, KeyEvent event)
  {
    int state;

    state = event.getModifiers ();
    state &= ~(KeyEvent.META_ALT_MASK | KeyEvent.META_CTRL_MASK);

    EmacsNative.sendKeyRelease (this.handle,
				event.getEventTime (),
				event.getModifiers (),
				keyCode,
				event.getUnicodeChar (state));
  }

  public void
  onFocusChanged (boolean gainFocus)
  {
    EmacsActivity.invalidateFocus ();
  }

  public void
  onActivityDetached ()
  {
    /* Destroy the associated frame when the activity is detached.  */
    EmacsNative.sendWindowAction (this.handle, 0);
  }
};
