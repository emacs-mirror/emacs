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

import java.util.LinkedList;
import java.util.List;

import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Rect;

public class EmacsPaintQueue
{
  /* Queue of paint operations.  This is modified from the Emacs
     thread, and entire paint queues are periodically flushed to the
     application thread where it is executed.  */
  private List<EmacsPaintReq> paintOperations;

  /* Number of operations in this queue.  */
  public int numRequests;

  public
  EmacsPaintQueue ()
  {
    paintOperations = new LinkedList<EmacsPaintReq> ();
  }

  public void
  run ()
  {
    EmacsDrawable drawable, last;
    Canvas canvas;
    EmacsGC gc;
    int i;
    Paint paint;
    Rect rect, offsetRect, copyRect;

    canvas = null;
    last = null;
    gc = null;
    paint = new Paint ();

    for (EmacsPaintReq req : paintOperations)
      {
	drawable = req.getDrawable ();
	canvas = drawable.lockCanvas ();

	if (canvas == null)
	  /* No canvas is currently available.  */
	  continue;

	gc = req.getGC ();
	rect = req.getRect ();

	drawable.damageRect (rect);

	if (gc.clip_rects == null)
	  {
	    /* No clipping is applied.  Just draw and continue.  */
	    req.paintTo (canvas, paint, gc);
	    continue;
	  }

	if (gc.clip_rects != null && gc.clip_rects.length > 0)
	  {
	    if (gc.clip_rects.length == 1)
	      {
		/* There is only a single clip rect, which is simple
		   enough.  */
		canvas.save ();
		canvas.clipRect (gc.clip_rects[0]);
		req.paintTo (canvas, paint, gc);
		canvas.restore ();
	      }
	    else
	      {
		/* There are multiple clip rects.  Android doesn't let
		   programs use RegionOp.UNION on the clip rectangle,
		   so Emacs must iterate over each intersection and
		   paint it manually.  This seems inefficient but
		   thankfully Emacs never seems to use more than one
		   clip rect.  */

		for (i = 0; i < gc.clip_rects.length; ++i)
		  {
		    copyRect = new Rect (gc.clip_rects[i]);

		    if (copyRect.intersect (rect))
		      {
			canvas.save ();
			canvas.clipRect (copyRect);
			req.paintTo (canvas, paint, gc);
			canvas.restore ();
		      }
		  }
	      }
	  }
      }
  }

  public void
  appendPaintOperation (EmacsPaintReq req)
  {
    paintOperations.add (req);
    numRequests++;
  }
};
