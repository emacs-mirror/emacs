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



import android.content.Context;

import android.view.View;
import android.view.View.MeasureSpec;
import android.view.ViewGroup;



/* This ``view group'' implements a container widget for multiple
   buttons of the type found in pop-up dialogs.  It is used when
   displaying a dialog box that contains more than three buttons, as
   the default dialog box widget is not capable of holding more than
   that many.  */



public final class EmacsDialogButtonLayout extends ViewGroup
{
  public
  EmacsDialogButtonLayout (Context context)
  {
    super (context);
  }

  @Override
  protected void
  onMeasure (int widthMeasureSpec, int heightMeasureSpec)
  {
    int width, count, i, x, y, height, spec, tempSpec;
    View view;

    /* Obtain the width of this widget and create the measure
       specification used to measure children.  */

    width = MeasureSpec.getSize (widthMeasureSpec);
    spec = MeasureSpec.makeMeasureSpec (0, MeasureSpec.UNSPECIFIED);
    tempSpec
      = MeasureSpec.makeMeasureSpec (width, MeasureSpec.AT_MOST);
    x = y = height = 0;

    /* Run through each widget.  */

    count = getChildCount ();

    for (i = 0; i < count; ++i)
      {
	view = getChildAt (i);

	/* Measure this view.  */
	view.measure (spec, spec);

	if (width - x < view.getMeasuredWidth ())
	  {
	    /* Move onto the next line, unless this line is empty.  */

	    if (x != 0)
	      {
		y += height;
		height = x = 0;
	      }

	    if (view.getMeasuredWidth () > width)
	      /* Measure the view again, this time forcing it to be at
		 most width wide, if it is not already.  */
	      view.measure (tempSpec, spec);
	  }

	height = Math.max (height, view.getMeasuredHeight ());
	x += view.getMeasuredWidth ();
      }

    /* Now set the measured size of this widget.  */
    setMeasuredDimension (width, y + height);
  }

  @Override
  protected void
  onLayout (boolean changed, int left, int top, int right,
	    int bottom)
  {
    int width, count, i, x, y, height, spec, tempSpec;
    View view;

    /* Obtain the width of this widget and create the measure
       specification used to measure children.  */

    width = getMeasuredWidth ();
    spec = MeasureSpec.makeMeasureSpec (0, MeasureSpec.UNSPECIFIED);
    tempSpec
      = MeasureSpec.makeMeasureSpec (width, MeasureSpec.AT_MOST);
    x = y = height = 0;

    /* Run through each widget.  */

    count = getChildCount ();

    for (i = 0; i < count; ++i)
      {
	view = getChildAt (i);

	/* Measure this view.  */
	view.measure (spec, spec);

	if (width - x < view.getMeasuredWidth ())
	  {
	    /* Move onto the next line, unless this line is empty.  */

	    if (x != 0)
	      {
		y += height;
		height = x = 0;
	      }

	    if (view.getMeasuredWidth () > width)
	      /* Measure the view again, this time forcing it to be at
		 most width wide, if it is not already.  */
	      view.measure (tempSpec, spec);
	  }

	/* Now assign this view its position.  */
	view.layout (x, y, x + view.getMeasuredWidth (),
		     y + view.getMeasuredHeight ());

	/* And move on to the next widget.  */
	height = Math.max (height, view.getMeasuredHeight ());
	x += view.getMeasuredWidth ();
      }
  }
};
