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
import java.util.List;
import java.util.ArrayList;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.FrameLayout;
import android.widget.FrameLayout.LayoutParams;

public class EmacsActivity extends Activity
{
  public static final String TAG = "EmacsActivity";

  /* List of all activities that do not have an associated
     EmacsWindow.  */
  public static List<EmacsActivity> availableActivities;

  /* The currently attached EmacsWindow, or null if none.  */
  private EmacsWindow window;

  /* The frame layout associated with the activity.  */
  private FrameLayout layout;

  static
  {
    /* Set up the list of available activities.  */
    availableActivities = new ArrayList<EmacsActivity> ();
  };

  public void
  attachChild (EmacsWindow child)
  {
    if (window != null)
      throw new IllegalStateException ("trying to attach window when one"
				       + " already exists");

    /* Record and attach the view.  */
    window = child;
    layout.addView (window.view);

    /* Remove the objects from the lists of what is available.  */
    EmacsService.availableChildren.remove (child);
    availableActivities.remove (this);

    /* Now set child->activity.  */
    child.setActivity (this);
  }

  /* Make this activity available for future windows to attach
     again.  */

  public void
  makeAvailable ()
  {
    window = null;

    for (EmacsWindow iterWindow
	   : EmacsService.availableChildren)
      {
	synchronized (iterWindow)
	  {
	    if (!iterWindow.isDestroyed ())
	      attachChild (iterWindow);

	    return;
	  }
      }

    availableActivities.add (this);
  }

  @Override
  public void
  onCreate (Bundle savedInstanceState)
  {
    FrameLayout.LayoutParams params;

    params = new FrameLayout.LayoutParams (LayoutParams.MATCH_PARENT,
					   LayoutParams.MATCH_PARENT);

    /* Make the frame layout.  */
    layout = new FrameLayout (this);
    layout.setLayoutParams (params);

    /* Set it as the content view.  */
    setContentView (layout);

    /* Make the activity available before starting the
       service.  */
    makeAvailable ();

    if (EmacsService.SERVICE == null)
      /* Start the Emacs service now.  */
      startService (new Intent (this, EmacsService.class));

    super.onCreate (savedInstanceState);
  }

  @Override
  public void
  onStop ()
  {
    /* The activity is no longer visible.  If there is a window
       attached, detach it.  */

    if (window != null)
      {
	layout.removeView (window.view);

	/* Notice that the window is already available too.  But do
	   not call noticeAvailableChild; that might assign it to some
	   other activity, which behaves badly.  */
	EmacsService.availableChildren.add (window);
	window = null;
      }

    /* Finally, remove this activity from the list of available
       activities.  */
    availableActivities.remove (this);
    super.onStop ();
  }
};
