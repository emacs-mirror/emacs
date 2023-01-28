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

import java.io.File;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Build;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.widget.LinearLayout;
import android.widget.TextView;

import android.R;

/* This module provides a ``preferences'' display for Emacs.  It is
   supposed to be launched from inside the Settings application to
   perform various actions, such as starting Emacs with the ``-Q''
   option, which would not be possible otherwise, as there is no
   command line on Android.  */

public class EmacsPreferencesActivity extends Activity
{
  /* The linear layout associated with the activity.  */
  private LinearLayout layout;

  /* Restart Emacs with -Q.  Call EmacsThread.exit to kill Emacs now, and
     tell the system to EmacsActivity with some parameters later.  */

  private void
  startEmacsQ ()
  {
    Intent intent;

    intent = new Intent (this, EmacsActivity.class);
    intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK
		     | Intent.FLAG_ACTIVITY_CLEAR_TASK);
    intent.putExtra ("org.gnu.emacs.START_DASH_Q", true);
    startActivity (intent);
    System.exit (0);
  }

  @Override
  public void
  onCreate (Bundle savedInstanceState)
  {
    LinearLayout layout;
    TextView textView;
    LinearLayout.LayoutParams params;
    int resid;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
      setTheme (R.style.Theme_DeviceDefault_Settings);
    else if (Build.VERSION.SDK_INT
	     >= Build.VERSION_CODES.ICE_CREAM_SANDWICH)
      setTheme (R.style.Theme_DeviceDefault);

    layout = new LinearLayout (this);
    layout.setOrientation (LinearLayout.VERTICAL);
    setContentView (layout);

    textView = new TextView (this);
    textView.setPadding (8, 20, 20, 8);

    params = new LinearLayout.LayoutParams (LayoutParams.MATCH_PARENT,
					    LayoutParams.WRAP_CONTENT);
    textView.setLayoutParams (params);
    textView.setText ("(Re)start Emacs with -Q");
    textView.setOnClickListener (new View.OnClickListener () {
	@Override
	public void
	onClick (View view)
	{
	  startEmacsQ ();
	}
      });
    layout.addView (textView);

    textView = new TextView (this);
    textView.setPadding (8, 20, 20, 8);

    params = new LinearLayout.LayoutParams (LayoutParams.MATCH_PARENT,
					    LayoutParams.WRAP_CONTENT);
    textView.setLayoutParams (params);
    textView.setText ("Erase dump file");
    textView.setOnClickListener (new View.OnClickListener () {
	@Override
	public void
	onClick (View view)
	{
	  String wantedDumpFile;
	  File file;

	  wantedDumpFile = ("emacs-" + EmacsNative.getFingerprint ()
			    + ".pdmp");
	  file = new File (getFilesDir (), wantedDumpFile);

	  if (file.exists ())
	    file.delete ();

	  /* Make sure to clear EmacsApplication.dumpFileName, or
	     starting Emacs without restarting this program will
	     make Emacs try to load a nonexistent dump file.  */
	  EmacsApplication.dumpFileName = null;
	}
      });
    layout.addView (textView);

    super.onCreate (savedInstanceState);
  }
};
