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

import java.io.File;

import android.app.Activity;

import android.content.Intent;

import android.os.Bundle;
import android.os.Build;

import android.view.View;

import android.widget.Toast;

import android.preference.*;

/* This module provides a ``preferences'' display for Emacs.  It is
   supposed to be launched from inside the Settings application to
   perform various actions, such as starting Emacs with the ``-Q''
   option, which would not be possible otherwise, as there is no
   command line on Android.

   This file extends a deprecated preferences activity, but no suitable
   alternative exists that is identical in appearance to system settings
   forms.  */

@SuppressWarnings ("deprecation")
public class EmacsPreferencesActivity extends PreferenceActivity
{
  /* Restart Emacs with -Q.  Call EmacsThread.exit to kill Emacs now,
     and tell the system to start EmacsActivity with some parameters
     later.  */

  private void
  startEmacsQ ()
  {
    Intent intent;

    intent = new Intent (this, EmacsActivity.class);
    intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK
		     | Intent.FLAG_ACTIVITY_CLEAR_TASK);
    intent.putExtra (EmacsActivity.EXTRA_STARTUP_ARGUMENTS,
		     new String[] {"--quick", });
    startActivity (intent);
    System.exit (0);
  }

  /* Restart Emacs with `--debug-init'.  Call EmacsThread.exit to kill
     Emacs now, and tell the system to EmacsActivity with some
     parameters later.  */

  private void
  startEmacsDebugInit ()
  {
    Intent intent;

    intent = new Intent (this, EmacsActivity.class);
    intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK
		     | Intent.FLAG_ACTIVITY_CLEAR_TASK);
    intent.putExtra (EmacsActivity.EXTRA_STARTUP_ARGUMENTS,
		     new String[] {"--debug-init", });
    startActivity (intent);
    System.exit (0);
  }

  /* Erase Emacs's dump file.  */

  private void
  eraseDumpFile ()
  {
    String wantedDumpFile;
    File file;
    Toast toast;

    wantedDumpFile = ("emacs-" + EmacsNative.getFingerprint ()
		      + ".pdmp");
    file = new File (getFilesDir (), wantedDumpFile);

    if (file.exists ())
      file.delete ();

    /* Make sure to clear EmacsApplication.dumpFileName, or
       starting Emacs without restarting this program will
       make Emacs try to load a nonexistent dump file.  */
    EmacsApplication.dumpFileName = null;

    /* Display a message stating that the dump file has been
       erased.  */
    toast = Toast.makeText (this, "Dump file removed",
			    Toast.LENGTH_SHORT);
    toast.show ();
  }

  @Override
  public final void
  onCreate (Bundle savedInstanceState)
  {
    Preference tem;
    Preference.OnPreferenceClickListener listener;
    View view;

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
      setTheme (android.R.style.Theme_DeviceDefault_Settings);
    else if (Build.VERSION.SDK_INT
	     >= Build.VERSION_CODES.ICE_CREAM_SANDWICH)
      setTheme (android.R.style.Theme_DeviceDefault);

    /* This must come before using any preference APIs.  */
    super.onCreate (savedInstanceState);

    /* Add preferences from the XML file where they are defined.  */
    addPreferencesFromResource (R.xml.preferences);

    /* Now, set up on click handlers for each of the preferences
       items.  */

    tem = findPreference ("start_quick");
    listener = new Preference.OnPreferenceClickListener () {
	@Override
	public boolean
	onPreferenceClick (Preference preference)
	{
	  startEmacsQ ();
	  return true;
	}
      };

    tem.setOnPreferenceClickListener (listener);
    tem = findPreference ("start_debug_init");
    listener = new Preference.OnPreferenceClickListener () {
	@Override
	public boolean
	onPreferenceClick (Preference preference)
	{
	  startEmacsDebugInit ();
	  return true;
	}
      };

    tem.setOnPreferenceClickListener (listener);
    tem = findPreference ("erase_dump");
    listener = new Preference.OnPreferenceClickListener () {
	@Override
	public boolean
	onPreferenceClick (Preference preference)
	{
	  eraseDumpFile ();
	  return true;
	}
      };

    tem.setOnPreferenceClickListener (listener);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.VANILLA_ICE_CREAM)
      {
	/* Align the list view to system windows, or they will be
	   obstructed by the title bar.  */
	view = this.getListView ();
	view.setFitsSystemWindows (true);
      }
  }
};
