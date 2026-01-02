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

import java.lang.Thread;
import java.util.Arrays;

import android.util.Log;

public final class EmacsThread extends Thread
{
  private static final String TAG = "EmacsThread";

  /* Whether or not Emacs should be started with additional arguments,
     and those additional arguments if non-NULL.  */
  private final String[] extraStartupArguments;

  /* Runnable run to initialize Emacs.  */
  private final Runnable paramsClosure;

  public
  EmacsThread (EmacsService service, Runnable paramsClosure,
	       String[] extraStartupArguments)
  {
    super ("Emacs main thread");
    this.extraStartupArguments = extraStartupArguments;
    this.paramsClosure = paramsClosure;
  }

  @Override
  public void
  run ()
  {
    String args[];

    if (extraStartupArguments == null)
      args = new String[] { "libandroid-emacs.so", };
    else
      {
	/* Prepend "libandroid-emacs.so" to the list of arguments.  */
	args = new String[extraStartupArguments.length + 1];
	args[0] = "libandroid-emacs.so";
	System.arraycopy (extraStartupArguments, 0, args,
			  1, extraStartupArguments.length);
      }

    paramsClosure.run ();

    /* Run the native code now.  */
    Log.d (TAG, "run: " + Arrays.toString (args));
    EmacsNative.initEmacs (args, EmacsApplication.dumpFileName);
  }
};
