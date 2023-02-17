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

import java.lang.Thread;

import android.os.Build;

public class EmacsThread extends Thread
{
  /* Whether or not Emacs should be started -Q.  */
  private boolean startDashQ;

  /* Runnable run to initialize Emacs.  */
  private Runnable paramsClosure;

  public
  EmacsThread (EmacsService service, Runnable paramsClosure,
	       boolean startDashQ)
  {
    super ("Emacs main thread");
    this.startDashQ = startDashQ;
    this.paramsClosure = paramsClosure;
  }

  @Override
  public void
  run ()
  {
    String args[];

    if (!startDashQ)
      args = new String[] { "libandroid-emacs.so", };
    else
      args = new String[] { "libandroid-emacs.so", "-Q", };

    paramsClosure.run ();

    /* Run the native code now.  */
    EmacsNative.initEmacs (args, EmacsApplication.dumpFileName,
			   Build.VERSION.SDK_INT);
  }
};
