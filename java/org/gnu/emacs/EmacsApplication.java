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
import java.io.FileFilter;

import android.content.Context;

import android.app.Application;
import android.util.Log;

public final class EmacsApplication extends Application
{
  private static final String TAG = "EmacsApplication";

  /* The name of the dump file to use.  */
  public static String dumpFileName;

  public static void
  findDumpFile (Context context)
  {
    File filesDirectory;
    File[] allFiles;
    String wantedDumpFile;
    int i;

    wantedDumpFile = ("emacs-" + EmacsNative.getFingerprint ()
		      + ".pdmp");

    /* Obtain a list of all files ending with ``.pdmp''.  Then, look
       for a file named ``emacs-<fingerprint>.pdmp'' and delete the
       rest.  */
    filesDirectory = context.getFilesDir ();

    allFiles = filesDirectory.listFiles (new FileFilter () {
	@Override
	public boolean
	accept (File file)
	{
	  return (!file.isDirectory ()
		  && file.getName ().endsWith (".pdmp"));
	}
      });

    if (allFiles == null)
      return;

    /* Now try to find the right dump file.  */
    for (i = 0; i < allFiles.length; ++i)
      {
	if (allFiles[i].getName ().equals (wantedDumpFile))
	  dumpFileName = allFiles[i].getAbsolutePath ();
	else
	  /* Delete this outdated dump file.  */
	  allFiles[i].delete ();
      }
  }

  @Override
  public void
  onCreate ()
  {
    /* Block signals which don't interest the current thread and its
       descendants created by the system.  The original signal mask
       will be restored for the Emacs thread in `initEmacs'.  */
    EmacsNative.setupSystemThread ();

    /* Locate a suitable dump file.  */
    findDumpFile (this);

    /* Start the rest of the application.  */
    super.onCreate ();
  }
};
