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

import android.app.Application;
import android.util.Log;

public class EmacsApplication extends Application implements FileFilter
{
  private static final String TAG = "EmacsApplication";

  /* The name of the dump file to use.  */
  public static String dumpFileName;

  @Override
  public boolean
  accept (File file)
  {
    return (!file.isDirectory ()
	    && file.getName ().endsWith (".pdmp"));
  }

  @Override
  public void
  onCreate ()
  {
    File filesDirectory;
    File[] allFiles;
    String wantedDumpFile;
    int i;

    wantedDumpFile = ("emacs-" + EmacsNative.getFingerprint ()
		      + ".pdmp");

    Log.d (TAG, "onCreate: looking for " + wantedDumpFile);

    /* Obtain a list of all files ending with ``.pdmp''.  Then, look
       for a file named ``emacs-<fingerprint>.pdmp'' and delete the
       rest.  */
    filesDirectory = getFilesDir ();
    allFiles = filesDirectory.listFiles (this);

    /* Now try to find the right dump file.  */
    for (i = 0; i < allFiles.length; ++i)
      {
	if (allFiles[i].getName ().equals (wantedDumpFile))
	  dumpFileName = allFiles[i].getAbsolutePath ();
	else
	  /* Delete this outdated dump file.  */
	  allFiles[i].delete ();
      }

    Log.d (TAG, "onCreate: found " + dumpFileName);

    super.onCreate ();
  }
};
