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
import java.io.FileFilter;

import android.content.Context;

import android.app.Application;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.ApplicationInfoFlags;
import android.content.pm.PackageManager;

import android.os.Build;

import android.util.Log;

public final class EmacsApplication extends Application
{
  private static final String TAG = "EmacsApplication";

  /* The name of the dump file to use, or NULL if this Emacs binary
     has yet to be dumped.  */
  public static String dumpFileName;

  /* The name of the APK file housing Emacs, or NULL if it could not
     be ascertained.  */
  public static String apkFileName;

  @SuppressWarnings ("deprecation")
  private String
  getApkFile ()
  {
    PackageManager manager;
    ApplicationInfo info;

    manager = getPackageManager ();

    try
      {
	if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU)
	  info = manager.getApplicationInfo ("org.gnu.emacs", 0);
	else
	  info = manager.getApplicationInfo ("org.gnu.emacs",
					     ApplicationInfoFlags.of (0));

	/* Return an empty string upon failure.  */

	if (info.sourceDir != null)
	  return info.sourceDir;

	return null;
      }
    catch (Exception e)
      {
	return null;
      }
  }

  public static void
  findDumpFile (Context context)
  {
    File filesDirectory, apk;
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
	  {
	    /* Compare the last modified time of the dumpfile with
	       that of apkFileName, the time at which Emacs was
	       installed.  Delete it if the dump file was created
	       before Emacs was installed, even if the C signature
	       (representing libemacs.so) remains identical.  */

	    if (apkFileName != null)
	      {
		apk = new File (apkFileName);

		if (apk.lastModified ()
		    > allFiles[i].lastModified ())
		  {
		    allFiles[i].delete ();

		    /* Don't set the dump file name in this case.  */
		    continue;
		  }
	      }

	    dumpFileName = allFiles[i].getAbsolutePath ();
	  }
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

    /* Establish the name of the APK.  */
    apkFileName = getApkFile ();

    /* Locate a suitable dump file.  */
    findDumpFile (this);

    /* Start the rest of the application.  */
    super.onCreate ();
  }
};
