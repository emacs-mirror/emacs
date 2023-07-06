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

import android.os.Looper;
import android.os.Build;

import android.content.Context;
import android.content.res.AssetManager;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/* Noninteractive Emacs.

   This is the class that libandroid-emacs.so starts.
   libandroid-emacs.so figures out the system classpath, then starts
   dalvikvm with the framework jars.

   At that point, dalvikvm calls main, which sets up the main looper,
   creates an ActivityThread and attaches it to the main thread.

   Then, it obtains an application context for the LoadedApk in the
   application thread.

   Finally, it obtains the necessary context specific objects and
   initializes Emacs.  */

@SuppressWarnings ("unchecked")
public final class EmacsNoninteractive
{
  public static void
  main (String[] args)
  {
    Object activityThread, loadedApk;
    Class activityThreadClass, loadedApkClass, contextImplClass;
    Class compatibilityInfoClass;
    Method method;
    Context context;
    AssetManager assets;
    String filesDir, libDir, cacheDir;

    Looper.prepare ();
    context = null;
    assets = null;
    filesDir = libDir = cacheDir = null;

    try
      {
	/* Get the activity thread.  */
	activityThreadClass = Class.forName ("android.app.ActivityThread");

	/* Get the systemMain method.  */
	method = activityThreadClass.getMethod ("systemMain");

	/* Create and attach the activity thread.  */
	activityThread = method.invoke (null);
	context = null;

	/* Now get an LoadedApk.  */

	try
	  {
	    loadedApkClass = Class.forName ("android.app.LoadedApk");
	  }
	catch (ClassNotFoundException exception)
	  {
	    /* Android 2.2 has no LoadedApk class, but fortunately it
	       does not need to be used, since contexts can be
	       directly created.  */

	    loadedApkClass = null;
	    contextImplClass = Class.forName ("android.app.ContextImpl");

	    method = activityThreadClass.getDeclaredMethod ("getSystemContext");
	    context = (Context) method.invoke (activityThread);
	    method = contextImplClass.getDeclaredMethod ("createPackageContext",
							 String.class,
							 int.class);
	    method.setAccessible (true);
	    context = (Context) method.invoke (context, "org.gnu.emacs",
					       0);
	  }

	/* If the context has not already been created, then do what
	   is appropriate for newer versions of Android.  */

	if (context == null)
	  {
	    /* Get a LoadedApk.  How to do this varies by Android version.
	       On Android 2.3.3 and earlier, there is no
	       ``compatibilityInfo'' argument to getPackageInfo.  */

	    if (Build.VERSION.SDK_INT
		<= Build.VERSION_CODES.GINGERBREAD_MR1)
	      {
		method
		  = activityThreadClass.getMethod ("getPackageInfo",
						   String.class,
						   int.class);
		loadedApk = method.invoke (activityThread, "org.gnu.emacs",
					   0);
	      }
	    else
	      {
		compatibilityInfoClass
		  = Class.forName ("android.content.res.CompatibilityInfo");

		method
		  = activityThreadClass.getMethod ("getPackageInfo",
						   String.class,
						   compatibilityInfoClass,
						   int.class);
		loadedApk = method.invoke (activityThread, "org.gnu.emacs",
					   null, 0);
	      }

	    if (loadedApk == null)
	      throw new RuntimeException ("getPackageInfo returned NULL");

	    /* Now, get a context.  */
	    contextImplClass = Class.forName ("android.app.ContextImpl");

	    try
	      {
		method
		  = contextImplClass.getDeclaredMethod ("createAppContext",
							activityThreadClass,
							loadedApkClass);
		method.setAccessible (true);
		context = (Context) method.invoke (null, activityThread,
						   loadedApk);
	      }
	    catch (NoSuchMethodException exception)
	      {
		/* Older Android versions don't have createAppContext, but
		   instead require creating a ContextImpl, and then
		   calling createPackageContext.  */
		method
		  = activityThreadClass.getDeclaredMethod ("getSystemContext");
		context = (Context) method.invoke (activityThread);
		method
		  = contextImplClass.getDeclaredMethod ("createPackageContext",
							String.class,
							int.class);
		method.setAccessible (true);
		context = (Context) method.invoke (context, "org.gnu.emacs",
						   0);
	      }
	  }

	/* Don't actually start the looper or anything.  Instead, obtain
	   an AssetManager.  */
	assets = context.getAssets ();

	/* Now configure Emacs.  The class path should already be set.  */

	filesDir = context.getFilesDir ().getCanonicalPath ();
	libDir = EmacsService.getLibraryDirectory (context);
	cacheDir = context.getCacheDir ().getCanonicalPath ();
      }
    catch (Exception e)
      {
	System.err.println ("Internal error: " + e);
	System.err.println ("This means that the Android platform changed,");
	System.err.println ("and that Emacs needs adjustments in order to");
	System.err.println ("obtain required system internal resources.");
	System.err.println ("Please report this bug to bug-gnu-emacs@gnu.org.");
	e.printStackTrace ();

	System.exit (1);
      }

    EmacsNative.setEmacsParams (assets, filesDir,
				libDir, cacheDir, 0.0f,
				0.0f, 0.0f, null, null);

    /* Now find the dump file that Emacs should use, if it has already
       been dumped.  */
    EmacsApplication.findDumpFile (context);

    /* Start Emacs.  */
    EmacsNative.initEmacs (args, EmacsApplication.dumpFileName,
			   Build.VERSION.SDK_INT);
  }
};
