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

import android.os.Looper;
import android.os.Build;

import android.content.Context;
import android.content.res.AssetManager;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/* Noninteractive Emacs.

   When started, libandroid-emacs.so invokes `app_process(64)' with a
   command line placing Emacs's classes.dex file in the JVM class path,
   which in turn transfers control to `main'.  `main' creates a context,
   which may be likened to a connection to the system server, and a
   class loader derived from Emacs's application package, which it loads
   beforehand.  From this class loader, it loads another instance of
   itself, and invokes `main1', to ensure the execution of
   `EmacsNative''s static initializer within the application class
   loader, where a proper library search path is in effect.  */

@SuppressWarnings ("unchecked")
public final class EmacsNoninteractive
{
  /* Prepare Emacs for startup and call `initEmacs'.  This function is
     called in an instance of `EmacsNoninteractive' loaded by the APK
     ClassLoader acquired in `main', which guarantees that shared
     libraries in the APK will be considered in resolving shared
     libraries for `EmacsNative'.  */

  public static void
  main1 (String[] args, Context context)
    throws Exception
  {
    AssetManager assets;
    String filesDir, libDir, cacheDir;

    /* Don't actually start the looper or anything.  Instead, obtain
       an AssetManager.  */
    assets = context.getAssets ();

    /* Now configure Emacs.  The class path should already be set.  */

    filesDir = context.getFilesDir ().getCanonicalPath ();
    libDir = EmacsService.getLibraryDirectory (context);
    cacheDir = context.getCacheDir ().getCanonicalPath ();
    EmacsNative.setEmacsParams (assets, filesDir,
				libDir, cacheDir, 0.0f,
				0.0f, 0.0f, 0x0, null, null,
				Build.VERSION.SDK_INT);

    /* Now find the dump file that Emacs should use, if it has already
       been dumped.  */
    EmacsApplication.findDumpFile (context);

    /* Start Emacs.  */
    EmacsNative.initEmacs (args, EmacsApplication.dumpFileName);
  }

  public static void
  main (String[] args)
  {
    Object activityThread, loadedApk;
    Class activityThreadClass, loadedApkClass, contextImplClass;
    Class compatibilityInfoClass, emacsNoninteractiveClass;
    Method method;
    Context context;
    ClassLoader classLoader;

    Looper.prepare ();

    context = null;
    loadedApkClass = null;
    classLoader = null;

    try
      {
	/* Get the activity thread.  */
	activityThreadClass = Class.forName ("android.app.ActivityThread");

	/* Get the systemMain method.  */
	method = activityThreadClass.getMethod ("systemMain");

	/* Create and attach the activity thread.  */
	activityThread = method.invoke (null);

	/* Now get an LoadedApk.  */

	try
	  {
	    loadedApkClass = Class.forName ("android.app.LoadedApk");
	  }
	catch (ClassNotFoundException exception)
	  {
	    /* Android 2.2 has no LoadedApk class; the several following
	       statements will load a context and an
	       ActivityThread.PackageInfo, as is appropriate on this
	       system.  */
	  }

	/* Get a LoadedApk or ActivityThread.PackageInfo.  How to do
	   this varies by Android version.  On Android 3.0 and earlier,
	   there is no ``compatibilityInfo'' argument to
	   getPackageInfo.  */

	try
	  {
	    method
	      = activityThreadClass.getMethod ("getPackageInfo",
					       String.class,
					       int.class);
	    loadedApk = method.invoke (activityThread, "org.gnu.emacs",
				       (Context.CONTEXT_INCLUDE_CODE
					| Context.CONTEXT_IGNORE_SECURITY));
	  }
	catch (NoSuchMethodException exception)
	  {
	    compatibilityInfoClass
	      = Class.forName ("android.content.res.CompatibilityInfo");

	    method
	      = activityThreadClass.getMethod ("getPackageInfo",
					       String.class,
					       compatibilityInfoClass,
					       int.class);
	    loadedApk = method.invoke (activityThread, "org.gnu.emacs",
				       null, (Context.CONTEXT_INCLUDE_CODE
					      | Context.CONTEXT_IGNORE_SECURITY));
	  }

	if (loadedApk == null)
	  throw new RuntimeException ("getPackageInfo returned NULL");

	/* If loadedApkClass remains NULL, substitute the class of
	   the object returned by getPackageInfo.  */
	if (loadedApkClass == null)
	  loadedApkClass = loadedApk.getClass ();

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

	/* Retrieve the LoadedApk's class loader and execute the
	   remaining portion of the start-up process within its version
	   of EmacsNoninteractive, which will indicate to the system
	   that it must load shared libraries from the APK's library
	   search path.  */

	method = loadedApkClass.getDeclaredMethod ("getClassLoader");
	classLoader = (ClassLoader) method.invoke (loadedApk);
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

    try
      {
	emacsNoninteractiveClass
	  = classLoader.loadClass ("org.gnu.emacs.EmacsNoninteractive");
	method = emacsNoninteractiveClass.getMethod ("main1", String[].class,
						     Context.class);
	method.setAccessible (true);
	method.invoke (null, args, context);
      }
    catch (Exception e)
      {
	System.err.println ("Internal error during startup: " + e);
	e.printStackTrace ();
	System.exit (1);
      }
  }
};
