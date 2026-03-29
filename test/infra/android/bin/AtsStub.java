/* Launch an intent stated on the command line as an activity.  -*- c-file-style: "GNU" -*-

Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

package ats;

import android.app.ActivityManagerNative;
import android.app.ActivityThread;
import android.app.IActivityManager;
import android.app.IApplicationThread;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;

import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Looper;
import android.os.ParcelFileDescriptor;
import android.os.RemoteException;

import android.net.Uri;

import java.lang.IllegalArgumentException;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public final class AtsStub
{
  public static final String IDENT = "$Id: AtsStub.java,v 1.4 2024/06/30 04:24:39 jw Exp $";

  private static void
  neutralizeApplicationThread (ActivityThread thread)
  {
    Field field;

    try
      {
	field = ActivityThread.class.getDeclaredField ("mAppThread");
	field.setAccessible (true);
	field.set (thread, null);
      }
     catch (NoSuchFieldException x)
       {
	 x.printStackTrace ();
       }
     catch (IllegalAccessException x)
       {
	 x.printStackTrace ();
       }
  }

  private static int
  main1 (String[] argv)
    throws NoSuchMethodException, IllegalAccessException,
      InvocationTargetException
  {
    ActivityThread thread;
    Context context;

    Looper.prepare ();

    thread = ActivityThread.systemMain ();
    context = thread.getSystemContext ();
    if (argv.length < 1 || argv[0].equals ("--help"))
      {
	System.out.println ("AtsStub [start] [--user <USER_ID>] <INTENT>");
	System.out.println ("  where INTENT is a series of arguments defining an Intent,");
	System.out.println ("  namely,");
	System.out.println ("    -a <ACTION>");
	System.out.println ("    -d <DATA URI>");
	System.out.println ("    -t <TYPE>");
	System.out.println ("    -c <CATEGORY>");
	System.out.println ("    -n <COMPONENT>");
	System.out.println ("    -e or --es <KEY> <STRING VALUE>");
	System.out.println ("    --esn <KEY>");
	System.out.println ("    --ei <KEY> <INTEGER VALUE>");
	System.out.println ("    --eu <KEY> <URI VALUE>");
	System.out.println ("    --ecn <KEY> <COMPONENT NAME>");
	System.out.println ("    --eia <KEY> <INTEGER>, ...");
	System.out.println ("    --el <KEY> <LONG>");
	System.out.println ("    --ela <KEY> <LONG>, ...");
	System.out.println ("    --ef <KEY> <FLOAT>");
	System.out.println ("    --efa <KEY> <FLOAT ARRAY>");
	System.out.println ("    --esa <KEY> <STRING>, ...");
	System.out.println ("    --ez <KEY> <BOOLEAN>");
	System.out.println ("    -f <KEY> <FLAGS>");
	return 0;
      }
    else if (argv[0].equals ("start"))
      {
	Intent intent;
	int i, userID = 0;
	String token, type;
	Uri data;
	boolean debug;

	intent = new Intent ();
	debug = false;
	data = null;
	type = null;

	for (i = 1; i < argv.length; ++i)
	  {
	    int j;

	    token = argv[i];

	    if (token.equals ("-a"))
	      intent.setAction (argv[++i]);
	    else if (token.equals ("-d"))
	      data = Uri.parse (argv[++i]);
	    else if (token.equals ("-t"))
	      type = argv[++i];
	    else if (token.equals ("-c"))
	      intent.addCategory (argv[++i]);
	    else if (token.equals ("-e") || token.equals ("--es"))
	      {
		intent.putExtra (argv[i + 1], argv[i + 2]);
		i += 2;
	      }
	    else if (token.equals ("--esn"))
	      intent.putExtra (argv[++i], (String) null);
	    else if (token.equals ("--ei"))
	      {
		int value = Integer.valueOf (argv[i + 2]);
		intent.putExtra (argv[i + 1], value);
		i += 2;
	      }
	    else if (token.equals ("--eu"))
	      {
		Uri value = Uri.parse (argv[i + 2]);
		intent.putExtra (argv[i + 1], value);
		i += 2;
	      }
	    else if (token.equals ("--ecn"))
	      {
		ComponentName value
		  = ComponentName.unflattenFromString (argv[i + 2]);
		intent.putExtra (argv[i + 1], value);
		i += 2;
	      }
	    else if (token.equals ("--eia"))
	      {
		String values[] = argv[i + 2].split (",");
		int array[] = new int[values.length];

		for (j = 0; j < values.length; ++j)
		  array[j] = Integer.valueOf (values[j]);
		intent.putExtra (argv[i + 1], array);
		i += 2;
	      }
	    else if (token.equals ("--el"))
	      {
		long value = Long.valueOf (argv[i + 2]);
		intent.putExtra (argv[i + 1], value);
		i += 2;
	      }
	    else if (token.equals ("--ela"))
	      {
		String values[] = argv[i + 2].split (",");
		long array[] = new long[values.length];

		for (j = 0; j < values.length; ++j)
		  array[j] = Long.valueOf (values[j]);
		intent.putExtra (argv[i + 1], array);
		i += 2;
	      }
	    else if (token.equals ("--ef"))
	      {
		float value = Float.valueOf (argv[i + 2]);
		intent.putExtra (argv[i + 1], value);
		i += 2;
	      }
	    else if (token.equals ("--efa"))
	      {
		String values[] = argv[i + 2].split (",");
		float array[] = new float[values.length];

		for (j = 0; j < values.length; ++j)
		  array[j] = Float.valueOf (values[j]);
		intent.putExtra (argv[i + 1], array);
		i += 2;
	      }
	    else if (token.equals ("--esa"))
	      {
		String[] strings;

		strings = argv[i + 2].split ("(?<!\\\\),");
                intent.putExtra (argv[i + 1], strings);
		i += 2;
	      }
	    else if (token.equals ("--ez"))
	      {
		boolean value = Boolean.valueOf (argv[i + 2]);
		intent.putExtra (argv[i + 1], value);
		i += 2;
	      }
	    else if (token.equals ("-n"))
	      {
		ComponentName value
		  = ComponentName.unflattenFromString (argv[++i]);
		if (value == null)
		  throw new IllegalArgumentException ("Invalid component name: " + argv[i]);
		intent.setComponent (value);
	      }
	    else if (token.equals ("-f"))
	      intent.addFlags (Integer.decode (argv[++i]).intValue ());
	    else if (token.equals ("--user"))
	      {
		int value = Integer.valueOf (argv[++i]);
		if (value != 0
		    && (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1))
		  throw new IllegalArgumentException ("Invalid user: " + value);
		userID = value;
	      }
	    else
	      throw new IllegalArgumentException ("Option not understood: " + argv[i]);
	  }

	intent.addFlags (Intent.FLAG_ACTIVITY_NEW_TASK);
	intent.setDataAndType (data, type);

	if ((Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1)
	    || userID == 0)
	  {
	    /* mAppThread must be neutralized, or the ActivityManager
	       service will attempt and fail to locate a matching app
	       record when it is passed as the caller argument to the
	       startActivity RPC routine.  */
	    neutralizeApplicationThread (thread);
	    context.startActivity (intent);
	  }
	else
	  {
	    /* Otherwise, there are two revisions of startActivityAsUser
	       this utility must support, whose signatures follow:

	       (IApplicationThread, Intent, String, IBinder, String,
	        int, int, String, ParcelFileDescriptor, Bundle, int)

	       (IApplicationThread, String, Intent, String, IBinder, String,
	       int, int, String, ParcelFileDescriptor, Bundle, int) */

	    Method method;
	    IActivityManager am = ActivityManagerNative.getDefault ();
	    int rc;
	    Class klass = IActivityManager.class;

	    /* Attempt to resolve the first variant which is mostly
	       observed on Jelly Bean MR1 systems.  */
	    try
	      {
		method = klass.getMethod ("startActivityAsUser",
					  IApplicationThread.class,
					  Intent.class, String.class,
					  IBinder.class, String.class,
					  int.class, int.class,
					  String.class,
					  ParcelFileDescriptor.class,
					  Bundle.class, int.class);
	      }
	    catch (NoSuchMethodException e)
	      {
		method = null;
	      }

	    if (method != null)
	      rc = (Integer) method.invoke (am, null, intent, intent.getType (),
					    null, null, 0, 0, null, null, null,
					    userID);
	    else
	      {
		/* Now the modern `IActivityManager#startActivityAsUser'.  */
		method = klass.getMethod ("startActivityAsUser",
					  IApplicationThread.class,
					  String.class, Intent.class,
					  String.class, IBinder.class,
					  String.class, int.class,
					  int.class, String.class,
					  ParcelFileDescriptor.class,
					  Bundle.class, int.class);

		rc = (Integer) method.invoke (am, null, null, intent,
					      intent.getType (),
					      null, null, 0, 0, null,
					      null, null, userID);
	      }

	    if (rc != 0)
	      {
		System.err.println ("Failed to start activity as user: " + rc);
		return 1;
	      }
	  }
	return 0;
      }
    return 1;
  }

  public static void
  main (String arg[])
  {
    try
      {
	System.exit (main1 (arg));
      }
    catch (Throwable e)
      {
	e.printStackTrace ();
	System.exit (0);
      }
  }
};
