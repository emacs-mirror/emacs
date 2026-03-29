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

/* Opening external documents on Android.

   This activity is registered as an application capable of opening text
   files and files in several other formats that Emacs understands, and
   assumes responsibility for deriving file names from the files
   provided to `onCreate', potentially copying them to temporary
   directories in the process, and invoking `emacsclient' with suitable
   arguments to open the same.  In this respect, it fills the role of
   `etc/emacs.desktop' on XDG systems.

   It is also registered as a handler for mailto URIs, in which capacity
   it constructs invocations of `emacsclient' so as to start
   `message-mailto' with their contents and attachments, much like
   `etc/emacs-mail.desktop'.

   As with all other activities, it is registered in the package
   manifest file.  */

import android.app.AlertDialog;
import android.app.Activity;

import android.content.ContentResolver;
import android.content.DialogInterface;
import android.content.Intent;

import android.net.Uri;

import android.os.Build;
import android.os.Bundle;
import android.os.ParcelFileDescriptor;
import android.os.Parcelable;

import android.util.Log;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import java.util.List;

public final class EmacsOpenActivity extends Activity
  implements DialogInterface.OnClickListener,
  DialogInterface.OnCancelListener
{
  private static final String TAG = "EmacsOpenActivity";

  /* Any currently focused EmacsOpenActivity.  Used to show pop ups
     while the activity is active and Emacs doesn't have permission to
     display over other programs.  */
  public static EmacsOpenActivity currentActivity;

  private class EmacsClientThread extends Thread
  {
    private ProcessBuilder builder;

    public
    EmacsClientThread (ProcessBuilder processBuilder)
    {
      builder = processBuilder;
    }

    @Override
    public void
    run ()
    {
      Process process;
      InputStream error;
      String errorText;

      try
	{
	  /* Start emacsclient.  */
	  process = builder.start ();
	  process.waitFor ();

	  /* Now figure out whether or not starting the process was
	     successful.  */
	  if (process.exitValue () == 0)
	    finishSuccess ();
	  else
	    finishFailure ("Error opening file", null);
	}
      catch (IOException exception)
	{
	  finishFailure ("Internal error", exception.toString ());
	}
      catch (InterruptedException exception)
	{
	  finishFailure ("Internal error", exception.toString ());
	}
    }
  }

  @Override
  public void
  onClick (DialogInterface dialog, int which)
  {
    finish ();
  }

  @Override
  public void
  onCancel (DialogInterface dialog)
  {
    finish ();
  }

  public String
  readEmacsClientLog ()
  {
    File file, cache;
    FileReader reader;
    char[] buffer;
    int rc;
    StringBuilder builder;

    /* Because the ProcessBuilder functions necessary to redirect
       process output are not implemented on Android 7 and earlier,
       print a generic error message.  */

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O)
      return ("This is likely because the Emacs server"
	      + " is not running, or because you did"
	      + " not grant Emacs permission to access"
	      + " external storage.");

    cache = getCacheDir ();
    file = new File (cache, "emacsclient.log");
    builder = new StringBuilder ();
    reader = null;

    try
      {
	reader = new FileReader (file);
	buffer = new char[2048];

	while ((rc = reader.read (buffer, 0, 2048)) != -1)
	  builder.append (buffer, 0, rc);

	reader.close ();
	return builder.toString ();
      }
    catch (IOException exception)
      {
	/* Close the reader if it's already been opened.  */

	try
	  {
	    if (reader != null)
	      reader.close ();
	  }
	catch (IOException e)
	  {
	    /* Not sure what to do here.  */
	  }

	return ("Couldn't read emacsclient.log: "
		+ exception.toString ());
      }
  }

  private void
  displayFailureDialog (String title, String text)
  {
    AlertDialog.Builder builder;
    AlertDialog dialog;

    builder = new AlertDialog.Builder (this);
    dialog = builder.create ();
    dialog.setTitle (title);

    if (text == null)
      /* Read in emacsclient.log instead.  */
      text = readEmacsClientLog ();

    dialog.setMessage (text);
    dialog.setButton (DialogInterface.BUTTON_POSITIVE, "OK", this);
    dialog.setOnCancelListener (this);
    dialog.show ();
  }

  /* Check that the specified FILE is non-NULL and readable.

     If it is not, then copy the file in FD to a location in the
     system cache directory and return the name of that file.

     Alternatively, return URI formatted into a `/content/' file name
     if the system runs Android 4.4 or later.  */

  private String
  checkReadableOrCopy (String file, ParcelFileDescriptor fd,
		       Uri uri)
    throws IOException, FileNotFoundException
  {
    File inFile;
    FileOutputStream outStream;
    InputStream stream;
    byte buffer[];
    int read;
    String content;

    if (file != null)
      {
	inFile = new File (file);

	if (inFile.canRead ())
	  return file;

	content = inFile.getName ();
      }
    else
      /* content is the name of this content file if the next branch
	 is not taken.  */
      content = uri.getLastPathSegment ();

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT)
      {
	content = EmacsService.buildContentName (uri, getContentResolver ());
	return content;
      }

    /* The file is unnamed if content is NULL.  Generate a unique
       name with the current time as a reference.  */

    if (content == null)
      content = "content." + System.currentTimeMillis () / 1000;

    /* inFile is now the file being written to.  */
    inFile = new File (getCacheDir (), content);
    buffer = new byte[4098];

    /* Initialize both streams to NULL.  */
    outStream = null;
    stream = null;

    try
      {
	outStream = new FileOutputStream (inFile);
	stream = new FileInputStream (fd.getFileDescriptor ());

	while ((read = stream.read (buffer)) >= 0)
	  outStream.write (buffer, 0, read);
      }
    finally
      {
	/* Note that this does not close FD.

	   Keep in mind that execution is transferred to ``finally''
	   even if an exception happens inside the while loop
	   above.  */

	if (stream != null)
	  stream.close ();

	if (outStream != null)
	  outStream.close ();
      }

    return inFile.getCanonicalPath ();
  }

  /* Finish this activity in response to emacsclient having
     successfully opened a file.

     In the main thread, close this window, and open a window
     belonging to an Emacs frame.  */

  public void
  finishSuccess ()
  {
    runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  Intent intent;

	  intent = new Intent (EmacsOpenActivity.this,
			       EmacsActivity.class);

	  /* This means only an existing frame will be displayed.  */
	  intent.addFlags (Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
	  startActivity (intent);

	  EmacsOpenActivity.this.finish ();
	}
      });
  }

  /* Finish this activity after displaying a dialog associated with
     failure to open a file.

     Use TITLE as the title of the dialog.  If TEXT is non-NULL,
     display that text in the dialog.  Otherwise, use the contents of
     emacsclient.log in the cache directory instead, or describe why
     that file cannot be read.  */

  public void
  finishFailure (final String title, final String text)
  {
    runOnUiThread (new Runnable () {
	@Override
	public void
	run ()
	{
	  displayFailureDialog (title, text);
	}
      });
  }

  /* Start `emacsclient' with the provided list of ARGUMENTS, after
     ARGUMENTS[0] is replaced with the name of the emacsclient binary.

     Create a new thread to await its completion, subsequently
     reporting any errors that arise to the user.  */

  public void
  startEmacsClient (String[] arguments)
  {
    String libDir;
    ProcessBuilder builder;
    Process process;
    EmacsClientThread thread;
    File file;
    Intent intent;

    libDir = EmacsService.getLibraryDirectory (this);
    arguments[0] = libDir + "/libemacsclient.so";

    builder = new ProcessBuilder (arguments);

    /* Redirection is unfortunately not possible in Android 7 and
       earlier.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
      {
	file = new File (getCacheDir (), "emacsclient.log");

	/* Redirect standard error to a file so that errors can be
	   meaningfully reported.  */

	if (file.exists ())
	  file.delete ();

	builder.redirectError (file);
      }

    /* Track process output in a new thread, since this is the UI
       thread and doing so here can cause deadlocks when EmacsService
       decides to wait for something.  */

    thread = new EmacsClientThread (builder);
    thread.start ();
  }

  /* Run emacsclient to open the file specified in the Intent that
     caused this activity to start.

     Determine the name of the file corresponding to the URI specified
     in that intent; then, run emacsclient and wait for it to finish.

     Finally, display any error message, transfer the focus to an
     Emacs frame, and finish the activity.  */

  @SuppressWarnings ("deprecation") /* getParcelableExtra */
  @Override
  public void
  onCreate (Bundle savedInstanceState)
  {
    String action, fileName;
    Intent intent;
    Uri uri;
    ContentResolver resolver;
    ParcelFileDescriptor fd;
    byte[] names;
    String errorBlurb, scheme;
    String subjectString, textString, attachmentString;
    CharSequence tem;
    String tem1;
    String[] emails;
    StringBuilder builder;
    List<Parcelable> list;

    super.onCreate (savedInstanceState);

    /* Obtain the intent that started Emacs.  */
    intent = getIntent ();
    action = intent.getAction ();
    resolver = getContentResolver ();

    if (action == null)
      {
	finish ();
	return;
      }

    /* Now see if the action specified is supported by Emacs.  */

    if (action.equals (Intent.ACTION_VIEW)
	|| action.equals (Intent.ACTION_EDIT)
	|| action.equals (Intent.ACTION_PICK)
	|| action.equals (Intent.ACTION_SEND)
	|| action.equals (Intent.ACTION_SENDTO))
      {
	/* Obtain the URI of the action.  */
	uri = intent.getData ();

	if (uri == null)
	  {
	    finish ();
	    return;
	  }

	scheme = uri.getScheme ();

	/* It is possible for scheme to be NULL, under Android 2.3 at
	   least.  */

	if (scheme == null)
	  return;

	/* If URL is a mailto URI, call `message-mailto' much the same
	   way emacsclient-mail.desktop does.  */

	if (scheme.equals ("mailto"))
	  {
	    /* Escape the special characters $ and " before enclosing
	       the string within the `message-mailto' wrapper.  */
	    fileName = uri.toString ();

	    /* If fileName is merely mailto: (absent either an email
	       address or content), then the program launching Emacs
	       conceivably provided such an URI to exclude non-email
	       programs from the Share dialog.  Intents created thus
	       might hold the recipient email as a string array, which
	       is non-standard behavior.  */

	    if (fileName.equals ("mailto:") || fileName.equals ("mailto://"))
	      {
	        emails = intent.getStringArrayExtra (Intent.EXTRA_EMAIL);

		if (emails[0] != null && emails.length > 0)
		  fileName = "mailto:" + emails[0];
	      }

	    /* Subsequently, escape fileName such that it is rendered
	       safe to append to the command line.  */

	    fileName = (fileName
			.replace ("\\", "\\\\")
			.replace ("\"", "\\\"")
			.replace ("$", "\\$"));

	    fileName = "(message-mailto \"" + fileName + "\" ";

	    /* Parse the intent itself to ascertain if any
	       non-standard subject, body, or something else of the
	       like is set.  Such fields, non-standard as they are,
	       yield to fields provided within the URL itself; refer
	       to message-mailto.  */

	    textString = attachmentString = subjectString = "()";

	    tem = intent.getCharSequenceExtra (Intent.EXTRA_SUBJECT);

	    if (tem != null)
	      subjectString = ("\"" + (tem.toString ()
				       .replace ("\\", "\\\\")
				       .replace ("\"", "\\\"")
				       .replace ("$", "\\$"))
			       + "\" ");

	    tem = intent.getCharSequenceExtra (Intent.EXTRA_TEXT);

	    if (tem != null)
	      textString = ("\"" + (tem.toString ()
				    .replace ("\\", "\\\\")
				    .replace ("\"", "\\\"")
				    .replace ("$", "\\$"))
			    + "\" ");

	    /* Producing a list of attachments is prey to two
	       mannerisms of the system: in the first instance, these
	       attachments are content URIs which don't allude to
	       their content types; and in the second instance, they
	       are either a list of such URIs or one individual URI,
	       subject to the type of the intent itself.  */

	    if (Intent.ACTION_SEND.equals (action))
	      {
		/* The attachment in this case is a single content
		   URI.  */

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU)
		  uri = intent.getParcelableExtra (Intent.EXTRA_STREAM,
						   Uri.class);
		else
		  uri = intent.getParcelableExtra (Intent.EXTRA_STREAM);

		if ((scheme = uri.getScheme ()) != null
		    && scheme.equals ("content")
		    && (Build.VERSION.SDK_INT
			>= Build.VERSION_CODES.KITKAT))
		  {
		    tem1 = EmacsService.buildContentName (uri, resolver);
		    attachmentString = ("'(\"" + (tem1.replace ("\\", "\\\\")
						  .replace ("\"", "\\\"")
						  .replace ("$", "\\$"))
					+ "\")");
		  }
		else if (scheme != null && scheme.equals ("file"))
		  {
		    tem1 = uri.getPath ();
		    attachmentString = ("'(\"" + (tem1.replace ("\\", "\\\\")
						  .replace ("\"", "\\\"")
						  .replace ("$", "\\$"))
					+ "\")");
		  }
	      }
	    else
	      {
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU)
		  list
		    = intent.getParcelableArrayListExtra (Intent.EXTRA_STREAM,
							  Parcelable.class);
		else
		  list
		    = intent.getParcelableArrayListExtra (Intent.EXTRA_STREAM);

		if (list != null)
		  {
		    builder = new StringBuilder ("'(");

		    for (Parcelable parcelable : list)
		      {
			if (!(parcelable instanceof Uri))
			  continue;

			uri = (Uri) parcelable;

			if (uri != null
			    && (scheme = uri.getScheme ()) != null
			    && scheme.equals ("content")
			    && (Build.VERSION.SDK_INT
				>= Build.VERSION_CODES.KITKAT))
			  {
			    tem1
			      = EmacsService.buildContentName (uri, resolver);
			    builder.append ("\"");
			    builder.append (tem1.replace ("\\", "\\\\")
					    .replace ("\"", "\\\"")
					    .replace ("$", "\\$"));
			    builder.append ("\"");
			  }
			else if (scheme != null
				 && scheme.equals ("file"))
			  {
			    tem1 = uri.getPath ();
			    builder.append ("\"");
			    builder.append (tem1.replace ("\\", "\\\\")
					    .replace ("\"", "\\\"")
					    .replace ("$", "\\$"));
			    builder.append ("\"");
			  }
		      }

		    builder.append (")");
		    attachmentString = builder.toString ();
		  }
	      }

	    fileName += subjectString;
	    fileName += textString;
	    fileName += attachmentString;
	    fileName += ")";

	    /* Execute emacsclient in order to execute this code.  */
	    currentActivity = this;
	    startEmacsClient (new String[] { "--timeout=10", "--no-wait",
					     "--eval", fileName, });
	    return;
	  }

	/* Now, try to get the file name.  */

	if (scheme.equals ("file"))
	  fileName = uri.getPath ();
	else
	  {
	    fileName = null;

	    if (scheme.equals ("content")
		/* Retrieving the native file descriptor of a
		   ParcelFileDescriptor requires Honeycomb MR1, and
		   proceeding without this capability is pointless on
		   systems before KitKat, since Emacs doesn't support
		   opening content files on those.  */
		&& (Build.VERSION.SDK_INT
		    >= Build.VERSION_CODES.HONEYCOMB_MR1))
	      {
		/* This is one of the annoying Android ``content''
		   URIs.  Most of the time, there is actually an
		   underlying file, but it cannot be found without
		   opening the file and doing readlink on its file
		   descriptor in /proc/self/fd.  */
		fd = null;

		try
		  {
		    fd = resolver.openFileDescriptor (uri, "r");
		    names = EmacsNative.getProcName (fd.getFd ());

		    /* What is the right encoding here? */

		    if (names != null)
		      fileName = new String (names, "UTF-8");

		    fileName = checkReadableOrCopy (fileName, fd, uri);
		  }
		catch (FileNotFoundException exception)
		  {
		    /* Do nothing.  */
		  }
		catch (IOException exception)
		  {
		    /* Do nothing.  */
		  }
		catch (SecurityException exception)
		  {
		    /* This means Emacs lacks the rights to open this
		       file.  Display the error message and exit.  */
		    displayFailureDialog ("Error opening file",
					  exception.toString ());
		    return;
		  }

		if (fd != null)
		  {
		    try
		      {
			fd.close ();
		      }
		    catch (IOException exception)
		      {
			/* Do nothing.  */
		      }
		  }
	      }
	    else if (scheme.equals ("org-protocol"))
	      /* URL is an org-protocol:// link, which is meant to be
		 directly relayed to emacsclient.  */
	      fileName = uri.toString ();

	    if (fileName == null)
	      {
		errorBlurb = ("The URI: " + uri + " could not be opened"
			      + ", as it does not encode file name inform"
			      + "ation.");
		displayFailureDialog ("Error opening file", errorBlurb);
		return;
	      }
	  }

	/* If the Emacs service is not running, then start Emacs and make
	   it open this file.  */

	if (EmacsService.SERVICE == null)
	  {
	    intent = new Intent (EmacsOpenActivity.this,
				 EmacsActivity.class);
	    intent.putExtra (EmacsActivity.EXTRA_STARTUP_ARGUMENTS,
			     new String [] { fileName, });
	    finish ();
	    startActivity (intent);
	    return;
	  }

	/* And start emacsclient.  Set `currentActivity' to this now.
	   Presumably, it will shortly become capable of displaying
	   dialogs.  */
	currentActivity = this;
	startEmacsClient (new String[] { "--timeout=10", "--no-wait",
					 "--reuse-frame", fileName, });
      }
    else
      finish ();
  }



  @Override
  public void
  onDestroy ()
  {
    /* Clear `currentActivity' if it refers to the activity being
       destroyed.  */

    if (currentActivity == this)
      this.currentActivity = null;

    super.onDestroy ();
  }

  @Override
  public void
  onWindowFocusChanged (boolean isFocused)
  {
    if (isFocused)
      currentActivity = this;
    else if (currentActivity == this)
      currentActivity = null;

    super.onWindowFocusChanged (isFocused);
  }

  @Override
  public void
  onPause ()
  {
    /* XXX: clear currentActivity here as well; I don't know whether
       or not onWindowFocusChanged is always called prior to this.  */

    if (currentActivity == this)
      currentActivity = null;

    super.onPause ();
  }
}
