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

import android.content.ClipboardManager;
import android.content.Context;
import android.content.ClipData;

import android.util.Log;

import android.os.Build;

import java.io.UnsupportedEncodingException;

/* This class implements EmacsClipboard for Android 3.0 and later
   systems.  */

public final class EmacsSdk11Clipboard extends EmacsClipboard
  implements ClipboardManager.OnPrimaryClipChangedListener
{
  private static final String TAG = "EmacsSdk11Clipboard";
  private ClipboardManager manager;
  private boolean ownsClipboard;
  private int clipboardChangedCount;
  private int monitoredClipboardChangedCount;

  public
  EmacsSdk11Clipboard ()
  {
    manager = EmacsService.SERVICE.getClipboardManager ();

    /* The system forbids Emacs from reading clipboard data in the
       background under Android 10 or later.  */

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.Q)
      manager.addPrimaryClipChangedListener (this);
  }

  @Override
  public synchronized void
  onPrimaryClipChanged ()
  {
    Log.d (TAG, ("onPrimaryClipChanged: "
		 + monitoredClipboardChangedCount
		 + " " + clipboardChangedCount));

    /* Increment monitoredClipboardChangeCount.  If it is now greater
       than clipboardChangedCount, then Emacs no longer owns the
       clipboard.  */
    monitoredClipboardChangedCount++;

    if (monitoredClipboardChangedCount > clipboardChangedCount)
      {
	ownsClipboard = false;

	/* Reset both values back to 0.  */
	monitoredClipboardChangedCount = 0;
	clipboardChangedCount = 0;
      }
  }

  /* Set the clipboard text to CLIPBOARD, a string in UTF-8
     encoding.  */

  @Override
  public synchronized void
  setClipboard (byte[] bytes)
  {
    ClipData data;
    String string;

    try
      {
	string = new String (bytes, "UTF-8");
	data = ClipData.newPlainText ("Emacs", string);
	manager.setPrimaryClip (data);
	ownsClipboard = true;

	/* onPrimaryClipChanged will be called again.  Use this
	   variable to keep track of how many times the clipboard has
	   been changed.  */
	++clipboardChangedCount;
      }
    catch (UnsupportedEncodingException exception)
      {
	Log.w (TAG, "setClipboard: " + exception);
      }
  }

  /* Return whether or not Emacs owns the clipboard.  Value is 1 if
     Emacs does, 0 if Emacs does not, and -1 if that information is
     unavailable.  */

  @Override
  public synchronized int
  ownsClipboard ()
  {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q)
      return -1;

    return ownsClipboard ? 1 : 0;
  }

  /* Return whether or not clipboard content currently exists.  */

  @Override
  public boolean
  clipboardExists ()
  {
    return manager.hasPrimaryClip ();
  }

  /* Return the current content of the clipboard, as plain text, or
     NULL if no content is available.  */

  @Override
  public byte[]
  getClipboard ()
  {
    ClipData clip;
    CharSequence text;
    Context context;

    clip = manager.getPrimaryClip ();

    if (clip == null || clip.getItemCount () < 1)
      return null;

    context = EmacsService.SERVICE;

    try
      {
	text = clip.getItemAt (0).coerceToText (context);
	return text.toString ().getBytes ("UTF-8");
      }
    catch (UnsupportedEncodingException exception)
      {
	Log.w (TAG, "getClipboard: " + exception);
      }

    return null;
  }
};
