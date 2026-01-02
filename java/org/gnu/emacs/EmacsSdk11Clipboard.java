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

import android.content.ClipboardManager;
import android.content.Context;
import android.content.ContentResolver;
import android.content.ClipData;
import android.content.ClipDescription;

import android.content.res.AssetFileDescriptor;

import android.net.Uri;

import android.util.Log;

import android.os.Build;

import java.io.FileNotFoundException;
import java.io.IOException;
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
  private ContentResolver resolver;

  public
  EmacsSdk11Clipboard ()
  {
    manager = EmacsService.SERVICE.getClipboardManager ();

    /* The system forbids Emacs from reading clipboard data in the
       background under Android 10 or later.  */

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.Q)
      manager.addPrimaryClipChangedListener (this);

    /* Now obtain the content resolver used to open file
       descriptors.  */

    resolver = EmacsService.SERVICE.getContentResolver ();
  }

  @Override
  public synchronized void
  onPrimaryClipChanged ()
  {
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

  /* Save the STRING into the clipboard by way of text copied by the
     user.  */

  @Override
  public synchronized void
  setClipboard (String string)
  {
    ClipData data;

    data = ClipData.newPlainText ("Emacs", string);
    manager.setPrimaryClip (data);
    ownsClipboard = true;

    /* onPrimaryClipChanged will be called again.  Use this
       variable to keep track of how many times the clipboard has
       been changed.  */
    ++clipboardChangedCount;
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
  public String
  getClipboard ()
  {
    ClipData clip;
    CharSequence text;
    Context context;

    clip = manager.getPrimaryClip ();

    if (clip == null || clip.getItemCount () < 1)
      return null;

    context = EmacsService.SERVICE;
    text = clip.getItemAt (0).coerceToText (context);
    return text.toString ();
  }

  /* Return an array of targets currently provided by the
     clipboard, or NULL if there are none.  */

  @Override
  public String[]
  getClipboardTargets ()
  {
    ClipData clip;
    ClipDescription description;
    String[] typeArray;
    int i;

    /* N.B. that Android calls the clipboard the ``primary clip''; it
       is not related to the X primary selection.  */
    clip = manager.getPrimaryClip ();

    if (clip == null)
      return null;

    description = clip.getDescription ();
    i = description.getMimeTypeCount ();
    typeArray = new String[i];

    for (i = 0; i < description.getMimeTypeCount (); ++i)
      typeArray[i] = description.getMimeType (i);

    return typeArray;
  }

  /* Return the clipboard data for the given target, or NULL if it
     does not exist.

     Value is normally an asset file descriptor, which in turn holds
     three important values: the file descriptor, the start offset of
     the data, and its length; length may be
     AssetFileDescriptor.UNKNOWN_LENGTH, meaning that the data extends
     from that offset to the end of the file.

     Do not use this function to open text targets; use `getClipboard'
     for that instead, as it will handle selection data consisting
     solely of a URI.  */

  @Override
  public AssetFileDescriptor
  getClipboardData (String target)
  {
    ClipData data;
    String mimeType;
    AssetFileDescriptor assetFd;
    Uri uri;

    /* Now obtain the clipboard data and the data corresponding to
       that MIME type.  */

    mimeType = target;
    data = manager.getPrimaryClip ();

    if (data == null || data.getItemCount () < 1)
      return null;

    try
      {
	uri = data.getItemAt (0).getUri ();

	if (uri == null)
	  return null;

	/* Now open the file descriptor.  */
	assetFd = resolver.openTypedAssetFileDescriptor (uri, mimeType,
							 null);
	return assetFd;
      }
    catch (SecurityException e)
      {
	return null;
      }
    catch (FileNotFoundException e)
      {
	return null;
      }
  }
};
