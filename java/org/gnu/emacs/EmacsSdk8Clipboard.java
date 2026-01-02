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

/* Importing the entire package instead of just the legacy
   ClipboardManager class avoids the deprecation warning.  */

import android.text.*;

import android.content.Context;
import android.content.res.AssetFileDescriptor;

import android.util.Log;

import java.io.UnsupportedEncodingException;

/* This class implements EmacsClipboard for Android 2.2 and other
   similarly old systems.  */

@SuppressWarnings ("deprecation")
public final class EmacsSdk8Clipboard extends EmacsClipboard
{
  private static final String TAG = "EmacsSdk8Clipboard";
  private ClipboardManager manager;

  public
  EmacsSdk8Clipboard ()
  {
    String what;
    Context context;

    what = Context.CLIPBOARD_SERVICE;
    context = EmacsService.SERVICE;
    manager
      = (ClipboardManager) context.getSystemService (what);
  }

  /* Save the STRING into the clipboard by way of text copied by the
     user.  */

  @Override
  public void
  setClipboard (String string)
  {
    manager.setText (string);
  }

  /* Return whether or not Emacs owns the clipboard.  Value is 1 if
     Emacs does, 0 if Emacs does not, and -1 if that information is
     unavailable.  */

  @Override
  public int
  ownsClipboard ()
  {
    return -1;
  }

  /* Return whether or not clipboard content currently exists.  */

  @Override
  public boolean
  clipboardExists ()
  {
    return manager.hasText ();
  }

  /* Return the current content of the clipboard, as plain text, or
     NULL if no content is available.  */

  @Override
  public String
  getClipboard ()
  {
    String string;
    CharSequence text;

    text = manager.getText ();

    if (text == null)
      return null;

    string = text.toString ();
    return string;
  }

  /* Return an array of targets currently provided by the
     clipboard, or NULL if there are none.  */

  @Override
  public String[]
  getClipboardTargets ()
  {
    return null;
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
    return null;
  }
};
