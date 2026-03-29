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

import android.content.res.AssetFileDescriptor;
import android.os.Build;

/* This class provides helper code for accessing the clipboard,
   abstracting between the different interfaces on API 8 and 11.  */

public abstract class EmacsClipboard
{
  public abstract void setClipboard (String string);
  public abstract int ownsClipboard ();
  public abstract boolean clipboardExists ();
  public abstract String getClipboard ();

  public abstract String[] getClipboardTargets ();
  public abstract AssetFileDescriptor getClipboardData (String target);

  /* Create the correct kind of clipboard for this system.  */

  public static EmacsClipboard
  makeClipboard ()
  {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB)
      return new EmacsSdk11Clipboard ();
    else
      return new EmacsSdk8Clipboard ();
  }
};
