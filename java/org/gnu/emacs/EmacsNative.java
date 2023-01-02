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

import java.lang.System;

import android.content.res.AssetManager;

public class EmacsNative
{
  /* Set certain parameters before initializing Emacs.  This proves
     that libemacs.so is being loaded from Java code.

     assetManager must be the asset manager associated with the
     context that is loading Emacs.  It is saved and remains for the
     remainder the lifetime of the Emacs process.

     filesDir must be the package's data storage location for the
     current Android user.

     libDir must be the package's data storage location for native
     libraries.  It is used as PATH.

     pixelDensityX and pixelDensityY are the DPI values that will be
     used by Emacs.

     emacsService must be the emacsService singleton.  */
  public static native void setEmacsParams (AssetManager assetManager,
					    String filesDir,
					    String libDir,
					    float pixelDensityX,
					    float pixelDensityY,
					    EmacsService emacsService);

  /* Initialize Emacs with the argument array ARGV.  Each argument
     must contain a NULL terminated string, or else the behavior is
     undefined.  */
  public static native void initEmacs (String argv[]);

  /* Abort and generate a native core dump.  */
  public static native void emacsAbort ();

  /* Send an ANDROID_CONFIGURE_NOTIFY event.  */
  public static native void sendConfigureNotify (short window, long time,
						 int x, int y, int width,
						 int height);

  /* Send an ANDROID_KEY_PRESS event.  */
  public static native void sendKeyPress (short window, long time, int state,
					  int keyCode, int unicodeChar);

  /* Send an ANDROID_KEY_RELEASE event.  */
  public static native void sendKeyRelease (short window, long time, int state,
					    int keyCode, int unicodeChar);

  /* Send an ANDROID_FOCUS_IN event.  */
  public static native void sendFocusIn (short window, long time);

  /* Send an ANDROID_FOCUS_OUT event.  */
  public static native void sendFocusOut (short window, long time);

  /* Send an ANDROID_WINDOW_ACTION event.  */
  public static native void sendWindowAction (short window, int action);

  static
  {
    System.loadLibrary ("emacs");
  };
};
