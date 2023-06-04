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

import android.content.res.AssetManager;

import android.graphics.Bitmap;

import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;

public final class EmacsNative
{
  /* List of native libraries that must be loaded during class
     initialization.  */
  private static final String[] libraryDeps;


  /* Like `dup' in C.  */
  public static native int dup (int fd);

  /* Obtain the fingerprint of this build of Emacs.  The fingerprint
     can be used to determine the dump file name.  */
  public static native String getFingerprint ();

  /* Set certain parameters before initializing Emacs.

     assetManager must be the asset manager associated with the
     context that is loading Emacs.  It is saved and remains for the
     remainder the lifetime of the Emacs process.

     filesDir must be the package's data storage location for the
     current Android user.

     libDir must be the package's data storage location for native
     libraries.  It is used as PATH.

     cacheDir must be the package's cache directory.  It is used as
     the `temporary-file-directory'.

     pixelDensityX and pixelDensityY are the DPI values that will be
     used by Emacs.

     classPath must be the classpath of this app_process process, or
     NULL.

     emacsService must be the EmacsService singleton, or NULL.  */
  public static native void setEmacsParams (AssetManager assetManager,
					    String filesDir,
					    String libDir,
					    String cacheDir,
					    float pixelDensityX,
					    float pixelDensityY,
					    String classPath,
					    EmacsService emacsService);

  /* Initialize Emacs with the argument array ARGV.  Each argument
     must contain a NULL terminated string, or else the behavior is
     undefined.

     DUMPFILE is the dump file to use, or NULL if Emacs is to load
     loadup.el itself.

     APILEVEL is the version of Android being used.  */
  public static native void initEmacs (String argv[], String dumpFile,
				       int apiLevel);

  /* Abort and generate a native core dump.  */
  public static native void emacsAbort ();

  /* Set Vquit_flag to t, resulting in Emacs quitting as soon as
     possible.  */
  public static native void quit ();

  /* Send an ANDROID_CONFIGURE_NOTIFY event.  The values of all the
     functions below are the serials of the events sent.  */
  public static native long sendConfigureNotify (short window, long time,
						 int x, int y, int width,
						 int height);

  /* Send an ANDROID_KEY_PRESS event.  */
  public static native long sendKeyPress (short window, long time, int state,
					  int keyCode, int unicodeChar);

  /* Send an ANDROID_KEY_RELEASE event.  */
  public static native long sendKeyRelease (short window, long time, int state,
					    int keyCode, int unicodeChar);

  /* Send an ANDROID_FOCUS_IN event.  */
  public static native long sendFocusIn (short window, long time);

  /* Send an ANDROID_FOCUS_OUT event.  */
  public static native long sendFocusOut (short window, long time);

  /* Send an ANDROID_WINDOW_ACTION event.  */
  public static native long sendWindowAction (short window, int action);

  /* Send an ANDROID_ENTER_NOTIFY event.  */
  public static native long sendEnterNotify (short window, int x, int y,
					     long time);

  /* Send an ANDROID_LEAVE_NOTIFY event.  */
  public static native long sendLeaveNotify (short window, int x, int y,
					     long time);

  /* Send an ANDROID_MOTION_NOTIFY event.  */
  public static native long sendMotionNotify (short window, int x, int y,
					      long time);

  /* Send an ANDROID_BUTTON_PRESS event.  */
  public static native long sendButtonPress (short window, int x, int y,
					     long time, int state,
					     int button);

  /* Send an ANDROID_BUTTON_RELEASE event.  */
  public static native long sendButtonRelease (short window, int x, int y,
					       long time, int state,
					       int button);

  /* Send an ANDROID_TOUCH_DOWN event.  */
  public static native long sendTouchDown (short window, int x, int y,
					   long time, int pointerID);

  /* Send an ANDROID_TOUCH_UP event.  */
  public static native long sendTouchUp (short window, int x, int y,
					 long time, int pointerID);

  /* Send an ANDROID_TOUCH_MOVE event.  */
  public static native long sendTouchMove (short window, int x, int y,
					   long time, int pointerID);

  /* Send an ANDROID_WHEEL event.  */
  public static native long sendWheel (short window, int x, int y,
				       long time, int state,
				       float xDelta, float yDelta);

  /* Send an ANDROID_ICONIFIED event.  */
  public static native long sendIconified (short window);

  /* Send an ANDROID_DEICONIFIED event.  */
  public static native long sendDeiconified (short window);

  /* Send an ANDROID_CONTEXT_MENU event.  */
  public static native long sendContextMenu (short window, int menuEventID,
					     int menuEventSerial);

  /* Send an ANDROID_EXPOSE event.  */
  public static native long sendExpose (short window, int x, int y,
					int width, int height);

  /* Return the file name associated with the specified file
     descriptor, or NULL if there is none.  */
  public static native byte[] getProcName (int fd);

  /* Notice that the Emacs thread will now start waiting for the main
     thread's looper to respond.  */
  public static native void beginSynchronous ();

  /* Notice that the Emacs thread will has finished waiting for the
     main thread's looper to respond.  */
  public static native void endSynchronous ();

  /* Prevent deadlocks while reliably allowing queries from the Emacs
     thread to the main thread to complete by waiting for a query to
     start from the main thread, then answer it; assume that a query
     is certain to start shortly.  */
  public static native void answerQuerySpin ();

  /* Return whether or not KEYCODE_VOLUME_DOWN, KEYCODE_VOLUME_UP and
     KEYCODE_VOLUME_MUTE should be forwarded to Emacs.  */
  public static native boolean shouldForwardMultimediaButtons ();



  /* Input connection functions.  These mostly correspond to their
     counterparts in Android's InputConnection.  */

  public static native void beginBatchEdit (short window);
  public static native void endBatchEdit (short window);
  public static native void commitCompletion (short window, String text,
					      int position);
  public static native void commitText (short window, String text,
					int position);
  public static native void deleteSurroundingText (short window,
						   int leftLength,
						   int rightLength);
  public static native void finishComposingText (short window);
  public static native String getSelectedText (short window, int flags);
  public static native String getTextAfterCursor (short window, int length,
						  int flags);
  public static native String getTextBeforeCursor (short window, int length,
						   int flags);
  public static native void setComposingText (short window, String text,
					      int newCursorPosition);
  public static native void setComposingRegion (short window, int start,
						int end);
  public static native void setSelection (short window, int start, int end);
  public static native void performEditorAction (short window,
						 int editorAction);
  public static native void performContextMenuAction (short window,
						      int contextMenuAction);
  public static native ExtractedText getExtractedText (short window,
						       ExtractedTextRequest req,
						       int flags);
  public static native void requestSelectionUpdate (short window);
  public static native void requestCursorUpdates (short window, int mode);


  /* Return the current value of the selection, or -1 upon
     failure.  */
  public static native int[] getSelection (short window);


  /* Graphics functions used as a replacement for potentially buggy
     Android APIs.  */

  public static native void blitRect (Bitmap src, Bitmap dest, int x1,
				      int y1, int x2, int y2);

  static
  {
    /* Older versions of Android cannot link correctly with shared
       libraries that link with other shared libraries built along
       Emacs unless all requisite shared libraries are explicitly
       loaded from Java.

       Every time you add a new shared library dependency to Emacs,
       please add it here as well.  */

    libraryDeps = new String[] { "png_emacs", "selinux_emacs",
				 "crypto_emacs", "pcre_emacs",
				 "packagelistparser_emacs",
				 "gnutls_emacs", "gmp_emacs",
				 "nettle_emacs", "p11-kit_emacs",
				 "tasn1_emacs", "hogweed_emacs",
				 "jansson_emacs", "jpeg_emacs",
				 "tiff_emacs", "xml2_emacs",
				 "icuuc_emacs",
				 "tree-sitter_emacs", };

    for (String dependency : libraryDeps)
      {
	try
	  {
	    System.loadLibrary (dependency);
	  }
	catch (UnsatisfiedLinkError exception)
	  {
	    /* Ignore this exception.  */
	  }
      }

    System.loadLibrary ("emacs");
  };
};
