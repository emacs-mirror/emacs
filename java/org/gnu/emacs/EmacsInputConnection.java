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

import android.view.inputmethod.BaseInputConnection;
import android.view.inputmethod.CompletionInfo;
import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;
import android.view.inputmethod.InputMethodManager;
import android.view.inputmethod.SurroundingText;
import android.view.inputmethod.TextSnapshot;
import android.view.KeyEvent;

import android.text.Editable;

import android.util.Log;

/* Android input methods, take number six.

   See EmacsEditable for more details.  */

public class EmacsInputConnection extends BaseInputConnection
{
  private static final String TAG = "EmacsInputConnection";
  private EmacsView view;
  private short windowHandle;

  public
  EmacsInputConnection (EmacsView view)
  {
    super (view, true);

    this.view = view;
    this.windowHandle = view.window.handle;
  }

  @Override
  public boolean
  beginBatchEdit ()
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "beginBatchEdit");

    EmacsNative.beginBatchEdit (windowHandle);
    return true;
  }

  @Override
  public boolean
  endBatchEdit ()
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "endBatchEdit");

    EmacsNative.endBatchEdit (windowHandle);
    return true;
  }

  @Override
  public boolean
  commitCompletion (CompletionInfo info)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "commitCompletion: " + info);

    EmacsNative.commitCompletion (windowHandle,
				  info.getText ().toString (),
				  info.getPosition ());
    return true;
  }

  @Override
  public boolean
  commitText (CharSequence text, int newCursorPosition)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "commitText: " + text + " " + newCursorPosition);

    EmacsNative.commitText (windowHandle, text.toString (),
			    newCursorPosition);
    return true;
  }

  @Override
  public boolean
  deleteSurroundingText (int leftLength, int rightLength)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, ("deleteSurroundingText: "
		   + leftLength + " " + rightLength));

    EmacsNative.deleteSurroundingText (windowHandle, leftLength,
				       rightLength);
    return true;
  }

  @Override
  public boolean
  finishComposingText ()
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "finishComposingText");

    EmacsNative.finishComposingText (windowHandle);
    return true;
  }

  @Override
  public String
  getSelectedText (int flags)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "getSelectedText: " + flags);

    return EmacsNative.getSelectedText (windowHandle, flags);
  }

  @Override
  public String
  getTextAfterCursor (int length, int flags)
  {
    String string;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "getTextAfterCursor: " + length + " " + flags);

    string = EmacsNative.getTextAfterCursor (windowHandle, length,
					     flags);

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "   --> " + string);

    return string;
  }

  @Override
  public String
  getTextBeforeCursor (int length, int flags)
  {
    String string;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "getTextBeforeCursor: " + length + " " + flags);

    string = EmacsNative.getTextBeforeCursor (windowHandle, length,
					      flags);

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "   --> " + string);

    return string;
  }

  @Override
  public boolean
  setComposingText (CharSequence text, int newCursorPosition)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, ("setComposingText: "
		   + text + " ## " + newCursorPosition));

    EmacsNative.setComposingText (windowHandle, text.toString (),
				  newCursorPosition);
    return true;
  }

  @Override
  public boolean
  setComposingRegion (int start, int end)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "setComposingRegion: " + start + " " + end);

    EmacsNative.setComposingRegion (windowHandle, start, end);
    return true;
  }

  @Override
  public boolean
  performEditorAction (int editorAction)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "performEditorAction: " + editorAction);

    EmacsNative.performEditorAction (windowHandle, editorAction);
    return true;
  }

  @Override
  public ExtractedText
  getExtractedText (ExtractedTextRequest request, int flags)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "getExtractedText: " + request + " " + flags);

    return EmacsNative.getExtractedText (windowHandle, request,
					 flags);
  }

  @Override
  public boolean
  setSelection (int start, int end)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "setSelection: " + start + " " + end);

    EmacsNative.setSelection (windowHandle, start, end);
    return true;
  }


  /* Override functions which are not implemented.  */

  @Override
  public TextSnapshot
  takeSnapshot ()
  {
    Log.d (TAG, "takeSnapshot");
    return null;
  }
}
