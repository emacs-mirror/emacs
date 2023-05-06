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
import android.view.inputmethod.TextSnapshot;
import android.view.KeyEvent;

import android.os.Build;

import android.util.Log;

/* Android input methods, take number six.  See textconv.c for more
   details; this is more-or-less a thin wrapper around that file.  */

public final class EmacsInputConnection extends BaseInputConnection
{
  private static final String TAG = "EmacsInputConnection";
  private EmacsView view;
  private short windowHandle;

  /* Whether or not to synchronize and call `updateIC' with the
     selection position after committing text.

     This helps with on screen keyboard programs found in some vendor
     versions of Android, which rely on immediate updates to the point
     position after text is commited in order to place the cursor
     within that text.  */

  private static boolean syncAfterCommit;

  /* Whether or not to return empty text with the offset set to zero
     if a request arrives that has no flags set and has requested no
     characters at all.

     This is necessary with on screen keyboard programs found in some
     vendor versions of Android which don't rely on the documented
     meaning of `ExtractedText.startOffset', and instead take the
     selection offset inside at face value.  */

  private static boolean extractAbsoluteOffsets;

  static
  {
    if (Build.MANUFACTURER.equalsIgnoreCase ("Huawei")
	|| Build.MANUFACTURER.equalsIgnoreCase ("Honor"))
      extractAbsoluteOffsets = syncAfterCommit = true;
  };

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
    int[] selection;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "commitText: " + text + " " + newCursorPosition);

    EmacsNative.commitText (windowHandle, text.toString (),
			    newCursorPosition);

    if (syncAfterCommit)
      {
	/* Synchronize with the Emacs thread, obtain the new
	   selection, and report it immediately.  */

	selection = EmacsNative.getSelection (windowHandle);

	if (EmacsService.DEBUG_IC && selection != null)
	  Log.d (TAG, "commitText: new selection is " + selection[0]
		 + ", by " + selection[1]);

	if (selection != null)
	  /* N.B. that the composing region is removed after text is
	     committed.  */
	  view.imManager.updateSelection (view, selection[0],
					  selection[1], -1, -1);
      }

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
    ExtractedText text;
    int[] selection;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "getExtractedText: " + request.hintMaxChars + ", "
	     + request.hintMaxLines + " " + flags);

    /* If a request arrives with hintMaxChars, hintMaxLines and flags
       set to 0, and the system is known to be buggy, return an empty
       extracted text object with the absolute selection positions.  */

    if (extractAbsoluteOffsets
	&& request.hintMaxChars == 0
	&& request.hintMaxLines == 0
	&& flags == 0)
      {
	/* Obtain the selection.  */
	selection = EmacsNative.getSelection (windowHandle);
	if (selection == null)
	  return null;

	/* Create the workaround extracted text.  */
	text = new ExtractedText ();
	text.partialStartOffset = -1;
	text.partialEndOffset = -1;
	text.text = "";
	text.selectionStart = selection[0];
	text.selectionEnd = selection[1];
      }
    else
      text = EmacsNative.getExtractedText (windowHandle, request,
					   flags);

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "getExtractedText: " + text.text + " @"
	     + text.startOffset + ":" + text.selectionStart
	     + ", " + text.selectionEnd);

    return text;
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

  @Override
  public boolean
  sendKeyEvent (KeyEvent key)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "sendKeyEvent: " + key);

    return super.sendKeyEvent (key);
  }

  @Override
  public boolean
  deleteSurroundingTextInCodePoints (int beforeLength, int afterLength)
  {
    /* This can be implemented the same way as
       deleteSurroundingText.  */
    return this.deleteSurroundingText (beforeLength, afterLength);
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
