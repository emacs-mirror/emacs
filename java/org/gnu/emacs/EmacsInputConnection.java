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

import android.os.Build;
import android.os.Bundle;
import android.os.Handler;

import android.view.KeyEvent;

import android.view.inputmethod.CompletionInfo;
import android.view.inputmethod.CorrectionInfo;
import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputContentInfo;
import android.view.inputmethod.SurroundingText;
import android.view.inputmethod.TextAttribute;
import android.view.inputmethod.TextSnapshot;

import android.util.Log;

/* Android input methods, take number six.  See textconv.c for more
   details; this is more-or-less a thin wrapper around that file.  */

public final class EmacsInputConnection implements InputConnection
{
  private static final String TAG = "EmacsInputConnection";

  /* View associated with this input connection.  */
  private EmacsView view;

  /* The handle ID associated with that view's window.  */
  private long windowHandle;

  /* Number of batch edits currently underway.  Used to avoid
     synchronizing with the Emacs thread after each
     `endBatchEdit'.  */
  private int batchEditCount;

  /* Whether or not to synchronize and call `updateIC' with the
     selection position after committing text.

     This helps with on screen keyboard programs found in some vendor
     versions of Android, which rely on immediate updates to the point
     position after text is committed in order to place the cursor
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

    /* The Samsung and Vivo keyboards take `selectionStart' at face
       value if some text is returned, and also search for words solely
       within that text.  However, when no text is returned, it falls
       back to getTextAfterCursor and getTextBeforeCursor.  */
    if (Build.MANUFACTURER.equalsIgnoreCase ("Samsung")
	|| Build.MANUFACTURER.equalsIgnoreCase ("Vivo"))
      extractAbsoluteOffsets = true;
  };


  public
  EmacsInputConnection (EmacsView view)
  {
    this.view = view;
    this.windowHandle = view.window.handle;
  }


  /* The functions below are called by input methods whenever they
     need to perform an edit.  */

  @Override
  public boolean
  beginBatchEdit ()
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "beginBatchEdit");

    EmacsNative.beginBatchEdit (windowHandle);

    /* Keep a record of the number of outstanding batch edits here as
       well.  */
    batchEditCount++;
    return true;
  }

  @Override
  public boolean
  endBatchEdit ()
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "endBatchEdit");

    EmacsNative.endBatchEdit (windowHandle);

    /* Subtract one from the UI thread record of the number of batch
       edits currently under way.  */

    if (batchEditCount > 0)
      batchEditCount -= 1;

    return batchEditCount > 0;
  }

  public boolean
  commitCompletion (CompletionInfo info)
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "commitCompletion: " + info);

    EmacsNative.commitCompletion (windowHandle,
				  info.getText ().toString (),
				  info.getPosition ());
    return true;
  }

  @Override
  public boolean
  commitCorrection (CorrectionInfo info)
  {
    /* The input method calls this function not to commit text, but to
       indicate that a subsequent edit will consist of a correction.
       Emacs has no use for this information.

       Of course this completely contradicts the provided
       documentation, but this is how Android actually behaves.  */
    return false;
  }

  @Override
  public boolean
  commitText (CharSequence text, int newCursorPosition)
  {
    int[] selection;

    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

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
  commitText (CharSequence text, int newCursorPosition,
	      TextAttribute textAttribute)
  {
    return commitText (text, newCursorPosition);
  }

  @Override
  public boolean
  deleteSurroundingText (int leftLength, int rightLength)
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, ("deleteSurroundingText: "
		   + leftLength + " " + rightLength));

    EmacsNative.deleteSurroundingText (windowHandle, leftLength,
				       rightLength);
    return true;
  }

  @Override
  public boolean
  deleteSurroundingTextInCodePoints (int leftLength, int rightLength)
  {
    /* Emacs returns characters which cannot be represented in a Java
       `char' as NULL characters, so code points always reflect
       characters themselves.  */
    return deleteSurroundingText (leftLength, rightLength);
  }

  @Override
  public boolean
  finishComposingText ()
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "finishComposingText");

    EmacsNative.finishComposingText (windowHandle);
    return true;
  }

  @Override
  public String
  getSelectedText (int flags)
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return null;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "getSelectedText: " + flags);

    return EmacsNative.getSelectedText (windowHandle, flags);
  }

  @Override
  public String
  getTextAfterCursor (int length, int flags)
  {
    String string;

    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return null;

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

    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return null;

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
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, ("setComposingText: "
		   + text + " ## " + newCursorPosition));

    EmacsNative.setComposingText (windowHandle, text.toString (),
				  newCursorPosition);
    return true;
  }

  @Override
  public boolean
  setComposingText (CharSequence text, int newCursorPosition,
		    TextAttribute textAttribute)
  {
    return setComposingText (text, newCursorPosition);
  }

  @Override
  public boolean
  setComposingRegion (int start, int end)
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "setComposingRegion: " + start + " " + end);

    EmacsNative.setComposingRegion (windowHandle, start, end);
    return true;
  }

  @Override
  public boolean
  setComposingRegion (int start, int end, TextAttribute textAttribute)
  {
    return setComposingRegion (start, end);
  }

  @Override
  public boolean
  performEditorAction (int editorAction)
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "performEditorAction: " + editorAction);

    EmacsNative.performEditorAction (windowHandle, editorAction);
    return true;
  }

  @Override
  public boolean
  performContextMenuAction (int contextMenuAction)
  {
    int action;

    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "performContextMenuAction: " + contextMenuAction);

    /* Translate the action in Java code.  That way, a great deal of
       JNI boilerplate can be avoided.  */

    switch (contextMenuAction)
      {
      case android.R.id.selectAll:
	action = 0;
	break;

      case android.R.id.startSelectingText:
	action = 1;
	break;

      case android.R.id.stopSelectingText:
	action = 2;
	break;

      case android.R.id.cut:
	action = 3;
	break;

      case android.R.id.copy:
	action = 4;
	break;

      case android.R.id.paste:
	action = 5;
	break;

      default:
	return true;
      }

    EmacsNative.performContextMenuAction (windowHandle, action);
    return true;
  }

  @Override
  public ExtractedText
  getExtractedText (ExtractedTextRequest request, int flags)
  {
    ExtractedText text;
    int[] selection;

    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return null;

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

    if (text == null)
      {
	if (EmacsService.DEBUG_IC)
	  Log.d (TAG, "getExtractedText: text is NULL");

	return null;
      }

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
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "setSelection: " + start + " " + end);

    EmacsNative.setSelection (windowHandle, start, end);
    return true;
  }

  @Override
  /* ACTION_MULTIPLE is apparently obsolete.  */
  @SuppressWarnings ("deprecation")
  public boolean
  sendKeyEvent (KeyEvent key)
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "sendKeyEvent: " + key);

    /* Use the standard API if possible.  */

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N)
      view.imManager.dispatchKeyEventFromInputMethod (view, key);
    else
      {
	/* Fall back to dispatching the event manually if not.  */

	switch (key.getAction ())
	  {
	  case KeyEvent.ACTION_DOWN:
	    view.onKeyDown (key.getKeyCode (), key);
	    break;

	  case KeyEvent.ACTION_UP:
	    view.onKeyUp (key.getKeyCode (), key);
	    break;

	  case KeyEvent.ACTION_MULTIPLE:
	    view.onKeyMultiple (key.getKeyCode (),
				key.getRepeatCount (),
				key);
	    break;
	  }
      }

    return true;
  }

  @Override
  public boolean
  requestCursorUpdates (int cursorUpdateMode)
  {
    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return false;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, "requestCursorUpdates: " + cursorUpdateMode);

    EmacsNative.requestCursorUpdates (windowHandle, cursorUpdateMode);
    return true;
  }

  @Override
  public boolean
  requestCursorUpdates (int cursorUpdateMode, int filter)
  {
    if (filter != 0)
      return false;

    return requestCursorUpdates (cursorUpdateMode);
  }

  @Override
  public SurroundingText
  getSurroundingText (int beforeLength, int afterLength,
		      int flags)
  {
    SurroundingText text;

    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return null;

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, ("getSurroundingText: " + beforeLength + ", "
		   + afterLength));

    text = EmacsNative.getSurroundingText (windowHandle, beforeLength,
					   afterLength, flags);

    if (EmacsService.DEBUG_IC && text != null)
      Log.d (TAG, ("getSurroundingText: "
		   + text.getSelectionStart ()
		   + ","
		   + text.getSelectionEnd ()
		   + "+"
		   + text.getOffset ()
		   + ": "
		   + text.getText ()));

    return text;
  }

  @Override
  public TextSnapshot
  takeSnapshot ()
  {
    TextSnapshot snapshot;

    /* Return if the input connection is out of date.  */
    if (view.icSerial < view.icGeneration)
      return null;

    snapshot = EmacsNative.takeSnapshot (windowHandle);

    if (EmacsService.DEBUG_IC)
      Log.d (TAG, ("takeSnapshot: "
		   + snapshot.getSurroundingText ().getText ()
		   + " @ " + snapshot.getCompositionEnd ()
		   + ", " + snapshot.getCompositionStart ()));

    return snapshot;
  }

  @Override
  public void
  closeConnection ()
  {
    batchEditCount = 0;
  }

  @Override
  public boolean
  replaceText (int start, int end, CharSequence text,
	       int newCursorPosition, TextAttribute attributes)
  {
    if (EmacsService.DEBUG_IC)
      Log.d (TAG, ("replaceText: " + text + ":: " + start + ","
		   + end + "," + newCursorPosition));

    EmacsNative.replaceText (windowHandle, start, end,
			     text.toString (), newCursorPosition,
			     attributes);
    return true;
  }



  public void
  reset ()
  {
    batchEditCount = 0;
  }


  /* Override functions which are not implemented.  */

  @Override
  public Handler
  getHandler ()
  {
    return null;
  }

  @Override
  public boolean
  commitContent (InputContentInfo inputContentInfo, int flags,
		 Bundle opts)
  {
    return false;
  }

  @Override
  public boolean
  setImeConsumesInput (boolean imeConsumesInput)
  {
    return false;
  }

  @Override
  public boolean
  clearMetaKeyStates (int states)
  {
    return false;
  }

  @Override
  public boolean
  reportFullscreenMode (boolean enabled)
  {
    return false;
  }

  @Override
  public boolean
  performSpellCheck ()
  {
    return false;
  }

  @Override
  public boolean
  performPrivateCommand (String action, Bundle data)
  {
    return false;
  }

  @Override
  public int
  getCursorCapsMode (int reqModes)
  {
    return 0;
  }
}
