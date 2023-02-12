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
import android.view.KeyEvent;

import android.text.Editable;

import android.util.Log;

/* Android input methods, take number six.

   See EmacsEditable for more details.  */

public class EmacsInputConnection extends BaseInputConnection
{
  private static final String TAG = "EmacsInputConnection";
  public EmacsView view;
  private EmacsEditable editable;

  /* The length of the last string to be committed.  */
  private int lastCommitLength;

  int currentLargeOffset;

  public
  EmacsInputConnection (EmacsView view)
  {
    super (view, false);
    this.view = view;
    this.editable = new EmacsEditable (this);
  }

  @Override
  public Editable
  getEditable ()
  {
    return editable;
  }

  @Override
  public boolean
  setComposingText (CharSequence text, int newCursorPosition)
  {
    editable.compositionStart ();
    super.setComposingText (text, newCursorPosition);
    return true;
  }

  @Override
  public boolean
  setComposingRegion (int start, int end)
  {
    int i;

    if (lastCommitLength != 0)
      {
	Log.d (TAG, "Restarting composition for: " + lastCommitLength);

	for (i = 0; i < lastCommitLength; ++i)
	  sendKeyEvent (new KeyEvent (KeyEvent.ACTION_DOWN,
				      KeyEvent.KEYCODE_DEL));

	lastCommitLength = 0;
      }

    editable.compositionStart ();
    super.setComposingRegion (start, end);
    return true;
  }
  
  @Override
  public boolean
  finishComposingText ()
  {
    editable.compositionEnd ();
    return super.finishComposingText ();
  }

  @Override
  public boolean
  beginBatchEdit ()
  {
    editable.beginBatchEdit ();
    return super.beginBatchEdit ();
  }

  @Override
  public boolean
  endBatchEdit ()
  {
    editable.endBatchEdit ();
    return super.endBatchEdit ();
  }
  
  @Override
  public boolean
  commitText (CharSequence text, int newCursorPosition)
  {
    editable.compositionEnd ();
    super.commitText (text, newCursorPosition);

    /* An observation is that input methods rarely recompose trailing
       spaces.  Avoid re-setting the commit length in that case.  */

    if (text.toString ().equals (" "))
      lastCommitLength += 1;
    else
      /* At this point, the editable is now empty.
	 
	 The input method may try to cancel the edit upon a subsequent
	 backspace by calling setComposingRegion with a region that is
	 the length of TEXT.
	 
	 Record this length in order to be able to send backspace
	 events to ``delete'' the text in that case.  */
      lastCommitLength = text.length ();

    Log.d (TAG, "commitText: \"" + text + "\"");

    return true;
  }

  /* Return a large offset, cycling through 100000, 30000, 0.
     The offset is typically used to force the input method to update
     its notion of ``surrounding text'', bypassing any caching that
     it might have in progress.

     There must be another way to do this, but I can't find it.  */

  public int
  largeSelectionOffset ()
  {
    switch (currentLargeOffset)
      {
      case 0:
	currentLargeOffset = 100000;
	return 100000;

      case 100000:
	currentLargeOffset = 30000;
	return 30000;

      case 30000:
	currentLargeOffset = 0;
	return 0;
      }

    currentLargeOffset = 0;
    return -1;
  }
}
