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

import android.text.InputFilter;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.SpanWatcher;
import android.text.Selection;

import android.content.Context;

import android.view.inputmethod.InputMethodManager;
import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;

import android.text.Spannable;

import android.util.Log;

import android.os.Build;

/* Android input methods insist on having access to buffer contents.
   Since Emacs is not designed like ``any other Android text editor'',
   that is not possible.

   This file provides a fake editing buffer that is designed to weasel
   as much information as possible out of an input method, without
   actually providing buffer contents to Emacs.

   The basic idea is to have the fake editing buffer be initially
   empty.

   When the input method inserts composed text, it sets a flag.
   Updates to the buffer while the flag is set are sent to Emacs to be
   displayed as ``preedit text''.

   Once some heuristics decide that composition has been completed,
   the composed text is sent to Emacs, and the text that was inserted
   in this editing buffer is erased.  */

public class EmacsEditable extends SpannableStringBuilder
  implements SpanWatcher
{
  private static final String TAG = "EmacsEditable";

  /* Whether or not composition is currently in progress.  */
  private boolean isComposing;

  /* The associated input connection.  */
  private EmacsInputConnection connection;

  /* The associated IM manager.  */
  private InputMethodManager imManager;

  /* Any extracted text an input method may be monitoring.  */
  private ExtractedText extractedText;

  /* The corresponding text request.  */
  private ExtractedTextRequest extractRequest;

  /* The number of nested batch edits.  */
  private int batchEditCount;

  /* Whether or not invalidateInput should be called upon batch edits
     ending.  */
  private boolean pendingInvalidate;

  /* The ``composing span'' indicating the bounds of an ongoing
     character composition.  */
  private Object composingSpan;

  public
  EmacsEditable (EmacsInputConnection connection)
  {
    /* Initialize the editable with one initial space, so backspace
       always works.  */
    super ();

    Object tem;
    Context context;

    this.connection = connection;

    context = connection.view.getContext ();
    tem = context.getSystemService (Context.INPUT_METHOD_SERVICE);
    imManager = (InputMethodManager) tem;

    /* To watch for changes to text properties on Android, you
       add... a text property.  */
    setSpan (this, 0, 0, Spanned.SPAN_INCLUSIVE_INCLUSIVE);
  }

  public void
  endBatchEdit ()
  {
    if (batchEditCount < 1)
      return;

    if (--batchEditCount == 0 && pendingInvalidate)
      invalidateInput ();
  }

  public void
  beginBatchEdit ()
  {
    ++batchEditCount;
  }

  public void
  setExtractedTextAndRequest (ExtractedText text,
			      ExtractedTextRequest request,
			      boolean monitor)
  {
    /* Extract the text.  If monitor is set, also record it as the
       text that is currently being extracted.  */

    text.startOffset = 0;
    text.selectionStart = Selection.getSelectionStart (this);
    text.selectionEnd = Selection.getSelectionStart (this);
    text.text = this;

    if (monitor)
      {
	extractedText = text;
	extractRequest = request;
      }
  }

  public void
  compositionStart ()
  {
    isComposing = true;
  }

  public void
  compositionEnd ()
  {
    isComposing = false;
    sendComposingText (null);
  }

  private void
  sendComposingText (String string)
  {
    EmacsWindow window;
    long time, serial;

    window = connection.view.window;

    if (window.isDestroyed ())
      return;

    time = System.currentTimeMillis ();

    /* A composition event is simply a special key event with a
       keycode of -1.  */

    synchronized (window.eventStrings)
      {
	serial
	  = EmacsNative.sendKeyPress (window.handle, time, 0, -1, -1);

	/* Save the string so that android_lookup_string can find
	   it.  */
	if (string != null)
	  window.saveUnicodeString ((int) serial, string);
      }
  }

  private void
  invalidateInput ()
  {
    int start, end, composingSpanStart, composingSpanEnd;

    if (batchEditCount > 0)
      {
	Log.d (TAG, "invalidateInput: deferring for batch edit");
	pendingInvalidate = true;
	return;
      }

    pendingInvalidate = false;

    start = Selection.getSelectionStart (this);
    end = Selection.getSelectionEnd (this);

    if (composingSpan != null)
      {
	composingSpanStart = getSpanStart (composingSpan);
	composingSpanEnd = getSpanEnd (composingSpan);
      }
    else
      {
	composingSpanStart = -1;
	composingSpanEnd = -1;
      }

    Log.d (TAG, "invalidateInput: now " + start + ", " + end);

    /* Tell the input method that the cursor changed.  */
    imManager.updateSelection (connection.view, start, end,
			       composingSpanStart,
			       composingSpanEnd);

    /* If there is any extracted text, tell the IME that it has
       changed.  */
    if (extractedText != null)
      imManager.updateExtractedText (connection.view,
				     extractRequest.token,
				     extractedText);
  }

  public SpannableStringBuilder
  replace (int start, int end, CharSequence tb, int tbstart,
	   int tbend)
  {
    super.replace (start, end, tb, tbstart, tbend);

    /* If a change happens during composition, perform the change and
       then send the text being composed.  */

    if (isComposing)
      sendComposingText (toString ());

    return this;
  }

  private boolean
  isSelectionSpan (Object span)
  {
    return ((Selection.SELECTION_START == span
	     || Selection.SELECTION_END == span)
	    && (getSpanFlags (span)
		& Spanned.SPAN_INTERMEDIATE) == 0);
  }

  @Override
  public void
  onSpanAdded (Spannable text, Object what, int start, int end)
  {
    Log.d (TAG, "onSpanAdded: " + text + " " + what + " "
	   + start + " " + end);

    /* Try to find the composing span.  This isn't a public API.  */

    if (what.getClass ().getName ().contains ("ComposingText"))
      composingSpan = what;

    if (isSelectionSpan (what))
      invalidateInput ();
  }

  @Override
  public void
  onSpanChanged (Spannable text, Object what, int ostart,
		 int oend, int nstart, int nend)
  {
    Log.d (TAG, "onSpanChanged: " + text + " " + what + " "
	   + nstart + " " + nend);

    if (isSelectionSpan (what))
      invalidateInput ();
  }

  @Override
  public void
  onSpanRemoved (Spannable text, Object what,
		 int start, int end)
  {
    Log.d (TAG, "onSpanRemoved: " + text + " " + what + " "
	   + start + " " + end);

    if (isSelectionSpan (what))
      invalidateInput ();
  }

  public boolean
  isInBatchEdit ()
  {
    return batchEditCount > 0;
  }
}
