/* String conversion support for graphics terminals.

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

/* String conversion support.

   Many input methods require access to text surrounding the cursor.
   They may then request that the text editor remove or substitute
   that text for something else, for example when providing the
   ability to ``undo'' or ``edit'' previously composed text.  This is
   most commonly seen in input methods for CJK laguages for X Windows,
   and is extensively used throughout Android by input methods for all
   kinds of scripts.

   In addition, these input methods may also need to make detailed
   edits to the content of a buffer.  That is also handled here.  */

#include <config.h>

#include "textconv.h"
#include "buffer.h"
#include "syntax.h"



/* The window system's text conversion interface.
   NULL when the window system has not set up text conversion.

   This interface will later be heavily extended on the
   feature/android branch to deal with Android's much less
   straightforward text conversion protocols.  */

static struct textconv_interface *text_interface;

/* Flags used to determine what must be sent after a batch edit
   ends.  */

enum textconv_batch_edit_flags
  {
    PENDING_POINT_CHANGE   = 1,
    PENDING_COMPOSE_CHANGE = 2,
  };



/* Copy the portion of the current buffer described by BEG, BEG_BYTE,
   END, END_BYTE to the buffer BUFFER, which is END_BYTE - BEG_BYTEs
   long.  */

static void
copy_buffer (ptrdiff_t beg, ptrdiff_t beg_byte,
	     ptrdiff_t end, ptrdiff_t end_byte,
	     char *buffer)
{
  ptrdiff_t beg0, end0, beg1, end1, size;

  if (beg_byte < GPT_BYTE && GPT_BYTE < end_byte)
    {
      /* Two regions, before and after the gap.  */
      beg0 = beg_byte;
      end0 = GPT_BYTE;
      beg1 = GPT_BYTE + GAP_SIZE - BEG_BYTE;
      end1 = end_byte + GAP_SIZE - BEG_BYTE;
    }
  else
    {
      /* The only region.  */
      beg0 = beg_byte;
      end0 = end_byte;
      beg1 = -1;
      end1 = -1;
    }

  size = end0 - beg0;
  memcpy (buffer, BYTE_POS_ADDR (beg0), size);
  if (beg1 != -1)
    memcpy (buffer, BEG_ADDR + beg1, end1 - beg1);
}



/* Conversion query.  */

/* Perform the text conversion operation specified in QUERY and return
   the results.

   Find the text between QUERY->position from point on F's selected
   window and QUERY->factor times QUERY->direction from that
   position.  Return it in QUERY->text.

   Then, either delete that text from the buffer if QUERY->operation
   is TEXTCONV_SUBSTITUTION, or return 0.

   If FLAGS & TEXTCONV_SKIP_CONVERSION_REGION, then first move PT past
   the conversion region in the specified direction if it is inside.

   Value is 0 if QUERY->operation was not TEXTCONV_SUBSTITUTION
   or if deleting the text was successful, and 1 otherwise.  */

int
textconv_query (struct frame *f, struct textconv_callback_struct *query,
		int flags)
{
  specpdl_ref count;
  ptrdiff_t pos, pos_byte, end, end_byte, start;
  ptrdiff_t temp, temp1;
  char *buffer;

  /* Save the excursion, as there will be extensive changes to the
     selected window.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect_excursion ();

  /* Inhibit quitting.  */
  specbind (Qinhibit_quit, Qt);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  Fselect_window ((WINDOW_LIVE_P (f->old_selected_window)
		   ? f->old_selected_window
		   : f->selected_window), Qt);

  /* Now find the appropriate text bounds for QUERY.  First, move
     point QUERY->position steps forward or backwards.  */

  pos = PT;

  /* Next, if POS lies within the conversion region and the caller
     asked for it to be moved away, move it away from the conversion
     region.  */

  if (flags & TEXTCONV_SKIP_CONVERSION_REGION
      && MARKERP (f->conversion.compose_region_start))
    {
      start = marker_position (f->conversion.compose_region_start);
      end = marker_position (f->conversion.compose_region_end);

      if (pos >= start && pos < end)
	{
	  switch (query->direction)
	    {
	    case TEXTCONV_FORWARD_CHAR:
	    case TEXTCONV_FORWARD_WORD:
	    case TEXTCONV_CARET_DOWN:
	    case TEXTCONV_NEXT_LINE:
	    case TEXTCONV_LINE_START:
	      pos = end;
	      break;

	    default:
	      pos = max (BEGV, start - 1);
	      break;
	    }
	}
    }

  /* If pos is outside the accessible part of the buffer or if it
     overflows, move back to point or to the extremes of the
     accessible region.  */

  if (INT_ADD_WRAPV (pos, query->position, &pos))
    pos = PT;

  if (pos < BEGV)
    pos = BEGV;

  if (pos > ZV)
    pos = ZV;

  /* Move to pos.  */
  set_point (pos);
  pos = PT;
  pos_byte = PT_BYTE;

  /* Now scan forward or backwards according to what is in QUERY.  */

  switch (query->direction)
    {
    case TEXTCONV_FORWARD_CHAR:
      /* Move forward by query->factor characters.  */
      if (INT_ADD_WRAPV (pos, query->factor, &end) || end > ZV)
	end = ZV;

      end_byte = CHAR_TO_BYTE (end);
      break;

    case TEXTCONV_BACKWARD_CHAR:
      /* Move backward by query->factor characters.  */
      if (INT_SUBTRACT_WRAPV (pos, query->factor, &end) || end < BEGV)
	end = BEGV;

      end_byte = CHAR_TO_BYTE (end);
      break;

    case TEXTCONV_FORWARD_WORD:
      /* Move forward by query->factor word.  */
      end = scan_words (pos, (EMACS_INT) query->factor);

      if (!end)
	{
	  end = ZV;
	  end_byte = ZV_BYTE;
	}
      else
	end_byte = CHAR_TO_BYTE (end);

      break;

    case TEXTCONV_BACKWARD_WORD:
      /* Move backwards by query->factor word.  */
      end = scan_words (pos, 0 - (EMACS_INT) query->factor);

      if (!end)
	{
	  end = BEGV;
	  end_byte = BEGV_BYTE;
	}
      else
	end_byte = CHAR_TO_BYTE (end);

      break;

    case TEXTCONV_CARET_UP:
      /* Move upwards one visual line, keeping the column intact.  */
      Fvertical_motion (Fcons (Fcurrent_column (), make_fixnum (-1)),
			Qnil, Qnil);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_CARET_DOWN:
      /* Move downwards one visual line, keeping the column
	 intact.  */
      Fvertical_motion (Fcons (Fcurrent_column (), make_fixnum (1)),
			Qnil, Qnil);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_NEXT_LINE:
      /* Move one line forward.  */
      scan_newline (pos, pos_byte, ZV, ZV_BYTE,
		    query->factor, false);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_PREVIOUS_LINE:
      /* Move one line backwards.  */
      scan_newline (pos, pos_byte, BEGV, BEGV_BYTE,
		    0 - (EMACS_INT) query->factor, false);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_LINE_START:
      /* Move to the beginning of the line.  */
      Fbeginning_of_line (Qnil);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_LINE_END:
      /* Move to the end of the line.  */
      Fend_of_line (Qnil);
      end = PT;
      end_byte = PT_BYTE;
      break;

    case TEXTCONV_ABSOLUTE_POSITION:
      /* How to implement this is unclear.  */
      SET_PT (query->factor);
      end = PT;
      end_byte = PT_BYTE;
      break;

    default:
      unbind_to (count, Qnil);
      return 1;
    }

  /* Sort end and pos.  */

  if (end < pos)
    {
      eassert (end_byte < pos_byte);
      temp = pos_byte;
      temp1 = pos;
      pos_byte = end_byte;
      pos = end;
      end = temp1;
      end_byte = temp;
    }

  /* Return the string first.  */
  buffer = xmalloc (end_byte - pos_byte);
  copy_buffer (pos, pos_byte, end, end_byte, buffer);
  query->text.text = buffer;
  query->text.length = end - pos;
  query->text.bytes = end_byte - pos_byte;

  /* Next, perform any operation specified.  */

  switch (query->operation)
    {
    case TEXTCONV_SUBSTITUTION:
      if (safe_del_range (pos, end))
	{
	  /* Undo any changes to the excursion.  */
	  unbind_to (count, Qnil);
	  return 1;
	}

    default:
      break;
    }

  /* Undo any changes to the excursion.  */
  unbind_to (count, Qnil);
  return 0;
}

/* Reset F's text conversion state.  Delete any overlays or
   markers inside.  */

void
reset_frame_state (struct frame *f)
{
  struct text_conversion_action *last, *next;

  /* Make the composition region markers point elsewhere.  */

  if (!NILP (f->conversion.compose_region_start))
    {
      Fset_marker (f->conversion.compose_region_start, Qnil, Qnil);
      Fset_marker (f->conversion.compose_region_end, Qnil, Qnil);
      f->conversion.compose_region_start = Qnil;
      f->conversion.compose_region_end = Qnil;
    }

  /* Delete the composition region overlay.  */

  if (!NILP (f->conversion.compose_region_overlay))
    Fdelete_overlay (f->conversion.compose_region_overlay);

  /* Delete each text conversion action queued up.  */

  next = f->conversion.actions;
  while (next)
    {
      last = next;
      next = next->next;

      /* Say that the conversion is finished.  */
      if (text_interface && text_interface->notify_conversion)
	text_interface->notify_conversion (last->counter);

      xfree (last);
    }
  f->conversion.actions = NULL;

  /* Clear batch edit state.  */
  f->conversion.batch_edit_count = 0;
  f->conversion.batch_edit_flags = 0;
}

/* Return whether or not there are pending edits from an input method
   on any frame.  */

bool
detect_conversion_events (void)
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    {
      if (XFRAME (frame)->conversion.actions)
	return true;
    }

  return false;
}

/* Restore the selected window WINDOW.  */

static void
restore_selected_window (Lisp_Object window)
{
  /* FIXME: not sure what to do if WINDOW has been deleted.  */
  Fselect_window (window, Qt);
}

/* Commit the given text in the composing region.  If there is no
   composing region, then insert the text after F's selected window's
   last point instead.  Finally, remove the composing region.  */

static void
really_commit_text (struct frame *f, EMACS_INT position,
		    Lisp_Object text)
{
  specpdl_ref count;
  ptrdiff_t wanted, start, end;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  Fselect_window (f->old_selected_window, Qt);

  /* Now detect whether or not there is a composing region.
     If there is, then replace it with TEXT.  Don't do that
     otherwise.  */

  if (MARKERP (f->conversion.compose_region_start))
    {
      /* Replace its contents.  */
      start = marker_position (f->conversion.compose_region_start);
      end = marker_position (f->conversion.compose_region_end);
      safe_del_range (start, end);
      Finsert (1, &text);

      /* Move to a the position specified in POSITION.  */

      if (position < 0)
	{
	  wanted
	    = marker_position (f->conversion.compose_region_start);

	  if (INT_SUBTRACT_WRAPV (wanted, position, &wanted)
	      || wanted < BEGV)
	    wanted = BEGV;

	  if (wanted > ZV)
	    wanted = ZV;

	  set_point (wanted);
	}
      else
	{
	  wanted
	    = marker_position (f->conversion.compose_region_end);

	  if (INT_ADD_WRAPV (wanted, position - 1, &wanted)
	      || wanted > ZV)
	    wanted = ZV;

	  if (wanted < BEGV)
	    wanted = BEGV;

	  set_point (wanted);
	}

      /* Make the composition region markers point elsewhere.  */

      if (!NILP (f->conversion.compose_region_start))
	{
	  Fset_marker (f->conversion.compose_region_start, Qnil, Qnil);
	  Fset_marker (f->conversion.compose_region_end, Qnil, Qnil);
	  f->conversion.compose_region_start = Qnil;
	  f->conversion.compose_region_end = Qnil;
	}

      /* Delete the composition region overlay.  */

      if (!NILP (f->conversion.compose_region_overlay))
	Fdelete_overlay (f->conversion.compose_region_overlay);
    }
  else
    {
      /* Otherwise, move the text and point to an appropriate
	 location.  */
      wanted = PT;
      Finsert (1, &text);

      if (position < 0)
	{
	  if (INT_SUBTRACT_WRAPV (wanted, position, &wanted)
	      || wanted < BEGV)
	    wanted = BEGV;

	  if (wanted > ZV)
	    wanted = ZV;

	  set_point (wanted);
	}
      else
	{
	  wanted = PT;

	  if (INT_ADD_WRAPV (wanted, position - 1, &wanted)
	      || wanted > ZV)
	    wanted = ZV;

	  if (wanted < BEGV)
	    wanted = BEGV;

	  set_point (wanted);
	}
    }

  unbind_to (count, Qnil);
}

/* Remove the composition region on the frame F, while leaving its
   contents intact.  */

static void
really_finish_composing_text (struct frame *f)
{
  if (!NILP (f->conversion.compose_region_start))
    {
      Fset_marker (f->conversion.compose_region_start, Qnil, Qnil);
      Fset_marker (f->conversion.compose_region_end, Qnil, Qnil);
      f->conversion.compose_region_start = Qnil;
      f->conversion.compose_region_end = Qnil;
    }

  /* Delete the composition region overlay.  */

  if (!NILP (f->conversion.compose_region_overlay))
    Fdelete_overlay (f->conversion.compose_region_overlay);
}

/* Set the composing text on F to TEXT.  Then, move point to an
   appropriate position relative to POSITION, and call
   `compose_region_changed' in the text conversion interface should
   point not have been changed relative to F's old selected window's
   last point.  */

static void
really_set_composing_text (struct frame *f, ptrdiff_t position,
			   Lisp_Object text)
{
  specpdl_ref count;
  ptrdiff_t start, wanted, end;
  struct window *w;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  w = XWINDOW (f->old_selected_window);
  Fselect_window (f->old_selected_window, Qt);

  /* Now set up the composition region if necessary.  */

  if (!MARKERP (f->conversion.compose_region_start))
    {
      f->conversion.compose_region_start = Fmake_marker ();
      f->conversion.compose_region_end = Fmake_marker ();
      Fset_marker (f->conversion.compose_region_start,
		   Fpoint (), Qnil);
      Fset_marker (f->conversion.compose_region_end,
		   Fpoint (), Qnil);
      Fset_marker_insertion_type (f->conversion.compose_region_end,
				  Qt);
    }
  else
    {
      /* Delete the text between the start of the composition region
	 and its end.  TODO: avoid this widening.  */
      record_unwind_protect (save_restriction_restore,
			     save_restriction_save ());
      Fwiden ();
      start = marker_position (f->conversion.compose_region_start);
      end = marker_position (f->conversion.compose_region_end);
      safe_del_range (start, end);
      set_point (start);
    }

  /* Insert the new text.  */
  Finsert (1, &text);

  /* Now move point to an appropriate location.  */
  if (position < 0)
    {
      wanted = start;

      if (INT_SUBTRACT_WRAPV (wanted, position, &wanted)
	  || wanted < BEGV)
	wanted = BEGV;

      if (wanted > ZV)
	wanted = ZV;
    }
  else
    {
      end = marker_position (f->conversion.compose_region_end);
      wanted = end;

      /* end should be PT after the edit.  */
      eassert (end == PT);

      if (INT_ADD_WRAPV (wanted, position - 1, &wanted)
	  || wanted > ZV)
	wanted = ZV;

      if (wanted < BEGV)
	wanted = BEGV;
    }

  set_point (wanted);

  /* If PT hasn't changed, the conversion region definitely has.
     Otherwise, redisplay will update the input method instead.  */

  if (PT == w->last_point
      && text_interface
      && text_interface->compose_region_changed)
    {
      if (f->conversion.batch_edit_count > 0)
	f->conversion.batch_edit_flags |= PENDING_COMPOSE_CHANGE;
      else
	text_interface->compose_region_changed (f);
    }

  unbind_to (count, Qnil);
}

/* Set the composing region to START by END.  Make it that it is not
   already set.  */

static void
really_set_composing_region (struct frame *f, ptrdiff_t start,
			     ptrdiff_t end)
{
  specpdl_ref count;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  /* If MAX (0, start) == end, then this should behave the same as
     really_finish_composing_text.  */

  if (max (0, start) == max (0, end))
    {
      really_finish_composing_text (f);
      return;
    }

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  Fselect_window (f->old_selected_window, Qt);

  /* Now set up the composition region if necessary.  */

  if (!MARKERP (f->conversion.compose_region_start))
    {
      f->conversion.compose_region_start = Fmake_marker ();
      f->conversion.compose_region_end = Fmake_marker ();
      Fset_marker_insertion_type (f->conversion.compose_region_end,
				  Qt);
    }

  Fset_marker (f->conversion.compose_region_start,
	       make_fixnum (start), Qnil);
  Fset_marker (f->conversion.compose_region_end,
	       make_fixnum (end), Qnil);

  unbind_to (count, Qnil);
}

/* Delete LEFT and RIGHT chars around point.  */

static void
really_delete_surrounding_text (struct frame *f, ptrdiff_t left,
				ptrdiff_t right)
{
  specpdl_ref count;
  ptrdiff_t start, end;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  Fselect_window (f->old_selected_window, Qt);

  start = max (BEGV, PT - left);
  end = min (ZV, PT + right);

  safe_del_range (start, end);
  unbind_to (count, Qnil);
}

/* Set point in F to POSITION.

   If it has not changed, signal an update through the text input
   interface, which is necessary for the IME to acknowledge that the
   change has completed.  */

static void
really_set_point (struct frame *f, ptrdiff_t point)
{
  specpdl_ref count;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  Fselect_window (f->old_selected_window, Qt);

  if (point == PT)
    {
      if (f->conversion.batch_edit_count > 0)
	f->conversion.batch_edit_flags |= PENDING_POINT_CHANGE;
      else
	text_interface->point_changed (f,
				       XWINDOW (f->old_selected_window),
				       current_buffer);
    }
  else
    /* Set the point.  */
    Fgoto_char (make_fixnum (point));

  unbind_to (count, Qnil);
}

/* Complete the edit specified by the counter value inside *TOKEN.  */

static void
complete_edit (void *token)
{
  if (text_interface && text_interface->notify_conversion)
    text_interface->notify_conversion (*(unsigned long *) token);
}

/* Process and free the text conversion ACTION.  F must be the frame
   on which ACTION will be performed.  */

static void
handle_pending_conversion_events_1 (struct frame *f,
				    struct text_conversion_action *action)
{
  Lisp_Object data;
  enum text_conversion_operation operation;
  struct buffer *buffer;
  struct window *w;
  specpdl_ref count;
  unsigned long token;

  /* Next, process this action and free it.  */

  data = action->data;
  operation = action->operation;
  token = action->counter;
  xfree (action);

  /* Make sure completion is signalled.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (complete_edit, &token);

  switch (operation)
    {
    case TEXTCONV_START_BATCH_EDIT:
      f->conversion.batch_edit_count++;
      break;

    case TEXTCONV_END_BATCH_EDIT:
      if (f->conversion.batch_edit_count > 0)
	f->conversion.batch_edit_count--;

      if (!WINDOW_LIVE_P (f->old_selected_window))
	break;

      if (f->conversion.batch_edit_flags & PENDING_POINT_CHANGE)
	{
	  w = XWINDOW (f->old_selected_window);
	  buffer = XBUFFER (WINDOW_BUFFER (w));

	  text_interface->point_changed (f, w, buffer);
	}

      if (f->conversion.batch_edit_flags & PENDING_COMPOSE_CHANGE)
	text_interface->compose_region_changed (f);

      f->conversion.batch_edit_flags = 0;
      break;

    case TEXTCONV_COMMIT_TEXT:
      really_commit_text (f, XFIXNUM (XCAR (data)), XCDR (data));
      break;

    case TEXTCONV_FINISH_COMPOSING_TEXT:
      really_finish_composing_text (f);
      break;

    case TEXTCONV_SET_COMPOSING_TEXT:
      really_set_composing_text (f, XFIXNUM (XCAR (data)),
				 XCDR (data));
      break;

    case TEXTCONV_SET_COMPOSING_REGION:
      really_set_composing_region (f, XFIXNUM (XCAR (data)),
				   XFIXNUM (XCDR (data)));
      break;

    case TEXTCONV_SET_POINT:
      really_set_point (f, XFIXNUM (data));
      break;

    case TEXTCONV_DELETE_SURROUNDING_TEXT:
      really_delete_surrounding_text (f, XFIXNUM (XCAR (data)),
				      XFIXNUM (XCDR (data)));
      break;
    }

  unbind_to (count, Qnil);
}

/* Process any outstanding text conversion events.
   This may run Lisp or signal.  */

void
handle_pending_conversion_events (void)
{
  struct frame *f;
  Lisp_Object tail, frame;
  struct text_conversion_action *action, *next;
  bool handled;

  handled = false;

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);

      /* Test if F has any outstanding conversion events.  Then
	 process them in bottom to up order.  */
      for (action = f->conversion.actions; action; action = next)
	{
	  /* Redisplay in between if there is more than one
	     action.  */

	  if (handled)
	    redisplay ();

	  /* Unlink this action.  */
	  next = action->next;
	  f->conversion.actions = next;

	  /* Handle and free the action.  */
	  handle_pending_conversion_events_1 (f, action);
	  handled = true;
	}
    }
}

/* Start a ``batch edit'' in F.  During a batch edit, point_changed
   will not be called until the batch edit ends.

   Process the actual operation in the event loop in keyboard.c; then,
   call `notify_conversion' in the text conversion interface with
   COUNTER.  */

void
start_batch_edit (struct frame *f, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_START_BATCH_EDIT;
  action->data = Qnil;
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* End a ``batch edit''.  It is ok to call this function even if a
   batch edit has not yet started, in which case it does nothing.

   COUNTER means the same as in `start_batch_edit'.  */

void
end_batch_edit (struct frame *f, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_END_BATCH_EDIT;
  action->data = Qnil;
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Insert the specified STRING into F's current buffer's composition
   region, and set point to POSITION relative to STRING.

   COUNTER means the same as in `start_batch_edit'.  */

void
commit_text (struct frame *f, Lisp_Object string,
	     ptrdiff_t position, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_COMMIT_TEXT;
  action->data = Fcons (make_fixnum (position), string);
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Remove the composition region and its overlay from F's current
   buffer.  Leave the text being composed intact.

   COUNTER means the same as in `start_batch_edit'.  */

void
finish_composing_text (struct frame *f, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_FINISH_COMPOSING_TEXT;
  action->data = Qnil;
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Insert the given STRING and make it the currently active
   composition.

   If there is currently no composing region, then the new value of
   point is used as the composing region.

   Then, the composing region is replaced with the text in the
   specified string.

   Finally, move point to new_point, which is relative to either the
   start or the end of OBJECT depending on whether or not it is less
   than zero.

   COUNTER means the same as in `start_batch_edit'.  */

void
set_composing_text (struct frame *f, Lisp_Object object,
		    ptrdiff_t new_point, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_SET_COMPOSING_TEXT;
  action->data = Fcons (make_fixnum (new_point),
			object);
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Make the region between START and END the currently active
   ``composing region''.

   The ``composing region'' is a region of text in the buffer that is
   about to undergo editing by the input method.  */

void
set_composing_region (struct frame *f, ptrdiff_t start,
		      ptrdiff_t end, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  if (start > MOST_POSITIVE_FIXNUM)
    start = MOST_POSITIVE_FIXNUM;

  if (end > MOST_POSITIVE_FIXNUM)
    end = MOST_POSITIVE_FIXNUM;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_SET_COMPOSING_REGION;
  action->data = Fcons (make_fixnum (start),
			make_fixnum (end));
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Move point in F's selected buffer to POINT.

   COUNTER means the same as in `start_batch_edit'.  */

void
textconv_set_point (struct frame *f, ptrdiff_t point,
		    unsigned long counter)
{
  struct text_conversion_action *action, **last;

  if (point > MOST_POSITIVE_FIXNUM)
    point = MOST_POSITIVE_FIXNUM;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_SET_POINT;
  action->data = make_fixnum (point);
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Delete LEFT and RIGHT characters around point in F's old selected
   window.  */

void
delete_surrounding_text (struct frame *f, ptrdiff_t left,
			 ptrdiff_t right, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_DELETE_SURROUNDING_TEXT;
  action->data = Fcons (make_fixnum (left),
			make_fixnum (right));
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Return N characters of text around point in F's old selected
   window.

   Set *N to the actual number of characters returned, *START_RETURN
   to the position of the first character returned, *OFFSET to the
   offset of point within that text, *LENGTH to the actual number of
   characters returned, and *BYTES to the actual number of bytes
   returned.

   Value is NULL upon failure, and a malloced string upon success.  */

char *
get_extracted_text (struct frame *f, ptrdiff_t n,
		    ptrdiff_t *start_return,
		    ptrdiff_t *offset, ptrdiff_t *length,
		    ptrdiff_t *bytes)
{
  specpdl_ref count;
  ptrdiff_t start, end, start_byte, end_byte;
  char *buffer;

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return NULL;

  /* Save the excursion, as there will be extensive changes to the
     selected window.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect_excursion ();

  /* Inhibit quitting.  */
  specbind (Qinhibit_quit, Qt);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  Fselect_window (f->old_selected_window, Qt);

  /* Figure out the bounds of the text to return.  */
  start = PT - n / 2;
  end = PT + n - n / 2;
  start = max (start, BEGV);
  end = min (end, ZV);
  buffer = NULL;

  /* Detect overflow.  */

  if (!(start <= PT <= end))
    goto finish;

  /* Convert the character positions to byte positions.  */
  start_byte = CHAR_TO_BYTE (start);
  end_byte = CHAR_TO_BYTE (end);

  /* Extract the text from the buffer.  */
  buffer = xmalloc (end_byte - start_byte);
  copy_buffer (start, start_byte, end, end_byte,
	       buffer);

  /* Return the offsets.  */
  *start_return = start;
  *offset = PT - start;
  *length = end - start;
  *bytes = end_byte - start_byte;

 finish:
  unbind_to (count, Qnil);
  return buffer;
}



/* Window system interface.  These are called from the rest of
   Emacs.  */

/* Notice that F's selected window has been set from redisplay.
   Reset F's input method state.  */

void
report_selected_window_change (struct frame *f)
{
  reset_frame_state (f);

  if (!text_interface)
    return;

  text_interface->reset (f);
}

/* Notice that the point in F's selected window's current buffer has
   changed.

   F is the frame whose selected window was changed, W is the window
   in question, and BUFFER is that window's current buffer.

   Tell the text conversion interface about the change; it will likely
   pass the information on to the system input method.  */

void
report_point_change (struct frame *f, struct window *window,
		     struct buffer *buffer)
{
  if (!text_interface || !text_interface->point_changed)
    return;

  if (f->conversion.batch_edit_count > 0)
    f->conversion.batch_edit_flags |= PENDING_POINT_CHANGE;
  else
    text_interface->point_changed (f, window, buffer);
}

/* Register INTERFACE as the text conversion interface.  */

void
register_textconv_interface (struct textconv_interface *interface)
{
  text_interface = interface;
}
