/* String conversion support for graphics terminals.

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

/* String conversion support.

   Many input methods require access to text surrounding the cursor.
   They may then request that the text editor remove or substitute
   that text for something else, for example when providing the
   ability to ``undo'' or ``edit'' previously composed text.  This is
   most commonly seen in input methods for CJK languages for X Windows,
   and is extensively used throughout Android by input methods for all
   kinds of scripts.

   In addition, these input methods may also need to make detailed
   edits to the content of a buffer.  That is also handled here.  */

#include <config.h>

#include "textconv.h"
#include "buffer.h"
#include "syntax.h"
#include "blockinput.h"
#include "keyboard.h"



/* Define debugging macros.  */

#if defined HAVE_ANDROID && !defined ANDROID_STUBIFY
#if 0
#include <android/log.h>

#define TEXTCONV_DEBUG(fmt, ...)					\
  __android_log_print (ANDROID_LOG_VERBOSE, "EmacsInputConnection",	\
		       "%s: " fmt, __func__, ## __VA_ARGS__)
#endif /* 0 */
#endif /* defined HAVE_ANDROID && !defined ANDROID_STUBIFY */

#ifndef TEXTCONV_DEBUG
#define TEXTCONV_DEBUG(...) ((void) 0)
#endif /* TEXTCONV_DEBUG */



/* The window system's text conversion interface.  NULL when the
   window system has not set up text conversion.  */

static struct textconv_interface *text_interface;

/* How many times text conversion has been disabled.  */

static int suppress_conversion_count;

/* Flags used to determine what must be sent after a batch edit
   ends.  */

enum textconv_batch_edit_flags
  {
    PENDING_POINT_CHANGE   = 1,
    PENDING_COMPOSE_CHANGE = 2,
  };



/* Copy the portion of the current buffer's text described by BEG,
   BEG_BYTE, END, END_BYTE to the char * buffer BUFFER, which should
   be at least END_BYTE - BEG_BYTEs long.  */

static void
copy_buffer_text (ptrdiff_t beg, ptrdiff_t beg_byte,
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
    memcpy (buffer + size, BEG_ADDR + beg1, end1 - beg1);
}



/* Conversion query.  */

/* Return the position of the active mark, or -1 if there is no mark
   or it is not active.  */

static ptrdiff_t
get_mark (void)
{
  if (!NILP (BVAR (current_buffer, mark_active))
      && XMARKER (BVAR (current_buffer, mark))->buffer)
    return marker_position (BVAR (current_buffer,
				  mark));

  return -1;
}

/* Like Fselect_window.  However, if WINDOW is a minibuffer window
   but not the active minibuffer window, select its frame's selected
   window instead.  */

static void
select_window (Lisp_Object window, Lisp_Object norecord)
{
  struct window *w;

  w = XWINDOW (window);

  /* Work around GCC bug 114893
     <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=114893>.  */
  eassume (w);

  if (MINI_WINDOW_P (w)
      && WINDOW_LIVE_P (window)
      && !EQ (window, Factive_minibuffer_window ()))
    window = WINDOW_XFRAME (w)->selected_window;

  Fselect_window (window, norecord);
}

/* Restore the selected window WINDOW.  */

static void
restore_selected_window (Lisp_Object window)
{
  /* FIXME: not sure what to do if WINDOW has been deleted.  */
  select_window (window, Qt);
}

/* Perform the text conversion operation specified in QUERY and return
   the results.

   Find the text between QUERY->position from point on frame F's
   selected window and QUERY->factor times QUERY->direction from that
   position.  Return it in QUERY->text.

   If QUERY->position is TYPE_MINIMUM (EMACS_INT) or EMACS_INT_MAX,
   start at the window's last point or mark, whichever is greater or
   smaller.

   Then, either delete that text from the buffer if QUERY->operation
   is TEXTCONV_SUBSTITUTION, or return 0.

   If FLAGS & TEXTCONV_SKIP_CONVERSION_REGION, then first move point
   past the conversion region in the specified direction if it is
   inside.

   Value is 0 if QUERY->operation was not TEXTCONV_SUBSTITUTION
   or if deleting the text was successful, and 1 otherwise.  */

int
textconv_query (struct frame *f, struct textconv_callback_struct *query,
		int flags)
{
  specpdl_ref count;
  ptrdiff_t pos, pos_byte, end, end_byte, start;
  ptrdiff_t temp, temp1, mark;
  char *buffer;
  struct window *w;

  /* Save the excursion, as there will be extensive changes to the
     selected window.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect_excursion ();
  record_unwind_protect (restore_selected_window, selected_window);

  /* Inhibit quitting.  */
  specbind (Qinhibit_quit, Qt);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  select_window ((WINDOW_LIVE_P (f->old_selected_window)
		  ? f->old_selected_window
		  : f->selected_window), Qt);
  w = XWINDOW (selected_window);

  /* Narrow to the field, if any.  */
  if (!NILP (f->conversion.field))
    {
      record_unwind_protect (save_restriction_restore,
			     save_restriction_save ());
      Fnarrow_to_region (XCAR (f->conversion.field),
			 XCAR (XCDR (f->conversion.field)));
    }

  /* Now find the appropriate text bounds for QUERY.  First, move
     point QUERY->position steps forward or backwards.  */

  pos = PT;

  /* If QUERY->position is EMACS_INT_MAX, use the last mark or the
     ephemeral last point, whichever is greater.

     The opposite applies for EMACS_INT_MIN.  */

  mark = get_mark ();

  if (query->position == EMACS_INT_MAX)
    {
      pos = (mark == -1
	     ? w->ephemeral_last_point
	     : max (w->ephemeral_last_point, mark));
      goto escape1;
    }
  else if (query->position == TYPE_MINIMUM (EMACS_INT))
    {
      pos = (mark == -1
	     ? w->ephemeral_last_point
	     : min (w->ephemeral_last_point, mark));
      goto escape1;
    }

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

  if (ckd_add (&pos, pos, query->position))
    pos = PT;

 escape1:

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
      if (ckd_add (&end, pos, query->factor) || end > ZV)
	end = ZV;

      end_byte = CHAR_TO_BYTE (end);
      break;

    case TEXTCONV_BACKWARD_CHAR:
      /* Move backward by query->factor characters.  */
      if (ckd_sub (&end, pos, query->factor) || end < BEGV)
	end = BEGV;

      end_byte = CHAR_TO_BYTE (end);
      break;

    case TEXTCONV_FORWARD_WORD:
      /* Move forward by query->factor words.  */
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
      /* Move backwards by query->factor words.  */
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
  copy_buffer_text (pos, pos_byte, end, end_byte, buffer);
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

/* Update the overlay displaying the conversion area on frame F after
   a change to the conversion region.  */

static void
sync_overlay (struct frame *f)
{
  if (MARKERP (f->conversion.compose_region_start)
      && !NILP (Vtext_conversion_face))
    {
      if (NILP (f->conversion.compose_region_overlay))
	{
	  f->conversion.compose_region_overlay
	    = Fmake_overlay (f->conversion.compose_region_start,
			     f->conversion.compose_region_end, Qnil,
			     Qt, Qnil);
	  Foverlay_put (f->conversion.compose_region_overlay,
			Qface, Vtext_conversion_face);
	}

      Fmove_overlay (f->conversion.compose_region_overlay,
		     f->conversion.compose_region_start,
		     f->conversion.compose_region_end, Qnil);
    }
  else if (!NILP (f->conversion.compose_region_overlay))
    {
      Fdelete_overlay (f->conversion.compose_region_overlay);
      f->conversion.compose_region_overlay = Qnil;
    }
}

/* Record a change to the current buffer as a result of an
   asynchronous text conversion operation.

   Consult the doc string of `text-conversion-edits' for the meaning
   of BEG, END, and EPHEMERAL.  */

static void
record_buffer_change (ptrdiff_t beg, ptrdiff_t end,
		      Lisp_Object ephemeral)
{
  Lisp_Object buffer, beg_marker, end_marker;

  XSETBUFFER (buffer, current_buffer);

  /* Make markers for both BEG and END.  */
  beg_marker = build_marker (current_buffer, beg,
			     CHAR_TO_BYTE (beg));

  /* If BEG and END are identical, make sure to keep the markers
     eq.  */

  if (beg == end)
    end_marker = beg_marker;
  else
    {
      end_marker = build_marker (current_buffer, end,
				 CHAR_TO_BYTE (end));

      /* Otherwise, make sure the marker extends past inserted
	 text.  */
      Fset_marker_insertion_type (end_marker, Qt);
    }

  Vtext_conversion_edits
    = Fcons (list4 (buffer, beg_marker, end_marker,
		    ephemeral),
	     Vtext_conversion_edits);
}

/* Reset text conversion state of frame F, and resume text conversion.
   Delete any overlays or markers inside.  */

void
reset_frame_conversion (struct frame *f)
{
  reset_frame_state (f);
  if (text_interface && FRAME_WINDOW_P (f) && FRAME_VISIBLE_P (f))
    text_interface->reset (f);
}

/* Reset text conversion state of frame F.  Delete any overlays or
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

  /* Clear active field.  */
  if (!NILP (f->conversion.field))
    {
      Fset_marker (XCAR (f->conversion.field), Qnil, Qnil);
      Fset_marker (XCAR (XCDR (f->conversion.field)), Qnil,
		   Qnil);
    }
  f->conversion.field = Qnil;
}

/* Return whether or not there are pending edits from an input method
   on any frame.  */

bool
detect_conversion_events (void)
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    {
      /* See if there's a pending edit on this frame.  */
      if (XFRAME (frame)->conversion.actions
	  && ((XFRAME (frame)->conversion.actions->operation
	       != TEXTCONV_BARRIER)
	      || (kbd_fetch_ptr == kbd_store_ptr)))
	return true;
    }

  return false;
}

/* Commit the given text in the composing region.  If there is no
   composing region, then insert the text after frame F's selected
   window's last point instead, unless the mark is active.  Finally,
   remove the composing region.

   If the mark is active, delete the text between mark and point.

   Then, move point to POSITION relative to TEXT.  If POSITION is
   greater than zero, it is relative to the character at the end of
   TEXT; otherwise, it is relative to the start of TEXT.  */

static void
really_commit_text (struct frame *f, EMACS_INT position,
		    Lisp_Object text)
{
  specpdl_ref count;
  ptrdiff_t wanted, start, end, mark;
  struct window *w;

  /* If F's old selected window is no longer alive, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  select_window (f->old_selected_window, Qt);

  /* Now detect whether or not there is a composing or active region.
     If there is, then replace it with TEXT.  Don't do that
     otherwise.  */

  mark = get_mark ();
  TEXTCONV_DEBUG ("the mark is: %zd", mark);
  if (MARKERP (f->conversion.compose_region_start) || mark != -1)
    {
      /* Replace its contents.  Set START and END to the start and end
	 of the composing region if it exists.  */

      if (MARKERP (f->conversion.compose_region_start))
	{
	  start = marker_position (f->conversion.compose_region_start);
	  end = marker_position (f->conversion.compose_region_end);
	}
      else
	{
	  /* Otherwise, set it to the start and end of the region.  */
	  start = min (mark, PT);
	  end = max (mark, PT);
	}

      TEXTCONV_DEBUG ("replacing text in composing region: %zd, %zd",
		      start, end);

      /* If it transpires that the start of the compose region is not
	 point, move point there.  */

      if (start != PT)
	set_point (start);

      /* Now delete whatever needs to go.  */

      del_range_1 (start, end, true, false);
      record_buffer_change (start, start, Qt);

      /* Don't record changes if TEXT is empty.  */

      if (SCHARS (text))
	{
	  /* Insert the new text.  Make sure to inherit text
	     properties from the surroundings: if this doesn't happen,
	     CC Mode fontification can get thrown off and become very
	     slow.  */

	  insert_from_string (text, 0, 0, SCHARS (text),
			      SBYTES (text), true);
	  record_buffer_change (start, PT, text);
	}

      /* Move to the position specified in POSITION.  */

      if (position <= 0)
	{
	  /* If POSITION is less than zero, it is relative to the
	     start of the text that was inserted.  */
	  wanted = start;

	  if (ckd_add (&wanted, wanted, position) || wanted < BEGV)
	    wanted = BEGV;

	  if (wanted > ZV)
	    wanted = ZV;

	  set_point (wanted);
	}
      else
	{
	  /* Otherwise, it is relative to the last character in
	     TEXT.  */
	  wanted = PT;

	  if (ckd_add (&wanted, wanted, position - 1) || wanted > ZV)
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

      /* Don't record changes if TEXT is empty.  */

      if (SCHARS (text))
	{
	  /* Insert the new text.  Make sure to inherit text
	     properties from the surroundings: if this doesn't happen,
	     CC Mode fontification can get thrown off and become very
	     slow.  */

	  insert_from_string (text, 0, 0, SCHARS (text),
			      SBYTES (text), true);

	  record_buffer_change (wanted, PT, text);
	}

      if (position <= 0)
	{
	  if (ckd_add (&wanted, wanted, position) || wanted < BEGV)
	    wanted = BEGV;

	  if (wanted > ZV)
	    wanted = ZV;

	  set_point (wanted);
	}
      else
	{
	  wanted = PT;

	  if (ckd_add (&wanted, wanted, position - 1) || wanted > ZV)
	    wanted = ZV;

	  if (wanted < BEGV)
	    wanted = BEGV;

	  set_point (wanted);
	}
    }

  /* This should deactivate the mark.  */
  call0 (Qdeactivate_mark);

  /* Print some debugging information.  */
  TEXTCONV_DEBUG ("text inserted: %s, point now: %zd",
		  SSDATA (text), PT);

  /* Update the ephemeral last point.  */
  w = XWINDOW (selected_window);
  w->ephemeral_last_point = PT;
  unbind_to (count, Qnil);
}

/* Remove the composition region on the frame F, while leaving its
   contents intact.  If UPDATE, also notify the input method of the
   change.  */

static void
really_finish_composing_text (struct frame *f, bool update)
{
  if (!NILP (f->conversion.compose_region_start))
    {
      Fset_marker (f->conversion.compose_region_start, Qnil, Qnil);
      Fset_marker (f->conversion.compose_region_end, Qnil, Qnil);
      f->conversion.compose_region_start = Qnil;
      f->conversion.compose_region_end = Qnil;

      if (update && text_interface
	  && text_interface->compose_region_changed)
	(*text_interface->compose_region_changed) (f);
    }

  /* Delete the composition region overlay.  */

  if (!NILP (f->conversion.compose_region_overlay))
    Fdelete_overlay (f->conversion.compose_region_overlay);

  TEXTCONV_DEBUG ("conversion region removed");
}

/* Set the composing text on frame F to TEXT.  Then, move point to an
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
  select_window (f->old_selected_window, Qt);

  /* Now set up the composition region if necessary.  */

  if (!MARKERP (f->conversion.compose_region_start))
    {
      /* Set START and END.  */
      start = PT;
      wanted = end = get_mark ();

      /* If END is -1, set it to start.  */

      if (end == -1)
	end = start;
      else
	{
	  /* Now sort start and end.  */
	  start = min (start, end);
	  end  = max (PT, wanted);
	}

      /* If END is not the same as start, delete the text in
	 between.  */

      if (end != start)
	{
	  del_range_1 (start, end, true, false);
	  set_point (start);
	  record_buffer_change (start, start, Qt);
	}

      /* Now set the markers which denote the composition region.  */
      f->conversion.compose_region_start
	= build_marker (current_buffer, PT, PT_BYTE);
      f->conversion.compose_region_end
	= build_marker (current_buffer, PT, PT_BYTE);

      Fset_marker_insertion_type (f->conversion.compose_region_end,
				  Qt);
    }
  else
    {
      /* Delete the text between the start of the composing region and
	 its end.  */
      start = marker_position (f->conversion.compose_region_start);
      end = marker_position (f->conversion.compose_region_end);
      del_range_1 (start, end, true, false);
      set_point (start);

      if (start != end)
	record_buffer_change (start, start, Qt);
    }

  /* Insert the new text.  Make sure to inherit text properties from
     the surroundings: if this doesn't happen, CC Mode fontification
     can get thrown off and become very slow.  */

  insert_from_string (text, 0, 0, SCHARS (text),
		      SBYTES (text), true);

  if (start != PT)
    record_buffer_change (start, PT, Qt);

  /* Now move point to an appropriate location.  */
  if (position <= 0)
    {
      wanted = start;

      if (ckd_sub (&wanted, wanted, position) || wanted < BEGV)
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

      if (ckd_add (&wanted, wanted, position - 1) || wanted > ZV)
	wanted = ZV;

      if (wanted < BEGV)
	wanted = BEGV;
    }

  set_point (wanted);

  /* This should deactivate the mark.  */
  call0 (Qdeactivate_mark);

  /* Move the composition overlay.  */
  sync_overlay (f);

  /* If TEXT is empty, remove the composing region.  This goes against
     the documentation, but is ultimately what programs expect.  */

  if (!SCHARS (text))
    really_finish_composing_text (f, false);

  /* If PT hasn't changed, the conversion region definitely has.
     Otherwise, redisplay will update the input method instead.  */

  if (PT == w->ephemeral_last_point
      && text_interface
      && text_interface->compose_region_changed)
    {
      if (f->conversion.batch_edit_count > 0)
	f->conversion.batch_edit_flags |= PENDING_COMPOSE_CHANGE;
      else
	text_interface->compose_region_changed (f);
    }

  /* Update the ephemeral last point.  */
  w = XWINDOW (selected_window);
  w->ephemeral_last_point = PT;

  if (SCHARS (text))
    TEXTCONV_DEBUG ("conversion region set to: %td %td",
		    marker_position (f->conversion.compose_region_start),
		    marker_position (f->conversion.compose_region_end));
  else
    TEXTCONV_DEBUG ("conversion region removed; PT is now: %td", PT);

  unbind_to (count, Qnil);
}

/* Set the composing region of frame F to START by END.  Make it if
   it is not already set.  */

static void
really_set_composing_region (struct frame *f, ptrdiff_t start,
			     ptrdiff_t end)
{
  specpdl_ref count;
  struct window *w;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  /* If MAX (0, start) == end, then this should behave the same as
     really_finish_composing_text.  */

  if (max (0, start) == max (0, end))
    {
      really_finish_composing_text (f, false);
      return;
    }

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  select_window (f->old_selected_window, Qt);

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
  sync_overlay (f);

  TEXTCONV_DEBUG ("composing region set to: %td, %td; point is: %td",
		  start, end, PT);

  /* Update the ephemeral last point.  */
  w = XWINDOW (selected_window);
  w->ephemeral_last_point = PT;

  unbind_to (count, Qnil);
}

/* Delete LEFT and RIGHT chars around point or the active mark,
   whichever is larger, in frame F's selected window, avoiding the
   composing region if necessary.  */

static void
really_delete_surrounding_text (struct frame *f, ptrdiff_t left,
				ptrdiff_t right)
{
  specpdl_ref count;
  ptrdiff_t start, end, a, b, a1, b1, lstart, rstart;
  struct window *w;
  Lisp_Object text;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  select_window (f->old_selected_window, Qt);

  /* Narrow to the field, if any.  */
  if (!NILP (f->conversion.field))
    {
      record_unwind_protect (save_restriction_restore,
			     save_restriction_save ());
      Fnarrow_to_region (XCAR (f->conversion.field),
			 XCAR (XCDR (f->conversion.field)));
    }

  /* Figure out where to start deleting from.  */

  a = get_mark ();

  if (a != -1 && a != PT)
    lstart = rstart = max (a, PT);
  else
    lstart = rstart = PT;

  /* Avoid the composing text.  This behavior is identical to how
     Android's BaseInputConnection actually implements avoiding the
     composing span.  */

  if (MARKERP (f->conversion.compose_region_start))
    {
      a = marker_position (f->conversion.compose_region_start);
      b = marker_position (f->conversion.compose_region_end);

      a1 = min (a, b);
      b1 = max (a, b);

      lstart = min (lstart, min (PT, a1));
      rstart = max (rstart, max (PT, b1));
    }

  if (lstart == rstart)
    {
      start = max (BEGV, lstart - left);
      end = min (ZV, rstart + right);

      text = del_range_1 (start, end, true, true);
      record_buffer_change (start, start, text);
    }
  else
    {
      /* Don't record a deletion if the text which was deleted lies
	 after point.  */

      start = rstart;
      end = min (ZV, rstart + right);
      text = del_range_1 (start, end, true, true);
      record_buffer_change (start, start, Qnil);

      /* Now delete what must be deleted on the left.  */

      start = max (BEGV, lstart - left);
      end = lstart;
      text = del_range_1 (start, end, true, true);
      record_buffer_change (start, start, text);
    }

  TEXTCONV_DEBUG ("deleted surrounding text: %td, %td; PT is now %td",
		  left, right, PT);

  /* if the mark is now equal to start, deactivate it.  */

  if (get_mark () == PT)
    call0 (Qdeactivate_mark);

  /* Update the ephemeral last point.  */
  w = XWINDOW (selected_window);
  w->ephemeral_last_point = PT;

  unbind_to (count, Qnil);
}

/* Save the confines of the field surrounding point in w into F's text
   conversion state.  If NOTIFY_COMPOSE, notify the input method of
   changes to the composition region if they arise in this process.  */

static void
locate_and_save_position_in_field (struct frame *f, struct window *w,
				   bool notify_compose)
{
  Lisp_Object pos, window, c1, c2;
  specpdl_ref count;
  ptrdiff_t beg, end, cstart, cend, newstart, newend;

  /* Set the current buffer to W's.  */
  count = SPECPDL_INDEX ();

  /* The current buffer must be saved, not merely the selected
     window.  */
  record_unwind_protect_excursion ();
  record_unwind_protect (restore_selected_window, selected_window);
  XSETWINDOW (window, w);
  select_window (window, Qt);

  /* Search for a field around the current editing position; this should
     also serve to confine text conversion to the visible region.  */
  XSETFASTINT (pos, min (max (w->ephemeral_last_point, BEGV), ZV));
  find_field (pos, Qnil, Qnil, &beg, Qnil, &end);

  /* If beg is 1 and end is ZV, disable the active field entirely.  */
  if (beg == 1 && end == ZV)
    {
      f->conversion.field = Qnil;
      goto exit;
    }

  /* Don't cons if a pair already exists.  */
  if (!NILP (f->conversion.field))
    {
      c1 = f->conversion.field;
      c2 = XCDR (c1);
      Fset_marker (XCAR (c1), make_fixed_natnum (beg), Qnil);
      Fset_marker (XCAR (c2), make_fixed_natnum (end), Qnil);
      XSETCDR (c2, window);
    }
  else
    {
      c1 = build_marker (current_buffer, beg, CHAR_TO_BYTE (beg));
      c2 = build_marker (current_buffer, end, CHAR_TO_BYTE (end));
      Fset_marker_insertion_type (c2, Qt);
      f->conversion.field = Fcons (c1, Fcons (c2, window));
    }

  /* If the composition region is active and oversteps the active field,
     restrict it to the same.  */

  if (!NILP (f->conversion.compose_region_start))
    {
      cstart = marker_position (f->conversion.compose_region_start);
      cend   = marker_position (f->conversion.compose_region_end);

      if (cend < beg || cstart > end)
	{
	  /* Remove the composition region in whole.  */
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

	  TEXTCONV_DEBUG ("removing composing region outside active field");
	}
      else
	{
	  newstart = max (beg, min (cstart, end));
	  newend   = max (beg, min (cend, end));

	  if (newstart != cstart || newend != cend)
	    {
	      TEXTCONV_DEBUG ("confined composing region to %td, %td",
			      newstart, newend);
	      Fset_marker (f->conversion.compose_region_start,
			   make_fixed_natnum (newstart), Qnil);
	      Fset_marker (f->conversion.compose_region_end,
			   make_fixed_natnum (newend), Qnil);
	    }
	  else
	    notify_compose = false;
	}
    }
  else
    notify_compose = false;

  if (notify_compose
      && text_interface->compose_region_changed)
    {
      if (f->conversion.batch_edit_count > 0)
	f->conversion.batch_edit_flags |= PENDING_COMPOSE_CHANGE;
      else
	text_interface->compose_region_changed (f);
    }

 exit:
  unbind_to (count, Qnil);
}

/* Update the interface with frame F's new point and mark.  If a batch
   edit is in progress, schedule the update for when it finishes
   instead.  */

static void
really_request_point_update (struct frame *f)
{
  struct window *w;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  if (f->conversion.batch_edit_count > 0)
    f->conversion.batch_edit_flags |= PENDING_POINT_CHANGE;
  else if (text_interface && text_interface->point_changed)
    {
      w = XWINDOW (f->old_selected_window);
      locate_and_save_position_in_field (f, w, false);
      text_interface->point_changed (f, w, current_buffer);
    }
}

/* Set point in frame F's selected window to POSITION.  If MARK is not
   at POSITION, activate the mark and set MARK to that as well.

   If point was not changed, signal an update through the text input
   interface, which is necessary for the IME to acknowledge that the
   change has completed.  */

static void
really_set_point_and_mark (struct frame *f, ptrdiff_t point,
			   ptrdiff_t mark)
{
  specpdl_ref count;
  struct window *w;

  /* If F's old selected window is no longer live, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  select_window (f->old_selected_window, Qt);

  if (point == PT)
    {
      if (f->conversion.batch_edit_count > 0)
	f->conversion.batch_edit_flags |= PENDING_POINT_CHANGE;
      else if (text_interface && text_interface->point_changed)
	{
	  w = XWINDOW (f->old_selected_window);
	  locate_and_save_position_in_field (f, w, false);
	  text_interface->point_changed (f, w, current_buffer);
	}
    }
  else
    /* Set the point.  */
    Fgoto_char (make_fixnum (point));

  if (mark == point
      && !NILP (BVAR (current_buffer, mark_active)))
    call0 (Qdeactivate_mark);
  else
    calln (Qpush_mark, make_fixnum (mark));

  /* Update the ephemeral last point.  */
  w = XWINDOW (selected_window);
  w->ephemeral_last_point = PT;

  TEXTCONV_DEBUG ("set point and mark: %td %td",
		  PT, get_mark ());

  unbind_to (count, Qnil);
}

/* Remove the composing region.  Replace the text between START and
   END in F's selected window with TEXT, then set point to POSITION
   relative to it.  If the mark is active, deactivate it.  */

static void
really_replace_text (struct frame *f, ptrdiff_t start, ptrdiff_t end,
		     Lisp_Object text, ptrdiff_t position)
{
  specpdl_ref count;
  ptrdiff_t new_start, new_end, wanted;
  struct window *w;

  /* If F's old selected window is no longer alive, fail.  */

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return;

  count = SPECPDL_INDEX ();
  record_unwind_protect (restore_selected_window,
			 selected_window);

  /* Make the composition region markers point elsewhere.  */

  if (!NILP (f->conversion.compose_region_start))
    {
      Fset_marker (f->conversion.compose_region_start, Qnil, Qnil);
      Fset_marker (f->conversion.compose_region_end, Qnil, Qnil);
      f->conversion.compose_region_start = Qnil;
      f->conversion.compose_region_end = Qnil;

      /* Notify the IME of an update to the composition region,
	 inasmuch as the point might not change if START and END are
	 identical and TEXT is empty, among other circumstances.  */

      if (text_interface
	  && text_interface->compose_region_changed)
	(*text_interface->compose_region_changed) (f);
    }

  /* Delete the composition region overlay.  */

  if (!NILP (f->conversion.compose_region_overlay))
    Fdelete_overlay (f->conversion.compose_region_overlay);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  select_window (f->old_selected_window, Qt);

  /* Sort START and END by magnitude.  */
  new_start = min (start, end);
  new_end   = max (start, end);

  /* Now constrain both to the accessible region.  */

  if (new_start < BEGV)
    new_start = BEGV;
  else if (new_start > ZV)
    new_start = ZV;

  if (new_end < BEGV)
    new_end = BEGV;
  else if (new_end > ZV)
    new_end = ZV;

  start = new_start;
  end   = new_end;

  /* This should deactivate the mark.  */
  call0 (Qdeactivate_mark);

  /* Go to start.  */
  set_point (start);

  /* Now delete the text in between, and save PT before TEXT is
     inserted.  */
  del_range_1 (start, end, true, false);
  record_buffer_change (start, start, Qt);
  wanted = PT;

  /* So long as TEXT isn't empty, insert it now.  */

  if (SCHARS (text))
    {
      /* Insert the new text.  Make sure to inherit text properties
	 from the surroundings: if this doesn't happen, CC Mode
	 fontification might grow confused and become very slow.  */

      insert_from_string (text, 0, 0, SCHARS (text),
			  SBYTES (text), true);
      record_buffer_change (start, PT, text);
    }

  /* Now, move point to the position designated by POSITION.  */

  if (position <= 0)
    {
      if (ckd_add (&wanted, wanted, position) || wanted < BEGV)
	wanted = BEGV;

      if (wanted > ZV)
	wanted = ZV;

      set_point (wanted);
    }
  else
    {
      wanted = PT;

      if (ckd_add (&wanted, wanted, position - 1) || wanted > ZV)
	wanted = ZV;

      if (wanted < BEGV)
	wanted = BEGV;

      set_point (wanted);
    }

  /* Print some debugging information.  */
  TEXTCONV_DEBUG ("text inserted: %s, point now: %zd",
		  SSDATA (text), PT);

  /* Update the ephemeral last point.  */
  w = XWINDOW (selected_window);
  w->ephemeral_last_point = PT;
  unbind_to (count, Qnil);
}

/* Complete the edit specified by the counter value inside *TOKEN.  */

static void
complete_edit (void *token)
{
  if (text_interface && text_interface->notify_conversion)
    text_interface->notify_conversion (*(unsigned long *) token);
}

/* Context for complete_edit_check.  */

struct complete_edit_check_context
{
  /* The window.  */
  struct window *w;

  /* Whether or not editing was successful.  */
  bool check;
};

/* Convert PTR to CONTEXT.  If CONTEXT->check is false, then update
   CONTEXT->w's ephemeral last point and give it to the input method,
   the assumption being that an editing operation signaled.  */

static void
complete_edit_check (void *ptr)
{
  struct complete_edit_check_context *context;
  struct frame *f;

  context = ptr;

  if (!context->check)
    {
      /* Figure out the new position of point.  */
      context->w->ephemeral_last_point
	= window_point (context->w);

      /* See if the frame is still alive.  */

      f = WINDOW_XFRAME (context->w);

      if (!FRAME_LIVE_P (f))
	return;

      if (text_interface && text_interface->point_changed)
	{
	  if (f->conversion.batch_edit_count > 0)
	    f->conversion.batch_edit_flags |= PENDING_POINT_CHANGE;
	  else
	    {
	      locate_and_save_position_in_field (f, context->w, false);
	      text_interface->point_changed (f, context->w, NULL);
	    }
	}
    }
}

/* Process and free the text conversion ACTION.  F must be the frame
   on which ACTION will be performed.

   Value is the window which was used, or NULL.  */

static struct window *
handle_pending_conversion_events_1 (struct frame *f,
				    struct text_conversion_action *action)
{
  Lisp_Object data;
  enum text_conversion_operation operation;
  struct buffer *buffer UNINIT;
  struct window *w;
  specpdl_ref count;
  unsigned long token;
  struct complete_edit_check_context context;

  /* Next, process this action and free it.  */

  data = action->data;
  operation = action->operation;
  token = action->counter;
  xfree (action);

  /* Text conversion events can still arrive immediately after
     `conversion_disabled_p' becomes true.  In that case, process all
     events, but don't perform any associated actions.  */

  if (conversion_disabled_p ())
    return NULL;

  /* check is a flag used by complete_edit_check to determine whether
     or not the editing operation completed successfully.  */
  context.check = false;

  /* Make sure completion is signaled.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (complete_edit, &token);
  w = NULL;

  if (WINDOW_LIVE_P (f->old_selected_window))
    {
      w = XWINDOW (f->old_selected_window);
      buffer = XBUFFER (WINDOW_BUFFER (w));
      context.w = w;

      /* Notify the input method of any editing failures.  */
      record_unwind_protect_ptr (complete_edit_check, &context);
    }

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
	  locate_and_save_position_in_field (f, w, false);
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
      really_finish_composing_text (f, !NILP (data));
      break;

    case TEXTCONV_SET_COMPOSING_TEXT:
      really_set_composing_text (f, XFIXNUM (XCAR (data)),
				 XCDR (data));
      break;

    case TEXTCONV_SET_COMPOSING_REGION:
      really_set_composing_region (f, XFIXNUM (XCAR (data)),
				   XFIXNUM (XCDR (data)));
      break;

    case TEXTCONV_SET_POINT_AND_MARK:
      really_set_point_and_mark (f, XFIXNUM (XCAR (data)),
				 XFIXNUM (XCDR (data)));
      break;

    case TEXTCONV_DELETE_SURROUNDING_TEXT:
      really_delete_surrounding_text (f, XFIXNUM (XCAR (data)),
				      XFIXNUM (XCDR (data)));
      break;

    case TEXTCONV_REQUEST_POINT_UPDATE:
      really_request_point_update (f);
      break;

    case TEXTCONV_BARRIER:
      if (kbd_fetch_ptr != kbd_store_ptr)
	emacs_abort ();

      /* Once a barrier is hit, synchronize F's selected window's
	 `ephemeral_last_point' with its current point.  The reason
	 for this is because otherwise a previous keyboard event may
	 have taken place without redisplay happening in between.  */

      if (w)
	w->ephemeral_last_point = window_point (w);
      break;

    case TEXTCONV_REPLACE_TEXT:
      really_replace_text (f, XFIXNUM (XCAR (data)),
			   XFIXNUM (XCAR (XCDR (data))),
			   XCAR (XCDR (XCDR (data))),
			   XFIXNUM (XCAR (XCDR (XCDR (XCDR (data))))));
      break;
    }

  /* Signal success.  */
  context.check = true;
  unbind_to (count, Qnil);

  return w;
}

/* Decrement the variable pointed to by *PTR.  */

static void
decrement_inside (void *ptr)
{
  int *i;

  i = ptr;
  (*i)--;
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
  static int inside;
  specpdl_ref count;
  ptrdiff_t last_point;
  struct window *w;

  handled = false;

  /* Reset Vtext_conversion_edits.  Do not do this if called
     reentrantly.  */

  if (!inside)
    Vtext_conversion_edits = Qnil;

  inside++;

  count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (decrement_inside, &inside);

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      last_point = -1;
      w = NULL;

      /* Test if F has any outstanding conversion events.  Then
	 process them in bottom to up order.  */
      while (true)
	{
	  /* Update the input method if handled &&
	     w->ephemeral_last_point != last_point.  */
	  if (w && (last_point != w->ephemeral_last_point))
	    {
	      if (handled
		  && last_point != -1
		  && text_interface
		  && text_interface->point_changed)
		{
		  if (f->conversion.batch_edit_count > 0)
		    f->conversion.batch_edit_flags |= PENDING_POINT_CHANGE;
		  else
		    {
		      locate_and_save_position_in_field (f, w, false);
		      text_interface->point_changed (f, NULL, NULL);
		    }
		}

	      last_point = w->ephemeral_last_point;
	    }

	  /* Reload action.  This needs to be reentrant as buffer
	     modification functions can call `read-char'.  */
	  action = f->conversion.actions;

	  /* If there are no more actions, break.  */

	  if (!action)
	    break;

	  /* If action is a barrier event and the keyboard buffer is
	     not yet empty, break out of the loop.  */

	  if (action->operation == TEXTCONV_BARRIER
	      && kbd_store_ptr != kbd_fetch_ptr)
	    break;

	  /* Unlink this action.  */
	  next = action->next;
	  f->conversion.actions = next;

	  /* Handle and free the action.  */
	  w = handle_pending_conversion_events_1 (f, action);
	  handled = true;
	}
    }

  unbind_to (count, Qnil);
}

#ifdef HAVE_ANDROID

/* Return the confines of the field to which editing operations on frame
   F should be constrained in *BEG and *END.  Should no field be active,
   set *END to PTRDIFF_MAX.  */

void
get_conversion_field (struct frame *f, ptrdiff_t *beg, ptrdiff_t *end)
{
  Lisp_Object c1, c2;
  struct window *w;

  if (!NILP (f->conversion.field))
    {
      c1 = f->conversion.field;
      c2 = XCDR (c1);

      if (!EQ (XCDR (c2), f->old_selected_window))
	{
	  /* Update this outdated field location.  */
	  w = XWINDOW (f->old_selected_window);
	  locate_and_save_position_in_field (f, w, true);
	  get_conversion_field (f, beg, end);
	  return;
	}

      *beg = marker_position (XCAR (c1));
      *end = marker_position (XCAR (c2));
      return;
    }

  *beg = 1;
  *end = PTRDIFF_MAX;
}

/* Start a ``batch edit'' in frame F.  During a batch edit,
   point_changed will not be called until the batch edit ends.

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

/* Insert the specified STRING into frame F's selected-window's
   buffer's composition region, and set point to POSITION relative to
   STRING.

   If there is no composition region, use the active region instead.
   If that doesn't exist either, insert STRING after point.

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

/* Remove the composition region and its overlay from frame F's
   selected-window's current buffer.  Leave the text being composed
   intact.

   If UPDATE, call `compose_region_changed' after the region is
   removed.

   COUNTER means the same as in `start_batch_edit'.  */

void
finish_composing_text (struct frame *f, unsigned long counter,
		       bool update)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_FINISH_COMPOSING_TEXT;
  action->data = update ? Qt : Qnil;
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Insert the given STRING and make it the currently active
   composition.

   If there is currently no composing or active region, then the new
   value of point is used as the composing region.

   Then, the composing or active region is replaced with the text in
   the specified string.

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
   ``composing region'' on frame F.  Which of START and END is the
   larger value is not significant.

   The ``composing region'' is a region of text in the buffer that is
   about to undergo editing by the input method.  */

void
set_composing_region (struct frame *f, ptrdiff_t start,
		      ptrdiff_t end, unsigned long counter)
{
  struct text_conversion_action *action, **last;
  ptrdiff_t field_start, field_end, temp;

  if (start > end)
    {
      temp  = end;
      end   = start;
      start = temp;
    }

  get_conversion_field (f, &field_start, &field_end);
  start = min (start + field_start - 1, MOST_POSITIVE_FIXNUM);
  end = max (start, min (end + field_start - 1, field_end));

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_SET_COMPOSING_REGION;
  action->data = Fcons (make_fixnum (start), make_fixnum (end));
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Move point in frame F's selected-window's buffer to POINT and maybe
   push MARK.

   COUNTER means the same as in `start_batch_edit'.  */

void
textconv_set_point_and_mark (struct frame *f, ptrdiff_t point,
			     ptrdiff_t mark, unsigned long counter)
{
  struct text_conversion_action *action, **last;
  ptrdiff_t field_start, field_end;

  get_conversion_field (f, &field_start, &field_end);
  point = min (max (point + field_start - 1, field_start),
	       field_end);
  mark = min (max (mark + field_start - 1, field_start),
	      field_end);

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_SET_POINT_AND_MARK;
  action->data = Fcons (make_fixnum (point),
			make_fixnum (mark));
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Delete LEFT and RIGHT characters around point in frame F's old
   selected window.  */

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

/* Request an immediate call to TEXT_INTERFACE->point_changed with the
   new details of frame F's region unless a batch edit is in
   progress.  */

void
request_point_update (struct frame *f, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_REQUEST_POINT_UPDATE;
  action->data = Qnil;
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Request that text conversion on frame F pause until the keyboard
   buffer becomes empty.

   Use this function to ensure that edits associated with a keyboard
   event complete before the text conversion edits after the barrier
   take place.  */

void
textconv_barrier (struct frame *f, unsigned long counter)
{
  struct text_conversion_action *action, **last;

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_BARRIER;
  action->data = Qnil;
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Remove the composing region.  Replace the text between START and END
   (whose order, as in `set_composing_region', is not significant)
   within F's selected window with TEXT; deactivate the mark if it is
   active.  Subsequently, set point to POSITION relative to TEXT, as
   `commit_text' would.  */

void
replace_text (struct frame *f, ptrdiff_t start, ptrdiff_t end,
	      Lisp_Object text, ptrdiff_t position,
	      unsigned long counter)
{
  struct text_conversion_action *action, **last;
  ptrdiff_t field_start, field_end, temp;

  if (start > end)
    {
      temp  = end;
      end   = start;
      start = temp;
    }

  get_conversion_field (f, &field_start, &field_end);
  start = min (start + field_start - 1, MOST_POSITIVE_FIXNUM);
  end = max (start, min (end + field_start - 1, field_end));

  action = xmalloc (sizeof *action);
  action->operation = TEXTCONV_REPLACE_TEXT;
  action->data = list4 (make_fixnum (start), make_fixnum (end),
			text, make_fixnum (position));
  action->next = NULL;
  action->counter = counter;
  for (last = &f->conversion.actions; *last; last = &(*last)->next)
    ;;
  *last = action;
  input_pending = true;
}

/* Return N characters of text around point in frame F's old selected
   window.

   If N is -1, return the text between point and mark instead, given
   that the mark is active.

   Set *START_RETURN to the position of the first character returned,
   *START_OFFSET to the offset of the lesser of mark and point within
   that text, *END_OFFSET to the greater of mark and point within that
   text, and *LENGTH to the actual number of characters returned,
   *BYTES to the actual number of bytes returned, and *MARK_ACTIVE to
   whether or not the mark is active.

   Value is NULL upon failure, and a malloced string upon success.  */

char *
get_extracted_text (struct frame *f, ptrdiff_t n,
		    ptrdiff_t *start_return,
		    ptrdiff_t *start_offset,
		    ptrdiff_t *end_offset, ptrdiff_t *length,
		    ptrdiff_t *bytes, bool *mark_active)
{
  specpdl_ref count;
  ptrdiff_t start, end, start_byte, end_byte, mark;
  char *buffer;
  ptrdiff_t field_start, field_end;

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return NULL;

  /* Save the excursion, as there will be extensive changes to the
     selected window.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect_excursion ();
  record_unwind_protect (restore_selected_window, selected_window);

  /* Inhibit quitting.  */
  specbind (Qinhibit_quit, Qt);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  select_window (f->old_selected_window, Qt);
  buffer = NULL;

  /* Figure out the bounds of the text to return.  */
  if (n != -1)
    {
      /* Make sure n is at least 4, leaving two characters around
	 PT.  */
      n = max (4, n);

      start = PT - n / 2;
      end = PT + n - n / 2;
    }
  else
    {
      if (!NILP (BVAR (current_buffer, mark_active))
	  && XMARKER (BVAR (current_buffer, mark))->buffer)
	{
	  start = marker_position (BVAR (current_buffer, mark));
	  end = PT;

	  /* Sort start and end.  start_byte is used to hold a
	     temporary value.  */

	  if (start > end)
	    {
	      start_byte = end;
	      end = start;
	      start = start_byte;
	    }
	}
      else
	goto finish;
    }

  /* Narrow to the field, if any.  */
  if (!NILP (f->conversion.field))
    {
      record_unwind_protect (save_restriction_restore,
			     save_restriction_save ());
      Fnarrow_to_region (XCAR (f->conversion.field),
			 XCAR (XCDR (f->conversion.field)));
    }

  start = max (start, BEGV);
  end = min (end, ZV);

  /* Detect overflow.  */

  if (!(start <= PT && PT <= end))
    goto finish;

  /* Convert the character positions to byte positions.  */
  start_byte = CHAR_TO_BYTE (start);
  end_byte = CHAR_TO_BYTE (end);

  /* Extract the text from the buffer.  */
  buffer = xmalloc (end_byte - start_byte);
  copy_buffer_text (start, start_byte, end, end_byte, buffer);

  /* Get the mark.  If it's not active, use PT.  */

  mark = get_mark ();
  *mark_active = true;

  if (mark == -1)
    {
      mark = PT;
      *mark_active = false;
    }

  /* Return the offsets.  */
  get_conversion_field (f, &field_start, &field_end);
  *start_return = max (1, start - field_start + 1);
  *start_offset = min (mark - start, PT - start);
  *end_offset = max (mark - start, PT - start);
  *length = end - start;
  *bytes = end_byte - start_byte;

  TEXTCONV_DEBUG ("get_extracted_text: PT, mark, start: %td, %td, %td",
		  PT, mark, start);

 finish:
  unbind_to (count, Qnil);
  return buffer;
}

/* Return the text between the positions pt - LEFT and pt + RIGHT,
   where pt is the position of point in frame F's selected window.  If
   the mark is active, return the range of text relative to the bounds
   of the region instead.

   Set *LENGTH to the number of characters returned, *BYTES to the
   number of bytes returned, *OFFSET to the character position of the
   returned text, and *START_RETURN and *END_RETURN to the mark and
   point relative to that position.  */

char *
get_surrounding_text (struct frame *f, ptrdiff_t left,
		      ptrdiff_t right, ptrdiff_t *length,
		      ptrdiff_t *bytes, ptrdiff_t *offset,
		      ptrdiff_t *start_return,
		      ptrdiff_t *end_return)
{
  specpdl_ref count;
  ptrdiff_t start, end, start_byte, end_byte, mark, temp;
  ptrdiff_t field_start, field_end;
  char *buffer;

  if (!WINDOW_LIVE_P (f->old_selected_window))
    return NULL;

  /* Save the excursion, as there will be extensive changes to the
     selected window.  */
  count = SPECPDL_INDEX ();
  record_unwind_protect_excursion ();
  record_unwind_protect (restore_selected_window, selected_window);

  /* Inhibit quitting.  */
  specbind (Qinhibit_quit, Qt);

  /* Temporarily switch to F's selected window at the time of the last
     redisplay.  */
  select_window (f->old_selected_window, Qt);
  buffer = NULL;

  /* Figure out the bounds of the text to return.  */

  /* First, obtain start and end.  */
  end = get_mark ();
  start = PT;

  /* If the mark is not active, make it start and end.  */

  if (end == -1)
    end = start;

  /* Now sort start and end.  */

  if (end < start)
    {
      temp = start;
      start = end;
      end = temp;
    }

  /* And subtract left and right.  */

  if (ckd_sub (&start, start, left)
      || ckd_add (&end, end, right))
    goto finish;

  /* Narrow to the field, if any.  */
  if (!NILP (f->conversion.field))
    {
      record_unwind_protect (save_restriction_restore,
			     save_restriction_save ());
      Fnarrow_to_region (XCAR (f->conversion.field),
			 XCAR (XCDR (f->conversion.field)));
    }

  start = max (start, BEGV);
  end = min (end, ZV);

  /* Detect overflow.  */

  if (!(start <= PT && PT <= end))
    goto finish;

  /* Convert the character positions to byte positions.  */
  start_byte = CHAR_TO_BYTE (start);
  end_byte = CHAR_TO_BYTE (end);

  /* Extract the text from the buffer.  */
  buffer = xmalloc (end_byte - start_byte);
  copy_buffer_text (start, start_byte, end, end_byte, buffer);

  /* Get the mark.  If it's not active, use PT.  */

  mark = get_mark ();

  if (mark == -1)
    mark = PT;

  /* Return the offsets.  Unlike `get_extracted_text', this need not
     sort mark and point.  */

  get_conversion_field (f, &field_start, &field_end);
  *offset = max (1, start - field_start + 1);
  *start_return = mark - start;
  *end_return = PT - start;
  *length = end - start;
  *bytes = end_byte - start_byte;

 finish:
  unbind_to (count, Qnil);
  return buffer;
}
#endif /* HAVE_ANDROID */

/* Return whether or not text conversion is temporarily disabled.
   `reset' should always call this to determine whether or not to
   disable the input method.  */

bool
conversion_disabled_p (void)
{
  return suppress_conversion_count > 0;
}



/* Window system interface.  These are called from the rest of
   Emacs.  */

/* Notice that frame F's selected window has been set from redisplay.
   Reset F's input method state.  */

void
report_selected_window_change (struct frame *f)
{
  struct window *w;

  reset_frame_state (f);

  if (!text_interface)
    return;

  /* When called from window.c, F's selected window has already been
     redisplayed, but w->last_point has not yet been updated.  Update
     it here to avoid race conditions when the IM asks for the initial
     selection position immediately after.  */

  if (WINDOWP (f->selected_window))
    {
      w = XWINDOW (f->selected_window);
      w->ephemeral_last_point = window_point (w);
    }

  text_interface->reset (f);
}

/* Notice that point in frame F's selected window's current buffer has
   changed.

   F is the frame whose selected window was changed, WINDOW is the
   window in question, and BUFFER is that window's buffer.

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
    {
      locate_and_save_position_in_field (f, window, false);
      text_interface->point_changed (f, window, buffer);
    }
}

/* Temporarily disable text conversion.  Must be paired with a
   corresponding call to resume_text_conversion.  */

void
disable_text_conversion (void)
{
  Lisp_Object tail, frame;
  struct frame *f;

  suppress_conversion_count++;

  if (!text_interface || suppress_conversion_count > 1)
    return;

  /* Loop through and reset the input method on each window system
     frame.  It should call conversion_disabled_p and then DTRT.  */

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      reset_frame_state (f);

      if (FRAME_WINDOW_P (f) && FRAME_VISIBLE_P (f))
	text_interface->reset (f);
    }
}

/* Undo the effect of the last call to `disable_text_conversion'.  */

void
resume_text_conversion (void)
{
  Lisp_Object tail, frame;
  struct frame *f;

  suppress_conversion_count--;
  eassert (suppress_conversion_count >= 0);

  if (!text_interface || suppress_conversion_count)
    return;

  /* Loop through and reset the input method on each window system
     frame.  It should call conversion_disabled_p and then DTRT.  */

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      reset_frame_state (f);

      if (FRAME_WINDOW_P (f) && FRAME_VISIBLE_P (f))
	text_interface->reset (f);
    }
}

/* Register INTERFACE as the text conversion interface.  */

void
register_textconv_interface (struct textconv_interface *interface)
{
  text_interface = interface;
}



/* List of buffers whose text conversion state will be reset after a
   key sequence is read.  */
static Lisp_Object postponed_buffers;

/* Reset the text conversion style of each frame whose selected buffer
   is contained inside `postponed_buffers'.  Set `postponed_buffers'
   to nil.  */

void
check_postponed_buffers (void)
{
  Lisp_Object buffer, tail, frame;
  struct buffer *b;
  struct frame *f;

  buffer = postponed_buffers;
  postponed_buffers = Qnil;

  if (!text_interface->reset)
    return;

  FOR_EACH_TAIL (buffer)
    {
      b = XBUFFER (XCAR (buffer));

      /* Continue if this is a dead buffer.  */

      if (!BUFFER_LIVE_P (b))
	continue;

      /* If no windows are displaying B anymore, continue.  */

      if (!buffer_window_count (b))
	continue;

      /* Look for frames which have B selected.  */

      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);

	  if (WINDOW_LIVE_P (f->old_selected_window)
	      && FRAME_WINDOW_P (f)
	      /* N.B. that the same frame can't be reset twice as long
		 as the list of buffers remains unique.  */
	      && EQ (XWINDOW (f->old_selected_window)->contents,
		     XCAR (buffer)))
	    {
	      block_input ();
	      reset_frame_state (f);
	      text_interface->reset (f);
	      unblock_input ();
	    }
	}
    }
}

/* Lisp interface.  */

DEFUN ("set-text-conversion-style", Fset_text_conversion_style,
       Sset_text_conversion_style, 1, 2, 0,
       doc: /* Set the current buffer's text conversion style to VALUE.

After setting `text-conversion-style', force input methods
editing in a selected window displaying this buffer on any frame
to stop themselves.

This can lead to a significant amount of time being taken by the input
method resetting itself, so you should not use this function lightly;
instead, set `text-conversion-style' before your buffer is displayed,
and let redisplay manage the input method appropriately.

If a key sequence is currently being read (either through the command
loop or by a call to `read-key-sequence') and AFTER-KEY-SEQUENCE is
non-nil, don't perform changes to the input method until the key
sequence is read.  This is useful within a function bound to
`input-decode-map' or `local-function-key-map', as it prevents the
input method from being redundantly enabled according to VALUE if the
replacement key sequence returned starts a new key sequence and makes
`read-key-sequence' disable text conversion again.  */)
  (Lisp_Object value, Lisp_Object after_key_sequence)
{
  Lisp_Object tail, frame;
  struct frame *f;
  Lisp_Object buffer;

  bset_text_conversion_style (current_buffer, value);

  if (!text_interface)
    return Qnil;

  /* If there are any selected windows displaying this buffer, reset
     text conversion on their associated frames.  */

  if (buffer_window_count (current_buffer))
    {
      buffer = Fcurrent_buffer ();

      /* Postpone changes to the actual text conversion state if
	 AFTER_KEY_SEQUENCE is non-nil and a key sequence is being
	 read.  */

      if (reading_key_sequence && !NILP (after_key_sequence))
	{
	  if (NILP (Fmemq (buffer, postponed_buffers)))
	    /* `check_postponed_buffers' will hopefully be called soon
	       enough to avoid postponed_buffers growing
	       indefinitely.  */
	    postponed_buffers = Fcons (buffer, postponed_buffers);
	  return Qnil;
	}

      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);

	  if (WINDOW_LIVE_P (f->old_selected_window)
	      && FRAME_WINDOW_P (f)
	      && (EQ (XWINDOW (f->old_selected_window)->contents,
		      buffer)
		  /* Always reset the text conversion style of the
		     selected frame.  */
		  || (f == SELECTED_FRAME ())))
	    {
	      block_input ();
	      reset_frame_state (f);
	      text_interface->reset (f);
	      unblock_input ();
	    }
	}
    }

  return Qnil;
}



void
syms_of_textconv (void)
{
  DEFSYM (Qaction, "action");
  DEFSYM (Qpassword, "password");
  DEFSYM (Qtext_conversion, "text-conversion");
  DEFSYM (Qpush_mark, "push-mark");
  DEFSYM (Qunderline, "underline");
  DEFSYM (Qoverriding_text_conversion_style,
	  "overriding-text-conversion-style");

  DEFVAR_LISP ("text-conversion-edits", Vtext_conversion_edits,
    doc: /* List of buffers last edited as a result of text conversion.

This list can be used while handling a `text-conversion' event to
determine which changes have taken place.

Each element of the list describes a single edit in a buffer, and
is of the form:

    (BUFFER BEG END EPHEMERAL)

If an insertion or an edit to the buffer text is described, then BEG
and END are markers which denote the bounds of the text that was
changed or inserted.  If a deletion is described, then BEG and END are
the same object.

If EPHEMERAL is t, then the input method is preparing to make further
edits to the text, so any actions that would otherwise be taken, such
as indenting or automatically filling text, should not take place.

Otherwise, it is either a string containing text that was inserted,
text deleted before point, or nil if text was deleted after point.

The list contents are arranged in the reverse of the order of editing,
i.e. latest edit first, so you must iterate through the list in
reverse.  */);
  Vtext_conversion_edits = Qnil;

  DEFVAR_LISP ("overriding-text-conversion-style",
	       Voverriding_text_conversion_style,
    doc: /* Non-buffer local version of `text-conversion-style'.

If this variable is the symbol `lambda', it means to consult the
buffer-local value of `text-conversion-style' to determine whether or
not to activate the input method.  Otherwise, the value is used in
preference to any buffer-local value of `text-conversion-style'.  */);
  Voverriding_text_conversion_style = Qlambda;

  DEFVAR_LISP ("text-conversion-face", Vtext_conversion_face,
    doc: /* Face in which to display temporary edits by an input method.
The value nil means to display no indication of a temporary edit.  */);
  Vtext_conversion_face = Qunderline;

  defsubr (&Sset_text_conversion_style);

  postponed_buffers = Qnil;
  staticpro (&postponed_buffers);
}
