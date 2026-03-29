/* Communication module for Android terminals.

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

#include <config.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <semaphore.h>

#include "lisp.h"
#include "androidterm.h"
#include "keyboard.h"
#include "blockinput.h"
#include "android.h"
#include "buffer.h"
#include "window.h"
#include "textconv.h"
#include "coding.h"
#include "pdumper.h"
#include "keymap.h"

/* This is a chain of structures for all the X displays currently in
   use.  */

struct android_display_info *x_display_list;



/* Android terminal interface functions.  */

#ifndef ANDROID_STUBIFY

#include <android/log.h>

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static bool any_help_event_p;

/* Counters for tallying up scroll wheel events if
   mwheel_coalesce_scroll_events is true.  */

static double wheel_event_x, wheel_event_y;

enum
  {
    ANDROID_EVENT_NORMAL,
    ANDROID_EVENT_GOTO_OUT,
    ANDROID_EVENT_DROP,
  };

/* Find the frame whose window has the identifier WDESC.

   This is like x_window_to_frame in xterm.c, except that DPYINFO may
   be NULL, as there is only at most one Android display, and is only
   specified in order to stay consistent with X.  */

static struct frame *
android_window_to_frame (struct android_display_info *dpyinfo,
			 android_window wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;

  if (wdesc == ANDROID_NONE)
    return NULL;

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);

      if (!FRAME_ANDROID_P (f))
	continue;

      if (FRAME_ANDROID_WINDOW (f) == wdesc)
        return f;
    }

  return NULL;
}

static void
android_clear_frame (struct frame *f)
{
  /* Clearing the frame will erase any cursor, so mark them all as no
     longer visible.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));
  android_clear_window (FRAME_ANDROID_DRAWABLE (f));
}

static void
android_show_hourglass (struct frame *f)
{
  struct android_output *x;

  /* This isn't implemented like X because a window brings alongside
     too many unneeded resources.  */

  x = FRAME_ANDROID_OUTPUT (f);

  /* If the hourglass window is mapped inside a popup menu, input
     could be lost if the menu is popped down and the grab is
     relinquished, but the hourglass window is still up.  Just
     avoid displaying the hourglass at all while popups are
     active.  */

  if (popup_activated ())
    return;

  x->hourglass = true;

  /* An hourglass cursor ought to be visible whether or not the standard
     cursor is invisible.  */
  android_define_cursor (FRAME_ANDROID_WINDOW (f),
			 x->hourglass_cursor);
}

static android_cursor
make_invisible_cursor (struct android_display_info *dpyinfo)
{
  return android_create_font_cursor (ANDROID_XC_NULL);
}

static void
android_hide_hourglass (struct frame *f)
{
  struct android_output *x;
  struct android_display_info *dpyinfo;

  x = FRAME_ANDROID_OUTPUT (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);
  x->hourglass = false;

  if (!f->pointer_invisible)
    android_define_cursor (FRAME_ANDROID_WINDOW (f),
			   x->current_cursor);
  else
    {
      if (!dpyinfo->invisible_cursor)
	dpyinfo->invisible_cursor = make_invisible_cursor (dpyinfo);

      android_define_cursor (FRAME_ANDROID_WINDOW (f),
			     dpyinfo->invisible_cursor);
    }
}

static void
android_flash (struct frame *f)
{
  struct android_gc *gc;
  struct android_gc_values values;
  int rc;
  fd_set fds;

  block_input ();
  values.function = ANDROID_GC_INVERT;
  gc = android_create_gc (ANDROID_GC_FUNCTION, &values);

  /* Get the height not including a menu bar widget.  */
  int height = FRAME_PIXEL_HEIGHT (f);
  /* Height of each line to flash.  */
  int flash_height = FRAME_LINE_HEIGHT (f);
  /* These will be the left and right margins of the rectangles.  */
  int flash_left = FRAME_INTERNAL_BORDER_WIDTH (f);
  int flash_right = FRAME_PIXEL_WIDTH (f) - FRAME_INTERNAL_BORDER_WIDTH (f);
  int width = flash_right - flash_left;

  /* If window is tall, flash top and bottom line.  */
  if (height > 3 * FRAME_LINE_HEIGHT (f))
    {
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc,
			      flash_left,
			      (FRAME_INTERNAL_BORDER_WIDTH (f)
			       + FRAME_TOP_MARGIN_HEIGHT (f)),
			      width, flash_height);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc,
			      flash_left,
			      (height - flash_height
			       - FRAME_INTERNAL_BORDER_WIDTH (f)
			       - FRAME_BOTTOM_MARGIN_HEIGHT (f)),
			      width, flash_height);

    }
  else
    /* If it is short, flash it all.  */
    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc,
			    flash_left, FRAME_INTERNAL_BORDER_WIDTH (f),
			    width, (height - 2
				    * FRAME_INTERNAL_BORDER_WIDTH (f)));

  flush_frame (f);

  struct timespec delay = make_timespec (0, 150 * 1000 * 1000);
  struct timespec wakeup = timespec_add (current_timespec (), delay);

  /* Keep waiting until past the time wakeup or any input gets
     available.  */
  while (! detect_input_pending ())
    {
      struct timespec current = current_timespec ();
      struct timespec timeout;

      /* Break if result would not be positive.  */
      if (timespec_cmp (wakeup, current) <= 0)
	break;

      /* How long `select' should wait.  */
      timeout = make_timespec (0, 10 * 1000 * 1000);

      /* Wait for some input to become available on the X
	 connection.  */
      FD_ZERO (&fds);

      /* Try to wait that long--but we might wake up sooner.  */
      rc = pselect (0, &fds, NULL, NULL, &timeout, NULL);

      /* Some input is available, exit the visible bell.  */
      if (rc >= 0)
	break;
    }

  /* If window is tall, flash top and bottom line.  */
  if (height > 3 * FRAME_LINE_HEIGHT (f))
    {
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc,
			      flash_left,
			      (FRAME_INTERNAL_BORDER_WIDTH (f)
			       + FRAME_TOP_MARGIN_HEIGHT (f)),
			      width, flash_height);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc,
			      flash_left,
			      (height - flash_height
			       - FRAME_INTERNAL_BORDER_WIDTH (f)
			       - FRAME_BOTTOM_MARGIN_HEIGHT (f)),
			      width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc,
			    flash_left, FRAME_INTERNAL_BORDER_WIDTH (f),
			    width, (height - 2
				    * FRAME_INTERNAL_BORDER_WIDTH (f)));

  android_free_gc (gc);
  flush_frame (f);

  unblock_input ();
}

static void
android_ring_bell (struct frame *f)
{
  if (visible_bell)
    android_flash (f);
  else
    {
      block_input ();
      android_bell ();
      unblock_input ();
    }
}

static void
android_toggle_visible_pointer (struct frame *f, bool invisible)
{
  struct android_display_info *dpyinfo;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  /* An hourglass cursor overrides invisibility.  */
  if (FRAME_ANDROID_OUTPUT (f)->hourglass)
    goto set_invisibility;

  if (!dpyinfo->invisible_cursor)
    dpyinfo->invisible_cursor = make_invisible_cursor (dpyinfo);

  if (invisible)
    android_define_cursor (FRAME_ANDROID_WINDOW (f),
			   dpyinfo->invisible_cursor);
  else
    android_define_cursor (FRAME_ANDROID_WINDOW (f),
			   f->output_data.android->current_cursor);

 set_invisibility:
  f->pointer_invisible = invisible;
}

static void
android_toggle_invisible_pointer (struct frame *f, bool invisible)
{
  block_input ();
  android_toggle_visible_pointer (f, invisible);
  unblock_input ();
}

/* Start an update of frame F.  This function is installed as a hook
   for update_begin, i.e. it is called when update_begin is called.
   This function is called prior to calls to gui_update_window_begin
   for each window being updated.  Currently, there is nothing to do
   here because all interesting stuff is done on a window basis.  */

static void
android_update_begin (struct frame *f)
{
  /* The frame is no longer complete, as it is in the midst of an
     update.  */
  FRAME_ANDROID_COMPLETE_P (f) = false;
}

/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
android_update_end (struct frame *f)
{
  /* Mouse highlight may be displayed again.  */
  MOUSE_HL_INFO (f)->mouse_face_defer = false;
}

static void
show_back_buffer (struct frame *f)
{
  struct android_swap_info swap_info;

  memset (&swap_info, 0, sizeof (swap_info));
  swap_info.swap_window = FRAME_ANDROID_WINDOW (f);
  swap_info.swap_action = ANDROID_COPIED;
  android_swap_buffers (&swap_info, 1);

  /* Now the back buffer no longer needs to be flipped.  */
  FRAME_ANDROID_NEED_BUFFER_FLIP (f) = false;
}

/* Flip back buffers on F if it has undrawn content.  */

static void
android_flush_dirty_back_buffer_on (struct frame *f)
{
  if (FRAME_GARBAGED_P (f)
      || buffer_flipping_blocked_p ()
      /* If the frame is not already up to date, do not flush buffers
	 on input, as that will result in flicker.  */
      || !FRAME_ANDROID_COMPLETE_P (f)
      || !FRAME_ANDROID_NEED_BUFFER_FLIP (f))
    return;

  show_back_buffer (f);
}

/* Convert between the modifier bits Android uses and the modifier
   bits Emacs uses.  */

static int
android_android_to_emacs_modifiers (struct android_display_info *dpyinfo,
				    int state)
{
  int mod_ctrl = ctrl_modifier;
  int mod_meta = meta_modifier;
  int mod_alt  = alt_modifier;
  int mod_super = super_modifier;
  Lisp_Object tem;

  tem = Fget (Vx_ctrl_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_ctrl = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_alt_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_alt = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_meta_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_meta = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_super_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_super = XFIXNUM (tem) & INT_MAX;

  return (((state & ANDROID_CONTROL_MASK) ? mod_ctrl		: 0)
	  | ((state & ANDROID_SHIFT_MASK) ? shift_modifier	: 0)
	  | ((state & ANDROID_ALT_MASK)   ? mod_meta		: 0)
	  | ((state & ANDROID_SUPER_MASK) ? mod_super		: 0)
	  | ((state & ANDROID_META_MASK)  ? mod_alt		: 0));
}

static int
android_emacs_to_android_modifiers (struct android_display_info *dpyinfo,
				    intmax_t state)
{
  EMACS_INT mod_ctrl  = ctrl_modifier;
  EMACS_INT mod_meta  = meta_modifier;
  EMACS_INT mod_alt   = alt_modifier;
  EMACS_INT mod_super = super_modifier;
  Lisp_Object tem;

  tem = Fget (Vx_ctrl_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_ctrl = XFIXNUM (tem);
  tem = Fget (Vx_alt_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_alt = XFIXNUM (tem);
  tem = Fget (Vx_meta_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_meta = XFIXNUM (tem);
  tem = Fget (Vx_super_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_super = XFIXNUM (tem);

  return (((state & mod_ctrl)		? ANDROID_CONTROL_MASK : 0)
	  | ((state & shift_modifier)	? ANDROID_SHIFT_MASK   : 0)
	  | ((state & mod_meta)		? ANDROID_ALT_MASK     : 0)
	  | ((state & mod_super)	? ANDROID_SUPER_MASK   : 0)
	  | ((state & mod_alt)		? ANDROID_META_MASK    : 0));
}

static void android_frame_rehighlight (struct android_display_info *);

static void
android_lower_frame (struct frame *f)
{
  android_lower_window (FRAME_ANDROID_WINDOW (f));
}

static void
android_raise_frame (struct frame *f)
{
  android_raise_window (FRAME_ANDROID_WINDOW (f));
}

static void
android_new_focus_frame (struct android_display_info *dpyinfo,
			 struct frame *frame)
{
  struct frame *old_focus;

  old_focus = dpyinfo->focus_frame;

  if (frame != dpyinfo->focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of x_focus_frame.  */
      dpyinfo->focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	android_lower_frame (old_focus);

      if (dpyinfo->focus_frame && dpyinfo->focus_frame->auto_raise)
	dpyinfo->pending_autoraise_frame = dpyinfo->focus_frame;
      else
	dpyinfo->pending_autoraise_frame = NULL;
    }

  android_frame_rehighlight (dpyinfo);
}

static void
android_focus_changed (int type, int state,
		       struct android_display_info *dpyinfo,
		       struct frame *frame, struct input_event *bufp)
{
  if (type == ANDROID_FOCUS_IN)
    {
      if (dpyinfo->x_focus_event_frame != frame)
        {
          android_new_focus_frame (dpyinfo, frame);
          dpyinfo->x_focus_event_frame = frame;
          bufp->kind = FOCUS_IN_EVENT;
          XSETFRAME (bufp->frame_or_window, frame);
        }

      frame->output_data.android->focus_state |= state;
    }
  else if (type == ANDROID_FOCUS_OUT)
    {
      frame->output_data.android->focus_state &= ~state;

      if (dpyinfo->x_focus_event_frame == frame)
        {
          dpyinfo->x_focus_event_frame = 0;
          android_new_focus_frame (dpyinfo, 0);

          bufp->kind = FOCUS_OUT_EVENT;
          XSETFRAME (bufp->frame_or_window, frame);
        }

      if (frame->pointer_invisible)
        android_toggle_invisible_pointer (frame, false);
    }
}

static void
android_detect_focus_change (struct android_display_info *dpyinfo,
			     struct frame *frame,
			     union android_event *event,
			     struct input_event *bufp)
{
  if (!frame)
    return;

  switch (event->type)
    {
    case ANDROID_FOCUS_IN:
    case ANDROID_FOCUS_OUT:
      android_focus_changed (event->type, FOCUS_EXPLICIT,
			     dpyinfo, frame, bufp);
      break;

    default:
      break;
    }
}

static bool
android_note_mouse_movement (struct frame *frame,
			     struct android_motion_event *event)
{
  struct android_display_info *dpyinfo;
  Emacs_Rectangle *r;

  if (!FRAME_ANDROID_OUTPUT (frame))
    return false;

  dpyinfo = FRAME_DISPLAY_INFO (frame);
  dpyinfo->last_mouse_motion_frame = frame;
  dpyinfo->last_mouse_motion_x = event->x;
  dpyinfo->last_mouse_motion_y = event->y;
  dpyinfo->last_mouse_movement_time = event->time;

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  r = &dpyinfo->last_mouse_glyph;
  if (frame != dpyinfo->last_mouse_glyph_frame
      || event->x < r->x || event->x >= r->x + (int) r->width
      || event->y < r->y || event->y >= r->y + (int) r->height)
    {
      frame->mouse_moved = true;
      note_mouse_highlight (frame, event->x, event->y);
      /* Remember which glyph we're now on.  */
      remember_mouse_glyph (frame, event->x, event->y, r);
      dpyinfo->last_mouse_glyph_frame = frame;
      return true;
    }

  return false;
}

static struct frame *
mouse_or_wdesc_frame (struct android_display_info *dpyinfo, int wdesc)
{
  struct frame *lm_f = (gui_mouse_grabbed (dpyinfo)
			? dpyinfo->last_mouse_frame
			: NULL);

  if (lm_f && !EQ (track_mouse, Qdropping)
      && !EQ (track_mouse, Qdrag_source))
    return lm_f;
  else
    {
      struct frame *w_f = android_window_to_frame (dpyinfo, wdesc);

      /* Do not return a tooltip frame.  */
      if (!w_f || FRAME_TOOLTIP_P (w_f))
	return EQ (track_mouse, Qdropping) ? lm_f : NULL;
      else
	/* When dropping it would be probably nice to raise w_f
	   here.  */
	return w_f;
    }
}

static Lisp_Object
android_construct_mouse_click (struct input_event *result,
			       struct android_button_event *event,
			       struct frame *f)
{
  struct android_display_info *dpyinfo;
  int x, y;

  dpyinfo = FRAME_DISPLAY_INFO (f);
  x = event->x;
  y = event->y;

  /* Make the event type NO_EVENT; we'll change that when we decide
     otherwise.  */
  result->kind = MOUSE_CLICK_EVENT;
  result->code = event->button - 1;
  result->timestamp = event->time;
  result->modifiers = (android_android_to_emacs_modifiers (dpyinfo,
							   event->state)
		       | (event->type == ANDROID_BUTTON_RELEASE
			  ? up_modifier : down_modifier));

  XSETINT (result->x, x);
  XSETINT (result->y, y);
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}

/* Generate a TOUCHSCREEN_UPDATE_EVENT for all pressed tools in FRAME.
   Return the event in IE.  Do not set IE->timestamp, as that is left
   to the caller.  */

static void
android_update_tools (struct frame *f, struct input_event *ie)
{
  struct android_touch_point *touchpoint;

  ie->kind = TOUCHSCREEN_UPDATE_EVENT;
  XSETFRAME (ie->frame_or_window, f);
  ie->arg = Qnil;

  /* Build the list of active touches.  */
  for (touchpoint = FRAME_OUTPUT_DATA (f)->touch_points;
       touchpoint; touchpoint = touchpoint->next)
    {
      /* Skip touch points which originated on the tool bar.  */

      if (touchpoint->tool_bar_p)
	continue;

      ie->arg = Fcons (list3i (touchpoint->x,
			       touchpoint->y,
			       touchpoint->tool_id),
		       ie->arg);
    }
}

/* Find and return an existing tool pressed against FRAME, identified
   by POINTER_ID.  Return NULL if no tool by that ID was found.  */

static struct android_touch_point *
android_find_tool (struct frame *f, int pointer_id)
{
  struct android_touch_point *touchpoint;

  for (touchpoint = FRAME_OUTPUT_DATA (f)->touch_points;
       touchpoint; touchpoint = touchpoint->next)
    {
      if (touchpoint->tool_id == pointer_id)
	return touchpoint;
    }

  return NULL;
}

/* Decode STRING, an array of N little endian UTF-16 characters, into
   a Lisp string.  Return Qnil if the string is too large, and the
   encoded string otherwise.  */

static Lisp_Object
android_decode_utf16 (unsigned short *utf16, size_t n)
{
  struct coding_system coding;
  ptrdiff_t size;

  if (ckd_mul (&size, n, sizeof *utf16))
    return Qnil;

  /* Set up the coding system.  Decoding a UTF-16 string (with no BOM)
     should not signal.  */

  memset (&coding, 0, sizeof coding);

  setup_coding_system (Qutf_16le, &coding);
  coding.source = (const unsigned char *) utf16;
  decode_coding_object (&coding, Qnil, 0, 0, size,
			size, Qt);

  return coding.dst_object;
}

/* Handle a cursor update request for F from the input method.
   MODE specifies whether or not an update should be sent immediately,
   and whether or not they are needed in the future.

   If MODE & ANDROID_CURSOR_UPDATE_IMMEDIATE, report the position of
   F's old selected window's phys cursor now.

   If MODE & ANDROID_CURSOR_UPDATE_MONITOR, set
   `need_cursor_updates'.  */

static void
android_request_cursor_updates (struct frame *f, int mode)
{
  struct window *w;

  if (mode & ANDROID_CURSOR_UPDATE_IMMEDIATE
      && WINDOWP (WINDOW_LIVE_P (f->old_selected_window)
		  ? f->old_selected_window
		  : f->selected_window))
    {
      /* Prefer the old selected window, as its selection is what was
	 reported to the IME previously.  */

      w = XWINDOW (WINDOW_LIVE_P (f->old_selected_window)
		   ? f->old_selected_window
		   : f->selected_window);
      android_set_preeditarea (w, w->cursor.x, w->cursor.y);
    }

  /* Now say whether or not updates are needed in the future.  */
  FRAME_OUTPUT_DATA (f)->need_cursor_updates
    = (mode & ANDROID_CURSOR_UPDATE_MONITOR);
}

/* Handle a single input method event EVENT, delivered to the frame
   F.

   Perform the text conversion action specified inside.  */

static void
android_handle_ime_event (union android_event *event, struct frame *f)
{
  Lisp_Object text UNINIT;
  struct android_output *output;

  /* First, decode the text if necessary.  */

  switch (event->ime.operation)
    {
    case ANDROID_IME_COMMIT_TEXT:
    case ANDROID_IME_SET_COMPOSING_TEXT:
    case ANDROID_IME_REPLACE_TEXT:
      text = android_decode_utf16 (event->ime.text,
				   event->ime.length);
      xfree (event->ime.text);

      /* Return should text be long enough that it overflows ptrdiff_t.
	 Such circumstances are detected within android_decode_utf16.  */

      if (NILP (text))
	return;

      break;

    default:
      break;
    }

  /* Finally, perform the appropriate conversion action.  */

  switch (event->ime.operation)
    {
    case ANDROID_IME_COMMIT_TEXT:
      commit_text (f, text, event->ime.position,
		   event->ime.counter);
      break;

    case ANDROID_IME_DELETE_SURROUNDING_TEXT:
      delete_surrounding_text (f, event->ime.start,
			       event->ime.end,
			       event->ime.counter);
      break;

    case ANDROID_IME_FINISH_COMPOSING_TEXT:

      if (event->ime.length == 2)
	{
	  output = FRAME_ANDROID_OUTPUT (f);

	  /* A new input method has connected to Emacs.  Stop
	     reporting changes that the previous input method has
	     asked to monitor.  */

	  output->extracted_text_flags = 0;
	  output->extracted_text_token = 0;
	  output->extracted_text_hint = 0;
	  output->need_cursor_updates = false;
	}

      finish_composing_text (f, event->ime.counter,
			     event->ime.length == 1);

      if (event->ime.length == 2)
	{
	  /* Now cancel outstanding batch edits if a new input method
	     has connected.  */

	  f->conversion.batch_edit_flags = 0;
	  f->conversion.batch_edit_count = 0;
	}

      break;

    case ANDROID_IME_SET_COMPOSING_TEXT:
      set_composing_text (f, text, event->ime.position,
			  event->ime.counter);
      break;

    case ANDROID_IME_SET_COMPOSING_REGION:
      set_composing_region (f, event->ime.start,
			    event->ime.end,
			    event->ime.counter);
      break;

    case ANDROID_IME_SET_POINT:
      textconv_set_point_and_mark (f, event->ime.start,
				   event->ime.end,
				   event->ime.counter);
      break;

    case ANDROID_IME_START_BATCH_EDIT:
      start_batch_edit (f, event->ime.counter);
      break;

    case ANDROID_IME_END_BATCH_EDIT:
      end_batch_edit (f, event->ime.counter);
      break;

    case ANDROID_IME_REQUEST_SELECTION_UPDATE:
      request_point_update (f, event->ime.counter);
      break;

    case ANDROID_IME_REQUEST_CURSOR_UPDATES:
      android_request_cursor_updates (f, event->ime.length);
      break;

    case ANDROID_IME_REPLACE_TEXT:
      replace_text (f, event->ime.start, event->ime.end,
		    text, event->ime.position,
		    event->ime.counter);
      break;
    }
}



/* Forward declaration.  */
static void android_notify_conversion (unsigned long);

static int
handle_one_android_event (struct android_display_info *dpyinfo,
			  union android_event *event, int *finish,
			  struct input_event *hold_quit)
{
  union android_event configureEvent;
  struct frame *f, *any, *mouse_frame;
  Mouse_HLInfo *hlinfo;
  union buffered_input_event inev;
  int modifiers, count, do_help;
  struct android_touch_point *touchpoint, **last;
  Lisp_Object window;
  int scroll_height;
  double scroll_unit;
  int keysym;
  ptrdiff_t nchars, i;
  struct window *w;
  static struct android_compose_status compose_status;

  /* It is okay for this to not resemble handle_one_xevent so much.
     Differences in event handling code are much less nasty than
     stuble differences in the graphics code.  */

  do_help = count = 0;
  hlinfo = &dpyinfo->mouse_highlight;
  *finish = ANDROID_EVENT_NORMAL;
  any = android_window_to_frame (dpyinfo, event->xany.window);
  nchars = 0;

  if (any && any->wait_event_type == event->type)
    any->wait_event_type = 0; /* Indicates we got it.  */

  EVENT_INIT (inev.ie);

  switch (event->type)
    {
    case ANDROID_CONFIGURE_NOTIFY:
      configureEvent = *event;

      f = android_window_to_frame (dpyinfo,
				   configureEvent.xconfigure.window);

      if (!f)
	goto OTHER;

      if (FRAME_TOOLTIP_P (f))
	{
	  if (FRAME_PIXEL_HEIGHT (f) != configureEvent.xconfigure.height
	      || FRAME_PIXEL_WIDTH (f) != configureEvent.xconfigure.width)
	    SET_FRAME_GARBAGED (f);

	  FRAME_PIXEL_HEIGHT (f) = configureEvent.xconfigure.height;
	  FRAME_PIXEL_WIDTH (f) = configureEvent.xconfigure.width;
	}

      int width = configureEvent.xconfigure.width;
      int height = configureEvent.xconfigure.height;

      if (CONSP (frame_size_history))
	frame_size_history_extra (f, build_string ("ConfigureNotify"),
				  FRAME_PIXEL_WIDTH (f),
				  FRAME_PIXEL_HEIGHT (f),
				  width, height, f->new_width,
				  f->new_height);

      /* Even if the number of character rows and columns has
	 not changed, the font size may have changed, so we need
	 to check the pixel dimensions as well.  */

      if (width != FRAME_PIXEL_WIDTH (f)
	  || height != FRAME_PIXEL_HEIGHT (f)
	  || (f->new_size_p
	      && ((f->new_width >= 0 && width != f->new_width)
		  || (f->new_height >= 0 && height != f->new_height))))
	{
	  change_frame_size (f, width, height, false, true, false);
	  android_clear_under_internal_border (f);
	  SET_FRAME_GARBAGED (f);
	  cancel_mouse_face (f);
	}

      /* Now change the left and top position of this window.  */

      {
	int old_left = f->left_pos;
	int old_top = f->top_pos;
	Lisp_Object frame;

	XSETFRAME (frame, f);

	{
	  android_window root;
	  unsigned int dummy_uint;

	  android_get_geometry (FRAME_ANDROID_WINDOW (f),
				&root, &f->left_pos, &f->top_pos,
				&dummy_uint, &dummy_uint,
				&dummy_uint);
	}

	if (!FRAME_TOOLTIP_P (f)
	    && (old_left != f->left_pos || old_top != f->top_pos))
	  {
	    inev.ie.kind = MOVE_FRAME_EVENT;
	    XSETFRAME (inev.ie.frame_or_window, f);
	  }

	if (f && FRAME_OUTPUT_DATA (f)->need_cursor_updates)
	  {
	    w = XWINDOW (f->selected_window);
	    android_set_preeditarea (w, w->cursor.x, w->cursor.y);
	  }
      }

      goto OTHER;

    case ANDROID_KEY_PRESS:

      /* Set f to any.  There are no ``outer windows'' on Android.  */
      f = any;

      /* If mouse-highlight is an integer, input clears out
	 mouse highlighting.  */
      if (!hlinfo->mouse_face_hidden && FIXNUMP (Vmouse_highlight)
	  && (any == NULL
	      || (!EQ (any->tool_bar_window, hlinfo->mouse_face_window)
		  && !EQ (any->tab_bar_window, hlinfo->mouse_face_window))))
        {
	  mouse_frame = hlinfo->mouse_face_mouse_frame;

	  clear_mouse_face (hlinfo);
	  hlinfo->mouse_face_hidden = true;

	  if (mouse_frame)
	    android_flush_dirty_back_buffer_on (mouse_frame);
	}

      if (!f)
	goto OTHER;

      if (event->xkey.counter)
	/* This event was generated by `performEditorAction'.  Make
	   sure it is processed before any subsequent edits.  */
	textconv_barrier (f, event->xkey.counter);

      wchar_t copy_buffer[512];
      wchar_t *copy_bufptr = copy_buffer;
      int copy_bufsiz = 512;

      event->xkey.state
	|= android_emacs_to_android_modifiers (dpyinfo,
					       extra_keyboard_modifiers);
      modifiers = event->xkey.state;

      /* In case Meta is ComposeCharacter, clear its status.  According
	 to Markus Ehrnsperger
	 Markus.Ehrnsperger@lehrstuhl-bross.physik.uni-muenchen.de this
	 enables ComposeCharacter to work whether or not it is combined
	 with Meta.  */
      if (modifiers & ANDROID_ALT_MASK)
	memset (&compose_status, 0, sizeof (compose_status));

      /* Common for all keysym input events.  */
      XSETFRAME (inev.ie.frame_or_window, any);
      inev.ie.modifiers
	= android_android_to_emacs_modifiers (dpyinfo, modifiers);
      inev.ie.timestamp = event->xkey.time;

      keysym = event->xkey.keycode;

      {
	enum android_lookup_status status_return;

	nchars = android_wc_lookup_string (&event->xkey, copy_bufptr,
					   copy_bufsiz, &keysym,
					   &status_return,
					   &compose_status);

	/* android_lookup_string can't be called twice, so there's no
	   way to recover from buffer overflow.  */
	if (status_return == ANDROID_BUFFER_OVERFLOW)
	  goto done_keysym;
	else if (status_return == ANDROID_LOOKUP_NONE)
	  {
	    /* Don't skip preedit text events.  */
	    if (event->xkey.keycode != (uint32_t) -1)
	      goto done_keysym;
	  }
	else if (status_return == ANDROID_LOOKUP_CHARS)
	  keysym = ANDROID_NO_SYMBOL;
	else if (status_return != ANDROID_LOOKUP_KEYSYM
		 && status_return != ANDROID_LOOKUP_BOTH)
	  emacs_abort ();

	/* Deal with pre-edit text events.  On Android, these are
	   simply encoded as events with associated strings and a
	   keycode set to ``-1''.  */

	if (event->xkey.keycode == (uint32_t) -1)
	  {
	    inev.ie.kind = PREEDIT_TEXT_EVENT;
	    inev.ie.arg = Qnil;

	    /* If text was looked up, decode it and make it the
	       preedit text.  */

	    if (status_return == ANDROID_LOOKUP_CHARS && nchars)
	      {
		copy_bufptr[nchars] = 0;
		inev.ie.arg = from_unicode_buffer (copy_bufptr);
	      }

	    goto done_keysym;
	  }
      }

      /* If a compose sequence is in progress, we break here.
	 Otherwise, chars_matched is always 0.  */
      if (compose_status.chars_matched > 0 && nchars == 0)
	break;

      memset (&compose_status, 0, sizeof (compose_status));

      if (nchars == 1 && copy_bufptr[0] >= 32)
	{
	  /* Deal with characters.  */

	  if (copy_bufptr[0] < 128)
	    inev.ie.kind = ASCII_KEYSTROKE_EVENT;
	  else
	    inev.ie.kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;

	  inev.ie.code = copy_bufptr[0];
	}
      else if (nchars < 2 && keysym)
	{
	  /* If the key is a modifier key, just return.  */
	  if (ANDROID_IS_MODIFIER_KEY (keysym))
	    goto done_keysym;

	  /* Next, deal with special ``characters'' by giving the
	     keycode to keyboard.c.  */
	  inev.ie.kind = NON_ASCII_KEYSTROKE_EVENT;
	  inev.ie.code = keysym;
	}
      else
	{
	  /* Finally, deal with strings.  */

	  for (i = 0; i < nchars; ++i)
	    {
	      inev.ie.kind = (SINGLE_BYTE_CHAR_P (copy_bufptr[i])
			      ? ASCII_KEYSTROKE_EVENT
			      : MULTIBYTE_CHAR_KEYSTROKE_EVENT);
	      inev.ie.code = copy_bufptr[i];

	      /* If the character is actually '\n', then change this
		 to RET.  */

	      if (copy_bufptr[i] == '\n')
		{
		  inev.ie.kind = NON_ASCII_KEYSTROKE_EVENT;
		  inev.ie.code = 66;
		}

	      kbd_buffer_store_buffered_event (&inev, hold_quit);
	    }

	  count += nchars;
	  inev.ie.kind = NO_EVENT;  /* Already stored above.  */
	}

      goto done_keysym;

    done_keysym:

      /* Now proceed to tell the input method the current position of
	 the cursor, if required.  */

      if (f && FRAME_OUTPUT_DATA (f)->need_cursor_updates)
	{
	  w = XWINDOW (f->selected_window);
	  android_set_preeditarea (w, w->cursor.x, w->cursor.y);
	}

      goto OTHER;

    case ANDROID_FOCUS_IN:
    case ANDROID_FOCUS_OUT:
      android_detect_focus_change (dpyinfo, any, event, &inev.ie);
      goto OTHER;

    case ANDROID_WINDOW_ACTION:

      /* This is a special event sent by android_run_in_emacs_thread
	 used to make Android run stuff.  */

      if (!event->xaction.window && !event->xaction.action)
	/* Don't run queries here, as it may run inside editor
	   commands, which can expose an inconsistent view of buffer
	   contents to the input method during command execution.

	   Instead, wait for Emacs to return to `android_select'.  */
	goto OTHER;

      f = any;

      if (event->xaction.action == 0)
	{
	  /* Action 0 either means that a window has been destroyed
	     and its associated frame should be as well.  */

	  if (event->xaction.window)
	    {
	      if (!f)
		goto OTHER;

	      inev.ie.kind = DELETE_WINDOW_EVENT;
	      XSETFRAME (inev.ie.frame_or_window, f);
	    }
	}

    case ANDROID_ENTER_NOTIFY:
      f = any;

      if (f)
	android_note_mouse_movement (f, &event->xmotion);
      goto OTHER;

    case ANDROID_MOTION_NOTIFY:

      previous_help_echo_string = help_echo_string;
      help_echo_string = Qnil;

      if (hlinfo->mouse_face_hidden)
	{
	  hlinfo->mouse_face_hidden = false;
	  clear_mouse_face (hlinfo);
	}

      f = any;

      if (f)
	{
	  /* Maybe generate a SELECT_WINDOW_EVENT for
	     `mouse-autoselect-window' but don't let popup menus
	     interfere with this (Bug#1261).  */
	  if (!NILP (Vmouse_autoselect_window)
	      && !popup_activated ()
	      /* Don't switch if we're currently in the minibuffer.
		 This tries to work around problems where the
		 minibuffer gets unselected unexpectedly, and where
		 you then have to move your mouse all the way down to
		 the minibuffer to select it.  */
	      && !MINI_WINDOW_P (XWINDOW (selected_window))
	      /* With `focus-follows-mouse' non-nil create an event
		 also when the target window is on another frame.  */
	      && (f == XFRAME (selected_frame)
		  || !NILP (focus_follows_mouse)))
	    {
	      Lisp_Object window
		= window_from_coordinates (f, event->xmotion.x,
					   event->xmotion.y, 0,
					   false, false, false);

	      /* A window will be autoselected only when it is not
		 selected now and the last mouse movement event was
		 not in it.  The remainder of the code is a bit vague
		 wrt what a "window" is.  For immediate autoselection,
		 the window is usually the entire window but for GTK
		 where the scroll bars don't count.  For delayed
		 autoselection the window is usually the window's text
		 area including the margins.  */
	      if (WINDOWP (window)
		  && !EQ (window, last_mouse_window)
		  && !EQ (window, selected_window))
		{
		  inev.ie.kind = SELECT_WINDOW_EVENT;
		  inev.ie.frame_or_window = window;
		}

	      /* Remember the last window where we saw the mouse.  */
	      last_mouse_window = window;
	    }

	  if (!android_note_mouse_movement (f, &event->xmotion))
	    help_echo_string = previous_help_echo_string;
	}

      /* If the contents of the global variable help_echo_string
	 has changed, generate a HELP_EVENT.  */
      if (!NILP (help_echo_string)
	  || !NILP (previous_help_echo_string))
	do_help = 1;

      if (f)
	android_flush_dirty_back_buffer_on (f);

      goto OTHER;

    case ANDROID_LEAVE_NOTIFY:
      f = any;

      if (f)
        {
	  /* Now clear dpyinfo->last_mouse_motion_frame, or
	     gui_redo_mouse_highlight will end up highlighting the
	     last known position of the mouse if a tooltip frame is
	     later unmapped.  */

	  if (f == dpyinfo->last_mouse_motion_frame)
	    dpyinfo->last_mouse_motion_frame = NULL;

	  /* Something similar applies to
	     dpyinfo->last_mouse_glyph_frame.  */
	  if (f == dpyinfo->last_mouse_glyph_frame)
	    dpyinfo->last_mouse_glyph_frame = NULL;

          if (f == hlinfo->mouse_face_mouse_frame)
            {
              /* If we move outside the frame, then we're
                 certainly no longer on any text in the frame.  */
              clear_mouse_face (hlinfo);
              hlinfo->mouse_face_mouse_frame = 0;
	      android_flush_dirty_back_buffer_on (f);
            }

          /* Generate a nil HELP_EVENT to cancel a help-echo.
             Do it only if there's something to cancel.
             Otherwise, the startup message is cleared when
             the mouse leaves the frame.  */
          if (any_help_event_p
	      /* But never if `mouse-drag-and-drop-region' is in
		 progress, since that results in the tooltip being
		 dismissed when the mouse moves on top.  */
	      && !((EQ (track_mouse, Qdrag_source)
		    || EQ (track_mouse, Qdropping))
		   && gui_mouse_grabbed (dpyinfo)))
	    do_help = -1;
        }

      goto OTHER;

    case ANDROID_EXPOSE:

      f = any;

      if (f)
        {
          if (!FRAME_VISIBLE_P (f))
            {
              f->output_data.android->has_been_visible = true;
              SET_FRAME_GARBAGED (f);
            }

          if (!FRAME_GARBAGED_P (f))
            {
              expose_frame (f, event->xexpose.x, event->xexpose.y,
			    event->xexpose.width, event->xexpose.height);

	      /* Do not display the back buffer if F is yet being
		 updated, as this might trigger premature bitmap
		 reconfiguration.  */
	      if (FRAME_ANDROID_COMPLETE_P (f))
		show_back_buffer (f);
	    }
        }

      goto OTHER;

    case ANDROID_BUTTON_PRESS:
    case ANDROID_BUTTON_RELEASE:
      /* If we decide we want to generate an event to be seen
	 by the rest of Emacs, we put it here.  */

      f = any;

      Lisp_Object tab_bar_arg = Qnil;
      bool tab_bar_p = false;
      bool tool_bar_p = false;

      dpyinfo->last_mouse_glyph_frame = NULL;

      f = mouse_or_wdesc_frame (dpyinfo, event->xbutton.window);

      if (f && event->xbutton.type == ANDROID_BUTTON_PRESS
	  && !popup_activated ()
	  /* && !x_window_to_scroll_bar (event->xbutton.display, */
	  /* 			      event->xbutton.window, 2) */
	  && !FRAME_NO_ACCEPT_FOCUS (f))
	{
	  /* When clicking into a child frame or when clicking
	     into a parent frame with the child frame selected and
	     `no-accept-focus' is not set, select the clicked
	     frame.  */
	  struct frame *hf = dpyinfo->highlight_frame;

	  if (FRAME_PARENT_FRAME (f) || (hf && frame_ancestor_p (f, hf)))
	    {
	      android_set_input_focus (FRAME_ANDROID_WINDOW (f),
				       event->xbutton.time);

	      if (FRAME_PARENT_FRAME (f))
		android_raise_window (FRAME_ANDROID_WINDOW (f));
	    }
	}

      if (f)
	{
	  /* Is this in the tab-bar?  */
	  if (WINDOWP (f->tab_bar_window)
	      && WINDOW_TOTAL_LINES (XWINDOW (f->tab_bar_window)))
	    {
	      Lisp_Object window;
	      int x = event->xbutton.x;
	      int y = event->xbutton.y;

	      window = window_from_coordinates (f, x, y, 0, true, true, true);
	      tab_bar_p = EQ (window, f->tab_bar_window);

	      if (tab_bar_p)
		{
		  tab_bar_arg = handle_tab_bar_click
		    (f, x, y, (event->xbutton.type
			       == ANDROID_BUTTON_PRESS),
		     android_android_to_emacs_modifiers (dpyinfo,
							 event->xbutton.state));
		  android_flush_dirty_back_buffer_on (f);
		}
	    }

	  /* Is this in the tool-bar?  */
	  if (WINDOWP (f->tool_bar_window)
	      && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
	    {
	      Lisp_Object window;
	      int x = event->xbutton.x;
	      int y = event->xbutton.y;

	      window = window_from_coordinates (f, x, y, 0, true, true, true);
	      tool_bar_p = (EQ (window, f->tool_bar_window)
			    && ((event->xbutton.type
				 != ANDROID_BUTTON_RELEASE)
				|| f->last_tool_bar_item != -1));

	      if (tool_bar_p && event->xbutton.button < 4)
		{
		  handle_tool_bar_click
		    (f, x, y, (event->xbutton.type
			       == ANDROID_BUTTON_PRESS),
		     android_android_to_emacs_modifiers (dpyinfo,
							 event->xbutton.state));
		  android_flush_dirty_back_buffer_on (f);
		}
	    }

	  if (!(tab_bar_p && NILP (tab_bar_arg)) && !tool_bar_p)
	    if (! popup_activated ())
	      {
		android_construct_mouse_click (&inev.ie, &event->xbutton, f);

		if (!NILP (tab_bar_arg))
		  inev.ie.arg = tab_bar_arg;
	      }
	}

      if (event->type == ANDROID_BUTTON_PRESS)
	{
	  dpyinfo->grabbed |= (1 << event->xbutton.button);
	  dpyinfo->last_mouse_frame = f;
	  if (f && !tab_bar_p)
	    f->last_tab_bar_item = -1;
	  if (f && !tool_bar_p)
	    f->last_tool_bar_item = -1;
	}
      else
	dpyinfo->grabbed &= ~(1 << event->xbutton.button);

      /* Ignore any mouse motion that happened before this event;
	 any subsequent mouse-movement Emacs events should reflect
	 only motion after the ButtonPress/Release.  */
      if (f != 0)
	f->mouse_moved = false;

      goto OTHER;

      /* Touch events.  The events here don't parallel X so much.  */
    case ANDROID_TOUCH_DOWN:

      if (!any)
	goto OTHER;

      /* This event is sent when a tool is put on the screen.  X and Y
	 are the location of the finger, and pointer_id identifies the
	 tool for as long as it is still held down.  First, see if the
	 touch point already exists and can be reused (this shouldn't
	 happen, but be safe.)  */

      touchpoint = android_find_tool (any, event->touch.pointer_id);

      if (touchpoint)
	{
	  /* Simply update the tool position and send an update.  */
	  touchpoint->x = event->touch.x;
	  touchpoint->y = event->touch.y;
	  android_update_tools (any, &inev.ie);
	  inev.ie.timestamp = event->touch.time;

	  goto OTHER;
	}

      /* Otherwise, link a new touchpoint onto the output's list of
	 pressed tools.  */

      touchpoint = xmalloc (sizeof *touchpoint);
      touchpoint->tool_id = event->touch.pointer_id;
      touchpoint->x = event->touch.x;
      touchpoint->y = event->touch.y;
      touchpoint->next = FRAME_OUTPUT_DATA (any)->touch_points;
      touchpoint->tool_bar_p = false;
      FRAME_OUTPUT_DATA (any)->touch_points = touchpoint;

      /* Figure out whether or not the tool was pressed on the tool
	 bar.  Note that the code which runs when it was is more or
	 less an abuse of the mouse highlight machinery, but it works
	 well enough in practice.  */

      if (WINDOWP (any->tool_bar_window)
	  && WINDOW_TOTAL_LINES (XWINDOW (any->tool_bar_window)))
	{
	  Lisp_Object window;
	  int x = event->touch.x;
	  int y = event->touch.y;

	  window = window_from_coordinates (any, x, y, 0, true,
					    true, true);

	  /* If this touch has started in the tool bar, do not
	     send it to Lisp.  Instead, simulate a tool bar
	     click, releasing it once it goes away.  */

	  if (EQ (window, any->tool_bar_window))
	    {
	      /* Call note_mouse_highlight on the tool bar
		 item.  Otherwise, get_tool_bar_item will
		 return 1.

		 This is not necessary when mouse-highlight is
		 nil.  */

	      if (!NILP (Vmouse_highlight))
		{
		  /* Clear the pointer invisible flag to always make
		     note_mouse_highlight do its thing.  */
		  any->pointer_invisible = false;
		  note_mouse_highlight (any, x, y);

		  /* Always allow future mouse motion to
		     update the mouse highlight, no matter
		     where it is.  */
		  memset (&dpyinfo->last_mouse_glyph, 0,
			  sizeof dpyinfo->last_mouse_glyph);
		  dpyinfo->last_mouse_glyph_frame = any;
		}

	      handle_tool_bar_click (any, x, y, true, 0);

	      /* Flush any changes made by that to the front
		 buffer.  */
	      android_flush_dirty_back_buffer_on (any);

	      /* Mark the touch point as being grabbed by the tool
		 bar.  */
	      touchpoint->tool_bar_p = true;
	      goto OTHER;
	    }
	}

      /* Now generate the Emacs event.  */
      inev.ie.kind = TOUCHSCREEN_BEGIN_EVENT;
      inev.ie.timestamp = event->touch.time;
      XSETFRAME (inev.ie.frame_or_window, any);
      XSETINT (inev.ie.x, event->touch.x);
      XSETINT (inev.ie.y, event->touch.y);
      XSETINT (inev.ie.arg, event->touch.pointer_id);

      goto OTHER;

    case ANDROID_TOUCH_MOVE:

      if (!any)
	goto OTHER;

      /* Look for the tool that moved.  */

      touchpoint = android_find_tool (any, event->touch.pointer_id);

      /* If it doesn't exist or has been grabbed by the tool bar, skip
	 processing this event.  */

      if (!touchpoint || touchpoint->tool_bar_p)
	goto OTHER;

      /* Otherwise, update the position and send the update event.  */

      touchpoint->x = event->touch.x;
      touchpoint->y = event->touch.y;
      android_update_tools (any, &inev.ie);
      inev.ie.timestamp = event->touch.time;

      goto OTHER;

    case ANDROID_TOUCH_UP:

      if (!any)
	goto OTHER;

      /* Now find and unlink the tool in question.  */

      last = &FRAME_OUTPUT_DATA (any)->touch_points;
      while ((touchpoint = *last))
	{
	  if (touchpoint->tool_id == event->touch.pointer_id)
	    {
	      *last = touchpoint->next;

	      if (touchpoint->tool_bar_p)
		{
		  xfree (touchpoint);

		  /* Do what is necessary to release the tool bar and
		     possibly trigger a click.  */

		  if (any->last_tool_bar_item != -1)
		    handle_tool_bar_click (any, event->touch.x,
					   event->touch.y, false,
					   0);

		  /* Cancel any outstanding mouse highlight.  */
		  note_mouse_highlight (any, -1, -1);
		  android_flush_dirty_back_buffer_on (any);

		  goto OTHER;
		}

	      /* The tool was unlinked.  Free it and generate the
		 appropriate Emacs event (assuming that it was not
		 grabbed by the tool bar).  */
	      xfree (touchpoint);

	      inev.ie.kind = TOUCHSCREEN_END_EVENT;
	      inev.ie.timestamp = event->touch.time;

	      /* Report whether the sequence has been canceled.  */

	      if (event->touch.flags & ANDROID_TOUCH_SEQUENCE_CANCELED)
		inev.ie.modifiers = 1;

	      XSETFRAME (inev.ie.frame_or_window, any);
	      XSETINT (inev.ie.x, event->touch.x);
	      XSETINT (inev.ie.y, event->touch.y);
	      XSETINT (inev.ie.arg, event->touch.pointer_id);

	      /* Break out of the loop.  */
	      goto OTHER;
	    }
	  else
	    last = &touchpoint->next;
	}

      /* No touch point was found.  This shouldn't happen.  */
      goto OTHER;

      /* Wheel motion.  The events here don't parallel X because
	 Android doesn't have scroll valuators.  */

    case ANDROID_WHEEL:

      if (!any)
	goto OTHER;

      if (fabs (event->wheel.x_delta) > 0
	  || fabs (event->wheel.y_delta) > 0)
	{
	  if (mwheel_coalesce_scroll_events)
	    {
	      if (signbit (event->wheel.x_delta)
		  != signbit (wheel_event_x))
		wheel_event_x = 0.0;

	      if (signbit (event->wheel.y_delta)
		  != signbit (wheel_event_y))
		wheel_event_y = 0.0;

	      /* Tally up deltas until one of them exceeds 1.0.  */
	      wheel_event_x += event->wheel.x_delta;
	      wheel_event_y += event->wheel.y_delta;

	      if (fabs (wheel_event_x) < 1.0
		  && fabs (wheel_event_y) < 1.0)
		goto OTHER;
	    }
	  else
	    {
	      /* Use the deltas in the event.  */
	      wheel_event_x = event->wheel.x_delta;
	      wheel_event_y = event->wheel.y_delta;
	    }

	  /* Determine what kind of event to send.  */
	  inev.ie.kind = ((fabs (wheel_event_y)
			   >= fabs (wheel_event_x))
			  ? WHEEL_EVENT : HORIZ_WHEEL_EVENT);
	  inev.ie.timestamp = event->wheel.time;

	  /* Set the event coordinates.  */
	  XSETINT (inev.ie.x, event->wheel.x);
	  XSETINT (inev.ie.y, event->wheel.y);

	  /* Set the frame.  */
	  XSETFRAME (inev.ie.frame_or_window, any);

	  /* Figure out the scroll direction.  */
	  inev.ie.modifiers = (signbit ((fabs (wheel_event_x)
					 >= fabs (wheel_event_y))
					? wheel_event_x
					: wheel_event_y)
			       ? down_modifier : up_modifier);

	  /* Figure out how much to scale the deltas by.  */
	  window = window_from_coordinates (any, event->wheel.x,
					    event->wheel.y, NULL,
					    false, false, false);

	  if (WINDOWP (window))
	    scroll_height = XWINDOW (window)->pixel_height;
	  else
	    /* EVENT_X and EVENT_Y can be outside the
	       frame if F holds the input grab, so fall
	       back to the height of the frame instead.  */
	    scroll_height = FRAME_PIXEL_HEIGHT (any);

	  scroll_unit = pow (scroll_height, 2.0 / 3.0);

	  /* Add the keyboard modifiers.  */
	  inev.ie.modifiers
	    |= android_android_to_emacs_modifiers (dpyinfo,
						   event->wheel.state);

	  /* Finally include the scroll deltas.  */
	  inev.ie.arg = list3 (Qnil,
			       make_float (wheel_event_x
					   * scroll_unit),
			       make_float (wheel_event_y
					   * scroll_unit));

	  wheel_event_x = 0.0;
	  wheel_event_y = 0.0;
	}

      goto OTHER;

      /* Iconification.  This is vastly simpler than on X.  */
    case ANDROID_ICONIFIED:

      if (!any)
	goto OTHER;

      if (FRAME_ICONIFIED_P (any))
	goto OTHER;

      SET_FRAME_VISIBLE (any, false);
      SET_FRAME_ICONIFIED (any, true);

      inev.ie.kind = ICONIFY_EVENT;
      XSETFRAME (inev.ie.frame_or_window, any);
      goto OTHER;

    case ANDROID_DEICONIFIED:

      if (!any)
	goto OTHER;

      if (!FRAME_ICONIFIED_P (any))
	goto OTHER;

      SET_FRAME_VISIBLE (any, true);
      SET_FRAME_ICONIFIED (any, false);

      inev.ie.kind = DEICONIFY_EVENT;
      XSETFRAME (inev.ie.frame_or_window, any);
      goto OTHER;

      /* Context menu handling.  */
    case ANDROID_CONTEXT_MENU:

      if (dpyinfo->menu_event_id == -1
	  /* Previously displayed popup menus might generate events
	     after dismissal, which might interfere.
	     `current_menu_serial' is always set to an identifier
	     identifying the last context menu to be displayed.  */
	  && event->menu.menu_event_serial == current_menu_serial)
	dpyinfo->menu_event_id = event->menu.menu_event_id;

      goto OTHER;

      /* Input method events.  textconv.c functions are called here to
	 queue events, which are then executed in a safe context
	 inside keyboard.c.  */
    case ANDROID_INPUT_METHOD:

      if (!any)
	{
	  /* Free any text allocated for this event.  */
	  xfree (event->ime.text);

	  /* If edits associated with this event haven't been
	     processed yet, signal their completion to avoid delays
	     the next time a call to `android_sync_edit' is made.

	     If events for a deleted frame are interleaved with events
	     for another frame, the edit counter may be prematurely
	     incremented before edits associated with the other frames
	     are processed.  This is not a problem in practice.  */

	  android_notify_conversion (event->ime.counter);
	}
      else
	android_handle_ime_event (event, any);

      goto OTHER;

    case ANDROID_DND_DRAG_EVENT:

      if (!any)
	goto OTHER;

      /* Generate a drag and drop event to convey its position.  */
      inev.ie.kind = DRAG_N_DROP_EVENT;
      XSETFRAME (inev.ie.frame_or_window, any);
      inev.ie.timestamp = ANDROID_CURRENT_TIME;
      XSETINT (inev.ie.x, event->dnd.x);
      XSETINT (inev.ie.y, event->dnd.y);
      inev.ie.arg = Fcons (inev.ie.x, inev.ie.y);
      goto OTHER;

    case ANDROID_DND_URI_EVENT:
    case ANDROID_DND_TEXT_EVENT:

      if (!any)
	{
	  free (event->dnd.uri_or_string);
	  goto OTHER;
	}

      /* An item was dropped over ANY, and is a file in the form of a
	 content or file URI or a string to be inserted.  Generate an
	 event with this information.  */

      inev.ie.kind = DRAG_N_DROP_EVENT;
      XSETFRAME (inev.ie.frame_or_window, any);
      inev.ie.timestamp = ANDROID_CURRENT_TIME;
      XSETINT (inev.ie.x, event->dnd.x);
      XSETINT (inev.ie.y, event->dnd.y);
      inev.ie.arg = Fcons ((event->type == ANDROID_DND_TEXT_EVENT
			    ? Qtext : Quri),
			   android_decode_utf16 (event->dnd.uri_or_string,
						 event->dnd.length));
      free (event->dnd.uri_or_string);
      goto OTHER;

    case ANDROID_NOTIFICATION_DELETED:
    case ANDROID_NOTIFICATION_ACTION:

      if (event->notification.type == ANDROID_NOTIFICATION_DELETED)
	android_notification_deleted (&event->notification, &inev.ie);
      else
	{
	  Lisp_Object action;

	  action = android_decode_utf16 (event->notification.action,
					 event->notification.length);
	  android_notification_action (&event->notification, &inev.ie,
				       action);
	}

      /* Free dynamically allocated data.  */
      free (event->notification.tag);
      free (event->notification.action);
      goto OTHER;

    case ANDROID_CONFIGURATION_CHANGED:

      if (event->config.detail == ANDROID_PIXEL_DENSITY_CHANGED)
	{
	  /* Update the display configuration from the event.  */
	  dpyinfo->resx = event->config.u.pixel_density.dpi_x;
	  dpyinfo->resy = event->config.u.pixel_density.dpi_y;
	  dpyinfo->font_resolution
	    = event->config.u.pixel_density.dpi_scaled;
#if notdef
	  __android_log_print (ANDROID_LOG_VERBOSE, __func__,
			       "New display configuration: "
			       "resx = %.2f resy = %.2f font_resolution = %.2f",
			       dpyinfo->resx, dpyinfo->resy, dpyinfo->font_resolution);
#endif /* notdef */
	  inev.ie.kind = CONFIG_CHANGED_EVENT;
	  inev.ie.frame_or_window = XCAR (dpyinfo->name_list_element);
	  inev.ie.arg = Qfont_render;
	}
      else if (event->config.detail == ANDROID_UI_MODE_CHANGED)
	{
	  android_ui_mode = event->config.u.ui_mode;
	  inev.ie.kind = TOOLKIT_THEME_CHANGED_EVENT;
	  inev.ie.arg = (android_ui_mode == UI_MODE_NIGHT_YES
			 ? Qdark : Qlight);
	}

      goto OTHER;

    default:
      goto OTHER;
    }

 OTHER:
  if (inev.ie.kind != NO_EVENT)
    {
      kbd_buffer_store_buffered_event (&inev, hold_quit);
      count++;
    }

  if (do_help
      && !(hold_quit && hold_quit->kind != NO_EVENT))
    {
      Lisp_Object frame;

      if (f)
	XSETFRAME (frame, f);
      else
	frame = Qnil;

      if (do_help > 0)
	{
	  any_help_event_p = true;
	  gen_help_event (help_echo_string, frame, help_echo_window,
			  help_echo_object, help_echo_pos);
	}
      else
	{
	  help_echo_string = Qnil;
	  gen_help_event (Qnil, frame, Qnil, Qnil, 0);
	}
      count++;
    }

  return count;
}

static int
android_read_socket (struct terminal *terminal,
		     struct input_event *hold_quit)
{
  int count = 0;
  struct android_display_info *dpyinfo;

  dpyinfo = terminal->display_info.android;

  block_input ();
  while (android_pending ())
    {
      int finish;
      union android_event event;

      android_next_event (&event);
      count += handle_one_android_event (dpyinfo, &event, &finish,
					 hold_quit);

      if (finish == ANDROID_EVENT_GOTO_OUT)
	break;
    }
  unblock_input ();

  /* If the focus was just given to an auto-raising frame, raise it
     now.  */
  if (dpyinfo->pending_autoraise_frame)
    {
      android_raise_frame (dpyinfo->pending_autoraise_frame);
      dpyinfo->pending_autoraise_frame = NULL;
    }

  return count;
}

static void
android_frame_up_to_date (struct frame *f)
{
  eassert (FRAME_ANDROID_P (f));
  block_input ();
  FRAME_MOUSE_UPDATE (f);

  if (!buffer_flipping_blocked_p ()
      && FRAME_ANDROID_NEED_BUFFER_FLIP (f))
    show_back_buffer (f);

  /* The frame is now complete, as its contents have been drawn.  */
  FRAME_ANDROID_COMPLETE_P (f) = true;

  /* Shrink the scanline buffer used by the font backend.  */
  sfntfont_android_shrink_scanline_buffer ();
  unblock_input ();
}

static void
android_buffer_flipping_unblocked_hook (struct frame *f)
{
  block_input ();

  if (FRAME_ANDROID_NEED_BUFFER_FLIP (f))
    show_back_buffer (f);

  unblock_input ();
}

static void
android_query_frame_background_color (struct frame *f, Emacs_Color *bgcolor)
{
  unsigned long background;

  background = FRAME_BACKGROUND_PIXEL (f);
  bgcolor->pixel = background;

  android_query_colors (f, bgcolor, 1);
}

int
android_parse_color (struct frame *f, const char *color_name,
		     Emacs_Color *color)
{
  unsigned short r, g, b;
  Lisp_Object tem, tem1;
  unsigned long lisp_color;

  if (parse_color_spec (color_name, &r, &g, &b))
    {
      color->red = r;
      color->green = g;
      color->blue = b;

      return 1;
    }

  tem = x_display_list->color_map;
  for (; CONSP (tem); tem = XCDR (tem))
    {
      tem1 = XCAR (tem);

      if (CONSP (tem1)
	  && !xstrcasecmp (SSDATA (XCAR (tem1)), color_name))
	{
	  lisp_color = XFIXNUM (XCDR (tem1));
	  color->red = RED_FROM_ULONG (lisp_color) * 257;
	  color->green = GREEN_FROM_ULONG (lisp_color) * 257;
	  color->blue = BLUE_FROM_ULONG (lisp_color) * 257;
	  return 1;
	}
    }

  return 0;
}

bool
android_alloc_nearest_color (struct frame *f, Emacs_Color *color)
{
  unsigned int ntsc;

  gamma_correct (f, color);

  if (FRAME_DISPLAY_INFO (f)->n_planes == 1)
    {
      /* Black and white.  I think this is the luminance formula applied
	 by the X server on generic monochrome framebuffers.  */
      color->pixel = ((((30l * color->red
			 + 59l * color->green
			 + 11l * color->blue) >> 8)
		       >= (((1 << 8) -1) * 50))
		      ? 0xffffff : 0);
    }
  else if (FRAME_DISPLAY_INFO (f)->n_planes <= 8)
    {
      /* 256 grays.  */
      ntsc = min (255, ((color->red * 0.299
			 + color->green * 0.587
			 + color->blue * 0.114)
			/ 256));
      color->pixel = RGB_TO_ULONG (ntsc, ntsc, ntsc);
    }
  else
    color->pixel = RGB_TO_ULONG (color->red / 256,
				 color->green / 256,
				 color->blue / 256);

  return true;
}

void
android_query_colors (struct frame *f, Emacs_Color *colors, int ncolors)
{
  int i;

  for (i = 0; i < ncolors; ++i)
    {
      colors[i].red = RED_FROM_ULONG (colors[i].pixel) * 257;
      colors[i].green = GREEN_FROM_ULONG (colors[i].pixel) * 257;
      colors[i].blue = BLUE_FROM_ULONG (colors[i].pixel) * 257;
    }
}

static void
android_mouse_position (struct frame **fp, int insist,
			Lisp_Object *bar_window,
			enum scroll_bar_part *part, Lisp_Object *x,
			Lisp_Object *y, Time *timestamp)
{
  Lisp_Object tail, frame;
  struct android_display_info *dpyinfo;

  dpyinfo = FRAME_DISPLAY_INFO (*fp);

  /* This is the best implementation possible on Android, where the
     system doesn't let Emacs obtain any information about the mouse
     pointer at all.  */

  if (dpyinfo->last_mouse_motion_frame)
    {
      *fp = dpyinfo->last_mouse_motion_frame;
      *timestamp = dpyinfo->last_mouse_movement_time;
      *x = make_fixnum (dpyinfo->last_mouse_motion_x);
      *y = make_fixnum (dpyinfo->last_mouse_motion_y);
      *bar_window = Qnil;
      *part = scroll_bar_nowhere;

      FOR_EACH_FRAME (tail, frame)
	{
	  if (FRAME_ANDROID_P (XFRAME (frame)))
	    XFRAME (frame)->mouse_moved = false;
	}

      dpyinfo->last_mouse_motion_frame->mouse_moved = false;
    }
}

static Lisp_Object
android_get_focus_frame (struct frame *f)
{
  Lisp_Object lisp_focus;
  struct frame *focus;

  focus = FRAME_DISPLAY_INFO (f)->focus_frame;

  if (!focus)
    return Qnil;

  XSETFRAME (lisp_focus, focus);
  return lisp_focus;
}

static void
android_focus_frame (struct frame *f, bool noactivate)
{
  /* Set the input focus to the frame's window.  The system only lets
     this work on child frames.  */
  android_set_input_focus (FRAME_ANDROID_WINDOW (f),
			   ANDROID_CURRENT_TIME);
}

/* The two procedures below only have to update the cursor on Android,
   as there are no window borders there.  */

static void
android_frame_highlight (struct frame *f)
{
  gui_update_cursor (f, true);
}

static void
android_frame_unhighlight (struct frame *f)
{
  gui_update_cursor (f, true);
}

static void
android_frame_rehighlight (struct android_display_info *dpyinfo)
{
  struct frame *old_highlight;

  old_highlight = dpyinfo->highlight_frame;

  if (dpyinfo->focus_frame)
    {
      dpyinfo->highlight_frame
	= ((FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->focus_frame))
	   : dpyinfo->focus_frame);
      if (!FRAME_LIVE_P (dpyinfo->highlight_frame))
	{
	  fset_focus_frame (dpyinfo->focus_frame, Qnil);
	  dpyinfo->highlight_frame = dpyinfo->focus_frame;
	}
    }
  else
    dpyinfo->highlight_frame = 0;

  if (dpyinfo->highlight_frame != old_highlight)
    {
      /* This is not yet required on Android.  */
      if (old_highlight)
	android_frame_unhighlight (old_highlight);
      if (dpyinfo->highlight_frame)
	android_frame_highlight (dpyinfo->highlight_frame);
    }
}

static void
android_frame_rehighlight_hook (struct frame *f)
{
  android_frame_rehighlight (FRAME_DISPLAY_INFO (f));
}

static void
android_frame_raise_lower (struct frame *f, bool raise_flag)
{
  if (raise_flag)
    android_raise_frame (f);
  else
    android_lower_frame (f);
}

void
android_make_frame_visible (struct frame *f)
{
  android_map_window (FRAME_ANDROID_WINDOW (f));

  SET_FRAME_VISIBLE (f, true);
  SET_FRAME_ICONIFIED (f, false);
}

void
android_make_frame_invisible (struct frame *f)
{
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_DISPLAY_INFO (f)->highlight_frame == f)
    FRAME_DISPLAY_INFO (f)->highlight_frame = 0;

  android_unmap_window (FRAME_ANDROID_WINDOW (f));

  SET_FRAME_VISIBLE (f, false);
  SET_FRAME_ICONIFIED (f, false);
}

static void
android_make_frame_visible_invisible (struct frame *f, bool visible)
{
  if (visible)
    android_make_frame_visible (f);
  else
    android_make_frame_invisible (f);
}

static void
android_fullscreen_hook (struct frame *f)
{
  Lisp_Object wanted;

  if (!FRAME_PARENT_FRAME (f))
    {
      /* Explicitly setting fullscreen is not supported on older
	 Android versions.  */

      wanted = (f->want_fullscreen == FULLSCREEN_BOTH
		? Qfullscreen : Qmaximized);

      if (android_set_fullscreen (FRAME_ANDROID_WINDOW (f),
				  EQ (wanted, Qfullscreen)))
	store_frame_param (f, Qfullscreen, Qmaximized);
      else
        store_frame_param (f, Qfullscreen, wanted);
    }
  else
    {
      store_frame_param (f, Qfullscreen, Qnil);

      /* If this is a child frame, don't keep it fullscreen
	 anymore.  */
      android_set_fullscreen (FRAME_ANDROID_WINDOW (f), false);
    }
}

void
android_iconify_frame (struct frame *f)
{
  /* This really doesn't work on Android.  */
  error ("Can't notify window manager of iconification");
}

static void
android_wait_for_event (struct frame *f, int eventtype)
{
  if (!FLOATP (Vandroid_wait_for_event_timeout))
    return;

  int level = interrupt_input_blocked;
  struct timespec tmo, tmo_at, time_now;

  f->wait_event_type = eventtype;

  /* Default timeout is 0.1 second.  Hopefully not noticeable.  */
  double timeout = XFLOAT_DATA (Vandroid_wait_for_event_timeout);
  time_t timeout_seconds = (time_t) timeout;
  tmo = make_timespec (timeout_seconds,
		       (long int) ((timeout - timeout_seconds)
				   * 1000 * 1000 * 1000));
  tmo_at = timespec_add (current_timespec (), tmo);

  while (f->wait_event_type)
    {
      pending_signals = true;
      totally_unblock_input ();
      /* XTread_socket is called after unblock.  */
      block_input ();
      interrupt_input_blocked = level;

      time_now = current_timespec ();
      if (timespec_cmp (tmo_at, time_now) < 0)
	break;

      tmo = timespec_sub (tmo_at, time_now);
      if (android_select (0, NULL, NULL, NULL, &tmo) == 0)
        break; /* Timeout */
    }

  f->wait_event_type = 0;
}

static void
android_set_window_size_1 (struct frame *f, bool change_gravity,
			   int width, int height)
{
  if (change_gravity)
    f->win_gravity = NorthWestGravity;

  android_resize_window (FRAME_ANDROID_WINDOW (f), width,
			 height);

  SET_FRAME_GARBAGED (f);

  if (FRAME_VISIBLE_P (f))
    {
      android_wait_for_event (f, ANDROID_CONFIGURE_NOTIFY);

      if (CONSP (frame_size_history))
	frame_size_history_extra (f, build_string ("set_window_size_1 visible"),
				  FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f),
				  width, height, f->new_width, f->new_height);
    }
  else
    {
      if (CONSP (frame_size_history))
	frame_size_history_extra (f, build_string ("set_window_size_1 "
						   "invisible"),
				  FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f),
				  width, height, f->new_width, f->new_height);

      adjust_frame_size (f, FRAME_PIXEL_TO_TEXT_WIDTH (f, width),
			 FRAME_PIXEL_TO_TEXT_HEIGHT (f, height),
			 5, 0, Qx_set_window_size_1);
    }
}

void
android_set_window_size (struct frame *f, bool change_gravity,
			 int width, int height)
{
  block_input ();

  android_set_window_size_1 (f, change_gravity, width, height);
  android_clear_under_internal_border (f);

  /* If cursor was outside the new size, mark it as off.  */
  mark_window_cursors_off (XWINDOW (f->root_window));

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size.
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  unblock_input ();

  do_pending_window_change (false);
}

static void
android_set_window_size_and_position_1 (struct frame *f, int width, int height)
{
  int x = f->left_pos;
  int y = f->top_pos;

  android_move_resize_window (FRAME_ANDROID_WINDOW (f), x, y, width, height);

  SET_FRAME_GARBAGED (f);

  if (FRAME_VISIBLE_P (f))
    android_wait_for_event (f, ANDROID_CONFIGURE_NOTIFY);
  else
    /* Call adjust_frame_size right.  It might be tempting to clear out
       f->new_width and f->new_height here.  */
    adjust_frame_size (f, FRAME_PIXEL_TO_TEXT_WIDTH (f, width),
		       FRAME_PIXEL_TO_TEXT_HEIGHT (f, height),
		       5, 0, Qx_set_window_size_1);
}

void
android_set_window_size_and_position (struct frame *f, int width, int height)
{
  block_input ();

  android_set_window_size_and_position_1 (f, width, height);
  android_clear_under_internal_border (f);

  /* If cursor was outside the new size, mark it as off.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size.
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  unblock_input ();

  do_pending_window_change (false);
}

/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

static void
android_calc_absolute_position (struct frame *f)
{
  int flags = f->size_hint_flags;
  struct frame *p = FRAME_PARENT_FRAME (f);

  /* We have nothing to do if the current position
     is already for the top-left corner.  */
  if (!((flags & XNegative) || (flags & YNegative)))
    return;

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    {
      int width = FRAME_PIXEL_WIDTH (f);

      /* A frame that has been visible at least once should have outer
	 edges.  */
      if (f->output_data.android->has_been_visible && !p)
	{
	  Lisp_Object frame;
	  Lisp_Object edges = Qnil;

	  XSETFRAME (frame, f);
	  edges = Fandroid_frame_edges (frame, Qouter_edges);
	  if (!NILP (edges))
	    width = (XFIXNUM (Fnth (make_fixnum (2), edges))
		     - XFIXNUM (Fnth (make_fixnum (0), edges)));
	}

      if (p)
	f->left_pos = (FRAME_PIXEL_WIDTH (p) - width - 2 * f->border_width
		       + f->left_pos);
      else
	/* Not that this is of much significance, for Android programs
	   cannot position their windows at absolute positions in the
	   screen.  */
	f->left_pos = (android_get_screen_width () - width + f->left_pos);

    }

  if (flags & YNegative)
    {
      int height = FRAME_PIXEL_HEIGHT (f);

      if (f->output_data.android->has_been_visible && !p)
	{
	  Lisp_Object frame;
	  Lisp_Object edges = Qnil;

	  XSETFRAME (frame, f);
	  if (NILP (edges))
	    edges = Fandroid_frame_edges (frame, Qouter_edges);
	  if (!NILP (edges))
	    height = (XFIXNUM (Fnth (make_fixnum (3), edges))
		      - XFIXNUM (Fnth (make_fixnum (1), edges)));
	}

      if (p)
	f->top_pos = (FRAME_PIXEL_HEIGHT (p) - height - 2 * f->border_width
		       + f->top_pos);
      else
	f->top_pos = (android_get_screen_height () - height + f->top_pos);
  }

  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->size_hint_flags &= ~(XNegative | YNegative);
}

static void
android_set_offset (struct frame *f, int xoff, int yoff,
		    int change_gravity)
{
  if (change_gravity > 0)
    {
      f->top_pos = yoff;
      f->left_pos = xoff;
      f->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->size_hint_flags |= YNegative;
      f->win_gravity = NorthWestGravity;
    }

  android_calc_absolute_position (f);
  android_move_window (FRAME_ANDROID_WINDOW (f), f->left_pos,
		       f->top_pos);
}

static void
android_set_alpha (struct frame *f)
{
  /* Not supported on Android.  */
}

static Lisp_Object
android_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  struct font *font = XFONT_OBJECT (font_object);
  int unit, font_ascent, font_descent;

  if (fontset < 0)
    fontset = fontset_from_font (font_object);
  FRAME_FONTSET (f) = fontset;
  if (FRAME_FONT (f) == font)
    /* This font is already set in frame F.  There's nothing more to
       do.  */
    return font_object;

  FRAME_FONT (f) = font;
  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;
  get_font_ascent_descent (font, &font_ascent, &font_descent);
  FRAME_LINE_HEIGHT (f) = font_ascent + font_descent;

  /* We could use a more elaborate calculation here.  */
  FRAME_TAB_BAR_HEIGHT (f) = FRAME_TAB_BAR_LINES (f) * FRAME_LINE_HEIGHT (f);

  /* Compute character columns occupied by scrollbar.

     Don't do things differently for non-toolkit scrollbars
     (Bug#17163).  */
  unit = FRAME_COLUMN_WIDTH (f);
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    FRAME_CONFIG_SCROLL_BAR_COLS (f)
      = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + unit - 1) / unit;
  else
    FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + unit - 1) / unit;


  /* Don't change the size of a tip frame; there's no point in doing it
     because it's done in Fx_show_tip, and it leads to problems because
     the tip frame has no widget.  */
  if (FRAME_ANDROID_WINDOW (f) != 0 && !FRAME_TOOLTIP_P (f))
    adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		       FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 3,
		       false, Qfont);

  return font_object;
}

static bool
android_bitmap_icon (struct frame *f, Lisp_Object file)
{
  return false;
}

static void
android_free_pixmap_hook (struct frame *f, Emacs_Pixmap pixmap)
{
  android_free_pixmap (pixmap);
}

void
android_free_frame_resources (struct frame *f)
{
  struct android_display_info *dpyinfo;
  Mouse_HLInfo *hlinfo;
  struct android_touch_point *last, *next;

  dpyinfo = FRAME_DISPLAY_INFO (f);
  hlinfo = &dpyinfo->mouse_highlight;

  block_input ();
  free_frame_faces (f);

  /* FRAME_ANDROID_WINDOW can be 0 if frame creation failed.  */
  if (FRAME_ANDROID_WINDOW (f))
    android_destroy_window (FRAME_ANDROID_WINDOW (f));

  android_free_gcs (f);

  /* Free cursors.  */
  if (f->output_data.android->text_cursor)
    android_free_cursor (f->output_data.android->text_cursor);
  if (f->output_data.android->nontext_cursor)
    android_free_cursor (f->output_data.android->nontext_cursor);
  if (f->output_data.android->modeline_cursor)
    android_free_cursor (f->output_data.android->modeline_cursor);
  if (f->output_data.android->hand_cursor)
    android_free_cursor (f->output_data.android->hand_cursor);
  if (f->output_data.android->hourglass_cursor)
    android_free_cursor (f->output_data.android->hourglass_cursor);
  if (f->output_data.android->horizontal_drag_cursor)
    android_free_cursor (f->output_data.android->horizontal_drag_cursor);
  if (f->output_data.android->vertical_drag_cursor)
    android_free_cursor (f->output_data.android->vertical_drag_cursor);
  if (f->output_data.android->left_edge_cursor)
    android_free_cursor (f->output_data.android->left_edge_cursor);
  if (f->output_data.android->top_left_corner_cursor)
    android_free_cursor (f->output_data.android->top_left_corner_cursor);
  if (f->output_data.android->top_edge_cursor)
    android_free_cursor (f->output_data.android->top_edge_cursor);
  if (f->output_data.android->top_right_corner_cursor)
    android_free_cursor (f->output_data.android->top_right_corner_cursor);
  if (f->output_data.android->right_edge_cursor)
    android_free_cursor (f->output_data.android->right_edge_cursor);
  if (f->output_data.android->bottom_right_corner_cursor)
    android_free_cursor (f->output_data.android->bottom_right_corner_cursor);
  if (f->output_data.android->bottom_edge_cursor)
    android_free_cursor (f->output_data.android->bottom_edge_cursor);
  if (f->output_data.android->bottom_left_corner_cursor)
    android_free_cursor (f->output_data.android->bottom_left_corner_cursor);

  /* Free extra GCs allocated by android_setup_relief_colors.  */
  if (f->output_data.android->white_relief.gc)
    {
      android_free_gc (f->output_data.android->white_relief.gc);
      f->output_data.android->white_relief.gc = 0;
    }
  if (f->output_data.android->black_relief.gc)
    {
      android_free_gc (f->output_data.android->black_relief.gc);
      f->output_data.android->black_relief.gc = 0;
    }

  if (f == dpyinfo->focus_frame)
    dpyinfo->focus_frame = 0;
  if (f == dpyinfo->x_focus_event_frame)
    dpyinfo->x_focus_event_frame = 0;
  if (f == dpyinfo->highlight_frame)
    dpyinfo->highlight_frame = 0;
  if (f == hlinfo->mouse_face_mouse_frame)
    reset_mouse_highlight (hlinfo);

  /* These two need to be freed now that they are used to compute the
     mouse position, I think.  */
  if (f == dpyinfo->last_mouse_motion_frame)
    dpyinfo->last_mouse_motion_frame = NULL;
  if (f == dpyinfo->last_mouse_frame)
    dpyinfo->last_mouse_frame = NULL;

  /* Free all tool presses currently active on this frame.  */
  next = FRAME_OUTPUT_DATA (f)->touch_points;
  while (next)
    {
      last = next;
      next = next->next;
      xfree (last);
    }

  /* Clear this in case unblock_input reads events.  */
  FRAME_OUTPUT_DATA (f)->touch_points = NULL;

  unblock_input ();
}

static void
android_delete_frame (struct frame *f)
{
  android_free_frame_resources (f);
  xfree (f->output_data.android);
  f->output_data.android = NULL;
}

static void
android_delete_terminal (struct terminal *terminal)
{
  error ("Cannot terminate connection to Android display server");
}



/* RIF functions.  */

static void
android_scroll_run (struct window *w, struct run *run)
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;

  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the left and right
     fringe of W.  */
  window_box (w, ANY_AREA, &x, &y, &width, &height);

  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->current_y);
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->desired_y);
  bottom_y = y + height;

  if (to_y < from_y)
    {
      /* Scrolling up.  Make sure we don't copy part of the mode
	 line at the bottom.  */
      if (from_y + run->height > bottom_y)
	height = bottom_y - from_y;
      else
	height = run->height;
    }
  else
    {
      /* Scrolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
    }

  block_input ();

  /* Cursor off.  Will be switched on again in gui_update_window_end.  */
  gui_clear_cursor (w);

  /* To avoid sequence point problems, make sure to only call
     FRAME_ANDROID_DRAWABLE once.  */
  android_copy_area (FRAME_ANDROID_DRAWABLE (f),
		     FRAME_ANDROID_WINDOW (f),
		     f->output_data.android->normal_gc,
		     x, from_y, width, height, x, to_y);

  unblock_input ();
}

static void
android_after_update_window_line (struct window *w, struct glyph_row *desired_row)
{
  eassert (w);

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = true;
}

static void
android_flip_and_flush (struct frame *f)
{
  block_input ();

  if (FRAME_ANDROID_NEED_BUFFER_FLIP (f))
    show_back_buffer (f);

  /* The frame is complete again as its contents were just
     flushed.  */
  FRAME_ANDROID_COMPLETE_P (f) = true;
  unblock_input ();
}

static void
android_clear_rectangle (struct frame *f, struct android_gc *gc, int x,
			 int y, int width, int height)
{
  struct android_gc_values xgcv;

  android_get_gc_values (gc, (ANDROID_GC_BACKGROUND
			      | ANDROID_GC_FOREGROUND),
			 &xgcv);
  android_set_foreground (gc, xgcv.background);
  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc,
			  x, y, width, height);
  android_set_foreground (gc, xgcv.foreground);
}

static void
android_reset_clip_rectangles (struct frame *f, struct android_gc *gc)
{
  android_set_clip_mask (gc, ANDROID_NONE);
}

static void
android_clip_to_row (struct window *w, struct glyph_row *row,
		     enum glyph_row_area area, struct android_gc *gc,
		     struct android_rectangle *rect_return)
{
  struct android_rectangle clip_rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  clip_rect.x = window_x;
  clip_rect.y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  clip_rect.y = max (clip_rect.y, window_y);
  clip_rect.width = window_width;
  clip_rect.height = row->visible_height;

  android_set_clip_rectangles (gc, 0, 0, &clip_rect, 1);

  if (rect_return)
    *rect_return = clip_rect;
}

static void
android_draw_fringe_bitmap (struct window *w, struct glyph_row *row,
			    struct draw_fringe_bitmap_params *p)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct android_gc *gc = f->output_data.android->normal_gc;
  struct face *face = p->face;
  struct android_rectangle clip_rect;

  /* Must clip because of partially visible lines.  */
  android_clip_to_row (w, row, ANY_AREA, gc, &clip_rect);

  if (p->bx >= 0 && !p->overlay_p)
    {
      /* In case the same realized face is used for fringes and for
	 something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 ANDROID_FILL_SOLID in
	 android_draw_glyph_string_background.  */
      if (face->stipple)
	{
	  android_set_fill_style (face->gc, ANDROID_FILL_OPAQUE_STIPPLED);
	  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), face->gc,
				  p->bx, p->by, p->nx, p->ny);
	  android_set_fill_style (face->gc, ANDROID_FILL_SOLID);

	  row->stipple_p = true;
	}
      else
	{
	  android_set_background (face->gc, face->background);
	  android_clear_rectangle (f, face->gc, p->bx, p->by, p->nx, p->ny);
	  android_set_foreground (face->gc, face->foreground);
	}
    }

  if (p->which)
    {
      android_drawable drawable;
      char *bits;
      android_pixmap pixmap, clipmask;
      struct android_gc_values gcv;
      unsigned long background, cursor_pixel;
      int depth;
      struct android_rectangle image_rect, dest;
      int px, py, pwidth, pheight;

      drawable = FRAME_ANDROID_DRAWABLE (f);
      clipmask = ANDROID_NONE;
      background = face->background;
      cursor_pixel = f->output_data.android->cursor_pixel;
      depth = FRAME_DISPLAY_INFO (f)->n_image_planes;

      /* Intersect the destination rectangle with that of the row.
	 Setting a clip mask overrides the clip rectangles provided by
	 android_clip_to_row, so clipping must be performed by
	 hand.  */

      image_rect.x = p->x;
      image_rect.y = p->y;
      image_rect.width = p->wd;
      image_rect.height = p->h;

      if (!gui_intersect_rectangles (&clip_rect, &image_rect, &dest))
	/* The entire destination rectangle falls outside the row.  */
	goto undo_clip;

      /* Extrapolate the source rectangle from the difference between
	 the destination and image rectangles.  */

      px = dest.x - image_rect.x;
      py = dest.y - image_rect.y;
      pwidth = dest.width;
      pheight = dest.height;

      if (p->wd > 8)
	bits = (char *) (p->bits + p->dh);
      else
	bits = (char *) p->bits + p->dh;

      pixmap = android_create_pixmap_from_bitmap_data (bits, p->wd, p->h,
						       (p->cursor_p
							? (p->overlay_p
							   ? face->background
							   : cursor_pixel)
							: face->foreground),
						       background, depth);

      if (p->overlay_p)
	{
	  clipmask = android_create_pixmap_from_bitmap_data (bits, p->wd, p->h,
							     1, 0, 1);

	  gcv.clip_mask = clipmask;
	  gcv.clip_x_origin = p->x;
	  gcv.clip_y_origin = p->y;
	  android_change_gc (gc, (ANDROID_GC_CLIP_MASK
				  | ANDROID_GC_CLIP_X_ORIGIN
				  | ANDROID_GC_CLIP_Y_ORIGIN),
			     &gcv);
	}

      android_copy_area (pixmap, drawable, gc, px, py,
			 pwidth, pheight, dest.x, dest.y);
      android_free_pixmap (pixmap);

      if (p->overlay_p)
	{
	  gcv.clip_mask = ANDROID_NONE;
	  android_change_gc (gc, ANDROID_GC_CLIP_MASK, &gcv);
	  android_free_pixmap (clipmask);
	}
    }

 undo_clip:
  android_reset_clip_rectangles (f, gc);
}

/* Set S->gc to a suitable GC for drawing glyph string S in cursor
   face.  */

static void
android_set_cursor_gc (struct glyph_string *s)
{
  if (s->font == FRAME_FONT (s->f)
      && s->face->background == FRAME_BACKGROUND_PIXEL (s->f)
      && s->face->foreground == FRAME_FOREGROUND_PIXEL (s->f)
      && !s->cmp)
    s->gc = s->f->output_data.android->cursor_gc;
  else
    {
      /* Cursor on non-default face: must merge.  */
      struct android_gc_values xgcv;
      unsigned long mask;

      xgcv.background = s->f->output_data.android->cursor_pixel;
      xgcv.foreground = s->face->background;

      /* If the glyph would be invisible, try a different foreground.  */
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->f->output_data.android->cursor_foreground_pixel;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;

      /* Make sure the cursor is distinct from text in this face.  */
      if (xgcv.background == s->face->background
	  && xgcv.foreground == s->face->foreground)
	{
	  xgcv.background = s->face->foreground;
	  xgcv.foreground = s->face->background;
	}

      mask = (ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND);

      if (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	android_change_gc (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc,
			   mask, &xgcv);
      else
	FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc
          = android_create_gc (mask, &xgcv);

      s->gc = FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }
}


/* Set up S->gc of glyph string S for drawing text in mouse face.  */

static void
android_set_mouse_face_gc (struct glyph_string *s)
{
  if (s->font == s->face->font)
    s->gc = s->face->gc;
  else
    {
      /* Otherwise construct scratch_cursor_gc with values from FACE
	 except for FONT.  */
      struct android_gc_values xgcv;
      unsigned long mask;

      xgcv.background = s->face->background;
      xgcv.foreground = s->face->foreground;

      mask = (ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND);

      if (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	android_change_gc (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc,
			   mask, &xgcv);
      else
	FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc
          = android_create_gc (mask, &xgcv);

      s->gc = FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }

  eassert (s->gc != 0);
}


/* Set S->gc of glyph string S to a GC suitable for drawing a mode line.
   Faces to use in the mode line have already been computed when the
   matrix was built, so there isn't much to do, here.  */

static void
android_set_mode_line_face_gc (struct glyph_string *s)
{
  s->gc = s->face->gc;
}

/* Set S->gc of glyph string S for drawing that glyph string.  Set
   S->stippled_p to a non-zero value if the face of S has a stipple
   pattern.  */

static void
android_set_glyph_string_gc (struct glyph_string *s)
{
  prepare_face_for_display (s->f, s->face);

  if (s->hl == DRAW_NORMAL_TEXT)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      android_set_mode_line_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_CURSOR)
    {
      android_set_cursor_gc (s);
      s->stippled_p = false;
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      android_set_mouse_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else
    emacs_abort ();

  /* GC must have been set.  */
  eassert (s->gc != 0);
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static void
android_set_glyph_string_clipping (struct glyph_string *s)
{
  struct android_rectangle *r = s->clip;
  int n = get_glyph_string_clip_rects (s, r, 2);

  if (n > 0)
    android_set_clip_rectangles (s->gc, 0, 0, r, n);
  s->num_clips = n;
}


/* Set SRC's clipping for output of glyph string DST.  This is called
   when we are drawing DST's left_overhang or right_overhang only in
   the area of SRC.  */

static void
android_set_glyph_string_clipping_exactly (struct glyph_string *src,
					   struct glyph_string *dst)
{
  struct android_rectangle r;

  r.x = src->x;
  r.width = src->width;
  r.y = src->y;
  r.height = src->height;
  dst->clip[0] = r;
  dst->num_clips = 1;
  android_set_clip_rectangles (dst->gc, 0, 0, &r, 1);
}

static void
android_compute_glyph_string_overhangs (struct glyph_string *s)
{
  if (s->cmp == NULL
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))
    {
      struct font_metrics metrics;

      if (s->first_glyph->type == CHAR_GLYPH)
	{
	  struct font *font = s->font;
	  font->driver->text_extents (font, s->char2b, s->nchars, &metrics);
	}
      else
	{
	  Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);

	  composition_gstring_width (gstring, s->cmp_from, s->cmp_to, &metrics);
	}
      s->right_overhang = (metrics.rbearing > metrics.width
			   ? metrics.rbearing - metrics.width : 0);
      s->left_overhang = metrics.lbearing < 0 ? - metrics.lbearing : 0;
    }
  else if (s->cmp)
    {
      s->right_overhang = s->cmp->rbearing - s->cmp->pixel_width;
      s->left_overhang = - s->cmp->lbearing;
    }
}

static void
android_clear_glyph_string_rect (struct glyph_string *s, int x, int y,
				 int w, int h)
{
  android_clear_rectangle (s->f, s->gc, x, y, w, h);
}

static void
android_draw_glyph_string_background (struct glyph_string *s, bool force_p)
{
  /* Nothing to do if background has already been drawn or if it
     shouldn't be drawn in the first place.  */
  if (!s->background_filled_p)
    {
      int box_line_width = max (s->face->box_horizontal_line_width, 0);

      if (s->stippled_p)
	{
	  /* Fill background with a stipple pattern.  */
	  android_set_fill_style (s->gc, ANDROID_FILL_OPAQUE_STIPPLED);
	  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc,
				  s->x, s->y + box_line_width,
				  s->background_width,
				  s->height - 2 * box_line_width);
	  android_set_fill_style (s->gc, ANDROID_FILL_SOLID);
	  s->background_filled_p = true;
	}
      else if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	       /* When xdisp.c ignores FONT_HEIGHT, we cannot trust
		  font dimensions, since the actual glyphs might be
		  much smaller.  So in that case we always clear the
		  rectangle with background color.  */
	       || FONT_TOO_HIGH (s->font)
	       || s->font_not_found_p
	       || s->extends_to_end_of_line_p
	       || force_p)
	{
	  android_clear_glyph_string_rect (s, s->x, s->y + box_line_width,
					   s->background_width,
					   s->height - 2 * box_line_width);
	  s->background_filled_p = true;
	}
    }
}

static void
android_fill_triangle (struct frame *f, struct android_gc *gc,
		       struct android_point point1,
		       struct android_point point2,
		       struct android_point point3)
{
  struct android_point abc[3];

  abc[0] = point1;
  abc[1] = point2;
  abc[2] = point3;

  android_fill_polygon (FRAME_ANDROID_DRAWABLE (f),
			gc, abc, 3, ANDROID_CONVEX,
			ANDROID_COORD_MODE_ORIGIN);
}

static struct android_point
android_make_point (int x, int y)
{
  struct android_point pt;

  pt.x = x;
  pt.y = y;

  return pt;
}

static bool
android_inside_rect_p (struct android_rectangle *rects, int nrects, int x,
		       int y)
{
  int i;

  for (i = 0; i < nrects; ++i)
    {
      if (x >= rects[i].x && y >= rects[i].y
	  && x < rects[i].x + rects[i].width
	  && y < rects[i].y + rects[i].height)
	return true;
    }

  return false;
}

static void
android_clear_point (struct frame *f, struct android_gc *gc,
		     int x, int y)
{
  struct android_gc_values xgcv;

  android_get_gc_values (gc, ANDROID_GC_BACKGROUND | ANDROID_GC_FOREGROUND,
			 &xgcv);
  android_set_foreground (gc, xgcv.background);
  android_draw_point (FRAME_ANDROID_DRAWABLE (f), gc, x, y);
  android_set_foreground (gc, xgcv.foreground);
}

static void
android_draw_relief_rect (struct frame *f, int left_x, int top_y, int right_x,
			  int bottom_y, int hwidth, int vwidth, bool raised_p,
			  bool top_p, bool bot_p, bool left_p, bool right_p,
			  struct android_rectangle *clip_rect)
{
  struct android_gc *gc, *white_gc, *black_gc, *normal_gc;
  android_drawable drawable;

  /* This code is more complicated than it has to be, because of two
     minor hacks to make the boxes look nicer: (i) if width > 1, draw
     the outermost line using the black relief.  (ii) Omit the four
     corner pixels.  */

  white_gc = f->output_data.android->white_relief.gc;
  black_gc = f->output_data.android->black_relief.gc;
  normal_gc = f->output_data.android->normal_gc;

  drawable = FRAME_ANDROID_DRAWABLE (f);

  android_set_clip_rectangles (white_gc, 0, 0, clip_rect, 1);
  android_set_clip_rectangles (black_gc, 0, 0, clip_rect, 1);

  if (raised_p)
    gc = white_gc;
  else
    gc = black_gc;

  /* Draw lines.  */

  if (top_p)
    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, left_x, top_y,
			    right_x - left_x + 1, hwidth);

  if (left_p)
    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, left_x, top_y,
			    vwidth, bottom_y - top_y + 1);

  if (raised_p)
    gc = black_gc;
  else
    gc = white_gc;

  if (bot_p)
    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, left_x,
			    bottom_y - hwidth + 1,
			    right_x - left_x + 1, hwidth);

  if (right_p)
    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc,
			    right_x - vwidth + 1,
			    top_y, vwidth, bottom_y - top_y + 1);

  /* Draw corners.  */

  if (bot_p && left_p)
    android_fill_triangle (f, raised_p ? white_gc : black_gc,
			   android_make_point (left_x, bottom_y - hwidth),
			   android_make_point (left_x + vwidth,
					       bottom_y - hwidth),
			   android_make_point (left_x, bottom_y));

  if (top_p && right_p)
    android_fill_triangle (f, raised_p ? white_gc : black_gc,
			   android_make_point (right_x - vwidth, top_y),
			   android_make_point (right_x, top_y),
			   android_make_point (right_x - vwidth,
					       top_y + hwidth));

  /* Draw outer line.  */

  if (top_p && left_p && bot_p && right_p
      && hwidth > 1 && vwidth > 1)
    android_draw_rectangle (FRAME_ANDROID_DRAWABLE (f),
			    black_gc, left_x, top_y,
			    right_x - left_x, bottom_y - top_y);
  else
    {
      if (top_p && hwidth > 1)
	android_draw_line (drawable, black_gc, left_x, top_y,
			   right_x + 1, top_y);

      if (bot_p && hwidth > 1)
	android_draw_line (drawable, black_gc, left_x, bottom_y,
			   right_x + 1, bottom_y);

      if (left_p && vwidth > 1)
	android_draw_line (drawable, black_gc, left_x, top_y,
			   left_x, bottom_y + 1);

      if (right_p && vwidth > 1)
	android_draw_line (drawable, black_gc, right_x, top_y,
			   right_x, bottom_y + 1);
    }

  /* Erase corners.  */

  if (hwidth > 1 && vwidth > 1)
    {
      if (left_p && top_p && android_inside_rect_p (clip_rect, 1,
						    left_x, top_y))
	android_clear_point (f, normal_gc, left_x, top_y);

      if (left_p && bot_p && android_inside_rect_p (clip_rect, 1,
						    left_x, bottom_y))
	android_clear_point (f, normal_gc, left_x, bottom_y);

      if (right_p && top_p && android_inside_rect_p (clip_rect, 1,
						     right_x, top_y))
	android_clear_point (f, normal_gc, right_x, top_y);

      if (right_p && bot_p && android_inside_rect_p (clip_rect, 1,
						     right_x, bottom_y))
	android_clear_point (f, normal_gc, right_x, bottom_y);
    }

  android_reset_clip_rectangles (f, white_gc);
  android_reset_clip_rectangles (f, black_gc);
}

static void
android_draw_box_rect (struct glyph_string *s,
		       int left_x, int top_y, int right_x, int bottom_y,
		       int hwidth, int vwidth, bool left_p, bool right_p,
		       struct android_rectangle *clip_rect)
{
  struct android_gc_values xgcv;

  android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
  android_set_foreground (s->gc, s->face->box_color);
  android_set_clip_rectangles (s->gc, 0, 0, clip_rect, 1);

  /* Top.  */
  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc, left_x,
			  top_y, right_x - left_x + 1, hwidth);

  /* Left.  */
  if (left_p)
    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc, left_x,
			    top_y, vwidth, bottom_y - top_y + 1);

  /* Bottom.  */
  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc, left_x,
			  bottom_y - hwidth + 1, right_x - left_x + 1,
			  hwidth);

  /* Right.  */
  if (right_p)
    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc,
			    right_x - vwidth + 1, top_y, vwidth,
			    bottom_y - top_y + 1);

  android_set_foreground (s->gc, xgcv.foreground);
  android_reset_clip_rectangles (s->f, s->gc);
}

#define HIGHLIGHT_COLOR_DARK_BOOST_LIMIT 48000

static bool
android_alloc_lighter_color (struct frame *f, unsigned long *pixel,
			     double factor, int delta)
{
  Emacs_Color color, new;
  long bright;
  bool success_p;

  /* Get RGB color values.  */
  color.pixel = *pixel;
  android_query_colors (f, &color, 1);

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  eassert (factor >= 0);
  new.red = min (0xffff, factor * color.red);
  new.green = min (0xffff, factor * color.green);
  new.blue = min (0xffff, factor * color.blue);

  /* Calculate brightness of COLOR.  */
  bright = (2 * color.red + 3 * color.green + color.blue) / 6;

  /* We only boost colors that are darker than
     HIGHLIGHT_COLOR_DARK_BOOST_LIMIT.  */
  if (bright < HIGHLIGHT_COLOR_DARK_BOOST_LIMIT)
    /* Make an additive adjustment to NEW, because it's dark enough so
       that scaling by FACTOR alone isn't enough.  */
    {
      /* How far below the limit this color is (0 - 1, 1 being darker).  */
      double dimness = 1 - (double) bright / HIGHLIGHT_COLOR_DARK_BOOST_LIMIT;
      /* The additive adjustment.  */
      int min_delta = delta * dimness * factor / 2;

      if (factor < 1)
	{
	  new.red =   max (0, new.red -   min_delta);
	  new.green = max (0, new.green - min_delta);
	  new.blue =  max (0, new.blue -  min_delta);
	}
      else
	{
	  new.red =   min (0xffff, min_delta + new.red);
	  new.green = min (0xffff, min_delta + new.green);
	  new.blue =  min (0xffff, min_delta + new.blue);
	}
    }

  /* Try to allocate the color.  */
  success_p = android_alloc_nearest_color (f, &new);

  if (success_p)
    {
      if (new.pixel == *pixel)
	{
	  /* If we end up with the same color as before, try adding
	     delta to the RGB values.  */
	  new.red = min (0xffff, delta + color.red);
	  new.green = min (0xffff, delta + color.green);
	  new.blue = min (0xffff, delta + color.blue);
	  success_p = android_alloc_nearest_color (f, &new);
	}
      else
	success_p = true;

      *pixel = new.pixel;
    }

  return success_p;
}

/* Set up the foreground color for drawing relief lines of glyph
   string S.  RELIEF is a pointer to a struct relief containing the GC
   with which lines will be drawn.  Use a color that is FACTOR or
   DELTA lighter or darker than the relief's background which is found
   in S->f->output_data.android->relief_background.  If such a color
   cannot be allocated, use DEFAULT_PIXEL, instead.  */

static void
android_setup_relief_color (struct frame *f, struct relief *relief,
			    double factor, int delta,
			    unsigned long default_pixel)
{
  struct android_gc_values xgcv;
  struct android_output *di = f->output_data.android;
  unsigned long mask = ANDROID_GC_FOREGROUND;
  unsigned long pixel;
  unsigned long background = di->relief_background;
  struct android_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  if (relief->gc && relief->pixel != -1)
    relief->pixel = -1;

  /* Allocate new color.  */
  xgcv.foreground = default_pixel;
  pixel = background;

  if (dpyinfo->n_planes != 1
      && android_alloc_lighter_color (f, &pixel, factor, delta))
    xgcv.foreground = relief->pixel = pixel;

  if (relief->gc == 0)
    relief->gc = android_create_gc (mask, &xgcv);
  else
    android_change_gc (relief->gc, mask, &xgcv);
}

/* Set up colors for the relief lines around glyph string S.  */

static void
android_setup_relief_colors (struct glyph_string *s)
{
  struct android_output *di;
  unsigned long color;

  di = s->f->output_data.android;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
    color = IMAGE_BACKGROUND (s->img, s->f, 0);
  else
    {
      struct android_gc_values xgcv;

      /* Get the background color of the face.  */
      android_get_gc_values (s->gc, ANDROID_GC_BACKGROUND, &xgcv);
      color = xgcv.background;
    }

  if (di->white_relief.gc == 0
      || color != di->relief_background)
    {
      di->relief_background = color;
      android_setup_relief_color (s->f, &di->white_relief, 1.2, 0x8000,
				  WHITE_PIX_DEFAULT (s->f));
      android_setup_relief_color (s->f, &di->black_relief, 0.6, 0x4000,
				  BLACK_PIX_DEFAULT (s->f));
    }
}

static void
android_draw_glyph_string_box (struct glyph_string *s)
{
  int hwidth, vwidth, left_x, right_x, top_y, bottom_y, last_x;
  bool raised_p, left_p, right_p;
  struct glyph *last_glyph;
  struct android_rectangle clip_rect;

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));

  /* The glyph that may have a right box line.  For static
     compositions and images, the right-box flag is on the first glyph
     of the glyph string; for other types it's on the last glyph.  */
  if (s->cmp || s->img)
    last_glyph = s->first_glyph;
  else if (s->first_glyph->type == COMPOSITE_GLYPH
	   && s->first_glyph->u.cmp.automatic)
    {
      /* For automatic compositions, we need to look up the last glyph
	 in the composition.  */
        struct glyph *end = s->row->glyphs[s->area] + s->row->used[s->area];
	struct glyph *g = s->first_glyph;
	for (last_glyph = g++;
	     g < end && g->u.cmp.automatic && g->u.cmp.id == s->cmp_id
	       && g->slice.cmp.to < s->cmp_to;
	     last_glyph = g++)
	  ;
    }
  else
    last_glyph = s->first_glyph + s->nchars - 1;

  vwidth = eabs (s->face->box_vertical_line_width);
  hwidth = eabs (s->face->box_horizontal_line_width);
  raised_p = s->face->box == FACE_RAISED_BOX;
  left_x = s->x;
  right_x = (s->row->full_width_p && s->extends_to_end_of_line_p
	     ? last_x - 1
	     : min (last_x, s->x + s->background_width) - 1);
  top_y = s->y;
  bottom_y = top_y + s->height - 1;

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL
		    || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL
		     || s->next->hl != s->hl)));

  get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    android_draw_box_rect (s, left_x, top_y, right_x, bottom_y, hwidth,
			   vwidth, left_p, right_p, &clip_rect);
  else
    {
      android_setup_relief_colors (s);
      android_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y, hwidth,
				vwidth, raised_p, true, true, left_p, right_p,
				&clip_rect);
    }
}

static void
android_draw_glyph_string_bg_rect (struct glyph_string *s, int x, int y,
				   int w, int h)
{
  if (s->stippled_p)
    {
      /* Fill background with a stipple pattern.  */
      android_set_fill_style (s->gc, ANDROID_FILL_OPAQUE_STIPPLED);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc, x,
			      y, w, h);
      android_set_fill_style (s->gc, ANDROID_FILL_SOLID);
    }
  else
    android_clear_glyph_string_rect (s, x, y, w, h);
}

static void
android_draw_image_relief (struct glyph_string *s)
{
  int x1, y1, thick;
  bool raised_p, top_p, bot_p, left_p, right_p;
  int extra_x, extra_y;
  struct android_rectangle r;
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      if (s->face->id == TAB_BAR_FACE_ID)
	thick = (tab_bar_button_relief < 0
		 ? DEFAULT_TAB_BAR_BUTTON_RELIEF
		 : min (tab_bar_button_relief, 1000000));
      else
	thick = (tool_bar_button_relief < 0
		 ? DEFAULT_TOOL_BAR_BUTTON_RELIEF
		 : min (tool_bar_button_relief, 1000000));
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = eabs (s->img->relief);
      raised_p = s->img->relief > 0;
    }

  x1 = x + s->slice.width - 1;
  y1 = y + s->slice.height - 1;

  extra_x = extra_y = 0;
  if (s->face->id == TAB_BAR_FACE_ID)
    {
      if (CONSP (Vtab_bar_button_margin)
	  && FIXNUMP (XCAR (Vtab_bar_button_margin))
	  && FIXNUMP (XCDR (Vtab_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtab_bar_button_margin)) - thick;
	  extra_y = XFIXNUM (XCDR (Vtab_bar_button_margin)) - thick;
	}
      else if (FIXNUMP (Vtab_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtab_bar_button_margin) - thick;
    }

  if (s->face->id == TOOL_BAR_FACE_ID)
    {
      if (CONSP (Vtool_bar_button_margin)
	  && FIXNUMP (XCAR (Vtool_bar_button_margin))
	  && FIXNUMP (XCDR (Vtool_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtool_bar_button_margin));
	  extra_y = XFIXNUM (XCDR (Vtool_bar_button_margin));
	}
      else if (FIXNUMP (Vtool_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtool_bar_button_margin);
    }

  top_p = bot_p = left_p = right_p = false;

  if (s->slice.x == 0)
    x -= thick + extra_x, left_p = true;
  if (s->slice.y == 0)
    y -= thick + extra_y, top_p = true;
  if (s->slice.x + s->slice.width == s->img->width)
    x1 += thick + extra_x, right_p = true;
  if (s->slice.y + s->slice.height == s->img->height)
    y1 += thick + extra_y, bot_p = true;

  android_setup_relief_colors (s);
  get_glyph_string_clip_rect (s, &r);
  android_draw_relief_rect (s->f, x, y, x1, y1, thick, thick, raised_p,
			    top_p, bot_p, left_p, right_p, &r);
}

static void
android_draw_image_foreground (struct glyph_string *s)
{
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->img->pixmap)
    {
      unsigned long mask = (ANDROID_GC_CLIP_MASK
			    | ANDROID_GC_CLIP_X_ORIGIN
			    | ANDROID_GC_CLIP_Y_ORIGIN
			    | ANDROID_GC_FUNCTION);
      struct android_gc_values xgcv;
      struct android_rectangle clip_rect, image_rect, r;

      xgcv.clip_mask = s->img->mask;
      xgcv.clip_x_origin = x - s->slice.x;
      xgcv.clip_y_origin = y - s->slice.y;
      xgcv.function = ANDROID_GC_COPY;
      android_change_gc (s->gc, mask, &xgcv);

      get_glyph_string_clip_rect (s, &clip_rect);
      image_rect.x = x;
      image_rect.y = y;
      image_rect.width = s->slice.width;
      image_rect.height = s->slice.height;

      if (gui_intersect_rectangles (&clip_rect, &image_rect, &r))
	android_copy_area (s->img->pixmap,
			   FRAME_ANDROID_DRAWABLE (s->f),
			   s->gc, s->slice.x + r.x - x,
			   s->slice.y + r.y - y,
			   r.width, r.height, r.x, r.y);

      /* When the image has a mask, we can expect that at least part
	 of a mouse highlight or a block cursor will be visible.  If
	 the image doesn't have a mask, make a block cursor visible by
	 drawing a rectangle around the image.  I believe it's looking
	 better if we do nothing here for mouse-face.  */
      if (s->hl == DRAW_CURSOR && !s->img->mask)
	{
	  int relief = eabs (s->img->relief);
	  android_draw_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc,
				  x - relief, y - relief,
				  s->slice.width + relief*2 - 1,
				  s->slice.height + relief*2 - 1);
	}

      android_set_clip_mask (s->gc, ANDROID_NONE);
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    android_draw_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc, x, y,
			    s->slice.width - 1, s->slice.height - 1);
}

static void
android_draw_image_glyph_string (struct glyph_string *s)
{
  int box_line_hwidth = max (s->face->box_vertical_line_width, 0);
  int box_line_vwidth = max (s->face->box_horizontal_line_width, 0);
  int height;

  height = s->height;
  if (s->slice.y == 0)
    height -= box_line_vwidth;
  if (s->slice.y + s->slice.height >= s->img->height)
    height -= box_line_vwidth;

  /* Fill background with face under the image.  Do it only if row is
     taller than image or if image has a clip mask to reduce
     flickering.  */
  s->stippled_p = s->face->stipple != 0;
  if (height > s->slice.height
      || s->img->hmargin
      || s->img->vmargin
      || s->img->mask
      || s->img->pixmap == 0
      || s->width != s->background_width)
    {
      if (s->stippled_p)
	s->row->stipple_p = true;

      int x = s->x;
      int y = s->y;
      int width = s->background_width;

      if (s->first_glyph->left_box_line_p
	  && s->slice.x == 0)
	{
	  x += box_line_hwidth;
	  width -= box_line_hwidth;
	}

      if (s->slice.y == 0)
	y += box_line_vwidth;

      android_draw_glyph_string_bg_rect (s, x, y, width, height);

      s->background_filled_p = true;
    }

  /* Draw the foreground.  */
  android_draw_image_foreground (s);
  android_set_glyph_string_clipping (s);

  /* If we must draw a relief around the image, do it.  */
  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    android_draw_image_relief (s);
}

static void
android_draw_stretch_glyph_string (struct glyph_string *s)
{
  eassert (s->first_glyph->type == STRETCH_GLYPH);

  if (s->hl == DRAW_CURSOR && !x_stretch_cursor_p)
    {
      /* If `x-stretch-cursor' is nil, don't draw a block cursor as
	 wide as the stretch glyph.  */
      int width, background_width = s->background_width;
      int x = s->x;

      if (!s->row->reversed_p)
	{
	  int left_x = window_box_left_offset (s->w, TEXT_AREA);

	  if (x < left_x)
	    {
	      background_width -= left_x - x;
	      x = left_x;
	    }
	}
      else
	{
	  /* In R2L rows, draw the cursor on the right edge of the
	     stretch glyph.  */
	  int right_x = window_box_right (s->w, TEXT_AREA);

	  if (x + background_width > right_x)
	    background_width -= x - right_x;
	  x += background_width;
	}
      width = min (FRAME_COLUMN_WIDTH (s->f), background_width);
      if (s->row->reversed_p)
	x -= width;

      /* Draw cursor.  */
      android_draw_glyph_string_bg_rect (s, x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < background_width)
	{
	  int y = s->y;
	  int w = background_width - width, h = s->height;
	  struct android_rectangle r;
	  struct android_gc *gc;

	  if (!s->row->reversed_p)
	    x += width;
	  else
	    x = s->x;
	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      android_set_mouse_face_gc (s);
	      gc = s->gc;
	    }
	  else
	    gc = s->face->gc;

	  get_glyph_string_clip_rect (s, &r);
	  android_set_clip_rectangles (gc, 0, 0, &r, 1);

	  if (s->face->stipple)
	    {
	      /* Fill background with a stipple pattern.  */
	      android_set_fill_style (gc, ANDROID_FILL_OPAQUE_STIPPLED);
	      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f),
				      gc, x, y, w, h);
	      android_set_fill_style (gc, ANDROID_FILL_SOLID);

	      s->row->stipple_p = true;
	    }
	  else
	    {
	      struct android_gc_values xgcv;
	      android_get_gc_values (gc, (ANDROID_GC_FOREGROUND
					  | ANDROID_GC_BACKGROUND),
				     &xgcv);
	      android_set_foreground (gc, xgcv.background);
	      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f),
				      gc, x, y, w, h);
	      android_set_foreground (gc, xgcv.foreground);
	    }

	  android_reset_clip_rectangles (s->f, gc);
	}
    }
  else if (!s->background_filled_p)
    {
      int background_width = s->background_width;
      int x = s->x, text_left_x = window_box_left (s->w, TEXT_AREA);

      /* Don't draw into left fringe or scrollbar area except for
         header line and mode line.  */
      if (s->area == TEXT_AREA
	  && x < text_left_x && !s->row->mode_line_p)
	{
	  background_width -= text_left_x - x;
	  x = text_left_x;
	}

      if (!s->row->stipple_p)
	s->row->stipple_p = s->stippled_p;

      if (background_width > 0)
	android_draw_glyph_string_bg_rect (s, x, s->y,
					   background_width,
					   s->height);
    }

  s->background_filled_p = true;
}

static void
android_get_scale_factor (int *scale_x, int *scale_y)
{
  /* This is 96 everywhere else, but 160 on Android.  */
  int base_res = 160;

  *scale_x = *scale_y = 1;
  eassert (x_display_list);

  if (x_display_list->resx > base_res)
    *scale_x = floor (x_display_list->resx / base_res);
  if (x_display_list->resy > base_res)
    *scale_y = floor (x_display_list->resy / base_res);
}

static void
android_draw_underwave (struct glyph_string *s, int decoration_width)
{
  int scale_x, scale_y;

  android_get_scale_factor (&scale_x, &scale_y);

  int wave_height = 3 * scale_y, wave_length = 2 * scale_x;

  int dx, dy, x0, y0, width, x1, y1, x2, y2, xmax;
  bool odd;
  struct android_rectangle wave_clip, string_clip, final_clip;

  dx = wave_length;
  dy = wave_height - 1;
  x0 = s->x;
  y0 = s->ybase + wave_height / 2;
  width = decoration_width;
  xmax = x0 + width;

  /* Find and set clipping rectangle */

  wave_clip.x = x0;
  wave_clip.y = y0;
  wave_clip.width = width;
  wave_clip.height = wave_height;
  get_glyph_string_clip_rect (s, &string_clip);

  if (!gui_intersect_rectangles (&wave_clip, &string_clip, &final_clip))
    return;

  android_set_clip_rectangles (s->gc, 0, 0, &final_clip, 1);

  /* Draw the waves */

  x1 = x0 - (x0 % dx);
  x2 = x1 + dx;
  odd = (x1 / dx) & 1;
  y1 = y2 = y0;

  if (odd)
    y1 += dy;
  else
    y2 += dy;

  if (INT_MAX - dx < xmax)
    emacs_abort ();

  while (x1 <= xmax)
    {
      android_draw_line (FRAME_ANDROID_DRAWABLE (s->f), s->gc,
			 x1, y1, x2, y2);
      x1  = x2, y1 = y2;
      x2 += dx, y2 = y0 + odd*dy;
      odd = !odd;
    }

  /* Restore previous clipping rectangle(s) */
  android_set_clip_rectangles (s->gc, 0, 0, s->clip, s->num_clips);
}

static void
android_draw_glyph_string_foreground (struct glyph_string *s)
{
  int i, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  /* Draw characters of S as rectangles if S's font could not be
     loaded.  */
  if (s->font_not_found_p)
    {
      for (i = 0; i < s->nchars; ++i)
	{
	  struct glyph *g = s->first_glyph + i;
	  android_draw_rectangle (FRAME_ANDROID_DRAWABLE (s->f),
				  s->gc, x, s->y,
				  g->pixel_width - 1,
				  s->height - 1);
	  x += g->pixel_width;
	}
    }
  else
    {
      struct font *font = s->font;
      int boff = font->baseline_offset;
      int y;

      if (font->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (font, s->f) - boff;

      y = s->ybase - boff;
      if (s->for_overlaps
	  || (s->background_filled_p && s->hl != DRAW_CURSOR))
	font->driver->draw (s, 0, s->nchars, x, y, false);
      else
	font->driver->draw (s, 0, s->nchars, x, y, true);
      if (s->face->overstrike)
	font->driver->draw (s, 0, s->nchars, x + 1, y, false);
    }
}

static void
android_draw_composite_glyph_string_foreground (struct glyph_string *s)
{
  int i, j, x;
  struct font *font = s->font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->cmp_from is the index
     of the first character drawn for glyphs of this composition.
     S->cmp_from == 0 means we are drawing the very first character of
     this composition.  */

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->cmp_from == 0)
	android_draw_rectangle (FRAME_ANDROID_DRAWABLE (s->f),
				s->gc, x, s->y,
				s->width - 1, s->height - 1);
    }
  else if (! s->first_glyph->u.cmp.automatic)
    {
      int y = s->ybase;

      for (i = 0, j = s->cmp_from; i < s->nchars; i++, j++)
	/* TAB in a composition means display glyphs with
	   padding space on the left or right.  */
	if (COMPOSITION_GLYPH (s->cmp, j) != '\t')
	  {
	    int xx = x + s->cmp->offsets[j * 2];
	    int yy = y - s->cmp->offsets[j * 2 + 1];

	    font->driver->draw (s, j, j + 1, xx, yy, false);
	    if (s->face->overstrike)
	      font->driver->draw (s, j, j + 1, xx + 1, yy, false);
	  }
    }
  else
    {
      Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);
      Lisp_Object glyph;
      int y = s->ybase;
      int width = 0;

      for (i = j = s->cmp_from; i < s->cmp_to; i++)
	{
	  glyph = LGSTRING_GLYPH (gstring, i);
	  if (NILP (LGLYPH_ADJUSTMENT (glyph)))
	    width += LGLYPH_WIDTH (glyph);
	  else
	    {
	      int xoff, yoff, wadjust;

	      if (j < i)
		{
		  font->driver->draw (s, j, i, x, y, false);
		  if (s->face->overstrike)
		    font->driver->draw (s, j, i, x + 1, y, false);
		  x += width;
		}
	      xoff = LGLYPH_XOFF (glyph);
	      yoff = LGLYPH_YOFF (glyph);
	      wadjust = LGLYPH_WADJUST (glyph);
	      font->driver->draw (s, i, i + 1, x + xoff, y + yoff, false);
	      if (s->face->overstrike)
		font->driver->draw (s, i, i + 1, x + xoff + 1, y + yoff,
				    false);
	      x += wadjust;
	      j = i + 1;
	      width = 0;
	    }
	}
      if (j < i)
	{
	  font->driver->draw (s, j, i, x, y, false);
	  if (s->face->overstrike)
	    font->driver->draw (s, j, i, x + 1, y, false);
	}
    }
}

static void
android_draw_glyphless_glyph_string_foreground (struct glyph_string *s)
{
  struct glyph *glyph = s->first_glyph;
  unsigned char2b[8];
  int x, i, j;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  s->char2b = char2b;

  for (i = 0; i < s->nchars; i++, glyph++)
    {
#ifdef GCC_LINT
      enum { PACIFY_GCC_BUG_81401 = 1 };
#else
      enum { PACIFY_GCC_BUG_81401 = 0 };
#endif
      char buf[7 + PACIFY_GCC_BUG_81401];
      char *str = NULL;
      int len = glyph->u.glyphless.len;

      if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_ACRONYM)
	{
	  if (len > 0
	      && CHAR_TABLE_P (Vglyphless_char_display)
	      && (CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (Vglyphless_char_display))
		  >= 1))
	    {
	      Lisp_Object acronym
		= (! glyph->u.glyphless.for_no_font
		   ? CHAR_TABLE_REF (Vglyphless_char_display,
				     glyph->u.glyphless.ch)
		   : XCHAR_TABLE (Vglyphless_char_display)->extras[0]);
	      if (CONSP (acronym))
		acronym = XCAR (acronym);
	      if (STRINGP (acronym))
		str = SSDATA (acronym);
	    }
	}
      else if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_HEX_CODE)
	{
	  unsigned int ch = glyph->u.glyphless.ch;
	  eassume (ch <= MAX_CHAR);
	  sprintf (buf, "%0*X", ch < 0x10000 ? 4 : 6, ch);
	  str = buf;
	}

      if (str)
	{
	  int upper_len = (len + 1) / 2;

	  /* It is assured that all LEN characters in STR is ASCII.  */
	  for (j = 0; j < len; j++)
            char2b[j] = s->font->driver->encode_char (s->font, str[j]) & 0xFFFF;
	  s->font->driver->draw (s, 0, upper_len,
				 x + glyph->slice.glyphless.upper_xoff,
				 s->ybase + glyph->slice.glyphless.upper_yoff,
				 false);
	  s->font->driver->draw (s, upper_len, len,
				 x + glyph->slice.glyphless.lower_xoff,
				 s->ybase + glyph->slice.glyphless.lower_yoff,
				 false);
	}
      if (glyph->u.glyphless.method != GLYPHLESS_DISPLAY_THIN_SPACE)
	android_draw_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc,
				x, s->ybase - glyph->ascent,
				glyph->pixel_width - 1,
				glyph->ascent + glyph->descent - 1);
      x += glyph->pixel_width;
   }

  /* Defend against hypothetical bad code elsewhere that uses
     s->char2b after this function returns.  */
  s->char2b = NULL;
}

/* Draw a dashed underline of thickness THICKNESS and width WIDTH onto F
   at a vertical offset of OFFSET from the position of the glyph string
   S, with each segment SEGMENT pixels in length.  */

static void
android_draw_dash (struct frame *f, struct glyph_string *s, int width,
		   int segment, int offset, int thickness)
{
  struct android_gc *gc;
  struct android_gc_values gcv;
  int y_center;

  /* Configure the GC, the dash pattern and a suitable offset.  */
  gc = s->gc;

  gcv.line_style = ANDROID_LINE_ON_OFF_DASH;
  gcv.line_width = thickness;
  android_change_gc (s->gc, (ANDROID_GC_LINE_STYLE
			     | ANDROID_GC_LINE_WIDTH), &gcv);
  android_set_dashes (s->gc, s->x, &segment, 1);

  /* Offset the origin of the line by half the line width. */
  y_center = s->ybase + offset + thickness / 2;
  android_draw_line (FRAME_ANDROID_WINDOW (f), gc,
		     s->x, y_center, s->x + width, y_center);

  /* Restore the initial line style.  */
  gcv.line_style = ANDROID_LINE_SOLID;
  gcv.line_width = 1;
  android_change_gc (s->gc, (ANDROID_GC_LINE_STYLE
			     | ANDROID_GC_LINE_WIDTH), &gcv);
}

/* Draw an underline of STYLE onto F at an offset of POSITION from the
   baseline of the glyph string S, DECORATION_WIDTH in length, and
   THICKNESS in height.  */

static void
android_fill_underline (struct frame *f, struct glyph_string *s,
			enum face_underline_type style, int position,
			int decoration_width, int thickness)
{
  int segment;

  segment = thickness * 3;

  switch (style)
    {
      /* FACE_UNDERLINE_DOUBLE_LINE is treated identically to SINGLE, as
	 the second line will be filled by another invocation of this
	 function.  */
    case FACE_UNDERLINE_SINGLE:
    case FACE_UNDERLINE_DOUBLE_LINE:
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f),
			      s->gc, s->x, s->ybase + position,
			      decoration_width, thickness);
      break;

    case FACE_UNDERLINE_DOTS:
      segment = thickness;
      FALLTHROUGH;

    case FACE_UNDERLINE_DASHES:
      android_draw_dash (f, s, decoration_width, segment, position,
			 thickness);
      break;

    case FACE_NO_UNDERLINE:
    case FACE_UNDERLINE_WAVE:
    default:
      emacs_abort ();
    }
}

static void
android_draw_glyph_string (struct glyph_string *s)
{
  bool relief_drawn_p = false;

  /* If S draws into the background of its successors, draw the
     background of the successors first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
	  {
	    android_set_glyph_string_gc (next);
	    android_set_glyph_string_clipping (next);
	    if (next->first_glyph->type == STRETCH_GLYPH)
	      android_draw_stretch_glyph_string (next);
	    else
	      android_draw_glyph_string_background (next, true);
	    next->num_clips = 0;
	  }
    }

  /* Set up S->gc, set clipping and draw S.  */
  android_set_glyph_string_gc (s);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps
      && s->face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))

    {
      android_set_glyph_string_clipping (s);
      android_draw_glyph_string_background (s, true);
      android_draw_glyph_string_box (s);
      android_set_glyph_string_clipping (s);
      relief_drawn_p = true;
    }
  else if (!s->clip_head /* draw_glyphs didn't specify a clip mask. */
	   && !s->clip_tail
	   && ((s->prev && s->prev->hl != s->hl && s->left_overhang)
	       || (s->next && s->next->hl != s->hl && s->right_overhang)))
    /* We must clip just this glyph.  left_overhang part has already
       drawn when s->prev was drawn, and right_overhang part will be
       drawn later when s->next is drawn. */
    android_set_glyph_string_clipping_exactly (s, s);
  else
    android_set_glyph_string_clipping (s);

  switch (s->first_glyph->type)
    {
    case IMAGE_GLYPH:
      android_draw_image_glyph_string (s);
      break;

    case XWIDGET_GLYPH:
      emacs_abort ();
      break;

    case STRETCH_GLYPH:
      android_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = true;
      else
	android_draw_glyph_string_background (s, false);
      android_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
	s->background_filled_p = true;
      else
	android_draw_glyph_string_background (s, true);
      android_draw_composite_glyph_string_foreground (s);
      break;

    case GLYPHLESS_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = true;
      else
	android_draw_glyph_string_background (s, true);
      android_draw_glyphless_glyph_string_foreground (s);
      break;

    default:
      emacs_abort ();
    }

  if (!s->for_overlaps)
    {
      int area_x, area_y, area_width, area_height;
      int area_max_x, decoration_width;

      /* Prevent the underline from overwriting surrounding areas
	 and the fringe.  */
      window_box (s->w, s->area, &area_x, &area_y,
		  &area_width, &area_height);
      area_max_x = area_x + area_width - 1;

      decoration_width = s->width;
      if (!s->row->mode_line_p
	  && !s->row->tab_line_p
	  && area_max_x < (s->x + decoration_width - 1))
	decoration_width -= (s->x + decoration_width - 1) - area_max_x;

      /* Draw relief if not yet drawn.  */
      if (!relief_drawn_p && s->face->box != FACE_NO_BOX)
	android_draw_glyph_string_box (s);

      /* Draw underline.  */
      if (s->face->underline)
        {
          if (s->face->underline == FACE_UNDERLINE_WAVE)
            {
              if (s->face->underline_defaulted_p)
                android_draw_underwave (s, decoration_width);
              else
                {
                  struct android_gc_values xgcv;
                  android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
                  android_set_foreground (s->gc, s->face->underline_color);
                  android_draw_underwave (s, decoration_width);
                  android_set_foreground (s->gc, xgcv.foreground);
                }
            }
          else if (s->face->underline >= FACE_UNDERLINE_SINGLE)
            {
              unsigned long thickness, position;

              if (s->prev
		  && (s->prev->face->underline != FACE_UNDERLINE_WAVE
		      && s->prev->face->underline >= FACE_UNDERLINE_SINGLE)
		  && (s->prev->face->underline_at_descent_line_p
		      == s->face->underline_at_descent_line_p)
		  && (s->prev->face->underline_pixels_above_descent_line
		      == s->face->underline_pixels_above_descent_line))
                {
                  /* We use the same underline style as the previous one.  */
                  thickness = s->prev->underline_thickness;
                  position = s->prev->underline_position;
                }
              else
                {
		  struct font *font = font_for_underline_metrics (s);
		  unsigned long minimum_offset;
		  bool underline_at_descent_line;
		  bool use_underline_position_properties;
		  Lisp_Object val = (WINDOW_BUFFER_LOCAL_VALUE
				     (Qunderline_minimum_offset, s->w));

		  if (FIXNUMP (val))
		    minimum_offset = max (0, XFIXNUM (val));
		  else
		    minimum_offset = 1;

		  val = (WINDOW_BUFFER_LOCAL_VALUE
			 (Qx_underline_at_descent_line, s->w));
		  underline_at_descent_line
		    = (!(NILP (val) || BASE_EQ (val, Qunbound))
		       || s->face->underline_at_descent_line_p);

		  val = (WINDOW_BUFFER_LOCAL_VALUE
			 (Qx_use_underline_position_properties, s->w));
		  use_underline_position_properties
		    = !(NILP (val) || BASE_EQ (val, Qunbound));

                  /* Get the underline thickness.  Default is 1 pixel.  */
                  if (font && font->underline_thickness > 0)
                    thickness = font->underline_thickness;
                  else
                    thickness = 1;
                  if (underline_at_descent_line)
		    position = ((s->height - thickness)
				- (s->ybase - s->y)
				- s->face->underline_pixels_above_descent_line);
                  else
                    {
                      /* Get the underline position.  This is the
                         recommended vertical offset in pixels from
                         the baseline to the top of the underline.
                         This is a signed value according to the
                         specs, and its default is

                         ROUND ((maximum descent) / 2), with
                         ROUND(x) = floor (x + 0.5)  */

                      if (use_underline_position_properties
                          && font && font->underline_position >= 0)
                        position = font->underline_position;
                      else if (font)
                        position = (font->descent + 1) / 2;
                      else
                        position = minimum_offset;
                    }

		  /* Ignore minimum_offset if the amount of pixels was
		     explicitly specified.  */
		  if (!s->face->underline_pixels_above_descent_line)
		    position = max (position, minimum_offset);
                }
              /* Check the sanity of thickness and position.  We should
                 avoid drawing underline out of the current line area.  */
	      if (s->y + s->height <= s->ybase + position)
		position = (s->height - 1) - (s->ybase - s->y);
              if (s->y + s->height < s->ybase + position + thickness)
                thickness = (s->y + s->height) - (s->ybase + position);
              s->underline_thickness = thickness;
              s->underline_position = position;

	      {
		struct android_gc_values xgcv;

		if (!s->face->underline_defaulted_p)
		  {
		    android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
		    android_set_foreground (s->gc, s->face->underline_color);
		  }

	        android_fill_underline (s->f, s, s->face->underline,
					position, decoration_width,
					thickness);

		/* Place a second underline above the first if this was
		   requested in the face specification.  */

		if (s->face->underline == FACE_UNDERLINE_DOUBLE_LINE)
		  {
		    /* Compute the position of the second underline.  */
		    position = position - thickness - 1;
		    android_fill_underline (s->f, s, s->face->underline,
					    position, decoration_width,
					    thickness);
		  }

		if (!s->face->underline_defaulted_p)
		  android_set_foreground (s->gc, xgcv.foreground);
	      }
            }
        }
      /* Draw overline.  */
      if (s->face->overline_p)
	{
	  unsigned long dy = 0, h = 1;

	  if (s->face->overline_color_defaulted_p)
	    android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f),
				    s->gc, s->x, s->y + dy,
				    decoration_width, h);
	  else
	    {
	      struct android_gc_values xgcv;
	      android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
	      android_set_foreground (s->gc, s->face->overline_color);
	      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc,
				      s->x, s->y + dy, decoration_width, h);
	      android_set_foreground (s->gc, xgcv.foreground);
	    }
	}

      /* Draw strike-through.  */
      if (s->face->strike_through_p)
	{
	  /* Y-coordinate and height of the glyph string's first
	     glyph.  We cannot use s->y and s->height because those
	     could be larger if there are taller display elements
	     (e.g., characters displayed with a larger font) in the
	     same glyph row.  */
	  int glyph_y = s->ybase - s->first_glyph->ascent;
	  int glyph_height = s->first_glyph->ascent + s->first_glyph->descent;
	  /* Strike-through width and offset from the glyph string's
	     top edge.  */
          unsigned long h = 1;
          unsigned long dy = (glyph_height - h) / 2;

	  if (s->face->strike_through_color_defaulted_p)
	    android_fill_rectangle (FRAME_ANDROID_WINDOW (s->f),
				    s->gc, s->x, glyph_y + dy,
				    s->width, h);
	  else
	    {
	      struct android_gc_values xgcv;
	      android_get_gc_values (s->gc, ANDROID_GC_FOREGROUND, &xgcv);
	      android_set_foreground (s->gc, s->face->strike_through_color);
	      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (s->f), s->gc,
				      s->x, glyph_y + dy, decoration_width,
				      h);
	      android_set_foreground (s->gc, xgcv.foreground);
	    }
	}

      if (s->prev)
	{
	  struct glyph_string *prev;

	  for (prev = s->prev; prev; prev = prev->prev)
	    if (prev->hl != s->hl
		&& prev->x + prev->width + prev->right_overhang > s->x)
	      {
		/* As prev was drawn while clipped to its own area, we
		   must draw the right_overhang part using s->hl now.  */
		enum draw_glyphs_face save = prev->hl;

		prev->hl = s->hl;
		android_set_glyph_string_gc (prev);
		android_set_glyph_string_clipping_exactly (s, prev);
		if (prev->first_glyph->type == CHAR_GLYPH)
		  android_draw_glyph_string_foreground (prev);
		else
		  android_draw_composite_glyph_string_foreground (prev);
		android_reset_clip_rectangles (prev->f, prev->gc);
		prev->hl = save;
		prev->num_clips = 0;
	      }
	}

      if (s->next)
	{
	  struct glyph_string *next;

	  for (next = s->next; next; next = next->next)
	    if (next->hl != s->hl
		&& next->x - next->left_overhang < s->x + s->width)
	      {
		/* As next will be drawn while clipped to its own area,
		   we must draw the left_overhang part using s->hl now.  */
		enum draw_glyphs_face save = next->hl;

		next->hl = s->hl;
		android_set_glyph_string_gc (next);
		android_set_glyph_string_clipping_exactly (s, next);
		if (next->first_glyph->type == CHAR_GLYPH)
		  android_draw_glyph_string_foreground (next);
		else
		  android_draw_composite_glyph_string_foreground (next);
		android_reset_clip_rectangles (next->f, next->gc);
		next->hl = save;
		next->num_clips = 0;
		next->clip_head = s->next;
	      }
	}
    }

  /* Reset clipping.  */
  android_reset_clip_rectangles (s->f, s->gc);
  s->num_clips = 0;

  /* Set the stippled flag that tells redisplay whether or not a
     stipple was actually draw.  */

  if (s->first_glyph->type != STRETCH_GLYPH
      && s->first_glyph->type != IMAGE_GLYPH
      && !s->row->stipple_p)
    s->row->stipple_p = s->stippled_p;
}

static void
android_define_frame_cursor (struct frame *f, Emacs_Cursor cursor)
{
  if (!f->pointer_invisible
      && !FRAME_ANDROID_OUTPUT (f)->hourglass
      && f->output_data.android->current_cursor != cursor)
    android_define_cursor (FRAME_ANDROID_WINDOW (f), cursor);

  f->output_data.android->current_cursor = cursor;
}

static void
android_clear_frame_area (struct frame *f, int x, int y,
			  int width, int height)
{
  android_clear_area (FRAME_ANDROID_DRAWABLE (f),
		      x, y, width, height);
}

void
android_clear_under_internal_border (struct frame *f)
{
  if (FRAME_INTERNAL_BORDER_WIDTH (f) > 0)
    {
      int border = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int height = FRAME_PIXEL_HEIGHT (f);
      int margin = FRAME_TOP_MARGIN_HEIGHT (f);
      int bottom_margin = FRAME_BOTTOM_MARGIN_HEIGHT (f);
      int face_id = (FRAME_PARENT_FRAME (f)
		     ? (!NILP (Vface_remapping_alist)
			? lookup_basic_face (NULL, f,
					     CHILD_FRAME_BORDER_FACE_ID)
			: CHILD_FRAME_BORDER_FACE_ID)
		     : (!NILP (Vface_remapping_alist)
			? lookup_basic_face (NULL, f,
					     INTERNAL_BORDER_FACE_ID)
			: INTERNAL_BORDER_FACE_ID));
      struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);

      if (face)
	{
	  unsigned long color = face->background;
	  struct android_gc *gc = f->output_data.android->normal_gc;

	  android_set_foreground (gc, color);
	  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, 0, margin,
				  width, border);
	  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, 0, 0,
				  border, height);
	  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, width - border,
				  0, border, height);
	  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, 0,
				  height - bottom_margin - border,
				  width, border);
	  android_set_foreground (gc, FRAME_FOREGROUND_PIXEL (f));
	}
      else
	{
	  android_clear_area (FRAME_ANDROID_DRAWABLE (f), 0, 0,
			      border, height);
	  android_clear_area (FRAME_ANDROID_DRAWABLE (f), 0,
			      margin, width, border);
	  android_clear_area (FRAME_ANDROID_DRAWABLE (f), width - border,
			      0, border, height);
	  android_clear_area (FRAME_ANDROID_DRAWABLE (f), 0,
			      height - bottom_margin - border,
			      width, border);
	}
    }
}

static void
android_draw_hollow_cursor (struct window *w, struct glyph_row *row)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct android_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  int x, y, wd, h;
  struct android_gc_values xgcv;
  struct glyph *cursor_glyph;
  struct android_gc *gc;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute frame-relative coordinates for phys cursor.  */
  get_phys_cursor_geometry (w, row, cursor_glyph, &x, &y, &h);
  wd = w->phys_cursor_width - 1;

  /* The foreground of cursor_gc is typically the same as the normal
     background color, which can cause the cursor box to be invisible.  */
  xgcv.foreground = f->output_data.android->cursor_pixel;
  if (dpyinfo->scratch_cursor_gc)
    android_change_gc (dpyinfo->scratch_cursor_gc,
		       ANDROID_GC_FOREGROUND, &xgcv);
  else
    dpyinfo->scratch_cursor_gc
      =  android_create_gc (ANDROID_GC_FOREGROUND, &xgcv);
  gc = dpyinfo->scratch_cursor_gc;

  /* When on R2L character, show cursor at the right edge of the
     glyph, unless the cursor box is as wide as the glyph or wider
     (the latter happens when x-stretch-cursor is non-nil).  */
  if ((cursor_glyph->resolved_level & 1) != 0
      && cursor_glyph->pixel_width > wd)
    {
      x += cursor_glyph->pixel_width - wd;
      if (wd > 0)
	wd -= 1;
    }
  /* Set clipping, draw the rectangle, and reset clipping again.  */
  android_clip_to_row (w, row, TEXT_AREA, gc, NULL);
  android_draw_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, x, y, wd, h - 1);
  android_reset_clip_rectangles (f, gc);
}

static void
android_draw_bar_cursor (struct window *w, struct glyph_row *row, int width,
			 enum text_cursor_kinds kind)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph *cursor_glyph;
  int cursor_start_y;

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Experimental avoidance of cursor on xwidget.  */
  if (cursor_glyph->type == XWIDGET_GLYPH)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      struct glyph_row *r;
      r = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      draw_phys_cursor_glyph (w, r, DRAW_CURSOR);
    }
  else
    {
      struct android_gc *gc = FRAME_DISPLAY_INFO (f)->scratch_cursor_gc;
      unsigned long mask = ANDROID_GC_FOREGROUND | ANDROID_GC_BACKGROUND;
      struct face *face = FACE_FROM_ID (f, cursor_glyph->face_id);
      struct android_gc_values xgcv;

      /* If the glyph's background equals the color we normally draw
	 the bars cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == f->output_data.android->cursor_pixel)
	xgcv.background = xgcv.foreground = face->foreground;
      else
	xgcv.background = xgcv.foreground = f->output_data.android->cursor_pixel;

      if (gc)
	android_change_gc (gc, mask, &xgcv);
      else
	{
          gc = android_create_gc (mask, &xgcv);
	  FRAME_DISPLAY_INFO (f)->scratch_cursor_gc = gc;
	}

      android_clip_to_row (w, row, TEXT_AREA, gc, NULL);

      if (kind == BAR_CURSOR)
	{
	  int x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

	  if (width < 0)
	    width = FRAME_CURSOR_WIDTH (f);
	  width = min (cursor_glyph->pixel_width, width);

	  w->phys_cursor_width = width;

	  /* If the character under cursor is R2L, draw the bar cursor
	     on the right of its glyph, rather than on the left.  */
	  if ((cursor_glyph->resolved_level & 1) != 0)
	    x += cursor_glyph->pixel_width - width;

	  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, x,
				  WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
				  width, row->height);
	}
      else /* HBAR_CURSOR */
	{
	  int dummy_x, dummy_y, dummy_h;
	  int x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

	  if (width < 0)
	    width = row->height;

	  width = min (row->height, width);

	  get_phys_cursor_geometry (w, row, cursor_glyph, &dummy_x,
				    &dummy_y, &dummy_h);

	  cursor_start_y = WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y
						    + row->height - width);

	  if ((cursor_glyph->resolved_level & 1) != 0
	      && cursor_glyph->pixel_width > w->phys_cursor_width - 1)
	    x += cursor_glyph->pixel_width - w->phys_cursor_width + 1;
	  android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f), gc, x,
				  cursor_start_y,
				  w->phys_cursor_width - 1, width);
	}

      android_reset_clip_rectangles (f, gc);
    }
}

static void
android_draw_window_cursor (struct window *w, struct glyph_row *glyph_row,
			    int x, int y, enum text_cursor_kinds cursor_type,
			    int cursor_width, bool on_p, bool active_p)
{
  struct frame *f;

  f = WINDOW_XFRAME (w);

  if (on_p)
    {
      w->phys_cursor_type = cursor_type;
      w->phys_cursor_on_p = true;

      if (glyph_row->exact_window_width_line_p
	  && (glyph_row->reversed_p
	      ? (w->phys_cursor.hpos < 0)
	      : (w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])))
	{
	  glyph_row->cursor_in_fringe_p = true;
	  draw_fringe_bitmap (w, glyph_row, glyph_row->reversed_p);
	}
      else
	{
	  switch (cursor_type)
	    {
	    case HOLLOW_BOX_CURSOR:
	      android_draw_hollow_cursor (w, glyph_row);
	      break;

	    case FILLED_BOX_CURSOR:
	      draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	      break;

	    case BAR_CURSOR:
	      android_draw_bar_cursor (w, glyph_row, cursor_width, BAR_CURSOR);
	      break;

	    case HBAR_CURSOR:
	      android_draw_bar_cursor (w, glyph_row, cursor_width, HBAR_CURSOR);
	      break;

	    case NO_CURSOR:
	      w->phys_cursor_width = 0;
	      break;

	    default:
	      emacs_abort ();
	    }
	}

      /* Now proceed to tell the input method the current position of
	 the cursor, if required.  */

      if (FRAME_OUTPUT_DATA (f)->need_cursor_updates
	  && w == XWINDOW (f->selected_window))
	android_set_preeditarea (w, x, y);
    }
}

static void
android_draw_vertical_window_border (struct window *w, int x, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;

  face = FACE_FROM_ID_OR_NULL (f, VERTICAL_BORDER_FACE_ID);
  if (face)
    android_set_foreground (f->output_data.android->normal_gc,
			    face->foreground);

  android_draw_line (FRAME_ANDROID_DRAWABLE (f),
		     f->output_data.android->normal_gc,
		     x, y0, x, y1);
}

static void
android_draw_window_divider (struct window *w, int x0, int x1, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_FACE_ID);
  struct face *face_first
    = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID);
  struct face *face_last
    = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_LAST_PIXEL_FACE_ID);
  unsigned long color = face ? face->foreground : FRAME_FOREGROUND_PIXEL (f);
  unsigned long color_first = (face_first
			       ? face_first->foreground
			       : FRAME_FOREGROUND_PIXEL (f));
  unsigned long color_last = (face_last
			      ? face_last->foreground
			      : FRAME_FOREGROUND_PIXEL (f));

  if ((y1 - y0 > x1 - x0) && (x1 - x0 >= 3))
    /* A vertical divider, at least three pixels wide: Draw first and
       last pixels differently.  */
    {
      android_set_foreground (f->output_data.android->normal_gc,
			      color_first);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f),
			      f->output_data.android->normal_gc,
			      x0, y0, 1, y1 - y0);
      android_set_foreground (f->output_data.android->normal_gc,
			      color);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f),
			      f->output_data.android->normal_gc,
			      x0 + 1, y0, x1 - x0 - 2, y1 - y0);
      android_set_foreground (f->output_data.android->normal_gc,
			      color_last);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f),
			      f->output_data.android->normal_gc,
			      x1 - 1, y0, 1, y1 - y0);
    }
  else if ((x1 - x0 > y1 - y0) && (y1 - y0 >= 3))
    /* A horizontal divider, at least three pixels high: Draw first
       and last pixels differently.  */
    {
      android_set_foreground (f->output_data.android->normal_gc,
			      color_first);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f),
			      f->output_data.android->normal_gc,
			      x0, y0, x1 - x0, 1);
      android_set_foreground (f->output_data.android->normal_gc, color);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f),
			      f->output_data.android->normal_gc,
			      x0, y0 + 1, x1 - x0, y1 - y0 - 2);
      android_set_foreground (f->output_data.android->normal_gc,
			      color_last);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f),
			      f->output_data.android->normal_gc,
			      x0, y1 - 1, x1 - x0, 1);
    }
  else
    {
      /* In any other case do not draw the first and last pixels
	 differently.  */
      android_set_foreground (f->output_data.android->normal_gc, color);
      android_fill_rectangle (FRAME_ANDROID_DRAWABLE (f),
			      f->output_data.android->normal_gc,
			      x0, y0, x1 - x0, y1 - y0);
    }
}



#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-prototypes"
#endif

/* Input method related functions.  Some of these are called from Java
   within the UI thread.  */

/* A counter used to decide when an editing request completes.  */
static unsigned long edit_counter;

/* The last counter known to have completed.  */
static unsigned long last_edit_counter;

/* Semaphore posted every time the counter increases.  */
static sem_t edit_sem;

/* Try to synchronize with the UI thread, waiting a certain amount of
   time for outstanding editing requests to complete.

   Every time one of the text retrieval functions is called and an
   editing request is made, Emacs gives the main thread approximately
   100 ms to process it, in order to mostly keep the input method in
   sync with the buffer contents.  */

static void
android_sync_edit (void)
{
  struct timespec start, end, rem;
  unsigned long counter;

  counter = __atomic_load_n (&last_edit_counter,
			     __ATOMIC_SEQ_CST);

  if (counter == edit_counter)
    return;

  start = current_timespec ();
  end = timespec_add (start, make_timespec (0, 100000000));

  while (true)
    {
      rem = timespec_sub (end, current_timespec ());

      /* Timeout.  */
      if (timespec_sign (rem) < 0)
	break;

      if (__atomic_load_n (&last_edit_counter,
			   __ATOMIC_SEQ_CST)
	  == edit_counter)
	break;

      sem_timedwait (&edit_sem, &end);
    }
}

/* Return a copy of the specified Java string and its length in
   *LENGTH.  Use the JNI environment ENV.  Value is NULL if copying
   the string fails.  */

static unsigned short *
android_copy_java_string (JNIEnv *env, jstring string, size_t *length)
{
  jsize size, i;
  const jchar *java;
  unsigned short *buffer;

  size = (*env)->GetStringLength (env, string);
  buffer = malloc (size * sizeof *buffer);

  if (!buffer)
    return NULL;

  java = (*env)->GetStringChars (env, string, NULL);

  if (!java)
    {
      free (buffer);
      return NULL;
    }

  for (i = 0; i < size; ++i)
    buffer[i] = java[i];

  *length = size;
  (*env)->ReleaseStringChars (env, string, java);
  return buffer;
}

JNIEXPORT void JNICALL
NATIVE_NAME (beginBatchEdit) (JNIEnv *env, jobject object, jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_START_BATCH_EDIT;
  event.ime.start = 0;
  event.ime.end = 0;
  event.ime.length = 0;
  event.ime.position = 0;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (endBatchEdit) (JNIEnv *env, jobject object, jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_END_BATCH_EDIT;
  event.ime.start = 0;
  event.ime.end = 0;
  event.ime.length = 0;
  event.ime.position = 0;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (commitCompletion) (JNIEnv *env, jobject object, jlong window,
				jstring completion_text, jint position)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  unsigned short *text;
  size_t length;

  /* First, obtain a copy of the Java string.  */
  text = android_copy_java_string (env, completion_text, &length);

  if (!text)
    return;

  /* Next, populate the event.  Events will always eventually be
     delivered on Android, so handle_one_android_event can be relied
     on to free text.  */

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_COMMIT_TEXT;
  event.ime.start = 0;
  event.ime.end = 0;
  event.ime.length = min (length, PTRDIFF_MAX);
  event.ime.position = position;
  event.ime.text = text;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (commitText) (JNIEnv *env, jobject object, jlong window,
			  jstring commit_text, jint position)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  unsigned short *text;
  size_t length;

  /* First, obtain a copy of the Java string.  */
  text = android_copy_java_string (env, commit_text, &length);

  if (!text)
    return;

  /* Next, populate the event.  Events will always eventually be
     delivered on Android, so handle_one_android_event can be relied
     on to free text.  */

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_COMMIT_TEXT;
  event.ime.start = 0;
  event.ime.end = 0;
  event.ime.length = min (length, PTRDIFF_MAX);
  event.ime.position = position;
  event.ime.text = text;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (deleteSurroundingText) (JNIEnv *env, jobject object,
				     jlong window, jint left_length,
				     jint right_length)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_DELETE_SURROUNDING_TEXT;
  event.ime.start = left_length;
  event.ime.end = right_length;
  event.ime.length = 0;
  event.ime.position = 0;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (finishComposingText) (JNIEnv *env, jobject object,
				   jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_FINISH_COMPOSING_TEXT;
  event.ime.start = 0;
  event.ime.end = 0;
  event.ime.length = 0;
  event.ime.position = 0;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (replaceText) (JNIEnv *env, jobject object, jlong window,
			   jint start, jint end, jobject text,
			   int new_cursor_position, jobject attribute)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  size_t length;

  /* First, obtain a copy of the Java string.  */
  text = android_copy_java_string (env, text, &length);

  if (!text)
    return;

  /* Next, populate the event with the information in this function's
     arguments.  */

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_REPLACE_TEXT;
  event.ime.start = start + 1;
  event.ime.end = end + 1;
  event.ime.length = length;
  event.ime.position = new_cursor_position;
  event.ime.text = text;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

/* Structure describing the context used for a text query.  */

struct android_conversion_query_context
{
  /* The conversion request.  */
  struct textconv_callback_struct query;

  /* The window the request is being made on.  */
  android_window window;

  /* Whether or not the request was successful.  */
  bool success;
};

/* Obtain the text from the frame whose window is that specified in
   DATA using the text conversion query specified there.

   Set ((struct android_conversion_query_context *) DATA)->success on
   success.  */

static void
android_perform_conversion_query (void *data)
{
  struct android_conversion_query_context *context;
  struct frame *f;

  context = data;

  /* Find the frame associated with the window.  */
  f = android_window_to_frame (NULL, context->window);

  if (!f)
    return;

  textconv_query (f, &context->query, 0);

  /* context->query.text will have been set even if textconv_query
     returns 1.  */

  context->success = true;
}

/* Convert a string in BUFFER, containing N characters in Emacs's
   internal multibyte encoding, to a Java string utilizing the
   specified JNI environment ENV.

   If N is equal to BYTES, then BUFFER holds unibyte or plain-ASCII
   characters.  Otherwise, BUFFER holds multibyte characters.

   Make sure N and BYTES are absolutely correct, or you are asking for
   trouble.

   Value is a jstring upon success, NULL otherwise.  Any exceptions
   generated are not cleared.  */

static jstring
android_text_to_string (JNIEnv *env, char *buffer, ptrdiff_t n,
			ptrdiff_t bytes)
{
  jchar *utf16;
  size_t size, index;
  jstring string;
  int encoded;

  if (n == bytes)
    {
      /* This buffer holds no multibyte characters.  */

      if (ckd_mul (&size, n, sizeof *utf16))
	return NULL;

      utf16 = malloc (size);
      index = 0;

      if (!utf16)
	return NULL;

      while (n--)
	{
	  utf16[index] = buffer[index];
	  index++;
	}

      string = (*env)->NewString (env, utf16, bytes);
      free (utf16);

      return string;
    }

  /* Allocate enough to hold N characters.  */

  if (ckd_mul (&size, n, sizeof *utf16))
    return NULL;

  utf16 = malloc (size);
  index = 0;

  if (!utf16)
    return NULL;

  while (n--)
    {
      eassert (CHAR_HEAD_P (*buffer));
      encoded = STRING_CHAR ((unsigned char *) buffer);

      /* Now establish how to save ENCODED into the string.
         Emacs operates on multibyte characters, not UTF-16 characters
         with surrogate pairs as Android does.

         However, character positions in Java are represented as
         character (rather than codepoint) indices into UTF-16
         strings, meaning that text positions reported to Android can
         become decoupled from their actual values if the text
         returned incorporates characters that must be encoded as
         surrogate pairs.

         The hack used by Emacs is to simply replace each multibyte
         character that doesn't fit in a jchar with the Unicode
         replacement character.  */

      if (encoded >= 65536)
	encoded = 0xfffd;

      utf16[index++] = encoded;
      buffer += BYTES_BY_CHAR_HEAD (*buffer);
    }

  /* Create the string.  */
  string = (*env)->NewString (env, utf16, index);
  free (utf16);
  return string;
}

JNIEXPORT jstring JNICALL
NATIVE_NAME (getTextAfterCursor) (JNIEnv *env, jobject object, jlong window,
				  jint length, jint flags)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  struct android_conversion_query_context context;
  jstring string;

  /* First, set up the conversion query.  */
  context.query.position = EMACS_INT_MAX;
  context.query.direction = TEXTCONV_FORWARD_CHAR;
  context.query.factor = min (length, 65535);
  context.query.operation = TEXTCONV_RETRIEVAL;

  /* Next, set the rest of the context.  */
  context.window = window;
  context.success = false;

  /* Now try to perform the query.  */
  android_sync_edit ();
  if (android_run_in_emacs_thread (android_perform_conversion_query,
				   &context))
    return NULL;

  if (!context.success)
    return NULL;

  /* context->query.text now contains the text in Emacs's internal
     UTF-8 based encoding.

     Convert it to Java's UTF-16 encoding, which is the same as
     UTF-16, except that NULL bytes are encoded as surrogate pairs.

     This assumes that `free' can free data allocated with xmalloc.  */

  string = android_text_to_string (env, context.query.text.text,
				   context.query.text.length,
				   context.query.text.bytes);
  free (context.query.text.text);

  return string;
}

JNIEXPORT jstring JNICALL
NATIVE_NAME (getTextBeforeCursor) (JNIEnv *env, jobject object, jlong window,
				   jint length, jint flags)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  struct android_conversion_query_context context;
  jstring string;

  /* First, set up the conversion query.  */
  context.query.position = TYPE_MINIMUM (EMACS_INT);
  context.query.direction = TEXTCONV_BACKWARD_CHAR;
  context.query.factor = min (length, 65535);
  context.query.operation = TEXTCONV_RETRIEVAL;

  /* Next, set the rest of the context.  */
  context.window = window;
  context.success = false;

  /* Now try to perform the query.  */
  android_sync_edit ();
  if (android_run_in_emacs_thread (android_perform_conversion_query,
				   &context))
    return NULL;

  if (!context.success)
    return NULL;

  /* context->query.text now contains the text in Emacs's internal
     UTF-8 based encoding.

     Convert it to Java's UTF-16 encoding, which is the same as
     UTF-16, except that NULL bytes are encoded as surrogate pairs.

     This assumes that `free' can free data allocated with xmalloc.  */

  string = android_text_to_string (env, context.query.text.text,
				   context.query.text.length,
				   context.query.text.bytes);
  free (context.query.text.text);

  return string;
}

JNIEXPORT void JNICALL
NATIVE_NAME (setComposingText) (JNIEnv *env, jobject object, jlong window,
				jstring composing_text,
				jint new_cursor_position)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  unsigned short *text;
  size_t length;

  /* First, obtain a copy of the Java string.  */
  text = android_copy_java_string (env, composing_text, &length);

  if (!text)
    return;

  /* Next, populate the event.  Events will always eventually be
     delivered on Android, so handle_one_android_event can be relied
     on to free text.  */

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_SET_COMPOSING_TEXT;
  event.ime.start = 0;
  event.ime.end = 0;
  event.ime.length = min (length, PTRDIFF_MAX);
  event.ime.position = new_cursor_position;
  event.ime.text = text;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (setComposingRegion) (JNIEnv *env, jobject object, jlong window,
				  jint start, jint end)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_SET_COMPOSING_REGION;
  event.ime.start = start + 1;
  event.ime.end = end + 1;
  event.ime.length = 0;
  event.ime.position = 0;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (setSelection) (JNIEnv *env, jobject object, jlong window,
			    jint start, jint end)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  /* While IMEs want access to the entire selection, Emacs only
     supports setting the point.  */

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_SET_POINT;
  event.ime.start = start + 1;
  event.ime.end = end + 1;
  event.ime.length = 0;
  event.ime.position = start;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

/* Structure describing the context for `getSelection'.  */

struct android_get_selection_context
{
  /* The window in question.  */
  android_window window;

  /* The position of the window's point when it was last
     redisplayed, and its last mark if active.  */
  ptrdiff_t point, mark;
};

/* Function run on the main thread by `getSelection'.
   Place the character position of point in PT.  */

static void
android_get_selection (void *data)
{
  struct android_get_selection_context *context;
  struct frame *f;
  struct window *w;
  struct buffer *b;

  context = data;

  /* Look up the associated frame and its selected window.  */
  f = android_window_to_frame (NULL, context->window);

  if (!f)
    context->point = -1;
  else
    {
      w = XWINDOW (f->selected_window);

      /* Return W's point as it is now.  Then, set
	 W->ephemeral_last_point to match the current point.  */
      context->point = window_point (w);
      w->ephemeral_last_point = context->point;

      /* Default context->mark to w->last_point too.  */
      context->mark = context->point;

      /* If the mark is active, then set it properly.  Also, adjust
	 w->last_mark to match.  */
      b = XBUFFER (w->contents);
      if (!NILP (BVAR (b, mark_active)))
	{
	  context->mark = marker_position (BVAR (b, mark));
	  w->last_mark = context->mark;
	}
    }
}

JNIEXPORT jintArray JNICALL
NATIVE_NAME (getSelection) (JNIEnv *env, jobject object, jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  struct android_get_selection_context context;
  jintArray array;
  jint contents[2];

  context.window = window;

  android_sync_edit ();
  if (android_run_in_emacs_thread (android_get_selection,
				   &context))
    return NULL;

  if (context.point == -1)
    return NULL;

  /* Wraparound actually makes more sense than truncation; at least
     editing will sort of work.  Convert the positions to start from
     index 0, as that is what Android expects.  */
  contents[0] = (unsigned int) min (context.point,
				    context.mark) - 1;
  contents[1] = (unsigned int) max (context.point,
				    context.mark) - 1;

  /* Now create the array.  */
  array = (*env)->NewIntArray (env, 2);

  if (!array)
    return NULL;

  /* Set its contents.  */
  (*env)->SetIntArrayRegion (env, array, 0, 2, contents);
  return array;
}

JNIEXPORT void JNICALL
NATIVE_NAME (performEditorAction) (JNIEnv *env, jobject object,
				   jlong window, int action)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  /* It's a good idea to call `android_sync_edit' before sending the
     key event.  Otherwise, if RET causes the current window to be
     changed, any text previously committed might end up in the newly
     selected window.  */

  android_sync_edit ();

  /* Undocumented behavior: performEditorAction is apparently expected
     to finish composing any text.  */

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_FINISH_COMPOSING_TEXT;
  event.ime.start = 0;
  event.ime.end = 0;

  /* This value of `length' means that the input method should receive
     an update containing the new conversion region.  */

  event.ime.length = 1;
  event.ime.position = 0;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);

  /* Finally, send the return key press.  `counter' is set; this means
     that a text conversion barrier will be generated once the event
     is read, which will cause subsequent edits to wait until the
     edits associated with this key press complete.  */

  event.xkey.type = ANDROID_KEY_PRESS;
  event.xkey.serial = ++event_serial;
  event.xkey.window = window;
  event.xkey.time = 0;
  event.xkey.state = 0;
  event.xkey.keycode = 66;
  event.xkey.unicode_char = 0;
  event.xkey.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (performContextMenuAction) (JNIEnv *env, jobject object,
					jlong window, int action)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  int key;

  /* Note that ACTION is determined in EmacsInputConnection, and as
     such they are not actual resource IDs.  */

  switch (action)
    {
      /* The subsequent three keycodes are addressed by
	 android_get_keysym_name rather than in keyboard.c.  */

    case 0: /* android.R.id.selectAll */
      key = 65536 + 1;
      break;

    case 1: /* android.R.id.startSelectingText */
      key = 65536 + 2;
      break;

    case 2: /* android.R.id.stopSelectingText */
      key = 65536 + 3;
      break;

    default:
      return;

    case 3: /* android.R.id.cut */
      key = 277;
      break;

    case 4: /* android.R.id.copy */
      key = 278;
      break;

    case 5: /* android.R.id.paste */
      key = 279;
      break;
    }

  event.xkey.type = ANDROID_KEY_PRESS;
  event.xkey.serial = ++event_serial;
  event.xkey.window = window;
  event.xkey.time = 0;
  event.xkey.state = 0;
  event.xkey.keycode = key;
  event.xkey.unicode_char = 0;
  event.xkey.counter = ++edit_counter;

  android_write_event (&event);
}



/* Text extraction.  */

struct android_get_extracted_text_context
{
  /* The parameters of the request.  */
  int hint_max_chars;

  /* Token for the request.  */
  int token;

  /* Flags associated with the request.  */
  int flags;

  /* The returned text, or NULL.  */
  char *text;

  /* The size of that text in characters and bytes.  */
  ptrdiff_t length, bytes;

  /* Offsets into that text.  */
  ptrdiff_t start, start_offset, end_offset;

  /* The window.  */
  android_window window;

  /* Whether or not the mark is active.  */
  bool mark_active;
};

/* Return the extracted text in the extracted text context specified
   by DATA.  Save its flags and token into its frame's state.  */

static void
android_get_extracted_text (void *data)
{
  struct android_get_extracted_text_context *request;
  struct frame *f;

  request = data;

  /* Find the frame associated with the window.  */
  f = android_window_to_frame (NULL, request->window);

  if (!f)
    return;

  /* Now get the extracted text.  */
  request->text
    = get_extracted_text (f, min (request->hint_max_chars, 600),
			  &request->start, &request->start_offset,
			  &request->end_offset, &request->length,
			  &request->bytes, &request->mark_active);

  /* See if request->flags & GET_EXTRACTED_TEXT_MONITOR.  If so, then
     the input method has asked to monitor changes to the extracted
     text until the next IM context reset.  */

  FRAME_ANDROID_OUTPUT (f)->extracted_text_flags = request->flags;
  FRAME_ANDROID_OUTPUT (f)->extracted_text_token = request->token;
  FRAME_ANDROID_OUTPUT (f)->extracted_text_hint = request->hint_max_chars;
}

/* Structure describing the `ExtractedTextRequest' class.
   Valid only on the UI thread.  */

struct android_extracted_text_request_class
{
  bool initialized;
  jfieldID hint_max_chars;
  jfieldID token;
};

/* Structure describing the `ExtractedText' class.
   Valid only on the UI thread.  */

struct android_extracted_text_class
{
  jclass class;
  jmethodID constructor;
  jfieldID flags;
  jfieldID partial_start_offset;
  jfieldID partial_end_offset;
  jfieldID selection_start;
  jfieldID selection_end;
  jfieldID start_offset;
  jfieldID text;
};

/* Fields and methods associated with the `ExtractedTextRequest'
   class.  */
static struct android_extracted_text_request_class request_class;

/* Fields and methods associated with the `ExtractedText' class.  */
static struct android_extracted_text_class text_class;

/* Return an ExtractedText object corresponding to the extracted text
   TEXT.  START is a character position describing the offset of the
   first character in TEXT.  START_OFFSET is the offset of the lesser
   of point or mark relative to START, and END_OFFSET is that of the
   greater of point or mark relative to START.  MARK_ACTIVE specifies
   whether or not the mark is currently active.

   Assume that request_class and text_class have already been
   initialized.

   Value is NULL if an error occurs; the exception is not cleared,
   else a local reference to the ExtractedText object.  */

static jobject
android_build_extracted_text (jstring text, ptrdiff_t start,
			      ptrdiff_t start_offset,
			      ptrdiff_t end_offset, bool mark_active)
{
  JNIEnv *env;
  jobject object;

  env = android_java_env;

  /* Return NULL if the class has not yet been obtained.  */
  if (!text_class.class)
    return NULL;

  /* Create an ExtractedText object containing this information.  */
  object = (*env)->NewObject (env, text_class.class,
			      text_class.constructor);
  if (!object)
    return NULL;

  (*env)->SetIntField (env, object, text_class.flags,
		       /* ExtractedText.FLAG_SELECTING */
		       mark_active ? 2 : 0);
  (*env)->SetIntField (env, object, text_class.partial_start_offset, -1);
  (*env)->SetIntField (env, object, text_class.partial_end_offset, -1);
  (*env)->SetIntField (env, object, text_class.selection_start,
		       min (start_offset, TYPE_MAXIMUM (jint)));
  (*env)->SetIntField (env, object, text_class.selection_end,
		       min (end_offset, TYPE_MAXIMUM (jint)));

  /* Subtract 1 from start: point indices in Emacs start from 1, but
     Android expects 0.  */
  (*env)->SetIntField (env, object, text_class.start_offset,
		       min (start - 1, TYPE_MAXIMUM (jint)));
  (*env)->SetObjectField (env, object, text_class.text, text);
  return object;
}

JNIEXPORT jobject JNICALL
NATIVE_NAME (getExtractedText) (JNIEnv *env, jobject ignored_object,
				jlong window, jobject request,
				jint flags)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  struct android_get_extracted_text_context context;
  jstring string;
  jclass class;
  jobject object;

  /* Initialize both classes if necessary.  */

  if (!request_class.initialized)
    {
      class
	= (*env)->FindClass (env, ("android/view/inputmethod"
				   "/ExtractedTextRequest"));
      eassert (class);

      request_class.hint_max_chars
	= (*env)->GetFieldID (env, class, "hintMaxChars", "I");
      eassert (request_class.hint_max_chars);

      request_class.token
	= (*env)->GetFieldID (env, class, "token", "I");
      eassert (request_class.token);

      request_class.initialized = true;
    }

  if (!text_class.class)
    {
      text_class.class
	= (*env)->FindClass (env, ("android/view/inputmethod"
				   "/ExtractedText"));
      eassert (text_class.class);

      class
	= text_class.class
	= (*env)->NewGlobalRef (env, text_class.class);
      eassert (text_class.class);

      text_class.flags
	= (*env)->GetFieldID (env, class, "flags", "I");
      text_class.partial_start_offset
	= (*env)->GetFieldID (env, class, "partialStartOffset", "I");
      text_class.partial_end_offset
	= (*env)->GetFieldID (env, class, "partialEndOffset", "I");
      text_class.selection_start
	= (*env)->GetFieldID (env, class, "selectionStart", "I");
      text_class.selection_end
	= (*env)->GetFieldID (env, class, "selectionEnd", "I");
      text_class.start_offset
	= (*env)->GetFieldID (env, class, "startOffset", "I");
      text_class.text
	= (*env)->GetFieldID (env, class, "text", "Ljava/lang/CharSequence;");
      text_class.constructor
	= (*env)->GetMethodID (env, class, "<init>", "()V");
    }

  context.hint_max_chars
    = (*env)->GetIntField (env, request, request_class.hint_max_chars);
  context.token
    = (*env)->GetIntField (env, request, request_class.token);
  context.flags = flags;
  context.text = NULL;
  context.window = window;

  android_sync_edit ();
  if (android_run_in_emacs_thread (android_get_extracted_text,
				   &context))
    return NULL;

  if (!context.text)
    return NULL;

  /* Encode the returned text.  */
  string = android_text_to_string (env, context.text, context.length,
				   context.bytes);
  free (context.text);

  if (!string)
    return NULL;

  /* Create an ExtractedText object containing this information.  */
  object = (*env)->NewObject (env, text_class.class,
			      text_class.constructor);
  if (!object)
    return NULL;

  (*env)->SetIntField (env, object, text_class.flags,
		       /* ExtractedText.FLAG_SELECTING */
		       context.mark_active ? 2 : 0);
  (*env)->SetIntField (env, object, text_class.partial_start_offset, -1);
  (*env)->SetIntField (env, object, text_class.partial_end_offset, -1);
  (*env)->SetIntField (env, object, text_class.selection_start,
		       min (context.start_offset, TYPE_MAXIMUM (jint)));
  (*env)->SetIntField (env, object, text_class.selection_end,
		       min (context.end_offset, TYPE_MAXIMUM (jint)));

  /* Subtract 1 from start: point indices in Emacs start from 1, but
     Android expects 0.  */
  (*env)->SetIntField (env, object, text_class.start_offset,
		       min (context.start - 1, TYPE_MAXIMUM (jint)));
  (*env)->SetObjectField (env, object, text_class.text, string);
  return object;
}



JNIEXPORT jstring JNICALL
NATIVE_NAME (getSelectedText) (JNIEnv *env, jobject object,
			       jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  struct android_get_extracted_text_context context;
  jstring string;

  context.hint_max_chars = -1;
  context.token = 0;
  context.text = NULL;
  context.window = window;

  android_sync_edit ();
  if (android_run_in_emacs_thread (android_get_extracted_text,
				   &context))
    return NULL;

  if (!context.text)
    return NULL;

  /* Encode the returned text.  */
  string = android_text_to_string (env, context.text, context.length,
				   context.bytes);
  free (context.text);

  return string;
}

JNIEXPORT void JNICALL
NATIVE_NAME (requestSelectionUpdate) (JNIEnv *env, jobject object,
				      jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_REQUEST_SELECTION_UPDATE;
  event.ime.start = 0;
  event.ime.end = 0;
  event.ime.length = 0;
  event.ime.position = 0;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}

JNIEXPORT void JNICALL
NATIVE_NAME (requestCursorUpdates) (JNIEnv *env, jobject object,
				    jlong window, jint mode)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_REQUEST_CURSOR_UPDATES;
  event.ime.start = 0;
  event.ime.end = 0;
  event.ime.length = mode;
  event.ime.position = 0;
  event.ime.text = NULL;

  /* Since this does not affect the state of the buffer text, there is
     no need to apply synchronization to this event.  */
  event.ime.counter = 0;

  android_write_event (&event);
}

/* Notice that a new input method connection has been initialized and
   clear cursor update requests, extracted text requests, and the
   composing region.  */

JNIEXPORT void JNICALL
NATIVE_NAME (clearInputFlags) (JNIEnv *env, jobject object,
			       jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.ime.type = ANDROID_INPUT_METHOD;
  event.ime.serial = ++event_serial;
  event.ime.window = window;
  event.ime.operation = ANDROID_IME_FINISH_COMPOSING_TEXT;
  event.ime.start = 0;
  event.ime.end = 0;

  /* This value of `length' means that updates to the cursor position
     and extracted text should not be reported anymore.  */

  event.ime.length = 2;
  event.ime.position = 0;
  event.ime.text = NULL;
  event.ime.counter = ++edit_counter;

  android_write_event (&event);
}



/* Context for a call to `getSurroundingText'.  */

struct android_get_surrounding_text_context
{
  /* Number of characters before the region to return.  */
  int before_length;

  /* Number of characters after the region to return.  */
  int after_length;

  /* The returned text, or NULL.  */
  char *text;

  /* The size of that text in characters and bytes.  */
  ptrdiff_t length, bytes;

  /* Offsets into that text.  */
  ptrdiff_t offset, start, end;

  /* The start and end indices of the conversion region.
     -1 if it does not exist.  */
  ptrdiff_t conversion_start, conversion_end;

  /* The window.  */
  android_window window;
};

/* Return the surrounding text in the surrounding text context
   specified by DATA.  */

static void
android_get_surrounding_text (void *data)
{
  struct android_get_surrounding_text_context *request;
  struct frame *f;
  ptrdiff_t temp;

  request = data;

  /* Find the frame associated with the window.  */
  f = android_window_to_frame (NULL, request->window);

  if (!f)
    return;

  /* Now get the surrounding text.  */
  request->text
    = get_surrounding_text (f, request->before_length,
			    request->after_length, &request->length,
			    &request->bytes, &request->offset,
			    &request->start, &request->end);

  /* Sort request->start and request->end for compatibility with some
     bad input methods.  */

  if (request->end < request->start)
    {
      temp = request->start;
      request->start = request->end;
      request->end = temp;
    }

  /* Retrieve the conversion region.  */

  request->conversion_start = -1;
  request->conversion_end = -1;

  if (MARKERP (f->conversion.compose_region_start))
    {
      request->conversion_start
	= marker_position (f->conversion.compose_region_start) - 1;
      request->conversion_end
	= marker_position (f->conversion.compose_region_end) - 1;
    }
}

/* Return a local reference to a `SurroundingText' object describing
   WINDOW's surrounding text.  ENV should be a valid JNI environment
   for the current thread.

   BEFORE_LENGTH and AFTER_LENGTH specify the number of characters
   around point and mark to return.

   Return the conversion region (or -1) in *CONVERSION_START and
   *CONVERSION_END if non-NULL.

   Value is the object upon success, else NULL.  */

static jobject
android_get_surrounding_text_internal (JNIEnv *env, jlong window,
				       jint before_length,
				       jint after_length,
				       ptrdiff_t *conversion_start,
				       ptrdiff_t *conversion_end)
{
  struct android_get_surrounding_text_context context;
  jstring string;
  jobject object;

  static jclass class;
  static jmethodID constructor;

  /* Initialize CLASS if it has not yet been initialized.  */

  if (!class)
    {
      class
	= (*env)->FindClass (env, ("android/view/inputmethod"
				   "/SurroundingText"));

#if __ANDROID_API__ < 31
      /* If CLASS cannot be found, the version of Android currently
	 running is too old.  */

      if (!class)
	{
	  (*env)->ExceptionClear (env);
	  return NULL;
	}
#else /* __ANDROID_API__ >= 31 */
      eassert (class);
#endif /* __ANDROID_API__ < 31 */

      class = (*env)->NewGlobalRef (env, class);
      if (!class)
	/* Clear class to prevent a local reference from remaining in
	   `class'.  */
	return (class = NULL);

      /* Now look for its constructor.  */
      constructor = (*env)->GetMethodID (env, class, "<init>",
					 "(Ljava/lang/CharSequence;III)V");
      eassert (constructor);
    }

  context.before_length = before_length;
  context.after_length = after_length;
  context.window = window;
  context.text = NULL;

  android_sync_edit ();
  if (android_run_in_emacs_thread (android_get_surrounding_text,
				   &context))
    return NULL;

  if (!context.text)
    return NULL;

  /* Encode the returned text.  */
  string = android_text_to_string (env, context.text, context.length,
				   context.bytes);
  free (context.text);

  if (!string)
    return NULL;

  /* Create an SurroundingText object containing this information.  */
  object = (*env)->NewObject (env, class, constructor, string,
			      (jint) min (context.start,
					  TYPE_MAXIMUM (jint)),
			      (jint) min (context.end,
					  TYPE_MAXIMUM (jint)),
			      /* Adjust point offsets to fit into
				 Android's 0-based indexing. */
			      (jint) min (context.offset - 1,
					  TYPE_MAXIMUM (jint)));
  if (!object)
    return NULL;

  /* Now return the conversion region if that was requested.  */

  if (conversion_start)
    {
      *conversion_start = context.conversion_start;
      *conversion_end = context.conversion_start;
    }

  return object;
}

JNIEXPORT jobject JNICALL
NATIVE_NAME (getSurroundingText) (JNIEnv *env, jobject object,
				  jlong window, jint before_length,
				  jint after_length, jint flags)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  return android_get_surrounding_text_internal (env, window, before_length,
						after_length, NULL, NULL);
}

JNIEXPORT jobject JNICALL
NATIVE_NAME (takeSnapshot) (JNIEnv *env, jobject object, jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  jobject text;
  ptrdiff_t start, end;

  static jclass class;
  static jmethodID constructor;

  /* First, obtain the surrounding text and conversion region.  */
  text = android_get_surrounding_text_internal (env, window, 600, 600,
						&start, &end);

  /* If that fails, return NULL.  */

  if (!text)
    return NULL;

  /* Next, initialize the TextSnapshot class.  */

  if (!class)
    {
      class
	= (*env)->FindClass (env, ("android/view/inputmethod"
				   "/TextSnapshot"));
#if __ANDROID_API__ < 33
      /* If CLASS cannot be found, the version of Android currently
	 running is too old.  */

      if (!class)
	{
	  (*env)->ExceptionClear (env);
	  return NULL;
	}
#else /* __ANDROID_API__ >= 33 */
      eassert (class);
#endif /* __ANDROID_API__ < 33 */

      class = (*env)->NewGlobalRef (env, class);
      if (!class)
	/* Clear class to prevent a local reference from remaining in
	   `class'.  */
	return (class = NULL);

      constructor = (*env)->GetMethodID (env, class, "<init>",
					 "(Landroid/view/inputmethod"
					 "/SurroundingText;III)V");
      eassert (constructor);
    }

  /* Try to create a TextSnapshot object.  */
  eassert (start <= end);
  object = (*env)->NewObject (env, class, constructor, text,
			      (jint) min (start, TYPE_MAXIMUM (jint)),
			      (jint) min (end, TYPE_MAXIMUM (jint)),
			      (jint) 0);
  return object;
}

#ifdef __clang__
#pragma clang diagnostic pop
#else /* GCC */
#pragma GCC diagnostic pop
#endif /* __clang__ */



/* Tell the input method where the composing region and selection of
   F's selected window is located.  W should be F's selected window;
   if it is NULL, then F->selected_window is used in its place.  */

static void
android_update_selection (struct frame *f, struct window *w)
{
  ptrdiff_t start, end, point, mark, start_offset, end_offset;
  ptrdiff_t length, bytes;
  struct buffer *b;
  int hint, token;
  char *text;
  jobject extracted;
  jstring string;
  bool mark_active;
  ptrdiff_t field_start, field_end;

  /* Offset these values by the start offset of the field.  */
  get_conversion_field (f, &field_start, &field_end);

  if (MARKERP (f->conversion.compose_region_start))
    {
      eassert (MARKERP (f->conversion.compose_region_end));

      /* Indexing in android starts from 0 instead of 1.  */
      start = marker_position (f->conversion.compose_region_start);
      end = marker_position (f->conversion.compose_region_end);

      /* Offset and detect underflow.  */
      start = max (start, field_start) - field_start;
      end = min (end, field_end) - field_start;
      if (end < 0 || start < 0)
	end = start = -1;
    }
  else
    start = -1, end = -1;

  /* Now constrain START and END to the maximum size of a Java
     integer.  */
  start = min (start, TYPE_MAXIMUM (jint));
  end = min (end, TYPE_MAXIMUM (jint));

  if (!w)
    w = XWINDOW (f->selected_window);

  /* Figure out where the point and mark are.  If the mark is not
     active, then point is set to equal mark.  */
  b = XBUFFER (w->contents);
  point = min (min (max (w->ephemeral_last_point,
			 field_start),
		    field_end) - field_start,
	       TYPE_MAXIMUM (jint));
  mark = ((!NILP (BVAR (b, mark_active))
	   && w->last_mark != -1)
	  ? min (min (max (w->last_mark, field_start),
		      field_end) - field_start,
		 TYPE_MAXIMUM (jint))
	  : point);

  /* Send the update.  Android doesn't employ a concept of "point" and
     "mark"; instead, it only has a selection, where the start of the
     selection is less than or equal to the end, and the region is
     "active" when those two values differ.  The indices will have been
     converted from 1-based Emacs indices to 0-based Android ones.  */
  android_update_ic (FRAME_ANDROID_WINDOW (f), min (point, mark),
		     max (point, mark), start, end);

  /* Update the extracted text as well, if the input method has asked
     for updates.  1 is InputConnection.GET_EXTRACTED_TEXT_MONITOR.  */

  if (FRAME_ANDROID_OUTPUT (f)->extracted_text_flags & 1)
    {
      hint = FRAME_ANDROID_OUTPUT (f)->extracted_text_hint;
      token = FRAME_ANDROID_OUTPUT (f)->extracted_text_token;
      text = get_extracted_text (f, min (hint, 600), &start,
				 &start_offset, &end_offset,
				 &length, &bytes, &mark_active);

      if (text)
	{
	  /* Make a string out of the extracted text.  */
	  string = android_text_to_string (android_java_env,
					   text, length, bytes);
	  xfree (text);
	  android_exception_check ();

	  /* Make extracted text out of that string.  */
	  extracted = android_build_extracted_text (string, start,
						    start_offset,
						    end_offset,
						    mark_active);
	  android_exception_check_1 (string);
	  ANDROID_DELETE_LOCAL_REF (string);

	  if (extracted)
	    {
	      /* extracted is now an associated ExtractedText object.
		 Perform the update.  */
	      android_update_extracted_text (FRAME_ANDROID_WINDOW (f),
					     extracted, token);
	      ANDROID_DELETE_LOCAL_REF (extracted);
	    }
	}
    }
}

/* Return whether or not EVENT is an input method event destined for
   the frame (struct frame *) ARG.  */

static bool
android_event_is_for_frame (union android_event *event, void *arg)
{
  struct frame *f;

  f = arg;
  return (event->type == ANDROID_INPUT_METHOD
	  && event->ime.window == FRAME_ANDROID_WINDOW (f));
}

/* Notice that the input method connection to F should be reset as a
   result of a change to its contents.  */

static void
android_reset_conversion (struct frame *f)
{
  enum android_ic_mode mode;
  struct window *w;
  struct buffer *buffer;
  Lisp_Object style;
  union android_event event;

  /* Reset the input method.

     Select an appropriate ``input mode'' based on whether or not the
     minibuffer window is selected, which in turn affects if ``RET''
     inserts a newline or sends an editor action Emacs transforms into
     a key event (refer to `performEditorAction'.)  */

  w = XWINDOW (f->selected_window);
  buffer = XBUFFER (WINDOW_BUFFER (w));

  style = (EQ (find_symbol_value (Qoverriding_text_conversion_style),
	       Qlambda)
	   ? BVAR (buffer, text_conversion_style)
	   : find_symbol_value (Qoverriding_text_conversion_style));

  if (NILP (style) || conversion_disabled_p ())
    mode = ANDROID_IC_MODE_NULL;
  else if (EQ (style, Qpassword))
    mode = ANDROID_IC_MODE_PASSWORD;
  else if (EQ (style, Qaction) || EQ (f->selected_window,
				      f->minibuffer_window))
    mode = ANDROID_IC_MODE_ACTION;
  else
    mode = ANDROID_IC_MODE_TEXT;

  /* Remove any existing input method events that apply to FRAME from
     the event queue.

     There's a small window between this and the call to
     android_reset_ic between which more events can be generated.  */

  while (android_check_if_event (&event, android_event_is_for_frame, f))
    {
      switch (event.ime.operation)
	{
	case ANDROID_IME_COMMIT_TEXT:
	case ANDROID_IME_FINISH_COMPOSING_TEXT:
	case ANDROID_IME_SET_COMPOSING_TEXT:
	  xfree (event.ime.text);
	  break;

	default:
	  break;
	}
    }

  android_reset_ic (FRAME_ANDROID_WINDOW (f), mode);

  /* Clear extracted text flags.  Since the IM has been reinitialized,
     it should no longer be displaying extracted text.  */
  FRAME_ANDROID_OUTPUT (f)->extracted_text_flags = 0;

  /* Move its selection to the specified position.  */
  android_update_selection (f, NULL);
}

/* Notice that point has moved in the F's selected window's selected
   buffer.  W is the window, and BUFFER is that buffer.  */

static void
android_set_point (struct frame *f, struct window *w,
		   struct buffer *buffer)
{
  android_update_selection (f, w);
}

/* Notice that the composition region on F's old selected window has
   changed.  */

static void
android_compose_region_changed (struct frame *f)
{
  android_update_selection (f, XWINDOW (f->old_selected_window));
}

/* Notice that the text conversion has completed.  */

static void
android_notify_conversion (unsigned long counter)
{
  int sval;

  if (last_edit_counter < counter)
    __atomic_store_n (&last_edit_counter, counter,
		      __ATOMIC_SEQ_CST);

  sem_getvalue (&edit_sem, &sval);

  if (sval <= 0)
    sem_post (&edit_sem);
}

/* Android text conversion interface.  */

static struct textconv_interface text_conversion_interface =
  {
    android_reset_conversion,
    android_set_point,
    android_compose_region_changed,
    android_notify_conversion,
  };



#endif /* !ANDROID_STUBIFY */

static struct redisplay_interface android_redisplay_interface =
  {
#ifndef ANDROID_STUBIFY
    android_frame_parm_handlers,
    gui_produce_glyphs,
    gui_write_glyphs,
    gui_insert_glyphs,
    gui_clear_end_of_line,
    android_scroll_run,
    android_after_update_window_line,
    NULL, /* update_window_begin */
    NULL, /* update_window_end   */
    android_flip_and_flush,
    gui_clear_window_mouse_face,
    gui_get_glyph_overhangs,
    gui_fix_overlapping_area,
    android_draw_fringe_bitmap,
    NULL, /* define_fringe_bitmap */
    NULL, /* destroy_fringe_bitmap */
    android_compute_glyph_string_overhangs,
    android_draw_glyph_string,
    android_define_frame_cursor,
    android_clear_frame_area,
    android_clear_under_internal_border,
    android_draw_window_cursor,
    android_draw_vertical_window_border,
    android_draw_window_divider,
    NULL,
    android_show_hourglass,
    android_hide_hourglass,
    android_default_font_parameter,
#endif
  };



void
frame_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
{
  /* This cannot be implemented on Android, and as such is left
     blank.  */
}

char *
get_keysym_name (int keysym)
{
  static char buffer[64];

#ifndef ANDROID_STUBIFY
  android_get_keysym_name (keysym, buffer, 64);
#else
  emacs_abort ();
#endif
  return buffer;
}



/* Create a struct terminal, initialize it with the Android specific
   functions and make DISPLAY->TERMINAL point to it.  */

static struct terminal *
android_create_terminal (struct android_display_info *dpyinfo)
{
  struct terminal *terminal;

  terminal = create_terminal (output_android,
			      &android_redisplay_interface);
  terminal->display_info.android = dpyinfo;
  dpyinfo->terminal = terminal;

  /* kboard is initialized in android_term_init.  */

#ifndef ANDROID_STUBIFY

  terminal->clear_frame_hook = android_clear_frame;
  terminal->ring_bell_hook = android_ring_bell;
  terminal->toggle_invisible_pointer_hook
    = android_toggle_invisible_pointer;
  terminal->update_begin_hook = android_update_begin;
  terminal->update_end_hook = android_update_end;
  terminal->read_socket_hook = android_read_socket;
  terminal->frame_up_to_date_hook = android_frame_up_to_date;
  terminal->buffer_flipping_unblocked_hook
    = android_buffer_flipping_unblocked_hook;
  terminal->defined_color_hook = android_defined_color;
  terminal->query_frame_background_color
    = android_query_frame_background_color;
  terminal->query_colors = android_query_colors;
  terminal->mouse_position_hook = android_mouse_position;
  terminal->get_focus_frame = android_get_focus_frame;
  terminal->focus_frame_hook = android_focus_frame;
  terminal->frame_rehighlight_hook = android_frame_rehighlight_hook;
  terminal->frame_raise_lower_hook = android_frame_raise_lower;
  terminal->frame_visible_invisible_hook
    = android_make_frame_visible_invisible;
  terminal->fullscreen_hook = android_fullscreen_hook;
  terminal->iconify_frame_hook = android_iconify_frame;
  terminal->set_window_size_hook = android_set_window_size;
  terminal->set_window_size_and_position_hook
    = android_set_window_size_and_position;
  terminal->set_frame_offset_hook = android_set_offset;
  terminal->set_frame_alpha_hook = android_set_alpha;
  terminal->set_new_font_hook = android_new_font;
  terminal->set_bitmap_icon_hook = android_bitmap_icon;
  terminal->implicit_set_name_hook = android_implicitly_set_name;
  terminal->menu_show_hook = android_menu_show;
  terminal->popup_dialog_hook = android_popup_dialog;
  terminal->change_tab_bar_height_hook = android_change_tab_bar_height;
  terminal->change_tool_bar_height_hook = android_change_tool_bar_height;
  terminal->set_scroll_bar_default_width_hook
    = android_set_scroll_bar_default_width;
  terminal->set_scroll_bar_default_height_hook
    = android_set_scroll_bar_default_height;
  terminal->free_pixmap = android_free_pixmap_hook;
  terminal->delete_frame_hook = android_delete_frame;
  terminal->delete_terminal_hook = android_delete_terminal;

#else
  emacs_abort ();
#endif

  return terminal;
}

/* Initialize the Android terminal interface.  The display connection
   has already been set up by the system at this point.  */

void
android_term_init (void)
{
  struct terminal *terminal;
  struct android_display_info *dpyinfo;
  Lisp_Object color_file, color_map;

  dpyinfo = xzalloc (sizeof *dpyinfo);
  terminal = android_create_terminal (dpyinfo);
  terminal->kboard = allocate_kboard (Qandroid);
  terminal->kboard->reference_count++;
  dpyinfo->n_planes = 24;
  dpyinfo->n_image_planes = 24;

  /* This function should only be called once at startup.  */
  eassert (!x_display_list);
  x_display_list = dpyinfo;

  dpyinfo->name_list_element
    = Fcons (build_string ("android"), Qnil);

  color_file = Fexpand_file_name (build_string ("rgb.txt"),
				  Vdata_directory);
  color_map = Fx_load_color_file (color_file);

  if (NILP (color_map))
    fatal ("Could not read %s.\n", SDATA (color_file));

  dpyinfo->color_map = color_map;

#ifndef ANDROID_STUBIFY
  dpyinfo->resx = android_pixel_density_x;
  dpyinfo->resy = android_pixel_density_y;
  dpyinfo->font_resolution = android_scaled_pixel_density;
  Vtoolkit_theme = (android_ui_mode == UI_MODE_NIGHT_YES
		    ? Qdark : Qlight);
#endif /* !ANDROID_STUBIFY */

  /* https://lists.gnu.org/r/emacs-devel/2015-11/msg00194.html  */
  dpyinfo->smallest_font_height = 1;
  dpyinfo->smallest_char_width = 1;

  terminal->name = xstrdup ("android");

  {
    Lisp_Object system_name = Fsystem_name ();
    static char const title[] = "GNU Emacs";
    if (STRINGP (system_name))
      {
	static char const at[] = " at ";
	ptrdiff_t nbytes = sizeof (title) + sizeof (at);
	if (ckd_add (&nbytes, nbytes, SBYTES (system_name)))
	  memory_full (SIZE_MAX);
	dpyinfo->x_id_name = xmalloc (nbytes);
	sprintf (dpyinfo->x_id_name, "%s%s%s", title, at,
		 SDATA (system_name));
      }
    else
      {
	dpyinfo->x_id_name = xmalloc (sizeof (title));
	strcpy (dpyinfo->x_id_name, title);
      }
  }

  /* The display "connection" is now set up, and it must never go
     away.  */
  terminal->reference_count = 30000;

  /* Set the baud rate to the same value it gets set to on X.  */
  baud_rate = 19200;

#ifndef ANDROID_STUBIFY
  sem_init (&edit_sem, false, 0);
  register_textconv_interface (&text_conversion_interface);
#endif /* !ANDROID_STUBIFY */

  /* Binding certain key events in the terminal's `input-decode-map',
     which being keyboard-local is not accessible from any point in
     android-win.el.  */
  Fdefine_key (KVAR (terminal->kboard, Vinput_decode_map),
	       make_vector (1, Qselect), make_vector (1, Qreturn),
	       Qnil);
}



/* Set Vandroid_build_fingerprint to a reasonable value, and also
   Vandroid_build_manufacturer.  */

static void
android_set_build_fingerprint (void)
{
#ifdef ANDROID_STUBIFY
  Vandroid_build_fingerprint = Qnil;
#else /* !ANDROID_STUBIFY */
  jclass class;
  jfieldID field;
  jobject string;
  const char *data;

  /* Set class to NULL so freeing an uninitialized local ref can be
     avoided.  */
  class = NULL;

  /* Likewise for string.  */
  string = NULL;

  if (!android_init_gui)
    goto fail;
  else
    {
      /* Obtain Build.FINGERPRINT.  Clear exceptions after each query;
	 JNI can't find Build.FINGERPRINT on some systems.  */

      class = (*android_java_env)->FindClass (android_java_env,
					      "android/os/Build");
      (*android_java_env)->ExceptionClear (android_java_env);

      if (!class)
	goto fail;

      field = (*android_java_env)->GetStaticFieldID (android_java_env,
						     class,
						     "FINGERPRINT",
						     "Ljava/lang/String;");
      (*android_java_env)->ExceptionClear (android_java_env);

      if (!field)
	goto fail;

      string
	= (*android_java_env)->GetStaticObjectField (android_java_env,
						     class, field);
      (*android_java_env)->ExceptionClear (android_java_env);

      if (!string)
	goto fail;

      data = (*android_java_env)->GetStringUTFChars (android_java_env,
						     string, NULL);
      (*android_java_env)->ExceptionClear (android_java_env);

      if (!data)
	goto fail;

      Vandroid_build_fingerprint = build_string_from_utf8 (data);
      (*android_java_env)->ReleaseStringUTFChars (android_java_env,
						  string, data);

      /* Now retrieve Build.MANUFACTURER.  */

      ANDROID_DELETE_LOCAL_REF (string);
      string = NULL;

      field = (*android_java_env)->GetStaticFieldID (android_java_env,
						     class,
						     "MANUFACTURER",
						     "Ljava/lang/String;");
      (*android_java_env)->ExceptionClear (android_java_env);

      if (!field)
	goto fail;

      string
	= (*android_java_env)->GetStaticObjectField (android_java_env,
						     class, field);
      (*android_java_env)->ExceptionClear (android_java_env);

      if (!string)
	goto fail;

      data = (*android_java_env)->GetStringUTFChars (android_java_env,
						     string, NULL);
      (*android_java_env)->ExceptionClear (android_java_env);

      if (!data)
	goto fail;

      Vandroid_build_manufacturer = build_string_from_utf8 (data);
      (*android_java_env)->ReleaseStringUTFChars (android_java_env,
						  string, data);
    }

  if (string)
    ANDROID_DELETE_LOCAL_REF (string);

  ANDROID_DELETE_LOCAL_REF (class);

  return;

 fail:
  if (class)
    ANDROID_DELETE_LOCAL_REF (class);

  Vandroid_build_fingerprint = Qnil;
  Vandroid_build_manufacturer = Qnil;
#endif /* ANDROID_STUBIFY */
}

void
syms_of_androidterm (void)
{
  Fprovide (Qandroid, Qnil);

  DEFVAR_LISP ("android-wait-for-event-timeout",
	       Vandroid_wait_for_event_timeout,
    doc: /* How long to wait for Android events.

Emacs will wait up to this many seconds to receive events after
making changes which affect the state of the graphical interface.
Under some situations this can take an indefinite amount of time,
so it is important to limit the wait.

If set to a non-float value, there will be no wait at all.  */);
  Vandroid_wait_for_event_timeout = make_float (0.1);

  DEFVAR_INT ("android-quit-keycode", android_quit_keycode,
    doc: /* Keycode that signals quit when typed twice in rapid succession.

This is the key code of a key whose repeated activation should prompt
Emacs to quit, enabling quitting on systems where a keyboard capable of
typing C-g is unavailable, when set to a key that does exist on the
device.  Its value must be a keycode defined by the operating system,
and defaults to 25 (KEYCODE_VOLUME_DOWN), though one of the following
values might be desired on those devices where this default is also
unavailable, or if another key must otherwise serve this function
instead:

  - 4  (KEYCODE_BACK)
  - 24 (KEYCODE_VOLUME_UP)  */);
  android_quit_keycode = 25;

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_use_underline_position_properties = true;
  DEFSYM (Qx_use_underline_position_properties,
	  "x-use-underline-position-properties");

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* SKIP: real doc in xterm.c.  */);
  x_underline_at_descent_line = false;

  DEFVAR_LISP ("android-build-fingerprint", Vandroid_build_fingerprint,
    doc: /* String identifying the device's OS version.
This is a string that uniquely identifies the version of Android
Emacs is running on.  */);
  Vandroid_build_fingerprint = Qnil;

  DEFVAR_LISP ("android-build-manufacturer", Vandroid_build_manufacturer,
    doc: /* Name of the developer of the running version of Android.  */);
  Vandroid_build_manufacturer = Qnil;

  DEFVAR_INT ("android-display-planes", android_display_planes,
    doc: /* Depth and visual class of the display.
This variable controls the visual class and depth of the display, which
cannot be detected on Android.  The default value of 24, and values from
there to 8 represent a TrueColor display providing 24 planes, values
between 8 and 1 StaticGray displays providing that many planes, and 1 or
lower monochrome displays with a single plane.  Modifications to this
variable must be completed before the window system is initialized, in,
for instance, `early-init.el', or they will be of no effect.  */);
  android_display_planes = 24;

  DEFVAR_LISP ("x-ctrl-keysym", Vx_ctrl_keysym,
    doc: /* SKIP: real doc in xterm.c.  */);
  Vx_ctrl_keysym = Qnil;

  DEFVAR_LISP ("x-alt-keysym", Vx_alt_keysym,
    doc: /* SKIP: real doc in xterm.c.  */);
  Vx_alt_keysym = Qnil;

  DEFVAR_LISP ("x-hyper-keysym", Vx_hyper_keysym,
    doc: /* SKIP: real doc in xterm.c.  */);
  Vx_hyper_keysym = Qnil;

  DEFVAR_LISP ("x-meta-keysym", Vx_meta_keysym,
    doc: /* SKIP: real doc in xterm.c.  */);
  Vx_meta_keysym = Qnil;

  DEFVAR_LISP ("x-super-keysym", Vx_super_keysym,
    doc: /* SKIP: real doc in xterm.c.  */);
  Vx_super_keysym = Qnil;

  /* Only defined so loadup.el loads scroll-bar.el.  */
  DEFVAR_LISP ("x-toolkit-scroll-bars", Vx_toolkit_scroll_bars,
    doc: /* SKIP: real doc in xterm.c.  */);
  Vx_toolkit_scroll_bars = Qnil;

  /* Avoid dumping Vandroid_build_fingerprint.  */
  pdumper_do_now_and_after_load (android_set_build_fingerprint);

  DEFSYM (Qx_underline_at_descent_line, "x-underline-at-descent-line");

  /* Symbols defined for DND events.  */
  DEFSYM (Quri, "uri");
  DEFSYM (Qtext, "text");

  /* Symbols defined for modifier value reassignment.  */
  DEFSYM (Qmodifier_value, "modifier-value");
  DEFSYM (Qctrl, "ctrl");
  Fput (Qctrl, Qmodifier_value, make_fixnum (ctrl_modifier));
  DEFSYM (Qalt, "alt");
  Fput (Qalt, Qmodifier_value, make_fixnum (alt_modifier));
  DEFSYM (Qmeta, "meta");
  Fput (Qmeta, Qmodifier_value, make_fixnum (meta_modifier));
  DEFSYM (Qsuper, "super");
  Fput (Qsuper, Qmodifier_value, make_fixnum (super_modifier));

  /* Key symbols.  */
  DEFSYM (Qselect, "select");
  DEFSYM (Qreturn, "return");

  /* Display configuration updates.  */
  DEFSYM (Qfont_render, "font-render");
  DEFSYM (Qdynamic_setting, "dynamic-setting");
  Fprovide (Qdynamic_setting, Qnil);
}

void
mark_androidterm (void)
{
  if (x_display_list)
    mark_object (x_display_list->color_map);
}
