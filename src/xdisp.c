/* Display generation from window structure and buffer text.
   Copyright (C) 1985, 1986, 1987, 1988, 1993 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include <stdio.h>
/*#include <ctype.h>*/
#undef NULL
#include "lisp.h"
#include "frame.h"
#include "window.h"
#include "termchar.h"
#include "dispextern.h"
#include "buffer.h"
#include "indent.h"
#include "commands.h"
#include "macros.h"
#include "disptab.h"
#include "termhooks.h"

extern int interrupt_input;
extern int command_loop_level;

/* Nonzero means print newline before next minibuffer message.  */

int noninteractive_need_newline;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* The buffer position of the first character appearing
 entirely or partially on the current frame line.
 Or zero, which disables the optimization for the current frame line. */
static int this_line_bufpos;

/* Number of characters past the end of this line,
   including the terminating newline */
static int this_line_endpos;

/* The vertical position of this frame line. */
static int this_line_vpos;

/* Hpos value for start of display on this frame line.
   Usually zero, but negative if first character really began
   on previous line */
static int this_line_start_hpos;

/* Buffer that this_line variables are describing. */
static struct buffer *this_line_buffer;

/* Set by try_window_id to the vpos of first of any lines
   scrolled on to the bottom of the frame.  These lines should
   not be included in any general scroll computation.  */
static int scroll_bottom_vpos;

/* Value of echo_area_glyphs when it was last acted on.
  If this is nonzero, there is a message on the frame
  in the minibuffer and it should be erased as soon
  as it is no longer requested to appear. */
char *previous_echo_glyphs;

/* Nonzero means truncate lines in all windows less wide than the frame */
int truncate_partial_width_windows;

Lisp_Object Vglobal_mode_string;

/* Marker for where to display an arrow on top of the buffer text.  */
Lisp_Object Voverlay_arrow_position;

/* String to display for the arrow.  */
Lisp_Object Voverlay_arrow_string;

/* Values of those variables at last redisplay.  */
static Lisp_Object last_arrow_position, last_arrow_string;

/* Nonzero if overlay arrow has been displayed once in this window.  */
static int overlay_arrow_seen;

/* If cursor motion alone moves point off frame,
   Try scrolling this many lines up or down if that will bring it back.  */
int scroll_step;

/* Nonzero if try_window_id has made blank lines at window bottom
 since the last redisplay that paused */
static int blank_end_of_window;

/* Number of windows showing the buffer of the selected window.
   keyboard.c refers to this.  */
int buffer_shared;

/* display_text_line sets these to the frame position (origin 0) of point,
   whether the window is selected or not.
   Set one to -1 first to determine whether point was found afterwards.  */

static int cursor_vpos;
static int cursor_hpos;

int debug_end_pos;

/* Nonzero means display mode line highlighted */
int mode_line_inverse_video;

static void echo_area_display ();
void mark_window_display_accurate ();
static void redisplay_windows ();
static void redisplay_window ();
static void try_window ();
static int try_window_id ();
static struct position *display_text_line ();
static void display_mode_line ();
static int display_mode_element ();
static char *fmodetrunc ();
static char *decode_mode_spec ();
static int display_string ();
static void display_menu_bar ();

/* Prompt to display in front of the minibuffer contents */
char *minibuf_prompt;

/* Width in columns of current minibuffer prompt.  */
int minibuf_prompt_width;

/* Message to display instead of minibuffer contents
   This is what the functions error and message make,
   and command echoing uses it as well.
   It overrides the minibuf_prompt as well as the buffer.  */
char *echo_area_glyphs;

/* true iff we should redraw the mode lines on the next redisplay */
int update_mode_lines;

/* Smallest number of characters before the gap
   at any time since last redisplay that finished.
   Valid for current buffer when try_window_id can be called.  */
int beg_unchanged;

/* Smallest number of characters after the gap
   at any time since last redisplay that finished.
   Valid for current buffer when try_window_id can be called.  */
int end_unchanged;

/* MODIFF as of last redisplay that finished;
   if it matches MODIFF, beg_unchanged and end_unchanged
   contain no useful information */
int unchanged_modified;

/* Nonzero if head_clip or tail_clip of current buffer has changed
   since last redisplay that finished */
int clip_changed;

/* Nonzero if window sizes or contents have changed
   since last redisplay that finished */
int windows_or_buffers_changed;


/* Specify m, a string, as a message in the minibuf.  If m is 0, clear out
   any existing message, and let the minibuffer text show through.  */
void
message1 (m)
     char *m;
{
  if (noninteractive)
    {
      if (noninteractive_need_newline)
	putc ('\n', stderr);
      noninteractive_need_newline = 0;
      fprintf (stderr, "%s\n", m);
      fflush (stderr);
    }
  /* A null message buffer means that the frame hasn't really been
     initialized yet.  Error messages get reported properly by
     cmd_error, so this must be just an informative message; toss it.  */
  else if (INTERACTIVE && FRAME_MESSAGE_BUF (selected_frame))
    {
#ifdef MULTI_FRAME
      Lisp_Object minibuf_frame;

      choose_minibuf_frame ();
      minibuf_frame = WINDOW_FRAME (XWINDOW (minibuf_window));
      FRAME_SAMPLE_VISIBILITY (XFRAME (minibuf_frame));
      if (FRAME_VISIBLE_P (selected_frame)
	  && ! FRAME_VISIBLE_P (XFRAME (minibuf_frame)))
	Fmake_frame_visible (WINDOW_FRAME (XWINDOW (minibuf_window)));
#endif

      if (m)
	echo_area_glyphs = m;
      else
	echo_area_glyphs = previous_echo_glyphs = 0;

      do_pending_window_change ();
      echo_area_display ();
      update_frame (XFRAME (XWINDOW (minibuf_window)->frame), 1, 1);
      do_pending_window_change ();
    }
}

/* Nonzero if FRAME_MESSAGE_BUF (selected_frame) is being used by print;
   zero if being used by message.  */
int message_buf_print;

/* Dump an informative message to the minibuf.  If m is 0, clear out
   any existing message, and let the minibuffer text show through.  */
/* VARARGS 1 */
void
message (m, a1, a2, a3)
     char *m;
{
  if (noninteractive)
    {
      if (m)
	{
	  if (noninteractive_need_newline)
	    putc ('\n', stderr);
	  noninteractive_need_newline = 0;
	  fprintf (stderr, m, a1, a2, a3);
	  fprintf (stderr, "\n");
	  fflush (stderr);
	}
    }
  else if (INTERACTIVE)
    {
      /* The frame whose minibuffer we're going to display the message on.
	 It may be larger than the selected frame, so we need
	 to use its buffer, not the selected frame's buffer.  */
      FRAME_PTR echo_frame;
#ifdef MULTI_FRAME
      choose_minibuf_frame ();
      echo_frame = XFRAME (WINDOW_FRAME (XWINDOW (minibuf_window)));
#else
      echo_frame = selected_frame;
#endif

      /* A null message buffer means that the frame hasn't really been
	 initialized yet.  Error messages get reported properly by
	 cmd_error, so this must be just an informative message; toss it.  */
      if (FRAME_MESSAGE_BUF (echo_frame))
	{
	  if (m)
	    {
	      {
#ifdef NO_ARG_ARRAY
		int a[3];
		a[0] = a1;
		a[1] = a2;
		a[2] = a3;

		doprnt (FRAME_MESSAGE_BUF (echo_frame),
			FRAME_WIDTH (echo_frame), m, 0, 3, a);
#else
		doprnt (FRAME_MESSAGE_BUF (echo_frame),
			FRAME_WIDTH (echo_frame), m, 0, 3, &a1);
#endif /* NO_ARG_ARRAY */
	      }

	      message1 (FRAME_MESSAGE_BUF (echo_frame));
	    }
	  else
	    message1 (0);

	  /* Print should start at the beginning of the message
	     buffer next time.  */
	  message_buf_print = 0;
	}
    }
}

static void
echo_area_display ()
{
  register int vpos;
  FRAME_PTR f;

#ifdef MULTI_FRAME
  choose_minibuf_frame ();
#endif

  f = XFRAME (WINDOW_FRAME (XWINDOW (minibuf_window)));

  if (! FRAME_VISIBLE_P (f))
    return;

  if (frame_garbaged)
    {
      Fredraw_display ();
      frame_garbaged = 0;
    }

  if (echo_area_glyphs || minibuf_level == 0)
    {
      vpos = XFASTINT (XWINDOW (minibuf_window)->top);
      get_display_line (f, vpos, 0);
      display_string (XWINDOW (minibuf_window), vpos,
		      echo_area_glyphs ? echo_area_glyphs : "",
		      0, 0, 0, FRAME_WIDTH (f));

      /* If desired cursor location is on this line, put it at end of text */
      if (FRAME_CURSOR_Y (f) == vpos)
	FRAME_CURSOR_X (f) = FRAME_DESIRED_GLYPHS (f)->used[vpos];

      /* Fill the rest of the minibuffer window with blank lines.  */
      {
	int i;

	for (i = vpos + 1; i < vpos + XWINDOW (minibuf_window)->height; i++)
	  {
	    get_display_line (f, i, 0);
	    display_string (XWINDOW (minibuf_window), vpos,
			    "", 0, 0, 0, FRAME_WIDTH (f));
	  }
      }
    }
  else if (!EQ (minibuf_window, selected_window))
    windows_or_buffers_changed++;

  if (EQ (minibuf_window, selected_window))
    this_line_bufpos = 0;

  previous_echo_glyphs = echo_area_glyphs;
}

/* Do a frame update, taking possible shortcuts into account.
   This is the main external entry point for redisplay.

   If the last redisplay displayed an echo area message and that
   message is no longer requested, we clear the echo area
   or bring back the minibuffer if that is in use.

   Everyone would like to have a hook here to call eval,
   but that cannot be done safely without a lot of changes elsewhere.
   This can be called from signal handlers; with alarms set up;
   or with synchronous processes running.
   See the function `echo' in keyboard.c.
   See Fcall_process; if you called it from here, it could be
   entered recursively.  */

void
redisplay ()
{
  register struct window *w = XWINDOW (selected_window);
  register int pause;
  int must_finish = 0;
  int all_windows;
  register int tlbufpos, tlendpos;
  struct position pos;
  extern int input_pending;

  if (noninteractive)
    return;

  /* Set the visible flags for all frames.
     Do this before checking for resized or garbaged frames; they want
     to know if their frames are visible.
     See the comment in frame.h for FRAME_SAMPLE_VISIBILITY.  */
  {
    Lisp_Object tail, frame;

    FOR_EACH_FRAME (tail, frame)
      FRAME_SAMPLE_VISIBILITY (XFRAME (frame));
  }

  /* Notice any pending interrupt request to change frame size.  */
  do_pending_window_change ();

  if (frame_garbaged)
    {
      Fredraw_display ();
      frame_garbaged = 0;
    }

  /* Normally the message* functions will have already displayed and
     updated the echo area, but the frame may have been trashed, or
     the update may have been preempted, so display the echo area
     again here.  */
  if (echo_area_glyphs || previous_echo_glyphs)
    {
      echo_area_display ();
      must_finish = 1;
    }

  if (clip_changed || windows_or_buffers_changed)
    update_mode_lines++;

  /* Detect case that we need to write a star in the mode line.  */
  if (XFASTINT (w->last_modified) < MODIFF
      && XFASTINT (w->last_modified) <= current_buffer->save_modified)
    {
      w->update_mode_line = Qt;
      if (buffer_shared > 1)
	update_mode_lines++;
    }

  FRAME_SCROLL_BOTTOM_VPOS (XFRAME (w->frame)) = -1;

  all_windows = update_mode_lines || buffer_shared > 1;

  /* If specs for an arrow have changed, do thorough redisplay
     to ensure we remove any arrow that should no longer exist.  */
  if (! EQ (Voverlay_arrow_position, last_arrow_position)
      || ! EQ (Voverlay_arrow_string, last_arrow_string))
    all_windows = 1, clip_changed = 1;

  tlbufpos = this_line_bufpos;
  tlendpos = this_line_endpos;
  if (!all_windows && tlbufpos > 0 && NILP (w->update_mode_line)
      && FRAME_VISIBLE_P (XFRAME (w->frame))
      /* Make sure recorded data applies to current buffer, etc */
      && this_line_buffer == current_buffer
      && current_buffer == XBUFFER (w->buffer)
      && NILP (w->force_start)
      /* Point must be on the line that we have info recorded about */
      && point >= tlbufpos
      && point <= Z - tlendpos
      /* All text outside that line, including its final newline,
	 must be unchanged */
      && (XFASTINT (w->last_modified) >= MODIFF
	  || (beg_unchanged >= tlbufpos - 1
	      && GPT >= tlbufpos
	      /* If selective display, can't optimize
		 if the changes start at the beginning of the line.  */
	      && ((XTYPE (current_buffer->selective_display) == Lisp_Int
		   && XINT (current_buffer->selective_display) > 0
		   ? (beg_unchanged >= tlbufpos
		      && GPT > tlbufpos)
		   : 1))
	      && end_unchanged >= tlendpos
	      && Z - GPT >= tlendpos)))
    {
      if (tlbufpos > BEGV && FETCH_CHAR (tlbufpos - 1) != '\n'
	  && (tlbufpos == ZV
	      || FETCH_CHAR (tlbufpos) == '\n'))
	/* Former continuation line has disappeared by becoming empty */
	goto cancel;
      else if (XFASTINT (w->last_modified) < MODIFF
	       || MINI_WINDOW_P (w))
	{
	  cursor_vpos = -1;
	  overlay_arrow_seen = 0;
	  display_text_line (w, tlbufpos, this_line_vpos, this_line_start_hpos,
			     pos_tab_offset (w, tlbufpos));
	  /* If line contains point, is not continued,
		 and ends at same distance from eob as before, we win */
	  if (cursor_vpos >= 0 && this_line_bufpos
	      && this_line_endpos == tlendpos)
	    {
	      if (XFASTINT (w->width) != FRAME_WIDTH (XFRAME (WINDOW_FRAME (w))))
		preserve_other_columns (w);
	      goto update;
	    }
	  else
	    goto cancel;
	}
      else if (point == XFASTINT (w->last_point))
	{
	  if (!must_finish)
	    {
	      do_pending_window_change ();
	      return;
	    }
	  goto update;
	}
      else
	{
	  pos = *compute_motion (tlbufpos, 0,
				 XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0,
				 point, 2, - (1 << (SHORTBITS - 1)),
				 window_internal_width (w) - 1,
				 XINT (w->hscroll),
				 pos_tab_offset (w, tlbufpos));
	  if (pos.vpos < 1)
	    {
	      FRAME_CURSOR_X (selected_frame)
		= XFASTINT (w->left) + max (pos.hpos, 0);
	      FRAME_CURSOR_Y (selected_frame) = this_line_vpos;
	      goto update;
	    }
	  else
	    goto cancel;
	}
    cancel:
      /* Text changed drastically or point moved off of line */
      cancel_line (this_line_vpos, selected_frame);
    }

  this_line_bufpos = 0;
  all_windows |= buffer_shared > 1;

  if (all_windows)
    {
      Lisp_Object tail, frame;

      /* Recompute # windows showing selected buffer.
	 This will be incremented each time such a window is displayed.  */
      buffer_shared = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  FRAME_PTR f = XFRAME (frame);

	  /* Mark all the scroll bars to be removed; we'll redeem the ones
	     we want when we redisplay their windows.  */
	  if (condemn_scroll_bars_hook)
	    (*condemn_scroll_bars_hook) (f);

	  if (FRAME_VISIBLE_P (f))
	    redisplay_windows (FRAME_ROOT_WINDOW (f));

	  /* Any scroll bars which redisplay_windows should have nuked
	     should now go away.  */
	  if (judge_scroll_bars_hook)
	    (*judge_scroll_bars_hook) (f);
	}
    }
  else if (FRAME_VISIBLE_P (selected_frame))
    {
      redisplay_window (selected_window, 1);
      if (XFASTINT (w->width) != FRAME_WIDTH (selected_frame))
	preserve_other_columns (w);
    }

update: 
  /* Prevent various kinds of signals during display update.
     stdio is not robust about handling signals,
     which can cause an apparent I/O error.  */
  if (interrupt_input)
    unrequest_sigio ();
  stop_polling ();

#ifdef MULTI_FRAME
  if (all_windows)
    {
      Lisp_Object tail;

      pause = 0;

      for (tail = Vframe_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  FRAME_PTR f;

	  if (XTYPE (XCONS (tail)->car) != Lisp_Frame)
	    continue;

	  f = XFRAME (XCONS (tail)->car);
	  if (FRAME_VISIBLE_P (f))
	    {
	      pause |= update_frame (f, 0, 0);
	      if (!pause)
		mark_window_display_accurate (f->root_window, 1);
	    }
	}
    }
  else
#endif /* MULTI_FRAME */
    {
      if (FRAME_VISIBLE_P (selected_frame))
	pause = update_frame (selected_frame, 0, 0);

      /* We may have called echo_area_display at the top of this
	 function.  If the echo area is on another frame, that may
	 have put text on a frame other than the selected one, so the
	 above call to update_frame would not have caught it.  Catch
	 it here.  */
      {
	FRAME_PTR mini_frame =
	  XFRAME (WINDOW_FRAME (XWINDOW (minibuf_window)));
	
	if (mini_frame != selected_frame)
	  pause |= update_frame (mini_frame, 0, 0);
      }
    }

  /* If frame does not match, prevent doing single-line-update next time.
     Also, don't forget to check every line to update the arrow.  */
  if (pause)
    {
      this_line_bufpos = 0;
      if (!NILP (last_arrow_position))
	{
	  last_arrow_position = Qt;
	  last_arrow_string = Qt;
	}
      /* If we pause after scrolling, some lines in current_frame
	 may be null, so preserve_other_columns won't be able to
	 preserve all the vertical-bar separators.  So, avoid using it
	 in that case.  */
      if (XFASTINT (w->width) != FRAME_WIDTH (selected_frame))
	update_mode_lines = 1;
    }

  /* Now text on frame agrees with windows, so
     put info into the windows for partial redisplay to follow */

  if (!pause)
    {
      register struct buffer *b = XBUFFER (w->buffer);

      blank_end_of_window = 0;
      clip_changed = 0;
      unchanged_modified = BUF_MODIFF (b);
      beg_unchanged = BUF_GPT (b) - BUF_BEG (b);
      end_unchanged = BUF_Z (b) - BUF_GPT (b);

      XFASTINT (w->last_point) = BUF_PT (b);
      XFASTINT (w->last_point_x) = FRAME_CURSOR_X (selected_frame);
      XFASTINT (w->last_point_y) = FRAME_CURSOR_Y (selected_frame);

      if (all_windows)
	mark_window_display_accurate (FRAME_ROOT_WINDOW (selected_frame), 1);
      else
	{
	  w->update_mode_line = Qnil;
	  XFASTINT (w->last_modified) = BUF_MODIFF (b);
	  w->window_end_valid = Qt;
	  last_arrow_position = Voverlay_arrow_position;
	  last_arrow_string = Voverlay_arrow_string;
	}
      update_mode_lines = 0;
      windows_or_buffers_changed = 0;
    }

  /* Start SIGIO interrupts coming again.
     Having them off during the code above
     makes it less likely one will discard output,
     but not impossible, since there might be stuff
     in the system buffer here.
     But it is much hairier to try to do anything about that.  */

  if (interrupt_input)
    request_sigio ();
  start_polling ();

  /* Change frame size now if a change is pending.  */
  do_pending_window_change ();
}

/* Redisplay, but leave alone any recent echo area message
   unless another message has been requested in its place.

   This is useful in situations where you need to redisplay but no
   user action has occurred, making it inappropriate for the message
   area to be cleared.  See tracking_off and
   wait_reading_process_input for examples of these situations.  */

redisplay_preserve_echo_area ()
{
  if (echo_area_glyphs == 0 && previous_echo_glyphs != 0)
    {
      echo_area_glyphs = previous_echo_glyphs;
      redisplay ();
      echo_area_glyphs = 0;
    }
  else
    redisplay ();
}

void
mark_window_display_accurate (window, flag)
     Lisp_Object window;
     int flag;
{
  register struct window *w;

  for (;!NILP (window); window = w->next)
    {
      if (XTYPE (window) != Lisp_Window) abort ();
      w = XWINDOW (window);

      if (!NILP (w->buffer))
	XFASTINT (w->last_modified)
	  = !flag ? 0
	    : XBUFFER (w->buffer) == current_buffer
	      ? MODIFF : BUF_MODIFF (XBUFFER (w->buffer));
      w->window_end_valid = Qt;
      w->update_mode_line = Qnil;

      if (!NILP (w->vchild))
	mark_window_display_accurate (w->vchild, flag);
      if (!NILP (w->hchild))
	mark_window_display_accurate (w->hchild, flag);
    }

  if (flag)
    {
      last_arrow_position = Voverlay_arrow_position;
      last_arrow_string = Voverlay_arrow_string;
    }
  else
    {
      /* t is unequal to any useful value of Voverlay_arrow_... */
      last_arrow_position = Qt;
      last_arrow_string = Qt;
    }
}

int do_id = 1;

static void
redisplay_windows (window)
     Lisp_Object window;
{
  for (; !NILP (window); window = XWINDOW (window)->next)
    redisplay_window (window, 0);
}

static void
redisplay_window (window, just_this_one)
     Lisp_Object window;
     int just_this_one;
{
  register struct window *w = XWINDOW (window);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  int height;
  register int lpoint = point;
  struct buffer *old = current_buffer;
  register int width = window_internal_width (w) - 1;
  register int startp;
  register int hscroll = XINT (w->hscroll);
  struct position pos;
  int opoint = point;
  int tem;
  int window_needs_modeline;

  if (FRAME_HEIGHT (f) == 0) abort (); /* Some bug zeros some core */

  /* If this is a combination window, do its children; that's all.  */

  if (!NILP (w->vchild))
    {
      redisplay_windows (w->vchild);
      return;
    }
  if (!NILP (w->hchild))
    {
      redisplay_windows (w->hchild);
      return;
    }
  if (NILP (w->buffer))
    abort ();
  
  height = window_internal_height (w);

  if (MINI_WINDOW_P (w))
    {
      if (w == XWINDOW (minibuf_window))
	{
	  if (echo_area_glyphs)
	    /* We've already displayed the echo area glyphs, if any.  */
	    goto finish_scroll_bars;
	}
      else
	{
	  /* This is a minibuffer, but it's not the currently active one, so
	     clear it.  */
	  int vpos = XFASTINT (XWINDOW (FRAME_MINIBUF_WINDOW (f))->top);
	  int i;

	  for (i = 0; i < height; i++)
	    {
	      get_display_line (f, vpos + i, 0);
	      display_string (w, vpos + i, "", 0, 0, 0, width);
	    }
	  
	  goto finish_scroll_bars;
	}
    }

  if (update_mode_lines)
    w->update_mode_line = Qt;

  /* Otherwise set up data on this window; select its buffer and point value */

  current_buffer = XBUFFER (w->buffer);
  opoint = point;

  /* Count number of windows showing the selected buffer.  */

  if (!just_this_one
      && current_buffer == XBUFFER (XWINDOW (selected_window)->buffer))
    buffer_shared++;

  /* POINT refers normally to the selected window.
     For any other window, set up appropriate value.  */

  if (!EQ (window, selected_window))
    {
      SET_PT (marker_position (w->pointm));
      if (point < BEGV)
	{
	  SET_PT (BEGV);
	  Fset_marker (w->pointm, make_number (point), Qnil);
	}
      else if (point > (ZV - 1))
	{
	  SET_PT (ZV);
	  Fset_marker (w->pointm, make_number (point), Qnil);
	}
    }

  /* If window-start is screwed up, choose a new one.  */
  if (XMARKER (w->start)->buffer != current_buffer)
    goto recenter;

  startp = marker_position (w->start);

  /* Handle case where place to start displaying has been specified,
     unless the specified location is outside the visible range.  */
  if (!NILP (w->force_start))
    {
      w->update_mode_line = Qt;
      w->force_start = Qnil;
      XFASTINT (w->last_modified) = 0;
      if (startp < BEGV) startp = BEGV;
      if (startp > ZV)   startp = ZV;
      try_window (window, startp);
      if (cursor_vpos < 0)
	{
	  /* If point does not appear, move point so it does appear */
	  pos = *compute_motion (startp, 0,
				((EQ (window, minibuf_window) && startp == 1)
				 ? minibuf_prompt_width : 0)
				+
				(hscroll ? 1 - hscroll : 0),
				ZV, height / 2,
				- (1 << (SHORTBITS - 1)),
				width, hscroll, pos_tab_offset (w, startp));
	  SET_PT (pos.bufpos);
	  if (w != XWINDOW (FRAME_SELECTED_WINDOW (f)))
	    Fset_marker (w->pointm, make_number (point), Qnil);
	  else
	    {
	      lpoint = point;
	      FRAME_CURSOR_X (f) = max (0, pos.hpos) + XFASTINT (w->left);
	      FRAME_CURSOR_Y (f) = pos.vpos + XFASTINT (w->top);
	    }
	}
      goto done;
    }

  /* Handle case where text has not changed, only point,
     and it has not moved off the frame */

  /* This code is not used for minibuffer for the sake of
     the case of redisplaying to replace an echo area message;
     since in that case the minibuffer contents per se are usually unchanged.
     This code is of no real use in the minibuffer since
     the handling of this_line_bufpos, etc.,
     in redisplay handles the same cases.  */

  if (XFASTINT (w->last_modified) >= MODIFF
      && point >= startp && !clip_changed
      && (just_this_one || XFASTINT (w->width) == FRAME_WIDTH (f))
      && !EQ (window, minibuf_window))
    {
      pos = *compute_motion (startp, 0, (hscroll ? 1 - hscroll : 0),
			    point, height + 1, 10000, width, hscroll,
			    pos_tab_offset (w, startp));

      if (pos.vpos < height)
	{
	  /* Ok, point is still on frame */
	  if (w == XWINDOW (FRAME_SELECTED_WINDOW (f)))
	    {
	      /* These variables are supposed to be origin 1 */
	      FRAME_CURSOR_X (f) = max (0, pos.hpos) + XFASTINT (w->left);
	      FRAME_CURSOR_Y (f) = pos.vpos + XFASTINT (w->top);
	    }
	  /* This doesn't do the trick, because if a window to the right of
	     this one must be redisplayed, this does nothing because there
	     is nothing in DesiredFrame yet, and then the other window is
	     redisplayed, making likes that are empty in this window's columns.
	     if (XFASTINT (w->width) != FRAME_WIDTH (f))
	     preserve_my_columns (w);
	     */
	  goto done;
	}
      /* Don't bother trying redisplay with same start;
	we already know it will lose */
    }
  /* If current starting point was originally the beginning of a line
     but no longer is, find a new starting point.  */
  else if (!NILP (w->start_at_line_beg)
	   && !(startp == BEGV
		|| FETCH_CHAR (startp - 1) == '\n'))
    {
      goto recenter;
    }
  else if (just_this_one && !MINI_WINDOW_P (w)
	   && point >= startp
	   && XFASTINT (w->last_modified)
	   && ! EQ (w->window_end_valid, Qnil)
	   && do_id && !clip_changed
	   && !blank_end_of_window
	   && XFASTINT (w->width) == FRAME_WIDTH (f)
	   && EQ (last_arrow_position, Voverlay_arrow_position)
	   && EQ (last_arrow_string, Voverlay_arrow_string)
	   && (tem = try_window_id (FRAME_SELECTED_WINDOW (f)))
	   && tem != -2)
    {
      /* tem > 0 means success.  tem == -1 means choose new start.
	 tem == -2 means try again with same start,
	  and nothing but whitespace follows the changed stuff.
	 tem == 0 means try again with same start.  */
      if (tem > 0)
	goto done;
    }
  else if (startp >= BEGV && startp <= ZV
	   /* Avoid starting display at end of buffer! */
	   && (startp < ZV || startp == BEGV
	       || (XFASTINT (w->last_modified) >= MODIFF)))
    {
      /* Try to redisplay starting at same place as before */
      /* If point has not moved off frame, accept the results */
      try_window (window, startp);
      if (cursor_vpos >= 0)
	goto done;
      else
	cancel_my_columns (w);
    }

  XFASTINT (w->last_modified) = 0;
  w->update_mode_line = Qt;

  /* Try to scroll by specified few lines */

  if (scroll_step && !clip_changed)
    {
      if (point > startp)
	{
	  pos = *vmotion (Z - XFASTINT (w->window_end_pos),
			  scroll_step, width, hscroll, window);
	  if (pos.vpos >= height)
	    goto scroll_fail;
	}

      pos = *vmotion (startp, point < startp ? - scroll_step : scroll_step,
		      width, hscroll, window);

      if (point >= pos.bufpos)
	{
	  try_window (window, pos.bufpos);
	  if (cursor_vpos >= 0)
	    goto done;
	  else
	    cancel_my_columns (w);
	}
    scroll_fail: ;
    }

  /* Finally, just choose place to start which centers point */

recenter:
  pos = *vmotion (point, - height / 2, width, hscroll, window);
  try_window (window, pos.bufpos);

  startp = marker_position (w->start);
  w->start_at_line_beg = 
    (startp == BEGV || FETCH_CHAR (startp - 1) == '\n') ? Qt : Qnil;

done:
  /* If window not full width, must redo its mode line
     if the window to its side is being redone */
  if ((!NILP (w->update_mode_line)
       || (!just_this_one && width < FRAME_WIDTH (f) - 1))
      && height != XFASTINT (w->height))
    display_mode_line (w);

  /* When we reach a frame's selected window, redo the frame's menu bar.  */
  if (!NILP (w->update_mode_line)
      && FRAME_MENU_BAR_LINES (f) > 0
      && EQ (FRAME_SELECTED_WINDOW (f), window))
    display_menu_bar (w);

 finish_scroll_bars:
  if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    {
      int start, end, whole;

      /* Calculate the start and end positions for the current window.
	 Note that minibuffers sometimes aren't displaying any text.  */
      if (! MINI_WINDOW_P (w)
	  || (w == XWINDOW (minibuf_window) && ! echo_area_glyphs))
	{
	  start = startp;
	  /* I don't think this is guaranteed to be right.  For the
	     moment, we'll pretend it is.  */
	  end = Z - XINT (w->window_end_pos);
	  whole = Z - BEG;
	}
      else
	start = end = whole = 0;

      /* Indicate what this scroll bar ought to be displaying now.  */
      (*set_vertical_scroll_bar_hook) (w, end - start, whole, start - 1);

      /* Note that we actually used the scroll bar attached to this window,
	 so it shouldn't be deleted at the end of redisplay.  */
      (*redeem_scroll_bar_hook) (w);
    }

  SET_PT (opoint);
  current_buffer = old;
  SET_PT (lpoint);
}

/* Do full redisplay on one window, starting at position `pos'. */

static void
try_window (window, pos)
     Lisp_Object window;
     register int pos;
{
  register struct window *w = XWINDOW (window);
  register int height = window_internal_height (w);
  register int vpos = XFASTINT (w->top);
  register int last_text_vpos = vpos;
  int tab_offset = pos_tab_offset (w, pos);
  FRAME_PTR f = XFRAME (w->frame);
  int width = window_internal_width (w) - 1;
  struct position val;

  Fset_marker (w->start, make_number (pos), Qnil);
  cursor_vpos = -1;
  overlay_arrow_seen = 0;
  val.hpos = XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0;

  while (--height >= 0)
    {
      val = *display_text_line (w, pos, vpos, val.hpos, tab_offset);
      tab_offset += width;
      if (val.vpos) tab_offset = 0;
      vpos++;
      if (pos != val.bufpos)
	last_text_vpos
	  /* Next line, unless prev line ended in end of buffer with no cr */
	  = vpos - (val.vpos && FETCH_CHAR (val.bufpos - 1) != '\n');
      pos = val.bufpos;
    }

  /* If last line is continued in middle of character,
     include the split character in the text considered on the frame */
  if (val.hpos < (XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0))
    pos++;

  /* If bottom just moved off end of frame, change mode line percentage.  */
  if (XFASTINT (w->window_end_pos) == 0
      && Z != pos)
    w->update_mode_line = Qt;

  /* Say where last char on frame will be, once redisplay is finished.  */
  XFASTINT (w->window_end_pos) = Z - pos;
  XFASTINT (w->window_end_vpos) = last_text_vpos - XFASTINT (w->top);
  /* But that is not valid info until redisplay finishes.  */
  w->window_end_valid = Qnil;
}

/* Try to redisplay when buffer is modified locally,
 computing insert/delete line to preserve text outside
 the bounds of the changes.
 Return 1 if successful, 0 if if cannot tell what to do,
 or -1 to tell caller to find a new window start,
 or -2 to tell caller to do normal redisplay with same window start.  */

static int
try_window_id (window)
     Lisp_Object window;
{
  int pos;
  register struct window *w = XWINDOW (window);
  register int height = window_internal_height (w);
  FRAME_PTR f = XFRAME (w->frame);
  int top = XFASTINT (w->top);
  int start = marker_position (w->start);
  int width = window_internal_width (w) - 1;
  int hscroll = XINT (w->hscroll);
  int lmargin = hscroll > 0 ? 1 - hscroll : 0;
  register int vpos;
  register int i, tem;
  int last_text_vpos = 0;
  int stop_vpos;

  struct position val, bp, ep, xp, pp;
  int scroll_amount = 0;
  int delta;
  int tab_offset, epto;

  if (GPT - BEG < beg_unchanged)
    beg_unchanged = GPT - BEG;
  if (Z - GPT < end_unchanged)
    end_unchanged = Z - GPT;

  if (beg_unchanged + 1 < start)
    return 0;			/* Give up if changes go above top of window */

  /* Find position before which nothing is changed.  */
  bp = *compute_motion (start, 0, lmargin,
			beg_unchanged + 1, height + 1, 0, width, hscroll,
			pos_tab_offset (w, start));
  if (bp.vpos >= height)
    {
      if (point < bp.bufpos && !bp.contin)
	{
	  /* All changes are below the frame, and point is on the frame.
	     We don't need to change the frame at all.
	     But we need to update window_end_pos to account for
	     any change in buffer size.  */
	  bp = *compute_motion (start, 0, lmargin,
				Z, height, 0,
				width, hscroll, pos_tab_offset (w, start));
	  XFASTINT (w->window_end_vpos) = height;
	  XFASTINT (w->window_end_pos) = Z - bp.bufpos;
	  return 1;
	}
      return 0;
    }

  vpos = bp.vpos;

  /* Find beginning of that frame line.  Must display from there.  */
  bp = *vmotion (bp.bufpos, 0, width, hscroll, window);

  pos = bp.bufpos;
  val.hpos = lmargin;
  if (pos < start)
    return -1;

  /* If about to start displaying at the beginning of a continuation line,
     really start with previous frame line, in case it was not
     continued when last redisplayed */
  if ((bp.contin && bp.bufpos - 1 == beg_unchanged && vpos > 0)
      ||
      /* Likewise if we have to worry about selective display.  */
      (XTYPE (current_buffer->selective_display) == Lisp_Int
       && XINT (current_buffer->selective_display) > 0
       && bp.bufpos - 1 == beg_unchanged && vpos > 0))
    {
      bp = *vmotion (bp.bufpos, -1, width, hscroll, window);
      --vpos;
      pos = bp.bufpos;
    }

  if (bp.contin && bp.hpos != lmargin)
    {
      val.hpos = bp.prevhpos - width + lmargin;
      pos--;
    }

  bp.vpos = vpos;

  /* Find first visible newline after which no more is changed.  */
  tem = find_next_newline (Z - max (end_unchanged, Z - ZV), 1);
  if (XTYPE (current_buffer->selective_display) == Lisp_Int
      && XINT (current_buffer->selective_display) > 0)
    while (tem < ZV - 1
	   && (position_indentation (tem)
	       >= XINT (current_buffer->selective_display)))
      tem = find_next_newline (tem, 1);

  /* Compute the cursor position after that newline.  */
  ep = *compute_motion (pos, vpos, val.hpos, tem,
			height, - (1 << (SHORTBITS - 1)),
			width, hscroll, pos_tab_offset (w, bp.bufpos));

  /* If changes reach past the text available on the frame,
     just display rest of frame.  */
  if (ep.bufpos > Z - XFASTINT (w->window_end_pos))
    stop_vpos = height;
  else
    stop_vpos = ep.vpos;

  /* If no newline before ep, the line ep is on includes some changes
     that must be displayed.  Make sure we don't stop before it.  */
  /* Also, if changes reach all the way until ep.bufpos,
     it is possible that something was deleted after the
     newline before it, so the following line must be redrawn. */
  if (stop_vpos == ep.vpos
      && (ep.bufpos == BEGV
	  || FETCH_CHAR (ep.bufpos - 1) != '\n'
	  || ep.bufpos == Z - end_unchanged))
    stop_vpos = ep.vpos + 1;

  cursor_vpos = -1;
  overlay_arrow_seen = 0;

  /* If changes do not reach to bottom of window,
     figure out how much to scroll the rest of the window */
  if (stop_vpos < height)
    {
      /* Now determine how far up or down the rest of the window has moved */
      epto = pos_tab_offset (w, ep.bufpos);
      xp = *compute_motion (ep.bufpos, ep.vpos, ep.hpos,
			    Z - XFASTINT (w->window_end_pos),
			    10000, 0, width, hscroll, epto);
      scroll_amount = xp.vpos - XFASTINT (w->window_end_vpos);

      /* Is everything on frame below the changes whitespace?
	 If so, no scrolling is really necessary.  */
      for (i = ep.bufpos; i < xp.bufpos; i++)
	{
	  tem = FETCH_CHAR (i);
	  if (tem != ' ' && tem != '\n' && tem != '\t')
	    break;
	}
      if (i == xp.bufpos)
	return -2;

      XFASTINT (w->window_end_vpos) += scroll_amount;

      /* Before doing any scrolling, verify that point will be on frame. */
      if (point > ep.bufpos && !(point <= xp.bufpos && xp.bufpos < height))
	{
	  if (point <= xp.bufpos)
	    {
	      pp = *compute_motion (ep.bufpos, ep.vpos, ep.hpos,
				    point, height, - (1 << (SHORTBITS - 1)),
				    width, hscroll, epto);
	    }
	  else
	    {
	      pp = *compute_motion (xp.bufpos, xp.vpos, xp.hpos,
				    point, height, - (1 << (SHORTBITS - 1)),
				    width, hscroll, pos_tab_offset (w, xp.bufpos));
	    }
	  if (pp.bufpos < point || pp.vpos == height)
	    return 0;
	  cursor_vpos = pp.vpos + top;
	  cursor_hpos = pp.hpos + XFASTINT (w->left);
	}

      if (stop_vpos - scroll_amount >= height
	  || ep.bufpos == xp.bufpos)
	{
	  if (scroll_amount < 0)
	    stop_vpos -= scroll_amount;
	  scroll_amount = 0;
	  /* In this path, we have altered window_end_vpos
	     and not left it negative.
	     We must make sure that, in case display is preempted
	     before the frame changes to reflect what we do here,
	     further updates will not come to try_window_id
	     and assume the frame and window_end_vpos match.  */
	  blank_end_of_window = 1;
	}
      else if (!scroll_amount)
	{}
      else if (bp.bufpos == Z - end_unchanged)
	{
	  /* If reprinting everything is nearly as fast as scrolling,
	     don't bother scrolling.  Can happen if lines are short.  */
	  if (scroll_cost (f, bp.vpos + top - scroll_amount,
			   top + height - max (0, scroll_amount),
			   scroll_amount)
	      > xp.bufpos - bp.bufpos - 20)
	    /* Return "try normal display with same window-start."
	       Too bad we can't prevent further scroll-thinking.  */
	    return -2;
	  /* If pure deletion, scroll up as many lines as possible.
	     In common case of killing a line, this can save the
	     following line from being overwritten by scrolling
	     and therefore having to be redrawn.  */
	  tem = scroll_frame_lines (f, bp.vpos + top - scroll_amount,
				     top + height - max (0, scroll_amount),
				     scroll_amount);
	  if (!tem) stop_vpos = height;
	}
      else if (scroll_amount)
	{
	  /* If reprinting everything is nearly as fast as scrolling,
	     don't bother scrolling.  Can happen if lines are short.  */
	  /* Note that if scroll_amount > 0, xp.bufpos - bp.bufpos is an
	     overestimate of cost of reprinting, since xp.bufpos
	     would end up below the bottom of the window.  */
	  if (scroll_cost (f, ep.vpos + top - scroll_amount,
			   top + height - max (0, scroll_amount),
			   scroll_amount)
	      > xp.bufpos - ep.bufpos - 20)
	    /* Return "try normal display with same window-start."
	       Too bad we can't prevent further scroll-thinking.  */
	    return -2;
	  tem = scroll_frame_lines (f, ep.vpos + top - scroll_amount,
				     top + height - max (0, scroll_amount),
				     scroll_amount);
	  if (!tem) stop_vpos = height;
	}
    }

  /* In any case, do not display past bottom of window */
  if (stop_vpos >= height)
    {
      stop_vpos = height;
      scroll_amount = 0;
    }

  /* Handle case where pos is before w->start --
     can happen if part of line had been clipped and is not clipped now */
  if (vpos == 0 && pos < marker_position (w->start))
    Fset_marker (w->start, make_number (pos), Qnil);

  /* Redisplay the lines where the text was changed */
  last_text_vpos = vpos;
  tab_offset = pos_tab_offset (w, pos);
  /* If we are starting display in mid-character, correct tab_offset
     to account for passing the line that that character really starts in.  */
  if (val.hpos < lmargin)
    tab_offset += width;
  while (vpos < stop_vpos)
    {
      val = *display_text_line (w, pos, top + vpos++, val.hpos, tab_offset);
      tab_offset += width;
      if (val.vpos) tab_offset = 0;
      if (pos != val.bufpos)
	last_text_vpos
	  /* Next line, unless prev line ended in end of buffer with no cr */
	    = vpos - (val.vpos && FETCH_CHAR (val.bufpos - 1) != '\n');
      pos = val.bufpos;
    }

  /* There are two cases:
     1) we have displayed down to the bottom of the window
     2) we have scrolled lines below stop_vpos by scroll_amount  */

  if (vpos == height)
    {
      /* If last line is continued in middle of character,
	 include the split character in the text considered on the frame */
      if (val.hpos < lmargin)
	val.bufpos++;
      XFASTINT (w->window_end_vpos) = last_text_vpos;
      XFASTINT (w->window_end_pos) = Z - val.bufpos;
    }

  /* If scrolling made blank lines at window bottom,
     redisplay to fill those lines */
  if (scroll_amount < 0)
    {
      /* Don't consider these lines for general-purpose scrolling.
	 That will save time in the scrolling computation.  */
      FRAME_SCROLL_BOTTOM_VPOS (f) = xp.vpos;
      vpos = xp.vpos;
      pos = xp.bufpos;
      val.hpos = lmargin;
      if (pos == ZV)
	vpos = height + scroll_amount;
      else if (xp.contin && xp.hpos != lmargin)
	{
	  val.hpos = xp.prevhpos - width + lmargin;
	  pos--;
	}

      blank_end_of_window = 1;
      tab_offset = pos_tab_offset (w, pos);
      /* If we are starting display in mid-character, correct tab_offset
	 to account for passing the line that that character starts in.  */
      if (val.hpos < lmargin)
	tab_offset += width;

      while (vpos < height)
	{
	  val = *display_text_line (w, pos, top + vpos++, val.hpos, tab_offset);
	  tab_offset += width;
	  if (val.vpos) tab_offset = 0;
	  pos = val.bufpos;
	}

      /* Here is a case where display_text_line sets cursor_vpos wrong.
	 Make it be fixed up, below.  */
      if (xp.bufpos == ZV
	  && xp.bufpos == point)
	cursor_vpos = -1;
    }

  /* If bottom just moved off end of frame, change mode line percentage.  */
  if (XFASTINT (w->window_end_pos) == 0
      && Z != val.bufpos)
    w->update_mode_line = Qt;

  /* Attempt to adjust end-of-text positions to new bottom line */
  if (scroll_amount)
    {
      delta = height - xp.vpos;
      if (delta < 0
	  || (delta > 0 && xp.bufpos <= ZV)
	  || (delta == 0 && xp.hpos))
	{
	  val = *vmotion (Z - XFASTINT (w->window_end_pos),
			  delta, width, hscroll, window);
	  XFASTINT (w->window_end_pos) = Z - val.bufpos;
	  XFASTINT (w->window_end_vpos) += val.vpos;
	}
    }

  w->window_end_valid = Qnil;

  /* If point was not in a line that was displayed, find it */
  if (cursor_vpos < 0)
    {
      val = *compute_motion (start, 0, lmargin, point, 10000, 10000,
			     width, hscroll, pos_tab_offset (w, start));
      /* Admit failure if point is off frame now */
      if (val.vpos >= height)
	{
	  for (vpos = 0; vpos < height; vpos++)
	    cancel_line (vpos + top, f);
	  return 0;
	}
      cursor_vpos = val.vpos + top;
      cursor_hpos = val.hpos + XFASTINT (w->left);
    }

  FRAME_CURSOR_X (f) = max (0, cursor_hpos);
  FRAME_CURSOR_Y (f) = cursor_vpos;

  if (debug_end_pos)
    {
      val = *compute_motion (start, 0, lmargin, ZV,
			     height, - (1 << (SHORTBITS - 1)),
			     width, hscroll, pos_tab_offset (w, start));
      if (val.vpos != XFASTINT (w->window_end_vpos))
	abort ();
      if (XFASTINT (w->window_end_pos)
	  != Z - val.bufpos)
	abort ();
    }

  return 1;
}

/* Copy glyphs from the vector FROM to the rope T.
   But don't actually copy the parts that would come in before S.
   Value is T, advanced past the copied data.  */

GLYPH *
copy_rope (t, s, from)
     register GLYPH *t; /* Copy to here. */
     register GLYPH *s; /* Starting point. */
     Lisp_Object from;    /* Data to copy; known to be a vector.  */
{
  register int n = XVECTOR (from)->size;
  register Lisp_Object *f = XVECTOR (from)->contents;

  while (n--)
    {
      if (t >= s) *t = *f;
      ++t;
      ++f;
    }
  return t;
}

/* Similar but copy at most LEN glyphs.  */

GLYPH *
copy_part_of_rope (t, s, from, len)
     register GLYPH *t; /* Copy to here. */
     register GLYPH *s; /* Starting point. */
     Lisp_Object from;    /* Data to copy; known to be a vector.  */
     int len;
{
  register int n = XVECTOR (from)->size;
  register Lisp_Object *f = XVECTOR (from)->contents;

  if (n > len)
    n = len;

  while (n--)
    {
      if (t >= s) *t = *f;
      ++t;
      ++f;
    }
  return t;
}

/* Display one line of window w, starting at position START in W's buffer.
   Display starting at horizontal position HPOS, which is normally zero
   or negative.  A negative value causes output up to hpos = 0 to be discarded.
   This is done for negative hscroll, or when this is a continuation line
   and the continuation occurred in the middle of a multi-column character.

   TABOFFSET is an offset for ostensible hpos, used in tab stop calculations.

   Display on position VPOS on the frame.  (origin 0).

   Returns a STRUCT POSITION giving character to start next line with
   and where to display it, including a zero or negative hpos.
   The vpos field is not really a vpos; it is 1 unless the line is continued */

struct position val_display_text_line;

static struct position *
display_text_line (w, start, vpos, hpos, taboffset)
     struct window *w;
     int start;
     int vpos;
     int hpos;
     int taboffset;
{
  register int pos = start;
  register int c;
  register GLYPH *p1;
  int end;
  register int pause;
  register unsigned char *p;
  GLYPH *endp;
  register GLYPH *startp;
  register GLYPH *p1prev;
  FRAME_PTR f = XFRAME (w->frame);
  int tab_width = XINT (current_buffer->tab_width);
  int ctl_arrow = !NILP (current_buffer->ctl_arrow);
  int width = window_internal_width (w) - 1;
  struct position val;
  int lastpos;
  int invis;
  int hscroll = XINT (w->hscroll);
  int truncate = hscroll
    || (truncate_partial_width_windows
	&& XFASTINT (w->width) < FRAME_WIDTH (f))
    || !NILP (current_buffer->truncate_lines);
  int selective
    = XTYPE (current_buffer->selective_display) == Lisp_Int
      ? XINT (current_buffer->selective_display)
	: !NILP (current_buffer->selective_display) ? -1 : 0;
#ifndef old
  int selective_e = selective && !NILP (current_buffer->selective_display_ellipses);
#endif
  register struct frame_glyphs *desired_glyphs = FRAME_DESIRED_GLYPHS (f);
  register struct Lisp_Vector *dp = window_display_table (w);
  int selective_rlen
    = (selective && dp && XTYPE (DISP_INVIS_VECTOR (dp)) == Lisp_Vector
       ? XVECTOR (DISP_INVIS_VECTOR (dp))->size : 0);
  GLYPH truncator = (dp == 0 || XTYPE (DISP_TRUNC_GLYPH (dp)) != Lisp_Int
		    ? '$' : XINT (DISP_TRUNC_GLYPH (dp)));
  GLYPH continuer = (dp == 0 || XTYPE (DISP_CONTINUE_GLYPH (dp)) != Lisp_Int
		    ? '\\' : XINT (DISP_CONTINUE_GLYPH (dp)));

  hpos += XFASTINT (w->left);
  get_display_line (f, vpos, XFASTINT (w->left));
  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  if (MINI_WINDOW_P (w) && start == 1
      && vpos == XFASTINT (w->top))
    {
      if (minibuf_prompt)
	hpos = display_string (w, vpos, minibuf_prompt, hpos,
			       (!truncate ? continuer : truncator),
			       -1, -1);
      minibuf_prompt_width = hpos;
    }

  desired_glyphs->bufp[vpos] = pos;
  p1 = desired_glyphs->glyphs[vpos] + hpos;
  end = ZV;
  startp = desired_glyphs->glyphs[vpos] + XFASTINT (w->left);
  endp = startp + width;

  /* Loop generating characters.
     Stop at end of buffer, before newline,
     or if reach or pass continuation column.  */

  pause = pos;
  while (p1 < endp)
    {
      p1prev = p1;
      if (pos == pause)
	{
	  if (pos == end)
	    break;
	  if (pos == point && cursor_vpos < 0)
	    {
	      cursor_vpos = vpos;
	      cursor_hpos = p1 - startp;
	    }

	  pause = end;
	  if (pos < point && point < pause)
	    pause = point;
	  if (pos < GPT && GPT < pause)
	    pause = GPT;

	  p = &FETCH_CHAR (pos);
	}
      c = *p++;
      if (c >= 040 && c < 0177
	  && (dp == 0 || XTYPE (DISP_CHAR_VECTOR (dp, c)) != Lisp_Vector))
	{
	  if (p1 >= startp)
	    *p1 = c;
	  p1++;
	}
      else if (c == '\n')
	{
	  invis = 0;
	  while (pos < end
		 && selective > 0
		 && position_indentation (pos + 1) >= selective)
	    {
	      invis = 1;
	      pos = find_next_newline (pos + 1, 1);
	      if (FETCH_CHAR (pos - 1) == '\n')
		pos--;
	    }
	  if (invis && selective_rlen > 0 && p1 >= startp)
	    {
	      p1 += selective_rlen;
	      if (p1 - startp > width)
		p1 = endp;
	      copy_part_of_rope (p1prev, p1prev,
				 XVECTOR (DISP_INVIS_VECTOR (dp))->contents,
				 (p1 - p1prev));
	    }
	  break;
	}
      else if (c == '\t')
	{
	  do
	    {
	      if (p1 >= startp && p1 < endp)
		*p1 = SPACEGLYPH;
	      p1++;
	    }
	  while ((p1 - startp + taboffset + hscroll - (hscroll > 0))
		 % tab_width);
	}
      else if (c == Ctl ('M') && selective == -1)
	{
	  pos = find_next_newline (pos, 1);
	  if (FETCH_CHAR (pos - 1) == '\n')
	    pos--;
	  if (selective_rlen > 0)
	    {
	      p1 += selective_rlen;
	      if (p1 - startp > width)
		p1 = endp;
	      copy_part_of_rope (p1prev, p1prev,
				 XVECTOR(DISP_INVIS_VECTOR (dp))->contents,
				 (p1 - p1prev));
	    }
	  break;
	}
      else if (dp != 0 && XTYPE (DISP_CHAR_VECTOR (dp, c)) == Lisp_Vector)
	{
	  p1 = copy_rope (p1, startp, DISP_CHAR_VECTOR (dp, c));
	}
      else if (c < 0200 && ctl_arrow)
	{
	  if (p1 >= startp)
	    *p1 = (dp && XTYPE (DISP_CTRL_GLYPH (dp)) == Lisp_Int
		   ? XINT (DISP_CTRL_GLYPH (dp)) : '^');
	  p1++;
	  if (p1 >= startp && p1 < endp)
	    *p1 = c ^ 0100;
	  p1++;
	}
      else
	{
	  if (p1 >= startp)
	    *p1 = (dp && XTYPE (DISP_ESCAPE_GLYPH (dp)) == Lisp_Int
		   ? XINT (DISP_ESCAPE_GLYPH (dp)) : '\\');
	  p1++;
	  if (p1 >= startp && p1 < endp)
	    *p1 = (c >> 6) + '0';
	  p1++;
	  if (p1 >= startp && p1 < endp)
	    *p1 = (7 & (c >> 3)) + '0';
	  p1++;
	  if (p1 >= startp && p1 < endp)
	    *p1 = (7 & c) + '0';
	  p1++;
	}
      pos++;
    }

  val.hpos = - XINT (w->hscroll);
  if (val.hpos)
    val.hpos++;

  val.vpos = 1;

  lastpos = pos;

  /* Handle continuation in middle of a character */
  /* by backing up over it */
  if (p1 > endp)
    {
      /* Start the next line with that same character */
      pos--;
      /* but at a negative hpos, to skip the columns output on this line.  */
      val.hpos += p1prev - endp;
      /* Keep in this line everything up to the continuation column.  */
      p1 = endp;
    }

  /* Finish deciding which character to start the next line on,
     and what hpos to start it at.
     Also set `lastpos' to the last position which counts as "on this line"
     for cursor-positioning.  */

  if (pos < ZV)
    {
      if (FETCH_CHAR (pos) == '\n')
	/* If stopped due to a newline, start next line after it */
	pos++;
      else
	/* Stopped due to right margin of window */
	{
	  if (truncate)
	    {
	      *p1++ = truncator;
	      /* Truncating => start next line after next newline,
		 and point is on this line if it is before the newline,
		 and skip none of first char of next line */
	      pos = find_next_newline (pos, 1);
	      val.hpos = XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0;

	      lastpos = pos - (FETCH_CHAR (pos - 1) == '\n');
	    }
	  else
	    {
	      *p1++ = continuer;
	      val.vpos = 0;
	      lastpos--;
	    }
	}
    }

  /* If point is at eol or in invisible text at eol,
     record its frame location now.  */

  if (start <= point && point <= lastpos && cursor_vpos < 0)
    {
      cursor_vpos = vpos;
      cursor_hpos = p1 - startp;
    }

  if (cursor_vpos == vpos)
    {
      if (cursor_hpos < 0) cursor_hpos = 0;
      if (cursor_hpos > width) cursor_hpos = width;
      cursor_hpos += XFASTINT (w->left);
      if (w == XWINDOW (FRAME_SELECTED_WINDOW (f)))
	{
	  FRAME_CURSOR_Y (f) = cursor_vpos;
	  FRAME_CURSOR_X (f) = cursor_hpos;

	  if (w == XWINDOW (selected_window))
	    {
	      /* Line is not continued and did not start
		 in middle of character */
	      if ((hpos - XFASTINT (w->left)
		   == (XINT (w->hscroll) ? 1 - XINT (w->hscroll) : 0))
		  && val.vpos)
		{
		  this_line_bufpos = start;
		  this_line_buffer = current_buffer;
		  this_line_vpos = cursor_vpos;
		  this_line_start_hpos = hpos;
		  this_line_endpos = Z - lastpos;
		}
	      else
		this_line_bufpos = 0;
	    }
	}
    }

  /* If hscroll and line not empty, insert truncation-at-left marker */
  if (hscroll && lastpos != start)
    {
      *startp = truncator;
      if (p1 <= startp)
	p1 = startp + 1;
    }

  if (XFASTINT (w->width) + XFASTINT (w->left) != FRAME_WIDTH (f))
    {
      endp++;
      if (p1 < startp) p1 = startp;
      while (p1 < endp) *p1++ = SPACEGLYPH;

      /* Don't draw vertical bars if we're using scroll bars.  They're
         covered up by the scroll bars, and it's distracting to see
         them when the scroll bar windows are flickering around to be
         reconfigured.  */
      *p1++ = (FRAME_HAS_VERTICAL_SCROLL_BARS (f)
	       ? ' ' : '|');
    }
  desired_glyphs->used[vpos] = max (desired_glyphs->used[vpos],
				   p1 - desired_glyphs->glyphs[vpos]);
  desired_glyphs->glyphs[vpos][desired_glyphs->used[vpos]] = 0;

  /* If the start of this line is the overlay arrow-position,
     then put the arrow string into the display-line.  */

  if (XTYPE (Voverlay_arrow_position) == Lisp_Marker
      && current_buffer == XMARKER (Voverlay_arrow_position)->buffer
      && start == marker_position (Voverlay_arrow_position)
      && XTYPE (Voverlay_arrow_string) == Lisp_String
      && ! overlay_arrow_seen)
    {
      unsigned char *p = XSTRING (Voverlay_arrow_string)->data;
      int i;
      int len = XSTRING (Voverlay_arrow_string)->size;

      if (len > width)
	len = width;
      for (i = 0; i < len; i++)
	startp[i] = p[i];
      if (desired_glyphs->used[vpos] <
	  (len + startp - desired_glyphs->glyphs[vpos]))
	desired_glyphs->used[vpos] = len + startp - desired_glyphs->glyphs[vpos];

      overlay_arrow_seen = 1;
    }

  val.bufpos = pos;
  val_display_text_line = val;
  return &val_display_text_line;
}

/* Redisplay the menu bar in the frame for window W.  */

static void
display_menu_bar (w)
     struct window *w;
{
  Lisp_Object items, tail;
  register int vpos = 0;
  register FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  int maxendcol = FRAME_WIDTH (f);
  int hpos = 0;

  if (FRAME_MENU_BAR_LINES (f) <= 0)
    return;

  get_display_line (f, vpos, 0);

  for (tail = FRAME_MENU_BAR_ITEMS (f); CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object string;

      string = XCONS (XCONS (XCONS (tail)->car)->cdr)->car;

      /* Record in each item its hpos.  */
      XFASTINT (XCONS (XCONS (XCONS (tail)->car)->cdr)->cdr) = hpos;

      if (hpos < maxendcol)
	hpos = display_string (XWINDOW (FRAME_ROOT_WINDOW (f)), vpos,
			       XSTRING (string)->data,
			       hpos, 0, hpos, maxendcol);
      /* Put a gap of 3 spaces between items.  */
      if (hpos < maxendcol)
	{
	  int hpos1 = hpos + 3;
	  hpos = display_string (w, vpos, "", hpos, 0,
				 min (hpos1, maxendcol), maxendcol);
	}
    }

  FRAME_DESIRED_GLYPHS (f)->bufp[vpos] = 0;
  FRAME_DESIRED_GLYPHS (f)->highlight[vpos] = mode_line_inverse_video;

  /* Fill out the line with spaces.  */
  if (maxendcol > hpos)
    hpos = display_string (w, vpos, "", hpos, 0, maxendcol, -1);
}

/* Display the mode line for window w */

static void
display_mode_line (w)
     struct window *w;
{
  register int vpos = XFASTINT (w->height) + XFASTINT (w->top) - 1;
  register int left = XFASTINT (w->left);
  register int right = XFASTINT (w->width) + left;
  register FRAME_PTR f = XFRAME (WINDOW_FRAME (w));

  get_display_line (f, vpos, left);
  display_mode_element (w, vpos, left, 0, right, right,
			current_buffer->mode_line_format);
  FRAME_DESIRED_GLYPHS (f)->bufp[vpos] = 0;

  /* Make the mode line inverse video if the entire line
     is made of mode lines.
     I.e. if this window is full width,
     or if it is the child of a full width window
     (which implies that that window is split side-by-side
     and the rest of this line is mode lines of the sibling windows).  */
  if (XFASTINT (w->width) == FRAME_WIDTH (f)
      || XFASTINT (XWINDOW (w->parent)->width) == FRAME_WIDTH (f))
    FRAME_DESIRED_GLYPHS (f)->highlight[vpos] = mode_line_inverse_video;

#ifdef HAVE_X_WINDOWS
  /* I'm trying this out because I saw Unimpress use it, but it's
     possible that this may mess adversely with some window managers.  -jla

     Wouldn't it be nice to use something like mode-line-format to
     describe frame titles?  -JimB  */

  /* Change the title of the frame to the name of the buffer displayed
     in the currently selected window.  Don't do this for minibuffer frames,
     and don't do it when there's only one non-minibuffer frame.  */
  if (FRAME_X_P (f)
      && ! FRAME_MINIBUF_ONLY_P (f)
      && w == XWINDOW (f->selected_window))
    x_implicitly_set_name (f, (EQ (Fnext_frame (WINDOW_FRAME (w), Qnil),
				   WINDOW_FRAME (w))
			       ? Qnil
			       : XBUFFER (w->buffer)->name),
			   Qnil);
#endif
}

/* Contribute ELT to the mode line for window W.
   How it translates into text depends on its data type.

   VPOS is the position of the mode line being displayed.

   HPOS is the position (absolute on frame) where this element's text
   should start.  The output is truncated automatically at the right
   edge of window W.

   DEPTH is the depth in recursion.  It is used to prevent
   infinite recursion here.

   MINENDCOL is the hpos before which the element may not end.
   The element is padded at the right with spaces if nec
   to reach this column.

   MAXENDCOL is the hpos past which this element may not extend.
   If MINENDCOL is > MAXENDCOL, MINENDCOL takes priority.
   (This is necessary to make nested padding and truncation work.)

   Returns the hpos of the end of the text generated by ELT.
   The next element will receive that value as its HPOS arg,
   so as to concatenate the elements.  */

static int
display_mode_element (w, vpos, hpos, depth, minendcol, maxendcol, elt)
     struct window *w;
     register int vpos, hpos;
     int depth;
     int minendcol;
     register int maxendcol;
     register Lisp_Object elt;
{
 tail_recurse:
  if (depth > 10)
    goto invalid;

  depth++;

#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (elt))
#else
  switch (XTYPE (elt))
#endif
    {
    case Lisp_String:
      {
	/* A string: output it and check for %-constructs within it.  */
	register unsigned char c;
	register unsigned char *this = XSTRING (elt)->data;

	while (hpos < maxendcol && *this)
	  {
	    unsigned char *last = this;
	    while ((c = *this++) != '\0' && c != '%')
	      ;
	    if (this - 1 != last)
	      {
		register int lim = --this - last + hpos;
		hpos = display_string (w, vpos, last, hpos, 0, hpos,
				       min (lim, maxendcol));
	      }
	    else /* c == '%' */
	      {
		register int spec_width = 0;

		/* We can't allow -ve args due to the "%-" construct */
		/* Argument specifies minwidth but not maxwidth
		   (maxwidth can be specified by
		     (<negative-number> . <stuff>) mode-line elements) */

		while ((c = *this++) >= '0' && c <= '9')
		  {
		    spec_width = spec_width * 10 + (c - '0');
		  }

		spec_width += hpos;
		if (spec_width > maxendcol)
		  spec_width = maxendcol;

		if (c == 'M')
		  hpos = display_mode_element (w, vpos, hpos, depth,
					       spec_width, maxendcol,
					       Vglobal_mode_string);
		else if (c != 0)
		  hpos = display_string (w, vpos,
					 decode_mode_spec (w, c,
							   maxendcol - hpos),
					 hpos, 0, spec_width, maxendcol);
	      }
	  }
      }
      break;

    case Lisp_Symbol:
      /* A symbol: process the value of the symbol recursively
	 as if it appeared here directly.  Avoid error if symbol void.
	 Special case: if value of symbol is a string, output the string
	 literally.  */
      {
	register Lisp_Object tem;
	tem = Fboundp (elt);
	if (!NILP (tem))
	  {
	    tem = Fsymbol_value (elt);
	    /* If value is a string, output that string literally:
	       don't check for % within it.  */
	    if (XTYPE (tem) == Lisp_String)
	      hpos = display_string (w, vpos, XSTRING (tem)->data,
				     hpos, 0, minendcol, maxendcol);
	    /* Give up right away for nil or t.  */
	    else if (!EQ (tem, elt))
	      { elt = tem; goto tail_recurse; }
	  }
      }
      break;

    case Lisp_Cons:
      {
	register Lisp_Object car, tem;

	/* A cons cell: three distinct cases.
	   If first element is a string or a cons, process all the elements
	   and effectively concatenate them.
	   If first element is a negative number, truncate displaying cdr to
	   at most that many characters.  If positive, pad (with spaces)
	   to at least that many characters.
	   If first element is a symbol, process the cadr or caddr recursively
	   according to whether the symbol's value is non-nil or nil.  */
	car = XCONS (elt)->car;
	if (XTYPE (car) == Lisp_Symbol)
	  {
	    tem = Fboundp (car);
	    elt = XCONS (elt)->cdr;
	    if (XTYPE (elt) != Lisp_Cons)
	      goto invalid;
	    /* elt is now the cdr, and we know it is a cons cell.
	       Use its car if CAR has a non-nil value.  */
	    if (!NILP (tem))
	      {
		tem = Fsymbol_value (car);
		if (!NILP (tem))
		  { elt = XCONS (elt)->car; goto tail_recurse; }
	      }
	    /* Symbol's value is nil (or symbol is unbound)
	       Get the cddr of the original list
	       and if possible find the caddr and use that.  */
	    elt = XCONS (elt)->cdr;
	    if (NILP (elt))
	      break;
	    else if (XTYPE (elt) != Lisp_Cons)
	      goto invalid;
	    elt = XCONS (elt)->car;
	    goto tail_recurse;
	  }
	else if (XTYPE (car) == Lisp_Int)
	  {
	    register int lim = XINT (car);
	    elt = XCONS (elt)->cdr;
	    if (lim < 0)
	      /* Negative int means reduce maximum width.
		 DO NOT change MINENDCOL here!
		 (20 -10 . foo) should truncate foo to 10 col
		 and then pad to 20.  */
	      maxendcol = min (maxendcol, hpos - lim);
	    else if (lim > 0)
	      {
		/* Padding specified.  Don't let it be more than
		   current maximum.  */
		lim += hpos;
		if (lim > maxendcol)
		  lim = maxendcol;
		/* If that's more padding than already wanted, queue it.
		   But don't reduce padding already specified even if
		   that is beyond the current truncation point.  */
		if (lim > minendcol)
		  minendcol = lim;
	      }
	    goto tail_recurse;
	  }
	else if (XTYPE (car) == Lisp_String || XTYPE (car) == Lisp_Cons)
	  {
	    register int limit = 50;
	    /* LIMIT is to protect against circular lists.  */
	    while (XTYPE (elt) == Lisp_Cons && --limit > 0
		   && hpos < maxendcol)
	      {
		hpos = display_mode_element (w, vpos, hpos, depth,
					     hpos, maxendcol,
					     XCONS (elt)->car);
		elt = XCONS (elt)->cdr;
	      }
	  }
      }
      break;

    default:
    invalid:
      return (display_string (w, vpos, "*invalid*", hpos, 0,
			      minendcol, maxendcol));
    }

 end:
  if (minendcol > hpos)
    hpos = display_string (w, vpos, "", hpos, 0, minendcol, -1);
  return hpos;
}

/* Return a string for the output of a mode line %-spec for window W,
   generated by character C and width MAXWIDTH.  */

static char lots_of_dashes[] = "--------------------------------------------------------------------------------------------------------------------------------------------";

static char *
decode_mode_spec (w, c, maxwidth)
     struct window *w;
     register char c;
     register int maxwidth;
{
  Lisp_Object obj = Qnil;
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  char *decode_mode_spec_buf = (char *) FRAME_TEMP_GLYPHS (f)->total_contents;

  if (maxwidth > FRAME_WIDTH (f))
    maxwidth = FRAME_WIDTH (f);

  switch (c)
    {
    case 'b': 
      obj = current_buffer->name;
#if 0
      if (maxwidth >= 3 && XSTRING (obj)->size > maxwidth)
	{
	  bcopy (XSTRING (obj)->data, decode_mode_spec_buf, maxwidth - 1);
	  decode_mode_spec_buf[maxwidth - 1] = '\\';
	  decode_mode_spec_buf[maxwidth] = '\0';
	  return decode_mode_spec_buf;
	}
#endif
      break;

    case 'f': 
      obj = current_buffer->filename;
#if 0
      if (NILP (obj))
	return "[none]";
      else if (XTYPE (obj) == Lisp_String && XSTRING (obj)->size > maxwidth)
	{
	  bcopy ("...", decode_mode_spec_buf, 3);
	  bcopy (XSTRING (obj)->data + XSTRING (obj)->size - maxwidth + 3,
		 decode_mode_spec_buf + 3, maxwidth - 3);
	  return decode_mode_spec_buf;
	}
#endif
      break;

    case 'm': 
      obj = current_buffer->mode_name;
      break;

    case 'n':
      if (BEGV > BEG || ZV < Z)
	return " Narrow";
      break;

    case '*':
      if (!NILP (current_buffer->read_only))
	return "%";
      if (MODIFF > current_buffer->save_modified)
	return "*";
      return "-";

    case 's':
      /* status of process */
      obj = Fget_buffer_process (Fcurrent_buffer ());
      if (NILP (obj))
	return "no process";
      obj = Fsymbol_name (Fprocess_status (obj));
      break;

    case 'p':
      {
	int pos = marker_position (w->start);
	int total = ZV - BEGV;

	if (XFASTINT (w->window_end_pos) <= Z - ZV)
	  {
	    if (pos <= BEGV)
	      return "All";
	    else
	      return "Bottom";
	  }
	else if (pos <= BEGV)
	  return "Top";
	else
	  {
	    total = ((pos - BEGV) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    sprintf (decode_mode_spec_buf, "%2d%%", total);
	    return decode_mode_spec_buf;
	  }
      }

    case '%':
      return "%";

    case '[': 
      {
	int i;
	char *p;

	if (command_loop_level > 5)
	  return "[[[... ";
	p = decode_mode_spec_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = '[';
	*p = 0;
	return decode_mode_spec_buf;
      }

    case ']': 
      {
	int i;
	char *p;

	if (command_loop_level > 5)
	  return " ...]]]";
	p = decode_mode_spec_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = ']';
	*p = 0;
	return decode_mode_spec_buf;
      }
      
    case '-':
      {
	register char *p;
	register int i;
	
	if (maxwidth < sizeof (lots_of_dashes))
	  return lots_of_dashes;
	else
	  {
	    for (p = decode_mode_spec_buf, i = maxwidth; i > 0; i--)
	      *p++ = '-';
	    *p = '\0';
	  }
	return decode_mode_spec_buf;
      }
    }
  
  if (XTYPE (obj) == Lisp_String)
    return (char *) XSTRING (obj)->data;
  else
    return "";
}

/* Display STRING on one line of window W, starting at HPOS.
   Display at position VPOS.  Caller should have done get_display_line.
   If VPOS == -1, display it as the current frame's title.

  TRUNCATE is GLYPH to display at end if truncated.  Zero for none.

  MINCOL is the first column ok to end at.  (Pad with spaces to this col.)
  MAXCOL is the last column ok to end at.  Truncate here.
    -1 for MINCOL or MAXCOL means no explicit minimum or maximum.
  Both count from the left edge of the frame, as does HPOS.
  The right edge of W is an implicit maximum.
  If TRUNCATE is nonzero, the implicit maximum is one column before the edge.

  Returns ending hpos */

static int
display_string (w, vpos, string, hpos, truncate, mincol, maxcol)
     struct window *w;
     unsigned char *string;
     int vpos, hpos;
     GLYPH truncate;
     int mincol, maxcol;
{
  register int c;
  register GLYPH *p1;
  int hscroll = XINT (w->hscroll);
  int tab_width = XINT (XBUFFER (w->buffer)->tab_width);
  register GLYPH *start;
  register GLYPH *end;
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  struct frame_glyphs *desired_glyphs = FRAME_DESIRED_GLYPHS (f);
  GLYPH *p1start = desired_glyphs->glyphs[vpos] + hpos;
  int window_width = XFASTINT (w->width);

  /* Use the standard display table, not the window's display table.
     We don't want the mode line in rot13.  */
  register struct Lisp_Vector *dp = 0;

  if (XTYPE (Vstandard_display_table) == Lisp_Vector
      && XVECTOR (Vstandard_display_table)->size == DISP_TABLE_SIZE)
    dp = XVECTOR (Vstandard_display_table);

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  p1 = p1start;
  start = desired_glyphs->glyphs[vpos] + XFASTINT (w->left);
  end = start + window_width - (truncate != 0);

  if ((window_width + XFASTINT (w->left)) != FRAME_WIDTH (f))
    {
      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
	{
	  int i;

	  for (i = 0; i < VERTICAL_SCROLL_BAR_WIDTH; i++)
	    *end-- = ' ';
	}
      else
	*end-- = '|';
    }

  if (maxcol >= 0 && end - desired_glyphs->glyphs[vpos] > maxcol)
    end = desired_glyphs->glyphs[vpos] + maxcol;
  if (maxcol >= 0 && mincol > maxcol)
    mincol = maxcol;

  while (p1 < end)
    {
      c = *string++;
      if (!c) break;
      if (c >= 040 && c < 0177
	  && (dp == 0 || XTYPE (DISP_CHAR_VECTOR (dp, c)) != Lisp_Vector))
	{
	  if (p1 >= start)
	    *p1 = c;
	  p1++;
	}
      else if (c == '\t')
	{
	  do
	    {
	      if (p1 >= start && p1 < end)
		*p1 = SPACEGLYPH;
	      p1++;
	    }
	  while ((p1 - start + hscroll - (hscroll > 0)) % tab_width);
	}
      else if (dp != 0 && XTYPE (DISP_CHAR_VECTOR (dp, c)) == Lisp_Vector)
        p1 = copy_rope (p1, start, DISP_CHAR_VECTOR (dp, c));
      else if (c < 0200 && ! NILP (buffer_defaults.ctl_arrow))
	{
	  if (p1 >= start)
	    *p1 = (dp && XTYPE (DISP_CTRL_GLYPH (dp)) == Lisp_Int
		   ? XINT (DISP_CTRL_GLYPH (dp)) : '^');
	  p1++;
	  if (p1 >= start && p1 < end)
	    *p1 = c ^ 0100;
	  p1++;
	}
      else
	{
	  if (p1 >= start)
	    *p1 = (dp && XTYPE (DISP_ESCAPE_GLYPH (dp)) == Lisp_Int
		   ? XINT (DISP_ESCAPE_GLYPH (dp)) : '\\');
	  p1++;
	  if (p1 >= start && p1 < end)
	    *p1 = (c >> 6) + '0';
	  p1++;
	  if (p1 >= start && p1 < end)
	    *p1 = (7 & (c >> 3)) + '0';
	  p1++;
	  if (p1 >= start && p1 < end)
	    *p1 = (7 & c) + '0';
	  p1++;
	}
    }

  if (c)
    {
      p1 = end;
      if (truncate) *p1++ = truncate;
    }
  else if (mincol >= 0)
    {
      end = desired_glyphs->glyphs[vpos] + mincol;
      while (p1 < end)
	*p1++ = SPACEGLYPH;
    }

  {
    register int len = p1 - desired_glyphs->glyphs[vpos];

    if (len > desired_glyphs->used[vpos])
      desired_glyphs->used[vpos] = len;
    desired_glyphs->glyphs[vpos][desired_glyphs->used[vpos]] = 0;

    return len;
  }
}

void
syms_of_xdisp ()
{
  staticpro (&last_arrow_position);
  staticpro (&last_arrow_string);
  last_arrow_position = Qnil;
  last_arrow_string = Qnil;

  DEFVAR_LISP ("global-mode-string", &Vglobal_mode_string,
    "String (or mode line construct) included (normally) in `mode-line-format'.");
  Vglobal_mode_string = Qnil;

  DEFVAR_LISP ("overlay-arrow-position", &Voverlay_arrow_position,
    "Marker for where to display an arrow on top of the buffer text.\n\
This must be the beginning of a line in order to work.\n\
See also `overlay-arrow-string'.");
  Voverlay_arrow_position = Qnil;

  DEFVAR_LISP ("overlay-arrow-string", &Voverlay_arrow_string,
    "String to display as an arrow.  See also `overlay-arrow-position'.");
  Voverlay_arrow_string = Qnil;

  DEFVAR_INT ("scroll-step", &scroll_step,
    "*The number of lines to try scrolling a window by when point moves out.\n\
If that fails to bring point back on frame, point is centered instead.\n\
If this is zero, point is always centered after it moves off frame.");

  DEFVAR_INT ("debug-end-pos", &debug_end_pos, "Don't ask");

  DEFVAR_BOOL ("truncate-partial-width-windows",
	       &truncate_partial_width_windows,
    "*Non-nil means truncate lines in all windows less than full frame wide.");
  truncate_partial_width_windows = 1;

  DEFVAR_BOOL ("mode-line-inverse-video", &mode_line_inverse_video,
    "*Non-nil means use inverse video for the mode line.");
  mode_line_inverse_video = 1;
}

/* initialize the window system */
init_xdisp ()
{
  Lisp_Object root_window;
#ifndef COMPILER_REGISTER_BUG
  register
#endif /* COMPILER_REGISTER_BUG */
    struct window *mini_w;

  this_line_bufpos = 0;

  mini_w = XWINDOW (minibuf_window);
  root_window = FRAME_ROOT_WINDOW (XFRAME (WINDOW_FRAME (mini_w)));

  echo_area_glyphs = 0;
  previous_echo_glyphs = 0;

  if (!noninteractive)
    {
      FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (root_window)));
      XFASTINT (XWINDOW (root_window)->top) = 0;
      set_window_height (root_window, FRAME_HEIGHT (f) - 1, 0);
      XFASTINT (mini_w->top) = FRAME_HEIGHT (f) - 1;
      set_window_height (minibuf_window, 1, 0);

      XFASTINT (XWINDOW (root_window)->width) = FRAME_WIDTH (f);
      XFASTINT (mini_w->width) = FRAME_WIDTH (f);
    }
}
