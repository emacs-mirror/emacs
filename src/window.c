/* Window creation, deletion and examination for GNU Emacs.
   Does not include redisplay.
   Copyright (C) 1985, 1986, 1987, 1993, 1994 Free Software Foundation, Inc.

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

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "frame.h"
#include "window.h"
#include "commands.h"
#include "indent.h"
#include "termchar.h"
#include "disptab.h"
#include "keyboard.h"

Lisp_Object Qwindowp, Qwindow_live_p;

Lisp_Object Fnext_window (), Fdelete_window (), Fselect_window ();
Lisp_Object Fset_window_buffer (), Fsplit_window (), Frecenter ();

void delete_all_subwindows ();
static struct window *decode_window();

/* This is the window in which the terminal's cursor should
   be left when nothing is being done with it.  This must
   always be a leaf window, and its buffer is selected by
   the top level editing loop at the end of each command.

   This value is always the same as
   FRAME_SELECTED_WINDOW (selected_frame).  */

Lisp_Object selected_window;

/* The minibuffer window of the selected frame.
   Note that you cannot test for minibufferness of an arbitrary window
   by comparing against this; but you can test for minibufferness of
   the selected window.  */
Lisp_Object minibuf_window;

/* Non-nil means it is the window for C-M-v to scroll
   when the minibuffer is selected.  */
Lisp_Object Vminibuf_scroll_window;

/* Non-nil means this is the buffer whose window C-M-v should scroll.  */
Lisp_Object Vother_window_scroll_buffer;

/* Non-nil means it's function to call to display temp buffers.  */
Lisp_Object Vtemp_buffer_show_function;

/* If a window gets smaller than either of these, it is removed. */
int window_min_height;
int window_min_width;

/* Nonzero implies Fdisplay_buffer should create windows. */
int pop_up_windows;

/* Nonzero implies make new frames for Fdisplay_buffer.  */
int pop_up_frames;

/* Non-nil means use this function instead of default */
Lisp_Object Vpop_up_frame_function;

/* Function to call to handle Fdisplay_buffer.  */
Lisp_Object Vdisplay_buffer_function;

/* List of buffer *names* for buffers that should have their own frames.  */
Lisp_Object Vspecial_display_buffer_names;

/* List of regexps for buffer names that should have their own frames.  */
Lisp_Object Vspecial_display_regexps;

/* Function to pop up a special frame.  */
Lisp_Object Vspecial_display_function;

/* Fdisplay_buffer always splits the largest window 
   if that window is more than this high.  */
int split_height_threshold;

/* Number of lines of continuity in scrolling by screenfuls.  */
int next_screen_context_lines;

/* Incremented for each window created.  */
static int sequence_number;

#define min(a, b) ((a) < (b) ? (a) : (b))

DEFUN ("windowp", Fwindowp, Swindowp, 1, 1, 0,
  "Returns t if OBJ is a window.")
  (obj)
     Lisp_Object obj;
{
  return WINDOWP (obj) ? Qt : Qnil;
}

DEFUN ("window-live-p", Fwindow_live_p, Swindow_live_p, 1, 1, 0,
  "Returns t if OBJ is a window which is currently visible.")
     (obj)
     Lisp_Object obj;
{
  return (WINDOWP (obj) && ! NILP (XWINDOW (obj)->buffer) ? Qt : Qnil);
}

Lisp_Object
make_window ()
{
  Lisp_Object val;
  register struct window *p;
  register struct Lisp_Vector *vec;
  int i;

  vec = allocate_vectorlike ((EMACS_INT) VECSIZE (struct window));
  for (i = 0; i < VECSIZE (struct window); i++)
    vec->contents[i] = Qnil;
  vec->size = VECSIZE (struct window);
  p = (struct window *)vec;
  XSETFASTINT (p->sequence_number, ++sequence_number);
  XSETFASTINT (p->left, 0);
  XSETFASTINT (p->top, 0);
  XSETFASTINT (p->height, 0);
  XSETFASTINT (p->width, 0);
  XSETFASTINT (p->hscroll, 0);
  XSETFASTINT (p->last_point_x, 0);
  XSETFASTINT (p->last_point_y, 0);
  p->start = Fmake_marker ();
  p->pointm = Fmake_marker ();
  XSETFASTINT (p->use_time, 0);
  p->frame = Qnil;
  p->display_table = Qnil;
  p->dedicated = Qnil;
  XSETWINDOW (val, p);
  return val;
}

DEFUN ("selected-window", Fselected_window, Sselected_window, 0, 0, 0,
  "Return the window that the cursor now appears in and commands apply to.")
  ()
{
  return selected_window;
}

DEFUN ("minibuffer-window", Fminibuffer_window, Sminibuffer_window, 0, 1, 0,
  "Return the window used now for minibuffers.\n\
If the optional argument FRAME is specified, return the minibuffer window\n\
used by that frame.")
  (frame)
    Lisp_Object frame;
{
#ifdef MULTI_FRAME
  if (NILP (frame))
    XSETFRAME (frame, selected_frame);
  else
    CHECK_LIVE_FRAME (frame, 0);
#endif

  return FRAME_MINIBUF_WINDOW (XFRAME (frame));
}

DEFUN ("window-minibuffer-p", Fwindow_minibuffer_p, Swindow_minibuffer_p, 0, 1, 0,
  "Returns non-nil if WINDOW is a minibuffer window.")
  (window)
     Lisp_Object window;
{
  struct window *w = decode_window (window);
  return (MINI_WINDOW_P (w) ? Qt : Qnil);
}

DEFUN ("pos-visible-in-window-p", Fpos_visible_in_window_p,
  Spos_visible_in_window_p, 0, 2, 0,
  "Return t if position POS is currently on the frame in WINDOW.\n\
Returns nil if that position is scrolled vertically out of view.\n\
POS defaults to point; WINDOW, to the selected window.")
  (pos, window)
     Lisp_Object pos, window;
{
  register struct window *w;
  register int top;
  register int height;
  register int posint;
  register struct buffer *buf;
  struct position posval;
  int hscroll;

  if (NILP (pos))
    posint = PT;
  else
    {
      CHECK_NUMBER_COERCE_MARKER (pos, 0);
      posint = XINT (pos);
    }

  w = decode_window (window);
  top = marker_position (w->start);
  hscroll = XINT (w->hscroll);

  if (posint < top)
    return Qnil;

  height = XFASTINT (w->height) - ! MINI_WINDOW_P (w);

  buf = XBUFFER (w->buffer);
  if (XFASTINT (w->last_modified) >= BUF_MODIFF (buf))
    {
      /* If frame is up to date,
	 use the info recorded about how much text fit on it. */
      if (posint < BUF_Z (buf) - XFASTINT (w->window_end_pos)
	  || (XFASTINT (w->window_end_vpos) < height))
	return Qt;
      return Qnil;
    }
  else
    {
      if (posint > BUF_ZV (buf))
	return Qnil;

      /* w->start can be out of range.  If it is, do something reasonable.  */
      if (top < BUF_BEGV (buf) || top > BUF_ZV (buf))
	return Qnil;

      /* If that info is not correct, calculate afresh */
      posval = *compute_motion (top, 0, (hscroll ? 1 - hscroll : 0),
				posint, height, 0,
				window_internal_width (w) - 1,
				hscroll, 0, w);

      return posval.vpos < height ? Qt : Qnil;
    }
}

static struct window *
decode_window (window)
     register Lisp_Object window;
{
  if (NILP (window))
    return XWINDOW (selected_window);

  CHECK_LIVE_WINDOW (window, 0);
  return XWINDOW (window);
}

DEFUN ("window-buffer", Fwindow_buffer, Swindow_buffer, 0, 1, 0,
  "Return the buffer that WINDOW is displaying.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->buffer;
}

DEFUN ("window-height", Fwindow_height, Swindow_height, 0, 1, 0,
  "Return the number of lines in WINDOW (including its mode line).")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->height;
}

DEFUN ("window-width", Fwindow_width, Swindow_width, 0, 1, 0,
  "Return the number of display columns in WINDOW.\n\
This is the width that is usable columns available for text in WINDOW.\n\
If you want to find out how many columns WINDOW takes up,\n\
use  (let ((edges (window-edges))) (- (nth 2 edges) (nth 0 edges))).")
  (window)
     Lisp_Object window;
{
  return make_number (window_internal_width (decode_window (window)));
}

DEFUN ("window-hscroll", Fwindow_hscroll, Swindow_hscroll, 0, 1, 0,
  "Return the number of columns by which WINDOW is scrolled from left margin.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->hscroll;
}

DEFUN ("set-window-hscroll", Fset_window_hscroll, Sset_window_hscroll, 2, 2, 0,
  "Set number of columns WINDOW is scrolled from left margin to NCOL.\n\
NCOL should be zero or positive.")
  (window, ncol)
     register Lisp_Object window, ncol;
{
  register struct window *w;

  CHECK_NUMBER (ncol, 1);
  if (XINT (ncol) < 0) XSETFASTINT (ncol, 0);
  if (XFASTINT (ncol) >= (1 << (SHORTBITS - 1)))
    args_out_of_range (ncol, Qnil);
  w = decode_window (window);
  if (XINT (w->hscroll) != XINT (ncol))
    clip_changed = 1;		/* Prevent redisplay shortcuts */
  w->hscroll = ncol;
  return ncol;
}

DEFUN ("window-edges", Fwindow_edges, Swindow_edges, 0, 1, 0,
  "Return a list of the edge coordinates of WINDOW.\n\
\(LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at top left corner of frame.\n\
RIGHT is one more than the rightmost column used by WINDOW,\n\
and BOTTOM is one more than the bottommost row used by WINDOW\n\
 and its mode-line.")
  (window)
     Lisp_Object window;
{
  register struct window *w = decode_window (window);

  return Fcons (w->left, Fcons (w->top,
           Fcons (make_number (XFASTINT (w->left) + XFASTINT (w->width)),
		  Fcons (make_number (XFASTINT (w->top)
				      + XFASTINT (w->height)),
			 Qnil))));
}

/* Test if the character at column *x, row *y is within window *w.
   If it is not, return 0;
   if it is in the window's text area,
      set *x and *y to its location relative to the upper left corner
         of the window, and
      return 1;
   if it is on the window's modeline, return 2;
   if it is on the border between the window and its right sibling,
      return 3.  */
static int
coordinates_in_window (w, x, y)
     register struct window *w;
     register int *x, *y;
{
  register int left = XINT (w->left);
  register int width = XINT (w->width);
  register int window_height = XINT (w->height);
  register int top = XFASTINT (w->top);

  if (   *x < left || *x >= left + width
      || *y < top  || *y >= top  + window_height)
    return 0;

  /* Is the character is the mode line?  */
  if (*y == top + window_height - 1
      && ! MINI_WINDOW_P (w))
    return 2;
  
  /* Is the character in the right border?  */
  if (*x == left + width - 1
      && left + width != FRAME_WIDTH (XFRAME (w->frame)))
    return 3;

  *x -= left;
  *y -= top;
  return 1;
}

DEFUN ("coordinates-in-window-p", Fcoordinates_in_window_p,
  Scoordinates_in_window_p, 2, 2, 0,
  "Return non-nil if COORDINATES are in WINDOW.\n\
COORDINATES is a cons of the form (X . Y), X and Y being distances\n\
measured in characters from the upper-left corner of the frame.\n\
(0 .  0) denotes the character in the upper left corner of the\n\
frame.\n\
If COORDINATES are in the text portion of WINDOW,\n\
   the coordinates relative to the window are returned.\n\
If they are in the mode line of WINDOW, `mode-line' is returned.\n\
If they are on the border between WINDOW and its right sibling,\n\
   `vertical-line' is returned.")
  (coordinates, window)
     register Lisp_Object coordinates, window;
{
  int x, y;

  CHECK_LIVE_WINDOW (window, 0);
  CHECK_CONS (coordinates, 1);
  x = XINT (Fcar (coordinates));
  y = XINT (Fcdr (coordinates));

  switch (coordinates_in_window (XWINDOW (window), &x, &y))
    {
    case 0:			/* NOT in window at all. */
      return Qnil;

    case 1:			/* In text part of window. */
      return Fcons (x, y);

    case 2:			/* In mode line of window. */
      return Qmode_line;
      
    case 3:			/* On right border of window.  */
      return Qvertical_line;

    default:
      abort ();
    }
}

/* Find the window containing column x, row y, and return it as a
   Lisp_Object.  If x, y is on the window's modeline, set *part
   to 1; if it is on the separating line between the window and its
   right sibling, set it to 2; otherwise set it to 0.  If there is no
   window under x, y return nil and leave *part unmodified.  */
Lisp_Object
window_from_coordinates (frame, x, y, part)
     FRAME_PTR frame;
     int x, y;
     int *part;
{
  register Lisp_Object tem, first;

  tem = first = FRAME_SELECTED_WINDOW (frame);

  do
    {
      int found = coordinates_in_window (XWINDOW (tem), &x, &y);

      if (found)
	{
	  *part = found - 1;
	  return tem;
	}

      tem = Fnext_window (tem, Qt, Qlambda);
    }
  while (! EQ (tem, first));
  
  return Qnil;
}

DEFUN ("window-at", Fwindow_at, Swindow_at, 2, 3, 0,
  "Return window containing coordinates X and Y on FRAME.\n\
If omitted, FRAME defaults to the currently selected frame.\n\
The top left corner of the frame is considered to be row 0,\n\
column 0.")
  (x, y, frame)
      Lisp_Object x, y, frame;
{
  int part;

#ifdef MULTI_FRAME
  if (NILP (frame))
    XSETFRAME (frame, selected_frame);
  else
    CHECK_LIVE_FRAME (frame, 2);
#endif
  CHECK_NUMBER (x, 0);
  CHECK_NUMBER (y, 1);

  return window_from_coordinates (XFRAME (frame),
				  XINT (x), XINT (y),
				  &part);
}

DEFUN ("window-point", Fwindow_point, Swindow_point, 0, 1, 0,
  "Return current value of point in WINDOW.\n\
For a nonselected window, this is the value point would have\n\
if that window were selected.\n\
\n\
Note that, when WINDOW is the selected window and its buffer\n\
is also currently selected, the value returned is the same as (point).\n\
It would be more strictly correct to return the `top-level' value\n\
of point, outside of any save-excursion forms.\n\
But that is hard to define.")
  (window)
     Lisp_Object window;
{
  register struct window *w = decode_window (window);

  if (w == XWINDOW (selected_window)
      && current_buffer == XBUFFER (w->buffer))
    return Fpoint ();
  return Fmarker_position (w->pointm);
}

DEFUN ("window-start", Fwindow_start, Swindow_start, 0, 1, 0,
  "Return position at which display currently starts in WINDOW.")
  (window)
     Lisp_Object window;
{
  return Fmarker_position (decode_window (window)->start);
}

/* This is text temporarily removed from the doc string below.

This function returns nil if the position is not currently known.\n\
That happens when redisplay is preempted and doesn't finish.\n\
If in that case you want to compute where the end of the window would\n\
have been if redisplay had finished, do this:\n\
    (save-excursion\n\
      (goto-char (window-start window))\n\
      (vertical-motion (1- (window-height window)) window)\n\
      (point))")  */

DEFUN ("window-end", Fwindow_end, Swindow_end, 0, 1, 0,
  "Return position at which display currently ends in WINDOW.\n\
This is updated by redisplay, when it runs to completion.\n\
Simply changing the buffer text or setting `window-start'\n\
does not update this value.")
  (window)
     Lisp_Object window;
{
  Lisp_Object value;
  struct window *w = decode_window (window);
  Lisp_Object buf;

  buf = w->buffer;
  CHECK_BUFFER (buf, 0);

#if 0 /* This change broke some things.  We should make it later.  */
  /* If we don't know the end position, return nil.
     The user can compute it with vertical-motion if he wants to.
     It would be nicer to do it automatically,
     but that's so slow that it would probably bother people.  */
  if (NILP (w->window_end_valid))
    return Qnil;
#endif

  XSETINT (value,
	   BUF_Z (XBUFFER (buf)) - XFASTINT (w->window_end_pos));

  return value;
}

DEFUN ("set-window-point", Fset_window_point, Sset_window_point, 2, 2, 0,
  "Make point value in WINDOW be at position POS in WINDOW's buffer.")
  (window, pos)
     Lisp_Object window, pos;
{
  register struct window *w = decode_window (window);

  CHECK_NUMBER_COERCE_MARKER (pos, 1);
  if (w == XWINDOW (selected_window))
    Fgoto_char (pos);
  else
    set_marker_restricted (w->pointm, pos, w->buffer);

  return pos;
}

DEFUN ("set-window-start", Fset_window_start, Sset_window_start, 2, 3, 0,
  "Make display in WINDOW start at position POS in WINDOW's buffer.\n\
Optional third arg NOFORCE non-nil inhibits next redisplay\n\
from overriding motion of point in order to display at this exact start.")
  (window, pos, noforce)
     Lisp_Object window, pos, noforce;
{
  register struct window *w = decode_window (window);

  CHECK_NUMBER_COERCE_MARKER (pos, 1);
  set_marker_restricted (w->start, pos, w->buffer);
  /* this is not right, but much easier than doing what is right. */
  w->start_at_line_beg = Qnil;
  if (NILP (noforce))
    w->force_start = Qt;
  w->update_mode_line = Qt;
  XSETFASTINT (w->last_modified, 0);
  if (!EQ (window, selected_window))
    windows_or_buffers_changed++;
  return pos;
}

DEFUN ("window-dedicated-p", Fwindow_dedicated_p, Swindow_dedicated_p,
       1, 1, 0,
  "Return WINDOW's dedicated object, usually t or nil.\n\
See also `set-window-dedicated-p'.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->dedicated;
}

DEFUN ("set-window-dedicated-p", Fset_window_dedicated_p,
       Sset_window_dedicated_p, 2, 2, 0,
  "Control whether WINDOW is dedicated to the buffer it displays.\n\
If it is dedicated, Emacs will not automatically change\n\
which buffer appears in it.\n\
The second argument is the new value for the dedication flag;\n\
non-nil means yes.")
  (window, arg)
       Lisp_Object window, arg;
{
  register struct window *w = decode_window (window);

  if (NILP (arg))
    w->dedicated = Qnil;
  else
    w->dedicated = Qt;

  return w->dedicated;
}

DEFUN ("window-display-table", Fwindow_display_table, Swindow_display_table,
       0, 1, 0,
  "Return the display-table that WINDOW is using.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->display_table;
}

/* Get the display table for use currently on window W.
   This is either W's display table or W's buffer's display table.
   Ignore the specified tables if they are not valid;
   if no valid table is specified, return 0.  */

struct Lisp_Vector *
window_display_table (w)
     struct window *w;
{
  Lisp_Object tem;
  tem = w->display_table;
  if (VECTORP (tem) && XVECTOR (tem)->size == DISP_TABLE_SIZE)
    return XVECTOR (tem);
  tem = XBUFFER (w->buffer)->display_table;
  if (VECTORP (tem) && XVECTOR (tem)->size == DISP_TABLE_SIZE)
    return XVECTOR (tem);
  tem = Vstandard_display_table;
  if (VECTORP (tem) && XVECTOR (tem)->size == DISP_TABLE_SIZE)
    return XVECTOR (tem);
  return 0;
}

DEFUN ("set-window-display-table", Fset_window_display_table, Sset_window_display_table, 2, 2, 0,
  "Set WINDOW's display-table to TABLE.")
  (window, table)
     register Lisp_Object window, table;
{
  register struct window *w;
  register Lisp_Object z;	/* Return value. */

  w = decode_window (window);
  w->display_table = table;
  return table;
}

/* Record info on buffer window w is displaying
   when it is about to cease to display that buffer.  */
static
unshow_buffer (w)
     register struct window *w;
{
  Lisp_Object buf;

  buf = w->buffer;
  if (XBUFFER (buf) != XMARKER (w->pointm)->buffer)
    abort ();

#if 0
  if (w == XWINDOW (selected_window)
      || ! EQ (buf, XWINDOW (selected_window)->buffer))
    /* Do this except when the selected window's buffer
       is being removed from some other window.  */
#endif
    /* last_window_start records the start position that this buffer
       had in the last window to be disconnected from it.
       Now that this statement is unconditional,
       it is possible for the buffer to be displayed in the
       selected window, while last_window_start reflects another
       window which was recently showing the same buffer.
       Some people might say that might be a good thing.  Let's see.  */
    XBUFFER (buf)->last_window_start = marker_position (w->start);

  /* Point in the selected window's buffer
     is actually stored in that buffer, and the window's pointm isn't used.
     So don't clobber point in that buffer.  */
  if (! EQ (buf, XWINDOW (selected_window)->buffer))
    BUF_PT (XBUFFER (buf))
      = clip_to_bounds (BUF_BEGV (XBUFFER (buf)),
			marker_position (w->pointm),
			BUF_ZV (XBUFFER (buf)));
}

/* Put replacement into the window structure in place of old. */
static
replace_window (old, replacement)
     Lisp_Object old, replacement;
{
  register Lisp_Object tem;
  register struct window *o = XWINDOW (old), *p = XWINDOW (replacement);

  /* If OLD is its frame's root_window, then replacement is the new
     root_window for that frame.  */

  if (EQ (old, FRAME_ROOT_WINDOW (XFRAME (o->frame))))
    FRAME_ROOT_WINDOW (XFRAME (o->frame)) = replacement;

  p->left = o->left;
  p->top = o->top;
  p->width = o->width;
  p->height = o->height;

  p->next = tem = o->next;
  if (!NILP (tem))
    XWINDOW (tem)->prev = replacement;

  p->prev = tem = o->prev;
  if (!NILP (tem))
    XWINDOW (tem)->next = replacement;

  p->parent = tem = o->parent;
  if (!NILP (tem))
    {
      if (EQ (XWINDOW (tem)->vchild, old))
	XWINDOW (tem)->vchild = replacement;
      if (EQ (XWINDOW (tem)->hchild, old))
	XWINDOW (tem)->hchild = replacement;
    }

/*** Here, if replacement is a vertical combination
and so is its new parent, we should make replacement's
children be children of that parent instead.  ***/
}

DEFUN ("delete-window", Fdelete_window, Sdelete_window, 0, 1, "",
  "Remove WINDOW from the display.  Default is selected window.")
  (window)
     register Lisp_Object window;
{
  register Lisp_Object tem, parent, sib;
  register struct window *p;
  register struct window *par;

  /* Because this function is called by other C code on non-leaf
     windows, the CHECK_LIVE_WINDOW macro would choke inappropriately,
     so we can't decode_window here.  */
  if (NILP (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);
  p = XWINDOW (window);

  /* It's okay to delete an already-deleted window.  */
  if (NILP (p->buffer)
      && NILP (p->hchild)
      && NILP (p->vchild))
    return Qnil;

  parent = p->parent;
  if (NILP (parent))
    error ("Attempt to delete minibuffer or sole ordinary window");
  par = XWINDOW (parent);

  windows_or_buffers_changed++;

  /* Are we trying to delete any frame's selected window?  */
  {
    Lisp_Object frame, pwindow;

    /* See if the frame's selected window is either WINDOW
       or any subwindow of it, by finding all that window's parents
       and comparing each one with WINDOW.  */
    frame = WINDOW_FRAME (XWINDOW (window));
    pwindow = FRAME_SELECTED_WINDOW (XFRAME (frame));

    while (!NILP (pwindow))
      {
	if (EQ (window, pwindow))
	  break;
	pwindow = XWINDOW (pwindow)->parent;
      }

    if (EQ (window, pwindow))
      {
	Lisp_Object alternative;
	alternative = Fnext_window (window, Qlambda, Qnil);

	/* If we're about to delete the selected window on the
	   selected frame, then we should use Fselect_window to select
	   the new window.  On the other hand, if we're about to
	   delete the selected window on any other frame, we shouldn't do
	   anything but set the frame's selected_window slot.  */
	if (EQ (window, selected_window))
	  Fselect_window (alternative);
	else
	  FRAME_SELECTED_WINDOW (XFRAME (frame)) = alternative;
      }
  }

  tem = p->buffer;
  /* tem is null for dummy parent windows
     (which have inferiors but not any contents themselves) */
  if (!NILP (tem))
    {
      unshow_buffer (p);
      unchain_marker (p->pointm);
      unchain_marker (p->start);
    }

  tem = p->next;
  if (!NILP (tem))
    XWINDOW (tem)->prev = p->prev;

  tem = p->prev;
  if (!NILP (tem))
    XWINDOW (tem)->next = p->next;

  if (EQ (window, par->hchild))
    par->hchild = p->next;
  if (EQ (window, par->vchild))
    par->vchild = p->next;

  /* Find one of our siblings to give our space to.  */
  sib = p->prev;
  if (NILP (sib))
    {
      /* If p gives its space to its next sibling, that sibling needs
	 to have its top/left side pulled back to where p's is.
	 set_window_{height,width} will re-position the sibling's
	 children.  */
      sib = p->next;
      XWINDOW (sib)->top = p->top;
      XWINDOW (sib)->left = p->left;
    }

  /* Stretch that sibling.  */
  if (!NILP (par->vchild))
    set_window_height (sib,
		       XFASTINT (XWINDOW (sib)->height) + XFASTINT (p->height),
		       1);
  if (!NILP (par->hchild))
    set_window_width (sib,
		      XFASTINT (XWINDOW (sib)->width) + XFASTINT (p->width),
		      1);

  /* If parent now has only one child,
     put the child into the parent's place.  */
  tem = par->hchild;
  if (NILP (tem))
    tem = par->vchild;
  if (NILP (XWINDOW (tem)->next))
    replace_window (parent, tem);

  /* Since we may be deleting combination windows, we must make sure that
     not only p but all its children have been marked as deleted.  */
  if (! NILP (p->hchild))
    delete_all_subwindows (XWINDOW (p->hchild));
  else if (! NILP (p->vchild))
    delete_all_subwindows (XWINDOW (p->vchild));

  /* Mark this window as deleted.  */
  p->buffer = p->hchild = p->vchild = Qnil;

  return Qnil;
}


extern Lisp_Object next_frame (), prev_frame ();

/* This comment supplies the doc string for `next-window',
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("next-window", Ffoo, Sfoo, 0, 3, 0,
  "Return next window after WINDOW in canonical ordering of windows.\n\
If omitted, WINDOW defaults to the selected window.\n\
\n\
Optional second arg MINIBUF t means count the minibuffer window even\n\
if not active.  MINIBUF nil or omitted means count the minibuffer iff\n\
it is active.  MINIBUF neither t nor nil means not to count the\n\
minibuffer even if it is active.\n\
\n\
Several frames may share a single minibuffer; if the minibuffer\n\
counts, all windows on all frames that share that minibuffer count\n\
too.  Therefore, `next-window' can be used to iterate through the\n\
set of windows even when the minibuffer is on another frame.  If the\n\
minibuffer does not count, only windows from WINDOW's frame count.\n\
\n\
Optional third arg ALL-FRAMES t means include windows on all frames.\n\
ALL-FRAMES nil or omitted means cycle within the frames as specified\n\
above.  ALL-FRAMES = `visible' means include windows on all visible frames.\n\
ALL-FRAMES = 0 means include windows on all visible and iconified frames.\n\
Anything else means restrict to WINDOW's frame.\n\
\n\
If you use consistent values for MINIBUF and ALL-FRAMES, you can use\n\
`next-window' to iterate through the entire cycle of acceptable\n\
windows, eventually ending up back at the window you started with.\n\
`previous-window' traverses the same cycle, in the reverse order.")
  (window, minibuf, all_frames) */

DEFUN ("next-window", Fnext_window, Snext_window, 0, 3, 0,
       0)
  (window, minibuf, all_frames)
     register Lisp_Object window, minibuf, all_frames;
{
  register Lisp_Object tem;
  Lisp_Object start_window;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_LIVE_WINDOW (window, 0);

  start_window = window;

  /* minibuf == nil may or may not include minibuffers.
     Decide if it does.  */
  if (NILP (minibuf))
    minibuf = (minibuf_level ? Qt : Qlambda);

#ifdef MULTI_FRAME
  /* all_frames == nil doesn't specify which frames to include.  */
  if (NILP (all_frames))
    all_frames = (EQ (minibuf, Qt)
		  ? (FRAME_MINIBUF_WINDOW
		     (XFRAME
		      (WINDOW_FRAME
		       (XWINDOW (window)))))
		  : Qnil);
  else if (EQ (all_frames, Qvisible))
    ;
  else if (XFASTINT (all_frames) == 0)
    ;
  else if (! EQ (all_frames, Qt))
    all_frames = Qnil;
  /* Now all_frames is t meaning search all frames,
     nil meaning search just current frame,
     visible meaning search just visible frames,
     0 meaning search visible and iconified frames,
     or a window, meaning search the frame that window belongs to.  */
#endif

  /* Do this loop at least once, to get the next window, and perhaps
     again, if we hit the minibuffer and that is not acceptable.  */
  do
    {
      /* Find a window that actually has a next one.  This loop
	 climbs up the tree.  */
      while (tem = XWINDOW (window)->next, NILP (tem))
	if (tem = XWINDOW (window)->parent, !NILP (tem))
	  window = tem;
	else
	  {
	    /* We've reached the end of this frame.
	       Which other frames are acceptable?  */
	    tem = WINDOW_FRAME (XWINDOW (window));
#ifdef MULTI_FRAME
	    if (! NILP (all_frames))
	      {
		Lisp_Object tem1;

		tem1 = tem;
		tem = next_frame (tem, all_frames);
		/* In the case where the minibuffer is active,
		   and we include its frame as well as the selected one,
		   next_frame may get stuck in that frame.
		   If that happens, go back to the selected frame
		   so we can complete the cycle.  */
		if (EQ (tem, tem1))
		  XSETFRAME (tem, selected_frame);
	      }
#endif
	    tem = FRAME_ROOT_WINDOW (XFRAME (tem));

	    break;
	  }

      window = tem;

      /* If we're in a combination window, find its first child and
	 recurse on that.  Otherwise, we've found the window we want.  */
      while (1)
	{
	  if (!NILP (XWINDOW (window)->hchild))
	    window = XWINDOW (window)->hchild;
	  else if (!NILP (XWINDOW (window)->vchild))
	    window = XWINDOW (window)->vchild;
	  else break;
	}
    }
  /* Which windows are acceptible?
     Exit the loop and accept this window if
     this isn't a minibuffer window, or
     we're accepting minibuffer windows, or
     we've come all the way around and we're back at the original window.  */
  while (MINI_WINDOW_P (XWINDOW (window))
	 && ! EQ (minibuf, Qt)
	 && ! EQ (window, start_window));

  return window;
}

/* This comment supplies the doc string for `previous-window',
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("previous-window", Ffoo, Sfoo, 0, 3, 0,
  "Return the window preceeding WINDOW in canonical ordering of windows.\n\
If omitted, WINDOW defaults to the selected window.\n\
\n\
Optional second arg MINIBUF t means count the minibuffer window even\n\
if not active.  MINIBUF nil or omitted means count the minibuffer iff\n\
it is active.  MINIBUF neither t nor nil means not to count the\n\
minibuffer even if it is active.\n\
\n\
Several frames may share a single minibuffer; if the minibuffer\n\
counts, all windows on all frames that share that minibuffer count\n\
too.  Therefore, `previous-window' can be used to iterate through\n\
the set of windows even when the minibuffer is on another frame.  If\n\
the minibuffer does not count, only windows from WINDOW's frame count\n\
\n\
Optional third arg ALL-FRAMES t means include windows on all frames.\n\
ALL-FRAMES nil or omitted means cycle within the frames as specified\n\
above.  ALL-FRAMES = `visible' means include windows on all visible frames.\n\
ALL-FRAMES = 0 means include windows on all visible and iconified frames.\n\
Anything else means restrict to WINDOW's frame.\n\
\n\
If you use consistent values for MINIBUF and ALL-FRAMES, you can use\n\
`previous-window' to iterate through the entire cycle of acceptable\n\
windows, eventually ending up back at the window you started with.\n\
`next-window' traverses the same cycle, in the reverse order.")
  (window, minibuf, all_frames)  */


DEFUN ("previous-window", Fprevious_window, Sprevious_window, 0, 3, 0,
       0)
  (window, minibuf, all_frames)
     register Lisp_Object window, minibuf, all_frames;
{
  register Lisp_Object tem;
  Lisp_Object start_window;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_LIVE_WINDOW (window, 0);

  start_window = window;

  /* minibuf == nil may or may not include minibuffers.
     Decide if it does.  */
  if (NILP (minibuf))
    minibuf = (minibuf_level ? Qt : Qlambda);

#ifdef MULTI_FRAME
  /* all_frames == nil doesn't specify which frames to include.
     Decide which frames it includes.  */
  if (NILP (all_frames))
    all_frames = (EQ (minibuf, Qt)
		   ? (FRAME_MINIBUF_WINDOW
		      (XFRAME
		       (WINDOW_FRAME
			(XWINDOW (window)))))
		   : Qnil);
  else if (EQ (all_frames, Qvisible))
    ;
  else if (XFASTINT (all_frames) == 0)
    ;
  else if (! EQ (all_frames, Qt))
    all_frames = Qnil;
  /* Now all_frames is t meaning search all frames,
     nil meaning search just current frame,
     visible meaning search just visible frames,
     0 meaning search visible and iconified frames,
     or a window, meaning search the frame that window belongs to.  */
#endif

  /* Do this loop at least once, to get the previous window, and perhaps
     again, if we hit the minibuffer and that is not acceptable.  */
  do
    {
      /* Find a window that actually has a previous one.  This loop
	 climbs up the tree.  */
      while (tem = XWINDOW (window)->prev, NILP (tem))
	if (tem = XWINDOW (window)->parent, !NILP (tem))
	  window = tem;
	else
	  {
	    /* We have found the top window on the frame.
	       Which frames are acceptable?  */
	    tem = WINDOW_FRAME (XWINDOW (window));
#ifdef MULTI_FRAME
	    if (! NILP (all_frames))
	      /* It's actually important that we use prev_frame here,
		 rather than next_frame.  All the windows acceptable
		 according to the given parameters should form a ring;
		 Fnext_window and Fprevious_window should go back and
		 forth around the ring.  If we use next_frame here,
		 then Fnext_window and Fprevious_window take different
		 paths through the set of acceptable windows.
		 window_loop assumes that these `ring' requirement are
		 met.  */
	      {
		Lisp_Object tem1;

		tem1 = tem;
		tem = prev_frame (tem, all_frames);
		/* In the case where the minibuffer is active,
		   and we include its frame as well as the selected one,
		   next_frame may get stuck in that frame.
		   If that happens, go back to the selected frame
		   so we can complete the cycle.  */
		if (EQ (tem, tem1))
		  XSETFRAME (tem, selected_frame);
	      }
#endif
	    /* If this frame has a minibuffer, find that window first,
	       because it is conceptually the last window in that frame.  */
	    if (FRAME_HAS_MINIBUF_P (XFRAME (tem)))
	      tem = FRAME_MINIBUF_WINDOW (XFRAME (tem));
	    else
	      tem = FRAME_ROOT_WINDOW (XFRAME (tem));

	    break;
	  }

      window = tem;
      /* If we're in a combination window, find its last child and
	 recurse on that.  Otherwise, we've found the window we want.  */
      while (1)
	{
	  if (!NILP (XWINDOW (window)->hchild))
	    window = XWINDOW (window)->hchild;
	  else if (!NILP (XWINDOW (window)->vchild))
	    window = XWINDOW (window)->vchild;
	  else break;
	  while (tem = XWINDOW (window)->next, !NILP (tem))
	    window = tem;
	}
    }
  /* Which windows are acceptable?
     Exit the loop and accept this window if
     this isn't a minibuffer window, or
     we're accepting minibuffer windows, or
     we've come all the way around and we're back at the original window.  */
  while (MINI_WINDOW_P (XWINDOW (window))
	 && !EQ (minibuf, Qt)
	 && !EQ (window, start_window));

  return window;
}

DEFUN ("other-window", Fother_window, Sother_window, 1, 2, "p",
  "Select the ARG'th different window on this frame.\n\
All windows on current frame are arranged in a cyclic order.\n\
This command selects the window ARG steps away in that order.\n\
A negative ARG moves in the opposite order.  If the optional second\n\
argument ALL_FRAMES is non-nil, cycle through all frames.")
  (n, all_frames)
     register Lisp_Object n, all_frames;
{
  register int i;
  register Lisp_Object w;

  CHECK_NUMBER (n, 0);
  w = selected_window;
  i = XINT (n);

  while (i > 0)
    {
      w = Fnext_window (w, Qnil, all_frames);
      i--;
    }
  while (i < 0)
    {
      w = Fprevious_window (w, Qnil, all_frames);
      i++;
    }
  Fselect_window (w);
  return Qnil;
}

/* Look at all windows, performing an operation specified by TYPE
   with argument OBJ.
   If FRAMES is Qt, look at all frames;
                Qnil, look at just the selected frame;
		Qvisible, look at visible frames;
	        a frame, just look at windows on that frame.
   If MINI is non-zero, perform the operation on minibuffer windows too.
*/

enum window_loop
{
  WINDOW_LOOP_UNUSED,
  GET_BUFFER_WINDOW,		/* Arg is buffer */
  GET_LRU_WINDOW,		/* Arg is t for full-width windows only */
  DELETE_OTHER_WINDOWS,		/* Arg is window not to delete */
  DELETE_BUFFER_WINDOWS,	/* Arg is buffer */
  GET_LARGEST_WINDOW,
  UNSHOW_BUFFER		/* Arg is buffer */
};

static Lisp_Object
window_loop (type, obj, mini, frames)
     enum window_loop type;
     register Lisp_Object obj, frames;
     int mini;
{
  register Lisp_Object w;
  register Lisp_Object best_window;
  register Lisp_Object next_window;
  register Lisp_Object last_window;
  FRAME_PTR frame;
  Lisp_Object frame_arg;
  frame_arg = Qt;

#ifdef MULTI_FRAME
  /* If we're only looping through windows on a particular frame,
     frame points to that frame.  If we're looping through windows
     on all frames, frame is 0.  */
  if (FRAMEP (frames))
    frame = XFRAME (frames);
  else if (NILP (frames))
    frame = selected_frame;
  else
    frame = 0;
  if (frame)
    frame_arg = Qlambda;
  else if (XFASTINT (frames) == 0)
    frame_arg = frames;
  else if (EQ (frames, Qvisible))
    frame_arg = frames;
#else
  frame = 0;
#endif

  /* frame_arg is Qlambda to stick to one frame,
     Qvisible to consider all visible frames,
     or Qt otherwise.  */

  /* Pick a window to start with.  */
  if (WINDOWP (obj))
    w = obj;
  else if (frame)
    w = FRAME_SELECTED_WINDOW (frame);
  else
    w = FRAME_SELECTED_WINDOW (selected_frame);

  /* Figure out the last window we're going to mess with.  Since
     Fnext_window, given the same options, is guaranteed to go in a
     ring, we can just use Fprevious_window to find the last one.

     We can't just wait until we hit the first window again, because
     it might be deleted.  */

  last_window = Fprevious_window (w, mini ? Qt : Qnil, frame_arg);

  best_window = Qnil;
  for (;;)
    {
      FRAME_PTR w_frame = XFRAME (WINDOW_FRAME (XWINDOW (w)));

      /* Pick the next window now, since some operations will delete
	 the current window.  */
      next_window = Fnext_window (w, mini ? Qt : Qnil, frame_arg);

      /* Note that we do not pay attention here to whether
	 the frame is visible, since Fnext_window skips non-visible frames
	 if that is desired, under the control of frame_arg.  */
      if (! MINI_WINDOW_P (XWINDOW (w))
	  || (mini && minibuf_level > 0))
	switch (type)
	  {
	  case GET_BUFFER_WINDOW:
	    if (XBUFFER (XWINDOW (w)->buffer) == XBUFFER (obj))
	      return w;
	    break;

	  case GET_LRU_WINDOW:
	    /* t as arg means consider only full-width windows */
	    if (!NILP (obj) && XFASTINT (XWINDOW (w)->width)
		!= FRAME_WIDTH (XFRAME (WINDOW_FRAME (XWINDOW (w)))))
	      break;
	    /* Ignore dedicated windows and minibuffers.  */
	    if (MINI_WINDOW_P (XWINDOW (w))
		|| !NILP (XWINDOW (w)->dedicated))
	      break;
	    if (NILP (best_window)
		|| (XFASTINT (XWINDOW (best_window)->use_time)
		    > XFASTINT (XWINDOW (w)->use_time)))
	      best_window = w;
	    break;

	  case DELETE_OTHER_WINDOWS:
	    if (XWINDOW (w) != XWINDOW (obj))
	      Fdelete_window (w);
	    break;

	  case DELETE_BUFFER_WINDOWS:
	    if (EQ (XWINDOW (w)->buffer, obj))
	      {
		/* If we're deleting the buffer displayed in the only window
		   on the frame, find a new buffer to display there.  */
		if (NILP (XWINDOW (w)->parent))
		  {
		    Lisp_Object new_buffer;
		    new_buffer = Fother_buffer (obj, Qnil);
		    if (NILP (new_buffer))
		      new_buffer
			= Fget_buffer_create (build_string ("*scratch*"));
		    Fset_window_buffer (w, new_buffer);
		    if (EQ (w, selected_window))
		      Fset_buffer (XWINDOW (w)->buffer);
		  }
		else
		  Fdelete_window (w);
	      }
	    break;

	  case GET_LARGEST_WINDOW:
	    /* Ignore dedicated windows and minibuffers.  */
	    if (MINI_WINDOW_P (XWINDOW (w))
		|| !NILP (XWINDOW (w)->dedicated))
	      break;
	    {
	      struct window *best_window_ptr = XWINDOW (best_window);
	      struct window *w_ptr = XWINDOW (w);
	      if (NILP (best_window)
		  || (XFASTINT (w_ptr->height) * XFASTINT (w_ptr->width)
		      > (XFASTINT (best_window_ptr->height)
			 * XFASTINT (best_window_ptr->width))))
		best_window = w;
	    }
	    break;

	  case UNSHOW_BUFFER:
	    if (EQ (XWINDOW (w)->buffer, obj))
	      {
		/* Find another buffer to show in this window.  */
		Lisp_Object another_buffer;
		FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (w)));
		another_buffer = Fother_buffer (obj, Qnil);
		if (NILP (another_buffer))
		  another_buffer
		    = Fget_buffer_create (build_string ("*scratch*"));
#ifdef MULTI_FRAME
		/* If this window is dedicated, and in a frame of its own,
		   kill the frame.  */
		if (EQ (w, FRAME_ROOT_WINDOW (f))
		    && !NILP (XWINDOW (w)->dedicated)
		    && other_visible_frames (f))
		  {
		    /* Skip the other windows on this frame.
		       There might be one, the minibuffer!  */
		    if (! EQ (w, last_window))
		      while (f == XFRAME (WINDOW_FRAME (XWINDOW (next_window))))
			{
			  /* As we go, check for the end of the loop.
			     We mustn't start going around a second time.  */
			  if (EQ (next_window, last_window))
			    {
			      last_window = w;
			      break;
			    }
			  next_window = Fnext_window (next_window,
						      mini ? Qt : Qnil,
						      frame_arg);
			}
		    /* Now we can safely delete the frame.  */
		    Fdelete_frame (WINDOW_FRAME (XWINDOW (w)), Qnil);
		  }
		else
#endif
		  {
		    /* Otherwise show a different buffer in the window.  */
		    XWINDOW (w)->dedicated = Qnil;
		    Fset_window_buffer (w, another_buffer);
		    if (EQ (w, selected_window))
		      Fset_buffer (XWINDOW (w)->buffer);
		  }
	      }
	    break;
	  }

      if (EQ (w, last_window))
	break;

      w = next_window;
    }

  return best_window;
}     

DEFUN ("get-lru-window", Fget_lru_window, Sget_lru_window, 0, 1, 0,
  "Return the window least recently selected or used for display.\n\
If optional argument FRAME is `visible', search all visible frames.\n\
If FRAME is 0, search all visible and iconified frames.\n\
If FRAME is t, search all frames.\n\
If FRAME is nil, search only the selected frame.\n\
If FRAME is a frame, search only that frame.")
  (frame)
    Lisp_Object frame;
{
  register Lisp_Object w;
  /* First try for a window that is full-width */
  w = window_loop (GET_LRU_WINDOW, Qt, 0, frame);
  if (!NILP (w) && !EQ (w, selected_window))
    return w;
  /* If none of them, try the rest */
  return window_loop (GET_LRU_WINDOW, Qnil, 0, frame);
}

DEFUN ("get-largest-window", Fget_largest_window, Sget_largest_window, 0, 1, 0,
  "Return the largest window in area.\n\
If optional argument FRAME is `visible', search all visible frames.\n\
If FRAME is 0, search all visible and iconified frames.\n\
If FRAME is t, search all frames.\n\
If FRAME is nil, search only the selected frame.\n\
If FRAME is a frame, search only that frame.")
  (frame)
    Lisp_Object frame;
{
  return window_loop (GET_LARGEST_WINDOW, Qnil, 0,
		      frame);
}

DEFUN ("get-buffer-window", Fget_buffer_window, Sget_buffer_window, 1, 2, 0,
  "Return a window currently displaying BUFFER, or nil if none.\n\
If optional argument FRAME is `visible', search all visible frames.\n\
If optional argument FRAME is 0, search all visible and iconified frames.\n\
If FRAME is t, search all frames.\n\
If FRAME is nil, search only the selected frame.\n\
If FRAME is a frame, search only that frame.")
  (buffer, frame)
    Lisp_Object buffer, frame;
{
  buffer = Fget_buffer (buffer);
  if (BUFFERP (buffer))
    return window_loop (GET_BUFFER_WINDOW, buffer, 1, frame);
  else
    return Qnil;
}

DEFUN ("delete-other-windows", Fdelete_other_windows, Sdelete_other_windows,
  0, 1, "",
  "Make WINDOW (or the selected window) fill its frame.\n\
Only the frame WINDOW is on is affected.\n\
This function tries to reduce display jumps\n\
by keeping the text previously visible in WINDOW\n\
in the same place on the frame.  Doing this depends on\n\
the value of (window-start WINDOW), so if calling this function\n\
in a program gives strange scrolling, make sure the window-start\n\
value is reasonable when this function is called.")
  (window)
     Lisp_Object window;
{
  struct window *w;
  int startpos;
  int top;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_LIVE_WINDOW (window, 0);

  w = XWINDOW (window);

  startpos = marker_position (w->start);
  top = XFASTINT (w->top) - FRAME_MENU_BAR_LINES (XFRAME (WINDOW_FRAME (w)));

  if (MINI_WINDOW_P (w) && top > 0)
    error ("Can't expand minibuffer to full frame");

  window_loop (DELETE_OTHER_WINDOWS, window, 0, WINDOW_FRAME (w));

  /* Try to minimize scrolling, by setting the window start to the point
     will cause the text at the old window start to be at the same place
     on the frame.  But don't try to do this if the window start is
     outside the visible portion (as might happen when the display is
     not current, due to typeahead).  */
  if (startpos >= BUF_BEGV (XBUFFER (w->buffer))
      && startpos <= BUF_ZV (XBUFFER (w->buffer)))
    {
      struct position pos;
      struct buffer *obuf = current_buffer;

      Fset_buffer (w->buffer);
      /* This computation used to temporarily move point, but that can
	 have unwanted side effects due to text properties.  */
      pos = *vmotion (startpos, -top, window_internal_width (w) - 1,
		      XINT (w->hscroll), window);
      Fset_marker (w->start, make_number (pos.bufpos), w->buffer);
      w->start_at_line_beg = ((pos.bufpos == BEGV
			       || FETCH_CHAR (pos.bufpos - 1) == '\n') ? Qt
			      : Qnil);

      set_buffer_internal (obuf);
    }
  return Qnil;
}

DEFUN ("delete-windows-on", Fdelete_windows_on, Sdelete_windows_on,
  1, 2, "bDelete windows on (buffer): ",
  "Delete all windows showing BUFFER.\n\
Optional second argument FRAME controls which frames are affected.\n\
If nil or omitted, delete all windows showing BUFFER in any frame.\n\
If t, delete only windows showing BUFFER in the selected frame.\n\
If `visible', delete all windows showing BUFFER in any visible frame.\n\
If a frame, delete only windows showing BUFFER in that frame.")
  (buffer, frame)
     Lisp_Object buffer, frame;
{
#ifdef MULTI_FRAME
  /* FRAME uses t and nil to mean the opposite of what window_loop
     expects.  */
  if (! FRAMEP (frame))
    frame = NILP (frame) ? Qt : Qnil;
#else
  frame = Qt;
#endif

  if (!NILP (buffer))
    {
      buffer = Fget_buffer (buffer);
      CHECK_BUFFER (buffer, 0);
      window_loop (DELETE_BUFFER_WINDOWS, buffer, 0, frame);
    }
  return Qnil;
}

DEFUN ("replace-buffer-in-windows", Freplace_buffer_in_windows,
  Sreplace_buffer_in_windows,
  1, 1, "bReplace buffer in windows: ",
  "Replace BUFFER with some other buffer in all windows showing it.")
  (buffer)
     Lisp_Object buffer;
{
  if (!NILP (buffer))
    {
      buffer = Fget_buffer (buffer);
      CHECK_BUFFER (buffer, 0);
      window_loop (UNSHOW_BUFFER, buffer, 0, Qt);
    }
  return Qnil;
}

/* Set the height of WINDOW and all its inferiors.  */

/* The smallest acceptable dimensions for a window.  Anything smaller
   might crash Emacs.  */
#define MIN_SAFE_WINDOW_WIDTH  (2)
#define MIN_SAFE_WINDOW_HEIGHT (2)

/* Make sure that window_min_height and window_min_width are
   not too small; if they are, set them to safe minima.  */

static void
check_min_window_sizes ()
{
  /* Smaller values might permit a crash.  */
  if (window_min_width < MIN_SAFE_WINDOW_WIDTH)
    window_min_width = MIN_SAFE_WINDOW_WIDTH;
  if (window_min_height < MIN_SAFE_WINDOW_HEIGHT)
    window_min_height = MIN_SAFE_WINDOW_HEIGHT;
}

/* If *ROWS or *COLS are too small a size for FRAME, set them to the
   minimum allowable size.  */
void
check_frame_size (frame, rows, cols)
     FRAME_PTR frame;
     int *rows, *cols;
{
  /* For height, we have to see:
     whether the frame has a minibuffer, 
     whether it wants a mode line, and
     whether it has a menu bar.  */
  int min_height =
    (FRAME_MINIBUF_ONLY_P (frame) ? MIN_SAFE_WINDOW_HEIGHT - 1
     : (! FRAME_HAS_MINIBUF_P (frame)) ? MIN_SAFE_WINDOW_HEIGHT
     : 2 * MIN_SAFE_WINDOW_HEIGHT - 1);
  if (FRAME_MENU_BAR_LINES (frame) > 0)
    min_height += FRAME_MENU_BAR_LINES (frame);

  if (*rows < min_height)
    *rows = min_height;
  if (*cols  < MIN_SAFE_WINDOW_WIDTH)
    *cols = MIN_SAFE_WINDOW_WIDTH;
}

/* Normally the window is deleted if it gets too small.
   nodelete nonzero means do not do this.
   (The caller should check later and do so if appropriate)  */

set_window_height (window, height, nodelete)
     Lisp_Object window;
     int height;
     int nodelete;
{
  register struct window *w = XWINDOW (window);
  register struct window *c;
  int oheight = XFASTINT (w->height);
  int top, pos, lastbot, opos, lastobot;
  Lisp_Object child;

  check_min_window_sizes ();

  if (!nodelete
      && ! NILP (w->parent)
      && height < window_min_height)
    {
      Fdelete_window (window);
      return;
    }

  XSETFASTINT (w->last_modified, 0);
  windows_or_buffers_changed++;
  XSETFASTINT (w->height, height);
  if (!NILP (w->hchild))
    {
      for (child = w->hchild; !NILP (child); child = XWINDOW (child)->next)
	{
	  XWINDOW (child)->top = w->top;
	  set_window_height (child, height, nodelete);
	}
    }
  else if (!NILP (w->vchild))
    {
      lastbot = top = XFASTINT (w->top);
      lastobot = 0;
      for (child = w->vchild; !NILP (child); child = c->next)
	{
	  c = XWINDOW (child);

	  opos = lastobot + XFASTINT (c->height);

	  XSETFASTINT (c->top, lastbot);

	  pos = (((opos * height) << 1) + oheight) / (oheight << 1);

	  /* Avoid confusion: inhibit deletion of child if becomes too small */
	  set_window_height (child, pos + top - lastbot, 1);

	  /* Now advance child to next window,
	     and set lastbot if child was not just deleted.  */
	  lastbot = pos + top;
	  lastobot = opos;
	}
      /* Now delete any children that became too small.  */
      if (!nodelete)
	for (child = w->vchild; !NILP (child); child = XWINDOW (child)->next)
	  {
	    set_window_height (child, XINT (XWINDOW (child)->height), 0);
	  }
    }
}

/* Recursively set width of WINDOW and its inferiors. */

set_window_width (window, width, nodelete)
     Lisp_Object window;
     int width;
     int nodelete;
{
  register struct window *w = XWINDOW (window);
  register struct window *c;
  int owidth = XFASTINT (w->width);
  int left, pos, lastright, opos, lastoright;
  Lisp_Object child;

  if (!nodelete && width < window_min_width && !NILP (w->parent))
    {
      Fdelete_window (window);
      return;
    }

  XSETFASTINT (w->last_modified, 0);
  windows_or_buffers_changed++;
  XSETFASTINT (w->width, width);
  if (!NILP (w->vchild))
    {
      for (child = w->vchild; !NILP (child); child = XWINDOW (child)->next)
	{
	  XWINDOW (child)->left = w->left;
	  set_window_width (child, width, nodelete);
	}
    }
  else if (!NILP (w->hchild))
    {
      lastright = left = XFASTINT (w->left);
      lastoright = 0;
      for (child = w->hchild; !NILP (child); child = c->next)
	{
	  c = XWINDOW (child);

	  opos = lastoright + XFASTINT (c->width);

	  XSETFASTINT (c->left, lastright);

	  pos = (((opos * width) << 1) + owidth) / (owidth << 1);

	  /* Inhibit deletion for becoming too small */
	  set_window_width (child, pos + left - lastright, 1);

	  /* Now advance child to next window,
	     and set lastright if child was not just deleted.  */
	  lastright = pos + left, lastoright = opos;
	}
      /* Delete children that became too small */
      if (!nodelete)
	for (child = w->hchild; !NILP (child); child = XWINDOW (child)->next)
	  {
	    set_window_width (child, XINT (XWINDOW (child)->width), 0);
	  }
    }
}

int window_select_count;

DEFUN ("set-window-buffer", Fset_window_buffer, Sset_window_buffer, 2, 2, 0,
  "Make WINDOW display BUFFER as its contents.\n\
BUFFER can be a buffer or buffer name.")
  (window, buffer)
     register Lisp_Object window, buffer;
{
  register Lisp_Object tem;
  register struct window *w = decode_window (window);

  buffer = Fget_buffer (buffer);
  CHECK_BUFFER (buffer, 1);

  if (NILP (XBUFFER (buffer)->name))
    error ("Attempt to display deleted buffer");

  tem = w->buffer;
  if (NILP (tem))
    error ("Window is deleted");
  else if (! EQ (tem, Qt))	/* w->buffer is t when the window
				   is first being set up.  */
    {
      if (!NILP (w->dedicated) && !EQ (tem, buffer))
	error ("Window is dedicated to `%s'",
	       XSTRING (XBUFFER (tem)->name)->data);

      unshow_buffer (w);
    }

  w->buffer = buffer;
  XSETFASTINT (w->window_end_pos, 0);
  w->window_end_valid = Qnil;
  XSETFASTINT(w->hscroll, 0);
  Fset_marker (w->pointm,
	       make_number (BUF_PT (XBUFFER (buffer))),
	       buffer);
  set_marker_restricted (w->start,
			 make_number (XBUFFER (buffer)->last_window_start),
			 buffer);
  w->start_at_line_beg = Qnil;
  w->force_start = Qnil;
  XSETFASTINT (w->last_modified, 0);
  windows_or_buffers_changed++;
  if (EQ (window, selected_window))
    Fset_buffer (buffer);

  return Qnil;
}

DEFUN ("select-window", Fselect_window, Sselect_window, 1, 1, 0,
  "Select WINDOW.  Most editing will apply to WINDOW's buffer.\n\
The main editor command loop selects the buffer of the selected window\n\
before each command.")
  (window)
     register Lisp_Object window;
{
  register struct window *w;
  register struct window *ow = XWINDOW (selected_window);

  CHECK_LIVE_WINDOW (window, 0);

  w = XWINDOW (window);

  if (NILP (w->buffer))
    error ("Trying to select deleted window or non-leaf window");

  XSETFASTINT (w->use_time, ++window_select_count);
  if (EQ (window, selected_window))
    return window;

  Fset_marker (ow->pointm, make_number (BUF_PT (XBUFFER (ow->buffer))),
	       ow->buffer);

  selected_window = window;
#ifdef MULTI_FRAME
  if (XFRAME (WINDOW_FRAME (w)) != selected_frame)
    {
      XFRAME (WINDOW_FRAME (w))->selected_window = window;
      /* Use this rather than Fhandle_switch_frame
	 so that FRAME_FOCUS_FRAME is moved appropriately as we
	 move around in the state where a minibuffer in a separate
	 frame is active.  */
      Fselect_frame (WINDOW_FRAME (w), Qnil);
    }
  else
    selected_frame->selected_window = window;
#endif

  record_buffer (w->buffer);
  Fset_buffer (w->buffer);

  /* Go to the point recorded in the window.
     This is important when the buffer is in more
     than one window.  It also matters when
     redisplay_window has altered point after scrolling,
     because it makes the change only in the window.  */
  {
    register int new_point = marker_position (w->pointm);
    if (new_point < BEGV)
      SET_PT (BEGV);
    else if (new_point > ZV)
      SET_PT (ZV);
    else
      SET_PT (new_point);
  }

  windows_or_buffers_changed++;
  return window;
}

DEFUN ("display-buffer", Fdisplay_buffer, Sdisplay_buffer, 1, 2,
       "bDisplay buffer: \nP",
  "Make BUFFER appear in some window but don't select it.\n\
BUFFER can be a buffer or a buffer name.\n\
If BUFFER is shown already in some window, just use that one,\n\
unless the window is the selected window and the optional second\n\
argument NOT-THIS-WINDOW is non-nil (interactively, with prefix arg).\n\
If `pop-up-frames' is non-nil, make a new frame if no window shows BUFFER.\n\
Returns the window displaying BUFFER.")
  (buffer, not_this_window)
     register Lisp_Object buffer, not_this_window;
{
  register Lisp_Object window, tem;

  buffer = Fget_buffer (buffer);
  CHECK_BUFFER (buffer, 0);

  if (!NILP (Vdisplay_buffer_function))
    return call2 (Vdisplay_buffer_function, buffer, not_this_window);

  if (NILP (not_this_window)
      && XBUFFER (XWINDOW (selected_window)->buffer) == XBUFFER (buffer))
    return selected_window;

#ifdef MULTI_FRAME
  /* If pop_up_frames,
     look for a window showing BUFFER on any visible or iconified frame.  */
  window = Fget_buffer_window (buffer, pop_up_frames ? make_number (0) : Qnil);
#else
  window = Fget_buffer_window (buffer, Qnil);
#endif
  if (!NILP (window)
      && (NILP (not_this_window) || !EQ (window, selected_window)))
    {
#ifdef MULTI_FRAME
      if (FRAME_ICONIFIED_P (XFRAME (WINDOW_FRAME (XWINDOW (window)))))
	Fmake_frame_visible (WINDOW_FRAME (XWINDOW (window)));
#endif
      return window;
    }

  /* Certain buffer names get special handling.  */
  if (! NILP (Vspecial_display_function))
    {
      tem = Fmember (XBUFFER (buffer)->name, Vspecial_display_buffer_names);
      if (!NILP (tem))
	return call1 (Vspecial_display_function, buffer);

      tem = Fassoc (XBUFFER (buffer)->name, Vspecial_display_buffer_names);
      if (!NILP (tem))
	return call2 (Vspecial_display_function, buffer, XCONS (tem)->cdr);
      
      for (tem = Vspecial_display_regexps; CONSP (tem); tem = XCONS (tem)->cdr)
	{
	  Lisp_Object car = XCONS (tem)->car;
	  if (STRINGP (car)
	      && fast_string_match (car, XBUFFER (buffer)->name) >= 0)
	    return call1 (Vspecial_display_function, buffer);
	  else if (CONSP (car)
		   && STRINGP (XCONS (car)->car)
		   && fast_string_match (XCONS (car)->car,
					 XBUFFER (buffer)->name) >= 0)
	    return call2 (Vspecial_display_function,
			  buffer,
			  XCONS (car)->cdr);
	}
    }

#ifdef MULTI_FRAME
  /* If there are no frames open that have more than a minibuffer,
     we need to create a new frame.  */
  if (pop_up_frames || last_nonminibuf_frame == 0)
    {
      window = Fframe_selected_window (call0 (Vpop_up_frame_function));
      Fset_window_buffer (window, buffer);
      return window;
    }
#endif /* MULTI_FRAME */

  if (pop_up_windows
#ifdef MULTI_FRAME
      || FRAME_MINIBUF_ONLY_P (selected_frame)
      /* If the current frame is a special display frame,
	 don't try to reuse its windows.  */
      || !NILP (XWINDOW (FRAME_ROOT_WINDOW (selected_frame))->dedicated)
#endif
      )
    {
      Lisp_Object frames;

      frames = Qnil;      
#ifdef MULTI_FRAME
      if (FRAME_MINIBUF_ONLY_P (selected_frame))
	XSETFRAME (frames, last_nonminibuf_frame);
#endif
      /* Don't try to create a window if would get an error */
      if (split_height_threshold < window_min_height << 1)
	split_height_threshold = window_min_height << 1;

      /* Note that both Fget_largest_window and Fget_lru_window
	 ignore minibuffers and dedicated windows.
	 This means they can return nil.  */

#ifdef MULTI_FRAME
      /* If the frame we would try to split cannot be split,
	 try other frames.  */
      if (FRAME_NO_SPLIT_P (NILP (frames) ? selected_frame
			    : last_nonminibuf_frame))
	{
	  /* Try visible frames first.  */
	  window = Fget_largest_window (Qvisible);
	  /* If that didn't work, try iconified frames.  */
	  if (NILP (window))
	    window = Fget_largest_window (make_number (0));
	  if (NILP (window))
	    window = Fget_largest_window (Qt);
	}
      else
#endif
	window = Fget_largest_window (frames);

      /* If we got a tall enough full-width window that can be split,
	 split it.  */
      if (!NILP (window)
	  && ! FRAME_NO_SPLIT_P (XFRAME (XWINDOW (window)->frame))
	  && window_height (window) >= split_height_threshold
	  && (XFASTINT (XWINDOW (window)->width)
	      == FRAME_WIDTH (XFRAME (WINDOW_FRAME (XWINDOW (window))))))
	window = Fsplit_window (window, Qnil, Qnil);
      else
	{
	  Lisp_Object upper, lower, other;

	  window = Fget_lru_window (frames);
	  /* If the LRU window is selected, and big enough,
	     and can be split, split it.  */
	  if (!NILP (window)
	      && ! FRAME_NO_SPLIT_P (XFRAME (XWINDOW (window)->frame))
	      && (EQ (window, selected_window)
		  || EQ (XWINDOW (window)->parent, Qnil))
	      && window_height (window) >= window_min_height << 1)
	    window = Fsplit_window (window, Qnil, Qnil);
#ifdef MULTI_FRAME
	  /* If Fget_lru_window returned nil, try other approaches.  */
	  /* Try visible frames first.  */
	  if (NILP (window))
	    window = Fget_largest_window (Qvisible);
	  /* If that didn't work, try iconified frames.  */
	  if (NILP (window))
	    window = Fget_largest_window (make_number (0));
	  /* Try invisible frames.  */
	  if (NILP (window))
	    window = Fget_largest_window (Qt);
	  /* As a last resort, make a new frame.  */
	  if (NILP (window))
	    window = Fframe_selected_window (call0 (Vpop_up_frame_function));
#else
	  /* As a last resort, use a non minibuffer window.  */
	  if (NILP (window))
	    window = Fframe_first_window (Fselected_frame ());
#endif
	  /* If window appears above or below another,
	     even out their heights.  */
	  if (!NILP (XWINDOW (window)->prev))
	    other = upper = XWINDOW (window)->prev, lower = window;
	  if (!NILP (XWINDOW (window)->next))
	    other = lower = XWINDOW (window)->next, upper = window;
	  if (!NILP (other)
	      /* Check that OTHER and WINDOW are vertically arrayed.  */
	      && XWINDOW (other)->top != XWINDOW (window)->top
	      && XWINDOW (other)->height > XWINDOW (window)->height)
	    {
	      int total = XWINDOW (other)->height + XWINDOW (window)->height;
	      Lisp_Object old_selected_window;
	      old_selected_window = selected_window;

	      selected_window = upper;
	      change_window_height (total / 2 - XWINDOW (upper)->height, 0);
	      selected_window = old_selected_window;
	    }
	}
    }
  else
    window = Fget_lru_window (Qnil);

  Fset_window_buffer (window, buffer);
  return window;
}

void
temp_output_buffer_show (buf)
     register Lisp_Object buf;
{
  register struct buffer *old = current_buffer;
  register Lisp_Object window;
  register struct window *w;

  Fset_buffer (buf);
  XBUFFER (buf)->save_modified = MODIFF;
  BEGV = BEG;
  ZV = Z;
  SET_PT (BEG);
  clip_changed = 1;
  set_buffer_internal (old);

  if (!EQ (Vtemp_buffer_show_function, Qnil))
    call1 (Vtemp_buffer_show_function, buf);
  else
    {
      window = Fdisplay_buffer (buf, Qnil);

#ifdef MULTI_FRAME
      if (XFRAME (XWINDOW (window)->frame) != selected_frame)
	Fmake_frame_visible (WINDOW_FRAME (XWINDOW (window)));
#endif /* MULTI_FRAME */
      Vminibuf_scroll_window = window;
      w = XWINDOW (window);
      XSETFASTINT (w->hscroll, 0);
      set_marker_restricted (w->start, make_number (1), buf);
      set_marker_restricted (w->pointm, make_number (1), buf);
    }
}

static
make_dummy_parent (window)
     Lisp_Object window;
{
  Lisp_Object new;
  register struct window *o, *p;
  register struct Lisp_Vector *vec;
  int i;

  o = XWINDOW (window);
  vec = allocate_vectorlike ((EMACS_INT)VECSIZE (struct window));
  for (i = 0; i < VECSIZE (struct window); ++i)
    vec->contents[i] = ((struct Lisp_Vector *)o)->contents[i];
  vec->size = VECSIZE (struct window);
  p = (struct window *)vec;
  XSETWINDOW (new, p);

  XSETFASTINT (p->sequence_number, ++sequence_number);

  /* Put new into window structure in place of window */
  replace_window (window, new);

  o->next = Qnil;
  o->prev = Qnil;
  o->vchild = Qnil;
  o->hchild = Qnil;
  o->parent = new;

  p->start = Qnil;
  p->pointm = Qnil;
  p->buffer = Qnil;
}

DEFUN ("split-window", Fsplit_window, Ssplit_window, 0, 3, "",
  "Split WINDOW, putting SIZE lines in the first of the pair.\n\
WINDOW defaults to selected one and SIZE to half its size.\n\
If optional third arg HOR-FLAG is non-nil, split side by side\n\
and put SIZE columns in the first of the pair.")
  (window, chsize, horflag)
     Lisp_Object window, chsize, horflag;
{
  register Lisp_Object new;
  register struct window *o, *p;
  register int size;

  if (NILP (window))
    window = selected_window;
  else
    CHECK_LIVE_WINDOW (window, 0);

  o = XWINDOW (window);

  if (NILP (chsize))
    {
      if (!NILP (horflag))
	/* Round odd size up, since this is for the left-hand window,
	   and it will lose a column for the separators.  */
	size = ((XFASTINT (o->width) + 1) & -2) >> 1;
      else
	size = XFASTINT (o->height) >> 1;
    }
  else
    {
      CHECK_NUMBER (chsize, 1);
      size = XINT (chsize);
    }

  if (MINI_WINDOW_P (o))
    error ("Attempt to split minibuffer window");
  else if (FRAME_NO_SPLIT_P (XFRAME (WINDOW_FRAME (o))))
    error ("Attempt to split unsplittable frame");

  check_min_window_sizes ();

  if (NILP (horflag))
    {
      if (size < window_min_height
	  || size + window_min_height > XFASTINT (o->height))
	args_out_of_range_3 (window, chsize, horflag);
      if (NILP (o->parent)
	  || NILP (XWINDOW (o->parent)->vchild))
	{
	  make_dummy_parent (window);
	  new = o->parent;
	  XWINDOW (new)->vchild = window;
	}
    }
  else
    {
      if (size < window_min_width
	  || size + window_min_width > XFASTINT (o->width))
	args_out_of_range_3 (window, chsize, horflag);
      if (NILP (o->parent)
	  || NILP (XWINDOW (o->parent)->hchild))
	{
	  make_dummy_parent (window);
	  new = o->parent;
	  XWINDOW (new)->hchild = window;
	}
    }

  /* Now we know that window's parent is a vertical combination
     if we are dividing vertically, or a horizontal combination
     if we are making side-by-side windows */

  windows_or_buffers_changed++;
  new = make_window ();
  p = XWINDOW (new);

  p->frame = o->frame;
  p->next = o->next;
  if (!NILP (p->next))
    XWINDOW (p->next)->prev = new;
  p->prev = window;
  o->next = new;
  p->parent = o->parent;
  p->buffer = Qt;

  Fset_window_buffer (new, o->buffer);

  /* Apportion the available frame space among the two new windows */

  if (!NILP (horflag))
    {
      p->height = o->height;
      p->top = o->top;
      XSETFASTINT (p->width, XFASTINT (o->width) - size);
      XSETFASTINT (o->width, size);
      XSETFASTINT (p->left, XFASTINT (o->left) + size);
    }
  else
    {
      p->left = o->left;
      p->width = o->width;
      XSETFASTINT (p->height, XFASTINT (o->height) - size);
      XSETFASTINT (o->height, size);
      XSETFASTINT (p->top, XFASTINT (o->top) + size);
    }

  return new;
}

DEFUN ("enlarge-window", Fenlarge_window, Senlarge_window, 1, 2, "p",
  "Make current window ARG lines bigger.\n\
From program, optional second arg non-nil means grow sideways ARG columns.")
  (n, side)
     register Lisp_Object n, side;
{
  CHECK_NUMBER (n, 0);
  change_window_height (XINT (n), !NILP (side));
  return Qnil;
}

DEFUN ("shrink-window", Fshrink_window, Sshrink_window, 1, 2, "p",
  "Make current window ARG lines smaller.\n\
From program, optional second arg non-nil means shrink sideways ARG columns.")
  (n, side)
     register Lisp_Object n, side;
{
  CHECK_NUMBER (n, 0);
  change_window_height (-XINT (n), !NILP (side));
  return Qnil;
}

int
window_height (window)
     Lisp_Object window;
{
  register struct window *p = XWINDOW (window);
  return XFASTINT (p->height);
}

int
window_width (window)
     Lisp_Object window;
{
  register struct window *p = XWINDOW (window);
  return XFASTINT (p->width);
}

#define MINSIZE(w)						\
  (widthflag							\
   ? window_min_width						\
   : (MINI_WINDOW_P (XWINDOW (w)) ? 1 : window_min_height))

#define CURBEG(w) \
  *(widthflag ? (int *) &(XWINDOW (w)->left) : (int *) &(XWINDOW (w)->top))

#define CURSIZE(w) \
  *(widthflag ? (int *) &(XWINDOW (w)->width) : (int *) &(XWINDOW (w)->height))

/* Unlike set_window_height, this function
   also changes the heights of the siblings so as to
   keep everything consistent. */

change_window_height (delta, widthflag)
     register int delta;
     int widthflag;
{
  register Lisp_Object parent;
  Lisp_Object window;
  register struct window *p;
  int *sizep;
  int (*sizefun) () = widthflag ? window_width : window_height;
  register int (*setsizefun) () = (widthflag
				   ? set_window_width
				   : set_window_height);

  check_min_window_sizes ();

  window = selected_window;
  while (1)
    {
      p = XWINDOW (window);
      parent = p->parent;
      if (NILP (parent))
	{
	  if (widthflag)
	    error ("No other window to side of this one");
	  break;
	}
      if (widthflag ? !NILP (XWINDOW (parent)->hchild)
	  : !NILP (XWINDOW (parent)->vchild))
	break;
      window = parent;
    }

  sizep = &CURSIZE (window);

  {
    register int maxdelta;

    maxdelta = (!NILP (parent) ? (*sizefun) (parent) - *sizep
		: !NILP (p->next) ? (*sizefun) (p->next) - MINSIZE (p->next)
		: !NILP (p->prev) ? (*sizefun) (p->prev) - MINSIZE (p->prev)
		/* This is a frame with only one window, a minibuffer-only
		   or a minibufferless frame.  */
		: (delta = 0));

    if (delta > maxdelta)
      /* This case traps trying to make the minibuffer
	 the full frame, or make the only window aside from the
	 minibuffer the full frame.  */
      delta = maxdelta;
  }

  if (*sizep + delta < MINSIZE (window))
    {
      Fdelete_window (window);
      return;
    }

  if (delta == 0)
    return;

  if (!NILP (p->next)
      && (*sizefun) (p->next) - delta >= MINSIZE (p->next))
    {
      (*setsizefun) (p->next, (*sizefun) (p->next) - delta, 0);
      (*setsizefun) (window, *sizep + delta, 0);
      CURBEG (p->next) += delta;
      /* This does not change size of p->next,
	 but it propagates the new top edge to its children */
      (*setsizefun) (p->next, (*sizefun) (p->next), 0);
    }
  else if (!NILP (p->prev)
	   && (*sizefun) (p->prev) - delta >= MINSIZE (p->prev))
    {
      (*setsizefun) (p->prev, (*sizefun) (p->prev) - delta, 0);
      CURBEG (window) -= delta;
      (*setsizefun) (window, *sizep + delta, 0);
    }
  else
    {
      register int delta1;
      register int opht = (*sizefun) (parent);

      /* If trying to grow this window to or beyond size of the parent,
	 make delta1 so big that, on shrinking back down,
	 all the siblings end up with less than one line and are deleted.  */
      if (opht <= *sizep + delta)
	delta1 = opht * opht * 2;
      /* Otherwise, make delta1 just right so that if we add delta1
	 lines to this window and to the parent, and then shrink
	 the parent back to its original size, the new proportional
	 size of this window will increase by delta.  */
      else
	delta1 = (delta * opht * 100) / ((opht - *sizep - delta) * 100);

      /* Add delta1 lines or columns to this window, and to the parent,
	 keeping things consistent while not affecting siblings.  */
      CURSIZE (parent) = opht + delta1;
      (*setsizefun) (window, *sizep + delta1, 0);

      /* Squeeze out delta1 lines or columns from our parent,
	 shriking this window and siblings proportionately.
	 This brings parent back to correct size.
	 Delta1 was calculated so this makes this window the desired size,
	 taking it all out of the siblings.  */
      (*setsizefun) (parent, opht, 0);
    }

  XSETFASTINT (p->last_modified, 0);
}
#undef MINSIZE
#undef CURBEG
#undef CURSIZE


/* Return number of lines of text (not counting mode line) in W.  */

int
window_internal_height (w)
     struct window *w;
{
  int ht = XFASTINT (w->height);

  if (MINI_WINDOW_P (w))
    return ht;

  if (!NILP (w->parent) || !NILP (w->vchild) || !NILP (w->hchild)
      || !NILP (w->next) || !NILP (w->prev)
      || FRAME_WANTS_MODELINE_P (XFRAME (WINDOW_FRAME (w))))
    return ht - 1;

  return ht;
}


/* Return the number of columns in W.
   Don't count columns occupied by scroll bars or the vertical bar
   separating W from the sibling to its right.  */
int
window_internal_width (w)
     struct window *w;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  int left = XINT (w->left);
  int width = XINT (w->width);

  /* If this window is flush against the right edge of the frame, its
     internal width is its full width.  */
  if (left + width >= FRAME_WIDTH (f))
    return width;

  /* If we are not flush right, then our rightmost columns are
     occupied by some sort of separator.  */

  /* Scroll bars occupy a few columns.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    return width - FRAME_SCROLL_BAR_COLS (f);

  /* The column of `|' characters separating side-by-side windows
     occupies one column only.  */
  return width - 1;
}


/* Scroll contents of window WINDOW up N lines.  */

void
window_scroll (window, n, noerror)
     Lisp_Object window;
     int n;
     int noerror;
{
  register struct window *w = XWINDOW (window);
  register int opoint = PT;
  register int pos;
  register int ht = window_internal_height (w);
  register Lisp_Object tem;
  int lose;
  Lisp_Object bolp, nmoved;

  XSETFASTINT (tem, PT);
  tem = Fpos_visible_in_window_p (tem, window);

  if (NILP (tem))
    {
      Fvertical_motion (make_number (- (ht / 2)), window);
      XSETFASTINT (tem, PT);
      Fset_marker (w->start, tem, w->buffer);
      w->force_start = Qt;
    }

  SET_PT (marker_position (w->start));
  lose = n < 0 && PT == BEGV;
  Fvertical_motion (make_number (n), window);
  pos = PT;
  bolp = Fbolp ();
  SET_PT (opoint);

  if (lose)
    {
      if (noerror)
	return;
      else
	Fsignal (Qbeginning_of_buffer, Qnil);
    }

  if (pos < ZV)
    {
      set_marker_restricted (w->start, make_number (pos), w->buffer);
      w->start_at_line_beg = bolp;
      w->update_mode_line = Qt;
      XSETFASTINT (w->last_modified, 0);
      if (pos > opoint)
	SET_PT (pos);
      if (n < 0)
	{
	  SET_PT (pos);
	  tem = Fvertical_motion (make_number (ht), window);
	  if (PT > opoint || XFASTINT (tem) < ht)
	    SET_PT (opoint);
	  else
	    Fvertical_motion (make_number (-1), window);
	}
    }
  else
    {
      if (noerror)
	return;
      else
	Fsignal (Qend_of_buffer, Qnil);
    }
}

/* This is the guts of Fscroll_up and Fscroll_down.  */

static void
scroll_command (n, direction)
     register Lisp_Object n;
     int direction;
{
  register int defalt;
  int count = specpdl_ptr - specpdl;

  /* If selected window's buffer isn't current, make it current for the moment.
     But don't screw up if window_scroll gets an error.  */
  if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
    {
      record_unwind_protect (save_excursion_restore, save_excursion_save ());
      Fset_buffer (XWINDOW (selected_window)->buffer);
    }

  defalt = (window_internal_height (XWINDOW (selected_window))
	    - next_screen_context_lines);
  defalt = direction * (defalt < 1 ? 1 : defalt);

  if (NILP (n))
    window_scroll (selected_window, defalt, 0);
  else if (EQ (n, Qminus))
    window_scroll (selected_window, - defalt, 0);
  else
    {
      n = Fprefix_numeric_value (n);
      window_scroll (selected_window, XINT (n) * direction, 0);
    }

  unbind_to (count, Qnil);
}

DEFUN ("scroll-up", Fscroll_up, Sscroll_up, 0, 1, "P",
  "Scroll text of current window upward ARG lines; or near full screen if no ARG.\n\
A near full screen is `next-screen-context-lines' less than a full screen.\n\
Negative ARG means scroll downward.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     Lisp_Object n;
{
  scroll_command (n, 1);
  return Qnil;
}

DEFUN ("scroll-down", Fscroll_down, Sscroll_down, 0, 1, "P",
  "Scroll text of current window downward ARG lines; or near full screen if no ARG.\n\
A near full screen is `next-screen-context-lines' less than a full screen.\n\
Negative ARG means scroll upward.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     Lisp_Object n;
{
  scroll_command (n, -1);
  return Qnil;
}

DEFUN ("other-window-for-scrolling", Fother_window_for_scrolling, Sother_window_for_scrolling, 0, 0, 0,
  "Return the other window for \"other window scroll\" commands.\n\
If in the minibuffer, `minibuffer-scroll-window' if non-nil\n\
specifies the window.\n\
If `other-window-scroll-buffer' is non-nil, a window\n\
showing that buffer is used.")
  ()
{
  Lisp_Object window;

  if (MINI_WINDOW_P (XWINDOW (selected_window))
      && !NILP (Vminibuf_scroll_window))
    window = Vminibuf_scroll_window;
  /* If buffer is specified, scroll that buffer.  */
  else if (!NILP (Vother_window_scroll_buffer))
    {
      window = Fget_buffer_window (Vother_window_scroll_buffer, Qnil);
      if (NILP (window))
	window = Fdisplay_buffer (Vother_window_scroll_buffer, Qt);
    }
  else
    {
      /* Nothing specified; look for a neighboring window on the same
	 frame.  */
      window = Fnext_window (selected_window, Qnil, Qnil);

      if (EQ (window, selected_window))
	/* That didn't get us anywhere; look for a window on another
           visible frame.  */
	do
	  window = Fnext_window (window, Qnil, Qt);
	while (! FRAME_VISIBLE_P (XFRAME (WINDOW_FRAME (XWINDOW (window))))
	       && ! EQ (window, selected_window));
    }

  CHECK_LIVE_WINDOW (window, 0);

  if (EQ (window, selected_window))
    error ("There is no other window");

  return window;
}

DEFUN ("scroll-other-window", Fscroll_other_window, Sscroll_other_window, 0, 1, "P",
  "Scroll next window upward ARG lines; or near full screen if no ARG.\n\
The next window is the one below the current one; or the one at the top\n\
if the current one is at the bottom.  Negative ARG means scroll downward.\n\
When calling from a program, supply a number as argument or nil.\n\
\n\
If in the minibuffer, `minibuffer-scroll-window' if non-nil\n\
specifies the window to scroll.\n\
If `other-window-scroll-buffer' is non-nil, scroll the window\n\
showing that buffer, popping the buffer up if necessary.")
  (n)
     register Lisp_Object n;
{
  register Lisp_Object window;
  register int ht;
  register struct window *w;
  register int count = specpdl_ptr - specpdl;

  window = Fother_window_for_scrolling ();

  w = XWINDOW (window);
  ht = window_internal_height (w);

  /* Don't screw up if window_scroll gets an error.  */
  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  Fset_buffer (w->buffer);
  SET_PT (marker_position (w->pointm));

  if (NILP (n))
    window_scroll (window, ht - next_screen_context_lines, 1);
  else if (EQ (n, Qminus))
    window_scroll (window, next_screen_context_lines - ht, 1);
  else
    {
      if (CONSP (n))
	n = Fcar (n);
      CHECK_NUMBER (n, 0);
      window_scroll (window, XINT (n), 1);
    }

  Fset_marker (w->pointm, make_number (PT), Qnil);
  unbind_to (count, Qnil);

  return Qnil;
}

DEFUN ("scroll-left", Fscroll_left, Sscroll_left, 0, 1, "P",
  "Scroll selected window display ARG columns left.\n\
Default for ARG is window width minus 2.")
  (arg)
     register Lisp_Object arg;
{

  if (NILP (arg))
    XSETFASTINT (arg, window_internal_width (XWINDOW (selected_window)) - 2);
  else
    arg = Fprefix_numeric_value (arg);

  return
    Fset_window_hscroll (selected_window,
			 make_number (XINT (XWINDOW (selected_window)->hscroll)
				      + XINT (arg)));
}

DEFUN ("scroll-right", Fscroll_right, Sscroll_right, 0, 1, "P",
  "Scroll selected window display ARG columns right.\n\
Default for ARG is window width minus 2.")
  (arg)
     register Lisp_Object arg;
{
  if (NILP (arg))
    XSETFASTINT (arg, window_internal_width (XWINDOW (selected_window)) - 2);
  else
    arg = Fprefix_numeric_value (arg);

  return
    Fset_window_hscroll (selected_window,
			 make_number (XINT (XWINDOW (selected_window)->hscroll)
				      - XINT (arg)));
}

DEFUN ("recenter", Frecenter, Srecenter, 0, 1, "P",
  "Center point in window and redisplay frame.  With ARG, put point on line ARG.\n\
The desired position of point is always relative to the current window.\n\
Just C-u as prefix means put point in the center of the window.\n\
No arg (i.e., it is nil) erases the entire frame and then\n\
redraws with point in the center of the current window.")
  (n)
     register Lisp_Object n;
{
  register struct window *w = XWINDOW (selected_window);
  register int ht = window_internal_height (w);
  struct position pos;
  Lisp_Object window;

  if (NILP (n))
    {
      extern int frame_garbaged;

      SET_FRAME_GARBAGED (XFRAME (WINDOW_FRAME (w)));
      XSETFASTINT (n, ht / 2);
    }
  else if (CONSP (n)) /* Just C-u. */
    {
      XSETFASTINT (n, ht / 2);
    }
  else
    {
      n = Fprefix_numeric_value (n);
      CHECK_NUMBER (n, 0);
    }

  if (XINT (n) < 0)
    XSETINT (n, XINT (n) + ht);

  XSETWINDOW (window, w);
  pos = *vmotion (point, - XINT (n), window_internal_width (w) - 1,
		  XINT (w->hscroll), window);

  Fset_marker (w->start, make_number (pos.bufpos), w->buffer);
  w->start_at_line_beg = ((pos.bufpos == BEGV
			   || FETCH_CHAR (pos.bufpos - 1) == '\n')
			  ? Qt : Qnil);
  w->force_start = Qt;

  return Qnil;
}

DEFUN ("move-to-window-line", Fmove_to_window_line, Smove_to_window_line,
  1, 1, "P",
  "Position point relative to window.\n\
With no argument, position point at center of window.\n\
An argument specifies frame line; zero means top of window,\n\
negative means relative to bottom of window.")
  (arg)
     register Lisp_Object arg;
{
  register struct window *w = XWINDOW (selected_window);
  register int height = window_internal_height (w);
  register int start;
  Lisp_Object window;

  if (NILP (arg))
    XSETFASTINT (arg, height / 2);
  else
    {
      arg = Fprefix_numeric_value (arg);
      if (XINT (arg) < 0)
	XSETINT (arg, XINT (arg) + height);
    }

  start = marker_position (w->start);
  XSETWINDOW (window, w);
  if (start < BEGV || start > ZV)
    {
      Fvertical_motion (make_number (- (height / 2)), window);
      Fset_marker (w->start, make_number (PT), w->buffer);
      w->start_at_line_beg = Fbolp ();
      w->force_start = Qt;
    }
  else
    SET_PT (start);

  return Fvertical_motion (arg, window);
}

struct save_window_data
  {
    int size_from_Lisp_Vector_struct;
    struct Lisp_Vector *next_from_Lisp_Vector_struct;
    Lisp_Object frame_width, frame_height, frame_menu_bar_lines;
    Lisp_Object selected_frame;
    Lisp_Object current_window;
    Lisp_Object current_buffer;
    Lisp_Object minibuf_scroll_window;
    Lisp_Object root_window;
    Lisp_Object focus_frame;
    /* Record the values of window-min-width and window-min-height
       so that window sizes remain consistent with them.  */
    Lisp_Object min_width, min_height;
    /* A vector, interpreted as a struct saved_window */
    Lisp_Object saved_windows;
  };

/* This is saved as a Lisp_Vector */
struct saved_window
  {
    /* these first two must agree with struct Lisp_Vector in lisp.h */
    int size_from_Lisp_Vector_struct;
    struct Lisp_Vector *next_from_Lisp_Vector_struct;

    Lisp_Object window;
    Lisp_Object buffer, start, pointm, mark;
    Lisp_Object left, top, width, height, hscroll;
    Lisp_Object parent, prev;
    Lisp_Object start_at_line_beg;
    Lisp_Object display_table;
  };
#define SAVED_WINDOW_VECTOR_SIZE 14 /* Arg to Fmake_vector */

#define SAVED_WINDOW_N(swv,n) \
  ((struct saved_window *) (XVECTOR ((swv)->contents[(n)])))

DEFUN ("window-configuration-p", Fwindow_configuration_p, Swindow_configuration_p, 1, 1, 0,
  "T if OBJECT is a window-configration object.")
  (obj)
     Lisp_Object obj;
{
  if (WINDOW_CONFIGURATIONP (obj))
    return Qt;
  return Qnil;
}


DEFUN ("set-window-configuration", Fset_window_configuration,
  Sset_window_configuration, 1, 1, 0,
  "Set the configuration of windows and buffers as specified by CONFIGURATION.\n\
CONFIGURATION must be a value previously returned\n\
by `current-window-configuration' (which see).")
     (configuration)
     Lisp_Object configuration;
{
  register struct save_window_data *data;
  struct Lisp_Vector *saved_windows;
  Lisp_Object new_current_buffer;
  Lisp_Object frame;
  FRAME_PTR f;

  while (!WINDOW_CONFIGURATIONP (configuration))
    {
      configuration = wrong_type_argument (intern ("window-configuration-p"),
					   configuration);
    }

  data = (struct save_window_data *) XVECTOR (configuration);
  saved_windows = XVECTOR (data->saved_windows);

  new_current_buffer = data->current_buffer;
  if (NILP (XBUFFER (new_current_buffer)->name))
    new_current_buffer = Qnil;

  frame = XWINDOW (SAVED_WINDOW_N (saved_windows, 0)->window)->frame;
  f = XFRAME (frame);

  /* If f is a dead frame, don't bother rebuilding its window tree.
     However, there is other stuff we should still try to do below.  */
  if (FRAME_LIVE_P (f))
    {
      register struct window *w;
      register struct saved_window *p;
      int k;

      /* If the frame has been resized since this window configuration was
	 made, we change the frame to the size specified in the
	 configuration, restore the configuration, and then resize it
	 back.  We keep track of the prevailing height in these variables.  */
      int previous_frame_height = FRAME_HEIGHT (f);
      int previous_frame_width =  FRAME_WIDTH  (f);
      int previous_frame_menu_bar_lines = FRAME_MENU_BAR_LINES (f);

      if (XFASTINT (data->frame_height) != previous_frame_height
	  || XFASTINT (data->frame_width) != previous_frame_width)
	change_frame_size (f, data->frame_height, data->frame_width, 0, 0);
#ifdef HAVE_X_WINDOWS
      if (XFASTINT (data->frame_menu_bar_lines)
	  != previous_frame_menu_bar_lines)
	x_set_menu_bar_lines (f, data->frame_menu_bar_lines, 0);
#endif

      windows_or_buffers_changed++;

      /* Temporarily avoid any problems with windows that are smaller
	 than they are supposed to be.  */
      window_min_height = 1;
      window_min_width = 1;

      /* Kludge Alert!
	 Mark all windows now on frame as "deleted".
	 Restoring the new configuration "undeletes" any that are in it.
	 
	 Save their current buffers in their height fields, since we may
	 need it later, if a buffer saved in the configuration is now
	 dead.  */
      delete_all_subwindows (XWINDOW (FRAME_ROOT_WINDOW (f)));

      for (k = 0; k < saved_windows->size; k++)
	{
	  p = SAVED_WINDOW_N (saved_windows, k);
	  w = XWINDOW (p->window);
	  w->next = Qnil;

	  if (!NILP (p->parent))
	    w->parent = SAVED_WINDOW_N (saved_windows,
					XFASTINT (p->parent))->window;
	  else
	    w->parent = Qnil;

	  if (!NILP (p->prev))
	    {
	      w->prev = SAVED_WINDOW_N (saved_windows,
					XFASTINT (p->prev))->window;
	      XWINDOW (w->prev)->next = p->window;
	    }
	  else
	    {
	      w->prev = Qnil;
	      if (!NILP (w->parent))
		{
		  if (EQ (p->width, XWINDOW (w->parent)->width))
		    {
		      XWINDOW (w->parent)->vchild = p->window;
		      XWINDOW (w->parent)->hchild = Qnil;
		    }
		  else
		    {
		      XWINDOW (w->parent)->hchild = p->window;
		      XWINDOW (w->parent)->vchild = Qnil;
		    }
		}
	    }

	  /* If we squirreled away the buffer in the window's height,
	     restore it now.  */
	  if (BUFFERP (w->height))
	    w->buffer = w->height;
	  w->left = p->left;
	  w->top = p->top;
	  w->width = p->width;
	  w->height = p->height;
	  w->hscroll = p->hscroll;
	  w->display_table = p->display_table;
	  XSETFASTINT (w->last_modified, 0);

	  /* Reinstall the saved buffer and pointers into it.  */
	  if (NILP (p->buffer))
	    w->buffer = p->buffer;
	  else
	    {
	      if (!NILP (XBUFFER (p->buffer)->name))
		/* If saved buffer is alive, install it.  */
		{
		  w->buffer = p->buffer;
		  w->start_at_line_beg = p->start_at_line_beg;
		  set_marker_restricted (w->start,
					 Fmarker_position (p->start),
					 w->buffer);
		  set_marker_restricted (w->pointm,
					 Fmarker_position (p->pointm),
					 w->buffer);
		  Fset_marker (XBUFFER (w->buffer)->mark,
			       Fmarker_position (p->mark), w->buffer);

		  /* As documented in Fcurrent_window_configuration, don't
		     save the location of point in the buffer which was current
		     when the window configuration was recorded.  */
		  if (!EQ (p->buffer, new_current_buffer)
		      && XBUFFER (p->buffer) == current_buffer)
		    Fgoto_char (w->pointm);
		}
	      else if (NILP (w->buffer) || NILP (XBUFFER (w->buffer)->name))
		/* Else unless window has a live buffer, get one.  */
		{
		  w->buffer = Fcdr (Fcar (Vbuffer_alist));
		  /* This will set the markers to beginning of visible
		     range.  */
		  set_marker_restricted (w->start, make_number (0), w->buffer);
		  set_marker_restricted (w->pointm, make_number (0),w->buffer);
		  w->start_at_line_beg = Qt;
		}
	      else
		/* Keeping window's old buffer; make sure the markers
		   are real.  */
		{
		  /* Set window markers at start of visible range.  */
		  if (XMARKER (w->start)->buffer == 0)
		    set_marker_restricted (w->start, make_number (0),
					   w->buffer);
		  if (XMARKER (w->pointm)->buffer == 0)
		    set_marker_restricted (w->pointm,
					   (make_number
					    (BUF_PT (XBUFFER (w->buffer)))),
					   w->buffer);
		  w->start_at_line_beg = Qt;
		}
	    }
	}

      FRAME_ROOT_WINDOW (f) = data->root_window;
      Fselect_window (data->current_window);

#ifdef MULTI_FRAME
      if (NILP (data->focus_frame)
	  || (FRAMEP (data->focus_frame)
	      && FRAME_LIVE_P (XFRAME (data->focus_frame))))
	Fredirect_frame_focus (frame, data->focus_frame);
#endif

#if 0 /* I don't understand why this is needed, and it causes problems
         when the frame's old selected window has been deleted.  */
#ifdef MULTI_FRAME
      if (f != selected_frame && ! FRAME_TERMCAP_P (f))
	Fhandle_switch_frame (WINDOW_FRAME (XWINDOW (data->root_window)), Qnil);
#endif
#endif

      /* Set the screen height to the value it had before this function.  */
      if (previous_frame_height != FRAME_HEIGHT (f)
	  || previous_frame_width != FRAME_WIDTH (f))
	change_frame_size (f, previous_frame_height, previous_frame_width,
			   0, 0);
#ifdef HAVE_X_WINDOWS
      if (previous_frame_menu_bar_lines != FRAME_MENU_BAR_LINES (f))
	x_set_menu_bar_lines (f, previous_frame_menu_bar_lines, 0);
#endif
    }

  /* Restore the minimum heights recorded in the configuration.  */
  window_min_height = XINT (data->min_height);
  window_min_width = XINT (data->min_width);

#ifdef MULTI_FRAME
  /* Fselect_window will have made f the selected frame, so we
     reselect the proper frame here.  Fhandle_switch_frame will change the
     selected window too, but that doesn't make the call to
     Fselect_window above totally superfluous; it still sets f's
     selected window.  */
  if (FRAME_LIVE_P (XFRAME (data->selected_frame)))
    Fhandle_switch_frame (data->selected_frame, Qnil);
#endif

  if (!NILP (new_current_buffer))
    Fset_buffer (new_current_buffer);

  Vminibuf_scroll_window = data->minibuf_scroll_window;
  return (Qnil);
}

/* Mark all windows now on frame as deleted
   by setting their buffers to nil.  */

void
delete_all_subwindows (w)
     register struct window *w;
{
  if (!NILP (w->next))
    delete_all_subwindows (XWINDOW (w->next));
  if (!NILP (w->vchild))
    delete_all_subwindows (XWINDOW (w->vchild));
  if (!NILP (w->hchild))
    delete_all_subwindows (XWINDOW (w->hchild));

  w->height = w->buffer;       /* See Fset_window_configuration for excuse.  */

  /* We set all three of these fields to nil, to make sure that we can
     distinguish this dead window from any live window.  Live leaf
     windows will have buffer set, and combination windows will have
     vchild or hchild set.  */
  w->buffer = Qnil;
  w->vchild = Qnil;
  w->hchild = Qnil;
}

static int
count_windows (window)
     register struct window *window;
{
  register int count = 1;
  if (!NILP (window->next))
    count += count_windows (XWINDOW (window->next));
  if (!NILP (window->vchild))
    count += count_windows (XWINDOW (window->vchild));
  if (!NILP (window->hchild))
    count += count_windows (XWINDOW (window->hchild));
  return count;
}

static int
save_window_save (window, vector, i)
     Lisp_Object window;
     struct Lisp_Vector *vector;
     int i;
{
  register struct saved_window *p;
  register struct window *w;
  register Lisp_Object tem;

  for (;!NILP (window); window = w->next)
    {
      p = SAVED_WINDOW_N (vector, i);
      w = XWINDOW (window);

      XSETFASTINT (w->temslot, i++);
      p->window = window;
      p->buffer = w->buffer;
      p->left = w->left;
      p->top = w->top;
      p->width = w->width;
      p->height = w->height;
      p->hscroll = w->hscroll;
      p->display_table = w->display_table;
      if (!NILP (w->buffer))
	{
	  /* Save w's value of point in the window configuration.
	     If w is the selected window, then get the value of point
	     from the buffer; pointm is garbage in the selected window.  */
	  if (EQ (window, selected_window))
	    {
	      p->pointm = Fmake_marker ();
	      Fset_marker (p->pointm, BUF_PT (XBUFFER (w->buffer)),
			   w->buffer);
	    }
	  else
	    p->pointm = Fcopy_marker (w->pointm);

	  p->start = Fcopy_marker (w->start);
	  p->start_at_line_beg = w->start_at_line_beg;

	  tem = XBUFFER (w->buffer)->mark;
	  p->mark = Fcopy_marker (tem);
	}
      else
	{
	  p->pointm = Qnil;
	  p->start = Qnil;
	  p->mark = Qnil;
	  p->start_at_line_beg = Qnil;
	}

      if (NILP (w->parent))
	p->parent = Qnil;
      else
	p->parent = XWINDOW (w->parent)->temslot;

      if (NILP (w->prev))
	p->prev = Qnil;
      else
	p->prev = XWINDOW (w->prev)->temslot;

      if (!NILP (w->vchild))
	i = save_window_save (w->vchild, vector, i);
      if (!NILP (w->hchild))
	i = save_window_save (w->hchild, vector, i);
    }

  return i;
}

DEFUN ("current-window-configuration",
	Fcurrent_window_configuration, Scurrent_window_configuration, 0, 1, 0,
  "Return an object representing the current window configuration of FRAME.\n\
If FRAME is nil or omitted, use the selected frame.\n\
This describes the number of windows, their sizes and current buffers,\n\
and for each displayed buffer, where display starts, and the positions of\n\
point and mark.  An exception is made for point in the current buffer:\n\
its value is -not- saved.\n\
This also records the currently selected frame, and FRAME's focus\n\
redirection (see `redirect-frame-focus').")
  (frame)
     Lisp_Object frame;
{
  register Lisp_Object tem;
  register int n_windows;
  register struct save_window_data *data;
  register struct Lisp_Vector *vec;
  register int i;
  FRAME_PTR f;

  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
    }

  n_windows = count_windows (XWINDOW (FRAME_ROOT_WINDOW (f)));
  vec = allocate_vectorlike (VECSIZE (struct save_window_data));
  for (i = 0; i < VECSIZE (struct save_window_data); i++)
    vec->contents[i] = Qnil;
  vec->size = VECSIZE (struct save_window_data);
  data = (struct save_window_data *)vec;

  XSETFASTINT (data->frame_width, FRAME_WIDTH (f));
  XSETFASTINT (data->frame_height, FRAME_HEIGHT (f));
  XSETFASTINT (data->frame_menu_bar_lines, FRAME_MENU_BAR_LINES (f));
#ifdef MULTI_FRAME
  XSETFRAME (data->selected_frame, selected_frame);
#endif
  data->current_window = FRAME_SELECTED_WINDOW (f);
  XSETBUFFER (data->current_buffer, current_buffer);
  data->minibuf_scroll_window = Vminibuf_scroll_window;
  data->root_window = FRAME_ROOT_WINDOW (f);
  data->focus_frame = FRAME_FOCUS_FRAME (f);
  XSETINT (data->min_height, window_min_height);
  XSETINT (data->min_width, window_min_width);
  tem = Fmake_vector (make_number (n_windows), Qnil);
  data->saved_windows = tem;
  for (i = 0; i < n_windows; i++)
    XVECTOR (tem)->contents[i]
      = Fmake_vector (make_number (SAVED_WINDOW_VECTOR_SIZE), Qnil);
  save_window_save (FRAME_ROOT_WINDOW (f),
		    XVECTOR (tem), 0);
  XSETWINDOW_CONFIGURATION (tem, data);
  return (tem);
}

DEFUN ("save-window-excursion", Fsave_window_excursion, Ssave_window_excursion,
  0, UNEVALLED, 0,
  "Execute body, preserving window sizes and contents.\n\
Restore which buffer appears in which window, where display starts,\n\
and the value of point and mark for each window.\n\
Also restore which buffer is current.\n\
But do not preserve point in the current buffer.\n\
Does not restore the value of point in current buffer.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  register int count = specpdl_ptr - specpdl;

  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration (Qnil));
  val = Fprogn (args);
  return unbind_to (count, val);
}

init_window_once ()
{
#ifdef MULTI_FRAME
  selected_frame = make_terminal_frame ();
  minibuf_window = selected_frame->minibuffer_window;
  selected_window = selected_frame->selected_window;
  last_nonminibuf_frame = selected_frame;
#else /* not MULTI_FRAME */
  extern Lisp_Object get_minibuffer ();

  selected_frame = last_nonminibuf_frame = &the_only_frame;

  minibuf_window = make_window ();
  FRAME_ROOT_WINDOW (selected_frame) = make_window ();

  XWINDOW (FRAME_ROOT_WINDOW (selected_frame))->next = minibuf_window;
  XWINDOW (minibuf_window)->prev = FRAME_ROOT_WINDOW (selected_frame);
  XWINDOW (minibuf_window)->mini_p = Qt;

  /* These values 9 and 10 are arbitrary,
     just so that there is "something there."
     Correct values are put in in init_xdisp */

  XSETFASTINT (XWINDOW (FRAME_ROOT_WINDOW (selected_frame))->width, 10);
  XSETFASTINT (XWINDOW (minibuf_window)->width, 10);

  XSETFASTINT (XWINDOW (FRAME_ROOT_WINDOW (selected_frame))->height, 9);
  XSETFASTINT (XWINDOW (minibuf_window)->top, 9);
  XSETFASTINT (XWINDOW (minibuf_window)->height, 1);

  /* Prevent error in Fset_window_buffer.  */
  XWINDOW (FRAME_ROOT_WINDOW (selected_frame))->buffer = Qt;
  XWINDOW (minibuf_window)->buffer = Qt;

  /* Now set them up for real.  */
  Fset_window_buffer (FRAME_ROOT_WINDOW (selected_frame),
		      Fcurrent_buffer ());
  Fset_window_buffer (minibuf_window, get_minibuffer (0));

  selected_window = FRAME_ROOT_WINDOW (selected_frame);
  /* Make sure this window seems more recently used than
     a newly-created, never-selected window.  Increment
     window_select_count so the first selection ever will get
     something newer than this.  */
  XSETFASTINT (XWINDOW (selected_window)->use_time, ++window_select_count);
#endif /* not MULTI_FRAME */
}

syms_of_window ()
{
  Qwindowp = intern ("windowp");
  staticpro (&Qwindowp);

  Qwindow_live_p = intern ("window-live-p");
  staticpro (&Qwindow_live_p);

#ifndef MULTI_FRAME
  /* Make sure all windows get marked */
  staticpro (&minibuf_window);
#endif

  DEFVAR_LISP ("temp-buffer-show-function", &Vtemp_buffer_show_function,
    "Non-nil means call as function to display a help buffer.\n\
The function is called with one argument, the buffer to be displayed.\n\
Used by `with-output-to-temp-buffer'.");
  Vtemp_buffer_show_function = Qnil;

  DEFVAR_LISP ("display-buffer-function", &Vdisplay_buffer_function,
    "If non-nil, function to call to handle `display-buffer'.\n\
It will receive two args, the buffer and a flag which if non-nil means\n\
 that the currently selected window is not acceptable.\n\
Commands such as `switch-to-buffer-other-window' and `find-file-other-window'\n\
work using this function.");
  Vdisplay_buffer_function = Qnil;

  DEFVAR_LISP ("minibuffer-scroll-window", &Vminibuf_scroll_window,
    "Non-nil means it is the window that C-M-v in minibuffer should scroll.");
  Vminibuf_scroll_window = Qnil;

  DEFVAR_LISP ("other-window-scroll-buffer", &Vother_window_scroll_buffer,
    "If non-nil, this is a buffer and \\[scroll-other-window] should scroll its window.");
  Vother_window_scroll_buffer = Qnil;

  DEFVAR_BOOL ("pop-up-frames", &pop_up_frames,
    "*Non-nil means `display-buffer' should make a separate frame.");
  pop_up_frames = 0;

  DEFVAR_LISP ("pop-up-frame-function", &Vpop_up_frame_function,
    "Function to call to handle automatic new frame creation.\n\
It is called with no arguments and should return a newly created frame.\n\
\n\
A typical value might be `(lambda () (new-frame pop-up-frame-alist))'\n\
where `pop-up-frame-alist' would hold the default frame parameters.");
  Vpop_up_frame_function = Qnil;

  DEFVAR_LISP ("special-display-buffer-names", &Vspecial_display_buffer_names,
    "*List of buffer names that should have their own special frames.\n\
Displaying a buffer whose name is in this list makes a special frame for it\n\
using `special-display-function'.\n\
Instead of a buffer name, the list entries can be cons cells.  In that\n\
case the car should be a buffer name, and the cdr data to be passed as a\n\
second argument to `special-display-function'.\n\
See also `special-display-regexps'.");
  Vspecial_display_buffer_names = Qnil;

  DEFVAR_LISP ("special-display-regexps", &Vspecial_display_regexps,
    "*List of regexps saying which buffers should have their own special frames.\n\
If a buffer name matches one of these regexps, it gets its own frame.\n\
Displaying a buffer whose name is in this list makes a special frame for it\n\
using `special-display-function'.\n\
Instead of a buffer name, the list entries can be cons cells.  In that\n\
case the car should be the regexp, and the cdr data to be passed as a\n\
second argument to `special-display-function'.\n\
See also `special-display-buffer-names'.");
  Vspecial_display_regexps = Qnil;

  DEFVAR_LISP ("special-display-function", &Vspecial_display_function,
    "Function to call to make a new frame for a special buffer.\n\
It is called with two arguments, the buffer and optional buffer specific\n\
data, and should return a window displaying that buffer.\n\
The default value makes a separate frame for the buffer,\n\
using `special-display-frame-alist' to specify the frame parameters.\n\
\n\
A buffer is special if its is listed in `special-display-buffer-names'\n\
or matches a regexp in `special-display-regexps'.");
  Vspecial_display_function = Qnil;

  DEFVAR_BOOL ("pop-up-windows", &pop_up_windows,
    "*Non-nil means display-buffer should make new windows.");
  pop_up_windows = 1;

  DEFVAR_INT ("next-screen-context-lines", &next_screen_context_lines,
    "*Number of lines of continuity when scrolling by screenfuls.");
  next_screen_context_lines = 2;

  DEFVAR_INT ("split-height-threshold", &split_height_threshold,
    "*display-buffer would prefer to split the largest window if this large.\n\
If there is only one window, it is split regardless of this value.");
  split_height_threshold = 500;

  DEFVAR_INT ("window-min-height", &window_min_height,
    "*Delete any window less than this tall (including its mode line).");
  window_min_height = 4;

  DEFVAR_INT ("window-min-width", &window_min_width,
    "*Delete any window less than this wide.");
  window_min_width = 10;

  defsubr (&Sselected_window);
  defsubr (&Sminibuffer_window);
  defsubr (&Swindow_minibuffer_p);
  defsubr (&Swindowp);
  defsubr (&Swindow_live_p);
  defsubr (&Spos_visible_in_window_p);
  defsubr (&Swindow_buffer);
  defsubr (&Swindow_height);
  defsubr (&Swindow_width);
  defsubr (&Swindow_hscroll);
  defsubr (&Sset_window_hscroll);
  defsubr (&Swindow_edges);
  defsubr (&Scoordinates_in_window_p);
  defsubr (&Swindow_at);
  defsubr (&Swindow_point);
  defsubr (&Swindow_start);
  defsubr (&Swindow_end);
  defsubr (&Sset_window_point);
  defsubr (&Sset_window_start);
  defsubr (&Swindow_dedicated_p);
  defsubr (&Sset_window_dedicated_p);
  defsubr (&Swindow_display_table);
  defsubr (&Sset_window_display_table);
  defsubr (&Snext_window);
  defsubr (&Sprevious_window);
  defsubr (&Sother_window);
  defsubr (&Sget_lru_window);
  defsubr (&Sget_largest_window);
  defsubr (&Sget_buffer_window);
  defsubr (&Sdelete_other_windows);
  defsubr (&Sdelete_windows_on);
  defsubr (&Sreplace_buffer_in_windows);
  defsubr (&Sdelete_window);
  defsubr (&Sset_window_buffer);
  defsubr (&Sselect_window);
  defsubr (&Sdisplay_buffer);
  defsubr (&Ssplit_window);
  defsubr (&Senlarge_window);
  defsubr (&Sshrink_window);
  defsubr (&Sscroll_up);
  defsubr (&Sscroll_down);
  defsubr (&Sscroll_left);
  defsubr (&Sscroll_right);
  defsubr (&Sother_window_for_scrolling);
  defsubr (&Sscroll_other_window);
  defsubr (&Srecenter);
  defsubr (&Smove_to_window_line);
  defsubr (&Swindow_configuration_p);
  defsubr (&Sset_window_configuration);
  defsubr (&Scurrent_window_configuration);
  defsubr (&Ssave_window_excursion);
}

keys_of_window ()
{
  initial_define_key (control_x_map, '1', "delete-other-windows");
  initial_define_key (control_x_map, '2', "split-window");
  initial_define_key (control_x_map, '0', "delete-window");
  initial_define_key (control_x_map, 'o', "other-window");
  initial_define_key (control_x_map, '^', "enlarge-window");
  initial_define_key (control_x_map, '<', "scroll-left");
  initial_define_key (control_x_map, '>', "scroll-right");

  initial_define_key (global_map, Ctl ('V'), "scroll-up");
  initial_define_key (meta_map, Ctl ('V'), "scroll-other-window");
  initial_define_key (meta_map, 'v', "scroll-down");

  initial_define_key (global_map, Ctl('L'), "recenter");
  initial_define_key (meta_map, 'r', "move-to-window-line");
}
