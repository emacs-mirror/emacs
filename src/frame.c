/* Generic frame functions.
   Copyright (C) 1993 Free Software Foundation.

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

#include <stdio.h>

#include <config.h>
#include "lisp.h"
#include "frame.h"

#ifdef MULTI_FRAME

#include "buffer.h"
#include "window.h"
#include "termhooks.h"

/* These help us bind and responding to switch-frame events.  */
#include "commands.h"
#include "keyboard.h"

Lisp_Object Vemacs_iconified;
Lisp_Object Vframe_list;
Lisp_Object Vterminal_frame;
Lisp_Object Vdefault_minibuffer_frame;
Lisp_Object Vdefault_frame_alist;

/* Evaluate this expression to rebuild the section of syms_of_frame
   that initializes and staticpros the symbols declared below.  Note
   that Emacs 18 has a bug that keeps C-x C-e from being able to
   evaluate this expression.

(progn
  ;; Accumulate a list of the symbols we want to initialize from the
  ;; declarations at the top of the file.
  (goto-char (point-min))
  (search-forward "/\*&&& symbols declared here &&&*\/\n")
  (let (symbol-list)
    (while (looking-at "Lisp_Object \\(Q[a-z_]+\\)")
      (setq symbol-list
	    (cons (buffer-substring (match-beginning 1) (match-end 1))
		  symbol-list))
      (forward-line 1))
    (setq symbol-list (nreverse symbol-list))
    ;; Delete the section of syms_of_... where we initialize the symbols.
    (search-forward "\n  /\*&&& init symbols here &&&*\/\n")
    (let ((start (point)))
      (while (looking-at "^  Q")
	(forward-line 2))
      (kill-region start (point)))
    ;; Write a new symbol initialization section.
    (while symbol-list
      (insert (format "  %s = intern (\"" (car symbol-list)))
      (let ((start (point)))
	(insert (substring (car symbol-list) 1))
	(subst-char-in-region start (point) ?_ ?-))
      (insert (format "\");\n  staticpro (&%s);\n" (car symbol-list)))
      (setq symbol-list (cdr symbol-list)))))
  */        

/*&&& symbols declared here &&&*/
Lisp_Object Qframep;
Lisp_Object Qframe_live_p;
Lisp_Object Qheight;
Lisp_Object Qicon;
Lisp_Object Qminibuffer;
Lisp_Object Qmodeline;
Lisp_Object Qname;
Lisp_Object Qonly;
Lisp_Object Qunsplittable;
Lisp_Object Qmenu_bar_lines;
Lisp_Object Qwidth;
Lisp_Object Qx;

extern Lisp_Object Vminibuffer_list;
extern Lisp_Object get_minibuffer ();
extern Lisp_Object Fhandle_switch_frame ();
extern Lisp_Object Fredirect_frame_focus ();

DEFUN ("framep", Fframep, Sframep, 1, 1, 0,
  "Return non-nil if OBJECT is a frame.\n\
Value is t for a termcap frame (a character-only terminal),\n\
`x' for an Emacs frame that is really an X window.\n\
Also see `live-frame-p'.")
  (object)
     Lisp_Object object;
{
  if (XTYPE (object) != Lisp_Frame)
    return Qnil;
  switch (XFRAME (object)->output_method)
    {
    case output_termcap:
      return Qt;
    case output_x_window:
      return Qx;
    default:
      abort ();
    }
}

DEFUN ("frame-live-p", Fframe_live_p, Sframe_live_p, 1, 1, 0,
  "Return non-nil if OBJECT is a frame which has not been deleted.\n\
Value is nil if OBJECT is not a live frame.  If object is a live\n\
frame, the return value indicates what sort of output device it is\n\
displayed on.  Value is t for a termcap frame (a character-only\n\
terminal), `x' for an Emacs frame being displayed in an X window.")
  (object)
     Lisp_Object object;
{
  return ((FRAMEP (object)
	   && FRAME_LIVE_P (XFRAME (object)))
	  ? Fframep (object)
	  : Qnil);
}

struct frame *
make_frame (mini_p)
     int mini_p;
{
  Lisp_Object frame;
  register struct frame *f;
  register Lisp_Object root_window;
  register Lisp_Object mini_window;

  frame = Fmake_vector (((sizeof (struct frame) - (sizeof (Lisp_Vector)
						     - sizeof (Lisp_Object)))
			  / sizeof (Lisp_Object)),
			 make_number (0));
  XSETTYPE (frame, Lisp_Frame);
  f = XFRAME (frame);

  f->cursor_x = 0;
  f->cursor_y = 0;
  f->current_glyphs = 0;
  f->desired_glyphs = 0;
  f->visible = 0;
  f->async_visible = 0;
  f->display.nothing = 0;
  f->iconified = 0;
  f->async_iconified = 0;
  f->wants_modeline = 1;
  f->auto_raise = 0;
  f->auto_lower = 0;
  f->no_split = 0;
  f->garbaged = 0;
  f->has_minibuffer = mini_p;
  f->focus_frame = Qnil;
  f->explicit_name = 0;
  f->can_have_scroll_bars = 0;
  f->has_vertical_scroll_bars = 0;
  f->param_alist = Qnil;
  f->scroll_bars = Qnil;
  f->condemned_scroll_bars = Qnil;
  f->face_alist = Qnil;

  root_window = make_window ();
  if (mini_p)
    {
      mini_window = make_window ();
      XWINDOW (root_window)->next = mini_window;
      XWINDOW (mini_window)->prev = root_window;
      XWINDOW (mini_window)->mini_p = Qt;
      XWINDOW (mini_window)->frame = frame;
      f->minibuffer_window = mini_window;
    }
  else
    {
      mini_window = Qnil;
      XWINDOW (root_window)->next = Qnil;
      f->minibuffer_window = Qnil;
    }

  XWINDOW (root_window)->frame = frame;

  /* 10 is arbitrary,
     just so that there is "something there."
     Correct size will be set up later with change_frame_size.  */

  f->width = 10;
  f->height = 10;

  XFASTINT (XWINDOW (root_window)->width) = 10;
  XFASTINT (XWINDOW (root_window)->height) = (mini_p ? 9 : 10);

  if (mini_p)
    {
      XFASTINT (XWINDOW (mini_window)->width) = 10;
      XFASTINT (XWINDOW (mini_window)->top) = 9;
      XFASTINT (XWINDOW (mini_window)->height) = 1;
    }

  /* Choose a buffer for the frame's root window.  */
  {
    Lisp_Object buf;

    XWINDOW (root_window)->buffer = Qt;
    buf = Fcurrent_buffer ();
    /* If buf is a 'hidden' buffer (i.e. one whose name starts with
       a space), try to find another one.  */
    if (XSTRING (Fbuffer_name (buf))->data[0] == ' ')
      buf = Fother_buffer (buf, Qnil);
    Fset_window_buffer (root_window, buf);
  }

  if (mini_p)
    {
      XWINDOW (mini_window)->buffer = Qt;
      Fset_window_buffer (mini_window,
			  (NILP (Vminibuffer_list)
			   ? get_minibuffer (0)
			   : Fcar (Vminibuffer_list)));
    }

  f->root_window = root_window;
  f->selected_window = root_window;
  /* Make sure this window seems more recently used than
     a newly-created, never-selected window.  */
  XFASTINT (XWINDOW (f->selected_window)->use_time) = ++window_select_count;

  Vframe_list = Fcons (frame, Vframe_list);

  return f;
}

/* Make a frame using a separate minibuffer window on another frame.
   MINI_WINDOW is the minibuffer window to use.  nil means use the
   default (the global minibuffer).  */

struct frame *
make_frame_without_minibuffer (mini_window)
     register Lisp_Object mini_window;
{
  register struct frame *f;

  /* Choose the minibuffer window to use.  */
  if (NILP (mini_window))
    {
      if (XTYPE (Vdefault_minibuffer_frame) != Lisp_Frame)
	error ("default-minibuffer-frame must be set when creating minibufferless frames");
      if (! FRAME_LIVE_P (XFRAME (Vdefault_minibuffer_frame)))
	error ("default-minibuffer-frame must be a live frame");
      mini_window = XFRAME (Vdefault_minibuffer_frame)->minibuffer_window;
    }
  else
    {
      CHECK_LIVE_WINDOW (mini_window, 0);
    }

  /* Make a frame containing just a root window.  */
  f = make_frame (0);

  /* Install the chosen minibuffer window, with proper buffer.  */
  f->minibuffer_window = mini_window;
  Fset_window_buffer (mini_window,
		      (NILP (Vminibuffer_list)
		       ? get_minibuffer (0)
		       : Fcar (Vminibuffer_list)));
  return f;
}

/* Make a frame containing only a minibuffer window.  */

struct frame *
make_minibuffer_frame ()
{
  /* First make a frame containing just a root window, no minibuffer.  */

  register struct frame *f = make_frame (0);
  register Lisp_Object mini_window;
  register Lisp_Object frame;

  XSET (frame, Lisp_Frame, f);

  f->auto_raise = 0;
  f->auto_lower = 0;
  f->no_split = 1;
  f->wants_modeline = 0;
  f->has_minibuffer = 1;

  /* Now label the root window as also being the minibuffer.
     Avoid infinite looping on the window chain by marking next pointer
     as nil. */

  mini_window = f->minibuffer_window = f->root_window;
  XWINDOW (mini_window)->mini_p = Qt;
  XWINDOW (mini_window)->next = Qnil;
  XWINDOW (mini_window)->prev = Qnil;
  XWINDOW (mini_window)->frame = frame;

  /* Put the proper buffer in that window.  */

  Fset_window_buffer (mini_window,
		      (NILP (Vminibuffer_list)
		       ? get_minibuffer (0)
		       : Fcar (Vminibuffer_list)));
  return f;
}

/* Construct a frame that refers to the terminal (stdin and stdout).  */

struct frame *
make_terminal_frame ()
{
  register struct frame *f;

  Vframe_list = Qnil;
  f = make_frame (1);
  f->name = build_string ("terminal");
  FRAME_SET_VISIBLE (f, 1);
  f->display.nothing = 1;   /* Nonzero means frame isn't deleted.  */
  XSET (Vterminal_frame, Lisp_Frame, f);
  return f;
}

DEFUN ("select-frame", Fselect_frame, Sselect_frame, 1, 2, "e",
  "Select the frame FRAME.\n\
Subsequent editing commands apply to its selected window.\n\
The selection of FRAME lasts until the next time the user does\n\
something to select a different frame, or until the next time this\n\
function is called.")
  (frame, no_enter)
    Lisp_Object frame, no_enter;
{
  return Fhandle_switch_frame (frame, no_enter);
}


DEFUN ("handle-switch-frame", Fhandle_switch_frame, Shandle_switch_frame, 1, 2, "e",
  "Handle a switch-frame event EVENT.\n\
Switch-frame events are usually bound to this function.\n\
A switch-frame event tells Emacs that the window manager has requested\n\
that the user's events be directed to the frame mentioned in the event.\n\
This function selects the selected window of the frame of EVENT.\n\
\n\
If EVENT is frame object, handle it as if it were a switch-frame event\n\
to that frame.")
  (frame, no_enter)
     Lisp_Object frame, no_enter;
{
  /* If FRAME is a switch-frame event, extract the frame we should
     switch to.  */
  if (CONSP (frame)
      && EQ (XCONS (frame)->car, Qswitch_frame)
      && CONSP (XCONS (frame)->cdr))
    frame = XCONS (XCONS (frame)->cdr)->car;

  CHECK_LIVE_FRAME (frame, 0);

  if (selected_frame == XFRAME (frame))
    return frame;

  /* If a frame's focus has been redirected toward the currently
     selected frame, we should change the redirection to point to the
     newly selected frame.  This means that if the focus is redirected
     from a minibufferless frame to a surrogate minibuffer frame, we
     can use `other-window' to switch between all the frames using
     that minibuffer frame, and the focus redirection will follow us
     around.  */
  {
    Lisp_Object tail;

    for (tail = Vframe_list; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	Lisp_Object focus;

	if (XTYPE (XCONS (tail)->car) != Lisp_Frame)
	  abort ();

	focus = FRAME_FOCUS_FRAME (XFRAME (XCONS (tail)->car));

	if (XTYPE (focus) == Lisp_Frame
	    && XFRAME (focus) == selected_frame)
	  Fredirect_frame_focus (XCONS (tail)->car, frame);
      }
  }

  selected_frame = XFRAME (frame);
  if (! FRAME_MINIBUF_ONLY_P (selected_frame))
    last_nonminibuf_frame = selected_frame;

  Fselect_window (XFRAME (frame)->selected_window);
  choose_minibuf_frame ();

  /* We want to make sure that the next event generates a frame-switch
     event to the appropriate frame.  This seems kludgy to me, but
     before you take it out, make sure that evaluating something like
     (select-window (frame-root-window (new-frame))) doesn't end up
     with your typing being interpreted in the new frame instead of
     the one you're actually typing in.  */
  internal_last_event_frame = Qnil;

  return frame;
}

DEFUN ("selected-frame", Fselected_frame, Sselected_frame, 0, 0, 0,
  "Return the frame that is now selected.")
  ()
{
  Lisp_Object tem;
  XSET (tem, Lisp_Frame, selected_frame);
  return tem;
}

DEFUN ("window-frame", Fwindow_frame, Swindow_frame, 1, 1, 0,
  "Return the frame object that window WINDOW is on.")
  (window)
     Lisp_Object window;
{
  CHECK_LIVE_WINDOW (window, 0);
  return XWINDOW (window)->frame;
}

DEFUN ("frame-root-window", Fframe_root_window, Sframe_root_window, 0, 1, 0,
       "Returns the root-window of FRAME.\n\
If omitted, FRAME defaults to the currently selected frame.")
  (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    XSET (frame, Lisp_Frame, selected_frame);
  else
    CHECK_LIVE_FRAME (frame, 0);

  return XFRAME (frame)->root_window;
}

DEFUN ("frame-selected-window", Fframe_selected_window,
       Sframe_selected_window, 0, 1, 0,
  "Return the selected window of frame object FRAME.\n\
If omitted, FRAME defaults to the currently selected frame.")
  (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    XSET (frame, Lisp_Frame, selected_frame);
  else
    CHECK_LIVE_FRAME (frame, 0);

  return XFRAME (frame)->selected_window;
}

DEFUN ("frame-list", Fframe_list, Sframe_list,
       0, 0, 0,
       "Return a list of all frames.")
  ()
{
  return Fcopy_sequence (Vframe_list);
}

/* Return the next frame in the frame list after FRAME.
   If MINIBUF is nil, exclude minibuffer-only frames.
   If MINIBUF is a window, include only frames using that window for
   their minibuffer.
   If MINIBUF is non-nil, and not a window, include all frames.  */
Lisp_Object
next_frame (frame, minibuf)
     Lisp_Object frame;
     Lisp_Object minibuf;
{
  Lisp_Object tail;
  int passed = 0;

  /* There must always be at least one frame in Vframe_list.  */
  if (! CONSP (Vframe_list))
    abort ();

  /* If this frame is dead, it won't be in Vframe_list, and we'll loop
     forever.  Forestall that.  */
  CHECK_LIVE_FRAME (frame, 0);

  while (1)
    for (tail = Vframe_list; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	Lisp_Object f = XCONS (tail)->car;

	if (passed)
	  {
	    /* Decide whether this frame is eligible to be returned.  */

	    /* If we've looped all the way around without finding any
	       eligible frames, return the original frame.  */
	    if (EQ (f, frame))
	      return f;

	    /* Let minibuf decide if this frame is acceptable.  */
	    if (NILP (minibuf))
	      {
		if (! FRAME_MINIBUF_ONLY_P (XFRAME (f)))
		  return f;
	      }
	    else if (XTYPE (minibuf) == Lisp_Window)
	      {
		if (EQ (FRAME_MINIBUF_WINDOW (XFRAME (f)), minibuf))
		  return f;
	      }
	    else
	      return f;
	  }

	if (EQ (frame, f))
	  passed++;
      }
}

/* Return the previous frame in the frame list before FRAME.
   If MINIBUF is nil, exclude minibuffer-only frames.
   If MINIBUF is a window, include only frames using that window for
   their minibuffer.
   If MINIBUF is non-nil and not a window, include all frames.  */
Lisp_Object
prev_frame (frame, minibuf)
     Lisp_Object frame;
     Lisp_Object minibuf;
{
  Lisp_Object tail;
  Lisp_Object prev;

  /* There must always be at least one frame in Vframe_list.  */
  if (! CONSP (Vframe_list))
    abort ();

  prev = Qnil;
  for (tail = Vframe_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object f = XCONS (tail)->car;

      if (XTYPE (f) != Lisp_Frame)
	abort ();

      if (EQ (frame, f) && !NILP (prev))
	return prev;

      /* Decide whether this frame is eligible to be returned,
	 according to minibuf.  */
      if (NILP (minibuf))
	{
	  if (! FRAME_MINIBUF_ONLY_P (XFRAME (f)))
	    prev = f;
	}
      else if (XTYPE (minibuf) == Lisp_Window)
	{
	  if (EQ (FRAME_MINIBUF_WINDOW (XFRAME (f)), minibuf))
	    prev = f;
	}
      else
	prev = f;
    }

  /* We've scanned the entire list.  */
  if (NILP (prev))
    /* We went through the whole frame list without finding a single
       acceptable frame.  Return the original frame.  */
    return frame;
  else
    /* There were no acceptable frames in the list before FRAME; otherwise,
       we would have returned directly from the loop.  Since PREV is the last
       acceptable frame in the list, return it.  */
    return prev;
}


DEFUN ("next-frame", Fnext_frame, Snext_frame, 0, 2, 0,
  "Return the next frame in the frame list after FRAME.\n\
By default, skip minibuffer-only frames.\n\
If omitted, FRAME defaults to the selected frame.\n\
If optional argument MINIFRAME is nil, exclude minibuffer-only frames.\n\
If MINIFRAME is a window, include only frames using that window for their\n\
minibuffer.\n\
If MINIFRAME is non-nil and not a window, include all frames.")
  (frame, miniframe)
     Lisp_Object frame, miniframe;
{
  Lisp_Object tail;

  if (NILP (frame))
    XSET (frame, Lisp_Frame, selected_frame);
  else
    CHECK_LIVE_FRAME (frame, 0);

  return next_frame (frame, miniframe);
}

DEFUN ("previous-frame", Fprevious_frame, Sprevious_frame, 0, 2, 0,
  "Return the previous frame in the frame list before FRAME.\n\
By default, skip minibuffer-only frames.\n\
If omitted, FRAME defaults to the selected frame.\n\
If optional argument MINIFRAME is nil, exclude minibuffer-only frames.\n\
If MINIFRAME is a window, include only frames using that window for their\n\
minibuffer.\n\
If MINIFRAME is non-nil and not a window, include all frames.")
  (frame, miniframe)
     Lisp_Object frame, miniframe;
{
  Lisp_Object tail;

  if (NILP (frame))
    XSET (frame, Lisp_Frame, selected_frame);
  else
    CHECK_LIVE_FRAME (frame, 0);

  return prev_frame (frame, miniframe);
}


DEFUN ("delete-frame", Fdelete_frame, Sdelete_frame, 0, 2, "",
  "Delete FRAME, permanently eliminating it from use.\n\
If omitted, FRAME defaults to the selected frame.\n\
A frame may not be deleted if its minibuffer is used by other frames.\n\
Normally, you may not delete a frame if all other frames are invisible,\n\
but if the second optional argument FORCE is non-nil, you may do so.")
  (frame, force)
     Lisp_Object frame, force;
{
  struct frame *f;

  if (EQ (frame, Qnil))
    {
      f = selected_frame;
      XSET (frame, Lisp_Frame, f);
    }
  else
    {
      CHECK_FRAME (frame, 0);
      f = XFRAME (frame);
    }

  if (! FRAME_LIVE_P (f))
    return Qnil;

  /* If all other frames are invisible, refuse to delete.
     (Exception: allow deleting the terminal frame when using X.)  */
  if (f == selected_frame && NILP (force))
    {
      Lisp_Object frames;
      int count = 0;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCONS (frames)->cdr)
	{
	  Lisp_Object this = XCONS (frames)->car;

	  if (FRAME_VISIBLE_P (XFRAME (this))
	      || FRAME_ICONIFIED_P (XFRAME (this))
	      /* Allow deleting the terminal frame when at least
		 one X frame exists!  */
	      || (FRAME_X_P (XFRAME (this)) && !FRAME_X_P (f)))
	    count++;
	}
      if (count == 1)
	error ("Attempt to delete the only frame");
    }

  /* Does this frame have a minibuffer, and is it the surrogate
     minibuffer for any other frame?  */
  if (FRAME_HAS_MINIBUF_P (XFRAME (frame)))
    {
      Lisp_Object frames;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCONS (frames)->cdr)
	{
	  Lisp_Object this = XCONS (frames)->car;

	  if (! EQ (this, frame)
	      && EQ (frame,
		     (WINDOW_FRAME
		      (XWINDOW
		       (FRAME_MINIBUF_WINDOW
			(XFRAME (this)))))))
	    error ("Attempt to delete a surrogate minibuffer frame");
	}
    }

  /* Don't let the frame remain selected.  */
  if (f == selected_frame)
    Fhandle_switch_frame (next_frame (frame, Qt), Qnil);

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (f->minibuffer_window, minibuf_window))
    {
      Fset_window_buffer (selected_frame->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_frame->minibuffer_window;
    }

  /* Mark all the windows that used to be on FRAME as deleted, and then
     remove the reference to them.  */
  delete_all_subwindows (XWINDOW (f->root_window));
  f->root_window = Qnil;

  Vframe_list = Fdelq (frame, Vframe_list);
  FRAME_SET_VISIBLE (f, 0);

  /* Since some events are handled at the interrupt level, we may get
     an event for f at any time; if we zero out the frame's display
     now, then we may trip up the event-handling code.  Instead, we'll
     promise that the display of the frame must be valid until we have
     called the window-system-dependent frame destruction routine.  */

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    x_destroy_window (f);
#endif

  f->display.nothing = 0;

  /* If we've deleted the last_nonminibuf_frame, then try to find
     another one.  */
  if (f == last_nonminibuf_frame)
    {
      Lisp_Object frames;

      last_nonminibuf_frame = 0;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCONS (frames)->cdr)
	{
	  f = XFRAME (XCONS (frames)->car);
	  if (!FRAME_MINIBUF_ONLY_P (f))
	    {
	      last_nonminibuf_frame = f;
	      break;
	    }
	}
    }

  /* If we've deleted Vdefault_minibuffer_frame, try to find another
     one.  Prefer minibuffer-only frames, but also notice frames
     with other windows.  */
  if (EQ (frame, Vdefault_minibuffer_frame))
    {
      Lisp_Object frames;

      /* The last frame we saw with a minibuffer, minibuffer-only or not.  */
      Lisp_Object frame_with_minibuf = Qnil;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCONS (frames)->cdr)
	{
	  Lisp_Object this = XCONS (frames)->car;

	  if (XTYPE (this) != Lisp_Frame)
	    abort ();
	  f = XFRAME (this);

	  if (FRAME_HAS_MINIBUF_P (f))
	    {
	      frame_with_minibuf = this;
	      if (FRAME_MINIBUF_ONLY_P (f))
		break;
	    }
	}

      /* We know that there must be some frame with a minibuffer out
	 there.  If this were not true, all of the frames present
	 would have to be minibufferless, which implies that at some
	 point their minibuffer frames must have been deleted, but
	 that is prohibited at the top; you can't delete surrogate
	 minibuffer frames.  */
      if (NILP (frame_with_minibuf))
	abort ();

      Vdefault_minibuffer_frame = frame_with_minibuf;
    }

  return Qnil;
}

/* Return mouse position in character cell units.  */

DEFUN ("mouse-position", Fmouse_position, Smouse_position, 0, 0, 0,
  "Return a list (FRAME X . Y) giving the current mouse frame and position.\n\
The position is given in character cells, where (0, 0) is the\n\
upper-left corner.\n\
If Emacs is running on a mouseless terminal or hasn't been programmed\n\
to read the mouse position, it returns the selected frame for FRAME\n\
and nil for X and Y.")
  ()
{
  FRAME_PTR f;
  Lisp_Object lispy_dummy;
  enum scroll_bar_part party_dummy;
  Lisp_Object x, y;
  unsigned long long_dummy;

  f = selected_frame;
  x = y = Qnil;

  /* It's okay for the hook to refrain from storing anything.  */
  if (mouse_position_hook)
    (*mouse_position_hook) (&f,
			    &lispy_dummy, &party_dummy,
			    &x, &y,
			    &long_dummy);

  XSET (lispy_dummy, Lisp_Frame, f);
  return Fcons (lispy_dummy, Fcons (x, y));
}

DEFUN ("set-mouse-position", Fset_mouse_position, Sset_mouse_position, 3, 3, 0,
  "Move the mouse pointer to the center of character cell (X,Y) in FRAME.\n\
WARNING:  If you use this under X, you should do `unfocus-frame' afterwards.")
  (frame, x, y)
     Lisp_Object frame, x, y;
{
  CHECK_LIVE_FRAME (frame, 0);
  CHECK_NUMBER (x, 2);
  CHECK_NUMBER (y, 1);

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (XFRAME (frame)))
    /* Warping the mouse will cause  enternotify and focus events. */
    x_set_mouse_position (XFRAME (frame), x, y);
#endif

  return Qnil;
}

DEFUN ("make-frame-visible", Fmake_frame_visible, Smake_frame_visible,
       0, 1, "",
  "Make the frame FRAME visible (assuming it is an X-window).\n\
If omitted, FRAME defaults to the currently selected frame.")
  (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    XSET (frame, Lisp_Frame, selected_frame);

  CHECK_LIVE_FRAME (frame, 0);

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (XFRAME (frame)))
    {
      FRAME_SAMPLE_VISIBILITY (XFRAME (frame));
      x_make_frame_visible (XFRAME (frame));
    }
#endif

  return frame;
}

DEFUN ("make-frame-invisible", Fmake_frame_invisible, Smake_frame_invisible,
       0, 1, "",
  "Make the frame FRAME invisible (assuming it is an X-window).\n\
If omitted, FRAME defaults to the currently selected frame.")
  (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    XSET (frame, Lisp_Frame, selected_frame);

  CHECK_LIVE_FRAME (frame, 0);

  /* Don't let the frame remain selected.  */
  if (XFRAME (frame) == selected_frame)
    Fhandle_switch_frame (next_frame (frame, Qt), Qnil);

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (XFRAME (frame)->minibuffer_window, minibuf_window))
    {
      Fset_window_buffer (selected_frame->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_frame->minibuffer_window;
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (XFRAME (frame)))
    x_make_frame_invisible (XFRAME (frame));
#endif

  return Qnil;
}

DEFUN ("iconify-frame", Ficonify_frame, Siconify_frame,
       0, 1, "",
  "Make the frame FRAME into an icon.\n\
If omitted, FRAME defaults to the currently selected frame.")
  (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    XSET (frame, Lisp_Frame, selected_frame);
  
  CHECK_LIVE_FRAME (frame, 0);

  /* Don't let the frame remain selected.  */
  if (XFRAME (frame) == selected_frame)
    Fhandle_switch_frame (next_frame (frame, Qt), Qnil);

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (XFRAME (frame)->minibuffer_window, minibuf_window))
    {
      Fset_window_buffer (selected_frame->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_frame->minibuffer_window;
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (XFRAME (frame)))
      x_iconify_frame (XFRAME (frame));
#endif

  return Qnil;
}

DEFUN ("frame-visible-p", Fframe_visible_p, Sframe_visible_p,
       1, 1, 0,
       "Return t if FRAME is now \"visible\" (actually in use for display).\n\
A frame that is not \"visible\" is not updated and, if it works through\n\
a window system, it may not show at all.\n\
Return the symbol `icon' if frame is visible only as an icon.")
  (frame)
     Lisp_Object frame;
{
  CHECK_LIVE_FRAME (frame, 0);

  if (FRAME_VISIBLE_P (XFRAME (frame)))
    return Qt;
  if (FRAME_ICONIFIED_P (XFRAME (frame)))
    return Qicon;
  return Qnil;
}

DEFUN ("visible-frame-list", Fvisible_frame_list, Svisible_frame_list,
       0, 0, 0,
       "Return a list of all frames now \"visible\" (being updated).")
  ()
{
  Lisp_Object tail, frame;
  struct frame *f;
  Lisp_Object value;

  value = Qnil;
  for (tail = Vframe_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      frame = XCONS (tail)->car;
      if (XTYPE (frame) != Lisp_Frame)
	continue;
      f = XFRAME (frame);
      if (FRAME_VISIBLE_P (f))
	value = Fcons (frame, value);
    }
  return value;
}


DEFUN ("raise-frame", Fraise_frame, Sraise_frame, 1, 1, 0,
  "Bring FRAME to the front, so it occludes any frames it overlaps.\n\
If FRAME is invisible, make it visible.\n\
If Emacs is displaying on an ordinary terminal or some other device which\n\
doesn't support multiple overlapping frames, this function does nothing.")
  (frame)
     Lisp_Object frame;
{
  CHECK_LIVE_FRAME (frame, 0);

  /* Do like the documentation says. */
  Fmake_frame_visible (frame);

  if (frame_raise_lower_hook)
    (*frame_raise_lower_hook) (XFRAME (frame), 1);

  return Qnil;
}

/* Should we have a corresponding function called Flower_Power?  */
DEFUN ("lower-frame", Flower_frame, Slower_frame, 1, 1, 0,
  "Send FRAME to the back, so it is occluded by any frames that overlap it.\n\
If Emacs is displaying on an ordinary terminal or some other device which\n\
doesn't support multiple overlapping frames, this function does nothing.")
  (frame)
     Lisp_Object frame;
{
  CHECK_LIVE_FRAME (frame, 0);
  
  if (frame_raise_lower_hook)
    (*frame_raise_lower_hook) (XFRAME (frame), 0);

  return Qnil;
}


DEFUN ("redirect-frame-focus", Fredirect_frame_focus, Sredirect_frame_focus,
       1, 2, 0,
  "Arrange for keystrokes typed at FRAME to be sent to FOCUS-FRAME.\n\
In other words, switch-frame events caused by events in FRAME will\n\
request a switch to FOCUS-FRAME, and `last-event-frame' will be\n\
FOCUS-FRAME after reading an event typed at FRAME.\n\
\n\
If FOCUS-FRAME is omitted or nil, any existing redirection is\n\
cancelled, and the frame again receives its own keystrokes.\n\
\n\
Focus redirection is useful for temporarily redirecting keystrokes to\n\
a surrogate minibuffer frame when a frame doesn't have its own\n\
minibuffer window.\n\
\n\
A frame's focus redirection can be changed by select-frame.  If frame\n\
FOO is selected, and then a different frame BAR is selected, any\n\
frames redirecting their focus to FOO are shifted to redirect their\n\
focus to BAR.  This allows focus redirection to work properly when the\n\
user switches from one frame to another using `select-window'.\n\
\n\
This means that a frame whose focus is redirected to itself is treated\n\
differently from a frame whose focus is redirected to nil; the former\n\
is affected by select-frame, while the latter is not.\n\
\n\
The redirection lasts until `redirect-frame-focus' is called to change it.")
  (frame, focus_frame)
    Lisp_Object frame, focus_frame;
{
  /* Note that we don't check for a live frame here.  It's reasonable
     to redirect the focus of a frame you're about to delete, if you
     know what other frame should receive those keystrokes.  */
  CHECK_FRAME (frame, 0);

  if (! NILP (focus_frame))
    CHECK_LIVE_FRAME (focus_frame, 1);

  XFRAME (frame)->focus_frame = focus_frame;

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (!NILP (focus_frame) && ! EQ (focus_frame, frame)
      && FRAME_X_P (XFRAME (focus_frame)))
    Ffocus_frame (focus_frame);
#endif

  if (frame_rehighlight_hook)
    (*frame_rehighlight_hook) ();
  
  return Qnil;
}


DEFUN ("frame-focus", Fframe_focus, Sframe_focus, 1, 1, 0,
  "Return the frame to which FRAME's keystrokes are currently being sent.\n\
This returns nil if FRAME's focus is not redirected.\n\
See `redirect-frame-focus'.")
  (frame)
    Lisp_Object frame;
{
  CHECK_LIVE_FRAME (frame, 0);

  return FRAME_FOCUS_FRAME (XFRAME (frame));
}



Lisp_Object
get_frame_param (frame, prop)
     register struct frame *frame;
     Lisp_Object prop;
{
  register Lisp_Object tem;

  tem = Fassq (prop, frame->param_alist);
  if (EQ (tem, Qnil))
    return tem;
  return Fcdr (tem);
}

void
store_in_alist (alistptr, prop, val)
     Lisp_Object *alistptr, val;
     Lisp_Object prop;
{
  register Lisp_Object tem;

  tem = Fassq (prop, *alistptr);
  if (EQ (tem, Qnil))
    *alistptr = Fcons (Fcons (prop, val), *alistptr);
  else
    Fsetcdr (tem, val);
}

void
store_frame_param (f, prop, val)
     struct frame *f;
     Lisp_Object prop, val;
{
  register Lisp_Object tem;

  tem = Fassq (prop, f->param_alist);
  if (EQ (tem, Qnil))
    f->param_alist = Fcons (Fcons (prop, val), f->param_alist);
  else
    Fsetcdr (tem, val);

  if (EQ (prop, Qminibuffer)
      && XTYPE (val) == Lisp_Window)
    {
      if (! MINI_WINDOW_P (XWINDOW (val)))
	error ("Surrogate minibuffer windows must be minibuffer windows.");

      if (FRAME_HAS_MINIBUF_P (f) || FRAME_MINIBUF_ONLY_P (f))
	error ("can't change the surrogate minibuffer of a frame with its own minibuffer");

      /* Install the chosen minibuffer window, with proper buffer.  */
      f->minibuffer_window = val;
    }
}

DEFUN ("frame-parameters", Fframe_parameters, Sframe_parameters, 0, 1, 0,
  "Return the parameters-alist of frame FRAME.\n\
It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.\n\
The meaningful PARMs depend on the kind of frame.\n\
If FRAME is omitted, return information on the currently selected frame.")
  (frame)
     Lisp_Object frame;
{
  Lisp_Object alist;
  struct frame *f;

  if (EQ (frame, Qnil))
    f = selected_frame;
  else
    {
      CHECK_FRAME (frame, 0);
      f = XFRAME (frame);
    }

  if (f->display.nothing == 0)
    return Qnil;

  alist = Fcopy_alist (f->param_alist);
  store_in_alist (&alist, Qname, f->name);
  store_in_alist (&alist, Qheight, make_number (f->height));
  store_in_alist (&alist, Qwidth, make_number (f->width));
  store_in_alist (&alist, Qmodeline, (f->wants_modeline ? Qt : Qnil));
  store_in_alist (&alist, Qminibuffer,
		  (! FRAME_HAS_MINIBUF_P (f) ? Qnil
		   : (FRAME_MINIBUF_ONLY_P (f) ? Qonly
                   : FRAME_MINIBUF_WINDOW (f))));
  store_in_alist (&alist, Qunsplittable, (f->no_split ? Qt : Qnil));
  store_in_alist (&alist, Qmenu_bar_lines, (FRAME_MENU_BAR_LINES (f)));

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    x_report_frame_params (f, &alist);
#endif
  return alist;
}

DEFUN ("modify-frame-parameters", Fmodify_frame_parameters, 
       Smodify_frame_parameters, 2, 2, 0,
  "Modify the parameters of frame FRAME according to ALIST.\n\
ALIST is an alist of parameters to change and their new values.\n\
Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.\n\
The meaningful PARMs depend on the kind of frame; undefined PARMs are ignored.")
  (frame, alist)
     Lisp_Object frame, alist;
{
  FRAME_PTR f;
  register Lisp_Object tail, elt, prop, val;

  if (EQ (frame, Qnil))
    f = selected_frame;
  else
    {
      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
#if 1
    x_set_frame_parameters (f, alist);
#else
    for (tail = alist; !EQ (tail, Qnil); tail = Fcdr (tail))
      {
	elt = Fcar (tail);
	prop = Fcar (elt);
	val = Fcdr (elt);
	x_set_frame_param (f, prop, val, get_frame_param (f, prop));
	store_frame_param (f, prop, val);
      }
#endif
#endif

  return Qnil;
}

DEFUN ("frame-char-height", Fframe_char_height, Sframe_char_height,
  0, 1, 0,
  "Height in pixels of a line in the font in frame FRAME.\n\
If FRAME is omitted, the selected frame is used.\n\
For a terminal frame, the value is always 1.")
  (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_FRAME (frame, 0);
      f = XFRAME (frame);
    }

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    return make_number (x_char_height (f));
  else
#endif
    return make_number (1);
}


DEFUN ("frame-char-width", Fframe_char_width, Sframe_char_width,
  0, 1, 0,
  "Width in pixels of characters in the font in frame FRAME.\n\
If FRAME is omitted, the selected frame is used.\n\
The width is the same for all characters, because\n\
currently Emacs supports only fixed-width fonts.\n\
For a terminal screen, the value is always 1.")
  (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_FRAME (frame, 0);
      f = XFRAME (frame);
    }

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    return make_number (x_char_width (f));
  else
#endif
    return make_number (1);
}

DEFUN ("frame-pixel-height", Fframe_pixel_height, 
       Sframe_pixel_height, 0, 1, 0,
  "Return a FRAME's height in pixels.\n\
For a terminal frame, the result really gives the height in characters.\n\
If FRAME is omitted, the selected frame is used.")
  (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_FRAME (frame, 0);
      f = XFRAME (frame);
    }

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    return make_number (x_pixel_height (f));
  else
#endif
    return make_number (FRAME_HEIGHT (f));
}

DEFUN ("frame-pixel-width", Fframe_pixel_width, 
       Sframe_pixel_width, 0, 1, 0,
  "Return FRAME's width in pixels.\n\
For a terminal frame, the result really gives the width in characters.\n\
If FRAME is omitted, the selected frame is used.")
  (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_FRAME (frame, 0);
      f = XFRAME (frame);
    }

#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    return make_number (x_pixel_width (f));
  else
#endif
    return make_number (FRAME_WIDTH (f));
}

DEFUN ("set-frame-height", Fset_frame_height, Sset_frame_height, 2, 3, 0,
  "Specify that the frame FRAME has LINES lines.\n\
Optional third arg non-nil means that redisplay should use LINES lines\n\
but that the idea of the actual height of the frame should not be changed.")
  (frame, rows, pretend)
     Lisp_Object frame, rows, pretend;
{
  register struct frame *f;

  CHECK_NUMBER (rows, 0);
  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    {
      if (XINT (rows) != f->width)
	x_set_window_size (f, f->width, XINT (rows));
    }
  else
#endif
    change_frame_size (f, XINT (rows), 0, !NILP (pretend), 0);
  return Qnil;
}

DEFUN ("set-frame-width", Fset_frame_width, Sset_frame_width, 2, 3, 0,
  "Specify that the frame FRAME has COLS columns.\n\
Optional third arg non-nil means that redisplay should use COLS columns\n\
but that the idea of the actual width of the frame should not be changed.")
  (frame, cols, pretend)
     Lisp_Object frame, cols, pretend;
{
  register struct frame *f;
  CHECK_NUMBER (cols, 0);
  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    {
      if (XINT (cols) != f->width)
	x_set_window_size (f, XINT (cols), f->height);
    }
  else
#endif
    change_frame_size (f, 0, XINT (cols), !NILP (pretend), 0);
  return Qnil;
}

DEFUN ("set-frame-size", Fset_frame_size, Sset_frame_size, 3, 3, 0,
  "Sets size of FRAME to COLS by ROWS, measured in characters.")
  (frame, cols, rows)
     Lisp_Object frame, cols, rows;
{
  register struct frame *f;
  int mask;

  CHECK_LIVE_FRAME (frame, 0);
  CHECK_NUMBER (cols, 2);
  CHECK_NUMBER (rows, 1);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    {
      if (XINT (rows) != f->height || XINT (cols) != f->width)
	x_set_window_size (f, XINT (cols), XINT (rows));
    }
  else
#endif
    change_frame_size (f, XINT (rows), XINT (cols), 0, 0);

  return Qnil;
}

DEFUN ("set-frame-position", Fset_frame_position, 
       Sset_frame_position, 3, 3, 0,
  "Sets position of FRAME in pixels to XOFFSET by YOFFSET.\n\
This is actually the position of the upper left corner of the frame.\n\
Negative values for XOFFSET or YOFFSET are interpreted relative to\n\
the rightmost or bottommost possible position (that stays within the screen).")
  (frame, xoffset, yoffset)
     Lisp_Object frame, xoffset, yoffset;
{
  register struct frame *f;
  int mask;

  CHECK_LIVE_FRAME (frame, 0);
  CHECK_NUMBER (xoffset, 1);
  CHECK_NUMBER (yoffset, 2);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    x_set_offset (f, XINT (xoffset), XINT (yoffset));
#endif

  return Qt;
}


choose_minibuf_frame ()
{
  /* For lowest-level minibuf, put it on currently selected frame
     if frame has a minibuffer.  */

  if (minibuf_level == 0
      && selected_frame != 0
      && !EQ (minibuf_window, selected_frame->minibuffer_window))
    {
      /* I don't think that any frames may validly have a null minibuffer
	 window anymore.  */
      if (NILP (selected_frame->minibuffer_window))
	abort ();

      Fset_window_buffer (selected_frame->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_frame->minibuffer_window;
    }
}

syms_of_frame ()
{
  /*&&& init symbols here &&&*/
  Qframep = intern ("framep");
  staticpro (&Qframep);
  Qframe_live_p = intern ("frame-live-p");
  staticpro (&Qframe_live_p);
  Qheight = intern ("height");
  staticpro (&Qheight);
  Qicon = intern ("icon");
  staticpro (&Qicon);
  Qminibuffer = intern ("minibuffer");
  staticpro (&Qminibuffer);
  Qmodeline = intern ("modeline");
  staticpro (&Qmodeline);
  Qname = intern ("name");
  staticpro (&Qname);
  Qonly = intern ("only");
  staticpro (&Qonly);
  Qunsplittable = intern ("unsplittable");
  staticpro (&Qunsplittable);
  Qwidth = intern ("width");
  staticpro (&Qwidth);
  Qx = intern ("x");
  staticpro (&Qx);
  Qmenu_bar_lines = intern ("menu-bar-lines");
  staticpro (&Qmenu_bar_lines);

  staticpro (&Vframe_list);

  DEFVAR_LISP ("terminal-frame", &Vterminal_frame,
    "The initial frame-object, which represents Emacs's stdout.");

  DEFVAR_LISP ("emacs-iconified", &Vemacs_iconified,
    "Non-nil if all of emacs is iconified and frame updates are not needed.");
  Vemacs_iconified = Qnil;

  DEFVAR_LISP ("default-minibuffer-frame", &Vdefault_minibuffer_frame,
    "Minibufferless frames use this frame's minibuffer.\n\
\n\
Emacs cannot create minibufferless frames unless this is set to an\n\
appropriate surrogate.\n\
\n\
Emacs consults this variable only when creating minibufferless\n\
frames; once the frame is created, it sticks with its assigned\n\
minibuffer, no matter what this variable is set to.  This means that\n\
this variable doesn't necessarily say anything meaningful about the\n\
current set of frames, or where the minibuffer is currently being\n\
displayed.");
  Vdefault_minibuffer_frame = Qnil;

  DEFVAR_LISP ("default-frame-alist", &Vdefault_frame_alist,
    "Alist of default values for frame creation.\n\
These may be set in your init file, like this:\n\
  (setq default-frame-alist '((width . 80) (height . 55)))\n\
These override values given in window system configuration data, like\n\
X Windows' defaults database.\n\
For values specific to the first Emacs frame, see `initial-frame-alist'.\n\
For values specific to the separate minibuffer frame, see\n\
`minibuffer-frame-alist'.");
  Vdefault_frame_alist = Qnil;

  defsubr (&Sframep);
  defsubr (&Sframe_live_p);
  defsubr (&Shandle_switch_frame);
  defsubr (&Sselect_frame);
  defsubr (&Sselected_frame);
  defsubr (&Swindow_frame);
  defsubr (&Sframe_root_window);
  defsubr (&Sframe_selected_window);
  defsubr (&Sframe_list);
  defsubr (&Snext_frame);
  defsubr (&Sprevious_frame);
  defsubr (&Sdelete_frame);
  defsubr (&Smouse_position);
  defsubr (&Sset_mouse_position);
#if 0
  defsubr (&Sframe_configuration);
  defsubr (&Srestore_frame_configuration);
#endif
  defsubr (&Smake_frame_visible);
  defsubr (&Smake_frame_invisible);
  defsubr (&Siconify_frame);
  defsubr (&Sframe_visible_p);
  defsubr (&Svisible_frame_list);
  defsubr (&Sraise_frame);
  defsubr (&Slower_frame);
  defsubr (&Sredirect_frame_focus);
  defsubr (&Sframe_focus);
  defsubr (&Sframe_parameters);
  defsubr (&Smodify_frame_parameters);
  defsubr (&Sframe_char_height);
  defsubr (&Sframe_char_width);
  defsubr (&Sframe_pixel_height);
  defsubr (&Sframe_pixel_width);
  defsubr (&Sset_frame_height);
  defsubr (&Sset_frame_width);
  defsubr (&Sset_frame_size);
  defsubr (&Sset_frame_position);
}

keys_of_frame ()
{
  initial_define_lispy_key (global_map, "switch-frame", "handle-switch-frame");
}

#else /* not MULTI_FRAME */

/* If we're not using multi-frame stuff, we still need to provide some
   support functions.  */

/* Unless this function is defined, providing set-frame-height and
   set-frame-width doesn't help compatibility any, since they both
   want this as their first argument.  */
DEFUN ("selected-frame", Fselected_frame, Sselected_frame, 0, 0, 0,
  "Return the frame that is now selected.")
  ()
{
  Lisp_Object tem;
  XFASTINT (tem) = 0;
  return tem;
}
DEFUN ("framep", Fframep, Sframep, 1, 1, 0,
  "Return non-nil if OBJECT is a frame.\n\
Value is t for a termcap frame (a character-only terminal),\n\
`x' for an Emacs frame that is really an X window.\n\
Also see `live-frame-p'.")
  (object)
     Lisp_Object object;
{
  return Qnil;
}

DEFUN ("set-frame-height", Fset_frame_height, Sset_frame_height, 2, 3, 0,
  "Specify that the frame FRAME has LINES lines.\n\
Optional third arg non-nil means that redisplay should use LINES lines\n\
but that the idea of the actual height of the frame should not be changed.")
  (frame, rows, pretend)
     Lisp_Object frame, rows, pretend;
{
  CHECK_NUMBER (rows, 0);

  change_frame_size (0, XINT (rows), 0, !NILP (pretend), 0);
  return Qnil;
}

DEFUN ("set-frame-width", Fset_frame_width, Sset_frame_width, 2, 3, 0,
  "Specify that the frame FRAME has COLS columns.\n\
Optional third arg non-nil means that redisplay should use COLS columns\n\
but that the idea of the actual width of the frame should not be changed.")
  (frame, cols, pretend)
     Lisp_Object frame, cols, pretend;
{
  CHECK_NUMBER (cols, 0);

  change_frame_size (0, 0, XINT (cols), !NILP (pretend), 0);
  return Qnil;
}

DEFUN ("set-frame-size", Fset_frame_size, Sset_frame_size, 3, 3, 0,
  "Sets size of FRAME to COLS by ROWS, measured in characters.")
  (frame, cols, rows)
     Lisp_Object frame, cols, rows;
{
  CHECK_NUMBER (cols, 2);
  CHECK_NUMBER (rows, 1);

  change_frame_size (0, XINT (rows), XINT (cols), 0, 0);

  return Qnil;
}

DEFUN ("frame-height", Fframe_height, Sframe_height, 0, 1, 0,
  "Return number of lines available for display on FRAME.\n\
If FRAME is omitted, describe the currently selected frame.")
  (frame)
    Lisp_Object frame;
{
  return make_number (FRAME_HEIGHT (selected_frame));
}

DEFUN ("frame-width", Fframe_width, Sframe_width, 0, 1, 0,
  "Return number of columns available for display on FRAME.\n\
If FRAME is omitted, describe the currently selected frame.")
  (frame)
    Lisp_Object frame;
{
  return make_number (FRAME_WIDTH (selected_frame));
}

DEFUN ("frame-char-height", Fframe_char_height, Sframe_char_height,
  0, 1, 0,
  "Height in pixels of a line in the font in frame FRAME.\n\
If FRAME is omitted, the selected frame is used.\n\
For a terminal frame, the value is always 1.")
  (frame)
     Lisp_Object frame;
{
  return make_number (1);
}


DEFUN ("frame-char-width", Fframe_char_width, Sframe_char_width,
  0, 1, 0,
  "Width in pixels of characters in the font in frame FRAME.\n\
If FRAME is omitted, the selected frame is used.\n\
The width is the same for all characters, because\n\
currently Emacs supports only fixed-width fonts.\n\
For a terminal screen, the value is always 1.")
  (frame)
     Lisp_Object frame;
{
  return make_number (1);
}

DEFUN ("frame-pixel-height", Fframe_pixel_height, 
       Sframe_pixel_height, 0, 1, 0,
  "Return FRAME's height in pixels.\n\
For a terminal frame, the result really gives the height in characters.\n\
If FRAME is omitted, the selected frame is used.")
  (frame)
     Lisp_Object frame;
{
  return make_number (FRAME_HEIGHT (f));
}

DEFUN ("frame-pixel-width", Fframe_pixel_width, 
       Sframe_pixel_width, 0, 1, 0,
  "Return FRAME's width in pixels.\n\
For a terminal frame, the result really gives the width in characters.\n\
If FRAME is omitted, the selected frame is used.")
  (frame)
     Lisp_Object frame;
{
  return make_number (FRAME_WIDTH (f));
}

/* These are for backward compatibility with Emacs 18.  */

DEFUN ("set-screen-height", Fset_screen_height, Sset_screen_height, 1, 2, 0,
  "Tell redisplay that the screen has LINES lines.\n\
Optional second arg non-nil means that redisplay should use LINES lines\n\
but that the idea of the actual height of the screen should not be changed.")
  (lines, pretend)
     Lisp_Object lines, pretend;
{
  CHECK_NUMBER (lines, 0);

  change_frame_size (0, XINT (lines), 0, !NILP (pretend), 0);
  return Qnil;
}

DEFUN ("set-screen-width", Fset_screen_width, Sset_screen_width, 1, 2, 0,
  "Tell redisplay that the screen has COLS columns.\n\
Optional second arg non-nil means that redisplay should use COLS columns\n\
but that the idea of the actual width of the screen should not be changed.")
  (cols, pretend)
     Lisp_Object cols, pretend;
{
  CHECK_NUMBER (cols, 0);

  change_frame_size (0, 0, XINT (cols), !NILP (pretend), 0);
  return Qnil;
}

syms_of_frame ()
{
  defsubr (&Sselected_frame);
  defsubr (&Sframep);
  defsubr (&Sframe_char_height);
  defsubr (&Sframe_char_width);
  defsubr (&Sframe_pixel_height);
  defsubr (&Sframe_pixel_width);
  defsubr (&Sset_frame_height);
  defsubr (&Sset_frame_width);
  defsubr (&Sset_frame_size);
  defsubr (&Sset_screen_height);
  defsubr (&Sset_screen_width);
  defsubr (&Sframe_height);
  Ffset (intern ("screen-height"), intern ("frame-height"));
  defsubr (&Sframe_width);
  Ffset (intern ("screen-width"), intern ("frame-width"));
}

keys_of_frame ()
{
}

#endif /* not MULTI_FRAME */




