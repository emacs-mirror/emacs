/* Generic frame functions.
   Copyright (C) 1993, 1994, 1995, 1997, 1999, 2000, 2001
   Free Software Foundation.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <config.h>

#include <stdio.h>
#include "lisp.h"
#include "charset.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif
#ifdef WINDOWSNT
#include "w32term.h"
#endif
#ifdef MAC_OS
#include "macterm.h"
#endif
#include "buffer.h"
/* These help us bind and responding to switch-frame events.  */
#include "commands.h"
#include "keyboard.h"
#include "frame.h"
#ifdef HAVE_WINDOW_SYSTEM
#include "fontset.h"
#endif
#include "termhooks.h"
#include "dispextern.h"
#include "window.h"
#ifdef MSDOS
#include "msdos.h"
#include "dosfns.h"
#endif

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
Lisp_Object Qtool_bar_lines;
Lisp_Object Qwidth;
Lisp_Object Qx;
Lisp_Object Qw32;
Lisp_Object Qpc;
Lisp_Object Qmac;
Lisp_Object Qvisible;
Lisp_Object Qbuffer_predicate;
Lisp_Object Qbuffer_list;
Lisp_Object Qtitle;
Lisp_Object Qdisplay_type;
Lisp_Object Qbackground_mode;
Lisp_Object Qinhibit_default_face_x_resources;
Lisp_Object Qleft_fringe;
Lisp_Object Qright_fringe;
Lisp_Object Qtty_color_mode;

Lisp_Object Vterminal_frame;
Lisp_Object Vdefault_frame_alist;
Lisp_Object Vmouse_position_function;
Lisp_Object Vmouse_highlight;

static void
set_menu_bar_lines_1 (window, n)
  Lisp_Object window;
  int n;
{
  struct window *w = XWINDOW (window);

  XSETFASTINT (w->last_modified, 0);
  XSETFASTINT (w->top, XFASTINT (w->top) + n);
  XSETFASTINT (w->height, XFASTINT (w->height) - n);
  
  if (INTEGERP (w->orig_top))
    XSETFASTINT (w->orig_top, XFASTINT (w->orig_top) + n);
  if (INTEGERP (w->orig_height))
    XSETFASTINT (w->orig_height, XFASTINT (w->orig_height) - n);

  /* Handle just the top child in a vertical split.  */
  if (!NILP (w->vchild))
    set_menu_bar_lines_1 (w->vchild, n);

  /* Adjust all children in a horizontal split.  */
  for (window = w->hchild; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      set_menu_bar_lines_1 (window, n);
    }
}

void
set_menu_bar_lines (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  int nlines;
  int olines = FRAME_MENU_BAR_LINES (f);

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itself, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (INTEGERP (value))
    nlines = XINT (value);
  else
    nlines = 0;

  if (nlines != olines)
    {
      windows_or_buffers_changed++;
      FRAME_WINDOW_SIZES_CHANGED (f) = 1;
      FRAME_MENU_BAR_LINES (f) = nlines;
      set_menu_bar_lines_1 (f->root_window, nlines - olines);
      adjust_glyphs (f);
    }
}

Lisp_Object Vemacs_iconified;
Lisp_Object Vframe_list;

struct x_output tty_display;

extern Lisp_Object Vminibuffer_list;
extern Lisp_Object get_minibuffer ();
extern Lisp_Object Fhandle_switch_frame ();
extern Lisp_Object Fredirect_frame_focus ();
extern Lisp_Object x_get_focus_frame ();

DEFUN ("framep", Fframep, Sframep, 1, 1, 0,
       doc: /* Return non-nil if OBJECT is a frame.
Value is t for a termcap frame (a character-only terminal),
`x' for an Emacs frame that is really an X window,
`w32' for an Emacs frame that is a window on MS-Windows display,
`mac' for an Emacs frame on a Macintosh display,
`pc' for a direct-write MS-DOS frame.
See also `frame-live-p'.  */)
     (object)
     Lisp_Object object;
{
  if (!FRAMEP (object))
    return Qnil;
  switch (XFRAME (object)->output_method)
    {
    case output_termcap:
      return Qt;
    case output_x_window:
      return Qx;
    case output_w32:
      return Qw32;
    case output_msdos_raw:
      return Qpc;
    case output_mac:
      return Qmac;
    default:
      abort ();
    }
}

DEFUN ("frame-live-p", Fframe_live_p, Sframe_live_p, 1, 1, 0,
       doc: /* Return non-nil if OBJECT is a frame which has not been deleted.
Value is nil if OBJECT is not a live frame.  If object is a live
frame, the return value indicates what sort of output device it is
displayed on.  See the documentation of `framep' for possible
return values.  */)
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

  f = allocate_frame ();
  XSETFRAME (frame, f);

  f->desired_matrix = 0;
  f->current_matrix = 0;
  f->desired_pool = 0;
  f->current_pool = 0;
  f->glyphs_initialized_p = 0;
  f->decode_mode_spec_buffer = 0;
  f->visible = 0;
  f->async_visible = 0;
  f->output_data.nothing = 0;
  f->iconified = 0;
  f->async_iconified = 0;
  f->wants_modeline = 1;
  f->auto_raise = 0;
  f->auto_lower = 0;
  f->no_split = 0;
  f->garbaged = 1;
  f->has_minibuffer = mini_p;
  f->focus_frame = Qnil;
  f->explicit_name = 0;
  f->can_have_scroll_bars = 0;
  f->vertical_scroll_bar_type = vertical_scroll_bar_none;
  f->param_alist = Qnil;
  f->scroll_bars = Qnil;
  f->condemned_scroll_bars = Qnil;
  f->face_alist = Qnil;
  f->face_cache = NULL;
  f->menu_bar_items = Qnil;
  f->menu_bar_vector = Qnil;
  f->menu_bar_items_used = 0;
  f->buffer_predicate = Qnil;
  f->buffer_list = Qnil;
#ifdef MULTI_KBOARD
  f->kboard = initial_kboard;
#endif
  f->namebuf = 0;
  f->title = Qnil;
  f->menu_bar_window = Qnil;
  f->tool_bar_window = Qnil;
  f->tool_bar_items = Qnil;
  f->desired_tool_bar_string = f->current_tool_bar_string = Qnil;
  f->n_tool_bar_items = 0;

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

  SET_FRAME_WIDTH (f, 10);
  f->height = 10;

  XSETFASTINT (XWINDOW (root_window)->width, 10);
  XSETFASTINT (XWINDOW (root_window)->height, (mini_p ? 9 : 10));

  if (mini_p)
    {
      XSETFASTINT (XWINDOW (mini_window)->width, 10);
      XSETFASTINT (XWINDOW (mini_window)->top, 9);
      XSETFASTINT (XWINDOW (mini_window)->height, 1);
    }

  /* Choose a buffer for the frame's root window.  */
  {
    Lisp_Object buf;

    XWINDOW (root_window)->buffer = Qt;
    buf = Fcurrent_buffer ();
    /* If buf is a 'hidden' buffer (i.e. one whose name starts with
       a space), try to find another one.  */
    if (XSTRING (Fbuffer_name (buf))->data[0] == ' ')
      buf = Fother_buffer (buf, Qnil, Qnil);

    /* Use set_window_buffer, not Fset_window_buffer, and don't let
       hooks be run by it.  The reason is that the whole frame/window
       arrangement is not yet fully intialized at this point.  Windows
       don't have the right size, glyph matrices aren't initialized
       etc.  Running Lisp functions at this point surely ends in a
       SEGV.  */
    set_window_buffer (root_window, buf, 0);
    f->buffer_list = Fcons (buf, Qnil);
  }

  if (mini_p)
    {
      XWINDOW (mini_window)->buffer = Qt;
      set_window_buffer (mini_window,
			 (NILP (Vminibuffer_list)
			  ? get_minibuffer (0)
			  : Fcar (Vminibuffer_list)),
			 0);
    }

  f->root_window = root_window;
  f->selected_window = root_window;
  /* Make sure this window seems more recently used than
     a newly-created, never-selected window.  */
  XSETFASTINT (XWINDOW (f->selected_window)->use_time, ++window_select_count);

  return f;
}

#ifdef HAVE_WINDOW_SYSTEM
/* Make a frame using a separate minibuffer window on another frame.
   MINI_WINDOW is the minibuffer window to use.  nil means use the
   default (the global minibuffer).  */

struct frame *
make_frame_without_minibuffer (mini_window, kb, display)
     register Lisp_Object mini_window;
     KBOARD *kb;
     Lisp_Object display;
{
  register struct frame *f;
  struct gcpro gcpro1;

  if (!NILP (mini_window))
    CHECK_LIVE_WINDOW (mini_window);

#ifdef MULTI_KBOARD
  if (!NILP (mini_window)
      && XFRAME (XWINDOW (mini_window)->frame)->kboard != kb)
    error ("frame and minibuffer must be on the same display");
#endif

  /* Make a frame containing just a root window.  */
  f = make_frame (0);

  if (NILP (mini_window))
    {
      /* Use default-minibuffer-frame if possible.  */
      if (!FRAMEP (kb->Vdefault_minibuffer_frame)
	  || ! FRAME_LIVE_P (XFRAME (kb->Vdefault_minibuffer_frame)))
	{
          Lisp_Object frame_dummy;

          XSETFRAME (frame_dummy, f);
          GCPRO1 (frame_dummy);
	  /* If there's no minibuffer frame to use, create one.  */
	  kb->Vdefault_minibuffer_frame =
	    call1 (intern ("make-initial-minibuffer-frame"), display);
          UNGCPRO;
	}
   
      mini_window = XFRAME (kb->Vdefault_minibuffer_frame)->minibuffer_window;
    }

  f->minibuffer_window = mini_window;

  /* Make the chosen minibuffer window display the proper minibuffer,
     unless it is already showing a minibuffer.  */
  if (NILP (Fmemq (XWINDOW (mini_window)->buffer, Vminibuffer_list)))
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

  XSETFRAME (frame, f);

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
#endif /* HAVE_WINDOW_SYSTEM */

/* Construct a frame that refers to the terminal (stdin and stdout).  */

static int terminal_frame_count;

struct frame *
make_terminal_frame ()
{
  register struct frame *f;
  Lisp_Object frame;
  char name[20];

#ifdef MULTI_KBOARD
  if (!initial_kboard)
    {
      initial_kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
      init_kboard (initial_kboard);
      initial_kboard->next_kboard = all_kboards;
      all_kboards = initial_kboard;
    }
#endif

  /* The first call must initialize Vframe_list.  */
  if (! (NILP (Vframe_list) || CONSP (Vframe_list)))
    Vframe_list = Qnil;

  f = make_frame (1);

  XSETFRAME (frame, f);
  Vframe_list = Fcons (frame, Vframe_list);

  terminal_frame_count++;
  sprintf (name, "F%d", terminal_frame_count);
  f->name = build_string (name);

  f->visible = 1;		/* FRAME_SET_VISIBLE wd set frame_garbaged. */
  f->async_visible = 1;		/* Don't let visible be cleared later. */
#ifdef MSDOS
  f->output_data.x = &the_only_x_display;
  if (!inhibit_window_system
      && (!FRAMEP (selected_frame) || !FRAME_LIVE_P (XFRAME (selected_frame))
	  || XFRAME (selected_frame)->output_method == output_msdos_raw))
    {
      f->output_method = output_msdos_raw;
      /* This initialization of foreground and background pixels is
	 only important for the initial frame created in temacs.  If
	 we don't do that, we get black background and foreground in
	 the dumped Emacs because the_only_x_display is a static
	 variable, hence it is born all-zeroes, and zero is the code
	 for the black color.  Other frames all inherit their pixels
	 from what's already in the_only_x_display.  */
      if ((!FRAMEP (selected_frame) || !FRAME_LIVE_P (XFRAME (selected_frame)))
	  && f->output_data.x->background_pixel == 0
	  && f->output_data.x->foreground_pixel == 0)
	{
	  f->output_data.x->background_pixel = FACE_TTY_DEFAULT_BG_COLOR;
	  f->output_data.x->foreground_pixel = FACE_TTY_DEFAULT_FG_COLOR;
	}
    }
  else
    f->output_method = output_termcap;
#else
#ifdef WINDOWSNT
  f->output_method = output_termcap;
  f->output_data.x = &tty_display;
#else
#ifdef MAC_OS8
  make_mac_terminal_frame (f);
#else
  f->output_data.x = &tty_display;
#endif /* MAC_OS8 */
#endif /* WINDOWSNT */
#endif /* MSDOS */

  if (!noninteractive)
    init_frame_faces (f);

  return f;
}

DEFUN ("make-terminal-frame", Fmake_terminal_frame, Smake_terminal_frame,
       1, 1, 0,
       doc: /* Create an additional terminal frame.
You can create multiple frames on a text-only terminal in this way.
Only the selected terminal frame is actually displayed.
This function takes one argument, an alist specifying frame parameters.
In practice, generally you don't need to specify any parameters.
Note that changing the size of one terminal frame automatically affects all.  */)
     (parms)
     Lisp_Object parms;
{
  struct frame *f;
  Lisp_Object frame, tem;
  struct frame *sf = SELECTED_FRAME ();

#ifdef MSDOS
  if (sf->output_method != output_msdos_raw
      && sf->output_method != output_termcap)
    abort ();
#else /* not MSDOS */

#ifdef MAC_OS
  if (sf->output_method != output_mac)
    error ("Not running on a Macintosh screen; cannot make a new Macintosh frame");
#else
  if (sf->output_method != output_termcap)
    error ("Not using an ASCII terminal now; cannot make a new ASCII frame");
#endif
#endif /* not MSDOS */

  f = make_terminal_frame ();

  change_frame_size (f, FRAME_HEIGHT (sf),
		     FRAME_WIDTH (sf), 0, 0, 0);
  adjust_glyphs (f);
  calculate_costs (f);
  XSETFRAME (frame, f);
  Fmodify_frame_parameters (frame, Vdefault_frame_alist);
  Fmodify_frame_parameters (frame, parms);

  /* Make the frame face alist be frame-specific, so that each
     frame could change its face definitions independently.  */
  f->face_alist = Fcopy_alist (sf->face_alist);
  /* Simple Fcopy_alist isn't enough, because we need the contents of
     the vectors which are the CDRs of associations in face_alist to
     be copied as well.  */
  for (tem = f->face_alist; CONSP (tem); tem = XCDR (tem))
    XSETCDR (XCAR (tem), Fcopy_sequence (XCDR (XCAR (tem))));
  return frame;
}


/* Perform the switch to frame FRAME.

   If FRAME is a switch-frame event `(switch-frame FRAME1)', use
   FRAME1 as frame.

   If TRACK is non-zero and the frame that currently has the focus
   redirects its focus to the selected frame, redirect that focused
   frame's focus to FRAME instead.

   FOR_DELETION non-zero means that the selected frame is being
   deleted, which includes the possibility that the frame's display
   is dead.  */

Lisp_Object
do_switch_frame (frame, track, for_deletion)
     Lisp_Object frame;
     int track, for_deletion;
{
  struct frame *sf = SELECTED_FRAME ();
  
  /* If FRAME is a switch-frame event, extract the frame we should
     switch to.  */
  if (CONSP (frame)
      && EQ (XCAR (frame), Qswitch_frame)
      && CONSP (XCDR (frame)))
    frame = XCAR (XCDR (frame));

  /* This used to say CHECK_LIVE_FRAME, but apparently it's possible for
     a switch-frame event to arrive after a frame is no longer live,
     especially when deleting the initial frame during startup.  */
  CHECK_FRAME (frame);
  if (! FRAME_LIVE_P (XFRAME (frame)))
    return Qnil;

  if (sf == XFRAME (frame))
    return frame;

  /* This is too greedy; it causes inappropriate focus redirection
     that's hard to get rid of.  */
#if 0
  /* If a frame's focus has been redirected toward the currently
     selected frame, we should change the redirection to point to the
     newly selected frame.  This means that if the focus is redirected
     from a minibufferless frame to a surrogate minibuffer frame, we
     can use `other-window' to switch between all the frames using
     that minibuffer frame, and the focus redirection will follow us
     around.  */
  if (track)
    {
      Lisp_Object tail;

      for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object focus;

	  if (!FRAMEP (XCAR (tail)))
	    abort ();

	  focus = FRAME_FOCUS_FRAME (XFRAME (XCAR (tail)));

	  if (FRAMEP (focus) && XFRAME (focus) == SELECTED_FRAME ())
	    Fredirect_frame_focus (XCAR (tail), frame);
	}
    }
#else /* ! 0 */
  /* Instead, apply it only to the frame we're pointing to.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (track && FRAME_WINDOW_P (XFRAME (frame)))
    {
      Lisp_Object focus, xfocus;

      xfocus = x_get_focus_frame (XFRAME (frame));
      if (FRAMEP (xfocus))
	{
	  focus = FRAME_FOCUS_FRAME (XFRAME (xfocus));
	  if (FRAMEP (focus) && XFRAME (focus) == SELECTED_FRAME ())
	    Fredirect_frame_focus (xfocus, frame);
	}
    }
#endif /* HAVE_X_WINDOWS */
#endif /* ! 0 */

  if (!for_deletion && FRAME_HAS_MINIBUF_P (sf))
    resize_mini_window (XWINDOW (FRAME_MINIBUF_WINDOW (sf)), 1);

  selected_frame = frame;
  if (! FRAME_MINIBUF_ONLY_P (XFRAME (selected_frame)))
    last_nonminibuf_frame = XFRAME (selected_frame);

  Fselect_window (XFRAME (frame)->selected_window);

#ifndef WINDOWSNT
  /* Make sure to switch the tty color mode to that of the newly
     selected frame.  */
  sf = SELECTED_FRAME ();
  if (FRAME_TERMCAP_P (sf))
    {
      Lisp_Object color_mode_spec, color_mode;

      color_mode_spec = assq_no_quit (Qtty_color_mode, sf->param_alist);
      if (CONSP (color_mode_spec))
	color_mode = XCDR (color_mode_spec);
      else
	color_mode = make_number (0);
      set_tty_color_mode (sf, color_mode);
    }
#endif /* !WINDOWSNT */

  /* We want to make sure that the next event generates a frame-switch
     event to the appropriate frame.  This seems kludgy to me, but
     before you take it out, make sure that evaluating something like
     (select-window (frame-root-window (new-frame))) doesn't end up
     with your typing being interpreted in the new frame instead of
     the one you're actually typing in.  */
  internal_last_event_frame = Qnil;

  return frame;
}

DEFUN ("select-frame", Fselect_frame, Sselect_frame, 1, 2, "e",
       doc: /* Select the frame FRAME.
Subsequent editing commands apply to its selected window.
The selection of FRAME lasts until the next time the user does
something to select a different frame, or until the next time this
function is called.  */)
  (frame, no_enter)
    Lisp_Object frame, no_enter;
{
  return do_switch_frame (frame, 1, 0);
}


DEFUN ("handle-switch-frame", Fhandle_switch_frame, Shandle_switch_frame, 1, 2, "e",
       doc: /* Handle a switch-frame event EVENT.
Switch-frame events are usually bound to this function.
A switch-frame event tells Emacs that the window manager has requested
that the user's events be directed to the frame mentioned in the event.
This function selects the selected window of the frame of EVENT.

If EVENT is frame object, handle it as if it were a switch-frame event
to that frame.  */)
     (event, no_enter)
     Lisp_Object event, no_enter;
{
  /* Preserve prefix arg that the command loop just cleared.  */
  current_kboard->Vprefix_arg = Vcurrent_prefix_arg;
  call1 (Vrun_hooks, Qmouse_leave_buffer_hook);
  return do_switch_frame (event, 0, 0);
}

DEFUN ("ignore-event", Fignore_event, Signore_event, 0, 0, "",
       doc: /* Do nothing, but preserve any prefix argument already specified.
This is a suitable binding for iconify-frame and make-frame-visible.  */)
     ()
{
  current_kboard->Vprefix_arg = Vcurrent_prefix_arg;
  return Qnil;
}

DEFUN ("selected-frame", Fselected_frame, Sselected_frame, 0, 0, 0,
       doc: /* Return the frame that is now selected.  */)
     ()
{
  return selected_frame;
}

DEFUN ("window-frame", Fwindow_frame, Swindow_frame, 1, 1, 0,
       doc: /* Return the frame object that window WINDOW is on.  */)
     (window)
     Lisp_Object window;
{
  CHECK_LIVE_WINDOW (window);
  return XWINDOW (window)->frame;
}

DEFUN ("frame-first-window", Fframe_first_window, Sframe_first_window, 0, 1, 0,
       doc: /* Returns the topmost, leftmost window of FRAME.
If omitted, FRAME defaults to the currently selected frame.  */)
     (frame)
     Lisp_Object frame;
{
  Lisp_Object w;

  if (NILP (frame))
    w = SELECTED_FRAME ()->root_window;
  else
    {
      CHECK_LIVE_FRAME (frame);
      w = XFRAME (frame)->root_window;
    }
  while (NILP (XWINDOW (w)->buffer))
    {
      if (! NILP (XWINDOW (w)->hchild))
	w = XWINDOW (w)->hchild;
      else if (! NILP (XWINDOW (w)->vchild))
	w = XWINDOW (w)->vchild;
      else
	abort ();
    }
  return w;
}

DEFUN ("active-minibuffer-window", Factive_minibuffer_window,
       Sactive_minibuffer_window, 0, 0, 0,
       doc: /* Return the currently active minibuffer window, or nil if none.  */)
     ()
{
  return minibuf_level ? minibuf_window : Qnil;
}

DEFUN ("frame-root-window", Fframe_root_window, Sframe_root_window, 0, 1, 0,
       doc: /* Returns the root-window of FRAME.
If omitted, FRAME defaults to the currently selected frame.  */)
     (frame)
     Lisp_Object frame;
{
  Lisp_Object window;
  
  if (NILP (frame))
    window = SELECTED_FRAME ()->root_window;
  else
    {
      CHECK_LIVE_FRAME (frame);
      window = XFRAME (frame)->root_window;
    }
  
  return window;
}

DEFUN ("frame-selected-window", Fframe_selected_window,
       Sframe_selected_window, 0, 1, 0,
       doc: /* Return the selected window of frame object FRAME.
If omitted, FRAME defaults to the currently selected frame.  */)
     (frame)
     Lisp_Object frame;
{
  Lisp_Object window;
  
  if (NILP (frame))
    window = SELECTED_FRAME ()->selected_window;
  else
    {
      CHECK_LIVE_FRAME (frame);
      window = XFRAME (frame)->selected_window;
    }

  return window;
}

DEFUN ("set-frame-selected-window", Fset_frame_selected_window,
       Sset_frame_selected_window, 2, 2, 0,
       doc: /* Set the selected window of frame object FRAME to WINDOW.
If FRAME is nil, the selected frame is used.
If FRAME is the selected frame, this makes WINDOW the selected window.  */)
     (frame, window)
     Lisp_Object frame, window;
{
  if (NILP (frame))
    frame = selected_frame;
  
  CHECK_LIVE_FRAME (frame);
  CHECK_LIVE_WINDOW (window);

  if (! EQ (frame, WINDOW_FRAME (XWINDOW (window))))
    error ("In `set-frame-selected-window', WINDOW is not on FRAME");

  if (EQ (frame, selected_frame))
    return Fselect_window (window);

  return XFRAME (frame)->selected_window = window;
}

DEFUN ("frame-list", Fframe_list, Sframe_list,
       0, 0, 0,
       doc: /* Return a list of all frames.  */)
     ()
{
  Lisp_Object frames;
  frames = Fcopy_sequence (Vframe_list);
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAMEP (tip_frame))
    frames = Fdelq (tip_frame, frames);
#endif
  return frames;
}

/* Return the next frame in the frame list after FRAME.
   If MINIBUF is nil, exclude minibuffer-only frames.
   If MINIBUF is a window, include only its own frame
   and any frame now using that window as the minibuffer.
   If MINIBUF is `visible', include all visible frames.
   If MINIBUF is 0, include all visible and iconified frames.
   Otherwise, include all frames.  */

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
  CHECK_LIVE_FRAME (frame);

  while (1)
    for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
      {
	Lisp_Object f;

	f = XCAR (tail);

	if (passed
	    && FRAME_KBOARD (XFRAME (f)) == FRAME_KBOARD (XFRAME (frame)))
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
	    else if (EQ (minibuf, Qvisible))
	      {
		FRAME_SAMPLE_VISIBILITY (XFRAME (f));
		if (FRAME_VISIBLE_P (XFRAME (f)))
		  return f;
	      }
	    else if (INTEGERP (minibuf) && XINT (minibuf) == 0)
	      {
		FRAME_SAMPLE_VISIBILITY (XFRAME (f));
		if (FRAME_VISIBLE_P (XFRAME (f))
		    || FRAME_ICONIFIED_P (XFRAME (f)))
		  return f;
	      }
	    else if (WINDOWP (minibuf))
	      {
		if (EQ (FRAME_MINIBUF_WINDOW (XFRAME (f)), minibuf)
		    || EQ (WINDOW_FRAME (XWINDOW (minibuf)), f)
		    || EQ (WINDOW_FRAME (XWINDOW (minibuf)),
			   FRAME_FOCUS_FRAME (XFRAME (f))))
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
   If MINIBUF is a window, include only its own frame
   and any frame now using that window as the minibuffer.
   If MINIBUF is `visible', include all visible frames.
   If MINIBUF is 0, include all visible and iconified frames.
   Otherwise, include all frames.  */

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
  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object f;

      f = XCAR (tail);
      if (!FRAMEP (f))
	abort ();

      if (EQ (frame, f) && !NILP (prev))
	return prev;

      if (FRAME_KBOARD (XFRAME (f)) == FRAME_KBOARD (XFRAME (frame)))
	{
	  /* Decide whether this frame is eligible to be returned,
	     according to minibuf.  */
	  if (NILP (minibuf))
	    {
	      if (! FRAME_MINIBUF_ONLY_P (XFRAME (f)))
		prev = f;
	    }
	  else if (WINDOWP (minibuf))
	    {
	      if (EQ (FRAME_MINIBUF_WINDOW (XFRAME (f)), minibuf)
		  || EQ (WINDOW_FRAME (XWINDOW (minibuf)), f)
		  || EQ (WINDOW_FRAME (XWINDOW (minibuf)),
			 FRAME_FOCUS_FRAME (XFRAME (f))))
		prev = f;
	    }
	  else if (EQ (minibuf, Qvisible))
	    {
	      FRAME_SAMPLE_VISIBILITY (XFRAME (f));
	      if (FRAME_VISIBLE_P (XFRAME (f)))
		prev = f;
	    }
	  else if (XFASTINT (minibuf) == 0)
	    {
	      FRAME_SAMPLE_VISIBILITY (XFRAME (f));
	      if (FRAME_VISIBLE_P (XFRAME (f))
		  || FRAME_ICONIFIED_P (XFRAME (f)))
		prev = f;
	    }
	  else
	    prev = f;
	}
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
       doc: /* Return the next frame in the frame list after FRAME.
It considers only frames on the same terminal as FRAME.
By default, skip minibuffer-only frames.
If omitted, FRAME defaults to the selected frame.
If optional argument MINIFRAME is nil, exclude minibuffer-only frames.
If MINIFRAME is a window, include only its own frame
and any frame now using that window as the minibuffer.
If MINIFRAME is `visible', include all visible frames.
If MINIFRAME is 0, include all visible and iconified frames.
Otherwise, include all frames.  */)
     (frame, miniframe)
     Lisp_Object frame, miniframe;
{
  if (NILP (frame))
    frame = selected_frame;
  
  CHECK_LIVE_FRAME (frame);
  return next_frame (frame, miniframe);
}

DEFUN ("previous-frame", Fprevious_frame, Sprevious_frame, 0, 2, 0,
       doc: /* Return the previous frame in the frame list before FRAME.
It considers only frames on the same terminal as FRAME.
By default, skip minibuffer-only frames.
If omitted, FRAME defaults to the selected frame.
If optional argument MINIFRAME is nil, exclude minibuffer-only frames.
If MINIFRAME is a window, include only its own frame
and any frame now using that window as the minibuffer.
If MINIFRAME is `visible', include all visible frames.
If MINIFRAME is 0, include all visible and iconified frames.
Otherwise, include all frames.  */)
     (frame, miniframe)
     Lisp_Object frame, miniframe;
{
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  return prev_frame (frame, miniframe);
}

/* Return 1 if it is ok to delete frame F;
   0 if all frames aside from F are invisible.
   (Exception: if F is the terminal frame, and we are using X, return 1.)  */

int
other_visible_frames (f)
     FRAME_PTR f;
{
  /* We know the selected frame is visible,
     so if F is some other frame, it can't be the sole visible one.  */
  if (f == SELECTED_FRAME ())
    {
      Lisp_Object frames;
      int count = 0;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCDR (frames))
	{
	  Lisp_Object this;

	  this = XCAR (frames);
	  /* Verify that the frame's window still exists
	     and we can still talk to it.  And note any recent change
	     in visibility.  */
#ifdef HAVE_WINDOW_SYSTEM
	  if (FRAME_WINDOW_P (XFRAME (this)))
	    {
	      x_sync (XFRAME (this));
	      FRAME_SAMPLE_VISIBILITY (XFRAME (this));
	    }
#endif

	  if (FRAME_VISIBLE_P (XFRAME (this))
	      || FRAME_ICONIFIED_P (XFRAME (this))
	      /* Allow deleting the terminal frame when at least
		 one X frame exists!  */
	      || (FRAME_WINDOW_P (XFRAME (this)) && !FRAME_WINDOW_P (f)))
	    count++;
	}
      return count > 1;
    }
  return 1;
}

DEFUN ("delete-frame", Fdelete_frame, Sdelete_frame, 0, 2, "",
       doc: /* Delete FRAME, permanently eliminating it from use.
If omitted, FRAME defaults to the selected frame.
A frame may not be deleted if its minibuffer is used by other frames.
Normally, you may not delete a frame if all other frames are invisible,
but if the second optional argument FORCE is non-nil, you may do so.

This function runs `delete-frame-hook' before actually deleting the
frame.  The hook is called with one argument FRAME.  */)
     (frame, force)
     Lisp_Object frame, force;
{
  struct frame *f;
  struct frame *sf = SELECTED_FRAME ();
  int minibuffer_selected;

  if (EQ (frame, Qnil))
    {
      f = sf;
      XSETFRAME (frame, f);
    }
  else
    {
      CHECK_FRAME (frame);
      f = XFRAME (frame);
    }

  if (! FRAME_LIVE_P (f))
    return Qnil;

  if (NILP (force) && !other_visible_frames (f)
#ifdef MAC_OS8
      /* Terminal frame deleted before any other visible frames are
	 created.  */
      && strcmp (XSTRING (f->name)->data, "F1") != 0
#endif
     )
    error ("Attempt to delete the sole visible or iconified frame");

#if 0
  /* This is a nice idea, but x_connection_closed needs to be able
     to delete the last frame, if it is gone.  */
  if (NILP (XCDR (Vframe_list)))
    error ("Attempt to delete the only frame");
#endif

  /* Does this frame have a minibuffer, and is it the surrogate
     minibuffer for any other frame?  */
  if (FRAME_HAS_MINIBUF_P (XFRAME (frame)))
    {
      Lisp_Object frames;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCDR (frames))
	{
	  Lisp_Object this;
	  this = XCAR (frames);

	  if (! EQ (this, frame)
	      && EQ (frame,
		     WINDOW_FRAME (XWINDOW
				   (FRAME_MINIBUF_WINDOW (XFRAME (this))))))
	    error ("Attempt to delete a surrogate minibuffer frame");
	}
    }

  /* Run `delete-frame-hook'.  */
  if (!NILP (Vrun_hooks))
    {
      Lisp_Object args[2];
      args[0] = intern ("delete-frame-hook");
      args[1] = frame;
      Frun_hook_with_args (2, args);
    }

  minibuffer_selected = EQ (minibuf_window, selected_window);

  /* Don't let the frame remain selected.  */
  if (f == sf)
    {
      Lisp_Object tail, frame1;

      /* Look for another visible frame on the same terminal.  */
      frame1 = next_frame (frame, Qvisible);

      /* If there is none, find *some* other frame.  */
      if (NILP (frame1) || EQ (frame1, frame))
	{
	  FOR_EACH_FRAME (tail, frame1)
	    {
	      if (! EQ (frame, frame1))
		break;
	    }
	}

      do_switch_frame (frame1, 0, 1);
      sf = SELECTED_FRAME ();
    }

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (f->minibuffer_window, minibuf_window))
    {
      Fset_window_buffer (sf->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = sf->minibuffer_window;

      /* If the dying minibuffer window was selected,
	 select the new one.  */
      if (minibuffer_selected)
	Fselect_window (minibuf_window);
    }

  /* Don't let echo_area_window to remain on a deleted frame.  */
  if (EQ (f->minibuffer_window, echo_area_window))
    echo_area_window = sf->minibuffer_window;

  /* Clear any X selections for this frame.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    x_clear_frame_selections (f);
#endif

  /* Free glyphs. 
     This function must be called before the window tree of the 
     frame is deleted because windows contain dynamically allocated
     memory. */
  free_glyphs (f);

  /* Mark all the windows that used to be on FRAME as deleted, and then
     remove the reference to them.  */
  delete_all_subwindows (XWINDOW (f->root_window));
  f->root_window = Qnil;

  Vframe_list = Fdelq (frame, Vframe_list);
  FRAME_SET_VISIBLE (f, 0);

  if (f->namebuf)
    xfree (f->namebuf);
  if (FRAME_INSERT_COST (f))
    xfree (FRAME_INSERT_COST (f));
  if (FRAME_DELETEN_COST (f))
    xfree (FRAME_DELETEN_COST (f));
  if (FRAME_INSERTN_COST (f))
    xfree (FRAME_INSERTN_COST (f));
  if (FRAME_DELETE_COST (f))
    xfree (FRAME_DELETE_COST (f));
  if (FRAME_MESSAGE_BUF (f))
    xfree (FRAME_MESSAGE_BUF (f));

  /* Since some events are handled at the interrupt level, we may get
     an event for f at any time; if we zero out the frame's display
     now, then we may trip up the event-handling code.  Instead, we'll
     promise that the display of the frame must be valid until we have
     called the window-system-dependent frame destruction routine.  */

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    x_destroy_window (f);
#endif

  f->output_data.nothing = 0;

  /* If we've deleted the last_nonminibuf_frame, then try to find
     another one.  */
  if (f == last_nonminibuf_frame)
    {
      Lisp_Object frames;

      last_nonminibuf_frame = 0;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCDR (frames))
	{
	  f = XFRAME (XCAR (frames));
	  if (!FRAME_MINIBUF_ONLY_P (f))
	    {
	      last_nonminibuf_frame = f;
	      break;
	    }
	}
    }

  /* If we've deleted this keyboard's default_minibuffer_frame, try to
     find another one.  Prefer minibuffer-only frames, but also notice
     frames with other windows.  */
  if (EQ (frame, FRAME_KBOARD (f)->Vdefault_minibuffer_frame))
    {
      Lisp_Object frames;

      /* The last frame we saw with a minibuffer, minibuffer-only or not.  */
      Lisp_Object frame_with_minibuf;
      /* Some frame we found on the same kboard, or nil if there are none.  */
      Lisp_Object frame_on_same_kboard;

      frame_on_same_kboard = Qnil;
      frame_with_minibuf = Qnil;

      for (frames = Vframe_list;
	   CONSP (frames);
	   frames = XCDR (frames))
	{
	  Lisp_Object this;
	  struct frame *f1;

	  this = XCAR (frames);
	  if (!FRAMEP (this))
	    abort ();
	  f1 = XFRAME (this);

	  /* Consider only frames on the same kboard
	     and only those with minibuffers.  */
	  if (FRAME_KBOARD (f) == FRAME_KBOARD (f1)
	      && FRAME_HAS_MINIBUF_P (f1))
	    {
	      frame_with_minibuf = this;
	      if (FRAME_MINIBUF_ONLY_P (f1))
		break;
	    }

	  if (FRAME_KBOARD (f) == FRAME_KBOARD (f1))
	    frame_on_same_kboard = this;
	}

      if (!NILP (frame_on_same_kboard))
	{
	  /* We know that there must be some frame with a minibuffer out
	     there.  If this were not true, all of the frames present
	     would have to be minibufferless, which implies that at some
	     point their minibuffer frames must have been deleted, but
	     that is prohibited at the top; you can't delete surrogate
	     minibuffer frames.  */
	  if (NILP (frame_with_minibuf))
	    abort ();

	  FRAME_KBOARD (f)->Vdefault_minibuffer_frame = frame_with_minibuf;
	}
      else
	/* No frames left on this kboard--say no minibuffer either.  */
	FRAME_KBOARD (f)->Vdefault_minibuffer_frame = Qnil;
    }

  /* Cause frame titles to update--necessary if we now have just one frame.  */
  update_mode_lines = 1;

  return Qnil;
}

/* Return mouse position in character cell units.  */

DEFUN ("mouse-position", Fmouse_position, Smouse_position, 0, 0, 0,
       doc: /* Return a list (FRAME X . Y) giving the current mouse frame and position.
The position is given in character cells, where (0, 0) is the
upper-left corner.
If Emacs is running on a mouseless terminal or hasn't been programmed
to read the mouse position, it returns the selected frame for FRAME
and nil for X and Y.
If `mouse-position-function' is non-nil, `mouse-position' calls it,
passing the normal return value to that function as an argument,
and returns whatever that function returns.  */)
     ()
{
  FRAME_PTR f;
  Lisp_Object lispy_dummy;
  enum scroll_bar_part party_dummy;
  Lisp_Object x, y, retval;
  int col, row;
  unsigned long long_dummy;
  struct gcpro gcpro1;

  f = SELECTED_FRAME ();
  x = y = Qnil;

#ifdef HAVE_MOUSE
  /* It's okay for the hook to refrain from storing anything.  */
  if (mouse_position_hook)
    (*mouse_position_hook) (&f, -1,
			    &lispy_dummy, &party_dummy,
			    &x, &y,
			    &long_dummy);
  if (! NILP (x))
    {
      col = XINT (x);
      row = XINT (y);
      pixel_to_glyph_coords (f, col, row, &col, &row, NULL, 1);
      XSETINT (x, col);
      XSETINT (y, row);
    }
#endif
  XSETFRAME (lispy_dummy, f);
  retval = Fcons (lispy_dummy, Fcons (x, y));
  GCPRO1 (retval);
  if (!NILP (Vmouse_position_function))
    retval = call1 (Vmouse_position_function, retval);
  RETURN_UNGCPRO (retval);
}

DEFUN ("mouse-pixel-position", Fmouse_pixel_position,
       Smouse_pixel_position, 0, 0, 0,
       doc: /* Return a list (FRAME X . Y) giving the current mouse frame and position.
The position is given in pixel units, where (0, 0) is the
upper-left corner.
If Emacs is running on a mouseless terminal or hasn't been programmed
to read the mouse position, it returns the selected frame for FRAME
and nil for X and Y.  */)
     ()
{
  FRAME_PTR f;
  Lisp_Object lispy_dummy;
  enum scroll_bar_part party_dummy;
  Lisp_Object x, y;
  unsigned long long_dummy;

  f = SELECTED_FRAME ();
  x = y = Qnil;

#ifdef HAVE_MOUSE
  /* It's okay for the hook to refrain from storing anything.  */
  if (mouse_position_hook)
    (*mouse_position_hook) (&f, -1,
			    &lispy_dummy, &party_dummy,
			    &x, &y,
			    &long_dummy);
#endif
  XSETFRAME (lispy_dummy, f);
  return Fcons (lispy_dummy, Fcons (x, y));
}

DEFUN ("set-mouse-position", Fset_mouse_position, Sset_mouse_position, 3, 3, 0,
       doc: /* Move the mouse pointer to the center of character cell (X,Y) in FRAME.
Coordinates are relative to the frame, not a window,
so the coordinates of the top left character in the frame
may be nonzero due to left-hand scroll bars or the menu bar.

This function is a no-op for an X frame that is not visible.
If you have just created a frame, you must wait for it to become visible
before calling this function on it, like this.
  (while (not (frame-visible-p frame)) (sleep-for .5))  */)
  (frame, x, y)
     Lisp_Object frame, x, y;
{
  CHECK_LIVE_FRAME (frame);
  CHECK_NUMBER (x);
  CHECK_NUMBER (y);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
    /* Warping the mouse will cause enternotify and focus events.  */
    x_set_mouse_position (XFRAME (frame), XINT (x), XINT (y));
#else
#if defined (MSDOS) && defined (HAVE_MOUSE)
  if (FRAME_MSDOS_P (XFRAME (frame)))
    {
      Fselect_frame (frame, Qnil);
      mouse_moveto (XINT (x), XINT (y));
    }
#endif
#endif

  return Qnil;
}

DEFUN ("set-mouse-pixel-position", Fset_mouse_pixel_position,
       Sset_mouse_pixel_position, 3, 3, 0,
       doc: /* Move the mouse pointer to pixel position (X,Y) in FRAME.
Note, this is a no-op for an X frame that is not visible.
If you have just created a frame, you must wait for it to become visible
before calling this function on it, like this.
  (while (not (frame-visible-p frame)) (sleep-for .5))  */)
  (frame, x, y)
     Lisp_Object frame, x, y;
{
  CHECK_LIVE_FRAME (frame);
  CHECK_NUMBER (x);
  CHECK_NUMBER (y);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
    /* Warping the mouse will cause enternotify and focus events.  */
    x_set_mouse_pixel_position (XFRAME (frame), XINT (x), XINT (y));
#else
#if defined (MSDOS) && defined (HAVE_MOUSE)
  if (FRAME_MSDOS_P (XFRAME (frame)))
    {
      Fselect_frame (frame, Qnil);
      mouse_moveto (XINT (x), XINT (y));
    }
#endif
#endif

  return Qnil;
}

static void make_frame_visible_1 P_ ((Lisp_Object));

DEFUN ("make-frame-visible", Fmake_frame_visible, Smake_frame_visible,
       0, 1, "",
       doc: /* Make the frame FRAME visible (assuming it is an X window).
If omitted, FRAME defaults to the currently selected frame.  */)
     (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
    {
      FRAME_SAMPLE_VISIBILITY (XFRAME (frame));
      x_make_frame_visible (XFRAME (frame));
    }
#endif

  make_frame_visible_1 (XFRAME (frame)->root_window);

  /* Make menu bar update for the Buffers and Frames menus.  */
  windows_or_buffers_changed++;

  return frame;
}

/* Update the display_time slot of the buffers shown in WINDOW
   and all its descendents.  */

static void
make_frame_visible_1 (window)
     Lisp_Object window;
{
  struct window *w;

  for (;!NILP (window); window = w->next)
    {
      w = XWINDOW (window);

      if (!NILP (w->buffer))
	XBUFFER (w->buffer)->display_time = Fcurrent_time ();

      if (!NILP (w->vchild))
	make_frame_visible_1 (w->vchild);
      if (!NILP (w->hchild))
	make_frame_visible_1 (w->hchild);
    }
}

DEFUN ("make-frame-invisible", Fmake_frame_invisible, Smake_frame_invisible,
       0, 2, "",
       doc: /* Make the frame FRAME invisible (assuming it is an X window).
If omitted, FRAME defaults to the currently selected frame.
Normally you may not make FRAME invisible if all other frames are invisible,
but if the second optional argument FORCE is non-nil, you may do so.  */)
  (frame, force)
     Lisp_Object frame, force;
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  if (NILP (force) && !other_visible_frames (XFRAME (frame)))
    error ("Attempt to make invisible the sole visible or iconified frame");

#if 0 /* This isn't logically necessary, and it can do GC.  */
  /* Don't let the frame remain selected.  */
  if (EQ (frame, selected_frame))
    do_switch_frame (next_frame (frame, Qt), 0, 0)
#endif

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (XFRAME (frame)->minibuffer_window, minibuf_window))
    {
      struct frame *sf = XFRAME (selected_frame);
      Fset_window_buffer (sf->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = sf->minibuffer_window;
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
    x_make_frame_invisible (XFRAME (frame));
#endif

  /* Make menu bar update for the Buffers and Frames menus.  */
  windows_or_buffers_changed++;

  return Qnil;
}

DEFUN ("iconify-frame", Ficonify_frame, Siconify_frame,
       0, 1, "",
       doc: /* Make the frame FRAME into an icon.
If omitted, FRAME defaults to the currently selected frame.  */)
  (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    frame = selected_frame;
  
  CHECK_LIVE_FRAME (frame);

#if 0 /* This isn't logically necessary, and it can do GC.  */
  /* Don't let the frame remain selected.  */
  if (EQ (frame, selected_frame))
    Fhandle_switch_frame (next_frame (frame, Qt), Qnil);
#endif

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (XFRAME (frame)->minibuffer_window, minibuf_window))
    {
      struct frame *sf = XFRAME (selected_frame);
      Fset_window_buffer (sf->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = sf->minibuffer_window;
    }

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (XFRAME (frame)))
      x_iconify_frame (XFRAME (frame));
#endif

  /* Make menu bar update for the Buffers and Frames menus.  */
  windows_or_buffers_changed++;

  return Qnil;
}

DEFUN ("frame-visible-p", Fframe_visible_p, Sframe_visible_p,
       1, 1, 0,
       doc: /* Return t if FRAME is now \"visible\" (actually in use for display).
A frame that is not \"visible\" is not updated and, if it works through
a window system, it may not show at all.
Return the symbol `icon' if frame is visible only as an icon.  */)
     (frame)
     Lisp_Object frame;
{
  CHECK_LIVE_FRAME (frame);

  FRAME_SAMPLE_VISIBILITY (XFRAME (frame));

  if (FRAME_VISIBLE_P (XFRAME (frame)))
    return Qt;
  if (FRAME_ICONIFIED_P (XFRAME (frame)))
    return Qicon;
  return Qnil;
}

DEFUN ("visible-frame-list", Fvisible_frame_list, Svisible_frame_list,
       0, 0, 0,
       doc: /* Return a list of all frames now \"visible\" (being updated).  */)
  ()
{
  Lisp_Object tail, frame;
  struct frame *f;
  Lisp_Object value;

  value = Qnil;
  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
	continue;
      f = XFRAME (frame);
      if (FRAME_VISIBLE_P (f))
	value = Fcons (frame, value);
    }
  return value;
}


DEFUN ("raise-frame", Fraise_frame, Sraise_frame, 0, 1, "",
       doc: /* Bring FRAME to the front, so it occludes any frames it overlaps.
If FRAME is invisible, make it visible.
If you don't specify a frame, the selected frame is used.
If Emacs is displaying on an ordinary terminal or some other device which
doesn't support multiple overlapping frames, this function does nothing.  */)
     (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  /* Do like the documentation says. */
  Fmake_frame_visible (frame);

  if (frame_raise_lower_hook)
    (*frame_raise_lower_hook) (XFRAME (frame), 1);

  return Qnil;
}

/* Should we have a corresponding function called Flower_Power?  */
DEFUN ("lower-frame", Flower_frame, Slower_frame, 0, 1, "",
       doc: /* Send FRAME to the back, so it is occluded by any frames that overlap it.
If you don't specify a frame, the selected frame is used.
If Emacs is displaying on an ordinary terminal or some other device which
doesn't support multiple overlapping frames, this function does nothing.  */)
     (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);
  
  if (frame_raise_lower_hook)
    (*frame_raise_lower_hook) (XFRAME (frame), 0);

  return Qnil;
}


DEFUN ("redirect-frame-focus", Fredirect_frame_focus, Sredirect_frame_focus,
       1, 2, 0,
       doc: /* Arrange for keystrokes typed at FRAME to be sent to FOCUS-FRAME.
In other words, switch-frame events caused by events in FRAME will
request a switch to FOCUS-FRAME, and `last-event-frame' will be
FOCUS-FRAME after reading an event typed at FRAME.

If FOCUS-FRAME is omitted or nil, any existing redirection is
cancelled, and the frame again receives its own keystrokes.

Focus redirection is useful for temporarily redirecting keystrokes to
a surrogate minibuffer frame when a frame doesn't have its own
minibuffer window.

A frame's focus redirection can be changed by select-frame.  If frame
FOO is selected, and then a different frame BAR is selected, any
frames redirecting their focus to FOO are shifted to redirect their
focus to BAR.  This allows focus redirection to work properly when the
user switches from one frame to another using `select-window'.

This means that a frame whose focus is redirected to itself is treated
differently from a frame whose focus is redirected to nil; the former
is affected by select-frame, while the latter is not.

The redirection lasts until `redirect-frame-focus' is called to change it.  */)
     (frame, focus_frame)
     Lisp_Object frame, focus_frame;
{
  /* Note that we don't check for a live frame here.  It's reasonable
     to redirect the focus of a frame you're about to delete, if you
     know what other frame should receive those keystrokes.  */
  CHECK_FRAME (frame);

  if (! NILP (focus_frame))
    CHECK_LIVE_FRAME (focus_frame);

  XFRAME (frame)->focus_frame = focus_frame;

  if (frame_rehighlight_hook)
    (*frame_rehighlight_hook) (XFRAME (frame));
  
  return Qnil;
}


DEFUN ("frame-focus", Fframe_focus, Sframe_focus, 1, 1, 0,
       doc: /* Return the frame to which FRAME's keystrokes are currently being sent.
This returns nil if FRAME's focus is not redirected.
See `redirect-frame-focus'.  */)
     (frame)
     Lisp_Object frame;
{
  CHECK_LIVE_FRAME (frame);

  return FRAME_FOCUS_FRAME (XFRAME (frame));
}



/* Return the value of frame parameter PROP in frame FRAME.  */

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

/* Return the buffer-predicate of the selected frame.  */

Lisp_Object
frame_buffer_predicate (frame)
     Lisp_Object frame;
{
  return XFRAME (frame)->buffer_predicate;
}

/* Return the buffer-list of the selected frame.  */

Lisp_Object
frame_buffer_list (frame)
     Lisp_Object frame;
{
  return XFRAME (frame)->buffer_list;
}

/* Set the buffer-list of the selected frame.  */

void
set_frame_buffer_list (frame, list)
     Lisp_Object frame, list;
{
  XFRAME (frame)->buffer_list = list;
}

/* Discard BUFFER from the buffer-list of each frame.  */

void
frames_discard_buffer (buffer)
     Lisp_Object buffer;
{
  Lisp_Object frame, tail;

  FOR_EACH_FRAME (tail, frame)
    {
      XFRAME (frame)->buffer_list
	= Fdelq (buffer, XFRAME (frame)->buffer_list);
    }
}

/* Move BUFFER to the end of the buffer-list of each frame.  */

void
frames_bury_buffer (buffer)
     Lisp_Object buffer;
{
  Lisp_Object frame, tail;

  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      Lisp_Object found;
      
      found = Fmemq (buffer, f->buffer_list);
      if (!NILP (found))
	f->buffer_list = nconc2 (Fdelq (buffer, f->buffer_list),
				 Fcons (buffer, Qnil));
    }
}

/* Modify the alist in *ALISTPTR to associate PROP with VAL.
   If the alist already has an element for PROP, we change it.  */

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

static int
frame_name_fnn_p (str, len)
     char *str;
     int len;
{
  if (len > 1 && str[0] == 'F')
    {
      char *end_ptr;

      strtol (str + 1, &end_ptr, 10);

      if (end_ptr == str + len)
	return 1;
    }
  return 0;
}

/* Set the name of the terminal frame.  Also used by MSDOS frames.
   Modeled after x_set_name which is used for WINDOW frames.  */

void
set_term_frame_name (f, name)
     struct frame *f;
     Lisp_Object name;
{
  f->explicit_name = ! NILP (name);

  /* If NAME is nil, set the name to F<num>.  */
  if (NILP (name))
    {
      char namebuf[20];

      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (frame_name_fnn_p (XSTRING (f->name)->data,
			    STRING_BYTES (XSTRING (f->name))))
	return;

      terminal_frame_count++;
      sprintf (namebuf, "F%d", terminal_frame_count);
      name = build_string (namebuf);
    }
  else
    {
      CHECK_STRING (name);

      /* Don't change the name if it's already NAME.  */
      if (! NILP (Fstring_equal (name, f->name)))
	return;

      /* Don't allow the user to set the frame name to F<num>, so it
	 doesn't clash with the names we generate for terminal frames.  */
      if (frame_name_fnn_p (XSTRING (name)->data, STRING_BYTES (XSTRING (name))))
	error ("Frame names of the form F<num> are usurped by Emacs");
    }

  f->name = name;
  update_mode_lines = 1;
}

void
store_frame_param (f, prop, val)
     struct frame *f;
     Lisp_Object prop, val;
{
  register Lisp_Object old_alist_elt;

  /* The buffer-alist parameter is stored in a special place and is
     not in the alist.  */
  if (EQ (prop, Qbuffer_list))
    {
      f->buffer_list = val;
      return;
    }

  /* If PROP is a symbol which is supposed to have frame-local values,
     and it is set up based on this frame, switch to the global
     binding.  That way, we can create or alter the frame-local binding
     without messing up the symbol's status.  */
  if (SYMBOLP (prop))
    {
      Lisp_Object valcontents;
      valcontents = SYMBOL_VALUE (prop);
      if ((BUFFER_LOCAL_VALUEP (valcontents)
  	   || SOME_BUFFER_LOCAL_VALUEP (valcontents))
	  && XBUFFER_LOCAL_VALUE (valcontents)->check_frame
 	  && XFRAME (XBUFFER_LOCAL_VALUE (valcontents)->frame) == f)
 	swap_in_global_binding (prop);
    }

#ifndef WINDOWSNT
  /* The tty color mode needs to be set before the frame's parameter
     alist is updated with the new value, because set_tty_color_mode
     wants to look at the old mode.  */
  if (FRAME_TERMCAP_P (f) && EQ (prop, Qtty_color_mode))
    set_tty_color_mode (f, val);
#endif

  /* Update the frame parameter alist.  */
  old_alist_elt = Fassq (prop, f->param_alist);
  if (EQ (old_alist_elt, Qnil))
    f->param_alist = Fcons (Fcons (prop, val), f->param_alist);
  else
    Fsetcdr (old_alist_elt, val);

  /* Update some other special parameters in their special places
     in addition to the alist.  */
  
  if (EQ (prop, Qbuffer_predicate))
    f->buffer_predicate = val;

  if (! FRAME_WINDOW_P (f))
    {
      if (EQ (prop, Qmenu_bar_lines))
	set_menu_bar_lines (f, val, make_number (FRAME_MENU_BAR_LINES (f)));
      else if (EQ (prop, Qname))
	set_term_frame_name (f, val);
    }

  if (EQ (prop, Qminibuffer) && WINDOWP (val))
    {
      if (! MINI_WINDOW_P (XWINDOW (val)))
	error ("Surrogate minibuffer windows must be minibuffer windows.");

      if ((FRAME_HAS_MINIBUF_P (f) || FRAME_MINIBUF_ONLY_P (f))
	  && !EQ (val, f->minibuffer_window))
	error ("Can't change the surrogate minibuffer of a frame with its own minibuffer");

      /* Install the chosen minibuffer window, with proper buffer.  */
      f->minibuffer_window = val;
    }
}

DEFUN ("frame-parameters", Fframe_parameters, Sframe_parameters, 0, 1, 0,
       doc: /* Return the parameters-alist of frame FRAME.
It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.
The meaningful PARMs depend on the kind of frame.
If FRAME is omitted, return information on the currently selected frame.  */)
     (frame)
     Lisp_Object frame;
{
  Lisp_Object alist;
  FRAME_PTR f;
  int height, width;
  struct gcpro gcpro1;

  if (NILP (frame))
    frame = selected_frame;

  CHECK_FRAME (frame);
  f = XFRAME (frame);

  if (!FRAME_LIVE_P (f))
    return Qnil;

  alist = Fcopy_alist (f->param_alist);
  GCPRO1 (alist);
  
  if (!FRAME_WINDOW_P (f))
    {
      int fg = FRAME_FOREGROUND_PIXEL (f);
      int bg = FRAME_BACKGROUND_PIXEL (f);
      Lisp_Object elt;

      /* If the frame's parameter alist says the colors are
	 unspecified and reversed, take the frame's background pixel
	 for foreground and vice versa.  */
      elt = Fassq (Qforeground_color, alist);
      if (!NILP (elt) && CONSP (elt) && STRINGP (XCDR (elt)))
	{
	  if (strncmp (XSTRING (XCDR (elt))->data,
		       unspecified_bg,
		       XSTRING (XCDR (elt))->size) == 0)
	    store_in_alist (&alist, Qforeground_color, tty_color_name (f, bg));
	  else if (strncmp (XSTRING (XCDR (elt))->data,
			    unspecified_fg,
			    XSTRING (XCDR (elt))->size) == 0)
	    store_in_alist (&alist, Qforeground_color, tty_color_name (f, fg));
	}
      else
	store_in_alist (&alist, Qforeground_color, tty_color_name (f, fg));
      elt = Fassq (Qbackground_color, alist);
      if (!NILP (elt) && CONSP (elt) && STRINGP (XCDR (elt)))
	{
	  if (strncmp (XSTRING (XCDR (elt))->data,
		       unspecified_fg,
		       XSTRING (XCDR (elt))->size) == 0)
	    store_in_alist (&alist, Qbackground_color, tty_color_name (f, fg));
	  else if (strncmp (XSTRING (XCDR (elt))->data,
			    unspecified_bg,
			    XSTRING (XCDR (elt))->size) == 0)
	    store_in_alist (&alist, Qbackground_color, tty_color_name (f, bg));
	}
      else
	store_in_alist (&alist, Qbackground_color, tty_color_name (f, bg));
      store_in_alist (&alist, intern ("font"),
		      build_string (FRAME_MSDOS_P (f)
				    ? "ms-dos"
				    : FRAME_W32_P (f) ? "w32term"
				    :"tty"));
    }
  store_in_alist (&alist, Qname, f->name);
  height = (FRAME_NEW_HEIGHT (f) ? FRAME_NEW_HEIGHT (f) : FRAME_HEIGHT (f));
  store_in_alist (&alist, Qheight, make_number (height));
  width = (FRAME_NEW_WIDTH (f) ? FRAME_NEW_WIDTH (f) : FRAME_WIDTH (f));
  store_in_alist (&alist, Qwidth, make_number (width));
  store_in_alist (&alist, Qmodeline, (FRAME_WANTS_MODELINE_P (f) ? Qt : Qnil));
  store_in_alist (&alist, Qminibuffer,
		  (! FRAME_HAS_MINIBUF_P (f) ? Qnil
		   : FRAME_MINIBUF_ONLY_P (f) ? Qonly
		   : FRAME_MINIBUF_WINDOW (f)));
  store_in_alist (&alist, Qunsplittable, (FRAME_NO_SPLIT_P (f) ? Qt : Qnil));
  store_in_alist (&alist, Qbuffer_list, frame_buffer_list (frame));

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    x_report_frame_params (f, &alist);
  else
#endif
    {
      /* This ought to be correct in f->param_alist for an X frame.  */
      Lisp_Object lines;
      XSETFASTINT (lines, FRAME_MENU_BAR_LINES (f));
      store_in_alist (&alist, Qmenu_bar_lines, lines);
    }

  UNGCPRO;
  return alist;
}


DEFUN ("frame-parameter", Fframe_parameter, Sframe_parameter, 2, 2, 0,
       doc: /* Return FRAME's value for parameter PARAMETER.
If FRAME is nil, describe the currently selected frame.  */)
     (frame, parameter)
     Lisp_Object frame, parameter;
{
  struct frame *f;
  Lisp_Object value;

  if (NILP (frame))
    frame = selected_frame;
  else
    CHECK_FRAME (frame);
  CHECK_SYMBOL (parameter);
  
  f = XFRAME (frame);
  value = Qnil;
  
  if (FRAME_LIVE_P (f))
    {
      /* Avoid consing in frequent cases.  */
      if (EQ (parameter, Qname))
	value = f->name;
#ifdef HAVE_X_WINDOWS
      else if (EQ (parameter, Qdisplay) && FRAME_X_P (f))
	value = XCAR (FRAME_X_DISPLAY_INFO (f)->name_list_element);
#endif /* HAVE_X_WINDOWS */
      else if (EQ (parameter, Qbackground_color)
	       || EQ (parameter, Qforeground_color))
	{
	  value = Fassq (parameter, f->param_alist);
	  if (CONSP (value))
	    {
	      value = XCDR (value);
	      /* Fframe_parameters puts the actual fg/bg color names,
		 even if f->param_alist says otherwise.  This is
		 important when param_alist's notion of colors is
		 "unspecified".  We need to do the same here.  */
	      if (STRINGP (value) && !FRAME_WINDOW_P (f))
		{
		  char *color_name;
		  EMACS_INT csz;

		  if (EQ (parameter, Qbackground_color))
		    {
		      color_name = XSTRING (value)->data;
		      csz = XSTRING (value)->size;
		      if (strncmp (color_name, unspecified_bg, csz) == 0)
			value = tty_color_name (f, FRAME_BACKGROUND_PIXEL (f));
		      else if (strncmp (color_name, unspecified_fg, csz) == 0)
			value = tty_color_name (f, FRAME_FOREGROUND_PIXEL (f));
		    }
		  else if (EQ (parameter, Qforeground_color))
		    {
		      color_name = XSTRING (value)->data;
		      csz = XSTRING (value)->size;
		      if (strncmp (color_name, unspecified_fg, csz) == 0)
			value = tty_color_name (f, FRAME_FOREGROUND_PIXEL (f));
		      else if (strncmp (color_name, unspecified_bg, csz) == 0)
			value = tty_color_name (f, FRAME_BACKGROUND_PIXEL (f));
		    }
		}
	    }
	  else
	    value = Fcdr (Fassq (parameter, Fframe_parameters (frame)));
	}
      else if (EQ (parameter, Qdisplay_type)
	       || EQ (parameter, Qbackground_mode))
	value = Fcdr (Fassq (parameter, f->param_alist));
      else
	value = Fcdr (Fassq (parameter, Fframe_parameters (frame)));
    }
  
  return value;
}


DEFUN ("modify-frame-parameters", Fmodify_frame_parameters, 
       Smodify_frame_parameters, 2, 2, 0,
       doc: /* Modify the parameters of frame FRAME according to ALIST.
If FRAME is nil, it defaults to the selected frame.
ALIST is an alist of parameters to change and their new values.
Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.
The meaningful PARMs depend on the kind of frame.
Undefined PARMs are ignored, but stored in the frame's parameter list
so that `frame-parameters' will return them.

The value of frame parameter FOO can also be accessed
as a frame-local binding for the variable FOO, if you have
enabled such bindings for that variable with `make-variable-frame-local'.  */)
     (frame, alist)
     Lisp_Object frame, alist;
{
  FRAME_PTR f;
  register Lisp_Object tail, prop, val;
  int count = BINDING_STACK_SIZE ();

  /* Bind this to t to inhibit initialization of the default face from
     X resources in face-set-after-frame-default.  If we don't inhibit
     this, modifying the `font' frame parameter, for example, while
     there is a `default.attributeFont' X resource, won't work,
     because `default's font is reset to the value of the X resource
     and that resets the `font' frame parameter.  */
  specbind (Qinhibit_default_face_x_resources, Qt);

  if (EQ (frame, Qnil))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    x_set_frame_parameters (f, alist);
  else
#endif
#ifdef MSDOS
  if (FRAME_MSDOS_P (f))
    IT_set_frame_parameters (f, alist);
  else
#endif

    {
      int length = XINT (Flength (alist));
      int i;
      Lisp_Object *parms
	= (Lisp_Object *) alloca (length * sizeof (Lisp_Object));
      Lisp_Object *values
	= (Lisp_Object *) alloca (length * sizeof (Lisp_Object));

      /* Extract parm names and values into those vectors.  */

      i = 0;
      for (tail = alist; CONSP (tail); tail = Fcdr (tail))
	{
	  Lisp_Object elt;

	  elt = Fcar (tail);
	  parms[i] = Fcar (elt);
	  values[i] = Fcdr (elt);
	  i++;
	}

      /* Now process them in reverse of specified order.  */
      for (i--; i >= 0; i--)
	{
	  prop = parms[i];
	  val = values[i];
	  store_frame_param (f, prop, val);
	}
    }

  return unbind_to (count, Qnil);
}

DEFUN ("frame-char-height", Fframe_char_height, Sframe_char_height,
       0, 1, 0,
       doc: /* Height in pixels of a line in the font in frame FRAME.
If FRAME is omitted, the selected frame is used.
For a terminal frame, the value is always 1.  */)
  (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    return make_number (x_char_height (f));
  else
#endif
    return make_number (1);
}


DEFUN ("frame-char-width", Fframe_char_width, Sframe_char_width,
       0, 1, 0,
       doc: /* Width in pixels of characters in the font in frame FRAME.
If FRAME is omitted, the selected frame is used.
The width is the same for all characters, because
currently Emacs supports only fixed-width fonts.
For a terminal screen, the value is always 1.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    return make_number (x_char_width (f));
  else
#endif
    return make_number (1);
}

DEFUN ("frame-pixel-height", Fframe_pixel_height, 
       Sframe_pixel_height, 0, 1, 0,
       doc: /* Return a FRAME's height in pixels.
This counts only the height available for text lines,
not menu bars on window-system Emacs frames.
For a terminal frame, the result really gives the height in characters.
If FRAME is omitted, the selected frame is used.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    return make_number (x_pixel_height (f));
  else
#endif
    return make_number (FRAME_HEIGHT (f));
}

DEFUN ("frame-pixel-width", Fframe_pixel_width, 
       Sframe_pixel_width, 0, 1, 0,
       doc: /* Return FRAME's width in pixels.
For a terminal frame, the result really gives the width in characters.
If FRAME is omitted, the selected frame is used.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_FRAME (frame);
  f = XFRAME (frame);

#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    return make_number (x_pixel_width (f));
  else
#endif
    return make_number (FRAME_WIDTH (f));
}

DEFUN ("set-frame-height", Fset_frame_height, Sset_frame_height, 2, 3, 0,
       doc: /* Specify that the frame FRAME has LINES lines.
Optional third arg non-nil means that redisplay should use LINES lines
but that the idea of the actual height of the frame should not be changed.  */)
     (frame, lines, pretend)
     Lisp_Object frame, lines, pretend;
{
  register struct frame *f;

  CHECK_NUMBER (lines);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      if (XINT (lines) != f->height)
	x_set_window_size (f, 1, f->width, XINT (lines));
      do_pending_window_change (0);
    }
  else
#endif
    change_frame_size (f, XINT (lines), 0, !NILP (pretend), 0, 0);
  return Qnil;
}

DEFUN ("set-frame-width", Fset_frame_width, Sset_frame_width, 2, 3, 0,
       doc: /* Specify that the frame FRAME has COLS columns.
Optional third arg non-nil means that redisplay should use COLS columns
but that the idea of the actual width of the frame should not be changed.  */)
     (frame, cols, pretend)
     Lisp_Object frame, cols, pretend;
{
  register struct frame *f;
  CHECK_NUMBER (cols);
  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      if (XINT (cols) != f->width)
	x_set_window_size (f, 1, XINT (cols), f->height);
      do_pending_window_change (0);
    }
  else
#endif
    change_frame_size (f, 0, XINT (cols), !NILP (pretend), 0, 0);
  return Qnil;
}

DEFUN ("set-frame-size", Fset_frame_size, Sset_frame_size, 3, 3, 0,
       doc: /* Sets size of FRAME to COLS by ROWS, measured in characters.  */)
     (frame, cols, rows)
     Lisp_Object frame, cols, rows;
{
  register struct frame *f;

  CHECK_LIVE_FRAME (frame);
  CHECK_NUMBER (cols);
  CHECK_NUMBER (rows);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    {
      if (XINT (rows) != f->height || XINT (cols) != f->width
	  || FRAME_NEW_HEIGHT (f) || FRAME_NEW_WIDTH (f))
	x_set_window_size (f, 1, XINT (cols), XINT (rows));
      do_pending_window_change (0);
    }
  else
#endif
    change_frame_size (f, XINT (rows), XINT (cols), 0, 0, 0);

  return Qnil;
}

DEFUN ("set-frame-position", Fset_frame_position, 
       Sset_frame_position, 3, 3, 0,
       doc: /* Sets position of FRAME in pixels to XOFFSET by YOFFSET.
This is actually the position of the upper left corner of the frame.
Negative values for XOFFSET or YOFFSET are interpreted relative to
the rightmost or bottommost possible position (that stays within the screen).  */)
     (frame, xoffset, yoffset)
     Lisp_Object frame, xoffset, yoffset;
{
  register struct frame *f;

  CHECK_LIVE_FRAME (frame);
  CHECK_NUMBER (xoffset);
  CHECK_NUMBER (yoffset);
  f = XFRAME (frame);

  /* I think this should be done with a hook.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f))
    x_set_offset (f, XINT (xoffset), XINT (yoffset), 1);
#endif

  return Qt;
}


void
syms_of_frame ()
{
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
  Qmenu_bar_lines = intern ("menu-bar-lines");
  staticpro (&Qmenu_bar_lines);
  Qtool_bar_lines = intern ("tool-bar-lines");
  staticpro (&Qtool_bar_lines);
  Qwidth = intern ("width");
  staticpro (&Qwidth);
  Qx = intern ("x");
  staticpro (&Qx);
  Qw32 = intern ("w32");
  staticpro (&Qw32);
  Qpc = intern ("pc");
  staticpro (&Qpc);
  Qmac = intern ("mac");
  staticpro (&Qmac);
  Qvisible = intern ("visible");
  staticpro (&Qvisible);
  Qbuffer_predicate = intern ("buffer-predicate");
  staticpro (&Qbuffer_predicate);
  Qbuffer_list = intern ("buffer-list");
  staticpro (&Qbuffer_list);
  Qtitle = intern ("title");
  staticpro (&Qtitle);
  Qdisplay_type = intern ("display-type");
  staticpro (&Qdisplay_type);
  Qbackground_mode = intern ("background-mode");
  staticpro (&Qbackground_mode);
  Qleft_fringe = intern ("left-fringe");
  staticpro (&Qleft_fringe);
  Qright_fringe = intern ("right-fringe");
  staticpro (&Qright_fringe);
  Qtty_color_mode = intern ("tty-color-mode");
  staticpro (&Qtty_color_mode);

  DEFVAR_LISP ("default-frame-alist", &Vdefault_frame_alist,
	       doc: /* Alist of default values for frame creation.
These may be set in your init file, like this:
  (setq default-frame-alist '((width . 80) (height . 55) (menu-bar-lines . 1))
These override values given in window system configuration data,
 including X Windows' defaults database.
For values specific to the first Emacs frame, see `initial-frame-alist'.
For values specific to the separate minibuffer frame, see
 `minibuffer-frame-alist'.
The `menu-bar-lines' element of the list controls whether new frames
 have menu bars; `menu-bar-mode' works by altering this element.
Setting this variable does not affect existing frames, only new ones.  */);
  Vdefault_frame_alist = Qnil;

  Qinhibit_default_face_x_resources
    = intern ("inhibit-default-face-x-resources");
  staticpro (&Qinhibit_default_face_x_resources);

  DEFVAR_LISP ("terminal-frame", &Vterminal_frame,
	       doc: /* The initial frame-object, which represents Emacs's stdout.  */);

  DEFVAR_LISP ("emacs-iconified", &Vemacs_iconified,
	       doc: /* Non-nil if all of emacs is iconified and frame updates are not needed.  */);
  Vemacs_iconified = Qnil;

  DEFVAR_LISP ("mouse-position-function", &Vmouse_position_function,
	       doc: /* If non-nil, function to transform normal value of `mouse-position'.
`mouse-position' calls this function, passing its usual return value as
argument, and returns whatever this function returns.
This abnormal hook exists for the benefit of packages like `xt-mouse.el'
which need to do mouse handling at the Lisp level.  */);
  Vmouse_position_function = Qnil;

  DEFVAR_LISP ("mouse-highlight", &Vmouse_highlight,
	       doc: /* If non-nil, clickable text is highlighted when mouse is over it.  
If the value is an integer, highlighting is only shown after moving the
mouse, while keyboard input turns off the highlight even when the mouse
is over the clickable text.  However, the mouse shape still indicates
when the mouse is over clickable text.  */);
  Vmouse_highlight = Qt;

  DEFVAR_KBOARD ("default-minibuffer-frame", Vdefault_minibuffer_frame,
		 doc: /* Minibufferless frames use this frame's minibuffer.

Emacs cannot create minibufferless frames unless this is set to an
appropriate surrogate.

Emacs consults this variable only when creating minibufferless
frames; once the frame is created, it sticks with its assigned
minibuffer, no matter what this variable is set to.  This means that
this variable doesn't necessarily say anything meaningful about the
current set of frames, or where the minibuffer is currently being
displayed.

This variable is local to the current terminal and cannot be buffer-local.  */);

  staticpro (&Vframe_list);

  defsubr (&Sactive_minibuffer_window);
  defsubr (&Sframep);
  defsubr (&Sframe_live_p);
  defsubr (&Smake_terminal_frame);
  defsubr (&Shandle_switch_frame);
  defsubr (&Signore_event);
  defsubr (&Sselect_frame);
  defsubr (&Sselected_frame);
  defsubr (&Swindow_frame);
  defsubr (&Sframe_root_window);
  defsubr (&Sframe_first_window);
  defsubr (&Sframe_selected_window);
  defsubr (&Sset_frame_selected_window);
  defsubr (&Sframe_list);
  defsubr (&Snext_frame);
  defsubr (&Sprevious_frame);
  defsubr (&Sdelete_frame);
  defsubr (&Smouse_position);
  defsubr (&Smouse_pixel_position);
  defsubr (&Sset_mouse_position);
  defsubr (&Sset_mouse_pixel_position);
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
  defsubr (&Sframe_parameter);
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
