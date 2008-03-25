/* Generic frame functions.
   Copyright (C) 1993, 1994, 1995, 1997, 1999, 2000, 2001, 2002, 2003,
                 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include <config.h>

#include <stdio.h>
#include "lisp.h"
#include "character.h"
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
#include "blockinput.h"
#include "termchar.h"
#include "termhooks.h"
#include "dispextern.h"
#include "window.h"
#ifdef MSDOS
#include "msdos.h"
#include "dosfns.h"
#endif


#ifdef HAVE_WINDOW_SYSTEM

#ifdef USE_FONT_BACKEND
#include "font.h"
#endif	/* USE_FONT_BACKEND */

/* The name we're using in resource queries.  Most often "emacs".  */

Lisp_Object Vx_resource_name;

/* The application class we're using in resource queries.
   Normally "Emacs".  */

Lisp_Object Vx_resource_class;

#endif

Lisp_Object Qframep, Qframe_live_p;
Lisp_Object Qicon, Qmodeline;
Lisp_Object Qonly;
Lisp_Object Qx, Qw32, Qmac, Qpc;
Lisp_Object Qvisible;
Lisp_Object Qdisplay_type;
Lisp_Object Qbackground_mode;
Lisp_Object Qnoelisp;

Lisp_Object Qx_frame_parameter;
Lisp_Object Qx_resource_name;
Lisp_Object Qterminal;
Lisp_Object Qterminal_live_p;

/* Frame parameters (set or reported).  */

Lisp_Object Qauto_raise, Qauto_lower;
Lisp_Object Qborder_color, Qborder_width;
Lisp_Object Qcursor_color, Qcursor_type;
Lisp_Object Qgeometry;  /* Not used */
Lisp_Object Qheight, Qwidth;
Lisp_Object Qleft, Qright;
Lisp_Object Qicon_left, Qicon_top, Qicon_type, Qicon_name;
Lisp_Object Qinternal_border_width;
Lisp_Object Qmouse_color;
Lisp_Object Qminibuffer;
Lisp_Object Qscroll_bar_width, Qvertical_scroll_bars;
Lisp_Object Qvisibility;
Lisp_Object Qscroll_bar_foreground, Qscroll_bar_background;
Lisp_Object Qscreen_gamma;
Lisp_Object Qline_spacing;
Lisp_Object Quser_position, Quser_size;
Lisp_Object Qwait_for_wm;
Lisp_Object Qwindow_id;
#ifdef HAVE_X_WINDOWS
Lisp_Object Qouter_window_id;
#endif
Lisp_Object Qparent_id;
Lisp_Object Qtitle, Qname;
Lisp_Object Qexplicit_name;
Lisp_Object Qunsplittable;
Lisp_Object Qmenu_bar_lines, Qtool_bar_lines;
Lisp_Object Qleft_fringe, Qright_fringe;
Lisp_Object Qbuffer_predicate, Qbuffer_list, Qburied_buffer_list;
Lisp_Object Qtty_color_mode;
Lisp_Object Qtty, Qtty_type;

Lisp_Object Qfullscreen, Qfullwidth, Qfullheight, Qfullboth;
#ifdef USE_FONT_BACKEND
Lisp_Object Qfont_backend;
#endif	/* USE_FONT_BACKEND */

Lisp_Object Qinhibit_face_set_after_frame_default;
Lisp_Object Qface_set_after_frame_default;

Lisp_Object Vterminal_frame;
Lisp_Object Vdefault_frame_alist;
Lisp_Object Vdefault_frame_scroll_bars;
Lisp_Object Vmouse_position_function;
Lisp_Object Vmouse_highlight;
Lisp_Object Vdelete_frame_functions;

int focus_follows_mouse;

static void
set_menu_bar_lines_1 (window, n)
     Lisp_Object window;
     int n;
{
  struct window *w = XWINDOW (window);

  XSETFASTINT (w->last_modified, 0);
  XSETFASTINT (w->top_line, XFASTINT (w->top_line) + n);
  XSETFASTINT (w->total_lines, XFASTINT (w->total_lines) - n);

  if (INTEGERP (w->orig_top_line))
    XSETFASTINT (w->orig_top_line, XFASTINT (w->orig_top_line) + n);
  if (INTEGERP (w->orig_total_lines))
    XSETFASTINT (w->orig_total_lines, XFASTINT (w->orig_total_lines) - n);

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
    case output_initial: /* The initial frame is like a termcap frame. */
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
frame, the return value indicates what sort of terminal device it is
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

DEFUN ("window-system", Fwindow_system, Swindow_system, 0, 1, 0,
       doc: /* The name of the window system that FRAME is displaying through.
The value is a symbol---for instance, 'x' for X windows.
The value is nil if Emacs is using a text-only terminal.

FRAME defaults to the currently selected frame.  */)
  (frame)
     Lisp_Object frame;
{
  Lisp_Object type;
  if (NILP (frame))
    frame = selected_frame;

  type = Fframep (frame);

  if (NILP (type))
    wrong_type_argument (Qframep, frame);

  if (EQ (type, Qt))
    return Qnil;
  else
    return type;
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
  f->buried_buffer_list = Qnil;
  f->namebuf = 0;
  f->title = Qnil;
  f->menu_bar_window = Qnil;
  f->tool_bar_window = Qnil;
  f->tool_bar_items = Qnil;
  f->desired_tool_bar_string = f->current_tool_bar_string = Qnil;
  f->n_tool_bar_items = 0;
  f->left_fringe_width = f->right_fringe_width = 0;
  f->fringe_cols = 0;
  f->scroll_bar_actual_width = 0;
  f->border_width = 0;
  f->internal_border_width = 0;
  f->column_width = 1;  /* !FRAME_WINDOW_P value */
  f->line_height = 1;  /* !FRAME_WINDOW_P value */
  f->x_pixels_diff = f->y_pixels_diff = 0;
#ifdef HAVE_WINDOW_SYSTEM
  f->want_fullscreen = FULLSCREEN_NONE;
#endif
  f->size_hint_flags = 0;
  f->win_gravity = 0;
#ifdef USE_FONT_BACKEND
  f->font_driver_list = NULL;
  f->font_data_list = NULL;
#endif	/* USE_FONT_BACKEND */

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

  SET_FRAME_COLS (f, 10);
  FRAME_LINES (f) = 10;

  XSETFASTINT (XWINDOW (root_window)->total_cols, 10);
  XSETFASTINT (XWINDOW (root_window)->total_lines, (mini_p ? 9 : 10));

  if (mini_p)
    {
      XSETFASTINT (XWINDOW (mini_window)->total_cols, 10);
      XSETFASTINT (XWINDOW (mini_window)->top_line, 9);
      XSETFASTINT (XWINDOW (mini_window)->total_lines, 1);
    }

  /* Choose a buffer for the frame's root window.  */
  {
    Lisp_Object buf;

    XWINDOW (root_window)->buffer = Qt;
    buf = Fcurrent_buffer ();
    /* If buf is a 'hidden' buffer (i.e. one whose name starts with
       a space), try to find another one.  */
    if (SREF (Fbuffer_name (buf), 0) == ' ')
      buf = Fother_buffer (buf, Qnil, Qnil);

    /* Use set_window_buffer, not Fset_window_buffer, and don't let
       hooks be run by it.  The reason is that the whole frame/window
       arrangement is not yet fully intialized at this point.  Windows
       don't have the right size, glyph matrices aren't initialized
       etc.  Running Lisp functions at this point surely ends in a
       SEGV.  */
    set_window_buffer (root_window, buf, 0, 0);
    f->buffer_list = Fcons (buf, Qnil);
  }

  if (mini_p)
    {
      XWINDOW (mini_window)->buffer = Qt;
      set_window_buffer (mini_window,
			 (NILP (Vminibuffer_list)
			  ? get_minibuffer (0)
			  : Fcar (Vminibuffer_list)),
			 0, 0);
    }

  f->root_window = root_window;
  f->selected_window = root_window;
  /* Make sure this window seems more recently used than
     a newly-created, never-selected window.  */
  ++window_select_count;
  XSETFASTINT (XWINDOW (f->selected_window)->use_time, window_select_count);

  f->default_face_done_p = 0;

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
      && FRAME_KBOARD (XFRAME (XWINDOW (mini_window)->frame)) != kb)
    error ("Frame and minibuffer must be on the same terminal");
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
			 : Fcar (Vminibuffer_list)), Qnil);
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
		       : Fcar (Vminibuffer_list)), Qnil);
  return f;
}
#endif /* HAVE_WINDOW_SYSTEM */

/* Construct a frame that refers to a terminal.  */

static int tty_frame_count;

struct frame *
make_initial_frame (void)
{
  struct frame *f;
  struct terminal *terminal;
  Lisp_Object frame;

  eassert (initial_kboard);

  /* The first call must initialize Vframe_list.  */
  if (! (NILP (Vframe_list) || CONSP (Vframe_list)))
    Vframe_list = Qnil;

  terminal = init_initial_terminal ();

  f = make_frame (1);
  XSETFRAME (frame, f);

  Vframe_list = Fcons (frame, Vframe_list);

  tty_frame_count = 1;
  f->name = build_string ("F1");

  f->visible = 1;
  f->async_visible = 1;

  f->output_method = terminal->type;
  f->terminal = terminal;
  f->terminal->reference_count++;
  f->output_data.nothing = 0;

  FRAME_FOREGROUND_PIXEL (f) = FACE_TTY_DEFAULT_FG_COLOR;
  FRAME_BACKGROUND_PIXEL (f) = FACE_TTY_DEFAULT_BG_COLOR;

  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
  FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = vertical_scroll_bar_none;

  return f;
}


struct frame *
make_terminal_frame (struct terminal *terminal)
{
  register struct frame *f;
  Lisp_Object frame;
  char name[20];

  if (!terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  f = make_frame (1);

  XSETFRAME (frame, f);
  Vframe_list = Fcons (frame, Vframe_list);

  tty_frame_count++;
  sprintf (name, "F%d", tty_frame_count);
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
	  && FRAME_BACKGROUND_PIXEL (f) == 0
	  && FRAME_FOREGROUND_PIXEL (f) == 0)
	{
	  FRAME_BACKGROUND_PIXEL (f) = FACE_TTY_DEFAULT_BG_COLOR;
	  FRAME_FOREGROUND_PIXEL (f) = FACE_TTY_DEFAULT_FG_COLOR;
	}
    }
  else
    f->output_method = output_termcap;
#else
  {
    f->output_method = output_termcap;
    f->terminal = terminal;
    f->terminal->reference_count++;
    create_tty_output (f);

    FRAME_FOREGROUND_PIXEL (f) = FACE_TTY_DEFAULT_FG_COLOR;
    FRAME_BACKGROUND_PIXEL (f) = FACE_TTY_DEFAULT_BG_COLOR;

    FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
    FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = vertical_scroll_bar_none;

    /* Set the top frame to the newly created frame. */
    if (FRAMEP (FRAME_TTY (f)->top_frame)
        && FRAME_LIVE_P (XFRAME (FRAME_TTY (f)->top_frame)))
      XFRAME (FRAME_TTY (f)->top_frame)->async_visible = 2; /* obscured */

    FRAME_TTY (f)->top_frame = frame;
  }

#ifdef CANNOT_DUMP
  FRAME_FOREGROUND_PIXEL(f) = FACE_TTY_DEFAULT_FG_COLOR;
  FRAME_BACKGROUND_PIXEL(f) = FACE_TTY_DEFAULT_BG_COLOR;
#endif
#endif /* MSDOS */

  if (!noninteractive)
    init_frame_faces (f);

  return f;
}

/* Get a suitable value for frame parameter PARAMETER for a newly
   created frame, based on (1) the user-supplied frame parameter
   alist SUPPLIED_PARMS, (2) CURRENT_VALUE, and finally, if all else
   fails, (3) Vdefault_frame_alist.  */

static Lisp_Object
get_future_frame_param (Lisp_Object parameter,
                        Lisp_Object supplied_parms,
                        char *current_value)
{
  Lisp_Object result;

  result = Fassq (parameter, supplied_parms);
  if (NILP (result))
    result = Fassq (parameter, XFRAME (selected_frame)->param_alist);
  if (NILP (result) && current_value != NULL)
    result = build_string (current_value);
  if (NILP (result))
    result = Fassq (parameter, Vdefault_frame_alist);
  if (!NILP (result) && !STRINGP (result))
    result = XCDR (result);
  if (NILP (result) || !STRINGP (result))
    result = Qnil;

  return result;
}

DEFUN ("make-terminal-frame", Fmake_terminal_frame, Smake_terminal_frame,
       1, 1, 0,
       doc: /* Create an additional terminal frame, possibly on another terminal.
This function takes one argument, an alist specifying frame parameters.

You can create multiple frames on a single text-only terminal, but
only one of them (the selected terminal frame) is actually displayed.

In practice, generally you don't need to specify any parameters,
except when you want to create a new frame on another terminal.
In that case, the `tty' parameter specifies the device file to open,
and the `tty-type' parameter specifies the terminal type.  Example:

   (make-terminal-frame '((tty . "/dev/pts/5") (tty-type . "xterm")))

Note that changing the size of one terminal frame automatically
affects all frames on the same terminal device.  */)
     (parms)
     Lisp_Object parms;
{
  struct frame *f;
  struct terminal *t = NULL;
  Lisp_Object frame, tem;
  struct frame *sf = SELECTED_FRAME ();

#ifdef MSDOS
  if (sf->output_method != output_msdos_raw
      && sf->output_method != output_termcap)
    abort ();
#else /* not MSDOS */

#if 0
  /* This can happen for multi-tty when using both terminal frames and
     Carbon frames. */
  if (sf->output_method != output_mac)
    error ("Not running on a Macintosh screen; cannot make a new Macintosh frame");
#else
#if 0                           /* This should work now! */
  if (sf->output_method != output_termcap)
    error ("Not using an ASCII terminal now; cannot make a new ASCII frame");
#endif
#endif
#endif /* not MSDOS */

  {
    Lisp_Object terminal;

    terminal = Fassq (Qterminal, parms);
    if (!NILP (terminal))
      {
        terminal = XCDR (terminal);
        t = get_terminal (terminal, 1);
      }
  }

  if (!t)
    {
      char *name = 0, *type = 0;
      Lisp_Object tty, tty_type;

      tty = get_future_frame_param
        (Qtty, parms, (FRAME_TERMCAP_P (XFRAME (selected_frame))
                       ? FRAME_TTY (XFRAME (selected_frame))->name
                       : NULL));
      if (!NILP (tty))
        {
          name = (char *) alloca (SBYTES (tty) + 1);
          strncpy (name, SDATA (tty), SBYTES (tty));
          name[SBYTES (tty)] = 0;
        }

      tty_type = get_future_frame_param
        (Qtty_type, parms, (FRAME_TERMCAP_P (XFRAME (selected_frame))
                            ? FRAME_TTY (XFRAME (selected_frame))->type
                            : NULL));
      if (!NILP (tty_type))
        {
          type = (char *) alloca (SBYTES (tty_type) + 1);
          strncpy (type, SDATA (tty_type), SBYTES (tty_type));
          type[SBYTES (tty_type)] = 0;
        }

      t = init_tty (name, type, 0); /* Errors are not fatal. */
    }

  f = make_terminal_frame (t);

  {
    int width, height;
    get_tty_size (fileno (FRAME_TTY (f)->input), &width, &height);
    change_frame_size (f, height, width, 0, 0, 0);
  }

  adjust_glyphs (f);
  calculate_costs (f);
  XSETFRAME (frame, f);
  Fmodify_frame_parameters (frame, Vdefault_frame_alist);
  Fmodify_frame_parameters (frame, parms);
  Fmodify_frame_parameters (frame, Fcons (Fcons (Qtty_type,
                                                 build_string (t->display_info.tty->type)),
                                          Qnil));
  if (t->display_info.tty->name != NULL)
    Fmodify_frame_parameters (frame, Fcons (Fcons (Qtty,
                                                   build_string (t->display_info.tty->name)),
                                            Qnil));
  else
    Fmodify_frame_parameters (frame, Fcons (Fcons (Qtty, Qnil), Qnil));

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
   deleted, which includes the possibility that the frame's terminal
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

  if (FRAME_TERMCAP_P (XFRAME (selected_frame))
      && FRAME_TERMCAP_P (XFRAME (frame))
      && FRAME_TTY (XFRAME (selected_frame)) == FRAME_TTY (XFRAME (frame)))
    {
      XFRAME (selected_frame)->async_visible = 2; /* obscured */
      XFRAME (frame)->async_visible = 1;
      FRAME_TTY (XFRAME (frame))->top_frame = frame;
    }

  selected_frame = frame;
  if (! FRAME_MINIBUF_ONLY_P (XFRAME (selected_frame)))
    last_nonminibuf_frame = XFRAME (selected_frame);

  Fselect_window (XFRAME (frame)->selected_window, Qnil);

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

DEFUN ("select-frame", Fselect_frame, Sselect_frame, 1, 1, "e",
       doc: /* Select the frame FRAME.
Subsequent editing commands apply to its selected window.
The selection of FRAME lasts until the next time the user does
something to select a different frame, or until the next time this
function is called.  If you are using a window system, the previously
selected frame may be restored as the selected frame after return to
the command loop, because it still may have the window system's input
focus.  On a text-only terminal, the next redisplay will display FRAME.

This function returns FRAME, or nil if FRAME has been deleted.  */)
  (frame)
    Lisp_Object frame;
{
  return do_switch_frame (frame, 1, 0);
}


DEFUN ("handle-switch-frame", Fhandle_switch_frame, Shandle_switch_frame, 1, 1, "e",
       doc: /* Handle a switch-frame event EVENT.
Switch-frame events are usually bound to this function.
A switch-frame event tells Emacs that the window manager has requested
that the user's events be directed to the frame mentioned in the event.
This function selects the selected window of the frame of EVENT.

If EVENT is frame object, handle it as if it were a switch-frame event
to that frame.  */)
     (event)
     Lisp_Object event;
{
  /* Preserve prefix arg that the command loop just cleared.  */
  current_kboard->Vprefix_arg = Vcurrent_prefix_arg;
  call1 (Vrun_hooks, Qmouse_leave_buffer_hook);
  return do_switch_frame (event, 0, 0);
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
Return WINDOW.
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
    return Fselect_window (window, Qnil);

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

static Lisp_Object
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
	    && ((!FRAME_TERMCAP_P (XFRAME (f)) && !FRAME_TERMCAP_P (XFRAME (frame))
                 && FRAME_KBOARD (XFRAME (f)) == FRAME_KBOARD (XFRAME (frame)))
                || (FRAME_TERMCAP_P (XFRAME (f)) && FRAME_TERMCAP_P (XFRAME (frame))
                    && FRAME_TTY (XFRAME (f)) == FRAME_TTY (XFRAME (frame)))))
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

static Lisp_Object
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

      if ((!FRAME_TERMCAP_P (XFRAME (f)) && !FRAME_TERMCAP_P (XFRAME (frame))
           && FRAME_KBOARD (XFRAME (f)) == FRAME_KBOARD (XFRAME (frame)))
          || (FRAME_TERMCAP_P (XFRAME (f)) && FRAME_TERMCAP_P (XFRAME (frame))
              && FRAME_TTY (XFRAME (f)) == FRAME_TTY (XFRAME (frame))))
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

/* Error handler for `delete-frame-functions'. */
static Lisp_Object
delete_frame_handler (Lisp_Object arg)
{
  add_to_log ("Error during `delete-frame': %s", arg, Qnil);
  return Qnil;
}

DEFUN ("delete-frame", Fdelete_frame, Sdelete_frame, 0, 2, "",
       doc: /* Delete FRAME, permanently eliminating it from use.
If omitted, FRAME defaults to the selected frame.
A frame may not be deleted if its minibuffer is used by other frames.
Normally, you may not delete a frame if all other frames are invisible,
but if the second optional argument FORCE is non-nil, you may do so.

This function runs `delete-frame-functions' before actually deleting the
frame, unless the frame is a tooltip.
The functions are run with one arg, the frame to be deleted.
But FORCE inhibits this too.  */)
/* FORCE is non-nil when handling a disconnected terminal.  */
     (frame, force)
     Lisp_Object frame, force;
{
  struct frame *f;
  struct frame *sf = SELECTED_FRAME ();
  struct kboard *kb;

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

  if (NILP (force) && !other_visible_frames (f))
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
	    {
	      /* If we MUST delete this frame, delete the other first.  */
	      if (!NILP (force))
		Fdelete_frame (this, force);
	      else
		error ("Attempt to delete a surrogate minibuffer frame");
	    }
	}
    }

  /* Run `delete-frame-functions'
     unless FORCE is `noelisp' or frame is a tooltip.
     FORCE is set to `noelisp' when handling a disconnect from the terminal,
     so we don't dare call Lisp code.  */
  if (!NILP (Vrun_hooks) && !EQ (force, Qnoelisp)
      && NILP (Fframe_parameter (frame, intern ("tooltip"))))
    {
      Lisp_Object args[2];
      struct gcpro gcpro1, gcpro2;

      /* Don't let a rogue function in `delete-frame-functions'
	 prevent the frame deletion. */
      GCPRO2 (args[0], args[1]);
      args[0] = intern ("delete-frame-functions");
      args[1] = frame;
      internal_condition_case_2 (Frun_hook_with_args, 2, args,
				 Qt, delete_frame_handler);
      UNGCPRO;
    }

  /* The hook may sometimes (indirectly) cause the frame to be deleted.  */
  if (! FRAME_LIVE_P (f))
    return Qnil;

  /* At this point, we are committed to deleting the frame.
     There is no more chance for errors to prevent it.  */

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
	      if (! EQ (frame, frame1) && FRAME_LIVE_P (XFRAME (frame1)))
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
			  XWINDOW (minibuf_window)->buffer, Qnil);
      minibuf_window = sf->minibuffer_window;

      /* If the dying minibuffer window was selected,
	 select the new one.  */
      if (minibuffer_selected)
	Fselect_window (minibuf_window, Qnil);
    }

  /* Don't let echo_area_window to remain on a deleted frame.  */
  if (EQ (f->minibuffer_window, echo_area_window))
    echo_area_window = sf->minibuffer_window;

  /* Clear any X selections for this frame.  */
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (f))
    x_clear_frame_selections (f);
#endif
#ifdef MAC_OS
  if (FRAME_MAC_P (f))
    x_clear_frame_selections (f);
#endif

  /* Free glyphs.
     This function must be called before the window tree of the
     frame is deleted because windows contain dynamically allocated
     memory. */
  free_glyphs (f);

#ifdef USE_FONT_BACKEND
  /* Give chance to each font driver to free a frame specific data.  */
  font_update_drivers (f, Qnil);
#endif	/* USE_FONT_BACKEND */

  /* Mark all the windows that used to be on FRAME as deleted, and then
     remove the reference to them.  */
  delete_all_subwindows (XWINDOW (f->root_window));
  f->root_window = Qnil;

  Vframe_list = Fdelq (frame, Vframe_list);
  FRAME_SET_VISIBLE (f, 0);

  if (f->namebuf)
    xfree (f->namebuf);
  if (f->decode_mode_spec_buffer)
    xfree (f->decode_mode_spec_buffer);
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
     an event for f at any time; if we zero out the frame's terminal
     now, then we may trip up the event-handling code.  Instead, we'll
     promise that the terminal of the frame must be valid until we
     have called the window-system-dependent frame destruction
     routine.  */

  if (FRAME_TERMINAL (f)->delete_frame_hook)
    (*FRAME_TERMINAL (f)->delete_frame_hook) (f);

  {
    struct terminal *terminal = FRAME_TERMINAL (f);
    f->output_data.nothing = 0;
    f->terminal = 0;             /* Now the frame is dead. */

    /* If needed, delete the terminal that this frame was on.
       (This must be done after the frame is killed.) */
    terminal->reference_count--;
    if (terminal->reference_count == 0)
      {
	Lisp_Object tmp;
	XSETTERMINAL (tmp, terminal);

        kb = NULL;
	Fdelete_terminal (tmp, NILP (force) ? Qt : force);
      }
#ifdef MULTI_KBOARD
    else
      kb = terminal->kboard;
#endif
  }

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

  /* If there's no other frame on the same kboard, get out of
     single-kboard state if we're in it for this kboard.  */
  if (kb != NULL)
    {
      Lisp_Object frames;
      /* Some frame we found on the same kboard, or nil if there are none.  */
      Lisp_Object frame_on_same_kboard;

      frame_on_same_kboard = Qnil;

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

	  if (kb == FRAME_KBOARD (f1))
	    frame_on_same_kboard = this;
	}

      if (NILP (frame_on_same_kboard))
	not_single_kboard_state (kb);
    }


  /* If we've deleted this keyboard's default_minibuffer_frame, try to
     find another one.  Prefer minibuffer-only frames, but also notice
     frames with other windows.  */
  if (kb != NULL && EQ (frame, kb->Vdefault_minibuffer_frame))
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
	  if (kb == FRAME_KBOARD (f1)
	      && FRAME_HAS_MINIBUF_P (f1))
	    {
	      frame_with_minibuf = this;
	      if (FRAME_MINIBUF_ONLY_P (f1))
		break;
	    }

	  if (kb == FRAME_KBOARD (f1))
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

	  kb->Vdefault_minibuffer_frame = frame_with_minibuf;
	}
      else
	/* No frames left on this kboard--say no minibuffer either.  */
	kb->Vdefault_minibuffer_frame = Qnil;
    }

  /* Cause frame titles to update--necessary if we now have just one frame.  */
  update_mode_lines = 1;

  return Qnil;
}

/* Return mouse position in character cell units.  */

DEFUN ("mouse-position", Fmouse_position, Smouse_position, 0, 0, 0,
       doc: /* Return a list (FRAME X . Y) giving the current mouse frame and position.
The position is given in character cells, where (0, 0) is the
upper-left corner of the frame, X is the horizontal offset, and Y is
the vertical offset.
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

#if defined (HAVE_MOUSE) || defined (HAVE_GPM)
  /* It's okay for the hook to refrain from storing anything.  */
  if (FRAME_TERMINAL (f)->mouse_position_hook)
    (*FRAME_TERMINAL (f)->mouse_position_hook) (&f, -1,
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
upper-left corner of the frame, X is the horizontal offset, and Y is
the vertical offset.
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

#if defined (HAVE_MOUSE) || defined (HAVE_GPM)
  /* It's okay for the hook to refrain from storing anything.  */
  if (FRAME_TERMINAL (f)->mouse_position_hook)
    (*FRAME_TERMINAL (f)->mouse_position_hook) (&f, -1,
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

The position is given in character cells, where (0, 0) is the
upper-left corner of the frame, X is the horizontal offset, and Y is
the vertical offset.

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
      Fselect_frame (frame);
      mouse_moveto (XINT (x), XINT (y));
    }
#else
#ifdef HAVE_GPM
    {
      Fselect_frame (frame);
      term_mouse_moveto (XINT (x), XINT (y));
    }
#endif
#endif
#endif

  return Qnil;
}

DEFUN ("set-mouse-pixel-position", Fset_mouse_pixel_position,
       Sset_mouse_pixel_position, 3, 3, 0,
       doc: /* Move the mouse pointer to pixel position (X,Y) in FRAME.
The position is given in pixels, where (0, 0) is the upper-left corner
of the frame, X is the horizontal offset, and Y is the vertical offset.

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
      Fselect_frame (frame);
      mouse_moveto (XINT (x), XINT (y));
    }
#else
#ifdef HAVE_GPM
    {
      Fselect_frame (frame);
      term_mouse_moveto (XINT (x), XINT (y));
    }
#endif
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
			  XWINDOW (minibuf_window)->buffer, Qnil);
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
    Fhandle_switch_frame (next_frame (frame, Qt));
#endif

  /* Don't allow minibuf_window to remain on a deleted frame.  */
  if (EQ (XFRAME (frame)->minibuffer_window, minibuf_window))
    {
      struct frame *sf = XFRAME (selected_frame);
      Fset_window_buffer (sf->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer, Qnil);
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
Return the symbol `icon' if frame is visible only as an icon.

On a text-only terminal, all frames are considered visible, whether
they are currently being displayed or not, and this function returns t
for all frames.  */)
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
If FRAME is invisible or iconified, make it visible.
If you don't specify a frame, the selected frame is used.
If Emacs is displaying on an ordinary terminal or some other device which
doesn't support multiple overlapping frames, this function does nothing.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f;
  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  f = XFRAME (frame);

  /* Do like the documentation says. */
  Fmake_frame_visible (frame);

  if (FRAME_TERMINAL (f)->frame_raise_lower_hook)
    (*FRAME_TERMINAL (f)->frame_raise_lower_hook) (f, 1);

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
  struct frame *f;

  if (NILP (frame))
    frame = selected_frame;

  CHECK_LIVE_FRAME (frame);

  f = XFRAME (frame);

  if (FRAME_TERMINAL (f)->frame_raise_lower_hook)
    (*FRAME_TERMINAL (f)->frame_raise_lower_hook) (f, 0);

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

A frame's focus redirection can be changed by `select-frame'.  If frame
FOO is selected, and then a different frame BAR is selected, any
frames redirecting their focus to FOO are shifted to redirect their
focus to BAR.  This allows focus redirection to work properly when the
user switches from one frame to another using `select-window'.

This means that a frame whose focus is redirected to itself is treated
differently from a frame whose focus is redirected to nil; the former
is affected by `select-frame', while the latter is not.

The redirection lasts until `redirect-frame-focus' is called to change it.  */)
     (frame, focus_frame)
     Lisp_Object frame, focus_frame;
{
  struct frame *f;

  /* Note that we don't check for a live frame here.  It's reasonable
     to redirect the focus of a frame you're about to delete, if you
     know what other frame should receive those keystrokes.  */
  CHECK_FRAME (frame);

  if (! NILP (focus_frame))
    CHECK_LIVE_FRAME (focus_frame);

  f = XFRAME (frame);

  f->focus_frame = focus_frame;

  if (FRAME_TERMINAL (f)->frame_rehighlight_hook)
    (*FRAME_TERMINAL (f)->frame_rehighlight_hook) (f);

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

/* Discard BUFFER from the buffer-list and buried-buffer-list of each frame.  */

void
frames_discard_buffer (buffer)
     Lisp_Object buffer;
{
  Lisp_Object frame, tail;

  FOR_EACH_FRAME (tail, frame)
    {
      XFRAME (frame)->buffer_list
	= Fdelq (buffer, XFRAME (frame)->buffer_list);
      XFRAME (frame)->buried_buffer_list
        = Fdelq (buffer, XFRAME (frame)->buried_buffer_list);
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
     EMACS_INT len;
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

static void
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
      if (frame_name_fnn_p (SDATA (f->name),
			    SBYTES (f->name)))
	return;

      tty_frame_count++;
      sprintf (namebuf, "F%d", tty_frame_count);
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
      if (frame_name_fnn_p (SDATA (name), SBYTES (name)))
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

  /* The buffer-list parameters are stored in a special place and not
     in the alist.  */
  if (EQ (prop, Qbuffer_list))
    {
      f->buffer_list = val;
      return;
    }
  if (EQ (prop, Qburied_buffer_list))
    {
      f->buried_buffer_list = val;
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
      if ((BUFFER_LOCAL_VALUEP (valcontents))
	  && XBUFFER_LOCAL_VALUE (valcontents)->check_frame
	  && XBUFFER_LOCAL_VALUE (valcontents)->found_for_frame
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
	error ("Surrogate minibuffer windows must be minibuffer windows");

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
      if (CONSP (elt) && STRINGP (XCDR (elt)))
	{
	  if (strncmp (SDATA (XCDR (elt)),
		       unspecified_bg,
		       SCHARS (XCDR (elt))) == 0)
	    store_in_alist (&alist, Qforeground_color, tty_color_name (f, bg));
	  else if (strncmp (SDATA (XCDR (elt)),
			    unspecified_fg,
			    SCHARS (XCDR (elt))) == 0)
	    store_in_alist (&alist, Qforeground_color, tty_color_name (f, fg));
	}
      else
	store_in_alist (&alist, Qforeground_color, tty_color_name (f, fg));
      elt = Fassq (Qbackground_color, alist);
      if (CONSP (elt) && STRINGP (XCDR (elt)))
	{
	  if (strncmp (SDATA (XCDR (elt)),
		       unspecified_fg,
		       SCHARS (XCDR (elt))) == 0)
	    store_in_alist (&alist, Qbackground_color, tty_color_name (f, fg));
	  else if (strncmp (SDATA (XCDR (elt)),
			    unspecified_bg,
			    SCHARS (XCDR (elt))) == 0)
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
  height = (f->new_text_lines ? f->new_text_lines : FRAME_LINES (f));
  store_in_alist (&alist, Qheight, make_number (height));
  width = (f->new_text_cols ? f->new_text_cols : FRAME_COLS (f));
  store_in_alist (&alist, Qwidth, make_number (width));
  store_in_alist (&alist, Qmodeline, (FRAME_WANTS_MODELINE_P (f) ? Qt : Qnil));
  store_in_alist (&alist, Qminibuffer,
		  (! FRAME_HAS_MINIBUF_P (f) ? Qnil
		   : FRAME_MINIBUF_ONLY_P (f) ? Qonly
		   : FRAME_MINIBUF_WINDOW (f)));
  store_in_alist (&alist, Qunsplittable, (FRAME_NO_SPLIT_P (f) ? Qt : Qnil));
  store_in_alist (&alist, Qbuffer_list, frame_buffer_list (frame));
  store_in_alist (&alist, Qburied_buffer_list, XFRAME (frame)->buried_buffer_list);

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
		  const char *color_name;
		  EMACS_INT csz;

		  if (EQ (parameter, Qbackground_color))
		    {
		      color_name = SDATA (value);
		      csz = SCHARS (value);
		      if (strncmp (color_name, unspecified_bg, csz) == 0)
			value = tty_color_name (f, FRAME_BACKGROUND_PIXEL (f));
		      else if (strncmp (color_name, unspecified_fg, csz) == 0)
			value = tty_color_name (f, FRAME_FOREGROUND_PIXEL (f));
		    }
		  else if (EQ (parameter, Qforeground_color))
		    {
		      color_name = SDATA (value);
		      csz = SCHARS (value);
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
      for (tail = alist; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object elt;

	  elt = XCAR (tail);
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

	  /* Changing the background color might change the background
	     mode, so that we have to load new defface specs.
	     Call frame-set-background-mode to do that.  */
	  if (EQ (prop, Qbackground_color))
	    call1 (Qframe_set_background_mode, frame);
	}
    }
  return Qnil;
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
On a graphical screen, the width is the standard width of the default font.
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
    return make_number (FRAME_LINES (f));
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
    return make_number (FRAME_COLS (f));
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
      if (XINT (lines) != FRAME_LINES (f))
	x_set_window_size (f, 1, FRAME_COLS (f), XINT (lines));
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
      if (XINT (cols) != FRAME_COLS (f))
	x_set_window_size (f, 1, XINT (cols), FRAME_LINES (f));
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
      if (XINT (rows) != FRAME_LINES (f)
	  || XINT (cols) != FRAME_COLS (f)
	  || f->new_text_lines || f->new_text_cols)
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


/***********************************************************************
				Frame Parameters
 ***********************************************************************/

/* Connect the frame-parameter names for X frames
   to the ways of passing the parameter values to the window system.

   The name of a parameter, as a Lisp symbol,
   has an `x-frame-parameter' property which is an integer in Lisp
   that is an index in this table.  */

struct frame_parm_table {
  char *name;
  Lisp_Object *variable;
};

static struct frame_parm_table frame_parms[] =
{
  {"auto-raise",		&Qauto_raise},
  {"auto-lower",		&Qauto_lower},
  {"background-color",		0},
  {"border-color",		&Qborder_color},
  {"border-width",		&Qborder_width},
  {"cursor-color",		&Qcursor_color},
  {"cursor-type",		&Qcursor_type},
  {"font",			0},
  {"foreground-color",		0},
  {"icon-name",			&Qicon_name},
  {"icon-type",			&Qicon_type},
  {"internal-border-width",	&Qinternal_border_width},
  {"menu-bar-lines",		&Qmenu_bar_lines},
  {"mouse-color",		&Qmouse_color},
  {"name",			&Qname},
  {"scroll-bar-width",		&Qscroll_bar_width},
  {"title",			&Qtitle},
  {"unsplittable",		&Qunsplittable},
  {"vertical-scroll-bars",	&Qvertical_scroll_bars},
  {"visibility",		&Qvisibility},
  {"tool-bar-lines",		&Qtool_bar_lines},
  {"scroll-bar-foreground",	&Qscroll_bar_foreground},
  {"scroll-bar-background",	&Qscroll_bar_background},
  {"screen-gamma",		&Qscreen_gamma},
  {"line-spacing",		&Qline_spacing},
  {"left-fringe",		&Qleft_fringe},
  {"right-fringe",		&Qright_fringe},
  {"wait-for-wm",		&Qwait_for_wm},
  {"fullscreen",                &Qfullscreen},
#ifdef USE_FONT_BACKEND
  {"font-backend",		&Qfont_backend}
#endif	/* USE_FONT_BACKEND */
};

#ifdef HAVE_WINDOW_SYSTEM

extern Lisp_Object Qbox;
extern Lisp_Object Qtop;

/* Calculate fullscreen size.  Return in *TOP_POS and *LEFT_POS the
   wanted positions of the WM window (not Emacs window).
   Return in *WIDTH and *HEIGHT the wanted width and height of Emacs
   window (FRAME_X_WINDOW).
 */

void
x_fullscreen_adjust (f, width, height, top_pos, left_pos)
     struct frame *f;
     int *width;
     int *height;
     int *top_pos;
     int *left_pos;
{
  int newwidth = FRAME_COLS (f);
  int newheight = FRAME_LINES (f);

  *top_pos = f->top_pos;
  *left_pos = f->left_pos;

  if (f->want_fullscreen & FULLSCREEN_HEIGHT)
    {
      int ph;

      ph = FRAME_X_DISPLAY_INFO (f)->height;
      newheight = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, ph);
      ph = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, newheight) - f->y_pixels_diff;
      newheight = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, ph);
      *top_pos = 0;
    }

  if (f->want_fullscreen & FULLSCREEN_WIDTH)
    {
      int pw;

      pw = FRAME_X_DISPLAY_INFO (f)->width;
      newwidth = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, pw);
      pw = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, newwidth) - f->x_pixels_diff;
      newwidth = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, pw);
      *left_pos = 0;
    }

  *width = newwidth;
  *height = newheight;
}


/* Change the parameters of frame F as specified by ALIST.
   If a parameter is not specially recognized, do nothing special;
   otherwise call the `x_set_...' function for that parameter.
   Except for certain geometry properties, always call store_frame_param
   to store the new value in the parameter alist.  */

void
x_set_frame_parameters (f, alist)
     FRAME_PTR f;
     Lisp_Object alist;
{
  Lisp_Object tail;

  /* If both of these parameters are present, it's more efficient to
     set them both at once.  So we wait until we've looked at the
     entire list before we set them.  */
  int width, height;

  /* Same here.  */
  Lisp_Object left, top;

  /* Same with these.  */
  Lisp_Object icon_left, icon_top;

  /* Record in these vectors all the parms specified.  */
  Lisp_Object *parms;
  Lisp_Object *values;
  int i, p;
  int left_no_change = 0, top_no_change = 0;
  int icon_left_no_change = 0, icon_top_no_change = 0;
  int fullscreen_is_being_set = 0;

  struct gcpro gcpro1, gcpro2;

  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    i++;

  parms = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));
  values = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));

  /* Extract parm names and values into those vectors.  */

  i = 0;
  for (tail = alist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt;

      elt = XCAR (tail);
      parms[i] = Fcar (elt);
      values[i] = Fcdr (elt);
      i++;
    }
  /* TAIL and ALIST are not used again below here.  */
  alist = tail = Qnil;

  GCPRO2 (*parms, *values);
  gcpro1.nvars = i;
  gcpro2.nvars = i;

  /* There is no need to gcpro LEFT, TOP, ICON_LEFT, or ICON_TOP,
     because their values appear in VALUES and strings are not valid.  */
  top = left = Qunbound;
  icon_left = icon_top = Qunbound;

  /* Provide default values for HEIGHT and WIDTH.  */
  width = (f->new_text_cols ? f->new_text_cols : FRAME_COLS (f));
  height = (f->new_text_lines ? f->new_text_lines : FRAME_LINES (f));

  /* Process foreground_color and background_color before anything else.
     They are independent of other properties, but other properties (e.g.,
     cursor_color) are dependent upon them.  */
  /* Process default font as well, since fringe widths depends on it.  */
  /* Also, process fullscreen, width and height depend upon that */
  for (p = 0; p < i; p++)
    {
      Lisp_Object prop, val;

      prop = parms[p];
      val = values[p];
      if (EQ (prop, Qforeground_color)
	  || EQ (prop, Qbackground_color)
	  || EQ (prop, Qfont)
          || EQ (prop, Qfullscreen))
	{
	  register Lisp_Object param_index, old_value;
	  int count = SPECPDL_INDEX ();

	  old_value = get_frame_param (f, prop);
 	  fullscreen_is_being_set |= EQ (prop, Qfullscreen);

	  if (NILP (Fequal (val, old_value)))
	    {
	      /* For :font attributes, the frame_parm_handler
		 x_set_font calls `face-set-after-frame-default'.
		 Unless we bind inhibit-face-set-after-frame-default
		 here, this would reset the :font attribute that we
		 just applied to the default value for new faces.  */
	      specbind (Qinhibit_face_set_after_frame_default, Qt);

	      store_frame_param (f, prop, val);

	      param_index = Fget (prop, Qx_frame_parameter);
	      if (NATNUMP (param_index)
		  && (XFASTINT (param_index)
		      < sizeof (frame_parms)/sizeof (frame_parms[0]))
                  && FRAME_RIF (f)->frame_parm_handlers[XINT (param_index)])
                (*(FRAME_RIF (f)->frame_parm_handlers[XINT (param_index)])) (f, val, old_value);
	      unbind_to (count, Qnil);
	    }
	}
    }

  /* Now process them in reverse of specified order.  */
  for (i--; i >= 0; i--)
    {
      Lisp_Object prop, val;

      prop = parms[i];
      val = values[i];

      if (EQ (prop, Qwidth) && NATNUMP (val))
	width = XFASTINT (val);
      else if (EQ (prop, Qheight) && NATNUMP (val))
	height = XFASTINT (val);
      else if (EQ (prop, Qtop))
	top = val;
      else if (EQ (prop, Qleft))
	left = val;
      else if (EQ (prop, Qicon_top))
	icon_top = val;
      else if (EQ (prop, Qicon_left))
	icon_left = val;
      else if (EQ (prop, Qforeground_color)
	       || EQ (prop, Qbackground_color)
	       || EQ (prop, Qfont)
               || EQ (prop, Qfullscreen))
	/* Processed above.  */
	continue;
      else
	{
	  register Lisp_Object param_index, old_value;

	  old_value = get_frame_param (f, prop);

	  store_frame_param (f, prop, val);

	  param_index = Fget (prop, Qx_frame_parameter);
	  if (NATNUMP (param_index)
	      && (XFASTINT (param_index)
		  < sizeof (frame_parms)/sizeof (frame_parms[0]))
	      && FRAME_RIF (f)->frame_parm_handlers[XINT (param_index)])
	    (*(FRAME_RIF (f)->frame_parm_handlers[XINT (param_index)])) (f, val, old_value);
	}
    }

  /* Don't die if just one of these was set.  */
  if (EQ (left, Qunbound))
    {
      left_no_change = 1;
      if (f->left_pos < 0)
	left = Fcons (Qplus, Fcons (make_number (f->left_pos), Qnil));
      else
	XSETINT (left, f->left_pos);
    }
  if (EQ (top, Qunbound))
    {
      top_no_change = 1;
      if (f->top_pos < 0)
	top = Fcons (Qplus, Fcons (make_number (f->top_pos), Qnil));
      else
	XSETINT (top, f->top_pos);
    }

  /* If one of the icon positions was not set, preserve or default it.  */
  if (EQ (icon_left, Qunbound) || ! INTEGERP (icon_left))
    {
      icon_left_no_change = 1;
      icon_left = Fcdr (Fassq (Qicon_left, f->param_alist));
      if (NILP (icon_left))
	XSETINT (icon_left, 0);
    }
  if (EQ (icon_top, Qunbound) || ! INTEGERP (icon_top))
    {
      icon_top_no_change = 1;
      icon_top = Fcdr (Fassq (Qicon_top, f->param_alist));
      if (NILP (icon_top))
	XSETINT (icon_top, 0);
    }

  if (FRAME_VISIBLE_P (f) && fullscreen_is_being_set)
    {
      /* If the frame is visible already and the fullscreen parameter is
         being set, it is too late to set WM manager hints to specify
         size and position.
         Here we first get the width, height and position that applies to
         fullscreen.  We then move the frame to the appropriate
         position.  Resize of the frame is taken care of in the code after
         this if-statement. */
      int new_left, new_top;

      x_fullscreen_adjust (f, &width, &height, &new_top, &new_left);
      if (new_top != f->top_pos || new_left != f->left_pos)
        x_set_offset (f, new_left, new_top, 1);
    }

  /* Don't set these parameters unless they've been explicitly
     specified.  The window might be mapped or resized while we're in
     this function, and we don't want to override that unless the lisp
     code has asked for it.

     Don't set these parameters unless they actually differ from the
     window's current parameters; the window may not actually exist
     yet.  */
  {
    Lisp_Object frame;

    check_frame_size (f, &height, &width);

    XSETFRAME (frame, f);

    if (width != FRAME_COLS (f)
	|| height != FRAME_LINES (f)
	|| f->new_text_lines || f->new_text_cols)
      Fset_frame_size (frame, make_number (width), make_number (height));

    if ((!NILP (left) || !NILP (top))
	&& ! (left_no_change && top_no_change)
	&& ! (NUMBERP (left) && XINT (left) == f->left_pos
	      && NUMBERP (top) && XINT (top) == f->top_pos))
      {
	int leftpos = 0;
	int toppos = 0;

	/* Record the signs.  */
	f->size_hint_flags &= ~ (XNegative | YNegative);
	if (EQ (left, Qminus))
	  f->size_hint_flags |= XNegative;
	else if (INTEGERP (left))
	  {
	    leftpos = XINT (left);
	    if (leftpos < 0)
	      f->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qminus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = - XINT (XCAR (XCDR (left)));
	    f->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qplus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = XINT (XCAR (XCDR (left)));
	  }

	if (EQ (top, Qminus))
	  f->size_hint_flags |= YNegative;
	else if (INTEGERP (top))
	  {
	    toppos = XINT (top);
	    if (toppos < 0)
	      f->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qminus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = - XINT (XCAR (XCDR (top)));
	    f->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qplus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = XINT (XCAR (XCDR (top)));
	  }


	/* Store the numeric value of the position.  */
	f->top_pos = toppos;
	f->left_pos = leftpos;

	f->win_gravity = NorthWestGravity;

	/* Actually set that position, and convert to absolute.  */
	x_set_offset (f, leftpos, toppos, -1);
      }

    if ((!NILP (icon_left) || !NILP (icon_top))
	&& ! (icon_left_no_change && icon_top_no_change))
      x_wm_set_icon_position (f, XINT (icon_left), XINT (icon_top));
  }

  UNGCPRO;
}


/* Insert a description of internally-recorded parameters of frame X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to the X window system
   and whose values are not correctly recorded in the frame's
   param_alist need to be considered here.  */

void
x_report_frame_params (f, alistptr)
     struct frame *f;
     Lisp_Object *alistptr;
{
  char buf[16];
  Lisp_Object tem;

  /* Represent negative positions (off the top or left screen edge)
     in a way that Fmodify_frame_parameters will understand correctly.  */
  XSETINT (tem, f->left_pos);
  if (f->left_pos >= 0)
    store_in_alist (alistptr, Qleft, tem);
  else
    store_in_alist (alistptr, Qleft, Fcons (Qplus, Fcons (tem, Qnil)));

  XSETINT (tem, f->top_pos);
  if (f->top_pos >= 0)
    store_in_alist (alistptr, Qtop, tem);
  else
    store_in_alist (alistptr, Qtop, Fcons (Qplus, Fcons (tem, Qnil)));

  store_in_alist (alistptr, Qborder_width,
		  make_number (f->border_width));
  store_in_alist (alistptr, Qinternal_border_width,
		  make_number (FRAME_INTERNAL_BORDER_WIDTH (f)));
  store_in_alist (alistptr, Qleft_fringe,
		  make_number (FRAME_LEFT_FRINGE_WIDTH (f)));
  store_in_alist (alistptr, Qright_fringe,
		  make_number (FRAME_RIGHT_FRINGE_WIDTH (f)));
  store_in_alist (alistptr, Qscroll_bar_width,
		  (! FRAME_HAS_VERTICAL_SCROLL_BARS (f)
		   ? make_number (0)
		   : FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0
		   ? make_number (FRAME_CONFIG_SCROLL_BAR_WIDTH (f))
		   /* nil means "use default width"
		      for non-toolkit scroll bar.
		      ruler-mode.el depends on this.  */
		   : Qnil));
  sprintf (buf, "%ld", (long) FRAME_X_WINDOW (f));
  store_in_alist (alistptr, Qwindow_id,
		  build_string (buf));
#ifdef HAVE_X_WINDOWS
#ifdef USE_X_TOOLKIT
  /* Tooltip frame may not have this widget.  */
  if (FRAME_X_OUTPUT (f)->widget)
#endif
    sprintf (buf, "%ld", (long) FRAME_OUTER_WINDOW (f));
  store_in_alist (alistptr, Qouter_window_id,
		  build_string (buf));
#endif
  store_in_alist (alistptr, Qicon_name, f->icon_name);
  FRAME_SAMPLE_VISIBILITY (f);
  store_in_alist (alistptr, Qvisibility,
		  (FRAME_VISIBLE_P (f) ? Qt
		   : FRAME_ICONIFIED_P (f) ? Qicon : Qnil));
  store_in_alist (alistptr, Qdisplay,
		  XCAR (FRAME_X_DISPLAY_INFO (f)->name_list_element));

  if (FRAME_X_OUTPUT (f)->parent_desc == FRAME_X_DISPLAY_INFO (f)->root_window)
    tem = Qnil;
  else
    XSETFASTINT (tem, FRAME_X_OUTPUT (f)->parent_desc);
  store_in_alist (alistptr, Qexplicit_name, (f->explicit_name ? Qt : Qnil));
  store_in_alist (alistptr, Qparent_id, tem);
}


/* Change the `fullscreen' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value. */

void
x_set_fullscreen (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  if (NILP (new_value))
    f->want_fullscreen = FULLSCREEN_NONE;
  else if (EQ (new_value, Qfullboth))
    f->want_fullscreen = FULLSCREEN_BOTH;
  else if (EQ (new_value, Qfullwidth))
    f->want_fullscreen = FULLSCREEN_WIDTH;
  else if (EQ (new_value, Qfullheight))
    f->want_fullscreen = FULLSCREEN_HEIGHT;

  if (FRAME_TERMINAL (f)->fullscreen_hook != NULL)
    FRAME_TERMINAL (f)->fullscreen_hook (f);
}


/* Change the `line-spacing' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.  */

void
x_set_line_spacing (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  if (NILP (new_value))
    f->extra_line_spacing = 0;
  else if (NATNUMP (new_value))
    f->extra_line_spacing = XFASTINT (new_value);
  else
    signal_error ("Invalid line-spacing", new_value);
  if (FRAME_VISIBLE_P (f))
    redraw_frame (f);
}


/* Change the `screen-gamma' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.  */

void
x_set_screen_gamma (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  Lisp_Object bgcolor;

  if (NILP (new_value))
    f->gamma = 0;
  else if (NUMBERP (new_value) && XFLOATINT (new_value) > 0)
    /* The value 0.4545 is the normal viewing gamma.  */
    f->gamma = 1.0 / (0.4545 * XFLOATINT (new_value));
  else
    signal_error ("Invalid screen-gamma", new_value);

  /* Apply the new gamma value to the frame background.  */
  bgcolor = Fassq (Qbackground_color, f->param_alist);
  if (CONSP (bgcolor) && (bgcolor = XCDR (bgcolor), STRINGP (bgcolor)))
    {
      Lisp_Object index = Fget (Qbackground_color, Qx_frame_parameter);
      if (NATNUMP (index)
	  && (XFASTINT (index)
	      < sizeof (frame_parms)/sizeof (frame_parms[0]))
	  && FRAME_RIF (f)->frame_parm_handlers[XFASTINT (index)])
	  (*FRAME_RIF (f)->frame_parm_handlers[XFASTINT (index)])
	    (f, bgcolor, Qnil);
    }

  Fclear_face_cache (Qnil);
}


void
x_set_font (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  Lisp_Object result;
  Lisp_Object fontset_name;
  Lisp_Object frame;
  int old_fontset = FRAME_FONTSET(f);

#ifdef USE_FONT_BACKEND
  if (enable_font_backend)
    {
      int fontset = -1;
      Lisp_Object font_object;

      /* ARG is a fontset name, a font name, or a font object.
	 In the last case, this function never fail.  */
      if (STRINGP (arg))
	{
	  fontset = fs_query_fontset (arg, 0);
	  if (fontset < 0)
	    font_object = font_open_by_name (f, SDATA (arg));
	  else if (fontset > 0)
	    {
	      Lisp_Object ascii_font = fontset_ascii (fontset);

	      font_object = font_open_by_name (f, SDATA (ascii_font));
	    }
	}
      else
	font_object = arg;

      if (fontset < 0 && ! NILP (font_object))
	fontset = new_fontset_from_font (font_object);

      if (fontset == 0)
	/* Refuse the default fontset.  */
	result = Qt;
      else if (NILP (font_object))
	result = Qnil;
      else
	result = x_new_fontset2 (f, fontset, font_object);
    }
  else
    {
#endif	/* USE_FONT_BACKEND */
  CHECK_STRING (arg);

  fontset_name = Fquery_fontset (arg, Qnil);

  BLOCK_INPUT;
  result = (STRINGP (fontset_name)
            ? x_new_fontset (f, fontset_name)
            : x_new_fontset (f, arg));
  UNBLOCK_INPUT;
#ifdef USE_FONT_BACKEND
    }
#endif

  if (EQ (result, Qnil))
    error ("Font `%s' is not defined", SDATA (arg));
  else if (EQ (result, Qt))
    error ("The default fontset can't be used for a frame font");
  else if (STRINGP (result))
    {
      set_default_ascii_font (result);
      if (STRINGP (fontset_name))
	{
	  /* Fontset names are built from ASCII font names, so the
	     names may be equal despite there was a change.  */
	  if (old_fontset == FRAME_FONTSET (f))
	    return;
	}
      store_frame_param (f, Qfont, result);

      if (!NILP (Fequal (result, oldval)))
        return;

      /* Recalculate toolbar height.  */
      f->n_tool_bar_rows = 0;
      /* Ensure we redraw it.  */
      clear_current_matrices (f);

      recompute_basic_faces (f);
    }
  else
    abort ();

  do_pending_window_change (0);

  /* Don't call `face-set-after-frame-default' when faces haven't been
     initialized yet.  This is the case when called from
     Fx_create_frame.  In that case, the X widget or window doesn't
     exist either, and we can end up in x_report_frame_params with a
     null widget which gives a segfault.  */
  if (FRAME_FACE_CACHE (f))
    {
      XSETFRAME (frame, f);
      call1 (Qface_set_after_frame_default, frame);
    }
}


#ifdef USE_FONT_BACKEND
void
x_set_font_backend (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  if (! NILP (new_value)
      && !CONSP (new_value))
    {
      char *p0, *p1;
	
      CHECK_STRING (new_value);
      p0 = p1 = SDATA (new_value);
      new_value = Qnil;
      while (*p0)
	{
	  while (*p1 && *p1 != ',') p1++;
	  if (p0 < p1)
	    new_value = Fcons (Fintern (make_string (p0, p1 - p0), Qnil),
			       new_value);
	  if (*p1)
	    p1++;
	  p0 = p1;
	}
      new_value = Fnreverse (new_value);
    }

  if (! NILP (old_value) && ! NILP (Fequal (old_value, new_value)))
    return;

  if (FRAME_FONT_OBJECT (f))
    free_all_realized_faces (Qnil);

  new_value = font_update_drivers (f, NILP (new_value) ? Qt : new_value);
  if (NILP (new_value))
    {
      if (NILP (old_value))
	error ("No font backend available");
      font_update_drivers (f, old_value);
      error ("None of specified font backends are available");
    }
  store_frame_param (f, Qfont_backend, new_value);

  if (FRAME_FONT_OBJECT (f))
    {
      Lisp_Object frame;

      XSETFRAME (frame, f);
      x_set_font (f, Fframe_parameter (frame, Qfont), Qnil);
      ++face_change_count;
      ++windows_or_buffers_changed;
    }
}
#endif	/* USE_FONT_BACKEND */


void
x_set_fringe_width (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  compute_fringe_widths (f, 1);
}

void
x_set_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  CHECK_NUMBER (arg);

  if (XINT (arg) == f->border_width)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    error ("Cannot change the border width of a frame");

  f->border_width = XINT (arg);
}

void
x_set_internal_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int old = FRAME_INTERNAL_BORDER_WIDTH (f);

  CHECK_NUMBER (arg);
  FRAME_INTERNAL_BORDER_WIDTH (f) = XINT (arg);
  if (FRAME_INTERNAL_BORDER_WIDTH (f) < 0)
    FRAME_INTERNAL_BORDER_WIDTH (f) = 0;

#ifdef USE_X_TOOLKIT
  if (FRAME_X_OUTPUT (f)->edit_widget)
    widget_store_internal_border (FRAME_X_OUTPUT (f)->edit_widget);
#endif

  if (FRAME_INTERNAL_BORDER_WIDTH (f) == old)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    {
      x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
      SET_FRAME_GARBAGED (f);
      do_pending_window_change (0);
    }
  else
    SET_FRAME_GARBAGED (f);
}

void
x_set_visibility (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  Lisp_Object frame;
  XSETFRAME (frame, f);

  if (NILP (value))
    Fmake_frame_invisible (frame, Qt);
  else if (EQ (value, Qicon))
    Ficonify_frame (frame);
  else
    Fmake_frame_visible (frame);
}

void
x_set_autoraise (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->auto_raise = !EQ (Qnil, arg);
}

void
x_set_autolower (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->auto_lower = !EQ (Qnil, arg);
}

void
x_set_unsplittable (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->no_split = !NILP (arg);
}

void
x_set_vertical_scroll_bars (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  if ((EQ (arg, Qleft) && FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
      || (EQ (arg, Qright) && FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f))
      || (NILP (arg) && FRAME_HAS_VERTICAL_SCROLL_BARS (f))
      || (!NILP (arg) && ! FRAME_HAS_VERTICAL_SCROLL_BARS (f)))
    {
      FRAME_VERTICAL_SCROLL_BAR_TYPE (f)
	= (NILP (arg)
	   ? vertical_scroll_bar_none
	   : EQ (Qleft, arg)
	   ? vertical_scroll_bar_left
	   : EQ (Qright, arg)
	   ? vertical_scroll_bar_right
	   : EQ (Qleft, Vdefault_frame_scroll_bars)
	   ? vertical_scroll_bar_left
	   : EQ (Qright, Vdefault_frame_scroll_bars)
	   ? vertical_scroll_bar_right
	   : vertical_scroll_bar_none);

      /* We set this parameter before creating the X window for the
	 frame, so we can get the geometry right from the start.
	 However, if the window hasn't been created yet, we shouldn't
	 call x_set_window_size.  */
      if (FRAME_X_WINDOW (f))
	x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
      do_pending_window_change (0);
    }
}

void
x_set_scroll_bar_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int wid = FRAME_COLUMN_WIDTH (f);

  if (NILP (arg))
    {
      x_set_scroll_bar_default_width (f);

      if (FRAME_X_WINDOW (f))
        x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
      do_pending_window_change (0);
    }
  else if (INTEGERP (arg) && XINT (arg) > 0
	   && XFASTINT (arg) != FRAME_CONFIG_SCROLL_BAR_WIDTH (f))
    {
      if (XFASTINT (arg) <= 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM)
	XSETINT (arg, 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM + 1);

      FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = XFASTINT (arg);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (XFASTINT (arg) + wid-1) / wid;
      if (FRAME_X_WINDOW (f))
	x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
      do_pending_window_change (0);
    }

  change_frame_size (f, 0, FRAME_COLS (f), 0, 0, 0);
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.hpos = 0;
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.x = 0;
}



/* Return non-nil if frame F wants a bitmap icon.  */

Lisp_Object
x_icon_type (f)
     FRAME_PTR f;
{
  Lisp_Object tem;

  tem = assq_no_quit (Qicon_type, f->param_alist);
  if (CONSP (tem))
    return XCDR (tem);
  else
    return Qnil;
}


/* Subroutines of creating an X frame.  */

/* Make sure that Vx_resource_name is set to a reasonable value.
   Fix it up, or set it to `emacs' if it is too hopeless.  */

void
validate_x_resource_name ()
{
  int len = 0;
  /* Number of valid characters in the resource name.  */
  int good_count = 0;
  /* Number of invalid characters in the resource name.  */
  int bad_count = 0;
  Lisp_Object new;
  int i;

  if (!STRINGP (Vx_resource_class))
    Vx_resource_class = build_string (EMACS_CLASS);

  if (STRINGP (Vx_resource_name))
    {
      unsigned char *p = SDATA (Vx_resource_name);
      int i;

      len = SBYTES (Vx_resource_name);

      /* Only letters, digits, - and _ are valid in resource names.
	 Count the valid characters and count the invalid ones.  */
      for (i = 0; i < len; i++)
	{
	  int c = p[i];
	  if (! ((c >= 'a' && c <= 'z')
		 || (c >= 'A' && c <= 'Z')
		 || (c >= '0' && c <= '9')
		 || c == '-' || c == '_'))
	    bad_count++;
	  else
	    good_count++;
	}
    }
  else
    /* Not a string => completely invalid.  */
    bad_count = 5, good_count = 0;

  /* If name is valid already, return.  */
  if (bad_count == 0)
    return;

  /* If name is entirely invalid, or nearly so, use `emacs'.  */
  if (good_count == 0
      || (good_count == 1 && bad_count > 0))
    {
      Vx_resource_name = build_string ("emacs");
      return;
    }

  /* Name is partly valid.  Copy it and replace the invalid characters
     with underscores.  */

  Vx_resource_name = new = Fcopy_sequence (Vx_resource_name);

  for (i = 0; i < len; i++)
    {
      int c = SREF (new, i);
      if (! ((c >= 'a' && c <= 'z')
	     || (c >= 'A' && c <= 'Z')
	     || (c >= '0' && c <= '9')
	     || c == '-' || c == '_'))
	SSET (new, i, '_');
    }
}


extern char *x_get_string_resource P_ ((XrmDatabase, char *, char *));
extern Display_Info *check_x_display_info P_ ((Lisp_Object));


/* Get specified attribute from resource database RDB.
   See Fx_get_resource below for other parameters.  */

static Lisp_Object
xrdb_get_resource (rdb, attribute, class, component, subclass)
     XrmDatabase rdb;
     Lisp_Object attribute, class, component, subclass;
{
  register char *value;
  char *name_key;
  char *class_key;

  CHECK_STRING (attribute);
  CHECK_STRING (class);

  if (!NILP (component))
    CHECK_STRING (component);
  if (!NILP (subclass))
    CHECK_STRING (subclass);
  if (NILP (component) != NILP (subclass))
    error ("x-get-resource: must specify both COMPONENT and SUBCLASS or neither");

  validate_x_resource_name ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  Make them big enough for the worst case.  */
  name_key = (char *) alloca (SBYTES (Vx_resource_name)
			      + (STRINGP (component)
				 ? SBYTES (component) : 0)
			      + SBYTES (attribute)
			      + 3);

  class_key = (char *) alloca (SBYTES (Vx_resource_class)
			       + SBYTES (class)
			       + (STRINGP (subclass)
				  ? SBYTES (subclass) : 0)
			       + 3);

  /* Start with emacs.FRAMENAME for the name (the specific one)
     and with `Emacs' for the class key (the general one).  */
  strcpy (name_key, SDATA (Vx_resource_name));
  strcpy (class_key, SDATA (Vx_resource_class));

  strcat (class_key, ".");
  strcat (class_key, SDATA (class));

  if (!NILP (component))
    {
      strcat (class_key, ".");
      strcat (class_key, SDATA (subclass));

      strcat (name_key, ".");
      strcat (name_key, SDATA (component));
    }

  strcat (name_key, ".");
  strcat (name_key, SDATA (attribute));

  value = x_get_string_resource (rdb, name_key, class_key);

  if (value != (char *) 0)
    return build_string (value);
  else
    return Qnil;
}


DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 2, 4, 0,
       doc: /* Return the value of ATTRIBUTE, of class CLASS, from the X defaults database.
This uses `INSTANCE.ATTRIBUTE' as the key and `Emacs.CLASS' as the
class, where INSTANCE is the name under which Emacs was invoked, or
the name specified by the `-name' or `-rn' command-line arguments.

The optional arguments COMPONENT and SUBCLASS add to the key and the
class, respectively.  You must specify both of them or neither.
If you specify them, the key is `INSTANCE.COMPONENT.ATTRIBUTE'
and the class is `Emacs.CLASS.SUBCLASS'.  */)
     (attribute, class, component, subclass)
     Lisp_Object attribute, class, component, subclass;
{
#ifdef HAVE_X_WINDOWS
  check_x ();
#endif

  return xrdb_get_resource (check_x_display_info (Qnil)->xrdb,
			    attribute, class, component, subclass);
}

/* Get an X resource, like Fx_get_resource, but for display DPYINFO.  */

Lisp_Object
display_x_get_resource (dpyinfo, attribute, class, component, subclass)
     Display_Info *dpyinfo;
     Lisp_Object attribute, class, component, subclass;
{
  return xrdb_get_resource (dpyinfo->xrdb,
			    attribute, class, component, subclass);
}

/* Used when C code wants a resource value.  */

char *
x_get_resource_string (attribute, class)
     char *attribute, *class;
{
  char *name_key;
  char *class_key;
  struct frame *sf = SELECTED_FRAME ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  */
  name_key = (char *) alloca (SBYTES (Vinvocation_name)
			      + strlen (attribute) + 2);
  class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
			       + strlen (class) + 2);

  sprintf (name_key, "%s.%s", SDATA (Vinvocation_name), attribute);
  sprintf (class_key, "%s.%s", EMACS_CLASS, class);

  return x_get_string_resource (FRAME_X_DISPLAY_INFO (sf)->xrdb,
				name_key, class_key);
}


/* Return the value of parameter PARAM.

   First search ALIST, then Vdefault_frame_alist, then the X defaults
   database, using ATTRIBUTE as the attribute name and CLASS as its class.

   Convert the resource to the type specified by desired_type.

   If no default is specified, return Qunbound.  If you call
   x_get_arg, make sure you deal with Qunbound in a reasonable way,
   and don't let it get stored in any Lisp-visible variables!  */

Lisp_Object
x_get_arg (dpyinfo, alist, param, attribute, class, type)
     Display_Info *dpyinfo;
     Lisp_Object alist, param;
     char *attribute;
     char *class;
     enum resource_types type;
{
  register Lisp_Object tem;

  tem = Fassq (param, alist);

  if (!NILP (tem))
    {
      /* If we find this parm in ALIST, clear it out
	 so that it won't be "left over" at the end.  */
#ifndef WINDOWSNT /* w32fns.c has not yet been changed to cope with this.  */
      Lisp_Object tail;
      XSETCAR (tem, Qnil);
      /* In case the parameter appears more than once in the alist,
	 clear it out.  */
      for (tail = alist; CONSP (tail); tail = XCDR (tail))
	if (CONSP (XCAR (tail))
	    && EQ (XCAR (XCAR (tail)), param))
	  XSETCAR (XCAR (tail), Qnil);
#endif
    }
  else
    tem = Fassq (param, Vdefault_frame_alist);

  /* If it wasn't specified in ALIST or the Lisp-level defaults,
     look in the X resources.  */
  if (EQ (tem, Qnil))
    {
      if (attribute)
	{
	  tem = display_x_get_resource (dpyinfo,
					build_string (attribute),
					build_string (class),
					Qnil, Qnil);

	  if (NILP (tem))
	    return Qunbound;

	  switch (type)
	    {
	    case RES_TYPE_NUMBER:
	      return make_number (atoi (SDATA (tem)));

	    case RES_TYPE_FLOAT:
	      return make_float (atof (SDATA (tem)));

	    case RES_TYPE_BOOLEAN:
	      tem = Fdowncase (tem);
	      if (!strcmp (SDATA (tem), "on")
		  || !strcmp (SDATA (tem), "true"))
		return Qt;
	      else
		return Qnil;

	    case RES_TYPE_STRING:
	      return tem;

	    case RES_TYPE_SYMBOL:
	      /* As a special case, we map the values `true' and `on'
		 to Qt, and `false' and `off' to Qnil.  */
	      {
		Lisp_Object lower;
		lower = Fdowncase (tem);
		if (!strcmp (SDATA (lower), "on")
		    || !strcmp (SDATA (lower), "true"))
		  return Qt;
		else if (!strcmp (SDATA (lower), "off")
		      || !strcmp (SDATA (lower), "false"))
		  return Qnil;
		else
		  return Fintern (tem, Qnil);
	      }

	    default:
	      abort ();
	    }
	}
      else
	return Qunbound;
    }
  return Fcdr (tem);
}

Lisp_Object
x_frame_get_arg (f, alist, param, attribute, class, type)
     struct frame *f;
     Lisp_Object alist, param;
     char *attribute;
     char *class;
     enum resource_types type;
{
  return x_get_arg (FRAME_X_DISPLAY_INFO (f),
		    alist, param, attribute, class, type);
}

/* Like x_frame_get_arg, but also record the value in f->param_alist.  */

Lisp_Object
x_frame_get_and_record_arg (f, alist, param, attribute, class, type)
     struct frame *f;
     Lisp_Object alist, param;
     char *attribute;
     char *class;
     enum resource_types type;
{
  Lisp_Object value;

  value = x_get_arg (FRAME_X_DISPLAY_INFO (f), alist, param,
		     attribute, class, type);
  if (! NILP (value) && ! EQ (value, Qunbound))
    store_frame_param (f, param, value);

  return value;
}


/* Record in frame F the specified or default value according to ALIST
   of the parameter named PROP (a Lisp symbol).
   If no value is specified for PROP, look for an X default for XPROP
   on the frame named NAME.
   If that is not found either, use the value DEFLT.  */

Lisp_Object
x_default_parameter (f, alist, prop, deflt, xprop, xclass, type)
     struct frame *f;
     Lisp_Object alist;
     Lisp_Object prop;
     Lisp_Object deflt;
     char *xprop;
     char *xclass;
     enum resource_types type;
{
  Lisp_Object tem;

  tem = x_frame_get_arg (f, alist, prop, xprop, xclass, type);
  if (EQ (tem, Qunbound))
    tem = deflt;
  x_set_frame_parameters (f, Fcons (Fcons (prop, tem), Qnil));
  return tem;
}




DEFUN ("x-parse-geometry", Fx_parse_geometry, Sx_parse_geometry, 1, 1, 0,
       doc: /* Parse an X-style geometry string STRING.
Returns an alist of the form ((top . TOP), (left . LEFT) ... ).
The properties returned may include `top', `left', `height', and `width'.
The value of `left' or `top' may be an integer,
or a list (+ N) meaning N pixels relative to top/left corner,
or a list (- N) meaning -N pixels relative to bottom/right corner.  */)
     (string)
     Lisp_Object string;
{
  int geometry, x, y;
  unsigned int width, height;
  Lisp_Object result;

  CHECK_STRING (string);

  geometry = XParseGeometry ((char *) SDATA (string),
			     &x, &y, &width, &height);

#if 0
  if (!!(geometry & XValue) != !!(geometry & YValue))
    error ("Must specify both x and y position, or neither");
#endif

  result = Qnil;
  if (geometry & XValue)
    {
      Lisp_Object element;

      if (x >= 0 && (geometry & XNegative))
	element = Fcons (Qleft, Fcons (Qminus, Fcons (make_number (-x), Qnil)));
      else if (x < 0 && ! (geometry & XNegative))
	element = Fcons (Qleft, Fcons (Qplus, Fcons (make_number (x), Qnil)));
      else
	element = Fcons (Qleft, make_number (x));
      result = Fcons (element, result);
    }

  if (geometry & YValue)
    {
      Lisp_Object element;

      if (y >= 0 && (geometry & YNegative))
	element = Fcons (Qtop, Fcons (Qminus, Fcons (make_number (-y), Qnil)));
      else if (y < 0 && ! (geometry & YNegative))
	element = Fcons (Qtop, Fcons (Qplus, Fcons (make_number (y), Qnil)));
      else
	element = Fcons (Qtop, make_number (y));
      result = Fcons (element, result);
    }

  if (geometry & WidthValue)
    result = Fcons (Fcons (Qwidth, make_number (width)), result);
  if (geometry & HeightValue)
    result = Fcons (Fcons (Qheight, make_number (height)), result);

  return result;
}

/* Calculate the desired size and position of frame F.
   Return the flags saying which aspects were specified.

   Also set the win_gravity and size_hint_flags of F.

   Adjust height for toolbar if TOOLBAR_P is 1.

   This function does not make the coordinates positive.  */

#define DEFAULT_ROWS 40
#define DEFAULT_COLS 80

int
x_figure_window_size (f, parms, toolbar_p)
     struct frame *f;
     Lisp_Object parms;
     int toolbar_p;
{
  register Lisp_Object tem0, tem1, tem2;
  long window_prompting = 0;
  Display_Info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  /* Default values if we fall through.
     Actually, if that happens we should get
     window manager prompting.  */
  SET_FRAME_COLS (f, DEFAULT_COLS);
  FRAME_LINES (f) = DEFAULT_ROWS;
  /* Window managers expect that if program-specified
     positions are not (0,0), they're intentional, not defaults.  */
  f->top_pos = 0;
  f->left_pos = 0;

  /* Ensure that old new_text_cols and new_text_lines will not override the
     values set here.  */
  /* ++KFS: This was specific to W32, but seems ok for all platforms */
  f->new_text_cols = f->new_text_lines = 0;

  tem0 = x_get_arg (dpyinfo, parms, Qheight, 0, 0, RES_TYPE_NUMBER);
  tem1 = x_get_arg (dpyinfo, parms, Qwidth, 0, 0, RES_TYPE_NUMBER);
  tem2 = x_get_arg (dpyinfo, parms, Quser_size, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (!EQ (tem0, Qunbound))
	{
	  CHECK_NUMBER (tem0);
	  FRAME_LINES (f) = XINT (tem0);
	}
      if (!EQ (tem1, Qunbound))
	{
	  CHECK_NUMBER (tem1);
	  SET_FRAME_COLS (f, XINT (tem1));
	}
      if (!NILP (tem2) && !EQ (tem2, Qunbound))
	window_prompting |= USSize;
      else
	window_prompting |= PSize;
    }

  f->scroll_bar_actual_width
    = FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f);

  /* This used to be done _before_ calling x_figure_window_size, but
     since the height is reset here, this was really a no-op.  I
     assume that moving it here does what Gerd intended (although he
     no longer can remember what that was...  ++KFS, 2003-03-25.  */

  /* Add the tool-bar height to the initial frame height so that the
     user gets a text display area of the size he specified with -g or
     via .Xdefaults.  Later changes of the tool-bar height don't
     change the frame size.  This is done so that users can create
     tall Emacs frames without having to guess how tall the tool-bar
     will get.  */
  if (toolbar_p && FRAME_TOOL_BAR_LINES (f))
    {
      int margin, relief, bar_height;

      relief = (tool_bar_button_relief >= 0
		? tool_bar_button_relief
		: DEFAULT_TOOL_BAR_BUTTON_RELIEF);

      if (INTEGERP (Vtool_bar_button_margin)
	  && XINT (Vtool_bar_button_margin) > 0)
	margin = XFASTINT (Vtool_bar_button_margin);
      else if (CONSP (Vtool_bar_button_margin)
	       && INTEGERP (XCDR (Vtool_bar_button_margin))
	       && XINT (XCDR (Vtool_bar_button_margin)) > 0)
	margin = XFASTINT (XCDR (Vtool_bar_button_margin));
      else
	margin = 0;

      bar_height = DEFAULT_TOOL_BAR_IMAGE_HEIGHT + 2 * margin + 2 * relief;
      FRAME_LINES (f) += (bar_height + FRAME_LINE_HEIGHT (f) - 1) / FRAME_LINE_HEIGHT (f);
    }

  compute_fringe_widths (f, 0);

  FRAME_PIXEL_WIDTH (f) = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, FRAME_COLS (f));
  FRAME_PIXEL_HEIGHT (f) = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, FRAME_LINES (f));

  tem0 = x_get_arg (dpyinfo, parms, Qtop, 0, 0, RES_TYPE_NUMBER);
  tem1 = x_get_arg (dpyinfo, parms, Qleft, 0, 0, RES_TYPE_NUMBER);
  tem2 = x_get_arg (dpyinfo, parms, Quser_position, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (EQ (tem0, Qminus))
	{
	  f->top_pos = 0;
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qminus)
	       && CONSP (XCDR (tem0))
	       && INTEGERP (XCAR (XCDR (tem0))))
	{
	  f->top_pos = - XINT (XCAR (XCDR (tem0)));
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qplus)
	       && CONSP (XCDR (tem0))
	       && INTEGERP (XCAR (XCDR (tem0))))
	{
	  f->top_pos = XINT (XCAR (XCDR (tem0)));
	}
      else if (EQ (tem0, Qunbound))
	f->top_pos = 0;
      else
	{
	  CHECK_NUMBER (tem0);
	  f->top_pos = XINT (tem0);
	  if (f->top_pos < 0)
	    window_prompting |= YNegative;
	}

      if (EQ (tem1, Qminus))
	{
	  f->left_pos = 0;
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qminus)
	       && CONSP (XCDR (tem1))
	       && INTEGERP (XCAR (XCDR (tem1))))
	{
	  f->left_pos = - XINT (XCAR (XCDR (tem1)));
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qplus)
	       && CONSP (XCDR (tem1))
	       && INTEGERP (XCAR (XCDR (tem1))))
	{
	  f->left_pos = XINT (XCAR (XCDR (tem1)));
	}
      else if (EQ (tem1, Qunbound))
	f->left_pos = 0;
      else
	{
	  CHECK_NUMBER (tem1);
	  f->left_pos = XINT (tem1);
	  if (f->left_pos < 0)
	    window_prompting |= XNegative;
	}

      if (!NILP (tem2) && ! EQ (tem2, Qunbound))
	window_prompting |= USPosition;
      else
	window_prompting |= PPosition;
    }

  if (f->want_fullscreen != FULLSCREEN_NONE)
    {
      int left, top;
      int width, height;

      /* It takes both for some WM:s to place it where we want */
      window_prompting |= USPosition | PPosition;
      x_fullscreen_adjust (f, &width, &height, &top, &left);
      FRAME_COLS (f) = width;
      FRAME_LINES (f) = height;
      FRAME_PIXEL_WIDTH (f) = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, width);
      FRAME_PIXEL_HEIGHT (f) = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, height);
      f->left_pos = left;
      f->top_pos = top;
    }

  if (window_prompting & XNegative)
    {
      if (window_prompting & YNegative)
	f->win_gravity = SouthEastGravity;
      else
	f->win_gravity = NorthEastGravity;
    }
  else
    {
      if (window_prompting & YNegative)
	f->win_gravity = SouthWestGravity;
      else
	f->win_gravity = NorthWestGravity;
    }

  f->size_hint_flags = window_prompting;

  return window_prompting;
}



#endif /* HAVE_WINDOW_SYSTEM */



/***********************************************************************
				Initialization
 ***********************************************************************/

void
syms_of_frame ()
{
  Qframep = intern ("framep");
  staticpro (&Qframep);
  Qframe_live_p = intern ("frame-live-p");
  staticpro (&Qframe_live_p);
  Qexplicit_name = intern ("explicit-name");
  staticpro (&Qexplicit_name);
  Qheight = intern ("height");
  staticpro (&Qheight);
  Qicon = intern ("icon");
  staticpro (&Qicon);
  Qminibuffer = intern ("minibuffer");
  staticpro (&Qminibuffer);
  Qmodeline = intern ("modeline");
  staticpro (&Qmodeline);
  Qonly = intern ("only");
  staticpro (&Qonly);
  Qwidth = intern ("width");
  staticpro (&Qwidth);
  Qgeometry = intern ("geometry");
  staticpro (&Qgeometry);
  Qicon_left = intern ("icon-left");
  staticpro (&Qicon_left);
  Qicon_top = intern ("icon-top");
  staticpro (&Qicon_top);
  Qleft = intern ("left");
  staticpro (&Qleft);
  Qright = intern ("right");
  staticpro (&Qright);
  Quser_position = intern ("user-position");
  staticpro (&Quser_position);
  Quser_size = intern ("user-size");
  staticpro (&Quser_size);
  Qwindow_id = intern ("window-id");
  staticpro (&Qwindow_id);
#ifdef HAVE_X_WINDOWS
  Qouter_window_id = intern ("outer-window-id");
  staticpro (&Qouter_window_id);
#endif
  Qparent_id = intern ("parent-id");
  staticpro (&Qparent_id);
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
  Qburied_buffer_list = intern ("buried-buffer-list");
  staticpro (&Qburied_buffer_list);
  Qdisplay_type = intern ("display-type");
  staticpro (&Qdisplay_type);
  Qbackground_mode = intern ("background-mode");
  staticpro (&Qbackground_mode);
  Qnoelisp = intern ("noelisp");
  staticpro (&Qnoelisp);
  Qtty_color_mode = intern ("tty-color-mode");
  staticpro (&Qtty_color_mode);
  Qtty = intern ("tty");
  staticpro (&Qtty);
  Qtty_type = intern ("tty-type");
  staticpro (&Qtty_type);

  Qface_set_after_frame_default = intern ("face-set-after-frame-default");
  staticpro (&Qface_set_after_frame_default);

  Qinhibit_face_set_after_frame_default
    = intern ("inhibit-face-set-after-frame-default");
  staticpro (&Qinhibit_face_set_after_frame_default);

  Qfullwidth = intern ("fullwidth");
  staticpro (&Qfullwidth);
  Qfullheight = intern ("fullheight");
  staticpro (&Qfullheight);
  Qfullboth = intern ("fullboth");
  staticpro (&Qfullboth);
  Qx_resource_name = intern ("x-resource-name");
  staticpro (&Qx_resource_name);

  Qx_frame_parameter = intern ("x-frame-parameter");
  staticpro (&Qx_frame_parameter);

  Qterminal = intern ("terminal");
  staticpro (&Qterminal);
  Qterminal_live_p = intern ("terminal-live-p");
  staticpro (&Qterminal_live_p);

  {
    int i;

    for (i = 0; i < sizeof (frame_parms) / sizeof (frame_parms[0]); i++)
      {
	Lisp_Object v = intern (frame_parms[i].name);
	if (frame_parms[i].variable)
	  {
	    *frame_parms[i].variable = v;
	    staticpro (frame_parms[i].variable);
	  }
	Fput (v, Qx_frame_parameter, make_number (i));
      }
  }

#ifdef HAVE_WINDOW_SYSTEM
  DEFVAR_LISP ("x-resource-name", &Vx_resource_name,
    doc: /* The name Emacs uses to look up X resources.
`x-get-resource' uses this as the first component of the instance name
when requesting resource values.
Emacs initially sets `x-resource-name' to the name under which Emacs
was invoked, or to the value specified with the `-name' or `-rn'
switches, if present.

It may be useful to bind this variable locally around a call
to `x-get-resource'.  See also the variable `x-resource-class'.  */);
  Vx_resource_name = Qnil;

  DEFVAR_LISP ("x-resource-class", &Vx_resource_class,
    doc: /* The class Emacs uses to look up X resources.
`x-get-resource' uses this as the first component of the instance class
when requesting resource values.

Emacs initially sets `x-resource-class' to "Emacs".

Setting this variable permanently is not a reasonable thing to do,
but binding this variable locally around a call to `x-get-resource'
is a reasonable practice.  See also the variable `x-resource-name'.  */);
  Vx_resource_class = build_string (EMACS_CLASS);
#endif

  DEFVAR_LISP ("default-frame-alist", &Vdefault_frame_alist,
	       doc: /* Alist of default values for frame creation.
These may be set in your init file, like this:
  (setq default-frame-alist '((width . 80) (height . 55) (menu-bar-lines . 1)))
These override values given in window system configuration data,
 including X Windows' defaults database.
For values specific to the first Emacs frame, see `initial-frame-alist'.
For window-system specific values, see `window-system-default-frame-alist'.
For values specific to the separate minibuffer frame, see
 `minibuffer-frame-alist'.
The `menu-bar-lines' element of the list controls whether new frames
 have menu bars; `menu-bar-mode' works by altering this element.
Setting this variable does not affect existing frames, only new ones.  */);
  Vdefault_frame_alist = Qnil;

  DEFVAR_LISP ("default-frame-scroll-bars", &Vdefault_frame_scroll_bars,
	       doc: /* Default position of scroll bars on this window-system.  */);
#ifdef HAVE_WINDOW_SYSTEM
#if defined(HAVE_NTGUI) || defined(MAC_OS)
  /* MS-Windows has scroll bars on the right by default.  */
  Vdefault_frame_scroll_bars = Qright;
#else
  Vdefault_frame_scroll_bars = Qleft;
#endif
#else
  Vdefault_frame_scroll_bars = Qnil;
#endif

  DEFVAR_LISP ("terminal-frame", &Vterminal_frame,
               doc: /* The initial frame-object, which represents Emacs's stdout.  */);

  DEFVAR_LISP ("emacs-iconified", &Vemacs_iconified,
	       doc: /* Non-nil if all of Emacs is iconified and frame updates are not needed.  */);
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

  DEFVAR_LISP ("delete-frame-functions", &Vdelete_frame_functions,
	       doc: /* Functions to be run before deleting a frame.
The functions are run with one arg, the frame to be deleted.
See `delete-frame'.

Note that functions in this list may be called twice on the same
frame.  In the second invocation, the frame is already deleted, and
the function should do nothing.  (You can use `frame-live-p' to check
for this.)  This wrinkle happens when an earlier function in
`delete-frame-functions' (indirectly) calls `delete-frame'
recursively.  */);
  Vdelete_frame_functions = Qnil;

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

  DEFVAR_BOOL ("focus-follows-mouse", &focus_follows_mouse,
	       doc: /* Non-nil if window system changes focus when you move the mouse.
You should set this variable to tell Emacs how your window manager
handles focus, since there is no way in general for Emacs to find out
automatically.  */);
#ifdef HAVE_WINDOW_SYSTEM
#if defined(HAVE_NTGUI) || defined(MAC_OS)
  focus_follows_mouse = 0;
#else
  focus_follows_mouse = 1;
#endif
#else
  focus_follows_mouse = 0;
#endif

  staticpro (&Vframe_list);

  defsubr (&Sactive_minibuffer_window);
  defsubr (&Sframep);
  defsubr (&Sframe_live_p);
  defsubr (&Swindow_system);
  defsubr (&Smake_terminal_frame);
  defsubr (&Shandle_switch_frame);
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

#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Sx_get_resource);
  defsubr (&Sx_parse_geometry);
#endif

}

/* arch-tag: 7dbf2c69-9aad-45f8-8296-db893d6dd039
   (do not change this comment) */
