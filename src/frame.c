/* Generic screen functions.
   Copyright (C) 1989 Free Software Foundation.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>

#include "config.h"
#include "lisp.h"
#include "screen.h"
#include "window.h"
#include "termhooks.h"

Lisp_Object Vemacs_iconified;
Lisp_Object Qscreenp;
Lisp_Object Qlive_screen_p;
Lisp_Object Vscreen_list;
Lisp_Object Vterminal_screen;
Lisp_Object Vdefault_minibuffer_screen;
Lisp_Object Vdefault_screen_alist;
Lisp_Object Qminibuffer;

/* A screen which is not just a minibuffer, or 0 if there are no
   such screens.  This is usually the most recent such screen that
   was selected.  */
struct screen *last_nonminibuf_screen;

extern Lisp_Object Vminibuffer_list;
extern Lisp_Object get_minibuffer ();

DEFUN ("screenp", Fscreenp, Sscreenp, 1, 1, 0,
  "Return non-nil if OBJECT is a screen.\n\
Value is t for a termcap screen (a character-only terminal),\n\
`x' for an Emacs screen that is really an X window.\n\
Also see live-screen-p.")
  (object)
     Lisp_Object object;
{
  if (XTYPE (object) != Lisp_Screen)
    return Qnil;
  switch (XSCREEN (object)->output_method)
    {
    case output_termcap:
      return Qt;
    case output_x_window:
      return intern ("x");
    default:
      abort ();
    }
}

DEFUN ("live-screen-p", Flive_screen_p, Slive_screen_p, 1, 1, 0,
  "Return non-nil if OBJECT is a screen which has not been deleted.\n\
Value is nil if OBJECT is not a live screen.  If object is a live\n\
screen, the return value indicates what sort of output device it is\n\
displayed on.  Value is t for a termcap screen (a character-only\n\
terminal), `x' for an Emacs screen being displayed in an X window.")
  (object)
     Lisp_Object object;
{
  return ((SCREENP (object)
	   && SCREEN_LIVE_P (XSCREEN (object)))
	  ? Fscreenp (object)
	  : Qnil);
}

struct screen *
make_screen (mini_p)
     int mini_p;
{
  Lisp_Object screen;
  register struct screen *s;
  register Lisp_Object root_window;
  register Lisp_Object mini_window;

  screen = Fmake_vector (((sizeof (struct screen) - (sizeof (Lisp_Vector)
						     - sizeof (Lisp_Object)))
			  / sizeof (Lisp_Object)),
			 make_number (0));
  XSETTYPE (screen, Lisp_Screen);
  s = XSCREEN (screen);

  s->cursor_x = 0;
  s->cursor_y = 0;
  s->current_glyphs = 0;
  s->desired_glyphs = 0;
  s->visible = 0;
  s->display.nothing = 0;
  s->iconified = 0;
  s->wants_modeline = 1;
  s->auto_raise = 0;
  s->auto_lower = 0;
  s->no_split = 0;
  s->garbaged = 0;
  s->has_minibuffer = mini_p;
  s->focus_screen = screen;

  s->param_alist = Qnil;

  root_window = make_window (0);
  if (mini_p)
    {
      mini_window = make_window (0);
      XWINDOW (root_window)->next = mini_window;
      XWINDOW (mini_window)->prev = root_window;
      XWINDOW (mini_window)->mini_p = Qt;
      XWINDOW (mini_window)->screen = screen;
      s->minibuffer_window = mini_window;
    }
  else
    {
      mini_window = Qnil;
      XWINDOW (root_window)->next = Qnil;
      s->minibuffer_window = Qnil;
    }

  XWINDOW (root_window)->screen = screen;

  /* 10 is arbitrary,
     just so that there is "something there."
     Correct size will be set up later with change_screen_size.  */

  s->width = 10;
  s->height = 10;

  XFASTINT (XWINDOW (root_window)->width) = 10;
  XFASTINT (XWINDOW (root_window)->height) = (mini_p ? 9 : 10);

  if (mini_p)
    {
      XFASTINT (XWINDOW (mini_window)->width) = 10;
      XFASTINT (XWINDOW (mini_window)->top) = 9;
      XFASTINT (XWINDOW (mini_window)->height) = 1;
    }

  /* Choose a buffer for the screen's root window.  */
  {
    Lisp_Object buf;

    XWINDOW (root_window)->buffer = Qt;
    buf = Fcurrent_buffer ();
    /* If buf is a 'hidden' buffer (i.e. one whose name starts with
       a space), try to find another one.  */
    if (XSTRING (Fbuffer_name (buf))->data[0] == ' ')
      buf = Fother_buffer (buf);
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

  s->root_window = root_window;
  s->selected_window = root_window;
  /* Make sure this window seems more recently used than
     a newly-created, never-selected window.  */
  XFASTINT (XWINDOW (s->selected_window)->use_time) = ++window_select_count;

  Vscreen_list = Fcons (screen, Vscreen_list);

  return s;
}

/* Make a screen using a separate minibuffer window on another screen.
   MINI_WINDOW is the minibuffer window to use.  nil means use the
   default (the global minibuffer).  */

struct screen *
make_screen_without_minibuffer (mini_window)
     register Lisp_Object mini_window;
{
  register struct screen *s;

  /* Choose the minibuffer window to use.  */
  if (NILP (mini_window))
    {
      if (XTYPE (Vdefault_minibuffer_screen) != Lisp_Screen)
	error ("default-minibuffer-screen must be set when creating minibufferless screens.");
      mini_window = XSCREEN (Vdefault_minibuffer_screen)->minibuffer_window;
    }
  else
    {
      CHECK_WINDOW (mini_window, 0);
    }

  /* Make a screen containing just a root window.  */
  s = make_screen (0);

  /* Install the chosen minibuffer window, with proper buffer.  */
  s->minibuffer_window = mini_window;
  Fset_window_buffer (mini_window,
		      (NILP (Vminibuffer_list)
		       ? get_minibuffer (0)
		       : Fcar (Vminibuffer_list)));
  return s;
}

/* Make a screen containing only a minibuffer window.  */

struct screen *
make_minibuffer_screen ()
{
  /* First make a screen containing just a root window, no minibuffer.  */

  register struct screen *s = make_screen (0);
  register Lisp_Object mini_window;
  register Lisp_Object screen;

  XSET (screen, Lisp_Screen, s);

  /* ??? Perhaps leave it to the user program to set auto_raise.  */
  s->auto_raise = 1;
  s->auto_lower = 0;
  s->no_split = 1;
  s->wants_modeline = 0;
  s->has_minibuffer = 1;

  /* Now label the root window as also being the minibuffer.
     Avoid infinite looping on the window chain by marking next pointer
     as nil. */

  mini_window = s->minibuffer_window = s->root_window;
  XWINDOW (mini_window)->mini_p = Qt;
  XWINDOW (mini_window)->next = Qnil;
  XWINDOW (mini_window)->prev = mini_window;
  XWINDOW (mini_window)->screen = screen;

  /* Put the proper buffer in that window.  */

  Fset_window_buffer (mini_window,
		      (NILP (Vminibuffer_list)
		       ? get_minibuffer (0)
		       : Fcar (Vminibuffer_list)));
  return s;
}

/* Construct a screen that refers to the terminal (stdin and stdout).  */

struct screen *
make_terminal_screen ()
{
  register struct screen *s;

  Vscreen_list = Qnil;
  s = make_screen (1);
  s->name = build_string ("terminal");
  s->visible = 1;
  s->display.nothing = 1;   /* Nonzero means screen isn't deleted.  */
  XSET (Vterminal_screen, Lisp_Screen, s);
  return s;
}

DEFUN ("select-screen", Fselect_screen, Sselect_screen, 1, 2, 0,
  "Select the screen S.  S's selected window becomes \"the\"\n\
selected window.  If the optional parameter NO-ENTER is non-nil, don't\n\
focus on that screen.")
  (screen, no_enter)
     Lisp_Object screen, no_enter;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (selected_screen == XSCREEN (screen))
    return screen;

  selected_screen = XSCREEN (screen);
  if (! SCREEN_MINIBUF_ONLY_P (selected_screen))
    last_nonminibuf_screen = selected_screen;

  Fselect_window (XSCREEN (screen)->selected_window);

#ifdef HAVE_X_WINDOWS
#ifdef MULTI_SCREEN
  if (SCREEN_IS_X (XSCREEN (screen))
      && NILP (no_enter))
    {
      Ffocus_screen (screen);
    }
#endif
#endif
  choose_minibuf_screen ();

  return screen;
}

DEFUN ("selected-screen", Fselected_screen, Sselected_screen, 0, 0, 0,
  "Return the screen that is now selected.")
  ()
{
  Lisp_Object tem;
  XSET (tem, Lisp_Screen, selected_screen);
  return tem;
}

DEFUN ("window-screen", Fwindow_screen, Swindow_screen, 1, 1, 0,
  "Return the screen object that window WINDOW is on.")
  (window)
     Lisp_Object window;
{
  CHECK_WINDOW (window, 0);
  return XWINDOW (window)->screen;
}

DEFUN ("screen-root-window", Fscreen_root_window, Sscreen_root_window, 0, 1, 0,
       "Returns the root-window of SCREEN.")
  (screen)
     Lisp_Object screen;
{
  if (NILP (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  else
    CHECK_LIVE_SCREEN (screen, 0);

  return XSCREEN (screen)->root_window;
}

DEFUN ("screen-selected-window", Fscreen_selected_window,
       Sscreen_selected_window, 0, 1, 0,
  "Return the selected window of screen object SCREEN.")
  (screen)
     Lisp_Object screen;
{
  if (NILP (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  else
    CHECK_LIVE_SCREEN (screen, 0);

  return XSCREEN (screen)->selected_window;
}

DEFUN ("screen-list", Fscreen_list, Sscreen_list,
       0, 0, 0,
       "Return a list of all screens.")
  ()
{
  return Fcopy_sequence (Vscreen_list);
}

#ifdef MULTI_SCREEN

/* Return the next screen in the screen list after SCREEN.
   If MINIBUF is non-nil, include all screens.
   If MINIBUF is nil, exclude minibuffer-only screens.
   If MINIBUF is a window, include only screens using that window for
   their minibuffer.  */
Lisp_Object
next_screen (screen, minibuf)
     Lisp_Object screen;
     Lisp_Object minibuf;
{
  Lisp_Object tail;
  int passed = 0;

  /* There must always be at least one screen in Vscreen_list.  */
  if (! CONSP (Vscreen_list))
    abort ();

  while (1)
    for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	if (passed)
	  {
	    Lisp_Object s = XCONS (tail)->car;

	    /* Decide whether this screen is eligible to be returned,
	       according to minibuf.  */
	    if ((NILP (minibuf) && ! SCREEN_MINIBUF_ONLY_P (XSCREEN (s)))
		|| XTYPE (minibuf) != Lisp_Window
		|| EQ (SCREEN_MINIBUF_WINDOW (XSCREEN (s)), minibuf)
		|| EQ (s, screen))
	      return s;
	  }

	if (EQ (screen, XCONS (tail)->car))
	  passed++;
      }
}

/* Return the previous screen in the screen list before SCREEN.
   If MINIBUF is non-nil, include all screens.
   If MINIBUF is nil, exclude minibuffer-only screens.
   If MINIBUF is a window, include only screens using that window for
   their minibuffer.  */
Lisp_Object
prev_screen (screen, minibuf)
     Lisp_Object screen;
     Lisp_Object minibuf;
{
  Lisp_Object tail;
  Lisp_Object prev;

  /* There must always be at least one screen in Vscreen_list.  */
  if (! CONSP (Vscreen_list))
    abort ();

  prev = Qnil;
  while (1)
    {
      for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  Lisp_Object scr = XCONS (tail)->car;

	  if (XTYPE (scr) != Lisp_Screen)
	    abort ();

	  if (EQ (screen, scr) && !NILP (prev))
	    return prev;

	  /* Decide whether this screen is eligible to be returned,
	     according to minibuf.  */
	  if ((NILP (minibuf) && ! SCREEN_MINIBUF_ONLY_P (XSCREEN (scr)))
	      || XTYPE (minibuf) != Lisp_Window
	      || EQ (SCREEN_MINIBUF_WINDOW (XSCREEN (scr)), minibuf))
	    prev = scr;
	}

      if (NILP (prev))
	/* We went through the whole screen list without finding a single
	   acceptable screen.  Return the original screen.  */
	prev = screen;
    }
	  
}

DEFUN ("next-screen", Fnext_screen, Snext_screen, 0, 2, 0,
  "Return the next screen in the screen list after SCREEN.\n\
If optional argument MINIBUF is non-nil, include all screens.  If\n\
MINIBUF is nil or omitted, exclude minibuffer-only screens.  If\n\
MINIBUF is a window, include only screens using that window for their\n\
minibuffer.")
  (screen, miniscreen)
Lisp_Object screen, miniscreen;
{
  Lisp_Object tail;

  if (NILP (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  else
    CHECK_LIVE_SCREEN (screen, 0);

  return next_screen (screen, miniscreen);
}
#endif /* MULTI_SCREEN */

DEFUN ("delete-screen", Fdelete_screen, Sdelete_screen, 0, 1, "",
  "Delete SCREEN, permanently eliminating it from use.\n\
If omitted, SCREEN defaults to the selected screen.\n\
A screen may not be deleted if its minibuffer is used by other screens.")
  (screen)
     Lisp_Object screen;
{
  struct screen *s;
  union display displ;

  if (EQ (screen, Qnil))
    {
      s = selected_screen;
      XSET (screen, Lisp_Screen, s);
    }
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (! SCREEN_LIVE_P (s))
    return;

  /* Are there any other screens besides this one?  */
  if (s == selected_screen && EQ (next_screen (screen, Qt), screen))
    error ("Attempt to delete the only screen");

  /* Does this screen have a minibuffer, and is it the surrogate
     minibuffer for any other screen?  */
  if (SCREEN_HAS_MINIBUF (XSCREEN (screen)))
    {
      Lisp_Object screen2;

      for (screen2 = Vscreen_list; CONSP (2); screen2 = XCONS (screen2)->cdr)
	if (! EQ (screen2, screen)
	    && EQ (screen,
		   (WINDOW_SCREEN
		    (XWINDOW
		     (SCREEN_MINIBUF_WINDOW
		      (XSCREEN (screen2)))))))
	  error ("Attempt to delete a surrogate minibuffer screen");
    }

  /* Don't let the screen remain selected.  */
  if (s == selected_screen)
    Fselect_screen (next_screen (screen, Qt));

  /* Don't allow minibuf_window to remain on a deleted screen.  */
  if (EQ (s->minibuffer_window, minibuf_window))
    {
      Fset_window_buffer (selected_screen->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_screen->minibuffer_window;
    }

  Vscreen_list = Fdelq (screen, Vscreen_list);
  s->visible = 0;
  displ = s->display;
  s->display.nothing = 0;

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (s))
    x_destroy_window (s, displ);
#endif

  /* If we've deleted the last_nonminibuf_screen, then try to find
     another one.  */
  if (s == last_nonminibuf_screen)
    {
      last_nonminibuf_screen = 0;

      for (screen = Vscreen_list; CONSP (screen); screen = XCONS (screen)->cdr)
	{
	  s = XSCREEN (XCONS (screen)->car);
	  if (!SCREEN_MINIBUF_ONLY_P (s))
	    {
	      last_nonminibuf_screen = s;
	      break;
	    }
	}
    }

  return Qnil;
}

/* Return mouse position in character cell units.  */

DEFUN ("mouse-position", Fmouse_position, Smouse_position, 0, 0, 0,
  "Return a list (SCREEN X . Y) giving the current mouse screen and position.\n\
If Emacs is running on a mouseless terminal or hasn't been programmed\n\
to read the mouse position, it returns the selected screen for SCREEN\n\
and nil for X and Y.")
  ()
{
  Lisp_Object x, y, dummy;
  SCREEN_PTR s;

  if (mouse_position_hook)
    (*mouse_position_hook) (&s, &x, &y, &dummy);
  else
    {
      s = selected_screen;
      x = y = Qnil;
    }

  XSET (dummy, Lisp_Screen, s);
  return Fcons (dummy, Fcons (make_number (x), make_number (y)));
}

DEFUN ("set-mouse-position", Fset_mouse_position, Sset_mouse_position, 3, 3, 0,
  "Move the mouse pointer to the center of cell (X,Y) in SCREEN.\n\
WARNING:  If you use this under X, you should do unfocus-screen afterwards.")
  (screen, x, y)
     Lisp_Object screen, x, y;
{
  CHECK_LIVE_SCREEN (screen, 0);
  CHECK_NUMBER (x, 2);
  CHECK_NUMBER (y, 1);

#ifdef HAVE_X_WINDOWS
  if (SCREEN_IS_X (XSCREEN (screen)))
    /* Warping the mouse will cause  enternotify and focus events. */
    x_set_mouse_position (XSCREEN (screen), x, y);
#endif

  return Qnil;
}

#if 0
/* ??? Can this be replaced with a Lisp function?
   It is used in minibuf.c.  Can we get rid of that?  */

DEFUN ("screen-configuration", Fscreen_configuration, Sscreen_configuration,
       0, 0, 0,
  "Return object describing current screen configuration.\n\
The screen configuration is the current mouse position and selected screen.\n\
This object can be given to `restore-screen-configuration'\n\
to restore this screen configuration.")
  ()
{
  Lisp_Object c, time;
  
  c = Fmake_vector (make_number(4), Qnil);
  XVECTOR (c)->contents[0] = Fselected_screen();
  if (mouse_position_hook)
    (*mouse_position_hook) (&XVECTOR (c)->contents[1]
			    &XVECTOR (c)->contents[2],
			    &XVECTOR (c)->contents[3],
			    &time);
  return c;
}

DEFUN ("restore-screen-configuration", Frestore_screen_configuration,
       Srestore_screen_configuration,
       1, 1, 0,
  "Restores screen configuration CONFIGURATION.")
  (config)
  Lisp_Object config;
{
  Lisp_Object x_pos, y_pos, screen;

  CHECK_VECTOR (config, 0);
  if (XVECTOR (config)->size != 3)
    {
      error ("Wrong size vector passed to restore-screen-configuration");
    }
  screen = XVECTOR (config)->contents[0];
  CHECK_LIVE_SCREEN (screen, 0);

  Fselect_screen (screen, Qnil);

#if 0
  /* This seems to interfere with the screen selection mechanism. jla */
  x_pos = XVECTOR (config)->contents[2];
  y_pos = XVECTOR (config)->contents[3];
  set_mouse_position (screen, XINT (x_pos), XINT (y_pos));
#endif

  return screen;
}    
#endif

DEFUN ("make-screen-visible", Fmake_screen_visible, Smake_screen_visible,
       1, 1, 0,
  "Make the screen SCREEN visible (assuming it is an X-window).\n\
Also raises the screen so that nothing obscures it.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
    x_make_screen_visible (XSCREEN (screen));

  return screen;
}

DEFUN ("make-screen-invisible", Fmake_screen_invisible, Smake_screen_invisible,
       1, 1, 0,
  "Make the screen SCREEN invisible (assuming it is an X-window).")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
    x_make_screen_invisible (XSCREEN (screen));

  return Qnil;
}

DEFUN ("iconify-screen", Ficonify_screen, Siconify_screen,
       1, 1, 0,
  "Make the screen SCREEN into an icon.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
      x_iconify_screen (XSCREEN (screen));

  return Qnil;
}

DEFUN ("deiconify-screen", Fdeiconify_screen, Sdeiconify_screen,
       1, 1, 0,
  "Open (de-iconify) the iconified screen SCREEN.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (SCREEN_IS_X (XSCREEN (screen)))
      x_make_screen_visible (XSCREEN (screen));

  return screen;
}

DEFUN ("screen-visible-p", Fscreen_visible_p, Sscreen_visible_p,
       1, 1, 0,
       "Return t if SCREEN is now \"visible\" (actually in use for display).\n\
A screen that is not \"visible\" is not updated and, if it works through\n\
a window system, it may not show at all.\n\
Return the symbol `icon' if window is visible only as an icon.")
  (screen)
     Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (XSCREEN (screen)->visible)
    return Qt;
  if (XSCREEN (screen)->iconified)
    return intern ("icon");
  return Qnil;
}

DEFUN ("visible-screen-list", Fvisible_screen_list, Svisible_screen_list,
       0, 0, 0,
       "Return a list of all screens now \"visible\" (being updated).")
  ()
{
  Lisp_Object tail, screen;
  struct screen *s;
  Lisp_Object value;

  value = Qnil;
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      screen = XCONS (tail)->car;
      if (XTYPE (screen) != Lisp_Screen)
	continue;
      s = XSCREEN (screen);
      if (s->visible)
	value = Fcons (screen, value);
    }
  return value;
}



DEFUN ("redirect-screen-focus", Fredirect_screen_focus, Sredirect_screen_focus,
       1, 2, 0,
  "Arrange for keystrokes typed at SCREEN to be sent to FOCUS-SCREEN.\n\
This means that, after reading a keystroke typed at SCREEN,\n\
last-event-screen will be FOCUS-SCREEN.\n\
\n\
If FOCUS-SCREEN is omitted or eq to SCREEN, any existing redirection is\n\
cancelled, and the screen again receives its own keystrokes.\n\
\n\
The redirection lasts until the next call to redirect-screen-focus\n\
or select-screen.\n\
\n\
This is useful for temporarily redirecting keystrokes to the minibuffer\n\
window when a screen doesn't have its own minibuffer.")
  (screen, focus_screen)
    Lisp_Object screen, focus_screen;
{
  CHECK_LIVE_SCREEN (screen, 0);

  if (NILP (focus_screen))
    focus_screen = screen;
  else
    CHECK_LIVE_SCREEN (focus_screen, 1);

  XSCREEN (screen)->focus_screen = focus_screen;

  if (screen_rehighlight_hook)
    (*screen_rehighlight_hook) ();
  
  return Qnil;
}


DEFUN ("screen-focus", Fscreen_focus, Sscreen_focus, 1, 1, 0,
  "Return the screen to which SCREEN's keystrokes are currently being sent.\n\
See redirect-screen-focus.")
  (screen)
    Lisp_Object screen;
{
  CHECK_LIVE_SCREEN (screen, 0);
  return SCREEN_FOCUS_SCREEN (XSCREEN (screen));
}



Lisp_Object
get_screen_param (screen, prop)
     register struct screen *screen;
     Lisp_Object prop;
{
  register Lisp_Object tem;

  tem = Fassq (prop, screen->param_alist);
  if (EQ (tem, Qnil))
    return tem;
  return Fcdr (tem);
}

void
store_in_alist (alistptr, propname, val)
     Lisp_Object *alistptr, val;
     char *propname;
{
  register Lisp_Object tem;
  register Lisp_Object prop;

  prop = intern (propname);
  tem = Fassq (prop, *alistptr);
  if (EQ (tem, Qnil))
    *alistptr = Fcons (Fcons (prop, val), *alistptr);
  else
    Fsetcdr (tem, val);
}

void
store_screen_param (s, prop, val)
     struct screen *s;
     Lisp_Object prop, val;
{
  register Lisp_Object tem;

  tem = Fassq (prop, s->param_alist);
  if (EQ (tem, Qnil))
    s->param_alist = Fcons (Fcons (prop, val), s->param_alist);
  else
    Fsetcdr (tem, val);

  if (EQ (prop, Qminibuffer)
      && XTYPE (val) == Lisp_Window)
    {
      if (! MINI_WINDOW_P (XWINDOW (val)))
	error ("Surrogate minibuffer windows must be minibuffer windows.");

      if (SCREEN_HAS_MINIBUF (s) || SCREEN_MINIBUF_ONLY_P (s))
	error ("Can't change surrogate minibuffer on screens with their own minibuffers.");

      /* Install the chosen minibuffer window, with proper buffer.  */
      s->minibuffer_window = val;
    }
}

DEFUN ("screen-parameters", Fscreen_parameters, Sscreen_parameters, 0, 1, 0,
  "Return the parameters-alist of screen SCREEN.\n\
It is a list of elements of the form (PARM . VALUE), where PARM is a symbol.\n\
The meaningful PARMs depend on the kind of screen.")
  (screen)
     Lisp_Object screen;
{
  Lisp_Object alist;
  struct screen *s;

  if (EQ (screen, Qnil))
    s = selected_screen;
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (s->display.nothing == 0)
    return Qnil;

  alist = Fcopy_alist (s->param_alist);
  store_in_alist (&alist, "name", s->name);
  store_in_alist (&alist, "height", make_number (s->height));
  store_in_alist (&alist, "width", make_number (s->width));
  store_in_alist (&alist, "modeline", (s->wants_modeline ? Qt : Qnil));
  store_in_alist (&alist, "minibuffer",
		  (SCREEN_HAS_MINIBUF (s)
		   ? (SCREEN_MINIBUF_ONLY_P (s) ? intern ("only") : Qt)
		   : SCREEN_MINIBUF_WINDOW (s)));
  store_in_alist (&alist, "unsplittable", (s->no_split ? Qt : Qnil));

  if (SCREEN_IS_X (s))
    x_report_screen_params (s, &alist);
  return alist;
}

DEFUN ("modify-screen-parameters", Fmodify_screen_parameters, 
       Smodify_screen_parameters, 2, 2, 0,
  "Modify the parameters of screen SCREEN according to ALIST.\n\
ALIST is an alist of parameters to change and their new values.\n\
Each element of ALIST has the form (PARM . VALUE), where PARM is a symbol.\n\
The meaningful PARMs depend on the kind of screen; undefined PARMs are ignored.")
  (screen, alist)
     Lisp_Object screen, alist;
{
  register struct screen *s;
  register Lisp_Object tail, elt, prop, val;

  if (EQ (screen, Qnil))
    s = selected_screen;
  else
    {
      CHECK_LIVE_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (SCREEN_IS_X (s))
    for (tail = alist; !EQ (tail, Qnil); tail = Fcdr (tail))
      {
	elt = Fcar (tail);
	prop = Fcar (elt);
	val = Fcdr (elt);
	x_set_screen_param (s, prop, val,
			    get_screen_param (s, prop));
	store_screen_param (s, prop, val);
      }

  return Qnil;
}


DEFUN ("screen-pixel-size", Fscreen_pixel_size, 
       Sscreen_pixel_size, 1, 1, 0,
  "Return a cons (width . height) of SCREEN's size in pixels.")
  (screen)
     Lisp_Object screen;
{
  register struct screen *s;
  int width, height;

  CHECK_LIVE_SCREEN (screen, 0);
  s = XSCREEN (screen);
  
  return Fcons (make_number (x_pixel_width (s)),
		make_number (x_pixel_height (s)));
}

DEFUN ("screen-height", Fscreen_height, Sscreen_height, 0, 0, 0,
  "Return number of lines available for display on selected screen.")
  ()
{
  return make_number (SCREEN_HEIGHT (selected_screen));
}

DEFUN ("screen-width", Fscreen_width, Sscreen_width, 0, 0, 0,
  "Return number of columns available for display on selected screen.")
  ()
{
  return make_number (SCREEN_WIDTH (selected_screen));
}

DEFUN ("set-screen-height", Fset_screen_height, Sset_screen_height, 2, 3, 0,
  "Specify that the screen SCREEN has LINES lines.\n\
Optional third arg non-nil means that redisplay should use LINES lines\n\
but that the idea of the actual height of the screen should not be changed.")
  (screen, rows, pretend)
     Lisp_Object rows, pretend;
{
  register struct screen *s;

  CHECK_NUMBER (rows, 0);
  if (NILP (screen))
    s = selected_screen;
  else
    {
      CHECK_LIVE_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (SCREEN_IS_X (s))
    {
      if (XINT (rows) != s->width)
	x_set_window_size (s, s->width, XINT (rows));
    }
  else
    change_screen_size (s, XINT (rows), 0, !NILP (pretend));
  return Qnil;
}

DEFUN ("set-screen-width", Fset_screen_width, Sset_screen_width, 2, 3, 0,
  "Specify that the screen SCREEN has COLS columns.\n\
Optional third arg non-nil means that redisplay should use COLS columns\n\
but that the idea of the actual width of the screen should not be changed.")
  (screen, cols, pretend)
     Lisp_Object cols, pretend;
{
  register struct screen *s;
  CHECK_NUMBER (cols, 0);
  if (NILP (screen))
    s = selected_screen;
  else
    {
      CHECK_LIVE_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (SCREEN_IS_X (s))
    {
      if (XINT (cols) != s->width)
	x_set_window_size (s, XINT (cols), s->height);
    }
  else
    change_screen_size (selected_screen, 0, XINT (cols), !NILP (pretend));
  return Qnil;
}

DEFUN ("set-screen-size", Fset_screen_size, Sset_screen_size, 3, 3, 0,
  "Sets size of SCREEN to COLS by ROWS, measured in characters.")
  (screen, cols, rows)
     Lisp_Object screen, cols, rows;
{
  register struct screen *s;
  int mask;

  CHECK_LIVE_SCREEN (screen, 0);
  CHECK_NUMBER (cols, 2);
  CHECK_NUMBER (rows, 1);
  s = XSCREEN (screen);

  if (SCREEN_IS_X (s))
    {
      if (XINT (rows) != s->height || XINT (cols) != s->width)
	x_set_window_size (s, XINT (cols), XINT (rows));
    }
  else
    change_screen_size (s, XINT (rows), XINT (cols), 0);

  return Qnil;
}

DEFUN ("set-screen-position", Fset_screen_position, 
       Sset_screen_position, 3, 3, 0,
  "Sets position of SCREEN in pixels to XOFFSET by YOFFSET.\n\
If XOFFSET or YOFFSET are negative, they are interpreted relative to\n\
the leftmost or bottommost position SCREEN could occupy without going\n\
off the screen.")
  (screen, xoffset, yoffset)
     Lisp_Object screen, xoffset, yoffset;
{
  register struct screen *s;
  int mask;

  CHECK_LIVE_SCREEN (screen, 0);
  CHECK_NUMBER (xoffset, 1);
  CHECK_NUMBER (yoffset, 2);
  s = XSCREEN (screen);

  if (SCREEN_IS_X (s))
    x_set_offset (s, XINT (xoffset), XINT (yoffset));

  return Qt;
}

#ifndef HAVE_X11
DEFUN ("rubber-band-rectangle", Frubber_band_rectangle, Srubber_band_rectangle,
       3, 3, "",
  "Ask user to specify a window position and size on SCREEN with the mouse.\n\
Arguments are SCREEN, NAME and GEO.  NAME is a name to be displayed as\n\
the purpose of this rectangle.  GEO is an X-windows size spec that can\n\
specify defaults for some sizes/positions.  If GEO specifies everything,\n\
the mouse is not used.\n\
Returns a list of five values: (SCREEN LEFT TOP WIDTH HEIGHT).")
  (screen, name, geo)
     Lisp_Object screen;
     Lisp_Object name;
     Lisp_Object geo;
{
  int vals[4];
  Lisp_Object nums[4];
  int i;

  CHECK_SCREEN (screen, 0);
  CHECK_STRING (name, 1);
  CHECK_STRING (geo, 2);

  switch (XSCREEN (screen)->output_method)
    {
    case output_x_window:
      x_rubber_band (XSCREEN (screen), &vals[0], &vals[1], &vals[2], &vals[3],
		     XSTRING (geo)->data, XSTRING (name)->data);
      break;

    default:
      return Qnil;
    }

  for (i = 0; i < 4; i++)
    XFASTINT (nums[i]) = vals[i];
  return Fcons (screen, Flist (4, nums));
  return Qnil;
}
#endif /* not HAVE_X11 */

choose_minibuf_screen ()
{
  /* For lowest-level minibuf, put it on currently selected screen
     if screen has a minibuffer.  */
  if (minibuf_level == 0
      && selected_screen != 0
      && !EQ (minibuf_window, selected_screen->minibuffer_window)
      && !EQ (Qnil, selected_screen->minibuffer_window))
    {
      Fset_window_buffer (selected_screen->minibuffer_window,
			  XWINDOW (minibuf_window)->buffer);
      minibuf_window = selected_screen->minibuffer_window;
    }
}

syms_of_screen ()
{
  Qscreenp = intern ("screenp");
  Qlive_screen_p = intern ("live_screen_p");
  Qminibuffer = intern ("minibuffer");

  staticpro (&Qscreenp);
  staticpro (&Qlive_screen_p);
  staticpro (&Qminibuffer);

  staticpro (&Vscreen_list);

  DEFVAR_LISP ("terminal-screen", &Vterminal_screen,
    "The initial screen-object, which represents Emacs's stdout.");

  DEFVAR_LISP ("emacs-iconified", &Vemacs_iconified,
    "Non-nil if all of emacs is iconified and screen updates are not needed.");
  Vemacs_iconified = Qnil;

  DEFVAR_LISP ("default-minibuffer-screen", &Vdefault_minibuffer_screen,
    "Minibufferless screens use this screen's minibuffer.\n\
\n\
Emacs cannot create minibufferless screens unless this is set to an\n\
appropriate surrogate.\n\
\n\
Emacs consults this variable only when creating minibufferless\n\
screens; once the screen is created, it sticks with its assigned\n\
minibuffer, no matter what this variable is set to.  This means that\n\
this variable doesn't necessarily say anything meaningful about the\n\
current set of screens, or where the minibuffer is currently being\n\
displayed.");
  Vdefault_minibuffer_screen = Qnil;

  DEFVAR_LISP ("default-screen-alist", &Vdefault_screen_alist,
    "Alist of default values for screen creation.\n\
These may be set in your init file, like this:\n\
  (setq default-screen-alist '((width . 80) (height . 55)))\n\
These override values given in window system configuration data, like\n\
X Windows' defaults database.\n\
For values specific to the first emacs screen, see initial-screen-alist.\n\
For values specific to the separate minibuffer screen, see\n\
minibuffer-screen-alist.");
  Vdefault_screen_alist = Qnil;

  defsubr (&Sscreenp);
  defsubr (&Slive_screen_p);
  defsubr (&Sselect_screen);
  defsubr (&Sselected_screen);
  defsubr (&Swindow_screen);
  defsubr (&Sscreen_root_window);
  defsubr (&Sscreen_selected_window);
  defsubr (&Sscreen_list);
  defsubr (&Snext_screen);
  defsubr (&Sdelete_screen);
  defsubr (&Smouse_position);
  defsubr (&Sset_mouse_position);
#if 0
  defsubr (&Sscreen_configuration);
  defsubr (&Srestore_screen_configuration);
#endif
  defsubr (&Smake_screen_visible);
  defsubr (&Smake_screen_invisible);
  defsubr (&Siconify_screen);
  defsubr (&Sdeiconify_screen);
  defsubr (&Sscreen_visible_p);
  defsubr (&Svisible_screen_list);
  defsubr (&Sredirect_screen_focus);
  defsubr (&Sscreen_focus);
  defsubr (&Sscreen_parameters);
  defsubr (&Smodify_screen_parameters);
  defsubr (&Sscreen_pixel_size);
  defsubr (&Sscreen_height);
  defsubr (&Sscreen_width);
  defsubr (&Sset_screen_height);
  defsubr (&Sset_screen_width);
  defsubr (&Sset_screen_size);
  defsubr (&Sset_screen_position);
#ifndef HAVE_X11
  defsubr (&Srubber_band_rectangle);
#endif	/* HAVE_X11 */
}
