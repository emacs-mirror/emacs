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

#include "config.h"
#include "lisp.h"
#include "screen.h"
#include "window.h"

Lisp_Object Vemacs_iconified;
Lisp_Object Qscreenp;
Lisp_Object Vscreen_list;
Lisp_Object Vterminal_screen;
Lisp_Object Vglobal_minibuffer_screen;

extern Lisp_Object Vminibuffer_list;
extern Lisp_Object get_minibuffer ();

DEFUN ("screenp", Fscreenp, Sscreenp, 1, 1, 0,
  "Return non-nil if OBJECT is a screen.\n\
Value is t for a termcap screen (a character-only terminal),\n\
`x' for an Emacs screen that is really an X window.")
  (screen)
     Lisp_Object screen;
{
  if (XTYPE (screen) != Lisp_Screen)
    return Qnil;
  switch (XSCREEN (screen)->output_method)
    {
    case output_termcap:
      return Qt;
    case output_x_window:
      return intern ("x");
    default:
      abort ();
    }
}

struct screen *
make_screen (mini_p)
     int mini_p;
{
  Lisp_Object screen;
  register struct screen *s;
  register Lisp_Object root_window;
  register Lisp_Object mini_window;

  screen = Fmake_vector (sizeof (struct screen) - sizeof (Lisp_Vector) + 1,
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

  XWINDOW (root_window)->buffer = Qt;
  Fset_window_buffer (root_window, Fcurrent_buffer ());
  if (mini_p)
    {
      XWINDOW (mini_window)->buffer = Qt;
      Fset_window_buffer (mini_window,
			  (NULL (Vminibuffer_list)
			   ? get_minibuffer (0)
			   : Fcar (Vminibuffer_list)));
    }

  s->selected_window = root_window;
  s->root_window = root_window;

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
  if (NULL (mini_window))
    {
      CHECK_SCREEN (Vglobal_minibuffer_screen, 0);
      mini_window = XSCREEN (Vglobal_minibuffer_screen)->minibuffer_window;
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
		      (NULL (Vminibuffer_list)
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
  /* Note we leave has_minibuffer as 0.  This is a little strange.  */

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
		      (NULL (Vminibuffer_list)
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
selected window.  If the optional parameter NO-ENTER is non-nil, don't
focus on that screen.")
  (screen, no_enter)
     Lisp_Object screen, no_enter;
{
  CHECK_SCREEN (screen, 0);

  if (selected_screen == XSCREEN (screen))
    return screen;

  selected_screen = XSCREEN (screen);
  Fselect_window (XSCREEN (screen)->selected_window);

#ifdef HAVE_X_WINDOWS
#ifdef MULTI_SCREEN
  if (XSCREEN (screen)->output_method == output_x_window
      && NULL (no_enter))
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
  if (NULL (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  CHECK_SCREEN (screen, 0);

  return XSCREEN (screen)->root_window;
}

DEFUN ("screen-selected-window", Fscreen_selected_window,
       Sscreen_selected_window, 0, 1, 0,
  "Return the selected window of screen object SCREEN.")
  (screen)
     Lisp_Object screen;
{
  if (NULL (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  CHECK_SCREEN (screen, 0);

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
Lisp_Object
next_screen (screen, mini_screen)
     Lisp_Object screen;
     int mini_screen;
{
  Lisp_Object tail;
  int passed = 0;

  while (1)
    for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	if (passed)
	  if (!mini_screen)
	    {
	      SCREEN_PTR s = XSCREEN (XCONS (tail)->car);

	      if (EQ (XCONS (tail)->car, Vglobal_minibuffer_screen)
		  && EQ (s->root_window, s->minibuffer_window))
		continue;
	    }
	  else
	    return XCONS (tail)->car;

	if (EQ (screen, XCONS (tail)->car))
	  passed++;
      }
}

Lisp_Object
prev_screen (screen, mini_screen)
     Lisp_Object screen;
     int mini_screen;
{
  Lisp_Object tail;
  Lisp_Object prev;

  prev = Qnil;
  while (1)
    for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	if (EQ (screen, XCONS (tail)->car))
	  {
	    if (!NULL (prev) && (mini_screen
				 || !EQ (XCONS (tail)->car,
					 Vglobal_minibuffer_screen)))
	      return prev;
	  }
	prev = XCONS (tail)->car;
      }
}

DEFUN ("next-screen", Fnext_screen, Snext_screen,
       0, 2, 0,
       "Return the next screen in the screen list after SCREEN.\n\
If MINISCREEN is non-nil, include screens whose only window is a minibuffer.\n\
If MINISCREEN is nil or omitted, these screens are skipped.")
  (screen, miniscreen)
Lisp_Object screen, miniscreen;
{
  Lisp_Object tail;

  if (NULL (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  CHECK_SCREEN (screen, 0);

  return next_screen (screen, (NULL (miniscreen) ? 0 : 1));
}
#endif /* MULTI_SCREEN */

DEFUN ("delete-screen", Fdelete_screen, Sdelete_screen,
       0, 1, "",
       "Delete SCREEN, permanently eliminating it from use.\n\
Default is current screen.")
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

  /* Don't allow deleted screen to remain selected.  */
  if (s == selected_screen)
    {
      Lisp_Object next;

      next = next_screen (screen, 0);
      if (EQ (next, screen))
	error ("Attempt to delete the only screen");
      Fselect_screen (next, Qnil);
    }

  /* Don't allow the global minibuffer screen to be deleted */
  if (s == XSCREEN (Vglobal_minibuffer_screen))
    error ("Attempt to delete the global minibuffer screen");

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

  if (s->output_method == output_x_window)
    x_destroy_window (s, displ);

  return Qnil;
}

/* Return mouse position in character cell units.  */

static
read_mouse_position (screen, x, y)
     Lisp_Object screen;
     int *x, *y;
{
  CHECK_SCREEN (screen, 0);

  *x = 1;
  *y = 1;

#ifdef HAVE_X_WINDOWS
  if (XSCREEN (screen)->output_method == output_x_window)
    x_read_mouse_position (XSCREEN (screen), x, y);
#endif
}

DEFUN ("read-mouse-position", Fread_mouse_position, Sread_mouse_position, 1, 1, 0,
  "Return a cons (x . y) which represents the position of the mouse.")
  (screen)
     Lisp_Object screen;
{
  int x, y;
  struct screen *s;

  CHECK_SCREEN (screen, 0);

  read_mouse_position (screen, &x, &y);
  return Fcons (make_number (x), make_number (y));
}

DEFUN ("set-mouse-position", Fset_mouse_position, Sset_mouse_position, 3, 3, 0,
  "Move the mouse pointer to the center of cell (X,Y) in SCREEN.\n\
WARNING:  If you use this under X, you should do unfocus-screen afterwards.")
  (screen, x, y)
     Lisp_Object screen, x, y;
{
  CHECK_SCREEN (screen, 0);
  CHECK_NUMBER (x, 2);
  CHECK_NUMBER (y, 1);

#ifdef HAVE_X_WINDOWS
  if (XSCREEN (screen)->output_method == output_x_window)
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
  int x, y;
  Lisp_Object c, screen;
  struct screen *s;
  
  c = Fmake_vector (make_number(3), Qnil);
  XVECTOR (c)->contents[0] = screen = Fselected_screen();
  read_mouse_position (screen, &x, &y);
  XVECTOR (c)->contents[1] = make_number (x);
  XVECTOR (c)->contents[2] = make_number (y);

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
  CHECK_SCREEN (screen, 0);

  Fselect_screen (screen, Qnil);

#if 0
  /* This seems to interfere with the screen selection mechanism. jla */
  x_pos = XVECTOR (config)->contents[1];
  y_pos = XVECTOR (config)->contents[2];
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
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->display.nothing == 0)
    error ("Cannot make a dead screen object visible");

  if (XSCREEN (screen)->output_method == output_x_window)
    x_make_screen_visible (XSCREEN (screen));

  return screen;
}

DEFUN ("make-screen-invisible", Fmake_screen_invisible, Smake_screen_invisible,
       1, 1, 0,
  "Make the screen SCREEN invisible (assuming it is an X-window).")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->output_method == output_x_window)
    x_make_screen_invisible (XSCREEN (screen));

  return Qnil;
}

DEFUN ("iconify-screen", Ficonify_screen, Siconify_screen,
       1, 1, 0,
  "Make the screen SCREEN into an icon.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->display.nothing == 0)
    error ("Cannot make a dead screen object iconified.");

  if (XSCREEN (screen)->output_method == output_x_window)
      x_iconify_screen (XSCREEN (screen));

  return Qnil;
}

DEFUN ("deiconify-screen", Fdeiconify_screen, Sdeiconify_screen,
       1, 1, 0,
  "Open (de-iconify) the iconified screen SCREEN.")
  (screen)
     Lisp_Object screen;
{
  CHECK_SCREEN (screen, 0);

  if (XSCREEN (screen)->display.nothing == 0)
    error ("Cannot deiconify a dead screen object.");

  if (XSCREEN (screen)->output_method == output_x_window)
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
  CHECK_SCREEN (screen, 0);

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
  store_in_alist (&alist, "minibuffer", (s->has_minibuffer ? Qt : Qnil));
  store_in_alist (&alist, "unsplittable", (s->no_split ? Qt : Qnil));

  if (s->output_method == output_x_window)
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
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (s->display.nothing == 0)
    error ("Cannot modify parameters of a deleted screen");

  if (s->output_method == output_x_window)
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
  "Return a cons (width . height) of screen SCREEN's dimensions.")
  (screen)
     Lisp_Object screen;
{
  register struct screen *s;
  int width, height;

  CHECK_SCREEN (screen, 0);
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
  if (NULL (screen))
    s = selected_screen;
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (s->output_method == output_x_window)
    {
      if (XINT (rows) != s->width)
	x_set_window_size (s, s->width, XINT (rows));
    }
  else
    change_screen_size (s, XINT (rows), 0, !NULL (pretend));
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
  if (NULL (screen))
    s = selected_screen;
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }

  if (s->output_method == output_x_window)
    {
      if (XINT (cols) != s->width)
	x_set_window_size (s, XINT (cols), s->height);
    }
  else
    change_screen_size (selected_screen, 0, XINT (cols), !NULL (pretend));
  return Qnil;
}

DEFUN ("set-screen-size", Fset_screen_size, 
       Sset_screen_size, 3, 3, 0,
  "Sets size of SCREEN to COLS by ROWS, measured in characters.")
  (screen, cols, rows)
     Lisp_Object screen, cols, rows;
{
  register struct screen *s;
  int mask;

  CHECK_SCREEN (screen, 0);
  CHECK_NUMBER (cols, 2);
  CHECK_NUMBER (rows, 1);
  s = XSCREEN (screen);

  if (s->output_method == output_x_window)
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
  "Sets size of SCREEN in pixels to XOFFSET by YOFFSET.")
  (screen, xoffset, yoffset)
     Lisp_Object screen, xoffset, yoffset;
{
  register struct screen *s;
  int mask;

  CHECK_SCREEN (screen, 0);
  CHECK_NUMBER (xoffset, 1);
  CHECK_NUMBER (yoffset, 2);
  s = XSCREEN (screen);

  if (s->output_method == output_x_window)
    x_set_offset (s, XINT (xoffset), XINT (yoffset));

  return Qt;
}

/* Test if column *x, row *y is within window *w.  If they are not,
   return 0;  if they are on the window's modeline, return -1; if
   they are in the window's text area (the only other alternative)
   set *x and *y to their locations relative to the upper left
   corner of the window, and return 1.  */
int
coordinates_in_window (w, x, y)
     register struct window *w;
     register int *x, *y;
{
  register int left = XINT (w->left);
  register int width = XINT (w->width);
  register int window_height = XINT (w->height);
  register int top = XFASTINT (w->top);

  if (*x < left || *x >= left + width
      || *y < top || *y > top + window_height - 1)
    return 0;

  if (*y == top + window_height - 1
      && window_height > 1)	/* 1 line => minibuffer */
    /* in modeline */
    return -1;

  *x -= left;
  *y -= top;
  return 1;
}

DEFUN ("coordinates-in-window-p", Fcoordinates_in_window_p,
  Scoordinates_in_window_p, 2, 2, 0,
  "Return non-nil if COORDINATES are in WINDOW.\n\
COORDINATES is a cons of the form (X Y), X and Y being screen-relative.\n\
If COORDINATES are in the text portion of WINDOW, the coordinates relative\n\
to the window are returned.  If they are in the modeline of WINDOW, t is\n\
returned.")
  (coordinates, window)
     register Lisp_Object coordinates, window;
{
  int x, y;

  CHECK_WINDOW (window, 0);
  CHECK_CONS (coordinates, 1);
  x = XINT (Fcar (coordinates));
  y = XINT (Fcar (Fcdr (coordinates)));

  switch (coordinates_in_window (XWINDOW (window), &x, &y))
    {
    case -1:			/* In modeline of window. */
      return Qt;

    case 0:			/* NOT in window at all. */
      return Qnil;

    case 1:			/* In text part of window. */
      return Fcons (x, Fcons (y, Qnil));

    default:
      abort ();
    }
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

  staticpro (&Vscreen_list);

  DEFVAR_LISP ("terminal-screen", &Vterminal_screen,
    "The initial screen-object, which represents Emacs's stdout.");

  DEFVAR_LISP ("emacs-iconified", &Vemacs_iconified,
    "Non-nil if all of emacs is iconified and not screen updates are needed.");
  Vemacs_iconified = Qnil;

  DEFVAR_LISP ("global-minibuffer-screen", &Vglobal_minibuffer_screen,
 "A screen whose minibuffer is used by minibufferless screens.\n\
When you create a minibufferless screen, by default it will use the\n\
minibuffer of this screen.  It is up to you to create a suitable screen\n\
and store it in this variable.");
  Vglobal_minibuffer_screen = Qnil;

  defsubr (&Sscreenp);
  defsubr (&Sselect_screen);
  defsubr (&Sselected_screen);
  defsubr (&Swindow_screen);
  defsubr (&Sscreen_root_window);
  defsubr (&Sscreen_selected_window);
  defsubr (&Sscreen_list);
  defsubr (&Snext_screen);
  defsubr (&Sdelete_screen);
  defsubr (&Sread_mouse_position);
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
  defsubr (&Sscreen_parameters);
  defsubr (&Smodify_screen_parameters);
  defsubr (&Sscreen_pixel_size);
  defsubr (&Sscreen_height);
  defsubr (&Sscreen_width);
  defsubr (&Sset_screen_height);
  defsubr (&Sset_screen_width);
  defsubr (&Sset_screen_size);
  defsubr (&Sset_screen_position);
  defsubr (&Scoordinates_in_window_p);
#ifndef HAVE_X11
  defsubr (&Srubber_band_rectangle);
#endif	/* HAVE_X11 */
}
