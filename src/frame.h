/* Define screen-object for GNU Emacs.
   Copyright (C) 1988, 1992 Free Software Foundation, Inc.

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


/* The structure representing a screen.

   We declare this even if MULTI_SCREEN is not defined, because when
   we lack multi-screen support, we use one instance of this structure
   to represent the one screen we support.  This is cleaner than
   having miscellaneous random variables scattered about.  */

enum output_method
{ output_termcap, output_x_window };

struct screen
{
  int size;
  struct Lisp_Vector *next;

  /* glyphs as they appear on the screen */
  struct screen_glyphs *current_glyphs;

  /* glyphs we'd like to appear on the screen */
  struct screen_glyphs *desired_glyphs;

  /* See do_line_insertion_deletion_costs for info on these arrays. */
  /* Cost of inserting 1 line on this screen */
  int *insert_line_cost;
  /* Cost of deleting 1 line on this screen */
  int *delete_line_cost;
  /* Cost of inserting n lines on this screen */
  int *insert_n_lines_cost;
  /* Cost of deleting n lines on this screen */
  int *delete_n_lines_cost;

  /* glyphs for the mode line */
  struct screen_glyphs *temp_glyphs;

  /* Intended cursor position of this screen.
     Measured in characters, counting from upper left corner
     within the screen.  */
  int cursor_x;
  int cursor_y;

  /* Actual cursor position of this screen, and the character under it.
     (Not used for terminal screens.)  */
  int phys_cursor_x;
  int phys_cursor_y;
  /* This is handy for undrawing the cursor, because current_glyphs is
     not always accurate when in do_scrolling.  */
  GLYPH phys_cursor_glyph;

  /* Size of this screen, in units of characters.  */
  int height;
  int width;

  /* New height and width for pending size change.  0 if no change pending.  */
  int new_height, new_width;

  /* Name of this screen: a Lisp string.  */
  Lisp_Object name;

  /* The screen which should recieve keystrokes that occur in this
     screen.  This is usually the screen itself, but if the screen is
     minibufferless, this points to the minibuffer screen when it is
     active.  */
  Lisp_Object focus_screen;

  /* This screen's root window.  Every screen has one.
     If the screen has only a minibuffer window, this is it.
     Otherwise, if the screen has a minibuffer window, this is its sibling.  */
  Lisp_Object root_window;

  /* This screen's selected window.
     Each screen has its own window hierarchy
     and one of the windows in it is selected within the screen.
     The selected window of the selected screen is Emacs's selected window.  */
  Lisp_Object selected_window;

  /* This screen's minibuffer window.
     Most screens have their own minibuffer windows,
     but only the selected screen's minibuffer window
     can actually appear to exist.  */
  Lisp_Object minibuffer_window;

  /* Parameter alist of this screen.
     These are the parameters specified when creating the screen
     or modified with modify-screen-parameters.  */
  Lisp_Object param_alist;

  /* The output method says how the contents of this screen
     are displayed.  It could be using termcap, or using an X window.  */
  enum output_method output_method;

  /* A structure of auxiliary data used for displaying the contents.
     struct x_display is used for X window screens;
     it is defined in xterm.h.  */
  union display { struct x_display *x; int nothing; } display;

  /* Nonzero if last attempt at redisplay on this screen was preempted.  */
  char display_preempted;

  /* Nonzero if screen is currently displayed.  */
  char visible;

  /* Nonzero if window is currently iconified.
     This and visible are mutually exclusive.  */
  char iconified;

  /* Nonzero if this screen should be redrawn.  */
  char garbaged;

  /* True if screen actually has a minibuffer window on it.
     0 if using a minibuffer window that isn't on this screen.  */
  char has_minibuffer;
     
  /* 0 means, if this screen has just one window,
     show no modeline for that window.  */
  char wants_modeline;

  /* Non-0 means raise this screen to the top of the heap when selected.  */
  char auto_raise;

  /* Non-0 means lower this screen to the bottom of the stack when left.  */
  char auto_lower;

  /* True if screen's root window can't be split.  */
  char no_split;

  /* Storage for messages to this screen. */
  char *message_buf;

  /* Nonnegative if current redisplay should not do scroll computation
     for lines beyond a certain vpos.  This is the vpos.  */
  int scroll_bottom_vpos;
};

#ifdef MULTI_SCREEN

typedef struct screen *SCREEN_PTR;

#define XSCREEN(p) ((struct screen *) XPNTR (p))
#define XSETSCREEN(p, v) ((struct screen *) XSETPNTR (p, v))

#define WINDOW_SCREEN(w) (w)->screen

#define SCREENP(s) (XTYPE(s) == Lisp_Screen)
#define SCREEN_LIVE_P(s) ((s)->display.nothing != 0)
#define SCREEN_IS_TERMCAP(s) ((s)->output_method == output_termcap)
#define SCREEN_IS_X(s) ((s)->output_method == output_x_window)
#define SCREEN_MINIBUF_ONLY_P(s) \
  EQ (SCREEN_ROOT_WINDOW (s), SCREEN_MINIBUF_WINDOW (s))
#define SCREEN_HAS_MINIBUF(s) ((s)->has_minibuffer)
#define SCREEN_CURRENT_GLYPHS(s) (s)->current_glyphs
#define SCREEN_DESIRED_GLYPHS(s) (s)->desired_glyphs
#define SCREEN_TEMP_GLYPHS(s) (s)->temp_glyphs
#define SCREEN_HEIGHT(s) (s)->height
#define SCREEN_WIDTH(s) (s)->width
#define SCREEN_NEW_HEIGHT(s) (s)->new_height
#define SCREEN_NEW_WIDTH(s) (s)->new_width
#define SCREEN_CURSOR_X(s) (s)->cursor_x
#define SCREEN_CURSOR_Y(s) (s)->cursor_y
#define SCREEN_VISIBLE_P(s) (s)->visible
#define SET_SCREEN_GARBAGED(s) (screen_garbaged = 1, s->garbaged = 1)
#define SCREEN_GARBAGED_P(s) (s)->garbaged
#define SCREEN_NO_SPLIT_P(s) (s)->no_split
#define SCREEN_WANTS_MODELINE_P(s) (s)->wants_modeline
#define SCREEN_ICONIFIED_P(s) (s)->iconified
#define SCREEN_MINIBUF_WINDOW(s) (s)->minibuffer_window
#define SCREEN_ROOT_WINDOW(s) (s)->root_window
#define SCREEN_SELECTED_WINDOW(s) (s)->selected_window
#define SET_GLYPHS_SCREEN(glyphs,screen) ((glyphs)->screen = (screen))
#define SCREEN_INSERT_COST(s) (s)->insert_line_cost    
#define SCREEN_DELETE_COST(s) (s)->delete_line_cost    
#define SCREEN_INSERTN_COST(s) (s)->insert_n_lines_cost
#define SCREEN_DELETEN_COST(s) (s)->delete_n_lines_cost
#define SCREEN_MESSAGE_BUF(s) (s)->message_buf
#define SCREEN_SCROLL_BOTTOM_VPOS(s) (s)->scroll_bottom_vpos
#define SCREEN_FOCUS_SCREEN(s) (s)->focus_screen

#define CHECK_SCREEN(x, i)				\
  {							\
    if (! SCREENP (x))					\
      x = wrong_type_argument (Qscreenp, (x));		\
  }

#define CHECK_LIVE_SCREEN(x, i)				\
  {							\
    if (! SCREENP (x)					\
	|| ! SCREEN_LIVE_P (XSCREEN (x)))		\
      x = wrong_type_argument (Qlive_screen_p, (x));	\
  }

/* FOR_EACH_SCREEN (LIST_VAR, SCREEN_VAR) followed by a statement is a
   `for' loop which iterates over the elements of Vscreen_list.  The
   loop will set SCREEN_VAR, a SCREEN_PTR, to each screen in
   Vscreen_list in succession and execute the statement.  LIST_VAR
   should be a Lisp_Object; it is used to iterate through the
   Vscreen_list.  

   If MULTI_SCREEN isn't defined, then this loop expands to something which 
   executes the statement once.  */
#define FOR_EACH_SCREEN(list_var, screen_var)			\
  for ((list_var) = Vscreen_list;				\
       (CONSP (list_var)					\
	&& (screen_var = XSCREEN (XCONS (list_var)->car), 1));	\
       list_var = XCONS (list_var)->cdr)


extern Lisp_Object Qscreenp, Qlive_screen_p;

extern struct screen *selected_screen;
extern struct screen *last_nonminibuf_screen;

extern struct screen *make_terminal_screen ();
extern struct screen *make_screen ();
extern struct screen *make_minibuffer_screen ();
extern struct screen *make_screen_without_minibuffer ();

/* Nonzero means SCREEN_MESSAGE_BUF (selected_screen) is being used by
   print.  */
extern int message_buf_print;

extern Lisp_Object Vscreen_list;
extern Lisp_Object Vdefault_screen_alist;

extern Lisp_Object Vterminal_screen;

#else /* not MULTI_SCREEN */

/* These definitions are used in a single-screen version of Emacs.  */

#define SCREEN_PTR int

extern struct screen the_only_screen;

extern int selected_screen;
extern int last_nonminibuf_screen;

/* Nonzero means SCREEN_MESSAGE_BUF (selected_screen) is being used by
   print.  */
extern int message_buf_print;

#define XSCREEN(s) selected_screen
#define WINDOW_SCREEN(w) selected_screen

#define SCREENP(s) (XTYPE(s) == Lisp_Screen)
#define SCREEN_LIVE_P(s) 1
#define SCREEN_IS_TERMCAP(s) 1
#define SCREEN_IS_X(s) 0
#define SCREEN_MINIBUF_ONLY_P(s) 0
#define SCREEN_HAS_MINIBUF(s) 1
#define SCREEN_CURRENT_GLYPHS(s) the_only_screen.current_glyphs
#define SCREEN_DESIRED_GLYPHS(s) the_only_screen.desired_glyphs
#define SCREEN_TEMP_GLYPHS(s) the_only_screen.temp_glyphs
#define SCREEN_HEIGHT(s) the_only_screen.height
#define SCREEN_WIDTH(s) the_only_screen.width
#define SCREEN_NEW_HEIGHT(s) the_only_screen.new_height
#define SCREEN_NEW_WIDTH(s) the_only_screen.new_width
#define SCREEN_CURSOR_X(s) the_only_screen.cursor_x
#define SCREEN_CURSOR_Y(s) the_only_screen.cursor_y
#define SCREEN_VISIBLE_P(s) 1
#define SET_SCREEN_GARBAGED(s) (screen_garbaged = 1)
#define SCREEN_GARBAGED_P(s) screen_garbaged
#define SCREEN_NO_SPLIT_P(s) 0
#define SCREEN_WANTS_MODELINE_P(s) 1
#define SCREEN_ICONIFIED_P(s) 0
#define SCREEN_MINIBUF_WINDOW(s) minibuf_window
#define SCREEN_ROOT_WINDOW(s) the_only_screen.root_window
#define SCREEN_SELECTED_WINDOW(s) selected_window
#define SET_GLYPHS_SCREEN(glyphs,screen)
#define SCREEN_INSERT_COST(screen)  the_only_screen.insert_line_cost    
#define SCREEN_DELETE_COST(screen)  the_only_screen.delete_line_cost    
#define SCREEN_INSERTN_COST(screen) the_only_screen.insert_n_lines_cost
#define SCREEN_DELETEN_COST(screen) the_only_screen.delete_n_lines_cost
#define SCREEN_MESSAGE_BUF(s) the_only_screen.message_buf
#define SCREEN_SCROLL_BOTTOM_VPOS(s) the_only_screen.scroll_bottom_vpos
#define SCREEN_FOCUS_SCREEN(s) 0

#define CHECK_SCREEN(x, i) { ; }
#define CHECK_LIVE_SCREEN(x, y) { ; }

/* FOR_EACH_SCREEN (LIST_VAR, SCREEN_VAR) followed by a statement is a
   `for' loop which iterates over the elements of Vscreen_list.  The
   loop will set SCREEN_VAR, a SCREEN_PTR, to each screen in
   Vscreen_list in succession and execute the statement.  LIST_VAR
   should be a Lisp_Object; it is used to iterate through the
   Vscreen_list.  

   If MULTI_SCREEN _is_ defined, then this loop expands to a real
   `for' loop which traverses Vscreen_list using LIST_VAR and
   SCREEN_VAR.  */
#define FOR_EACH_SCREEN(list_var, screen_var)			\
  for (screen_var = (SCREEN_PTR) 1; screen_var; screen_var = (SCREEN_PTR) 0)

#endif /* not MULTI_SCREEN */
