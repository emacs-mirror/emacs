/* Define frame-object for GNU Emacs.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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


/* Miscellanea.  */

/* Nonzero means don't assume anything about current
   contents of actual terminal frame */
extern int frame_garbaged;

/* Nonzero means FRAME_MESSAGE_BUF (selected_frame) is being used by
   print.  */
extern int message_buf_print;


/* The structure representing a frame.

   We declare this even if MULTI_FRAME is not defined, because when
   we lack multi-frame support, we use one instance of this structure
   to represent the one frame we support.  This is cleaner than
   having miscellaneous random variables scattered about.  */

enum output_method
{ output_termcap, output_x_window, output_msdos_raw };

struct frame
{
  EMACS_INT size;
  struct Lisp_Vector *next;

  /* All Lisp_Object components must come first.
     Only EMACS_INT values can be intermixed with them.
     That ensures they are all aligned normally.  */

  /* Name of this frame: a Lisp string.  See also `explicit_name'.  */
  Lisp_Object name;

  /* The frame which should receive keystrokes that occur in this
     frame, or nil if they should go to the frame itself.  This is
     usually nil, but if the frame is minibufferless, we can use this
     to redirect keystrokes to a surrogate minibuffer frame when
     needed.

     Note that a value of nil is different than having the field point
     to the frame itself.  Whenever the Fselect_frame function is used
     to shift from one frame to the other, any redirections to the
     original frame are shifted to the newly selected frame; if
     focus_frame is nil, Fselect_frame will leave it alone.  */
  Lisp_Object focus_frame;

  /* This frame's root window.  Every frame has one.
     If the frame has only a minibuffer window, this is it.
     Otherwise, if the frame has a minibuffer window, this is its sibling.  */
  Lisp_Object root_window;

  /* This frame's selected window.
     Each frame has its own window hierarchy
     and one of the windows in it is selected within the frame.
     The selected window of the selected frame is Emacs's selected window.  */
  Lisp_Object selected_window;

  /* This frame's minibuffer window.
     Most frames have their own minibuffer windows,
     but only the selected frame's minibuffer window
     can actually appear to exist.  */
  Lisp_Object minibuffer_window;

  /* Parameter alist of this frame.
     These are the parameters specified when creating the frame
     or modified with modify-frame-parameters.  */
  Lisp_Object param_alist;

  /* List of scroll bars on this frame.  
     Actually, we don't specify exactly what is stored here at all; the
     scroll bar implementation code can use it to store anything it likes.
     This field is marked by the garbage collector.  It is here
     instead of in the `display' structure so that the garbage
     collector doesn't need to look inside the window-system-dependent
     structure.  */
  Lisp_Object scroll_bars;
  Lisp_Object condemned_scroll_bars;

  /* List of elements to display in the menu bar.
     The elements have the form (KEY STRING . nil) to start;
     when they are displayed, the hpos of the left edge goes in the cddr.  */
  Lisp_Object menu_bar_items;

  /* Alist of elements (FACE-NAME . FACE-VECTOR-DATA).  */
  Lisp_Object face_alist;

  /* A vector that records the entire structure of this frame's menu bar.
     For the format of the data, see extensive comments in xmenu.c.
     Only the X toolkit version uses this.  */
  Lisp_Object menu_bar_vector;
  /* Number of elements in the vector that have meaningful data.  */
  EMACS_INT menu_bar_items_used;

  /* Predicate for selecting buffers for other-buffer.  */
  Lisp_Object buffer_predicate;

  /* Beyond here, there should be no more Lisp_Object components.  */


  /* glyphs as they appear on the frame */
  struct frame_glyphs *current_glyphs;

  /* glyphs we'd like to appear on the frame */
  struct frame_glyphs *desired_glyphs;

  /* See do_line_insertion_deletion_costs for info on these arrays. */
  /* Cost of inserting 1 line on this frame */
  int *insert_line_cost;
  /* Cost of deleting 1 line on this frame */
  int *delete_line_cost;
  /* Cost of inserting n lines on this frame */
  int *insert_n_lines_cost;
  /* Cost of deleting n lines on this frame */
  int *delete_n_lines_cost;

  /* glyphs for the mode line */
  struct frame_glyphs *temp_glyphs;

  /* Intended cursor position of this frame.
     Measured in characters, counting from upper left corner
     within the frame.  */
  int cursor_x;
  int cursor_y;

  /* Actual cursor position of this frame, and the character under it.
     (Not used for terminal frames.)  */
  int phys_cursor_x;
  int phys_cursor_y;
  /* This is handy for undrawing the cursor, because current_glyphs is
     not always accurate when in do_scrolling.  */
  GLYPH phys_cursor_glyph;

  /* Size of this frame, in units of characters.  */
  EMACS_INT height;
  EMACS_INT width;

  /* New height and width for pending size change.  0 if no change pending.  */
  int new_height, new_width;

  /* The output method says how the contents of this frame
     are displayed.  It could be using termcap, or using an X window.  */
  enum output_method output_method;

  /* A structure of auxiliary data used for displaying the contents.
     struct x_display is used for X window frames;
     it is defined in xterm.h.  */
  union display { struct x_display *x; int nothing; } display;

#ifdef MULTI_KBOARD
  /* A pointer to the kboard structure associated with this frame.
     For termcap frames, this points to initial_kboard.  For X frames,
     it will be the same as display.x->display_info->kboard.  */
  struct kboard *kboard;
#endif

  /* Number of lines of menu bar.  */
  int menu_bar_lines;

#ifdef USE_X_TOOLKIT
  /* Nonzero means using a menu bar that comes from the X toolkit.  */
  int external_menu_bar;
#endif

  /* Nonzero if last attempt at redisplay on this frame was preempted.  */
  char display_preempted;

  /* visible is nonzero if the frame is currently displayed; we check
     it to see if we should bother updating the frame's contents.
     DON'T SET IT DIRECTLY; instead, use FRAME_SET_VISIBLE.

     Note that, since invisible frames aren't updated, whenever a
     frame becomes visible again, it must be marked as garbaged.  The
     FRAME_SAMPLE_VISIBILITY macro takes care of this.

     iconified is nonzero if the frame is currently iconified.

     Asynchronous input handlers should NOT change these directly;
     instead, they should change async_visible or async_iconified, and
     let the FRAME_SAMPLE_VISIBILITY macro set visible and iconified
     at the next redisplay.

     These should probably be considered read-only by everyone except
     FRAME_SAMPLE_VISIBILITY.

     These two are mutually exclusive.  They might both be zero, if the
     frame has been made invisible without an icon.  */
  char visible, iconified;

  /* Asynchronous input handlers change these, and
     FRAME_SAMPLE_VISIBILITY copies them into visible and iconified.
     See FRAME_SAMPLE_VISIBILITY, below.  */
#ifdef __STDC__
  volatile
#endif
  char async_visible, async_iconified;

  /* Nonzero if this frame should be redrawn.  */
#ifdef __STDC__
  volatile
#endif
  char garbaged;

  /* True if frame actually has a minibuffer window on it.
     0 if using a minibuffer window that isn't on this frame.  */
  char has_minibuffer;
     
  /* 0 means, if this frame has just one window,
     show no modeline for that window.  */
  char wants_modeline;

  /* Non-zero if the hardware device this frame is displaying on can
     support scroll bars.  */
  char can_have_scroll_bars;

  /* If can_have_scroll_bars is non-zero, this is non-zero if we should
     actually display them on this frame.  */
  char has_vertical_scroll_bars;

  /* Non-0 means raise this frame to the top of the heap when selected.  */
  char auto_raise;

  /* Non-0 means lower this frame to the bottom of the stack when left.  */
  char auto_lower;

  /* True if frame's root window can't be split.  */
  char no_split;

  /* If this is set, then Emacs won't change the frame name to indicate
     the current buffer, etcetera.  If the user explicitly sets the frame
     name, this gets set.  If the user sets the name to Qnil, this is
     cleared.  */
  char explicit_name;

  /* Nonzero if size of some window on this frame has changed.  */
  char window_sizes_changed;

  /* Storage for messages to this frame. */
  char *message_buf;

  /* Nonnegative if current redisplay should not do scroll computation
     for lines beyond a certain vpos.  This is the vpos.  */
  int scroll_bottom_vpos;

  /* Width of the scroll bar, in pixels and in characters.
     scroll_bar_cols tracks scroll_bar_pixel_width if the latter is positive;
     a zero value in scroll_bar_pixel_width means to compute the actual width
     on the fly, using scroll_bar_cols and the current font width.  */
  int scroll_bar_pixel_width;
  int scroll_bar_cols;

  /* The baud rate that was used to calculate costs for this frame.  */
  int cost_calculation_baud_rate;
};

#ifdef MULTI_KBOARD  /* Note that MULTI_KBOARD implies MULTI_FRAME */
#define FRAME_KBOARD(f) ((f)->kboard)
#else
#define FRAME_KBOARD(f) (&the_only_kboard)
#endif

#ifdef MULTI_FRAME

typedef struct frame *FRAME_PTR;

#define XFRAME(p) ((struct frame *) XPNTR (p))
#define XSETFRAME(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_FRAME))

#define WINDOW_FRAME(w) (w)->frame

#define FRAME_LIVE_P(f) ((f)->display.nothing != 0)
#define FRAME_TERMCAP_P(f) ((f)->output_method == output_termcap)
#define FRAME_X_P(f) ((f)->output_method == output_x_window)
#define FRAME_MINIBUF_ONLY_P(f) \
  EQ (FRAME_ROOT_WINDOW (f), FRAME_MINIBUF_WINDOW (f))
#define FRAME_HAS_MINIBUF_P(f) ((f)->has_minibuffer)
#define FRAME_CURRENT_GLYPHS(f) (f)->current_glyphs
#define FRAME_DESIRED_GLYPHS(f) (f)->desired_glyphs
#define FRAME_TEMP_GLYPHS(f) (f)->temp_glyphs
#define FRAME_HEIGHT(f) (f)->height
#define FRAME_WIDTH(f) (f)->width
#define FRAME_NEW_HEIGHT(f) (f)->new_height
#define FRAME_NEW_WIDTH(f) (f)->new_width
#define FRAME_MENU_BAR_LINES(f) (f)->menu_bar_lines
#ifdef USE_X_TOOLKIT
#define FRAME_EXTERNAL_MENU_BAR(f) (f)->external_menu_bar
#else
#define FRAME_EXTERNAL_MENU_BAR(f) 0
#endif
#define FRAME_CURSOR_X(f) (f)->cursor_x
#define FRAME_CURSOR_Y(f) (f)->cursor_y
#define FRAME_VISIBLE_P(f) ((f)->visible != 0)
#define FRAME_SET_VISIBLE(f,p) \
  ((f)->async_visible = (p), FRAME_SAMPLE_VISIBILITY (f))
#define SET_FRAME_GARBAGED(f) (frame_garbaged = 1, f->garbaged = 1)
#define FRAME_GARBAGED_P(f) (f)->garbaged
#define FRAME_NO_SPLIT_P(f) (f)->no_split
#define FRAME_WANTS_MODELINE_P(f) (f)->wants_modeline
#define FRAME_ICONIFIED_P(f) (f)->iconified
#define FRAME_WINDOW_SIZES_CHANGED(f) (f)->window_sizes_changed
#define FRAME_MINIBUF_WINDOW(f) (f)->minibuffer_window
#define FRAME_ROOT_WINDOW(f) (f)->root_window
#define FRAME_SELECTED_WINDOW(f) (f)->selected_window
#define SET_GLYPHS_FRAME(glyphs,frame) ((glyphs)->frame = (frame))
#define FRAME_INSERT_COST(f) (f)->insert_line_cost    
#define FRAME_DELETE_COST(f) (f)->delete_line_cost    
#define FRAME_INSERTN_COST(f) (f)->insert_n_lines_cost
#define FRAME_DELETEN_COST(f) (f)->delete_n_lines_cost
#define FRAME_MESSAGE_BUF(f) (f)->message_buf
#define FRAME_SCROLL_BOTTOM_VPOS(f) (f)->scroll_bottom_vpos
#define FRAME_FOCUS_FRAME(f) (f)->focus_frame
#define FRAME_CAN_HAVE_SCROLL_BARS(f) ((f)->can_have_scroll_bars)
#define FRAME_HAS_VERTICAL_SCROLL_BARS(f) ((f)->has_vertical_scroll_bars)
#define FRAME_SCROLL_BAR_PIXEL_WIDTH(f) ((f)->scroll_bar_pixel_width)
#define FRAME_SCROLL_BAR_COLS(f) ((f)->scroll_bar_cols)
#define FRAME_SCROLL_BARS(f) ((f)->scroll_bars)
#define FRAME_CONDEMNED_SCROLL_BARS(f) ((f)->condemned_scroll_bars)
#define FRAME_MENU_BAR_ITEMS(f) ((f)->menu_bar_items)
#define FRAME_COST_BAUD_RATE(f) ((f)->cost_calculation_baud_rate)

/* Emacs's redisplay code could become confused if a frame's
   visibility changes at arbitrary times.  For example, if a frame is
   visible while the desired glyphs are being built, but becomes
   invisible before they are updated, then some rows of the
   desired_glyphs will be left marked as enabled after redisplay is
   complete, which should never happen.  The next time the frame
   becomes visible, redisplay will probably barf.

   Currently, there are no similar situations involving iconified, but
   the principle is the same.

   So instead of having asynchronous input handlers directly set and
   clear the frame's visibility and iconification flags, they just set
   the async_visible and async_iconified flags; the redisplay code
   calls the FRAME_SAMPLE_VISIBILITY macro before doing any redisplay,
   which sets visible and iconified from their asynchronous
   counterparts.

   Synchronous code must use the FRAME_SET_VISIBLE macro.

   Also, if a frame used to be invisible, but has just become visible,
   it must be marked as garbaged, since redisplay hasn't been keeping
   up its contents.  */
#define FRAME_SAMPLE_VISIBILITY(f) \
  (((f)->async_visible && ! (f)->visible) ? SET_FRAME_GARBAGED (f) : 0, \
   (f)->visible = (f)->async_visible, \
   (f)->iconified = (f)->async_iconified)

#define CHECK_FRAME(x, i)				\
  {							\
    if (! FRAMEP (x))					\
      x = wrong_type_argument (Qframep, (x));		\
  }

#define CHECK_LIVE_FRAME(x, i)				\
  {							\
    if (! FRAMEP (x)					\
	|| ! FRAME_LIVE_P (XFRAME (x)))		\
      x = wrong_type_argument (Qframe_live_p, (x));	\
  }

/* FOR_EACH_FRAME (LIST_VAR, FRAME_VAR) followed by a statement is a
   `for' loop which iterates over the elements of Vframe_list.  The
   loop will set FRAME_VAR, a Lisp_Object, to each frame in
   Vframe_list in succession and execute the statement.  LIST_VAR
   should be a Lisp_Object too; it is used to iterate through the
   Vframe_list.  

   If MULTI_FRAME isn't defined, then this loop expands to something which 
   executes the statement once.  */
#define FOR_EACH_FRAME(list_var, frame_var)			\
  for ((list_var) = Vframe_list;				\
       (CONSP (list_var)					\
	&& (frame_var = XCONS (list_var)->car, 1));		\
       list_var = XCONS (list_var)->cdr)


extern Lisp_Object Qframep, Qframe_live_p, Qicon;

extern struct frame *selected_frame;
extern struct frame *last_nonminibuf_frame;

extern struct frame *make_terminal_frame ();
extern struct frame *make_frame ();
extern struct frame *make_minibuffer_frame ();
extern struct frame *make_frame_without_minibuffer ();

extern Lisp_Object Vframe_list;
extern Lisp_Object Vdefault_frame_alist;

extern Lisp_Object Vterminal_frame;

#else /* not MULTI_FRAME */

/* These definitions are used in a single-frame version of Emacs.  */

/* A frame we use to store all the data concerning the screen when we
   don't have multiple frames.  Remember, if you store any data in it
   which needs to be protected from GC, you should staticpro that
   element explicitly.  */
extern struct frame the_only_frame;

typedef struct frame *FRAME_PTR;
#ifdef __GNUC__
/* A function call for always getting 0 is overkill, so... */
#define WINDOW_FRAME(w) ({ Lisp_Object tem; XSETFASTINT (tem, 0); tem; })
#else
#define WINDOW_FRAME(w) (Fselected_frame ())
#endif
#define XSETFRAME(p, v) (p = WINDOW_FRAME (***bogus***))
#define XFRAME(frame) (&the_only_frame)

extern FRAME_PTR selected_frame;
extern FRAME_PTR last_nonminibuf_frame;

#define FRAME_LIVE_P(f) 1
#ifdef MSDOS
/* The following definitions could also be used in the non-MSDOS case,
   but the constants below lead to better code.  */
#define FRAME_TERMCAP_P(f) (the_only_frame.output_method == output_termcap)
#define FRAME_X_P(f) (the_only_frame.output_method != output_termcap)
#else
#define FRAME_TERMCAP_P(f) 1
#define FRAME_X_P(f) 0
#endif
#define FRAME_MINIBUF_ONLY_P(f) 0
#define FRAME_HAS_MINIBUF_P(f) 1
#define FRAME_CURRENT_GLYPHS(f) (the_only_frame.current_glyphs)
#define FRAME_DESIRED_GLYPHS(f) (the_only_frame.desired_glyphs)
#define FRAME_TEMP_GLYPHS(f) (the_only_frame.temp_glyphs)
#define FRAME_HEIGHT(f) (the_only_frame.height)
#define FRAME_WIDTH(f) (the_only_frame.width)
#define FRAME_NEW_HEIGHT(f) (the_only_frame.new_height)
#define FRAME_NEW_WIDTH(f) (the_only_frame.new_width)
#define FRAME_MENU_BAR_LINES(f) (the_only_frame.menu_bar_lines)
#define FRAME_CURSOR_X(f) (the_only_frame.cursor_x)
#define FRAME_CURSOR_Y(f) (the_only_frame.cursor_y)
#define FRAME_SET_VISIBLE(f,p) (p)
#define FRAME_VISIBLE_P(f) 1
#define SET_FRAME_GARBAGED(f) (frame_garbaged = 1)
#define FRAME_GARBAGED_P(f) (frame_garbaged)
#define FRAME_NO_SPLIT_P(f) 0
#define FRAME_WANTS_MODELINE_P(f) 1
#define FRAME_ICONIFIED_P(f) 0
#define FRAME_WINDOW_SIZES_CHANGED(f) the_only_frame.window_sizes_changed
#define FRAME_MINIBUF_WINDOW(f) (minibuf_window)
#define FRAME_ROOT_WINDOW(f) (the_only_frame.root_window)
#define FRAME_SELECTED_WINDOW(f) (selected_window)
#define SET_GLYPHS_FRAME(glyphs,frame) do ; while (0)
#define FRAME_INSERT_COST(frame)  (the_only_frame.insert_line_cost)
#define FRAME_DELETE_COST(frame)  (the_only_frame.delete_line_cost)
#define FRAME_INSERTN_COST(frame) (the_only_frame.insert_n_lines_cost)
#define FRAME_DELETEN_COST(frame) (the_only_frame.delete_n_lines_cost)
#define FRAME_MESSAGE_BUF(f) (the_only_frame.message_buf)
#define FRAME_SCROLL_BOTTOM_VPOS(f) (the_only_frame.scroll_bottom_vpos)
#define FRAME_FOCUS_FRAME(f) (Qnil)
#define FRAME_CAN_HAVE_SCROLL_BARS(f) (the_only_frame.can_have_scroll_bars)
#define FRAME_HAS_VERTICAL_SCROLL_BARS(f) \
  (the_only_frame.has_vertical_scroll_bars)
#define FRAME_SCROLL_BAR_PIXEL_WIDTH(f) (the_only_frame.scroll_bar_pixel_width)
#define FRAME_SCROLL_BAR_COLS(f) (the_only_frame.scroll_bar_cols)
#define FRAME_SCROLL_BARS(f) (the_only_frame.scroll_bars)
#define FRAME_CONDEMNED_SCROLL_BARS(f) (the_only_frame.condemned_scroll_bars)
#define FRAME_MENU_BAR_ITEMS(f) (the_only_frame.menu_bar_items)
#define FRAME_COST_BAUD_RATE(f) (the_only_frame.cost_calculation_baud_rate)

/* See comments in definition above.  */
#define FRAME_SAMPLE_VISIBILITY(f) (0)

#define CHECK_FRAME(x, i) do; while (0)
#define CHECK_LIVE_FRAME(x, y) do; while (0)

/* FOR_EACH_FRAME (LIST_VAR, FRAME_VAR) followed by a statement is a
   `for' loop which iterates over the elements of Vframe_list.  The
   loop will set FRAME_VAR, a Lisp_Object, to each frame in
   Vframe_list in succession and execute the statement.  LIST_VAR
   should be a Lisp_Object too; it is used to iterate through the
   Vframe_list.  

   If MULTI_FRAME _is_ defined, then this loop expands to a real
   `for' loop which traverses Vframe_list using LIST_VAR and
   FRAME_VAR.  */
#define FOR_EACH_FRAME(list_var, frame_var)			\
  for (list_var = Qt; frame_var = WINDOW_FRAME (***bogus***), ! NILP (list_var); list_var = Qnil)

#endif /* not MULTI_FRAME */


/* Device- and MULTI_FRAME-independent scroll bar stuff.  */

/* Return the starting column (zero-based) of the vertical scroll bar
   for window W.  The column before this one is the last column we can
   use for text.  If the window touches the right edge of the frame,
   we have extra space allocated for it.  Otherwise, the scroll bar
   takes over the window's rightmost columns.  */
#define WINDOW_VERTICAL_SCROLL_BAR_COLUMN(w) \
  (((XINT ((w)->left) + XINT ((w)->width)) \
    < FRAME_WIDTH (XFRAME (WINDOW_FRAME (w)))) \
   ? (XINT ((w)->left) + XINT ((w)->width) \
      - FRAME_SCROLL_BAR_COLS (XFRAME (WINDOW_FRAME (w)))) \
   : FRAME_WIDTH (XFRAME (WINDOW_FRAME (w))))

/* Return the height in lines of the vertical scroll bar in w.  If the
   window has a mode line, don't make the scroll bar extend that far.  */
#define WINDOW_VERTICAL_SCROLL_BAR_HEIGHT(w) (window_internal_height (w))
