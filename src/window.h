/* Window definitions for GNU Emacs.
   Copyright (C) 1985, 1986, 1993, 1995 Free Software Foundation, Inc.

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


/* Windows are allocated as if they were vectors, but then the
Lisp data type is changed to Lisp_Window.  They are garbage
collected along with the vectors.

All windows in use are arranged into a tree, with pointers up and down.

Windows that are leaves of the tree are actually displayed
and show the contents of buffers.  Windows that are not leaves
are used for representing the way groups of leaf windows are
arranged on the frame.  Leaf windows never become non-leaves.
They are deleted only by calling delete-window on them (but
this can be done implicitly).  Combination windows can be created
and deleted at any time.

A leaf window has a non-nil buffer field, and also
 has markers in its start and pointm fields.  Non-leaf windows
 have nil in these fields.

Non-leaf windows are either vertical or horizontal combinations.

A vertical combination window has children that are arranged on the frame
one above the next.  Its vchild field points to the uppermost child.
The parent field of each of the children points to the vertical
combination window.  The next field of each child points to the
child below it, or is nil for the lowest child.  The prev field
of each child points to the child above it, or is nil for the
highest child.

A horizontal combination window has children that are side by side.
Its hchild field points to the leftmost child.  In each child
the next field points to the child to the right and the prev field
points to the child to the left.

The children of a vertical combination window may be leaf windows
or horizontal combination windows.  The children of a horizontal
combination window may be leaf windows or vertical combination windows.

At the top of the tree are two windows which have nil as parent.
The second of these is minibuf_window.  The first one manages all
the frame area that is not minibuffer, and is called the root window.
Different windows can be the root at different times;
initially the root window is a leaf window, but if more windows
are created then that leaf window ceases to be root and a newly
made combination window becomes root instead.

In any case, on screens which have an ordinary window and a
minibuffer, prev of the minibuf window is the root window and next of
the root window is the minibuf window.  On minibufferless screens or
minibuffer-only screens, the root window and the minibuffer window are
one and the same, so its prev and next members are nil.

A dead window has its buffer, hchild, and vchild windows all nil.  */

struct window
  {
    /* The first two fields are really the header of a vector */
    /* The window code does not refer to them.  */
    EMACS_INT size;
    struct Lisp_Vector *vec_next;
    /* The frame this window is on.  */
    Lisp_Object frame;
    /* t if this window is a minibuffer window.  */
    Lisp_Object mini_p;
    /* Following child (to right or down) at same level of tree */
    Lisp_Object next;
    /* Preceding child (to left or up) at same level of tree */
    Lisp_Object prev;
    /* First child of this window. */
    /* vchild is used if this is a vertical combination,
       hchild if this is a horizontal combination. */
    Lisp_Object hchild, vchild;
    /* The window this one is a child of. */
    Lisp_Object parent;
    /* The upper left corner coordinates of this window,
       as integers relative to upper left corner of frame = 0, 0 */
    Lisp_Object left;
    Lisp_Object top;
    /* The size of the window */
    Lisp_Object height;
    Lisp_Object width;
    /* The buffer displayed in this window */
    /* Of the fields vchild, hchild and buffer, only one is non-nil.  */
    Lisp_Object buffer;
    /* A marker pointing to where in the text to start displaying */
    Lisp_Object start;
    /* A marker pointing to where in the text point is in this window,
       used only when the window is not selected.
       This exists so that when multiple windows show one buffer
       each one can have its own value of point.  */
    Lisp_Object pointm;
    /* Non-nil means next redisplay must use the value of start
       set up for it in advance.  Set by scrolling commands.  */
    Lisp_Object force_start;
    /* Number of columns display within the window is scrolled to the left.  */
    Lisp_Object hscroll;
    /* Number saying how recently window was selected */
    Lisp_Object use_time;
    /* Unique number of window assigned when it was created */
    Lisp_Object sequence_number;
    /* No permanent meaning; used by save-window-excursion's bookkeeping */
    Lisp_Object temslot;
    /* text.modified of displayed buffer as of last time display completed */
    Lisp_Object last_modified;
    /* Value of point at that time */
    Lisp_Object last_point;
    /* This window's vertical scroll bar.  This field is only for use
       by the window-system-dependent code which implements the
       scroll bars; it can store anything it likes here.  If this
       window is newly created and we haven't displayed a scroll bar in
       it yet, or if the frame doesn't have any scroll bars, this is nil.  */
    Lisp_Object vertical_scroll_bar;

/* The rest are currently not used or only half used */
    /* Frame coords of point at that time */
    Lisp_Object last_point_x;
    Lisp_Object last_point_y;
    /* Frame coords of mark as of last time display completed */
    /* May be nil if mark does not exist or was not on frame */
    Lisp_Object last_mark_x;
    Lisp_Object last_mark_y;
    /* Number of characters in buffer past bottom of window,
       as of last redisplay that finished. */
    Lisp_Object window_end_pos;
    /* t if window_end_pos is truly valid.
       This is nil if nontrivial redisplay is preempted
       since in that case the frame image that window_end_pos
       did not get onto the frame.  */
    Lisp_Object window_end_valid;
    /* Vertical position (relative to window top) of that buffer position
       of the first of those characters */
    Lisp_Object window_end_vpos;
    /* Non-nil means must regenerate mode line of this window */
    Lisp_Object update_mode_line;
    /* Non-nil means current value of `start'
       was the beginning of a line when it was chosen.  */
    Lisp_Object start_at_line_beg;
    /* Display-table to use for displaying chars in this window.
       Nil means use the buffer's own display-table.  */
    Lisp_Object display_table;
    /* Non-nil means window is marked as dedicated.  */
    Lisp_Object dedicated;
    /* Line number and position of a line somewhere above the
       top of the screen.  */
    /* If this field is nil, it means we don't have a base line.  */
    Lisp_Object base_line_number;
    /* If this field is nil, it means we don't have a base line.
       If it is a buffer, it means don't display the line number
       as long as the window shows that buffer.  */
    Lisp_Object base_line_pos;
    /* If we have highlighted the region (or any part of it),
       this is the mark position that we used, as an integer.  */
    Lisp_Object region_showing;
    /* The column number currently displayed in this window's mode line,
       or nil if column numbers are not being displayed.  */
    Lisp_Object column_number_displayed;
  };

/* 1 if W is a minibuffer window.  */

#define MINI_WINDOW_P(W)  (!EQ ((W)->mini_p, Qnil))

/* This is the window in which the terminal's cursor should
   be left when nothing is being done with it.  This must
   always be a leaf window, and its buffer is selected by
   the top level editing loop at the end of each command.

   This value is always the same as
    FRAME_SELECTED_WINDOW (selected_frame).  */

extern Lisp_Object selected_window;

/* This is a time stamp for window selection, so we can find the least
   recently used window.  Its only users are Fselect_window,
   init_window_once, and make_frame.  */

extern int window_select_count;

/* The minibuffer window of the selected frame.
   Note that you cannot test for minibufferness of an arbitrary window
   by comparing against this; use the MINI_WINDOW_P macro instead.  */

extern Lisp_Object minibuf_window;

/* Non-nil => window to for C-M-v to scroll
   when the minibuffer is selected.  */
extern Lisp_Object Vminibuf_scroll_window;

/* nil or a symbol naming the window system
   under which emacs is running
   ('x is the only current possibility) */
extern Lisp_Object Vwindow_system;

/* Version number of X windows: 10, 11 or nil.  */
extern Lisp_Object Vwindow_system_version;

/* Window that the mouse is over (nil if no mouse support).  */
extern Lisp_Object Vmouse_window;

/* Last mouse-click event (nil if no mouse support).  */
extern Lisp_Object Vmouse_event;

extern Lisp_Object Fnext_window ();
extern Lisp_Object Fselect_window ();
extern Lisp_Object Fdisplay_buffer ();
extern Lisp_Object Fset_window_buffer ();
extern Lisp_Object make_window ();
extern Lisp_Object window_from_coordinates ();
extern Lisp_Object Fwindow_dedicated_p ();

/* Prompt to display in front of the minibuffer contents.  */
extern Lisp_Object minibuf_prompt;

/* The visual width of the above.  */
extern int minibuf_prompt_width;

/* Message to display instead of minibuffer contents. 
   This is what the functions error and message make,
   and command echoing uses it as well. It overrides the
   minibuf_prompt as well as the buffer.  */
extern char *echo_area_glyphs;

/* This is the length of the message in echo_area_glyphs.  */
extern int echo_area_glyphs_length;

/* Value of echo_area_glyphs when it was last acted on.
  If this is nonzero, there is a message on the frame
  in the minibuffer and it should be erased as soon
  as it is no longer requested to appear. */
extern char *previous_echo_glyphs;

/* Depth in recursive edits.  */
extern int command_loop_level;

/* Depth in minibuffer invocations.  */
extern int minibuf_level;

/* true iff we should redraw the mode lines on the next redisplay.  */
extern int update_mode_lines;

/* Minimum value of GPT since last redisplay that finished.  */

extern int beg_unchanged;

/* Minimum value of Z - GPT since last redisplay that finished.  */

extern int end_unchanged;

/* MODIFF as of last redisplay that finished;
   if it matches MODIFF, beg_unchanged and end_unchangedn
   contain no useful information.  */
extern int unchanged_modified;

/* Nonzero if BEGV - BEG or Z - ZV of current buffer has changed
   since last redisplay that finished.  */
extern int clip_changed;

/* Nonzero if window sizes or contents have changed
 since last redisplay that finished */
extern int windows_or_buffers_changed;

/* Number of windows displaying the selected buffer.
   Normally this is 1, but it can be more.  */
extern int buffer_shared;

/* If *ROWS or *COLS are too small a size for FRAME, set them to the
   minimum allowable size.  */
extern void check_frame_size ( /* FRAME_PTR frame, int *rows, int *cols */ );
