/* Window definitions for GNU Emacs.
   Copyright (C) 1985-1986, 1993, 1995, 1997-2020 Free Software
   Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef WINDOW_H_INCLUDED
#define WINDOW_H_INCLUDED

#include "dispextern.h"

INLINE_HEADER_BEGIN

/* Windows are allocated as if they were vectors, but then the Lisp
data type is changed to Lisp_Window.  They are garbage collected along
with the vectors.

All windows in use are arranged into a tree, with pointers up and down.

Windows that are leaves of the tree are actually displayed and show
the contents of buffers.  Windows that are not leaves are used for
representing the way groups of leaf windows are arranged on the frame.
Leaf windows never become non-leaves.  They are deleted only by
calling `delete-window' on them (but this can be done implicitly).
Non-leaf windows never become leaf windows and can be created and
deleted at any time by the window management code.  Non-leaf windows
can be seen but not directly manipulated by Lisp functions.

A leaf window has a buffer stored in its contents field and markers in
its 'start' and 'pointm' fields.  Non-leaf windows have nil in the
latter two fields.  Non-leaf windows are either vertical or horizontal
combinations.

A vertical combination window has children that are arranged on the
frame one above the next.  Its 'contents' field points to the
uppermost child.  The 'parent' field of each of the children points to
the vertical combination window.  The 'next' field of each child
points to the child below it, or is nil for the lowest child.  The
'prev' field of each child points to the child above it, or is nil for
the highest child.

A horizontal combination window has children that are arranged side by
side.  Its 'contents' field points to the leftmost child.  In each
child the 'next' field points to the child to the right and the 'prev'
field points to the child to the left.

On each frame there are at least one and at most two windows which
have nil as parent.  The second of these, if present, is the frame's
minibuffer window and shows the minibuffer or the echo area.  The
first one manages the remaining frame area and is called the frame's
root window.  Different windows can be the root at different times;
initially the root window is a leaf window, but if more windows are
created, then that leaf window ceases to be root and a newly made
combination window becomes the root instead.

On frames which have an ordinary window and a minibuffer window,
'prev' of the minibuffer window is the root window and 'next' of the
root window is the minibuffer window.  On minibuffer-less frames there
is only a root window and 'next' of the root window is nil.  On
minibuffer-only frames, the root window and the minibuffer window are
one and the same, so its 'prev' and 'next' members are nil.  In any
case, 'prev' of a root window and 'next' of a minibuffer window are
always nil.

In Lisp parlance, leaf windows are called "live windows" and non-leaf
windows are called "internal windows".  Together, live and internal
windows form the set of "valid windows".  A window that has been
deleted is considered "dead" regardless of whether it formerly was a
leaf or a non-leaf window.  A dead window has its 'contents' field set
to nil.

Frames may also contain pseudo windows, windows that are not exposed
directly to Lisp code.  Pseudo windows are currently either used to
display the menu bar or the tool bar (when Emacs uses toolkits that
don't display their own menu bar and tool bar) or a tooltip in a
tooltip frame (when tooltips are not display by the toolkit).  */

struct cursor_pos
{
  /* Pixel position.  These are always window relative.  */
  int x, y;

  /* Glyph matrix position.  */
  int hpos, vpos;
};

struct window
  {
    /* This is for Lisp; the terminal code does not refer to it.  */
    union vectorlike_header header;

    /* The frame this window is on.  */
    Lisp_Object frame;

    /* Following (to right or down) and preceding (to left or up)
       child at same level of tree.  Whether this is left/right or
       up/down is determined by the parent window's 'horizontal' flag,
       see below.  On a frame that is neither a minibuffer-only nor a
       minibuffer-less frame, 'next' of the root window points to the
       frame's minibuffer window and 'prev' of the minibuffer window
       points to the frame's root window.  In all other cases, 'next'
       of the root window and 'prev' of the minibuffer window, if
       present, are nil.  'prev' of the root window and 'next' of the
       minibuffer window are always nil.  */
    Lisp_Object next;
    Lisp_Object prev;

    /* The window this one is a child of.  For the root and a
       minibuffer window this is always nil.  */
    Lisp_Object parent;

    /* The "normal" size of the window.  These are fractions, but we
       do not use C doubles to avoid creating new Lisp_Float objects
       while interfacing Lisp in Fwindow_normal_size.  */
    Lisp_Object normal_lines;
    Lisp_Object normal_cols;

    /* The new sizes of the window as proposed by the window resizing
       functions.  Note that Lisp code may set new_normal to something
       beyond an integer, so C int can't be used here.  */
    Lisp_Object new_total;
    Lisp_Object new_normal;
    Lisp_Object new_pixel;

    /* For a leaf window or a tooltip window this is the buffer shown
       in the window; for a combination window this is the first of
       its child windows; for a pseudo window showing the menu bar or
       tool bar this is nil.  It is a buffer for a minibuffer window
       as well.  */
    Lisp_Object contents;

    /* The old buffer of this window, set to this window's buffer by
       run_window_change_functions every time it sees this window.
       Unused for internal windows.  */
    Lisp_Object old_buffer;

    /* A marker pointing to where in the text to start displaying.
       BIDI Note: This is the _logical-order_ start, i.e. the smallest
       buffer position visible in the window, not necessarily the
       character displayed in the top left corner of the window.  */
    Lisp_Object start;

    /* A marker pointing to where in the text point is in this window,
       used only when the window is not selected.
       This exists so that when multiple windows show one buffer
       each one can have its own value of point.  */
    Lisp_Object pointm;

    /* A marker pointing to where in the text point was in this window
       at the time of last redisplay.  The value is saved for the
       selected window too.  */
    Lisp_Object old_pointm;

    /* No permanent meaning; used by save-window-excursion's
       bookkeeping.  */
    Lisp_Object temslot;

    /* This window's vertical scroll bar.  This field is only for use by
       the window-system-dependent code which implements the scroll
       bars; it can store anything it likes here.  If this window is
       newly created and we haven't displayed a scroll bar in it yet, or
       if the frame doesn't have any scroll bars, this is nil.  */
    Lisp_Object vertical_scroll_bar;

    /* Type of vertical scroll bar.  A value of nil means
       no scroll bar.  A value of t means use frame value.  */
    Lisp_Object vertical_scroll_bar_type;

    /* This window's horizontal scroll bar.  This field is only for use
       by the window-system-dependent code which implements the scroll
       bars; it can store anything it likes here.  If this window is
       newly created and we haven't displayed a scroll bar in it yet, or
       if the frame doesn't have any scroll bars, this is nil.  */
    Lisp_Object horizontal_scroll_bar;

    /* Type of horizontal scroll bar.  A value of nil means
       no scroll bar.  A value of t means use frame value.  */
    Lisp_Object horizontal_scroll_bar_type;

    /* Display-table to use for displaying chars in this window.
       Nil means use the buffer's own display-table.  */
    Lisp_Object display_table;

    /* Non-nil usually means window is marked as dedicated.
       Note Lisp code may set this to something beyond Qnil
       and Qt, so bitfield can't be used here.  */
    Lisp_Object dedicated;

    /* If redisplay in this window goes beyond this buffer position,
       must run the redisplay-end-trigger-hook.  */
    Lisp_Object redisplay_end_trigger;

    /* t means this window's child windows are not (re-)combined.  */
    Lisp_Object combination_limit;

    /* An alist with parameters.  */
    Lisp_Object window_parameters;

    /* The help echo text for this window.  Qnil if there's none.  */
    Lisp_Object mode_line_help_echo;

    /* No Lisp data may follow this point; mode_line_help_echo must be
       the last Lisp member.  */

    /* Glyph matrices.  */
    struct glyph_matrix *current_matrix;
    struct glyph_matrix *desired_matrix;

    /* The two Lisp_Object fields below are marked in a special way,
       which is why they're placed after `current_matrix'.  */
    /* A list of <buffer, window-start, window-point> triples listing
       buffers previously shown in this window.  */
    Lisp_Object prev_buffers;
    /* List of buffers re-shown in this window.  */
    Lisp_Object next_buffers;

    /* Number saying how recently window was selected.  */
    EMACS_INT use_time;

    /* Unique number of window assigned when it was created.  */
    EMACS_INT sequence_number;

    /* The change stamp of this window.  Set to 0 when the window is
       created, it is set to its frame's change stamp every time
       run_window_change_functions is run on that frame with this
       window live.  It is left alone when the window exists only
       within a window configuration.  Not useful for internal
       windows.  */
    int change_stamp;

    /* The upper left corner pixel coordinates of this window, as
       integers relative to upper left corner of frame = 0, 0.  */
    int pixel_left;
    int pixel_top;

    /* The upper left corner coordinates of this window,
       relative to upper left corner of frame = 0, 0.  */
    int left_col;
    int top_line;

    /* The pixel size of the window.  */
    int pixel_width;
    int pixel_height;

    /* The pixel and pixel body sizes of the window at the last time
       run_window_change_functions was run with this window live.  Not
       useful for internal windows.  */
    int old_pixel_width;
    int old_pixel_height;
    int old_body_pixel_width;
    int old_body_pixel_height;

    /* The size of the window.  */
    int total_cols;
    int total_lines;

    /* Number of columns display within the window is scrolled to the left.  */
    ptrdiff_t hscroll;

    /* Minimum hscroll for automatic hscrolling.  This is the value
       the user has set, by set-window-hscroll for example.  */
    ptrdiff_t min_hscroll;

    /* Maximum line length in pixels within window bound by size of
       window (set up by set_horizontal_scroll_bar).  */
    ptrdiff_t hscroll_whole;

    /* Displayed buffer's text modification events counter as of last time
       display completed.  */
    modiff_count last_modified;

    /* Displayed buffer's overlays modification events counter as of last
       complete update.  */
    modiff_count last_overlay_modified;

    /* Value of point at that time.  Since this is a position in a buffer,
       it should be positive.  */
    ptrdiff_t last_point;

    /* Line number and position of a line somewhere above the top of the
       screen.  If this field is zero, it means we don't have a base line.  */
    ptrdiff_t base_line_number;

    /* If this field is zero, it means we don't have a base line.
       If it is -1, it means don't display the line number as long
       as the window shows its buffer.  */
    ptrdiff_t base_line_pos;

    /* The column number currently displayed in this window's mode
       line, or -1 if column numbers are not being displayed.  */
    ptrdiff_t column_number_displayed;

    /* Scaling factor for the glyph_matrix size calculation in this window.
       Used if window contains many small images or uses proportional fonts,
       as the normal may yield a matrix which is too small.  */
    int nrows_scale_factor, ncols_scale_factor;

    /* Intended cursor position.   This is a position within the
       glyph matrix.  */
    struct cursor_pos cursor;

    /* Where the cursor actually is.  */
    struct cursor_pos phys_cursor;

    /* Internally used for redisplay purposes.  */
    struct cursor_pos output_cursor;

    /* Vertical cursor position as of last update that completed
       without pause.  This is the position of last_point.  */
    int last_cursor_vpos;

#ifdef HAVE_WINDOW_SYSTEM

    /* Cursor type of last cursor drawn on the window.  */
    enum text_cursor_kinds phys_cursor_type;

    /* Width of the cursor above.  */
    int phys_cursor_width;

    /* This is handy for undrawing the cursor.  */
    int phys_cursor_ascent, phys_cursor_height;

#endif /* HAVE_WINDOW_SYSTEM */

    /* Width of left and right fringes, in pixels.
       A value of -1 means use frame values.  */
    int left_fringe_width;
    int right_fringe_width;

    /* Requested width of left and right marginal areas in columns.  A
       value of 0 means no margin.  The actual values are recorded in
       the window's glyph matrix, in the left_margin_glyphs and
       right_margin_glyphs members.  */
    int left_margin_cols;
    int right_margin_cols;

    /* Pixel width of scroll bars.
       A value of -1 means use frame values.  */
    int scroll_bar_width;

    /* Pixel height of scroll bars.
       A value of -1 means use frame values.  */
    int scroll_bar_height;

    /* Effective height of the mode line, or -1 if not known.  */
    int mode_line_height;

    /* Effective height of the header line, or -1 if not known.  */
    int header_line_height;

    /* Effective height of the tab line, or -1 if not known.  */
    int tab_line_height;

    /* Z - the buffer position of the last glyph in the current
       matrix of W.  Only valid if window_end_valid is true.  */
    ptrdiff_t window_end_pos;

    /* Glyph matrix row of the last glyph in the current matrix
       of W.  Only valid if window_end_valid is true.  */
    int window_end_vpos;

    /* True if this window is a minibuffer window.  */
    bool_bf mini : 1;

    /* Meaningful for internal windows only: true if this window is a
       horizontal combination, false if it is a vertical
       combination.  */
    bool_bf horizontal : 1;

    /* True means must regenerate mode line of this window.  */
    bool_bf update_mode_line : 1;

    /* True if the buffer was "modified" when the window
       was last updated.  */
    bool_bf last_had_star : 1;

    /* True means current value of `start'
       was the beginning of a line when it was chosen.  */
    bool_bf start_at_line_beg : 1;

    /* True means next redisplay must use the value of start
       set up for it in advance.  Set by scrolling commands.  */
    bool_bf force_start : 1;

    /* True means we have explicitly changed the value of start,
       but that the next redisplay is not obliged to use the new value.
       This is used in Fdelete_other_windows to force a call to
       Vwindow_scroll_functions; also by Frecenter with argument.  */
    bool_bf optional_new_start : 1;

    /* True means the cursor is currently displayed.  This can be
       set to zero by functions overpainting the cursor image.  */
    bool_bf phys_cursor_on_p : 1;

    /* False means cursor is logically on, true means it's off.  Used for
       blinking cursor.  */
    bool_bf cursor_off_p : 1;

    /* Value of cursor_off_p as of the last redisplay.  */
    bool_bf last_cursor_off_p : 1;

    /* True means desired matrix has been build and window must be
       updated in update_frame.  */
    bool_bf must_be_updated_p : 1;

    /* Flag indicating that this window is not a real one.
       Currently only used for menu bar windows, for tool bar windows,
       and for tooltips.  */
    bool_bf pseudo_window_p : 1;

    /* True means fringes are drawn outside display margins.
       Otherwise draw them between margin areas and text.  */
    bool_bf fringes_outside_margins : 1;

    /* True if this window's fringe specifications are persistent,
       i.e., always survive Fset_window_buffer.  */
    bool_bf fringes_persistent : 1;

    /* True if this window's scroll bar specifications are persistent,
       i.e., always survive Fset_window_buffer.  */
    bool_bf scroll_bars_persistent : 1;

    /* True if window_end_pos and window_end_vpos are truly valid.
       This is false if nontrivial redisplay is preempted since in that case
       the frame image that window_end_pos did not get onto the frame.  */
    bool_bf window_end_valid : 1;

    /* True if it needs to be redisplayed.  */
    bool_bf redisplay : 1;

    /* True if auto hscrolling is currently suspended in this
       window.  */
    bool_bf suspend_auto_hscroll : 1;

    /* Amount by which lines of this window are scrolled in
       y-direction (smooth scrolling).  */
    int vscroll;

    /* Z_BYTE - buffer position of the last glyph in the current matrix of W.
       Should be nonnegative, and only valid if window_end_valid is true.  */
    ptrdiff_t window_end_bytepos;
  } GCALIGNED_STRUCT;

INLINE bool
WINDOWP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_WINDOW);
}

INLINE void
CHECK_WINDOW (Lisp_Object x)
{
  CHECK_TYPE (WINDOWP (x), Qwindowp, x);
}

INLINE struct window *
XWINDOW (Lisp_Object a)
{
  eassert (WINDOWP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct window);
}

/* Most code should use these functions to set Lisp fields in struct
   window.  */
INLINE void
wset_frame (struct window *w, Lisp_Object val)
{
  w->frame = val;
}

INLINE void
wset_next (struct window *w, Lisp_Object val)
{
  w->next = val;
}

INLINE void
wset_prev (struct window *w, Lisp_Object val)
{
  w->prev = val;
}

INLINE void
wset_redisplay_end_trigger (struct window *w, Lisp_Object val)
{
  w->redisplay_end_trigger = val;
}

INLINE void
wset_mode_line_help_echo (struct window *w, Lisp_Object val)
{
  w->mode_line_help_echo = val;
}

INLINE void
wset_new_pixel (struct window *w, Lisp_Object val)
{
  w->new_pixel = val;
}

INLINE void
wset_vertical_scroll_bar (struct window *w, Lisp_Object val)
{
  w->vertical_scroll_bar = val;
}

INLINE void
wset_horizontal_scroll_bar (struct window *w, Lisp_Object val)
{
  w->horizontal_scroll_bar = val;
}

INLINE void
wset_horizontal_scroll_bar_type (struct window *w, Lisp_Object val)
{
  w->horizontal_scroll_bar_type = val;
}

INLINE void
wset_prev_buffers (struct window *w, Lisp_Object val)
{
  w->prev_buffers = val;
}

INLINE void
wset_next_buffers (struct window *w, Lisp_Object val)
{
  w->next_buffers = val;
}

/* True if W is a minibuffer window.  */
#define MINI_WINDOW_P(W) ((W)->mini)

/* True if W is a minibuffer window on a frame that contains at least
   one other window.  */
#define MINI_NON_ONLY_WINDOW_P(W)	 \
  (MINI_WINDOW_P (W) && !NILP ((W)->prev))

/* True if W is a minibuffer window that is alone on its frame.  */
#define MINI_ONLY_WINDOW_P(W)		 \
  (MINI_WINDOW_P (W) && NILP ((W)->prev))

/* General window layout:

   LEFT_EDGE_COL         RIGHT_EDGE_COL
   |                                  |
   |                                  |
   |  BOX_LEFT_EDGE_COL               |
   |  |           BOX_RIGHT_EDGE_COL  |
   |  |                            |  |
   v  v                            v  v
   <-><-><---><-----------><---><-><->
    ^  ^   ^        ^        ^   ^  ^
    |  |   |        |        |   |  |
    |  |   |        |        |   |  +-- RIGHT_SCROLL_BAR_COLS
    |  |   |        |        |   +----- RIGHT_FRINGE_WIDTH
    |  |   |        |        +--------- RIGHT_MARGIN_COLS
    |  |   |        |
    |  |   |        +------------------ TEXT_AREA_COLS
    |  |   |
    |  |   +--------------------------- LEFT_MARGIN_COLS
    |  +------------------------------- LEFT_FRINGE_WIDTH
    +---------------------------------- LEFT_SCROLL_BAR_COLS

*/


/* A handy macro.  */

/* Non-nil if window W is leaf window (has a buffer).  */
#define WINDOW_LEAF_P(W) \
  (BUFFERP ((W)->contents))

/* Non-nil if window W is internal (is a parent window).  */
#define WINDOW_INTERNAL_P(W) \
  (WINDOWP ((W)->contents))

/* True if window W is a horizontal combination of windows.  */
#define WINDOW_HORIZONTAL_COMBINATION_P(W) \
  (WINDOW_INTERNAL_P (W) && (W)->horizontal)

/* True if window W is a vertical combination of windows.  */
#define WINDOW_VERTICAL_COMBINATION_P(W) \
  (WINDOW_INTERNAL_P (W) && !(W)->horizontal)

/* Window W's XFRAME.  */
#define WINDOW_XFRAME(W) (XFRAME (WINDOW_FRAME ((W))))

/* Whether window W is a pseudo window.  */
#define WINDOW_PSEUDO_P(W) ((W)->pseudo_window_p)

/* Window W's buffer.  */
#define WINDOW_BUFFER(W)			\
  (WINDOW_LEAF_P(W)				\
   ? (W)->contents				\
   : Qnil)

/* Local value of variable V in window W's buffer.  Nil if W has no
   buffer.  */
#define WINDOW_BUFFER_LOCAL_VALUE(V, W)		\
  (BUFFERP ((W)->contents)			\
   ? buffer_local_value(V, (W)->contents)	\
   : Qnil)

/* Return the canonical column width of the frame of window W.  */
#define WINDOW_FRAME_COLUMN_WIDTH(W) \
  (FRAME_COLUMN_WIDTH (WINDOW_XFRAME ((W))))

/* Return the canonical line height of the frame of window W.  */
#define WINDOW_FRAME_LINE_HEIGHT(W) \
  (FRAME_LINE_HEIGHT (WINDOW_XFRAME ((W))))

/* Return the pixel width of window W.  This includes dividers, scroll
   bars, fringes and margins, if any.  */
#define WINDOW_PIXEL_WIDTH(W) (W)->pixel_width

/* Return the pixel height of window W.  This includes dividers, scroll
   bars, header and mode lines, if any.  */
#define WINDOW_PIXEL_HEIGHT(W) (W)->pixel_height

/* Return the width of window W in canonical column units.  This
   includes dividers, scroll bars, fringes and margins, if any.  The
   value is adjusted such that the sum of the widths of all child
   windows equals the width of their parent window.  */
#define WINDOW_TOTAL_COLS(W) (W)->total_cols

/* Return the height of window W in canonical line units.  This includes
   dividers, scroll bars, header and mode lines, if any.  The value is
   adjusted such that the sum of the heights of all child windows equals
   the height of their parent window.  */
#define WINDOW_TOTAL_LINES(W) (W)->total_lines

/* The smallest acceptable dimensions for a window.  Anything smaller
   might crash Emacs.  */
#define MIN_SAFE_WINDOW_WIDTH (2)

#define MIN_SAFE_WINDOW_PIXEL_WIDTH(W) \
  (2 * WINDOW_FRAME_COLUMN_WIDTH (W))

#define MIN_SAFE_WINDOW_HEIGHT (1)

#define MIN_SAFE_WINDOW_PIXEL_HEIGHT(W) \
  (WINDOW_FRAME_LINE_HEIGHT (W))

/* True if window W has no other windows to its left on its frame.  */
#define WINDOW_LEFTMOST_P(W)			\
  (WINDOW_LEFT_PIXEL_EDGE (W) == 0)

/* True if window W has no other windows above it on its frame.  */
#define WINDOW_TOPMOST_P(W)			\
  (WINDOW_TOP_PIXEL_EDGE (W) == 0)

/* True if window W has no other windows to its right on its frame.  */
#define WINDOW_RIGHTMOST_P(W)					\
  (WINDOW_RIGHT_PIXEL_EDGE (W)					\
   == (WINDOW_RIGHT_PIXEL_EDGE					\
       (XWINDOW (FRAME_ROOT_WINDOW (WINDOW_XFRAME (W))))))

/* True if window W has no other windows below it on its frame (the
   minibuffer window is not counted in this respect unless W itself is a
   minibuffer window).  */
#define WINDOW_BOTTOMMOST_P(W)					\
  (WINDOW_BOTTOM_PIXEL_EDGE (W)					\
   == (WINDOW_BOTTOM_PIXEL_EDGE					\
       (XWINDOW (FRAME_ROOT_WINDOW (WINDOW_XFRAME (W))))))

/* True if window W takes up the full width of its frame.  */
#define WINDOW_FULL_WIDTH_P(W)					\
  (WINDOW_PIXEL_WIDTH (W)					\
   == (WINDOW_PIXEL_WIDTH					\
       (XWINDOW (FRAME_ROOT_WINDOW (WINDOW_XFRAME (W))))))

/* Width of right divider of window W.  */
#define WINDOW_RIGHT_DIVIDER_WIDTH(W)				\
  (WINDOW_RIGHTMOST_P (W)					\
   ? 0 : FRAME_RIGHT_DIVIDER_WIDTH (WINDOW_XFRAME (W)))

/* Width of bottom divider of window W.  */
#define WINDOW_BOTTOM_DIVIDER_WIDTH(W)					\
  (((WINDOW_BOTTOMMOST_P (W)						\
     && NILP ((XWINDOW (FRAME_ROOT_WINDOW				\
			(WINDOW_XFRAME (W))))->next))			\
    || EQ ((W)->prev, FRAME_ROOT_WINDOW (WINDOW_XFRAME (W)))		\
    || (W)->pseudo_window_p)						\
   ? 0 : FRAME_BOTTOM_DIVIDER_WIDTH (WINDOW_XFRAME (W)))

/* Return the canonical frame column at which window W starts.
   This includes a left-hand scroll bar, if any.  */
#define WINDOW_LEFT_EDGE_COL(W) (W)->left_col

/* Return the canonical frame column before which window W ends.
   This includes a right-hand scroll bar, if any.  */
#define WINDOW_RIGHT_EDGE_COL(W) \
  (WINDOW_LEFT_EDGE_COL (W) + WINDOW_TOTAL_COLS (W))

/* Return the canonical frame line at which window W starts.
   This includes a header/tab line, if any.  */
#define WINDOW_TOP_EDGE_LINE(W) (W)->top_line

/* Return the canonical frame line before which window W ends.
   This includes a mode line, if any.  */
#define WINDOW_BOTTOM_EDGE_LINE(W) \
  (WINDOW_TOP_EDGE_LINE (W) + WINDOW_TOTAL_LINES (W))

/* Return the left pixel edge at which window W starts.
   This includes a left-hand scroll bar, if any.  */
#define WINDOW_LEFT_PIXEL_EDGE(W) (W)->pixel_left

/* Return the right pixel edge before which window W ends.
   This includes a right-hand scroll bar, if any.  */
#define WINDOW_RIGHT_PIXEL_EDGE(W) \
  (WINDOW_LEFT_PIXEL_EDGE (W) + WINDOW_PIXEL_WIDTH (W))

/* Return the top pixel edge at which window W starts.
   This includes a header/tab line, if any.  */
#define WINDOW_TOP_PIXEL_EDGE(W) (W)->pixel_top

/* Return the bottom pixel edge before which window W ends.
   This includes a mode line, if any.  */
#define WINDOW_BOTTOM_PIXEL_EDGE(W) \
  (WINDOW_TOP_PIXEL_EDGE (W) + WINDOW_PIXEL_HEIGHT (W))

/* Return the frame x-position at which window W starts.
   This includes a left-hand scroll bar, if any.  */
#define WINDOW_LEFT_EDGE_X(W) \
  (FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W)) \
   + WINDOW_LEFT_PIXEL_EDGE (W))

/* Return the frame x- position before which window W ends.
   This includes a right-hand scroll bar, if any.  */
#define WINDOW_RIGHT_EDGE_X(W) \
  (FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W)) \
   + WINDOW_RIGHT_PIXEL_EDGE (W))

/* True if W is a menu bar window.  */
#if defined (HAVE_X_WINDOWS) && ! defined (USE_X_TOOLKIT) && ! defined (USE_GTK)
#define WINDOW_MENU_BAR_P(W) \
  (WINDOWP (WINDOW_XFRAME (W)->menu_bar_window) \
   && (W) == XWINDOW (WINDOW_XFRAME (W)->menu_bar_window))
#else
/* No menu bar windows if X toolkit is in use.  */
#define WINDOW_MENU_BAR_P(W) false
#endif

/* True if W is a tab bar window.  */
#if defined (HAVE_WINDOW_SYSTEM)
# define WINDOW_TAB_BAR_P(W) \
   (WINDOWP (WINDOW_XFRAME (W)->tab_bar_window) \
    && (W) == XWINDOW (WINDOW_XFRAME (W)->tab_bar_window))
#else
# define WINDOW_TAB_BAR_P(W) false
#endif

/* True if W is a tool bar window.  */
#if defined (HAVE_WINDOW_SYSTEM) && ! defined (HAVE_EXT_TOOL_BAR)
#define WINDOW_TOOL_BAR_P(W) \
  (WINDOWP (WINDOW_XFRAME (W)->tool_bar_window) \
   && (W) == XWINDOW (WINDOW_XFRAME (W)->tool_bar_window))
#else
#define WINDOW_TOOL_BAR_P(W) false
#endif

/* Return the frame y-position at which window W starts.  */
#define WINDOW_TOP_EDGE_Y(W) \
  (((WINDOW_MENU_BAR_P (W) || WINDOW_TAB_BAR_P (W) || WINDOW_TOOL_BAR_P (W)) \
    ? 0 : FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W))) \
   + WINDOW_TOP_PIXEL_EDGE (W))

/* Return the frame y-position before which window W ends.  */
#define WINDOW_BOTTOM_EDGE_Y(W)				   \
  (((WINDOW_MENU_BAR_P (W) || WINDOW_TAB_BAR_P (W) || WINDOW_TOOL_BAR_P (W))	   \
    ? 0 : FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W))) \
   + WINDOW_BOTTOM_PIXEL_EDGE (W))

/* Return the pixel value where the text (or left fringe) in window W
   starts.  */
#define WINDOW_BOX_LEFT_PIXEL_EDGE(W)		\
  (WINDOW_LEFT_PIXEL_EDGE (W)			\
   + WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH (W))

/* Return the pixel value before which the text in window W ends.  This
   is different from the `RIGHT_EDGE' because it does not include a
   right-hand scroll bar or window-separating line on the right
   edge.  */
#define WINDOW_BOX_RIGHT_PIXEL_EDGE(W)		\
  (WINDOW_RIGHT_PIXEL_EDGE (W)			\
   - WINDOW_RIGHT_DIVIDER_WIDTH (W)		\
   - WINDOW_RIGHT_SCROLL_BAR_AREA_WIDTH (W))

/* Return the frame x-position at which the text (or left fringe) in
   window W starts.  This does not include a left-hand scroll bar if
   any.  */
#define WINDOW_BOX_LEFT_EDGE_X(W)		   \
  (FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W)) \
   + WINDOW_BOX_LEFT_PIXEL_EDGE (W))

/* Return the frame x-position before which the text in window W ends.
   This does not include a scroll bar, divider or window-separating line
   on the right edge.  */
#define WINDOW_BOX_RIGHT_EDGE_X(W)		   \
  (FRAME_INTERNAL_BORDER_WIDTH (WINDOW_XFRAME (W)) \
   + WINDOW_BOX_RIGHT_PIXEL_EDGE (W))

/* Widths of marginal areas in columns.  */
#define WINDOW_LEFT_MARGIN_COLS(W) (W->left_margin_cols)

#define WINDOW_RIGHT_MARGIN_COLS(W) (W->right_margin_cols)

#define WINDOW_MARGINS_COLS(W)			\
  (WINDOW_LEFT_MARGIN_COLS (W)			\
   + WINDOW_RIGHT_MARGIN_COLS (W))

/* Widths of marginal areas in pixels.  */
#define WINDOW_LEFT_MARGIN_WIDTH(W)			\
  (W->left_margin_cols * WINDOW_FRAME_COLUMN_WIDTH (W))

#define WINDOW_RIGHT_MARGIN_WIDTH(W)				\
  (W->right_margin_cols * WINDOW_FRAME_COLUMN_WIDTH (W))

#define WINDOW_MARGINS_WIDTH(W)			\
  (WINDOW_LEFT_MARGIN_WIDTH (W)			\
   + WINDOW_RIGHT_MARGIN_WIDTH (W))

/* Pixel-widths of fringes.  */
#define WINDOW_LEFT_FRINGE_WIDTH(W)			\
  (W->left_fringe_width >= 0				\
   ? W->left_fringe_width				\
   : FRAME_LEFT_FRINGE_WIDTH (WINDOW_XFRAME (W)))

#define WINDOW_RIGHT_FRINGE_WIDTH(W)			\
  (W->right_fringe_width >= 0				\
   ? W->right_fringe_width				\
   : FRAME_RIGHT_FRINGE_WIDTH (WINDOW_XFRAME (W)))

#define WINDOW_FRINGES_WIDTH(W)		\
  (WINDOW_LEFT_FRINGE_WIDTH (W) + WINDOW_RIGHT_FRINGE_WIDTH (W))

/* Are fringes outside display margins in window W.  */
#define WINDOW_HAS_FRINGES_OUTSIDE_MARGINS(W)	\
  ((W)->fringes_outside_margins)

/* Say whether vertical scroll bars are currently enabled for window W,
   and which side they are on.  */
#define WINDOW_VERTICAL_SCROLL_BAR_TYPE(W)		\
  (WINDOW_PSEUDO_P (W)					\
   ? vertical_scroll_bar_none				\
   : EQ (W->vertical_scroll_bar_type, Qt)		\
   ? FRAME_VERTICAL_SCROLL_BAR_TYPE (WINDOW_XFRAME (W))	\
   : EQ (W->vertical_scroll_bar_type, Qleft)		\
   ? vertical_scroll_bar_left				\
   : EQ (W->vertical_scroll_bar_type, Qright)		\
   ? vertical_scroll_bar_right				\
   : vertical_scroll_bar_none)

#define WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT(W)			\
  (WINDOW_VERTICAL_SCROLL_BAR_TYPE (W) == vertical_scroll_bar_left)

#define WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT(W)			\
  (WINDOW_VERTICAL_SCROLL_BAR_TYPE (W) == vertical_scroll_bar_right)

#define WINDOW_HAS_VERTICAL_SCROLL_BAR(W)		\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (W)		\
   || WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (W))

#if (defined (HAVE_WINDOW_SYSTEM)					\
     && ((defined (USE_TOOLKIT_SCROLL_BARS))	\
	 || defined (HAVE_NTGUI)))
# define USE_HORIZONTAL_SCROLL_BARS true
#else
# define USE_HORIZONTAL_SCROLL_BARS false
#endif

/* Say whether horizontal scroll bars are currently enabled for window
   W.  Horizontal scrollbars exist for toolkit versions only.  */
#if USE_HORIZONTAL_SCROLL_BARS
#define WINDOW_HAS_HORIZONTAL_SCROLL_BAR(W)			\
  ((WINDOW_PSEUDO_P (W)						\
    || (MINI_WINDOW_P (W)					\
	&& !EQ (W->horizontal_scroll_bar_type, Qbottom)))	\
   ? false							\
   : EQ (W->horizontal_scroll_bar_type, Qt)			\
   ? FRAME_HAS_HORIZONTAL_SCROLL_BARS (WINDOW_XFRAME (W))	\
   : EQ (W->horizontal_scroll_bar_type, Qbottom)		\
   ? true							\
   : false)
#else
#define WINDOW_HAS_HORIZONTAL_SCROLL_BAR(W) false
#endif

/* Width that a scroll bar in window W should have, if there is one.
   Measured in pixels.  If scroll bars are turned off, this is still
   nonzero.  */
#define WINDOW_CONFIG_SCROLL_BAR_WIDTH(W)		\
  (W->scroll_bar_width >= 0				\
   ? W->scroll_bar_width				\
   : FRAME_CONFIG_SCROLL_BAR_WIDTH (WINDOW_XFRAME (W)))

/* Width that a scroll bar in window W should have, if there is one.
   Measured in columns (characters).  If scroll bars are turned off,
   this is still nonzero.  */
#define WINDOW_CONFIG_SCROLL_BAR_COLS(W)		\
  (W->scroll_bar_width >= 0				\
   ? ((W->scroll_bar_width				\
       + WINDOW_FRAME_COLUMN_WIDTH (W) - 1)		\
      / WINDOW_FRAME_COLUMN_WIDTH (W))			\
   : FRAME_CONFIG_SCROLL_BAR_COLS (WINDOW_XFRAME (W)))

/* Width of left scroll bar in window W, measured in columns
   (characters).  If scroll bars are on the right in this frame, or
   there are no scroll bars, value is 0.  */
#define WINDOW_LEFT_SCROLL_BAR_COLS(W)	       \
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (W)  \
   ? (WINDOW_CONFIG_SCROLL_BAR_COLS (W))       \
   : 0)

/* Width of right scroll bar in window W, measured in columns
   (characters).  If scroll bars are on the left in this frame, or there
   are no scroll bars, value is 0.  */
#define WINDOW_RIGHT_SCROLL_BAR_COLS(W)		\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (W)	\
   ? WINDOW_CONFIG_SCROLL_BAR_COLS (W)		\
   : 0)

/* Width of a scroll bar in window W, measured in columns.  */
#define WINDOW_SCROLL_BAR_COLS(W)	       \
  (WINDOW_HAS_VERTICAL_SCROLL_BAR (W)	       \
   ? WINDOW_CONFIG_SCROLL_BAR_COLS (W)	       \
   : 0)

/* Width of a left scroll bar area in window W, measured in pixels.  */
#define WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH(W)				\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (W)				\
   ? WINDOW_CONFIG_SCROLL_BAR_WIDTH (W)					\
   : 0)

/* Width of a right scroll bar area in window W, measured in pixels.  */
#define WINDOW_RIGHT_SCROLL_BAR_AREA_WIDTH(W)				\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (W)				\
   ? WINDOW_CONFIG_SCROLL_BAR_WIDTH (W)					\
   : 0)

/* Width of scroll bar area in window W, measured in pixels.  */
#define WINDOW_SCROLL_BAR_AREA_WIDTH(W)					\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR (W)					\
   ? WINDOW_CONFIG_SCROLL_BAR_WIDTH (W)					\
   : 0)

/* Return the frame position where the vertical scroll bar of window W
   starts.  */
#define WINDOW_SCROLL_BAR_AREA_X(W)		\
  (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (W)	\
   ? WINDOW_BOX_RIGHT_EDGE_X (W)		\
   : WINDOW_LEFT_EDGE_X (W))

/* Height that a scroll bar in window W should have, if there is one.
   Measured in pixels.  If scroll bars are turned off, this is still
   nonzero.  */
#define WINDOW_CONFIG_SCROLL_BAR_HEIGHT(W)			\
  (W->scroll_bar_height >= 0					\
   ? W->scroll_bar_height					\
   : FRAME_CONFIG_SCROLL_BAR_HEIGHT (WINDOW_XFRAME (W)))

/* Height that a scroll bar in window W should have, if there is one.
   Measured in lines (characters).  If scroll bars are turned off, this
   is still nonzero.  */
#define WINDOW_CONFIG_SCROLL_BAR_LINES(W)		\
  (W->scroll_bar_height >= 0				\
   ? ((W->scroll_bar_height				\
       + WINDOW_FRAME_LINE_HEIGHT (W) - 1)		\
      / WINDOW_FRAME_LINE_HEIGHT (W))			\
   : FRAME_CONFIG_SCROLL_BAR_LINES (WINDOW_XFRAME (W)))

/* Height of a scroll bar in window W, measured in columns.  */
#define WINDOW_SCROLL_BAR_LINES(W)	       \
  (WINDOW_HAS_HORIZONTAL_SCROLL_BAR (W)	       \
   ? WINDOW_CONFIG_SCROLL_BAR_LINES (W)	       \
   : 0)

/* Height of scroll bar area in window W, measured in pixels.  */
#define WINDOW_SCROLL_BAR_AREA_HEIGHT(W)	\
  (WINDOW_HAS_HORIZONTAL_SCROLL_BAR (W)		\
   ? WINDOW_CONFIG_SCROLL_BAR_HEIGHT (W)	\
   : 0)

/* Height in pixels of the mode line.
   May be zero if W doesn't have a mode line.  */
#define WINDOW_MODE_LINE_HEIGHT(W)	\
  (window_wants_mode_line ((W))		\
   ? CURRENT_MODE_LINE_HEIGHT (W)	\
   : 0)

#define WINDOW_MODE_LINE_LINES(W)	\
  window_wants_mode_line (W)

/* Height in pixels of the header line.
   Zero if W doesn't have a header line.  */
#define WINDOW_HEADER_LINE_HEIGHT(W)	\
  (window_wants_header_line (W)		\
   ? CURRENT_HEADER_LINE_HEIGHT (W)	\
   : 0)

#define WINDOW_HEADER_LINE_LINES(W)	\
  window_wants_header_line (W)

/* Height in pixels of the tab line.
   Zero if W doesn't have a tab line.  */
#define WINDOW_TAB_LINE_HEIGHT(W)	\
  (window_wants_tab_line (W)		\
   ? CURRENT_TAB_LINE_HEIGHT (W)	\
   : 0)

#define WINDOW_TAB_LINE_LINES(W)	\
  window_wants_tab_line (W)

/* Pixel height of window W without mode line, bottom scroll bar and
   bottom divider.  */
#define WINDOW_BOX_HEIGHT_NO_MODE_LINE(W)	\
  (WINDOW_PIXEL_HEIGHT (W)			\
   - WINDOW_BOTTOM_DIVIDER_WIDTH (W)		\
   - WINDOW_SCROLL_BAR_AREA_HEIGHT (W)		\
   - WINDOW_MODE_LINE_HEIGHT (W))

/* Pixel height of window W without mode and header/tab line and bottom
   divider.  */
#define WINDOW_BOX_TEXT_HEIGHT(W)	\
  (WINDOW_PIXEL_HEIGHT ((W))		\
   - WINDOW_BOTTOM_DIVIDER_WIDTH (W)	\
   - WINDOW_SCROLL_BAR_AREA_HEIGHT (W)	\
   - WINDOW_MODE_LINE_HEIGHT (W)	\
   - WINDOW_HEADER_LINE_HEIGHT (W)	\
   - WINDOW_TAB_LINE_HEIGHT (W))

/* Return the frame position where the horizontal scroll bar of window W
   starts.  */
#define WINDOW_SCROLL_BAR_AREA_Y(W)			\
  (WINDOW_TOP_EDGE_Y (W)				\
   + (WINDOW_HAS_HORIZONTAL_SCROLL_BAR (W)		\
      ? WINDOW_BOX_HEIGHT_NO_MODE_LINE (W) : 0))

/* Convert window W relative pixel X to frame pixel coordinates.  */
#define WINDOW_TO_FRAME_PIXEL_X(W, X)	\
  ((X) + WINDOW_BOX_LEFT_EDGE_X ((W)))

/* Convert window W relative pixel Y to frame pixel coordinates.  */
#define WINDOW_TO_FRAME_PIXEL_Y(W, Y)		\
  ((Y) + WINDOW_TOP_EDGE_Y (W))

/* Convert frame relative pixel X to window relative pixel X.  */
#define FRAME_TO_WINDOW_PIXEL_X(W, X)		\
  ((X) - WINDOW_BOX_LEFT_EDGE_X ((W)))

/* Convert frame relative pixel Y to window relative pixel Y.  */
#define FRAME_TO_WINDOW_PIXEL_Y(W, Y)		\
  ((Y) - WINDOW_TOP_EDGE_Y (W))

/* Convert a text area relative x-position in window W to frame X
   pixel coordinates.  */
#define WINDOW_TEXT_TO_FRAME_PIXEL_X(W, X)	\
  (window_box_left ((W), TEXT_AREA) + (X))

/* This is the window in which the terminal's cursor should be left when
   nothing is being done with it.  This must always be a leaf window, and its
   buffer is selected by the top level editing loop at the end of each command.

   This value is always the same as FRAME_SELECTED_WINDOW (selected_frame).  */

extern Lisp_Object selected_window;

/* This is a time stamp for window selection, so we can find the least
   recently used window.  Its only users are Fselect_window,
   init_window_once, and make_frame.  */

extern EMACS_INT window_select_count;

/* The minibuffer window of the selected frame.
   Note that you cannot test for minibufferness of an arbitrary window
   by comparing against this; use the MINI_WINDOW_P macro instead.  */

extern Lisp_Object minibuf_window;

/* Non-nil means it is the window whose mode line should be
   shown as the selected window when the minibuffer is selected.  */

extern Lisp_Object minibuf_selected_window;

extern Lisp_Object make_window (void);
extern Lisp_Object window_from_coordinates (struct frame *, int, int,
                                            enum window_part *, bool, bool);
extern void resize_frame_windows (struct frame *, int, bool);
extern void restore_window_configuration (Lisp_Object);
extern void delete_all_child_windows (Lisp_Object);
extern void grow_mini_window (struct window *, int);
extern void shrink_mini_window (struct window *);
extern int window_relative_x_coord (struct window *, enum window_part, int);

void run_window_change_functions (void);

/* Make WINDOW display BUFFER.  RUN_HOOKS_P means it's allowed
   to run hooks.  See make_frame for a case where it's not allowed.  */

void set_window_buffer (Lisp_Object window, Lisp_Object buffer,
                        bool run_hooks_p, bool keep_margins_p);

/* This is the window where the echo area message was displayed.  It
   is always a minibuffer window, but it may not be the same window
   currently active as a minibuffer.  */

extern Lisp_Object echo_area_window;

/* Depth in recursive edits.  */

extern EMACS_INT command_loop_level;

/* Depth in minibuffer invocations.  */

extern EMACS_INT minibuf_level;

/* Non-zero if we should redraw the mode lines on the next redisplay.
   Usually set to a unique small integer so we can track the main causes of
   full redisplays in `redisplay--mode-lines-cause'.  */

extern int update_mode_lines;

/* Nonzero if window sizes or contents have changed since last
   redisplay that finished.  Usually set to a unique small integer so
   we can track the main causes of full redisplays in
   `redisplay--all-windows-cause'.  */

extern int windows_or_buffers_changed;

/* The main redisplay routine usually only redisplays the selected-window,
   so when something's changed elsewhere, we call one of the functions below
   to indicate which other windows might also need to be redisplayed.  */

extern void wset_redisplay (struct window *w);
extern void fset_redisplay (struct frame *f);
extern void bset_redisplay (struct buffer *b);
extern void bset_update_mode_line (struct buffer *b);
/* Call this to tell redisplay to look for other windows than selected-window
   that need to be redisplayed.  Calling one of the *set_redisplay functions
   above already does it, so it's only needed in unusual cases.  */
extern void redisplay_other_windows (void);

/* Return a pointer to the glyph W's physical cursor is on.  Value is
   null if W's current matrix is invalid, so that no meaningful glyph
   can be returned.  */

struct glyph *get_phys_cursor_glyph (struct window *w);

/* True if WINDOW is a valid window.  */
#define WINDOW_VALID_P(WINDOW)					\
  (WINDOWP (WINDOW) && !NILP (XWINDOW (WINDOW)->contents))

/* A window of any sort, leaf or interior, is "valid" if its
   contents slot is non-nil.  */
#define CHECK_VALID_WINDOW(WINDOW)				\
  CHECK_TYPE (WINDOW_VALID_P (WINDOW), Qwindow_valid_p, WINDOW)

/* True if WINDOW is a live window.  */
#define WINDOW_LIVE_P(WINDOW)					\
  (WINDOWP (WINDOW) && BUFFERP (XWINDOW (WINDOW)->contents))

/* A window is "live" if and only if it shows a buffer.  */
#define CHECK_LIVE_WINDOW(WINDOW)				\
  CHECK_TYPE (WINDOW_LIVE_P (WINDOW), Qwindow_live_p, WINDOW)

/* These used to be in lisp.h.  */
extern Lisp_Object Vwindow_list;

extern Lisp_Object window_list (void);
extern Lisp_Object window_parameter (struct window *, Lisp_Object parameter);
extern struct window *decode_live_window (Lisp_Object);
extern struct window *decode_any_window (Lisp_Object);
extern bool compare_window_configurations (Lisp_Object, Lisp_Object, bool);
extern void mark_window_cursors_off (struct window *);
extern bool window_wants_mode_line (struct window *);
extern bool window_wants_header_line (struct window *);
extern bool window_wants_tab_line (struct window *);
extern int window_internal_height (struct window *);
extern int window_body_width (struct window *w, bool);
enum margin_unit { MARGIN_IN_LINES, MARGIN_IN_PIXELS };
extern int window_scroll_margin (struct window *, enum margin_unit);
extern void temp_output_buffer_show (Lisp_Object);
extern void replace_buffer_in_windows (Lisp_Object);
extern void replace_buffer_in_windows_safely (Lisp_Object);
extern void sanitize_window_sizes (Lisp_Object horizontal);
/* This looks like a setter, but it is a bit special.  */
extern void wset_buffer (struct window *, Lisp_Object);
extern bool window_outdated (struct window *);
extern void init_window_once (void);
extern void init_window (void);
extern void syms_of_window (void);
extern void keys_of_window (void);
/* Move cursor to row/column position VPOS/HPOS, pixel coordinates
   Y/X. HPOS/VPOS are window-relative row and column numbers and X/Y
   are window-relative pixel positions.  This is always done during
   window update, so the position is the future output cursor position
   for currently updated window W.  */

INLINE void
output_cursor_to (struct window *w, int vpos, int hpos, int y, int x)
{
  eassert (w);
  w->output_cursor.hpos = hpos;
  w->output_cursor.vpos = vpos;
  w->output_cursor.x = x;
  w->output_cursor.y = y;
}

INLINE_HEADER_END

#endif /* not WINDOW_H_INCLUDED */
