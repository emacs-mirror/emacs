/* Display generation from window structure and buffer text.
   Copyright (C) 1985, 86, 87, 88, 93, 94, 95, 97, 98, 99, 2000, 2001, 2002
   Free Software Foundation, Inc.

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

/* New redisplay written by Gerd Moellmann <gerd@gnu.org>.

   Redisplay.

   Emacs separates the task of updating the display from code
   modifying global state, e.g. buffer text.  This way functions
   operating on buffers don't also have to be concerned with updating
   the display.

   Updating the display is triggered by the Lisp interpreter when it
   decides it's time to do it.  This is done either automatically for
   you as part of the interpreter's command loop or as the result of
   calling Lisp functions like `sit-for'.  The C function `redisplay'
   in xdisp.c is the only entry into the inner redisplay code.  (Or,
   let's say almost---see the description of direct update
   operations, below.)

   The following diagram shows how redisplay code is invoked.  As you
   can see, Lisp calls redisplay and vice versa.  Under window systems
   like X, some portions of the redisplay code are also called
   asynchronously during mouse movement or expose events.  It is very
   important that these code parts do NOT use the C library (malloc,
   free) because many C libraries under Unix are not reentrant.  They
   may also NOT call functions of the Lisp interpreter which could
   change the interpreter's state.  If you don't follow these rules,
   you will encounter bugs which are very hard to explain.

	     (Direct functions, see below)
             direct_output_for_insert, 
             direct_forward_char (dispnew.c)
   	  +---------------------------------+
          |                                 |
	  |                                 V
   +--------------+   redisplay     +----------------+
   | Lisp machine |---------------->| Redisplay code |<--+
   +--------------+   (xdisp.c)     +----------------+   |
	  ^				     |		 |
	  +----------------------------------+           |
	    Don't use this path when called		 |
	    asynchronously!				 |
                                                         |
                           expose_window (asynchronous)  |
                                                         |
			           X expose events  -----+

   What does redisplay?  Obviously, it has to figure out somehow what
   has been changed since the last time the display has been updated,
   and to make these changes visible.  Preferably it would do that in
   a moderately intelligent way, i.e. fast.

   Changes in buffer text can be deduced from window and buffer
   structures, and from some global variables like `beg_unchanged' and
   `end_unchanged'.  The contents of the display are additionally
   recorded in a `glyph matrix', a two-dimensional matrix of glyph
   structures.  Each row in such a matrix corresponds to a line on the
   display, and each glyph in a row corresponds to a column displaying
   a character, an image, or what else.  This matrix is called the
   `current glyph matrix' or `current matrix' in redisplay
   terminology.

   For buffer parts that have been changed since the last update, a
   second glyph matrix is constructed, the so called `desired glyph
   matrix' or short `desired matrix'.  Current and desired matrix are
   then compared to find a cheap way to update the display, e.g. by
   reusing part of the display by scrolling lines.


   Direct operations.

   You will find a lot of redisplay optimizations when you start
   looking at the innards of redisplay.  The overall goal of all these
   optimizations is to make redisplay fast because it is done
   frequently.

   Two optimizations are not found in xdisp.c.  These are the direct
   operations mentioned above.  As the name suggests they follow a
   different principle than the rest of redisplay.  Instead of
   building a desired matrix and then comparing it with the current
   display, they perform their actions directly on the display and on
   the current matrix.

   One direct operation updates the display after one character has
   been entered.  The other one moves the cursor by one position
   forward or backward.  You find these functions under the names
   `direct_output_for_insert' and `direct_output_forward_char' in
   dispnew.c.

   
   Desired matrices.

   Desired matrices are always built per Emacs window.  The function
   `display_line' is the central function to look at if you are
   interested.  It constructs one row in a desired matrix given an
   iterator structure containing both a buffer position and a
   description of the environment in which the text is to be
   displayed.  But this is too early, read on.

   Characters and pixmaps displayed for a range of buffer text depend
   on various settings of buffers and windows, on overlays and text
   properties, on display tables, on selective display.  The good news
   is that all this hairy stuff is hidden behind a small set of
   interface functions taking a iterator structure (struct it)
   argument.

   Iteration over things to be displayed is then simple.  It is
   started by initializing an iterator with a call to init_iterator.
   Calls to get_next_display_element fill the iterator structure with
   relevant information about the next thing to display.  Calls to
   set_iterator_to_next move the iterator to the next thing.

   Besides this, an iterator also contains information about the
   display environment in which glyphs for display elements are to be
   produced.  It has fields for the width and height of the display,
   the information whether long lines are truncated or continued, a
   current X and Y position, and lots of other stuff you can better
   see in dispextern.h.

   Glyphs in a desired matrix are normally constructed in a loop
   calling get_next_display_element and then produce_glyphs.  The call
   to produce_glyphs will fill the iterator structure with pixel
   information about the element being displayed and at the same time
   produce glyphs for it.  If the display element fits on the line
   being displayed, set_iterator_to_next is called next, otherwise the
   glyphs produced are discarded.


   Frame matrices.

   That just couldn't be all, could it?  What about terminal types not
   supporting operations on sub-windows of the screen?  To update the
   display on such a terminal, window-based glyph matrices are not
   well suited.  To be able to reuse part of the display (scrolling
   lines up and down), we must instead have a view of the whole
   screen.  This is what `frame matrices' are for.  They are a trick.

   Frames on terminals like above have a glyph pool.  Windows on such
   a frame sub-allocate their glyph memory from their frame's glyph
   pool.  The frame itself is given its own glyph matrices.  By
   coincidence---or maybe something else---rows in window glyph
   matrices are slices of corresponding rows in frame matrices.  Thus
   writing to window matrices implicitly updates a frame matrix which
   provides us with the view of the whole screen that we originally
   wanted to have without having to move many bytes around.  To be
   honest, there is a little bit more done, but not much more.  If you
   plan to extend that code, take a look at dispnew.c.  The function
   build_frame_matrix is a good starting point.  */

#include <config.h>
#include <stdio.h>

#include "lisp.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "termchar.h"
#include "dispextern.h"
#include "buffer.h"
#include "charset.h"
#include "indent.h"
#include "commands.h"
#include "macros.h"
#include "disptab.h"
#include "termhooks.h"
#include "intervals.h"
#include "coding.h"
#include "process.h"
#include "region-cache.h"
#include "fontset.h"

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif
#ifdef WINDOWSNT
#include "w32term.h"
#endif
#ifdef macintosh
#include "macterm.h"
#endif

#define INFINITY 10000000

#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) || defined (macintosh)
extern void set_frame_menubar P_ ((struct frame *f, int, int));
extern int pending_menu_activation;
#endif

extern int interrupt_input;
extern int command_loop_level;

extern int minibuffer_auto_raise;

extern Lisp_Object Qface;

extern Lisp_Object Voverriding_local_map;
extern Lisp_Object Voverriding_local_map_menu_flag;
extern Lisp_Object Qmenu_item;

Lisp_Object Qoverriding_local_map, Qoverriding_terminal_local_map;
Lisp_Object Qwindow_scroll_functions, Vwindow_scroll_functions;
Lisp_Object Qredisplay_end_trigger_functions;
Lisp_Object Qinhibit_point_motion_hooks;
Lisp_Object QCeval, Qwhen, QCfile, QCdata, QCpropertize;
Lisp_Object Qfontified;
Lisp_Object Qgrow_only;
Lisp_Object Qinhibit_eval_during_redisplay;
Lisp_Object Qbuffer_position, Qposition, Qobject;

/* Holds the list (error).  */
Lisp_Object list_of_error;

/* Functions called to fontify regions of text.  */

Lisp_Object Vfontification_functions;
Lisp_Object Qfontification_functions;

/* Non-zero means draw tool bar buttons raised when the mouse moves
   over them.  */

int auto_raise_tool_bar_buttons_p;

/* Margin around tool bar buttons in pixels.  */

Lisp_Object Vtool_bar_button_margin;

/* Thickness of shadow to draw around tool bar buttons.  */

EMACS_INT tool_bar_button_relief;

/* Non-zero means automatically resize tool-bars so that all tool-bar
   items are visible, and no blank lines remain.  */

int auto_resize_tool_bars_p;

/* Non-nil means don't actually do any redisplay.  */

Lisp_Object Vinhibit_redisplay, Qinhibit_redisplay;

/* Non-zero means Lisp evaluation during redisplay is inhibited.  */

int inhibit_eval_during_redisplay;

/* Names of text properties relevant for redisplay.  */

Lisp_Object Qdisplay, Qrelative_width, Qalign_to;
extern Lisp_Object Qface, Qinvisible, Qwidth;

/* Symbols used in text property values.  */

Lisp_Object Qspace, QCalign_to, QCrelative_width, QCrelative_height;
Lisp_Object Qleft_margin, Qright_margin, Qspace_width, Qraise;
Lisp_Object Qmargin;
extern Lisp_Object Qheight;

/* Non-nil means highlight trailing whitespace.  */

Lisp_Object Vshow_trailing_whitespace;

/* Name of the face used to highlight trailing whitespace.  */

Lisp_Object Qtrailing_whitespace;

/* The symbol `image' which is the car of the lists used to represent
   images in Lisp.  */

Lisp_Object Qimage;

/* Non-zero means print newline to stdout before next mini-buffer
   message.  */

int noninteractive_need_newline;

/* Non-zero means print newline to message log before next message.  */

static int message_log_need_newline;

/* Three markers that message_dolog uses.
   It could allocate them itself, but that causes trouble
   in handling memory-full errors.  */
static Lisp_Object message_dolog_marker1;
static Lisp_Object message_dolog_marker2;
static Lisp_Object message_dolog_marker3;

/* The buffer position of the first character appearing entirely or
   partially on the line of the selected window which contains the
   cursor; <= 0 if not known.  Set by set_cursor_from_row, used for
   redisplay optimization in redisplay_internal.  */

static struct text_pos this_line_start_pos;

/* Number of characters past the end of the line above, including the
   terminating newline.  */

static struct text_pos this_line_end_pos;

/* The vertical positions and the height of this line.  */

static int this_line_vpos;
static int this_line_y;
static int this_line_pixel_height;

/* X position at which this display line starts.  Usually zero;
   negative if first character is partially visible.  */

static int this_line_start_x;

/* Buffer that this_line_.* variables are referring to.  */

static struct buffer *this_line_buffer;

/* Nonzero means truncate lines in all windows less wide than the
   frame.  */

int truncate_partial_width_windows;

/* A flag to control how to display unibyte 8-bit character.  */

int unibyte_display_via_language_environment;
 
/* Nonzero means we have more than one non-mini-buffer-only frame.
   Not guaranteed to be accurate except while parsing
   frame-title-format.  */

int multiple_frames;

Lisp_Object Vglobal_mode_string;

/* Marker for where to display an arrow on top of the buffer text.  */

Lisp_Object Voverlay_arrow_position;

/* String to display for the arrow.  Only used on terminal frames.  */

Lisp_Object Voverlay_arrow_string;

/* Values of those variables at last redisplay.  However, if
   Voverlay_arrow_position is a marker, last_arrow_position is its
   numerical position.  */

static Lisp_Object last_arrow_position, last_arrow_string;

/* Like mode-line-format, but for the title bar on a visible frame.  */

Lisp_Object Vframe_title_format;

/* Like mode-line-format, but for the title bar on an iconified frame.  */

Lisp_Object Vicon_title_format;

/* List of functions to call when a window's size changes.  These
   functions get one arg, a frame on which one or more windows' sizes
   have changed.  */

static Lisp_Object Vwindow_size_change_functions;

Lisp_Object Qmenu_bar_update_hook, Vmenu_bar_update_hook;

/* Nonzero if overlay arrow has been displayed once in this window.  */

static int overlay_arrow_seen;

/* Nonzero means highlight the region even in nonselected windows.  */

int highlight_nonselected_windows;

/* If cursor motion alone moves point off frame, try scrolling this
   many lines up or down if that will bring it back.  */

static EMACS_INT scroll_step;

/* Nonzero means scroll just far enough to bring point back on the
   screen, when appropriate.  */

static EMACS_INT scroll_conservatively;

/* Recenter the window whenever point gets within this many lines of
   the top or bottom of the window.  This value is translated into a
   pixel value by multiplying it with CANON_Y_UNIT, which means that
   there is really a fixed pixel height scroll margin.  */

EMACS_INT scroll_margin;

/* Number of windows showing the buffer of the selected window (or
   another buffer with the same base buffer).  keyboard.c refers to
   this.  */

int buffer_shared;

/* Vector containing glyphs for an ellipsis `...'.  */

static Lisp_Object default_invis_vector[3];

/* Zero means display the mode-line/header-line/menu-bar in the default face
   (this slightly odd definition is for compatibility with previous versions
   of emacs), non-zero means display them using their respective faces.

   This variable is deprecated.  */

int mode_line_inverse_video;

/* Prompt to display in front of the mini-buffer contents.  */

Lisp_Object minibuf_prompt;

/* Width of current mini-buffer prompt.  Only set after display_line
   of the line that contains the prompt.  */

int minibuf_prompt_width;
int minibuf_prompt_pixel_width;

/* This is the window where the echo area message was displayed.  It
   is always a mini-buffer window, but it may not be the same window
   currently active as a mini-buffer.  */

Lisp_Object echo_area_window;

/* List of pairs (MESSAGE . MULTIBYTE).  The function save_message
   pushes the current message and the value of
   message_enable_multibyte on the stack, the function restore_message
   pops the stack and displays MESSAGE again.  */

Lisp_Object Vmessage_stack;

/* Nonzero means multibyte characters were enabled when the echo area
   message was specified.  */

int message_enable_multibyte;

/* Nonzero if we should redraw the mode lines on the next redisplay.  */

int update_mode_lines;

/* Nonzero if window sizes or contents have changed since last
   redisplay that finished.  */

int windows_or_buffers_changed;

/* Nonzero means a frame's cursor type has been changed.  */

int cursor_type_changed;

/* Nonzero after display_mode_line if %l was used and it displayed a
   line number.  */

int line_number_displayed;

/* Maximum buffer size for which to display line numbers.  */

Lisp_Object Vline_number_display_limit;

/* Line width to consider when repositioning for line number display.  */

static EMACS_INT line_number_display_limit_width;

/* Number of lines to keep in the message log buffer.  t means
   infinite.  nil means don't log at all.  */

Lisp_Object Vmessage_log_max;

/* The name of the *Messages* buffer, a string.  */

static Lisp_Object Vmessages_buffer_name;

/* Current, index 0, and last displayed echo area message.  Either
   buffers from echo_buffers, or nil to indicate no message.  */

Lisp_Object echo_area_buffer[2];

/* The buffers referenced from echo_area_buffer.  */

static Lisp_Object echo_buffer[2];

/* A vector saved used in with_area_buffer to reduce consing.  */

static Lisp_Object Vwith_echo_area_save_vector;

/* Non-zero means display_echo_area should display the last echo area
   message again.  Set by redisplay_preserve_echo_area.  */

static int display_last_displayed_message_p;

/* Nonzero if echo area is being used by print; zero if being used by
   message.  */

int message_buf_print;

/* The symbol `inhibit-menubar-update' and its DEFVAR_BOOL variable.  */

Lisp_Object Qinhibit_menubar_update;
int inhibit_menubar_update;

/* Maximum height for resizing mini-windows.  Either a float
   specifying a fraction of the available height, or an integer
   specifying a number of lines.  */

Lisp_Object Vmax_mini_window_height;

/* Non-zero means messages should be displayed with truncated
   lines instead of being continued.  */

int message_truncate_lines;
Lisp_Object Qmessage_truncate_lines;

/* Set to 1 in clear_message to make redisplay_internal aware
   of an emptied echo area.   */

static int message_cleared_p;

/* Non-zero means we want a hollow cursor in windows that are not
   selected.  Zero means there's no cursor in such windows.  */

int cursor_in_non_selected_windows;
Lisp_Object Qcursor_in_non_selected_windows;

/* A scratch glyph row with contents used for generating truncation
   glyphs.  Also used in direct_output_for_insert.  */

#define MAX_SCRATCH_GLYPHS 100
struct glyph_row scratch_glyph_row;
static struct glyph scratch_glyphs[MAX_SCRATCH_GLYPHS];

/* Ascent and height of the last line processed by move_it_to.  */

static int last_max_ascent, last_height;

/* Non-zero if there's a help-echo in the echo area.  */

int help_echo_showing_p;

/* If >= 0, computed, exact values of mode-line and header-line height
   to use in the macros CURRENT_MODE_LINE_HEIGHT and
   CURRENT_HEADER_LINE_HEIGHT.  */

int current_mode_line_height, current_header_line_height;

/* The maximum distance to look ahead for text properties.  Values
   that are too small let us call compute_char_face and similar 
   functions too often which is expensive.  Values that are too large
   let us call compute_char_face and alike too often because we
   might not be interested in text properties that far away.  */

#define TEXT_PROP_DISTANCE_LIMIT 100

#if GLYPH_DEBUG

/* Variables to turn off display optimizations from Lisp.  */

int inhibit_try_window_id, inhibit_try_window_reusing;
int inhibit_try_cursor_movement;

/* Non-zero means print traces of redisplay if compiled with
   GLYPH_DEBUG != 0.  */

int trace_redisplay_p;

#endif /* GLYPH_DEBUG */

#ifdef DEBUG_TRACE_MOVE
/* Non-zero means trace with TRACE_MOVE to stderr.  */
int trace_move;

#define TRACE_MOVE(x)	if (trace_move) fprintf x; else (void) 0
#else
#define TRACE_MOVE(x)	(void) 0
#endif
 
/* Non-zero means automatically scroll windows horizontally to make
   point visible.  */

int automatic_hscrolling_p;

/* How close to the margin can point get before the window is scrolled
   horizontally.  */
EMACS_INT hscroll_margin;

/* How much to scroll horizontally when point is inside the above margin.  */
Lisp_Object Vhscroll_step;

/* A list of symbols, one for each supported image type.  */

Lisp_Object Vimage_types;

/* The variable `resize-mini-windows'.  If nil, don't resize
   mini-windows.  If t, always resize them to fit the text they
   display.  If `grow-only', let mini-windows grow only until they
   become empty.  */

Lisp_Object Vresize_mini_windows;

/* Buffer being redisplayed -- for redisplay_window_error.  */

struct buffer *displayed_buffer;

/* Value returned from text property handlers (see below).  */

enum prop_handled
{
  HANDLED_NORMALLY,
  HANDLED_RECOMPUTE_PROPS,
  HANDLED_OVERLAY_STRING_CONSUMED,
  HANDLED_RETURN
};

/* A description of text properties that redisplay is interested
   in.  */

struct props
{
  /* The name of the property.  */
  Lisp_Object *name;

  /* A unique index for the property.  */
  enum prop_idx idx;

  /* A handler function called to set up iterator IT from the property
     at IT's current position.  Value is used to steer handle_stop.  */
  enum prop_handled (*handler) P_ ((struct it *it));
};

static enum prop_handled handle_face_prop P_ ((struct it *));
static enum prop_handled handle_invisible_prop P_ ((struct it *));
static enum prop_handled handle_display_prop P_ ((struct it *));
static enum prop_handled handle_composition_prop P_ ((struct it *));
static enum prop_handled handle_overlay_change P_ ((struct it *));
static enum prop_handled handle_fontified_prop P_ ((struct it *));

/* Properties handled by iterators.  */

static struct props it_props[] =
{
  {&Qfontified,		FONTIFIED_PROP_IDX,	handle_fontified_prop},
  /* Handle `face' before `display' because some sub-properties of
     `display' need to know the face.  */
  {&Qface,		FACE_PROP_IDX,		handle_face_prop},
  {&Qdisplay,		DISPLAY_PROP_IDX,	handle_display_prop},
  {&Qinvisible,		INVISIBLE_PROP_IDX,	handle_invisible_prop},
  {&Qcomposition,	COMPOSITION_PROP_IDX,	handle_composition_prop},
  {NULL,		0,			NULL}
};

/* Value is the position described by X.  If X is a marker, value is
   the marker_position of X.  Otherwise, value is X.  */

#define COERCE_MARKER(X) (MARKERP ((X)) ? Fmarker_position (X) : (X))

/* Enumeration returned by some move_it_.* functions internally.  */

enum move_it_result
{
  /* Not used.  Undefined value.  */
  MOVE_UNDEFINED,

  /* Move ended at the requested buffer position or ZV.  */
  MOVE_POS_MATCH_OR_ZV,

  /* Move ended at the requested X pixel position.  */
  MOVE_X_REACHED,

  /* Move within a line ended at the end of a line that must be
     continued.  */
  MOVE_LINE_CONTINUED,
  
  /* Move within a line ended at the end of a line that would
     be displayed truncated.  */
  MOVE_LINE_TRUNCATED,

  /* Move within a line ended at a line end.  */
  MOVE_NEWLINE_OR_CR
};



/* Function prototypes.  */

static void setup_for_ellipsis P_ ((struct it *));
static void mark_window_display_accurate_1 P_ ((struct window *, int));
static int single_display_prop_string_p P_ ((Lisp_Object, Lisp_Object));
static int display_prop_string_p P_ ((Lisp_Object, Lisp_Object));
static int cursor_row_p P_ ((struct window *, struct glyph_row *));
static int redisplay_mode_lines P_ ((Lisp_Object, int));
static char *decode_mode_spec_coding P_ ((Lisp_Object, char *, int));

#if 0
static int invisible_text_between_p P_ ((struct it *, int, int));
#endif

static int next_element_from_ellipsis P_ ((struct it *));
static void pint2str P_ ((char *, int, int));
static struct text_pos run_window_scroll_functions P_ ((Lisp_Object,
							struct text_pos));
static void reconsider_clip_changes P_ ((struct window *, struct buffer *));
static int text_outside_line_unchanged_p P_ ((struct window *, int, int));
static void store_frame_title_char P_ ((char));
static int store_frame_title P_ ((unsigned char *, int, int));
static void x_consider_frame_title P_ ((Lisp_Object));
static void handle_stop P_ ((struct it *));
static int tool_bar_lines_needed P_ ((struct frame *));
static int single_display_prop_intangible_p P_ ((Lisp_Object));
static void ensure_echo_area_buffers P_ ((void));
static Lisp_Object unwind_with_echo_area_buffer P_ ((Lisp_Object));
static Lisp_Object with_echo_area_buffer_unwind_data P_ ((struct window *));
static int with_echo_area_buffer P_ ((struct window *, int,
				      int (*) (EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT),
				      EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT));
static void clear_garbaged_frames P_ ((void));
static int current_message_1 P_ ((EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT));
static int truncate_message_1 P_ ((EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT));
static int set_message_1 P_ ((EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT));
static int display_echo_area P_ ((struct window *));
static int display_echo_area_1 P_ ((EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT));
static int resize_mini_window_1 P_ ((EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT));
static Lisp_Object unwind_redisplay P_ ((Lisp_Object));
static int string_char_and_length P_ ((unsigned char *, int, int *));
static struct text_pos display_prop_end P_ ((struct it *, Lisp_Object,
					     struct text_pos));
static int compute_window_start_on_continuation_line P_ ((struct window *));
static Lisp_Object safe_eval_handler P_ ((Lisp_Object));
static void insert_left_trunc_glyphs P_ ((struct it *));
static struct glyph_row *get_overlay_arrow_glyph_row P_ ((struct window *));
static void extend_face_to_end_of_line P_ ((struct it *));
static int append_space P_ ((struct it *, int));
static int make_cursor_line_fully_visible P_ ((struct window *));
static int try_scrolling P_ ((Lisp_Object, int, EMACS_INT, EMACS_INT, int));
static int try_cursor_movement P_ ((Lisp_Object, struct text_pos, int *));
static int trailing_whitespace_p P_ ((int));
static int message_log_check_duplicate P_ ((int, int, int, int));
static void push_it P_ ((struct it *));
static void pop_it P_ ((struct it *));
static void sync_frame_with_window_matrix_rows P_ ((struct window *));
static void redisplay_internal P_ ((int));
static int echo_area_display P_ ((int));
static void redisplay_windows P_ ((Lisp_Object));
static void redisplay_window P_ ((Lisp_Object, int));
static Lisp_Object redisplay_window_error ();
static Lisp_Object redisplay_window_0 P_ ((Lisp_Object));
static Lisp_Object redisplay_window_1 P_ ((Lisp_Object));
static void update_menu_bar P_ ((struct frame *, int));
static int try_window_reusing_current_matrix P_ ((struct window *));
static int try_window_id P_ ((struct window *));
static int display_line P_ ((struct it *));
static int display_mode_lines P_ ((struct window *));
static int display_mode_line P_ ((struct window *, enum face_id, Lisp_Object));
static int display_mode_element P_ ((struct it *, int, int, int, Lisp_Object, Lisp_Object));
static char *decode_mode_spec P_ ((struct window *, int, int, int, int *));
static void display_menu_bar P_ ((struct window *));
static int display_count_lines P_ ((int, int, int, int, int *));
static int display_string P_ ((unsigned char *, Lisp_Object, Lisp_Object,
			       int, int, struct it *, int, int, int, int));
static void compute_line_metrics P_ ((struct it *));
static void run_redisplay_end_trigger_hook P_ ((struct it *));
static int get_overlay_strings P_ ((struct it *, int));
static void next_overlay_string P_ ((struct it *));
static void reseat P_ ((struct it *, struct text_pos, int));
static void reseat_1 P_ ((struct it *, struct text_pos, int));
static void back_to_previous_visible_line_start P_ ((struct it *));
static void reseat_at_previous_visible_line_start P_ ((struct it *));
static void reseat_at_next_visible_line_start P_ ((struct it *, int));
static int next_element_from_display_vector P_ ((struct it *));
static int next_element_from_string P_ ((struct it *));
static int next_element_from_c_string P_ ((struct it *));
static int next_element_from_buffer P_ ((struct it *));
static int next_element_from_composition P_ ((struct it *));
static int next_element_from_image P_ ((struct it *));
static int next_element_from_stretch P_ ((struct it *));
static void load_overlay_strings P_ ((struct it *, int));
static int init_from_display_pos P_ ((struct it *, struct window *,
				      struct display_pos *));
static void reseat_to_string P_ ((struct it *, unsigned char *,
				  Lisp_Object, int, int, int, int));
static enum move_it_result move_it_in_display_line_to P_ ((struct it *,
							   int, int, int));
void move_it_vertically_backward P_ ((struct it *, int));
static void init_to_row_start P_ ((struct it *, struct window *,
				   struct glyph_row *));
static int init_to_row_end P_ ((struct it *, struct window *,
				struct glyph_row *));
static void back_to_previous_line_start P_ ((struct it *));
static int forward_to_next_line_start P_ ((struct it *, int *));
static struct text_pos string_pos_nchars_ahead P_ ((struct text_pos,
						    Lisp_Object, int));
static struct text_pos string_pos P_ ((int, Lisp_Object));
static struct text_pos c_string_pos P_ ((int, unsigned char *, int));
static int number_of_chars P_ ((unsigned char *, int));
static void compute_stop_pos P_ ((struct it *));
static void compute_string_pos P_ ((struct text_pos *, struct text_pos,
				    Lisp_Object));
static int face_before_or_after_it_pos P_ ((struct it *, int));
static int next_overlay_change P_ ((int));
static int handle_single_display_prop P_ ((struct it *, Lisp_Object,
					   Lisp_Object, struct text_pos *,
					   int));
static int underlying_face_id P_ ((struct it *));
static int in_ellipses_for_invisible_text_p P_ ((struct display_pos *,
						 struct window *));

#define face_before_it_pos(IT) face_before_or_after_it_pos ((IT), 1)
#define face_after_it_pos(IT)  face_before_or_after_it_pos ((IT), 0)

#ifdef HAVE_WINDOW_SYSTEM

static void update_tool_bar P_ ((struct frame *, int));
static void build_desired_tool_bar_string P_ ((struct frame *f));
static int redisplay_tool_bar P_ ((struct frame *));
static void display_tool_bar_line P_ ((struct it *));

#endif /* HAVE_WINDOW_SYSTEM */


/***********************************************************************
		      Window display dimensions
 ***********************************************************************/

/* Return the window-relative maximum y + 1 for glyph rows displaying
   text in window W.  This is the height of W minus the height of a
   mode line, if any.  */

INLINE int
window_text_bottom_y (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  int height = XFASTINT (w->height) * CANON_Y_UNIT (f);

  if (WINDOW_WANTS_MODELINE_P (w))
    height -= CURRENT_MODE_LINE_HEIGHT (w);
  return height;
}


/* Return the pixel width of display area AREA of window W.  AREA < 0
   means return the total width of W, not including fringes to
   the left and right of the window.  */

INLINE int
window_box_width (w, area)
     struct window *w;
     int area;
{
  struct frame *f = XFRAME (w->frame);
  int width = XFASTINT (w->width);
  
  if (!w->pseudo_window_p)
    {
      width -= FRAME_SCROLL_BAR_WIDTH (f) + FRAME_FRINGE_COLS (f);
      
      if (area == TEXT_AREA)
	{
	  if (INTEGERP (w->left_margin_width))
	    width -= XFASTINT (w->left_margin_width);
	  if (INTEGERP (w->right_margin_width))
	    width -= XFASTINT (w->right_margin_width);
	}
      else if (area == LEFT_MARGIN_AREA)
	width = (INTEGERP (w->left_margin_width)
		 ? XFASTINT (w->left_margin_width) : 0);
      else if (area == RIGHT_MARGIN_AREA)
	width = (INTEGERP (w->right_margin_width)
		 ? XFASTINT (w->right_margin_width) : 0);
    }

  return width * CANON_X_UNIT (f);
}


/* Return the pixel height of the display area of window W, not
   including mode lines of W, if any.  */

INLINE int
window_box_height (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  int height = XFASTINT (w->height) * CANON_Y_UNIT (f);

  xassert (height >= 0);
  
  /* Note: the code below that determines the mode-line/header-line
     height is essentially the same as that contained in the macro
     CURRENT_{MODE,HEADER}_LINE_HEIGHT, except that it checks whether
     the appropriate glyph row has its `mode_line_p' flag set,
     and if it doesn't, uses estimate_mode_line_height instead.  */

  if (WINDOW_WANTS_MODELINE_P (w))
    {
      struct glyph_row *ml_row
	= (w->current_matrix && w->current_matrix->rows
	   ? MATRIX_MODE_LINE_ROW (w->current_matrix)
	   : 0);
      if (ml_row && ml_row->mode_line_p)
	height -= ml_row->height;
      else
	height -= estimate_mode_line_height (f, CURRENT_MODE_LINE_FACE_ID (w));
    }

  if (WINDOW_WANTS_HEADER_LINE_P (w))
    {
      struct glyph_row *hl_row
	= (w->current_matrix && w->current_matrix->rows
	   ? MATRIX_HEADER_LINE_ROW (w->current_matrix)
	   : 0);
      if (hl_row && hl_row->mode_line_p)
	height -= hl_row->height;
      else
	height -= estimate_mode_line_height (f, HEADER_LINE_FACE_ID);
    }

  /* With a very small font and a mode-line that's taller than
     default, we might end up with a negative height.  */
  return max (0, height);
}


/* Return the frame-relative coordinate of the left edge of display
   area AREA of window W.  AREA < 0 means return the left edge of the
   whole window, to the right of the left fringe of W.  */

INLINE int
window_box_left (w, area)
     struct window *w;
     int area;
{
  struct frame *f = XFRAME (w->frame);
  int x = FRAME_INTERNAL_BORDER_WIDTH_SAFE (f);

  if (!w->pseudo_window_p)
    {
      x += (WINDOW_LEFT_MARGIN (w) * CANON_X_UNIT (f)
	    + FRAME_LEFT_FRINGE_WIDTH (f));
      
      if (area == TEXT_AREA)
	x += window_box_width (w, LEFT_MARGIN_AREA);
      else if (area == RIGHT_MARGIN_AREA)
	x += (window_box_width (w, LEFT_MARGIN_AREA)
	      + window_box_width (w, TEXT_AREA));
    }

  return x;
}     


/* Return the frame-relative coordinate of the right edge of display
   area AREA of window W.  AREA < 0 means return the left edge of the
   whole window, to the left of the right fringe of W.  */

INLINE int
window_box_right (w, area)
     struct window *w;
     int area;
{
  return window_box_left (w, area) + window_box_width (w, area);
}     
     

/* Get the bounding box of the display area AREA of window W, without
   mode lines, in frame-relative coordinates.  AREA < 0 means the
   whole window, not including the left and right fringes of
   the window.  Return in *BOX_X and *BOX_Y the frame-relative pixel
   coordinates of the upper-left corner of the box.  Return in
   *BOX_WIDTH, and *BOX_HEIGHT the pixel width and height of the box.  */

INLINE void
window_box (w, area, box_x, box_y, box_width, box_height)
     struct window *w;
     int area;
     int *box_x, *box_y, *box_width, *box_height;
{
  struct frame *f = XFRAME (w->frame);
  
  *box_width = window_box_width (w, area);
  *box_height = window_box_height (w);
  *box_x = window_box_left (w, area);
  *box_y = (FRAME_INTERNAL_BORDER_WIDTH_SAFE (f)
	    + XFASTINT (w->top) * CANON_Y_UNIT (f));
  if (WINDOW_WANTS_HEADER_LINE_P (w))
    *box_y += CURRENT_HEADER_LINE_HEIGHT (w);
}


/* Get the bounding box of the display area AREA of window W, without
   mode lines.  AREA < 0 means the whole window, not including the
   left and right fringe of the window.  Return in *TOP_LEFT_X
   and TOP_LEFT_Y the frame-relative pixel coordinates of the
   upper-left corner of the box.  Return in *BOTTOM_RIGHT_X, and
   *BOTTOM_RIGHT_Y the coordinates of the bottom-right corner of the
   box.  */

INLINE void
window_box_edges (w, area, top_left_x, top_left_y,
		  bottom_right_x, bottom_right_y)
     struct window *w;
     int area;
     int *top_left_x, *top_left_y, *bottom_right_x, *bottom_right_y;
{
  window_box (w, area, top_left_x, top_left_y, bottom_right_x,
	      bottom_right_y);
  *bottom_right_x += *top_left_x;
  *bottom_right_y += *top_left_y;
}



/***********************************************************************
			      Utilities
 ***********************************************************************/

/* Return the bottom y-position of the line the iterator IT is in.
   This can modify IT's settings.  */

int
line_bottom_y (it)
     struct it *it;
{
  int line_height = it->max_ascent + it->max_descent;
  int line_top_y = it->current_y;
  
  if (line_height == 0)
    {
      if (last_height)
	line_height = last_height;
      else if (IT_CHARPOS (*it) < ZV)
	{
	  move_it_by_lines (it, 1, 1);
	  line_height = (it->max_ascent || it->max_descent
			 ? it->max_ascent + it->max_descent
			 : last_height);
	}
      else
	{
	  struct glyph_row *row = it->glyph_row;
	  
	  /* Use the default character height.  */
	  it->glyph_row = NULL;
	  it->what = IT_CHARACTER;
	  it->c = ' ';
	  it->len = 1;
	  PRODUCE_GLYPHS (it);
	  line_height = it->ascent + it->descent;
	  it->glyph_row = row;
	}
    }

  return line_top_y + line_height;
}


/* Return 1 if position CHARPOS is visible in window W.  Set *FULLY to
   1 if POS is visible and the line containing POS is fully visible.
   EXACT_MODE_LINE_HEIGHTS_P non-zero means compute exact mode-line
   and header-lines heights.  */

int
pos_visible_p (w, charpos, fully, exact_mode_line_heights_p)
     struct window *w;
     int charpos, *fully, exact_mode_line_heights_p;
{
  struct it it;
  struct text_pos top;
  int visible_p;
  struct buffer *old_buffer = NULL;

  if (XBUFFER (w->buffer) != current_buffer)
    {
      old_buffer = current_buffer;
      set_buffer_internal_1 (XBUFFER (w->buffer));
    }

  *fully = visible_p = 0;
  SET_TEXT_POS_FROM_MARKER (top, w->start);
  
  /* Compute exact mode line heights, if requested.  */
  if (exact_mode_line_heights_p)
    {
      if (WINDOW_WANTS_MODELINE_P (w))
	current_mode_line_height
	  = display_mode_line (w, CURRENT_MODE_LINE_FACE_ID (w),
			       current_buffer->mode_line_format);
  
      if (WINDOW_WANTS_HEADER_LINE_P (w))
	current_header_line_height
	  = display_mode_line (w, HEADER_LINE_FACE_ID,
			       current_buffer->header_line_format);
    }

  start_display (&it, w, top);
  move_it_to (&it, charpos, 0, it.last_visible_y, -1,
	      MOVE_TO_POS | MOVE_TO_X | MOVE_TO_Y);

  /* Note that we may overshoot because of invisible text.  */
  if (IT_CHARPOS (it) >= charpos)
    {
      int top_y = it.current_y;
      int bottom_y = line_bottom_y (&it);
      int window_top_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
      
      if (top_y < window_top_y)
	visible_p = bottom_y > window_top_y;
      else if (top_y < it.last_visible_y)
	{
	  visible_p = 1;
	  *fully = bottom_y <= it.last_visible_y;
	}
    }
  else if (it.current_y + it.max_ascent + it.max_descent > it.last_visible_y)
    {
      move_it_by_lines (&it, 1, 0);
      if (charpos < IT_CHARPOS (it))
	{
	  visible_p = 1;
	  *fully  = 0;
	}
    }

  if (old_buffer)
    set_buffer_internal_1 (old_buffer);

  current_header_line_height = current_mode_line_height = -1;
  return visible_p;
}


/* Return the next character from STR which is MAXLEN bytes long.
   Return in *LEN the length of the character.  This is like
   STRING_CHAR_AND_LENGTH but never returns an invalid character.  If
   we find one, we return a `?', but with the length of the invalid
   character.  */

static INLINE int
string_char_and_length (str, maxlen, len)
     unsigned char *str;
     int maxlen, *len;
{
  int c;

  c = STRING_CHAR_AND_LENGTH (str, maxlen, *len);
  if (!CHAR_VALID_P (c, 1))
    /* We may not change the length here because other places in Emacs
       don't use this function, i.e. they silently accept invalid
       characters.  */
    c = '?';

  return c;
}



/* Given a position POS containing a valid character and byte position
   in STRING, return the position NCHARS ahead (NCHARS >= 0).  */

static struct text_pos
string_pos_nchars_ahead (pos, string, nchars)
     struct text_pos pos;
     Lisp_Object string;
     int nchars;
{
  xassert (STRINGP (string) && nchars >= 0);

  if (STRING_MULTIBYTE (string))
    {
      int rest = SBYTES (string) - BYTEPOS (pos);
      unsigned char *p = SDATA (string) + BYTEPOS (pos);
      int len;

      while (nchars--)
	{
	  string_char_and_length (p, rest, &len);
	  p += len, rest -= len;
	  xassert (rest >= 0);
	  CHARPOS (pos) += 1;
	  BYTEPOS (pos) += len;
	}
    }
  else
    SET_TEXT_POS (pos, CHARPOS (pos) + nchars, BYTEPOS (pos) + nchars);

  return pos;
}


/* Value is the text position, i.e. character and byte position,
   for character position CHARPOS in STRING.  */

static INLINE struct text_pos
string_pos (charpos, string)
     int charpos;
     Lisp_Object string;
{
  struct text_pos pos;
  xassert (STRINGP (string));
  xassert (charpos >= 0);
  SET_TEXT_POS (pos, charpos, string_char_to_byte (string, charpos));
  return pos;
}


/* Value is a text position, i.e. character and byte position, for
   character position CHARPOS in C string S.  MULTIBYTE_P non-zero
   means recognize multibyte characters.  */

static struct text_pos
c_string_pos (charpos, s, multibyte_p)
     int charpos;
     unsigned char *s;
     int multibyte_p;
{
  struct text_pos pos;

  xassert (s != NULL);
  xassert (charpos >= 0);

  if (multibyte_p)
    {
      int rest = strlen (s), len;

      SET_TEXT_POS (pos, 0, 0);
      while (charpos--)
	{
	  string_char_and_length (s, rest, &len);
	  s += len, rest -= len;
	  xassert (rest >= 0);
	  CHARPOS (pos) += 1;
	  BYTEPOS (pos) += len;
	}
    }
  else
    SET_TEXT_POS (pos, charpos, charpos);

  return pos;
}


/* Value is the number of characters in C string S.  MULTIBYTE_P
   non-zero means recognize multibyte characters.  */

static int
number_of_chars (s, multibyte_p)
     unsigned char *s;
     int multibyte_p;
{
  int nchars;
  
  if (multibyte_p)
    {
      int rest = strlen (s), len;
      unsigned char *p = (unsigned char *) s;

      for (nchars = 0; rest > 0; ++nchars)
	{
	  string_char_and_length (p, rest, &len);
	  rest -= len, p += len;
	}
    }
  else
    nchars = strlen (s);

  return nchars;
}

     
/* Compute byte position NEWPOS->bytepos corresponding to
   NEWPOS->charpos.  POS is a known position in string STRING.
   NEWPOS->charpos must be >= POS.charpos.  */

static void
compute_string_pos (newpos, pos, string)
     struct text_pos *newpos, pos;
     Lisp_Object string;
{
  xassert (STRINGP (string));
  xassert (CHARPOS (*newpos) >= CHARPOS (pos));
  
  if (STRING_MULTIBYTE (string))
    *newpos = string_pos_nchars_ahead (pos, string,
				       CHARPOS (*newpos) - CHARPOS (pos));
  else
    BYTEPOS (*newpos) = CHARPOS (*newpos);
}



/***********************************************************************
			Lisp form evaluation
 ***********************************************************************/

/* Error handler for safe_eval and safe_call.  */

static Lisp_Object
safe_eval_handler (arg)
     Lisp_Object arg;
{
  add_to_log ("Error during redisplay: %s", arg, Qnil);
  return Qnil;
}


/* Evaluate SEXPR and return the result, or nil if something went
   wrong.  Prevent redisplay during the evaluation.  */

Lisp_Object
safe_eval (sexpr)
     Lisp_Object sexpr;
{
  Lisp_Object val;
  
  if (inhibit_eval_during_redisplay)
    val = Qnil;
  else
    {
      int count = BINDING_STACK_SIZE ();
      struct gcpro gcpro1;

      GCPRO1 (sexpr);
      specbind (Qinhibit_redisplay, Qt);
      /* Use Qt to ensure debugger does not run,
	 so there is no possibility of wanting to redisplay.  */
      val = internal_condition_case_1 (Feval, sexpr, Qt,
				       safe_eval_handler);
      UNGCPRO;
      val = unbind_to (count, val);
    }
  
  return val;
}


/* Call function ARGS[0] with arguments ARGS[1] to ARGS[NARGS - 1].
   Return the result, or nil if something went wrong.  Prevent
   redisplay during the evaluation.  */

Lisp_Object
safe_call (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object val;
  
  if (inhibit_eval_during_redisplay)
    val = Qnil;
  else
    {
      int count = BINDING_STACK_SIZE ();
      struct gcpro gcpro1;

      GCPRO1 (args[0]);
      gcpro1.nvars = nargs;
      specbind (Qinhibit_redisplay, Qt);
      /* Use Qt to ensure debugger does not run,
	 so there is no possibility of wanting to redisplay.  */
      val = internal_condition_case_2 (Ffuncall, nargs, args, Qt,
				       safe_eval_handler);
      UNGCPRO;
      val = unbind_to (count, val);
    }

  return val;
}


/* Call function FN with one argument ARG.
   Return the result, or nil if something went wrong.  */

Lisp_Object
safe_call1 (fn, arg)
     Lisp_Object fn, arg;
{
  Lisp_Object args[2];
  args[0] = fn;
  args[1] = arg;
  return safe_call (2, args);
}



/***********************************************************************
			      Debugging
 ***********************************************************************/

#if 0

/* Define CHECK_IT to perform sanity checks on iterators.
   This is for debugging.  It is too slow to do unconditionally.  */

static void
check_it (it)
     struct it *it;
{
  if (it->method == next_element_from_string)
    {
      xassert (STRINGP (it->string));
      xassert (IT_STRING_CHARPOS (*it) >= 0);
    }
  else if (it->method == next_element_from_buffer)
    {
      /* Check that character and byte positions agree.  */
      xassert (IT_CHARPOS (*it) == BYTE_TO_CHAR (IT_BYTEPOS (*it)));
    }

  if (it->dpvec)
    xassert (it->current.dpvec_index >= 0);
  else
    xassert (it->current.dpvec_index < 0);
}

#define CHECK_IT(IT)	check_it ((IT))

#else /* not 0 */

#define CHECK_IT(IT)	(void) 0

#endif /* not 0 */


#if GLYPH_DEBUG

/* Check that the window end of window W is what we expect it
   to be---the last row in the current matrix displaying text.  */

static void
check_window_end (w)
     struct window *w;
{
  if (!MINI_WINDOW_P (w)
      && !NILP (w->window_end_valid))
    {
      struct glyph_row *row;
      xassert ((row = MATRIX_ROW (w->current_matrix,
				  XFASTINT (w->window_end_vpos)),
		!row->enabled_p
		|| MATRIX_ROW_DISPLAYS_TEXT_P (row)
		|| MATRIX_ROW_VPOS (row, w->current_matrix) == 0));
    }
}

#define CHECK_WINDOW_END(W)	check_window_end ((W))

#else /* not GLYPH_DEBUG */

#define CHECK_WINDOW_END(W)	(void) 0

#endif /* not GLYPH_DEBUG */



/***********************************************************************
		       Iterator initialization
 ***********************************************************************/

/* Initialize IT for displaying current_buffer in window W, starting
   at character position CHARPOS.  CHARPOS < 0 means that no buffer
   position is specified which is useful when the iterator is assigned
   a position later.  BYTEPOS is the byte position corresponding to
   CHARPOS.  BYTEPOS < 0 means compute it from CHARPOS.

   If ROW is not null, calls to produce_glyphs with IT as parameter
   will produce glyphs in that row.

   BASE_FACE_ID is the id of a base face to use.  It must be one of
   DEFAULT_FACE_ID for normal text, MODE_LINE_FACE_ID,
   MODE_LINE_INACTIVE_FACE_ID, or HEADER_LINE_FACE_ID for displaying
   mode lines, or TOOL_BAR_FACE_ID for displaying the tool-bar.
   
   If ROW is null and BASE_FACE_ID is equal to MODE_LINE_FACE_ID,
   MODE_LINE_INACTIVE_FACE_ID, or HEADER_LINE_FACE_ID, the iterator
   will be initialized to use the corresponding mode line glyph row of
   the desired matrix of W.  */

void
init_iterator (it, w, charpos, bytepos, row, base_face_id)
     struct it *it;
     struct window *w;
     int charpos, bytepos;
     struct glyph_row *row;
     enum face_id base_face_id;
{
  int highlight_region_p;

  /* Some precondition checks.  */
  xassert (w != NULL && it != NULL);
  xassert (charpos < 0 || (charpos >= BUF_BEG (current_buffer)
			   && charpos <= ZV));

  /* If face attributes have been changed since the last redisplay,
     free realized faces now because they depend on face definitions
     that might have changed.  */
  if (face_change_count)
    {
      face_change_count = 0;
      free_all_realized_faces (Qnil);
    }

  /* Use one of the mode line rows of W's desired matrix if
     appropriate.  */
  if (row == NULL)
    {
      if (base_face_id == MODE_LINE_FACE_ID
	  || base_face_id == MODE_LINE_INACTIVE_FACE_ID)
	row = MATRIX_MODE_LINE_ROW (w->desired_matrix);
      else if (base_face_id == HEADER_LINE_FACE_ID)
	row = MATRIX_HEADER_LINE_ROW (w->desired_matrix);
    }
  
  /* Clear IT.  */
  bzero (it, sizeof *it);
  it->current.overlay_string_index = -1;
  it->current.dpvec_index = -1;
  it->base_face_id = base_face_id;

  /* The window in which we iterate over current_buffer:  */
  XSETWINDOW (it->window, w);
  it->w = w;
  it->f = XFRAME (w->frame);

  /* Extra space between lines (on window systems only).  */
  if (base_face_id == DEFAULT_FACE_ID
      && FRAME_WINDOW_P (it->f))
    {
      if (NATNUMP (current_buffer->extra_line_spacing))
	it->extra_line_spacing = XFASTINT (current_buffer->extra_line_spacing);
      else if (it->f->extra_line_spacing > 0)
	it->extra_line_spacing = it->f->extra_line_spacing;
    }

  /* If realized faces have been removed, e.g. because of face
     attribute changes of named faces, recompute them.  When running
     in batch mode, the face cache of Vterminal_frame is null.  If
     we happen to get called, make a dummy face cache.  */
  if (
#ifndef WINDOWSNT
      noninteractive &&
#endif
      FRAME_FACE_CACHE (it->f) == NULL)
    init_frame_faces (it->f);
  if (FRAME_FACE_CACHE (it->f)->used == 0)
    recompute_basic_faces (it->f);

  /* Current value of the `space-width', and 'height' properties.  */
  it->space_width = Qnil;
  it->font_height = Qnil;
  
  /* Are control characters displayed as `^C'?  */
  it->ctl_arrow_p = !NILP (current_buffer->ctl_arrow);

  /* -1 means everything between a CR and the following line end
     is invisible.  >0 means lines indented more than this value are
     invisible.  */
  it->selective = (INTEGERP (current_buffer->selective_display)
		   ? XFASTINT (current_buffer->selective_display)
		   : (!NILP (current_buffer->selective_display) 
		      ? -1 : 0));
  it->selective_display_ellipsis_p
    = !NILP (current_buffer->selective_display_ellipses);

  /* Display table to use.  */
  it->dp = window_display_table (w);

  /* Are multibyte characters enabled in current_buffer?  */
  it->multibyte_p = !NILP (current_buffer->enable_multibyte_characters);

  /* Non-zero if we should highlight the region.  */
  highlight_region_p
    = (!NILP (Vtransient_mark_mode)
       && !NILP (current_buffer->mark_active)
       && XMARKER (current_buffer->mark)->buffer != 0);

  /* Set IT->region_beg_charpos and IT->region_end_charpos to the
     start and end of a visible region in window IT->w.  Set both to
     -1 to indicate no region.  */
  if (highlight_region_p
      /* Maybe highlight only in selected window.  */
      && (/* Either show region everywhere.  */ 
	  highlight_nonselected_windows
	  /* Or show region in the selected window.  */
	  || w == XWINDOW (selected_window)
	  /* Or show the region if we are in the mini-buffer and W is
	     the window the mini-buffer refers to.  */
	  || (MINI_WINDOW_P (XWINDOW (selected_window))
	      && WINDOWP (minibuf_selected_window)
	      && w == XWINDOW (minibuf_selected_window))))
    {
      int charpos = marker_position (current_buffer->mark);
      it->region_beg_charpos = min (PT, charpos);
      it->region_end_charpos = max (PT, charpos);
    }
  else
    it->region_beg_charpos = it->region_end_charpos = -1;

  /* Get the position at which the redisplay_end_trigger hook should
     be run, if it is to be run at all.  */
  if (MARKERP (w->redisplay_end_trigger)
      && XMARKER (w->redisplay_end_trigger)->buffer != 0)
    it->redisplay_end_trigger_charpos
      = marker_position (w->redisplay_end_trigger);
  else if (INTEGERP (w->redisplay_end_trigger))
    it->redisplay_end_trigger_charpos = XINT (w->redisplay_end_trigger);

  /* Correct bogus values of tab_width.  */
  it->tab_width = XINT (current_buffer->tab_width);
  if (it->tab_width <= 0 || it->tab_width > 1000)
    it->tab_width = 8;

  /* Are lines in the display truncated?  */
  it->truncate_lines_p
    = (base_face_id != DEFAULT_FACE_ID
       || XINT (it->w->hscroll)
       || (truncate_partial_width_windows
	   && !WINDOW_FULL_WIDTH_P (it->w))
       || !NILP (current_buffer->truncate_lines));

  /* Get dimensions of truncation and continuation glyphs.  These are
     displayed as fringe bitmaps under X, so we don't need them for such
     frames.  */
  if (!FRAME_WINDOW_P (it->f))
    {
      if (it->truncate_lines_p)
	{
	  /* We will need the truncation glyph.  */
	  xassert (it->glyph_row == NULL);
	  produce_special_glyphs (it, IT_TRUNCATION);
	  it->truncation_pixel_width = it->pixel_width;
	}
      else
	{
	  /* We will need the continuation glyph.  */
	  xassert (it->glyph_row == NULL);
	  produce_special_glyphs (it, IT_CONTINUATION);
	  it->continuation_pixel_width = it->pixel_width;
	}

      /* Reset these values to zero because the produce_special_glyphs
	 above has changed them.  */
      it->pixel_width = it->ascent = it->descent = 0;
      it->phys_ascent = it->phys_descent = 0;
    }

  /* Set this after getting the dimensions of truncation and
     continuation glyphs, so that we don't produce glyphs when calling
     produce_special_glyphs, above.  */
  it->glyph_row = row;
  it->area = TEXT_AREA;

  /* Get the dimensions of the display area.  The display area
     consists of the visible window area plus a horizontally scrolled
     part to the left of the window.  All x-values are relative to the
     start of this total display area.  */
  if (base_face_id != DEFAULT_FACE_ID)
    {
      /* Mode lines, menu bar in terminal frames.  */
      it->first_visible_x = 0;
      it->last_visible_x = XFASTINT (w->width) * CANON_X_UNIT (it->f);
    }
  else
    {
      it->first_visible_x
	= XFASTINT (it->w->hscroll) * CANON_X_UNIT (it->f);
      it->last_visible_x = (it->first_visible_x
			    + window_box_width (w, TEXT_AREA));

      /* If we truncate lines, leave room for the truncator glyph(s) at
	 the right margin.  Otherwise, leave room for the continuation
	 glyph(s).  Truncation and continuation glyphs are not inserted
	 for window-based redisplay.  */
      if (!FRAME_WINDOW_P (it->f))
	{
	  if (it->truncate_lines_p)
	    it->last_visible_x -= it->truncation_pixel_width;
	  else
	    it->last_visible_x -= it->continuation_pixel_width;
	}

      it->header_line_p = WINDOW_WANTS_HEADER_LINE_P (w);
      it->current_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w) + w->vscroll;
    }

  /* Leave room for a border glyph.  */
  if (!FRAME_WINDOW_P (it->f)
      && !WINDOW_RIGHTMOST_P (it->w))
    it->last_visible_x -= 1;

  it->last_visible_y = window_text_bottom_y (w);

  /* For mode lines and alike, arrange for the first glyph having a
     left box line if the face specifies a box.  */
  if (base_face_id != DEFAULT_FACE_ID)
    {
      struct face *face;
      
      it->face_id = base_face_id;

      /* If we have a boxed mode line, make the first character appear
	 with a left box line.  */
      face = FACE_FROM_ID (it->f, base_face_id);
      if (face->box != FACE_NO_BOX)
	it->start_of_box_run_p = 1;
    }

  /* If a buffer position was specified, set the iterator there,
     getting overlays and face properties from that position.  */
  if (charpos >= BUF_BEG (current_buffer))
    {
      it->end_charpos = ZV;
      it->face_id = -1;
      IT_CHARPOS (*it) = charpos;
      
      /* Compute byte position if not specified.  */
      if (bytepos < charpos)
	IT_BYTEPOS (*it) = CHAR_TO_BYTE (charpos);
      else
	IT_BYTEPOS (*it) = bytepos;

      /* Compute faces etc.  */
      reseat (it, it->current.pos, 1);
    }
  
  CHECK_IT (it);
}


/* Initialize IT for the display of window W with window start POS.  */

void
start_display (it, w, pos)
     struct it *it;
     struct window *w;
     struct text_pos pos;
{
  struct glyph_row *row;
  int first_vpos = WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0;

  row = w->desired_matrix->rows + first_vpos;
  init_iterator (it, w, CHARPOS (pos), BYTEPOS (pos), row, DEFAULT_FACE_ID);

  if (!it->truncate_lines_p)
    {
      int start_at_line_beg_p;
      int first_y = it->current_y;
  
      /* If window start is not at a line start, skip forward to POS to
	 get the correct continuation lines width.  */
      start_at_line_beg_p = (CHARPOS (pos) == BEGV
			     || FETCH_BYTE (BYTEPOS (pos) - 1) == '\n');
      if (!start_at_line_beg_p)
	{
	  reseat_at_previous_visible_line_start (it);
	  move_it_to (it, CHARPOS (pos), -1, -1, -1, MOVE_TO_POS);

	  /* If lines are continued, this line may end in the middle
	     of a multi-glyph character (e.g. a control character
	     displayed as \003, or in the middle of an overlay
	     string).  In this case move_it_to above will not have
	     taken us to the start of the continuation line but to the
	     end of the continued line.  */
	  if (it->current_x > 0)
	    {
	      if (it->current.dpvec_index >= 0
		  || it->current.overlay_string_index >= 0)
		{
		  set_iterator_to_next (it, 1);
		  move_it_in_display_line_to (it, -1, -1, 0);
		}
	      
	      it->continuation_lines_width += it->current_x;
	    }

	  /* We're starting a new display line, not affected by the
	     height of the continued line, so clear the appropriate
	     fields in the iterator structure.  */
	  it->max_ascent = it->max_descent = 0;
	  it->max_phys_ascent = it->max_phys_descent = 0;
      
	  it->current_y = first_y;
	  it->vpos = 0;
	  it->current_x = it->hpos = 0;
	}
    }

#if 0 /* Don't assert the following because start_display is sometimes
         called intentionally with a window start that is not at a
	 line start.  Please leave this code in as a comment.  */
  
  /* Window start should be on a line start, now.  */
  xassert (it->continuation_lines_width
	   || IT_CHARPOS (it) == BEGV
	   || FETCH_BYTE (IT_BYTEPOS (it) - 1) == '\n');
#endif /* 0 */
}


/* Return 1 if POS is a position in ellipses displayed for invisible
   text.  W is the window we display, for text property lookup.  */

static int
in_ellipses_for_invisible_text_p (pos, w)
     struct display_pos *pos;
     struct window *w;
{
  Lisp_Object prop, window;
  int ellipses_p = 0;
  int charpos = CHARPOS (pos->pos);
  
  /* If POS specifies a position in a display vector, this might
     be for an ellipsis displayed for invisible text.  We won't
     get the iterator set up for delivering that ellipsis unless
     we make sure that it gets aware of the invisible text.  */
  if (pos->dpvec_index >= 0
      && pos->overlay_string_index < 0
      && CHARPOS (pos->string_pos) < 0
      && charpos > BEGV
      && (XSETWINDOW (window, w),
	  prop = Fget_char_property (make_number (charpos),
				     Qinvisible, window),
	  !TEXT_PROP_MEANS_INVISIBLE (prop)))
    {
      prop = Fget_char_property (make_number (charpos - 1), Qinvisible,
				 window);
      ellipses_p = 2 == TEXT_PROP_MEANS_INVISIBLE (prop);
    }

  return ellipses_p;
}


/* Initialize IT for stepping through current_buffer in window W,
   starting at position POS that includes overlay string and display
   vector/ control character translation position information.  Value
   is zero if there are overlay strings with newlines at POS.  */

static int
init_from_display_pos (it, w, pos)
     struct it *it;
     struct window *w;
     struct display_pos *pos;
{
  int charpos = CHARPOS (pos->pos), bytepos = BYTEPOS (pos->pos);
  int i, overlay_strings_with_newlines = 0;
  
  /* If POS specifies a position in a display vector, this might
     be for an ellipsis displayed for invisible text.  We won't
     get the iterator set up for delivering that ellipsis unless
     we make sure that it gets aware of the invisible text.  */
  if (in_ellipses_for_invisible_text_p (pos, w))
    {
      --charpos;
      bytepos = 0;
    }
    
  /* Keep in mind: the call to reseat in init_iterator skips invisible
     text, so we might end up at a position different from POS.  This
     is only a problem when POS is a row start after a newline and an
     overlay starts there with an after-string, and the overlay has an
     invisible property.  Since we don't skip invisible text in
     display_line and elsewhere immediately after consuming the
     newline before the row start, such a POS will not be in a string,
     but the call to init_iterator below will move us to the
     after-string.  */
  init_iterator (it, w, charpos, bytepos, NULL, DEFAULT_FACE_ID);

  for (i = 0; i < it->n_overlay_strings; ++i)
    {
      char *s = SDATA (it->overlay_strings[i]);
      char *e = s + SBYTES (it->overlay_strings[i]);
      
      while (s < e && *s != '\n')
	++s;

      if (s < e)
	{
	  overlay_strings_with_newlines = 1;
	  break;
	}
    }

  /* If position is within an overlay string, set up IT to the right
     overlay string.  */
  if (pos->overlay_string_index >= 0)
    {
      int relative_index;

      /* If the first overlay string happens to have a `display'
	 property for an image, the iterator will be set up for that
	 image, and we have to undo that setup first before we can
	 correct the overlay string index.  */
      if (it->method == next_element_from_image)
	pop_it (it);
      
      /* We already have the first chunk of overlay strings in
	 IT->overlay_strings.  Load more until the one for
	 pos->overlay_string_index is in IT->overlay_strings.  */
      if (pos->overlay_string_index >= OVERLAY_STRING_CHUNK_SIZE)
	{
	  int n = pos->overlay_string_index / OVERLAY_STRING_CHUNK_SIZE;
	  it->current.overlay_string_index = 0;
	  while (n--)
	    {
	      load_overlay_strings (it, 0);
	      it->current.overlay_string_index += OVERLAY_STRING_CHUNK_SIZE;
	    }
	}
      
      it->current.overlay_string_index = pos->overlay_string_index;
      relative_index = (it->current.overlay_string_index
			% OVERLAY_STRING_CHUNK_SIZE);
      it->string = it->overlay_strings[relative_index];
      xassert (STRINGP (it->string));
      it->current.string_pos = pos->string_pos;
      it->method = next_element_from_string;
    }
  
#if 0 /* This is bogus because POS not having an overlay string
	 position does not mean it's after the string.  Example: A
	 line starting with a before-string and initialization of IT
	 to the previous row's end position.  */
  else if (it->current.overlay_string_index >= 0)
    {
      /* If POS says we're already after an overlay string ending at
	 POS, make sure to pop the iterator because it will be in
	 front of that overlay string.  When POS is ZV, we've thereby
	 also ``processed'' overlay strings at ZV.  */
      while (it->sp)
	pop_it (it);
      it->current.overlay_string_index = -1;
      it->method = next_element_from_buffer;
      if (CHARPOS (pos->pos) == ZV)
	it->overlay_strings_at_end_processed_p = 1;
    }
#endif /* 0 */
  
  if (CHARPOS (pos->string_pos) >= 0)
    {
      /* Recorded position is not in an overlay string, but in another
	 string.  This can only be a string from a `display' property.
	 IT should already be filled with that string.  */
      it->current.string_pos = pos->string_pos;
      xassert (STRINGP (it->string));
    }

  /* Restore position in display vector translations, control
     character translations or ellipses.  */
  if (pos->dpvec_index >= 0)
    {
      if (it->dpvec == NULL)
	get_next_display_element (it);
      xassert (it->dpvec && it->current.dpvec_index == 0);
      it->current.dpvec_index = pos->dpvec_index;
    }
  
  CHECK_IT (it);
  return !overlay_strings_with_newlines;
}


/* Initialize IT for stepping through current_buffer in window W
   starting at ROW->start.  */

static void
init_to_row_start (it, w, row)
     struct it *it;
     struct window *w;
     struct glyph_row *row;
{
  init_from_display_pos (it, w, &row->start);
  it->continuation_lines_width = row->continuation_lines_width;
  CHECK_IT (it);
}

     
/* Initialize IT for stepping through current_buffer in window W
   starting in the line following ROW, i.e. starting at ROW->end.
   Value is zero if there are overlay strings with newlines at ROW's
   end position.  */

static int
init_to_row_end (it, w, row)
     struct it *it;
     struct window *w;
     struct glyph_row *row;
{
  int success = 0;
  
  if (init_from_display_pos (it, w, &row->end))
    {
      if (row->continued_p)
	it->continuation_lines_width
	  = row->continuation_lines_width + row->pixel_width;
      CHECK_IT (it);
      success = 1;
    }
  
  return success;
}




/***********************************************************************
			   Text properties
 ***********************************************************************/

/* Called when IT reaches IT->stop_charpos.  Handle text property and
   overlay changes.  Set IT->stop_charpos to the next position where
   to stop.  */

static void
handle_stop (it)
     struct it *it;
{
  enum prop_handled handled;
  int handle_overlay_change_p = 1;
  struct props *p;

  it->dpvec = NULL;
  it->current.dpvec_index = -1;

  do
    {
      handled = HANDLED_NORMALLY;
      
      /* Call text property handlers.  */
      for (p = it_props; p->handler; ++p)
	{
	  handled = p->handler (it);

	  if (handled == HANDLED_RECOMPUTE_PROPS)
	    break;
	  else if (handled == HANDLED_RETURN)
	    return;
	  else if (handled == HANDLED_OVERLAY_STRING_CONSUMED)
	    handle_overlay_change_p = 0;
	}

      if (handled != HANDLED_RECOMPUTE_PROPS)
	{
	  /* Don't check for overlay strings below when set to deliver
	     characters from a display vector.  */
	  if (it->method == next_element_from_display_vector)
	    handle_overlay_change_p = 0;

	  /* Handle overlay changes.  */
	  if (handle_overlay_change_p)
	    handled = handle_overlay_change (it);
      
	  /* Determine where to stop next.  */
	  if (handled == HANDLED_NORMALLY)
	    compute_stop_pos (it);
	}
    }
  while (handled == HANDLED_RECOMPUTE_PROPS);
}


/* Compute IT->stop_charpos from text property and overlay change
   information for IT's current position.  */

static void
compute_stop_pos (it)
     struct it *it;
{
  register INTERVAL iv, next_iv;
  Lisp_Object object, limit, position;

  /* If nowhere else, stop at the end.  */
  it->stop_charpos = it->end_charpos;
  
  if (STRINGP (it->string))
    {
      /* Strings are usually short, so don't limit the search for
	 properties.  */
      object = it->string;
      limit = Qnil;
      position = make_number (IT_STRING_CHARPOS (*it));
    }
  else
    {
      int charpos;

      /* If next overlay change is in front of the current stop pos
	 (which is IT->end_charpos), stop there.  Note: value of
	 next_overlay_change is point-max if no overlay change
	 follows.  */
      charpos = next_overlay_change (IT_CHARPOS (*it));
      if (charpos < it->stop_charpos)
	it->stop_charpos = charpos;

      /* If showing the region, we have to stop at the region
	 start or end because the face might change there.  */
      if (it->region_beg_charpos > 0)
	{
	  if (IT_CHARPOS (*it) < it->region_beg_charpos)
	    it->stop_charpos = min (it->stop_charpos, it->region_beg_charpos);
	  else if (IT_CHARPOS (*it) < it->region_end_charpos)
	    it->stop_charpos = min (it->stop_charpos, it->region_end_charpos);
	}
      
      /* Set up variables for computing the stop position from text
         property changes.  */
      XSETBUFFER (object, current_buffer);
      limit = make_number (IT_CHARPOS (*it) + TEXT_PROP_DISTANCE_LIMIT);
      position = make_number (IT_CHARPOS (*it));

    }

  /* Get the interval containing IT's position.  Value is a null
     interval if there isn't such an interval.  */
  iv = validate_interval_range (object, &position, &position, 0);
  if (!NULL_INTERVAL_P (iv))
    {
      Lisp_Object values_here[LAST_PROP_IDX];
      struct props *p;

      /* Get properties here.  */
      for (p = it_props; p->handler; ++p)
	values_here[p->idx] = textget (iv->plist, *p->name);

      /* Look for an interval following iv that has different
	 properties.  */
      for (next_iv = next_interval (iv);
	   (!NULL_INTERVAL_P (next_iv)
	    && (NILP (limit)
		|| XFASTINT (limit) > next_iv->position));
	   next_iv = next_interval (next_iv))
	{
	  for (p = it_props; p->handler; ++p)
	    {
	      Lisp_Object new_value;

	      new_value = textget (next_iv->plist, *p->name);
	      if (!EQ (values_here[p->idx], new_value))
		break;
	    }
	  
	  if (p->handler)
	    break;
	}

      if (!NULL_INTERVAL_P (next_iv))
	{
	  if (INTEGERP (limit)
	      && next_iv->position >= XFASTINT (limit))
	    /* No text property change up to limit.  */
	    it->stop_charpos = min (XFASTINT (limit), it->stop_charpos);
	  else
	    /* Text properties change in next_iv.  */
	    it->stop_charpos = min (it->stop_charpos, next_iv->position);
	}
    }

  xassert (STRINGP (it->string)
	   || (it->stop_charpos >= BEGV
	       && it->stop_charpos >= IT_CHARPOS (*it)));
}


/* Return the position of the next overlay change after POS in
   current_buffer.  Value is point-max if no overlay change
   follows.  This is like `next-overlay-change' but doesn't use
   xmalloc.  */

static int
next_overlay_change (pos)
     int pos;
{
  int noverlays;
  int endpos;
  Lisp_Object *overlays;
  int len;
  int i;

  /* Get all overlays at the given position.  */
  len = 10;
  overlays = (Lisp_Object *) alloca (len * sizeof *overlays);
  noverlays = overlays_at (pos, 0, &overlays, &len, &endpos, NULL, 1);
  if (noverlays > len)
    {
      len = noverlays;
      overlays = (Lisp_Object *) alloca (len * sizeof *overlays);
      noverlays = overlays_at (pos, 0, &overlays, &len, &endpos, NULL, 1);
    }

  /* If any of these overlays ends before endpos,
     use its ending point instead.  */
  for (i = 0; i < noverlays; ++i)
    {
      Lisp_Object oend;
      int oendpos;

      oend = OVERLAY_END (overlays[i]);
      oendpos = OVERLAY_POSITION (oend);
      endpos = min (endpos, oendpos);
    }

  return endpos;
}



/***********************************************************************
			    Fontification
 ***********************************************************************/

/* Handle changes in the `fontified' property of the current buffer by
   calling hook functions from Qfontification_functions to fontify
   regions of text.  */

static enum prop_handled
handle_fontified_prop (it)
     struct it *it;
{
  Lisp_Object prop, pos;
  enum prop_handled handled = HANDLED_NORMALLY;

  /* Get the value of the `fontified' property at IT's current buffer
     position.  (The `fontified' property doesn't have a special
     meaning in strings.)  If the value is nil, call functions from
     Qfontification_functions.  */
  if (!STRINGP (it->string)
      && it->s == NULL
      && !NILP (Vfontification_functions)
      && !NILP (Vrun_hooks)
      && (pos = make_number (IT_CHARPOS (*it)),
	  prop = Fget_char_property (pos, Qfontified, Qnil),
	  NILP (prop)))
    {
      int count = BINDING_STACK_SIZE ();
      Lisp_Object val;

      val = Vfontification_functions;
      specbind (Qfontification_functions, Qnil);
  
      if (!CONSP (val) || EQ (XCAR (val), Qlambda))
	safe_call1 (val, pos);
      else
	{
	  Lisp_Object globals, fn;
	  struct gcpro gcpro1, gcpro2;

	  globals = Qnil;
	  GCPRO2 (val, globals);
	  
	  for (; CONSP (val); val = XCDR (val))
	    {
	      fn = XCAR (val);
	      
	      if (EQ (fn, Qt))
		{
		  /* A value of t indicates this hook has a local
		     binding; it means to run the global binding too.
		     In a global value, t should not occur.  If it
		     does, we must ignore it to avoid an endless
		     loop.  */
		  for (globals = Fdefault_value (Qfontification_functions);
		       CONSP (globals);
		       globals = XCDR (globals))
		    {
		      fn = XCAR (globals);
		      if (!EQ (fn, Qt))
			safe_call1 (fn, pos);
		    }
		}
	      else
		safe_call1 (fn, pos);
	    }

	  UNGCPRO;
	}

      unbind_to (count, Qnil);

      /* Return HANDLED_RECOMPUTE_PROPS only if function fontified
	 something.  This avoids an endless loop if they failed to
	 fontify the text for which reason ever.  */
      if (!NILP (Fget_char_property (pos, Qfontified, Qnil)))
	handled = HANDLED_RECOMPUTE_PROPS;
    }

  return handled;
}



/***********************************************************************
				Faces
 ***********************************************************************/

/* Set up iterator IT from face properties at its current position.
   Called from handle_stop.  */

static enum prop_handled
handle_face_prop (it)
     struct it *it;
{
  int new_face_id, next_stop;
  
  if (!STRINGP (it->string))
    {
      new_face_id
	= face_at_buffer_position (it->w,
				   IT_CHARPOS (*it),
				   it->region_beg_charpos,
				   it->region_end_charpos,
				   &next_stop,
				   (IT_CHARPOS (*it)
				    + TEXT_PROP_DISTANCE_LIMIT),
				   0);
      
      /* Is this a start of a run of characters with box face?
	 Caveat: this can be called for a freshly initialized
	 iterator; face_id is -1 in this case.  We know that the new
	 face will not change until limit, i.e. if the new face has a
	 box, all characters up to limit will have one.  But, as
	 usual, we don't know whether limit is really the end.  */
      if (new_face_id != it->face_id)
	{
	  struct face *new_face = FACE_FROM_ID (it->f, new_face_id);
	  
	  /* If new face has a box but old face has not, this is
	     the start of a run of characters with box, i.e. it has
	     a shadow on the left side.  The value of face_id of the
	     iterator will be -1 if this is the initial call that gets
	     the face.  In this case, we have to look in front of IT's
	     position and see whether there is a face != new_face_id.  */
	  it->start_of_box_run_p
	    = (new_face->box != FACE_NO_BOX
	       && (it->face_id >= 0
		   || IT_CHARPOS (*it) == BEG
		   || new_face_id != face_before_it_pos (it)));
	  it->face_box_p = new_face->box != FACE_NO_BOX;
	}
    }
  else
    {
      int base_face_id, bufpos;

      if (it->current.overlay_string_index >= 0)
	bufpos = IT_CHARPOS (*it);
      else
	bufpos = 0;
      
      /* For strings from a buffer, i.e. overlay strings or strings
	 from a `display' property, use the face at IT's current
	 buffer position as the base face to merge with, so that
	 overlay strings appear in the same face as surrounding
	 text, unless they specify their own faces.  */
      base_face_id = underlying_face_id (it);
      
      new_face_id = face_at_string_position (it->w,
					     it->string,
					     IT_STRING_CHARPOS (*it),
					     bufpos,
					     it->region_beg_charpos,
					     it->region_end_charpos,
					     &next_stop,
					     base_face_id, 0);
      
#if 0 /* This shouldn't be neccessary.  Let's check it.  */
      /* If IT is used to display a mode line we would really like to
	 use the mode line face instead of the frame's default face.  */
      if (it->glyph_row == MATRIX_MODE_LINE_ROW (it->w->desired_matrix)
	  && new_face_id == DEFAULT_FACE_ID)
	new_face_id = CURRENT_MODE_LINE_FACE_ID (it->w);
#endif
      
      /* Is this a start of a run of characters with box?  Caveat:
	 this can be called for a freshly allocated iterator; face_id
	 is -1 is this case.  We know that the new face will not
	 change until the next check pos, i.e. if the new face has a
	 box, all characters up to that position will have a
	 box.  But, as usual, we don't know whether that position
	 is really the end.  */
      if (new_face_id != it->face_id)
	{
	  struct face *new_face = FACE_FROM_ID (it->f, new_face_id);
	  struct face *old_face = FACE_FROM_ID (it->f, it->face_id);
	  
	  /* If new face has a box but old face hasn't, this is the
	     start of a run of characters with box, i.e. it has a
	     shadow on the left side.  */
	  it->start_of_box_run_p
	    = new_face->box && (old_face == NULL || !old_face->box);
	  it->face_box_p = new_face->box != FACE_NO_BOX;
	}
    }
  
  it->face_id = new_face_id;
  return HANDLED_NORMALLY;
}


/* Return the ID of the face ``underlying'' IT's current position,
   which is in a string.  If the iterator is associated with a
   buffer, return the face at IT's current buffer position.
   Otherwise, use the iterator's base_face_id.  */

static int
underlying_face_id (it)
     struct it *it;
{
  int face_id = it->base_face_id, i;

  xassert (STRINGP (it->string));

  for (i = it->sp - 1; i >= 0; --i)
    if (NILP (it->stack[i].string))
      face_id = it->stack[i].face_id;

  return face_id;
}


/* Compute the face one character before or after the current position
   of IT.  BEFORE_P non-zero means get the face in front of IT's
   position.  Value is the id of the face.  */

static int
face_before_or_after_it_pos (it, before_p)
     struct it *it;
     int before_p;
{
  int face_id, limit;
  int next_check_charpos;
  struct text_pos pos;

  xassert (it->s == NULL);
    
  if (STRINGP (it->string))
    {
      int bufpos, base_face_id;
      
      /* No face change past the end of the string (for the case
	 we are padding with spaces).  No face change before the
	 string start.  */
      if (IT_STRING_CHARPOS (*it) >= SCHARS (it->string)
	  || (IT_STRING_CHARPOS (*it) == 0 && before_p))
	return it->face_id;

      /* Set pos to the position before or after IT's current position.  */
      if (before_p)
	pos = string_pos (IT_STRING_CHARPOS (*it) - 1, it->string);
      else
	/* For composition, we must check the character after the
           composition.  */
	pos = (it->what == IT_COMPOSITION
	       ? string_pos (IT_STRING_CHARPOS (*it) + it->cmp_len, it->string)
	       : string_pos (IT_STRING_CHARPOS (*it) + 1, it->string));

      if (it->current.overlay_string_index >= 0)
	bufpos = IT_CHARPOS (*it);
      else
	bufpos = 0;

      base_face_id = underlying_face_id (it);

      /* Get the face for ASCII, or unibyte.  */
      face_id = face_at_string_position (it->w,
					 it->string,
					 CHARPOS (pos),
					 bufpos,
					 it->region_beg_charpos,
					 it->region_end_charpos,
					 &next_check_charpos,
					 base_face_id, 0);

      /* Correct the face for charsets different from ASCII.  Do it
	 for the multibyte case only.  The face returned above is
	 suitable for unibyte text if IT->string is unibyte.  */
      if (STRING_MULTIBYTE (it->string))
	{
	  unsigned char *p = SDATA (it->string) + BYTEPOS (pos);
	  int rest = SBYTES (it->string) - BYTEPOS (pos);
	  int c, len;
	  struct face *face = FACE_FROM_ID (it->f, face_id);
      
	  c = string_char_and_length (p, rest, &len);
	  face_id = FACE_FOR_CHAR (it->f, face, c);
	}
    }
  else
    {
      if ((IT_CHARPOS (*it) >= ZV && !before_p)
	  || (IT_CHARPOS (*it) <= BEGV && before_p))
	return it->face_id;
      
      limit = IT_CHARPOS (*it) + TEXT_PROP_DISTANCE_LIMIT;
      pos = it->current.pos;
      
      if (before_p)
	DEC_TEXT_POS (pos, it->multibyte_p);
      else
	{
	  if (it->what == IT_COMPOSITION)
	    /* For composition, we must check the position after the
	       composition.  */
	    pos.charpos += it->cmp_len, pos.bytepos += it->len;
	  else
	    INC_TEXT_POS (pos, it->multibyte_p);
	}
      
      /* Determine face for CHARSET_ASCII, or unibyte.  */
      face_id = face_at_buffer_position (it->w,
					 CHARPOS (pos),
					 it->region_beg_charpos,
					 it->region_end_charpos,
					 &next_check_charpos,
					 limit, 0);

      /* Correct the face for charsets different from ASCII.  Do it
	 for the multibyte case only.  The face returned above is
	 suitable for unibyte text if current_buffer is unibyte.  */
      if (it->multibyte_p)
	{
	  int c = FETCH_MULTIBYTE_CHAR (CHARPOS (pos));
	  struct face *face = FACE_FROM_ID (it->f, face_id);
	  face_id = FACE_FOR_CHAR (it->f, face, c);
	}
    }
  
  return face_id;
}



/***********************************************************************
			    Invisible text
 ***********************************************************************/

/* Set up iterator IT from invisible properties at its current
   position.  Called from handle_stop.  */

static enum prop_handled
handle_invisible_prop (it)
     struct it *it;
{
  enum prop_handled handled = HANDLED_NORMALLY;

  if (STRINGP (it->string))
    {
      extern Lisp_Object Qinvisible;
      Lisp_Object prop, end_charpos, limit, charpos;

      /* Get the value of the invisible text property at the
	 current position.  Value will be nil if there is no such
	 property.  */
      charpos = make_number (IT_STRING_CHARPOS (*it));
      prop = Fget_text_property (charpos, Qinvisible, it->string);

      if (!NILP (prop)
	  && IT_STRING_CHARPOS (*it) < it->end_charpos)
	{
	  handled = HANDLED_RECOMPUTE_PROPS;
	  
	  /* Get the position at which the next change of the
	     invisible text property can be found in IT->string.
	     Value will be nil if the property value is the same for
	     all the rest of IT->string.  */
	  XSETINT (limit, SCHARS (it->string));
	  end_charpos = Fnext_single_property_change (charpos, Qinvisible,
						      it->string, limit);
	  
	  /* Text at current position is invisible.  The next
	     change in the property is at position end_charpos.
	     Move IT's current position to that position.  */
	  if (INTEGERP (end_charpos)
	      && XFASTINT (end_charpos) < XFASTINT (limit))
	    {
	      struct text_pos old;
	      old = it->current.string_pos;
	      IT_STRING_CHARPOS (*it) = XFASTINT (end_charpos);
	      compute_string_pos (&it->current.string_pos, old, it->string);
	    }
	  else
	    {
	      /* The rest of the string is invisible.  If this is an
		 overlay string, proceed with the next overlay string
		 or whatever comes and return a character from there.  */
	      if (it->current.overlay_string_index >= 0)
		{
		  next_overlay_string (it);
		  /* Don't check for overlay strings when we just
		     finished processing them.  */
		  handled = HANDLED_OVERLAY_STRING_CONSUMED;
		}
	      else
		{
		  IT_STRING_CHARPOS (*it) = SCHARS (it->string);
		  IT_STRING_BYTEPOS (*it) = SBYTES (it->string);
		}
	    }
	}
    }
  else
    {
      int invis_p, newpos, next_stop, start_charpos;
      Lisp_Object pos, prop, overlay;

      /* First of all, is there invisible text at this position?  */
      start_charpos = IT_CHARPOS (*it);
      pos = make_number (IT_CHARPOS (*it));
      prop = get_char_property_and_overlay (pos, Qinvisible, it->window,
					    &overlay);
      invis_p = TEXT_PROP_MEANS_INVISIBLE (prop);

      /* If we are on invisible text, skip over it.  */
      if (invis_p && IT_CHARPOS (*it) < it->end_charpos)
	{
	  /* Record whether we have to display an ellipsis for the
	     invisible text.  */
	  int display_ellipsis_p = invis_p == 2;

	  handled = HANDLED_RECOMPUTE_PROPS;
	  
	  /* Loop skipping over invisible text.  The loop is left at
	     ZV or with IT on the first char being visible again.  */
	  do
	    {
	      /* Try to skip some invisible text.  Return value is the
		 position reached which can be equal to IT's position
		 if there is nothing invisible here.  This skips both
		 over invisible text properties and overlays with
		 invisible property.  */
	      newpos = skip_invisible (IT_CHARPOS (*it),
				       &next_stop, ZV, it->window);

	      /* If we skipped nothing at all we weren't at invisible
		 text in the first place.  If everything to the end of
		 the buffer was skipped, end the loop.  */
	      if (newpos == IT_CHARPOS (*it) || newpos >= ZV)
		invis_p = 0;
	      else
		{
		  /* We skipped some characters but not necessarily
		     all there are.  Check if we ended up on visible
		     text.  Fget_char_property returns the property of
		     the char before the given position, i.e. if we
		     get invis_p = 0, this means that the char at
		     newpos is visible.  */
		  pos = make_number (newpos);
		  prop = Fget_char_property (pos, Qinvisible, it->window);
		  invis_p = TEXT_PROP_MEANS_INVISIBLE (prop);
		}
	      
	      /* If we ended up on invisible text, proceed to
		 skip starting with next_stop.  */
	      if (invis_p)
		IT_CHARPOS (*it) = next_stop;
	    }
	  while (invis_p);

	  /* The position newpos is now either ZV or on visible text.  */
	  IT_CHARPOS (*it) = newpos;
	  IT_BYTEPOS (*it) = CHAR_TO_BYTE (newpos);
	  
	  /* If there are before-strings at the start of invisible
	     text, and the text is invisible because of a text
	     property, arrange to show before-strings because 20.x did
	     it that way.  (If the text is invisible because of an
	     overlay property instead of a text property, this is
	     already handled in the overlay code.)  */
	  if (NILP (overlay)
	      && get_overlay_strings (it, start_charpos))
	    {
	      handled = HANDLED_RECOMPUTE_PROPS;
	      it->stack[it->sp - 1].display_ellipsis_p = display_ellipsis_p;
	    }
	  else if (display_ellipsis_p)
	    setup_for_ellipsis (it);
	}
    }

  return handled;
}


/* Make iterator IT return `...' next.  */

static void
setup_for_ellipsis (it)
     struct it *it;
{
  if (it->dp 
      && VECTORP (DISP_INVIS_VECTOR (it->dp)))
    {
      struct Lisp_Vector *v = XVECTOR (DISP_INVIS_VECTOR (it->dp));
      it->dpvec = v->contents;
      it->dpend = v->contents + v->size;
    }
  else 
    {
      /* Default `...'.  */
      it->dpvec = default_invis_vector;
      it->dpend = default_invis_vector + 3;
    }
  
  /* The ellipsis display does not replace the display of the
     character at the new position.  Indicate this by setting
     IT->dpvec_char_len to zero.  */
  it->dpvec_char_len = 0;
  
  it->current.dpvec_index = 0;
  it->method = next_element_from_display_vector;
}



/***********************************************************************
			    'display' property
 ***********************************************************************/

/* Set up iterator IT from `display' property at its current position.
   Called from handle_stop.  */

static enum prop_handled
handle_display_prop (it)
     struct it *it;
{
  Lisp_Object prop, object;
  struct text_pos *position;
  int display_replaced_p = 0;

  if (STRINGP (it->string))
    {
      object = it->string;
      position = &it->current.string_pos;
    }
  else
    {
      object = it->w->buffer;
      position = &it->current.pos;
    }

  /* Reset those iterator values set from display property values.  */
  it->font_height = Qnil;
  it->space_width = Qnil;
  it->voffset = 0;

  /* We don't support recursive `display' properties, i.e. string
     values that have a string `display' property, that have a string
     `display' property etc.  */
  if (!it->string_from_display_prop_p)
    it->area = TEXT_AREA;

  prop = Fget_char_property (make_number (position->charpos),
			     Qdisplay, object);
  if (NILP (prop))
    return HANDLED_NORMALLY;

  if (CONSP (prop)
      /* Simple properties.  */
      && !EQ (XCAR (prop), Qimage)
      && !EQ (XCAR (prop), Qspace)
      && !EQ (XCAR (prop), Qwhen)
      && !EQ (XCAR (prop), Qspace_width)
      && !EQ (XCAR (prop), Qheight)
      && !EQ (XCAR (prop), Qraise)
      /* Marginal area specifications.  */
      && !(CONSP (XCAR (prop)) && EQ (XCAR (XCAR (prop)), Qmargin))
      && !NILP (XCAR (prop)))
    {
      for (; CONSP (prop); prop = XCDR (prop))
	{
	  if (handle_single_display_prop (it, XCAR (prop), object,
					  position, display_replaced_p))
	    display_replaced_p = 1;
	}
    }
  else if (VECTORP (prop))
    {
      int i;
      for (i = 0; i < ASIZE (prop); ++i)
	if (handle_single_display_prop (it, AREF (prop, i), object,
					position, display_replaced_p))
	  display_replaced_p = 1;
    }
  else
    {
      if (handle_single_display_prop (it, prop, object, position, 0))
	display_replaced_p = 1;
    }

  return display_replaced_p ? HANDLED_RETURN : HANDLED_NORMALLY;
}


/* Value is the position of the end of the `display' property starting
   at START_POS in OBJECT.  */

static struct text_pos
display_prop_end (it, object, start_pos)
     struct it *it;
     Lisp_Object object;
     struct text_pos start_pos;
{
  Lisp_Object end;
  struct text_pos end_pos;

  end = Fnext_single_char_property_change (make_number (CHARPOS (start_pos)),
					   Qdisplay, object, Qnil);
  CHARPOS (end_pos) = XFASTINT (end);
  if (STRINGP (object))
    compute_string_pos (&end_pos, start_pos, it->string);
  else
    BYTEPOS (end_pos) = CHAR_TO_BYTE (XFASTINT (end));

  return end_pos;
}


/* Set up IT from a single `display' sub-property value PROP.  OBJECT
   is the object in which the `display' property was found.  *POSITION
   is the position at which it was found.  DISPLAY_REPLACED_P non-zero
   means that we previously saw a display sub-property which already
   replaced text display with something else, for example an image;
   ignore such properties after the first one has been processed.

   If PROP is a `space' or `image' sub-property, set *POSITION to the
   end position of the `display' property.

   Value is non-zero if something was found which replaces the display
   of buffer or string text.  */

static int
handle_single_display_prop (it, prop, object, position,
			    display_replaced_before_p)
     struct it *it;
     Lisp_Object prop;
     Lisp_Object object;
     struct text_pos *position;
     int display_replaced_before_p;
{
  Lisp_Object value;
  int replaces_text_display_p = 0;
  Lisp_Object form;

  /* If PROP is a list of the form `(when FORM . VALUE)', FORM is
     evaluated.  If the result is nil, VALUE is ignored.  */
  form = Qt;
  if (CONSP (prop) && EQ (XCAR (prop), Qwhen))
    {
      prop = XCDR (prop);
      if (!CONSP (prop))
	return 0;
      form = XCAR (prop);
      prop = XCDR (prop);
    }

  if (!NILP (form) && !EQ (form, Qt))
    {
      int count = BINDING_STACK_SIZE ();
      struct gcpro gcpro1;

      /* Bind `object' to the object having the `display' property, a
	 buffer or string.  Bind `position' to the position in the
	 object where the property was found, and `buffer-position'
	 to the current position in the buffer.  */
      specbind (Qobject, object);
      specbind (Qposition, make_number (CHARPOS (*position)));
      specbind (Qbuffer_position,
		make_number (STRINGP (object)
			     ? IT_CHARPOS (*it) : CHARPOS (*position)));
      GCPRO1 (form);
      form = safe_eval (form);
      UNGCPRO;
      unbind_to (count, Qnil);
    }
  
  if (NILP (form))
    return 0;

  if (CONSP (prop)
      && EQ (XCAR (prop), Qheight)
      && CONSP (XCDR (prop)))
    {
      if (FRAME_TERMCAP_P (it->f) || FRAME_MSDOS_P (it->f))
	return 0;
      
      /* `(height HEIGHT)'.  */
      it->font_height = XCAR (XCDR (prop));
      if (!NILP (it->font_height))
	{
	  struct face *face = FACE_FROM_ID (it->f, it->face_id);
	  int new_height = -1;

	  if (CONSP (it->font_height)
	      && (EQ (XCAR (it->font_height), Qplus)
		  || EQ (XCAR (it->font_height), Qminus))
	      && CONSP (XCDR (it->font_height))
	      && INTEGERP (XCAR (XCDR (it->font_height))))
	    {
	      /* `(+ N)' or `(- N)' where N is an integer.  */
	      int steps = XINT (XCAR (XCDR (it->font_height)));
	      if (EQ (XCAR (it->font_height), Qplus))
		steps = - steps;
	      it->face_id = smaller_face (it->f, it->face_id, steps);
	    }
	  else if (FUNCTIONP (it->font_height))
	    {
	      /* Call function with current height as argument.
		 Value is the new height.  */
	      Lisp_Object height;
	      height = safe_call1 (it->font_height,
				   face->lface[LFACE_HEIGHT_INDEX]);
	      if (NUMBERP (height))
		new_height = XFLOATINT (height);
	    }
	  else if (NUMBERP (it->font_height))
	    {
	      /* Value is a multiple of the canonical char height.  */
	      struct face *face;
	      
	      face = FACE_FROM_ID (it->f, DEFAULT_FACE_ID);
	      new_height = (XFLOATINT (it->font_height)
			    * XINT (face->lface[LFACE_HEIGHT_INDEX]));
	    }
	  else
	    {
	      /* Evaluate IT->font_height with `height' bound to the
		 current specified height to get the new height.  */
	      Lisp_Object value;
	      int count = BINDING_STACK_SIZE ();
	      
	      specbind (Qheight, face->lface[LFACE_HEIGHT_INDEX]);
	      value = safe_eval (it->font_height);
	      unbind_to (count, Qnil);
	      
	      if (NUMBERP (value))
		new_height = XFLOATINT (value);
	    }
	  
	  if (new_height > 0)
	    it->face_id = face_with_height (it->f, it->face_id, new_height);
	}
    }
  else if (CONSP (prop)
	   && EQ (XCAR (prop), Qspace_width)
	   && CONSP (XCDR (prop)))
    {
      /* `(space_width WIDTH)'.  */
      if (FRAME_TERMCAP_P (it->f) || FRAME_MSDOS_P (it->f))
	return 0;
      
      value = XCAR (XCDR (prop));
      if (NUMBERP (value) && XFLOATINT (value) > 0)
	it->space_width = value;
    }
  else if (CONSP (prop)
	   && EQ (XCAR (prop), Qraise)
	   && CONSP (XCDR (prop)))
    {
      /* `(raise FACTOR)'.  */
      if (FRAME_TERMCAP_P (it->f) || FRAME_MSDOS_P (it->f))
	return 0;
      
#ifdef HAVE_WINDOW_SYSTEM
      value = XCAR (XCDR (prop));
      if (NUMBERP (value))
	{
	  struct face *face = FACE_FROM_ID (it->f, it->face_id);
	  it->voffset = - (XFLOATINT (value)
			   * (FONT_HEIGHT (face->font)));
	}
#endif /* HAVE_WINDOW_SYSTEM */
    }
  else if (!it->string_from_display_prop_p)
    {
      /* `((margin left-margin) VALUE)' or `((margin right-margin)
	 VALUE) or `((margin nil) VALUE)' or VALUE.  */
      Lisp_Object location, value;
      struct text_pos start_pos;
      int valid_p;

      /* Characters having this form of property are not displayed, so
         we have to find the end of the property.  */
      start_pos = *position;
      *position = display_prop_end (it, object, start_pos);
      value = Qnil;

      /* Let's stop at the new position and assume that all
	 text properties change there.  */
      it->stop_charpos = position->charpos;

      location = Qunbound;
      if (CONSP (prop) && CONSP (XCAR (prop)))
	{
	  Lisp_Object tem;
	  
	  value = XCDR (prop);
	  if (CONSP (value))
	    value = XCAR (value);

	  tem = XCAR (prop);
	  if (EQ (XCAR (tem), Qmargin)
	      && (tem = XCDR (tem),
		  tem = CONSP (tem) ? XCAR (tem) : Qnil,
		  (NILP (tem)
		   || EQ (tem, Qleft_margin)
		   || EQ (tem, Qright_margin))))
	    location = tem;
	}

      if (EQ (location, Qunbound))
	{
	  location = Qnil;
	  value = prop;
	}

#ifdef HAVE_WINDOW_SYSTEM
      if (FRAME_TERMCAP_P (it->f))
	valid_p = STRINGP (value);
      else
	valid_p = (STRINGP (value)
		   || (CONSP (value) && EQ (XCAR (value), Qspace))
		   || valid_image_p (value));
#else /* not HAVE_WINDOW_SYSTEM */
      valid_p = STRINGP (value);
#endif /* not HAVE_WINDOW_SYSTEM */
      
      if ((EQ (location, Qleft_margin)
	   || EQ (location, Qright_margin)
	   || NILP (location))
	  && valid_p
	  && !display_replaced_before_p)
	{
	  replaces_text_display_p = 1;
	  
	  /* Save current settings of IT so that we can restore them
	     when we are finished with the glyph property value.  */
	  push_it (it);
	  
	  if (NILP (location))
	    it->area = TEXT_AREA;
	  else if (EQ (location, Qleft_margin))
	    it->area = LEFT_MARGIN_AREA;
	  else
	    it->area = RIGHT_MARGIN_AREA;
	  
	  if (STRINGP (value))
	    {
	      it->string = value;
	      it->multibyte_p = STRING_MULTIBYTE (it->string);
	      it->current.overlay_string_index = -1;
	      IT_STRING_CHARPOS (*it) = IT_STRING_BYTEPOS (*it) = 0;
	      it->end_charpos = it->string_nchars = SCHARS (it->string);
	      it->method = next_element_from_string;
	      it->stop_charpos = 0;
	      it->string_from_display_prop_p = 1;
	      /* Say that we haven't consumed the characters with
		 `display' property yet.  The call to pop_it in
		 set_iterator_to_next will clean this up.  */
	      *position = start_pos;
	    }
	  else if (CONSP (value) && EQ (XCAR (value), Qspace))
	    {
	      it->method = next_element_from_stretch;
	      it->object = value;
	      it->current.pos = it->position = start_pos;
	    }
#ifdef HAVE_WINDOW_SYSTEM
	  else
	    {
	      it->what = IT_IMAGE;
	      it->image_id = lookup_image (it->f, value);
	      it->position = start_pos;
	      it->object = NILP (object) ? it->w->buffer : object;
	      it->method = next_element_from_image;
	      
	      /* Say that we haven't consumed the characters with
		 `display' property yet.  The call to pop_it in
		 set_iterator_to_next will clean this up.  */
	      *position = start_pos;
	    }
#endif /* HAVE_WINDOW_SYSTEM */
	}
      else
	/* Invalid property or property not supported.  Restore
	   the position to what it was before.  */
	*position = start_pos;
    }

  return replaces_text_display_p;
}


/* Check if PROP is a display sub-property value whose text should be
   treated as intangible.  */

static int
single_display_prop_intangible_p (prop)
     Lisp_Object prop;
{
  /* Skip over `when FORM'.  */
  if (CONSP (prop) && EQ (XCAR (prop), Qwhen))
    {
      prop = XCDR (prop);
      if (!CONSP (prop))
	return 0;
      prop = XCDR (prop);
    }

  if (!CONSP (prop))
    return 0;

  /* Skip over `margin LOCATION'.  If LOCATION is in the margins,
     we don't need to treat text as intangible.  */
  if (EQ (XCAR (prop), Qmargin))
    {
      prop = XCDR (prop);
      if (!CONSP (prop))
	return 0;

      prop = XCDR (prop);
      if (!CONSP (prop)
	  || EQ (XCAR (prop), Qleft_margin)
	  || EQ (XCAR (prop), Qright_margin))
	return 0;
    }
  
  return CONSP (prop) && EQ (XCAR (prop), Qimage);
}


/* Check if PROP is a display property value whose text should be
   treated as intangible.  */

int
display_prop_intangible_p (prop)
     Lisp_Object prop;
{
  if (CONSP (prop)
      && CONSP (XCAR (prop))
      && !EQ (Qmargin, XCAR (XCAR (prop))))
    {
      /* A list of sub-properties.  */
      while (CONSP (prop))
	{
	  if (single_display_prop_intangible_p (XCAR (prop)))
	    return 1;
	  prop = XCDR (prop);
	}
    }
  else if (VECTORP (prop))
    {
      /* A vector of sub-properties.  */
      int i;
      for (i = 0; i < ASIZE (prop); ++i)
	if (single_display_prop_intangible_p (AREF (prop, i)))
	  return 1;
    }
  else
    return single_display_prop_intangible_p (prop);

  return 0;
}


/* Return 1 if PROP is a display sub-property value containing STRING.  */

static int
single_display_prop_string_p (prop, string)
     Lisp_Object prop, string;
{
  if (EQ (string, prop))
    return 1;
  
  /* Skip over `when FORM'.  */
  if (CONSP (prop) && EQ (XCAR (prop), Qwhen))
    {
      prop = XCDR (prop);
      if (!CONSP (prop))
	return 0;
      prop = XCDR (prop);
    }

  if (CONSP (prop))
    /* Skip over `margin LOCATION'.  */
    if (EQ (XCAR (prop), Qmargin))
      {
	prop = XCDR (prop);
	if (!CONSP (prop))
	  return 0;

	prop = XCDR (prop);
	if (!CONSP (prop))
	  return 0;
      }
  
  return CONSP (prop) && EQ (XCAR (prop), string);
}


/* Return 1 if STRING appears in the `display' property PROP.  */

static int
display_prop_string_p (prop, string)
     Lisp_Object prop, string;
{
  if (CONSP (prop)
      && CONSP (XCAR (prop))
      && !EQ (Qmargin, XCAR (XCAR (prop))))
    {
      /* A list of sub-properties.  */
      while (CONSP (prop))
	{
	  if (single_display_prop_string_p (XCAR (prop), string))
	    return 1;
	  prop = XCDR (prop);
	}
    }
  else if (VECTORP (prop))
    {
      /* A vector of sub-properties.  */
      int i;
      for (i = 0; i < ASIZE (prop); ++i)
	if (single_display_prop_string_p (AREF (prop, i), string))
	  return 1;
    }
  else
    return single_display_prop_string_p (prop, string);

  return 0;
}


/* Determine from which buffer position in W's buffer STRING comes
   from.  AROUND_CHARPOS is an approximate position where it could
   be from.  Value is the buffer position or 0 if it couldn't be
   determined.

   W's buffer must be current.

   This function is necessary because we don't record buffer positions
   in glyphs generated from strings (to keep struct glyph small).
   This function may only use code that doesn't eval because it is
   called asynchronously from note_mouse_highlight.  */

int
string_buffer_position (w, string, around_charpos)
     struct window *w;
     Lisp_Object string;
     int around_charpos;
{
  Lisp_Object limit, prop, pos;
  const int MAX_DISTANCE = 1000;
  int found = 0;

  pos = make_number (around_charpos);
  limit = make_number (min (XINT (pos) + MAX_DISTANCE, ZV));
  while (!found && !EQ (pos, limit))
    {
      prop = Fget_char_property (pos, Qdisplay, Qnil);
      if (!NILP (prop) && display_prop_string_p (prop, string))
	found = 1;
      else
	pos = Fnext_single_char_property_change (pos, Qdisplay, Qnil, limit);
    }

  if (!found)
    {
      pos = make_number (around_charpos);
      limit = make_number (max (XINT (pos) - MAX_DISTANCE, BEGV));
      while (!found && !EQ (pos, limit))
	{
	  prop = Fget_char_property (pos, Qdisplay, Qnil);
	  if (!NILP (prop) && display_prop_string_p (prop, string))
	    found = 1;
	  else
	    pos = Fprevious_single_char_property_change (pos, Qdisplay, Qnil,
							 limit);
	}
    }

  return found ? XINT (pos) : 0;
}



/***********************************************************************
			`composition' property
 ***********************************************************************/

/* Set up iterator IT from `composition' property at its current
   position.  Called from handle_stop.  */

static enum prop_handled
handle_composition_prop (it)
     struct it *it;
{
  Lisp_Object prop, string;
  int pos, pos_byte, end;
  enum prop_handled handled = HANDLED_NORMALLY;

  if (STRINGP (it->string))
    {
      pos = IT_STRING_CHARPOS (*it);
      pos_byte = IT_STRING_BYTEPOS (*it);
      string = it->string;
    }
  else
    {
      pos = IT_CHARPOS (*it);
      pos_byte = IT_BYTEPOS (*it);
      string = Qnil;
    }

  /* If there's a valid composition and point is not inside of the
     composition (in the case that the composition is from the current
     buffer), draw a glyph composed from the composition components.  */
  if (find_composition (pos, -1, &pos, &end, &prop, string)
      && COMPOSITION_VALID_P (pos, end, prop)
      && (STRINGP (it->string) || (PT <= pos || PT >= end)))
    {
      int id = get_composition_id (pos, pos_byte, end - pos, prop, string);

      if (id >= 0)
	{
	  it->method = next_element_from_composition;
	  it->cmp_id = id;
	  it->cmp_len = COMPOSITION_LENGTH (prop);
	  /* For a terminal, draw only the first character of the
             components.  */
	  it->c = COMPOSITION_GLYPH (composition_table[id], 0);
	  it->len = (STRINGP (it->string)
		     ? string_char_to_byte (it->string, end)
		     : CHAR_TO_BYTE (end)) - pos_byte;
	  it->stop_charpos = end;
	  handled = HANDLED_RETURN;
	}
    }

  return handled;
}



/***********************************************************************
			   Overlay strings
 ***********************************************************************/

/* The following structure is used to record overlay strings for
   later sorting in load_overlay_strings.  */

struct overlay_entry
{
  Lisp_Object overlay;
  Lisp_Object string;
  int priority;
  int after_string_p;
};


/* Set up iterator IT from overlay strings at its current position.
   Called from handle_stop.  */

static enum prop_handled
handle_overlay_change (it)
     struct it *it;
{
  if (!STRINGP (it->string) && get_overlay_strings (it, 0))
    return HANDLED_RECOMPUTE_PROPS;
  else
    return HANDLED_NORMALLY;
}


/* Set up the next overlay string for delivery by IT, if there is an
   overlay string to deliver.  Called by set_iterator_to_next when the
   end of the current overlay string is reached.  If there are more
   overlay strings to display, IT->string and
   IT->current.overlay_string_index are set appropriately here.
   Otherwise IT->string is set to nil.  */
   
static void
next_overlay_string (it)
     struct it *it;
{
  ++it->current.overlay_string_index;
  if (it->current.overlay_string_index == it->n_overlay_strings)
    {
      /* No more overlay strings.  Restore IT's settings to what
	 they were before overlay strings were processed, and
	 continue to deliver from current_buffer.  */
      int display_ellipsis_p = it->stack[it->sp - 1].display_ellipsis_p;
      
      pop_it (it);
      xassert (it->stop_charpos >= BEGV
	       && it->stop_charpos <= it->end_charpos);
      it->string = Qnil;
      it->current.overlay_string_index = -1;
      SET_TEXT_POS (it->current.string_pos, -1, -1);
      it->n_overlay_strings = 0;
      it->method = next_element_from_buffer;

      /* If we're at the end of the buffer, record that we have
	 processed the overlay strings there already, so that
	 next_element_from_buffer doesn't try it again.  */
      if (IT_CHARPOS (*it) >= it->end_charpos)
	it->overlay_strings_at_end_processed_p = 1;

      /* If we have to display `...' for invisible text, set
	 the iterator up for that.  */
      if (display_ellipsis_p)
	setup_for_ellipsis (it);
    }
  else
    {
      /* There are more overlay strings to process.  If
	 IT->current.overlay_string_index has advanced to a position
	 where we must load IT->overlay_strings with more strings, do
	 it.  */
      int i = it->current.overlay_string_index % OVERLAY_STRING_CHUNK_SIZE;
  
      if (it->current.overlay_string_index && i == 0)
	load_overlay_strings (it, 0);

      /* Initialize IT to deliver display elements from the overlay
         string.  */
      it->string = it->overlay_strings[i];
      it->multibyte_p = STRING_MULTIBYTE (it->string);
      SET_TEXT_POS (it->current.string_pos, 0, 0);
      it->method = next_element_from_string;
      it->stop_charpos = 0;
    }
  
  CHECK_IT (it);
}


/* Compare two overlay_entry structures E1 and E2.  Used as a
   comparison function for qsort in load_overlay_strings.  Overlay
   strings for the same position are sorted so that

   1. All after-strings come in front of before-strings, except
   when they come from the same overlay.
   
   2. Within after-strings, strings are sorted so that overlay strings
   from overlays with higher priorities come first.

   2. Within before-strings, strings are sorted so that overlay
   strings from overlays with higher priorities come last.

   Value is analogous to strcmp.  */

  
static int
compare_overlay_entries (e1, e2)
     void *e1, *e2;
{
  struct overlay_entry *entry1 = (struct overlay_entry *) e1;
  struct overlay_entry *entry2 = (struct overlay_entry *) e2;
  int result;

  if (entry1->after_string_p != entry2->after_string_p)
    {
      /* Let after-strings appear in front of before-strings if
	 they come from different overlays.  */
      if (EQ (entry1->overlay, entry2->overlay))
	result = entry1->after_string_p ? 1 : -1;
      else
	result = entry1->after_string_p ? -1 : 1;
    }
  else if (entry1->after_string_p)
    /* After-strings sorted in order of decreasing priority.  */
    result = entry2->priority - entry1->priority;
  else
    /* Before-strings sorted in order of increasing priority.  */
    result = entry1->priority - entry2->priority;

  return result;
}


/* Load the vector IT->overlay_strings with overlay strings from IT's
   current buffer position, or from CHARPOS if that is > 0.  Set
   IT->n_overlays to the total number of overlay strings found.

   Overlay strings are processed OVERLAY_STRING_CHUNK_SIZE strings at
   a time.  On entry into load_overlay_strings,
   IT->current.overlay_string_index gives the number of overlay
   strings that have already been loaded by previous calls to this
   function.

   IT->add_overlay_start contains an additional overlay start
   position to consider for taking overlay strings from, if non-zero.
   This position comes into play when the overlay has an `invisible'
   property, and both before and after-strings.  When we've skipped to
   the end of the overlay, because of its `invisible' property, we
   nevertheless want its before-string to appear.
   IT->add_overlay_start will contain the overlay start position
   in this case.

   Overlay strings are sorted so that after-string strings come in
   front of before-string strings.  Within before and after-strings,
   strings are sorted by overlay priority.  See also function
   compare_overlay_entries.  */
   
static void
load_overlay_strings (it, charpos)
     struct it *it;
     int charpos;
{
  extern Lisp_Object Qafter_string, Qbefore_string, Qwindow, Qpriority;
  Lisp_Object ov, overlay, window, str, invisible;
  int start, end;
  int size = 20;
  int n = 0, i, j, invis_p;
  struct overlay_entry *entries
    = (struct overlay_entry *) alloca (size * sizeof *entries);

  if (charpos <= 0)
    charpos = IT_CHARPOS (*it);

  /* Append the overlay string STRING of overlay OVERLAY to vector
     `entries' which has size `size' and currently contains `n'
     elements.  AFTER_P non-zero means STRING is an after-string of
     OVERLAY.  */
#define RECORD_OVERLAY_STRING(OVERLAY, STRING, AFTER_P)			\
  do									\
    {									\
      Lisp_Object priority;						\
									\
      if (n == size)							\
	{								\
	  int new_size = 2 * size;					\
	  struct overlay_entry *old = entries;				\
	  entries =							\
            (struct overlay_entry *) alloca (new_size			\
					     * sizeof *entries);	\
	  bcopy (old, entries, size * sizeof *entries);			\
	  size = new_size;						\
	}								\
									\
      entries[n].string = (STRING);					\
      entries[n].overlay = (OVERLAY);					\
      priority = Foverlay_get ((OVERLAY), Qpriority);			\
      entries[n].priority = INTEGERP (priority) ? XINT (priority) : 0;  \
      entries[n].after_string_p = (AFTER_P);				\
      ++n;								\
    }									\
  while (0)

  /* Process overlay before the overlay center.  */
  for (ov = current_buffer->overlays_before; CONSP (ov); ov = XCDR (ov))
    {
      overlay = XCAR (ov);
      xassert (OVERLAYP (overlay));
      start = OVERLAY_POSITION (OVERLAY_START (overlay));
      end = OVERLAY_POSITION (OVERLAY_END (overlay));
      
      if (end < charpos)
	break;

      /* Skip this overlay if it doesn't start or end at IT's current
	 position.  */
      if (end != charpos && start != charpos)
	continue;
      
      /* Skip this overlay if it doesn't apply to IT->w.  */
      window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != it->w)
	continue;

      /* If the text ``under'' the overlay is invisible, both before-
	 and after-strings from this overlay are visible; start and
	 end position are indistinguishable.  */
      invisible = Foverlay_get (overlay, Qinvisible);
      invis_p = TEXT_PROP_MEANS_INVISIBLE (invisible);

      /* If overlay has a non-empty before-string, record it.  */
      if ((start == charpos || (end == charpos && invis_p))
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str))
	  && SCHARS (str))
	RECORD_OVERLAY_STRING (overlay, str, 0);
      
      /* If overlay has a non-empty after-string, record it.  */
      if ((end == charpos || (start == charpos && invis_p))
	  && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str))
	  && SCHARS (str))
	RECORD_OVERLAY_STRING (overlay, str, 1);
    }
      
  /* Process overlays after the overlay center.  */
  for (ov = current_buffer->overlays_after; CONSP (ov); ov = XCDR (ov))
    {
      overlay = XCAR (ov);
      xassert (OVERLAYP (overlay));
      start = OVERLAY_POSITION (OVERLAY_START (overlay));
      end = OVERLAY_POSITION (OVERLAY_END (overlay));

      if (start > charpos)
	break;
      
      /* Skip this overlay if it doesn't start or end at IT's current
	 position.  */
      if (end != charpos && start != charpos)
	continue;

      /* Skip this overlay if it doesn't apply to IT->w.  */
      window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != it->w)
	continue;
      
      /* If the text ``under'' the overlay is invisible, it has a zero
	 dimension, and both before- and after-strings apply.  */
      invisible = Foverlay_get (overlay, Qinvisible);
      invis_p = TEXT_PROP_MEANS_INVISIBLE (invisible);

      /* If overlay has a non-empty before-string, record it.  */
      if ((start == charpos || (end == charpos && invis_p))
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str))
	  && SCHARS (str))
	RECORD_OVERLAY_STRING (overlay, str, 0);
			       
      /* If overlay has a non-empty after-string, record it.  */
      if ((end == charpos || (start == charpos && invis_p))
	  && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str))
	  && SCHARS (str))
	RECORD_OVERLAY_STRING (overlay, str, 1);
    }

#undef RECORD_OVERLAY_STRING
   
  /* Sort entries.  */
  if (n > 1)
    qsort (entries, n, sizeof *entries, compare_overlay_entries);

  /* Record the total number of strings to process.  */
  it->n_overlay_strings = n;

  /* IT->current.overlay_string_index is the number of overlay strings
     that have already been consumed by IT.  Copy some of the
     remaining overlay strings to IT->overlay_strings.  */
  i = 0;
  j = it->current.overlay_string_index;
  while (i < OVERLAY_STRING_CHUNK_SIZE && j < n)
    it->overlay_strings[i++] = entries[j++].string;

  CHECK_IT (it);
}


/* Get the first chunk of overlay strings at IT's current buffer
   position, or at CHARPOS if that is > 0.  Value is non-zero if at
   least one overlay string was found.  */

static int
get_overlay_strings (it, charpos)
     struct it *it;
     int charpos;
{
  /* Get the first OVERLAY_STRING_CHUNK_SIZE overlay strings to
     process.  This fills IT->overlay_strings with strings, and sets
     IT->n_overlay_strings to the total number of strings to process.
     IT->pos.overlay_string_index has to be set temporarily to zero
     because load_overlay_strings needs this; it must be set to -1
     when no overlay strings are found because a zero value would
     indicate a position in the first overlay string.  */
  it->current.overlay_string_index = 0;
  load_overlay_strings (it, charpos);

  /* If we found overlay strings, set up IT to deliver display
     elements from the first one.  Otherwise set up IT to deliver
     from current_buffer.  */
  if (it->n_overlay_strings)
    {
      /* Make sure we know settings in current_buffer, so that we can
	 restore meaningful values when we're done with the overlay
	 strings.  */
      compute_stop_pos (it);
      xassert (it->face_id >= 0);
      
      /* Save IT's settings.  They are restored after all overlay
	 strings have been processed.  */
      xassert (it->sp == 0);
      push_it (it);

      /* Set up IT to deliver display elements from the first overlay
	 string.  */
      IT_STRING_CHARPOS (*it) = IT_STRING_BYTEPOS (*it) = 0;
      it->string = it->overlay_strings[0];
      it->stop_charpos = 0;
      xassert (STRINGP (it->string));
      it->end_charpos = SCHARS (it->string);
      it->multibyte_p = SMBP (it->string);
      it->method = next_element_from_string;
    }
  else
    {
      it->string = Qnil;
      it->current.overlay_string_index = -1;
      it->method = next_element_from_buffer;
    }

  CHECK_IT (it);

  /* Value is non-zero if we found at least one overlay string.  */
  return STRINGP (it->string);
}



/***********************************************************************
		      Saving and restoring state
 ***********************************************************************/

/* Save current settings of IT on IT->stack.  Called, for example,
   before setting up IT for an overlay string, to be able to restore
   IT's settings to what they were after the overlay string has been
   processed.  */

static void
push_it (it)
     struct it *it;
{
  struct iterator_stack_entry *p;
  
  xassert (it->sp < 2);
  p = it->stack + it->sp;

  p->stop_charpos = it->stop_charpos;
  xassert (it->face_id >= 0);
  p->face_id = it->face_id;
  p->string = it->string;
  p->pos = it->current;
  p->end_charpos = it->end_charpos;
  p->string_nchars = it->string_nchars;
  p->area = it->area;
  p->multibyte_p = it->multibyte_p;
  p->space_width = it->space_width;
  p->font_height = it->font_height;
  p->voffset = it->voffset;
  p->string_from_display_prop_p = it->string_from_display_prop_p;
  p->display_ellipsis_p = 0;
  ++it->sp;
}


/* Restore IT's settings from IT->stack.  Called, for example, when no
   more overlay strings must be processed, and we return to delivering
   display elements from a buffer, or when the end of a string from a
   `display' property is reached and we return to delivering display
   elements from an overlay string, or from a buffer.  */

static void
pop_it (it)
     struct it *it;
{
  struct iterator_stack_entry *p;
  
  xassert (it->sp > 0);
  --it->sp;
  p = it->stack + it->sp;
  it->stop_charpos = p->stop_charpos;
  it->face_id = p->face_id;
  it->string = p->string;
  it->current = p->pos;
  it->end_charpos = p->end_charpos;
  it->string_nchars = p->string_nchars;
  it->area = p->area;
  it->multibyte_p = p->multibyte_p;
  it->space_width = p->space_width;
  it->font_height = p->font_height;
  it->voffset = p->voffset;
  it->string_from_display_prop_p = p->string_from_display_prop_p;
}



/***********************************************************************
			  Moving over lines
 ***********************************************************************/

/* Set IT's current position to the previous line start.  */

static void
back_to_previous_line_start (it)
     struct it *it;
{
  IT_CHARPOS (*it) = find_next_newline_no_quit (IT_CHARPOS (*it) - 1, -1);
  IT_BYTEPOS (*it) = CHAR_TO_BYTE (IT_CHARPOS (*it));
}


/* Move IT to the next line start.
   
   Value is non-zero if a newline was found.  Set *SKIPPED_P to 1 if
   we skipped over part of the text (as opposed to moving the iterator
   continuously over the text).  Otherwise, don't change the value
   of *SKIPPED_P.
   
   Newlines may come from buffer text, overlay strings, or strings
   displayed via the `display' property.  That's the reason we can't
   simply use find_next_newline_no_quit.

   Note that this function may not skip over invisible text that is so
   because of text properties and immediately follows a newline.  If
   it would, function reseat_at_next_visible_line_start, when called
   from set_iterator_to_next, would effectively make invisible
   characters following a newline part of the wrong glyph row, which
   leads to wrong cursor motion.  */

static int
forward_to_next_line_start (it, skipped_p)
     struct it *it;
     int *skipped_p;
{
  int old_selective, newline_found_p, n;
  const int MAX_NEWLINE_DISTANCE = 500;

  /* If already on a newline, just consume it to avoid unintended
     skipping over invisible text below.  */
  if (it->what == IT_CHARACTER
      && it->c == '\n'
      && CHARPOS (it->position) == IT_CHARPOS (*it))
    {
      set_iterator_to_next (it, 0);
      it->c = 0;
      return 1;
    }

  /* Don't handle selective display in the following.  It's (a)
     unnecessary because it's done by the caller, and (b) leads to an
     infinite recursion because next_element_from_ellipsis indirectly
     calls this function.  */
  old_selective = it->selective;
  it->selective = 0;

  /* Scan for a newline within MAX_NEWLINE_DISTANCE display elements
     from buffer text.  */
  for (n = newline_found_p = 0;
       !newline_found_p && n < MAX_NEWLINE_DISTANCE;
       n += STRINGP (it->string) ? 0 : 1)
    {
      if (!get_next_display_element (it))
	break;
      newline_found_p = it->what == IT_CHARACTER && it->c == '\n';
      set_iterator_to_next (it, 0);
    }

  /* If we didn't find a newline near enough, see if we can use a
     short-cut.  */
  if (n == MAX_NEWLINE_DISTANCE)
    {
      int start = IT_CHARPOS (*it);
      int limit = find_next_newline_no_quit (start, 1);
      Lisp_Object pos;

      xassert (!STRINGP (it->string));

      /* If there isn't any `display' property in sight, and no
	 overlays, we can just use the position of the newline in
	 buffer text.  */
      if (it->stop_charpos >= limit
	  || ((pos = Fnext_single_property_change (make_number (start),
						   Qdisplay,
						   Qnil, make_number (limit)),
	       NILP (pos))
	      && next_overlay_change (start) == ZV))
	{
	  IT_CHARPOS (*it) = limit;
	  IT_BYTEPOS (*it) = CHAR_TO_BYTE (limit);
	  *skipped_p = newline_found_p = 1;
	}
      else
	{
	  while (get_next_display_element (it)
		 && !newline_found_p)
	    {
	      newline_found_p = ITERATOR_AT_END_OF_LINE_P (it);
	      set_iterator_to_next (it, 0);
	    }
	}
    }

  it->selective = old_selective;
  return newline_found_p;
}


/* Set IT's current position to the previous visible line start.  Skip
   invisible text that is so either due to text properties or due to
   selective display.  Caution: this does not change IT->current_x and
   IT->hpos.  */

static void
back_to_previous_visible_line_start (it)
     struct it *it;
{
  int visible_p = 0;

  /* Go back one newline if not on BEGV already.  */
  if (IT_CHARPOS (*it) > BEGV)
    back_to_previous_line_start (it);

  /* Move over lines that are invisible because of selective display
     or text properties.  */
  while (IT_CHARPOS (*it) > BEGV
	 && !visible_p)
    {
      visible_p = 1;

      /* If selective > 0, then lines indented more than that values
	 are invisible.  */
      if (it->selective > 0
	  && indented_beyond_p (IT_CHARPOS (*it), IT_BYTEPOS (*it),
				it->selective))
	visible_p = 0;
      else 
	{
	  Lisp_Object prop;

	  prop = Fget_char_property (make_number (IT_CHARPOS (*it)),
				     Qinvisible, it->window);
	  if (TEXT_PROP_MEANS_INVISIBLE (prop))
	    visible_p = 0;
	}

      /* Back one more newline if the current one is invisible.  */
      if (!visible_p)
	back_to_previous_line_start (it);
    }

  xassert (IT_CHARPOS (*it) >= BEGV);
  xassert (IT_CHARPOS (*it) == BEGV
	   || FETCH_BYTE (IT_BYTEPOS (*it) - 1) == '\n');
  CHECK_IT (it);
}


/* Reseat iterator IT at the previous visible line start.  Skip
   invisible text that is so either due to text properties or due to
   selective display.  At the end, update IT's overlay information,
   face information etc.  */

static void
reseat_at_previous_visible_line_start (it)
     struct it *it;
{
  back_to_previous_visible_line_start (it);
  reseat (it, it->current.pos, 1);
  CHECK_IT (it);
}


/* Reseat iterator IT on the next visible line start in the current
   buffer.  ON_NEWLINE_P non-zero means position IT on the newline
   preceding the line start.  Skip over invisible text that is so
   because of selective display.  Compute faces, overlays etc at the
   new position.  Note that this function does not skip over text that
   is invisible because of text properties.  */

static void
reseat_at_next_visible_line_start (it, on_newline_p)
     struct it *it;
     int on_newline_p;
{
  int newline_found_p, skipped_p = 0;

  newline_found_p = forward_to_next_line_start (it, &skipped_p);

  /* Skip over lines that are invisible because they are indented
     more than the value of IT->selective.  */
  if (it->selective > 0)
    while (IT_CHARPOS (*it) < ZV
	   && indented_beyond_p (IT_CHARPOS (*it), IT_BYTEPOS (*it),
				 it->selective))
      {
	xassert (FETCH_BYTE (IT_BYTEPOS (*it) - 1) == '\n');
	newline_found_p = forward_to_next_line_start (it, &skipped_p);
      }

  /* Position on the newline if that's what's requested.  */
  if (on_newline_p && newline_found_p)
    {
      if (STRINGP (it->string))
	{
	  if (IT_STRING_CHARPOS (*it) > 0)
	    {
	      --IT_STRING_CHARPOS (*it);
	      --IT_STRING_BYTEPOS (*it);
	    }
	}
      else if (IT_CHARPOS (*it) > BEGV)
	{
	  --IT_CHARPOS (*it);
	  --IT_BYTEPOS (*it);
	  reseat (it, it->current.pos, 0);
	}
    }
  else if (skipped_p)
    reseat (it, it->current.pos, 0);
  
  CHECK_IT (it);
}



/***********************************************************************
		   Changing an iterator's position
***********************************************************************/

/* Change IT's current position to POS in current_buffer.  If FORCE_P
   is non-zero, always check for text properties at the new position.
   Otherwise, text properties are only looked up if POS >=
   IT->check_charpos of a property.  */

static void
reseat (it, pos, force_p)
     struct it *it;
     struct text_pos pos;
     int force_p;
{
  int original_pos = IT_CHARPOS (*it);

  reseat_1 (it, pos, 0);

  /* Determine where to check text properties.  Avoid doing it
     where possible because text property lookup is very expensive.  */
  if (force_p
      || CHARPOS (pos) > it->stop_charpos
      || CHARPOS (pos) < original_pos)
    handle_stop (it);

  CHECK_IT (it);
}


/* Change IT's buffer position to POS.  SET_STOP_P non-zero means set
   IT->stop_pos to POS, also.  */

static void
reseat_1 (it, pos, set_stop_p)
     struct it *it;
     struct text_pos pos;
     int set_stop_p;
{
  /* Don't call this function when scanning a C string.  */
  xassert (it->s == NULL);

  /* POS must be a reasonable value.  */
  xassert (CHARPOS (pos) >= BEGV && CHARPOS (pos) <= ZV);

  it->current.pos = it->position = pos;
  XSETBUFFER (it->object, current_buffer);
  it->end_charpos = ZV;
  it->dpvec = NULL;
  it->current.dpvec_index = -1;
  it->current.overlay_string_index = -1;
  IT_STRING_CHARPOS (*it) = -1;
  IT_STRING_BYTEPOS (*it) = -1;
  it->string = Qnil;
  it->method = next_element_from_buffer;
  it->multibyte_p = !NILP (current_buffer->enable_multibyte_characters);
  it->sp = 0;
  it->face_before_selective_p = 0;

  if (set_stop_p)
    it->stop_charpos = CHARPOS (pos);
}


/* Set up IT for displaying a string, starting at CHARPOS in window W.
   If S is non-null, it is a C string to iterate over.  Otherwise,
   STRING gives a Lisp string to iterate over.
   
   If PRECISION > 0, don't return more then PRECISION number of
   characters from the string.

   If FIELD_WIDTH > 0, return padding spaces until FIELD_WIDTH
   characters have been returned.  FIELD_WIDTH < 0 means an infinite
   field width.

   MULTIBYTE = 0 means disable processing of multibyte characters,
   MULTIBYTE > 0 means enable it,
   MULTIBYTE < 0 means use IT->multibyte_p.

   IT must be initialized via a prior call to init_iterator before
   calling this function.  */

static void
reseat_to_string (it, s, string, charpos, precision, field_width, multibyte)
     struct it *it;
     unsigned char *s;
     Lisp_Object string;
     int charpos;
     int precision, field_width, multibyte;
{
  /* No region in strings.  */
  it->region_beg_charpos = it->region_end_charpos = -1;

  /* No text property checks performed by default, but see below.  */
  it->stop_charpos = -1;

  /* Set iterator position and end position.  */
  bzero (&it->current, sizeof it->current);
  it->current.overlay_string_index = -1;
  it->current.dpvec_index = -1;
  xassert (charpos >= 0);
  
  /* If STRING is specified, use its multibyteness, otherwise use the
     setting of MULTIBYTE, if specified.  */
  if (multibyte >= 0)
    it->multibyte_p = multibyte > 0;
  
  if (s == NULL)
    {
      xassert (STRINGP (string));
      it->string = string;
      it->s = NULL;
      it->end_charpos = it->string_nchars = SCHARS (string);
      it->method = next_element_from_string;
      it->current.string_pos = string_pos (charpos, string);
    }
  else
    {
      it->s = s;
      it->string = Qnil;

      /* Note that we use IT->current.pos, not it->current.string_pos,
	 for displaying C strings.  */
      IT_STRING_CHARPOS (*it) = IT_STRING_BYTEPOS (*it) = -1;
      if (it->multibyte_p)
	{
	  it->current.pos = c_string_pos (charpos, s, 1);
	  it->end_charpos = it->string_nchars = number_of_chars (s, 1);
	}
      else
	{
	  IT_CHARPOS (*it) = IT_BYTEPOS (*it) = charpos;
	  it->end_charpos = it->string_nchars = strlen (s);
	}
      
      it->method = next_element_from_c_string;
    }

  /* PRECISION > 0 means don't return more than PRECISION characters
     from the string.  */
  if (precision > 0 && it->end_charpos - charpos > precision)
    it->end_charpos = it->string_nchars = charpos + precision;

  /* FIELD_WIDTH > 0 means pad with spaces until FIELD_WIDTH
     characters have been returned.  FIELD_WIDTH == 0 means don't pad,
     FIELD_WIDTH < 0 means infinite field width.  This is useful for
     padding with `-' at the end of a mode line.  */
  if (field_width < 0)
    field_width = INFINITY;
  if (field_width > it->end_charpos - charpos)
    it->end_charpos = charpos + field_width;

  /* Use the standard display table for displaying strings.  */
  if (DISP_TABLE_P (Vstandard_display_table))
    it->dp = XCHAR_TABLE (Vstandard_display_table);

  it->stop_charpos = charpos;
  CHECK_IT (it);
}



/***********************************************************************
			      Iteration
 ***********************************************************************/

/* Load IT's display element fields with information about the next
   display element from the current position of IT.  Value is zero if
   end of buffer (or C string) is reached.  */

int
get_next_display_element (it)
     struct it *it;
{
  /* Non-zero means that we found an display element.  Zero means that
     we hit the end of what we iterate over.  Performance note: the
     function pointer `method' used here turns out to be faster than
     using a sequence of if-statements.  */
  int success_p = (*it->method) (it);

  if (it->what == IT_CHARACTER)
    {
      /* Map via display table or translate control characters.
	 IT->c, IT->len etc. have been set to the next character by
	 the function call above.  If we have a display table, and it
	 contains an entry for IT->c, translate it.  Don't do this if
	 IT->c itself comes from a display table, otherwise we could
	 end up in an infinite recursion.  (An alternative could be to
	 count the recursion depth of this function and signal an
	 error when a certain maximum depth is reached.)  Is it worth
	 it?  */
      if (success_p && it->dpvec == NULL)
	{
	  Lisp_Object dv;

	  if (it->dp
	      && (dv = DISP_CHAR_VECTOR (it->dp, it->c),
		  VECTORP (dv)))
	    {
	      struct Lisp_Vector *v = XVECTOR (dv);

	      /* Return the first character from the display table
		 entry, if not empty.  If empty, don't display the
		 current character.  */
	      if (v->size)
		{
		  it->dpvec_char_len = it->len;
		  it->dpvec = v->contents;
		  it->dpend = v->contents + v->size;
		  it->current.dpvec_index = 0;
		  it->method = next_element_from_display_vector;
		  success_p = get_next_display_element (it);
		}
	      else
		{
		  set_iterator_to_next (it, 0);
		  success_p = get_next_display_element (it);
		}
	    }

	  /* Translate control characters into `\003' or `^C' form.
	     Control characters coming from a display table entry are
	     currently not translated because we use IT->dpvec to hold
	     the translation.  This could easily be changed but I
	     don't believe that it is worth doing.

	     Non-printable multibyte characters are also translated
	     octal form.  */
	  else if ((it->c < ' '
		    && (it->area != TEXT_AREA
			|| (it->c != '\n' && it->c != '\t')))
		   || (it->c >= 127
		       && it->len == 1)
		   || !CHAR_PRINTABLE_P (it->c))
	    {
	      /* IT->c is a control character which must be displayed
		 either as '\003' or as `^C' where the '\\' and '^'
		 can be defined in the display table.  Fill
		 IT->ctl_chars with glyphs for what we have to
		 display.  Then, set IT->dpvec to these glyphs.  */
	      GLYPH g;

	      if (it->c < 128 && it->ctl_arrow_p)
		{
		  /* Set IT->ctl_chars[0] to the glyph for `^'.  */
		  if (it->dp
		      && INTEGERP (DISP_CTRL_GLYPH (it->dp))
		      && GLYPH_CHAR_VALID_P (XINT (DISP_CTRL_GLYPH (it->dp))))
		    g = XINT (DISP_CTRL_GLYPH (it->dp));
		  else
		    g = FAST_MAKE_GLYPH ('^', 0);
		  XSETINT (it->ctl_chars[0], g);

		  g = FAST_MAKE_GLYPH (it->c ^ 0100, 0);
		  XSETINT (it->ctl_chars[1], g);

		  /* Set up IT->dpvec and return first character from it.  */
		  it->dpvec_char_len = it->len;
		  it->dpvec = it->ctl_chars;
		  it->dpend = it->dpvec + 2;
		  it->current.dpvec_index = 0;
		  it->method = next_element_from_display_vector;
		  get_next_display_element (it);
		}
	      else
		{
		  unsigned char str[MAX_MULTIBYTE_LENGTH];
		  int len;
		  int i;
		  GLYPH escape_glyph;

		  /* Set IT->ctl_chars[0] to the glyph for `\\'.  */
		  if (it->dp
		      && INTEGERP (DISP_ESCAPE_GLYPH (it->dp))
		      && GLYPH_CHAR_VALID_P (XFASTINT (DISP_ESCAPE_GLYPH (it->dp))))
		    escape_glyph = XFASTINT (DISP_ESCAPE_GLYPH (it->dp));
		  else
		    escape_glyph = FAST_MAKE_GLYPH ('\\', 0);

		  if (SINGLE_BYTE_CHAR_P (it->c))
		    str[0] = it->c, len = 1;
		  else
		    {
		      len = CHAR_STRING_NO_SIGNAL (it->c, str);
		      if (len < 0)
			{
			  /* It's an invalid character, which
			     shouldn't happen actually, but due to
			     bugs it may happen.  Let's print the char
			     as is, there's not much meaningful we can
			     do with it.  */
			  str[0] = it->c;
			  str[1] = it->c >> 8;
			  str[2] = it->c >> 16;
			  str[3] = it->c >> 24;
			  len = 4;
			}
		    }

		  for (i = 0; i < len; i++)
		    {
		      XSETINT (it->ctl_chars[i * 4], escape_glyph);
		      /* Insert three more glyphs into IT->ctl_chars for
			 the octal display of the character.  */
		      g = FAST_MAKE_GLYPH (((str[i] >> 6) & 7) + '0', 0); 
		      XSETINT (it->ctl_chars[i * 4 + 1], g);
		      g = FAST_MAKE_GLYPH (((str[i] >> 3) & 7) + '0', 0); 
		      XSETINT (it->ctl_chars[i * 4 + 2], g);
		      g = FAST_MAKE_GLYPH ((str[i] & 7) + '0', 0); 
		      XSETINT (it->ctl_chars[i * 4 + 3], g);
		    }

		  /* Set up IT->dpvec and return the first character
                     from it.  */
		  it->dpvec_char_len = it->len;
		  it->dpvec = it->ctl_chars;
		  it->dpend = it->dpvec + len * 4;
		  it->current.dpvec_index = 0;
		  it->method = next_element_from_display_vector;
		  get_next_display_element (it);
		}
	    }
	}

      /* Adjust face id for a multibyte character.  There are no
         multibyte character in unibyte text.  */
      if (it->multibyte_p
	  && success_p
	  && FRAME_WINDOW_P (it->f))
	{
	  struct face *face = FACE_FROM_ID (it->f, it->face_id);
	  it->face_id = FACE_FOR_CHAR (it->f, face, it->c);
	}
    }

  /* Is this character the last one of a run of characters with
     box?  If yes, set IT->end_of_box_run_p to 1.  */
  if (it->face_box_p
      && it->s == NULL)
    {
      int face_id;
      struct face *face;

      it->end_of_box_run_p
	= ((face_id = face_after_it_pos (it),
	    face_id != it->face_id)
	   && (face = FACE_FROM_ID (it->f, face_id),
	       face->box == FACE_NO_BOX));
    }

  /* Value is 0 if end of buffer or string reached.  */
  return success_p;
}


/* Move IT to the next display element.

   RESEAT_P non-zero means if called on a newline in buffer text,
   skip to the next visible line start.

   Functions get_next_display_element and set_iterator_to_next are
   separate because I find this arrangement easier to handle than a
   get_next_display_element function that also increments IT's
   position.  The way it is we can first look at an iterator's current
   display element, decide whether it fits on a line, and if it does,
   increment the iterator position.  The other way around we probably
   would either need a flag indicating whether the iterator has to be
   incremented the next time, or we would have to implement a
   decrement position function which would not be easy to write.  */

void
set_iterator_to_next (it, reseat_p)
     struct it *it;
     int reseat_p;
{
  /* Reset flags indicating start and end of a sequence of characters
     with box.  Reset them at the start of this function because
     moving the iterator to a new position might set them.  */
  it->start_of_box_run_p = it->end_of_box_run_p = 0;
  
  if (it->method == next_element_from_buffer)
    {
      /* The current display element of IT is a character from
	 current_buffer.  Advance in the buffer, and maybe skip over
	 invisible lines that are so because of selective display.  */
      if (ITERATOR_AT_END_OF_LINE_P (it) && reseat_p)
	reseat_at_next_visible_line_start (it, 0);
      else
	{
	  xassert (it->len != 0);
	  IT_BYTEPOS (*it) += it->len;
	  IT_CHARPOS (*it) += 1;
	  xassert (IT_BYTEPOS (*it) == CHAR_TO_BYTE (IT_CHARPOS (*it)));
	}
    }
  else if (it->method == next_element_from_composition)
    {
      xassert (it->cmp_id >= 0 && it ->cmp_id < n_compositions);
      if (STRINGP (it->string))
	{
	  IT_STRING_BYTEPOS (*it) += it->len;
	  IT_STRING_CHARPOS (*it) += it->cmp_len;
	  it->method = next_element_from_string;
	  goto consider_string_end;
	}
      else
	{
	  IT_BYTEPOS (*it) += it->len;
	  IT_CHARPOS (*it) += it->cmp_len;
	  it->method = next_element_from_buffer;
	}
    }
  else if (it->method == next_element_from_c_string)
    {
      /* Current display element of IT is from a C string.  */
      IT_BYTEPOS (*it) += it->len;
      IT_CHARPOS (*it) += 1;
    }
  else if (it->method == next_element_from_display_vector)
    {
      /* Current display element of IT is from a display table entry.
	 Advance in the display table definition.  Reset it to null if
	 end reached, and continue with characters from buffers/
	 strings.  */
      ++it->current.dpvec_index;

      /* Restore face of the iterator to what they were before the
         display vector entry (these entries may contain faces).  */
      it->face_id = it->saved_face_id;
      
      if (it->dpvec + it->current.dpvec_index == it->dpend)
	{
	  if (it->s)
	    it->method = next_element_from_c_string;
	  else if (STRINGP (it->string))
	    it->method = next_element_from_string;
	  else
	    it->method = next_element_from_buffer;

	  it->dpvec = NULL;
	  it->current.dpvec_index = -1;

	  /* Skip over characters which were displayed via IT->dpvec.  */
	  if (it->dpvec_char_len < 0)
	    reseat_at_next_visible_line_start (it, 1);
	  else if (it->dpvec_char_len > 0)
	    {
	      it->len = it->dpvec_char_len;
	      set_iterator_to_next (it, reseat_p);
	    }
	}
    }
  else if (it->method == next_element_from_string)
    {
      /* Current display element is a character from a Lisp string.  */
      xassert (it->s == NULL && STRINGP (it->string));
      IT_STRING_BYTEPOS (*it) += it->len;
      IT_STRING_CHARPOS (*it) += 1;
      
    consider_string_end:

      if (it->current.overlay_string_index >= 0)
	{
	  /* IT->string is an overlay string.  Advance to the
	     next, if there is one.  */
	  if (IT_STRING_CHARPOS (*it) >= SCHARS (it->string))
	    next_overlay_string (it);
	}
      else
	{
	  /* IT->string is not an overlay string.  If we reached
	     its end, and there is something on IT->stack, proceed
	     with what is on the stack.  This can be either another
	     string, this time an overlay string, or a buffer.  */
	  if (IT_STRING_CHARPOS (*it) == SCHARS (it->string)
	      && it->sp > 0)
	    {
	      pop_it (it);
	      if (!STRINGP (it->string))
		it->method = next_element_from_buffer;
	      else
		goto consider_string_end;
	    }
	}
    }
  else if (it->method == next_element_from_image
	   || it->method == next_element_from_stretch)
    {
      /* The position etc with which we have to proceed are on
	 the stack.  The position may be at the end of a string,
         if the `display' property takes up the whole string.  */
      pop_it (it);
      it->image_id = 0;
      if (STRINGP (it->string))
	{
	  it->method = next_element_from_string;
	  goto consider_string_end;
	}
      else
	it->method = next_element_from_buffer;
    }
  else
    /* There are no other methods defined, so this should be a bug.  */
    abort ();

  xassert (it->method != next_element_from_string
	   || (STRINGP (it->string)
	       && IT_STRING_CHARPOS (*it) >= 0));
}


/* Load IT's display element fields with information about the next
   display element which comes from a display table entry or from the
   result of translating a control character to one of the forms `^C'
   or `\003'.  IT->dpvec holds the glyphs to return as characters.  */

static int
next_element_from_display_vector (it)
     struct it *it;
{
  /* Precondition.  */
  xassert (it->dpvec && it->current.dpvec_index >= 0);

  /* Remember the current face id in case glyphs specify faces.
     IT's face is restored in set_iterator_to_next.  */
  it->saved_face_id = it->face_id;
  
  if (INTEGERP (*it->dpvec)
      && GLYPH_CHAR_VALID_P (XFASTINT (*it->dpvec)))
    {
      int lface_id;
      GLYPH g;

      g = XFASTINT (it->dpvec[it->current.dpvec_index]);
      it->c = FAST_GLYPH_CHAR (g);
      it->len = CHAR_BYTES (it->c);

      /* The entry may contain a face id to use.  Such a face id is
	 the id of a Lisp face, not a realized face.  A face id of
	 zero means no face is specified.  */
      lface_id = FAST_GLYPH_FACE (g);
      if (lface_id)
	{
	  /* The function returns -1 if lface_id is invalid.  */
	  int face_id = ascii_face_of_lisp_face (it->f, lface_id);
	  if (face_id >= 0)
	    it->face_id = face_id;
	}
    }
  else
    /* Display table entry is invalid.  Return a space.  */
    it->c = ' ', it->len = 1;

  /* Don't change position and object of the iterator here.  They are
     still the values of the character that had this display table
     entry or was translated, and that's what we want.  */
  it->what = IT_CHARACTER;
  return 1;
}


/* Load IT with the next display element from Lisp string IT->string.
   IT->current.string_pos is the current position within the string.
   If IT->current.overlay_string_index >= 0, the Lisp string is an
   overlay string.  */

static int
next_element_from_string (it)
     struct it *it;
{
  struct text_pos position;

  xassert (STRINGP (it->string));
  xassert (IT_STRING_CHARPOS (*it) >= 0);
  position = it->current.string_pos;

  /* Time to check for invisible text?  */
  if (IT_STRING_CHARPOS (*it) < it->end_charpos
      && IT_STRING_CHARPOS (*it) == it->stop_charpos)
    {
      handle_stop (it);

      /* Since a handler may have changed IT->method, we must
	 recurse here.  */
      return get_next_display_element (it);
    }

  if (it->current.overlay_string_index >= 0)
    {
      /* Get the next character from an overlay string.  In overlay
	 strings, There is no field width or padding with spaces to
	 do.  */
      if (IT_STRING_CHARPOS (*it) >= SCHARS (it->string))
	{
	  it->what = IT_EOB;
	  return 0;
	}
      else if (STRING_MULTIBYTE (it->string))
	{
	  int remaining = SBYTES (it->string) - IT_STRING_BYTEPOS (*it);
	  unsigned char *s = SDATA (it->string) + IT_STRING_BYTEPOS (*it);
	  it->c = string_char_and_length (s, remaining, &it->len);
	}
      else
	{
	  it->c = SREF (it->string, IT_STRING_BYTEPOS (*it));
	  it->len = 1;
	}
    }
  else
    {
      /* Get the next character from a Lisp string that is not an
	 overlay string.  Such strings come from the mode line, for
	 example.  We may have to pad with spaces, or truncate the
	 string.  See also next_element_from_c_string.  */
      if (IT_STRING_CHARPOS (*it) >= it->end_charpos)
	{
	  it->what = IT_EOB;
	  return 0;
	}
      else if (IT_STRING_CHARPOS (*it) >= it->string_nchars)
	{
	  /* Pad with spaces.  */
	  it->c = ' ', it->len = 1;
	  CHARPOS (position) = BYTEPOS (position) = -1;
	}
      else if (STRING_MULTIBYTE (it->string))
	{
	  int maxlen = SBYTES (it->string) - IT_STRING_BYTEPOS (*it);
	  unsigned char *s = SDATA (it->string) + IT_STRING_BYTEPOS (*it);
	  it->c = string_char_and_length (s, maxlen, &it->len);
	}
      else
	{
	  it->c = SREF (it->string, IT_STRING_BYTEPOS (*it));
	  it->len = 1;
	}
    }

  /* Record what we have and where it came from.  Note that we store a
     buffer position in IT->position although it could arguably be a
     string position.  */
  it->what = IT_CHARACTER;
  it->object = it->string;
  it->position = position;
  return 1;
}


/* Load IT with next display element from C string IT->s.
   IT->string_nchars is the maximum number of characters to return
   from the string.  IT->end_charpos may be greater than
   IT->string_nchars when this function is called, in which case we
   may have to return padding spaces.  Value is zero if end of string
   reached, including padding spaces.  */

static int
next_element_from_c_string (it)
     struct it *it;
{
  int success_p = 1;
  
  xassert (it->s);
  it->what = IT_CHARACTER;
  BYTEPOS (it->position) = CHARPOS (it->position) = 0;
  it->object = Qnil;
  
  /* IT's position can be greater IT->string_nchars in case a field
     width or precision has been specified when the iterator was
     initialized.  */
  if (IT_CHARPOS (*it) >= it->end_charpos)
    {
      /* End of the game.  */
      it->what = IT_EOB;
      success_p = 0;
    }
  else if (IT_CHARPOS (*it) >= it->string_nchars)
    {
      /* Pad with spaces.  */
      it->c = ' ', it->len = 1;
      BYTEPOS (it->position) = CHARPOS (it->position) = -1;
    }
  else if (it->multibyte_p)
    {
      /* Implementation note: The calls to strlen apparently aren't a
	 performance problem because there is no noticeable performance
	 difference between Emacs running in unibyte or multibyte mode.  */
      int maxlen = strlen (it->s) - IT_BYTEPOS (*it);
      it->c = string_char_and_length (it->s + IT_BYTEPOS (*it),
				      maxlen, &it->len);
    }
  else
    it->c = it->s[IT_BYTEPOS (*it)], it->len = 1;
  
  return success_p;
}


/* Set up IT to return characters from an ellipsis, if appropriate.
   The definition of the ellipsis glyphs may come from a display table
   entry.  This function Fills IT with the first glyph from the
   ellipsis if an ellipsis is to be displayed.  */

static int
next_element_from_ellipsis (it)
     struct it *it;
{
  if (it->selective_display_ellipsis_p)
    {
      if (it->dp && VECTORP (DISP_INVIS_VECTOR (it->dp)))
	{
	  /* Use the display table definition for `...'.  Invalid glyphs
	     will be handled by the method returning elements from dpvec.  */
	  struct Lisp_Vector *v = XVECTOR (DISP_INVIS_VECTOR (it->dp));
	  it->dpvec_char_len = it->len;
	  it->dpvec = v->contents;
	  it->dpend = v->contents + v->size;
	  it->current.dpvec_index = 0;
	  it->method = next_element_from_display_vector;
	}
      else
	{
	  /* Use default `...' which is stored in default_invis_vector.  */
	  it->dpvec_char_len = it->len;
	  it->dpvec = default_invis_vector;
	  it->dpend = default_invis_vector + 3;
	  it->current.dpvec_index = 0;
	  it->method = next_element_from_display_vector;
	}
    }
  else
    {
      /* The face at the current position may be different from the
	 face we find after the invisible text.  Remember what it
	 was in IT->saved_face_id, and signal that it's there by
	 setting face_before_selective_p.  */
      it->saved_face_id = it->face_id;
      it->method = next_element_from_buffer;
      reseat_at_next_visible_line_start (it, 1);
      it->face_before_selective_p = 1;
    }
  
  return get_next_display_element (it);
}


/* Deliver an image display element.  The iterator IT is already
   filled with image information (done in handle_display_prop).  Value
   is always 1.  */
  

static int
next_element_from_image (it)
     struct it *it;
{
  it->what = IT_IMAGE;
  return 1;
}


/* Fill iterator IT with next display element from a stretch glyph
   property.  IT->object is the value of the text property.  Value is
   always 1.  */

static int
next_element_from_stretch (it)
     struct it *it;
{
  it->what = IT_STRETCH;
  return 1;
}


/* Load IT with the next display element from current_buffer.  Value
   is zero if end of buffer reached.  IT->stop_charpos is the next
   position at which to stop and check for text properties or buffer
   end.  */

static int
next_element_from_buffer (it)
     struct it *it;
{
  int success_p = 1;

  /* Check this assumption, otherwise, we would never enter the
     if-statement, below.  */
  xassert (IT_CHARPOS (*it) >= BEGV
	   && IT_CHARPOS (*it) <= it->stop_charpos);

  if (IT_CHARPOS (*it) >= it->stop_charpos)
    {
      if (IT_CHARPOS (*it) >= it->end_charpos)
	{
	  int overlay_strings_follow_p;
	  
	  /* End of the game, except when overlay strings follow that
	     haven't been returned yet.  */
	  if (it->overlay_strings_at_end_processed_p)
	    overlay_strings_follow_p = 0;
	  else
	    {
	      it->overlay_strings_at_end_processed_p = 1;
	      overlay_strings_follow_p = get_overlay_strings (it, 0);
	    }

	  if (overlay_strings_follow_p)
	    success_p = get_next_display_element (it);
	  else
	    {
	      it->what = IT_EOB;
	      it->position = it->current.pos;
	      success_p = 0;
	    }
	}
      else
	{
	  handle_stop (it);
	  return get_next_display_element (it);
	}
    }
  else
    {
      /* No face changes, overlays etc. in sight, so just return a
	 character from current_buffer.  */
      unsigned char *p;

      /* Maybe run the redisplay end trigger hook.  Performance note:
	 This doesn't seem to cost measurable time.  */
      if (it->redisplay_end_trigger_charpos
	  && it->glyph_row
	  && IT_CHARPOS (*it) >= it->redisplay_end_trigger_charpos)
	run_redisplay_end_trigger_hook (it);

      /* Get the next character, maybe multibyte.  */
      p = BYTE_POS_ADDR (IT_BYTEPOS (*it));
      if (it->multibyte_p && !ASCII_BYTE_P (*p))
	{
	  int maxlen = ((IT_BYTEPOS (*it) >= GPT_BYTE ? ZV_BYTE : GPT_BYTE)
			- IT_BYTEPOS (*it));
	  it->c = string_char_and_length (p, maxlen, &it->len);
	}
      else
	it->c = *p, it->len = 1;

      /* Record what we have and where it came from.  */
      it->what = IT_CHARACTER;;
      it->object = it->w->buffer;
      it->position = it->current.pos;

      /* Normally we return the character found above, except when we
	 really want to return an ellipsis for selective display.  */
      if (it->selective)
	{
	  if (it->c == '\n')
	    {
	      /* A value of selective > 0 means hide lines indented more
		 than that number of columns.  */
	      if (it->selective > 0
		  && IT_CHARPOS (*it) + 1 < ZV
		  && indented_beyond_p (IT_CHARPOS (*it) + 1,
					IT_BYTEPOS (*it) + 1,
					it->selective))
		{
		  success_p = next_element_from_ellipsis (it);
		  it->dpvec_char_len = -1;
		}
	    }
	  else if (it->c == '\r' && it->selective == -1)
	    {
	      /* A value of selective == -1 means that everything from the
		 CR to the end of the line is invisible, with maybe an
		 ellipsis displayed for it.  */
	      success_p = next_element_from_ellipsis (it);
	      it->dpvec_char_len = -1;
	    }
	}
    }

  /* Value is zero if end of buffer reached.  */
  xassert (!success_p || it->what != IT_CHARACTER || it->len > 0);
  return success_p;
}

     
/* Run the redisplay end trigger hook for IT.  */

static void
run_redisplay_end_trigger_hook (it)
     struct it *it;
{
  Lisp_Object args[3];

  /* IT->glyph_row should be non-null, i.e. we should be actually
     displaying something, or otherwise we should not run the hook.  */
  xassert (it->glyph_row);

  /* Set up hook arguments.  */
  args[0] = Qredisplay_end_trigger_functions;
  args[1] = it->window;
  XSETINT (args[2], it->redisplay_end_trigger_charpos);
  it->redisplay_end_trigger_charpos = 0;

  /* Since we are *trying* to run these functions, don't try to run
     them again, even if they get an error.  */
  it->w->redisplay_end_trigger = Qnil;
  Frun_hook_with_args (3, args);
  
  /* Notice if it changed the face of the character we are on.  */
  handle_face_prop (it);
}


/* Deliver a composition display element.  The iterator IT is already
   filled with composition information (done in
   handle_composition_prop).  Value is always 1.  */

static int
next_element_from_composition (it)
     struct it *it;
{
  it->what = IT_COMPOSITION;
  it->position = (STRINGP (it->string)
		  ? it->current.string_pos
		  : it->current.pos);
  return 1;
}



/***********************************************************************
	     Moving an iterator without producing glyphs
 ***********************************************************************/

/* Move iterator IT to a specified buffer or X position within one
   line on the display without producing glyphs.

   Begin to skip at IT's current position.  Skip to TO_CHARPOS or TO_X
   whichever is reached first.

   TO_CHARPOS <= 0 means no TO_CHARPOS is specified.

   TO_X < 0 means that no TO_X is specified.  TO_X is normally a value
   0 <= TO_X <= IT->last_visible_x.  This means in particular, that
   TO_X includes the amount by which a window is horizontally
   scrolled.

   Value is

   MOVE_POS_MATCH_OR_ZV
     - when TO_POS or ZV was reached.
	
   MOVE_X_REACHED
     -when TO_X was reached before TO_POS or ZV were reached.
	
   MOVE_LINE_CONTINUED
     - when we reached the end of the display area and the line must
     be continued.
			   
   MOVE_LINE_TRUNCATED
     - when we reached the end of the display area and the line is
     truncated.

   MOVE_NEWLINE_OR_CR
     - when we stopped at a line end, i.e. a newline or a CR and selective
     display is on.  */

static enum move_it_result
move_it_in_display_line_to (it, to_charpos, to_x, op)
     struct it *it;
     int to_charpos, to_x, op;
{
  enum move_it_result result = MOVE_UNDEFINED;
  struct glyph_row *saved_glyph_row;

  /* Don't produce glyphs in produce_glyphs.  */
  saved_glyph_row = it->glyph_row;
  it->glyph_row = NULL;

  while (1)
    {
      int x, i, ascent = 0, descent = 0;
      
      /* Stop when ZV or TO_CHARPOS reached.  */
      if (!get_next_display_element (it)
	  || ((op & MOVE_TO_POS) != 0
	      && BUFFERP (it->object)
	      && IT_CHARPOS (*it) >= to_charpos))
	{
	  result = MOVE_POS_MATCH_OR_ZV;
	  break;
	}
	  
      /* The call to produce_glyphs will get the metrics of the
	 display element IT is loaded with.  We record in x the
	 x-position before this display element in case it does not
	 fit on the line.  */
      x = it->current_x;
      
      /* Remember the line height so far in case the next element doesn't
	 fit on the line.  */
      if (!it->truncate_lines_p)
	{
	  ascent = it->max_ascent;
	  descent = it->max_descent;
	}
      
      PRODUCE_GLYPHS (it);

      if (it->area != TEXT_AREA)
	{
	  set_iterator_to_next (it, 1);
	  continue;
	}

      /* The number of glyphs we get back in IT->nglyphs will normally
	 be 1 except when IT->c is (i) a TAB, or (ii) a multi-glyph
	 character on a terminal frame, or (iii) a line end.  For the
	 second case, IT->nglyphs - 1 padding glyphs will be present
	 (on X frames, there is only one glyph produced for a
	 composite character.

	 The behavior implemented below means, for continuation lines,
	 that as many spaces of a TAB as fit on the current line are
	 displayed there.  For terminal frames, as many glyphs of a
	 multi-glyph character are displayed in the current line, too.
	 This is what the old redisplay code did, and we keep it that
	 way.  Under X, the whole shape of a complex character must
	 fit on the line or it will be completely displayed in the
	 next line.

	 Note that both for tabs and padding glyphs, all glyphs have
	 the same width.   */
      if (it->nglyphs)
	{
	  /* More than one glyph or glyph doesn't fit on line.  All
	     glyphs have the same width.  */
	  int single_glyph_width = it->pixel_width / it->nglyphs;
	  int new_x;
	  
	  for (i = 0; i < it->nglyphs; ++i, x = new_x)
	    {
	      new_x = x + single_glyph_width;

	      /* We want to leave anything reaching TO_X to the caller.  */
	      if ((op & MOVE_TO_X) && new_x > to_x)
		{
		  it->current_x = x;
		  result = MOVE_X_REACHED;
		  break;
		}
	      else if (/* Lines are continued.  */
		       !it->truncate_lines_p
		       && (/* And glyph doesn't fit on the line.  */
			   new_x > it->last_visible_x
			   /* Or it fits exactly and we're on a window
			      system frame.  */
			   || (new_x == it->last_visible_x
			       && FRAME_WINDOW_P (it->f))))
		{
		  if (/* IT->hpos == 0 means the very first glyph
			 doesn't fit on the line, e.g. a wide image.  */
		      it->hpos == 0
		      || (new_x == it->last_visible_x
			  && FRAME_WINDOW_P (it->f)))
		    {
		      ++it->hpos;
		      it->current_x = new_x;
		      if (i == it->nglyphs - 1)
			set_iterator_to_next (it, 1);
		    }
		  else
		    {
		      it->current_x = x;
		      it->max_ascent = ascent;
		      it->max_descent = descent;
		    }
		  
		  TRACE_MOVE ((stderr, "move_it_in: continued at %d\n",
			       IT_CHARPOS (*it)));
		  result = MOVE_LINE_CONTINUED;
		  break;
		}
	      else if (new_x > it->first_visible_x)
		{
		  /* Glyph is visible.  Increment number of glyphs that
		     would be displayed.  */
		  ++it->hpos;
		}
	      else
		{
		  /* Glyph is completely off the left margin of the display 
		     area.  Nothing to do.  */
		}
	    }

	  if (result != MOVE_UNDEFINED)
	    break;
	}
      else if ((op & MOVE_TO_X) && it->current_x >= to_x)
	{
	  /* Stop when TO_X specified and reached.  This check is
	     necessary here because of lines consisting of a line end,
	     only.  The line end will not produce any glyphs and we
	     would never get MOVE_X_REACHED.  */
	  xassert (it->nglyphs == 0);
	  result = MOVE_X_REACHED;
	  break;
	}
  
      /* Is this a line end?  If yes, we're done.  */
      if (ITERATOR_AT_END_OF_LINE_P (it))
	{
	  result = MOVE_NEWLINE_OR_CR;
	  break;
	}
      
      /* The current display element has been consumed.  Advance
	 to the next.  */
      set_iterator_to_next (it, 1);
      
      /* Stop if lines are truncated and IT's current x-position is
	 past the right edge of the window now.  */
      if (it->truncate_lines_p
	  && it->current_x >= it->last_visible_x)
	{
	  result = MOVE_LINE_TRUNCATED;
	  break;
	}
    }

  /* Restore the iterator settings altered at the beginning of this
     function.  */
  it->glyph_row = saved_glyph_row;
  return result;
}


/* Move IT forward to a specified buffer position TO_CHARPOS, TO_X,
   TO_Y, TO_VPOS.  OP is a bit-mask that specifies where to stop.  See
   the description of enum move_operation_enum.
   
   If TO_CHARPOS is in invisible text, e.g. a truncated part of a
   screen line, this function will set IT to the next position >
   TO_CHARPOS.  */

void
move_it_to (it, to_charpos, to_x, to_y, to_vpos, op)
     struct it *it;
     int to_charpos, to_x, to_y, to_vpos;
     int op;
{
  enum move_it_result skip, skip2 = MOVE_X_REACHED;
  int line_height;
  int reached = 0;

  for (;;)
    {
      if (op & MOVE_TO_VPOS)
	{
	  /* If no TO_CHARPOS and no TO_X specified, stop at the
	     start of the line TO_VPOS.  */
	  if ((op & (MOVE_TO_X | MOVE_TO_POS)) == 0)
	    {
	      if (it->vpos == to_vpos)
		{
		  reached = 1;
		  break;
		}
	      else
		skip = move_it_in_display_line_to (it, -1, -1, 0);
	    }
	  else
	    {
	      /* TO_VPOS >= 0 means stop at TO_X in the line at
		 TO_VPOS, or at TO_POS, whichever comes first.  */
	      if (it->vpos == to_vpos)
		{
		  reached = 2;
		  break;
		}
	      
	      skip = move_it_in_display_line_to (it, to_charpos, to_x, op);

	      if (skip == MOVE_POS_MATCH_OR_ZV || it->vpos == to_vpos)
		{
		  reached = 3;
		  break;
		}
	      else if (skip == MOVE_X_REACHED && it->vpos != to_vpos)
		{
		  /* We have reached TO_X but not in the line we want.  */
		  skip = move_it_in_display_line_to (it, to_charpos,
						     -1, MOVE_TO_POS);
		  if (skip == MOVE_POS_MATCH_OR_ZV)
		    {
		      reached = 4;
		      break;
		    }
		}
	    }
	}
      else if (op & MOVE_TO_Y)
	{
	  struct it it_backup;
	  
	  /* TO_Y specified means stop at TO_X in the line containing
	     TO_Y---or at TO_CHARPOS if this is reached first.  The
	     problem is that we can't really tell whether the line
	     contains TO_Y before we have completely scanned it, and
	     this may skip past TO_X.  What we do is to first scan to
	     TO_X.

	     If TO_X is not specified, use a TO_X of zero.  The reason
	     is to make the outcome of this function more predictable.
	     If we didn't use TO_X == 0, we would stop at the end of
	     the line which is probably not what a caller would expect
	     to happen.  */
	  skip = move_it_in_display_line_to (it, to_charpos,
					     ((op & MOVE_TO_X)
					      ? to_x : 0),
					     (MOVE_TO_X
					      | (op & MOVE_TO_POS)));

	  /* If TO_CHARPOS is reached or ZV, we don't have to do more.  */
	  if (skip == MOVE_POS_MATCH_OR_ZV)
	    {
	      reached = 5;
	      break;
	    }
	  
	  /* If TO_X was reached, we would like to know whether TO_Y
	     is in the line.  This can only be said if we know the
	     total line height which requires us to scan the rest of
	     the line.  */
	  if (skip == MOVE_X_REACHED)
	    {
	      it_backup = *it;
	      TRACE_MOVE ((stderr, "move_it: from %d\n", IT_CHARPOS (*it)));
	      skip2 = move_it_in_display_line_to (it, to_charpos, -1,
						  op & MOVE_TO_POS);
	      TRACE_MOVE ((stderr, "move_it: to %d\n", IT_CHARPOS (*it)));
	    }

	  /* Now, decide whether TO_Y is in this line.  */
	  line_height = it->max_ascent + it->max_descent;
	  TRACE_MOVE ((stderr, "move_it: line_height = %d\n", line_height));
	  
	  if (to_y >= it->current_y
	      && to_y < it->current_y + line_height)
	    {
	      if (skip == MOVE_X_REACHED)
		/* If TO_Y is in this line and TO_X was reached above,
		   we scanned too far.  We have to restore IT's settings
		   to the ones before skipping.  */
		*it = it_backup;
	      reached = 6;
	    }
	  else if (skip == MOVE_X_REACHED)
	    {
	      skip = skip2;
	      if (skip == MOVE_POS_MATCH_OR_ZV)
		reached = 7;
	    }

	  if (reached)
	    break;
	}
      else
	skip = move_it_in_display_line_to (it, to_charpos, -1, MOVE_TO_POS);

      switch (skip)
	{
	case MOVE_POS_MATCH_OR_ZV:
	  reached = 8;
	  goto out;

	case MOVE_NEWLINE_OR_CR:
	  set_iterator_to_next (it, 1);
	  it->continuation_lines_width = 0;
	  break;

	case MOVE_LINE_TRUNCATED:
	  it->continuation_lines_width = 0;
	  reseat_at_next_visible_line_start (it, 0);
	  if ((op & MOVE_TO_POS) != 0
	      && IT_CHARPOS (*it) > to_charpos)
	    {
	      reached = 9;
	      goto out;
	    }
	  break;

	case MOVE_LINE_CONTINUED:
	  it->continuation_lines_width += it->current_x;
	  break;

	default:
	  abort ();
	}

      /* Reset/increment for the next run.  */
      recenter_overlay_lists (current_buffer, IT_CHARPOS (*it));
      it->current_x = it->hpos = 0;
      it->current_y += it->max_ascent + it->max_descent;
      ++it->vpos;
      last_height = it->max_ascent + it->max_descent;
      last_max_ascent = it->max_ascent;
      it->max_ascent = it->max_descent = 0;
    }
  
 out:

  TRACE_MOVE ((stderr, "move_it_to: reached %d\n", reached));
}


/* Move iterator IT backward by a specified y-distance DY, DY >= 0.

   If DY > 0, move IT backward at least that many pixels.  DY = 0
   means move IT backward to the preceding line start or BEGV.  This
   function may move over more than DY pixels if IT->current_y - DY
   ends up in the middle of a line; in this case IT->current_y will be
   set to the top of the line moved to.  */

void
move_it_vertically_backward (it, dy)
     struct it *it;
     int dy;
{
  int nlines, h;
  struct it it2, it3;
  int start_pos = IT_CHARPOS (*it);
  
  xassert (dy >= 0);

  /* Estimate how many newlines we must move back.  */
  nlines = max (1, dy / CANON_Y_UNIT (it->f));

  /* Set the iterator's position that many lines back.  */
  while (nlines-- && IT_CHARPOS (*it) > BEGV)
    back_to_previous_visible_line_start (it);

  /* Reseat the iterator here.  When moving backward, we don't want
     reseat to skip forward over invisible text, set up the iterator
     to deliver from overlay strings at the new position etc.  So,
     use reseat_1 here.  */
  reseat_1 (it, it->current.pos, 1);

  /* We are now surely at a line start.  */
  it->current_x = it->hpos = 0;

  /* Move forward and see what y-distance we moved.  First move to the
     start of the next line so that we get its height.  We need this
     height to be able to tell whether we reached the specified
     y-distance.  */
  it2 = *it;
  it2.max_ascent = it2.max_descent = 0;
  move_it_to (&it2, start_pos, -1, -1, it2.vpos + 1,
	      MOVE_TO_POS | MOVE_TO_VPOS);
  xassert (IT_CHARPOS (*it) >= BEGV);
  it3 = it2;
  
  move_it_to (&it2, start_pos, -1, -1, -1, MOVE_TO_POS);
  xassert (IT_CHARPOS (*it) >= BEGV);
  h = it2.current_y - it->current_y;
  nlines = it2.vpos - it->vpos;

  /* Correct IT's y and vpos position.  */
  it->vpos -= nlines;
  it->current_y -= h;
  
  if (dy == 0)
    {
      /* DY == 0 means move to the start of the screen line.  The
	 value of nlines is > 0 if continuation lines were involved.  */
      if (nlines > 0)
	move_it_by_lines (it, nlines, 1);
      xassert (IT_CHARPOS (*it) <= start_pos);
    }
  else if (nlines)
    {
      /* The y-position we try to reach.  Note that h has been
         subtracted in front of the if-statement.  */
      int target_y = it->current_y + h - dy;
      int y0 = it3.current_y;
      int y1 = line_bottom_y (&it3);
      int line_height = y1 - y0;

      /* If we did not reach target_y, try to move further backward if
	 we can.  If we moved too far backward, try to move forward.  */
      if (target_y < it->current_y
	  /* This is heuristic.  In a window that's 3 lines high, with
	     a line height of 13 pixels each, recentering with point
	     on the bottom line will try to move -39/2 = 19 pixels
	     backward.  Try to avoid moving into the first line.  */
	  && it->current_y - target_y > line_height / 3 * 2
	  && IT_CHARPOS (*it) > BEGV)
	{
	  TRACE_MOVE ((stderr, "  not far enough -> move_vert %d\n",
		       target_y - it->current_y));
	  move_it_vertically (it, target_y - it->current_y);
	  xassert (IT_CHARPOS (*it) >= BEGV);
	}
      else if (target_y >= it->current_y + line_height
	       && IT_CHARPOS (*it) < ZV)
	{
	  /* Should move forward by at least one line, maybe more.
	     
	     Note: Calling move_it_by_lines can be expensive on
	     terminal frames, where compute_motion is used (via
	     vmotion) to do the job, when there are very long lines
	     and truncate-lines is nil.  That's the reason for
	     treating terminal frames specially here.  */
	  
	  if (!FRAME_WINDOW_P (it->f))
	    move_it_vertically (it, target_y - (it->current_y + line_height));
	  else
	    {
	      do
		{
		  move_it_by_lines (it, 1, 1);
		}
	      while (target_y >= line_bottom_y (it) && IT_CHARPOS (*it) < ZV);
	    }

	  xassert (IT_CHARPOS (*it) >= BEGV);
	}
    }
}


/* Move IT by a specified amount of pixel lines DY.  DY negative means
   move backwards.  DY = 0 means move to start of screen line.  At the
   end, IT will be on the start of a screen line.  */

void 
move_it_vertically (it, dy)
    struct it *it;
    int dy;
{
  if (dy <= 0)
    move_it_vertically_backward (it, -dy);
  else if (dy > 0)
    {
      TRACE_MOVE ((stderr, "move_it_v: from %d, %d\n", IT_CHARPOS (*it), dy));
      move_it_to (it, ZV, -1, it->current_y + dy, -1,
		  MOVE_TO_POS | MOVE_TO_Y);
      TRACE_MOVE ((stderr, "move_it_v: to %d\n", IT_CHARPOS (*it)));

      /* If buffer ends in ZV without a newline, move to the start of
	 the line to satisfy the post-condition.  */
      if (IT_CHARPOS (*it) == ZV
	  && FETCH_BYTE (IT_BYTEPOS (*it) - 1) != '\n')
	move_it_by_lines (it, 0, 0);
    }
}


/* Move iterator IT past the end of the text line it is in.  */

void
move_it_past_eol (it)
     struct it *it;
{
  enum move_it_result rc;
  
  rc = move_it_in_display_line_to (it, Z, 0, MOVE_TO_POS);
  if (rc == MOVE_NEWLINE_OR_CR)
    set_iterator_to_next (it, 0);
}


#if 0 /* Currently not used.  */

/* Return non-zero if some text between buffer positions START_CHARPOS
   and END_CHARPOS is invisible.  IT->window is the window for text
   property lookup.  */

static int
invisible_text_between_p (it, start_charpos, end_charpos)
     struct it *it;
     int start_charpos, end_charpos;
{
  Lisp_Object prop, limit;
  int invisible_found_p;
  
  xassert (it != NULL && start_charpos <= end_charpos);

  /* Is text at START invisible?  */
  prop = Fget_char_property (make_number (start_charpos), Qinvisible,
			     it->window);
  if (TEXT_PROP_MEANS_INVISIBLE (prop))
    invisible_found_p = 1;
  else
    {
      limit = Fnext_single_char_property_change (make_number (start_charpos),
						 Qinvisible, Qnil,
						 make_number (end_charpos));
      invisible_found_p = XFASTINT (limit) < end_charpos;
    }

  return invisible_found_p;
}

#endif /* 0 */


/* Move IT by a specified number DVPOS of screen lines down.  DVPOS
   negative means move up.  DVPOS == 0 means move to the start of the
   screen line.  NEED_Y_P non-zero means calculate IT->current_y.  If
   NEED_Y_P is zero, IT->current_y will be left unchanged.

   Further optimization ideas: If we would know that IT->f doesn't use
   a face with proportional font, we could be faster for
   truncate-lines nil.  */

void
move_it_by_lines (it, dvpos, need_y_p)
     struct it *it;
     int dvpos, need_y_p;
{
  struct position pos;
  
  if (!FRAME_WINDOW_P (it->f))
    {
      struct text_pos textpos;
      
      /* We can use vmotion on frames without proportional fonts.  */
      pos = *vmotion (IT_CHARPOS (*it), dvpos, it->w);
      SET_TEXT_POS (textpos, pos.bufpos, pos.bytepos);
      reseat (it, textpos, 1);
      it->vpos += pos.vpos;
      it->current_y += pos.vpos;
    }
  else if (dvpos == 0)
    {
      /* DVPOS == 0 means move to the start of the screen line.  */
      move_it_vertically_backward (it, 0);
      xassert (it->current_x == 0 && it->hpos == 0);
    }
  else if (dvpos > 0)
    move_it_to (it, -1, -1, -1, it->vpos + dvpos, MOVE_TO_VPOS);
  else
    {
      struct it it2;
      int start_charpos, i;
      
      /* Start at the beginning of the screen line containing IT's
	 position.  */
      move_it_vertically_backward (it, 0);
      
      /* Go back -DVPOS visible lines and reseat the iterator there.  */
      start_charpos = IT_CHARPOS (*it);
      for (i = -dvpos; i && IT_CHARPOS (*it) > BEGV; --i)
	back_to_previous_visible_line_start (it);
      reseat (it, it->current.pos, 1);
      it->current_x = it->hpos = 0;

      /* Above call may have moved too far if continuation lines
	 are involved.  Scan forward and see if it did.  */
      it2 = *it;
      it2.vpos = it2.current_y = 0;
      move_it_to (&it2, start_charpos, -1, -1, -1, MOVE_TO_POS);
      it->vpos -= it2.vpos;
      it->current_y -= it2.current_y;
      it->current_x = it->hpos = 0;

      /* If we moved too far, move IT some lines forward.  */
      if (it2.vpos > -dvpos)
	{
	  int delta = it2.vpos + dvpos;
	  move_it_to (it, -1, -1, -1, it->vpos + delta, MOVE_TO_VPOS);
	}
    }
}



/***********************************************************************
			       Messages
 ***********************************************************************/


/* Add a message with format string FORMAT and arguments ARG1 and ARG2
   to *Messages*.  */

void
add_to_log (format, arg1, arg2)
     char *format;
     Lisp_Object arg1, arg2;
{
  Lisp_Object args[3];
  Lisp_Object msg, fmt;
  char *buffer;
  int len;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  /* Do nothing if called asynchronously.  Inserting text into
     a buffer may call after-change-functions and alike and
     that would means running Lisp asynchronously.  */
  if (handling_signal)
    return;

  fmt = msg = Qnil;
  GCPRO4 (fmt, msg, arg1, arg2);
  
  args[0] = fmt = build_string (format);
  args[1] = arg1;
  args[2] = arg2;
  msg = Fformat (3, args);

  len = SBYTES (msg) + 1;
  buffer = (char *) alloca (len);
  bcopy (SDATA (msg), buffer, len);
  
  message_dolog (buffer, len - 1, 1, 0);
  UNGCPRO;
}


/* Output a newline in the *Messages* buffer if "needs" one.  */

void
message_log_maybe_newline ()
{
  if (message_log_need_newline)
    message_dolog ("", 0, 1, 0);
}


/* Add a string M of length NBYTES to the message log, optionally
   terminated with a newline when NLFLAG is non-zero.  MULTIBYTE, if
   nonzero, means interpret the contents of M as multibyte.  This
   function calls low-level routines in order to bypass text property
   hooks, etc. which might not be safe to run.  */

void
message_dolog (m, nbytes, nlflag, multibyte)
     char *m;
     int nbytes, nlflag, multibyte;
{
  if (!NILP (Vmessage_log_max))
    {
      struct buffer *oldbuf;
      Lisp_Object oldpoint, oldbegv, oldzv;
      int old_windows_or_buffers_changed = windows_or_buffers_changed;
      int point_at_end = 0;
      int zv_at_end = 0;
      Lisp_Object old_deactivate_mark, tem;
      struct gcpro gcpro1;

      old_deactivate_mark = Vdeactivate_mark;
      oldbuf = current_buffer;
      Fset_buffer (Fget_buffer_create (Vmessages_buffer_name));
      current_buffer->undo_list = Qt;

      oldpoint = message_dolog_marker1;
      set_marker_restricted (oldpoint, make_number (PT), Qnil);
      oldbegv = message_dolog_marker2;
      set_marker_restricted (oldbegv, make_number (BEGV), Qnil);
      oldzv = message_dolog_marker3;
      set_marker_restricted (oldzv, make_number (ZV), Qnil);
      GCPRO1 (old_deactivate_mark);

      if (PT == Z)
	point_at_end = 1;
      if (ZV == Z)
	zv_at_end = 1;

      BEGV = BEG;
      BEGV_BYTE = BEG_BYTE;
      ZV = Z;
      ZV_BYTE = Z_BYTE;
      TEMP_SET_PT_BOTH (Z, Z_BYTE);

      /* Insert the string--maybe converting multibyte to single byte
	 or vice versa, so that all the text fits the buffer.  */
      if (multibyte
	  && NILP (current_buffer->enable_multibyte_characters))
	{
	  int i, c, char_bytes;
	  unsigned char work[1];
	  
	  /* Convert a multibyte string to single-byte
	     for the *Message* buffer.  */
	  for (i = 0; i < nbytes; i += nbytes)
	    {
	      c = string_char_and_length (m + i, nbytes - i, &char_bytes);
	      work[0] = (SINGLE_BYTE_CHAR_P (c)
			 ? c
			 : multibyte_char_to_unibyte (c, Qnil));
	      insert_1_both (work, 1, 1, 1, 0, 0);
	    }
	}
      else if (! multibyte
	       && ! NILP (current_buffer->enable_multibyte_characters))
	{
	  int i, c, char_bytes;
	  unsigned char *msg = (unsigned char *) m;
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
	  /* Convert a single-byte string to multibyte
	     for the *Message* buffer.  */
	  for (i = 0; i < nbytes; i++)
	    {
	      c = unibyte_char_to_multibyte (msg[i]);
	      char_bytes = CHAR_STRING (c, str);
	      insert_1_both (str, 1, char_bytes, 1, 0, 0);
	    }
	}
      else if (nbytes)
	insert_1 (m, nbytes, 1, 0, 0);

      if (nlflag)
	{
	  int this_bol, this_bol_byte, prev_bol, prev_bol_byte, dup;
	  insert_1 ("\n", 1, 1, 0, 0);

	  scan_newline (Z, Z_BYTE, BEG, BEG_BYTE, -2, 0);
	  this_bol = PT;
	  this_bol_byte = PT_BYTE;

	  /* See if this line duplicates the previous one.
	     If so, combine duplicates.  */
	  if (this_bol > BEG)
	    {
	      scan_newline (PT, PT_BYTE, BEG, BEG_BYTE, -2, 0);
	      prev_bol = PT;
	      prev_bol_byte = PT_BYTE;

	      dup = message_log_check_duplicate (prev_bol, prev_bol_byte,
						 this_bol, this_bol_byte);
	      if (dup)
		{
		  del_range_both (prev_bol, prev_bol_byte,
				  this_bol, this_bol_byte, 0);
		  if (dup > 1)
		    {
		      char dupstr[40];
		      int duplen;

		      /* If you change this format, don't forget to also
			 change message_log_check_duplicate.  */
		      sprintf (dupstr, " [%d times]", dup);
		      duplen = strlen (dupstr);
		      TEMP_SET_PT_BOTH (Z - 1, Z_BYTE - 1);
		      insert_1 (dupstr, duplen, 1, 0, 1);
		    }
		}
	    }

	  /* If we have more than the desired maximum number of lines
	     in the *Messages* buffer now, delete the oldest ones.
	     This is safe because we don't have undo in this buffer.  */

	  if (NATNUMP (Vmessage_log_max))
	    {
	      scan_newline (Z, Z_BYTE, BEG, BEG_BYTE,
			    -XFASTINT (Vmessage_log_max) - 1, 0);
	      del_range_both (BEG, BEG_BYTE, PT, PT_BYTE, 0);
	    }
	}
      BEGV = XMARKER (oldbegv)->charpos;
      BEGV_BYTE = marker_byte_position (oldbegv);

      if (zv_at_end)
	{
	  ZV = Z;
	  ZV_BYTE = Z_BYTE;
	}
      else
	{
	  ZV = XMARKER (oldzv)->charpos;
	  ZV_BYTE = marker_byte_position (oldzv);
	}

      if (point_at_end)
	TEMP_SET_PT_BOTH (Z, Z_BYTE);
      else
	/* We can't do Fgoto_char (oldpoint) because it will run some
           Lisp code.  */
	TEMP_SET_PT_BOTH (XMARKER (oldpoint)->charpos,
			  XMARKER (oldpoint)->bytepos);

      UNGCPRO;
      unchain_marker (oldpoint);
      unchain_marker (oldbegv);
      unchain_marker (oldzv);

      tem = Fget_buffer_window (Fcurrent_buffer (), Qt);
      set_buffer_internal (oldbuf);
      if (NILP (tem))
	windows_or_buffers_changed = old_windows_or_buffers_changed;
      message_log_need_newline = !nlflag;
      Vdeactivate_mark = old_deactivate_mark;
    }
}


/* We are at the end of the buffer after just having inserted a newline.
   (Note: We depend on the fact we won't be crossing the gap.)
   Check to see if the most recent message looks a lot like the previous one.
   Return 0 if different, 1 if the new one should just replace it, or a
   value N > 1 if we should also append " [N times]".  */

static int
message_log_check_duplicate (prev_bol, prev_bol_byte, this_bol, this_bol_byte)
     int prev_bol, this_bol;
     int prev_bol_byte, this_bol_byte;
{
  int i;
  int len = Z_BYTE - 1 - this_bol_byte;
  int seen_dots = 0;
  unsigned char *p1 = BUF_BYTE_ADDRESS (current_buffer, prev_bol_byte);
  unsigned char *p2 = BUF_BYTE_ADDRESS (current_buffer, this_bol_byte);

  for (i = 0; i < len; i++)
    {
      if (i >= 3 && p1[i-3] == '.' && p1[i-2] == '.' && p1[i-1] == '.')
	seen_dots = 1;
      if (p1[i] != p2[i])
	return seen_dots;
    }
  p1 += len;
  if (*p1 == '\n')
    return 2;
  if (*p1++ == ' ' && *p1++ == '[')
    {
      int n = 0;
      while (*p1 >= '0' && *p1 <= '9')
	n = n * 10 + *p1++ - '0';
      if (strncmp (p1, " times]\n", 8) == 0)
	return n+1;
    }
  return 0;
}


/* Display an echo area message M with a specified length of NBYTES
   bytes.  The string may include null characters.  If M is 0, clear
   out any existing message, and let the mini-buffer text show
   through.

   The buffer M must continue to exist until after the echo area gets
   cleared or some other message gets displayed there.  This means do
   not pass text that is stored in a Lisp string; do not pass text in
   a buffer that was alloca'd.  */

void
message2 (m, nbytes, multibyte)
     char *m;
     int nbytes;
     int multibyte;
{
  /* First flush out any partial line written with print.  */
  message_log_maybe_newline ();
  if (m)
    message_dolog (m, nbytes, 1, multibyte);
  message2_nolog (m, nbytes, multibyte);
}


/* The non-logging counterpart of message2.  */

void
message2_nolog (m, nbytes, multibyte)
     char *m;
     int nbytes;
{
  struct frame *sf = SELECTED_FRAME ();
  message_enable_multibyte = multibyte;

  if (noninteractive)
    {
      if (noninteractive_need_newline)
	putc ('\n', stderr);
      noninteractive_need_newline = 0;
      if (m)
	fwrite (m, nbytes, 1, stderr);
      if (cursor_in_echo_area == 0)
	fprintf (stderr, "\n");
      fflush (stderr);
    }
  /* A null message buffer means that the frame hasn't really been
     initialized yet.  Error messages get reported properly by
     cmd_error, so this must be just an informative message; toss it.  */
  else if (INTERACTIVE 
	   && sf->glyphs_initialized_p
	   && FRAME_MESSAGE_BUF (sf))
    {
      Lisp_Object mini_window;
      struct frame *f;

      /* Get the frame containing the mini-buffer
	 that the selected frame is using.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      f = XFRAME (WINDOW_FRAME (XWINDOW (mini_window)));

      FRAME_SAMPLE_VISIBILITY (f);
      if (FRAME_VISIBLE_P (sf)
	  && ! FRAME_VISIBLE_P (f))
	Fmake_frame_visible (WINDOW_FRAME (XWINDOW (mini_window)));

      if (m)
	{
	  set_message (m, Qnil, nbytes, multibyte);
	  if (minibuffer_auto_raise)
	    Fraise_frame  (WINDOW_FRAME (XWINDOW (mini_window)));
	}
      else
	clear_message (1, 1);

      do_pending_window_change (0);
      echo_area_display (1);
      do_pending_window_change (0);
      if (frame_up_to_date_hook != 0 && ! gc_in_progress)
	(*frame_up_to_date_hook) (f);
    }
}


/* Display an echo area message M with a specified length of NBYTES
   bytes.  The string may include null characters.  If M is not a
   string, clear out any existing message, and let the mini-buffer
   text show through.  */

void
message3 (m, nbytes, multibyte)
     Lisp_Object m;
     int nbytes;
     int multibyte;
{
  struct gcpro gcpro1;

  GCPRO1 (m);
  
  /* First flush out any partial line written with print.  */
  message_log_maybe_newline ();
  if (STRINGP (m))
    message_dolog (SDATA (m), nbytes, 1, multibyte);
  message3_nolog (m, nbytes, multibyte);

  UNGCPRO;
}


/* The non-logging version of message3.  */

void
message3_nolog (m, nbytes, multibyte)
     Lisp_Object m;
     int nbytes, multibyte;
{
  struct frame *sf = SELECTED_FRAME ();
  message_enable_multibyte = multibyte;

  if (noninteractive)
    {
      if (noninteractive_need_newline)
	putc ('\n', stderr);
      noninteractive_need_newline = 0;
      if (STRINGP (m))
	fwrite (SDATA (m), nbytes, 1, stderr);
      if (cursor_in_echo_area == 0)
	fprintf (stderr, "\n");
      fflush (stderr);
    }
  /* A null message buffer means that the frame hasn't really been
     initialized yet.  Error messages get reported properly by
     cmd_error, so this must be just an informative message; toss it.  */
  else if (INTERACTIVE 
	   && sf->glyphs_initialized_p
	   && FRAME_MESSAGE_BUF (sf))
    {
      Lisp_Object mini_window;
      Lisp_Object frame;
      struct frame *f;

      /* Get the frame containing the mini-buffer
	 that the selected frame is using.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      frame = XWINDOW (mini_window)->frame;
      f = XFRAME (frame);

      FRAME_SAMPLE_VISIBILITY (f);
      if (FRAME_VISIBLE_P (sf)
	  && !FRAME_VISIBLE_P (f))
	Fmake_frame_visible (frame);

      if (STRINGP (m) && SCHARS (m) > 0)
	{
	  set_message (NULL, m, nbytes, multibyte);
	  if (minibuffer_auto_raise)
	    Fraise_frame (frame);
	}
      else
	clear_message (1, 1);

      do_pending_window_change (0);
      echo_area_display (1);
      do_pending_window_change (0);
      if (frame_up_to_date_hook != 0 && ! gc_in_progress)
	(*frame_up_to_date_hook) (f);
    }
}


/* Display a null-terminated echo area message M.  If M is 0, clear
   out any existing message, and let the mini-buffer text show through.

   The buffer M must continue to exist until after the echo area gets
   cleared or some other message gets displayed there.  Do not pass
   text that is stored in a Lisp string.  Do not pass text in a buffer
   that was alloca'd.  */

void
message1 (m)
     char *m;
{
  message2 (m, (m ? strlen (m) : 0), 0);
}


/* The non-logging counterpart of message1.  */

void
message1_nolog (m)
     char *m;
{
  message2_nolog (m, (m ? strlen (m) : 0), 0);
}

/* Display a message M which contains a single %s
   which gets replaced with STRING.  */

void
message_with_string (m, string, log)
     char *m;
     Lisp_Object string;
     int log;
{
  if (noninteractive)
    {
      if (m)
	{
	  if (noninteractive_need_newline)
	    putc ('\n', stderr);
	  noninteractive_need_newline = 0;
	  fprintf (stderr, m, SDATA (string));
	  if (cursor_in_echo_area == 0)
	    fprintf (stderr, "\n");
	  fflush (stderr);
	}
    }
  else if (INTERACTIVE)
    {
      /* The frame whose minibuffer we're going to display the message on.
	 It may be larger than the selected frame, so we need
	 to use its buffer, not the selected frame's buffer.  */
      Lisp_Object mini_window;
      struct frame *f, *sf = SELECTED_FRAME ();

      /* Get the frame containing the minibuffer
	 that the selected frame is using.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      f = XFRAME (WINDOW_FRAME (XWINDOW (mini_window)));

      /* A null message buffer means that the frame hasn't really been
	 initialized yet.  Error messages get reported properly by
	 cmd_error, so this must be just an informative message; toss it.  */
      if (FRAME_MESSAGE_BUF (f))
	{
	  Lisp_Object args[2], message;
	  struct gcpro gcpro1, gcpro2;

	  args[0] = build_string (m);
	  args[1] = message = string;
	  GCPRO2 (args[0], message);
	  gcpro1.nvars = 2;
	  
	  message = Fformat (2, args);

	  if (log)
	    message3 (message, SBYTES (message), SMBP (message));
	  else
	    message3_nolog (message, SBYTES (message), SMBP (message));

	  UNGCPRO;

	  /* Print should start at the beginning of the message
	     buffer next time.  */
	  message_buf_print = 0;
	}
    }
}


/* Dump an informative message to the minibuf.  If M is 0, clear out
   any existing message, and let the mini-buffer text show through.  */

/* VARARGS 1 */
void
message (m, a1, a2, a3)
     char *m;
     EMACS_INT a1, a2, a3;
{
  if (noninteractive)
    {
      if (m)
	{
	  if (noninteractive_need_newline)
	    putc ('\n', stderr);
	  noninteractive_need_newline = 0;
	  fprintf (stderr, m, a1, a2, a3);
	  if (cursor_in_echo_area == 0)
	    fprintf (stderr, "\n");
	  fflush (stderr);
	}
    }
  else if (INTERACTIVE)
    {
      /* The frame whose mini-buffer we're going to display the message
	 on.  It may be larger than the selected frame, so we need to
	 use its buffer, not the selected frame's buffer.  */
      Lisp_Object mini_window;
      struct frame *f, *sf = SELECTED_FRAME ();

      /* Get the frame containing the mini-buffer
	 that the selected frame is using.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      f = XFRAME (WINDOW_FRAME (XWINDOW (mini_window)));

      /* A null message buffer means that the frame hasn't really been
	 initialized yet.  Error messages get reported properly by
	 cmd_error, so this must be just an informative message; toss
	 it.  */
      if (FRAME_MESSAGE_BUF (f))
	{
	  if (m)
	    {
	      int len;
#ifdef NO_ARG_ARRAY
	      char *a[3];
	      a[0] = (char *) a1;
	      a[1] = (char *) a2;
	      a[2] = (char *) a3;

	      len = doprnt (FRAME_MESSAGE_BUF (f),
			    FRAME_MESSAGE_BUF_SIZE (f), m, (char *)0, 3, a);
#else
	      len = doprnt (FRAME_MESSAGE_BUF (f),
			    FRAME_MESSAGE_BUF_SIZE (f), m, (char *)0, 3,
			    (char **) &a1);
#endif /* NO_ARG_ARRAY */

	      message2 (FRAME_MESSAGE_BUF (f), len, 0);
	    }
	  else
	    message1 (0);

	  /* Print should start at the beginning of the message
	     buffer next time.  */
	  message_buf_print = 0;
	}
    }
}


/* The non-logging version of message.  */

void
message_nolog (m, a1, a2, a3)
     char *m;
     EMACS_INT a1, a2, a3;
{
  Lisp_Object old_log_max;
  old_log_max = Vmessage_log_max;
  Vmessage_log_max = Qnil;
  message (m, a1, a2, a3);
  Vmessage_log_max = old_log_max;
}


/* Display the current message in the current mini-buffer.  This is
   only called from error handlers in process.c, and is not time
   critical.  */

void
update_echo_area ()
{
  if (!NILP (echo_area_buffer[0]))
    {
      Lisp_Object string;
      string = Fcurrent_message ();
      message3 (string, SBYTES (string), 
		!NILP (current_buffer->enable_multibyte_characters));
    }
}


/* Make sure echo area buffers in echo_buffers[] are life.  If they
   aren't, make new ones.  */

static void
ensure_echo_area_buffers ()
{
  int i;

  for (i = 0; i < 2; ++i)
    if (!BUFFERP (echo_buffer[i])
	|| NILP (XBUFFER (echo_buffer[i])->name))
      {
	char name[30];
	Lisp_Object old_buffer;
	int j;

	old_buffer = echo_buffer[i];
	sprintf (name, " *Echo Area %d*", i);
	echo_buffer[i] = Fget_buffer_create (build_string (name));
	XBUFFER (echo_buffer[i])->truncate_lines = Qnil;

	for (j = 0; j < 2; ++j)
	  if (EQ (old_buffer, echo_area_buffer[j]))
	    echo_area_buffer[j] = echo_buffer[i];
      }
}


/* Call FN with args A1..A4 with either the current or last displayed
   echo_area_buffer as current buffer.

   WHICH zero means use the current message buffer
   echo_area_buffer[0].  If that is nil, choose a suitable buffer
   from echo_buffer[] and clear it.

   WHICH > 0 means use echo_area_buffer[1].  If that is nil, choose a
   suitable buffer from echo_buffer[] and clear it.

   If WHICH < 0, set echo_area_buffer[1] to echo_area_buffer[0], so
   that the current message becomes the last displayed one, make
   choose a suitable buffer for echo_area_buffer[0], and clear it.

   Value is what FN returns.  */

static int
with_echo_area_buffer (w, which, fn, a1, a2, a3, a4)
     struct window *w;
     int which;
     int (*fn) P_ ((EMACS_INT, Lisp_Object, EMACS_INT, EMACS_INT));
     EMACS_INT a1;
     Lisp_Object a2;
     EMACS_INT a3, a4;
{
  Lisp_Object buffer;
  int this_one, the_other, clear_buffer_p, rc;
  int count = BINDING_STACK_SIZE ();

  /* If buffers aren't life, make new ones.  */
  ensure_echo_area_buffers ();

  clear_buffer_p = 0;
  
  if (which == 0)
    this_one = 0, the_other = 1;
  else if (which > 0)
    this_one = 1, the_other = 0;
  else
    {
      this_one = 0, the_other = 1;
      clear_buffer_p = 1;
      
      /* We need a fresh one in case the current echo buffer equals
	 the one containing the last displayed echo area message.  */
      if (!NILP (echo_area_buffer[this_one])
	  && EQ (echo_area_buffer[this_one], echo_area_buffer[the_other]))
	echo_area_buffer[this_one] = Qnil;
    }

  /* Choose a suitable buffer from echo_buffer[] is we don't
     have one.  */
  if (NILP (echo_area_buffer[this_one]))
    {
      echo_area_buffer[this_one]
	= (EQ (echo_area_buffer[the_other], echo_buffer[this_one])
	   ? echo_buffer[the_other]
	   : echo_buffer[this_one]);
      clear_buffer_p = 1;
    }

  buffer = echo_area_buffer[this_one];

  /* Don't get confused by reusing the buffer used for echoing
     for a different purpose.  */
  if (echo_kboard == NULL && EQ (buffer, echo_message_buffer))
    cancel_echoing ();

  record_unwind_protect (unwind_with_echo_area_buffer,
			 with_echo_area_buffer_unwind_data (w));

  /* Make the echo area buffer current.  Note that for display
     purposes, it is not necessary that the displayed window's buffer
     == current_buffer, except for text property lookup.  So, let's
     only set that buffer temporarily here without doing a full
     Fset_window_buffer.  We must also change w->pointm, though,
     because otherwise an assertions in unshow_buffer fails, and Emacs
     aborts.  */
  set_buffer_internal_1 (XBUFFER (buffer));
  if (w)
    {
      w->buffer = buffer;
      set_marker_both (w->pointm, buffer, BEG, BEG_BYTE);
    }

  current_buffer->undo_list = Qt;
  current_buffer->read_only = Qnil;
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);

  if (clear_buffer_p && Z > BEG)
    del_range (BEG, Z);

  xassert (BEGV >= BEG);
  xassert (ZV <= Z && ZV >= BEGV);

  rc = fn (a1, a2, a3, a4);

  xassert (BEGV >= BEG);
  xassert (ZV <= Z && ZV >= BEGV);

  unbind_to (count, Qnil);
  return rc;
}


/* Save state that should be preserved around the call to the function
   FN called in with_echo_area_buffer.  */

static Lisp_Object
with_echo_area_buffer_unwind_data (w)
     struct window *w;
{
  int i = 0;
  Lisp_Object vector;

  /* Reduce consing by keeping one vector in
     Vwith_echo_area_save_vector.  */
  vector = Vwith_echo_area_save_vector;
  Vwith_echo_area_save_vector = Qnil;
  
  if (NILP (vector))
    vector = Fmake_vector (make_number (7), Qnil);
  
  XSETBUFFER (AREF (vector, i), current_buffer); ++i;
  AREF (vector, i) = Vdeactivate_mark, ++i;
  AREF (vector, i) = make_number (windows_or_buffers_changed), ++i;
  
  if (w)
    {
      XSETWINDOW (AREF (vector, i), w); ++i;
      AREF (vector, i) = w->buffer; ++i;
      AREF (vector, i) = make_number (XMARKER (w->pointm)->charpos); ++i;
      AREF (vector, i) = make_number (XMARKER (w->pointm)->bytepos); ++i;
    }
  else
    {
      int end = i + 4;
      for (; i < end; ++i)
	AREF (vector, i) = Qnil;
    }

  xassert (i == ASIZE (vector));
  return vector;
}


/* Restore global state from VECTOR which was created by
   with_echo_area_buffer_unwind_data.  */

static Lisp_Object
unwind_with_echo_area_buffer (vector)
     Lisp_Object vector;
{
  set_buffer_internal_1 (XBUFFER (AREF (vector, 0)));
  Vdeactivate_mark = AREF (vector, 1);
  windows_or_buffers_changed = XFASTINT (AREF (vector, 2));

  if (WINDOWP (AREF (vector, 3)))
    {
      struct window *w;
      Lisp_Object buffer, charpos, bytepos;
      
      w = XWINDOW (AREF (vector, 3));
      buffer = AREF (vector, 4);
      charpos = AREF (vector, 5);
      bytepos = AREF (vector, 6);
      
      w->buffer = buffer;
      set_marker_both (w->pointm, buffer,
		       XFASTINT (charpos), XFASTINT (bytepos));
    }

  Vwith_echo_area_save_vector = vector;
  return Qnil;
}


/* Set up the echo area for use by print functions.  MULTIBYTE_P
   non-zero means we will print multibyte.  */

void
setup_echo_area_for_printing (multibyte_p)
     int multibyte_p;
{
  ensure_echo_area_buffers ();

  if (!message_buf_print)
    {
      /* A message has been output since the last time we printed.
	 Choose a fresh echo area buffer.  */
      if (EQ (echo_area_buffer[1], echo_buffer[0]))
	echo_area_buffer[0] = echo_buffer[1]; 
      else
	echo_area_buffer[0] = echo_buffer[0];

      /* Switch to that buffer and clear it.  */
      set_buffer_internal (XBUFFER (echo_area_buffer[0]));
      current_buffer->truncate_lines = Qnil;
      
      if (Z > BEG)
	{
	  int count = BINDING_STACK_SIZE ();
	  specbind (Qinhibit_read_only, Qt);
	  del_range (BEG, Z);
	  unbind_to (count, Qnil);
	}
      TEMP_SET_PT_BOTH (BEG, BEG_BYTE);

      /* Set up the buffer for the multibyteness we need.  */
      if (multibyte_p
	  != !NILP (current_buffer->enable_multibyte_characters))
	Fset_buffer_multibyte (multibyte_p ? Qt : Qnil);

      /* Raise the frame containing the echo area.  */
      if (minibuffer_auto_raise)
	{
	  struct frame *sf = SELECTED_FRAME ();
	  Lisp_Object mini_window;
	  mini_window = FRAME_MINIBUF_WINDOW (sf);
	  Fraise_frame  (WINDOW_FRAME (XWINDOW (mini_window)));
	}

      message_log_maybe_newline ();
      message_buf_print = 1;
    }
  else
    {
      if (NILP (echo_area_buffer[0]))
	{
	  if (EQ (echo_area_buffer[1], echo_buffer[0]))
	    echo_area_buffer[0] = echo_buffer[1]; 
	  else
	    echo_area_buffer[0] = echo_buffer[0];
	}
      
      if (current_buffer != XBUFFER (echo_area_buffer[0]))
	{
	  /* Someone switched buffers between print requests.  */
	  set_buffer_internal (XBUFFER (echo_area_buffer[0]));
	  current_buffer->truncate_lines = Qnil;
	}
    }
}


/* Display an echo area message in window W.  Value is non-zero if W's
   height is changed.  If display_last_displayed_message_p is
   non-zero, display the message that was last displayed, otherwise
   display the current message.  */

static int
display_echo_area (w)
     struct window *w;
{
  int i, no_message_p, window_height_changed_p, count;

  /* Temporarily disable garbage collections while displaying the echo
     area.  This is done because a GC can print a message itself.
     That message would modify the echo area buffer's contents while a
     redisplay of the buffer is going on, and seriously confuse
     redisplay.  */
  count = inhibit_garbage_collection ();

  /* If there is no message, we must call display_echo_area_1
     nevertheless because it resizes the window.  But we will have to
     reset the echo_area_buffer in question to nil at the end because
     with_echo_area_buffer will sets it to an empty buffer.  */
  i = display_last_displayed_message_p ? 1 : 0;
  no_message_p = NILP (echo_area_buffer[i]);
  
  window_height_changed_p
    = with_echo_area_buffer (w, display_last_displayed_message_p,
			     display_echo_area_1,
			     (EMACS_INT) w, Qnil, 0, 0);

  if (no_message_p)
    echo_area_buffer[i] = Qnil;

  unbind_to (count, Qnil);
  return window_height_changed_p;
}


/* Helper for display_echo_area.  Display the current buffer which
   contains the current echo area message in window W, a mini-window,
   a pointer to which is passed in A1.  A2..A4 are currently not used.
   Change the height of W so that all of the message is displayed.
   Value is non-zero if height of W was changed.  */

static int
display_echo_area_1 (a1, a2, a3, a4)
     EMACS_INT a1;
     Lisp_Object a2;
     EMACS_INT a3, a4;
{
  struct window *w = (struct window *) a1;
  Lisp_Object window;
  struct text_pos start;
  int window_height_changed_p = 0;

  /* Do this before displaying, so that we have a large enough glyph
     matrix for the display.  */
  window_height_changed_p = resize_mini_window (w, 0);

  /* Display.  */
  clear_glyph_matrix (w->desired_matrix);
  XSETWINDOW (window, w);
  SET_TEXT_POS (start, BEG, BEG_BYTE);
  try_window (window, start);

  return window_height_changed_p;
}


/* Resize the echo area window to exactly the size needed for the
   currently displayed message, if there is one.  If a mini-buffer
   is active, don't shrink it.  */

void
resize_echo_area_exactly ()
{
  if (BUFFERP (echo_area_buffer[0])
      && WINDOWP (echo_area_window))
    {
      struct window *w = XWINDOW (echo_area_window);
      int resized_p;
      Lisp_Object resize_exactly;

      if (minibuf_level == 0)
	resize_exactly = Qt;
      else
	resize_exactly = Qnil;
      
      resized_p = with_echo_area_buffer (w, 0, resize_mini_window_1,
					 (EMACS_INT) w, resize_exactly, 0, 0);
      if (resized_p)
	{
	  ++windows_or_buffers_changed;
	  ++update_mode_lines;
	  redisplay_internal (0);
	}
    }
}


/* Callback function for with_echo_area_buffer, when used from
   resize_echo_area_exactly.  A1 contains a pointer to the window to
   resize, EXACTLY non-nil means resize the mini-window exactly to the
   size of the text displayed.  A3 and A4 are not used.  Value is what
   resize_mini_window returns.  */

static int
resize_mini_window_1 (a1, exactly, a3, a4)
     EMACS_INT a1;
     Lisp_Object exactly;
     EMACS_INT a3, a4;
{
  return resize_mini_window ((struct window *) a1, !NILP (exactly));
}


/* Resize mini-window W to fit the size of its contents.  EXACT:P
   means size the window exactly to the size needed.  Otherwise, it's
   only enlarged until W's buffer is empty.  Value is non-zero if
   the window height has been changed.  */

int
resize_mini_window (w, exact_p)
     struct window *w;
     int exact_p;
{
  struct frame *f = XFRAME (w->frame);
  int window_height_changed_p = 0;

  xassert (MINI_WINDOW_P (w));

  /* Don't resize windows while redisplaying a window; it would
     confuse redisplay functions when the size of the window they are
     displaying changes from under them.  Such a resizing can happen,
     for instance, when which-func prints a long message while
     we are running fontification-functions.  We're running these
     functions with safe_call which binds inhibit-redisplay to t.  */
  if (!NILP (Vinhibit_redisplay))
    return 0;
  
  /* Nil means don't try to resize.  */
  if (NILP (Vresize_mini_windows)
      || (FRAME_X_P (f) && f->output_data.x == NULL))
    return 0;
  
  if (!FRAME_MINIBUF_ONLY_P (f))
    {
      struct it it;
      struct window *root = XWINDOW (FRAME_ROOT_WINDOW (f));
      int total_height = XFASTINT (root->height) + XFASTINT (w->height);
      int height, max_height;
      int unit = CANON_Y_UNIT (f);
      struct text_pos start;
      struct buffer *old_current_buffer = NULL;

      if (current_buffer != XBUFFER (w->buffer))
	{
	  old_current_buffer = current_buffer;
	  set_buffer_internal (XBUFFER (w->buffer));
	}

      init_iterator (&it, w, BEGV, BEGV_BYTE, NULL, DEFAULT_FACE_ID);

      /* Compute the max. number of lines specified by the user.  */
      if (FLOATP (Vmax_mini_window_height))
	max_height = XFLOATINT (Vmax_mini_window_height) * FRAME_HEIGHT (f);
      else if (INTEGERP (Vmax_mini_window_height))
	max_height = XINT (Vmax_mini_window_height);
      else
	max_height = total_height / 4;
      
      /* Correct that max. height if it's bogus.  */
      max_height = max (1, max_height);
      max_height = min (total_height, max_height);
      
      /* Find out the height of the text in the window.  */
      if (it.truncate_lines_p)
	height = 1;
      else
	{
	  last_height = 0;
	  move_it_to (&it, ZV, -1, -1, -1, MOVE_TO_POS);
	  if (it.max_ascent == 0 && it.max_descent == 0)
	    height = it.current_y + last_height;
	  else
	    height = it.current_y + it.max_ascent + it.max_descent;
	  height -= it.extra_line_spacing;
	  height = (height + unit - 1) / unit;
	}
      
      /* Compute a suitable window start.  */
      if (height > max_height)
	{
	  height = max_height;
	  init_iterator (&it, w, PT, PT_BYTE, NULL, DEFAULT_FACE_ID);
	  move_it_vertically_backward (&it, (height - 1) * unit);
	  start = it.current.pos;
	}
      else
	SET_TEXT_POS (start, BEGV, BEGV_BYTE);
      SET_MARKER_FROM_TEXT_POS (w->start, start);

      if (EQ (Vresize_mini_windows, Qgrow_only))
	{
	  /* Let it grow only, until we display an empty message, in which
	     case the window shrinks again.  */
	  if (height > XFASTINT (w->height))
	    {
	      int old_height = XFASTINT (w->height);
	      freeze_window_starts (f, 1);
	      grow_mini_window (w, height - XFASTINT (w->height));
	      window_height_changed_p = XFASTINT (w->height) != old_height;
	    }
	  else if (height < XFASTINT (w->height)
		   && (exact_p || BEGV == ZV))
	    {
	      int old_height = XFASTINT (w->height);
	      freeze_window_starts (f, 0);
	      shrink_mini_window (w);
	      window_height_changed_p = XFASTINT (w->height) != old_height;
	    }
	}
      else 
	{
	  /* Always resize to exact size needed.  */
	  if (height > XFASTINT (w->height))
	    {
	      int old_height = XFASTINT (w->height);
	      freeze_window_starts (f, 1);
	      grow_mini_window (w, height - XFASTINT (w->height));
	      window_height_changed_p = XFASTINT (w->height) != old_height;
	    }
	  else if (height < XFASTINT (w->height))
	    {
	      int old_height = XFASTINT (w->height);
	      freeze_window_starts (f, 0);
	      shrink_mini_window (w);

	      if (height)
		{
		  freeze_window_starts (f, 1);
		  grow_mini_window (w, height - XFASTINT (w->height));
		}
	      
	      window_height_changed_p = XFASTINT (w->height) != old_height;
	    }
	}

      if (old_current_buffer)
	set_buffer_internal (old_current_buffer);
    }

  return window_height_changed_p;
}


/* Value is the current message, a string, or nil if there is no
   current message.  */

Lisp_Object
current_message ()
{
  Lisp_Object msg;

  if (NILP (echo_area_buffer[0]))
    msg = Qnil;
  else
    {
      with_echo_area_buffer (0, 0, current_message_1,
			     (EMACS_INT) &msg, Qnil, 0, 0);
      if (NILP (msg))
	echo_area_buffer[0] = Qnil;
    }
  
  return msg;
}


static int
current_message_1 (a1, a2, a3, a4)
     EMACS_INT a1;
     Lisp_Object a2;
     EMACS_INT a3, a4;
{
  Lisp_Object *msg = (Lisp_Object *) a1;
  
  if (Z > BEG)
    *msg = make_buffer_string (BEG, Z, 1);
  else
    *msg = Qnil;
  return 0;
}


/* Push the current message on Vmessage_stack for later restauration
   by restore_message.  Value is non-zero if the current message isn't
   empty.  This is a relatively infrequent operation, so it's not
   worth optimizing.  */

int
push_message ()
{
  Lisp_Object msg;
  msg = current_message ();
  Vmessage_stack = Fcons (msg, Vmessage_stack);
  return STRINGP (msg);
}


/* Handler for record_unwind_protect calling pop_message.  */

Lisp_Object
push_message_unwind (dummy)
     Lisp_Object dummy;
{
  pop_message ();
  return Qnil;
}


/* Restore message display from the top of Vmessage_stack.  */

void
restore_message ()
{
  Lisp_Object msg;
  
  xassert (CONSP (Vmessage_stack));
  msg = XCAR (Vmessage_stack);
  if (STRINGP (msg))
    message3_nolog (msg, SBYTES (msg), SMBP (msg));
  else
    message3_nolog (msg, 0, 0);
}


/* Pop the top-most entry off Vmessage_stack.  */

void
pop_message ()
{
  xassert (CONSP (Vmessage_stack));
  Vmessage_stack = XCDR (Vmessage_stack);
}


/* Check that Vmessage_stack is nil.  Called from emacs.c when Emacs
   exits.  If the stack is not empty, we have a missing pop_message
   somewhere.  */

void
check_message_stack ()
{
  if (!NILP (Vmessage_stack))
    abort ();
}


/* Truncate to NCHARS what will be displayed in the echo area the next
   time we display it---but don't redisplay it now.  */

void
truncate_echo_area (nchars)
     int nchars;
{
  if (nchars == 0)
    echo_area_buffer[0] = Qnil;
  /* A null message buffer means that the frame hasn't really been
     initialized yet.  Error messages get reported properly by
     cmd_error, so this must be just an informative message; toss it.  */
  else if (!noninteractive
	   && INTERACTIVE
	   && !NILP (echo_area_buffer[0]))
    {
      struct frame *sf = SELECTED_FRAME ();
      if (FRAME_MESSAGE_BUF (sf))
	with_echo_area_buffer (0, 0, truncate_message_1, nchars, Qnil, 0, 0);
    }
}


/* Helper function for truncate_echo_area.  Truncate the current
   message to at most NCHARS characters.  */

static int
truncate_message_1 (nchars, a2, a3, a4)
     EMACS_INT nchars;
     Lisp_Object a2;
     EMACS_INT a3, a4;
{
  if (BEG + nchars < Z)
    del_range (BEG + nchars, Z);
  if (Z == BEG)
    echo_area_buffer[0] = Qnil;
  return 0;
}


/* Set the current message to a substring of S or STRING.

   If STRING is a Lisp string, set the message to the first NBYTES
   bytes from STRING.  NBYTES zero means use the whole string.  If
   STRING is multibyte, the message will be displayed multibyte.

   If S is not null, set the message to the first LEN bytes of S.  LEN
   zero means use the whole string.  MULTIBYTE_P non-zero means S is
   multibyte.  Display the message multibyte in that case.  */

void
set_message (s, string, nbytes, multibyte_p)
     char *s;
     Lisp_Object string;
     int nbytes;
{
  message_enable_multibyte
    = ((s && multibyte_p)
       || (STRINGP (string) && STRING_MULTIBYTE (string)));
  
  with_echo_area_buffer (0, -1, set_message_1,
			 (EMACS_INT) s, string, nbytes, multibyte_p);
  message_buf_print = 0;
  help_echo_showing_p = 0;
}


/* Helper function for set_message.  Arguments have the same meaning
   as there, with A1 corresponding to S and A2 corresponding to STRING
   This function is called with the echo area buffer being
   current.  */

static int
set_message_1 (a1, a2, nbytes, multibyte_p)
     EMACS_INT a1;
     Lisp_Object a2;
     EMACS_INT nbytes, multibyte_p;
{
  char *s = (char *) a1;
  Lisp_Object string = a2;
  
  xassert (BEG == Z);
  
  /* Change multibyteness of the echo buffer appropriately.  */
  if (message_enable_multibyte
      != !NILP (current_buffer->enable_multibyte_characters))
    Fset_buffer_multibyte (message_enable_multibyte ? Qt : Qnil);

  current_buffer->truncate_lines = message_truncate_lines ? Qt : Qnil;
  
  /* Insert new message at BEG.  */
  TEMP_SET_PT_BOTH (BEG, BEG_BYTE);

  if (STRINGP (string))
    {
      int nchars;
      
      if (nbytes == 0)
	nbytes = SBYTES (string);
      nchars = string_byte_to_char (string, nbytes);
      
      /* This function takes care of single/multibyte conversion.  We
         just have to ensure that the echo area buffer has the right
         setting of enable_multibyte_characters.  */
      insert_from_string (string, 0, 0, nchars, nbytes, 1);
    }
  else if (s)
    {
      if (nbytes == 0)
	nbytes = strlen (s);
      
      if (multibyte_p && NILP (current_buffer->enable_multibyte_characters))
	{
	  /* Convert from multi-byte to single-byte.  */
	  int i, c, n;
	  unsigned char work[1];
	  
	  /* Convert a multibyte string to single-byte.  */
	  for (i = 0; i < nbytes; i += n)
	    {
	      c = string_char_and_length (s + i, nbytes - i, &n);
	      work[0] = (SINGLE_BYTE_CHAR_P (c)
			 ? c
			 : multibyte_char_to_unibyte (c, Qnil));
	      insert_1_both (work, 1, 1, 1, 0, 0);
	    }
	}
      else if (!multibyte_p
	       && !NILP (current_buffer->enable_multibyte_characters))
	{
	  /* Convert from single-byte to multi-byte.  */
	  int i, c, n;
	  unsigned char *msg = (unsigned char *) s;
	  unsigned char str[MAX_MULTIBYTE_LENGTH];
      
	  /* Convert a single-byte string to multibyte.  */
	  for (i = 0; i < nbytes; i++)
	    {
	      c = unibyte_char_to_multibyte (msg[i]);
	      n = CHAR_STRING (c, str);
	      insert_1_both (str, 1, n, 1, 0, 0);
	    }
	}
      else
	insert_1 (s, nbytes, 1, 0, 0);
    }

  return 0;
}


/* Clear messages.  CURRENT_P non-zero means clear the current
   message.  LAST_DISPLAYED_P non-zero means clear the message
   last displayed.  */

void
clear_message (current_p, last_displayed_p)
     int current_p, last_displayed_p;
{
  if (current_p)
    {
      echo_area_buffer[0] = Qnil;
      message_cleared_p = 1;
    }
  
  if (last_displayed_p)
    echo_area_buffer[1] = Qnil;
  
  message_buf_print = 0;
}

/* Clear garbaged frames.

   This function is used where the old redisplay called
   redraw_garbaged_frames which in turn called redraw_frame which in
   turn called clear_frame.  The call to clear_frame was a source of
   flickering.  I believe a clear_frame is not necessary.  It should
   suffice in the new redisplay to invalidate all current matrices,
   and ensure a complete redisplay of all windows.  */

static void
clear_garbaged_frames ()
{
  if (frame_garbaged)
    {
      Lisp_Object tail, frame;
      int changed_count = 0;
      
      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  
	  if (FRAME_VISIBLE_P (f) && FRAME_GARBAGED_P (f))
	    {
	      if (f->resized_p)
		Fredraw_frame (frame);
	      clear_current_matrices (f);
	      changed_count++;
	      f->garbaged = 0;
	      f->resized_p = 0;
	    }
	}

      frame_garbaged = 0;
      if (changed_count)
	++windows_or_buffers_changed;
    }
}


/* Redisplay the echo area of the selected frame.  If UPDATE_FRAME_P
   is non-zero update selected_frame.  Value is non-zero if the
   mini-windows height has been changed.  */

static int
echo_area_display (update_frame_p)
     int update_frame_p;
{
  Lisp_Object mini_window;
  struct window *w;
  struct frame *f;
  int window_height_changed_p = 0;
  struct frame *sf = SELECTED_FRAME ();

  mini_window = FRAME_MINIBUF_WINDOW (sf);
  w = XWINDOW (mini_window);
  f = XFRAME (WINDOW_FRAME (w));

  /* Don't display if frame is invisible or not yet initialized.  */
  if (!FRAME_VISIBLE_P (f) || !f->glyphs_initialized_p)
    return 0;

/* The terminal frame is used as the first Emacs frame on the Mac OS.  */
#ifndef macintosh
#ifdef HAVE_WINDOW_SYSTEM
  /* When Emacs starts, selected_frame may be a visible terminal
     frame, even if we run under a window system.  If we let this
     through, a message would be displayed on the terminal.  */
  if (EQ (selected_frame, Vterminal_frame) 
      && !NILP (Vwindow_system))
    return 0;
#endif /* HAVE_WINDOW_SYSTEM */
#endif

  /* Redraw garbaged frames.  */
  if (frame_garbaged)
    clear_garbaged_frames ();

  if (!NILP (echo_area_buffer[0]) || minibuf_level == 0)
    {
      echo_area_window = mini_window;
      window_height_changed_p = display_echo_area (w);
      w->must_be_updated_p = 1;

      /* Update the display, unless called from redisplay_internal.
	 Also don't update the screen during redisplay itself.  The
	 update will happen at the end of redisplay, and an update
	 here could cause confusion.  */
      if (update_frame_p && !redisplaying_p)
	{
	  int n = 0;

	  /* If the display update has been interrupted by pending
	     input, update mode lines in the frame.  Due to the
	     pending input, it might have been that redisplay hasn't
	     been called, so that mode lines above the echo area are
	     garbaged.  This looks odd, so we prevent it here.  */
	  if (!display_completed)
	    n = redisplay_mode_lines (FRAME_ROOT_WINDOW (f), 0);
	    
	  if (window_height_changed_p
	      /* Don't do this if Emacs is shutting down.  Redisplay
	         needs to run hooks.  */
	      && !NILP (Vrun_hooks))
	    {
	      /* Must update other windows.  Likewise as in other
		 cases, don't let this update be interrupted by
		 pending input.  */
	      int count = BINDING_STACK_SIZE ();
	      specbind (Qredisplay_dont_pause, Qt);
	      windows_or_buffers_changed = 1;
	      redisplay_internal (0);
	      unbind_to (count, Qnil);
	    }
	  else if (FRAME_WINDOW_P (f) && n == 0)
	    {
	      /* Window configuration is the same as before.
		 Can do with a display update of the echo area,
		 unless we displayed some mode lines.  */
	      update_single_window (w, 1);
	      rif->flush_display (f);
	    }
	  else
	    update_frame (f, 1, 1);

	  /* If cursor is in the echo area, make sure that the next
	     redisplay displays the minibuffer, so that the cursor will
	     be replaced with what the minibuffer wants.  */
	  if (cursor_in_echo_area)
	    ++windows_or_buffers_changed;
	}
    }
  else if (!EQ (mini_window, selected_window))
    windows_or_buffers_changed++;

  /* Last displayed message is now the current message.  */
  echo_area_buffer[1] = echo_area_buffer[0];
      
  /* Prevent redisplay optimization in redisplay_internal by resetting
     this_line_start_pos.  This is done because the mini-buffer now
     displays the message instead of its buffer text.  */
  if (EQ (mini_window, selected_window))
    CHARPOS (this_line_start_pos) = 0;

  return window_height_changed_p;
}



/***********************************************************************
			     Frame Titles
 ***********************************************************************/


#ifdef HAVE_WINDOW_SYSTEM

/* A buffer for constructing frame titles in it; allocated from the
   heap in init_xdisp and resized as needed in store_frame_title_char.  */

static char *frame_title_buf;

/* The buffer's end, and a current output position in it.  */

static char *frame_title_buf_end;
static char *frame_title_ptr;


/* Store a single character C for the frame title in frame_title_buf.
   Re-allocate frame_title_buf if necessary.  */

static void
store_frame_title_char (c)
    char c;
{
  /* If output position has reached the end of the allocated buffer,
     double the buffer's size.  */
  if (frame_title_ptr == frame_title_buf_end)
    {
      int len = frame_title_ptr - frame_title_buf;
      int new_size = 2 * len * sizeof *frame_title_buf;
      frame_title_buf = (char *) xrealloc (frame_title_buf, new_size);
      frame_title_buf_end = frame_title_buf + new_size;
      frame_title_ptr = frame_title_buf + len;
    }

  *frame_title_ptr++ = c;
}


/* Store part of a frame title in frame_title_buf, beginning at
   frame_title_ptr.  STR is the string to store.  Do not copy
   characters that yield more columns than PRECISION; PRECISION <= 0
   means copy the whole string.  Pad with spaces until FIELD_WIDTH
   number of characters have been copied; FIELD_WIDTH <= 0 means don't
   pad.  Called from display_mode_element when it is used to build a
   frame title.  */

static int
store_frame_title (str, field_width, precision)
     unsigned char *str;
     int field_width, precision;
{
  int n = 0;
  int dummy, nbytes;

  /* Copy at most PRECISION chars from STR.  */
  nbytes = strlen (str);
  n+= c_string_width (str, nbytes, precision, &dummy, &nbytes);
  while (nbytes--)
    store_frame_title_char (*str++);

  /* Fill up with spaces until FIELD_WIDTH reached.  */
  while (field_width > 0
	 && n < field_width)
    {
      store_frame_title_char (' ');
      ++n;
    }

  return n;
}


/* Set the title of FRAME, if it has changed.  The title format is
   Vicon_title_format if FRAME is iconified, otherwise it is
   frame_title_format.  */

static void
x_consider_frame_title (frame)
     Lisp_Object frame;
{
  struct frame *f = XFRAME (frame);

  if (FRAME_WINDOW_P (f)
      || FRAME_MINIBUF_ONLY_P (f)
      || f->explicit_name)
    {
      /* Do we have more than one visible frame on this X display?  */
      Lisp_Object tail;
      Lisp_Object fmt;
      struct buffer *obuf;
      int len;
      struct it it;

      for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object other_frame = XCAR (tail);
	  struct frame *tf = XFRAME (other_frame);

	  if (tf != f 
	      && FRAME_KBOARD (tf) == FRAME_KBOARD (f)
	      && !FRAME_MINIBUF_ONLY_P (tf)
	      && !EQ (other_frame, tip_frame)
	      && (FRAME_VISIBLE_P (tf) || FRAME_ICONIFIED_P (tf)))
	    break;
	}

      /* Set global variable indicating that multiple frames exist.  */
      multiple_frames = CONSP (tail);

      /* Switch to the buffer of selected window of the frame.  Set up
	 frame_title_ptr so that display_mode_element will output into it;
	 then display the title.  */
      obuf = current_buffer;
      set_buffer_internal_1 (XBUFFER (XWINDOW (f->selected_window)->buffer));
      fmt = FRAME_ICONIFIED_P (f) ? Vicon_title_format : Vframe_title_format;
      frame_title_ptr = frame_title_buf;
      init_iterator (&it, XWINDOW (f->selected_window), -1, -1,
		     NULL, DEFAULT_FACE_ID);
      display_mode_element (&it, 0, -1, -1, fmt, Qnil);
      len = frame_title_ptr - frame_title_buf;
      frame_title_ptr = NULL;
      set_buffer_internal_1 (obuf);

      /* Set the title only if it's changed.  This avoids consing in
	 the common case where it hasn't.  (If it turns out that we've
	 already wasted too much time by walking through the list with
	 display_mode_element, then we might need to optimize at a
	 higher level than this.)  */
      if (! STRINGP (f->name) 
	  || SBYTES (f->name) != len
	  || bcmp (frame_title_buf, SDATA (f->name), len) != 0)
	x_implicitly_set_name (f, make_string (frame_title_buf, len), Qnil);
    }
}

#else /* not HAVE_WINDOW_SYSTEM */

#define frame_title_ptr ((char *)0)
#define store_frame_title(str, mincol, maxcol) 0

#endif /* not HAVE_WINDOW_SYSTEM */




/***********************************************************************
			      Menu Bars
 ***********************************************************************/


/* Prepare for redisplay by updating menu-bar item lists when
   appropriate.  This can call eval.  */

void
prepare_menu_bars ()
{
  int all_windows;
  struct gcpro gcpro1, gcpro2;
  struct frame *f;
  Lisp_Object tooltip_frame;

#ifdef HAVE_WINDOW_SYSTEM
  tooltip_frame = tip_frame;
#else
  tooltip_frame = Qnil;
#endif

  /* Update all frame titles based on their buffer names, etc.  We do
     this before the menu bars so that the buffer-menu will show the
     up-to-date frame titles.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (windows_or_buffers_changed || update_mode_lines)
    {
      Lisp_Object tail, frame;

      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);
	  if (!EQ (frame, tooltip_frame)
	      && (FRAME_VISIBLE_P (f) || FRAME_ICONIFIED_P (f)))
	    x_consider_frame_title (frame);
	}
    }
#endif /* HAVE_WINDOW_SYSTEM */

  /* Update the menu bar item lists, if appropriate.  This has to be
     done before any actual redisplay or generation of display lines.  */
  all_windows = (update_mode_lines 
		 || buffer_shared > 1
		 || windows_or_buffers_changed);
  if (all_windows)
    {
      Lisp_Object tail, frame;
      int count = BINDING_STACK_SIZE ();

      record_unwind_protect (Fset_match_data, Fmatch_data (Qnil, Qnil));

      FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);

	  /* Ignore tooltip frame.  */
	  if (EQ (frame, tooltip_frame))
	    continue;
	  
	  /* If a window on this frame changed size, report that to
	     the user and clear the size-change flag.  */
	  if (FRAME_WINDOW_SIZES_CHANGED (f))
	    {
	      Lisp_Object functions;
	      
	      /* Clear flag first in case we get an error below.  */
	      FRAME_WINDOW_SIZES_CHANGED (f) = 0;
	      functions = Vwindow_size_change_functions;
	      GCPRO2 (tail, functions);
	      
	      while (CONSP (functions))
		{
		  call1 (XCAR (functions), frame);
		  functions = XCDR (functions);
		}
	      UNGCPRO;
	    }
	  
	  GCPRO1 (tail);
	  update_menu_bar (f, 0);
#ifdef HAVE_WINDOW_SYSTEM
	  update_tool_bar (f, 0);
#endif
	  UNGCPRO;
	}

      unbind_to (count, Qnil);
    }
  else
    {
      struct frame *sf = SELECTED_FRAME ();
      update_menu_bar (sf, 1);
#ifdef HAVE_WINDOW_SYSTEM
      update_tool_bar (sf, 1);
#endif
    }

  /* Motif needs this.  See comment in xmenu.c.  Turn it off when
     pending_menu_activation is not defined.  */
#ifdef USE_X_TOOLKIT
  pending_menu_activation = 0;
#endif
}


/* Update the menu bar item list for frame F.  This has to be done
   before we start to fill in any display lines, because it can call
   eval.

   If SAVE_MATCH_DATA is non-zero, we must save and restore it here.  */

static void
update_menu_bar (f, save_match_data)
     struct frame *f;
     int save_match_data;
{
  Lisp_Object window;
  register struct window *w;

  /* If called recursively during a menu update, do nothing.  This can
     happen when, for instance, an activate-menubar-hook causes a
     redisplay.  */
  if (inhibit_menubar_update)
    return;

  window = FRAME_SELECTED_WINDOW (f);
  w = XWINDOW (window);
  
#if 0 /* The if statement below this if statement used to include the
         condition !NILP (w->update_mode_line), rather than using
         update_mode_lines directly, and this if statement may have
         been added to make that condition work.  Now the if
         statement below matches its comment, this isn't needed.  */  
  if (update_mode_lines)
    w->update_mode_line = Qt;
#endif

  if (FRAME_WINDOW_P (f)
      ?
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) || defined (macintosh)
      FRAME_EXTERNAL_MENU_BAR (f) 
#else
      FRAME_MENU_BAR_LINES (f) > 0
#endif
      : FRAME_MENU_BAR_LINES (f) > 0)
    {
      /* If the user has switched buffers or windows, we need to
	 recompute to reflect the new bindings.  But we'll
	 recompute when update_mode_lines is set too; that means
	 that people can use force-mode-line-update to request
	 that the menu bar be recomputed.  The adverse effect on
	 the rest of the redisplay algorithm is about the same as
	 windows_or_buffers_changed anyway.  */
      if (windows_or_buffers_changed
	  /* This used to test w->update_mode_line, but we believe
	     there is no need to recompute the menu in that case.  */
	  || update_mode_lines
	  || ((BUF_SAVE_MODIFF (XBUFFER (w->buffer))
	       < BUF_MODIFF (XBUFFER (w->buffer)))
	      != !NILP (w->last_had_star))
	  || ((!NILP (Vtransient_mark_mode)
	       && !NILP (XBUFFER (w->buffer)->mark_active))
	      != !NILP (w->region_showing)))
	{
	  struct buffer *prev = current_buffer;
	  int count = BINDING_STACK_SIZE ();

	  specbind (Qinhibit_menubar_update, Qt);

	  set_buffer_internal_1 (XBUFFER (w->buffer));
	  if (save_match_data)
	    record_unwind_protect (Fset_match_data, Fmatch_data (Qnil, Qnil));
	  if (NILP (Voverriding_local_map_menu_flag))
	    {
	      specbind (Qoverriding_terminal_local_map, Qnil);
	      specbind (Qoverriding_local_map, Qnil);
	    }

	  /* Run the Lucid hook.  */
	  safe_run_hooks (Qactivate_menubar_hook);
	  
	  /* If it has changed current-menubar from previous value,
	     really recompute the menu-bar from the value.  */
	  if (! NILP (Vlucid_menu_bar_dirty_flag))
	    call0 (Qrecompute_lucid_menubar);
	  
	  safe_run_hooks (Qmenu_bar_update_hook);
	  FRAME_MENU_BAR_ITEMS (f) = menu_bar_items (FRAME_MENU_BAR_ITEMS (f));
	  
	  /* Redisplay the menu bar in case we changed it.  */
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) || defined (macintosh)
	  if (FRAME_WINDOW_P (f)
#if defined (macintosh)
              /* All frames on Mac OS share the same menubar.  So only the
                 selected frame should be allowed to set it.  */
              && f == SELECTED_FRAME ()
#endif
	     )
	    set_frame_menubar (f, 0, 0);
	  else
	    /* On a terminal screen, the menu bar is an ordinary screen
	       line, and this makes it get updated.  */
	    w->update_mode_line = Qt;
#else /* ! (USE_X_TOOLKIT || HAVE_NTGUI) */
	  /* In the non-toolkit version, the menu bar is an ordinary screen
	     line, and this makes it get updated.  */
	  w->update_mode_line = Qt;
#endif /* ! (USE_X_TOOLKIT || HAVE_NTGUI) */

	  unbind_to (count, Qnil);
	  set_buffer_internal_1 (prev);
	}
    }
}



/***********************************************************************
			       Tool-bars
 ***********************************************************************/

#ifdef HAVE_WINDOW_SYSTEM

/* Update the tool-bar item list for frame F.  This has to be done
   before we start to fill in any display lines.  Called from
   prepare_menu_bars.  If SAVE_MATCH_DATA is non-zero, we must save
   and restore it here.  */

static void
update_tool_bar (f, save_match_data)
     struct frame *f;
     int save_match_data;
{
  if (WINDOWP (f->tool_bar_window)
      && XFASTINT (XWINDOW (f->tool_bar_window)->height) > 0)
    {
      Lisp_Object window;
      struct window *w;

      window = FRAME_SELECTED_WINDOW (f);
      w = XWINDOW (window);
  
      /* If the user has switched buffers or windows, we need to
	 recompute to reflect the new bindings.  But we'll
	 recompute when update_mode_lines is set too; that means
	 that people can use force-mode-line-update to request
	 that the menu bar be recomputed.  The adverse effect on
	 the rest of the redisplay algorithm is about the same as
	 windows_or_buffers_changed anyway.  */
      if (windows_or_buffers_changed
	  || !NILP (w->update_mode_line)
	  || ((BUF_SAVE_MODIFF (XBUFFER (w->buffer))
	       < BUF_MODIFF (XBUFFER (w->buffer)))
	      != !NILP (w->last_had_star))
	  || ((!NILP (Vtransient_mark_mode)
	       && !NILP (XBUFFER (w->buffer)->mark_active))
	      != !NILP (w->region_showing)))
	{
	  struct buffer *prev = current_buffer;
	  int count = BINDING_STACK_SIZE ();

	  /* Set current_buffer to the buffer of the selected
	     window of the frame, so that we get the right local
	     keymaps.  */
	  set_buffer_internal_1 (XBUFFER (w->buffer));

	  /* Save match data, if we must.  */
	  if (save_match_data)
	    record_unwind_protect (Fset_match_data, Fmatch_data (Qnil, Qnil));

	  /* Make sure that we don't accidentally use bogus keymaps.  */
	  if (NILP (Voverriding_local_map_menu_flag))
	    {
	      specbind (Qoverriding_terminal_local_map, Qnil);
	      specbind (Qoverriding_local_map, Qnil);
	    }

	  /* Build desired tool-bar items from keymaps.  */
	  f->tool_bar_items
	    = tool_bar_items (f->tool_bar_items, &f->n_tool_bar_items);
	  
	  /* Redisplay the tool-bar in case we changed it.  */
	  w->update_mode_line = Qt;

	  unbind_to (count, Qnil);
	  set_buffer_internal_1 (prev);
	}
    }
}


/* Set F->desired_tool_bar_string to a Lisp string representing frame
   F's desired tool-bar contents.  F->tool_bar_items must have
   been set up previously by calling prepare_menu_bars.  */

static void
build_desired_tool_bar_string (f)
     struct frame *f;
{
  int i, size, size_needed;
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object image, plist, props;

  image = plist = props = Qnil;
  GCPRO3 (image, plist, props);

  /* Prepare F->desired_tool_bar_string.  If we can reuse it, do so.
     Otherwise, make a new string.  */
  
  /* The size of the string we might be able to reuse.  */
  size = (STRINGP (f->desired_tool_bar_string)
	  ? SCHARS (f->desired_tool_bar_string)
	  : 0);

  /* We need one space in the string for each image.  */
  size_needed = f->n_tool_bar_items;
  
  /* Reuse f->desired_tool_bar_string, if possible.  */
  if (size < size_needed || NILP (f->desired_tool_bar_string))
    f->desired_tool_bar_string = Fmake_string (make_number (size_needed),
					       make_number (' '));
  else
    {
      props = list4 (Qdisplay, Qnil, Qmenu_item, Qnil);
      Fremove_text_properties (make_number (0), make_number (size),
			       props, f->desired_tool_bar_string);
    }

  /* Put a `display' property on the string for the images to display,
     put a `menu_item' property on tool-bar items with a value that
     is the index of the item in F's tool-bar item vector.  */
  for (i = 0; i < f->n_tool_bar_items; ++i)
    {
#define PROP(IDX) AREF (f->tool_bar_items, i * TOOL_BAR_ITEM_NSLOTS + (IDX))

      int enabled_p = !NILP (PROP (TOOL_BAR_ITEM_ENABLED_P));
      int selected_p = !NILP (PROP (TOOL_BAR_ITEM_SELECTED_P));
      int hmargin, vmargin, relief, idx, end;
      extern Lisp_Object QCrelief, QCmargin, QCconversion, Qimage;

      /* If image is a vector, choose the image according to the
	 button state.  */
      image = PROP (TOOL_BAR_ITEM_IMAGES);
      if (VECTORP (image))
	{
	  if (enabled_p)
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_ENABLED_SELECTED
		   : TOOL_BAR_IMAGE_ENABLED_DESELECTED);
	  else
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_DISABLED_SELECTED
		   : TOOL_BAR_IMAGE_DISABLED_DESELECTED);
	  
	  xassert (ASIZE (image) >= idx);
	  image = AREF (image, idx);
	}
      else
	idx = -1;

      /* Ignore invalid image specifications.  */
      if (!valid_image_p (image))
	continue;

      /* Display the tool-bar button pressed, or depressed.  */
      plist = Fcopy_sequence (XCDR (image));

      /* Compute margin and relief to draw.  */
      relief = (tool_bar_button_relief >= 0
		? tool_bar_button_relief
		: DEFAULT_TOOL_BAR_BUTTON_RELIEF);
      hmargin = vmargin = relief;

      if (INTEGERP (Vtool_bar_button_margin)
	  && XINT (Vtool_bar_button_margin) > 0)
	{
	  hmargin += XFASTINT (Vtool_bar_button_margin);
	  vmargin += XFASTINT (Vtool_bar_button_margin);
	}
      else if (CONSP (Vtool_bar_button_margin))
	{
	  if (INTEGERP (XCAR (Vtool_bar_button_margin))
	      && XINT (XCAR (Vtool_bar_button_margin)) > 0)
	    hmargin += XFASTINT (XCAR (Vtool_bar_button_margin));
	  
	  if (INTEGERP (XCDR (Vtool_bar_button_margin))
	      && XINT (XCDR (Vtool_bar_button_margin)) > 0)
	    vmargin += XFASTINT (XCDR (Vtool_bar_button_margin));
	}
      
      if (auto_raise_tool_bar_buttons_p)
	{
	  /* Add a `:relief' property to the image spec if the item is
	     selected.  */
	  if (selected_p)
	    {
	      plist = Fplist_put (plist, QCrelief, make_number (-relief));
	      hmargin -= relief;
	      vmargin -= relief;
	    }
	}
      else
	{
	  /* If image is selected, display it pressed, i.e. with a
	     negative relief.  If it's not selected, display it with a
	     raised relief.  */
	  plist = Fplist_put (plist, QCrelief,
			      (selected_p
			       ? make_number (-relief)
			       : make_number (relief)));
	  hmargin -= relief;
	  vmargin -= relief;
	}

      /* Put a margin around the image.  */
      if (hmargin || vmargin)
	{
	  if (hmargin == vmargin)
	    plist = Fplist_put (plist, QCmargin, make_number (hmargin));
	  else
	    plist = Fplist_put (plist, QCmargin,
				Fcons (make_number (hmargin),
				       make_number (vmargin)));
	}
	  
      /* If button is not enabled, and we don't have special images
	 for the disabled state, make the image appear disabled by
	 applying an appropriate algorithm to it.  */
      if (!enabled_p && idx < 0)
	plist = Fplist_put (plist, QCconversion, Qdisabled);
      
      /* Put a `display' text property on the string for the image to
	 display.  Put a `menu-item' property on the string that gives
	 the start of this item's properties in the tool-bar items
	 vector.  */
      image = Fcons (Qimage, plist);
      props = list4 (Qdisplay, image,
		     Qmenu_item, make_number (i * TOOL_BAR_ITEM_NSLOTS));

      /* Let the last image hide all remaining spaces in the tool bar
         string.  The string can be longer than needed when we reuse a
         previous string.  */
      if (i + 1 == f->n_tool_bar_items)
	end = SCHARS (f->desired_tool_bar_string);
      else
	end = i + 1;
      Fadd_text_properties (make_number (i), make_number (end),
			    props, f->desired_tool_bar_string);
#undef PROP
    }

  UNGCPRO;
}


/* Display one line of the tool-bar of frame IT->f.  */

static void
display_tool_bar_line (it)
     struct it *it;
{
  struct glyph_row *row = it->glyph_row;
  int max_x = it->last_visible_x;
  struct glyph *last;
  
  prepare_desired_row (row);
  row->y = it->current_y;

  /* Note that this isn't made use of if the face hasn't a box,
     so there's no need to check the face here.  */
  it->start_of_box_run_p = 1;
  
  while (it->current_x < max_x)
    {
      int x_before, x, n_glyphs_before, i, nglyphs;

      /* Get the next display element.  */
      if (!get_next_display_element (it))
	break;

      /* Produce glyphs.  */
      x_before = it->current_x;
      n_glyphs_before = it->glyph_row->used[TEXT_AREA];
      PRODUCE_GLYPHS (it);

      nglyphs = it->glyph_row->used[TEXT_AREA] - n_glyphs_before;
      i = 0;
      x = x_before;
      while (i < nglyphs)
	{
	  struct glyph *glyph = row->glyphs[TEXT_AREA] + n_glyphs_before + i;
	  
	  if (x + glyph->pixel_width > max_x)
	    {
	      /* Glyph doesn't fit on line.  */
	      it->glyph_row->used[TEXT_AREA] = n_glyphs_before + i;
	      it->current_x = x;
	      goto out;
	    }

	  ++it->hpos;
	  x += glyph->pixel_width;
	  ++i;
	}

      /* Stop at line ends.  */
      if (ITERATOR_AT_END_OF_LINE_P (it))
	break;

      set_iterator_to_next (it, 1);
    }

 out:;

  row->displays_text_p = row->used[TEXT_AREA] != 0;
  extend_face_to_end_of_line (it);
  last = row->glyphs[TEXT_AREA] + row->used[TEXT_AREA] - 1;
  last->right_box_line_p = 1;
  if (last == row->glyphs[TEXT_AREA])
    last->left_box_line_p = 1;
  compute_line_metrics (it);
  
  /* If line is empty, make it occupy the rest of the tool-bar.  */
  if (!row->displays_text_p)
    {
      row->height = row->phys_height = it->last_visible_y - row->y;
      row->ascent = row->phys_ascent = 0;
    }
  
  row->full_width_p = 1;
  row->continued_p = 0;
  row->truncated_on_left_p = 0;
  row->truncated_on_right_p = 0;

  it->current_x = it->hpos = 0;
  it->current_y += row->height;
  ++it->vpos;
  ++it->glyph_row;
}


/* Value is the number of screen lines needed to make all tool-bar
   items of frame F visible.  */

static int
tool_bar_lines_needed (f)
     struct frame *f;
{
  struct window *w = XWINDOW (f->tool_bar_window);
  struct it it;
  
  /* Initialize an iterator for iteration over
     F->desired_tool_bar_string in the tool-bar window of frame F.  */
  init_iterator (&it, w, -1, -1, w->desired_matrix->rows, TOOL_BAR_FACE_ID);
  it.first_visible_x = 0;
  it.last_visible_x = FRAME_WINDOW_WIDTH (f) * CANON_X_UNIT (f);
  reseat_to_string (&it, NULL, f->desired_tool_bar_string, 0, 0, 0, -1);

  while (!ITERATOR_AT_END_P (&it))
    {
      it.glyph_row = w->desired_matrix->rows;
      clear_glyph_row (it.glyph_row);
      display_tool_bar_line (&it);
    }

  return (it.current_y + CANON_Y_UNIT (f) - 1) / CANON_Y_UNIT (f);
}


DEFUN ("tool-bar-lines-needed", Ftool_bar_lines_needed, Stool_bar_lines_needed,
       0, 1, 0,
       doc: /* Return the number of lines occupied by the tool bar of FRAME.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f;
  struct window *w;
  int nlines = 0;

  if (NILP (frame))
    frame = selected_frame;
  else
    CHECK_FRAME (frame);
  f = XFRAME (frame);
  
  if (WINDOWP (f->tool_bar_window)
      || (w = XWINDOW (f->tool_bar_window),
	  XFASTINT (w->height) > 0))
    {
      update_tool_bar (f, 1);
      if (f->n_tool_bar_items)
	{
	  build_desired_tool_bar_string (f);
	  nlines = tool_bar_lines_needed (f);
	}
    }

  return make_number (nlines);
}


/* Display the tool-bar of frame F.  Value is non-zero if tool-bar's
   height should be changed.  */

static int
redisplay_tool_bar (f)
     struct frame *f;
{
  struct window *w;
  struct it it;
  struct glyph_row *row;
  int change_height_p = 0;
  
  /* If frame hasn't a tool-bar window or if it is zero-height, don't
     do anything.  This means you must start with tool-bar-lines
     non-zero to get the auto-sizing effect.  Or in other words, you
     can turn off tool-bars by specifying tool-bar-lines zero.  */
  if (!WINDOWP (f->tool_bar_window)
      || (w = XWINDOW (f->tool_bar_window),
	  XFASTINT (w->height) == 0))
    return 0;

  /* Set up an iterator for the tool-bar window.  */
  init_iterator (&it, w, -1, -1, w->desired_matrix->rows, TOOL_BAR_FACE_ID);
  it.first_visible_x = 0;
  it.last_visible_x = FRAME_WINDOW_WIDTH (f) * CANON_X_UNIT (f);
  row = it.glyph_row;

  /* Build a string that represents the contents of the tool-bar.  */
  build_desired_tool_bar_string (f);
  reseat_to_string (&it, NULL, f->desired_tool_bar_string, 0, 0, 0, -1);

  /* Display as many lines as needed to display all tool-bar items.  */
  while (it.current_y < it.last_visible_y)
    display_tool_bar_line (&it);

  /* It doesn't make much sense to try scrolling in the tool-bar
     window, so don't do it.  */
  w->desired_matrix->no_scrolling_p = 1;
  w->must_be_updated_p = 1;

  if (auto_resize_tool_bars_p)
    {
      int nlines;

      /* If we couldn't display everything, change the tool-bar's
	 height.  */
      if (IT_STRING_CHARPOS (it) < it.end_charpos)
	change_height_p = 1;
      
      /* If there are blank lines at the end, except for a partially
	 visible blank line at the end that is smaller than
	 CANON_Y_UNIT, change the tool-bar's height.  */
      row = it.glyph_row - 1;
      if (!row->displays_text_p
	  && row->height >= CANON_Y_UNIT (f))
	change_height_p = 1;

      /* If row displays tool-bar items, but is partially visible,
	 change the tool-bar's height.  */
      if (row->displays_text_p
	  && MATRIX_ROW_BOTTOM_Y (row) > it.last_visible_y)
	change_height_p = 1;

      /* Resize windows as needed by changing the `tool-bar-lines'
	 frame parameter.  */
      if (change_height_p
	  && (nlines = tool_bar_lines_needed (f),
	      nlines != XFASTINT (w->height)))
	{
	  extern Lisp_Object Qtool_bar_lines;
	  Lisp_Object frame;
	  int old_height = XFASTINT (w->height);
	  
	  XSETFRAME (frame, f);
	  clear_glyph_matrix (w->desired_matrix);
	  Fmodify_frame_parameters (frame,
				    Fcons (Fcons (Qtool_bar_lines,
						  make_number (nlines)),
					   Qnil));
	  if (XFASTINT (w->height) != old_height)
	    fonts_changed_p = 1;
	}
    }

  return change_height_p;
}


/* Get information about the tool-bar item which is displayed in GLYPH
   on frame F.  Return in *PROP_IDX the index where tool-bar item
   properties start in F->tool_bar_items.  Value is zero if
   GLYPH doesn't display a tool-bar item.  */

int
tool_bar_item_info (f, glyph, prop_idx)
     struct frame *f;
     struct glyph *glyph;
     int *prop_idx;
{
  Lisp_Object prop;
  int success_p;
  int charpos;

  /* This function can be called asynchronously, which means we must
     exclude any possibility that Fget_text_property signals an
     error.  */
  charpos = min (SCHARS (f->current_tool_bar_string), glyph->charpos);
  charpos = max (0, charpos);
  
  /* Get the text property `menu-item' at pos. The value of that
     property is the start index of this item's properties in
     F->tool_bar_items.  */
  prop = Fget_text_property (make_number (charpos),
			     Qmenu_item, f->current_tool_bar_string);
  if (INTEGERP (prop))
    {
      *prop_idx = XINT (prop);
      success_p = 1;
    }
  else
    success_p = 0;

  return success_p;
}
  
#endif /* HAVE_WINDOW_SYSTEM */



/************************************************************************
			 Horizontal scrolling
 ************************************************************************/

static int hscroll_window_tree P_ ((Lisp_Object));
static int hscroll_windows P_ ((Lisp_Object));

/* For all leaf windows in the window tree rooted at WINDOW, set their
   hscroll value so that PT is (i) visible in the window, and (ii) so
   that it is not within a certain margin at the window's left and
   right border.  Value is non-zero if any window's hscroll has been
   changed.  */

static int
hscroll_window_tree (window)
     Lisp_Object window;
{
  int hscrolled_p = 0;
  int hscroll_relative_p = FLOATP (Vhscroll_step);
  int hscroll_step_abs = 0;
  double hscroll_step_rel = 0;

  if (hscroll_relative_p)
    {
      hscroll_step_rel = XFLOAT_DATA (Vhscroll_step);
      if (hscroll_step_rel < 0)
	{
	  hscroll_relative_p = 0;
	  hscroll_step_abs = 0;
	}
    }
  else if (INTEGERP (Vhscroll_step))
    {
      hscroll_step_abs = XINT (Vhscroll_step);
      if (hscroll_step_abs < 0)
	hscroll_step_abs = 0;
    }
  else
    hscroll_step_abs = 0;

  while (WINDOWP (window))
    {
      struct window *w = XWINDOW (window);

      if (WINDOWP (w->hchild))
	hscrolled_p |= hscroll_window_tree (w->hchild);
      else if (WINDOWP (w->vchild))
	hscrolled_p |= hscroll_window_tree (w->vchild);
      else if (w->cursor.vpos >= 0)
	{
	  int h_margin, text_area_x, text_area_y;
	  int text_area_width, text_area_height;
	  struct glyph_row *current_cursor_row
	    = MATRIX_ROW (w->current_matrix, w->cursor.vpos);
	  struct glyph_row *desired_cursor_row
	    = MATRIX_ROW (w->desired_matrix, w->cursor.vpos);
	  struct glyph_row *cursor_row
	    = (desired_cursor_row->enabled_p
	       ? desired_cursor_row
	       : current_cursor_row);

	  window_box (w, TEXT_AREA, &text_area_x, &text_area_y,
		      &text_area_width, &text_area_height);

	  /* Scroll when cursor is inside this scroll margin.  */
	  h_margin = hscroll_margin * CANON_X_UNIT (XFRAME (w->frame));

	  if ((XFASTINT (w->hscroll)
	       && w->cursor.x <= h_margin)
	      || (cursor_row->enabled_p
		  && cursor_row->truncated_on_right_p
		  && (w->cursor.x >= text_area_width - h_margin)))
	    {
	      struct it it;
	      int hscroll;
	      struct buffer *saved_current_buffer;
	      int pt;
	      int wanted_x;

	      /* Find point in a display of infinite width.  */
	      saved_current_buffer = current_buffer;
	      current_buffer = XBUFFER (w->buffer);

	      if (w == XWINDOW (selected_window))
		pt = BUF_PT (current_buffer);
	      else
		{
		  pt = marker_position (w->pointm);
		  pt = max (BEGV, pt);
		  pt = min (ZV, pt);
		}

	      /* Move iterator to pt starting at cursor_row->start in
		 a line with infinite width.  */
	      init_to_row_start (&it, w, cursor_row);
	      it.last_visible_x = INFINITY;
	      move_it_in_display_line_to (&it, pt, -1, MOVE_TO_POS);
	      current_buffer = saved_current_buffer;

	      /* Position cursor in window.  */
	      if (!hscroll_relative_p && hscroll_step_abs == 0)
		hscroll = max (0, it.current_x - text_area_width / 2)
		    	  / CANON_X_UNIT (it.f);
	      else if (w->cursor.x >= text_area_width - h_margin)
		{
		  if (hscroll_relative_p)
		    wanted_x = text_area_width * (1 - hscroll_step_rel)
		      	       - h_margin;
		  else
		    wanted_x = text_area_width
		      	       - hscroll_step_abs * CANON_X_UNIT (it.f)
		      	       - h_margin;
		  hscroll
		    = max (0, it.current_x - wanted_x) / CANON_X_UNIT (it.f);
		}
	      else
		{
		  if (hscroll_relative_p)
		    wanted_x = text_area_width * hscroll_step_rel
		      	       + h_margin;
		  else
		    wanted_x = hscroll_step_abs * CANON_X_UNIT (it.f)
		      	       + h_margin;
		  hscroll
		    = max (0, it.current_x - wanted_x) / CANON_X_UNIT (it.f);
		}
	      hscroll = max (hscroll, XFASTINT (w->min_hscroll));

	      /* Don't call Fset_window_hscroll if value hasn't
		 changed because it will prevent redisplay
		 optimizations.  */
	      if (XFASTINT (w->hscroll) != hscroll)
		{
		  XBUFFER (w->buffer)->prevent_redisplay_optimizations_p = 1;
		  w->hscroll = make_number (hscroll);
		  hscrolled_p = 1;
		}
	    }
	}

      window = w->next;
    }

  /* Value is non-zero if hscroll of any leaf window has been changed.  */
  return hscrolled_p;
}


/* Set hscroll so that cursor is visible and not inside horizontal
   scroll margins for all windows in the tree rooted at WINDOW.  See
   also hscroll_window_tree above.  Value is non-zero if any window's
   hscroll has been changed.  If it has, desired matrices on the frame
   of WINDOW are cleared.  */

static int
hscroll_windows (window)
     Lisp_Object window;
{
  int hscrolled_p;
  
  if (automatic_hscrolling_p)
    {
      hscrolled_p = hscroll_window_tree (window);
      if (hscrolled_p)
	clear_desired_matrices (XFRAME (WINDOW_FRAME (XWINDOW (window))));
    }
  else
    hscrolled_p = 0;
  return hscrolled_p;
}



/************************************************************************
				Redisplay
 ************************************************************************/

/* Variables holding some state of redisplay if GLYPH_DEBUG is defined
   to a non-zero value.  This is sometimes handy to have in a debugger
   session.  */

#if GLYPH_DEBUG

/* First and last unchanged row for try_window_id.  */

int debug_first_unchanged_at_end_vpos;
int debug_last_unchanged_at_beg_vpos;

/* Delta vpos and y.  */

int debug_dvpos, debug_dy;

/* Delta in characters and bytes for try_window_id.  */

int debug_delta, debug_delta_bytes;

/* Values of window_end_pos and window_end_vpos at the end of
   try_window_id.  */

EMACS_INT debug_end_pos, debug_end_vpos;

/* Append a string to W->desired_matrix->method.  FMT is a printf
   format string.  A1...A9 are a supplement for a variable-length
   argument list.  If trace_redisplay_p is non-zero also printf the
   resulting string to stderr.  */

static void
debug_method_add (w, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
     struct window *w;
     char *fmt;
     int a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
  char buffer[512];
  char *method = w->desired_matrix->method;
  int len = strlen (method);
  int size = sizeof w->desired_matrix->method;
  int remaining = size - len - 1;

  sprintf (buffer, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
  if (len && remaining)
    {
      method[len] = '|';
      --remaining, ++len;
    }
  
  strncpy (method + len, buffer, remaining);

  if (trace_redisplay_p)
    fprintf (stderr, "%p (%s): %s\n",
	     w,
	     ((BUFFERP (w->buffer)
	       && STRINGP (XBUFFER (w->buffer)->name))
	      ? (char *) SDATA (XBUFFER (w->buffer)->name)
	      : "no buffer"),
	     buffer);
}

#endif /* GLYPH_DEBUG */


/* This counter is used to clear the face cache every once in a while
   in redisplay_internal.  It is incremented for each redisplay.
   Every CLEAR_FACE_CACHE_COUNT full redisplays, the face cache is
   cleared.  */

#define CLEAR_FACE_CACHE_COUNT	500
static int clear_face_cache_count;

/* Record the previous terminal frame we displayed.  */

static struct frame *previous_terminal_frame;

/* Non-zero while redisplay_internal is in progress.  */

int redisplaying_p;


/* Value is non-zero if all changes in window W, which displays
   current_buffer, are in the text between START and END.  START is a
   buffer position, END is given as a distance from Z.  Used in
   redisplay_internal for display optimization.  */

static INLINE int
text_outside_line_unchanged_p (w, start, end)
     struct window *w;
     int start, end;
{
  int unchanged_p = 1;
  
  /* If text or overlays have changed, see where.  */
  if (XFASTINT (w->last_modified) < MODIFF
      || XFASTINT (w->last_overlay_modified) < OVERLAY_MODIFF)
    {
      /* Gap in the line?  */
      if (GPT < start || Z - GPT < end)
	unchanged_p = 0;

      /* Changes start in front of the line, or end after it?  */
      if (unchanged_p
	  && (BEG_UNCHANGED < start - 1
	      || END_UNCHANGED < end))
	unchanged_p = 0;
      
      /* If selective display, can't optimize if changes start at the
	 beginning of the line.  */
      if (unchanged_p
	  && INTEGERP (current_buffer->selective_display)
	  && XINT (current_buffer->selective_display) > 0
	  && (BEG_UNCHANGED < start || GPT <= start))
	unchanged_p = 0;

      /* If there are overlays at the start or end of the line, these
	 may have overlay strings with newlines in them.  A change at
	 START, for instance, may actually concern the display of such
	 overlay strings as well, and they are displayed on different
	 lines.  So, quickly rule out this case.  (For the future, it
	 might be desirable to implement something more telling than
	 just BEG/END_UNCHANGED.)  */
      if (unchanged_p)
	{
	  if (BEG + BEG_UNCHANGED == start
	      && overlay_touches_p (start))
	    unchanged_p = 0;
	  if (END_UNCHANGED == end
	      && overlay_touches_p (Z - end))
	    unchanged_p = 0;
	}
    }

  return unchanged_p;
}


/* Do a frame update, taking possible shortcuts into account.  This is
   the main external entry point for redisplay.

   If the last redisplay displayed an echo area message and that message
   is no longer requested, we clear the echo area or bring back the
   mini-buffer if that is in use.  */

void
redisplay ()
{
  redisplay_internal (0);
}


/* Return 1 if point moved out of or into a composition.  Otherwise
   return 0.  PREV_BUF and PREV_PT are the last point buffer and
   position.  BUF and PT are the current point buffer and position.  */

int
check_point_in_composition (prev_buf, prev_pt, buf, pt)
     struct buffer *prev_buf, *buf;
     int prev_pt, pt;
{
  int start, end;
  Lisp_Object prop;
  Lisp_Object buffer;

  XSETBUFFER (buffer, buf);
  /* Check a composition at the last point if point moved within the
     same buffer.  */
  if (prev_buf == buf)
    {
      if (prev_pt == pt)
	/* Point didn't move.  */
	return 0;
    
      if (prev_pt > BUF_BEGV (buf) && prev_pt < BUF_ZV (buf)
	  && find_composition (prev_pt, -1, &start, &end, &prop, buffer)
	  && COMPOSITION_VALID_P (start, end, prop)
	  && start < prev_pt && end > prev_pt)
	/* The last point was within the composition.  Return 1 iff
            point moved out of the composition.  */
	return (pt <= start || pt >= end);
    }

  /* Check a composition at the current point.  */
  return (pt > BUF_BEGV (buf) && pt < BUF_ZV (buf)
	  && find_composition (pt, -1, &start, &end, &prop, buffer)
	  && COMPOSITION_VALID_P (start, end, prop)
	  && start < pt && end > pt);
}


/* Reconsider the setting of B->clip_changed which is displayed
   in window W.  */

static INLINE void
reconsider_clip_changes (w, b)
     struct window *w;
     struct buffer *b;
{
  if (b->prevent_redisplay_optimizations_p)
    b->clip_changed = 1;
  else if (b->clip_changed
	   && !NILP (w->window_end_valid)
	   && w->current_matrix->buffer == b
	   && w->current_matrix->zv == BUF_ZV (b)
	   && w->current_matrix->begv == BUF_BEGV (b))
    b->clip_changed = 0;

  /* If display wasn't paused, and W is not a tool bar window, see if
     point has been moved into or out of a composition.  In that case,
     we set b->clip_changed to 1 to force updating the screen.  If
     b->clip_changed has already been set to 1, we can skip this
     check.  */
  if (!b->clip_changed
      && BUFFERP (w->buffer) && !NILP (w->window_end_valid))
    {
      int pt;

      if (w == XWINDOW (selected_window))
	pt = BUF_PT (current_buffer);
      else
	pt = marker_position (w->pointm);

      if ((w->current_matrix->buffer != XBUFFER (w->buffer)
	   || pt != XINT (w->last_point))
	  && check_point_in_composition (w->current_matrix->buffer,
					 XINT (w->last_point),
					 XBUFFER (w->buffer), pt))
	b->clip_changed = 1;
    }
}


/* If PRESERVE_ECHO_AREA is nonzero, it means this redisplay is not in
   response to any user action; therefore, we should preserve the echo
   area.  (Actually, our caller does that job.)  Perhaps in the future
   avoid recentering windows if it is not necessary; currently that
   causes some problems.  */

static void
redisplay_internal (preserve_echo_area)
     int preserve_echo_area;
{
  struct window *w = XWINDOW (selected_window);
  struct frame *f = XFRAME (w->frame);
  int pause;
  int must_finish = 0;
  struct text_pos tlbufpos, tlendpos;
  int number_of_visible_frames;
  int count;
  struct frame *sf = SELECTED_FRAME ();

  /* Non-zero means redisplay has to consider all windows on all
     frames.  Zero means, only selected_window is considered.  */
  int consider_all_windows_p;
  
  TRACE ((stderr, "redisplay_internal %d\n", redisplaying_p));

  /* No redisplay if running in batch mode or frame is not yet fully
     initialized, or redisplay is explicitly turned off by setting
     Vinhibit_redisplay.  */
  if (noninteractive
      || !NILP (Vinhibit_redisplay)
      || !f->glyphs_initialized_p)
    return;

  /* The flag redisplay_performed_directly_p is set by
     direct_output_for_insert when it already did the whole screen
     update necessary.  */
  if (redisplay_performed_directly_p)
    {
      redisplay_performed_directly_p = 0;
      if (!hscroll_windows (selected_window))
	return;
    }

#ifdef USE_X_TOOLKIT
  if (popup_activated ())
    return;
#endif

  /* I don't think this happens but let's be paranoid.  */
  if (redisplaying_p)
    return;

  /* Record a function that resets redisplaying_p to its old value
     when we leave this function.  */
  count = BINDING_STACK_SIZE ();
  record_unwind_protect (unwind_redisplay, make_number (redisplaying_p));
  ++redisplaying_p;
  
 retry:
  pause = 0;
  reconsider_clip_changes (w, current_buffer);

  /* If new fonts have been loaded that make a glyph matrix adjustment
     necessary, do it.  */
  if (fonts_changed_p)
    {
      adjust_glyphs (NULL);
      ++windows_or_buffers_changed;
      fonts_changed_p = 0;
    }

  /* If face_change_count is non-zero, init_iterator will free all
     realized faces, which includes the faces referenced from current
     matrices.  So, we can't reuse current matrices in this case.  */
  if (face_change_count)
    ++windows_or_buffers_changed;

  if (! FRAME_WINDOW_P (sf)
      && previous_terminal_frame != sf)
    {
      /* Since frames on an ASCII terminal share the same display
	 area, displaying a different frame means redisplay the whole
	 thing.  */
      windows_or_buffers_changed++;
      SET_FRAME_GARBAGED (sf);
      XSETFRAME (Vterminal_frame, sf);
    }
  previous_terminal_frame = sf;

  /* Set the visible flags for all frames.  Do this before checking
     for resized or garbaged frames; they want to know if their frames
     are visible.  See the comment in frame.h for
     FRAME_SAMPLE_VISIBILITY.  */
  {
    Lisp_Object tail, frame;

    number_of_visible_frames = 0;

    FOR_EACH_FRAME (tail, frame)
      {
	struct frame *f = XFRAME (frame);
	
	FRAME_SAMPLE_VISIBILITY (f);
	if (FRAME_VISIBLE_P (f))
	  ++number_of_visible_frames;
	clear_desired_matrices (f);
      }
  }

  /* Notice any pending interrupt request to change frame size.  */
  do_pending_window_change (1);

  /* Clear frames marked as garbaged.  */
  if (frame_garbaged)
    clear_garbaged_frames ();

  /* Build menubar and tool-bar items.  */
  prepare_menu_bars ();

  if (windows_or_buffers_changed)
    update_mode_lines++;

  /* Detect case that we need to write or remove a star in the mode line.  */
  if ((SAVE_MODIFF < MODIFF) != !NILP (w->last_had_star))
    {
      w->update_mode_line = Qt;
      if (buffer_shared > 1)
	update_mode_lines++;
    }

  /* If %c is in the mode line, update it if needed.  */
  if (!NILP (w->column_number_displayed)
      /* This alternative quickly identifies a common case
	 where no change is needed.  */
      && !(PT == XFASTINT (w->last_point)
	   && XFASTINT (w->last_modified) >= MODIFF
	   && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF)
      && XFASTINT (w->column_number_displayed) != current_column ())
    w->update_mode_line = Qt; 

  FRAME_SCROLL_BOTTOM_VPOS (XFRAME (w->frame)) = -1;

  /* The variable buffer_shared is set in redisplay_window and
     indicates that we redisplay a buffer in different windows.  See
     there.  */
  consider_all_windows_p = (update_mode_lines || buffer_shared > 1
			    || cursor_type_changed);

  /* If specs for an arrow have changed, do thorough redisplay
     to ensure we remove any arrow that should no longer exist.  */
  if (! EQ (COERCE_MARKER (Voverlay_arrow_position), last_arrow_position)
      || ! EQ (Voverlay_arrow_string, last_arrow_string))
    consider_all_windows_p = windows_or_buffers_changed = 1;

  /* Normally the message* functions will have already displayed and
     updated the echo area, but the frame may have been trashed, or
     the update may have been preempted, so display the echo area
     again here.  Checking message_cleared_p captures the case that
     the echo area should be cleared.  */
  if ((!NILP (echo_area_buffer[0]) && !display_last_displayed_message_p)
      || (!NILP (echo_area_buffer[1]) && display_last_displayed_message_p)
      || (message_cleared_p
	  && minibuf_level == 0
	  /* If the mini-window is currently selected, this means the
	     echo-area doesn't show through.  */
	  && !MINI_WINDOW_P (XWINDOW (selected_window))))
    {
      int window_height_changed_p = echo_area_display (0);
      must_finish = 1;

      /* If we don't display the current message, don't clear the
	 message_cleared_p flag, because, if we did, we wouldn't clear
	 the echo area in the next redisplay which doesn't preserve
	 the echo area.  */
      if (!display_last_displayed_message_p)
	message_cleared_p = 0;
      
      if (fonts_changed_p)
	goto retry;
      else if (window_height_changed_p)
	{
	  consider_all_windows_p = 1;
	  ++update_mode_lines;
	  ++windows_or_buffers_changed;
	  
	  /* If window configuration was changed, frames may have been
	     marked garbaged.  Clear them or we will experience
	     surprises wrt scrolling.  */
	  if (frame_garbaged)
	    clear_garbaged_frames ();
	}
    }
  else if (EQ (selected_window, minibuf_window)
	   && (current_buffer->clip_changed
	       || XFASTINT (w->last_modified) < MODIFF
	       || XFASTINT (w->last_overlay_modified) < OVERLAY_MODIFF)
	   && resize_mini_window (w, 0))
    {
      /* Resized active mini-window to fit the size of what it is
         showing if its contents might have changed.  */
      must_finish = 1;
      consider_all_windows_p = 1;
      ++windows_or_buffers_changed;
      ++update_mode_lines;
      
      /* If window configuration was changed, frames may have been
	 marked garbaged.  Clear them or we will experience
	 surprises wrt scrolling.  */
      if (frame_garbaged)
	clear_garbaged_frames ();
    }
  

  /* If showing the region, and mark has changed, we must redisplay
     the whole window.  The assignment to this_line_start_pos prevents
     the optimization directly below this if-statement.  */
  if (((!NILP (Vtransient_mark_mode)
	&& !NILP (XBUFFER (w->buffer)->mark_active))
       != !NILP (w->region_showing))
      || (!NILP (w->region_showing)
	  && !EQ (w->region_showing,
		  Fmarker_position (XBUFFER (w->buffer)->mark))))
    CHARPOS (this_line_start_pos) = 0;

  /* Optimize the case that only the line containing the cursor in the
     selected window has changed.  Variables starting with this_ are
     set in display_line and record information about the line
     containing the cursor.  */
  tlbufpos = this_line_start_pos;
  tlendpos = this_line_end_pos;
  if (!consider_all_windows_p
      && CHARPOS (tlbufpos) > 0
      && NILP (w->update_mode_line)
      && !current_buffer->clip_changed
      && FRAME_VISIBLE_P (XFRAME (w->frame))
      && !FRAME_OBSCURED_P (XFRAME (w->frame))
      /* Make sure recorded data applies to current buffer, etc.  */
      && this_line_buffer == current_buffer
      && current_buffer == XBUFFER (w->buffer)
      && NILP (w->force_start)
      /* Point must be on the line that we have info recorded about.  */
      && PT >= CHARPOS (tlbufpos)
      && PT <= Z - CHARPOS (tlendpos)
      /* All text outside that line, including its final newline,
	 must be unchanged */
      && text_outside_line_unchanged_p (w, CHARPOS (tlbufpos),
					CHARPOS (tlendpos)))
    {
      if (CHARPOS (tlbufpos) > BEGV
	  && FETCH_BYTE (BYTEPOS (tlbufpos) - 1) != '\n'
	  && (CHARPOS (tlbufpos) == ZV
	      || FETCH_BYTE (BYTEPOS (tlbufpos)) == '\n'))
	/* Former continuation line has disappeared by becoming empty */
	goto cancel;
      else if (XFASTINT (w->last_modified) < MODIFF
	       || XFASTINT (w->last_overlay_modified) < OVERLAY_MODIFF
	       || MINI_WINDOW_P (w))
	{
	  /* We have to handle the case of continuation around a
	     wide-column character (See the comment in indent.c around
	     line 885).

	     For instance, in the following case:

	     --------  Insert  --------
	     K_A_N_\\   `a'    K_A_N_a\		`X_' are wide-column chars.
	     J_I_       ==>    J_I_		`^^' are cursors.
	     ^^                ^^
	     --------          --------

	     As we have to redraw the line above, we should goto cancel.  */

	  struct it it;
	  int line_height_before = this_line_pixel_height;

	  /* Note that start_display will handle the case that the
	     line starting at tlbufpos is a continuation lines.  */
	  start_display (&it, w, tlbufpos);

	  /* Implementation note: It this still necessary?  */
	  if (it.current_x != this_line_start_x)
	    goto cancel;

	  TRACE ((stderr, "trying display optimization 1\n"));
	  w->cursor.vpos = -1;
	  overlay_arrow_seen = 0;
	  it.vpos = this_line_vpos;
	  it.current_y = this_line_y;
	  it.glyph_row = MATRIX_ROW (w->desired_matrix, this_line_vpos);
	  display_line (&it);

	  /* If line contains point, is not continued,
             and ends at same distance from eob as before, we win */
	  if (w->cursor.vpos >= 0 
              /* Line is not continued, otherwise this_line_start_pos
                 would have been set to 0 in display_line.  */
	      && CHARPOS (this_line_start_pos)
	      /* Line ends as before.  */
	      && CHARPOS (this_line_end_pos) == CHARPOS (tlendpos)
              /* Line has same height as before.  Otherwise other lines
                 would have to be shifted up or down.  */
	      && this_line_pixel_height == line_height_before)
	    {
 	      /* If this is not the window's last line, we must adjust
 		 the charstarts of the lines below.  */
 	      if (it.current_y < it.last_visible_y)
  		{
 		  struct glyph_row *row
 		    = MATRIX_ROW (w->current_matrix, this_line_vpos + 1);
  		  int delta, delta_bytes;
  
  		  if (Z - CHARPOS (tlendpos) == ZV)
		    {
		      /* This line ends at end of (accessible part of)
			 buffer.  There is no newline to count.  */
		      delta = (Z
			       - CHARPOS (tlendpos)
			       - MATRIX_ROW_START_CHARPOS (row));
		      delta_bytes = (Z_BYTE
				     - BYTEPOS (tlendpos)
				     - MATRIX_ROW_START_BYTEPOS (row));
		    }
  		  else
		    {
		      /* This line ends in a newline.  Must take
			 account of the newline and the rest of the
			 text that follows.  */
		      delta = (Z
			       - CHARPOS (tlendpos)
			       - MATRIX_ROW_START_CHARPOS (row));
		      delta_bytes = (Z_BYTE
				     - BYTEPOS (tlendpos)
				     - MATRIX_ROW_START_BYTEPOS (row));
		    }
  
  		  increment_matrix_positions (w->current_matrix,
					      this_line_vpos + 1,
					      w->current_matrix->nrows,
					      delta, delta_bytes);
		}

	      /* If this row displays text now but previously didn't,
		 or vice versa, w->window_end_vpos may have to be
		 adjusted.  */
	      if ((it.glyph_row - 1)->displays_text_p)
		{
		  if (XFASTINT (w->window_end_vpos) < this_line_vpos)
		    XSETINT (w->window_end_vpos, this_line_vpos);
		}
	      else if (XFASTINT (w->window_end_vpos) == this_line_vpos
		       && this_line_vpos > 0)
		XSETINT (w->window_end_vpos, this_line_vpos - 1);
	      w->window_end_valid = Qnil;
	      
	      /* Update hint: No need to try to scroll in update_window.  */
	      w->desired_matrix->no_scrolling_p = 1;

#if GLYPH_DEBUG
	      *w->desired_matrix->method = 0;
	      debug_method_add (w, "optimization 1");
#endif
	      goto update;
	    }
	  else
	    goto cancel;
	}
      else if (/* Cursor position hasn't changed.  */
	       PT == XFASTINT (w->last_point)
	       /* Make sure the cursor was last displayed
		  in this window.  Otherwise we have to reposition it.  */
	       && 0 <= w->cursor.vpos
	       && XINT (w->height) > w->cursor.vpos)
	{
	  if (!must_finish)
	    {
	      do_pending_window_change (1);

	      /* We used to always goto end_of_redisplay here, but this 
		 isn't enough if we have a blinking cursor.  */
	      if (w->cursor_off_p == w->last_cursor_off_p)
		goto end_of_redisplay;
	    }
	  goto update;
	}
      /* If highlighting the region, or if the cursor is in the echo area,
	 then we can't just move the cursor.  */
      else if (! (!NILP (Vtransient_mark_mode)
		  && !NILP (current_buffer->mark_active))
	       && (EQ (selected_window, current_buffer->last_selected_window)
		   || highlight_nonselected_windows)
	       && NILP (w->region_showing)
	       && NILP (Vshow_trailing_whitespace)
	       && !cursor_in_echo_area)
	{
	  struct it it;
	  struct glyph_row *row;

	  /* Skip from tlbufpos to PT and see where it is.  Note that
	     PT may be in invisible text.  If so, we will end at the
	     next visible position.  */
	  init_iterator (&it, w, CHARPOS (tlbufpos), BYTEPOS (tlbufpos),
			 NULL, DEFAULT_FACE_ID);
	  it.current_x = this_line_start_x;
	  it.current_y = this_line_y;
	  it.vpos = this_line_vpos;
	  
	  /* The call to move_it_to stops in front of PT, but
	     moves over before-strings.  */
	  move_it_to (&it, PT, -1, -1, -1, MOVE_TO_POS);

	  if (it.vpos == this_line_vpos
	      && (row = MATRIX_ROW (w->current_matrix, this_line_vpos),
		  row->enabled_p))
	    {
	      xassert (this_line_vpos == it.vpos);
	      xassert (this_line_y == it.current_y);
	      set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
#if GLYPH_DEBUG
	      *w->desired_matrix->method = 0;
	      debug_method_add (w, "optimization 3");
#endif
	      goto update;
	    }
	  else
	    goto cancel;
	}

    cancel:
      /* Text changed drastically or point moved off of line.  */
      SET_MATRIX_ROW_ENABLED_P (w->desired_matrix, this_line_vpos, 0);
    }

  CHARPOS (this_line_start_pos) = 0;
  consider_all_windows_p |= buffer_shared > 1;
  ++clear_face_cache_count;

  
  /* Build desired matrices, and update the display.  If
     consider_all_windows_p is non-zero, do it for all windows on all
     frames.  Otherwise do it for selected_window, only.  */

  if (consider_all_windows_p)
    {
      Lisp_Object tail, frame;
      int i, n = 0, size = 50;
      struct frame **updated
	= (struct frame **) alloca (size * sizeof *updated);

      /* Clear the face cache eventually.  */
      if (clear_face_cache_count > CLEAR_FACE_CACHE_COUNT)
	{
	  clear_face_cache (0);
	  clear_face_cache_count = 0;
	}

      /* Recompute # windows showing selected buffer.  This will be
	 incremented each time such a window is displayed.  */
      buffer_shared = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);
	  
	  if (FRAME_WINDOW_P (f) || f == sf)
	    {
#ifdef HAVE_WINDOW_SYSTEM
	      if (clear_face_cache_count % 50 == 0
		  && FRAME_WINDOW_P (f))
		clear_image_cache (f, 0);
#endif /* HAVE_WINDOW_SYSTEM */

	      /* Mark all the scroll bars to be removed; we'll redeem
		 the ones we want when we redisplay their windows.  */
	      if (condemn_scroll_bars_hook)
		condemn_scroll_bars_hook (f);

	      if (FRAME_VISIBLE_P (f) && !FRAME_OBSCURED_P (f))
		redisplay_windows (FRAME_ROOT_WINDOW (f));

	      /* Any scroll bars which redisplay_windows should have
		 nuked should now go away.  */
	      if (judge_scroll_bars_hook)
		judge_scroll_bars_hook (f);

	      /* If fonts changed, display again.  */
	      if (fonts_changed_p)
		goto retry;
	      
	      if (FRAME_VISIBLE_P (f) && !FRAME_OBSCURED_P (f))
		{
		  /* See if we have to hscroll.  */
		  if (hscroll_windows (f->root_window))
		    goto retry;

		  /* Prevent various kinds of signals during display
		     update.  stdio is not robust about handling
		     signals, which can cause an apparent I/O
		     error.  */
		  if (interrupt_input)
		    unrequest_sigio ();
		  stop_polling ();

		  /* Update the display.  */
		  set_window_update_flags (XWINDOW (f->root_window), 1);
		  pause |= update_frame (f, 0, 0);
		  if (pause)
		    break;

		  if (n == size)
		    {
		      int nbytes = size * sizeof *updated;
		      struct frame **p = (struct frame **) alloca (2 * nbytes);
		      bcopy (updated, p, nbytes);
		      size *= 2;
		    }
		  
		  updated[n++] = f;
		}
	    }
	}

      /* Do the mark_window_display_accurate after all windows have
	 been redisplayed because this call resets flags in buffers
	 which are needed for proper redisplay.  */
      for (i = 0; i < n; ++i)
	{
	  struct frame *f = updated[i];
	  mark_window_display_accurate (f->root_window, 1);
	  if (frame_up_to_date_hook)
	    frame_up_to_date_hook (f);
	}
    }
  else if (FRAME_VISIBLE_P (sf) && !FRAME_OBSCURED_P (sf))
    {
      Lisp_Object mini_window;
      struct frame *mini_frame;

      displayed_buffer = XBUFFER (XWINDOW (selected_window)->buffer);
      /* Use list_of_error, not Qerror, so that
	 we catch only errors and don't run the debugger.  */
      internal_condition_case_1 (redisplay_window_1, selected_window,
				 list_of_error,
				 redisplay_window_error);
  
      /* Compare desired and current matrices, perform output.  */
    update:
  
      /* If fonts changed, display again.  */
      if (fonts_changed_p)
	goto retry;

      /* Prevent various kinds of signals during display update.
	 stdio is not robust about handling signals,
	 which can cause an apparent I/O error.  */
      if (interrupt_input)
	unrequest_sigio ();
      stop_polling ();

      if (FRAME_VISIBLE_P (sf) && !FRAME_OBSCURED_P (sf))
	{
	  if (hscroll_windows (selected_window))
	    goto retry;
	  
	  XWINDOW (selected_window)->must_be_updated_p = 1;
	  pause = update_frame (sf, 0, 0);
	}

      /* We may have called echo_area_display at the top of this
	 function.  If the echo area is on another frame, that may
	 have put text on a frame other than the selected one, so the
	 above call to update_frame would not have caught it.  Catch
	 it here.  */
      mini_window = FRAME_MINIBUF_WINDOW (sf);
      mini_frame = XFRAME (WINDOW_FRAME (XWINDOW (mini_window)));
	
      if (mini_frame != sf && FRAME_WINDOW_P (mini_frame))
	{
	  XWINDOW (mini_window)->must_be_updated_p = 1;
	  pause |= update_frame (mini_frame, 0, 0);
	  if (!pause && hscroll_windows (mini_window))
	    goto retry;
	}
    }

  /* If display was paused because of pending input, make sure we do a
     thorough update the next time.  */
  if (pause)
    {
      /* Prevent the optimization at the beginning of
	 redisplay_internal that tries a single-line update of the
	 line containing the cursor in the selected window.  */
      CHARPOS (this_line_start_pos) = 0;

      /* Let the overlay arrow be updated the next time.  */
      if (!NILP (last_arrow_position))
	{
	  last_arrow_position = Qt;
	  last_arrow_string = Qt;
	}
      
      /* If we pause after scrolling, some rows in the current
	 matrices of some windows are not valid.  */
      if (!WINDOW_FULL_WIDTH_P (w)
	  && !FRAME_WINDOW_P (XFRAME (w->frame)))
	update_mode_lines = 1;
    }
  else
    {
      if (!consider_all_windows_p)
	{
	  /* This has already been done above if
	     consider_all_windows_p is set.  */
	  mark_window_display_accurate_1 (w, 1);
	  
	  last_arrow_position = COERCE_MARKER (Voverlay_arrow_position);
	  last_arrow_string = Voverlay_arrow_string;
	  
	  if (frame_up_to_date_hook != 0)
	    frame_up_to_date_hook (sf);
	}

      update_mode_lines = 0;
      windows_or_buffers_changed = 0;
      cursor_type_changed = 0;
    }

  /* Start SIGIO interrupts coming again.  Having them off during the
     code above makes it less likely one will discard output, but not
     impossible, since there might be stuff in the system buffer here.
     But it is much hairier to try to do anything about that.  */
  if (interrupt_input)
    request_sigio ();
  start_polling ();

  /* If a frame has become visible which was not before, redisplay
     again, so that we display it.  Expose events for such a frame
     (which it gets when becoming visible) don't call the parts of
     redisplay constructing glyphs, so simply exposing a frame won't
     display anything in this case.  So, we have to display these
     frames here explicitly.  */
  if (!pause)
    {
      Lisp_Object tail, frame;
      int new_count = 0;

      FOR_EACH_FRAME (tail, frame)
	{
	  int this_is_visible = 0;

	  if (XFRAME (frame)->visible)
	    this_is_visible = 1;
	  FRAME_SAMPLE_VISIBILITY (XFRAME (frame));
	  if (XFRAME (frame)->visible)
	    this_is_visible = 1;

	  if (this_is_visible)
	    new_count++;
	}

      if (new_count != number_of_visible_frames)
	windows_or_buffers_changed++;
    }

  /* Change frame size now if a change is pending.  */
  do_pending_window_change (1);

  /* If we just did a pending size change, or have additional
     visible frames, redisplay again.  */
  if (windows_or_buffers_changed && !pause)
    goto retry;

 end_of_redisplay:;

  unbind_to (count, Qnil);
}


/* Redisplay, but leave alone any recent echo area message unless
   another message has been requested in its place.

   This is useful in situations where you need to redisplay but no
   user action has occurred, making it inappropriate for the message
   area to be cleared.  See tracking_off and
   wait_reading_process_input for examples of these situations.

   FROM_WHERE is an integer saying from where this function was
   called.  This is useful for debugging.  */

void
redisplay_preserve_echo_area (from_where)
     int from_where;
{
  TRACE ((stderr, "redisplay_preserve_echo_area (%d)\n", from_where));

  if (!NILP (echo_area_buffer[1]))
    {
      /* We have a previously displayed message, but no current
	 message.  Redisplay the previous message.  */
      display_last_displayed_message_p = 1;
      redisplay_internal (1);
      display_last_displayed_message_p = 0;
    }
  else
    redisplay_internal (1);
}


/* Function registered with record_unwind_protect in
   redisplay_internal.  Clears the flag indicating that a redisplay is
   in progress.  */

static Lisp_Object
unwind_redisplay (old_redisplaying_p)
     Lisp_Object old_redisplaying_p;
{
  redisplaying_p = XFASTINT (old_redisplaying_p);
  return Qnil;
}


/* Mark the display of window W as accurate or inaccurate.  If
   ACCURATE_P is non-zero mark display of W as accurate.  If
   ACCURATE_P is zero, arrange for W to be redisplayed the next time
   redisplay_internal is called.  */

static void
mark_window_display_accurate_1 (w, accurate_p)
     struct window *w;
     int accurate_p;
{
  if (BUFFERP (w->buffer))
    {
      struct buffer *b = XBUFFER (w->buffer);
	  
      w->last_modified
	= make_number (accurate_p ? BUF_MODIFF (b) : 0);
      w->last_overlay_modified
	= make_number (accurate_p ? BUF_OVERLAY_MODIFF (b) : 0);
      w->last_had_star
	= BUF_MODIFF (b) > BUF_SAVE_MODIFF (b) ? Qt : Qnil;

      if (accurate_p)
	{
	  b->clip_changed = 0;
	  b->prevent_redisplay_optimizations_p = 0;

	  BUF_UNCHANGED_MODIFIED (b) = BUF_MODIFF (b);
	  BUF_OVERLAY_UNCHANGED_MODIFIED (b) = BUF_OVERLAY_MODIFF (b);
	  BUF_BEG_UNCHANGED (b) = BUF_GPT (b) - BUF_BEG (b);
	  BUF_END_UNCHANGED (b) = BUF_Z (b) - BUF_GPT (b);
	  
	  w->current_matrix->buffer = b;
	  w->current_matrix->begv = BUF_BEGV (b);
	  w->current_matrix->zv = BUF_ZV (b);
	  
	  w->last_cursor = w->cursor;
	  w->last_cursor_off_p = w->cursor_off_p;
	  
	  if (w == XWINDOW (selected_window))
	    w->last_point = make_number (BUF_PT (b));
	  else
	    w->last_point = make_number (XMARKER (w->pointm)->charpos);
	}
    }

  if (accurate_p)
    {
      w->window_end_valid = w->buffer;
#if 0 /* This is incorrect with variable-height lines.  */
      xassert (XINT (w->window_end_vpos)
	       < (XINT (w->height)
		  - (WINDOW_WANTS_MODELINE_P (w) ? 1 : 0)));
#endif
      w->update_mode_line = Qnil;
    }
}


/* Mark the display of windows in the window tree rooted at WINDOW as
   accurate or inaccurate.  If ACCURATE_P is non-zero mark display of
   windows as accurate.  If ACCURATE_P is zero, arrange for windows to
   be redisplayed the next time redisplay_internal is called.  */

void
mark_window_display_accurate (window, accurate_p)
     Lisp_Object window;
     int accurate_p;
{
  struct window *w;
  
  for (; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      mark_window_display_accurate_1 (w, accurate_p);

      if (!NILP (w->vchild))
	mark_window_display_accurate (w->vchild, accurate_p);
      if (!NILP (w->hchild))
	mark_window_display_accurate (w->hchild, accurate_p);
    }

  if (accurate_p)
    {
      last_arrow_position = COERCE_MARKER (Voverlay_arrow_position);
      last_arrow_string = Voverlay_arrow_string;
    }
  else
    {
      /* Force a thorough redisplay the next time by setting
	 last_arrow_position and last_arrow_string to t, which is
	 unequal to any useful value of Voverlay_arrow_...  */
      last_arrow_position = Qt;
      last_arrow_string = Qt;
    }
}


/* Return value in display table DP (Lisp_Char_Table *) for character
   C.  Since a display table doesn't have any parent, we don't have to
   follow parent.  Do not call this function directly but use the
   macro DISP_CHAR_VECTOR.  */

Lisp_Object
disp_char_vector (dp, c)
     struct Lisp_Char_Table *dp;
     int c;
{
  int code[4], i;
  Lisp_Object val;

  if (SINGLE_BYTE_CHAR_P (c))
    return (dp->contents[c]);
  
  SPLIT_CHAR (c, code[0], code[1], code[2]);
  if (code[1] < 32)
    code[1] = -1;
  else if (code[2] < 32)
    code[2] = -1;
  
  /* Here, the possible range of code[0] (== charset ID) is
     128..max_charset.  Since the top level char table contains data
     for multibyte characters after 256th element, we must increment
     code[0] by 128 to get a correct index.  */
  code[0] += 128;
  code[3] = -1;		/* anchor */

  for (i = 0; code[i] >= 0; i++, dp = XCHAR_TABLE (val))
    {
      val = dp->contents[code[i]];
      if (!SUB_CHAR_TABLE_P (val))
	return (NILP (val) ? dp->defalt : val);
    }
  
  /* Here, val is a sub char table.  We return the default value of
     it.  */
  return (dp->defalt);
}



/***********************************************************************
			   Window Redisplay
 ***********************************************************************/

/* Redisplay all leaf windows in the window tree rooted at WINDOW.  */

static void
redisplay_windows (window)
     Lisp_Object window;
{
  while (!NILP (window))
    {
      struct window *w = XWINDOW (window);
      
      if (!NILP (w->hchild))
	redisplay_windows (w->hchild);
      else if (!NILP (w->vchild))
	redisplay_windows (w->vchild);
      else
	{
	  displayed_buffer = XBUFFER (w->buffer);
	  /* Use list_of_error, not Qerror, so that
	     we catch only errors and don't run the debugger.  */
	  internal_condition_case_1 (redisplay_window_0, window, 
				     list_of_error,
				     redisplay_window_error);
	}

      window = w->next;
    }
}

static Lisp_Object
redisplay_window_error ()
{
  displayed_buffer->display_error_modiff = BUF_MODIFF (displayed_buffer);
  return Qnil;
}

static Lisp_Object
redisplay_window_0 (window)
     Lisp_Object window;
{
  if (displayed_buffer->display_error_modiff < BUF_MODIFF (displayed_buffer))
    redisplay_window (window, 0);
  return Qnil;
}

static Lisp_Object
redisplay_window_1 (window)
     Lisp_Object window;
{
  if (displayed_buffer->display_error_modiff < BUF_MODIFF (displayed_buffer))
    redisplay_window (window, 1);
  return Qnil;
}

/* Set cursor position of W.  PT is assumed to be displayed in ROW.
   DELTA is the number of bytes by which positions recorded in ROW
   differ from current buffer positions.  */

void
set_cursor_from_row (w, row, matrix, delta, delta_bytes, dy, dvpos)
     struct window *w;
     struct glyph_row *row;
     struct glyph_matrix *matrix;
     int delta, delta_bytes, dy, dvpos;
{
  struct glyph *glyph = row->glyphs[TEXT_AREA];
  struct glyph *end = glyph + row->used[TEXT_AREA];
  int x = row->x;
  int pt_old = PT - delta;

  /* Skip over glyphs not having an object at the start of the row.
     These are special glyphs like truncation marks on terminal
     frames.  */
  if (row->displays_text_p)
    while (glyph < end
	   && INTEGERP (glyph->object)
	   && glyph->charpos < 0)
      {
	x += glyph->pixel_width;
	++glyph;
      }

  while (glyph < end
	 && !INTEGERP (glyph->object)
	 && (!BUFFERP (glyph->object)
	     || glyph->charpos < pt_old))
    {
      x += glyph->pixel_width;
      ++glyph;
    }

  w->cursor.hpos = glyph - row->glyphs[TEXT_AREA];
  w->cursor.x = x;
  w->cursor.vpos = MATRIX_ROW_VPOS (row, matrix) + dvpos;
  w->cursor.y = row->y + dy;

  if (w == XWINDOW (selected_window))
    {
      if (!row->continued_p
	  && !MATRIX_ROW_CONTINUATION_LINE_P (row)
	  && row->x == 0)
	{
	  this_line_buffer = XBUFFER (w->buffer);
	  
	  CHARPOS (this_line_start_pos)
	    = MATRIX_ROW_START_CHARPOS (row) + delta;
	  BYTEPOS (this_line_start_pos)
	    = MATRIX_ROW_START_BYTEPOS (row) + delta_bytes;
	  
	  CHARPOS (this_line_end_pos)
	    = Z - (MATRIX_ROW_END_CHARPOS (row) + delta);
	  BYTEPOS (this_line_end_pos)
	    = Z_BYTE - (MATRIX_ROW_END_BYTEPOS (row) + delta_bytes);
	  
	  this_line_y = w->cursor.y;
	  this_line_pixel_height = row->height;
	  this_line_vpos = w->cursor.vpos;
	  this_line_start_x = row->x;
	}
      else
	CHARPOS (this_line_start_pos) = 0;
    }
}


/* Run window scroll functions, if any, for WINDOW with new window
   start STARTP.  Sets the window start of WINDOW to that position.

   We assume that the window's buffer is really current.  */

static INLINE struct text_pos
run_window_scroll_functions (window, startp)
     Lisp_Object window;
     struct text_pos startp;
{
  struct window *w = XWINDOW (window);
  SET_MARKER_FROM_TEXT_POS (w->start, startp);

  if (current_buffer != XBUFFER (w->buffer))
    abort ();

  if (!NILP (Vwindow_scroll_functions))
    {
      run_hook_with_args_2 (Qwindow_scroll_functions, window, 
			    make_number (CHARPOS (startp)));
      SET_TEXT_POS_FROM_MARKER (startp, w->start);
      /* In case the hook functions switch buffers.  */
      if (current_buffer != XBUFFER (w->buffer))
	set_buffer_internal_1 (XBUFFER (w->buffer));
    }

  return startp;
}


/* Modify the desired matrix of window W and W->vscroll so that the
   line containing the cursor is fully visible.  If this requires
   larger matrices than are allocated, set fonts_changed_p and return
   0.  */

static int
make_cursor_line_fully_visible (w)
     struct window *w;
{
  struct glyph_matrix *matrix;
  struct glyph_row *row;
  int window_height;
  
  /* It's not always possible to find the cursor, e.g, when a window
     is full of overlay strings.  Don't do anything in that case.  */
  if (w->cursor.vpos < 0)
    return 1;
  
  matrix = w->desired_matrix;
  row = MATRIX_ROW (matrix, w->cursor.vpos);

  /* If the cursor row is not partially visible, there's nothing
     to do.  */
  if (!MATRIX_ROW_PARTIALLY_VISIBLE_P (row))
    return 1;

  /* If the row the cursor is in is taller than the window's height,
     it's not clear what to do, so do nothing.  */
  window_height = window_box_height (w);
  if (row->height >= window_height)
    return 1;

  if (MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P (w, row))
    {
      int dy = row->height - row->visible_height;
      w->vscroll = 0;
      w->cursor.y += dy;
      shift_glyph_matrix (w, matrix, 0, matrix->nrows, dy);
    }
  else /* MATRIX_ROW_PARTIALLY_VISIBLE_AT_BOTTOM_P (w, row)) */
    {
      int dy = - (row->height - row->visible_height);
      w->vscroll = dy;
      w->cursor.y += dy;
      shift_glyph_matrix (w, matrix, 0, matrix->nrows, dy);
    }
  
  /* When we change the cursor y-position of the selected window,
     change this_line_y as well so that the display optimization for
     the cursor line of the selected window in redisplay_internal uses
     the correct y-position.  */
  if (w == XWINDOW (selected_window))
    this_line_y = w->cursor.y;

  /* If vscrolling requires a larger glyph matrix, arrange for a fresh
     redisplay with larger matrices.  */
  if (matrix->nrows < required_matrix_height (w))
    {
      fonts_changed_p = 1;
      return 0;
    }

  return 1;
}


/* Try scrolling PT into view in window WINDOW.  JUST_THIS_ONE_P
   non-zero means only WINDOW is redisplayed in redisplay_internal.
   TEMP_SCROLL_STEP has the same meaning as scroll_step, and is used
   in redisplay_window to bring a partially visible line into view in
   the case that only the cursor has moved.

   Value is

   1	if scrolling succeeded
    
   0	if scrolling didn't find point.
   
   -1	if new fonts have been loaded so that we must interrupt
   redisplay, adjust glyph matrices, and try again.  */

enum
{
  SCROLLING_SUCCESS,
  SCROLLING_FAILED,
  SCROLLING_NEED_LARGER_MATRICES
};

static int
try_scrolling (window, just_this_one_p, scroll_conservatively,
	       scroll_step, temp_scroll_step)
     Lisp_Object window;
     int just_this_one_p;
     EMACS_INT scroll_conservatively, scroll_step;
     int temp_scroll_step;
{
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  struct text_pos scroll_margin_pos;
  struct text_pos pos;
  struct text_pos startp;
  struct it it;
  Lisp_Object window_end;
  int this_scroll_margin;
  int dy = 0;
  int scroll_max;
  int rc;
  int amount_to_scroll = 0;
  Lisp_Object aggressive;
  int height;

#if GLYPH_DEBUG
  debug_method_add (w, "try_scrolling");
#endif

  SET_TEXT_POS_FROM_MARKER (startp, w->start);
  
  /* Compute scroll margin height in pixels.  We scroll when point is
     within this distance from the top or bottom of the window.  */
  if (scroll_margin > 0)
    {
      this_scroll_margin = min (scroll_margin, XINT (w->height) / 4);
      this_scroll_margin *= CANON_Y_UNIT (f);
    }
  else
    this_scroll_margin = 0;

  /* Compute how much we should try to scroll maximally to bring point
     into view.  */
  if (scroll_step || scroll_conservatively || temp_scroll_step)
    scroll_max = max (scroll_step,
		      max (scroll_conservatively, temp_scroll_step));
  else if (NUMBERP (current_buffer->scroll_down_aggressively)
	   || NUMBERP (current_buffer->scroll_up_aggressively))
    /* We're trying to scroll because of aggressive scrolling
       but no scroll_step is set.  Choose an arbitrary one.  Maybe
       there should be a variable for this.  */
    scroll_max = 10;
  else
    scroll_max = 0;
  scroll_max *= CANON_Y_UNIT (f);

  /* Decide whether we have to scroll down.  Start at the window end
     and move this_scroll_margin up to find the position of the scroll
     margin.  */
  window_end = Fwindow_end (window, Qt);
  CHARPOS (scroll_margin_pos) = XINT (window_end);
  BYTEPOS (scroll_margin_pos) = CHAR_TO_BYTE (CHARPOS (scroll_margin_pos));
  if (this_scroll_margin)
    {
      start_display (&it, w, scroll_margin_pos);
      move_it_vertically (&it, - this_scroll_margin);
      scroll_margin_pos = it.current.pos;
    }

  if (PT >= CHARPOS (scroll_margin_pos))
    {
      int y0;
      
      /* Point is in the scroll margin at the bottom of the window, or
	 below.  Compute a new window start that makes point visible.  */

      /* Compute the distance from the scroll margin to PT.
	 Give up if the distance is greater than scroll_max.  */
      start_display (&it, w, scroll_margin_pos);
      y0 = it.current_y;
      move_it_to (&it, PT, 0, it.last_visible_y, -1,
		  MOVE_TO_POS | MOVE_TO_X | MOVE_TO_Y);

      /* To make point visible, we have to move the window start
	 down so that the line the cursor is in is visible, which
	 means we have to add in the height of the cursor line.  */
      dy = line_bottom_y (&it) - y0;
      
      if (dy > scroll_max)
	return SCROLLING_FAILED;
      
      /* Move the window start down.  If scrolling conservatively,
	 move it just enough down to make point visible.  If
	 scroll_step is set, move it down by scroll_step.  */
      start_display (&it, w, startp);

      if (scroll_conservatively)
	amount_to_scroll
	  = max (max (dy, CANON_Y_UNIT (f)),
		 CANON_Y_UNIT (f) * max (scroll_step, temp_scroll_step));
      else if (scroll_step || temp_scroll_step)
	amount_to_scroll = scroll_max;
      else
	{
	  aggressive = current_buffer->scroll_up_aggressively;
	  height = (WINDOW_DISPLAY_HEIGHT_NO_MODE_LINE (w)
		    - WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w));
	  if (NUMBERP (aggressive))
	    amount_to_scroll = XFLOATINT (aggressive) * height;
	}

      if (amount_to_scroll <= 0)
	return SCROLLING_FAILED;

      move_it_vertically (&it, amount_to_scroll);
      startp = it.current.pos;
    }
  else
    {
      /* See if point is inside the scroll margin at the top of the
         window.  */
      scroll_margin_pos = startp;
      if (this_scroll_margin)
	{
	  start_display (&it, w, startp);
	  move_it_vertically (&it, this_scroll_margin);
	  scroll_margin_pos = it.current.pos;
	}

      if (PT < CHARPOS (scroll_margin_pos))
	{
	  /* Point is in the scroll margin at the top of the window or
	     above what is displayed in the window.  */
	  int y0;
	  
	  /* Compute the vertical distance from PT to the scroll
	     margin position.  Give up if distance is greater than
	     scroll_max.  */
	  SET_TEXT_POS (pos, PT, PT_BYTE);
	  start_display (&it, w, pos);
	  y0 = it.current_y;
	  move_it_to (&it, CHARPOS (scroll_margin_pos), 0,
		      it.last_visible_y, -1,
		      MOVE_TO_POS | MOVE_TO_X | MOVE_TO_Y);
	  dy = it.current_y - y0;
	  if (dy > scroll_max)
	    return SCROLLING_FAILED;
	  
	  /* Compute new window start.  */
	  start_display (&it, w, startp);
	  
	  if (scroll_conservatively)
	    amount_to_scroll =
	      max (dy, CANON_Y_UNIT (f) * max (scroll_step, temp_scroll_step));
	  else if (scroll_step || temp_scroll_step)
	    amount_to_scroll = scroll_max;
	  else
	    {
	      aggressive = current_buffer->scroll_down_aggressively;
	      height = (WINDOW_DISPLAY_HEIGHT_NO_MODE_LINE (w)
			- WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w));
	      if (NUMBERP (aggressive))
		amount_to_scroll = XFLOATINT (aggressive) * height;
	    }

	  if (amount_to_scroll <= 0)
	    return SCROLLING_FAILED;
	  
	  move_it_vertically (&it, - amount_to_scroll);
	  startp = it.current.pos;
	}
    }

  /* Run window scroll functions.  */
  startp = run_window_scroll_functions (window, startp);

  /* Display the window.  Give up if new fonts are loaded, or if point
     doesn't appear.  */
  if (!try_window (window, startp))
    rc = SCROLLING_NEED_LARGER_MATRICES;
  else if (w->cursor.vpos < 0)
    {
      clear_glyph_matrix (w->desired_matrix);
      rc = SCROLLING_FAILED;
    }
  else
    {
      /* Maybe forget recorded base line for line number display.  */
      if (!just_this_one_p 
	  || current_buffer->clip_changed
	  || BEG_UNCHANGED < CHARPOS (startp))
	w->base_line_number = Qnil;
      
      /* If cursor ends up on a partially visible line, shift display
	 lines up or down.  If that fails because we need larger
	 matrices, give up.  */
      if (!make_cursor_line_fully_visible (w))
	rc = SCROLLING_NEED_LARGER_MATRICES;
      else
	rc = SCROLLING_SUCCESS;
    }

  return rc;
}


/* Compute a suitable window start for window W if display of W starts
   on a continuation line.  Value is non-zero if a new window start
   was computed.

   The new window start will be computed, based on W's width, starting
   from the start of the continued line.  It is the start of the
   screen line with the minimum distance from the old start W->start.  */

static int
compute_window_start_on_continuation_line (w)
     struct window *w;
{
  struct text_pos pos, start_pos;
  int window_start_changed_p = 0;

  SET_TEXT_POS_FROM_MARKER (start_pos, w->start);

  /* If window start is on a continuation line...  Window start may be
     < BEGV in case there's invisible text at the start of the 
     buffer (M-x rmail, for example).  */
  if (CHARPOS (start_pos) > BEGV
      && FETCH_BYTE (BYTEPOS (start_pos) - 1) != '\n')
    {
      struct it it;
      struct glyph_row *row;

      /* Handle the case that the window start is out of range.  */
      if (CHARPOS (start_pos) < BEGV)
	SET_TEXT_POS (start_pos, BEGV, BEGV_BYTE);
      else if (CHARPOS (start_pos) > ZV)
	SET_TEXT_POS (start_pos, ZV, ZV_BYTE);
      
      /* Find the start of the continued line.  This should be fast
	 because scan_buffer is fast (newline cache).  */
      row = w->desired_matrix->rows + (WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0);
      init_iterator (&it, w, CHARPOS (start_pos), BYTEPOS (start_pos),
		     row, DEFAULT_FACE_ID);
      reseat_at_previous_visible_line_start (&it);

      /* If the line start is "too far" away from the window start,
         say it takes too much time to compute a new window start.  */
      if (CHARPOS (start_pos) - IT_CHARPOS (it)
	  < XFASTINT (w->height) * XFASTINT (w->width))
	{
	  int min_distance, distance;
	  
	  /* Move forward by display lines to find the new window
	     start.  If window width was enlarged, the new start can
	     be expected to be > the old start.  If window width was
	     decreased, the new window start will be < the old start.
	     So, we're looking for the display line start with the
	     minimum distance from the old window start.  */
	  pos = it.current.pos;
	  min_distance = INFINITY;
	  while ((distance = abs (CHARPOS (start_pos) - IT_CHARPOS (it))),
		 distance < min_distance)
	    {
	      min_distance = distance;
	      pos = it.current.pos;
	      move_it_by_lines (&it, 1, 0);
	    }
	  
	  /* Set the window start there.  */
	  SET_MARKER_FROM_TEXT_POS (w->start, pos);
	  window_start_changed_p = 1;
	}
    }
  
  return window_start_changed_p;
}


/* Try cursor movement in case text has not changes in window WINDOW,
   with window start STARTP.  Value is

   CURSOR_MOVEMENT_SUCCESS if successful
   
   CURSOR_MOVEMENT_CANNOT_BE_USED if this method cannot be used

   CURSOR_MOVEMENT_MUST_SCROLL if we know we have to scroll the
   display.  *SCROLL_STEP is set to 1, under certain circumstances, if
   we want to scroll as if scroll-step were set to 1.  See the code.

   CURSOR_MOVEMENT_NEED_LARGER_MATRICES if we need larger matrices, in
   which case we have to abort this redisplay, and adjust matrices
   first.  */

enum 
{
  CURSOR_MOVEMENT_SUCCESS,
  CURSOR_MOVEMENT_CANNOT_BE_USED,
  CURSOR_MOVEMENT_MUST_SCROLL,
  CURSOR_MOVEMENT_NEED_LARGER_MATRICES
};

static int
try_cursor_movement (window, startp, scroll_step)
     Lisp_Object window;
     struct text_pos startp;
     int *scroll_step;
{
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  int rc = CURSOR_MOVEMENT_CANNOT_BE_USED;
  
#if GLYPH_DEBUG
  if (inhibit_try_cursor_movement)
    return rc;
#endif

  /* Handle case where text has not changed, only point, and it has
     not moved off the frame.  */
  if (/* Point may be in this window.  */
      PT >= CHARPOS (startp)
      /* Selective display hasn't changed.  */
      && !current_buffer->clip_changed
      /* Function force-mode-line-update is used to force a thorough
	 redisplay.  It sets either windows_or_buffers_changed or
	 update_mode_lines.  So don't take a shortcut here for these
	 cases.  */
      && !update_mode_lines
      && !windows_or_buffers_changed
      && !cursor_type_changed
      /* Can't use this case if highlighting a region.  When a 
         region exists, cursor movement has to do more than just
         set the cursor.  */
      && !(!NILP (Vtransient_mark_mode)
	   && !NILP (current_buffer->mark_active))
      && NILP (w->region_showing)
      && NILP (Vshow_trailing_whitespace)
      /* Right after splitting windows, last_point may be nil.  */
      && INTEGERP (w->last_point)
      /* This code is not used for mini-buffer for the sake of the case
	 of redisplaying to replace an echo area message; since in
	 that case the mini-buffer contents per se are usually
	 unchanged.  This code is of no real use in the mini-buffer
	 since the handling of this_line_start_pos, etc., in redisplay
	 handles the same cases.  */
      && !EQ (window, minibuf_window)
      /* When splitting windows or for new windows, it happens that
	 redisplay is called with a nil window_end_vpos or one being
	 larger than the window.  This should really be fixed in
	 window.c.  I don't have this on my list, now, so we do
	 approximately the same as the old redisplay code.  --gerd.  */
      && INTEGERP (w->window_end_vpos)
      && XFASTINT (w->window_end_vpos) < w->current_matrix->nrows
      && (FRAME_WINDOW_P (f)
	  || !MARKERP (Voverlay_arrow_position)
	  || current_buffer != XMARKER (Voverlay_arrow_position)->buffer))
    {
      int this_scroll_margin;
      struct glyph_row *row = NULL;

#if GLYPH_DEBUG
      debug_method_add (w, "cursor movement");
#endif

      /* Scroll if point within this distance from the top or bottom
	 of the window.  This is a pixel value.  */
      this_scroll_margin = max (0, scroll_margin);
      this_scroll_margin = min (this_scroll_margin, XFASTINT (w->height) / 4);
      this_scroll_margin *= CANON_Y_UNIT (f);

      /* Start with the row the cursor was displayed during the last
	 not paused redisplay.  Give up if that row is not valid.  */
      if (w->last_cursor.vpos < 0
	  || w->last_cursor.vpos >= w->current_matrix->nrows)
	rc = CURSOR_MOVEMENT_MUST_SCROLL;
      else
	{
	  row = MATRIX_ROW (w->current_matrix, w->last_cursor.vpos);
	  if (row->mode_line_p)
	    ++row;
	  if (!row->enabled_p)
	    rc = CURSOR_MOVEMENT_MUST_SCROLL;
	}

      if (rc == CURSOR_MOVEMENT_CANNOT_BE_USED)
	{
	  int scroll_p = 0;
	  int last_y = window_text_bottom_y (w) - this_scroll_margin;
	  
	  if (PT > XFASTINT (w->last_point))
	    {
	      /* Point has moved forward.  */
	      while (MATRIX_ROW_END_CHARPOS (row) < PT
		     && MATRIX_ROW_BOTTOM_Y (row) < last_y)
		{
		  xassert (row->enabled_p);
		  ++row;
		}

	      /* The end position of a row equals the start position
		 of the next row.  If PT is there, we would rather
		 display it in the next line.  */
	      while (MATRIX_ROW_BOTTOM_Y (row) < last_y
		     && MATRIX_ROW_END_CHARPOS (row) == PT
		     && !cursor_row_p (w, row))
		++row;

	      /* If within the scroll margin, scroll.  Note that
		 MATRIX_ROW_BOTTOM_Y gives the pixel position at which
		 the next line would be drawn, and that
		 this_scroll_margin can be zero.  */
	      if (MATRIX_ROW_BOTTOM_Y (row) > last_y
		  || PT > MATRIX_ROW_END_CHARPOS (row)
		  /* Line is completely visible last line in window
		     and PT is to be set in the next line.  */
		  || (MATRIX_ROW_BOTTOM_Y (row) == last_y
		      && PT == MATRIX_ROW_END_CHARPOS (row)
		      && !row->ends_at_zv_p
		      && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row)))
		scroll_p = 1;
	    }
	  else if (PT < XFASTINT (w->last_point))
	    {
	      /* Cursor has to be moved backward.  Note that PT >=
		 CHARPOS (startp) because of the outer
		 if-statement.  */
	      while (!row->mode_line_p
		     && (MATRIX_ROW_START_CHARPOS (row) > PT
			 || (MATRIX_ROW_START_CHARPOS (row) == PT
			     && MATRIX_ROW_STARTS_IN_MIDDLE_OF_CHAR_P (row)))
		     && (row->y > this_scroll_margin
			 || CHARPOS (startp) == BEGV))
		{
		  xassert (row->enabled_p);
		  --row;
		}

	      /* Consider the following case: Window starts at BEGV,
		 there is invisible, intangible text at BEGV, so that
		 display starts at some point START > BEGV.  It can
		 happen that we are called with PT somewhere between
		 BEGV and START.  Try to handle that case.  */
	      if (row < w->current_matrix->rows
		  || row->mode_line_p)
		{
		  row = w->current_matrix->rows;
		  if (row->mode_line_p)
		    ++row;
		}

	      /* Due to newlines in overlay strings, we may have to
		 skip forward over overlay strings.  */
	      while (MATRIX_ROW_BOTTOM_Y (row) < last_y
		     && MATRIX_ROW_END_CHARPOS (row) == PT
		     && !cursor_row_p (w, row))
		++row;
	  
	      /* If within the scroll margin, scroll.  */
	      if (row->y < this_scroll_margin
		  && CHARPOS (startp) != BEGV)
		scroll_p = 1;
	    }

	  if (PT < MATRIX_ROW_START_CHARPOS (row)
	      || PT > MATRIX_ROW_END_CHARPOS (row))
	    {
	      /* if PT is not in the glyph row, give up.  */
	      rc = CURSOR_MOVEMENT_MUST_SCROLL;
	    }
	  else if (MATRIX_ROW_PARTIALLY_VISIBLE_P (row))
	    {
	      if (PT == MATRIX_ROW_END_CHARPOS (row)
		  && !row->ends_at_zv_p
		  && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row))
		rc = CURSOR_MOVEMENT_MUST_SCROLL;
	      else if (row->height > window_box_height (w))
		{
		  /* If we end up in a partially visible line, let's
		     make it fully visible, except when it's taller
		     than the window, in which case we can't do much
		     about it.  */
		  *scroll_step = 1;
		  rc = CURSOR_MOVEMENT_MUST_SCROLL;
		}
	      else
		{
		  set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
		  try_window (window, startp);
		  if (!make_cursor_line_fully_visible (w))
		    rc = CURSOR_MOVEMENT_NEED_LARGER_MATRICES;
		  else
		    rc = CURSOR_MOVEMENT_SUCCESS;
		}
	    }
	  else if (scroll_p)
	    rc = CURSOR_MOVEMENT_MUST_SCROLL;
	  else
	    {
	      set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
	      rc = CURSOR_MOVEMENT_SUCCESS;
	    }
	}
    }

  return rc;
}


/* Redisplay leaf window WINDOW.  JUST_THIS_ONE_P non-zero means only
   selected_window is redisplayed.  */

static void
redisplay_window (window, just_this_one_p)
     Lisp_Object window;
     int just_this_one_p;
{
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  struct buffer *buffer = XBUFFER (w->buffer);
  struct buffer *old = current_buffer;
  struct text_pos lpoint, opoint, startp;
  int update_mode_line;
  int tem;
  struct it it;
  /* Record it now because it's overwritten.  */
  int current_matrix_up_to_date_p = 0;
  int temp_scroll_step = 0;
  int count = BINDING_STACK_SIZE ();
  int rc;

  SET_TEXT_POS (lpoint, PT, PT_BYTE);
  opoint = lpoint;

  /* W must be a leaf window here.  */
  xassert (!NILP (w->buffer));
#if GLYPH_DEBUG
  *w->desired_matrix->method = 0;
#endif

  specbind (Qinhibit_point_motion_hooks, Qt);

  reconsider_clip_changes (w, buffer);
    
  /* Has the mode line to be updated?  */ 
  update_mode_line = (!NILP (w->update_mode_line)
		      || update_mode_lines
		      || buffer->clip_changed);

  if (MINI_WINDOW_P (w))
    {
      if (w == XWINDOW (echo_area_window)
	  && !NILP (echo_area_buffer[0]))
	{
	  if (update_mode_line)
	    /* We may have to update a tty frame's menu bar or a
	       tool-bar.  Example `M-x C-h C-h C-g'.  */
	    goto finish_menu_bars;
	  else
	    /* We've already displayed the echo area glyphs in this window.  */
	    goto finish_scroll_bars;
	}
      else if (w != XWINDOW (minibuf_window))
	{
	  /* W is a mini-buffer window, but it's not the currently
	     active one, so clear it.  */
	  int yb = window_text_bottom_y (w);
	  struct glyph_row *row;
	  int y;

	  for (y = 0, row = w->desired_matrix->rows;
	       y < yb;
	       y += row->height, ++row)
	    blank_row (w, row, y);
	  goto finish_scroll_bars;
	}

      clear_glyph_matrix (w->desired_matrix);
    }

  /* Otherwise set up data on this window; select its buffer and point
     value.  */
  /* Really select the buffer, for the sake of buffer-local
     variables.  */
  set_buffer_internal_1 (XBUFFER (w->buffer));
  SET_TEXT_POS (opoint, PT, PT_BYTE);

  current_matrix_up_to_date_p
    = (!NILP (w->window_end_valid)
       && !current_buffer->clip_changed
       && XFASTINT (w->last_modified) >= MODIFF
       && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF);

  /* When windows_or_buffers_changed is non-zero, we can't rely on
     the window end being valid, so set it to nil there.  */
  if (windows_or_buffers_changed)
    {
      /* If window starts on a continuation line, maybe adjust the
	 window start in case the window's width changed.  */
      if (XMARKER (w->start)->buffer == current_buffer)
	compute_window_start_on_continuation_line (w);
      
      w->window_end_valid = Qnil;
    }

  /* Some sanity checks.  */
  CHECK_WINDOW_END (w);
  if (Z == Z_BYTE && CHARPOS (opoint) != BYTEPOS (opoint))
    abort ();
  if (BYTEPOS (opoint) < CHARPOS (opoint))
    abort ();

  /* If %c is in mode line, update it if needed.  */
  if (!NILP (w->column_number_displayed)
      /* This alternative quickly identifies a common case
	 where no change is needed.  */
      && !(PT == XFASTINT (w->last_point)
	   && XFASTINT (w->last_modified) >= MODIFF
	   && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF)
      && XFASTINT (w->column_number_displayed) != current_column ())
    update_mode_line = 1; 

  /* Count number of windows showing the selected buffer.  An indirect
     buffer counts as its base buffer.  */
  if (!just_this_one_p)
    {
      struct buffer *current_base, *window_base;
      current_base = current_buffer;
      window_base = XBUFFER (XWINDOW (selected_window)->buffer);
      if (current_base->base_buffer)
	current_base = current_base->base_buffer;
      if (window_base->base_buffer)
	window_base = window_base->base_buffer;
      if (current_base == window_base)
	buffer_shared++;
    }

  /* Point refers normally to the selected window.  For any other
     window, set up appropriate value.  */
  if (!EQ (window, selected_window))
    {
      int new_pt = XMARKER (w->pointm)->charpos;
      int new_pt_byte = marker_byte_position (w->pointm);
      if (new_pt < BEGV)
	{
	  new_pt = BEGV;
	  new_pt_byte = BEGV_BYTE;
	  set_marker_both (w->pointm, Qnil, BEGV, BEGV_BYTE);
	}
      else if (new_pt > (ZV - 1))
	{
	  new_pt = ZV;
	  new_pt_byte = ZV_BYTE;
	  set_marker_both (w->pointm, Qnil, ZV, ZV_BYTE);
	}
      
      /* We don't use SET_PT so that the point-motion hooks don't run.  */
      TEMP_SET_PT_BOTH (new_pt, new_pt_byte);
    }

  /* If any of the character widths specified in the display table
     have changed, invalidate the width run cache.  It's true that
     this may be a bit late to catch such changes, but the rest of
     redisplay goes (non-fatally) haywire when the display table is
     changed, so why should we worry about doing any better?  */
  if (current_buffer->width_run_cache)
    {
      struct Lisp_Char_Table *disptab = buffer_display_table ();

      if (! disptab_matches_widthtab (disptab,
                                      XVECTOR (current_buffer->width_table)))
        {
          invalidate_region_cache (current_buffer,
                                   current_buffer->width_run_cache,
                                   BEG, Z);
          recompute_width_table (current_buffer, disptab);
        }
    }

  /* If window-start is screwed up, choose a new one.  */
  if (XMARKER (w->start)->buffer != current_buffer)
    goto recenter;

  SET_TEXT_POS_FROM_MARKER (startp, w->start);

  /* If someone specified a new starting point but did not insist,
     check whether it can be used.  */
  if (!NILP (w->optional_new_start)
      && CHARPOS (startp) >= BEGV
      && CHARPOS (startp) <= ZV)
    {
      w->optional_new_start = Qnil;
      start_display (&it, w, startp);
      move_it_to (&it, PT, 0, it.last_visible_y, -1,
		  MOVE_TO_POS | MOVE_TO_X | MOVE_TO_Y);
      if (IT_CHARPOS (it) == PT)
	w->force_start = Qt;
    }

  /* Handle case where place to start displaying has been specified,
     unless the specified location is outside the accessible range.  */
  if (!NILP (w->force_start)
      || w->frozen_window_start_p)
    {
      w->force_start = Qnil;
      w->vscroll = 0;
      w->window_end_valid = Qnil;

      /* Forget any recorded base line for line number display.  */
      if (!current_matrix_up_to_date_p
	  || current_buffer->clip_changed)
	w->base_line_number = Qnil;

      /* Redisplay the mode line.  Select the buffer properly for that.
	 Also, run the hook window-scroll-functions
	 because we have scrolled.  */
      /* Note, we do this after clearing force_start because
	 if there's an error, it is better to forget about force_start
	 than to get into an infinite loop calling the hook functions
	 and having them get more errors.  */
      if (!update_mode_line
	  || ! NILP (Vwindow_scroll_functions))
	{
	  update_mode_line = 1;
	  w->update_mode_line = Qt;
	  startp = run_window_scroll_functions (window, startp);
	}
      
      w->last_modified = make_number (0);
      w->last_overlay_modified = make_number (0);
      if (CHARPOS (startp) < BEGV)
	SET_TEXT_POS (startp, BEGV, BEGV_BYTE);
      else if (CHARPOS (startp) > ZV)
	SET_TEXT_POS (startp, ZV, ZV_BYTE);
      
      /* Redisplay, then check if cursor has been set during the 
	 redisplay.  Give up if new fonts were loaded.  */
      if (!try_window (window, startp))
	{
	  w->force_start = Qt;
	  clear_glyph_matrix (w->desired_matrix);
	  goto finish_scroll_bars;
	}

      if (w->cursor.vpos < 0 && !w->frozen_window_start_p)
	{
	  /* If point does not appear, try to move point so it does
	     appear. The desired matrix has been built above, so we
	     can use it here.  */
	  int window_height;
	  struct glyph_row *row;

	  window_height = window_box_height (w) / 2;
	  row = MATRIX_FIRST_TEXT_ROW (w->desired_matrix);
	  while (MATRIX_ROW_BOTTOM_Y (row) < window_height)
	    ++row;

	  TEMP_SET_PT_BOTH (MATRIX_ROW_START_CHARPOS (row),
			    MATRIX_ROW_START_BYTEPOS (row));

	  if (w != XWINDOW (selected_window))
	    set_marker_both (w->pointm, Qnil, PT, PT_BYTE);
	  else if (current_buffer == old)
	    SET_TEXT_POS (lpoint, PT, PT_BYTE);

	  set_cursor_from_row (w, row, w->desired_matrix, 0, 0, 0, 0);
	  
	  /* If we are highlighting the region, then we just changed
	     the region, so redisplay to show it.  */
	  if (!NILP (Vtransient_mark_mode)
	      && !NILP (current_buffer->mark_active))
	    {
	      clear_glyph_matrix (w->desired_matrix);
	      if (!try_window (window, startp))
		goto need_larger_matrices;
	    }
	}

      if (!make_cursor_line_fully_visible (w))
	goto need_larger_matrices;
#if GLYPH_DEBUG
      debug_method_add (w, "forced window start");
#endif
      goto done;
    }

  /* Handle case where text has not changed, only point, and it has
     not moved off the frame.  */
  if (current_matrix_up_to_date_p
      && (rc = try_cursor_movement (window, startp, &temp_scroll_step),
	  rc != CURSOR_MOVEMENT_CANNOT_BE_USED))
    {
      switch (rc)
	{
	case CURSOR_MOVEMENT_SUCCESS:
	  goto done;
	  
	case CURSOR_MOVEMENT_NEED_LARGER_MATRICES:
	  goto need_larger_matrices;
	  
	case CURSOR_MOVEMENT_MUST_SCROLL:
	  goto try_to_scroll;
	  
	default:
	  abort ();
	}
    }
  /* If current starting point was originally the beginning of a line
     but no longer is, find a new starting point.  */
  else if (!NILP (w->start_at_line_beg)
	   && !(CHARPOS (startp) <= BEGV
		|| FETCH_BYTE (BYTEPOS (startp) - 1) == '\n'))
    {
#if GLYPH_DEBUG
      debug_method_add (w, "recenter 1");
#endif
      goto recenter;
    }
  
  /* Try scrolling with try_window_id.  Value is > 0 if update has
     been done, it is -1 if we know that the same window start will
     not work.  It is 0 if unsuccessful for some other reason.  */
  else if ((tem = try_window_id (w)) != 0)
    {
#if GLYPH_DEBUG
      debug_method_add (w, "try_window_id %d", tem);
#endif

      if (fonts_changed_p)
	goto need_larger_matrices;
      if (tem > 0)
	goto done;

      /* Otherwise try_window_id has returned -1 which means that we
	 don't want the alternative below this comment to execute.  */
    }
  else if (CHARPOS (startp) >= BEGV
	   && CHARPOS (startp) <= ZV
	   && PT >= CHARPOS (startp)
	   && (CHARPOS (startp) < ZV
	       /* Avoid starting at end of buffer.  */
	       || CHARPOS (startp) == BEGV
	       || (XFASTINT (w->last_modified) >= MODIFF
		   && XFASTINT (w->last_overlay_modified) >= OVERLAY_MODIFF)))
    {
#if GLYPH_DEBUG
      debug_method_add (w, "same window start");
#endif
      
      /* Try to redisplay starting at same place as before.
         If point has not moved off frame, accept the results.  */
      if (!current_matrix_up_to_date_p
	  /* Don't use try_window_reusing_current_matrix in this case
	     because a window scroll function can have changed the
	     buffer.  */
	  || !NILP (Vwindow_scroll_functions)
	  || MINI_WINDOW_P (w)
	  || !try_window_reusing_current_matrix (w))
	{
	  IF_DEBUG (debug_method_add (w, "1"));
	  try_window (window, startp);
	}

      if (fonts_changed_p)
	goto need_larger_matrices;
      
      if (w->cursor.vpos >= 0)
	{
	  if (!just_this_one_p 
	      || current_buffer->clip_changed
	      || BEG_UNCHANGED < CHARPOS (startp))
	    /* Forget any recorded base line for line number display.  */
	    w->base_line_number = Qnil;
	  
	  if (!make_cursor_line_fully_visible (w))
	    goto need_larger_matrices;
	  goto done;
	}
      else
	clear_glyph_matrix (w->desired_matrix);
    }

 try_to_scroll:

  w->last_modified = make_number (0);
  w->last_overlay_modified = make_number (0);

  /* Redisplay the mode line.  Select the buffer properly for that.  */
  if (!update_mode_line)
    {
      update_mode_line = 1;
      w->update_mode_line = Qt;
    }

  /* Try to scroll by specified few lines.  */
  if ((scroll_conservatively
       || scroll_step
       || temp_scroll_step
       || NUMBERP (current_buffer->scroll_up_aggressively)
       || NUMBERP (current_buffer->scroll_down_aggressively))
      && !current_buffer->clip_changed
      && CHARPOS (startp) >= BEGV 
      && CHARPOS (startp) <= ZV)
    {
      /* The function returns -1 if new fonts were loaded, 1 if
	 successful, 0 if not successful.  */
      int rc = try_scrolling (window, just_this_one_p,
			      scroll_conservatively,
			      scroll_step,
			      temp_scroll_step);
      switch (rc)
	{
	case SCROLLING_SUCCESS:
	  goto done;
	  
	case SCROLLING_NEED_LARGER_MATRICES:
	  goto need_larger_matrices;
	  
	case SCROLLING_FAILED:
	  break;
	  
	default:
	  abort ();
	}
    }

  /* Finally, just choose place to start which centers point */

 recenter:

#if GLYPH_DEBUG
  debug_method_add (w, "recenter");
#endif

  /* w->vscroll = 0; */

  /* Forget any previously recorded base line for line number display.  */
  if (!current_matrix_up_to_date_p
      || current_buffer->clip_changed)
    w->base_line_number = Qnil;

  /* Move backward half the height of the window.  */
  init_iterator (&it, w, PT, PT_BYTE, NULL, DEFAULT_FACE_ID);
  it.current_y = it.last_visible_y;
  move_it_vertically_backward (&it, window_box_height (w) / 2);
  xassert (IT_CHARPOS (it) >= BEGV);

  /* The function move_it_vertically_backward may move over more
     than the specified y-distance.  If it->w is small, e.g. a
     mini-buffer window, we may end up in front of the window's
     display area.  Start displaying at the start of the line
     containing PT in this case.  */
  if (it.current_y <= 0)
    {
      init_iterator (&it, w, PT, PT_BYTE, NULL, DEFAULT_FACE_ID);
      move_it_vertically (&it, 0);
      xassert (IT_CHARPOS (it) <= PT);
      it.current_y = 0;
    }

  it.current_x = it.hpos = 0;
  
  /* Set startp here explicitly in case that helps avoid an infinite loop
     in case the window-scroll-functions functions get errors.  */
  set_marker_both (w->start, Qnil, IT_CHARPOS (it), IT_BYTEPOS (it));

  /* Run scroll hooks.  */
  startp = run_window_scroll_functions (window, it.current.pos);

  /* Redisplay the window.  */
  if (!current_matrix_up_to_date_p
      || windows_or_buffers_changed
      || cursor_type_changed
      /* Don't use try_window_reusing_current_matrix in this case
	 because it can have changed the buffer.  */
      || !NILP (Vwindow_scroll_functions)
      || !just_this_one_p
      || MINI_WINDOW_P (w)
      || !try_window_reusing_current_matrix (w))
    try_window (window, startp);

  /* If new fonts have been loaded (due to fontsets), give up.  We
     have to start a new redisplay since we need to re-adjust glyph
     matrices.  */
  if (fonts_changed_p)
    goto need_larger_matrices;

  /* If cursor did not appear assume that the middle of the window is
     in the first line of the window.  Do it again with the next line.
     (Imagine a window of height 100, displaying two lines of height
     60.  Moving back 50 from it->last_visible_y will end in the first
     line.)  */
  if (w->cursor.vpos < 0)
    {
      if (!NILP (w->window_end_valid)
	  && PT >= Z - XFASTINT (w->window_end_pos))
	{
	  clear_glyph_matrix (w->desired_matrix);
	  move_it_by_lines (&it, 1, 0);
	  try_window (window, it.current.pos);
	}
      else if (PT < IT_CHARPOS (it))
	{
	  clear_glyph_matrix (w->desired_matrix);
	  move_it_by_lines (&it, -1, 0);
	  try_window (window, it.current.pos);
	}
      else
	{
	  /* Not much we can do about it.  */
	}
    }

  /* Consider the following case: Window starts at BEGV, there is
     invisible, intangible text at BEGV, so that display starts at
     some point START > BEGV.  It can happen that we are called with
     PT somewhere between BEGV and START.  Try to handle that case.  */
  if (w->cursor.vpos < 0)
    {
      struct glyph_row *row = w->current_matrix->rows;
      if (row->mode_line_p)
	++row;
      set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
    }
  
  if (!make_cursor_line_fully_visible (w))
    goto need_larger_matrices;

 done:

  SET_TEXT_POS_FROM_MARKER (startp, w->start);
  w->start_at_line_beg = ((CHARPOS (startp) == BEGV
			   || FETCH_BYTE (BYTEPOS (startp) - 1) == '\n')
			  ? Qt : Qnil);

  /* Display the mode line, if we must.  */
  if ((update_mode_line
       /* If window not full width, must redo its mode line
	  if (a) the window to its side is being redone and 
	  (b) we do a frame-based redisplay.  This is a consequence
	  of how inverted lines are drawn in frame-based redisplay.  */
       || (!just_this_one_p 
	   && !FRAME_WINDOW_P (f)
	   && !WINDOW_FULL_WIDTH_P (w))
       /* Line number to display.  */
       || INTEGERP (w->base_line_pos)
       /* Column number is displayed and different from the one displayed.  */
       || (!NILP (w->column_number_displayed)
	   && XFASTINT (w->column_number_displayed) != current_column ()))
       /* This means that the window has a mode line.  */
       && (WINDOW_WANTS_MODELINE_P (w)
	   || WINDOW_WANTS_HEADER_LINE_P (w)))
    {
      display_mode_lines (w);

      /* If mode line height has changed, arrange for a thorough
	 immediate redisplay using the correct mode line height.  */
      if (WINDOW_WANTS_MODELINE_P (w)
	  && CURRENT_MODE_LINE_HEIGHT (w) != DESIRED_MODE_LINE_HEIGHT (w))
	{
	  fonts_changed_p = 1;
	  MATRIX_MODE_LINE_ROW (w->current_matrix)->height
	    = DESIRED_MODE_LINE_HEIGHT (w);
	}
      
      /* If top line height has changed, arrange for a thorough
	 immediate redisplay using the correct mode line height.  */
      if (WINDOW_WANTS_HEADER_LINE_P (w)
	  && CURRENT_HEADER_LINE_HEIGHT (w) != DESIRED_HEADER_LINE_HEIGHT (w))
	{
	  fonts_changed_p = 1;
	  MATRIX_HEADER_LINE_ROW (w->current_matrix)->height
	    = DESIRED_HEADER_LINE_HEIGHT (w);
	}

      if (fonts_changed_p)
	goto need_larger_matrices;
    }

  if (!line_number_displayed
      && !BUFFERP (w->base_line_pos))
    {
      w->base_line_pos = Qnil;
      w->base_line_number = Qnil;
    }

 finish_menu_bars:
  
  /* When we reach a frame's selected window, redo the frame's menu bar.  */
  if (update_mode_line
      && EQ (FRAME_SELECTED_WINDOW (f), window))
    {
      int redisplay_menu_p = 0;

      if (FRAME_WINDOW_P (f))
	{
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) || defined (macintosh)
	  redisplay_menu_p = FRAME_EXTERNAL_MENU_BAR (f);
#else
	  redisplay_menu_p = FRAME_MENU_BAR_LINES (f) > 0;
#endif
	}
      else
        redisplay_menu_p = FRAME_MENU_BAR_LINES (f) > 0;

      if (redisplay_menu_p)
        display_menu_bar (w);

#ifdef HAVE_WINDOW_SYSTEM
      if (WINDOWP (f->tool_bar_window)
	  && (FRAME_TOOL_BAR_LINES (f) > 0
	      || auto_resize_tool_bars_p))
	redisplay_tool_bar (f);
#endif
    }

 need_larger_matrices:
  ;
 finish_scroll_bars:

  if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    {
      int start, end, whole;

      /* Calculate the start and end positions for the current window.
	 At some point, it would be nice to choose between scrollbars
	 which reflect the whole buffer size, with special markers
	 indicating narrowing, and scrollbars which reflect only the
	 visible region.

	 Note that mini-buffers sometimes aren't displaying any text.  */
      if (!MINI_WINDOW_P (w)
	  || (w == XWINDOW (minibuf_window)
	      && NILP (echo_area_buffer[0])))
	{
	  whole = ZV - BEGV;
	  start = marker_position (w->start) - BEGV;
	  /* I don't think this is guaranteed to be right.  For the
	     moment, we'll pretend it is.  */
	  end = (Z - XFASTINT (w->window_end_pos)) - BEGV;

	  if (end < start) 
	    end = start;
	  if (whole < (end - start)) 
	    whole = end - start;
	}
      else
	start = end = whole = 0;

      /* Indicate what this scroll bar ought to be displaying now.  */
      set_vertical_scroll_bar_hook (w, end - start, whole, start);

      /* Note that we actually used the scroll bar attached to this
	 window, so it shouldn't be deleted at the end of redisplay.  */
      redeem_scroll_bar_hook (w);
    }

  /* Restore current_buffer and value of point in it.  */
  TEMP_SET_PT_BOTH (CHARPOS (opoint), BYTEPOS (opoint));
  set_buffer_internal_1 (old);
  TEMP_SET_PT_BOTH (CHARPOS (lpoint), BYTEPOS (lpoint));

  unbind_to (count, Qnil);
}


/* Build the complete desired matrix of WINDOW with a window start
   buffer position POS.  Value is non-zero if successful.  It is zero
   if fonts were loaded during redisplay which makes re-adjusting
   glyph matrices necessary.  */

int
try_window (window, pos)
     Lisp_Object window;
     struct text_pos pos;
{
  struct window *w = XWINDOW (window);
  struct it it;
  struct glyph_row *last_text_row = NULL;

  /* Make POS the new window start.  */
  set_marker_both (w->start, Qnil, CHARPOS (pos), BYTEPOS (pos));

  /* Mark cursor position as unknown.  No overlay arrow seen.  */
  w->cursor.vpos = -1;
  overlay_arrow_seen = 0;

  /* Initialize iterator and info to start at POS.  */
  start_display (&it, w, pos);

  /* Display all lines of W.  */
  while (it.current_y < it.last_visible_y)
    {
      if (display_line (&it))
	last_text_row = it.glyph_row - 1;
      if (fonts_changed_p)
	return 0;
    }

  /* If bottom moved off end of frame, change mode line percentage.  */
  if (XFASTINT (w->window_end_pos) <= 0
      && Z != IT_CHARPOS (it))
    w->update_mode_line = Qt;

  /* Set window_end_pos to the offset of the last character displayed
     on the window from the end of current_buffer.  Set
     window_end_vpos to its row number.  */
  if (last_text_row)
    {
      xassert (MATRIX_ROW_DISPLAYS_TEXT_P (last_text_row));
      w->window_end_bytepos
	= Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row);
      w->window_end_pos
	= make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row));
      w->window_end_vpos
	= make_number (MATRIX_ROW_VPOS (last_text_row, w->desired_matrix));
      xassert (MATRIX_ROW (w->desired_matrix, XFASTINT (w->window_end_vpos))
	       ->displays_text_p);
    }
  else
    {
      w->window_end_bytepos = 0;
      w->window_end_pos = w->window_end_vpos = make_number (0);
    }
  
  /* But that is not valid info until redisplay finishes.  */
  w->window_end_valid = Qnil;
  return 1;
}



/************************************************************************
    Window redisplay reusing current matrix when buffer has not changed
 ************************************************************************/

/* Try redisplay of window W showing an unchanged buffer with a
   different window start than the last time it was displayed by
   reusing its current matrix.  Value is non-zero if successful.
   W->start is the new window start.  */

static int
try_window_reusing_current_matrix (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  struct glyph_row *row, *bottom_row;
  struct it it;
  struct run run;
  struct text_pos start, new_start;
  int nrows_scrolled, i;
  struct glyph_row *last_text_row;
  struct glyph_row *last_reused_text_row;
  struct glyph_row *start_row;
  int start_vpos, min_y, max_y;

#if GLYPH_DEBUG
  if (inhibit_try_window_reusing)
    return 0;
#endif

  if (/* This function doesn't handle terminal frames.  */
      !FRAME_WINDOW_P (f)
      /* Don't try to reuse the display if windows have been split
	 or such.  */
      || windows_or_buffers_changed
      || cursor_type_changed)
    return 0;

  /* Can't do this if region may have changed.  */
  if ((!NILP (Vtransient_mark_mode)
       && !NILP (current_buffer->mark_active))
      || !NILP (w->region_showing)
      || !NILP (Vshow_trailing_whitespace))
    return 0;

  /* If top-line visibility has changed, give up.  */
  if (WINDOW_WANTS_HEADER_LINE_P (w)
      != MATRIX_HEADER_LINE_ROW (w->current_matrix)->mode_line_p)
    return 0;

  /* Give up if old or new display is scrolled vertically.  We could
     make this function handle this, but right now it doesn't.  */
  start_row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  if (w->vscroll || MATRIX_ROW_PARTIALLY_VISIBLE_P (start_row))
    return 0;

  /* The variable new_start now holds the new window start.  The old
     start `start' can be determined from the current matrix.  */
  SET_TEXT_POS_FROM_MARKER (new_start, w->start);
  start = start_row->start.pos;
  start_vpos = MATRIX_ROW_VPOS (start_row, w->current_matrix);

  /* Clear the desired matrix for the display below.  */
  clear_glyph_matrix (w->desired_matrix);
  
  if (CHARPOS (new_start) <= CHARPOS (start))
    {
      int first_row_y;
      
      /* Don't use this method if the display starts with an ellipsis
	 displayed for invisible text.  It's not easy to handle that case
	 below, and it's certainly not worth the effort since this is
	 not a frequent case.  */
      if (in_ellipses_for_invisible_text_p (&start_row->start, w))
	return 0;

      IF_DEBUG (debug_method_add (w, "twu1"));
      
      /* Display up to a row that can be reused.  The variable
	 last_text_row is set to the last row displayed that displays
	 text.  Note that it.vpos == 0 if or if not there is a
         header-line; it's not the same as the MATRIX_ROW_VPOS!  */
      start_display (&it, w, new_start);
      first_row_y = it.current_y;
      w->cursor.vpos = -1;
      last_text_row = last_reused_text_row = NULL;
      
      while (it.current_y < it.last_visible_y
	     && IT_CHARPOS (it) < CHARPOS (start)
	     && !fonts_changed_p)
	if (display_line (&it))
	  last_text_row = it.glyph_row - 1;

      /* A value of current_y < last_visible_y means that we stopped
	 at the previous window start, which in turn means that we
	 have at least one reusable row.  */
      if (it.current_y < it.last_visible_y)
	{
	  /* IT.vpos always starts from 0; it counts text lines.  */
	  nrows_scrolled = it.vpos;
	  
	  /* Find PT if not already found in the lines displayed.  */
	  if (w->cursor.vpos < 0)
	    {
	      int dy = it.current_y - first_row_y;
	      
	      row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
	      row = row_containing_pos (w, PT, row, NULL, dy);
	      if (row)
		set_cursor_from_row (w, row, w->current_matrix, 0, 0,
				     dy, nrows_scrolled);
	      else
		{
		  clear_glyph_matrix (w->desired_matrix);
		  return 0;
		}
	    }

	  /* Scroll the display.  Do it before the current matrix is
	     changed.  The problem here is that update has not yet
	     run, i.e. part of the current matrix is not up to date.
	     scroll_run_hook will clear the cursor, and use the
	     current matrix to get the height of the row the cursor is
	     in.  */
	  run.current_y = first_row_y;
	  run.desired_y = it.current_y;
	  run.height = it.last_visible_y - it.current_y;

	  if (run.height > 0 && run.current_y != run.desired_y)
	    {
	      update_begin (f);
	      rif->update_window_begin_hook (w);
	      rif->clear_mouse_face (w);
	      rif->scroll_run_hook (w, &run);
	      rif->update_window_end_hook (w, 0, 0);
	      update_end (f);
	    }

	  /* Shift current matrix down by nrows_scrolled lines.  */
	  bottom_row = MATRIX_BOTTOM_TEXT_ROW (w->current_matrix, w);
	  rotate_matrix (w->current_matrix,
			 start_vpos,
			 MATRIX_ROW_VPOS (bottom_row, w->current_matrix),
			 nrows_scrolled);
	  
	  /* Disable lines that must be updated.  */
	  for (i = 0; i < it.vpos; ++i)
	    (start_row + i)->enabled_p = 0;

	  /* Re-compute Y positions.  */
	  min_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
	  max_y = it.last_visible_y;
	  for (row = start_row + nrows_scrolled;
	       row < bottom_row;
	       ++row)
	    {
	      row->y = it.current_y;
	      row->visible_height = row->height;

	      if (row->y < min_y)
		row->visible_height -= min_y - row->y;
	      if (row->y + row->height > max_y)
		row->visible_height -= row->y + row->height - max_y;
	      
	      it.current_y += row->height;

	      if (MATRIX_ROW_DISPLAYS_TEXT_P (row))
		last_reused_text_row = row;
	      if (MATRIX_ROW_BOTTOM_Y (row) >= it.last_visible_y)
		break;
	    }
	  
	  /* Disable lines in the current matrix which are now
	     below the window.  */
	  for (++row; row < bottom_row; ++row)
	    row->enabled_p = 0;
	}

      /* Update window_end_pos etc.; last_reused_text_row is the last
	 reused row from the current matrix containing text, if any.
	 The value of last_text_row is the last displayed line
	 containing text.  */
      if (last_reused_text_row)
	{
	  w->window_end_bytepos
	    = Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_reused_text_row);
	  w->window_end_pos
	    = make_number (Z - MATRIX_ROW_END_CHARPOS (last_reused_text_row));
	  w->window_end_vpos
	    = make_number (MATRIX_ROW_VPOS (last_reused_text_row,
					    w->current_matrix));
	}
      else if (last_text_row)
	{
	  w->window_end_bytepos
	    = Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row);
	  w->window_end_pos
	    = make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row));
	  w->window_end_vpos
	    = make_number (MATRIX_ROW_VPOS (last_text_row, w->desired_matrix));
	}
      else
	{
	  /* This window must be completely empty.  */
	  w->window_end_bytepos = 0;
	  w->window_end_pos = w->window_end_vpos = make_number (0);
	}
      w->window_end_valid = Qnil;

      /* Update hint: don't try scrolling again in update_window.  */
      w->desired_matrix->no_scrolling_p = 1;
      
#if GLYPH_DEBUG
      debug_method_add (w, "try_window_reusing_current_matrix 1");
#endif
      return 1;
    }
  else if (CHARPOS (new_start) > CHARPOS (start))
    {
      struct glyph_row *pt_row, *row;
      struct glyph_row *first_reusable_row;
      struct glyph_row *first_row_to_display;
      int dy;
      int yb = window_text_bottom_y (w);

      /* Find the row starting at new_start, if there is one.  Don't
	 reuse a partially visible line at the end.  */
      first_reusable_row = start_row;
      while (first_reusable_row->enabled_p
	     && MATRIX_ROW_BOTTOM_Y (first_reusable_row) < yb
	     && (MATRIX_ROW_START_CHARPOS (first_reusable_row)
		 < CHARPOS (new_start)))
	++first_reusable_row;

      /* Give up if there is no row to reuse.  */
      if (MATRIX_ROW_BOTTOM_Y (first_reusable_row) >= yb
	  || !first_reusable_row->enabled_p
	  || (MATRIX_ROW_START_CHARPOS (first_reusable_row)
	      != CHARPOS (new_start)))
	return 0;

      /* We can reuse fully visible rows beginning with
         first_reusable_row to the end of the window.  Set
         first_row_to_display to the first row that cannot be reused.
         Set pt_row to the row containing point, if there is any.  */
      pt_row = NULL;
      for (first_row_to_display = first_reusable_row;
	   MATRIX_ROW_BOTTOM_Y (first_row_to_display) < yb;
	   ++first_row_to_display)
	{
	  if (PT >= MATRIX_ROW_START_CHARPOS (first_row_to_display)
	      && PT < MATRIX_ROW_END_CHARPOS (first_row_to_display))
	    pt_row = first_row_to_display;
	}

      /* Start displaying at the start of first_row_to_display.  */
      xassert (first_row_to_display->y < yb);
      init_to_row_start (&it, w, first_row_to_display);

      nrows_scrolled = (MATRIX_ROW_VPOS (first_reusable_row, w->current_matrix)
			- start_vpos);
      it.vpos = (MATRIX_ROW_VPOS (first_row_to_display, w->current_matrix)
		 - nrows_scrolled);
      it.current_y = (first_row_to_display->y - first_reusable_row->y
		      + WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w));

      /* Display lines beginning with first_row_to_display in the
         desired matrix.  Set last_text_row to the last row displayed
         that displays text.  */
      it.glyph_row = MATRIX_ROW (w->desired_matrix, it.vpos);
      if (pt_row == NULL)
	w->cursor.vpos = -1;
      last_text_row = NULL;
      while (it.current_y < it.last_visible_y && !fonts_changed_p)
	if (display_line (&it))
	  last_text_row = it.glyph_row - 1;

      /* Give up If point isn't in a row displayed or reused.  */
      if (w->cursor.vpos < 0)
	{
	  clear_glyph_matrix (w->desired_matrix);
	  return 0;
	}

      /* If point is in a reused row, adjust y and vpos of the cursor
	 position.  */
      if (pt_row)
	{
	  w->cursor.vpos -= MATRIX_ROW_VPOS (first_reusable_row,
					     w->current_matrix);
	  w->cursor.y -= first_reusable_row->y;
	}

      /* Scroll the display.  */
      run.current_y = first_reusable_row->y;
      run.desired_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
      run.height = it.last_visible_y - run.current_y;
      dy = run.current_y - run.desired_y;
      
      if (run.height)
	{
	  struct frame *f = XFRAME (WINDOW_FRAME (w));
	  update_begin (f);
	  rif->update_window_begin_hook (w);
	  rif->clear_mouse_face (w);
	  rif->scroll_run_hook (w, &run);
	  rif->update_window_end_hook (w, 0, 0);
	  update_end (f);
	}

      /* Adjust Y positions of reused rows.  */
      bottom_row = MATRIX_BOTTOM_TEXT_ROW (w->current_matrix, w);
      min_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
      max_y = it.last_visible_y;
      for (row = first_reusable_row; row < first_row_to_display; ++row)
	{
	  row->y -= dy;
	  row->visible_height = row->height;
	  if (row->y < min_y)
	    row->visible_height -= min_y - row->y;
	  if (row->y + row->height > max_y)
	    row->visible_height -= row->y + row->height - max_y;
	}

      /* Scroll the current matrix.  */
      xassert (nrows_scrolled > 0);
      rotate_matrix (w->current_matrix,
		     start_vpos,
		     MATRIX_ROW_VPOS (bottom_row, w->current_matrix),
		     -nrows_scrolled);

      /* Disable rows not reused.  */
      for (row -= nrows_scrolled; row < bottom_row; ++row)
	row->enabled_p = 0;

      /* Adjust window end.  A null value of last_text_row means that
	 the window end is in reused rows which in turn means that
	 only its vpos can have changed.  */
      if (last_text_row)
	{
	  w->window_end_bytepos
	    = Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row);
	  w->window_end_pos
	    = make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row));
	  w->window_end_vpos
	    = make_number (MATRIX_ROW_VPOS (last_text_row, w->desired_matrix));
	}
      else
	{
	  w->window_end_vpos
	    = make_number (XFASTINT (w->window_end_vpos) - nrows_scrolled);
	}
      
      w->window_end_valid = Qnil;
      w->desired_matrix->no_scrolling_p = 1;

#if GLYPH_DEBUG
      debug_method_add (w, "try_window_reusing_current_matrix 2");
#endif
      return 1;
    }
  
  return 0;
}



/************************************************************************
   Window redisplay reusing current matrix when buffer has changed
 ************************************************************************/

static struct glyph_row *find_last_unchanged_at_beg_row P_ ((struct window *));
static struct glyph_row *find_first_unchanged_at_end_row P_ ((struct window *,
							     int *, int *));
static struct glyph_row *
find_last_row_displaying_text P_ ((struct glyph_matrix *, struct it *,
				   struct glyph_row *));


/* Return the last row in MATRIX displaying text.  If row START is
   non-null, start searching with that row.  IT gives the dimensions
   of the display.  Value is null if matrix is empty; otherwise it is
   a pointer to the row found.  */

static struct glyph_row *
find_last_row_displaying_text (matrix, it, start)
     struct glyph_matrix *matrix;
     struct it *it;
     struct glyph_row *start;
{
  struct glyph_row *row, *row_found;

  /* Set row_found to the last row in IT->w's current matrix
     displaying text.  The loop looks funny but think of partially
     visible lines.  */
  row_found = NULL;
  row = start ? start : MATRIX_FIRST_TEXT_ROW (matrix);
  while (MATRIX_ROW_DISPLAYS_TEXT_P (row))
    {
      xassert (row->enabled_p);
      row_found = row;
      if (MATRIX_ROW_BOTTOM_Y (row) >= it->last_visible_y)
	break;
      ++row;
    }
  
  return row_found;
}


/* Return the last row in the current matrix of W that is not affected
   by changes at the start of current_buffer that occurred since W's
   current matrix was built.  Value is null if no such row exists.

   BEG_UNCHANGED us the number of characters unchanged at the start of
   current_buffer.  BEG + BEG_UNCHANGED is the buffer position of the
   first changed character in current_buffer.  Characters at positions <
   BEG + BEG_UNCHANGED are at the same buffer positions as they were
   when the current matrix was built.  */

static struct glyph_row *
find_last_unchanged_at_beg_row (w)
     struct window *w;
{
  int first_changed_pos = BEG + BEG_UNCHANGED;
  struct glyph_row *row;
  struct glyph_row *row_found = NULL;
  int yb = window_text_bottom_y (w);

  /* Find the last row displaying unchanged text.  */
  row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  while (MATRIX_ROW_DISPLAYS_TEXT_P (row)
	 && MATRIX_ROW_START_CHARPOS (row) < first_changed_pos)
    {
      if (/* If row ends before first_changed_pos, it is unchanged,
	     except in some case.  */
	  MATRIX_ROW_END_CHARPOS (row) <= first_changed_pos
	  /* When row ends in ZV and we write at ZV it is not
             unchanged.  */
	  && !row->ends_at_zv_p
	  /* When first_changed_pos is the end of a continued line,
	     row is not unchanged because it may be no longer
	     continued.  */
	  && !(MATRIX_ROW_END_CHARPOS (row) == first_changed_pos
	       && row->continued_p))
	row_found = row;

      /* Stop if last visible row.  */
     if (MATRIX_ROW_BOTTOM_Y (row) >= yb)
	break;
      
      ++row;
    }

  return row_found;
}


/* Find the first glyph row in the current matrix of W that is not
   affected by changes at the end of current_buffer since the 
   time W's current matrix was built.

   Return in *DELTA the number of chars by which buffer positions in
   unchanged text at the end of current_buffer must be adjusted.
   
   Return in *DELTA_BYTES the corresponding number of bytes.

   Value is null if no such row exists, i.e. all rows are affected by
   changes.  */
   
static struct glyph_row *
find_first_unchanged_at_end_row (w, delta, delta_bytes)
     struct window *w;
     int *delta, *delta_bytes;
{
  struct glyph_row *row;
  struct glyph_row *row_found = NULL;

  *delta = *delta_bytes = 0;

  /* Display must not have been paused, otherwise the current matrix
     is not up to date.  */
  if (NILP (w->window_end_valid))
    abort ();
  
  /* A value of window_end_pos >= END_UNCHANGED means that the window
     end is in the range of changed text.  If so, there is no
     unchanged row at the end of W's current matrix.  */
  if (XFASTINT (w->window_end_pos) >= END_UNCHANGED)
    return NULL;

  /* Set row to the last row in W's current matrix displaying text.  */
  row = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));
  
  /* If matrix is entirely empty, no unchanged row exists.  */ 
  if (MATRIX_ROW_DISPLAYS_TEXT_P (row))
    {
      /* The value of row is the last glyph row in the matrix having a
	 meaningful buffer position in it.  The end position of row
	 corresponds to window_end_pos.  This allows us to translate
	 buffer positions in the current matrix to current buffer
	 positions for characters not in changed text.  */
      int Z_old = MATRIX_ROW_END_CHARPOS (row) + XFASTINT (w->window_end_pos);
      int Z_BYTE_old = MATRIX_ROW_END_BYTEPOS (row) + w->window_end_bytepos;
      int last_unchanged_pos, last_unchanged_pos_old;
      struct glyph_row *first_text_row
	= MATRIX_FIRST_TEXT_ROW (w->current_matrix);

      *delta = Z - Z_old;
      *delta_bytes = Z_BYTE - Z_BYTE_old;

      /* Set last_unchanged_pos to the buffer position of the last
	 character in the buffer that has not been changed.  Z is the
	 index + 1 of the last character in current_buffer, i.e. by
	 subtracting END_UNCHANGED we get the index of the last
	 unchanged character, and we have to add BEG to get its buffer
	 position.  */
      last_unchanged_pos = Z - END_UNCHANGED + BEG;
      last_unchanged_pos_old = last_unchanged_pos - *delta;
      
      /* Search backward from ROW for a row displaying a line that
	 starts at a minimum position >= last_unchanged_pos_old.  */
      for (; row > first_text_row; --row)
	{
	  if (!row->enabled_p || !MATRIX_ROW_DISPLAYS_TEXT_P (row))
	    abort ();
	  
	  if (MATRIX_ROW_START_CHARPOS (row) >= last_unchanged_pos_old)
	    row_found = row;
	}
    }

  if (row_found && !MATRIX_ROW_DISPLAYS_TEXT_P (row_found))
    abort ();
  
  return row_found;
}


/* Make sure that glyph rows in the current matrix of window W
   reference the same glyph memory as corresponding rows in the
   frame's frame matrix.  This function is called after scrolling W's
   current matrix on a terminal frame in try_window_id and
   try_window_reusing_current_matrix.  */

static void
sync_frame_with_window_matrix_rows (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  struct glyph_row *window_row, *window_row_end, *frame_row;

  /* Preconditions: W must be a leaf window and full-width.  Its frame
     must have a frame matrix.  */
  xassert (NILP (w->hchild) && NILP (w->vchild));
  xassert (WINDOW_FULL_WIDTH_P (w));
  xassert (!FRAME_WINDOW_P (f));

  /* If W is a full-width window, glyph pointers in W's current matrix
     have, by definition, to be the same as glyph pointers in the
     corresponding frame matrix.  */
  window_row = w->current_matrix->rows;
  window_row_end = window_row + w->current_matrix->nrows;
  frame_row = f->current_matrix->rows + XFASTINT (w->top);
  while (window_row < window_row_end)
    {
      int area;
      
      for (area = LEFT_MARGIN_AREA; area <= LAST_AREA; ++area)
	frame_row->glyphs[area] = window_row->glyphs[area];

      /* Disable frame rows whose corresponding window rows have
	 been disabled in try_window_id.  */
      if (!window_row->enabled_p)
	frame_row->enabled_p = 0;
      
      ++window_row, ++frame_row;
    }
}


/* Find the glyph row in window W containing CHARPOS.  Consider all
   rows between START and END (not inclusive).  END null means search
   all rows to the end of the display area of W.  Value is the row
   containing CHARPOS or null.  */

struct glyph_row *
row_containing_pos (w, charpos, start, end, dy)
     struct window *w;
     int charpos;
     struct glyph_row *start, *end;
     int dy;
{
  struct glyph_row *row = start;
  int last_y;

  /* If we happen to start on a header-line, skip that.  */
  if (row->mode_line_p)
    ++row;
  
  if ((end && row >= end) || !row->enabled_p)
    return NULL;
  
  last_y = window_text_bottom_y (w) - dy;
      
  while ((end == NULL || row < end)
	 && MATRIX_ROW_BOTTOM_Y (row) < last_y
	 && (MATRIX_ROW_END_CHARPOS (row) < charpos
	     || (MATRIX_ROW_END_CHARPOS (row) == charpos
		 /* The end position of a row equals the start
		    position of the next row.  If CHARPOS is there, we
		    would rather display it in the next line, except
		    when this line ends in ZV.  */
		 && !row->ends_at_zv_p
		 && !MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row))))
    ++row;
      
  /* Give up if CHARPOS not found.  */
  if ((end && row >= end)
      || charpos < MATRIX_ROW_START_CHARPOS (row)
      || charpos > MATRIX_ROW_END_CHARPOS (row))
    row = NULL;

  return row;
}


/* Try to redisplay window W by reusing its existing display.  W's
   current matrix must be up to date when this function is called,
   i.e. window_end_valid must not be nil.

   Value is

   1	if display has been updated
   0	if otherwise unsuccessful
   -1	if redisplay with same window start is known not to succeed

   The following steps are performed:

   1. Find the last row in the current matrix of W that is not
   affected by changes at the start of current_buffer.  If no such row
   is found, give up.

   2. Find the first row in W's current matrix that is not affected by
   changes at the end of current_buffer.  Maybe there is no such row.

   3. Display lines beginning with the row + 1 found in step 1 to the
   row found in step 2 or, if step 2 didn't find a row, to the end of
   the window.

   4. If cursor is not known to appear on the window, give up.

   5. If display stopped at the row found in step 2, scroll the
   display and current matrix as needed.

   6. Maybe display some lines at the end of W, if we must.  This can
   happen under various circumstances, like a partially visible line
   becoming fully visible, or because newly displayed lines are displayed
   in smaller font sizes.

   7. Update W's window end information.  */

static int
try_window_id (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  struct glyph_matrix *current_matrix = w->current_matrix;
  struct glyph_matrix *desired_matrix = w->desired_matrix;
  struct glyph_row *last_unchanged_at_beg_row;
  struct glyph_row *first_unchanged_at_end_row;
  struct glyph_row *row;
  struct glyph_row *bottom_row;
  int bottom_vpos;
  struct it it;
  int delta = 0, delta_bytes = 0, stop_pos, dvpos, dy;
  struct text_pos start_pos;
  struct run run;
  int first_unchanged_at_end_vpos = 0;
  struct glyph_row *last_text_row, *last_text_row_at_end;
  struct text_pos start;
  int first_changed_charpos, last_changed_charpos;

#if GLYPH_DEBUG
  if (inhibit_try_window_id)
    return 0;
#endif

  /* This is handy for debugging.  */
#if 0
#define GIVE_UP(X)						\
  do {								\
    fprintf (stderr, "try_window_id give up %d\n", (X));	\
    return 0;							\
  } while (0)
#else
#define GIVE_UP(X) return 0
#endif
  
  SET_TEXT_POS_FROM_MARKER (start, w->start);

  /* Don't use this for mini-windows because these can show
     messages and mini-buffers, and we don't handle that here.  */
  if (MINI_WINDOW_P (w))
    GIVE_UP (1);
  
  /* This flag is used to prevent redisplay optimizations.  */
  if (windows_or_buffers_changed || cursor_type_changed)
    GIVE_UP (2);
  
  /* Verify that narrowing has not changed.  This flag is also set to prevent
     redisplay optimizations.  It would be nice to further
     reduce the number of cases where this prevents try_window_id.  */
  if (current_buffer->clip_changed)
    GIVE_UP (3);

  /* Window must either use window-based redisplay or be full width.  */
  if (!FRAME_WINDOW_P (f)
      && (!line_ins_del_ok
	  || !WINDOW_FULL_WIDTH_P (w)))
    GIVE_UP (4);

  /* Give up if point is not known NOT to appear in W.  */
  if (PT < CHARPOS (start))
    GIVE_UP (5);

  /* Another way to prevent redisplay optimizations.  */
  if (XFASTINT (w->last_modified) == 0)
    GIVE_UP (6);
  
  /* Verify that window is not hscrolled.  */
  if (XFASTINT (w->hscroll) != 0)
    GIVE_UP (7);
  
  /* Verify that display wasn't paused.  */
  if (NILP (w->window_end_valid))
    GIVE_UP (8);
  
  /* Can't use this if highlighting a region because a cursor movement
     will do more than just set the cursor.  */
  if (!NILP (Vtransient_mark_mode)
      && !NILP (current_buffer->mark_active))
    GIVE_UP (9);

  /* Likewise if highlighting trailing whitespace.  */
  if (!NILP (Vshow_trailing_whitespace))
    GIVE_UP (11);
  
  /* Likewise if showing a region.  */
  if (!NILP (w->region_showing))
    GIVE_UP (10);
  
  /* Can use this if overlay arrow position and or string have changed.  */
  if (!EQ (last_arrow_position, COERCE_MARKER (Voverlay_arrow_position))
      || !EQ (last_arrow_string, Voverlay_arrow_string))
    GIVE_UP (12);

  
  /* Make sure beg_unchanged and end_unchanged are up to date.  Do it
     only if buffer has really changed.  The reason is that the gap is
     initially at Z for freshly visited files.  The code below would
     set end_unchanged to 0 in that case.  */
  if (MODIFF > SAVE_MODIFF
      /* This seems to happen sometimes after saving a buffer.  */
      || BEG_UNCHANGED + END_UNCHANGED > Z_BYTE)
    {
      if (GPT - BEG < BEG_UNCHANGED)
	BEG_UNCHANGED = GPT - BEG;
      if (Z - GPT < END_UNCHANGED)
	END_UNCHANGED = Z - GPT;
    }

  /* The position of the first and last character that has been changed.  */
  first_changed_charpos = BEG + BEG_UNCHANGED;
  last_changed_charpos  = Z - END_UNCHANGED;

  /* If window starts after a line end, and the last change is in
     front of that newline, then changes don't affect the display.
     This case happens with stealth-fontification.  Note that although
     the display is unchanged, glyph positions in the matrix have to
     be adjusted, of course.  */
  row = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));
  if (MATRIX_ROW_DISPLAYS_TEXT_P (row)
      && ((last_changed_charpos < CHARPOS (start)
	   && CHARPOS (start) == BEGV)
	  || (last_changed_charpos < CHARPOS (start) - 1
	      && FETCH_BYTE (BYTEPOS (start) - 1) == '\n')))
    {
      int Z_old, delta, Z_BYTE_old, delta_bytes;
      struct glyph_row *r0;

      /* Compute how many chars/bytes have been added to or removed
	 from the buffer.  */
      Z_old = MATRIX_ROW_END_CHARPOS (row) + XFASTINT (w->window_end_pos);
      Z_BYTE_old = MATRIX_ROW_END_BYTEPOS (row) + w->window_end_bytepos;
      delta = Z - Z_old;
      delta_bytes = Z_BYTE - Z_BYTE_old;
	  
      /* Give up if PT is not in the window.  Note that it already has
	 been checked at the start of try_window_id that PT is not in
	 front of the window start.  */
      if (PT >= MATRIX_ROW_END_CHARPOS (row) + delta)
	GIVE_UP (13);

      /* If window start is unchanged, we can reuse the whole matrix
	 as is, after adjusting glyph positions.  No need to compute
	 the window end again, since its offset from Z hasn't changed.  */
      r0 = MATRIX_FIRST_TEXT_ROW (current_matrix);
      if (CHARPOS (start) == MATRIX_ROW_START_CHARPOS (r0) + delta
	  && BYTEPOS (start) == MATRIX_ROW_START_BYTEPOS (r0) + delta_bytes)
	{
	  /* Adjust positions in the glyph matrix.  */
	  if (delta || delta_bytes)
	    {
	      struct glyph_row *r1
		= MATRIX_BOTTOM_TEXT_ROW (current_matrix, w);
	      increment_matrix_positions (w->current_matrix,
					  MATRIX_ROW_VPOS (r0, current_matrix),
					  MATRIX_ROW_VPOS (r1, current_matrix),
					  delta, delta_bytes);
	    }
      
	  /* Set the cursor.  */
	  row = row_containing_pos (w, PT, r0, NULL, 0);
	  set_cursor_from_row (w, row, current_matrix, 0, 0, 0, 0);
	  return 1;
	}
    }

  /* Handle the case that changes are all below what is displayed in
     the window, and that PT is in the window.  This shortcut cannot
     be taken if ZV is visible in the window, and text has been added
     there that is visible in the window.  */
  if (first_changed_charpos >= MATRIX_ROW_END_CHARPOS (row)
      /* ZV is not visible in the window, or there are no
	 changes at ZV, actually.  */
      && (current_matrix->zv > MATRIX_ROW_END_CHARPOS (row)
	  || first_changed_charpos == last_changed_charpos))
    {
      struct glyph_row *r0;

      /* Give up if PT is not in the window.  Note that it already has
	 been checked at the start of try_window_id that PT is not in
	 front of the window start.  */
      if (PT >= MATRIX_ROW_END_CHARPOS (row))
	GIVE_UP (14);

      /* If window start is unchanged, we can reuse the whole matrix
	 as is, without changing glyph positions since no text has
	 been added/removed in front of the window end.  */
      r0 = MATRIX_FIRST_TEXT_ROW (current_matrix);
      if (TEXT_POS_EQUAL_P (start, r0->start.pos))
	{
	  /* We have to compute the window end anew since text
	     can have been added/removed after it.  */
	  w->window_end_pos
	    = make_number (Z - MATRIX_ROW_END_CHARPOS (row));
	  w->window_end_bytepos
	    = Z_BYTE - MATRIX_ROW_END_BYTEPOS (row);

	  /* Set the cursor.  */
	  row = row_containing_pos (w, PT, r0, NULL, 0);
	  set_cursor_from_row (w, row, current_matrix, 0, 0, 0, 0);
	  return 2;
	}
    }

  /* Give up if window start is in the changed area.
     
     The condition used to read

     (BEG_UNCHANGED + END_UNCHANGED != Z - BEG && ...)

     but why that was tested escapes me at the moment.  */
  if (CHARPOS (start) >= first_changed_charpos
      && CHARPOS (start) <= last_changed_charpos)
    GIVE_UP (15);
  
  /* Check that window start agrees with the start of the first glyph
     row in its current matrix.  Check this after we know the window
     start is not in changed text, otherwise positions would not be
     comparable.  */
  row = MATRIX_FIRST_TEXT_ROW (current_matrix);
  if (!TEXT_POS_EQUAL_P (start, row->start.pos))
    GIVE_UP (16);

  /* Give up if the window ends in strings.  Overlay strings
     at the end are difficult to handle, so don't try.  */
  row = MATRIX_ROW (current_matrix, XFASTINT (w->window_end_vpos));
  if (MATRIX_ROW_START_CHARPOS (row) == MATRIX_ROW_END_CHARPOS (row))
    GIVE_UP (20);

  /* Compute the position at which we have to start displaying new
     lines.  Some of the lines at the top of the window might be
     reusable because they are not displaying changed text.  Find the
     last row in W's current matrix not affected by changes at the
     start of current_buffer.  Value is null if changes start in the
     first line of window.  */
  last_unchanged_at_beg_row = find_last_unchanged_at_beg_row (w);
  if (last_unchanged_at_beg_row)
    {
      /* Avoid starting to display in the moddle of a character, a TAB
	 for instance.  This is easier than to set up the iterator
	 exactly, and it's not a frequent case, so the additional
	 effort wouldn't really pay off.  */
      while ((MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (last_unchanged_at_beg_row)
	      || last_unchanged_at_beg_row->ends_in_newline_from_string_p)
	     && last_unchanged_at_beg_row > w->current_matrix->rows)
	--last_unchanged_at_beg_row;

      if (MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (last_unchanged_at_beg_row))
	GIVE_UP (17);

      if (init_to_row_end (&it, w, last_unchanged_at_beg_row) == 0)
	GIVE_UP (18);
      start_pos = it.current.pos;
      
      /* Start displaying new lines in the desired matrix at the same
	 vpos we would use in the current matrix, i.e. below
	 last_unchanged_at_beg_row.  */
      it.vpos = 1 + MATRIX_ROW_VPOS (last_unchanged_at_beg_row,
				     current_matrix);
      it.glyph_row = MATRIX_ROW (desired_matrix, it.vpos);
      it.current_y = MATRIX_ROW_BOTTOM_Y (last_unchanged_at_beg_row);

      xassert (it.hpos == 0 && it.current_x == 0);
    }
  else
    {
      /* There are no reusable lines at the start of the window.
	 Start displaying in the first line.  */
      start_display (&it, w, start);
      start_pos = it.current.pos;
    }

  /* Find the first row that is not affected by changes at the end of
     the buffer.  Value will be null if there is no unchanged row, in
     which case we must redisplay to the end of the window.  delta
     will be set to the value by which buffer positions beginning with
     first_unchanged_at_end_row have to be adjusted due to text
     changes.  */
  first_unchanged_at_end_row
    = find_first_unchanged_at_end_row (w, &delta, &delta_bytes);
  IF_DEBUG (debug_delta = delta);
  IF_DEBUG (debug_delta_bytes = delta_bytes);
  
  /* Set stop_pos to the buffer position up to which we will have to
     display new lines.  If first_unchanged_at_end_row != NULL, this
     is the buffer position of the start of the line displayed in that
     row.  For first_unchanged_at_end_row == NULL, use 0 to indicate
     that we don't stop at a buffer position.  */
  stop_pos = 0;
  if (first_unchanged_at_end_row)
    {
      xassert (last_unchanged_at_beg_row == NULL
	       || first_unchanged_at_end_row >= last_unchanged_at_beg_row);
      
      /* If this is a continuation line, move forward to the next one
	 that isn't.  Changes in lines above affect this line.
	 Caution: this may move first_unchanged_at_end_row to a row
	 not displaying text.  */
      while (MATRIX_ROW_CONTINUATION_LINE_P (first_unchanged_at_end_row)
	     && MATRIX_ROW_DISPLAYS_TEXT_P (first_unchanged_at_end_row)
	     && (MATRIX_ROW_BOTTOM_Y (first_unchanged_at_end_row)
		 < it.last_visible_y))
	++first_unchanged_at_end_row;

      if (!MATRIX_ROW_DISPLAYS_TEXT_P (first_unchanged_at_end_row)
	  || (MATRIX_ROW_BOTTOM_Y (first_unchanged_at_end_row)
	      >= it.last_visible_y))
	first_unchanged_at_end_row = NULL;
      else
	{
	  stop_pos = (MATRIX_ROW_START_CHARPOS (first_unchanged_at_end_row)
		      + delta);
	  first_unchanged_at_end_vpos
	    = MATRIX_ROW_VPOS (first_unchanged_at_end_row, current_matrix);
	  xassert (stop_pos >= Z - END_UNCHANGED);
	}
    }
  else if (last_unchanged_at_beg_row == NULL)
    GIVE_UP (19);


#if GLYPH_DEBUG
  
  /* Either there is no unchanged row at the end, or the one we have
     now displays text.  This is a necessary condition for the window
     end pos calculation at the end of this function.  */
  xassert (first_unchanged_at_end_row == NULL
	   || MATRIX_ROW_DISPLAYS_TEXT_P (first_unchanged_at_end_row));
  
  debug_last_unchanged_at_beg_vpos
    = (last_unchanged_at_beg_row
       ? MATRIX_ROW_VPOS (last_unchanged_at_beg_row, current_matrix)
       : -1);
  debug_first_unchanged_at_end_vpos = first_unchanged_at_end_vpos;
  
#endif /* GLYPH_DEBUG != 0 */

  
  /* Display new lines.  Set last_text_row to the last new line
     displayed which has text on it, i.e. might end up as being the
     line where the window_end_vpos is.  */
  w->cursor.vpos = -1;
  last_text_row = NULL;
  overlay_arrow_seen = 0;
  while (it.current_y < it.last_visible_y
	 && !fonts_changed_p
	 && (first_unchanged_at_end_row == NULL
	     || IT_CHARPOS (it) < stop_pos))
    {
      if (display_line (&it))
	last_text_row = it.glyph_row - 1;
    }

  if (fonts_changed_p)
    return -1;


  /* Compute differences in buffer positions, y-positions etc.  for
     lines reused at the bottom of the window.  Compute what we can
     scroll.  */
  if (first_unchanged_at_end_row
      /* No lines reused because we displayed everything up to the
         bottom of the window.  */
      && it.current_y < it.last_visible_y)
    {
      dvpos = (it.vpos
	       - MATRIX_ROW_VPOS (first_unchanged_at_end_row,
				  current_matrix));
      dy = it.current_y - first_unchanged_at_end_row->y;
      run.current_y = first_unchanged_at_end_row->y;
      run.desired_y = run.current_y + dy;
      run.height = it.last_visible_y - max (run.current_y, run.desired_y);
    }
  else
    {
      delta = dvpos = dy = run.current_y = run.desired_y = run.height = 0;
      first_unchanged_at_end_row = NULL;
    }
  IF_DEBUG (debug_dvpos = dvpos; debug_dy = dy);


  /* Find the cursor if not already found.  We have to decide whether
     PT will appear on this window (it sometimes doesn't, but this is
     not a very frequent case.)  This decision has to be made before
     the current matrix is altered.  A value of cursor.vpos < 0 means
     that PT is either in one of the lines beginning at
     first_unchanged_at_end_row or below the window.  Don't care for
     lines that might be displayed later at the window end; as
     mentioned, this is not a frequent case.  */
  if (w->cursor.vpos < 0)
    {
      /* Cursor in unchanged rows at the top?  */
      if (PT < CHARPOS (start_pos)
	  && last_unchanged_at_beg_row)
	{
	  row = row_containing_pos (w, PT,
				    MATRIX_FIRST_TEXT_ROW (w->current_matrix),
				    last_unchanged_at_beg_row + 1, 0);
	  if (row)
	    set_cursor_from_row (w, row, w->current_matrix, 0, 0, 0, 0);
	}

      /* Start from first_unchanged_at_end_row looking for PT.  */
      else if (first_unchanged_at_end_row)
	{
	  row = row_containing_pos (w, PT - delta,
				    first_unchanged_at_end_row, NULL, 0);
	  if (row)
	    set_cursor_from_row (w, row, w->current_matrix, delta,
				 delta_bytes, dy, dvpos);
	}

      /* Give up if cursor was not found.  */
      if (w->cursor.vpos < 0)
	{
	  clear_glyph_matrix (w->desired_matrix);
	  return -1;
	}
    }
  
  /* Don't let the cursor end in the scroll margins.  */
  {
    int this_scroll_margin, cursor_height;
    
    this_scroll_margin = max (0, scroll_margin);
    this_scroll_margin = min (this_scroll_margin,
			      XFASTINT (w->height) / 4);
    this_scroll_margin *= CANON_Y_UNIT (it.f);
    cursor_height = MATRIX_ROW (w->desired_matrix, w->cursor.vpos)->height;
    
    if ((w->cursor.y < this_scroll_margin
	 && CHARPOS (start) > BEGV)
	/* Don't take scroll margin into account at the bottom because
	   old redisplay didn't do it either.  */
	|| w->cursor.y + cursor_height > it.last_visible_y)
      {
	w->cursor.vpos = -1;
	clear_glyph_matrix (w->desired_matrix);
	return -1;
      }
  }

  /* Scroll the display.  Do it before changing the current matrix so
     that xterm.c doesn't get confused about where the cursor glyph is
     found.  */
  if (dy && run.height)
    {
      update_begin (f);
	  
      if (FRAME_WINDOW_P (f))
	{
	  rif->update_window_begin_hook (w);
	  rif->clear_mouse_face (w);
	  rif->scroll_run_hook (w, &run);
	  rif->update_window_end_hook (w, 0, 0);
	}
      else
	{
	  /* Terminal frame.  In this case, dvpos gives the number of
	     lines to scroll by; dvpos < 0 means scroll up.  */
	  int first_unchanged_at_end_vpos
	    = MATRIX_ROW_VPOS (first_unchanged_at_end_row, w->current_matrix);
	  int from = XFASTINT (w->top) + first_unchanged_at_end_vpos;
	  int end = (XFASTINT (w->top)
		     + (WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0)
		     + window_internal_height (w));
	  
	  /* Perform the operation on the screen.  */
	  if (dvpos > 0)
	    {
	      /* Scroll last_unchanged_at_beg_row to the end of the
		 window down dvpos lines.  */
	      set_terminal_window (end);

	      /* On dumb terminals delete dvpos lines at the end
		 before inserting dvpos empty lines.  */
	      if (!scroll_region_ok)
		ins_del_lines (end - dvpos, -dvpos);

	      /* Insert dvpos empty lines in front of
                 last_unchanged_at_beg_row.  */
	      ins_del_lines (from, dvpos);
	    }
	  else if (dvpos < 0)
	    {
	      /* Scroll up last_unchanged_at_beg_vpos to the end of
		 the window to last_unchanged_at_beg_vpos - |dvpos|.  */
	      set_terminal_window (end);

	      /* Delete dvpos lines in front of
		 last_unchanged_at_beg_vpos.  ins_del_lines will set
		 the cursor to the given vpos and emit |dvpos| delete
		 line sequences.  */
	      ins_del_lines (from + dvpos, dvpos);

	      /* On a dumb terminal insert dvpos empty lines at the
                 end.  */
	      if (!scroll_region_ok)
		ins_del_lines (end + dvpos, -dvpos);
	    }
	  
	  set_terminal_window (0);
	}
      
      update_end (f);
    }

  /* Shift reused rows of the current matrix to the right position.
     BOTTOM_ROW is the last + 1 row in the current matrix reserved for
     text.  */
  bottom_row = MATRIX_BOTTOM_TEXT_ROW (current_matrix, w);
  bottom_vpos = MATRIX_ROW_VPOS (bottom_row, current_matrix);
  if (dvpos < 0)
    {
      rotate_matrix (current_matrix, first_unchanged_at_end_vpos + dvpos,
		     bottom_vpos, dvpos);
      enable_glyph_matrix_rows (current_matrix, bottom_vpos + dvpos,
				bottom_vpos, 0);
    }
  else if (dvpos > 0)
    {
      rotate_matrix (current_matrix, first_unchanged_at_end_vpos,
		     bottom_vpos, dvpos);
      enable_glyph_matrix_rows (current_matrix, first_unchanged_at_end_vpos,
				first_unchanged_at_end_vpos + dvpos, 0);
    }

  /* For frame-based redisplay, make sure that current frame and window
     matrix are in sync with respect to glyph memory.  */
  if (!FRAME_WINDOW_P (f))
    sync_frame_with_window_matrix_rows (w);

  /* Adjust buffer positions in reused rows.  */
  if (delta)
    increment_matrix_positions (current_matrix,
				first_unchanged_at_end_vpos + dvpos,
				bottom_vpos, delta, delta_bytes);

  /* Adjust Y positions.  */
  if (dy)
    shift_glyph_matrix (w, current_matrix,
			first_unchanged_at_end_vpos + dvpos,
			bottom_vpos, dy);

  if (first_unchanged_at_end_row)
    first_unchanged_at_end_row += dvpos;

  /* If scrolling up, there may be some lines to display at the end of
     the window.  */
  last_text_row_at_end = NULL;
  if (dy < 0)
    {
      /* Scrolling up can leave for example a partially visible line
	 at the end of the window to be redisplayed.  */
      /* Set last_row to the glyph row in the current matrix where the
	 window end line is found.  It has been moved up or down in
	 the matrix by dvpos.  */
      int last_vpos = XFASTINT (w->window_end_vpos) + dvpos;
      struct glyph_row *last_row = MATRIX_ROW (current_matrix, last_vpos);

      /* If last_row is the window end line, it should display text.  */
      xassert (last_row->displays_text_p);

      /* If window end line was partially visible before, begin
	 displaying at that line.  Otherwise begin displaying with the
	 line following it.  */
      if (MATRIX_ROW_BOTTOM_Y (last_row) - dy >= it.last_visible_y)
	{
	  init_to_row_start (&it, w, last_row);
	  it.vpos = last_vpos;
	  it.current_y = last_row->y;
	}
      else
	{
	  init_to_row_end (&it, w, last_row);
	  it.vpos = 1 + last_vpos;
	  it.current_y = MATRIX_ROW_BOTTOM_Y (last_row);
	  ++last_row;
	}

      /* We may start in a continuation line.  If so, we have to
	 get the right continuation_lines_width and current_x.  */
      it.continuation_lines_width = last_row->continuation_lines_width;
      it.hpos = it.current_x = 0;
      
      /* Display the rest of the lines at the window end.  */
      it.glyph_row = MATRIX_ROW (desired_matrix, it.vpos);
      while (it.current_y < it.last_visible_y
	     && !fonts_changed_p)
	{
	  /* Is it always sure that the display agrees with lines in
	     the current matrix?  I don't think so, so we mark rows
	     displayed invalid in the current matrix by setting their
	     enabled_p flag to zero.  */
	  MATRIX_ROW (w->current_matrix, it.vpos)->enabled_p = 0;
	  if (display_line (&it))
	    last_text_row_at_end = it.glyph_row - 1;
	}
    }

  /* Update window_end_pos and window_end_vpos.  */
  if (first_unchanged_at_end_row
      && first_unchanged_at_end_row->y < it.last_visible_y
      && !last_text_row_at_end)
    {
      /* Window end line if one of the preserved rows from the current
	 matrix.  Set row to the last row displaying text in current
	 matrix starting at first_unchanged_at_end_row, after
	 scrolling.  */
      xassert (first_unchanged_at_end_row->displays_text_p);
      row = find_last_row_displaying_text (w->current_matrix, &it,
					   first_unchanged_at_end_row);
      xassert (row && MATRIX_ROW_DISPLAYS_TEXT_P (row));
      
      w->window_end_pos = make_number (Z - MATRIX_ROW_END_CHARPOS (row));
      w->window_end_bytepos = Z_BYTE - MATRIX_ROW_END_BYTEPOS (row);
      w->window_end_vpos
	= make_number (MATRIX_ROW_VPOS (row, w->current_matrix));
      xassert (w->window_end_bytepos >= 0);
      IF_DEBUG (debug_method_add (w, "A"));
    }
  else if (last_text_row_at_end)
    {
      w->window_end_pos
	= make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row_at_end));
      w->window_end_bytepos
	= Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row_at_end);
      w->window_end_vpos
	= make_number (MATRIX_ROW_VPOS (last_text_row_at_end, desired_matrix));
      xassert (w->window_end_bytepos >= 0);
      IF_DEBUG (debug_method_add (w, "B"));
    }
  else if (last_text_row)
    {
      /* We have displayed either to the end of the window or at the
	 end of the window, i.e. the last row with text is to be found
	 in the desired matrix.  */
      w->window_end_pos
	= make_number (Z - MATRIX_ROW_END_CHARPOS (last_text_row));
      w->window_end_bytepos
	= Z_BYTE - MATRIX_ROW_END_BYTEPOS (last_text_row);
      w->window_end_vpos
	= make_number (MATRIX_ROW_VPOS (last_text_row, desired_matrix));
      xassert (w->window_end_bytepos >= 0);
    }
  else if (first_unchanged_at_end_row == NULL
	   && last_text_row == NULL
	   && last_text_row_at_end == NULL)
    {
      /* Displayed to end of window, but no line containing text was
	 displayed.  Lines were deleted at the end of the window.  */
      int first_vpos = WINDOW_WANTS_HEADER_LINE_P (w) ? 1 : 0;
      int vpos = XFASTINT (w->window_end_vpos);
      struct glyph_row *current_row = current_matrix->rows + vpos;
      struct glyph_row *desired_row = desired_matrix->rows + vpos;

      for (row = NULL;
	   row == NULL && vpos >= first_vpos;
	   --vpos, --current_row, --desired_row)
	{
	  if (desired_row->enabled_p)
	    {
	      if (desired_row->displays_text_p)
		row = desired_row;
	    }
	  else if (current_row->displays_text_p)
	    row  = current_row;
	}

      xassert (row != NULL);
      w->window_end_vpos = make_number (vpos + 1);
      w->window_end_pos = make_number (Z - MATRIX_ROW_END_CHARPOS (row));
      w->window_end_bytepos = Z_BYTE - MATRIX_ROW_END_BYTEPOS (row);
      xassert (w->window_end_bytepos >= 0);
      IF_DEBUG (debug_method_add (w, "C"));
    }
  else
    abort ();

#if 0 /* This leads to problems, for instance when the cursor is
	 at ZV, and the cursor line displays no text.  */
  /* Disable rows below what's displayed in the window.  This makes
     debugging easier.  */
  enable_glyph_matrix_rows (current_matrix,
			    XFASTINT (w->window_end_vpos) + 1,
			    bottom_vpos, 0);
#endif

  IF_DEBUG (debug_end_pos = XFASTINT (w->window_end_pos);
	    debug_end_vpos = XFASTINT (w->window_end_vpos));

  /* Record that display has not been completed.  */
  w->window_end_valid = Qnil;
  w->desired_matrix->no_scrolling_p = 1;
  return 3;

#undef GIVE_UP
}



/***********************************************************************
			More debugging support
 ***********************************************************************/

#if GLYPH_DEBUG

void dump_glyph_row P_ ((struct glyph_row *, int, int));
void dump_glyph_matrix P_ ((struct glyph_matrix *, int));
void dump_glyph P_ ((struct glyph_row *, struct glyph *, int));


/* Dump the contents of glyph matrix MATRIX on stderr.

   GLYPHS 0 means don't show glyph contents.
   GLYPHS 1 means show glyphs in short form
   GLYPHS > 1 means show glyphs in long form.  */

void
dump_glyph_matrix (matrix, glyphs)
     struct glyph_matrix *matrix;
     int glyphs;
{
  int i;
  for (i = 0; i < matrix->nrows; ++i)
    dump_glyph_row (MATRIX_ROW (matrix, i), i, glyphs);
}


/* Dump contents of glyph GLYPH to stderr.  ROW and AREA are
   the glyph row and area where the glyph comes from.  */

void
dump_glyph (row, glyph, area)
     struct glyph_row *row;
     struct glyph *glyph;
     int area;
{
  if (glyph->type == CHAR_GLYPH)
    {
      fprintf (stderr,
	       "  %5d %4c %6d %c %3d 0x%05x %c %4d %1.1d%1.1d\n",
	       glyph - row->glyphs[TEXT_AREA],
	       'C',
	       glyph->charpos,
	       (BUFFERP (glyph->object)
		? 'B'
		: (STRINGP (glyph->object)
		   ? 'S'
		   : '-')),
	       glyph->pixel_width,
	       glyph->u.ch,
	       (glyph->u.ch < 0x80 && glyph->u.ch >= ' '
		? glyph->u.ch
		: '.'),
	       glyph->face_id,
	       glyph->left_box_line_p,
	       glyph->right_box_line_p);
    }
  else if (glyph->type == STRETCH_GLYPH)
    {
      fprintf (stderr,
	       "  %5d %4c %6d %c %3d 0x%05x %c %4d %1.1d%1.1d\n",
	       glyph - row->glyphs[TEXT_AREA],
	       'S',
	       glyph->charpos,
	       (BUFFERP (glyph->object)
		? 'B'
		: (STRINGP (glyph->object)
		   ? 'S'
		   : '-')),
	       glyph->pixel_width,
	       0,
	       '.',
	       glyph->face_id,
	       glyph->left_box_line_p,
	       glyph->right_box_line_p);
    }
  else if (glyph->type == IMAGE_GLYPH)
    {
      fprintf (stderr,
	       "  %5d %4c %6d %c %3d 0x%05x %c %4d %1.1d%1.1d\n",
	       glyph - row->glyphs[TEXT_AREA],
	       'I',
	       glyph->charpos,
	       (BUFFERP (glyph->object)
		? 'B'
		: (STRINGP (glyph->object)
		   ? 'S'
		   : '-')),
	       glyph->pixel_width,
	       glyph->u.img_id,
	       '.',
	       glyph->face_id,
	       glyph->left_box_line_p,
	       glyph->right_box_line_p);
    }
}


/* Dump the contents of glyph row at VPOS in MATRIX to stderr.
   GLYPHS 0 means don't show glyph contents.
   GLYPHS 1 means show glyphs in short form
   GLYPHS > 1 means show glyphs in long form.  */

void
dump_glyph_row (row, vpos, glyphs)
     struct glyph_row *row;
     int vpos, glyphs;
{
  if (glyphs != 1)
    {
      fprintf (stderr, "Row Start   End Used oEI><O\\CTZFesm     X    Y    W    H    V    A    P\n");
      fprintf (stderr, "=======================================================================\n");
  
      fprintf (stderr, "%3d %5d %5d %4d %1.1d%1.1d%1.1d%1.1d%1.1d\
%1.1d%1.1d%1.1d%1.1d%1.1d%1.1d%1.1d%1.1d  %4d %4d %4d %4d %4d %4d %4d\n",
	       vpos,
	       MATRIX_ROW_START_CHARPOS (row),
	       MATRIX_ROW_END_CHARPOS (row),
	       row->used[TEXT_AREA],
	       row->contains_overlapping_glyphs_p,
	       row->enabled_p,
	       row->truncated_on_left_p,
	       row->truncated_on_right_p,
	       row->overlay_arrow_p,
	       row->continued_p,
	       MATRIX_ROW_CONTINUATION_LINE_P (row),
	       row->displays_text_p,
	       row->ends_at_zv_p,
	       row->fill_line_p,
	       row->ends_in_middle_of_char_p,
	       row->starts_in_middle_of_char_p,
	       row->mouse_face_p,
	       row->x,
	       row->y,
	       row->pixel_width,
	       row->height,
	       row->visible_height,
	       row->ascent,
	       row->phys_ascent);
      fprintf (stderr, "%9d %5d\t%5d\n", row->start.overlay_string_index,
	       row->end.overlay_string_index,
	       row->continuation_lines_width);
      fprintf (stderr, "%9d %5d\n",
	       CHARPOS (row->start.string_pos),
	       CHARPOS (row->end.string_pos));
      fprintf (stderr, "%9d %5d\n", row->start.dpvec_index,
	       row->end.dpvec_index);
    }
  
  if (glyphs > 1)
    {
      int area;

      for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	{
	  struct glyph *glyph = row->glyphs[area];
	  struct glyph *glyph_end = glyph + row->used[area];
      
	  /* Glyph for a line end in text.  */
	  if (area == TEXT_AREA && glyph == glyph_end && glyph->charpos > 0)
	    ++glyph_end;
      
	  if (glyph < glyph_end)
	    fprintf (stderr, "  Glyph    Type Pos   O W    Code C Face LR\n");
      
	  for (; glyph < glyph_end; ++glyph)
	    dump_glyph (row, glyph, area);
	}
    }
  else if (glyphs == 1)
    {
      int area;

      for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	{
	  char *s = (char *) alloca (row->used[area] + 1);
	  int i;

	  for (i = 0; i < row->used[area]; ++i)
	    {
	      struct glyph *glyph = row->glyphs[area] + i;
	      if (glyph->type == CHAR_GLYPH
		  && glyph->u.ch < 0x80
		  && glyph->u.ch >= ' ')
		s[i] = glyph->u.ch;
	      else
		s[i] = '.';
	    }
      
	  s[i] = '\0';
	  fprintf (stderr, "%3d: (%d) '%s'\n", vpos, row->enabled_p, s);
	}
    }
}


DEFUN ("dump-glyph-matrix", Fdump_glyph_matrix,
       Sdump_glyph_matrix, 0, 1, "p",
       doc: /* Dump the current matrix of the selected window to stderr.
Shows contents of glyph row structures.  With non-nil
parameter GLYPHS, dump glyphs as well.  If GLYPHS is 1 show
glyphs in short form, otherwise show glyphs in long form.  */)
     (glyphs)
     Lisp_Object glyphs;
{
  struct window *w = XWINDOW (selected_window);
  struct buffer *buffer = XBUFFER (w->buffer);

  fprintf (stderr, "PT = %d, BEGV = %d. ZV = %d\n",
	   BUF_PT (buffer), BUF_BEGV (buffer), BUF_ZV (buffer));
  fprintf (stderr, "Cursor x = %d, y = %d, hpos = %d, vpos = %d\n",
	   w->cursor.x, w->cursor.y, w->cursor.hpos, w->cursor.vpos);
  fprintf (stderr, "=============================================\n");
  dump_glyph_matrix (w->current_matrix,
		     NILP (glyphs) ? 0 : XINT (glyphs));
  return Qnil;
}


DEFUN ("dump-glyph-row", Fdump_glyph_row, Sdump_glyph_row, 1, 2, "",
       doc: /* Dump glyph row ROW to stderr.
GLYPH 0 means don't dump glyphs.
GLYPH 1 means dump glyphs in short form.
GLYPH > 1 or omitted means dump glyphs in long form.  */)
     (row, glyphs)
     Lisp_Object row, glyphs;
{
  struct glyph_matrix *matrix;
  int vpos;
  
  CHECK_NUMBER (row);
  matrix = XWINDOW (selected_window)->current_matrix;
  vpos = XINT (row);
  if (vpos >= 0 && vpos < matrix->nrows)
    dump_glyph_row (MATRIX_ROW (matrix, vpos),
		    vpos,
		    INTEGERP (glyphs) ? XINT (glyphs) : 2);
  return Qnil;
}


DEFUN ("dump-tool-bar-row", Fdump_tool_bar_row, Sdump_tool_bar_row, 1, 2, "",
       doc: /* Dump glyph row ROW of the tool-bar of the current frame to stderr.
GLYPH 0 means don't dump glyphs.
GLYPH 1 means dump glyphs in short form.
GLYPH > 1 or omitted means dump glyphs in long form.  */)
     (row, glyphs)
     Lisp_Object row, glyphs;
{
  struct frame *sf = SELECTED_FRAME ();
  struct glyph_matrix *m = XWINDOW (sf->tool_bar_window)->current_matrix;
  int vpos;
  
  CHECK_NUMBER (row);
  vpos = XINT (row);
  if (vpos >= 0 && vpos < m->nrows)
    dump_glyph_row (MATRIX_ROW (m, vpos), vpos,
		    INTEGERP (glyphs) ? XINT (glyphs) : 2);
  return Qnil;
}


DEFUN ("trace-redisplay", Ftrace_redisplay, Strace_redisplay, 0, 1, "P",
       doc: /* Toggle tracing of redisplay.
With ARG, turn tracing on if and only if ARG is positive.  */)
     (arg)
     Lisp_Object arg;
{
  if (NILP (arg))
    trace_redisplay_p = !trace_redisplay_p;
  else
    {
      arg = Fprefix_numeric_value (arg);
      trace_redisplay_p = XINT (arg) > 0;
    }
  
  return Qnil;
}


DEFUN ("trace-to-stderr", Ftrace_to_stderr, Strace_to_stderr, 1, MANY, "",
       doc: /* Like `format', but print result to stderr.  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object s = Fformat (nargs, args);
  fprintf (stderr, "%s", SDATA (s));
  return Qnil;
}
	
#endif /* GLYPH_DEBUG */



/***********************************************************************
		     Building Desired Matrix Rows
 ***********************************************************************/

/* Return a temporary glyph row holding the glyphs of an overlay
   arrow.  Only used for non-window-redisplay windows.  */

static struct glyph_row *
get_overlay_arrow_glyph_row (w)
     struct window *w;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct buffer *buffer = XBUFFER (w->buffer);
  struct buffer *old = current_buffer;
  unsigned char *arrow_string = SDATA (Voverlay_arrow_string);
  int arrow_len = SCHARS (Voverlay_arrow_string);
  unsigned char *arrow_end = arrow_string + arrow_len;
  unsigned char *p;
  struct it it;
  int multibyte_p;
  int n_glyphs_before;

  set_buffer_temp (buffer);
  init_iterator (&it, w, -1, -1, &scratch_glyph_row, DEFAULT_FACE_ID);
  it.glyph_row->used[TEXT_AREA] = 0;
  SET_TEXT_POS (it.position, 0, 0);

  multibyte_p = !NILP (buffer->enable_multibyte_characters);
  p = arrow_string;
  while (p < arrow_end)
    {
      Lisp_Object face, ilisp;
      
      /* Get the next character.  */
      if (multibyte_p)
	it.c = string_char_and_length (p, arrow_len, &it.len);
      else
	it.c = *p, it.len = 1;
      p += it.len;
      
      /* Get its face.  */
      ilisp = make_number (p - arrow_string);
      face = Fget_text_property (ilisp, Qface, Voverlay_arrow_string);
      it.face_id = compute_char_face (f, it.c, face);

      /* Compute its width, get its glyphs.  */
      n_glyphs_before = it.glyph_row->used[TEXT_AREA];
      SET_TEXT_POS (it.position, -1, -1);
      PRODUCE_GLYPHS (&it);

      /* If this character doesn't fit any more in the line, we have
	 to remove some glyphs.  */
      if (it.current_x > it.last_visible_x)
	{
	  it.glyph_row->used[TEXT_AREA] = n_glyphs_before;
	  break;
	}
    }
  
  set_buffer_temp (old);
  return it.glyph_row;
}


/* Insert truncation glyphs at the start of IT->glyph_row.  Truncation
   glyphs are only inserted for terminal frames since we can't really
   win with truncation glyphs when partially visible glyphs are
   involved.  Which glyphs to insert is determined by
   produce_special_glyphs.  */

static void
insert_left_trunc_glyphs (it)
     struct it *it;
{
  struct it truncate_it;
  struct glyph *from, *end, *to, *toend;

  xassert (!FRAME_WINDOW_P (it->f));

  /* Get the truncation glyphs.  */
  truncate_it = *it;
  truncate_it.current_x = 0;
  truncate_it.face_id = DEFAULT_FACE_ID;
  truncate_it.glyph_row = &scratch_glyph_row;
  truncate_it.glyph_row->used[TEXT_AREA] = 0;
  CHARPOS (truncate_it.position) = BYTEPOS (truncate_it.position) = -1;
  truncate_it.object = make_number (0);
  produce_special_glyphs (&truncate_it, IT_TRUNCATION);
  
  /* Overwrite glyphs from IT with truncation glyphs.  */
  from = truncate_it.glyph_row->glyphs[TEXT_AREA];
  end = from + truncate_it.glyph_row->used[TEXT_AREA];
  to = it->glyph_row->glyphs[TEXT_AREA];
  toend = to + it->glyph_row->used[TEXT_AREA];

  while (from < end)
    *to++ = *from++;

  /* There may be padding glyphs left over.  Overwrite them too.  */
  while (to < toend && CHAR_GLYPH_PADDING_P (*to))
    {
      from = truncate_it.glyph_row->glyphs[TEXT_AREA];
      while (from < end)
	*to++ = *from++;
    }

  if (to > toend)
    it->glyph_row->used[TEXT_AREA] = to - it->glyph_row->glyphs[TEXT_AREA];
}


/* Compute the pixel height and width of IT->glyph_row.

   Most of the time, ascent and height of a display line will be equal
   to the max_ascent and max_height values of the display iterator
   structure.  This is not the case if

   1. We hit ZV without displaying anything.  In this case, max_ascent
   and max_height will be zero.

   2. We have some glyphs that don't contribute to the line height.
   (The glyph row flag contributes_to_line_height_p is for future
   pixmap extensions).

   The first case is easily covered by using default values because in
   these cases, the line height does not really matter, except that it
   must not be zero.  */

static void
compute_line_metrics (it)
     struct it *it;
{
  struct glyph_row *row = it->glyph_row;
  int area, i;

  if (FRAME_WINDOW_P (it->f))
    {
      int i, min_y, max_y;

      /* The line may consist of one space only, that was added to
	 place the cursor on it.  If so, the row's height hasn't been
	 computed yet.  */
      if (row->height == 0)
	{
	  if (it->max_ascent + it->max_descent == 0)
	    it->max_descent = it->max_phys_descent = CANON_Y_UNIT (it->f);
	  row->ascent = it->max_ascent;
	  row->height = it->max_ascent + it->max_descent;
	  row->phys_ascent = it->max_phys_ascent;
	  row->phys_height = it->max_phys_ascent + it->max_phys_descent;
	}
      
      /* Compute the width of this line.  */
      row->pixel_width = row->x;
      for (i = 0; i < row->used[TEXT_AREA]; ++i)
	row->pixel_width += row->glyphs[TEXT_AREA][i].pixel_width;

      xassert (row->pixel_width >= 0);
      xassert (row->ascent >= 0 && row->height > 0);

      row->overlapping_p = (MATRIX_ROW_OVERLAPS_SUCC_P (row)
			    || MATRIX_ROW_OVERLAPS_PRED_P (row));

      /* If first line's physical ascent is larger than its logical
         ascent, use the physical ascent, and make the row taller.
         This makes accented characters fully visible.  */
      if (row == MATRIX_FIRST_TEXT_ROW (it->w->desired_matrix)
	  && row->phys_ascent > row->ascent)
	{
	  row->height += row->phys_ascent - row->ascent;
	  row->ascent = row->phys_ascent;
	}

      /* Compute how much of the line is visible.  */
      row->visible_height = row->height;
      
      min_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (it->w);
      max_y = WINDOW_DISPLAY_HEIGHT_NO_MODE_LINE (it->w);

      if (row->y < min_y)
	row->visible_height -= min_y - row->y;
      if (row->y + row->height > max_y)
	row->visible_height -= row->y + row->height - max_y;
    }
  else
    {
      row->pixel_width = row->used[TEXT_AREA];
      if (row->continued_p)
	row->pixel_width -= it->continuation_pixel_width;
      else if (row->truncated_on_right_p)
	row->pixel_width -= it->truncation_pixel_width;
      row->ascent = row->phys_ascent = 0;
      row->height = row->phys_height = row->visible_height = 1;
    }

  /* Compute a hash code for this row.  */
  row->hash = 0;
  for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
    for (i = 0; i < row->used[area]; ++i)
      row->hash = ((((row->hash << 4) + (row->hash >> 24)) & 0x0fffffff)
		   + row->glyphs[area][i].u.val
		   + row->glyphs[area][i].face_id
		   + row->glyphs[area][i].padding_p
		   + (row->glyphs[area][i].type << 2));

  it->max_ascent = it->max_descent = 0;
  it->max_phys_ascent = it->max_phys_descent = 0;
}


/* Append one space to the glyph row of iterator IT if doing a
   window-based redisplay.  DEFAULT_FACE_P non-zero means let the
   space have the default face, otherwise let it have the same face as
   IT->face_id.  Value is non-zero if a space was added.

   This function is called to make sure that there is always one glyph
   at the end of a glyph row that the cursor can be set on under
   window-systems.  (If there weren't such a glyph we would not know
   how wide and tall a box cursor should be displayed).

   At the same time this space let's a nicely handle clearing to the
   end of the line if the row ends in italic text.  */

static int
append_space (it, default_face_p)
     struct it *it;
     int default_face_p;
{
  if (FRAME_WINDOW_P (it->f))
    {
      int n = it->glyph_row->used[TEXT_AREA];

      if (it->glyph_row->glyphs[TEXT_AREA] + n
	  < it->glyph_row->glyphs[1 + TEXT_AREA])
	{
	  /* Save some values that must not be changed.
	     Must save IT->c and IT->len because otherwise
	     ITERATOR_AT_END_P wouldn't work anymore after
	     append_space has been called.  */
	  enum display_element_type saved_what = it->what;
	  int saved_c = it->c, saved_len = it->len;
	  int saved_x = it->current_x;
	  int saved_face_id = it->face_id;
	  struct text_pos saved_pos;
	  Lisp_Object saved_object;
	  struct face *face;

	  saved_object = it->object;
	  saved_pos = it->position;
	  
	  it->what = IT_CHARACTER;
	  bzero (&it->position, sizeof it->position);
	  it->object = make_number (0);
	  it->c = ' ';
	  it->len = 1;

	  if (default_face_p)
	    it->face_id = DEFAULT_FACE_ID;
	  else if (it->face_before_selective_p)
	    it->face_id = it->saved_face_id;
	  face = FACE_FROM_ID (it->f, it->face_id);
	  it->face_id = FACE_FOR_CHAR (it->f, face, 0);

	  PRODUCE_GLYPHS (it);
	  
	  it->current_x = saved_x;
	  it->object = saved_object;
	  it->position = saved_pos;
	  it->what = saved_what;
	  it->face_id = saved_face_id;
	  it->len = saved_len;
	  it->c = saved_c;
	  return 1;
	}
    }

  return 0;
}


/* Extend the face of the last glyph in the text area of IT->glyph_row
   to the end of the display line.  Called from display_line.
   If the glyph row is empty, add a space glyph to it so that we
   know the face to draw.  Set the glyph row flag fill_line_p.  */
   
static void
extend_face_to_end_of_line (it)
     struct it *it;
{
  struct face *face;
  struct frame *f = it->f;

  /* If line is already filled, do nothing.  */
  if (it->current_x >= it->last_visible_x)
    return;
  
  /* Face extension extends the background and box of IT->face_id
     to the end of the line.  If the background equals the background
     of the frame, we don't have to do anything.  */
  if (it->face_before_selective_p)
    face = FACE_FROM_ID (it->f, it->saved_face_id);
  else
    face = FACE_FROM_ID (f, it->face_id);
  
  if (FRAME_WINDOW_P (f)
      && face->box == FACE_NO_BOX
      && face->background == FRAME_BACKGROUND_PIXEL (f)
      && !face->stipple)
    return;

  /* Set the glyph row flag indicating that the face of the last glyph
     in the text area has to be drawn to the end of the text area.  */
  it->glyph_row->fill_line_p = 1;

  /* If current character of IT is not ASCII, make sure we have the
         ASCII face.  This will be automatically undone the next time
         get_next_display_element returns a multibyte character.  Note
         that the character will always be single byte in unibyte text.  */
  if (!SINGLE_BYTE_CHAR_P (it->c))
    {
      it->face_id = FACE_FOR_CHAR (f, face, 0);
    }

  if (FRAME_WINDOW_P (f))
    {
      /* If the row is empty, add a space with the current face of IT,
	 so that we know which face to draw.  */
      if (it->glyph_row->used[TEXT_AREA] == 0)
	{
	  it->glyph_row->glyphs[TEXT_AREA][0] = space_glyph;
	  it->glyph_row->glyphs[TEXT_AREA][0].face_id = it->face_id;
	  it->glyph_row->used[TEXT_AREA] = 1;
	}
    }
  else
    {
      /* Save some values that must not be changed.  */
      int saved_x = it->current_x;
      struct text_pos saved_pos;
      Lisp_Object saved_object;
      enum display_element_type saved_what = it->what;
      int saved_face_id = it->face_id;

      saved_object = it->object;
      saved_pos = it->position;
  
      it->what = IT_CHARACTER;
      bzero (&it->position, sizeof it->position);
      it->object = make_number (0);
      it->c = ' ';
      it->len = 1;
      it->face_id = face->id;
      
      PRODUCE_GLYPHS (it);
      
      while (it->current_x <= it->last_visible_x)
	PRODUCE_GLYPHS (it);
      
      /* Don't count these blanks really.  It would let us insert a left
	 truncation glyph below and make us set the cursor on them, maybe.  */
      it->current_x = saved_x;
      it->object = saved_object;
      it->position = saved_pos;
      it->what = saved_what;
      it->face_id = saved_face_id;
    }
}


/* Value is non-zero if text starting at CHARPOS in current_buffer is
   trailing whitespace.  */

static int
trailing_whitespace_p (charpos)
     int charpos;
{
  int bytepos = CHAR_TO_BYTE (charpos);
  int c = 0;

  while (bytepos < ZV_BYTE
	 && (c = FETCH_CHAR (bytepos),
	     c == ' ' || c == '\t'))
    ++bytepos;

  if (bytepos >= ZV_BYTE || c == '\n' || c == '\r')
    {
      if (bytepos != PT_BYTE)
	return 1;
    }
  return 0;
}


/* Highlight trailing whitespace, if any, in ROW.  */

void
highlight_trailing_whitespace (f, row)
     struct frame *f;
     struct glyph_row *row;
{
  int used = row->used[TEXT_AREA];
  
  if (used)
    {
      struct glyph *start = row->glyphs[TEXT_AREA];
      struct glyph *glyph = start + used - 1;

      /* Skip over glyphs inserted to display the cursor at the
	 end of a line, for extending the face of the last glyph
	 to the end of the line on terminals, and for truncation
	 and continuation glyphs.  */
      while (glyph >= start
	     && glyph->type == CHAR_GLYPH
	     && INTEGERP (glyph->object))
	--glyph;

      /* If last glyph is a space or stretch, and it's trailing
	 whitespace, set the face of all trailing whitespace glyphs in
	 IT->glyph_row to `trailing-whitespace'.  */
      if (glyph >= start
	  && BUFFERP (glyph->object)
	  && (glyph->type == STRETCH_GLYPH
	      || (glyph->type == CHAR_GLYPH
		  && glyph->u.ch == ' '))
	  && trailing_whitespace_p (glyph->charpos))
	{
	  int face_id = lookup_named_face (f, Qtrailing_whitespace, 0);
	  
	  while (glyph >= start
		 && BUFFERP (glyph->object)
		 && (glyph->type == STRETCH_GLYPH
		     || (glyph->type == CHAR_GLYPH
			 && glyph->u.ch == ' ')))
	    (glyph--)->face_id = face_id;
	}
    }
}


/* Value is non-zero if glyph row ROW in window W should be
   used to hold the cursor.  */

static int
cursor_row_p (w, row)
     struct window *w;
     struct glyph_row *row;
{
  int cursor_row_p = 1;
  
  if (PT == MATRIX_ROW_END_CHARPOS (row))
    {
      /* If the row ends with a newline from a string, we don't want
	 the cursor there (if the row is continued it doesn't end in a
	 newline).  */
      if (CHARPOS (row->end.string_pos) >= 0
	  || MATRIX_ROW_ENDS_IN_MIDDLE_OF_CHAR_P (row))
	cursor_row_p = row->continued_p;

      /* If the row ends at ZV, display the cursor at the end of that
	 row instead of at the start of the row below.  */
      else if (row->ends_at_zv_p)
	cursor_row_p = 1;
      else
	cursor_row_p = 0;
    }

  return cursor_row_p;
}


/* Construct the glyph row IT->glyph_row in the desired matrix of
   IT->w from text at the current position of IT.  See dispextern.h
   for an overview of struct it.  Value is non-zero if
   IT->glyph_row displays text, as opposed to a line displaying ZV
   only.  */
   
static int
display_line (it)
     struct it *it;
{
  struct glyph_row *row = it->glyph_row;

  /* We always start displaying at hpos zero even if hscrolled.  */
  xassert (it->hpos == 0 && it->current_x == 0);

  /* We must not display in a row that's not a text row.  */
  xassert (MATRIX_ROW_VPOS (row, it->w->desired_matrix)
	   < it->w->desired_matrix->nrows);

  /* Is IT->w showing the region?  */
  it->w->region_showing = it->region_beg_charpos > 0 ? Qt : Qnil;

  /* Clear the result glyph row and enable it.  */
  prepare_desired_row (row);

  row->y = it->current_y;
  row->start = it->current;
  row->continuation_lines_width = it->continuation_lines_width;
  row->displays_text_p = 1;
  row->starts_in_middle_of_char_p = it->starts_in_middle_of_char_p;
  it->starts_in_middle_of_char_p = 0;

  /* Arrange the overlays nicely for our purposes.  Usually, we call
     display_line on only one line at a time, in which case this
     can't really hurt too much, or we call it on lines which appear
     one after another in the buffer, in which case all calls to
     recenter_overlay_lists but the first will be pretty cheap.  */
  recenter_overlay_lists (current_buffer, IT_CHARPOS (*it));

  /* Move over display elements that are not visible because we are
     hscrolled.  This may stop at an x-position < IT->first_visible_x
     if the first glyph is partially visible or if we hit a line end.  */
  if (it->current_x < it->first_visible_x)
    move_it_in_display_line_to (it, ZV, it->first_visible_x,
				MOVE_TO_POS | MOVE_TO_X);

  /* Get the initial row height.  This is either the height of the
     text hscrolled, if there is any, or zero.  */
  row->ascent = it->max_ascent;
  row->height = it->max_ascent + it->max_descent;
  row->phys_ascent = it->max_phys_ascent;
  row->phys_height = it->max_phys_ascent + it->max_phys_descent;

  /* Loop generating characters.  The loop is left with IT on the next
     character to display.  */
  while (1)
    {
      int n_glyphs_before, hpos_before, x_before;
      int x, i, nglyphs;
      int ascent = 0, descent = 0, phys_ascent = 0, phys_descent = 0;

      /* Retrieve the next thing to display.  Value is zero if end of
	 buffer reached.  */
      if (!get_next_display_element (it))
	{
	  /* Maybe add a space at the end of this line that is used to
	     display the cursor there under X.  Set the charpos of the
	     first glyph of blank lines not corresponding to any text
	     to -1.  */
	  if ((append_space (it, 1) && row->used[TEXT_AREA] == 1)
	      || row->used[TEXT_AREA] == 0)
	    {
	      row->glyphs[TEXT_AREA]->charpos = -1;
	      row->displays_text_p = 0;

	      if (!NILP (XBUFFER (it->w->buffer)->indicate_empty_lines)
		  && (!MINI_WINDOW_P (it->w)
		      || (minibuf_level && EQ (it->window, minibuf_window))))
		row->indicate_empty_line_p = 1;
	    }
	  
	  it->continuation_lines_width = 0;
	  row->ends_at_zv_p = 1;
	  break;
	}

      /* Now, get the metrics of what we want to display.  This also
	 generates glyphs in `row' (which is IT->glyph_row).  */
      n_glyphs_before = row->used[TEXT_AREA];
      x = it->current_x;

      /* Remember the line height so far in case the next element doesn't
	 fit on the line.  */
      if (!it->truncate_lines_p)
	{
	  ascent = it->max_ascent;
	  descent = it->max_descent;
	  phys_ascent = it->max_phys_ascent;
	  phys_descent = it->max_phys_descent;
	}
      
      PRODUCE_GLYPHS (it);

      /* If this display element was in marginal areas, continue with
	 the next one.  */
      if (it->area != TEXT_AREA)
	{
	  row->ascent = max (row->ascent, it->max_ascent);
	  row->height = max (row->height, it->max_ascent + it->max_descent);
	  row->phys_ascent = max (row->phys_ascent, it->max_phys_ascent);
	  row->phys_height = max (row->phys_height,
				  it->max_phys_ascent + it->max_phys_descent);
	  set_iterator_to_next (it, 1);
	  continue;
	}

      /* Does the display element fit on the line?  If we truncate
	 lines, we should draw past the right edge of the window.  If
	 we don't truncate, we want to stop so that we can display the
	 continuation glyph before the right margin.  If lines are
	 continued, there are two possible strategies for characters
	 resulting in more than 1 glyph (e.g. tabs): Display as many
	 glyphs as possible in this line and leave the rest for the
	 continuation line, or display the whole element in the next
	 line.  Original redisplay did the former, so we do it also.  */
      nglyphs = row->used[TEXT_AREA] - n_glyphs_before;
      hpos_before = it->hpos;
      x_before = x;

      if (/* Not a newline.  */
	  nglyphs > 0
	  /* Glyphs produced fit entirely in the line.  */
	  && it->current_x < it->last_visible_x)
	{
	  it->hpos += nglyphs;
	  row->ascent = max (row->ascent, it->max_ascent);
	  row->height = max (row->height, it->max_ascent + it->max_descent);
	  row->phys_ascent = max (row->phys_ascent, it->max_phys_ascent);
	  row->phys_height = max (row->phys_height,
				  it->max_phys_ascent + it->max_phys_descent);
	  if (it->current_x - it->pixel_width < it->first_visible_x)
	    row->x = x - it->first_visible_x;
	}
      else
	{
	  int new_x;
	  struct glyph *glyph;
	  
	  for (i = 0; i < nglyphs; ++i, x = new_x)
	    {
	      glyph = row->glyphs[TEXT_AREA] + n_glyphs_before + i;
	      new_x = x + glyph->pixel_width;

	      if (/* Lines are continued.  */
		  !it->truncate_lines_p
		  && (/* Glyph doesn't fit on the line.  */
		      new_x > it->last_visible_x
		      /* Or it fits exactly on a window system frame.  */
		      || (new_x == it->last_visible_x
			  && FRAME_WINDOW_P (it->f))))
		{
		  /* End of a continued line.  */
		  
		  if (it->hpos == 0
		      || (new_x == it->last_visible_x
			  && FRAME_WINDOW_P (it->f)))
		    {
		      /* Current glyph is the only one on the line or
			 fits exactly on the line.  We must continue
			 the line because we can't draw the cursor
			 after the glyph.  */
		      row->continued_p = 1;
		      it->current_x = new_x;
		      it->continuation_lines_width += new_x;
		      ++it->hpos;
		      if (i == nglyphs - 1)
			set_iterator_to_next (it, 1);
		    }
		  else if (CHAR_GLYPH_PADDING_P (*glyph)
			   && !FRAME_WINDOW_P (it->f))
		    {
		      /* A padding glyph that doesn't fit on this line.
			 This means the whole character doesn't fit
			 on the line.  */
		      row->used[TEXT_AREA] = n_glyphs_before;
		  
		      /* Fill the rest of the row with continuation
			 glyphs like in 20.x.  */
		      while (row->glyphs[TEXT_AREA] + row->used[TEXT_AREA]
			     < row->glyphs[1 + TEXT_AREA])
			produce_special_glyphs (it, IT_CONTINUATION);
		      
		      row->continued_p = 1;
		      it->current_x = x_before;
		      it->continuation_lines_width += x_before;
		      
		      /* Restore the height to what it was before the
			 element not fitting on the line.  */
		      it->max_ascent = ascent;
		      it->max_descent = descent;
		      it->max_phys_ascent = phys_ascent;
		      it->max_phys_descent = phys_descent;
		    }
		  else if (it->c == '\t' && FRAME_WINDOW_P (it->f))
		    {
		      /* A TAB that extends past the right edge of the
			 window.  This produces a single glyph on
			 window system frames.  We leave the glyph in
			 this row and let it fill the row, but don't
			 consume the TAB.  */
		      it->continuation_lines_width += it->last_visible_x;
		      row->ends_in_middle_of_char_p = 1;
		      row->continued_p = 1;
		      glyph->pixel_width = it->last_visible_x - x;
		      it->starts_in_middle_of_char_p = 1;
		    }
		  else
		    {
		      /* Something other than a TAB that draws past
			 the right edge of the window.  Restore
			 positions to values before the element.  */
		      row->used[TEXT_AREA] = n_glyphs_before + i;
		  
		      /* Display continuation glyphs.  */
		      if (!FRAME_WINDOW_P (it->f))
			produce_special_glyphs (it, IT_CONTINUATION);
		      row->continued_p = 1;

		      it->continuation_lines_width += x;
		      
		      if (nglyphs > 1 && i > 0)
			{
			  row->ends_in_middle_of_char_p = 1;
			  it->starts_in_middle_of_char_p = 1;
			}
		      
		      /* Restore the height to what it was before the
			 element not fitting on the line.  */
		      it->max_ascent = ascent;
		      it->max_descent = descent;
		      it->max_phys_ascent = phys_ascent;
		      it->max_phys_descent = phys_descent;
		    }

		  break;
		}
	      else if (new_x > it->first_visible_x)
		{
		  /* Increment number of glyphs actually displayed.  */
		  ++it->hpos;
		  
		  if (x < it->first_visible_x)
		    /* Glyph is partially visible, i.e. row starts at
		       negative X position.  */
		    row->x = x - it->first_visible_x;
		}
	      else
		{
		  /* Glyph is completely off the left margin of the
		     window.  This should not happen because of the
		     move_it_in_display_line at the start of
		     this function.  */
		  abort ();
		}
	    }
	  
	  row->ascent = max (row->ascent, it->max_ascent);
	  row->height = max (row->height, it->max_ascent + it->max_descent);
	  row->phys_ascent = max (row->phys_ascent, it->max_phys_ascent);
	  row->phys_height = max (row->phys_height,
				  it->max_phys_ascent + it->max_phys_descent);
	  
	  /* End of this display line if row is continued.  */
	  if (row->continued_p)
	    break;
	}

      /* Is this a line end?  If yes, we're also done, after making
	 sure that a non-default face is extended up to the right
	 margin of the window.  */
      if (ITERATOR_AT_END_OF_LINE_P (it))
	{
	  int used_before = row->used[TEXT_AREA];

	  row->ends_in_newline_from_string_p = STRINGP (it->object);

	  /* Add a space at the end of the line that is used to
	     display the cursor there.  */
	  append_space (it, 0);
	  
	  /* Extend the face to the end of the line.  */
	  extend_face_to_end_of_line (it);

	  /* Make sure we have the position.  */
	  if (used_before == 0)
	    row->glyphs[TEXT_AREA]->charpos = CHARPOS (it->position);
	  
	  /* Consume the line end.  This skips over invisible lines.  */
	  set_iterator_to_next (it, 1);
	  it->continuation_lines_width = 0;
	  break;
	}

      /* Proceed with next display element.  Note that this skips 
	 over lines invisible because of selective display.  */
      set_iterator_to_next (it, 1);

      /* If we truncate lines, we are done when the last displayed
	 glyphs reach past the right margin of the window.  */
      if (it->truncate_lines_p
	  && (FRAME_WINDOW_P (it->f)
	      ? (it->current_x >= it->last_visible_x)
	      : (it->current_x > it->last_visible_x)))
	{
	  /* Maybe add truncation glyphs.  */
	  if (!FRAME_WINDOW_P (it->f))
	    {
	      int i, n;
	      
	      for (i = row->used[TEXT_AREA] - 1; i > 0; --i)
		if (!CHAR_GLYPH_PADDING_P (row->glyphs[TEXT_AREA][i]))
		  break;

	      for (n = row->used[TEXT_AREA]; i < n; ++i)
		{
		  row->used[TEXT_AREA] = i;
		  produce_special_glyphs (it, IT_TRUNCATION);
		}
	    }
	  
	  row->truncated_on_right_p = 1;
	  it->continuation_lines_width = 0;
	  reseat_at_next_visible_line_start (it, 0);
	  row->ends_at_zv_p = FETCH_BYTE (IT_BYTEPOS (*it) - 1) != '\n';
	  it->hpos = hpos_before;
	  it->current_x = x_before;
	  break;
	}
    }

  /* If line is not empty and hscrolled, maybe insert truncation glyphs
     at the left window margin.  */
  if (it->first_visible_x
      && IT_CHARPOS (*it) != MATRIX_ROW_START_CHARPOS (row))
    {
      if (!FRAME_WINDOW_P (it->f))
	insert_left_trunc_glyphs (it);
      row->truncated_on_left_p = 1;
    }

  /* If the start of this line is the overlay arrow-position, then
     mark this glyph row as the one containing the overlay arrow.
     This is clearly a mess with variable size fonts.  It would be
     better to let it be displayed like cursors under X.  */
  if (MARKERP (Voverlay_arrow_position)
      && current_buffer == XMARKER (Voverlay_arrow_position)->buffer
      && (MATRIX_ROW_START_CHARPOS (row)
	  == marker_position (Voverlay_arrow_position))
      && STRINGP (Voverlay_arrow_string)
      && ! overlay_arrow_seen)
    {
      /* Overlay arrow in window redisplay is a fringe bitmap.  */
      if (!FRAME_WINDOW_P (it->f))
	{
	  struct glyph_row *arrow_row = get_overlay_arrow_glyph_row (it->w);
	  struct glyph *glyph = arrow_row->glyphs[TEXT_AREA];
	  struct glyph *arrow_end = glyph + arrow_row->used[TEXT_AREA];
	  struct glyph *p = row->glyphs[TEXT_AREA];
	  struct glyph *p2, *end;

	  /* Copy the arrow glyphs.  */
	  while (glyph < arrow_end)
	    *p++ = *glyph++;

	  /* Throw away padding glyphs.  */
	  p2 = p;
	  end = row->glyphs[TEXT_AREA] + row->used[TEXT_AREA];
	  while (p2 < end && CHAR_GLYPH_PADDING_P (*p2))
	    ++p2;
	  if (p2 > p)
	    {
	      while (p2 < end)
		*p++ = *p2++;
	      row->used[TEXT_AREA] = p2 - row->glyphs[TEXT_AREA];
	    }
	}
      
      overlay_arrow_seen = 1;
      row->overlay_arrow_p = 1;
    }

  /* Compute pixel dimensions of this line.  */
  compute_line_metrics (it);

  /* Remember the position at which this line ends.  */
  row->end = it->current;

  /* Maybe set the cursor.  */
  if (it->w->cursor.vpos < 0
      && PT >= MATRIX_ROW_START_CHARPOS (row)
      && PT <= MATRIX_ROW_END_CHARPOS (row)
      && cursor_row_p (it->w, row))
    set_cursor_from_row (it->w, row, it->w->desired_matrix, 0, 0, 0, 0);

  /* Highlight trailing whitespace.  */
  if (!NILP (Vshow_trailing_whitespace))
    highlight_trailing_whitespace (it->f, it->glyph_row);

  /* Prepare for the next line.  This line starts horizontally at (X
     HPOS) = (0 0).  Vertical positions are incremented.  As a
     convenience for the caller, IT->glyph_row is set to the next
     row to be used.  */
  it->current_x = it->hpos = 0;
  it->current_y += row->height;
  ++it->vpos;
  ++it->glyph_row;
  return row->displays_text_p;
}



/***********************************************************************
			       Menu Bar
 ***********************************************************************/

/* Redisplay the menu bar in the frame for window W.

   The menu bar of X frames that don't have X toolkit support is
   displayed in a special window W->frame->menu_bar_window.
   
   The menu bar of terminal frames is treated specially as far as
   glyph matrices are concerned.  Menu bar lines are not part of
   windows, so the update is done directly on the frame matrix rows
   for the menu bar.  */

static void
display_menu_bar (w)
     struct window *w;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct it it;
  Lisp_Object items;
  int i;

  /* Don't do all this for graphical frames.  */
#ifdef HAVE_NTGUI
  if (!NILP (Vwindow_system))
    return;
#endif
#ifdef USE_X_TOOLKIT
  if (FRAME_X_P (f))
    return;
#endif
#ifdef macintosh
  if (FRAME_MAC_P (f))
    return;
#endif

#ifdef USE_X_TOOLKIT
  xassert (!FRAME_WINDOW_P (f));
  init_iterator (&it, w, -1, -1, f->desired_matrix->rows, MENU_FACE_ID);
  it.first_visible_x = 0;
  it.last_visible_x = FRAME_WINDOW_WIDTH (f) * CANON_X_UNIT (f);
#else /* not USE_X_TOOLKIT */
  if (FRAME_WINDOW_P (f))
    {
      /* Menu bar lines are displayed in the desired matrix of the
	 dummy window menu_bar_window.  */
      struct window *menu_w;
      xassert (WINDOWP (f->menu_bar_window));
      menu_w = XWINDOW (f->menu_bar_window);
      init_iterator (&it, menu_w, -1, -1, menu_w->desired_matrix->rows,
		     MENU_FACE_ID);
      it.first_visible_x = 0;
      it.last_visible_x = FRAME_WINDOW_WIDTH (f) * CANON_X_UNIT (f);
    }
  else
    {
      /* This is a TTY frame, i.e. character hpos/vpos are used as
	 pixel x/y.  */
      init_iterator (&it, w, -1, -1, f->desired_matrix->rows,
		     MENU_FACE_ID);
      it.first_visible_x = 0;
      it.last_visible_x = FRAME_WIDTH (f);
    }
#endif /* not USE_X_TOOLKIT */

  if (! mode_line_inverse_video)
    /* Force the menu-bar to be displayed in the default face.  */
    it.base_face_id = it.face_id = DEFAULT_FACE_ID;

  /* Clear all rows of the menu bar.  */
  for (i = 0; i < FRAME_MENU_BAR_LINES (f); ++i)
    {
      struct glyph_row *row = it.glyph_row + i;
      clear_glyph_row (row);
      row->enabled_p = 1;
      row->full_width_p = 1;
    }

  /* Display all items of the menu bar.  */
  items = FRAME_MENU_BAR_ITEMS (it.f);
  for (i = 0; i < XVECTOR (items)->size; i += 4)
    {
      Lisp_Object string;

      /* Stop at nil string.  */
      string = AREF (items, i + 1);
      if (NILP (string))
	break;

      /* Remember where item was displayed.  */
      AREF (items, i + 3) = make_number (it.hpos);

      /* Display the item, pad with one space.  */
      if (it.current_x < it.last_visible_x)
	display_string (NULL, string, Qnil, 0, 0, &it,
			SCHARS (string) + 1, 0, 0, -1);
    }

  /* Fill out the line with spaces.  */
  if (it.current_x < it.last_visible_x)
    display_string ("", Qnil, Qnil, 0, 0, &it, -1, 0, 0, -1);

  /* Compute the total height of the lines.  */
  compute_line_metrics (&it);
}



/***********************************************************************
			      Mode Line
 ***********************************************************************/

/* Redisplay mode lines in the window tree whose root is WINDOW.  If
   FORCE is non-zero, redisplay mode lines unconditionally.
   Otherwise, redisplay only mode lines that are garbaged.  Value is
   the number of windows whose mode lines were redisplayed.  */

static int
redisplay_mode_lines (window, force)
     Lisp_Object window;
     int force;
{
  int nwindows = 0;
  
  while (!NILP (window))
    {
      struct window *w = XWINDOW (window);
      
      if (WINDOWP (w->hchild))
	nwindows += redisplay_mode_lines (w->hchild, force);
      else if (WINDOWP (w->vchild))
	nwindows += redisplay_mode_lines (w->vchild, force);
      else if (force
	       || FRAME_GARBAGED_P (XFRAME (w->frame))
	       || !MATRIX_MODE_LINE_ROW (w->current_matrix)->enabled_p)
	{
	  struct text_pos lpoint;
	  struct buffer *old = current_buffer;

	  /* Set the window's buffer for the mode line display.  */
	  SET_TEXT_POS (lpoint, PT, PT_BYTE);
	  set_buffer_internal_1 (XBUFFER (w->buffer));
	  
	  /* Point refers normally to the selected window.  For any
	     other window, set up appropriate value.  */
	  if (!EQ (window, selected_window))
	    {
	      struct text_pos pt;
	      
	      SET_TEXT_POS_FROM_MARKER (pt, w->pointm);
	      if (CHARPOS (pt) < BEGV)
		TEMP_SET_PT_BOTH (BEGV, BEGV_BYTE);
	      else if (CHARPOS (pt) > (ZV - 1))
		TEMP_SET_PT_BOTH (ZV, ZV_BYTE);
	      else
		TEMP_SET_PT_BOTH (CHARPOS (pt), BYTEPOS (pt));
	    }

	  /* Display mode lines.  */
	  clear_glyph_matrix (w->desired_matrix);
	  if (display_mode_lines (w))
	    {
	      ++nwindows;
	      w->must_be_updated_p = 1;
	    }

	  /* Restore old settings.  */
	  set_buffer_internal_1 (old);
	  TEMP_SET_PT_BOTH (CHARPOS (lpoint), BYTEPOS (lpoint));
	}

      window = w->next;
    }

  return nwindows;
}


/* Display the mode and/or top line of window W.  Value is the number
   of mode lines displayed.  */

static int
display_mode_lines (w)
     struct window *w;
{
  Lisp_Object old_selected_window, old_selected_frame;
  int n = 0;

  old_selected_frame = selected_frame;
  selected_frame = w->frame;
  old_selected_window = selected_window;
  XSETWINDOW (selected_window, w);
  
  /* These will be set while the mode line specs are processed.  */
  line_number_displayed = 0;
  w->column_number_displayed = Qnil;

  if (WINDOW_WANTS_MODELINE_P (w))
    {
      struct window *sel_w = XWINDOW (old_selected_window);

      /* Select mode line face based on the real selected window.  */
      display_mode_line (w, CURRENT_MODE_LINE_FACE_ID_3 (sel_w, sel_w, w),
			 current_buffer->mode_line_format);
      ++n;
    }
  
  if (WINDOW_WANTS_HEADER_LINE_P (w))
    {
      display_mode_line (w, HEADER_LINE_FACE_ID,
			 current_buffer->header_line_format);
      ++n;
    }

  selected_frame = old_selected_frame;
  selected_window = old_selected_window;
  return n;
}


/* Display mode or top line of window W.  FACE_ID specifies which line
   to display; it is either MODE_LINE_FACE_ID or HEADER_LINE_FACE_ID.
   FORMAT is the mode line format to display.  Value is the pixel
   height of the mode line displayed.  */

static int
display_mode_line (w, face_id, format)
     struct window *w;
     enum face_id face_id;
     Lisp_Object format;
{
  struct it it;
  struct face *face;

  init_iterator (&it, w, -1, -1, NULL, face_id);
  prepare_desired_row (it.glyph_row);

  if (! mode_line_inverse_video)
    /* Force the mode-line to be displayed in the default face.  */
    it.base_face_id = it.face_id = DEFAULT_FACE_ID;

  /* Temporarily make frame's keyboard the current kboard so that
     kboard-local variables in the mode_line_format will get the right
     values.  */
  push_frame_kboard (it.f);
  display_mode_element (&it, 0, 0, 0, format, Qnil);
  pop_frame_kboard ();

  /* Fill up with spaces.  */
  display_string (" ", Qnil, Qnil, 0, 0, &it, 10000, -1, -1, 0);
  
  compute_line_metrics (&it);
  it.glyph_row->full_width_p = 1;
  it.glyph_row->mode_line_p = 1;
  it.glyph_row->continued_p = 0;
  it.glyph_row->truncated_on_left_p = 0;
  it.glyph_row->truncated_on_right_p = 0;

  /* Make a 3D mode-line have a shadow at its right end.  */
  face = FACE_FROM_ID (it.f, face_id);
  extend_face_to_end_of_line (&it);
  if (face->box != FACE_NO_BOX)
    {
      struct glyph *last = (it.glyph_row->glyphs[TEXT_AREA]
			    + it.glyph_row->used[TEXT_AREA] - 1);
      last->right_box_line_p = 1;
    }

  return it.glyph_row->height;
}

/* Alist that caches the results of :propertize.
   Each element is (PROPERTIZED-STRING . PROPERTY-LIST).  */
Lisp_Object mode_line_proptrans_alist;

/* Contribute ELT to the mode line for window IT->w.  How it
   translates into text depends on its data type.

   IT describes the display environment in which we display, as usual.

   DEPTH is the depth in recursion.  It is used to prevent
   infinite recursion here.

   FIELD_WIDTH is the number of characters the display of ELT should
   occupy in the mode line, and PRECISION is the maximum number of
   characters to display from ELT's representation.  See
   display_string for details.

   Returns the hpos of the end of the text generated by ELT.  */

static int
display_mode_element (it, depth, field_width, precision, elt, props)
     struct it *it;
     int depth;
     int field_width, precision;
     Lisp_Object elt, props;
{
  int n = 0, field, prec;
  int literal = 0;

 tail_recurse:
  if (depth > 10)
    goto invalid;

  depth++;

  switch (SWITCH_ENUM_CAST (XTYPE (elt)))
    {
    case Lisp_String:
      {
	/* A string: output it and check for %-constructs within it.  */
	unsigned char c;
	unsigned char *this, *lisp_string;

	if (!NILP (props))
	  {
	    Lisp_Object oprops, aelt;
	    oprops = Ftext_properties_at (make_number (0), elt);
	    if (NILP (Fequal (props, oprops)))
	      {
		/* If the starting string has properties,
		   merge the specified ones onto the existing ones.  */
		if (! NILP (oprops))
		  {
		    Lisp_Object tem;

		    oprops = Fcopy_sequence (oprops);
		    tem = props;
		    while (CONSP (tem))
		      {
			oprops = Fplist_put (oprops, XCAR (tem),
					     XCAR (XCDR (tem)));
			tem = XCDR (XCDR (tem));
		      }
		    props = oprops;
		  }

		aelt = Fassoc (elt, mode_line_proptrans_alist);
		if (! NILP (aelt) && !NILP (Fequal (props, XCDR (aelt))))
		  elt = XCAR (aelt);
		else
		  {
		    elt = Fcopy_sequence (elt);
		    Fset_text_properties (make_number (0), Flength (elt),
					  props, elt);
		    mode_line_proptrans_alist
		      = Fcons (Fcons (elt, props),
			       mode_line_proptrans_alist);
		  }
	      }
	  }

	this = SDATA (elt);
	lisp_string = this;

	if (literal)
	  {
	    prec = precision - n;
	    if (frame_title_ptr)
	      n += store_frame_title (SDATA (elt), -1, prec);
	    else
	      n += display_string (NULL, elt, Qnil, 0, 0, it,
				   0, prec, 0, STRING_MULTIBYTE (elt));

	    break;
	  }

	while ((precision <= 0 || n < precision)
	       && *this
	       && (frame_title_ptr
		   || it->current_x < it->last_visible_x))
	  {
	    unsigned char *last = this;

	    /* Advance to end of string or next format specifier.  */
	    while ((c = *this++) != '\0' && c != '%')
	      ;
	    
	    if (this - 1 != last)
	      {
		/* Output to end of string or up to '%'.  Field width
		   is length of string.  Don't output more than
		   PRECISION allows us.  */
		--this;

		prec = chars_in_text (last, this - last);
		if (precision > 0 && prec > precision - n)
		  prec = precision - n;
		
		if (frame_title_ptr)
		  n += store_frame_title (last, 0, prec);
		else
		  {
		    int bytepos = last - lisp_string;
		    int charpos = string_byte_to_char (elt, bytepos);
		    n += display_string (NULL, elt, Qnil, 0, charpos,
					 it, 0, prec, 0,
					 STRING_MULTIBYTE (elt));
		  }
	      }
	    else /* c == '%' */
	      {
		unsigned char *percent_position = this;
		
		/* Get the specified minimum width.  Zero means
		   don't pad.  */
		field = 0;
		while ((c = *this++) >= '0' && c <= '9')
		  field = field * 10 + c - '0';

		/* Don't pad beyond the total padding allowed.  */
		if (field_width - n > 0 && field > field_width - n)
		  field = field_width - n;

		/* Note that either PRECISION <= 0 or N < PRECISION.  */
		prec = precision - n;
		
		if (c == 'M')
		  n += display_mode_element (it, depth, field, prec,
					     Vglobal_mode_string, props);
		else if (c != 0)
		  {
		    int multibyte;
		    int bytepos, charpos;
		    unsigned char *spec;
			
		    bytepos = percent_position - lisp_string;
		    charpos = (STRING_MULTIBYTE (elt)
			       ? string_byte_to_char (elt, bytepos)
			       : bytepos);

		    spec
		      = decode_mode_spec (it->w, c, field, prec, &multibyte);

		    if (frame_title_ptr)
		      n += store_frame_title (spec, field, prec);
		    else
		      {
			int nglyphs_before, nwritten;
			
			nglyphs_before = it->glyph_row->used[TEXT_AREA];
			nwritten = display_string (spec, Qnil, elt,
						   charpos, 0, it,
						   field, prec, 0,
						   multibyte);

			/* Assign to the glyphs written above the
			   string where the `%x' came from, position
			   of the `%'.  */
			if (nwritten > 0)
			  {
			    struct glyph *glyph
			      = (it->glyph_row->glyphs[TEXT_AREA]
				 + nglyphs_before);
			    int i;

			    for (i = 0; i < nwritten; ++i)
			      {
				glyph[i].object = elt;
				glyph[i].charpos = charpos;
			      }
			    
			    n += nwritten;
			  }
		      }
		  }
		else /* c == 0 */
		  break;
	      }
	  }
      }
      break;

    case Lisp_Symbol:
      /* A symbol: process the value of the symbol recursively
	 as if it appeared here directly.  Avoid error if symbol void.
	 Special case: if value of symbol is a string, output the string
	 literally.  */
      {
	register Lisp_Object tem;
	tem = Fboundp (elt);
	if (!NILP (tem))
	  {
	    tem = Fsymbol_value (elt);
	    /* If value is a string, output that string literally:
	       don't check for % within it.  */
	    if (STRINGP (tem))
	      literal = 1;

	    if (!EQ (tem, elt))
	      {
		/* Give up right away for nil or t.  */
		elt = tem;
		goto tail_recurse;
	      }
	  }
      }
      break;

    case Lisp_Cons:
      {
	register Lisp_Object car, tem;

	/* A cons cell: five distinct cases.
	   If first element is :eval or :propertize, do something special.
	   If first element is a string or a cons, process all the elements
	   and effectively concatenate them.
	   If first element is a negative number, truncate displaying cdr to
	   at most that many characters.  If positive, pad (with spaces)
	   to at least that many characters.
	   If first element is a symbol, process the cadr or caddr recursively
	   according to whether the symbol's value is non-nil or nil.  */
	car = XCAR (elt);
	if (EQ (car, QCeval))
	  {
	    /* An element of the form (:eval FORM) means evaluate FORM
	       and use the result as mode line elements.  */

	    if (CONSP (XCDR (elt)))
	      {
		Lisp_Object spec;
		spec = safe_eval (XCAR (XCDR (elt)));
		n += display_mode_element (it, depth, field_width - n,
					   precision - n, spec, props);
	      }
	  }
	else if (EQ (car, QCpropertize))
	  {
	    if (CONSP (XCDR (elt)))
	      {
		/* An element of the form (:propertize ELT PROPS...)
		   means display ELT but applying properties PROPS.  */
		n += display_mode_element (it, depth, field_width - n,
					   precision - n, XCAR (XCDR (elt)),
					   XCDR (XCDR (elt)));
	      }
	  }
	else if (SYMBOLP (car))
	  {
	    tem = Fboundp (car);
	    elt = XCDR (elt);
	    if (!CONSP (elt))
	      goto invalid;
	    /* elt is now the cdr, and we know it is a cons cell.
	       Use its car if CAR has a non-nil value.  */
	    if (!NILP (tem))
	      {
		tem = Fsymbol_value (car);
		if (!NILP (tem))
		  {
		    elt = XCAR (elt);
		    goto tail_recurse;
		  }
	      }
	    /* Symbol's value is nil (or symbol is unbound)
	       Get the cddr of the original list
	       and if possible find the caddr and use that.  */
	    elt = XCDR (elt);
	    if (NILP (elt))
	      break;
	    else if (!CONSP (elt))
	      goto invalid;
	    elt = XCAR (elt);
	    goto tail_recurse;
	  }
	else if (INTEGERP (car))
	  {
	    register int lim = XINT (car);
	    elt = XCDR (elt);
	    if (lim < 0)
	      {
		/* Negative int means reduce maximum width.  */
		if (precision <= 0)
		  precision = -lim;
		else
		  precision = min (precision, -lim);
	      }
	    else if (lim > 0)
	      {
		/* Padding specified.  Don't let it be more than
		   current maximum.  */
		if (precision > 0)
		  lim = min (precision, lim);

		/* If that's more padding than already wanted, queue it.
		   But don't reduce padding already specified even if
		   that is beyond the current truncation point.  */
		field_width = max (lim, field_width);
	      }
	    goto tail_recurse;
	  }
	else if (STRINGP (car) || CONSP (car))
	  {
	    register int limit = 50;
	    /* Limit is to protect against circular lists.  */
	    while (CONSP (elt)
		   && --limit > 0
		   && (precision <= 0 || n < precision))
	      {
		n += display_mode_element (it, depth, field_width - n,
					   precision - n, XCAR (elt), props);
		elt = XCDR (elt);
	      }
	  }
      }
      break;

    default:
    invalid:
      if (frame_title_ptr)
	n += store_frame_title ("*invalid*", 0, precision - n);
      else
	n += display_string ("*invalid*", Qnil, Qnil, 0, 0, it, 0,
			     precision - n, 0, 0);
      return n;
    }

  /* Pad to FIELD_WIDTH.  */
  if (field_width > 0 && n < field_width)
    {
      if (frame_title_ptr)
	n += store_frame_title ("", field_width - n, 0);
      else
	n += display_string ("", Qnil, Qnil, 0, 0, it, field_width - n,
			     0, 0, 0);
    }
  
  return n;
}


/* Write a null-terminated, right justified decimal representation of
   the positive integer D to BUF using a minimal field width WIDTH.  */

static void
pint2str (buf, width, d)
     register char *buf;
     register int width;
     register int d;
{
  register char *p = buf;
  
  if (d <= 0)
    *p++ = '0';
  else
    {
      while (d > 0)
	{
	  *p++ = d % 10 + '0';
	  d /= 10;
	}
    }
  
  for (width -= (int) (p - buf); width > 0; --width)
    *p++ = ' ';
  *p-- = '\0';
  while (p > buf)
    {
      d = *buf;
      *buf++ = *p;
      *p-- = d;
    }
}

/* Set a mnemonic character for coding_system (Lisp symbol) in BUF.
   If EOL_FLAG is 1, set also a mnemonic character for end-of-line
   type of CODING_SYSTEM.  Return updated pointer into BUF.  */

static unsigned char invalid_eol_type[] = "(*invalid*)";

static char *
decode_mode_spec_coding (coding_system, buf, eol_flag)
     Lisp_Object coding_system;
     register char *buf;
     int eol_flag;
{
  Lisp_Object val;
  int multibyte = !NILP (current_buffer->enable_multibyte_characters);
  unsigned char *eol_str;
  int eol_str_len;
  /* The EOL conversion we are using.  */
  Lisp_Object eoltype;

  val = Fget (coding_system, Qcoding_system);
  eoltype = Qnil;

  if (!VECTORP (val))		/* Not yet decided.  */
    {
      if (multibyte)
	*buf++ = '-';
      if (eol_flag)
	eoltype = eol_mnemonic_undecided;
      /* Don't mention EOL conversion if it isn't decided.  */
    }
  else
    {
      Lisp_Object eolvalue;

      eolvalue = Fget (coding_system, Qeol_type);

      if (multibyte)
	*buf++ = XFASTINT (AREF (val, 1));

      if (eol_flag)
	{
	  /* The EOL conversion that is normal on this system.  */

	  if (NILP (eolvalue))	/* Not yet decided.  */
	    eoltype = eol_mnemonic_undecided;
	  else if (VECTORP (eolvalue)) /* Not yet decided.  */
	    eoltype = eol_mnemonic_undecided;
	  else			/* INTEGERP (eolvalue) -- 0:LF, 1:CRLF, 2:CR */
	    eoltype = (XFASTINT (eolvalue) == 0
		       ? eol_mnemonic_unix
		       : (XFASTINT (eolvalue) == 1
			  ? eol_mnemonic_dos : eol_mnemonic_mac));
	}
    }
  
  if (eol_flag)
    {
      /* Mention the EOL conversion if it is not the usual one.  */
      if (STRINGP (eoltype))
	{
	  eol_str = SDATA (eoltype);
	  eol_str_len = SBYTES (eoltype);
	}
      else if (INTEGERP (eoltype)
	       && CHAR_VALID_P (XINT (eoltype), 0))
	{
	  eol_str = (unsigned char *) alloca (MAX_MULTIBYTE_LENGTH);
	  eol_str_len = CHAR_STRING (XINT (eoltype), eol_str);
	}
      else
	{
	  eol_str = invalid_eol_type;
	  eol_str_len = sizeof (invalid_eol_type) - 1;
	}
      bcopy (eol_str, buf, eol_str_len);
      buf += eol_str_len;
    }

  return buf;
}

/* Return a string for the output of a mode line %-spec for window W,
   generated by character C.  PRECISION >= 0 means don't return a
   string longer than that value.  FIELD_WIDTH > 0 means pad the
   string returned with spaces to that value.  Return 1 in *MULTIBYTE
   if the result is multibyte text.  */

static char lots_of_dashes[] = "--------------------------------------------------------------------------------------------------------------------------------------------";

static char *
decode_mode_spec (w, c, field_width, precision, multibyte)
     struct window *w;
     register int c;
     int field_width, precision;
     int *multibyte;
{
  Lisp_Object obj;
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  char *decode_mode_spec_buf = f->decode_mode_spec_buffer;
  struct buffer *b = XBUFFER (w->buffer);

  obj = Qnil;
  *multibyte = 0;

  switch (c)
    {
    case '*':
      if (!NILP (b->read_only))
	return "%";
      if (BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
	return "*";
      return "-";

    case '+':
      /* This differs from %* only for a modified read-only buffer.  */
      if (BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
	return "*";
      if (!NILP (b->read_only))
	return "%";
      return "-";

    case '&':
      /* This differs from %* in ignoring read-only-ness.  */
      if (BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
	return "*";
      return "-";

    case '%':
      return "%";

    case '[': 
      {
	int i;
	char *p;

	if (command_loop_level > 5)
	  return "[[[... ";
	p = decode_mode_spec_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = '[';
	*p = 0;
	return decode_mode_spec_buf;
      }

    case ']': 
      {
	int i;
	char *p;

	if (command_loop_level > 5)
	  return " ...]]]";
	p = decode_mode_spec_buf;
	for (i = 0; i < command_loop_level; i++)
	  *p++ = ']';
	*p = 0;
	return decode_mode_spec_buf;
      }

    case '-':
      {
	register int i;

	/* Let lots_of_dashes be a string of infinite length.  */
	if (field_width <= 0
	    || field_width > sizeof (lots_of_dashes))
	  {
	    for (i = 0; i < FRAME_MESSAGE_BUF_SIZE (f) - 1; ++i)
	      decode_mode_spec_buf[i] = '-';
	    decode_mode_spec_buf[i] = '\0';
	    return decode_mode_spec_buf;
	  }
	else
	  return lots_of_dashes;
      }

    case 'b': 
      obj = b->name;
      break;

    case 'c':
      {
	int col = current_column ();
	w->column_number_displayed = make_number (col);
	pint2str (decode_mode_spec_buf, field_width, col);
	return decode_mode_spec_buf;
      }

    case 'F':
      /* %F displays the frame name.  */
      if (!NILP (f->title))
	return (char *) SDATA (f->title);
      if (f->explicit_name || ! FRAME_WINDOW_P (f))
	return (char *) SDATA (f->name);
      return "Emacs";

    case 'f': 
      obj = b->filename;
      break;

    case 'l':
      {
	int startpos = XMARKER (w->start)->charpos;
	int startpos_byte = marker_byte_position (w->start);
	int line, linepos, linepos_byte, topline;
	int nlines, junk;
	int height = XFASTINT (w->height);

	/* If we decided that this buffer isn't suitable for line numbers, 
	   don't forget that too fast.  */
	if (EQ (w->base_line_pos, w->buffer))
	  goto no_value;
	/* But do forget it, if the window shows a different buffer now.  */
	else if (BUFFERP (w->base_line_pos))
	  w->base_line_pos = Qnil;

	/* If the buffer is very big, don't waste time.  */
	if (INTEGERP (Vline_number_display_limit)
	    && BUF_ZV (b) - BUF_BEGV (b) > XINT (Vline_number_display_limit))
	  {
	    w->base_line_pos = Qnil;
	    w->base_line_number = Qnil;
	    goto no_value;
	  }

	if (!NILP (w->base_line_number)
	    && !NILP (w->base_line_pos)
	    && XFASTINT (w->base_line_pos) <= startpos)
	  {
	    line = XFASTINT (w->base_line_number);
	    linepos = XFASTINT (w->base_line_pos);
	    linepos_byte = buf_charpos_to_bytepos (b, linepos);
	  }
	else
	  {
	    line = 1;
	    linepos = BUF_BEGV (b);
	    linepos_byte = BUF_BEGV_BYTE (b);
	  }

	/* Count lines from base line to window start position.  */
	nlines = display_count_lines (linepos, linepos_byte,
				      startpos_byte,
				      startpos, &junk);

	topline = nlines + line;

	/* Determine a new base line, if the old one is too close
	   or too far away, or if we did not have one.
	   "Too close" means it's plausible a scroll-down would
	   go back past it.  */
	if (startpos == BUF_BEGV (b))
	  {
	    w->base_line_number = make_number (topline);
	    w->base_line_pos = make_number (BUF_BEGV (b));
	  }
	else if (nlines < height + 25 || nlines > height * 3 + 50
		 || linepos == BUF_BEGV (b))
	  {
	    int limit = BUF_BEGV (b);
	    int limit_byte = BUF_BEGV_BYTE (b);
	    int position;
	    int distance = (height * 2 + 30) * line_number_display_limit_width;

	    if (startpos - distance > limit)
	      {
		limit = startpos - distance;
		limit_byte = CHAR_TO_BYTE (limit);
	      }

	    nlines = display_count_lines (startpos, startpos_byte,
					  limit_byte,
					  - (height * 2 + 30),
					  &position);
	    /* If we couldn't find the lines we wanted within 
	       line_number_display_limit_width chars per line,
	       give up on line numbers for this window.  */
	    if (position == limit_byte && limit == startpos - distance)
	      {
		w->base_line_pos = w->buffer;
		w->base_line_number = Qnil;
		goto no_value;
	      }

	    w->base_line_number = make_number (topline - nlines);
	    w->base_line_pos = make_number (BYTE_TO_CHAR (position));
	  }

	/* Now count lines from the start pos to point.  */
	nlines = display_count_lines (startpos, startpos_byte,
				      PT_BYTE, PT, &junk);

	/* Record that we did display the line number.  */
	line_number_displayed = 1;

	/* Make the string to show.  */
	pint2str (decode_mode_spec_buf, field_width, topline + nlines);
	return decode_mode_spec_buf;
    no_value:
        {
	  char* p = decode_mode_spec_buf;
	  int pad = field_width - 2;
	  while (pad-- > 0)
	    *p++ = ' ';
	  *p++ = '?';
	  *p++ = '?';
	  *p = '\0';
	  return decode_mode_spec_buf;
	}
      }
      break;

    case 'm': 
      obj = b->mode_name;
      break;

    case 'n':
      if (BUF_BEGV (b) > BUF_BEG (b) || BUF_ZV (b) < BUF_Z (b))
	return " Narrow";
      break;

    case 'p':
      {
	int pos = marker_position (w->start);
	int total = BUF_ZV (b) - BUF_BEGV (b);

	if (XFASTINT (w->window_end_pos) <= BUF_Z (b) - BUF_ZV (b))
	  {
	    if (pos <= BUF_BEGV (b))
	      return "All";
	    else
	      return "Bottom";
	  }
	else if (pos <= BUF_BEGV (b))
	  return "Top";
	else
	  {
	    if (total > 1000000)
	      /* Do it differently for a large value, to avoid overflow.  */
	      total = ((pos - BUF_BEGV (b)) + (total / 100) - 1) / (total / 100);
	    else
	      total = ((pos - BUF_BEGV (b)) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    sprintf (decode_mode_spec_buf, "%2d%%", total);
	    return decode_mode_spec_buf;
	  }
      }

      /* Display percentage of size above the bottom of the screen.  */
    case 'P':
      {
	int toppos = marker_position (w->start);
	int botpos = BUF_Z (b) - XFASTINT (w->window_end_pos);
	int total = BUF_ZV (b) - BUF_BEGV (b);

	if (botpos >= BUF_ZV (b))
	  {
	    if (toppos <= BUF_BEGV (b))
	      return "All";
	    else
	      return "Bottom";
	  }
	else
	  {
	    if (total > 1000000)
	      /* Do it differently for a large value, to avoid overflow.  */
	      total = ((botpos - BUF_BEGV (b)) + (total / 100) - 1) / (total / 100);
	    else
	      total = ((botpos - BUF_BEGV (b)) * 100 + total - 1) / total;
	    /* We can't normally display a 3-digit number,
	       so get us a 2-digit number that is close.  */
	    if (total == 100)
	      total = 99;
	    if (toppos <= BUF_BEGV (b))
	      sprintf (decode_mode_spec_buf, "Top%2d%%", total);
	    else
	      sprintf (decode_mode_spec_buf, "%2d%%", total);
	    return decode_mode_spec_buf;
	  }
      }

    case 's':
      /* status of process */
      obj = Fget_buffer_process (w->buffer);
      if (NILP (obj))
	return "no process";
#ifdef subprocesses
      obj = Fsymbol_name (Fprocess_status (obj));
#endif
      break;

    case 't':			/* indicate TEXT or BINARY */
#ifdef MODE_LINE_BINARY_TEXT
      return MODE_LINE_BINARY_TEXT (b);
#else
      return "T";
#endif

    case 'z':
      /* coding-system (not including end-of-line format) */
    case 'Z':
      /* coding-system (including end-of-line type) */
      {
	int eol_flag = (c == 'Z');
	char *p = decode_mode_spec_buf;

	if (! FRAME_WINDOW_P (f))
	  {
	    /* No need to mention EOL here--the terminal never needs
	       to do EOL conversion.  */
	    p = decode_mode_spec_coding (keyboard_coding.symbol, p, 0);
	    p = decode_mode_spec_coding (terminal_coding.symbol, p, 0);
	  }
	p = decode_mode_spec_coding (b->buffer_file_coding_system,
				     p, eol_flag);

#if 0 /* This proves to be annoying; I think we can do without.  -- rms.  */
#ifdef subprocesses
	obj = Fget_buffer_process (Fcurrent_buffer ());
	if (PROCESSP (obj))
	  {
	    p = decode_mode_spec_coding (XPROCESS (obj)->decode_coding_system,
					 p, eol_flag);
	    p = decode_mode_spec_coding (XPROCESS (obj)->encode_coding_system,
					 p, eol_flag);
	  }
#endif /* subprocesses */
#endif /* 0 */
	*p = 0;
	return decode_mode_spec_buf;
      }
    }

  if (STRINGP (obj))
    {
      *multibyte = STRING_MULTIBYTE (obj);
      return (char *) SDATA (obj);
    }
  else
    return "";
}


/* Count up to COUNT lines starting from START / START_BYTE.
   But don't go beyond LIMIT_BYTE.
   Return the number of lines thus found (always nonnegative).

   Set *BYTE_POS_PTR to 1 if we found COUNT lines, 0 if we hit LIMIT.  */

static int
display_count_lines (start, start_byte, limit_byte, count, byte_pos_ptr)
     int start, start_byte, limit_byte, count;
     int *byte_pos_ptr;
{
  register unsigned char *cursor;
  unsigned char *base;

  register int ceiling;
  register unsigned char *ceiling_addr;
  int orig_count = count;

  /* If we are not in selective display mode,
     check only for newlines.  */
  int selective_display = (!NILP (current_buffer->selective_display)
			   && !INTEGERP (current_buffer->selective_display));

  if (count > 0)
    {
      while (start_byte < limit_byte)
	{
	  ceiling =  BUFFER_CEILING_OF (start_byte);
	  ceiling = min (limit_byte - 1, ceiling);
	  ceiling_addr = BYTE_POS_ADDR (ceiling) + 1;
	  base = (cursor = BYTE_POS_ADDR (start_byte));
	  while (1)
	    {
	      if (selective_display)
		while (*cursor != '\n' && *cursor != 015 && ++cursor != ceiling_addr)
		  ;
	      else
		while (*cursor != '\n' && ++cursor != ceiling_addr)
		  ;

	      if (cursor != ceiling_addr)
		{
		  if (--count == 0)
		    {
		      start_byte += cursor - base + 1;
		      *byte_pos_ptr = start_byte;
		      return orig_count;
		    }
		  else
		    if (++cursor == ceiling_addr)
		      break;
		}
	      else
		break;
	    }
	  start_byte += cursor - base;
	}
    }
  else
    {
      while (start_byte > limit_byte)
	{
	  ceiling = BUFFER_FLOOR_OF (start_byte - 1);
	  ceiling = max (limit_byte, ceiling);
	  ceiling_addr = BYTE_POS_ADDR (ceiling) - 1;
	  base = (cursor = BYTE_POS_ADDR (start_byte - 1) + 1);
	  while (1)
	    {
	      if (selective_display)
		while (--cursor != ceiling_addr
		       && *cursor != '\n' && *cursor != 015)
		  ;
	      else
		while (--cursor != ceiling_addr && *cursor != '\n')
		  ;

	      if (cursor != ceiling_addr)
		{
		  if (++count == 0)
		    {
		      start_byte += cursor - base + 1;
		      *byte_pos_ptr = start_byte;
		      /* When scanning backwards, we should
			 not count the newline posterior to which we stop.  */
		      return - orig_count - 1;
		    }
		}
	      else
		break;
	    }
	  /* Here we add 1 to compensate for the last decrement
	     of CURSOR, which took it past the valid range.  */
	  start_byte += cursor - base + 1;
	}
    }

  *byte_pos_ptr = limit_byte;

  if (count < 0)
    return - orig_count + count;
  return orig_count - count;

}



/***********************************************************************
			 Displaying strings
 ***********************************************************************/

/* Display a NUL-terminated string, starting with index START.

   If STRING is non-null, display that C string.  Otherwise, the Lisp
   string LISP_STRING is displayed.

   If FACE_STRING is not nil, FACE_STRING_POS is a position in
   FACE_STRING.  Display STRING or LISP_STRING with the face at
   FACE_STRING_POS in FACE_STRING:

   Display the string in the environment given by IT, but use the
   standard display table, temporarily.

   FIELD_WIDTH is the minimum number of output glyphs to produce.
   If STRING has fewer characters than FIELD_WIDTH, pad to the right
   with spaces.  If STRING has more characters, more than FIELD_WIDTH
   glyphs will be produced.  FIELD_WIDTH <= 0 means don't pad.
   
   PRECISION is the maximum number of characters to output from
   STRING.  PRECISION < 0  means don't truncate the string.

   This is roughly equivalent to printf format specifiers:

   FIELD_WIDTH	PRECISION	PRINTF
   ----------------------------------------
   -1		-1		%s
   -1		10		%.10s
   10		-1		%10s
   20		10		%20.10s

   MULTIBYTE zero means do not display multibyte chars, > 0 means do
   display them, and < 0 means obey the current buffer's value of
   enable_multibyte_characters.

   Value is the number of glyphs produced.  */

static int
display_string (string, lisp_string, face_string, face_string_pos,
		start, it, field_width, precision, max_x, multibyte)
     unsigned char *string;
     Lisp_Object lisp_string;
     Lisp_Object face_string;
     int face_string_pos;
     int start;
     struct it *it;
     int field_width, precision, max_x;
     int multibyte;
{
  int hpos_at_start = it->hpos;
  int saved_face_id = it->face_id;
  struct glyph_row *row = it->glyph_row;

  /* Initialize the iterator IT for iteration over STRING beginning
     with index START.  */
  reseat_to_string (it, string, lisp_string, start,
		    precision, field_width, multibyte);

  /* If displaying STRING, set up the face of the iterator
     from LISP_STRING, if that's given.  */
  if (STRINGP (face_string))
    {
      int endptr;
      struct face *face;
      
      it->face_id
	= face_at_string_position (it->w, face_string, face_string_pos,
				   0, it->region_beg_charpos,
				   it->region_end_charpos,
				   &endptr, it->base_face_id, 0);
      face = FACE_FROM_ID (it->f, it->face_id);
      it->face_box_p = face->box != FACE_NO_BOX;
    }

  /* Set max_x to the maximum allowed X position.  Don't let it go
     beyond the right edge of the window.  */
  if (max_x <= 0)
    max_x = it->last_visible_x;
  else
    max_x = min (max_x, it->last_visible_x);

  /* Skip over display elements that are not visible. because IT->w is
     hscrolled.  */
  if (it->current_x < it->first_visible_x)
    move_it_in_display_line_to (it, 100000, it->first_visible_x,
				MOVE_TO_POS | MOVE_TO_X);

  row->ascent = it->max_ascent;
  row->height = it->max_ascent + it->max_descent;
  row->phys_ascent = it->max_phys_ascent;
  row->phys_height = it->max_phys_ascent + it->max_phys_descent;

  /* This condition is for the case that we are called with current_x
     past last_visible_x.  */
  while (it->current_x < max_x)
    {
      int x_before, x, n_glyphs_before, i, nglyphs;

      /* Get the next display element.  */
      if (!get_next_display_element (it))
	break;

      /* Produce glyphs.  */
      x_before = it->current_x;
      n_glyphs_before = it->glyph_row->used[TEXT_AREA];
      PRODUCE_GLYPHS (it);

      nglyphs = it->glyph_row->used[TEXT_AREA] - n_glyphs_before;
      i = 0;
      x = x_before;
      while (i < nglyphs)
	{
	  struct glyph *glyph = row->glyphs[TEXT_AREA] + n_glyphs_before + i;
	  
	  if (!it->truncate_lines_p
	      && x + glyph->pixel_width > max_x)
	    {
	      /* End of continued line or max_x reached.  */
	      if (CHAR_GLYPH_PADDING_P (*glyph))
		{
		  /* A wide character is unbreakable.  */
		  it->glyph_row->used[TEXT_AREA] = n_glyphs_before;
		  it->current_x = x_before;
		}
	      else
		{
		  it->glyph_row->used[TEXT_AREA] = n_glyphs_before + i;
		  it->current_x = x;
		}
	      break;
	    }
	  else if (x + glyph->pixel_width > it->first_visible_x)
	    {
	      /* Glyph is at least partially visible.  */
	      ++it->hpos;
	      if (x < it->first_visible_x)
		it->glyph_row->x = x - it->first_visible_x;
	    }
	  else
	    {
	      /* Glyph is off the left margin of the display area.
		 Should not happen.  */
	      abort ();
	    }

	  row->ascent = max (row->ascent, it->max_ascent);
	  row->height = max (row->height, it->max_ascent + it->max_descent);
	  row->phys_ascent = max (row->phys_ascent, it->max_phys_ascent);
	  row->phys_height = max (row->phys_height,
				  it->max_phys_ascent + it->max_phys_descent);
	  x += glyph->pixel_width;
	  ++i;
	}

      /* Stop if max_x reached.  */
      if (i < nglyphs)
	break;

      /* Stop at line ends.  */
      if (ITERATOR_AT_END_OF_LINE_P (it))
	{
	  it->continuation_lines_width = 0;
	  break;
	}

      set_iterator_to_next (it, 1);

      /* Stop if truncating at the right edge.  */
      if (it->truncate_lines_p
	  && it->current_x >= it->last_visible_x)
	{
	  /* Add truncation mark, but don't do it if the line is
	     truncated at a padding space.  */
	  if (IT_CHARPOS (*it) < it->string_nchars)
	    {
	      if (!FRAME_WINDOW_P (it->f))
		{
		  int i, n;

		  if (it->current_x > it->last_visible_x)
		    {
		      for (i = row->used[TEXT_AREA] - 1; i > 0; --i)
			if (!CHAR_GLYPH_PADDING_P (row->glyphs[TEXT_AREA][i]))
			  break;
		      for (n = row->used[TEXT_AREA]; i < n; ++i)
			{
			  row->used[TEXT_AREA] = i;
			  produce_special_glyphs (it, IT_TRUNCATION);
			}
		    }
		  produce_special_glyphs (it, IT_TRUNCATION);
		}
	      it->glyph_row->truncated_on_right_p = 1;
	    }
	  break;
	}
    }

  /* Maybe insert a truncation at the left.  */
  if (it->first_visible_x
      && IT_CHARPOS (*it) > 0)
    {
      if (!FRAME_WINDOW_P (it->f))
	insert_left_trunc_glyphs (it);
      it->glyph_row->truncated_on_left_p = 1;
    }

  it->face_id = saved_face_id;
  
  /* Value is number of columns displayed.  */
  return it->hpos - hpos_at_start;
}



/* This is like a combination of memq and assq.  Return 1/2 if PROPVAL
   appears as an element of LIST or as the car of an element of LIST.
   If PROPVAL is a list, compare each element against LIST in that
   way, and return 1/2 if any element of PROPVAL is found in LIST.
   Otherwise return 0.  This function cannot quit.
   The return value is 2 if the text is invisible but with an ellipsis
   and 1 if it's invisible and without an ellipsis.  */

int
invisible_p (propval, list)
     register Lisp_Object propval;
     Lisp_Object list;
{
  register Lisp_Object tail, proptail;
  
  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object tem;
      tem = XCAR (tail);
      if (EQ (propval, tem))
	return 1;
      if (CONSP (tem) && EQ (propval, XCAR (tem)))
	return NILP (XCDR (tem)) ? 1 : 2;
    }
  
  if (CONSP (propval))
    {
      for (proptail = propval; CONSP (proptail); proptail = XCDR (proptail))
	{
	  Lisp_Object propelt;
	  propelt = XCAR (proptail);
	  for (tail = list; CONSP (tail); tail = XCDR (tail))
	    {
	      register Lisp_Object tem;
	      tem = XCAR (tail);
	      if (EQ (propelt, tem))
		return 1;
	      if (CONSP (tem) && EQ (propelt, XCAR (tem)))
		return NILP (XCDR (tem)) ? 1 : 2;
	    }
	}
    }
  
  return 0;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_xdisp ()
{
  Vwith_echo_area_save_vector = Qnil;
  staticpro (&Vwith_echo_area_save_vector);

  Vmessage_stack = Qnil;
  staticpro (&Vmessage_stack);
  
  Qinhibit_redisplay = intern ("inhibit-redisplay");
  staticpro (&Qinhibit_redisplay);

  message_dolog_marker1 = Fmake_marker ();
  staticpro (&message_dolog_marker1);
  message_dolog_marker2 = Fmake_marker ();
  staticpro (&message_dolog_marker2);
  message_dolog_marker3 = Fmake_marker ();
  staticpro (&message_dolog_marker3);

#if GLYPH_DEBUG
  defsubr (&Sdump_glyph_matrix);
  defsubr (&Sdump_glyph_row);
  defsubr (&Sdump_tool_bar_row);
  defsubr (&Strace_redisplay);
  defsubr (&Strace_to_stderr);
#endif
#ifdef HAVE_WINDOW_SYSTEM
  defsubr (&Stool_bar_lines_needed);
#endif

  staticpro (&Qmenu_bar_update_hook);
  Qmenu_bar_update_hook = intern ("menu-bar-update-hook");

  staticpro (&Qoverriding_terminal_local_map);
  Qoverriding_terminal_local_map = intern ("overriding-terminal-local-map");

  staticpro (&Qoverriding_local_map);
  Qoverriding_local_map = intern ("overriding-local-map");

  staticpro (&Qwindow_scroll_functions);
  Qwindow_scroll_functions = intern ("window-scroll-functions");

  staticpro (&Qredisplay_end_trigger_functions);
  Qredisplay_end_trigger_functions = intern ("redisplay-end-trigger-functions");
  
  staticpro (&Qinhibit_point_motion_hooks);
  Qinhibit_point_motion_hooks = intern ("inhibit-point-motion-hooks");

  QCdata = intern (":data");
  staticpro (&QCdata);
  Qdisplay = intern ("display");
  staticpro (&Qdisplay);
  Qspace_width = intern ("space-width");
  staticpro (&Qspace_width);
  Qraise = intern ("raise");
  staticpro (&Qraise);
  Qspace = intern ("space");
  staticpro (&Qspace);
  Qmargin = intern ("margin");
  staticpro (&Qmargin);
  Qleft_margin = intern ("left-margin");
  staticpro (&Qleft_margin);
  Qright_margin = intern ("right-margin");
  staticpro (&Qright_margin);
  Qalign_to = intern ("align-to");
  staticpro (&Qalign_to);
  QCalign_to = intern (":align-to");
  staticpro (&QCalign_to);
  Qrelative_width = intern ("relative-width");
  staticpro (&Qrelative_width);
  QCrelative_width = intern (":relative-width");
  staticpro (&QCrelative_width);
  QCrelative_height = intern (":relative-height");
  staticpro (&QCrelative_height);
  QCeval = intern (":eval");
  staticpro (&QCeval);
  QCpropertize = intern (":propertize");
  staticpro (&QCpropertize);
  Qwhen = intern ("when");
  staticpro (&Qwhen);
  QCfile = intern (":file");
  staticpro (&QCfile);
  Qfontified = intern ("fontified");
  staticpro (&Qfontified);
  Qfontification_functions = intern ("fontification-functions");
  staticpro (&Qfontification_functions);
  Qtrailing_whitespace = intern ("trailing-whitespace");
  staticpro (&Qtrailing_whitespace);
  Qimage = intern ("image");
  staticpro (&Qimage);
  Qmessage_truncate_lines = intern ("message-truncate-lines");
  staticpro (&Qmessage_truncate_lines);
  Qcursor_in_non_selected_windows = intern ("cursor-in-non-selected-windows");
  staticpro (&Qcursor_in_non_selected_windows);
  Qgrow_only = intern ("grow-only");
  staticpro (&Qgrow_only);
  Qinhibit_menubar_update = intern ("inhibit-menubar-update");
  staticpro (&Qinhibit_menubar_update);
  Qinhibit_eval_during_redisplay = intern ("inhibit-eval-during-redisplay");
  staticpro (&Qinhibit_eval_during_redisplay);
  Qposition = intern ("position");
  staticpro (&Qposition);
  Qbuffer_position = intern ("buffer-position");
  staticpro (&Qbuffer_position);
  Qobject = intern ("object");
  staticpro (&Qobject);

  list_of_error = Fcons (intern ("error"), Qnil);
  staticpro (&list_of_error);

  last_arrow_position = Qnil;
  last_arrow_string = Qnil;
  staticpro (&last_arrow_position);
  staticpro (&last_arrow_string);
  
  echo_buffer[0] = echo_buffer[1] = Qnil;
  staticpro (&echo_buffer[0]);
  staticpro (&echo_buffer[1]);

  echo_area_buffer[0] = echo_area_buffer[1] = Qnil;
  staticpro (&echo_area_buffer[0]);
  staticpro (&echo_area_buffer[1]);

  Vmessages_buffer_name = build_string ("*Messages*");
  staticpro (&Vmessages_buffer_name);

  mode_line_proptrans_alist = Qnil;
  staticpro (&mode_line_proptrans_alist);
  
  DEFVAR_LISP ("show-trailing-whitespace", &Vshow_trailing_whitespace,
    doc: /* Non-nil means highlight trailing whitespace.
The face used for trailing whitespace is `trailing-whitespace'.  */);
  Vshow_trailing_whitespace = Qnil;

  DEFVAR_LISP ("inhibit-redisplay", &Vinhibit_redisplay,
    doc: /* Non-nil means don't actually do any redisplay.
This is used for internal purposes.  */);
  Vinhibit_redisplay = Qnil;

  DEFVAR_LISP ("global-mode-string", &Vglobal_mode_string,
    doc: /* String (or mode line construct) included (normally) in `mode-line-format'.  */);
  Vglobal_mode_string = Qnil;

  DEFVAR_LISP ("overlay-arrow-position", &Voverlay_arrow_position,
    doc: /* Marker for where to display an arrow on top of the buffer text.
This must be the beginning of a line in order to work.
See also `overlay-arrow-string'.  */);
  Voverlay_arrow_position = Qnil;

  DEFVAR_LISP ("overlay-arrow-string", &Voverlay_arrow_string,
    doc: /* String to display as an arrow.  See also `overlay-arrow-position'.  */);
  Voverlay_arrow_string = Qnil;

  DEFVAR_INT ("scroll-step", &scroll_step,
    doc: /* *The number of lines to try scrolling a window by when point moves out.
If that fails to bring point back on frame, point is centered instead.
If this is zero, point is always centered after it moves off frame.
If you want scrolling to always be a line at a time, you should set
`scroll-conservatively' to a large value rather than set this to 1.  */);

  DEFVAR_INT ("scroll-conservatively", &scroll_conservatively,
    doc: /* *Scroll up to this many lines, to bring point back on screen.
A value of zero means to scroll the text to center point vertically
in the window.  */);
  scroll_conservatively = 0;

  DEFVAR_INT ("scroll-margin", &scroll_margin,
    doc: /* *Number of lines of margin at the top and bottom of a window.
Recenter the window whenever point gets within this many lines
of the top or bottom of the window.  */);
  scroll_margin = 0;

#if GLYPH_DEBUG
  DEFVAR_INT ("debug-end-pos", &debug_end_pos, doc: /* Don't ask.  */);
#endif

  DEFVAR_BOOL ("truncate-partial-width-windows",
	       &truncate_partial_width_windows,
    doc: /* *Non-nil means truncate lines in all windows less than full frame wide.  */);
  truncate_partial_width_windows = 1;

  DEFVAR_BOOL ("mode-line-inverse-video", &mode_line_inverse_video,
    doc: /* nil means display the mode-line/header-line/menu-bar in the default face.
Any other value means to use the appropriate face, `mode-line',
`header-line', or `menu' respectively.

This variable is deprecated; please change the above faces instead.  */);
  mode_line_inverse_video = 1;

  DEFVAR_LISP ("line-number-display-limit", &Vline_number_display_limit,
    doc: /* *Maximum buffer size for which line number should be displayed.
If the buffer is bigger than this, the line number does not appear
in the mode line.  A value of nil means no limit.  */);
  Vline_number_display_limit = Qnil;

  DEFVAR_INT ("line-number-display-limit-width",
	      &line_number_display_limit_width,
    doc: /* *Maximum line width (in characters) for line number display.
If the average length of the lines near point is bigger than this, then the
line number may be omitted from the mode line.  */);
  line_number_display_limit_width = 200;

  DEFVAR_BOOL ("highlight-nonselected-windows", &highlight_nonselected_windows,
    doc: /* *Non-nil means highlight region even in nonselected windows.  */);
  highlight_nonselected_windows = 0;

  DEFVAR_BOOL ("multiple-frames", &multiple_frames,
    doc: /* Non-nil if more than one frame is visible on this display.
Minibuffer-only frames don't count, but iconified frames do.
This variable is not guaranteed to be accurate except while processing
`frame-title-format' and `icon-title-format'.  */);

  DEFVAR_LISP ("frame-title-format", &Vframe_title_format,
    doc: /* Template for displaying the title bar of visible frames.
\(Assuming the window manager supports this feature.)
This variable has the same structure as `mode-line-format' (which see),
and is used only on frames for which no explicit name has been set
\(see `modify-frame-parameters').  */);
  DEFVAR_LISP ("icon-title-format", &Vicon_title_format,
    doc: /* Template for displaying the title bar of an iconified frame.
\(Assuming the window manager supports this feature.)
This variable has the same structure as `mode-line-format' (which see),
and is used only on frames for which no explicit name has been set
\(see `modify-frame-parameters').  */);
  Vicon_title_format
    = Vframe_title_format
    = Fcons (intern ("multiple-frames"),
	     Fcons (build_string ("%b"),
		    Fcons (Fcons (empty_string,
				  Fcons (intern ("invocation-name"),
					 Fcons (build_string ("@"),
						Fcons (intern ("system-name"),
							       Qnil)))),
			   Qnil)));

  DEFVAR_LISP ("message-log-max", &Vmessage_log_max,
    doc: /* Maximum number of lines to keep in the message log buffer.
If nil, disable message logging.  If t, log messages but don't truncate
the buffer when it becomes large.  */);
  Vmessage_log_max = make_number (50);

  DEFVAR_LISP ("window-size-change-functions", &Vwindow_size_change_functions,
    doc: /* Functions called before redisplay, if window sizes have changed.
The value should be a list of functions that take one argument.
Just before redisplay, for each frame, if any of its windows have changed
size since the last redisplay, or have been split or deleted,
all the functions in the list are called, with the frame as argument.  */);
  Vwindow_size_change_functions = Qnil;

  DEFVAR_LISP ("window-scroll-functions", &Vwindow_scroll_functions,
    doc: /* List of Functions to call before redisplaying a window with scrolling.
Each function is called with two arguments, the window
and its new display-start position.  Note that the value of `window-end'
is not valid when these functions are called.  */);
  Vwindow_scroll_functions = Qnil;
  
  DEFVAR_BOOL ("auto-resize-tool-bars", &auto_resize_tool_bars_p,
    doc: /* *Non-nil means automatically resize tool-bars.
This increases a tool-bar's height if not all tool-bar items are visible.
It decreases a tool-bar's height when it would display blank lines
otherwise.  */);
  auto_resize_tool_bars_p = 1;
  
  DEFVAR_BOOL ("auto-raise-tool-bar-buttons", &auto_raise_tool_bar_buttons_p,
    doc: /* *Non-nil means raise tool-bar buttons when the mouse moves over them.  */);
  auto_raise_tool_bar_buttons_p = 1;

  DEFVAR_LISP ("tool-bar-button-margin", &Vtool_bar_button_margin,
    doc: /* *Margin around tool-bar buttons in pixels.
If an integer, use that for both horizontal and vertical margins.
Otherwise, value should be a pair of integers `(HORZ . VERT)' with
HORZ specifying the horizontal margin, and VERT specifying the
vertical margin.  */);
  Vtool_bar_button_margin = make_number (DEFAULT_TOOL_BAR_BUTTON_MARGIN);

  DEFVAR_INT ("tool-bar-button-relief", &tool_bar_button_relief,
    doc: /* *Relief thickness of tool-bar buttons.  */);
  tool_bar_button_relief = DEFAULT_TOOL_BAR_BUTTON_RELIEF;

  DEFVAR_LISP ("fontification-functions", &Vfontification_functions,
    doc: /* List of functions to call to fontify regions of text.
Each function is called with one argument POS.  Functions must
fontify a region starting at POS in the current buffer, and give
fontified regions the property `fontified'.  */);
  Vfontification_functions = Qnil;
  Fmake_variable_buffer_local (Qfontification_functions);

  DEFVAR_BOOL ("unibyte-display-via-language-environment",
               &unibyte_display_via_language_environment,
    doc: /* *Non-nil means display unibyte text according to language environment.
Specifically this means that unibyte non-ASCII characters
are displayed by converting them to the equivalent multibyte characters
according to the current language environment.  As a result, they are
displayed according to the current fontset.  */);
  unibyte_display_via_language_environment = 0;

  DEFVAR_LISP ("max-mini-window-height", &Vmax_mini_window_height,
    doc: /* *Maximum height for resizing mini-windows.
If a float, it specifies a fraction of the mini-window frame's height.
If an integer, it specifies a number of lines.  */);
  Vmax_mini_window_height = make_float (0.25);

  DEFVAR_LISP ("resize-mini-windows", &Vresize_mini_windows,
    doc: /* *How to resize mini-windows.
A value of nil means don't automatically resize mini-windows.
A value of t means resize them to fit the text displayed in them.
A value of `grow-only', the default, means let mini-windows grow
only, until their display becomes empty, at which point the windows
go back to their normal size.  */);
  Vresize_mini_windows = Qgrow_only;

  DEFVAR_BOOL ("cursor-in-non-selected-windows",
	       &cursor_in_non_selected_windows,
    doc: /* *Non-nil means display a hollow cursor in non-selected windows.
nil means don't display a cursor there.  */);
  cursor_in_non_selected_windows = 1;
  
  DEFVAR_BOOL ("auto-hscroll-mode", &automatic_hscrolling_p,
    doc: /* *Non-nil means scroll the display automatically to make point visible.  */);
  automatic_hscrolling_p = 1;

  DEFVAR_INT ("hscroll-margin", &hscroll_margin,
    doc: /* *How many columns away from the window edge point is allowed to get
before automatic hscrolling will horizontally scroll the window.  */);
  hscroll_margin = 5;

  DEFVAR_LISP ("hscroll-step", &Vhscroll_step,
    doc: /* *How many columns to scroll the window when point gets too close to the edge.
When point is less than `automatic-hscroll-margin' columns from the window
edge, automatic hscrolling will scroll the window by the amount of columns
determined by this variable.  If its value is a positive integer, scroll that
many columns.  If it's a positive floating-point number, it specifies the
fraction of the window's width to scroll.  If it's nil or zero, point will be
centered horizontally after the scroll.  Any other value, including negative
numbers, are treated as if the value were zero.

Automatic hscrolling always moves point outside the scroll margin, so if
point was more than scroll step columns inside the margin, the window will
scroll more than the value given by the scroll step.

Note that the lower bound for automatic hscrolling specified by `scroll-left'
and `scroll-right' overrides this variable's effect.  */);
  Vhscroll_step = make_number (0);
  
  DEFVAR_LISP ("image-types", &Vimage_types,
    doc: /* List of supported image types.
Each element of the list is a symbol for a supported image type.  */);
  Vimage_types = Qnil;
  
  DEFVAR_BOOL ("message-truncate-lines", &message_truncate_lines,
    doc: /* If non-nil, messages are truncated instead of resizing the echo area.
Bind this around calls to `message' to let it take effect.  */);
  message_truncate_lines = 0;

  DEFVAR_LISP ("menu-bar-update-hook",  &Vmenu_bar_update_hook,
    doc: /* Normal hook run for clicks on menu bar, before displaying a submenu.
Can be used to update submenus whose contents should vary.  */);
  Vmenu_bar_update_hook = Qnil;
  
  DEFVAR_BOOL ("inhibit-menubar-update", &inhibit_menubar_update,
    doc: /* Non-nil means don't update menu bars.  Internal use only.  */);
  inhibit_menubar_update = 0;

  DEFVAR_BOOL ("inhibit-eval-during-redisplay", &inhibit_eval_during_redisplay,
    doc: /* Non-nil means don't eval Lisp during redisplay.  */);
  inhibit_eval_during_redisplay = 0;

#if GLYPH_DEBUG
  DEFVAR_BOOL ("inhibit-try-window-id", &inhibit_try_window_id,
	       doc: /* Inhibit try_window_id display optimization.  */);
  inhibit_try_window_id = 0;

  DEFVAR_BOOL ("inhibit-try-window-reusing", &inhibit_try_window_reusing,
	       doc: /* Inhibit try_window_reusing display optimization.  */);
  inhibit_try_window_reusing = 0;

  DEFVAR_BOOL ("inhibit-try-cursor-movement", &inhibit_try_cursor_movement,
	       doc: /* Inhibit try_cursor_movement display optimization.  */);
  inhibit_try_cursor_movement = 0;
#endif /* GLYPH_DEBUG */
}


/* Initialize this module when Emacs starts.  */

void
init_xdisp ()
{
  Lisp_Object root_window;
  struct window *mini_w;

  current_header_line_height = current_mode_line_height = -1;

  CHARPOS (this_line_start_pos) = 0;

  mini_w = XWINDOW (minibuf_window);
  root_window = FRAME_ROOT_WINDOW (XFRAME (WINDOW_FRAME (mini_w)));

  if (!noninteractive)
    {
      struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (root_window)));
      int i;

      XWINDOW (root_window)->top = make_number (FRAME_TOP_MARGIN (f));
      set_window_height (root_window,
			 FRAME_HEIGHT (f) - 1 - FRAME_TOP_MARGIN (f),
			 0);
      mini_w->top = make_number (FRAME_HEIGHT (f) - 1);
      set_window_height (minibuf_window, 1, 0);

      XWINDOW (root_window)->width = make_number (FRAME_WIDTH (f));
      mini_w->width = make_number (FRAME_WIDTH (f));

      scratch_glyph_row.glyphs[TEXT_AREA] = scratch_glyphs;
      scratch_glyph_row.glyphs[TEXT_AREA + 1]
	= scratch_glyphs + MAX_SCRATCH_GLYPHS;

      /* The default ellipsis glyphs `...'.  */ 
      for (i = 0; i < 3; ++i)
	default_invis_vector[i] = make_number ('.');
    }

#ifdef HAVE_WINDOW_SYSTEM
  {
    /* Allocate the buffer for frame titles.  */
    int size = 100;
    frame_title_buf = (char *) xmalloc (size);
    frame_title_buf_end = frame_title_buf + size;
    frame_title_ptr = NULL;
  }
#endif /* HAVE_WINDOW_SYSTEM */
  
  help_echo_showing_p = 0;
}


