/* Hooks by which low level terminal operations
   can be made to call other routines.
   Copyright (C) 1985, 1986, 1993, 1994 Free Software Foundation, Inc.

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


/* Miscellanea.   */

struct glyph;
struct frame;

/* If nonzero, send all terminal output characters to this stream also.  */
extern FILE *termscript;

/* Only use prototypes when lisp.h has been included.  */
#ifndef P_
#define P_(X) ()
#endif

/* Text display hooks.  */

extern void (*cursor_to_hook) P_ ((int vpos, int hpos));
extern void (*raw_cursor_to_hook) P_ ((int, int));

extern void (*clear_to_end_hook) P_ ((void));
extern void (*clear_frame_hook) P_ ((void));
extern void (*clear_end_of_line_hook) P_ ((int));

extern void (*ins_del_lines_hook) P_ ((int, int));

extern void (*insert_glyphs_hook) P_ ((struct glyph *s, int n));
extern void (*write_glyphs_hook) P_ ((struct glyph *s, int n));
extern void (*delete_glyphs_hook) P_ ((int));

extern void (*ring_bell_hook) P_ ((void));

extern void (*reset_terminal_modes_hook) P_ ((void));
extern void (*set_terminal_modes_hook) P_ ((void));
extern void (*update_begin_hook) P_ ((struct frame *));
extern void (*update_end_hook) P_ ((struct frame *));
extern void (*set_terminal_window_hook) P_ ((int));



/* Multi-frame and mouse support hooks.  */

enum scroll_bar_part {
  scroll_bar_above_handle,
  scroll_bar_handle,
  scroll_bar_below_handle,
  scroll_bar_up_arrow,
  scroll_bar_down_arrow,
  scroll_bar_to_top,
  scroll_bar_to_bottom,
  scroll_bar_end_scroll,
  scroll_bar_move_ratio
};

/* Return the current position of the mouse.

   Set *f to the frame the mouse is in, or zero if the mouse is in no
   Emacs frame.  If it is set to zero, all the other arguments are
   garbage.

   If the motion started in a scroll bar, set *bar_window to the
   scroll bar's window, *part to the part the mouse is currently over,
   *x to the position of the mouse along the scroll bar, and *y to the
   overall length of the scroll bar.

   Otherwise, set *bar_window to Qnil, and *x and *y to the column and
   row of the character cell the mouse is over.

   Set *time to the time the mouse was at the returned position.

   This should clear mouse_moved until the next motion
   event arrives.  */
extern void (*mouse_position_hook) P_ ((struct frame **f, int,
					Lisp_Object *bar_window,
					enum scroll_bar_part *part,
					Lisp_Object *x,
					Lisp_Object *y,
					unsigned long *time));

/* The window system handling code should set this if the mouse has
   moved since the last call to the mouse_position_hook.  Calling that
   hook should clear this.  */
extern int mouse_moved;

/* When a frame's focus redirection is changed, this hook tells the
   window system code to re-decide where to put the highlight.  Under
   X, this means that Emacs lies about where the focus is.  */
extern void (*frame_rehighlight_hook) P_ ((struct frame *));

/* If we're displaying frames using a window system that can stack
   frames on top of each other, this hook allows you to bring a frame
   to the front, or bury it behind all the other windows.  If this
   hook is zero, that means the device we're displaying on doesn't
   support overlapping frames, so there's no need to raise or lower
   anything.

   If RAISE is non-zero, F is brought to the front, before all other
   windows.  If RAISE is zero, F is sent to the back, behind all other
   windows.  */
extern void (*frame_raise_lower_hook) P_ ((struct frame *f, int raise));


/* Scroll bar hooks.  */

/* The representation of scroll bars is determined by the code which
   implements them, except for one thing: they must be represented by
   lisp objects.  This allows us to place references to them in
   Lisp_Windows without worrying about those references becoming
   dangling references when the scroll bar is destroyed.

   The window-system-independent portion of Emacs just refers to
   scroll bars via their windows, and never looks inside the scroll bar
   representation; it always uses hook functions to do all the
   scroll bar manipulation it needs.

   The `vertical_scroll_bar' field of a Lisp_Window refers to that
   window's scroll bar, or is nil if the window doesn't have a
   scroll bar.

   The `scroll_bars' and `condemned_scroll_bars' fields of a Lisp_Frame
   are free for use by the scroll bar implementation in any way it sees
   fit.  They are marked by the garbage collector.  */


/* Set the vertical scroll bar for WINDOW to have its upper left corner
   at (TOP, LEFT), and be LENGTH rows high.  Set its handle to
   indicate that we are displaying PORTION characters out of a total
   of WHOLE characters, starting at POSITION.  If WINDOW doesn't yet
   have a scroll bar, create one for it.  */
extern void (*set_vertical_scroll_bar_hook)
            P_ ((struct window *window,
	         int portion, int whole, int position));


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - can you say set-window-configuration?
   Instead, we just assert at the beginning of redisplay that *all*
   scroll bars are to be removed, and then save scroll bars from the
   fiery pit when we actually redisplay their window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgement. 

   This should be applied to each frame each time its window tree is
   redisplayed, even if it is not displaying scroll bars at the moment;
   if the HAS_SCROLL_BARS flag has just been turned off, only calling
   this and the judge_scroll_bars_hook will get rid of them.

   If non-zero, this hook should be safe to apply to any frame,
   whether or not it can support scroll bars, and whether or not it is
   currently displaying them.  */
extern void (*condemn_scroll_bars_hook) P_ ((struct frame *frame));

/* Unmark WINDOW's scroll bar for deletion in this judgement cycle.
   Note that it's okay to redeem a scroll bar that is not condemned.  */
extern void (*redeem_scroll_bar_hook) P_ ((struct window *window));

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  

   This should be applied to each frame after each time its window
   tree is redisplayed, even if it is not displaying scroll bars at the
   moment; if the HAS_SCROLL_BARS flag has just been turned off, only
   calling this and condemn_scroll_bars_hook will get rid of them.

   If non-zero, this hook should be safe to apply to any frame,
   whether or not it can support scroll bars, and whether or not it is
   currently displaying them.  */
extern void (*judge_scroll_bars_hook) P_ ((struct frame *FRAME));


/* Input queue declarations and hooks.  */

/* Expedient hack: only provide the below definitions to files that
   are prepared to handle lispy things.  CONSP is defined iff lisp.h
   has been included before this file.  */
#ifdef CONSP

enum event_kind
{
  NO_EVENT,			/* nothing happened.  This should never
				   actually appear in the event queue.  */

  ASCII_KEYSTROKE_EVENT,	/* The ASCII code is in .code, perhaps
				   with modifiers applied.
				   .modifiers holds the state of the
				   modifier keys.
				   .frame_or_window is the frame in
				   which the key was typed.
				   .timestamp gives a timestamp (in
				   milliseconds) for the keystroke.  */
  MULTIBYTE_CHAR_KEYSTROKE_EVENT,	/* The multibyte char code is in .code,
				   perhaps with modifiers applied.
				   The others are the same as
				   ASCII_KEYSTROKE_EVENT.  This type of event
				   is generated only when we are using
				   XIM on X window.  */
  NON_ASCII_KEYSTROKE_EVENT,	/* .code is a number identifying the
				   function key.  A code N represents
				   a key whose name is
				   function_key_names[N]; function_key_names
				   is a table in keyboard.c to which you
				   should feel free to add missing keys.
				   .modifiers holds the state of the
				   modifier keys.
				   .frame_or_window is the frame in
				   which the key was typed.
				   .timestamp gives a timestamp (in
				   milliseconds) for the keystroke.  */
  TIMER_EVENT,                  /* A timer fired.  */
  MOUSE_CLICK_EVENT,		/* The button number is in .code; it must
				   be >= 0 and < NUM_MOUSE_BUTTONS, defined
				   below.
				   .modifiers holds the state of the
				   modifier keys.
				   .x and .y give the mouse position,
				   in characters, within the window.
				   .frame_or_window gives the frame
				   the mouse click occurred in.
				   .timestamp gives a timestamp (in
				   milliseconds) for the click.  */
#ifdef WINDOWSNT
  MOUSE_WHEEL_EVENT,		/* A mouse-wheel event is generated 
				   on WINDOWSNT by a 
				   wheel on a mouse (e.g., MS Intellimouse).
				   The event contains a delta that corresponds
				   to the amount and direction that the wheel
				   is rotated.  This delta is typically
				   used to implement a scroll or zoom.
				   .code gives the delta.
				   .modifiers holds the state of the
				   modifier keys.
				   .x and .y give the mouse position,
				   in characters, within the window.
				   .frame_or_window gives the frame
				   the wheel event occurred in.
				   .timestamp gives a timestamp (in
				   milliseconds) for the wheel event.  */
  LANGUAGE_CHANGE_EVENT,	/* A LANGUAGE_CHANGE_EVENT is generated
				   on WINDOWSNT when the keyboard layout
				   or input language is changed by the
				   user.  */
#endif
  SCROLL_BAR_CLICK_EVENT,	/* .code gives the number of the mouse button
				   that was clicked.
				   .modifiers holds the state of the modifier
				   keys.
				   .part is a lisp symbol indicating which
				   part of the scroll bar got clicked.
				   .x gives the distance from the start of the
				   scroll bar of the click; .y gives the total
				   length of the scroll bar.
				   .frame_or_window gives the window
				   whose scroll bar was clicked in.
				   .timestamp gives a timestamp (in
				   milliseconds) for the click.  */
#ifdef WINDOWSNT
  W32_SCROLL_BAR_CLICK_EVENT,	/* as for SCROLL_BAR_CLICK, but only generated
				   by MS-Windows scroll bar controls. */
#endif
  SELECTION_REQUEST_EVENT,	/* Another X client wants a selection from us.
				   See `struct selection_event'.  */
  SELECTION_CLEAR_EVENT,	/* Another X client cleared our selection.  */
  BUFFER_SWITCH_EVENT,		/* A process filter has switched buffers.  */
  DELETE_WINDOW_EVENT,		/* An X client said "delete this window".  */
  MENU_BAR_EVENT,		/* An event generated by the menu bar.
				   The frame_or_window field's cdr holds the
				   Lisp-level event value.
				   (Only the toolkit version uses these.)  */
  ICONIFY_EVENT,		/* An X client iconified this window.  */
  DEICONIFY_EVENT,		/* An X client deiconified this window.  */
  MENU_BAR_ACTIVATE_EVENT,      /* A button press in the menu bar
				   (toolkit version only).  */
  DRAG_N_DROP_EVENT,		/* A drag-n-drop event is generated when
				   files selected outside of Emacs are dropped
				   onto an Emacs window.
				   Currently used only on Windows NT.
				   .modifiers holds the state of the 
				   modifier keys.
				   .x and .y give the mouse position,
				   in characters, within the window.
				   .frame_or_window is a cons of the frame
				   in which the drop was made and a list of
				   the filenames of the dropped files.
				   .timestamp gives a timestamp (in
				   milliseconds) for the click.  */
  USER_SIGNAL_EVENT,		/* A user signal.
                                   code is a number identifying it,
                                   index into lispy_user_signals.  */

  /* Help events.  Member `frame_or_window' of the input_event is the
     frame on which the event occurred, and member `arg' contains
     the help to show.  */
  HELP_EVENT,

  /* An event from a tool-bar.  Member `arg' of the input event
     contains the tool-bar item selected.  If `frame_or_window'
     and `arg' are equal, this is a prefix event.  */
  TOOL_BAR_EVENT,

  /* Queued from XTread_socket on FocusIn events.  Translated into
     `switch-frame' events in kbd_buffer_get_event, if necessary.  */
  FOCUS_IN_EVENT,

  /* Generated when mouse moves over window not currently selected.  */
  SELECT_WINDOW_EVENT,

  /* Queued from XTread_socket when session manager sends
     save yourself before shutdown. */
  SAVE_SESSION_EVENT
};

/* If a struct input_event has a kind which is SELECTION_REQUEST_EVENT
   or SELECTION_CLEAR_EVENT, then its contents are really described
   by `struct selection_event'; see xterm.h.  */

/* The keyboard input buffer is an array of these structures.  Each one
   represents some sort of input event - a keystroke, a mouse click, or
   a window system event.  These get turned into their lispy forms when
   they are removed from the event queue.  */

struct input_event
{
  /* What kind of event was this?  */
  enum event_kind kind;
  
  /* For an ASCII_KEYSTROKE_EVENT and MULTIBYTE_CHAR_KEYSTROKE_EVENT,
     this is the character.
     For a NON_ASCII_KEYSTROKE_EVENT, this is the keysym code.
     For a mouse event, this is the button number.  */
  /* In WindowsNT, for a mouse wheel event, this is the delta.  */
  int code;
  enum scroll_bar_part part;

  int modifiers;		/* See enum below for interpretation.  */

  Lisp_Object x, y;
  unsigned long timestamp;

  /* This is padding just to put the frame_or_window field
     past the size of struct selection_event.  */
  int *padding[2];

  /* This field is copied into a vector while the event is in the queue,
     so that garbage collections won't kill it.  */
  /* In a menu_bar_event, this is a cons cell whose car is the frame
     and whose cdr is the Lisp object that is the event's value.  */
  /* This field is last so that struct selection_input_event
     does not overlap with it.  */
  Lisp_Object frame_or_window;

  /* Additional event argument.  This is used for TOOL_BAR_EVENTs and
     HELP_EVENTs and avoids calling Fcons during signal handling.  */
  Lisp_Object arg;
};

/* Called to read input events.  */
extern int (*read_socket_hook) P_ ((int, struct input_event *, int, int));

/* Called when a frame's display becomes entirely up to date.  */
extern void (*frame_up_to_date_hook) P_ ((struct frame *));


/* Bits in the modifiers member of the input_event structure.
   Note that reorder_modifiers assumes that the bits are in canonical
   order.  

   The modifiers applied to mouse clicks are rather ornate.  The
   window-system-specific code should store mouse clicks with
   up_modifier or down_modifier set.  Having an explicit down modifier
   simplifies some of window-system-independent code; without it, the
   code would have to recognize down events by checking if the event
   is a mouse click lacking the click and drag modifiers.

   The window-system independent code turns all up_modifier events
   bits into drag_modifier, click_modifier, double_modifier, or
   triple_modifier events.  The click_modifier has no written
   representation in the names of the symbols used as event heads,
   but it does appear in the Qevent_symbol_components property of the
   event heads.  */
enum {
  up_modifier	=   1,		/* Only used on mouse buttons - always
				   turned into a click or a drag modifier
				   before lisp code sees the event.  */
  down_modifier =   2,		/* Only used on mouse buttons.  */
  drag_modifier =   4,		/* This is never used in the event
				   queue; it's only used internally by
				   the window-system-independent code.  */
  click_modifier=   8,		/* See drag_modifier.  */
  double_modifier= 16,          /* See drag_modifier.  */
  triple_modifier= 32,          /* See drag_modifier.  */

  /* The next four modifier bits are used also in keyboard events at
     the Lisp level.

     It's probably not the greatest idea to use the 2^23 bit for any
     modifier.  It may or may not be the sign bit, depending on
     VALBITS, so using it to represent a modifier key means that
     characters thus modified have different integer equivalents
     depending on the architecture they're running on.  Oh, and
     applying XINT to a character whose 2^23 bit is set sign-extends
     it, so you get a bunch of bits in the mask you didn't want.

     The CHAR_ macros are defined in lisp.h.  */
  alt_modifier	=  CHAR_ALT,	/* Under X, the XK_Alt_[LR] keysyms.  */
  super_modifier=  CHAR_SUPER,	/* Under X, the XK_Super_[LR] keysyms.  */
  hyper_modifier=  CHAR_HYPER,	/* Under X, the XK_Hyper_[LR] keysyms.  */
  shift_modifier=  CHAR_SHIFT,
  ctrl_modifier	=  CHAR_CTL,
  meta_modifier	=  CHAR_META	/* Under X, the XK_Meta_[LR] keysyms.  */
};

#endif
