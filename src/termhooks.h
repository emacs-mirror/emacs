/* Hooks by which low level terminal operations
   can be made to call other routines.
   Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc.

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


extern int (*cursor_to_hook) ();
extern int (*raw_cursor_to_hook) ();

extern int (*clear_to_end_hook) ();
extern int (*clear_frame_hook) ();
extern int (*clear_end_of_line_hook) ();

extern int (*ins_del_lines_hook) ();

extern int (*change_line_highlight_hook) ();
extern int (*reassert_line_highlight_hook) ();

extern int (*insert_glyphs_hook) ();
extern int (*write_glyphs_hook) ();
extern int (*delete_glyphs_hook) ();

extern int (*ring_bell_hook) ();

extern int (*reset_terminal_modes_hook) ();
extern int (*set_terminal_modes_hook) ();
extern int (*update_begin_hook) ();
extern int (*update_end_hook) ();
extern int (*set_terminal_window_hook) ();

extern int (*read_socket_hook) ();

enum scrollbar_part {
  scrollbar_above_handle,
  scrollbar_handle,
  scrollbar_below_handle
};

/* Return the current position of the mouse.
   Set `bar' to point to the scrollbar if the mouse movement started
   in a scrollbar, or zero if it started elsewhere in the frame.
   This should clear mouse_moved until the next motion event arrives.  */
extern void (*mouse_position_hook) ( /* FRAME_PTR *f,
					struct scrollbar **bar,
					enum scrollbar_part *part,
					Lisp_Object *x,
					Lisp_Object *y,
					unsigned long *time */ );

/* The window system handling code should set this if the mouse has
   moved since the last call to the mouse_position_hook.  Calling that
   hook should clear this.  */
extern int mouse_moved;

/* When a frame's focus redirection is changed, this hook tells the
   window system code to re-decide where to put the highlight.  Under
   X, this means that Emacs lies about where the focus is.  */
extern void (*frame_rehighlight_hook) ( /* void */ );

/* Set vertical scollbar BAR to have its upper left corner at (TOP,
   LEFT), and be LENGTH rows high.  Set its handle to indicate that we
   are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  Return BAR.  If BAR is zero,
   create a new scrollbar and return a pointer to it.  */
extern struct scrollbar *(*set_vertical_scrollbar_hook)
     ( /* struct scrollbar *BAR,
	  struct window *window,
	  int portion, int whole, int position */ );


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scrollbars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - can you say set-window-configuration?
   Instead, we just assert at the beginning of redisplay that *all*
   scrollbars are to be removed, and then save scrollbars from the
   firey pit when we actually redisplay their window.  */

/* Arrange for all scrollbars on FRAME to be removed at the next call
   to `*judge_scrollbars_hook'.  A scrollbar may be spared if
   `*redeem_scrollbar_hook' is applied to it before the judgement.  */
extern void (*condemn_scrollbars_hook)( /* FRAME_PTR *FRAME */ );

/* Unmark BAR for deletion in this judgement cycle.  */
extern void (*redeem_scrollbar_hook)( /* struct scrollbar *BAR */ );

/* Remove all scrollbars on FRAME that haven't been saved since the
   last call to `*condemn_scrollbars_hook'.  */
extern void (*judge_scrollbars_hook)( /* FRAME_PTR *FRAME */ );



/* If nonzero, send all terminal output characters to this stream also.  */
extern FILE *termscript;

/* Expedient hack: only provide the below definitions to files that
   are prepared to handle lispy things.  XINT is defined iff lisp.h
   has been included before this file.  */
#ifdef XINT

/* The keyboard input buffer is an array of these structures.  Each one
   represents some sort of input event - a keystroke, a mouse click, or
   a window system event.  These get turned into their lispy forms when
   they are removed from the event queue.  */

struct input_event {

  /* What kind of event was this?  */
  enum {
    no_event,			/* nothing happened.  This should never
				   actually appear in the event queue.  */
    ascii_keystroke,		/* The ASCII code is in .code.
				   .frame is the frame in which the key
				   was typed.
				   Note that this includes meta-keys, and
				   the modifiers field of the event
				   is unused.
				   .timestamp gives a timestamp (in
				   milliseconds) for the keystroke.  */
    non_ascii_keystroke,	/* .code is a number identifying the
				   function key.  A code N represents
				   a key whose name is
				   function_key_names[N]; function_key_names
				   is a table in keyboard.c to which you
				   should feel free to add missing keys.
				   .modifiers holds the state of the
				   modifier keys.
				   .frame is the frame in which the key
				   was typed.
				   .timestamp gives a timestamp (in
				   milliseconds) for the keystroke.  */
    mouse_click,		/* The button number is in .code; it must
				   be >= 0 and < NUM_MOUSE_BUTTONS, defined
				   below.
				   .modifiers holds the state of the
				   modifier keys.
				   .x and .y give the mouse position,
				   in characters, within the window.
				   .frame gives the frame the mouse
				   click occurred in.
				   .timestamp gives a timestamp (in
				   milliseconds) for the click.  */

    scrollbar_click,		/* .code gives the number of the mouse button
				   that was clicked.
				   .modifiers holds the state of the modifier
				   keys.
				   .part is a lisp symbol indicating which
				   part of the scrollbar got clicked.
				   .scrollbar is a pointer to the scrollbar
				   clicked on.  Since redisplay may delete
				   scrollbars at any time, you may not assume
				   that this scrollbar still exists when you
				   dequeue this event.  You have to traverse
				   the window tree to make it's in a valid
				   window.
				   .x gives the distance from the start of the
				   scroll bar of the click; .y gives the total
				   length of the scroll bar.
				   .frame gives the frame the click should
				   apply to.
				   .timestamp gives a timestamp (in
				   milliseconds) for the click.  */
  } kind;
  
  Lisp_Object code;
  enum scrollbar_part part;
  struct scrollbar *scrollbar;

/* This is obviously wrong, but I'm not sure what else I should do.
   Obviously, this should be a FRAME_PTR.  But that would require that
   every file which #includes this one should also #include "frame.h",
   which would mean that files like cm.c and other innocents would be
   dragged into the set of frame.h users.  Maybe the definition of this
   structure should be elsewhere?  In its own file?  */
#ifdef MULTI_FRAME
  struct frame *frame;
#else
  int frame;
#endif
  int modifiers;		/* See enum below for interpretation.  */

  Lisp_Object x, y;
  unsigned long timestamp;
};

/* This is used in keyboard.c, to tell how many buttons we will need
   to track the positions of.  */
#define NUM_MOUSE_BUTTONS (5)

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
   bits into either drag_modifier or click_modifier events.  The
   click_modifier has no written representation in the names of the
   symbols used as event heads, but it does appear in the
   Qevent_symbol_components property of the event heads.  */
enum {
  up_modifier	=   1,		/* Only used on mouse buttons - always
				   turned into a click or a drag modifier
				   before lisp code sees the event.  */
  alt_modifier	=   2,		/* Under X, the XK_Alt_[LR] keysyms.  */
  ctrl_modifier	=   4,
  hyper_modifier=   8,		/* Under X, the XK_Hyper_[LR] keysyms.  */
  meta_modifier	=  16,		/* Under X, the XK_Meta_[LR] keysyms.  */
  shift_modifier=  32,
  super_modifier=  64,		/* Under X, the XK_Super_[LR] keysyms.  */
  down_modifier = 128,		/* Only used on mouse buttons.  */
  drag_modifier = 256,		/* This is never used in the event
				   queue; it's only used internally by
				   the window-system-independent code.  */
  click_modifier= 512,		/* See drag_modifier.  */
  last_modifier			/* This should always be one more than the
				   highest modifier bit defined.  */
};

#endif
