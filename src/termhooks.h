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
extern int (*clear_screen_hook) ();
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

/* Return the current position of the mouse.  This should clear
   mouse_moved until the next motion event arrives.  */
extern void (*mouse_position_hook) ( /* SCREEN_PTR *s,
					Lisp_Object *x,
					Lisp_Object *y,
					Lisp_Object *time */ );

/* The window system handling code should set this if the mouse has
   moved since the last call to the mouse_position_hook.  Calling that
   hook should clear this.  */
extern int mouse_moved;

/* When a screen's focus redirection is changed, this hook tells the
   window system code to re-decide where to put the highlight.  Under
   X, this means that Emacs lies about where the focus is.  */
extern void (*screen_rehighlight_hook) ( /* void */ );

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
				   .screen is the screen in which the key
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
				   .screen is the screen in which the key
				   was typed.
				   .timestamp gives a timestamp (in
				   milliseconds) for the keystroke.  */
    mouse_click,		/* The button number is in .code.
				   .modifiers holds the state of the
				   modifier keys.
				   .x and .y give the mouse position,
				   in pixels, within the window.
				   .screen gives the screen the mouse
				   click occurred in.
				   .timestamp gives a timestamp (in
				   milliseconds) for the click.  */
    scrollbar_click,		/* .code gives the number of the mouse
				   button that was clicked.
				   .part is a lisp symbol indicating which
				   part of the scrollbar got clicked.  This
				   indicates whether the scroll bar was
				   horizontal or vertical.
				   .modifiers gives the state of the
				   modifier keys.
				   .x gives the distance from the start
				   of the scroll bar of the click; .y gives
				   the total length of the scroll bar.
				   .screen gives the screen the click
				   should apply to.
				   .timestamp gives a timestamp (in
				   milliseconds) for the click.  */
#if 0
    screen_selected,		/* The user has moved the focus to another
				   screen.
				   .screen is the screen that should become
				   selected at the next convenient time.  */
#endif
  } kind;
  
  Lisp_Object code;
  Lisp_Object part;
  struct screen *screen;
  int modifiers;		/* See enum below for interpretation.  */
  Lisp_Object x, y;
  unsigned long timestamp;
};

/* Bits in the modifiers member of the input_event structure.  */
enum {
  shift_modifier = 1,
  ctrl_modifier = 2,
  meta_modifier = 4,
  up_modifier = 8,		/* This only applies to mouse buttons.  */
  last_modifier			/* This should always be one more than the
				   highest modifier bit defined.  */
};

#define NUM_MODIFIER_COMBOS ((last_modifier-1) << 1)

#endif
