/* Definitions needed by most editing commands.
   Copyright (C) 1985, 1994 Free Software Foundation, Inc.

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


#define Ctl(c) ((c)&037)

/* Define the names of keymaps, just so people can refer to them in
   calls to initial_define_key.  These should *not* be used after
   initialization; use-global-map doesn't affect these; it sets
   current_global_map instead.  */
extern Lisp_Object global_map;
extern Lisp_Object meta_map;
extern Lisp_Object control_x_map;

extern Lisp_Object Vminibuffer_local_map;

extern Lisp_Object Vminibuffer_local_ns_map;

/* keymap used for minibuffers when doing completion */
extern Lisp_Object Vminibuffer_local_completion_map;

/* keymap used for minibuffers when doing completion and require a match */
extern Lisp_Object Vminibuffer_local_must_match_map;

/* Last character of last key sequence.  */
extern Lisp_Object last_command_char;

/* Last input character read as a command, not counting menus
   reached by the mouse.  */
extern Lisp_Object last_nonmenu_event;

/* List of command events to be re-read, or Qnil.  */
extern Lisp_Object Vunread_command_events;

/* Command char event to be re-read, or -1 if none.
   Setting this is obsolete, but some things should still check it.  */
extern int unread_command_char;

/* The command being executed by the command loop.
   Commands may set this, and the value set will be copied into
   current_kboard->Vlast_command instead of the actual command.  */
extern Lisp_Object this_command;

/* If not Qnil, this is a switch-frame event which we decided to put
   off until the end of a key sequence.  This should be read as the
   next command input, after any Vunread_command_events.

   read_key_sequence uses this to delay switch-frame events until the
   end of the key sequence; Fread_char uses it to put off switch-frame
   events until a non-ASCII event is acceptable as input.  */
extern Lisp_Object unread_switch_frame;

/* The value of point when the last command was executed.  */
extern int last_point_position;

/* The buffer that was current when the last command was started.  */
extern Lisp_Object last_point_position_buffer;

/* Nonzero means ^G can quit instantly */
extern int immediate_quit;

extern Lisp_Object Vexecuting_macro;

/* Nonzero if input is coming from the keyboard */

#define INTERACTIVE (NILP (Vexecuting_macro) && !noninteractive)

/* Set this nonzero to force reconsideration of mode line. */

extern int update_mode_lines;

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.  */

extern int cursor_in_echo_area;
