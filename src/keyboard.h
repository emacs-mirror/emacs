/* Declarations useful when processing input.
   Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

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

/* Total number of times read_char has returned.  */
extern int num_input_chars;

/* Nonzero means polling for input is temporarily suppresed.  */
extern int poll_suppress_count;

/* Keymap mapping ASCII function key sequences onto their preferred forms.
   Initialized by the terminal-specific lisp files.  */
extern Lisp_Object Vfunction_key_map;


/* Macros for dealing with lispy events.  */

/* True iff EVENT has data fields describing it (i.e. a mouse click).  */
#define EVENT_HAS_PARAMETERS(event) \
  (XTYPE (event) == Lisp_Cons && Flength (event) == 5)

/* Access the components of composite events.  */
#define EVENT_HEAD(event) 		(Fnth (0, (event)))
#define EVENT_WINDOW(event)		(Fnth (1, (event)))
#define EVENT_BUFFER_POSN(event)	(Fnth (2, (event)))
#define EVENT_SCROLLBAR_BUTTON EVENT_BUFFER_POSN
#define EVENT_WINDOW_POSN(event)	(Fnth (3, (event)))
#define EVENT_TIMESTAMP(event)		(Fnth (4, (event)))

/* Some of the event heads.  */
extern Lisp_Object Qvscrollbar_part, Qvslider_part;
extern Lisp_Object Qvthumbup_part, Qvthumbdown_part;
extern Lisp_Object Qhscrollbar_part, Qhslider_part;
extern Lisp_Object Qhthumbleft_part, Qhthumbright_part;

/* Properties on event heads.  */
extern Lisp_Object Qevent_kind, Qevent_unmodified;

/* Getting an unmodified version of an event head.  */
#define EVENT_HEAD_UNMODIFIED(event_head) \
  (Fget ((event_head), Qevent_unmodified))

/* The values of Qevent_kind properties.  */
extern Lisp_Object Qfunction_key, Qmouse_click, Qmouse_movement;
extern Lisp_Object Qscrollbar_click;

/* Getting the kind of an event head.  */
#define EVENT_HEAD_KIND(event_head) \
  (Fget ((event_head), Qevent_kind))

/* Symbols to use for non-text mouse positions.  */
extern Lisp_Object Qmode_line, Qvertical_line;

