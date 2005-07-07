/* Definitions for keyboard macro interpretation in GNU Emacs.
   Copyright (C) 1985 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


/* Kbd macro currently being executed (a string or vector).  */

extern Lisp_Object Vexecuting_kbd_macro;

/* Index of next character to fetch from that macro.  */

extern EMACS_INT executing_kbd_macro_index;

/* Number of successful iterations so far
   for innermost keyboard macro.
   This is not bound at each level,
   so after an error, it describes the innermost interrupted macro.  */

extern int executing_kbd_macro_iterations;

/* This is the macro that was executing.
   This is not bound at each level,
   so after an error, it describes the innermost interrupted macro.  */

extern Lisp_Object executing_kbd_macro;

/* Finish defining the current keyboard macro.  */

extern void end_kbd_macro P_ ((void));

/* Declare that all chars stored so far in the kbd macro being defined
 really belong to it.  This is done in between editor commands.  */

extern void finalize_kbd_macro_chars P_ ((void));

/* Store a character into kbd macro being defined */

extern void store_kbd_macro_char P_ ((Lisp_Object));

/* arch-tag: 8edb7088-682f-4d1f-a4d9-0fbb7284234e
   (do not change this comment) */
