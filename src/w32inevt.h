/* Win32 input routines.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

#ifndef __NTINEVT_H__
#define __NTINEVT_H__

int win32_read_socket (/* int sd, struct input_event *bufp, int numchars,
			  int waitp, int expected */);
void win32_mouse_position (/* FRAME_PTR *f,
			      Lisp_Object *bar_window,
			      enum scroll_bar_part *part,
			      Lisp_Object *x,
			      Lisp_Object *y,
			      unsigned long *time */);

#endif
