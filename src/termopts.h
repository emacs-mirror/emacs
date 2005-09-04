/* Flags and parameters describing user options for handling the terminal.
   Copyright (C) 1985, 1986, 1990, 2002, 2003, 2004,
                 2005 Free Software Foundation, Inc.

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


/* Nonzero means flash the screen instead of ringing the bell.  */
extern int visible_bell;

/* Nonzero means invert white and black for the entire screen.  */
extern int inverse_video;

/* Nonzero means use ^S/^Q as cretinous flow control.  */
extern int flow_control;

/* Nonzero means use interrupt-driven input.  */
extern int interrupt_input;

/* Nonzero while interrupts are temporarily deferred during redisplay.  */
extern int interrupts_deferred;

/* Terminal has meta key */
extern int meta_key;

/* Nonzero means truncate lines in all windows less wide than the frame */
extern int truncate_partial_width_windows;

/* Nonzero means no need to redraw the entire frame on resuming a suspended
   Emacs.  This is useful on terminals with multiple pages, where one page is
   used for Emacs and another for all else. */
extern int no_redraw_on_reenter;

/* arch-tag: 35d4d284-dc1a-4fff-97fa-0154a21aebdb
   (do not change this comment) */
