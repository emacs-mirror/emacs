/* Flags and parameters describing terminal's characteristics.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


/* extern EMACS_INT baud_rate; */		/* Output speed in baud */
extern int must_write_spaces;	/* Nonzero means spaces in the text
				   must actually be output; can't just skip
				   over some columns to leave them blank.  */
extern int min_padding_speed;	/* Speed below which no padding necessary */
extern int fast_clear_end_of_line; /* Nonzero means terminal has
				      command for this */

extern int line_ins_del_ok;	/* Terminal can insert and delete lines */
extern int char_ins_del_ok;	/* Terminal can insert and delete chars */
extern int scroll_region_ok;	/* Terminal supports setting the scroll
				   window */
extern int scroll_region_cost;	/* Cost of setting the scroll window,
				   measured in characters */
extern int memory_below_frame;	/* Terminal remembers lines scrolled
				   off bottom */
extern int fast_clear_end_of_line; /* Terminal has a `ce' string */

extern int dont_calculate_costs; /* Nonzero means don't bother computing
				    various cost tables; we won't use them. */

/* Nonzero means no need to redraw the entire frame on resuming
   a suspended Emacs.  This is useful on terminals with multiple pages,
   where one page is used for Emacs and another for all else. */
extern int no_redraw_on_reenter;
