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

struct terminal
{
  
  /* EMACS_INT baud_rate; */	/* Output speed in baud */
  int must_write_spaces;	/* Nonzero means spaces in the text must
				   actually be output; can't just skip over
				   some columns to leave them blank.  */
  int fast_clear_end_of_line;   /* Nonzero means terminal has a `ce' string */
  
  int line_ins_del_ok;          /* Terminal can insert and delete lines */
  int char_ins_del_ok;          /* Terminal can insert and delete chars */
  int scroll_region_ok;         /* Terminal supports setting the scroll
                                   window */
  int scroll_region_cost;	/* Cost of setting the scroll window,
                                   measured in characters. */
  int memory_below_frame;	/* Terminal remembers lines scrolled
                                   off bottom */

#if 0  /* These are not used anywhere. */
  int min_padding_speed;	/* Speed below which no padding necessary. */
  int dont_calculate_costs;     /* Nonzero means don't bother computing
                                   various cost tables; we won't use them. */
#endif
};

typedef struct terminal *TERMINAL_PTR;

extern TERMINAL_PTR current_terminal;

#define CURRENT_TERMINAL() \
  (current_terminal ? current_terminal : (abort(), (TERMINAL_PTR) 0))

#define TERMINAL_MUST_WRITE_SPACES(t) ((t)->must_write_spaces)
#define TERMINAL_FAST_CLEAR_END_OF_LINE(t) ((t)->fast_clear_end_of_line)
#define TERMINAL_LINE_INS_DEL_OK(t) ((t)->line_ins_del_ok)
#define TERMINAL_CHAR_INS_DEL_OK(t) ((t)->char_ins_del_ok)
#define TERMINAL_SCROLL_REGION_OK(t) ((t)->scroll_region_ok)
#define TERMINAL_SCROLL_REGION_COST(t) ((t)->scroll_region_cost)
#define TERMINAL_MEMORY_BELOW_FRAME(t) ((t)->memory_below_frame)

#if 0
/* These are not used anywhere. */
#define TERMINAL_MIN_PADDING_SPEED(t) ((t)->min_padding_speed)
#define TERMINAL_DONT_CALCULATE_COSTS(t) ((t)->dont_calculate_costs)
#endif

/* arch-tag: bf9f0d49-842b-42fb-9348-ec8759b27193
   (do not change this comment) */
