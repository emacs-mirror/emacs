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

/* Each termcap frame points to its own struct tty_output object in the
   output_data.tty field.  The tty_output structure contains the information
   that is specific to terminals. */
struct tty_output
{
  char *name;                   /* The name of the device file or 0 if
                                   stdin/stdout. */
  char *type;                   /* The type of the tty. */
  
  /* Input/output */
  
  FILE *input;                  /* The stream to be used for terminal input. */
  FILE *output;                 /* The stream to be used for terminal output. */
  
  FILE *termscript;             /* If nonzero, send all terminal output
                                   characters to this stream also.  */

  struct emacs_tty old_tty;     /* The initial tty mode bits */

  int term_initted;             /* 1 if we have been through init_sys_modes. */
  int old_tty_valid;            /* 1 if outer tty status has been recorded.  */
  
  
  /* Pixel values.
     XXX What are these used for? */
  
  unsigned long background_pixel;
  unsigned long foreground_pixel;

  /* Terminal characteristics. */
  
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
  /* EMACS_INT baud_rate; */	/* Output speed in baud */
  int min_padding_speed;	/* Speed below which no padding necessary. */
  int dont_calculate_costs;     /* Nonzero means don't bother computing
                                   various cost tables; we won't use them. */
#endif

  struct tty_output *next;
};

extern struct tty_output *tty_list;


#define FRAME_TTY(f) \
  ((f)->output_method == output_termcap \
   ? (f)->output_data.tty : (abort(), (struct tty_output *) 0))
  
#define CURTTY() FRAME_TTY (SELECTED_FRAME())

#define TTY_NAME(t) ((t)->name)
#define TTY_TYPE(t) ((t)->type)

#define TTY_INPUT(t) ((t)->input)
#define TTY_OUTPUT(t) ((t)->output)
#define TTY_TERMSCRIPT(t) ((t)->termscript)

#define TTY_MUST_WRITE_SPACES(t) ((t)->must_write_spaces)
#define TTY_FAST_CLEAR_END_OF_LINE(t) ((t)->fast_clear_end_of_line)
#define TTY_LINE_INS_DEL_OK(t) ((t)->line_ins_del_ok)
#define TTY_CHAR_INS_DEL_OK(t) ((t)->char_ins_del_ok)
#define TTY_SCROLL_REGION_OK(t) ((t)->scroll_region_ok)
#define TTY_SCROLL_REGION_COST(t) ((t)->scroll_region_cost)
#define TTY_MEMORY_BELOW_FRAME(t) ((t)->memory_below_frame)

#if 0
/* These are not used anywhere. */
#define TTY_MIN_PADDING_SPEED(t) ((t)->min_padding_speed)
#define TTY_DONT_CALCULATE_COSTS(t) ((t)->dont_calculate_costs)
#endif

/* arch-tag: bf9f0d49-842b-42fb-9348-ec8759b27193
   (do not change this comment) */
