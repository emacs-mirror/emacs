/* terminal control module for terminals described by TERMCAP
   Copyright (C) 1985, 86, 87, 93, 94, 95 Free Software Foundation, Inc.

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


#include <config.h>
#include <stdio.h>
#include <ctype.h>
#include "termchar.h"
#include "termopts.h"
#include "cm.h"
#undef NULL
#include "lisp.h"
#include "frame.h"
#include "disptab.h"
#include "termhooks.h"
#include "keyboard.h"

extern Lisp_Object Fmake_sparse_keymap ();

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

#define OUTPUT(a) tputs (a, FRAME_HEIGHT (selected_frame) - curY, cmputc)
#define OUTPUT1(a) tputs (a, 1, cmputc)
#define OUTPUTL(a, lines) tputs (a, lines, cmputc)
#define OUTPUT_IF(a) { if (a) tputs (a, FRAME_HEIGHT (selected_frame) - curY, cmputc); }
#define OUTPUT1_IF(a) { if (a) tputs (a, 1, cmputc); }

/* Terminal characteristics that higher levels want to look at.
   These are all extern'd in termchar.h */

int must_write_spaces;		/* Nonzero means spaces in the text
				   must actually be output; can't just skip
				   over some columns to leave them blank.  */
int min_padding_speed;		/* Speed below which no padding necessary */

int line_ins_del_ok;		/* Terminal can insert and delete lines */
int char_ins_del_ok;		/* Terminal can insert and delete chars */
int scroll_region_ok;		/* Terminal supports setting the
				   scroll window */
int scroll_region_cost;		/* Cost of setting a scroll window,
				   measured in characters */
int memory_below_frame;		/* Terminal remembers lines
				   scrolled off bottom */
int fast_clear_end_of_line;	/* Terminal has a `ce' string */

/* Nonzero means no need to redraw the entire frame on resuming
   a suspended Emacs.  This is useful on terminals with multiple pages,
   where one page is used for Emacs and another for all else. */
int no_redraw_on_reenter;

/* Hook functions that you can set to snap out the functions in this file.
   These are all extern'd in termhooks.h  */

int (*cursor_to_hook) ();
int (*raw_cursor_to_hook) ();

int (*clear_to_end_hook) ();
int (*clear_frame_hook) ();
int (*clear_end_of_line_hook) ();

int (*ins_del_lines_hook) ();

int (*change_line_highlight_hook) ();
int (*reassert_line_highlight_hook) ();

int (*insert_glyphs_hook) ();
int (*write_glyphs_hook) ();
int (*delete_glyphs_hook) ();

int (*ring_bell_hook) ();

int (*reset_terminal_modes_hook) ();
int (*set_terminal_modes_hook) ();
int (*update_begin_hook) ();
int (*update_end_hook) ();
int (*set_terminal_window_hook) ();

int (*read_socket_hook) ();

int (*frame_up_to_date_hook) ();

/* Return the current position of the mouse.

   Set *f to the frame the mouse is in, or zero if the mouse is in no
   Emacs frame.  If it is set to zero, all the other arguments are
   garbage.

   If the motion started in a scroll bar, set *bar_window to the
   scroll bar's window, *part to the part the mouse is currently over,
   *x to the position of the mouse along the scroll bar, and *y to the
   overall length of the scroll bar.

   Otherwise, set *bar_window to Qnil, and *x and *y to the column and
   row of the character cell the mouse is over.

   Set *time to the time the mouse was at the returned position.

   This should clear mouse_moved until the next motion
   event arrives.  */
void (*mouse_position_hook) ( /* FRAME_PTR *f, int insist,
				 Lisp_Object *bar_window,
				 enum scroll_bar_part *part,
				 Lisp_Object *x,
				 Lisp_Object *y,
				 unsigned long *time */ );

/* When reading from a minibuffer in a different frame, Emacs wants
   to shift the highlight from the selected frame to the minibuffer's
   frame; under X, this means it lies about where the focus is.
   This hook tells the window system code to re-decide where to put
   the highlight.  */
void (*frame_rehighlight_hook) ( /* FRAME_PTR f */ );

/* If we're displaying frames using a window system that can stack
   frames on top of each other, this hook allows you to bring a frame
   to the front, or bury it behind all the other windows.  If this
   hook is zero, that means the device we're displaying on doesn't
   support overlapping frames, so there's no need to raise or lower
   anything.

   If RAISE is non-zero, F is brought to the front, before all other
   windows.  If RAISE is zero, F is sent to the back, behind all other
   windows.  */
void (*frame_raise_lower_hook) ( /* FRAME_PTR f, int raise */ );

/* Set the vertical scroll bar for WINDOW to have its upper left corner
   at (TOP, LEFT), and be LENGTH rows high.  Set its handle to
   indicate that we are displaying PORTION characters out of a total
   of WHOLE characters, starting at POSITION.  If WINDOW doesn't yet
   have a scroll bar, create one for it.  */
void (*set_vertical_scroll_bar_hook)
     ( /* struct window *window,
	  int portion, int whole, int position */ );


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - can you say set-window-configuration?
   Instead, we just assert at the beginning of redisplay that *all*
   scroll bars are to be removed, and then save scroll bars from the
   firey pit when we actually redisplay their window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgement. 

   This should be applied to each frame each time its window tree is
   redisplayed, even if it is not displaying scroll bars at the moment;
   if the HAS_SCROLL_BARS flag has just been turned off, only calling
   this and the judge_scroll_bars_hook will get rid of them.

   If non-zero, this hook should be safe to apply to any frame,
   whether or not it can support scroll bars, and whether or not it is
   currently displaying them.  */
void (*condemn_scroll_bars_hook)( /* FRAME_PTR *frame */ );

/* Unmark WINDOW's scroll bar for deletion in this judgement cycle.
   Note that it's okay to redeem a scroll bar that is not condemned.  */
void (*redeem_scroll_bar_hook)( /* struct window *window */ );

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  

   This should be applied to each frame after each time its window
   tree is redisplayed, even if it is not displaying scroll bars at the
   moment; if the HAS_SCROLL_BARS flag has just been turned off, only
   calling this and condemn_scroll_bars_hook will get rid of them.

   If non-zero, this hook should be safe to apply to any frame,
   whether or not it can support scroll bars, and whether or not it is
   currently displaying them.  */
void (*judge_scroll_bars_hook)( /* FRAME_PTR *FRAME */ );


/* Strings, numbers and flags taken from the termcap entry.  */

char *TS_ins_line;		/* termcap "al" */
char *TS_ins_multi_lines;	/* "AL" (one parameter, # lines to insert) */
char *TS_bell;			/* "bl" */
char *TS_clr_to_bottom;		/* "cd" */
char *TS_clr_line;		/* "ce", clear to end of line */
char *TS_clr_frame;		/* "cl" */
char *TS_set_scroll_region;	/* "cs" (2 params, first line and last line) */
char *TS_set_scroll_region_1;   /* "cS" (4 params: total lines,
				   lines above scroll region, lines below it,
				   total lines again) */
char *TS_del_char;		/* "dc" */
char *TS_del_multi_chars;	/* "DC" (one parameter, # chars to delete) */
char *TS_del_line;		/* "dl" */
char *TS_del_multi_lines;	/* "DL" (one parameter, # lines to delete) */
char *TS_delete_mode;		/* "dm", enter character-delete mode */
char *TS_end_delete_mode;	/* "ed", leave character-delete mode */
char *TS_end_insert_mode;	/* "ei", leave character-insert mode */
char *TS_ins_char;		/* "ic" */
char *TS_ins_multi_chars;	/* "IC" (one parameter, # chars to insert) */
char *TS_insert_mode;		/* "im", enter character-insert mode */
char *TS_pad_inserted_char;	/* "ip".  Just padding, no commands.  */
char *TS_end_keypad_mode;	/* "ke" */
char *TS_keypad_mode;		/* "ks" */
char *TS_pad_char;		/* "pc", char to use as padding */
char *TS_repeat;		/* "rp" (2 params, # times to repeat
				   and character to be repeated) */
char *TS_end_standout_mode;	/* "se" */
char *TS_fwd_scroll;		/* "sf" */
char *TS_standout_mode;		/* "so" */
char *TS_rev_scroll;		/* "sr" */
char *TS_end_termcap_modes;	/* "te" */
char *TS_termcap_modes;		/* "ti" */
char *TS_visible_bell;		/* "vb" */
char *TS_end_visual_mode;	/* "ve" */
char *TS_visual_mode;		/* "vi" */
char *TS_set_window;		/* "wi" (4 params, start and end of window,
				   each as vpos and hpos) */

int TF_hazeltine;	/* termcap hz flag. */
int TF_insmode_motion;	/* termcap mi flag: can move while in insert mode. */
int TF_standout_motion;	/* termcap mi flag: can move while in standout mode. */
int TF_underscore;	/* termcap ul flag: _ underlines if overstruck on
			   nonblank position.  Must clear before writing _.  */
int TF_teleray;		/* termcap xt flag: many weird consequences.
			   For t1061. */

int TF_xs;		/* Nonzero for "xs".  If set together with
			   TN_standout_width == 0, it means don't bother
			   to write any end-standout cookies.  */

int TN_standout_width;	/* termcap sg number: width occupied by standout
			   markers */

static int RPov;	/* # chars to start a TS_repeat */

static int delete_in_insert_mode;	/* delete mode == insert mode */

static int se_is_so;	/* 1 if same string both enters and leaves
			   standout mode */

/* internal state */

/* The largest frame width in any call to calculate_costs.  */
int max_frame_width;
/* The largest frame height in any call to calculate_costs.  */
int max_frame_height;

/* Number of chars of space used for standout marker at beginning of line,
   or'd with 0100.  Zero if no standout marker at all.
   The length of these vectors is max_frame_height.

   Used IFF TN_standout_width >= 0. */

static char *chars_wasted;
static char *copybuf;

/* nonzero means supposed to write text in standout mode.  */
int standout_requested;

int insert_mode;			/* Nonzero when in insert mode.  */
int standout_mode;			/* Nonzero when in standout mode.  */

/* Size of window specified by higher levels.
   This is the number of lines, from the top of frame downwards,
   which can participate in insert-line/delete-line operations.

   Effectively it excludes the bottom frame_height - specified_window_size
   lines from those operations.  */

int specified_window;

/* Frame currently being redisplayed; 0 if not currently redisplaying.
   (Direct output does not count).  */

FRAME_PTR updating_frame;

/* Provided for lisp packages.  */
static int system_uses_terminfo;

char *tparam ();

extern char *tgetstr ();


#ifdef WINDOWSNT
/* We aren't X windows, but we aren't termcap either.  This makes me
   uncertain as to what value to use for frame.output_method.  For
   this file, we'll define FRAME_TERMCAP_P to be zero so that our
   output hooks get called instead of the termcap functions.  Probably
   the best long-term solution is to define an output_windows_nt...  */

#undef FRAME_TERMCAP_P
#define FRAME_TERMCAP_P(_f_) 0
#endif /* WINDOWSNT */

ring_bell ()
{
  if (! FRAME_TERMCAP_P (selected_frame))
    {
      (*ring_bell_hook) ();
      return;
    }
  OUTPUT (TS_visible_bell && visible_bell ? TS_visible_bell : TS_bell);
}

set_terminal_modes ()
{
  if (! FRAME_TERMCAP_P (selected_frame))
    {
      (*set_terminal_modes_hook) ();
      return;
    }
  OUTPUT_IF (TS_termcap_modes);
  OUTPUT_IF (TS_visual_mode);
  OUTPUT_IF (TS_keypad_mode);
  losecursor ();
}

reset_terminal_modes ()
{
  if (! FRAME_TERMCAP_P (selected_frame))
    {
      (*reset_terminal_modes_hook) ();
      return;
    }
  if (TN_standout_width < 0)
    turn_off_highlight ();
  turn_off_insert ();
  OUTPUT_IF (TS_end_keypad_mode);
  OUTPUT_IF (TS_end_visual_mode);
  OUTPUT_IF (TS_end_termcap_modes);
  /* Output raw CR so kernel can track the cursor hpos.  */
  /* But on magic-cookie terminals this can erase an end-standout marker and
     cause the rest of the frame to be in standout, so move down first.  */
  if (TN_standout_width >= 0)
    cmputc ('\n');
  cmputc ('\r');
}

update_begin (f)
     FRAME_PTR f;
{
  updating_frame = f;
  if (! FRAME_TERMCAP_P (updating_frame))
    (*update_begin_hook) (f);
}

update_end (f)
     FRAME_PTR f;
{
  if (! FRAME_TERMCAP_P (updating_frame))
    {
      (*update_end_hook) (f);
      updating_frame = 0;
      return;
    }
  turn_off_insert ();
  background_highlight ();
  standout_requested = 0;
  updating_frame = 0;
}

set_terminal_window (size)
     int size;
{
  if (! FRAME_TERMCAP_P (updating_frame))
    {
      (*set_terminal_window_hook) (size);
      return;
    }
  specified_window = size ? size : FRAME_HEIGHT (selected_frame);
  if (!scroll_region_ok)
    return;
  set_scroll_region (0, specified_window);
}

set_scroll_region (start, stop)
     int start, stop;
{
  char *buf;
  if (TS_set_scroll_region)
    {
      buf = tparam (TS_set_scroll_region, 0, 0, start, stop - 1);
    }
  else if (TS_set_scroll_region_1)
    {
      buf = tparam (TS_set_scroll_region_1, 0, 0,
		    FRAME_HEIGHT (selected_frame), start,
		    FRAME_HEIGHT (selected_frame) - stop,
		    FRAME_HEIGHT (selected_frame));
    }
  else
    {
      buf = tparam (TS_set_window, 0, 0, start, 0, stop, FRAME_WIDTH (selected_frame));
    }
  OUTPUT (buf);
  xfree (buf);
  losecursor ();
}

turn_on_insert ()
{
  if (!insert_mode)
    OUTPUT (TS_insert_mode);
  insert_mode = 1;
}

turn_off_insert ()
{
  if (insert_mode)
    OUTPUT (TS_end_insert_mode);
  insert_mode = 0;
}

/* Handle highlighting when TN_standout_width (termcap sg) is not specified.
   In these terminals, output is affected by the value of standout
   mode when the output is written.

   These functions are called on all terminals, but do nothing
   on terminals whose standout mode does not work that way.  */

turn_off_highlight ()
{
  if (TN_standout_width < 0)
    {
      if (standout_mode)
	OUTPUT_IF (TS_end_standout_mode);
      standout_mode = 0;
    }
}

turn_on_highlight ()
{
  if (TN_standout_width < 0)
    {
      if (!standout_mode)
	OUTPUT_IF (TS_standout_mode);
      standout_mode = 1;
    }
}

/* Set standout mode to the state it should be in for
   empty space inside windows.  What this is,
   depends on the user option inverse-video.  */

background_highlight ()
{
  if (TN_standout_width >= 0)
    return;
  if (inverse_video)
    turn_on_highlight ();
  else
    turn_off_highlight ();
}

/* Set standout mode to the mode specified for the text to be output.  */

static
highlight_if_desired ()
{
  if (TN_standout_width >= 0)
    return;
  if (!inverse_video == !standout_requested)
    turn_off_highlight ();
  else
    turn_on_highlight ();
}

/* Handle standout mode for terminals in which TN_standout_width >= 0.
   On these terminals, standout is controlled by markers that
   live inside the terminal's memory.  TN_standout_width is the width
   that the marker occupies in memory.  Standout runs from the marker
   to the end of the line on some terminals, or to the next
   turn-off-standout marker (TS_end_standout_mode) string
   on other terminals.  */

/* Write a standout marker or end-standout marker at the front of the line
   at vertical position vpos.  */

write_standout_marker (flag, vpos)
     int flag, vpos;
{
  if (flag || (TS_end_standout_mode && !TF_teleray && !se_is_so
	       && !(TF_xs && TN_standout_width == 0)))
    {
      cmgoto (vpos, 0);
      cmplus (TN_standout_width);
      OUTPUT (flag ? TS_standout_mode : TS_end_standout_mode);
      chars_wasted[curY] = TN_standout_width | 0100;
    }
}

/* External interface to control of standout mode.
   Call this when about to modify line at position VPOS
   and not change whether it is highlighted.  */

reassert_line_highlight (highlight, vpos)
     int highlight;
     int vpos;
{
  if (! FRAME_TERMCAP_P ((updating_frame ? updating_frame : selected_frame)))
    {
      (*reassert_line_highlight_hook) (highlight, vpos);
      return;
    }
  if (TN_standout_width < 0)
    /* Handle terminals where standout takes affect at output time */
    standout_requested = highlight;
  else if (chars_wasted[vpos] == 0)
    /* For terminals with standout markers, write one on this line
       if there isn't one already.  */
    write_standout_marker (highlight, vpos);
}

/* Call this when about to modify line at position VPOS
   and change whether it is highlighted.  */

change_line_highlight (new_highlight, vpos, first_unused_hpos)
     int new_highlight, vpos, first_unused_hpos;
{
  standout_requested = new_highlight;
  if (! FRAME_TERMCAP_P (updating_frame))
    {
      (*change_line_highlight_hook) (new_highlight, vpos, first_unused_hpos);
      return;
    }

  cursor_to (vpos, 0);

  if (TN_standout_width < 0)
    background_highlight ();
  /* If line starts with a marker, delete the marker */
  else if (TS_clr_line && chars_wasted[curY])
    {
      turn_off_insert ();
      /* On Teleray, make sure to erase the SO marker.  */
      if (TF_teleray)
	{
	  cmgoto (curY - 1, FRAME_WIDTH (selected_frame) - 4);
	  OUTPUT ("\033S");
	  curY++;		/* ESC S moves to next line where the TS_standout_mode was */
	  curX = 0;
	}
      else
	cmgoto (curY, 0);	/* reposition to kill standout marker */
    }
  clear_end_of_line_raw (first_unused_hpos);
  reassert_line_highlight (new_highlight, curY);
}


/* Move to absolute position, specified origin 0 */

cursor_to (row, col)
     int row, col;
{
  if (! FRAME_TERMCAP_P ((updating_frame
			    ? updating_frame
			    : selected_frame))
      && cursor_to_hook)
    {
      (*cursor_to_hook) (row, col);
      return;
    }

  col += chars_wasted[row] & 077;
  if (curY == row && curX == col)
    return;
  if (!TF_standout_motion)
    background_highlight ();
  if (!TF_insmode_motion)
    turn_off_insert ();
  cmgoto (row, col);
}

/* Similar but don't take any account of the wasted characters.  */

raw_cursor_to (row, col)
     int row, col;
{
  if (! FRAME_TERMCAP_P ((updating_frame ? updating_frame : selected_frame)))
    {
      (*raw_cursor_to_hook) (row, col);
      return;
    }
  if (curY == row && curX == col)
    return;
  if (!TF_standout_motion)
    background_highlight ();
  if (!TF_insmode_motion)
    turn_off_insert ();
  cmgoto (row, col);
}

/* Erase operations */

/* clear from cursor to end of frame */
clear_to_end ()
{
  register int i;

  if (clear_to_end_hook && ! FRAME_TERMCAP_P (updating_frame))
    {
      (*clear_to_end_hook) ();
      return;
    }
  if (TS_clr_to_bottom)
    {
      background_highlight ();
      OUTPUT (TS_clr_to_bottom);
      bzero (chars_wasted + curY, FRAME_HEIGHT (selected_frame) - curY);
    }
  else
    {
      for (i = curY; i < FRAME_HEIGHT (selected_frame); i++)
	{
	  cursor_to (i, 0);
	  clear_end_of_line_raw (FRAME_WIDTH (selected_frame));
	}
    }
}

/* Clear entire frame */

clear_frame ()
{
  if (clear_frame_hook
      && ! FRAME_TERMCAP_P ((updating_frame ? updating_frame : selected_frame)))
    {
      (*clear_frame_hook) ();
      return;
    }
  if (TS_clr_frame)
    {
      background_highlight ();
      OUTPUT (TS_clr_frame);
      bzero (chars_wasted, FRAME_HEIGHT (selected_frame));
      cmat (0, 0);
    }
  else
    {
      cursor_to (0, 0);
      clear_to_end ();
    }
}

/* Clear to end of line, but do not clear any standout marker.
   Assumes that the cursor is positioned at a character of real text,
   which implies it cannot be before a standout marker
   unless the marker has zero width.

   Note that the cursor may be moved.  */

clear_end_of_line (first_unused_hpos)
     int first_unused_hpos;
{
  static GLYPH buf = SPACEGLYPH;
  if (FRAME_TERMCAP_P (selected_frame)
      && TN_standout_width == 0 && curX == 0 && chars_wasted[curY] != 0)
    write_glyphs (&buf, 1);
  clear_end_of_line_raw (first_unused_hpos);
}

/* Clear from cursor to end of line.
   Assume that the line is already clear starting at column first_unused_hpos.
   If the cursor is at a standout marker, erase the marker.

   Note that the cursor may be moved, on terminals lacking a `ce' string.  */

clear_end_of_line_raw (first_unused_hpos)
     int first_unused_hpos;
{
  register int i;

  if (clear_end_of_line_hook
      && ! FRAME_TERMCAP_P ((updating_frame
			       ? updating_frame
			       : selected_frame)))
    {
      (*clear_end_of_line_hook) (first_unused_hpos);
      return;
    }

  first_unused_hpos += chars_wasted[curY] & 077;
  if (curX >= first_unused_hpos)
    return;
  /* Notice if we are erasing a magic cookie */
  if (curX == 0)
    chars_wasted[curY] = 0;
  background_highlight ();
  if (TS_clr_line)
    {
      OUTPUT1 (TS_clr_line);
    }
  else
    {			/* have to do it the hard way */
      turn_off_insert ();

      /* Do not write in last row last col with Autowrap on. */
      if (AutoWrap && curY == FRAME_HEIGHT (selected_frame) - 1
	  && first_unused_hpos == FRAME_WIDTH (selected_frame))
	first_unused_hpos--;

      for (i = curX; i < first_unused_hpos; i++)
	{
	  if (termscript)
	    fputc (' ', termscript);
	  putchar (' ');
	}
      cmplus (first_unused_hpos - curX);
    }
}


write_glyphs (string, len)
     register GLYPH *string;
     register int len;
{
  register GLYPH g;
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;

  if (write_glyphs_hook
      && ! FRAME_TERMCAP_P ((updating_frame ? updating_frame : selected_frame)))
    {
      (*write_glyphs_hook) (string, len);
      return;
    }

  highlight_if_desired ();
  turn_off_insert ();

  /* Don't dare write in last column of bottom line, if AutoWrap,
     since that would scroll the whole frame on some terminals.  */

  if (AutoWrap
      && curY + 1 == FRAME_HEIGHT (selected_frame)
      && (curX + len - (chars_wasted[curY] & 077)
	  == FRAME_WIDTH (selected_frame)))
    len --;

  cmplus (len);
  while (--len >= 0)
    {
      g = *string++;
      /* Check quickly for G beyond length of table.
	 That implies it isn't an alias and is simple.  */
      if (g >= tlen)
	{
	simple:
	  putc (g & 0xff, stdout);
	  if (ferror (stdout))
	    clearerr (stdout);
	  if (termscript)
	    putc (g & 0xff, termscript);
	}
      else
	{
	  /* G has an entry in Vglyph_table,
	     so process any alias and then test for simpleness.  */
	  while (GLYPH_ALIAS_P (tbase, tlen, g))
	    g = GLYPH_ALIAS (tbase, g);
	  if (GLYPH_SIMPLE_P (tbase, tlen, g))
	    goto simple;
	  else
	    {
	      /* Here if G (or its definition as an alias) is not simple.  */
	      fwrite (GLYPH_STRING (tbase, g), 1, GLYPH_LENGTH (tbase, g),
		      stdout);
	      if (ferror (stdout))
		clearerr (stdout);
	      if (termscript)
		fwrite (GLYPH_STRING (tbase, g), 1, GLYPH_LENGTH (tbase, g),
			termscript);
	    }
	}
    }
  cmcheckmagic ();
}

/* If start is zero, insert blanks instead of a string at start */
 
insert_glyphs (start, len)
     register GLYPH *start;
     register int len;
{
  char *buf;
  register GLYPH g;
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;

  if (insert_glyphs_hook && ! FRAME_TERMCAP_P (updating_frame))
    {
      (*insert_glyphs_hook) (start, len);
      return;
    }
  highlight_if_desired ();

  if (TS_ins_multi_chars)
    {
      buf = tparam (TS_ins_multi_chars, 0, 0, len);
      OUTPUT1 (buf);
      xfree (buf);
      if (start)
	write_glyphs (start, len);
      return;
    }

  turn_on_insert ();
  cmplus (len);
  while (--len >= 0)
    {
      OUTPUT1_IF (TS_ins_char);
      if (!start)
	g = SPACEGLYPH;
      else
	g = *start++;

      if (GLYPH_SIMPLE_P (tbase, tlen, g))
	{
	  putc (g & 0xff, stdout);
	  if (ferror (stdout))
	    clearerr (stdout);
	  if (termscript)
	    putc (g & 0xff, termscript);
	}
      else
	{
	  fwrite (GLYPH_STRING (tbase, g), 1, GLYPH_LENGTH (tbase, g), stdout);
	  if (ferror (stdout))
	    clearerr (stdout);
	  if (termscript)
	    fwrite (GLYPH_STRING (tbase, g), 1, GLYPH_LENGTH (tbase, g),
		    termscript);
	}

      OUTPUT1_IF (TS_pad_inserted_char);
    }
  cmcheckmagic ();
}

delete_glyphs (n)
     register int n;
{
  char *buf;
  register int i;

  if (delete_glyphs_hook && ! FRAME_TERMCAP_P (updating_frame))
    {
      (*delete_glyphs_hook) (n);
      return;
    }

  if (delete_in_insert_mode)
    {
      turn_on_insert ();
    }
  else
    {
      turn_off_insert ();
      OUTPUT_IF (TS_delete_mode);
    }

  if (TS_del_multi_chars)
    {
      buf = tparam (TS_del_multi_chars, 0, 0, n);
      OUTPUT1 (buf);
      xfree (buf);
    }
  else
    for (i = 0; i < n; i++)
      OUTPUT1 (TS_del_char);
  if (!delete_in_insert_mode)
    OUTPUT_IF (TS_end_delete_mode);
}

/* Insert N lines at vpos VPOS.  If N is negative, delete -N lines.  */

ins_del_lines (vpos, n)
     int vpos, n;
{
  char *multi = n > 0 ? TS_ins_multi_lines : TS_del_multi_lines;
  char *single = n > 0 ? TS_ins_line : TS_del_line;
  char *scroll = n > 0 ? TS_rev_scroll : TS_fwd_scroll;

  register int i = n > 0 ? n : -n;
  register char *buf;

  if (ins_del_lines_hook && ! FRAME_TERMCAP_P (updating_frame))
    {
      (*ins_del_lines_hook) (vpos, n);
      return;
    }

  /* If the lines below the insertion are being pushed
     into the end of the window, this is the same as clearing;
     and we know the lines are already clear, since the matching
     deletion has already been done.  So can ignore this.  */
  /* If the lines below the deletion are blank lines coming
     out of the end of the window, don't bother,
     as there will be a matching inslines later that will flush them. */
  if (scroll_region_ok && vpos + i >= specified_window)
    return;
  if (!memory_below_frame && vpos + i >= FRAME_HEIGHT (selected_frame))
    return;

  if (multi)
    {
      raw_cursor_to (vpos, 0);
      background_highlight ();
      buf = tparam (multi, 0, 0, i);
      OUTPUT (buf);
      xfree (buf);
    }
  else if (single)
    {
      raw_cursor_to (vpos, 0);
      background_highlight ();
      while (--i >= 0)
	OUTPUT (single);
      if (TF_teleray)
	curX = 0;
    }
  else
    {
      set_scroll_region (vpos, specified_window);
      if (n < 0)
	raw_cursor_to (specified_window - 1, 0);
      else
	raw_cursor_to (vpos, 0);
      background_highlight ();
      while (--i >= 0)
	OUTPUTL (scroll, specified_window - vpos);
      set_scroll_region (0, specified_window);
    }

  if (TN_standout_width >= 0)
    {
      register lower_limit
	= (scroll_region_ok
	   ? specified_window
	   : FRAME_HEIGHT (selected_frame));

      if (n < 0)
	{
	  bcopy (&chars_wasted[vpos - n], &chars_wasted[vpos],
		 lower_limit - vpos + n);
	  bzero (&chars_wasted[lower_limit + n], - n);
	}
      else
	{
	  bcopy (&chars_wasted[vpos], &copybuf[vpos], lower_limit - vpos - n);
	  bcopy (&copybuf[vpos], &chars_wasted[vpos + n],
		 lower_limit - vpos - n);
	  bzero (&chars_wasted[vpos], n);
	}
    }
  if (!scroll_region_ok && memory_below_frame && n < 0)
    {
      cursor_to (FRAME_HEIGHT (selected_frame) + n, 0);
      clear_to_end ();
    }
}

/* Compute cost of sending "str", in characters,
   not counting any line-dependent padding.  */

int
string_cost (str)
     char *str;
{
  cost = 0;
  if (str)
    tputs (str, 0, evalcost);
  return cost;
}

/* Compute cost of sending "str", in characters,
   counting any line-dependent padding at one line.  */

static int
string_cost_one_line (str)
     char *str;
{
  cost = 0;
  if (str)
    tputs (str, 1, evalcost);
  return cost;
}

/* Compute per line amount of line-dependent padding,
   in tenths of characters.  */

int
per_line_cost (str)
     register char *str;
{
  cost = 0;
  if (str)
    tputs (str, 0, evalcost);
  cost = - cost;
  if (str)
    tputs (str, 10, evalcost);
  return cost;
}

#ifndef old
/* char_ins_del_cost[n] is cost of inserting N characters.
   char_ins_del_cost[-n] is cost of deleting N characters.
   The length of this vector is based on max_frame_width.  */

int *char_ins_del_vector;

#define char_ins_del_cost(f) (&char_ins_del_vector[FRAME_WIDTH ((f))])
#endif

/* ARGSUSED */
static void
calculate_ins_del_char_costs (frame)
     FRAME_PTR frame;
{
  int ins_startup_cost, del_startup_cost;
  int ins_cost_per_char, del_cost_per_char;
  register int i;
  register int *p;

  if (TS_ins_multi_chars)
    {
      ins_cost_per_char = 0;
      ins_startup_cost = string_cost_one_line (TS_ins_multi_chars);
    }
  else if (TS_ins_char || TS_pad_inserted_char
	   || (TS_insert_mode && TS_end_insert_mode))
    {
      ins_startup_cost = (30 * (string_cost (TS_insert_mode)
				+ string_cost (TS_end_insert_mode))) / 100;
      ins_cost_per_char = (string_cost_one_line (TS_ins_char)
			   + string_cost_one_line (TS_pad_inserted_char));
    }
  else
    {
      ins_startup_cost = 9999;
      ins_cost_per_char = 0;
    }

  if (TS_del_multi_chars)
    {
      del_cost_per_char = 0;
      del_startup_cost = string_cost_one_line (TS_del_multi_chars);
    }
  else if (TS_del_char)
    {
      del_startup_cost = (string_cost (TS_delete_mode)
			  + string_cost (TS_end_delete_mode));
      if (delete_in_insert_mode)
	del_startup_cost /= 2;
      del_cost_per_char = string_cost_one_line (TS_del_char);
    }
  else
    {
      del_startup_cost = 9999;
      del_cost_per_char = 0;
    }

  /* Delete costs are at negative offsets */
  p = &char_ins_del_cost (frame)[0];
  for (i = FRAME_WIDTH (selected_frame); --i >= 0;)
    *--p = (del_startup_cost += del_cost_per_char);

  /* Doing nothing is free */
  p = &char_ins_del_cost (frame)[0];
  *p++ = 0;

  /* Insert costs are at positive offsets */
  for (i = FRAME_WIDTH (frame); --i >= 0;)
    *p++ = (ins_startup_cost += ins_cost_per_char);
}

extern do_line_insertion_deletion_costs ();

calculate_costs (frame)
     FRAME_PTR frame;
{
  register char *f = (TS_set_scroll_region
		      ? TS_set_scroll_region
		      : TS_set_scroll_region_1);

  FRAME_COST_BAUD_RATE (frame) = baud_rate;

  scroll_region_cost = string_cost (f);
#ifdef HAVE_X_WINDOWS
  if (FRAME_X_P (frame))
    {
      do_line_insertion_deletion_costs (frame, 0, ".5*", 0, ".5*",
					0, 0,
					x_screen_planes (frame));
      scroll_region_cost = 0;
      return;
    }
#endif

  /* These variables are only used for terminal stuff.  They are allocated
     once for the terminal frame of X-windows emacs, but not used afterwards.

     char_ins_del_vector (i.e., char_ins_del_cost) isn't used because
     X turns off char_ins_del_ok.

     chars_wasted and copybuf are only used here in term.c in cases where
     the term hook isn't called. */

  max_frame_height = max (max_frame_height, FRAME_HEIGHT (frame));
  max_frame_width = max (max_frame_width, FRAME_WIDTH (frame));

  if (chars_wasted != 0)
    chars_wasted = (char *) xrealloc (chars_wasted, max_frame_height);
  else
    chars_wasted = (char *) xmalloc (max_frame_height);

  if (copybuf != 0)
    copybuf = (char *) xrealloc (copybuf, max_frame_height);
  else
    copybuf = (char *) xmalloc (max_frame_height);

  if (char_ins_del_vector != 0)
    char_ins_del_vector
      = (int *) xrealloc (char_ins_del_vector,
			  (sizeof (int)
			   + 2 * max_frame_width * sizeof (int)));
  else
    char_ins_del_vector
      = (int *) xmalloc (sizeof (int)
			 + 2 * max_frame_width * sizeof (int));

  bzero (chars_wasted, max_frame_height);
  bzero (copybuf, max_frame_height);
  bzero (char_ins_del_vector, (sizeof (int)
			       + 2 * max_frame_width * sizeof (int)));

  if (f && (!TS_ins_line && !TS_del_line))
    do_line_insertion_deletion_costs (frame,
				      TS_rev_scroll, TS_ins_multi_lines,
				      TS_fwd_scroll, TS_del_multi_lines,
				      f, f, 1);
  else
    do_line_insertion_deletion_costs (frame,
				      TS_ins_line, TS_ins_multi_lines,
				      TS_del_line, TS_del_multi_lines,
				      0, 0, 1);

  calculate_ins_del_char_costs (frame);

  /* Don't use TS_repeat if its padding is worse than sending the chars */
  if (TS_repeat && per_line_cost (TS_repeat) * baud_rate < 9000)
    RPov = string_cost (TS_repeat);
  else
    RPov = FRAME_WIDTH (frame) * 2;

  cmcostinit ();		/* set up cursor motion costs */
}

struct fkey_table {
  char *cap, *name;
};

  /* Termcap capability names that correspond directly to X keysyms.
     Some of these (marked "terminfo") aren't supplied by old-style
     (Berkeley) termcap entries.  They're listed in X keysym order;
     except we put the keypad keys first, so that if they clash with
     other keys (as on the IBM PC keyboard) they get overridden.
  */

static struct fkey_table keys[] = {
  "kh", "home",		/* termcap */
  "kl", "left",		/* termcap */
  "ku", "up",		/* termcap */
  "kr", "right",	/* termcap */
  "kd", "down",		/* termcap */
  "%8", "prior",	/* terminfo */
  "%5", "next",		/* terminfo */
  "@7",	"end",		/* terminfo */
  "@1", "begin",	/* terminfo */
  "*6", "select",	/* terminfo */
  "%9", "print",	/* terminfo */
  "@4", "execute",	/* terminfo --- actually the `command' key */
  /*
   * "insert" --- see below
   */
  "&8",	"undo",		/* terminfo */
  "%0",	"redo",		/* terminfo */
  "%7",	"menu",		/* terminfo --- actually the `options' key */
  "@0",	"find",		/* terminfo */
  "@2",	"cancel",	/* terminfo */
  "%1", "help",		/* terminfo */
  /*
   * "break" goes here, but can't be reliably intercepted with termcap
   */
  "&4", "reset",	/* terminfo --- actually `restart' */
  /*
   * "system" and "user" --- no termcaps
   */
  "kE", "clearline",	/* terminfo */
  "kA", "insertline",	/* terminfo */
  "kL", "deleteline",	/* terminfo */
  "kI", "insertchar",	/* terminfo */
  "kD", "deletechar",	/* terminfo */
  "kB", "backtab",	/* terminfo */
  /*
   * "kp_backtab", "kp-space", "kp-tab" --- no termcaps
   */
  "@8", "kp-enter",	/* terminfo */
  /*
   * "kp-f1", "kp-f2", "kp-f3" "kp-f4",
   * "kp-multiply", "kp-add", "kp-separator",
   * "kp-subtract", "kp-decimal", "kp-divide", "kp-0";
   * --- no termcaps for any of these.
   */
  "K4", "kp-1",		/* terminfo */
  /*
   * "kp-2" --- no termcap
   */
  "K5", "kp-3",		/* terminfo */
  /*
   * "kp-4" --- no termcap
   */
  "K2", "kp-5",		/* terminfo */
  /*
   * "kp-6" --- no termcap
   */
  "K1", "kp-7",		/* terminfo */
  /*
   * "kp-8" --- no termcap
   */
  "K3", "kp-9",		/* terminfo */
  /*
   * "kp-equal" --- no termcap
   */
  "k1",	"f1",
  "k2",	"f2",
  "k3",	"f3",
  "k4",	"f4",
  "k5",	"f5",
  "k6",	"f6",
  "k7",	"f7",
  "k8",	"f8",
  "k9",	"f9",
  };

static char **term_get_fkeys_arg;
static Lisp_Object term_get_fkeys_1 ();

/* Find the escape codes sent by the function keys for Vfunction_key_map.
   This function scans the termcap function key sequence entries, and 
   adds entries to Vfunction_key_map for each function key it finds.  */

void
term_get_fkeys (address)
     char **address;
{
  /* We run the body of the function (term_get_fkeys_1) and ignore all Lisp
     errors during the call.  The only errors should be from Fdefine_key
     when given a key sequence containing an invalid prefix key.  If the
     termcap defines function keys which use a prefix that is already bound
     to a command by the default bindings, we should silently ignore that
     function key specification, rather than giving the user an error and
     refusing to run at all on such a terminal.  */

  extern Lisp_Object Fidentity ();
  term_get_fkeys_arg = address;
  internal_condition_case (term_get_fkeys_1, Qerror, Fidentity);
}

static Lisp_Object
term_get_fkeys_1 ()
{
  int i;

  char **address = term_get_fkeys_arg;

  /* This can happen if CANNOT_DUMP or with strange options.  */
  if (!initialized)
    Vfunction_key_map = Fmake_sparse_keymap (Qnil);

  for (i = 0; i < (sizeof (keys)/sizeof (keys[0])); i++)
    {
      char *sequence = tgetstr (keys[i].cap, address);
      if (sequence)
	Fdefine_key (Vfunction_key_map, build_string (sequence),
		     Fmake_vector (make_number (1),
				   intern (keys[i].name)));
    }

  /* The uses of the "k0" capability are inconsistent; sometimes it
     describes F10, whereas othertimes it describes F0 and "k;" describes F10.
     We will attempt to politely accommodate both systems by testing for
     "k;", and if it is present, assuming that "k0" denotes F0, otherwise F10.
     */
  {
    char *k_semi  = tgetstr ("k;", address);
    char *k0      = tgetstr ("k0", address);
    char *k0_name = "f10";

    if (k_semi)
      {
	Fdefine_key (Vfunction_key_map, build_string (k_semi),
		     Fmake_vector (make_number (1), intern ("f10")));
	k0_name = "f0";
      }

    if (k0)
      Fdefine_key (Vfunction_key_map, build_string (k0),
		   Fmake_vector (make_number (1), intern (k0_name)));
  }

  /* Set up cookies for numbered function keys above f10. */
  {
    char fcap[3], fkey[4];

    fcap[0] = 'F'; fcap[2] = '\0';
    for (i = 11; i < 64; i++)
      {
	if (i <= 19)
	  fcap[1] = '1' + i - 11;
	else if (i <= 45)
	  fcap[1] = 'A' + i - 20;
	else
	  fcap[1] = 'a' + i - 46;

	{
	  char *sequence = tgetstr (fcap, address);
	  if (sequence)
	    {
	      sprintf (fkey, "f%d", i);
	      Fdefine_key (Vfunction_key_map, build_string (sequence),
			   Fmake_vector (make_number (1),
					 intern (fkey)));
	    }
	}
      }
   }

  /*
   * Various mappings to try and get a better fit.
   */
  {
#define CONDITIONAL_REASSIGN(cap1, cap2, sym)				\
      if (!tgetstr (cap1, address))					\
	{								\
	  char *sequence = tgetstr (cap2, address);			\
	  if (sequence)							\
	    Fdefine_key (Vfunction_key_map, build_string (sequence),	\
			 Fmake_vector (make_number (1),	\
				       intern (sym)));	\
	}
	  
      /* if there's no key_next keycap, map key_npage to `next' keysym */
      CONDITIONAL_REASSIGN ("%5", "kN", "next");
      /* if there's no key_prev keycap, map key_ppage to `previous' keysym */
      CONDITIONAL_REASSIGN ("%8", "kP", "prior");
      /* if there's no key_dc keycap, map key_ic to `insert' keysym */
      CONDITIONAL_REASSIGN ("kD", "kI", "insert");

      /* IBM has their own non-standard dialect of terminfo.
	 If the standard name isn't found, try the IBM name.  */
      CONDITIONAL_REASSIGN ("kB", "KO", "backtab");
      CONDITIONAL_REASSIGN ("@4", "kJ", "execute"); /* actually "action" */
      CONDITIONAL_REASSIGN ("@4", "kc", "execute"); /* actually "command" */
      CONDITIONAL_REASSIGN ("%7", "ki", "menu");
      CONDITIONAL_REASSIGN ("@7", "kw", "end");
      CONDITIONAL_REASSIGN ("F1", "k<", "f11");
      CONDITIONAL_REASSIGN ("F2", "k>", "f12");
      CONDITIONAL_REASSIGN ("%1", "kq", "help");
      CONDITIONAL_REASSIGN ("*6", "kU", "select");
#undef CONDITIONAL_REASSIGN
  }
}


term_init (terminal_type)
     char *terminal_type;
{
  char *area;
  char **address = &area;
  char buffer[2044];
  register char *p;
  int status;

#ifdef WINDOWSNT
  initialize_win_nt_display ();

  Wcm_clear ();

  area = (char *) malloc (2044);

  if (area == 0)
    abort ();

  FrameRows = FRAME_HEIGHT (selected_frame);
  FrameCols = FRAME_WIDTH (selected_frame);
  specified_window = FRAME_HEIGHT (selected_frame);

  delete_in_insert_mode = 1;

  UseTabs = 0;
  scroll_region_ok = 0;

  /* Seems to insert lines when it's not supposed to, messing
     up the display.  In doing a trace, it didn't seem to be
     called much, so I don't think we're losing anything by
     turning it off.  */

  line_ins_del_ok = 0;
  char_ins_del_ok = 1;

  baud_rate = 19200;

  FRAME_CAN_HAVE_SCROLL_BARS (selected_frame) = 0;
  FRAME_HAS_VERTICAL_SCROLL_BARS (selected_frame) = 0;

  return;
#endif /* WINDOWSNT */

  Wcm_clear ();

  status = tgetent (buffer, terminal_type);
  if (status < 0)
    {
#ifdef TERMINFO
      fatal ("Cannot open terminfo database file.\n");
#else
      fatal ("Cannot open termcap database file.\n");
#endif
    }
  if (status == 0)
    {
#ifdef TERMINFO
      fatal ("Terminal type %s is not defined.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMINFO' (C-shell: `unsetenv TERMINFO') as well.\n",
	     terminal_type);
#else
      fatal ("Terminal type %s is not defined.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMCAP' (C-shell: `unsetenv TERMCAP') as well.\n",
	     terminal_type);
#endif
    }
#ifdef TERMINFO
  area = (char *) malloc (2044);
#else
  area = (char *) malloc (strlen (buffer));
#endif /* not TERMINFO */
  if (area == 0)
    abort ();

  TS_ins_line = tgetstr ("al", address);
  TS_ins_multi_lines = tgetstr ("AL", address);
  TS_bell = tgetstr ("bl", address);
  BackTab = tgetstr ("bt", address);
  TS_clr_to_bottom = tgetstr ("cd", address);
  TS_clr_line = tgetstr ("ce", address);
  TS_clr_frame = tgetstr ("cl", address);
  ColPosition = tgetstr ("ch", address);
  AbsPosition = tgetstr ("cm", address);
  CR = tgetstr ("cr", address);
  TS_set_scroll_region = tgetstr ("cs", address);
  TS_set_scroll_region_1 = tgetstr ("cS", address);
  RowPosition = tgetstr ("cv", address);
  TS_del_char = tgetstr ("dc", address);
  TS_del_multi_chars = tgetstr ("DC", address);
  TS_del_line = tgetstr ("dl", address);
  TS_del_multi_lines = tgetstr ("DL", address);
  TS_delete_mode = tgetstr ("dm", address);
  TS_end_delete_mode = tgetstr ("ed", address);
  TS_end_insert_mode = tgetstr ("ei", address);
  Home = tgetstr ("ho", address);
  TS_ins_char = tgetstr ("ic", address);
  TS_ins_multi_chars = tgetstr ("IC", address);
  TS_insert_mode = tgetstr ("im", address);
  TS_pad_inserted_char = tgetstr ("ip", address);
  TS_end_keypad_mode = tgetstr ("ke", address);
  TS_keypad_mode = tgetstr ("ks", address);
  LastLine = tgetstr ("ll", address);
  Right = tgetstr ("nd", address);
  Down = tgetstr ("do", address);
  if (!Down)
    Down = tgetstr ("nl", address); /* Obsolete name for "do" */
#ifdef VMS
  /* VMS puts a carriage return before each linefeed,
     so it is not safe to use linefeeds.  */
  if (Down && Down[0] == '\n' && Down[1] == '\0')
    Down = 0;
#endif /* VMS */
  if (tgetflag ("bs"))
    Left = "\b";		  /* can't possibly be longer! */
  else				  /* (Actually, "bs" is obsolete...) */
    Left = tgetstr ("le", address);
  if (!Left)
    Left = tgetstr ("bc", address); /* Obsolete name for "le" */
  TS_pad_char = tgetstr ("pc", address);
  TS_repeat = tgetstr ("rp", address);
  TS_end_standout_mode = tgetstr ("se", address);
  TS_fwd_scroll = tgetstr ("sf", address);
  TS_standout_mode = tgetstr ("so", address);
  TS_rev_scroll = tgetstr ("sr", address);
  Wcm.cm_tab = tgetstr ("ta", address);
  TS_end_termcap_modes = tgetstr ("te", address);
  TS_termcap_modes = tgetstr ("ti", address);
  Up = tgetstr ("up", address);
  TS_visible_bell = tgetstr ("vb", address);
  TS_end_visual_mode = tgetstr ("ve", address);
  TS_visual_mode = tgetstr ("vs", address);
  TS_set_window = tgetstr ("wi", address);
  MultiUp = tgetstr ("UP", address);
  MultiDown = tgetstr ("DO", address);
  MultiLeft = tgetstr ("LE", address);
  MultiRight = tgetstr ("RI", address);

  MagicWrap = tgetflag ("xn");
  /* Since we make MagicWrap terminals look like AutoWrap, we need to have
     the former flag imply the latter.  */
  AutoWrap = MagicWrap || tgetflag ("am");
  memory_below_frame = tgetflag ("db");
  TF_hazeltine = tgetflag ("hz");
  must_write_spaces = tgetflag ("in");
  meta_key = tgetflag ("km") || tgetflag ("MT");
  TF_insmode_motion = tgetflag ("mi");
  TF_standout_motion = tgetflag ("ms");
  TF_underscore = tgetflag ("ul");
  TF_xs = tgetflag ("xs");
  TF_teleray = tgetflag ("xt");

  term_get_fkeys (address);

  /* Get frame size from system, or else from termcap.  */
  get_frame_size (&FRAME_WIDTH (selected_frame),
		   &FRAME_HEIGHT (selected_frame));
  if (FRAME_WIDTH (selected_frame) <= 0)
    FRAME_WIDTH (selected_frame) = tgetnum ("co");
  if (FRAME_HEIGHT (selected_frame) <= 0)
    FRAME_HEIGHT (selected_frame) = tgetnum ("li");

  if (FRAME_HEIGHT (selected_frame) < 3
      || FRAME_WIDTH (selected_frame) < 3)
    fatal ("Screen size %dx%d is too small.\n",
	   FRAME_HEIGHT (selected_frame), FRAME_WIDTH (selected_frame));

  min_padding_speed = tgetnum ("pb");
  TN_standout_width = tgetnum ("sg");
  TabWidth = tgetnum ("tw");

#ifdef VMS
  /* These capabilities commonly use ^J.
     I don't know why, but sending them on VMS does not work;
     it causes following spaces to be lost, sometimes.
     For now, the simplest fix is to avoid using these capabilities ever.  */
  if (Down && Down[0] == '\n')
    Down = 0;
#endif /* VMS */

  if (!TS_bell)
    TS_bell = "\07";

  if (!TS_fwd_scroll)
    TS_fwd_scroll = Down;

  PC = TS_pad_char ? *TS_pad_char : 0;

  if (TabWidth < 0)
    TabWidth = 8;
  
/* Turned off since /etc/termcap seems to have :ta= for most terminals
   and newer termcap doc does not seem to say there is a default.
  if (!Wcm.cm_tab)
    Wcm.cm_tab = "\t";
*/

  if (TS_standout_mode == 0)
    {
      TN_standout_width = tgetnum ("ug");
      TS_end_standout_mode = tgetstr ("ue", address);
      TS_standout_mode = tgetstr ("us", address);
    }

  /* If no `se' string, try using a `me' string instead.
     If that fails, we can't use standout mode at all.  */
  if (TS_end_standout_mode == 0)
    {
      char *s = tgetstr ("me", address);
      if (s != 0)
	TS_end_standout_mode = s;
      else
	TS_standout_mode = 0;
    }

  if (TF_teleray)
    {
      Wcm.cm_tab = 0;
      /* Teleray: most programs want a space in front of TS_standout_mode,
	   but Emacs can do without it (and give one extra column).  */
      TS_standout_mode = "\033RD";
      TN_standout_width = 1;
      /* But that means we cannot rely on ^M to go to column zero! */
      CR = 0;
      /* LF can't be trusted either -- can alter hpos */
      /* if move at column 0 thru a line with TS_standout_mode */
      Down = 0;
    }

  /* Special handling for certain terminal types known to need it */

  if (!strcmp (terminal_type, "supdup"))
    {
      memory_below_frame = 1;
      Wcm.cm_losewrap = 1;
    }
  if (!strncmp (terminal_type, "c10", 3)
      || !strcmp (terminal_type, "perq"))
    {
      /* Supply a makeshift :wi string.
	 This string is not valid in general since it works only
	 for windows starting at the upper left corner;
	 but that is all Emacs uses.

	 This string works only if the frame is using
	 the top of the video memory, because addressing is memory-relative.
	 So first check the :ti string to see if that is true.

	 It would be simpler if the :wi string could go in the termcap
	 entry, but it can't because it is not fully valid.
	 If it were in the termcap entry, it would confuse other programs.  */
      if (!TS_set_window)
	{
	  p = TS_termcap_modes;
	  while (*p && strcmp (p, "\033v  "))
	    p++;
	  if (*p)
	    TS_set_window = "\033v%C %C %C %C ";
	}
      /* Termcap entry often fails to have :in: flag */
      must_write_spaces = 1;
      /* :ti string typically fails to have \E^G! in it */
      /* This limits scope of insert-char to one line.  */
      strcpy (area, TS_termcap_modes);
      strcat (area, "\033\007!");
      TS_termcap_modes = area;
      area += strlen (area) + 1;
      p = AbsPosition;
      /* Change all %+ parameters to %C, to handle
	 values above 96 correctly for the C100.  */
      while (*p)
	{
	  if (p[0] == '%' && p[1] == '+')
	    p[1] = 'C';
	  p++;
	}
    }

  FrameRows = FRAME_HEIGHT (selected_frame);
  FrameCols = FRAME_WIDTH (selected_frame);
  specified_window = FRAME_HEIGHT (selected_frame);

  if (Wcm_init () == -1)	/* can't do cursor motion */
#ifdef VMS
    fatal ("Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have, use either the\n\
DCL command `SET TERMINAL/DEVICE= ...' for DEC-compatible terminals,\n\
or `define EMACS_TERM \"terminal type\"' for non-DEC terminals.\n",
           terminal_type);
#else
    fatal ("Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMCAP' (C-shell: `unsetenv TERMCAP') as well.\n",
	   terminal_type);
#endif
  if (FRAME_HEIGHT (selected_frame) <= 0
      || FRAME_WIDTH (selected_frame) <= 0)
    fatal ("The frame size has not been specified.");

  delete_in_insert_mode
    = TS_delete_mode && TS_insert_mode
      && !strcmp (TS_delete_mode, TS_insert_mode);

  se_is_so = (TS_standout_mode
	      && TS_end_standout_mode
	      && !strcmp (TS_standout_mode, TS_end_standout_mode));

  /* Remove width of standout marker from usable width of line */
  if (TN_standout_width > 0)
    FRAME_WIDTH (selected_frame) -= TN_standout_width;

  UseTabs = tabs_safe_p () && TabWidth == 8;

  scroll_region_ok
    = (Wcm.cm_abs
       && (TS_set_window || TS_set_scroll_region || TS_set_scroll_region_1));

  line_ins_del_ok = (((TS_ins_line || TS_ins_multi_lines)
		      && (TS_del_line || TS_del_multi_lines))
		     || (scroll_region_ok && TS_fwd_scroll && TS_rev_scroll));

  char_ins_del_ok = ((TS_ins_char || TS_insert_mode
		      || TS_pad_inserted_char || TS_ins_multi_chars)
		     && (TS_del_char || TS_del_multi_chars));

  fast_clear_end_of_line = TS_clr_line != 0;

  init_baud_rate ();
  if (read_socket_hook)		/* Baudrate is somewhat */
				/* meaningless in this case */
    baud_rate = 9600;

  FRAME_CAN_HAVE_SCROLL_BARS (selected_frame) = 0;
  FRAME_HAS_VERTICAL_SCROLL_BARS (selected_frame) = 0;
}

/* VARARGS 1 */
fatal (str, arg1, arg2)
     char *str, *arg1, *arg2;
{
  fprintf (stderr, "emacs: ");
  fprintf (stderr, str, arg1, arg2);
  fflush (stderr);
  exit (1);
}

syms_of_term ()
{
  DEFVAR_BOOL ("system-uses-terminfo", &system_uses_terminfo,
    "Non-nil means the system uses terminfo rather than termcap.\n\
This variable can be used by terminal emulator packages.");
#ifdef TERMINFO
  system_uses_terminfo = 1;
#else
  system_uses_terminfo = 0;
#endif
}
