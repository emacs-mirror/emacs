/* Terminal control module for terminals described by TERMCAP
   Copyright (C) 1985, 86, 87, 93, 94, 95, 98, 2000, 2001
   Free Software Foundation, Inc.

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

/* New redisplay, TTY faces by Gerd Moellmann <gerd@gnu.org>.  */

#include <config.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "termchar.h"
#include "termopts.h"
#include "lisp.h"
#include "charset.h"
#include "coding.h"
#include "keyboard.h"
#include "frame.h"
#include "disptab.h"
#include "termhooks.h"
#include "dispextern.h"
#include "window.h"
#include "keymap.h"

/* For now, don't try to include termcap.h.  On some systems,
   configure finds a non-standard termcap.h that the main build
   won't find.  */

#if defined HAVE_TERMCAP_H && 0
#include <termcap.h>
#else
extern void tputs P_ ((const char *, int, int (*)(int)));
extern int tgetent P_ ((char *, const char *));
extern int tgetflag P_ ((char *id));
extern int tgetnum P_ ((char *id));
#endif

#include "cm.h"
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif
#ifdef macintosh
#include "macterm.h"
#endif

static void turn_on_face P_ ((struct frame *, int face_id));
static void turn_off_face P_ ((struct frame *, int face_id));
static void tty_show_cursor P_ ((void));
static void tty_hide_cursor P_ ((void));

#define OUTPUT(a) \
     tputs (a, (int) (FRAME_HEIGHT (XFRAME (selected_frame)) - curY), cmputc)
#define OUTPUT1(a) tputs (a, 1, cmputc)
#define OUTPUTL(a, lines) tputs (a, lines, cmputc)

#define OUTPUT_IF(a)							\
     do {								\
       if (a)								\
         tputs (a, (int) (FRAME_HEIGHT (XFRAME (selected_frame))	\
			  - curY), cmputc);				\
     } while (0)
     
#define OUTPUT1_IF(a) do { if (a) tputs (a, 1, cmputc); } while (0)

/* Function to use to ring the bell.  */

Lisp_Object Vring_bell_function;

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

void (*cursor_to_hook) P_ ((int, int));
void (*raw_cursor_to_hook) P_ ((int, int));
void (*clear_to_end_hook) P_ ((void));
void (*clear_frame_hook) P_ ((void));
void (*clear_end_of_line_hook) P_ ((int));

void (*ins_del_lines_hook) P_ ((int, int));

void (*delete_glyphs_hook) P_ ((int));

void (*ring_bell_hook) P_ ((void));

void (*reset_terminal_modes_hook) P_ ((void));
void (*set_terminal_modes_hook) P_ ((void));
void (*update_begin_hook) P_ ((struct frame *));
void (*update_end_hook) P_ ((struct frame *));
void (*set_terminal_window_hook) P_ ((int));
void (*insert_glyphs_hook) P_ ((struct glyph *, int));
void (*write_glyphs_hook) P_ ((struct glyph *, int));
void (*delete_glyphs_hook) P_ ((int));

int (*read_socket_hook) P_ ((int, struct input_event *, int, int));

void (*frame_up_to_date_hook) P_ ((struct frame *));

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

void (*mouse_position_hook) P_ ((FRAME_PTR *f, int insist,
				 Lisp_Object *bar_window,
				 enum scroll_bar_part *part,
				 Lisp_Object *x,
				 Lisp_Object *y,
				 unsigned long *time));

/* When reading from a minibuffer in a different frame, Emacs wants
   to shift the highlight from the selected frame to the mini-buffer's
   frame; under X, this means it lies about where the focus is.
   This hook tells the window system code to re-decide where to put
   the highlight.  */

void (*frame_rehighlight_hook) P_ ((FRAME_PTR f));

/* If we're displaying frames using a window system that can stack
   frames on top of each other, this hook allows you to bring a frame
   to the front, or bury it behind all the other windows.  If this
   hook is zero, that means the device we're displaying on doesn't
   support overlapping frames, so there's no need to raise or lower
   anything.

   If RAISE is non-zero, F is brought to the front, before all other
   windows.  If RAISE is zero, F is sent to the back, behind all other
   windows.  */

void (*frame_raise_lower_hook) P_ ((FRAME_PTR f, int raise));

/* Set the vertical scroll bar for WINDOW to have its upper left corner
   at (TOP, LEFT), and be LENGTH rows high.  Set its handle to
   indicate that we are displaying PORTION characters out of a total
   of WHOLE characters, starting at POSITION.  If WINDOW doesn't yet
   have a scroll bar, create one for it.  */

void (*set_vertical_scroll_bar_hook)
     P_ ((struct window *window,
	  int portion, int whole, int position));


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - can you say set-window-configuration?
   Instead, we just assert at the beginning of redisplay that *all*
   scroll bars are to be removed, and then save scroll bars from the
   fiery pit when we actually redisplay their window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgment. 

   This should be applied to each frame each time its window tree is
   redisplayed, even if it is not displaying scroll bars at the moment;
   if the HAS_SCROLL_BARS flag has just been turned off, only calling
   this and the judge_scroll_bars_hook will get rid of them.

   If non-zero, this hook should be safe to apply to any frame,
   whether or not it can support scroll bars, and whether or not it is
   currently displaying them.  */

void (*condemn_scroll_bars_hook) P_ ((FRAME_PTR frame));

/* Unmark WINDOW's scroll bar for deletion in this judgement cycle.
   Note that it's okay to redeem a scroll bar that is not condemned.  */

void (*redeem_scroll_bar_hook) P_ ((struct window *window));

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  

   This should be applied to each frame after each time its window
   tree is redisplayed, even if it is not displaying scroll bars at the
   moment; if the HAS_SCROLL_BARS flag has just been turned off, only
   calling this and condemn_scroll_bars_hook will get rid of them.

   If non-zero, this hook should be safe to apply to any frame,
   whether or not it can support scroll bars, and whether or not it is
   currently displaying them.  */

void (*judge_scroll_bars_hook) P_ ((FRAME_PTR FRAME));

/* Hook to call in estimate_mode_line_height, if any.  */

int (* estimate_mode_line_height_hook) P_ ((struct frame *f, enum face_id));


/* Strings, numbers and flags taken from the termcap entry.  */

char *TS_ins_line;		/* "al" */
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
char *TS_cursor_normal;		/* "ve" */
char *TS_cursor_visible;	/* "vs" */
char *TS_cursor_invisible;	/* "vi" */
char *TS_set_window;		/* "wi" (4 params, start and end of window,
				   each as vpos and hpos) */

/* Value of the "NC" (no_color_video) capability, or 0 if not
   present.  */

static int TN_no_color_video;

/* Meaning of bits in no_color_video.  Each bit set means that the
   corresponding attribute cannot be combined with colors.  */

enum no_color_bit
{
  NC_STANDOUT	 = 1 << 0,
  NC_UNDERLINE	 = 1 << 1,
  NC_REVERSE	 = 1 << 2,
  NC_BLINK	 = 1 << 3,
  NC_DIM	 = 1 << 4,
  NC_BOLD	 = 1 << 5,
  NC_INVIS	 = 1 << 6,
  NC_PROTECT	 = 1 << 7,
  NC_ALT_CHARSET = 1 << 8
};

/* "md" -- turn on bold (extra bright mode).  */

char *TS_enter_bold_mode;

/* "mh" -- turn on half-bright mode.  */

char *TS_enter_dim_mode;

/* "mb" -- enter blinking mode.  */

char *TS_enter_blink_mode;

/* "mr" -- enter reverse video mode.  */

char *TS_enter_reverse_mode;

/* "us"/"ue" -- start/end underlining.  */

char *TS_exit_underline_mode, *TS_enter_underline_mode;

/* "as"/"ae" -- start/end alternate character set.  Not really
   supported, yet.  */

char *TS_enter_alt_charset_mode, *TS_exit_alt_charset_mode;

/* "me" -- switch appearances off.  */

char *TS_exit_attribute_mode;

/* "Co" -- number of colors.  */

int TN_max_colors;

/* "pa" -- max. number of color pairs on screen.  Not handled yet.
   Could be a problem if not equal to TN_max_colors * TN_max_colors.  */

int TN_max_pairs;

/* "op" -- SVr4 set default pair to its original value.  */

char *TS_orig_pair;

/* "AF"/"AB" or "Sf"/"Sb"-- set ANSI or SVr4 foreground/background color.
   1 param, the color index.  */

char *TS_set_foreground, *TS_set_background;

int TF_hazeltine;	/* termcap hz flag. */
int TF_insmode_motion;	/* termcap mi flag: can move while in insert mode. */
int TF_standout_motion;	/* termcap mi flag: can move while in standout mode. */
int TF_underscore;	/* termcap ul flag: _ underlines if over-struck on
			   non-blank position.  Must clear before writing _.  */
int TF_teleray;		/* termcap xt flag: many weird consequences.
			   For t1061. */

static int RPov;	/* # chars to start a TS_repeat */

static int delete_in_insert_mode;	/* delete mode == insert mode */

static int se_is_so;	/* 1 if same string both enters and leaves
			   standout mode */

/* internal state */

/* The largest frame width in any call to calculate_costs.  */

int max_frame_width;

/* The largest frame height in any call to calculate_costs.  */

int max_frame_height;

int costs_set = 0;		/* Nonzero if costs have been calculated. */

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

/* Flag used in tty_show/hide_cursor.  */

static int tty_cursor_hidden;

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

void
ring_bell ()
{
  if (!NILP (Vring_bell_function))
    {
      Lisp_Object function;

      /* Temporarily set the global variable to nil
	 so that if we get an error, it stays nil
	 and we don't call it over and over.

	 We don't specbind it, because that would carefully
	 restore the bad value if there's an error
	 and make the loop of errors happen anyway.  */
      
      function = Vring_bell_function;
      Vring_bell_function = Qnil;

      call0 (function);

      Vring_bell_function = function;
    }
  else if (!FRAME_TERMCAP_P (XFRAME (selected_frame)))
    (*ring_bell_hook) ();
  else
    OUTPUT (TS_visible_bell && visible_bell ? TS_visible_bell : TS_bell);
}

void
set_terminal_modes ()
{
  if (FRAME_TERMCAP_P (XFRAME (selected_frame)))
    {
      OUTPUT_IF (TS_termcap_modes);
      OUTPUT_IF (TS_cursor_visible);
      OUTPUT_IF (TS_keypad_mode);
      losecursor ();
    }
  else
    (*set_terminal_modes_hook) ();
}

void
reset_terminal_modes ()
{
  if (FRAME_TERMCAP_P (XFRAME (selected_frame)))
    {
      turn_off_highlight ();
      turn_off_insert ();
      OUTPUT_IF (TS_end_keypad_mode);
      OUTPUT_IF (TS_cursor_normal);
      OUTPUT_IF (TS_end_termcap_modes);
      OUTPUT_IF (TS_orig_pair);
      /* Output raw CR so kernel can track the cursor hpos.  */
      cmputc ('\r');
    }
  else if (reset_terminal_modes_hook)
    (*reset_terminal_modes_hook) ();
}

void
update_begin (f)
     struct frame *f;
{
  updating_frame = f;
  if (!FRAME_TERMCAP_P (f))
    update_begin_hook (f);
}

void
update_end (f)
     struct frame *f;
{
  if (FRAME_TERMCAP_P (f))
    {
      if (!XWINDOW (selected_window)->cursor_off_p)
	tty_show_cursor ();
      turn_off_insert ();
      background_highlight ();
    }
  else
    update_end_hook (f);
  
  updating_frame = NULL;
}

void
set_terminal_window (size)
     int size;
{
  if (FRAME_TERMCAP_P (updating_frame))
    {
      specified_window = size ? size : FRAME_HEIGHT (updating_frame);
      if (scroll_region_ok)
	set_scroll_region (0, specified_window);
    }
  else
    set_terminal_window_hook (size);
}

void
set_scroll_region (start, stop)
     int start, stop;
{
  char *buf;
  struct frame *sf = XFRAME (selected_frame);
  
  if (TS_set_scroll_region)
    buf = tparam (TS_set_scroll_region, 0, 0, start, stop - 1);
  else if (TS_set_scroll_region_1)
    buf = tparam (TS_set_scroll_region_1, 0, 0,
		  FRAME_HEIGHT (sf), start,
		  FRAME_HEIGHT (sf) - stop,
		  FRAME_HEIGHT (sf));
  else
    buf = tparam (TS_set_window, 0, 0, start, 0, stop, FRAME_WIDTH (sf));
  
  OUTPUT (buf);
  xfree (buf);
  losecursor ();
}


static void
turn_on_insert ()
{
  if (!insert_mode)
    OUTPUT (TS_insert_mode);
  insert_mode = 1;
}

void
turn_off_insert ()
{
  if (insert_mode)
    OUTPUT (TS_end_insert_mode);
  insert_mode = 0;
}

/* Handle highlighting.  */

void
turn_off_highlight ()
{
  if (standout_mode)
    OUTPUT_IF (TS_end_standout_mode);
  standout_mode = 0;
}

static void
turn_on_highlight ()
{
  if (!standout_mode)
    OUTPUT_IF (TS_standout_mode);
  standout_mode = 1;
}

static void
toggle_highlight ()
{
  if (standout_mode)
    turn_off_highlight ();
  else
    turn_on_highlight ();
}


/* Make cursor invisible.  */

static void
tty_hide_cursor ()
{
  if (tty_cursor_hidden == 0)
    {
      tty_cursor_hidden = 1;
      OUTPUT_IF (TS_cursor_invisible);
    }
}


/* Ensure that cursor is visible.  */

static void
tty_show_cursor ()
{
  if (tty_cursor_hidden)
    {
      tty_cursor_hidden = 0;
      OUTPUT_IF (TS_cursor_normal);
      OUTPUT_IF (TS_cursor_visible);
    }
}


/* Set standout mode to the state it should be in for
   empty space inside windows.  What this is,
   depends on the user option inverse-video.  */

void
background_highlight ()
{
  if (inverse_video)
    turn_on_highlight ();
  else
    turn_off_highlight ();
}

/* Set standout mode to the mode specified for the text to be output.  */

static void
highlight_if_desired ()
{
  if (inverse_video)
    turn_on_highlight ();
  else
    turn_off_highlight ();
}


/* Move cursor to row/column position VPOS/HPOS.  HPOS/VPOS are
   frame-relative coordinates.  */

void
cursor_to (vpos, hpos)
     int vpos, hpos;
{
  struct frame *f = updating_frame ? updating_frame : XFRAME (selected_frame);
  
  if (! FRAME_TERMCAP_P (f) && cursor_to_hook)
    {
      (*cursor_to_hook) (vpos, hpos);
      return;
    }

  /* Detect the case where we are called from reset_sys_modes
     and the costs have never been calculated.  Do nothing.  */
  if (! costs_set)
    return;

  if (curY == vpos && curX == hpos)
    return;
  if (!TF_standout_motion)
    background_highlight ();
  if (!TF_insmode_motion)
    turn_off_insert ();
  cmgoto (vpos, hpos);
}

/* Similar but don't take any account of the wasted characters.  */

void
raw_cursor_to (row, col)
     int row, col;
{
  struct frame *f = updating_frame ? updating_frame : XFRAME (selected_frame);
  if (! FRAME_TERMCAP_P (f))
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
void
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
    }
  else
    {
      for (i = curY; i < FRAME_HEIGHT (XFRAME (selected_frame)); i++)
	{
	  cursor_to (i, 0);
	  clear_end_of_line (FRAME_WIDTH (XFRAME (selected_frame)));
	}
    }
}

/* Clear entire frame */

void
clear_frame ()
{
  struct frame *sf = XFRAME (selected_frame);
  
  if (clear_frame_hook
      && ! FRAME_TERMCAP_P ((updating_frame ? updating_frame : sf)))
    {
      (*clear_frame_hook) ();
      return;
    }
  if (TS_clr_frame)
    {
      background_highlight ();
      OUTPUT (TS_clr_frame);
      cmat (0, 0);
    }
  else
    {
      cursor_to (0, 0);
      clear_to_end ();
    }
}

/* Clear from cursor to end of line.
   Assume that the line is already clear starting at column first_unused_hpos.

   Note that the cursor may be moved, on terminals lacking a `ce' string.  */

void
clear_end_of_line (first_unused_hpos)
     int first_unused_hpos;
{
  register int i;

  if (clear_end_of_line_hook
      && ! FRAME_TERMCAP_P ((updating_frame
			       ? updating_frame
			     : XFRAME (selected_frame))))
    {
      (*clear_end_of_line_hook) (first_unused_hpos);
      return;
    }

  /* Detect the case where we are called from reset_sys_modes
     and the costs have never been calculated.  Do nothing.  */
  if (! costs_set)
    return;

  if (curX >= first_unused_hpos)
    return;
  background_highlight ();
  if (TS_clr_line)
    {
      OUTPUT1 (TS_clr_line);
    }
  else
    {			/* have to do it the hard way */
      struct frame *sf = XFRAME (selected_frame);
      turn_off_insert ();

      /* Do not write in last row last col with Auto-wrap on. */
      if (AutoWrap && curY == FRAME_HEIGHT (sf) - 1
	  && first_unused_hpos == FRAME_WIDTH (sf))
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

/* Encode SRC_LEN glyphs starting at SRC to terminal output codes and
   store them at DST.  Do not write more than DST_LEN bytes.  That may
   require stopping before all SRC_LEN input glyphs have been
   converted.

   We store the number of glyphs actually converted in *CONSUMED.  The
   return value is the number of bytes store in DST.  */

int
encode_terminal_code (src, dst, src_len, dst_len, consumed)
     struct glyph *src;
     int src_len;
     unsigned char *dst;
     int dst_len, *consumed;
{
  struct glyph *src_start = src, *src_end = src + src_len;
  unsigned char *dst_start = dst, *dst_end = dst + dst_len;
  register GLYPH g;
  unsigned char workbuf[MAX_MULTIBYTE_LENGTH], *buf;
  int len;
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;
  int result;
  struct coding_system *coding;

  /* If terminal_coding does any conversion, use it, otherwise use
     safe_terminal_coding.  We can't use CODING_REQUIRE_ENCODING here
     because it always return 1 if the member src_multibyte is 1.  */
  coding = (terminal_coding.common_flags & CODING_REQUIRE_ENCODING_MASK
	    ? &terminal_coding
	    : &safe_terminal_coding);

  while (src < src_end)
    {
      /* We must skip glyphs to be padded for a wide character.  */
      if (! CHAR_GLYPH_PADDING_P (*src))
	{
	  g = GLYPH_FROM_CHAR_GLYPH (src[0]);

	  if (g < 0 || g >= tlen)
	    {
	      /* This glyph doesn't has an entry in Vglyph_table.  */
	      if (! CHAR_VALID_P (src->u.ch, 0))
		{
		  len = 1;
		  buf = " ";
		  coding->src_multibyte = 0;
		}
	      else
		{
		  len = CHAR_STRING (src->u.ch, workbuf);
		  buf = workbuf;
		  coding->src_multibyte = 1;
		}
	    }
	  else
	    {
	      /* This glyph has an entry in Vglyph_table,
		 so process any alias before testing for simpleness.  */
	      GLYPH_FOLLOW_ALIASES (tbase, tlen, g);

	      if (GLYPH_SIMPLE_P (tbase, tlen, g))
		{
		  /* We set the multi-byte form of a character in G
		     (that should be an ASCII character) at
		     WORKBUF.  */
		  workbuf[0] = FAST_GLYPH_CHAR (g);
		  len = 1;
		  buf = workbuf;
		  coding->src_multibyte = 0;
		}
	      else
		{
		  /* We have a string in Vglyph_table.  */
		  len = GLYPH_LENGTH (tbase, g);
		  buf = GLYPH_STRING (tbase, g);
		  coding->src_multibyte = STRING_MULTIBYTE (tbase[g]);
		}
	    }
	  
	  result = encode_coding (coding, buf, dst, len, dst_end - dst);
	  len -= coding->consumed;
	  dst += coding->produced;
	  if (result == CODING_FINISH_INSUFFICIENT_DST
	      || (result == CODING_FINISH_INSUFFICIENT_SRC
		  && len > dst_end - dst))
	    /* The remaining output buffer is too short.  We must
	       break the loop here without increasing SRC so that the
	       next call of this function starts from the same glyph.  */
	    break;

	  if (len > 0)
	    {
	      /* This is the case that a code of the range 0200..0237
		 exists in buf.  We must just write out such a code.  */
	      buf += coding->consumed;
	      while (len--)
		*dst++ = *buf++;
	    }
	}
      src++;
    }
  
  *consumed = src - src_start;
  return (dst - dst_start);
}


void
write_glyphs (string, len)
     register struct glyph *string;
     register int len;
{
  int produced, consumed;
  struct frame *sf = XFRAME (selected_frame);
  struct frame *f = updating_frame ? updating_frame : sf;
  unsigned char conversion_buffer[1024];
  int conversion_buffer_size = sizeof conversion_buffer;

  if (write_glyphs_hook
      && ! FRAME_TERMCAP_P (f))
    {
      (*write_glyphs_hook) (string, len);
      return;
    }

  turn_off_insert ();
  tty_hide_cursor ();

  /* Don't dare write in last column of bottom line, if Auto-Wrap,
     since that would scroll the whole frame on some terminals.  */

  if (AutoWrap
      && curY + 1 == FRAME_HEIGHT (sf)
      && (curX + len) == FRAME_WIDTH (sf))
    len --;
  if (len <= 0)
    return;

  cmplus (len);
  
  /* The mode bit CODING_MODE_LAST_BLOCK should be set to 1 only at
     the tail.  */
  terminal_coding.mode &= ~CODING_MODE_LAST_BLOCK;
  
  while (len > 0)
    {
      /* Identify a run of glyphs with the same face.  */
      int face_id = string->face_id;
      int n;
      
      for (n = 1; n < len; ++n)
	if (string[n].face_id != face_id)
	  break;

      /* Turn appearance modes of the face of the run on.  */
      highlight_if_desired ();
      turn_on_face (f, face_id);

      while (n > 0)
	{
	  /* We use a fixed size (1024 bytes) of conversion buffer.
	     Usually it is sufficient, but if not, we just repeat the
	     loop.  */
	  produced = encode_terminal_code (string, conversion_buffer,
					   n, conversion_buffer_size,
					   &consumed);
	  if (produced > 0)
	    {
	      fwrite (conversion_buffer, 1, produced, stdout);
	      if (ferror (stdout))
		clearerr (stdout);
	      if (termscript)
		fwrite (conversion_buffer, 1, produced, termscript);
	    }
	  len -= consumed;
	  n -= consumed;
	  string += consumed;
	}

      /* Turn appearance modes off.  */
      turn_off_face (f, face_id);
      turn_off_highlight ();
    }
  
  /* We may have to output some codes to terminate the writing.  */
  if (CODING_REQUIRE_FLUSHING (&terminal_coding))
    {
      terminal_coding.mode |= CODING_MODE_LAST_BLOCK;
      encode_coding (&terminal_coding, "", conversion_buffer,
		     0, conversion_buffer_size);
      if (terminal_coding.produced > 0)
	{
	  fwrite (conversion_buffer, 1, terminal_coding.produced, stdout);
	  if (ferror (stdout))
	    clearerr (stdout);
	  if (termscript)
	    fwrite (conversion_buffer, 1, terminal_coding.produced,
		    termscript);
	}
    }
  
  cmcheckmagic ();
}

/* If start is zero, insert blanks instead of a string at start */
 
void
insert_glyphs (start, len)
     register struct glyph *start;
     register int len;
{
  char *buf;
  struct glyph *glyph = NULL;
  struct frame *f, *sf;

  if (len <= 0)
    return;

  if (insert_glyphs_hook)
    {
      (*insert_glyphs_hook) (start, len);
      return;
    }

  sf = XFRAME (selected_frame);
  f = updating_frame ? updating_frame : sf;

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
  /* The bit CODING_MODE_LAST_BLOCK should be set to 1 only at the tail.  */
  terminal_coding.mode &= ~CODING_MODE_LAST_BLOCK;
  while (len-- > 0)
    {
      int produced, consumed;
      unsigned char conversion_buffer[1024];
      int conversion_buffer_size = sizeof conversion_buffer;

      OUTPUT1_IF (TS_ins_char);
      if (!start)
	{
	  conversion_buffer[0] = SPACEGLYPH;
	  produced = 1;
	}
      else
	{
	  highlight_if_desired ();
	  turn_on_face (f, start->face_id);
	  glyph = start;
	  ++start;
	  /* We must open sufficient space for a character which
	     occupies more than one column.  */
	  while (len && CHAR_GLYPH_PADDING_P (*start))
	    {
	      OUTPUT1_IF (TS_ins_char);
	      start++, len--;
	    }

	  if (len <= 0)
	    /* This is the last glyph.  */
	    terminal_coding.mode |= CODING_MODE_LAST_BLOCK;

	  /* The size of conversion buffer (1024 bytes) is surely
	     sufficient for just one glyph.  */
	  produced = encode_terminal_code (glyph, conversion_buffer, 1,
					   conversion_buffer_size, &consumed);
	}

      if (produced > 0)
	{
	  fwrite (conversion_buffer, 1, produced, stdout);
	  if (ferror (stdout))
	    clearerr (stdout);
	  if (termscript)
	    fwrite (conversion_buffer, 1, produced, termscript);
	}

      OUTPUT1_IF (TS_pad_inserted_char);
      if (start)
	{
	  turn_off_face (f, glyph->face_id);
	  turn_off_highlight ();
	}
    }
  
  cmcheckmagic ();
}

void
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

void
ins_del_lines (vpos, n)
     int vpos, n;
{
  char *multi = n > 0 ? TS_ins_multi_lines : TS_del_multi_lines;
  char *single = n > 0 ? TS_ins_line : TS_del_line;
  char *scroll = n > 0 ? TS_rev_scroll : TS_fwd_scroll;
  struct frame *sf;

  register int i = n > 0 ? n : -n;
  register char *buf;

  if (ins_del_lines_hook && ! FRAME_TERMCAP_P (updating_frame))
    {
      (*ins_del_lines_hook) (vpos, n);
      return;
    }

  sf = XFRAME (selected_frame);
  
  /* If the lines below the insertion are being pushed
     into the end of the window, this is the same as clearing;
     and we know the lines are already clear, since the matching
     deletion has already been done.  So can ignore this.  */
  /* If the lines below the deletion are blank lines coming
     out of the end of the window, don't bother,
     as there will be a matching inslines later that will flush them. */
  if (scroll_region_ok && vpos + i >= specified_window)
    return;
  if (!memory_below_frame && vpos + i >= FRAME_HEIGHT (sf))
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

  if (!scroll_region_ok && memory_below_frame && n < 0)
    {
      cursor_to (FRAME_HEIGHT (sf) + n, 0);
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
  for (i = FRAME_WIDTH (frame); --i >= 0;)
    *--p = (del_startup_cost += del_cost_per_char);

  /* Doing nothing is free */
  p = &char_ins_del_cost (frame)[0];
  *p++ = 0;

  /* Insert costs are at positive offsets */
  for (i = FRAME_WIDTH (frame); --i >= 0;)
    *p++ = (ins_startup_cost += ins_cost_per_char);
}

void
calculate_costs (frame)
     FRAME_PTR frame;
{
  register char *f = (TS_set_scroll_region
		      ? TS_set_scroll_region
		      : TS_set_scroll_region_1);

  FRAME_COST_BAUD_RATE (frame) = baud_rate;

  scroll_region_cost = string_cost (f);

  /* These variables are only used for terminal stuff.  They are allocated
     once for the terminal frame of X-windows emacs, but not used afterwards.

     char_ins_del_vector (i.e., char_ins_del_cost) isn't used because
     X turns off char_ins_del_ok. */

  max_frame_height = max (max_frame_height, FRAME_HEIGHT (frame));
  max_frame_width = max (max_frame_width, FRAME_WIDTH (frame));

  costs_set = 1;

  if (char_ins_del_vector != 0)
    char_ins_del_vector
      = (int *) xrealloc (char_ins_del_vector,
			  (sizeof (int)
			   + 2 * max_frame_width * sizeof (int)));
  else
    char_ins_del_vector
      = (int *) xmalloc (sizeof (int)
			 + 2 * max_frame_width * sizeof (int));

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

static struct fkey_table keys[] =
{
  {"kh", "home"},	/* termcap */
  {"kl", "left"},	/* termcap */
  {"ku", "up"},		/* termcap */
  {"kr", "right"},	/* termcap */
  {"kd", "down"},	/* termcap */
  {"%8", "prior"},	/* terminfo */
  {"%5", "next"},	/* terminfo */
  {"@7", "end"},	/* terminfo */
  {"@1", "begin"},	/* terminfo */
  {"*6", "select"},	/* terminfo */
  {"%9", "print"},	/* terminfo */
  {"@4", "execute"},	/* terminfo --- actually the `command' key */
  /*
   * "insert" --- see below
   */
  {"&8", "undo"},	/* terminfo */
  {"%0", "redo"},	/* terminfo */
  {"%7", "menu"},	/* terminfo --- actually the `options' key */
  {"@0", "find"},	/* terminfo */
  {"@2", "cancel"},	/* terminfo */
  {"%1", "help"},	/* terminfo */
  /*
   * "break" goes here, but can't be reliably intercepted with termcap
   */
  {"&4", "reset"},	/* terminfo --- actually `restart' */
  /*
   * "system" and "user" --- no termcaps
   */
  {"kE", "clearline"},	/* terminfo */
  {"kA", "insertline"},	/* terminfo */
  {"kL", "deleteline"},	/* terminfo */
  {"kI", "insertchar"},	/* terminfo */
  {"kD", "deletechar"},	/* terminfo */
  {"kB", "backtab"},	/* terminfo */
  /*
   * "kp_backtab", "kp-space", "kp-tab" --- no termcaps
   */
  {"@8", "kp-enter"},	/* terminfo */
  /*
   * "kp-f1", "kp-f2", "kp-f3" "kp-f4",
   * "kp-multiply", "kp-add", "kp-separator",
   * "kp-subtract", "kp-decimal", "kp-divide", "kp-0";
   * --- no termcaps for any of these.
   */
  {"K4", "kp-1"},	/* terminfo */
  /*
   * "kp-2" --- no termcap
   */
  {"K5", "kp-3"},	/* terminfo */
  /*
   * "kp-4" --- no termcap
   */
  {"K2", "kp-5"},	/* terminfo */
  /*
   * "kp-6" --- no termcap
   */
  {"K1", "kp-7"},	/* terminfo */
  /*
   * "kp-8" --- no termcap
   */
  {"K3", "kp-9"},	/* terminfo */
  /*
   * "kp-equal" --- no termcap
   */
  {"k1", "f1"},
  {"k2", "f2"},
  {"k3", "f3"},
  {"k4", "f4"},
  {"k5", "f5"},
  {"k6", "f6"},
  {"k7", "f7"},
  {"k8", "f8"},
  {"k9", "f9"}
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
	if (k0)
	  /* Define f0 first, so that f10 takes precedence in case the
	     key sequences happens to be the same.  */
	  Fdefine_key (Vfunction_key_map, build_string (k0),
		       Fmake_vector (make_number (1), intern ("f0")));
	Fdefine_key (Vfunction_key_map, build_string (k_semi),
		     Fmake_vector (make_number (1), intern ("f10")));
      }
    else if (k0)
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
      /* if there's no key_end keycap, map key_ll to 'end' keysym */
      CONDITIONAL_REASSIGN ("@7", "kH", "end");

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

  return Qnil;
}


/***********************************************************************
		       Character Display Information
 ***********************************************************************/

static void append_glyph P_ ((struct it *));


/* Append glyphs to IT's glyph_row.  Called from produce_glyphs for
   terminal frames if IT->glyph_row != NULL.  IT->c is the character
   for which to produce glyphs; IT->face_id contains the character's
   face.  Padding glyphs are appended if IT->c has a IT->pixel_width >
   1.  */
   
static void
append_glyph (it)
     struct it *it;
{
  struct glyph *glyph, *end;
  int i;

  xassert (it->glyph_row);
  glyph = (it->glyph_row->glyphs[it->area]
	   + it->glyph_row->used[it->area]);
  end = it->glyph_row->glyphs[1 + it->area];

  for (i = 0; 
       i < it->pixel_width && glyph < end; 
       ++i)
    {
      glyph->type = CHAR_GLYPH;
      glyph->pixel_width = 1;
      glyph->u.ch = it->c;
      glyph->face_id = it->face_id;
      glyph->padding_p = i > 0;
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      
      ++it->glyph_row->used[it->area];
      ++glyph;
    }
}


/* Produce glyphs for the display element described by IT.  *IT
   specifies what we want to produce a glyph for (character, image, ...),
   and where in the glyph matrix we currently are (glyph row and hpos).
   produce_glyphs fills in output fields of *IT with information such as the
   pixel width and height of a character, and maybe output actual glyphs at
   the same time if IT->glyph_row is non-null.  See the explanation of
   struct display_iterator in dispextern.h for an overview.

   produce_glyphs also stores the result of glyph width, ascent
   etc. computations in *IT.

   IT->glyph_row may be null, in which case produce_glyphs does not
   actually fill in the glyphs.  This is used in the move_* functions
   in xdisp.c for text width and height computations.

   Callers usually don't call produce_glyphs directly;
   instead they use the macro PRODUCE_GLYPHS.  */

void 
produce_glyphs (it)
     struct it *it;
{
  /* If a hook is installed, let it do the work.  */
  xassert (it->what == IT_CHARACTER
	   || it->what == IT_COMPOSITION
	   || it->what == IT_IMAGE
	   || it->what == IT_STRETCH);
  
  /* Nothing but characters are supported on terminal frames.  For a
     composition sequence, it->c is the first character of the
     sequence.  */
  xassert (it->what == IT_CHARACTER
	   || it->what == IT_COMPOSITION);

  if (it->c >= 040 && it->c < 0177)
    {
      it->pixel_width = it->nglyphs = 1;
      if (it->glyph_row)
	append_glyph (it);
    }
  else if (it->c == '\n')
    it->pixel_width = it->nglyphs = 0;
  else if (it->c == '\t')
    {
      int absolute_x = (it->current_x
			+ it->continuation_lines_width);
      int next_tab_x 
	= (((1 + absolute_x + it->tab_width - 1) 
	    / it->tab_width)
	   * it->tab_width);
      int nspaces;

      /* If part of the TAB has been displayed on the previous line
	 which is continued now, continuation_lines_width will have
	 been incremented already by the part that fitted on the
	 continued line.  So, we will get the right number of spaces
	 here.  */
      nspaces = next_tab_x - absolute_x;
      
      if (it->glyph_row)
	{
	  int n = nspaces;
	  
	  it->c = ' ';
	  it->pixel_width = it->len = 1;
	  
	  while (n--)
	    append_glyph (it);
	  
	  it->c = '\t';
	}

      it->pixel_width = nspaces;
      it->nglyphs = nspaces;
    }
  else if (SINGLE_BYTE_CHAR_P (it->c))
    {
      /* Coming here means that it->c is from display table, thus we
	 must send the code as is to the terminal.  Although there's
	 no way to know how many columns it occupies on a screen, it
	 is a good assumption that a single byte code has 1-column
	 width.  */
      it->pixel_width = it->nglyphs = 1;
      if (it->glyph_row)
	append_glyph (it);
    }
  else
    {
      /* A multi-byte character.  The display width is fixed for all
	 characters of the set.  Some of the glyphs may have to be
	 ignored because they are already displayed in a continued
	 line.  */
      int charset = CHAR_CHARSET (it->c);

      it->pixel_width = CHARSET_WIDTH (charset);
      it->nglyphs = it->pixel_width;
      
      if (it->glyph_row)
	append_glyph (it);
    }

  /* Advance current_x by the pixel width as a convenience for 
     the caller.  */
  if (it->area == TEXT_AREA)
    it->current_x += it->pixel_width;
  it->ascent = it->max_ascent = it->phys_ascent = it->max_phys_ascent = 0;
  it->descent = it->max_descent = it->phys_descent = it->max_phys_descent = 1;
}


/* Get information about special display element WHAT in an
   environment described by IT.  WHAT is one of IT_TRUNCATION or
   IT_CONTINUATION.  Maybe produce glyphs for WHAT if IT has a
   non-null glyph_row member.  This function ensures that fields like
   face_id, c, len of IT are left untouched.  */

void
produce_special_glyphs (it, what)
     struct it *it;
     enum display_element_type what;
{
  struct it temp_it;
  
  temp_it = *it;
  temp_it.dp = NULL;
  temp_it.what = IT_CHARACTER;
  temp_it.len = 1;
  temp_it.object = make_number (0);
  bzero (&temp_it.current, sizeof temp_it.current);

  if (what == IT_CONTINUATION)
    {
      /* Continuation glyph.  */
      if (it->dp
	  && INTEGERP (DISP_CONTINUE_GLYPH (it->dp))
	  && GLYPH_CHAR_VALID_P (XINT (DISP_CONTINUE_GLYPH (it->dp))))
	{
	  temp_it.c = FAST_GLYPH_CHAR (XINT (DISP_CONTINUE_GLYPH (it->dp)));
	  temp_it.len = CHAR_BYTES (temp_it.c);
	}
      else
	temp_it.c = '\\';
      
      produce_glyphs (&temp_it);
      it->pixel_width = temp_it.pixel_width;
      it->nglyphs = temp_it.pixel_width;
    }
  else if (what == IT_TRUNCATION)
    {
      /* Truncation glyph.  */
      if (it->dp
	  && INTEGERP (DISP_TRUNC_GLYPH (it->dp))
	  && GLYPH_CHAR_VALID_P (XINT (DISP_TRUNC_GLYPH (it->dp))))
	{
	  temp_it.c = FAST_GLYPH_CHAR (XINT (DISP_TRUNC_GLYPH (it->dp)));
	  temp_it.len = CHAR_BYTES (temp_it.c);
	}
      else
	temp_it.c = '$';
      
      produce_glyphs (&temp_it);
      it->pixel_width = temp_it.pixel_width;
      it->nglyphs = temp_it.pixel_width;
    }
  else
    abort ();
}


/* Return an estimation of the pixel height of mode or top lines on
   frame F.  FACE_ID specifies what line's height to estimate.  */

int
estimate_mode_line_height (f, face_id)
     struct frame *f;
     enum face_id face_id;
{
  if (estimate_mode_line_height_hook)
    return estimate_mode_line_height_hook (f, face_id);
  else
    return 1;
}



/***********************************************************************
				Faces
 ***********************************************************************/

/* Value is non-zero if attribute ATTR may be used.  ATTR should be
   one of the enumerators from enum no_color_bit, or a bit set built
   from them.  Some display attributes may not be used together with
   color; the termcap capability `NC' specifies which ones.  */

#define MAY_USE_WITH_COLORS_P(ATTR)		\
     (TN_max_colors > 0				\
      ? (TN_no_color_video & (ATTR)) == 0	\
      : 1)

/* Turn appearances of face FACE_ID on tty frame F on.  */

static void
turn_on_face (f, face_id)
     struct frame *f;
     int face_id;
{
  struct face *face = FACE_FROM_ID (f, face_id);
  long fg = face->foreground;
  long bg = face->background;

  /* Do this first because TS_end_standout_mode may be the same
     as TS_exit_attribute_mode, which turns all appearances off. */
  if (MAY_USE_WITH_COLORS_P (NC_REVERSE))
    {
      if (TN_max_colors > 0)
	{
	  if (fg >= 0 && bg >= 0)
	    {
	      /* If the terminal supports colors, we can set them
		 below without using reverse video.  The face's fg
		 and bg colors are set as they should appear on
		 the screen, i.e. they take the inverse-video'ness
		 of the face already into account.  */
	    }
	  else if (inverse_video)
	    {
	      if (fg == FACE_TTY_DEFAULT_FG_COLOR
		  || bg == FACE_TTY_DEFAULT_BG_COLOR)
		toggle_highlight ();
	    }
	  else
	    {
	      if (fg == FACE_TTY_DEFAULT_BG_COLOR
		  || bg == FACE_TTY_DEFAULT_FG_COLOR)
		toggle_highlight ();
	    }
	}
      else
	{
	  /* If we can't display colors, use reverse video
	     if the face specifies that.  */
	  if (inverse_video)
	    {
	      if (fg == FACE_TTY_DEFAULT_FG_COLOR
		  || bg == FACE_TTY_DEFAULT_BG_COLOR)
		toggle_highlight ();
	    }
	  else
	    {
	      if (fg == FACE_TTY_DEFAULT_BG_COLOR
		  || bg == FACE_TTY_DEFAULT_FG_COLOR)
		toggle_highlight ();
	    }
	}
    }

  if (face->tty_bold_p)
    {
      if (MAY_USE_WITH_COLORS_P (NC_BOLD))
	OUTPUT1_IF (TS_enter_bold_mode);
    }
  else if (face->tty_dim_p)
    if (MAY_USE_WITH_COLORS_P (NC_DIM))
      OUTPUT1_IF (TS_enter_dim_mode);

  /* Alternate charset and blinking not yet used.  */
  if (face->tty_alt_charset_p
      && MAY_USE_WITH_COLORS_P (NC_ALT_CHARSET))
    OUTPUT1_IF (TS_enter_alt_charset_mode);

  if (face->tty_blinking_p
      && MAY_USE_WITH_COLORS_P (NC_BLINK))
    OUTPUT1_IF (TS_enter_blink_mode);

  if (face->tty_underline_p && MAY_USE_WITH_COLORS_P (NC_UNDERLINE))
    OUTPUT1_IF (TS_enter_underline_mode);

  if (TN_max_colors > 0)
    {
      char *p;
      
      if (fg >= 0 && TS_set_foreground)
	{
	  p = tparam (TS_set_foreground, NULL, 0, (int) fg);
	  OUTPUT (p);
	  xfree (p);
	}

      if (bg >= 0 && TS_set_background)
	{
	  p = tparam (TS_set_background, NULL, 0, (int) bg);
	  OUTPUT (p);
	  xfree (p);
	}
    }
}
  

/* Turn off appearances of face FACE_ID on tty frame F.  */

static void
turn_off_face (f, face_id)
     struct frame *f;
     int face_id;
{
  struct face *face = FACE_FROM_ID (f, face_id);

  xassert (face != NULL);

  if (TS_exit_attribute_mode)
    {
      /* Capability "me" will turn off appearance modes double-bright,
	 half-bright, reverse-video, standout, underline.  It may or
	 may not turn off alt-char-mode.  */
      if (face->tty_bold_p
	  || face->tty_dim_p
	  || face->tty_reverse_p
	  || face->tty_alt_charset_p
	  || face->tty_blinking_p
	  || face->tty_underline_p)
	{
	  OUTPUT1_IF (TS_exit_attribute_mode);
	  if (strcmp (TS_exit_attribute_mode, TS_end_standout_mode) == 0)
	    standout_mode = 0;
	}

      if (face->tty_alt_charset_p)
	OUTPUT_IF (TS_exit_alt_charset_mode);
    }
  else
    {
      /* If we don't have "me" we can only have those appearances
	 that have exit sequences defined.  */
      if (face->tty_alt_charset_p)
	OUTPUT_IF (TS_exit_alt_charset_mode);

      if (face->tty_underline_p)
	OUTPUT_IF (TS_exit_underline_mode);
    }

  /* Switch back to default colors.  */
  if (TN_max_colors > 0
      && ((face->foreground != FACE_TTY_DEFAULT_COLOR
	   && face->foreground != FACE_TTY_DEFAULT_FG_COLOR)
	  || (face->background != FACE_TTY_DEFAULT_COLOR
	      && face->background != FACE_TTY_DEFAULT_BG_COLOR)))
    OUTPUT1_IF (TS_orig_pair);
}
  
    
/* Return non-zero if the terminal is capable to display colors.  */

DEFUN ("tty-display-color-p", Ftty_display_color_p, Stty_display_color_p,
       0, 1, 0,
       doc: /* Return non-nil if TTY can display colors on DISPLAY.  */)
     (display)
     Lisp_Object display;
{
  return TN_max_colors > 0 ? Qt : Qnil;
}

/* Return the number of supported colors.  */
DEFUN ("tty-display-color-cells", Ftty_display_color_cells,
       Stty_display_color_cells, 0, 1, 0,
       doc: /* Return the number of colors supported by TTY on DISPLAY.  */)
     (display)
     Lisp_Object display;
{
  return make_number (TN_max_colors);
}

#ifndef WINDOWSNT

/* Save or restore the default color-related capabilities of this
   terminal.  */
static void
tty_default_color_capabilities (save)
     int save;
{
  static char
    *default_orig_pair, *default_set_foreground, *default_set_background;
  static int default_max_colors, default_max_pairs, default_no_color_video;

  if (save)
    {
      if (default_orig_pair)
	xfree (default_orig_pair);
      default_orig_pair = TS_orig_pair ? xstrdup (TS_orig_pair) : NULL;

      if (default_set_foreground)
	xfree (default_set_foreground);
      default_set_foreground = TS_set_foreground ? xstrdup (TS_set_foreground)
			       : NULL;

      if (default_set_background)
	xfree (default_set_background);
      default_set_background = TS_set_background ? xstrdup (TS_set_background)
			       : NULL;

      default_max_colors = TN_max_colors;
      default_max_pairs = TN_max_pairs;
      default_no_color_video = TN_no_color_video;
    }
  else
    {
      TS_orig_pair = default_orig_pair;
      TS_set_foreground = default_set_foreground;
      TS_set_background = default_set_background;
      TN_max_colors = default_max_colors;
      TN_max_pairs = default_max_pairs;
      TN_no_color_video = default_no_color_video;
    }
}

/* Setup one of the standard tty color schemes according to MODE.
   MODE's value is generally the number of colors which we want to
   support; zero means set up for the default capabilities, the ones
   we saw at term_init time; -1 means turn off color support.  */
void
tty_setup_colors (mode)
     int mode;
{
  switch (mode)
    {
      case -1:	 /* no colors at all */
	TN_max_colors = 0;
	TN_max_pairs = 0;
	TN_no_color_video = 0;
	TS_set_foreground = TS_set_background = TS_orig_pair = NULL;
	break;
      case 0:	 /* default colors, if any */
      default:
	tty_default_color_capabilities (0);
	break;
      case 8:	/* 8 standard ANSI colors */
	TS_orig_pair = "\033[0m";
#ifdef TERMINFO
	TS_set_foreground = "\033[3%p1%dm";
	TS_set_background = "\033[4%p1%dm";
#else
	TS_set_foreground = "\033[3%dm";
	TS_set_background = "\033[4%dm";
#endif
	TN_max_colors = 8;
	TN_max_pairs = 64;
	TN_no_color_video = 0;
	break;
    }
}

void
set_tty_color_mode (f, val)
     struct frame *f;
     Lisp_Object val;
{
  Lisp_Object color_mode_spec, current_mode_spec;
  Lisp_Object color_mode, current_mode;
  int mode, old_mode;
  extern Lisp_Object Qtty_color_mode;
  Lisp_Object tty_color_mode_alist;

  tty_color_mode_alist = Fintern_soft (build_string ("tty-color-mode-alist"),
				       Qnil);

  if (NATNUMP (val))
    color_mode = val;
  else
    {
      if (NILP (tty_color_mode_alist))
	color_mode_spec = Qnil;
      else
	color_mode_spec = Fassq (val, XSYMBOL (tty_color_mode_alist)->value);
      current_mode_spec = assq_no_quit (Qtty_color_mode, f->param_alist);

      if (CONSP (color_mode_spec))
	color_mode = XCDR (color_mode_spec);
      else
	color_mode = Qnil;
    }
  if (CONSP (current_mode_spec))
    current_mode = XCDR (current_mode_spec);
  else
    current_mode = Qnil;
  if (NATNUMP (color_mode))
    mode = XINT (color_mode);
  else
    mode = 0;	/* meaning default */
  if (NATNUMP (current_mode))
    old_mode = XINT (current_mode);
  else
    old_mode = 0;

  if (mode != old_mode)
    {
      tty_setup_colors (mode);
      /*  This recomputes all the faces given the new color
	  definitions.  */
      call0 (intern ("tty-set-up-initial-frame-faces"));
      redraw_frame (f);
    }
}

#endif /* !WINDOWSNT */


/***********************************************************************
			    Initialization
 ***********************************************************************/

void
term_init (terminal_type)
     char *terminal_type;
{
  char *area;
  char **address = &area;
  char buffer[2044];
  register char *p;
  int status;
  struct frame *sf = XFRAME (selected_frame);

#ifdef WINDOWSNT
  initialize_w32_display ();

  Wcm_clear ();

  area = (char *) xmalloc (2044);

  if (area == 0)
    abort ();

  FrameRows = FRAME_HEIGHT (sf);
  FrameCols = FRAME_WIDTH (sf);
  specified_window = FRAME_HEIGHT (sf);

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

  FRAME_CAN_HAVE_SCROLL_BARS (sf) = 0;
  FRAME_VERTICAL_SCROLL_BAR_TYPE (sf) = vertical_scroll_bar_none;
  TN_max_colors = 16;  /* Required to be non-zero for tty-display-color-p */

  return;
#else  /* not WINDOWSNT */

  Wcm_clear ();

  status = tgetent (buffer, terminal_type);
  if (status < 0)
    {
#ifdef TERMINFO
      fatal ("Cannot open terminfo database file");
#else
      fatal ("Cannot open termcap database file");
#endif
    }
  if (status == 0)
    {
#ifdef TERMINFO
      fatal ("Terminal type %s is not defined.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMINFO' (C-shell: `unsetenv TERMINFO') as well.",
	     terminal_type);
#else
      fatal ("Terminal type %s is not defined.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMCAP' (C-shell: `unsetenv TERMCAP') as well.",
	     terminal_type);
#endif
    }
#ifdef TERMINFO
  area = (char *) xmalloc (2044);
#else
  area = (char *) xmalloc (strlen (buffer));
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
  ColPosition = NULL; /* tgetstr ("ch", address); */
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
  TS_cursor_normal = tgetstr ("ve", address);
  TS_cursor_visible = tgetstr ("vs", address);
  TS_cursor_invisible = tgetstr ("vi", address);
  TS_set_window = tgetstr ("wi", address);
  
  TS_enter_underline_mode = tgetstr ("us", address);
  TS_exit_underline_mode = tgetstr ("ue", address);
  TS_enter_bold_mode = tgetstr ("md", address);
  TS_enter_dim_mode = tgetstr ("mh", address);
  TS_enter_blink_mode = tgetstr ("mb", address);
  TS_enter_reverse_mode = tgetstr ("mr", address);
  TS_enter_alt_charset_mode = tgetstr ("as", address);
  TS_exit_alt_charset_mode = tgetstr ("ae", address);
  TS_exit_attribute_mode = tgetstr ("me", address);
  
  MultiUp = tgetstr ("UP", address);
  MultiDown = tgetstr ("DO", address);
  MultiLeft = tgetstr ("LE", address);
  MultiRight = tgetstr ("RI", address);

  /* SVr4/ANSI color suppert.  If "op" isn't available, don't support
     color because we can't switch back to the default foreground and
     background.  */
  TS_orig_pair = tgetstr ("op", address);
  if (TS_orig_pair)
    {
      TS_set_foreground = tgetstr ("AF", address);
      TS_set_background = tgetstr ("AB", address);
      if (!TS_set_foreground)
	{
	  /* SVr4.  */
	  TS_set_foreground = tgetstr ("Sf", address);
	  TS_set_background = tgetstr ("Sb", address);
	}
      
      TN_max_colors = tgetnum ("Co");
      TN_max_pairs = tgetnum ("pa");
      
      TN_no_color_video = tgetnum ("NC");
      if (TN_no_color_video == -1)
	TN_no_color_video = 0;
    }

  tty_default_color_capabilities (1);

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
  TF_teleray = tgetflag ("xt");

  term_get_fkeys (address);

  /* Get frame size from system, or else from termcap.  */
  {
    int height, width;
    get_frame_size (&width, &height);
    FRAME_WIDTH (sf) = width;
    FRAME_HEIGHT (sf) = height;
  }

  if (FRAME_WIDTH (sf) <= 0)
    SET_FRAME_WIDTH (sf, tgetnum ("co"));
  else
    /* Keep width and external_width consistent */
    SET_FRAME_WIDTH (sf, FRAME_WIDTH (sf));
  if (FRAME_HEIGHT (sf) <= 0)
    FRAME_HEIGHT (sf) = tgetnum ("li");
  
  if (FRAME_HEIGHT (sf) < 3 || FRAME_WIDTH (sf) < 3)
    fatal ("Screen size %dx%d is too small",
	   FRAME_HEIGHT (sf), FRAME_WIDTH (sf));

  min_padding_speed = tgetnum ("pb");
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

  /* We don't support standout modes that use `magic cookies', so
     turn off any that do.  */
  if (TS_standout_mode && tgetnum ("sg") >= 0)
    {
      TS_standout_mode = 0;
      TS_end_standout_mode = 0;
    }
  if (TS_enter_underline_mode && tgetnum ("ug") >= 0)
    {
      TS_enter_underline_mode = 0;
      TS_exit_underline_mode = 0;
    }

  /* If there's no standout mode, try to use underlining instead.  */
  if (TS_standout_mode == 0)
    {
      TS_standout_mode = TS_enter_underline_mode;
      TS_end_standout_mode = TS_exit_underline_mode;
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
      /* We can't support standout mode, because it uses magic cookies.  */
      TS_standout_mode = 0;
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

  FrameRows = FRAME_HEIGHT (sf);
  FrameCols = FRAME_WIDTH (sf);
  specified_window = FRAME_HEIGHT (sf);

  if (Wcm_init () == -1)	/* can't do cursor motion */
#ifdef VMS
    fatal ("Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have, use either the\n\
DCL command `SET TERMINAL/DEVICE= ...' for DEC-compatible terminals,\n\
or `define EMACS_TERM \"terminal type\"' for non-DEC terminals.",
           terminal_type);
#else /* not VMS */
# ifdef TERMINFO
    fatal ("Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMINFO' (C-shell: `unsetenv TERMINFO') as well.",
	   terminal_type);
# else /* TERMCAP */
    fatal ("Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMCAP' (C-shell: `unsetenv TERMCAP') as well.",
	   terminal_type);
# endif /* TERMINFO */
#endif /*VMS */
  if (FRAME_HEIGHT (sf) <= 0
      || FRAME_WIDTH (sf) <= 0)
    fatal ("The frame size has not been specified");

  delete_in_insert_mode
    = TS_delete_mode && TS_insert_mode
      && !strcmp (TS_delete_mode, TS_insert_mode);

  se_is_so = (TS_standout_mode
	      && TS_end_standout_mode
	      && !strcmp (TS_standout_mode, TS_end_standout_mode));

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

  FRAME_CAN_HAVE_SCROLL_BARS (sf) = 0;
  FRAME_VERTICAL_SCROLL_BAR_TYPE (sf) = vertical_scroll_bar_none;
#endif /* WINDOWSNT */
}

/* VARARGS 1 */
void
fatal (str, arg1, arg2)
     char *str, *arg1, *arg2;
{
  fprintf (stderr, "emacs: ");
  fprintf (stderr, str, arg1, arg2);
  fprintf (stderr, "\n");
  fflush (stderr);
  exit (1);
}

void
syms_of_term ()
{
  DEFVAR_BOOL ("system-uses-terminfo", &system_uses_terminfo,
    doc: /* Non-nil means the system uses terminfo rather than termcap.
This variable can be used by terminal emulator packages.  */);
#ifdef TERMINFO
  system_uses_terminfo = 1;
#else
  system_uses_terminfo = 0;
#endif

  DEFVAR_LISP ("ring-bell-function", &Vring_bell_function,
    doc: /* Non-nil means call this function to ring the bell.
The function should accept no arguments.  */);
  Vring_bell_function = Qnil;

  defsubr (&Stty_display_color_p);
  defsubr (&Stty_display_color_cells);
}

