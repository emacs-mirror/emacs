/* Terminal control module for terminals described by TERMCAP
   Copyright (C) 1985, 86, 87, 93, 94, 95, 98, 2000, 2001, 2002
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

#include <sys/file.h>

#include "lisp.h"
#include "termchar.h"
#include "termopts.h"
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
#ifdef MAC_OS
#include "macterm.h"
#endif

#ifndef O_RDWR
#define O_RDWR 2
#endif

static void turn_on_face P_ ((struct frame *, int face_id));
static void turn_off_face P_ ((struct frame *, int face_id));
static void tty_show_cursor P_ ((struct tty_display_info *));
static void tty_hide_cursor P_ ((struct tty_display_info *));

void delete_initial_display P_ ((struct display *));
void delete_tty P_ ((struct display *));
void create_tty_output P_ ((struct frame *));
void delete_tty_output P_ ((struct frame *));

#define OUTPUT(tty, a)                                          \
  emacs_tputs ((tty), a,                                        \
               (int) (FRAME_LINES (XFRAME (selected_frame))     \
                      - curY (tty)),                            \
               cmputc)

#define OUTPUT1(tty, a) emacs_tputs ((tty), a, 1, cmputc)
#define OUTPUTL(tty, a, lines) emacs_tputs ((tty), a, lines, cmputc)

#define OUTPUT_IF(tty, a)                                               \
  do {                                                                  \
    if (a)                                                              \
      emacs_tputs ((tty), a,                                            \
                   (int) (FRAME_LINES (XFRAME (selected_frame))         \
                          - curY (tty) ),                               \
                   cmputc);                                             \
  } while (0)

#define OUTPUT1_IF(tty, a) do { if (a) emacs_tputs ((tty), a, 1, cmputc); } while (0)

/* Function to use to ring the bell.  */

Lisp_Object Vring_bell_function;

/* Functions to call after a tty was deleted. */
Lisp_Object Vdelete_tty_after_functions;

/* Chain of all displays currently in use. */
struct display *display_list;

/* The initial display device, created by initial_term_init. */
struct display *initial_display;

/* Chain of all tty device parameters. */
struct tty_display_info *tty_list;

/* Nonzero means no need to redraw the entire frame on resuming a
   suspended Emacs.  This is useful on terminals with multiple
   pages, where one page is used for Emacs and another for all
   else. */
int no_redraw_on_reenter;

Lisp_Object Qframe_tty_name, Qframe_tty_type;



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

/* internal state */

/* The largest frame width in any call to calculate_costs.  */

int max_frame_cols;

/* The largest frame height in any call to calculate_costs.  */

int max_frame_lines;

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

void
ring_bell ()
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

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
  else if (FRAME_DISPLAY (f)->ring_bell_hook)
    (*FRAME_DISPLAY (f)->ring_bell_hook) ();
}

void
tty_ring_bell ()
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);

  OUTPUT (tty, (tty->TS_visible_bell && visible_bell
                ? tty->TS_visible_bell
                : tty->TS_bell));
}

void tty_set_terminal_modes (struct display *display)
{
  struct tty_display_info *tty = display->display_info.tty;
  
  OUTPUT_IF (tty, tty->TS_termcap_modes);
  OUTPUT_IF (tty, tty->TS_cursor_visible);
  OUTPUT_IF (tty, tty->TS_keypad_mode);
  losecursor (tty);
}

void tty_reset_terminal_modes (struct display *display)
{
  struct tty_display_info *tty = display->display_info.tty;
  
  turn_off_highlight (tty);
  turn_off_insert (tty);
  OUTPUT_IF (tty, tty->TS_end_keypad_mode);
  OUTPUT_IF (tty, tty->TS_cursor_normal);
  OUTPUT_IF (tty, tty->TS_end_termcap_modes);
  OUTPUT_IF (tty, tty->TS_orig_pair);
  /* Output raw CR so kernel can track the cursor hpos.  */
  current_tty = tty;
  cmputc ('\r');
}

void
update_begin (f)
     struct frame *f;
{
  updating_frame = f;
  if (FRAME_DISPLAY (f)->update_begin_hook)
    (*FRAME_DISPLAY (f)->update_begin_hook) (f);
}

void
update_end (f)
     struct frame *f;
{
  if (FRAME_DISPLAY (f)->update_end_hook)
    (*FRAME_DISPLAY (f)->update_end_hook) (f);
  updating_frame = NULL;
}

void
tty_update_end (struct frame *f)
{
  struct tty_display_info *tty = FRAME_TTY (f);

  if (!XWINDOW (selected_window)->cursor_off_p)
    tty_show_cursor (tty);
  turn_off_insert (tty);
  background_highlight (tty);
}

void
set_terminal_window (size)
     int size;
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  if (FRAME_DISPLAY (f)->set_terminal_window_hook)
    (*FRAME_DISPLAY (f)->set_terminal_window_hook) (size);
}

void
tty_set_terminal_window (int size)
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);

  tty->specified_window = size ? size : FRAME_LINES (f);
  if (FRAME_SCROLL_REGION_OK (f))
    set_scroll_region (0, tty->specified_window);
}

void
set_scroll_region (start, stop)
     int start, stop;
{
  char *buf;
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->TS_set_scroll_region)
    buf = tparam (tty->TS_set_scroll_region, 0, 0, start, stop - 1);
  else if (tty->TS_set_scroll_region_1)
    buf = tparam (tty->TS_set_scroll_region_1, 0, 0,
		  FRAME_LINES (f), start,
		  FRAME_LINES (f) - stop,
		  FRAME_LINES (f));
  else
    buf = tparam (tty->TS_set_window, 0, 0, start, 0, stop, FRAME_COLS (f));

  OUTPUT (tty, buf);
  xfree (buf);
  losecursor (tty);
}


static void
turn_on_insert (struct tty_display_info *tty)
{
  if (!tty->insert_mode)
    OUTPUT (tty, tty->TS_insert_mode);
  tty->insert_mode = 1;
}

void
turn_off_insert (struct tty_display_info *tty)
{
  if (tty->insert_mode)
    OUTPUT (tty, tty->TS_end_insert_mode);
  tty->insert_mode = 0;
}

/* Handle highlighting.  */

void
turn_off_highlight (struct tty_display_info *tty)
{
  if (tty->standout_mode)
    OUTPUT_IF (tty, tty->TS_end_standout_mode);
  tty->standout_mode = 0;
}

static void
turn_on_highlight (struct tty_display_info *tty)
{
  if (!tty->standout_mode)
    OUTPUT_IF (tty, tty->TS_standout_mode);
  tty->standout_mode = 1;
}

static void
toggle_highlight (struct tty_display_info *tty)
{
  if (tty->standout_mode)
    turn_off_highlight (tty);
  else
    turn_on_highlight (tty);
}


/* Make cursor invisible.  */

static void
tty_hide_cursor (struct tty_display_info *tty)
{
  if (tty->cursor_hidden == 0)
    {
      tty->cursor_hidden = 1;
      OUTPUT_IF (tty, tty->TS_cursor_invisible);
    }
}


/* Ensure that cursor is visible.  */

static void
tty_show_cursor (struct tty_display_info *tty)
{
  if (tty->cursor_hidden)
    {
      tty->cursor_hidden = 0;
      OUTPUT_IF (tty, tty->TS_cursor_normal);
      OUTPUT_IF (tty, tty->TS_cursor_visible);
    }
}


/* Set standout mode to the state it should be in for
   empty space inside windows.  What this is,
   depends on the user option inverse-video.  */

void
background_highlight (struct tty_display_info *tty)
{
  if (inverse_video)
    turn_on_highlight (tty);
  else
    turn_off_highlight (tty);
}

/* Set standout mode to the mode specified for the text to be output.  */

static void
highlight_if_desired (struct tty_display_info *tty)
{
  if (inverse_video)
    turn_on_highlight (tty);
  else
    turn_off_highlight (tty);
}


/* Move cursor to row/column position VPOS/HPOS.  HPOS/VPOS are
   frame-relative coordinates.  */

void
cursor_to (vpos, hpos)
     int vpos, hpos;
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  if (FRAME_DISPLAY (f)->cursor_to_hook)
    (*FRAME_DISPLAY (f)->cursor_to_hook) (vpos, hpos);
}

void
tty_cursor_to (int vpos, int hpos)
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));
  
  struct tty_display_info *tty = FRAME_TTY (f);

  /* Detect the case where we are called from reset_sys_modes
     and the costs have never been calculated.  Do nothing.  */
  if (! tty->costs_set)
    return;

  if (curY (tty) == vpos
      && curX (tty) == hpos)
    return;
  if (!tty->TF_standout_motion)
    background_highlight (tty);
  if (!tty->TF_insmode_motion)
    turn_off_insert (tty);
  cmgoto (tty, vpos, hpos);
}

/* Similar but don't take any account of the wasted characters.  */

void
raw_cursor_to (row, col)
     int row, col;
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  if (FRAME_DISPLAY (f)->raw_cursor_to_hook)
    (*FRAME_DISPLAY (f)->raw_cursor_to_hook) (row, col);  
}

void
tty_raw_cursor_to (int row, int col)
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);

  if (curY (tty) == row
      && curX (tty) == col)
    return;
  if (!tty->TF_standout_motion)
    background_highlight (tty);
  if (!tty->TF_insmode_motion)
    turn_off_insert (tty);
  cmgoto (tty, row, col);
}

/* Erase operations */

/* clear from cursor to end of frame */
void
clear_to_end ()
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  if (FRAME_DISPLAY (f)->clear_to_end_hook)
    (*FRAME_DISPLAY (f)->clear_to_end_hook) ();
}

void
tty_clear_to_end (void)
{
  register int i;
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));
  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->TS_clr_to_bottom)
    {
      background_highlight (tty);
      OUTPUT (tty, tty->TS_clr_to_bottom);
    }
  else
    {
      for (i = curY (tty); i < FRAME_LINES (f); i++)
	{
	  cursor_to (i, 0);
	  clear_end_of_line (FRAME_COLS (f));
	}
    }
}

/* Clear entire frame */

void
clear_frame ()
{
  struct frame *f = (updating_frame ? updating_frame : XFRAME (selected_frame));

  if (FRAME_DISPLAY (f)->clear_frame_hook)
    (*FRAME_DISPLAY (f)->clear_frame_hook) ();
}

void
tty_clear_frame ()
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->TS_clr_frame)
    {
      background_highlight (tty);
      OUTPUT (tty, tty->TS_clr_frame);
      cmat (tty, 0, 0);
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
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  if (FRAME_DISPLAY (f)->clear_end_of_line_hook)
    (*FRAME_DISPLAY (f)->clear_end_of_line_hook) (first_unused_hpos);
}

void
tty_clear_end_of_line (int first_unused_hpos)
{
  register int i;
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));
  struct tty_display_info *tty = FRAME_TTY (f);

  /* Detect the case where we are called from reset_sys_modes
     and the costs have never been calculated.  Do nothing.  */
  if (! tty->costs_set)
    return;

  if (curX (tty) >= first_unused_hpos)
    return;
  background_highlight (tty);
  if (tty->TS_clr_line)
    {
      OUTPUT1 (tty, tty->TS_clr_line);
    }
  else
    {			/* have to do it the hard way */
      turn_off_insert (tty);

      /* Do not write in last row last col with Auto-wrap on. */
      if (AutoWrap (tty)
          && curY (tty) == FrameRows (tty) - 1
	  && first_unused_hpos == FrameCols (tty))
	first_unused_hpos--;

      for (i = curX (tty); i < first_unused_hpos; i++)
	{
	  if (TTY_TERMSCRIPT (tty))
	    fputc (' ', TTY_TERMSCRIPT (tty));
	  fputc (' ', TTY_OUTPUT (tty));
	}
      cmplus (tty, first_unused_hpos - curX (tty));
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
  unsigned char workbuf[MAX_MULTIBYTE_LENGTH];
  const unsigned char *buf;
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
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  if (FRAME_DISPLAY (f)->write_glyphs_hook)
    (*FRAME_DISPLAY (f)->write_glyphs_hook) (string, len);
}

void
tty_write_glyphs (struct glyph *string, int len)
{
  int produced, consumed;
  unsigned char conversion_buffer[1024];
  int conversion_buffer_size = sizeof conversion_buffer;

  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);

  turn_off_insert (tty);
  tty_hide_cursor (tty);

  /* Don't dare write in last column of bottom line, if Auto-Wrap,
     since that would scroll the whole frame on some terminals.  */

  if (AutoWrap (tty)
      && curY (tty) + 1 == FRAME_LINES (f)
      && (curX (tty) + len) == FRAME_COLS (f))
    len --;
  if (len <= 0)
    return;

  cmplus (tty, len);

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
      highlight_if_desired (tty);
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
	      fwrite (conversion_buffer, 1, produced,
                      TTY_OUTPUT (tty));
	      if (ferror (TTY_OUTPUT (tty)))
		clearerr (TTY_OUTPUT (tty));
	      if (TTY_TERMSCRIPT (tty))
		fwrite (conversion_buffer, 1, produced,
                        TTY_TERMSCRIPT (tty));
	    }
	  len -= consumed;
	  n -= consumed;
	  string += consumed;
	}

      /* Turn appearance modes off.  */
      turn_off_face (f, face_id);
      turn_off_highlight (tty);
    }

  /* We may have to output some codes to terminate the writing.  */
  if (CODING_REQUIRE_FLUSHING (&terminal_coding))
    {
      terminal_coding.mode |= CODING_MODE_LAST_BLOCK;
      encode_coding (&terminal_coding, "", conversion_buffer,
		     0, conversion_buffer_size);
      if (terminal_coding.produced > 0)
	{
	  fwrite (conversion_buffer, 1, terminal_coding.produced,
                  TTY_OUTPUT (tty));
	  if (ferror (TTY_OUTPUT (tty)))
	    clearerr (TTY_OUTPUT (tty));
	  if (TTY_TERMSCRIPT (tty))
	    fwrite (conversion_buffer, 1, terminal_coding.produced,
		    TTY_TERMSCRIPT (tty));
	}
    }

  cmcheckmagic (tty);
}

/* If start is zero, insert blanks instead of a string at start */

void
insert_glyphs (start, len)
     register struct glyph *start;
     register int len;
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  if (len <= 0)
    return;

  if (FRAME_DISPLAY (f)->insert_glyphs_hook)
    (*FRAME_DISPLAY (f)->insert_glyphs_hook) (start, len);
}

void
tty_insert_glyphs (struct glyph *start, int len)
{
  char *buf;
  struct glyph *glyph = NULL;
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->TS_ins_multi_chars)
    {
      buf = tparam (tty->TS_ins_multi_chars, 0, 0, len);
      OUTPUT1 (tty, buf);
      xfree (buf);
      if (start)
	write_glyphs (start, len);
      return;
    }

  turn_on_insert (tty);
  cmplus (tty, len);
  /* The bit CODING_MODE_LAST_BLOCK should be set to 1 only at the tail.  */
  terminal_coding.mode &= ~CODING_MODE_LAST_BLOCK;
  while (len-- > 0)
    {
      int produced, consumed;
      unsigned char conversion_buffer[1024];
      int conversion_buffer_size = sizeof conversion_buffer;

      OUTPUT1_IF (tty, tty->TS_ins_char);
      if (!start)
	{
	  conversion_buffer[0] = SPACEGLYPH;
	  produced = 1;
	}
      else
	{
	  highlight_if_desired (tty);
	  turn_on_face (f, start->face_id);
	  glyph = start;
	  ++start;
	  /* We must open sufficient space for a character which
	     occupies more than one column.  */
	  while (len && CHAR_GLYPH_PADDING_P (*start))
	    {
	      OUTPUT1_IF (tty, tty->TS_ins_char);
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
	  fwrite (conversion_buffer, 1, produced,
                  TTY_OUTPUT (tty));
	  if (ferror (TTY_OUTPUT (tty)))
	    clearerr (TTY_OUTPUT (tty));
	  if (TTY_TERMSCRIPT (tty))
	    fwrite (conversion_buffer, 1, produced,
                    TTY_TERMSCRIPT (tty));
	}

      OUTPUT1_IF (tty, tty->TS_pad_inserted_char);
      if (start)
	{
	  turn_off_face (f, glyph->face_id);
	  turn_off_highlight (tty);
	}
    }

  cmcheckmagic (tty);
}

void
delete_glyphs (n)
     register int n;
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  if (FRAME_DISPLAY (f)->delete_glyphs_hook)
    (*FRAME_DISPLAY (f)->delete_glyphs_hook) (n);
}

void
tty_delete_glyphs (int n)
{
  char *buf;
  register int i;
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);

  if (tty->delete_in_insert_mode)
    {
      turn_on_insert (tty);
    }
  else
    {
      turn_off_insert (tty);
      OUTPUT_IF (tty, tty->TS_delete_mode);
    }

  if (tty->TS_del_multi_chars)
    {
      buf = tparam (tty->TS_del_multi_chars, 0, 0, n);
      OUTPUT1 (tty, buf);
      xfree (buf);
    }
  else
    for (i = 0; i < n; i++)
      OUTPUT1 (tty, tty->TS_del_char);
  if (!tty->delete_in_insert_mode)
    OUTPUT_IF (tty, tty->TS_end_delete_mode);
}

/* Insert N lines at vpos VPOS.  If N is negative, delete -N lines.  */

void
ins_del_lines (vpos, n)
     int vpos, n;
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));
  
  if (FRAME_DISPLAY (f)->ins_del_lines_hook)
    (*FRAME_DISPLAY (f)->ins_del_lines_hook) (vpos, n);
}

void
tty_ins_del_lines (int vpos, int n)
{
  struct frame *f = (updating_frame
                     ? updating_frame
                     : XFRAME (selected_frame));

  struct tty_display_info *tty = FRAME_TTY (f);
  char *multi = n > 0 ? tty->TS_ins_multi_lines : tty->TS_del_multi_lines;
  char *single = n > 0 ? tty->TS_ins_line : tty->TS_del_line;
  char *scroll = n > 0 ? tty->TS_rev_scroll : tty->TS_fwd_scroll;

  register int i = n > 0 ? n : -n;
  register char *buf;

  /* If the lines below the insertion are being pushed
     into the end of the window, this is the same as clearing;
     and we know the lines are already clear, since the matching
     deletion has already been done.  So can ignore this.  */
  /* If the lines below the deletion are blank lines coming
     out of the end of the window, don't bother,
     as there will be a matching inslines later that will flush them. */
  if (FRAME_SCROLL_REGION_OK (f)
      && vpos + i >= tty->specified_window)
    return;
  if (!FRAME_MEMORY_BELOW_FRAME (f)
      && vpos + i >= FRAME_LINES (f))
    return;
  
  if (multi)
    {
      raw_cursor_to (vpos, 0);
      background_highlight (tty);
      buf = tparam (multi, 0, 0, i);
      OUTPUT (tty, buf);
      xfree (buf);
    }
  else if (single)
    {
      raw_cursor_to (vpos, 0);
      background_highlight (tty);
      while (--i >= 0)
        OUTPUT (tty, single);
      if (tty->TF_teleray)
        curX (tty) = 0;
    }
  else
    {
      set_scroll_region (vpos, tty->specified_window);
      if (n < 0)
        raw_cursor_to (tty->specified_window - 1, 0);
      else
        raw_cursor_to (vpos, 0);
      background_highlight (tty);
      while (--i >= 0)
        OUTPUTL (tty, scroll, tty->specified_window - vpos);
      set_scroll_region (0, tty->specified_window);
    }
  
  if (!FRAME_SCROLL_REGION_OK (f)
      && FRAME_MEMORY_BELOW_FRAME (f)
      && n < 0)
    {
      cursor_to (FRAME_LINES (f) + n, 0);
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
   The length of this vector is based on max_frame_cols.  */

int *char_ins_del_vector;

#define char_ins_del_cost(f) (&char_ins_del_vector[FRAME_COLS ((f))])
#endif

/* ARGSUSED */
static void
calculate_ins_del_char_costs (f)
     FRAME_PTR f;
{
  struct tty_display_info *tty = FRAME_TTY (f);
  int ins_startup_cost, del_startup_cost;
  int ins_cost_per_char, del_cost_per_char;
  register int i;
  register int *p;

  if (tty->TS_ins_multi_chars)
    {
      ins_cost_per_char = 0;
      ins_startup_cost = string_cost_one_line (tty->TS_ins_multi_chars);
    }
  else if (tty->TS_ins_char || tty->TS_pad_inserted_char
	   || (tty->TS_insert_mode && tty->TS_end_insert_mode))
    {
      ins_startup_cost = (30 * (string_cost (tty->TS_insert_mode)
				+ string_cost (tty->TS_end_insert_mode))) / 100;
      ins_cost_per_char = (string_cost_one_line (tty->TS_ins_char)
			   + string_cost_one_line (tty->TS_pad_inserted_char));
    }
  else
    {
      ins_startup_cost = 9999;
      ins_cost_per_char = 0;
    }

  if (tty->TS_del_multi_chars)
    {
      del_cost_per_char = 0;
      del_startup_cost = string_cost_one_line (tty->TS_del_multi_chars);
    }
  else if (tty->TS_del_char)
    {
      del_startup_cost = (string_cost (tty->TS_delete_mode)
			  + string_cost (tty->TS_end_delete_mode));
      if (tty->delete_in_insert_mode)
	del_startup_cost /= 2;
      del_cost_per_char = string_cost_one_line (tty->TS_del_char);
    }
  else
    {
      del_startup_cost = 9999;
      del_cost_per_char = 0;
    }

  /* Delete costs are at negative offsets */
  p = &char_ins_del_cost (f)[0];
  for (i = FRAME_COLS (f); --i >= 0;)
    *--p = (del_startup_cost += del_cost_per_char);

  /* Doing nothing is free */
  p = &char_ins_del_cost (f)[0];
  *p++ = 0;

  /* Insert costs are at positive offsets */
  for (i = FRAME_COLS (f); --i >= 0;)
    *p++ = (ins_startup_cost += ins_cost_per_char);
}

void
calculate_costs (frame)
     FRAME_PTR frame;
{
  FRAME_COST_BAUD_RATE (frame) = baud_rate;

  if (FRAME_TERMCAP_P (frame))
    {
      struct tty_display_info *tty = FRAME_TTY (frame);
      register char *f = (tty->TS_set_scroll_region
                          ? tty->TS_set_scroll_region
                          : tty->TS_set_scroll_region_1);

      FRAME_SCROLL_REGION_COST (frame) = string_cost (f);

      tty->costs_set = 1;

      /* These variables are only used for terminal stuff.  They are
         allocated once for the terminal frame of X-windows emacs, but not
         used afterwards.

         char_ins_del_vector (i.e., char_ins_del_cost) isn't used because
         X turns off char_ins_del_ok. */

      max_frame_lines = max (max_frame_lines, FRAME_LINES (frame));
      max_frame_cols = max (max_frame_cols, FRAME_COLS (frame));

      if (char_ins_del_vector != 0)
        char_ins_del_vector
          = (int *) xrealloc (char_ins_del_vector,
                              (sizeof (int)
                               + 2 * max_frame_cols * sizeof (int)));
      else
        char_ins_del_vector
          = (int *) xmalloc (sizeof (int)
                             + 2 * max_frame_cols * sizeof (int));

      bzero (char_ins_del_vector, (sizeof (int)
                                   + 2 * max_frame_cols * sizeof (int)));


      if (f && (!tty->TS_ins_line && !tty->TS_del_line))
        do_line_insertion_deletion_costs (frame,
                                          tty->TS_rev_scroll, tty->TS_ins_multi_lines,
                                          tty->TS_fwd_scroll, tty->TS_del_multi_lines,
                                          f, f, 1);
      else
        do_line_insertion_deletion_costs (frame,
                                          tty->TS_ins_line, tty->TS_ins_multi_lines,
                                          tty->TS_del_line, tty->TS_del_multi_lines,
                                          0, 0, 1);

      calculate_ins_del_char_costs (frame);

      /* Don't use TS_repeat if its padding is worse than sending the chars */
      if (tty->TS_repeat && per_line_cost (tty->TS_repeat) * baud_rate < 9000)
        tty->RPov = string_cost (tty->TS_repeat);
      else
        tty->RPov = FRAME_COLS (frame) * 2;

      cmcostinit (FRAME_TTY (frame)); /* set up cursor motion costs */
    }
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



/***********************************************************************
				Faces
 ***********************************************************************/

/* Value is non-zero if attribute ATTR may be used.  ATTR should be
   one of the enumerators from enum no_color_bit, or a bit set built
   from them.  Some display attributes may not be used together with
   color; the termcap capability `NC' specifies which ones.  */

#define MAY_USE_WITH_COLORS_P(tty, ATTR)                \
  (tty->TN_max_colors > 0				\
   ? (tty->TN_no_color_video & (ATTR)) == 0             \
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
  struct tty_display_info *tty = FRAME_TTY (f);

  /* Do this first because TS_end_standout_mode may be the same
     as TS_exit_attribute_mode, which turns all appearances off. */
  if (MAY_USE_WITH_COLORS_P (tty, NC_REVERSE))
    {
      if (tty->TN_max_colors > 0)
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
		toggle_highlight (tty);
	    }
	  else
	    {
	      if (fg == FACE_TTY_DEFAULT_BG_COLOR
		  || bg == FACE_TTY_DEFAULT_FG_COLOR)
		toggle_highlight (tty);
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
		toggle_highlight (tty);
	    }
	  else
	    {
	      if (fg == FACE_TTY_DEFAULT_BG_COLOR
		  || bg == FACE_TTY_DEFAULT_FG_COLOR)
		toggle_highlight (tty);
	    }
	}
    }

  if (face->tty_bold_p)
    {
      if (MAY_USE_WITH_COLORS_P (tty, NC_BOLD))
	OUTPUT1_IF (tty, tty->TS_enter_bold_mode);
    }
  else if (face->tty_dim_p)
    if (MAY_USE_WITH_COLORS_P (tty, NC_DIM))
      OUTPUT1_IF (tty, tty->TS_enter_dim_mode);

  /* Alternate charset and blinking not yet used.  */
  if (face->tty_alt_charset_p
      && MAY_USE_WITH_COLORS_P (tty, NC_ALT_CHARSET))
    OUTPUT1_IF (tty, tty->TS_enter_alt_charset_mode);

  if (face->tty_blinking_p
      && MAY_USE_WITH_COLORS_P (tty, NC_BLINK))
    OUTPUT1_IF (tty, tty->TS_enter_blink_mode);

  if (face->tty_underline_p && MAY_USE_WITH_COLORS_P (tty, NC_UNDERLINE))
    OUTPUT1_IF (tty, tty->TS_enter_underline_mode);

  if (tty->TN_max_colors > 0)
    {
      char *p;

      if (fg >= 0 && tty->TS_set_foreground)
	{
	  p = tparam (tty->TS_set_foreground, NULL, 0, (int) fg);
	  OUTPUT (tty, p);
	  xfree (p);
	}

      if (bg >= 0 && tty->TS_set_background)
	{
	  p = tparam (tty->TS_set_background, NULL, 0, (int) bg);
	  OUTPUT (tty, p);
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
  struct tty_display_info *tty = FRAME_TTY (f);

  xassert (face != NULL);

  if (tty->TS_exit_attribute_mode)
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
	  OUTPUT1_IF (tty, tty->TS_exit_attribute_mode);
	  if (strcmp (tty->TS_exit_attribute_mode, tty->TS_end_standout_mode) == 0)
	    tty->standout_mode = 0;
	}

      if (face->tty_alt_charset_p)
	OUTPUT_IF (tty, tty->TS_exit_alt_charset_mode);
    }
  else
    {
      /* If we don't have "me" we can only have those appearances
	 that have exit sequences defined.  */
      if (face->tty_alt_charset_p)
	OUTPUT_IF (tty, tty->TS_exit_alt_charset_mode);

      if (face->tty_underline_p)
	OUTPUT_IF (tty, tty->TS_exit_underline_mode);
    }

  /* Switch back to default colors.  */
  if (tty->TN_max_colors > 0
      && ((face->foreground != FACE_TTY_DEFAULT_COLOR
	   && face->foreground != FACE_TTY_DEFAULT_FG_COLOR)
	  || (face->background != FACE_TTY_DEFAULT_COLOR
	      && face->background != FACE_TTY_DEFAULT_BG_COLOR)))
    OUTPUT1_IF (tty, tty->TS_orig_pair);
}


/* Return non-zero if the terminal on frame F supports all of the
   capabilities in CAPS simultaneously, with foreground and background
   colors FG and BG.  */

int
tty_capable_p (tty, caps, fg, bg)
     struct tty_display_info *tty;
     unsigned caps;
     unsigned long fg, bg;
{
#define TTY_CAPABLE_P_TRY(tty, cap, TS, NC_bit)				\
  if ((caps & (cap)) && (!(TS) || !MAY_USE_WITH_COLORS_P(tty, NC_bit)))	\
    return 0;

  TTY_CAPABLE_P_TRY (tty, TTY_CAP_INVERSE,	tty->TS_standout_mode, 	 	NC_REVERSE);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_UNDERLINE, 	tty->TS_enter_underline_mode, 	NC_UNDERLINE);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_BOLD, 	tty->TS_enter_bold_mode, 	NC_BOLD);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_DIM, 		tty->TS_enter_dim_mode, 	NC_DIM);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_BLINK, 	tty->TS_enter_blink_mode, 	NC_BLINK);
  TTY_CAPABLE_P_TRY (tty, TTY_CAP_ALT_CHARSET, 	tty->TS_enter_alt_charset_mode, NC_ALT_CHARSET);

  /* We can do it!  */
  return 1;
}

/* Return the tty display object specified by DISPLAY.
   DISPLAY may be a frame or a string. */

static struct display *
get_tty_display (Lisp_Object display)
{
  struct display *d;

  if (! FRAMEP (display) && ! STRINGP (display))
    return 0;

  /* The initial frame does not support colors. */
  if (FRAMEP (display) && FRAME_INITIAL_P (XFRAME (display)))
    return 0;

  if (FRAMEP (display))
    {
      if (! FRAME_TERMCAP_P (XFRAME (display)))
#if 0   /* XXX We need a predicate as the first argument; find one. */
        wrong_type_argument ("Not a termcap frame", display);
#else /* Until we fix the wrong_type_argument call above, simply throw
         a dumb error. */
      error ("DISPLAY is not a termcap frame");
#endif  
  
      d = FRAME_DISPLAY (XFRAME (display));
    }
  else if (STRINGP (display))
    {
      char *name = (char *) alloca (SBYTES (display) + 1);
      strncpy (name, SDATA (display), SBYTES (display));
      name[SBYTES (display)] = 0;

      d = get_named_tty_display (name);

      if (!d)
        error ("There is no tty display on %s", name);
    }

  return d;
}


/* Return non-zero if the terminal is capable to display colors.  */

DEFUN ("tty-display-color-p", Ftty_display_color_p, Stty_display_color_p,
       0, 1, 0,
       doc: /* Return non-nil if TTY can display colors on DISPLAY.  */)
     (display)
     Lisp_Object display;
{
  struct display *d = get_tty_display (display);
  if (!d)
    return Qnil;
  else
    return d->display_info.tty->TN_max_colors > 0 ? Qt : Qnil;
}

/* Return the number of supported colors.  */
DEFUN ("tty-display-color-cells", Ftty_display_color_cells,
       Stty_display_color_cells, 0, 1, 0,
       doc: /* Return the number of colors supported by TTY on DISPLAY.  */)
     (display)
     Lisp_Object display;
{
  struct display *d = get_tty_display (display);
  if (!d)
    return Qnil;
  else
    return make_number (d->display_info.tty->TN_max_colors);
}

#ifndef WINDOWSNT

/* Save or restore the default color-related capabilities of this
   terminal.  */
static void
tty_default_color_capabilities (struct tty_display_info *tty, int save)
{
  static char
    *default_orig_pair, *default_set_foreground, *default_set_background;
  static int default_max_colors, default_max_pairs, default_no_color_video;

  if (save)
    {
      if (default_orig_pair)
	xfree (default_orig_pair);
      default_orig_pair = tty->TS_orig_pair ? xstrdup (tty->TS_orig_pair) : NULL;

      if (default_set_foreground)
	xfree (default_set_foreground);
      default_set_foreground = tty->TS_set_foreground ? xstrdup (tty->TS_set_foreground)
			       : NULL;

      if (default_set_background)
	xfree (default_set_background);
      default_set_background = tty->TS_set_background ? xstrdup (tty->TS_set_background)
			       : NULL;

      default_max_colors = tty->TN_max_colors;
      default_max_pairs = tty->TN_max_pairs;
      default_no_color_video = tty->TN_no_color_video;
    }
  else
    {
      tty->TS_orig_pair = default_orig_pair;
      tty->TS_set_foreground = default_set_foreground;
      tty->TS_set_background = default_set_background;
      tty->TN_max_colors = default_max_colors;
      tty->TN_max_pairs = default_max_pairs;
      tty->TN_no_color_video = default_no_color_video;
    }
}

/* Setup one of the standard tty color schemes according to MODE.
   MODE's value is generally the number of colors which we want to
   support; zero means set up for the default capabilities, the ones
   we saw at term_init time; -1 means turn off color support.  */
void
tty_setup_colors (struct tty_display_info *tty, int mode)
{
  /* Canonicalize all negative values of MODE.  */
  if (mode < -1)
    mode = -1;

  switch (mode)
    {
      case -1:	 /* no colors at all */
	tty->TN_max_colors = 0;
	tty->TN_max_pairs = 0;
	tty->TN_no_color_video = 0;
	tty->TS_set_foreground = tty->TS_set_background = tty->TS_orig_pair = NULL;
	break;
      case 0:	 /* default colors, if any */
      default:
	tty_default_color_capabilities (tty, 0);
	break;
      case 8:	/* 8 standard ANSI colors */
	tty->TS_orig_pair = "\033[0m";
#ifdef TERMINFO
	tty->TS_set_foreground = "\033[3%p1%dm";
	tty->TS_set_background = "\033[4%p1%dm";
#else
	tty->TS_set_foreground = "\033[3%dm";
	tty->TS_set_background = "\033[4%dm";
#endif
	tty->TN_max_colors = 8;
	tty->TN_max_pairs = 64;
	tty->TN_no_color_video = 0;
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

  if (INTEGERP (val))
    color_mode = val;
  else
    {
      if (NILP (tty_color_mode_alist))
	color_mode_spec = Qnil;
      else
	color_mode_spec = Fassq (val, XSYMBOL (tty_color_mode_alist)->value);

      if (CONSP (color_mode_spec))
	color_mode = XCDR (color_mode_spec);
      else
	color_mode = Qnil;
    }

  current_mode_spec = assq_no_quit (Qtty_color_mode, f->param_alist);

  if (CONSP (current_mode_spec))
    current_mode = XCDR (current_mode_spec);
  else
    current_mode = Qnil;
  if (INTEGERP (color_mode))
    mode = XINT (color_mode);
  else
    mode = 0;	/* meaning default */
  if (INTEGERP (current_mode))
    old_mode = XINT (current_mode);
  else
    old_mode = 0;

  if (mode != old_mode)
    {
      tty_setup_colors (FRAME_TTY (f), mode);
      /*  This recomputes all the faces given the new color
	  definitions.  */
      call0 (intern ("tty-set-up-initial-frame-faces"));
      redraw_frame (f);
    }
}

#endif /* !WINDOWSNT */



struct display *
get_named_tty_display (name)
     char *name;
{
  struct display *d;

  for (d = display_list; d; d = d->next_display) {
    if (d->type == output_termcap
        && ((d->display_info.tty->name == 0 && name == 0)
            || (name && d->display_info.tty->name
                && !strcmp (d->display_info.tty->name, name))))
      return d;
  };

  return 0;
}



DEFUN ("frame-tty-name", Fframe_tty_name, Sframe_tty_name, 0, 1, 0,
       doc: /* Return the name of the TTY device that FRAME is displayed on. */)
  (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    {
      f = XFRAME (selected_frame);
    }
  else
    {
      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
    }

  if (f->output_method != output_termcap)
    wrong_type_argument (Qframe_tty_name, frame);

  if (FRAME_TTY (f)->name)
    return build_string (FRAME_TTY (f)->name);
  else
    return Qnil;
}

DEFUN ("frame-tty-type", Fframe_tty_type, Sframe_tty_type, 0, 1, 0,
       doc: /* Return the type of the TTY device that FRAME is displayed on. */)
  (frame)
     Lisp_Object frame;
{
  struct frame *f;

  if (NILP (frame))
    {
      f = XFRAME (selected_frame);
    }
  else
    {
      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
    }

  if (f->output_method != output_termcap)
    wrong_type_argument (Qframe_tty_type, frame);

  if (FRAME_TTY (f)->type)
    return build_string (FRAME_TTY (f)->type);
  else
    return Qnil;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Create the bootstrap display device for the initial frame.

Returns a display of type output_initial. */
struct display *
init_initial_display (void)
{
  struct tty_display_info *tty;
  
  if (initialized || display_list || tty_list)
    abort ();

  initial_display = create_display ();
  initial_display->type = output_initial;
  
  initial_display->delete_display_hook = &delete_initial_display;
  /* All other hooks are NULL. */
  
  return initial_display;
}

/* Deletes the bootstrap display device.
   Called through delete_display_hook. */
void
delete_initial_display (struct display *display)
{
  if (display != initial_display)
    abort ();

  delete_display (display);
  initial_display = NULL;
}

/* Create a termcap display on the tty device with the given name and
   type.

   If NAME is NULL, then use the controlling tty, i.e., stdin/stdout.
   Otherwise NAME should be a path to the tty device file,
   e.g. "/dev/pts/7".

   TERMINAL_TYPE is the termcap type of the device, e.g. "vt100".

   If MUST_SUCCEED is true, then all errors are fatal. */
struct display *
term_init (char *name, char *terminal_type, int must_succeed)
{
  char *area;
  char **address = &area;
  char *buffer = NULL;
  int buffer_size = 4096;
  register char *p;
  int status;
  struct tty_display_info *tty;
  struct display *display;

  static void maybe_fatal();

  if (!terminal_type)
    maybe_fatal (must_succeed, 0, 0,
                 "Unknown terminal type",
                 "Unknown terminal type");
  
  display = get_named_tty_display (name);
  if (display)
    return display;             /* We have already opened a display there. */

  display = create_display ();
  tty = (struct tty_display_info *) xmalloc (sizeof (struct tty_display_info));
  bzero (tty, sizeof (struct tty_display_info));
  tty->next = tty_list;
  tty_list = tty;

  display->type = output_termcap;
  display->display_info.tty = tty;
  tty->display = display;

  tty->Wcm = (struct cm *) xmalloc (sizeof (struct cm));
  Wcm_clear (tty);

  display->rif = 0; /* ttys don't support window-based redisplay. */

  display->cursor_to_hook = &tty_cursor_to;
  display->raw_cursor_to_hook = &tty_raw_cursor_to;

  display->clear_to_end_hook = &tty_clear_to_end;
  display->clear_frame_hook = &tty_clear_frame;
  display->clear_end_of_line_hook = &tty_clear_end_of_line;

  display->ins_del_lines_hook = &tty_ins_del_lines;

  display->insert_glyphs_hook = &tty_insert_glyphs;
  display->write_glyphs_hook = &tty_write_glyphs;
  display->delete_glyphs_hook = &tty_delete_glyphs;

  display->ring_bell_hook = &tty_ring_bell;
  
  display->reset_terminal_modes_hook = &tty_reset_terminal_modes;
  display->set_terminal_modes_hook = &tty_set_terminal_modes;
  display->update_begin_hook = 0; /* Not needed. */
  display->update_end_hook = &tty_update_end;
  display->set_terminal_window_hook = &tty_set_terminal_window;

  display->mouse_position_hook = 0; /* Not needed. */
  display->frame_rehighlight_hook = 0; /* Not needed. */
  display->frame_raise_lower_hook = 0; /* Not needed. */

  display->set_vertical_scroll_bar_hook = 0; /* Not needed. */
  display->condemn_scroll_bars_hook = 0; /* Not needed. */
  display->redeem_scroll_bar_hook = 0; /* Not needed. */
  display->judge_scroll_bars_hook = 0; /* Not needed. */

  display->read_socket_hook = &tty_read_avail_input; /* keyboard.c */
  display->frame_up_to_date_hook = 0; /* Not needed. */
  
  display->delete_frame_hook = &delete_tty_output;
  display->delete_display_hook = &delete_tty;
  
  if (name)
    {
      int fd;
      FILE *file;
      fd = emacs_open (name, O_RDWR, 0);
      if (fd < 0)
        {
          delete_tty (display);
          error ("Could not open file: %s", name);
        }
      file = fdopen (fd, "w+");
      tty->name = xstrdup (name);
      tty->input = file;
      tty->output = file;
    }
  else
    {
      tty->name = 0;
      tty->input = stdin;
      tty->output = stdout;
    }

  tty->type = xstrdup (terminal_type);

  add_keyboard_wait_descriptor (fileno (tty->input));

#ifdef WINDOWSNT
  initialize_w32_display ();

  Wcm_clear (tty);

  area = (char *) xmalloc (2044);

  FrameRows (tty) = FRAME_LINES (f); /* XXX */
  FrameCols (tty) = FRAME_COLS (f);  /* XXX */
  tty->specified_window = FRAME_LINES (f); /* XXX */

  tty->display->delete_in_insert_mode = 1;

  UseTabs (tty) = 0;
  display->scroll_region_ok = 0;

  /* Seems to insert lines when it's not supposed to, messing
     up the display.  In doing a trace, it didn't seem to be
     called much, so I don't think we're losing anything by
     turning it off.  */
  display->line_ins_del_ok = 0;
  display->char_ins_del_ok = 1;

  baud_rate = 19200;

  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0; /* XXX */
  FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = vertical_scroll_bar_none; /* XXX */
  TN_max_colors = 16;  /* Required to be non-zero for tty-display-color-p */

  return display;
#else  /* not WINDOWSNT */

  Wcm_clear (tty);

  buffer = (char *) xmalloc (buffer_size);
  status = tgetent (buffer, terminal_type);
  if (status < 0)
    {
#ifdef TERMINFO
      maybe_fatal (must_succeed, buffer, display,
                   "Cannot open terminfo database file",
                   "Cannot open terminfo database file");
#else
      maybe_fatal (must_succeed, buffer, display,
                   "Cannot open termcap database file",
                   "Cannot open termcap database file");
#endif
    }
  if (status == 0)
    {
#ifdef TERMINFO
      maybe_fatal (must_succeed, buffer, display,
                   "Terminal type %s is not defined",
                   "Terminal type %s is not defined.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMINFO' (C-shell: `unsetenv TERMINFO') as well.",
                   terminal_type);
#else
      maybe_fatal (must_succeed, buffer, display,
                   "Terminal type %s is not defined",
                   "Terminal type %s is not defined.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMCAP' (C-shell: `unsetenv TERMCAP') as well.",
                   terminal_type);
#endif
    }

#ifndef TERMINFO
  if (strlen (buffer) >= buffer_size)
    abort ();
  buffer_size = strlen (buffer);
#endif
  area = (char *) xmalloc (buffer_size);

  tty->TS_ins_line = tgetstr ("al", address);
  tty->TS_ins_multi_lines = tgetstr ("AL", address);
  tty->TS_bell = tgetstr ("bl", address);
  BackTab (tty) = tgetstr ("bt", address);
  tty->TS_clr_to_bottom = tgetstr ("cd", address);
  tty->TS_clr_line = tgetstr ("ce", address);
  tty->TS_clr_frame = tgetstr ("cl", address);
  ColPosition (tty) = NULL; /* tgetstr ("ch", address); */
  AbsPosition (tty) = tgetstr ("cm", address);
  CR (tty) = tgetstr ("cr", address);
  tty->TS_set_scroll_region = tgetstr ("cs", address);
  tty->TS_set_scroll_region_1 = tgetstr ("cS", address);
  RowPosition (tty) = tgetstr ("cv", address);
  tty->TS_del_char = tgetstr ("dc", address);
  tty->TS_del_multi_chars = tgetstr ("DC", address);
  tty->TS_del_line = tgetstr ("dl", address);
  tty->TS_del_multi_lines = tgetstr ("DL", address);
  tty->TS_delete_mode = tgetstr ("dm", address);
  tty->TS_end_delete_mode = tgetstr ("ed", address);
  tty->TS_end_insert_mode = tgetstr ("ei", address);
  Home (tty) = tgetstr ("ho", address);
  tty->TS_ins_char = tgetstr ("ic", address);
  tty->TS_ins_multi_chars = tgetstr ("IC", address);
  tty->TS_insert_mode = tgetstr ("im", address);
  tty->TS_pad_inserted_char = tgetstr ("ip", address);
  tty->TS_end_keypad_mode = tgetstr ("ke", address);
  tty->TS_keypad_mode = tgetstr ("ks", address);
  LastLine (tty) = tgetstr ("ll", address);
  Right (tty) = tgetstr ("nd", address);
  Down (tty) = tgetstr ("do", address);
  if (!Down (tty))
    Down (tty) = tgetstr ("nl", address); /* Obsolete name for "do" */
#ifdef VMS
  /* VMS puts a carriage return before each linefeed,
     so it is not safe to use linefeeds.  */
  if (Down (tty) && Down (tty)[0] == '\n' && Down (tty)[1] == '\0')
    Down (tty) = 0;
#endif /* VMS */
  if (tgetflag ("bs"))
    Left (tty) = "\b";		  /* can't possibly be longer! */
  else				  /* (Actually, "bs" is obsolete...) */
    Left (tty) = tgetstr ("le", address);
  if (!Left (tty))
    Left (tty) = tgetstr ("bc", address); /* Obsolete name for "le" */
  tty->TS_pad_char = tgetstr ("pc", address);
  tty->TS_repeat = tgetstr ("rp", address);
  tty->TS_end_standout_mode = tgetstr ("se", address);
  tty->TS_fwd_scroll = tgetstr ("sf", address);
  tty->TS_standout_mode = tgetstr ("so", address);
  tty->TS_rev_scroll = tgetstr ("sr", address);
  tty->Wcm->cm_tab = tgetstr ("ta", address);
  tty->TS_end_termcap_modes = tgetstr ("te", address);
  tty->TS_termcap_modes = tgetstr ("ti", address);
  Up (tty) = tgetstr ("up", address);
  tty->TS_visible_bell = tgetstr ("vb", address);
  tty->TS_cursor_normal = tgetstr ("ve", address);
  tty->TS_cursor_visible = tgetstr ("vs", address);
  tty->TS_cursor_invisible = tgetstr ("vi", address);
  tty->TS_set_window = tgetstr ("wi", address);

  tty->TS_enter_underline_mode = tgetstr ("us", address);
  tty->TS_exit_underline_mode = tgetstr ("ue", address);
  tty->TS_enter_bold_mode = tgetstr ("md", address);
  tty->TS_enter_dim_mode = tgetstr ("mh", address);
  tty->TS_enter_blink_mode = tgetstr ("mb", address);
  tty->TS_enter_reverse_mode = tgetstr ("mr", address);
  tty->TS_enter_alt_charset_mode = tgetstr ("as", address);
  tty->TS_exit_alt_charset_mode = tgetstr ("ae", address);
  tty->TS_exit_attribute_mode = tgetstr ("me", address);

  MultiUp (tty) = tgetstr ("UP", address);
  MultiDown (tty) = tgetstr ("DO", address);
  MultiLeft (tty) = tgetstr ("LE", address);
  MultiRight (tty) = tgetstr ("RI", address);

  /* SVr4/ANSI color suppert.  If "op" isn't available, don't support
     color because we can't switch back to the default foreground and
     background.  */
  tty->TS_orig_pair = tgetstr ("op", address);
  if (tty->TS_orig_pair)
    {
      tty->TS_set_foreground = tgetstr ("AF", address);
      tty->TS_set_background = tgetstr ("AB", address);
      if (!tty->TS_set_foreground)
	{
	  /* SVr4.  */
	  tty->TS_set_foreground = tgetstr ("Sf", address);
	  tty->TS_set_background = tgetstr ("Sb", address);
	}

      tty->TN_max_colors = tgetnum ("Co");
      tty->TN_max_pairs = tgetnum ("pa");

      tty->TN_no_color_video = tgetnum ("NC");
      if (tty->TN_no_color_video == -1)
        tty->TN_no_color_video = 0;
    }

  tty_default_color_capabilities (tty, 1);

  MagicWrap (tty) = tgetflag ("xn");
  /* Since we make MagicWrap terminals look like AutoWrap, we need to have
     the former flag imply the latter.  */
  AutoWrap (tty) = MagicWrap (tty) || tgetflag ("am");
  display->memory_below_frame = tgetflag ("db");
  tty->TF_hazeltine = tgetflag ("hz");
  display->must_write_spaces = tgetflag ("in");
  tty->meta_key = tgetflag ("km") || tgetflag ("MT");
  tty->TF_insmode_motion = tgetflag ("mi");
  tty->TF_standout_motion = tgetflag ("ms");
  tty->TF_underscore = tgetflag ("ul");
  tty->TF_teleray = tgetflag ("xt");

  term_get_fkeys (address);

  /* Get frame size from system, or else from termcap.  */
  {
    int height, width;
    get_tty_size (fileno (TTY_INPUT (tty)), &width, &height);
    FrameCols (tty) = width;
    FrameRows (tty) = height;
  }

  if (FrameCols (tty) <= 0)
    FrameCols (tty) = tgetnum ("co");
  if (FrameRows (tty) <= 0)
    FrameRows (tty) = tgetnum ("li");

  if (FrameRows (tty) < 3 || FrameCols (tty) < 3)
    maybe_fatal (must_succeed, NULL, display,
                 "Screen size %dx%d is too small"
                 "Screen size %dx%d is too small",
                 FrameCols (tty), FrameRows (tty));

#if 0  /* This is not used anywhere. */
  tty->display->min_padding_speed = tgetnum ("pb");
#endif

  TabWidth (tty) = tgetnum ("tw");

#ifdef VMS
  /* These capabilities commonly use ^J.
     I don't know why, but sending them on VMS does not work;
     it causes following spaces to be lost, sometimes.
     For now, the simplest fix is to avoid using these capabilities ever.  */
  if (Down (tty) && Down (tty)[0] == '\n')
    Down (tty) = 0;
#endif /* VMS */

  if (!tty->TS_bell)
    tty->TS_bell = "\07";

  if (!tty->TS_fwd_scroll)
    tty->TS_fwd_scroll = Down (tty);

  PC = tty->TS_pad_char ? *tty->TS_pad_char : 0;

  if (TabWidth (tty) < 0)
    TabWidth (tty) = 8;

/* Turned off since /etc/termcap seems to have :ta= for most terminals
   and newer termcap doc does not seem to say there is a default.
  if (!tty->Wcm->cm_tab)
    tty->Wcm->cm_tab = "\t";
*/

  /* We don't support standout modes that use `magic cookies', so
     turn off any that do.  */
  if (tty->TS_standout_mode && tgetnum ("sg") >= 0)
    {
      tty->TS_standout_mode = 0;
      tty->TS_end_standout_mode = 0;
    }
  if (tty->TS_enter_underline_mode && tgetnum ("ug") >= 0)
    {
      tty->TS_enter_underline_mode = 0;
      tty->TS_exit_underline_mode = 0;
    }

  /* If there's no standout mode, try to use underlining instead.  */
  if (tty->TS_standout_mode == 0)
    {
      tty->TS_standout_mode = tty->TS_enter_underline_mode;
      tty->TS_end_standout_mode = tty->TS_exit_underline_mode;
    }

  /* If no `se' string, try using a `me' string instead.
     If that fails, we can't use standout mode at all.  */
  if (tty->TS_end_standout_mode == 0)
    {
      char *s = tgetstr ("me", address);
      if (s != 0)
        tty->TS_end_standout_mode = s;
      else
        tty->TS_standout_mode = 0;
    }

  if (tty->TF_teleray)
    {
      tty->Wcm->cm_tab = 0;
      /* We can't support standout mode, because it uses magic cookies.  */
      tty->TS_standout_mode = 0;
      /* But that means we cannot rely on ^M to go to column zero! */
      CR (tty) = 0;
      /* LF can't be trusted either -- can alter hpos */
      /* if move at column 0 thru a line with TS_standout_mode */
      Down (tty) = 0;
    }

  /* Special handling for certain terminal types known to need it */

  if (!strcmp (terminal_type, "supdup"))
    {
      display->memory_below_frame = 1;
      tty->Wcm->cm_losewrap = 1;
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
      if (!tty->TS_set_window)
	{
	  p = tty->TS_termcap_modes;
	  while (*p && strcmp (p, "\033v  "))
	    p++;
	  if (*p)
	    tty->TS_set_window = "\033v%C %C %C %C ";
	}
      /* Termcap entry often fails to have :in: flag */
      display->must_write_spaces = 1;
      /* :ti string typically fails to have \E^G! in it */
      /* This limits scope of insert-char to one line.  */
      strcpy (area, tty->TS_termcap_modes);
      strcat (area, "\033\007!");
      tty->TS_termcap_modes = area;
      area += strlen (area) + 1;
      p = AbsPosition (tty);
      /* Change all %+ parameters to %C, to handle
         values above 96 correctly for the C100.  */
      while (*p)
        {
          if (p[0] == '%' && p[1] == '+')
            p[1] = 'C';
          p++;
        }
    }

  tty->specified_window = FrameRows (tty);

  if (Wcm_init (tty) == -1)	/* can't do cursor motion */
    {
      maybe_fatal (must_succeed, NULL, display,
                   "Terminal type \"%s\" is not powerful enough to run Emacs",
#ifdef VMS
                   "Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have, use either the\n\
DCL command `SET TERMINAL/DEVICE= ...' for DEC-compatible terminals,\n\
or `define EMACS_TERM \"terminal type\"' for non-DEC terminals.",
#else /* not VMS */
# ifdef TERMINFO
                   "Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMINFO' (C-shell: `unsetenv TERMINFO') as well.",
# else /* TERMCAP */
                   "Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have,\n\
use the Bourne shell command `TERM=... export TERM' (C-shell:\n\
`setenv TERM ...') to specify the correct type.  It may be necessary\n\
to do `unset TERMCAP' (C-shell: `unsetenv TERMCAP') as well.",
# endif /* TERMINFO */
#endif /*VMS */
                   terminal_type);
    }

  if (FrameRows (tty) <= 0 || FrameCols (tty) <= 0)
    maybe_fatal (must_succeed, NULL, display,
                 "Could not determine the frame size",
                 "Could not determine the frame size");

  tty->delete_in_insert_mode
    = tty->TS_delete_mode && tty->TS_insert_mode
    && !strcmp (tty->TS_delete_mode, tty->TS_insert_mode);

  tty->se_is_so = (tty->TS_standout_mode
              && tty->TS_end_standout_mode
              && !strcmp (tty->TS_standout_mode, tty->TS_end_standout_mode));

  UseTabs (tty) = tabs_safe_p (fileno (TTY_INPUT (tty))) && TabWidth (tty) == 8;

  display->scroll_region_ok
    = (tty->Wcm->cm_abs
       && (tty->TS_set_window || tty->TS_set_scroll_region || tty->TS_set_scroll_region_1));

  display->line_ins_del_ok
    = (((tty->TS_ins_line || tty->TS_ins_multi_lines)
        && (tty->TS_del_line || tty->TS_del_multi_lines))
       || (display->scroll_region_ok
           && tty->TS_fwd_scroll && tty->TS_rev_scroll));

  display->char_ins_del_ok
    = ((tty->TS_ins_char || tty->TS_insert_mode
        || tty->TS_pad_inserted_char || tty->TS_ins_multi_chars)
       && (tty->TS_del_char || tty->TS_del_multi_chars));

  display->fast_clear_end_of_line = tty->TS_clr_line != 0;

  init_baud_rate (fileno (TTY_INPUT (tty)));

#ifdef AIXHFT
  /* The HFT system on AIX doesn't optimize for scrolling, so it's
     really ugly at times.  */
  display->line_ins_del_ok = 0;
  display->char_ins_del_ok = 0;
#endif

#ifdef MULTI_KBOARD
  tty->kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
  init_kboard (tty->kboard);
  tty->kboard->next_kboard = all_kboards;
  all_kboards = tty->kboard;
  /* Don't let the initial kboard remain current longer than necessary.
     That would cause problems if a file loaded on startup tries to
     prompt in the mini-buffer.  */
  if (current_kboard == initial_kboard)
    current_kboard = tty->kboard;
  tty->kboard->reference_count++;
#endif

  /* Don't do this.  I think termcap may still need the buffer. */
  /* xfree (buffer); */

  /* Init system terminal modes (RAW or CBREAK, etc.).  */
  init_sys_modes (tty);

  return display;
#endif /* not WINDOWSNT */
}

/* Auxiliary error-handling function for term_init.
   Free BUFFER and delete DISPLAY, then call error or fatal
   with str1 or str2, respectively, according to MUST_SUCCEED.
*/
static void
maybe_fatal (must_succeed, buffer, display, str1, str2, arg1, arg2)
     int must_succeed;
     char *buffer;
     struct display *display;
     char *str1, *str2, *arg1, *arg2;
{
  if (buffer)
    xfree (buffer);

  if (display)
    delete_tty (display);
  
  if (must_succeed)
    fatal (str2, arg1, arg2);
  else
    error (str1, arg1, arg2);

  abort ();
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



DEFUN ("delete-tty", Fdelete_tty, Sdelete_tty, 0, 1, 0,
       doc: /* Delete all frames on the terminal named TTY, and close the device.
If omitted, TTY defaults to the controlling terminal.

This function runs `delete-tty-after-functions' after closing the
tty.  The functions are run with one arg, the frame to be deleted.  */)
  (tty)
     Lisp_Object tty;
{
  struct display *d;
  char *name = 0;

  CHECK_STRING (tty);

  if (SBYTES (tty) > 0)
    {
      name = (char *) alloca (SBYTES (tty) + 1);
      strncpy (name, SDATA (tty), SBYTES (tty));
      name[SBYTES (tty)] = 0;
    }

  d = get_named_tty_display (name);

  if (! d)
    error ("No such terminal device: %s", name);

  delete_tty (d);
}

static int deleting_tty = 0;

void
delete_tty (struct display *display)
{
  struct tty_display_info *tty;
  Lisp_Object tail, frame;
  char *tty_name;
  
  if (deleting_tty)
    /* We get a recursive call when we delete the last frame on this
       display. */
    return;

  deleting_tty = 1;

  if (display->type != output_termcap)
    abort ();

  tty = display->display_info.tty;
  
  if (tty == tty_list)
    tty_list = tty->next;
  else
    {
      struct tty_display_info *p;
      for (p = tty_list; p && p->next != tty; p = p->next)
        ;

      if (! p)
        /* This should not happen. */
        abort ();

      p->next = tty->next;
      tty->next = 0;
    }

  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_TERMCAP_P (f) && FRAME_LIVE_P (f) && FRAME_TTY (f) == tty)
        {
          Fdelete_frame (frame, Qt);
          f->output_data.tty = 0;
        }
    }

  /* reset_sys_modes needs a valid display, so this call needs to be
     before delete_display. */
  reset_sys_modes (tty);

  delete_display (display);

  tty_name = tty->name;
  if (tty->type)
    xfree (tty->type);

  if (tty->input)
    {
      delete_keyboard_wait_descriptor (fileno (tty->input));
      if (tty->input != stdin)
        fclose (tty->input);
    }
  if (tty->output && tty->output != stdout && tty->output != tty->input)
    fclose (tty->output);
  if (tty->termscript)
    fclose (tty->termscript);

  if (tty->old_tty)
    xfree (tty->old_tty);

  if (tty->Wcm)
    xfree (tty->Wcm);

#ifdef MULTI_KBOARD
  if (tty->kboard && --tty->kboard->reference_count > 0)
    abort ();
  if (tty->kboard)
    delete_kboard (tty->kboard);
#endif
  
  bzero (tty, sizeof (struct tty_display_info));
  xfree (tty);
  deleting_tty = 0;

  /* Run `delete-tty-after-functions'.  */
  if (!NILP (Vrun_hooks))
    {
      Lisp_Object args[2];
      args[0] = intern ("delete-tty-after-functions");
      if (tty_name)
        {
          args[1] = build_string (tty_name);
          xfree (tty_name);
        }
      else
        args[1] = Qnil;
      Frun_hook_with_args (2, args);
    }
}



/* Initialize the tty-dependent part of frame F.  The frame must
   already have its display initialized. */
void
create_tty_output (struct frame *f)
{
  if (! FRAME_TERMCAP_P (f))
    abort ();

  struct tty_output *t = xmalloc (sizeof (struct tty_output));
  bzero (t, sizeof (struct tty_output));

  t->display_info = FRAME_DISPLAY (f)->display_info.tty;

  f->output_data.tty = t;
}

/* Delete the tty-dependent part of frame F. */
void
delete_tty_output (struct frame *f)
{
  if (! FRAME_TERMCAP_P (f))
    abort ();

  xfree (f->output_data.tty);
}




/* Mark the pointers in the tty_display_info objects.
   Called by the Fgarbage_collector.  */
void
mark_ttys ()
{
  struct tty_display_info *tty;
  Lisp_Object *p;
  for (tty = tty_list; tty; tty = tty->next)
    {
      if (tty->top_frame)
        mark_object (tty->top_frame);
    }
}



/* Create a new display object and add it to the display list. */
struct display *
create_display (void)
{
  struct display *dev = (struct display *) xmalloc (sizeof (struct display));
  
  bzero (dev, sizeof (struct display));
  dev->next_display = display_list;
  display_list = dev;

  return dev;
}

/* Remove a display from the display list and free its memory. */
void
delete_display (struct display *dev)
{
  struct display **dp;
  for (dp = &display_list; *dp != dev; dp = &(*dp)->next_display)
    if (! *dp)
      abort ();
  *dp = dev->next_display;

  bzero (dev, sizeof (struct display));
  xfree (dev);
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

  DEFVAR_LISP ("delete-tty-after-functions", &Vdelete_tty_after_functions,
    doc: /* Functions to be run after deleting a tty.
The functions are run with one argument, the name of the tty to be deleted.
See `delete-tty'.  */);
  Vdelete_tty_after_functions = Qnil;

  Qframe_tty_name = intern ("frame-tty-name");
  staticpro (&Qframe_tty_name);

  Qframe_tty_type = intern ("frame-tty-type");
  staticpro (&Qframe_tty_type);

  defsubr (&Stty_display_color_p);
  defsubr (&Stty_display_color_cells);
  defsubr (&Sframe_tty_name);
  defsubr (&Sframe_tty_type);
  defsubr (&Sdelete_tty);

  Fprovide (intern ("multi-tty"), Qnil);

}



/* arch-tag: 498e7449-6f2e-45e2-91dd-b7d4ca488193
   (do not change this comment) */
