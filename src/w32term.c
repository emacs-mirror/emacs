/* Implementation of Win32 GUI terminal
   Copyright (C) 1989, 1993, 1994, 1995 Free Software Foundation, Inc.

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
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Added by Kevin Gallo */

#include <signal.h>
#include <config.h>
#include <stdio.h>
#include "lisp.h"
#include "blockinput.h"

#include <w32term.h>

#include "systty.h"
#include "systime.h"

#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>

#include "frame.h"
#include "dispextern.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "gnu.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"

extern void free_frame_menubar ();

extern Lisp_Object Vwindow_system;

#define x_any_window_to_frame x_window_to_frame
#define x_top_window_to_frame x_window_to_frame


/* This is display since win32 does not support multiple ones.  */
struct win32_display_info one_win32_display_info;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of win32_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
Lisp_Object win32_display_name_list;

/* Frame being updated by update_frame.  This is declared in term.c.
   This is set by update_begin and looked at by all the
   win32 functions.  It is zero while not inside an update.
   In that case, the win32 functions assume that `selected_frame'
   is the frame to apply to.  */
extern struct frame *updating_frame;

/* This is a frame waiting to be autoraised, within w32_read_socket.  */
struct frame *pending_autoraise_frame;

/* During an update, maximum vpos for ins/del line operations to affect.  */

static int flexlines;

/* During an update, nonzero if chars output now should be highlighted.  */

static int highlight;

/* Nominal cursor position -- where to draw output.
   During an update, these are different from the cursor-box position.  */

static int curs_x;
static int curs_y;

DWORD dwWinThreadId = 0;
HANDLE hWinThread = NULL;
DWORD dwMainThreadId = 0;
HANDLE hMainThread = NULL;

/* Mouse movement. */

/* Where the mouse was last time we reported a mouse event.  */
static FRAME_PTR last_mouse_frame;
static RECT last_mouse_glyph;

Lisp_Object Vwin32_num_mouse_buttons;

Lisp_Object Vwin32_swap_mouse_buttons;

/* The scroll bar in which the last motion event occurred.

   If the last motion event occurred in a scroll bar, we set this
   so win32_mouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last motion event didn't occur in a scroll bar, we set this
   to Qnil, to tell win32_mouse_position to return an ordinary motion event.  */
Lisp_Object last_mouse_scroll_bar;
int last_mouse_scroll_bar_pos;

/* This is a hack.  We would really prefer that win32_mouse_position would
   return the time associated with the position it returns, but there
   doesn't seem to be any way to wrest the timestamp from the server
   along with the position query.  So, we just keep track of the time
   of the last movement we received, and return that in hopes that
   it's somewhat accurate.  */
Time last_mouse_movement_time;

/* Incremented by w32_read_socket whenever it really tries to read events.  */
#ifdef __STDC__
static int volatile input_signal_count;
#else
static int input_signal_count;
#endif

extern Lisp_Object Vcommand_line_args, Vsystem_name;

extern Lisp_Object Qface, Qmouse_face;

extern int errno;

/* A mask of extra modifier bits to put into every keyboard char.  */
extern int extra_keyboard_modifiers;

static Lisp_Object Qvendor_specific_keysyms;

void win32_delete_display ();

static void redraw_previous_char ();
static void redraw_following_char ();
static unsigned int win32_get_modifiers ();

static int fast_find_position ();
static void note_mouse_highlight ();
static void clear_mouse_face ();
static void show_mouse_face ();
static void do_line_dance ();

static int win32_cursor_to ();
static int win32_clear_end_of_line ();

#if 0
/* This is a function useful for recording debugging information
   about the sequence of occurrences in this file.  */

struct record 
{
  char *locus;
  int type;
};

struct record event_record[100];

int event_record_index;

record_event (locus, type)
     char *locus;
     int type;
{
  if (event_record_index == sizeof (event_record) / sizeof (struct record))
    event_record_index = 0;

  event_record[event_record_index].locus = locus;
  event_record[event_record_index].type = type;
  event_record_index++;
}

#endif /* 0 */

/* Return the struct win32_display_info.  */

struct win32_display_info *
win32_display_info_for_display ()
{
  return (&one_win32_display_info);
}

void 
win32_fill_rect (f, _hdc, pix, lprect)
     FRAME_PTR f;
     HDC _hdc;
     COLORREF pix;
     RECT * lprect;
{
  HDC hdc;
  HBRUSH hb;
  RECT rect;
  
  if (_hdc)
    hdc = _hdc;
  else 
    {
      if (!f) return;
      hdc = get_frame_dc (f);
    }
  
  hb = CreateSolidBrush (pix);
  FillRect (hdc, lprect, hb);
  DeleteObject (hb);
  
  if (!_hdc)
    release_frame_dc (f, hdc);
}

void 
win32_clear_window (f)
     FRAME_PTR f;
{
  RECT rect;

  GetClientRect (FRAME_WIN32_WINDOW (f), &rect);
  win32_clear_rect (f, NULL, &rect);
}


/* Starting and ending updates.

   These hooks are called by update_frame at the beginning and end
   of a frame update.  We record in `updating_frame' the identity
   of the frame being updated, so that the win32_... functions do not
   need to take a frame as argument.  Most of the win32_... functions
   should never be called except during an update, the only exceptions
   being win32_cursor_to, win32_write_glyphs and win32_reassert_line_highlight.  */

static
win32_update_begin (f)
     struct frame *f;
{
  if (f == 0)
    abort ();

  flexlines = f->height;
  highlight = 0;

  BLOCK_INPUT;

  /* Regenerate display palette before drawing if list of requested
     colors has changed. */
  if (FRAME_WIN32_DISPLAY_INFO (f)->regen_palette)
  {
    win32_regenerate_palette (f);
    FRAME_WIN32_DISPLAY_INFO (f)->regen_palette = FALSE;
  }

  if (f == FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_defer = 1;

      /* If the frame needs to be redrawn,
	 simply forget about any prior mouse highlighting.  */
      if (FRAME_GARBAGED_P (f))
	FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_window = Qnil;

      if (!NILP (FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_window))
	{
	  int firstline, lastline, i;
	  struct window *w = XWINDOW (FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_window);

	  /* Find the first, and the last+1, lines affected by redisplay.  */
	  for (firstline = 0; firstline < f->height; firstline++)
	    if (FRAME_DESIRED_GLYPHS (f)->enable[firstline])
	      break;

	  lastline = f->height;
	  for (i = f->height - 1; i >= 0; i--)
	    {
	      if (FRAME_DESIRED_GLYPHS (f)->enable[i])
		break;
	      else
		lastline = i;
	    }

	  /* Can we tell that this update does not affect the window
	     where the mouse highlight is?  If so, no need to turn off.
	     Likewise, don't do anything if the frame is garbaged;
	     in that case, the FRAME_CURRENT_GLYPHS that we would use
	     are all wrong, and we will redisplay that line anyway.  */
	  if (! (firstline > (XFASTINT (w->top) + window_internal_height (w))
		 || lastline < XFASTINT (w->top)))
	    clear_mouse_face (FRAME_WIN32_DISPLAY_INFO (f));
	}
    }

  UNBLOCK_INPUT;
}

static
win32_update_end (f)
     struct frame *f;
{
  BLOCK_INPUT;

  do_line_dance ();
  x_display_cursor (f, 1);

  if (f == FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_defer = 0;

  UNBLOCK_INPUT;
}

/* This is called after a redisplay on frame F.  */

static
win32_frame_up_to_date (f)
     FRAME_PTR f;
{
  if (FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_deferred_gc
      || f == FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    {
      note_mouse_highlight (FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_frame,
			    FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_x,
			    FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_y);
      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_deferred_gc = 0;
    }
}

/* External interface to control of standout mode.
   Call this when about to modify line at position VPOS
   and not change whether it is highlighted.  */

win32_reassert_line_highlight (new, vpos)
     int new, vpos;
{
  highlight = new;
}

/* Call this when about to modify line at position VPOS
   and change whether it is highlighted.  */

static
win32_change_line_highlight (new_highlight, vpos, first_unused_hpos)
     int new_highlight, vpos, first_unused_hpos;
{
  highlight = new_highlight;
  win32_cursor_to (vpos, 0);
  win32_clear_end_of_line (updating_frame->width);
}

/* This is used when starting Emacs and when restarting after suspend.
   When starting Emacs, no window is mapped.  And nothing must be done
   to Emacs's own window if it is suspended (though that rarely happens).  */

static
win32_set_terminal_modes ()
{
}

/* This is called when exiting or suspending Emacs.
   Exiting will make the Win32 windows go away, and suspending
   requires no action.  */

static
win32_reset_terminal_modes ()
{
}

/* Set the nominal cursor position of the frame.
   This is where display update commands will take effect.
   This does not affect the place where the cursor-box is displayed.  */

static int
win32_cursor_to (row, col)
     register int row, col;
{
  int orow = row;

  curs_x = col;
  curs_y = row;

  if (updating_frame == 0)
    {
      BLOCK_INPUT;
      x_display_cursor (selected_frame, 1);
      UNBLOCK_INPUT;
    }
}

/* Display a sequence of N glyphs found at GP.
   WINDOW is the window to output to.  LEFT and TOP are starting coords.
   HL is 1 if this text is highlighted, 2 if the cursor is on it,
   3 if should appear in its mouse-face.
   JUST_FOREGROUND if 1 means draw only the foreground;
   don't alter the background.

   FONT is the default font to use (for glyphs whose font-code is 0).

   Since the display generation code is responsible for calling
   compute_char_face and compute_glyph_face on everything it puts in
   the display structure, we can assume that the face code on each
   glyph is a valid index into FRAME_COMPUTED_FACES (f), and the one
   to which we can actually apply intern_face.
   Call this function with input blocked.  */

static void
dumpglyphs (f, left, top, gp, n, hl, just_foreground)
     struct frame *f;
     int left, top;
     register GLYPH *gp; /* Points to first GLYPH. */
     register int n;  /* Number of glyphs to display. */
     int hl;
     int just_foreground;
{
  /* Holds characters to be displayed. */
  char *buf = (char *) alloca (f->width * sizeof (*buf));
  register char *cp;            /* Steps through buf[]. */
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;
  Window window = FRAME_WIN32_WINDOW (f);
  int orig_left = left;
  HDC hdc;

  hdc = get_frame_dc (f);

  while (n > 0)
    {
      /* Get the face-code of the next GLYPH.  */
      int cf, len;
      int g = *gp;

      GLYPH_FOLLOW_ALIASES (tbase, tlen, g);
      cf = FAST_GLYPH_FACE (g);

      /* Find the run of consecutive glyphs with the same face-code.
	 Extract their character codes into BUF.  */
      cp = buf;
      while (n > 0)
	{
	  g = *gp;
	  GLYPH_FOLLOW_ALIASES (tbase, tlen, g);
	  if (FAST_GLYPH_FACE (g) != cf)
	    break;

	  *cp++ = FAST_GLYPH_CHAR (g);
	  --n;
	  ++gp;
	}

      /* LEN gets the length of the run.  */
      len = cp - buf;

      /* Now output this run of chars, with the font and pixel values
	 determined by the face code CF.  */
      {
	struct face *face = FRAME_DEFAULT_FACE (f);
	XFontStruct *font = FACE_FONT (face);
	int stippled = 0;
	COLORREF fg;
	COLORREF bg;

	/* HL = 3 means use a mouse face previously chosen.  */
	if (hl == 3)
	  cf = FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_face_id;

	/* First look at the face of the text itself.  */
	if (cf != 0)
	  {
	    /* It's possible for the display table to specify
	       a face code that is out of range.  Use 0 in that case.  */
	    if (cf < 0 || cf >= FRAME_N_COMPUTED_FACES (f)
		|| FRAME_COMPUTED_FACES (f) [cf] == 0)
	      cf = 0;

	    if (cf == 1)
	      face = FRAME_MODE_LINE_FACE (f);
	    else
	      face = intern_face (f, FRAME_COMPUTED_FACES (f) [cf]);
	    font = FACE_FONT (face);
	    if (FACE_STIPPLE (face))
	      stippled = 1;
	  }

	/* Then comes the distinction between modeline and normal text.  */
	else if (hl == 0)
	 ;
	else if (hl == 1)
	  {
	    face = FRAME_MODE_LINE_FACE (f);
	    font = FACE_FONT (face);
	    if (FACE_STIPPLE (face))
	      stippled = 1;
	  }

	fg = face->foreground;
	bg = face->background;

	/* Now override that if the cursor's on this character.  */
	if (hl == 2)
	  {
	    /* The cursor overrides stippling.  */
	    stippled = 0;

	    if ((!face->font
		 || face->font == (XFontStruct *) FACE_DEFAULT
		 || face->font == f->output_data.win32->font)
		&& face->background == f->output_data.win32->background_pixel
		&& face->foreground == f->output_data.win32->foreground_pixel)
	      {
		bg = f->output_data.win32->cursor_pixel;
		fg = face->background;
	      }
	    /* Cursor on non-default face: must merge.  */
	    else
	      {
		bg = f->output_data.win32->cursor_pixel;
		fg = face->background;
		/* If the glyph would be invisible,
		   try a different foreground.  */
		if (fg == bg)
		  fg = face->foreground;
		if (fg == bg)
		  fg = f->output_data.win32->cursor_foreground_pixel;
		if (fg == bg)
		  fg = face->foreground;
		/* Make sure the cursor is distinct from text in this face.  */
		if (bg == face->background
		    && fg == face->foreground)
		  {
		    bg = face->foreground;
		    fg = face->background;
		  }
	      }
	  }

	if (font == (XFontStruct *) FACE_DEFAULT)
	  font = f->output_data.win32->font;

	SetBkMode (hdc, just_foreground ? TRANSPARENT : OPAQUE);

	SetTextColor (hdc, fg);
	SetBkColor (hdc, bg);

	SelectObject (hdc, font->hfont);
		
	TextOut (hdc, left, top, buf, len);

	if (!just_foreground)
	  {
	    /* Clear the rest of the line's height.  */
	    if (f->output_data.win32->line_height != FONT_HEIGHT (font))
		win32_fill_area (f, hdc, bg,
				 left,
				 top + FONT_HEIGHT (font),
				 FONT_WIDTH (font) * len,
				 f->output_data.win32->line_height - FONT_HEIGHT (font));
	  }

	{
	  int underline_position = 1;

	  if (font->tm.tmDescent <= underline_position)
	      underline_position = font->tm.tmDescent - 1;

	  if (face->underline)
	      win32_fill_area (f, hdc, fg,
			       left, (top
				      + FONT_BASE (font)
				      + underline_position),
			       len * FONT_WIDTH (font), 1);
	}

	left += len * FONT_WIDTH (font);
      }
    }

  release_frame_dc (f, hdc);
}


/* Output some text at the nominal frame cursor position.
   Advance the cursor over the text.
   Output LEN glyphs at START.

   `highlight', set up by win32_reassert_line_highlight or win32_change_line_highlight,
   controls the pixel values used for foreground and background.  */

static
win32_write_glyphs (start, len)
     register GLYPH *start;
     int len;
{
  register int temp_length;
  struct frame *f;

  BLOCK_INPUT;

  do_line_dance ();
  f = updating_frame;
  if (f == 0)
    {
      f = selected_frame;
      /* If not within an update,
	 output at the frame's visible cursor.  */
      curs_x = f->cursor_x;
      curs_y = f->cursor_y;
    }

  dumpglyphs (f,
	      CHAR_TO_PIXEL_COL (f, curs_x),
	      CHAR_TO_PIXEL_ROW (f, curs_y),
	      start, len, highlight, 0);

  /* If we drew on top of the cursor, note that it is turned off.  */
  if (curs_y == f->phys_cursor_y
      && curs_x <= f->phys_cursor_x
      && curs_x + len > f->phys_cursor_x)
    f->phys_cursor_x = -1;

  if (updating_frame == 0)
    {
      f->cursor_x += len;
      x_display_cursor (f, 1);
      f->cursor_x -= len;
    }
  else
    curs_x += len;

  UNBLOCK_INPUT;
}

/* Clear to the end of the line.
   Erase the current text line from the nominal cursor position (inclusive)
   to column FIRST_UNUSED (exclusive).  The idea is that everything
   from FIRST_UNUSED onward is already erased.  */

static
win32_clear_end_of_line (first_unused)
     register int first_unused;
{
  struct frame *f = updating_frame;

  if (f == 0)
    abort ();

  if (curs_y < 0 || curs_y >= f->height)
    return 1;
  if (first_unused <= 0)
    return 1;

  if (first_unused >= f->width)
    first_unused = f->width;

  BLOCK_INPUT;

  do_line_dance ();

  /* Notice if the cursor will be cleared by this operation.  */
  if (curs_y == f->phys_cursor_y
      && curs_x <= f->phys_cursor_x
      && f->phys_cursor_x < first_unused)
    f->phys_cursor_x = -1;

  win32_clear_area (f, NULL,
		    CHAR_TO_PIXEL_COL (f, curs_x),
		    CHAR_TO_PIXEL_ROW (f, curs_y),
		    FONT_WIDTH (f->output_data.win32->font) * (first_unused - curs_x),
		    f->output_data.win32->line_height);

  UNBLOCK_INPUT;
}

static
win32_clear_frame ()
{
  struct frame *f = updating_frame;

  if (f == 0)
    f = selected_frame;

  f->phys_cursor_x = -1;        /* Cursor not visible.  */
  curs_x = 0;                   /* Nominal cursor position is top left.  */
  curs_y = 0;

  BLOCK_INPUT;

  win32_clear_window (f);

  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  x_scroll_bar_clear (f);

  UNBLOCK_INPUT;
}

/* Make audible bell.  */

win32_ring_bell ()
{
  BLOCK_INPUT;

  if (visible_bell)
      FlashWindow (FRAME_WIN32_WINDOW (selected_frame), FALSE);
  else
      nt_ring_bell ();

  UNBLOCK_INPUT;

  return 1;
}

/* Insert and delete character.
   These are not supposed to be used because we are supposed to turn
   off the feature of using them.  */

static
win32_insert_glyphs (start, len)
     register char *start;
     register int len;
{
  abort ();
}

static
win32_delete_glyphs (n)
     register int n;
{
  abort ();
}

/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to win32_update_begin and win32_update_end.  */

static
win32_set_terminal_window (n)
     register int n;
{
  if (updating_frame == 0)
    abort ();

  if ((n <= 0) || (n > updating_frame->height))
    flexlines = updating_frame->height;
  else
    flexlines = n;
}

/* These variables need not be per frame
   because redisplay is done on a frame-by-frame basis
   and the line dance for one frame is finished before
   anything is done for another frame.  */

/* Array of line numbers from cached insert/delete operations.
   line_dance[i] is the old position of the line that we want
   to move to line i, or -1 if we want a blank line there.  */
static int *line_dance;

/* Allocated length of that array.  */
static int line_dance_len;

/* Flag indicating whether we've done any work.  */
static int line_dance_in_progress;

/* Perform an insert-lines or delete-lines operation,
   inserting N lines or deleting -N lines at vertical position VPOS.  */
win32_ins_del_lines (vpos, n)
     int vpos, n;
{
  register int fence, i;

  if (vpos >= flexlines)
    return 1;

  if (!line_dance_in_progress)
    {
      int ht = updating_frame->height;
      if (ht > line_dance_len)
	{
	  line_dance = (int *)xrealloc (line_dance, ht * sizeof (int));
	  line_dance_len = ht;
	}
      for (i = 0; i < ht; ++i) line_dance[i] = i;
      line_dance_in_progress = 1;
    }
  if (n >= 0)
    {
      if (n > flexlines - vpos)
	n = flexlines - vpos;
      fence = vpos + n;
      for (i = flexlines; --i >= fence;)
	line_dance[i] = line_dance[i-n];
      for (i = fence; --i >= vpos;)
	line_dance[i] = -1;
    }
  else
    {
      n = -n;
      if (n > flexlines - vpos)
	n = flexlines - vpos;
      fence = flexlines - n;
      for (i = vpos; i < fence; ++i)
	line_dance[i] = line_dance[i + n];
      for (i = fence; i < flexlines; ++i)
	line_dance[i] = -1;
    }
}

/* Here's where we actually move the pixels around.
   Must be called with input blocked.  */
static void
do_line_dance ()
{
  register int i, j, distance;
  register struct frame *f;
  int ht;
  int intborder;
  HDC hdc;

  /* Must check this flag first.  If it's not set, then not only is the
     array uninitialized, but we might not even have a frame.  */
  if (!line_dance_in_progress)
    return;

  f = updating_frame;
  if (f == 0)
    abort ();

  ht = f->height;
  intborder = f->output_data.win32->internal_border_width;

  x_display_cursor (updating_frame, 0);

  hdc = get_frame_dc (f);

  for (i = 0; i < ht; ++i)
    if (line_dance[i] != -1 && (distance = line_dance[i]-i) > 0)
      {
	for (j = i; (j < ht && line_dance[j] != -1
		     && line_dance[j]-j == distance); ++j);
	/* Copy [i,j) upward from [i+distance, j+distance) */
	BitBlt (hdc, 
		intborder, CHAR_TO_PIXEL_ROW (f, i+distance),
		f->width * FONT_WIDTH (f->output_data.win32->font),
		(j-i) * f->output_data.win32->line_height, 
		hdc,
		intborder, CHAR_TO_PIXEL_ROW (f, i),
		SRCCOPY);
	i = j-1;
      }

  for (i = ht; --i >=0; )
    if (line_dance[i] != -1 && (distance = line_dance[i]-i) < 0)
      {
	for (j = i; (--j >= 0 && line_dance[j] != -1
		     && line_dance[j]-j == distance););
	/* Copy (j, i] downward from (j+distance, i+distance] */
	BitBlt (hdc,
		intborder, CHAR_TO_PIXEL_ROW (f, j+1+distance),
		f->width * FONT_WIDTH (f->output_data.win32->font),
		(i-j) * f->output_data.win32->line_height, 
		hdc,
		intborder, CHAR_TO_PIXEL_ROW (f, j+1),
		SRCCOPY);
	i = j+1;
      }

  release_frame_dc (f, hdc);

  for (i = 0; i < ht; ++i)
    if (line_dance[i] == -1)
      {
	for (j = i; j < ht && line_dance[j] == -1; ++j);
	/* Clear [i,j) */
	win32_clear_area (f, NULL,
			  intborder, 
			  CHAR_TO_PIXEL_ROW (f, i),
			  f->width * FONT_WIDTH (f->output_data.win32->font),
			  (j-i) * f->output_data.win32->line_height);
	i = j-1;
      }
  line_dance_in_progress = 0;
}

/* Support routines for exposure events.  */
static void clear_cursor ();

/* Output into a rectangle of a window (for frame F)
   the characters in f->phys_lines that overlap that rectangle.
   TOP and LEFT are the position of the upper left corner of the rectangle.
   ROWS and COLS are the size of the rectangle.
   Call this function with input blocked.  */

void
dumprectangle (f, left, top, cols, rows)
     struct frame *f;
     register int left, top, cols, rows;
{
  register struct frame_glyphs *active_frame = FRAME_CURRENT_GLYPHS (f);
  int cursor_cleared = 0;
  int bottom, right;
  register int y;

  if (FRAME_GARBAGED_P (f))
    return;

  /* Express rectangle as four edges, instead of position-and-size.  */
  bottom = top + rows;
  right = left + cols;

  /* Convert rectangle edges in pixels to edges in chars.
     Round down for left and top, up for right and bottom.  */
  top  = PIXEL_TO_CHAR_ROW (f, top);
  left = PIXEL_TO_CHAR_COL (f, left);
  bottom += (f->output_data.win32->line_height - 1);
  right += (FONT_WIDTH (f->output_data.win32->font) - 1);
  bottom = PIXEL_TO_CHAR_ROW (f, bottom);
  right = PIXEL_TO_CHAR_COL (f, right);

  /* Clip the rectangle to what can be visible.  */
  if (left < 0)
    left = 0;
  if (top < 0)
    top = 0;
  if (right > f->width)
    right = f->width;
  if (bottom > f->height)
    bottom = f->height;

  /* Get size in chars of the rectangle.  */
  cols = right - left;
  rows = bottom - top;

  /* If rectangle has zero area, return.  */
  if (rows <= 0) return;
  if (cols <= 0) return;

  /* Turn off the cursor if it is in the rectangle.
     We will turn it back on afterward.  */
  if ((f->phys_cursor_x >= left) && (f->phys_cursor_x < right)
      && (f->phys_cursor_y >= top) && (f->phys_cursor_y < bottom))
    {
      clear_cursor (f);
      cursor_cleared = 1;
    }

  /* Display the text in the rectangle, one text line at a time.  */

  for (y = top; y < bottom; y++)
    {
      GLYPH *line = &active_frame->glyphs[y][left];

      if (! active_frame->enable[y] || left > active_frame->used[y])
	continue;

      dumpglyphs (f,
		  CHAR_TO_PIXEL_COL (f, left),
		  CHAR_TO_PIXEL_ROW (f, y),
		  line, min (cols, active_frame->used[y] - left),
		  active_frame->highlight[y], 0);
    }

  /* Turn the cursor on if we turned it off.  */

  if (cursor_cleared)
    x_display_cursor (f, 1);
}

static void
frame_highlight (f)
     struct frame *f;
{
  x_display_cursor (f, 1);
}

static void
frame_unhighlight (f)
     struct frame *f;
{
  x_display_cursor (f, 1);
}

static void win32_frame_rehighlight ();
static void x_frame_rehighlight ();

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the Lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occurred, so the
   Lisp code can tell when the switch took place by examining the events.  */

void
x_new_focus_frame (dpyinfo, frame)
     struct win32_display_info *dpyinfo;
     struct frame *frame;
{
  struct frame *old_focus = dpyinfo->win32_focus_frame;
  int events_enqueued = 0;

  if (frame != dpyinfo->win32_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of win32_focus_frame.  */
      dpyinfo->win32_focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	x_lower_frame (old_focus);

      if (dpyinfo->win32_focus_frame && dpyinfo->win32_focus_frame->auto_raise)
	pending_autoraise_frame = dpyinfo->win32_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  x_frame_rehighlight (dpyinfo);
}

/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
x_mouse_leave (dpyinfo)
     struct win32_display_info *dpyinfo;
{
  x_new_focus_frame (dpyinfo, dpyinfo->win32_focus_event_frame);
}

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   minibuffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or unhighlighted; we only use it to find
   the appropriate display info.  */
static void
win32_frame_rehighlight (frame)
     struct frame *frame;
{
  x_frame_rehighlight (FRAME_WIN32_DISPLAY_INFO (frame));
}

static void
x_frame_rehighlight (dpyinfo)
     struct win32_display_info *dpyinfo;
{
  struct frame *old_highlight = dpyinfo->win32_highlight_frame;

  if (dpyinfo->win32_focus_frame)
    {
      dpyinfo->win32_highlight_frame
	= ((GC_FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->win32_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->win32_focus_frame))
	   : dpyinfo->win32_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->win32_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (dpyinfo->win32_focus_frame) = Qnil;
	  dpyinfo->win32_highlight_frame = dpyinfo->win32_focus_frame;
	}
    }
  else
    dpyinfo->win32_highlight_frame = 0;

  if (dpyinfo->win32_highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (dpyinfo->win32_highlight_frame)
	frame_highlight (dpyinfo->win32_highlight_frame);
    }
}

/* Keyboard processing - modifier keys, etc. */

/* Convert a keysym to its name.  */

char *
x_get_keysym_name (keysym)
    int keysym;
{
  /* Make static so we can always return it */
  static char value[100];

  BLOCK_INPUT;
  GetKeyNameText(keysym, value, 100);
  UNBLOCK_INPUT;

  return value;
}

/* Mouse clicks and mouse movement.  Rah.  */

/* Given a pixel position (PIX_X, PIX_Y) on the frame F, return
   glyph co-ordinates in (*X, *Y).  Set *BOUNDS to the rectangle
   that the glyph at X, Y occupies, if BOUNDS != 0.
   If NOCLIP is nonzero, do not force the value into range.  */

void
pixel_to_glyph_coords (f, pix_x, pix_y, x, y, bounds, noclip)
     FRAME_PTR f;
     register int pix_x, pix_y;
     register int *x, *y;
     RECT *bounds;
     int noclip;
{
  /* Support tty mode: if Vwindow_system is nil, behave correctly. */
  if (NILP (Vwindow_system))
    {
      *x = pix_x;
      *y = pix_y;
      return;
    }

  /* Arrange for the division in PIXEL_TO_CHAR_COL etc. to round down
     even for negative values.  */
  if (pix_x < 0)
    pix_x -= FONT_WIDTH ((f)->output_data.win32->font) - 1;
  if (pix_y < 0)
    pix_y -= (f)->output_data.win32->line_height - 1;

  pix_x = PIXEL_TO_CHAR_COL (f, pix_x);
  pix_y = PIXEL_TO_CHAR_ROW (f, pix_y);

  if (bounds)
    {
      bounds->left = CHAR_TO_PIXEL_COL (f, pix_x);
      bounds->top = CHAR_TO_PIXEL_ROW (f, pix_y);
      bounds->right  = bounds->left + FONT_WIDTH  (f->output_data.win32->font) - 1;
      bounds->bottom = bounds->top + f->output_data.win32->line_height - 1;
    }

  if (!noclip)
    {
      if (pix_x < 0)
	pix_x = 0;
      else if (pix_x > f->width)
	pix_x = f->width;

      if (pix_y < 0)
	pix_y = 0;
      else if (pix_y > f->height)
	pix_y = f->height;
    }

  *x = pix_x;
  *y = pix_y;
}

void
glyph_to_pixel_coords (f, x, y, pix_x, pix_y)
     FRAME_PTR f;
     register int x, y;
     register int *pix_x, *pix_y;
{
  /* Support tty mode: if Vwindow_system is nil, behave correctly. */
  if (NILP (Vwindow_system))
    {
      *pix_x = x;
      *pix_y = y;
      return;
    }

  *pix_x = CHAR_TO_PIXEL_COL (f, x);
  *pix_y = CHAR_TO_PIXEL_ROW (f, y);
}

BOOL 
parse_button (message, pbutton, pup)
     int message;
     int * pbutton;
     int * pup;
{
  int button = 0;
  int up = 0;
  
  switch (message)
    {
    case WM_LBUTTONDOWN:
      button = 0;
      up = 0;
      break;
    case WM_LBUTTONUP:
      button = 0;
      up = 1;
      break;
    case WM_MBUTTONDOWN:
      if (NILP (Vwin32_swap_mouse_buttons))
	button = 1;
      else
	button = 2;
      up = 0;
      break;
    case WM_MBUTTONUP:
      if (NILP (Vwin32_swap_mouse_buttons))
	button = 1;
      else
	button = 2;
      up = 1;
      break;
    case WM_RBUTTONDOWN:
      if (NILP (Vwin32_swap_mouse_buttons))
	button = 2;
      else
	button = 1;
      up = 0;
      break;
    case WM_RBUTTONUP:
      if (NILP (Vwin32_swap_mouse_buttons))
	button = 2;
      else
	button = 1;
      up = 1;
      break;
    default:
      return (FALSE);
    }
  
  if (pup) *pup = up;
  if (pbutton) *pbutton = button;
  
  return (TRUE);
}


/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static void
construct_mouse_click (result, msg, f)
     struct input_event *result;
     Win32Msg *msg;
     struct frame *f;
{
  int button;
  int up;

  parse_button (msg->msg.message, &button, &up);

  /* Make the event type no_event; we'll change that when we decide
     otherwise.  */
  result->kind = mouse_click;
  result->code = button;
  result->timestamp = msg->msg.time;
  result->modifiers = (msg->dwModifiers
		       | (up
			  ? up_modifier
			  : down_modifier));

  {
    int row, column;

    XSETINT (result->x, LOWORD (msg->msg.lParam));
    XSETINT (result->y, HIWORD (msg->msg.lParam));
    XSETFRAME (result->frame_or_window, f);
  }
}


/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static void
note_mouse_movement (frame, msg)
     FRAME_PTR frame;
     MSG *msg;
{
  last_mouse_movement_time = msg->time;

  if (msg->hwnd != FRAME_WIN32_WINDOW (frame))
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;

      note_mouse_highlight (frame, -1, -1);
    }

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  else if (LOWORD (msg->lParam) < last_mouse_glyph.left
	   || LOWORD (msg->lParam) > last_mouse_glyph.right
	   || HIWORD (msg->lParam) < last_mouse_glyph.top
	   || HIWORD (msg->lParam) > last_mouse_glyph.bottom)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;

      note_mouse_highlight (frame, LOWORD (msg->lParam), HIWORD (msg->lParam));
    }
}

/* This is used for debugging, to turn off note_mouse_highlight.  */
static int disable_mouse_highlight;

/* Take proper action when the mouse has moved to position X, Y on frame F
   as regards highlighting characters that have mouse-face properties.
   Also dehighlighting chars where the mouse was before.
   X and Y can be negative or out of range.  */

static void
note_mouse_highlight (f, x, y)
     FRAME_PTR f;
     int x, y;
{
  int row, column, portion;
  RECT new_glyph;
  Lisp_Object window;
  struct window *w;

  if (disable_mouse_highlight)
    return;

  FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_x = x;
  FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_y = y;
  FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_frame = f;

  if (FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_defer)
    return;

  if (gc_in_progress)
    {
      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_deferred_gc = 1;
      return;
    }

  /* Find out which glyph the mouse is on.  */
  pixel_to_glyph_coords (f, x, y, &column, &row,
			 &new_glyph, FRAME_WIN32_DISPLAY_INFO (f)->grabbed);

  /* Which window is that in?  */
  window = window_from_coordinates (f, column, row, &portion);
  w = XWINDOW (window);

  /* If we were displaying active text in another window, clear that.  */
  if (! EQ (window, FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_window))
    clear_mouse_face (FRAME_WIN32_DISPLAY_INFO (f));

  /* Are we in a window whose display is up to date?
     And verify the buffer's text has not changed.  */
  if (WINDOWP (window) && portion == 0 && row >= 0 && column >= 0
      && row < FRAME_HEIGHT (f) && column < FRAME_WIDTH (f)
      && EQ (w->window_end_valid, w->buffer)
      && w->last_modified == BUF_MODIFF (XBUFFER (w->buffer)))
    {
      int *ptr = FRAME_CURRENT_GLYPHS (f)->charstarts[row];
      int i, pos;

      /* Find which buffer position the mouse corresponds to.  */
      for (i = column; i >= 0; i--)
	if (ptr[i] > 0)
	  break;
      pos = ptr[i];
      /* Is it outside the displayed active region (if any)?  */
      if (pos <= 0)
	clear_mouse_face (FRAME_WIN32_DISPLAY_INFO (f));
      else if (! (EQ (window, FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_window)
		  && row >= FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_row
		  && row <= FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_row
		  && (row > FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_row
		      || column >= FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_col)
		  && (row < FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_row
		      || column < FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_col
		      || FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_past_end)))
	{
	  Lisp_Object mouse_face, overlay, position;
	  Lisp_Object *overlay_vec;
	  int len, noverlays, ignor1;
	  struct buffer *obuf;
	  int obegv, ozv;

	  /* If we get an out-of-range value, return now; avoid an error.  */
	  if (pos > BUF_Z (XBUFFER (w->buffer)))
	    return;

	  /* Make the window's buffer temporarily current for
	     overlays_at and compute_char_face.  */
	  obuf = current_buffer;
	  current_buffer = XBUFFER (w->buffer);
	  obegv = BEGV;
	  ozv = ZV;
	  BEGV = BEG;
	  ZV = Z;

	  /* Yes.  Clear the display of the old active region, if any.  */
	  clear_mouse_face (FRAME_WIN32_DISPLAY_INFO (f));

	  /* Is this char mouse-active?  */
	  XSETINT (position, pos);

	  len = 10;
	  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

	  /* Put all the overlays we want in a vector in overlay_vec.
	     Store the length in len.  */
	  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len,
				   NULL, NULL);
	  noverlays = sort_overlays (overlay_vec, noverlays, w);

	  /* Find the highest priority overlay that has a mouse-face prop.  */
	  overlay = Qnil;
	  for (i = 0; i < noverlays; i++)
	    {
	      mouse_face = Foverlay_get (overlay_vec[i], Qmouse_face);
	      if (!NILP (mouse_face))
		{
		  overlay = overlay_vec[i];
		  break;
		}
	    }
	  free (overlay_vec);
	  /* If no overlay applies, get a text property.  */
	  if (NILP (overlay))
	    mouse_face = Fget_text_property (position, Qmouse_face, w->buffer);

	  /* Handle the overlay case.  */
	  if (! NILP (overlay))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after;
	      int ignore;

	      before = Foverlay_start (overlay);
	      after = Foverlay_end (overlay);
	      /* Record this as the current active region.  */
	      fast_find_position (window, before,
				  &FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_col,
				  &FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_row);
	      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_past_end
		= !fast_find_position (window, after,
				       &FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_col,
				       &FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_row);
	      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_window = window;
	      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_face_id
		= compute_char_face (f, w, pos, 0, 0,
				     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (FRAME_WIN32_DISPLAY_INFO (f), 1);
	    }
	  /* Handle the text property case.  */
	  else if (! NILP (mouse_face))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after, beginning, end;
	      int ignore;

	      beginning = Fmarker_position (w->start);
	      XSETINT (end, (BUF_Z (XBUFFER (w->buffer))
			     - XFASTINT (w->window_end_pos)));
	      before
		= Fprevious_single_property_change (make_number (pos + 1),
						    Qmouse_face,
						    w->buffer, beginning);
	      after
		= Fnext_single_property_change (position, Qmouse_face,
						w->buffer, end);
	      /* Record this as the current active region.  */
	      fast_find_position (window, before,
				  &FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_col,
				  &FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_row);
	      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_past_end
		= !fast_find_position (window, after,
				       &FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_col,
				       &FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_row);
	      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_window = window;
	      FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_face_id
		= compute_char_face (f, w, pos, 0, 0,
				     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (FRAME_WIN32_DISPLAY_INFO (f), 1);
	    }
	  BEGV = obegv;
	  ZV = ozv;
	  current_buffer = obuf;
	}
    }
}

/* Find the row and column of position POS in window WINDOW.
   Store them in *COLUMNP and *ROWP.
   This assumes display in WINDOW is up to date.
   If POS is above start of WINDOW, return coords
   of start of first screen line.
   If POS is after end of WINDOW, return coords of end of last screen line.

   Value is 1 if POS is in range, 0 if it was off screen.  */

static int
fast_find_position (window, pos, columnp, rowp)
     Lisp_Object window;
     int pos;
     int *columnp, *rowp;
{
  struct window *w = XWINDOW (window);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  int i;
  int row = 0;
  int left = w->left;
  int top = w->top;
  int height = XFASTINT (w->height) - ! MINI_WINDOW_P (w);
  int width = window_internal_width (w);
  int *charstarts;
  int lastcol;
  int maybe_next_line = 0;

  /* Find the right row.  */
  for (i = 0;
       i < height;
       i++)
    {
      int linestart = FRAME_CURRENT_GLYPHS (f)->charstarts[top + i][left];
      if (linestart > pos)
	break;
      /* If the position sought is the end of the buffer,
	 don't include the blank lines at the bottom of the window.  */
      if (linestart == pos && pos == BUF_ZV (XBUFFER (w->buffer)))
	{
	  maybe_next_line = 1;
	  break;
	}
      if (linestart > 0)
	row = i;
    }

  /* Find the right column with in it.  */
  charstarts = FRAME_CURRENT_GLYPHS (f)->charstarts[top + row];
  lastcol = left;
  for (i = 0; i < width; i++)
    {
      if (charstarts[left + i] == pos)
	{
	  *rowp = row + top;
	  *columnp = i + left;
	  return 1;
	}
      else if (charstarts[left + i] > pos)
	break;
      else if (charstarts[left + i] > 0)
	lastcol = left + i;
    }

  /* If we're looking for the end of the buffer,
     and we didn't find it in the line we scanned,
     use the start of the following line.  */
  if (maybe_next_line)
    {
      row++;
      i = 0;
    }

  *rowp = row + top;
  *columnp = lastcol;
  return 0;
}

/* Display the active region described by mouse_face_*
   in its mouse-face if HL > 0, in its normal face if HL = 0.  */

static void
show_mouse_face (dpyinfo, hl)
     struct win32_display_info *dpyinfo;
     int hl;
{
  struct window *w = XWINDOW (dpyinfo->mouse_face_window);
  int width = window_internal_width (w);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  int i;
  int cursor_off = 0;
  int old_curs_x = curs_x;
  int old_curs_y = curs_y;

  /* Set these variables temporarily
     so that if we have to turn the cursor off and on again
     we will put it back at the same place.  */
  curs_x = f->phys_cursor_x;
  curs_y = f->phys_cursor_y;

  for (i = FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_row;
       i <= FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_row; i++)
    {
      int column = (i == FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_row
		    ? FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_col
		    : w->left);
      int endcolumn = (i == FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_row
		       ? FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_col
		       : w->left + width);
      endcolumn = min (endcolumn, FRAME_CURRENT_GLYPHS (f)->used[i]);

      /* If the cursor's in the text we are about to rewrite,
	 turn the cursor off.  */
      if (i == curs_y
	  && curs_x >= FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_col - 1
	  && curs_x <= FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_col)
	{
	  x_display_cursor (f, 0);
	  cursor_off = 1;
	}

      dumpglyphs (f,
		  CHAR_TO_PIXEL_COL (f, column),
		  CHAR_TO_PIXEL_ROW (f, i),
		  FRAME_CURRENT_GLYPHS (f)->glyphs[i] + column,
		  endcolumn - column,
		  /* Highlight with mouse face if hl > 0.  */
		  hl > 0 ? 3 : 0, 0);
    }

  /* If we turned the cursor off, turn it back on.  */
  if (cursor_off)
    x_display_cursor (f, 1);

  curs_x = old_curs_x;
  curs_y = old_curs_y;

  /* Change the mouse cursor according to the value of HL.  */
  if (hl > 0)
    SetCursor (f->output_data.win32->cross_cursor);
  else
    SetCursor (f->output_data.win32->text_cursor);
}

/* Clear out the mouse-highlighted active region.
   Redraw it unhighlighted first.  */

static void
clear_mouse_face (dpyinfo)
     struct win32_display_info *dpyinfo;
{
  if (! NILP (dpyinfo->mouse_face_window))
    show_mouse_face (dpyinfo, 0);

  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_window = Qnil;
}

struct scroll_bar *x_window_to_scroll_bar ();
static void x_scroll_bar_report_motion ();

/* Return the current position of the mouse.
   *fp should be a frame which indicates which display to ask about.

   If the mouse movement started in a scroll bar, set *fp, *bar_window,
   and *part to the frame, window, and scroll bar part that the mouse
   is over.  Set *x and *y to the portion and whole of the mouse's
   position on the scroll bar.

   If the mouse movement started elsewhere, set *fp to the frame the
   mouse is on, *bar_window to nil, and *x and *y to the character cell
   the mouse is over.

   Set *time to the server timestamp for the time at which the mouse
   was at this position.

   Don't store anything if we don't have a valid set of values to report.

   This clears the mouse_moved flag, so we can wait for the next mouse
   movement.  This also calls XQueryPointer, which will cause the
   server to give us another MotionNotify when the mouse moves
   again. */

static void
win32_mouse_position (fp, insist, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     int insist;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  FRAME_PTR f1;

  BLOCK_INPUT;

  if (! NILP (last_mouse_scroll_bar))
    x_scroll_bar_report_motion (fp, bar_window, part, x, y, time);
  else
    {
      POINT pt;

      Lisp_Object frame, tail;

      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
	XFRAME (frame)->mouse_moved = 0;

      last_mouse_scroll_bar = Qnil;
      
      GetCursorPos (&pt);

      /* Now we have a position on the root; find the innermost window
	 containing the pointer.  */
      {
	if (FRAME_WIN32_DISPLAY_INFO (*fp)->grabbed && last_mouse_frame
	    && FRAME_LIVE_P (last_mouse_frame))
	  {
	    f1 = last_mouse_frame;
	  }
	else
	  {
	    /* Is win one of our frames?  */
	    f1 = x_window_to_frame (FRAME_WIN32_DISPLAY_INFO (*fp), WindowFromPoint(pt));
	  }

	/* If not, is it one of our scroll bars?  */
	if (! f1)
	  {
	    struct scroll_bar *bar = x_window_to_scroll_bar (WindowFromPoint(pt));

	    if (bar)
	      {
		f1 = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
	      }
	  }

	if (f1 == 0 && insist)
	  f1 = selected_frame;

	if (f1)
	  {
	    int ignore1, ignore2;

	    ScreenToClient (FRAME_WIN32_WINDOW (f1), &pt);

	    /* Ok, we found a frame.  Store all the values.  */

	    pixel_to_glyph_coords (f1, pt.x, pt.y, &ignore1, &ignore2,
				   &last_mouse_glyph,
				   FRAME_WIN32_DISPLAY_INFO (f1)->grabbed
				   || insist);

	    *bar_window = Qnil;
	    *part = 0;
	    *fp = f1;
	    XSETINT (*x, pt.x);
	    XSETINT (*y, pt.y);
	    *time = last_mouse_movement_time;
	  }
      }
    }

  UNBLOCK_INPUT;
}

/* Scroll bar support.  */

/* Given an window ID, find the struct scroll_bar which manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */
struct scroll_bar *
x_window_to_scroll_bar (window_id)
     Window window_id;
{
  Lisp_Object tail, frame;

  for (tail = Vframe_list;
       XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      Lisp_Object frame, bar, condemned;

      frame = XCONS (tail)->car;
      /* All elements of Vframe_list should be frames.  */
      if (! GC_FRAMEP (frame))
	abort ();

      /* Scan this frame's scroll bar list for a scroll bar with the
	 right window ID.  */
      condemned = FRAME_CONDEMNED_SCROLL_BARS (XFRAME (frame));
      for (bar = FRAME_SCROLL_BARS (XFRAME (frame));
	   /* This trick allows us to search both the ordinary and
	      condemned scroll bar lists with one loop.  */
	   ! GC_NILP (bar) || (bar = condemned,
			       condemned = Qnil,
			       ! GC_NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	if (SCROLL_BAR_WIN32_WINDOW (XSCROLL_BAR (bar)) == window_id)
	  return XSCROLL_BAR (bar);
    }

  return 0;
}

HWND 
my_create_scrollbar (f, bar)
     struct frame * f;
     struct scroll_bar * bar;
{
  MSG msg;
  
  PostThreadMessage (dwWinThreadId, WM_EMACS_CREATESCROLLBAR, (WPARAM) f, 
		     (LPARAM) bar);
  GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
  
  return ((HWND) msg.wParam);
}

//#define ATTACH_THREADS

void
my_show_window (HWND hwnd, int how)
{
#ifndef ATTACH_THREADS
  SendMessage (hwnd, WM_EMACS_SHOWWINDOW, (WPARAM) how, 0);
#else
  ShowWindow (hwnd , how);
#endif
}

void
my_set_window_pos (HWND hwnd, HWND hwndAfter,
		   int x, int y, int cx, int cy, int flags)
{
#ifndef ATTACH_THREADS
  Win32WindowPos pos;
  pos.hwndAfter = hwndAfter;
  pos.x = x;
  pos.y = y;
  pos.cx = cx;
  pos.cy = cy;
  pos.flags = flags;
  SendMessage (hwnd, WM_EMACS_SETWINDOWPOS, (WPARAM) &pos, 0);
#else
  SetWindowPos (hwnd, hwndAfter, x, y, cx, cy, flags);
#endif
}

void
my_destroy_window (f, hwnd)
     struct frame * f;
     HWND hwnd;
{
  SendMessage (FRAME_WIN32_WINDOW (f), WM_EMACS_DESTROYWINDOW, 
	       (WPARAM) hwnd, 0);
}

/* Open a new window to serve as a scroll bar, and return the
   scroll bar vector for it.  */
static struct scroll_bar *
x_scroll_bar_create (window, top, left, width, height)
     struct window *window;
     int top, left, width, height;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (window));
  struct scroll_bar *bar
    = XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));
  HWND hwnd;

  BLOCK_INPUT;

  XSETWINDOW (bar->window, window);
  XSETINT (bar->top, top);
  XSETINT (bar->left, left);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);
  XSETINT (bar->start, 0);
  XSETINT (bar->end, 0);
  bar->dragging = Qnil;

  /* Requires geometry to be set before call to create the real window */

  hwnd = my_create_scrollbar (f, bar);

  SetScrollRange (hwnd, SB_CTL, 0, height, FALSE);
  SetScrollPos (hwnd, SB_CTL, 0, TRUE);

  SET_SCROLL_BAR_WIN32_WINDOW (bar, hwnd);

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  UNBLOCK_INPUT;

  return bar;
}

/* Draw BAR's handle in the proper position.
   If the handle is already drawn from START to END, don't bother
   redrawing it, unless REBUILD is non-zero; in that case, always
   redraw it.  (REBUILD is handy for drawing the handle after expose
   events.)

   Normally, we want to constrain the start and end of the handle to
   fit inside its rectangle, but if the user is dragging the scroll bar
   handle, we want to let them drag it down all the way, so that the
   bar's top is as far down as it goes; otherwise, there's no way to
   move to the very end of the buffer.  */
static void
x_scroll_bar_set_handle (bar, start, end, rebuild)
     struct scroll_bar *bar;
     int start, end;
     int rebuild;
{
  int dragging = ! NILP (bar->dragging);
  Window w = SCROLL_BAR_WIN32_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == XINT (bar->start)
      && end == XINT (bar->end))
    return;

  BLOCK_INPUT;

  /* Store the adjusted setting in the scroll bar.  */
  XSETINT (bar->start, start);
  XSETINT (bar->end, end);

  SetScrollPos (w, SB_CTL, start, TRUE);

  UNBLOCK_INPUT;
}

/* Move a scroll bar around on the screen, to accommodate changing
   window configurations.  */
static void
x_scroll_bar_move (bar, top, left, width, height)
     struct scroll_bar *bar;
     int top, left, width, height;
{
  Window w = SCROLL_BAR_WIN32_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  BLOCK_INPUT;

  MoveWindow (w, left, top, width, height, TRUE);
  SetScrollRange (w, SB_CTL, 0, height, FALSE);
  InvalidateRect (w, NULL, FALSE);
  my_show_window (w, SW_NORMAL);

  XSETINT (bar->left, left);
  XSETINT (bar->top, top);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);

  UNBLOCK_INPUT;
}

/* Destroy the window for BAR, and set its Emacs window's scroll bar
   to nil.  */
static void
x_scroll_bar_remove (bar)
     struct scroll_bar *bar;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  BLOCK_INPUT;

  /* Destroy the window.  */
  my_destroy_window (f, SCROLL_BAR_WIN32_WINDOW (bar));

  /* Disassociate this scroll bar from its window.  */
  XWINDOW (bar->window)->vertical_scroll_bar = Qnil;

  UNBLOCK_INPUT;
}

/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */
static void
win32_set_vertical_scroll_bar (window, portion, whole, position)
     struct window *window;
     int portion, whole, position;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (window));
  int top = XINT (window->top);
  int left = WINDOW_VERTICAL_SCROLL_BAR_COLUMN (window);
  int height = WINDOW_VERTICAL_SCROLL_BAR_HEIGHT (window);

  /* Where should this scroll bar be, pixelwise?  */
  int pixel_top  = CHAR_TO_PIXEL_ROW (f, top);
  int pixel_left = CHAR_TO_PIXEL_COL (f, left);
  int pixel_width
    = (FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.win32->font)));
  int pixel_height = VERTICAL_SCROLL_BAR_PIXEL_HEIGHT (f, height);

  struct scroll_bar *bar;

  /* Does the scroll bar exist yet?  */
  if (NILP (window->vertical_scroll_bar))
    bar = x_scroll_bar_create (window,
			      pixel_top, pixel_left,
			      pixel_width, pixel_height);
  else
    {
      /* It may just need to be moved and resized.  */
      bar = XSCROLL_BAR (window->vertical_scroll_bar);
      x_scroll_bar_move (bar, pixel_top, pixel_left, pixel_width, pixel_height);
    }

  /* Set the scroll bar's current state, unless we're currently being
     dragged.  */
  if (NILP (bar->dragging))
    {
      int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (pixel_height);

      if (whole == 0)
	x_scroll_bar_set_handle (bar, 0, top_range, 0);
      else
	{
	  int start = (int) (((double) position * top_range) / whole);
	  int end = (int) (((double) (position + portion) * top_range) / whole);

	  x_scroll_bar_set_handle (bar, start, end, 0);
	}
    }

  XSETVECTOR (window->vertical_scroll_bar, bar);
}


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - "Can you say set-window-configuration, boys
   and girls?"  Instead, we just assert at the beginning of redisplay
   that *all* scroll bars are to be removed, and then save a scroll bar
   from the fiery pit when we actually redisplay its window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgement.  */
static void
win32_condemn_scroll_bars (frame)
     FRAME_PTR frame;
{
  /* The condemned list should be empty at this point; if it's not,
     then the rest of Emacs isn't using the condemn/redeem/judge
     protocol correctly.  */
  if (! NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
    abort ();

  /* Move them all to the "condemned" list.  */
  FRAME_CONDEMNED_SCROLL_BARS (frame) = FRAME_SCROLL_BARS (frame);
  FRAME_SCROLL_BARS (frame) = Qnil;
}

/* Unmark WINDOW's scroll bar for deletion in this judgement cycle.
   Note that WINDOW isn't necessarily condemned at all.  */
static void
win32_redeem_scroll_bar (window)
     struct window *window;
{
  struct scroll_bar *bar;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (window->vertical_scroll_bar))
    abort ();

  bar = XSCROLL_BAR (window->vertical_scroll_bar);

  /* Unlink it from the condemned list.  */
  {
    FRAME_PTR f = XFRAME (WINDOW_FRAME (window));

    if (NILP (bar->prev))
      {
	/* If the prev pointer is nil, it must be the first in one of
	   the lists.  */
	if (EQ (FRAME_SCROLL_BARS (f), window->vertical_scroll_bar))
	  /* It's not condemned.  Everything's fine.  */
	  return;
	else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		     window->vertical_scroll_bar))
	  FRAME_CONDEMNED_SCROLL_BARS (f) = bar->next;
	else
	  /* If its prev pointer is nil, it must be at the front of
	     one or the other!  */
	  abort ();
      }
    else
      XSCROLL_BAR (bar->prev)->next = bar->next;

    if (! NILP (bar->next))
      XSCROLL_BAR (bar->next)->prev = bar->prev;

    bar->next = FRAME_SCROLL_BARS (f);
    bar->prev = Qnil;
    XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
    if (! NILP (bar->next))
      XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
  }
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */
static void
win32_judge_scroll_bars (f)
     FRAME_PTR f;
{
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  FRAME_CONDEMNED_SCROLL_BARS (f) = Qnil;

  for (; ! NILP (bar); bar = next)
    {
      struct scroll_bar *b = XSCROLL_BAR (bar);

      x_scroll_bar_remove (b);

      next = b->next;
      b->next = b->prev = Qnil;
    }

  /* Now there should be no references to the condemned scroll bars,
     and they should get garbage-collected.  */
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than no_event, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static int
x_scroll_bar_handle_click (bar, msg, emacs_event)
     struct scroll_bar *bar;
     Win32Msg *msg;
     struct input_event *emacs_event;
{
  if (! GC_WINDOWP (bar->window))
    abort ();

  emacs_event->kind = win32_scroll_bar_click;
  emacs_event->code = 0;
  /* not really meaningful to distinguish up/down */
  emacs_event->modifiers = msg->dwModifiers;
  emacs_event->frame_or_window = bar->window;
  emacs_event->timestamp = msg->msg.time;

  {
    int internal_height
      = VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (XINT (bar->height));
    int top_range
      = VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height));
    int y = GetScrollPos ((HWND) msg->msg.lParam, SB_CTL);

    switch (LOWORD (msg->msg.wParam))
      {
      case SB_THUMBTRACK:
	emacs_event->part = scroll_bar_handle;
	if (VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height)) <= 0xffff)
	    y = HIWORD (msg->msg.wParam);
	break;
      case SB_LINEDOWN:
	emacs_event->part = scroll_bar_down_arrow;
	break;
      case SB_LINEUP:
	emacs_event->part = scroll_bar_up_arrow;
	break;
      case SB_PAGEUP:
	emacs_event->part = scroll_bar_above_handle;
	break;
      case SB_PAGEDOWN:
	emacs_event->part = scroll_bar_below_handle;
	break;
      case SB_TOP:
	emacs_event->part = scroll_bar_handle;
	y = 0;
	break;
      case SB_BOTTOM:
	emacs_event->part = scroll_bar_handle;
	y = top_range;
	break;
      case SB_THUMBPOSITION:
	emacs_event->part = scroll_bar_handle;
	break;
      case SB_ENDSCROLL:
      default:
	SetScrollPos (SCROLL_BAR_WIN32_WINDOW (bar), SB_CTL, y, TRUE);
	return FALSE;
      }

    XSETINT (emacs_event->x, y);
    XSETINT (emacs_event->y, top_range);

    return TRUE;
  }
}

/* Return information to the user about the current position of the mouse
   on the scroll bar.  */
static void
x_scroll_bar_report_motion (fp, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  struct scroll_bar *bar = XSCROLL_BAR (last_mouse_scroll_bar);
  Window w = SCROLL_BAR_WIN32_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int pos;

  BLOCK_INPUT;

  *fp = f;
  *bar_window = bar->window;

  pos = GetScrollPos (w, SB_CTL);

  switch (LOWORD (last_mouse_scroll_bar_pos))
  {
  case SB_THUMBPOSITION:
  case SB_THUMBTRACK:
      *part = scroll_bar_handle;
      if (VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height)) <= 0xffff)
	  pos = HIWORD (last_mouse_scroll_bar_pos);
      break;
  case SB_LINEDOWN:
      *part = scroll_bar_handle;
      pos++;
      break;
  default:
      *part = scroll_bar_handle;
      break;
  }

  XSETINT(*x, pos);
  XSETINT(*y, VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height)));

  f->mouse_moved = 0;
  last_mouse_scroll_bar = Qnil;

  *time = last_mouse_movement_time;

  UNBLOCK_INPUT;
}

/* The screen has been cleared so we may have changed foreground or
   background colors, and the scroll bars may need to be redrawn.
   Clear out the scroll bars, and ask for expose events, so we can
   redraw them.  */

x_scroll_bar_clear (f)
     FRAME_PTR f;
{
  Lisp_Object bar;

  for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
       bar = XSCROLL_BAR (bar)->next)
    {
      HWND window = SCROLL_BAR_WIN32_WINDOW (XSCROLL_BAR (bar));
      HDC hdc = GetDC (window);
      RECT rect;

      my_show_window (window, SW_HIDE);
      GetClientRect (window, &rect);
      select_palette (f, hdc);
      win32_clear_rect (f, hdc, &rect);
      deselect_palette (f, hdc);
    }
}

show_scroll_bars (f, how)
     FRAME_PTR f;
     int how;
{
  Lisp_Object bar;

  for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
       bar = XSCROLL_BAR (bar)->next)
    {
      HWND window = SCROLL_BAR_WIN32_WINDOW (XSCROLL_BAR (bar));
      my_show_window (window, how);
    }
}


/* The main Win32 event-reading loop - w32_read_socket.  */

/* Timestamp of enter window event.  This is only used by w32_read_socket,
   but we have to put it out here, since static variables within functions
   sometimes don't work.  */
static Time enter_timestamp;

/* Record the last 100 characters stored
   to help debug the loss-of-chars-during-GC problem.  */
int temp_index;
short temp_buffer[100];

extern int key_event (KEY_EVENT_RECORD *, struct input_event *);

/* Map a Win32 WM_CHAR message into a KEY_EVENT_RECORD so that
   we can use the same routines to handle input in both console
   and window modes.  */

static void
convert_to_key_event (Win32Msg *msgp, KEY_EVENT_RECORD *eventp)
{
  eventp->bKeyDown = TRUE;
  eventp->wRepeatCount = 1;
  eventp->wVirtualKeyCode = msgp->msg.wParam;
  eventp->wVirtualScanCode = (msgp->msg.lParam & 0xFF0000) >> 16;
  eventp->uChar.AsciiChar = 0;
  eventp->dwControlKeyState = msgp->dwModifiers;
}

/* Return nonzero if the virtual key is a dead key.  */

static int
is_dead_key (int wparam)
{
  unsigned int code = MapVirtualKey (wparam, 2);

  /* Win95 returns 0x8000, NT returns 0x80000000.  */
  if ((code & 0x8000) || (code & 0x80000000))
    return 1;
  else
    return 0;
}

/* Read events coming from the Win32 shell.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   Events representing keys are stored in buffer BUFP,
   which can hold up to NUMCHARS characters.
   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   WAITP is nonzero if we should block until input arrives.
   EXPECTED is nonzero if the caller knows input is available.  

   Some of these messages are reposted back to the message queue since the
   system calls the winproc directly in a context where we cannot return the
   data nor can we guarantee the state we are in.  So if we dispatch  them
   we will get into an infinite loop.  To prevent this from ever happening we
   will set a variable to indicate we are in the read_socket call and indicate
   which message we are processing since the winproc gets called recursively with different
   messages by the system.
*/

int
w32_read_socket (sd, bufp, numchars, waitp, expected)
     register int sd;
     register struct input_event *bufp;
     register int numchars;
     int waitp;
     int expected;
{
  int count = 0;
  int nbytes = 0;
  int items_pending;            /* How many items are in the X queue. */
  Win32Msg msg;
  struct frame *f;
  int event_found = 0;
  int prefix;
  Lisp_Object part;
  struct win32_display_info *dpyinfo = &one_win32_display_info;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
  BLOCK_INPUT;

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  if (numchars <= 0)
    abort ();                   /* Don't think this happens. */

  while (get_next_msg (&msg, FALSE))
    {
      switch (msg.msg.message)
	{
	case WM_PAINT:
	  {
	    f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	    if (f) 
	      {
		if (f->async_visible == 0)
		  {
		    f->async_visible = 1;
		    f->async_iconified = 0;
		    SET_FRAME_GARBAGED (f);
		  }
		else
		  {
		    /* Erase background again for safety.  */
		    win32_clear_rect (f, NULL, &msg.rect);
		    dumprectangle (f,
				   msg.rect.left,
				   msg.rect.top,
				   msg.rect.right-msg.rect.left+1,
				   msg.rect.bottom-msg.rect.top+1);
		  }
	      }
	  }
	  break;
	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->iconified)
	    {
	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      bufp->kind = non_ascii_keystroke;
	      bufp->code = msg.msg.wParam;
	      bufp->modifiers = win32_kbd_mods_to_emacs (msg.dwModifiers);
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->timestamp = msg.msg.time;
	      bufp++;
	      numchars--;
	      count++;
	    }
	  break;
	case WM_SYSCHAR:
	case WM_CHAR:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->iconified)
	    {
	      if (numchars > 1) 
		{
		  int add;
		  KEY_EVENT_RECORD key, *keyp = &key;

		  if (temp_index == sizeof temp_buffer / sizeof (short))
		    temp_index = 0;
		  
		  convert_to_key_event (&msg, keyp);
		  add = key_event (keyp, bufp);
		  XSETFRAME (bufp->frame_or_window, f);
		  if (add == -1)
		    {
		      /* The key pressed generated two characters, most likely
			 an accent character and a key that could not be
			 combined with it.  Prepend the message on the queue
			 again to process the second character (which is
			 being held internally in key_event), and process
			 the first character now.  */
		      prepend_msg (&msg);
		      add = 1;
		    }

		  /* Throw dead keys away.  However, be sure not to
		     throw away the dead key if it was produced using
		     AltGr and there is a valid AltGr scan code for
		     this key.  */
		  if (is_dead_key (msg.msg.wParam) 
		      && !((VkKeyScan ((char) bufp->code) & 0xff00) == 0x600))
		    break;

		  bufp += add;
		  numchars -= add;
		  count += add;
		} 
	      else 
		{
		  abort ();
		}
	    }
	  break;
	case WM_MOUSEMOVE:
	  if (dpyinfo->grabbed && last_mouse_frame
	      && FRAME_LIVE_P (last_mouse_frame))
	    f = last_mouse_frame;
	  else
	    f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f)
	    note_mouse_movement (f, &msg.msg);
	  else
	    clear_mouse_face (FRAME_WIN32_DISPLAY_INFO (f));
	  
	  break;
	case WM_LBUTTONDOWN:
	case WM_LBUTTONUP:
	case WM_MBUTTONDOWN:
	case WM_MBUTTONUP:
	case WM_RBUTTONDOWN:
	case WM_RBUTTONUP:
	  {
	    int button;
	    int up;
	    
	    if (dpyinfo->grabbed && last_mouse_frame
		&& FRAME_LIVE_P (last_mouse_frame))
	      f = last_mouse_frame;
	    else
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	    
	    if (f)
	      {
		if ((!dpyinfo->win32_focus_frame || f == dpyinfo->win32_focus_frame) 
		    && (numchars >= 1))
		  {
		    construct_mouse_click (bufp, &msg, f);
		    bufp++;
		    count++;
		    numchars--;
		  }
	      }
	    
	    parse_button (msg.msg.message, &button, &up);
	    
	    if (up)
	      {
		dpyinfo->grabbed &= ~ (1 << button);
	      }
	    else
	      {
		dpyinfo->grabbed |= (1 << button);
		last_mouse_frame = f;
	      }
	  }
	  
	  break;
	case WM_VSCROLL:
	  {
	    struct scroll_bar *bar = x_window_to_scroll_bar ((HWND)msg.msg.lParam);
	      
	    if (bar && numchars >= 1)
	      {
		if (x_scroll_bar_handle_click (bar, &msg, bufp))
		  {
		    bufp++;
		    count++;
		    numchars--;
		  }
	      }
	  }
	  
	  break;
	case WM_MOVE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->async_iconified)
	    {
	      f->output_data.win32->left_pos = LOWORD (msg.msg.lParam);
	      f->output_data.win32->top_pos = HIWORD (msg.msg.lParam);
	    }
	  
	  break;
	case WM_SIZE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->async_iconified && msg.msg.wParam != SIZE_MINIMIZED)
	    {
	      RECT rect;
	      int rows;
	      int columns;
	      int width;
	      int height;
	      
	      GetClientRect(msg.msg.hwnd, &rect);
	      
	      height = rect.bottom - rect.top + 1;
	      width = rect.right - rect.left + 1;
	      
	      rows = PIXEL_TO_CHAR_HEIGHT (f, height);
	      columns = PIXEL_TO_CHAR_WIDTH (f, width);
	      
	      /* Even if the number of character rows and columns has
		 not changed, the font size may have changed, so we need
		 to check the pixel dimensions as well.  */
	      
	      if (columns != f->width
		  || rows != f->height
		  || width != f->output_data.win32->pixel_width
		  || height != f->output_data.win32->pixel_height)
		{
		  /* I had set this to 0, 0 - I am not sure why?? */
		  
		  change_frame_size (f, rows, columns, 0, 1);
		  SET_FRAME_GARBAGED (f);
		  
		  f->output_data.win32->pixel_width = width;
		  f->output_data.win32->pixel_height = height;
		  f->output_data.win32->win_gravity = NorthWestGravity;
		}
	    }
	  
	  break;
	case WM_SETFOCUS:
	case WM_KILLFOCUS:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (msg.msg.message == WM_SETFOCUS)
	    {
	      x_new_focus_frame (dpyinfo, f);
	    }
	  else if (f == dpyinfo->win32_focus_frame)
	    x_new_focus_frame (dpyinfo, 0);
	  
	  break;
	case WM_SYSCOMMAND:
	  switch (msg.msg.wParam & 0xfff0)  /* Lower 4 bits used by Windows. */
	    {
	    case SC_CLOSE:
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	      
	      if (f)
		{
		  if (numchars == 0)
		    abort ();
		  
		  bufp->kind = delete_window_event;
		  XSETFRAME (bufp->frame_or_window, f);
		  bufp++;
		  count++;
		  numchars--;
		}
	      
	      break;
	    case SC_MINIMIZE:
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	      
	      if (f)
		{
		  f->async_visible = 1;
		  f->async_iconified = 1;
		  
		  bufp->kind = iconify_event;
		  XSETFRAME (bufp->frame_or_window, f);
		  bufp++;
		  count++;
		  numchars--;
		}
	      
	      break;
	    case SC_MAXIMIZE:
	    case SC_RESTORE:
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	      
	      if (f)
		{
		  f->async_visible = 1;
		  f->async_iconified = 0;
		  
		  /* wait_reading_process_input will notice this and update
		     the frame's display structures.  */
		  SET_FRAME_GARBAGED (f);
		  
		  if (f->iconified)
		    {
		      bufp->kind = deiconify_event;
		      XSETFRAME (bufp->frame_or_window, f);
		      bufp++;
		      count++;
		      numchars--;
		    }
		  else
		    /* Force a redisplay sooner or later
		       to update the frame titles
		       in case this is the second frame.  */
		    record_asynch_buffer_change ();
		}
	      
	      break;
	    }
	  
	  break;
	case WM_CLOSE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f)
	    {
	      if (numchars == 0)
		abort ();
	      
	      bufp->kind = delete_window_event;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp++;
	      count++;
	      numchars--;
	    }
	  
	  break;
	case WM_COMMAND:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f)
	    {
	      if (msg.msg.lParam == 0) 
		{
		  /* Came from window menu */
		  
		  extern Lisp_Object get_frame_menubar_event ();
		  Lisp_Object event = get_frame_menubar_event (f, msg.msg.wParam);
		  struct input_event buf;
		  Lisp_Object frame;
		  
		  XSETFRAME (frame, f);
		  buf.kind = menu_bar_event;
		  
		  /* Store initial menu bar event */
		  
		  if (!NILP (event))
		    {
		      buf.frame_or_window = Fcons (frame, Fcons (Qmenu_bar, Qnil));
		      kbd_buffer_store_event (&buf);
		    }
		  
		  /* Enqueue the events */
		  
		  while (!NILP (event))
		    {
		      buf.frame_or_window = Fcons (frame, XCONS (event)->car);
		      kbd_buffer_store_event (&buf);
		      event = XCONS (event)->cdr;
		    }
		} 
	      else 
		{
		  /* Came from popup menu */
		}
	    }
	  break;
	}
    }

  /* If the focus was just given to an autoraising frame,
     raise it now.  */
  /* ??? This ought to be able to handle more than one such frame.  */
  if (pending_autoraise_frame)
    {
      x_raise_frame (pending_autoraise_frame);
      pending_autoraise_frame = 0;
    }

  UNBLOCK_INPUT;
  return count;
}

/* Drawing the cursor.  */


/* Draw a hollow box cursor.  Don't change the inside of the box.  */

static void
x_draw_box (f)
     struct frame *f;
{
  RECT rect;
  HBRUSH hb;
  HDC hdc;
  
  hdc = get_frame_dc (f);
  
  hb = CreateSolidBrush (f->output_data.win32->cursor_pixel);
  
  rect.left = CHAR_TO_PIXEL_COL (f, curs_x);
  rect.top  = CHAR_TO_PIXEL_ROW (f, curs_y);
  rect.right = rect.left + FONT_WIDTH (f->output_data.win32->font);
  rect.bottom = rect.top + f->output_data.win32->line_height;

  FrameRect (hdc, &rect, hb);
  DeleteObject (hb);

  release_frame_dc (f, hdc);
}

/* Clear the cursor of frame F to background color,
   and mark the cursor as not shown.
   This is used when the text where the cursor is
   is about to be rewritten.  */

static void
clear_cursor (f)
     struct frame *f;
{
  if (! FRAME_VISIBLE_P (f)
      || f->phys_cursor_x < 0)
    return;

  x_display_cursor (f, 0);
  f->phys_cursor_x = -1;
}

/* Redraw the glyph at ROW, COLUMN on frame F, in the style
   HIGHLIGHT.  HIGHLIGHT is as defined for dumpglyphs.  Return the
   glyph drawn.  */

static void
x_draw_single_glyph (f, row, column, glyph, highlight)
     struct frame *f;
     int row, column;
     GLYPH glyph;
     int highlight;
{
  dumpglyphs (f,
	      CHAR_TO_PIXEL_COL (f, column),
	      CHAR_TO_PIXEL_ROW (f, row),
	      &glyph, 1, highlight, 0);
}

static void
x_display_bar_cursor (f, on)
     struct frame *f;
     int on;
{
  struct frame_glyphs *current_glyphs = FRAME_CURRENT_GLYPHS (f);

  /* This is pointless on invisible frames, and dangerous on garbaged
     frames; in the latter case, the frame may be in the midst of
     changing its size, and curs_x and curs_y may be off the frame.  */
  if (! FRAME_VISIBLE_P (f) || FRAME_GARBAGED_P (f))
    return;

  if (! on && f->phys_cursor_x < 0)
    return;

  /* If we're not updating, then we want to use the current frame's
     cursor position, not our local idea of where the cursor ought to be.  */
  if (f != updating_frame)
    {
      curs_x = FRAME_CURSOR_X (f);
      curs_y = FRAME_CURSOR_Y (f);
    }

  /* If there is anything wrong with the current cursor state, remove it.  */
  if (f->phys_cursor_x >= 0
      && (!on
	  || f->phys_cursor_x != curs_x
	  || f->phys_cursor_y != curs_y
	  || f->output_data.win32->current_cursor != bar_cursor))
    {
      /* Erase the cursor by redrawing the character underneath it.  */
      x_draw_single_glyph (f, f->phys_cursor_y, f->phys_cursor_x,
			   f->phys_cursor_glyph,
			   current_glyphs->highlight[f->phys_cursor_y]);
      f->phys_cursor_x = -1;
    }

  /* If we now need a cursor in the new place or in the new form, do it so.  */
  if (on
      && (f->phys_cursor_x < 0
	  || (f->output_data.win32->current_cursor != bar_cursor)))
    {
      f->phys_cursor_glyph
	= ((current_glyphs->enable[curs_y]
	    && curs_x < current_glyphs->used[curs_y])
	   ? current_glyphs->glyphs[curs_y][curs_x]
	   : SPACEGLYPH);
      win32_fill_area (f, NULL, f->output_data.win32->cursor_pixel,
		       CHAR_TO_PIXEL_COL (f, curs_x),
		       CHAR_TO_PIXEL_ROW (f, curs_y),
		       max (f->output_data.win32->cursor_width, 1),
		       f->output_data.win32->line_height);

      f->phys_cursor_x = curs_x;
      f->phys_cursor_y = curs_y;

      f->output_data.win32->current_cursor = bar_cursor;
    }
}


/* Turn the displayed cursor of frame F on or off according to ON.
   If ON is nonzero, where to put the cursor is specified
   by F->cursor_x and F->cursor_y.  */

static void
x_display_box_cursor (f, on)
     struct frame *f;
     int on;
{
  struct frame_glyphs *current_glyphs = FRAME_CURRENT_GLYPHS (f);

  /* This is pointless on invisible frames, and dangerous on garbaged
     frames; in the latter case, the frame may be in the midst of
     changing its size, and curs_x and curs_y may be off the frame.  */
  if (! FRAME_VISIBLE_P (f) || FRAME_GARBAGED_P (f))
    return;

  /* If cursor is off and we want it off, return quickly.  */
  if (!on && f->phys_cursor_x < 0)
    return;

  /* If we're not updating, then we want to use the current frame's
     cursor position, not our local idea of where the cursor ought to be.  */
  if (f != updating_frame)
    {
      curs_x = FRAME_CURSOR_X (f);
      curs_y = FRAME_CURSOR_Y (f);
    }

  /* If cursor is currently being shown and we don't want it to be
     or it is in the wrong place,
     or we want a hollow box and it's not so, (pout!)
     erase it.  */
  if (f->phys_cursor_x >= 0
      && (!on
	  || f->phys_cursor_x != curs_x
	  || f->phys_cursor_y != curs_y
	  || (f->output_data.win32->current_cursor != hollow_box_cursor
	      && (f != FRAME_WIN32_DISPLAY_INFO (f)->win32_highlight_frame))))
    {
      int mouse_face_here = 0;
      struct frame_glyphs *active_glyphs = FRAME_CURRENT_GLYPHS (f);

      /* If the cursor is in the mouse face area, redisplay that when
	 we clear the cursor.  */
      if (f == FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_mouse_frame
	  &&
	  (f->phys_cursor_y > FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_row
	   || (f->phys_cursor_y == FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_row
	       && f->phys_cursor_x >= FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_beg_col))
	  &&
	  (f->phys_cursor_y < FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_row
	   || (f->phys_cursor_y == FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_row
	       && f->phys_cursor_x < FRAME_WIN32_DISPLAY_INFO (f)->mouse_face_end_col))
	  /* Don't redraw the cursor's spot in mouse face
	     if it is at the end of a line (on a newline).
	     The cursor appears there, but mouse highlighting does not.  */
	  && active_glyphs->used[f->phys_cursor_y] > f->phys_cursor_x)
	mouse_face_here = 1;

      /* If the font is not as tall as a whole line,
	 we must explicitly clear the line's whole height.  */
      if (FONT_HEIGHT (f->output_data.win32->font) != f->output_data.win32->line_height)
	win32_clear_area (f, NULL,
			  CHAR_TO_PIXEL_COL (f, f->phys_cursor_x),
			  CHAR_TO_PIXEL_ROW (f, f->phys_cursor_y),
			  FONT_WIDTH (f->output_data.win32->font),
			  f->output_data.win32->line_height);
      /* Erase the cursor by redrawing the character underneath it.  */
      x_draw_single_glyph (f, f->phys_cursor_y, f->phys_cursor_x,
			   f->phys_cursor_glyph,
			   (mouse_face_here
			    ? 3
			    : current_glyphs->highlight[f->phys_cursor_y]));
      f->phys_cursor_x = -1;
    }

  /* If we want to show a cursor,
     or we want a box cursor and it's not so,
     write it in the right place.  */
  if (on
      && (f->phys_cursor_x < 0
	  || (f->output_data.win32->current_cursor != filled_box_cursor
	      && f == FRAME_WIN32_DISPLAY_INFO (f)->win32_highlight_frame)))
    {
      f->phys_cursor_glyph
	= ((current_glyphs->enable[curs_y]
	    && curs_x < current_glyphs->used[curs_y])
	   ? current_glyphs->glyphs[curs_y][curs_x]
	   : SPACEGLYPH);
      if (f != FRAME_WIN32_DISPLAY_INFO (f)->win32_highlight_frame)
	{
	  x_draw_box (f);
	  f->output_data.win32->current_cursor = hollow_box_cursor;
	}
      else
	{
	  x_draw_single_glyph (f, curs_y, curs_x,
			       f->phys_cursor_glyph, 2);
	  f->output_data.win32->current_cursor = filled_box_cursor;
	}

      f->phys_cursor_x = curs_x;
      f->phys_cursor_y = curs_y;
    }
}

x_display_cursor (f, on)
     struct frame *f;
     int on;
{
  BLOCK_INPUT;

  if (FRAME_DESIRED_CURSOR (f) == filled_box_cursor)
    x_display_box_cursor (f, on);
  else if (FRAME_DESIRED_CURSOR (f) == bar_cursor)
    x_display_bar_cursor (f, on);
  else
    /* Those are the only two we have implemented!  */
    abort ();

  UNBLOCK_INPUT;
}

/* Changing the font of the frame.  */

/* Give frame F the font named FONTNAME as its default font, and
   return the full name of that font.  FONTNAME may be a wildcard
   pattern; in that case, we choose some font that fits the pattern.
   The return value shows which font we chose.  */

Lisp_Object
x_new_font (f, fontname)
     struct frame *f;
     register char *fontname;
{
  int already_loaded;
  int n_matching_fonts;
  XFontStruct *font_info;
  char new_font_name[101];

  /* Get a font which matches this name */
  {
      LOGFONT lf;

      if (!x_to_win32_font(fontname, &lf)
	  || !win32_to_x_font(&lf, new_font_name, 100))
      {
	  return Qnil;
      }
  }

  /* See if we've already loaded a matching font. */
  already_loaded = -1;

  {
      int i;

      for (i = 0; i < FRAME_WIN32_DISPLAY_INFO (f)->n_fonts; i++)
	  if (!strcmp (FRAME_WIN32_DISPLAY_INFO (f)->font_table[i].name, new_font_name))
	  {
	      already_loaded = i;
	      fontname = FRAME_WIN32_DISPLAY_INFO (f)->font_table[i].name;
	      break;
	  }
  }

  /* If we have, just return it from the table.  */
  if (already_loaded >= 0)
    f->output_data.win32->font = FRAME_WIN32_DISPLAY_INFO (f)->font_table[already_loaded].font;
  /* Otherwise, load the font and add it to the table.  */
  else
    {
      XFontStruct *font;
      int n_fonts;

      font = win32_load_font(FRAME_WIN32_DISPLAY_INFO (f), fontname);

      if (! font)
	{
	  return Qnil;
	}

      /* Do we need to create the table?  */
      if (FRAME_WIN32_DISPLAY_INFO (f)->font_table_size == 0)
	{
	  FRAME_WIN32_DISPLAY_INFO (f)->font_table_size = 16;
	  FRAME_WIN32_DISPLAY_INFO (f)->font_table
	    = (struct font_info *) xmalloc (FRAME_WIN32_DISPLAY_INFO (f)->font_table_size
					    * sizeof (struct font_info));
	}
      /* Do we need to grow the table?  */
      else if (FRAME_WIN32_DISPLAY_INFO (f)->n_fonts
	       >= FRAME_WIN32_DISPLAY_INFO (f)->font_table_size)
	{
	  FRAME_WIN32_DISPLAY_INFO (f)->font_table_size *= 2;
	  FRAME_WIN32_DISPLAY_INFO (f)->font_table
	    = (struct font_info *) xrealloc (FRAME_WIN32_DISPLAY_INFO (f)->font_table,
					     (FRAME_WIN32_DISPLAY_INFO (f)->font_table_size
					      * sizeof (struct font_info)));
	}

      n_fonts = FRAME_WIN32_DISPLAY_INFO (f)->n_fonts;
      FRAME_WIN32_DISPLAY_INFO (f)->font_table[n_fonts].name = (char *) xmalloc (strlen (fontname) + 1);
      bcopy (fontname, FRAME_WIN32_DISPLAY_INFO (f)->font_table[n_fonts].name, strlen (fontname) + 1);
      f->output_data.win32->font = FRAME_WIN32_DISPLAY_INFO (f)->font_table[n_fonts].font = font;
      FRAME_WIN32_DISPLAY_INFO (f)->n_fonts++;
    }

  /* Compute the scroll bar width in character columns.  */
  if (f->scroll_bar_pixel_width > 0)
    {
      int wid = FONT_WIDTH (f->output_data.win32->font);
      f->scroll_bar_cols = (f->scroll_bar_pixel_width + wid-1) / wid;
    }
  else
    f->scroll_bar_cols = 2;

  /* Now make the frame display the given font.  */
  if (FRAME_WIN32_WINDOW (f) != 0)
    {
      frame_update_line_height (f);
      x_set_window_size (f, 0, f->width, f->height);
    }
  else
    /* If we are setting a new frame's font for the first time,
       there are no faces yet, so this font's height is the line height.  */
    f->output_data.win32->line_height = FONT_HEIGHT (f->output_data.win32->font);

  {
    Lisp_Object lispy_name;

    lispy_name = build_string (fontname);

    return lispy_name;
  }
}

x_calc_absolute_position (f)
     struct frame *f;
{
  Window win, child;
  POINT pt;
  int flags = f->output_data.win32->size_hint_flags;

  pt.x = pt.y = 0;

  /* Find the position of the outside upper-left corner of
     the inner window, with respect to the outer window.  */
  if (f->output_data.win32->parent_desc != FRAME_WIN32_DISPLAY_INFO (f)->root_window)
    {
      BLOCK_INPUT;
      MapWindowPoints (FRAME_WIN32_WINDOW (f),
		       f->output_data.win32->parent_desc,
		       &pt, 1);
      UNBLOCK_INPUT;
    }

  {
      RECT rt;
      rt.left = rt.right = rt.top = rt.bottom = 0;
      
      BLOCK_INPUT;
      AdjustWindowRect(&rt, f->output_data.win32->dwStyle,
		       FRAME_EXTERNAL_MENU_BAR (f));
      UNBLOCK_INPUT;

      pt.x += (rt.right - rt.left);
      pt.y += (rt.bottom - rt.top);
  }

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->output_data.win32->left_pos = (FRAME_WIN32_DISPLAY_INFO (f)->width
			      - 2 * f->output_data.win32->border_width - pt.x
			      - PIXEL_WIDTH (f)
			      + f->output_data.win32->left_pos);

  if (flags & YNegative)
    f->output_data.win32->top_pos = (FRAME_WIN32_DISPLAY_INFO (f)->height
			     - 2 * f->output_data.win32->border_width - pt.y
			     - PIXEL_HEIGHT (f)
			     + f->output_data.win32->top_pos);
  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->output_data.win32->size_hint_flags &= ~ (XNegative | YNegative);
}

/* CHANGE_GRAVITY is 1 when calling from Fset_frame_position,
   to really change the position, and 0 when calling from
   x_make_frame_visible (in that case, XOFF and YOFF are the current
   position values).  It is -1 when calling from x_set_frame_parameters,
   which means, do adjust for borders but don't change the gravity.  */

x_set_offset (f, xoff, yoff, change_gravity)
     struct frame *f;
     register int xoff, yoff;
     int change_gravity;
{
  int modified_top, modified_left;

  if (change_gravity > 0)
    {
      f->output_data.win32->top_pos = yoff;
      f->output_data.win32->left_pos = xoff;
      f->output_data.win32->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->output_data.win32->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->output_data.win32->size_hint_flags |= YNegative;
      f->output_data.win32->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, (long) 0, 0);

  /* It is a mystery why we need to add the border_width here
     when the frame is already visible, but experiment says we do.  */
  modified_left = f->output_data.win32->left_pos;
  modified_top = f->output_data.win32->top_pos;
  if (change_gravity != 0)
    {
      modified_left += f->output_data.win32->border_width;
      modified_top += f->output_data.win32->border_width;
    }

  my_set_window_pos (FRAME_WIN32_WINDOW (f),
		     NULL,
		     modified_left, modified_top,
		     0,0,
		     SWP_NOZORDER | SWP_NOSIZE);
  UNBLOCK_INPUT;
}

/* Call this to change the size of frame F's x-window.
   If CHANGE_GRAVITY is 1, we change to top-left-corner window gravity
   for this size change and subsequent size changes.
   Otherwise we leave the window gravity unchanged.  */

x_set_window_size (f, change_gravity, cols, rows)
     struct frame *f;
     int change_gravity;
     int cols, rows;
{
  int pixelwidth, pixelheight;
  
  BLOCK_INPUT;
  
  check_frame_size (f, &rows, &cols);
  f->output_data.win32->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.win32->font)));
  pixelwidth = CHAR_TO_PIXEL_WIDTH (f, cols);
  pixelheight = CHAR_TO_PIXEL_HEIGHT (f, rows);
  
  f->output_data.win32->win_gravity = NorthWestGravity;
  x_wm_set_size_hint (f, (long) 0, 0);
  
  {
    RECT rect;

    rect.left = rect.top = 0;
    rect.right = pixelwidth;
    rect.bottom = pixelheight;
      
    AdjustWindowRect(&rect, f->output_data.win32->dwStyle,
		     FRAME_EXTERNAL_MENU_BAR (f));
      
    /* All windows have an extra pixel */

    my_set_window_pos (FRAME_WIN32_WINDOW (f),
		       NULL, 
		       0, 0,
		       rect.right - rect.left + 1,
		       rect.bottom - rect.top + 1,
		       SWP_NOZORDER | SWP_NOMOVE);
  }
  
  /* Now, strictly speaking, we can't be sure that this is accurate,
     but the window manager will get around to dealing with the size
     change request eventually, and we'll hear how it went when the
     ConfigureNotify event gets here.
     
     We could just not bother storing any of this information here,
     and let the ConfigureNotify event set everything up, but that
     might be kind of confusing to the lisp code, since size changes
     wouldn't be reported in the frame parameters until some random
     point in the future when the ConfigureNotify event arrives.  */
  change_frame_size (f, rows, cols, 0, 0);
  PIXEL_WIDTH (f) = pixelwidth;
  PIXEL_HEIGHT (f) = pixelheight;

  /* If cursor was outside the new size, mark it as off.  */
  if (f->phys_cursor_y >= rows
      || f->phys_cursor_x >= cols)
    {
      f->phys_cursor_x = -1;
      f->phys_cursor_y = -1;
    }

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);
  
  UNBLOCK_INPUT;
}

/* Mouse warping.  */

void
x_set_mouse_pixel_position (f, pix_x, pix_y)
     struct frame *f;
     int pix_x, pix_y;
{
  BLOCK_INPUT;

  pix_x += f->output_data.win32->left_pos;
  pix_y += f->output_data.win32->top_pos;

  SetCursorPos (pix_x, pix_y);

  UNBLOCK_INPUT;
}

void
x_set_mouse_position (f, x, y)
     struct frame *f;
     int x, y;
{
  int pix_x, pix_y;

  pix_x = CHAR_TO_PIXEL_COL (f, x) + FONT_WIDTH  (f->output_data.win32->font) / 2;
  pix_y = CHAR_TO_PIXEL_ROW (f, y) + f->output_data.win32->line_height / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > PIXEL_WIDTH (f)) pix_x = PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > PIXEL_HEIGHT (f)) pix_y = PIXEL_HEIGHT (f);

  x_set_mouse_pixel_position (f, pix_x, pix_y);
}

/* focus shifting, raising and lowering.  */

x_focus_on_frame (f)
     struct frame *f;
{
}

x_unfocus_frame (f)
     struct frame *f;
{
}

/* Raise frame F.  */

x_raise_frame (f)
     struct frame *f;
{
//  if (f->async_visible)
    {
      BLOCK_INPUT;
      my_set_window_pos (FRAME_WIN32_WINDOW (f),
		         HWND_TOP,
		         0, 0, 0, 0,
		         SWP_NOSIZE | SWP_NOMOVE);
      UNBLOCK_INPUT;
    }
}

/* Lower frame F.  */

x_lower_frame (f)
     struct frame *f;
{
//  if (f->async_visible)
    {
      BLOCK_INPUT;
      my_set_window_pos (FRAME_WIN32_WINDOW (f),
		         HWND_BOTTOM,
		         0, 0, 0, 0,
		         SWP_NOSIZE | SWP_NOMOVE);
      UNBLOCK_INPUT;
    }
}

static void
win32_frame_raise_lower (f, raise)
     FRAME_PTR f;
     int raise;
{
  if (raise)
    x_raise_frame (f);
  else
    x_lower_frame (f);
}

/* Change of visibility.  */

/* This tries to wait until the frame is really visible.
   However, if the window manager asks the user where to position
   the frame, this will return before the user finishes doing that.
   The frame will not actually be visible at that time,
   but it will become visible later when the window manager
   finishes with it.  */

x_make_frame_visible (f)
     struct frame *f;
{
  BLOCK_INPUT;

  if (! FRAME_VISIBLE_P (f))
    {
      /* We test FRAME_GARBAGED_P here to make sure we don't
	 call x_set_offset a second time
	 if we get to x_make_frame_visible a second time
	 before the window gets really visible.  */
      if (! FRAME_ICONIFIED_P (f)
	  && ! f->output_data.win32->asked_for_visible)
      {
	x_set_offset (f, f->output_data.win32->left_pos, f->output_data.win32->top_pos, 0);
//	SetForegroundWindow (FRAME_WIN32_WINDOW (f));
      }

      f->output_data.win32->asked_for_visible = 1;

      my_show_window (FRAME_WIN32_WINDOW (f), SW_SHOWNORMAL);
    }

  /* Synchronize to ensure Emacs knows the frame is visible
     before we do anything else.  We do this loop with input not blocked
     so that incoming events are handled.  */
  {
    Lisp_Object frame;
    int count = input_signal_count;

    /* This must come after we set COUNT.  */
    UNBLOCK_INPUT;

    XSETFRAME (frame, f);

    while (1)
      {
	/* Once we have handled input events,
	   we should have received the MapNotify if one is coming.
	   So if we have not got it yet, stop looping.
	   Some window managers make their own decisions
	   about visibility.  */
	if (input_signal_count != count)
	  break;
	/* Machines that do polling rather than SIGIO have been observed
	   to go into a busy-wait here.  So we'll fake an alarm signal
	   to let the handler know that there's something to be read.
	   We used to raise a real alarm, but it seems that the handler
	   isn't always enabled here.  This is probably a bug.  */
	if (input_polling_used ())
	  {
	    /* It could be confusing if a real alarm arrives while processing
	       the fake one.  Turn it off and let the handler reset it.  */
	    alarm (0);
	    input_poll_signal ();
	  }
	/* Once we have handled input events,
	   we should have received the MapNotify if one is coming.
	   So if we have not got it yet, stop looping.
	   Some window managers make their own decisions
	   about visibility.  */
	if (input_signal_count != count)
	  break;
      }
    FRAME_SAMPLE_VISIBILITY (f);
  }
}

/* Change from mapped state to withdrawn state. */

/* Make the frame visible (mapped and not iconified).  */

x_make_frame_invisible (f)
     struct frame *f;
{
  Window window;
  
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_WIN32_DISPLAY_INFO (f)->win32_highlight_frame == f)
    FRAME_WIN32_DISPLAY_INFO (f)->win32_highlight_frame = 0;
  
  BLOCK_INPUT;
  
  my_show_window (FRAME_WIN32_WINDOW (f), SW_HIDE);
  
  /* We can't distinguish this from iconification
     just by the event that we get from the server.
     So we can't win using the usual strategy of letting
     FRAME_SAMPLE_VISIBILITY set this.  So do it by hand,
     and synchronize with the server to make sure we agree.  */
  f->visible = 0;
  FRAME_ICONIFIED_P (f) = 0;
  f->async_visible = 0;
  f->async_iconified = 0;
  
  UNBLOCK_INPUT;
}

/* Change window state from mapped to iconified. */

void
x_iconify_frame (f)
     struct frame *f;
{
  int result;

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_WIN32_DISPLAY_INFO (f)->win32_highlight_frame == f)
    FRAME_WIN32_DISPLAY_INFO (f)->win32_highlight_frame = 0;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

  my_show_window (FRAME_WIN32_WINDOW (f), SW_SHOWMINIMIZED);
  /* The frame doesn't seem to be lowered automatically. */
  x_lower_frame (f);

  f->async_iconified = 1;

  UNBLOCK_INPUT;
}

/* Destroy the window of frame F.  */

x_destroy_window (f)
     struct frame *f;
{
  struct win32_display_info *dpyinfo = FRAME_WIN32_DISPLAY_INFO (f);

  BLOCK_INPUT;

  my_destroy_window (f, FRAME_WIN32_WINDOW (f));
  free_frame_menubar (f);
  free_frame_faces (f);

  xfree (f->output_data.win32);
  f->output_data.win32 = 0;
  if (f == dpyinfo->win32_focus_frame)
    dpyinfo->win32_focus_frame = 0;
  if (f == dpyinfo->win32_focus_event_frame)
    dpyinfo->win32_focus_event_frame = 0;
  if (f == dpyinfo->win32_highlight_frame)
    dpyinfo->win32_highlight_frame = 0;

  dpyinfo->reference_count--;

  if (f == dpyinfo->mouse_face_mouse_frame)
    {
      dpyinfo->mouse_face_beg_row
	= dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row
	= dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }

  UNBLOCK_INPUT;
}

/* Setting window manager hints.  */

/* Set the normal size hints for the window manager, for frame F.
   FLAGS is the flags word to use--or 0 meaning preserve the flags
   that the window now has.
   If USER_POSITION is nonzero, we set the USPosition
   flag (this is useful when FLAGS is 0).  */

x_wm_set_size_hint (f, flags, user_position)
     struct frame *f;
     long flags;
     int user_position;
{
  Window window = FRAME_WIN32_WINDOW (f);

  flexlines = f->height;

  enter_crit ();

  SetWindowLong (window, WND_X_UNITS_INDEX, FONT_WIDTH (f->output_data.win32->font));
  SetWindowLong (window, WND_Y_UNITS_INDEX, f->output_data.win32->line_height);

  leave_crit ();
}

/* Window manager things */
x_wm_set_icon_position (f, icon_x, icon_y)
     struct frame *f;
     int icon_x, icon_y;
{
#if 0
  Window window = FRAME_WIN32_WINDOW (f);

  f->display.x->wm_hints.flags |= IconPositionHint;
  f->display.x->wm_hints.icon_x = icon_x;
  f->display.x->wm_hints.icon_y = icon_y;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->display.x->wm_hints);
#endif
}


/* Initialization.  */

#ifdef USE_X_TOOLKIT
static XrmOptionDescRec emacs_options[] = {
  {"-geometry", ".geometry", XrmoptionSepArg, NULL},
  {"-iconic",   ".iconic", XrmoptionNoArg, (XtPointer) "yes"},

  {"-internal-border-width", "*EmacsScreen.internalBorderWidth",
     XrmoptionSepArg, NULL},
  {"-ib",       "*EmacsScreen.internalBorderWidth", XrmoptionSepArg, NULL},

  {"-T",        "*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-wn",       "*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-title",    "*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-iconname", "*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-in",       "*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-mc",       "*pointerColor", XrmoptionSepArg, (XtPointer) NULL},
  {"-cr",       "*cursorColor", XrmoptionSepArg, (XtPointer) NULL}
};
#endif /* USE_X_TOOLKIT */

static int win32_initialized = 0;

struct win32_display_info *
win32_term_init (display_name, xrm_option, resource_name)
     Lisp_Object display_name;
     char *xrm_option;
     char *resource_name;
{
  Lisp_Object frame;
  char *defaultvalue;
  struct win32_display_info *dpyinfo;
  HDC hdc;
  
  BLOCK_INPUT;
  
  if (!win32_initialized)
    {
      win32_initialize ();
      win32_initialized = 1;
    }
  
  {
    int argc = 0;
    char *argv[3];

    argv[0] = "";
    argc = 1;
    if (xrm_option)
      {
	argv[argc++] = "-xrm";
	argv[argc++] = xrm_option;
      }
  }
  
  dpyinfo = &one_win32_display_info;
  
  /* Put this display on the chain.  */
  dpyinfo->next = NULL;
  
  /* Put it on win32_display_name_list as well, to keep them parallel.  */ 
  win32_display_name_list = Fcons (Fcons (display_name, Qnil),
				   win32_display_name_list);
  dpyinfo->name_list_element = XCONS (win32_display_name_list)->car;
  
  dpyinfo->win32_id_name
    = (char *) xmalloc (XSTRING (Vinvocation_name)->size
			+ XSTRING (Vsystem_name)->size
			+ 2);
  sprintf (dpyinfo->win32_id_name, "%s@%s",
	   XSTRING (Vinvocation_name)->data, XSTRING (Vsystem_name)->data);

#if 0
  xrdb = x_load_resources (dpyinfo->display, xrm_option,
			   resource_name, EMACS_CLASS);
  
  /* Put the rdb where we can find it in a way that works on
     all versions.  */
  dpyinfo->xrdb = xrdb;
#endif
  hdc = GetDC (GetDesktopWindow ());
  
  dpyinfo->height = GetDeviceCaps (hdc, VERTRES);
  dpyinfo->width = GetDeviceCaps (hdc, HORZRES);
  dpyinfo->root_window = GetDesktopWindow ();
  dpyinfo->n_planes = GetDeviceCaps (hdc, PLANES);
  dpyinfo->n_cbits = GetDeviceCaps (hdc, BITSPIXEL);
  dpyinfo->height_in = GetDeviceCaps (hdc, LOGPIXELSX);
  dpyinfo->width_in = GetDeviceCaps (hdc, LOGPIXELSY);
  dpyinfo->has_palette = GetDeviceCaps (hdc, RASTERCAPS) & RC_PALETTE;
  dpyinfo->grabbed = 0;
  dpyinfo->reference_count = 0;
  dpyinfo->n_fonts = 0;
  dpyinfo->font_table_size = 0;
  dpyinfo->bitmaps = 0;
  dpyinfo->bitmaps_size = 0;
  dpyinfo->bitmaps_last = 0;
  dpyinfo->mouse_face_mouse_frame = 0;
  dpyinfo->mouse_face_deferred_gc = 0;
  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_face_id = 0;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_mouse_x = dpyinfo->mouse_face_mouse_y = 0;
  dpyinfo->mouse_face_defer = 0;
  dpyinfo->win32_focus_frame = 0;
  dpyinfo->win32_focus_event_frame = 0;
  dpyinfo->win32_highlight_frame = 0;
  
  ReleaseDC (GetDesktopWindow (), hdc);

  /* Determine if there is a middle mouse button, to allow parse_button
     to decide whether right mouse events should be mouse-2 or
     mouse-3. */
  XSETINT (Vwin32_num_mouse_buttons, GetSystemMetrics (SM_CMOUSEBUTTONS));

  /* initialise palette with white and black */
  {
    COLORREF color;
    defined_color (0, "white", &color, 1);
    defined_color (0, "black", &color, 1);
  }

#ifndef F_SETOWN_BUG
#ifdef F_SETOWN
#ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (connection, F_SETOWN, -getpid ());
#else /* ! defined (F_SETOWN_SOCK_NEG) */
  fcntl (connection, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN_SOCK_NEG) */
#endif /* ! defined (F_SETOWN) */
#endif /* F_SETOWN_BUG */

#ifdef SIGIO
  if (interrupt_input)
    init_sigio (connection);
#endif /* ! defined (SIGIO) */

  UNBLOCK_INPUT;

  return dpyinfo;
}

/* Get rid of display DPYINFO, assuming all frames are already gone.  */

void
x_delete_display (dpyinfo)
     struct win32_display_info *dpyinfo;
{
  /* Discard this display from win32_display_name_list and win32_display_list.
     We can't use Fdelq because that can quit.  */
  if (! NILP (win32_display_name_list)
      && EQ (XCONS (win32_display_name_list)->car, dpyinfo->name_list_element))
    win32_display_name_list = XCONS (win32_display_name_list)->cdr;
  else
    {
      Lisp_Object tail;

      tail = win32_display_name_list;
      while (CONSP (tail) && CONSP (XCONS (tail)->cdr))
	{
	  if (EQ (XCONS (XCONS (tail)->cdr)->car,
		  dpyinfo->name_list_element))
	    {
	      XCONS (tail)->cdr = XCONS (XCONS (tail)->cdr)->cdr;
	      break;
	    }
	  tail = XCONS (tail)->cdr;
	}
    }

  /* free palette table */
  {
    struct win32_palette_entry * plist;

    plist = dpyinfo->color_list;
    while (plist)
    {
      struct win32_palette_entry * pentry = plist;
      plist = plist->next;
      xfree(pentry);
    }
    dpyinfo->color_list = NULL;
    if (dpyinfo->palette)
      DeleteObject(dpyinfo->palette);
  }
  xfree (dpyinfo->font_table);
  xfree (dpyinfo->win32_id_name);
}

/* Set up use of Win32.  */

DWORD win_msg_worker ();

win32_initialize ()
{
  clear_frame_hook = win32_clear_frame;
  clear_end_of_line_hook = win32_clear_end_of_line;
  ins_del_lines_hook = win32_ins_del_lines;
  change_line_highlight_hook = win32_change_line_highlight;
  insert_glyphs_hook = win32_insert_glyphs;
  write_glyphs_hook = win32_write_glyphs;
  delete_glyphs_hook = win32_delete_glyphs;
  ring_bell_hook = win32_ring_bell;
  reset_terminal_modes_hook = win32_reset_terminal_modes;
  set_terminal_modes_hook = win32_set_terminal_modes;
  update_begin_hook = win32_update_begin;
  update_end_hook = win32_update_end;
  set_terminal_window_hook = win32_set_terminal_window;
  read_socket_hook = w32_read_socket;
  frame_up_to_date_hook = win32_frame_up_to_date;
  cursor_to_hook = win32_cursor_to;
  reassert_line_highlight_hook = win32_reassert_line_highlight;
  mouse_position_hook = win32_mouse_position;
  frame_rehighlight_hook = win32_frame_rehighlight;
  frame_raise_lower_hook = win32_frame_raise_lower;
  set_vertical_scroll_bar_hook = win32_set_vertical_scroll_bar;
  condemn_scroll_bars_hook = win32_condemn_scroll_bars;
  redeem_scroll_bar_hook = win32_redeem_scroll_bar;
  judge_scroll_bars_hook = win32_judge_scroll_bars;

  scroll_region_ok = 1;         /* we'll scroll partial frames */
  char_ins_del_ok = 0;          /* just as fast to write the line */
  line_ins_del_ok = 1;          /* we'll just blt 'em */
  fast_clear_end_of_line = 1;   /* X does this well */
  memory_below_frame = 0;       /* we don't remember what scrolls
				   off the bottom */
  baud_rate = 19200;

  /* Try to use interrupt input; if we can't, then start polling.  */
  Fset_input_mode (Qt, Qnil, Qt, Qnil);

  /* Create the window thread - it will terminate itself or when the app terminates */

  init_crit ();

  dwMainThreadId = GetCurrentThreadId ();
  DuplicateHandle (GetCurrentProcess (), GetCurrentThread (), 
		   GetCurrentProcess (), &hMainThread, 0, TRUE, DUPLICATE_SAME_ACCESS);

  /* Wait for thread to start */

  {
    MSG msg;

    PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE);

    hWinThread = CreateThread (NULL, 0, 
			       (LPTHREAD_START_ROUTINE) win_msg_worker, 
			       0, 0, &dwWinThreadId);

    GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
  }
  
  /* It is desirable that mainThread should have the same notion of
     focus window and active window as winThread.  Unfortunately, the
     following call to AttachThreadInput, which should do precisely what
     we need, causes major problems when Emacs is linked as a console
     program.  Unfortunately, we have good reasons for doing that, so
     instead we need to send messages to winThread to make some API
     calls for us (ones that affect, or depend on, the active/focus
     window state.  */
#ifdef ATTACH_THREADS
  AttachThreadInput (dwMainThreadId, dwWinThreadId, TRUE);
#endif
}

void
syms_of_win32term ()
{
  staticpro (&win32_display_name_list);
  win32_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;

  staticpro (&Qvendor_specific_keysyms);
  Qvendor_specific_keysyms = intern ("vendor-specific-keysyms");

  DEFVAR_INT ("win32-num-mouse-buttons",
	      &Vwin32_num_mouse_buttons,
	      "Number of physical mouse buttons.");
  Vwin32_num_mouse_buttons = Qnil;

  DEFVAR_LISP ("win32-swap-mouse-buttons",
	      &Vwin32_swap_mouse_buttons,
	      "Swap the mapping of middle and right mouse buttons.\n\
When nil, middle button is mouse-2 and right button is mouse-3.");
  Vwin32_swap_mouse_buttons = Qnil;
}
