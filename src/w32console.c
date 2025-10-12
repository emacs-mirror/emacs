/* Terminal hooks for GNU Emacs on the Microsoft Windows API.
   Copyright (C) 1992, 1999, 2001-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/*
   Tim Fleehart (apollo@online.com)		1-17-92
   Geoff Voelker (voelker@cs.washington.edu)	9-12-93
   Ewan Townshend (ewan@etown.dev)              2025-08
   * c. ~ 2025: 24bit RGB support in Windows (10+) Terminal
   * https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
*/


#include <config.h>

#include <stdio.h>
#include <windows.h>

#include "lisp.h"
#include "coding.h"
#include "termchar.h"	/* for FRAME_TTY */
#include "dispextern.h"	/* for tty_defined_color */
#include "menu.h"	/* for tty_menu_show */
#include "w32term.h"
#include "w32common.h"	/* for os_subtype */
#include "w32inevt.h"

#ifdef WINDOWSNT
#include "w32.h"	/* for syms_of_ntterm */
#endif

static void w32con_move_cursor (struct frame *f, int row, int col);
static void w32con_clear_to_end (struct frame *f);
static void w32con_clear_frame (struct frame *f);
static void w32con_clear_end_of_line (struct frame *f, int);
static void w32con_ins_del_lines (struct frame *f, int vpos, int n);
static void w32con_insert_glyphs (struct frame *f, struct glyph *start, int len);
static void w32con_write_glyphs (struct frame *f, struct glyph *string, int len);
static void w32con_delete_glyphs (struct frame *f, int n);
static void w32con_reset_terminal_modes (struct terminal *t);
static void w32con_set_terminal_modes (struct terminal *t);
static void w32con_update_begin (struct frame * f);
static void w32con_update_end (struct frame * f);
static WORD w32_face_attributes (struct frame *f, int face_id);
static int  w32con_write_vt_seq (const char *);
static void turn_on_face (struct frame *, int face_id);
static void turn_off_face (struct frame *, int face_id);
static COORD w32con_get_cursor_coords ();

static COORD	cursor_coords;
static HANDLE	prev_screen, cur_screen;
static WORD	char_attr_normal;
static DWORD	prev_console_mode;
static int      bg_normal;
static int      fg_normal;

static CONSOLE_CURSOR_INFO console_cursor_info;
#ifndef USE_SEPARATE_SCREEN
static CONSOLE_CURSOR_INFO prev_console_cursor;
#endif

extern HANDLE  keyboard_handle;
HANDLE  keyboard_handle;
int w32_console_unicode_input;
extern int w32_use_virtual_terminal;
int w32_use_virtual_terminal = 1;
extern struct tty_display_info *current_tty;
struct tty_display_info *current_tty = NULL;

/* Setting this as the ctrl handler prevents emacs from being killed when
   someone hits ^C in a 'suspended' session (child shell).
   Also ignore Ctrl-Break signals.  */

BOOL ctrl_c_handler (unsigned long);

BOOL
ctrl_c_handler (unsigned long type)
{
  /* Only ignore "interrupt" events when running interactively.  */
  return (!noninteractive
	  && (type == CTRL_C_EVENT || type == CTRL_BREAK_EVENT));
}

#define DEFAULTP(p)							\
  (p == FACE_TTY_DEFAULT_COLOR						\
   || p == FACE_TTY_DEFAULT_FG_COLOR					\
   || p == FACE_TTY_DEFAULT_BG_COLOR)

#define SEQMAX 256 /* Arbitrary upper limit on VT sequence size */

#define SSPRINTF(buf, i, sz, fmt, ...)					\
  do {									\
    eassert (*i < sz && sz <= SEQMAX);					\
    if (fmt && *i < sz && sz <= SEQMAX)				\
      *i += snprintf (buf + *i, sz - *i, fmt, __VA_ARGS__);		\
  } while (0)

/* Writes virtual terminal sequence to screen */
static int
w32con_write_vt_seq (const char *seq)
{
  char buf[SEQMAX];
  DWORD n = 0, k = 0;
  SSPRINTF (buf, &n, SEQMAX, seq, NULL);
  if (n) WriteConsole (cur_screen, (LPCSTR) buf, n, &k, NULL);
  return k;
}

static COORD
w32con_get_cursor_coords ()
{
  CONSOLE_SCREEN_BUFFER_INFO info;
  GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), &info);
  return info.dwCursorPosition;
}

/* Move the cursor to (ROW, COL) on FRAME.  */
static void
w32con_move_cursor (struct frame *f, int row, int col)
{
  cursor_coords.X = col;
  cursor_coords.Y = row;

  /* TODO: for multi-tty support, cur_screen should be replaced with a
     reference to the terminal for this frame.  */
  SetConsoleCursorPosition (cur_screen, cursor_coords);
}

void
w32con_hide_cursor (void)
{
  GetConsoleCursorInfo (cur_screen, &console_cursor_info);
  console_cursor_info.bVisible = FALSE;
  SetConsoleCursorInfo (cur_screen, &console_cursor_info);
}

void
w32con_show_cursor (void)
{
  GetConsoleCursorInfo (cur_screen, &console_cursor_info);
  console_cursor_info.bVisible = TRUE;
  SetConsoleCursorInfo (cur_screen, &console_cursor_info);
}

/* Clear from cursor to end of screen.  */
static void
w32con_clear_to_end (struct frame *f)
{
  w32con_clear_end_of_line (f, FRAME_COLS (f) - 1);
  w32con_ins_del_lines (f, cursor_coords.Y, FRAME_TOTAL_LINES (f) - cursor_coords.Y - 1);
}

/* Clear the frame.  */
static void
w32con_clear_frame (struct frame *f)
{
  COORD	     dest;
  int        n;
  DWORD      r;
  CONSOLE_SCREEN_BUFFER_INFO info;

  GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), &info);

  /* Remember that the screen buffer might be wider than the window.  */
  n = FRAME_TOTAL_LINES (f) * info.dwSize.X;
  dest.X = dest.Y = 0;

  FillConsoleOutputAttribute (cur_screen, char_attr_normal, n, dest, &r);
  FillConsoleOutputCharacter (cur_screen, ' ', n, dest, &r);

  w32con_move_cursor (f, 0, 0);
}


static struct glyph glyph_base[80];
static struct glyph *glyphs = glyph_base;
static size_t glyphs_len = ARRAYELTS (glyph_base);
static BOOL  ceol_initialized = FALSE;

/* Clear from Cursor to end (what's "standout marker"?).  */
static void
w32con_clear_end_of_line (struct frame *f, int end)
{
  /* Time to reallocate our "empty row"?  With today's large screens,
     it is not unthinkable to see TTY frames well in excess of
     80-character width.  */
  if (end - cursor_coords.X > glyphs_len)
    {
      if (glyphs == glyph_base)
	glyphs = NULL;
      glyphs = xrealloc (glyphs, FRAME_COLS (f) * sizeof (struct glyph));
      glyphs_len = FRAME_COLS (f);
      ceol_initialized = FALSE;
    }
  if (!ceol_initialized)
    {
      int i;
      for (i = 0; i < glyphs_len; i++)
        {
	  memcpy (&glyphs[i], &space_glyph, sizeof (struct glyph));
	  glyphs[i].frame = NULL;
        }
      ceol_initialized = TRUE;
    }
  w32con_write_glyphs (f, glyphs, end - cursor_coords.X);
}

/* Insert n lines at vpos. if n is negative delete -n lines.  */
static void
w32con_ins_del_lines (struct frame *f, int vpos, int n)
{
  int	     i, nb;
  SMALL_RECT scroll;
  SMALL_RECT clip;
  COORD	     dest;
  CHAR_INFO  fill;

  if (n < 0)
    {
      scroll.Top = vpos - n;
      scroll.Bottom = FRAME_TOTAL_LINES (f);
      dest.Y = vpos;
    }
  else
    {
      scroll.Top = vpos;
      scroll.Bottom = FRAME_TOTAL_LINES (f) - n;
      dest.Y = vpos + n;
    }
  clip.Top = clip.Left = scroll.Left = 0;
  clip.Right = scroll.Right = FRAME_COLS (f);
  clip.Bottom = FRAME_TOTAL_LINES (f);

  dest.X = 0;

  fill.Char.AsciiChar = 0x20;
  fill.Attributes = char_attr_normal;

  ScrollConsoleScreenBuffer (cur_screen, &scroll, &clip, dest, &fill);

  /* Here we have to deal with a w32 console flake: If the scroll
     region looks like abc and we scroll c to a and fill with d we get
     cbd... if we scroll block c one line at a time to a, we get cdd...
     Emacs expects cdd consistently... So we have to deal with that
     here... (this also occurs scrolling the same way in the other
     direction.  */

  if (n > 0)
    {
      if (scroll.Bottom < dest.Y)
        {
	  for (i = scroll.Bottom; i < dest.Y; i++)
            {
	      w32con_move_cursor (f, i, 0);
	      w32con_clear_end_of_line (f, FRAME_COLS (f));
            }
        }
    }
  else
    {
      nb = dest.Y + (scroll.Bottom - scroll.Top) + 1;

      if (nb < scroll.Top)
        {
	  for (i = nb; i < scroll.Top; i++)
            {
	      w32con_move_cursor (f, i, 0);
	      w32con_clear_end_of_line (f, FRAME_COLS (f));
            }
        }
    }

  cursor_coords.X = 0;
  cursor_coords.Y = vpos;
}

#undef	LEFT
#undef	RIGHT
#define	LEFT	1
#define	RIGHT	0

static void
scroll_line (struct frame *f, int dist, int direction)
{
  /* The idea here is to implement a horizontal scroll in one line to
     implement delete and half of insert.  */
  SMALL_RECT scroll, clip;
  COORD	     dest;
  CHAR_INFO  fill;

  clip.Top = scroll.Top = clip.Bottom = scroll.Bottom = cursor_coords.Y;
  clip.Left = 0;
  clip.Right = FRAME_COLS (f);

  if (direction == LEFT)
    {
      scroll.Left = cursor_coords.X + dist;
      scroll.Right = FRAME_COLS (f) - 1;
    }
  else
    {
      scroll.Left = cursor_coords.X;
      scroll.Right = FRAME_COLS (f) - dist - 1;
    }

  dest.X = cursor_coords.X;
  dest.Y = cursor_coords.Y;

  fill.Char.AsciiChar = 0x20;
  fill.Attributes = char_attr_normal;

  ScrollConsoleScreenBuffer (cur_screen, &scroll, &clip, dest, &fill);
}


/* If start is zero insert blanks instead of a string at start ?. */
static void
w32con_insert_glyphs (struct frame *f, register struct glyph *start,
		      register int len)
{
  scroll_line (f, len, RIGHT);

  /* Move len chars to the right starting at cursor_coords, fill with blanks */
  if (start)
    {
      /* Print the first len characters of start, cursor_coords.X adjusted
	 by write_glyphs.  */

      w32con_write_glyphs (f, start, len);
    }
  else
    {
      w32con_clear_end_of_line (f, cursor_coords.X + len);
    }
}

static void
w32con_write_glyphs (struct frame *f, register struct glyph *string,
                     register int len)
{
  DWORD r;
  LPCSTR conversion_buffer;
  struct coding_system *coding;

  w32con_hide_cursor();

  if (len <= 0)
    return;

  /* If terminal_coding does any conversion, use it, otherwise use
     safe_terminal_coding.  We can't use CODING_REQUIRE_ENCODING here
     because it always return 1 if the member src_multibyte is 1.  */
  coding = (FRAME_TERMINAL_CODING (f)->common_flags & CODING_REQUIRE_ENCODING_MASK
	    ? FRAME_TERMINAL_CODING (f) : &safe_terminal_coding);
  /* The mode bit CODING_MODE_LAST_BLOCK should be set to 1 only at
     the tail.  */
  coding->mode &= ~CODING_MODE_LAST_BLOCK;

  while (len > 0)
    {
      /* Identify a run of glyphs with the same face.  */
      int face_id = string->face_id;
      /* Since this is called to deliver the frame glyph matrix to the
	 glass, some of the glyphs might be from a child frame, which
	 affects the interpretation of face ID.  */
      struct frame *face_id_frame = string->frame;
      int n;

      for (n = 1; n < len; ++n)
	if (!(string[n].face_id == face_id
	      && string[n].frame == face_id_frame))
	  break;

      /* w32con_clear_end_of_line sets frame of glyphs to NULL.  */
      struct frame *attr_frame = face_id_frame ? face_id_frame : f;

      if (n == len)
	/* This is the last run.  */
	coding->mode |= CODING_MODE_LAST_BLOCK;
      conversion_buffer = (LPCSTR) encode_terminal_code (string, n, coding);
      if (coding->produced > 0)
	{
	  if (w32_use_virtual_terminal)
	    {
	      turn_on_face (f, face_id);
	      WriteConsole (cur_screen, conversion_buffer,
			    coding->produced, &r, NULL);
	      turn_off_face (f, face_id);
	      cursor_coords = w32con_get_cursor_coords ();
	    }
	  else
	    {
	      /* Account for character width.
		 FIXME: this doesn't handle character compositions.  */
	      ptrdiff_t ncols = strwidth (coding->source, coding->src_bytes);

	      /* Turn appearance modes of the face of the run on.  */
	      WORD char_attr = w32_face_attributes (attr_frame, face_id);

	      /* Set the attribute for these characters.  */
	      if (!FillConsoleOutputAttribute (cur_screen, char_attr, ncols,
					       cursor_coords, &r))
		{
		  printf ("Failed writing console attributes: %lu\n",
			  GetLastError ());
		  fflush (stdout);
		}

	      /* Write the characters.  */
	      if (!WriteConsoleOutputCharacter (cur_screen, conversion_buffer,
						coding->produced, cursor_coords,
						&r))
		{
		  printf ("Failed writing console characters: %lu\n",
			  GetLastError ());
		  fflush (stdout);
		}

	      cursor_coords.X += ncols;
	      w32con_move_cursor (f, cursor_coords.Y, cursor_coords.X);
	    }
	}
      len -= n;
      string += n;
    }
}

/* Used for mouse highlight.  */
static void
w32con_write_glyphs_with_face (struct frame *f, register int x, register int y,
			       register struct glyph *string, register int len,
			       register int face_id)
{
  LPCSTR conversion_buffer;
  struct coding_system *coding;

  w32con_hide_cursor();

  if (len <= 0)
    return;

  /* If terminal_coding does any conversion, use it, otherwise use
     safe_terminal_coding.  We can't use CODING_REQUIRE_ENCODING here
     because it always return 1 if the member src_multibyte is 1.  */
  coding = (FRAME_TERMINAL_CODING (f)->common_flags & CODING_REQUIRE_ENCODING_MASK
	    ? FRAME_TERMINAL_CODING (f) : &safe_terminal_coding);
  /* We are going to write the entire block of glyphs in one go, as
     they all have the same face.  So this _is_ the last block.  */
  coding->mode |= CODING_MODE_LAST_BLOCK;

  conversion_buffer = (LPCSTR) encode_terminal_code (string, len, coding);
  if (coding->produced > 0)
    {
      DWORD filled, written;
      if (w32_use_virtual_terminal)
	{
	  COORD saved_coords = cursor_coords;
	  w32con_move_cursor(f, y, x);
	  turn_on_face (f, face_id);
	  WriteConsole (cur_screen, conversion_buffer,
			coding->produced, &written, NULL);
	  turn_off_face (f, face_id);
	  w32con_move_cursor(f, saved_coords.Y, saved_coords.X);
	}
      else
	{
	  /* Compute the character attributes corresponding to the face.  */
	  DWORD char_attr = w32_face_attributes (f, face_id);
	  COORD start_coords;

	  start_coords.X = x;
	  start_coords.Y = y;

	  /* Account for character width.
	     FIXME: this doesn't handle character compositions.  */
	  ptrdiff_t ncols = strwidth (coding->source, coding->src_bytes);

	  /* Set the attribute for these characters.  */
	  if (!FillConsoleOutputAttribute (cur_screen, char_attr, ncols,
					   start_coords, &filled))
	    DebPrint (("Failed writing console attributes: %d\n", GetLastError ()));
	  else
	    {
	      /* Write the characters.  */
	      if (!WriteConsoleOutputCharacter (cur_screen, conversion_buffer,
						coding->produced, start_coords,
						&written))
		DebPrint (("Failed writing console characters: %d\n",
			   GetLastError ()));
	    }
	}
    }
}

/* Implementation of draw_row_with_mouse_face for W32 console.  */
void
tty_draw_row_with_mouse_face (struct window *w, struct glyph_row *window_row,
			      int window_start_x, int window_end_x,
			      enum draw_glyphs_face draw)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct frame *root = root_frame (f);

  /* Window coordinates are relative to the text area.  Make
     them relative to the window's left edge,  */
  window_end_x = min (window_end_x, window_row->used[TEXT_AREA]);
  window_start_x += window_row->used[LEFT_MARGIN_AREA];
  window_end_x += window_row->used[LEFT_MARGIN_AREA];

  /* Translate from window to window's frame.  */
  int frame_start_x = WINDOW_LEFT_EDGE_X (w) + window_start_x;
  int frame_end_x = WINDOW_LEFT_EDGE_X (w) + window_end_x;
  int frame_y = window_row->y + WINDOW_TOP_EDGE_Y (w);

  /* Translate from (possible) child frame to root frame.  */
  int root_start_x, root_end_x, root_y;
  root_xy (f, frame_start_x, frame_y, &root_start_x, &root_y);
  root_xy (f, frame_end_x, frame_y, &root_end_x, &root_y);
  struct glyph_row *root_row = MATRIX_ROW (root->current_matrix, root_y);

  /* Remember current cursor coordinates so that we can restore
     them at the end.  */
  COORD save_coords = cursor_coords;

  /* If the root frame displays child frames, we cannot naively
     write to the terminal what the window thinks should be drawn.
     Instead, write only those parts that are not obscured by
     other frames.  */
  for (int root_x = root_start_x; root_x < root_end_x; )
    {
      /* Find the start of a run of glyphs from frame F.  */
      struct glyph *root_start = root_row->glyphs[TEXT_AREA] + root_x;
      while (root_x < root_end_x && root_start->frame != f)
	++root_x, ++root_start;

      /* If start of a run of glyphs from F found.  */
      int root_run_start_x = root_x;
      if (root_run_start_x < root_end_x)
	{
	  /* Find the end of the run of glyphs from frame F.  */
	  struct glyph *root_end = root_start;
	  while (root_x < root_end_x && root_end->frame == f)
	    ++root_x, ++root_end;

	  /* If we have a run glyphs to output, do it.  */
	  if (root_end > root_start)
	    {
	      w32con_move_cursor (root, root_y, root_run_start_x);

	      ptrdiff_t nglyphs = root_end - root_start;
	      switch (draw)
		{
		case DRAW_NORMAL_TEXT:
		  write_glyphs (f, root_start, nglyphs);
		  break;

		case DRAW_MOUSE_FACE:
		  {
		    struct tty_display_info *tty = FRAME_TTY (f);
		    int face_id = tty->mouse_highlight.mouse_face_face_id;
		    w32con_write_glyphs_with_face (f, root_run_start_x, root_y,
						   root_start, nglyphs,
						   face_id);
		  }
		  break;

		case DRAW_INVERSE_VIDEO:
		case DRAW_CURSOR:
		case DRAW_IMAGE_RAISED:
		case DRAW_IMAGE_SUNKEN:
		  emacs_abort ();
		}
	    }
	}
    }

  /* Restore cursor where it was before.  */
  w32con_move_cursor (f, save_coords.Y, save_coords.X);
}

static void
w32con_delete_glyphs (struct frame *f, int n)
{
  /* delete chars means scroll chars from cursor_coords.X + n to
     cursor_coords.X, anything beyond the edge of the screen should
     come out empty...  */

  scroll_line (f, n, LEFT);
}

static void
w32con_setup_virtual_terminal (void)
{
  /* Disable unless 24bit color is supported (v. > 10.0.15063).  */
  w32_use_virtual_terminal = w32_use_virtual_terminal
    && (w32_major_version > 10
	|| (w32_major_version == 10
	    && (w32_minor_version > 0 || w32_build_number > 15063)));

  DWORD out_mode;
  GetConsoleMode (cur_screen, &out_mode);
  out_mode |= ENABLE_PROCESSED_OUTPUT;
  out_mode |= DISABLE_NEWLINE_AUTO_RETURN;

  if (w32_use_virtual_terminal)
    out_mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  else
    out_mode &= ~ENABLE_VIRTUAL_TERMINAL_PROCESSING;

  int out_mode_set = SetConsoleMode (cur_screen, out_mode);
  w32_use_virtual_terminal = w32_use_virtual_terminal && out_mode_set;

  int max_colors = w32_use_virtual_terminal ? 16777216 : 16;
  tty_setup_colors (current_tty, max_colors);

  safe_calln (Qtty_set_up_initial_frame_faces);
}

static void
w32con_reset_terminal_modes (struct terminal *t)
{
  COORD dest;
  CONSOLE_SCREEN_BUFFER_INFO info;
  int n;
  DWORD r;

  /* Clear the complete screen buffer.  This is required because Emacs
     sets the cursor position to the top of the buffer, but there might
     be other output below the bottom of the Emacs frame if the screen buffer
     is larger than the window size.  */
  GetConsoleScreenBufferInfo (cur_screen, &info);
  dest.X = 0;
  dest.Y = 0;
  n = info.dwSize.X * info.dwSize.Y;

  FillConsoleOutputAttribute (cur_screen, char_attr_normal, n, dest, &r);
  FillConsoleOutputCharacter (cur_screen, ' ', n, dest, &r);
  /* Now that the screen is clear, put the cursor at the top.  */
  SetConsoleCursorPosition (cur_screen, dest);

#ifdef USE_SEPARATE_SCREEN
  SetConsoleActiveScreenBuffer (prev_screen);
#else
  SetConsoleCursorInfo (prev_screen, &prev_console_cursor);
#endif

  SetConsoleMode (keyboard_handle, prev_console_mode);
}

static void
w32con_set_terminal_modes (struct terminal *t)
{
  CONSOLE_CURSOR_INFO cci;

  /* make cursor big and visible (100 on Windows 95 makes it disappear)  */
  cci.dwSize = 99;
  cci.bVisible = TRUE;
  (void) SetConsoleCursorInfo (cur_screen, &cci);

  SetConsoleActiveScreenBuffer (cur_screen);

  /* If Quick Edit is enabled for the console, it will get in the way
     of receiving mouse events, so we disable it.  But leave the
     Insert Mode as it was set by the user.  */
  DWORD new_console_mode
    = ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS;
  if ((prev_console_mode & ENABLE_INSERT_MODE) != 0)
    new_console_mode |= ENABLE_INSERT_MODE;
  SetConsoleMode (keyboard_handle, new_console_mode);

  /* Initialize input mode: interrupt_input off, no flow control, allow
     8 bit character input, standard quit char.  */
  Fset_input_mode (Qnil, Qnil, make_fixnum (2), Qnil);
  w32con_setup_virtual_terminal ();
}

/* hmmm... perhaps these let us bracket screen changes so that we can flush
   clumps rather than one-character-at-a-time...

   we'll start with not moving the cursor while an update is in progress.  */
static void
w32con_update_begin (struct frame * f)
{
}

static void
w32con_update_end (struct frame * f)
{
  SetConsoleCursorPosition (cur_screen, cursor_coords);
  if (!XWINDOW (selected_window)->cursor_off_p
      && cursor_coords.X < FRAME_COLS (f))
    w32con_show_cursor ();
  else
    w32con_hide_cursor ();
}

/***********************************************************************
			stubs from termcap.c
 ***********************************************************************/

void sys_tputs (char *, int, int (*) (int));

void
sys_tputs (char *str, int nlines, int (*outfun) (int))
{
}

char *sys_tgetstr (char *, char **);

char *
sys_tgetstr (char *cap, char **area)
{
  return NULL;
}


/***********************************************************************
			stubs from cm.c
 ***********************************************************************/

extern int cost;
int cost = 0;

int evalcost (int);

int
evalcost (int c)
{
  return c;
}

int cmputc (int);

int
cmputc (int c)
{
  return c;
}

void cmcheckmagic (struct tty_display_info *);

void
cmcheckmagic (struct tty_display_info *tty)
{
}

void cmcostinit (struct tty_display_info *);

void
cmcostinit (struct tty_display_info *tty)
{
}

void cmgoto (struct tty_display_info *, int, int);

void
cmgoto (struct tty_display_info *tty, int row, int col)
{
}

void Wcm_clear (struct tty_display_info *);

void
Wcm_clear (struct tty_display_info *tty)
{
}


/* Report the current cursor position.  The following two functions
   are used in term.c's tty menu code, so they are not really
   "stubs".  */
int
cursorX (struct tty_display_info *tty)
{
  return cursor_coords.X;
}

int
cursorY (struct tty_display_info *tty)
{
  return cursor_coords.Y;
}

/***********************************************************************
				Faces
 ***********************************************************************/


/* Turn appearances of face FACE_ID on tty frame F on.  */

static WORD
w32_face_attributes (struct frame *f, int face_id)
{
  WORD char_attr;
  struct face *face = FACE_FROM_ID (f, face_id);

  char_attr = char_attr_normal;

  /* Reverse the default color if requested. If background and
     foreground are specified, then they have been reversed already.  */
  if (face->tty_reverse_p)
    char_attr = (char_attr & 0xff00) + ((char_attr & 0x000f) << 4)
      + ((char_attr & 0x00f0) >> 4);

  /* Before the terminal is properly initialized, all colors map to 0.
     Don't try to resolve them.  */
  if (NILP (Vtty_defined_color_alist))
    return char_attr;

  /* Colors should be in the range 0...15 unless they are one of
     FACE_TTY_DEFAULT_COLOR, FACE_TTY_DEFAULT_FG_COLOR or
     FACE_TTY_DEFAULT_BG_COLOR.  Other out of range colors are
     invalid, so it is better to use the default color if they ever
     get through to here.  */
  if (face->foreground >= 0 && face->foreground < 16)
    char_attr = (char_attr & 0xfff0) + face->foreground;

  if (face->background >= 0 && face->background < 16)
    char_attr = (char_attr & 0xff0f) + (face->background << 4);

  return char_attr;
}

static void
turn_on_face (struct frame *f, int face_id)
{
  struct face *face = FACE_FROM_ID (f, face_id);
  struct tty_display_info *tty = FRAME_TTY (f);
  unsigned long fg = face->foreground;
  unsigned long bg = face->background;

  /* construct combined VT sequence for face attributes */
  DWORD n = 0;
  size_t sz = SEQMAX;
  char seq[sz];
  sz--;

  if (face->tty_bold_p)
    SSPRINTF (seq, &n, sz, tty->TS_enter_bold_mode, NULL);
  if (face->tty_italic_p)
    SSPRINTF (seq, &n, sz, tty->TS_enter_italic_mode, NULL);
  if (face->tty_strike_through_p)
    SSPRINTF (seq, &n, sz, tty->TS_enter_strike_through_mode, NULL);
  if (face->underline != 0)
    SSPRINTF (seq, &n, sz, tty->TS_enter_underline_mode, NULL);
  /* Note: when face->tty_reverse_p != 0 and fg and bg are specified,
     their values are already swapped and reversing them here would swap
     them back, but we need to handle the reversal when unspecified.  */
  if (face->tty_reverse_p && DEFAULTP (fg) && DEFAULTP (bg))
    SSPRINTF (seq, &n, sz, tty->TS_enter_reverse_mode, NULL);

  if (DEFAULTP (fg)) fg = fg_normal;
  if (DEFAULTP (bg)) bg = bg_normal;

  const char *set_fg = tty->TS_set_foreground;
  const char *set_bg = tty->TS_set_background;
  if (tty->TN_max_colors == 8  || tty->TN_max_colors == 16)
    {
      /* fg and bg are indices into 16 base colors (see link at top).  */
      unsigned long fgi = 0, bgi = 0;

      fgi = (fg >= 0  && fg < 8)
	    ? fg + 30
	    : (fg >= 8  && fg < 16)
	      ? fg - 8 + 90
	      : 0;
      if (fgi)
	SSPRINTF (seq, &n, sz, set_fg, fgi);

      bgi = (bg >= 0  && bg < 8)
	    ? bg + 40
	    : (bg >= 8  && bg < 16)
	      ? bg - 8 + 100
	      : 0;
      if (bgi)
	SSPRINTF (seq, &n, sz, set_bg, bgi);
    }
  else if (tty->TN_max_colors == 256)
    {
      /* fg and bg are xterm indices.  */
      if (fg >= 0 && fg < 256)
	SSPRINTF (seq, &n, sz, set_fg, fg);

      if (bg >= 0 && bg < 256)
	SSPRINTF (seq, &n, sz, set_bg, bg);
    }
  else if (tty->TN_max_colors == 16777216)
    {
      /* fg and bg are pixel values -- decompose to rgb triples.  */
      unsigned long rf = fg/65536, gf = (fg/256)&255, bf = fg&255;
      unsigned long rb = bg/65536, gb = (bg/256)&255, bb = bg&255;
      SSPRINTF (seq, &n, sz, set_fg, rf, gf, bf);
      SSPRINTF (seq, &n, sz, set_bg, rb, gb, bb);
    }
  w32con_write_vt_seq ((const char *) seq);
}

static void
turn_off_face (struct frame *f, int face_id)
{
  struct tty_display_info *tty = FRAME_TTY (f);
  w32con_write_vt_seq (tty->TS_exit_attribute_mode);
}

/* The IME window is needed to receive the session notifications
   required to reset the low level keyboard hook state.  */

static BOOL CALLBACK
find_ime_window (HWND hwnd, LPARAM arg)
{
  char window_class[32];

  GetClassName (hwnd, window_class, sizeof (window_class));
  if (strcmp (window_class, "IME") == 0)
    {
      *(HWND *) arg = hwnd;
      return FALSE;
    }
  /* keep looking */
  return TRUE;
}

void
initialize_w32_display (struct terminal *term, int *width, int *height)
{
  CONSOLE_SCREEN_BUFFER_INFO	info;

  term->rif = 0; /* No window based redisplay on the console.  */
  term->cursor_to_hook		= w32con_move_cursor;
  term->raw_cursor_to_hook	= w32con_move_cursor;
  term->clear_to_end_hook	= w32con_clear_to_end;
  term->clear_frame_hook	= w32con_clear_frame;
  term->clear_end_of_line_hook	= w32con_clear_end_of_line;
  term->ins_del_lines_hook	= w32con_ins_del_lines;
  term->insert_glyphs_hook	= w32con_insert_glyphs;
  term->write_glyphs_hook	= w32con_write_glyphs;
  term->delete_glyphs_hook	= w32con_delete_glyphs;
  term->ring_bell_hook		= w32_sys_ring_bell;
  term->reset_terminal_modes_hook = w32con_reset_terminal_modes;
  term->set_terminal_modes_hook	= w32con_set_terminal_modes;
  term->set_terminal_window_hook = NULL;
  term->update_begin_hook	= w32con_update_begin;
  term->update_end_hook		= w32con_update_end;

  term->defined_color_hook = &tty_defined_color; /* xfaces.c */
  term->read_socket_hook = w32_console_read_socket;
  term->mouse_position_hook = w32_console_mouse_position;
  term->menu_show_hook = tty_menu_show;

  /* The following are not used on the console.  */
  term->frame_rehighlight_hook = 0;
  term->frame_raise_lower_hook = 0;
  term->set_vertical_scroll_bar_hook = 0;
  term->set_horizontal_scroll_bar_hook = 0;
  term->condemn_scroll_bars_hook = 0;
  term->redeem_scroll_bar_hook = 0;
  term->judge_scroll_bars_hook = 0;
  term->frame_up_to_date_hook = 0;

  /* Initialize the mouse-highlight data.  */
  reset_mouse_highlight (&term->display_info.tty->mouse_highlight);

  /* Initialize interrupt_handle.  */
  init_crit ();

  /* Remember original console settings.  */
  keyboard_handle = GetStdHandle (STD_INPUT_HANDLE);
  GetConsoleMode (keyboard_handle, &prev_console_mode);
  /* Make sure ENABLE_EXTENDED_FLAGS is set in console settings,
     otherwise restoring the original setting of ENABLE_MOUSE_INPUT
     will not work.  */
  prev_console_mode |= ENABLE_EXTENDED_FLAGS;

  prev_screen = GetStdHandle (STD_OUTPUT_HANDLE);

#ifdef USE_SEPARATE_SCREEN
  cur_screen = CreateConsoleScreenBuffer (GENERIC_READ | GENERIC_WRITE,
					  0, NULL,
					  CONSOLE_TEXTMODE_BUFFER,
					  NULL);

  if (cur_screen == INVALID_HANDLE_VALUE)
    {
      printf ("CreateConsoleScreenBuffer failed in initialize_w32_display\n");
      printf ("LastError = 0x%lx\n", GetLastError ());
      fflush (stdout);
      exit (1);
    }
#else
  cur_screen = prev_screen;
  GetConsoleCursorInfo (prev_screen, &prev_console_cursor);
#endif

  /* Respect setting of LINES and COLUMNS environment variables.  */
  {
    char * lines = getenv ("LINES");
    char * columns = getenv ("COLUMNS");

    if (lines != NULL && columns != NULL)
      {
	SMALL_RECT new_win_dims;
	COORD new_size;

	new_size.X = atoi (columns);
	new_size.Y = atoi (lines);

	GetConsoleScreenBufferInfo (cur_screen, &info);

	/* Shrink the window first, so the buffer dimensions can be
           reduced if necessary.  */
	new_win_dims.Top = 0;
	new_win_dims.Left = 0;
	new_win_dims.Bottom = min (new_size.Y, info.dwSize.Y) - 1;
	new_win_dims.Right = min (new_size.X, info.dwSize.X) - 1;
	SetConsoleWindowInfo (cur_screen, TRUE, &new_win_dims);

	SetConsoleScreenBufferSize (cur_screen, new_size);

	/* Set the window size to match the buffer dimension.  */
	new_win_dims.Top = 0;
	new_win_dims.Left = 0;
	new_win_dims.Bottom = new_size.Y - 1;
	new_win_dims.Right = new_size.X - 1;
	SetConsoleWindowInfo (cur_screen, TRUE, &new_win_dims);
      }
  }

  if (!GetConsoleScreenBufferInfo (cur_screen, &info))
    {
      printf ("GetConsoleScreenBufferInfo failed in initialize_w32_display\n");
      printf ("LastError = 0x%lx\n", GetLastError ());
      fflush (stdout);
      exit (1);
    }

  char_attr_normal = info.wAttributes;
  fg_normal = char_attr_normal & 0x000f;
  bg_normal = (char_attr_normal >> 4) & 0x000f;

  /* Determine if the info returned by GetConsoleScreenBufferInfo
     is realistic.  Old MS Telnet servers used to only fill out
     the dwSize portion, even modern one fill the whole struct with
     garbage when using non-MS telnet clients.  */
  if ((w32_use_full_screen_buffer
       && (info.dwSize.Y < 20 || info.dwSize.Y > 100
	   || info.dwSize.X < 40 || info.dwSize.X > 200))
      || (!w32_use_full_screen_buffer
	  && (info.srWindow.Bottom - info.srWindow.Top < 20
	      || info.srWindow.Bottom - info.srWindow.Top > 100
	      || info.srWindow.Right - info.srWindow.Left < 40
	      || info.srWindow.Right - info.srWindow.Left > 100)))
    {
      *height = 25;
      *width = 80;
    }

  else if (w32_use_full_screen_buffer)
    {
      *height = info.dwSize.Y;	/* lines per page */
      *width = info.dwSize.X;	/* characters per line */
    }
  else
    {
      /* Lines per page.  Use buffer coords instead of buffer size.  */
      *height = 1 + info.srWindow.Bottom - info.srWindow.Top;
      /* Characters per line.  Use buffer coords instead of buffer size.  */
      *width = 1 + info.srWindow.Right - info.srWindow.Left;
    }

  /* Force reinitialization of the "empty row" buffer, in case they
     dumped from a running session.  */
  if (glyphs != glyph_base)
    {
      glyphs = NULL;
      glyphs_len = 0;
      ceol_initialized = FALSE;
    }

  if (os_subtype == OS_SUBTYPE_NT)
    w32_console_unicode_input = 1;
  else
    w32_console_unicode_input = 0;

  /* Setup w32_display_info structure for this frame.  */
  w32_initialize_display_info (build_string ("Console"));

  HWND hwnd = NULL;
  EnumThreadWindows (GetCurrentThreadId (), find_ime_window, (LPARAM) &hwnd);

  /* Set up the keyboard hook.  */
  setup_w32_kbdhook (hwnd);

  /* Set current_tty to the tty of this terminal */
  current_tty = term->display_info.tty;
}


DEFUN ("set-screen-color", Fset_screen_color, Sset_screen_color, 2, 3, 0,
       doc: /* Set screen foreground and background colors.

Arguments should be indices for colors in the list returned by `tty-color-alist'.
If VTP is non-nil, settings affect virtual terminal processing only.
Otherwise, arguments should be between 0 and 15, and settings will
be effective only when virtual terminal processing is disabled.

See w32console.el and the documentation for `w32-use-virtual-terminal'.  */)
  (Lisp_Object foreground, Lisp_Object background, Lisp_Object vtp)
{
  int fg = XFIXNAT (foreground);
  int bg = XFIXNAT (background);

  if (NILP (vtp))
    {
      char_attr_normal = fg + (bg << 4);
    }
  else
    {
      fg_normal = fg;
      bg_normal = bg;
    }

  Frecenter (Qnil, Qt);
  return Qt;
}

DEFUN ("get-screen-color", Fget_screen_color, Sget_screen_color, 0, 1, 0,
       doc: /* Get color indices of the current screen foreground and background.

The colors are returned as a list of 2 indices (FOREGROUND BACKGROUND) for
colors in the list returned by `tty-color-alist`.

If VTP is non-nil, returns settings effective when virtual terminal
processing is enabled.  Otherwise, returns settings effective when
virtual terminal processing is disabled.

See w32console.el and the documentation for `w32-use-virtual-terminal'.  */)
  (Lisp_Object vtp)
{
  int fg = NILP (vtp) ? char_attr_normal & 0x000f : fg_normal;
  int bg = NILP (vtp) ? (char_attr_normal >> 4) & 0x000f : bg_normal;

  return Fcons (make_fixnum (fg), Fcons (make_fixnum (bg), Qnil));
}

DEFUN ("w32-use-virtual-terminal", Fw32_use_virtual_terminal, Sw32_use_virtual_terminal, 1, 1, 0,
       doc: /* Enables (disables) virtual terminal sequence processing if argument is t (nil).  */)
  (Lisp_Object arg)
{
  if (EQ (arg, Qt))
    w32_use_virtual_terminal = 1;
  else if (EQ (arg, Qnil))
    w32_use_virtual_terminal = 0;
  else {
    error ("Invalid argument: expects t or nil.");
    return Qnil;
  }

  w32con_setup_virtual_terminal ();
  return Qt;
}

DEFUN ("w32-use-virtual-terminal-p", Fw32_use_virtual_terminal_p, Sw32_use_virtual_terminal_p, 0, 0, 0,
       doc: /* Returns t (nil) if virtual terminal sequence processing is enabled (disabled).  */)
  (void)
{
  return w32_use_virtual_terminal ? Qt : Qnil;
}

DEFUN ("set-cursor-size", Fset_cursor_size, Sset_cursor_size, 1, 1, 0,
       doc: /* Set cursor size.  */)
  (Lisp_Object size)
{
  CONSOLE_CURSOR_INFO cci;
  cci.dwSize = XFIXNAT (size);
  cci.bVisible = TRUE;
  (void) SetConsoleCursorInfo (cur_screen, &cci);

  return Qt;
}

void
syms_of_ntterm (void)
{
  DEFVAR_BOOL ("w32-use-full-screen-buffer",
               w32_use_full_screen_buffer,
	       doc: /* Non-nil means make terminal frames use the full screen buffer dimensions.
This is desirable when running Emacs over telnet.
A value of nil means use the current console window dimensions; this
may be preferable when working directly at the console with a large
scroll-back buffer.  */);
  w32_use_full_screen_buffer = 0;

  defsubr (&Sset_screen_color);
  defsubr (&Sget_screen_color);
  defsubr (&Sw32_use_virtual_terminal);
  defsubr (&Sw32_use_virtual_terminal_p);
  defsubr (&Sset_cursor_size);
}
