/* Terminal hooks for Windows NT port of GNU Emacs.
   Copyright (C) 1992 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any later
   version.

   GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with GNU Emacs; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Tim Fleehart (apollo@online.com)		1-17-92
   Geoff Voelker (voelker@cs.washington.edu)	9-12-93
*/


#include <stdlib.h>
#include <stdio.h>

#include "config.h"

#include <windows.h>

#include "lisp.h"
#include "frame.h"
#include "disptab.h"
#include "termhooks.h"

#include "ntinevt.h"

/* from window.c */
extern Lisp_Object Frecenter ();

/* from keyboard.c */
extern int detect_input_pending ();

/* from sysdep.c */
extern int read_input_pending ();

extern FRAME_PTR updating_frame;
extern int meta_key;

static void move_cursor (int row, int col);
static void clear_to_end (void);
static void clear_frame (void);
static void clear_end_of_line (int);
static void ins_del_lines (int vpos, int n);
static void change_line_highlight (int, int, int);
static void reassert_line_highlight (int, int);
static void insert_glyphs (GLYPH *start, int len);
static void write_glyphs (GLYPH *string, int len);
static void delete_glyphs (int n);
static void ring_bell (void);
static void reset_terminal_modes (void);
static void set_terminal_modes (void);
static void set_terminal_window (int size);
static void update_begin (FRAME_PTR f);
static void update_end (FRAME_PTR f);
static void reset_kbd (void);
static void unset_kbd (void);
static int  hl_mode (int new_highlight);

void
DebPrint ()
{
}

/* Init hook called in init_keyboard.  */
void (*keyboard_init_hook)(void) = reset_kbd;
    
COORD	cursor_coords;
HANDLE	prev_screen, cur_screen;
UCHAR	char_attr, char_attr_normal, char_attr_reverse;
HANDLE  keyboard_handle;


/* Setting this as the ctrl handler prevents emacs from being killed when
 * someone hits ^C in a 'suspended' session (child shell).  */
BOOL
ctrl_c_handler (unsigned long type)
{
  return (type == CTRL_C_EVENT) ? TRUE : FALSE;
}

/* If we're updating a frame, use it as the current frame
   Otherwise, use the selected frame.  */
#define PICK_FRAME() (updating_frame ? updating_frame : selected_frame)

/* Move the cursor to (row, col).  */
void
move_cursor (int row, int col)
{
  cursor_coords.X = col;
  cursor_coords.Y = row;
  
  if (updating_frame == (FRAME_PTR) NULL)
    {
      SetConsoleCursorPosition (cur_screen, cursor_coords);
    }
}

/* Clear from cursor to end of screen.  */
void
clear_to_end (void)
{
  FRAME_PTR f = PICK_FRAME ();
  
  clear_end_of_line (FRAME_WIDTH (f) - 1);
  ins_del_lines (cursor_coords.Y, FRAME_HEIGHT (f) - cursor_coords.Y - 1);
}

/* Clear the frame.  */
void
clear_frame (void)
{
  SMALL_RECT scroll;
  COORD	     dest;
  CHAR_INFO  fill;
  FRAME_PTR  f = PICK_FRAME ();
  
  hl_mode (0);
  
  scroll.Top = 0;
  scroll.Bottom = FRAME_HEIGHT (f) - 1;
  scroll.Left = 0;
  scroll.Right = FRAME_WIDTH (f) - 1;
  
  dest.Y = FRAME_HEIGHT (f);
  dest.X = 0;
  
  fill.Char.AsciiChar = 0x20;
  fill.Attributes = char_attr;
  
  ScrollConsoleScreenBuffer (cur_screen, &scroll, NULL, dest, &fill);
  move_cursor (0, 0);
}


static GLYPH glyph_base[256];
static BOOL  ceol_initialized = FALSE;

/* Clear from Cursor to end (what's "standout marker"?).  */
void
clear_end_of_line (int end)
{
  if (!ceol_initialized)
    {
      int i;
      for (i = 0; i < 256; i++)
        {
	  glyph_base[i] = SPACEGLYPH;	/* empty space	*/
        }
      ceol_initialized = TRUE;
    }
  write_glyphs (glyph_base, end - cursor_coords.X);	/* fencepost ?	*/
}

/* Insert n lines at vpos. if n is negative delete -n lines.  */
void
ins_del_lines (int vpos, int n)
{
  int	     i, nb, save_highlight;
  SMALL_RECT scroll;
  COORD	     dest;
  CHAR_INFO  fill;
  FRAME_PTR  f = PICK_FRAME ();

  if (n < 0)
    {
      scroll.Top = vpos - n;
      scroll.Bottom = FRAME_HEIGHT (f);
      dest.Y = vpos;
    }
  else
    {
      scroll.Top = vpos;
      scroll.Bottom = FRAME_HEIGHT (f) - n;
      dest.Y = vpos + n;
    }
  scroll.Left = 0;
  scroll.Right = FRAME_WIDTH (f);
  
  dest.X = 0;
  
  save_highlight = hl_mode (0);
  
  fill.Char.AsciiChar = 0x20;
  fill.Attributes = char_attr;
  
  ScrollConsoleScreenBuffer (cur_screen, &scroll, NULL, dest, &fill);

  /* Here we have to deal with a win32 console flake: If the scroll
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
	      move_cursor (i, 0);
	      clear_end_of_line (FRAME_WIDTH (f));
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
	      move_cursor (i, 0);
	      clear_end_of_line (FRAME_WIDTH (f));
            }
        }
    }
  
  cursor_coords.X = 0;
  cursor_coords.Y = vpos;
  
  hl_mode (save_highlight);
}

/* Changes attribute to use when drawing characters to control.  */
static int
hl_mode (int new_highlight)
{
  static int highlight = 0;
  int old_highlight;
  
  old_highlight = highlight;
  highlight = (new_highlight != 0);
  if (highlight)
    {
      char_attr = char_attr_reverse;
    }
  else
    {
      char_attr = char_attr_normal;
    }
  return old_highlight;
}

/* Call this when about to modify line at position VPOS and change whether it
   is highlighted.  */
void
change_line_highlight (int new_highlight, int vpos, int first_unused_hpos)
{
  hl_mode (new_highlight);
  move_cursor (vpos, 0);
  clear_end_of_line (first_unused_hpos);
}

/* External interface to control of standout mode. Call this when about to
 * modify line at position VPOS and not change whether it is highlighted.  */
void
reassert_line_highlight (int highlight, int vpos)
{
  hl_mode (highlight);
  vpos;				/* pedantic compiler silencer */
}

#undef	LEFT
#undef	RIGHT
#define	LEFT	1
#define	RIGHT	0

void
scroll_line (int dist, int direction)
{
  /* The idea here is to implement a horizontal scroll in one line to
     implement delete and half of insert.  */
  SMALL_RECT scroll;
  COORD	     dest;
  CHAR_INFO  fill;
  FRAME_PTR  f = PICK_FRAME ();
  
  scroll.Top = cursor_coords.Y;
  scroll.Bottom = cursor_coords.Y;
  
  if (direction == LEFT)
    {
      scroll.Left = cursor_coords.X + dist;
      scroll.Right = FRAME_WIDTH (f) - 1;
    }
  else
    {
      scroll.Left = cursor_coords.X;
      scroll.Right = FRAME_WIDTH (f) - dist - 1;
    }
  
  dest.X = cursor_coords.X;
  dest.Y = cursor_coords.Y;
  
  fill.Char.AsciiChar = 0x20;
  fill.Attributes = char_attr;
  
  ScrollConsoleScreenBuffer (cur_screen, &scroll, NULL, dest, &fill);
}


/* If start is zero insert blanks instead of a string at start ?. */
void
insert_glyphs (register GLYPH *start, register int len)
{
  scroll_line (len, RIGHT);

  /* Move len chars to the right starting at cursor_coords, fill with blanks */
  if (start)
    {
      /* Print the first len characters of start, cursor_coords.X adjusted
	 by write_glyphs.  */
	
      write_glyphs (start, len);
    }
  else
    {
      clear_end_of_line (cursor_coords.X + len);
    }
}

void
write_glyphs (register GLYPH *string, register int len)
{
  register unsigned int glyph_len = GLYPH_TABLE_LENGTH;
  Lisp_Object *glyph_table = GLYPH_TABLE_BASE;
  FRAME_PTR f = PICK_FRAME ();
  register char *ptr;
  GLYPH glyph;
  WORD *attrs;
  char *chars;
  int i;
  
  attrs = alloca (len * sizeof (*attrs));
  chars = alloca (len * sizeof (*chars));
  if (attrs == NULL || chars == NULL)
    {
      printf ("alloca failed in write_glyphs\n");
      return;
    }
  
  /* We have to deal with the glyph indirection...go over the glyph
     buffer and extract the characters.  */
  ptr = chars;
  while (--len >= 0)
    {
      glyph = *string++;

      if (glyph > glyph_len)
        {
	  *ptr++ = glyph & 0xFF;
	  continue;
	}
      GLYPH_FOLLOW_ALIASES (glyph_table, glyph_len, glyph);
      if (GLYPH_FACE (fixfix, glyph) != 0)
	printf ("Glyph face is %d\n", GLYPH_FACE (fixfix, glyph));
      if (GLYPH_SIMPLE_P (glyph_table, glyph_len, glyph))
        {
	  *ptr++ = glyph & 0xFF;
	  continue;
	}
      for (i = 0; i < GLYPH_LENGTH (glyph_table, glyph); i++)
        {
	  *ptr++ = (GLYPH_STRING (glyph_table, glyph))[i];
	}
    }
  
  /* Number of characters we have in the buffer.  */
  len = ptr-chars;
  
  /* Fill in the attributes for these characters.  */
  memset (attrs, char_attr, len*sizeof (*attrs));
  
  /* Write the attributes.  */
  if (!WriteConsoleOutputAttribute (cur_screen, attrs, len, cursor_coords, &i))
    {
      printf ("Failed writing console attributes: %d\n", GetLastError ());
      fflush (stdout);
    }

  /* Write the characters.  */
  if (!WriteConsoleOutputCharacter (cur_screen, chars, len, cursor_coords, &i))
    {
      printf ("Failed writing console characters: %d\n", GetLastError ());
      fflush (stdout);
    }
  
  cursor_coords.X += len;
  move_cursor (cursor_coords.Y, cursor_coords.X);
}

void
delete_glyphs (int n)
{
  /* delete chars means scroll chars from cursor_coords.X + n to 
     cursor_coords.X, anything beyond the edge of the screen should 
     come out empty...  */

  scroll_line (n, LEFT);
}

static unsigned int sound_type = 0xFFFFFFFF;

void
ring_bell (void)
{
  if (sound_type == 0xFFFFFFFF) 
      Beep (666, 100);
  else
      MessageBeep (sound_type);
}

DEFUN ("set-message-beep", Fset_message_beep, Sset_message_beep, 1, 1, 0,
       "Set the sound generated when the bell is rung.\n\
SOUND is 'asterisk, 'exclamation, 'hand, 'question, or 'ok\n\
to use the corresponding system sound for the bell.\n\
SOUND is nil to use the normal beep.")
     (sound)
     Lisp_Object sound;
{
  CHECK_SYMBOL (sound, 0);

  if (NILP (sound)) 
      sound_type = 0xFFFFFFFF;
  else if (EQ (sound, intern ("asterisk")))
      sound_type = MB_ICONASTERISK;
  else if (EQ (sound, intern ("exclamation"))) 
      sound_type = MB_ICONEXCLAMATION;
  else if (EQ (sound, intern ("hand"))) 
      sound_type = MB_ICONHAND;
  else if (EQ (sound, intern ("question"))) 
      sound_type = MB_ICONQUESTION;
  else if (EQ (sound, intern ("ok"))) 
      sound_type = MB_OK;
  else
      sound_type = 0xFFFFFFFF;

  return sound;
}

/* Put our console back up, for ending a suspended session.  */
void
take_console (void)
{
  reset_kbd ();
  SetConsoleActiveScreenBuffer (cur_screen);
}
   
void
reset_terminal_modes (void)
{
  unset_kbd ();
  SetConsoleActiveScreenBuffer (prev_screen);
}

void
set_terminal_modes (void)
{
  CONSOLE_CURSOR_INFO cci;

  if (cur_screen == NULL)
    {
      reset_kbd ();
      cur_screen = CreateConsoleScreenBuffer (GENERIC_READ | GENERIC_WRITE,
                                              0, NULL,
                                              CONSOLE_TEXTMODE_BUFFER,
                                              NULL);

      if (cur_screen == INVALID_HANDLE_VALUE)
        {
	  printf ("CreateConsoleScreenBuffer failed in ResetTerm\n");
	  printf ("LastError = 0x%lx\n", GetLastError ());
	  fflush (stdout);
	  exit (0);
	}

      SetConsoleActiveScreenBuffer (cur_screen);

      /* make cursor big and visible */
      cci.dwSize = 100;
      cci.bVisible = TRUE;
      (void) SetConsoleCursorInfo (cur_screen, &cci);
    }
}

/* hmmm... perhaps these let us bracket screen changes so that we can flush
   clumps rather than one-character-at-a-time...
   
   we'll start with not moving the cursor while an update is in progress.  */
void
update_begin (FRAME_PTR f)
{
}

void
update_end (FRAME_PTR f)
{
  SetConsoleCursorPosition (cur_screen, cursor_coords);
}

void
set_terminal_window (int size)
{
}

void
unset_kbd (void)
{
  SetConsoleMode (keyboard_handle, ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT |
		  ENABLE_ECHO_INPUT | ENABLE_MOUSE_INPUT);
}

void
reset_kbd (void)
{
  keyboard_handle = GetStdHandle (STD_INPUT_HANDLE);
  SetConsoleMode (keyboard_handle, ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT);
}

typedef int (*term_hook) ();

void
initialize_win_nt_display (void)
{
  CONSOLE_SCREEN_BUFFER_INFO	info;
  
  cursor_to_hook		= (term_hook) move_cursor;
  raw_cursor_to_hook		= (term_hook) move_cursor;
  clear_to_end_hook		= (term_hook) clear_to_end;
  clear_frame_hook		= (term_hook) clear_frame;
  clear_end_of_line_hook	= (term_hook) clear_end_of_line;
  ins_del_lines_hook		= (term_hook) ins_del_lines;
  change_line_highlight_hook	= (term_hook) change_line_highlight;
  reassert_line_highlight_hook  = (term_hook) reassert_line_highlight;
  insert_glyphs_hook		= (term_hook) insert_glyphs;
  write_glyphs_hook		= (term_hook) write_glyphs;
  delete_glyphs_hook		= (term_hook) delete_glyphs;
  ring_bell_hook		= (term_hook) ring_bell;
  reset_terminal_modes_hook	= (term_hook) reset_terminal_modes;
  set_terminal_modes_hook	= (term_hook) set_terminal_modes;
  set_terminal_window_hook	= (term_hook) set_terminal_window;
  update_begin_hook		= (term_hook) update_begin;
  update_end_hook		= (term_hook) update_end;
  
  read_socket_hook = win32_read_socket;
  mouse_position_hook = win32_mouse_position;
  
  prev_screen = GetStdHandle (STD_OUTPUT_HANDLE);
  
  set_terminal_modes ();
  
  GetConsoleScreenBufferInfo (cur_screen, &info);
  
  meta_key = 1;
  char_attr = info.wAttributes & 0xFF;
  char_attr_normal = char_attr;
  char_attr_reverse = ((char_attr & 0xf) << 4) + ((char_attr & 0xf0) >> 4);
  
  FRAME_HEIGHT (selected_frame) = info.dwSize.Y;	/* lines per page */
  FRAME_WIDTH (selected_frame) = info.dwSize.X;  /* characters per line */
  
  move_cursor (0, 0);
  
  clear_frame ();
}

DEFUN ("set-screen-color", Fset_screen_color, Sset_screen_color, 2, 2, 0,
       "Set screen colors.")
    (foreground, background)
    Lisp_Object foreground;
    Lisp_Object background;
{
  char_attr_normal = XFASTINT (foreground) + (XFASTINT (background) << 4);
  char_attr_reverse = XFASTINT (background) + (XFASTINT (foreground) << 4);

  Frecenter (Qnil);
  return Qt;
}

DEFUN ("set-cursor-size", Fset_cursor_size, Sset_cursor_size, 1, 1, 0,
       "Set cursor size.")
    (size)
    Lisp_Object size;
{
  CONSOLE_CURSOR_INFO cci;
  cci.dwSize = XFASTINT (size);
  cci.bVisible = TRUE;
  (void) SetConsoleCursorInfo (cur_screen, &cci);
  
  return Qt;
}

void
pixel_to_glyph_coords (FRAME_PTR f, int pix_x, int pix_y, int *x, int *y,
		      void *bounds, int noclip)
{
  *x = pix_x;
  *y = pix_y;
}

void
glyph_to_pixel_coords (FRAME_PTR f, int x, int y, int *pix_x, int *pix_y)
{
  *pix_x = x;
  *pix_y = y;
}

void
syms_of_ntterm ()
{
  defsubr (&Sset_screen_color);
  defsubr (&Sset_cursor_size);
  defsubr (&Sset_message_beep);
}
