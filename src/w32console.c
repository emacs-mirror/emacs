/* Terminal hooks for GNU Emacs on the Microsoft W32 API.
   Copyright (C) 1992, 1999 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.

   Tim Fleehart (apollo@online.com)		1-17-92
   Geoff Voelker (voelker@cs.washington.edu)	9-12-93
*/


#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <windows.h>
#include <string.h>

#include "lisp.h"
#include "charset.h"
#include "coding.h"
#include "disptab.h"
#include "termhooks.h"
#include "dispextern.h"
/* Disable features in frame.h that require a Window System.  */
#undef HAVE_WINDOW_SYSTEM
#include "frame.h"
#include "w32inevt.h"

/* from window.c */
extern Lisp_Object Frecenter ();

/* from keyboard.c */
extern int detect_input_pending ();

/* from sysdep.c */
extern int read_input_pending ();

extern struct frame * updating_frame;
extern int meta_key;

static void move_cursor (int row, int col);
static void clear_to_end (void);
static void clear_frame (void);
static void clear_end_of_line (int);
static void ins_del_lines (int vpos, int n);
static void insert_glyphs (struct glyph *start, int len);
static void write_glyphs (struct glyph *string, int len);
static void delete_glyphs (int n);
void w32_sys_ring_bell (void);
static void reset_terminal_modes (void);
static void set_terminal_modes (void);
static void set_terminal_window (int size);
static void update_begin (struct frame * f);
static void update_end (struct frame * f);
static WORD w32_face_attributes (struct frame *f, int face_id);

static COORD	cursor_coords;
static HANDLE	prev_screen, cur_screen;
static WORD	char_attr_normal;
static DWORD   prev_console_mode;

#ifndef USE_SEPARATE_SCREEN
static CONSOLE_CURSOR_INFO prev_console_cursor;
#endif

/* Determine whether to make frame dimensions match the screen buffer,
   or the current window size.  The former is desirable when running
   over telnet, while the latter is more useful when working directly at
   the console with a large scroll-back buffer.  */
int w32_use_full_screen_buffer;
HANDLE  keyboard_handle;


/* Setting this as the ctrl handler prevents emacs from being killed when
   someone hits ^C in a 'suspended' session (child shell).
   Also ignore Ctrl-Break signals.  */

BOOL
ctrl_c_handler (unsigned long type)
{
  /* Only ignore "interrupt" events when running interactively.  */
  return (!noninteractive
	  && (type == CTRL_C_EVENT || type == CTRL_BREAK_EVENT));
}

/* If we're updating a frame, use it as the current frame
   Otherwise, use the selected frame.  */
#define PICK_FRAME() (updating_frame ? updating_frame : SELECTED_FRAME ())

/* Move the cursor to (row, col).  */
void
move_cursor (int row, int col)
{
  cursor_coords.X = col;
  cursor_coords.Y = row;
  
  if (updating_frame == (struct frame *) NULL)
    {
      SetConsoleCursorPosition (cur_screen, cursor_coords);
    }
}

/* Clear from cursor to end of screen.  */
void
clear_to_end (void)
{
  struct frame * f = PICK_FRAME ();
  
  clear_end_of_line (FRAME_WIDTH (f) - 1);
  ins_del_lines (cursor_coords.Y, FRAME_HEIGHT (f) - cursor_coords.Y - 1);
}

/* Clear the frame.  */
void
clear_frame (void)
{
  struct frame *  f = PICK_FRAME ();
  COORD	     dest;
  int        n;
  DWORD      r;
  CONSOLE_SCREEN_BUFFER_INFO info;

  GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), &info);

  /* Remember that the screen buffer might be wider than the window.  */
  n = FRAME_HEIGHT (f) * info.dwSize.X;
  dest.X = dest.Y = 0;

  FillConsoleOutputAttribute (cur_screen, char_attr_normal, n, dest, &r);
  FillConsoleOutputCharacter (cur_screen, ' ', n, dest, &r);

  move_cursor (0, 0);
}


static struct glyph glyph_base[256];
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
	  memcpy (&glyph_base[i], &space_glyph, sizeof (struct glyph));
        }
      ceol_initialized = TRUE;
    }
  write_glyphs (glyph_base, end - cursor_coords.X);	/* fencepost ?	*/
}

/* Insert n lines at vpos. if n is negative delete -n lines.  */
void
ins_del_lines (int vpos, int n)
{
  int	     i, nb;
  SMALL_RECT scroll;
  COORD	     dest;
  CHAR_INFO  fill;
  struct frame *  f = PICK_FRAME ();

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
  
  fill.Char.AsciiChar = 0x20;
  fill.Attributes = char_attr_normal;
  
  ScrollConsoleScreenBuffer (cur_screen, &scroll, NULL, dest, &fill);

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
  struct frame *  f = PICK_FRAME ();
  
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
  fill.Attributes = char_attr_normal;

  ScrollConsoleScreenBuffer (cur_screen, &scroll, NULL, dest, &fill);
}


/* If start is zero insert blanks instead of a string at start ?. */
void
insert_glyphs (register struct glyph *start, register int len)
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
write_glyphs (register struct glyph *string, register int len)
{
  int produced, consumed;
  DWORD r;
  struct frame * f = PICK_FRAME ();
  WORD char_attr;
  unsigned char conversion_buffer[1024];
  int conversion_buffer_size = sizeof conversion_buffer;

  if (len <= 0)
    return;

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
      char_attr = w32_face_attributes (f, face_id);

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
              /* Set the attribute for these characters.  */
              if (!FillConsoleOutputAttribute (cur_screen, char_attr,
                                               produced, cursor_coords, &r)) 
                {
                  printf ("Failed writing console attributes: %d\n",
                          GetLastError ());
                  fflush (stdout);
                }

              /* Write the characters.  */
              if (!WriteConsoleOutputCharacter (cur_screen, conversion_buffer,
                                                produced, cursor_coords, &r))
                {
                  printf ("Failed writing console characters: %d\n",
                          GetLastError ());
                  fflush (stdout);
                }

              cursor_coords.X += produced;
              move_cursor (cursor_coords.Y, cursor_coords.X);
            }    
          len -= consumed;
          n -= consumed;
          string += consumed;
        }
    }

  /* We may have to output some codes to terminate the writing.  */
  if (CODING_REQUIRE_FLUSHING (&terminal_coding))
    {
      terminal_coding.mode |= CODING_MODE_LAST_BLOCK;
      encode_coding (&terminal_coding, "", conversion_buffer,
		     0, conversion_buffer_size);
      if (terminal_coding.produced > 0)
        {
          if (!FillConsoleOutputAttribute (cur_screen, char_attr_normal,
                                           terminal_coding.produced,
                                           cursor_coords, &r)) 
            {
              printf ("Failed writing console attributes: %d\n",
                      GetLastError ());
              fflush (stdout);
            }

          /* Write the characters.  */
          if (!WriteConsoleOutputCharacter (cur_screen, conversion_buffer,
                                            produced, cursor_coords, &r))
            {
              printf ("Failed writing console characters: %d\n",
                      GetLastError ());
              fflush (stdout);
            }
        }
    }
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
#define MB_EMACS_SILENT (0xFFFFFFFF - 1)

void
w32_sys_ring_bell (void)
{
  if (sound_type == 0xFFFFFFFF) 
    {
      Beep (666, 100);
    }
  else if (sound_type == MB_EMACS_SILENT)
    {
      /* Do nothing.  */
    }
  else
    MessageBeep (sound_type);
}

DEFUN ("set-message-beep", Fset_message_beep, Sset_message_beep, 1, 1, 0,
       doc: /* Set the sound generated when the bell is rung.
SOUND is 'asterisk, 'exclamation, 'hand, 'question, 'ok, or 'silent
to use the corresponding system sound for the bell.  The 'silent sound
prevents Emacs from making any sound at all.
SOUND is nil to use the normal beep.  */)
     (sound)
     Lisp_Object sound;
{
  CHECK_SYMBOL (sound);

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
  else if (EQ (sound, intern ("silent")))
      sound_type = MB_EMACS_SILENT;
  else
      sound_type = 0xFFFFFFFF;

  return sound;
}
   
void
reset_terminal_modes (void)
{
#ifdef USE_SEPARATE_SCREEN
  SetConsoleActiveScreenBuffer (prev_screen);
#else
  SetConsoleCursorInfo (prev_screen, &prev_console_cursor);
#endif
  SetConsoleMode (keyboard_handle, prev_console_mode);
}

void
set_terminal_modes (void)
{
  CONSOLE_CURSOR_INFO cci;

  /* make cursor big and visible (100 on Win95 makes it disappear)  */
  cci.dwSize = 99;
  cci.bVisible = TRUE;
  (void) SetConsoleCursorInfo (cur_screen, &cci);

  SetConsoleActiveScreenBuffer (cur_screen);

  SetConsoleMode (keyboard_handle, ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT);

  /* Initialize input mode: interrupt_input off, no flow control, allow
     8 bit character input, standard quit char.  */
  Fset_input_mode (Qnil, Qnil, make_number (2), Qnil);
}

/* hmmm... perhaps these let us bracket screen changes so that we can flush
   clumps rather than one-character-at-a-time...
   
   we'll start with not moving the cursor while an update is in progress.  */
void
update_begin (struct frame * f)
{
}

void
update_end (struct frame * f)
{
  SetConsoleCursorPosition (cur_screen, cursor_coords);
}

void
set_terminal_window (int size)
{
}

/***********************************************************************
				Faces
 ***********************************************************************/


/* Turn appearances of face FACE_ID on tty frame F on.  */

static WORD
w32_face_attributes (f, face_id)
     struct frame *f;
     int face_id;
{
  WORD char_attr;
  struct face *face = FACE_FROM_ID (f, face_id);

  xassert (face != NULL);

  char_attr = char_attr_normal;

  if (face->foreground != FACE_TTY_DEFAULT_FG_COLOR
      && face->foreground != FACE_TTY_DEFAULT_COLOR)
    char_attr = (char_attr & 0xfff0) + (face->foreground % 16);

  if (face->background != FACE_TTY_DEFAULT_BG_COLOR
      && face->background != FACE_TTY_DEFAULT_COLOR)
    char_attr = (char_attr & 0xff0f) + ((face->background % 16) << 4); 


  /* NTEMACS_TODO: Faces defined during startup get both foreground
     and background of 0. Need a better way around this - for now detect
     the problem and invert one of the faces to make the text readable. */
  if (((char_attr & 0x00f0) >> 4) == (char_attr & 0x000f))
    char_attr ^= 0x0007;

  if (face->tty_reverse_p)
    char_attr = (char_attr & 0xff00) + ((char_attr & 0x000f) << 4)
      + ((char_attr & 0x00f0) >> 4);

  return char_attr;
}


/* Emulation of some X window features from xfns.c and xfaces.c.  */

extern char unspecified_fg[], unspecified_bg[];


/* Given a color index, return its standard name.  */
Lisp_Object
vga_stdcolor_name (int idx)
{
  /* Standard VGA colors, in the order of their standard numbering
     in the default VGA palette.  */
  static char *vga_colors[16] = {
    "black", "blue", "green", "cyan", "red", "magenta", "brown",
    "lightgray", "darkgray", "lightblue", "lightgreen", "lightcyan",
    "lightred", "lightmagenta", "yellow", "white"
  };

  extern Lisp_Object Qunspecified;

  if (idx >= 0 && idx < sizeof (vga_colors) / sizeof (vga_colors[0]))
    return build_string (vga_colors[idx]);
  else
    return Qunspecified;	/* meaning the default */
}

typedef int (*term_hook) ();

void
initialize_w32_display (void)
{
  CONSOLE_SCREEN_BUFFER_INFO	info;
  
  cursor_to_hook		= move_cursor;
  raw_cursor_to_hook		= move_cursor;
  clear_to_end_hook		= clear_to_end;
  clear_frame_hook		= clear_frame;
  clear_end_of_line_hook	= clear_end_of_line;
  ins_del_lines_hook		= ins_del_lines;
  insert_glyphs_hook		= insert_glyphs;
  write_glyphs_hook		= write_glyphs;
  delete_glyphs_hook		= delete_glyphs;
  ring_bell_hook		= w32_sys_ring_bell;
  reset_terminal_modes_hook	= reset_terminal_modes;
  set_terminal_modes_hook	= set_terminal_modes;
  set_terminal_window_hook	= set_terminal_window;
  update_begin_hook		= update_begin;
  update_end_hook		= update_end;
  
  read_socket_hook = w32_console_read_socket;
  mouse_position_hook = w32_console_mouse_position;
  estimate_mode_line_height_hook = 0;

  /* Initialize interrupt_handle.  */
  init_crit ();

  /* Remember original console settings.  */
  keyboard_handle = GetStdHandle (STD_INPUT_HANDLE);
  GetConsoleMode (keyboard_handle, &prev_console_mode);

  prev_screen = GetStdHandle (STD_OUTPUT_HANDLE);
  
#ifdef USE_SEPARATE_SCREEN
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
#else
  cur_screen = prev_screen;
  GetConsoleCursorInfo (prev_screen, &prev_console_cursor);
#endif

  /* Respect setting of LINES and COLUMNS environment variables.  */
  {
    char * lines = getenv("LINES");
    char * columns = getenv("COLUMNS");

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

  GetConsoleScreenBufferInfo (cur_screen, &info);
  
  meta_key = 1;
  char_attr_normal = info.wAttributes;

  if (w32_use_full_screen_buffer)
    {
      FRAME_HEIGHT (SELECTED_FRAME ()) = info.dwSize.Y;	/* lines per page */
      SET_FRAME_WIDTH (SELECTED_FRAME (), info.dwSize.X);  /* characters per line */
    }
  else
    {
      /* Lines per page.  Use buffer coords instead of buffer size.  */
      FRAME_HEIGHT (SELECTED_FRAME ()) = 1 + info.srWindow.Bottom - 
	info.srWindow.Top; 
      /* Characters per line.  Use buffer coords instead of buffer size.  */
      SET_FRAME_WIDTH (SELECTED_FRAME (), 1 + info.srWindow.Right - 
		       info.srWindow.Left);
    }

  /* Setup w32_display_info structure for this frame. */

  w32_initialize_display_info (build_string ("Console"));

}

DEFUN ("set-screen-color", Fset_screen_color, Sset_screen_color, 2, 2, 0,
       doc: /* Set screen colors.  */)
    (foreground, background)
    Lisp_Object foreground;
    Lisp_Object background;
{
  char_attr_normal = XFASTINT (foreground) + (XFASTINT (background) << 4);

  Frecenter (Qnil);
  return Qt;
}

DEFUN ("set-cursor-size", Fset_cursor_size, Sset_cursor_size, 1, 1, 0,
       doc: /* Set cursor size.  */)
    (size)
    Lisp_Object size;
{
  CONSOLE_CURSOR_INFO cci;
  cci.dwSize = XFASTINT (size);
  cci.bVisible = TRUE;
  (void) SetConsoleCursorInfo (cur_screen, &cci);
  
  return Qt;
}

#ifndef HAVE_NTGUI
void
pixel_to_glyph_coords (struct frame * f, int pix_x, int pix_y, int *x, int *y,
		      void *bounds, int noclip)
{
  *x = pix_x;
  *y = pix_y;
}

void
glyph_to_pixel_coords (struct window * f, int x, int y, int *pix_x, int *pix_y)
{
  *pix_x = x;
  *pix_y = y;
}
#endif /* !HAVE_NTGUI */

void
syms_of_ntterm ()
{
  DEFVAR_BOOL ("w32-use-full-screen-buffer",
               &w32_use_full_screen_buffer,
	       doc: /* Non-nil means make terminal frames use the full screen buffer dimensions.
This is desirable when running Emacs over telnet, and is the default.
A value of nil means use the current console window dimensions; this
may be preferrable when working directly at the console with a large
scroll-back buffer.  */);
  w32_use_full_screen_buffer = 1;

  defsubr (&Sset_screen_color);
  defsubr (&Sset_cursor_size);
  defsubr (&Sset_message_beep);
}
