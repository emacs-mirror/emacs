/* MS-DOS specific C utilities.
   Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

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

/* Contributed by Morten Welinder */
/* New display, keyboard, and mouse control by Kim F. Storm */

/* Note: some of the stuff here was taken from end of sysdep.c in demacs. */

#include <config.h>

#ifdef MSDOS
#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/time.h>
#include <dos.h>
#include <errno.h>
#include <sys/stat.h>    /* for _fixpath */
#if __DJGPP__ >= 2
#include <fcntl.h>
#include <libc/dosio.h>  /* for _USE_LFN */
#endif

#include "dosfns.h"
#include "msdos.h"
#include "systime.h"
#include "termhooks.h"
#include "dispextern.h"
#include "termopts.h"
#include "frame.h"
#include "window.h"
#include <go32.h>
#include <pc.h>
#include <ctype.h>
/* #include <process.h> */
/* Damn that local process.h!  Instead we can define P_WAIT ourselves.  */
#define P_WAIT 1

#ifndef _USE_LFN
#define _USE_LFN 0
#endif

#if __DJGPP__ > 1

#include <signal.h>

#ifndef SYSTEM_MALLOC

#ifdef GNU_MALLOC

/* If other `malloc' than ours is used, force our `sbrk' behave like
   Unix programs expect (resize memory blocks to keep them contiguous).
   If `sbrk' from `ralloc.c' is NOT used, also zero-out sbrk'ed memory,
   because that's what `gmalloc' expects to get.  */
#include <crt0.h>

#ifdef REL_ALLOC
int _crt0_startup_flags = _CRT0_FLAG_UNIX_SBRK;
#else  /* not REL_ALLOC */
int _crt0_startup_flags = (_CRT0_FLAG_UNIX_SBRK | _CRT0_FLAG_FILL_SBRK_MEMORY);
#endif /* not REL_ALLOC */
#endif /* GNU_MALLOC */

#endif /* not SYSTEM_MALLOC */
#endif /* __DJGPP__ > 1 */

static unsigned long
event_timestamp ()
{
  struct time t;
  unsigned long s;
  
  gettime (&t);
  s = t.ti_min;
  s *= 60;
  s += t.ti_sec;
  s *= 1000;
  s += t.ti_hund * 10;
  
  return s;
}


/* ------------------------ Mouse control ---------------------------
 *
 * Coordinates are in screen positions and zero based.
 * Mouse buttons are numbered from left to right and also zero based.
 */

int have_mouse;          /* 0: no, 1: enabled, -1: disabled */
static int mouse_visible;

static int mouse_last_x;
static int mouse_last_y;

static int mouse_button_translate[NUM_MOUSE_BUTTONS];
static int mouse_button_count;

void
mouse_on ()
{
  union REGS regs;

  if (have_mouse > 0 && !mouse_visible)
    {
      if (termscript)
	fprintf (termscript, "<M_ON>");
      regs.x.ax = 0x0001;
      int86 (0x33, &regs, &regs);
      mouse_visible = 1;
    }
}

void
mouse_off ()
{
  union REGS regs;

  if (have_mouse > 0 && mouse_visible)
    {
      if (termscript)
	fprintf (termscript, "<M_OFF>");
      regs.x.ax = 0x0002;
      int86 (0x33, &regs, &regs);
      mouse_visible = 0;
    }
}

void
mouse_moveto (x, y)
     int x, y;
{
  union REGS regs;

  if (termscript)
    fprintf (termscript, "<M_XY=%dx%d>", x, y);
  regs.x.ax = 0x0004;
  mouse_last_x = regs.x.cx = x * 8;
  mouse_last_y = regs.x.dx = y * 8;
  int86 (0x33, &regs, &regs);
}

static int
mouse_pressed (b, xp, yp)
     int b, *xp, *yp;
{
  union REGS regs;

  if (b >= mouse_button_count)
    return 0;
  regs.x.ax = 0x0005;
  regs.x.bx = mouse_button_translate[b];
  int86 (0x33, &regs, &regs);
  if (regs.x.bx)
    *xp = regs.x.cx / 8, *yp = regs.x.dx / 8;
  return (regs.x.bx != 0);
}

static int
mouse_released (b, xp, yp)
     int b, *xp, *yp;
{
  union REGS regs;

  if (b >= mouse_button_count)
    return 0;
  regs.x.ax = 0x0006;
  regs.x.bx = mouse_button_translate[b];
  int86 (0x33, &regs, &regs);
  if (regs.x.bx)
    *xp = regs.x.cx / 8, *yp = regs.x.dx / 8;
  return (regs.x.bx != 0);
}

static void
mouse_get_xy (int *x, int *y)
{
  union REGS regs;

  regs.x.ax = 0x0003;
  int86 (0x33, &regs, &regs);
  *x = regs.x.cx / 8;
  *y = regs.x.dx / 8;
}

void
mouse_get_pos (f, insist, bar_window, part, x, y, time)
     FRAME_PTR *f;
     int insist;
     Lisp_Object *bar_window, *x, *y;
     enum scroll_bar_part *part;
     unsigned long *time;
{
  int ix, iy;
  union REGS regs;

  regs.x.ax = 0x0003;
  int86 (0x33, &regs, &regs);
  *f = selected_frame;
  *bar_window = Qnil;
  mouse_get_xy (&ix, &iy);
  selected_frame->mouse_moved = 0;
  *x = make_number (ix);
  *y = make_number (iy);
  *time = event_timestamp ();
}

static void
mouse_check_moved ()
{
  int x, y;

  mouse_get_xy (&x, &y);
  selected_frame->mouse_moved |= (x != mouse_last_x || y != mouse_last_y);
  mouse_last_x = x;
  mouse_last_y = y;
}

void
mouse_init ()
{
  union REGS regs;

  if (termscript)
    fprintf (termscript, "<M_INIT>");

  regs.x.ax = 0x0021;
  int86 (0x33, &regs, &regs);

  regs.x.ax = 0x0007;
  regs.x.cx = 0;
  regs.x.dx = 8 * (ScreenCols () - 1);
  int86 (0x33, &regs, &regs);

  regs.x.ax = 0x0008;
  regs.x.cx = 0;
  regs.x.dx = 8 * (ScreenRows () - 1);
  int86 (0x33, &regs, &regs);

  mouse_moveto (0, 0);
  mouse_visible = 0;
}

/* ------------------------- Screen control ----------------------
 *
 */

static int internal_terminal = 0;

#ifndef HAVE_X_WINDOWS
extern unsigned char ScreenAttrib;
static int screen_face;
static int highlight;

static int screen_size_X;
static int screen_size_Y;
static int screen_size;

static int current_pos_X;
static int current_pos_Y;
static int new_pos_X;
static int new_pos_Y;

static void *startup_screen_buffer;
static int startup_screen_size_X;
static int startup_screen_size_Y;
static int startup_pos_X;
static int startup_pos_Y;
static unsigned char startup_screen_attrib;

static int term_setup_done;

/* Similar to the_only_frame.  */
struct x_output the_only_x_display;

/* This is never dereferenced.  */
Display *x_current_display;


#define SCREEN_SET_CURSOR() 						\
  if (current_pos_X != new_pos_X || current_pos_Y != new_pos_Y) 	\
    ScreenSetCursor (current_pos_Y = new_pos_Y, current_pos_X = new_pos_X)

static
dos_direct_output (y, x, buf, len)
     int y;
     int x;
     char *buf;
     int len;
{
  int t = (int) ScreenPrimary + 2 * (x + y * screen_size_X);
  
  while (--len >= 0) {
    dosmemput (buf++, 1, t);
    t += 2;
  }
}
#endif

/* Flash the screen as a substitute for BEEPs.  */

#if (__DJGPP__ < 2)
static void
do_visible_bell (xorattr)
     unsigned char xorattr;
{
  asm volatile
    ("  movb   $1,%%dl
visible_bell_0:
	movl   _ScreenPrimary,%%eax
	call   dosmemsetup
	movl   %%eax,%%ebx
	movl   %1,%%ecx
	movb   %0,%%al
	incl   %%ebx
visible_bell_1:
	xorb   %%al,%%gs:(%%ebx)
	addl   $2,%%ebx
	decl   %%ecx
	jne    visible_bell_1
	decb   %%dl
	jne    visible_bell_3
visible_bell_2:
	movzwl %%ax,%%eax
        movzwl %%ax,%%eax
	movzwl %%ax,%%eax
	movzwl %%ax,%%eax
	decw   %%cx
	jne    visible_bell_2
	jmp    visible_bell_0
visible_bell_3:"
     : /* no output */
     : "m" (xorattr), "g" (screen_size)
     : "%eax", "%ebx", /* "%gs",*/ "%ecx", "%edx");
}

static void
ScreenVisualBell (void)
{
  /* This creates an xor-mask that will swap the default fore- and
     background colors.  */
  do_visible_bell (((the_only_x_display.foreground_pixel
		     ^ the_only_x_display.background_pixel)
		    * 0x11) & 0x7f);
}
#endif

#ifndef HAVE_X_WINDOWS

/* Enable bright background colors.  */
static void
bright_bg (void)
{
  union REGS regs;

  regs.h.bl = 0;
  regs.x.ax = 0x1003;
  int86 (0x10, &regs, &regs);
}

/* Set the screen dimensions so that it can show no less than
   ROWS x COLS frame.  */

void
dos_set_window_size (rows, cols)
     int *rows, *cols;
{
  char video_name[30];
  Lisp_Object video_mode;
  int video_mode_value;
  int have_vga = 0;
  union REGS regs;
  int current_rows = ScreenRows (), current_cols = ScreenCols ();

  if (*rows == current_rows && *cols == current_cols)
    return;

  /* Do we have a VGA?  */
  regs.x.ax = 0x1a00;
  int86 (0x10, &regs, &regs);
  if (regs.h.al == 0x1a && regs.h.bl > 5 && regs.h.bl < 13)
    have_vga = 1;

  mouse_off ();

  /* If the user specified a special video mode for these dimensions,
     use that mode.  */
  sprintf (video_name, "screen-dimensions-%dx%d", *rows, *cols);
  video_mode = XSYMBOL (Fintern_soft (build_string (video_name),
				      Qnil))-> value;

  if (INTEGERP (video_mode)
      && (video_mode_value = XINT (video_mode)) > 0)
    {
      regs.x.ax = video_mode_value;
      int86 (0x10, &regs, &regs);

      if (have_mouse)
	{
	  /* Must hardware-reset the mouse, or else it won't update
	     its notion of screen dimensions for some non-standard
	     video modes.  This is *painfully* slow...  */
	  regs.x.ax = 0;
	  int86 (0x33, &regs, &regs);
	}
    }

  /* Find one of the dimensions supported by standard EGA/VGA
     which gives us at least the required dimensions.  */

#if __DJGPP__ > 1

  else
    {
      static struct {
	int rows;
	int need_vga;
      }	std_dimension[] = {
	  {25, 0},
	  {28, 1},
	  {35, 0},
	  {40, 1},
	  {43, 0},
	  {50, 1}
      };
      int i = 0;

      while (i < sizeof (std_dimension) / sizeof (std_dimension[0]))
	{
	 if (std_dimension[i].need_vga <= have_vga
	     && std_dimension[i].rows >= *rows)
	   {
	     if (std_dimension[i].rows != current_rows
		 || *cols != current_cols)
	       _set_screen_lines (std_dimension[i].rows);
	     break;
	   }
	 i++;
	}
    }

#else /* not __DJGPP__ > 1 */

  else if (*rows <= 25)
    {
      if (current_rows != 25 || current_cols != 80)
	{
	  regs.x.ax = 3;
	  int86 (0x10, &regs, &regs);
	  regs.x.ax = 0x1101;
	  regs.h.bl = 0;
	  int86 (0x10, &regs, &regs);
	  regs.x.ax = 0x1200;
	  regs.h.bl = 32;
	  int86 (0x10, &regs, &regs);
	  regs.x.ax = 3;
	  int86 (0x10, &regs, &regs);
	}
    }
  else if (*rows <= 50)
    if (have_vga && (current_rows != 50 || current_cols != 80)
	|| *rows <= 43 && (current_rows != 43 || current_cols != 80))
      {
	regs.x.ax = 3;
	int86 (0x10, &regs, &regs);
	regs.x.ax = 0x1112;
	regs.h.bl = 0;
	int86 (0x10, &regs, &regs);
	regs.x.ax = 0x1200;
	regs.h.bl = 32;
	int86 (0x10, &regs, &regs);
	regs.x.ax = 0x0100;
	regs.x.cx = 7;
	int86 (0x10, &regs, &regs);
      }
#endif /* not __DJGPP__ > 1 */

  if (have_mouse)
    {
      mouse_init ();
      mouse_on ();
    }

  /* Tell the caller what dimensions have been REALLY set.  */
  *rows = ScreenRows ();
  *cols = ScreenCols ();

  /* Enable bright background colors.  */
  bright_bg ();
}

/* If we write a character in the position where the mouse is,
   the mouse cursor may need to be refreshed.  */

static void
mouse_off_maybe ()
{
  int x, y;
  
  if (!mouse_visible)
    return;
  
  mouse_get_xy (&x, &y);
  if (y != new_pos_Y || x < new_pos_X)
    return;
  
  mouse_off ();
}

static
IT_ring_bell ()
{
  if (visible_bell)
    {
      mouse_off ();
      ScreenVisualBell ();
    }
  else
    {
      union REGS inregs, outregs;
      inregs.h.ah = 2;
      inregs.h.dl = 7;
      intdos (&inregs, &outregs);
    }
}

static void
IT_set_face (int face)
{
  struct face *fp;
  extern struct face *intern_face (/* FRAME_PTR, struct face * */);

  if (face == 1 || (face == 0 && highlight))
    fp = FRAME_MODE_LINE_FACE (foo);
  else if (face <= 0 || face >= FRAME_N_COMPUTED_FACES (foo))
    fp = FRAME_DEFAULT_FACE (foo);
  else
    fp = intern_face (selected_frame, FRAME_COMPUTED_FACES (foo)[face]);
  if (termscript)
    fprintf (termscript, "<FACE:%d:%d>", FACE_FOREGROUND (fp), FACE_BACKGROUND (fp));
  screen_face = face;
  ScreenAttrib = (FACE_BACKGROUND (fp) << 4) | FACE_FOREGROUND (fp);
}

static
IT_write_glyphs (GLYPH *str, int len)
{
  int newface;
  int ch, l = len;
  unsigned char *buf, *bp;

  if (len == 0) return;
  
  buf = bp = alloca (len * 2);
  
  while (--l >= 0)
    {
      newface = FAST_GLYPH_FACE (*str);
      if (newface != screen_face)
	IT_set_face (newface);
      ch = FAST_GLYPH_CHAR (*str);
      *bp++ = (unsigned char)ch;
      *bp++ = ScreenAttrib;
      
      if (termscript)
	fputc (ch, termscript);
      str++;
    }

  mouse_off_maybe ();
  dosmemput (buf, 2 * len, 
	     (int)ScreenPrimary + 2 * (new_pos_X + screen_size_X * new_pos_Y));
  new_pos_X += len;
}

static
IT_clear_end_of_line (first_unused)
{
  char *spaces, *sp;
  int i, j;

  IT_set_face (0);
  if (termscript)
    fprintf (termscript, "<CLR:EOL>");
  i = (j = screen_size_X - new_pos_X) * 2;
  spaces = sp = alloca (i);
  
  while (--j >= 0)
    {
      *sp++ = ' ';
      *sp++ = ScreenAttrib;
    }

  mouse_off_maybe ();
  dosmemput (spaces, i, 
	     (int)ScreenPrimary + 2 * (new_pos_X + screen_size_X * new_pos_Y));
}

static
IT_clear_screen (void)
{
  if (termscript)
    fprintf (termscript, "<CLR:SCR>");
  IT_set_face (0);
  mouse_off ();
  ScreenClear ();
  new_pos_X = new_pos_Y = 0;
}

static
IT_clear_to_end (void)
{
  if (termscript)
    fprintf (termscript, "<CLR:EOS>");

  while (new_pos_Y < screen_size_Y) {
    new_pos_X = 0;
    IT_clear_end_of_line (0);
    new_pos_Y++;
  }
}

static
IT_cursor_to (int y, int x)
{
  if (termscript)
    fprintf (termscript, "\n<XY=%dx%d>", x, y);
  new_pos_X = x;
  new_pos_Y = y;
}

static
IT_reassert_line_highlight (new, vpos)
     int new, vpos;
{
  highlight = new;
  IT_set_face (0); /* To possibly clear the highlighting.  */
}

static
IT_change_line_highlight (new_highlight, vpos, first_unused_hpos)
{
  highlight = new_highlight;
  IT_set_face (0); /* To possibly clear the highlighting.  */
  IT_cursor_to (vpos, 0);
  IT_clear_end_of_line (first_unused_hpos);
}

static
IT_update_begin ()
{
  highlight = 0;
  IT_set_face (0); /* To possibly clear the highlighting.  */
  screen_face = -1;
}

static
IT_update_end ()
{
}

/* This was more or less copied from xterm.c

   Nowadays, the corresponding function under X is `x_set_menu_bar_lines_1'
   on xfns.c  */

static void
IT_set_menu_bar_lines (window, n)
     Lisp_Object window;
     int n;
{
  struct window *w = XWINDOW (window);

  XSETFASTINT (w->last_modified, 0);
  XSETFASTINT (w->top, XFASTINT (w->top) + n);
  XSETFASTINT (w->height, XFASTINT (w->height) - n);

  /* Handle just the top child in a vertical split.  */
  if (!NILP (w->vchild))
    IT_set_menu_bar_lines (w->vchild, n);

  /* Adjust all children in a horizontal split.  */
  for (window = w->hchild; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      IT_set_menu_bar_lines (window, n);
    }
}

/* This was copied from xfns.c  */

void
x_set_menu_bar_lines (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  int nlines;
  int olines = FRAME_MENU_BAR_LINES (f);

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itslef, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (INTEGERP (value))
    nlines = XINT (value);
  else
    nlines = 0;

  FRAME_MENU_BAR_LINES (f) = nlines;
  IT_set_menu_bar_lines (f->root_window, nlines - olines);
}

/* IT_set_terminal_modes is called when emacs is started,
   resumed, and whenever the screen is redrawn!  */

static
IT_set_terminal_modes (void)
{
  char *colors;
  FRAME_PTR f;
  struct face *fp;

  if (termscript)
    fprintf (termscript, "\n<SET_TERM>");
  highlight = 0;

  screen_size_X = ScreenCols ();
  screen_size_Y = ScreenRows ();
  screen_size = screen_size_X * screen_size_Y;
  
  new_pos_X = new_pos_Y = 0;
  current_pos_X = current_pos_Y = -1;

  if (term_setup_done)
    return;
  term_setup_done = 1;
  
  startup_screen_size_X = screen_size_X;
  startup_screen_size_Y = screen_size_Y;
  startup_screen_attrib = ScreenAttrib;

  ScreenGetCursor (&startup_pos_Y, &startup_pos_X);
  ScreenRetrieve (startup_screen_buffer = xmalloc (screen_size * 2));

  if (termscript)
    fprintf (termscript, "<SCREEN SAVED (dimensions=%dx%d)>\n",
             screen_size_X, screen_size_Y);

  bright_bg ();
}

/* IT_reset_terminal_modes is called when emacs is
   suspended or killed.  */

static
IT_reset_terminal_modes (void)
{
  int display_row_start = (int) ScreenPrimary;
  int saved_row_len     = startup_screen_size_X * 2;
  int update_row_len    = ScreenCols () * 2;
  int current_rows      = ScreenRows ();
  int to_next_row       = update_row_len;
  unsigned char *saved_row = startup_screen_buffer;
  int cursor_pos_X = ScreenCols () - 1;
  int cursor_pos_Y = ScreenRows () - 1;

  if (termscript)
    fprintf (termscript, "\n<RESET_TERM>");

  highlight = 0;

  if (!term_setup_done)
    return;
  
  mouse_off ();
 
  /* We have a situation here.
     We cannot just do ScreenUpdate(startup_screen_buffer) because
     the luser could have changed screen dimensions inside Emacs
     and failed (or didn't want) to restore them before killing
     Emacs.  ScreenUpdate() uses the *current* screen dimensions and
     thus will happily use memory outside what was allocated for
     `startup_screen_buffer'.
     Thus we only restore as much as the current screen dimensions
     can hold, and clear the rest (if the saved screen is smaller than
     the current) with the color attribute saved at startup.  The cursor
     is also restored within the visible dimensions.  */

  ScreenAttrib = startup_screen_attrib;
  ScreenClear ();

  if (update_row_len > saved_row_len)
    update_row_len = saved_row_len;
  if (current_rows > startup_screen_size_Y)
    current_rows = startup_screen_size_Y;

  if (termscript)
    fprintf (termscript, "<SCREEN RESTORED (dimensions=%dx%d)>\n",
             update_row_len / 2, current_rows);

  while (current_rows--)
    {
      dosmemput (saved_row, update_row_len, display_row_start);
      saved_row         += saved_row_len;
      display_row_start += to_next_row;
    }
  if (startup_pos_X < cursor_pos_X)
    cursor_pos_X = startup_pos_X;
  if (startup_pos_Y < cursor_pos_Y)
    cursor_pos_Y = startup_pos_Y;

  ScreenSetCursor (cursor_pos_Y, cursor_pos_X);
  xfree (startup_screen_buffer);

  term_setup_done = 0;
}

static
IT_set_terminal_window (void)
{
}

void
IT_set_frame_parameters (f, alist)
     FRAME_PTR f;
     Lisp_Object alist;
{
  Lisp_Object tail;
  int redraw;
  extern unsigned long load_color ();

  redraw = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt, prop, val;

      elt = Fcar (tail);
      prop = Fcar (elt);
      val = Fcdr (elt);
      CHECK_SYMBOL (prop, 1);

      if (EQ (prop, intern ("foreground-color")))
	{
	  unsigned long new_color = load_color (f, val);
	  if (new_color != ~0)
	    {
	      FRAME_FOREGROUND_PIXEL (f) = new_color;
	      redraw = 1;
	      if (termscript)
		fprintf (termscript, "<FGCOLOR %d>\n", new_color);
	    }
	}
      else if (EQ (prop, intern ("background-color")))
	{
	  unsigned long new_color = load_color (f, val);
	  if (new_color != ~0)
	    {
	      FRAME_BACKGROUND_PIXEL (f) = new_color;
	      redraw = 1;
	      if (termscript)
		fprintf (termscript, "<BGCOLOR %d>\n", new_color);
	    }
	}
      else if (EQ (prop, intern ("menu-bar-lines")))
	x_set_menu_bar_lines (f, val, 0);
    }

  if (redraw)
    {
      recompute_basic_faces (f);
      if (f == selected_frame)
	redraw_frame (f);
    }
}

#endif /* !HAVE_X_WINDOWS */


/* Do we need the internal terminal?  */

void
internal_terminal_init ()
{
  char *term = getenv ("TERM");
  char *colors;

#ifdef HAVE_X_WINDOWS
  if (!inhibit_window_system)
    return;
#endif

  internal_terminal
    = (!noninteractive) && term && !strcmp (term, "internal");

  if (getenv ("EMACSTEST"))
    termscript = fopen (getenv ("EMACSTEST"), "wt");
  
#ifndef HAVE_X_WINDOWS
  if (!internal_terminal || inhibit_window_system)
    {
      selected_frame->output_method = output_termcap;
      return;
    }

  Vwindow_system = intern ("pc");
  Vwindow_system_version = make_number (1);
 
  bzero (&the_only_x_display, sizeof the_only_x_display);
  the_only_x_display.background_pixel = 7; /* White */
  the_only_x_display.foreground_pixel = 0; /* Black */
  bright_bg ();
  colors = getenv ("EMACSCOLORS");
  if (colors && strlen (colors) >= 2)
    {
      /* The colors use 4 bits each (we enable bright background).  */
      if (isdigit (colors[0]))
        colors[0] -= '0';
      else if (isxdigit (colors[0]))
        colors[0] -= (isupper (colors[0]) ? 'A' : 'a') - 10;
      if (colors[0] >= 0 && colors[0] < 16)
        the_only_x_display.foreground_pixel = colors[0];
      if (isdigit (colors[1]))
        colors[1] -= '0';
      else if (isxdigit (colors[1]))
        colors[1] -= (isupper (colors[1]) ? 'A' : 'a') - 10;
      if (colors[1] >= 0 && colors[1] < 16)
        the_only_x_display.background_pixel = colors[1];
    }
  the_only_x_display.line_height = 1;
  the_only_x_display.font = (XFontStruct *)1;   /* must *not* be zero */

  init_frame_faces (selected_frame);

  ring_bell_hook = IT_ring_bell;
  write_glyphs_hook = IT_write_glyphs;
  cursor_to_hook = raw_cursor_to_hook = IT_cursor_to;
  clear_to_end_hook = IT_clear_to_end;
  clear_end_of_line_hook = IT_clear_end_of_line;
  clear_frame_hook = IT_clear_screen;
  change_line_highlight_hook = IT_change_line_highlight;
  update_begin_hook = IT_update_begin;
  update_end_hook = IT_update_end;
  reassert_line_highlight_hook = IT_reassert_line_highlight;

  /* These hooks are called by term.c without being checked.  */
  set_terminal_modes_hook = IT_set_terminal_modes;
  reset_terminal_modes_hook = IT_reset_terminal_modes;
  set_terminal_window_hook = IT_set_terminal_window;
#endif
}

dos_get_saved_screen (screen, rows, cols)
     char **screen;
     int *rows;
     int *cols;
{
#ifndef HAVE_X_WINDOWS
  *screen = startup_screen_buffer;
  *cols = startup_screen_size_X;
  *rows = startup_screen_size_Y;
  return 1;
#else
  return 0;
#endif  
}

#ifndef HAVE_X_WINDOWS

/* We are not X, but we can emulate it well enough for our needs... */
void
check_x (void)
{
  if (! FRAME_MSDOS_P (selected_frame))
    error ("Not running under a windows system");
}

#endif


/* ----------------------- Keyboard control ----------------------
 *
 * Keymaps reflect the following keyboard layout:
 *
 *    0  1  2  3  4  5  6  7  8  9  10 11 12  BS
 *    TAB 15 16 17 18 19 20 21 22 23 24 25 26 (41)
 *    CLOK 30 31 32 33 34 35 36 37 38 39 40 (41) RET
 *    SH () 45 46 47 48 49 50 51 52 53 54  SHIFT
 *                    SPACE
 */

static int extended_kbd; /* 101 (102) keyboard present.	*/

struct dos_keyboard_map
{
  char *unshifted;
  char *shifted;
  char *alt_gr;
};


static struct dos_keyboard_map us_keyboard = {
/* 0         1         2         3         4         5      */
/* 01234567890123456789012345678901234567890 12345678901234 */
  "`1234567890-=  qwertyuiop[]   asdfghjkl;'\\   zxcvbnm,./  ",
/* 0123456789012345678901234567890123456789 012345678901234 */
  "~!@#$%^&*()_+  QWERTYUIOP{}   ASDFGHJKL:\"|   ZXCVBNM<>?  ",
  0				/* no Alt-Gr key */
};

static struct dos_keyboard_map fr_keyboard = {
/* 0         1         2         3         4         5      */
/* 012 3456789012345678901234567890123456789012345678901234 */
  "˝&Ç\",(-ä_ÄÖ)=  azertyuiop^$   qsdfghjklmó*   wxcvbnm;:!  ",
/* 0123456789012345678901234567890123456789012345678901234 */
  " 1234567890¯+  AZERTYUIOP˘ú   QSDFGHJKLM%Ê   WXCVBN?./ı  ",
/* 01234567 89012345678901234567890123456789012345678901234 */
  "  ~#{[|`\\^@]}             œ                              "
};

static struct dos_keyboard_map dk_keyboard = {
/* 0         1         2         3         4         5      */
/* 0123456789012345678901234567890123456789012345678901234 */
  "´1234567890+|  qwertyuiopÜ~   asdfghjklëõ'   zxcvbnm,.-  ",
/* 01 23456789012345678901234567890123456789012345678901234 */
  "ı!\"#$%&/()=?`  QWERTYUIOPè^   ASDFGHJKLíù*   ZXCVBNM;:_  ",
/* 0123456789012345678901234567890123456789012345678901234 */
  "  @ú$  {[]} |                                             "
};

static struct keyboard_layout_list
{
  int country_code;
  struct dos_keyboard_map *keyboard_map;
} keyboard_layout_list[] =
{
  1, &us_keyboard,
  33, &fr_keyboard,
  45, &dk_keyboard
};

static struct dos_keyboard_map *keyboard;
static int keyboard_map_all;

int
dos_set_keyboard (code, always)
     int code;
     int always;
{
  int i;

  /* Initialize to US settings, for countries that don't have their own.  */
  keyboard = keyboard_layout_list[0].keyboard_map;
  keyboard_map_all = always;
  dos_keyboard_layout = 1;
  
  for (i = 0; i < (sizeof (keyboard_layout_list)/sizeof (struct keyboard_layout_list)); i++)
    if (code == keyboard_layout_list[i].country_code)
      {
	keyboard = keyboard_layout_list[i].keyboard_map;
	keyboard_map_all = always;
	dos_keyboard_layout = code;
	return 1;
      }
  return 0;
}

#define Ignore	0x0000
#define Normal	0x0000	/* normal key - alt changes scan-code */
#define FctKey	0x1000	/* func key if c == 0, else c */
#define Special	0x2000	/* func key even if c != 0 */
#define ModFct	0x3000	/* special if mod-keys, else 'c' */
#define Map	0x4000	/* alt scan-code, map to unshift/shift key */
#define KeyPad	0x5000	/* map to insert/kp-0 depending on c == 0xe0 */
#define Grey	0x6000	/* Grey keypad key */

#define Alt	0x0100	/* alt scan-code */
#define Ctrl	0x0200	/* ctrl scan-code */
#define Shift	0x0400	/* shift scan-code */

static struct
{
  unsigned char char_code;	/* normal code	*/
  unsigned char meta_code;	/* M- code	*/
  unsigned char keypad_code;	/* keypad code	*/
  unsigned char editkey_code;	/* edit key	*/
} keypad_translate_map[] = {
  '0',  '0',  0xb0, /* kp-0 */		0x63, /* insert */
  '1',  '1',  0xb1, /* kp-1 */		0x57, /* end */
  '2',  '2',  0xb2, /* kp-2 */		0x54, /* down */
  '3',  '3',  0xb3, /* kp-3 */		0x56, /* next */
  '4',  '4',  0xb4, /* kp-4 */		0x51, /* left */
  '5',  '5',  0xb5, /* kp-5 */		0xb5, /* kp-5 */
  '6',  '6',  0xb6, /* kp-6 */		0x53, /* right */
  '7',  '7',  0xb7, /* kp-7 */		0x50, /* home */
  '8',  '8',  0xb8, /* kp-8 */		0x52, /* up */
  '9',  '9',  0xb9, /* kp-9 */		0x55, /* prior */
  '.',  '-',  0xae, /* kp-decimal */	0xff  /* delete */
};

static struct
{
  unsigned char char_code;	/* normal code	*/
  unsigned char keypad_code;	/* keypad code	*/
} grey_key_translate_map[] = {
  '/',  0xaf, /* kp-decimal */
  '*',  0xaa, /* kp-multiply */
  '-',  0xad, /* kp-subtract */
  '+',  0xab, /* kp-add */
  '\r', 0x8d  /* kp-enter */
};

static unsigned short
ibmpc_translate_map[] =
{
  /* --------------- 00 to 0f --------------- */
  Normal | 0xff,	/* Ctrl Break + Alt-NNN */
  Alt | ModFct | 0x1b,		/* Escape */
  Normal | 1,			/* '1' */
  Normal | 2,			/* '2' */
  Normal | 3,			/* '3' */
  Normal | 4,			/* '4' */
  Normal | 5,			/* '5' */
  Normal | 6,			/* '6' */
  Normal | 7,			/* '7' */
  Normal | 8,			/* '8' */
  Normal | 9,			/* '9' */
  Normal | 10,			/* '0' */
  Normal | 11,			/* '-' */
  Normal | 12,			/* '=' */
  Special | 0x08,		/* Backspace */
  ModFct | 0x74,		/* Tab/Backtab */

  /* --------------- 10 to 1f --------------- */
  Map | 15,			/* 'q' */
  Map | 16,			/* 'w' */
  Map | 17,			/* 'e' */
  Map | 18,			/* 'r' */
  Map | 19,			/* 't' */
  Map | 20,			/* 'y' */
  Map | 21,			/* 'u' */
  Map | 22,			/* 'i' */
  Map | 23,			/* 'o' */
  Map | 24,			/* 'p' */
  Map | 25,			/* '[' */
  Map | 26,			/* ']' */ 
  ModFct | 0x0d,		/* Return */
  Ignore,			/* Ctrl */
  Map | 30,			/* 'a' */
  Map | 31,			/* 's' */

  /* --------------- 20 to 2f --------------- */
  Map | 32,			/* 'd' */
  Map | 33,			/* 'f' */
  Map | 34,			/* 'g' */
  Map | 35,			/* 'h' */
  Map | 36,			/* 'j' */
  Map | 37,			/* 'k' */
  Map | 38,			/* 'l' */
  Map | 39,			/* ';' */
  Map | 40,			/* '\'' */
  Map |  0,			/* '`' */
  Ignore,			/* Left shift */
  Map | 41,			/* '\\' */
  Map | 45,			/* 'z' */
  Map | 46,			/* 'x' */
  Map | 47,			/* 'c' */
  Map | 48,			/* 'v' */

  /* --------------- 30 to 3f --------------- */
  Map | 49,			/* 'b' */
  Map | 50,			/* 'n' */
  Map | 51,			/* 'm' */
  Map | 52,			/* ',' */
  Map | 53,			/* '.' */
  Map | 54,			/* '/' */
  Ignore,			/* Right shift */
  Grey | 1,			/* Grey * */
  Ignore,			/* Alt */
  Normal |  ' ',		/* ' ' */
  Ignore,			/* Caps Lock */
  FctKey | 0xbe,		/* F1 */
  FctKey | 0xbf,		/* F2 */
  FctKey | 0xc0,		/* F3 */
  FctKey | 0xc1,		/* F4 */
  FctKey | 0xc2,		/* F5 */

  /* --------------- 40 to 4f --------------- */
  FctKey | 0xc3,		/* F6 */
  FctKey | 0xc4,		/* F7 */
  FctKey | 0xc5,		/* F8 */
  FctKey | 0xc6,		/* F9 */
  FctKey | 0xc7,		/* F10 */
  Ignore,			/* Num Lock */
  Ignore,			/* Scroll Lock */
  KeyPad | 7,			/* Home */
  KeyPad | 8,			/* Up */
  KeyPad | 9,			/* Page Up */
  Grey | 2,			/* Grey - */
  KeyPad | 4,			/* Left */
  KeyPad | 5,			/* Keypad 5 */
  KeyPad | 6,			/* Right */
  Grey | 3,			/* Grey + */
  KeyPad | 1,			/* End */

  /* --------------- 50 to 5f --------------- */
  KeyPad | 2,			/* Down */
  KeyPad | 3,			/* Page Down */
  KeyPad | 0,			/* Insert */
  KeyPad | 10,			/* Delete */
  Shift | FctKey | 0xbe,	/* (Shift) F1 */
  Shift | FctKey | 0xbf,	/* (Shift) F2 */
  Shift | FctKey | 0xc0,	/* (Shift) F3 */
  Shift | FctKey | 0xc1,	/* (Shift) F4 */
  Shift | FctKey | 0xc2,	/* (Shift) F5 */
  Shift | FctKey | 0xc3,	/* (Shift) F6 */
  Shift | FctKey | 0xc4,	/* (Shift) F7 */
  Shift | FctKey | 0xc5,	/* (Shift) F8 */
  Shift | FctKey | 0xc6,	/* (Shift) F9 */
  Shift | FctKey | 0xc7,	/* (Shift) F10 */
  Ctrl | FctKey | 0xbe,		/* (Ctrl) F1 */
  Ctrl | FctKey | 0xbf,		/* (Ctrl) F2 */

  /* --------------- 60 to 6f --------------- */
  Ctrl | FctKey | 0xc0,		/* (Ctrl) F3 */
  Ctrl | FctKey | 0xc1,		/* (Ctrl) F4 */
  Ctrl | FctKey | 0xc2,		/* (Ctrl) F5 */
  Ctrl | FctKey | 0xc3,		/* (Ctrl) F6 */
  Ctrl | FctKey | 0xc4,		/* (Ctrl) F7 */
  Ctrl | FctKey | 0xc5,		/* (Ctrl) F8 */
  Ctrl | FctKey | 0xc6,		/* (Ctrl) F9 */
  Ctrl | FctKey | 0xc7,		/* (Ctrl) F10 */
  Alt | FctKey | 0xbe,		/* (Alt) F1 */
  Alt | FctKey | 0xbf,		/* (Alt) F2 */
  Alt | FctKey | 0xc0,		/* (Alt) F3 */
  Alt | FctKey | 0xc1,		/* (Alt) F4 */
  Alt | FctKey | 0xc2,		/* (Alt) F5 */
  Alt | FctKey | 0xc3,		/* (Alt) F6 */
  Alt | FctKey | 0xc4,		/* (Alt) F7 */
  Alt | FctKey | 0xc5,		/* (Alt) F8 */

  /* --------------- 70 to 7f --------------- */
  Alt | FctKey | 0xc6,		/* (Alt) F9 */
  Alt | FctKey | 0xc7,		/* (Alt) F10 */
  Ctrl | FctKey | 0x6d,		/* (Ctrl) Sys Rq */
  Ctrl | KeyPad | 4,		/* (Ctrl) Left */
  Ctrl | KeyPad | 6,		/* (Ctrl) Right */
  Ctrl | KeyPad | 1,		/* (Ctrl) End */
  Ctrl | KeyPad | 3,		/* (Ctrl) Page Down */
  Ctrl | KeyPad | 7,		/* (Ctrl) Home */
  Alt | Map | 1,		/* '1' */
  Alt | Map | 2,		/* '2' */
  Alt | Map | 3,		/* '3' */
  Alt | Map | 4,		/* '4' */
  Alt | Map | 5,		/* '5' */
  Alt | Map | 6,		/* '6' */
  Alt | Map | 7,		/* '7' */
  Alt | Map | 8,		/* '8' */

  /* --------------- 80 to 8f --------------- */
  Alt | Map | 9,		/* '9' */
  Alt | Map | 10,		/* '0' */
  Alt | Map | 11,		/* '-' */
  Alt | Map | 12,		/* '=' */
  Ctrl | KeyPad | 9,		/* (Ctrl) Page Up */
  FctKey | 0xc8,		/* F11 */
  FctKey | 0xc9,		/* F12 */
  Shift | FctKey | 0xc8,	/* (Shift) F11 */
  Shift | FctKey | 0xc9,	/* (Shift) F12 */
  Ctrl | FctKey | 0xc8,		/* (Ctrl) F11 */
  Ctrl | FctKey | 0xc9,		/* (Ctrl) F12 */
  Alt | FctKey | 0xc8,		/* (Alt) F11 */
  Alt | FctKey | 0xc9,		/* (Alt) F12 */
  Ctrl | KeyPad | 8,		/* (Ctrl) Up */
  Ctrl | Grey | 2,		/* (Ctrl) Grey - */
  Ctrl | KeyPad | 5,		/* (Ctrl) Keypad 5 */

  /* --------------- 90 to 9f --------------- */
  Ctrl | Grey | 3,		/* (Ctrl) Grey + */
  Ctrl | KeyPad | 2,		/* (Ctrl) Down */
  Ctrl | KeyPad | 0,		/* (Ctrl) Insert */
  Ctrl | KeyPad | 10,		/* (Ctrl) Delete */
  Ctrl | FctKey | 0x09,		/* (Ctrl) Tab */
  Ctrl | Grey | 0,		/* (Ctrl) Grey / */
  Ctrl | Grey | 1,		/* (Ctrl) Grey * */
  Alt | FctKey | 0x50,		/* (Alt) Home */
  Alt | FctKey | 0x52,		/* (Alt) Up */
  Alt | FctKey | 0x55,		/* (Alt) Page Up */
  Ignore,			/* NO KEY */
  Alt | FctKey | 0x51,		/* (Alt) Left */
  Ignore,			/* NO KEY */
  Alt | FctKey | 0x53,		/* (Alt) Right */
  Ignore,			/* NO KEY */
  Alt | FctKey | 0x57,		/* (Alt) End */

  /* --------------- a0 to af --------------- */
  Alt | KeyPad | 2,		/* (Alt) Down */
  Alt | KeyPad | 3,		/* (Alt) Page Down */
  Alt | KeyPad | 0,		/* (Alt) Insert */
  Alt | KeyPad | 10,		/* (Alt) Delete */
  Alt | Grey | 0,		/* (Alt) Grey / */
  Alt | FctKey | 0x09,		/* (Alt) Tab */
  Alt | Grey | 4		/* (Alt) Keypad Enter */
};

/* These bit-positions corresponds to values returned by BIOS */
#define SHIFT_P		0x0003	/* two bits! */
#define CTRL_P		0x0004
#define ALT_P		0x0008
#define SCRLOCK_P	0x0010
#define NUMLOCK_P	0x0020
#define CAPSLOCK_P	0x0040
#define ALT_GR_P	0x0800
#define SUPER_P		0x4000	/* pseudo */
#define HYPER_P		0x8000	/* pseudo */

static int
dos_get_modifiers (keymask)
     int *keymask;
{
  union REGS regs;
  int mask;
  int modifiers = 0;
  
  /* Calculate modifier bits */
  regs.h.ah = extended_kbd ? 0x12 : 0x02;
  int86 (0x16, &regs, &regs);

  if (!extended_kbd)
    {
      mask = regs.h.al & (SHIFT_P | CTRL_P | ALT_P | 
			  SCRLOCK_P | NUMLOCK_P | CAPSLOCK_P);
    }
  else
    {
      mask = regs.h.al & (SHIFT_P |
			  SCRLOCK_P | NUMLOCK_P | CAPSLOCK_P);
  
      /* Do not break international keyboard support.   */
      /* When Keyb.Com is loaded, the right Alt key is  */
      /* used for accessing characters like { and } 	  */
      if (regs.h.ah & 2)		/* Left ALT pressed ? */
	mask |= ALT_P;

      if ((regs.h.ah & 8) != 0)		/* Right ALT pressed ? */
	{
	  mask |= ALT_GR_P;
	  if (dos_hyper_key == 1)
	    {
	      mask |= HYPER_P;
	      modifiers |= hyper_modifier;
	    }
	  else if (dos_super_key == 1)
	    {
	      mask |= SUPER_P;
	      modifiers |= super_modifier;
	    }
	}
      
      if (regs.h.ah & 1)		/* Left CTRL pressed ? */
	mask |= CTRL_P;

      if (regs.h.ah & 4)	 	/* Right CTRL pressed ? */
	{
	  if (dos_hyper_key == 2)
	    {
	      mask |= HYPER_P;
	      modifiers |= hyper_modifier;
	    }
	  else if (dos_super_key == 2)
	    {
	      mask |= SUPER_P;
	      modifiers |= super_modifier;
	    }
	  else
	    mask |= CTRL_P;
	}
    }

  if (mask & SHIFT_P)
    modifiers |= shift_modifier;
  if (mask & CTRL_P)
    modifiers |= ctrl_modifier;
  if (mask & ALT_P)
    modifiers |= meta_modifier;

  if (keymask)
    *keymask = mask;
  return modifiers;
}

#define NUM_RECENT_DOSKEYS (100)
int recent_doskeys_index;	/* Index for storing next element into recent_doskeys */
int total_doskeys;		/* Total number of elements stored into recent_doskeys */
Lisp_Object recent_doskeys; /* A vector, holding the last 100 keystrokes */

DEFUN ("recent-doskeys", Frecent_doskeys, Srecent_doskeys, 0, 0, 0,
  "Return vector of last 100 keyboard input values seen in dos_rawgetc.\n\
Each input key receives two values in this vector: first the ASCII code,\n\
and then the scan code.")
     ()
{
  Lisp_Object *keys = XVECTOR (recent_doskeys)->contents;
  Lisp_Object val;

  if (total_doskeys < NUM_RECENT_DOSKEYS)
    return Fvector (total_doskeys, keys);
  else
    {
      val = Fvector (NUM_RECENT_DOSKEYS, keys);
      bcopy (keys + recent_doskeys_index,
	     XVECTOR (val)->contents,
	     (NUM_RECENT_DOSKEYS - recent_doskeys_index) * sizeof (Lisp_Object));
      bcopy (keys,
	     XVECTOR (val)->contents + NUM_RECENT_DOSKEYS - recent_doskeys_index,
	     recent_doskeys_index * sizeof (Lisp_Object));
      return val;
    }
}

/* Get a char from keyboard.  Function keys are put into the event queue.  */

static int
dos_rawgetc ()
{
  struct input_event event;
  union REGS regs;
  
#ifndef HAVE_X_WINDOWS
  SCREEN_SET_CURSOR ();
  if (!mouse_visible) mouse_on ();
#endif
    
  /* The following condition is equivalent to `kbhit ()', except that
     it uses the bios to do its job.  This pleases DESQview/X.  */
  while ((regs.h.ah = extended_kbd ? 0x11 : 0x01),
	 int86 (0x16, &regs, &regs),
	 (regs.x.flags & 0x40) == 0)
    {
      union REGS regs;
      register unsigned char c;
      int sc, code, mask, kp_mode;
      int modifiers;

      regs.h.ah = extended_kbd ? 0x10 : 0x00;
      int86 (0x16, &regs, &regs);
      c = regs.h.al;
      sc = regs.h.ah;

      total_doskeys += 2;
      XVECTOR (recent_doskeys)->contents[recent_doskeys_index++]
	= make_number (c);
      if (recent_doskeys_index == NUM_RECENT_DOSKEYS)
	recent_doskeys_index = 0;
      XVECTOR (recent_doskeys)->contents[recent_doskeys_index++]
	= make_number (sc);
      if (recent_doskeys_index == NUM_RECENT_DOSKEYS)
	recent_doskeys_index = 0;

      modifiers = dos_get_modifiers (&mask);
      
#ifndef HAVE_X_WINDOWS
      if (!NILP (Vdos_display_scancodes))
	{
	  char buf[11];
	  sprintf (buf, "%02x:%02x*%04x",
		   (unsigned) (sc&0xff), (unsigned) c, mask);
	  dos_direct_output (screen_size_Y - 2, screen_size_X - 12, buf, 10);
	}
#endif

      if (sc == 0xe0)
	{
	  switch (c)
	    {
	    case 10:		/* Ctrl Grey Enter */
	      code = Ctrl | Grey | 4;
	      break;
	    case 13:		/* Grey Enter */
	      code = Grey | 4;
	      break;
	    case '/':		/* Grey / */
	      code = Grey | 0;
	      break;
	    default:
	      continue;
	    };
	  c = 0;
	}
      else
	{
	  if (sc >= (sizeof (ibmpc_translate_map) / sizeof (short)))
	    continue;
	  if ((code = ibmpc_translate_map[sc]) == Ignore)
	    continue;
	}
      
      if (c == 0)
	{
	  if (code & Alt)
	    modifiers |= meta_modifier;
	  if (code & Ctrl)
	    modifiers |= ctrl_modifier;
	  if (code & Shift)
	    modifiers |= shift_modifier;
	}
      
      switch (code & 0xf000)
	{
	case ModFct:
	  if (c && !(mask & (SHIFT_P | ALT_P | CTRL_P | HYPER_P | SUPER_P)))
	    return c;
	  c = 0;		/* Special */
	  
	case FctKey:
	  if (c != 0)
	    return c;
	    
	case Special:
	  code |= 0xff00;
	  break;
	  
	case Normal:
	  if (sc == 0)
	    {
	      if (c == 0)	/* ctrl-break */
		continue;
	      return c;		/* ALT-nnn */
	    }
	  if (!keyboard_map_all)
	    {
	      if (c != ' ')
		return c;
	      code = c;
	      break;
	    }
	  
	case Map:
	  if (c && !(mask & ALT_P) && !((mask & SHIFT_P) && (mask & CTRL_P)))
	    if (!keyboard_map_all)
	      return c;

	  code &= 0xff;
	  if (mask & ALT_P && code <= 10 && code > 0 && dos_keypad_mode & 0x200)
	    mask |= SHIFT_P;	/* ALT-1 => M-! etc. */
	  
	  if (mask & SHIFT_P)
	    {
	      code = keyboard->shifted[code];
	      mask -= SHIFT_P;
	      modifiers &= ~shift_modifier;
	    }
	  else
	    if ((mask & ALT_GR_P) && keyboard->alt_gr && keyboard->alt_gr[code] != ' ')
	      code = keyboard->alt_gr[code];
	    else
	      code = keyboard->unshifted[code];
	  break;

	case KeyPad:
	  code &= 0xff;
	  if (c == 0xe0)	/* edit key */
	    kp_mode = 3;
	  else
	    if ((mask & (NUMLOCK_P|CTRL_P|SHIFT_P|ALT_P)) == NUMLOCK_P) /* numlock on */
	      kp_mode = dos_keypad_mode & 0x03;
	    else
	      kp_mode = (dos_keypad_mode >> 4) & 0x03;
	  
	  switch (kp_mode)
	    {
	    case 0:
	      if (code == 10 && dos_decimal_point)
		return dos_decimal_point;
	      return keypad_translate_map[code].char_code;

	    case 1:
	      code = 0xff00 | keypad_translate_map[code].keypad_code;
	      break;

	    case 2:
	      code = keypad_translate_map[code].meta_code;
	      modifiers = meta_modifier;
	      break;
	      
	    case 3:
	      code = 0xff00 | keypad_translate_map[code].editkey_code;
	      break;
	    }
	  break;
	  
	case Grey:
	  code &= 0xff;
	  kp_mode = ((mask & (NUMLOCK_P|CTRL_P|SHIFT_P|ALT_P)) == NUMLOCK_P) ? 0x04 : 0x40;
	  if (dos_keypad_mode & kp_mode)
	    code = 0xff00 | grey_key_translate_map[code].keypad_code;
	  else
	    code = grey_key_translate_map[code].char_code;
	  break;
	}
      
    make_event:
      if (code == 0)
	continue;
      
      if (code >= 0x100)
	event.kind = non_ascii_keystroke;
      else
	event.kind = ascii_keystroke;
      event.code = code;
      event.modifiers =	modifiers;
      XSETFRAME (event.frame_or_window, selected_frame);
      event.timestamp = event_timestamp ();
      kbd_buffer_store_event (&event);
    }

  if (have_mouse > 0)
    {
      int but, press, x, y, ok;

      /* Check for mouse movement *before* buttons.  */
      mouse_check_moved ();

      for (but = 0; but < NUM_MOUSE_BUTTONS; but++)
	for (press = 0; press < 2; press++)
	  {
	    int button_num = but;

	    if (press)
	      ok = mouse_pressed (but, &x, &y);
	    else
	      ok = mouse_released (but, &x, &y);
	    if (ok)
	      {
		/* Allow a simultaneous press/release of Mouse-1 and
		   Mouse-2 to simulate Mouse-3 on two-button mice.  */
		if (mouse_button_count == 2 && but < 2)
		  {
		    int x2, y2;	/* don't clobber original coordinates */

		    /* If only one button is pressed, wait 100 msec and
		       check again.  This way, Speedy Gonzales isn't
		       punished, while the slow get their chance.  */
		    if (press && mouse_pressed (1-but, &x2, &y2)
			|| !press && mouse_released (1-but, &x2, &y2))
		      button_num = 2;
		    else
		      {
			delay (100);
			if (press && mouse_pressed (1-but, &x2, &y2)
			    || !press && mouse_released (1-but, &x2, &y2))
			  button_num = 2;
		      }
		  }

		event.kind = mouse_click;
		event.code = button_num;
		event.modifiers = dos_get_modifiers (0)
		  | (press ? down_modifier : up_modifier);
		event.x = x;
		event.y = y;
		XSETFRAME (event.frame_or_window, selected_frame);
		event.timestamp = event_timestamp ();
		kbd_buffer_store_event (&event);
	      }
	  }
    }

  return -1;
}

static int prev_get_char = -1;

/* Return 1 if a key is ready to be read without suspending execution.  */

dos_keysns ()
{
  if (prev_get_char != -1)
    return 1;
  else
    return ((prev_get_char = dos_rawgetc ()) != -1);
}

/* Read a key.  Return -1 if no key is ready.  */

dos_keyread ()
{
  if (prev_get_char != -1)
    {
      int c = prev_get_char;
      prev_get_char = -1;
      return c;
    }
  else
    return dos_rawgetc ();
}

#ifndef HAVE_X_WINDOWS
/* See xterm.c for more info.  */
void
pixel_to_glyph_coords (f, pix_x, pix_y, x, y, bounds, noclip)
     FRAME_PTR f;
     register int pix_x, pix_y;
     register int *x, *y;
     void /* XRectangle */ *bounds;
     int noclip;
{
  if (bounds) abort ();

  /* Ignore clipping.  */

  *x = pix_x;
  *y = pix_y;
}

void
glyph_to_pixel_coords (f, x, y, pix_x, pix_y)
     FRAME_PTR f;
     register int x, y;
     register int *pix_x, *pix_y;
{
  *pix_x = x;
  *pix_y = y;
}

/* Simulation of X's menus.  Nothing too fancy here -- just make it work
   for now.

   Actually, I don't know the meaning of all the parameters of the functions
   here -- I only know how they are called by xmenu.c.  I could of course
   grab the nearest Xlib manual (down the hall, second-to-last door on the
   left), but I don't think it's worth the effort.  */

static XMenu *
IT_menu_create ()
{
  XMenu *menu;

  menu = (XMenu *) xmalloc (sizeof (XMenu));
  menu->allocated = menu->count = menu->panecount = menu->width = 0;
  return menu;
}

/* Allocate some (more) memory for MENU ensuring that there is room for one
   for item.  */

static void
IT_menu_make_room (XMenu *menu)
{
  if (menu->allocated == 0)
    {
      int count = menu->allocated = 10;
      menu->text = (char **) xmalloc (count * sizeof (char *));
      menu->submenu = (XMenu **) xmalloc (count * sizeof (XMenu *));
      menu->panenumber = (int *) xmalloc (count * sizeof (int));
    }
  else if (menu->allocated == menu->count)
    {
      int count = menu->allocated = menu->allocated + 10;
      menu->text
	= (char **) xrealloc (menu->text, count * sizeof (char *));
      menu->submenu
	= (XMenu **) xrealloc (menu->submenu, count * sizeof (XMenu *));
      menu->panenumber
	= (int *) xrealloc (menu->panenumber, count * sizeof (int));
    }
}

/* Search the given menu structure for a given pane number.  */

static XMenu *
IT_menu_search_pane (XMenu *menu, int pane)
{
  int i;
  XMenu *try;

  for (i = 0; i < menu->count; i++)
    if (menu->submenu[i])
      {
	if (pane == menu->panenumber[i])
	  return menu->submenu[i];
	if ((try = IT_menu_search_pane (menu->submenu[i], pane)))
	  return try;
      }
  return (XMenu *) 0;
}

/* Determine how much screen space a given menu needs.  */

static void
IT_menu_calc_size (XMenu *menu, int *width, int *height)
{
  int i, h2, w2, maxsubwidth, maxheight;

  maxsubwidth = 0;
  maxheight = menu->count;
  for (i = 0; i < menu->count; i++)
    {
      if (menu->submenu[i])
	{
	  IT_menu_calc_size (menu->submenu[i], &w2, &h2);
	  if (w2 > maxsubwidth) maxsubwidth = w2;
	  if (i + h2 > maxheight) maxheight = i + h2;
	}
    }
  *width = menu->width + maxsubwidth;
  *height = maxheight;
}

/* Display MENU at (X,Y) using FACES.  */

static void
IT_menu_display (XMenu *menu, int y, int x, int *faces)
{
  int i, j, face, width;
  GLYPH *text, *p;
  char *q;
  int mx, my;
  int enabled, mousehere;
  int row, col;

  width = menu->width;
  text = (GLYPH *) xmalloc ((width + 2) * sizeof (GLYPH));
  ScreenGetCursor (&row, &col);
  mouse_get_xy (&mx, &my);
  IT_update_begin ();
  for (i = 0; i < menu->count; i++)
    {
      IT_cursor_to (y + i, x);
      enabled
	= (!menu->submenu[i] && menu->panenumber[i]) || (menu->submenu[i]);
      mousehere = (y + i == my && x <= mx && mx < x + width + 2);
      face = faces[enabled + mousehere * 2];
      p = text;
      *p++ = FAST_MAKE_GLYPH (' ', face);
      for (j = 0, q = menu->text[i]; *q; j++)
	{
	  if (*q > 26)
	    *p++ = FAST_MAKE_GLYPH (*q++, face);
	  else	/* make '^x' */
	    {
	      *p++ = FAST_MAKE_GLYPH ('^', face);
	      j++;
	      *p++ = FAST_MAKE_GLYPH (*q++ + 64, face);
	    }
	}
	    
      for (; j < width; j++)
	*p++ = FAST_MAKE_GLYPH (' ', face);
      *p++ = FAST_MAKE_GLYPH (menu->submenu[i] ? 16 : ' ', face);
      IT_write_glyphs (text, width + 2);
    }
  IT_update_end ();
  IT_cursor_to (row, col);
  xfree (text);
}

/* --------------------------- X Menu emulation ---------------------- */

/* Report availability of menus.  */

int
have_menus_p ()
{
  return 1;
}

/* Create a brand new menu structure.  */

XMenu *
XMenuCreate (Display *foo1, Window foo2, char *foo3)
{
  return IT_menu_create ();
}

/* Create a new pane and place it on the outer-most level.  It is not
   clear that it should be placed out there, but I don't know what else
   to do.  */

int
XMenuAddPane (Display *foo, XMenu *menu, char *txt, int enable)
{
  int len;
  char *p;

  if (!enable)
    abort ();

  IT_menu_make_room (menu);
  menu->submenu[menu->count] = IT_menu_create ();
  menu->text[menu->count] = txt;
  menu->panenumber[menu->count] = ++menu->panecount;
  menu->count++;

  /* Adjust length for possible control characters (which will
     be written as ^x).  */
  for (len = strlen (txt), p = txt; *p; p++)
    if (*p < 27)
      len++;

  if (len > menu->width)
    menu->width = len;

  return menu->panecount;
}

/* Create a new item in a menu pane.  */

int
XMenuAddSelection (Display *bar, XMenu *menu, int pane,
		   int foo, char *txt, int enable)
{
  int len;
  char *p;

  if (pane)
    if (!(menu = IT_menu_search_pane (menu, pane)))
      return XM_FAILURE;
  IT_menu_make_room (menu);
  menu->submenu[menu->count] = (XMenu *) 0;
  menu->text[menu->count] = txt;
  menu->panenumber[menu->count] = enable;
  menu->count++;

  /* Adjust length for possible control characters (which will
     be written as ^x).  */
  for (len = strlen (txt), p = txt; *p; p++)
    if (*p < 27)
      len++;

  if (len > menu->width)
    menu->width = len;

  return XM_SUCCESS;
}

/* Decide where the menu would be placed if requested at (X,Y).  */

void
XMenuLocate (Display *foo0, XMenu *menu, int foo1, int foo2, int x, int y,
	     int *ulx, int *uly, int *width, int *height)
{
  IT_menu_calc_size (menu, width, height);
  *ulx = x + 1;
  *uly = y;
  *width += 2;
}

struct IT_menu_state
{
  void *screen_behind;
  XMenu *menu;
  int pane;
  int x, y;
};


/* Display menu, wait for user's response, and return that response.  */

int
XMenuActivate (Display *foo, XMenu *menu, int *pane, int *selidx,
	       int x0, int y0, unsigned ButtonMask, char **txt)
{
  struct IT_menu_state *state;
  int statecount;
  int x, y, i, b;
  int screensize;
  int faces[4], selectface;
  int leave, result, onepane;
  int title_faces[4];		/* face to display the menu title */
  int buffers_num_deleted = 0;

  /* Just in case we got here without a mouse present...  */
  if (have_mouse <= 0)
    return XM_IA_SELECT;

  state = alloca (menu->panecount * sizeof (struct IT_menu_state));
  screensize = screen_size * 2;
  faces[0]
    = compute_glyph_face (selected_frame,
			  face_name_id_number
			  (selected_frame,
			   intern ("msdos-menu-passive-face")),
			  0);
  faces[1]
    = compute_glyph_face (selected_frame,
			  face_name_id_number
			  (selected_frame,
			   intern ("msdos-menu-active-face")),
			  0);
  selectface
    = face_name_id_number (selected_frame, intern ("msdos-menu-select-face"));
  faces[2] = compute_glyph_face (selected_frame, selectface, faces[0]);
  faces[3] = compute_glyph_face (selected_frame, selectface, faces[1]);

  /* Make sure the menu title is always displayed with
     `msdos-menu-active-face', no matter where the mouse pointer is.  */
  for (i = 0; i < 4; i++)
    title_faces[i] = faces[3];

  statecount = 1;

  /* Don't let the title for the "Buffers" popup menu include a
     digit (which is ugly).
     
     This is a terrible kludge, but I think the "Buffers" case is
     the only one where the title includes a number, so it doesn't
     seem to be necessary to make this more general.  */
  if (strncmp (menu->text[0], "Buffers 1", 9) == 0)
    {
      menu->text[0][7] = '\0';
      buffers_num_deleted = 1;
    }
  state[0].menu = menu;
  mouse_off ();
  ScreenRetrieve (state[0].screen_behind = xmalloc (screensize));

  IT_menu_display (menu, y0 - 1, x0 - 1, title_faces); /* display menu title */
  if (buffers_num_deleted)
    menu->text[0][7] = ' ';
  if ((onepane = menu->count == 1 && menu->submenu[0]))
    {
      menu->width = menu->submenu[0]->width;
      state[0].menu = menu->submenu[0];
    }
  else
    {
      state[0].menu = menu;
    }
  state[0].x = x0 - 1;
  state[0].y = y0;
  state[0].pane = onepane;

  mouse_last_x = -1;  /* A hack that forces display.  */
  leave = 0;
  while (!leave)
    {
      if (!mouse_visible) mouse_on ();
      mouse_check_moved ();
      if (selected_frame->mouse_moved)
	{
	  selected_frame->mouse_moved = 0;
	  result = XM_IA_SELECT;
	  mouse_get_xy (&x, &y);
	  for (i = 0; i < statecount; i++)
	    if (state[i].x <= x && x < state[i].x + state[i].menu->width + 2)
	      {
		int dy = y - state[i].y;
		if (0 <= dy && dy < state[i].menu->count)
		  {
		    if (!state[i].menu->submenu[dy])
		      if (state[i].menu->panenumber[dy])
			result = XM_SUCCESS;
		      else
			result = XM_IA_SELECT;
		    *pane = state[i].pane - 1;
		    *selidx = dy;
		    /* We hit some part of a menu, so drop extra menus that
		       have been opened.  That does not include an open and
		       active submenu.  */
		    if (i != statecount - 2
			|| state[i].menu->submenu[dy] != state[i+1].menu)
		      while (i != statecount - 1)
			{
			  statecount--;
			  mouse_off ();
			  ScreenUpdate (state[statecount].screen_behind);
			  xfree (state[statecount].screen_behind);
			}
		    if (i == statecount - 1 && state[i].menu->submenu[dy])
		      {
			IT_menu_display (state[i].menu,
					 state[i].y,
					 state[i].x,
					 faces);
			state[statecount].menu = state[i].menu->submenu[dy];
			state[statecount].pane = state[i].menu->panenumber[dy];
			mouse_off ();
			ScreenRetrieve (state[statecount].screen_behind
					= xmalloc (screensize));
			state[statecount].x
			  = state[i].x + state[i].menu->width + 2;
			state[statecount].y = y;
			statecount++;			  
		      }
		  }
	      }
	  IT_menu_display (state[statecount - 1].menu,
			   state[statecount - 1].y,
			   state[statecount - 1].x,
			   faces);
	}
      for (b = 0; b < mouse_button_count; b++)
	{
	  (void) mouse_pressed (b, &x, &y);
	  if (mouse_released (b, &x, &y))
	    leave = 1;
	}
    }

  mouse_off ();
  ScreenUpdate (state[0].screen_behind);
  while (statecount--)
    xfree (state[statecount].screen_behind);
  return result;
}

/* Dispose of a menu.  */

void
XMenuDestroy (Display *foo, XMenu *menu)
{
  int i;
  if (menu->allocated)
    {
      for (i = 0; i < menu->count; i++)
	if (menu->submenu[i])
	  XMenuDestroy (foo, menu->submenu[i]);
      xfree (menu->text);
      xfree (menu->submenu);
      xfree (menu->panenumber);
    }
  xfree (menu);
}

int
x_pixel_width (struct frame *f)
{
  return FRAME_WIDTH (f);
}

int
x_pixel_height (struct frame *f)
{
  return FRAME_HEIGHT (f);
}
#endif /* !HAVE_X_WINDOWS */

/* ----------------------- DOS / UNIX conversion --------------------- */

void msdos_downcase_filename (unsigned char *);

/* Destructively turn backslashes into slashes.  */

void
dostounix_filename (p)
     register char *p;
{
  msdos_downcase_filename (p);

  while (*p)
    {
      if (*p == '\\')
	*p = '/';
      p++;
    }
}

/* Destructively turn slashes into backslashes.  */

void
unixtodos_filename (p)
     register char *p;
{
  if (p[1] == ':' && *p >= 'A' && *p <= 'Z')
    {
      *p += 'a' - 'A';
      p += 2;
    }

  while (*p)
    {
      if (*p == '/')
	*p = '\\';
      p++;
    }
}

/* Get the default directory for a given drive.  0=def, 1=A, 2=B, ...  */

int
getdefdir (drive, dst)
     int drive;
     char *dst;
{
  char in_path[4], *p = in_path;
  int e = errno;

  /* Generate "X:." (when drive is X) or "." (when drive is 0).  */
  if (drive != 0)
    {
      *p++ = drive + 'A' - 1;
      *p++ = ':';
    }

  *p++ = '.';
  *p = '\0';
  errno = 0;
  _fixpath (in_path, dst);
  if (errno)
    return 0;

  msdos_downcase_filename (dst);

  errno = e;
  return 1;
}

/* Remove all CR's that are followed by a LF.  */

int
crlf_to_lf (n, buf)
     register int n;
     register unsigned char *buf;
{
  unsigned char *np = buf;
  unsigned char *startp = buf;
  unsigned char *endp = buf + n;
  unsigned char c;

  if (n == 0)
    return n;
  while (buf < endp - 1)
    {
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
  if (buf < endp)
    *np++ = *buf++;
  return np - startp;
}

#if defined(__DJGPP__) && __DJGPP__ == 2 && __DJGPP_MINOR__ == 0

/* In DJGPP v2.0, library `write' can call `malloc', which might
   cause relocation of the buffer whose address we get in ADDR.
   Here is a version of `write' that avoids calling `malloc',
   to serve us until such time as the library is fixed.
   Actually, what we define here is called `__write', because
   `write' is a stub that just jmp's to `__write' (to be
   POSIXLY-correct with respect to the global name-space).  */

#include <io.h>		      /* for _write */
#include <libc/dosio.h>       /* for __file_handle_modes[] */

static char xbuf[64 * 1024];  /* DOS cannot write more in one chunk */

#define XBUF_END (xbuf + sizeof (xbuf) - 1)

int
__write (int handle, const void *buffer, size_t count)
{
  if (count == 0)
    return 0;

  if(__file_handle_modes[handle] & O_BINARY)
    return _write (handle, buffer, count);
  else
    {
      char *xbp = xbuf;
      const char *bp = buffer;
      int total_written = 0;
      int nmoved = 0, ncr = 0;

      while (count)
	{
	  /* The next test makes sure there's space for at least 2 more
	     characters in xbuf[], so both CR and LF can be put there.  */
	  if (xbp < XBUF_END)
	    {
	      if (*bp == '\n')
		{
		  ncr++;
		  *xbp++ = '\r';
		}
	      *xbp++ = *bp++;
	      nmoved++;
	      count--;
	    }
	  if (xbp >= XBUF_END || !count)
	    {
	      size_t to_write = nmoved + ncr;
	      int written = _write (handle, xbuf, to_write);

	      if (written == -1)
		return -1;
	      else
		total_written += nmoved;  /* CRs aren't counted in ret value */

	      /* If some, but not all were written (disk full?), return
		 an estimate of the total written bytes not counting CRs.  */
	      if (written < to_write)
		return total_written - (to_write - written) * nmoved/to_write;

	      nmoved = 0;
	      ncr = 0;
	      xbp = xbuf;
	    }
	}
      return total_written;
    }
}

#endif /* __DJGPP__ == 2 && __DJGPP_MINOR__ == 0 */

DEFUN ("msdos-long-file-names", Fmsdos_long_file_names, Smsdos_long_file_names,
  0, 0, 0,
  "Return non-nil if long file names are supported on MSDOS.")
  ()
{
  return (_USE_LFN ? Qt : Qnil);
}

/* Convert alphabetic characters in a filename to lower-case.  */

void
msdos_downcase_filename (p)
     register unsigned char *p;
{
  /* Always lower-case drive letters a-z, even if the filesystem
     preserves case in filenames.
     This is so MSDOS filenames could be compared by string comparison
     functions that are case-sensitive.  Even case-preserving filesystems
     do not distinguish case in drive letters.  */
  if (p[1] == ':' && *p >= 'A' && *p <= 'Z')
    {
      *p += 'a' - 'A';
      p += 2;
    }

  /* Under LFN we expect to get pathnames in their true case.  */
  if (NILP (Fmsdos_long_file_names ()))
    for ( ; *p; p++)
      if (*p >= 'A' && *p <= 'Z')
	*p += 'a' - 'A';
}

DEFUN ("msdos-downcase-filename", Fmsdos_downcase_filename, Smsdos_downcase_filename,
       1, 1, 0,
  "Convert alphabetic characters in FILENAME to lower case and return that.\n\
When long filenames are supported, doesn't change FILENAME.\n\
If FILENAME is not a string, returns nil.\n\
The argument object is never altered--the value is a copy.")
  (filename)
     Lisp_Object filename;
{
  char *fname;
  Lisp_Object tem;

  if (! STRINGP (filename))
    return Qnil;

  tem = Fcopy_sequence (filename);
  msdos_downcase_filename (XSTRING (tem)->data);
  return tem;
}

/* The Emacs root directory as determined by init_environment.  */

static char emacsroot[MAXPATHLEN];

char *
rootrelativepath (rel)
     char *rel;
{
  static char result[MAXPATHLEN + 10];

  strcpy (result, emacsroot);
  strcat (result, "/");
  strcat (result, rel);
  return result;
}

/* Define a lot of environment variables if not already defined.  Don't
   remove anything unless you know what you're doing -- lots of code will
   break if one or more of these are missing.  */

void
init_environment (argc, argv, skip_args)
     int argc;
     char **argv;
     int skip_args;
{
  char *s, *t, *root;
  int len;

  /* Find our root from argv[0].  Assuming argv[0] is, say,
     "c:/emacs/bin/emacs.exe" our root will be "c:/emacs".  */
  root = alloca (MAXPATHLEN + 20);
  _fixpath (argv[0], root);
  msdos_downcase_filename (root);
  len = strlen (root);
  while (len > 0 && root[len] != '/' && root[len] != ':')
    len--;
  root[len] = '\0';
  if (len > 4 && strcmp (root + len - 4, "/bin") == 0)
    root[len - 4] = '\0';
  else
    strcpy (root, "c:/emacs");  /* Only under debuggers, I think.  */
  len = strlen (root);
  strcpy (emacsroot, root);

  /* We default HOME to our root.  */
  setenv ("HOME", root, 0);

  /* We default EMACSPATH to root + "/bin".  */
  strcpy (root + len, "/bin");
  setenv ("EMACSPATH", root, 0);

  /* I don't expect anybody to ever use other terminals so the internal
     terminal is the default.  */
  setenv ("TERM", "internal", 0);

#ifdef HAVE_X_WINDOWS
  /* Emacs expects DISPLAY to be set.  */
  setenv ("DISPLAY", "unix:0.0", 0);
#endif

  /* SHELL is a bit tricky -- COMSPEC is the closest we come, but we must
     downcase it and mirror the backslashes.  */
  s = getenv ("COMSPEC");
  if (!s) s = "c:/command.com";
  t = alloca (strlen (s) + 1);
  strcpy (t, s);
  dostounix_filename (t);
  setenv ("SHELL", t, 0);

  /* PATH is also downcased and backslashes mirrored.  */
  s = getenv ("PATH");
  if (!s) s = "";
  t = alloca (strlen (s) + 3);
  /* Current directory is always considered part of MsDos's path but it is
     not normally mentioned.  Now it is.  */
  strcat (strcpy (t, ".;"), s);
  dostounix_filename (t); /* Not a single file name, but this should work.  */
  setenv ("PATH", t, 1);

  /* In some sense all dos users have root privileges, so...  */
  setenv ("USER", "root", 0);
  setenv ("NAME", getenv ("USER"), 0);

  /* Time zone determined from country code.  To make this possible, the
     country code may not span more than one time zone.  In other words,
     in the USA, you lose.  */
  if (!getenv ("TZ"))
    switch (dos_country_code)
      {
      case 31:			/* Belgium */
      case 32:			/* The Netherlands */
      case 33:			/* France */
      case 34:			/* Spain */
      case 36:			/* Hungary */
      case 38:			/* Yugoslavia (or what's left of it?) */
      case 39:			/* Italy */
      case 41:			/* Switzerland */
      case 42:			/* Tjekia */
      case 45:			/* Denmark */
      case 46:			/* Sweden */
      case 47:			/* Norway */
      case 48:			/* Poland */
      case 49:			/* Germany */
	/* Daylight saving from last Sunday in March to last Sunday in
	   September, both at 2AM.  */
	setenv ("TZ", "MET-01METDST-02,M3.5.0/02:00,M9.5.0/02:00", 0);
	break;
      case 44:			/* United Kingdom */
      case 351:			/* Portugal */
      case 354:			/* Iceland */
	setenv ("TZ", "GMT+00", 0);
	break;
      case 81:			/* Japan */
      case 82:			/* Korea */
	setenv ("TZ", "JST-09", 0);
	break;
      case 90:			/* Turkey */
      case 358:			/* Finland */
	setenv ("TZ", "EET-02", 0);
	break;
      case 972:			/* Israel */
	/* This is an approximation.  (For exact rules, use the
	   `zoneinfo/israel' file which comes with DJGPP, but you need
	   to install it in `/usr/share/zoneinfo/' directory first.)  */
	setenv ("TZ", "IST-02IDT-03,M4.1.6/00:00,M9.5.6/01:00", 0);
	break;
      }
  tzset ();
}



static int break_stat;	 /* BREAK check mode status.	*/
static int stdin_stat;	 /* stdin IOCTL status.		*/

#if __DJGPP__ < 2

/* These must be global.  */
static _go32_dpmi_seginfo ctrl_break_vector;
static _go32_dpmi_registers ctrl_break_regs;
static int ctrlbreakinstalled = 0;

/* Interrupt level detection of Ctrl-Break.  Don't do anything fancy here!  */

void
ctrl_break_func (regs)
     _go32_dpmi_registers *regs;
{
  Vquit_flag = Qt;
}

void
install_ctrl_break_check ()
{
  if (!ctrlbreakinstalled)
    {
      /* Don't press Ctrl-Break if you don't have either DPMI or Emacs
	 was compiler with Djgpp 1.11 maintenance level 5 or later!  */
      ctrlbreakinstalled = 1;
      ctrl_break_vector.pm_offset = (int) ctrl_break_func;
      _go32_dpmi_allocate_real_mode_callback_iret (&ctrl_break_vector,
						   &ctrl_break_regs);
      _go32_dpmi_set_real_mode_interrupt_vector (0x1b, &ctrl_break_vector);
    }
}

#endif /* __DJGPP__ < 2 */

/* Turn off Dos' Ctrl-C checking and inhibit interpretation of
   control chars by DOS.   Determine the keyboard type.  */

int
dos_ttraw ()
{
  union REGS inregs, outregs;
  static int first_time = 1;
  
  break_stat = getcbrk ();
  setcbrk (0);
#if __DJGPP__ < 2
  install_ctrl_break_check ();
#endif

  if (first_time)
    {
      inregs.h.ah = 0xc0;
      int86 (0x15, &inregs, &outregs);
      extended_kbd = (!outregs.x.cflag) && (outregs.h.ah == 0);
  
      have_mouse = 0;

      if (internal_terminal
#ifdef HAVE_X_WINDOWS
	  && inhibit_window_system
#endif
	  )
	{
	  inregs.x.ax = 0x0021;
	  int86 (0x33, &inregs, &outregs);
	  have_mouse = (outregs.x.ax & 0xffff) == 0xffff;
	  if (!have_mouse)
	    {
	      /* Reportedly, the above doesn't work for some mouse drivers.  There
		 is an additional detection method that should work, but might be
		 a little slower.  Use that as an alternative.  */
	      inregs.x.ax = 0x0000;
	      int86 (0x33, &inregs, &outregs);
	      have_mouse = (outregs.x.ax & 0xffff) == 0xffff;
	    }

	  if (have_mouse)
	    {
	      have_mouse = 1;	/* enable mouse */
	      mouse_visible = 0;
	      
	      if (outregs.x.bx == 3)
		{
		  mouse_button_count = 3;
		  mouse_button_translate[0] = 0; /* Left */
		  mouse_button_translate[1] = 2; /* Middle */
		  mouse_button_translate[2] = 1; /* Right */
		}
	      else
		{
		  mouse_button_count = 2;
		  mouse_button_translate[0] = 0;
		  mouse_button_translate[1] = 1;
		}
	      mouse_position_hook = &mouse_get_pos;
	      mouse_init ();
	    }
	}

      first_time = 0;

#if __DJGPP__ >= 2

      stdin_stat = setmode (fileno (stdin), O_BINARY);
      return (stdin_stat != -1);
    }
  else
    return (setmode (fileno (stdin), O_BINARY) != -1);

#else /* __DJGPP__ < 2 */

    }

  /* I think it is wrong to overwrite `stdin_stat' every time
     but the first one this function is called, but I don't
     want to change the way it used to work in v1.x.--EZ  */

  inregs.x.ax = 0x4400;		/* Get IOCTL status. */
  inregs.x.bx = 0x00;		/* 0 = stdin. */
  intdos (&inregs, &outregs);
  stdin_stat = outregs.h.dl;

  inregs.x.dx = stdin_stat | 0x0020; /* raw mode */
  inregs.x.ax = 0x4401;		/* Set IOCTL status */
  intdos (&inregs, &outregs);
  return !outregs.x.cflag;

#endif /* __DJGPP__ < 2 */
}

/*  Restore status of standard input and Ctrl-C checking.  */

int
dos_ttcooked ()
{
  union REGS inregs, outregs;

  setcbrk (break_stat);
  mouse_off ();

#if __DJGPP__ >= 2

  return (setmode (fileno (stdin), stdin_stat) != -1);

#else  /* not __DJGPP__ >= 2 */

  inregs.x.ax = 0x4401;	/* Set IOCTL status.	*/
  inregs.x.bx = 0x00;	/* 0 = stdin.		*/
  inregs.x.dx = stdin_stat;
  intdos (&inregs, &outregs);
  return !outregs.x.cflag;

#endif /* not __DJGPP__ >= 2 */
}


/* Run command as specified by ARGV in directory DIR.
   The command is run with input from TEMPIN, output to
   file TEMPOUT and stderr to TEMPERR.  */

int
run_msdos_command (argv, dir, tempin, tempout, temperr)
     unsigned char **argv;
     Lisp_Object dir;
     int tempin, tempout, temperr;
{
  char *saveargv1, *saveargv2, **envv;
  char oldwd[MAXPATHLEN + 1]; /* Fixed size is safe on MSDOS.  */
  int msshell, result = -1;
  int in, out, inbak, outbak, errbak;
  int x, y;
  Lisp_Object cmd;

  /* Get current directory as MSDOS cwd is not per-process.  */
  getwd (oldwd);

  cmd = Ffile_name_nondirectory (build_string (argv[0]));
  msshell = !NILP (Fmember (cmd, Fsymbol_value (intern ("msdos-shells"))))
    && !strcmp ("-c", argv[1]);
  if (msshell)
    {
      saveargv1 = argv[1];
      saveargv2 = argv[2];
      argv[1] = "/c";
      if (argv[2])
	{
	  char *p = alloca (strlen (argv[2]) + 1);

	  strcpy (argv[2] = p, saveargv2);
	  while (*p && isspace (*p))
	    p++;
	  while (*p && !isspace (*p))
	    if (*p == '/')
	      *p++ = '\\';
	    else
	      p++;
	}
    }

  /* Build the environment array.  */
  {
    extern Lisp_Object Vprocess_environment;
    Lisp_Object tmp, lst;
    int i, len;

    lst = Vprocess_environment;
    len = XFASTINT (Flength (lst));

    envv = alloca ((len + 1) * sizeof (char *));
    for (i = 0; i < len; i++)
      {
	tmp = Fcar (lst);
	lst = Fcdr (lst);
	CHECK_STRING (tmp, 0);
	envv[i] = alloca (XSTRING (tmp)->size + 1);
	strcpy (envv[i], XSTRING (tmp)->data);
      }
    envv[len] = (char *) 0;
  }

  if (STRINGP (dir))
    chdir (XSTRING (dir)->data);
  inbak = dup (0);
  outbak = dup (1);
  errbak = dup (2);
  if (inbak < 0 || outbak < 0 || errbak < 0)
    goto done; /* Allocation might fail due to lack of descriptors.  */

  if (have_mouse > 0)
    mouse_get_xy (&x, &y);

  dos_ttcooked ();	/* do it here while 0 = stdin */
  
  dup2 (tempin, 0);
  dup2 (tempout, 1);
  dup2 (temperr, 2);

#if __DJGPP__ > 1

  if (msshell && !argv[3])
    {
      /* MS-DOS native shells are too restrictive.  For starters, they
	 cannot grok commands longer than 126 characters.  In DJGPP v2
	 and later, `system' is much smarter, so we'll call it instead.  */

      extern char **environ;
      environ = envv;

      /* A shell gets a single argument--its full command
	 line--whose original was saved in `saveargv2'.  */
      result = system (saveargv2);
    }
  else

#endif /* __DJGPP__ > 1 */

  result = spawnve (P_WAIT, argv[0], argv, envv);
  
  dup2 (inbak, 0);
  dup2 (outbak, 1);
  dup2 (errbak, 2);
  close (inbak);
  close (outbak);
  close (errbak);

  dos_ttraw ();
  if (have_mouse > 0)
    {
      mouse_init ();
      mouse_moveto (x, y);
    }
  
 done:
  chdir (oldwd);
  if (msshell)
    {
      argv[1] = saveargv1;
      argv[2] = saveargv2;
    }
  return result;
}

croak (badfunc)
     char *badfunc;
{
  fprintf (stderr, "%s not yet implemented\r\n", badfunc);
  reset_sys_modes ();
  exit (1);
}

#if __DJGPP__ < 2

/* ------------------------- Compatibility functions -------------------
 *	gethostname
 *	gettimeofday
 */

/* Hostnames for a pc are not really funny,
   but they are used in change log so we emulate the best we can.  */

gethostname (p, size)
     char *p;
     int size;
{
  char *q = egetenv ("HOSTNAME");

  if (!q) q = "pc";
  strcpy (p, q);
  return 0;
}

/* When time zones are set from Ms-Dos too many C-libraries are playing
   tricks with time values.  We solve this by defining our own version
   of `gettimeofday' bypassing GO32.  Our version needs to be initialized
   once and after each call to `tzset' with TZ changed.  That is 
   accomplished by aliasing tzset to init_gettimeofday. */

static struct tm time_rec;

int
gettimeofday (struct timeval *tp, struct timezone *tzp)
{
  if (tp)
    {
      struct time t;
      struct tm tm;
      
      gettime (&t);
      if (t.ti_hour < time_rec.tm_hour) /* midnight wrap */
	{
	  struct date d;
	  getdate (&d);
	  time_rec.tm_year = d.da_year - 1900;
	  time_rec.tm_mon = d.da_mon - 1;
	  time_rec.tm_mday = d.da_day;
	}
      
      time_rec.tm_hour = t.ti_hour;
      time_rec.tm_min = t.ti_min;
      time_rec.tm_sec = t.ti_sec;

      tm = time_rec;
      tm.tm_gmtoff = dos_timezone_offset;
      
      tp->tv_sec = mktime (&tm);	/* may modify tm */
      tp->tv_usec = t.ti_hund * (1000000 / 100);
    }
  /* Ignore tzp; it's obsolescent.  */
  return 0;
}

#endif /* __DJGPP__ < 2 */

/*
 * A list of unimplemented functions that we silently ignore.
 */

#if __DJGPP__ < 2
unsigned alarm (s) unsigned s; {}
fork () { return 0; }
int kill (x, y) int x, y; { return -1; }
nice (p) int p; {}
void volatile pause () {}
sigsetmask (x) int x; { return 0; }
#endif

request_sigio () {}
setpgrp () {return 0; }
setpriority (x,y,z) int x,y,z; { return 0; }
sigblock (mask) int mask; { return 0; } 
unrequest_sigio () {}

#ifndef HAVE_SELECT
#include "sysselect.h"

#ifndef EMACS_TIME_ZERO_OR_NEG_P
#define EMACS_TIME_ZERO_OR_NEG_P(time)	\
  ((long)(time).tv_sec < 0		\
   || ((time).tv_sec == 0		\
       && (long)(time).tv_usec <= 0))
#endif


/* Only event queue is checked.  */
/* We don't have to call timer_check here
   because wait_reading_process_input takes care of that.  */
int
sys_select (nfds, rfds, wfds, efds, timeout)
     int nfds;
     SELECT_TYPE *rfds, *wfds, *efds;
     EMACS_TIME *timeout;
{
  int check_input;
  struct time t;

  check_input = 0;
  if (rfds)
    {
      check_input = FD_ISSET (0, rfds);
      FD_ZERO (rfds);
    }
  if (wfds)
    FD_ZERO (wfds);
  if (efds)
    FD_ZERO (efds);

  if (nfds != 1)
    abort ();
  
  /* If we are looking only for the terminal, with no timeout,
     just read it and wait -- that's more efficient.  */
  if (!timeout)
    {
      while (!detect_input_pending ())
	{
#if __DJGPP__ >= 2
	  __dpmi_yield ();
#endif	  
	}
    }
  else
    {
      EMACS_TIME clnow, cllast, cldiff;

      gettime (&t);
      EMACS_SET_SECS_USECS (cllast, t.ti_sec, t.ti_hund * 10000L);

      while (!check_input || !detect_input_pending ())
	{
	  gettime (&t);
	  EMACS_SET_SECS_USECS (clnow, t.ti_sec, t.ti_hund * 10000L);
	  EMACS_SUB_TIME (cldiff, clnow, cllast);

	  /* When seconds wrap around, we assume that no more than
	     1 minute passed since last `gettime'.  */
	  if (EMACS_TIME_NEG_P (cldiff))
	    EMACS_SET_SECS (cldiff, EMACS_SECS (cldiff) + 60);
	  EMACS_SUB_TIME (*timeout, *timeout, cldiff);

	  /* Stop when timeout value crosses zero.  */
	  if (EMACS_TIME_ZERO_OR_NEG_P (*timeout))
	    return 0;
	  cllast = clnow;
#if __DJGPP__ >= 2
	  __dpmi_yield ();
#endif	  
	}
    }
  
  FD_SET (0, rfds);
  return 1;
}
#endif

/*
 * Define overlaid functions:
 *
 *	chdir -> sys_chdir
 *	tzset -> init_gettimeofday
 *	abort -> dos_abort
 */

#ifdef chdir
#undef chdir
extern int chdir ();

int
sys_chdir (path)
     const char* path;
{
  int len = strlen (path);
  char *tmp = (char *)path;

  if (*tmp && tmp[1] == ':')
    {
      if (getdisk () != tolower (tmp[0]) - 'a')
	setdisk (tolower (tmp[0]) - 'a');
      tmp += 2;	/* strip drive: KFS 1995-07-06 */
      len -= 2;
    }
  
  if (len > 1 && (tmp[len - 1] == '/'))
    {
      char *tmp1 = (char *) alloca (len + 1);
      strcpy (tmp1, tmp);
      tmp1[len - 1] = 0;
      tmp = tmp1;
    }
  return chdir (tmp);
}
#endif

#ifdef tzset
#undef tzset
extern void tzset (void);

void
init_gettimeofday ()
{
  time_t ltm, gtm;
  struct tm *lstm;

  tzset ();
  ltm = gtm = time (NULL);
  ltm = mktime (lstm = localtime (&ltm));
  gtm = mktime (gmtime (&gtm));
  time_rec.tm_hour = 99;	/* force gettimeofday to get date */
  time_rec.tm_isdst = lstm->tm_isdst;
  dos_timezone_offset = time_rec.tm_gmtoff = (int)(gtm - ltm) / 60;
}
#endif

#ifdef abort
#undef abort
void
dos_abort (file, line)
     char *file;
     int  line;
{
  char buffer1[200], buffer2[400];
  int i, j;
  
  sprintf (buffer1, "<EMACS FATAL ERROR IN %s LINE %d>", file, line);
  for (i = j = 0; buffer1[i]; i++) {
    buffer2[j++] = buffer1[i];
    buffer2[j++] = 0x70;
  }
  dosmemput (buffer2, j, (int)ScreenPrimary);
  ScreenSetCursor (2, 0);
  abort ();
}
#else
void
abort ()
{
  dos_ttcooked ();
  ScreenSetCursor (10, 0);
  cputs ("\r\n\nEmacs aborted!\r\n");
#if __DJGPP__ > 1
  /* Generate traceback, so we could tell whodunit.  */
  signal (SIGINT, SIG_DFL);
  __asm__ __volatile__ ("movb $0x1b,%al;call ___djgpp_hw_exception");
#endif
  exit (2);
}
#endif

syms_of_msdos ()
{
  recent_doskeys = Fmake_vector (make_number (NUM_RECENT_DOSKEYS), Qnil);
  staticpro (&recent_doskeys);

  defsubr (&Srecent_doskeys);
  defsubr (&Smsdos_long_file_names);
  defsubr (&Smsdos_downcase_filename);
}

#endif /* MSDOS */
