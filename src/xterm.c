/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1989, 1992 Free Software Foundation, Inc.

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

/* Serious problems:

   Kludge: dup2 is used to put the X-connection socket into desc # 0
   so that wait_reading_process_input will wait for it in place of
   actual terminal input.
   
*/

#include "config.h"

#ifdef HAVE_X_WINDOWS

#include "lisp.h"

/* On 4.3 this loses if it comes after xterm.h.  */
#include <signal.h>

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif

#ifdef BSD
#include <sys/ioctl.h>
#include <strings.h>
#else
#include <sys/termio.h>
#include <string.h>
#endif

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

/* We are unable to use interrupts if FIONREAD is not available,
   so flush SIGIO so we won't try.  */
#ifndef FIONREAD
#ifdef SIGIO
#undef SIGIO
#endif
#endif

#include "systime.h"

#include <fcntl.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>
#include <sys/param.h>

#include "dispextern.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#if 0
#include "sink.h"
#include "sinkmask.h"
#endif
#include "gnu.h"
#include "screen.h"
#include "disptab.h"
#include "buffer.h"

#ifdef HAVE_X11
#define XMapWindow XMapRaised		/* Raise them when mapping. */
#else
#include <X/Xkeyboard.h>
/*#include <X/Xproto.h>	*/
#endif /* HAVE_X11 */

/* For sending Meta-characters.  Do we need this? */
#define METABIT 0200

#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))

/* Nonzero means we must reprint all windows
   because 1) we received an ExposeWindow event
   or 2) we received too many ExposeRegion events to record.  */

static int expose_all_windows;

/* Nonzero means we must reprint all icon windows.  */

static int expose_all_icons;

#ifndef HAVE_X11
/* ExposeRegion events, when received, are copied into this queue
   for later processing.  */

static struct event_queue x_expose_queue;

/* ButtonPressed and ButtonReleased events, when received,
   are copied into this queue for later processing.  */

struct event_queue x_mouse_queue;
#endif

/* Nonzero after BLOCK_INPUT; prevents input events from being
   processed until later.  */

int x_input_blocked;

#if defined (SIGIO) && defined (FIONREAD)
int BLOCK_INPUT_mask;
#endif

/* Nonzero if input events came in while x_input_blocked was nonzero.
   UNBLOCK_INPUT checks for this.  */

int x_pending_input;

/* Nonzero if in redisplay ();  prevents us from calling it recursively */

int in_display;

/* The id of a bitmap used for icon windows.
   One such map is shared by all Emacs icon windows.
   This is zero if we have not yet had a need to create the bitmap.  */

static Bitmap icon_bitmap;

/* Font used for text icons.  */

static FONT_TYPE *icon_font_info;

/* Stuff for dealing with the main icon title. */

extern Lisp_Object Vcommand_line_args;
char *hostname, *x_id_name;
Lisp_Object invocation_name;

/* This is the X connection that we are using.  */

Display *x_current_display;

/* Screen being updated by update_screen.  */
/* This is set by XTupdate_begin and looked at by all the
   XT functions.  It is zero while not inside an update.
   In that case, the XT functions assume that `selected_screen'
   is the screen to apply to.  */

static struct screen *updating_screen;

/* The screen (if any) which has the X window that has keyboard focus.
   Zero if none.  This is examined by Ffocus_screen in screen.c.  */
struct screen *x_focus_screen;

/* The screen which currently has the visual highlight, and should get
   keyboard input (other sorts of input have the screen encoded in the
   event).  It points to the X focus screen's selected window's
   screen.  It differs from x_focus_screen when we're using a global
   minibuffer.  */
static struct screen *x_highlight_screen;

/* From .Xdefaults, the value of "emacs.WarpMouse".  If non-zero,
   mouse is moved to inside of screen when screen is de-iconified.  */

static int warp_mouse_on_deiconify;

/* During an update, maximum vpos for ins/del line operations to affect.  */

static int flexlines;

/* During an update, nonzero if chars output now should be highlighted.  */

static int highlight;

/* Nominal cursor position -- where to draw output.
   During an update, these are different from the cursor-box position.  */

static int curs_x;
static int curs_y;

#ifdef HAVE_X11
/* `t' if a mouse button is depressed. */

extern Lisp_Object Vmouse_depressed;

/* Tells if a window manager is present or not. */

extern Lisp_Object Vx_no_window_manager;

/* Timestamp that we requested selection data was made. */
extern Time requestor_time;

/* ID of the window requesting selection data. */
extern Window requestor_window;

/* Nonzero enables some debugging for the X interface code. */
extern int _Xdebug;

#else /* X10 stuff */

/* Bit patterns for the mouse cursor.  */

short MouseCursor[] = {
  0x0000, 0x0008, 0x0018, 0x0038,
  0x0078, 0x00f8, 0x01f8, 0x03f8,
  0x07f8, 0x00f8, 0x00d8, 0x0188,
  0x0180, 0x0300, 0x0300, 0x0000};

short MouseMask[] = {
  0x000c, 0x001c, 0x003c, 0x007c,
  0x00fc, 0x01fc, 0x03fc, 0x07fc,
  0x0ffc, 0x0ffc, 0x01fc, 0x03dc,
  0x03cc, 0x0780, 0x0780, 0x0300};

static short grey_bits[] = {
  0x0005, 0x000a, 0x0005, 0x000a};

static Pixmap GreyPixmap = 0;
#endif /* X10 stuff */

/* From time to time we get info on an Emacs window, here.  */

static WINDOWINFO_TYPE windowinfo;

extern int errno;

extern Display *XOpenDisplay ();
extern Window XCreateWindow ();

extern Cursor XCreateCursor ();
extern FONT_TYPE *XOpenFont ();

static void flashback ();

#ifndef HAVE_X11
static void dumpqueue ();
#endif

void dumpborder ();
static int XTcursor_to ();
static int XTclear_end_of_line ();

/* These hooks are called by update_screen at the beginning and end
   of a screen update.  We record in `updating_screen' the identity
   of the screen being updated, so that the XT... functions do not
   need to take a screen as argument.  Most of the XT... functions
   should never be called except during an update, the only exceptions
   being XTcursor_to, XTwrite_char and XTreassert_line_highlight.  */

extern int mouse_track_top, mouse_track_left, mouse_track_width;

static
XTupdate_begin (s)
     struct screen *s;
{	
  int mask;

  if (s == 0)
    abort ();

  updating_screen = s;
  flexlines = s->height;
  highlight = 0;

  BLOCK_INPUT;
#ifndef HAVE_X11
  dumpqueue ();
#endif
  UNBLOCK_INPUT;
}

static void x_do_pending_expose ();

static
XTupdate_end (s)
     struct screen *s;
{	
  int mask;

  if (updating_screen == 0
      || updating_screen != s)
    abort ();

  BLOCK_INPUT;
#ifndef HAVE_X11
  dumpqueue ();
#endif
  adjust_scrollbars (s);
  x_do_pending_expose ();

  x_display_cursor (s, 1);

  updating_screen = 0;
  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* External interface to control of standout mode.
   Call this when about to modify line at position VPOS
   and not change whether it is highlighted.  */

XTreassert_line_highlight (new, vpos)
     int new, vpos;
{
  highlight = new;
}

/* Call this when about to modify line at position VPOS
   and change whether it is highlighted.  */

static
XTchange_line_highlight (new_highlight, vpos, first_unused_hpos)
     int new_highlight, vpos, first_unused_hpos;
{
  highlight = new_highlight;
  XTcursor_to (vpos, 0);
  XTclear_end_of_line (updating_screen->width);
}

/* This is used when starting Emacs and when restarting after suspend.
   When starting Emacs, no X window is mapped.  And nothing must be done
   to Emacs's own window if it is suspended (though that rarely happens).  */

static
XTset_terminal_modes ()
{
}

/* This is called when exiting or suspending Emacs.
   Exiting will make the X-windows go away, and suspending
   requires no action.  */

static
XTreset_terminal_modes ()
{
/*  XTclear_screen ();  */
}

/* Set the nominal cursor position of the screen:
   where display update commands will take effect.
   This does not affect the place where the cursor-box is displayed.  */

static int
XTcursor_to (row, col)
     register int row, col;
{
  int mask;
  int orow = row;

  curs_x = col;
  curs_y = row;

  if (updating_screen == 0)
    {
      BLOCK_INPUT;
      x_display_cursor (selected_screen, 1);
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Display a sequence of N glyphs found at GP.
   WINDOW is the x-window to output to.  LEFT and TOP are starting coords.
   HL is 1 if this text is highlighted, 2 if the cursor is on it.

   FONT is the default font to use (for glyphs whose font-code is 0).  */

static void
dumpglyphs (s, left, top, gp, n, hl, font)
     struct screen *s;
     int left, top;
     register GLYPH *gp; /* Points to first GLYPH. */
     register int n;  /* Number of glyphs to display. */
     int hl;
     FONT_TYPE *font;
{
  register int len;
  Window window = s->display.x->window_desc;
  GC drawing_gc =   (hl == 2 ? s->display.x->cursor_gc
		             : (hl ? s->display.x->reverse_gc
				   : s->display.x->normal_gc));

  if (sizeof (GLYPH) == sizeof (XChar2b))
    XDrawImageString16 (x_current_display, window, drawing_gc,
			left, top + FONT_BASE (font), (XChar2b *) gp, n);
  else if (sizeof (GLYPH) == sizeof (unsigned char))
    XDrawImageString (x_current_display, window, drawing_gc,
		      left, top + FONT_BASE (font), (char *) gp, n);
  else
    /* What size of glyph ARE you using?  And does X have a function to
       draw them?  */
    abort ();
}

#if 0
static void
dumpglyphs (s, left, top, gp, n, hl, font)
     struct screen *s;
     int left, top;
     register GLYPH *gp; /* Points to first GLYPH. */
     register int n;  /* Number of glyphs to display. */
     int hl;
     FONT_TYPE *font;
{
  char buf[s->width]; /* Holds characters to be displayed. */
  register char *cp;		/* Steps through buf[]. */
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;
  Window window = s->display.x->window_desc;
  int cursor_pixel = s->display.x->cursor_pixel;
  int fg_pixel = s->display.x->foreground_pixel;
  int bg_pixel = s->display.x->background_pixel;
  int intborder = s->display.x->internal_border_width;

  while (n)
    {
      /* Get the face-code of the next GLYPH.  */
      int cf, len;
      int g = *gp;

      while (GLYPH_ALIAS_P (tbase, tlen, g))
	g = GLYPH_ALIAS (tbase, g);
	
      cf = g >> 8;

      /* Find the run of consecutive glyphs with the same face-code.
	 Extract their character codes into BUF.  */
      cp = buf;
      while (n > 0)
	{
	  g = *gp;
	  while (GLYPH_ALIAS_P (tbase, tlen, g))
	    g = GLYPH_ALIAS (tbase, g);
	  if ((g >> 8) != cf)
	    break;

	  *cp++ = 0377 & g;
	  --n;
	  ++gp;
	}

      /* LEN gets the length of the run.  */
      len = cp - buf;

      /* Now output this run of chars, with the font and pixel values
	 determined by the face code CF.  */
      if (cf == 0)
	{
#ifdef HAVE_X11
	  GC GC_cursor = s->display.x->cursor_gc;
	  GC GC_reverse = s->display.x->reverse_gc;
	  GC GC_normal = s->display.x->normal_gc;

	  XDrawImageString (x_current_display, window,
			    (hl == 2
			     ? GC_cursor
			     : (hl ? GC_reverse : GC_normal)),
			    left, top + FONT_BASE (font), buf, len);
#else
	  XText (window, left, top,
		 buf,
		 len,
		 font->id,
		 (hl == 2
		  ? (cursor_pixel == fg_pixel ? bg_pixel : fg_pixel)
		  : hl ? bg_pixel : fg_pixel),
		 (hl == 2 ? cursor_pixel
		  : hl ? fg_pixel : bg_pixel));
#endif /* HAVE_X11 */
	}
      else
	{
#ifdef HAVE_X11
	  if (FACE_IS_FONT (cf))
	    XDrawImageString (x_current_display, s->display.x->window_desc,
			      FACE_GC (cf),
			      left, top + FONT_BASE (FACE_FONT (cf)),
			      buf, len);
	  else if (FACE_IS_IMAGE (cf))
	    XCopyPlane (x_current_display, FACE_IMAGE (cf),
			s->display.x->window_desc,
			s->display.x->normal_gc,
			0, 0,
			FACE_IMAGE_WIDTH (cf),
			FACE_IMAGE_HEIGHT (cf), left, top);
	  else
	    abort ();
#else
	  register struct face *fp = x_face_table[cf];

	  XText (window, left, top,
		 buf,
		 len,
		 fp->font->id,
		 (hl == 2
		  ? (cursor_pixel == fp->fg ? fp->bg : fp->fg)
		  : hl ? fp->bg : fp->fg),
		 (hl == 2 ? cursor_pixel
		  : hl ? fp->fg : fp->bg));
#endif /* HAVE_X11 */
	}
      left += len * FONT_WIDTH (font);
    }
}
#endif

/* Output some text at the nominal screen cursor position,
   advancing the cursor over the text.
   Output LEN glyphs at START.

   `highlight', set up by XTreassert_line_highlight or XTchange_line_highlight,
   controls the pixel values used for foreground and background.  */

static
XTwrite_glyphs (start, len)
     register GLYPH *start;
     int len;
{
  register int temp_length;
  int mask;
  struct screen *s;

  BLOCK_INPUT;

  s = updating_screen;
  if (s == 0)
    {
      s = selected_screen;
      /* If not within an update,
	 output at the screen's visible cursor.  */
      curs_x = s->cursor_x;
      curs_y = s->cursor_y;
    }

  dumpglyphs (s,
	     (curs_x * FONT_WIDTH (s->display.x->font)
	      + s->display.x->internal_border_width),
	     (curs_y * FONT_HEIGHT (s->display.x->font)
	      + s->display.x->internal_border_width),
	     start, len, highlight, s->display.x->font);

  /* If we drew on top of the cursor, note that it is turned off.  */
  if (curs_y == s->phys_cursor_y
      && curs_x <= s->phys_cursor_x
      && curs_x + len > s->phys_cursor_x)
    s->phys_cursor_x = -1;
  
  if (updating_screen == 0)
    {
      s->cursor_x += len;
      x_display_cursor (s, 1);
      s->cursor_x -= len;
    }
  else
    curs_x += len;

  UNBLOCK_INPUT;
}

/* Erase the current text line from the nominal cursor position (inclusive)
   to column FIRST_UNUSED (exclusive).  The idea is that everything
   from FIRST_UNUSED onward is already erased.  */
  
static int
XTclear_end_of_line (first_unused)
     register int first_unused;
{
  struct screen *s = updating_screen;
  int mask;

  if (s == 0)
    abort ();

  if (curs_y < 0 || curs_y >= s->height)
    return;
  if (first_unused <= 0)
    return;

  if (first_unused >= s->width)
    first_unused = s->width;

  BLOCK_INPUT;

  /* Notice if the cursor will be cleared by this operation.  */
  if (curs_y == s->phys_cursor_y
      && curs_x <= s->phys_cursor_x
      && s->phys_cursor_x < first_unused)
    s->phys_cursor_x = -1;

#ifdef HAVE_X11
  XClearArea (x_current_display, s->display.x->window_desc,
	      curs_x * FONT_WIDTH (s->display.x->font)
	      + s->display.x->internal_border_width,
	      curs_y * FONT_HEIGHT (s->display.x->font)
	      + s->display.x->internal_border_width,
	      FONT_WIDTH (s->display.x->font) * (first_unused - curs_x),
	      FONT_HEIGHT (s->display.x->font), False);
	      
#else
  XPixSet (s->display.x->window_desc,
	   curs_x * FONT_WIDTH (s->display.x->font) + s->display.x->internal_border_width,
	   curs_y * FONT_HEIGHT (s->display.x->font) + s->display.x->internal_border_width,
	   FONT_WIDTH (s->display.x->font) * (first_unused - curs_x),
	   FONT_HEIGHT (s->display.x->font),
	   s->display.x->background_pixel);	
#endif /* HAVE_X11 */

  UNBLOCK_INPUT;
}

static
XTclear_screen ()
{
  int mask;
  struct screen *s = updating_screen;

  if (s == 0)
    s = selected_screen;

  s->phys_cursor_x = -1;	/* Cursor not visible.  */
  curs_x = 0;			/* Nominal cursor position is top left.  */
  curs_y = 0;
  
  BLOCK_INPUT;
  XClear (s->display.x->window_desc);
#ifndef HAVE_X11
  dumpborder (s, 0);
#endif
  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Paint horzontal bars down the screen for a visible bell.
   Note that this may be way too slow on some machines. */

XTflash (s)
     struct screen *s;
{
  register struct screen_glyphs *active_screen = SCREEN_CURRENT_GLYPHS (s);
  register int i;
  int x, y;

  if (updating_screen != 0)
    abort ();

  BLOCK_INPUT;
#ifdef HAVE_X11
#if 0
  for (i = s->height * FONT_HEIGHT (s->display.x->font) - 10;
       i >= 0;    
       i -= 100)	   /* Should be NO LOWER than 75 for speed reasons. */
    XFillRectangle (x_current_display, s->display.x->window_desc,
		    s->display.x->cursor_gc,
		    0, i, s->width * FONT_WIDTH (s->display.x->font)
		    + 2 * s->display.x->internal_border_width, 25);
#endif

  x = (s->width * FONT_WIDTH (s->display.x->font)) / 4;
  y = (s->height * FONT_HEIGHT (s->display.x->font)) / 4;
  XFillRectangle (x_current_display, s->display.x->window_desc,
		  s->display.x->cursor_gc,
		  x, y, 2 * x, 2 * y);
  dumpglyphs (s, (x + s->display.x->internal_border_width),
	     (y + s->display.x->internal_border_width),
	     &active_screen->glyphs[(s->height / 4) + 1][(s->width / 4)],
	     1, 0, s->display.x->font);

#else /* X10 */
  for (i = s->height * FONT_HEIGHT (s->display.x->font) - 10;
       i >= 0;
       i -= 50)
    XPixFill (s->display.x->window_desc, 0, i,
	      s->width * FONT_WIDTH (s->display.x->font)
	      + 2 * s->display.x->internal_border_width, 10,
	      WHITE_PIX_DEFAULT, ClipModeClipped, GXinvert, AllPlanes);
#endif /* X10 */

  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Flip background and forground colors of the screen. */

x_invert_screen (s)
     struct screen *s;
{
#ifdef HAVE_X11
  GC temp;
  unsigned long pix_temp;

  x_display_cursor (s, 0);
  XClearWindow (x_current_display, s->display.x->window_desc);
  temp = s->display.x->normal_gc;
  s->display.x->normal_gc = s->display.x->reverse_gc;
  s->display.x->reverse_gc = temp;
  pix_temp = s->display.x->foreground_pixel;
  s->display.x->foreground_pixel = s->display.x->background_pixel;
  s->display.x->background_pixel = pix_temp;

  XSetWindowBackground (x_current_display, s->display.x->window_desc,
			s->display.x->background_pixel);
  if (s->display.x->background_pixel == s->display.x->cursor_pixel)
    {
      s->display.x->cursor_pixel = s->display.x->foreground_pixel;
      XSetBackground (x_current_display, s->display.x->cursor_gc,
		      s->display.x->cursor_pixel);
      XSetForeground (x_current_display, s->display.x->cursor_gc,
		      s->display.x->background_pixel);
    }
  redraw_screen (s);
#endif /* X11 */
}

/* Make audible bell.  */

#ifdef HAVE_X11
#define XRINGBELL XBell(x_current_display, 0)
#else
#define XRINGBELL XFeep(0);
#endif

XTring_bell ()
{
  if (visible_bell)
#if 0
    XTflash (selected_screen);
#endif
    {
      x_invert_screen (selected_screen);
      x_invert_screen (selected_screen);
    }
  else
    {
      BLOCK_INPUT;
      XRINGBELL;
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Insert and delete character are not supposed to be used
   because we are supposed to turn off the feature of using them.  */

static 
XTinsert_glyphs (start, len)
     register char *start;
     register int len;
{
  abort ();
}

static 
XTdelete_glyphs (n)
     register int n;
{
  abort ();
}

/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to XTupdate_begin and XTupdate_end.  */

static
XTset_terminal_window (n)
     register int n;
{
  if (updating_screen == 0)
    abort ();

  if ((n <= 0) || (n > updating_screen->height))
    flexlines = updating_screen->height;
  else
    flexlines = n;
}

/* Perform an insert-lines operation, inserting N lines
   at a vertical position curs_y.  */

static void
stufflines (n)
     register int n;
{
  register int topregion, bottomregion;
  register int length, newtop, mask;
  register struct screen *s = updating_screen;
  int intborder = s->display.x->internal_border_width;

  if (curs_y >= flexlines)
    return;

  topregion = curs_y;
  bottomregion = flexlines - (n + 1);
  newtop = topregion + n;
  length = (bottomregion - topregion) + 1;

#ifndef HAVE_X11
  dumpqueue ();
#endif

  if ((length > 0) && (newtop <= flexlines))
    {
#ifdef HAVE_X11
      XCopyArea (x_current_display, s->display.x->window_desc,
		 s->display.x->window_desc, s->display.x->normal_gc,
		 intborder, topregion * FONT_HEIGHT (s->display.x->font) + intborder,
		 s->width * FONT_WIDTH (s->display.x->font),
		 length * FONT_HEIGHT (s->display.x->font), intborder,
		 newtop * FONT_HEIGHT (s->display.x->font) + intborder);
#else
      XMoveArea (s->display.x->window_desc,
		 intborder, topregion * FONT_HEIGHT (s->display.x->font) + intborder,
		 intborder, newtop * FONT_HEIGHT (s->display.x->font) + intborder,
		 s->width * FONT_WIDTH (s->display.x->font),
		 length * FONT_HEIGHT (s->display.x->font));
      /* Now we must process any ExposeRegion events that occur
	 if the area being copied from is obscured.
	 We can't let it wait because further i/d operations
	 may want to copy this area to another area.  */
      x_read_exposes ();
#endif /* HAVE_X11 */
    }

  newtop = min (newtop, (flexlines - 1));
  length = newtop - topregion;
  if (length > 0)
    {
#ifdef HAVE_X11
      XClearArea (x_current_display, s->display.x->window_desc, intborder, 
		  topregion * FONT_HEIGHT (s->display.x->font) + intborder,
		  s->width * FONT_WIDTH (s->display.x->font),
		  n * FONT_HEIGHT (s->display.x->font), False);
#else
      XPixSet (s->display.x->window_desc,
	       intborder,
	       topregion * FONT_HEIGHT (s->display.x->font) + intborder,
	       s->width * FONT_WIDTH (s->display.x->font),
	       n * FONT_HEIGHT (s->display.x->font),
	       s->display.x->background_pixel);
#endif /* HAVE_X11 */
    }
}

/* Perform a delete-lines operation, deleting N lines
   at a vertical position curs_y.  */

static void
scraplines (n)
     register int n;
{
  int mask;
  register struct screen *s = updating_screen;
  int intborder = s->display.x->internal_border_width;

  if (curs_y >= flexlines)
    return;

#ifndef HAVE_X11
  dumpqueue ();
#endif

  if ((curs_y + n) >= flexlines)
    {
      if (flexlines >= (curs_y + 1))
	{
#ifdef HAVE_X11
	  XClearArea (x_current_display, s->display.x->window_desc, intborder,
		      curs_y * FONT_HEIGHT (s->display.x->font) + intborder,
		      s->width * FONT_WIDTH (s->display.x->font),
		      (flexlines - curs_y) * FONT_HEIGHT (s->display.x->font), False);
#else
	  XPixSet (s->display.x->window_desc,
		   intborder, curs_y * FONT_HEIGHT (s->display.x->font) + intborder,
		   s->width * FONT_WIDTH (s->display.x->font),
		   (flexlines - curs_y) * FONT_HEIGHT (s->display.x->font),
		   s->display.x->background_pixel);
#endif /* HAVE_X11 */
	}
    }
  else
    {
#ifdef HAVE_X11
      XCopyArea (x_current_display, s->display.x->window_desc,
		 s->display.x->window_desc, s->display.x->normal_gc,
		 intborder,
		 (curs_y + n) * FONT_HEIGHT (s->display.x->font) + intborder,
		 s->width * FONT_WIDTH (s->display.x->font),
		 (flexlines - (curs_y + n)) * FONT_HEIGHT (s->display.x->font),
		 intborder, curs_y * FONT_HEIGHT (s->display.x->font) + intborder);
      XClearArea (x_current_display, s->display.x->window_desc,
		  intborder,
		  (flexlines - n) * FONT_HEIGHT (s->display.x->font) + intborder,
		  s->width * FONT_WIDTH (s->display.x->font),
		  n * FONT_HEIGHT (s->display.x->font), False);
#else
      XMoveArea (s->display.x->window_desc,
		 intborder,
		 (curs_y + n) * FONT_HEIGHT (s->display.x->font) + intborder,
		 intborder, curs_y * FONT_HEIGHT (s->display.x->font) + intborder,
		 s->width * FONT_WIDTH (s->display.x->font),
		 (flexlines - (curs_y + n)) * FONT_HEIGHT (s->display.x->font));
      /* Now we must process any ExposeRegion events that occur
	 if the area being copied from is obscured.
	 We can't let it wait because further i/d operations
	 may want to copy this area to another area.  */
      x_read_exposes ();
      XPixSet (s->display.x->window_desc, intborder,
	       (flexlines - n) * FONT_HEIGHT (s->display.x->font) + intborder,
	       s->width * FONT_WIDTH (s->display.x->font),
	       n * FONT_HEIGHT (s->display.x->font), s->display.x->background_pixel);
#endif /* HAVE_X11 */
    }
}

/* Perform an insert-lines or delete-lines operation,
   inserting N lines or deleting -N lines at vertical position VPOS.  */

XTins_del_lines (vpos, n)
     int vpos, n;
{
  if (updating_screen == 0)
    abort ();

  /* Hide the cursor.  */
  x_display_cursor (updating_screen, 0);

  XTcursor_to (vpos, 0);

  BLOCK_INPUT;
  if (n >= 0)
    stufflines (n);
  else
    scraplines (-n);
  XFlushQueue ();
  UNBLOCK_INPUT;
}

static void clear_cursor ();

/* Output into a rectangle of an X-window (for screen S)
   the characters in s->phys_lines that overlap that rectangle.
   TOP and LEFT are the position of the upper left corner of the rectangle.
   ROWS and COLS are the size of the rectangle.  */

static void
dumprectangle (s, left, top, cols, rows)
     struct screen *s;
     register int left, top, cols, rows;
{
  register struct screen_glyphs *active_screen = SCREEN_CURRENT_GLYPHS (s);
  int cursor_cleared = 0;
  int bottom, right;
  register int y;

  if (SCREEN_GARBAGED_P (s))
    return;

  top -= s->display.x->internal_border_width;
  left -= s->display.x->internal_border_width;

  /* Express rectangle as four edges, instead of position-and-size.  */
  bottom = top + rows;
  right = left + cols;

#ifndef HAVE_X11		/* Window manger does this for X11. */
  /* If the rectangle includes any of the internal border area,
     redisplay the border emphasis.  */
  if (top < 0 || left < 0
      || bottom > s->height * FONT_HEIGHT (s->display.x->font)
      || right > s->width * FONT_WIDTH (s->display.x->font))
    dumpborder (s, 0);
#endif /* HAVE_X11 */
  
  /* Convert rectangle edges in pixels to edges in chars.
     Round down for left and top, up for right and bottom.  */
  top /= FONT_HEIGHT (s->display.x->font);
  left /= FONT_WIDTH (s->display.x->font);
  bottom += (FONT_HEIGHT (s->display.x->font) - 1);
  right += (FONT_WIDTH (s->display.x->font) - 1);
  bottom /= FONT_HEIGHT (s->display.x->font);
  right /= FONT_WIDTH (s->display.x->font);

  /* Clip the rectangle to what can be visible.  */
  if (left < 0)
    left = 0;
  if (top < 0)
    top = 0;
  if (right > s->width)
    right = s->width;
  if (bottom > s->height)
    bottom = s->height;

  /* Get size in chars of the rectangle.  */
  cols = right - left;
  rows = bottom - top;

  /* If rectangle has zero area, return.  */
  if (rows <= 0) return;
  if (cols <= 0) return;

  /* Turn off the cursor if it is in the rectangle.
     We will turn it back on afterward.  */
  if ((s->phys_cursor_x >= left) && (s->phys_cursor_x < right)
      && (s->phys_cursor_y >= top) && (s->phys_cursor_y < bottom))
    {
      clear_cursor (s);
      cursor_cleared = 1;
    }

  /* Display the text in the rectangle, one text line at a time.  */

  for (y = top; y < bottom; y++)
    {
      GLYPH *line = &active_screen->glyphs[y][left];

      if (! active_screen->enable[y] || left > active_screen->used[y])
	continue;

      dumpglyphs (s,
		 (left * FONT_WIDTH (s->display.x->font)
		  + s->display.x->internal_border_width),
		 (y * FONT_HEIGHT (s->display.x->font)
		  + s->display.x->internal_border_width),
		 line, min (cols, active_screen->used[y] - left),
		 active_screen->highlight[y], s->display.x->font);
    }

  /* Turn the cursor on if we turned it off.  */

  if (cursor_cleared)
    x_display_cursor (s, 1);
}

#ifndef HAVE_X11
/* Process all queued ExposeRegion events. */

static void
dumpqueue ()
{
  register int i;
  XExposeRegionEvent r;

  while (dequeue_event (&r, &x_expose_queue))
    {
      struct screen *s = x_window_to_screen (r.window);
      if (s->display.x->icon_desc == r.window)
	refreshicon (s);
      else
	dumprectangle (s, r.x, r.y, r.width, r.height);
    }
  XFlushQueue ();
}
#endif

/* Process all expose events that are pending.
   Redraws the cursor if necessary on any screen that
   is not in the process of being updated with update_screen.  */

static void
x_do_pending_expose ()
{
  int mask;
  struct screen *s;
  Lisp_Object tail, screen;

  if (expose_all_windows)
    {
      expose_all_windows = 0;
      for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  register int temp_width, temp_height;
	  int intborder;

	  screen = XCONS (tail)->car;
	  if (XTYPE (screen) != Lisp_Screen)
	    continue;
	  s = XSCREEN (screen);
	  if (! SCREEN_IS_X (s))
	    continue;
	  if (!s->visible)
	    continue;
	  if (!s->display.x->needs_exposure)
	    continue;

	  intborder = s->display.x->internal_border_width;

	  clear_cursor (s);
	  XGetWindowInfo (s->display.x->window_desc, &windowinfo);
	  temp_width = ((windowinfo.width - 2 * intborder
			 - s->display.x->v_scrollbar_width)
			/ FONT_WIDTH (s->display.x->font));
	  temp_height = ((windowinfo.height- 2 * intborder
			  - s->display.x->h_scrollbar_height)
			 / FONT_HEIGHT (s->display.x->font));
	  if (temp_width != s->width || temp_height != s->height)
	    {
	      change_screen_size (s, max (1, temp_height),
				  max (1, temp_width), 0);
	      x_resize_scrollbars (s);
	    }
	  s->display.x->left_pos = windowinfo.x;
	  s->display.x->top_pos = windowinfo.y;
	  dumprectangle (s, 0, 0, PIXEL_WIDTH (s), PIXEL_HEIGHT (s));
#if 0
	  dumpborder (s, 0);
#endif
	  s->display.x->needs_exposure = 0;
	  if (updating_screen != s)
	    x_display_cursor (s, 1);
	  XFlushQueue ();
	}
    }
  else
    /* Handle any individual-rectangle expose events queued
       for various windows.  */
#ifdef HAVE_X11
    ;
#else
    dumpqueue ();
#endif
}

#ifdef HAVE_X11
static void
screen_highlight (screen)
     struct screen *screen;
{
  if (! EQ (Vx_no_window_manager, Qnil))
    XSetWindowBorder (x_current_display, screen->display.x->window_desc,
		      screen->display.x->border_pixel);
  x_display_cursor (screen, 1);
}

static void
screen_unhighlight (screen)
     struct screen *screen;
{
  if (! EQ (Vx_no_window_manager, Qnil))
    XSetWindowBorderPixmap (x_current_display, screen->display.x->window_desc,
			    screen->display.x->border_tile);
  x_display_cursor (screen, 1);
}
#else	/* X10 */
/* Dump the border-emphasis of screen S.
   If S is selected, this is a lining of the same color as the border,
   just within the border, occupying a portion of the internal border.
   If S is not selected, it is background in the same place.
   If ALWAYS is 0, don't bother explicitly drawing if it's background.

   ALWAYS = 1 is used when a screen becomes selected or deselected.
   In that case, we also turn the cursor off and on again
   so it will appear in the proper shape (solid if selected; else hollow.)  */

static void
dumpborder (s, always)
     struct screen *s;
     int always;
{
  int thickness = s->display.x->internal_border_width / 2;
  int width = PIXEL_WIDTH (s);
  int height = PIXEL_HEIGHT (s);
  int pixel;

  if (s != selected_screen)
    {
      if (!always)
	return;

      pixel = s->display.x->background_pixel;
    }
  else
    {
      pixel = s->display.x->border_pixel;
    }

  XPixSet (s->display.x->window_desc, 0, 0, width, thickness, pixel);
  XPixSet (s->display.x->window_desc, 0, 0, thickness, height, pixel);
  XPixSet (s->display.x->window_desc, 0, height - thickness, width,
	   thickness, pixel);
  XPixSet (s->display.x->window_desc, width - thickness, 0, thickness,
	   height, pixel);

  if (always)
    x_display_cursor (s, 1);
}
#endif	/* X10 */

static void XTscreen_rehighlight ();

/* The focus has changed.  Update the screens as necessary to reflect
   the new situation.  Note that we can't change the selected screen
   here, because the lisp code we are interrupting might become confused.
   Each event gets marked with the screen in which it occured, so the
   lisp code can tell when the switch took place by examining the events.  */

static void
x_new_focus_screen (screen)
     struct screen *screen;
{
  struct screen *old_focus = x_focus_screen;
  int events_enqueued = 0;

  if (screen != x_focus_screen)
    {
      /* Set this before calling other routines, so that they see 
	 the correct value of x_focus_screen.  */
      x_focus_screen = screen;

      if (old_focus && old_focus->auto_lower)
	x_lower_screen (old_focus);

#if 0
      selected_screen = screen;
      XSET (XWINDOW (selected_screen->selected_window)->screen,
	    Lisp_Screen, selected_screen);
      Fselect_window (selected_screen->selected_window);
      choose_minibuf_screen ();
#endif

      if (x_focus_screen && x_focus_screen->auto_raise)
	x_raise_screen (x_focus_screen);
    }

  XTscreen_rehighlight ();
}


/* The focus has changed, or we have make a screen's selected window
   point to a window on a different screen (this happens with global
   minibuffer screens).  Shift the highlight as appropriate.  */
static void
XTscreen_rehighlight ()
{
  struct screen *old_highlight = x_highlight_screen;

  if (x_focus_screen)
    {
      x_highlight_screen = XSCREEN (SCREEN_FOCUS_SCREEN (x_focus_screen));
      if (x_highlight_screen->display.nothing == 0)
	XSET (SCREEN_FOCUS_SCREEN (x_focus_screen), Lisp_Screen,
	      (x_highlight_screen = x_focus_screen));
    }
  else
    x_highlight_screen = 0;

  if (x_highlight_screen != old_highlight)
    {
      if (old_highlight)
	screen_unhighlight (old_highlight);
      if (x_highlight_screen)
	screen_highlight (x_highlight_screen);
    }
}

enum window_type
{
  no_window,
  scrollbar_window,
  text_window,
};

/* Position of the mouse in characters */
unsigned int x_mouse_x, x_mouse_y;

/* Offset in buffer of character under the pointer, or 0. */
extern int mouse_buffer_offset;

extern int buffer_posn_from_coords ();

/* Symbols from xfns.c to denote the different parts of a window.  */
extern Lisp_Object Qmodeline_part, Qtext_part;

#if 0
/* Set *RESULT to an emacs input_event corresponding to MOTION_EVENT.
   S is the screen in which the event occurred.

   WINDOW_TYPE says whether the event happened in a scrollbar window
   or a text window, affecting the format of the event created.

   PART specifies which part of the scrollbar the event happened in,
   if WINDOW_TYPE == scrollbar_window.

   If the mouse is over the same character as the last time we checked,
   don't return an event; set result->kind to no_event.  */

static void
notice_mouse_movement (result, motion_event, s, window_type, part)
     struct input_event *result;
     XMotionEvent motion_event;
     struct screen *s;
     int window_type;
     Lisp_Object part;
{
  int x, y, root_x, root_y, pix_x, pix_y;
  unsigned int keys_and_buttons;
  Window w, root_window;

  /* Unless we decide otherwise below, return a non-event.  */
  result->kind = no_event;
  
  if (XQueryPointer (x_current_display,
		     s->display.x->window_desc,
		     &root_window, &w,
		     &root_x, &root_y, &pix_x, &pix_y,
		     &keys_and_buttons)
      == False)
    return;

#if 0
  if (w == None)   /* Mouse no longer in window. */
    return Qnil;
#endif

  pixel_to_glyph_translation (s, pix_x, pix_y, &x, &y);
  if (x == x_mouse_x && y == x_mouse_y)
    return;

  x_mouse_x = x;
  x_mouse_y = y;

  /* What sort of window are we in now?  */
  if (window_type == text_window)            /* Text part */
    {
      int modeline_p;

      Vmouse_window = window_from_coordinates (s, x, y, &modeline_p);

      if (XTYPE (Vmouse_window) == Lisp_Window)
	mouse_buffer_offset
	  = buffer_posn_from_coords (XWINDOW (Vmouse_window), x, y);
      else
	mouse_buffer_offset = 0;

      if (EQ (Vmouse_window, Qnil))
	Vmouse_screen_part = Qnil;
      else if (modeline_p)
	Vmouse_screen_part = Qmodeline_part;
      else
	Vmouse_screen_part = Qtext_part;
      
      result->kind = window_sys_event;
      result->code = Qmouse_moved;

      return;
    }
  else if (window_type == scrollbar_window)  /* Scrollbar */
    {
      Vmouse_window = s->selected_window;
      mouse_buffer_offset = 0;
      Vmouse_screen_part = part;

      result->kind = window_sys_event;
      result->code = Qmouse_moved;

      return;
    }

  return;
}
#endif


/* Mouse clicks and mouse movement.  Rah.  */
#ifdef HAVE_X11

/* Given a pixel position (PIX_X, PIX_Y) on the screen S, return
   glyph co-ordinates in (*X, *Y).  Set *BOUNDS to the rectangle
   that the glyph at X, Y occupies, if BOUNDS != 0.  */
static void
pixel_to_glyph_coords (s, pix_x, pix_y, x, y, bounds)
     SCREEN_PTR s;
     register unsigned int pix_x, pix_y;
     register int *x, *y;
     XRectangle *bounds;
{
  int ibw = s->display.x->internal_border_width;
  int width, height;
  FONT_TYPE *font = s->display.x->font;

  width = FONT_WIDTH (font);
  height = FONT_HEIGHT (font);

  /* What line is it on?  */
  if (pix_y < ibw)
    *y = 0;
  else if (pix_y > s->display.x->pixel_height - ibw)
    *y = SCREEN_HEIGHT (s) - 1;
  else
    *y = (pix_y - ibw) / height;

  /* And what column?  */
  if (pix_x < ibw)
    *x = 0;
  else if (pix_x > s->display.x->pixel_width - ibw)
    *x = SCREEN_WIDTH (s) - 1;
  else
    *x = (pix_x - ibw) / width;

  if (bounds)
    {
      bounds->width = width;
      bounds->height = height;
      bounds->x = ibw + (*x * width);
      bounds->y = ibw + (*y * height);
    }
}

/* Any buttons grabbed. */
unsigned int x_mouse_grabbed;

/* Convert a set of X modifier bits to the proper form for a
   struct input_event modifiers value.  */

static Lisp_Object
x_convert_modifiers (state)
     unsigned int state;
{
  return (  ((state & (ShiftMask | LockMask)) ? shift_modifier : 0)
	  | ((state & ControlMask)            ? ctrl_modifier  : 0)
	  | ((state & Mod1Mask)               ? meta_modifier  : 0));
}

extern struct screen *x_window_to_scrollbar ();
extern Lisp_Object Vmouse_event;

/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.

   If PART and PREFIX are 0, then the event occurred in the text part;
   otherwise it happened in a scrollbar. */

static Lisp_Object
construct_mouse_click (result, event, s, part, prefix)
     struct input_event *result;
     XButtonEvent *event;
     struct screen *s;
     int prefix;
     Lisp_Object part;
{
  /* Initialize those fields text and scrollbar clicks hold in common.
     Make the event type no_event; we'll change that when we decide
     otherwise.  */
  result->kind = no_event;
  XSET (result->code, Lisp_Int, event->button);
  XSET (result->timestamp, Lisp_Int, event->time);
  result->modifiers = (x_convert_modifiers (event->state)
		       | (event->type == ButtonRelease ? up_modifier : 0));
  XSET (result->timestamp, Lisp_Int, (event->time & 0x7fffff));

  /* Notice if the mouse is still grabbed.  */
  if (event->type == ButtonPress)
    {
      if (! x_mouse_grabbed)
	Vmouse_depressed = Qt;
      x_mouse_grabbed |= (1 << event->button);
    }
  else if (event->type == ButtonRelease)
    {
      x_mouse_grabbed &= ~(1 << event->button);
      if (!x_mouse_grabbed)
	Vmouse_depressed = Qnil;
    }

  if (part)			/* Scrollbar event */
    {
      int pos, len;

      pos = event->y - (s->display.x->v_scrollbar_width - 2);
      XSET (x_mouse_x, Lisp_Int, pos);
      len = ((FONT_HEIGHT (s->display.x->font) * s->height)
	     + s->display.x->internal_border_width
	     - (2 * (s->display.x->v_scrollbar_width - 2)));
      XSET (x_mouse_y, Lisp_Int, len);

      result->kind = scrollbar_click;
      result->part = part;
      XSET (result->x, Lisp_Int, (s->display.x->top_pos - event->y));
      XSET (result->y, Lisp_Int, s->display.x->pixel_height);
      result->screen = s;
    }
  else				/* Text Window Event */
    {
      int row, column;

      pixel_to_glyph_coords (s, event->x, event->y, &column, &row, NULL);
      result->kind = mouse_click;
      result->x = column;
      result->y = row;
      result->screen = s;
    }
}


/* Mouse movement.  Rah.

   In order to avoid asking for motion events and then throwing most
   of them away or busy-polling the server for mouse positions, we ask
   the server for pointer motion hints.  This means that we get only
   one event per group of mouse movements.  "Groups" are delimited by
   other kinds of events (focus changes and button clicks, for
   example), or by XQueryPointer calls; when one of these happens, we
   get another MotionNotify event the next time the mouse moves.  This
   is at least as efficient than getting motion events when mouse
   tracking is on, and I suspect only negligibly worse when tracking
   is off.

   The silly O'Reilly & Associates Nutshell guides barely document
   pointer motion hints at all (I think you have to infer how they
   work from an example), and the description of XQueryPointer doesn't
   mention that calling it causes you to get another motion hint from
   the server, which is very important.  */

/* Where the mouse was last time we reported a mouse event.  */
static SCREEN_PTR last_mouse_screen;
static XRectangle last_mouse_glyph;

/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */
static void
note_mouse_position (screen, event)
     SCREEN_PTR screen;
     XMotionEvent *event;

{
  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  if (event->x < last_mouse_glyph.x
      || event->x >= last_mouse_glyph.x + last_mouse_glyph.width
      || event->y < last_mouse_glyph.y
      || event->y >= last_mouse_glyph.y + last_mouse_glyph.height)
    mouse_moved = 1;
  else
    {
      /* It's on the same glyph.  Call XQueryPointer so we'll get an
	 event the next time the mouse moves and we can see if it's
	 *still* on the same glyph.  */
      int dummy;
      
      XQueryPointer (event->display, event->window,
		     (Window *) &dummy, (Window *) &dummy,
		     &dummy, &dummy, &dummy, &dummy,
		     (unsigned int *) &dummy);
    }
}

/* Return the current position of the mouse.

   This clears the mouse_moved flag, so we can wait for the next mouse
   position.  This also calls XQueryPointer, which will cause the
   server to give us another MotionNotify when the mouse moves again.
   */

static void
XTmouse_position (s, x, y, time)
     SCREEN_PTR *s;
     Lisp_Object *x, *y;
     Lisp_Object *time;
{
  int ix, iy, dummy;
  Display *d = x_current_display;
  Window guess, root, child;

  BLOCK_INPUT;

  /* I would like to have an X function that just told me the
     innermost window containing the mouse.  

  /* There doesn't seem to be any way to just get the innermost window
     containing the pointer, no matter what X screen it's on; you have
     to guess a window, and then X will tell you which one of that
     window's children it's in.  If the pointer isn't in any of that
     window's children, it gives you a root window that contains it.

     So we start with the selected screen's window and chase down
     branches under the guidance of XQueryPointer until we hit a leaf
     (all of the Emacs windows we care about are leaf windows).  If at
     any time XQueryPointer returns false, that means that the current
     window does not contain the pointer any more (perhaps it moved),
     so we start with the root window XQueryPointer has given us and
     start again.  */

  guess = selected_screen->display.x->window_desc;
  for (;;)
    if (XQueryPointer (d, guess, &root, &child,
		       &dummy, &dummy, &ix, &iy, (unsigned int *) &dummy))
      {
	if (child == None)
	  /* Guess is a leaf window, and it contains the pointer.  */
	  break;
	else 
	  guess = child;
      }
    else
      /* When XQueryPointer returns False, the pointer isn't in guess
         anymore, but root is the root window of the screen we should
         try instead.  */
      guess = root;

  *s = last_mouse_screen = x_window_to_screen (guess);
  if (! *s)
    *x = *y = Qnil;
  else
    {
      pixel_to_glyph_coords (*s, ix, iy, &ix, &iy, &last_mouse_glyph);
      XSET (*x, Lisp_Int, ix);
      XSET (*y, Lisp_Int, iy);
    }

  mouse_moved = 0;

  /* I don't know how to find the time for the last movement; it seems
   like XQueryPointer ought to return it, but it doesn't.  */
  *time = Qnil;

  UNBLOCK_INPUT;
}


static char *events[] =
{
  "0: ERROR!",
  "1: REPLY",
  "KeyPress",
  "KeyRelease",
  "ButtonPress",
  "ButtonRelease",
  "MotionNotify",
  "EnterNotify",
  "LeaveNotify",
  "FocusIn",
  "FocusOut",
  "KeymapNotify",
  "Expose",
  "GraphicsExpose",
  "NoExpose",
  "VisibilityNotify",
  "CreateNotify",
  "DestroyNotify",
  "UnmapNotify",
  "MapNotify",
  "MapRequest",
  "ReparentNotify",
  "ConfigureNotify",
  "ConfigureRequest",
  "GravityNotify",
  "ResizeRequest",
  "CirculateNotify",
  "CirculateRequest",
  "PropertyNotify",
  "SelectionClear",
  "SelectionRequest",
  "SelectionNotify",
  "ColormapNotify",
  "ClientMessage",
  "MappingNotify",
  "LASTEvent"
};
#else  /* X10 */
#define XEvent XKeyPressedEvent
#endif /* HAVE_X11 */ 

/* Timestamp of enter window event.  This is only used by XTread_socket,
   but we have to put it out here, since static variables within functions
   sometimes don't work.  */
static Time enter_timestamp;

/* Read events coming from the X server.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   Events representing keys are stored in buffer BUFP,
   which can hold up to NUMCHARS characters.
   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   WAITP is nonzero if we should block until input arrives.
   EXPECTED is nonzero if the caller knows input is available.  */

Lisp_Object
XTread_socket (sd, bufp, numchars, waitp, expected)
     register int sd;
     register struct input_event *bufp;
     register int numchars;
     int waitp;
     int expected;
{
  int count = 0;
  int nbytes = 0;
  int mask;
  int items_pending;		/* How many items are in the X queue. */
  XEvent event;
  struct screen *s;
  int event_found;
  int prefix;
  Lisp_Object part;

  if (x_input_blocked)
    {
      x_pending_input = 1;
      return -1;
    }

  x_pending_input = 0;
  BLOCK_INPUT;
	
  if (numchars <= 0)
    abort ();			/* Don't think this happens. */

#ifdef FIOSNBIO
  /* If available, Xlib uses FIOSNBIO to make the socket
     non-blocking, and then looks for EWOULDBLOCK.  If O_NDELAY is set,
     FIOSNBIO is ignored, and instead of signalling EWOULDBLOCK,
     a read returns 0, which Xlib interprets as equivalent to EPIPE. */
  fcntl (fileno (stdin), F_SETFL, 0);
#endif

#ifndef SIGIO
#ifndef HAVE_SELECT
  if (! (fcntl (fileno (stdin), F_GETFL, 0) & O_NDELAY))
    {
      extern int read_alarm_should_throw;
      read_alarm_should_throw = 1;
      XPeekEvent (XDISPLAY &event);
      read_alarm_should_throw = 0;
    }
#endif
#endif

  while (XStuffPending () != 0)
    {
      XNextEvent (XDISPLAY &event);
      event_found = 1;

      switch (event.type)
	{
#ifdef HAVE_X11

	case SelectionClear:	/* Someone has grabbed ownership. */
	  x_disown_selection (event.xselectionclear.window,
			      event.xselectionclear.selection,
			      event.xselectionclear.time);
	  break;

	case SelectionRequest:	/* Someone wants our selection. */
	  x_answer_selection_request (event);
	  break;

	case PropertyNotify:
	  /* If we were to do this synchronously, there'd be no worry
	     about re-selecting. */
	  x_send_incremental (event);
	  break;

	case Expose:
	  s = x_window_to_screen (event.xexpose.window);
	  if (s)
	    {
	      if (s->visible == 0)
		{
		  s->visible = 1;
		  s->iconified = 0;
		  SET_SCREEN_GARBAGED (s);
		}
	      else
		dumprectangle (x_window_to_screen (event.xexpose.window),
			       event.xexpose.x, event.xexpose.y,
			       event.xexpose.width, event.xexpose.height);
	    }
	  break;

	case GraphicsExpose:	/* This occurs when an XCopyArea's
				  source area was obscured or not
				  available.*/
	  dumprectangle (x_window_to_screen (event.xgraphicsexpose.drawable),
			 event.xgraphicsexpose.x, event.xgraphicsexpose.y,
			 event.xgraphicsexpose.width,
			 event.xgraphicsexpose.height);
	  break;

	case NoExpose:		/* This occurs when an XCopyArea's
				  source area was completely
				  available */
	  break;
#else /* not HAVE_X11 */
	case ExposeWindow:
	  if (event.subwindow != 0)
	    break;		/* duplicate event */
	  s = x_window_to_screen (event.window);
	  if (event.window == s->display.x->icon_desc)
	    {
	      refreshicon (s);
	      s->iconified = 1;
	    }
	  if (event.window == s->display.x->window_desc)
	    {
	      /* Say must check all windows' needs_exposure flags.  */
	      expose_all_windows = 1;
	      s->display.x->needs_exposure = 1;
	      s->visible = 1;
	    }
	  break;

	case ExposeRegion:
	  if (event.subwindow != 0)
	    break;		/* duplicate event */
	  s = x_window_to_screen (event.window);
	  if (event.window == s->display.x->icon_desc)
	    {
	      refreshicon (s);
	      break;
	    }
	  /* If window already needs full redraw, ignore this rectangle.  */
	  if (expose_all_windows && s->display.x->needs_exposure)
	    break;
	  /* Put the event on the queue of rectangles to redraw.  */
	  if (enqueue_event (&event, &x_expose_queue))
	    /* If it is full, we can't record the rectangle,
	       so redraw this entire window.  */
	    {
	      /* Say must check all windows' needs_exposure flags.  */
	      expose_all_windows = 1;
	      s->display.x->needs_exposure = 1;
	    }
	  break;

	case ExposeCopy:
	  /* This should happen only when we are expecting it,
	     in x_read_exposes.  */
	  abort ();
#endif /* not HAVE_X11 */

#ifdef HAVE_X11
	case UnmapNotify:
	  {
	    XWMHints *hints;

	    s = x_window_to_screen (event.xunmap.window);
	    if (s)		/* S may no longer exist if
				   the screen was deleted.  */
	      {
		/* While a screen is unmapped, display generation is
		   disabled; you don't want to spend time updating a
		   display that won't ever be seen.  */
		s->visible = 0;
		x_mouse_x = x_mouse_y = -1;
	      }
	  }
	  break;

	case MapNotify:
	  s = x_window_to_screen (event.xmap.window);
	  if (s)
	    {
	      s->visible = 1;
	      s->iconified = 0;

	      /* wait_reading_process_input will notice this and update
		 the screen's display structures.  */
	      SET_SCREEN_GARBAGED (s);
	    }
	  break;

	  /* Turn off processing if we become fully obscured. */
	case VisibilityNotify:
	  break;

#else
	case UnmapWindow:
	  s = x_window_to_screen (event.window);
	  if (event.window == s->display.x->icon_desc)
	    s->iconified = 0;
	  if (event.window == s->display.x->window_desc)
	    s->visible = 0;
	  break;
#endif /* HAVE_X11 */

#ifdef HAVE_X11
	case KeyPress:
	  s = x_window_to_screen (event.xkey.window);
	  if (s != 0)
	    {
	      KeySym keysym;
	      XComposeStatus status;
	      char copy_buffer[80];
	      int modifiers = event.xkey.state;

	      /* Some keyboards generate different characters
		 depending on the state of the meta key, in an attempt
		 to support non-English typists.  It would be nice to
		 keep this functionality somehow, but for now, we will
		 just clear the meta-key flag to get the 'pure' character.  */
	      event.xkey.state &= ~Mod1Mask;

	      /* This will have to go some day... */
	      nbytes = XLookupString (&event.xkey,
				      copy_buffer,
				      80,
				      &keysym,
				      &status);

	      /* Strip off the vendor-specific keysym bit, and take a shot
		 at recognizing the codes.  HP servers have extra keysyms
		 that fit into the MiscFunctionKey category.  */
	      keysym &= ~(1<<28);

	      if (numchars > 1)
		{
		  if (IsCursorKey (keysym)          /* 0xff50 <= x < 0xff60 */
		      || IsMiscFunctionKey (keysym) /* 0xff60 <= x < 0xff80 */
		      || IsKeypadKey (keysym)       /* 0xff80 <= x < 0xffbe */
		      || IsFunctionKey (keysym))    /* 0xffbe <= x < 0xffe1 */
		    {
		      bufp->kind = non_ascii_keystroke;
		      XSET (bufp->code, Lisp_Int, (unsigned) keysym - 0xff50);
		      bufp->screen = XSCREEN (SCREEN_FOCUS_SCREEN (s));
		      bufp->modifiers = x_convert_modifiers (modifiers);
		      XSET (bufp->timestamp, Lisp_Int, event.xkey.time);
		      bufp++;
		      count++;
		      numchars--;
		    }
		  else if (numchars > nbytes)
		    {
		      register int i;

		      if (nbytes == 1)
			{
			  if (modifiers & Mod1Mask)
			    *copy_buffer |= METABIT;
			  bufp->kind = ascii_keystroke;
			  bufp->screen = XSCREEN (SCREEN_FOCUS_SCREEN (s));
			  XSET (bufp->code, Lisp_Int, *copy_buffer);
			  XSET (bufp->timestamp, Lisp_Int, event.xkey.time);
			  bufp++;
			}
		      else
			for (i = nbytes - 1; i > 1; i--)
			  {
			    bufp->kind = ascii_keystroke;
			    XSET (bufp->code, Lisp_Int, copy_buffer[i]);
			    XSET (bufp->timestamp, Lisp_Int, event.xkey.time);
			    bufp->screen = XSCREEN (SCREEN_FOCUS_SCREEN (s));
			    bufp++;
			  }

		      count += nbytes;
		      numchars -= nbytes;
		    }
		}
	    }
	  break;
#else
	case KeyPressed:
	  {
	    register char *where_mapping;

	    s = x_window_to_screen (event.window);
	    /* Ignore keys typed on icon windows.  */
	    if (s != 0 && event.window == s->display.x->icon_desc)
	      break;
	    where_mapping = XLookupMapping (&event, &nbytes);
	    /* Nasty fix for arrow keys */
	    if (!nbytes && IsCursorKey (event.detail & 0xff))
	      {
		switch (event.detail & 0xff)
		  {
		  case KC_CURSOR_LEFT:
		    where_mapping = "\002";
		    break;
		  case KC_CURSOR_RIGHT:
		    where_mapping = "\006";
		    break;
		  case KC_CURSOR_UP:
		    where_mapping = "\020";
		    break;
		  case KC_CURSOR_DOWN:
		    where_mapping = "\016";
		    break;
		  }
		nbytes = 1;
	      }
	    if (numchars - nbytes > 0)
	      {
		register int i;

		for (i = 0; i < nbytes; i++)
		  {
		    bufp->kind = ascii_keystroke;
		    XSET (bufp->code, Lisp_Int, where_mapping[i]);
		    XSET (bufp->time, Lisp_Int, event.xkey.time);
		    bufp->screen = XSCREEN (SCREEN_FOCUS_SCREEN (s));
		    bufp++;
		  }
		count += nbytes;
		numchars -= nbytes;
	      }
	  }
	  break;
#endif /* HAVE_X11 */

#ifdef HAVE_X11
	case EnterNotify:
	  s = x_window_to_screen (event.xcrossing.window);

	  if (event.xcrossing.detail == NotifyInferior)	/* Left Scrollbar */
	    ;
	  else if (event.xcrossing.focus)		/* Entered Window */
	    {
	      /* If we decide we want to generate an event to be seen
		 by the rest of Emacs, we put it here.  */
	      struct input_event emacs_event;
	      emacs_event.kind = no_event;

	      /* Avoid nasty pop/raise loops. */
	      if (s && (!(s->auto_raise)
			|| !(s->auto_lower)
			|| (event.xcrossing.time - enter_timestamp) > 500))
		{
		  x_new_focus_screen (s);
		  enter_timestamp = event.xcrossing.time;
		}
#if 0
	      else if ((s = x_window_to_scrollbar (event.xcrossing.window,
						   &part, &prefix)))
		/* Fake a motion event */
		notice_mouse_movement (&emacs_event,
				       event.xmotion, s, scrollbar_window,
				       part);
#endif

#if 0
	      if (! EQ (Vx_send_mouse_movement_events, Qnil)
		  && numchars >= 1
		  && emacs_event.kind != no_event)
		{
		  bcopy (&emacs_event, bufp, sizeof (struct input_event));
		  bufp++;
		  count++;
		  numchars--;
		}
#endif
	    }
	  else if (s == x_focus_screen)
	    x_new_focus_screen (0);
#if 0
	  else if (s = x_window_to_screen (event.xcrossing.window))
	    x_mouse_screen = s;
#endif

	  break;

	case FocusIn:
	  s = x_window_to_screen (event.xfocus.window);
	  if (s)
	    x_new_focus_screen (s);
	  break;

	case LeaveNotify:
	  if (event.xcrossing.detail != NotifyInferior
	      && event.xcrossing.subwindow == None
	      && event.xcrossing.mode == NotifyNormal)
	    {
	      s = x_window_to_screen (event.xcrossing.window);
	      if (event.xcrossing.focus)
		x_new_focus_screen (s);
	      else if (s == x_focus_screen)
		x_new_focus_screen (0);
	    }
	  break;

	case FocusOut:
	  s = x_window_to_screen (event.xfocus.window);
	  if (s && s == x_focus_screen)
	    x_new_focus_screen (0);
	  break;

#else /* not HAVE_X11 */

	case EnterWindow:
	  if ((event.detail & 0xFF) == 1)
	    break;		/* Coming from our own subwindow */
	  if (event.subwindow != 0)
	    break;		/* Entering our own subwindow.  */

	  {
	    extern int waiting_for_input;
	    struct screen *old_s = x_input_screen;

	    s = x_window_to_screen (event.window);
	    x_mouse_screen = s;

	    if (waiting_for_input && x_focus_screen == 0)
	      x_new_focus_screen (s);
	  }
	  break;

	case LeaveWindow:
	  if ((event.detail & 0xFF) == 1)
	    break;		/* Entering our own subwindow */
	  if (event.subwindow != 0)
	    break;		/* Leaving our own subwindow.  */

	  x_mouse_screen = 0;
	  if (x_focus_screen == 0
	      && x_input_screen != 0
	      && x_input_screen == x_window_to_screen (event.window)
	      && event.window == x_input_screen->display.x->window_desc)
	    {
	      s = x_input_screen;
	      x_input_screen = 0;
	      if (s)
		screen_unhighlight (s);
	    }
	  break;
#endif /* not HAVE_X11 */

#ifdef HAVE_X11
	case MotionNotify:
	  {
	    s = x_window_to_screen (event.xmotion.window);
	    if (s)
	      note_mouse_position (s, &event.xmotion);
#if 0
	    else if ((s = x_window_to_scrollbar (event.xmotion.window,
						 &part, &prefix)))
	      {
		What should go here?
	      }
#endif
	  }
	  break;

	case ConfigureNotify:
	  {
	    int rows, columns;
	    s = x_window_to_screen (event.xconfigure.window);
	    if (!s)
	      break;

	    columns = ((event.xconfigure.width -
			(2 * s->display.x->internal_border_width)
			- s->display.x->v_scrollbar_width)
		       / FONT_WIDTH (s->display.x->font));
	    rows = ((event.xconfigure.height -
		     (2 * s->display.x->internal_border_width)
		     - s->display.x->h_scrollbar_height)
		    / FONT_HEIGHT (s->display.x->font));

	    /* Even if the number of character rows and columns has
	       not changed, the font size may have changed, so we need
	       to check the pixel dimensions as well.  */
	    if (columns != s->width
		|| rows != s->height
		|| event.xconfigure.width != s->display.x->pixel_width
		|| event.xconfigure.height != s->display.x->pixel_height)
	      {
		change_screen_size (s, rows, columns, 0);
		x_resize_scrollbars (s);
		SET_SCREEN_GARBAGED (s);
	      }

	    s->display.x->pixel_width = event.xconfigure.width;
	    s->display.x->pixel_height = event.xconfigure.height;
	    s->display.x->left_pos = event.xconfigure.x;
	    s->display.x->top_pos = event.xconfigure.y;
	    break;
	  }

	case ButtonPress:
	case ButtonRelease:
	  {
	    /* If we decide we want to generate an event to be seen
	       by the rest of Emacs, we put it here.  */
	    struct input_event emacs_event;
	    emacs_event.kind = no_event;

	    s = x_window_to_screen (event.xbutton.window);
	    if (s)
	      if (!x_focus_screen || (s == x_focus_screen))
		construct_mouse_click (&emacs_event,
				       &event, s, 0, 0);
	      else
		continue;
	    else
	      if ((s = x_window_to_scrollbar (event.xbutton.window,
					      &part, &prefix)))
		{
		  if (!x_focus_screen || (selected_screen == x_focus_screen))
		    construct_mouse_click (&emacs_event,
					   &event, s, part, prefix);
		  else
		    continue;
		}

	    if (numchars >= 1 && emacs_event.kind != no_event)
	      {
		bcopy (&emacs_event, bufp, sizeof (struct input_event));
		bufp++;
		count++;
		numchars--;
	      }
	  }
	  break;

#else /* not HAVE_X11 */
	case ButtonPressed:
	case ButtonReleased:
	  s = x_window_to_screen (event.window);
	  if (s)
	    {
	      if (event.window == s->display.x->icon_desc)
		{
		  x_make_screen_visible (s);

		  if (warp_mouse_on_deiconify)
		    XWarpMouse (s->display.x->window_desc, 10, 10);
		  break;
		}
	      if (event.window == s->display.x->window_desc)
		{
		  if (s->auto_raise)
		    x_raise_screen (s);
		}
	    }
	  enqueue_event (&event, &x_mouse_queue);
	  if (numchars >= 2)
	    {
	      bufp->kind = ascii_keystroke;
	      bufp->code = (char) 'X' & 037; /* C-x */
	      bufp->screen = XSCREEN (SCREEN_FOCUS_SCREEN (s));
	      XSET (bufp->time, Lisp_Int, event.xkey.time);
	      bufp++;

	      bufp->kind = ascii_keystroke;
	      bufp->code = (char) 0; /* C-@ */
	      bufp->screen = XSCREEN (SCREEN_FOCUS_SCREEN (s));
	      XSET (bufp->time, Lisp_Int, event.xkey.time);
	      bufp++;

	      count += 2;
	      numchars -= 2;
	    }
	  break;
#endif /* not HAVE_X11 */

#ifdef HAVE_X11

	case CirculateNotify:
	  break;
	case CirculateRequest:
	  break;

#endif /* HAVE_X11 */

	case MappingNotify:
	  if (event.xmapping.request == MappingKeyboard)
	    /* Someone has changed the keyboard mapping - flush the
	       local cache.  */
	    XRefreshKeyboardMapping (&event.xmapping);
	  break;

	default:
	  break;
	}
    }

#if 0
#ifdef HAVE_SELECT
  if (expected && ! event_found)
    {
      /* AOJ 880406: if select returns true but XPending doesn't, it means that
	 there is an EOF condition; in other words, that X has died.
	 Act as if there had been a hangup. */

      int fd = ConnectionNumber (x_current_display);
      int mask = 1 << fd;

      if (0 != select (fd + 1, &mask, (long *) 0, (long *) 0,
		       (EMACS_TIME) 0)
	  && !XStuffPending ())
	kill (getpid (), SIGHUP);
    }
#endif /* HAVE_SELECT */
#endif

  if (updating_screen == 0)
    x_do_pending_expose ();

  UNBLOCK_INPUT;
  return count;
}

#ifndef HAVE_X11
/* Read and process only Expose events
   until we get an ExposeCopy event; then return.
   This is used in insert/delete line.
   We assume input is already blocked.  */

static void
x_read_exposes ()
{
  struct screen *s;
  XKeyPressedEvent event;

  while (1)
    {
      /* while there are more events*/
      XMaskEvent (ExposeWindow | ExposeRegion | ExposeCopy, &event);
      switch (event.type)
	{
	case ExposeWindow:
	  if (event.subwindow != 0)
	    break;			/* duplicate event */
	  s = x_window_to_screen (event.window);
	  if (event.window == s->display.x->icon_desc)
	    {
	      refreshicon (s);
	      break;
	    }
	  if (event.window == s->display.x->window_desc)
	    {
	      expose_all_windows = 1;
	      s->display.x->needs_exposure = 1;
	      break;
	    }
	  break;

	case ExposeRegion:
	  if (event.subwindow != 0)
	    break;			/* duplicate event */
	  s = x_window_to_screen (event.window);
	  if (event.window == s->display.x->icon_desc)
	    {
	      refreshicon (s);
	      break;
	    }
	  /* If window already needs full redraw, ignore this rectangle.  */
	  if (expose_all_windows && s->display.x->needs_exposure)
	    break;
	  /* Put the event on the queue of rectangles to redraw.  */
	  if (enqueue_event (&event, &x_expose_queue))
	    /* If it is full, we can't record the rectangle,
	       so redraw this entire window.  */
	    {
	      /* Say must check all windows' needs_exposure flags.  */
	      expose_all_windows = 1;
	      s->display.x->needs_exposure = 1;
	    }
	  break;

	case ExposeCopy:
	  return;
	}
    }
}
#endif /* HAVE_X11 */


/* Draw a hollow box cursor.  Don't change the inside of the box.  */

static void
x_draw_box (s)
     struct screen *s;
{
  int left = s->cursor_x * FONT_WIDTH (s->display.x->font)
    + s->display.x->internal_border_width;
  int top = s->cursor_y * FONT_HEIGHT (s->display.x->font)
    + s->display.x->internal_border_width;
  int width = FONT_WIDTH (s->display.x->font);
  int height = FONT_HEIGHT (s->display.x->font);

#ifdef HAVE_X11
  /* Perhaps we should subtract 1 from width and height... */
  XDrawRectangle (x_current_display, s->display.x->window_desc,
		  s->display.x->cursor_gc,
		  left, top, width - 1, height - 1);
#else
  XPixSet (s->display.x->window_desc,
	   left, top, width, 1,
	   s->display.x->cursor_pixel);

  XPixSet (s->display.x->window_desc,
	   left, top, 1, height,
	   s->display.x->cursor_pixel);

  XPixSet (s->display.x->window_desc,
	   left+width-1, top, 1, height,
	   s->display.x->cursor_pixel);

  XPixSet (s->display.x->window_desc,
	   left, top+height-1, width, 1,
	   s->display.x->cursor_pixel);
#endif /* HAVE_X11 */
}

/* Clear the cursor of screen S to background color,
   and mark the cursor as not shown.
   This is used when the text where the cursor is
   is about to be rewritten.  */

static void
clear_cursor (s)
     struct screen *s;
{
  int mask;

  if (! s->visible
      || s->phys_cursor_x < 0)
    return;

#ifdef HAVE_X11
  x_display_cursor (s, 0);
#if 0
  XClearArea (x_current_display, s->display.x->window_desc,
	      s->phys_cursor_x * FONT_WIDTH (s->display.x->font)
	      + s->display.x->internal_border_width,
	      s->phys_cursor_y * FONT_HEIGHT (s->display.x->font)
	      + s->display.x->internal_border_width,
	      FONT_WIDTH (s->display.x->font) + 1, FONT_HEIGHT (s->display.x->font) + 1, False);
#endif
#else
  XPixSet (s->display.x->window_desc,
	   s->phys_cursor_x * FONT_WIDTH (s->display.x->font) + s->display.x->internal_border_width,
	   s->phys_cursor_y * FONT_HEIGHT (s->display.x->font) + s->display.x->internal_border_width,
	   FONT_WIDTH (s->display.x->font), FONT_HEIGHT (s->display.x->font),
	   s->display.x->background_pixel);
#endif /* HAVE_X11 */
  s->phys_cursor_x = -1;
}

static void
x_display_bar_cursor (s, on)
     struct screen *s;
     int on;
{
  register int phys_x = s->phys_cursor_x;
  register int phys_y = s->phys_cursor_y;
  register int x1;
  register int y1;
  register int y2;

  if (! s->visible || (! on && s->phys_cursor_x < 0))
    return;

#ifdef HAVE_X11
  if (phys_x >= 0 &&
      (!on || phys_x != s->cursor_x || phys_y != s->cursor_y))
    {
      x1 = phys_x * FONT_WIDTH (s->display.x->font)
	+ s->display.x->internal_border_width;
      y1 = phys_y * FONT_HEIGHT (s->display.x->font)
	+ s->display.x->internal_border_width - 1;
      y2 = y1 + FONT_HEIGHT (s->display.x->font) + 1;

      XDrawLine (x_current_display, s->display.x->window_desc,
		 s->display.x->reverse_gc, x1, y1, x1, y2);

      s->phys_cursor_x = phys_x = -1;
    }

  if (on && s == x_highlight_screen)
    {
      x1 = s->cursor_x * FONT_WIDTH (s->display.x->font)
	+ s->display.x->internal_border_width;
      y1 = s->cursor_y * FONT_HEIGHT (s->display.x->font)
	+ s->display.x->internal_border_width - 1;
      y2 = y1 + FONT_HEIGHT (s->display.x->font) + 1;

      XDrawLine (x_current_display, s->display.x->window_desc,
		 s->display.x->cursor_gc, x1, y1, x1, y2);

      s->phys_cursor_x = s->cursor_x;
      s->phys_cursor_y = s->cursor_y;
    }
#else  /* X10 */
  Give it up, dude.
#endif /* X10 */
}


/* Redraw the glyph at ROW, COLUMN on screen S, in the style
   HIGHLIGHT.  HIGHLIGHT is as defined for dumpglyphs.  Return the
   glyph drawn.  */

static void
x_draw_single_glyph (s, row, column, glyph, highlight)
     struct screen *s;
     int row, column;
     GLYPH glyph;
     int highlight;
{
  dumpglyphs (s,
	      (column * FONT_WIDTH (s->display.x->font)
	       + s->display.x->internal_border_width),
	      (row * FONT_HEIGHT (s->display.x->font)
	       + s->display.x->internal_border_width),
	      &glyph, 1, highlight, s->display.x->font);
}

/* Turn the displayed cursor of screen S on or off according to ON.
   If ON is nonzero, where to put the cursor is specified
   by S->cursor_x and S->cursor_y.  */

static void
x_display_box_cursor (s, on)
     struct screen *s;
     int on;
{
  struct screen_glyphs *current_glyphs = SCREEN_CURRENT_GLYPHS (s);

  if (! s->visible)
    return;

  /* If cursor is off and we want it off, return quickly.  */
  if (!on && s->phys_cursor_x < 0)
    return;

  /* If cursor is currently being shown and we don't want it to be
     or it is in the wrong place,
     or we want a hollow box and it's not so, (pout!)
     erase it.  */
  if (s->phys_cursor_x >= 0
      && (!on
	  || s->phys_cursor_x != s->cursor_x
	  || s->phys_cursor_y != s->cursor_y
	  || (s->display.x->text_cursor_kind != hollow_box_cursor
	      && (s != x_highlight_screen))))
    {
      /* Erase the cursor by redrawing the character underneath it.  */
      x_draw_single_glyph (s, s->phys_cursor_y, s->phys_cursor_x,
			   s->phys_cursor_glyph,
			   current_glyphs->highlight[s->phys_cursor_y]);
      s->phys_cursor_x = -1;
    }

  /* If we want to show a cursor,
     or we want a box cursor and it's not so,
     write it in the right place.  */
  if (on
      && (s->phys_cursor_x < 0
	  || (s->display.x->text_cursor_kind != filled_box_cursor
	      && s == x_highlight_screen)))
    {
      s->phys_cursor_glyph
	= ((current_glyphs->enable[s->cursor_y]
	    && s->cursor_x < current_glyphs->used[s->cursor_y])
	   ? current_glyphs->glyphs[s->cursor_y][s->cursor_x]
	   : SPACEGLYPH);
      if (s != x_highlight_screen)
	{
	  x_draw_box (s);
	  s->display.x->text_cursor_kind = hollow_box_cursor;
	}
      else
	{
	  x_draw_single_glyph (s, s->cursor_y, s->cursor_x,
			       s->phys_cursor_glyph, 2);
	  s->display.x->text_cursor_kind = filled_box_cursor;
	}

      s->phys_cursor_x = s->cursor_x;
      s->phys_cursor_y = s->cursor_y;
    }

  if (updating_screen != s)
    XFlushQueue ();
}

extern Lisp_Object Vbar_cursor;

x_display_cursor (s, on)
     struct screen *s;
     int on;
{
  if (EQ (Vbar_cursor, Qnil))
    x_display_box_cursor (s, on);
  else
    x_display_bar_cursor (s, on);
}

/* Icons.  */

/* Refresh bitmap kitchen sink icon for screen S
   when we get an expose event for it. */

refreshicon (s)
     struct screen *s;
{
#ifdef HAVE_X11
  /* Normally, the window manager handles this function. */
#else
  int mask;

  if (s->display.x->icon_bitmap_flag)
    XBitmapBitsPut (s->display.x->icon_desc, 0,  0, sink_width, sink_height,
		    sink_bits, BlackPixel, WHITE_PIX_DEFAULT, 
		    icon_bitmap, GXcopy, AllPlanes);
  else
    {
      extern struct screen *selected_screen;
      struct Lisp_String *str;
      unsigned char *string;

      string
	= XSTRING (XBUFFER (XWINDOW (s->selected_window)->buffer)->name)->data;

      if (s->display.x->icon_label != string)
	{
	  s->display.x->icon_label = string;
	  XChangeWindow (s->display.x->icon_desc,
			 XQueryWidth (string, icon_font_info->id) + 10,
			 icon_font_info->height + 10);
	}

      XText (s->display.x->icon_desc, 5, 5, string,
	     str->size, icon_font_info->id,
	     BLACK_PIX_DEFAULT, WHITE_PIX_DEFAULT);
    }
  XFlushQueue ();
#endif /* HAVE_X11 */
}

/* Make the x-window of screen S use the kitchen-sink icon
   that's a window generated by Emacs.  */

int
x_bitmap_icon (s)
     struct screen *s;
{
  int mask;
  Window icon_window;

  if (s->display.x->window_desc == 0)
    return 1;

#ifdef HAVE_X11
  if (icon_bitmap)
    XFreePixmap (x_current_display, icon_bitmap);
  
  icon_bitmap =
    XCreateBitmapFromData (x_current_display, s->display.x->window_desc,
			   gnu_bits, gnu_width, gnu_height);
  x_wm_set_icon_pixmap (s, icon_bitmap);
  s->display.x->icon_bitmap_flag = 1;
#else
  if (s->display.x->icon_desc)
    {
      XClearIconWindow (s->display.x->window_desc);
      XDestroyWindow (s->display.x->icon_desc);
    }

  icon_window = XCreateWindow (s->display.x->parent_desc,
			       0, 0, sink_width, sink_height,
			       2, WhitePixmap, (Pixmap) NULL);

  if (icon_window == 0)
    return 1;

  XSetIconWindow (s->display.x->window_desc, icon_window);
  XSelectInput (icon_window, ExposeWindow | UnmapWindow);

  s->display.x->icon_desc = icon_window;
  s->display.x->icon_bitmap_flag = 1;

  if (icon_bitmap == 0)
    icon_bitmap
      = XStoreBitmap (sink_mask_width, sink_mask_height, sink_mask_bits);
#endif /* HAVE_X11 */

  return 0;
}


/* Make the x-window of screen S use a rectangle with text.  */

int
x_text_icon (s, icon_name)
     struct screen *s;
     char *icon_name;
{
#ifndef HAVE_X11
  int mask;
  int width;
  Window icon_window;
  char *X_DefaultValue;
  Bitmap b1;

#ifndef WhitePixel
#define WhitePixel 1
#endif

#ifndef BlackPixel
#define BlackPixel 0
#endif
#endif /* not HAVE_X11 */
  
  if (s->display.x->window_desc == 0)
    return 1;

  if (icon_font_info == 0)
    icon_font_info
      = XGetFont (XGetDefault (XDISPLAY
			       (char *) XSTRING (invocation_name)->data,
			       "BodyFont"));

#ifdef HAVE_X11
  if (icon_name)
    s->display.x->icon_label = icon_name;
  else
    if (! s->display.x->icon_label)
      s->display.x->icon_label = " *emacs* ";
  
  XSetIconName (x_current_display, s->display.x->window_desc,
		(char *) s->display.x->icon_label);
  
  s->display.x->icon_bitmap_flag = 0;
#else
  if (s->display.x->icon_desc)
    {
      XClearIconWindow (XDISPLAY s->display.x->window_desc);
      XDestroyWindow (XDISPLAY s->display.x->icon_desc);
    }

  if (icon_name)
    s->display.x->icon_label = (unsigned char *) icon_name;
  else
    if (! s->display.x->icon_label)
      s->display.x->icon_label = XSTRING (s->name)->data;

  width = XStringWidth (s->display.x->icon_label, icon_font_info, 0, 0);
  icon_window = XCreateWindow (s->display.x->parent_desc,
			       s->display.x->left_pos,
			       s->display.x->top_pos,
			       width + 10, icon_font_info->height + 10,
			       2, BlackPixmap, WhitePixmap);

  if (icon_window == 0)
    return 1;

  XSetIconWindow (s->display.x->window_desc, icon_window);
  XSelectInput (icon_window, ExposeWindow | ExposeRegion | UnmapWindow | ButtonPressed);

  s->display.x->icon_desc = icon_window;
  s->display.x->icon_bitmap_flag = 0;
  s->display.x->icon_label = 0;
#endif /* HAVE_X11 */

  return 0;
}

/* Handling X errors.  */

/* A handler for SIGPIPE, when it occurs on the X server's connection.
   This basically does an orderly shutdown of Emacs.  */
static SIGTYPE
x_death_handler ()
{
  if (_Xdebug)
    abort ();
  else
    Fkill_emacs (make_number (70));
}

static char *x_proto_requests[] =
{
  "CreateWindow",
  "ChangeWindowAttributes",
  "GetWindowAttributes",
  "DestroyWindow",
  "DestroySubwindows",
  "ChangeSaveSet",
  "ReparentWindow",
  "MapWindow",
  "MapSubwindows",
  "UnmapWindow",
  "UnmapSubwindows",
  "ConfigureWindow",
  "CirculateWindow",
  "GetGeometry",
  "QueryTree",
  "InternAtom",
  "GetAtomName",
  "ChangeProperty",
  "DeleteProperty",
  "GetProperty",
  "ListProperties",
  "SetSelectionOwner",
  "GetSelectionOwner",
  "ConvertSelection",
  "SendEvent",
  "GrabPointer",
  "UngrabPointer",
  "GrabButton",
  "UngrabButton",
  "ChangeActivePointerGrab",
  "GrabKeyboard",
  "UngrabKeyboard",
  "GrabKey",
  "UngrabKey",
  "AllowEvents",
  "GrabServer",
  "UngrabServer",
  "QueryPointer",
  "GetMotionEvents",
  "TranslateCoords",
  "WarpPointer",
  "SetInputFocus",
  "GetInputFocus",
  "QueryKeymap",
  "OpenFont",
  "CloseFont",
  "QueryFont",
  "QueryTextExtents",
  "ListFonts",
  "ListFontsWithInfo",
  "SetFontPath",
  "GetFontPath",
  "CreatePixmap",
  "FreePixmap",
  "CreateGC",
  "ChangeGC",
  "CopyGC",
  "SetDashes",
  "SetClipRectangles",
  "FreeGC",
  "ClearArea",
  "CopyArea",
  "CopyPlane",
  "PolyPoint",
  "PolyLine",
  "PolySegment",
  "PolyRectangle",
  "PolyArc",
  "FillPoly",
  "PolyFillRectangle",
  "PolyFillArc",
  "PutImage",
  "GetImage",
  "PolyText",
  "PolyText",
  "ImageText",
  "ImageText",
  "CreateColormap",
  "FreeColormap",
  "CopyColormapAndFree",
  "InstallColormap",
  "UninstallColormap",
  "ListInstalledColormaps",
  "AllocColor",
  "AllocNamedColor",
  "AllocColorCells",
  "AllocColorPlanes",
  "FreeColors",
  "StoreColors",
  "StoreNamedColor",
  "QueryColors",
  "LookupColor",
  "CreateCursor",
  "CreateGlyphCursor",
  "FreeCursor",
  "RecolorCursor",
  "QueryBestSize",
  "QueryExtension",
  "ListExtensions",
  "ChangeKeyboardMapping",
  "GetKeyboardMapping",
  "ChangeKeyboardControl",
  "GetKeyboardControl",
  "Bell",
  "ChangePointerControl",
  "GetPointerControl",
  "SetScreenSaver",
  "GetScreenSaver",
  "ChangeHosts",
  "ListHosts",
  "SetAccessControl",
  "SetCloseDownMode",
  "KillClient",
  "RotateProperties",
  "ForceScreenSaver",
  "SetPointerMapping",
  "GetPointerMapping",
  "SetModifierMapping",
  "GetModifierMapping",
  "NoOperation"
};

#define acceptable_x_error_p(type) ((type) == 94)

x_handle_error_gracefully (event)
     XErrorEvent *event;
{
  char error_ptr[128];
  char *proto_ptr = x_proto_requests[event->request_code];
  char str[128];

  XGetErrorText (x_current_display, event->error_code, error_ptr, 128);
  sprintf (str, "X Protocol Error: %s on request: %s", error_ptr, proto_ptr);
  TOTALLY_UNBLOCK_INPUT;
  error (str);
}

#if 0
extern int x_selection_alloc_error;
extern int x_converting_selection;
#endif

/* Handle X Errors.  If the error is not traumatic,
   just call error ().  Otherwise print a (hopefully) interesting
   message and quit.

   The arg to Fkill_emacs is an exit status value
   and also prevents any questions.  */

x_error_handler (disp, event)
     Display *disp;
#ifdef HAVE_X11
     XErrorEvent *event;

#define XlibDisplayIOError	(1L << 0)

#else
     struct _XErrorEvent *event;
#endif
{
  /* Here we use the standard X handlers. */

  BLOCK_INPUT;
  if (event && event->type == 0) /* 0 is the XError Event type. */
    {
#if 0
#ifdef HAVE_X11
      if (event->request_code == BadAlloc && x_converting_selection)
	x_selection_alloc_error = 1;
      else
#endif
#endif
      if (acceptable_x_error_p (event->request_code))
	x_handle_error_gracefully (event);
      else
	_XDefaultError (disp, event);
    }
  else
    {
      disp->flags |= XlibDisplayIOError;
      _XDefaultIOError (disp);
    }
  UNBLOCK_INPUT;

  x_death_handler ();
}

#if 0
static unsigned int x_wire_count;
x_trace_wire ()
{
  fprintf (stderr, "Lib call: %d\n", ++x_wire_count);
}
#endif


/* Set the font of the x-window specified by screen S
   to the font named NEWNAME.  This is safe to use
   even before S has an actual x-window.  */

#ifdef HAVE_X11

/* A table of all the fonts we have already loaded.  */
static XFontStruct **x_font_table;

/* The current capacity of x_font_table.  */
static int x_font_table_size;

/* The number of fonts actually stored in x_font_table.
   x_font_table[n] is used and valid iff 0 <= n < n_fonts.
   0 <= n_fonts <= x_font_table_size.  */
static int n_fonts;

x_new_font (s, fontname)
     struct screen *s;
     register char *fontname;
{
  XFontStruct *temp;
  int already_loaded;
  int n_matching_fonts;
  XFontStruct *font_info;
  char **font_names;

  /* Get a list of all the fonts that match this name.  Once we
     have a list of matching fonts, we compare them against the fonts
     we already have by comparing font ids.  */
  font_names = (char **) XListFontsWithInfo (x_current_display, fontname,
					     1024, &n_matching_fonts,
					     &font_info);
  /* If the server couldn't find any fonts whose named matched fontname,
     return an error code.  */
  if (n_matching_fonts == 0)
    return 1;

  /* See if we've already loaded a matching font. */
  {
    int i, j;

    already_loaded = 0;
    for (i = 0; i < n_fonts; i++)
      for (j = 0; j < n_matching_fonts; j++)
	if (x_font_table[i]->fid == font_info[j].fid)
	  {
	    already_loaded = i;
	    goto found_font;
	  }
  }
 found_font:
  
  /* If we have, just return it from the table.  */
  if (already_loaded)
    s->display.x->font = x_font_table[already_loaded];
  
  /* Otherwise, load the font and add it to the table.  */
  else
    {
      XFontStruct *font;

      font = (XFontStruct *) XLoadQueryFont (x_current_display, fontname);
      if (! font)
	return 1;

      /* Do we need to create the table?  */
      if (x_font_table_size == 0)
	{
	  x_font_table_size = 16;
	  x_font_table
	    = (XFontStruct **) xmalloc (x_font_table_size
					* sizeof (x_font_table[0]));
	}
      /* Do we need to grow the table?  */
      else if (n_fonts >= x_font_table_size)
	{
	  x_font_table_size *= 2;
	  x_font_table
	    = (XFontStruct **) xrealloc (x_font_table,
					 (x_font_table_size
					  * sizeof (x_font_table[0])));
	}

      s->display.x->font = x_font_table[n_fonts++] = font;
    }
  
  /* Free the information from XListFontsWithInfo.  The data
     we actually retain comes from XLoadQueryFont.  */
  XFreeFontInfo (font_names, font_info, n_matching_fonts);

  /* Now make the screen display the given font.  */
  if (s->display.x->window_desc != 0)
    {
      XSetFont (x_current_display, s->display.x->normal_gc,
		s->display.x->font->fid);
      XSetFont (x_current_display, s->display.x->reverse_gc,
		s->display.x->font->fid);
      XSetFont (x_current_display, s->display.x->cursor_gc,
		s->display.x->font->fid);

      x_set_window_size (s, s->width, s->height);
    }

  return 0;
}
#else
x_new_font (s, newname)
     struct screen *s;
     register char *newname;
{
  FONT_TYPE *temp;
  int mask;

  temp = XGetFont (newname);
  if (temp == (FONT_TYPE *) 0)
    return 1;

  if (s->display.x->font)
    XLoseFont (s->display.x->font);

  s->display.x->font = temp;

  if (s->display.x->window_desc != 0)
    x_set_window_size (s, s->width, s->height);

  return 0;
}
#endif

x_calc_absolute_position (s)
     struct screen *s;
{
#ifdef HAVE_X11
  if (s->display.x->left_pos < 0)
    s->display.x->left_pos
      = XINT (x_screen_width) - PIXEL_WIDTH (s) + s->display.x->left_pos;

  if (s->display.x->top_pos < 0)
    s->display.x->top_pos
      = XINT (x_screen_height) - PIXEL_HEIGHT (s) + s->display.x->top_pos;
#else /* X10 */
  WINDOWINFO_TYPE parentinfo;

  XGetWindowInfo (s->display.x->window_desc, &parentinfo);

  if (s->display.x->left_pos < 0)
    s->display.x->left_pos = parentinfo.width + (s->display.x->left_pos + 1)
      - PIXEL_WIDTH (s) - 2 * s->display.x->internal_border_width;

  if (s->display.x->top_pos < 0)
    s->display.x->top_pos = parentinfo.height + (s->display.x->top_pos + 1)
      - PIXEL_HEIGHT (s) - 2 * s->display.x->internal_border_width;
#endif /* X10 */
}

x_set_offset (s, xoff, yoff)
     struct screen *s;
     register int xoff, yoff;
{
  s->display.x->top_pos = yoff;
  s->display.x->left_pos = xoff;
  x_calc_absolute_position (s);

  BLOCK_INPUT;
  XMoveWindow (XDISPLAY s->display.x->window_desc,
	       s->display.x->left_pos, s->display.x->top_pos);
#ifdef HAVE_X11
  x_wm_set_size_hint (s, 0);
#endif
  UNBLOCK_INPUT;
}

/* Call this to change the size of screen S's x-window. */

x_set_window_size (s, cols, rows)
     struct screen *s;
     register int cols, rows;
{
  int pixelwidth, pixelheight;
  int mask;
  int ibw = s->display.x->internal_border_width;

  BLOCK_INPUT;

  /* ??? Who DOES worry about minimum reasonable sizes?  */
  pixelwidth =  (cols * FONT_WIDTH (s->display.x->font) + 2 * ibw
		 + s->display.x->v_scrollbar_width);
  pixelheight = (rows * FONT_HEIGHT (s->display.x->font) + 2 * ibw
		 + s->display.x->h_scrollbar_height);

#ifdef HAVE_X11
  x_wm_set_size_hint (s, 0);
#endif /* HAVE_X11 */
  XChangeWindowSize (s->display.x->window_desc, pixelwidth, pixelheight);
  XFlushQueue ();
  UNBLOCK_INPUT;
}

#ifndef HAVE_X11
x_set_resize_hint (s)
     struct screen *s;
{

  XSetResizeHint (s->display.x->window_desc, 2 * s->display.x->internal_border_width,
		  2 * s->display.x->internal_border_width,
		  FONT_WIDTH (s->display.x->font), FONT_HEIGHT (s->display.x->font));
}
#endif /* not HAVE_X11 */


x_set_mouse_position (s, x, y)
     struct screen *s;
     int x, y;
{
  int pix_x, pix_y;

  x_raise_screen (s);

  if (x < 0)
    pix_x = (SCREEN_WIDTH (s)
             * FONT_WIDTH (s->display.x->font)
             + 2 * s->display.x->internal_border_width
             + s->display.x->v_scrollbar_width) / 2;
  else
    pix_x = x * FONT_WIDTH (s->display.x->font) + 2; /* add 2 pixels to each
       						 dimension to move the
       						 mouse into the char
       						 cell */

  if (y < 0)
    pix_y = (SCREEN_HEIGHT (s)
             * FONT_HEIGHT (s->display.x->font)
             + 2 * s->display.x->internal_border_width
             + s->display.x->h_scrollbar_height) / 2;
  else
    pix_y = y * FONT_HEIGHT (s->display.x->font) + 2;

  BLOCK_INPUT;
  x_mouse_x = x;
  x_mouse_y = y;

  XWarpMousePointer (s->display.x->window_desc, pix_x, pix_y);
  UNBLOCK_INPUT;
}

#ifdef HAVE_X11
x_focus_on_screen (s)
     struct screen *s;
{
  x_raise_screen (s);
#if 0
  /* I don't think that the ICCCM allows programs to do things like this
     without the interaction of the window manager.  Whatever you end up
     doing with this code, do it to x_unfocus_screen too.  */
  XSetInputFocus (x_current_display, s->display.x->window_desc,
		  RevertToPointerRoot, CurrentTime);
#endif
}

x_unfocus_screen (s)
     struct screen *s;
{
#if 0
  /* Look at the remarks in x_focus_on_screen.  */
  if (x_focus_screen == s)
    XSetInputFocus (x_current_display, PointerRoot,
		    RevertToPointerRoot, CurrentTime);
#endif
}

#endif

/* Raise screen S.  */

x_raise_screen (s)
     struct screen *s;
{
  if (s->visible)
    {
      BLOCK_INPUT;
      XRaiseWindow (XDISPLAY s->display.x->window_desc);
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Lower screen S.  */

x_lower_screen (s)
     struct screen *s;
{
  if (s->visible)
    {
      BLOCK_INPUT;
      XLowerWindow (XDISPLAY s->display.x->window_desc);
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Change from withdrawn state to mapped state. */

x_make_screen_visible (s)
     struct screen *s;
{
  int mask;

  BLOCK_INPUT;

  if (! SCREEN_VISIBLE_P (s))
    {
#ifdef HAVE_X11
      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (s, NormalState);

      XMapWindow (XDISPLAY s->display.x->window_desc);
      if (s->display.x->v_scrollbar != 0 || s->display.x->h_scrollbar != 0)
	XMapSubwindows (x_current_display, s->display.x->window_desc);
#else
      XMapWindow (XDISPLAY s->display.x->window_desc);
      if (s->display.x->icon_desc != 0)
	XUnmapWindow (s->display.x->icon_desc);

      /* Handled by the MapNotify event for X11 */
      s->visible = 1;
      s->iconified = 0;

      /* NOTE: this may cause problems for the first screen. */
      XTcursor_to (0, 0);
#endif				/* not HAVE_X11 */
    }

  XFlushQueue ();

  UNBLOCK_INPUT;
}

/* Change from mapped state to withdrawn state. */

x_make_screen_invisible (s)
     struct screen *s;
{
  int mask;

  if (! s->visible)
    return;

  BLOCK_INPUT;
#ifdef HAVE_X11
#if 0
  if (! EQ (Vx_no_window_manager, Qt))
    {
      XUnmapEvent unmap;

      unmap.type = UnmapNotify;
      unmap.window = s->display.x->window_desc;
      unmap.event = DefaultRootWindow (x_current_display);
      unmap.from_configure = False;
      XSendEvent (x_current_display, DefaultRootWindow (x_current_display),
		  False, SubstructureRedirectMask|SubstructureNotifyMask,
		  &unmap);
    }

  /* The new function below does the same as the above code, plus unmapping
     the window.  Sending the event without actually unmapping can make
     the window manager start ignoring the window (i.e., no more title bar,
     icon manager stuff.) */
#endif

  /* New function available with R4 */
  if (! XWithdrawWindow (x_current_display, s->display.x->window_desc,
			 DefaultScreen (x_current_display)))
    {
      UNBLOCK_INPUT_RESIGNAL;
      error ("Can't notify window manager of iconification.");
    }

#else
  XUnmapWindow (XDISPLAY s->display.x->window_desc);

  s->visible = 0;		/* Handled by the UnMap event for X11 */
  if (s->display.x->icon_desc != 0)
    XUnmapWindow (XDISPLAY s->display.x->icon_desc);
#endif /* not HAVE_X11 */

  XFlushQueue ();
  UNBLOCK_INPUT;
}

 /* Window manager communication.  Created in Fx_open_connection. */
extern Atom Xatom_wm_change_state;

/* Change window state from mapped to iconified. */

x_iconify_screen (s)
     struct screen *s;
{
  int mask;

  if (s->iconified)
    return;

  BLOCK_INPUT;

#ifdef HAVE_X11
  if (! EQ (Vx_no_window_manager, Qt))
    if (! XIconifyWindow (x_current_display, s->display.x->window_desc,
			  DefaultScreen (x_current_display)))
      {
	UNBLOCK_INPUT_RESIGNAL;
	error ("Can't notify window manager of iconification.");
      }

  s->iconified = 1;
  
#if 0
    {
      XClientMessageEvent message;
    
      message.window = s->display.x->window_desc;
      message.type = ClientMessage;
      message.message_type = Xatom_wm_change_state;
      message.format = 32;
      message.data.l[0] = IconicState;

      if (! XSendEvent (x_current_display,
			DefaultRootWindow (x_current_display),
			False,
			SubstructureRedirectMask | SubstructureNotifyMask,
			&message))
	{
	  UNBLOCK_INPUT_RESIGNAL;
	  error ("Can't notify window manager of iconification.");
	}
    }
#endif
#else /* X10 */
  XUnmapWindow (XDISPLAY s->display.x->window_desc);

  s->visible = 0;		/* Handled in the UnMap event for X11. */
  if (s->display.x->icon_desc != 0)
    {
      XMapWindow (XDISPLAY s->display.x->icon_desc);
      refreshicon (s);
    }
#endif /* X10 */

  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Destroy the X window of screen S.
   DISPL is the former s->display (since s->display
   has already been nulled out).  */

x_destroy_window (s, displ)
     struct screen *s;
     union display displ;
{
  int mask;

  BLOCK_INPUT;
  if (displ.x->icon_desc != 0)
    XDestroyWindow (XDISPLAY displ.x->icon_desc);
  XDestroyWindow (XDISPLAY displ.x->window_desc);
  XFlushQueue ();
  UNBLOCK_INPUT;

  free (displ.x);
  if (s == x_focus_screen)
    x_focus_screen = 0;
  if (s == x_highlight_screen)
    x_highlight_screen = 0;
}

#ifndef HAVE_X11

/* Manage event queues.

   This code is only used by the X10 support.

   We cannot leave events in the X queue and get them when we are ready
   because X does not provide a subroutine to get only a certain kind
   of event but not block if there are no queued events of that kind.

   Therefore, we must examine events as they come in and copy events
   of certain kinds into our private queues.

   All ExposeRegion events are put in x_expose_queue.
   All ButtonPressed and ButtonReleased events are put in x_mouse_queue.  */


/* Write the event *P_XREP into the event queue *QUEUE.
   If the queue is full, do nothing, but return nonzero.  */

int
enqueue_event (p_xrep, queue)
     register XEvent *p_xrep;
     register struct event_queue *queue;
{
  int newindex = queue->windex + 1;
  if (newindex == EVENT_BUFFER_SIZE)
    newindex = 0;
  if (newindex == queue->rindex)
    return -1;
  queue->xrep[queue->windex] = *p_xrep;
  queue->windex = newindex;
  return 0;
}

/* Fetch the next event from queue *QUEUE and store it in *P_XREP.
   If *QUEUE is empty, do nothing and return 0.  */

int
dequeue_event (p_xrep, queue)
     register XEvent *p_xrep;
     register struct event_queue *queue;
{
  if (queue->windex == queue->rindex)
    return 0;
  *p_xrep = queue->xrep[queue->rindex++];
  if (queue->rindex == EVENT_BUFFER_SIZE)
    queue->rindex = 0;
  return 1;
}

/* Return the number of events buffered in *QUEUE.  */

int
queue_event_count (queue)
     register struct event_queue *queue;
{
  int tem = queue->windex - queue->rindex;
  if (tem >= 0)
    return tem;
  return EVENT_BUFFER_SIZE + tem;
}

/* Return nonzero if mouse input is pending.  */

int
mouse_event_pending_p ()
{
  return queue_event_count (&x_mouse_queue);
}
#endif

#ifdef HAVE_X11

x_wm_set_size_hint (s, prompting)
     struct screen *s;
     long prompting;
{
  XSizeHints size_hints;
  Window window = s->display.x->window_desc;

  size_hints.flags = PResizeInc | PMinSize | PMaxSize;

  flexlines = s->height;

  size_hints.x = s->display.x->left_pos;
  size_hints.y = s->display.x->top_pos;
  size_hints.height = PIXEL_HEIGHT (s);
  size_hints.width = PIXEL_WIDTH (s);
  size_hints.width_inc = FONT_WIDTH (s->display.x->font);
  size_hints.height_inc = FONT_HEIGHT (s->display.x->font);
  size_hints.base_width = (2 * s->display.x->internal_border_width)
    + s->display.x->v_scrollbar_width;
  size_hints.base_height = (2 * s->display.x->internal_border_width)
    + s->display.x->h_scrollbar_height;
  size_hints.min_width = size_hints.base_width + size_hints.width_inc;
  size_hints.min_height = size_hints.base_height + size_hints.height_inc;
  size_hints.max_width = x_screen_width
    - ((2 * s->display.x->internal_border_width)
       + s->display.x->v_scrollbar_width);
  size_hints.max_height = x_screen_height
    - ((2 * s->display.x->internal_border_width)
       + s->display.x->h_scrollbar_height);

  if (prompting)
    size_hints.flags |= prompting;
  else
    {
      XSizeHints hints;		/* Sometimes I hate X Windows... */
      
      XGetNormalHints (x_current_display, window, &hints);
      if (hints.flags & PSize)
	size_hints.flags |= PSize;
      if (hints.flags & PPosition)
	size_hints.flags |= PPosition;
      if (hints.flags & USPosition)
	size_hints.flags |= USPosition;
      if (hints.flags & USSize)
	size_hints.flags |= USSize;
    }
  
#if 0				/* R3 */
  XSetNormalHints (x_current_display, window, &size_hints);
#endif
  XSetWMNormalHints (x_current_display, window, &size_hints);
}

/* Used for IconicState or NormalState */
x_wm_set_window_state (s, state)
     struct screen *s;
     int state;
{
  XWMHints wm_hints;
  Window window = s->display.x->window_desc;

  wm_hints.flags = StateHint;
  wm_hints.initial_state = state;
  XSetWMHints (x_current_display, window, &wm_hints);
}

x_wm_set_icon_pixmap (s, icon_pixmap)
     struct screen *s;
     Pixmap icon_pixmap;
{
  XWMHints wm_hints;
  Window window = s->display.x->window_desc;

  wm_hints.flags = IconPixmapHint;
  wm_hints.icon_pixmap = icon_pixmap;
  XSetWMHints (x_current_display, window, &wm_hints);
}

x_wm_set_icon_position (s, icon_x, icon_y)
     struct screen *s;
     int icon_x, icon_y;
{
  XWMHints wm_hints;
  Window window = s->display.x->window_desc;

  wm_hints.flags = IconPositionHint;
  wm_hints.icon_x = icon_x;
  wm_hints.icon_y = icon_y;
  XSetWMHints (x_current_display, window, &wm_hints);
}


void
x_term_init (display_name)
     char *display_name;
{
  Lisp_Object screen;
  char *defaultvalue;
#ifdef F_SETOWN
  extern int old_fcntl_owner;
#endif
  
  x_focus_screen = x_highlight_screen = 0;

  x_current_display = XOpenDisplay (display_name);
  if (x_current_display == 0)
    fatal ("X server %s not responding; check the DISPLAY environment variable or use \"-d\"\n",
	   display_name);

#ifdef HAVE_X11
  {
    int hostname_size = MAXHOSTNAMELEN + 1;

    hostname = (char *) xmalloc (hostname_size);

#if 0
    XSetAfterFunction (x_current_display, x_trace_wire);
#endif

    invocation_name = Ffile_name_nondirectory (Fcar (Vcommand_line_args));

    /* Try to get the host name; if the buffer is too short, try
       again.  Apparently, the only indication gethostname gives of
       whether the buffer was large enough is the presence or absence
       of a '\0' in the string.  Eech.  */
    for (;;)
      {
	gethostname (hostname, hostname_size - 1);
	hostname[hostname_size - 1] = '\0';

	/* Was the buffer large enough for gethostname to store the '\0'?  */
	if (strlen (hostname) < hostname_size - 1)
	  break;

	hostname_size <<= 1;
	hostname = (char *) xrealloc (hostname, hostname_size);
      }
    x_id_name = (char *) xmalloc (XSTRING (invocation_name)->size
				+ strlen (hostname)
				+ 2);
    sprintf (x_id_name, "%s@%s", XSTRING (invocation_name)->data, hostname);
  }
  
  dup2 (ConnectionNumber (x_current_display), 0);

#ifndef SYSV_STREAMS
  /* Streams somehow keeps track of which descriptor number
     is being used to talk to X.  So it is not safe to substitute
     descriptor 0.  But it is safe to make descriptor 0 a copy of it.  */
  close (ConnectionNumber (x_current_display));
  ConnectionNumber (x_current_display) = 0;	/* Looks a little strange?
						 * check the def of the macro;
						 * it is a genuine lvalue */
#endif /* not SYSV_STREAMS */

#endif /* HAVE_X11 */
  
#ifdef F_SETOWN
  old_fcntl_owner = fcntl (0, F_GETOWN, 0);
#ifdef F_SETOWN_SOCK_NEG
  fcntl (0, F_SETOWN, -getpid ());	/* stdin is a socket here */
#else
  fcntl (0, F_SETOWN, getpid ());
#endif /* F_SETOWN_SOCK_NEG */
#endif /* F_SETOWN */

#ifdef SIGIO
  init_sigio ();
#endif

  /* Must use interrupt input because we cannot otherwise
     arrange for C-g to be noticed immediately.
     We cannot connect it to SIGINT.  */
  Fset_input_mode (Qt, Qnil, Qt, Qnil);

  expose_all_windows = 0;

  clear_screen_hook = XTclear_screen;
  clear_end_of_line_hook = XTclear_end_of_line;
  ins_del_lines_hook = XTins_del_lines;
  change_line_highlight_hook = XTchange_line_highlight;
  insert_glyphs_hook = XTinsert_glyphs;
  write_glyphs_hook = XTwrite_glyphs;
  delete_glyphs_hook = XTdelete_glyphs;
  ring_bell_hook = XTring_bell;
  reset_terminal_modes_hook = XTreset_terminal_modes;
  set_terminal_modes_hook = XTset_terminal_modes;
  update_begin_hook = XTupdate_begin;
  update_end_hook = XTupdate_end;
  set_terminal_window_hook = XTset_terminal_window;
  read_socket_hook = XTread_socket;
  cursor_to_hook = XTcursor_to;
  reassert_line_highlight_hook = XTreassert_line_highlight;
  screen_rehighlight_hook = XTscreen_rehighlight;
  mouse_position_hook = XTmouse_position;
  
  scroll_region_ok = 1;		/* we'll scroll partial screens */
  char_ins_del_ok = 0;		/* just as fast to write the line */
  line_ins_del_ok = 1;		/* we'll just blt 'em */
  fast_clear_end_of_line = 1;	/* X does this well */
  memory_below_screen = 0;	/* we don't remember what scrolls 
				   off the bottom */
  baud_rate = 19200;

  XHandleError (x_error_handler);
  XHandleIOError (x_error_handler);

  /* Disable Window Change signals;  they are handled by X events. */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_DFL);
#endif /* SIGWINCH */

  signal (SIGPIPE, x_death_handler);
}

void
syms_of_xterm ()
{
  staticpro (&invocation_name);
  invocation_name = Qnil;
}
#endif /* HAVE_X11 */
#endif /* HAVE_X_WINDOWS */
