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

/* On 4.3 these lose if they come after xterm.h.  */
#include <stdio.h>
#include <signal.h>

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"

#ifndef USG
/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif /* makedev */
#endif /* USG */

#ifdef BSD
#include <sys/ioctl.h>
#include <strings.h>
#else /* ! defined (BSD) */
#include <sys/termio.h>
#include <string.h>
#endif /* ! defined (BSD) */

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif /* ! defined (BROKEN_FIONREAD) */

/* We are unable to use interrupts if FIONREAD is not available,
   so flush SIGIO so we won't try.  */
#ifndef FIONREAD
#ifdef SIGIO
#undef SIGIO
#endif /* ! defined (SIGIO) */
#endif /* FIONREAD */

#include "systime.h"

#include <fcntl.h>
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
#endif /* ! 0 */
#include "gnu.h"
#include "frame.h"
#include "disptab.h"
#include "buffer.h"

#ifdef HAVE_X11
#define XMapWindow XMapRaised		/* Raise them when mapping. */
#else /* ! defined (HAVE_X11) */
#include <X/Xkeyboard.h>
/*#include <X/Xproto.h>	*/
#endif /* ! defined (HAVE_X11) */

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
#endif /* HAVE_X11 */

/* Nonzero after BLOCK_INPUT; prevents input events from being
   processed until later.  */

int x_input_blocked;

#if defined (SIGIO) && defined (FIONREAD)
int BLOCK_INPUT_mask;
#endif /* ! defined (SIGIO) && defined (FIONREAD) */

/* Nonzero if input events came in while x_input_blocked was nonzero.
   UNBLOCK_INPUT checks for this.  */

int x_pending_input;

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

/* Frame being updated by update_frame.  */
/* This is set by XTupdate_begin and looked at by all the
   XT functions.  It is zero while not inside an update.
   In that case, the XT functions assume that `selected_frame'
   is the frame to apply to.  */

static struct frame *updating_frame;

/* The frame (if any) which has the X window that has keyboard focus.
   Zero if none.  This is examined by Ffocus_frame in frame.c.  */
struct frame *x_focus_frame;

/* The frame which currently has the visual highlight, and should get
   keyboard input (other sorts of input have the frame encoded in the
   event).  It points to the X focus frame's selected window's
   frame.  It differs from x_focus_frame when we're using a global
   minibuffer.  */
static struct frame *x_highlight_frame;

/* From .Xdefaults, the value of "emacs.WarpMouse".  If non-zero,
   mouse is moved to inside of frame when frame is de-iconified.  */

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

#else /* ! defined (HAVE_X11) */

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
#endif /* ! defined (HAVE_X11) */

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
#endif /* HAVE_X11 */

void dumpborder ();
static int XTcursor_to ();
static int XTclear_end_of_line ();


/* These hooks are called by update_frame at the beginning and end
   of a frame update.  We record in `updating_frame' the identity
   of the frame being updated, so that the XT... functions do not
   need to take a frame as argument.  Most of the XT... functions
   should never be called except during an update, the only exceptions
   being XTcursor_to, XTwrite_char and XTreassert_line_highlight.  */

extern int mouse_track_top, mouse_track_left, mouse_track_width;

static
XTupdate_begin (f)
     struct frame *f;
{	
  int mask;

  if (f == 0)
    abort ();

  updating_frame = f;
  flexlines = f->height;
  highlight = 0;

  BLOCK_INPUT;
#ifndef HAVE_X11
  dumpqueue ();
#endif /* HAVE_X11 */
  UNBLOCK_INPUT;
}

static void x_do_pending_expose ();

static
XTupdate_end (f)
     struct frame *f;
{	
  int mask;

  if (updating_frame == 0
      || updating_frame != f)
    abort ();

  BLOCK_INPUT;
#ifndef HAVE_X11
  dumpqueue ();
#endif /* HAVE_X11 */
  adjust_scrollbars (f);
  x_do_pending_expose ();

  x_display_cursor (f, 1);

  updating_frame = 0;
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
  XTclear_end_of_line (updating_frame->width);
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
/*  XTclear_frame ();  */
}

/* Set the nominal cursor position of the frame:
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

  if (updating_frame == 0)
    {
      BLOCK_INPUT;
      x_display_cursor (selected_frame, 1);
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Display a sequence of N glyphs found at GP.
   WINDOW is the x-window to output to.  LEFT and TOP are starting coords.
   HL is 1 if this text is highlighted, 2 if the cursor is on it.

   FONT is the default font to use (for glyphs whose font-code is 0).  */

static void
dumpglyphs (f, left, top, gp, n, hl, font)
     struct frame *f;
     int left, top;
     register GLYPH *gp; /* Points to first GLYPH. */
     register int n;  /* Number of glyphs to display. */
     int hl;
     FONT_TYPE *font;
{
  register int len;
  Window window = FRAME_X_WINDOW (f);
  GC drawing_gc =   (hl == 2 ? f->display.x->cursor_gc
		             : (hl ? f->display.x->reverse_gc
				   : f->display.x->normal_gc));

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
dumpglyphs (f, left, top, gp, n, hl, font)
     struct frame *f;
     int left, top;
     register GLYPH *gp; /* Points to first GLYPH. */
     register int n;  /* Number of glyphs to display. */
     int hl;
     FONT_TYPE *font;
{
  char buf[f->width]; /* Holds characters to be displayed. */
  register char *cp;		/* Steps through buf[]. */
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;
  Window window = FRAME_X_WINDOW (f);
  int cursor_pixel = f->display.x->cursor_pixel;
  int fg_pixel = f->display.x->foreground_pixel;
  int bg_pixel = f->display.x->background_pixel;
  int intborder = f->display.x->internal_border_width;

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
	  GC GC_cursor = f->display.x->cursor_gc;
	  GC GC_reverse = f->display.x->reverse_gc;
	  GC GC_normal = f->display.x->normal_gc;

	  XDrawImageString (x_current_display, window,
			    (hl == 2
			     ? GC_cursor
			     : (hl ? GC_reverse : GC_normal)),
			    left, top + FONT_BASE (font), buf, len);
#else /* ! defined (HAVE_X11) */
	  XText (window, left, top,
		 buf,
		 len,
		 font->id,
		 (hl == 2
		  ? (cursor_pixel == fg_pixel ? bg_pixel : fg_pixel)
		  : hl ? bg_pixel : fg_pixel),
		 (hl == 2 ? cursor_pixel
		  : hl ? fg_pixel : bg_pixel));
#endif /* ! defined (HAVE_X11) */
	}
      else
	{
#ifdef HAVE_X11
	  if (FACE_IS_FONT (cf))
	    XDrawImageString (x_current_display, FRAME_X_WINDOW (f),
			      FACE_GC (cf),
			      left, top + FONT_BASE (FACE_FONT (cf)),
			      buf, len);
	  else if (FACE_IS_IMAGE (cf))
	    XCopyPlane (x_current_display, FACE_IMAGE (cf),
			FRAME_X_WINDOW (f),
			f->display.x->normal_gc,
			0, 0,
			FACE_IMAGE_WIDTH (cf),
			FACE_IMAGE_HEIGHT (cf), left, top);
	  else
	    abort ();
#else /* ! defined (HAVE_X11) */
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
#endif /* ! defined (HAVE_X11) */
	}
      left += len * FONT_WIDTH (font);
    }
}
#endif /* ! 0 */

/* Output some text at the nominal frame cursor position,
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
  struct frame *f;

  BLOCK_INPUT;

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
	     (curs_x * FONT_WIDTH (f->display.x->font)
	      + f->display.x->internal_border_width),
	     (curs_y * FONT_HEIGHT (f->display.x->font)
	      + f->display.x->internal_border_width),
	     start, len, highlight, f->display.x->font);

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

/* Erase the current text line from the nominal cursor position (inclusive)
   to column FIRST_UNUSED (exclusive).  The idea is that everything
   from FIRST_UNUSED onward is already erased.  */
  
static int
XTclear_end_of_line (first_unused)
     register int first_unused;
{
  struct frame *f = updating_frame;
  int mask;

  if (f == 0)
    abort ();

  if (curs_y < 0 || curs_y >= f->height)
    return;
  if (first_unused <= 0)
    return;

  if (first_unused >= f->width)
    first_unused = f->width;

  BLOCK_INPUT;

  /* Notice if the cursor will be cleared by this operation.  */
  if (curs_y == f->phys_cursor_y
      && curs_x <= f->phys_cursor_x
      && f->phys_cursor_x < first_unused)
    f->phys_cursor_x = -1;

#ifdef HAVE_X11
  XClearArea (x_current_display, FRAME_X_WINDOW (f),
	      curs_x * FONT_WIDTH (f->display.x->font)
	      + f->display.x->internal_border_width,
	      curs_y * FONT_HEIGHT (f->display.x->font)
	      + f->display.x->internal_border_width,
	      FONT_WIDTH (f->display.x->font) * (first_unused - curs_x),
	      FONT_HEIGHT (f->display.x->font), False);
	      
#else /* ! defined (HAVE_X11) */
  XPixSet (FRAME_X_WINDOW (f),
	   curs_x * FONT_WIDTH (f->display.x->font) + f->display.x->internal_border_width,
	   curs_y * FONT_HEIGHT (f->display.x->font) + f->display.x->internal_border_width,
	   FONT_WIDTH (f->display.x->font) * (first_unused - curs_x),
	   FONT_HEIGHT (f->display.x->font),
	   f->display.x->background_pixel);	
#endif /* ! defined (HAVE_X11) */

  UNBLOCK_INPUT;
}

static
XTclear_frame ()
{
  int mask;
  struct frame *f = updating_frame;

  if (f == 0)
    f = selected_frame;

  f->phys_cursor_x = -1;	/* Cursor not visible.  */
  curs_x = 0;			/* Nominal cursor position is top left.  */
  curs_y = 0;
  
  BLOCK_INPUT;
  XClear (FRAME_X_WINDOW (f));
#ifndef HAVE_X11
  dumpborder (f, 0);
#endif /* HAVE_X11 */
  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Paint horzontal bars down the frame for a visible bell.
   Note that this may be way too slow on some machines. */

XTflash (f)
     struct frame *f;
{
  register struct frame_glyphs *active_frame = FRAME_CURRENT_GLYPHS (f);
  register int i;
  int x, y;

  if (updating_frame != 0)
    abort ();

  BLOCK_INPUT;
#ifdef HAVE_X11
#if 0
  for (i = f->height * FONT_HEIGHT (f->display.x->font) - 10;
       i >= 0;    
       i -= 100)	   /* Should be NO LOWER than 75 for speed reasons. */
    XFillRectangle (x_current_display, FRAME_X_WINDOW (f),
		    f->display.x->cursor_gc,
		    0, i, f->width * FONT_WIDTH (f->display.x->font)
		    + 2 * f->display.x->internal_border_width, 25);
#endif /* ! 0 */

  x = (f->width * FONT_WIDTH (f->display.x->font)) / 4;
  y = (f->height * FONT_HEIGHT (f->display.x->font)) / 4;
  XFillRectangle (x_current_display, FRAME_X_WINDOW (f),
		  f->display.x->cursor_gc,
		  x, y, 2 * x, 2 * y);
  dumpglyphs (f, (x + f->display.x->internal_border_width),
	     (y + f->display.x->internal_border_width),
	     &active_frame->glyphs[(f->height / 4) + 1][(f->width / 4)],
	     1, 0, f->display.x->font);

#else /* ! defined (HAVE_X11) */
  for (i = f->height * FONT_HEIGHT (f->display.x->font) - 10;
       i >= 0;
       i -= 50)
    XPixFill (FRAME_X_WINDOW (f), 0, i,
	      f->width * FONT_WIDTH (f->display.x->font)
	      + 2 * f->display.x->internal_border_width, 10,
	      WHITE_PIX_DEFAULT, ClipModeClipped, GXinvert, AllPlanes);
#endif /* ! defined (HAVE_X11) */

  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Flip background and forground colors of the frame. */

x_invert_frame (f)
     struct frame *f;
{
#ifdef HAVE_X11
  GC temp;
  unsigned long pix_temp;

  x_display_cursor (f, 0);
  XClearWindow (x_current_display, FRAME_X_WINDOW (f));
  temp = f->display.x->normal_gc;
  f->display.x->normal_gc = f->display.x->reverse_gc;
  f->display.x->reverse_gc = temp;
  pix_temp = f->display.x->foreground_pixel;
  f->display.x->foreground_pixel = f->display.x->background_pixel;
  f->display.x->background_pixel = pix_temp;

  XSetWindowBackground (x_current_display, FRAME_X_WINDOW (f),
			f->display.x->background_pixel);
  if (f->display.x->background_pixel == f->display.x->cursor_pixel)
    {
      f->display.x->cursor_pixel = f->display.x->foreground_pixel;
      XSetBackground (x_current_display, f->display.x->cursor_gc,
		      f->display.x->cursor_pixel);
      XSetForeground (x_current_display, f->display.x->cursor_gc,
		      f->display.x->background_pixel);
    }
  redraw_frame (f);
#endif /* ! defined (HAVE_X11) */
}

/* Make audible bell.  */

#ifdef HAVE_X11
#define XRINGBELL XBell(x_current_display, 0)
#else /* ! defined (HAVE_X11) */
#define XRINGBELL XFeep(0);
#endif /* ! defined (HAVE_X11) */

XTring_bell ()
{
  if (visible_bell)
#if 0
    XTflash (selected_frame);
#endif /* ! 0 */
    {
      x_invert_frame (selected_frame);
      x_invert_frame (selected_frame);
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
  if (updating_frame == 0)
    abort ();

  if ((n <= 0) || (n > updating_frame->height))
    flexlines = updating_frame->height;
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
  register struct frame *f = updating_frame;
  int intborder = f->display.x->internal_border_width;

  if (curs_y >= flexlines)
    return;

  topregion = curs_y;
  bottomregion = flexlines - (n + 1);
  newtop = topregion + n;
  length = (bottomregion - topregion) + 1;

#ifndef HAVE_X11
  dumpqueue ();
#endif /* HAVE_X11 */

  if ((length > 0) && (newtop <= flexlines))
    {
#ifdef HAVE_X11
      XCopyArea (x_current_display, FRAME_X_WINDOW (f),
		 FRAME_X_WINDOW (f), f->display.x->normal_gc,
		 intborder, topregion * FONT_HEIGHT (f->display.x->font) + intborder,
		 f->width * FONT_WIDTH (f->display.x->font),
		 length * FONT_HEIGHT (f->display.x->font), intborder,
		 newtop * FONT_HEIGHT (f->display.x->font) + intborder);
#else /* ! defined (HAVE_X11) */
      XMoveArea (FRAME_X_WINDOW (f),
		 intborder, topregion * FONT_HEIGHT (f->display.x->font) + intborder,
		 intborder, newtop * FONT_HEIGHT (f->display.x->font) + intborder,
		 f->width * FONT_WIDTH (f->display.x->font),
		 length * FONT_HEIGHT (f->display.x->font));
      /* Now we must process any ExposeRegion events that occur
	 if the area being copied from is obscured.
	 We can't let it wait because further i/d operations
	 may want to copy this area to another area.  */
      x_read_exposes ();
#endif /* ! defined (HAVE_X11) */
    }

  newtop = min (newtop, (flexlines - 1));
  length = newtop - topregion;
  if (length > 0)
    {
#ifdef HAVE_X11
      XClearArea (x_current_display, FRAME_X_WINDOW (f), intborder, 
		  topregion * FONT_HEIGHT (f->display.x->font) + intborder,
		  f->width * FONT_WIDTH (f->display.x->font),
		  n * FONT_HEIGHT (f->display.x->font), False);
#else /* ! defined (HAVE_X11) */
      XPixSet (FRAME_X_WINDOW (f),
	       intborder,
	       topregion * FONT_HEIGHT (f->display.x->font) + intborder,
	       f->width * FONT_WIDTH (f->display.x->font),
	       n * FONT_HEIGHT (f->display.x->font),
	       f->display.x->background_pixel);
#endif /* ! defined (HAVE_X11) */
    }
}

/* Perform a delete-lines operation, deleting N lines
   at a vertical position curs_y.  */

static void
scraplines (n)
     register int n;
{
  int mask;
  register struct frame *f = updating_frame;
  int intborder = f->display.x->internal_border_width;

  if (curs_y >= flexlines)
    return;

#ifndef HAVE_X11
  dumpqueue ();
#endif /* HAVE_X11 */

  if ((curs_y + n) >= flexlines)
    {
      if (flexlines >= (curs_y + 1))
	{
#ifdef HAVE_X11
	  XClearArea (x_current_display, FRAME_X_WINDOW (f), intborder,
		      curs_y * FONT_HEIGHT (f->display.x->font) + intborder,
		      f->width * FONT_WIDTH (f->display.x->font),
		      (flexlines - curs_y) * FONT_HEIGHT (f->display.x->font), False);
#else /* ! defined (HAVE_X11) */
	  XPixSet (FRAME_X_WINDOW (f),
		   intborder, curs_y * FONT_HEIGHT (f->display.x->font) + intborder,
		   f->width * FONT_WIDTH (f->display.x->font),
		   (flexlines - curs_y) * FONT_HEIGHT (f->display.x->font),
		   f->display.x->background_pixel);
#endif /* ! defined (HAVE_X11) */
	}
    }
  else
    {
#ifdef HAVE_X11
      XCopyArea (x_current_display, FRAME_X_WINDOW (f),
		 FRAME_X_WINDOW (f), f->display.x->normal_gc,
		 intborder,
		 (curs_y + n) * FONT_HEIGHT (f->display.x->font) + intborder,
		 f->width * FONT_WIDTH (f->display.x->font),
		 (flexlines - (curs_y + n)) * FONT_HEIGHT (f->display.x->font),
		 intborder, curs_y * FONT_HEIGHT (f->display.x->font) + intborder);
      XClearArea (x_current_display, FRAME_X_WINDOW (f),
		  intborder,
		  (flexlines - n) * FONT_HEIGHT (f->display.x->font) + intborder,
		  f->width * FONT_WIDTH (f->display.x->font),
		  n * FONT_HEIGHT (f->display.x->font), False);
#else /* ! defined (HAVE_X11) */
      XMoveArea (FRAME_X_WINDOW (f),
		 intborder,
		 (curs_y + n) * FONT_HEIGHT (f->display.x->font) + intborder,
		 intborder, curs_y * FONT_HEIGHT (f->display.x->font) + intborder,
		 f->width * FONT_WIDTH (f->display.x->font),
		 (flexlines - (curs_y + n)) * FONT_HEIGHT (f->display.x->font));
      /* Now we must process any ExposeRegion events that occur
	 if the area being copied from is obscured.
	 We can't let it wait because further i/d operations
	 may want to copy this area to another area.  */
      x_read_exposes ();
      XPixSet (FRAME_X_WINDOW (f), intborder,
	       (flexlines - n) * FONT_HEIGHT (f->display.x->font) + intborder,
	       f->width * FONT_WIDTH (f->display.x->font),
	       n * FONT_HEIGHT (f->display.x->font), f->display.x->background_pixel);
#endif /* ! defined (HAVE_X11) */
    }
}

/* Perform an insert-lines or delete-lines operation,
   inserting N lines or deleting -N lines at vertical position VPOS.  */

XTins_del_lines (vpos, n)
     int vpos, n;
{
  if (updating_frame == 0)
    abort ();

  /* Hide the cursor.  */
  x_display_cursor (updating_frame, 0);

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

/* Output into a rectangle of an X-window (for frame F)
   the characters in f->phys_lines that overlap that rectangle.
   TOP and LEFT are the position of the upper left corner of the rectangle.
   ROWS and COLS are the size of the rectangle.  */

static void
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

  top -= f->display.x->internal_border_width;
  left -= f->display.x->internal_border_width;

  /* Express rectangle as four edges, instead of position-and-size.  */
  bottom = top + rows;
  right = left + cols;

#ifndef HAVE_X11		/* Window manger does this for X11. */
  /* If the rectangle includes any of the internal border area,
     redisplay the border emphasis.  */
  if (top < 0 || left < 0
      || bottom > f->height * FONT_HEIGHT (f->display.x->font)
      || right > f->width * FONT_WIDTH (f->display.x->font))
    dumpborder (f, 0);
#endif /* HAVE_X11		/* Window manger does this for X11. */ */
  
  /* Convert rectangle edges in pixels to edges in chars.
     Round down for left and top, up for right and bottom.  */
  top /= FONT_HEIGHT (f->display.x->font);
  left /= FONT_WIDTH (f->display.x->font);
  bottom += (FONT_HEIGHT (f->display.x->font) - 1);
  right += (FONT_WIDTH (f->display.x->font) - 1);
  bottom /= FONT_HEIGHT (f->display.x->font);
  right /= FONT_WIDTH (f->display.x->font);

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
		 (left * FONT_WIDTH (f->display.x->font)
		  + f->display.x->internal_border_width),
		 (y * FONT_HEIGHT (f->display.x->font)
		  + f->display.x->internal_border_width),
		 line, min (cols, active_frame->used[y] - left),
		 active_frame->highlight[y], f->display.x->font);
    }

  /* Turn the cursor on if we turned it off.  */

  if (cursor_cleared)
    x_display_cursor (f, 1);
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
      struct frame *f = x_window_to_frame (r.window);
      if (f->display.x->icon_desc == r.window)
	refreshicon (f);
      else
	dumprectangle (f, r.x, r.y, r.width, r.height);
    }
  XFlushQueue ();
}
#endif /* HAVE_X11 */

/* Process all expose events that are pending.
   Redraws the cursor if necessary on any frame that
   is not in the process of being updated with update_frame.  */

static void
x_do_pending_expose ()
{
  int mask;
  struct frame *f;
  Lisp_Object tail, frame;

  if (expose_all_windows)
    {
      expose_all_windows = 0;
      for (tail = Vframe_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  register int temp_width, temp_height;
	  int intborder;

	  frame = XCONS (tail)->car;
	  if (XTYPE (frame) != Lisp_Frame)
	    continue;
	  f = XFRAME (frame);
	  if (! FRAME_X_P (f))
	    continue;
	  if (!f->async_visible)
	    continue;
	  if (!f->display.x->needs_exposure)
	    continue;

	  intborder = f->display.x->internal_border_width;

	  clear_cursor (f);
	  XGetWindowInfo (FRAME_X_WINDOW (f), &windowinfo);
	  temp_width = ((windowinfo.width - 2 * intborder
			 - f->display.x->v_scrollbar_width)
			/ FONT_WIDTH (f->display.x->font));
	  temp_height = ((windowinfo.height- 2 * intborder
			  - f->display.x->h_scrollbar_height)
			 / FONT_HEIGHT (f->display.x->font));
	  if (temp_width != f->width || temp_height != f->height)
	    {
	      change_frame_size (f, max (1, temp_height),
				  max (1, temp_width), 0, 1);
	      x_resize_scrollbars (f);
	    }
	  f->display.x->left_pos = windowinfo.x;
	  f->display.x->top_pos = windowinfo.y;
	  dumprectangle (f, 0, 0, PIXEL_WIDTH (f), PIXEL_HEIGHT (f));
#if 0
	  dumpborder (f, 0);
#endif /* ! 0 */
	  f->display.x->needs_exposure = 0;
	  if (updating_frame != f)
	    x_display_cursor (f, 1);
	  XFlushQueue ();
	}
    }
  else
    /* Handle any individual-rectangle expose events queued
       for various windows.  */
#ifdef HAVE_X11
    ;
#else /* ! defined (HAVE_X11) */
    dumpqueue ();
#endif /* ! defined (HAVE_X11) */
}

#ifdef HAVE_X11
static void
frame_highlight (frame)
     struct frame *frame;
{
  if (! EQ (Vx_no_window_manager, Qnil))
    XSetWindowBorder (x_current_display, FRAME_X_WINDOW (frame),
		      frame->display.x->border_pixel);
  x_display_cursor (frame, 1);
}

static void
frame_unhighlight (frame)
     struct frame *frame;
{
  if (! EQ (Vx_no_window_manager, Qnil))
    XSetWindowBorderPixmap (x_current_display, FRAME_X_WINDOW (frame),
			    frame->display.x->border_tile);
  x_display_cursor (frame, 1);
}
#else /* ! defined (HAVE_X11) */
/* Dump the border-emphasis of frame F.
   If F is selected, this is a lining of the same color as the border,
   just within the border, occupying a portion of the internal border.
   If F is not selected, it is background in the same place.
   If ALWAYS is 0, don't bother explicitly drawing if it's background.

   ALWAYS = 1 is used when a frame becomes selected or deselected.
   In that case, we also turn the cursor off and on again
   so it will appear in the proper shape (solid if selected; else hollow.)  */

static void
dumpborder (f, always)
     struct frame *f;
     int always;
{
  int thickness = f->display.x->internal_border_width / 2;
  int width = PIXEL_WIDTH (f);
  int height = PIXEL_HEIGHT (f);
  int pixel;

  if (f != selected_frame)
    {
      if (!always)
	return;

      pixel = f->display.x->background_pixel;
    }
  else
    {
      pixel = f->display.x->border_pixel;
    }

  XPixSet (FRAME_X_WINDOW (f), 0, 0, width, thickness, pixel);
  XPixSet (FRAME_X_WINDOW (f), 0, 0, thickness, height, pixel);
  XPixSet (FRAME_X_WINDOW (f), 0, height - thickness, width,
	   thickness, pixel);
  XPixSet (FRAME_X_WINDOW (f), width - thickness, 0, thickness,
	   height, pixel);

  if (always)
    x_display_cursor (f, 1);
}
#endif /* ! defined (HAVE_X11) */

static void XTframe_rehighlight ();

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occured, so the
   lisp code can tell when the switch took place by examining the events.  */

static void
x_new_focus_frame (frame)
     struct frame *frame;
{
  struct frame *old_focus = x_focus_frame;
  int events_enqueued = 0;

  if (frame != x_focus_frame)
    {
      /* Set this before calling other routines, so that they see 
	 the correct value of x_focus_frame.  */
      x_focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	x_lower_frame (old_focus);

#if 0
      selected_frame = frame;
      XSET (XWINDOW (selected_frame->selected_window)->frame,
	    Lisp_Frame, selected_frame);
      Fselect_window (selected_frame->selected_window);
      choose_minibuf_frame ();
#endif /* ! 0 */

      if (x_focus_frame && x_focus_frame->auto_raise)
	x_raise_frame (x_focus_frame);
    }

  XTframe_rehighlight ();
}


/* The focus has changed, or we have make a frame's selected window
   point to a window on a different frame (this happens with global
   minibuffer frames).  Shift the highlight as appropriate.  */
static void
XTframe_rehighlight ()
{
  struct frame *old_highlight = x_highlight_frame;

  if (x_focus_frame)
    {
      x_highlight_frame = XFRAME (FRAME_FOCUS_FRAME (x_focus_frame));
      if (x_highlight_frame->display.nothing == 0)
	XSET (FRAME_FOCUS_FRAME (x_focus_frame), Lisp_Frame,
	      (x_highlight_frame = x_focus_frame));
    }
  else
    x_highlight_frame = 0;

  if (x_highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (x_highlight_frame)
	frame_highlight (x_highlight_frame);
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
   F is the frame in which the event occurred.

   WINDOW_TYPE says whether the event happened in a scrollbar window
   or a text window, affecting the format of the event created.

   PART specifies which part of the scrollbar the event happened in,
   if WINDOW_TYPE == scrollbar_window.

   If the mouse is over the same character as the last time we checked,
   don't return an event; set result->kind to no_event.  */

static void
notice_mouse_movement (result, motion_event, f, window_type, part)
     struct input_event *result;
     XMotionEvent motion_event;
     struct frame *f;
     int window_type;
     Lisp_Object part;
{
  int x, y, root_x, root_y, pix_x, pix_y;
  unsigned int keys_and_buttons;
  Window w, root_window;

  /* Unless we decide otherwise below, return a non-event.  */
  result->kind = no_event;
  
  if (XQueryPointer (x_current_display,
		     FRAME_X_WINDOW (f),
		     &root_window, &w,
		     &root_x, &root_y, &pix_x, &pix_y,
		     &keys_and_buttons)
      == False)
    return;

#if 0
  if (w == None)   /* Mouse no longer in window. */
    return Qnil;
#endif /* ! 0 */

  pixel_to_glyph_translation (f, pix_x, pix_y, &x, &y);
  if (x == x_mouse_x && y == x_mouse_y)
    return;

  x_mouse_x = x;
  x_mouse_y = y;

  /* What sort of window are we in now?  */
  if (window_type == text_window)            /* Text part */
    {
      int modeline_p;

      Vmouse_window = window_from_coordinates (f, x, y, &modeline_p);

      if (XTYPE (Vmouse_window) == Lisp_Window)
	mouse_buffer_offset
	  = buffer_posn_from_coords (XWINDOW (Vmouse_window), x, y);
      else
	mouse_buffer_offset = 0;

      if (EQ (Vmouse_window, Qnil))
	Vmouse_frame_part = Qnil;
      else if (modeline_p)
	Vmouse_frame_part = Qmodeline_part;
      else
	Vmouse_frame_part = Qtext_part;
      
      result->kind = window_sys_event;
      result->code = Qmouse_moved;

      return;
    }
  else if (window_type == scrollbar_window)  /* Scrollbar */
    {
      Vmouse_window = f->selected_window;
      mouse_buffer_offset = 0;
      Vmouse_frame_part = part;

      result->kind = window_sys_event;
      result->code = Qmouse_moved;

      return;
    }

  return;
}
#endif /* ! 0 */


/* Mouse clicks and mouse movement.  Rah.  */
#ifdef HAVE_X11

/* Given a pixel position (PIX_X, PIX_Y) on the frame F, return
   glyph co-ordinates in (*X, *Y).  Set *BOUNDS to the rectangle
   that the glyph at X, Y occupies, if BOUNDS != 0.  */
static void
pixel_to_glyph_coords (f, pix_x, pix_y, x, y, bounds)
     FRAME_PTR f;
     register unsigned int pix_x, pix_y;
     register int *x, *y;
     XRectangle *bounds;
{
  int ibw = f->display.x->internal_border_width;
  int width, height;
  FONT_TYPE *font = f->display.x->font;

  width = FONT_WIDTH (font);
  height = FONT_HEIGHT (font);

  /* What line is it on?  */
  if (pix_y < ibw)
    *y = 0;
  else if (pix_y > f->display.x->pixel_height - ibw)
    *y = FRAME_HEIGHT (f) - 1;
  else
    *y = (pix_y - ibw) / height;

  /* And what column?  */
  if (pix_x < ibw)
    *x = 0;
  else if (pix_x > f->display.x->pixel_width - ibw)
    *x = FRAME_WIDTH (f) - 1;
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

/* Which modifier keys are on which modifier bits?

   With each keystroke, X returns eight bits indicating which modifier
   keys were held down when the key was pressed.  The interpretation
   of the top five modifier bits depends on what keys are attached
   to them.  If the Meta_L and Meta_R keysyms are on mod5, then mod5
   is the meta bit.
   
   x_meta_mod_mask is a mask containing the bits used for the meta key.
   It may have more than one bit set, if more than one modifier bit
   has meta keys on it.  Basically, if EVENT is a KeyPress event,
   the meta key is pressed if (EVENT.state & x_meta_mod_mask) != 0.  

   x_shift_lock_mask is LockMask if the XK_Shift_Lock keysym is on the
   lock modifier bit, or zero otherwise.  Non-alphabetic keys should
   only be affected by the lock modifier bit if XK_Shift_Lock is in
   use; XK_Caps_Lock should only affect alphabetic keys.  With this
   arrangement, the lock modifier should shift the character if
   (EVENT.state & x_shift_lock_mask) != 0.  */
static int x_meta_mod_mask, x_shift_lock_mask;

/* Initialize mode_switch_bit and modifier_meaning.  */
static void
x_find_modifier_meanings ()
{
  int min_code, max_code;
  KeySym *syms;
  int syms_per_code;
  XModifierKeymap *mods;
  int alt_mod_mask = 0;

  x_meta_mod_mask = 0;
  x_shift_lock_mask = 0;
  
  XDisplayKeycodes (x_current_display, &min_code, &max_code);
  syms = XGetKeyboardMapping (x_current_display,
			      min_code, max_code - min_code + 1,
			      &syms_per_code);
  mods = XGetModifierMapping (x_current_display);

  /* Scan the modifier table to see which modifier bits the Meta and 
     Alt keysyms are on.  */
  {
    int row, col;	/* The row and column in the modifier table. */

    for (row = 3; row < 8; row++)
      for (col = 0; col < mods->max_keypermod; col++)
	{
	  KeyCode code =
	    mods->modifiermap[(row * mods->max_keypermod) + col];

	  /* Are any of this keycode's keysyms a meta key?  */
	  {
	    int code_col;

	    for (code_col = 0; code_col < syms_per_code; code_col++)
	      {
		int sym = syms[((code - min_code) * syms_per_code) + code_col];

		switch (sym)
		  {
		  case XK_Meta_L:
		  case XK_Meta_R:
		    x_meta_mod_mask |= (1 << row);
		    break;

		  case XK_Alt_L:
		  case XK_Alt_R:
		    alt_mod_mask |= (1 << row);
		    break;

		  case XK_Shift_Lock:
		    /* Ignore this if it's not on the lock modifier.  */
		    if ((1 << row) == LockMask)
		      x_shift_lock_mask = LockMask;
		    break;
		  }
	      }
	  }
	}
  }

  /* If we couldn't find any meta keys, accept any alt keys as meta keys.  */
  if (! x_meta_mod_mask)
    x_meta_mod_mask = alt_mod_mask;

  XFree ((char *) syms);
  XFreeModifiermap (mods);
}


/* Convert a set of X modifier bits to the proper form for a
   struct input_event modifiers value.  */

static unsigned int
x_convert_modifiers (state)
     unsigned int state;
{
  return (  ((state & (ShiftMask | x_shift_lock_mask)) ? shift_modifier : 0)
	  | ((state & ControlMask)		       ? ctrl_modifier  : 0)
	  | ((state & x_meta_mod_mask)		       ? meta_modifier  : 0));
}

extern struct frame *x_window_to_scrollbar ();
extern Lisp_Object Vmouse_event;

/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.

   If PART and PREFIX are 0, then the event occurred in the text part;
   otherwise it happened in a scrollbar. */

static Lisp_Object
construct_mouse_click (result, event, f, part, prefix)
     struct input_event *result;
     XButtonEvent *event;
     struct frame *f;
     int prefix;
     Lisp_Object part;
{
  /* Initialize those fields text and scrollbar clicks hold in common.
     Make the event type no_event; we'll change that when we decide
     otherwise.  */
  result->kind = no_event;
  XSET (result->code, Lisp_Int, event->button);
  result->timestamp = event->time;
  result->modifiers = (x_convert_modifiers (event->state)
		       | (event->type == ButtonRelease
			  ? up_modifier 
			  : down_modifier));

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

  if (! NILP (part))		/* Scrollbar event */
    {
      int pos, len;

      pos = event->y - (f->display.x->v_scrollbar_width - 2);
      x_mouse_x = pos;
      len = ((FONT_HEIGHT (f->display.x->font) * f->height)
	     + f->display.x->internal_border_width
	     - (2 * (f->display.x->v_scrollbar_width - 2)));
      x_mouse_y = len;

      result->kind = scrollbar_click;
      result->part = part;
      XSET (result->x, Lisp_Int, (f->display.x->top_pos - event->y));
      XSET (result->y, Lisp_Int, f->display.x->pixel_height);
      result->frame = f;
    }
  else				/* Text Window Event */
    {
      int row, column;

      pixel_to_glyph_coords (f, event->x, event->y, &column, &row, NULL);
      result->kind = mouse_click;
      XFASTINT (result->x) = column;
      XFASTINT (result->y) = row;
      result->frame = f;
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
   is at least as efficient as getting motion events when mouse
   tracking is on, and I suspect only negligibly worse when tracking
   is off.

   The silly O'Reilly & Associates Nutshell guides barely document
   pointer motion hints at all (I think you have to infer how they
   work from an example), and the description of XQueryPointer doesn't
   mention that calling it causes you to get another motion hint from
   the server, which is very important.  */

/* Where the mouse was last time we reported a mouse event.  */
static FRAME_PTR last_mouse_frame;
static XRectangle last_mouse_glyph;

/* This is a hack.  We would really prefer that XTmouse_position would
   return the time associated with the position it returns, but there
   doesn't seem to be any way to wrest the timestamp from the server
   along with the position query.  So, we just keep track of the time
   of the last movement we received, and return that in hopes that
   it's somewhat accurate.  */
static Time last_mouse_movement_time;

/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */
static void
note_mouse_position (frame, event)
     FRAME_PTR frame;
     XMotionEvent *event;

{
  last_mouse_movement_time = event->time;

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
XTmouse_position (f, x, y, time)
     FRAME_PTR *f;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  int ix, iy, dummy;
  Display *d = x_current_display;
  Window guess, root, child;

  BLOCK_INPUT;

  /* I would like to have an X function that just told me the
     innermost window containing the mouse.  

  /* There doesn't seem to be any way to just get the innermost window
     containing the pointer, no matter what X frame it's on; you have
     to guess a window, and then X will tell you which one of that
     window's children it's in.  If the pointer isn't in any of that
     window's children, it gives you a root window that contains it.

     So we start with the selected frame's window and chase down
     branches under the guidance of XQueryPointer until we hit a leaf
     (all of the Emacs windows we care about are leaf windows).  If at
     any time XQueryPointer returns false, that means that the current
     window does not contain the pointer any more (perhaps it moved),
     so we start with the root window XQueryPointer has given us and
     start again.  */

  guess = FRAME_X_WINDOW (selected_frame);
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
         anymore, but root is the root window of the frame we should
         try instead.  */
      guess = root;

  *f = last_mouse_frame = x_window_to_frame (guess);
  if (! *f)
    *x = *y = Qnil;
  else
    {
      pixel_to_glyph_coords (*f, ix, iy, &ix, &iy, &last_mouse_glyph);
      XSET (*x, Lisp_Int, ix);
      XSET (*y, Lisp_Int, iy);
    }

  mouse_moved = 0;

  /* I don't know how to find the time for the last movement; it seems
     like XQueryPointer ought to return it, but it doesn't.  So, we'll
     return the time of the last MotionNotify event we received.  Note
     that the use of motion hints means that this isn't guaranteed to
     be accurate at all.  */
  *time = last_mouse_movement_time;

  UNBLOCK_INPUT;
}

#else /* ! defined (HAVE_X11) */
#define XEvent XKeyPressedEvent
#endif /* ! defined (HAVE_X11) */


/* Timestamp of enter window event.  This is only used by XTread_socket,
   but we have to put it out here, since static variables within functions
   sometimes don't work.  */
static Time enter_timestamp;

/* This holds the state XLookupString needs to implement dead keys
   and other tricks known as "compose processing".  _X Window System_ 
   says that a portable program can't use this, but Stephen Gildea assures
   me that letting the compiler initialize it to zeros will work okay.

   This must be defined outside of XTread_socket, for the same reasons
   given for enter_timestamp, above.  */
static XComposeStatus compose_status;

/* Communication with window managers. */
Atom Xatom_wm_protocols;

/* Kinds of protocol things we may receive. */
Atom Xatom_wm_take_focus;
Atom Xatom_wm_save_yourself;
Atom Xatom_wm_delete_window;

/* Other WM communication */
Atom Xatom_wm_configure_denied;	  /* When our config request is denied */
Atom Xatom_wm_window_moved;	  /* When the WM moves us. */

/* Read events coming from the X server.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   Events representing keys are stored in buffer BUFP,
   which can hold up to NUMCHARS characters.
   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   WAITP is nonzero if we should block until input arrives.
   EXPECTED is nonzero if the caller knows input is available.  */

int
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
  struct frame *f;
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
#endif /* ! defined (FIOSNBIO) */

#ifndef SIGIO
#ifndef HAVE_SELECT
  if (! (fcntl (fileno (stdin), F_GETFL, 0) & O_NDELAY))
    {
      extern int read_alarm_should_throw;
      read_alarm_should_throw = 1;
      XPeekEvent (XDISPLAY &event);
      read_alarm_should_throw = 0;
    }
#endif /* HAVE_SELECT */
#endif /* SIGIO */

  while (XStuffPending () != 0)
    {
      XNextEvent (XDISPLAY &event);
      event_found = 1;

      switch (event.type)
	{
#ifdef HAVE_X11
	case ClientMessage:
	  {
	    if (event.xclient.message_type == Xatom_wm_protocols
		&& event.xclient.format == 32)
	      {
		if (event.xclient.data.l[0] == Xatom_wm_take_focus)
		  {
		    f = x_window_to_frame (event.xclient.window);
		    if (f)
		      x_focus_on_frame (f);
		    /* Not certain about handling scrollbars here */
		  }
		else if (event.xclient.data.l[0] == Xatom_wm_save_yourself)
		  {
		    /* Save state modify the WM_COMMAND property to
		       something which can reinstate us. This notifies
		       the session manager, who's looking for such a
		       PropertyNotify.  Can restart processing when
		       a keyboard or mouse event arrives. */
		    if (numchars > 0)
		      {
		      }
		  }
		else if (event.xclient.data.l[0] == Xatom_wm_delete_window)
		  {
		    struct frame *f = x_window_to_frame (event.xclient.window);

		    if (f)
		      if (numchars > 0)
			{
			}
		  }
	      }
	    else if (event.xclient.message_type == Xatom_wm_configure_denied)
	      {
	      }
	    else if (event.xclient.message_type == Xatom_wm_window_moved)
	      {
		int new_x, new_y;

		new_x = event.xclient.data.s[0];
		new_y = event.xclient.data.s[1];
	      }
	  }
	  break;

	case SelectionClear:	/* Someone has grabbed ownership. */
	  x_disown_selection (event.xselectionclear.window,
			      event.xselectionclear.selection,
			      event.xselectionclear.time);
	  break;

	case SelectionRequest:	/* Someone wants our selection. */
	  x_answer_selection_request (event);
	  break;

	case PropertyNotify:

	  /* If we're being told about a root window property, then it's
	     a cut buffer change.  */
	  if (event.xproperty.window == ROOT_WINDOW)
	    x_invalidate_cut_buffer_cache (&event.xproperty);

	  /* Otherwise, we're probably handling an incremental
             selection transmission.  */
	  else
	    {
	      /* If we were to do this synchronously, there'd be no worry
		 about re-selecting. */
	      x_send_incremental (event);
	    }
	  break;

	case Expose:
	  f = x_window_to_frame (event.xexpose.window);
	  if (f)
	    {
	      if (f->async_visible == 0)
		{
		  f->async_visible = 1;
		  f->async_iconified = 0;
		  SET_FRAME_GARBAGED (f);
		}
	      else
		dumprectangle (x_window_to_frame (event.xexpose.window),
			       event.xexpose.x, event.xexpose.y,
			       event.xexpose.width, event.xexpose.height);
	    }
	  break;

	case GraphicsExpose:	/* This occurs when an XCopyArea's
				  source area was obscured or not
				  available.*/
	  dumprectangle (x_window_to_frame (event.xgraphicsexpose.drawable),
			 event.xgraphicsexpose.x, event.xgraphicsexpose.y,
			 event.xgraphicsexpose.width,
			 event.xgraphicsexpose.height);
	  break;

	case NoExpose:		/* This occurs when an XCopyArea's
				  source area was completely
				  available */
	  break;
#else /* ! defined (HAVE_X11) */
	case ExposeWindow:
	  if (event.subwindow != 0)
	    break;		/* duplicate event */
	  f = x_window_to_frame (event.window);
	  if (event.window == f->display.x->icon_desc)
	    {
	      refreshicon (f);
	      f->async_iconified = 1;
	    }
	  if (event.window == FRAME_X_WINDOW (f))
	    {
	      /* Say must check all windows' needs_exposure flags.  */
	      expose_all_windows = 1;
	      f->display.x->needs_exposure = 1;
	      f->async_visible = 1;
	    }
	  break;

	case ExposeRegion:
	  if (event.subwindow != 0)
	    break;		/* duplicate event */
	  f = x_window_to_frame (event.window);
	  if (event.window == f->display.x->icon_desc)
	    {
	      refreshicon (f);
	      break;
	    }
	  /* If window already needs full redraw, ignore this rectangle.  */
	  if (expose_all_windows && f->display.x->needs_exposure)
	    break;
	  /* Put the event on the queue of rectangles to redraw.  */
	  if (enqueue_event (&event, &x_expose_queue))
	    /* If it is full, we can't record the rectangle,
	       so redraw this entire window.  */
	    {
	      /* Say must check all windows' needs_exposure flags.  */
	      expose_all_windows = 1;
	      f->display.x->needs_exposure = 1;
	    }
	  break;

	case ExposeCopy:
	  /* This should happen only when we are expecting it,
	     in x_read_exposes.  */
	  abort ();
#endif /* ! defined (HAVE_X11) */

#ifdef HAVE_X11
	case UnmapNotify:
	  {
	    XWMHints *hints;

	    f = x_window_to_frame (event.xunmap.window);
	    if (f)		/* F may no longer exist if
				   the frame was deleted.  */
	      {
		/* While a frame is unmapped, display generation is
		   disabled; you don't want to spend time updating a
		   display that won't ever be seen.  */
		f->async_visible = 0;
		x_mouse_x = x_mouse_y = -1;
	      }
	  }
	  break;

	case MapNotify:
	  f = x_window_to_frame (event.xmap.window);
	  if (f)
	    {
	      f->async_visible = 1;
	      f->async_iconified = 0;

	      /* wait_reading_process_input will notice this and update
		 the frame's display structures.  */
	      SET_FRAME_GARBAGED (f);
	    }
	  break;

	  /* Turn off processing if we become fully obscured. */
	case VisibilityNotify:
	  break;

#else /* ! defined (HAVE_X11) */
	case UnmapWindow:
	  f = x_window_to_frame (event.window);
	  if (event.window == f->display.x->icon_desc)
	    f->async_iconified = 0;
	  if (event.window == FRAME_X_WINDOW (f))
	    f->async_visible = 0;
	  break;
#endif /* ! defined (HAVE_X11) */

#ifdef HAVE_X11
	case KeyPress:
	  f = x_window_to_frame (event.xkey.window);
	  if (f != 0)
	    {
	      KeySym keysym;
	      char copy_buffer[80];
	      int modifiers = event.xkey.state;

	      /* Some keyboards generate different characters
		 depending on the state of the meta key, in an attempt
		 to support non-English typists.  It would be nice to
		 keep this functionality somehow, but for now, we will
		 just clear the meta-key flag to get the 'pure' character.  */
	      event.xkey.state &= ~Mod1Mask;

	      /* This will have to go some day...  */
	      nbytes =
		XLookupString (&event.xkey, copy_buffer, 80, &keysym,
			       &compose_status);

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
		      bufp->frame = f;
		      bufp->modifiers = x_convert_modifiers (modifiers);
		      bufp->timestamp = event.xkey.time;
		      bufp++;
		      count++;
		      numchars--;
		    }
		  else if (numchars > nbytes)
		    {
		      register int i;

		      if (nbytes == 1)
			{
			  if (modifiers & x_meta_mod_mask)
			    *copy_buffer |= METABIT;
			  bufp->kind = ascii_keystroke;
			  XSET (bufp->code, Lisp_Int, *copy_buffer);
			  bufp->frame = f;
			  bufp->timestamp = event.xkey.time;
			  bufp++;
			}
		      else
			for (i = nbytes - 1; i > 1; i--)
			  {
			    bufp->kind = ascii_keystroke;
			    XSET (bufp->code, Lisp_Int, copy_buffer[i]);
			    bufp->frame = f;
			    bufp->timestamp = event.xkey.time;
			    bufp++;
			  }

		      count += nbytes;
		      numchars -= nbytes;
		    }
		}
	    }
	  break;
#else /* ! defined (HAVE_X11) */
	case KeyPressed:
	  {
	    register char *where_mapping;

	    f = x_window_to_frame (event.window);
	    /* Ignore keys typed on icon windows.  */
	    if (f != 0 && event.window == f->display.x->icon_desc)
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
		    bufp->frame = f;
		    bufp++;
		  }
		count += nbytes;
		numchars -= nbytes;
	      }
	  }
	  break;
#endif /* ! defined (HAVE_X11) */

#ifdef HAVE_X11
	case EnterNotify:
	  f = x_window_to_frame (event.xcrossing.window);

	  if (event.xcrossing.detail == NotifyInferior)	/* Left Scrollbar */
	    ;
	  else if (event.xcrossing.focus)		/* Entered Window */
	    {
	      /* If we decide we want to generate an event to be seen
		 by the rest of Emacs, we put it here.  */
	      struct input_event emacs_event;
	      emacs_event.kind = no_event;

	      /* Avoid nasty pop/raise loops. */
	      if (f && (!(f->auto_raise)
			|| !(f->auto_lower)
			|| (event.xcrossing.time - enter_timestamp) > 500))
		{
		  x_new_focus_frame (f);
		  enter_timestamp = event.xcrossing.time;
		}
#if 0
	      else if ((f = x_window_to_scrollbar (event.xcrossing.window,
						   &part, &prefix)))
		/* Fake a motion event */
		notice_mouse_movement (&emacs_event,
				       event.xmotion, f, scrollbar_window,
				       part);
#endif /* ! 0 */

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
#endif /* ! 0 */
	    }
	  else if (f == x_focus_frame)
	    x_new_focus_frame (0);
#if 0
	  else if (f = x_window_to_frame (event.xcrossing.window))
	    x_mouse_frame = f;
#endif /* ! 0 */

	  break;

	case FocusIn:
	  f = x_window_to_frame (event.xfocus.window);
	  if (f)
	    x_new_focus_frame (f);
	  break;

	case LeaveNotify:
	  if (event.xcrossing.detail != NotifyInferior
	      && event.xcrossing.subwindow == None
	      && event.xcrossing.mode == NotifyNormal)
	    {
	      f = x_window_to_frame (event.xcrossing.window);

	      if (event.xcrossing.focus)
		x_new_focus_frame (f);
	      else if (f == x_focus_frame)
		x_new_focus_frame (0);
	    }
	  break;

	case FocusOut:
	  f = x_window_to_frame (event.xfocus.window);
	  if (f && f == x_focus_frame)
	    x_new_focus_frame (0);
	  break;

#else /* ! defined (HAVE_X11) */

	case EnterWindow:
	  if ((event.detail & 0xFF) == 1)
	    break;		/* Coming from our own subwindow */
	  if (event.subwindow != 0)
	    break;		/* Entering our own subwindow.  */

	  {
	    f = x_window_to_frame (event.window);
	    x_mouse_frame = f;

	    x_new_focus_frame (f);
	  }
	  break;

	case LeaveWindow:
	  if ((event.detail & 0xFF) == 1)
	    break;		/* Entering our own subwindow */
	  if (event.subwindow != 0)
	    break;		/* Leaving our own subwindow.  */

	  x_mouse_frame = 0;
	  if (x_focus_frame == 0
	      && x_input_frame != 0
	      && x_input_frame == x_window_to_frame (event.window)
	      && event.window == FRAME_X_WINDOW (x_input_frame))
	    {
	      f = x_input_frame;
	      x_input_frame = 0;
	      if (f)
		frame_unhighlight (f);
	    }
	  break;
#endif /* ! defined (HAVE_X11) */

#ifdef HAVE_X11
	case MotionNotify:
	  {
	    f = x_window_to_frame (event.xmotion.window);
	    if (f)
	      note_mouse_position (f, &event.xmotion);
#if 0
	    else if ((f = x_window_to_scrollbar (event.xmotion.window,
						 &part, &prefix)))
	      {
		What should go here?
	      }
#endif /* ! 0 */
	  }
	  break;

	case ConfigureNotify:
	  {
	    int rows, columns;
	    f = x_window_to_frame (event.xconfigure.window);
	    if (!f)
	      break;

	    columns = ((event.xconfigure.width -
			(2 * f->display.x->internal_border_width)
			- f->display.x->v_scrollbar_width)
		       / FONT_WIDTH (f->display.x->font));
	    rows = ((event.xconfigure.height -
		     (2 * f->display.x->internal_border_width)
		     - f->display.x->h_scrollbar_height)
		    / FONT_HEIGHT (f->display.x->font));

	    /* Even if the number of character rows and columns has
	       not changed, the font size may have changed, so we need
	       to check the pixel dimensions as well.  */
	    if (columns != f->width
		|| rows != f->height
		|| event.xconfigure.width != f->display.x->pixel_width
		|| event.xconfigure.height != f->display.x->pixel_height)
	      {
		change_frame_size (f, rows, columns, 0, 1);
		x_resize_scrollbars (f);
		SET_FRAME_GARBAGED (f);
	      }

	    f->display.x->pixel_width = event.xconfigure.width;
	    f->display.x->pixel_height = event.xconfigure.height;
	    f->display.x->left_pos = event.xconfigure.x;
	    f->display.x->top_pos = event.xconfigure.y;
	    break;
	  }

	case ButtonPress:
	case ButtonRelease:
	  {
	    /* If we decide we want to generate an event to be seen
	       by the rest of Emacs, we put it here.  */
	    struct input_event emacs_event;
	    emacs_event.kind = no_event;

	    f = x_window_to_frame (event.xbutton.window);
	    if (f)
	      if (!x_focus_frame || (f == x_focus_frame))
		construct_mouse_click (&emacs_event,
				       &event, f, Qnil, 0);
	      else
		continue;
	    else
	      if ((f = x_window_to_scrollbar (event.xbutton.window,
					      &part, &prefix)))
		{
		  if (!x_focus_frame || (selected_frame == x_focus_frame))
		    construct_mouse_click (&emacs_event,
					   &event, f, part, prefix);
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

#else /* ! defined (HAVE_X11) */
	case ButtonPressed:
	case ButtonReleased:
	  f = x_window_to_frame (event.window);
	  if (f)
	    {
	      if (event.window == f->display.x->icon_desc)
		{
		  x_make_frame_visible (f);

		  if (warp_mouse_on_deiconify)
		    XWarpMouse (FRAME_X_WINDOW (f), 10, 10);
		  break;
		}
	      if (event.window == FRAME_X_WINDOW (f))
		{
		  if (f->auto_raise)
		    x_raise_frame (f);
		}
	    }
	  enqueue_event (&event, &x_mouse_queue);
	  if (numchars >= 2)
	    {
	      bufp->kind = ascii_keystroke;
	      bufp->code = (char) 'X' & 037; /* C-x */
	      bufp->frame = f;
	      XSET (bufp->time, Lisp_Int, event.xkey.time);
	      bufp++;

	      bufp->kind = ascii_keystroke;
	      bufp->code = (char) 0; /* C-@ */
	      bufp->frame = f;
	      XSET (bufp->time, Lisp_Int, event.xkey.time);
	      bufp++;

	      count += 2;
	      numchars -= 2;
	    }
	  break;
#endif /* ! defined (HAVE_X11) */

#ifdef HAVE_X11

	case CirculateNotify:
	  break;
	case CirculateRequest:
	  break;

#endif /* ! defined (HAVE_X11) */

	case MappingNotify:
	  /* Someone has changed the keyboard mapping - update the
	     local cache.  */
	  switch (event.xmapping.request)
	    {
	    case MappingModifier:
	      x_find_modifier_meanings ();
	      /* This is meant to fall through.  */
	    case MappingKeyboard:
	      XRefreshKeyboardMapping (&event.xmapping);
	    }
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
#endif /* ! defined (HAVE_SELECT) */
#endif /* ! 0 */

  if (updating_frame == 0)
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
  struct frame *f;
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
	  f = x_window_to_frame (event.window);
	  if (event.window == f->display.x->icon_desc)
	    {
	      refreshicon (f);
	      break;
	    }
	  if (event.window == FRAME_X_WINDOW (f))
	    {
	      expose_all_windows = 1;
	      f->display.x->needs_exposure = 1;
	      break;
	    }
	  break;

	case ExposeRegion:
	  if (event.subwindow != 0)
	    break;			/* duplicate event */
	  f = x_window_to_frame (event.window);
	  if (event.window == f->display.x->icon_desc)
	    {
	      refreshicon (f);
	      break;
	    }
	  /* If window already needs full redraw, ignore this rectangle.  */
	  if (expose_all_windows && f->display.x->needs_exposure)
	    break;
	  /* Put the event on the queue of rectangles to redraw.  */
	  if (enqueue_event (&event, &x_expose_queue))
	    /* If it is full, we can't record the rectangle,
	       so redraw this entire window.  */
	    {
	      /* Say must check all windows' needs_exposure flags.  */
	      expose_all_windows = 1;
	      f->display.x->needs_exposure = 1;
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
x_draw_box (f)
     struct frame *f;
{
  int left = f->cursor_x * FONT_WIDTH (f->display.x->font)
    + f->display.x->internal_border_width;
  int top = f->cursor_y * FONT_HEIGHT (f->display.x->font)
    + f->display.x->internal_border_width;
  int width = FONT_WIDTH (f->display.x->font);
  int height = FONT_HEIGHT (f->display.x->font);

#ifdef HAVE_X11
  /* Perhaps we should subtract 1 from width and height... */
  XDrawRectangle (x_current_display, FRAME_X_WINDOW (f),
		  f->display.x->cursor_gc,
		  left, top, width - 1, height - 1);
#else /* ! defined (HAVE_X11) */
  XPixSet (FRAME_X_WINDOW (f),
	   left, top, width, 1,
	   f->display.x->cursor_pixel);

  XPixSet (FRAME_X_WINDOW (f),
	   left, top, 1, height,
	   f->display.x->cursor_pixel);

  XPixSet (FRAME_X_WINDOW (f),
	   left+width-1, top, 1, height,
	   f->display.x->cursor_pixel);

  XPixSet (FRAME_X_WINDOW (f),
	   left, top+height-1, width, 1,
	   f->display.x->cursor_pixel);
#endif /* ! defined (HAVE_X11) */
}

/* Clear the cursor of frame F to background color,
   and mark the cursor as not shown.
   This is used when the text where the cursor is
   is about to be rewritten.  */

static void
clear_cursor (f)
     struct frame *f;
{
  int mask;

  if (! f->visible
      || f->phys_cursor_x < 0)
    return;

#ifdef HAVE_X11
  x_display_cursor (f, 0);
#if 0
  XClearArea (x_current_display, FRAME_X_WINDOW (f),
	      f->phys_cursor_x * FONT_WIDTH (f->display.x->font)
	      + f->display.x->internal_border_width,
	      f->phys_cursor_y * FONT_HEIGHT (f->display.x->font)
	      + f->display.x->internal_border_width,
	      FONT_WIDTH (f->display.x->font) + 1, FONT_HEIGHT (f->display.x->font) + 1, False);
#endif /* ! 0 */
#else /* ! defined (HAVE_X11) */
  XPixSet (FRAME_X_WINDOW (f),
	   f->phys_cursor_x * FONT_WIDTH (f->display.x->font) + f->display.x->internal_border_width,
	   f->phys_cursor_y * FONT_HEIGHT (f->display.x->font) + f->display.x->internal_border_width,
	   FONT_WIDTH (f->display.x->font), FONT_HEIGHT (f->display.x->font),
	   f->display.x->background_pixel);
#endif /* ! defined (HAVE_X11) */
  f->phys_cursor_x = -1;
}

static void
x_display_bar_cursor (f, on)
     struct frame *f;
     int on;
{
  register int phys_x = f->phys_cursor_x;
  register int phys_y = f->phys_cursor_y;
  register int x1;
  register int y1;
  register int y2;

  if (! f->visible || (! on && f->phys_cursor_x < 0))
    return;

#ifdef HAVE_X11
  if (phys_x >= 0 &&
      (!on || phys_x != f->cursor_x || phys_y != f->cursor_y))
    {
      x1 = phys_x * FONT_WIDTH (f->display.x->font)
	+ f->display.x->internal_border_width;
      y1 = phys_y * FONT_HEIGHT (f->display.x->font)
	+ f->display.x->internal_border_width - 1;
      y2 = y1 + FONT_HEIGHT (f->display.x->font) + 1;

      XDrawLine (x_current_display, FRAME_X_WINDOW (f),
		 f->display.x->reverse_gc, x1, y1, x1, y2);

      f->phys_cursor_x = phys_x = -1;
    }

  if (on && f == x_highlight_frame)
    {
      x1 = f->cursor_x * FONT_WIDTH (f->display.x->font)
	+ f->display.x->internal_border_width;
      y1 = f->cursor_y * FONT_HEIGHT (f->display.x->font)
	+ f->display.x->internal_border_width - 1;
      y2 = y1 + FONT_HEIGHT (f->display.x->font) + 1;

      XDrawLine (x_current_display, FRAME_X_WINDOW (f),
		 f->display.x->cursor_gc, x1, y1, x1, y2);

      f->phys_cursor_x = f->cursor_x;
      f->phys_cursor_y = f->cursor_y;
    }
#else /* ! defined (HAVE_X11) */
  Give it up, dude.
#endif /* ! defined (HAVE_X11) */
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
	      (column * FONT_WIDTH (f->display.x->font)
	       + f->display.x->internal_border_width),
	      (row * FONT_HEIGHT (f->display.x->font)
	       + f->display.x->internal_border_width),
	      &glyph, 1, highlight, f->display.x->font);
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

  /* If we're not updating, then we want to use the current frame's
     cursor position, not our local idea of where the cursor ought to be.  */
  if (f != updating_frame)
    {
      curs_x = FRAME_CURSOR_X (f);
      curs_y = FRAME_CURSOR_Y (f);
    }

  if (! f->visible)
    return;

  /* If cursor is off and we want it off, return quickly.  */
  if (!on && f->phys_cursor_x < 0)
    return;

  /* If cursor is currently being shown and we don't want it to be
     or it is in the wrong place,
     or we want a hollow box and it's not so, (pout!)
     erase it.  */
  if (f->phys_cursor_x >= 0
      && (!on
	  || f->phys_cursor_x != curs_x
	  || f->phys_cursor_y != curs_y
	  || (f->display.x->text_cursor_kind != hollow_box_cursor
	      && (f != x_highlight_frame))))
    {
      /* Erase the cursor by redrawing the character underneath it.  */
      x_draw_single_glyph (f, f->phys_cursor_y, f->phys_cursor_x,
			   f->phys_cursor_glyph,
			   current_glyphs->highlight[f->phys_cursor_y]);
      f->phys_cursor_x = -1;
    }

  /* If we want to show a cursor,
     or we want a box cursor and it's not so,
     write it in the right place.  */
  if (on
      && (f->phys_cursor_x < 0
	  || (f->display.x->text_cursor_kind != filled_box_cursor
	      && f == x_highlight_frame)))
    {
      f->phys_cursor_glyph
	= ((current_glyphs->enable[curs_y]
	    && curs_x < current_glyphs->used[curs_y])
	   ? current_glyphs->glyphs[curs_y][curs_x]
	   : SPACEGLYPH);
      if (f != x_highlight_frame)
	{
	  x_draw_box (f);
	  f->display.x->text_cursor_kind = hollow_box_cursor;
	}
      else
	{
	  x_draw_single_glyph (f, curs_y, curs_x,
			       f->phys_cursor_glyph, 2);
	  f->display.x->text_cursor_kind = filled_box_cursor;
	}

      f->phys_cursor_x = curs_x;
      f->phys_cursor_y = curs_y;
    }

  if (updating_frame != f)
    XFlushQueue ();
}

extern Lisp_Object Vbar_cursor;

x_display_cursor (f, on)
     struct frame *f;
     int on;
{
  if (EQ (Vbar_cursor, Qnil))
    x_display_box_cursor (f, on);
  else
    x_display_bar_cursor (f, on);
}

/* Icons.  */

/* Refresh bitmap kitchen sink icon for frame F
   when we get an expose event for it. */

refreshicon (f)
     struct frame *f;
{
#ifdef HAVE_X11
  /* Normally, the window manager handles this function. */
#else /* ! defined (HAVE_X11) */
  int mask;

  if (f->display.x->icon_bitmap_flag)
    XBitmapBitsPut (f->display.x->icon_desc, 0,  0, sink_width, sink_height,
		    sink_bits, BlackPixel, WHITE_PIX_DEFAULT, 
		    icon_bitmap, GXcopy, AllPlanes);
  else
    {
      extern struct frame *selected_frame;
      struct Lisp_String *str;
      unsigned char *string;

      string
	= XSTRING (XBUFFER (XWINDOW (f->selected_window)->buffer)->name)->data;

      if (f->display.x->icon_label != string)
	{
	  f->display.x->icon_label = string;
	  XChangeWindow (f->display.x->icon_desc,
			 XQueryWidth (string, icon_font_info->id) + 10,
			 icon_font_info->height + 10);
	}

      XText (f->display.x->icon_desc, 5, 5, string,
	     str->size, icon_font_info->id,
	     BLACK_PIX_DEFAULT, WHITE_PIX_DEFAULT);
    }
  XFlushQueue ();
#endif /* ! defined (HAVE_X11) */
}

/* Make the x-window of frame F use the kitchen-sink icon
   that's a window generated by Emacs.  */

int
x_bitmap_icon (f)
     struct frame *f;
{
  int mask;
  Window icon_window;

  if (FRAME_X_WINDOW (f) == 0)
    return 1;

#ifdef HAVE_X11
  if (icon_bitmap)
    XFreePixmap (x_current_display, icon_bitmap);
  
  icon_bitmap =
    XCreateBitmapFromData (x_current_display, FRAME_X_WINDOW (f),
			   gnu_bits, gnu_width, gnu_height);
  x_wm_set_icon_pixmap (f, icon_bitmap);
  f->display.x->icon_bitmap_flag = 1;
#else /* ! defined (HAVE_X11) */
  if (f->display.x->icon_desc)
    {
      XClearIconWindow (FRAME_X_WINDOW (f));
      XDestroyWindow (f->display.x->icon_desc);
    }

  icon_window = XCreateWindow (f->display.x->parent_desc,
			       0, 0, sink_width, sink_height,
			       2, WhitePixmap, (Pixmap) NULL);

  if (icon_window == 0)
    return 1;

  XSetIconWindow (FRAME_X_WINDOW (f), icon_window);
  XSelectInput (icon_window, ExposeWindow | UnmapWindow);

  f->display.x->icon_desc = icon_window;
  f->display.x->icon_bitmap_flag = 1;

  if (icon_bitmap == 0)
    icon_bitmap
      = XStoreBitmap (sink_mask_width, sink_mask_height, sink_mask_bits);
#endif /* ! defined (HAVE_X11) */

  return 0;
}


/* Make the x-window of frame F use a rectangle with text.  */

int
x_text_icon (f, icon_name)
     struct frame *f;
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
#endif /* WhitePixel */

#ifndef BlackPixel
#define BlackPixel 0
#endif /* BlackPixel */
#endif /* HAVE_X11 */
  
  if (FRAME_X_WINDOW (f) == 0)
    return 1;

  if (icon_font_info == 0)
    icon_font_info
      = XGetFont (XGetDefault (XDISPLAY
			       (char *) XSTRING (invocation_name)->data,
			       "BodyFont"));

#ifdef HAVE_X11
  if (icon_name)
    f->display.x->icon_label = icon_name;
  else
    if (! f->display.x->icon_label)
      f->display.x->icon_label = " *emacs* ";
  
  XSetIconName (x_current_display, FRAME_X_WINDOW (f),
		(char *) f->display.x->icon_label);
  
  f->display.x->icon_bitmap_flag = 0;
  x_wm_set_icon_pixmap (f, 0);
#else /* ! defined (HAVE_X11) */
  if (f->display.x->icon_desc)
    {
      XClearIconWindow (XDISPLAY FRAME_X_WINDOW (f));
      XDestroyWindow (XDISPLAY f->display.x->icon_desc);
    }

  if (icon_name)
    f->display.x->icon_label = (unsigned char *) icon_name;
  else
    if (! f->display.x->icon_label)
      f->display.x->icon_label = XSTRING (f->name)->data;

  width = XStringWidth (f->display.x->icon_label, icon_font_info, 0, 0);
  icon_window = XCreateWindow (f->display.x->parent_desc,
			       f->display.x->left_pos,
			       f->display.x->top_pos,
			       width + 10, icon_font_info->height + 10,
			       2, BlackPixmap, WhitePixmap);

  if (icon_window == 0)
    return 1;

  XSetIconWindow (FRAME_X_WINDOW (f), icon_window);
  XSelectInput (icon_window, ExposeWindow | ExposeRegion | UnmapWindow | ButtonPressed);

  f->display.x->icon_desc = icon_window;
  f->display.x->icon_bitmap_flag = 0;
  f->display.x->icon_label = 0;
#endif /* ! defined (HAVE_X11) */

  return 0;
}

/* Handling X errors.  */

/* A handler for SIGPIPE, when it occurs on the X server's connection.
   This basically does an orderly shutdown of Emacs.  */

static SIGTYPE
x_connection_closed ()
{
  if (_Xdebug)
    abort ();
  else
    Fkill_emacs (make_number (70));
}

/* An X error handler which prints an error message and then kills Emacs.  
   This is what's normally installed as Xlib's handler for protocol and 
   I/O errors.  */
static int
x_error_quitter (display, error)
     Display *display;
     XErrorEvent *error;
{
  char buf[256];

  /* Note that there is no real way portable across R3/R4 to get the 
     original error handler.  */

  XGetErrorText (display, error->error_code, buf, sizeof (buf));
  fprintf (stderr, "X protocol error: %s on protocol request %d\n",
	   buf, error->request_code);

  x_connection_closed ();
}

/* A buffer for storing X error messages.  */
static char (*x_caught_error_message)[200];

/* An X error handler which stores the error message in
   x_caught_error_message.  This is what's installed when
   x_catch_errors is in effect.  */
static int
x_error_catcher (display, error)
     Display *display;
     XErrorEvent *error;
{
  XGetErrorText (display, error->error_code,
		 *x_caught_error_message, sizeof (*x_caught_error_message));
}


/* Begin trapping X errors.

   After calling this function, X protocol errors no longer cause
   Emacs to exit; instead, they are recorded in x_cfc_error_message.

   Calling x_check_errors signals an Emacs error if an X error has
   occurred since the last call to x_catch_errors or x_check_errors.

   Calling x_uncatch_errors resumes the normal error handling.  */

void x_catch_errors(), x_check_errors (), x_uncatch_errors ();

void
x_catch_errors ()
{
  /* Make sure any errors from previous requests have been dealt with.  */
  XSync (x_current_display, False);

  /* Set up the error buffer.  */
  x_caught_error_message =
    (char (*)[]) xmalloc (sizeof (*x_caught_error_message));
  (*x_caught_error_message)[0] = '\0';

  /* Install our little error handler.  */
  XHandleError (x_error_catcher);
}

/* If any X protocol errors have arrived since the last call to
   x_catch_errors or x_check_errors, signal an Emacs error using
   sprintf (a buffer, FORMAT, the x error message text) as the text.  */
void
x_check_errors (format)
     char *format;
{
  /* Make sure to catch any errors incurred so far.  */
  XSync (x_current_display, False);

  if ((*x_caught_error_message)[0])
    {
      char buf[256];

      sprintf (buf, format, *x_caught_error_message);
      free (x_caught_error_message);

      x_uncatch_errors ();
      error (buf);
    }
}

void
x_uncatch_errors ()
{
  free (x_caught_error_message);
  XHandleError (x_error_quitter);
}

#if 0
static unsigned int x_wire_count;
x_trace_wire ()
{
  fprintf (stderr, "Lib call: %d\n", ++x_wire_count);
}
#endif /* ! 0 */


/* Set the font of the x-window specified by frame F
   to the font named NEWNAME.  This is safe to use
   even before F has an actual x-window.  */

#ifdef HAVE_X11

/* A table of all the fonts we have already loaded.  */
static XFontStruct **x_font_table;

/* The current capacity of x_font_table.  */
static int x_font_table_size;

/* The number of fonts actually stored in x_font_table.
   x_font_table[n] is used and valid iff 0 <= n < n_fonts.
   0 <= n_fonts <= x_font_table_size.  */
static int n_fonts;

x_new_font (f, fontname)
     struct frame *f;
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
    f->display.x->font = x_font_table[already_loaded];
  
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

      f->display.x->font = x_font_table[n_fonts++] = font;
    }
  
  /* Free the information from XListFontsWithInfo.  The data
     we actually retain comes from XLoadQueryFont.  */
  XFreeFontInfo (font_names, font_info, n_matching_fonts);

  /* Now make the frame display the given font.  */
  if (FRAME_X_WINDOW (f) != 0)
    {
      XSetFont (x_current_display, f->display.x->normal_gc,
		f->display.x->font->fid);
      XSetFont (x_current_display, f->display.x->reverse_gc,
		f->display.x->font->fid);
      XSetFont (x_current_display, f->display.x->cursor_gc,
		f->display.x->font->fid);

      x_set_window_size (f, f->width, f->height);
    }

  return 0;
}
#else /* ! defined (HAVE_X11) */
x_new_font (f, newname)
     struct frame *f;
     register char *newname;
{
  FONT_TYPE *temp;
  int mask;

  temp = XGetFont (newname);
  if (temp == (FONT_TYPE *) 0)
    return 1;

  if (f->display.x->font)
    XLoseFont (f->display.x->font);

  f->display.x->font = temp;

  if (FRAME_X_WINDOW (f) != 0)
    x_set_window_size (f, f->width, f->height);

  return 0;
}
#endif /* ! defined (HAVE_X11) */

x_calc_absolute_position (f)
     struct frame *f;
{
#ifdef HAVE_X11
  if (f->display.x->left_pos < 0)
    f->display.x->left_pos
      = x_screen_width - PIXEL_WIDTH (f) + f->display.x->left_pos;

  if (f->display.x->top_pos < 0)
    f->display.x->top_pos
      = x_screen_height - PIXEL_HEIGHT (f) + f->display.x->top_pos;
#else /* ! defined (HAVE_X11) */
  WINDOWINFO_TYPE parentinfo;

  XGetWindowInfo (FRAME_X_WINDOW (f), &parentinfo);

  if (f->display.x->left_pos < 0)
    f->display.x->left_pos = parentinfo.width + (f->display.x->left_pos + 1)
      - PIXEL_WIDTH (f) - 2 * f->display.x->internal_border_width;

  if (f->display.x->top_pos < 0)
    f->display.x->top_pos = parentinfo.height + (f->display.x->top_pos + 1)
      - PIXEL_HEIGHT (f) - 2 * f->display.x->internal_border_width;
#endif /* ! defined (HAVE_X11) */
}

x_set_offset (f, xoff, yoff)
     struct frame *f;
     register int xoff, yoff;
{
  f->display.x->top_pos = yoff;
  f->display.x->left_pos = xoff;
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  XMoveWindow (XDISPLAY FRAME_X_WINDOW (f),
	       f->display.x->left_pos, f->display.x->top_pos);
#ifdef HAVE_X11
  x_wm_set_size_hint (f, 0);
#endif /* ! defined (HAVE_X11) */
  UNBLOCK_INPUT;
}

/* Call this to change the size of frame F's x-window. */

x_set_window_size (f, cols, rows)
     struct frame *f;
     int cols, rows;
{
  int pixelwidth, pixelheight;
  int mask;
  int ibw = f->display.x->internal_border_width;

  BLOCK_INPUT;

  check_frame_size (f, &rows, &cols);
  pixelwidth =  (cols * FONT_WIDTH (f->display.x->font) + 2 * ibw
		 + f->display.x->v_scrollbar_width);
  pixelheight = (rows * FONT_HEIGHT (f->display.x->font) + 2 * ibw
		 + f->display.x->h_scrollbar_height);

#ifdef HAVE_X11
  x_wm_set_size_hint (f, 0);
#endif /* ! defined (HAVE_X11) */
  XChangeWindowSize (FRAME_X_WINDOW (f), pixelwidth, pixelheight);

  /* Now, strictly speaking, we can't be sure that this is accurate,
     but the window manager will get around to dealing with the size
     change request eventually, and we'll hear how it went when the
     ConfigureNotify event gets here.  */
  FRAME_WIDTH (f) = cols;
  FRAME_WIDTH (f) = rows;
  PIXEL_WIDTH (f) = pixelwidth;
  PIXEL_HEIGHT (f) = pixelheight;

  XFlushQueue ();
  UNBLOCK_INPUT;
}

#ifndef HAVE_X11
x_set_resize_hint (f)
     struct frame *f;
{

  XSetResizeHint (FRAME_X_WINDOW (f), 2 * f->display.x->internal_border_width,
		  2 * f->display.x->internal_border_width,
		  FONT_WIDTH (f->display.x->font), FONT_HEIGHT (f->display.x->font));
}
#endif /* HAVE_X11 */


x_set_mouse_position (f, x, y)
     struct frame *f;
     int x, y;
{
  int pix_x, pix_y;

  x_raise_frame (f);

  if (x < 0)
    pix_x = (FRAME_WIDTH (f)
             * FONT_WIDTH (f->display.x->font)
             + 2 * f->display.x->internal_border_width
             + f->display.x->v_scrollbar_width) / 2;
  else
    pix_x = x * FONT_WIDTH (f->display.x->font) + 2; /* add 2 pixels to each
       						 dimension to move the
       						 mouse into the char
       						 cell */

  if (y < 0)
    pix_y = (FRAME_HEIGHT (f)
             * FONT_HEIGHT (f->display.x->font)
             + 2 * f->display.x->internal_border_width
             + f->display.x->h_scrollbar_height) / 2;
  else
    pix_y = y * FONT_HEIGHT (f->display.x->font) + 2;

  BLOCK_INPUT;
  x_mouse_x = x;
  x_mouse_y = y;

  XWarpMousePointer (FRAME_X_WINDOW (f), pix_x, pix_y);
  UNBLOCK_INPUT;
}

#ifdef HAVE_X11
x_focus_on_frame (f)
     struct frame *f;
{
  x_raise_frame (f);
#if 0
  /* I don't think that the ICCCM allows programs to do things like this
     without the interaction of the window manager.  Whatever you end up
     doing with this code, do it to x_unfocus_frame too.  */
  XSetInputFocus (x_current_display, FRAME_X_WINDOW (f),
		  RevertToPointerRoot, CurrentTime);
#endif /* ! 0 */
}

x_unfocus_frame (f)
     struct frame *f;
{
#if 0
  /* Look at the remarks in x_focus_on_frame.  */
  if (x_focus_frame == f)
    XSetInputFocus (x_current_display, PointerRoot,
		    RevertToPointerRoot, CurrentTime);
#endif /* ! 0 */
}

#endif /* ! defined (HAVE_X11) */

/* Raise frame F.  */

x_raise_frame (f)
     struct frame *f;
{
  if (f->async_visible)
    {
      BLOCK_INPUT;
      XRaiseWindow (XDISPLAY FRAME_X_WINDOW (f));
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Lower frame F.  */

x_lower_frame (f)
     struct frame *f;
{
  if (f->async_visible)
    {
      BLOCK_INPUT;
      XLowerWindow (XDISPLAY FRAME_X_WINDOW (f));
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Change from withdrawn state to mapped state. */

x_make_frame_visible (f)
     struct frame *f;
{
  int mask;

  BLOCK_INPUT;

  if (! FRAME_VISIBLE_P (f))
    {
#ifdef HAVE_X11
      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, NormalState);

      XMapWindow (XDISPLAY FRAME_X_WINDOW (f));
      if (f->display.x->v_scrollbar != 0 || f->display.x->h_scrollbar != 0)
	XMapSubwindows (x_current_display, FRAME_X_WINDOW (f));
#else /* ! defined (HAVE_X11) */
      XMapWindow (XDISPLAY FRAME_X_WINDOW (f));
      if (f->display.x->icon_desc != 0)
	XUnmapWindow (f->display.x->icon_desc);

      /* Handled by the MapNotify event for X11 */
      f->async_visible = 1;
      f->async_iconified = 0;

      /* NOTE: this may cause problems for the first frame. */
      XTcursor_to (0, 0);
#endif /* ! defined (HAVE_X11) */
    }

  XFlushQueue ();

  UNBLOCK_INPUT;
}

/* Change from mapped state to withdrawn state. */

x_make_frame_invisible (f)
     struct frame *f;
{
  int mask;

  if (! f->async_visible)
    return;

  BLOCK_INPUT;

#ifdef HAVE_X11R4

  if (! XWithdrawWindow (x_current_display, FRAME_X_WINDOW (f),
			 DefaultScreen (x_current_display)))
    {
      UNBLOCK_INPUT_RESIGNAL;
      error ("can't notify window manager of window withdrawl");
    }

#else /* ! defined (HAVE_X11R4) */
#ifdef HAVE_X11

  /*  Tell the window manager what we're going to do.  */
  if (! EQ (Vx_no_window_manager, Qt))
    {
      XEvent unmap;

      unmap.xunmap.type = UnmapNotify;
      unmap.xunmap.window = FRAME_X_WINDOW (f);
      unmap.xunmap.event = DefaultRootWindow (x_current_display);
      unmap.xunmap.from_configure = False;
      if (! XSendEvent (x_current_display,
			DefaultRootWindow (x_current_display),
			False,
			SubstructureRedirectMask|SubstructureNotifyMask,
			&unmap))
	{
	  UNBLOCK_INPUT_RESIGNAL;
	  error ("can't notify window manager of withdrawal");
	}
    }

  /* Unmap the window ourselves.  Cheeky!  */
  XUnmapWindow (x_current_display, FRAME_X_WINDOW (f));

#else /* ! defined (HAVE_X11) */

  XUnmapWindow (FRAME_X_WINDOW (f));
  f->async_visible = 0;		/* Handled by the UnMap event for X11 */
  if (f->display.x->icon_desc != 0)
    XUnmapWindow (f->display.x->icon_desc);

#endif /* ! defined (HAVE_X11) */
#endif /* ! defined (HAVE_X11R4) */

  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Window manager communication.  Created in Fx_open_connection. */
extern Atom Xatom_wm_change_state;

/* Change window state from mapped to iconified. */

x_iconify_frame (f)
     struct frame *f;
{
  int mask;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

#ifdef HAVE_X11
  /* Since we don't know which revision of X we're running, we'll use both
     the X11R3 and X11R4 techniques.  I don't know if this is a good idea.  */

  /* X11R4: send a ClientMessage to the window manager using the
     WM_CHANGE_STATE type.  */
  {
    XEvent message;
    
    message.xclient.window = FRAME_X_WINDOW (f);
    message.xclient.type = ClientMessage;
    message.xclient.message_type = Xatom_wm_change_state;
    message.xclient.format = 32;
    message.xclient.data.l[0] = IconicState;

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

  /* X11R3: set the initial_state field of the window manager hints to 
     IconicState.  */
  x_wm_set_window_state (f, IconicState);

  f->async_iconified = 1;
#else /* ! defined (HAVE_X11) */
  XUnmapWindow (XDISPLAY FRAME_X_WINDOW (f));

  f->async_visible = 0;		/* Handled in the UnMap event for X11. */
  if (f->display.x->icon_desc != 0)
    {
      XMapWindow (XDISPLAY f->display.x->icon_desc);
      refreshicon (f);
    }
#endif /* ! defined (HAVE_X11) */

  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Destroy the X window of frame F.
   DISPL is the former f->display (since f->display
   has already been nulled out).  */

x_destroy_window (f, displ)
     struct frame *f;
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
  if (f == x_focus_frame)
    x_focus_frame = 0;
  if (f == x_highlight_frame)
    x_highlight_frame = 0;
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
#endif /* HAVE_X11 */

#ifdef HAVE_X11

x_wm_set_size_hint (f, prompting)
     struct frame *f;
     long prompting;
{
  XSizeHints size_hints;
  Window window = FRAME_X_WINDOW (f);

  size_hints.flags = PResizeInc | PMinSize | PMaxSize;

  flexlines = f->height;

  size_hints.x = f->display.x->left_pos;
  size_hints.y = f->display.x->top_pos;
  size_hints.height = PIXEL_HEIGHT (f);
  size_hints.width = PIXEL_WIDTH (f);
  size_hints.width_inc = FONT_WIDTH (f->display.x->font);
  size_hints.height_inc = FONT_HEIGHT (f->display.x->font);
  size_hints.max_width =
    (x_screen_width - ((2 * f->display.x->internal_border_width)
		       + f->display.x->v_scrollbar_width));
  size_hints.max_height =
    (x_screen_height - ((2 * f->display.x->internal_border_width)
			+ f->display.x->h_scrollbar_height));
  {
    int base_width, base_height;

    base_width = ((2 * f->display.x->internal_border_width)
		  + f->display.x->v_scrollbar_width);
    base_height = ((2 * f->display.x->internal_border_width)
		   + f->display.x->h_scrollbar_height);

    {
      int min_rows = 0, min_cols = 0;
      check_frame_size (f, &min_rows, &min_cols);

      /* The window manager uses the base width hints to calculate the
	 current number of rows and columns in the frame while
	 resizing; min_width and min_height aren't useful for this
	 purpose, since they might not give the dimensions for a
	 zero-row, zero-column frame.

	 We use the base_width and base_height members if we have
	 them; otherwise, we set the min_width and min_height members
	 to the size for a zero x zero frame.  */

#ifdef HAVE_X11R4
      size_hints.flags |= PBaseSize;
      size_hints.base_width = base_width;
      size_hints.base_height = base_height;
      size_hints.min_width  = base_width + min_cols * size_hints.width_inc;
      size_hints.min_height = base_height + min_rows * size_hints.height_inc;
#else
      size_hints.min_width = base_width;
      size_hints.min_height = base_height;
#endif
    }

  }

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

#ifdef HAVE_X11R4
  XSetWMNormalHints (x_current_display, window, &size_hints);
#else
  XSetNormalHints (x_current_display, window, &size_hints);
#endif
}

/* Used for IconicState or NormalState */
x_wm_set_window_state (f, state)
     struct frame *f;
     int state;
{
  Window window = FRAME_X_WINDOW (f);

  f->display.x->wm_hints.flags |= StateHint;
  f->display.x->wm_hints.initial_state = state;

  XSetWMHints (x_current_display, window, &f->display.x->wm_hints);
}

x_wm_set_icon_pixmap (f, icon_pixmap)
     struct frame *f;
     Pixmap icon_pixmap;
{
  Window window = FRAME_X_WINDOW (f);

  f->display.x->wm_hints.flags |= IconPixmapHint;
  f->display.x->wm_hints.icon_pixmap = icon_pixmap ? icon_pixmap : None;

  XSetWMHints (x_current_display, window, &f->display.x->wm_hints);
}

x_wm_set_icon_position (f, icon_x, icon_y)
     struct frame *f;
     int icon_x, icon_y;
{
  Window window = FRAME_X_WINDOW (f);

  f->display.x->wm_hints.flags |= IconPositionHint;
  f->display.x->wm_hints.icon_x = icon_x;
  f->display.x->wm_hints.icon_y = icon_y;

  XSetWMHints (x_current_display, window, &f->display.x->wm_hints);
}


void
x_term_init (display_name)
     char *display_name;
{
  Lisp_Object frame;
  char *defaultvalue;
#ifdef F_SETOWN
  extern int old_fcntl_owner;
#endif /* ! defined (F_SETOWN) */
  
  x_focus_frame = x_highlight_frame = 0;

  x_current_display = XOpenDisplay (display_name);
  if (x_current_display == 0)
    fatal ("X server %s not responding; check the DISPLAY environment variable or use \"-d\"\n",
	   display_name);

#ifdef HAVE_X11
  {
    int hostname_size = 256;

    hostname = (char *) xmalloc (hostname_size);

#if 0
    XSetAfterFunction (x_current_display, x_trace_wire);
#endif /* ! 0 */

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

  /* Figure out which modifier bits mean what.  */
  x_find_modifier_meanings ();
  
  /* Watch for PropertyNotify events on the root window; we use them
     to figure out when to invalidate our cache of the cut buffers.  */
  x_watch_cut_buffer_cache ();

  dup2 (ConnectionNumber (x_current_display), 0);

#ifndef SYSV_STREAMS
  /* Streams somehow keeps track of which descriptor number
     is being used to talk to X.  So it is not safe to substitute
     descriptor 0.  But it is safe to make descriptor 0 a copy of it.  */
  close (ConnectionNumber (x_current_display));
  ConnectionNumber (x_current_display) = 0;	/* Looks a little strange?
						 * check the def of the macro;
						 * it is a genuine lvalue */
#endif /* SYSV_STREAMS */

#endif /* ! defined (HAVE_X11) */
  
#ifdef F_SETOWN
  old_fcntl_owner = fcntl (0, F_GETOWN, 0);
#ifdef F_SETOWN_SOCK_NEG
  fcntl (0, F_SETOWN, -getpid ());	/* stdin is a socket here */
#else /* ! defined (F_SETOWN_SOCK_NEG) */
  fcntl (0, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN_SOCK_NEG) */
#endif /* ! defined (F_SETOWN) */

#ifdef SIGIO
  init_sigio ();
#endif /* ! defined (SIGIO) */

  /* Must use interrupt input because we cannot otherwise
     arrange for C-g to be noticed immediately.
     We cannot connect it to SIGINT.  */
  Fset_input_mode (Qt, Qnil, Qt, Qnil);

  expose_all_windows = 0;

  clear_frame_hook = XTclear_frame;
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
  frame_rehighlight_hook = XTframe_rehighlight;
  mouse_position_hook = XTmouse_position;
  
  scroll_region_ok = 1;		/* we'll scroll partial frames */
  char_ins_del_ok = 0;		/* just as fast to write the line */
  line_ins_del_ok = 1;		/* we'll just blt 'em */
  fast_clear_end_of_line = 1;	/* X does this well */
  memory_below_frame = 0;	/* we don't remember what scrolls 
				   off the bottom */
  baud_rate = 19200;

  /* Note that there is no real way portable across R3/R4 to get the 
     original error handler.  */
  XHandleError (x_error_quitter);
  XHandleIOError (x_error_quitter);

  /* Disable Window Change signals;  they are handled by X events. */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_DFL);
#endif /* ! defined (SIGWINCH) */

  signal (SIGPIPE, x_connection_closed);
}

void
syms_of_xterm ()
{
  staticpro (&invocation_name);
  invocation_name = Qnil;
}
#endif /* ! defined (HAVE_X11) */
#endif /* ! defined (HAVE_X_WINDOWS) */
