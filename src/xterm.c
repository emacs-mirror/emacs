/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1989, 1993 Free Software Foundation, Inc.

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

#define NEW_SELECTIONS

/* On 4.3 these lose if they come after xterm.h.  */
/* On HP-UX 8.0 signal.h loses if it comes after config.h.  */
/* Putting these at the beginning seems to be standard for other .c files.  */
#include <stdio.h>
#include <signal.h>

#include <config.h>

#ifdef HAVE_X_WINDOWS

#include "lisp.h"
#include "blockinput.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"
#include <X11/cursorfont.h>

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
#ifndef VMS
#include <string.h>
#endif
#endif /* ! defined (BSD) */

#include "systty.h"
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
#include "window.h"

#ifdef HAVE_X11
#define XMapWindow XMapRaised		/* Raise them when mapping. */
#else /* ! defined (HAVE_X11) */
#include <X/Xkeyboard.h>
/*#include <X/Xproto.h>	*/
#endif /* ! defined (HAVE_X11) */

#ifdef FD_SET
/* We could get this from param.h, but better not to depend on finding that.
   And better not to risk that it might define other symbols used in this
   file.  */
#ifdef FD_SETSIZE
#define MAXDESC FD_SETSIZE
#else
#define MAXDESC 64
#endif
#define SELECT_TYPE fd_set
#else /* no FD_SET */
#define MAXDESC 32
#define SELECT_TYPE int

/* Define the macros to access a single-int bitmap of descriptors.  */
#define FD_SET(n, p) (*(p) |= (1 << (n)))
#define FD_CLR(n, p) (*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) (*(p) & (1 << (n)))
#define FD_ZERO(p) (*(p) = 0)
#endif /* no FD_SET */

/* For sending Meta-characters.  Do we need this? */
#define METABIT 0200

#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))

/* Nonzero means we must reprint all windows
   because 1) we received an ExposeWindow event
   or 2) we received too many ExposeRegion events to record.

   This is never needed under X11.  */
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

#if defined (SIGIO) && defined (FIONREAD)
int BLOCK_INPUT_mask;
#endif /* ! defined (SIGIO) && defined (FIONREAD) */

/* The id of a bitmap used for icon windows.
   One such map is shared by all Emacs icon windows.
   This is zero if we have not yet had a need to create the bitmap.  */

static Bitmap icon_bitmap;

/* Font used for text icons.  */

static FONT_TYPE *icon_font_info;

/* Stuff for dealing with the main icon title. */

extern Lisp_Object Vcommand_line_args;
char *hostname, *x_id_name;

/* This is the X connection that we are using.  */

Display *x_current_display;

/* The cursor to use for vertical scroll bars on x_current_display.  */
static Cursor x_vertical_scroll_bar_cursor;

/* Frame being updated by update_frame.  This is declared in term.c.
   This is set by update_begin and looked at by all the
   XT functions.  It is zero while not inside an update.
   In that case, the XT functions assume that `selected_frame'
   is the frame to apply to.  */
extern struct frame *updating_frame;

/* The frame (if any) which has the X window that has keyboard focus.
   Zero if none.  This is examined by Ffocus_frame in frame.c.  Note
   that a mere EnterNotify event can set this; if you need to know the
   last frame specified in a FocusIn or FocusOut event, use
   x_focus_event_frame.  */
struct frame *x_focus_frame;

/* The last frame mentioned in a FocusIn or FocusOut event.  This is
   separate from x_focus_frame, because whether or not LeaveNotify
   events cause us to lose focus depends on whether or not we have
   received a FocusIn event for it.  */
struct frame *x_focus_event_frame;

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

/* A mask of extra modifier bits to put into every keyboard char.  */
extern int extra_keyboard_modifiers;

extern Display *XOpenDisplay ();
extern Window XCreateWindow ();

extern Cursor XCreateCursor ();
extern FONT_TYPE *XOpenFont ();

static void flashback ();
static void redraw_previous_char ();

#ifndef HAVE_X11
static void dumpqueue ();
#endif /* HAVE_X11 */

void dumpborder ();
static int XTcursor_to ();
static int XTclear_end_of_line ();


/* Starting and ending updates. 

   These hooks are called by update_frame at the beginning and end
   of a frame update.  We record in `updating_frame' the identity
   of the frame being updated, so that the XT... functions do not
   need to take a frame as argument.  Most of the XT... functions
   should never be called except during an update, the only exceptions
   being XTcursor_to, XTwrite_glyphs and XTreassert_line_highlight.  */

extern int mouse_track_top, mouse_track_left, mouse_track_width;

static
XTupdate_begin (f)
     struct frame *f;
{	
  int mask;

  if (f == 0)
    abort ();

  flexlines = f->height;
  highlight = 0;

  BLOCK_INPUT;
#ifndef HAVE_X11
  dumpqueue ();
#endif /* HAVE_X11 */
  UNBLOCK_INPUT;
}

#ifndef HAVE_X11
static void x_do_pending_expose ();
#endif

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
  x_do_pending_expose ();
#endif /* HAVE_X11 */

  x_display_cursor (f, 1);

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

/* Set the nominal cursor position of the frame.
   This is where display update commands will take effect.
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

   FONT is the default font to use (for glyphs whose font-code is 0).

   Since the display generation code is responsible for calling
   compute_char_face and compute_glyph_face on everything it puts in
   the display structure, we can assume that the face code on each
   glyph is a valid index into FRAME_COMPUTED_FACES (f), and the one
   to which we can actually apply intern_face.  */

#if 1
/* This is the multi-face code.  */

static void
dumpglyphs (f, left, top, gp, n, hl)
     struct frame *f;
     int left, top;
     register GLYPH *gp; /* Points to first GLYPH. */
     register int n;  /* Number of glyphs to display. */
     int hl;
{
  /* Holds characters to be displayed. */
  char *buf = (char *) alloca (f->width * sizeof (*buf));
  register char *cp;		/* Steps through buf[]. */
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;
  Window window = FRAME_X_WINDOW (f);

  while (n > 0)
    {
      /* Get the face-code of the next GLYPH.  */
      int cf, len;
      int g = *gp;

      GLYPH_FOLLOW_ALIASES (tbase, tlen, g);
      cf = GLYPH_FACE (g);

      /* Find the run of consecutive glyphs with the same face-code.
	 Extract their character codes into BUF.  */
      cp = buf;
      while (n > 0)
	{
	  g = *gp;
	  GLYPH_FOLLOW_ALIASES (tbase, tlen, g);
	  if (GLYPH_FACE (g) != cf)
	    break;

	  *cp++ = GLYPH_CHAR (g);
	  --n;
	  ++gp;
	}

      /* LEN gets the length of the run.  */
      len = cp - buf;

      /* Now output this run of chars, with the font and pixel values
	 determined by the face code CF.  */
      {
	struct face *face = FRAME_DEFAULT_FACE (f);
	FONT_TYPE *font = FACE_FONT (face);
	GC gc = FACE_GC (face);
	int defaulted = 1;
	int gc_temporary = 0;

	/* First look at the face of the text itself.  */
	if (cf != 0)
	  {
	    /* The face codes on the glyphs must be valid indices into the
	       frame's face table.  */
	    if (cf < 0 || cf >= FRAME_N_COMPUTED_FACES (f)
		|| FRAME_COMPUTED_FACES (f) [cf] == 0)
	      abort ();

	    if (cf == 1)
	      face = FRAME_MODE_LINE_FACE (f);
	    else
	      face = intern_face (f, FRAME_COMPUTED_FACES (f) [cf]);
	    font = FACE_FONT (face);
	    gc = FACE_GC (face);
	    defaulted = 0;
	  }

	/* Then comes the distinction between modeline and normal text.  */
	else if (hl == 0)
	  ;
	else if (hl == 1)
	  {
	    face = FRAME_MODE_LINE_FACE (f);
	    font = FACE_FONT (face);
	    gc   = FACE_GC   (face);
	    defaulted = 0;
	  }

#define FACE_DEFAULT (~0)

	/* Now override that if the cursor's on this character.  */
	if (hl == 2)
	  {
	    if (defaulted
		|| !face->font
		|| (int) face->font == FACE_DEFAULT)
	      {
		gc = f->display.x->cursor_gc;
	      }
	    /* Cursor on non-default face: must merge.  */
	    else
	      {
		XGCValues xgcv;
		unsigned long mask;

		xgcv.background = f->display.x->cursor_pixel;
		xgcv.foreground = f->display.x->cursor_foreground_pixel;
		xgcv.font = face->font->fid;
		xgcv.graphics_exposures = 0;
		mask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;
		gc = XCreateGC (x_current_display, FRAME_X_WINDOW (f),
				mask, &xgcv);
#if 0
		if (face->stipple && face->stipple != FACE_DEFAULT)
		  XSetStipple (x_current_display, gc, face->stipple);
#endif
		gc_temporary = 1;
	      }
	  }

	if ((int) font == FACE_DEFAULT)
	  font = f->display.x->font;

	XDrawImageString (x_current_display, window, gc,
			  left, top + FONT_BASE (font), buf, len);

	if (gc_temporary)
	  XFreeGC (x_current_display, gc);

	/* We should probably check for XA_UNDERLINE_POSITION and
	   XA_UNDERLINE_THICKNESS properties on the font, but let's
	   just get the thing working, and come back to that.  */
	{
	  int underline_position = 1;

	  if (font->descent <= underline_position)
	    underline_position = font->descent - 1;

	  if (face->underline)
	    XFillRectangle (x_current_display, FRAME_X_WINDOW (f),
			    FACE_GC (face),
			    left, (top
				   + FONT_BASE (font)
				   + underline_position),
			    len * FONT_WIDTH (font), 1);
	}

	left += len * FONT_WIDTH (font);
      }
    }
}
#endif /* 1 */

#if 0
/* This is the old single-face code.  */

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
#endif

/* Output some text at the nominal frame cursor position.
   Advance the cursor over the text.
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
	      CHAR_TO_PIXEL_COL (f, curs_x),
	      CHAR_TO_PIXEL_ROW (f, curs_y),
	      start, len, highlight);

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
	      CHAR_TO_PIXEL_COL (f, curs_x),
	      CHAR_TO_PIXEL_ROW (f, curs_y),
	      FONT_WIDTH (f->display.x->font) * (first_unused - curs_x),
	      FONT_HEIGHT (f->display.x->font), False);
#if 0
  redraw_previous_char (f, curs_x, curs_y);
#endif
#else /* ! defined (HAVE_X11) */
  XPixSet (FRAME_X_WINDOW (f),
	   CHAR_TO_PIXEL_COL (f, curs_x),
	   CHAR_TO_PIXEL_ROW (f, curs_y),
	   FONT_WIDTH (f->display.x->font) * (first_unused - curs_x),
	   FONT_HEIGHT (f->display.x->font),
	   f->display.x->background_pixel);	
#endif /* ! defined (HAVE_X11) */

  UNBLOCK_INPUT;
}

/* Erase the character (if any) at the position just before X, Y in frame F,
   then redraw it and the character before it.
   This is necessary when we erase starting at X,
   in case the character after X overlaps into the one before X.  */

static void
redraw_previous_char (f, x, y)
     FRAME_PTR f;
     int x, y;
{
  /* Erase the character before the new ones, in case
     what was here before overlaps it.
     Reoutput that character, and the previous character
     (in case the previous character overlaps it).  */
  if (x > 0)
    {
      int start_x = x - 2;
      if (start_x < 0)
	start_x = 0;
      XClearArea (x_current_display, FRAME_X_WINDOW (f),
		  CHAR_TO_PIXEL_COL (f, x - 1),
		  CHAR_TO_PIXEL_ROW (f, y),
		  FONT_WIDTH (f->display.x->font),
		  FONT_HEIGHT (f->display.x->font), False);

      dumpglyphs (f, CHAR_TO_PIXEL_COL (f, start_x),
		  CHAR_TO_PIXEL_ROW (f, y),
		  &FRAME_CURRENT_GLYPHS (f)->glyphs[y][start_x],
		  x - start_x, highlight);
    }
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

  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  x_scroll_bar_clear (f);

#ifndef HAVE_X11
  dumpborder (f, 0);
#endif /* HAVE_X11 */

  XFlushQueue ();
  UNBLOCK_INPUT;
}

/* Invert the middle quarter of the frame for .15 sec.  */

/* We use the select system call to do the waiting, so we have to make sure
   it's available.  If it isn't, we just won't do visual bells.  */
#if defined (HAVE_TIMEVAL) && defined (HAVE_SELECT)

/* Subtract the `struct timeval' values X and Y,
   storing the result in RESULT.
   Return 1 if the difference is negative, otherwise 0.  */

static int
timeval_subtract (result, x, y)
     struct timeval *result, x, y;
{
  /* Perform the carry for the later subtraction by updating y.
     This is safer because on some systems
     the tv_sec member is unsigned.  */
  if (x.tv_usec < y.tv_usec)
    {
      int nsec = (y.tv_usec - x.tv_usec) / 1000000 + 1;
      y.tv_usec -= 1000000 * nsec;
      y.tv_sec += nsec;
    }
  if (x.tv_usec - y.tv_usec > 1000000)
    {
      int nsec = (y.tv_usec - x.tv_usec) / 1000000;
      y.tv_usec += 1000000 * nsec;
      y.tv_sec -= nsec;
    }

  /* Compute the time remaining to wait.  tv_usec is certainly positive.  */
  result->tv_sec = x.tv_sec - y.tv_sec;
  result->tv_usec = x.tv_usec - y.tv_usec;

  /* Return indication of whether the result should be considered negative.  */
  return x.tv_sec < y.tv_sec;
}

XTflash (f)
     struct frame *f;
{
  BLOCK_INPUT;

  {
    GC gc;

    /* Create a GC that will use the GXxor function to flip foreground pixels
       into background pixels.  */
    {
      XGCValues values;

      values.function = GXxor;
      values.foreground = (f->display.x->foreground_pixel
			   ^ f->display.x->background_pixel);
      
      gc = XCreateGC (x_current_display, FRAME_X_WINDOW (f),
		      GCFunction | GCForeground, &values);
    }

    {
      int width  = PIXEL_WIDTH  (f);
      int height = PIXEL_HEIGHT (f);

      XFillRectangle (x_current_display, FRAME_X_WINDOW (f), gc,
		      width/4, height/4, width/2, height/2);
      XFlush (x_current_display);

      {
	struct timeval wakeup, now;

	gettimeofday (&wakeup, (struct timezone *) 0);

	/* Compute time to wait until, propagating carry from usecs.  */
	wakeup.tv_usec += 150000;
	wakeup.tv_sec += (wakeup.tv_usec / 1000000);
	wakeup.tv_usec %= 1000000;

	/* Keep waiting until past the time wakeup.  */
	while (1)
	  {
	    struct timeval timeout;

	    gettimeofday (&timeout, (struct timezone *)0);

	    /* In effect, timeout = wakeup - timeout.
	       Break if result would be negative.  */
	    if (timeval_subtract (&timeout, wakeup, timeout))
	      break;

	    /* Try to wait that long--but we might wake up sooner.  */
	    select (0, 0, 0, 0, &timeout);
	  }
      }
	
      XFillRectangle (x_current_display, FRAME_X_WINDOW (f), gc,
		      width/4, height/4, width/2, height/2);
      XFreeGC (x_current_display, gc);
      XFlush (x_current_display);
    }
  }

  UNBLOCK_INPUT;
}

#endif


/* Make audible bell.  */

#ifdef HAVE_X11
#define XRINGBELL XBell(x_current_display, 0)
#else /* ! defined (HAVE_X11) */
#define XRINGBELL XFeep(0);
#endif /* ! defined (HAVE_X11) */

XTring_bell ()
{
  if (x_current_display == 0)
    return;

#if defined (HAVE_TIMEVAL) && defined (HAVE_SELECT)
  if (visible_bell)
    XTflash (selected_frame);
  else
#endif
    {
      BLOCK_INPUT;
      XRINGBELL;
      XFlushQueue ();
      UNBLOCK_INPUT;
    }
}

/* Insert and delete character.
   These are not supposed to be used because we are supposed to turn
   off the feature of using them.  */

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

/* Perform an insert-lines operation.
   Insert N lines at a vertical position curs_y.  */

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
		 intborder, CHAR_TO_PIXEL_ROW (f, topregion),
		 f->width * FONT_WIDTH (f->display.x->font),
		 length * FONT_HEIGHT (f->display.x->font), intborder,
		 CHAR_TO_PIXEL_ROW (f, newtop));
#else /* ! defined (HAVE_X11) */
      XMoveArea (FRAME_X_WINDOW (f),
		 intborder, CHAR_TO_PIXEL_ROW (f, topregion),
		 intborder, CHAR_TO_PIXEL_ROW (f, newtop),
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
		  CHAR_TO_PIXEL_ROW (f, topregion),
		  f->width * FONT_WIDTH (f->display.x->font),
		  n * FONT_HEIGHT (f->display.x->font), False);
#else /* ! defined (HAVE_X11) */
      XPixSet (FRAME_X_WINDOW (f),
	       intborder,
	       CHAR_TO_PIXEL_ROW (f, topregion),
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
		      CHAR_TO_PIXEL_ROW (f, curs_y),
		      f->width * FONT_WIDTH (f->display.x->font),
		      (flexlines - curs_y) * FONT_HEIGHT (f->display.x->font), False);
#else /* ! defined (HAVE_X11) */
	  XPixSet (FRAME_X_WINDOW (f),
		   intborder, CHAR_TO_PIXEL_ROW (f, curs_y),
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
		 CHAR_TO_PIXEL_ROW (f, curs_y + n),
		 f->width * FONT_WIDTH (f->display.x->font),
		 (flexlines - (curs_y + n)) * FONT_HEIGHT (f->display.x->font),
		 intborder, CHAR_TO_PIXEL_ROW (f, curs_y));
      XClearArea (x_current_display, FRAME_X_WINDOW (f),
		  intborder,
		  CHAR_TO_PIXEL_ROW (f, flexlines - n),
		  f->width * FONT_WIDTH (f->display.x->font),
		  n * FONT_HEIGHT (f->display.x->font), False);
#else /* ! defined (HAVE_X11) */
      XMoveArea (FRAME_X_WINDOW (f),
		 intborder,
		 CHAR_TO_PIXEL_ROW (f, curs_y + n),
		 intborder, CHAR_TO_PIXEL_ROW (f, curs_y),
		 f->width * FONT_WIDTH (f->display.x->font),
		 (flexlines - (curs_y + n)) * FONT_HEIGHT (f->display.x->font));
      /* Now we must process any ExposeRegion events that occur
	 if the area being copied from is obscured.
	 We can't let it wait because further i/d operations
	 may want to copy this area to another area.  */
      x_read_exposes ();
      XPixSet (FRAME_X_WINDOW (f), intborder,
	       CHAR_TO_PIXEL_ROW (f, flexlines - n),
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

/* Support routines for exposure events.  */
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

  /* Express rectangle as four edges, instead of position-and-size.  */
  bottom = top + rows;
  right = left + cols;

#ifndef HAVE_X11		/* Window manger does this for X11. */
  {
    int intborder = f->display.x->internal_border_width;

    /* If the rectangle includes any of the internal border area,
       redisplay the border emphasis.  */
    if (top < intborder || left < intborder
	|| bottom > intborder + f->height * FONT_HEIGHT (f->display.x->font)
	|| right > intborder + f->width * FONT_WIDTH (f->display.x->font))
      dumpborder (f, 0);
  }
#endif /* HAVE_X11		Window manger does this for X11. */
  
  /* Convert rectangle edges in pixels to edges in chars.
     Round down for left and top, up for right and bottom.  */
  top  = PIXEL_TO_CHAR_ROW (f, top);
  left = PIXEL_TO_CHAR_COL (f, left);
  bottom += (FONT_HEIGHT (f->display.x->font) - 1);
  right += (FONT_WIDTH (f->display.x->font) - 1);
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
		  active_frame->highlight[y]);
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

/* Process all expose events that are pending, for X10.
   Redraws the cursor if necessary on any frame that
   is not in the process of being updated with update_frame.  */

#ifndef HAVE_X11
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
	  if (XGCTYPE (frame) != Lisp_Frame)
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
			 - f->display.x->v_scroll_bar_width)
			/ FONT_WIDTH (f->display.x->font));
	  temp_height = ((windowinfo.height- 2 * intborder
			  - f->display.x->h_scroll_bar_height)
			 / FONT_HEIGHT (f->display.x->font));
	  if (temp_width != f->width || temp_height != f->height)
	    {
	      change_frame_size (f, max (1, temp_height),
				  max (1, temp_width), 0, 1);
	      x_resize_scroll_bars (f);
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
#endif

#ifdef HAVE_X11
static void
frame_highlight (frame)
     struct frame *frame;
{
  /* We used to only do this if Vx_no_window_manager was non-nil, but
     the ICCCM (section 4.1.6) says that the window's border pixmap
     and border pixel are window attributes which are "private to the
     client", so we can always change it to whatever we want.  */
  BLOCK_INPUT;
  XSetWindowBorder (x_current_display, FRAME_X_WINDOW (frame),
		    frame->display.x->border_pixel);
  UNBLOCK_INPUT;
  x_display_cursor (frame, 1);
}

static void
frame_unhighlight (frame)
     struct frame *frame;
{
  /* We used to only do this if Vx_no_window_manager was non-nil, but
     the ICCCM (section 4.1.6) says that the window's border pixmap
     and border pixel are window attributes which are "private to the
     client", so we can always change it to whatever we want.  */
  BLOCK_INPUT;
  XSetWindowBorderPixmap (x_current_display, FRAME_X_WINDOW (frame),
			  frame->display.x->border_tile);
  UNBLOCK_INPUT;
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
   Each event gets marked with the frame in which it occurred, so the
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


/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   minibuffer frame).  Shift the highlight as appropriate.  */
static void
XTframe_rehighlight ()
{
  struct frame *old_highlight = x_highlight_frame;

  if (x_focus_frame)
    {
      x_highlight_frame =
	((XGCTYPE (FRAME_FOCUS_FRAME (x_focus_frame)) == Lisp_Frame)
	 ? XFRAME (FRAME_FOCUS_FRAME (x_focus_frame))
	 : x_focus_frame);
      if (! FRAME_LIVE_P (x_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (x_focus_frame) = Qnil;
	  x_highlight_frame = x_focus_frame;
	}
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

/* Keyboard processing - modifier keys, vendor-specific keysyms, etc. */

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

/* These are like x_meta_mod_mask, but for different modifiers.  */
static int x_alt_mod_mask, x_super_mod_mask, x_hyper_mod_mask;

/* Initialize mode_switch_bit and modifier_meaning.  */
static void
x_find_modifier_meanings ()
{
  int min_code, max_code;
  KeySym *syms;
  int syms_per_code;
  XModifierKeymap *mods;

  x_meta_mod_mask = 0;
  x_shift_lock_mask = 0;
  x_alt_mod_mask = 0;
  x_super_mod_mask = 0;
  x_hyper_mod_mask = 0;
  
#ifdef HAVE_X11R4
  XDisplayKeycodes (x_current_display, &min_code, &max_code);
#else
  min_code = x_current_display->min_keycode;
  max_code = x_current_display->max_keycode;
#endif

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
		    x_alt_mod_mask |= (1 << row);
		    break;

		  case XK_Hyper_L:
		  case XK_Hyper_R:
		    x_hyper_mod_mask |= (1 << row);
		    break;

		  case XK_Super_L:
		  case XK_Super_R:
		    x_super_mod_mask |= (1 << row);
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
    {
      x_meta_mod_mask = x_alt_mod_mask;
      x_alt_mod_mask = 0;
    }

  /* If some keys are both alt and meta,
     make them just meta, not alt.  */
  if (x_alt_mod_mask & x_meta_mod_mask)
    {
      x_alt_mod_mask &= ~x_meta_mod_mask;
    }
  
  XFree ((char *) syms);
  XFreeModifiermap (mods);
}


/* Convert between the modifier bits X uses and the modifier bits
   Emacs uses.  */
static unsigned int
x_x_to_emacs_modifiers (state)
     unsigned int state;
{
  return (  ((state & (ShiftMask | x_shift_lock_mask)) ? shift_modifier : 0)
	  | ((state & ControlMask)		       ? ctrl_modifier  : 0)
	  | ((state & x_meta_mod_mask)		       ? meta_modifier  : 0)
	  | ((state & x_alt_mod_mask)		       ? alt_modifier  : 0)
	  | ((state & x_super_mod_mask)		       ? super_modifier  : 0)
	  | ((state & x_hyper_mod_mask)		       ? hyper_modifier  : 0));
}

static unsigned int
x_emacs_to_x_modifiers (state)
     unsigned int state;
{
  return (  ((state & alt_modifier)		? x_alt_mod_mask   : 0)
	  | ((state & super_modifier)		? x_super_mod_mask : 0)
	  | ((state & hyper_modifier)		? x_hyper_mod_mask : 0)
	  | ((state & shift_modifier)		? ShiftMask        : 0)
	  | ((state & ctrl_modifier)		? ControlMask      : 0)
	  | ((state & meta_modifier)		? x_meta_mod_mask  : 0));
}

/* Return true iff KEYSYM is a vendor-specific keysym that we should
   return as a function key.  If you add a keysym to this, you should
   make sure that the tables make_lispy_event uses contain a suitable
   name for it.  */
static int
x_is_vendor_fkey (sym)
     KeySym sym;
{
  return 0
#ifdef DXK_Remove
    || (sym == DXK_Remove)
#endif
      ;
}


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
  pix_x = PIXEL_TO_CHAR_COL (f, pix_x);
  pix_y = PIXEL_TO_CHAR_ROW (f, pix_y);

  if (bounds)
    {
      bounds->width  = FONT_WIDTH  (f->display.x->font);
      bounds->height = FONT_HEIGHT (f->display.x->font);
      bounds->x = CHAR_TO_PIXEL_COL (f, pix_x);
      bounds->y = CHAR_TO_PIXEL_ROW (f, pix_y);
    }

  if (pix_x < 0) pix_x = 0;
  else if (pix_x > f->width) pix_x = f->width;

  if (pix_y < 0) pix_y = 0;
  else if (pix_y > f->height) pix_y = f->height;

  *x = pix_x;
  *y = pix_y;
}

/* Any buttons grabbed. */
unsigned int x_mouse_grabbed;

/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static Lisp_Object
construct_mouse_click (result, event, f)
     struct input_event *result;
     XButtonEvent *event;
     struct frame *f;
{
  /* Make the event type no_event; we'll change that when we decide
     otherwise.  */
  result->kind = mouse_click;
  XSET (result->code, Lisp_Int, event->button - Button1);
  result->timestamp = event->time;
  result->modifiers = (x_x_to_emacs_modifiers (event->state)
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

  {
    int row, column;

    pixel_to_glyph_coords (f, event->x, event->y, &column, &row, NULL);
    XFASTINT (result->x) = column;
    XFASTINT (result->y) = row;
    XSET (result->frame_or_window, Lisp_Frame, f);
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

/* The scroll bar in which the last X motion event occurred.

   If the last X motion event occurred in a scroll bar, we set this
   so XTmouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last X motion event didn't occur in a scroll bar, we set this
   to Qnil, to tell XTmouse_position to return an ordinary motion event.  */
static Lisp_Object last_mouse_scroll_bar;

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
note_mouse_movement (frame, event)
     FRAME_PTR frame;
     XMotionEvent *event;

{
  last_mouse_movement_time = event->time;

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  if (event->x < last_mouse_glyph.x
      || event->x >= last_mouse_glyph.x + last_mouse_glyph.width
      || event->y < last_mouse_glyph.y
      || event->y >= last_mouse_glyph.y + last_mouse_glyph.height)
    {
      mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
    }
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

static struct scroll_bar *x_window_to_scroll_bar ();
static void x_scroll_bar_report_motion ();

/* Return the current position of the mouse.

   If the mouse movement started in a scroll bar, set *f, *bar_window,
   and *part to the frame, window, and scroll bar part that the mouse
   is over.  Set *x and *y to the portion and whole of the mouse's
   position on the scroll bar.

   If the mouse movement started elsewhere, set *f to the frame the
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
XTmouse_position (f, bar_window, part, x, y, time)
     FRAME_PTR *f;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  FRAME_PTR f1;

  BLOCK_INPUT;

  if (! NILP (last_mouse_scroll_bar))
    x_scroll_bar_report_motion (f, bar_window, part, x, y, time);
  else
    {
      Window root;
      int root_x, root_y;

      Window dummy_window;
      int dummy;

      mouse_moved = 0;
      last_mouse_scroll_bar = Qnil;

      /* Figure out which root window we're on.  */
      XQueryPointer (x_current_display,
		     DefaultRootWindow (x_current_display),

		     /* The root window which contains the pointer.  */
		     &root,

		     /* Trash which we can't trust if the pointer is on
			a different screen.  */
		     &dummy_window,

		     /* The position on that root window.  */
		     &root_x, &root_y, 

		     /* More trash we can't trust.  */
		     &dummy, &dummy,

		     /* Modifier keys and pointer buttons, about which
			we don't care.  */
		     (unsigned int *) &dummy);

      /* Now we have a position on the root; find the innermost window
	 containing the pointer.  */
      {
	Window win, child;
	int win_x, win_y;
	int parent_x, parent_y;

	win = root;
	for (;;)
	  {
	    XTranslateCoordinates (x_current_display,
			       
				   /* From-window, to-window.  */
				   root, win,

				   /* From-position, to-position.  */
				   root_x, root_y, &win_x, &win_y,

				   /* Child of win.  */
				   &child);

	    if (child == None)
	      break;

	    win = child;
	    parent_x = win_x;
	    parent_y = win_y;
	  }

	/* Now we know that:
	   win is the innermost window containing the pointer
	   (XTC says it has no child containing the pointer),
	   win_x and win_y are the pointer's position in it
	   (XTC did this the last time through), and
	   parent_x and parent_y are the pointer's position in win's parent.
	   (They are what win_x and win_y were when win was child.
	   If win is the root window, it has no parent, and
	   parent_{x,y} are invalid, but that's okay, because we'll
	   never use them in that case.)  */

	/* Is win one of our frames?  */
	f1 = x_window_to_frame (win);
      
	/* If not, is it one of our scroll bars?  */
	if (! f1)
	  {
	    struct scroll_bar *bar = x_window_to_scroll_bar (win);

	    if (bar)
	      {
		f1 = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
		win_x = parent_x;
		win_y = parent_y;
	      }
	  }

	if (f1)
	  {
	    /* Ok, we found a frame.  Convert from pixels to characters
	       and store all the values.  */

	    pixel_to_glyph_coords (f1, win_x, win_y, &win_x, &win_y,
				   &last_mouse_glyph);

	    *bar_window = Qnil;
	    *part = 0;
	    *f = f1;
	    XSET (*x, Lisp_Int, win_x);
	    XSET (*y, Lisp_Int, win_y);
	    *time = last_mouse_movement_time;
	  }
      }
    }

  UNBLOCK_INPUT;
}

#else /* ! defined (HAVE_X11) */
#define XEvent XKeyPressedEvent
#endif /* ! defined (HAVE_X11) */

/* Scroll bar support.  */

/* Given an X window ID, find the struct scroll_bar which manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */
static struct scroll_bar *
x_window_to_scroll_bar (window_id)
     Window window_id;
{
  Lisp_Object tail, frame;

  for (tail = Vframe_list;
       XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      Lisp_Object frame = XCONS (tail)->car;
      Lisp_Object bar, condemned;

      /* All elements of Vframe_list should be frames.  */
      if (XGCTYPE (frame) != Lisp_Frame)
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
	   bar = XSCROLL_BAR(bar)->next)
	if (SCROLL_BAR_X_WINDOW (XSCROLL_BAR (bar)) == window_id)
	  return XSCROLL_BAR (bar);
    }

  return 0;
}

/* Open a new X window to serve as a scroll bar, and return the
   scroll bar vector for it.  */
static struct scroll_bar *
x_scroll_bar_create (window, top, left, width, height)
     struct window *window;
     int top, left, width, height;
{
  FRAME_PTR frame = XFRAME (WINDOW_FRAME (window));
  struct scroll_bar *bar =
    XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));

  BLOCK_INPUT;

  {
    XSetWindowAttributes a;
    unsigned long mask;

    a.background_pixel = frame->display.x->background_pixel;
    a.event_mask = (ButtonPressMask | ButtonReleaseMask
		    | ButtonMotionMask | PointerMotionHintMask
		    | ExposureMask);
    a.cursor = x_vertical_scroll_bar_cursor;

    mask = (CWBackPixel | CWEventMask | CWCursor);

    SET_SCROLL_BAR_X_WINDOW
      (bar, 
       XCreateWindow (x_current_display, FRAME_X_WINDOW (frame),

		      /* Position and size of scroll bar.  */
		      left, top, width, height,

		      /* Border width, depth, class, and visual.  */
		      0, CopyFromParent, CopyFromParent, CopyFromParent,

		      /* Attributes.  */
		      mask, &a));
  }

  XSET (bar->window, Lisp_Window, window);
  XSET (bar->top,    Lisp_Int, top);
  XSET (bar->left,   Lisp_Int, left);
  XSET (bar->width,  Lisp_Int, width);
  XSET (bar->height, Lisp_Int, height);
  XSET (bar->start,  Lisp_Int, 0);
  XSET (bar->end,    Lisp_Int, 0);
  bar->dragging = Qnil;

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (frame);
  bar->prev = Qnil;
  XSET (FRAME_SCROLL_BARS (frame), Lisp_Vector, bar);
  if (! NILP (bar->next))
    XSET (XSCROLL_BAR (bar->next)->prev, Lisp_Vector, bar);

  XMapWindow (x_current_display, SCROLL_BAR_X_WINDOW (bar));

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
  Window w = SCROLL_BAR_X_WINDOW (bar);
  GC gc = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)))->display.x->normal_gc;

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == XINT (bar->start)
      && end == XINT (bar->end))
    return;

  BLOCK_INPUT;

  {
    int inside_width = VERTICAL_SCROLL_BAR_INSIDE_WIDTH (XINT (bar->width));
    int inside_height = VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (XINT (bar->height));
    int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height));

    /* Make sure the values are reasonable, and try to preserve
       the distance between start and end.  */
    {
      int length = end - start;

      if (start < 0)
	start = 0;
      else if (start > top_range)
	start = top_range;
      end = start + length;

      if (end < start)
	end = start;
      else if (end > top_range && ! dragging)
	end = top_range;
    }

    /* Store the adjusted setting in the scroll bar.  */
    XSET (bar->start, Lisp_Int, start);
    XSET (bar->end, Lisp_Int, end);

    /* Clip the end position, just for display.  */
    if (end > top_range)
      end = top_range;

    /* Draw bottom positions VERTICAL_SCROLL_BAR_MIN_HANDLE pixels
       below top positions, to make sure the handle is always at least
       that many pixels tall.  */
    end += VERTICAL_SCROLL_BAR_MIN_HANDLE;

    /* Draw the empty space above the handle.  Note that we can't clear
       zero-height areas; that means "clear to end of window."  */
    if (0 < start)
      XClearArea (x_current_display, w,

		  /* x, y, width, height, and exposures.  */
		  VERTICAL_SCROLL_BAR_LEFT_BORDER,
		  VERTICAL_SCROLL_BAR_TOP_BORDER,
		  inside_width, start,
		  False);

    /* Draw the handle itself.  */
    XFillRectangle (x_current_display, w, gc,

		    /* x, y, width, height */
		    VERTICAL_SCROLL_BAR_LEFT_BORDER,
		    VERTICAL_SCROLL_BAR_TOP_BORDER + start,
		    inside_width, end - start);


    /* Draw the empty space below the handle.  Note that we can't
       clear zero-height areas; that means "clear to end of window." */
    if (end < inside_height)
      XClearArea (x_current_display, w,

		  /* x, y, width, height, and exposures.  */
		  VERTICAL_SCROLL_BAR_LEFT_BORDER,
		  VERTICAL_SCROLL_BAR_TOP_BORDER + end,
		  inside_width, inside_height - end,
		  False);

  }

  UNBLOCK_INPUT;
}

/* Move a scroll bar around on the screen, to accommodate changing
   window configurations.  */
static void
x_scroll_bar_move (bar, top, left, width, height)
     struct scroll_bar *bar;
     int top, left, width, height;
{
  BLOCK_INPUT;

  {
    XWindowChanges wc;
    unsigned int mask = 0;

    wc.x = left;
    wc.y = top;
    wc.width = width;
    wc.height = height;

    if (left != XINT (bar->left))	mask |= CWX;
    if (top != XINT (bar->top))		mask |= CWY;
    if (width != XINT (bar->width))	mask |= CWWidth;
    if (height != XINT (bar->height))	mask |= CWHeight;
    
    if (mask)
      XConfigureWindow (x_current_display, SCROLL_BAR_X_WINDOW (bar),
			mask, &wc);
  }

  XSET (bar->left,   Lisp_Int, left);
  XSET (bar->top,    Lisp_Int, top);
  XSET (bar->width,  Lisp_Int, width);
  XSET (bar->height, Lisp_Int, height);

  UNBLOCK_INPUT;
}

/* Destroy the X window for BAR, and set its Emacs window's scroll bar
   to nil.  */
static void
x_scroll_bar_remove (bar)
     struct scroll_bar *bar;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  BLOCK_INPUT;

  /* Destroy the window.  */
  XDestroyWindow (x_current_display, SCROLL_BAR_X_WINDOW (bar));

  /* Disassociate this scroll bar from its window.  */
  XWINDOW (bar->window)->vertical_scroll_bar = Qnil;

  UNBLOCK_INPUT;
}

/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */
static void
XTset_vertical_scroll_bar (window, portion, whole, position)
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
  int pixel_width = VERTICAL_SCROLL_BAR_PIXEL_WIDTH (f);
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
      int top_range =
	VERTICAL_SCROLL_BAR_TOP_RANGE (pixel_height);

      if (whole == 0)
	x_scroll_bar_set_handle (bar, 0, top_range, 0);
      else
	{
	  int start = ((double) position * top_range) / whole;
	  int end = ((double) (position + portion) * top_range) / whole;

	  x_scroll_bar_set_handle (bar, start, end, 0);
	}
    }

  XSET (window->vertical_scroll_bar, Lisp_Vector, bar);
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
XTcondemn_scroll_bars (frame)
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
XTredeem_scroll_bar (window)
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
    XSET (FRAME_SCROLL_BARS (f), Lisp_Vector, bar);
    if (! NILP (bar->next))
      XSET (XSCROLL_BAR (bar->next)->prev, Lisp_Vector, bar);
  }
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */
static void
XTjudge_scroll_bars (f)
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


/* Handle an Expose or GraphicsExpose event on a scroll bar.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */
static void
x_scroll_bar_expose (bar, event)
     struct scroll_bar *bar;
     XEvent *event;
{
  Window w = SCROLL_BAR_X_WINDOW (bar);
  GC gc = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)))->display.x->normal_gc;

  BLOCK_INPUT;

  x_scroll_bar_set_handle (bar, XINT (bar->start), XINT (bar->end), 1);

  /* Draw a one-pixel border just inside the edges of the scroll bar. */
  XDrawRectangle (x_current_display, w, gc,

		  /* x, y, width, height */
		  0, 0, XINT (bar->width) - 1, XINT (bar->height) - 1);

  /* Draw another line to make the extra-thick border on the right.  */
  XFillRectangle (x_current_display, w, gc,

		  /* x, y, width, height */
		  XINT (bar->width) - 2, 1, 1, XINT (bar->height) - 2);

  UNBLOCK_INPUT;
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than no_event, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */
static void
x_scroll_bar_handle_click (bar, event, emacs_event)
     struct scroll_bar *bar;
     XEvent *event;
     struct input_event *emacs_event;
{
  if (XGCTYPE (bar->window) != Lisp_Window)
    abort ();

  emacs_event->kind = scroll_bar_click;
  XSET (emacs_event->code, Lisp_Int, event->xbutton.button - Button1);
  emacs_event->modifiers =
    (x_x_to_emacs_modifiers (event->xbutton.state)
     | (event->type == ButtonRelease
	? up_modifier
	: down_modifier));
  emacs_event->frame_or_window = bar->window;
  emacs_event->timestamp = event->xbutton.time;
  {
    int internal_height =
      VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (XINT (bar->height));
    int top_range =
      VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height));
    int y = event->xbutton.y - VERTICAL_SCROLL_BAR_TOP_BORDER;

    if (y < 0) y = 0;
    if (y > top_range) y = top_range;

    if (y < XINT (bar->start))
      emacs_event->part = scroll_bar_above_handle;
    else if (y < XINT (bar->end) + VERTICAL_SCROLL_BAR_MIN_HANDLE)
      emacs_event->part = scroll_bar_handle;
    else
      emacs_event->part = scroll_bar_below_handle;

    /* Just because the user has clicked on the handle doesn't mean
       they want to drag it.  Lisp code needs to be able to decide
       whether or not we're dragging.  */
#if 0
    /* If the user has just clicked on the handle, record where they're
       holding it.  */
    if (event->type == ButtonPress
	&& emacs_event->part == scroll_bar_handle)
      XSET (bar->dragging, Lisp_Int, y - XINT (bar->start));
#endif

    /* If the user has released the handle, set it to its final position.  */
    if (event->type == ButtonRelease
	&& ! NILP (bar->dragging))
      {
	int new_start = y - XINT (bar->dragging);
	int new_end = new_start + (XINT (bar->end) - XINT (bar->start));

	x_scroll_bar_set_handle (bar, new_start, new_end, 0);
	bar->dragging = Qnil;
      }

    /* Same deal here as the other #if 0.  */
#if 0
    /* Clicks on the handle are always reported as occurring at the top of 
       the handle.  */
    if (emacs_event->part == scroll_bar_handle)
      emacs_event->x = bar->start;
    else
      XSET (emacs_event->x, Lisp_Int, y);
#else
    XSET (emacs_event->x, Lisp_Int, y);
#endif

    XSET (emacs_event->y, Lisp_Int, top_range);
  }
}

/* Handle some mouse motion while someone is dragging the scroll bar.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */
static void
x_scroll_bar_note_movement (bar, event)
     struct scroll_bar *bar;
     XEvent *event;
{
  last_mouse_movement_time = event->xmotion.time;

  mouse_moved = 1;
  XSET (last_mouse_scroll_bar, Lisp_Vector, bar);

  /* If we're dragging the bar, display it.  */
  if (! GC_NILP (bar->dragging))
    {
      /* Where should the handle be now?  */
      int new_start = event->xmotion.y - XINT (bar->dragging);

      if (new_start != XINT (bar->start))
	{
	  int new_end = new_start + (XINT (bar->end) - XINT (bar->start));
	
	  x_scroll_bar_set_handle (bar, new_start, new_end, 0);
	}
    }

  /* Call XQueryPointer so we'll get an event the next time the mouse
     moves and we can see *still* on the same position.  */
  {
    int dummy;
      
    XQueryPointer (event->xmotion.display, event->xmotion.window,
		   (Window *) &dummy, (Window *) &dummy,
		   &dummy, &dummy, &dummy, &dummy,
		   (unsigned int *) &dummy);
  }
}

/* Return information to the user about the current position of the mouse
   on the scroll bar.  */
static void
x_scroll_bar_report_motion (f, bar_window, part, x, y, time)
     FRAME_PTR *f;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  struct scroll_bar *bar = XSCROLL_BAR (last_mouse_scroll_bar);
  int win_x, win_y;
  Window dummy_window;
  int dummy_coord;
  unsigned int dummy_mask;

  BLOCK_INPUT;

  /* Get the mouse's position relative to the scroll bar window, and
     report that.  */
  if (! XQueryPointer (x_current_display,
		       SCROLL_BAR_X_WINDOW (bar),

		       /* Root, child, root x and root y.  */
		       &dummy_window, &dummy_window,
		       &dummy_coord, &dummy_coord,

		       /* Position relative to scroll bar.  */
		       &win_x, &win_y,

		       /* Mouse buttons and modifier keys.  */
		       &dummy_mask))
    *f = 0;
  else
    {
      int inside_height
	= VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (XINT (bar->height));
      int top_range
	= VERTICAL_SCROLL_BAR_TOP_RANGE     (XINT (bar->height));

      win_y -= VERTICAL_SCROLL_BAR_TOP_BORDER;

      if (! NILP (bar->dragging))
	win_y -= XINT (bar->dragging);

      if (win_y < 0)
	win_y = 0;
      if (win_y > top_range)
	win_y = top_range;

      *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
      *bar_window = bar->window;

      if (! NILP (bar->dragging))
	*part = scroll_bar_handle;
      else if (win_y < XINT (bar->start))
	*part = scroll_bar_above_handle;
      else if (win_y < XINT (bar->end) + VERTICAL_SCROLL_BAR_MIN_HANDLE)
	*part = scroll_bar_handle;
      else
	*part = scroll_bar_below_handle;

      XSET (*x, Lisp_Int, win_y);
      XSET (*y, Lisp_Int, top_range);

      mouse_moved = 0;
      last_mouse_scroll_bar = Qnil;
    }

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

  for (bar = FRAME_SCROLL_BARS (f);
       XTYPE (bar) == Lisp_Vector;
       bar = XSCROLL_BAR (bar)->next)
    XClearArea (x_current_display, SCROLL_BAR_X_WINDOW (XSCROLL_BAR (bar)),
		0, 0, 0, 0, True);
}



/* The main X event-reading loop - XTread_socket.  */

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

/* Window manager communication.  */
Atom Xatom_wm_change_state;

/* Record the last 100 characters stored
   to help debug the loss-of-chars-during-GC problem.  */
int temp_index;
short temp_buffer[100];

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
  int event_found = 0;
  int prefix;
  Lisp_Object part;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
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
		    /* Not certain about handling scroll bars here */
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
		      {
			if (numchars == 0)
			  abort ();

			bufp->kind = delete_window_event;
			XSET (bufp->frame_or_window, Lisp_Frame, f);
			bufp++;

			count += 1;
			numchars -= 1;
		      }
		  }
	      }
	    else if (event.xclient.message_type == Xatom_wm_configure_denied)
	      {
	      }
	    else if (event.xclient.message_type == Xatom_wm_window_moved)
	      {
		int new_x, new_y;
		struct frame *f = x_window_to_frame (event.xclient.window);
		
		new_x = event.xclient.data.s[0];
		new_y = event.xclient.data.s[1];

		if (f)
		  {
		    f->display.x->left_pos = new_x;
		    f->display.x->top_pos = new_y;
		  }
	      }
	  }
	  break;

#ifdef NEW_SELECTIONS
	case SelectionNotify:
	  x_handle_selection_notify (&event);
	  break;
#endif

	case SelectionClear:	/* Someone has grabbed ownership. */
#ifdef NEW_SELECTIONS
	  {
	    XSelectionClearEvent *eventp = (XSelectionClearEvent *) &event;

	    if (numchars == 0)
	      abort ();

	    bufp->kind = selection_clear_event;
	    SELECTION_EVENT_DISPLAY (bufp) = eventp->display;
	    SELECTION_EVENT_SELECTION (bufp) = eventp->selection;
	    SELECTION_EVENT_TIME (bufp) = eventp->time;
	    bufp++;

	    count += 1;
	    numchars -= 1;
	  }
#else
	  x_disown_selection (event.xselectionclear.window,
			      event.xselectionclear.selection,
			      event.xselectionclear.time);
#endif
	  break;

	case SelectionRequest:	/* Someone wants our selection. */
#ifdef NEW_SELECTIONS
	  {
	    XSelectionRequestEvent *eventp = (XSelectionRequestEvent *) &event;

	    if (numchars == 0)
	      abort ();

	    bufp->kind = selection_request_event;
	    SELECTION_EVENT_DISPLAY (bufp) = eventp->display;
	    SELECTION_EVENT_REQUESTOR (bufp) = eventp->requestor;
	    SELECTION_EVENT_SELECTION (bufp) = eventp->selection;
	    SELECTION_EVENT_TARGET (bufp) = eventp->target;
	    SELECTION_EVENT_PROPERTY (bufp) = eventp->property;
	    SELECTION_EVENT_TIME (bufp) = eventp->time;
	    bufp++;

	    count += 1;
	    numchars -= 1;
	  }
#else
	  x_answer_selection_request (event);
#endif
	  break;

	case PropertyNotify:
#ifdef NEW_SELECTIONS
	  x_handle_property_notify (&event);
#else
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
#endif
	  break;

	case ReparentNotify:
	  f = x_window_to_frame (event.xreparent.window);
	  if (f)
	    f->display.x->parent_desc = event.xreparent.parent;
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
		{
		  dumprectangle (x_window_to_frame (event.xexpose.window),
				 event.xexpose.x, event.xexpose.y,
				 event.xexpose.width, event.xexpose.height);
		}
	    }
	  else
	    {
	      struct scroll_bar *bar
		= x_window_to_scroll_bar (event.xexpose.window);

	      if (bar)
		x_scroll_bar_expose (bar, &event);
	    }
	  break;

	case GraphicsExpose:	/* This occurs when an XCopyArea's
				  source area was obscured or not
				  available.*/
	  f = x_window_to_frame (event.xgraphicsexpose.drawable);
	  if (f)
	    {
	      dumprectangle (f,
			     event.xgraphicsexpose.x, event.xgraphicsexpose.y,
			     event.xgraphicsexpose.width,
			     event.xgraphicsexpose.height);
	    }
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
	  f = x_window_to_frame (event.xunmap.window);
	  if (f)		/* F may no longer exist if
				   the frame was deleted.  */
	    {
	      /* While a frame is unmapped, display generation is
		 disabled; you don't want to spend time updating a
		 display that won't ever be seen.  */
	      f->async_visible = 0;
	      /* The window manager never makes a window invisible
		 ("withdrawn"); all it does is switch between visible
		 and iconified.  Frames get into the invisible state
		 only through x_make_frame_invisible.  */
	      if (FRAME_VISIBLE_P (f) || FRAME_ICONIFIED_P (f))
		f->async_iconified = 1;
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
	      KeySym keysym, orig_keysym;
	      /* al%imercury@uunet.uu.net says that making this 81 instead of
		 80 fixed a bug whereby meta chars made his Emacs hang.  */
	      unsigned char copy_buffer[81];
	      int modifiers;

	      event.xkey.state
		|= x_emacs_to_x_modifiers (extra_keyboard_modifiers);
	      modifiers = event.xkey.state;

	      /* This will have to go some day...  */

	      /* make_lispy_event turns chars into control chars.
		 Don't do it here because XLookupString is too eager.  */
	      event.xkey.state &= ~ControlMask;
	      nbytes =
		XLookupString (&event.xkey, copy_buffer, 80, &keysym,
			       &compose_status);

	      /* Strip off the vendor-specific keysym bit, and take a shot
		 at recognizing the codes.  HP servers have extra keysyms
		 that fit into the MiscFunctionKey category.  */
	      orig_keysym = keysym;
	      keysym &= ~(1<<28);

	      if (numchars > 1)
		{
		  if (((keysym >= XK_BackSpace && keysym <= XK_Escape)
		       || keysym == XK_Delete
		       || IsCursorKey (keysym) /* 0xff50 <= x < 0xff60 */
		       || IsMiscFunctionKey (keysym) /* 0xff60 <= x < VARIES */
#ifdef HPUX
		       /* This recognizes the "extended function keys".
			  It seems there's no cleaner way.
			  Test IsModifierKey to avoid handling mode_switch
			  incorrectly.  */
		       || ((unsigned) (keysym) >= XK_Select
			   && (unsigned)(keysym) < XK_KP_Space)
#endif
		       || IsKeypadKey (keysym) /* 0xff80 <= x < 0xffbe */
		       || IsFunctionKey (keysym) /* 0xffbe <= x < 0xffe1 */
		       || x_is_vendor_fkey (orig_keysym))
		      && ! (IsModifierKey (orig_keysym)
#ifndef HAVE_X11R5
#ifdef XK_Mode_switch
			    || ((unsigned)(orig_keysym) == XK_Mode_switch)
#endif
#ifdef XK_Num_Lock
			    || ((unsigned)(orig_keysym) == XK_Num_Lock)
#endif
#endif /* not HAVE_X11R5 */
			    ))
		    {
		      if (temp_index == sizeof temp_buffer / sizeof (short))
			temp_index = 0;
		      temp_buffer[temp_index++] = keysym;
		      bufp->kind = non_ascii_keystroke;
		      XSET (bufp->code, Lisp_Int, (unsigned) keysym - 0xff00);
		      XSET (bufp->frame_or_window, Lisp_Frame, f);
		      bufp->modifiers = x_x_to_emacs_modifiers (modifiers);
		      bufp->timestamp = event.xkey.time;
		      bufp++;
		      count++;
		      numchars--;
		    }
		  else if (numchars > nbytes)
		    {
		      register int i;

		      for (i = 0; i < nbytes; i++)
			{
			  if (temp_index == sizeof temp_buffer / sizeof (short))
			    temp_index = 0;
			  temp_buffer[temp_index++] = copy_buffer[i];
			  bufp->kind = ascii_keystroke;
			  XSET (bufp->code, Lisp_Int, copy_buffer[i]);
			  XSET (bufp->frame_or_window, Lisp_Frame, f);
			  bufp->modifiers = x_x_to_emacs_modifiers (modifiers);
			  bufp->timestamp = event.xkey.time;
			  bufp++;
			}

		      count += nbytes;
		      numchars -= nbytes;
		    }
		  else
		    abort ();
		}
	      else
		abort ();
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
		    XSET (bufp->frame_or_window, Lisp_Frame, f);
		    bufp++;
		  }
		count += nbytes;
		numchars -= nbytes;
	      }
	  }
	  break;
#endif /* ! defined (HAVE_X11) */

#ifdef HAVE_X11

	  /* Here's a possible interpretation of the whole
	     FocusIn-EnterNotify FocusOut-LeaveNotify mess.  If you get a
	     FocusIn event, you have to get a FocusOut event before you
	     relinquish the focus.  If you haven't received a FocusIn event,
	     then a mere LeaveNotify is enough to free you.  */

	case EnterNotify:
	  f = x_window_to_frame (event.xcrossing.window);

	  if (event.xcrossing.focus)		/* Entered Window */
	    {
	      /* Avoid nasty pop/raise loops. */
	      if (f && (!(f->auto_raise)
			|| !(f->auto_lower)
			|| (event.xcrossing.time - enter_timestamp) > 500))
		{
		  x_new_focus_frame (f);
		  enter_timestamp = event.xcrossing.time;
		}
	    }
	  else if (f == x_focus_frame)
	    x_new_focus_frame (0);

	  break;

	case FocusIn:
	  f = x_window_to_frame (event.xfocus.window);
	  if (event.xfocus.detail != NotifyPointer) 
	    x_focus_event_frame = f;
	  if (f)
	    x_new_focus_frame (f);
	  break;


	case LeaveNotify:
	  f = x_window_to_frame (event.xcrossing.window);

	  if (event.xcrossing.focus)
	    {
	      if (! x_focus_event_frame)
		x_new_focus_frame (0);
	      else
		x_new_focus_frame (f);
	    }
	  else 
	    {
	      if (f == x_focus_event_frame)
		x_focus_event_frame = 0;
	      if (f == x_focus_frame)
		x_new_focus_frame (0);
	    }
	  break;

	case FocusOut:
	  f = x_window_to_frame (event.xfocus.window);
	  if (event.xfocus.detail != NotifyPointer
	      && f == x_focus_event_frame)
	    x_focus_event_frame = 0;
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
	      note_mouse_movement (f, &event.xmotion);
	    else
	      {
		struct scroll_bar *bar =
		  x_window_to_scroll_bar (event.xmotion.window);

		if (bar)
		  x_scroll_bar_note_movement (bar, &event);
	      }
	  }
	  break;

	case ConfigureNotify:
	  f = x_window_to_frame (event.xconfigure.window);
	  if (f)
	    {
	      int rows = PIXEL_TO_CHAR_HEIGHT (f, event.xconfigure.height);
	      int columns = PIXEL_TO_CHAR_WIDTH (f, event.xconfigure.width);

	      /* Even if the number of character rows and columns has
		 not changed, the font size may have changed, so we need
		 to check the pixel dimensions as well.  */
	      if (columns != f->width
		  || rows != f->height
		  || event.xconfigure.width != f->display.x->pixel_width
		  || event.xconfigure.height != f->display.x->pixel_height)
		{
		  change_frame_size (f, rows, columns, 0, 1);
		  SET_FRAME_GARBAGED (f);
		}

	      if (! event.xconfigure.send_event)
		{
		  Window win, child;
		  int win_x, win_y;

		  /* Find the position of the outside upper-left corner of
		     the window, in the root coordinate system.  Don't
		     refer to the parent window here; we may be processing
		     this event after the window manager has changed our
		     parent, but before we have reached the ReparentNotify.  */
		  XTranslateCoordinates (x_current_display,
			       
					 /* From-window, to-window.  */
					 f->display.x->window_desc,
					 ROOT_WINDOW,

					 /* From-position, to-position.  */
					 -event.xconfigure.border_width,
					 -event.xconfigure.border_width,
					 &win_x, &win_y,

					 /* Child of win.  */
					 &child);
		  event.xconfigure.x = win_x;
		  event.xconfigure.y = win_y;
		}

	      f->display.x->pixel_width = event.xconfigure.width;
	      f->display.x->pixel_height = event.xconfigure.height;
	      f->display.x->left_pos = event.xconfigure.x;
	      f->display.x->top_pos = event.xconfigure.y;
	    }
	  break;

	case ButtonPress:
	case ButtonRelease:
	  {
	    /* If we decide we want to generate an event to be seen
	       by the rest of Emacs, we put it here.  */
	    struct input_event emacs_event;
	    emacs_event.kind = no_event;

	    f = x_window_to_frame (event.xbutton.window);
	    if (f)
	      {
		if (!x_focus_frame || (f == x_focus_frame))
		  construct_mouse_click (&emacs_event,
					 &event, f);
	      }
	    else
	      {
		struct scroll_bar *bar =
		  x_window_to_scroll_bar (event.xbutton.window);

		if (bar)
		  x_scroll_bar_handle_click (bar, &event, &emacs_event);
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
	      XSET (bufp->frame_or_window, Lisp_Frame, f);
	      XSET (bufp->time, Lisp_Int, event.xkey.time);
	      bufp++;

	      bufp->kind = ascii_keystroke;
	      bufp->code = (char) 0; /* C-@ */
	      XSET (bufp->frame_or_window, Lisp_Frame, f);
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

#ifdef HAVE_SELECT
  if (expected && ! event_found)
    {
      /* AOJ 880406: if select returns true but XPending doesn't, it means that
	 there is an EOF condition; in other words, that X has died.
	 Act as if there had been a hangup. */
      int fd = ConnectionNumber (x_current_display);
      SELECT_TYPE mask;
      EMACS_TIME timeout;

      FD_ZERO (&mask);
      FD_SET (fd, &mask);
      EMACS_SET_SECS_USECS (timeout, 0, 0);
      if (0 != select (fd + 1, &mask, (long *) 0, (long *) 0, &timeout)
	  && !XStuffPending ())
	kill (getpid (), SIGHUP);
    }
#endif /* HAVE_SELECT */

#ifndef HAVE_X11
  if (updating_frame == 0)
    x_do_pending_expose ();
#endif

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


/* Drawing the cursor.  */


/* Draw a hollow box cursor.  Don't change the inside of the box.  */

static void
x_draw_box (f)
     struct frame *f;
{
  int left = CHAR_TO_PIXEL_COL (f, f->cursor_x);
  int top  = CHAR_TO_PIXEL_ROW (f, f->cursor_y);
  int width = FONT_WIDTH (f->display.x->font);
  int height = FONT_HEIGHT (f->display.x->font);

#ifdef HAVE_X11
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

  if (! FRAME_VISIBLE_P (f)
      || f->phys_cursor_x < 0)
    return;

#ifdef HAVE_X11
  x_display_cursor (f, 0);
#else /* ! defined (HAVE_X11) */
  XPixSet (FRAME_X_WINDOW (f),
	   CHAR_TO_PIXEL_COL (f, f->phys_cursor_x),
	   CHAR_TO_PIXEL_ROW (f, f->phys_cursor_y),
	   FONT_WIDTH (f->display.x->font), FONT_HEIGHT (f->display.x->font),
	   f->display.x->background_pixel);
#endif /* ! defined (HAVE_X11) */
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
	      &glyph, 1, highlight);
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
	  || f->display.x->current_cursor != bar_cursor))
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
	  || (f->display.x->current_cursor != bar_cursor)))
    {
      f->phys_cursor_glyph
	= ((current_glyphs->enable[curs_y]
	    && curs_x < current_glyphs->used[curs_y])
	   ? current_glyphs->glyphs[curs_y][curs_x]
	   : SPACEGLYPH);
      XFillRectangle (x_current_display, FRAME_X_WINDOW (f),
		      f->display.x->cursor_gc,
		      CHAR_TO_PIXEL_COL (f, curs_x),
		      CHAR_TO_PIXEL_ROW (f, curs_y),
		      1, FONT_HEIGHT (f->display.x->font));

      f->phys_cursor_x = curs_x;
      f->phys_cursor_y = curs_y;

      f->display.x->current_cursor = bar_cursor;
    }

  if (updating_frame != f)
    XFlushQueue ();
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
	  || (f->display.x->current_cursor != hollow_box_cursor
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
	  || (f->display.x->current_cursor != filled_box_cursor
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
	  f->display.x->current_cursor = hollow_box_cursor;
	}
      else
	{
	  x_draw_single_glyph (f, curs_y, curs_x,
			       f->phys_cursor_glyph, 2);
	  f->display.x->current_cursor = filled_box_cursor;
	}

      f->phys_cursor_x = curs_x;
      f->phys_cursor_y = curs_y;
    }

  if (updating_frame != f)
    XFlushQueue ();
}

x_display_cursor (f, on)
     struct frame *f;
     int on;
{
  if (FRAME_DESIRED_CURSOR (f) == filled_box_cursor)
    x_display_box_cursor (f, on);
  else if (FRAME_DESIRED_CURSOR (f) == bar_cursor)
    x_display_bar_cursor (f, on);
  else
    /* Those are the only two we have implemented!  */
    abort ();
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

/* Make the x-window of frame F use the gnu icon bitmap.  */

int
x_bitmap_icon (f)
     struct frame *f;
{
  int mask;
  Window icon_window;

  if (FRAME_X_WINDOW (f) == 0)
    return 1;

#ifdef HAVE_X11
  if (! icon_bitmap)
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

#ifdef HAVE_X11
  if (icon_name)
    f->display.x->icon_label = icon_name;
  else
    if (! f->display.x->icon_label)
      f->display.x->icon_label = " *emacs* ";
  
#if 0
  XSetIconName (x_current_display, FRAME_X_WINDOW (f),
		(char *) f->display.x->icon_label);
#endif
  
  f->display.x->icon_bitmap_flag = 0;
  x_wm_set_icon_pixmap (f, 0);
#else /* ! defined (HAVE_X11) */
  if (icon_font_info == 0)
    icon_font_info
      = XGetFont (XGetDefault (XDISPLAY
			       (char *) XSTRING (Vinvocation_name)->data,
			       "BodyFont"));

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

/* Shut down Emacs in an orderly fashion, because of a SIGPIPE on the
   X server's connection, or an error reported via the X protocol.  */

static SIGTYPE
x_connection_closed ()
{
  if (_Xdebug)
    abort ();

  shut_down_emacs (0, 1, Qnil);

  exit (70);
}

/* An X error handler which prints an error message and then kills
   Emacs.  This is what's normally installed as Xlib's handler for
   protocol errors.  */
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

#if 0
  /* While we're testing Emacs 19, we'll just dump core whenever we
     get an X error, so we can figure out why it happened.  */
  abort ();
#endif

  x_connection_closed ();
}

/* A handler for X IO errors which prints an error message and then
   kills Emacs.  This is what is always installed as Xlib's handler
   for I/O errors.  */
static int
x_io_error_quitter (display)
     Display *display;
{
  fprintf (stderr, "Connection to X server %s lost.\n",
	   XDisplayName (DisplayString (display)));

#if 0
  /* While we're testing Emacs 19, we'll just dump core whenever we
     get an X error, so we can figure out why it happened.  */
  abort ();
#endif

  x_connection_closed ();
}

/* A buffer for storing X error messages.  */
static char *x_caught_error_message;
#define X_CAUGHT_ERROR_MESSAGE_SIZE 200

/* An X error handler which stores the error message in
   x_caught_error_message.  This is what's installed when
   x_catch_errors is in effect.  */
static int
x_error_catcher (display, error)
     Display *display;
     XErrorEvent *error;
{
  XGetErrorText (display, error->error_code,
		 x_caught_error_message, X_CAUGHT_ERROR_MESSAGE_SIZE);
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
  x_caught_error_message
    = (char*) xmalloc (X_CAUGHT_ERROR_MESSAGE_SIZE);
  x_caught_error_message[0] = '\0';

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

  if (x_caught_error_message[0])
    {
      char buf[X_CAUGHT_ERROR_MESSAGE_SIZE + 56];

      sprintf (buf, format, x_caught_error_message);
      x_uncatch_errors ();
      error (buf);
    }
}

void
x_uncatch_errors ()
{
  xfree (x_caught_error_message);
  x_caught_error_message = 0;
  XHandleError (x_error_quitter);
}

#if 0
static unsigned int x_wire_count;
x_trace_wire ()
{
  fprintf (stderr, "Lib call: %d\n", ++x_wire_count);
}
#endif /* ! 0 */


/* Changing the font of the frame.  */

/* Set the font of the x-window specified by frame F
   to the font named NEWNAME.  This is safe to use
   even before F has an actual x-window.  */

#ifdef HAVE_X11

struct font_info
{
  XFontStruct *font;
  char *name;
};

/* A table of all the fonts we have already loaded.  */
static struct font_info *x_font_table;

/* The current capacity of x_font_table.  */
static int x_font_table_size;

/* The number of fonts actually stored in x_font_table.
   x_font_table[n] is used and valid iff 0 <= n < n_fonts.
   0 <= n_fonts <= x_font_table_size.  */
static int n_fonts;

Lisp_Object
x_new_font (f, fontname)
     struct frame *f;
     register char *fontname;
{
  int already_loaded;
  int n_matching_fonts;
  XFontStruct *font_info;
  char **font_names;

  /* Get a list of all the fonts that match this name.  Once we
     have a list of matching fonts, we compare them against the fonts
     we already have by comparing font ids.  */
  font_names = (char **) XListFonts (x_current_display, fontname,
				     1024, &n_matching_fonts);
  /* Apparently it doesn't set n_matching_fonts to zero when it can't
     find any matches; font_names == 0 is the only clue.  */
  if (! font_names)
    n_matching_fonts = 0;

  /* Don't just give up if n_matching_fonts is 0.
     Apparently there's a bug on Suns: XListFontsWithInfo can
     fail to find a font, but XLoadQueryFont may still find it.  */

  /* See if we've already loaded a matching font. */
  already_loaded = -1;
  if (n_matching_fonts != 0)
    {
      int i, j;

      for (i = 0; i < n_fonts; i++)
	for (j = 0; j < n_matching_fonts; j++)
	  if (!strcmp (x_font_table[i].name, font_names[j]))
	    {
	      already_loaded = i;
	      fontname = font_names[j];
	      goto found_font;
	    }
    }
 found_font:
  
  /* If we have, just return it from the table.  */
  if (already_loaded >= 0)
    f->display.x->font = x_font_table[already_loaded].font;
  
  /* Otherwise, load the font and add it to the table.  */
  else
    {
      int i;
      XFontStruct *font;

      /* Try to find a character-cell font in the list.  */
#if 0 
      /* A laudable goal, but this isn't how to do it.  */
      for (i = 0; i < n_matching_fonts; i++)
	if (! font_info[i].per_char)
	  break;
#else
      i = 0;
#endif

      /* See comment above.  */
      if (n_matching_fonts != 0)
	fontname = font_names[i];

      font = (XFontStruct *) XLoadQueryFont (x_current_display, fontname);
      if (! font)
	{
	  /* Free the information from XListFonts.  */
	  if (n_matching_fonts)
	    XFreeFontNames (font_names);
	  return Qnil;
	}

      /* Do we need to create the table?  */
      if (x_font_table_size == 0)
	{
	  x_font_table_size = 16;
	  x_font_table
	    = (struct font_info *) xmalloc (x_font_table_size
					    * sizeof (x_font_table[0]));
	}
      /* Do we need to grow the table?  */
      else if (n_fonts >= x_font_table_size)
	{
	  x_font_table_size *= 2;
	  x_font_table
	    = (struct font_info *) xrealloc (x_font_table,
					     (x_font_table_size
					      * sizeof (x_font_table[0])));
	}

      x_font_table[n_fonts].name = (char *) xmalloc (strlen (fontname) + 1);
      bcopy (fontname, x_font_table[n_fonts].name, strlen (fontname) + 1);
      f->display.x->font = x_font_table[n_fonts++].font = font;
    }

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

  {
    Lisp_Object lispy_name = build_string (fontname);


    /* Free the information from XListFonts.  The data
       we actually retain comes from XLoadQueryFont.  */
    XFreeFontNames (font_names);

    return lispy_name;
  }
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

/* X Window sizes and positions.  */

x_calc_absolute_position (f)
     struct frame *f;
{
#ifdef HAVE_X11
  Window win, child;
  int win_x = 0, win_y = 0;

  /* Find the position of the outside upper-left corner of
     the inner window, with respect to the outer window.  */
  if (f->display.x->parent_desc != ROOT_WINDOW)
    {
      BLOCK_INPUT;
      XTranslateCoordinates (x_current_display,
			       
			     /* From-window, to-window.  */
			     f->display.x->window_desc,
			     f->display.x->parent_desc,

			     /* From-position, to-position.  */
			     0, 0, &win_x, &win_y,

			     /* Child of win.  */
			     &child);
      UNBLOCK_INPUT;
    }

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (f->display.x->left_pos < 0)
    f->display.x->left_pos = (x_screen_width 
			      - f->display.x->border_width - win_x
			      - PIXEL_WIDTH (f)
			      + f->display.x->left_pos);

  if (f->display.x->top_pos < 0)
    f->display.x->top_pos = (x_screen_height
			     - f->display.x->border_width - win_y
			     - PIXEL_HEIGHT (f)
			     + f->display.x->top_pos);

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
  x_wm_set_size_hint (f, 0, xoff, yoff);
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

  BLOCK_INPUT;

  check_frame_size (f, &rows, &cols);
  f->display.x->vertical_scroll_bar_extra
    = (FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? VERTICAL_SCROLL_BAR_PIXEL_WIDTH (f)
       : 0);
  pixelwidth = CHAR_TO_PIXEL_WIDTH (f, cols);
  pixelheight = CHAR_TO_PIXEL_HEIGHT (f, rows);

#ifdef HAVE_X11
  x_wm_set_size_hint (f, 0, 0, 0);
#endif /* ! defined (HAVE_X11) */
  XChangeWindowSize (FRAME_X_WINDOW (f), pixelwidth, pixelheight);

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

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);

  XFlushQueue ();
  UNBLOCK_INPUT;
}

#ifndef HAVE_X11
x_set_resize_hint (f)
     struct frame *f;
{
  XSetResizeHint (FRAME_X_WINDOW (f),
		  2 * f->display.x->internal_border_width,
		  2 * f->display.x->internal_border_width,
		  FONT_WIDTH (f->display.x->font),
		  FONT_HEIGHT (f->display.x->font));
}
#endif /* HAVE_X11 */

/* Mouse warping, focus shifting, raising and lowering.  */

x_set_mouse_position (f, x, y)
     struct frame *f;
     int x, y;
{
  int pix_x, pix_y;

  x_raise_frame (f);

  pix_x = CHAR_TO_PIXEL_COL (f, x) + FONT_WIDTH  (f->display.x->font) / 2;
  pix_y = CHAR_TO_PIXEL_ROW (f, y) + FONT_HEIGHT (f->display.x->font) / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > PIXEL_WIDTH (f)) pix_x = PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > PIXEL_HEIGHT (f)) pix_y = PIXEL_HEIGHT (f);

  BLOCK_INPUT;

  XWarpMousePointer (FRAME_X_WINDOW (f), pix_x, pix_y);
  UNBLOCK_INPUT;
}

#ifdef HAVE_X11
x_focus_on_frame (f)
     struct frame *f;
{
#if 0  /* This proves to be unpleasant.  */
  x_raise_frame (f);
#endif
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

static void
XTframe_raise_lower (f, raise)
     FRAME_PTR f;
     int raise;
{
  if (raise)
    x_raise_frame (f);
  else
    x_lower_frame (f);
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
      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
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

  /* Don't keep the highlight on an invisible frame.  */
  if (x_highlight_frame == f)
    x_highlight_frame = 0;

  if (! f->async_visible && ! f->async_iconified)
    return;

  BLOCK_INPUT;

#ifdef HAVE_X11R4

  if (! XWithdrawWindow (x_current_display, FRAME_X_WINDOW (f),
			 DefaultScreen (x_current_display)))
    {
      UNBLOCK_INPUT_RESIGNAL;
      error ("can't notify window manager of window withdrawal");
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

/* Change window state from mapped to iconified. */

x_iconify_frame (f)
     struct frame *f;
{
  int mask;

  /* Don't keep the highlight on an invisible frame.  */
  if (x_highlight_frame == f)
    x_highlight_frame = 0;

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

  if (!FRAME_VISIBLE_P (f))
    {
      /* If the frame was withdrawn, before, we must map it.  */
      XMapWindow (XDISPLAY FRAME_X_WINDOW (f));
      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
	XMapSubwindows (x_current_display, FRAME_X_WINDOW (f));
    }

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

/* Destroy the X window of frame F.  */

x_destroy_window (f)
     struct frame *f;
{
  BLOCK_INPUT;

  if (f->display.x->icon_desc != 0)
    XDestroyWindow (XDISPLAY f->display.x->icon_desc);
  XDestroyWindow (XDISPLAY f->display.x->window_desc);
  free_frame_faces (f);
  XFlushQueue ();

  xfree (f->display.x);
  f->display.x = 0;
  if (f == x_focus_frame)
    x_focus_frame = 0;
  if (f == x_highlight_frame)
    x_highlight_frame = 0;

  UNBLOCK_INPUT;
}

/* Manage event queues for X10.  */

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

/* Setting window manager hints.  */

#ifdef HAVE_X11

/* SPEC_X and SPEC_Y are the specified positions.
   We look only at their sign, to decide the gravity.  */

x_wm_set_size_hint (f, prompting, spec_x, spec_y)
     struct frame *f;
     long prompting;
     int spec_x, spec_y;
{
  XSizeHints size_hints;
  Window window = FRAME_X_WINDOW (f);

  size_hints.flags = PResizeInc | PMinSize /* | PMaxSize */;

  flexlines = f->height;

  size_hints.x = f->display.x->left_pos;
  size_hints.y = f->display.x->top_pos;
  size_hints.height = PIXEL_HEIGHT (f);
  size_hints.width = PIXEL_WIDTH (f);
  size_hints.width_inc = FONT_WIDTH (f->display.x->font);
  size_hints.height_inc = FONT_HEIGHT (f->display.x->font);
#if 0
  size_hints.max_width = x_screen_width - CHAR_TO_PIXEL_WIDTH (f, 0);
  size_hints.max_height = x_screen_height - CHAR_TO_PIXEL_HEIGHT (f, 0);
#endif    
  {
    int base_width, base_height;

    base_width = CHAR_TO_PIXEL_WIDTH (f, 0);
    base_height = CHAR_TO_PIXEL_HEIGHT (f, 0);

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
      
      if (XGetNormalHints (x_current_display, window, &hints) == 0)
	hints.flags = 0;
      if (hints.flags & PSize)
	size_hints.flags |= PSize;
      if (hints.flags & PPosition)
	size_hints.flags |= PPosition;
      if (hints.flags & USPosition)
	size_hints.flags |= USPosition;
      if (hints.flags & USSize)
	size_hints.flags |= USSize;
    }
#if defined (PWinGravity)
  switch (((spec_x < 0) << 1) + (spec_y < 0))
    {
    case 0:
      size_hints.win_gravity = NorthWestGravity;
      break;
    case 1:
      size_hints.win_gravity = SouthWestGravity;
      break;
    case 2:
      size_hints.win_gravity = NorthEastGravity;
      break;
    case 3:
      size_hints.win_gravity = SouthEastGravity;
      break;
    }
  size_hints.flags |= PWinGravity;
#endif /* PWinGravity */

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

  if (icon_pixmap)
    {
      f->display.x->wm_hints.icon_pixmap = icon_pixmap;
      f->display.x->wm_hints.flags |= IconPixmapHint;
    }
  else
    f->display.x->wm_hints.flags &= ~IconPixmapHint;

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


/* Initialization.  */

void
x_term_init (display_name)
     char *display_name;
{
  Lisp_Object frame;
  char *defaultvalue;
#ifndef F_SETOWN_BUG
#ifdef F_SETOWN
  extern int old_fcntl_owner;
#endif /* ! defined (F_SETOWN) */
#endif /* F_SETOWN_BUG */
  
  x_focus_frame = x_highlight_frame = 0;

  x_current_display = XOpenDisplay (display_name);
  if (x_current_display == 0)
    fatal ("X server %s not responding.\n\
Check the DISPLAY environment variable or use \"-d\"\n",
	   display_name);

#ifdef HAVE_X11
  {
    int hostname_size = 256;

    hostname = (char *) xmalloc (hostname_size);

#if 0
    XSetAfterFunction (x_current_display, x_trace_wire);
#endif /* ! 0 */

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
    x_id_name = (char *) xmalloc (XSTRING (Vinvocation_name)->size
				+ strlen (hostname)
				+ 2);
    sprintf (x_id_name, "%s@%s", XSTRING (Vinvocation_name)->data, hostname);
  }

  /* Figure out which modifier bits mean what.  */
  x_find_modifier_meanings ();

  /* Get the scroll bar cursor.  */
  x_vertical_scroll_bar_cursor
    = XCreateFontCursor (x_current_display, XC_sb_v_double_arrow);

#if 0
  /* Watch for PropertyNotify events on the root window; we use them
     to figure out when to invalidate our cache of the cut buffers.  */
  x_watch_cut_buffer_cache ();
#endif

  if (ConnectionNumber (x_current_display) != 0)
    change_keyboard_wait_descriptor (ConnectionNumber (x_current_display));
  change_input_fd (ConnectionNumber (x_current_display));

#endif /* ! defined (HAVE_X11) */
  
#ifndef F_SETOWN_BUG
#ifdef F_SETOWN
  old_fcntl_owner = fcntl (ConnectionNumber (x_current_display), F_GETOWN, 0);
#ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (ConnectionNumber (x_current_display), F_SETOWN, -getpid ());
#else /* ! defined (F_SETOWN_SOCK_NEG) */
  fcntl (ConnectionNumber (x_current_display), F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN_SOCK_NEG) */
#endif /* ! defined (F_SETOWN) */
#endif /* F_SETOWN_BUG */

#ifdef SIGIO
  init_sigio ();
#endif /* ! defined (SIGIO) */

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
  mouse_position_hook = XTmouse_position;
  frame_rehighlight_hook = XTframe_rehighlight;
  frame_raise_lower_hook = XTframe_raise_lower;
  set_vertical_scroll_bar_hook = XTset_vertical_scroll_bar;
  condemn_scroll_bars_hook = XTcondemn_scroll_bars;
  redeem_scroll_bar_hook = XTredeem_scroll_bar;
  judge_scroll_bars_hook = XTjudge_scroll_bars;
  
  scroll_region_ok = 1;		/* we'll scroll partial frames */
  char_ins_del_ok = 0;		/* just as fast to write the line */
  line_ins_del_ok = 1;		/* we'll just blt 'em */
  fast_clear_end_of_line = 1;	/* X does this well */
  memory_below_frame = 0;	/* we don't remember what scrolls 
				   off the bottom */
  baud_rate = 19200;

  /* Try to use interrupt input; if we can't, then start polling.  */
  Fset_input_mode (Qt, Qnil, Qt, Qnil);

  /* Note that there is no real way portable across R3/R4 to get the 
     original error handler.  */
  XHandleError (x_error_quitter);
  XHandleIOError (x_io_error_quitter);

  /* Disable Window Change signals;  they are handled by X events. */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_DFL);
#endif /* ! defined (SIGWINCH) */

  signal (SIGPIPE, x_connection_closed);
}

void
syms_of_xterm ()
{
  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;
}
#endif /* ! defined (HAVE_X11) */
#endif /* ! defined (HAVE_X_WINDOWS) */
