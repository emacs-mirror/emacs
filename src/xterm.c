/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1989, 1993, 1994 Free Software Foundation, Inc.

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

/* Xt features made by Fred Pierresteguy.  */

/* On 4.3 these lose if they come after xterm.h.  */
/* On HP-UX 8.0 signal.h loses if it comes after config.h.  */
/* Putting these at the beginning seems to be standard for other .c files.  */
#include <signal.h>

#include <config.h>

#include <stdio.h>

/* Need syssignal.h for various externs and definitions that may be required
   by some configurations for calls to signal later in this source file.  */
#include "syssignal.h"

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
#endif /* ! defined (BSD) */

#include "systty.h"
#include "systime.h"

#ifndef INCLUDED_FCNTL
#include <fcntl.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>
#include <sys/param.h>

#include "frame.h"
#include "dispextern.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#if 0
#include "sink.h"
#include "sinkmask.h"
#endif /* ! 0 */
#include "gnu.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"

#ifdef USE_X_TOOLKIT
extern void free_frame_menubar ();
extern void _XEditResCheckMessages ();
#endif /* USE_X_TOOLKIT */

#ifndef USE_X_TOOLKIT
#define x_any_window_to_frame x_window_to_frame
#define x_top_window_to_frame x_window_to_frame
#endif

#ifdef USE_X_TOOLKIT
#ifndef XtNinitialState
#define XtNinitialState "initialState"
#endif
#endif

#define min(a,b) ((a)<(b) ? (a) : (b))
#define max(a,b) ((a)>(b) ? (a) : (b))

/* This is a chain of structures for all the X displays currently in use.  */
struct x_display_info *x_display_list;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of x_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
Lisp_Object x_display_name_list;

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

/* This is a frame waiting to be autoraised, within XTread_socket.  */
struct frame *pending_autoraise_frame;

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

#ifdef USE_X_TOOLKIT
/* The application context for Xt use.  */
XtAppContext Xt_app_con;
#endif

/* During an update, maximum vpos for ins/del line operations to affect.  */

static int flexlines;

/* During an update, nonzero if chars output now should be highlighted.  */

static int highlight;

/* Nominal cursor position -- where to draw output.
   During an update, these are different from the cursor-box position.  */

static int curs_x;
static int curs_y;

/* Mouse movement.

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

/* Incremented by XTread_socket whenever it really tries to read events.  */
#ifdef __STDC__
static int volatile input_signal_count;
#else
static int input_signal_count;
#endif

/* Used locally within XTread_socket.  */
static int x_noop_count;

/* Initial values of argv and argc.  */
extern char **initial_argv;
extern int initial_argc;

extern Lisp_Object Vcommand_line_args, Vsystem_name;

/* Tells if a window manager is present or not. */

extern Lisp_Object Vx_no_window_manager;

/* Nonzero enables some debugging for the X interface code. */
extern int _Xdebug;

extern Lisp_Object Qface, Qmouse_face;

extern int errno;

/* A mask of extra modifier bits to put into every keyboard char.  */
extern int extra_keyboard_modifiers;

extern XrmDatabase x_load_resources ();

void x_delete_display ();

static void flashback ();
static void redraw_previous_char ();
static void redraw_following_char ();
static unsigned int x_x_to_emacs_modifiers ();

static int fast_find_position ();
static void note_mouse_highlight ();
static void clear_mouse_face ();
static void show_mouse_face ();
static void do_line_dance ();

static int XTcursor_to ();
static int XTclear_end_of_line ();
static int x_io_error_quitter ();

/* Return the struct x_display_info corresponding to DPY.  */

struct x_display_info *
x_display_info_for_display (dpy)
     Display *dpy;
{
  struct x_display_info *dpyinfo;

  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    if (dpyinfo->display == dpy)
      return dpyinfo;

  return 0;
}

/* Starting and ending updates.

   These hooks are called by update_frame at the beginning and end
   of a frame update.  We record in `updating_frame' the identity
   of the frame being updated, so that the XT... functions do not
   need to take a frame as argument.  Most of the XT... functions
   should never be called except during an update, the only exceptions
   being XTcursor_to, XTwrite_glyphs and XTreassert_line_highlight.  */

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

  if (f == FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      FRAME_X_DISPLAY_INFO (f)->mouse_face_defer = 1;

      /* If the frame needs to be redrawn,
	 simply forget about any prior mouse highlighting.  */
      if (! FRAME_GARBAGED_P (f))
	FRAME_X_DISPLAY_INFO (f)->mouse_face_window = Qnil;

      if (!NILP (FRAME_X_DISPLAY_INFO (f)->mouse_face_window))
	{
	  int firstline, lastline, i;
	  struct window *w = XWINDOW (FRAME_X_DISPLAY_INFO (f)->mouse_face_window);

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
	    clear_mouse_face (FRAME_X_DISPLAY_INFO (f));
	}
    }

  UNBLOCK_INPUT;
}

static
XTupdate_end (f)
     struct frame *f;
{
  int mask;

  BLOCK_INPUT;

  do_line_dance ();
  x_display_cursor (f, 1);

  if (f == FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    FRAME_X_DISPLAY_INFO (f)->mouse_face_defer = 0;
#if 0
  /* This fails in the case of having updated only the echo area
     if we have switched buffers.  In that case, FRAME_CURRENT_GLYPHS
     has no relation to the current contents, and its charstarts
     have no relation to the contents of the window-buffer.
  I don't know a clean way to check
     for that case.  window_end_valid isn't set up yet.  */
  if (f == FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    note_mouse_highlight (f, FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_x,
			  FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_y);
#endif

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
}

/* This is called after a redisplay on frame F.  */

static
XTframe_up_to_date (f)
     FRAME_PTR f;
{
  if (FRAME_X_DISPLAY_INFO (f)->mouse_face_deferred_gc
      || f == FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    {
      note_mouse_highlight (FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_frame,
			    FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_x,
			    FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_y);
      FRAME_X_DISPLAY_INFO (f)->mouse_face_deferred_gc = 0;
    }
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
      XFlush (FRAME_X_DISPLAY (selected_frame));
      UNBLOCK_INPUT;
    }
}

/* Display a sequence of N glyphs found at GP.
   WINDOW is the x-window to output to.  LEFT and TOP are starting coords.
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

#if 1
/* This is the multi-face code.  */

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
  register char *cp;		/* Steps through buf[]. */
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;
  Window window = FRAME_X_WINDOW (f);
  int orig_left = left;

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
	GC gc = FACE_GC (face);
	int stippled = 0;

	/* HL = 3 means use a mouse face previously chosen.  */
	if (hl == 3)
	  cf = FRAME_X_DISPLAY_INFO (f)->mouse_face_face_id;

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
	    gc = FACE_GC (face);
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
	    gc   = FACE_GC   (face);
	    if (FACE_STIPPLE (face))
	      stippled = 1;
	  }

#define FACE_DEFAULT (~0)

	/* Now override that if the cursor's on this character.  */
	if (hl == 2)
	  {
	    /* The cursor overrides stippling.  */
	    stippled = 0;

	    if ((!face->font
		 || (int) face->font == FACE_DEFAULT
		 || face->font == f->display.x->font)
		&& face->background == f->display.x->background_pixel
		&& face->foreground == f->display.x->foreground_pixel)
	      {
		gc = f->display.x->cursor_gc;
	      }
	    /* Cursor on non-default face: must merge.  */
	    else
	      {
		XGCValues xgcv;
		unsigned long mask;

		xgcv.background = f->display.x->cursor_pixel;
		xgcv.foreground = face->background;
		/* If the glyph would be invisible,
		   try a different foreground.  */
		if (xgcv.foreground == xgcv.background)
		  xgcv.foreground = face->foreground;
		if (xgcv.foreground == xgcv.background)
		  xgcv.foreground = f->display.x->cursor_foreground_pixel;
		if (xgcv.foreground == xgcv.background)
		  xgcv.foreground = face->foreground;
		/* Make sure the cursor is distinct from text in this face.  */
		if (xgcv.background == face->background
		    && xgcv.foreground == face->foreground)
		  {
		    xgcv.background = face->foreground;
		    xgcv.foreground = face->background;
		  }
		xgcv.font = face->font->fid;
		xgcv.graphics_exposures = 0;
		mask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;
		if (FRAME_X_DISPLAY_INFO (f)->scratch_cursor_gc)
		  XChangeGC (FRAME_X_DISPLAY (f),
			     FRAME_X_DISPLAY_INFO (f)->scratch_cursor_gc,
			     mask, &xgcv);
		else
		  FRAME_X_DISPLAY_INFO (f)->scratch_cursor_gc
		    = XCreateGC (FRAME_X_DISPLAY (f), window, mask, &xgcv);
		gc = FRAME_X_DISPLAY_INFO (f)->scratch_cursor_gc;
#if 0
/* If this code is restored, it must also reset to the default stipple
   if necessary. */
		if (face->stipple && face->stipple != FACE_DEFAULT)
		  XSetStipple (FRAME_X_DISPLAY (f), gc, face->stipple);
#endif
	      }
	  }

	if ((int) font == FACE_DEFAULT)
	  font = f->display.x->font;

	if (just_foreground)
	  XDrawString (FRAME_X_DISPLAY (f), window, gc,
		       left, top + FONT_BASE (font), buf, len);
	else
	  {
	    if (stippled)
	      {
		/* Turn stipple on.  */
		XSetFillStyle (FRAME_X_DISPLAY (f), gc, FillOpaqueStippled);

		/* Draw stipple on background.  */
		XFillRectangle (FRAME_X_DISPLAY (f), window, gc,
				left, top,
				FONT_WIDTH (font) * len,
				FONT_HEIGHT (font));

		/* Turn stipple off.  */
		XSetFillStyle (FRAME_X_DISPLAY (f), gc, FillSolid);

		/* Draw the text, solidly, onto the stipple pattern.  */
		XDrawString (FRAME_X_DISPLAY (f), window, gc,
			     left, top + FONT_BASE (font), buf, len);
	      }
	    else
	      XDrawImageString (FRAME_X_DISPLAY (f), window, gc,
				left, top + FONT_BASE (font), buf, len);

	    /* Clear the rest of the line's height.  */
	    if (f->display.x->line_height != FONT_HEIGHT (font))
	      XClearArea (FRAME_X_DISPLAY (f), window, left,
			  top + FONT_HEIGHT (font),
			  FONT_WIDTH (font) * len,
			  /* This is how many pixels of height
			     we have to clear.  */
			  f->display.x->line_height - FONT_HEIGHT (font),
			  False);
	  }

#if 0 /* Doesn't work, because it uses FRAME_CURRENT_GLYPHS,
	 which often is not up to date yet.  */
	if (!just_foreground)
	  {
	    if (left == orig_left)
	      redraw_previous_char (f, PIXEL_TO_CHAR_COL (f, left),
				    PIXEL_TO_CHAR_ROW (f, top), hl == 1);
	    if (n == 0)
	      redraw_following_char (f, PIXEL_TO_CHAR_COL (f, left + len * FONT_WIDTH (font)),
				     PIXEL_TO_CHAR_ROW (f, top), hl == 1);
	  }
#endif

	/* We should probably check for XA_UNDERLINE_POSITION and
	   XA_UNDERLINE_THICKNESS properties on the font, but let's
	   just get the thing working, and come back to that.  */
	{
	  int underline_position = 1;

	  if (font->descent <= underline_position)
	    underline_position = font->descent - 1;

	  if (face->underline)
	    XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
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
     XFontStruct *font;
{
  register int len;
  Window window = FRAME_X_WINDOW (f);
  GC drawing_gc =   (hl == 2 ? f->display.x->cursor_gc
		             : (hl ? f->display.x->reverse_gc
				   : f->display.x->normal_gc));

  if (sizeof (GLYPH) == sizeof (XChar2b))
    XDrawImageString16 (FRAME_X_DISPLAY (f), window, drawing_gc,
			left, top + FONT_BASE (font), (XChar2b *) gp, n);
  else if (sizeof (GLYPH) == sizeof (unsigned char))
    XDrawImageString (FRAME_X_DISPLAY (f), window, drawing_gc,
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

  do_line_dance ();

  /* Notice if the cursor will be cleared by this operation.  */
  if (curs_y == f->phys_cursor_y
      && curs_x <= f->phys_cursor_x
      && f->phys_cursor_x < first_unused)
    f->phys_cursor_x = -1;

  XClearArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	      CHAR_TO_PIXEL_COL (f, curs_x),
	      CHAR_TO_PIXEL_ROW (f, curs_y),
	      FONT_WIDTH (f->display.x->font) * (first_unused - curs_x),
	      f->display.x->line_height, False);
#if 0
  redraw_previous_char (f, curs_x, curs_y, highlight);
#endif

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

  XClearWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));

  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  x_scroll_bar_clear (f);

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
}

#if 0
/* This currently does not work because FRAME_CURRENT_GLYPHS doesn't
   always contain the right glyphs to use.

   It also needs to be changed to look at the details of the font and
   see whether there is really overlap, and do nothing when there is
   not.  This can use font_char_overlap_left and font_char_overlap_right,
   but just how to use them is not clear.  */

/* Erase the character (if any) at the position just before X, Y in frame F,
   then redraw it and the character before it.
   This is necessary when we erase starting at X,
   in case the character after X overlaps into the one before X.
   Call this function with input blocked.  */

static void
redraw_previous_char (f, x, y, highlight_flag)
     FRAME_PTR f;
     int x, y;
     int highlight_flag;
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
      XClearArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  CHAR_TO_PIXEL_COL (f, x - 1),
		  CHAR_TO_PIXEL_ROW (f, y),
		  FONT_WIDTH (f->display.x->font),
		  f->display.x->line_height, False);

      dumpglyphs (f, CHAR_TO_PIXEL_COL (f, start_x),
		  CHAR_TO_PIXEL_ROW (f, y),
		  &FRAME_CURRENT_GLYPHS (f)->glyphs[y][start_x],
		  x - start_x, highlight_flag, 1);
    }
}

/* Erase the character (if any) at the position X, Y in frame F,
   then redraw it and the character after it.
   This is necessary when we erase endng at X,
   in case the character after X overlaps into the one before X.
   Call this function with input blocked.  */

static void
redraw_following_char (f, x, y, highlight_flag)
     FRAME_PTR f;
     int x, y;
     int highlight_flag;
{
  int limit = FRAME_CURRENT_GLYPHS (f)->used[y];
  /* Erase the character after the new ones, in case
     what was here before overlaps it.
     Reoutput that character, and the following character
     (in case the following character overlaps it).  */
  if (x < limit
      && FRAME_CURRENT_GLYPHS (f)->glyphs[y][x] != SPACEGLYPH)
    {
      int end_x = x + 2;
      if (end_x > limit)
	end_x = limit;
      XClearArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  CHAR_TO_PIXEL_COL (f, x),
		  CHAR_TO_PIXEL_ROW (f, y),
		  FONT_WIDTH (f->display.x->font),
		  f->display.x->line_height, False);

      dumpglyphs (f, CHAR_TO_PIXEL_COL (f, x),
		  CHAR_TO_PIXEL_ROW (f, y),
		  &FRAME_CURRENT_GLYPHS (f)->glyphs[y][x],
		  end_x - x, highlight_flag, 1);
    }
}
#endif /* 0 */

#if 0 /* Not in use yet */

/* Return 1 if character C in font F extends past its left edge.  */

static int
font_char_overlap_left (font, c)
     XFontStruct *font;
     int c;
{
  XCharStruct *s;

  /* Find the bounding-box info for C.  */
  if (font->per_char == 0)
    s = &font->max_bounds;
  else
    {
      int rowlen = font->max_char_or_byte2 - font->min_char_or_byte2 + 1;
      int row, within;

      /* Decode char into row number (byte 1) and code within row (byte 2).  */
      row = c >> 8;
      within = c & 0177;
      if (!(within >= font->min_char_or_byte2
	    && within <= font->max_char_or_byte2
	    && row >= font->min_byte1
	    && row <= font->max_byte1))
	{
	  /* If char is out of range, try the font's default char instead.  */
	  c = font->default_char;
	  row = c >> (INTBITS - 8);
	  within = c & 0177;
	}
      if (!(within >= font->min_char_or_byte2
	    && within <= font->max_char_or_byte2
	    && row >= font->min_byte1
	    && row <= font->max_byte1))
	/* Still out of range means this char does not overlap.  */
	return 0;
      else
	/* We found the info for this char.  */
	s = (font->per_char + (within - font->min_char_or_byte2)
	     + row * rowlen);
    }

  return (s && s->lbearing < 0);
}

/* Return 1 if character C in font F extends past its right edge.  */

static int
font_char_overlap_right (font, c)
     XFontStruct *font;
     int c;
{
  XCharStruct *s;

  /* Find the bounding-box info for C.  */
  if (font->per_char == 0)
    s = &font->max_bounds;
  else
    {
      int rowlen = font->max_char_or_byte2 - font->min_char_or_byte2 + 1;
      int row, within;

      /* Decode char into row number (byte 1) and code within row (byte 2).  */
      row = c >> 8;
      within = c & 0177;
      if (!(within >= font->min_char_or_byte2
	    && within <= font->max_char_or_byte2
	    && row >= font->min_byte1
	    && row <= font->max_byte1))
	{
	  /* If char is out of range, try the font's default char instead.  */
	  c = font->default_char;
	  row = c >> (INTBITS - 8);
	  within = c & 0177;
	}
      if (!(within >= font->min_char_or_byte2
	    && within <= font->max_char_or_byte2
	    && row >= font->min_byte1
	    && row <= font->max_byte1))
	/* Still out of range means this char does not overlap.  */
	return 0;
      else
	/* We found the info for this char.  */
	s = (font->per_char + (within - font->min_char_or_byte2)
	     + row * rowlen);
    }

  return (s && s->rbearing >= s->width);
}
#endif /* 0 */

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

      gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		      GCFunction | GCForeground, &values);
    }

    {
      int width  = PIXEL_WIDTH  (f);
      int height = PIXEL_HEIGHT (f);

      XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		      width/4, height/4, width/2, height/2);
      XFlush (FRAME_X_DISPLAY (f));

      {
	struct timeval wakeup, now;

	EMACS_GET_TIME (wakeup);

	/* Compute time to wait until, propagating carry from usecs.  */
	wakeup.tv_usec += 150000;
	wakeup.tv_sec += (wakeup.tv_usec / 1000000);
	wakeup.tv_usec %= 1000000;

	/* Keep waiting until past the time wakeup.  */
	while (1)
	  {
	    struct timeval timeout;

	    EMACS_GET_TIME (timeout);

	    /* In effect, timeout = wakeup - timeout.
	       Break if result would be negative.  */
	    if (timeval_subtract (&timeout, wakeup, timeout))
	      break;

	    /* Try to wait that long--but we might wake up sooner.  */
	    select (0, 0, 0, 0, &timeout);
	  }
      }

      XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		      width/4, height/4, width/2, height/2);
      XFreeGC (FRAME_X_DISPLAY (f), gc);
      XFlush (FRAME_X_DISPLAY (f));
    }
  }

  UNBLOCK_INPUT;
}

#endif


/* Make audible bell.  */

#define XRINGBELL XBell (FRAME_X_DISPLAY (selected_frame), 0)

XTring_bell ()
{
  if (FRAME_X_DISPLAY (selected_frame) == 0)
    return;

#if defined (HAVE_TIMEVAL) && defined (HAVE_SELECT)
  if (visible_bell)
    XTflash (selected_frame);
  else
#endif
    {
      BLOCK_INPUT;
      XRINGBELL;
      XFlush (FRAME_X_DISPLAY (selected_frame));
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

/* These variables need not be per frame
   because redisplay is done on a frame-by-frame basis
   and the line dance for one frame is finished before
   anything is done for anoter frame.  */

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
XTins_del_lines (vpos, n)
     int vpos, n;
{
  register int fence, i;

  if (vpos >= flexlines)
    return;

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

  /* Must check this flag first.  If it's not set, then not only is the
     array uninitialized, but we might not even have a frame.  */
  if (!line_dance_in_progress)
    return;

  f = updating_frame;
  if (f == 0)
    abort ();

  ht = f->height;
  intborder = f->display.x->internal_border_width;

  x_display_cursor (updating_frame, 0);

  for (i = 0; i < ht; ++i)
    if (line_dance[i] != -1 && (distance = line_dance[i]-i) > 0)
      {
	for (j = i; (j < ht && line_dance[j] != -1
		     && line_dance[j]-j == distance); ++j);
	/* Copy [i,j) upward from [i+distance,j+distance) */
	XCopyArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   FRAME_X_WINDOW (f), f->display.x->normal_gc,
		   intborder, CHAR_TO_PIXEL_ROW (f, i+distance),
		   f->width * FONT_WIDTH (f->display.x->font),
		   (j-i) * f->display.x->line_height,
		   intborder, CHAR_TO_PIXEL_ROW (f, i));
	i = j-1;
      }

  for (i = ht; --i >=0; )
    if (line_dance[i] != -1 && (distance = line_dance[i]-i) < 0)
      {
	for (j = i; (--j >= 0 && line_dance[j] != -1
		     && line_dance[j]-j == distance););
	/* Copy (j,i] downward from (j+distance, i+distance] */
	XCopyArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   FRAME_X_WINDOW (f), f->display.x->normal_gc,
		   intborder, CHAR_TO_PIXEL_ROW (f, j+1+distance),
		   f->width * FONT_WIDTH (f->display.x->font),
		   (i-j) * f->display.x->line_height,
		   intborder, CHAR_TO_PIXEL_ROW (f, j+1));
	i = j+1;
      }

  for (i = 0; i < ht; ++i)
    if (line_dance[i] == -1)
      {
	for (j = i; j < ht && line_dance[j] == -1; ++j);
	/* Clear [i,j) */
	XClearArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    intborder, CHAR_TO_PIXEL_ROW (f, i),
		    f->width * FONT_WIDTH (f->display.x->font),
		    (j-i) * f->display.x->line_height, False);
	i = j-1;
      }
  line_dance_in_progress = 0;
}

/* Support routines for exposure events.  */
static void clear_cursor ();

/* Output into a rectangle of an X-window (for frame F)
   the characters in f->phys_lines that overlap that rectangle.
   TOP and LEFT are the position of the upper left corner of the rectangle.
   ROWS and COLS are the size of the rectangle.
   Call this function with input blocked.  */

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

  /* Convert rectangle edges in pixels to edges in chars.
     Round down for left and top, up for right and bottom.  */
  top  = PIXEL_TO_CHAR_ROW (f, top);
  left = PIXEL_TO_CHAR_COL (f, left);
  bottom += (f->display.x->line_height - 1);
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
  /* We used to only do this if Vx_no_window_manager was non-nil, but
     the ICCCM (section 4.1.6) says that the window's border pixmap
     and border pixel are window attributes which are "private to the
     client", so we can always change it to whatever we want.  */
  BLOCK_INPUT;
  XSetWindowBorder (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    f->display.x->border_pixel);
  UNBLOCK_INPUT;
  x_display_cursor (f, 1);
}

static void
frame_unhighlight (f)
     struct frame *f;
{
  /* We used to only do this if Vx_no_window_manager was non-nil, but
     the ICCCM (section 4.1.6) says that the window's border pixmap
     and border pixel are window attributes which are "private to the
     client", so we can always change it to whatever we want.  */
  BLOCK_INPUT;
  XSetWindowBorderPixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			  f->display.x->border_tile);
  UNBLOCK_INPUT;
  x_display_cursor (f, 1);
}

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
      XSETFRAME (XWINDOW (selected_frame->selected_window)->frame,
		 selected_frame);
      Fselect_window (selected_frame->selected_window);
      choose_minibuf_frame ();
#endif /* ! 0 */

      if (x_focus_frame && x_focus_frame->auto_raise)
	pending_autoraise_frame = x_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  XTframe_rehighlight ();
}

/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
x_mouse_leave ()
{
  if (! x_focus_event_frame)
    x_new_focus_frame (NULL);
  else
    x_new_focus_frame (x_focus_event_frame);  /* Was f, but that seems wrong.  */
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

/* Initialize mode_switch_bit and modifier_meaning.  */
static void
x_find_modifier_meanings (dpyinfo)
     struct x_display_info *dpyinfo;
{
  int min_code, max_code;
  KeySym *syms;
  int syms_per_code;
  XModifierKeymap *mods;

  dpyinfo->meta_mod_mask = 0;
  dpyinfo->shift_lock_mask = 0;
  dpyinfo->alt_mod_mask = 0;
  dpyinfo->super_mod_mask = 0;
  dpyinfo->hyper_mod_mask = 0;

#ifdef HAVE_X11R4
  XDisplayKeycodes (dpyinfo->display, &min_code, &max_code);
#else
  min_code = display->min_keycode;
  max_code = display->max_keycode;
#endif

  syms = XGetKeyboardMapping (dpyinfo->display,
			      min_code, max_code - min_code + 1,
			      &syms_per_code);
  mods = XGetModifierMapping (dpyinfo->display);

  /* Scan the modifier table to see which modifier bits the Meta and
     Alt keysyms are on.  */
  {
    int row, col;	/* The row and column in the modifier table. */

    for (row = 3; row < 8; row++)
      for (col = 0; col < mods->max_keypermod; col++)
	{
	  KeyCode code =
	    mods->modifiermap[(row * mods->max_keypermod) + col];

	  /* Zeroes are used for filler.  Skip them.  */
	  if (code == 0)
	    continue;

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
		    dpyinfo->meta_mod_mask |= (1 << row);
		    break;

		  case XK_Alt_L:
		  case XK_Alt_R:
		    dpyinfo->alt_mod_mask |= (1 << row);
		    break;

		  case XK_Hyper_L:
		  case XK_Hyper_R:
		    dpyinfo->hyper_mod_mask |= (1 << row);
		    break;

		  case XK_Super_L:
		  case XK_Super_R:
		    dpyinfo->super_mod_mask |= (1 << row);
		    break;

		  case XK_Shift_Lock:
		    /* Ignore this if it's not on the lock modifier.  */
		    if ((1 << row) == LockMask)
		      dpyinfo->shift_lock_mask = LockMask;
		    break;
		  }
	      }
	  }
	}
  }

  /* If we couldn't find any meta keys, accept any alt keys as meta keys.  */
  if (! dpyinfo->meta_mod_mask)
    {
      dpyinfo->meta_mod_mask = dpyinfo->alt_mod_mask;
      dpyinfo->alt_mod_mask = 0;
    }

  /* If some keys are both alt and meta,
     make them just meta, not alt.  */
  if (dpyinfo->alt_mod_mask & dpyinfo->meta_mod_mask)
    {
      dpyinfo->alt_mod_mask &= ~dpyinfo->meta_mod_mask;
    }

  XFree ((char *) syms);
  XFreeModifiermap (mods);
}

/* Convert between the modifier bits X uses and the modifier bits
   Emacs uses.  */
static unsigned int
x_x_to_emacs_modifiers (dpyinfo, state)
     struct x_display_info *dpyinfo;
     unsigned int state;
{
  return (  ((state & (ShiftMask | dpyinfo->shift_lock_mask)) ? shift_modifier : 0)
	  | ((state & ControlMask)	       ? ctrl_modifier  : 0)
	  | ((state & dpyinfo->meta_mod_mask)  ? meta_modifier  : 0)
	  | ((state & dpyinfo->alt_mod_mask)   ? alt_modifier  : 0)
	  | ((state & dpyinfo->super_mod_mask) ? super_modifier  : 0)
	  | ((state & dpyinfo->hyper_mod_mask) ? hyper_modifier  : 0));
}

static unsigned int
x_emacs_to_x_modifiers (dpyinfo, state)
     struct x_display_info *dpyinfo;
     unsigned int state;
{
  return (  ((state & alt_modifier)	? dpyinfo->alt_mod_mask   : 0)
	  | ((state & super_modifier)	? dpyinfo->super_mod_mask : 0)
	  | ((state & hyper_modifier)	? dpyinfo->hyper_mod_mask : 0)
	  | ((state & shift_modifier)	? ShiftMask        : 0)
	  | ((state & ctrl_modifier)	? ControlMask      : 0)
	  | ((state & meta_modifier)	? dpyinfo->meta_mod_mask  : 0));
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
     XRectangle *bounds;
     int noclip;
{
  /* Arrange for the division in PIXEL_TO_CHAR_COL etc. to round down
     even for negative values.  */
  if (pix_x < 0)
    pix_x -= FONT_WIDTH ((f)->display.x->font) - 1;
  if (pix_y < 0)
    pix_y -= (f)->display.x->line_height - 1;

  pix_x = PIXEL_TO_CHAR_COL (f, pix_x);
  pix_y = PIXEL_TO_CHAR_ROW (f, pix_y);

  if (bounds)
    {
      bounds->width  = FONT_WIDTH  (f->display.x->font);
      bounds->height = f->display.x->line_height;
      bounds->x = CHAR_TO_PIXEL_COL (f, pix_x);
      bounds->y = CHAR_TO_PIXEL_ROW (f, pix_y);
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
  *pix_x = CHAR_TO_PIXEL_COL (f, x);
  *pix_y = CHAR_TO_PIXEL_ROW (f, y);
}

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
  result->code = event->button - Button1;
  result->timestamp = event->time;
  result->modifiers = (x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f),
					       event->state)
		       | (event->type == ButtonRelease
			  ? up_modifier
			  : down_modifier));

  {
    int row, column;

#if 0
    pixel_to_glyph_coords (f, event->x, event->y, &column, &row, NULL, 0);
    XSETFASTINT (result->x, column);
    XSETFASTINT (result->y, row);
#endif
    XSETINT (result->x, event->x);
    XSETINT (result->y, event->y);
    XSETFRAME (result->frame_or_window, f);
  }
}

/* Prepare a menu-event in *RESULT for placement in the input queue.  */

static Lisp_Object
construct_menu_click (result, event, f)
     struct input_event *result;
     XButtonEvent *event;
     struct frame *f;
{
  /* Make the event type no_event; we'll change that when we decide
     otherwise.  */
  result->kind = mouse_click;
  XSETINT (result->code, event->button - Button1);
  result->timestamp = event->time;
  result->modifiers = (x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f),
					       event->state)
		       | (event->type == ButtonRelease
			  ? up_modifier
			  : down_modifier));

  XSETINT (result->x, event->x);
  XSETINT (result->y, -1);
  XSETFRAME (result->frame_or_window, f);
}

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

  if (event->window != FRAME_X_WINDOW (frame))
    {
      mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;

      note_mouse_highlight (frame, -1, -1);

      /* Ask for another mouse motion event.  */
      {
	int dummy;
	Window dummy_window;

	XQueryPointer (event->display, FRAME_X_WINDOW (frame),
		       &dummy_window, &dummy_window,
		       &dummy, &dummy, &dummy, &dummy,
		       (unsigned int *) &dummy);
      }
    }

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  else if (event->x < last_mouse_glyph.x
	   || event->x >= last_mouse_glyph.x + last_mouse_glyph.width
	   || event->y < last_mouse_glyph.y
	   || event->y >= last_mouse_glyph.y + last_mouse_glyph.height)
    {
      mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;

      note_mouse_highlight (frame, event->x, event->y);

      /* Ask for another mouse motion event.  */
      {
	int dummy;
	Window dummy_window;

	XQueryPointer (event->display, FRAME_X_WINDOW (frame),
		       &dummy_window, &dummy_window,
		       &dummy, &dummy, &dummy, &dummy,
		       (unsigned int *) &dummy);
      }
    }
  else
    {
      /* It's on the same glyph.  Call XQueryPointer so we'll get an
	 event the next time the mouse moves and we can see if it's
	 *still* on the same glyph.  */
      int dummy;
      Window dummy_window;

      XQueryPointer (event->display, FRAME_X_WINDOW (frame),
		     &dummy_window, &dummy_window,
		     &dummy, &dummy, &dummy, &dummy,
		     (unsigned int *) &dummy);
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
{
  int row, column, portion;
  XRectangle new_glyph;
  Lisp_Object window;
  struct window *w;

  if (disable_mouse_highlight)
    return;

  FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_x = x;
  FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_y = y;
  FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_frame = f;

  if (FRAME_X_DISPLAY_INFO (f)->mouse_face_defer)
    return;

  if (gc_in_progress)
    {
      FRAME_X_DISPLAY_INFO (f)->mouse_face_deferred_gc = 1;
      return;
    }

  /* Find out which glyph the mouse is on.  */
  pixel_to_glyph_coords (f, x, y, &column, &row,
			 &new_glyph, FRAME_X_DISPLAY_INFO (f)->grabbed);

  /* Which window is that in?  */
  window = window_from_coordinates (f, column, row, &portion);
  w = XWINDOW (window);

  /* If we were displaying active text in another window, clear that.  */
  if (! EQ (window, FRAME_X_DISPLAY_INFO (f)->mouse_face_window))
    clear_mouse_face (FRAME_X_DISPLAY_INFO (f));

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
	clear_mouse_face (FRAME_X_DISPLAY_INFO (f));
      else if (! (EQ (window, FRAME_X_DISPLAY_INFO (f)->mouse_face_window)
		  && row >= FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_row
		  && row <= FRAME_X_DISPLAY_INFO (f)->mouse_face_end_row
		  && (row > FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_row
		      || column >= FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_col)
		  && (row < FRAME_X_DISPLAY_INFO (f)->mouse_face_end_row
		      || column < FRAME_X_DISPLAY_INFO (f)->mouse_face_end_col
		      || FRAME_X_DISPLAY_INFO (f)->mouse_face_past_end)))
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
	  clear_mouse_face (FRAME_X_DISPLAY_INFO (f));

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
				  &FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_col,
				  &FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_row);
	      FRAME_X_DISPLAY_INFO (f)->mouse_face_past_end
		= !fast_find_position (window, after,
				       &FRAME_X_DISPLAY_INFO (f)->mouse_face_end_col,
				       &FRAME_X_DISPLAY_INFO (f)->mouse_face_end_row);
	      FRAME_X_DISPLAY_INFO (f)->mouse_face_window = window;
	      FRAME_X_DISPLAY_INFO (f)->mouse_face_face_id
		= compute_char_face (f, w, pos, 0, 0,
				     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (FRAME_X_DISPLAY_INFO (f), 1);
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
				  &FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_col,
				  &FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_row);
	      FRAME_X_DISPLAY_INFO (f)->mouse_face_past_end
		= !fast_find_position (window, after,
				       &FRAME_X_DISPLAY_INFO (f)->mouse_face_end_col,
				       &FRAME_X_DISPLAY_INFO (f)->mouse_face_end_row);
	      FRAME_X_DISPLAY_INFO (f)->mouse_face_window = window;
	      FRAME_X_DISPLAY_INFO (f)->mouse_face_face_id
		= compute_char_face (f, w, pos, 0, 0,
				     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (FRAME_X_DISPLAY_INFO (f), 1);
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
     struct x_display_info *dpyinfo;
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

  for (i = FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_row;
       i <= FRAME_X_DISPLAY_INFO (f)->mouse_face_end_row; i++)
    {
      int column = (i == FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_row
		    ? FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_col
		    : w->left);
      int endcolumn = (i == FRAME_X_DISPLAY_INFO (f)->mouse_face_end_row
		       ? FRAME_X_DISPLAY_INFO (f)->mouse_face_end_col
		       : w->left + width);
      endcolumn = min (endcolumn, FRAME_CURRENT_GLYPHS (f)->used[i]);

      /* If the cursor's in the text we are about to rewrite,
	 turn the cursor off.  */
      if (i == curs_y
	  && curs_x >= FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_col - 1
	  && curs_x <= FRAME_X_DISPLAY_INFO (f)->mouse_face_end_col)
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
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->display.x->cross_cursor);
  else
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->display.x->text_cursor);
}

/* Clear out the mouse-highlighted active region.
   Redraw it unhighlighted first.  */

static void
clear_mouse_face (dpyinfo)
     struct x_display_info *dpyinfo;
{
  if (! NILP (dpyinfo->mouse_face_window))
    show_mouse_face (dpyinfo, 0);

  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_window = Qnil;
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
XTmouse_position (fp, bar_window, part, x, y, time)
     FRAME_PTR *fp;
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
      Window root;
      int root_x, root_y;

      Window dummy_window;
      int dummy;

      mouse_moved = 0;
      last_mouse_scroll_bar = Qnil;

      /* Figure out which root window we're on.  */
      XQueryPointer (FRAME_X_DISPLAY (*fp),
		     DefaultRootWindow (FRAME_X_DISPLAY (*fp)),

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

	if (FRAME_X_DISPLAY_INFO (*fp)->grabbed && last_mouse_frame
	    && FRAME_LIVE_P (last_mouse_frame))
	  {
	    /* If mouse was grabbed on a frame, give coords for that frame
	       even if the mouse is now outside it.  */
	    XTranslateCoordinates (FRAME_X_DISPLAY (*fp),

				   /* From-window, to-window.  */
				   root, FRAME_X_WINDOW (last_mouse_frame),

				   /* From-position, to-position.  */
				   root_x, root_y, &win_x, &win_y,

				   /* Child of win.  */
				   &child);
	    f1 = last_mouse_frame;
	  }
	else
	  {
	    while (1)
	      {
		XTranslateCoordinates (FRAME_X_DISPLAY (*fp),

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
	    f1 = x_any_window_to_frame (win);
	  }

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
	    int ignore1, ignore2;

	    /* Ok, we found a frame.  Store all the values.  */

	    pixel_to_glyph_coords (f1, win_x, win_y, &ignore1, &ignore2,
				   &last_mouse_glyph,
				   FRAME_X_DISPLAY_INFO (f1)->grabbed);

	    *bar_window = Qnil;
	    *part = 0;
	    *fp = f1;
	    XSETINT (*x, win_x);
	    XSETINT (*y, win_y);
	    *time = last_mouse_movement_time;
	  }
      }
    }

  UNBLOCK_INPUT;
}

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
      Lisp_Object frame, bar, condemned;

      frame = XCONS (tail)->car;
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
	   bar = XSCROLL_BAR (bar)->next)
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
  FRAME_PTR f = XFRAME (WINDOW_FRAME (window));
  struct scroll_bar *bar
    = XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));

  BLOCK_INPUT;

  {
    XSetWindowAttributes a;
    unsigned long mask;
    a.background_pixel = f->display.x->background_pixel;
    a.event_mask = (ButtonPressMask | ButtonReleaseMask
		    | ButtonMotionMask | PointerMotionHintMask
		    | ExposureMask);
    a.cursor = FRAME_X_DISPLAY_INFO (f)->vertical_scroll_bar_cursor;

    mask = (CWBackPixel | CWEventMask | CWCursor);

#if 0

    ac = 0;
    XtSetArg (al[ac], XtNx, left); ac++;
    XtSetArg (al[ac], XtNy, top); ac++;
    XtSetArg (al[ac], XtNwidth, width); ac++;
    XtSetArg (al[ac], XtNheight, height); ac++;
    XtSetArg (al[ac], XtNborderWidth, 0); ac++;
    sb_widget = XtCreateManagedWidget ("box",
				       boxWidgetClass,
				       f->display.x->edit_widget, al, ac);
    SET_SCROLL_BAR_X_WINDOW
      (bar, sb_widget->core.window);
#endif
    SET_SCROLL_BAR_X_WINDOW
      (bar,
       XCreateWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),

		      /* Position and size of scroll bar.  */
		      left, top, width, height,

		      /* Border width, depth, class, and visual.  */
		      0, CopyFromParent, CopyFromParent, CopyFromParent,

		      /* Attributes.  */
		      mask, &a));
  }

  XSETWINDOW (bar->window, window);
  XSETINT (bar->top, top);
  XSETINT (bar->left, left);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);
  XSETINT (bar->start, 0);
  XSETINT (bar->end, 0);
  bar->dragging = Qnil;

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  XMapRaised (FRAME_X_DISPLAY (f), SCROLL_BAR_X_WINDOW (bar));

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
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  GC gc = f->display.x->normal_gc;

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
    XSETINT (bar->start, start);
    XSETINT (bar->end, end);

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
      XClearArea (FRAME_X_DISPLAY (f), w,

		  /* x, y, width, height, and exposures.  */
		  VERTICAL_SCROLL_BAR_LEFT_BORDER,
		  VERTICAL_SCROLL_BAR_TOP_BORDER,
		  inside_width, start,
		  False);

    /* Draw the handle itself.  */
    XFillRectangle (FRAME_X_DISPLAY (f), w, gc,

		    /* x, y, width, height */
		    VERTICAL_SCROLL_BAR_LEFT_BORDER,
		    VERTICAL_SCROLL_BAR_TOP_BORDER + start,
		    inside_width, end - start);


    /* Draw the empty space below the handle.  Note that we can't
       clear zero-height areas; that means "clear to end of window." */
    if (end < inside_height)
      XClearArea (FRAME_X_DISPLAY (f), w,

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
  Window w = SCROLL_BAR_X_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

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
      XConfigureWindow (FRAME_X_DISPLAY (f), SCROLL_BAR_X_WINDOW (bar),
			mask, &wc);
  }

  XSETINT (bar->left, left);
  XSETINT (bar->top, top);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);

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
  XDestroyWindow (FRAME_X_DISPLAY (f), SCROLL_BAR_X_WINDOW (bar));

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
  int pixel_width
    = (FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->display.x->font)));
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
    XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
    if (! NILP (bar->next))
      XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
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
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  GC gc = f->display.x->normal_gc;

  BLOCK_INPUT;

  x_scroll_bar_set_handle (bar, XINT (bar->start), XINT (bar->end), 1);

  /* Draw a one-pixel border just inside the edges of the scroll bar. */
  XDrawRectangle (FRAME_X_DISPLAY (f), w, gc,

		  /* x, y, width, height */
		  0, 0, XINT (bar->width) - 1, XINT (bar->height) - 1);

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
  emacs_event->code = event->xbutton.button - Button1;
  emacs_event->modifiers =
    (x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (XFRAME (WINDOW_FRAME (XWINDOW (bar->window)))),
			     event->xbutton.state)
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
      XSETINT (bar->dragging, y - XINT (bar->start));
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
      XSETINT (emacs_event->x, y);
#else
    XSETINT (emacs_event->x, y);
#endif

    XSETINT (emacs_event->y, top_range);
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
  XSETVECTOR (last_mouse_scroll_bar, bar);

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
    Window dummy_window;

    XQueryPointer (event->xmotion.display, event->xmotion.window,
		   &dummy_window, &dummy_window,
		   &dummy, &dummy, &dummy, &dummy,
		   (unsigned int *) &dummy);
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
  Window w = SCROLL_BAR_X_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int win_x, win_y;
  Window dummy_window;
  int dummy_coord;
  unsigned int dummy_mask;

  BLOCK_INPUT;

  /* Get the mouse's position relative to the scroll bar window, and
     report that.  */
  if (! XQueryPointer (FRAME_X_DISPLAY (f), w,

		       /* Root, child, root x and root y.  */
		       &dummy_window, &dummy_window,
		       &dummy_coord, &dummy_coord,

		       /* Position relative to scroll bar.  */
		       &win_x, &win_y,

		       /* Mouse buttons and modifier keys.  */
		       &dummy_mask))
    ;
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

      *fp = f;
      *bar_window = bar->window;

      if (! NILP (bar->dragging))
	*part = scroll_bar_handle;
      else if (win_y < XINT (bar->start))
	*part = scroll_bar_above_handle;
      else if (win_y < XINT (bar->end) + VERTICAL_SCROLL_BAR_MIN_HANDLE)
	*part = scroll_bar_handle;
      else
	*part = scroll_bar_below_handle;

      XSETINT (*x, win_y);
      XSETINT (*y, top_range);

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

  for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
       bar = XSCROLL_BAR (bar)->next)
    XClearArea (FRAME_X_DISPLAY (f), SCROLL_BAR_X_WINDOW (XSCROLL_BAR (bar)),
		0, 0, 0, 0, True);
}

/* This processes Expose events from the menubar specific X event
   loop in menubar.c.  This allows to redisplay the frame if necessary
   when handling menubar or popup items.  */

void
process_expose_from_menu (event)
     XEvent event;
{
  FRAME_PTR f;

  BLOCK_INPUT;

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

  UNBLOCK_INPUT;
}

/* Define a queue to save up SelectionRequest events for later handling.  */

struct selection_event_queue
  {
    XEvent event;
    struct selection_event_queue *next;
  };

static struct selection_event_queue *queue;

/* Nonzero means queue up certain events--don't process them yet.  */
static int x_queue_selection_requests;

/* Queue up an X event *EVENT, to be processed later.  */

static void
x_queue_event (f, event)
     FRAME_PTR f;
     XEvent *event;
{
  struct selection_event_queue *queue_tmp
    = (struct selection_event_queue *) malloc (sizeof (struct selection_event_queue));

  if (queue_tmp != NULL)
    {
      queue_tmp->event = *event;
      queue_tmp->next = queue;
      queue = queue_tmp;
    }
}

/* Take all the queued events and put them back
   so that they get processed afresh.  */

static void
x_unqueue_events (f)
     FRAME_PTR f;
{
  while (queue != NULL)
    {
      struct selection_event_queue *queue_tmp = queue;
      XPutBackEvent (FRAME_X_DISPLAY (f), &queue_tmp->event);
      queue = queue_tmp->next;
      free ((char *)queue_tmp);
    }
}

/* Start queuing SelectionRequest events.  */

void
x_start_queuing_selection_requests (f)
     FRAME_PTR f;
{
  x_queue_selection_requests++;
}

/* Stop queuing SelectionRequest events.  */

void
x_stop_queuing_selection_requests (f)
     FRAME_PTR f;
{
  x_queue_selection_requests--;
  x_unqueue_events (f);
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

/* Record the last 100 characters stored
   to help debug the loss-of-chars-during-GC problem.  */
int temp_index;
short temp_buffer[100];

/* Set this to nonzero to fake an "X I/O error"
   on a particular display.  */
struct x_display_info *XTread_socket_fake_io_error;

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
  struct x_display_info *dpyinfo;

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
    abort ();			/* Don't think this happens. */

  /* Find the display we are supposed to read input for.
     It's the one communicating on descriptor SD.  */
  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    {
#if 0 /* This ought to be unnecessary; let's verify it.  */
#ifdef FIOSNBIO
      /* If available, Xlib uses FIOSNBIO to make the socket
	 non-blocking, and then looks for EWOULDBLOCK.  If O_NDELAY is set,
	 FIOSNBIO is ignored, and instead of signalling EWOULDBLOCK,
	 a read returns 0, which Xlib interprets as equivalent to EPIPE. */
      fcntl (dpyinfo->connection, F_SETFL, 0);
#endif /* ! defined (FIOSNBIO) */
#endif

#if 0 /* This code can't be made to work, with multiple displays,
	 and appears not to be used on any system any more.
	 Also keyboard.c doesn't turn O_NDELAY on and off
	 for X connections.  */
#ifndef SIGIO
#ifndef HAVE_SELECT
      if (! (fcntl (dpyinfo->connection, F_GETFL, 0) & O_NDELAY))
	{
	  extern int read_alarm_should_throw;
	  read_alarm_should_throw = 1;
	  XPeekEvent (dpyinfo->display, &event);
	  read_alarm_should_throw = 0;
	}
#endif /* HAVE_SELECT */
#endif /* SIGIO */
#endif

      /* For debugging, this gives a way to fake an I/O error.  */
      if (dpyinfo == XTread_socket_fake_io_error)
	{
	  XTread_socket_fake_io_error = 0;
	  x_io_error_quitter (dpyinfo->display);
	}

      while (XPending (dpyinfo->display) != 0)
	{
	  XNextEvent (dpyinfo->display, &event);
	  event_found = 1;

	  switch (event.type)
	    {
	    case ClientMessage:
	      {
		if (event.xclient.message_type
		    == dpyinfo->Xatom_wm_protocols
		    && event.xclient.format == 32)
		  {
		    if (event.xclient.data.l[0]
			== dpyinfo->Xatom_wm_take_focus)
		      {
			f = x_window_to_frame (event.xclient.window);
			/* Since we set WM_TAKE_FOCUS, we must call
			   XSetInputFocus explicitly.  But not if f is null,
			   since that might be an event for a deleted frame.  */
			if (f)
			  XSetInputFocus (event.xclient.display,
					  event.xclient.window,
					  RevertToPointerRoot,
					  event.xclient.data.l[1]);
			/* Not certain about handling scroll bars here */
		      }
		    else if (event.xclient.data.l[0]
			     == dpyinfo->Xatom_wm_save_yourself)
		      {
			/* Save state modify the WM_COMMAND property to
			   something which can reinstate us. This notifies
			   the session manager, who's looking for such a
			   PropertyNotify.  Can restart processing when
			   a keyboard or mouse event arrives. */
			if (numchars > 0)
			  {
			    f = x_top_window_to_frame (event.xclient.window);

			    /* This is just so we only give real data once
			       for a single Emacs process.  */
			    if (f == selected_frame)
			      XSetCommand (FRAME_X_DISPLAY (f),
					   event.xclient.window,
					   initial_argv, initial_argc);
			    else
			      XSetCommand (FRAME_X_DISPLAY (f),
					   event.xclient.window,
					   0, 0);
			  }
		      }
		    else if (event.xclient.data.l[0]
			     == dpyinfo->Xatom_wm_delete_window)
		      {
			struct frame *f = x_any_window_to_frame (event.xclient.window);

			if (f)
			  {
			    if (numchars == 0)
			      abort ();

			    bufp->kind = delete_window_event;
			    XSETFRAME (bufp->frame_or_window, f);
			    bufp++;

			    count += 1;
			    numchars -= 1;
			  }
		      }
		  }
		else if (event.xclient.message_type
			 == dpyinfo->Xatom_wm_configure_denied)
		  {
		  }
		else if (event.xclient.message_type
			 == dpyinfo->Xatom_wm_window_moved)
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
#if defined (USE_X_TOOLKIT) && defined (HAVE_X11R5)
		else if (event.xclient.message_type
			 == dpyinfo->Xatom_editres)
		  {
		    struct frame *f = x_any_window_to_frame (event.xclient.window);
		    _XEditResCheckMessages (f->display.x->widget, NULL, &event, NULL);
		  }
#endif /* USE_X_TOOLKIT and HAVE_X11R5 */
	      }
	      break;

	    case SelectionNotify:
#ifdef USE_X_TOOLKIT
	      if (! x_window_to_frame (event.xselection.requestor))
		goto OTHER;
#endif /* not USE_X_TOOLKIT */
	      x_handle_selection_notify (&event);
	      break;

	    case SelectionClear:	/* Someone has grabbed ownership. */
#ifdef USE_X_TOOLKIT
	      if (! x_window_to_frame (event.xselectionclear.window))
		goto OTHER;
#endif /* USE_X_TOOLKIT */
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
	      break;

	    case SelectionRequest:	/* Someone wants our selection. */
#ifdef USE_X_TOOLKIT
	      if (!x_window_to_frame (event.xselectionrequest.owner))
		goto OTHER;
#endif /* USE_X_TOOLKIT */
	      if (x_queue_selection_requests)
		x_queue_event (x_window_to_frame (event.xselectionrequest.owner),
			       &event);
	      else
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
	      break;

	    case PropertyNotify:
#ifdef USE_X_TOOLKIT
	      if (!x_any_window_to_frame (event.xproperty.window))
		goto OTHER;
#endif /* not USE_X_TOOLKIT */
	      x_handle_property_notify (&event);
	      break;

	    case ReparentNotify:
	      f = x_top_window_to_frame (event.xreparent.window);
	      if (f)
		{
		  int x, y;
		  f->display.x->parent_desc = event.xreparent.parent;
		  x_real_positions (f, &x, &y);
		  f->display.x->left_pos = x;
		  f->display.x->top_pos = y;
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
	      else
		{
		  struct scroll_bar *bar
		    = x_window_to_scroll_bar (event.xexpose.window);

		  if (bar)
		    x_scroll_bar_expose (bar, &event);
#ifdef USE_X_TOOLKIT
		  else
		    goto OTHER;
#endif /* USE_X_TOOLKIT */
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
#ifdef USE_X_TOOLKIT
	      else
		goto OTHER;
#endif /* USE_X_TOOLKIT */
	      break;

	    case NoExpose:		/* This occurs when an XCopyArea's
				      source area was completely
				      available */
	      break;

	    case UnmapNotify:
	      f = x_any_window_to_frame (event.xunmap.window);
	      if (f)		/* F may no longer exist if
				       the frame was deleted.  */
		{
		  /* While a frame is unmapped, display generation is
		     disabled; you don't want to spend time updating a
		     display that won't ever be seen.  */
		  f->async_visible = 0;
		  /* We can't distinguish, from the event, whether the window
		     has become iconified or invisible.  So assume, if it
		     was previously visible, than now it is iconified.
		     We depend on x_make_frame_invisible to mark it iconified.  */
		  if (FRAME_VISIBLE_P (f) || FRAME_ICONIFIED_P (f))
		    f->async_iconified = 1;

		  bufp->kind = iconify_event;
		  XSETFRAME (bufp->frame_or_window, f);
		  bufp++;
		  count++;
		  numchars--;
		}
#ifdef USE_X_TOOLKIT
	      goto OTHER;
#endif /* USE_X_TOOLKIT */
	      break;

	    case MapNotify:
	      /* We use x_top_window_to_frame because map events can come
		 for subwindows and they don't mean that the frame is visible.  */
	      f = x_top_window_to_frame (event.xmap.window);
	      if (f)
		{
		  f->async_visible = 1;
		  f->async_iconified = 0;

		  /* wait_reading_process_input will notice this and update
		     the frame's display structures.  */
		  SET_FRAME_GARBAGED (f);

		  bufp->kind = deiconify_event;
		  XSETFRAME (bufp->frame_or_window, f);
		  bufp++;
		  count++;
		  numchars--;
		}
#ifdef USE_X_TOOLKIT
	      goto OTHER;
#endif /* USE_X_TOOLKIT */
	      break;

	      /* Turn off processing if we become fully obscured. */
	    case VisibilityNotify:
	      break;

	    case KeyPress:
	      f = x_any_window_to_frame (event.xkey.window);

	      if (f != 0)
		{
		  KeySym keysym, orig_keysym;
		  /* al%imercury@uunet.uu.net says that making this 81 instead of
		     80 fixed a bug whereby meta chars made his Emacs hang.  */
		  unsigned char copy_buffer[81];
		  int modifiers;

		  event.xkey.state
		    |= x_emacs_to_x_modifiers (FRAME_X_DISPLAY_INFO (f),
					       extra_keyboard_modifiers);
		  modifiers = event.xkey.state;

		  /* This will have to go some day...  */

		  /* make_lispy_event turns chars into control chars.
		     Don't do it here because XLookupString is too eager.  */
		  event.xkey.state &= ~ControlMask;
		  nbytes =
		    XLookupString (&event.xkey, copy_buffer, 80, &keysym,
				   &compose_status);

		  orig_keysym = keysym;

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
#ifdef XK_dead_circumflex
			   || orig_keysym == XK_dead_circumflex
#endif
#ifdef XK_dead_grave
			   || orig_keysym == XK_dead_grave
#endif
#ifdef XK_dead_tilde
			   || orig_keysym == XK_dead_tilde
#endif
#ifdef XK_dead_diaeresis
			   || orig_keysym == XK_dead_diaeresis
#endif
#ifdef XK_dead_macron
			   || orig_keysym == XK_dead_macron
#endif
#ifdef XK_dead_degree
			   || orig_keysym == XK_dead_degree
#endif
#ifdef XK_dead_acute
			   || orig_keysym == XK_dead_acute
#endif
#ifdef XK_dead_cedilla
			   || orig_keysym == XK_dead_cedilla
#endif
#ifdef XK_dead_breve
			   || orig_keysym == XK_dead_breve
#endif
#ifdef XK_dead_ogonek
			   || orig_keysym == XK_dead_ogonek
#endif
#ifdef XK_dead_caron
			   || orig_keysym == XK_dead_caron
#endif
#ifdef XK_dead_doubleacute
			   || orig_keysym == XK_dead_doubleacute
#endif
#ifdef XK_dead_abovedot
			   || orig_keysym == XK_dead_abovedot
#endif
			   || IsKeypadKey (keysym) /* 0xff80 <= x < 0xffbe */
			   || IsFunctionKey (keysym) /* 0xffbe <= x < 0xffe1 */
			   /* Any "vendor-specific" key is ok.  */
			   || (orig_keysym & (1 << 28)))
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
			  bufp->code = keysym;
			  XSETFRAME (bufp->frame_or_window, f);
			  bufp->modifiers
			    = x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f),
						      modifiers);
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
			      bufp->code = copy_buffer[i];
			      XSETFRAME (bufp->frame_or_window, f);
			      bufp->modifiers
				= x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f),
							  modifiers);
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

	      /* Here's a possible interpretation of the whole
		 FocusIn-EnterNotify FocusOut-LeaveNotify mess.  If you get a
		 FocusIn event, you have to get a FocusOut event before you
		 relinquish the focus.  If you haven't received a FocusIn event,
		 then a mere LeaveNotify is enough to free you.  */

	    case EnterNotify:
	      f = x_any_window_to_frame (event.xcrossing.window);

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
	      /* EnterNotify counts as mouse movement,
		 so update things that depend on mouse position.  */
	      if (f)
		note_mouse_movement (f, &event.xmotion);
#ifdef USE_X_TOOLKIT
	      goto OTHER;
#endif /* USE_X_TOOLKIT */
	      break;

	    case FocusIn:
	      f = x_any_window_to_frame (event.xfocus.window);
	      if (event.xfocus.detail != NotifyPointer)
		x_focus_event_frame = f;
	      if (f)
		x_new_focus_frame (f);
#ifdef USE_X_TOOLKIT
	      goto OTHER;
#endif /* USE_X_TOOLKIT */
	      break;


	    case LeaveNotify:
	      f = x_top_window_to_frame (event.xcrossing.window);
	      if (f)
		{
		  if (f == dpyinfo->mouse_face_mouse_frame)
		    /* If we move outside the frame,
		       then we're certainly no longer on any text in the frame.  */
		    clear_mouse_face (dpyinfo);

		  if (event.xcrossing.focus)
		    x_mouse_leave ();
		  else
		    {
		      if (f == x_focus_event_frame)
			x_focus_event_frame = 0;
		      if (f == x_focus_frame)
			x_new_focus_frame (0);
		    }
		}
#ifdef USE_X_TOOLKIT
	      goto OTHER;
#endif /* USE_X_TOOLKIT */
	      break;

	    case FocusOut:
	      f = x_any_window_to_frame (event.xfocus.window);
	      if (event.xfocus.detail != NotifyPointer
		  && f == x_focus_event_frame)
		x_focus_event_frame = 0;
	      if (f && f == x_focus_frame)
		x_new_focus_frame (0);
#ifdef USE_X_TOOLKIT
	      goto OTHER;
#endif /* USE_X_TOOLKIT */
	      break;

	    case MotionNotify:
	      {
		if (dpyinfo->grabbed && last_mouse_frame
		    && FRAME_LIVE_P (last_mouse_frame))
		  f = last_mouse_frame;
		else
		  f = x_window_to_frame (event.xmotion.window);
		if (f)
		  note_mouse_movement (f, &event.xmotion);
		else
		  {
		    struct scroll_bar *bar
		      = x_window_to_scroll_bar (event.xmotion.window);

		    if (bar)
		      x_scroll_bar_note_movement (bar, &event);

		    /* If we move outside the frame,
		       then we're certainly no longer on any text in the frame.  */
		    clear_mouse_face (dpyinfo);
		  }
	      }
#if 0 /* This should be unnecessary, since the toolkit has no use
	     for motion events that happen outside of the menu event loop,
	     and it seems to cause the bug that mouse events stop coming
	     after a while.  */
#ifdef USE_X_TOOLKIT
	      goto OTHER;
#endif /* USE_X_TOOLKIT */
#endif
	      break;

	    case ConfigureNotify:
	      f = x_any_window_to_frame (event.xconfigure.window);
#ifdef USE_X_TOOLKIT
	      if (f
#if 0
		  && ! event.xconfigure.send_event
#endif
		  && (event.xconfigure.window == XtWindow (f->display.x->widget)))
		{
		  Window win, child;
		  int win_x, win_y;

		  /* Find the position of the outside upper-left corner of
		     the window, in the root coordinate system.  Don't
		     refer to the parent window here; we may be processing
		     this event after the window manager has changed our
		     parent, but before we have reached the ReparentNotify.  */
		  XTranslateCoordinates (FRAME_X_DISPLAY (f),

					 /* From-window, to-window.  */
					 XtWindow (f->display.x->widget),
					 FRAME_X_DISPLAY_INFO (f)->root_window,

					 /* From-position, to-position.  */
					 -event.xconfigure.border_width,
					 -event.xconfigure.border_width,
					 &win_x, &win_y,

					 /* Child of win.  */
					 &child);
		  event.xconfigure.x = win_x;
		  event.xconfigure.y = win_y;

		  f->display.x->pixel_width = event.xconfigure.width;
		  f->display.x->pixel_height = event.xconfigure.height;
		  f->display.x->left_pos = event.xconfigure.x;
		  f->display.x->top_pos = event.xconfigure.y;

		  /* What we have now is the position of Emacs's own window.
		     Convert that to the position of the window manager window.  */
		  {
		    int x, y;
		    x_real_positions (f, &x, &y);
		    f->display.x->left_pos = x;
		    f->display.x->top_pos = y;
		  }
		}
	      goto OTHER;
#else /* not USE_X_TOOLKIT */
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
		      XTranslateCoordinates (FRAME_X_DISPLAY (f),

					     /* From-window, to-window.  */
					     f->display.x->window_desc,
					     FRAME_X_DISPLAY_INFO (f)->root_window,

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

		  /* What we have now is the position of Emacs's own window.
		     Convert that to the position of the window manager window.  */
		  {
		    int x, y;
		    x_real_positions (f, &x, &y);
		    f->display.x->left_pos = x;
		    f->display.x->top_pos = y;
		    if (y != event.xconfigure.y)
		      {
			/* Since the WM decorations come below top_pos now,
			   we must put them below top_pos in the future.  */
			f->display.x->win_gravity = NorthWestGravity;
			x_wm_set_size_hint (f, 0, 0);
		      }
		  }
		}
#endif /* not USE_X_TOOLKIT */
	      break;

	    case ButtonPress:
	    case ButtonRelease:
	      {
		/* If we decide we want to generate an event to be seen
		   by the rest of Emacs, we put it here.  */
		struct input_event emacs_event;
		emacs_event.kind = no_event;

		bzero (&compose_status, sizeof (compose_status));

		f = x_window_to_frame (event.xbutton.window);
		if (f)
		  {
		    if (!x_focus_frame || (f == x_focus_frame))
		      construct_mouse_click (&emacs_event, &event, f);
		  }
		else
		  {
		    struct scroll_bar *bar
		      = x_window_to_scroll_bar (event.xbutton.window);

		    if (bar)
		      x_scroll_bar_handle_click (bar, &event, &emacs_event);
#ifdef USE_X_TOOLKIT
		    else
		      {
			/* Assume we have a menubar button press. A bad
			   assumption should behave benignly. */
			popup_get_selection (&event);
			break;
		      }
#endif /* USE_X_TOOLKIT */
		  }

		if (event.type == ButtonPress)
		  {
		    dpyinfo->grabbed |= (1 << event.xbutton.button);
		    last_mouse_frame = f;
		  }
		else
		  {
		    dpyinfo->grabbed &= ~(1 << event.xbutton.button);
		  }

		if (numchars >= 1 && emacs_event.kind != no_event)
		  {
		    bcopy (&emacs_event, bufp, sizeof (struct input_event));
		    bufp++;
		    count++;
		    numchars--;
		  }

#ifdef USE_X_TOOLKIT
		goto OTHER;
#endif /* USE_X_TOOLKIT */
	      }
	      break;

	    case CirculateNotify:
	      break;
	    case CirculateRequest:
	      break;

	    case MappingNotify:
	      /* Someone has changed the keyboard mapping - update the
		 local cache.  */
	      switch (event.xmapping.request)
		{
		case MappingModifier:
		  x_find_modifier_meanings (dpyinfo);
		  /* This is meant to fall through.  */
		case MappingKeyboard:
		  XRefreshKeyboardMapping (&event.xmapping);
		}
#ifdef USE_X_TOOLKIT
	      goto OTHER;
#endif /* USE_X_TOOLKIT */
	      break;

	    default:
#ifdef USE_X_TOOLKIT
	    OTHER:
	      BLOCK_INPUT;
	      XtDispatchEvent (&event);
	      UNBLOCK_INPUT;
#endif /* USE_X_TOOLKIT */
	      break;
	    }
	}
    }

  /* On some systems, an X bug causes Emacs to get no more events
     when the window is destroyed.  Detect that.  (1994.)  */
  if (! event_found)
    {
      /* Emacs and the X Server eats up CPU time if XNoOp is done every time.
	 One XNOOP in 100 loops will make Emacs terminate.
	 B. Bretthauer, 1994 */
      x_noop_count++;
      if (x_noop_count >= 100)
	{
	  x_noop_count=0;
	  /* Use the first display in the list.  Why not?  */
	  XNoOp (x_display_list->display);
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
  int left = CHAR_TO_PIXEL_COL (f, curs_x);
  int top  = CHAR_TO_PIXEL_ROW (f, curs_y);
  int width = FONT_WIDTH (f->display.x->font);
  int height = f->display.x->line_height;

  XDrawRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  f->display.x->cursor_gc,
		  left, top, width - 1, height - 1);
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
      XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		      f->display.x->cursor_gc,
		      CHAR_TO_PIXEL_COL (f, curs_x),
		      CHAR_TO_PIXEL_ROW (f, curs_y),
		      1, f->display.x->line_height);

      f->phys_cursor_x = curs_x;
      f->phys_cursor_y = curs_y;

      f->display.x->current_cursor = bar_cursor;
    }

  if (updating_frame != f)
    XFlush (FRAME_X_DISPLAY (f));
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
      int mouse_face_here = 0;

      /* If the cursor is in the mouse face area, redisplay that when
	 we clear the cursor.  */
      if (f == FRAME_X_DISPLAY_INFO (f)->mouse_face_mouse_frame
	  &&
	  (f->phys_cursor_y > FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_row
	   || (f->phys_cursor_y == FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_row
	       && f->phys_cursor_x >= FRAME_X_DISPLAY_INFO (f)->mouse_face_beg_col))
	  &&
	  (f->phys_cursor_y < FRAME_X_DISPLAY_INFO (f)->mouse_face_end_row
	   || (f->phys_cursor_y == FRAME_X_DISPLAY_INFO (f)->mouse_face_end_row
	       && f->phys_cursor_x < FRAME_X_DISPLAY_INFO (f)->mouse_face_end_col)))
	mouse_face_here = 1;

      /* If the font is not as tall as a whole line,
	 we must explicitly clear the line's whole height.  */
      if (FONT_HEIGHT (f->display.x->font) != f->display.x->line_height)
	XClearArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    CHAR_TO_PIXEL_COL (f, f->phys_cursor_x),
		    CHAR_TO_PIXEL_ROW (f, f->phys_cursor_y),
		    FONT_WIDTH (f->display.x->font),
		    f->display.x->line_height, False);
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
    XFlush (FRAME_X_DISPLAY (f));
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

/* Icons.  */

/* Refresh bitmap kitchen sink icon for frame F
   when we get an expose event for it. */

refreshicon (f)
     struct frame *f;
{
  /* Normally, the window manager handles this function. */
}

/* Make the x-window of frame F use the gnu icon bitmap.  */

int
x_bitmap_icon (f, file)
     struct frame *f;
     Lisp_Object file;
{
  int mask, bitmap_id;
  Window icon_window;

  if (FRAME_X_WINDOW (f) == 0)
    return 1;

  /* Free up our existing icon bitmap if any.  */
  if (f->display.x->icon_bitmap > 0)
    x_destroy_bitmap (f, f->display.x->icon_bitmap);
  f->display.x->icon_bitmap = 0;

  if (STRINGP (file))
    bitmap_id = x_create_bitmap_from_file (f, file);
  else
    {
      /* Create the GNU bitmap if necessary.  */
      if (!FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id < 0)
	FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id
	  = x_create_bitmap_from_data (f, gnu_bits,
				       gnu_width, gnu_height);

      /* The first time we create the GNU bitmap,
	 this increments the refcount one extra time.
	 As a result, the GNU bitmap is never freed.
	 That way, we don't have to worry about allocating it again.  */
      x_reference_bitmap (f, FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id);

      bitmap_id = FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id;
    }

  x_wm_set_icon_pixmap (f, bitmap_id);
  f->display.x->icon_bitmap = bitmap_id;

  return 0;
}


/* Make the x-window of frame F use a rectangle with text.  */

int
x_text_icon (f, icon_name)
     struct frame *f;
     char *icon_name;
{
  if (FRAME_X_WINDOW (f) == 0)
    return 1;

  if (icon_name)
    f->display.x->icon_label = icon_name;
  else
    if (! f->display.x->icon_label)
      f->display.x->icon_label = " *emacs* ";

#if 0
  XSetIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		(char *) f->display.x->icon_label);
#endif

  if (f->display.x->icon_bitmap > 0)
    x_destroy_bitmap (f, f->display.x->icon_bitmap);
  f->display.x->icon_bitmap = 0;
  x_wm_set_icon_pixmap (f, 0);

  return 0;
}

/* Handling X errors.  */

/* Handle the loss of connection to display DISPLAY.  */

static SIGTYPE
x_connection_closed (display, error_message)
     Display *display;
     char *error_message;
{
  struct x_display_info *dpyinfo = x_display_info_for_display (display);
  Lisp_Object frame, tail;

  if (_Xdebug)
    abort ();

  /* First delete frames whose minibuffers are on frames
     that are on the dead display.  */
  FOR_EACH_FRAME (tail, frame)
    {
      Lisp_Object minibuf_frame;
      minibuf_frame
	= WINDOW_FRAME (XWINDOW (FRAME_MINIBUF_WINDOW (XFRAME (frame))));
      if (! EQ (frame, minibuf_frame)
	  && FRAME_X_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
	Fdelete_frame (frame, Qt);
    }

  /* Now delete all remaining frames on the dead display.
     We are now sure none of these is used as the minibuffer
     for another frame that we need to delete.  */
  FOR_EACH_FRAME (tail, frame)
    if (FRAME_X_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
      Fdelete_frame (frame, Qt);

  x_delete_display (dpyinfo);

  if (x_display_list == 0)
    {
      fprintf (stderr, "%s", error_message);
      shut_down_emacs (0, 0, Qnil);
      exit (70);
    }

  /* Ordinary stack unwind doesn't deal with these.  */
#ifdef SIGIO
  sigunblock (sigmask (SIGIO));
#endif
  sigunblock (sigmask (SIGALRM));
  TOTALLY_UNBLOCK_INPUT;

  error ("%s", error_message);
}

/* This is the usual handler for X protocol errors.
   It kills all frames on the display that we got the error for.
   If that was the only one, it prints an error message and kills Emacs.  */

static int
x_error_quitter (display, error)
     Display *display;
     XErrorEvent *error;
{
  char buf[256], buf1[356];

  /* Note that there is no real way portable across R3/R4 to get the
     original error handler.  */

  XGetErrorText (display, error->error_code, buf, sizeof (buf));
  sprintf (buf1, "X protocol error: %s on protocol request %d",
	   buf, error->request_code);
  x_connection_closed (display, buf1);
}

/* This is the handler for X IO errors, always.
   It kills all frames on the display that we lost touch with.
   If that was the only one, it prints an error message and kills Emacs.  */

static int
x_io_error_quitter (display)
     Display *display;
{
  char buf[256];

  sprintf (buf, "Connection lost to X server `%s'", DisplayString (display));
  x_connection_closed (display, buf);
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


/* Begin trapping X errors for display DPY.  Actually we trap X errors
   for all displays, but DPY should be the display you are actually
   operating on.

   After calling this function, X protocol errors no longer cause
   Emacs to exit; instead, they are recorded in x_cfc_error_message.

   Calling x_check_errors signals an Emacs error if an X error has
   occurred since the last call to x_catch_errors or x_check_errors.

   Calling x_uncatch_errors resumes the normal error handling.  */

void x_catch_errors (), x_check_errors (), x_uncatch_errors ();

void
x_catch_errors (dpy)
     Display *dpy;
{
  /* Make sure any errors from previous requests have been dealt with.  */
  XSync (dpy, False);

  /* Set up the error buffer.  */
  x_caught_error_message
    = (char*) xmalloc (X_CAUGHT_ERROR_MESSAGE_SIZE);
  x_caught_error_message[0] = '\0';

  /* Install our little error handler.  */
  XSetErrorHandler (x_error_catcher);
}

/* If any X protocol errors have arrived since the last call to
   x_catch_errors or x_check_errors, signal an Emacs error using
   sprintf (a buffer, FORMAT, the x error message text) as the text.  */

void
x_check_errors (dpy, format)
     Display *dpy;
     char *format;
{
  /* Make sure to catch any errors incurred so far.  */
  XSync (dpy, False);

  if (x_caught_error_message[0])
    {
      char buf[X_CAUGHT_ERROR_MESSAGE_SIZE + 56];

      sprintf (buf, format, x_caught_error_message);
      x_uncatch_errors (dpy);
      error (buf);
    }
}

/* Nonzero if we had any X protocol errors since we did x_catch_errors.  */

int
x_had_errors_p (dpy)
     Display *dpy;
{
  /* Make sure to catch any errors incurred so far.  */
  XSync (dpy, False);

  return x_caught_error_message[0] != 0;
}

/* Stop catching X protocol errors and let them make Emacs die.  */

void
x_uncatch_errors (dpy)
     Display *dpy;
{
  xfree (x_caught_error_message);
  x_caught_error_message = 0;
  XSetErrorHandler (x_error_quitter);
}

#if 0
static unsigned int x_wire_count;
x_trace_wire ()
{
  fprintf (stderr, "Lib call: %d\n", ++x_wire_count);
}
#endif /* ! 0 */


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
  char **font_names;

  /* Get a list of all the fonts that match this name.  Once we
     have a list of matching fonts, we compare them against the fonts
     we already have by comparing font ids.  */
  font_names = (char **) XListFonts (FRAME_X_DISPLAY (f), fontname,
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

      for (i = 0; i < FRAME_X_DISPLAY_INFO (f)->n_fonts; i++)
	for (j = 0; j < n_matching_fonts; j++)
	  if (!strcmp (FRAME_X_DISPLAY_INFO (f)->font_table[i].name, font_names[j])
	      || !strcmp (FRAME_X_DISPLAY_INFO (f)->font_table[i].full_name, font_names[j]))
	    {
	      already_loaded = i;
	      fontname = FRAME_X_DISPLAY_INFO (f)->font_table[i].full_name;
	      goto found_font;
	    }
    }
 found_font:

  /* If we have, just return it from the table.  */
  if (already_loaded >= 0)
    f->display.x->font = FRAME_X_DISPLAY_INFO (f)->font_table[already_loaded].font;
  /* Otherwise, load the font and add it to the table.  */
  else
    {
      int i;
      char *full_name;
      XFontStruct *font;
      int n_fonts;

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

      font = (XFontStruct *) XLoadQueryFont (FRAME_X_DISPLAY (f), fontname);
      if (! font)
	{
	  /* Free the information from XListFonts.  */
	  if (n_matching_fonts)
	    XFreeFontNames (font_names);
	  return Qnil;
	}

      /* Do we need to create the table?  */
      if (FRAME_X_DISPLAY_INFO (f)->font_table_size == 0)
	{
	  FRAME_X_DISPLAY_INFO (f)->font_table_size = 16;
	  FRAME_X_DISPLAY_INFO (f)->font_table
	    = (struct font_info *) xmalloc (FRAME_X_DISPLAY_INFO (f)->font_table_size
					    * sizeof (struct font_info));
	}
      /* Do we need to grow the table?  */
      else if (FRAME_X_DISPLAY_INFO (f)->n_fonts
	       >= FRAME_X_DISPLAY_INFO (f)->font_table_size)
	{
	  FRAME_X_DISPLAY_INFO (f)->font_table_size *= 2;
	  FRAME_X_DISPLAY_INFO (f)->font_table
	    = (struct font_info *) xrealloc (FRAME_X_DISPLAY_INFO (f)->font_table,
					     (FRAME_X_DISPLAY_INFO (f)->font_table_size
					      * sizeof (struct font_info)));
	}

      /* Try to get the full name of FONT.  Put it in full_name.  */
      full_name = 0;
      for (i = 0; i < font->n_properties; i++)
	{
	  char *atom
	    = XGetAtomName (FRAME_X_DISPLAY (f), font->properties[i].name);
	  if (!strcmp (atom, "FONT"))
	    {
	      char *name = XGetAtomName (FRAME_X_DISPLAY (f),
					 (Atom) (font->properties[i].card32));
	      char *p = name;
	      int dashes = 0;

	      /* Count the number of dashes in the "full name".
		 If it is too few, this isn't really the font's full name,
		 so don't use it.
		 In X11R4, the fonts did not come with their canonical names
		 stored in them.  */
	      while (*p)
		{
		  if (*p == '-')
		    dashes++;
		  p++;
		}

	      if (dashes >= 13)
		full_name = name;

	      break;
	    }

	  XFree (atom);
	}

      n_fonts = FRAME_X_DISPLAY_INFO (f)->n_fonts;
      FRAME_X_DISPLAY_INFO (f)->font_table[n_fonts].name = (char *) xmalloc (strlen (fontname) + 1);
      bcopy (fontname, FRAME_X_DISPLAY_INFO (f)->font_table[n_fonts].name, strlen (fontname) + 1);
      if (full_name != 0)
	FRAME_X_DISPLAY_INFO (f)->font_table[n_fonts].full_name = full_name;
      else
	FRAME_X_DISPLAY_INFO (f)->font_table[n_fonts].full_name = FRAME_X_DISPLAY_INFO (f)->font_table[n_fonts].name;
      f->display.x->font = FRAME_X_DISPLAY_INFO (f)->font_table[n_fonts].font = font;
      FRAME_X_DISPLAY_INFO (f)->n_fonts++;

      if (full_name)
	fontname = full_name;
    }

  /* Compute the scroll bar width in character columns.  */
  if (f->scroll_bar_pixel_width > 0)
    {
      int wid = FONT_WIDTH (f->display.x->font);
      f->scroll_bar_cols = (f->scroll_bar_pixel_width + wid-1) / wid;
    }
  else
    f->scroll_bar_cols = 2;

  /* Now make the frame display the given font.  */
  if (FRAME_X_WINDOW (f) != 0)
    {
      XSetFont (FRAME_X_DISPLAY (f), f->display.x->normal_gc,
		f->display.x->font->fid);
      XSetFont (FRAME_X_DISPLAY (f), f->display.x->reverse_gc,
		f->display.x->font->fid);
      XSetFont (FRAME_X_DISPLAY (f), f->display.x->cursor_gc,
		f->display.x->font->fid);

      frame_update_line_height (f);
      x_set_window_size (f, 0, f->width, f->height);
    }
  else
    /* If we are setting a new frame's font for the first time,
       there are no faces yet, so this font's height is the line height.  */
    f->display.x->line_height = FONT_HEIGHT (f->display.x->font);

  {
    Lisp_Object lispy_name;

    lispy_name = build_string (fontname);

    /* Free the information from XListFonts.  The data
       we actually retain comes from XLoadQueryFont.  */
    XFreeFontNames (font_names);

    return lispy_name;
  }
}

x_calc_absolute_position (f)
     struct frame *f;
{
  Window win, child;
  int win_x = 0, win_y = 0;
  int flags = f->display.x->size_hint_flags;

  /* Find the position of the outside upper-left corner of
     the inner window, with respect to the outer window.  */
  if (f->display.x->parent_desc != FRAME_X_DISPLAY_INFO (f)->root_window)
    {
      BLOCK_INPUT;
      XTranslateCoordinates (FRAME_X_DISPLAY (f),

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
  if (flags & XNegative)
    f->display.x->left_pos = (FRAME_X_DISPLAY_INFO (f)->width
			      - 2 * f->display.x->border_width - win_x
			      - PIXEL_WIDTH (f)
			      + f->display.x->left_pos);

  if (flags & YNegative)
    f->display.x->top_pos = (FRAME_X_DISPLAY_INFO (f)->height
			     - 2 * f->display.x->border_width - win_y
			     - PIXEL_HEIGHT (f)
			     + f->display.x->top_pos);
  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->display.x->size_hint_flags &= ~ (XNegative | YNegative);
}

/* CHANGE_GRAVITY is 1 when calling from Fset_frame_position,
   to really change the position, and 0 when calling from
   x_make_frame_visible (in that case, XOFF and YOFF are the current
   position values).  */

x_set_offset (f, xoff, yoff, change_gravity)
     struct frame *f;
     register int xoff, yoff;
     int change_gravity;
{
  if (change_gravity)
    {
      f->display.x->top_pos = yoff;
      f->display.x->left_pos = xoff;
      f->display.x->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->display.x->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->display.x->size_hint_flags |= YNegative;
      f->display.x->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, 0, 0);

#ifdef USE_X_TOOLKIT
  XMoveWindow (FRAME_X_DISPLAY (f), XtWindow (f->display.x->widget),
	       f->display.x->left_pos, f->display.x->top_pos);
#else /* not USE_X_TOOLKIT */
  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	       f->display.x->left_pos, f->display.x->top_pos);
#endif /* not USE_X_TOOLKIT */
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
  int mask;

#ifdef USE_X_TOOLKIT
  BLOCK_INPUT;
  {
    /* The x and y position of the widget is clobbered by the
       call to XtSetValues within EmacsFrameSetCharSize.
       This is a real kludge, but I don't understand Xt so I can't
       figure out a correct fix.  Can anyone else tell me? -- rms.  */
    int xpos = f->display.x->widget->core.x;
    int ypos = f->display.x->widget->core.y;
    EmacsFrameSetCharSize (f->display.x->edit_widget, cols, rows);
    f->display.x->widget->core.x = xpos;
    f->display.x->widget->core.y = ypos;
  }
  UNBLOCK_INPUT;

#else /* not USE_X_TOOLKIT */

  BLOCK_INPUT;

  check_frame_size (f, &rows, &cols);
  f->display.x->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->display.x->font)));
  pixelwidth = CHAR_TO_PIXEL_WIDTH (f, cols);
  pixelheight = CHAR_TO_PIXEL_HEIGHT (f, rows);

  f->display.x->win_gravity = NorthWestGravity;
  x_wm_set_size_hint (f, 0, 0);

  XSync (FRAME_X_DISPLAY (f), False);
  XResizeWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 pixelwidth, pixelheight);

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

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
#endif /* not USE_X_TOOLKIT */
}

/* Mouse warping, focus shifting, raising and lowering.  */

void
x_set_mouse_position (f, x, y)
     struct frame *f;
     int x, y;
{
  int pix_x, pix_y;

  pix_x = CHAR_TO_PIXEL_COL (f, x) + FONT_WIDTH  (f->display.x->font) / 2;
  pix_y = CHAR_TO_PIXEL_ROW (f, y) + f->display.x->line_height / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > PIXEL_WIDTH (f)) pix_x = PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > PIXEL_HEIGHT (f)) pix_y = PIXEL_HEIGHT (f);

  BLOCK_INPUT;

  XWarpPointer (FRAME_X_DISPLAY (f), None, FRAME_X_WINDOW (f),
		0, 0, 0, 0, pix_x, pix_y);
  UNBLOCK_INPUT;
}

/* Move the mouse to position pixel PIX_X, PIX_Y relative to frame F.  */

void
x_set_mouse_pixel_position (f, pix_x, pix_y)
     struct frame *f;
     int pix_x, pix_y;
{
  BLOCK_INPUT;

  XWarpPointer (FRAME_X_DISPLAY (f), None, FRAME_X_WINDOW (f),
		0, 0, 0, 0, pix_x, pix_y);
  UNBLOCK_INPUT;
}

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
  XSetInputFocus (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  RevertToPointerRoot, CurrentTime);
#endif /* ! 0 */
}

x_unfocus_frame (f)
     struct frame *f;
{
#if 0
  /* Look at the remarks in x_focus_on_frame.  */
  if (x_focus_frame == f)
    XSetInputFocus (FRAME_X_DISPLAY (f), PointerRoot,
		    RevertToPointerRoot, CurrentTime);
#endif /* ! 0 */
}

/* Raise frame F.  */

x_raise_frame (f)
     struct frame *f;
{
  if (f->async_visible)
    {
      BLOCK_INPUT;
#ifdef USE_X_TOOLKIT
      XRaiseWindow (FRAME_X_DISPLAY (f), XtWindow (f->display.x->widget));
#else /* not USE_X_TOOLKIT */
      XRaiseWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* not USE_X_TOOLKIT */
      XFlush (FRAME_X_DISPLAY (f));
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
#ifdef USE_X_TOOLKIT
      XLowerWindow (FRAME_X_DISPLAY (f), XtWindow (f->display.x->widget));
#else /* not USE_X_TOOLKIT */
      XLowerWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* not USE_X_TOOLKIT */
      XFlush (FRAME_X_DISPLAY (f));
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

/* Change from withdrawn state to mapped state,
   or deiconify. */

x_make_frame_visible (f)
     struct frame *f;
{
  int mask;
  Lisp_Object type;

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

  if (! FRAME_VISIBLE_P (f))
    {
#ifndef USE_X_TOOLKIT
      if (! FRAME_ICONIFIED_P (f))
	x_set_offset (f, f->display.x->left_pos, f->display.x->top_pos, 0);
#endif

      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, NormalState);
#ifdef USE_X_TOOLKIT
      /* This was XtPopup, but that did nothing for an iconified frame.  */
      XtMapWidget (f->display.x->widget);
#else /* not USE_X_TOOLKIT */
      XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* not USE_X_TOOLKIT */
#if 0 /* This seems to bring back scroll bars in the wrong places
	 if the window configuration has changed.  They seem
	 to come back ok without this.  */
      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
	XMapSubwindows (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif
    }

  XFlush (FRAME_X_DISPLAY (f));

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
	x_sync (f);
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

x_make_frame_invisible (f)
     struct frame *f;
{
  int mask;
  Window window;

#ifdef USE_X_TOOLKIT
  /* Use the frame's outermost window, not the one we normally draw on.  */
  window = XtWindow (f->display.x->widget);
#else /* not USE_X_TOOLKIT */
  window = FRAME_X_WINDOW (f);
#endif /* not USE_X_TOOLKIT */

  /* Don't keep the highlight on an invisible frame.  */
  if (x_highlight_frame == f)
    x_highlight_frame = 0;

#if 0/* This might add unreliability; I don't trust it -- rms.  */
  if (! f->async_visible && ! f->async_iconified)
    return;
#endif

  BLOCK_INPUT;

  /* Before unmapping the window, update the WM_SIZE_HINTS property to claim
     that the current position of the window is user-specified, rather than
     program-specified, so that when the window is mapped again, it will be
     placed at the same location, without forcing the user to position it
     by hand again (they have already done that once for this window.)  */
  x_wm_set_size_hint (f, 0, 1);

#ifdef HAVE_X11R4

  if (! XWithdrawWindow (FRAME_X_DISPLAY (f), window,
			 DefaultScreen (FRAME_X_DISPLAY (f))))
    {
      UNBLOCK_INPUT_RESIGNAL;
      error ("Can't notify window manager of window withdrawal");
    }
#else /* ! defined (HAVE_X11R4) */

  /*  Tell the window manager what we're going to do.  */
  if (! EQ (Vx_no_window_manager, Qt))
    {
      XEvent unmap;

      unmap.xunmap.type = UnmapNotify;
      unmap.xunmap.window = window;
      unmap.xunmap.event = DefaultRootWindow (FRAME_X_DISPLAY (f));
      unmap.xunmap.from_configure = False;
      if (! XSendEvent (FRAME_X_DISPLAY (f),
			DefaultRootWindow (FRAME_X_DISPLAY (f)),
			False,
			SubstructureRedirectMask|SubstructureNotifyMask,
			&unmap))
	{
	  UNBLOCK_INPUT_RESIGNAL;
	  error ("Can't notify window manager of withdrawal");
	}
    }

  /* Unmap the window ourselves.  Cheeky!  */
  XUnmapWindow (FRAME_X_DISPLAY (f), window);
#endif /* ! defined (HAVE_X11R4) */

  /* We can't distinguish this from iconification
     just by the event that we get from the server.
     So we can't win using the usual strategy of letting
     FRAME_SAMPLE_VISIBILITY set this.  So do it by hand,
     and synchronize with the server to make sure we agree.  */
  f->visible = 0;
  FRAME_ICONIFIED_P (f) = 0;
  f->async_visible = 0;
  f->async_iconified = 0;

  x_sync (f);

  UNBLOCK_INPUT;
}

/* Change window state from mapped to iconified. */

x_iconify_frame (f)
     struct frame *f;
{
  int mask;
  int result;
  Lisp_Object type;

  /* Don't keep the highlight on an invisible frame.  */
  if (x_highlight_frame == f)
    x_highlight_frame = 0;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

#ifdef USE_X_TOOLKIT

  if (! FRAME_VISIBLE_P (f))
    {
      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, IconicState);
      /* This was XtPopup, but that did nothing for an iconified frame.  */
      XtMapWidget (f->display.x->widget);
      UNBLOCK_INPUT;
      return;
    }

  result = XIconifyWindow (FRAME_X_DISPLAY (f),
			   XtWindow (f->display.x->widget),
			   DefaultScreen (FRAME_X_DISPLAY (f)));
  UNBLOCK_INPUT;

  if (!result)
    error ("Can't notify window manager of iconification");

  f->async_iconified = 1;

  BLOCK_INPUT;
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
#else /* not USE_X_TOOLKIT */

  /* Make sure the X server knows where the window should be positioned,
     in case the user deiconifies with the window manager.  */
  if (! FRAME_VISIBLE_P (f) && !FRAME_ICONIFIED_P (f))
    x_set_offset (f, f->display.x->left_pos, f->display.x->top_pos, 0);

  /* Since we don't know which revision of X we're running, we'll use both
     the X11R3 and X11R4 techniques.  I don't know if this is a good idea.  */

  /* X11R4: send a ClientMessage to the window manager using the
     WM_CHANGE_STATE type.  */
  {
    XEvent message;

    message.xclient.window = FRAME_X_WINDOW (f);
    message.xclient.type = ClientMessage;
    message.xclient.message_type = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_change_state;
    message.xclient.format = 32;
    message.xclient.data.l[0] = IconicState;

    if (! XSendEvent (FRAME_X_DISPLAY (f),
		      DefaultRootWindow (FRAME_X_DISPLAY (f)),
		      False,
		      SubstructureRedirectMask | SubstructureNotifyMask,
		      &message))
      {
	UNBLOCK_INPUT_RESIGNAL;
	error ("Can't notify window manager of iconification");
      }
  }

  /* X11R3: set the initial_state field of the window manager hints to
     IconicState.  */
  x_wm_set_window_state (f, IconicState);

  if (!FRAME_VISIBLE_P (f))
    {
      /* If the frame was withdrawn, before, we must map it.  */
      XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
    }

  f->async_iconified = 1;

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
#endif /* not USE_X_TOOLKIT */
}

/* Destroy the X window of frame F.  */

x_destroy_window (f)
     struct frame *f;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  BLOCK_INPUT;

  if (f->display.x->icon_desc != 0)
    XDestroyWindow (FRAME_X_DISPLAY (f), f->display.x->icon_desc);
  XDestroyWindow (FRAME_X_DISPLAY (f), f->display.x->window_desc);
#ifdef USE_X_TOOLKIT
  XtDestroyWidget (f->display.x->widget);
  free_frame_menubar (f);
#endif /* USE_X_TOOLKIT */

  free_frame_faces (f);
  XFlush (FRAME_X_DISPLAY (f));

  xfree (f->display.x);
  f->display.x = 0;
  if (f == x_focus_frame)
    x_focus_frame = 0;
  if (f == x_highlight_frame)
    x_highlight_frame = 0;

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
  XSizeHints size_hints;

#ifdef USE_X_TOOLKIT
  Arg al[2];
  int ac = 0;
  Dimension widget_width, widget_height;
  Window window = XtWindow (f->display.x->widget);
#else /* not USE_X_TOOLKIT */
  Window window = FRAME_X_WINDOW (f);
#endif /* not USE_X_TOOLKIT */

  /* Setting PMaxSize caused various problems.  */
  size_hints.flags = PResizeInc | PMinSize /* | PMaxSize */;

  flexlines = f->height;

  size_hints.x = f->display.x->left_pos;
  size_hints.y = f->display.x->top_pos;

#ifdef USE_X_TOOLKIT
  XtSetArg (al[ac], XtNwidth, &widget_width); ac++;
  XtSetArg (al[ac], XtNheight, &widget_height); ac++;
  XtGetValues (f->display.x->column_widget, al, ac);
  size_hints.height = widget_height;
  size_hints.width = widget_width;
#else /* not USE_X_TOOLKIT */
  size_hints.height = PIXEL_HEIGHT (f);
  size_hints.width = PIXEL_WIDTH (f);
#endif /* not USE_X_TOOLKIT */

  size_hints.width_inc = FONT_WIDTH (f->display.x->font);
  size_hints.height_inc = f->display.x->line_height;
  size_hints.max_width
    = FRAME_X_DISPLAY_INFO (f)->width - CHAR_TO_PIXEL_WIDTH (f, 0);
  size_hints.max_height
    = FRAME_X_DISPLAY_INFO (f)->height - CHAR_TO_PIXEL_HEIGHT (f, 0);

  {
    int base_width, base_height;
    int min_rows = 0, min_cols = 0;

    base_width = CHAR_TO_PIXEL_WIDTH (f, 0);
    base_height = CHAR_TO_PIXEL_HEIGHT (f, 0);

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

  if (flags)
    size_hints.flags |= flags;
  else
    {
      XSizeHints hints;		/* Sometimes I hate X Windows... */
      long supplied_return;
      int value;

#ifdef HAVE_X11R4
      value = XGetWMNormalHints (FRAME_X_DISPLAY (f), window, &hints,
				 &supplied_return);
#else
      value = XGetNormalHints (FRAME_X_DISPLAY (f), window, &hints);
#endif

      if (value == 0)
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

#ifdef PWinGravity
  size_hints.win_gravity = f->display.x->win_gravity;
  size_hints.flags |= PWinGravity;

  if (user_position)
    {
      size_hints.flags &= ~ PPosition;
      size_hints.flags |= USPosition;
    }
#endif /* PWinGravity */

#ifdef HAVE_X11R4
  XSetWMNormalHints (FRAME_X_DISPLAY (f), window, &size_hints);
#else
  XSetNormalHints (FRAME_X_DISPLAY (f), window, &size_hints);
#endif
}

/* Used for IconicState or NormalState */
x_wm_set_window_state (f, state)
     struct frame *f;
     int state;
{
#ifdef USE_X_TOOLKIT
  Arg al[1];

  XtSetArg (al[0], XtNinitialState, state);
  XtSetValues (f->display.x->widget, al, 1);
#else /* not USE_X_TOOLKIT */
  Window window = FRAME_X_WINDOW (f);

  f->display.x->wm_hints.flags |= StateHint;
  f->display.x->wm_hints.initial_state = state;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->display.x->wm_hints);
#endif /* not USE_X_TOOLKIT */
}

x_wm_set_icon_pixmap (f, pixmap_id)
     struct frame *f;
     int pixmap_id;
{
#ifdef USE_X_TOOLKIT
  Window window = XtWindow (f->display.x->widget);
#else
  Window window = FRAME_X_WINDOW (f);
#endif

  if (pixmap_id > 0)
    {
      Pixmap icon_pixmap = x_bitmap_pixmap (f, pixmap_id);
      f->display.x->wm_hints.icon_pixmap = icon_pixmap;
      f->display.x->wm_hints.flags |= IconPixmapHint;
    }
  else
    {
      f->display.x->wm_hints.icon_pixmap = None;
      f->display.x->wm_hints.flags &= ~IconPixmapHint;
    }

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->display.x->wm_hints);
}

x_wm_set_icon_position (f, icon_x, icon_y)
     struct frame *f;
     int icon_x, icon_y;
{
#ifdef USE_X_TOOLKIT
  Window window = XtWindow (f->display.x->widget);
#else
  Window window = FRAME_X_WINDOW (f);
#endif

  f->display.x->wm_hints.flags |= IconPositionHint;
  f->display.x->wm_hints.icon_x = icon_x;
  f->display.x->wm_hints.icon_y = icon_y;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->display.x->wm_hints);
}


/* Initialization.  */

#ifdef USE_X_TOOLKIT
static XrmOptionDescRec emacs_options[] = {
  {"-geometry",	".geometry", XrmoptionSepArg, NULL},
  {"-iconic",	".iconic", XrmoptionNoArg, (XtPointer) "yes"},

  {"-internal-border-width", "*EmacsScreen.internalBorderWidth",
     XrmoptionSepArg, NULL},
  {"-ib",	"*EmacsScreen.internalBorderWidth", XrmoptionSepArg, NULL},

  {"-T",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-wn",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-title",	"*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-iconname",	"*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-in",	"*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-mc",	"*pointerColor", XrmoptionSepArg, (XtPointer) NULL},
  {"-cr",	"*cursorColor", XrmoptionSepArg, (XtPointer) NULL}
};
#endif /* USE_X_TOOLKIT */

static int x_initialized;

struct x_display_info *
x_term_init (display_name, xrm_option, resource_name)
     Lisp_Object display_name;
     char *xrm_option;
     char *resource_name;
{
  Lisp_Object frame;
  char *defaultvalue;
  int connection;
  Display *dpy;
  struct x_display_info *dpyinfo;
  XrmDatabase xrdb;

  if (!x_initialized)
    {
      x_initialize ();
      x_initialized = 1;
    }

#ifdef USE_X_TOOLKIT
#ifdef HAVE_X11R5
  XtSetLanguageProc (NULL, NULL, NULL);
#endif

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
    dpy = XtOpenDisplay (Xt_app_con, XSTRING (display_name)->data,
			 resource_name, EMACS_CLASS,
			 emacs_options, XtNumber (emacs_options),
			 &argc, argv);
  }

#else /* not USE_X_TOOLKIT */
#ifdef HAVE_X11R5
  XSetLocaleModifiers ("");
#endif
  dpy = XOpenDisplay (XSTRING (display_name)->data);
#endif /* not USE_X_TOOLKIT */

  /* Detect failure.  */
  if (dpy == 0)
    return 0;

  /* We have definitely succeeded.  Record the new connection.  */

  dpyinfo = (struct x_display_info *) xmalloc (sizeof (struct x_display_info));

  /* Put this display on the chain.  */
  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;

  /* Put it on x_display_name_list as well, to keep them parallel.  */ 
  x_display_name_list = Fcons (Fcons (display_name, Qnil),
			       x_display_name_list);
  dpyinfo->name_list_element = XCONS (x_display_name_list)->car;

  dpyinfo->display = dpy;

#if 0
  XSetAfterFunction (x_current_display, x_trace_wire);
#endif /* ! 0 */

  dpyinfo->x_id_name
    = (char *) xmalloc (XSTRING (Vinvocation_name)->size
			+ XSTRING (Vsystem_name)->size
			+ 2);
  sprintf (dpyinfo->x_id_name, "%s@%s",
	   XSTRING (Vinvocation_name)->data, XSTRING (Vsystem_name)->data);

  /* Figure out which modifier bits mean what.  */
  x_find_modifier_meanings (dpyinfo);

  /* Get the scroll bar cursor.  */
  dpyinfo->vertical_scroll_bar_cursor
    = XCreateFontCursor (dpyinfo->display, XC_sb_v_double_arrow);

  xrdb = x_load_resources (dpyinfo->display, xrm_option,
			   resource_name, EMACS_CLASS);
#ifdef HAVE_XRMSETDATABASE
  XrmSetDatabase (dpyinfo->display, xrdb);
#else
  dpyinfo->display->db = xrdb;
#endif
  /* Put thr rdb where we can find it in a way that works on
     all versions.  */
  dpyinfo->xrdb = xrdb;

  dpyinfo->screen = ScreenOfDisplay (dpyinfo->display,
				     DefaultScreen (dpyinfo->display));
  dpyinfo->visual = select_visual (dpyinfo->display, dpyinfo->screen,
				   &dpyinfo->n_planes);
  dpyinfo->height = HeightOfScreen (dpyinfo->screen);
  dpyinfo->width = WidthOfScreen (dpyinfo->screen);
  dpyinfo->root_window = RootWindowOfScreen (dpyinfo->screen);
  dpyinfo->grabbed = 0;
  dpyinfo->reference_count = 0;
  dpyinfo->icon_bitmap_id = -1;
  dpyinfo->n_fonts = 0;
  dpyinfo->font_table_size = 0;
  dpyinfo->bitmaps = 0;
  dpyinfo->bitmaps_size = 0;
  dpyinfo->bitmaps_last = 0;
  dpyinfo->scratch_cursor_gc = 0;
  dpyinfo->mouse_face_mouse_frame = 0;
  dpyinfo->mouse_face_deferred_gc = 0;
  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_face_id = 0;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_mouse_x = dpyinfo->mouse_face_mouse_y = 0;
  dpyinfo->mouse_face_defer = 0;

  dpyinfo->Xatom_wm_protocols
    = XInternAtom (dpyinfo->display, "WM_PROTOCOLS", False);
  dpyinfo->Xatom_wm_take_focus
    = XInternAtom (dpyinfo->display, "WM_TAKE_FOCUS", False);
  dpyinfo->Xatom_wm_save_yourself
    = XInternAtom (dpyinfo->display, "WM_SAVE_YOURSELF", False);
  dpyinfo->Xatom_wm_delete_window
    = XInternAtom (dpyinfo->display, "WM_DELETE_WINDOW", False);
  dpyinfo->Xatom_wm_change_state
    = XInternAtom (dpyinfo->display, "WM_CHANGE_STATE", False);
  dpyinfo->Xatom_wm_configure_denied
    = XInternAtom (dpyinfo->display, "WM_CONFIGURE_DENIED", False);
  dpyinfo->Xatom_wm_window_moved
    = XInternAtom (dpyinfo->display, "WM_MOVED", False);
  dpyinfo->Xatom_editres
    = XInternAtom (dpyinfo->display, "Editres", False);
  dpyinfo->Xatom_CLIPBOARD
    = XInternAtom (dpyinfo->display, "CLIPBOARD", False);
  dpyinfo->Xatom_TIMESTAMP
    = XInternAtom (dpyinfo->display, "TIMESTAMP", False);
  dpyinfo->Xatom_TEXT
    = XInternAtom (dpyinfo->display, "TEXT", False);
  dpyinfo->Xatom_DELETE
    = XInternAtom (dpyinfo->display, "DELETE", False);
  dpyinfo->Xatom_MULTIPLE
    = XInternAtom (dpyinfo->display, "MULTIPLE", False);
  dpyinfo->Xatom_INCR
    = XInternAtom (dpyinfo->display, "INCR", False);
  dpyinfo->Xatom_EMACS_TMP
    = XInternAtom (dpyinfo->display, "_EMACS_TMP_", False);
  dpyinfo->Xatom_TARGETS
    = XInternAtom (dpyinfo->display, "TARGETS", False);
  dpyinfo->Xatom_NULL
    = XInternAtom (dpyinfo->display, "NULL", False);
  dpyinfo->Xatom_ATOM_PAIR
    = XInternAtom (dpyinfo->display, "ATOM_PAIR", False);

  connection = ConnectionNumber (dpyinfo->display);
  dpyinfo->connection = connection;

#ifdef subprocesses
  /* This is only needed for distinguishing keyboard and process input.  */
  if (connection != 0)
    add_keyboard_wait_descriptor (connection);
#endif

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
  init_sigio (connection);
#endif /* ! defined (SIGIO) */

  return dpyinfo;
}

/* Get rid of display DPYINFO, assuming all frames are already gone,
   and without sending any more commands to the X server.  */

void
x_delete_display (dpyinfo)
     struct x_display_info *dpyinfo;
{
  delete_keyboard_wait_descriptor (dpyinfo->connection);

  /* Discard this display from x_display_name_list and x_display_list.
     We can't use Fdelq because that can quit.  */
  if (! NILP (x_display_name_list)
      && EQ (XCONS (x_display_name_list)->car, dpyinfo->name_list_element))
    x_display_name_list = XCONS (x_display_name_list)->cdr;
  else
    {
      Lisp_Object tail;

      tail = x_display_name_list;
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

  if (x_display_list == dpyinfo)
    x_display_list = dpyinfo->next;
  else
    {
      struct x_display_info *tail;

      for (tail = x_display_list; tail; tail = tail->next)
	if (tail->next == dpyinfo)
	  tail->next = tail->next->next;
    }

#ifndef USE_X_TOOLKIT
  /* I'm told Xt does this itself.  */
  XrmDestroyDatabase (dpyinfo->xrdb);
#endif
  free (dpyinfo->font_table);
  free (dpyinfo->x_id_name);
  free (dpyinfo);
}

/* Set up use of X before we make the first connection.  */

x_initialize ()
{
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
  frame_up_to_date_hook = XTframe_up_to_date;
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

  x_noop_count = 0;

  x_focus_frame = x_highlight_frame = 0;

  /* Try to use interrupt input; if we can't, then start polling.  */
  Fset_input_mode (Qt, Qnil, Qt, Qnil);

#ifdef USE_X_TOOLKIT
  XtToolkitInitialize ();
  Xt_app_con = XtCreateApplicationContext ();
#endif

  /* Note that there is no real way portable across R3/R4 to get the
     original error handler.  */
  XSetErrorHandler (x_error_quitter);
  XSetIOErrorHandler (x_io_error_quitter);

  /* Disable Window Change signals;  they are handled by X events. */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_DFL);
#endif /* ! defined (SIGWINCH) */

  signal (SIGPIPE, x_connection_closed);
}

void
syms_of_xterm ()
{
  staticpro (&x_display_name_list);
  x_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;
}
#endif /* ! defined (HAVE_X_WINDOWS) */
