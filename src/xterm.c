/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1989, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
                 2002, 2003, 2004, 2005, 2006, 2007, 2008
                 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* New display code by Gerd Moellmann <gerd@gnu.org>.  */
/* Xt features made by Fred Pierresteguy.  */

#include <config.h>

/* On 4.3 these lose if they come after xterm.h.  */
/* Putting these at the beginning seems to be standard for other .c files.  */
#include <signal.h>

#include <stdio.h>

#ifdef HAVE_X_WINDOWS

#include "lisp.h"
#include "blockinput.h"

/* Need syssignal.h for various externs and definitions that may be required
   by some configurations for calls to signal later in this source file.  */
#include "syssignal.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"
#include <X11/cursorfont.h>

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif /* makedev */

#ifdef BSD_SYSTEM
#include <sys/ioctl.h>
#endif /* ! defined (BSD_SYSTEM) */

#include "systime.h"

#ifndef INCLUDED_FCNTL
#include <fcntl.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>
/* Caused redefinition of DBL_DIG on Netbsd; seems not to be needed.  */
/* #include <sys/param.h>  */

#include "charset.h"
#include "character.h"
#include "coding.h"
#include "ccl.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "emacs-icon.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"
#include "process.h"
#include "atimer.h"
#include "keymap.h"

#ifdef USE_X_TOOLKIT
#include <X11/Shell.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef USE_GTK
#include "gtkutil.h"
#endif

#ifdef USE_FONT_BACKEND
#include "font.h"
#endif	/* USE_FONT_BACKEND */

#ifdef USE_LUCID
extern int xlwmenu_window_p P_ ((Widget w, Window window));
extern void xlwmenu_redisplay P_ ((Widget));
#endif

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)

extern void free_frame_menubar P_ ((struct frame *));
extern struct frame *x_menubar_window_to_frame P_ ((struct x_display_info *,
						    int));
#endif

#ifdef USE_X_TOOLKIT
#if (XtSpecificationRelease >= 5) && !defined(NO_EDITRES)
#define HACK_EDITRES
extern void _XEditResCheckMessages ();
#endif /* not NO_EDITRES */

/* Include toolkit specific headers for the scroll bar widget.  */

#ifdef USE_TOOLKIT_SCROLL_BARS
#if defined USE_MOTIF
#include <Xm/Xm.h>		/* for LESSTIF_VERSION */
#include <Xm/ScrollBar.h>
#else /* !USE_MOTIF i.e. use Xaw */

#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Simple.h>
#include <X11/Xaw3d/Scrollbar.h>
#include <X11/Xaw3d/ThreeD.h>
#else /* !HAVE_XAW3D */
#include <X11/Xaw/Simple.h>
#include <X11/Xaw/Scrollbar.h>
#endif /* !HAVE_XAW3D */
#ifndef XtNpickTop
#define XtNpickTop "pickTop"
#endif /* !XtNpickTop */
#endif /* !USE_MOTIF */
#endif /* USE_TOOLKIT_SCROLL_BARS */

#endif /* USE_X_TOOLKIT */

#if ! defined (USE_X_TOOLKIT) && ! defined (USE_GTK)
#define x_any_window_to_frame x_window_to_frame
#define x_top_window_to_frame x_window_to_frame
#endif

#ifdef USE_X_TOOLKIT
#include "widget.h"
#ifndef XtNinitialState
#define XtNinitialState "initialState"
#endif
#endif

/* Default to using XIM if available.  */
#ifdef USE_XIM
int use_xim = 1;
#else
int use_xim = 0;  /* configure --without-xim */
#endif



/* Non-nil means Emacs uses toolkit scroll bars.  */

Lisp_Object Vx_toolkit_scroll_bars;

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static int any_help_event_p;

/* Last window where we saw the mouse.  Used by mouse-autoselect-window.  */
static Lisp_Object last_window;

/* Non-zero means make use of UNDERLINE_POSITION font properties.  */

int x_use_underline_position_properties;

/* Non-zero means to draw the underline at the same place as the descent line.  */

int x_underline_at_descent_line;

/* This is a chain of structures for all the X displays currently in
   use.  */

struct x_display_info *x_display_list;

/* This is a list of cons cells, each of the form (NAME
   . FONT-LIST-CACHE), one for each element of x_display_list and in
   the same order.  NAME is the name of the frame.  FONT-LIST-CACHE
   records previous values returned by x-list-fonts.  */

Lisp_Object x_display_name_list;

/* Frame being updated by update_frame.  This is declared in term.c.
   This is set by update_begin and looked at by all the XT functions.
   It is zero while not inside an update.  In that case, the XT
   functions assume that `selected_frame' is the frame to apply to.  */

extern struct frame *updating_frame;

/* This is a frame waiting to be auto-raised, within XTread_socket.  */

struct frame *pending_autoraise_frame;

#ifdef USE_X_TOOLKIT
/* The application context for Xt use.  */
XtAppContext Xt_app_con;
static String Xt_default_resources[] = {0};
#endif /* USE_X_TOOLKIT */

/* Non-zero means user is interacting with a toolkit scroll bar.  */

static int toolkit_scroll_bar_interaction;

/* Non-zero means to not move point as a result of clicking on a
   frame to focus it (when focus-follows-mouse is nil).  */

int x_mouse_click_focus_ignore_position;

/* Non-zero timeout value means ignore next mouse click if it arrives
   before that timeout elapses (i.e. as part of the same sequence of
   events resulting from clicking on a frame to select it).  */

static unsigned long ignore_next_mouse_click_timeout;

/* Mouse movement.

   Formerly, we used PointerMotionHintMask (in standard_event_mask)
   so that we would have to call XQueryPointer after each MotionNotify
   event to ask for another such event.  However, this made mouse tracking
   slow, and there was a bug that made it eventually stop.

   Simply asking for MotionNotify all the time seems to work better.

   In order to avoid asking for motion events and then throwing most
   of them away or busy-polling the server for mouse positions, we ask
   the server for pointer motion hints.  This means that we get only
   one event per group of mouse movements.  "Groups" are delimited by
   other kinds of events (focus changes and button clicks, for
   example), or by XQueryPointer calls; when one of these happens, we
   get another MotionNotify event the next time the mouse moves.  This
   is at least as efficient as getting motion events when mouse
   tracking is on, and I suspect only negligibly worse when tracking
   is off.  */

/* Where the mouse was last time we reported a mouse event.  */

static XRectangle last_mouse_glyph;
static FRAME_PTR last_mouse_glyph_frame;
static Lisp_Object last_mouse_press_frame;

/* The scroll bar in which the last X motion event occurred.

   If the last X motion event occurred in a scroll bar, we set this so
   XTmouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last X motion event didn't occur in a scroll bar, we set
   this to Qnil, to tell XTmouse_position to return an ordinary motion
   event.  */

static Lisp_Object last_mouse_scroll_bar;

/* This is a hack.  We would really prefer that XTmouse_position would
   return the time associated with the position it returns, but there
   doesn't seem to be any way to wrest the time-stamp from the server
   along with the position query.  So, we just keep track of the time
   of the last movement we received, and return that in hopes that
   it's somewhat accurate.  */

static Time last_mouse_movement_time;

/* Time for last user interaction as returned in X events.  */

static Time last_user_time;

/* Incremented by XTread_socket whenever it really tries to read
   events.  */

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

/* Tells if a window manager is present or not.  */

extern Lisp_Object Vx_no_window_manager;

extern Lisp_Object Qeql;

extern int errno;

/* A mask of extra modifier bits to put into every keyboard char.  */

extern EMACS_INT extra_keyboard_modifiers;

/* The keysyms to use for the various modifiers.  */

Lisp_Object Vx_alt_keysym, Vx_hyper_keysym, Vx_meta_keysym, Vx_super_keysym;
Lisp_Object Vx_keysym_table;
static Lisp_Object Qalt, Qhyper, Qmeta, Qsuper, Qmodifier_value;

static Lisp_Object Qvendor_specific_keysyms;
static Lisp_Object Qlatin_1;

/* Used in x_flush.  */

extern Lisp_Object Vinhibit_redisplay;

extern XrmDatabase x_load_resources P_ ((Display *, char *, char *, char *));
extern int x_bitmap_mask P_ ((FRAME_PTR, int));

static int x_alloc_nearest_color_1 P_ ((Display *, Colormap, XColor *));
static void x_set_window_size_1 P_ ((struct frame *, int, int, int));
static const XColor *x_color_cells P_ ((Display *, int *));
static void x_update_window_end P_ ((struct window *, int, int));

static int x_io_error_quitter P_ ((Display *));
static struct terminal *x_create_terminal P_ ((struct x_display_info *));
void x_delete_terminal P_ ((struct terminal *));
static void x_font_min_bounds P_ ((XFontStruct *, int *, int *));
static int x_compute_min_glyph_bounds P_ ((struct frame *));
static void x_update_end P_ ((struct frame *));
static void XTframe_up_to_date P_ ((struct frame *));
static void XTset_terminal_modes P_ ((struct terminal *));
static void XTreset_terminal_modes P_ ((struct terminal *));
static void x_clear_frame P_ ((struct frame *));
static void frame_highlight P_ ((struct frame *));
static void frame_unhighlight P_ ((struct frame *));
static void x_new_focus_frame P_ ((struct x_display_info *, struct frame *));
static void  x_focus_changed P_ ((int, int, struct x_display_info *,
				  struct frame *, struct input_event *));
static void x_detect_focus_change P_ ((struct x_display_info *,
                                       XEvent *, struct input_event *));
static void XTframe_rehighlight P_ ((struct frame *));
static void x_frame_rehighlight P_ ((struct x_display_info *));
static void x_draw_hollow_cursor P_ ((struct window *, struct glyph_row *));
static void x_draw_bar_cursor P_ ((struct window *, struct glyph_row *, int,
				   enum text_cursor_kinds));

static void x_clip_to_row P_ ((struct window *, struct glyph_row *, int, GC));
static void x_flush P_ ((struct frame *f));
static void x_update_begin P_ ((struct frame *));
static void x_update_window_begin P_ ((struct window *));
static void x_after_update_window_line P_ ((struct glyph_row *));
static struct scroll_bar *x_window_to_scroll_bar P_ ((Display *, Window));
static void x_scroll_bar_report_motion P_ ((struct frame **, Lisp_Object *,
					    enum scroll_bar_part *,
					    Lisp_Object *, Lisp_Object *,
					    unsigned long *));
static void x_check_fullscreen P_ ((struct frame *));
static void x_check_expected_move P_ ((struct frame *, int, int));
static void x_sync_with_move P_ ((struct frame *, int, int, int));
static int handle_one_xevent P_ ((struct x_display_info *, XEvent *,
				  int *, struct input_event *));
/* Don't declare this NO_RETURN because we want no
   interference with debugging failing X calls.  */
static SIGTYPE x_connection_closed P_ ((Display *, char *));


/* Flush display of frame F, or of all frames if F is null.  */

static void
x_flush (f)
     struct frame *f;
{
  /* Don't call XFlush when it is not safe to redisplay; the X
     connection may be broken.  */
  if (!NILP (Vinhibit_redisplay))
    return;

  BLOCK_INPUT;
  if (f == NULL)
    {
      Lisp_Object rest, frame;
      FOR_EACH_FRAME (rest, frame)
        if (FRAME_X_P (XFRAME (frame)))
          x_flush (XFRAME (frame));
    }
  else if (FRAME_X_P (f))
    XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
}


/* Remove calls to XFlush by defining XFlush to an empty replacement.
   Calls to XFlush should be unnecessary because the X output buffer
   is flushed automatically as needed by calls to XPending,
   XNextEvent, or XWindowEvent according to the XFlush man page.
   XTread_socket calls XPending.  Removing XFlush improves
   performance.  */

#define XFlush(DISPLAY)	(void) 0


/***********************************************************************
			      Debugging
 ***********************************************************************/

#if 0

/* This is a function useful for recording debugging information about
   the sequence of occurrences in this file.  */

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



/***********************************************************************
		    Starting and ending an update
 ***********************************************************************/

/* Start an update of frame F.  This function is installed as a hook
   for update_begin, i.e. it is called when update_begin is called.
   This function is called prior to calls to x_update_window_begin for
   each window being updated.  Currently, there is nothing to do here
   because all interesting stuff is done on a window basis.  */

static void
x_update_begin (f)
     struct frame *f;
{
  /* Nothing to do.  */
}


/* Start update of window W.  Set the global variable updated_window
   to the window being updated and set output_cursor to the cursor
   position of W.  */

static void
x_update_window_begin (w)
     struct window *w;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct x_display_info *display_info = FRAME_X_DISPLAY_INFO (f);

  updated_window = w;
  set_output_cursor (&w->cursor);

  BLOCK_INPUT;

  if (f == display_info->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      display_info->mouse_face_defer = 1;

      /* If F needs to be redrawn, simply forget about any prior mouse
	 highlighting.  */
      if (FRAME_GARBAGED_P (f))
	display_info->mouse_face_window = Qnil;

#if 0 /* Rows in a current matrix containing glyphs in mouse-face have
	 their mouse_face_p flag set, which means that they are always
	 unequal to rows in a desired matrix which never have that
	 flag set.  So, rows containing mouse-face glyphs are never
	 scrolled, and we don't have to switch the mouse highlight off
	 here to prevent it from being scrolled.  */

      /* Can we tell that this update does not affect the window
	 where the mouse highlight is?  If so, no need to turn off.
	 Likewise, don't do anything if the frame is garbaged;
	 in that case, the frame's current matrix that we would use
	 is all wrong, and we will redisplay that line anyway.  */
      if (!NILP (display_info->mouse_face_window)
	  && w == XWINDOW (display_info->mouse_face_window))
	{
	  int i;

	  for (i = 0; i < w->desired_matrix->nrows; ++i)
	    if (MATRIX_ROW_ENABLED_P (w->desired_matrix, i))
	      break;

	  if (i < w->desired_matrix->nrows)
	    clear_mouse_face (display_info);
	}
#endif /* 0 */
    }

  UNBLOCK_INPUT;
}


/* Draw a vertical window border from (x,y0) to (x,y1)  */

static void
x_draw_vertical_window_border (w, x, y0, y1)
     struct window *w;
     int x, y0, y1;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;

  face = FACE_FROM_ID (f, VERTICAL_BORDER_FACE_ID);
  if (face)
    XSetForeground (FRAME_X_DISPLAY (f), f->output_data.x->normal_gc,
		    face->foreground);

  XDrawLine (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	     f->output_data.x->normal_gc, x, y0, x, y1);
}

/* End update of window W (which is equal to updated_window).

   Draw vertical borders between horizontally adjacent windows, and
   display W's cursor if CURSOR_ON_P is non-zero.

   MOUSE_FACE_OVERWRITTEN_P non-zero means that some row containing
   glyphs in mouse-face were overwritten.  In that case we have to
   make sure that the mouse-highlight is properly redrawn.

   W may be a menu bar pseudo-window in case we don't have X toolkit
   support.  Such windows don't have a cursor, so don't display it
   here.  */

static void
x_update_window_end (w, cursor_on_p, mouse_face_overwritten_p)
     struct window *w;
     int cursor_on_p, mouse_face_overwritten_p;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (XFRAME (w->frame));

  if (!w->pseudo_window_p)
    {
      BLOCK_INPUT;

      if (cursor_on_p)
	display_and_set_cursor (w, 1, output_cursor.hpos,
				output_cursor.vpos,
				output_cursor.x, output_cursor.y);

      if (draw_window_fringes (w, 1))
	x_draw_vertical_border (w);

      UNBLOCK_INPUT;
    }

  /* If a row with mouse-face was overwritten, arrange for
     XTframe_up_to_date to redisplay the mouse highlight.  */
  if (mouse_face_overwritten_p)
    {
      dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }

  updated_window = NULL;
}


/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
x_update_end (f)
     struct frame *f;
{
  /* Mouse highlight may be displayed again.  */
  FRAME_X_DISPLAY_INFO (f)->mouse_face_defer = 0;

#ifndef XFlush
  BLOCK_INPUT;
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
#endif
}


/* This function is called from various places in xdisp.c whenever a
   complete update has been performed.  The global variable
   updated_window is not available here.  */

static void
XTframe_up_to_date (f)
     struct frame *f;
{
  if (FRAME_X_P (f))
    {
      struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

      if (dpyinfo->mouse_face_deferred_gc
	  || f == dpyinfo->mouse_face_mouse_frame)
	{
	  BLOCK_INPUT;
	  if (dpyinfo->mouse_face_mouse_frame)
	    note_mouse_highlight (dpyinfo->mouse_face_mouse_frame,
				  dpyinfo->mouse_face_mouse_x,
				  dpyinfo->mouse_face_mouse_y);
	  dpyinfo->mouse_face_deferred_gc = 0;
	  UNBLOCK_INPUT;
	}
    }
}


/* Draw truncation mark bitmaps, continuation mark bitmaps, overlay
   arrow bitmaps, or clear the fringes if no bitmaps are required
   before DESIRED_ROW is made current.  The window being updated is
   found in updated_window.  This function It is called from
   update_window_line only if it is known that there are differences
   between bitmaps to be drawn between current row and DESIRED_ROW.  */

static void
x_after_update_window_line (desired_row)
     struct glyph_row *desired_row;
{
  struct window *w = updated_window;
  struct frame *f;
  int width, height;

  xassert (w);

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = 1;

  /* When a window has disappeared, make sure that no rest of
     full-width rows stays visible in the internal border.  Could
     check here if updated_window is the leftmost/rightmost window,
     but I guess it's not worth doing since vertically split windows
     are almost never used, internal border is rarely set, and the
     overhead is very small.  */
  if (windows_or_buffers_changed
      && desired_row->full_width_p
      && (f = XFRAME (w->frame),
	  width = FRAME_INTERNAL_BORDER_WIDTH (f),
	  width != 0)
      && (height = desired_row->visible_height,
	  height > 0))
    {
      int y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, desired_row->y));

      /* Internal border is drawn below the tool bar.  */
      if (WINDOWP (f->tool_bar_window)
	  && w == XWINDOW (f->tool_bar_window))
	y -= width;

      BLOCK_INPUT;
      x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    0, y, width, height, False);
      x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    FRAME_PIXEL_WIDTH (f) - width,
		    y, width, height, False);
      UNBLOCK_INPUT;
    }
}

static void
x_draw_fringe_bitmap (w, row, p)
     struct window *w;
     struct glyph_row *row;
     struct draw_fringe_bitmap_params *p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Display *display = FRAME_X_DISPLAY (f);
  Window window = FRAME_X_WINDOW (f);
  GC gc = f->output_data.x->normal_gc;
  struct face *face = p->face;
  int rowY;

  /* Must clip because of partially visible lines.  */
  rowY = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  if (p->y < rowY)
    {
      /* Adjust position of "bottom aligned" bitmap on partially
	 visible last row.  */
      int oldY = row->y;
      int oldVH = row->visible_height;
      row->visible_height = p->h;
      row->y -= rowY - p->y;
      x_clip_to_row (w, row, -1, gc);
      row->y = oldY;
      row->visible_height = oldVH;
    }
  else
    x_clip_to_row (w, row, -1, gc);

  if (!p->overlay_p)
    {
      int bx = p->bx, by = p->by, nx = p->nx, ny = p->ny;

      /* In case the same realized face is used for fringes and
	 for something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 FillSolid in x_draw_glyph_string_background.  */
      if (face->stipple)
	XSetFillStyle (display, face->gc, FillOpaqueStippled);
      else
	XSetForeground (display, face->gc, face->background);

#ifdef USE_TOOLKIT_SCROLL_BARS
      /* If the fringe is adjacent to the left (right) scroll bar of a
	 leftmost (rightmost, respectively) window, then extend its
	 background to the gap between the fringe and the bar.  */
      if ((WINDOW_LEFTMOST_P (w)
	   && WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w))
	  || (WINDOW_RIGHTMOST_P (w)
	      && WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w)))
	{
	  int sb_width = WINDOW_CONFIG_SCROLL_BAR_WIDTH (w);

	  if (sb_width > 0)
	    {
	      int left = WINDOW_SCROLL_BAR_AREA_X (w);
	      int width = (WINDOW_CONFIG_SCROLL_BAR_COLS (w)
			   * FRAME_COLUMN_WIDTH (f));

	      if (bx < 0)
		{
		  /* Bitmap fills the fringe.  */
		  if (left + width == p->x)
		    bx = left + sb_width;
		  else if (p->x + p->wd == left)
		    bx = left;
		  if (bx >= 0)
		    {
		      int header_line_height = WINDOW_HEADER_LINE_HEIGHT (w);

		      nx = width - sb_width;
		      by = WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
							    row->y));
		      ny = row->visible_height;
		    }
		}
	      else
		{
		  if (left + width == bx)
		    {
		      bx = left + sb_width;
		      nx += width - sb_width;
		    }
		  else if (bx + nx == left)
		    nx += width - sb_width;
		}
	    }
	}
#endif
      if (bx >= 0 && nx > 0)
	XFillRectangle (display, window, face->gc, bx, by, nx, ny);

      if (!face->stipple)
	XSetForeground (display, face->gc, face->foreground);
    }

  if (p->which)
    {
      unsigned char *bits;
      Pixmap pixmap, clipmask = (Pixmap) 0;
      int depth = DefaultDepthOfScreen (FRAME_X_SCREEN (f));
      XGCValues gcv;

      if (p->wd > 8)
	bits = (unsigned char *)(p->bits + p->dh);
      else
	bits = (unsigned char *)p->bits + p->dh;

      /* Draw the bitmap.  I believe these small pixmaps can be cached
	 by the server.  */
      pixmap = XCreatePixmapFromBitmapData (display, window, bits, p->wd, p->h,
					    (p->cursor_p
					     ? (p->overlay_p ? face->background
						: f->output_data.x->cursor_pixel)
					     : face->foreground),
					    face->background, depth);

      if (p->overlay_p)
	{
	  clipmask = XCreatePixmapFromBitmapData (display,
						  FRAME_X_DISPLAY_INFO (f)->root_window,
						  bits, p->wd, p->h,
						  1, 0, 1);
	  gcv.clip_mask = clipmask;
	  gcv.clip_x_origin = p->x;
	  gcv.clip_y_origin = p->y;
	  XChangeGC (display, gc, GCClipMask | GCClipXOrigin | GCClipYOrigin, &gcv);
	}

      XCopyArea (display, pixmap, window, gc, 0, 0,
		 p->wd, p->h, p->x, p->y);
      XFreePixmap (display, pixmap);

      if (p->overlay_p)
	{
	  gcv.clip_mask = (Pixmap) 0;
	  XChangeGC (display, gc, GCClipMask, &gcv);
	  XFreePixmap (display, clipmask);
	}
    }

  XSetClipMask (display, gc, None);
}



/* This is called when starting Emacs and when restarting after
   suspend.  When starting Emacs, no X window is mapped.  And nothing
   must be done to Emacs's own window if it is suspended (though that
   rarely happens).  */

static void
XTset_terminal_modes (struct terminal *terminal)
{
}

/* This is called when exiting or suspending Emacs.  Exiting will make
   the X-windows go away, and suspending requires no action.  */

static void
XTreset_terminal_modes (struct terminal *terminal)
{
}



/***********************************************************************
			   Display Iterator
 ***********************************************************************/

/* Function prototypes of this page.  */

static int x_encode_char P_ ((int, XChar2b *, struct font_info *,
			      struct charset *, int *));


/* Get metrics of character CHAR2B in FONT.  Value is null if CHAR2B
   is not contained in the font.  */

static XCharStruct *
x_per_char_metric (font, char2b, font_type)
     XFontStruct *font;
     XChar2b *char2b;
     int font_type;  /* unused on X */
{
  /* The result metric information.  */
  XCharStruct *pcm = NULL;

  xassert (font && char2b);

  if (font->per_char != NULL)
    {
      if (font->min_byte1 == 0 && font->max_byte1 == 0)
	{
	  /* min_char_or_byte2 specifies the linear character index
	     corresponding to the first element of the per_char array,
	     max_char_or_byte2 is the index of the last character.  A
	     character with non-zero CHAR2B->byte1 is not in the font.
	     A character with byte2 less than min_char_or_byte2 or
	     greater max_char_or_byte2 is not in the font.  */
	  if (char2b->byte1 == 0
	      && char2b->byte2 >= font->min_char_or_byte2
	      && char2b->byte2 <= font->max_char_or_byte2)
	    pcm = font->per_char + char2b->byte2 - font->min_char_or_byte2;
	}
      else
	{
	  /* If either min_byte1 or max_byte1 are nonzero, both
	     min_char_or_byte2 and max_char_or_byte2 are less than
	     256, and the 2-byte character index values corresponding
	     to the per_char array element N (counting from 0) are:

	     byte1 = N/D + min_byte1
	     byte2 = N\D + min_char_or_byte2

	     where:

	     D = max_char_or_byte2 - min_char_or_byte2 + 1
	     / = integer division
	     \ = integer modulus  */
	  if (char2b->byte1 >= font->min_byte1
	      && char2b->byte1 <= font->max_byte1
	      && char2b->byte2 >= font->min_char_or_byte2
	      && char2b->byte2 <= font->max_char_or_byte2)
	    {
	      pcm = (font->per_char
		     + ((font->max_char_or_byte2 - font->min_char_or_byte2 + 1)
			* (char2b->byte1 - font->min_byte1))
		     + (char2b->byte2 - font->min_char_or_byte2));
	    }
	}
    }
  else
    {
      /* If the per_char pointer is null, all glyphs between the first
	 and last character indexes inclusive have the same
	 information, as given by both min_bounds and max_bounds.  */
      if (char2b->byte2 >= font->min_char_or_byte2
	  && char2b->byte2 <= font->max_char_or_byte2)
	pcm = &font->max_bounds;
    }

  return ((pcm == NULL
	   || (pcm->width == 0 && (pcm->rbearing - pcm->lbearing) == 0))
	  ? NULL : pcm);
}


/* Encode CHAR2B using encoding information from FONT_INFO.  CHAR2B is
   the two-byte form of C.  Encoding is returned in *CHAR2B.  */

static int
x_encode_char (c, char2b, font_info, charset, two_byte_p)
     int c;
     XChar2b *char2b;
     struct font_info *font_info;
     struct charset *charset;
     int *two_byte_p;
{
  XFontStruct *font = font_info->font;

  /* FONT_INFO may define a scheme by which to encode byte1 and byte2.
     This may be either a program in a special encoder language or a
     fixed encoding.  */
  if (font_info->font_encoder)
    {
      /* It's a program.  */
      struct ccl_program *ccl = font_info->font_encoder;

      check_ccl_update (ccl);
      if (CHARSET_DIMENSION (charset) == 1)
	{
	  ccl->reg[0] = CHARSET_ID (charset);
	  ccl->reg[1] = char2b->byte2;
	  ccl->reg[2] = -1;
	}
      else
	{
	  ccl->reg[0] = CHARSET_ID (charset);
	  ccl->reg[1] = char2b->byte1;
	  ccl->reg[2] = char2b->byte2;
	}

      ccl_driver (ccl, NULL, NULL, 0, 0, Qnil);

      /* We assume that MSBs are appropriately set/reset by CCL
	 program.  */
      if (font->max_byte1 == 0)	/* 1-byte font */
	STORE_XCHAR2B (char2b, 0, ccl->reg[1]);
      else
	STORE_XCHAR2B (char2b, ccl->reg[1], ccl->reg[2]);
    }
  else if (font_info->encoding_type)
    {
      /* Fixed encoding scheme.  See fontset.h for the meaning of the
	 encoding numbers.  */
      unsigned char enc = font_info->encoding_type;

      if ((enc == 1 || enc == 2)
	  && CHARSET_DIMENSION (charset) == 2)
	char2b->byte1 |= 0x80;

      if (enc == 1 || enc == 3)
	char2b->byte2 |= 0x80;
    }

  if (two_byte_p)
    *two_byte_p = ((XFontStruct *) (font_info->font))->max_byte1 > 0;

  return FONT_TYPE_UNKNOWN;
}



/***********************************************************************
			    Glyph display
 ***********************************************************************/



static void x_set_glyph_string_clipping P_ ((struct glyph_string *));
static void x_set_glyph_string_gc P_ ((struct glyph_string *));
static void x_draw_glyph_string_background P_ ((struct glyph_string *,
						int));
static void x_draw_glyph_string_foreground P_ ((struct glyph_string *));
static void x_draw_composite_glyph_string_foreground P_ ((struct glyph_string *));
static void x_draw_glyph_string_box P_ ((struct glyph_string *));
static void x_draw_glyph_string  P_ ((struct glyph_string *));
static void x_compute_glyph_string_overhangs P_ ((struct glyph_string *));
static void x_set_cursor_gc P_ ((struct glyph_string *));
static void x_set_mode_line_face_gc P_ ((struct glyph_string *));
static void x_set_mouse_face_gc P_ ((struct glyph_string *));
static int x_alloc_lighter_color P_ ((struct frame *, Display *, Colormap,
				      unsigned long *, double, int));
static void x_setup_relief_color P_ ((struct frame *, struct relief *,
				      double, int, unsigned long));
static void x_setup_relief_colors P_ ((struct glyph_string *));
static void x_draw_image_glyph_string P_ ((struct glyph_string *));
static void x_draw_image_relief P_ ((struct glyph_string *));
static void x_draw_image_foreground P_ ((struct glyph_string *));
static void x_draw_image_foreground_1 P_ ((struct glyph_string *, Pixmap));
static void x_clear_glyph_string_rect P_ ((struct glyph_string *, int,
					   int, int, int));
static void x_draw_relief_rect P_ ((struct frame *, int, int, int, int,
				    int, int, int, int, int, int,
				    XRectangle *));
static void x_draw_box_rect P_ ((struct glyph_string *, int, int, int, int,
				 int, int, int, XRectangle *));

#if GLYPH_DEBUG
static void x_check_font P_ ((struct frame *, XFontStruct *));
#endif


/* Set S->gc to a suitable GC for drawing glyph string S in cursor
   face.  */

static void
x_set_cursor_gc (s)
     struct glyph_string *s;
{
  if (s->font == FRAME_FONT (s->f)
      && s->face->background == FRAME_BACKGROUND_PIXEL (s->f)
      && s->face->foreground == FRAME_FOREGROUND_PIXEL (s->f)
      && !s->cmp)
    s->gc = s->f->output_data.x->cursor_gc;
  else
    {
      /* Cursor on non-default face: must merge.  */
      XGCValues xgcv;
      unsigned long mask;

      xgcv.background = s->f->output_data.x->cursor_pixel;
      xgcv.foreground = s->face->background;

      /* If the glyph would be invisible, try a different foreground.  */
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->f->output_data.x->cursor_foreground_pixel;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;

      /* Make sure the cursor is distinct from text in this face.  */
      if (xgcv.background == s->face->background
	  && xgcv.foreground == s->face->foreground)
	{
	  xgcv.background = s->face->foreground;
	  xgcv.foreground = s->face->background;
	}

      IF_DEBUG (x_check_font (s->f, s->font));
#ifdef USE_FONT_BACKEND
      if (enable_font_backend)
	xgcv.font = FRAME_X_DISPLAY_INFO (s->f)->font->fid;
      else
#endif
      xgcv.font = s->font->fid;
      xgcv.graphics_exposures = False;
      mask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;

      if (FRAME_X_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (s->display, FRAME_X_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_X_DISPLAY_INFO (s->f)->scratch_cursor_gc
	  = XCreateGC (s->display, s->window, mask, &xgcv);

      s->gc = FRAME_X_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }
}


/* Set up S->gc of glyph string S for drawing text in mouse face.  */

static void
x_set_mouse_face_gc (s)
     struct glyph_string *s;
{
  int face_id;
  struct face *face;

  /* What face has to be used last for the mouse face?  */
  face_id = FRAME_X_DISPLAY_INFO (s->f)->mouse_face_face_id;
  face = FACE_FROM_ID (s->f, face_id);
  if (face == NULL)
    face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);

  if (s->first_glyph->type == CHAR_GLYPH)
    face_id = FACE_FOR_CHAR (s->f, face, s->first_glyph->u.ch, -1, Qnil);
  else
    face_id = FACE_FOR_CHAR (s->f, face, 0, -1, Qnil);
  s->face = FACE_FROM_ID (s->f, face_id);
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);

  /* If font in this face is same as S->font, use it.  */
  if (s->font == s->face->font)
    s->gc = s->face->gc;
#ifdef USE_FONT_BACKEND
  else if (enable_font_backend)
    /* No need of setting a font for s->gc.  */
    s->gc = s->face->gc;
#endif	/* USE_FONT_BACKEND */
  else
    {
      /* Otherwise construct scratch_cursor_gc with values from FACE
	 but font FONT.  */
      XGCValues xgcv;
      unsigned long mask;

      xgcv.background = s->face->background;
      xgcv.foreground = s->face->foreground;
      IF_DEBUG (x_check_font (s->f, s->font));
#ifdef USE_FONT_BACKEND
      if (enable_font_backend)
	xgcv.font = FRAME_X_DISPLAY_INFO (s->f)->font->fid;
      else
#endif
      xgcv.font = s->font->fid;
      xgcv.graphics_exposures = False;
      mask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;

      if (FRAME_X_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (s->display, FRAME_X_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_X_DISPLAY_INFO (s->f)->scratch_cursor_gc
	  = XCreateGC (s->display, s->window, mask, &xgcv);

      s->gc = FRAME_X_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }

  xassert (s->gc != 0);
}


/* Set S->gc of glyph string S to a GC suitable for drawing a mode line.
   Faces to use in the mode line have already been computed when the
   matrix was built, so there isn't much to do, here.  */

static INLINE void
x_set_mode_line_face_gc (s)
     struct glyph_string *s;
{
  s->gc = s->face->gc;
}


/* Set S->gc of glyph string S for drawing that glyph string.  Set
   S->stippled_p to a non-zero value if the face of S has a stipple
   pattern.  */

static INLINE void
x_set_glyph_string_gc (s)
     struct glyph_string *s;
{
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);

  if (s->hl == DRAW_NORMAL_TEXT)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      x_set_mode_line_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_CURSOR)
    {
      x_set_cursor_gc (s);
      s->stippled_p = 0;
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      x_set_mouse_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }

  /* GC must have been set.  */
  xassert (s->gc != 0);
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static INLINE void
x_set_glyph_string_clipping (s)
     struct glyph_string *s;
{
#ifdef USE_FONT_BACKEND
  XRectangle *r = s->clip;
#else
  XRectangle r[2];
#endif
  int n = get_glyph_string_clip_rects (s, r, 2);

  if (n > 0)
    XSetClipRectangles (s->display, s->gc, 0, 0, r, n, Unsorted);
#ifdef USE_FONT_BACKEND
  s->num_clips = n;
#endif
}


/* Set SRC's clipping for output of glyph string DST.  This is called
   when we are drawing DST's left_overhang or right_overhang only in
   the area of SRC.  */

static void
x_set_glyph_string_clipping_exactly (src, dst)
     struct glyph_string *src, *dst;
{
  XRectangle r;

#ifdef USE_FONT_BACKEND
  if (enable_font_backend)
    {
      r.x = src->x;
      r.width = src->width;
      r.y = src->y;
      r.height = src->height;
      dst->clip[0] = r;
      dst->num_clips = 1;
    }
  else
    {
#endif	/* USE_FONT_BACKEND */
  struct glyph_string *clip_head = src->clip_head;
  struct glyph_string *clip_tail = src->clip_tail;

  /* This foces clipping just this glyph string.  */
  src->clip_head = src->clip_tail = src;
  get_glyph_string_clip_rect (src, &r);
  src->clip_head = clip_head, src->clip_tail = clip_tail;
#ifdef USE_FONT_BACKEND
    }
#endif	/* USE_FONT_BACKEND */
  XSetClipRectangles (dst->display, dst->gc, 0, 0, &r, 1, Unsorted);
}


/* RIF:
   Compute left and right overhang of glyph string S.  */

static void
x_compute_glyph_string_overhangs (s)
     struct glyph_string *s;
{
  if (s->cmp == NULL
      && s->first_glyph->type == CHAR_GLYPH)
    {
      XCharStruct cs;
      int direction, font_ascent, font_descent;

#ifdef USE_FONT_BACKEND
      if (enable_font_backend)
	{
	  unsigned *code = alloca (sizeof (unsigned) * s->nchars);
	  struct font *font = (struct font *) s->font_info;
	  struct font_metrics metrics;
	  int i;

	  for (i = 0; i < s->nchars; i++)
	    code[i] = (s->char2b[i].byte1 << 8) | s->char2b[i].byte2;
	  font->driver->text_extents (font, code, s->nchars, &metrics);
	  cs.rbearing = metrics.rbearing;
	  cs.lbearing = metrics.lbearing;
	  cs.width = metrics.width;
	}
      else
#endif	/* USE_FONT_BACKEND */
      XTextExtents16 (s->font, s->char2b, s->nchars, &direction,
		      &font_ascent, &font_descent, &cs);
      s->right_overhang = cs.rbearing > cs.width ? cs.rbearing - cs.width : 0;
      s->left_overhang = cs.lbearing < 0 ? -cs.lbearing : 0;
    }
  else if (s->cmp)
    {
      s->right_overhang = s->cmp->rbearing - s->cmp->pixel_width;
      s->left_overhang = - s->cmp->lbearing;
    }
}


/* Fill rectangle X, Y, W, H with background color of glyph string S.  */

static INLINE void
x_clear_glyph_string_rect (s, x, y, w, h)
     struct glyph_string *s;
     int x, y, w, h;
{
  XGCValues xgcv;
  XGetGCValues (s->display, s->gc, GCForeground | GCBackground, &xgcv);
  XSetForeground (s->display, s->gc, xgcv.background);
  XFillRectangle (s->display, s->window, s->gc, x, y, w, h);
  XSetForeground (s->display, s->gc, xgcv.foreground);
}


/* Draw the background of glyph_string S.  If S->background_filled_p
   is non-zero don't draw it.  FORCE_P non-zero means draw the
   background even if it wouldn't be drawn normally.  This is used
   when a string preceding S draws into the background of S, or S
   contains the first component of a composition.  */

static void
x_draw_glyph_string_background (s, force_p)
     struct glyph_string *s;
     int force_p;
{
  /* Nothing to do if background has already been drawn or if it
     shouldn't be drawn in the first place.  */
  if (!s->background_filled_p)
    {
      int box_line_width = max (s->face->box_line_width, 0);

      if (s->stippled_p)
	{
	  /* Fill background with a stipple pattern.  */
	  XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
	  XFillRectangle (s->display, s->window, s->gc, s->x,
			  s->y + box_line_width,
			  s->background_width,
			  s->height - 2 * box_line_width);
	  XSetFillStyle (s->display, s->gc, FillSolid);
	  s->background_filled_p = 1;
	}
      else if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	       || s->font_not_found_p
	       || s->extends_to_end_of_line_p
	       || force_p)
	{
	  x_clear_glyph_string_rect (s, s->x, s->y + box_line_width,
				     s->background_width,
				     s->height - 2 * box_line_width);
	  s->background_filled_p = 1;
	}
    }
}


/* Draw the foreground of glyph string S.  */

static void
x_draw_glyph_string_foreground (s)
     struct glyph_string *s;
{
  int i, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + eabs (s->face->box_line_width);
  else
    x = s->x;

  /* Draw characters of S as rectangles if S's font could not be
     loaded.  */
  if (s->font_not_found_p)
    {
      for (i = 0; i < s->nchars; ++i)
	{
	  struct glyph *g = s->first_glyph + i;
	  XDrawRectangle (s->display, s->window,
			  s->gc, x, s->y, g->pixel_width - 1,
			  s->height - 1);
	  x += g->pixel_width;
	}
    }
#ifdef USE_FONT_BACKEND
  else if (enable_font_backend)
    {
      int boff = s->font_info->baseline_offset;
      struct font *font = (struct font *) s->font_info;
      int y;

      if (s->font_info->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (s->font, s->f) - boff;

      y = s->ybase - boff;
      if (s->for_overlaps
	  || (s->background_filled_p && s->hl != DRAW_CURSOR))
	font->driver->draw (s, 0, s->nchars, x, y, 0);
      else
	font->driver->draw (s, 0, s->nchars, x, y, 1);
      if (s->face->overstrike)
	font->driver->draw (s, 0, s->nchars, x + 1, y, 0);
    }
#endif	/* USE_FONT_BACKEND */
  else
    {
      char *char1b = (char *) s->char2b;
      int boff = s->font_info->baseline_offset;

      if (s->font_info->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (s->font, s->f) - boff;

      /* If we can use 8-bit functions, condense S->char2b.  */
      if (!s->two_byte_p)
	for (i = 0; i < s->nchars; ++i)
	  char1b[i] = s->char2b[i].byte2;

      /* Draw text with XDrawString if background has already been
	 filled.  Otherwise, use XDrawImageString.  (Note that
	 XDrawImageString is usually faster than XDrawString.)  Always
	 use XDrawImageString when drawing the cursor so that there is
	 no chance that characters under a box cursor are invisible.  */
      if (s->for_overlaps
	  || (s->background_filled_p && s->hl != DRAW_CURSOR))
	{
	  /* Draw characters with 16-bit or 8-bit functions.  */
	  if (s->two_byte_p)
	    XDrawString16 (s->display, s->window, s->gc, x,
			   s->ybase - boff, s->char2b, s->nchars);
	  else
	    XDrawString (s->display, s->window, s->gc, x,
			 s->ybase - boff, char1b, s->nchars);
	}
      else
	{
	  if (s->two_byte_p)
	    XDrawImageString16 (s->display, s->window, s->gc, x,
				s->ybase - boff, s->char2b, s->nchars);
	  else
	    XDrawImageString (s->display, s->window, s->gc, x,
			      s->ybase - boff, char1b, s->nchars);
	}

      if (s->face->overstrike)
	{
	  /* For overstriking (to simulate bold-face), draw the
	     characters again shifted to the right by one pixel.  */
	  if (s->two_byte_p)
	    XDrawString16 (s->display, s->window, s->gc, x + 1,
			   s->ybase - boff, s->char2b, s->nchars);
	  else
	    XDrawString (s->display, s->window, s->gc, x + 1,
			 s->ybase - boff, char1b, s->nchars);
	}
    }
}

/* Draw the foreground of composite glyph string S.  */

static void
x_draw_composite_glyph_string_foreground (s)
     struct glyph_string *s;
{
  int i, j, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + eabs (s->face->box_line_width);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->gidx is the index of
     the first character drawn for glyphs of this composition.
     S->gidx == 0 means we are drawing the very first character of
     this composition.  */

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->gidx == 0)
	XDrawRectangle (s->display, s->window, s->gc, x, s->y,
			s->width - 1, s->height - 1);
    }
#ifdef USE_FONT_BACKEND
  else if (enable_font_backend)
    {
      struct font *font = (struct font *) s->font_info;
      int y = s->ybase;
      int width = 0;

      if (s->cmp->method == COMPOSITION_WITH_GLYPH_STRING)
	{
	  Lisp_Object gstring = AREF (XHASH_TABLE (composition_hash_table)
				      ->key_and_value,
				      s->cmp->hash_index * 2);
	  int from;

	  for (i = from = 0; i < s->nchars; i++)
	    {
	      Lisp_Object g = LGSTRING_GLYPH (gstring, i);
	      Lisp_Object adjustment = LGLYPH_ADJUSTMENT (g);
	      int xoff, yoff, wadjust;

	      if (! VECTORP (adjustment))
		{
		  width += LGLYPH_WIDTH (g);
		  continue;
		}
	      if (from < i)
		{
		  font->driver->draw (s, from, i, x, y, 0);
		  x += width;
		}
	      xoff = XINT (AREF (adjustment, 0));
	      yoff = XINT (AREF (adjustment, 1));
	      wadjust = XINT (AREF (adjustment, 2));

	      font->driver->draw (s, i, i + 1, x + xoff, y + yoff, 0);
	      x += wadjust;
	      from = i + 1;
	      width = 0;
	    }
	  if (from < i)
	    font->driver->draw (s, from, i, x, y, 0);
	}
      else
	{
	  for (i = 0, j = s->gidx; i < s->nchars; i++, j++)
	    if (COMPOSITION_GLYPH (s->cmp, j) != '\t')
	      {
		int xx = x + s->cmp->offsets[j * 2];
		int yy = y - s->cmp->offsets[j * 2 + 1];

		font->driver->draw (s, j, j + 1, xx, yy, 0);
		if (s->face->overstrike)
		  font->driver->draw (s, j, j + 1, xx + 1, yy, 0);
	      }
	}
    }
#endif	/* USE_FONT_BACKEND */
  else
    {
      for (i = 0, j = s->gidx; i < s->nchars; i++, j++)
	if (s->face)
	  {
	    XDrawString16 (s->display, s->window, s->gc,
			   x + s->cmp->offsets[j * 2],
			   s->ybase - s->cmp->offsets[j * 2 + 1],
			   s->char2b + j, 1);
	    if (s->face->overstrike)
	      XDrawString16 (s->display, s->window, s->gc,
			     x + s->cmp->offsets[j * 2] + 1,
			     s->ybase - s->cmp->offsets[j * 2 + 1],
			     s->char2b + j, 1);
	  }
    }
}


#ifdef USE_X_TOOLKIT

static struct frame *x_frame_of_widget P_ ((Widget));
static Boolean cvt_string_to_pixel P_ ((Display *, XrmValue *, Cardinal *,
					XrmValue *, XrmValue *, XtPointer *));
static void cvt_pixel_dtor P_ ((XtAppContext, XrmValue *, XtPointer,
				XrmValue *, Cardinal *));


/* Return the frame on which widget WIDGET is used.. Abort if frame
   cannot be determined.  */

static struct frame *
x_frame_of_widget (widget)
     Widget widget;
{
  struct x_display_info *dpyinfo;
  Lisp_Object tail;
  struct frame *f;

  dpyinfo = x_display_info_for_display (XtDisplay (widget));

  /* Find the top-level shell of the widget.  Note that this function
     can be called when the widget is not yet realized, so XtWindow
     (widget) == 0.  That's the reason we can't simply use
     x_any_window_to_frame.  */
  while (!XtIsTopLevelShell (widget))
    widget = XtParent (widget);

  /* Look for a frame with that top-level widget.  Allocate the color
     on that frame to get the right gamma correction value.  */
  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    if (FRAMEP (XCAR (tail))
	&& (f = XFRAME (XCAR (tail)),
	    (FRAME_X_P (f)
             && f->output_data.nothing != 1
	     && FRAME_X_DISPLAY_INFO (f) == dpyinfo))
	&& f->output_data.x->widget == widget)
      return f;

  abort ();
}


/* Allocate the color COLOR->pixel on the screen and display of
   widget WIDGET in colormap CMAP.  If an exact match cannot be
   allocated, try the nearest color available.  Value is non-zero
   if successful.  This is called from lwlib.  */

int
x_alloc_nearest_color_for_widget (widget, cmap, color)
     Widget widget;
     Colormap cmap;
     XColor *color;
{
  struct frame *f = x_frame_of_widget (widget);
  return x_alloc_nearest_color (f, cmap, color);
}


/* Allocate a color which is lighter or darker than *PIXEL by FACTOR
   or DELTA.  Try a color with RGB values multiplied by FACTOR first.
   If this produces the same color as PIXEL, try a color where all RGB
   values have DELTA added.  Return the allocated color in *PIXEL.
   DISPLAY is the X display, CMAP is the colormap to operate on.
   Value is non-zero if successful.  */

int
x_alloc_lighter_color_for_widget (widget, display, cmap, pixel, factor, delta)
     Widget widget;
     Display *display;
     Colormap cmap;
     unsigned long *pixel;
     double factor;
     int delta;
{
  struct frame *f = x_frame_of_widget (widget);
  return x_alloc_lighter_color (f, display, cmap, pixel, factor, delta);
}


/* Structure specifying which arguments should be passed by Xt to
   cvt_string_to_pixel.  We want the widget's screen and colormap.  */

static XtConvertArgRec cvt_string_to_pixel_args[] =
  {
    {XtWidgetBaseOffset, (XtPointer) XtOffset (Widget, core.screen),
     sizeof (Screen *)},
    {XtWidgetBaseOffset, (XtPointer) XtOffset (Widget, core.colormap),
     sizeof (Colormap)}
  };


/* The address of this variable is returned by
   cvt_string_to_pixel.  */

static Pixel cvt_string_to_pixel_value;


/* Convert a color name to a pixel color.

   DPY is the display we are working on.

   ARGS is an array of *NARGS XrmValue structures holding additional
   information about the widget for which the conversion takes place.
   The contents of this array are determined by the specification
   in cvt_string_to_pixel_args.

   FROM is a pointer to an XrmValue which points to the color name to
   convert.  TO is an XrmValue in which to return the pixel color.

   CLOSURE_RET is a pointer to user-data, in which we record if
   we allocated the color or not.

   Value is True if successful, False otherwise.  */

static Boolean
cvt_string_to_pixel (dpy, args, nargs, from, to, closure_ret)
     Display *dpy;
     XrmValue *args;
     Cardinal *nargs;
     XrmValue *from, *to;
     XtPointer *closure_ret;
{
  Screen *screen;
  Colormap cmap;
  Pixel pixel;
  String color_name;
  XColor color;

  if (*nargs != 2)
    {
      XtAppWarningMsg (XtDisplayToApplicationContext (dpy),
		       "wrongParameters", "cvt_string_to_pixel",
		       "XtToolkitError",
		       "Screen and colormap args required", NULL, NULL);
      return False;
    }

  screen = *(Screen **) args[0].addr;
  cmap = *(Colormap *) args[1].addr;
  color_name = (String) from->addr;

  if (strcmp (color_name, XtDefaultBackground) == 0)
    {
      *closure_ret = (XtPointer) False;
      pixel = WhitePixelOfScreen (screen);
    }
  else if (strcmp (color_name, XtDefaultForeground) == 0)
    {
      *closure_ret = (XtPointer) False;
      pixel = BlackPixelOfScreen (screen);
    }
  else if (XParseColor (dpy, cmap, color_name, &color)
	   && x_alloc_nearest_color_1 (dpy, cmap, &color))
    {
      pixel = color.pixel;
      *closure_ret = (XtPointer) True;
    }
  else
    {
      String params[1];
      Cardinal nparams = 1;

      params[0] = color_name;
      XtAppWarningMsg (XtDisplayToApplicationContext (dpy),
		       "badValue", "cvt_string_to_pixel",
		       "XtToolkitError", "Invalid color `%s'",
		       params, &nparams);
      return False;
    }

  if (to->addr != NULL)
    {
      if (to->size < sizeof (Pixel))
	{
	  to->size = sizeof (Pixel);
	  return False;
	}

      *(Pixel *) to->addr = pixel;
    }
  else
    {
      cvt_string_to_pixel_value = pixel;
      to->addr = (XtPointer) &cvt_string_to_pixel_value;
    }

  to->size = sizeof (Pixel);
  return True;
}


/* Free a pixel color which was previously allocated via
   cvt_string_to_pixel.  This is registered as the destructor
   for this type of resource via XtSetTypeConverter.

   APP is the application context in which we work.

   TO is a pointer to an XrmValue holding the color to free.
   CLOSURE is the value we stored in CLOSURE_RET for this color
   in cvt_string_to_pixel.

   ARGS and NARGS are like for cvt_string_to_pixel.  */

static void
cvt_pixel_dtor (app, to, closure, args, nargs)
    XtAppContext app;
    XrmValuePtr to;
    XtPointer closure;
    XrmValuePtr args;
    Cardinal *nargs;
{
  if (*nargs != 2)
    {
      XtAppWarningMsg (app, "wrongParameters", "cvt_pixel_dtor",
		       "XtToolkitError",
		       "Screen and colormap arguments required",
		       NULL, NULL);
    }
  else if (closure != NULL)
    {
      /* We did allocate the pixel, so free it.  */
      Screen *screen = *(Screen **) args[0].addr;
      Colormap cmap = *(Colormap *) args[1].addr;
      x_free_dpy_colors (DisplayOfScreen (screen), screen, cmap,
			 (Pixel *) to->addr, 1);
    }
}


#endif /* USE_X_TOOLKIT */


/* Value is an array of XColor structures for the contents of the
   color map of display DPY.  Set *NCELLS to the size of the array.
   Note that this probably shouldn't be called for large color maps,
   say a 24-bit TrueColor map.  */

static const XColor *
x_color_cells (dpy, ncells)
     Display *dpy;
     int *ncells;
{
  struct x_display_info *dpyinfo = x_display_info_for_display (dpy);

  if (dpyinfo->color_cells == NULL)
    {
      Screen *screen = dpyinfo->screen;
      int i;

      dpyinfo->ncolor_cells
	= XDisplayCells (dpy, XScreenNumberOfScreen (screen));
      dpyinfo->color_cells
	= (XColor *) xmalloc (dpyinfo->ncolor_cells
			      * sizeof *dpyinfo->color_cells);

      for (i = 0; i < dpyinfo->ncolor_cells; ++i)
	dpyinfo->color_cells[i].pixel = i;

      XQueryColors (dpy, dpyinfo->cmap,
		    dpyinfo->color_cells, dpyinfo->ncolor_cells);
    }

  *ncells = dpyinfo->ncolor_cells;
  return dpyinfo->color_cells;
}


/* On frame F, translate pixel colors to RGB values for the NCOLORS
   colors in COLORS.  Use cached information, if available.  */

void
x_query_colors (f, colors, ncolors)
     struct frame *f;
     XColor *colors;
     int ncolors;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  if (dpyinfo->color_cells)
    {
      int i;
      for (i = 0; i < ncolors; ++i)
	{
	  unsigned long pixel = colors[i].pixel;
	  xassert (pixel < dpyinfo->ncolor_cells);
	  xassert (dpyinfo->color_cells[pixel].pixel == pixel);
	  colors[i] = dpyinfo->color_cells[pixel];
	}
    }
  else
    XQueryColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), colors, ncolors);
}


/* On frame F, translate pixel color to RGB values for the color in
   COLOR.  Use cached information, if available.  */

void
x_query_color (f, color)
     struct frame *f;
     XColor *color;
{
  x_query_colors (f, color, 1);
}


/* Allocate the color COLOR->pixel on DISPLAY, colormap CMAP.  If an
   exact match can't be allocated, try the nearest color available.
   Value is non-zero if successful.  Set *COLOR to the color
   allocated.  */

static int
x_alloc_nearest_color_1 (dpy, cmap, color)
     Display *dpy;
     Colormap cmap;
     XColor *color;
{
  int rc;

  rc = XAllocColor (dpy, cmap, color);
  if (rc == 0)
    {
      /* If we got to this point, the colormap is full, so we're going
	 to try to get the next closest color.  The algorithm used is
	 a least-squares matching, which is what X uses for closest
	 color matching with StaticColor visuals.  */
      int nearest, i;
      unsigned long nearest_delta = ~0;
      int ncells;
      const XColor *cells = x_color_cells (dpy, &ncells);

      for (nearest = i = 0; i < ncells; ++i)
	{
	  long dred   = (color->red   >> 8) - (cells[i].red   >> 8);
	  long dgreen = (color->green >> 8) - (cells[i].green >> 8);
	  long dblue  = (color->blue  >> 8) - (cells[i].blue  >> 8);
	  unsigned long delta = dred * dred + dgreen * dgreen + dblue * dblue;

	  if (delta < nearest_delta)
	    {
	      nearest = i;
	      nearest_delta = delta;
	    }
	}

      color->red   = cells[nearest].red;
      color->green = cells[nearest].green;
      color->blue  = cells[nearest].blue;
      rc = XAllocColor (dpy, cmap, color);
    }
  else
    {
      /* If allocation succeeded, and the allocated pixel color is not
         equal to a cached pixel color recorded earlier, there was a
         change in the colormap, so clear the color cache.  */
      struct x_display_info *dpyinfo = x_display_info_for_display (dpy);
      XColor *cached_color;

      if (dpyinfo->color_cells
	  && (cached_color = &dpyinfo->color_cells[color->pixel],
	      (cached_color->red != color->red
	       || cached_color->blue != color->blue
	       || cached_color->green != color->green)))
	{
	  xfree (dpyinfo->color_cells);
	  dpyinfo->color_cells = NULL;
	  dpyinfo->ncolor_cells = 0;
	}
    }

#ifdef DEBUG_X_COLORS
  if (rc)
    register_color (color->pixel);
#endif /* DEBUG_X_COLORS */

  return rc;
}


/* Allocate the color COLOR->pixel on frame F, colormap CMAP.  If an
   exact match can't be allocated, try the nearest color available.
   Value is non-zero if successful.  Set *COLOR to the color
   allocated.  */

int
x_alloc_nearest_color (f, cmap, color)
     struct frame *f;
     Colormap cmap;
     XColor *color;
{
  gamma_correct (f, color);
  return x_alloc_nearest_color_1 (FRAME_X_DISPLAY (f), cmap, color);
}


/* Allocate color PIXEL on frame F.  PIXEL must already be allocated.
   It's necessary to do this instead of just using PIXEL directly to
   get color reference counts right.  */

unsigned long
x_copy_color (f, pixel)
     struct frame *f;
     unsigned long pixel;
{
  XColor color;

  color.pixel = pixel;
  BLOCK_INPUT;
  x_query_color (f, &color);
  XAllocColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), &color);
  UNBLOCK_INPUT;
#ifdef DEBUG_X_COLORS
  register_color (pixel);
#endif
  return color.pixel;
}


/* Allocate color PIXEL on display DPY.  PIXEL must already be allocated.
   It's necessary to do this instead of just using PIXEL directly to
   get color reference counts right.  */

unsigned long
x_copy_dpy_color (dpy, cmap, pixel)
     Display *dpy;
     Colormap cmap;
     unsigned long pixel;
{
  XColor color;

  color.pixel = pixel;
  BLOCK_INPUT;
  XQueryColor (dpy, cmap, &color);
  XAllocColor (dpy, cmap, &color);
  UNBLOCK_INPUT;
#ifdef DEBUG_X_COLORS
  register_color (pixel);
#endif
  return color.pixel;
}


/* Brightness beyond which a color won't have its highlight brightness
   boosted.

   Nominally, highlight colors for `3d' faces are calculated by
   brightening an object's color by a constant scale factor, but this
   doesn't yield good results for dark colors, so for colors who's
   brightness is less than this value (on a scale of 0-65535) have an
   use an additional additive factor.

   The value here is set so that the default menu-bar/mode-line color
   (grey75) will not have its highlights changed at all.  */
#define HIGHLIGHT_COLOR_DARK_BOOST_LIMIT 48000


/* Allocate a color which is lighter or darker than *PIXEL by FACTOR
   or DELTA.  Try a color with RGB values multiplied by FACTOR first.
   If this produces the same color as PIXEL, try a color where all RGB
   values have DELTA added.  Return the allocated color in *PIXEL.
   DISPLAY is the X display, CMAP is the colormap to operate on.
   Value is non-zero if successful.  */

static int
x_alloc_lighter_color (f, display, cmap, pixel, factor, delta)
     struct frame *f;
     Display *display;
     Colormap cmap;
     unsigned long *pixel;
     double factor;
     int delta;
{
  XColor color, new;
  long bright;
  int success_p;

  /* Get RGB color values.  */
  color.pixel = *pixel;
  x_query_color (f, &color);

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  xassert (factor >= 0);
  new.red = min (0xffff, factor * color.red);
  new.green = min (0xffff, factor * color.green);
  new.blue = min (0xffff, factor * color.blue);

  /* Calculate brightness of COLOR.  */
  bright = (2 * color.red + 3 * color.green + color.blue) / 6;

  /* We only boost colors that are darker than
     HIGHLIGHT_COLOR_DARK_BOOST_LIMIT.  */
  if (bright < HIGHLIGHT_COLOR_DARK_BOOST_LIMIT)
    /* Make an additive adjustment to NEW, because it's dark enough so
       that scaling by FACTOR alone isn't enough.  */
    {
      /* How far below the limit this color is (0 - 1, 1 being darker).  */
      double dimness = 1 - (double)bright / HIGHLIGHT_COLOR_DARK_BOOST_LIMIT;
      /* The additive adjustment.  */
      int min_delta = delta * dimness * factor / 2;

      if (factor < 1)
	{
	  new.red =   max (0, new.red -   min_delta);
	  new.green = max (0, new.green - min_delta);
	  new.blue =  max (0, new.blue -  min_delta);
	}
      else
	{
	  new.red =   min (0xffff, min_delta + new.red);
	  new.green = min (0xffff, min_delta + new.green);
	  new.blue =  min (0xffff, min_delta + new.blue);
	}
    }

  /* Try to allocate the color.  */
  success_p = x_alloc_nearest_color (f, cmap, &new);
  if (success_p)
    {
      if (new.pixel == *pixel)
	{
	  /* If we end up with the same color as before, try adding
	     delta to the RGB values.  */
	  x_free_colors (f, &new.pixel, 1);

	  new.red = min (0xffff, delta + color.red);
	  new.green = min (0xffff, delta + color.green);
	  new.blue = min (0xffff, delta + color.blue);
	  success_p = x_alloc_nearest_color (f, cmap, &new);
	}
      else
	success_p = 1;
      *pixel = new.pixel;
    }

  return success_p;
}


/* Set up the foreground color for drawing relief lines of glyph
   string S.  RELIEF is a pointer to a struct relief containing the GC
   with which lines will be drawn.  Use a color that is FACTOR or
   DELTA lighter or darker than the relief's background which is found
   in S->f->output_data.x->relief_background.  If such a color cannot
   be allocated, use DEFAULT_PIXEL, instead.  */

static void
x_setup_relief_color (f, relief, factor, delta, default_pixel)
     struct frame *f;
     struct relief *relief;
     double factor;
     int delta;
     unsigned long default_pixel;
{
  XGCValues xgcv;
  struct x_output *di = f->output_data.x;
  unsigned long mask = GCForeground | GCLineWidth | GCGraphicsExposures;
  unsigned long pixel;
  unsigned long background = di->relief_background;
  Colormap cmap = FRAME_X_COLORMAP (f);
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Display *dpy = FRAME_X_DISPLAY (f);

  xgcv.graphics_exposures = False;
  xgcv.line_width = 1;

  /* Free previously allocated color.  The color cell will be reused
     when it has been freed as many times as it was allocated, so this
     doesn't affect faces using the same colors.  */
  if (relief->gc
      && relief->allocated_p)
    {
      x_free_colors (f, &relief->pixel, 1);
      relief->allocated_p = 0;
    }

  /* Allocate new color.  */
  xgcv.foreground = default_pixel;
  pixel = background;
  if (dpyinfo->n_planes != 1
      && x_alloc_lighter_color (f, dpy, cmap, &pixel, factor, delta))
    {
      relief->allocated_p = 1;
      xgcv.foreground = relief->pixel = pixel;
    }

  if (relief->gc == 0)
    {
      xgcv.stipple = dpyinfo->gray;
      mask |= GCStipple;
      relief->gc = XCreateGC (dpy, FRAME_X_WINDOW (f), mask, &xgcv);
    }
  else
    XChangeGC (dpy, relief->gc, mask, &xgcv);
}


/* Set up colors for the relief lines around glyph string S.  */

static void
x_setup_relief_colors (s)
     struct glyph_string *s;
{
  struct x_output *di = s->f->output_data.x;
  unsigned long color;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
    color = IMAGE_BACKGROUND (s->img, s->f, 0);
  else
    {
      XGCValues xgcv;

      /* Get the background color of the face.  */
      XGetGCValues (s->display, s->gc, GCBackground, &xgcv);
      color = xgcv.background;
    }

  if (di->white_relief.gc == 0
      || color != di->relief_background)
    {
      di->relief_background = color;
      x_setup_relief_color (s->f, &di->white_relief, 1.2, 0x8000,
			    WHITE_PIX_DEFAULT (s->f));
      x_setup_relief_color (s->f, &di->black_relief, 0.6, 0x4000,
			    BLACK_PIX_DEFAULT (s->f));
    }
}


/* Draw a relief on frame F inside the rectangle given by LEFT_X,
   TOP_Y, RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the relief
   to draw, it must be >= 0.  RAISED_P non-zero means draw a raised
   relief.  LEFT_P non-zero means draw a relief on the left side of
   the rectangle.  RIGHT_P non-zero means draw a relief on the right
   side of the rectangle.  CLIP_RECT is the clipping rectangle to use
   when drawing.  */

static void
x_draw_relief_rect (f, left_x, top_y, right_x, bottom_y, width,
		    raised_p, top_p, bot_p, left_p, right_p, clip_rect)
     struct frame *f;
     int left_x, top_y, right_x, bottom_y, width;
     int top_p, bot_p, left_p, right_p, raised_p;
     XRectangle *clip_rect;
{
  Display *dpy = FRAME_X_DISPLAY (f);
  Window window = FRAME_X_WINDOW (f);
  int i;
  GC gc;

  if (raised_p)
    gc = f->output_data.x->white_relief.gc;
  else
    gc = f->output_data.x->black_relief.gc;
  XSetClipRectangles (dpy, gc, 0, 0, clip_rect, 1, Unsorted);

  /* Top.  */
  if (top_p)
    for (i = 0; i < width; ++i)
      XDrawLine (dpy, window, gc,
		 left_x + i * left_p, top_y + i,
		 right_x + 1 - i * right_p, top_y + i);

  /* Left.  */
  if (left_p)
    for (i = 0; i < width; ++i)
      XDrawLine (dpy, window, gc,
		 left_x + i, top_y + i, left_x + i, bottom_y - i + 1);

  XSetClipMask (dpy, gc, None);
  if (raised_p)
    gc = f->output_data.x->black_relief.gc;
  else
    gc = f->output_data.x->white_relief.gc;
  XSetClipRectangles (dpy, gc, 0, 0, clip_rect, 1, Unsorted);

  /* Bottom.  */
  if (bot_p)
    for (i = 0; i < width; ++i)
      XDrawLine (dpy, window, gc,
		 left_x + i * left_p, bottom_y - i,
		 right_x + 1 - i * right_p, bottom_y - i);

  /* Right.  */
  if (right_p)
    for (i = 0; i < width; ++i)
      XDrawLine (dpy, window, gc,
		 right_x - i, top_y + i + 1, right_x - i, bottom_y - i);

  XSetClipMask (dpy, gc, None);
}


/* Draw a box on frame F inside the rectangle given by LEFT_X, TOP_Y,
   RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the lines to
   draw, it must be >= 0.  LEFT_P non-zero means draw a line on the
   left side of the rectangle.  RIGHT_P non-zero means draw a line
   on the right side of the rectangle.  CLIP_RECT is the clipping
   rectangle to use when drawing.  */

static void
x_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
		 left_p, right_p, clip_rect)
     struct glyph_string *s;
     int left_x, top_y, right_x, bottom_y, width, left_p, right_p;
     XRectangle *clip_rect;
{
  XGCValues xgcv;

  XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
  XSetForeground (s->display, s->gc, s->face->box_color);
  XSetClipRectangles (s->display, s->gc, 0, 0, clip_rect, 1, Unsorted);

  /* Top.  */
  XFillRectangle (s->display, s->window, s->gc,
		  left_x, top_y, right_x - left_x + 1, width);

  /* Left.  */
  if (left_p)
    XFillRectangle (s->display, s->window, s->gc,
		    left_x, top_y, width, bottom_y - top_y + 1);

  /* Bottom.  */
  XFillRectangle (s->display, s->window, s->gc,
		  left_x, bottom_y - width + 1, right_x - left_x + 1, width);

  /* Right.  */
  if (right_p)
    XFillRectangle (s->display, s->window, s->gc,
		    right_x - width + 1, top_y, width, bottom_y - top_y + 1);

  XSetForeground (s->display, s->gc, xgcv.foreground);
  XSetClipMask (s->display, s->gc, None);
}


/* Draw a box around glyph string S.  */

static void
x_draw_glyph_string_box (s)
     struct glyph_string *s;
{
  int width, left_x, right_x, top_y, bottom_y, last_x, raised_p;
  int left_p, right_p;
  struct glyph *last_glyph;
  XRectangle clip_rect;

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));

  /* The glyph that may have a right box line.  */
  last_glyph = (s->cmp || s->img
		? s->first_glyph
		: s->first_glyph + s->nchars - 1);

  width = eabs (s->face->box_line_width);
  raised_p = s->face->box == FACE_RAISED_BOX;
  left_x = s->x;
  right_x = (s->row->full_width_p && s->extends_to_end_of_line_p
	     ? last_x - 1
	     : min (last_x, s->x + s->background_width) - 1);
  top_y = s->y;
  bottom_y = top_y + s->height - 1;

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL
		    || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL
		     || s->next->hl != s->hl)));

  get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    x_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
		     left_p, right_p, &clip_rect);
  else
    {
      x_setup_relief_colors (s);
      x_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y,
			  width, raised_p, 1, 1, left_p, right_p, &clip_rect);
    }
}


/* Draw foreground of image glyph string S.  */

static void
x_draw_image_foreground (s)
     struct glyph_string *s;
{
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += eabs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->img->pixmap)
    {
      if (s->img->mask)
	{
	  /* We can't set both a clip mask and use XSetClipRectangles
	     because the latter also sets a clip mask.  We also can't
	     trust on the shape extension to be available
	     (XShapeCombineRegion).  So, compute the rectangle to draw
	     manually.  */
	  unsigned long mask = (GCClipMask | GCClipXOrigin | GCClipYOrigin
				| GCFunction);
	  XGCValues xgcv;
	  XRectangle clip_rect, image_rect, r;

	  xgcv.clip_mask = s->img->mask;
	  xgcv.clip_x_origin = x;
	  xgcv.clip_y_origin = y;
	  xgcv.function = GXcopy;
	  XChangeGC (s->display, s->gc, mask, &xgcv);

	  get_glyph_string_clip_rect (s, &clip_rect);
	  image_rect.x = x;
	  image_rect.y = y;
	  image_rect.width = s->slice.width;
	  image_rect.height = s->slice.height;
	  if (x_intersect_rectangles (&clip_rect, &image_rect, &r))
	    XCopyArea (s->display, s->img->pixmap, s->window, s->gc,
		       s->slice.x + r.x - x, s->slice.y + r.y - y,
		       r.width, r.height, r.x, r.y);
	}
      else
	{
	  XRectangle clip_rect, image_rect, r;

	  get_glyph_string_clip_rect (s, &clip_rect);
	  image_rect.x = x;
	  image_rect.y = y;
	  image_rect.width = s->slice.width;
	  image_rect.height = s->slice.height;
	  if (x_intersect_rectangles (&clip_rect, &image_rect, &r))
	    XCopyArea (s->display, s->img->pixmap, s->window, s->gc,
		       s->slice.x + r.x - x, s->slice.y + r.y - y,
		       r.width, r.height, r.x, r.y);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int r = s->img->relief;
	      if (r < 0) r = -r;
	      XDrawRectangle (s->display, s->window, s->gc,
			      x - r, y - r,
			      s->slice.width + r*2 - 1,
			      s->slice.height + r*2 - 1);
	    }
	}
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    XDrawRectangle (s->display, s->window, s->gc, x, y,
		    s->slice.width - 1, s->slice.height - 1);
}


/* Draw a relief around the image glyph string S.  */

static void
x_draw_image_relief (s)
     struct glyph_string *s;
{
  int x0, y0, x1, y1, thick, raised_p;
  XRectangle r;
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += eabs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      thick = tool_bar_button_relief >= 0 ? tool_bar_button_relief : DEFAULT_TOOL_BAR_BUTTON_RELIEF;
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = eabs (s->img->relief);
      raised_p = s->img->relief > 0;
    }

  x0 = x - thick;
  y0 = y - thick;
  x1 = x + s->slice.width + thick - 1;
  y1 = y + s->slice.height + thick - 1;

  x_setup_relief_colors (s);
  get_glyph_string_clip_rect (s, &r);
  x_draw_relief_rect (s->f, x0, y0, x1, y1, thick, raised_p,
		      s->slice.y == 0,
		      s->slice.y + s->slice.height == s->img->height,
		      s->slice.x == 0,
		      s->slice.x + s->slice.width == s->img->width,
		      &r);
}


/* Draw the foreground of image glyph string S to PIXMAP.  */

static void
x_draw_image_foreground_1 (s, pixmap)
     struct glyph_string *s;
     Pixmap pixmap;
{
  int x = 0;
  int y = s->ybase - s->y - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += eabs (s->face->box_line_width);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->img->pixmap)
    {
      if (s->img->mask)
	{
	  /* We can't set both a clip mask and use XSetClipRectangles
	     because the latter also sets a clip mask.  We also can't
	     trust on the shape extension to be available
	     (XShapeCombineRegion).  So, compute the rectangle to draw
	     manually.  */
	  unsigned long mask = (GCClipMask | GCClipXOrigin | GCClipYOrigin
				| GCFunction);
	  XGCValues xgcv;

	  xgcv.clip_mask = s->img->mask;
	  xgcv.clip_x_origin = x - s->slice.x;
	  xgcv.clip_y_origin = y - s->slice.y;
	  xgcv.function = GXcopy;
	  XChangeGC (s->display, s->gc, mask, &xgcv);

	  XCopyArea (s->display, s->img->pixmap, pixmap, s->gc,
		     s->slice.x, s->slice.y,
		     s->slice.width, s->slice.height, x, y);
	  XSetClipMask (s->display, s->gc, None);
	}
      else
	{
	  XCopyArea (s->display, s->img->pixmap, pixmap, s->gc,
		     s->slice.x, s->slice.y,
		     s->slice.width, s->slice.height, x, y);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int r = s->img->relief;
	      if (r < 0) r = -r;
	      XDrawRectangle (s->display, s->window, s->gc, x - r, y - r,
			      s->slice.width + r*2 - 1,
			      s->slice.height + r*2 - 1);
	    }
	}
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    XDrawRectangle (s->display, pixmap, s->gc, x, y,
		    s->slice.width - 1, s->slice.height - 1);
}


/* Draw part of the background of glyph string S.  X, Y, W, and H
   give the rectangle to draw.  */

static void
x_draw_glyph_string_bg_rect (s, x, y, w, h)
     struct glyph_string *s;
     int x, y, w, h;
{
  if (s->stippled_p)
    {
      /* Fill background with a stipple pattern.  */
      XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
      XFillRectangle (s->display, s->window, s->gc, x, y, w, h);
      XSetFillStyle (s->display, s->gc, FillSolid);
    }
  else
    x_clear_glyph_string_rect (s, x, y, w, h);
}


/* Draw image glyph string S.

            s->y
   s->x      +-------------------------
	     |   s->face->box
	     |
	     |     +-------------------------
	     |     |  s->img->margin
	     |     |
	     |     |       +-------------------
	     |     |       |  the image

 */

static void
x_draw_image_glyph_string (s)
     struct glyph_string *s;
{
  int box_line_hwidth = eabs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height;
  Pixmap pixmap = None;

  height = s->height;
  if (s->slice.y == 0)
    height -= box_line_vwidth;
  if (s->slice.y + s->slice.height >= s->img->height)
    height -= box_line_vwidth;

  /* Fill background with face under the image.  Do it only if row is
     taller than image or if image has a clip mask to reduce
     flickering.  */
  s->stippled_p = s->face->stipple != 0;
  if (height > s->slice.height
      || s->img->hmargin
      || s->img->vmargin
      || s->img->mask
      || s->img->pixmap == 0
      || s->width != s->background_width)
    {
      if (s->img->mask)
	{
	  /* Create a pixmap as large as the glyph string.  Fill it
	     with the background color.  Copy the image to it, using
	     its mask.  Copy the temporary pixmap to the display.  */
	  Screen *screen = FRAME_X_SCREEN (s->f);
	  int depth = DefaultDepthOfScreen (screen);

	  /* Create a pixmap as large as the glyph string.  */
 	  pixmap = XCreatePixmap (s->display, s->window,
				  s->background_width,
				  s->height, depth);

	  /* Don't clip in the following because we're working on the
	     pixmap.  */
	  XSetClipMask (s->display, s->gc, None);

	  /* Fill the pixmap with the background color/stipple.  */
	  if (s->stippled_p)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (s->display, s->gc, FillOpaqueStippled);
	      XSetTSOrigin (s->display, s->gc, - s->x, - s->y);
	      XFillRectangle (s->display, pixmap, s->gc,
			      0, 0, s->background_width, s->height);
	      XSetFillStyle (s->display, s->gc, FillSolid);
	      XSetTSOrigin (s->display, s->gc, 0, 0);
	    }
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground | GCBackground,
			    &xgcv);
	      XSetForeground (s->display, s->gc, xgcv.background);
	      XFillRectangle (s->display, pixmap, s->gc,
			      0, 0, s->background_width, s->height);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}
      else
	{
	  int x = s->x;
	  int y = s->y;

	  if (s->first_glyph->left_box_line_p
	      && s->slice.x == 0)
	    x += box_line_hwidth;

	  if (s->slice.y == 0)
	    y += box_line_vwidth;

	  x_draw_glyph_string_bg_rect (s, x, y, s->background_width, height);
	}

      s->background_filled_p = 1;
    }

  /* Draw the foreground.  */
  if (pixmap != None)
    {
      x_draw_image_foreground_1 (s, pixmap);
      x_set_glyph_string_clipping (s);
      XCopyArea (s->display, pixmap, s->window, s->gc,
		 0, 0, s->background_width, s->height, s->x, s->y);
      XFreePixmap (s->display, pixmap);
    }
  else
    x_draw_image_foreground (s);

  /* If we must draw a relief around the image, do it.  */
  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    x_draw_image_relief (s);
}


/* Draw stretch glyph string S.  */

static void
x_draw_stretch_glyph_string (s)
     struct glyph_string *s;
{
  xassert (s->first_glyph->type == STRETCH_GLYPH);

  if (s->hl == DRAW_CURSOR
      && !x_stretch_cursor_p)
    {
      /* If `x-stretch-block-cursor' is nil, don't draw a block cursor
	 as wide as the stretch glyph.  */
      int width, background_width = s->background_width;
      int x = s->x, left_x = window_box_left_offset (s->w, TEXT_AREA);

      if (x < left_x)
	{
	  background_width -= left_x - x;
	  x = left_x;
	}
      width = min (FRAME_COLUMN_WIDTH (s->f), background_width);

      /* Draw cursor.  */
      x_draw_glyph_string_bg_rect (s, x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < background_width)
	{
	  int y = s->y;
	  int w = background_width - width, h = s->height;
	  XRectangle r;
	  GC gc;

	  x += width;
	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      x_set_mouse_face_gc (s);
	      gc = s->gc;
	    }
	  else
	    gc = s->face->gc;

	  get_glyph_string_clip_rect (s, &r);
	  XSetClipRectangles (s->display, gc, 0, 0, &r, 1, Unsorted);

	  if (s->face->stipple)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (s->display, gc, FillOpaqueStippled);
	      XFillRectangle (s->display, s->window, gc, x, y, w, h);
	      XSetFillStyle (s->display, gc, FillSolid);
	    }
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, gc, GCForeground | GCBackground, &xgcv);
	      XSetForeground (s->display, gc, xgcv.background);
	      XFillRectangle (s->display, s->window, gc, x, y, w, h);
	      XSetForeground (s->display, gc, xgcv.foreground);
	    }
	}
    }
  else if (!s->background_filled_p)
    {
      int background_width = s->background_width;
      int x = s->x, left_x = window_box_left_offset (s->w, TEXT_AREA);

      /* Don't draw into left margin, fringe or scrollbar area
         except for header line and mode line.  */
      if (x < left_x && !s->row->mode_line_p)
	{
	  background_width -= left_x - x;
	  x = left_x;
	}
      if (background_width > 0)
	x_draw_glyph_string_bg_rect (s, x, s->y, background_width, s->height);
    }

  s->background_filled_p = 1;
}


/* Draw glyph string S.  */

static void
x_draw_glyph_string (s)
     struct glyph_string *s;
{
  int relief_drawn_p = 0;

  /* If S draws into the background of its successors, draw the
     background of the successors first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next; next;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
	  {
	    x_set_glyph_string_gc (next);
	    x_set_glyph_string_clipping (next);
	    x_draw_glyph_string_background (next, 1);
#ifdef USE_FONT_BACKEND
	    next->num_clips = 0;
#endif	/* USE_FONT_BACKEND */
	  }
    }

  /* Set up S->gc, set clipping and draw S.  */
  x_set_glyph_string_gc (s);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps
      && s->face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))

    {
      x_set_glyph_string_clipping (s);
      x_draw_glyph_string_background (s, 1);
      x_draw_glyph_string_box (s);
      x_set_glyph_string_clipping (s);
      relief_drawn_p = 1;
    }
  else if ((s->prev && s->prev->hl != s->hl && s->left_overhang)
	   || (s->next && s->next->hl != s->hl && s->right_overhang))
    /* We must clip just this glyph.  left_overhang part has already
       drawn when s->prev was drawn, and right_overhang part will be
       drawn later when s->next is drawn. */
    x_set_glyph_string_clipping_exactly (s, s);
  else
    x_set_glyph_string_clipping (s);

  switch (s->first_glyph->type)
    {
    case IMAGE_GLYPH:
      x_draw_image_glyph_string (s);
      break;

    case STRETCH_GLYPH:
      x_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = 1;
      else
	x_draw_glyph_string_background (s, 0);
      x_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      if (s->for_overlaps || s->gidx > 0)
	s->background_filled_p = 1;
      else
	x_draw_glyph_string_background (s, 1);
      x_draw_composite_glyph_string_foreground (s);
      break;

    default:
      abort ();
    }

  if (!s->for_overlaps)
    {
      /* Draw underline.  */
      if (s->face->underline_p)
	{
	  unsigned long tem, h;
	  int y;

	  /* Get the underline thickness.  Default is 1 pixel.  */
#ifdef USE_FONT_BACKEND
	  if (enable_font_backend)
	    /* In the future, we must use information of font.  */
	    h = 1;
	  else
#endif	/* USE_FONT_BACKEND */
	  if (!XGetFontProperty (s->font, XA_UNDERLINE_THICKNESS, &h))
	    h = 1;

#ifdef USE_FONT_BACKEND
	  if (enable_font_backend)
	    {
	      if (s->face->font)
		/* In the future, we must use information of font.  */
		y = s->ybase + (s->face->font->max_bounds.descent + 1) / 2;
	      else
		y = s->y + s->height - h;
	    }
	  else
#endif
	    {
	      y = s->y + s->height - h;
	      if (!x_underline_at_descent_line)
		{
		  /* Get the underline position.  This is the recommended
		     vertical offset in pixels from the baseline to the top of
		     the underline.  This is a signed value according to the
		     specs, and its default is

		     ROUND ((maximum descent) / 2), with
		     ROUND(x) = floor (x + 0.5)  */

		  if (x_use_underline_position_properties
		      && XGetFontProperty (s->font, XA_UNDERLINE_POSITION, &tem))
		    y = s->ybase + (long) tem;
		  else if (s->face->font)
		    y = s->ybase + (s->face->font->max_bounds.descent + 1) / 2;
		}
	    }

	  if (s->face->underline_defaulted_p)
	    XFillRectangle (s->display, s->window, s->gc,
			    s->x, y, s->background_width, h);
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
	      XSetForeground (s->display, s->gc, s->face->underline_color);
	      XFillRectangle (s->display, s->window, s->gc,
			      s->x, y, s->background_width, h);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}

      /* Draw overline.  */
      if (s->face->overline_p)
	{
	  unsigned long dy = 0, h = 1;

	  if (s->face->overline_color_defaulted_p)
	    XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			    s->background_width, h);
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
	      XSetForeground (s->display, s->gc, s->face->overline_color);
	      XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			      s->background_width, h);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}

      /* Draw strike-through.  */
      if (s->face->strike_through_p)
	{
	  unsigned long h = 1;
	  unsigned long dy = (s->height - h) / 2;

	  if (s->face->strike_through_color_defaulted_p)
	    XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			    s->width, h);
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
	      XSetForeground (s->display, s->gc, s->face->strike_through_color);
	      XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			      s->width, h);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}

      /* Draw relief if not yet drawn.  */
      if (!relief_drawn_p && s->face->box != FACE_NO_BOX)
	x_draw_glyph_string_box (s);

      if (s->prev)
	{
	  struct glyph_string *prev;

	  for (prev = s->prev; prev; prev = prev->prev)
	    if (prev->hl != s->hl
		&& prev->x + prev->width + prev->right_overhang > s->x)
	      {
		/* As prev was drawn while clipped to its own area, we
		   must draw the right_overhang part using s->hl now.  */
		enum draw_glyphs_face save = prev->hl;

		prev->hl = s->hl;
		x_set_glyph_string_gc (prev);
		x_set_glyph_string_clipping_exactly (s, prev);
		if (prev->first_glyph->type == CHAR_GLYPH)
		  x_draw_glyph_string_foreground (prev);
		else
		  x_draw_composite_glyph_string_foreground (prev);
		XSetClipMask (prev->display, prev->gc, None);
		prev->hl = save;
#ifdef USE_FONT_BACKEND
		prev->num_clips = 0;
#endif	/* USE_FONT_BACKEND */
	      }
	}

      if (s->next)
	{
	  struct glyph_string *next;

	  for (next = s->next; next; next = next->next)
	    if (next->hl != s->hl
		&& next->x - next->left_overhang < s->x + s->width)
	      {
		/* As next will be drawn while clipped to its own area,
		   we must draw the left_overhang part using s->hl now.  */
		enum draw_glyphs_face save = next->hl;

		next->hl = s->hl;
		x_set_glyph_string_gc (next);
		x_set_glyph_string_clipping_exactly (s, next);
		if (next->first_glyph->type == CHAR_GLYPH)
		  x_draw_glyph_string_foreground (next);
		else
		  x_draw_composite_glyph_string_foreground (next);
		XSetClipMask (next->display, next->gc, None);
		next->hl = save;
#ifdef USE_FONT_BACKEND
		next->num_clips = 0;
#endif	/* USE_FONT_BACKEND */
	      }
	}
    }

  /* Reset clipping.  */
  XSetClipMask (s->display, s->gc, None);
#ifdef USE_FONT_BACKEND
  s->num_clips = 0;
#endif	/* USE_FONT_BACKEND */
}

/* Shift display to make room for inserted glyphs.   */

void
x_shift_glyphs_for_insert (f, x, y, width, height, shift_by)
     struct frame *f;
     int x, y, width, height, shift_by;
{
  XCopyArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), FRAME_X_WINDOW (f),
	     f->output_data.x->normal_gc,
	     x, y, width, height,
	     x + shift_by, y);
}

/* Delete N glyphs at the nominal cursor position.  Not implemented
   for X frames.  */

static void
x_delete_glyphs (f, n)
     struct frame *f;
     register int n;
{
  abort ();
}


/* Like XClearArea, but check that WIDTH and HEIGHT are reasonable.
   If they are <= 0, this is probably an error.  */

void
x_clear_area (dpy, window, x, y, width, height, exposures)
     Display *dpy;
     Window window;
     int x, y;
     int width, height;
     int exposures;
{
  xassert (width > 0 && height > 0);
  XClearArea (dpy, window, x, y, width, height, exposures);
}


/* Clear an entire frame.  */

static void
x_clear_frame (struct frame *f)
{
  /* Clearing the frame will erase any cursor, so mark them all as no
     longer visible.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));
  output_cursor.hpos = output_cursor.vpos = 0;
  output_cursor.x = -1;

  /* We don't set the output cursor here because there will always
     follow an explicit cursor_to.  */
  BLOCK_INPUT;
  XClearWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));

  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  x_scroll_bar_clear (f);

  XFlush (FRAME_X_DISPLAY (f));

  UNBLOCK_INPUT;
}



/* Invert the middle quarter of the frame for .15 sec.  */

/* We use the select system call to do the waiting, so we have to make
   sure it's available.  If it isn't, we just won't do visual bells.  */

#if defined (HAVE_TIMEVAL) && defined (HAVE_SELECT)


/* Subtract the `struct timeval' values X and Y, storing the result in
   *RESULT.  Return 1 if the difference is negative, otherwise 0.  */

static int
timeval_subtract (result, x, y)
     struct timeval *result, x, y;
{
  /* Perform the carry for the later subtraction by updating y.  This
     is safer because on some systems the tv_sec member is unsigned.  */
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

  /* Compute the time remaining to wait.  tv_usec is certainly
     positive.  */
  result->tv_sec = x.tv_sec - y.tv_sec;
  result->tv_usec = x.tv_usec - y.tv_usec;

  /* Return indication of whether the result should be considered
     negative.  */
  return x.tv_sec < y.tv_sec;
}

void
XTflash (f)
     struct frame *f;
{
  BLOCK_INPUT;

  {
    GC gc;

    /* Create a GC that will use the GXxor function to flip foreground
       pixels into background pixels.  */
    {
      XGCValues values;

      values.function = GXxor;
      values.foreground = (FRAME_FOREGROUND_PIXEL (f)
			   ^ FRAME_BACKGROUND_PIXEL (f));

      gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		      GCFunction | GCForeground, &values);
    }

    {
      /* Get the height not including a menu bar widget.  */
      int height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, FRAME_LINES (f));
      /* Height of each line to flash.  */
      int flash_height = FRAME_LINE_HEIGHT (f);
      /* These will be the left and right margins of the rectangles.  */
      int flash_left = FRAME_INTERNAL_BORDER_WIDTH (f);
      int flash_right = FRAME_PIXEL_WIDTH (f) - FRAME_INTERNAL_BORDER_WIDTH (f);

      int width;

      /* Don't flash the area between a scroll bar and the frame
	 edge it is next to.  */
      switch (FRAME_VERTICAL_SCROLL_BAR_TYPE (f))
	{
	case vertical_scroll_bar_left:
	  flash_left += VERTICAL_SCROLL_BAR_WIDTH_TRIM;
	  break;

	case vertical_scroll_bar_right:
	  flash_right -= VERTICAL_SCROLL_BAR_WIDTH_TRIM;
	  break;

	default:
	  break;
	}

      width = flash_right - flash_left;

      /* If window is tall, flash top and bottom line.  */
      if (height > 3 * FRAME_LINE_HEIGHT (f))
	{
	  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
			  flash_left,
			  (FRAME_INTERNAL_BORDER_WIDTH (f)
			   + FRAME_TOOL_BAR_LINES (f) * FRAME_LINE_HEIGHT (f)),
			  width, flash_height);
	  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
			  flash_left,
			  (height - flash_height
			   - FRAME_INTERNAL_BORDER_WIDTH (f)),
			  width, flash_height);
	}
      else
	/* If it is short, flash it all.  */
	XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
			flash_left, FRAME_INTERNAL_BORDER_WIDTH (f),
			width, height - 2 * FRAME_INTERNAL_BORDER_WIDTH (f));

      x_flush (f);

      {
	struct timeval wakeup;

	EMACS_GET_TIME (wakeup);

	/* Compute time to wait until, propagating carry from usecs.  */
	wakeup.tv_usec += 150000;
	wakeup.tv_sec += (wakeup.tv_usec / 1000000);
	wakeup.tv_usec %= 1000000;

	/* Keep waiting until past the time wakeup or any input gets
	   available.  */
	while (! detect_input_pending ())
	  {
	    struct timeval current;
	    struct timeval timeout;

	    EMACS_GET_TIME (current);

	    /* Break if result would be negative.  */
	    if (timeval_subtract (&current, wakeup, current))
	      break;

	    /* How long `select' should wait.  */
	    timeout.tv_sec = 0;
	    timeout.tv_usec = 10000;

	    /* Try to wait that long--but we might wake up sooner.  */
	    select (0, NULL, NULL, NULL, &timeout);
	  }
      }

      /* If window is tall, flash top and bottom line.  */
      if (height > 3 * FRAME_LINE_HEIGHT (f))
	{
	  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
			  flash_left,
			  (FRAME_INTERNAL_BORDER_WIDTH (f)
			   + FRAME_TOOL_BAR_LINES (f) * FRAME_LINE_HEIGHT (f)),
			  width, flash_height);
	  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
			  flash_left,
			  (height - flash_height
			   - FRAME_INTERNAL_BORDER_WIDTH (f)),
			  width, flash_height);
	}
      else
	/* If it is short, flash it all.  */
	XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
			flash_left, FRAME_INTERNAL_BORDER_WIDTH (f),
			width, height - 2 * FRAME_INTERNAL_BORDER_WIDTH (f));

      XFreeGC (FRAME_X_DISPLAY (f), gc);
      x_flush (f);
    }
  }

  UNBLOCK_INPUT;
}

#endif /* defined (HAVE_TIMEVAL) && defined (HAVE_SELECT) */


/* Make audible bell.  */

void
XTring_bell ()
{
  struct frame *f = SELECTED_FRAME ();

  if (FRAME_X_DISPLAY (f))
    {
#if defined (HAVE_TIMEVAL) && defined (HAVE_SELECT)
      if (visible_bell)
	XTflash (f);
      else
#endif
	{
	  BLOCK_INPUT;
	  XBell (FRAME_X_DISPLAY (f), 0);
	  XFlush (FRAME_X_DISPLAY (f));
	  UNBLOCK_INPUT;
	}
    }
}


/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to x_update_begin and x_update_end.  */

static void
XTset_terminal_window (n)
     register int n;
{
  /* This function intentionally left blank.  */
}



/***********************************************************************
			      Line Dance
 ***********************************************************************/

/* Perform an insert-lines or delete-lines operation, inserting N
   lines or deleting -N lines at vertical position VPOS.  */

static void
x_ins_del_lines (f, vpos, n)
     struct frame *f;
     int vpos, n;
{
  abort ();
}


/* Scroll part of the display as described by RUN.  */

static void
x_scroll_run (w, run)
     struct window *w;
     struct run *run;
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;

  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the left and right
     fringe of W.  */
  window_box (w, -1, &x, &y, &width, &height);

  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->current_y);
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->desired_y);
  bottom_y = y + height;

  if (to_y < from_y)
    {
      /* Scrolling up.  Make sure we don't copy part of the mode
	 line at the bottom.  */
      if (from_y + run->height > bottom_y)
	height = bottom_y - from_y;
      else
	height = run->height;
    }
  else
    {
      /* Scolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
    }

  BLOCK_INPUT;

  /* Cursor off.  Will be switched on again in x_update_window_end.  */
  updated_window = w;
  x_clear_cursor (w);

  XCopyArea (FRAME_X_DISPLAY (f),
	     FRAME_X_WINDOW (f), FRAME_X_WINDOW (f),
	     f->output_data.x->normal_gc,
	     x, from_y,
	     width, height,
	     x, to_y);

  UNBLOCK_INPUT;
}



/***********************************************************************
			   Exposure Events
 ***********************************************************************/


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
		    f->output_data.x->border_pixel);
  UNBLOCK_INPUT;
  x_update_cursor (f, 1);
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
			  f->output_data.x->border_tile);
  UNBLOCK_INPUT;
  x_update_cursor (f, 1);
}

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the Lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occurred, so the
   Lisp code can tell when the switch took place by examining the events.  */

static void
x_new_focus_frame (dpyinfo, frame)
     struct x_display_info *dpyinfo;
     struct frame *frame;
{
  struct frame *old_focus = dpyinfo->x_focus_frame;

  if (frame != dpyinfo->x_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of x_focus_frame.  */
      dpyinfo->x_focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	x_lower_frame (old_focus);

#if 0
      selected_frame = frame;
      XSETFRAME (XWINDOW (selected_frame->selected_window)->frame,
		 selected_frame);
      Fselect_window (selected_frame->selected_window, Qnil);
      choose_minibuf_frame ();
#endif /* ! 0 */

      if (dpyinfo->x_focus_frame && dpyinfo->x_focus_frame->auto_raise)
	pending_autoraise_frame = dpyinfo->x_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  x_frame_rehighlight (dpyinfo);
}

/* Handle FocusIn and FocusOut state changes for FRAME.
   If FRAME has focus and there exists more than one frame, puts
   a FOCUS_IN_EVENT into *BUFP.  */

static void
x_focus_changed (type, state, dpyinfo, frame, bufp)
     int type;
     int state;
     struct x_display_info *dpyinfo;
     struct frame *frame;
     struct input_event *bufp;
{
  if (type == FocusIn)
    {
      if (dpyinfo->x_focus_event_frame != frame)
        {
          x_new_focus_frame (dpyinfo, frame);
          dpyinfo->x_focus_event_frame = frame;

          /* Don't stop displaying the initial startup message
             for a switch-frame event we don't need.  */
          if (NILP (Vterminal_frame)
              && CONSP (Vframe_list)
              && !NILP (XCDR (Vframe_list)))
            {
              bufp->kind = FOCUS_IN_EVENT;
              XSETFRAME (bufp->frame_or_window, frame);
            }
        }

      frame->output_data.x->focus_state |= state;

#ifdef HAVE_X_I18N
      if (FRAME_XIC (frame))
        XSetICFocus (FRAME_XIC (frame));
#endif
    }
  else if (type == FocusOut)
    {
      frame->output_data.x->focus_state &= ~state;

      if (dpyinfo->x_focus_event_frame == frame)
        {
          dpyinfo->x_focus_event_frame = 0;
          x_new_focus_frame (dpyinfo, 0);
        }

#ifdef HAVE_X_I18N
      if (FRAME_XIC (frame))
        XUnsetICFocus (FRAME_XIC (frame));
#endif
    }
}

/* The focus may have changed.  Figure out if it is a real focus change,
   by checking both FocusIn/Out and Enter/LeaveNotify events.

   Returns FOCUS_IN_EVENT event in *BUFP. */

static void
x_detect_focus_change (dpyinfo, event, bufp)
     struct x_display_info *dpyinfo;
     XEvent *event;
     struct input_event *bufp;
{
  struct frame *frame;

  frame = x_any_window_to_frame (dpyinfo, event->xany.window);
  if (! frame)
    return;

  switch (event->type)
    {
    case EnterNotify:
    case LeaveNotify:
      {
        struct frame *focus_frame = dpyinfo->x_focus_event_frame;
        int focus_state
          = focus_frame ? focus_frame->output_data.x->focus_state : 0;

        if (event->xcrossing.detail != NotifyInferior
            && event->xcrossing.focus
            && ! (focus_state & FOCUS_EXPLICIT))
          x_focus_changed ((event->type == EnterNotify ? FocusIn : FocusOut),
			   FOCUS_IMPLICIT,
			   dpyinfo, frame, bufp);
      }
      break;

    case FocusIn:
    case FocusOut:
      x_focus_changed (event->type,
		       (event->xfocus.detail == NotifyPointer ?
			FOCUS_IMPLICIT : FOCUS_EXPLICIT),
		       dpyinfo, frame, bufp);
      break;

    case ClientMessage:
      if (event->xclient.message_type == dpyinfo->Xatom_XEMBED)
	{
	  enum xembed_message msg = event->xclient.data.l[1];
	  x_focus_changed ((msg == XEMBED_FOCUS_IN ? FocusIn : FocusOut),
			   FOCUS_EXPLICIT, dpyinfo, frame, bufp);
	}
      break;
    }
}


/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
x_mouse_leave (dpyinfo)
     struct x_display_info *dpyinfo;
{
  x_new_focus_frame (dpyinfo, dpyinfo->x_focus_event_frame);
}

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   mini-buffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or un-highlighted; we only use it to find
   the appropriate X display info.  */

static void
XTframe_rehighlight (frame)
     struct frame *frame;
{
  x_frame_rehighlight (FRAME_X_DISPLAY_INFO (frame));
}

static void
x_frame_rehighlight (dpyinfo)
     struct x_display_info *dpyinfo;
{
  struct frame *old_highlight = dpyinfo->x_highlight_frame;

  if (dpyinfo->x_focus_frame)
    {
      dpyinfo->x_highlight_frame
	= ((FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame))
	   : dpyinfo->x_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->x_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame) = Qnil;
	  dpyinfo->x_highlight_frame = dpyinfo->x_focus_frame;
	}
    }
  else
    dpyinfo->x_highlight_frame = 0;

  if (dpyinfo->x_highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (dpyinfo->x_highlight_frame)
	frame_highlight (dpyinfo->x_highlight_frame);
    }
}



/* Keyboard processing - modifier keys, vendor-specific keysyms, etc.  */

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

  XDisplayKeycodes (dpyinfo->display, &min_code, &max_code);

  syms = XGetKeyboardMapping (dpyinfo->display,
			      min_code, max_code - min_code + 1,
			      &syms_per_code);
  mods = XGetModifierMapping (dpyinfo->display);

  /* Scan the modifier table to see which modifier bits the Meta and
     Alt keysyms are on.  */
  {
    int row, col;	/* The row and column in the modifier table.  */
    int found_alt_or_meta;

    for (row = 3; row < 8; row++)
    {
      found_alt_or_meta = 0;
      for (col = 0; col < mods->max_keypermod; col++)
	{
	  KeyCode code = mods->modifiermap[(row * mods->max_keypermod) + col];

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
		    found_alt_or_meta = 1;
		    dpyinfo->meta_mod_mask |= (1 << row);
		    break;

		  case XK_Alt_L:
		  case XK_Alt_R:
		    found_alt_or_meta = 1;
		    dpyinfo->alt_mod_mask |= (1 << row);
		    break;

		  case XK_Hyper_L:
		  case XK_Hyper_R:
		    if (!found_alt_or_meta)
		      dpyinfo->hyper_mod_mask |= (1 << row);
		    code_col = syms_per_code;
		    col = mods->max_keypermod;
		    break;

		  case XK_Super_L:
		  case XK_Super_R:
		    if (!found_alt_or_meta)
		      dpyinfo->super_mod_mask |= (1 << row);
		    code_col = syms_per_code;
		    col = mods->max_keypermod;
		    break;

		  case XK_Shift_Lock:
		    /* Ignore this if it's not on the lock modifier.  */
		    if (!found_alt_or_meta && ((1 << row) == LockMask))
		      dpyinfo->shift_lock_mask = LockMask;
		    code_col = syms_per_code;
		    col = mods->max_keypermod;
		    break;
		  }
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

unsigned int
x_x_to_emacs_modifiers (dpyinfo, state)
     struct x_display_info *dpyinfo;
     unsigned int state;
{
  EMACS_UINT mod_meta = meta_modifier;
  EMACS_UINT mod_alt  = alt_modifier;
  EMACS_UINT mod_hyper = hyper_modifier;
  EMACS_UINT mod_super = super_modifier;
  Lisp_Object tem;

  tem = Fget (Vx_alt_keysym, Qmodifier_value);
  if (! EQ (tem, Qnil)) mod_alt = XUINT (tem);
  tem = Fget (Vx_meta_keysym, Qmodifier_value);
  if (! EQ (tem, Qnil)) mod_meta = XUINT (tem);
  tem = Fget (Vx_hyper_keysym, Qmodifier_value);
  if (! EQ (tem, Qnil)) mod_hyper = XUINT (tem);
  tem = Fget (Vx_super_keysym, Qmodifier_value);
  if (! EQ (tem, Qnil)) mod_super = XUINT (tem);


  return (  ((state & (ShiftMask | dpyinfo->shift_lock_mask)) ? shift_modifier : 0)
            | ((state & ControlMask)			? ctrl_modifier	: 0)
            | ((state & dpyinfo->meta_mod_mask)		? mod_meta	: 0)
            | ((state & dpyinfo->alt_mod_mask)		? mod_alt	: 0)
            | ((state & dpyinfo->super_mod_mask)	? mod_super	: 0)
            | ((state & dpyinfo->hyper_mod_mask)	? mod_hyper	: 0));
}

static unsigned int
x_emacs_to_x_modifiers (dpyinfo, state)
     struct x_display_info *dpyinfo;
     unsigned int state;
{
  EMACS_UINT mod_meta = meta_modifier;
  EMACS_UINT mod_alt  = alt_modifier;
  EMACS_UINT mod_hyper = hyper_modifier;
  EMACS_UINT mod_super = super_modifier;

  Lisp_Object tem;

  tem = Fget (Vx_alt_keysym, Qmodifier_value);
  if (! EQ (tem, Qnil)) mod_alt = XUINT (tem);
  tem = Fget (Vx_meta_keysym, Qmodifier_value);
  if (! EQ (tem, Qnil)) mod_meta = XUINT (tem);
  tem = Fget (Vx_hyper_keysym, Qmodifier_value);
  if (! EQ (tem, Qnil)) mod_hyper = XUINT (tem);
  tem = Fget (Vx_super_keysym, Qmodifier_value);
  if (! EQ (tem, Qnil)) mod_super = XUINT (tem);


  return (  ((state & mod_alt)		? dpyinfo->alt_mod_mask   : 0)
            | ((state & mod_super)	? dpyinfo->super_mod_mask : 0)
            | ((state & mod_hyper)	? dpyinfo->hyper_mod_mask : 0)
            | ((state & shift_modifier)	? ShiftMask        : 0)
            | ((state & ctrl_modifier)	? ControlMask      : 0)
            | ((state & mod_meta)	? dpyinfo->meta_mod_mask  : 0));
}

/* Convert a keysym to its name.  */

char *
x_get_keysym_name (keysym)
     KeySym keysym;
{
  char *value;

  BLOCK_INPUT;
  value = XKeysymToString (keysym);
  UNBLOCK_INPUT;

  return value;
}



/* Mouse clicks and mouse movement.  Rah.  */

/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static Lisp_Object
construct_mouse_click (result, event, f)
     struct input_event *result;
     XButtonEvent *event;
     struct frame *f;
{
  /* Make the event type NO_EVENT; we'll change that when we decide
     otherwise.  */
  result->kind = MOUSE_CLICK_EVENT;
  result->code = event->button - Button1;
  result->timestamp = event->time;
  result->modifiers = (x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f),
					       event->state)
		       | (event->type == ButtonRelease
			  ? up_modifier
			  : down_modifier));

  XSETINT (result->x, event->x);
  XSETINT (result->y, event->y);
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}


/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static XMotionEvent last_mouse_motion_event;
static Lisp_Object last_mouse_motion_frame;

static int
note_mouse_movement (frame, event)
     FRAME_PTR frame;
     XMotionEvent *event;
{
  last_mouse_movement_time = event->time;
  last_mouse_motion_event = *event;
  XSETFRAME (last_mouse_motion_frame, frame);

  if (!FRAME_X_OUTPUT (frame))
    return 0;

  if (event->window != FRAME_X_WINDOW (frame))
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, -1, -1);
      last_mouse_glyph_frame = 0;
      return 1;
    }


  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  if (frame != last_mouse_glyph_frame
      || event->x < last_mouse_glyph.x
      || event->x >= last_mouse_glyph.x + last_mouse_glyph.width
      || event->y < last_mouse_glyph.y
      || event->y >= last_mouse_glyph.y + last_mouse_glyph.height)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, event->x, event->y);
      /* Remember which glyph we're now on.  */
      remember_mouse_glyph (frame, event->x, event->y, &last_mouse_glyph);
      last_mouse_glyph_frame = frame;
      return 1;
    }

  return 0;
}


/************************************************************************
			      Mouse Face
 ************************************************************************/

static void
redo_mouse_highlight ()
{
  if (!NILP (last_mouse_motion_frame)
      && FRAME_LIVE_P (XFRAME (last_mouse_motion_frame)))
    note_mouse_highlight (XFRAME (last_mouse_motion_frame),
			  last_mouse_motion_event.x,
			  last_mouse_motion_event.y);
}



/* Return the current position of the mouse.
   *FP should be a frame which indicates which display to ask about.

   If the mouse movement started in a scroll bar, set *FP, *BAR_WINDOW,
   and *PART to the frame, window, and scroll bar part that the mouse
   is over.  Set *X and *Y to the portion and whole of the mouse's
   position on the scroll bar.

   If the mouse movement started elsewhere, set *FP to the frame the
   mouse is on, *BAR_WINDOW to nil, and *X and *Y to the character cell
   the mouse is over.

   Set *TIME to the server time-stamp for the time at which the mouse
   was at this position.

   Don't store anything if we don't have a valid set of values to report.

   This clears the mouse_moved flag, so we can wait for the next mouse
   movement.  */

static void
XTmouse_position (fp, insist, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     int insist;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  FRAME_PTR f1;

  BLOCK_INPUT;

  if (! NILP (last_mouse_scroll_bar) && insist == 0)
    x_scroll_bar_report_motion (fp, bar_window, part, x, y, time);
  else
    {
      Window root;
      int root_x, root_y;

      Window dummy_window;
      int dummy;

      Lisp_Object frame, tail;

      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
	if (FRAME_X_P (XFRAME (frame))
            && FRAME_X_DISPLAY (XFRAME (frame)) == FRAME_X_DISPLAY (*fp))
	  XFRAME (frame)->mouse_moved = 0;

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
	int parent_x = 0, parent_y = 0;

	win = root;

	/* XTranslateCoordinates can get errors if the window
	   structure is changing at the same time this function
	   is running.  So at least we must not crash from them.  */

	x_catch_errors (FRAME_X_DISPLAY (*fp));

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

		if (child == None || child == win)
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
	    f1 = x_any_window_to_frame (FRAME_X_DISPLAY_INFO (*fp), win);

#ifdef USE_X_TOOLKIT
	    /* If we end up with the menu bar window, say it's not
	       on the frame.  */
	    if (f1 != NULL
		&& f1->output_data.x->menubar_widget
		&& win == XtWindow (f1->output_data.x->menubar_widget))
	      f1 = NULL;
#endif /* USE_X_TOOLKIT */
	  }

	if (x_had_errors_p (FRAME_X_DISPLAY (*fp)))
	  f1 = 0;

	x_uncatch_errors ();

	/* If not, is it one of our scroll bars?  */
	if (! f1)
	  {
	    struct scroll_bar *bar;

            bar = x_window_to_scroll_bar (FRAME_X_DISPLAY (*fp), win);

	    if (bar)
	      {
		f1 = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
		win_x = parent_x;
		win_y = parent_y;
	      }
	  }

	if (f1 == 0 && insist > 0)
	  f1 = SELECTED_FRAME ();

	if (f1)
	  {
	    /* Ok, we found a frame.  Store all the values.
	       last_mouse_glyph is a rectangle used to reduce the
	       generation of mouse events.  To not miss any motion
	       events, we must divide the frame into rectangles of the
	       size of the smallest character that could be displayed
	       on it, i.e. into the same rectangles that matrices on
	       the frame are divided into.  */

	    remember_mouse_glyph (f1, win_x, win_y, &last_mouse_glyph);
	    last_mouse_glyph_frame = f1;

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



/***********************************************************************
			       Scroll bars
 ***********************************************************************/

/* Scroll bar support.  */

/* Given an X window ID and a DISPLAY, find the struct scroll_bar which
   manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */

static struct scroll_bar *
x_window_to_scroll_bar (display, window_id)
     Display *display;
     Window window_id;
{
  Lisp_Object tail;

#if defined (USE_GTK) && defined (USE_TOOLKIT_SCROLL_BARS)
  window_id = (Window) xg_get_scroll_id_for_window (display, window_id);
#endif /* USE_GTK  && USE_TOOLKIT_SCROLL_BARS */

  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object frame, bar, condemned;

      frame = XCAR (tail);
      /* All elements of Vframe_list should be frames.  */
      if (! FRAMEP (frame))
	abort ();

      if (! FRAME_X_P (XFRAME (frame)))
        continue;
      
      /* Scan this frame's scroll bar list for a scroll bar with the
         right window ID.  */
      condemned = FRAME_CONDEMNED_SCROLL_BARS (XFRAME (frame));
      for (bar = FRAME_SCROLL_BARS (XFRAME (frame));
	   /* This trick allows us to search both the ordinary and
              condemned scroll bar lists with one loop.  */
	   ! NILP (bar) || (bar = condemned,
			       condemned = Qnil,
			       ! NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	if (XSCROLL_BAR (bar)->x_window == window_id &&
            FRAME_X_DISPLAY (XFRAME (frame)) == display)
	  return XSCROLL_BAR (bar);
    }

  return 0;
}


#if defined USE_LUCID

/* Return the Lucid menu bar WINDOW is part of.  Return null
   if WINDOW is not part of a menu bar.  */

static Widget
x_window_to_menu_bar (window)
     Window window;
{
  Lisp_Object tail;

  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      if (FRAME_X_P (XFRAME (XCAR (tail))))
        {
          Lisp_Object frame = XCAR (tail);
          Widget menu_bar = XFRAME (frame)->output_data.x->menubar_widget;

          if (menu_bar && xlwmenu_window_p (menu_bar, window))
            return menu_bar;
        }
    }

  return NULL;
}

#endif /* USE_LUCID */


/************************************************************************
			 Toolkit scroll bars
 ************************************************************************/

#ifdef USE_TOOLKIT_SCROLL_BARS

static void x_scroll_bar_to_input_event P_ ((XEvent *, struct input_event *));
static void x_send_scroll_bar_event P_ ((Lisp_Object, int, int, int));
static void x_create_toolkit_scroll_bar P_ ((struct frame *,
					     struct scroll_bar *));
static void x_set_toolkit_scroll_bar_thumb P_ ((struct scroll_bar *,
						int, int, int));


/* Lisp window being scrolled.  Set when starting to interact with
   a toolkit scroll bar, reset to nil when ending the interaction.  */

static Lisp_Object window_being_scrolled;

/* Last scroll bar part sent in xm_scroll_callback.  */

static int last_scroll_bar_part;

/* Whether this is an Xaw with arrow-scrollbars.  This should imply
   that movements of 1/20 of the screen size are mapped to up/down.  */

#ifndef USE_GTK
/* Id of action hook installed for scroll bars.  */

static XtActionHookId action_hook_id;

static Boolean xaw3d_arrow_scroll;

/* Whether the drag scrolling maintains the mouse at the top of the
   thumb.  If not, resizing the thumb needs to be done more carefully
   to avoid jerkyness.  */

static Boolean xaw3d_pick_top;

/* Action hook installed via XtAppAddActionHook when toolkit scroll
   bars are used..  The hook is responsible for detecting when
   the user ends an interaction with the scroll bar, and generates
   a `end-scroll' SCROLL_BAR_CLICK_EVENT' event if so.  */

static void
xt_action_hook (widget, client_data, action_name, event, params,
		num_params)
     Widget widget;
     XtPointer client_data;
     String action_name;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
  int scroll_bar_p;
  char *end_action;

#ifdef USE_MOTIF
  scroll_bar_p = XmIsScrollBar (widget);
  end_action = "Release";
#else /* !USE_MOTIF i.e. use Xaw */
  scroll_bar_p = XtIsSubclass (widget, scrollbarWidgetClass);
  end_action = "EndScroll";
#endif /* USE_MOTIF */

  if (scroll_bar_p
      && strcmp (action_name, end_action) == 0
      && WINDOWP (window_being_scrolled))
    {
      struct window *w;

      x_send_scroll_bar_event (window_being_scrolled,
			       scroll_bar_end_scroll, 0, 0);
      w = XWINDOW (window_being_scrolled);

      if (!NILP (XSCROLL_BAR (w->vertical_scroll_bar)->dragging))
	{
	  XSCROLL_BAR (w->vertical_scroll_bar)->dragging = Qnil;
	  /* The thumb size is incorrect while dragging: fix it.  */
	  set_vertical_scroll_bar (w);
	}
      window_being_scrolled = Qnil;
      last_scroll_bar_part = -1;

      /* Xt timeouts no longer needed.  */
      toolkit_scroll_bar_interaction = 0;
    }
}
#endif /* not USE_GTK */

/* A vector of windows used for communication between
   x_send_scroll_bar_event and x_scroll_bar_to_input_event.  */

static struct window **scroll_bar_windows;
static int scroll_bar_windows_size;


/* Send a client message with message type Xatom_Scrollbar for a
   scroll action to the frame of WINDOW.  PART is a value identifying
   the part of the scroll bar that was clicked on.  PORTION is the
   amount to scroll of a whole of WHOLE.  */

static void
x_send_scroll_bar_event (window, part, portion, whole)
     Lisp_Object window;
     int part, portion, whole;
{
  XEvent event;
  XClientMessageEvent *ev = (XClientMessageEvent *) &event;
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  int i;

  BLOCK_INPUT;

  /* Construct a ClientMessage event to send to the frame.  */
  ev->type = ClientMessage;
  ev->message_type = FRAME_X_DISPLAY_INFO (f)->Xatom_Scrollbar;
  ev->display = FRAME_X_DISPLAY (f);
  ev->window = FRAME_X_WINDOW (f);
  ev->format = 32;

  /* We can only transfer 32 bits in the XClientMessageEvent, which is
     not enough to store a pointer or Lisp_Object on a 64 bit system.
     So, store the window in scroll_bar_windows and pass the index
     into that array in the event.  */
  for (i = 0; i < scroll_bar_windows_size; ++i)
    if (scroll_bar_windows[i] == NULL)
      break;

  if (i == scroll_bar_windows_size)
    {
      int new_size = max (10, 2 * scroll_bar_windows_size);
      size_t nbytes = new_size * sizeof *scroll_bar_windows;
      size_t old_nbytes = scroll_bar_windows_size * sizeof *scroll_bar_windows;

      scroll_bar_windows = (struct window **) xrealloc (scroll_bar_windows,
							nbytes);
      bzero (&scroll_bar_windows[i], nbytes - old_nbytes);
      scroll_bar_windows_size = new_size;
    }

  scroll_bar_windows[i] = w;
  ev->data.l[0] = (long) i;
  ev->data.l[1] = (long) part;
  ev->data.l[2] = (long) 0;
  ev->data.l[3] = (long) portion;
  ev->data.l[4] = (long) whole;

  /* Make Xt timeouts work while the scroll bar is active.  */
  toolkit_scroll_bar_interaction = 1;
#ifdef USE_X_TOOLKIT
  x_activate_timeout_atimer ();
#endif

  /* Setting the event mask to zero means that the message will
     be sent to the client that created the window, and if that
     window no longer exists, no event will be sent.  */
  XSendEvent (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), False, 0, &event);
  UNBLOCK_INPUT;
}


/* Transform a scroll bar ClientMessage EVENT to an Emacs input event
   in *IEVENT.  */

static void
x_scroll_bar_to_input_event (event, ievent)
     XEvent *event;
     struct input_event *ievent;
{
  XClientMessageEvent *ev = (XClientMessageEvent *) event;
  Lisp_Object window;
  struct frame *f;
  struct window *w;

  w = scroll_bar_windows[ev->data.l[0]];
  scroll_bar_windows[ev->data.l[0]] = NULL;

  XSETWINDOW (window, w);
  f = XFRAME (w->frame);

  ievent->kind = SCROLL_BAR_CLICK_EVENT;
  ievent->frame_or_window = window;
  ievent->arg = Qnil;
#ifdef USE_GTK
  ievent->timestamp = CurrentTime;
#else
  ievent->timestamp = XtLastTimestampProcessed (FRAME_X_DISPLAY (f));
#endif
  ievent->part = ev->data.l[1];
  ievent->code = ev->data.l[2];
  ievent->x = make_number ((int) ev->data.l[3]);
  ievent->y = make_number ((int) ev->data.l[4]);
  ievent->modifiers = 0;
}


#ifdef USE_MOTIF

/* Minimum and maximum values used for Motif scroll bars.  */

#define XM_SB_MAX 10000000


/* Scroll bar callback for Motif scroll bars.  WIDGET is the scroll
   bar widget.  CLIENT_DATA is a pointer to the scroll_bar structure.
   CALL_DATA is a pointer to a XmScrollBarCallbackStruct.  */

static void
xm_scroll_callback (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  struct scroll_bar *bar = (struct scroll_bar *) client_data;
  XmScrollBarCallbackStruct *cs = (XmScrollBarCallbackStruct *) call_data;
  int part = -1, whole = 0, portion = 0;

  switch (cs->reason)
    {
    case XmCR_DECREMENT:
      bar->dragging = Qnil;
      part = scroll_bar_up_arrow;
      break;

    case XmCR_INCREMENT:
      bar->dragging = Qnil;
      part = scroll_bar_down_arrow;
      break;

    case XmCR_PAGE_DECREMENT:
      bar->dragging = Qnil;
      part = scroll_bar_above_handle;
      break;

    case XmCR_PAGE_INCREMENT:
      bar->dragging = Qnil;
      part = scroll_bar_below_handle;
      break;

    case XmCR_TO_TOP:
      bar->dragging = Qnil;
      part = scroll_bar_to_top;
      break;

    case XmCR_TO_BOTTOM:
      bar->dragging = Qnil;
      part = scroll_bar_to_bottom;
      break;

    case XmCR_DRAG:
      {
	int slider_size;

	/* Get the slider size.  */
	BLOCK_INPUT;
	XtVaGetValues (widget, XmNsliderSize, &slider_size, NULL);
	UNBLOCK_INPUT;

	whole = XM_SB_MAX - slider_size;
	portion = min (cs->value, whole);
	part = scroll_bar_handle;
	bar->dragging = make_number (cs->value);
      }
      break;

    case XmCR_VALUE_CHANGED:
      break;
    };

  if (part >= 0)
    {
      window_being_scrolled = bar->window;
      last_scroll_bar_part = part;
      x_send_scroll_bar_event (bar->window, part, portion, whole);
    }
}

#elif defined USE_GTK

/* Scroll bar callback for GTK scroll bars.  WIDGET is the scroll
   bar widget.  DATA is a pointer to the scroll_bar structure. */

static void
xg_scroll_callback (widget, data)
     GtkRange *widget;
     gpointer data;
{
  struct scroll_bar *bar = (struct scroll_bar *) data;
  gdouble previous;
  gdouble position;
  gdouble *p;
  int diff;

  int part = -1, whole = 0, portion = 0;
  GtkAdjustment *adj = GTK_ADJUSTMENT (gtk_range_get_adjustment (widget));

  position = gtk_adjustment_get_value (adj);

  p = g_object_get_data (G_OBJECT (widget), XG_LAST_SB_DATA);
  if (! p)
    {
      p = (gdouble*) xmalloc (sizeof (gdouble));
      *p = XG_SB_MIN;
      g_object_set_data (G_OBJECT (widget), XG_LAST_SB_DATA, p);
    }

  previous = *p;
  *p = position;

  if (xg_ignore_gtk_scrollbar) return;

  diff = (int) (position - previous);

  if (diff == (int) adj->step_increment)
    {
      part = scroll_bar_down_arrow;
      bar->dragging = Qnil;
    }
  else if (-diff == (int) adj->step_increment)
    {
      part = scroll_bar_up_arrow;
      bar->dragging = Qnil;
    }
  else if (diff == (int) adj->page_increment)
    {
      part = scroll_bar_below_handle;
      bar->dragging = Qnil;
    }
  else if (-diff == (int) adj->page_increment)
    {
      part = scroll_bar_above_handle;
      bar->dragging = Qnil;
    }
  else
    {
      part = scroll_bar_handle;
      whole = adj->upper - adj->page_size;
      portion = min ((int)position, whole);
      bar->dragging = make_number ((int)portion);
    }

  if (part >= 0)
    {
      window_being_scrolled = bar->window;
      last_scroll_bar_part = part;
      x_send_scroll_bar_event (bar->window, part, portion, whole);
    }
}

#else /* not USE_GTK and not USE_MOTIF */

/* Xaw scroll bar callback.  Invoked when the thumb is dragged.
   WIDGET is the scroll bar widget.  CLIENT_DATA is a pointer to the
   scroll bar struct.  CALL_DATA is a pointer to a float saying where
   the thumb is.  */

static void
xaw_jump_callback (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  struct scroll_bar *bar = (struct scroll_bar *) client_data;
  float top = *(float *) call_data;
  float shown;
  int whole, portion, height;
  int part;

  /* Get the size of the thumb, a value between 0 and 1.  */
  BLOCK_INPUT;
  XtVaGetValues (widget, XtNshown, &shown, XtNheight, &height, NULL);
  UNBLOCK_INPUT;

  whole = 10000000;
  portion = shown < 1 ? top * whole : 0;

  if (shown < 1 && (eabs (top + shown - 1) < 1.0/height))
    /* Some derivatives of Xaw refuse to shrink the thumb when you reach
       the bottom, so we force the scrolling whenever we see that we're
       too close to the bottom (in x_set_toolkit_scroll_bar_thumb
       we try to ensure that we always stay two pixels away from the
       bottom).  */
    part = scroll_bar_down_arrow;
  else
    part = scroll_bar_handle;

  window_being_scrolled = bar->window;
  bar->dragging = make_number (portion);
  last_scroll_bar_part = part;
  x_send_scroll_bar_event (bar->window, part, portion, whole);
}


/* Xaw scroll bar callback.  Invoked for incremental scrolling.,
   i.e. line or page up or down.  WIDGET is the Xaw scroll bar
   widget.  CLIENT_DATA is a pointer to the scroll_bar structure for
   the scroll bar.  CALL_DATA is an integer specifying the action that
   has taken place.  Its magnitude is in the range 0..height of the
   scroll bar.  Negative values mean scroll towards buffer start.
   Values < height of scroll bar mean line-wise movement.  */

static void
xaw_scroll_callback (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  struct scroll_bar *bar = (struct scroll_bar *) client_data;
  /* The position really is stored cast to a pointer.  */
  int position = (long) call_data;
  Dimension height;
  int part;

  /* Get the height of the scroll bar.  */
  BLOCK_INPUT;
  XtVaGetValues (widget, XtNheight, &height, NULL);
  UNBLOCK_INPUT;

  if (eabs (position) >= height)
    part = (position < 0) ? scroll_bar_above_handle : scroll_bar_below_handle;

  /* If Xaw3d was compiled with ARROW_SCROLLBAR,
     it maps line-movement to call_data = max(5, height/20).  */
  else if (xaw3d_arrow_scroll && eabs (position) <= max (5, height / 20))
    part = (position < 0) ? scroll_bar_up_arrow : scroll_bar_down_arrow;
  else
    part = scroll_bar_move_ratio;

  window_being_scrolled = bar->window;
  bar->dragging = Qnil;
  last_scroll_bar_part = part;
  x_send_scroll_bar_event (bar->window, part, position, height);
}

#endif /* not USE_GTK and not USE_MOTIF */

#define SCROLL_BAR_NAME "verticalScrollBar"

/* Create the widget for scroll bar BAR on frame F.  Record the widget
   and X window of the scroll bar in BAR.  */

#ifdef USE_GTK
static void
x_create_toolkit_scroll_bar (f, bar)
     struct frame *f;
     struct scroll_bar *bar;
{
  char *scroll_bar_name = SCROLL_BAR_NAME;

  BLOCK_INPUT;
  xg_create_scroll_bar (f, bar, G_CALLBACK (xg_scroll_callback),
                        scroll_bar_name);
  UNBLOCK_INPUT;
}

#else /* not USE_GTK */

static void
x_create_toolkit_scroll_bar (f, bar)
     struct frame *f;
     struct scroll_bar *bar;
{
  Window xwindow;
  Widget widget;
  Arg av[20];
  int ac = 0;
  char *scroll_bar_name = SCROLL_BAR_NAME;
  unsigned long pixel;

  BLOCK_INPUT;

#ifdef USE_MOTIF
  /* Set resources.  Create the widget.  */
  XtSetArg (av[ac], XtNmappedWhenManaged, False); ++ac;
  XtSetArg (av[ac], XmNminimum, 0); ++ac;
  XtSetArg (av[ac], XmNmaximum, XM_SB_MAX); ++ac;
  XtSetArg (av[ac], XmNorientation, XmVERTICAL); ++ac;
  XtSetArg (av[ac], XmNprocessingDirection, XmMAX_ON_BOTTOM), ++ac;
  XtSetArg (av[ac], XmNincrement, 1); ++ac;
  XtSetArg (av[ac], XmNpageIncrement, 1); ++ac;

  pixel = f->output_data.x->scroll_bar_foreground_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XmNforeground, pixel);
      ++ac;
    }

  pixel = f->output_data.x->scroll_bar_background_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XmNbackground, pixel);
      ++ac;
    }

  widget = XmCreateScrollBar (f->output_data.x->edit_widget,
			      scroll_bar_name, av, ac);

  /* Add one callback for everything that can happen.  */
  XtAddCallback (widget, XmNdecrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNdragCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNincrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNpageDecrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNpageIncrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNtoBottomCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNtoTopCallback, xm_scroll_callback,
		 (XtPointer) bar);

  /* Realize the widget.  Only after that is the X window created.  */
  XtRealizeWidget (widget);

  /* Set the cursor to an arrow.  I didn't find a resource to do that.
     And I'm wondering why it hasn't an arrow cursor by default.  */
  XDefineCursor (XtDisplay (widget), XtWindow (widget),
		 f->output_data.x->nontext_cursor);

#else /* !USE_MOTIF i.e. use Xaw */

  /* Set resources.  Create the widget.  The background of the
     Xaw3d scroll bar widget is a little bit light for my taste.
     We don't alter it here to let users change it according
     to their taste with `emacs*verticalScrollBar.background: xxx'.  */
  XtSetArg (av[ac], XtNmappedWhenManaged, False); ++ac;
  XtSetArg (av[ac], XtNorientation, XtorientVertical); ++ac;
  /* For smoother scrolling with Xaw3d   -sm */
  /* XtSetArg (av[ac], XtNpickTop, True); ++ac; */

  pixel = f->output_data.x->scroll_bar_foreground_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XtNforeground, pixel);
      ++ac;
    }

  pixel = f->output_data.x->scroll_bar_background_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XtNbackground, pixel);
      ++ac;
    }

  /* Top/bottom shadow colors.  */

  /* Allocate them, if necessary.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel == -1)
    {
      pixel = f->output_data.x->scroll_bar_background_pixel;
      if (!x_alloc_lighter_color (f, FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
				  &pixel, 1.2, 0x8000))
	pixel = -1;
      f->output_data.x->scroll_bar_top_shadow_pixel = pixel;
    }
  if (f->output_data.x->scroll_bar_bottom_shadow_pixel == -1)
    {
      pixel = f->output_data.x->scroll_bar_background_pixel;
      if (!x_alloc_lighter_color (f, FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
				  &pixel, 0.6, 0x4000))
	pixel = -1;
      f->output_data.x->scroll_bar_bottom_shadow_pixel = pixel;
    }

#ifdef XtNbeNiceToColormap
  /* Tell the toolkit about them.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel == -1
      || f->output_data.x->scroll_bar_bottom_shadow_pixel == -1)
    /* We tried to allocate a color for the top/bottom shadow, and
       failed, so tell Xaw3d to use dithering instead.   */
    {
      XtSetArg (av[ac], XtNbeNiceToColormap, True);
      ++ac;
    }
  else
    /* Tell what colors Xaw3d should use for the top/bottom shadow, to
       be more consistent with other emacs 3d colors, and since Xaw3d is
       not good at dealing with allocation failure.  */
    {
      /* This tells Xaw3d to use real colors instead of dithering for
	 the shadows.  */
      XtSetArg (av[ac], XtNbeNiceToColormap, False);
      ++ac;

      /* Specify the colors.  */
      pixel = f->output_data.x->scroll_bar_top_shadow_pixel;
      if (pixel != -1)
	{
	  XtSetArg (av[ac], XtNtopShadowPixel, pixel);
	  ++ac;
	}
      pixel = f->output_data.x->scroll_bar_bottom_shadow_pixel;
      if (pixel != -1)
	{
	  XtSetArg (av[ac], XtNbottomShadowPixel, pixel);
	  ++ac;
	}
    }
#endif

  widget = XtCreateWidget (scroll_bar_name, scrollbarWidgetClass,
			   f->output_data.x->edit_widget, av, ac);

  {
    char *initial = "";
    char *val = initial;
    XtVaGetValues (widget, XtNscrollVCursor, (XtPointer) &val,
#ifdef XtNarrowScrollbars
		   XtNarrowScrollbars, (XtPointer) &xaw3d_arrow_scroll,
#endif
		   XtNpickTop, (XtPointer) &xaw3d_pick_top, NULL);
    if (xaw3d_arrow_scroll || val == initial)
      {	/* ARROW_SCROLL */
	xaw3d_arrow_scroll = True;
	/* Isn't that just a personal preference ?   --Stef */
	XtVaSetValues (widget, XtNcursorName, "top_left_arrow", NULL);
      }
  }

  /* Define callbacks.  */
  XtAddCallback (widget, XtNjumpProc, xaw_jump_callback, (XtPointer) bar);
  XtAddCallback (widget, XtNscrollProc, xaw_scroll_callback,
		 (XtPointer) bar);

  /* Realize the widget.  Only after that is the X window created.  */
  XtRealizeWidget (widget);

#endif /* !USE_MOTIF */

  /* Install an action hook that lets us detect when the user
     finishes interacting with a scroll bar.  */
  if (action_hook_id == 0)
    action_hook_id = XtAppAddActionHook (Xt_app_con, xt_action_hook, 0);

  /* Remember X window and widget in the scroll bar vector.  */
  SET_SCROLL_BAR_X_WIDGET (bar, widget);
  xwindow = XtWindow (widget);
  bar->x_window = xwindow;

  UNBLOCK_INPUT;
}
#endif /* not USE_GTK */


/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

#ifdef USE_GTK
static void
x_set_toolkit_scroll_bar_thumb (bar, portion, position, whole)
     struct scroll_bar *bar;
     int portion, position, whole;
{
  xg_set_toolkit_scroll_bar_thumb (bar, portion, position, whole);
}

#else /* not USE_GTK */
static void
x_set_toolkit_scroll_bar_thumb (bar, portion, position, whole)
     struct scroll_bar *bar;
     int portion, position, whole;
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  Widget widget = SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar);
  float top, shown;

  BLOCK_INPUT;

#ifdef USE_MOTIF

  /* We use an estimate of 30 chars per line rather than the real
     `portion' value.  This has the disadvantage that the thumb size
     is not very representative, but it makes our life a lot easier.
     Otherwise, we have to constantly adjust the thumb size, which
     we can't always do quickly enough: while dragging, the size of
     the thumb might prevent the user from dragging the thumb all the
     way to the end.  but Motif and some versions of Xaw3d don't allow
     updating the thumb size while dragging.  Also, even if we can update
     its size, the update will often happen too late.
     If you don't believe it, check out revision 1.650 of xterm.c to see
     what hoops we were going through and the still poor behavior we got.  */
  portion = WINDOW_TOTAL_LINES (XWINDOW (bar->window)) * 30;
  /* When the thumb is at the bottom, position == whole.
     So we need to increase `whole' to make space for the thumb.  */
  whole += portion;

  if (whole <= 0)
    top = 0, shown = 1;
  else
    {
      top = (float) position / whole;
      shown = (float) portion / whole;
    }

  if (NILP (bar->dragging))
    {
      int size, value;

      /* Slider size.  Must be in the range [1 .. MAX - MIN] where MAX
         is the scroll bar's maximum and MIN is the scroll bar's minimum
	 value.  */
      size = shown * XM_SB_MAX;
      size = min (size, XM_SB_MAX);
      size = max (size, 1);

      /* Position.  Must be in the range [MIN .. MAX - SLIDER_SIZE].  */
      value = top * XM_SB_MAX;
      value = min (value, XM_SB_MAX - size);

      XmScrollBarSetValues (widget, value, size, 0, 0, False);
    }
#else /* !USE_MOTIF i.e. use Xaw */

  if (whole == 0)
    top = 0, shown = 1;
  else
    {
      top = (float) position / whole;
      shown = (float) portion / whole;
    }

  {
    float old_top, old_shown;
    Dimension height;
    XtVaGetValues (widget,
		   XtNtopOfThumb, &old_top,
		   XtNshown, &old_shown,
		   XtNheight, &height,
		   NULL);

    /* Massage the top+shown values.  */
    if (NILP (bar->dragging) || last_scroll_bar_part == scroll_bar_down_arrow)
      top = max (0, min (1, top));
    else
      top = old_top;
    /* Keep two pixels available for moving the thumb down.  */
    shown = max (0, min (1 - top - (2.0 / height), shown));

    /* If the call to XawScrollbarSetThumb below doesn't seem to work,
       check that your system's configuration file contains a define
       for `NARROWPROTO'.  See s/freebsd.h for an example.  */
    if (top != old_top || shown != old_shown)
      {
	if (NILP (bar->dragging))
	  XawScrollbarSetThumb (widget, top, shown);
	else
	  {
	    /* Try to make the scrolling a tad smoother.  */
	    if (!xaw3d_pick_top)
	      shown = min (shown, old_shown);

	    XawScrollbarSetThumb (widget, top, shown);
	  }
      }
  }
#endif /* !USE_MOTIF */

  UNBLOCK_INPUT;
}
#endif /* not USE_GTK */

#endif /* USE_TOOLKIT_SCROLL_BARS */



/************************************************************************
			 Scroll bars, general
 ************************************************************************/

/* Create a scroll bar and return the scroll bar vector for it.  W is
   the Emacs window on which to create the scroll bar. TOP, LEFT,
   WIDTH and HEIGHT are the pixel coordinates and dimensions of the
   scroll bar. */

static struct scroll_bar *
x_scroll_bar_create (w, top, left, width, height)
     struct window *w;
     int top, left, width, height;
{
  struct frame *f = XFRAME (w->frame);
  struct scroll_bar *bar
    = ALLOCATE_PSEUDOVECTOR (struct scroll_bar, x_window, PVEC_OTHER);

  BLOCK_INPUT;

#ifdef USE_TOOLKIT_SCROLL_BARS
  x_create_toolkit_scroll_bar (f, bar);
#else /* not USE_TOOLKIT_SCROLL_BARS */
  {
    XSetWindowAttributes a;
    unsigned long mask;
    Window window;

    a.background_pixel = f->output_data.x->scroll_bar_background_pixel;
    if (a.background_pixel == -1)
      a.background_pixel = FRAME_BACKGROUND_PIXEL (f);

    a.event_mask = (ButtonPressMask | ButtonReleaseMask
		    | ButtonMotionMask | PointerMotionHintMask
		    | ExposureMask);
    a.cursor = FRAME_X_DISPLAY_INFO (f)->vertical_scroll_bar_cursor;

    mask = (CWBackPixel | CWEventMask | CWCursor);

    /* Clear the area of W that will serve as a scroll bar.  This is
       for the case that a window has been split horizontally.  In
       this case, no clear_frame is generated to reduce flickering.  */
    if (width > 0 && height > 0)
      x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    left, top, width,
		    window_box_height (w), False);

    window = XCreateWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    /* Position and size of scroll bar.  */
			    left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
			    top,
			    width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
			    height,
			    /* Border width, depth, class, and visual.  */
			     0,
			    CopyFromParent,
			    CopyFromParent,
			    CopyFromParent,
			     /* Attributes.  */
			    mask, &a);
    bar->x_window = window;
  }
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  XSETWINDOW (bar->window, w);
  bar->top = top;
  bar->left = left;
  bar->width = width;
  bar->height = height;
  bar->start = 0;
  bar->end = 0;
  bar->dragging = Qnil;
  bar->fringe_extended_p = 0;

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (!NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  /* Map the window/widget.  */
#ifdef USE_TOOLKIT_SCROLL_BARS
  {
#ifdef USE_GTK
    xg_update_scrollbar_pos (f,
                             bar->x_window,
                             top,
                             left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
                             width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
                             max (height, 1));
    xg_show_scroll_bar (bar->x_window);
#else /* not USE_GTK */
    Widget scroll_bar = SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar);
    XtConfigureWidget (scroll_bar,
		       left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
		       top,
		       width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
		       max (height, 1), 0);
    XtMapWidget (scroll_bar);
#endif /* not USE_GTK */
    }
#else /* not USE_TOOLKIT_SCROLL_BARS */
  XMapRaised (FRAME_X_DISPLAY (f), bar->x_window);
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  UNBLOCK_INPUT;
  return bar;
}


#ifndef USE_TOOLKIT_SCROLL_BARS

/* Draw BAR's handle in the proper position.

   If the handle is already drawn from START to END, don't bother
   redrawing it, unless REBUILD is non-zero; in that case, always
   redraw it.  (REBUILD is handy for drawing the handle after expose
   events.)

   Normally, we want to constrain the start and end of the handle to
   fit inside its rectangle, but if the user is dragging the scroll
   bar handle, we want to let them drag it down all the way, so that
   the bar's top is as far down as it goes; otherwise, there's no way
   to move to the very end of the buffer.  */

static void
x_scroll_bar_set_handle (bar, start, end, rebuild)
     struct scroll_bar *bar;
     int start, end;
     int rebuild;
{
  int dragging = ! NILP (bar->dragging);
  Window w = bar->x_window;
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  GC gc = f->output_data.x->normal_gc;

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == bar->start
      && end == bar->end)
    return;

  BLOCK_INPUT;

  {
    int inside_width = VERTICAL_SCROLL_BAR_INSIDE_WIDTH (f, bar->width);
    int inside_height = VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, bar->height);
    int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, bar->height);

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
    bar->start = start;
    bar->end = end;

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
      x_clear_area (FRAME_X_DISPLAY (f), w,
		    /* x, y, width, height, and exposures.  */
		    VERTICAL_SCROLL_BAR_LEFT_BORDER,
		    VERTICAL_SCROLL_BAR_TOP_BORDER,
		    inside_width, start,
		    False);

    /* Change to proper foreground color if one is specified.  */
    if (f->output_data.x->scroll_bar_foreground_pixel != -1)
      XSetForeground (FRAME_X_DISPLAY (f), gc,
		      f->output_data.x->scroll_bar_foreground_pixel);

    /* Draw the handle itself.  */
    XFillRectangle (FRAME_X_DISPLAY (f), w, gc,
		    /* x, y, width, height */
		    VERTICAL_SCROLL_BAR_LEFT_BORDER,
		    VERTICAL_SCROLL_BAR_TOP_BORDER + start,
		    inside_width, end - start);

    /* Restore the foreground color of the GC if we changed it above.  */
    if (f->output_data.x->scroll_bar_foreground_pixel != -1)
      XSetForeground (FRAME_X_DISPLAY (f), gc,
		      FRAME_FOREGROUND_PIXEL (f));

    /* Draw the empty space below the handle.  Note that we can't
       clear zero-height areas; that means "clear to end of window." */
    if (end < inside_height)
      x_clear_area (FRAME_X_DISPLAY (f), w,
		    /* x, y, width, height, and exposures.  */
		    VERTICAL_SCROLL_BAR_LEFT_BORDER,
		    VERTICAL_SCROLL_BAR_TOP_BORDER + end,
		    inside_width, inside_height - end,
		    False);

  }

  UNBLOCK_INPUT;
}

#endif /* !USE_TOOLKIT_SCROLL_BARS */

/* Destroy scroll bar BAR, and set its Emacs window's scroll bar to
   nil.  */

static void
x_scroll_bar_remove (bar)
     struct scroll_bar *bar;
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  BLOCK_INPUT;

#ifdef USE_TOOLKIT_SCROLL_BARS
#ifdef USE_GTK
  xg_remove_scroll_bar (f, bar->x_window);
#else /* not USE_GTK */
  XtDestroyWidget (SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar));
#endif /* not USE_GTK */
#else
  XDestroyWindow (FRAME_X_DISPLAY (f), bar->x_window);
#endif

  /* Disassociate this scroll bar from its window.  */
  XWINDOW (bar->window)->vertical_scroll_bar = Qnil;

  UNBLOCK_INPUT;
}


/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */

static void
XTset_vertical_scroll_bar (w, portion, whole, position)
     struct window *w;
     int portion, whole, position;
{
  struct frame *f = XFRAME (w->frame);
  struct scroll_bar *bar;
  int top, height, left, sb_left, width, sb_width;
  int window_y, window_height;
#ifdef USE_TOOLKIT_SCROLL_BARS
  int fringe_extended_p;
#endif

  /* Get window dimensions.  */
  window_box (w, -1, 0, &window_y, 0, &window_height);
  top = window_y;
  width = WINDOW_CONFIG_SCROLL_BAR_COLS (w) * FRAME_COLUMN_WIDTH (f);
  height = window_height;

  /* Compute the left edge of the scroll bar area.  */
  left = WINDOW_SCROLL_BAR_AREA_X (w);

  /* Compute the width of the scroll bar which might be less than
     the width of the area reserved for the scroll bar.  */
  if (WINDOW_CONFIG_SCROLL_BAR_WIDTH (w) > 0)
    sb_width = WINDOW_CONFIG_SCROLL_BAR_WIDTH (w);
  else
    sb_width = width;

  /* Compute the left edge of the scroll bar.  */
#ifdef USE_TOOLKIT_SCROLL_BARS
  if (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w))
    sb_left = left + (WINDOW_RIGHTMOST_P (w) ? width - sb_width : 0);
  else
    sb_left = left + (WINDOW_LEFTMOST_P (w) ? 0 : width - sb_width);
#else
  if (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_RIGHT (w))
    sb_left = left + width - sb_width;
  else
    sb_left = left;
#endif

#ifdef USE_TOOLKIT_SCROLL_BARS
  if (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w))
    fringe_extended_p = (WINDOW_LEFTMOST_P (w)
			 && WINDOW_LEFT_FRINGE_WIDTH (w)
			 && (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
			     || WINDOW_LEFT_MARGIN_COLS (w) == 0));
  else
    fringe_extended_p = (WINDOW_RIGHTMOST_P (w)
			 && WINDOW_RIGHT_FRINGE_WIDTH (w)
			 && (WINDOW_HAS_FRINGES_OUTSIDE_MARGINS (w)
			     || WINDOW_RIGHT_MARGIN_COLS (w) == 0));
#endif

  /* Does the scroll bar exist yet?  */
  if (NILP (w->vertical_scroll_bar))
    {
      if (width > 0 && height > 0)
	{
	  BLOCK_INPUT;
#ifdef USE_TOOLKIT_SCROLL_BARS
	  if (fringe_extended_p)
	    x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			  sb_left, top, sb_width, height, False);
	  else
#endif
	    x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			  left, top, width, height, False);
	  UNBLOCK_INPUT;
	}

      bar = x_scroll_bar_create (w, top, sb_left, sb_width, height);
    }
  else
    {
      /* It may just need to be moved and resized.  */
      unsigned int mask = 0;

      bar = XSCROLL_BAR (w->vertical_scroll_bar);

      BLOCK_INPUT;

      if (sb_left != bar->left)
	mask |= CWX;
      if (top != bar->top)
	mask |= CWY;
      if (sb_width != bar->width)
	mask |= CWWidth;
      if (height != bar->height)
	mask |= CWHeight;

#ifdef USE_TOOLKIT_SCROLL_BARS

      /* Move/size the scroll bar widget.  */
      if (mask || bar->fringe_extended_p != fringe_extended_p)
	{
	  /* Since toolkit scroll bars are smaller than the space reserved
	     for them on the frame, we have to clear "under" them.  */
	  if (width > 0 && height > 0)
	    {
	      if (fringe_extended_p)
		x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			      sb_left, top, sb_width, height, False);
	      else
		x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			      left, top, width, height, False);
	    }
#ifdef USE_GTK
          xg_update_scrollbar_pos (f,
                                   bar->x_window,
                                   top,
                                   sb_left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
                                   sb_width - VERTICAL_SCROLL_BAR_WIDTH_TRIM *2,
                                   max (height, 1));
#else /* not USE_GTK */
          XtConfigureWidget (SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar),
                             sb_left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
                             top,
                             sb_width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
                             max (height, 1), 0);
#endif /* not USE_GTK */
	}
#else /* not USE_TOOLKIT_SCROLL_BARS */

      /* Clear areas not covered by the scroll bar because of
	 VERTICAL_SCROLL_BAR_WIDTH_TRIM.  */
      if (VERTICAL_SCROLL_BAR_WIDTH_TRIM)
	{
	  x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			left, top, VERTICAL_SCROLL_BAR_WIDTH_TRIM,
			height, False);
	  x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			left + width - VERTICAL_SCROLL_BAR_WIDTH_TRIM,
			top, VERTICAL_SCROLL_BAR_WIDTH_TRIM,
			height, False);
	}

      /* Clear areas not covered by the scroll bar because it's not as
	 wide as the area reserved for it.  This makes sure a
	 previous mode line display is cleared after C-x 2 C-x 1, for
	 example.  */
      {
	int area_width = WINDOW_CONFIG_SCROLL_BAR_COLS (w) * FRAME_COLUMN_WIDTH (f);
	int rest = area_width - sb_width;
	if (rest > 0 && height > 0)
	  {
	    if (WINDOW_HAS_VERTICAL_SCROLL_BAR_ON_LEFT (w))
	      x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    left + area_width -  rest, top,
			    rest, height, False);
	    else
	      x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    left, top, rest, height, False);
	  }
      }

      /* Move/size the scroll bar window.  */
      if (mask)
	{
	  XWindowChanges wc;

	  wc.x = sb_left + VERTICAL_SCROLL_BAR_WIDTH_TRIM;
	  wc.y = top;
	  wc.width = sb_width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2;
	  wc.height = height;
	  XConfigureWindow (FRAME_X_DISPLAY (f), bar->x_window,
			    mask, &wc);
	}

#endif /* not USE_TOOLKIT_SCROLL_BARS */

      /* Remember new settings.  */
      bar->left = sb_left;
      bar->top = top;
      bar->width = sb_width;
      bar->height = height;

      UNBLOCK_INPUT;
    }

#ifdef USE_TOOLKIT_SCROLL_BARS
  bar->fringe_extended_p = fringe_extended_p;

  x_set_toolkit_scroll_bar_thumb (bar, portion, position, whole);
#else /* not USE_TOOLKIT_SCROLL_BARS */
  /* Set the scroll bar's current state, unless we're currently being
     dragged.  */
  if (NILP (bar->dragging))
    {
      int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, height);

      if (whole == 0)
	x_scroll_bar_set_handle (bar, 0, top_range, 0);
      else
	{
	  int start = ((double) position * top_range) / whole;
	  int end = ((double) (position + portion) * top_range) / whole;
	  x_scroll_bar_set_handle (bar, start, end, 0);
	}
    }
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  XSETVECTOR (w->vertical_scroll_bar, bar);
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
   `*redeem_scroll_bar_hook' is applied to its window before the judgment.  */

static void
XTcondemn_scroll_bars (frame)
     FRAME_PTR frame;
{
  /* Transfer all the scroll bars to FRAME_CONDEMNED_SCROLL_BARS.  */
  while (! NILP (FRAME_SCROLL_BARS (frame)))
    {
      Lisp_Object bar;
      bar = FRAME_SCROLL_BARS (frame);
      FRAME_SCROLL_BARS (frame) = XSCROLL_BAR (bar)->next;
      XSCROLL_BAR (bar)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
      XSCROLL_BAR (bar)->prev = Qnil;
      if (! NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
	XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = bar;
      FRAME_CONDEMNED_SCROLL_BARS (frame) = bar;
    }
}


/* Un-mark WINDOW's scroll bar for deletion in this judgment cycle.
   Note that WINDOW isn't necessarily condemned at all.  */

static void
XTredeem_scroll_bar (window)
     struct window *window;
{
  struct scroll_bar *bar;
  struct frame *f;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (window->vertical_scroll_bar))
    abort ();

  bar = XSCROLL_BAR (window->vertical_scroll_bar);

  /* Unlink it from the condemned list.  */
  f = XFRAME (WINDOW_FRAME (window));
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


#ifndef USE_TOOLKIT_SCROLL_BARS
/* Handle an Expose or GraphicsExpose event on a scroll bar.  This
   is a no-op when using toolkit scroll bars.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static void
x_scroll_bar_expose (bar, event)
     struct scroll_bar *bar;
     XEvent *event;
{
  Window w = bar->x_window;
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  GC gc = f->output_data.x->normal_gc;
  int width_trim = VERTICAL_SCROLL_BAR_WIDTH_TRIM;

  BLOCK_INPUT;

  x_scroll_bar_set_handle (bar, bar->start, bar->end, 1);

  /* Switch to scroll bar foreground color.  */
  if (f->output_data.x->scroll_bar_foreground_pixel != -1)
    XSetForeground (FRAME_X_DISPLAY (f), gc,
 		    f->output_data.x->scroll_bar_foreground_pixel);

  /* Draw a one-pixel border just inside the edges of the scroll bar.  */
  XDrawRectangle (FRAME_X_DISPLAY (f), w, gc,

		  /* x, y, width, height */
		  0, 0,
		  bar->width - 1 - width_trim - width_trim,
		  bar->height - 1);

   /* Restore the foreground color of the GC if we changed it above.  */
   if (f->output_data.x->scroll_bar_foreground_pixel != -1)
     XSetForeground (FRAME_X_DISPLAY (f), gc,
		     FRAME_FOREGROUND_PIXEL (f));

   UNBLOCK_INPUT;

}
#endif /* not USE_TOOLKIT_SCROLL_BARS */

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than NO_EVENT, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */


static void
x_scroll_bar_handle_click (bar, event, emacs_event)
     struct scroll_bar *bar;
     XEvent *event;
     struct input_event *emacs_event;
{
  if (! WINDOWP (bar->window))
    abort ();

  emacs_event->kind = SCROLL_BAR_CLICK_EVENT;
  emacs_event->code = event->xbutton.button - Button1;
  emacs_event->modifiers
    = (x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO
			       (XFRAME (WINDOW_FRAME (XWINDOW (bar->window)))),
			       event->xbutton.state)
       | (event->type == ButtonRelease
	  ? up_modifier
	  : down_modifier));
  emacs_event->frame_or_window = bar->window;
  emacs_event->arg = Qnil;
  emacs_event->timestamp = event->xbutton.time;
  {
#if 0
    FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
    int internal_height
      = VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, bar->height);
#endif
    int top_range
      = VERTICAL_SCROLL_BAR_TOP_RANGE (f, bar->height);
    int y = event->xbutton.y - VERTICAL_SCROLL_BAR_TOP_BORDER;

    if (y < 0) y = 0;
    if (y > top_range) y = top_range;

    if (y < bar->start)
      emacs_event->part = scroll_bar_above_handle;
    else if (y < bar->end + VERTICAL_SCROLL_BAR_MIN_HANDLE)
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
      XSETINT (bar->dragging, y - bar->start);
#endif

#ifndef USE_TOOLKIT_SCROLL_BARS
    /* If the user has released the handle, set it to its final position.  */
    if (event->type == ButtonRelease
	&& ! NILP (bar->dragging))
      {
	int new_start = y - XINT (bar->dragging);
	int new_end = new_start + bar->end - bar->start;

	x_scroll_bar_set_handle (bar, new_start, new_end, 0);
	bar->dragging = Qnil;
      }
#endif

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

#ifndef USE_TOOLKIT_SCROLL_BARS

/* Handle some mouse motion while someone is dragging the scroll bar.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static void
x_scroll_bar_note_movement (bar, event)
     struct scroll_bar *bar;
     XEvent *event;
{
  FRAME_PTR f = XFRAME (XWINDOW (bar->window)->frame);

  last_mouse_movement_time = event->xmotion.time;

  f->mouse_moved = 1;
  XSETVECTOR (last_mouse_scroll_bar, bar);

  /* If we're dragging the bar, display it.  */
  if (! NILP (bar->dragging))
    {
      /* Where should the handle be now?  */
      int new_start = event->xmotion.y - XINT (bar->dragging);

      if (new_start != bar->start)
	{
	  int new_end = new_start + bar->end - bar->start;

	  x_scroll_bar_set_handle (bar, new_start, new_end, 0);
	}
    }
}

#endif /* !USE_TOOLKIT_SCROLL_BARS */

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
  Window w = bar->x_window;
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
#if 0
      int inside_height
	= VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, bar->height);
#endif
      int top_range
	= VERTICAL_SCROLL_BAR_TOP_RANGE     (f, bar->height);

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
      else if (win_y < bar->start)
	*part = scroll_bar_above_handle;
      else if (win_y < bar->end + VERTICAL_SCROLL_BAR_MIN_HANDLE)
	*part = scroll_bar_handle;
      else
	*part = scroll_bar_below_handle;

      XSETINT (*x, win_y);
      XSETINT (*y, top_range);

      f->mouse_moved = 0;
      last_mouse_scroll_bar = Qnil;
    }

  *time = last_mouse_movement_time;

  UNBLOCK_INPUT;
}


/* The screen has been cleared so we may have changed foreground or
   background colors, and the scroll bars may need to be redrawn.
   Clear out the scroll bars, and ask for expose events, so we can
   redraw them.  */

void
x_scroll_bar_clear (f)
     FRAME_PTR f;
{
#ifndef USE_TOOLKIT_SCROLL_BARS
  Lisp_Object bar;

  /* We can have scroll bars even if this is 0,
     if we just turned off scroll bar mode.
     But in that case we should not clear them.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
	 bar = XSCROLL_BAR (bar)->next)
      XClearArea (FRAME_X_DISPLAY (f),
		  XSCROLL_BAR (bar)->x_window,
		  0, 0, 0, 0, True);
#endif /* not USE_TOOLKIT_SCROLL_BARS */
}


/* The main X event-reading loop - XTread_socket.  */

#if 0
/* Time stamp of enter window event.  This is only used by XTread_socket,
   but we have to put it out here, since static variables within functions
   sometimes don't work.  */

static Time enter_timestamp;
#endif

/* This holds the state XLookupString needs to implement dead keys
   and other tricks known as "compose processing".  _X Window System_
   says that a portable program can't use this, but Stephen Gildea assures
   me that letting the compiler initialize it to zeros will work okay.

   This must be defined outside of XTread_socket, for the same reasons
   given for enter_timestamp, above.  */

static XComposeStatus compose_status;

/* Record the last 100 characters stored
   to help debug the loss-of-chars-during-GC problem.  */

static int temp_index;
static short temp_buffer[100];

#define STORE_KEYSYM_FOR_DEBUG(keysym)				\
  if (temp_index == sizeof temp_buffer / sizeof (short))	\
    temp_index = 0;						\
  temp_buffer[temp_index++] = (keysym)

/* Set this to nonzero to fake an "X I/O error"
   on a particular display.  */

struct x_display_info *XTread_socket_fake_io_error;

/* When we find no input here, we occasionally do a no-op command
   to verify that the X server is still running and we can still talk with it.
   We try all the open displays, one by one.
   This variable is used for cycling thru the displays.  */

static struct x_display_info *next_noop_dpyinfo;

#define SET_SAVED_MENU_EVENT(size)					\
     do									\
       {								\
	 if (f->output_data.x->saved_menu_event == 0)			\
           f->output_data.x->saved_menu_event				\
	     = (XEvent *) xmalloc (sizeof (XEvent));			\
         bcopy (&event, f->output_data.x->saved_menu_event, size);	\
	 inev.ie.kind = MENU_BAR_ACTIVATE_EVENT;			\
	 XSETFRAME (inev.ie.frame_or_window, f);			\
       }								\
     while (0)

#define SET_SAVED_BUTTON_EVENT SET_SAVED_MENU_EVENT (sizeof (XButtonEvent))
#define SET_SAVED_KEY_EVENT    SET_SAVED_MENU_EVENT (sizeof (XKeyEvent))


enum
{
  X_EVENT_NORMAL,
  X_EVENT_GOTO_OUT,
  X_EVENT_DROP
};

/* Filter events for the current X input method.
   DPYINFO is the display this event is for.
   EVENT is the X event to filter.

   Returns non-zero if the event was filtered, caller shall not process
   this event further.
   Returns zero if event is wasn't filtered.  */

#ifdef HAVE_X_I18N
static int
x_filter_event (dpyinfo, event)
     struct x_display_info *dpyinfo;
     XEvent *event;
{
  /* XFilterEvent returns non-zero if the input method has
   consumed the event.  We pass the frame's X window to
   XFilterEvent because that's the one for which the IC
   was created.  */

  struct frame *f1 = x_any_window_to_frame (dpyinfo,
                                            event->xclient.window);

  return XFilterEvent (event, f1 ? FRAME_X_WINDOW (f1) : None);
}
#endif

#ifdef USE_GTK
static int current_count;
static int current_finish;
static struct input_event *current_hold_quit;

/* This is the filter function invoked by the GTK event loop.
   It is invoked before the XEvent is translated to a GdkEvent,
   so we have a chance to act on the event before GTK. */
static GdkFilterReturn
event_handler_gdk (gxev, ev, data)
     GdkXEvent *gxev;
     GdkEvent *ev;
     gpointer data;
{
  XEvent *xev = (XEvent *) gxev;

  if (current_count >= 0)
    {
      struct x_display_info *dpyinfo;

      dpyinfo = x_display_info_for_display (xev->xany.display);

#ifdef HAVE_X_I18N
      /* Filter events for the current X input method.
         GTK calls XFilterEvent but not for key press and release,
         so we do it here.  */
      if (xev->type == KeyPress || xev->type == KeyRelease)
        if (dpyinfo && x_filter_event (dpyinfo, xev))
          return GDK_FILTER_REMOVE;
#endif

      if (! dpyinfo)
        current_finish = X_EVENT_NORMAL;
      else
	{
	  current_count +=
	    handle_one_xevent (dpyinfo, xev, &current_finish,
			       current_hold_quit);
	}
    }
  else
    current_finish = x_dispatch_event (xev, xev->xany.display);

  if (current_finish == X_EVENT_GOTO_OUT || current_finish == X_EVENT_DROP)
    return GDK_FILTER_REMOVE;

  return GDK_FILTER_CONTINUE;
}
#endif /* USE_GTK */


/* Handles the XEvent EVENT on display DPYINFO.

   *FINISH is X_EVENT_GOTO_OUT if caller should stop reading events.
   *FINISH is zero if caller should continue reading events.
   *FINISH is X_EVENT_DROP if event should not be passed to the toolkit.

   We return the number of characters stored into the buffer. */

static int
handle_one_xevent (dpyinfo, eventp, finish, hold_quit)
     struct x_display_info *dpyinfo;
     XEvent *eventp;
     int *finish;
     struct input_event *hold_quit;
{
  union {
    struct input_event ie;
    struct selection_input_event sie;
  } inev;
  int count = 0;
  int do_help = 0;
  int nbytes = 0;
  struct frame *f = NULL;
  struct coding_system coding;
  XEvent event = *eventp;

  *finish = X_EVENT_NORMAL;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

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
                /* Use x_any_window_to_frame because this
                   could be the shell widget window
                   if the frame has no title bar.  */
                f = x_any_window_to_frame (dpyinfo, event.xclient.window);
#ifdef HAVE_X_I18N
                /* Not quite sure this is needed -pd */
                if (f && FRAME_XIC (f))
                  XSetICFocus (FRAME_XIC (f));
#endif
#if 0 /* Emacs sets WM hints whose `input' field is `true'.  This
	 instructs the WM to set the input focus automatically for
	 Emacs with a call to XSetInputFocus.  Setting WM_TAKE_FOCUS
	 tells the WM to send us a ClientMessage WM_TAKE_FOCUS after
	 it has set the focus.  So, XSetInputFocus below is not
	 needed.

	 The call to XSetInputFocus below has also caused trouble.  In
	 cases where the XSetInputFocus done by the WM and the one
	 below are temporally close (on a fast machine), the call
	 below can generate additional FocusIn events which confuse
	 Emacs.  */

                /* Since we set WM_TAKE_FOCUS, we must call
                   XSetInputFocus explicitly.  But not if f is null,
                   since that might be an event for a deleted frame.  */
                if (f)
                  {
                    Display *d = event.xclient.display;
                    /* Catch and ignore errors, in case window has been
                       iconified by a window manager such as GWM.  */
                    x_catch_errors (d);
                    XSetInputFocus (d, event.xclient.window,
                                    /* The ICCCM says this is
                                       the only valid choice.  */
                                    RevertToParent,
                                    event.xclient.data.l[1]);
                    /* This is needed to detect the error
                       if there is an error.  */
                    XSync (d, False);
                    x_uncatch_errors ();
                  }
                /* Not certain about handling scroll bars here */
#endif /* 0 */
		goto done;
              }

            if (event.xclient.data.l[0]
                     == dpyinfo->Xatom_wm_save_yourself)
              {
                /* Save state modify the WM_COMMAND property to
                   something which can reinstate us.  This notifies
                   the session manager, who's looking for such a
                   PropertyNotify.  Can restart processing when
                   a keyboard or mouse event arrives.  */
                /* If we have a session manager, don't set this.
                   KDE will then start two Emacsen, one for the
                   session manager and one for this. */
#ifdef HAVE_X_SM
                if (! x_session_have_connection ())
#endif
                  {
                    f = x_top_window_to_frame (dpyinfo,
                                               event.xclient.window);
                    /* This is just so we only give real data once
                       for a single Emacs process.  */
                    if (f == SELECTED_FRAME ())
                      XSetCommand (FRAME_X_DISPLAY (f),
                                   event.xclient.window,
                                   initial_argv, initial_argc);
                    else if (f)
                      XSetCommand (FRAME_X_DISPLAY (f),
                                   event.xclient.window,
                                   0, 0);
                  }
		goto done;
              }

            if (event.xclient.data.l[0]
		== dpyinfo->Xatom_wm_delete_window)
              {
                f = x_any_window_to_frame (dpyinfo,
                                           event.xclient.window);
                if (!f)
		  goto OTHER; /* May be a dialog that is to be removed  */

		inev.ie.kind = DELETE_WINDOW_EVENT;
		XSETFRAME (inev.ie.frame_or_window, f);
		goto done;
              }

	    goto done;
          }

        if (event.xclient.message_type
                 == dpyinfo->Xatom_wm_configure_denied)
          {
	    goto done;
          }

        if (event.xclient.message_type
	    == dpyinfo->Xatom_wm_window_moved)
          {
            int new_x, new_y;
	    f = x_window_to_frame (dpyinfo, event.xclient.window);

            new_x = event.xclient.data.s[0];
            new_y = event.xclient.data.s[1];

            if (f)
              {
                f->left_pos = new_x;
                f->top_pos = new_y;
              }
	    goto done;
          }

#ifdef HACK_EDITRES
        if (event.xclient.message_type
	    == dpyinfo->Xatom_editres)
          {
	    f = x_any_window_to_frame (dpyinfo, event.xclient.window);
	    if (f)
              _XEditResCheckMessages (f->output_data.x->widget, NULL,
                                      &event, NULL);
	    goto done;
          }
#endif /* HACK_EDITRES */

        if ((event.xclient.message_type
	     == dpyinfo->Xatom_DONE)
	    || (event.xclient.message_type
		== dpyinfo->Xatom_PAGE))
          {
            /* Ghostview job completed.  Kill it.  We could
               reply with "Next" if we received "Page", but we
               currently never do because we are interested in
               images, only, which should have 1 page.  */
            Pixmap pixmap = (Pixmap) event.xclient.data.l[1];
	    f = x_window_to_frame (dpyinfo, event.xclient.window);
	    if (!f)
	      goto OTHER;
            x_kill_gs_process (pixmap, f);
            expose_frame (f, 0, 0, 0, 0);
	    goto done;
          }

#ifdef USE_TOOLKIT_SCROLL_BARS
        /* Scroll bar callbacks send a ClientMessage from which
           we construct an input_event.  */
        if (event.xclient.message_type
	    == dpyinfo->Xatom_Scrollbar)
          {
            x_scroll_bar_to_input_event (&event, &inev.ie);
	    *finish = X_EVENT_GOTO_OUT;
            goto done;
          }
#endif /* USE_TOOLKIT_SCROLL_BARS */

	/* XEmbed messages from the embedder (if any).  */
        if (event.xclient.message_type
	    == dpyinfo->Xatom_XEMBED)
          {
	    enum xembed_message msg = event.xclient.data.l[1];
	    if (msg == XEMBED_FOCUS_IN || msg == XEMBED_FOCUS_OUT)
	      x_detect_focus_change (dpyinfo, &event, &inev.ie);

	    *finish = X_EVENT_GOTO_OUT;
            goto done;
          }

	f = x_any_window_to_frame (dpyinfo, event.xclient.window);
	if (!f)
	  goto OTHER;
	if (x_handle_dnd_message (f, &event.xclient, dpyinfo, &inev.ie))
	  *finish = X_EVENT_DROP;
      }
      break;

    case SelectionNotify:
      last_user_time = event.xselection.time;
#ifdef USE_X_TOOLKIT
      if (! x_window_to_frame (dpyinfo, event.xselection.requestor))
        goto OTHER;
#endif /* not USE_X_TOOLKIT */
      x_handle_selection_notify (&event.xselection);
      break;

    case SelectionClear:	/* Someone has grabbed ownership.  */
      last_user_time = event.xselectionclear.time;
#ifdef USE_X_TOOLKIT
      if (! x_window_to_frame (dpyinfo, event.xselectionclear.window))
        goto OTHER;
#endif /* USE_X_TOOLKIT */
      {
        XSelectionClearEvent *eventp = (XSelectionClearEvent *) &event;

        inev.ie.kind = SELECTION_CLEAR_EVENT;
        SELECTION_EVENT_DISPLAY (&inev.sie) = eventp->display;
        SELECTION_EVENT_SELECTION (&inev.sie) = eventp->selection;
        SELECTION_EVENT_TIME (&inev.sie) = eventp->time;
        inev.ie.frame_or_window = Qnil;
      }
      break;

    case SelectionRequest:	/* Someone wants our selection.  */
      last_user_time = event.xselectionrequest.time;
#ifdef USE_X_TOOLKIT
      if (!x_window_to_frame (dpyinfo, event.xselectionrequest.owner))
        goto OTHER;
#endif /* USE_X_TOOLKIT */
      {
          XSelectionRequestEvent *eventp
            = (XSelectionRequestEvent *) &event;

          inev.ie.kind = SELECTION_REQUEST_EVENT;
          SELECTION_EVENT_DISPLAY (&inev.sie) = eventp->display;
          SELECTION_EVENT_REQUESTOR (&inev.sie) = eventp->requestor;
          SELECTION_EVENT_SELECTION (&inev.sie) = eventp->selection;
          SELECTION_EVENT_TARGET (&inev.sie) = eventp->target;
          SELECTION_EVENT_PROPERTY (&inev.sie) = eventp->property;
          SELECTION_EVENT_TIME (&inev.sie) = eventp->time;
          inev.ie.frame_or_window = Qnil;
      }
      break;

    case PropertyNotify:
      last_user_time = event.xproperty.time;
#if 0 /* This is plain wrong.  In the case that we are waiting for a
	 PropertyNotify used as an ACK in incremental selection
	 transfer, the property will be on the receiver's window.  */
#if defined USE_X_TOOLKIT
      if (!x_any_window_to_frame (dpyinfo, event.xproperty.window))
        goto OTHER;
#endif
#endif
      x_handle_property_notify (&event.xproperty);
      goto OTHER;

    case ReparentNotify:
      f = x_top_window_to_frame (dpyinfo, event.xreparent.window);
      if (f)
        {
          int x, y;
          f->output_data.x->parent_desc = event.xreparent.parent;
          x_real_positions (f, &x, &y);
          f->left_pos = x;
          f->top_pos = y;

          /* Perhaps reparented due to a WM restart.  Reset this.  */
          FRAME_X_DISPLAY_INFO (f)->wm_type = X_WMTYPE_UNKNOWN;
          FRAME_X_DISPLAY_INFO (f)->net_supported_window = 0;
        }
      goto OTHER;

    case Expose:
      f = x_window_to_frame (dpyinfo, event.xexpose.window);
      if (f)
        {
          x_check_fullscreen (f);

#ifdef USE_GTK
          /* This seems to be needed for GTK 2.6.  */
          x_clear_area (event.xexpose.display,
                        event.xexpose.window,
                        event.xexpose.x, event.xexpose.y,
                        event.xexpose.width, event.xexpose.height,
                        FALSE);
#endif
          if (f->async_visible == 0)
            {
              f->async_visible = 1;
              f->async_iconified = 0;
              f->output_data.x->has_been_visible = 1;
              SET_FRAME_GARBAGED (f);
            }
          else
            expose_frame (f,
			  event.xexpose.x, event.xexpose.y,
                          event.xexpose.width, event.xexpose.height);
        }
      else
        {
#ifndef USE_TOOLKIT_SCROLL_BARS
          struct scroll_bar *bar;
#endif
#if defined USE_LUCID
          /* Submenus of the Lucid menu bar aren't widgets
             themselves, so there's no way to dispatch events
             to them.  Recognize this case separately.  */
          {
            Widget widget
              = x_window_to_menu_bar (event.xexpose.window);
            if (widget)
              xlwmenu_redisplay (widget);
          }
#endif /* USE_LUCID */

#ifdef USE_TOOLKIT_SCROLL_BARS
          /* Dispatch event to the widget.  */
          goto OTHER;
#else /* not USE_TOOLKIT_SCROLL_BARS */
          bar = x_window_to_scroll_bar (event.xexpose.display,
                                        event.xexpose.window);

          if (bar)
            x_scroll_bar_expose (bar, &event);
#ifdef USE_X_TOOLKIT
          else
            goto OTHER;
#endif /* USE_X_TOOLKIT */
#endif /* not USE_TOOLKIT_SCROLL_BARS */
        }
      break;

    case GraphicsExpose:	/* This occurs when an XCopyArea's
                                   source area was obscured or not
                                   available.  */
      f = x_window_to_frame (dpyinfo, event.xgraphicsexpose.drawable);
      if (f)
        {
          expose_frame (f,
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
                                   available.  */
      break;

    case UnmapNotify:
      /* Redo the mouse-highlight after the tooltip has gone.  */
      if (event.xmap.window == tip_window)
        {
          tip_window = 0;
          redo_mouse_highlight ();
        }

      f = x_top_window_to_frame (dpyinfo, event.xunmap.window);
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
             But x_make_frame_invisible clears both
             the visible flag and the iconified flag;
             and that way, we know the window is not iconified now.  */
          if (FRAME_VISIBLE_P (f) || FRAME_ICONIFIED_P (f))
            {
              f->async_iconified = 1;

              inev.ie.kind = ICONIFY_EVENT;
              XSETFRAME (inev.ie.frame_or_window, f);
            }
        }
      goto OTHER;

    case MapNotify:
      if (event.xmap.window == tip_window)
        /* The tooltip has been drawn already.  Avoid
           the SET_FRAME_GARBAGED below.  */
        goto OTHER;

      /* We use x_top_window_to_frame because map events can
         come for sub-windows and they don't mean that the
         frame is visible.  */
      f = x_top_window_to_frame (dpyinfo, event.xmap.window);
      if (f)
        {
          /* wait_reading_process_output will notice this and update
             the frame's display structures.
             If we where iconified, we should not set garbaged,
             because that stops redrawing on Expose events.  This looks
             bad if we are called from a recursive event loop
             (x_dispatch_event), for example when a dialog is up.  */
          if (! f->async_iconified)
            SET_FRAME_GARBAGED (f);

          f->async_visible = 1;
          f->async_iconified = 0;
          f->output_data.x->has_been_visible = 1;

          if (f->iconified)
            {
              inev.ie.kind = DEICONIFY_EVENT;
              XSETFRAME (inev.ie.frame_or_window, f);
            }
          else if (! NILP (Vframe_list)
                   && ! NILP (XCDR (Vframe_list)))
            /* Force a redisplay sooner or later
               to update the frame titles
               in case this is the second frame.  */
            record_asynch_buffer_change ();
        }
      goto OTHER;

    case KeyPress:

      last_user_time = event.xkey.time;
      ignore_next_mouse_click_timeout = 0;

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
      /* Dispatch KeyPress events when in menu.  */
      if (popup_activated ())
        goto OTHER;
#endif

      f = x_any_window_to_frame (dpyinfo, event.xkey.window);

      /* If mouse-highlight is an integer, input clears out
	 mouse highlighting.  */
      if (!dpyinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight)
	  && (f == 0
	      || !EQ (f->tool_bar_window, dpyinfo->mouse_face_window)))
        {
          clear_mouse_face (dpyinfo);
          dpyinfo->mouse_face_hidden = 1;
        }

#if defined USE_MOTIF && defined USE_TOOLKIT_SCROLL_BARS
      if (f == 0)
        {
          /* Scroll bars consume key events, but we want
             the keys to go to the scroll bar's frame.  */
          Widget widget = XtWindowToWidget (dpyinfo->display,
                                            event.xkey.window);
          if (widget && XmIsScrollBar (widget))
            {
              widget = XtParent (widget);
              f = x_any_window_to_frame (dpyinfo, XtWindow (widget));
            }
        }
#endif /* USE_MOTIF and USE_TOOLKIT_SCROLL_BARS */

      if (f != 0)
        {
          KeySym keysym, orig_keysym;
          /* al%imercury@uunet.uu.net says that making this 81
             instead of 80 fixed a bug whereby meta chars made
             his Emacs hang.

             It seems that some version of XmbLookupString has
             a bug of not returning XBufferOverflow in
             status_return even if the input is too long to
             fit in 81 bytes.  So, we must prepare sufficient
             bytes for copy_buffer.  513 bytes (256 chars for
             two-byte character set) seems to be a fairly good
             approximation.  -- 2000.8.10 handa@etl.go.jp  */
          unsigned char copy_buffer[513];
          unsigned char *copy_bufptr = copy_buffer;
          int copy_bufsiz = sizeof (copy_buffer);
          int modifiers;
          Lisp_Object coding_system = Qlatin_1;
	  Lisp_Object c;

#ifdef USE_GTK
          /* Don't pass keys to GTK.  A Tab will shift focus to the
             tool bar in GTK 2.4.  Keys will still go to menus and
             dialogs because in that case popup_activated is TRUE
             (see above).  */
          *finish = X_EVENT_DROP;
#endif

          event.xkey.state
            |= x_emacs_to_x_modifiers (FRAME_X_DISPLAY_INFO (f),
                                       extra_keyboard_modifiers);
          modifiers = event.xkey.state;

          /* This will have to go some day...  */

          /* make_lispy_event turns chars into control chars.
             Don't do it here because XLookupString is too eager.  */
          event.xkey.state &= ~ControlMask;
          event.xkey.state &= ~(dpyinfo->meta_mod_mask
                                | dpyinfo->super_mod_mask
                                | dpyinfo->hyper_mod_mask
                                | dpyinfo->alt_mod_mask);

          /* In case Meta is ComposeCharacter,
             clear its status.  According to Markus Ehrnsperger
             Markus.Ehrnsperger@lehrstuhl-bross.physik.uni-muenchen.de
             this enables ComposeCharacter to work whether or
             not it is combined with Meta.  */
          if (modifiers & dpyinfo->meta_mod_mask)
            bzero (&compose_status, sizeof (compose_status));

#ifdef HAVE_X_I18N
          if (FRAME_XIC (f))
            {
              Status status_return;

              coding_system = Vlocale_coding_system;
              nbytes = XmbLookupString (FRAME_XIC (f),
                                        &event.xkey, copy_bufptr,
                                        copy_bufsiz, &keysym,
                                        &status_return);
              if (status_return == XBufferOverflow)
                {
                  copy_bufsiz = nbytes + 1;
                  copy_bufptr = (unsigned char *) alloca (copy_bufsiz);
                  nbytes = XmbLookupString (FRAME_XIC (f),
                                            &event.xkey, copy_bufptr,
                                            copy_bufsiz, &keysym,
                                            &status_return);
                }
              /* Xutf8LookupString is a new but already deprecated interface.  -stef  */
#if 0 && defined X_HAVE_UTF8_STRING
              else if (status_return == XLookupKeySym)
                {  /* Try again but with utf-8.  */
                  coding_system = Qutf_8;
                  nbytes = Xutf8LookupString (FRAME_XIC (f),
                                              &event.xkey, copy_bufptr,
                                              copy_bufsiz, &keysym,
                                              &status_return);
                  if (status_return == XBufferOverflow)
                    {
                      copy_bufsiz = nbytes + 1;
                      copy_bufptr = (unsigned char *) alloca (copy_bufsiz);
                      nbytes = Xutf8LookupString (FRAME_XIC (f),
                                                  &event.xkey,
                                                  copy_bufptr,
                                                  copy_bufsiz, &keysym,
                                                  &status_return);
                    }
                }
#endif

              if (status_return == XLookupNone)
                break;
              else if (status_return == XLookupChars)
                {
                  keysym = NoSymbol;
                  modifiers = 0;
                }
              else if (status_return != XLookupKeySym
                       && status_return != XLookupBoth)
                abort ();
            }
          else
            nbytes = XLookupString (&event.xkey, copy_bufptr,
                                    copy_bufsiz, &keysym,
                                    &compose_status);
#else
          nbytes = XLookupString (&event.xkey, copy_bufptr,
                                  copy_bufsiz, &keysym,
                                  &compose_status);
#endif

          /* If not using XIM/XIC, and a compose sequence is in progress,
             we break here.  Otherwise, chars_matched is always 0.  */
          if (compose_status.chars_matched > 0 && nbytes == 0)
            break;

          bzero (&compose_status, sizeof (compose_status));
          orig_keysym = keysym;

 	  /* Common for all keysym input events.  */
 	  XSETFRAME (inev.ie.frame_or_window, f);
 	  inev.ie.modifiers
 	    = x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f), modifiers);
 	  inev.ie.timestamp = event.xkey.time;

 	  /* First deal with keysyms which have defined
 	     translations to characters.  */
 	  if (keysym >= 32 && keysym < 128)
 	    /* Avoid explicitly decoding each ASCII character.  */
 	    {
 	      inev.ie.kind = ASCII_KEYSTROKE_EVENT;
 	      inev.ie.code = keysym;
	      goto done_keysym;
	    }

	  /* Keysyms directly mapped to Unicode characters.  */
	  if (keysym >= 0x01000000 && keysym <= 0x0110FFFF)
	    {
	      if (keysym < 0x01000080)
		inev.ie.kind = ASCII_KEYSTROKE_EVENT;
	      else
		inev.ie.kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	      inev.ie.code = keysym & 0xFFFFFF;
	      goto done_keysym;
	    }

	  /* Now non-ASCII.  */
	  if (HASH_TABLE_P (Vx_keysym_table)
	      && (NATNUMP (c = Fgethash (make_number (keysym),
 					 Vx_keysym_table,
 					 Qnil))))
 	    {
 	      inev.ie.kind = (SINGLE_BYTE_CHAR_P (XFASTINT (c))
                              ? ASCII_KEYSTROKE_EVENT
                              : MULTIBYTE_CHAR_KEYSTROKE_EVENT);
 	      inev.ie.code = XFASTINT (c);
 	      goto done_keysym;
 	    }
 
 	  /* Random non-modifier sorts of keysyms.  */
 	  if (((keysym >= XK_BackSpace && keysym <= XK_Escape)
                        || keysym == XK_Delete
#ifdef XK_ISO_Left_Tab
                        || (keysym >= XK_ISO_Left_Tab
                            && keysym <= XK_ISO_Enter)
#endif
                        || IsCursorKey (keysym) /* 0xff50 <= x < 0xff60 */
                        || IsMiscFunctionKey (keysym) /* 0xff60 <= x < VARIES */
#ifdef HPUX
                        /* This recognizes the "extended function
                           keys".  It seems there's no cleaner way.
                           Test IsModifierKey to avoid handling
                           mode_switch incorrectly.  */
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
                        || (orig_keysym & (1 << 28))
                        || (keysym != NoSymbol && nbytes == 0))
                       && ! (IsModifierKey (orig_keysym)
                             /* The symbols from XK_ISO_Lock
                                to XK_ISO_Last_Group_Lock
                                don't have real modifiers but
                                should be treated similarly to
                                Mode_switch by Emacs. */
#if defined XK_ISO_Lock && defined XK_ISO_Last_Group_Lock
                             || ((unsigned)(orig_keysym)
                                 >=  XK_ISO_Lock
                                 && (unsigned)(orig_keysym)
                                 <= XK_ISO_Last_Group_Lock)
#endif
                             ))
	    {
	      STORE_KEYSYM_FOR_DEBUG (keysym);
	      /* make_lispy_event will convert this to a symbolic
		 key.  */
	      inev.ie.kind = NON_ASCII_KEYSTROKE_EVENT;
	      inev.ie.code = keysym;
	      goto done_keysym;
	    }

	  {	/* Raw bytes, not keysym.  */
	    register int i;
	    register int c;
	    int nchars, len;

	    for (i = 0, nchars = 0; i < nbytes; i++)
	      {
		if (ASCII_BYTE_P (copy_bufptr[i]))
		  nchars++;
		STORE_KEYSYM_FOR_DEBUG (copy_bufptr[i]);
	      }

	    if (nchars < nbytes)
	      {
		/* Decode the input data.  */
		int require;
		unsigned char *p;

		/* The input should be decoded with `coding_system'
		   which depends on which X*LookupString function
		   we used just above and the locale.  */
		setup_coding_system (coding_system, &coding);
		coding.src_multibyte = 0;
		coding.dst_multibyte = 1;
		/* The input is converted to events, thus we can't
		   handle composition.  Anyway, there's no XIM that
		   gives us composition information.  */
		coding.common_flags &= ~CODING_ANNOTATION_MASK;

		require = MAX_MULTIBYTE_LENGTH * nbytes;
		coding.destination = alloca (require);
		coding.dst_bytes = require;
		coding.mode |= CODING_MODE_LAST_BLOCK;
		decode_coding_c_string (&coding, copy_bufptr, nbytes, Qnil);
		nbytes = coding.produced;
		nchars = coding.produced_char;
		copy_bufptr = coding.destination;
	      }

	    /* Convert the input data to a sequence of
	       character events.  */
	    for (i = 0; i < nbytes; i += len)
	      {
		if (nchars == nbytes)
		  c = copy_bufptr[i], len = 1;
		else
		  c = STRING_CHAR_AND_LENGTH (copy_bufptr + i,
					      nbytes - i, len);
		inev.ie.kind = (SINGLE_BYTE_CHAR_P (c)
				? ASCII_KEYSTROKE_EVENT
				: MULTIBYTE_CHAR_KEYSTROKE_EVENT);
		inev.ie.code = c;
		kbd_buffer_store_event_hold (&inev.ie, hold_quit);
	      }

	    count += nchars;

	    inev.ie.kind = NO_EVENT;  /* Already stored above.  */

	    if (keysym == NoSymbol)
	      break;
	  }
        }
    done_keysym:
#ifdef HAVE_X_I18N
      /* Don't dispatch this event since XtDispatchEvent calls
         XFilterEvent, and two calls in a row may freeze the
         client.  */
      break;
#else
      goto OTHER;
#endif

    case KeyRelease:
      last_user_time = event.xkey.time;
#ifdef HAVE_X_I18N
      /* Don't dispatch this event since XtDispatchEvent calls
         XFilterEvent, and two calls in a row may freeze the
         client.  */
      break;
#else
      goto OTHER;
#endif

    case EnterNotify:
      last_user_time = event.xcrossing.time;
      x_detect_focus_change (dpyinfo, &event, &inev.ie);

      f = x_any_window_to_frame (dpyinfo, event.xcrossing.window);

      if (f && x_mouse_click_focus_ignore_position)
	ignore_next_mouse_click_timeout = event.xmotion.time + 200;

#if 0
      if (event.xcrossing.focus)
	{
	  /* Avoid nasty pop/raise loops.  */
	  if (f && (!(f->auto_raise)
		    || !(f->auto_lower)
		    || (event.xcrossing.time - enter_timestamp) > 500))
	    {
	      x_new_focus_frame (dpyinfo, f);
	      enter_timestamp = event.xcrossing.time;
	    }
	}
      else if (f == dpyinfo->x_focus_frame)
	x_new_focus_frame (dpyinfo, 0);
#endif

      /* EnterNotify counts as mouse movement,
	 so update things that depend on mouse position.  */
      if (f && !f->output_data.x->hourglass_p)
	note_mouse_movement (f, &event.xmotion);
#ifdef USE_GTK
      /* We may get an EnterNotify on the buttons in the toolbar.  In that
         case we moved out of any highlighted area and need to note this.  */
      if (!f && last_mouse_glyph_frame)
        note_mouse_movement (last_mouse_glyph_frame, &event.xmotion);
#endif
      goto OTHER;

    case FocusIn:
      x_detect_focus_change (dpyinfo, &event, &inev.ie);
      goto OTHER;

    case LeaveNotify:
      last_user_time = event.xcrossing.time;
      x_detect_focus_change (dpyinfo, &event, &inev.ie);

      f = x_top_window_to_frame (dpyinfo, event.xcrossing.window);
      if (f)
        {
          if (f == dpyinfo->mouse_face_mouse_frame)
            {
              /* If we move outside the frame, then we're
                 certainly no longer on any text in the frame.  */
              clear_mouse_face (dpyinfo);
              dpyinfo->mouse_face_mouse_frame = 0;
            }

          /* Generate a nil HELP_EVENT to cancel a help-echo.
             Do it only if there's something to cancel.
             Otherwise, the startup message is cleared when
             the mouse leaves the frame.  */
          if (any_help_event_p)
	    do_help = -1;
        }
#ifdef USE_GTK
      /* See comment in EnterNotify above */
      else if (last_mouse_glyph_frame)
        note_mouse_movement (last_mouse_glyph_frame, &event.xmotion);
#endif
      goto OTHER;

    case FocusOut:
      x_detect_focus_change (dpyinfo, &event, &inev.ie);
      goto OTHER;

    case MotionNotify:
      {
        last_user_time = event.xmotion.time;
        previous_help_echo_string = help_echo_string;
        help_echo_string = Qnil;

        if (dpyinfo->grabbed && last_mouse_frame
            && FRAME_LIVE_P (last_mouse_frame))
          f = last_mouse_frame;
        else
          f = x_window_to_frame (dpyinfo, event.xmotion.window);

        if (dpyinfo->mouse_face_hidden)
          {
            dpyinfo->mouse_face_hidden = 0;
            clear_mouse_face (dpyinfo);
          }

        if (f)
          {

            /* Generate SELECT_WINDOW_EVENTs when needed.  */
            if (!NILP (Vmouse_autoselect_window))
              {
                Lisp_Object window;

                window = window_from_coordinates (f,
                                                  event.xmotion.x, event.xmotion.y,
                                                  0, 0, 0, 0);

                /* Window will be selected only when it is not selected now and
                   last mouse movement event was not in it.  Minibuffer window
                   will be selected only when it is active.  */
                if (WINDOWP (window)
                    && !EQ (window, last_window)
		    && !EQ (window, selected_window)
		    /* For click-to-focus window managers
		       create event iff we don't leave the
		       selected frame.  */
		    && (focus_follows_mouse
			|| (EQ (XWINDOW (window)->frame,
				XWINDOW (selected_window)->frame))))
                  {
                    inev.ie.kind = SELECT_WINDOW_EVENT;
                    inev.ie.frame_or_window = window;
                  }

                last_window=window;
              }
            if (!note_mouse_movement (f, &event.xmotion))
	      help_echo_string = previous_help_echo_string;
          }
        else
          {
#ifndef USE_TOOLKIT_SCROLL_BARS
            struct scroll_bar *bar
              = x_window_to_scroll_bar (event.xmotion.display,
                                        event.xmotion.window);

            if (bar)
              x_scroll_bar_note_movement (bar, &event);
#endif /* USE_TOOLKIT_SCROLL_BARS */

            /* If we move outside the frame, then we're
               certainly no longer on any text in the frame.  */
            clear_mouse_face (dpyinfo);
          }

        /* If the contents of the global variable help_echo_string
           has changed, generate a HELP_EVENT.  */
        if (!NILP (help_echo_string)
            || !NILP (previous_help_echo_string))
	  do_help = 1;
        goto OTHER;
      }

    case ConfigureNotify:
      f = x_top_window_to_frame (dpyinfo, event.xconfigure.window);
#ifdef USE_GTK
      if (!f
          && (f = x_any_window_to_frame (dpyinfo, event.xconfigure.window))
          && event.xconfigure.window == FRAME_X_WINDOW (f))
        {
          xg_frame_resized (f, event.xconfigure.width,
                            event.xconfigure.height);
          f = 0;
        }
#endif  
      if (f)
        {
#ifndef USE_X_TOOLKIT
#ifndef USE_GTK
          /* If there is a pending resize for fullscreen, don't
             do this one, the right one will come later.
             The toolkit version doesn't seem to need this, but we
             need to reset it below.  */
          int dont_resize
	    = ((f->want_fullscreen & FULLSCREEN_WAIT)
	       && f->new_text_cols != 0);
          int rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, event.xconfigure.height);
          int columns = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, event.xconfigure.width);

          if (dont_resize)
            goto OTHER;

          /* In the toolkit version, change_frame_size
             is called by the code that handles resizing
             of the EmacsFrame widget.  */

          /* Even if the number of character rows and columns has
             not changed, the font size may have changed, so we need
             to check the pixel dimensions as well.  */
          if (columns != FRAME_COLS (f)
              || rows != FRAME_LINES (f)
              || event.xconfigure.width != FRAME_PIXEL_WIDTH (f)
              || event.xconfigure.height != FRAME_PIXEL_HEIGHT (f))
            {
              change_frame_size (f, rows, columns, 0, 1, 0);
              SET_FRAME_GARBAGED (f);
              cancel_mouse_face (f);
            }

          FRAME_PIXEL_WIDTH (f) = event.xconfigure.width;
          FRAME_PIXEL_HEIGHT (f) = event.xconfigure.height;
#endif /* not USE_GTK */
#endif

#ifdef USE_GTK
          /* GTK creates windows but doesn't map them.
             Only get real positions and check fullscreen when mapped. */
          if (FRAME_GTK_OUTER_WIDGET (f)
              && GTK_WIDGET_MAPPED (FRAME_GTK_OUTER_WIDGET (f)))
#endif
            {
	      x_real_positions (f, &f->left_pos, &f->top_pos);

	      if (f->want_fullscreen & FULLSCREEN_WAIT)
		f->want_fullscreen &= ~(FULLSCREEN_WAIT|FULLSCREEN_BOTH);
            }

#ifdef HAVE_X_I18N
          if (FRAME_XIC (f) && (FRAME_XIC_STYLE (f) & XIMStatusArea))
            xic_set_statusarea (f);
#endif

          if (f->output_data.x->parent_desc != FRAME_X_DISPLAY_INFO (f)->root_window)
            {
              /* Since the WM decorations come below top_pos now,
                 we must put them below top_pos in the future.  */
              f->win_gravity = NorthWestGravity;
              x_wm_set_size_hint (f, (long) 0, 0);
            }
        }
      goto OTHER;

    case ButtonRelease:
    case ButtonPress:
      {
        /* If we decide we want to generate an event to be seen
           by the rest of Emacs, we put it here.  */
        int tool_bar_p = 0;

        bzero (&compose_status, sizeof (compose_status));
	last_mouse_glyph_frame = 0;
        last_user_time = event.xbutton.time;

        if (dpyinfo->grabbed
            && last_mouse_frame
            && FRAME_LIVE_P (last_mouse_frame))
          f = last_mouse_frame;
        else
          f = x_window_to_frame (dpyinfo, event.xbutton.window);

        if (f)
          {
            /* Is this in the tool-bar?  */
            if (WINDOWP (f->tool_bar_window)
                && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
              {
                Lisp_Object window;
                int x = event.xbutton.x;
                int y = event.xbutton.y;

                window = window_from_coordinates (f, x, y, 0, 0, 0, 1);
                tool_bar_p = EQ (window, f->tool_bar_window);

                if (tool_bar_p && event.xbutton.button < 4)
                  {
		    handle_tool_bar_click (f, x, y,
					   event.xbutton.type == ButtonPress,
					   x_x_to_emacs_modifiers (dpyinfo,
								   event.xbutton.state));
                  }
              }

            if (!tool_bar_p)
#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
              if (! popup_activated ())
#endif
                {
                  if (ignore_next_mouse_click_timeout)
                    {
                      if (event.type == ButtonPress
                          && (int)(event.xbutton.time - ignore_next_mouse_click_timeout) > 0)
                        {
                          ignore_next_mouse_click_timeout = 0;
                          construct_mouse_click (&inev.ie, &event.xbutton, f);
                        }
                      if (event.type == ButtonRelease)
                        ignore_next_mouse_click_timeout = 0;
                    }
                  else
                    construct_mouse_click (&inev.ie, &event.xbutton, f);
                }
            if (FRAME_X_EMBEDDED_P (f))
              xembed_send_message (f, event.xbutton.time,
                                   XEMBED_REQUEST_FOCUS, 0, 0, 0);
          }
        else
          {
            struct scroll_bar *bar
              = x_window_to_scroll_bar (event.xbutton.display,
                                        event.xbutton.window);

#ifdef USE_TOOLKIT_SCROLL_BARS
            /* Make the "Ctrl-Mouse-2 splits window" work for toolkit
               scroll bars.  */
            if (bar && event.xbutton.state & ControlMask)
              {
                x_scroll_bar_handle_click (bar, &event, &inev.ie);
                *finish = X_EVENT_DROP;
              }
#else /* not USE_TOOLKIT_SCROLL_BARS */
            if (bar)
              x_scroll_bar_handle_click (bar, &event, &inev.ie);
#endif /* not USE_TOOLKIT_SCROLL_BARS */
          }

        if (event.type == ButtonPress)
          {
            dpyinfo->grabbed |= (1 << event.xbutton.button);
            last_mouse_frame = f;

            if (!tool_bar_p)
              last_tool_bar_item = -1;
          }
        else
          dpyinfo->grabbed &= ~(1 << event.xbutton.button);

	/* Ignore any mouse motion that happened before this event;
	   any subsequent mouse-movement Emacs events should reflect
	   only motion after the ButtonPress/Release.  */
	if (f != 0)
	  f->mouse_moved = 0;

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
        f = x_menubar_window_to_frame (dpyinfo, event.xbutton.window);
        /* For a down-event in the menu bar,
           don't pass it to Xt right now.
           Instead, save it away
           and we will pass it to Xt from kbd_buffer_get_event.
           That way, we can run some Lisp code first.  */
        if (
#ifdef USE_GTK
            ! popup_activated ()
            /* Gtk+ menus only react to the first three buttons. */
            && event.xbutton.button < 3
            &&
#endif
            f && event.type == ButtonPress
            /* Verify the event is really within the menu bar
               and not just sent to it due to grabbing.  */
            && event.xbutton.x >= 0
            && event.xbutton.x < FRAME_PIXEL_WIDTH (f)
            && event.xbutton.y >= 0
            && event.xbutton.y < f->output_data.x->menubar_height
            && event.xbutton.same_screen)
          {
            SET_SAVED_BUTTON_EVENT;
            XSETFRAME (last_mouse_press_frame, f);
#ifdef USE_GTK
            *finish = X_EVENT_DROP;
#endif
          }
        else if (event.type == ButtonPress)
          {
            last_mouse_press_frame = Qnil;
            goto OTHER;
          }

#ifdef USE_MOTIF /* This should do not harm for Lucid,
		    but I am trying to be cautious.  */
        else if (event.type == ButtonRelease)
          {
            if (!NILP (last_mouse_press_frame))
              {
                f = XFRAME (last_mouse_press_frame);
                if (f->output_data.x)
                  SET_SAVED_BUTTON_EVENT;
              }
            else
              goto OTHER;
          }
#endif /* USE_MOTIF */
        else
          goto OTHER;
#endif /* USE_X_TOOLKIT || USE_GTK */
      }
      break;

    case CirculateNotify:
      goto OTHER;

    case CirculateRequest:
      goto OTHER;

    case VisibilityNotify:
      goto OTHER;

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
      goto OTHER;

    default:
    OTHER:
#ifdef USE_X_TOOLKIT
    BLOCK_INPUT;
    if (*finish != X_EVENT_DROP)
      XtDispatchEvent (&event);
    UNBLOCK_INPUT;
#endif /* USE_X_TOOLKIT */
    break;
    }

 done:
  if (inev.ie.kind != NO_EVENT)
    {
      kbd_buffer_store_event_hold (&inev.ie, hold_quit);
      count++;
    }

  if (do_help
      && !(hold_quit && hold_quit->kind != NO_EVENT))
    {
      Lisp_Object frame;

      if (f)
	XSETFRAME (frame, f);
      else
	frame = Qnil;

      if (do_help > 0)
	{
	  any_help_event_p = 1;
	  gen_help_event (help_echo_string, frame, help_echo_window,
			  help_echo_object, help_echo_pos);
	}
      else
	{
	  help_echo_string = Qnil;
	  gen_help_event (Qnil, frame, Qnil, Qnil, 0);
	}
      count++;
    }

  *eventp = event;
  return count;
}


/* Handles the XEvent EVENT on display DISPLAY.
   This is used for event loops outside the normal event handling,
   i.e. looping while a popup menu or a dialog is posted.

   Returns the value handle_one_xevent sets in the finish argument.  */
int
x_dispatch_event (event, display)
     XEvent *event;
     Display *display;
{
  struct x_display_info *dpyinfo;
  int finish = X_EVENT_NORMAL;

  dpyinfo = x_display_info_for_display (display);

  if (dpyinfo)
    handle_one_xevent (dpyinfo, event, &finish, 0);

  return finish;
}


/* Read events coming from the X server.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   We return the number of characters stored into the buffer,
   thus pretending to be `read' (except the characters we store
   in the keyboard buffer can be multibyte, so are not necessarily
   C chars).

   EXPECTED is nonzero if the caller knows input is available.  */

static int
XTread_socket (terminal, expected, hold_quit)
     struct terminal *terminal;
     int expected;
     struct input_event *hold_quit;
{
  int count = 0;
  XEvent event;
  int event_found = 0;
#if 0
  struct x_display_info *dpyinfo;
#endif

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
  BLOCK_INPUT;

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  ++handling_signal;

#ifdef HAVE_X_SM
  /* Only check session manager input for the primary display. */
  if (terminal->id == 1 && x_session_have_connection ())
    {
      struct input_event inev;
      BLOCK_INPUT;
      /* We don't need to EVENT_INIT (inev) here, as
         x_session_check_input copies an entire input_event.  */
      if (x_session_check_input (&inev))
        {
          kbd_buffer_store_event_hold (&inev, hold_quit);
          count++;
        }
      UNBLOCK_INPUT;
    }
#endif

  /* For debugging, this gives a way to fake an I/O error.  */
  if (terminal->display_info.x == XTread_socket_fake_io_error)
    {
      XTread_socket_fake_io_error = 0;
      x_io_error_quitter (terminal->display_info.x->display);
    }
  
#if 0 /* This loop is a noop now.  */
  /* Find the display we are supposed to read input for.
     It's the one communicating on descriptor SD.  */
  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    {
#if 0 /* This ought to be unnecessary; let's verify it.  */
#ifdef FIOSNBIO
      /* If available, Xlib uses FIOSNBIO to make the socket
	 non-blocking, and then looks for EWOULDBLOCK.  If O_NDELAY is set,
	 FIOSNBIO is ignored, and instead of signaling EWOULDBLOCK,
	 a read returns 0, which Xlib interprets as equivalent to EPIPE.  */
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
    }
#endif

#ifndef USE_GTK
  while (XPending (terminal->display_info.x->display))
    {
      int finish;

      XNextEvent (terminal->display_info.x->display, &event);

#ifdef HAVE_X_I18N
      /* Filter events for the current X input method.  */
      if (x_filter_event (terminal->display_info.x, &event))
        break;
#endif
      event_found = 1;

      count += handle_one_xevent (terminal->display_info.x,
                                  &event, &finish, hold_quit);

      if (finish == X_EVENT_GOTO_OUT)
        goto out;
    }

#else /* USE_GTK */

  /* For GTK we must use the GTK event loop.  But XEvents gets passed
     to our filter function above, and then to the big event switch.
     We use a bunch of globals to communicate with our filter function,
     that is kind of ugly, but it works.

     There is no way to do one display at the time, GTK just does events
     from all displays.  */

  while (gtk_events_pending ())
    {
      current_count = count;
      current_hold_quit = hold_quit;

      gtk_main_iteration ();

      count = current_count;
      current_count = -1;
      current_hold_quit = 0;

      if (current_finish == X_EVENT_GOTO_OUT)
        break;
    }
#endif /* USE_GTK */

 out:;

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

	  if (next_noop_dpyinfo == 0)
	    next_noop_dpyinfo = x_display_list;

	  XNoOp (next_noop_dpyinfo->display);

	  /* Each time we get here, cycle through the displays now open.  */
	  next_noop_dpyinfo = next_noop_dpyinfo->next;
	}
    }

  /* If the focus was just given to an auto-raising frame,
     raise it now.  */
  /* ??? This ought to be able to handle more than one such frame.  */
  if (pending_autoraise_frame)
    {
      x_raise_frame (pending_autoraise_frame);
      pending_autoraise_frame = 0;
    }

  --handling_signal;
  UNBLOCK_INPUT;

  return count;
}




/***********************************************************************
			     Text Cursor
 ***********************************************************************/

/* Set clipping for output in glyph row ROW.  W is the window in which
   we operate.  GC is the graphics context to set clipping in.

   ROW may be a text row or, e.g., a mode line.  Text rows must be
   clipped to the interior of the window dedicated to text display,
   mode lines must be clipped to the whole window.  */

static void
x_clip_to_row (w, row, area, gc)
     struct window *w;
     struct glyph_row *row;
     int area;
     GC gc;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  XRectangle clip_rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  clip_rect.x = window_x;
  clip_rect.y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  clip_rect.y = max (clip_rect.y, window_y);
  clip_rect.width = window_width;
  clip_rect.height = row->visible_height;

  XSetClipRectangles (FRAME_X_DISPLAY (f), gc, 0, 0, &clip_rect, 1, Unsorted);
}


/* Draw a hollow box cursor on window W in glyph row ROW.  */

static void
x_draw_hollow_cursor (w, row)
     struct window *w;
     struct glyph_row *row;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Display *dpy = FRAME_X_DISPLAY (f);
  int x, y, wd, h;
  XGCValues xgcv;
  struct glyph *cursor_glyph;
  GC gc;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute frame-relative coordinates for phys cursor.  */
  get_phys_cursor_geometry (w, row, cursor_glyph, &x, &y, &h);
  wd = w->phys_cursor_width;

  /* The foreground of cursor_gc is typically the same as the normal
     background color, which can cause the cursor box to be invisible.  */
  xgcv.foreground = f->output_data.x->cursor_pixel;
  if (dpyinfo->scratch_cursor_gc)
    XChangeGC (dpy, dpyinfo->scratch_cursor_gc, GCForeground, &xgcv);
  else
    dpyinfo->scratch_cursor_gc = XCreateGC (dpy, FRAME_X_WINDOW (f),
					    GCForeground, &xgcv);
  gc = dpyinfo->scratch_cursor_gc;

  /* Set clipping, draw the rectangle, and reset clipping again.  */
  x_clip_to_row (w, row, TEXT_AREA, gc);
  XDrawRectangle (dpy, FRAME_X_WINDOW (f), gc, x, y, wd, h - 1);
  XSetClipMask (dpy, gc, None);
}


/* Draw a bar cursor on window W in glyph row ROW.

   Implementation note: One would like to draw a bar cursor with an
   angle equal to the one given by the font property XA_ITALIC_ANGLE.
   Unfortunately, I didn't find a font yet that has this property set.
   --gerd.  */

static void
x_draw_bar_cursor (w, row, width, kind)
     struct window *w;
     struct glyph_row *row;
     int width;
     enum text_cursor_kinds kind;
{
  struct frame *f = XFRAME (w->frame);
  struct glyph *cursor_glyph;

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      struct glyph_row *row;
      row = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      draw_phys_cursor_glyph (w, row, DRAW_CURSOR);
    }
  else
    {
      Display *dpy = FRAME_X_DISPLAY (f);
      Window window = FRAME_X_WINDOW (f);
      GC gc = FRAME_X_DISPLAY_INFO (f)->scratch_cursor_gc;
      unsigned long mask = GCForeground | GCBackground | GCGraphicsExposures;
      struct face *face = FACE_FROM_ID (f, cursor_glyph->face_id);
      XGCValues xgcv;

      /* If the glyph's background equals the color we normally draw
	 the bar cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == f->output_data.x->cursor_pixel)
	xgcv.background = xgcv.foreground = face->foreground;
      else
	xgcv.background = xgcv.foreground = f->output_data.x->cursor_pixel;
      xgcv.graphics_exposures = 0;

      if (gc)
	XChangeGC (dpy, gc, mask, &xgcv);
      else
	{
	  gc = XCreateGC (dpy, window, mask, &xgcv);
	  FRAME_X_DISPLAY_INFO (f)->scratch_cursor_gc = gc;
	}

      if (width < 0)
	width = FRAME_CURSOR_WIDTH (f);
      width = min (cursor_glyph->pixel_width, width);

      w->phys_cursor_width = width;
      x_clip_to_row (w, row, TEXT_AREA, gc);

      if (kind == BAR_CURSOR)
	  XFillRectangle (dpy, window, gc,
			  WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x),
			  WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
			  width, row->height);
      else
	  XFillRectangle (dpy, window, gc,
			  WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x),
			  WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y +
						   row->height - width),
			  cursor_glyph->pixel_width,
			  width);

      XSetClipMask (dpy, gc, None);
    }
}


/* RIF: Define cursor CURSOR on frame F.  */

static void
x_define_frame_cursor (f, cursor)
     struct frame *f;
     Cursor cursor;
{
  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), cursor);
}


/* RIF: Clear area on frame F.  */

static void
x_clear_frame_area (f, x, y, width, height)
     struct frame *f;
     int x, y, width, height;
{
  x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		x, y, width, height, False);
}


/* RIF: Draw cursor on window W.  */

static void
x_draw_window_cursor (w, glyph_row, x, y, cursor_type, cursor_width, on_p, active_p)
     struct window *w;
     struct glyph_row *glyph_row;
     int x, y;
     int cursor_type, cursor_width;
     int on_p, active_p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));

  if (on_p)
    {
      w->phys_cursor_type = cursor_type;
      w->phys_cursor_on_p = 1;

      if (glyph_row->exact_window_width_line_p
	  && w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])
	{
	  glyph_row->cursor_in_fringe_p = 1;
	  draw_fringe_bitmap (w, glyph_row, 0);
	}
      else
      switch (cursor_type)
	{
	case HOLLOW_BOX_CURSOR:
	  x_draw_hollow_cursor (w, glyph_row);
	  break;

	case FILLED_BOX_CURSOR:
	  draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	  break;

	case BAR_CURSOR:
	  x_draw_bar_cursor (w, glyph_row, cursor_width, BAR_CURSOR);
	  break;

	case HBAR_CURSOR:
	  x_draw_bar_cursor (w, glyph_row, cursor_width, HBAR_CURSOR);
	  break;

	case NO_CURSOR:
	  w->phys_cursor_width = 0;
	  break;

	default:
	  abort ();
	}

#ifdef HAVE_X_I18N
      if (w == XWINDOW (f->selected_window))
	if (FRAME_XIC (f) && (FRAME_XIC_STYLE (f) & XIMPreeditPosition))
	  xic_set_preeditarea (w, x, y);
#endif
    }

#ifndef XFlush
  XFlush (FRAME_X_DISPLAY (f));
#endif
}


/* Icons.  */

/* Make the x-window of frame F use the gnu icon bitmap.  */

int
x_bitmap_icon (f, file)
     struct frame *f;
     Lisp_Object file;
{
  int bitmap_id;

  if (FRAME_X_WINDOW (f) == 0)
    return 1;

  /* Free up our existing icon bitmap and mask if any.  */
  if (f->output_data.x->icon_bitmap > 0)
    x_destroy_bitmap (f, f->output_data.x->icon_bitmap);
  f->output_data.x->icon_bitmap = 0;

  if (STRINGP (file))
    {
#ifdef USE_GTK
      /* Use gtk_window_set_icon_from_file () if available,
	 It's not restricted to bitmaps */
      if (xg_set_icon (f, file))
	return 0;
#endif /* USE_GTK */
      bitmap_id = x_create_bitmap_from_file (f, file);
      x_create_bitmap_mask (f, bitmap_id);
    }
  else
    {
      /* Create the GNU bitmap and mask if necessary.  */
      if (FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id < 0)
	{
	  int rc = -1;

#if defined (HAVE_XPM) && defined (HAVE_X_WINDOWS)
#ifdef USE_GTK
	  if (xg_set_icon_from_xpm_data (f, gnu_xpm_bits))
	    return 0;
#else
	  rc = x_create_bitmap_from_xpm_data (f, gnu_xpm_bits);
	  if (rc != -1)
	    FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id = rc;
#endif /* USE_GTK */
#endif /* defined (HAVE_XPM) && defined (HAVE_X_WINDOWS) */

	  /* If all else fails, use the (black and white) xbm image. */
	  if (rc == -1)
	    {
	      rc = x_create_bitmap_from_data (f, gnu_xbm_bits,
					      gnu_xbm_width, gnu_xbm_height);
	      if (rc == -1)
		return 1;

	      FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id = rc;
	      x_create_bitmap_mask (f, FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id);
	    }
	}

      /* The first time we create the GNU bitmap and mask,
	 this increments the ref-count one extra time.
	 As a result, the GNU bitmap and mask are never freed.
	 That way, we don't have to worry about allocating it again.  */
      x_reference_bitmap (f, FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id);

      bitmap_id = FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id;
    }

  x_wm_set_icon_pixmap (f, bitmap_id);
  f->output_data.x->icon_bitmap = bitmap_id;

  return 0;
}


/* Make the x-window of frame F use a rectangle with text.
   Use ICON_NAME as the text.  */

int
x_text_icon (f, icon_name)
     struct frame *f;
     char *icon_name;
{
  if (FRAME_X_WINDOW (f) == 0)
    return 1;

  {
    XTextProperty text;
    text.value = (unsigned char *) icon_name;
    text.encoding = XA_STRING;
    text.format = 8;
    text.nitems = strlen (icon_name);
    XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), &text);
  }

  if (f->output_data.x->icon_bitmap > 0)
    x_destroy_bitmap (f, f->output_data.x->icon_bitmap);
  f->output_data.x->icon_bitmap = 0;
  x_wm_set_icon_pixmap (f, 0);

  return 0;
}

#define X_ERROR_MESSAGE_SIZE 200

/* If non-nil, this should be a string.
   It means catch X errors  and store the error message in this string.

   The reason we use a stack is that x_catch_error/x_uncatch_error can
   be called from a signal handler.
*/

struct x_error_message_stack {
  char string[X_ERROR_MESSAGE_SIZE];
  Display *dpy;
  struct x_error_message_stack *prev;
};
static struct x_error_message_stack *x_error_message;

/* An X error handler which stores the error message in
   *x_error_message.  This is called from x_error_handler if
   x_catch_errors is in effect.  */

static void
x_error_catcher (display, error)
     Display *display;
     XErrorEvent *error;
{
  XGetErrorText (display, error->error_code,
		 x_error_message->string,
		 X_ERROR_MESSAGE_SIZE);
}

/* Begin trapping X errors for display DPY.  Actually we trap X errors
   for all displays, but DPY should be the display you are actually
   operating on.

   After calling this function, X protocol errors no longer cause
   Emacs to exit; instead, they are recorded in the string
   stored in *x_error_message.

   Calling x_check_errors signals an Emacs error if an X error has
   occurred since the last call to x_catch_errors or x_check_errors.

   Calling x_uncatch_errors resumes the normal error handling.  */

void x_check_errors ();

void
x_catch_errors (dpy)
     Display *dpy;
{
  struct x_error_message_stack *data = xmalloc (sizeof (*data));

  /* Make sure any errors from previous requests have been dealt with.  */
  XSync (dpy, False);

  data->dpy = dpy;
  data->string[0] = 0;
  data->prev = x_error_message;
  x_error_message = data;
}

/* Undo the last x_catch_errors call.
   DPY should be the display that was passed to x_catch_errors.  */

void
x_uncatch_errors ()
{
  struct x_error_message_stack *tmp;

  BLOCK_INPUT;

  /* The display may have been closed before this function is called.
     Check if it is still open before calling XSync.  */
  if (x_display_info_for_display (x_error_message->dpy) != 0)
    XSync (x_error_message->dpy, False);

  tmp = x_error_message;
  x_error_message = x_error_message->prev;
  xfree (tmp);
  UNBLOCK_INPUT;
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

  if (x_error_message->string[0])
    {
      char string[X_ERROR_MESSAGE_SIZE];
      bcopy (x_error_message->string, string, X_ERROR_MESSAGE_SIZE);
      x_uncatch_errors ();
      error (format, string);
    }
}

/* Nonzero if we had any X protocol errors
   since we did x_catch_errors on DPY.  */

int
x_had_errors_p (dpy)
     Display *dpy;
{
  /* Make sure to catch any errors incurred so far.  */
  XSync (dpy, False);

  return x_error_message->string[0] != 0;
}

/* Forget about any errors we have had, since we did x_catch_errors on DPY.  */

void
x_clear_errors (dpy)
     Display *dpy;
{
  x_error_message->string[0] = 0;
}

#if 0 /* See comment in unwind_to_catch why calling this is a bad
       * idea.  --lorentey   */
/* Close off all unclosed x_catch_errors calls.  */

void
x_fully_uncatch_errors ()
{
  while (x_error_message)
    x_uncatch_errors ();
}
#endif

/* Nonzero if x_catch_errors has been done and not yet canceled.  */

int
x_catching_errors ()
{
  return x_error_message != 0;
}

#if 0
static unsigned int x_wire_count;
x_trace_wire ()
{
  fprintf (stderr, "Lib call: %d\n", ++x_wire_count);
}
#endif /* ! 0 */


/* Handle SIGPIPE, which can happen when the connection to a server
   simply goes away.  SIGPIPE is handled by x_connection_signal.
   Don't need to do anything, because the write which caused the
   SIGPIPE will fail, causing Xlib to invoke the X IO error handler,
   which will do the appropriate cleanup for us.  */

static SIGTYPE
x_connection_signal (signalnum)	/* If we don't have an argument, */
     int signalnum;		/* some compilers complain in signal calls.  */
{
#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signalnum, x_connection_signal);
#endif /* USG */
}


/************************************************************************
			  Handling X errors
 ************************************************************************/

/* Error message passed to x_connection_closed.  */

static char *error_msg;

/* Function installed as fatal_error_signal_hook in
   x_connection_closed.  Print the X error message, and exit normally,
   instead of dumping core when XtCloseDisplay fails.  */

static void
x_fatal_error_signal ()
{
  fprintf (stderr, "%s\n", error_msg);
  exit (70);
}

/* Handle the loss of connection to display DPY.  ERROR_MESSAGE is
   the text of an error message that lead to the connection loss.  */

static SIGTYPE
x_connection_closed (dpy, error_message)
     Display *dpy;
     char *error_message;
{
  struct x_display_info *dpyinfo = x_display_info_for_display (dpy);
  Lisp_Object frame, tail;
  int index = SPECPDL_INDEX ();

  error_msg = (char *) alloca (strlen (error_message) + 1);
  strcpy (error_msg, error_message);
  handling_signal = 0;

  /* Prevent being called recursively because of an error condition
     below.  Otherwise, we might end up with printing ``can't find per
     display information'' in the recursive call instead of printing
     the original message here.  */
  x_catch_errors (dpy);

  /* Inhibit redisplay while frames are being deleted. */
  specbind (Qinhibit_redisplay, Qt);

  if (dpyinfo)
    {
      /* Protect display from being closed when we delete the last
         frame on it. */
      dpyinfo->reference_count++;
      dpyinfo->terminal->reference_count++;
    }
  
  /* First delete frames whose mini-buffers are on frames
     that are on the dead display.  */
  FOR_EACH_FRAME (tail, frame)
    {
      Lisp_Object minibuf_frame;
      minibuf_frame
	= WINDOW_FRAME (XWINDOW (FRAME_MINIBUF_WINDOW (XFRAME (frame))));
      if (FRAME_X_P (XFRAME (frame))
	  && FRAME_X_P (XFRAME (minibuf_frame))
	  && ! EQ (frame, minibuf_frame)
	  && FRAME_X_DISPLAY_INFO (XFRAME (minibuf_frame)) == dpyinfo)
	Fdelete_frame (frame, Qnoelisp);
    }

  /* Now delete all remaining frames on the dead display.
     We are now sure none of these is used as the mini-buffer
     for another frame that we need to delete.  */
  FOR_EACH_FRAME (tail, frame)
    if (FRAME_X_P (XFRAME (frame))
	&& FRAME_X_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
      {
	/* Set this to t so that Fdelete_frame won't get confused
	   trying to find a replacement.  */
	FRAME_KBOARD (XFRAME (frame))->Vdefault_minibuffer_frame = Qt;
	Fdelete_frame (frame, Qnoelisp);
      }

  /* We have to close the display to inform Xt that it doesn't
     exist anymore.  If we don't, Xt will continue to wait for
     events from the display.  As a consequence, a sequence of

     M-x make-frame-on-display RET :1 RET
     ...kill the new frame, so that we get an IO error...
     M-x make-frame-on-display RET :1 RET

     will indefinitely wait in Xt for events for display `:1', opened
     in the first call to make-frame-on-display.

     Closing the display is reported to lead to a bus error on
     OpenWindows in certain situations.  I suspect that is a bug
     in OpenWindows.  I don't know how to circumvent it here.  */

  if (dpyinfo)
    {
#ifdef USE_X_TOOLKIT
      /* If DPYINFO is null, this means we didn't open the display
	 in the first place, so don't try to close it.  */
      {
	extern void (*fatal_error_signal_hook) P_ ((void));
	fatal_error_signal_hook = x_fatal_error_signal;
	XtCloseDisplay (dpy);
	fatal_error_signal_hook = NULL;
      }
#endif

#ifdef USE_GTK
      /* Due to bugs in some Gtk+ versions, just exit here if this
         is the last display/terminal. */
      if (terminal_list->next_terminal == NULL)
        {
          fprintf (stderr, "%s\n", error_msg);
          shut_down_emacs (0, 0, Qnil);
          exit (70);
        }
      xg_display_close (dpyinfo->display);
#endif

      /* Indicate that this display is dead.  */
      dpyinfo->display = 0;

      dpyinfo->reference_count--;
      dpyinfo->terminal->reference_count--;
      if (dpyinfo->reference_count != 0)
        /* We have just closed all frames on this display. */
        abort ();

      {
	Lisp_Object tmp;
	XSETTERMINAL (tmp, dpyinfo->terminal);
	Fdelete_terminal (tmp, Qnoelisp);
      }
    }

  x_uncatch_errors ();

  if (terminal_list == 0)
    {
      fprintf (stderr, "%s\n", error_msg);
      shut_down_emacs (0, 0, Qnil);
      exit (70);
    }

  /* Ordinary stack unwind doesn't deal with these.  */
#ifdef SIGIO
  sigunblock (sigmask (SIGIO));
#endif
  sigunblock (sigmask (SIGALRM));
  TOTALLY_UNBLOCK_INPUT;

  unbind_to (index, Qnil);
  clear_waiting_for_input ();
  /* Here, we absolutely have to use a non-local exit (e.g. signal, throw,
     longjmp), because returning from this function would get us back into
     Xlib's code which will directly call `exit'.  */
  error ("%s", error_msg);
}

/* We specifically use it before defining it, so that gcc doesn't inline it,
   otherwise gdb doesn't know how to properly put a breakpoint on it.  */
static void x_error_quitter P_ ((Display *, XErrorEvent *));

/* This is the first-level handler for X protocol errors.
   It calls x_error_quitter or x_error_catcher.  */

static int
x_error_handler (display, error)
     Display *display;
     XErrorEvent *error;
{
  if (x_error_message)
    x_error_catcher (display, error);
  else
    x_error_quitter (display, error);
  return 0;
}

/* This is the usual handler for X protocol errors.
   It kills all frames on the display that we got the error for.
   If that was the only one, it prints an error message and kills Emacs.  */

/* .gdbinit puts a breakpoint here, so make sure it is not inlined.  */

#if __GNUC__ >= 3  /* On GCC 3.0 we might get a warning.  */
#define NO_INLINE __attribute__((noinline))
#else
#define NO_INLINE
#endif

/* Some versions of GNU/Linux define noinline in their headers.  */

#ifdef noinline
#undef noinline
#endif

/* On older GCC versions, just putting x_error_quitter
   after x_error_handler prevents inlining into the former.  */

static void NO_INLINE
x_error_quitter (display, error)
     Display *display;
     XErrorEvent *error;
{
  char buf[256], buf1[356];

  /* Ignore BadName errors.  They can happen because of fonts
     or colors that are not defined.  */

  if (error->error_code == BadName)
    return;

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
  return 0;
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
  struct font_info *fontp
    = FS_LOAD_FONT (f, fontname);

  if (!fontp)
    return Qnil;

  if (FRAME_FONT (f) == (XFontStruct *) (fontp->font))
    /* This font is already set in frame F.  There's nothing more to
       do.  */
    return build_string (fontp->full_name);

  FRAME_FONT (f) = (XFontStruct *) (fontp->font);
  FRAME_BASELINE_OFFSET (f) = fontp->baseline_offset;
  FRAME_FONTSET (f) = -1;

  FRAME_COLUMN_WIDTH (f) = fontp->average_width;
  FRAME_SPACE_WIDTH (f) = fontp->space_width;
  FRAME_LINE_HEIGHT (f) = FONT_HEIGHT (FRAME_FONT (f));

  compute_fringe_widths (f, 1);

  /* Compute the scroll bar width in character columns.  */
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f)
	= (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + wid-1) / wid;
    }
  else
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_X_WINDOW (f) != 0)
    {
      Font fid;

#ifdef USE_FONT_BACKEND
      if (enable_font_backend)
	fid = FRAME_X_DISPLAY_INFO (f)->font->fid;
      else
#endif
      fid = FRAME_FONT (f)->fid;
      XSetFont (FRAME_X_DISPLAY (f), f->output_data.x->normal_gc, fid);
      XSetFont (FRAME_X_DISPLAY (f), f->output_data.x->reverse_gc, fid);
      XSetFont (FRAME_X_DISPLAY (f), f->output_data.x->cursor_gc, fid);

      /* Don't change the size of a tip frame; there's no point in
	 doing it because it's done in Fx_show_tip, and it leads to
	 problems because the tip frame has no widget.  */
      if (NILP (tip_frame) || XFRAME (tip_frame) != f)
	x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
    }

  return build_string (fontp->full_name);
}

/* Give frame F the fontset named FONTSETNAME as its default fontset,
   and return the full name of that fontset.  FONTSETNAME may be a
   wildcard pattern; in that case, we choose some fontset that fits
   the pattern.  FONTSETNAME may be a font name for ASCII characters;
   in that case, we create a fontset from that font name.

   The return value shows which fontset we chose.
   If FONTSETNAME specifies the default fontset, return Qt.
   If an ASCII font in the specified fontset can't be loaded, return
   Qnil.  */

Lisp_Object
x_new_fontset (f, fontsetname)
     struct frame *f;
     Lisp_Object fontsetname;
{
  int fontset = fs_query_fontset (fontsetname, 0);
  Lisp_Object result;

  if (fontset > 0 && f->output_data.x->fontset == fontset)
    /* This fontset is already set in frame F.  There's nothing more
       to do.  */
    return fontset_name (fontset);
  else if (fontset == 0)
    /* The default fontset can't be the default font.   */
    return Qt;

  if (fontset > 0)
    result = x_new_font (f, (SDATA (fontset_ascii (fontset))));
  else
    result = x_new_font (f, SDATA (fontsetname));

  if (!STRINGP (result))
    /* Can't load ASCII font.  */
    return Qnil;

  if (fontset < 0)
    fontset = new_fontset_from_font_name (result);

  /* Since x_new_font doesn't update any fontset information, do it now.  */
  FRAME_FONTSET (f) = fontset;

#ifdef HAVE_X_I18N
  if (FRAME_XIC (f)
      && (FRAME_XIC_STYLE (f) & (XIMPreeditPosition | XIMStatusArea)))
    xic_set_xfontset (f, SDATA (fontset_ascii (fontset)));
#endif

  return fontset_name (fontset);
}

#ifdef USE_FONT_BACKEND
Lisp_Object
x_new_fontset2 (f, fontset, font_object)
     struct frame *f;
     int fontset;
     Lisp_Object font_object;
{
  struct font *font = XSAVE_VALUE (font_object)->pointer;

  if (FRAME_FONT_OBJECT (f) == font)
    /* This font is already set in frame F.  There's nothing more to
       do.  */
    return fontset_name (fontset);

  BLOCK_INPUT;

  FRAME_FONT_OBJECT (f) = font;
  FRAME_FONT (f) = font->font.font;
  FRAME_BASELINE_OFFSET (f) = font->font.baseline_offset;
  FRAME_FONTSET (f) = fontset;

  FRAME_COLUMN_WIDTH (f) = font->font.average_width;
  FRAME_SPACE_WIDTH (f) = font->font.space_width;
  FRAME_LINE_HEIGHT (f) = font->font.height;

  compute_fringe_widths (f, 1);

  /* Compute the scroll bar width in character columns.  */
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f)
	= (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + wid - 1) / wid;
    }
  else
    {
      int wid = FRAME_COLUMN_WIDTH (f);
      FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_X_WINDOW (f) != 0)
    {
      /* Don't change the size of a tip frame; there's no point in
	 doing it because it's done in Fx_show_tip, and it leads to
	 problems because the tip frame has no widget.  */
      if (NILP (tip_frame) || XFRAME (tip_frame) != f)
	x_set_window_size (f, 0, FRAME_COLS (f), FRAME_LINES (f));
    }

#ifdef HAVE_X_I18N
  if (FRAME_XIC (f)
      && (FRAME_XIC_STYLE (f) & (XIMPreeditPosition | XIMStatusArea)))
    xic_set_xfontset (f, SDATA (fontset_ascii (fontset)));
#endif

  UNBLOCK_INPUT;

  return fontset_name (fontset);
}
#endif	/* USE_FONT_BACKEND */


/***********************************************************************
			   X Input Methods
 ***********************************************************************/

#ifdef HAVE_X_I18N

#ifdef HAVE_X11R6

/* XIM destroy callback function, which is called whenever the
   connection to input method XIM dies.  CLIENT_DATA contains a
   pointer to the x_display_info structure corresponding to XIM.  */

static void
xim_destroy_callback (xim, client_data, call_data)
     XIM xim;
     XPointer client_data;
     XPointer call_data;
{
  struct x_display_info *dpyinfo = (struct x_display_info *) client_data;
  Lisp_Object frame, tail;

  BLOCK_INPUT;

  /* No need to call XDestroyIC.. */
  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_X_P (f) && FRAME_X_DISPLAY_INFO (f) == dpyinfo)
	{
	  FRAME_XIC (f) = NULL;
          xic_free_xfontset (f);
	}
    }

  /* No need to call XCloseIM.  */
  dpyinfo->xim = NULL;
  XFree (dpyinfo->xim_styles);
  UNBLOCK_INPUT;
}

#endif /* HAVE_X11R6 */

#ifdef HAVE_X11R6
/* This isn't prototyped in OSF 5.0 or 5.1a.  */
extern char *XSetIMValues P_ ((XIM, ...));
#endif

/* Open the connection to the XIM server on display DPYINFO.
   RESOURCE_NAME is the resource name Emacs uses.  */

static void
xim_open_dpy (dpyinfo, resource_name)
     struct x_display_info *dpyinfo;
     char *resource_name;
{
  XIM xim;

#ifdef HAVE_XIM
  if (use_xim)
    {
      xim = XOpenIM (dpyinfo->display, dpyinfo->xrdb, resource_name,
		     EMACS_CLASS);
      dpyinfo->xim = xim;

      if (xim)
	{
#ifdef HAVE_X11R6
	  XIMCallback destroy;
#endif

	  /* Get supported styles and XIM values.  */
	  XGetIMValues (xim, XNQueryInputStyle, &dpyinfo->xim_styles, NULL);

#ifdef HAVE_X11R6
	  destroy.callback = xim_destroy_callback;
	  destroy.client_data = (XPointer)dpyinfo;
	  XSetIMValues (xim, XNDestroyCallback, &destroy, NULL);
#endif
	}
    }

  else
#endif /* HAVE_XIM */
    dpyinfo->xim = NULL;
}


#ifdef HAVE_X11R6_XIM

struct xim_inst_t
{
  struct x_display_info *dpyinfo;
  char *resource_name;
};

/* XIM instantiate callback function, which is called whenever an XIM
   server is available.  DISPLAY is the display of the XIM.
   CLIENT_DATA contains a pointer to an xim_inst_t structure created
   when the callback was registered.  */

static void
xim_instantiate_callback (display, client_data, call_data)
     Display *display;
     XPointer client_data;
     XPointer call_data;
{
  struct xim_inst_t *xim_inst = (struct xim_inst_t *) client_data;
  struct x_display_info *dpyinfo = xim_inst->dpyinfo;

  /* We don't support multiple XIM connections. */
  if (dpyinfo->xim)
    return;

  xim_open_dpy (dpyinfo, xim_inst->resource_name);

  /* Create XIC for the existing frames on the same display, as long
     as they have no XIC.  */
  if (dpyinfo->xim && dpyinfo->reference_count > 0)
    {
      Lisp_Object tail, frame;

      BLOCK_INPUT;
      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);

	  if (FRAME_X_P (f)
              && FRAME_X_DISPLAY_INFO (f) == xim_inst->dpyinfo)
	    if (FRAME_XIC (f) == NULL)
	      {
		create_frame_xic (f);
		if (FRAME_XIC_STYLE (f) & XIMStatusArea)
		  xic_set_statusarea (f);
		if (FRAME_XIC_STYLE (f) & XIMPreeditPosition)
		  {
		    struct window *w = XWINDOW (f->selected_window);
		    xic_set_preeditarea (w, w->cursor.x, w->cursor.y);
		  }
	      }
	}

      UNBLOCK_INPUT;
    }
}

#endif /* HAVE_X11R6_XIM */


/* Open a connection to the XIM server on display DPYINFO.
   RESOURCE_NAME is the resource name for Emacs.  On X11R5, open the
   connection only at the first time.  On X11R6, open the connection
   in the XIM instantiate callback function.  */

static void
xim_initialize (dpyinfo, resource_name)
     struct x_display_info *dpyinfo;
     char *resource_name;
{
#ifdef HAVE_XIM
  if (use_xim)
    {
#ifdef HAVE_X11R6_XIM
      struct xim_inst_t *xim_inst;
      int len;

      dpyinfo->xim = NULL;
      xim_inst = (struct xim_inst_t *) xmalloc (sizeof (struct xim_inst_t));
      xim_inst->dpyinfo = dpyinfo;
      len = strlen (resource_name);
      xim_inst->resource_name = (char *) xmalloc (len + 1);
      bcopy (resource_name, xim_inst->resource_name, len + 1);
      XRegisterIMInstantiateCallback (dpyinfo->display, dpyinfo->xrdb,
				      resource_name, EMACS_CLASS,
				      xim_instantiate_callback,
				      /* This is XPointer in XFree86
					 but (XPointer *) on Tru64, at
					 least, hence the configure test.  */
				      (XRegisterIMInstantiateCallback_arg6) xim_inst);
#else /* not HAVE_X11R6_XIM */
      dpyinfo->xim = NULL;
      xim_open_dpy (dpyinfo, resource_name);
#endif /* not HAVE_X11R6_XIM */

    }
  else
#endif /* HAVE_XIM */
    dpyinfo->xim = NULL;
}


/* Close the connection to the XIM server on display DPYINFO. */

static void
xim_close_dpy (dpyinfo)
     struct x_display_info *dpyinfo;
{
#ifdef HAVE_XIM
  if (use_xim)
    {
#ifdef HAVE_X11R6_XIM
      if (dpyinfo->display)
	XUnregisterIMInstantiateCallback (dpyinfo->display, dpyinfo->xrdb,
					  NULL, EMACS_CLASS,
					  xim_instantiate_callback, NULL);
#endif /* not HAVE_X11R6_XIM */
      if (dpyinfo->display)
	XCloseIM (dpyinfo->xim);
      dpyinfo->xim = NULL;
      XFree (dpyinfo->xim_styles);
    }
#endif /* HAVE_XIM */
}

#endif /* not HAVE_X11R6_XIM */



/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

void
x_calc_absolute_position (f)
     struct frame *f;
{
  int flags = f->size_hint_flags;

  /* We have nothing to do if the current position
     is already for the top-left corner.  */
  if (! ((flags & XNegative) || (flags & YNegative)))
    return;

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->left_pos = (FRAME_X_DISPLAY_INFO (f)->width
                   - FRAME_PIXEL_WIDTH (f) + f->left_pos);

  {
    int height = FRAME_PIXEL_HEIGHT (f);

#if defined USE_X_TOOLKIT && defined USE_MOTIF
    /* Something is fishy here.  When using Motif, starting Emacs with
       `-g -0-0', the frame appears too low by a few pixels.

       This seems to be so because initially, while Emacs is starting,
       the column widget's height and the frame's pixel height are
       different.  The column widget's height is the right one.  In
       later invocations, when Emacs is up, the frame's pixel height
       is right, though.

       It's not obvious where the initial small difference comes from.
       2000-12-01, gerd.  */

    XtVaGetValues (f->output_data.x->column_widget, XtNheight, &height, NULL);
#endif

  if (flags & YNegative)
    f->top_pos = (FRAME_X_DISPLAY_INFO (f)->height - height + f->top_pos);
  }

  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->size_hint_flags &= ~ (XNegative | YNegative);
}

/* CHANGE_GRAVITY is 1 when calling from Fset_frame_position,
   to really change the position, and 0 when calling from
   x_make_frame_visible (in that case, XOFF and YOFF are the current
   position values).  It is -1 when calling from x_set_frame_parameters,
   which means, do adjust for borders but don't change the gravity.  */

void
x_set_offset (f, xoff, yoff, change_gravity)
     struct frame *f;
     register int xoff, yoff;
     int change_gravity;
{
  int modified_top, modified_left;

  if (change_gravity > 0)
    {
      FRAME_X_OUTPUT (f)->left_before_move = f->left_pos;
      FRAME_X_OUTPUT (f)->top_before_move = f->top_pos;

      f->top_pos = yoff;
      f->left_pos = xoff;
      f->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->size_hint_flags |= YNegative;
      f->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, (long) 0, 0);

  modified_left = f->left_pos;
  modified_top = f->top_pos;

  if (change_gravity != 0 && FRAME_X_DISPLAY_INFO (f)->wm_type == X_WMTYPE_A)
    {
      /* Some WMs (twm, wmaker at least) has an offset that is smaller
         than the WM decorations.  So we use the calculated offset instead
         of the WM decoration sizes here (x/y_pixels_outer_diff).  */
      modified_left += FRAME_X_OUTPUT (f)->move_offset_left;
      modified_top += FRAME_X_OUTPUT (f)->move_offset_top;
    }

  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
               modified_left, modified_top);

  x_sync_with_move (f, f->left_pos, f->top_pos,
                    FRAME_X_DISPLAY_INFO (f)->wm_type == X_WMTYPE_UNKNOWN
                    ? 1 : 0);

  /* change_gravity is non-zero when this function is called from Lisp to
     programmatically move a frame.  In that case, we call
     x_check_expected_move to discover if we have a "Type A" or "Type B"
     window manager, and, for a "Type A" window manager, adjust the position
     of the frame.

     We call x_check_expected_move if a programmatic move occurred, and
     either the window manager type (A/B) is unknown or it is Type A but we
     need to compute the top/left offset adjustment for this frame.  */

  if (change_gravity != 0 &&
      (FRAME_X_DISPLAY_INFO (f)->wm_type == X_WMTYPE_UNKNOWN
       || (FRAME_X_DISPLAY_INFO (f)->wm_type == X_WMTYPE_A
           && (FRAME_X_OUTPUT (f)->move_offset_left == 0
               && FRAME_X_OUTPUT (f)->move_offset_top == 0))))
    x_check_expected_move (f, modified_left, modified_top);

  UNBLOCK_INPUT;
}

/* Return non-zero if _NET_SUPPORTING_WM_CHECK window exists and _NET_SUPPORTED
   on the root window for frame F contains ATOMNAME.
   This is how a WM check shall be done according to the Window Manager
   Specification/Extended Window Manager Hints at
   http://freedesktop.org/wiki/Specifications/wm-spec.  */

static int
wm_supports (f, atomname)
     struct frame *f;
     const char *atomname;
{
  Atom actual_type;
  unsigned long actual_size, bytes_remaining;
  int i, rc, actual_format;
  Atom prop_atom;
  Window wmcheck_window;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Window target_window = dpyinfo->root_window;
  long max_len = 65536;
  Display *dpy = FRAME_X_DISPLAY (f);
  unsigned char *tmp_data = NULL;
  Atom target_type = XA_WINDOW;
  Atom want_atom;

  BLOCK_INPUT;

  prop_atom = XInternAtom (dpy, "_NET_SUPPORTING_WM_CHECK", False);

  x_catch_errors (dpy);
  rc = XGetWindowProperty (dpy, target_window,
                           prop_atom, 0, max_len, False, target_type,
                           &actual_type, &actual_format, &actual_size,
                           &bytes_remaining, &tmp_data);

  if (rc != Success || actual_type != XA_WINDOW || x_had_errors_p (dpy))
    {
      if (tmp_data) XFree (tmp_data);
      x_uncatch_errors ();
      UNBLOCK_INPUT;
      return 0;
    }

  wmcheck_window = *(Window *) tmp_data;
  XFree (tmp_data);

  /* Check if window exists. */
  XSelectInput (dpy, wmcheck_window, StructureNotifyMask);
  x_sync (f);
  if (x_had_errors_p (dpy))
    {
      x_uncatch_errors ();
      UNBLOCK_INPUT;
      return 0;
    }

  if (dpyinfo->net_supported_window != wmcheck_window)
    {
      /* Window changed, reload atoms */
      if (dpyinfo->net_supported_atoms != NULL)
        XFree (dpyinfo->net_supported_atoms);
      dpyinfo->net_supported_atoms = NULL;
      dpyinfo->nr_net_supported_atoms = 0;
      dpyinfo->net_supported_window = 0;

      target_type = XA_ATOM;
      prop_atom = XInternAtom (dpy, "_NET_SUPPORTED", False);
      tmp_data = NULL;
      rc = XGetWindowProperty (dpy, target_window,
                               prop_atom, 0, max_len, False, target_type,
                               &actual_type, &actual_format, &actual_size,
                               &bytes_remaining, &tmp_data);

      if (rc != Success || actual_type != XA_ATOM || x_had_errors_p (dpy))
        {
          if (tmp_data) XFree (tmp_data);
          x_uncatch_errors ();
          UNBLOCK_INPUT;
          return 0;
        }

      dpyinfo->net_supported_atoms = (Atom *)tmp_data;
      dpyinfo->nr_net_supported_atoms = actual_size;
      dpyinfo->net_supported_window = wmcheck_window;
    }

  rc = 0;
  want_atom = XInternAtom (dpy, atomname, False);

  for (i = 0; rc == 0 && i < dpyinfo->nr_net_supported_atoms; ++i)
    rc = dpyinfo->net_supported_atoms[i] == want_atom;

  x_uncatch_errors ();
  UNBLOCK_INPUT;

  return rc;
}

/* Do fullscreen as specified in extended window manager hints */

static int
do_ewmh_fullscreen (f)
     struct frame *f;
{
  int have_net_atom = wm_supports (f, "_NET_WM_STATE");

  /* Some window managers don't say they support _NET_WM_STATE, but they do say
     they support _NET_WM_STATE_FULLSCREEN.  Try that also.  */
  if (!have_net_atom)
      have_net_atom = wm_supports (f, "_NET_WM_STATE_FULLSCREEN");

  if (have_net_atom)
    {
      Lisp_Object frame;
      const char *atom = "_NET_WM_STATE";
      const char *fs = "_NET_WM_STATE_FULLSCREEN";
      const char *fw = "_NET_WM_STATE_MAXIMIZED_HORZ";
      const char *fh = "_NET_WM_STATE_MAXIMIZED_VERT";
      const char *what = NULL;

      XSETFRAME (frame, f);

      /* If there are _NET_ atoms we assume we have extended window manager
         hints.  */
      switch (f->want_fullscreen)
        {
        case FULLSCREEN_BOTH:
          what = fs;
          break;
        case FULLSCREEN_WIDTH:
          what = fw;
          break;
        case FULLSCREEN_HEIGHT:
          what = fh;
          break;
        }

      if (what != NULL && !wm_supports (f, what)) return 0;


      Fx_send_client_event (frame, make_number (0), frame,
                            make_unibyte_string (atom, strlen (atom)),
                            make_number (32),
                            Fcons (make_number (0), /* Remove */
                                   Fcons
                                   (make_unibyte_string (fs,
                                                         strlen (fs)),
                                    Qnil)));
      Fx_send_client_event (frame, make_number (0), frame,
                            make_unibyte_string (atom, strlen (atom)),
                            make_number (32),
                            Fcons (make_number (0), /* Remove */
                                   Fcons
                                   (make_unibyte_string (fh,
                                                         strlen (fh)),
                                    Qnil)));
      Fx_send_client_event (frame, make_number (0), frame,
                            make_unibyte_string (atom, strlen (atom)),
                            make_number (32),
                            Fcons (make_number (0), /* Remove */
                                   Fcons
                                   (make_unibyte_string (fw,
                                                         strlen (fw)),
                                    Qnil)));
      f->want_fullscreen = FULLSCREEN_NONE;
      if (what != NULL)
        Fx_send_client_event (frame, make_number (0), frame,
                              make_unibyte_string (atom, strlen (atom)),
                              make_number (32),
                              Fcons (make_number (1), /* Add */
                                     Fcons
                                     (make_unibyte_string (what,
                                                           strlen (what)),
                                      Qnil)));
    }

  return have_net_atom;
}

static void
XTfullscreen_hook (f)
     FRAME_PTR f;
{
  if (f->async_visible)
    {
      BLOCK_INPUT;
      do_ewmh_fullscreen (f);
      x_sync (f);
      UNBLOCK_INPUT;
    }
}


/* Check if we need to resize the frame due to a fullscreen request.
   If so needed, resize the frame. */
static void
x_check_fullscreen (f)
     struct frame *f;
{
  if (f->want_fullscreen & FULLSCREEN_BOTH)
    {
      int width, height, ign;

      if (do_ewmh_fullscreen (f))
        return;

      x_real_positions (f, &f->left_pos, &f->top_pos);

      x_fullscreen_adjust (f, &width, &height, &ign, &ign);

      /* We do not need to move the window, it shall be taken care of
         when setting WM manager hints.
         If the frame is visible already, the position is checked by
         x_check_expected_move. */
      if (FRAME_COLS (f) != width || FRAME_LINES (f) != height)
        {
          change_frame_size (f, height, width, 0, 1, 0);
          SET_FRAME_GARBAGED (f);
          cancel_mouse_face (f);

          /* Wait for the change of frame size to occur */
          f->want_fullscreen |= FULLSCREEN_WAIT;
        }
    }
}

/* This function is called by x_set_offset to determine whether the window
   manager interfered with the positioning of the frame.  Type A window
   managers position the surrounding window manager decorations a small
   amount above and left of the user-supplied position.  Type B window
   managers position the surrounding window manager decorations at the
   user-specified position.  If we detect a Type A window manager, we
   compensate by moving the window right and down by the proper amount.  */

static void
x_check_expected_move (f, expected_left, expected_top)
     struct frame *f;
     int expected_left;
     int expected_top;
{
  int current_left = 0, current_top = 0;

  /* x_real_positions returns the left and top offsets of the outermost
     window manager window around the frame.  */

  x_real_positions (f, &current_left, &current_top);

  if (current_left != expected_left || current_top != expected_top)
      {
      /* It's a "Type A" window manager. */

      int adjusted_left;
      int adjusted_top;

        FRAME_X_DISPLAY_INFO (f)->wm_type = X_WMTYPE_A;
      FRAME_X_OUTPUT (f)->move_offset_left = expected_left - current_left;
      FRAME_X_OUTPUT (f)->move_offset_top = expected_top - current_top;

      /* Now fix the mispositioned frame's location. */

      adjusted_left = expected_left + FRAME_X_OUTPUT (f)->move_offset_left;
      adjusted_top = expected_top + FRAME_X_OUTPUT (f)->move_offset_top;

      XMoveWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
                   adjusted_left, adjusted_top);

      x_sync_with_move (f, expected_left, expected_top, 0);
      }
  else
    /* It's a "Type B" window manager.  We don't have to adjust the
       frame's position. */

      FRAME_X_DISPLAY_INFO (f)->wm_type = X_WMTYPE_B;
}


/* Wait for XGetGeometry to return up-to-date position information for a
   recently-moved frame.  Call this immediately after calling XMoveWindow.
   If FUZZY is non-zero, then LEFT and TOP are just estimates of where the
   frame has been moved to, so we use a fuzzy position comparison instead
   of an exact comparison.  */

static void
x_sync_with_move (f, left, top, fuzzy)
    struct frame *f;
    int left, top, fuzzy;
{
  int count = 0;

  while (count++ < 50)
    {
      int current_left = 0, current_top = 0;

      /* In theory, this call to XSync only needs to happen once, but in
         practice, it doesn't seem to work, hence the need for the surrounding
         loop.  */

      XSync (FRAME_X_DISPLAY (f), False);
      x_real_positions (f, &current_left, &current_top);

      if (fuzzy)
        {
          /* The left fuzz-factor is 10 pixels.  The top fuzz-factor is 40
             pixels.  */

          if (eabs (current_left - left) <= 10
	      && eabs (current_top - top) <= 40)
            return;
  }
      else if (current_left == left && current_top == top)
        return;
    }

  /* As a last resort, just wait 0.5 seconds and hope that XGetGeometry
     will then return up-to-date position info. */

  wait_reading_process_output (0, 500000, 0, 0, Qnil, NULL, 0);
}


/* Change the size of frame F's X window to COLS/ROWS in the case F
   doesn't have a widget.  If CHANGE_GRAVITY is 1, we change to
   top-left-corner window gravity for this size change and subsequent
   size changes.  Otherwise we leave the window gravity unchanged.  */

static void
x_set_window_size_1 (f, change_gravity, cols, rows)
     struct frame *f;
     int change_gravity;
     int cols, rows;
{
  int pixelwidth, pixelheight;

  check_frame_size (f, &rows, &cols);
  f->scroll_bar_actual_width
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0
       ? FRAME_CONFIG_SCROLL_BAR_WIDTH (f)
       : (FRAME_CONFIG_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f)));

  compute_fringe_widths (f, 0);

  pixelwidth = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, cols);
  pixelheight = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, rows);

  f->win_gravity = NorthWestGravity;
  x_wm_set_size_hint (f, (long) 0, 0);

  XSync (FRAME_X_DISPLAY (f), False);
  XResizeWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 pixelwidth, pixelheight);

  /* Now, strictly speaking, we can't be sure that this is accurate,
     but the window manager will get around to dealing with the size
     change request eventually, and we'll hear how it went when the
     ConfigureNotify event gets here.

     We could just not bother storing any of this information here,
     and let the ConfigureNotify event set everything up, but that
     might be kind of confusing to the Lisp code, since size changes
     wouldn't be reported in the frame parameters until some random
     point in the future when the ConfigureNotify event arrives.

     We pass 1 for DELAY since we can't run Lisp code inside of
     a BLOCK_INPUT.  */
  change_frame_size (f, rows, cols, 0, 1, 0);
  FRAME_PIXEL_WIDTH (f) = pixelwidth;
  FRAME_PIXEL_HEIGHT (f) = pixelheight;

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);

  XFlush (FRAME_X_DISPLAY (f));
}


/* Call this to change the size of frame F's x-window.
   If CHANGE_GRAVITY is 1, we change to top-left-corner window gravity
   for this size change and subsequent size changes.
   Otherwise we leave the window gravity unchanged.  */

void
x_set_window_size (f, change_gravity, cols, rows)
     struct frame *f;
     int change_gravity;
     int cols, rows;
{
  BLOCK_INPUT;

#ifdef USE_GTK
  if (FRAME_GTK_WIDGET (f))
    xg_frame_set_char_size (f, cols, rows);
  else
    x_set_window_size_1 (f, change_gravity, cols, rows);
#elif USE_X_TOOLKIT

  if (f->output_data.x->widget != NULL)
    {
      /* The x and y position of the widget is clobbered by the
	 call to XtSetValues within EmacsFrameSetCharSize.
	 This is a real kludge, but I don't understand Xt so I can't
	 figure out a correct fix.  Can anyone else tell me? -- rms.  */
      int xpos = f->output_data.x->widget->core.x;
      int ypos = f->output_data.x->widget->core.y;
      EmacsFrameSetCharSize (f->output_data.x->edit_widget, cols, rows);
      f->output_data.x->widget->core.x = xpos;
      f->output_data.x->widget->core.y = ypos;
    }
  else
    x_set_window_size_1 (f, change_gravity, cols, rows);

#else /* not USE_X_TOOLKIT */

  x_set_window_size_1 (f, change_gravity, cols, rows);

#endif /* not USE_X_TOOLKIT */

  /* If cursor was outside the new size, mark it as off.  */
  mark_window_cursors_off (XWINDOW (f->root_window));

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size.
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  UNBLOCK_INPUT;
}

/* Mouse warping.  */

void
x_set_mouse_position (f, x, y)
     struct frame *f;
     int x, y;
{
  int pix_x, pix_y;

  pix_x = FRAME_COL_TO_PIXEL_X (f, x) + FRAME_COLUMN_WIDTH (f) / 2;
  pix_y = FRAME_LINE_TO_PIXEL_Y (f, y) + FRAME_LINE_HEIGHT (f) / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > FRAME_PIXEL_WIDTH (f)) pix_x = FRAME_PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > FRAME_PIXEL_HEIGHT (f)) pix_y = FRAME_PIXEL_HEIGHT (f);

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

/* focus shifting, raising and lowering.  */

void
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

void
x_unfocus_frame (f)
     struct frame *f;
{
#if 0
  /* Look at the remarks in x_focus_on_frame.  */
  if (FRAME_X_DISPLAY_INFO (f)->x_focus_frame == f)
    XSetInputFocus (FRAME_X_DISPLAY (f), PointerRoot,
		    RevertToPointerRoot, CurrentTime);
#endif /* ! 0 */
}

/* Raise frame F.  */

void
x_raise_frame (f)
     struct frame *f;
{
  BLOCK_INPUT;
  if (f->async_visible)
    XRaiseWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f));

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
}

/* Lower frame F.  */

void
x_lower_frame (f)
     struct frame *f;
{
  if (f->async_visible)
    {
      BLOCK_INPUT;
      XLowerWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f));
      XFlush (FRAME_X_DISPLAY (f));
      UNBLOCK_INPUT;
    }
}

/* Activate frame with Extended Window Manager Hints */

void
x_ewmh_activate_frame (f)
     FRAME_PTR f;
{
  /* See Window Manager Specification/Extended Window Manager Hints at
     http://freedesktop.org/wiki/Specifications/wm-spec  */

  const char *atom = "_NET_ACTIVE_WINDOW";
  if (f->async_visible && wm_supports (f, atom))
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      Fx_send_client_event (frame, make_number (0), frame,
                            make_unibyte_string (atom, strlen (atom)),
                            make_number (32),
                            Fcons (make_number (1),
                                   Fcons (make_number (last_user_time),
                                          Qnil)));
    }
}

static void
XTframe_raise_lower (f, raise_flag)
     FRAME_PTR f;
     int raise_flag;
{
  if (raise_flag)
    x_raise_frame (f);
  else
    x_lower_frame (f);
}

/* XEmbed implementation.  */

void
xembed_set_info (f, flags)
     struct frame *f;
     enum xembed_info flags;
{
  Atom atom;
  unsigned long data[2];

  atom = XInternAtom (FRAME_X_DISPLAY (f), "_XEMBED_INFO", False);

  data[0] = XEMBED_VERSION;
  data[1] = flags;

  XChangeProperty (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), atom, atom,
		   32, PropModeReplace, (unsigned char *) data, 2);
}

void
xembed_send_message (f, time, message, detail, data1, data2)
     struct frame *f;
     Time time;
     enum xembed_message message;
     long detail;
     long data1;
     long data2;
{
  XEvent event;

  event.xclient.type = ClientMessage;
  event.xclient.window = FRAME_X_OUTPUT (f)->parent_desc;
  event.xclient.message_type = FRAME_X_DISPLAY_INFO (f)->Xatom_XEMBED;
  event.xclient.format = 32;
  event.xclient.data.l[0] = time;
  event.xclient.data.l[1] = message;
  event.xclient.data.l[2] = detail;
  event.xclient.data.l[3] = data1;
  event.xclient.data.l[4] = data2;

  XSendEvent (FRAME_X_DISPLAY (f), FRAME_X_OUTPUT (f)->parent_desc,
	      False, NoEventMask, &event);
  XSync (FRAME_X_DISPLAY (f), False);
}

/* Change of visibility.  */

/* This tries to wait until the frame is really visible.
   However, if the window manager asks the user where to position
   the frame, this will return before the user finishes doing that.
   The frame will not actually be visible at that time,
   but it will become visible later when the window manager
   finishes with it.  */

void
x_make_frame_visible (f)
     struct frame *f;
{
  Lisp_Object type;
  int original_top, original_left;
  int retry_count = 2;

 retry:

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

  if (! FRAME_VISIBLE_P (f))
    {
      /* We test FRAME_GARBAGED_P here to make sure we don't
	 call x_set_offset a second time
	 if we get to x_make_frame_visible a second time
	 before the window gets really visible.  */
      if (! FRAME_ICONIFIED_P (f)
	  && ! FRAME_X_EMBEDDED_P (f)
	  && ! f->output_data.x->asked_for_visible)
	x_set_offset (f, f->left_pos, f->top_pos, 0);

      f->output_data.x->asked_for_visible = 1;

      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, NormalState);
#ifdef USE_X_TOOLKIT
      if (FRAME_X_EMBEDDED_P (f))
	xembed_set_info (f, XEMBED_MAPPED);
      else
	{
	  /* This was XtPopup, but that did nothing for an iconified frame.  */
	  XtMapWidget (f->output_data.x->widget);
	}
#else /* not USE_X_TOOLKIT */
#ifdef USE_GTK
      gtk_widget_show_all (FRAME_GTK_OUTER_WIDGET (f));
      gtk_window_deiconify (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
#else
      if (FRAME_X_EMBEDDED_P (f))
	xembed_set_info (f, XEMBED_MAPPED);
      else
	XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* not USE_GTK */
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
    int count;
    /* This must be before UNBLOCK_INPUT
       since events that arrive in response to the actions above
       will set it when they are handled.  */
    int previously_visible = f->output_data.x->has_been_visible;

    original_left = f->left_pos;
    original_top = f->top_pos;

    /* This must come after we set COUNT.  */
    UNBLOCK_INPUT;

    /* We unblock here so that arriving X events are processed.  */

    /* Now move the window back to where it was "supposed to be".
       But don't do it if the gravity is negative.
       When the gravity is negative, this uses a position
       that is 3 pixels too low.  Perhaps that's really the border width.

       Don't do this if the window has never been visible before,
       because the window manager may choose the position
       and we don't want to override it.  */

    if (! FRAME_VISIBLE_P (f)
	&& ! FRAME_ICONIFIED_P (f)
	&& ! FRAME_X_EMBEDDED_P (f)
	&& f->win_gravity == NorthWestGravity
	&& previously_visible)
      {
	Drawable rootw;
	int x, y;
	unsigned int width, height, border, depth;

	BLOCK_INPUT;

	/* On some window managers (such as FVWM) moving an existing
	   window, even to the same place, causes the window manager
	   to introduce an offset.  This can cause the window to move
	   to an unexpected location.  Check the geometry (a little
	   slow here) and then verify that the window is in the right
	   place.  If the window is not in the right place, move it
	   there, and take the potential window manager hit.  */
	XGetGeometry (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		      &rootw, &x, &y, &width, &height, &border, &depth);

	if (original_left != x || original_top != y)
	  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		       original_left, original_top);

	UNBLOCK_INPUT;
      }

    XSETFRAME (frame, f);

    /* Wait until the frame is visible.  Process X events until a
       MapNotify event has been seen, or until we think we won't get a
       MapNotify at all..  */
    for (count = input_signal_count + 10;
	 input_signal_count < count && !FRAME_VISIBLE_P (f);)
      {
	/* Force processing of queued events.  */
	x_sync (f);

	/* Machines that do polling rather than SIGIO have been
	   observed to go into a busy-wait here.  So we'll fake an
	   alarm signal to let the handler know that there's something
	   to be read.  We used to raise a real alarm, but it seems
	   that the handler isn't always enabled here.  This is
	   probably a bug.  */
	if (input_polling_used ())
	  {
	    /* It could be confusing if a real alarm arrives while
	       processing the fake one.  Turn it off and let the
	       handler reset it.  */
	    extern void poll_for_input_1 P_ ((void));
	    int old_poll_suppress_count = poll_suppress_count;
	    poll_suppress_count = 1;
	    poll_for_input_1 ();
	    poll_suppress_count = old_poll_suppress_count;
	  }

	/* See if a MapNotify event has been processed.  */
	FRAME_SAMPLE_VISIBILITY (f);
      }

    /* 2000-09-28: In

       (let ((f (selected-frame)))
          (iconify-frame f)
	  (raise-frame f))

       the frame is not raised with various window managers on
       FreeBSD, GNU/Linux and Solaris.  It turns out that, for some
       unknown reason, the call to XtMapWidget is completely ignored.
       Mapping the widget a second time works.  */

    if (!FRAME_VISIBLE_P (f) && --retry_count > 0)
      goto retry;
  }
}

/* Change from mapped state to withdrawn state.  */

/* Make the frame visible (mapped and not iconified).  */

void
x_make_frame_invisible (f)
     struct frame *f;
{
  Window window;

  /* Use the frame's outermost window, not the one we normally draw on.  */
  window = FRAME_OUTER_WINDOW (f);

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_X_DISPLAY_INFO (f)->x_highlight_frame == f)
    FRAME_X_DISPLAY_INFO (f)->x_highlight_frame = 0;

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
  x_wm_set_size_hint (f, (long) 0, 1);

#ifdef USE_GTK
  if (FRAME_GTK_OUTER_WIDGET (f))
    gtk_widget_hide (FRAME_GTK_OUTER_WIDGET (f));
  else
#else
  if (FRAME_X_EMBEDDED_P (f))
    xembed_set_info (f, 0);
  else
#endif
  {

  if (! XWithdrawWindow (FRAME_X_DISPLAY (f), window,
			 DefaultScreen (FRAME_X_DISPLAY (f))))
    {
      UNBLOCK_INPUT_RESIGNAL;
      error ("Can't notify window manager of window withdrawal");
    }
  }

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

/* Change window state from mapped to iconified.  */

void
x_iconify_frame (f)
     struct frame *f;
{
  int result;
  Lisp_Object type;

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_X_DISPLAY_INFO (f)->x_highlight_frame == f)
    FRAME_X_DISPLAY_INFO (f)->x_highlight_frame = 0;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

  FRAME_SAMPLE_VISIBILITY (f);

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

#ifdef USE_GTK
  if (FRAME_GTK_OUTER_WIDGET (f))
    {
      if (! FRAME_VISIBLE_P (f))
        gtk_widget_show_all (FRAME_GTK_OUTER_WIDGET (f));

      gtk_window_iconify (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
      f->iconified = 1;
      f->visible = 1;
      f->async_iconified = 1;
      f->async_visible = 0;
      UNBLOCK_INPUT;
      return;
    }
#endif

#ifdef USE_X_TOOLKIT

  if (! FRAME_VISIBLE_P (f))
    {
      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, IconicState);
      /* This was XtPopup, but that did nothing for an iconified frame.  */
      XtMapWidget (f->output_data.x->widget);
      /* The server won't give us any event to indicate
	 that an invisible frame was changed to an icon,
	 so we have to record it here.  */
      f->iconified = 1;
      f->visible = 1;
      f->async_iconified = 1;
      f->async_visible = 0;
      UNBLOCK_INPUT;
      return;
    }

  result = XIconifyWindow (FRAME_X_DISPLAY (f),
			   XtWindow (f->output_data.x->widget),
			   DefaultScreen (FRAME_X_DISPLAY (f)));
  UNBLOCK_INPUT;

  if (!result)
    error ("Can't notify window manager of iconification");

  f->async_iconified = 1;
  f->async_visible = 0;


  BLOCK_INPUT;
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
#else /* not USE_X_TOOLKIT */

  /* Make sure the X server knows where the window should be positioned,
     in case the user deiconifies with the window manager.  */
  if (! FRAME_VISIBLE_P (f)
      && ! FRAME_ICONIFIED_P (f)
      && ! FRAME_X_EMBEDDED_P (f))
    x_set_offset (f, f->left_pos, f->top_pos, 0);

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
  f->async_visible = 0;

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
#endif /* not USE_X_TOOLKIT */
}


/* Free X resources of frame F.  */

void
x_free_frame_resources (f)
     struct frame *f;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Lisp_Object bar;
  struct scroll_bar *b;

  BLOCK_INPUT;

  /* If a display connection is dead, don't try sending more
     commands to the X server.  */
  if (dpyinfo->display)
    {
#ifdef USE_FONT_BACKEND
      /* We must free faces before destroying windows because some
	 font-driver (e.g. xft) access a window while finishing a
	 face.  */
      if (enable_font_backend
	  && FRAME_FACE_CACHE (f))
	free_frame_faces (f);
#endif	/* USE_FONT_BACKEND */

      if (f->output_data.x->icon_desc)
	XDestroyWindow (FRAME_X_DISPLAY (f), f->output_data.x->icon_desc);

#ifdef USE_X_TOOLKIT
      /* Explicitly destroy the scroll bars of the frame.  Without
	 this, we get "BadDrawable" errors from the toolkit later on,
	 presumably from expose events generated for the disappearing
	 toolkit scroll bars.  */
      for (bar = FRAME_SCROLL_BARS (f); !NILP (bar); bar = b->next)
	{
	  b = XSCROLL_BAR (bar);
	  x_scroll_bar_remove (b);
	}
#endif

#ifdef HAVE_X_I18N
      if (FRAME_XIC (f))
	free_frame_xic (f);
#endif

#ifdef USE_X_TOOLKIT
      if (f->output_data.x->widget)
	{
	  XtDestroyWidget (f->output_data.x->widget);
	  f->output_data.x->widget = NULL;
	}
      /* Tooltips don't have widgets, only a simple X window, even if
	 we are using a toolkit.  */
      else if (FRAME_X_WINDOW (f))
	XDestroyWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));

      free_frame_menubar (f);
#else  /* !USE_X_TOOLKIT */

#ifdef USE_GTK
      /* In the GTK version, tooltips are normal X
         frames.  We must check and free both types. */
      if (FRAME_GTK_OUTER_WIDGET (f))
        {
          gtk_widget_destroy (FRAME_GTK_OUTER_WIDGET (f));
          FRAME_X_WINDOW (f) = 0; /* Set to avoid XDestroyWindow below */
          FRAME_GTK_OUTER_WIDGET (f) = 0;
        }
#endif /* USE_GTK */

      if (FRAME_X_WINDOW (f))
	XDestroyWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* !USE_X_TOOLKIT */

      unload_color (f, FRAME_FOREGROUND_PIXEL (f));
      unload_color (f, FRAME_BACKGROUND_PIXEL (f));
      unload_color (f, f->output_data.x->cursor_pixel);
      unload_color (f, f->output_data.x->cursor_foreground_pixel);
      unload_color (f, f->output_data.x->border_pixel);
      unload_color (f, f->output_data.x->mouse_pixel);

      if (f->output_data.x->scroll_bar_background_pixel != -1)
	unload_color (f, f->output_data.x->scroll_bar_background_pixel);
      if (f->output_data.x->scroll_bar_foreground_pixel != -1)
	unload_color (f, f->output_data.x->scroll_bar_foreground_pixel);
#ifdef USE_TOOLKIT_SCROLL_BARS
      /* Scrollbar shadow colors.  */
      if (f->output_data.x->scroll_bar_top_shadow_pixel != -1)
	unload_color (f, f->output_data.x->scroll_bar_top_shadow_pixel);
      if (f->output_data.x->scroll_bar_bottom_shadow_pixel != -1)
	unload_color (f, f->output_data.x->scroll_bar_bottom_shadow_pixel);
#endif /* USE_TOOLKIT_SCROLL_BARS */
      if (f->output_data.x->white_relief.allocated_p)
	unload_color (f, f->output_data.x->white_relief.pixel);
      if (f->output_data.x->black_relief.allocated_p)
	unload_color (f, f->output_data.x->black_relief.pixel);

      if (FRAME_FACE_CACHE (f))
	free_frame_faces (f);

      x_free_gcs (f);
      XFlush (FRAME_X_DISPLAY (f));
    }

  if (f->output_data.x->saved_menu_event)
    xfree (f->output_data.x->saved_menu_event);

  xfree (f->output_data.x);
  f->output_data.x = NULL;

  if (f == dpyinfo->x_focus_frame)
    dpyinfo->x_focus_frame = 0;
  if (f == dpyinfo->x_focus_event_frame)
    dpyinfo->x_focus_event_frame = 0;
  if (f == dpyinfo->x_highlight_frame)
    dpyinfo->x_highlight_frame = 0;

  if (f == dpyinfo->mouse_face_mouse_frame)
    {
      dpyinfo->mouse_face_beg_row
	= dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row
	= dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
      dpyinfo->mouse_face_deferred_gc = 0;
      dpyinfo->mouse_face_mouse_frame = 0;
    }

  UNBLOCK_INPUT;
}


/* Destroy the X window of frame F.  */

void
x_destroy_window (f)
     struct frame *f;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  /* If a display connection is dead, don't try sending more
     commands to the X server.  */
  if (dpyinfo->display != 0)
    x_free_frame_resources (f);

  dpyinfo->reference_count--;
}


/* Setting window manager hints.  */

/* Set the normal size hints for the window manager, for frame F.
   FLAGS is the flags word to use--or 0 meaning preserve the flags
   that the window now has.
   If USER_POSITION is nonzero, we set the USPosition
   flag (this is useful when FLAGS is 0).
   The GTK version is in gtkutils.c  */

#ifndef USE_GTK
void
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
#endif

  Window window = FRAME_OUTER_WINDOW (f);

  /* Setting PMaxSize caused various problems.  */
  size_hints.flags = PResizeInc | PMinSize /* | PMaxSize */;

  size_hints.x = f->left_pos;
  size_hints.y = f->top_pos;

#ifdef USE_X_TOOLKIT
  XtSetArg (al[ac], XtNwidth, &widget_width); ac++;
  XtSetArg (al[ac], XtNheight, &widget_height); ac++;
  XtGetValues (f->output_data.x->widget, al, ac);
  size_hints.height = widget_height;
  size_hints.width = widget_width;
#else /* not USE_X_TOOLKIT */
  size_hints.height = FRAME_PIXEL_HEIGHT (f);
  size_hints.width = FRAME_PIXEL_WIDTH (f);
#endif /* not USE_X_TOOLKIT */

  size_hints.width_inc = FRAME_COLUMN_WIDTH (f);
  size_hints.height_inc = FRAME_LINE_HEIGHT (f);
  size_hints.max_width
    = FRAME_X_DISPLAY_INFO (f)->width - FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, 0);
  size_hints.max_height
    = FRAME_X_DISPLAY_INFO (f)->height - FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, 0);

  /* Calculate the base and minimum sizes.

     (When we use the X toolkit, we don't do it here.
     Instead we copy the values that the widgets are using, below.)  */
#ifndef USE_X_TOOLKIT
  {
    int base_width, base_height;
    int min_rows = 0, min_cols = 0;

    base_width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, 0);
    base_height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, 0);

    check_frame_size (f, &min_rows, &min_cols);

    /* The window manager uses the base width hints to calculate the
       current number of rows and columns in the frame while
       resizing; min_width and min_height aren't useful for this
       purpose, since they might not give the dimensions for a
       zero-row, zero-column frame.

       We use the base_width and base_height members if we have
       them; otherwise, we set the min_width and min_height members
       to the size for a zero x zero frame.  */

    size_hints.flags |= PBaseSize;
    size_hints.base_width = base_width;
    size_hints.base_height = base_height;
    size_hints.min_width  = base_width + min_cols * size_hints.width_inc;
    size_hints.min_height = base_height + min_rows * size_hints.height_inc;
  }

  /* If we don't need the old flags, we don't need the old hint at all.  */
  if (flags)
    {
      size_hints.flags |= flags;
      goto no_read;
    }
#endif /* not USE_X_TOOLKIT */

  {
    XSizeHints hints;		/* Sometimes I hate X Windows... */
    long supplied_return;
    int value;

    value = XGetWMNormalHints (FRAME_X_DISPLAY (f), window, &hints,
			       &supplied_return);

#ifdef USE_X_TOOLKIT
    size_hints.base_height = hints.base_height;
    size_hints.base_width = hints.base_width;
    size_hints.min_height = hints.min_height;
    size_hints.min_width = hints.min_width;
#endif

    if (flags)
      size_hints.flags |= flags;
    else
      {
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
  }

#ifndef USE_X_TOOLKIT
 no_read:
#endif

#ifdef PWinGravity
  size_hints.win_gravity = f->win_gravity;
  size_hints.flags |= PWinGravity;

  if (user_position)
    {
      size_hints.flags &= ~ PPosition;
      size_hints.flags |= USPosition;
    }
#endif /* PWinGravity */

  XSetWMNormalHints (FRAME_X_DISPLAY (f), window, &size_hints);
}
#endif /* not USE_GTK */

/* Used for IconicState or NormalState */

void
x_wm_set_window_state (f, state)
     struct frame *f;
     int state;
{
#ifdef USE_X_TOOLKIT
  Arg al[1];

  XtSetArg (al[0], XtNinitialState, state);
  XtSetValues (f->output_data.x->widget, al, 1);
#else /* not USE_X_TOOLKIT */
  Window window = FRAME_X_WINDOW (f);

  f->output_data.x->wm_hints.flags |= StateHint;
  f->output_data.x->wm_hints.initial_state = state;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);
#endif /* not USE_X_TOOLKIT */
}

void
x_wm_set_icon_pixmap (f, pixmap_id)
     struct frame *f;
     int pixmap_id;
{
  Pixmap icon_pixmap, icon_mask;

#ifndef USE_X_TOOLKIT
  Window window = FRAME_OUTER_WINDOW (f);
#endif

  if (pixmap_id > 0)
    {
      icon_pixmap = x_bitmap_pixmap (f, pixmap_id);
      f->output_data.x->wm_hints.icon_pixmap = icon_pixmap;
      icon_mask = x_bitmap_mask (f, pixmap_id);
      f->output_data.x->wm_hints.icon_mask = icon_mask;
    }
  else
    {
      /* It seems there is no way to turn off use of an icon pixmap.
	 The following line does it, only if no icon has yet been created,
	 for some window managers.  But with mwm it crashes.
	 Some people say it should clear the IconPixmapHint bit in this case,
	 but that doesn't work, and the X consortium said it isn't the
	 right thing at all.  Since there is no way to win,
	 best to explicitly give up.  */
#if 0
      f->output_data.x->wm_hints.icon_pixmap = None;
      f->output_data.x->wm_hints.icon_mask = None;
#else
      return;
#endif
    }


#ifdef USE_GTK
  {
    xg_set_frame_icon (f, icon_pixmap, icon_mask);
    return;
  }

#elif defined (USE_X_TOOLKIT) /* same as in x_wm_set_window_state.  */

  {
    Arg al[1];
    XtSetArg (al[0], XtNiconPixmap, icon_pixmap);
    XtSetValues (f->output_data.x->widget, al, 1);
    XtSetArg (al[0], XtNiconMask, icon_mask);
    XtSetValues (f->output_data.x->widget, al, 1);
  }

#else /* not USE_X_TOOLKIT && not USE_GTK */

  f->output_data.x->wm_hints.flags |= (IconPixmapHint | IconMaskHint);
  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);

#endif /* not USE_X_TOOLKIT && not USE_GTK */
}

void
x_wm_set_icon_position (f, icon_x, icon_y)
     struct frame *f;
     int icon_x, icon_y;
{
  Window window = FRAME_OUTER_WINDOW (f);

  f->output_data.x->wm_hints.flags |= IconPositionHint;
  f->output_data.x->wm_hints.icon_x = icon_x;
  f->output_data.x->wm_hints.icon_y = icon_y;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);
}


/***********************************************************************
				Fonts
 ***********************************************************************/

/* Return a pointer to struct font_info of font FONT_IDX of frame F.  */

struct font_info *
x_get_font_info (f, font_idx)
     FRAME_PTR f;
     int font_idx;
{
  return (FRAME_X_FONT_TABLE (f) + font_idx);
}


/* Return a list of names of available fonts matching PATTERN on frame F.

   If SIZE is > 0, it is the size (maximum bounds width) of fonts
   to be listed.

   SIZE < 0 means include auto scaled fonts.

   Frame F null means we have not yet created any frame on X, and
   consult the first display in x_display_list.  MAXNAMES sets a limit
   on how many fonts to match.  */

Lisp_Object
x_list_fonts (f, pattern, size, maxnames)
     struct frame *f;
     Lisp_Object pattern;
     int size;
     int maxnames;
{
  Lisp_Object list = Qnil, patterns, newlist = Qnil, key = Qnil;
  Lisp_Object tem, second_best;
  struct x_display_info *dpyinfo
    = f ? FRAME_X_DISPLAY_INFO (f) : x_display_list;
  Display *dpy = dpyinfo->display;
  int try_XLoadQueryFont = 0;
  int allow_auto_scaled_font = 0;

  if (size < 0)
    {
      allow_auto_scaled_font = 1;
      size = 0;
    }

  patterns = Fassoc (pattern, Valternate_fontname_alist);
  if (NILP (patterns))
    patterns = Fcons (pattern, Qnil);

  if (maxnames == 1 && !size)
    /* We can return any single font matching PATTERN.  */
    try_XLoadQueryFont = 1;

  for (; CONSP (patterns); patterns = XCDR (patterns))
    {
      int num_fonts;
      char **names = NULL;

      pattern = XCAR (patterns);
      /* See if we cached the result for this particular query.
         The cache is an alist of the form:
	 ((((PATTERN . MAXNAMES) . SCALABLE) (FONTNAME . WIDTH) ...) ...)  */
      tem = XCDR (dpyinfo->name_list_element);
      key = Fcons (Fcons (pattern, make_number (maxnames)),
		   allow_auto_scaled_font ? Qt : Qnil);
      list = Fassoc (key, tem);
      if (!NILP (list))
	{
	  list = Fcdr_safe (list);
	  /* We have a cashed list.  Don't have to get the list again.  */
	  goto label_cached;
	}

      /* At first, put PATTERN in the cache.  */

      BLOCK_INPUT;
      x_catch_errors (dpy);

      if (try_XLoadQueryFont)
	{
	  XFontStruct *font;
	  unsigned long value;

	  font = XLoadQueryFont (dpy, SDATA (pattern));
	  if (x_had_errors_p (dpy))
	    {
	      /* This error is perhaps due to insufficient memory on X
                 server.  Let's just ignore it.  */
	      font = NULL;
	      x_clear_errors (dpy);
	    }

	  if (font
	      && XGetFontProperty (font, XA_FONT, &value))
	    {
	      char *name = (char *) XGetAtomName (dpy, (Atom) value);
	      int len = strlen (name);
	      char *tmp;

	      /* If DXPC (a Differential X Protocol Compressor)
                 Ver.3.7 is running, XGetAtomName will return null
                 string.  We must avoid such a name.  */
	      if (len == 0)
		try_XLoadQueryFont = 0;
	      else
		{
		  num_fonts = 1;
		  names = (char **) alloca (sizeof (char *));
		  /* Some systems only allow alloca assigned to a
                     simple var.  */
		  tmp = (char *) alloca (len + 1);  names[0] = tmp;
		  bcopy (name, names[0], len + 1);
		  XFree (name);
		}
	    }
	  else
	    try_XLoadQueryFont = 0;

	  if (font)
	    XFreeFont (dpy, font);
	}

      if (!try_XLoadQueryFont)
	{
	  /* We try at least 10 fonts because XListFonts will return
	     auto-scaled fonts at the head.  */
          if (maxnames < 0)
            {
              int limit;

              for (limit = 500;;)
                {
                  names = XListFonts (dpy, SDATA (pattern), limit, &num_fonts);
                  if (num_fonts == limit)
                    {
                      BLOCK_INPUT;
                      XFreeFontNames (names);
                      UNBLOCK_INPUT;
                      limit *= 2;
                    }
                  else
                    break;
                }
            }
          else
            names = XListFonts (dpy, SDATA (pattern), max (maxnames, 10),
                                &num_fonts);

	  if (x_had_errors_p (dpy))
	    {
	      /* This error is perhaps due to insufficient memory on X
                 server.  Let's just ignore it.  */
	      names = NULL;
	      x_clear_errors (dpy);
	    }
	}

      x_uncatch_errors ();
      UNBLOCK_INPUT;

      if (names)
	{
	  int i;

	  /* Make a list of all the fonts we got back.
	     Store that in the font cache for the display.  */
	  for (i = 0; i < num_fonts; i++)
	    {
	      int width = 0;
	      char *p = names[i];
	      int average_width = -1, resx = 0, dashes = 0;

	      /* Count the number of dashes in NAMES[I].  If there are
		 14 dashes, the field value following 9th dash
		 (RESOLUTION_X) is nonzero, and the field value
		 following 12th dash (AVERAGE_WIDTH) is 0, this is a
		 auto-scaled font which is usually too ugly to be used
		 for editing.  Let's ignore it.  */
	      while (*p)
		if (*p++ == '-')
		  {
		    dashes++;
		    if (dashes == 7) /* PIXEL_SIZE field */
		      width = atoi (p);
		    else if (dashes == 9)
		      resx = atoi (p);
		    else if (dashes == 12) /* AVERAGE_WIDTH field */
		      average_width = atoi (p);
		  }

	      if (allow_auto_scaled_font
		  || dashes < 14 || average_width != 0 || resx == 0)
		{
		  tem = build_string (names[i]);
		  if (NILP (Fassoc (tem, list)))
		    {
		      if (STRINGP (Vx_pixel_size_width_font_regexp)
			  && ((fast_c_string_match_ignore_case
			       (Vx_pixel_size_width_font_regexp, names[i]))
			      >= 0))
			/* We can set the value of PIXEL_SIZE to the
			  width of this font.  */
			list = Fcons (Fcons (tem, make_number (width)), list);
		      else
			/* For the moment, width is not known.  */
			list = Fcons (Fcons (tem, Qnil), list);
		    }
		}
	    }

	  if (!try_XLoadQueryFont)
	    {
	      BLOCK_INPUT;
	      XFreeFontNames (names);
	      UNBLOCK_INPUT;
	    }
	}

      /* Now store the result in the cache.  */
      XSETCDR (dpyinfo->name_list_element,
	       Fcons (Fcons (key, list), XCDR (dpyinfo->name_list_element)));

    label_cached:
      if (NILP (list)) continue; /* Try the remaining alternatives.  */

      newlist = second_best = Qnil;
      /* Make a list of the fonts that have the right width.  */
      for (; CONSP (list); list = XCDR (list))
	{
	  int found_size;

	  tem = XCAR (list);

	  if (!CONSP (tem) || NILP (XCAR (tem)))
	    continue;
	  if (!size)
	    {
	      newlist = Fcons (XCAR (tem), newlist);
	      continue;
	    }

	  if (!INTEGERP (XCDR (tem)))
	    {
	      /* Since we have not yet known the size of this font, we
		 must try slow function call XLoadQueryFont.  */
	      XFontStruct *thisinfo;

	      BLOCK_INPUT;
	      x_catch_errors (dpy);
	      thisinfo = XLoadQueryFont (dpy,
					 SDATA (XCAR (tem)));
	      if (x_had_errors_p (dpy))
		{
		  /* This error is perhaps due to insufficient memory on X
		     server.  Let's just ignore it.  */
		  thisinfo = NULL;
		  x_clear_errors (dpy);
		}
	      x_uncatch_errors ();
	      UNBLOCK_INPUT;

	      if (thisinfo)
		{
		  XSETCDR (tem,
			   (thisinfo->min_bounds.width == 0
			    ? make_number (0)
			    : make_number (thisinfo->max_bounds.width)));
		  BLOCK_INPUT;
		  XFreeFont (dpy, thisinfo);
		  UNBLOCK_INPUT;
		}
	      else
		/* For unknown reason, the previous call of XListFont had
		  returned a font which can't be opened.  Record the size
		  as 0 not to try to open it again.  */
		XSETCDR (tem, make_number (0));
	    }

	  found_size = XINT (XCDR (tem));
	  if (found_size == size)
	    newlist = Fcons (XCAR (tem), newlist);
	  else if (found_size > 0)
	    {
	      if (NILP (second_best))
		second_best = tem;
	      else if (found_size < size)
		{
		  if (XINT (XCDR (second_best)) > size
		      || XINT (XCDR (second_best)) < found_size)
		    second_best = tem;
		}
	      else
		{
		  if (XINT (XCDR (second_best)) > size
		      && XINT (XCDR (second_best)) > found_size)
		    second_best = tem;
		}
	    }
	}
      if (!NILP (newlist))
	break;
      else if (!NILP (second_best))
	{
	  newlist = Fcons (XCAR (second_best), Qnil);
	  break;
	}
    }

  return newlist;
}


#if GLYPH_DEBUG

/* Check that FONT is valid on frame F.  It is if it can be found in F's
   font table.  */

static void
x_check_font (f, font)
     struct frame *f;
     XFontStruct *font;
{
  int i;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  xassert (font != NULL);

#ifdef USE_FONT_BACKEND
  if (enable_font_backend)
    /* Fixme: Perhaps we should check all cached fonts.  */
    return;
#endif
  for (i = 0; i < dpyinfo->n_fonts; i++)
    if (dpyinfo->font_table[i].name
	&& font == dpyinfo->font_table[i].font)
      break;

  xassert (i < dpyinfo->n_fonts);
}

#endif /* GLYPH_DEBUG != 0 */

/* Set *W to the minimum width, *H to the minimum font height of FONT.
   Note: There are (broken) X fonts out there with invalid XFontStruct
   min_bounds contents.  For example, handa@etl.go.jp reports that
   "-adobe-courier-medium-r-normal--*-180-*-*-m-*-iso8859-1" fonts
   have font->min_bounds.width == 0.  */

static INLINE void
x_font_min_bounds (font, w, h)
     XFontStruct *font;
     int *w, *h;
{
  *h = FONT_HEIGHT (font);
  *w = font->min_bounds.width;

  /* Try to handle the case where FONT->min_bounds has invalid
     contents.  Since the only font known to have invalid min_bounds
     is fixed-width, use max_bounds if min_bounds seems to be invalid.  */
  if (*w <= 0)
    *w = font->max_bounds.width;
}


/* Compute the smallest character width and smallest font height over
   all fonts available on frame F.  Set the members smallest_char_width
   and smallest_font_height in F's x_display_info structure to
   the values computed.  Value is non-zero if smallest_font_height or
   smallest_char_width become smaller than they were before.  */

static int
x_compute_min_glyph_bounds (f)
     struct frame *f;
{
  int i;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  XFontStruct *font;
  int old_width = dpyinfo->smallest_char_width;
  int old_height = dpyinfo->smallest_font_height;

  dpyinfo->smallest_font_height = 100000;
  dpyinfo->smallest_char_width = 100000;

  for (i = 0; i < dpyinfo->n_fonts; ++i)
    if (dpyinfo->font_table[i].name)
      {
	struct font_info *fontp = dpyinfo->font_table + i;
	int w, h;

	font = (XFontStruct *) fontp->font;
	xassert (font != (XFontStruct *) ~0);
	x_font_min_bounds (font, &w, &h);

	dpyinfo->smallest_font_height = min (dpyinfo->smallest_font_height, h);
	dpyinfo->smallest_char_width = min (dpyinfo->smallest_char_width, w);
      }

  xassert (dpyinfo->smallest_char_width > 0
	   && dpyinfo->smallest_font_height > 0);

  return (dpyinfo->n_fonts == 1
	  || dpyinfo->smallest_char_width < old_width
	  || dpyinfo->smallest_font_height < old_height);
}


/* Load font named FONTNAME of the size SIZE for frame F, and return a
   pointer to the structure font_info while allocating it dynamically.
   If SIZE is 0, load any size of font.
   If loading is failed, return NULL.  */

struct font_info *
x_load_font (f, fontname, size)
     struct frame *f;
     register char *fontname;
     int size;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Lisp_Object font_names;

  /* Get a list of all the fonts that match this name.  Once we
     have a list of matching fonts, we compare them against the fonts
     we already have by comparing names.  */
  font_names = x_list_fonts (f, build_string (fontname), size, 1);

  if (!NILP (font_names))
    {
      Lisp_Object tail;
      int i;

      for (i = 0; i < dpyinfo->n_fonts; i++)
	for (tail = font_names; CONSP (tail); tail = XCDR (tail))
	  if (dpyinfo->font_table[i].name
	      && (!strcmp (dpyinfo->font_table[i].name,
			   SDATA (XCAR (tail)))
		  || !strcmp (dpyinfo->font_table[i].full_name,
			      SDATA (XCAR (tail)))))
	    return (dpyinfo->font_table + i);
    }

  /* Load the font and add it to the table.  */
  {
    char *full_name;
    XFontStruct *font;
    struct font_info *fontp;
    unsigned long value;
    int i;

    /* If we have found fonts by x_list_font, load one of them.  If
       not, we still try to load a font by the name given as FONTNAME
       because XListFonts (called in x_list_font) of some X server has
       a bug of not finding a font even if the font surely exists and
       is loadable by XLoadQueryFont.  */
    if (size > 0 && !NILP (font_names))
      fontname = (char *) SDATA (XCAR (font_names));

    BLOCK_INPUT;
    x_catch_errors (FRAME_X_DISPLAY (f));
    font = (XFontStruct *) XLoadQueryFont (FRAME_X_DISPLAY (f), fontname);
    if (x_had_errors_p (FRAME_X_DISPLAY (f)))
      {
	/* This error is perhaps due to insufficient memory on X
	   server.  Let's just ignore it.  */
	font = NULL;
	x_clear_errors (FRAME_X_DISPLAY (f));
      }
    x_uncatch_errors ();
    UNBLOCK_INPUT;
    if (!font)
      return NULL;

    /* Find a free slot in the font table.  */
    for (i = 0; i < dpyinfo->n_fonts; ++i)
      if (dpyinfo->font_table[i].name == NULL)
	break;

    /* If no free slot found, maybe enlarge the font table.  */
    if (i == dpyinfo->n_fonts
	&& dpyinfo->n_fonts == dpyinfo->font_table_size)
      {
	int sz;
	dpyinfo->font_table_size = max (16, 2 * dpyinfo->font_table_size);
	sz = dpyinfo->font_table_size * sizeof *dpyinfo->font_table;
	dpyinfo->font_table
	  = (struct font_info *) xrealloc (dpyinfo->font_table, sz);
      }

    fontp = dpyinfo->font_table + i;
    if (i == dpyinfo->n_fonts)
      ++dpyinfo->n_fonts;

    /* Now fill in the slots of *FONTP.  */
    BLOCK_INPUT;
    bzero (fontp, sizeof (*fontp));
    fontp->font = font;
    fontp->font_idx = i;
    fontp->charset = -1;	/* fs_load_font sets it.  */
    fontp->name = (char *) xmalloc (strlen (fontname) + 1);
    bcopy (fontname, fontp->name, strlen (fontname) + 1);

    if (font->min_bounds.width == font->max_bounds.width)
      {
	/* Fixed width font.  */
	fontp->average_width = fontp->space_width = font->min_bounds.width;
      }
    else
      {
	XChar2b char2b;
	XCharStruct *pcm;

	char2b.byte1 = 0x00, char2b.byte2 = 0x20;
	pcm = x_per_char_metric (font, &char2b, 0);
	if (pcm)
	  fontp->space_width = pcm->width;
	else
	  fontp->space_width = FONT_WIDTH (font);

	fontp->average_width
	  = (XGetFontProperty (font, dpyinfo->Xatom_AVERAGE_WIDTH, &value)
	     ? (long) value / 10 : 0);
	if (fontp->average_width < 0)
	  fontp->average_width = - fontp->average_width;
	if (fontp->average_width == 0)
	  {
	    if (pcm)
	      {
		int width = pcm->width;
		for (char2b.byte2 = 33; char2b.byte2 <= 126; char2b.byte2++)
		  if ((pcm = x_per_char_metric (font, &char2b, 0)) != NULL)
		    width += pcm->width;
		fontp->average_width = width / 95;
	      }
	    else
	      fontp->average_width = FONT_WIDTH (font);
	  }
      }

    /* Try to get the full name of FONT.  Put it in FULL_NAME.  */
    full_name = 0;
    if (XGetFontProperty (font, XA_FONT, &value))
      {
	char *name = (char *) XGetAtomName (FRAME_X_DISPLAY (f), (Atom) value);
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
	  {
	    full_name = (char *) xmalloc (p - name + 1);
	    bcopy (name, full_name, p - name + 1);
	  }

	XFree (name);
      }

    if (full_name != 0)
      fontp->full_name = full_name;
    else
      fontp->full_name = fontp->name;

    fontp->size = font->max_bounds.width;
    fontp->height = FONT_HEIGHT (font);

    if (NILP (font_names))
      {
	/* We come here because of a bug of XListFonts mentioned at
	   the head of this block.  Let's store this information in
	   the cache for x_list_fonts.  */
	Lisp_Object lispy_name = build_string (fontname);
	Lisp_Object lispy_full_name = build_string (fontp->full_name);
	Lisp_Object key = Fcons (Fcons (lispy_name, make_number (256)),
				 Qnil);

	XSETCDR (dpyinfo->name_list_element,
		 Fcons (Fcons (key,
			       Fcons (Fcons (lispy_full_name,
					     make_number (fontp->size)),
				      Qnil)),
			XCDR (dpyinfo->name_list_element)));
	if (full_name)
	  {
	    key = Fcons (Fcons (lispy_full_name, make_number (256)),
			 Qnil);
	    XSETCDR (dpyinfo->name_list_element,
		     Fcons (Fcons (key,
				   Fcons (Fcons (lispy_full_name,
						 make_number (fontp->size)),
					  Qnil)),
			    XCDR (dpyinfo->name_list_element)));
	  }
      }

    /* The slot `encoding' specifies how to map a character
       code-points (0x20..0x7F or 0x2020..0x7F7F) of each charset to
       the font code-points (0:0x20..0x7F, 1:0xA0..0xFF), or
       (0:0x2020..0x7F7F, 1:0xA0A0..0xFFFF, 3:0x20A0..0x7FFF,
       2:0xA020..0xFF7F).  For the moment, we don't know which charset
       uses this font.  So, we set information in fontp->encoding_type
       which is never used by any charset.  If mapping can't be
       decided, set FONT_ENCODING_NOT_DECIDED.  */
    fontp->encoding_type
      = (font->max_byte1 == 0
	 /* 1-byte font */
	 ? (font->min_char_or_byte2 < 0x80
	    ? (font->max_char_or_byte2 < 0x80
	       ? 0		/* 0x20..0x7F */
	       : FONT_ENCODING_NOT_DECIDED) /* 0x20..0xFF */
	    : 1)		/* 0xA0..0xFF */
	 /* 2-byte font */
	 : (font->min_byte1 < 0x80
	    ? (font->max_byte1 < 0x80
	       ? (font->min_char_or_byte2 < 0x80
		  ? (font->max_char_or_byte2 < 0x80
		     ? 0		/* 0x2020..0x7F7F */
		     : FONT_ENCODING_NOT_DECIDED) /* 0x2020..0x7FFF */
		  : 3)		/* 0x20A0..0x7FFF */
	       : FONT_ENCODING_NOT_DECIDED) /* 0x20??..0xA0?? */
	    : (font->min_char_or_byte2 < 0x80
	       ? (font->max_char_or_byte2 < 0x80
		  ? 2		/* 0xA020..0xFF7F */
		  : FONT_ENCODING_NOT_DECIDED) /* 0xA020..0xFFFF */
	       : 1)));		/* 0xA0A0..0xFFFF */

    fontp->baseline_offset
      = (XGetFontProperty (font, dpyinfo->Xatom_MULE_BASELINE_OFFSET, &value)
	 ? (long) value : 0);
    fontp->relative_compose
      = (XGetFontProperty (font, dpyinfo->Xatom_MULE_RELATIVE_COMPOSE, &value)
	 ? (long) value : 0);
    fontp->default_ascent
      = (XGetFontProperty (font, dpyinfo->Xatom_MULE_DEFAULT_ASCENT, &value)
	 ? (long) value : 0);

    /* Set global flag fonts_changed_p to non-zero if the font loaded
       has a character with a smaller width than any other character
       before, or if the font loaded has a smaller height than any
       other font loaded before.  If this happens, it will make a
       glyph matrix reallocation necessary.  */
    fonts_changed_p |= x_compute_min_glyph_bounds (f);
    UNBLOCK_INPUT;
    return fontp;
  }
}


/* Return a pointer to struct font_info of a font named FONTNAME for
   frame F.  If no such font is loaded, return NULL.  */

struct font_info *
x_query_font (f, fontname)
     struct frame *f;
     register char *fontname;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  int i;

  for (i = 0; i < dpyinfo->n_fonts; i++)
    if (dpyinfo->font_table[i].name
	&& (!xstricmp (dpyinfo->font_table[i].name, fontname)
	    || !xstricmp (dpyinfo->font_table[i].full_name, fontname)))
      return (dpyinfo->font_table + i);
  return NULL;
}


/* Find a CCL program for a font specified by FONTP, and set the member
 `encoder' of the structure.  */

void
x_find_ccl_program (fontp)
     struct font_info *fontp;
{
  Lisp_Object list, elt;

  elt = Qnil;
  for (list = Vfont_ccl_encoder_alist; CONSP (list); list = XCDR (list))
    {
      elt = XCAR (list);
      if (CONSP (elt)
	  && STRINGP (XCAR (elt))
	  && ((fast_c_string_match_ignore_case (XCAR (elt), fontp->name)
	       >= 0)
	      || (fast_c_string_match_ignore_case (XCAR (elt), fontp->full_name)
		  >= 0)))
	break;
    }

  if (! NILP (list))
    {
      struct ccl_program *ccl
	= (struct ccl_program *) xmalloc (sizeof (struct ccl_program));

      if (setup_ccl_program (ccl, XCDR (elt)) < 0)
	xfree (ccl);
      else
	fontp->font_encoder = ccl;
    }
}


/* Return a char-table whose elements are t if the font FONT_INFO
   contains a glyph for the corresponding character, and nil if
   not.  */

Lisp_Object
x_get_font_repertory (f, font_info)
     FRAME_PTR f;
     struct font_info *font_info;
{
  XFontStruct *font = (XFontStruct *) font_info->font;
  Lisp_Object table;
  int min_byte1, max_byte1, min_byte2, max_byte2;
  int c;
  struct charset *charset = CHARSET_FROM_ID (font_info->charset);
  int offset = CHARSET_OFFSET (charset);

  table = Fmake_char_table (Qnil, Qnil);

  min_byte1 = font->min_byte1;
  max_byte1 = font->max_byte1;
  min_byte2 = font->min_char_or_byte2;
  max_byte2 = font->max_char_or_byte2;
  if (min_byte1 == 0 && max_byte1 == 0)
    {
      if (! font->per_char || font->all_chars_exist == True)
	{
	  if (offset >= 0)
	    char_table_set_range (table, offset + min_byte2,
				  offset + max_byte2, Qt);
	  else
	    for (; min_byte2 <= max_byte2; min_byte2++)
	      {
		c = DECODE_CHAR (charset, min_byte2);
		CHAR_TABLE_SET (table, c, Qt);
	      }
	}
      else
	{
	  XCharStruct *pcm = font->per_char;
	  int from = -1;
	  int i;

	  for (i = min_byte2; i <= max_byte2; i++, pcm++)
	    {
	      if (pcm->width == 0 && pcm->rbearing == pcm->lbearing)
		{
		  if (from >= 0)
		    {
		      if (offset >= 0)
			char_table_set_range (table, offset + from,
					      offset + i - 1, Qt);
		      else
			for (; from < i; from++)
			  {
			    c = DECODE_CHAR (charset, from);
			    CHAR_TABLE_SET (table, c, Qt);
			  }
		      from = -1;
		    }
		}
	      else if (from < 0)
		from = i;
	    }
	  if (from >= 0)
	    {
	      if (offset >= 0)
		char_table_set_range (table, offset + from, offset + i - 1,
				      Qt);
	      else
		for (; from < i; from++)
		  {
		    c = DECODE_CHAR (charset, from);
		    CHAR_TABLE_SET (table, c, Qt);
		  }
	    }
	}
    }
  else
    {
      if (! font->per_char || font->all_chars_exist == True)
	{
	  int i, j;

	  if (offset >= 0)
	    for (i = min_byte1; i <= max_byte1; i++)
	      char_table_set_range
		(table, offset + ((i << 8) | min_byte2),
		 offset + ((i << 8) | max_byte2), Qt);
	  else
	    for (i = min_byte1; i <= max_byte1; i++)	    
	      for (j = min_byte2; j <= max_byte2; j++)
		{
		  unsigned code = (i << 8) | j;
		  c = DECODE_CHAR (charset, code);
		  CHAR_TABLE_SET (table, c, Qt);
		}
	}
      else
	{
	  XCharStruct *pcm = font->per_char;
	  int i;

	  for (i = min_byte1; i <= max_byte1; i++)
	    {
	      int from = -1;
	      int j;

	      for (j = min_byte2; j <= max_byte2; j++, pcm++)
		{
		  if (pcm->width == 0 && pcm->rbearing == pcm->lbearing)
		    {
		      if (from >= 0)
			{
			  if (offset >= 0)
			    char_table_set_range
			      (table, offset + ((i << 8) | from),
			       offset + ((i << 8) | (j - 1)), Qt);
			  else
			    {
			      for (; from < j; from++)
				{
				  unsigned code = (i << 8) | from;
				  c = ENCODE_CHAR (charset, code);
				  CHAR_TABLE_SET (table, c, Qt);
				}
			    }
			  from = -1;
			}
		    }
		  else if (from < 0)
		    from = j;
		}
	      if (from >= 0)
		{
		  if (offset >= 0)
		    char_table_set_range
		      (table, offset + ((i << 8) | from),
		       offset + ((i << 8) | (j - 1)), Qt);
		  else
		    {
		      for (; from < j; from++)
			{
			  unsigned code = (i << 8) | from;
			  c = DECODE_CHAR (charset, code);
			  CHAR_TABLE_SET (table, c, Qt);
			}
		    }
		}
	    }
	}
    }

  return table;
}

/***********************************************************************
			    Initialization
 ***********************************************************************/

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

/* Whether atimer for Xt timeouts is activated or not.  */

static int x_timeout_atimer_activated_flag;

#endif /* USE_X_TOOLKIT */

static int x_initialized;

#ifdef HAVE_X_SM
static int x_session_initialized;
#endif

#ifdef MULTI_KBOARD
/* Test whether two display-name strings agree up to the dot that separates
   the screen number from the server number.  */
static int
same_x_server (name1, name2)
     const char *name1, *name2;
{
  int seen_colon = 0;
  const unsigned char *system_name = SDATA (Vsystem_name);
  int system_name_length = strlen (system_name);
  int length_until_period = 0;

  while (system_name[length_until_period] != 0
	 && system_name[length_until_period] != '.')
    length_until_period++;

  /* Treat `unix' like an empty host name.  */
  if (! strncmp (name1, "unix:", 5))
    name1 += 4;
  if (! strncmp (name2, "unix:", 5))
    name2 += 4;
  /* Treat this host's name like an empty host name.  */
  if (! strncmp (name1, system_name, system_name_length)
      && name1[system_name_length] == ':')
    name1 += system_name_length;
  if (! strncmp (name2, system_name, system_name_length)
      && name2[system_name_length] == ':')
    name2 += system_name_length;
  /* Treat this host's domainless name like an empty host name.  */
  if (! strncmp (name1, system_name, length_until_period)
      && name1[length_until_period] == ':')
    name1 += length_until_period;
  if (! strncmp (name2, system_name, length_until_period)
      && name2[length_until_period] == ':')
    name2 += length_until_period;

  for (; *name1 != '\0' && *name1 == *name2; name1++, name2++)
    {
      if (*name1 == ':')
	seen_colon++;
      if (seen_colon && *name1 == '.')
	return 1;
    }
  return (seen_colon
	  && (*name1 == '.' || *name1 == '\0')
	  && (*name2 == '.' || *name2 == '\0'));
}
#endif

/* Count number of set bits in mask and number of bits to shift to
   get to the first bit.  With MASK 0x7e0, *BITS is set to 6, and *OFFSET
   to 5.  */
static void
get_bits_and_offset (mask, bits, offset)
     unsigned long mask;
     int *bits;
     int *offset;
{
  int nr = 0;
  int off = 0;

  while (!(mask & 1))
    {
      off++;
      mask >>= 1;
    }

  while (mask & 1)
    {
      nr++;
      mask >>= 1;
    }

  *offset = off;
  *bits = nr;
}

/* Return 1 if display DISPLAY is available for use, 0 otherwise.
   But don't permanently open it, just test its availability.  */

int
x_display_ok (display)
    const char *display;
{
    int dpy_ok = 1;
    Display *dpy;

    dpy = XOpenDisplay (display);
    if (dpy)
      XCloseDisplay (dpy);
    else
      dpy_ok = 0;
    return dpy_ok;
}

/* Open a connection to X display DISPLAY_NAME, and return
   the structure that describes the open display.
   If we cannot contact the display, return null.  */

struct x_display_info *
x_term_init (display_name, xrm_option, resource_name)
     Lisp_Object display_name;
     char *xrm_option;
     char *resource_name;
{
  int connection;
  Display *dpy;
  struct terminal *terminal;
  struct x_display_info *dpyinfo;
  XrmDatabase xrdb;

  BLOCK_INPUT;

  if (!x_initialized)
    {
      x_initialize ();
      ++x_initialized;
    }

  if (! x_display_ok (SDATA (display_name)))
    error ("Display %s can't be opened", SDATA (display_name));

#ifdef USE_GTK
  {
#define NUM_ARGV 10
    int argc;
    char *argv[NUM_ARGV];
    char **argv2 = argv;
    GdkAtom atom;

#ifndef HAVE_GTK_MULTIDISPLAY
    if (!EQ (Vinitial_window_system, intern ("x")))
      error ("Sorry, you cannot connect to X servers with the GTK toolkit");
#endif

    if (x_initialized++ > 1)
      {
#ifdef HAVE_GTK_MULTIDISPLAY
        /* Opening another display.  If xg_display_open returns less
           than zero, we are probably on GTK 2.0, which can only handle
           one display.  GTK 2.2 or later can handle more than one.  */
        if (xg_display_open (SDATA (display_name), &dpy) < 0)
#endif
          error ("Sorry, this version of GTK can only handle one display");
      }
    else
      {
        for (argc = 0; argc < NUM_ARGV; ++argc)
          argv[argc] = 0;

        argc = 0;
        argv[argc++] = initial_argv[0];

        if (! NILP (display_name))
          {
            argv[argc++] = "--display";
            argv[argc++] = SDATA (display_name);
          }

        argv[argc++] = "--name";
        argv[argc++] = resource_name;

        XSetLocaleModifiers ("");

        gtk_init (&argc, &argv2);

        /* gtk_init does set_locale.  We must fix locale after calling it.  */
        fixup_locale ();
        xg_initialize ();

        dpy = GDK_DISPLAY ();

        /* NULL window -> events for all windows go to our function */
        gdk_window_add_filter (NULL, event_handler_gdk, NULL);

        /* Load our own gtkrc if it exists.  */
        {
          char *file = "~/.emacs.d/gtkrc";
          Lisp_Object s, abs_file;

          s = make_string (file, strlen (file));
          abs_file = Fexpand_file_name (s, Qnil);

          if (! NILP (abs_file) && !NILP (Ffile_readable_p (abs_file)))
            gtk_rc_parse (SDATA (abs_file));
        }

        XSetErrorHandler (x_error_handler);
        XSetIOErrorHandler (x_io_error_quitter);
      }
  }
#else /* not USE_GTK */
#ifdef USE_X_TOOLKIT
  /* weiner@footloose.sps.mot.com reports that this causes
     errors with X11R5:
	   X protocol error: BadAtom (invalid Atom parameter)
	   on protocol request 18skiloaf.
     So let's not use it until R6.  */
#ifdef HAVE_X11XTR6
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
    turn_on_atimers (0);
    dpy = XtOpenDisplay (Xt_app_con, SDATA (display_name),
			 resource_name, EMACS_CLASS,
			 emacs_options, XtNumber (emacs_options),
			 &argc, argv);
    turn_on_atimers (1);

#ifdef HAVE_X11XTR6
    /* I think this is to compensate for XtSetLanguageProc.  */
    fixup_locale ();
#endif
  }

#else /* not USE_X_TOOLKIT */
  XSetLocaleModifiers ("");
  dpy = XOpenDisplay (SDATA (display_name));
#endif /* not USE_X_TOOLKIT */
#endif /* not USE_GTK*/

  /* Detect failure.  */
  if (dpy == 0)
    {
      UNBLOCK_INPUT;
      return 0;
    }

  /* We have definitely succeeded.  Record the new connection.  */

  dpyinfo = (struct x_display_info *) xmalloc (sizeof (struct x_display_info));
  bzero (dpyinfo, sizeof *dpyinfo);

  terminal = x_create_terminal (dpyinfo);

#ifdef MULTI_KBOARD
  {
    struct x_display_info *share;
    Lisp_Object tail;

    for (share = x_display_list, tail = x_display_name_list; share;
	 share = share->next, tail = XCDR (tail))
      if (same_x_server (SDATA (XCAR (XCAR (tail))),
			 SDATA (display_name)))
	break;
    if (share)
      terminal->kboard = share->terminal->kboard;
    else
      {
	terminal->kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
	init_kboard (terminal->kboard);
	terminal->kboard->Vwindow_system = intern ("x");
	if (!EQ (XSYMBOL (Qvendor_specific_keysyms)->function, Qunbound))
	  {
	    char *vendor = ServerVendor (dpy);
	    UNBLOCK_INPUT;
	    terminal->kboard->Vsystem_key_alist
	      = call1 (Qvendor_specific_keysyms,
		       vendor ? build_string (vendor) : empty_unibyte_string);
	    BLOCK_INPUT;
	  }

	terminal->kboard->next_kboard = all_kboards;
	all_kboards = terminal->kboard;
	/* Don't let the initial kboard remain current longer than necessary.
	   That would cause problems if a file loaded on startup tries to
	   prompt in the mini-buffer.  */
	if (current_kboard == initial_kboard)
	  current_kboard = terminal->kboard;
      }
    terminal->kboard->reference_count++;
  }
#endif

  /* Put this display on the chain.  */
  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;

  /* Put it on x_display_name_list as well, to keep them parallel.  */
  x_display_name_list = Fcons (Fcons (display_name, Qnil),
			       x_display_name_list);
  dpyinfo->name_list_element = XCAR (x_display_name_list);

  dpyinfo->display = dpy;

  /* Set the name of the terminal. */
  terminal->name = (char *) xmalloc (SBYTES (display_name) + 1);
  strncpy (terminal->name, SDATA (display_name), SBYTES (display_name));
  terminal->name[SBYTES (display_name)] = 0;
  
#if 0
  XSetAfterFunction (x_current_display, x_trace_wire);
#endif /* ! 0 */

  dpyinfo->x_id_name
    = (char *) xmalloc (SBYTES (Vinvocation_name)
			+ SBYTES (Vsystem_name)
			+ 2);
  sprintf (dpyinfo->x_id_name, "%s@%s",
	   SDATA (Vinvocation_name), SDATA (Vsystem_name));

  /* Figure out which modifier bits mean what.  */
  x_find_modifier_meanings (dpyinfo);

  /* Get the scroll bar cursor.  */
#ifdef USE_GTK
  /* We must create a GTK cursor, it is required for GTK widgets.  */
  dpyinfo->xg_cursor = xg_create_default_cursor (dpyinfo->display);
#endif /* USE_GTK */

  dpyinfo->vertical_scroll_bar_cursor
    = XCreateFontCursor (dpyinfo->display, XC_sb_v_double_arrow);

  xrdb = x_load_resources (dpyinfo->display, xrm_option,
			   resource_name, EMACS_CLASS);
#ifdef HAVE_XRMSETDATABASE
  XrmSetDatabase (dpyinfo->display, xrdb);
#else
  dpyinfo->display->db = xrdb;
#endif
  /* Put the rdb where we can find it in a way that works on
     all versions.  */
  dpyinfo->xrdb = xrdb;

  dpyinfo->screen = ScreenOfDisplay (dpyinfo->display,
				     DefaultScreen (dpyinfo->display));
  select_visual (dpyinfo);
  dpyinfo->cmap = DefaultColormapOfScreen (dpyinfo->screen);
  dpyinfo->height = HeightOfScreen (dpyinfo->screen);
  dpyinfo->width = WidthOfScreen (dpyinfo->screen);
  dpyinfo->root_window = RootWindowOfScreen (dpyinfo->screen);
  dpyinfo->client_leader_window = 0;
  dpyinfo->grabbed = 0;
  dpyinfo->reference_count = 0;
  dpyinfo->icon_bitmap_id = -1;
  dpyinfo->font_table = NULL;
  dpyinfo->n_fonts = 0;
  dpyinfo->font_table_size = 0;
#ifdef USE_FONT_BACKEND
  dpyinfo->font = XLoadQueryFont (dpyinfo->display, "fixed");
  if (! dpyinfo->font)
    dpyinfo->font = XLoadQueryFont (dpyinfo->display, "*");
  if (! dpyinfo->font)
    abort ();
#endif	/* USE_FONT_BACKEND */
  dpyinfo->bitmaps = 0;
  dpyinfo->bitmaps_size = 0;
  dpyinfo->bitmaps_last = 0;
  dpyinfo->scratch_cursor_gc = 0;
  dpyinfo->mouse_face_mouse_frame = 0;
  dpyinfo->mouse_face_deferred_gc = 0;
  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_face_id = DEFAULT_FACE_ID;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_overlay = Qnil;
  dpyinfo->mouse_face_mouse_x = dpyinfo->mouse_face_mouse_y = 0;
  dpyinfo->mouse_face_defer = 0;
  dpyinfo->mouse_face_hidden = 0;
  dpyinfo->x_focus_frame = 0;
  dpyinfo->x_focus_event_frame = 0;
  dpyinfo->x_highlight_frame = 0;
  dpyinfo->terminal->image_cache = make_image_cache ();
  dpyinfo->wm_type = X_WMTYPE_UNKNOWN;

  /* See if we can construct pixel values from RGB values.  */
  dpyinfo->red_bits = dpyinfo->blue_bits = dpyinfo->green_bits = 0;
  dpyinfo->red_offset = dpyinfo->blue_offset = dpyinfo->green_offset = 0;

  if (dpyinfo->visual->class == TrueColor)
    {
      get_bits_and_offset (dpyinfo->visual->red_mask,
                           &dpyinfo->red_bits, &dpyinfo->red_offset);
      get_bits_and_offset (dpyinfo->visual->blue_mask,
                           &dpyinfo->blue_bits, &dpyinfo->blue_offset);
      get_bits_and_offset (dpyinfo->visual->green_mask,
                           &dpyinfo->green_bits, &dpyinfo->green_offset);
    }

  /* See if a private colormap is requested.  */
  if (dpyinfo->visual == DefaultVisualOfScreen (dpyinfo->screen))
    {
      if (dpyinfo->visual->class == PseudoColor)
	{
	  Lisp_Object value;
	  value = display_x_get_resource (dpyinfo,
					  build_string ("privateColormap"),
					  build_string ("PrivateColormap"),
					  Qnil, Qnil);
	  if (STRINGP (value)
	      && (!strcmp (SDATA (value), "true")
		  || !strcmp (SDATA (value), "on")))
	    dpyinfo->cmap = XCopyColormapAndFree (dpyinfo->display, dpyinfo->cmap);
	}
    }
  else
    dpyinfo->cmap = XCreateColormap (dpyinfo->display, dpyinfo->root_window,
				     dpyinfo->visual, AllocNone);

  {
    int screen_number = XScreenNumberOfScreen (dpyinfo->screen);
    double pixels = DisplayHeight (dpyinfo->display, screen_number);
    double mm = DisplayHeightMM (dpyinfo->display, screen_number);
    /* Mac OS X 10.3's Xserver sometimes reports 0.0mm.  */
    dpyinfo->resy = (mm < 1) ? 100 : pixels * 25.4 / mm;
    pixels = DisplayWidth (dpyinfo->display, screen_number);
    mm = DisplayWidthMM (dpyinfo->display, screen_number);
    /* Mac OS X 10.3's Xserver sometimes reports 0.0mm.  */
    dpyinfo->resx = (mm < 1) ? 100 : pixels * 25.4 / mm;
  }

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
  dpyinfo->Xatom_wm_client_leader
    = XInternAtom (dpyinfo->display, "WM_CLIENT_LEADER", False);
  dpyinfo->Xatom_editres
    = XInternAtom (dpyinfo->display, "Editres", False);
  dpyinfo->Xatom_CLIPBOARD
    = XInternAtom (dpyinfo->display, "CLIPBOARD", False);
  dpyinfo->Xatom_TIMESTAMP
    = XInternAtom (dpyinfo->display, "TIMESTAMP", False);
  dpyinfo->Xatom_TEXT
    = XInternAtom (dpyinfo->display, "TEXT", False);
  dpyinfo->Xatom_COMPOUND_TEXT
    = XInternAtom (dpyinfo->display, "COMPOUND_TEXT", False);
  dpyinfo->Xatom_UTF8_STRING
    = XInternAtom (dpyinfo->display, "UTF8_STRING", False);
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
  /* For properties of font.  */
  dpyinfo->Xatom_PIXEL_SIZE
    = XInternAtom (dpyinfo->display, "PIXEL_SIZE", False);
  dpyinfo->Xatom_AVERAGE_WIDTH
    = XInternAtom (dpyinfo->display, "AVERAGE_WIDTH", False);
  dpyinfo->Xatom_MULE_BASELINE_OFFSET
    = XInternAtom (dpyinfo->display, "_MULE_BASELINE_OFFSET", False);
  dpyinfo->Xatom_MULE_RELATIVE_COMPOSE
    = XInternAtom (dpyinfo->display, "_MULE_RELATIVE_COMPOSE", False);
  dpyinfo->Xatom_MULE_DEFAULT_ASCENT
    = XInternAtom (dpyinfo->display, "_MULE_DEFAULT_ASCENT", False);

  /* Ghostscript support.  */
  dpyinfo->Xatom_PAGE = XInternAtom (dpyinfo->display, "PAGE", False);
  dpyinfo->Xatom_DONE = XInternAtom (dpyinfo->display, "DONE", False);

  dpyinfo->Xatom_Scrollbar = XInternAtom (dpyinfo->display, "SCROLLBAR",
					  False);

  dpyinfo->Xatom_XEMBED = XInternAtom (dpyinfo->display, "_XEMBED",
				       False);

  dpyinfo->cut_buffers_initialized = 0;

  dpyinfo->x_dnd_atoms_size = 8;
  dpyinfo->x_dnd_atoms_length = 0;
  dpyinfo->x_dnd_atoms = xmalloc (sizeof (*dpyinfo->x_dnd_atoms)
                                  * dpyinfo->x_dnd_atoms_size);

  dpyinfo->net_supported_atoms = NULL;
  dpyinfo->nr_net_supported_atoms = 0;
  dpyinfo->net_supported_window = 0;

  connection = ConnectionNumber (dpyinfo->display);
  dpyinfo->connection = connection;

  {
    char null_bits[1];

    null_bits[0] = 0x00;

    dpyinfo->null_pixel
      = XCreatePixmapFromBitmapData (dpyinfo->display, dpyinfo->root_window,
				     null_bits, 1, 1, (long) 0, (long) 0,
				     1);
  }

  {
    extern int gray_bitmap_width, gray_bitmap_height;
    extern char *gray_bitmap_bits;
    dpyinfo->gray
      = XCreatePixmapFromBitmapData (dpyinfo->display, dpyinfo->root_window,
				     gray_bitmap_bits,
				     gray_bitmap_width, gray_bitmap_height,
				     (unsigned long) 1, (unsigned long) 0, 1);
  }

#ifdef HAVE_X_I18N
  xim_initialize (dpyinfo, resource_name);
#endif

#ifdef subprocesses
  /* This is only needed for distinguishing keyboard and process input.  */
  if (connection != 0)
    add_keyboard_wait_descriptor (connection);
#endif

#ifdef F_SETOWN
  fcntl (connection, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN) */

#ifdef SIGIO
  if (interrupt_input)
    init_sigio (connection);
#endif /* ! defined (SIGIO) */

#ifdef USE_LUCID
  {
    Display *dpy = dpyinfo->display;
    XrmValue d, fr, to;
    Font font;

    d.addr = (XPointer)&dpy;
    d.size = sizeof (Display *);
    fr.addr = XtDefaultFont;
    fr.size = sizeof (XtDefaultFont);
    to.size = sizeof (Font *);
    to.addr = (XPointer)&font;
    x_catch_errors (dpy);
    if (!XtCallConverter (dpy, XtCvtStringToFont, &d, 1, &fr, &to, NULL))
      abort ();
    if (x_had_errors_p (dpy) || !XQueryFont (dpy, font))
      XrmPutLineResource (&xrdb, "Emacs.dialog.*.font: 9x15");
    x_uncatch_errors ();
  }
#endif

  /* See if we should run in synchronous mode.  This is useful
     for debugging X code.  */
  {
    Lisp_Object value;
    value = display_x_get_resource (dpyinfo,
				    build_string ("synchronous"),
				    build_string ("Synchronous"),
				    Qnil, Qnil);
    if (STRINGP (value)
	&& (!strcmp (SDATA (value), "true")
	    || !strcmp (SDATA (value), "on")))
      XSynchronize (dpyinfo->display, True);
  }

  {
    Lisp_Object value;
    value = display_x_get_resource (dpyinfo,
				    build_string ("useXIM"),
				    build_string ("UseXIM"),
				    Qnil, Qnil);
#ifdef USE_XIM
    if (STRINGP (value)
	&& (!strcmp (SDATA (value), "false")
	    || !strcmp (SDATA (value), "off")))
      use_xim = 0;
#else
    if (STRINGP (value)
	&& (!strcmp (SDATA (value), "true")
	    || !strcmp (SDATA (value), "on")))
      use_xim = 1;
#endif
  }

#ifdef HAVE_X_SM
  /* Only do this for the very first display in the Emacs session.
     Ignore X session management when Emacs was first started on a
     tty.  */
  if (terminal->id == 1)
    x_session_initialize (dpyinfo);
#endif

  UNBLOCK_INPUT;

  return dpyinfo;
}

/* Get rid of display DPYINFO, deleting all frames on it,
   and without sending any more commands to the X server.  */

void
x_delete_display (dpyinfo)
     struct x_display_info *dpyinfo;
{
  int i;
  struct terminal *t;

  /* Close all frames and delete the generic struct terminal for this
     X display.  */
  for (t = terminal_list; t; t = t->next_terminal)
    if (t->type == output_x_window && t->display_info.x == dpyinfo)
      {
#ifdef HAVE_X_SM
        /* Close X session management when we close its display.  */
        if (t->id == 1 && x_session_have_connection ())
          x_session_close();
#endif
        delete_terminal (t);
        break;
      }

  delete_keyboard_wait_descriptor (dpyinfo->connection);

  /* Discard this display from x_display_name_list and x_display_list.
     We can't use Fdelq because that can quit.  */
  if (! NILP (x_display_name_list)
      && EQ (XCAR (x_display_name_list), dpyinfo->name_list_element))
    x_display_name_list = XCDR (x_display_name_list);
  else
    {
      Lisp_Object tail;

      tail = x_display_name_list;
      while (CONSP (tail) && CONSP (XCDR (tail)))
	{
	  if (EQ (XCAR (XCDR (tail)), dpyinfo->name_list_element))
	    {
	      XSETCDR (tail, XCDR (XCDR (tail)));
	      break;
	    }
	  tail = XCDR (tail);
	}
    }

  if (next_noop_dpyinfo == dpyinfo)
    next_noop_dpyinfo = dpyinfo->next;

  if (x_display_list == dpyinfo)
    x_display_list = dpyinfo->next;
  else
    {
      struct x_display_info *tail;

      for (tail = x_display_list; tail; tail = tail->next)
	if (tail->next == dpyinfo)
	  tail->next = tail->next->next;
    }

#ifndef USE_X_TOOLKIT   /* I'm told Xt does this itself.  */
#ifndef AIX		/* On AIX, XCloseDisplay calls this.  */
  XrmDestroyDatabase (dpyinfo->xrdb);
#endif
#endif
#ifdef HAVE_X_I18N
  if (dpyinfo->xim)
    xim_close_dpy (dpyinfo);
#endif

#ifdef USE_FONT_BACKEND
  if (! enable_font_backend)
    {
#endif
  /* Free the font names in the font table.  */
  for (i = 0; i < dpyinfo->n_fonts; i++)
    if (dpyinfo->font_table[i].name)
      {
	if (dpyinfo->font_table[i].name != dpyinfo->font_table[i].full_name)
	  xfree (dpyinfo->font_table[i].full_name);
	xfree (dpyinfo->font_table[i].name);
      }

  if (dpyinfo->font_table)
    {
      if (dpyinfo->font_table->font_encoder)
	xfree (dpyinfo->font_table->font_encoder);
      xfree (dpyinfo->font_table);
    }
#ifdef USE_FONT_BACKEND
    }
#endif

  if (dpyinfo->x_id_name)
    xfree (dpyinfo->x_id_name);
  if (dpyinfo->color_cells)
    xfree (dpyinfo->color_cells);
  xfree (dpyinfo);
}

#ifdef USE_X_TOOLKIT

/* Atimer callback function for TIMER.  Called every 0.1s to process
   Xt timeouts, if needed.  We must avoid calling XtAppPending as
   much as possible because that function does an implicit XFlush
   that slows us down.  */

static void
x_process_timeouts (timer)
     struct atimer *timer;
{
  BLOCK_INPUT;
  x_timeout_atimer_activated_flag = 0;
  if (toolkit_scroll_bar_interaction || popup_activated ())
    {
      while (XtAppPending (Xt_app_con) & XtIMTimer)
	XtAppProcessEvent (Xt_app_con, XtIMTimer);
      /* Reactivate the atimer for next time.  */
      x_activate_timeout_atimer ();
    }
  UNBLOCK_INPUT;
}

/* Install an asynchronous timer that processes Xt timeout events
   every 0.1s as long as either `toolkit_scroll_bar_interaction' or
   `popup_activated_flag' (in xmenu.c) is set.  Make sure to call this
   function whenever these variables are set.  This is necessary
   because some widget sets use timeouts internally, for example the
   LessTif menu bar, or the Xaw3d scroll bar.  When Xt timeouts aren't
   processed, these widgets don't behave normally.  */

void
x_activate_timeout_atimer ()
{
  BLOCK_INPUT;
  if (!x_timeout_atimer_activated_flag)
    {
      EMACS_TIME interval;

      EMACS_SET_SECS_USECS (interval, 0, 100000);
      start_atimer (ATIMER_RELATIVE, interval, x_process_timeouts, 0);
      x_timeout_atimer_activated_flag = 1;
    }
  UNBLOCK_INPUT;
}

#endif /* USE_X_TOOLKIT */


/* Set up use of X before we make the first connection.  */

extern frame_parm_handler x_frame_parm_handlers[];

static struct redisplay_interface x_redisplay_interface =
  {
    x_frame_parm_handlers,
    x_produce_glyphs,
    x_write_glyphs,
    x_insert_glyphs,
    x_clear_end_of_line,
    x_scroll_run,
    x_after_update_window_line,
    x_update_window_begin,
    x_update_window_end,
    x_cursor_to,
    x_flush,
#ifdef XFlush
    x_flush,
#else
    0,  /* flush_display_optional */
#endif
    x_clear_window_mouse_face,
    x_get_glyph_overhangs,
    x_fix_overlapping_area,
    x_draw_fringe_bitmap,
    0, /* define_fringe_bitmap */
    0, /* destroy_fringe_bitmap */
    x_per_char_metric,
    x_encode_char,
    x_compute_glyph_string_overhangs,
    x_draw_glyph_string,
    x_define_frame_cursor,
    x_clear_frame_area,
    x_draw_window_cursor,
    x_draw_vertical_window_border,
    x_shift_glyphs_for_insert
  };


/* This function is called when the last frame on a display is deleted. */
void
x_delete_terminal (struct terminal *terminal)
{
  struct x_display_info *dpyinfo = terminal->display_info.x;
  int i;

  /* Protect against recursive calls.  Fdelete_frame in
     delete_terminal calls us back when it deletes our last frame.  */
  if (!terminal->name)
    return;

  BLOCK_INPUT;
  /* If called from x_connection_closed, the display may already be closed
     and dpyinfo->display was set to 0 to indicate that.  */
  if (dpyinfo->display)
    {
#ifdef USE_FONT_BACKEND
      if (enable_font_backend)
	XFreeFont (dpyinfo->display, dpyinfo->font);
      else
#endif
	/* Free the fonts in the font table.  */
	for (i = 0; i < dpyinfo->n_fonts; i++)
	  if (dpyinfo->font_table[i].name)
	    {
	      XFreeFont (dpyinfo->display, dpyinfo->font_table[i].font);
	    }

      x_destroy_all_bitmaps (dpyinfo);
      XSetCloseDownMode (dpyinfo->display, DestroyAll);

#ifdef USE_GTK
      xg_display_close (dpyinfo->display);
#else
#ifdef USE_X_TOOLKIT
      XtCloseDisplay (dpyinfo->display);
#else
      XCloseDisplay (dpyinfo->display);
#endif
#endif /* ! USE_GTK */
    }

  x_delete_display (dpyinfo);
  UNBLOCK_INPUT;
}

/* Create a struct terminal, initialize it with the X11 specific
   functions and make DISPLAY->TERMINAL point to it.  */

static struct terminal *
x_create_terminal (struct x_display_info *dpyinfo)
{
  struct terminal *terminal;
  
  terminal = create_terminal ();

  terminal->type = output_x_window;
  terminal->display_info.x = dpyinfo;
  dpyinfo->terminal = terminal;

  /* kboard is initialized in x_term_init. */
  
  terminal->clear_frame_hook = x_clear_frame;
  terminal->ins_del_lines_hook = x_ins_del_lines;
  terminal->delete_glyphs_hook = x_delete_glyphs;
  terminal->ring_bell_hook = XTring_bell;
  terminal->reset_terminal_modes_hook = XTreset_terminal_modes;
  terminal->set_terminal_modes_hook = XTset_terminal_modes;
  terminal->update_begin_hook = x_update_begin;
  terminal->update_end_hook = x_update_end;
  terminal->set_terminal_window_hook = XTset_terminal_window;
  terminal->read_socket_hook = XTread_socket;
  terminal->frame_up_to_date_hook = XTframe_up_to_date;
  terminal->mouse_position_hook = XTmouse_position;
  terminal->frame_rehighlight_hook = XTframe_rehighlight;
  terminal->frame_raise_lower_hook = XTframe_raise_lower;
  terminal->fullscreen_hook = XTfullscreen_hook;
  terminal->set_vertical_scroll_bar_hook = XTset_vertical_scroll_bar;
  terminal->condemn_scroll_bars_hook = XTcondemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = XTredeem_scroll_bar;
  terminal->judge_scroll_bars_hook = XTjudge_scroll_bars;

  terminal->delete_frame_hook = x_destroy_window;
  terminal->delete_terminal_hook = x_delete_terminal;
  
  terminal->rif = &x_redisplay_interface;
  terminal->scroll_region_ok = 1;    /* We'll scroll partial frames. */
  terminal->char_ins_del_ok = 1;
  terminal->line_ins_del_ok = 1;         /* We'll just blt 'em. */
  terminal->fast_clear_end_of_line = 1;  /* X does this well. */
  terminal->memory_below_frame = 0;   /* We don't remember what scrolls
                                        off the bottom. */

  return terminal;
}

void
x_initialize ()
{
  baud_rate = 19200;

  x_noop_count = 0;
  last_tool_bar_item = -1;
  any_help_event_p = 0;
  ignore_next_mouse_click_timeout = 0;
#ifdef HAVE_X_SM
  x_session_initialized = 0;
#endif

#ifdef USE_GTK
  current_count = -1;
#endif

  /* Try to use interrupt input; if we can't, then start polling.  */
  Fset_input_interrupt_mode (Qt);

#ifdef USE_X_TOOLKIT
  XtToolkitInitialize ();

  Xt_app_con = XtCreateApplicationContext ();

  /* Register a converter from strings to pixels, which uses
     Emacs' color allocation infrastructure.  */
  XtAppSetTypeConverter (Xt_app_con,
			 XtRString, XtRPixel, cvt_string_to_pixel,
			 cvt_string_to_pixel_args,
			 XtNumber (cvt_string_to_pixel_args),
			 XtCacheByDisplay, cvt_pixel_dtor);

  XtAppSetFallbackResources (Xt_app_con, Xt_default_resources);
#endif

#ifdef USE_TOOLKIT_SCROLL_BARS
#ifndef USE_GTK
  xaw3d_arrow_scroll = False;
  xaw3d_pick_top = True;
#endif
#endif

  /* Note that there is no real way portable across R3/R4 to get the
     original error handler.  */
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_quitter);

  /* Disable Window Change signals;  they are handled by X events.  */
#if 0              /* Don't.  We may want to open tty frames later. */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_DFL);
#endif /* SIGWINCH */
#endif

  signal (SIGPIPE, x_connection_signal);
}


void
syms_of_xterm ()
{
  x_error_message = NULL;

  staticpro (&x_display_name_list);
  x_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;

  staticpro (&Qvendor_specific_keysyms);
  Qvendor_specific_keysyms = intern ("vendor-specific-keysyms");

  staticpro (&Qlatin_1);
  Qlatin_1 = intern ("latin-1");

  staticpro (&last_mouse_press_frame);
  last_mouse_press_frame = Qnil;

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       &x_use_underline_position_properties,
     doc: /* *Non-nil means make use of UNDERLINE_POSITION font properties.
A value of nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, for example 7x13 on XFree prior
to 4.1, set this to nil.  */);
  x_use_underline_position_properties = 1;

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       &x_underline_at_descent_line,
     doc: /* *Non-nil means to draw the underline at the same place as the descent line.
A value of nil means to draw the underline according to the value of the
variable `x-use-underline-position-properties', which is usually at the
baseline level.  The default value is nil.  */);
  x_underline_at_descent_line = 0;

  DEFVAR_BOOL ("x-mouse-click-focus-ignore-position",
	       &x_mouse_click_focus_ignore_position,
    doc: /* Non-nil means that a mouse click to focus a frame does not move point.
This variable is only used when the window manager requires that you
click on a frame to select it (give it focus).  In that case, a value
of nil, means that the selected window and cursor position changes to
reflect the mouse click position, while a non-nil value means that the
selected window or cursor position is preserved.  */);
  x_mouse_click_focus_ignore_position = 0;

  DEFVAR_LISP ("x-toolkit-scroll-bars", &Vx_toolkit_scroll_bars,
    doc: /* What X toolkit scroll bars Emacs uses.
A value of nil means Emacs doesn't use X toolkit scroll bars.
Otherwise, value is a symbol describing the X toolkit.  */);
#ifdef USE_TOOLKIT_SCROLL_BARS
#ifdef USE_MOTIF
  Vx_toolkit_scroll_bars = intern ("motif");
#elif defined HAVE_XAW3D
  Vx_toolkit_scroll_bars = intern ("xaw3d");
#elif USE_GTK
  Vx_toolkit_scroll_bars = intern ("gtk");
#else
  Vx_toolkit_scroll_bars = intern ("xaw");
#endif
#else
  Vx_toolkit_scroll_bars = Qnil;
#endif

  staticpro (&last_mouse_motion_frame);
  last_mouse_motion_frame = Qnil;

  Qmodifier_value = intern ("modifier-value");
  Qalt = intern ("alt");
  Fput (Qalt, Qmodifier_value, make_number (alt_modifier));
  Qhyper = intern ("hyper");
  Fput (Qhyper, Qmodifier_value, make_number (hyper_modifier));
  Qmeta = intern ("meta");
  Fput (Qmeta, Qmodifier_value, make_number (meta_modifier));
  Qsuper = intern ("super");
  Fput (Qsuper, Qmodifier_value, make_number (super_modifier));

  DEFVAR_LISP ("x-alt-keysym", &Vx_alt_keysym,
    doc: /* Which keys Emacs uses for the alt modifier.
This should be one of the symbols `alt', `hyper', `meta', `super'.
For example, `alt' means use the Alt_L and Alt_R keysyms.  The default
is nil, which is the same as `alt'.  */);
  Vx_alt_keysym = Qnil;

  DEFVAR_LISP ("x-hyper-keysym", &Vx_hyper_keysym,
    doc: /* Which keys Emacs uses for the hyper modifier.
This should be one of the symbols `alt', `hyper', `meta', `super'.
For example, `hyper' means use the Hyper_L and Hyper_R keysyms.  The
default is nil, which is the same as `hyper'.  */);
  Vx_hyper_keysym = Qnil;

  DEFVAR_LISP ("x-meta-keysym", &Vx_meta_keysym,
    doc: /* Which keys Emacs uses for the meta modifier.
This should be one of the symbols `alt', `hyper', `meta', `super'.
For example, `meta' means use the Meta_L and Meta_R keysyms.  The
default is nil, which is the same as `meta'.  */);
  Vx_meta_keysym = Qnil;

  DEFVAR_LISP ("x-super-keysym", &Vx_super_keysym,
    doc: /* Which keys Emacs uses for the super modifier.
This should be one of the symbols `alt', `hyper', `meta', `super'.
For example, `super' means use the Super_L and Super_R keysyms.  The
default is nil, which is the same as `super'.  */);
  Vx_super_keysym = Qnil;

  DEFVAR_LISP ("x-keysym-table", &Vx_keysym_table,
    doc: /* Hash table of character codes indexed by X keysym codes.  */);
  Vx_keysym_table = make_hash_table (Qeql, make_number (900),
				     make_float (DEFAULT_REHASH_SIZE),
				     make_float (DEFAULT_REHASH_THRESHOLD),
				     Qnil, Qnil, Qnil);
}

#endif /* HAVE_X_WINDOWS */

/* arch-tag: 6d4e4cb7-abc1-4302-9585-d84dcfb09d0f
   (do not change this comment) */
