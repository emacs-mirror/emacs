/* X Communication module for terminals which understand the X protocol.
   Copyright (C) 1989, 93, 94, 95, 96, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.

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

#include "systty.h"
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
#include "coding.h"
#include "ccl.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "gnu.h"
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

#ifdef USE_LUCID
extern int xlwmenu_window_p (Widget w, Window window);
extern void xlwmenu_redisplay P_ ((Widget));
#endif

#ifdef USE_X_TOOLKIT

extern void free_frame_menubar P_ ((struct frame *));
extern struct frame *x_menubar_window_to_frame P_ ((struct x_display_info *,
						    int));

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
#define ARROW_SCROLLBAR
#include <X11/Xaw3d/ScrollbarP.h>
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

#ifndef USE_X_TOOLKIT
#define x_any_window_to_frame x_window_to_frame
#define x_top_window_to_frame x_window_to_frame
#endif

#ifdef USE_X_TOOLKIT
#include "widget.h"
#ifndef XtNinitialState
#define XtNinitialState "initialState"
#endif
#endif

#define abs(x)	((x) < 0 ? -(x) : (x))

#define BETWEEN(X, LOWER, UPPER)  ((X) >= (LOWER) && (X) < (UPPER))


/* Fringe bitmaps.  */

enum fringe_bitmap_type
{
  NO_FRINGE_BITMAP,
  LEFT_TRUNCATION_BITMAP,
  RIGHT_TRUNCATION_BITMAP,
  OVERLAY_ARROW_BITMAP,
  CONTINUED_LINE_BITMAP,
  CONTINUATION_LINE_BITMAP,
  ZV_LINE_BITMAP
};

/* Bitmap drawn to indicate lines not displaying text if
   `indicate-empty-lines' is non-nil.  */

#define zv_width 8
#define zv_height 72
#define zv_period 3
static unsigned char zv_bits[] = {
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00,
  0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00, 0x00, 0x3c, 0x00};

/* An arrow like this: `<-'.  */

#define left_width 8
#define left_height 8
static unsigned char left_bits[] = {
   0x18, 0x0c, 0x06, 0x3f, 0x3f, 0x06, 0x0c, 0x18};

/* Right truncation arrow bitmap `->'.  */

#define right_width 8
#define right_height 8
static unsigned char right_bits[] = {
   0x18, 0x30, 0x60, 0xfc, 0xfc, 0x60, 0x30, 0x18};

/* Marker for continued lines.  */

#define continued_width 8
#define continued_height 8
static unsigned char continued_bits[] = {
   0x3c, 0x7c, 0xc0, 0xe4, 0xfc, 0x7c, 0x3c, 0x7c};

/* Marker for continuation lines.  */

#define continuation_width 8
#define continuation_height 8
static unsigned char continuation_bits[] = {
   0x3c, 0x3e, 0x03, 0x27, 0x3f, 0x3e, 0x3c, 0x3e};

/* Overlay arrow bitmap.  */

#if 0
/* A bomb.  */
#define ov_width 8
#define ov_height 8
static unsigned char ov_bits[] = {
   0x30, 0x08, 0x3c, 0x7e, 0x7a, 0x7a, 0x62, 0x3c};
#else
/* A triangular arrow.  */
#define ov_width 8
#define ov_height 8
static unsigned char ov_bits[] = {
   0x03, 0x0f, 0x1f, 0x3f, 0x3f, 0x1f, 0x0f, 0x03};

#endif

extern Lisp_Object Qhelp_echo;


/* Non-nil means Emacs uses toolkit scroll bars.  */

Lisp_Object Vx_toolkit_scroll_bars;

/* If a string, XTread_socket generates an event to display that string.
   (The display is done in read_char.)  */
   
static Lisp_Object help_echo;
static Lisp_Object help_echo_window;
static Lisp_Object help_echo_object;
static int help_echo_pos;

/* Temporary variable for XTread_socket.  */

static Lisp_Object previous_help_echo;

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static int any_help_event_p;

/* Non-zero means autoselect window with the mouse cursor.  */

int x_autoselect_window_p;

/* Non-zero means draw block and hollow cursor as wide as the glyph
   under it.  For example, if a block cursor is over a tab, it will be
   drawn as wide as that tab on the display.  */

int x_stretch_cursor_p;

/* Non-zero means make use of UNDERLINE_POSITION font properties.  */

int x_use_underline_position_properties;

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

/* Nominal cursor position -- where to draw output.  
   HPOS and VPOS are window relative glyph matrix coordinates.
   X and Y are window relative pixel coordinates.  */

struct cursor_pos output_cursor;

/* Non-zero means user is interacting with a toolkit scroll bar.  */

static int toolkit_scroll_bar_interaction;

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

FRAME_PTR last_mouse_frame;
static XRectangle last_mouse_glyph;
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

extern Lisp_Object Qface, Qmouse_face;

extern int errno;

/* A mask of extra modifier bits to put into every keyboard char.  */

extern EMACS_INT extra_keyboard_modifiers;

/* The keysyms to use for the various modifiers.  */

Lisp_Object Vx_alt_keysym, Vx_hyper_keysym, Vx_meta_keysym, Vx_super_keysym;
static Lisp_Object Qalt, Qhyper, Qmeta, Qsuper, Qmodifier_value;

static Lisp_Object Qvendor_specific_keysyms;

extern XrmDatabase x_load_resources P_ ((Display *, char *, char *, char *));
extern Lisp_Object x_icon_type P_ ((struct frame *));


/* Enumeration for overriding/changing the face to use for drawing
   glyphs in x_draw_glyphs.  */

enum draw_glyphs_face
{
  DRAW_NORMAL_TEXT,
  DRAW_INVERSE_VIDEO,
  DRAW_CURSOR,
  DRAW_MOUSE_FACE,
  DRAW_IMAGE_RAISED,
  DRAW_IMAGE_SUNKEN
};

static int cursor_in_mouse_face_p P_ ((struct window *));
static int clear_mouse_face P_ ((struct x_display_info *));
static int x_alloc_nearest_color_1 P_ ((Display *, Colormap, XColor *));
static void x_set_window_size_1 P_ ((struct frame *, int, int, int));
static const XColor *x_color_cells P_ ((Display *, int *));
static void x_update_window_end P_ ((struct window *, int, int));
static void frame_to_window_pixel_xy P_ ((struct window *, int *, int *));
void x_delete_display P_ ((struct x_display_info *));
static unsigned int x_x_to_emacs_modifiers P_ ((struct x_display_info *,
						unsigned));
static int fast_find_position P_ ((struct window *, int, int *, int *,
				   int *, int *, Lisp_Object));
static int fast_find_string_pos P_ ((struct window *, int, Lisp_Object,
				     int *, int *, int *, int *, int));
static void set_output_cursor P_ ((struct cursor_pos *));
static struct glyph *x_y_to_hpos_vpos P_ ((struct window *, int, int,
					   int *, int *, int *, int));
static void note_mode_line_highlight P_ ((struct window *, int, int));
static void note_mouse_highlight P_ ((struct frame *, int, int));
static void note_tool_bar_highlight P_ ((struct frame *f, int, int));
static void x_handle_tool_bar_click P_ ((struct frame *, XButtonEvent *));
static void show_mouse_face P_ ((struct x_display_info *,
				 enum draw_glyphs_face));
static int x_io_error_quitter P_ ((Display *));
int x_catch_errors P_ ((Display *));
void x_uncatch_errors P_ ((Display *, int));
void x_lower_frame P_ ((struct frame *));
void x_scroll_bar_clear P_ ((struct frame *));
int x_had_errors_p P_ ((Display *));
void x_wm_set_size_hint P_ ((struct frame *, long, int));
void x_raise_frame P_ ((struct frame *));
void x_set_window_size P_ ((struct frame *, int, int, int));
void x_wm_set_window_state P_ ((struct frame *, int));
void x_wm_set_icon_pixmap P_ ((struct frame *, int));
void x_initialize P_ ((void));
static void x_font_min_bounds P_ ((XFontStruct *, int *, int *));
static int x_compute_min_glyph_bounds P_ ((struct frame *));
static void x_draw_phys_cursor_glyph P_ ((struct window *,
					  struct glyph_row *,
					  enum draw_glyphs_face));
static void x_update_end P_ ((struct frame *));
static void XTframe_up_to_date P_ ((struct frame *));
static void XTset_terminal_modes P_ ((void));
static void XTreset_terminal_modes P_ ((void));
static void XTcursor_to P_ ((int, int, int, int));
static void x_write_glyphs P_ ((struct glyph *, int));
static void x_clear_end_of_line P_ ((int));
static void x_clear_frame P_ ((void));
static void x_clear_cursor P_ ((struct window *));
static void frame_highlight P_ ((struct frame *));
static void frame_unhighlight P_ ((struct frame *));
static void x_new_focus_frame P_ ((struct x_display_info *, struct frame *));
static void XTframe_rehighlight P_ ((struct frame *));
static void x_frame_rehighlight P_ ((struct x_display_info *));
static void x_draw_hollow_cursor P_ ((struct window *, struct glyph_row *));
static void x_draw_bar_cursor P_ ((struct window *, struct glyph_row *, int));
static int x_intersect_rectangles P_ ((XRectangle *, XRectangle *,
				       XRectangle *));
static void expose_frame P_ ((struct frame *, int, int, int, int));
static int expose_window_tree P_ ((struct window *, XRectangle *));
static int expose_window P_ ((struct window *, XRectangle *));
static void expose_area P_ ((struct window *, struct glyph_row *,
			     XRectangle *, enum glyph_row_area));
static int expose_line P_ ((struct window *, struct glyph_row *,
			     XRectangle *));
static void x_update_cursor_in_window_tree P_ ((struct window *, int));
static void x_update_window_cursor P_ ((struct window *, int));
static void x_erase_phys_cursor P_ ((struct window *));
void x_display_and_set_cursor P_ ((struct window *, int, int, int, int, int));
static void x_draw_fringe_bitmap P_ ((struct window *, struct glyph_row *, 
				      enum fringe_bitmap_type, int left_p));

static void x_clip_to_row P_ ((struct window *, struct glyph_row *,
			       GC, int));
static int x_phys_cursor_in_rect_p P_ ((struct window *, XRectangle *));
static void x_draw_row_fringe_bitmaps P_ ((struct window *, struct glyph_row *));
static void notice_overwritten_cursor P_ ((struct window *, enum glyph_row_area,
					   int, int, int, int));
static void x_flush P_ ((struct frame *f));
static void x_update_begin P_ ((struct frame *));
static void x_update_window_begin P_ ((struct window *));
static void x_draw_vertical_border P_ ((struct window *));
static void x_after_update_window_line P_ ((struct glyph_row *));
static INLINE void take_vertical_position_into_account P_ ((struct it *));
static void x_produce_stretch_glyph P_ ((struct it *));
static struct scroll_bar *x_window_to_scroll_bar P_ ((Window));
static void x_scroll_bar_report_motion P_ ((struct frame **, Lisp_Object *,
					    enum scroll_bar_part *,
					    Lisp_Object *, Lisp_Object *,
					    unsigned long *));
static void x_check_fullscreen P_ ((struct frame *));
static void x_check_fullscreen_move P_ ((struct frame *));

/* Flush display of frame F, or of all frames if F is null.  */

static void
x_flush (f)
     struct frame *f;
{
  BLOCK_INPUT;
  if (f == NULL)
    {
      Lisp_Object rest, frame;
      FOR_EACH_FRAME (rest, frame)
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


/* Draw a vertical window border to the right of window W if W doesn't
   have vertical scroll bars.  */

static void
x_draw_vertical_border (w)
     struct window *w;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  
  /* Redraw borders between horizontally adjacent windows.  Don't
     do it for frames with vertical scroll bars because either the
     right scroll bar of a window, or the left scroll bar of its
     neighbor will suffice as a border.  */
  if (!WINDOW_RIGHTMOST_P (w)
      && !FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    {
      int x0, x1, y0, y1;

      window_box_edges (w, -1, &x0, &y0, &x1, &y1);
      x1 += FRAME_X_RIGHT_FRINGE_WIDTH (f);
      y1 -= 1;
      
      XDrawLine (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), 
		 f->output_data.x->normal_gc, x1, y0, x1, y1);
    }
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
	x_display_and_set_cursor (w, 1, output_cursor.hpos,
				  output_cursor.vpos,
				  output_cursor.x, output_cursor.y);
      
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

  BLOCK_INPUT;
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
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
    {
      BLOCK_INPUT;
      x_draw_row_fringe_bitmaps (w, desired_row);
      UNBLOCK_INPUT;
    }
      
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
		    f->output_data.x->pixel_width - width,
		    y, width, height, False);
      UNBLOCK_INPUT;
    }
}


/* Draw the bitmap WHICH in one of the left or right fringes of
   window W.  ROW is the glyph row for which to display the bitmap; it
   determines the vertical position at which the bitmap has to be
   drawn.  */

static void
x_draw_fringe_bitmap (w, row, which, left_p)
     struct window *w;
     struct glyph_row *row;
     enum fringe_bitmap_type which;
     int left_p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Display *display = FRAME_X_DISPLAY (f);
  Window window = FRAME_X_WINDOW (f);
  int x, y, wd, h, dy;
  int b1, b2;
  unsigned char *bits;
  Pixmap pixmap;
  GC gc = f->output_data.x->normal_gc;
  struct face *face;
  int depth = DefaultDepthOfScreen (FRAME_X_SCREEN (f));

  /* Must clip because of partially visible lines.  */
  x_clip_to_row (w, row, gc, 1);

  /* Convert row to frame coordinates.  */
  y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);

  switch (which)
    {
    case NO_FRINGE_BITMAP:
      wd = 0;
      h = 0;
      break;

    case LEFT_TRUNCATION_BITMAP:
      wd = left_width;
      h = left_height;
      bits = left_bits;
      break;
      
    case OVERLAY_ARROW_BITMAP:
      wd = ov_width;
      h = ov_height;
      bits = ov_bits;
      break;
      
    case RIGHT_TRUNCATION_BITMAP:
      wd = right_width;
      h = right_height;
      bits = right_bits;
      break;

    case CONTINUED_LINE_BITMAP:
      wd = continued_width;
      h = continued_height;
      bits = continued_bits;
      break;
      
    case CONTINUATION_LINE_BITMAP:
      wd = continuation_width;
      h = continuation_height;
      bits = continuation_bits;
      break;

    case ZV_LINE_BITMAP:
      wd = zv_width;
      h = zv_height - (y % zv_period);
      bits = zv_bits + (y % zv_period);
      break;

    default:
      abort ();
    }

  /* Clip bitmap if too high.  */
  if (h > row->height)
    h = row->height;

  /* Set dy to the offset in the row to start drawing the bitmap.  */
  dy = (row->height - h) / 2;

  face = FACE_FROM_ID (f, FRINGE_FACE_ID);
  PREPARE_FACE_FOR_DISPLAY (f, face);

  /* Clear left fringe if no bitmap to draw or if bitmap doesn't fill
     the fringe.  */
  b1 = b2 = -1;
  if (left_p)
    {
      if (wd > FRAME_X_LEFT_FRINGE_WIDTH (f))
	wd = FRAME_X_LEFT_FRINGE_WIDTH (f);
      x = (WINDOW_TO_FRAME_PIXEL_X (w, 0)
	   - wd
	   - (FRAME_X_LEFT_FRINGE_WIDTH (f) - wd) / 2);
      if (wd < FRAME_X_LEFT_FRINGE_WIDTH (f) || row->height > h)
	{
	  /* If W has a vertical border to its left, don't draw over it.  */
	  int border = ((XFASTINT (w->left) > 0
			 && !FRAME_HAS_VERTICAL_SCROLL_BARS (f))
			? 1 : 0);
	  b1 = (window_box_left (w, -1)
		- FRAME_X_LEFT_FRINGE_WIDTH (f)
		+ border);
	  b2 = (FRAME_X_LEFT_FRINGE_WIDTH (f) - border);
	}
    }
  else
    {
      if (wd > FRAME_X_RIGHT_FRINGE_WIDTH (f))
	wd = FRAME_X_RIGHT_FRINGE_WIDTH (f);
      x = (window_box_right (w, -1)
	   + (FRAME_X_RIGHT_FRINGE_WIDTH (f) - wd) / 2);
      /* Clear right fringe if no bitmap to draw of if bitmap doesn't fill
	 the fringe.  */
      if (wd < FRAME_X_RIGHT_FRINGE_WIDTH (f) || row->height > h)
	{
	  b1 = window_box_right (w, -1);
	  b2 = FRAME_X_RIGHT_FRINGE_WIDTH (f);
	}
    }

  if (b1 >= 0)
    {
      int header_line_height = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
      
      /* In case the same realized face is used for fringes and
	 for something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 FillSolid in x_draw_glyph_string_background.  */
      if (face->stipple)
	XSetFillStyle (display, face->gc, FillOpaqueStippled);
      else
	XSetForeground (display, face->gc, face->background);
      
      XFillRectangle (display, window, face->gc,
		      b1,
		      WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
						       row->y)),
		      b2,
		      row->visible_height);
      if (!face->stipple)
	XSetForeground (display, face->gc, face->foreground);
    }

  if (which != NO_FRINGE_BITMAP)
    {
      /* Draw the bitmap.  I believe these small pixmaps can be cached
	 by the server.  */
      pixmap = XCreatePixmapFromBitmapData (display, window, bits, wd, h,
					    face->foreground,
					    face->background, depth);
      XCopyArea (display, pixmap, window, gc, 0, 0, wd, h, x, y + dy);
      XFreePixmap (display, pixmap);
    }
  
  XSetClipMask (display, gc, None);
}


/* Draw fringe bitmaps for glyph row ROW on window W.  Call this
   function with input blocked.  */

static void
x_draw_row_fringe_bitmaps (w, row)
     struct window *w;
     struct glyph_row *row;
{
  struct frame *f = XFRAME (w->frame);
  enum fringe_bitmap_type bitmap;

  xassert (interrupt_input_blocked);

  /* If row is completely invisible, because of vscrolling, we
     don't have to draw anything.  */
  if (row->visible_height <= 0)
    return;

  if (FRAME_X_LEFT_FRINGE_WIDTH (f) != 0)
    {
      /* Decide which bitmap to draw in the left fringe.  */
      if (row->overlay_arrow_p)
	bitmap = OVERLAY_ARROW_BITMAP;
      else if (row->truncated_on_left_p)
	bitmap = LEFT_TRUNCATION_BITMAP;
      else if (MATRIX_ROW_CONTINUATION_LINE_P (row))
	bitmap = CONTINUATION_LINE_BITMAP;
      else if (row->indicate_empty_line_p)
	bitmap = ZV_LINE_BITMAP;
      else
	bitmap = NO_FRINGE_BITMAP;

      x_draw_fringe_bitmap (w, row, bitmap, 1);
    }

  if (FRAME_X_RIGHT_FRINGE_WIDTH (f) != 0)
    {
      /* Decide which bitmap to draw in the right fringe.  */
      if (row->truncated_on_right_p)
	bitmap = RIGHT_TRUNCATION_BITMAP;
      else if (row->continued_p)
	bitmap = CONTINUED_LINE_BITMAP;
      else if (row->indicate_empty_line_p && FRAME_X_LEFT_FRINGE_WIDTH (f) == 0)
	bitmap = ZV_LINE_BITMAP;
      else
	bitmap = NO_FRINGE_BITMAP;

      x_draw_fringe_bitmap (w, row, bitmap, 0);
    }
}



/* This is called when starting Emacs and when restarting after
   suspend.  When starting Emacs, no X window is mapped.  And nothing
   must be done to Emacs's own window if it is suspended (though that
   rarely happens).  */

static void
XTset_terminal_modes ()
{
}

/* This is called when exiting or suspending Emacs.  Exiting will make
   the X-windows go away, and suspending requires no action.  */

static void
XTreset_terminal_modes ()
{
}



/***********************************************************************
			    Output Cursor
 ***********************************************************************/

/* Set the global variable output_cursor to CURSOR.  All cursor
   positions are relative to updated_window.  */

static void
set_output_cursor (cursor)
    struct cursor_pos *cursor;
{
  output_cursor.hpos = cursor->hpos;
  output_cursor.vpos = cursor->vpos;
  output_cursor.x = cursor->x;
  output_cursor.y = cursor->y;
}


/* Set a nominal cursor position.

   HPOS and VPOS are column/row positions in a window glyph matrix.  X
   and Y are window text area relative pixel positions.
   
   If this is done during an update, updated_window will contain the
   window that is being updated and the position is the future output
   cursor position for that window.  If updated_window is null, use
   selected_window and display the cursor at the given position.  */

static void
XTcursor_to (vpos, hpos, y, x)
     int vpos, hpos, y, x;
{
  struct window *w;

  /* If updated_window is not set, work on selected_window.  */
  if (updated_window)
    w = updated_window;
  else
    w = XWINDOW (selected_window);

  /* Set the output cursor.  */
  output_cursor.hpos = hpos;
  output_cursor.vpos = vpos;
  output_cursor.x = x;
  output_cursor.y = y;

  /* If not called as part of an update, really display the cursor.
     This will also set the cursor position of W.  */
  if (updated_window == NULL)
    {
      BLOCK_INPUT;
      x_display_cursor (w, 1, hpos, vpos, x, y);
      XFlush (FRAME_X_DISPLAY (SELECTED_FRAME ()));
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
			   Display Iterator
 ***********************************************************************/

/* Function prototypes of this page.  */

static struct face *x_get_glyph_face_and_encoding P_ ((struct frame *,
						       struct glyph *,
						       XChar2b *,
						       int *));
static struct face *x_get_char_face_and_encoding P_ ((struct frame *, int,
						      int, XChar2b *, int));
static XCharStruct *x_per_char_metric P_ ((XFontStruct *, XChar2b *));
static void x_encode_char P_ ((int, XChar2b *, struct font_info *));
static void x_append_glyph P_ ((struct it *));
static void x_append_composite_glyph P_ ((struct it *));
static void x_append_stretch_glyph P_ ((struct it *it, Lisp_Object,
					int, int, double));
static void x_produce_glyphs P_ ((struct it *));
static void x_produce_image_glyph P_ ((struct it *it));


/* Get metrics of character CHAR2B in FONT.  Value is null if CHAR2B
   is not contained in the font.  */

static INLINE XCharStruct *
x_per_char_metric (font, char2b)
     XFontStruct *font;
     XChar2b *char2b;
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

static INLINE void
x_encode_char (c, char2b, font_info)
     int c;
     XChar2b *char2b;
     struct font_info *font_info;
{
  int charset = CHAR_CHARSET (c);
  XFontStruct *font = font_info->font;

  /* FONT_INFO may define a scheme by which to encode byte1 and byte2.
     This may be either a program in a special encoder language or a
     fixed encoding.  */
  if (font_info->font_encoder)
    {
      /* It's a program.  */
      struct ccl_program *ccl = font_info->font_encoder;

      if (CHARSET_DIMENSION (charset) == 1)
	{
	  ccl->reg[0] = charset;
	  ccl->reg[1] = char2b->byte2;
	}
      else
	{
	  ccl->reg[0] = charset;
	  ccl->reg[1] = char2b->byte1;
	  ccl->reg[2] = char2b->byte2;
	}
      
      ccl_driver (ccl, NULL, NULL, 0, 0, NULL);
      
      /* We assume that MSBs are appropriately set/reset by CCL
	 program.  */
      if (font->max_byte1 == 0)	/* 1-byte font */
	char2b->byte1 = 0, char2b->byte2 = ccl->reg[1];
      else
	char2b->byte1 = ccl->reg[1], char2b->byte2 = ccl->reg[2];
    }
  else if (font_info->encoding[charset])
    {
      /* Fixed encoding scheme.  See fontset.h for the meaning of the
	 encoding numbers.  */
      int enc = font_info->encoding[charset];
      
      if ((enc == 1 || enc == 2)
	  && CHARSET_DIMENSION (charset) == 2)
	char2b->byte1 |= 0x80;
      
      if (enc == 1 || enc == 3)
	char2b->byte2 |= 0x80;
    }
}


/* Get face and two-byte form of character C in face FACE_ID on frame
   F.  The encoding of C is returned in *CHAR2B.  MULTIBYTE_P non-zero
   means we want to display multibyte text.  Value is a pointer to a
   realized face that is ready for display.  */

static INLINE struct face *
x_get_char_face_and_encoding (f, c, face_id, char2b, multibyte_p)
     struct frame *f;
     int c, face_id;
     XChar2b *char2b;
     int multibyte_p;
{
  struct face *face = FACE_FROM_ID (f, face_id);

  if (!multibyte_p)
    {
      /* Unibyte case.  We don't have to encode, but we have to make
	 sure to use a face suitable for unibyte.  */
      char2b->byte1 = 0;
      char2b->byte2 = c;
      face_id = FACE_FOR_CHAR (f, face, c);
      face = FACE_FROM_ID (f, face_id);
    }
  else if (c < 128 && face_id < BASIC_FACE_ID_SENTINEL)
    {
      /* Case of ASCII in a face known to fit ASCII.  */
      char2b->byte1 = 0;
      char2b->byte2 = c;
    }
  else
    {
      int c1, c2, charset;
      
      /* Split characters into bytes.  If c2 is -1 afterwards, C is
	 really a one-byte character so that byte1 is zero.  */
      SPLIT_CHAR (c, charset, c1, c2);
      if (c2 > 0)
	char2b->byte1 = c1, char2b->byte2 = c2;
      else
	char2b->byte1 = 0, char2b->byte2 = c1;

      /* Maybe encode the character in *CHAR2B.  */
      if (face->font != NULL)
	{
	  struct font_info *font_info
	    = FONT_INFO_FROM_ID (f, face->font_info_id);
	  if (font_info)
	    x_encode_char (c, char2b, font_info);
	}
    }

  /* Make sure X resources of the face are allocated.  */
  xassert (face != NULL);
  PREPARE_FACE_FOR_DISPLAY (f, face);
  
  return face;
}


/* Get face and two-byte form of character glyph GLYPH on frame F.
   The encoding of GLYPH->u.ch is returned in *CHAR2B.  Value is
   a pointer to a realized face that is ready for display.  */

static INLINE struct face *
x_get_glyph_face_and_encoding (f, glyph, char2b, two_byte_p)
     struct frame *f;
     struct glyph *glyph;
     XChar2b *char2b;
     int *two_byte_p;
{
  struct face *face;

  xassert (glyph->type == CHAR_GLYPH);
  face = FACE_FROM_ID (f, glyph->face_id);

  if (two_byte_p)
    *two_byte_p = 0;

  if (!glyph->multibyte_p)
    {
      /* Unibyte case.  We don't have to encode, but we have to make
	 sure to use a face suitable for unibyte.  */
      char2b->byte1 = 0;
      char2b->byte2 = glyph->u.ch;
    }
  else if (glyph->u.ch < 128
	   && glyph->face_id < BASIC_FACE_ID_SENTINEL)
    {
      /* Case of ASCII in a face known to fit ASCII.  */
      char2b->byte1 = 0;
      char2b->byte2 = glyph->u.ch;
    }
  else
    {
      int c1, c2, charset;
      
      /* Split characters into bytes.  If c2 is -1 afterwards, C is
	 really a one-byte character so that byte1 is zero.  */
      SPLIT_CHAR (glyph->u.ch, charset, c1, c2);
      if (c2 > 0)
	char2b->byte1 = c1, char2b->byte2 = c2;
      else
	char2b->byte1 = 0, char2b->byte2 = c1;

      /* Maybe encode the character in *CHAR2B.  */
      if (charset != CHARSET_ASCII)
	{
	  struct font_info *font_info
	    = FONT_INFO_FROM_ID (f, face->font_info_id);
	  if (font_info)
	    {
	      x_encode_char (glyph->u.ch, char2b, font_info);
	      if (two_byte_p)
		*two_byte_p
		  = ((XFontStruct *) (font_info->font))->max_byte1 > 0;
	    }
	}
    }

  /* Make sure X resources of the face are allocated.  */
  xassert (face != NULL);
  PREPARE_FACE_FOR_DISPLAY (f, face);
  return face;
}


/* Store one glyph for IT->char_to_display in IT->glyph_row.  
   Called from x_produce_glyphs when IT->glyph_row is non-null.  */

static INLINE void
x_append_glyph (it)
     struct it *it;
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;
  
  xassert (it->glyph_row);
  xassert (it->char_to_display != '\n' && it->char_to_display != '\t');
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      glyph->pixel_width = it->pixel_width;
      glyph->voffset = it->voffset;
      glyph->type = CHAR_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = it->glyph_not_available_p;
      glyph->face_id = it->face_id;
      glyph->u.ch = it->char_to_display;
      ++it->glyph_row->used[area];
    }
}

/* Store one glyph for the composition IT->cmp_id in IT->glyph_row.  
   Called from x_produce_glyphs when IT->glyph_row is non-null.  */

static INLINE void
x_append_composite_glyph (it)
     struct it *it;
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;
  
  xassert (it->glyph_row);
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = it->object;
      glyph->pixel_width = it->pixel_width;
      glyph->voffset = it->voffset;
      glyph->type = COMPOSITE_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = (it->phys_ascent > it->ascent
				      || it->phys_descent > it->descent);
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = it->face_id;
      glyph->u.cmp_id = it->cmp_id;
      ++it->glyph_row->used[area];
    }
}


/* Change IT->ascent and IT->height according to the setting of
   IT->voffset.  */

static INLINE void
take_vertical_position_into_account (it)
     struct it *it;
{
  if (it->voffset)
    {
      if (it->voffset < 0)
	/* Increase the ascent so that we can display the text higher
	   in the line.  */
	it->ascent += abs (it->voffset);
      else
	/* Increase the descent so that we can display the text lower
	   in the line.  */
	it->descent += it->voffset;
    }
}


/* Produce glyphs/get display metrics for the image IT is loaded with.
   See the description of struct display_iterator in dispextern.h for
   an overview of struct display_iterator.  */

static void
x_produce_image_glyph (it)
     struct it *it;
{
  struct image *img;
  struct face *face;

  xassert (it->what == IT_IMAGE);

  face = FACE_FROM_ID (it->f, it->face_id);
  img = IMAGE_FROM_ID (it->f, it->image_id);
  xassert (img);

  /* Make sure X resources of the face and image are loaded.  */
  PREPARE_FACE_FOR_DISPLAY (it->f, face);
  prepare_image_for_display (it->f, img);

  it->ascent = it->phys_ascent = image_ascent (img, face);
  it->descent = it->phys_descent = img->height + 2 * img->vmargin - it->ascent;
  it->pixel_width = img->width + 2 * img->hmargin;

  it->nglyphs = 1;
  
  if (face->box != FACE_NO_BOX)
    {
      if (face->box_line_width > 0)
	{
	  it->ascent += face->box_line_width;
	  it->descent += face->box_line_width;
	}
      
      if (it->start_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
      if (it->end_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
    }

  take_vertical_position_into_account (it);
  
  if (it->glyph_row)
    {
      struct glyph *glyph;
      enum glyph_row_area area = it->area;
      
      glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
      if (glyph < it->glyph_row->glyphs[area + 1])
	{
	  glyph->charpos = CHARPOS (it->position);
	  glyph->object = it->object;
	  glyph->pixel_width = it->pixel_width;
	  glyph->voffset = it->voffset;
	  glyph->type = IMAGE_GLYPH;
	  glyph->multibyte_p = it->multibyte_p;
	  glyph->left_box_line_p = it->start_of_box_run_p;
	  glyph->right_box_line_p = it->end_of_box_run_p;
	  glyph->overlaps_vertically_p = 0;
          glyph->padding_p = 0;
	  glyph->glyph_not_available_p = 0;
	  glyph->face_id = it->face_id;
	  glyph->u.img_id = img->id;
	  ++it->glyph_row->used[area];
	}
    }
}


/* Append a stretch glyph to IT->glyph_row.  OBJECT is the source
   of the glyph, WIDTH and HEIGHT are the width and height of the 
   stretch.  ASCENT is the percentage/100 of HEIGHT to use for the 
   ascent of the glyph (0 <= ASCENT <= 1).  */
  
static void
x_append_stretch_glyph (it, object, width, height, ascent)
     struct it *it;
     Lisp_Object object;
     int width, height;
     double ascent;
{
  struct glyph *glyph;
  enum glyph_row_area area = it->area;

  xassert (ascent >= 0 && ascent <= 1);
  
  glyph = it->glyph_row->glyphs[area] + it->glyph_row->used[area];
  if (glyph < it->glyph_row->glyphs[area + 1])
    {
      glyph->charpos = CHARPOS (it->position);
      glyph->object = object;
      glyph->pixel_width = width;
      glyph->voffset = it->voffset;
      glyph->type = STRETCH_GLYPH;
      glyph->multibyte_p = it->multibyte_p;
      glyph->left_box_line_p = it->start_of_box_run_p;
      glyph->right_box_line_p = it->end_of_box_run_p;
      glyph->overlaps_vertically_p = 0;
      glyph->padding_p = 0;
      glyph->glyph_not_available_p = 0;
      glyph->face_id = it->face_id;
      glyph->u.stretch.ascent = height * ascent;
      glyph->u.stretch.height = height;
      ++it->glyph_row->used[area];
    }
}


/* Produce a stretch glyph for iterator IT.  IT->object is the value
   of the glyph property displayed.  The value must be a list
   `(space KEYWORD VALUE ...)' with the following KEYWORD/VALUE pairs
   being recognized:

   1. `:width WIDTH' specifies that the space should be WIDTH *
   canonical char width wide.  WIDTH may be an integer or floating 
   point number.

   2. `:relative-width FACTOR' specifies that the width of the stretch
   should be computed from the width of the first character having the
   `glyph' property, and should be FACTOR times that width.

   3. `:align-to HPOS' specifies that the space should be wide enough
   to reach HPOS, a value in canonical character units.

   Exactly one of the above pairs must be present.  

   4. `:height HEIGHT' specifies that the height of the stretch produced
   should be HEIGHT, measured in canonical character units.

   5. `:relative-height FACTOR' specifies that the height of the
   stretch should be FACTOR times the height of the characters having
   the glyph property.

   Either none or exactly one of 4 or 5 must be present.

   6. `:ascent ASCENT'  specifies that ASCENT percent of the height
   of the stretch should be used for the ascent of the stretch.
   ASCENT must be in the range 0 <= ASCENT <= 100.  */

#define NUMVAL(X)				\
     ((INTEGERP (X) || FLOATP (X))		\
      ? XFLOATINT (X)				\
      : - 1)


static void
x_produce_stretch_glyph (it)
     struct it *it;
{
  /* (space :width WIDTH :height HEIGHT.  */
#if GLYPH_DEBUG
  extern Lisp_Object Qspace;
#endif
  extern Lisp_Object QCwidth, QCheight, QCascent;
  extern Lisp_Object QCrelative_width, QCrelative_height;
  extern Lisp_Object QCalign_to;
  Lisp_Object prop, plist;
  double width = 0, height = 0, ascent = 0;
  struct face *face = FACE_FROM_ID (it->f, it->face_id);
  XFontStruct *font = face->font ? face->font : FRAME_FONT (it->f);

  PREPARE_FACE_FOR_DISPLAY (it->f, face);
  
  /* List should start with `space'.  */
  xassert (CONSP (it->object) && EQ (XCAR (it->object), Qspace));
  plist = XCDR (it->object);

  /* Compute the width of the stretch.  */
  if (prop = Fplist_get (plist, QCwidth),
      NUMVAL (prop) > 0)
    /* Absolute width `:width WIDTH' specified and valid.  */
    width = NUMVAL (prop) * CANON_X_UNIT (it->f);
  else if (prop = Fplist_get (plist, QCrelative_width),
	   NUMVAL (prop) > 0)
    {
      /* Relative width `:relative-width FACTOR' specified and valid.
	 Compute the width of the characters having the `glyph'
	 property.  */
      struct it it2;
      unsigned char *p = BYTE_POS_ADDR (IT_BYTEPOS (*it));
      
      it2 = *it;
      if (it->multibyte_p)
	{
	  int maxlen = ((IT_BYTEPOS (*it) >= GPT ? ZV : GPT)
			- IT_BYTEPOS (*it));
	  it2.c = STRING_CHAR_AND_LENGTH (p, maxlen, it2.len);
	}
      else
	it2.c = *p, it2.len = 1;

      it2.glyph_row = NULL;
      it2.what = IT_CHARACTER;
      x_produce_glyphs (&it2);
      width = NUMVAL (prop) * it2.pixel_width;
    }
  else if (prop = Fplist_get (plist, QCalign_to),
	   NUMVAL (prop) > 0)
    width = NUMVAL (prop) * CANON_X_UNIT (it->f) - it->current_x;
  else
    /* Nothing specified -> width defaults to canonical char width.  */
    width = CANON_X_UNIT (it->f);
  
  /* Compute height.  */
  if (prop = Fplist_get (plist, QCheight),
      NUMVAL (prop) > 0)
    height = NUMVAL (prop) * CANON_Y_UNIT (it->f);
  else if (prop = Fplist_get (plist, QCrelative_height),
	   NUMVAL (prop) > 0)
    height = FONT_HEIGHT (font) * NUMVAL (prop);
  else
    height = FONT_HEIGHT (font);

  /* Compute percentage of height used for ascent.  If 
     `:ascent ASCENT' is present and valid, use that.  Otherwise,
     derive the ascent from the font in use.  */
  if (prop = Fplist_get (plist, QCascent),
      NUMVAL (prop) > 0 && NUMVAL (prop) <= 100)
    ascent = NUMVAL (prop) / 100.0;
  else
    ascent = (double) font->ascent / FONT_HEIGHT (font);

  if (width <= 0)
    width = 1;
  if (height <= 0)
    height = 1;

  if (it->glyph_row)
    {
      Lisp_Object object = it->stack[it->sp - 1].string;
      if (!STRINGP (object))
	object = it->w->buffer;
      x_append_stretch_glyph (it, object, width, height, ascent);
    }

  it->pixel_width = width;
  it->ascent = it->phys_ascent = height * ascent;
  it->descent = it->phys_descent = height - it->ascent;
  it->nglyphs = 1;

  if (face->box != FACE_NO_BOX)
    {
      if (face->box_line_width > 0)
	{
	  it->ascent += face->box_line_width;
	  it->descent += face->box_line_width;
	}
      
      if (it->start_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
      if (it->end_of_box_run_p)
	it->pixel_width += abs (face->box_line_width);
    }
  
  take_vertical_position_into_account (it);
}

/* Return proper value to be used as baseline offset of font that has
   ASCENT and DESCENT to draw characters by the font at the vertical
   center of the line of frame F.

   Here, out task is to find the value of BOFF in the following figure;

	-------------------------+-----------+-
	 -+-+---------+-+        |           |
	  | |         | |        |           |
	  | |         | |        F_ASCENT    F_HEIGHT
	  | |         | ASCENT   |           |
     HEIGHT |         | |        |           |
	  | |         |-|-+------+-----------|------- baseline
	  | |         | | BOFF   |           |
	  | |---------|-+-+      |           |
	  | |         | DESCENT  |           |
	 -+-+---------+-+        F_DESCENT   |
	-------------------------+-----------+-

	-BOFF + DESCENT + (F_HEIGHT - HEIGHT) / 2 = F_DESCENT
	BOFF = DESCENT +  (F_HEIGHT - HEIGHT) / 2 - F_DESCENT
	DESCENT = FONT->descent
	HEIGHT = FONT_HEIGHT (FONT)
	F_DESCENT = (F->output_data.x->font->descent
		     - F->output_data.x->baseline_offset)
	F_HEIGHT = FRAME_LINE_HEIGHT (F)
*/

#define VCENTER_BASELINE_OFFSET(FONT, F)			\
  ((FONT)->descent						\
   + (FRAME_LINE_HEIGHT ((F)) - FONT_HEIGHT ((FONT))		\
      + (FRAME_LINE_HEIGHT ((F)) > FONT_HEIGHT ((FONT)))) / 2	\
   - ((F)->output_data.x->font->descent - (F)->output_data.x->baseline_offset))

/* Produce glyphs/get display metrics for the display element IT is
   loaded with.  See the description of struct display_iterator in
   dispextern.h for an overview of struct display_iterator.  */

static void
x_produce_glyphs (it)
     struct it *it;
{
  it->glyph_not_available_p = 0;

  if (it->what == IT_CHARACTER)
    {
      XChar2b char2b;
      XFontStruct *font;
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      XCharStruct *pcm;
      int font_not_found_p;
      struct font_info *font_info;
      int boff;			/* baseline offset */
      /* We may change it->multibyte_p upon unibyte<->multibyte
	 conversion.  So, save the current value now and restore it
	 later.

	 Note: It seems that we don't have to record multibyte_p in
	 struct glyph because the character code itself tells if or
	 not the character is multibyte.  Thus, in the future, we must
	 consider eliminating the field `multibyte_p' in the struct
	 glyph.  */
      int saved_multibyte_p = it->multibyte_p;

      /* Maybe translate single-byte characters to multibyte, or the
	 other way.  */
      it->char_to_display = it->c;
      if (!ASCII_BYTE_P (it->c))
	{
	  if (unibyte_display_via_language_environment
	      && SINGLE_BYTE_CHAR_P (it->c)
	      && (it->c >= 0240
		  || !NILP (Vnonascii_translation_table)))
	    {
	      it->char_to_display = unibyte_char_to_multibyte (it->c);
	      it->multibyte_p = 1;
	      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
	      face = FACE_FROM_ID (it->f, it->face_id);
	    }
	  else if (!SINGLE_BYTE_CHAR_P (it->c)
		   && !it->multibyte_p)
	    {
	      it->multibyte_p = 1;
	      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
	      face = FACE_FROM_ID (it->f, it->face_id);
	    }
	}
      
      /* Get font to use.  Encode IT->char_to_display.  */
      x_get_char_face_and_encoding (it->f, it->char_to_display,
				    it->face_id, &char2b,
				    it->multibyte_p);
      font = face->font;

      /* When no suitable font found, use the default font.  */
      font_not_found_p = font == NULL;
      if (font_not_found_p)
	{
	  font = FRAME_FONT (it->f);
	  boff = it->f->output_data.x->baseline_offset;
	  font_info = NULL;
	}
      else
	{
	  font_info = FONT_INFO_FROM_ID (it->f, face->font_info_id);
	  boff = font_info->baseline_offset;
	  if (font_info->vertical_centering)
	    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
	}

      if (it->char_to_display >= ' '
	  && (!it->multibyte_p || it->char_to_display < 128))
	{
	  /* Either unibyte or ASCII.  */
	  int stretched_p;

	  it->nglyphs = 1;

	  pcm = x_per_char_metric (font, &char2b);
	  it->ascent = font->ascent + boff;
	  it->descent = font->descent - boff;

	  if (pcm)
	    {
	      it->phys_ascent = pcm->ascent + boff;
	      it->phys_descent = pcm->descent - boff;
	      it->pixel_width = pcm->width;
	    }
	  else
	    {
	      it->glyph_not_available_p = 1;
	      it->phys_ascent = font->ascent + boff;
	      it->phys_descent = font->descent - boff;
	      it->pixel_width = FONT_WIDTH (font);
	    }

	  /* If this is a space inside a region of text with
	     `space-width' property, change its width.  */
	  stretched_p = it->char_to_display == ' ' && !NILP (it->space_width);
	  if (stretched_p)
	    it->pixel_width *= XFLOATINT (it->space_width);

	  /* If face has a box, add the box thickness to the character
	     height.  If character has a box line to the left and/or
	     right, add the box line width to the character's width.  */
	  if (face->box != FACE_NO_BOX)
	    {
	      int thick = face->box_line_width;
	      
	      if (thick > 0)
		{
		  it->ascent += thick;
		  it->descent += thick;
		}
	      else
		thick = -thick;

	      if (it->start_of_box_run_p)
		it->pixel_width += thick;
	      if (it->end_of_box_run_p)
		it->pixel_width += thick;
	    }

	  /* If face has an overline, add the height of the overline
	     (1 pixel) and a 1 pixel margin to the character height.  */
	  if (face->overline_p)
	    it->ascent += 2;

	  take_vertical_position_into_account (it);
  
	  /* If we have to actually produce glyphs, do it.  */
	  if (it->glyph_row)
	    {
	      if (stretched_p)
		{
		  /* Translate a space with a `space-width' property
		     into a stretch glyph.  */
		  double ascent = (double) font->ascent / FONT_HEIGHT (font);
		  x_append_stretch_glyph (it, it->object, it->pixel_width, 
					  it->ascent + it->descent, ascent);
		}
	      else
		x_append_glyph (it);

	      /* If characters with lbearing or rbearing are displayed
		 in this line, record that fact in a flag of the
		 glyph row.  This is used to optimize X output code.  */
	      if (pcm && (pcm->lbearing < 0 || pcm->rbearing > pcm->width))
		it->glyph_row->contains_overlapping_glyphs_p = 1;
	    }
	}
      else if (it->char_to_display == '\n')
	{
	  /* A newline has no width but we need the height of the line.  */
	  it->pixel_width = 0;
	  it->nglyphs = 0;
	  it->ascent = it->phys_ascent = font->ascent + boff;
	  it->descent = it->phys_descent = font->descent - boff;
      
	  if (face->box != FACE_NO_BOX
	      && face->box_line_width > 0)
	    {
	      it->ascent += face->box_line_width;
	      it->descent += face->box_line_width;
	    }
	}
      else if (it->char_to_display == '\t')
	{
	  int tab_width = it->tab_width * CANON_X_UNIT (it->f);
	  int x = it->current_x + it->continuation_lines_width;
	  int next_tab_x = ((1 + x + tab_width - 1) / tab_width) * tab_width;

	  /* If the distance from the current position to the next tab
	     stop is less than a canonical character width, use the
	     tab stop after that.  */
	  if (next_tab_x - x < CANON_X_UNIT (it->f))
	    next_tab_x += tab_width;
      
	  it->pixel_width = next_tab_x - x;
	  it->nglyphs = 1;
	  it->ascent = it->phys_ascent = font->ascent + boff;
	  it->descent = it->phys_descent = font->descent - boff;
	  
	  if (it->glyph_row)
	    {
	      double ascent = (double) it->ascent / (it->ascent + it->descent);
	      x_append_stretch_glyph (it, it->object, it->pixel_width, 
				      it->ascent + it->descent, ascent);
	    }
	}
      else 
	{
	  /* A multi-byte character.  Assume that the display width of the
	     character is the width of the character multiplied by the
	     width of the font.  */

	  /* If we found a font, this font should give us the right
	     metrics.  If we didn't find a font, use the frame's
	     default font and calculate the width of the character
	     from the charset width; this is what old redisplay code
	     did.  */
	  pcm = x_per_char_metric (font, &char2b);
	  if (font_not_found_p || !pcm)
	    {
	      int charset = CHAR_CHARSET (it->char_to_display);

	      it->glyph_not_available_p = 1;
	      it->pixel_width = (FONT_WIDTH (FRAME_FONT (it->f))
				 * CHARSET_WIDTH (charset));
	      it->phys_ascent = font->ascent + boff;
	      it->phys_descent = font->descent - boff;
	    }
	  else
	    {
	      it->pixel_width = pcm->width;
	      it->phys_ascent = pcm->ascent + boff;
	      it->phys_descent = pcm->descent - boff;
	      if (it->glyph_row
		  && (pcm->lbearing < 0
		      || pcm->rbearing > pcm->width))
		it->glyph_row->contains_overlapping_glyphs_p = 1;
	    }
	  it->nglyphs = 1;
	  it->ascent = font->ascent + boff;
	  it->descent = font->descent - boff;
	  if (face->box != FACE_NO_BOX)
	    {
	      int thick = face->box_line_width;

	      if (thick > 0)
		{
		  it->ascent += thick;
		  it->descent += thick;
		}
	      else
		thick = - thick;
	  
	      if (it->start_of_box_run_p)
		it->pixel_width += thick;
	      if (it->end_of_box_run_p)
		it->pixel_width += thick;
	    }
  
	  /* If face has an overline, add the height of the overline
	     (1 pixel) and a 1 pixel margin to the character height.  */
	  if (face->overline_p)
	    it->ascent += 2;

	  take_vertical_position_into_account (it);
  
	  if (it->glyph_row)
	    x_append_glyph (it);
	}
      it->multibyte_p = saved_multibyte_p;
    }
  else if (it->what == IT_COMPOSITION)
    {
      /* Note: A composition is represented as one glyph in the
	 glyph matrix.  There are no padding glyphs.  */
      XChar2b char2b;
      XFontStruct *font;
      struct face *face = FACE_FROM_ID (it->f, it->face_id);
      XCharStruct *pcm;
      int font_not_found_p;
      struct font_info *font_info;
      int boff;			/* baseline offset */
      struct composition *cmp = composition_table[it->cmp_id];

      /* Maybe translate single-byte characters to multibyte.  */
      it->char_to_display = it->c;
      if (unibyte_display_via_language_environment
	  && SINGLE_BYTE_CHAR_P (it->c)
	  && (it->c >= 0240
	      || (it->c >= 0200
		  && !NILP (Vnonascii_translation_table))))
	{
	  it->char_to_display = unibyte_char_to_multibyte (it->c);
	}
      
      /* Get face and font to use.  Encode IT->char_to_display.  */
      it->face_id = FACE_FOR_CHAR (it->f, face, it->char_to_display);
      face = FACE_FROM_ID (it->f, it->face_id);
      x_get_char_face_and_encoding (it->f, it->char_to_display,
				    it->face_id, &char2b, it->multibyte_p);
      font = face->font;

      /* When no suitable font found, use the default font.  */
      font_not_found_p = font == NULL;
      if (font_not_found_p)
	{
	  font = FRAME_FONT (it->f);
	  boff = it->f->output_data.x->baseline_offset;
	  font_info = NULL;
	}
      else
	{
	  font_info = FONT_INFO_FROM_ID (it->f, face->font_info_id);
	  boff = font_info->baseline_offset;
	  if (font_info->vertical_centering)
	    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
	}

      /* There are no padding glyphs, so there is only one glyph to
	 produce for the composition.  Important is that pixel_width,
	 ascent and descent are the values of what is drawn by
	 draw_glyphs (i.e. the values of the overall glyphs composed).  */
      it->nglyphs = 1;

      /* If we have not yet calculated pixel size data of glyphs of
	 the composition for the current face font, calculate them
	 now.  Theoretically, we have to check all fonts for the
	 glyphs, but that requires much time and memory space.  So,
	 here we check only the font of the first glyph.  This leads
	 to incorrect display very rarely, and C-l (recenter) can
	 correct the display anyway.  */
      if (cmp->font != (void *) font)
	{
	  /* Ascent and descent of the font of the first character of
	     this composition (adjusted by baseline offset).  Ascent
	     and descent of overall glyphs should not be less than
	     them respectively.  */
	  int font_ascent = font->ascent + boff;
	  int font_descent = font->descent - boff;
	  /* Bounding box of the overall glyphs.  */
	  int leftmost, rightmost, lowest, highest;
	  int i, width, ascent, descent;

	  cmp->font = (void *) font;

	  /* Initialize the bounding box.  */
	  if (font_info
	      && (pcm = x_per_char_metric (font, &char2b)))
	    {
	      width = pcm->width;
	      ascent = pcm->ascent;
	      descent = pcm->descent;
	    }
	  else
	    {
	      width = FONT_WIDTH (font);
	      ascent = font->ascent;
	      descent = font->descent;
	    }
	  
	  rightmost = width;
	  lowest = - descent + boff;
	  highest = ascent + boff;
	  leftmost = 0;
	  
	  if (font_info
	      && font_info->default_ascent
	      && CHAR_TABLE_P (Vuse_default_ascent)
	      && !NILP (Faref (Vuse_default_ascent,
			       make_number (it->char_to_display))))
	    highest = font_info->default_ascent + boff;

	  /* Draw the first glyph at the normal position.  It may be
	     shifted to right later if some other glyphs are drawn at
	     the left.  */
	  cmp->offsets[0] = 0;
	  cmp->offsets[1] = boff;

	  /* Set cmp->offsets for the remaining glyphs.  */
	  for (i = 1; i < cmp->glyph_len; i++)
	    {
	      int left, right, btm, top;
	      int ch = COMPOSITION_GLYPH (cmp, i);
	      int face_id = FACE_FOR_CHAR (it->f, face, ch);
	      
	      face = FACE_FROM_ID (it->f, face_id);
	      x_get_char_face_and_encoding (it->f, ch, face->id, &char2b,
					    it->multibyte_p);
	      font = face->font;
	      if (font == NULL)
		{
		  font = FRAME_FONT (it->f);
		  boff = it->f->output_data.x->baseline_offset;
		  font_info = NULL;
		}
	      else
		{
		  font_info
		    = FONT_INFO_FROM_ID (it->f, face->font_info_id);
		  boff = font_info->baseline_offset;
		  if (font_info->vertical_centering)
		    boff = VCENTER_BASELINE_OFFSET (font, it->f) - boff;
		}

	      if (font_info
		  && (pcm = x_per_char_metric (font, &char2b)))
		{
		  width = pcm->width;
		  ascent = pcm->ascent;
		  descent = pcm->descent;
		}
	      else
		{
		  width = FONT_WIDTH (font);
		  ascent = 1;
		  descent = 0;
		}

	      if (cmp->method != COMPOSITION_WITH_RULE_ALTCHARS)
		{
		  /* Relative composition with or without
		     alternate chars.  */
		  left = (leftmost + rightmost - width) / 2;
		  btm = - descent + boff;
		  if (font_info && font_info->relative_compose
		      && (! CHAR_TABLE_P (Vignore_relative_composition)
			  || NILP (Faref (Vignore_relative_composition,
					  make_number (ch)))))
		    {

		      if (- descent >= font_info->relative_compose)
			/* One extra pixel between two glyphs.  */
			btm = highest + 1;
		      else if (ascent <= 0)
			/* One extra pixel between two glyphs.  */
			btm = lowest - 1 - ascent - descent;
		    }
		}
	      else
		{
		  /* A composition rule is specified by an integer
		     value that encodes global and new reference
		     points (GREF and NREF).  GREF and NREF are
		     specified by numbers as below:

			0---1---2 -- ascent
			|       |
			|       |
			|       |
			9--10--11 -- center
			|       |
		     ---3---4---5--- baseline
			|       |
			6---7---8 -- descent
		  */
		  int rule = COMPOSITION_RULE (cmp, i);
		  int gref, nref, grefx, grefy, nrefx, nrefy;

		  COMPOSITION_DECODE_RULE (rule, gref, nref);
		  grefx = gref % 3, nrefx = nref % 3;
		  grefy = gref / 3, nrefy = nref / 3;

		  left = (leftmost
			  + grefx * (rightmost - leftmost) / 2
			  - nrefx * width / 2);
		  btm = ((grefy == 0 ? highest
			  : grefy == 1 ? 0
			  : grefy == 2 ? lowest
			  : (highest + lowest) / 2)
			 - (nrefy == 0 ? ascent + descent
			    : nrefy == 1 ? descent - boff
			    : nrefy == 2 ? 0
			    : (ascent + descent) / 2));
		}

	      cmp->offsets[i * 2] = left;
	      cmp->offsets[i * 2 + 1] = btm + descent;

	      /* Update the bounding box of the overall glyphs. */
	      right = left + width;
	      top = btm + descent + ascent;
	      if (left < leftmost)
		leftmost = left;
	      if (right > rightmost)
		rightmost = right;
	      if (top > highest)
		highest = top;
	      if (btm < lowest)
		lowest = btm;
	    }

	  /* If there are glyphs whose x-offsets are negative,
	     shift all glyphs to the right and make all x-offsets
	     non-negative.  */
	  if (leftmost < 0)
	    {
	      for (i = 0; i < cmp->glyph_len; i++)
		cmp->offsets[i * 2] -= leftmost;
	      rightmost -= leftmost;
	    }

	  cmp->pixel_width = rightmost;
	  cmp->ascent = highest;
	  cmp->descent = - lowest;
	  if (cmp->ascent < font_ascent)
	    cmp->ascent = font_ascent;
	  if (cmp->descent < font_descent)
	    cmp->descent = font_descent;
	}

      it->pixel_width = cmp->pixel_width;
      it->ascent = it->phys_ascent = cmp->ascent;
      it->descent = it->phys_descent = cmp->descent;

      if (face->box != FACE_NO_BOX)
	{
	  int thick = face->box_line_width;

	  if (thick > 0)
	    {
	      it->ascent += thick;
	      it->descent += thick;
	    }
	  else
	    thick = - thick;
	  
	  if (it->start_of_box_run_p)
	    it->pixel_width += thick;
	  if (it->end_of_box_run_p)
	    it->pixel_width += thick;
	}
  
      /* If face has an overline, add the height of the overline
	 (1 pixel) and a 1 pixel margin to the character height.  */
      if (face->overline_p)
	it->ascent += 2;

      take_vertical_position_into_account (it);
  
      if (it->glyph_row)
	x_append_composite_glyph (it);
    }
  else if (it->what == IT_IMAGE)
    x_produce_image_glyph (it);
  else if (it->what == IT_STRETCH)
    x_produce_stretch_glyph (it);

  /* Accumulate dimensions.  Note: can't assume that it->descent > 0
     because this isn't true for images with `:ascent 100'.  */
  xassert (it->ascent >= 0 && it->descent >= 0);
  if (it->area == TEXT_AREA)
    it->current_x += it->pixel_width;
  
  it->descent += it->extra_line_spacing;
  
  it->max_ascent = max (it->max_ascent, it->ascent);
  it->max_descent = max (it->max_descent, it->descent);
  it->max_phys_ascent = max (it->max_phys_ascent, it->phys_ascent);
  it->max_phys_descent = max (it->max_phys_descent, it->phys_descent);
}


/* Estimate the pixel height of the mode or top line on frame F.
   FACE_ID specifies what line's height to estimate.  */

int
x_estimate_mode_line_height (f, face_id)
     struct frame *f;
     enum face_id face_id;
{
  int height = FONT_HEIGHT (FRAME_FONT (f));

  /* This function is called so early when Emacs starts that the face
     cache and mode line face are not yet initialized.  */
  if (FRAME_FACE_CACHE (f))
      {
	struct face *face = FACE_FROM_ID (f, face_id);
	if (face)
	  {
	    if (face->font)
	      height = FONT_HEIGHT (face->font);
	    if (face->box_line_width > 0)
	      height += 2 * face->box_line_width;
	  }
      }
  
  return height;
}


/***********************************************************************
			    Glyph display
 ***********************************************************************/

/* A sequence of glyphs to be drawn in the same face.

   This data structure is not really completely X specific, so it
   could possibly, at least partially, be useful for other systems.  It
   is currently not part of the external redisplay interface because
   it's not clear what other systems will need.  */

struct glyph_string
{
  /* X-origin of the string.  */
  int x;

  /* Y-origin and y-position of the base line of this string.  */
  int y, ybase;

  /* The width of the string, not including a face extension.  */
  int width;

  /* The width of the string, including a face extension.  */
  int background_width;

  /* The height of this string.  This is the height of the line this
     string is drawn in, and can be different from the height of the
     font the string is drawn in.  */
  int height;

  /* Number of pixels this string overwrites in front of its x-origin.
     This number is zero if the string has an lbearing >= 0; it is
     -lbearing, if the string has an lbearing < 0.  */
  int left_overhang;

  /* Number of pixels this string overwrites past its right-most
     nominal x-position, i.e. x + width.  Zero if the string's
     rbearing is <= its nominal width, rbearing - width otherwise.  */
  int right_overhang;

  /* The frame on which the glyph string is drawn.  */
  struct frame *f;

  /* The window on which the glyph string is drawn.  */
  struct window *w;

  /* X display and window for convenience.  */
  Display *display;
  Window window;

  /* The glyph row for which this string was built.  It determines the
     y-origin and height of the string.  */
  struct glyph_row *row;

  /* The area within row.  */
  enum glyph_row_area area;

  /* Characters to be drawn, and number of characters.  */
  XChar2b *char2b;
  int nchars;

  /* A face-override for drawing cursors, mouse face and similar.  */
  enum draw_glyphs_face hl;

  /* Face in which this string is to be drawn.  */
  struct face *face;

  /* Font in which this string is to be drawn.  */
  XFontStruct *font;

  /* Font info for this string.  */
  struct font_info *font_info;

  /* Non-null means this string describes (part of) a composition.
     All characters from char2b are drawn composed.  */
  struct composition *cmp;

  /* Index of this glyph string's first character in the glyph
     definition of CMP.  If this is zero, this glyph string describes
     the first character of a composition.  */
  int gidx;

  /* 1 means this glyph strings face has to be drawn to the right end
     of the window's drawing area.  */
  unsigned extends_to_end_of_line_p : 1;

  /* 1 means the background of this string has been drawn.  */
  unsigned background_filled_p : 1;

  /* 1 means glyph string must be drawn with 16-bit functions.  */
  unsigned two_byte_p : 1;

  /* 1 means that the original font determined for drawing this glyph
     string could not be loaded.  The member `font' has been set to
     the frame's default font in this case.  */
  unsigned font_not_found_p : 1;

  /* 1 means that the face in which this glyph string is drawn has a
     stipple pattern.  */
  unsigned stippled_p : 1;

  /* 1 means only the foreground of this glyph string must be drawn,
     and we should use the physical height of the line this glyph
     string appears in as clip rect.  */
  unsigned for_overlaps_p : 1;

  /* The GC to use for drawing this glyph string.  */
  GC gc;

  /* A pointer to the first glyph in the string.  This glyph
     corresponds to char2b[0].  Needed to draw rectangles if
     font_not_found_p is 1.  */
  struct glyph *first_glyph;

  /* Image, if any.  */
  struct image *img;

  struct glyph_string *next, *prev;
};


#if GLYPH_DEBUG

static void
x_dump_glyph_string (s)
     struct glyph_string *s;
{
  fprintf (stderr, "glyph string\n");
  fprintf (stderr, "  x, y, w, h = %d, %d, %d, %d\n",
	   s->x, s->y, s->width, s->height);
  fprintf (stderr, "  ybase = %d\n", s->ybase);
  fprintf (stderr, "  hl = %d\n", s->hl);
  fprintf (stderr, "  left overhang = %d, right = %d\n",
	   s->left_overhang, s->right_overhang);
  fprintf (stderr, "  nchars = %d\n", s->nchars);
  fprintf (stderr, "  extends to end of line = %d\n",
	   s->extends_to_end_of_line_p);
  fprintf (stderr, "  font height = %d\n", FONT_HEIGHT (s->font));
  fprintf (stderr, "  bg width = %d\n", s->background_width);
}

#endif /* GLYPH_DEBUG */



static void x_append_glyph_string_lists P_ ((struct glyph_string **,
					     struct glyph_string **,
					     struct glyph_string *,
					     struct glyph_string *));
static void x_prepend_glyph_string_lists P_ ((struct glyph_string **,
					      struct glyph_string **,
					      struct glyph_string *,
					      struct glyph_string *));
static void x_append_glyph_string P_ ((struct glyph_string **,
				       struct glyph_string **,
				       struct glyph_string *));
static int x_left_overwritten P_ ((struct glyph_string *));
static int x_left_overwriting P_ ((struct glyph_string *));
static int x_right_overwritten P_ ((struct glyph_string *));
static int x_right_overwriting P_ ((struct glyph_string *));
static int x_fill_glyph_string P_ ((struct glyph_string *, int, int, int,
				    int));
static void x_init_glyph_string P_ ((struct glyph_string *,
					XChar2b *, struct window *,
					struct glyph_row *,
					enum glyph_row_area, int, 
					enum draw_glyphs_face));
static int x_draw_glyphs P_ ((struct window *, int , struct glyph_row *,
			      enum glyph_row_area, int, int,
			      enum draw_glyphs_face, int));
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
static void x_get_glyph_overhangs P_ ((struct glyph *, struct frame *,
				       int *, int *));
static void x_compute_overhangs_and_x P_ ((struct glyph_string *, int, int));
static int x_alloc_lighter_color P_ ((struct frame *, Display *, Colormap,
				      unsigned long *, double, int));
static void x_setup_relief_color P_ ((struct frame *, struct relief *,
				      double, int, unsigned long));
static void x_setup_relief_colors P_ ((struct glyph_string *));
static void x_draw_image_glyph_string P_ ((struct glyph_string *));
static void x_draw_image_relief P_ ((struct glyph_string *));
static void x_draw_image_foreground P_ ((struct glyph_string *));
static void x_draw_image_foreground_1 P_ ((struct glyph_string *, Pixmap));
static void x_fill_image_glyph_string P_ ((struct glyph_string *));
static void x_clear_glyph_string_rect P_ ((struct glyph_string *, int,
					   int, int, int));
static void x_draw_relief_rect P_ ((struct frame *, int, int, int, int,
				    int, int, int, int, XRectangle *));
static void x_draw_box_rect P_ ((struct glyph_string *, int, int, int, int,
				 int, int, int, XRectangle *));
static void x_fix_overlapping_area P_ ((struct window *, struct glyph_row *,
					enum glyph_row_area));
static int x_fill_stretch_glyph_string P_ ((struct glyph_string *,
					    struct glyph_row *,
					    enum glyph_row_area, int, int));

#if GLYPH_DEBUG
static void x_check_font P_ ((struct frame *, XFontStruct *));
#endif


/* Append the list of glyph strings with head H and tail T to the list
   with head *HEAD and tail *TAIL.  Set *HEAD and *TAIL to the result.  */

static INLINE void
x_append_glyph_string_lists (head, tail, h, t)
     struct glyph_string **head, **tail;
     struct glyph_string *h, *t;
{
  if (h)
    {
      if (*head)
	(*tail)->next = h;
      else
	*head = h;
      h->prev = *tail;
      *tail = t;
    }
}


/* Prepend the list of glyph strings with head H and tail T to the
   list with head *HEAD and tail *TAIL.  Set *HEAD and *TAIL to the
   result.  */

static INLINE void
x_prepend_glyph_string_lists (head, tail, h, t)
     struct glyph_string **head, **tail;
     struct glyph_string *h, *t;
{
  if (h)
    {
      if (*head)
	(*head)->prev = t;
      else
	*tail = t;
      t->next = *head;
      *head = h;
    }
}


/* Append glyph string S to the list with head *HEAD and tail *TAIL.
   Set *HEAD and *TAIL to the resulting list.  */

static INLINE void
x_append_glyph_string (head, tail, s)
     struct glyph_string **head, **tail;
     struct glyph_string *s;
{
  s->next = s->prev = NULL;
  x_append_glyph_string_lists (head, tail, s, s);
}


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
    face_id = FACE_FOR_CHAR (s->f, face, s->first_glyph->u.ch);
  else
    face_id = FACE_FOR_CHAR (s->f, face, 0);
  s->face = FACE_FROM_ID (s->f, face_id);
  PREPARE_FACE_FOR_DISPLAY (s->f, s->face);

  /* If font in this face is same as S->font, use it.  */
  if (s->font == s->face->font)
    s->gc = s->face->gc;
  else
    {
      /* Otherwise construct scratch_cursor_gc with values from FACE
	 but font FONT.  */
      XGCValues xgcv;
      unsigned long mask;
      
      xgcv.background = s->face->background;
      xgcv.foreground = s->face->foreground;
      IF_DEBUG (x_check_font (s->f, s->font));
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


/* Return in *R the clipping rectangle for glyph string S.  */

static void
x_get_glyph_string_clip_rect (s, r)
     struct glyph_string *s;
     XRectangle *r;
{
  if (s->row->full_width_p)
    {
      /* Draw full-width.  X coordinates are relative to S->w->left.  */
      int canon_x = CANON_X_UNIT (s->f);
      
      r->x = WINDOW_LEFT_MARGIN (s->w) * canon_x;
      r->width = XFASTINT (s->w->width) * canon_x;

      if (FRAME_HAS_VERTICAL_SCROLL_BARS (s->f))
	{
	  int width = FRAME_SCROLL_BAR_WIDTH (s->f) * canon_x;
	  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (s->f))
	    r->x -= width;
	}
      
      r->x += FRAME_INTERNAL_BORDER_WIDTH (s->f);
      
      /* Unless displaying a mode or menu bar line, which are always
	 fully visible, clip to the visible part of the row.  */
      if (s->w->pseudo_window_p)
	r->height = s->row->visible_height;
      else
	r->height = s->height;
    }
  else
    {
      /* This is a text line that may be partially visible.  */
      r->x = WINDOW_AREA_TO_FRAME_PIXEL_X (s->w, s->area, 0);
      r->width = window_box_width (s->w, s->area);
      r->height = s->row->visible_height;
    }

  /* If S draws overlapping rows, it's sufficient to use the top and
     bottom of the window for clipping because this glyph string
     intentionally draws over other lines.  */
  if (s->for_overlaps_p)
    {
      r->y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (s->w);
      r->height = window_text_bottom_y (s->w) - r->y;
    }
  else
    {
      /* Don't use S->y for clipping because it doesn't take partially
	 visible lines into account.  For example, it can be negative for
	 partially visible lines at the top of a window.  */
      if (!s->row->full_width_p
	  && MATRIX_ROW_PARTIALLY_VISIBLE_AT_TOP_P (s->w, s->row))
	r->y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (s->w);
      else
	r->y = max (0, s->row->y);

      /* If drawing a tool-bar window, draw it over the internal border
	 at the top of the window.  */
      if (s->w == XWINDOW (s->f->tool_bar_window))
	r->y -= s->f->output_data.x->internal_border_width;
    }

  r->y = WINDOW_TO_FRAME_PIXEL_Y (s->w, r->y);
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static INLINE void
x_set_glyph_string_clipping (s)
     struct glyph_string *s;
{
  XRectangle r;
  x_get_glyph_string_clip_rect (s, &r);
  XSetClipRectangles (s->display, s->gc, 0, 0, &r, 1, Unsorted);
}


/* Compute left and right overhang of glyph string S.  If S is a glyph
   string for a composition, assume overhangs don't exist.  */

static INLINE void
x_compute_glyph_string_overhangs (s)
     struct glyph_string *s;
{
  if (s->cmp == NULL
      && s->first_glyph->type == CHAR_GLYPH)
    {
      XCharStruct cs;
      int direction, font_ascent, font_descent;
      XTextExtents16 (s->font, s->char2b, s->nchars, &direction,
		      &font_ascent, &font_descent, &cs);
      s->right_overhang = cs.rbearing > cs.width ? cs.rbearing - cs.width : 0;
      s->left_overhang = cs.lbearing < 0 ? -cs.lbearing : 0;
    }
}


/* Compute overhangs and x-positions for glyph string S and its
   predecessors, or successors.  X is the starting x-position for S.
   BACKWARD_P non-zero means process predecessors.  */
   
static void
x_compute_overhangs_and_x (s, x, backward_p)
     struct glyph_string *s;
     int x;
     int backward_p;
{
  if (backward_p)
    {
      while (s)
	{
	  x_compute_glyph_string_overhangs (s);
	  x -= s->width;
	  s->x = x;
	  s = s->prev;
	}
    }
  else
    {
      while (s)
	{
	  x_compute_glyph_string_overhangs (s);
	  s->x = x;
	  x += s->width;
	  s = s->next;
	}
    }
}


/* Set *LEFT and *RIGHT to the left and right overhang of GLYPH on
   frame F.  Overhangs of glyphs other than type CHAR_GLYPH are
   assumed to be zero.  */

static void
x_get_glyph_overhangs (glyph, f, left, right)
     struct glyph *glyph;
     struct frame *f;
     int *left, *right;
{
  *left = *right = 0;
  
  if (glyph->type == CHAR_GLYPH)
    {
      XFontStruct *font;
      struct face *face;
      struct font_info *font_info;
      XChar2b char2b;
      XCharStruct *pcm;

      face = x_get_glyph_face_and_encoding (f, glyph, &char2b, NULL);
      font = face->font;
      font_info = FONT_INFO_FROM_ID (f, face->font_info_id);
      if (font
	  && (pcm = x_per_char_metric (font, &char2b)))
	{
	  if (pcm->rbearing > pcm->width)
	    *right = pcm->rbearing - pcm->width;
	  if (pcm->lbearing < 0)
	    *left = -pcm->lbearing;
	}
    }
}


/* Return the index of the first glyph preceding glyph string S that
   is overwritten by S because of S's left overhang.  Value is -1
   if no glyphs are overwritten.  */

static int
x_left_overwritten (s)
     struct glyph_string *s;
{
  int k;
    
  if (s->left_overhang)
    {
      int x = 0, i;
      struct glyph *glyphs = s->row->glyphs[s->area];
      int first = s->first_glyph - glyphs;

      for (i = first - 1; i >= 0 && x > -s->left_overhang; --i)
	x -= glyphs[i].pixel_width;

      k = i + 1;
    }
  else
    k = -1;

  return k;
}


/* Return the index of the first glyph preceding glyph string S that
   is overwriting S because of its right overhang.  Value is -1 if no
   glyph in front of S overwrites S.  */

static int
x_left_overwriting (s)
     struct glyph_string *s;
{
  int i, k, x;
  struct glyph *glyphs = s->row->glyphs[s->area];
  int first = s->first_glyph - glyphs;

  k = -1;
  x = 0;
  for (i = first - 1; i >= 0; --i)
    {
      int left, right;
      x_get_glyph_overhangs (glyphs + i, s->f, &left, &right);
      if (x + right > 0)
	k = i;
      x -= glyphs[i].pixel_width;
    }

  return k;
}


/* Return the index of the last glyph following glyph string S that is
   not overwritten by S because of S's right overhang.  Value is -1 if
   no such glyph is found.  */

static int
x_right_overwritten (s)
     struct glyph_string *s;
{
  int k = -1;

  if (s->right_overhang)
    {
      int x = 0, i;
      struct glyph *glyphs = s->row->glyphs[s->area];
      int first = (s->first_glyph - glyphs) + (s->cmp ? 1 : s->nchars);
      int end = s->row->used[s->area];
      
      for (i = first; i < end && s->right_overhang > x; ++i)
	x += glyphs[i].pixel_width;

      k = i;
    }

  return k;
}


/* Return the index of the last glyph following glyph string S that
   overwrites S because of its left overhang.  Value is negative
   if no such glyph is found.  */

static int
x_right_overwriting (s)
     struct glyph_string *s;
{
  int i, k, x;
  int end = s->row->used[s->area];
  struct glyph *glyphs = s->row->glyphs[s->area];
  int first = (s->first_glyph - glyphs) + (s->cmp ? 1 : s->nchars);

  k = -1;
  x = 0;
  for (i = first; i < end; ++i)
    {
      int left, right;
      x_get_glyph_overhangs (glyphs + i, s->f, &left, &right);
      if (x - left < 0)
	k = i;
      x += glyphs[i].pixel_width;
    }

  return k;
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
    x = s->x + abs (s->face->box_line_width);
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
      if (s->for_overlaps_p
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
    }
}

/* Draw the foreground of composite glyph string S.  */

static void
x_draw_composite_glyph_string_foreground (s)
     struct glyph_string *s;
{
  int i, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
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
  else
    {
      for (i = 0; i < s->nchars; i++, ++s->gidx)
	XDrawString16 (s->display, s->window, s->gc,
		       x + s->cmp->offsets[s->gidx * 2],
		       s->ybase - s->cmp->offsets[s->gidx * 2 + 1],
		       s->char2b + i, 1);
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
  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    if (GC_FRAMEP (XCAR (tail))
	&& (f = XFRAME (XCAR (tail)),
	    (f->output_data.nothing != 1
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
		    raised_p, left_p, right_p, clip_rect)
     struct frame *f;
     int left_x, top_y, right_x, bottom_y, left_p, right_p, raised_p;
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
     int left_x, top_y, right_x, bottom_y, left_p, right_p;
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

  last_x = window_box_right (s->w, s->area);
  if (s->row->full_width_p
      && !s->w->pseudo_window_p)
    {
      last_x += FRAME_X_RIGHT_FRINGE_WIDTH (s->f);
      if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (s->f))
	last_x += FRAME_SCROLL_BAR_WIDTH (s->f) * CANON_X_UNIT (s->f);
    }
  
  /* The glyph that may have a right box line.  */
  last_glyph = (s->cmp || s->img
		? s->first_glyph
		: s->first_glyph + s->nchars - 1);

  width = abs (s->face->box_line_width);
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

  x_get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    x_draw_box_rect (s, left_x, top_y, right_x, bottom_y, width,
		     left_p, right_p, &clip_rect);
  else
    {
      x_setup_relief_colors (s);
      x_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y,
			  width, raised_p, left_p, right_p, &clip_rect);
    }
}


/* Draw foreground of image glyph string S.  */

static void
x_draw_image_foreground (s)
     struct glyph_string *s;
{
  int x;
  int y = s->ybase - image_ascent (s->img, s->face);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  x += s->img->hmargin;
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
	  
	  x_get_glyph_string_clip_rect (s, &clip_rect);
	  image_rect.x = x;
	  image_rect.y = y;
	  image_rect.width = s->img->width;
	  image_rect.height = s->img->height;
	  if (x_intersect_rectangles (&clip_rect, &image_rect, &r))
	    XCopyArea (s->display, s->img->pixmap, s->window, s->gc,
		       r.x - x, r.y - y, r.width, r.height, r.x, r.y);
	}
      else
	{
	  XRectangle clip_rect, image_rect, r;

	  x_get_glyph_string_clip_rect (s, &clip_rect);
	  image_rect.x = x;
	  image_rect.y = y;
	  image_rect.width = s->img->width;
	  image_rect.height = s->img->height;
	  if (x_intersect_rectangles (&clip_rect, &image_rect, &r))
	    XCopyArea (s->display, s->img->pixmap, s->window, s->gc,
		       r.x - x, r.y - y, r.width, r.height, r.x, r.y);
	  
	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    XDrawRectangle (s->display, s->window, s->gc, x, y,
			    s->img->width - 1, s->img->height - 1);
	}
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    XDrawRectangle (s->display, s->window, s->gc, x, y,
		    s->img->width - 1, s->img->height - 1);
}


/* Draw a relief around the image glyph string S.  */

static void
x_draw_image_relief (s)
     struct glyph_string *s;
{
  int x0, y0, x1, y1, thick, raised_p;
  XRectangle r;
  int x;
  int y = s->ybase - image_ascent (s->img, s->face);
  
  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + abs (s->face->box_line_width);
  else
    x = s->x;
  
  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  x += s->img->hmargin;
  y += s->img->vmargin;
  
  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      thick = tool_bar_button_relief >= 0 ? tool_bar_button_relief : DEFAULT_TOOL_BAR_BUTTON_RELIEF;
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = abs (s->img->relief);
      raised_p = s->img->relief > 0;
    }
  
  x0 = x - thick;
  y0 = y - thick;
  x1 = x + s->img->width + thick - 1;
  y1 = y + s->img->height + thick - 1;
  
  x_setup_relief_colors (s);
  x_get_glyph_string_clip_rect (s, &r);
  x_draw_relief_rect (s->f, x0, y0, x1, y1, thick, raised_p, 1, 1, &r);
}


/* Draw the foreground of image glyph string S to PIXMAP.  */

static void
x_draw_image_foreground_1 (s, pixmap)
     struct glyph_string *s;
     Pixmap pixmap;
{
  int x;
  int y = s->ybase - s->y - image_ascent (s->img, s->face);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = abs (s->face->box_line_width);
  else
    x = 0;

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  x += s->img->hmargin;
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
	  xgcv.clip_x_origin = x;
	  xgcv.clip_y_origin = y;
	  xgcv.function = GXcopy;
	  XChangeGC (s->display, s->gc, mask, &xgcv);

	  XCopyArea (s->display, s->img->pixmap, pixmap, s->gc,
		     0, 0, s->img->width, s->img->height, x, y);
	  XSetClipMask (s->display, s->gc, None);
	}
      else
	{
	  XCopyArea (s->display, s->img->pixmap, pixmap, s->gc,
		     0, 0, s->img->width, s->img->height, x, y);
	  
	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    XDrawRectangle (s->display, pixmap, s->gc, x, y,
			    s->img->width - 1, s->img->height - 1);
	}
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    XDrawRectangle (s->display, pixmap, s->gc, x, y,
		    s->img->width - 1, s->img->height - 1);
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
  int x, y;
  int box_line_hwidth = abs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height;
  Pixmap pixmap = None;

  height = s->height - 2 * box_line_vwidth;

  /* Fill background with face under the image.  Do it only if row is
     taller than image or if image has a clip mask to reduce
     flickering.  */
  s->stippled_p = s->face->stipple != 0;
  if (height > s->img->height
      || s->img->hmargin
      || s->img->vmargin
      || s->img->mask
      || s->img->pixmap == 0
      || s->width != s->background_width)
    {
      if (box_line_hwidth && s->first_glyph->left_box_line_p)
	x = s->x + box_line_hwidth;
      else
	x = s->x;
      
      y = s->y + box_line_vwidth;
      
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
	      XFillRectangle (s->display, pixmap, s->gc,
			      0, 0, s->background_width, s->height);
	      XSetFillStyle (s->display, s->gc, FillSolid);
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
	x_draw_glyph_string_bg_rect (s, x, y, s->background_width, height);
      
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
  s->stippled_p = s->face->stipple != 0;

  if (s->hl == DRAW_CURSOR
      && !x_stretch_cursor_p)
    {
      /* If `x-stretch-block-cursor' is nil, don't draw a block cursor
	 as wide as the stretch glyph.  */
      int width = min (CANON_X_UNIT (s->f), s->background_width);

      /* Draw cursor.  */
      x_draw_glyph_string_bg_rect (s, s->x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < s->background_width)
	{
	  int x = s->x + width, y = s->y;
	  int w = s->background_width - width, h = s->height;
	  XRectangle r;
	  GC gc;

	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      x_set_mouse_face_gc (s);
	      gc = s->gc;
	    }
	  else
	    gc = s->face->gc;
  
	  x_get_glyph_string_clip_rect (s, &r);
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
    x_draw_glyph_string_bg_rect (s, s->x, s->y, s->background_width,
				 s->height);
  
  s->background_filled_p = 1;
}


/* Draw glyph string S.  */

static void
x_draw_glyph_string (s)
     struct glyph_string *s;
{
  int relief_drawn_p = 0;

  /* If S draws into the background of its successor, draw the
     background of the successor first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps_p)
    {
      xassert (s->next->img == NULL);
      x_set_glyph_string_gc (s->next);
      x_set_glyph_string_clipping (s->next);
      x_draw_glyph_string_background (s->next, 1);
    }

  /* Set up S->gc, set clipping and draw S.  */
  x_set_glyph_string_gc (s);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps_p
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
      if (s->for_overlaps_p)
	s->background_filled_p = 1;
      else
	x_draw_glyph_string_background (s, 0);
      x_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      if (s->for_overlaps_p || s->gidx > 0)
	s->background_filled_p = 1;
      else
	x_draw_glyph_string_background (s, 1);
      x_draw_composite_glyph_string_foreground (s);
      break;

    default:
      abort ();
    }

  if (!s->for_overlaps_p)
    {
      /* Draw underline.  */
      if (s->face->underline_p)
	{
	  unsigned long tem, h;
	  int y;

	  /* Get the underline thickness.  Default is 1 pixel.  */
	  if (!XGetFontProperty (s->font, XA_UNDERLINE_THICKNESS, &h))
	    h = 1;

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
	  else
	    y = s->y + s->height - h;
      
	  if (s->face->underline_defaulted_p)
	    XFillRectangle (s->display, s->window, s->gc,
			    s->x, y, s->width, h);
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
	      XSetForeground (s->display, s->gc, s->face->underline_color);
	      XFillRectangle (s->display, s->window, s->gc,
			      s->x, y, s->width, h);
	      XSetForeground (s->display, s->gc, xgcv.foreground);
	    }
	}

      /* Draw overline.  */
      if (s->face->overline_p)
	{
	  unsigned long dy = 0, h = 1;

	  if (s->face->overline_color_defaulted_p)
	    XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			    s->width, h);
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (s->display, s->gc, GCForeground, &xgcv);
	      XSetForeground (s->display, s->gc, s->face->overline_color);
	      XFillRectangle (s->display, s->window, s->gc, s->x, s->y + dy,
			      s->width, h);
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
    }
  
  /* Reset clipping.  */
  XSetClipMask (s->display, s->gc, None);
}


static int x_fill_composite_glyph_string P_ ((struct glyph_string *,
					      struct face **, int));


/* Fill glyph string S with composition components specified by S->cmp.
   
   FACES is an array of faces for all components of this composition.
   S->gidx is the index of the first component for S.
   OVERLAPS_P non-zero means S should draw the foreground only, and
   use its physical height for clipping.

   Value is the index of a component not in S.  */

static int
x_fill_composite_glyph_string (s, faces, overlaps_p)
     struct glyph_string *s;
     struct face **faces;
     int overlaps_p;
{
  int i;

  xassert (s);

  s->for_overlaps_p = overlaps_p;
  
  s->face = faces[s->gidx];
  s->font = s->face->font;
  s->font_info = FONT_INFO_FROM_ID (s->f, s->face->font_info_id);

  /* For all glyphs of this composition, starting at the offset
     S->gidx, until we reach the end of the definition or encounter a
     glyph that requires the different face, add it to S.  */
  ++s->nchars;
  for (i = s->gidx + 1; i < s->cmp->glyph_len && faces[i] == s->face; ++i)
    ++s->nchars;

  /* All glyph strings for the same composition has the same width,
     i.e. the width set for the first component of the composition.  */

  s->width = s->first_glyph->pixel_width;

  /* If the specified font could not be loaded, use the frame's
     default font, but record the fact that we couldn't load it in
     the glyph string so that we can draw rectangles for the
     characters of the glyph string.  */
  if (s->font == NULL)
    {
      s->font_not_found_p = 1;
      s->font = FRAME_FONT (s->f);
    }

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += s->first_glyph->voffset;
  
  xassert (s->face && s->face->gc);

  /* This glyph string must always be drawn with 16-bit functions.  */
  s->two_byte_p = 1;

  return s->gidx + s->nchars;
}


/* Fill glyph string S from a sequence of character glyphs.
   
   FACE_ID is the face id of the string.  START is the index of the
   first glyph to consider, END is the index of the last + 1.
   OVERLAPS_P non-zero means S should draw the foreground only, and
   use its physical height for clipping.

   Value is the index of the first glyph not in S.  */

static int
x_fill_glyph_string (s, face_id, start, end, overlaps_p)
     struct glyph_string *s;
     int face_id;
     int start, end, overlaps_p;
{
  struct glyph *glyph, *last;
  int voffset;
  int glyph_not_available_p;
  
  xassert (s->f == XFRAME (s->w->frame));
  xassert (s->nchars == 0);
  xassert (start >= 0 && end > start);

  s->for_overlaps_p = overlaps_p,
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  voffset = glyph->voffset;
  
  glyph_not_available_p = glyph->glyph_not_available_p;

  while (glyph < last
	 && glyph->type == CHAR_GLYPH
	 && glyph->voffset == voffset
	 /* Same face id implies same font, nowadays.  */
	 && glyph->face_id == face_id
	 && glyph->glyph_not_available_p == glyph_not_available_p)
    {
      int two_byte_p;

      s->face = x_get_glyph_face_and_encoding (s->f, glyph,
					       s->char2b + s->nchars,
					       &two_byte_p);
      s->two_byte_p = two_byte_p;
      ++s->nchars;
      xassert (s->nchars <= end - start);
      s->width += glyph->pixel_width;
      ++glyph;
    }

  s->font = s->face->font;
  s->font_info = FONT_INFO_FROM_ID (s->f, s->face->font_info_id);
  
  /* If the specified font could not be loaded, use the frame's font,
     but record the fact that we couldn't load it in
     S->font_not_found_p so that we can draw rectangles for the
     characters of the glyph string.  */
  if (s->font == NULL || glyph_not_available_p)
    {
      s->font_not_found_p = 1;
      s->font = FRAME_FONT (s->f);
    }

  /* Adjust base line for subscript/superscript text.  */
  s->ybase += voffset;

  xassert (s->face && s->face->gc);
  return glyph - s->row->glyphs[s->area];
}


/* Fill glyph string S from image glyph S->first_glyph.  */

static void
x_fill_image_glyph_string (s)
     struct glyph_string *s;
{
  xassert (s->first_glyph->type == IMAGE_GLYPH);
  s->img = IMAGE_FROM_ID (s->f, s->first_glyph->u.img_id);
  xassert (s->img);
  s->face = FACE_FROM_ID (s->f, s->first_glyph->face_id);
  s->font = s->face->font;
  s->width = s->first_glyph->pixel_width;
  
  /* Adjust base line for subscript/superscript text.  */
  s->ybase += s->first_glyph->voffset;
}


/* Fill glyph string S from a sequence of stretch glyphs.

   ROW is the glyph row in which the glyphs are found, AREA is the
   area within the row.  START is the index of the first glyph to
   consider, END is the index of the last + 1.

   Value is the index of the first glyph not in S.  */

static int
x_fill_stretch_glyph_string (s, row, area, start, end)
     struct glyph_string *s;
     struct glyph_row *row;
     enum glyph_row_area area;
     int start, end;
{
  struct glyph *glyph, *last;
  int voffset, face_id;
  
  xassert (s->first_glyph->type == STRETCH_GLYPH);
  
  glyph = s->row->glyphs[s->area] + start;
  last = s->row->glyphs[s->area] + end;
  face_id = glyph->face_id;
  s->face = FACE_FROM_ID (s->f, face_id);
  s->font = s->face->font;
  s->font_info = FONT_INFO_FROM_ID (s->f, s->face->font_info_id);
  s->width = glyph->pixel_width;
  voffset = glyph->voffset;

  for (++glyph;
       (glyph < last
	&& glyph->type == STRETCH_GLYPH
	&& glyph->voffset == voffset
	&& glyph->face_id == face_id);
       ++glyph)
    s->width += glyph->pixel_width;
  
  /* Adjust base line for subscript/superscript text.  */
  s->ybase += voffset;

  /* The case that face->gc == 0 is handled when drawing the glyph
     string by calling PREPARE_FACE_FOR_DISPLAY.  */
  xassert (s->face);
  return glyph - s->row->glyphs[s->area];
}


/* Initialize glyph string S.  CHAR2B is a suitably allocated vector
   of XChar2b structures for S; it can't be allocated in
   x_init_glyph_string because it must be allocated via `alloca'.  W
   is the window on which S is drawn.  ROW and AREA are the glyph row
   and area within the row from which S is constructed.  START is the
   index of the first glyph structure covered by S.  HL is a
   face-override for drawing S.  */
   
static void
x_init_glyph_string (s, char2b, w, row, area, start, hl)
     struct glyph_string *s;
     XChar2b *char2b;
     struct window *w;
     struct glyph_row *row;
     enum glyph_row_area area;
     int start;
     enum draw_glyphs_face hl;
{
  bzero (s, sizeof *s);
  s->w = w;
  s->f = XFRAME (w->frame);
  s->display = FRAME_X_DISPLAY (s->f);
  s->window = FRAME_X_WINDOW (s->f);
  s->char2b = char2b;
  s->hl = hl;
  s->row = row;
  s->area = area;
  s->first_glyph = row->glyphs[area] + start;
  s->height = row->height;
  s->y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);

  /* Display the internal border below the tool-bar window.  */
  if (s->w == XWINDOW (s->f->tool_bar_window))
    s->y -= s->f->output_data.x->internal_border_width;
  
  s->ybase = s->y + row->ascent;
}


/* Set background width of glyph string S.  START is the index of the
   first glyph following S.  LAST_X is the right-most x-position + 1
   in the drawing area.  */

static INLINE void
x_set_glyph_string_background_width (s, start, last_x)
     struct glyph_string *s;
     int start;
     int last_x;
{
  /* If the face of this glyph string has to be drawn to the end of
     the drawing area, set S->extends_to_end_of_line_p.  */
  struct face *default_face = FACE_FROM_ID (s->f, DEFAULT_FACE_ID);
  
  if (start == s->row->used[s->area]
      && s->area == TEXT_AREA
      && ((s->hl == DRAW_NORMAL_TEXT
	   && (s->row->fill_line_p
	       || s->face->background != default_face->background
	       || s->face->stipple != default_face->stipple
	       || s->row->mouse_face_p))
	  || s->hl == DRAW_MOUSE_FACE
	  || ((s->hl == DRAW_IMAGE_RAISED || s->hl == DRAW_IMAGE_SUNKEN)
	      && s->row->fill_line_p)))
      s->extends_to_end_of_line_p = 1;
  
  /* If S extends its face to the end of the line, set its
     background_width to the distance to the right edge of the drawing
     area.  */
  if (s->extends_to_end_of_line_p)
    s->background_width = last_x - s->x + 1;
  else
    s->background_width = s->width;
}


/* Add a glyph string for a stretch glyph to the list of strings
   between HEAD and TAIL.  START is the index of the stretch glyph in
   row area AREA of glyph row ROW.  END is the index of the last glyph
   in that glyph row area.  X is the current output position assigned
   to the new glyph string constructed.  HL overrides that face of the
   glyph; e.g. it is DRAW_CURSOR if a cursor has to be drawn.  LAST_X
   is the right-most x-position of the drawing area.  */

/* SunOS 4 bundled cc, barfed on continuations in the arg lists here
   and below -- keep them on one line.  */
#define BUILD_STRETCH_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X) \
     do									    \
       {								    \
	 s = (struct glyph_string *) alloca (sizeof *s);		    \
	 x_init_glyph_string (s, NULL, W, ROW, AREA, START, HL);	    \
	 START = x_fill_stretch_glyph_string (s, ROW, AREA, START, END);    \
	 x_append_glyph_string (&HEAD, &TAIL, s);			    \
         s->x = (X);							    \
       }								    \
     while (0)


/* Add a glyph string for an image glyph to the list of strings
   between HEAD and TAIL.  START is the index of the image glyph in
   row area AREA of glyph row ROW.  END is the index of the last glyph
   in that glyph row area.  X is the current output position assigned
   to the new glyph string constructed.  HL overrides that face of the
   glyph; e.g. it is DRAW_CURSOR if a cursor has to be drawn.  LAST_X
   is the right-most x-position of the drawing area.  */

#define BUILD_IMAGE_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X) \
     do									\
       {								\
	 s = (struct glyph_string *) alloca (sizeof *s);		\
	 x_init_glyph_string (s, NULL, W, ROW, AREA, START, HL);        \
	 x_fill_image_glyph_string (s);					\
	 x_append_glyph_string (&HEAD, &TAIL, s);			\
	 ++START;							\
         s->x = (X);							\
       }								\
     while (0)


/* Add a glyph string for a sequence of character glyphs to the list
   of strings between HEAD and TAIL.  START is the index of the first
   glyph in row area AREA of glyph row ROW that is part of the new
   glyph string.  END is the index of the last glyph in that glyph row
   area.  X is the current output position assigned to the new glyph
   string constructed.  HL overrides that face of the glyph; e.g. it
   is DRAW_CURSOR if a cursor has to be drawn.  LAST_X is the
   right-most x-position of the drawing area.  */

#define BUILD_CHAR_GLYPH_STRINGS(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P) \
     do									   \
       {								   \
	 int c, face_id;						   \
	 XChar2b *char2b;						   \
									   \
	 c = (ROW)->glyphs[AREA][START].u.ch;				   \
	 face_id = (ROW)->glyphs[AREA][START].face_id;			   \
									   \
	 s = (struct glyph_string *) alloca (sizeof *s);		   \
	 char2b = (XChar2b *) alloca ((END - START) * sizeof *char2b);	   \
	 x_init_glyph_string (s, char2b, W, ROW, AREA, START, HL);	   \
	 x_append_glyph_string (&HEAD, &TAIL, s);			   \
	 s->x = (X);							   \
	 START = x_fill_glyph_string (s, face_id, START, END,		   \
                                          OVERLAPS_P);			   \
       }								   \
     while (0)
     

/* Add a glyph string for a composite sequence to the list of strings
   between HEAD and TAIL.  START is the index of the first glyph in
   row area AREA of glyph row ROW that is part of the new glyph
   string.  END is the index of the last glyph in that glyph row area.
   X is the current output position assigned to the new glyph string
   constructed.  HL overrides that face of the glyph; e.g. it is
   DRAW_CURSOR if a cursor has to be drawn.  LAST_X is the right-most
   x-position of the drawing area.  */

#define BUILD_COMPOSITE_GLYPH_STRING(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P)	  \
  do {									  \
    int cmp_id = (ROW)->glyphs[AREA][START].u.cmp_id;			  \
    int face_id = (ROW)->glyphs[AREA][START].face_id;			  \
    struct face *base_face = FACE_FROM_ID (XFRAME (w->frame), face_id);	  \
    struct composition *cmp = composition_table[cmp_id];		  \
    int glyph_len = cmp->glyph_len;					  \
    XChar2b *char2b;							  \
    struct face **faces;						  \
    struct glyph_string *first_s = NULL;				  \
    int n;								  \
    									  \
    base_face = base_face->ascii_face;					  \
    char2b = (XChar2b *) alloca ((sizeof *char2b) * glyph_len);		  \
    faces = (struct face **) alloca ((sizeof *faces) * glyph_len);	  \
    /* At first, fill in `char2b' and `faces'.  */			  \
    for (n = 0; n < glyph_len; n++)					  \
      {									  \
	int c = COMPOSITION_GLYPH (cmp, n);				  \
	int this_face_id = FACE_FOR_CHAR (XFRAME (w->frame), base_face, c); \
	faces[n] = FACE_FROM_ID (XFRAME (w->frame), this_face_id);	  \
	x_get_char_face_and_encoding (XFRAME (w->frame), c,		  \
				      this_face_id, char2b + n, 1);	  \
      }									  \
    									  \
    /* Make glyph_strings for each glyph sequence that is drawable by	  \
       the same face, and append them to HEAD/TAIL.  */			  \
    for (n = 0; n < cmp->glyph_len;)					  \
      {									  \
	s = (struct glyph_string *) alloca (sizeof *s);			  \
	x_init_glyph_string (s, char2b + n, W, ROW, AREA, START, HL);	  \
	x_append_glyph_string (&(HEAD), &(TAIL), s);			  \
	s->cmp = cmp;							  \
	s->gidx = n;							  \
	s->x = (X);							  \
									  \
	if (n == 0)							  \
	  first_s = s;							  \
									  \
	n = x_fill_composite_glyph_string (s, faces, OVERLAPS_P);	  \
      }									  \
    									  \
    ++START;								  \
    s = first_s;							  \
  } while (0)
    

/* Build a list of glyph strings between HEAD and TAIL for the glyphs
   of AREA of glyph row ROW on window W between indices START and END.
   HL overrides the face for drawing glyph strings, e.g. it is
   DRAW_CURSOR to draw a cursor.  X and LAST_X are start and end
   x-positions of the drawing area.

   This is an ugly monster macro construct because we must use alloca
   to allocate glyph strings (because x_draw_glyphs can be called
   asynchronously).  */

#define BUILD_GLYPH_STRINGS(W, ROW, AREA, START, END, HEAD, TAIL, HL, X, LAST_X, OVERLAPS_P) \
     do									   \
       {								   \
	 HEAD = TAIL = NULL;						   \
	 while (START < END)						   \
	   {								   \
             struct glyph *first_glyph = (ROW)->glyphs[AREA] + START;	   \
             switch (first_glyph->type)					   \
	       {							   \
	       case CHAR_GLYPH:						   \
                 BUILD_CHAR_GLYPH_STRINGS (W, ROW, AREA, START, END, HEAD, \
		                           TAIL, HL, X, LAST_X,		   \
                                           OVERLAPS_P);	                   \
		 break;							   \
									   \
	       case COMPOSITE_GLYPH:					   \
                 BUILD_COMPOSITE_GLYPH_STRING (W, ROW, AREA, START, END,   \
						 HEAD, TAIL, HL, X, LAST_X,\
						 OVERLAPS_P);		   \
		 break;							   \
									   \
	       case STRETCH_GLYPH:					   \
		 BUILD_STRETCH_GLYPH_STRING (W, ROW, AREA, START, END,	   \
					     HEAD, TAIL, HL, X, LAST_X);   \
		 break;							   \
									   \
	       case IMAGE_GLYPH:					   \
		 BUILD_IMAGE_GLYPH_STRING (W, ROW, AREA, START, END, HEAD, \
					   TAIL, HL, X, LAST_X);	   \
		 break;							   \
									   \
	       default:							   \
		 abort ();						   \
	       }							   \
									   \
             x_set_glyph_string_background_width (s, START, LAST_X);	   \
	     (X) += s->width;						   \
            }								   \
       }								   \
     while (0)


/* Draw glyphs between START and END in AREA of ROW on window W,
   starting at x-position X.  X is relative to AREA in W.  HL is a
   face-override with the following meaning:

   DRAW_NORMAL_TEXT	draw normally
   DRAW_CURSOR		draw in cursor face
   DRAW_MOUSE_FACE	draw in mouse face.
   DRAW_INVERSE_VIDEO	draw in mode line face
   DRAW_IMAGE_SUNKEN	draw an image with a sunken relief around it
   DRAW_IMAGE_RAISED	draw an image with a raised relief around it

   If OVERLAPS_P is non-zero, draw only the foreground of characters
   and clip to the physical height of ROW.

   Value is the x-position reached, relative to AREA of W.  */
     
static int
x_draw_glyphs (w, x, row, area, start, end, hl, overlaps_p)
     struct window *w;
     int x;
     struct glyph_row *row;
     enum glyph_row_area area;
     int start, end;
     enum draw_glyphs_face hl;
     int overlaps_p;
{
  struct glyph_string *head, *tail;
  struct glyph_string *s;
  int last_x, area_width;
  int x_reached;
  int i, j;

  /* Let's rather be paranoid than getting a SEGV.  */
  end = min (end, row->used[area]);
  start = max (0, start);
  start = min (end, start);

  /* Translate X to frame coordinates.  Set last_x to the right
     end of the drawing area.  */
  if (row->full_width_p)
    {
      /* X is relative to the left edge of W, without scroll bars
	 or fringes.  */
      struct frame *f = XFRAME (w->frame);
      int window_left_x = WINDOW_LEFT_MARGIN (w) * CANON_X_UNIT (f);

      x += window_left_x;
      area_width = XFASTINT (w->width) * CANON_X_UNIT (f);
      last_x = window_left_x + area_width;

      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
	{
	  int width = FRAME_SCROLL_BAR_WIDTH (f) * CANON_X_UNIT (f);
	  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
	    last_x += width;
	  else
	    x -= width;
	}

      x += FRAME_INTERNAL_BORDER_WIDTH (f);
      last_x += FRAME_INTERNAL_BORDER_WIDTH (f);
    }
  else
    {
      x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, area, x);
      area_width = window_box_width (w, area);
      last_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, area, area_width);
    }

  /* Build a doubly-linked list of glyph_string structures between
     head and tail from what we have to draw.  Note that the macro
     BUILD_GLYPH_STRINGS will modify its start parameter.  That's
     the reason we use a separate variable `i'.  */
  i = start;
  BUILD_GLYPH_STRINGS (w, row, area, i, end, head, tail, hl, x, last_x,
		       overlaps_p);
  if (tail)
    x_reached = tail->x + tail->background_width;
  else
    x_reached = x;

  /* If there are any glyphs with lbearing < 0 or rbearing > width in
     the row, redraw some glyphs in front or following the glyph
     strings built above.  */
  if (head && !overlaps_p && row->contains_overlapping_glyphs_p)
    {
      int dummy_x = 0;
      struct glyph_string *h, *t;

      /* Compute overhangs for all glyph strings.  */
      for (s = head; s; s = s->next)
	x_compute_glyph_string_overhangs (s);

      /* Prepend glyph strings for glyphs in front of the first glyph
	 string that are overwritten because of the first glyph
	 string's left overhang.  The background of all strings
	 prepended must be drawn because the first glyph string 
	 draws over it.  */
      i = x_left_overwritten (head);
      if (i >= 0)
	{
	  j = i;
	  BUILD_GLYPH_STRINGS (w, row, area, j, start, h, t,
			       DRAW_NORMAL_TEXT, dummy_x, last_x,
			       overlaps_p);
	  start = i;
	  x_compute_overhangs_and_x (t, head->x, 1);
	  x_prepend_glyph_string_lists (&head, &tail, h, t);
	}

      /* Prepend glyph strings for glyphs in front of the first glyph
	 string that overwrite that glyph string because of their
	 right overhang.  For these strings, only the foreground must
	 be drawn, because it draws over the glyph string at `head'.
	 The background must not be drawn because this would overwrite
	 right overhangs of preceding glyphs for which no glyph
	 strings exist.  */
      i = x_left_overwriting (head);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (w, row, area, i, start, h, t,
			       DRAW_NORMAL_TEXT, dummy_x, last_x,
			       overlaps_p);
	  for (s = h; s; s = s->next)
	    s->background_filled_p = 1;
	  x_compute_overhangs_and_x (t, head->x, 1);
	  x_prepend_glyph_string_lists (&head, &tail, h, t);
	}

      /* Append glyphs strings for glyphs following the last glyph
	 string tail that are overwritten by tail.  The background of
	 these strings has to be drawn because tail's foreground draws
	 over it.  */
      i = x_right_overwritten (tail);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (w, row, area, end, i, h, t,
			       DRAW_NORMAL_TEXT, x, last_x,
			       overlaps_p);
	  x_compute_overhangs_and_x (h, tail->x + tail->width, 0);
	  x_append_glyph_string_lists (&head, &tail, h, t);
	}

      /* Append glyph strings for glyphs following the last glyph
	 string tail that overwrite tail.  The foreground of such
	 glyphs has to be drawn because it writes into the background
	 of tail.  The background must not be drawn because it could
	 paint over the foreground of following glyphs.  */
      i = x_right_overwriting (tail);
      if (i >= 0)
	{
	  BUILD_GLYPH_STRINGS (w, row, area, end, i, h, t,
			       DRAW_NORMAL_TEXT, x, last_x,
			       overlaps_p);
	  for (s = h; s; s = s->next)
	    s->background_filled_p = 1;
	  x_compute_overhangs_and_x (h, tail->x + tail->width, 0);
	  x_append_glyph_string_lists (&head, &tail, h, t);
	}
    }

  /* Draw all strings.  */
  for (s = head; s; s = s->next)
    x_draw_glyph_string (s);

  if (area == TEXT_AREA
      && !row->full_width_p
      /* When drawing overlapping rows, only the glyph strings'
	 foreground is drawn, which doesn't erase a cursor
	 completely. */
      && !overlaps_p)
    {
      int x0 = head ? head->x : x;
      int x1 = tail ? tail->x + tail->background_width : x;
      
      x0 = FRAME_TO_WINDOW_PIXEL_X (w, x0);
      x1 = FRAME_TO_WINDOW_PIXEL_X (w, x1);
      
      if (XFASTINT (w->left_margin_width) != 0)
	{
	  int left_area_width = window_box_width (w, LEFT_MARGIN_AREA);
	  x0 -= left_area_width;
	  x1 -= left_area_width;
	}

      notice_overwritten_cursor (w, area, x0, x1,
				 row->y, MATRIX_ROW_BOTTOM_Y (row));
    }

  /* Value is the x-position up to which drawn, relative to AREA of W.
     This doesn't include parts drawn because of overhangs.  */
  x_reached = FRAME_TO_WINDOW_PIXEL_X (w, x_reached);
  if (!row->full_width_p)
    {
      if (area > LEFT_MARGIN_AREA && XFASTINT (w->left_margin_width) != 0)
	x_reached -= window_box_width (w, LEFT_MARGIN_AREA);
      if (area > TEXT_AREA)
	x_reached -= window_box_width (w, TEXT_AREA);
    }
  
  return x_reached;
}


/* Fix the display of area AREA of overlapping row ROW in window W.  */

static void
x_fix_overlapping_area (w, row, area)
     struct window *w;
     struct glyph_row *row;
     enum glyph_row_area area;
{
  int i, x;
  
  BLOCK_INPUT;
  
  if (area == LEFT_MARGIN_AREA)
    x = 0;
  else if (area == TEXT_AREA)
    x = row->x + window_box_width (w, LEFT_MARGIN_AREA);
  else
    x = (window_box_width (w, LEFT_MARGIN_AREA)
	 + window_box_width (w, TEXT_AREA));

  for (i = 0; i < row->used[area];)
    {
      if (row->glyphs[area][i].overlaps_vertically_p)
	{
	  int start = i, start_x = x;

	  do
	    {
	      x += row->glyphs[area][i].pixel_width;
	      ++i;
	    }
	  while (i < row->used[area]
		 && row->glyphs[area][i].overlaps_vertically_p);

	  x_draw_glyphs (w, start_x, row, area, start, i,
			 DRAW_NORMAL_TEXT, 1);
	}
      else
	{
	  x += row->glyphs[area][i].pixel_width;
	  ++i;
	}
    }
  
  UNBLOCK_INPUT;
}


/* Output LEN glyphs starting at START at the nominal cursor position.
   Advance the nominal cursor over the text.  The global variable
   updated_window contains the window being updated, updated_row is
   the glyph row being updated, and updated_area is the area of that
   row being updated.  */

static void
x_write_glyphs (start, len)
     struct glyph *start;
     int len;
{
  int x, hpos;

  xassert (updated_window && updated_row);
  BLOCK_INPUT;
  
  /* Write glyphs.  */

  hpos = start - updated_row->glyphs[updated_area];
  x = x_draw_glyphs (updated_window, output_cursor.x,
		     updated_row, updated_area,
		     hpos, hpos + len,
		     DRAW_NORMAL_TEXT, 0);

  UNBLOCK_INPUT;
  
  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x = x;
}


/* Insert LEN glyphs from START at the nominal cursor position.   */

static void
x_insert_glyphs (start, len)
     struct glyph *start;
     register int len;
{
  struct frame *f;
  struct window *w;
  int line_height, shift_by_width, shifted_region_width;
  struct glyph_row *row;
  struct glyph *glyph;
  int frame_x, frame_y, hpos;

  xassert (updated_window && updated_row);
  BLOCK_INPUT;
  w = updated_window;
  f = XFRAME (WINDOW_FRAME (w));

  /* Get the height of the line we are in.  */
  row = updated_row;
  line_height = row->height;

  /* Get the width of the glyphs to insert.  */
  shift_by_width = 0;
  for (glyph = start; glyph < start + len; ++glyph)
    shift_by_width += glyph->pixel_width;

  /* Get the width of the region to shift right.  */
  shifted_region_width = (window_box_width (w, updated_area)
			  - output_cursor.x
			  - shift_by_width);

  /* Shift right.  */
  frame_x = window_box_left (w, updated_area) + output_cursor.x;
  frame_y = WINDOW_TO_FRAME_PIXEL_Y (w, output_cursor.y);
  XCopyArea (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), FRAME_X_WINDOW (f),
	     f->output_data.x->normal_gc,
	     frame_x, frame_y,
	     shifted_region_width, line_height,
	     frame_x + shift_by_width, frame_y);

  /* Write the glyphs.  */
  hpos = start - row->glyphs[updated_area];
  x_draw_glyphs (w, output_cursor.x, row, updated_area, hpos, hpos + len,
		 DRAW_NORMAL_TEXT, 0);
  
  /* Advance the output cursor.  */
  output_cursor.hpos += len;
  output_cursor.x += shift_by_width;
  UNBLOCK_INPUT;
}


/* Delete N glyphs at the nominal cursor position.  Not implemented
   for X frames.  */

static void
x_delete_glyphs (n)
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


/* Erase the current text line from the nominal cursor position
   (inclusive) to pixel column TO_X (exclusive).  The idea is that
   everything from TO_X onward is already erased.

   TO_X is a pixel position relative to updated_area of
   updated_window.  TO_X == -1 means clear to the end of this area.  */

static void
x_clear_end_of_line (to_x)
     int to_x;
{
  struct frame *f;
  struct window *w = updated_window;
  int max_x, min_y, max_y;
  int from_x, from_y, to_y;
  
  xassert (updated_window && updated_row);
  f = XFRAME (w->frame);
  
  if (updated_row->full_width_p)
    {
      max_x = XFASTINT (w->width) * CANON_X_UNIT (f);
      if (FRAME_HAS_VERTICAL_SCROLL_BARS (f)
	  && !w->pseudo_window_p)
	max_x += FRAME_SCROLL_BAR_WIDTH (f) * CANON_X_UNIT (f);
    }
  else
    max_x = window_box_width (w, updated_area);
  max_y = window_text_bottom_y (w);

  /* TO_X == 0 means don't do anything.  TO_X < 0 means clear to end
     of window.  For TO_X > 0, truncate to end of drawing area.  */
  if (to_x == 0)
    return;
  else if (to_x < 0)
    to_x = max_x;
  else
    to_x = min (to_x, max_x);

  to_y = min (max_y, output_cursor.y + updated_row->height);
  
  /* Notice if the cursor will be cleared by this operation.  */
  if (!updated_row->full_width_p)
    notice_overwritten_cursor (w, updated_area,
			       output_cursor.x, -1,
			       updated_row->y,
			       MATRIX_ROW_BOTTOM_Y (updated_row));

  from_x = output_cursor.x;
     
  /* Translate to frame coordinates.  */
  if (updated_row->full_width_p)
    {
      from_x = WINDOW_TO_FRAME_PIXEL_X (w, from_x);
      to_x = WINDOW_TO_FRAME_PIXEL_X (w, to_x);
    }
  else
    {
      from_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, updated_area, from_x);
      to_x = WINDOW_AREA_TO_FRAME_PIXEL_X (w, updated_area, to_x);
    }
  
  min_y = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);
  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, max (min_y, output_cursor.y));
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, to_y);
  
  /* Prevent inadvertently clearing to end of the X window.  */
  if (to_x > from_x && to_y > from_y)
    {
      BLOCK_INPUT;
      x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    from_x, from_y, to_x - from_x, to_y - from_y,
		    False);
      UNBLOCK_INPUT;
    }
}


/* Clear entire frame.  If updating_frame is non-null, clear that
   frame.  Otherwise clear the selected frame.  */

static void
x_clear_frame ()
{
  struct frame *f;

  if (updating_frame)
    f = updating_frame;
  else
    f = SELECTED_FRAME ();

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
      values.foreground = (f->output_data.x->foreground_pixel
			   ^ f->output_data.x->background_pixel);

      gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		      GCFunction | GCForeground, &values);
    }

    {
      /* Get the height not including a menu bar widget.  */
      int height = CHAR_TO_PIXEL_HEIGHT (f, FRAME_HEIGHT (f));
      /* Height of each line to flash.  */
      int flash_height = FRAME_LINE_HEIGHT (f);
      /* These will be the left and right margins of the rectangles.  */
      int flash_left = FRAME_INTERNAL_BORDER_WIDTH (f);
      int flash_right = PIXEL_WIDTH (f) - FRAME_INTERNAL_BORDER_WIDTH (f);

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
			   + FRAME_TOOL_BAR_LINES (f) * CANON_Y_UNIT (f)),
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
			   + FRAME_TOOL_BAR_LINES (f) * CANON_Y_UNIT (f)),
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
x_ins_del_lines (vpos, n)
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
  width += FRAME_X_FRINGE_WIDTH (f);
  x -= FRAME_X_LEFT_FRINGE_WIDTH (f);

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
									
/* Redisplay an exposed area of frame F.  X and Y are the upper-left
   corner of the exposed rectangle.  W and H are width and height of
   the exposed area.  All are pixel values.  W or H zero means redraw
   the entire frame.  */

static void
expose_frame (f, x, y, w, h)
     struct frame *f;
     int x, y, w, h;
{
  XRectangle r;
  int mouse_face_overwritten_p = 0;

  TRACE ((stderr, "expose_frame "));

  /* No need to redraw if frame will be redrawn soon.  */
  if (FRAME_GARBAGED_P (f))
    {
      TRACE ((stderr, " garbaged\n"));
      return;
    }

  /* If basic faces haven't been realized yet, there is no point in
     trying to redraw anything.  This can happen when we get an expose
     event while Emacs is starting, e.g. by moving another window.  */
  if (FRAME_FACE_CACHE (f) == NULL
      || FRAME_FACE_CACHE (f)->used < BASIC_FACE_ID_SENTINEL)
    {
      TRACE ((stderr, " no faces\n"));
      return;
    }

  if (w == 0 || h == 0)
    {
      r.x = r.y = 0;
      r.width = CANON_X_UNIT (f) * f->width;
      r.height = CANON_Y_UNIT (f) * f->height;
    }
  else
    {
      r.x = x;
      r.y = y;
      r.width = w;
      r.height = h;
    }

  TRACE ((stderr, "(%d, %d, %d, %d)\n", r.x, r.y, r.width, r.height));
  mouse_face_overwritten_p = expose_window_tree (XWINDOW (f->root_window), &r);

  if (WINDOWP (f->tool_bar_window))
    mouse_face_overwritten_p
      |= expose_window (XWINDOW (f->tool_bar_window), &r);

#ifndef USE_X_TOOLKIT
  if (WINDOWP (f->menu_bar_window))
    mouse_face_overwritten_p
      |= expose_window (XWINDOW (f->menu_bar_window), &r);
#endif /* not USE_X_TOOLKIT */

  /* Some window managers support a focus-follows-mouse style with
     delayed raising of frames.  Imagine a partially obscured frame,
     and moving the mouse into partially obscured mouse-face on that
     frame.  The visible part of the mouse-face will be highlighted,
     then the WM raises the obscured frame.  With at least one WM, KDE
     2.1, Emacs is not getting any event for the raising of the frame
     (even tried with SubstructureRedirectMask), only Expose events.
     These expose events will draw text normally, i.e. not
     highlighted.  Which means we must redo the highlight here.
     Subsume it under ``we love X''.  --gerd 2001-08-15  */
  if (mouse_face_overwritten_p && !FRAME_GARBAGED_P (f))
    {
      struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
      if (f == dpyinfo->mouse_face_mouse_frame)
	{
	  int x = dpyinfo->mouse_face_mouse_x;
	  int y = dpyinfo->mouse_face_mouse_y;
	  clear_mouse_face (dpyinfo);
	  note_mouse_highlight (f, x, y);
	}
    }
}


/* Redraw (parts) of all windows in the window tree rooted at W that
   intersect R.  R contains frame pixel coordinates.  Value is
   non-zero if the exposure overwrites mouse-face.  */

static int
expose_window_tree (w, r)
     struct window *w;
     XRectangle *r;
{
  struct frame *f = XFRAME (w->frame);
  int mouse_face_overwritten_p = 0;
  
  while (w && !FRAME_GARBAGED_P (f))
    {
      if (!NILP (w->hchild))
	mouse_face_overwritten_p
	  |= expose_window_tree (XWINDOW (w->hchild), r);
      else if (!NILP (w->vchild))
	mouse_face_overwritten_p
	  |= expose_window_tree (XWINDOW (w->vchild), r);
      else
	mouse_face_overwritten_p |= expose_window (w, r);
      
      w = NILP (w->next) ? NULL : XWINDOW (w->next);
    }

  return mouse_face_overwritten_p;
}


/* Redraw the part of glyph row area AREA of glyph row ROW on window W
   which intersects rectangle R.  R is in window-relative coordinates.  */

static void
expose_area (w, row, r, area)
     struct window *w;
     struct glyph_row *row;
     XRectangle *r;
     enum glyph_row_area area;
{
  struct glyph *first = row->glyphs[area];
  struct glyph *end = row->glyphs[area] + row->used[area];
  struct glyph *last;
  int first_x, start_x, x;

  if (area == TEXT_AREA && row->fill_line_p)
    /* If row extends face to end of line write the whole line.  */
    x_draw_glyphs (w, 0, row, area, 0, row->used[area],
		   DRAW_NORMAL_TEXT, 0);
  else
    {
      /* Set START_X to the window-relative start position for drawing glyphs of
	 AREA.  The first glyph of the text area can be partially visible.
	 The first glyphs of other areas cannot.  */
      if (area == LEFT_MARGIN_AREA)
	start_x = 0;
      else if (area == TEXT_AREA)
	start_x = row->x + window_box_width (w, LEFT_MARGIN_AREA);
      else
	start_x = (window_box_width (w, LEFT_MARGIN_AREA)
		   + window_box_width (w, TEXT_AREA));
      x = start_x;

      /* Find the first glyph that must be redrawn.  */
      while (first < end
	     && x + first->pixel_width < r->x)
	{
	  x += first->pixel_width;
	  ++first;
	}
      
      /* Find the last one.  */
      last = first;
      first_x = x;
      while (last < end
	     && x < r->x + r->width)
	{
	  x += last->pixel_width;
	  ++last;
	}
      
      /* Repaint.  */
      if (last > first)
	x_draw_glyphs (w, first_x - start_x, row, area,
		       first - row->glyphs[area],
		       last - row->glyphs[area],
		       DRAW_NORMAL_TEXT, 0);
    }
}
      

/* Redraw the parts of the glyph row ROW on window W intersecting
   rectangle R.  R is in window-relative coordinates.  Value is
   non-zero if mouse-face was overwritten.  */

static int
expose_line (w, row, r)
     struct window *w;
     struct glyph_row *row;
     XRectangle *r;
{
  xassert (row->enabled_p);
  
  if (row->mode_line_p || w->pseudo_window_p)
    x_draw_glyphs (w, 0, row, TEXT_AREA, 0, row->used[TEXT_AREA],
		   DRAW_NORMAL_TEXT, 0);
  else
    {
      if (row->used[LEFT_MARGIN_AREA])
	expose_area (w, row, r, LEFT_MARGIN_AREA);
      if (row->used[TEXT_AREA])
	expose_area (w, row, r, TEXT_AREA);
      if (row->used[RIGHT_MARGIN_AREA])
	expose_area (w, row, r, RIGHT_MARGIN_AREA);
      x_draw_row_fringe_bitmaps (w, row);
    }

  return row->mouse_face_p;
}


/* Return non-zero if W's cursor intersects rectangle R.  */

static int
x_phys_cursor_in_rect_p (w, r)
     struct window *w;
     XRectangle *r;
{
  XRectangle cr, result;
  struct glyph *cursor_glyph;

  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph)
    {
      cr.x = w->phys_cursor.x;
      cr.y = w->phys_cursor.y;
      cr.width = cursor_glyph->pixel_width;
      cr.height = w->phys_cursor_height;
      return x_intersect_rectangles (&cr, r, &result);
    }
  else
    return 0;
}


/* Redraw the part of window W intersection rectangle FR.  Pixel
   coordinates in FR are frame-relative.  Call this function with
   input blocked.  Value is non-zero if the exposure overwrites
   mouse-face.  */

static int
expose_window (w, fr)
     struct window *w;
     XRectangle *fr;
{
  struct frame *f = XFRAME (w->frame);
  XRectangle wr, r;
  int mouse_face_overwritten_p = 0;

  /* If window is not yet fully initialized, do nothing.  This can
     happen when toolkit scroll bars are used and a window is split.
     Reconfiguring the scroll bar will generate an expose for a newly
     created window.  */
  if (w->current_matrix == NULL)
    return 0;

  /* When we're currently updating the window, display and current
     matrix usually don't agree.  Arrange for a thorough display
     later.  */
  if (w == updated_window)
    {
      SET_FRAME_GARBAGED (f);
      return 0;
    }

  /* Frame-relative pixel rectangle of W.  */
  wr.x = XFASTINT (w->left) * CANON_X_UNIT (f);
  wr.y = XFASTINT (w->top) * CANON_Y_UNIT (f);
  wr.width = XFASTINT (w->width) * CANON_X_UNIT (f);
  wr.height = XFASTINT (w->height) * CANON_Y_UNIT (f);

  if (x_intersect_rectangles (fr, &wr, &r))
    {
      int yb = window_text_bottom_y (w);
      struct glyph_row *row;
      int cursor_cleared_p;
  
      TRACE ((stderr, "expose_window (%d, %d, %d, %d)\n",
	      r.x, r.y, r.width, r.height));

      /* Convert to window coordinates.  */
      r.x = FRAME_TO_WINDOW_PIXEL_X (w, r.x);
      r.y = FRAME_TO_WINDOW_PIXEL_Y (w, r.y);

      /* Turn off the cursor.  */
      if (!w->pseudo_window_p
	  && x_phys_cursor_in_rect_p (w, &r))
	{
	  x_clear_cursor (w);
	  cursor_cleared_p = 1;
	}
      else
	cursor_cleared_p = 0;

      /* Find the first row intersecting the rectangle R.  */
      for (row = w->current_matrix->rows;
	   row->enabled_p;
	   ++row)
	{
	  int y0 = row->y;
	  int y1 = MATRIX_ROW_BOTTOM_Y (row);
	  
	  if ((y0 >= r.y && y0 < r.y + r.height)
	      || (y1 > r.y && y1 < r.y + r.height)
	      || (r.y >= y0 && r.y < y1)
	      || (r.y + r.height > y0 && r.y + r.height < y1))
	    {
	      if (expose_line (w, row, &r))
		mouse_face_overwritten_p = 1;
	    }
	      
	  if (y1 >= yb)
	    break;
	}

      /* Display the mode line if there is one.  */
      if (WINDOW_WANTS_MODELINE_P (w)
	  && (row = MATRIX_MODE_LINE_ROW (w->current_matrix),
	      row->enabled_p)
	  && row->y < r.y + r.height)
	{
	  if (expose_line (w, row, &r))
	    mouse_face_overwritten_p = 1;
	}

      if (!w->pseudo_window_p)
	{
	  /* Draw border between windows.  */
	  x_draw_vertical_border (w);
      
	  /* Turn the cursor on again.  */
	  if (cursor_cleared_p)
	    x_update_window_cursor (w, 1);
	}
    }

  return mouse_face_overwritten_p;
}


/* Determine the intersection of two rectangles R1 and R2.  Return
   the intersection in *RESULT.  Value is non-zero if RESULT is not
   empty.  */

static int
x_intersect_rectangles (r1, r2, result)
     XRectangle *r1, *r2, *result;
{
  XRectangle *left, *right;
  XRectangle *upper, *lower;
  int intersection_p = 0;
  
  /* Rearrange so that R1 is the left-most rectangle.  */
  if (r1->x < r2->x)
    left = r1, right = r2;
  else
    left = r2, right = r1;

  /* X0 of the intersection is right.x0, if this is inside R1,
     otherwise there is no intersection.  */
  if (right->x <= left->x + left->width)
    {
      result->x = right->x;
      
      /* The right end of the intersection is the minimum of the
	 the right ends of left and right.  */
      result->width = (min (left->x + left->width, right->x + right->width)
		       - result->x);

      /* Same game for Y.  */
      if (r1->y < r2->y)
	upper = r1, lower = r2;
      else
	upper = r2, lower = r1;

      /* The upper end of the intersection is lower.y0, if this is inside
	 of upper.  Otherwise, there is no intersection.  */
      if (lower->y <= upper->y + upper->height)
	{
	  result->y = lower->y;
	  
	  /* The lower end of the intersection is the minimum of the lower
	     ends of upper and lower.  */
	  result->height = (min (lower->y + lower->height, 
				 upper->y + upper->height)
			    - result->y);
	  intersection_p = 1;
	}
    }

  return intersection_p;
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
      Fselect_window (selected_frame->selected_window);
      choose_minibuf_frame ();
#endif /* ! 0 */

      if (dpyinfo->x_focus_frame && dpyinfo->x_focus_frame->auto_raise)
	pending_autoraise_frame = dpyinfo->x_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  x_frame_rehighlight (dpyinfo);
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
	= ((GC_FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame)))
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

#ifdef HAVE_X11R4
  XDisplayKeycodes (dpyinfo->display, &min_code, &max_code);
#else
  min_code = dpyinfo->display->min_keycode;
  max_code = dpyinfo->display->max_keycode;
#endif

  syms = XGetKeyboardMapping (dpyinfo->display,
			      min_code, max_code - min_code + 1,
			      &syms_per_code);
  mods = XGetModifierMapping (dpyinfo->display);

  /* Scan the modifier table to see which modifier bits the Meta and
     Alt keysyms are on.  */
  {
    int row, col;	/* The row and column in the modifier table.  */

    for (row = 3; row < 8; row++)
      for (col = 0; col < mods->max_keypermod; col++)
	{
	  KeyCode code
	    = mods->modifiermap[(row * mods->max_keypermod) + col];

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

/* Given a pixel position (PIX_X, PIX_Y) on frame F, return glyph
   co-ordinates in (*X, *Y).  Set *BOUNDS to the rectangle that the
   glyph at X, Y occupies, if BOUNDS != 0.  If NOCLIP is non-zero, do
   not force the value into range.  */

void
pixel_to_glyph_coords (f, pix_x, pix_y, x, y, bounds, noclip)
     FRAME_PTR f;
     register int pix_x, pix_y;
     register int *x, *y;
     XRectangle *bounds;
     int noclip;
{
  /* Arrange for the division in PIXEL_TO_CHAR_COL etc.  to round down
     even for negative values.  */
  if (pix_x < 0)
    pix_x -= FONT_WIDTH ((f)->output_data.x->font) - 1;
  if (pix_y < 0)
    pix_y -= (f)->output_data.x->line_height - 1;

  pix_x = PIXEL_TO_CHAR_COL (f, pix_x);
  pix_y = PIXEL_TO_CHAR_ROW (f, pix_y);

  if (bounds)
    {
      bounds->width  = FONT_WIDTH  (f->output_data.x->font);
      bounds->height = f->output_data.x->line_height;
      bounds->x = CHAR_TO_PIXEL_COL (f, pix_x);
      bounds->y = CHAR_TO_PIXEL_ROW (f, pix_y);
    }

  if (!noclip)
    {
      if (pix_x < 0)
	pix_x = 0;
      else if (pix_x > FRAME_WINDOW_WIDTH (f))
	pix_x = FRAME_WINDOW_WIDTH (f);

      if (pix_y < 0)
	pix_y = 0;
      else if (pix_y > f->height)
	pix_y = f->height;
    }

  *x = pix_x;
  *y = pix_y;
}


/* Given HPOS/VPOS in the current matrix of W, return corresponding
   frame-relative pixel positions in *FRAME_X and *FRAME_Y.  If we
   can't tell the positions because W's display is not up to date,
   return 0.  */

int
glyph_to_pixel_coords (w, hpos, vpos, frame_x, frame_y)
     struct window *w;
     int hpos, vpos;
     int *frame_x, *frame_y;
{
  int success_p;

  xassert (hpos >= 0 && hpos < w->current_matrix->matrix_w);
  xassert (vpos >= 0 && vpos < w->current_matrix->matrix_h);

  if (display_completed)
    {
      struct glyph_row *row = MATRIX_ROW (w->current_matrix, vpos);
      struct glyph *glyph = row->glyphs[TEXT_AREA];
      struct glyph *end = glyph + min (hpos, row->used[TEXT_AREA]);

      *frame_y = row->y;
      *frame_x = row->x;
      while (glyph < end)
	{
	  *frame_x += glyph->pixel_width;
	  ++glyph;
	}

      success_p = 1;
    }
  else
    {
      *frame_y = *frame_x = 0;
      success_p = 0;
    }

  *frame_y = WINDOW_TO_FRAME_PIXEL_Y (w, *frame_y);
  *frame_x = WINDOW_TO_FRAME_PIXEL_X (w, *frame_x);
  return success_p;
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

static void
note_mouse_movement (frame, event)
     FRAME_PTR frame;
     XMotionEvent *event;
{
  last_mouse_movement_time = event->time;
  last_mouse_motion_event = *event;
  XSETFRAME (last_mouse_motion_frame, frame);

#if 0 /* Lisp must not be called asynchronously, so this must
	 not be done.  */
  if (x_autoselect_window_p)
    {
      int area;
      Lisp_Object window;
      static Lisp_Object last_window;

      window = window_from_coordinates (frame, XINT (event->x), XINT (event->y), &area, 0);

      /* Window will be selected only when it is not selected now and
	 last mouse movement event was not in it.  Minubuffer window
	 will be selected iff it is active.  */
      if (!EQ (window, last_window)
	  && !EQ (window, selected_window)
	  && (!MINI_WINDOW_P (XWINDOW (window))
	      || (EQ (window, minibuf_window) && minibuf_level > 0)))
	Fselect_window (window);

      last_window=window;
    }
#endif

  if (event->window != FRAME_X_WINDOW (frame))
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, -1, -1);
    }

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  else if (event->x < last_mouse_glyph.x
	   || event->x >= last_mouse_glyph.x + last_mouse_glyph.width
	   || event->y < last_mouse_glyph.y
	   || event->y >= last_mouse_glyph.y + last_mouse_glyph.height)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;
      note_mouse_highlight (frame, event->x, event->y);
    }
}


/************************************************************************
			      Mouse Face
 ************************************************************************/

/* Find the glyph under window-relative coordinates X/Y in window W.
   Consider only glyphs from buffer text, i.e. no glyphs from overlay
   strings.  Return in *HPOS and *VPOS the row and column number of
   the glyph found.  Return in *AREA the glyph area containing X.
   Value is a pointer to the glyph found or null if X/Y is not on
   text, or we can't tell because W's current matrix is not up to
   date.  */

static struct glyph *
x_y_to_hpos_vpos (w, x, y, hpos, vpos, area, buffer_only_p)
     struct window *w;
     int x, y;
     int *hpos, *vpos, *area;
     int buffer_only_p;
{
  struct glyph *glyph, *end;
  struct glyph_row *row = NULL;
  int x0, i, left_area_width;

  /* Find row containing Y.  Give up if some row is not enabled.  */
  for (i = 0; i < w->current_matrix->nrows; ++i)
    {
      row = MATRIX_ROW (w->current_matrix, i);
      if (!row->enabled_p)
	return NULL;
      if (y >= row->y && y < MATRIX_ROW_BOTTOM_Y (row))
	break;
    }

  *vpos = i;
  *hpos = 0;

  /* Give up if Y is not in the window.  */
  if (i == w->current_matrix->nrows)
    return NULL;

  /* Get the glyph area containing X.  */
  if (w->pseudo_window_p)
    {
      *area = TEXT_AREA;
      x0 = 0;
    }
  else
    {
      left_area_width = window_box_width (w, LEFT_MARGIN_AREA);
      if (x < left_area_width)
	{
	  *area = LEFT_MARGIN_AREA;
	  x0 = 0;
	}
      else if (x < left_area_width + window_box_width (w, TEXT_AREA))
	{
	  *area = TEXT_AREA;
	  x0 = row->x + left_area_width;
	}
      else
	{
	  *area = RIGHT_MARGIN_AREA;
	  x0 = left_area_width + window_box_width (w, TEXT_AREA);
	}
    }

  /* Find glyph containing X.  */
  glyph = row->glyphs[*area];
  end = glyph + row->used[*area];
  while (glyph < end)
    {
      if (x < x0 + glyph->pixel_width)
	{
	  if (w->pseudo_window_p)
	    break;
	  else if (!buffer_only_p || BUFFERP (glyph->object))
	    break;
	}
      
      x0 += glyph->pixel_width;
      ++glyph;
    }

  if (glyph == end)
    return NULL;

  *hpos = glyph - row->glyphs[*area];
  return glyph;
}


/* Convert frame-relative x/y to coordinates relative to window W.
   Takes pseudo-windows into account.  */

static void
frame_to_window_pixel_xy (w, x, y)
     struct window *w;
     int *x, *y;
{
  if (w->pseudo_window_p)
    {
      /* A pseudo-window is always full-width, and starts at the
	 left edge of the frame, plus a frame border.  */
      struct frame *f = XFRAME (w->frame);
      *x -= FRAME_INTERNAL_BORDER_WIDTH_SAFE (f);
      *y = FRAME_TO_WINDOW_PIXEL_Y (w, *y);
    }
  else
    {
      *x = FRAME_TO_WINDOW_PIXEL_X (w, *x);
      *y = FRAME_TO_WINDOW_PIXEL_Y (w, *y);
    }
}


/* Take proper action when mouse has moved to the mode or header line of
   window W, x-position X.  MODE_LINE_P non-zero means mouse is on the
   mode line.  X is relative to the start of the text display area of
   W, so the width of fringes and scroll bars must be subtracted
   to get a position relative to the start of the mode line.  */

static void
note_mode_line_highlight (w, x, mode_line_p)
     struct window *w;
     int x, mode_line_p;
{
  struct frame *f = XFRAME (w->frame);
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Cursor cursor = dpyinfo->vertical_scroll_bar_cursor;
  struct glyph_row *row;

  if (mode_line_p)
    row = MATRIX_MODE_LINE_ROW (w->current_matrix);
  else
    row = MATRIX_HEADER_LINE_ROW (w->current_matrix);

  if (row->enabled_p)
    {
      struct glyph *glyph, *end;
      Lisp_Object help, map;
      int x0;
      
      /* Find the glyph under X.  */
      glyph = row->glyphs[TEXT_AREA];
      end = glyph + row->used[TEXT_AREA];
      x0 = - (FRAME_LEFT_SCROLL_BAR_WIDTH (f) * CANON_X_UNIT (f)
	      + FRAME_X_LEFT_FRINGE_WIDTH (f));
      
      while (glyph < end
	     && x >= x0 + glyph->pixel_width)
	{
	  x0 += glyph->pixel_width;
	  ++glyph;
	}

      if (glyph < end
	  && STRINGP (glyph->object)
	  && XSTRING (glyph->object)->intervals
	  && glyph->charpos >= 0
	  && glyph->charpos < XSTRING (glyph->object)->size)
	{
	  /* If we're on a string with `help-echo' text property,
	     arrange for the help to be displayed.  This is done by
	     setting the global variable help_echo to the help string.  */
	  help = Fget_text_property (make_number (glyph->charpos),
				     Qhelp_echo, glyph->object);
	  if (!NILP (help))
	    {
	      help_echo = help;
	      XSETWINDOW (help_echo_window, w);
	      help_echo_object = glyph->object;
	      help_echo_pos = glyph->charpos;
	    }

	  /* Change the mouse pointer according to what is under X/Y.  */
	  map = Fget_text_property (make_number (glyph->charpos),
				    Qlocal_map, glyph->object);
	  if (KEYMAPP (map))
	    cursor = f->output_data.x->nontext_cursor;
	  else
	    {
	      map = Fget_text_property (make_number (glyph->charpos),
					Qkeymap, glyph->object);
	      if (KEYMAPP (map))
		cursor = f->output_data.x->nontext_cursor;
	    }
	}
    }

  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), cursor);
}


/* Take proper action when the mouse has moved to position X, Y on
   frame F as regards highlighting characters that have mouse-face
   properties.  Also de-highlighting chars where the mouse was before.
   X and Y can be negative or out of range.  */

static void
note_mouse_highlight (f, x, y)
     struct frame *f;
     int x, y;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  int portion;
  Lisp_Object window;
  struct window *w;
  Cursor cursor = None;
  struct buffer *b;

  /* When a menu is active, don't highlight because this looks odd.  */
#ifdef USE_X_TOOLKIT
  if (popup_activated ())
    return;
#endif

  if (NILP (Vmouse_highlight)
      || !f->glyphs_initialized_p)
    return;

  dpyinfo->mouse_face_mouse_x = x;
  dpyinfo->mouse_face_mouse_y = y;
  dpyinfo->mouse_face_mouse_frame = f;

  if (dpyinfo->mouse_face_defer)
    return;

  if (gc_in_progress)
    {
      dpyinfo->mouse_face_deferred_gc = 1;
      return;
    }

  /* Which window is that in?  */
  window = window_from_coordinates (f, x, y, &portion, 1);

  /* If we were displaying active text in another window, clear that.  */
  if (! EQ (window, dpyinfo->mouse_face_window))
    clear_mouse_face (dpyinfo);

  /* Not on a window -> return.  */
  if (!WINDOWP (window))
    return;

  /* Convert to window-relative pixel coordinates.  */
  w = XWINDOW (window);
  frame_to_window_pixel_xy (w, &x, &y);

  /* Handle tool-bar window differently since it doesn't display a
     buffer.  */
  if (EQ (window, f->tool_bar_window))
    {
      note_tool_bar_highlight (f, x, y);
      return;
    }

  /* Mouse is on the mode or header line?  */
  if (portion == 1 || portion == 3)
    {
      note_mode_line_highlight (w, x, portion == 1);
      return;
    }
  
  if (portion == 2)
    cursor = f->output_data.x->horizontal_drag_cursor;
  else
    cursor = f->output_data.x->text_cursor;

  /* Are we in a window whose display is up to date?
     And verify the buffer's text has not changed.  */
  b = XBUFFER (w->buffer);
  if (/* Within text portion of the window.  */
      portion == 0
      && EQ (w->window_end_valid, w->buffer)
      && XFASTINT (w->last_modified) == BUF_MODIFF (b)
      && XFASTINT (w->last_overlay_modified) == BUF_OVERLAY_MODIFF (b))
    {
      int hpos, vpos, pos, i, area;
      struct glyph *glyph;
      Lisp_Object object;
      Lisp_Object mouse_face = Qnil, overlay = Qnil, position;
      Lisp_Object *overlay_vec = NULL;
      int len, noverlays;
      struct buffer *obuf;
      int obegv, ozv, same_region;

      /* Find the glyph under X/Y.  */
      glyph = x_y_to_hpos_vpos (w, x, y, &hpos, &vpos, &area, 0);

      /* Clear mouse face if X/Y not over text.  */
      if (glyph == NULL
	  || area != TEXT_AREA
	  || !MATRIX_ROW (w->current_matrix, vpos)->displays_text_p)
	{
	  if (clear_mouse_face (dpyinfo))
	    cursor = None;
	  goto set_cursor;
	}

      pos = glyph->charpos;
      object = glyph->object;
      if (!STRINGP (object) && !BUFFERP (object))
	goto set_cursor;

      /* If we get an out-of-range value, return now; avoid an error.  */
      if (BUFFERP (object) && pos > BUF_Z (b))
	goto set_cursor;

      /* Make the window's buffer temporarily current for
	 overlays_at and compute_char_face.  */
      obuf = current_buffer;
      current_buffer = b;
      obegv = BEGV;
      ozv = ZV;
      BEGV = BEG;
      ZV = Z;

      /* Is this char mouse-active or does it have help-echo?  */
      position = make_number (pos);

      if (BUFFERP (object))
	{
	  /* Put all the overlays we want in a vector in overlay_vec.
	     Store the length in len.  If there are more than 10, make
	     enough space for all, and try again.  */
	  len = 10;
	  overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	  noverlays = overlays_at (pos, 0, &overlay_vec, &len, NULL, NULL, 0);
	  if (noverlays > len)
	    {
	      len = noverlays;
	      overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	      noverlays = overlays_at (pos, 0, &overlay_vec, &len, NULL, NULL,0);
	    }

	  /* Sort overlays into increasing priority order.  */
	  noverlays = sort_overlays (overlay_vec, noverlays, w);
	}
      else
	noverlays = 0;

      same_region = (EQ (window, dpyinfo->mouse_face_window)
		     && vpos >= dpyinfo->mouse_face_beg_row
		     && vpos <= dpyinfo->mouse_face_end_row
		     && (vpos > dpyinfo->mouse_face_beg_row
			 || hpos >= dpyinfo->mouse_face_beg_col)
		     && (vpos < dpyinfo->mouse_face_end_row
			 || hpos < dpyinfo->mouse_face_end_col
			 || dpyinfo->mouse_face_past_end));

      if (same_region)
	cursor = None;

      /* Check mouse-face highlighting.  */
      if (! same_region
	  /* If there exists an overlay with mouse-face overlapping
	     the one we are currently highlighting, we have to
	     check if we enter the overlapping overlay, and then
	     highlight only that.  */
	  || (OVERLAYP (dpyinfo->mouse_face_overlay)
	      && mouse_face_overlay_overlaps (dpyinfo->mouse_face_overlay)))
	{
	  /* Find the highest priority overlay that has a mouse-face
	     property.  */
	  overlay = Qnil;
	  for (i = noverlays - 1; i >= 0 && NILP (overlay); --i)
	    {
	      mouse_face = Foverlay_get (overlay_vec[i], Qmouse_face);
	      if (!NILP (mouse_face))
		overlay = overlay_vec[i];
	    }
	  
	  /* If we're actually highlighting the same overlay as
	     before, there's no need to do that again.  */
	  if (!NILP (overlay)
	      && EQ (overlay, dpyinfo->mouse_face_overlay))
	    goto check_help_echo;
	    
	  dpyinfo->mouse_face_overlay = overlay;

	  /* Clear the display of the old active region, if any.  */
	  if (clear_mouse_face (dpyinfo))
	    cursor = None;

	  /* If no overlay applies, get a text property.  */
	  if (NILP (overlay))
	    mouse_face = Fget_text_property (position, Qmouse_face, object);

	  /* Handle the overlay case.  */
	  if (!NILP (overlay))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after;
	      int ignore;

	      before = Foverlay_start (overlay);
	      after = Foverlay_end (overlay);
	      /* Record this as the current active region.  */
	      fast_find_position (w, XFASTINT (before),
				  &dpyinfo->mouse_face_beg_col,
				  &dpyinfo->mouse_face_beg_row,
				  &dpyinfo->mouse_face_beg_x,
				  &dpyinfo->mouse_face_beg_y, Qnil);
		       
	      dpyinfo->mouse_face_past_end
		= !fast_find_position (w, XFASTINT (after),
				       &dpyinfo->mouse_face_end_col,
				       &dpyinfo->mouse_face_end_row,
				       &dpyinfo->mouse_face_end_x,
				       &dpyinfo->mouse_face_end_y, Qnil);
	      dpyinfo->mouse_face_window = window;
	      dpyinfo->mouse_face_face_id
		= face_at_buffer_position (w, pos, 0, 0,
					   &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      cursor = None;
	    }
	  /* Handle the text property case.  */
	  else if (!NILP (mouse_face) && BUFFERP (object))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after, beginning, end;
	      int ignore;

	      beginning = Fmarker_position (w->start);
	      end = make_number (BUF_Z (XBUFFER (object))
				 - XFASTINT (w->window_end_pos));
	      before
		= Fprevious_single_property_change (make_number (pos + 1),
						    Qmouse_face,
						    object, beginning);
	      after
		= Fnext_single_property_change (position, Qmouse_face,
						object, end);
		
	      /* Record this as the current active region.  */
	      fast_find_position (w, XFASTINT (before),
				  &dpyinfo->mouse_face_beg_col,
				  &dpyinfo->mouse_face_beg_row,
				  &dpyinfo->mouse_face_beg_x,
				  &dpyinfo->mouse_face_beg_y, Qnil);
	      dpyinfo->mouse_face_past_end
		= !fast_find_position (w, XFASTINT (after),
				       &dpyinfo->mouse_face_end_col,
				       &dpyinfo->mouse_face_end_row,
				       &dpyinfo->mouse_face_end_x,
				       &dpyinfo->mouse_face_end_y, Qnil);
	      dpyinfo->mouse_face_window = window;

	      if (BUFFERP (object))
		dpyinfo->mouse_face_face_id
		  = face_at_buffer_position (w, pos, 0, 0,
					     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      cursor = None;
	    }
	  else if (!NILP (mouse_face) && STRINGP (object))
	    {
	      Lisp_Object b, e;
	      int ignore;
		
	      b = Fprevious_single_property_change (make_number (pos + 1),
						    Qmouse_face,
						    object, Qnil);
	      e = Fnext_single_property_change (position, Qmouse_face,
						object, Qnil);
	      if (NILP (b))
		b = make_number (0);
	      if (NILP (e))
		e = make_number (XSTRING (object)->size - 1);
	      fast_find_string_pos (w, XINT (b), object,
				    &dpyinfo->mouse_face_beg_col,
				    &dpyinfo->mouse_face_beg_row,
				    &dpyinfo->mouse_face_beg_x,
				    &dpyinfo->mouse_face_beg_y, 0);
	      fast_find_string_pos (w, XINT (e), object,
				    &dpyinfo->mouse_face_end_col,
				    &dpyinfo->mouse_face_end_row,
				    &dpyinfo->mouse_face_end_x,
				    &dpyinfo->mouse_face_end_y, 1);
	      dpyinfo->mouse_face_past_end = 0;
	      dpyinfo->mouse_face_window = window;
	      dpyinfo->mouse_face_face_id
		= face_at_string_position (w, object, pos, 0, 0, 0, &ignore,
					   glyph->face_id, 1);
	      show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
	      cursor = None;
	    }
	  else if (STRINGP (object) && NILP (mouse_face))
	    {
	      /* A string which doesn't have mouse-face, but
		 the text ``under'' it might have.  */
	      struct glyph_row *r = MATRIX_ROW (w->current_matrix, vpos);
	      int start = MATRIX_ROW_START_CHARPOS (r);
	      
	      pos = string_buffer_position (w, object, start);
	      if (pos > 0)
		mouse_face = get_char_property_and_overlay (make_number (pos),
							    Qmouse_face,
							    w->buffer,
							    &overlay);
	      if (!NILP (mouse_face) && !NILP (overlay))
		{
		  Lisp_Object before = Foverlay_start (overlay);
		  Lisp_Object after = Foverlay_end (overlay);
		  int ignore;

		  /* Note that we might not be able to find position
		     BEFORE in the glyph matrix if the overlay is
		     entirely covered by a `display' property.  In
		     this case, we overshoot.  So let's stop in
		     the glyph matrix before glyphs for OBJECT.  */
		  fast_find_position (w, XFASTINT (before),
				      &dpyinfo->mouse_face_beg_col,
				      &dpyinfo->mouse_face_beg_row,
				      &dpyinfo->mouse_face_beg_x,
				      &dpyinfo->mouse_face_beg_y,
				      object);
		       
		  dpyinfo->mouse_face_past_end
		    = !fast_find_position (w, XFASTINT (after),
					   &dpyinfo->mouse_face_end_col,
					   &dpyinfo->mouse_face_end_row,
					   &dpyinfo->mouse_face_end_x,
					   &dpyinfo->mouse_face_end_y,
					   Qnil);
		  dpyinfo->mouse_face_window = window;
		  dpyinfo->mouse_face_face_id
		    = face_at_buffer_position (w, pos, 0, 0,
					       &ignore, pos + 1, 1);

		  /* Display it as active.  */
		  show_mouse_face (dpyinfo, DRAW_MOUSE_FACE);
		  cursor = None;
		}
	    }
	}

    check_help_echo:

      /* Look for a `help-echo' property.  */
      {
	Lisp_Object help, overlay;

	/* Check overlays first.  */
	help = overlay = Qnil;
	for (i = noverlays - 1; i >= 0 && NILP (help); --i)
	  {
	    overlay = overlay_vec[i];
	    help = Foverlay_get (overlay, Qhelp_echo);
	  }

	if (!NILP (help))
	  {
	    help_echo = help;
	    help_echo_window = window;
	    help_echo_object = overlay;
	    help_echo_pos = pos;
	  }
	else
	  {
	    Lisp_Object object = glyph->object;
	    int charpos = glyph->charpos;
	      
	    /* Try text properties.  */
	    if (STRINGP (object)
		&& charpos >= 0
		&& charpos < XSTRING (object)->size)
	      {
		help = Fget_text_property (make_number (charpos),
					   Qhelp_echo, object);
		if (NILP (help))
		  {
		    /* If the string itself doesn't specify a help-echo,
		       see if the buffer text ``under'' it does.  */
		    struct glyph_row *r
		      = MATRIX_ROW (w->current_matrix, vpos);
		    int start = MATRIX_ROW_START_CHARPOS (r);
		    int pos = string_buffer_position (w, object, start);
		    if (pos > 0)
		      {
			help = Fget_char_property (make_number (pos),
						   Qhelp_echo, w->buffer);
			if (!NILP (help))
			  {
			    charpos = pos;
			    object = w->buffer;
			  }
		      }
		  }
	      }
	    else if (BUFFERP (object)
		     && charpos >= BEGV
		     && charpos < ZV)
	      help = Fget_text_property (make_number (charpos), Qhelp_echo,
					 object);
	    
	    if (!NILP (help))
	      {
		help_echo = help;
		help_echo_window = window;
		help_echo_object = object;
		help_echo_pos = charpos;
	      }
	  }
      }
	  
      BEGV = obegv;
      ZV = ozv;
      current_buffer = obuf;
    }

 set_cursor:
  
  if (cursor != None)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), cursor);
}

static void
redo_mouse_highlight ()
{
  if (!NILP (last_mouse_motion_frame)
      && FRAME_LIVE_P (XFRAME (last_mouse_motion_frame)))
    note_mouse_highlight (XFRAME (last_mouse_motion_frame),
			  last_mouse_motion_event.x,
			  last_mouse_motion_event.y);
}



/***********************************************************************
			       Tool-bars
 ***********************************************************************/

static int x_tool_bar_item P_ ((struct frame *, int, int,
				struct glyph **, int *, int *, int *));

/* Tool-bar item index of the item on which a mouse button was pressed
   or -1.  */

static int last_tool_bar_item;


/* Get information about the tool-bar item at position X/Y on frame F.
   Return in *GLYPH a pointer to the glyph of the tool-bar item in
   the current matrix of the tool-bar window of F, or NULL if not
   on a tool-bar item.  Return in *PROP_IDX the index of the tool-bar
   item in F->tool_bar_items.  Value is

   -1	if X/Y is not on a tool-bar item
   0	if X/Y is on the same item that was highlighted before.
   1	otherwise.  */

static int
x_tool_bar_item (f, x, y, glyph, hpos, vpos, prop_idx)
     struct frame *f;
     int x, y;
     struct glyph **glyph;
     int *hpos, *vpos, *prop_idx;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int area;

  /* Find the glyph under X/Y.  */
  *glyph = x_y_to_hpos_vpos (w, x, y, hpos, vpos, &area, 0);
  if (*glyph == NULL)
    return -1;

  /* Get the start of this tool-bar item's properties in
     f->tool_bar_items.  */
  if (!tool_bar_item_info (f, *glyph, prop_idx))
    return -1;

  /* Is mouse on the highlighted item?  */
  if (EQ (f->tool_bar_window, dpyinfo->mouse_face_window)
      && *vpos >= dpyinfo->mouse_face_beg_row
      && *vpos <= dpyinfo->mouse_face_end_row
      && (*vpos > dpyinfo->mouse_face_beg_row
	  || *hpos >= dpyinfo->mouse_face_beg_col)
      && (*vpos < dpyinfo->mouse_face_end_row
	  || *hpos < dpyinfo->mouse_face_end_col
	  || dpyinfo->mouse_face_past_end))
    return 0;
  
  return 1;
}


/* Handle mouse button event on the tool-bar of frame F, at
   frame-relative coordinates X/Y.  EVENT_TYPE is either ButtonPress
   or ButtonRelase.  */

static void
x_handle_tool_bar_click (f, button_event)
     struct frame *f;
     XButtonEvent *button_event;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  struct window *w = XWINDOW (f->tool_bar_window);
  int hpos, vpos, prop_idx;
  struct glyph *glyph;
  Lisp_Object enabled_p;
  int x = button_event->x;
  int y = button_event->y;
  
  /* If not on the highlighted tool-bar item, return.  */
  frame_to_window_pixel_xy (w, &x, &y);
  if (x_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx) != 0)
    return;

  /* If item is disabled, do nothing.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (NILP (enabled_p))
    return;
  
  if (button_event->type == ButtonPress)
    {
      /* Show item in pressed state.  */
      show_mouse_face (dpyinfo, DRAW_IMAGE_SUNKEN);
      dpyinfo->mouse_face_image_state = DRAW_IMAGE_SUNKEN;
      last_tool_bar_item = prop_idx;
    }
  else
    {
      Lisp_Object key, frame;
      struct input_event event;

      /* Show item in released state.  */
      show_mouse_face (dpyinfo, DRAW_IMAGE_RAISED);
      dpyinfo->mouse_face_image_state = DRAW_IMAGE_RAISED;

      key = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_KEY);

      XSETFRAME (frame, f);
      event.kind = TOOL_BAR_EVENT;
      event.frame_or_window = frame;
      event.arg = frame;
      kbd_buffer_store_event (&event);

      event.kind = TOOL_BAR_EVENT;
      event.frame_or_window = frame;
      event.arg = key;
      event.modifiers = x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f),
						button_event->state);
      kbd_buffer_store_event (&event);
      last_tool_bar_item = -1;
    }
}


/* Possibly highlight a tool-bar item on frame F when mouse moves to
   tool-bar window-relative coordinates X/Y.  Called from
   note_mouse_highlight.  */

static void
note_tool_bar_highlight (f, x, y)
     struct frame *f;
     int x, y;
{
  Lisp_Object window = f->tool_bar_window;
  struct window *w = XWINDOW (window);
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  int hpos, vpos;
  struct glyph *glyph;
  struct glyph_row *row;
  int i;
  Lisp_Object enabled_p;
  int prop_idx;
  enum draw_glyphs_face draw;
  int mouse_down_p, rc;

  /* Function note_mouse_highlight is called with negative x(y
     values when mouse moves outside of the frame.  */
  if (x <= 0 || y <= 0)
    {
      clear_mouse_face (dpyinfo);
      return;
    }

  rc = x_tool_bar_item (f, x, y, &glyph, &hpos, &vpos, &prop_idx);
  if (rc < 0)
    {
      /* Not on tool-bar item.  */
      clear_mouse_face (dpyinfo);
      return;
    }
  else if (rc == 0)
    goto set_help_echo;

  clear_mouse_face (dpyinfo);
  
  /* Mouse is down, but on different tool-bar item?  */
  mouse_down_p = (dpyinfo->grabbed
		  && f == last_mouse_frame
		  && FRAME_LIVE_P (f));
  if (mouse_down_p
      && last_tool_bar_item != prop_idx)
    return;

  dpyinfo->mouse_face_image_state = DRAW_NORMAL_TEXT;
  draw = mouse_down_p ? DRAW_IMAGE_SUNKEN : DRAW_IMAGE_RAISED;
  
  /* If tool-bar item is not enabled, don't highlight it.  */
  enabled_p = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_ENABLED_P);
  if (!NILP (enabled_p))
    {
      /* Compute the x-position of the glyph.  In front and past the
	 image is a space.  We include this is the highlighted area.  */
      row = MATRIX_ROW (w->current_matrix, vpos);
      for (i = x = 0; i < hpos; ++i)
	x += row->glyphs[TEXT_AREA][i].pixel_width;
      
      /* Record this as the current active region.  */
      dpyinfo->mouse_face_beg_col = hpos;
      dpyinfo->mouse_face_beg_row = vpos;
      dpyinfo->mouse_face_beg_x = x;
      dpyinfo->mouse_face_beg_y = row->y;
      dpyinfo->mouse_face_past_end = 0;
      
      dpyinfo->mouse_face_end_col = hpos + 1;
      dpyinfo->mouse_face_end_row = vpos;
      dpyinfo->mouse_face_end_x = x + glyph->pixel_width;
      dpyinfo->mouse_face_end_y = row->y;
      dpyinfo->mouse_face_window = window;
      dpyinfo->mouse_face_face_id = TOOL_BAR_FACE_ID;
      
      /* Display it as active.  */
      show_mouse_face (dpyinfo, draw);
      dpyinfo->mouse_face_image_state = draw;
    }
      
 set_help_echo:
  
  /* Set help_echo to a help string to display for this tool-bar item.
     XTread_socket does the rest.  */
  help_echo_object = help_echo_window = Qnil;
  help_echo_pos = -1;
  help_echo = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_HELP);
  if (NILP (help_echo))
    help_echo = AREF (f->tool_bar_items, prop_idx + TOOL_BAR_ITEM_CAPTION);
}



/* Find the glyph matrix position of buffer position CHARPOS in window
   *W.  HPOS, *VPOS, *X, and *Y are set to the positions found.  W's
   current glyphs must be up to date.  If CHARPOS is above window
   start return (0, 0, 0, 0).  If CHARPOS is after end of W, return end
   of last line in W.  In the row containing CHARPOS, stop before glyphs
   having STOP as object.  */

#if 0 /* This is a version of fast_find_position that's more correct
	 in the presence of hscrolling, for example.  I didn't install
	 it right away because the problem fixed is minor, it failed
	 in 20.x as well, and I think it's too risky to install 
	 so near the release of 21.1.  2001-09-25 gerd.  */

static int
fast_find_position (w, charpos, hpos, vpos, x, y, stop)
     struct window *w;
     int charpos;
     int *hpos, *vpos, *x, *y;
     Lisp_Object stop;
{
  struct glyph_row *row, *first;
  struct glyph *glyph, *end;
  int i, past_end = 0;

  first = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  row = row_containing_pos (w, charpos, first, NULL, 0);
  if (row == NULL)
    {
      if (charpos < MATRIX_ROW_START_CHARPOS (first))
	{
	  *x = *y = *hpos = *vpos = 0;
	  return 0;
	}
      else
	{
	  row = MATRIX_ROW (w->current_matrix, XFASTINT (w->window_end_vpos));
	  past_end = 1;
	}
    }

  *x = row->x;
  *y = row->y;
  *vpos = MATRIX_ROW_VPOS (row, w->current_matrix);
  
  glyph = row->glyphs[TEXT_AREA];
  end = glyph + row->used[TEXT_AREA];
  
  /* Skip over glyphs not having an object at the start of the row.
     These are special glyphs like truncation marks on terminal
     frames.  */
  if (row->displays_text_p)
    while (glyph < end
	   && INTEGERP (glyph->object)
	   && !EQ (stop, glyph->object)
	   && glyph->charpos < 0)
      {
	*x += glyph->pixel_width;
	++glyph;
      }

  while (glyph < end
	 && !INTEGERP (glyph->object)
	 && !EQ (stop, glyph->object)
	 && (!BUFFERP (glyph->object)
	     || glyph->charpos < charpos))
    {
      *x += glyph->pixel_width;
      ++glyph;
    }

  *hpos = glyph - row->glyphs[TEXT_AREA];
  return past_end;
}

#else /* not 0 */

static int
fast_find_position (w, pos, hpos, vpos, x, y, stop)
     struct window *w;
     int pos;
     int *hpos, *vpos, *x, *y;
     Lisp_Object stop;
{
  int i;
  int lastcol;
  int maybe_next_line_p = 0;
  int line_start_position;
  int yb = window_text_bottom_y (w);
  struct glyph_row *row, *best_row;
  int row_vpos, best_row_vpos;
  int current_x;

  row = best_row = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
  row_vpos = best_row_vpos = MATRIX_ROW_VPOS (row, w->current_matrix);

  while (row->y < yb)
    {
      if (row->used[TEXT_AREA])
	line_start_position = row->glyphs[TEXT_AREA]->charpos;
      else
	line_start_position = 0;

      if (line_start_position > pos)
	break;
      /* If the position sought is the end of the buffer,
	 don't include the blank lines at the bottom of the window.  */
      else if (line_start_position == pos
	       && pos == BUF_ZV (XBUFFER (w->buffer)))
	{
	  maybe_next_line_p = 1;
	  break;
	}
      else if (line_start_position > 0)
	{
	  best_row = row;
	  best_row_vpos = row_vpos;
	}

      if (row->y + row->height >= yb)
	break;
      
      ++row;
      ++row_vpos;
    }
  
  /* Find the right column within BEST_ROW.  */
  lastcol = 0;
  current_x = best_row->x;
  for (i = 0; i < best_row->used[TEXT_AREA]; i++)
    {
      struct glyph *glyph = best_row->glyphs[TEXT_AREA] + i;
      int charpos = glyph->charpos;

      if (BUFFERP (glyph->object))
	{
	  if (charpos == pos)
	    {
	      *hpos = i;
	      *vpos = best_row_vpos;
	      *x = current_x;
	      *y = best_row->y;
	      return 1;
	    }
	  else if (charpos > pos)
	    break;
	}
      else if (EQ (glyph->object, stop))
	break;

      if (charpos > 0)
	lastcol = i;
      current_x += glyph->pixel_width;
    }

  /* If we're looking for the end of the buffer,
     and we didn't find it in the line we scanned,
     use the start of the following line.  */
  if (maybe_next_line_p)
    {
      ++best_row;
      ++best_row_vpos;
      lastcol = 0;
      current_x = best_row->x;
    }

  *vpos = best_row_vpos;
  *hpos = lastcol + 1;
  *x = current_x;
  *y = best_row->y;
  return 0;
}

#endif /* not 0 */


/* Find the position of the glyph for position POS in OBJECT in
   window W's current matrix, and return in *X, *Y the pixel
   coordinates, and return in *HPOS, *VPOS the column/row of the glyph.

   RIGHT_P non-zero means return the position of the right edge of the
   glyph, RIGHT_P zero means return the left edge position.

   If no glyph for POS exists in the matrix, return the position of
   the glyph with the next smaller position that is in the matrix, if
   RIGHT_P is zero.  If RIGHT_P is non-zero, and no glyph for POS
   exists in the matrix, return the position of the glyph with the
   next larger position in OBJECT.

   Value is non-zero if a glyph was found.  */

static int
fast_find_string_pos (w, pos, object, hpos, vpos, x, y, right_p)
     struct window *w;
     int pos;
     Lisp_Object object;
     int *hpos, *vpos, *x, *y;
     int right_p;
{
  int yb = window_text_bottom_y (w);
  struct glyph_row *r;
  struct glyph *best_glyph = NULL;
  struct glyph_row *best_row = NULL;
  int best_x = 0;

  for (r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
       r->enabled_p && r->y < yb;
       ++r)
    {
      struct glyph *g = r->glyphs[TEXT_AREA];
      struct glyph *e = g + r->used[TEXT_AREA];
      int gx;

      for (gx = r->x; g < e; gx += g->pixel_width, ++g)
	if (EQ (g->object, object))
	  {
	    if (g->charpos == pos)
	      {
		best_glyph = g;
		best_x = gx;
		best_row = r;
		goto found;
	      }
	    else if (best_glyph == NULL
		     || ((abs (g->charpos - pos)
			 < abs (best_glyph->charpos - pos))
			 && (right_p
			     ? g->charpos < pos
			     : g->charpos > pos)))
	      {
		best_glyph = g;
		best_x = gx;
		best_row = r;
	      }
	  }
    }

 found:

  if (best_glyph)
    {
      *x = best_x;
      *hpos = best_glyph - best_row->glyphs[TEXT_AREA];

      if (right_p)
	{
	  *x += best_glyph->pixel_width;
	  ++*hpos;
	}
      
      *y = best_row->y;
      *vpos = best_row - w->current_matrix->rows;
    }

  return best_glyph != NULL;
}


/* Display the active region described by mouse_face_*
   in its mouse-face if HL > 0, in its normal face if HL = 0.  */

static void
show_mouse_face (dpyinfo, draw)
     struct x_display_info *dpyinfo;
     enum draw_glyphs_face draw;
{
  struct window *w = XWINDOW (dpyinfo->mouse_face_window);
  struct frame *f = XFRAME (WINDOW_FRAME (w));

  if (/* If window is in the process of being destroyed, don't bother
	 to do anything.  */
      w->current_matrix != NULL
      /* Don't update mouse highlight if hidden */
      && (draw != DRAW_MOUSE_FACE || !dpyinfo->mouse_face_hidden)
      /* Recognize when we are called to operate on rows that don't exist
	 anymore.  This can happen when a window is split.  */
      && dpyinfo->mouse_face_end_row < w->current_matrix->nrows)
    {
      int phys_cursor_on_p = w->phys_cursor_on_p;
      struct glyph_row *row, *first, *last;

      first = MATRIX_ROW (w->current_matrix, dpyinfo->mouse_face_beg_row);
      last = MATRIX_ROW (w->current_matrix, dpyinfo->mouse_face_end_row);
      
      for (row = first; row <= last && row->enabled_p; ++row)
	{
	  int start_hpos, end_hpos, start_x;

	  /* For all but the first row, the highlight starts at column 0.  */
	  if (row == first)
	    {
	      start_hpos = dpyinfo->mouse_face_beg_col;
	      start_x = dpyinfo->mouse_face_beg_x;
	    }
	  else
	    {
	      start_hpos = 0;
	      start_x = 0;
	    }

	  if (row == last)
	    end_hpos = dpyinfo->mouse_face_end_col;
	  else
	    end_hpos = row->used[TEXT_AREA];

	  if (end_hpos > start_hpos)
	    {
	      x_draw_glyphs (w, start_x, row, TEXT_AREA, 
			     start_hpos, end_hpos, draw, 0);

	      row->mouse_face_p
		= draw == DRAW_MOUSE_FACE || draw == DRAW_IMAGE_RAISED;
	    }
	}

      /* When we've written over the cursor, arrange for it to
	 be displayed again.  */
      if (phys_cursor_on_p && !w->phys_cursor_on_p)
	x_display_cursor (w, 1,
			  w->phys_cursor.hpos, w->phys_cursor.vpos,
			  w->phys_cursor.x, w->phys_cursor.y);
    }

  /* Change the mouse cursor.  */
  if (draw == DRAW_NORMAL_TEXT)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->text_cursor);
  else if (draw == DRAW_MOUSE_FACE)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->cross_cursor);
  else
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   f->output_data.x->nontext_cursor);
}

/* Clear out the mouse-highlighted active region.
   Redraw it un-highlighted first.  Value is non-zero if mouse
   face was actually drawn unhighlighted.  */

static int
clear_mouse_face (dpyinfo)
     struct x_display_info *dpyinfo;
{
  int cleared = 0;
  
  if (!NILP (dpyinfo->mouse_face_window))
    {
      show_mouse_face (dpyinfo, DRAW_NORMAL_TEXT);
      cleared = 1;
    }

  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_overlay = Qnil;
  return cleared;
}


/* Clear any mouse-face on window W.  This function is part of the
   redisplay interface, and is called from try_window_id and similar
   functions to ensure the mouse-highlight is off.  */

static void
x_clear_mouse_face (w)
     struct window *w;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (XFRAME (w->frame));
  Lisp_Object window;

  BLOCK_INPUT;
  XSETWINDOW (window, w);
  if (EQ (window, dpyinfo->mouse_face_window))
    clear_mouse_face (dpyinfo);
  UNBLOCK_INPUT;
}


/* Just discard the mouse face information for frame F, if any.
   This is used when the size of F is changed.  */

void
cancel_mouse_face (f)
     FRAME_PTR f;
{
  Lisp_Object window;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  window = dpyinfo->mouse_face_window;
  if (! NILP (window) && XFRAME (XWINDOW (window)->frame) == f)
    {
      dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }
}


static int glyph_rect P_ ((struct frame *f, int, int, XRectangle *));


/* Try to determine frame pixel position and size of the glyph under
   frame pixel coordinates X/Y on frame F .  Return the position and
   size in *RECT.  Value is non-zero if we could compute these
   values.  */

static int
glyph_rect (f, x, y, rect)
     struct frame *f;
     int x, y;
     XRectangle *rect;
{
  Lisp_Object window;
  int part, found = 0;

  window = window_from_coordinates (f, x, y, &part, 0);
  if (!NILP (window))
    {
      struct window *w = XWINDOW (window);
      struct glyph_row *r = MATRIX_FIRST_TEXT_ROW (w->current_matrix);
      struct glyph_row *end = r + w->current_matrix->nrows - 1;

      frame_to_window_pixel_xy (w, &x, &y);
      
      for (; !found && r < end && r->enabled_p; ++r)
	if (r->y >= y)
	  {
	    struct glyph *g = r->glyphs[TEXT_AREA];
	    struct glyph *end = g + r->used[TEXT_AREA];
	    int gx;
	      
	    for (gx = r->x; !found && g < end; gx += g->pixel_width, ++g)
	      if (gx >= x)
		{
		  rect->width = g->pixel_width;
		  rect->height = r->height;
		  rect->x = WINDOW_TO_FRAME_PIXEL_X (w, gx);
		  rect->y = WINDOW_TO_FRAME_PIXEL_Y (w, r->y);
		  found = 1;
		}
	  }
    }

  return found;
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
	if (FRAME_X_DISPLAY (XFRAME (frame)) == FRAME_X_DISPLAY (*fp))
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
	int count;

	win = root;

	/* XTranslateCoordinates can get errors if the window
	   structure is changing at the same time this function
	   is running.  So at least we must not crash from them.  */

	count = x_catch_errors (FRAME_X_DISPLAY (*fp));

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

	x_uncatch_errors (FRAME_X_DISPLAY (*fp), count);

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

	    int width, height, gx, gy;
	    XRectangle rect;
	    
	    if (glyph_rect (f1, win_x, win_y, &rect))
	      last_mouse_glyph = rect;
	    else
	      {
		width = FRAME_SMALLEST_CHAR_WIDTH (f1);
		height = FRAME_SMALLEST_FONT_HEIGHT (f1);
		gx = win_x;
		gy = win_y;
	      
		/* Arrange for the division in PIXEL_TO_CHAR_COL etc. to
		   round down even for negative values.  */
		if (gx < 0)
		  gx -= width - 1;
		if (gy < 0)
		  gy -= height - 1;
		gx = (gx + width - 1) / width * width;
		gy = (gy + height - 1) / height * height;
	    
		last_mouse_glyph.width  = width;
		last_mouse_glyph.height = height;
		last_mouse_glyph.x = gx;
		last_mouse_glyph.y = gy;
	      }

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


#ifdef USE_X_TOOLKIT

/* Atimer callback function for TIMER.  Called every 0.1s to process
   Xt timeouts, if needed.  We must avoid calling XtAppPending as
   much as possible because that function does an implicit XFlush
   that slows us down.  */

static void
x_process_timeouts (timer)
     struct atimer *timer;
{
  if (toolkit_scroll_bar_interaction || popup_activated_flag)
    {
      BLOCK_INPUT;
      while (XtAppPending (Xt_app_con) & XtIMTimer)
	XtAppProcessEvent (Xt_app_con, XtIMTimer);
      UNBLOCK_INPUT;
    }
}

#endif /* USE_X_TOOLKIT */


/* Scroll bar support.  */

/* Given an X window ID, find the struct scroll_bar which manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */

static struct scroll_bar *
x_window_to_scroll_bar (window_id)
     Window window_id;
{
  Lisp_Object tail;

  for (tail = Vframe_list;
       XGCTYPE (tail) == Lisp_Cons;
       tail = XCDR (tail))
    {
      Lisp_Object frame, bar, condemned;

      frame = XCAR (tail);
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
	if (SCROLL_BAR_X_WINDOW (XSCROLL_BAR (bar)) == window_id)
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
  
  for (tail = Vframe_list;
       XGCTYPE (tail) == Lisp_Cons;
       tail = XCDR (tail))
    {
      Lisp_Object frame = XCAR (tail);
      Widget menu_bar = XFRAME (frame)->output_data.x->menubar_widget;
      
      if (menu_bar && xlwmenu_window_p (menu_bar, window))
	return menu_bar;
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


/* Id of action hook installed for scroll bars.  */

static XtActionHookId action_hook_id;

/* Lisp window being scrolled.  Set when starting to interact with
   a toolkit scroll bar, reset to nil when ending the interaction.  */

static Lisp_Object window_being_scrolled;

/* Last scroll bar part sent in xm_scroll_callback.  */

static int last_scroll_bar_part;

/* Whether this is an Xaw with arrow-scrollbars.  This should imply
   that movements of 1/20 of the screen size are mapped to up/down.  */

static Boolean xaw3d_arrow_scroll;

/* Whether the drag scrolling maintains the mouse at the top of the
   thumb.  If not, resizing the thumb needs to be done more carefully
   to avoid jerkyness.  */

static Boolean xaw3d_pick_top;


/* Action hook installed via XtAppAddActionHook when toolkit scroll
   bars are used..  The hook is responsible for detecting when
   the user ends an interaction with the scroll bar, and generates
   a `end-scroll' scroll_bar_click' event if so.  */

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
      XSCROLL_BAR (w->vertical_scroll_bar)->dragging = Qnil;
      window_being_scrolled = Qnil;
      last_scroll_bar_part = -1;

      /* Xt timeouts no longer needed.  */
      toolkit_scroll_bar_interaction = 0;
    }
}

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
  
  ievent->kind = scroll_bar_click;
  ievent->frame_or_window = window;
  ievent->arg = Qnil;
  ievent->timestamp = XtLastTimestampProcessed (FRAME_X_DISPLAY (f));
  ievent->part = ev->data.l[1];
  ievent->code = ev->data.l[2];
  ievent->x = make_number ((int) ev->data.l[3]);
  ievent->y = make_number ((int) ev->data.l[4]);
  ievent->modifiers = 0;
}


#ifdef USE_MOTIF

/* Minimum and maximum values used for Motif scroll bars.  */

#define XM_SB_MIN 1
#define XM_SB_MAX 10000000
#define XM_SB_RANGE (XM_SB_MAX - XM_SB_MIN)


/* Scroll bar callback for Motif scroll bars.  WIDGET is the scroll
   bar widget.  CLIENT_DATA is a pointer to the scroll_bar structure.
   CALL_DATA is a pointer a a XmScrollBarCallbackStruct.  */

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
	int dragging_down_p = (INTEGERP (bar->dragging)
			       && XINT (bar->dragging) <= cs->value);

	/* Get the slider size.  */
	BLOCK_INPUT;
	XtVaGetValues (widget, XmNsliderSize, &slider_size, NULL);
	UNBLOCK_INPUT;

	/* At the max position of the scroll bar, do a line-wise
	   movement.  Without doing anything, we would be called with
	   the same cs->value again and again.  If we want to make
	   sure that we can reach the end of the buffer, we have to do
	   something.

	   Implementation note: setting bar->dragging always to
	   cs->value gives a smoother movement at the max position.
	   Setting it to nil when doing line-wise movement gives
	   a better slider behavior. */
	
	if (cs->value + slider_size == XM_SB_MAX
	    || (dragging_down_p
		&& last_scroll_bar_part == scroll_bar_down_arrow))
	  {
	    part = scroll_bar_down_arrow;
	    bar->dragging = Qnil;
	  }
	else
	  {
	    whole = XM_SB_RANGE;
	    portion = min (cs->value - XM_SB_MIN, XM_SB_MAX - slider_size);
	    part = scroll_bar_handle;
	    bar->dragging = make_number (cs->value);
	  }
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


#else /* !USE_MOTIF, i.e. Xaw.  */


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

  if (shown < 1 && (abs (top + shown - 1) < 1.0/height))
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
   has taken place.  It's magnitude is in the range 0..height of the
   scroll bar.  Negative values mean scroll towards buffer start.
   Values < height of scroll bar mean line-wise movement.  */

static void
xaw_scroll_callback (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  struct scroll_bar *bar = (struct scroll_bar *) client_data;
  int position = (int) call_data;
  Dimension height;
  int part;

  /* Get the height of the scroll bar.  */
  BLOCK_INPUT;
  XtVaGetValues (widget, XtNheight, &height, NULL);
  UNBLOCK_INPUT;

  if (abs (position) >= height)
    part = (position < 0) ? scroll_bar_above_handle : scroll_bar_below_handle;

  /* If Xaw3d was compiled with ARROW_SCROLLBAR,
     it maps line-movement to call_data = max(5, height/20).  */
  else if (xaw3d_arrow_scroll && abs (position) <= max (5, height / 20))
    part = (position < 0) ? scroll_bar_up_arrow : scroll_bar_down_arrow;
  else
    part = scroll_bar_move_ratio;

  window_being_scrolled = bar->window;
  bar->dragging = Qnil;
  last_scroll_bar_part = part;
  x_send_scroll_bar_event (bar->window, part, position, height);
}


#endif /* not USE_MOTIF */


/* Create the widget for scroll bar BAR on frame F.  Record the widget
   and X window of the scroll bar in BAR.  */

static void
x_create_toolkit_scroll_bar (f, bar)
     struct frame *f;
     struct scroll_bar *bar;
{
  Window xwindow;
  Widget widget;
  Arg av[20];
  int ac = 0;
  char *scroll_bar_name = "verticalScrollBar";
  unsigned long pixel;

  BLOCK_INPUT;

#ifdef USE_MOTIF
  /* Set resources.  Create the widget.  */
  XtSetArg (av[ac], XtNmappedWhenManaged, False); ++ac;
  XtSetArg (av[ac], XmNminimum, XM_SB_MIN); ++ac;
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
	  XtSetArg (av[ac], "topShadowPixel", pixel);
	  ++ac;
	}
      pixel = f->output_data.x->scroll_bar_bottom_shadow_pixel;
      if (pixel != -1)
	{
	  XtSetArg (av[ac], "bottomShadowPixel", pixel);
	  ++ac;
	}
    }

  widget = XtCreateWidget (scroll_bar_name, scrollbarWidgetClass,
			   f->output_data.x->edit_widget, av, ac);

  {
    char *initial = "";
    char *val = initial;
    XtVaGetValues (widget, XtNscrollVCursor, (XtPointer) &val,
		   XtNpickTop, (XtPointer) &xaw3d_pick_top, NULL);
    if (val == initial)
      {	/* ARROW_SCROLL */
	xaw3d_arrow_scroll = True;
	/* Isn't that just a personal preference ?   -sm */
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

  /* Install an action hook that let's us detect when the user
     finishes interacting with a scroll bar.  */
  if (action_hook_id == 0)
    action_hook_id = XtAppAddActionHook (Xt_app_con, xt_action_hook, 0);
  
  /* Remember X window and widget in the scroll bar vector.  */
  SET_SCROLL_BAR_X_WIDGET (bar, widget);
  xwindow = XtWindow (widget);
  SET_SCROLL_BAR_X_WINDOW (bar, xwindow);

  UNBLOCK_INPUT;
}


/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

static void
x_set_toolkit_scroll_bar_thumb (bar, portion, position, whole)
     struct scroll_bar *bar;
     int portion, position, whole;
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  Widget widget = SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar);
  float top, shown;

  if (whole == 0)
    top = 0, shown = 1;
  else
    {
      top = (float) position / whole;
      shown = (float) portion / whole;
    }

  BLOCK_INPUT;

#ifdef USE_MOTIF
  {
    int size, value;

    /* Slider size.  Must be in the range [1 .. MAX - MIN] where MAX
       is the scroll bar's maximum and MIN is the scroll bar's minimum
       value.  */
    size = shown * XM_SB_RANGE;
    size = min (size, XM_SB_RANGE);
    size = max (size, 1);

    /* Position.  Must be in the range [MIN .. MAX - SLIDER_SIZE].  */
    value = top * XM_SB_RANGE;
    value = min (value, XM_SB_MAX - size);
    value = max (value, XM_SB_MIN);

    if (NILP (bar->dragging))
      XmScrollBarSetValues (widget, value, size, 0, 0, False);
    else if (last_scroll_bar_part == scroll_bar_down_arrow)
      /* This has the negative side effect that the slider value is
	 not what it would be if we scrolled here using line-wise or
	 page-wise movement.  */
      XmScrollBarSetValues (widget, value, XM_SB_RANGE - value, 0, 0, False);
    else
      {
	/* If currently dragging, only update the slider size.
	   This reduces flicker effects.  */
	int old_value, old_size, increment, page_increment;
	
	XmScrollBarGetValues (widget, &old_value, &old_size,
			      &increment, &page_increment);
	XmScrollBarSetValues (widget, old_value,
			      min (size, XM_SB_RANGE - old_value),
			      0, 0, False);
      }
  }
#else /* !USE_MOTIF i.e. use Xaw */
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
#ifdef HAVE_XAW3D
	    ScrollbarWidget sb = (ScrollbarWidget) widget;
	    int scroll_mode = 0;
	    
	    /* `scroll_mode' only exists with Xaw3d + ARROW_SCROLLBAR.  */
	    if (xaw3d_arrow_scroll)
	      {
		/* Xaw3d stupidly ignores resize requests while dragging
		   so we have to make it believe it's not in dragging mode.  */
		scroll_mode = sb->scrollbar.scroll_mode;
		if (scroll_mode == 2)
		  sb->scrollbar.scroll_mode = 0;
	      }
#endif
	    /* Try to make the scrolling a tad smoother.  */
	    if (!xaw3d_pick_top)
	      shown = min (shown, old_shown);
	    
	    XawScrollbarSetThumb (widget, top, shown);
	    
#ifdef HAVE_XAW3D
	    if (xaw3d_arrow_scroll && scroll_mode == 2)
	      sb->scrollbar.scroll_mode = scroll_mode;
#endif
	  }
      }
  }
#endif /* !USE_MOTIF */

  UNBLOCK_INPUT;
}

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
    = XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));

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
      a.background_pixel = f->output_data.x->background_pixel;
    
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
    SET_SCROLL_BAR_X_WINDOW (bar, window);
  }
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  XSETWINDOW (bar->window, w);
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
  if (!NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  /* Map the window/widget.  */
#ifdef USE_TOOLKIT_SCROLL_BARS
  {
    Widget scroll_bar = SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar);
    XtConfigureWidget (scroll_bar,
		       left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
		       top,
		       width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
		       max (height, 1), 0);
    XtMapWidget (scroll_bar);
    }
#else /* not USE_TOOLKIT_SCROLL_BARS */
  XMapRaised (FRAME_X_DISPLAY (f), SCROLL_BAR_X_WINDOW (bar));
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  UNBLOCK_INPUT;
  return bar;
}


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

#ifndef USE_TOOLKIT_SCROLL_BARS

static void
x_scroll_bar_set_handle (bar, start, end, rebuild)
     struct scroll_bar *bar;
     int start, end;
     int rebuild;
{
  int dragging = ! NILP (bar->dragging);
  Window w = SCROLL_BAR_X_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  GC gc = f->output_data.x->normal_gc;

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == XINT (bar->start)
      && end == XINT (bar->end))
    return;

  BLOCK_INPUT;

  {
    int inside_width = VERTICAL_SCROLL_BAR_INSIDE_WIDTH (f, XINT (bar->width));
    int inside_height = VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, XINT (bar->height));
    int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));

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
		      f->output_data.x->foreground_pixel);

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
  XtDestroyWidget (SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar));
#else
  XDestroyWindow (FRAME_X_DISPLAY (f), SCROLL_BAR_X_WINDOW (bar));
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
  int window_x, window_y, window_width, window_height;

  /* Get window dimensions.  */
  window_box (w, -1, &window_x, &window_y, &window_width, &window_height);
  top = window_y;
  width = FRAME_SCROLL_BAR_COLS (f) * CANON_X_UNIT (f);
  height = window_height;

  /* Compute the left edge of the scroll bar area.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
    left = XINT (w->left) + XINT (w->width) - FRAME_SCROLL_BAR_COLS (f);
  else
    left = XFASTINT (w->left);
  left *= CANON_X_UNIT (f);
  left += FRAME_INTERNAL_BORDER_WIDTH (f);

  /* Compute the width of the scroll bar which might be less than
     the width of the area reserved for the scroll bar.  */
  if (FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0)
    sb_width = FRAME_SCROLL_BAR_PIXEL_WIDTH (f);
  else
    sb_width = width;

  /* Compute the left edge of the scroll bar.  */
#ifdef USE_TOOLKIT_SCROLL_BARS
  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
    sb_left = left + width - sb_width - (width - sb_width) / 2; 
  else
    sb_left = left + (width - sb_width) / 2;
#else
  if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
    sb_left = left + width - sb_width; 
  else
    sb_left = left;
#endif
  
  /* Does the scroll bar exist yet?  */
  if (NILP (w->vertical_scroll_bar))
    {
      if (width > 0 && height > 0)
	{
	  BLOCK_INPUT;
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

      if (sb_left != XINT (bar->left))
	mask |= CWX;
      if (top != XINT (bar->top))
	mask |= CWY;
      if (sb_width != XINT (bar->width))
	mask |= CWWidth;
      if (height != XINT (bar->height))	
	mask |= CWHeight;
      
#ifdef USE_TOOLKIT_SCROLL_BARS

      /* Since toolkit scroll bars are smaller than the space reserved
	 for them on the frame, we have to clear "under" them.  */
      if (width > 0 && height > 0)
	x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		      left, top, width, height, False);

      /* Move/size the scroll bar widget.  */
      if (mask)
	XtConfigureWidget (SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar),
			   sb_left + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
			   top,
			   sb_width - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
			   max (height, 1), 0);

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
	int area_width = FRAME_SCROLL_BAR_COLS (f) * CANON_X_UNIT (f);
	int rest = area_width - sb_width;
	if (rest > 0 && height > 0)
	  {
	    if (FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f))
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
	  XConfigureWindow (FRAME_X_DISPLAY (f), SCROLL_BAR_X_WINDOW (bar),
			    mask, &wc);
	}
  
#endif /* not USE_TOOLKIT_SCROLL_BARS */

      /* Remember new settings.  */
      XSETINT (bar->left, sb_left);
      XSETINT (bar->top, top);
      XSETINT (bar->width, sb_width);
      XSETINT (bar->height, height);
      
      UNBLOCK_INPUT;
    }

#ifdef USE_TOOLKIT_SCROLL_BARS
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


/* Handle an Expose or GraphicsExpose event on a scroll bar.  This
   is a no-op when using toolkit scroll bars.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static void
x_scroll_bar_expose (bar, event)
     struct scroll_bar *bar;
     XEvent *event;
{
#ifndef USE_TOOLKIT_SCROLL_BARS

  Window w = SCROLL_BAR_X_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  GC gc = f->output_data.x->normal_gc;
  int width_trim = VERTICAL_SCROLL_BAR_WIDTH_TRIM;

  BLOCK_INPUT;

  x_scroll_bar_set_handle (bar, XINT (bar->start), XINT (bar->end), 1);

  /* Draw a one-pixel border just inside the edges of the scroll bar.  */
  XDrawRectangle (FRAME_X_DISPLAY (f), w, gc,

		  /* x, y, width, height */
		  0, 0,
		  XINT (bar->width) - 1 - width_trim - width_trim,
		  XINT (bar->height) - 1);
    
  UNBLOCK_INPUT;

#endif /* not USE_TOOLKIT_SCROLL_BARS */
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than no_event, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

#ifndef USE_TOOLKIT_SCROLL_BARS

static void
x_scroll_bar_handle_click (bar, event, emacs_event)
     struct scroll_bar *bar;
     XEvent *event;
     struct input_event *emacs_event;
{
  if (! GC_WINDOWP (bar->window))
    abort ();

  emacs_event->kind = scroll_bar_click;
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
      = VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, XINT (bar->height));
#endif
    int top_range
      = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));
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
  FRAME_PTR f = XFRAME (XWINDOW (bar->window)->frame);

  last_mouse_movement_time = event->xmotion.time;

  f->mouse_moved = 1;
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
#if 0
      int inside_height
	= VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, XINT (bar->height));
#endif
      int top_range
	= VERTICAL_SCROLL_BAR_TOP_RANGE     (f, XINT (bar->height));

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
		  SCROLL_BAR_X_WINDOW (XSCROLL_BAR (bar)),
		  0, 0, 0, 0, True);
#endif /* not USE_TOOLKIT_SCROLL_BARS */
}

/* This processes Expose events from the menu-bar specific X event
   loop in xmenu.c.  This allows to redisplay the frame if necessary
   when handling menu-bar or pop-up items.  */

int
process_expose_from_menu (event)
     XEvent event;
{
  FRAME_PTR f;
  struct x_display_info *dpyinfo;
  int frame_exposed_p = 0;

  BLOCK_INPUT;

  dpyinfo = x_display_info_for_display (event.xexpose.display);
  f = x_window_to_frame (dpyinfo, event.xexpose.window);
  if (f)
    {
      if (f->async_visible == 0)
	{
	  f->async_visible = 1;
	  f->async_iconified = 0;
	  f->output_data.x->has_been_visible = 1;
	  SET_FRAME_GARBAGED (f);
	}
      else
	{
	  expose_frame (x_window_to_frame (dpyinfo, event.xexpose.window),
			event.xexpose.x, event.xexpose.y,
			event.xexpose.width, event.xexpose.height);
	  frame_exposed_p = 1;
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
  return frame_exposed_p;
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
    = (struct selection_event_queue *) xmalloc (sizeof (struct selection_event_queue));

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
x_unqueue_events (display)
     Display *display;
{
  while (queue != NULL)
    {
      struct selection_event_queue *queue_tmp = queue;
      XPutBackEvent (display, &queue_tmp->event);
      queue = queue_tmp->next;
      xfree ((char *)queue_tmp);
    }
}

/* Start queuing SelectionRequest events.  */

void
x_start_queuing_selection_requests (display)
     Display *display;
{
  x_queue_selection_requests++;
}

/* Stop queuing SelectionRequest events.  */

void
x_stop_queuing_selection_requests (display)
     Display *display;
{
  x_queue_selection_requests--;
  x_unqueue_events (display);
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
         if (numchars >= 1)						\
           {								\
             bufp->kind = menu_bar_activate_event;			\
             XSETFRAME (bufp->frame_or_window, f);			\
             bufp->arg = Qnil;						\
             bufp++;							\
             count++;							\
             numchars--;						\
           }								\
       }								\
     while (0)

#define SET_SAVED_BUTTON_EVENT SET_SAVED_MENU_EVENT (sizeof (XButtonEvent))
#define SET_SAVED_KEY_EVENT    SET_SAVED_MENU_EVENT (sizeof (XKeyEvent))

/* Read events coming from the X server.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   Events representing keys are stored in buffer BUFP,
   which can hold up to NUMCHARS characters.
   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   EXPECTED is nonzero if the caller knows input is available.  */

int
XTread_socket (sd, bufp, numchars, expected)
     register int sd;
     /* register */ struct input_event *bufp;
     /* register */ int numchars;
     int expected;
{
  int count = 0;
  int nbytes = 0;
  XEvent event;
  struct frame *f;
  int event_found = 0;
  struct x_display_info *dpyinfo;
  struct coding_system coding;

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
    abort ();			/* Don't think this happens.  */

  ++handling_signal;
  
  /* The input should be decoded if it is from XIM.  Currently the
     locale of XIM is the same as that of the system.  So, we can use
     Vlocale_coding_system which is initialized properly at Emacs
     startup time.  */
  setup_coding_system (Vlocale_coding_system, &coding);
  coding.src_multibyte = 0;
  coding.dst_multibyte = 1;
  /* The input is converted to events, thus we can't handle
     composition.  Anyway, there's no XIM that gives us composition
     information.  */
  coding.composing = COMPOSITION_DISABLED;

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

      /* For debugging, this gives a way to fake an I/O error.  */
      if (dpyinfo == XTread_socket_fake_io_error)
	{
	  XTread_socket_fake_io_error = 0;
	  x_io_error_quitter (dpyinfo->display);
	}

#ifdef HAVE_X_SM
      BLOCK_INPUT;
      count += x_session_check_input (bufp, &numchars);
      UNBLOCK_INPUT;
#endif

      while (XPending (dpyinfo->display))
	{
	  XNextEvent (dpyinfo->display, &event);

#ifdef HAVE_X_I18N
	  {
	    /* Filter events for the current X input method.
	       XFilterEvent returns non-zero if the input method has
	       consumed the event.  We pass the frame's X window to
	       XFilterEvent because that's the one for which the IC
	       was created.  */
	    struct frame *f1 = x_any_window_to_frame (dpyinfo,
						      event.xclient.window);
	    if (XFilterEvent (&event, f1 ? FRAME_X_WINDOW (f1) : None))
	      break;
	  }
#endif
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
			    int count = x_catch_errors (d);
			    XSetInputFocus (d, event.xclient.window,
					    /* The ICCCM says this is
					       the only valid choice.  */
					    RevertToParent,
					    event.xclient.data.l[1]);
			    /* This is needed to detect the error
			       if there is an error.  */
			    XSync (d, False);
			    x_uncatch_errors (d, count);
			  }  
			/* Not certain about handling scroll bars here */
#endif /* 0 */
		      }
		    else if (event.xclient.data.l[0]
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
			if (numchars > 0
#ifdef HAVE_X_SM
                            && ! x_session_have_connection ()
#endif
                            )
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
		      }
		    else if (event.xclient.data.l[0]
			     == dpyinfo->Xatom_wm_delete_window)
		      {
			struct frame *f
			  = x_any_window_to_frame (dpyinfo,
						   event.xclient.window);

			if (f)
			  {
			    if (numchars == 0)
			      abort ();

			    bufp->kind = delete_window_event;
			    XSETFRAME (bufp->frame_or_window, f);
			    bufp->arg = Qnil;
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
		    struct frame *f
		      = x_window_to_frame (dpyinfo, event.xclient.window);

		    new_x = event.xclient.data.s[0];
		    new_y = event.xclient.data.s[1];

		    if (f)
		      {
			f->output_data.x->left_pos = new_x;
			f->output_data.x->top_pos = new_y;
		      }
		  }
#ifdef HACK_EDITRES
		else if (event.xclient.message_type
			 == dpyinfo->Xatom_editres)
		  {
		    struct frame *f
		      = x_any_window_to_frame (dpyinfo, event.xclient.window);
		    _XEditResCheckMessages (f->output_data.x->widget, NULL,
					    &event, NULL);
		  }
#endif /* HACK_EDITRES */
		else if ((event.xclient.message_type
			  == dpyinfo->Xatom_DONE)
			 || (event.xclient.message_type
			     == dpyinfo->Xatom_PAGE))
		  {
		    /* Ghostview job completed.  Kill it.  We could
		       reply with "Next" if we received "Page", but we
		       currently never do because we are interested in
		       images, only, which should have 1 page.  */
		    Pixmap pixmap = (Pixmap) event.xclient.data.l[1];
		    struct frame *f
		      = x_window_to_frame (dpyinfo, event.xclient.window);
		    x_kill_gs_process (pixmap, f);
		    expose_frame (f, 0, 0, 0, 0);
		  }
#ifdef USE_TOOLKIT_SCROLL_BARS
		/* Scroll bar callbacks send a ClientMessage from which
		   we construct an input_event.  */
		else if (event.xclient.message_type
			 == dpyinfo->Xatom_Scrollbar)
		  {
		    x_scroll_bar_to_input_event (&event, bufp);
		    ++bufp, ++count, --numchars;
		    goto out;
		  }
#endif /* USE_TOOLKIT_SCROLL_BARS */
		else
		  goto OTHER;
	      }
	      break;

	    case SelectionNotify:
#ifdef USE_X_TOOLKIT
	      if (! x_window_to_frame (dpyinfo, event.xselection.requestor))
		goto OTHER;
#endif /* not USE_X_TOOLKIT */
	      x_handle_selection_notify (&event.xselection);
	      break;

	    case SelectionClear:	/* Someone has grabbed ownership.  */
#ifdef USE_X_TOOLKIT
	      if (! x_window_to_frame (dpyinfo, event.xselectionclear.window))
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
		bufp->frame_or_window = Qnil;
		bufp->arg = Qnil;
		bufp++;

		count += 1;
		numchars -= 1;
	      }
	      break;

	    case SelectionRequest:	/* Someone wants our selection.  */
#ifdef USE_X_TOOLKIT
	      if (!x_window_to_frame (dpyinfo, event.xselectionrequest.owner))
		goto OTHER;
#endif /* USE_X_TOOLKIT */
	      if (x_queue_selection_requests)
		x_queue_event (x_window_to_frame (dpyinfo, event.xselectionrequest.owner),
			       &event);
	      else
		{
		  XSelectionRequestEvent *eventp
		    = (XSelectionRequestEvent *) &event;

		  if (numchars == 0)
		    abort ();

		  bufp->kind = selection_request_event;
		  SELECTION_EVENT_DISPLAY (bufp) = eventp->display;
		  SELECTION_EVENT_REQUESTOR (bufp) = eventp->requestor;
		  SELECTION_EVENT_SELECTION (bufp) = eventp->selection;
		  SELECTION_EVENT_TARGET (bufp) = eventp->target;
		  SELECTION_EVENT_PROPERTY (bufp) = eventp->property;
		  SELECTION_EVENT_TIME (bufp) = eventp->time;
		  bufp->frame_or_window = Qnil;
		  bufp->arg = Qnil;
		  bufp++;

		  count += 1;
		  numchars -= 1;
		}
	      break;

	    case PropertyNotify:
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
		  f->output_data.x->left_pos = x;
		  f->output_data.x->top_pos = y;
		}
	      break;

	    case Expose:
	      f = x_window_to_frame (dpyinfo, event.xexpose.window);
	      if (f)
		{
                  x_check_fullscreen (f);

		  if (f->async_visible == 0)
		    {
		      f->async_visible = 1;
		      f->async_iconified = 0;
		      f->output_data.x->has_been_visible = 1;
		      SET_FRAME_GARBAGED (f);
		    }
		  else
		    expose_frame (x_window_to_frame (dpyinfo,
						     event.xexpose.window),
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
		  bar = x_window_to_scroll_bar (event.xexpose.window);

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

		      bufp->kind = iconify_event;
		      XSETFRAME (bufp->frame_or_window, f);
		      bufp->arg = Qnil;
		      bufp++;
		      count++;
		      numchars--;
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
		  f->async_visible = 1;
		  f->async_iconified = 0;
		  f->output_data.x->has_been_visible = 1;

		  /* wait_reading_process_input will notice this and update
		     the frame's display structures.  */
		  SET_FRAME_GARBAGED (f);

		  if (f->iconified)
		    {
		      bufp->kind = deiconify_event;
		      XSETFRAME (bufp->frame_or_window, f);
		      bufp->arg = Qnil;
		      bufp++;
		      count++;
		      numchars--;
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
	      f = x_any_window_to_frame (dpyinfo, event.xkey.window);

	      if (!dpyinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
		{
		  dpyinfo->mouse_face_hidden = 1;
		  clear_mouse_face (dpyinfo);
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

		      nbytes = XmbLookupString (FRAME_XIC (f),
						&event.xkey, copy_bufptr,
						copy_bufsiz, &keysym,
						&status_return);
		      if (status_return == XBufferOverflow)
			{
			  copy_bufsiz = nbytes + 1;
			  copy_bufptr = (char *) alloca (copy_bufsiz);
			  nbytes = XmbLookupString (FRAME_XIC (f),
						    &event.xkey, copy_bufptr,
						    copy_bufsiz, &keysym,
						    &status_return);
			}

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

		  orig_keysym = keysym;

		  if (numchars > 1)
		    {
		      if (((keysym >= XK_BackSpace && keysym <= XK_Escape)
			   || keysym == XK_Delete
#ifdef XK_ISO_Left_Tab
			   || (keysym >= XK_ISO_Left_Tab && keysym <= XK_ISO_Enter)
#endif
			   || (keysym >= XK_Kanji && keysym <= XK_Eisu_toggle)
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
			   || (orig_keysym & (1 << 28))
			   || (keysym != NoSymbol && nbytes == 0))
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
			  bufp->arg = Qnil;
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
			  register int c;
			  int nchars, len;

			  for (i = 0; i < nbytes; i++)
			    {
			      if (temp_index == (sizeof temp_buffer
						 / sizeof (short)))
				temp_index = 0;
			      temp_buffer[temp_index++] = copy_bufptr[i];
			    }

			  if (/* If the event is not from XIM, */
			      event.xkey.keycode != 0
			      /* or the current locale doesn't request
				 decoding of the intup data, ... */
			      || coding.type == coding_type_raw_text
			      || coding.type == coding_type_no_conversion)
			    {
			      /* ... we can use the input data as is.  */
			      nchars = nbytes;
			    }
			  else
			    { 
			      /* We have to decode the input data.  */
			      int require;
			      unsigned char *p;

			      require = decoding_buffer_size (&coding, nbytes);
			      p = (unsigned char *) alloca (require);
			      coding.mode |= CODING_MODE_LAST_BLOCK;
			      decode_coding (&coding, copy_bufptr, p,
					     nbytes, require);
			      nbytes = coding.produced;
			      nchars = coding.produced_char;
			      copy_bufptr = p;
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
			      
			      bufp->kind = (SINGLE_BYTE_CHAR_P (c)
					    ? ascii_keystroke
					    : multibyte_char_keystroke);
			      bufp->code = c;
			      XSETFRAME (bufp->frame_or_window, f);
			      bufp->arg = Qnil;
			      bufp->modifiers
				= x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f),
							  modifiers);
			      bufp->timestamp = event.xkey.time;
			      bufp++;
			    }

			  count += nchars;
			  numchars -= nchars;

			  if (keysym == NoSymbol)
			    break;
			}
		      else
			abort ();
		    }
		  else
		    abort ();
		}
#ifdef HAVE_X_I18N
	      /* Don't dispatch this event since XtDispatchEvent calls
		 XFilterEvent, and two calls in a row may freeze the
		 client.  */
	      break;
#else
	      goto OTHER;
#endif

	    case KeyRelease:
#ifdef HAVE_X_I18N
	      /* Don't dispatch this event since XtDispatchEvent calls
		 XFilterEvent, and two calls in a row may freeze the
		 client.  */
	      break;
#else
	      goto OTHER;
#endif

	      /* Here's a possible interpretation of the whole
		 FocusIn-EnterNotify FocusOut-LeaveNotify mess.  If
		 you get a FocusIn event, you have to get a FocusOut
		 event before you relinquish the focus.  If you
		 haven't received a FocusIn event, then a mere
		 LeaveNotify is enough to free you.  */

	    case EnterNotify:
	      {
		f = x_any_window_to_frame (dpyinfo, event.xcrossing.window);

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
		goto OTHER;
	      }

	    case FocusIn:
	      f = x_any_window_to_frame (dpyinfo, event.xfocus.window);
	      if (event.xfocus.detail != NotifyPointer)
		dpyinfo->x_focus_event_frame = f;
	      if (f)
		{
		  x_new_focus_frame (dpyinfo, f);

		  /* Don't stop displaying the initial startup message
		     for a switch-frame event we don't need.  */
		  if (GC_NILP (Vterminal_frame)
		      && GC_CONSP (Vframe_list)
		      && !GC_NILP (XCDR (Vframe_list)))
		    {
		      bufp->kind = FOCUS_IN_EVENT;
		      XSETFRAME (bufp->frame_or_window, f);
		      bufp->arg = Qnil;
		      ++bufp, ++count, --numchars;
		    }
		}

#ifdef HAVE_X_I18N
	      if (f && FRAME_XIC (f))
		XSetICFocus (FRAME_XIC (f));
#endif

	      goto OTHER;

	    case LeaveNotify:
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
		    {
		      Lisp_Object frame;
		      int n;

		      XSETFRAME (frame, f);
		      help_echo = Qnil;
		      n = gen_help_event (bufp, numchars,
					  Qnil, frame, Qnil, Qnil, 0);
		      bufp += n, count += n, numchars -= n;
		    }

#if 0
		  if (event.xcrossing.focus)
		    x_mouse_leave (dpyinfo);
		  else
		    {
		      if (f == dpyinfo->x_focus_event_frame)
			dpyinfo->x_focus_event_frame = 0;
		      if (f == dpyinfo->x_focus_frame)
			x_new_focus_frame (dpyinfo, 0);
		    }
#endif
		}
	      goto OTHER;

	    case FocusOut:
	      f = x_any_window_to_frame (dpyinfo, event.xfocus.window);
	      if (event.xfocus.detail != NotifyPointer
		  && f == dpyinfo->x_focus_event_frame)
		dpyinfo->x_focus_event_frame = 0;
	      if (f && f == dpyinfo->x_focus_frame)
		x_new_focus_frame (dpyinfo, 0);

#ifdef HAVE_X_I18N
	      if (f && FRAME_XIC (f))
		XUnsetICFocus (FRAME_XIC (f));
#endif

	      goto OTHER;

	    case MotionNotify:
	      {
		previous_help_echo = help_echo;
		help_echo = help_echo_object = help_echo_window = Qnil;
		help_echo_pos = -1;
		
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
		  note_mouse_movement (f, &event.xmotion);
		else
		  {
#ifndef USE_TOOLKIT_SCROLL_BARS
		    struct scroll_bar *bar
		      = x_window_to_scroll_bar (event.xmotion.window);

		    if (bar)
		      x_scroll_bar_note_movement (bar, &event);
#endif /* USE_TOOLKIT_SCROLL_BARS */

		    /* If we move outside the frame, then we're
		       certainly no longer on any text in the frame.  */
		    clear_mouse_face (dpyinfo);
		  }

		/* If the contents of the global variable help_echo
		   has changed, generate a HELP_EVENT.  */
		if (!NILP (help_echo)
		    || !NILP (previous_help_echo))
		  {
		    Lisp_Object frame;
		    int n;

		    if (f)
		      XSETFRAME (frame, f);
		    else
		      frame = Qnil;

		    any_help_event_p = 1;
		    n = gen_help_event (bufp, numchars, help_echo, frame,
					help_echo_window, help_echo_object,
					help_echo_pos);
		    bufp += n, count += n, numchars -= n;
		  }
		
		goto OTHER;
	      }

	    case ConfigureNotify:
	      f = x_top_window_to_frame (dpyinfo, event.xconfigure.window);
	      if (f)
		{
#ifndef USE_X_TOOLKIT
                  /* If there is a pending resize for fullscreen, don't
                     do this one, the right one will come later.
		     The toolkit version doesn't seem to need this, but we
		     need to reset it below.  */
                  int dont_resize =
                    ((f->output_data.x->want_fullscreen & FULLSCREEN_WAIT)
                     && FRAME_NEW_WIDTH (f) != 0);
		  int rows = PIXEL_TO_CHAR_HEIGHT (f, event.xconfigure.height);
		  int columns = PIXEL_TO_CHAR_WIDTH (f, event.xconfigure.width);
                  if (dont_resize)
                    goto OTHER;
		  
		  /* In the toolkit version, change_frame_size
		     is called by the code that handles resizing
		     of the EmacsFrame widget.  */

		  /* Even if the number of character rows and columns has
		     not changed, the font size may have changed, so we need
		     to check the pixel dimensions as well.  */
		  if (columns != f->width
		      || rows != f->height
		      || event.xconfigure.width != f->output_data.x->pixel_width
		      || event.xconfigure.height != f->output_data.x->pixel_height)
		    {
		      change_frame_size (f, rows, columns, 0, 1, 0);
		      SET_FRAME_GARBAGED (f);
		      cancel_mouse_face (f);
		    }
#endif

		  f->output_data.x->pixel_width = event.xconfigure.width;
		  f->output_data.x->pixel_height = event.xconfigure.height;

		  /* What we have now is the position of Emacs's own window.
		     Convert that to the position of the window manager window.  */
		  x_real_positions (f, &f->output_data.x->left_pos,
				    &f->output_data.x->top_pos);

                  x_check_fullscreen_move(f);
                  if (f->output_data.x->want_fullscreen & FULLSCREEN_WAIT)
                    f->output_data.x->want_fullscreen &=
                      ~(FULLSCREEN_WAIT|FULLSCREEN_BOTH);
#ifdef HAVE_X_I18N
		  if (FRAME_XIC (f) && (FRAME_XIC_STYLE (f) & XIMStatusArea))
		    xic_set_statusarea (f);
#endif

		  if (f->output_data.x->parent_desc != FRAME_X_DISPLAY_INFO (f)->root_window)
		    {
		      /* Since the WM decorations come below top_pos now,
			 we must put them below top_pos in the future.  */
		      f->output_data.x->win_gravity = NorthWestGravity;
		      x_wm_set_size_hint (f, (long) 0, 0);
		    }
#ifdef USE_MOTIF
		  /* Some window managers pass (0,0) as the location of
		     the window, and the Motif event handler stores it
		     in the emacs widget, which messes up Motif menus.  */
		  if (event.xconfigure.x == 0 && event.xconfigure.y == 0)
		    {
		      event.xconfigure.x = f->output_data.x->widget->core.x;
		      event.xconfigure.y = f->output_data.x->widget->core.y;
		    }
#endif /* USE_MOTIF */
		}
	      goto OTHER;

	    case ButtonPress:
	    case ButtonRelease:
	      {
		/* If we decide we want to generate an event to be seen
		   by the rest of Emacs, we put it here.  */
		struct input_event emacs_event;
		int tool_bar_p = 0;
		
		emacs_event.kind = no_event;
		bzero (&compose_status, sizeof (compose_status));

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
			&& XFASTINT (XWINDOW (f->tool_bar_window)->height))
		      {
			Lisp_Object window;
			int p, x, y;

			x = event.xbutton.x;
			y = event.xbutton.y;

			/* Set x and y.  */
			window = window_from_coordinates (f, x, y, &p, 1);
			if (EQ (window, f->tool_bar_window))
			  {
			    x_handle_tool_bar_click (f, &event.xbutton);
			    tool_bar_p = 1;
			  }
		      }

		    if (!tool_bar_p)
		      if (!dpyinfo->x_focus_frame
			  || f == dpyinfo->x_focus_frame)
			construct_mouse_click (&emacs_event, &event, f);
		  }
		else
		  {
#ifndef USE_TOOLKIT_SCROLL_BARS
		    struct scroll_bar *bar
		      = x_window_to_scroll_bar (event.xbutton.window);

		    if (bar)
		      x_scroll_bar_handle_click (bar, &event, &emacs_event);
#endif /* not USE_TOOLKIT_SCROLL_BARS */
		  }

		if (event.type == ButtonPress)
		  {
		    dpyinfo->grabbed |= (1 << event.xbutton.button);
		    last_mouse_frame = f;
		    /* Ignore any mouse motion that happened
		       before this event; any subsequent mouse-movement
		       Emacs events should reflect only motion after
		       the ButtonPress.  */
		    if (f != 0)
		      f->mouse_moved = 0;
		    
		    if (!tool_bar_p)
		      last_tool_bar_item = -1;
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
		f = x_menubar_window_to_frame (dpyinfo, event.xbutton.window);
		/* For a down-event in the menu bar,
		   don't pass it to Xt right now.
		   Instead, save it away
		   and we will pass it to Xt from kbd_buffer_get_event.
		   That way, we can run some Lisp code first.  */
		if (f && event.type == ButtonPress
		    /* Verify the event is really within the menu bar
		       and not just sent to it due to grabbing.  */
		    && event.xbutton.x >= 0
		    && event.xbutton.x < f->output_data.x->pixel_width
		    && event.xbutton.y >= 0
		    && event.xbutton.y < f->output_data.x->menubar_height
		    && event.xbutton.same_screen)
		  {
		    SET_SAVED_BUTTON_EVENT;
		    XSETFRAME (last_mouse_press_frame, f);
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
#endif /* USE_X_TOOLKIT */
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
	      XtDispatchEvent (&event);
	      UNBLOCK_INPUT;
#endif /* USE_X_TOOLKIT */
	      break;
	    }
	}
    }

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

  UNBLOCK_INPUT;
  --handling_signal;
  return count;
}




/***********************************************************************
			     Text Cursor
 ***********************************************************************/

/* Notice when the text cursor of window W has been completely
   overwritten by a drawing operation that outputs glyphs in AREA
   starting at X0 and ending at X1 in the line starting at Y0 and
   ending at Y1.  X coordinates are area-relative.  X1 < 0 means all
   the rest of the line after X0 has been written.  Y coordinates
   are window-relative.  */

static void
notice_overwritten_cursor (w, area, x0, x1, y0, y1)
     struct window *w;
     enum glyph_row_area area;
     int x0, y0, x1, y1;
{
  if (area == TEXT_AREA
      && w->phys_cursor_on_p
      && y0 <= w->phys_cursor.y
      && y1 >= w->phys_cursor.y + w->phys_cursor_height
      && x0 <= w->phys_cursor.x
      && (x1 < 0 || x1 > w->phys_cursor.x))
    w->phys_cursor_on_p = 0;
}


/* Set clipping for output in glyph row ROW.  W is the window in which
   we operate.  GC is the graphics context to set clipping in.
   WHOLE_LINE_P non-zero means include the areas used for truncation
   mark display and alike in the clipping rectangle.

   ROW may be a text row or, e.g., a mode line.  Text rows must be
   clipped to the interior of the window dedicated to text display,
   mode lines must be clipped to the whole window.  */

static void
x_clip_to_row (w, row, gc, whole_line_p)
     struct window *w;
     struct glyph_row *row;
     GC gc;
     int whole_line_p;
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  XRectangle clip_rect;
  int window_x, window_y, window_width, window_height;

  window_box (w, -1, &window_x, &window_y, &window_width, &window_height);

  clip_rect.x = WINDOW_TO_FRAME_PIXEL_X (w, 0);
  clip_rect.y = WINDOW_TO_FRAME_PIXEL_Y (w, row->y);
  clip_rect.y = max (clip_rect.y, window_y);
  clip_rect.width = window_width;
  clip_rect.height = row->visible_height;

  /* If clipping to the whole line, including trunc marks, extend
     the rectangle to the left and increase its width.  */
  if (whole_line_p)
    {
      clip_rect.x -= FRAME_X_LEFT_FRINGE_WIDTH (f);
      clip_rect.width += FRAME_X_FRINGE_WIDTH (f);
    }

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

  /* Compute frame-relative coordinates from window-relative
     coordinates.  */
  x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
  y = (WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y)
       + row->ascent - w->phys_cursor_ascent);
  h = row->height - 1;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute the width of the rectangle to draw.  If on a stretch
     glyph, and `x-stretch-block-cursor' is nil, don't draw a
     rectangle as wide as the glyph, but use a canonical character
     width instead.  */
  wd = cursor_glyph->pixel_width - 1;
  if (cursor_glyph->type == STRETCH_GLYPH
      && !x_stretch_cursor_p)
    wd = min (CANON_X_UNIT (f), wd);
  
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
  x_clip_to_row (w, row, gc, 0);
  XDrawRectangle (dpy, FRAME_X_WINDOW (f), gc, x, y, wd, h);
  XSetClipMask (dpy, gc, None);
}


/* Draw a bar cursor on window W in glyph row ROW.

   Implementation note: One would like to draw a bar cursor with an
   angle equal to the one given by the font property XA_ITALIC_ANGLE.
   Unfortunately, I didn't find a font yet that has this property set.
   --gerd.  */

static void
x_draw_bar_cursor (w, row, width)
     struct window *w;
     struct glyph_row *row;
     int width;
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
      x_draw_phys_cursor_glyph (w, row, DRAW_CURSOR);
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
	width = f->output_data.x->cursor_width;
      width = min (cursor_glyph->pixel_width, width);
  
      x_clip_to_row (w, row, gc, 0);
      XFillRectangle (dpy, window, gc,
		      WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x),
		      WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
		      width, row->height);
      XSetClipMask (dpy, gc, None);
    }
}


/* Clear the cursor of window W to background color, and mark the
   cursor as not shown.  This is used when the text where the cursor
   is is about to be rewritten.  */

static void
x_clear_cursor (w)
     struct window *w;
{
  if (FRAME_VISIBLE_P (XFRAME (w->frame)) && w->phys_cursor_on_p)
    x_update_window_cursor (w, 0);
}


/* Draw the cursor glyph of window W in glyph row ROW.  See the
   comment of x_draw_glyphs for the meaning of HL.  */

static void
x_draw_phys_cursor_glyph (w, row, hl)
     struct window *w;
     struct glyph_row *row;
     enum draw_glyphs_face hl;
{
  /* If cursor hpos is out of bounds, don't draw garbage.  This can
     happen in mini-buffer windows when switching between echo area
     glyphs and mini-buffer.  */
  if (w->phys_cursor.hpos < row->used[TEXT_AREA])
    {
      int on_p = w->phys_cursor_on_p;
      
      x_draw_glyphs (w, w->phys_cursor.x, row, TEXT_AREA,
		     w->phys_cursor.hpos, w->phys_cursor.hpos + 1,
		     hl, 0);
      w->phys_cursor_on_p = on_p;

      /* When we erase the cursor, and ROW is overlapped by other
	 rows, make sure that these overlapping parts of other rows
	 are redrawn.  */
      if (hl == DRAW_NORMAL_TEXT && row->overlapped_p)
	{
	  if (row > w->current_matrix->rows
	      && MATRIX_ROW_OVERLAPS_SUCC_P (row - 1))
	    x_fix_overlapping_area (w, row - 1, TEXT_AREA);

	  if (MATRIX_ROW_BOTTOM_Y (row) < window_text_bottom_y (w)
	      && MATRIX_ROW_OVERLAPS_PRED_P (row + 1))
	    x_fix_overlapping_area (w, row + 1, TEXT_AREA);
	}
    }
}


/* Erase the image of a cursor of window W from the screen.  */

static void
x_erase_phys_cursor (w)
     struct window *w;
{
  struct frame *f = XFRAME (w->frame);
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  int hpos = w->phys_cursor.hpos;
  int vpos = w->phys_cursor.vpos;
  int mouse_face_here_p = 0;
  struct glyph_matrix *active_glyphs = w->current_matrix;
  struct glyph_row *cursor_row;
  struct glyph *cursor_glyph;
  enum draw_glyphs_face hl;

  /* No cursor displayed or row invalidated => nothing to do on the
     screen.  */
  if (w->phys_cursor_type == NO_CURSOR)
    goto mark_cursor_off;
	   
  /* VPOS >= active_glyphs->nrows means that window has been resized.
     Don't bother to erase the cursor.  */
  if (vpos >= active_glyphs->nrows)
    goto mark_cursor_off;

  /* If row containing cursor is marked invalid, there is nothing we
     can do.  */
  cursor_row = MATRIX_ROW (active_glyphs, vpos);
  if (!cursor_row->enabled_p)
    goto mark_cursor_off;
  
  /* If row is completely invisible, don't attempt to delete a cursor which
     isn't there.  This can happen if cursor is at top of a window, and
     we switch to a buffer with a header line in that window.  */
  if (cursor_row->visible_height <= 0)
    goto mark_cursor_off;
  
  /* This can happen when the new row is shorter than the old one.
     In this case, either x_draw_glyphs or clear_end_of_line
     should have cleared the cursor.  Note that we wouldn't be
     able to erase the cursor in this case because we don't have a
     cursor glyph at hand.  */
  if (w->phys_cursor.hpos >= cursor_row->used[TEXT_AREA])
    goto mark_cursor_off;
	 
  /* If the cursor is in the mouse face area, redisplay that when
     we clear the cursor.  */
  if (! NILP (dpyinfo->mouse_face_window)
      && w == XWINDOW (dpyinfo->mouse_face_window)
      && (vpos > dpyinfo->mouse_face_beg_row
	  || (vpos == dpyinfo->mouse_face_beg_row
	      && hpos >= dpyinfo->mouse_face_beg_col))
      && (vpos < dpyinfo->mouse_face_end_row
	  || (vpos == dpyinfo->mouse_face_end_row
	      && hpos < dpyinfo->mouse_face_end_col))
      /* Don't redraw the cursor's spot in mouse face if it is at the
	 end of a line (on a newline).  The cursor appears there, but
	 mouse highlighting does not.  */
      && cursor_row->used[TEXT_AREA] > hpos)
    mouse_face_here_p = 1;

  /* Maybe clear the display under the cursor.  */
  if (w->phys_cursor_type == HOLLOW_BOX_CURSOR)
    {
      int x;
      int header_line_height = WINDOW_DISPLAY_HEADER_LINE_HEIGHT (w);

      cursor_glyph = get_phys_cursor_glyph (w);
      if (cursor_glyph == NULL)
	goto mark_cursor_off;

      x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);
      
      x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    x,
		    WINDOW_TO_FRAME_PIXEL_Y (w, max (header_line_height,
						     cursor_row->y)),
		    cursor_glyph->pixel_width,
		    cursor_row->visible_height,
		    False);
    }
  
  /* Erase the cursor by redrawing the character underneath it.  */
  if (mouse_face_here_p)
    hl = DRAW_MOUSE_FACE;
  else
    hl = DRAW_NORMAL_TEXT;
  x_draw_phys_cursor_glyph (w, cursor_row, hl);

 mark_cursor_off:
  w->phys_cursor_on_p = 0;
  w->phys_cursor_type = NO_CURSOR;
}


/* Non-zero if physical cursor of window W is within mouse face.  */

static int
cursor_in_mouse_face_p (w)
     struct window *w;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (XFRAME (w->frame));
  int in_mouse_face = 0;
  
  if (WINDOWP (dpyinfo->mouse_face_window)
      && XWINDOW (dpyinfo->mouse_face_window) == w)
    {
      int hpos = w->phys_cursor.hpos;
      int vpos = w->phys_cursor.vpos;

      if (vpos >= dpyinfo->mouse_face_beg_row
	  && vpos <= dpyinfo->mouse_face_end_row
	  && (vpos > dpyinfo->mouse_face_beg_row
	      || hpos >= dpyinfo->mouse_face_beg_col)
	  && (vpos < dpyinfo->mouse_face_end_row
	      || hpos < dpyinfo->mouse_face_end_col
	      || dpyinfo->mouse_face_past_end))
	in_mouse_face = 1;
    }

  return in_mouse_face;
}


/* Display or clear cursor of window W.  If ON is zero, clear the
   cursor.  If it is non-zero, display the cursor.  If ON is nonzero,
   where to put the cursor is specified by HPOS, VPOS, X and Y.  */

void
x_display_and_set_cursor (w, on, hpos, vpos, x, y)
     struct window *w;
     int on, hpos, vpos, x, y;
{
  struct frame *f = XFRAME (w->frame);
  int new_cursor_type;
  int new_cursor_width;
  struct glyph_matrix *current_glyphs;
  struct glyph_row *glyph_row;
  struct glyph *glyph;
  int cursor_non_selected;

  /* This is pointless on invisible frames, and dangerous on garbaged
     windows and frames; in the latter case, the frame or window may
     be in the midst of changing its size, and x and y may be off the
     window.  */
  if (! FRAME_VISIBLE_P (f)
      || FRAME_GARBAGED_P (f)
      || vpos >= w->current_matrix->nrows
      || hpos >= w->current_matrix->matrix_w)
    return;

  /* If cursor is off and we want it off, return quickly.  */
  if (!on && !w->phys_cursor_on_p)
    return;

  current_glyphs = w->current_matrix;
  glyph_row = MATRIX_ROW (current_glyphs, vpos);
  glyph = glyph_row->glyphs[TEXT_AREA] + hpos;
  
  /* If cursor row is not enabled, we don't really know where to 
     display the cursor.  */
  if (!glyph_row->enabled_p)
    {
      w->phys_cursor_on_p = 0;
      return;
    }

  xassert (interrupt_input_blocked);

  /* Set new_cursor_type to the cursor we want to be displayed.  In a
     mini-buffer window, we want the cursor only to appear if we are
     reading input from this window.  For the selected window, we want
     the cursor type given by the frame parameter.  If explicitly
     marked off, draw no cursor.  In all other cases, we want a hollow
     box cursor.  */
  cursor_non_selected 
    = !NILP (Fbuffer_local_value (Qcursor_in_non_selected_windows,
				  w->buffer));
  new_cursor_width = -1;
  if (cursor_in_echo_area
      && FRAME_HAS_MINIBUF_P (f)
      && EQ (FRAME_MINIBUF_WINDOW (f), echo_area_window))
    {
      if (w == XWINDOW (echo_area_window))
	new_cursor_type = FRAME_DESIRED_CURSOR (f);
      else if (cursor_non_selected)
	new_cursor_type = HOLLOW_BOX_CURSOR;
      else
	new_cursor_type = NO_CURSOR;
    }
  else
    {
      if (f != FRAME_X_DISPLAY_INFO (f)->x_highlight_frame
	  || w != XWINDOW (f->selected_window))
	{
	  if ((MINI_WINDOW_P (w) && minibuf_level == 0)
	      || !cursor_non_selected
	      || NILP (XBUFFER (w->buffer)->cursor_type))
	    new_cursor_type = NO_CURSOR;
	  else
	    new_cursor_type = HOLLOW_BOX_CURSOR;
	}
      else
	{
	  struct buffer *b = XBUFFER (w->buffer);

	  if (EQ (b->cursor_type, Qt))
	    new_cursor_type = FRAME_DESIRED_CURSOR (f);
	  else
	    new_cursor_type = x_specified_cursor_type (b->cursor_type, 
						       &new_cursor_width);
	  if (w->cursor_off_p)
	    {
	      if (new_cursor_type == FILLED_BOX_CURSOR)
		new_cursor_type = HOLLOW_BOX_CURSOR;
	      else if (new_cursor_type == BAR_CURSOR && new_cursor_width > 1)
		new_cursor_width = 1;
	      else
		new_cursor_type = NO_CURSOR;
	    }
	}
    }

  /* If cursor is currently being shown and we don't want it to be or
     it is in the wrong place, or the cursor type is not what we want,
     erase it.  */
  if (w->phys_cursor_on_p
      && (!on
	  || w->phys_cursor.x != x
	  || w->phys_cursor.y != y
	  || new_cursor_type != w->phys_cursor_type
	  || (new_cursor_type == BAR_CURSOR
	      && new_cursor_width != w->phys_cursor_width)))
    x_erase_phys_cursor (w);

  /* If the cursor is now invisible and we want it to be visible,
     display it.  */
  if (on && !w->phys_cursor_on_p)
    {
      w->phys_cursor_ascent = glyph_row->ascent;
      w->phys_cursor_height = glyph_row->height;
      
      /* Set phys_cursor_.* before x_draw_.* is called because some
	 of them may need the information.  */
      w->phys_cursor.x = x;
      w->phys_cursor.y = glyph_row->y;
      w->phys_cursor.hpos = hpos;
      w->phys_cursor.vpos = vpos;
      w->phys_cursor_type = new_cursor_type;
      w->phys_cursor_width = new_cursor_width;
      w->phys_cursor_on_p = 1;

      switch (new_cursor_type)
	{
	case HOLLOW_BOX_CURSOR:
	  x_draw_hollow_cursor (w, glyph_row);
	  break;

	case FILLED_BOX_CURSOR:
	  x_draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	  break;

	case BAR_CURSOR:
	  x_draw_bar_cursor (w, glyph_row, new_cursor_width);
	  break;

	case NO_CURSOR:
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
  if (updating_frame != f)
    XFlush (FRAME_X_DISPLAY (f));
#endif
}


/* Display the cursor on window W, or clear it.  X and Y are window
   relative pixel coordinates.  HPOS and VPOS are glyph matrix
   positions.  If W is not the selected window, display a hollow
   cursor.  ON non-zero means display the cursor at X, Y which
   correspond to HPOS, VPOS, otherwise it is cleared.  */

void
x_display_cursor (w, on, hpos, vpos, x, y)
     struct window *w;
     int on, hpos, vpos, x, y;
{
  BLOCK_INPUT;
  x_display_and_set_cursor (w, on, hpos, vpos, x, y);
  UNBLOCK_INPUT;
}


/* Display the cursor on window W, or clear it, according to ON_P.
   Don't change the cursor's position.  */

void
x_update_cursor (f, on_p)
     struct frame *f;
{
  x_update_cursor_in_window_tree (XWINDOW (f->root_window), on_p);
}


/* Call x_update_window_cursor with parameter ON_P on all leaf windows
   in the window tree rooted at W.  */

static void
x_update_cursor_in_window_tree (w, on_p)
     struct window *w;
     int on_p;
{
  while (w)
    {
      if (!NILP (w->hchild))
	x_update_cursor_in_window_tree (XWINDOW (w->hchild), on_p);
      else if (!NILP (w->vchild))
	x_update_cursor_in_window_tree (XWINDOW (w->vchild), on_p);
      else
	x_update_window_cursor (w, on_p);

      w = NILP (w->next) ? 0 : XWINDOW (w->next);
    }
}


/* Switch the display of W's cursor on or off, according to the value
   of ON.  */

static void
x_update_window_cursor (w, on)
     struct window *w;
     int on;
{
  /* Don't update cursor in windows whose frame is in the process
     of being deleted.  */
  if (w->current_matrix)
    {
      BLOCK_INPUT;
      x_display_and_set_cursor (w, on, w->phys_cursor.hpos, w->phys_cursor.vpos,
				w->phys_cursor.x, w->phys_cursor.y);
      UNBLOCK_INPUT;
    }
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

  /* Free up our existing icon bitmap if any.  */
  if (f->output_data.x->icon_bitmap > 0)
    x_destroy_bitmap (f, f->output_data.x->icon_bitmap);
  f->output_data.x->icon_bitmap = 0;

  if (STRINGP (file))
    bitmap_id = x_create_bitmap_from_file (f, file);
  else
    {
      /* Create the GNU bitmap if necessary.  */
      if (FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id < 0)
	FRAME_X_DISPLAY_INFO (f)->icon_bitmap_id
	  = x_create_bitmap_from_data (f, gnu_bits,
				       gnu_width, gnu_height);

      /* The first time we create the GNU bitmap,
	 this increments the ref-count one extra time.
	 As a result, the GNU bitmap is never freed.
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

#ifdef HAVE_X11R4
  {
    XTextProperty text;
    text.value = (unsigned char *) icon_name;
    text.encoding = XA_STRING;
    text.format = 8;
    text.nitems = strlen (icon_name);
#ifdef USE_X_TOOLKIT
    XSetWMIconName (FRAME_X_DISPLAY (f), XtWindow (f->output_data.x->widget),
		    &text);
#else /* not USE_X_TOOLKIT */
    XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &text);
#endif /* not USE_X_TOOLKIT */
  }
#else /* not HAVE_X11R4 */
  XSetIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), icon_name);
#endif /* not HAVE_X11R4 */

  if (f->output_data.x->icon_bitmap > 0)
    x_destroy_bitmap (f, f->output_data.x->icon_bitmap);
  f->output_data.x->icon_bitmap = 0;
  x_wm_set_icon_pixmap (f, 0);

  return 0;
}

#define X_ERROR_MESSAGE_SIZE 200

/* If non-nil, this should be a string.
   It means catch X errors  and store the error message in this string.  */

static Lisp_Object x_error_message_string;

/* An X error handler which stores the error message in
   x_error_message_string.  This is called from x_error_handler if
   x_catch_errors is in effect.  */

static void
x_error_catcher (display, error)
     Display *display;
     XErrorEvent *error;
{
  XGetErrorText (display, error->error_code,
		 XSTRING (x_error_message_string)->data,
		 X_ERROR_MESSAGE_SIZE);
}

/* Begin trapping X errors for display DPY.  Actually we trap X errors
   for all displays, but DPY should be the display you are actually
   operating on.

   After calling this function, X protocol errors no longer cause
   Emacs to exit; instead, they are recorded in the string
   stored in x_error_message_string.

   Calling x_check_errors signals an Emacs error if an X error has
   occurred since the last call to x_catch_errors or x_check_errors.

   Calling x_uncatch_errors resumes the normal error handling.  */

void x_check_errors ();
static Lisp_Object x_catch_errors_unwind ();

int
x_catch_errors (dpy)
     Display *dpy;
{
  int count = specpdl_ptr - specpdl;

  /* Make sure any errors from previous requests have been dealt with.  */
  XSync (dpy, False);

  record_unwind_protect (x_catch_errors_unwind, x_error_message_string);

  x_error_message_string = make_uninit_string (X_ERROR_MESSAGE_SIZE);
  XSTRING (x_error_message_string)->data[0] = 0;

  return count;
}

/* Unbind the binding that we made to check for X errors.  */

static Lisp_Object
x_catch_errors_unwind (old_val)
     Lisp_Object old_val;
{
  x_error_message_string = old_val;
  return Qnil;
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

  if (XSTRING (x_error_message_string)->data[0])
    error (format, XSTRING (x_error_message_string)->data);
}

/* Nonzero if we had any X protocol errors
   since we did x_catch_errors on DPY.  */

int
x_had_errors_p (dpy)
     Display *dpy;
{
  /* Make sure to catch any errors incurred so far.  */
  XSync (dpy, False);

  return XSTRING (x_error_message_string)->data[0] != 0;
}

/* Forget about any errors we have had, since we did x_catch_errors on DPY.  */

void
x_clear_errors (dpy)
     Display *dpy;
{
  XSTRING (x_error_message_string)->data[0] = 0;
}

/* Stop catching X protocol errors and let them make Emacs die.
   DPY should be the display that was passed to x_catch_errors.
   COUNT should be the value that was returned by
   the corresponding call to x_catch_errors.  */

void
x_uncatch_errors (dpy, count)
     Display *dpy;
     int count;
{
  unbind_to (count, Qnil);
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
  int count;
  
  error_msg = (char *) alloca (strlen (error_message) + 1);
  strcpy (error_msg, error_message);
  handling_signal = 0;
  
  /* Prevent being called recursively because of an error condition
     below.  Otherwise, we might end up with printing ``can't find per
     display information'' in the recursive call instead of printing
     the original message here.  */
  count = x_catch_errors (dpy);
  
  /* We have to close the display to inform Xt that it doesn't
     exist anymore.  If we don't, Xt will continue to wait for
     events from the display.  As a consequence, a sequence of

     M-x make-frame-on-display RET :1 RET
     ...kill the new frame, so that we get an IO error...
     M-x make-frame-on-display RET :1 RET

     will indefinitely wait in Xt for events for display `:1', opened
     in the first class to make-frame-on-display.

     Closing the display is reported to lead to a bus error on
     OpenWindows in certain situations.  I suspect that is a bug
     in OpenWindows.  I don't know how to cicumvent it here.  */
  
#ifdef USE_X_TOOLKIT
  /* If DPYINFO is null, this means we didn't open the display
     in the first place, so don't try to close it.  */
  if (dpyinfo)
    {
      extern void (*fatal_error_signal_hook) P_ ((void));
      fatal_error_signal_hook = x_fatal_error_signal;
      XtCloseDisplay (dpy);
      fatal_error_signal_hook = NULL;
    }
#endif

  /* Indicate that this display is dead.  */
  if (dpyinfo)
    dpyinfo->display = 0;

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
	Fdelete_frame (frame, Qt);
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
	Fdelete_frame (frame, Qt);
      }

  if (dpyinfo)
    x_delete_display (dpyinfo);

  x_uncatch_errors (dpy, count);
  
  if (x_display_list == 0)
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

  clear_waiting_for_input ();
  error ("%s", error_msg);
}


/* This is the usual handler for X protocol errors.
   It kills all frames on the display that we got the error for.
   If that was the only one, it prints an error message and kills Emacs.  */

static void
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


/* This is the first-level handler for X protocol errors.
   It calls x_error_quitter or x_error_catcher.  */

static int
x_error_handler (display, error)
     Display *display;
     XErrorEvent *error;
{
  if (! NILP (x_error_message_string))
    x_error_catcher (display, error);
  else
    x_error_quitter (display, error);
  return 0;
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
    = FS_LOAD_FONT (f, 0, fontname, -1);

  if (!fontp)
    return Qnil;

  f->output_data.x->font = (XFontStruct *) (fontp->font);
  f->output_data.x->baseline_offset = fontp->baseline_offset;
  f->output_data.x->fontset = -1;

  x_compute_fringe_widths (f, 1);

  /* Compute the scroll bar width in character columns.  */
  if (f->scroll_bar_pixel_width > 0)
    {
      int wid = FONT_WIDTH (f->output_data.x->font);
      f->scroll_bar_cols = (f->scroll_bar_pixel_width + wid-1) / wid;
    }
  else
    {
      int wid = FONT_WIDTH (f->output_data.x->font);
      f->scroll_bar_cols = (14 + wid - 1) / wid;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_X_WINDOW (f) != 0)
    {
      XSetFont (FRAME_X_DISPLAY (f), f->output_data.x->normal_gc,
		f->output_data.x->font->fid);
      XSetFont (FRAME_X_DISPLAY (f), f->output_data.x->reverse_gc,
		f->output_data.x->font->fid);
      XSetFont (FRAME_X_DISPLAY (f), f->output_data.x->cursor_gc,
		f->output_data.x->font->fid);

      frame_update_line_height (f);

      /* Don't change the size of a tip frame; there's no point in
	 doing it because it's done in Fx_show_tip, and it leads to
	 problems because the tip frame has no widget.  */
      if (NILP (tip_frame) || XFRAME (tip_frame) != f)
	x_set_window_size (f, 0, f->width, f->height);
    }
  else
    /* If we are setting a new frame's font for the first time,
       there are no faces yet, so this font's height is the line height.  */
    f->output_data.x->line_height = FONT_HEIGHT (f->output_data.x->font);

  return build_string (fontp->full_name);
}

/* Give frame F the fontset named FONTSETNAME as its default font, and
   return the full name of that fontset.  FONTSETNAME may be a wildcard
   pattern; in that case, we choose some fontset that fits the pattern.
   The return value shows which fontset we chose.  */

Lisp_Object
x_new_fontset (f, fontsetname)
     struct frame *f;
     char *fontsetname;
{
  int fontset = fs_query_fontset (build_string (fontsetname), 0);
  Lisp_Object result;

  if (fontset < 0)
    return Qnil;

  if (f->output_data.x->fontset == fontset)
    /* This fontset is already set in frame F.  There's nothing more
       to do.  */
    return fontset_name (fontset);

  result = x_new_font (f, (XSTRING (fontset_ascii (fontset))->data));

  if (!STRINGP (result))
    /* Can't load ASCII font.  */
    return Qnil;

  /* Since x_new_font doesn't update any fontset information, do it now.  */
  f->output_data.x->fontset = fontset;

#ifdef HAVE_X_I18N
  if (FRAME_XIC (f)
      && (FRAME_XIC_STYLE (f) & (XIMPreeditPosition | XIMStatusArea)))
    xic_set_xfontset (f, XSTRING (fontset_ascii (fontset))->data);
#endif
  
  return build_string (fontsetname);
}

/* Compute actual fringe widths */

void
x_compute_fringe_widths (f, redraw)
     struct frame *f;
     int redraw;
{
  int o_left = f->output_data.x->left_fringe_width;
  int o_right = f->output_data.x->right_fringe_width;
  int o_cols = f->output_data.x->fringe_cols;

  Lisp_Object left_fringe = Fassq (Qleft_fringe, f->param_alist);
  Lisp_Object right_fringe = Fassq (Qright_fringe, f->param_alist);
  int left_fringe_width, right_fringe_width;

  if (!NILP (left_fringe))
    left_fringe = Fcdr (left_fringe);
  if (!NILP (right_fringe))
    right_fringe = Fcdr (right_fringe);

  left_fringe_width = ((NILP (left_fringe) || !INTEGERP (left_fringe)) ? 8 :
		       XINT (left_fringe));
  right_fringe_width = ((NILP (right_fringe) || !INTEGERP (right_fringe)) ? 8 :
			XINT (right_fringe));

  if (left_fringe_width || right_fringe_width)
    {
      int left_wid = left_fringe_width >= 0 ? left_fringe_width : -left_fringe_width;
      int right_wid = right_fringe_width >= 0 ? right_fringe_width : -right_fringe_width;
      int conf_wid = left_wid + right_wid;
      int font_wid = FONT_WIDTH (f->output_data.x->font);
      int cols = (left_wid + right_wid + font_wid-1) / font_wid;
      int real_wid = cols * font_wid;
      if (left_wid && right_wid)
	{
	  if (left_fringe_width < 0)
	    {
	      /* Left fringe width is fixed, adjust right fringe if necessary */
	      f->output_data.x->left_fringe_width = left_wid;
	      f->output_data.x->right_fringe_width = real_wid - left_wid;
	    }
	  else if (right_fringe_width < 0)
	    {
	      /* Right fringe width is fixed, adjust left fringe if necessary */
	      f->output_data.x->left_fringe_width = real_wid - right_wid;
	      f->output_data.x->right_fringe_width = right_wid;
	    }
	  else
	    {
	      /* Adjust both fringes with an equal amount.
		 Note that we are doing integer arithmetic here, so don't
		 lose a pixel if the total width is an odd number.  */
	      int fill = real_wid - conf_wid;
	      f->output_data.x->left_fringe_width = left_wid + fill/2;
	      f->output_data.x->right_fringe_width = right_wid + fill - fill/2;
	    }
	}
      else if (left_fringe_width)
	{
	  f->output_data.x->left_fringe_width = real_wid;
	  f->output_data.x->right_fringe_width = 0;
	}
      else
	{
	  f->output_data.x->left_fringe_width = 0;
	  f->output_data.x->right_fringe_width = real_wid;
	}
      f->output_data.x->fringe_cols = cols;
      f->output_data.x->fringes_extra = real_wid;
    }
  else
    {
      f->output_data.x->left_fringe_width = 0;
      f->output_data.x->right_fringe_width = 0;
      f->output_data.x->fringe_cols = 0;
      f->output_data.x->fringes_extra = 0;
    }

  if (redraw && FRAME_VISIBLE_P (f))
    if (o_left != f->output_data.x->left_fringe_width ||
	o_right != f->output_data.x->right_fringe_width ||
	o_cols != f->output_data.x->fringe_cols)
      redraw_frame (f);
}

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
      if (FRAME_X_DISPLAY_INFO (f) == dpyinfo)
	{
	  FRAME_XIC (f) = NULL;
	  if (FRAME_XIC_FONTSET (f))
	    {
	      XFreeFontSet (FRAME_X_DISPLAY (f), FRAME_XIC_FONTSET (f));
	      FRAME_XIC_FONTSET (f) = NULL;
	    }
	}
    }
  
  /* No need to call XCloseIM.  */
  dpyinfo->xim = NULL;
  XFree (dpyinfo->xim_styles);
  UNBLOCK_INPUT;
}

#endif /* HAVE_X11R6 */

/* Open the connection to the XIM server on display DPYINFO.
   RESOURCE_NAME is the resource name Emacs uses.  */

static void
xim_open_dpy (dpyinfo, resource_name)
     struct x_display_info *dpyinfo;
     char *resource_name;
{
#ifdef USE_XIM
  XIM xim;

  xim = XOpenIM (dpyinfo->display, dpyinfo->xrdb, resource_name, EMACS_CLASS);
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
      /* This isn't prototyped in OSF 5.0.  */
      XSetIMValues (xim, XNDestroyCallback, &destroy, NULL);
#endif
    }
  
#else /* not USE_XIM */
  dpyinfo->xim = NULL;
#endif /* not USE_XIM */
}


#ifdef HAVE_X11R6_XIM

struct xim_inst_t
{
  struct x_display_info *dpyinfo;
  char *resource_name;
};

/* XIM instantiate callback function, which is called whenever an XIM
   server is available.  DISPLAY is teh display of the XIM.
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
	  
	  if (FRAME_X_DISPLAY_INFO (f) == xim_inst->dpyinfo)
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
#ifdef USE_XIM
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
				  /* Fixme: This is XPointer in
				     XFree86 but (XPointer *) on
				     Tru64, at least.  */
				  (XPointer) xim_inst);
#else /* not HAVE_X11R6_XIM */
  dpyinfo->xim = NULL;
  xim_open_dpy (dpyinfo, resource_name);
#endif /* not HAVE_X11R6_XIM */
  
#else /* not USE_XIM */
  dpyinfo->xim = NULL;
#endif /* not USE_XIM */
}


/* Close the connection to the XIM server on display DPYINFO. */

static void
xim_close_dpy (dpyinfo)
     struct x_display_info *dpyinfo;
{
#ifdef USE_XIM
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
#endif /* USE_XIM */
}

#endif /* not HAVE_X11R6_XIM */



/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

void
x_calc_absolute_position (f)
     struct frame *f;
{
  Window child;
  int win_x = 0, win_y = 0;
  int flags = f->output_data.x->size_hint_flags;
  int this_window;

  /* We have nothing to do if the current position
     is already for the top-left corner.  */
  if (! ((flags & XNegative) || (flags & YNegative)))
    return;

#ifdef USE_X_TOOLKIT
  this_window = XtWindow (f->output_data.x->widget);
#else
  this_window = FRAME_X_WINDOW (f);
#endif

  /* Find the position of the outside upper-left corner of
     the inner window, with respect to the outer window.
     But do this only if we will need the results.  */
  if (f->output_data.x->parent_desc != FRAME_X_DISPLAY_INFO (f)->root_window)
    {
      int count;

      BLOCK_INPUT;
      count = x_catch_errors (FRAME_X_DISPLAY (f));
      while (1)
	{
	  x_clear_errors (FRAME_X_DISPLAY (f));
	  XTranslateCoordinates (FRAME_X_DISPLAY (f),

				 /* From-window, to-window.  */
				 this_window,
				 f->output_data.x->parent_desc,

				 /* From-position, to-position.  */
				 0, 0, &win_x, &win_y,

				 /* Child of win.  */
				 &child);
	  if (x_had_errors_p (FRAME_X_DISPLAY (f)))
	    {
	      Window newroot, newparent = 0xdeadbeef;
	      Window *newchildren;
	      unsigned int nchildren;

	      if (! XQueryTree (FRAME_X_DISPLAY (f), this_window, &newroot,
				&newparent, &newchildren, &nchildren))
		break;

	      XFree ((char *) newchildren);

	      f->output_data.x->parent_desc = newparent;
	    }
	  else
	    break;
	}

      x_uncatch_errors (FRAME_X_DISPLAY (f), count);
      UNBLOCK_INPUT;
    }

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->output_data.x->left_pos = (FRAME_X_DISPLAY_INFO (f)->width
				  - 2 * f->output_data.x->border_width - win_x
				  - PIXEL_WIDTH (f)
				  + f->output_data.x->left_pos);

  {
    int height = PIXEL_HEIGHT (f);

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
    f->output_data.x->top_pos = (FRAME_X_DISPLAY_INFO (f)->height
				 - 2 * f->output_data.x->border_width
				 - win_y
				 - height
				 + f->output_data.x->top_pos);
  }
  
  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->output_data.x->size_hint_flags &= ~ (XNegative | YNegative);
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
      f->output_data.x->top_pos = yoff;
      f->output_data.x->left_pos = xoff;
      f->output_data.x->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->output_data.x->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->output_data.x->size_hint_flags |= YNegative;
      f->output_data.x->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, (long) 0, 0);

  modified_left = f->output_data.x->left_pos;
  modified_top = f->output_data.x->top_pos;
#if 0 /* Running on psilocin (Debian), and displaying on the NCD X-terminal,
	 this seems to be unnecessary and incorrect.  rms, 4/17/97.  */
  /* It is a mystery why we need to add the border_width here
     when the frame is already visible, but experiment says we do.  */
  if (change_gravity != 0)
    {
      modified_left += f->output_data.x->border_width;
      modified_top += f->output_data.x->border_width;
    }
#endif

#ifdef USE_X_TOOLKIT
  XMoveWindow (FRAME_X_DISPLAY (f), XtWindow (f->output_data.x->widget),
	       modified_left, modified_top);
#else /* not USE_X_TOOLKIT */
  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	       modified_left, modified_top);
#endif /* not USE_X_TOOLKIT */
  UNBLOCK_INPUT;
}

/* Check if we need to resize the frame due to a fullscreen request.
   If so needed, resize the frame. */
static void
x_check_fullscreen (f)
     struct frame *f;
{
  if (f->output_data.x->want_fullscreen & FULLSCREEN_BOTH)
    {
      int width, height, ign;
                      
      x_real_positions (f, &f->output_data.x->left_pos,
                        &f->output_data.x->top_pos);

      x_fullscreen_adjust (f, &width, &height, &ign, &ign);
                  
      /* We do not need to move the window, it shall be taken care of
         when setting WM manager hints.
         If the frame is visible already, the position is checked by
         x_check_fullscreen_move. */
      if (f->width != width || f->height != height)
        {
          change_frame_size (f, height, width, 0, 1, 0);
          SET_FRAME_GARBAGED (f);
          cancel_mouse_face (f);

          /* Wait for the change of frame size to occur */
          f->output_data.x->want_fullscreen |= FULLSCREEN_WAIT;
          
        }
    }
}

/* If frame parameters are set after the frame is mapped, we need to move
   the window.  This is done in xfns.c.
   Some window managers moves the window to the right position, some
   moves the outer window manager window to the specified position.
   Here we check that we are in the right spot.  If not, make a second
   move, assuming we are dealing with the second kind of window manager. */
static void
x_check_fullscreen_move (f)
     struct frame *f;
{
  if (f->output_data.x->want_fullscreen & FULLSCREEN_MOVE_WAIT)
  {
    int expect_top = f->output_data.x->top_pos;
    int expect_left = f->output_data.x->left_pos;

    if (f->output_data.x->want_fullscreen & FULLSCREEN_HEIGHT)
      expect_top = 0;
    if (f->output_data.x->want_fullscreen & FULLSCREEN_WIDTH)
      expect_left = 0;
    
    if (expect_top != f->output_data.x->top_pos
        || expect_left != f->output_data.x->left_pos)
      x_set_offset (f, expect_left, expect_top, 1);

    /* Just do this once */
    f->output_data.x->want_fullscreen &= ~FULLSCREEN_MOVE_WAIT;
  }
}


/* Calculate fullscreen size.  Return in *TOP_POS and *LEFT_POS the
   wanted positions of the WM window (not emacs window).
   Return in *WIDTH and *HEIGHT the wanted width and height of Emacs
   window (FRAME_X_WINDOW).
 */
void
x_fullscreen_adjust (f, width, height, top_pos, left_pos)
     struct frame *f;
     int *width;
     int *height;
     int *top_pos;
     int *left_pos;
{
  int newwidth = f->width, newheight = f->height;

  *top_pos = f->output_data.x->top_pos;
  *left_pos = f->output_data.x->left_pos;
  
  if (f->output_data.x->want_fullscreen & FULLSCREEN_HEIGHT)
    {
      int ph;
      
      ph = FRAME_X_DISPLAY_INFO (f)->height;
      newheight = PIXEL_TO_CHAR_HEIGHT (f, ph);
      ph = CHAR_TO_PIXEL_HEIGHT (f, newheight)
        - f->output_data.x->y_pixels_diff;
      newheight = PIXEL_TO_CHAR_HEIGHT (f, ph);
      *top_pos = 0;
    }

  if (f->output_data.x->want_fullscreen & FULLSCREEN_WIDTH)
    {
      int pw;
      
      pw = FRAME_X_DISPLAY_INFO (f)->width;
      newwidth = PIXEL_TO_CHAR_WIDTH (f, pw);
      pw = CHAR_TO_PIXEL_WIDTH (f, newwidth)
        - f->output_data.x->x_pixels_diff;
      newwidth = PIXEL_TO_CHAR_WIDTH (f, pw);
      *left_pos = 0;
    }

  *width = newwidth;
  *height = newheight;
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
  f->output_data.x->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.x->font)));

  x_compute_fringe_widths (f, 0);

  pixelwidth = CHAR_TO_PIXEL_WIDTH (f, cols);
  pixelheight = CHAR_TO_PIXEL_HEIGHT (f, rows);

  f->output_data.x->win_gravity = NorthWestGravity;
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
  PIXEL_WIDTH (f) = pixelwidth;
  PIXEL_HEIGHT (f) = pixelheight;

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

#ifdef USE_X_TOOLKIT
  
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

  pix_x = CHAR_TO_PIXEL_COL (f, x) + FONT_WIDTH  (f->output_data.x->font) / 2;
  pix_y = CHAR_TO_PIXEL_ROW (f, y) + f->output_data.x->line_height / 2;

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
  if (f->async_visible)
    {
      BLOCK_INPUT;
#ifdef USE_X_TOOLKIT
      XRaiseWindow (FRAME_X_DISPLAY (f), XtWindow (f->output_data.x->widget));
#else /* not USE_X_TOOLKIT */
      XRaiseWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* not USE_X_TOOLKIT */
      XFlush (FRAME_X_DISPLAY (f));
      UNBLOCK_INPUT;
    }
}

/* Lower frame F.  */

void
x_lower_frame (f)
     struct frame *f;
{
  if (f->async_visible)
    {
      BLOCK_INPUT;
#ifdef USE_X_TOOLKIT
      XLowerWindow (FRAME_X_DISPLAY (f), XtWindow (f->output_data.x->widget));
#else /* not USE_X_TOOLKIT */
      XLowerWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* not USE_X_TOOLKIT */
      XFlush (FRAME_X_DISPLAY (f));
      UNBLOCK_INPUT;
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
	  && ! f->output_data.x->asked_for_visible)
	x_set_offset (f, f->output_data.x->left_pos, f->output_data.x->top_pos, 0);

      f->output_data.x->asked_for_visible = 1;

      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, NormalState);
#ifdef USE_X_TOOLKIT
      /* This was XtPopup, but that did nothing for an iconified frame.  */
      XtMapWidget (f->output_data.x->widget);
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
    int count;
    /* This must be before UNBLOCK_INPUT
       since events that arrive in response to the actions above
       will set it when they are handled.  */
    int previously_visible = f->output_data.x->has_been_visible;

    original_left = f->output_data.x->left_pos;
    original_top = f->output_data.x->top_pos;

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

    if (! FRAME_VISIBLE_P (f) && ! FRAME_ICONIFIED_P (f)
	&& f->output_data.x->win_gravity == NorthWestGravity
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
       FreeBSD, Linux and Solaris.  It turns out that, for some
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

#ifdef USE_X_TOOLKIT
  /* Use the frame's outermost window, not the one we normally draw on.  */
  window = XtWindow (f->output_data.x->widget);
#else /* not USE_X_TOOLKIT */
  window = FRAME_X_WINDOW (f);
#endif /* not USE_X_TOOLKIT */

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
			SubstructureRedirectMaskSubstructureNotifyMask,
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
  if (! FRAME_VISIBLE_P (f) && !FRAME_ICONIFIED_P (f))
    x_set_offset (f, f->output_data.x->left_pos, f->output_data.x->top_pos, 0);

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
      if (FRAME_X_WINDOW (f))
	XDestroyWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* !USE_X_TOOLKIT */

      unload_color (f, f->output_data.x->foreground_pixel);
      unload_color (f, f->output_data.x->background_pixel);
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
   flag (this is useful when FLAGS is 0).  */

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
  Window window = XtWindow (f->output_data.x->widget);
#else /* not USE_X_TOOLKIT */
  Window window = FRAME_X_WINDOW (f);
#endif /* not USE_X_TOOLKIT */

  /* Setting PMaxSize caused various problems.  */
  size_hints.flags = PResizeInc | PMinSize /* | PMaxSize */;

  size_hints.x = f->output_data.x->left_pos;
  size_hints.y = f->output_data.x->top_pos;

#ifdef USE_X_TOOLKIT
  XtSetArg (al[ac], XtNwidth, &widget_width); ac++;
  XtSetArg (al[ac], XtNheight, &widget_height); ac++;
  XtGetValues (f->output_data.x->widget, al, ac);
  size_hints.height = widget_height;
  size_hints.width = widget_width;
#else /* not USE_X_TOOLKIT */
  size_hints.height = PIXEL_HEIGHT (f);
  size_hints.width = PIXEL_WIDTH (f);
#endif /* not USE_X_TOOLKIT */

  size_hints.width_inc = FONT_WIDTH (f->output_data.x->font);
  size_hints.height_inc = f->output_data.x->line_height;
  size_hints.max_width
    = FRAME_X_DISPLAY_INFO (f)->width - CHAR_TO_PIXEL_WIDTH (f, 0);
  size_hints.max_height
    = FRAME_X_DISPLAY_INFO (f)->height - CHAR_TO_PIXEL_HEIGHT (f, 0);

  /* Calculate the base and minimum sizes.

     (When we use the X toolkit, we don't do it here.
     Instead we copy the values that the widgets are using, below.)  */
#ifndef USE_X_TOOLKIT
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

#ifdef HAVE_X11R4
    value = XGetWMNormalHints (FRAME_X_DISPLAY (f), window, &hints,
			       &supplied_return);
#else
    value = XGetNormalHints (FRAME_X_DISPLAY (f), window, &hints);
#endif

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
  size_hints.win_gravity = f->output_data.x->win_gravity;
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
  Pixmap icon_pixmap;

#ifndef USE_X_TOOLKIT
  Window window = FRAME_X_WINDOW (f);
#endif

  if (pixmap_id > 0)
    {
      icon_pixmap = x_bitmap_pixmap (f, pixmap_id);
      f->output_data.x->wm_hints.icon_pixmap = icon_pixmap;
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
#else
      return;
#endif
    }

#ifdef USE_X_TOOLKIT /* same as in x_wm_set_window_state.  */

  {
    Arg al[1];
    XtSetArg (al[0], XtNiconPixmap, icon_pixmap);
    XtSetValues (f->output_data.x->widget, al, 1);
  }

#else /* not USE_X_TOOLKIT */
  
  f->output_data.x->wm_hints.flags |= IconPixmapHint;
  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);

#endif /* not USE_X_TOOLKIT */
}

void
x_wm_set_icon_position (f, icon_x, icon_y)
     struct frame *f;
     int icon_x, icon_y;
{
#ifdef USE_X_TOOLKIT
  Window window = XtWindow (f->output_data.x->widget);
#else
  Window window = FRAME_X_WINDOW (f);
#endif

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

   SIZE < 0 means include scalable fonts.

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
  int count;
  int allow_scalable_fonts_p = 0;

  if (size < 0)
    {
      allow_scalable_fonts_p = 1;
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
		   allow_scalable_fonts_p ? Qt : Qnil);
      list = Fassoc (key, tem);
      if (!NILP (list))
	{
	  list = Fcdr_safe (list);
	  /* We have a cashed list.  Don't have to get the list again.  */
	  goto label_cached;
	}

      /* At first, put PATTERN in the cache.  */

      BLOCK_INPUT;
      count = x_catch_errors (dpy);

      if (try_XLoadQueryFont)
	{
	  XFontStruct *font;
	  unsigned long value;

	  font = XLoadQueryFont (dpy, XSTRING (pattern)->data);
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
	  names = XListFonts (dpy, XSTRING (pattern)->data, max (maxnames, 10),
			      &num_fonts);
	  if (x_had_errors_p (dpy))
	    {
	      /* This error is perhaps due to insufficient memory on X
                 server.  Let's just ignore it.  */
	      names = NULL;
	      x_clear_errors (dpy);
	    }
	}

      x_uncatch_errors (dpy, count);
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
	      int average_width = -1, dashes = 0;
	      
	      /* Count the number of dashes in NAMES[I].  If there are
		 14 dashes, and the field value following 12th dash
		 (AVERAGE_WIDTH) is 0, this is a auto-scaled font which
		 is usually too ugly to be used for editing.  Let's
		 ignore it.  */
	      while (*p)
		if (*p++ == '-')
		  {
		    dashes++;
		    if (dashes == 7) /* PIXEL_SIZE field */
		      width = atoi (p);
		    else if (dashes == 12) /* AVERAGE_WIDTH field */
		      average_width = atoi (p);
		  }
	      
	      if (allow_scalable_fonts_p
		  || dashes < 14 || average_width != 0)
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
	      count = x_catch_errors (dpy);
	      thisinfo = XLoadQueryFont (dpy,
					 XSTRING (XCAR (tem))->data);
	      if (x_had_errors_p (dpy))
		{
		  /* This error is perhaps due to insufficient memory on X
		     server.  Let's just ignore it.  */
		  thisinfo = NULL;
		  x_clear_errors (dpy);
		}
	      x_uncatch_errors (dpy, count);
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
  int count;

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
			   XSTRING (XCAR (tail))->data)
		  || !strcmp (dpyinfo->font_table[i].full_name,
			      XSTRING (XCAR (tail))->data)))
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
      fontname = (char *) XSTRING (XCAR (font_names))->data;

    BLOCK_INPUT;
    count = x_catch_errors (FRAME_X_DISPLAY (f));
    font = (XFontStruct *) XLoadQueryFont (FRAME_X_DISPLAY (f), fontname);
    if (x_had_errors_p (FRAME_X_DISPLAY (f)))
      {
	/* This error is perhaps due to insufficient memory on X
	   server.  Let's just ignore it.  */
	font = NULL;
	x_clear_errors (FRAME_X_DISPLAY (f));
      }
    x_uncatch_errors (FRAME_X_DISPLAY (f), count);
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
    fontp->font = font;
    fontp->font_idx = i;
    fontp->name = (char *) xmalloc (strlen (fontname) + 1);
    bcopy (fontname, fontp->name, strlen (fontname) + 1);

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
       uses this font.  So, we set information in fontp->encoding[1]
       which is never used by any charset.  If mapping can't be
       decided, set FONT_ENCODING_NOT_DECIDED.  */
    fontp->encoding[1]
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
	&& (!strcmp (dpyinfo->font_table[i].name, fontname)
	    || !strcmp (dpyinfo->font_table[i].full_name, fontname)))
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
#endif /* USE_X_TOOLKIT */

static int x_initialized;

#ifdef MULTI_KBOARD
/* Test whether two display-name strings agree up to the dot that separates
   the screen number from the server number.  */
static int
same_x_server (name1, name2)
     char *name1, *name2;
{
  int seen_colon = 0;
  unsigned char *system_name = XSTRING (Vsystem_name)->data;
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

struct x_display_info *
x_term_init (display_name, xrm_option, resource_name)
     Lisp_Object display_name;
     char *xrm_option;
     char *resource_name;
{
  int connection;
  Display *dpy;
  struct x_display_info *dpyinfo;
  XrmDatabase xrdb;

  BLOCK_INPUT;

  if (!x_initialized)
    {
      x_initialize ();
      x_initialized = 1;
    }

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
    dpy = XtOpenDisplay (Xt_app_con, XSTRING (display_name)->data,
			 resource_name, EMACS_CLASS,
			 emacs_options, XtNumber (emacs_options),
			 &argc, argv);

#ifdef HAVE_X11XTR6
    /* I think this is to compensate for XtSetLanguageProc.  */
    fixup_locale ();
#endif
  }

#else /* not USE_X_TOOLKIT */
#ifdef HAVE_X11R5
  XSetLocaleModifiers ("");
#endif
  dpy = XOpenDisplay (XSTRING (display_name)->data);
#endif /* not USE_X_TOOLKIT */

  /* Detect failure.  */
  if (dpy == 0)
    {
      UNBLOCK_INPUT;
      return 0;
    }

  /* We have definitely succeeded.  Record the new connection.  */

  dpyinfo = (struct x_display_info *) xmalloc (sizeof (struct x_display_info));
  bzero (dpyinfo, sizeof *dpyinfo);

#ifdef MULTI_KBOARD
  {
    struct x_display_info *share;
    Lisp_Object tail;

    for (share = x_display_list, tail = x_display_name_list; share;
	 share = share->next, tail = XCDR (tail))
      if (same_x_server (XSTRING (XCAR (XCAR (tail)))->data,
			 XSTRING (display_name)->data))
	break;
    if (share)
      dpyinfo->kboard = share->kboard;
    else
      {
	dpyinfo->kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
	init_kboard (dpyinfo->kboard);
	if (!EQ (XSYMBOL (Qvendor_specific_keysyms)->function, Qunbound))
	  {
	    char *vendor = ServerVendor (dpy);
	    UNBLOCK_INPUT;
	    dpyinfo->kboard->Vsystem_key_alist
	      = call1 (Qvendor_specific_keysyms,
		       build_string (vendor ? vendor : ""));
	    BLOCK_INPUT;
	  }

	dpyinfo->kboard->next_kboard = all_kboards;
	all_kboards = dpyinfo->kboard;
	/* Don't let the initial kboard remain current longer than necessary.
	   That would cause problems if a file loaded on startup tries to
	   prompt in the mini-buffer.  */
	if (current_kboard == initial_kboard)
	  current_kboard = dpyinfo->kboard;
      }
    dpyinfo->kboard->reference_count++;
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

#if 0
  XSetAfterFunction (x_current_display, x_trace_wire);
#endif /* ! 0 */

  dpyinfo->x_id_name
    = (char *) xmalloc (STRING_BYTES (XSTRING (Vinvocation_name))
			+ STRING_BYTES (XSTRING (Vsystem_name))
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
  dpyinfo->grabbed = 0;
  dpyinfo->reference_count = 0;
  dpyinfo->icon_bitmap_id = -1;
  dpyinfo->font_table = NULL;
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
  dpyinfo->mouse_face_face_id = DEFAULT_FACE_ID;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_overlay = Qnil;
  dpyinfo->mouse_face_mouse_x = dpyinfo->mouse_face_mouse_y = 0;
  dpyinfo->mouse_face_defer = 0;
  dpyinfo->mouse_face_hidden = 0;
  dpyinfo->x_focus_frame = 0;
  dpyinfo->x_focus_event_frame = 0;
  dpyinfo->x_highlight_frame = 0;
  dpyinfo->image_cache = make_image_cache ();

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
	      && (!strcmp (XSTRING (value)->data, "true")
		  || !strcmp (XSTRING (value)->data, "on")))
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
    dpyinfo->resy = pixels * 25.4 / mm;
    pixels = DisplayWidth (dpyinfo->display, screen_number);
    mm = DisplayWidthMM (dpyinfo->display, screen_number);
    dpyinfo->resx = pixels * 25.4 / mm;
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

  dpyinfo->cut_buffers_initialized = 0;

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

#ifdef USE_LUCID
#ifdef HAVE_X11R5 /* It seems X11R4 lacks XtCvtStringToFont, and XPointer.  */
  /* Make sure that we have a valid font for dialog boxes
     so that Xt does not crash.  */
  {
    Display *dpy = dpyinfo->display;
    XrmValue d, fr, to;
    Font font;
    int count;
    
    d.addr = (XPointer)&dpy;
    d.size = sizeof (Display *);
    fr.addr = XtDefaultFont;
    fr.size = sizeof (XtDefaultFont);
    to.size = sizeof (Font *);
    to.addr = (XPointer)&font;
    count = x_catch_errors (dpy);
    if (!XtCallConverter (dpy, XtCvtStringToFont, &d, 1, &fr, &to, NULL))
      abort ();
    if (x_had_errors_p (dpy) || !XQueryFont (dpy, font))
      XrmPutLineResource (&xrdb, "Emacs.dialog.*.font: 9x15");
    x_uncatch_errors (dpy, count);
  }
#endif
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
	&& (!strcmp (XSTRING (value)->data, "true")
	    || !strcmp (XSTRING (value)->data, "on")))
      XSynchronize (dpyinfo->display, True);
  }
  
  UNBLOCK_INPUT;

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
#ifdef MULTI_KBOARD
  if (--dpyinfo->kboard->reference_count == 0)
    delete_kboard (dpyinfo->kboard);
#endif
#ifdef HAVE_X_I18N
  if (dpyinfo->xim)
    xim_close_dpy (dpyinfo);
#endif
  
  xfree (dpyinfo->font_table);
  xfree (dpyinfo->x_id_name);
  xfree (dpyinfo->color_cells);
  xfree (dpyinfo);
}


/* Set up use of X before we make the first connection.  */

static struct redisplay_interface x_redisplay_interface =
{
  x_produce_glyphs,
  x_write_glyphs,
  x_insert_glyphs,
  x_clear_end_of_line,
  x_scroll_run,
  x_after_update_window_line,
  x_update_window_begin,
  x_update_window_end,
  XTcursor_to,
  x_flush,
  x_clear_mouse_face,
  x_get_glyph_overhangs,
  x_fix_overlapping_area
};

void
x_initialize ()
{
  rif = &x_redisplay_interface;

  clear_frame_hook = x_clear_frame;
  ins_del_lines_hook = x_ins_del_lines;
  delete_glyphs_hook = x_delete_glyphs;
  ring_bell_hook = XTring_bell;
  reset_terminal_modes_hook = XTreset_terminal_modes;
  set_terminal_modes_hook = XTset_terminal_modes;
  update_begin_hook = x_update_begin;
  update_end_hook = x_update_end;
  set_terminal_window_hook = XTset_terminal_window;
  read_socket_hook = XTread_socket;
  frame_up_to_date_hook = XTframe_up_to_date;
  mouse_position_hook = XTmouse_position;
  frame_rehighlight_hook = XTframe_rehighlight;
  frame_raise_lower_hook = XTframe_raise_lower;
  set_vertical_scroll_bar_hook = XTset_vertical_scroll_bar;
  condemn_scroll_bars_hook = XTcondemn_scroll_bars;
  redeem_scroll_bar_hook = XTredeem_scroll_bar;
  judge_scroll_bars_hook = XTjudge_scroll_bars;
  estimate_mode_line_height_hook = x_estimate_mode_line_height;

  scroll_region_ok = 1;		/* we'll scroll partial frames */
  char_ins_del_ok = 1;
  line_ins_del_ok = 1;		/* we'll just blt 'em */
  fast_clear_end_of_line = 1;	/* X does this well */
  memory_below_frame = 0;	/* we don't remember what scrolls
				   off the bottom */
  baud_rate = 19200;

  x_noop_count = 0;
  last_tool_bar_item = -1;
  any_help_event_p = 0;
  
  /* Try to use interrupt input; if we can't, then start polling.  */
  Fset_input_mode (Qt, Qnil, Qt, Qnil);

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

  /* Install an asynchronous timer that processes Xt timeout events
     every 0.1s.  This is necessary because some widget sets use
     timeouts internally, for example the LessTif menu bar, or the
     Xaw3d scroll bar.  When Xt timouts aren't processed, these
     widgets don't behave normally.  */
  {
    EMACS_TIME interval;
    EMACS_SET_SECS_USECS (interval, 0, 100000);
    start_atimer (ATIMER_CONTINUOUS, interval, x_process_timeouts, 0);
  }
#endif
  
#ifdef USE_TOOLKIT_SCROLL_BARS
  xaw3d_arrow_scroll = False;
  xaw3d_pick_top = True;
#endif

  /* Note that there is no real way portable across R3/R4 to get the
     original error handler.  */
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_quitter);

  /* Disable Window Change signals;  they are handled by X events.  */
#ifdef SIGWINCH
  signal (SIGWINCH, SIG_DFL);
#endif /* ! defined (SIGWINCH) */

  signal (SIGPIPE, x_connection_signal);

#ifdef HAVE_X_SM
  x_session_initialize ();
#endif
}


void
syms_of_xterm ()
{
  staticpro (&x_error_message_string);
  x_error_message_string = Qnil;

  staticpro (&x_display_name_list);
  x_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;

  staticpro (&Qvendor_specific_keysyms);
  Qvendor_specific_keysyms = intern ("vendor-specific-keysyms");

  staticpro (&last_mouse_press_frame);
  last_mouse_press_frame = Qnil;

  help_echo = Qnil;
  staticpro (&help_echo);
  help_echo_object = Qnil;
  staticpro (&help_echo_object);
  help_echo_window = Qnil;
  staticpro (&help_echo_window);
  previous_help_echo = Qnil;
  staticpro (&previous_help_echo);
  help_echo_pos = -1;

  DEFVAR_BOOL ("x-autoselect-window", &x_autoselect_window_p,
    doc: /* *Non-nil means autoselect window with mouse pointer.  */);
  x_autoselect_window_p = 0;

  DEFVAR_BOOL ("x-stretch-cursor", &x_stretch_cursor_p,
    doc: /* *Non-nil means draw block cursor as wide as the glyph under it.
For example, if a block cursor is over a tab, it will be drawn as
wide as that tab on the display.  */);
  x_stretch_cursor_p = 0;

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       &x_use_underline_position_properties,
     doc: /* *Non-nil means make use of UNDERLINE_POSITION font properties.
nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, for example 7x13 on XFree prior
to 4.1, set this to nil.  */);
  x_use_underline_position_properties = 1;

  DEFVAR_LISP ("x-toolkit-scroll-bars", &Vx_toolkit_scroll_bars,
    doc: /* What X toolkit scroll bars Emacs uses.
A value of nil means Emacs doesn't use X toolkit scroll bars.
Otherwise, value is a symbol describing the X toolkit.  */);
#ifdef USE_TOOLKIT_SCROLL_BARS
#ifdef USE_MOTIF
  Vx_toolkit_scroll_bars = intern ("motif");
#elif defined HAVE_XAW3D
  Vx_toolkit_scroll_bars = intern ("xaw3d");
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

}

#endif /* HAVE_X_WINDOWS */
