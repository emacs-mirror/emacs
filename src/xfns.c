/* Functions for the X Window System.

Copyright (C) 1989, 1992-2020 Free Software Foundation, Inc.

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

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>

#include "lisp.h"
#include "xterm.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "dispextern.h"
#include "keyboard.h"
#include "blockinput.h"
#include "charset.h"
#include "coding.h"
#include "termhooks.h"
#include "font.h"

#include <sys/types.h>
#include <sys/stat.h>

#include "bitmaps/gray.xbm"
#include "xsettings.h"

#ifdef HAVE_XRANDR
#include <X11/extensions/Xrandr.h>
#endif
#ifdef HAVE_XINERAMA
#include <X11/extensions/Xinerama.h>
#endif

#ifdef USE_GTK
#include "gtkutil.h"
#endif

#ifdef HAVE_XDBE
#include <X11/extensions/Xdbe.h>
#endif

#ifdef USE_X_TOOLKIT
#include <X11/Shell.h>

#ifndef USE_MOTIF
#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Paned.h>
#include <X11/Xaw3d/Label.h>
#else /* !HAVE_XAW3D */
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Label.h>
#endif /* HAVE_XAW3D */
#endif /* USE_MOTIF */

#ifdef USG
#undef USG	/* ####KLUDGE for Solaris 2.2 and up */
#include <X11/Xos.h>
#define USG
#ifdef USG /* Pacify gcc -Wunused-macros.  */
#endif
#else
#include <X11/Xos.h>
#endif

#include "widget.h"

#include "../lwlib/lwlib.h"

#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/FileSB.h>
#include <Xm/List.h>
#include <Xm/TextF.h>
#include <Xm/MwmUtil.h>
#endif

#ifdef USE_LUCID
#include "../lwlib/xlwmenu.h"
#endif

/* Unique id counter for widgets created by the Lucid Widget Library.  */

extern LWLIB_ID widget_id_tick;

#ifdef USE_MOTIF

#endif /* USE_MOTIF */

#endif /* USE_X_TOOLKIT */

#ifdef USE_GTK

#endif /* USE_GTK */

#define MAXREQUEST(dpy) (XMaxRequestSize (dpy))

static ptrdiff_t image_cache_refcount;
#ifdef GLYPH_DEBUG
static int dpyinfo_refcount;
#endif

#ifndef USE_MOTIF
#ifndef USE_GTK
/** #define MWM_HINTS_FUNCTIONS     (1L << 0) **/
#define MWM_HINTS_DECORATIONS   (1L << 1)
/** #define MWM_HINTS_INPUT_MODE    (1L << 2) **/
/** #define MWM_HINTS_STATUS        (1L << 3) **/

#define MWM_DECOR_ALL           (1L << 0)
/** #define MWM_DECOR_BORDER        (1L << 1) **/
/** #define MWM_DECOR_RESIZEH       (1L << 2) **/
/** #define MWM_DECOR_TITLE         (1L << 3) **/
/** #define MWM_DECOR_MENU          (1L << 4) **/
/** #define MWM_DECOR_MINIMIZE      (1L << 5) **/
/** #define MWM_DECOR_MAXIMIZE      (1L << 6) **/

/** #define _XA_MOTIF_WM_HINTS "_MOTIF_WM_HINTS" **/

typedef struct {
    unsigned long flags;
    unsigned long functions;
    unsigned long decorations;
    long input_mode;
    unsigned long status;
} PropMotifWmHints;

#define PROP_MOTIF_WM_HINTS_ELEMENTS 5
#endif /* NOT USE_GTK */
#endif /* NOT USE_MOTIF */

static struct x_display_info *x_display_info_for_name (Lisp_Object);
static void set_up_x_back_buffer (struct frame *f);

/* Let the user specify an X display with a Lisp object.
   OBJECT may be nil, a frame or a terminal object.
   nil stands for the selected frame--or, if that is not an X frame,
   the first X display on the list.  */

struct x_display_info *
check_x_display_info (Lisp_Object object)
{
  struct x_display_info *dpyinfo = NULL;

  if (NILP (object))
    {
      struct frame *sf = XFRAME (selected_frame);

      if (FRAME_X_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_DISPLAY_INFO (sf);
      else if (x_display_list != 0)
	dpyinfo = x_display_list;
      else
	error ("X windows are not in use or not initialized");
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = decode_live_terminal (object);

      if (t->type != output_x_window)
        error ("Terminal %d is not an X display", t->id);

      dpyinfo = t->display_info.x;
    }
  else if (STRINGP (object))
    dpyinfo = x_display_info_for_name (object);
  else
    {
      struct frame *f = decode_window_system_frame (object);
      dpyinfo = FRAME_DISPLAY_INFO (f);
    }

  return dpyinfo;
}

/* Return the screen positions and offsets of frame F.
   Store the offsets between FRAME_OUTER_WINDOW and the containing
   window manager window into LEFT_OFFSET_X, RIGHT_OFFSET_X,
   TOP_OFFSET_Y and BOTTOM_OFFSET_Y.
   Store the offsets between FRAME_X_WINDOW and the containing
   window manager window into X_PIXELS_DIFF and Y_PIXELS_DIFF.
   Store the screen positions of frame F into XPTR and YPTR.
   These are the positions of the containing window manager window,
   not Emacs's own window.  */
void
x_real_pos_and_offsets (struct frame *f,
                        int *left_offset_x,
                        int *right_offset_x,
                        int *top_offset_y,
                        int *bottom_offset_y,
                        int *x_pixels_diff,
                        int *y_pixels_diff,
                        int *xptr,
                        int *yptr,
                        int *outer_border)
{
  int win_x = 0, win_y = 0, outer_x = 0, outer_y = 0;
  int real_x = 0, real_y = 0;
  bool had_errors = false;
  struct frame *parent_frame = FRAME_PARENT_FRAME (f);
  Window win = (parent_frame
		? FRAME_X_WINDOW (parent_frame)
		: f->output_data.x->parent_desc);
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  long max_len = 400;
  Atom target_type = XA_CARDINAL;
  unsigned int ow = 0, oh = 0;
  unsigned int fw = 0, fh = 0;
  unsigned int bw = 0;
  /* We resort to XCB if possible because there are several X calls
     here which require responses from the server but do not have data
     dependencies between them.  Using XCB lets us pipeline requests,
     whereas with Xlib we must wait for each answer before sending the
     next request.

     For a non-local display, the round-trip time could be a few tens
     of milliseconds, depending on the network distance.  It doesn't
     take a lot of those to add up to a noticeable hesitation in
     responding to user actions.  */
#ifdef USE_XCB
  xcb_connection_t *xcb_conn = dpyinfo->xcb_connection;
  xcb_get_property_cookie_t prop_cookie;
  xcb_get_geometry_cookie_t outer_geom_cookie;
  bool sent_requests = false;
#else
  Atom actual_type;
  unsigned long actual_size, bytes_remaining;
  int rc, actual_format;
  Display *dpy = FRAME_X_DISPLAY (f);
  unsigned char *tmp_data = NULL;
#endif

  if (x_pixels_diff) *x_pixels_diff = 0;
  if (y_pixels_diff) *y_pixels_diff = 0;
  if (left_offset_x) *left_offset_x = 0;
  if (top_offset_y) *top_offset_y = 0;
  if (right_offset_x) *right_offset_x = 0;
  if (bottom_offset_y) *bottom_offset_y = 0;
  if (xptr) *xptr = 0;
  if (yptr) *yptr = 0;
  if (outer_border) *outer_border = 0;

  if (win == dpyinfo->root_window)
    win = FRAME_OUTER_WINDOW (f);

  block_input ();

#ifndef USE_XCB
  /* If we're using XCB, all errors are checked for on each call.  */
  x_catch_errors (dpy);
#endif

  /* This loop traverses up the containment tree until we hit the root
     window.  Window managers may intersect many windows between our window
     and the root window.  The window we find just before the root window
     should be the outer WM window. */
  for (;;)
    {
      Window wm_window UNINIT, rootw UNINIT;

#ifdef USE_XCB
      xcb_query_tree_cookie_t query_tree_cookie;
      xcb_query_tree_reply_t *query_tree;

      query_tree_cookie = xcb_query_tree (xcb_conn, win);
      query_tree = xcb_query_tree_reply (xcb_conn, query_tree_cookie, NULL);
      if (query_tree == NULL)
	had_errors = true;
      else
	{
	  wm_window = query_tree->parent;
	  rootw = query_tree->root;
	  free (query_tree);
	}
#else
      Window *tmp_children;
      unsigned int tmp_nchildren;
      int success;

      success = XQueryTree (dpy, win, &rootw,
			    &wm_window, &tmp_children, &tmp_nchildren);

      had_errors = x_had_errors_p (dpy);

      /* Don't free tmp_children if XQueryTree failed.  */
      if (! success)
	break;

      XFree (tmp_children);
#endif

      if (had_errors || wm_window == rootw)
        break;

      win = wm_window;
    }

  if (! had_errors)
    {
#ifdef USE_XCB
      xcb_get_geometry_cookie_t geom_cookie;
      xcb_translate_coordinates_cookie_t trans_cookie;
      xcb_translate_coordinates_cookie_t outer_trans_cookie;

      xcb_translate_coordinates_reply_t *trans;
      xcb_get_geometry_reply_t *geom;
#else
      Window child, rootw;
      unsigned int ign;
#endif

#ifdef USE_XCB
      /* Fire off the requests that don't have data dependencies.

         Once we've done this, we must collect the results for each
         one before returning, even if other errors are detected,
         making the other responses moot.  */
      geom_cookie = xcb_get_geometry (xcb_conn, win);

      trans_cookie =
        xcb_translate_coordinates (xcb_conn,
                                   /* From-window, to-window.  */
                                   FRAME_DISPLAY_INFO (f)->root_window,
                                   FRAME_X_WINDOW (f),

                                   /* From-position.  */
                                   0, 0);
      if (FRAME_X_WINDOW (f) != FRAME_OUTER_WINDOW (f))
        outer_trans_cookie =
          xcb_translate_coordinates (xcb_conn,
                                     /* From-window, to-window.  */
                                     FRAME_DISPLAY_INFO (f)->root_window,
                                     FRAME_OUTER_WINDOW (f),

                                     /* From-position.  */
                                     0, 0);
      if (right_offset_x || bottom_offset_y)
	outer_geom_cookie = xcb_get_geometry (xcb_conn,
					      FRAME_OUTER_WINDOW (f));

      if (!parent_frame
	  && dpyinfo->root_window == f->output_data.x->parent_desc)
	/* Try _NET_FRAME_EXTENTS if our parent is the root window.  */
	prop_cookie = xcb_get_property (xcb_conn, 0, win,
					dpyinfo->Xatom_net_frame_extents,
					target_type, 0, max_len);

      sent_requests = true;
#endif

      /* Get the real coordinates for the WM window upper left corner */
#ifdef USE_XCB
      geom = xcb_get_geometry_reply (xcb_conn, geom_cookie, NULL);
      if (geom)
	{
	  real_x = geom->x;
	  real_y = geom->y;
	  ow = geom->width;
	  oh = geom->height;
	  bw = geom->border_width;
	  free (geom);
	}
      else
	had_errors = true;
#else
      XGetGeometry (dpy, win,
		    &rootw, &real_x, &real_y, &ow, &oh, &bw, &ign);
#endif

      /* Translate real coordinates to coordinates relative to our
         window.  For our window, the upper left corner is 0, 0.
         Since the upper left corner of the WM window is outside
         our window, win_x and win_y will be negative:

         ------------------          ---> x
         |      title                |
         | -----------------         v y
         | |  our window

         Since we don't care about the child window corresponding to
         the actual coordinates, we can send zero to get the offsets
         and compute the resulting coordinates below.  This reduces
         the data dependencies between calls and lets us pipeline the
         requests better in the XCB case.  */
#ifdef USE_XCB
      trans = xcb_translate_coordinates_reply (xcb_conn, trans_cookie, NULL);
      if (trans)
	{
	  win_x = trans->dst_x;
	  win_y = trans->dst_y;
	  free (trans);
	}
      else
	had_errors = true;
#else
      XTranslateCoordinates (dpy,

			     /* From-window, to-window.  */
			     FRAME_DISPLAY_INFO (f)->root_window,
                             FRAME_X_WINDOW (f),

			     /* From-position, to-position.  */
                             0, 0, &win_x, &win_y,

			     /* Child of win.  */
			     &child);
#endif

      win_x += real_x;
      win_y += real_y;

      if (FRAME_X_WINDOW (f) == FRAME_OUTER_WINDOW (f))
	{
          outer_x = win_x;
          outer_y = win_y;
	}
      else
        {
#ifdef USE_XCB
          xcb_translate_coordinates_reply_t *outer_trans;

          outer_trans = xcb_translate_coordinates_reply (xcb_conn,
                                                         outer_trans_cookie,
                                                         NULL);
          if (outer_trans)
            {
              outer_x = outer_trans->dst_x;
              outer_y = outer_trans->dst_y;
              free (outer_trans);
            }
          else
	    had_errors = true;
#else
          XTranslateCoordinates (dpy,

                                 /* From-window, to-window.  */
                                 FRAME_DISPLAY_INFO (f)->root_window,
                                 FRAME_OUTER_WINDOW (f),

                                 /* From-position, to-position.  */
                                 0, 0, &outer_x, &outer_y,

                                 /* Child of win.  */
                                 &child);
#endif

	  outer_x += real_x;
	  outer_y += real_y;
	}

#ifndef USE_XCB
      had_errors = x_had_errors_p (dpy);
#endif
    }

  if (!parent_frame && dpyinfo->root_window == f->output_data.x->parent_desc)
    {
      /* Try _NET_FRAME_EXTENTS if our parent is the root window.  */
#ifdef USE_XCB
      /* Make sure we didn't get an X error early and skip sending the
         request.  */
      if (sent_requests)
        {
          xcb_get_property_reply_t *prop;

          prop = xcb_get_property_reply (xcb_conn, prop_cookie, NULL);
          if (prop)
            {
              if (prop->type == target_type
                  && prop->format == 32
                  && (xcb_get_property_value_length (prop)
		      == 4 * sizeof (int32_t)))
                {
                  int32_t *fe = xcb_get_property_value (prop);

                  outer_x = -fe[0];
                  outer_y = -fe[2];
                  real_x -= fe[0];
                  real_y -= fe[2];
                }
              free (prop);
            }
          /* Xlib version doesn't set had_errors here.  Intentional or bug?  */
        }
#else
      rc = XGetWindowProperty (dpy, win, dpyinfo->Xatom_net_frame_extents,
                               0, max_len, False, target_type,
                               &actual_type, &actual_format, &actual_size,
                               &bytes_remaining, &tmp_data);

      if (rc == Success && actual_type == target_type && !x_had_errors_p (dpy)
          && actual_size == 4 && actual_format == 32)
        {
          long *fe = (long *)tmp_data;

          outer_x = -fe[0];
          outer_y = -fe[2];
          real_x -= fe[0];
          real_y -= fe[2];
        }

      if (tmp_data) XFree (tmp_data);
#endif
    }

  if (right_offset_x || bottom_offset_y)
    {
#ifdef USE_XCB
      /* Make sure we didn't get an X error early and skip sending the
         request.  */
      if (sent_requests)
        {
          xcb_get_geometry_reply_t *outer_geom;

          outer_geom = xcb_get_geometry_reply (xcb_conn, outer_geom_cookie,
                                               NULL);
          if (outer_geom)
            {
              fw = outer_geom->width;
              fh = outer_geom->height;
              free (outer_geom);
            }
          else
	    had_errors = true;
        }
#else
      int xy_ign;
      unsigned int ign;
      Window rootw;

      XGetGeometry (dpy, FRAME_OUTER_WINDOW (f),
		    &rootw, &xy_ign, &xy_ign, &fw, &fh, &ign, &ign);
#endif
    }

#ifndef USE_XCB
  x_uncatch_errors ();
#endif

  unblock_input ();

  if (had_errors) return;

  if (x_pixels_diff) *x_pixels_diff = -win_x;
  if (y_pixels_diff) *y_pixels_diff = -win_y;

  if (left_offset_x) *left_offset_x = -outer_x;
  if (top_offset_y) *top_offset_y = -outer_y;

  if (xptr) *xptr = real_x;
  if (yptr) *yptr = real_y;

  if (outer_border) *outer_border = bw;

  if (right_offset_x) *right_offset_x = ow - fw + outer_x;
  if (bottom_offset_y) *bottom_offset_y = oh - fh + outer_y;
}

/* Store the screen positions of frame F into XPTR and YPTR.
   These are the positions of the containing window manager window,
   not Emacs's own window.  */

void
x_real_positions (struct frame *f, int *xptr, int *yptr)
{
  x_real_pos_and_offsets (f, NULL, NULL, NULL, NULL, NULL, NULL, xptr, yptr,
                          NULL);
}


/* Get the mouse position in frame relative coordinates.  */

void
x_relative_mouse_position (struct frame *f, int *x, int *y)
{
  Window root, dummy_window;
  int dummy;

  eassert (FRAME_X_P (f));

  block_input ();

  XQueryPointer (FRAME_X_DISPLAY (f),
                 DefaultRootWindow (FRAME_X_DISPLAY (f)),

                 /* The root window which contains the pointer.  */
                 &root,

                 /* Window pointer is on, not used  */
                 &dummy_window,

                 /* The position on that root window.  */
                 x, y,

                 /* x/y in dummy_window coordinates, not used.  */
                 &dummy, &dummy,

                 /* Modifier keys and pointer buttons, about which
                    we don't care.  */
                 (unsigned int *) &dummy);

  XTranslateCoordinates (FRAME_X_DISPLAY (f),

                         /* From-window, to-window.  */
                         FRAME_DISPLAY_INFO (f)->root_window,
                         FRAME_X_WINDOW (f),

                         /* From-position, to-position.  */
                         *x, *y, x, y,

                         /* Child of win.  */
                         &dummy_window);

  unblock_input ();
}

/* Gamma-correct COLOR on frame F.  */

void
gamma_correct (struct frame *f, XColor *color)
{
  if (f->gamma)
    {
      color->red = pow (color->red / 65535.0, f->gamma) * 65535.0 + 0.5;
      color->green = pow (color->green / 65535.0, f->gamma) * 65535.0 + 0.5;
      color->blue = pow (color->blue / 65535.0, f->gamma) * 65535.0 + 0.5;
    }
}


/* Decide if color named COLOR_NAME is valid for use on frame F.  If
   so, return the RGB values in COLOR.  If ALLOC_P,
   allocate the color.  Value is false if COLOR_NAME is invalid, or
   no color could be allocated.  */

bool
x_defined_color (struct frame *f, const char *color_name,
		 Emacs_Color *color, bool alloc_p, bool _makeIndex)
{
  bool success_p = false;
  Colormap cmap = FRAME_X_COLORMAP (f);

  block_input ();
#ifdef USE_GTK
  success_p = xg_check_special_colors (f, color_name, color);
#endif
  if (!success_p)
    success_p = x_parse_color (f, color_name, color) != 0;
  if (success_p && alloc_p)
    success_p = x_alloc_nearest_color (f, cmap, color);
  unblock_input ();

  return success_p;
}


/* Return the pixel color value for color COLOR_NAME on frame F.  If F
   is a monochrome frame, return MONO_COLOR regardless of what ARG says.
   Signal an error if color can't be allocated.  */

static int
x_decode_color (struct frame *f, Lisp_Object color_name, int mono_color)
{
  XColor cdef;

  CHECK_STRING (color_name);

#if false /* Don't do this.  It's wrong when we're not using the default
	     colormap, it makes freeing difficult, and it's probably not
	     an important optimization.  */
  if (strcmp (SDATA (color_name), "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (SDATA (color_name), "white") == 0)
    return WHITE_PIX_DEFAULT (f);
#endif

  /* Return MONO_COLOR for monochrome frames.  */
  if (FRAME_DISPLAY_INFO (f)->n_planes == 1)
    return mono_color;

  /* x_defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (x_defined_color (f, SSDATA (color_name), &cdef, true, false))
    return cdef.pixel;

  signal_error ("Undefined color", color_name);
}



/* Change the `wait-for-wm' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.
   See also the comment of wait_for_wm in struct x_output.  */

static void
x_set_wait_for_wm (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  f->output_data.x->wait_for_wm = !NILP (new_value);
}

static void
x_set_tool_bar_position (struct frame *f,
                         Lisp_Object new_value,
                         Lisp_Object old_value)
{
  Lisp_Object choice = list4 (Qleft, Qright, Qtop, Qbottom);

  if (!NILP (Fmemq (new_value, choice)))
    {
#ifdef USE_GTK
      if (!EQ (new_value, old_value))
	{
	  xg_change_toolbar_position (f, new_value);
	  fset_tool_bar_position (f, new_value);
	}
#else
      if (!EQ (new_value, Qtop))
	error ("The only supported tool bar position is top");
#endif
    }
  else
    wrong_choice (choice, new_value);
}

static void
x_set_inhibit_double_buffering (struct frame *f,
                                Lisp_Object new_value,
                                Lisp_Object old_value)
{
  block_input ();
  if (FRAME_X_WINDOW (f) && !EQ (new_value, old_value))
    {
      bool want_double_buffering = NILP (new_value);
      bool was_double_buffered = FRAME_X_DOUBLE_BUFFERED_P (f);
      /* font_drop_xrender_surfaces in xftfont does something only if
         we're double-buffered, so call font_drop_xrender_surfaces before
         and after any potential change.  One of the calls will end up
         being a no-op.  */
      if (want_double_buffering != was_double_buffered)
        font_drop_xrender_surfaces (f);
      if (FRAME_X_DOUBLE_BUFFERED_P (f) && !want_double_buffering)
        tear_down_x_back_buffer (f);
      else if (!FRAME_X_DOUBLE_BUFFERED_P (f) && want_double_buffering)
        set_up_x_back_buffer (f);
      if (FRAME_X_DOUBLE_BUFFERED_P (f) != was_double_buffered)
        {
          SET_FRAME_GARBAGED (f);
          font_drop_xrender_surfaces (f);
        }
    }
  unblock_input ();
}

/**
 * x_set_undecorated:
 *
 * Set frame F's `undecorated' parameter.  If non-nil, F's window-system
 * window is drawn without decorations, title, minimize/maximize boxes
 * and external borders.  This usually means that the window cannot be
 * dragged, resized, iconified, maximized or deleted with the mouse.  If
 * nil, draw the frame with all the elements listed above unless these
 * have been suspended via window manager settings.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_undecorated (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      FRAME_UNDECORATED (f) = NILP (new_value) ? false : true;
#ifdef USE_GTK
      xg_set_undecorated (f, new_value);
#else
      Display *dpy = FRAME_X_DISPLAY (f);
      PropMotifWmHints hints;
      Atom prop = XInternAtom (dpy, "_MOTIF_WM_HINTS", False);

      memset (&hints, 0, sizeof(hints));
      hints.flags = MWM_HINTS_DECORATIONS;
      hints.decorations = NILP (new_value) ? MWM_DECOR_ALL : 0;

      block_input ();
      /* For some reason the third and fourth arguments in the following
	 call must be identical: In the corresponding XGetWindowProperty
	 call in getMotifHints, xfwm has the third and seventh args both
	 display_info->atoms[MOTIF_WM_HINTS].  Obviously, YMMV.   */
      XChangeProperty (dpy, FRAME_OUTER_WINDOW (f), prop, prop, 32,
		       PropModeReplace, (unsigned char *) &hints,
		       PROP_MOTIF_WM_HINTS_ELEMENTS);
      unblock_input ();

#endif /* USE_GTK */
    }
}

/**
 * x_set_parent_frame:
 *
 * Set frame F's `parent-frame' parameter.  If non-nil, make F a child
 * frame of the frame specified by that parameter.  Technically, this
 * makes F's window-system window a child window of the parent frame's
 * window-system window.  If nil, make F's window-system window a
 * top-level window--a child of its display's root window.
 *
 * A child frame is clipped at the native edges of its parent frame.
 * Its `left' and `top' parameters specify positions relative to the
 * top-left corner of its parent frame's native rectangle.  Usually,
 * moving a parent frame moves all its child frames too, keeping their
 * position relative to the parent unaltered.  When a parent frame is
 * iconified or made invisible, its child frames are made invisible.
 * When a parent frame is deleted, its child frames are deleted too.
 *
 * A visible child frame always appears on top of its parent frame thus
 * obscuring parts of it.  When a frame has more than one child frame,
 * their stacking order is specified just as that of non-child frames
 * relative to their display.
 *
 * Whether a child frame has a menu or tool bar may be window-system or
 * window manager dependent.  It's advisable to disable both via the
 * frame parameter settings.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_parent_frame (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  struct frame *p = NULL;

  if (!NILP (new_value)
      && (!FRAMEP (new_value)
	  || !FRAME_LIVE_P (p = XFRAME (new_value))
	  || !FRAME_X_P (p)))
    {
      store_frame_param (f, Qparent_frame, old_value);
      error ("Invalid specification of `parent-frame'");
    }

  if (p != FRAME_PARENT_FRAME (f))
    {
      block_input ();
      XReparentWindow
	(FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
	 p ? FRAME_X_WINDOW (p) : DefaultRootWindow (FRAME_X_DISPLAY (f)),
	 f->left_pos, f->top_pos);
#ifdef USE_GTK
      if (EQ (x_gtk_resize_child_frames, Qresize_mode))
	gtk_container_set_resize_mode
	  (GTK_CONTAINER (FRAME_GTK_OUTER_WIDGET (f)),
	   p ? GTK_RESIZE_IMMEDIATE : GTK_RESIZE_QUEUE);
#endif
      unblock_input ();

      fset_parent_frame (f, new_value);
    }
}

/**
 * x_set_no_focus_on_map:
 *
 * Set frame F's `no-focus-on-map' parameter which, if non-nil, means
 * that F's window-system window does not want to receive input focus
 * when it is mapped.  (A frame's window is mapped when the frame is
 * displayed for the first time and when the frame changes its state
 * from `iconified' or `invisible' to `visible'.)
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_no_focus_on_map (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
#ifdef USE_GTK
      xg_set_no_focus_on_map (f, new_value);
#else /* not USE_GTK */
      Display *dpy = FRAME_X_DISPLAY (f);
      Atom prop = XInternAtom (dpy, "_NET_WM_USER_TIME", False);
      Time timestamp = NILP (new_value) ? CurrentTime : 0;

      XChangeProperty (dpy, FRAME_OUTER_WINDOW (f), prop,
		       XA_CARDINAL, 32, PropModeReplace,
		       (unsigned char *) &timestamp, 1);
#endif /* USE_GTK */
      FRAME_NO_FOCUS_ON_MAP (f) = !NILP (new_value);
    }
}

/**
 * x_set_no_accept_focus:
 *
 * Set frame F's `no-accept-focus' parameter which, if non-nil, hints
 * that F's window-system window does not want to receive input focus
 * via mouse clicks or by moving the mouse into it.
 *
 * If non-nil, this may have the unwanted side-effect that a user cannot
 * scroll a non-selected frame with the mouse.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_no_accept_focus (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
#ifdef USE_GTK
      xg_set_no_accept_focus (f, new_value);
#else /* not USE_GTK */
#ifdef USE_X_TOOLKIT
      Arg al[1];

      XtSetArg (al[0], XtNinput, NILP (new_value) ? True : False);
      XtSetValues (f->output_data.x->widget, al, 1);
#else /* not USE_X_TOOLKIT */
      Window window = FRAME_X_WINDOW (f);

      f->output_data.x->wm_hints.input = NILP (new_value) ? True : False;
      XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);
#endif /* USE_X_TOOLKIT */
#endif /* USE_GTK */
      FRAME_NO_ACCEPT_FOCUS (f) = !NILP (new_value);
    }
}

/**
 * x_set_override_redirect:
 *
 * Set frame F's `override_redirect' parameter which, if non-nil, hints
 * that the window manager doesn't want to deal with F.  Usually, such
 * frames have no decorations and always appear on top of all frames.
 *
 * Some window managers may not honor this parameter.
 */
static void
x_set_override_redirect (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
      /* Here (xfwm) override_redirect can be changed for invisible
	 frames only.  */
      x_make_frame_invisible (f);

#ifdef USE_GTK
      xg_set_override_redirect (f, new_value);
#else /* not USE_GTK */
      XSetWindowAttributes attributes;

      attributes.override_redirect = NILP (new_value) ? False : True;
      XChangeWindowAttributes (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
			       CWOverrideRedirect, &attributes);
#endif
      x_make_frame_visible (f);
      FRAME_OVERRIDE_REDIRECT (f) = !NILP (new_value);
    }
}


#ifdef USE_GTK

/* Set icon from FILE for frame F.  By using GTK functions the icon
   may be any format that GdkPixbuf knows about, i.e. not just bitmaps.  */

bool
xg_set_icon (struct frame *f, Lisp_Object file)
{
  bool result = false;
  Lisp_Object found;

  found = image_find_image_file (file);

  if (! NILP (found))
    {
      GdkPixbuf *pixbuf;
      GError *err = NULL;
      char *filename = SSDATA (ENCODE_FILE (found));
      block_input ();

      pixbuf = gdk_pixbuf_new_from_file (filename, &err);

      if (pixbuf)
	{
	  gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			       pixbuf);
	  g_object_unref (pixbuf);

	  result = true;
	}
      else
	g_error_free (err);

      unblock_input ();
    }

  return result;
}

bool
xg_set_icon_from_xpm_data (struct frame *f, const char **data)
{
  GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data (data);

  if (!pixbuf)
    return false;

  gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), pixbuf);
  g_object_unref (pixbuf);
  return true;
}
#endif /* USE_GTK */


/* Functions called only from `gui_set_frame_parameters'
   to set individual parameters.

   If FRAME_X_WINDOW (f) is 0,
   the frame is being created and its X-window does not exist yet.
   In that case, just record the parameter's new value
   in the standard place; do not attempt to change the window.  */

static void
x_set_foreground_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct x_output *x = f->output_data.x;
  unsigned long fg, old_fg;

  fg = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  old_fg = FRAME_FOREGROUND_PIXEL (f);
  FRAME_FOREGROUND_PIXEL (f) = fg;

  if (FRAME_X_WINDOW (f) != 0)
    {
      Display *dpy = FRAME_X_DISPLAY (f);

      block_input ();
      XSetForeground (dpy, x->normal_gc, fg);
      XSetBackground (dpy, x->reverse_gc, fg);

      if (x->cursor_pixel == old_fg)
	{
	  unload_color (f, x->cursor_pixel);
	  x->cursor_pixel = x_copy_color (f, fg);
	  XSetBackground (dpy, x->cursor_gc, x->cursor_pixel);
	}

      unblock_input ();

      update_face_from_frame_parameter (f, Qforeground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }

  unload_color (f, old_fg);
}

static void
x_set_background_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct x_output *x = f->output_data.x;
  unsigned long bg;

  bg = x_decode_color (f, arg, WHITE_PIX_DEFAULT (f));
  unload_color (f, FRAME_BACKGROUND_PIXEL (f));
  FRAME_BACKGROUND_PIXEL (f) = bg;

  if (FRAME_X_WINDOW (f) != 0)
    {
      Display *dpy = FRAME_X_DISPLAY (f);

      block_input ();
      XSetBackground (dpy, x->normal_gc, bg);
      XSetForeground (dpy, x->reverse_gc, bg);
      XSetWindowBackground (dpy, FRAME_X_WINDOW (f), bg);
      XSetForeground (dpy, x->cursor_gc, bg);

#ifdef USE_GTK
      xg_set_background_color (f, bg);
#endif

#ifndef USE_TOOLKIT_SCROLL_BARS /* Turns out to be annoying with
				   toolkit scroll bars.  */
      {
	Lisp_Object bar;
	for (bar = FRAME_SCROLL_BARS (f);
	     !NILP (bar);
	     bar = XSCROLL_BAR (bar)->next)
	  {
	    Window window = XSCROLL_BAR (bar)->x_window;
	    XSetWindowBackground (dpy, window, bg);
	  }
      }
#endif /* USE_TOOLKIT_SCROLL_BARS */

      unblock_input ();
      update_face_from_frame_parameter (f, Qbackground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

/* This array must stay in sync with the mouse_cursor_types array below!  */
enum mouse_cursor {
  mouse_cursor_text,
  mouse_cursor_nontext,
  mouse_cursor_hourglass,
  mouse_cursor_mode,
  mouse_cursor_hand,
  mouse_cursor_horizontal_drag,
  mouse_cursor_vertical_drag,
  mouse_cursor_left_edge,
  mouse_cursor_top_left_corner,
  mouse_cursor_top_edge,
  mouse_cursor_top_right_corner,
  mouse_cursor_right_edge,
  mouse_cursor_bottom_right_corner,
  mouse_cursor_bottom_edge,
  mouse_cursor_bottom_left_corner,
  mouse_cursor_max
};

struct mouse_cursor_types {
  /* Printable name for error messages (optional).  */
  const char *name;

  /* Lisp variable controlling the cursor shape.  */
  /* FIXME: A couple of these variables are defined in the C code but
     are not actually accessible from Lisp.  They should probably be
     made accessible or removed.  */
  Lisp_Object *shape_var_ptr;

  /* The default shape.  */
  int default_shape;
};

/* This array must stay in sync with enum mouse_cursor above!  */
static const struct mouse_cursor_types mouse_cursor_types[] = {
  { "text",      &Vx_pointer_shape,                    XC_xterm               },
  { "nontext",   &Vx_nontext_pointer_shape,            XC_left_ptr            },
  { "hourglass", &Vx_hourglass_pointer_shape,          XC_watch               },
  { "modeline",  &Vx_mode_pointer_shape,               XC_xterm               },
  { NULL,        &Vx_sensitive_text_pointer_shape,     XC_hand2               },
  { NULL,        &Vx_window_horizontal_drag_shape,     XC_sb_h_double_arrow   },
  { NULL,        &Vx_window_vertical_drag_shape,       XC_sb_v_double_arrow   },
  { NULL,        &Vx_window_left_edge_shape,           XC_left_side           },
  { NULL,        &Vx_window_top_left_corner_shape,     XC_top_left_corner     },
  { NULL,        &Vx_window_top_edge_shape,            XC_top_side            },
  { NULL,        &Vx_window_top_right_corner_shape,    XC_top_right_corner    },
  { NULL,        &Vx_window_right_edge_shape,          XC_right_side          },
  { NULL,        &Vx_window_bottom_right_corner_shape, XC_bottom_right_corner },
  { NULL,        &Vx_window_bottom_edge_shape,         XC_bottom_side         },
  { NULL,        &Vx_window_bottom_left_corner_shape,  XC_bottom_left_corner  },
};

struct mouse_cursor_data {
  /* Last index for which XCreateFontCursor has been called, and thus
     the last index for which x_request_serial[] is valid.  */
  int last_cursor_create_request;

  /* Last index for which an X error event was received in response to
     attempting to create the cursor.  */
  int error_cursor;

  /* Cursor numbers chosen.  */
  unsigned int cursor_num[mouse_cursor_max];

  /* Allocated Cursor values, or zero for failed attempts.  */
  Cursor cursor[mouse_cursor_max];

  /* X serial numbers for the first request sent by XCreateFontCursor.
     Note that there may be more than one request sent.  */
  unsigned long x_request_serial[mouse_cursor_max];

  /* If an error has been received, a pointer to where the current
     error-message text is stored.  */
  char *error_string;
};

static void
x_set_mouse_color_handler (Display *dpy, XErrorEvent *event,
			   char *error_string, void *data)
{
  struct mouse_cursor_data *cursor_data = data;
  int i;

  cursor_data->error_cursor = -1;
  cursor_data->error_string = error_string;
  for (i = 0; i < cursor_data->last_cursor_create_request; i++)
    {
      if (event->serial >= cursor_data->x_request_serial[i])
	cursor_data->error_cursor = i;
    }
  if (cursor_data->error_cursor >= 0)
    /* If we failed to allocate it, don't try to free it.  */
    cursor_data->cursor[cursor_data->error_cursor] = 0;
}

static void
x_set_mouse_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  struct x_output *x = f->output_data.x;
  Display *dpy = FRAME_X_DISPLAY (f);
  struct mouse_cursor_data cursor_data = { -1, -1 };
  unsigned long pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  unsigned long mask_color = FRAME_BACKGROUND_PIXEL (f);
  int i;

  /* Don't let pointers be invisible.  */
  if (mask_color == pixel)
    {
      x_free_colors (f, &pixel, 1);
      pixel = x_copy_color (f, FRAME_FOREGROUND_PIXEL (f));
    }

  unload_color (f, x->mouse_pixel);
  x->mouse_pixel = pixel;

  for (i = 0; i < mouse_cursor_max; i++)
    {
      Lisp_Object shape_var = *mouse_cursor_types[i].shape_var_ptr;
      if (!NILP (shape_var))
	{
	  CHECK_TYPE_RANGED_INTEGER (unsigned, shape_var);
	  cursor_data.cursor_num[i] = XFIXNUM (shape_var);
	}
      else
	cursor_data.cursor_num[i] = mouse_cursor_types[i].default_shape;
    }

  block_input ();

  /* It's not okay to crash if the user selects a screwy cursor.  */
  x_catch_errors_with_handler (dpy, x_set_mouse_color_handler, &cursor_data);

  for (i = 0; i < mouse_cursor_max; i++)
    {
      cursor_data.x_request_serial[i] = XNextRequest (dpy);
      cursor_data.last_cursor_create_request = i;
      cursor_data.cursor[i] = XCreateFontCursor (dpy,
						 cursor_data.cursor_num[i]);
    }

  /* Now sync up and process all received errors from cursor
     creation.  */
  if (x_had_errors_p (dpy))
    {
      const char *bad_cursor_name = NULL;
      /* Bounded by X_ERROR_MESSAGE_SIZE in xterm.c.  */
      size_t message_length = strlen (cursor_data.error_string);
      char *xmessage = alloca (1 + message_length);
      memcpy (xmessage, cursor_data.error_string, message_length);

      x_uncatch_errors ();

      /* Free any successfully created cursors.  */
      for (i = 0; i < mouse_cursor_max; i++)
	if (cursor_data.cursor[i] != 0)
	  XFreeCursor (dpy, cursor_data.cursor[i]);

      /* This should only be able to fail if the server's serial
	 number tracking is broken.  */
      if (cursor_data.error_cursor >= 0)
	bad_cursor_name = mouse_cursor_types[cursor_data.error_cursor].name;
      if (bad_cursor_name)
	error ("bad %s pointer cursor: %s", bad_cursor_name, xmessage);
      else
	error ("can't set cursor shape: %s", xmessage);
    }

  x_uncatch_errors_after_check ();

  {
    XColor colors[2]; /* 0=foreground, 1=background */

    colors[0].pixel = x->mouse_pixel;
    colors[1].pixel = mask_color;
    x_query_colors (f, colors, 2);

    for (i = 0; i < mouse_cursor_max; i++)
      XRecolorCursor (dpy, cursor_data.cursor[i], &colors[0], &colors[1]);
  }

  if (FRAME_X_WINDOW (f) != 0)
    {
      f->output_data.x->current_cursor = cursor_data.cursor[mouse_cursor_text];
      XDefineCursor (dpy, FRAME_X_WINDOW (f),
		     f->output_data.x->current_cursor);
    }

#define INSTALL_CURSOR(FIELD, SHORT_INDEX)				\
  eassert (x->FIELD != cursor_data.cursor[mouse_cursor_ ## SHORT_INDEX]); \
  if (x->FIELD != 0)							\
    XFreeCursor (dpy, x->FIELD);					\
  x->FIELD = cursor_data.cursor[mouse_cursor_ ## SHORT_INDEX];

  INSTALL_CURSOR (text_cursor, text);
  INSTALL_CURSOR (nontext_cursor, nontext);
  INSTALL_CURSOR (hourglass_cursor, hourglass);
  INSTALL_CURSOR (modeline_cursor, mode);
  INSTALL_CURSOR (hand_cursor, hand);
  INSTALL_CURSOR (horizontal_drag_cursor, horizontal_drag);
  INSTALL_CURSOR (vertical_drag_cursor, vertical_drag);
  INSTALL_CURSOR (left_edge_cursor, left_edge);
  INSTALL_CURSOR (top_left_corner_cursor, top_left_corner);
  INSTALL_CURSOR (top_edge_cursor, top_edge);
  INSTALL_CURSOR (top_right_corner_cursor, top_right_corner);
  INSTALL_CURSOR (right_edge_cursor, right_edge);
  INSTALL_CURSOR (bottom_right_corner_cursor, bottom_right_corner);
  INSTALL_CURSOR (bottom_edge_cursor, bottom_edge);
  INSTALL_CURSOR (bottom_left_corner_cursor, bottom_left_corner);

#undef INSTALL_CURSOR

  XFlush (dpy);
  unblock_input ();

  update_face_from_frame_parameter (f, Qmouse_color, arg);
}

static void
x_set_cursor_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  unsigned long fore_pixel, pixel;
  bool fore_pixel_allocated_p = false, pixel_allocated_p = false;
  struct x_output *x = f->output_data.x;

  if (!NILP (Vx_cursor_fore_pixel))
    {
      fore_pixel = x_decode_color (f, Vx_cursor_fore_pixel,
				   WHITE_PIX_DEFAULT (f));
      fore_pixel_allocated_p = true;
    }
  else
    fore_pixel = FRAME_BACKGROUND_PIXEL (f);

  pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  pixel_allocated_p = true;

  /* Make sure that the cursor color differs from the background color.  */
  if (pixel == FRAME_BACKGROUND_PIXEL (f))
    {
      if (pixel_allocated_p)
	{
	  x_free_colors (f, &pixel, 1);
	  pixel_allocated_p = false;
	}

      pixel = x->mouse_pixel;
      if (pixel == fore_pixel)
	{
	  if (fore_pixel_allocated_p)
	    {
	      x_free_colors (f, &fore_pixel, 1);
	      fore_pixel_allocated_p = false;
	    }
	  fore_pixel = FRAME_BACKGROUND_PIXEL (f);
	}
    }

  unload_color (f, x->cursor_foreground_pixel);
  if (!fore_pixel_allocated_p)
    fore_pixel = x_copy_color (f, fore_pixel);
  x->cursor_foreground_pixel = fore_pixel;

  unload_color (f, x->cursor_pixel);
  if (!pixel_allocated_p)
    pixel = x_copy_color (f, pixel);
  x->cursor_pixel = pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
      block_input ();
      XSetBackground (FRAME_X_DISPLAY (f), x->cursor_gc, x->cursor_pixel);
      XSetForeground (FRAME_X_DISPLAY (f), x->cursor_gc, fore_pixel);
      unblock_input ();

      if (FRAME_VISIBLE_P (f))
	{
	  gui_update_cursor (f, false);
	  gui_update_cursor (f, true);
	}
    }

  update_face_from_frame_parameter (f, Qcursor_color, arg);
}

/* Set the border-color of frame F to pixel value PIX.
   Note that this does not fully take effect if done before
   F has an x-window.  */

static void
x_set_border_pixel (struct frame *f, int pix)
{
  unload_color (f, f->output_data.x->border_pixel);
  f->output_data.x->border_pixel = pix;

  if (FRAME_X_WINDOW (f) != 0 && f->border_width > 0)
    {
      block_input ();
      XSetWindowBorder (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), pix);
      unblock_input ();

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

/* Set the border-color of frame F to value described by ARG.
   ARG can be a string naming a color.
   The border-color is used for the border that is drawn by the X server.
   Note that this does not fully take effect if done before
   F has an x-window; it must be redone when the window is created.

   Note: this is done in two routines because of the way X10 works.

   Note: under X11, this is normally the province of the window manager,
   and so emacs's border colors may be overridden.  */

static void
x_set_border_color (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int pix;

  CHECK_STRING (arg);
  pix = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  x_set_border_pixel (f, pix);
  update_face_from_frame_parameter (f, Qborder_color, arg);
}


static void
x_set_cursor_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  set_frame_cursor_types (f, arg);
}

static void
x_set_icon_type (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  bool result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!STRINGP (oldval) && NILP (oldval) == NILP (arg))
    return;

  block_input ();
  if (NILP (arg))
    result = x_text_icon (f,
			  SSDATA ((!NILP (f->icon_name)
				   ? f->icon_name
				   : f->name)));
  else
    result = FRAME_TERMINAL (f)->set_bitmap_icon_hook (f, arg);

  if (result)
    {
      unblock_input ();
      error ("No icon window available");
    }

  XFlush (FRAME_X_DISPLAY (f));
  unblock_input ();
}

static void
x_set_icon_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  bool result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!NILP (arg) || NILP (oldval))
    return;

  fset_icon_name (f, arg);

  if (f->output_data.x->icon_bitmap != 0)
    return;

  block_input ();

  result = x_text_icon (f,
			SSDATA ((!NILP (f->icon_name)
				 ? f->icon_name
				 : !NILP (f->title)
				 ? f->title
				 : f->name)));

  if (result)
    {
      unblock_input ();
      error ("No icon window available");
    }

  XFlush (FRAME_X_DISPLAY (f));
  unblock_input ();
}


static void
x_set_menu_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;
#if ! defined (USE_X_TOOLKIT) && ! defined (USE_GTK)
  int olines = FRAME_MENU_BAR_LINES (f);
#endif

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itself, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f) || FRAME_PARENT_FRAME (f))
    return;

  if (TYPE_RANGED_FIXNUMP (int, value))
    nlines = XFIXNUM (value);
  else
    nlines = 0;

  /* Make sure we redisplay all windows in this frame.  */
  fset_redisplay (f);

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
  FRAME_MENU_BAR_LINES (f) = 0;
  FRAME_MENU_BAR_HEIGHT (f) = 0;
  if (nlines)
    {
      FRAME_EXTERNAL_MENU_BAR (f) = 1;
      if (FRAME_X_P (f) && f->output_data.x->menubar_widget == 0)
	/* Make sure next redisplay shows the menu bar.  */
	XWINDOW (FRAME_SELECTED_WINDOW (f))->update_mode_line = true;
    }
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 1)
	free_frame_menubar (f);
      FRAME_EXTERNAL_MENU_BAR (f) = 0;
      if (FRAME_X_P (f))
	f->output_data.x->menubar_widget = 0;
    }
#else /* not USE_X_TOOLKIT && not USE_GTK */
  FRAME_MENU_BAR_LINES (f) = nlines;
  FRAME_MENU_BAR_HEIGHT (f) = nlines * FRAME_LINE_HEIGHT (f);
  adjust_frame_size (f, -1, -1, 2, true, Qx_set_menu_bar_lines);
  if (FRAME_X_WINDOW (f))
    x_clear_under_internal_border (f);

  /* If the menu bar height gets changed, the internal border below
     the top margin has to be cleared.  Also, if the menu bar gets
     larger, the area for the added lines has to be cleared except for
     the first menu bar line that is to be drawn later.  */
  if (nlines != olines)
    {
      int height = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int y;

      /* height can be zero here. */
      if (FRAME_X_WINDOW (f) && height > 0 && width > 0)
	{
	  y = FRAME_TOP_MARGIN_HEIGHT (f);

	  block_input ();
	  x_clear_area (f, 0, y, width, height);
	  unblock_input ();
	}

      if (nlines > 1 && nlines > olines)
	{
	  y = (olines == 0 ? 1 : olines) * FRAME_LINE_HEIGHT (f);
	  height = nlines * FRAME_LINE_HEIGHT (f) - y;

	  block_input ();
	  x_clear_area (f, 0, y, width, height);
	  unblock_input ();
	}

      if (nlines == 0 && WINDOWP (f->menu_bar_window))
	clear_glyph_matrix (XWINDOW (f->menu_bar_window)->current_matrix);
    }
#endif /* not USE_X_TOOLKIT && not USE_GTK */
  adjust_frame_glyphs (f);
}


/* Set the number of lines used for the tab bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL
   is the old number of tab bar lines.  This function changes the
   height of all windows on frame F to match the new tab bar height.
   The frame's height doesn't change.  */

static void
x_set_tab_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;

  /* Treat tab bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an int >= 0.  */
  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  x_change_tab_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}


/* Set the pixel height of the tab bar of frame F to HEIGHT.  */
void
x_change_tab_bar_height (struct frame *f, int height)
{
  int unit = FRAME_LINE_HEIGHT (f);
  int old_height = FRAME_TAB_BAR_HEIGHT (f);
  int lines = (height + unit - 1) / unit;
  Lisp_Object fullscreen;

  /* Make sure we redisplay all windows in this frame.  */
  fset_redisplay (f);

  /* Recalculate tab bar and frame text sizes.  */
  FRAME_TAB_BAR_HEIGHT (f) = height;
  FRAME_TAB_BAR_LINES (f) = lines;
  /* Store the `tab-bar-lines' and `height' frame parameters.  */
  store_frame_param (f, Qtab_bar_lines, make_fixnum (lines));
  store_frame_param (f, Qheight, make_fixnum (FRAME_LINES (f)));

  /* We also have to make sure that the internal border at the top of
     the frame, below the menu bar or tab bar, is redrawn when the
     tab bar disappears.  This is so because the internal border is
     below the tab bar if one is displayed, but is below the menu bar
     if there isn't a tab bar.  The tab bar draws into the area
     below the menu bar.  */
  if (FRAME_X_WINDOW (f) && FRAME_TAB_BAR_HEIGHT (f) == 0)
    {
      clear_frame (f);
      clear_current_matrices (f);
    }

  if ((height < old_height) && WINDOWP (f->tab_bar_window))
    clear_glyph_matrix (XWINDOW (f->tab_bar_window)->current_matrix);

  /* Recalculate tabbar height.  */
  f->n_tab_bar_rows = 0;
  if (old_height == 0
      && (!f->after_make_frame
	  || NILP (frame_inhibit_implied_resize)
	  || (CONSP (frame_inhibit_implied_resize)
	      && NILP (Fmemq (Qtab_bar_lines, frame_inhibit_implied_resize)))))
    f->tab_bar_redisplayed = f->tab_bar_resized = false;

  adjust_frame_size (f, -1, -1,
		     ((!f->tab_bar_resized
		       && (NILP (fullscreen =
				 get_frame_param (f, Qfullscreen))
			   || EQ (fullscreen, Qfullwidth))) ? 1
		      : (old_height == 0 || height == 0) ? 2
		      : 4),
		     false, Qtab_bar_lines);

  f->tab_bar_resized = f->tab_bar_redisplayed;

  /* adjust_frame_size might not have done anything, garbage frame
     here.  */
  adjust_frame_glyphs (f);
  SET_FRAME_GARBAGED (f);
  if (FRAME_X_WINDOW (f))
    x_clear_under_internal_border (f);
}


/* Set the number of lines used for the tool bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL
   is the old number of tool bar lines.  This function changes the
   height of all windows on frame F to match the new tool bar height.
   The frame's height doesn't change.  */

static void
x_set_tool_bar_lines (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  int nlines;

  /* Treat tool bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an int >= 0.  */
  if (RANGED_FIXNUMP (0, value, INT_MAX))
    nlines = XFIXNAT (value);
  else
    nlines = 0;

  x_change_tool_bar_height (f, nlines * FRAME_LINE_HEIGHT (f));
}


/* Set the pixel height of the tool bar of frame F to HEIGHT.  */
void
x_change_tool_bar_height (struct frame *f, int height)
{
#ifdef USE_GTK
  FRAME_TOOL_BAR_LINES (f) = 0;
  FRAME_TOOL_BAR_HEIGHT (f) = 0;
  if (height)
    {
      FRAME_EXTERNAL_TOOL_BAR (f) = true;
      if (FRAME_X_P (f) && f->output_data.x->toolbar_widget == 0)
	/* Make sure next redisplay shows the tool bar.  */
	XWINDOW (FRAME_SELECTED_WINDOW (f))->update_mode_line = true;
      update_frame_tool_bar (f);
    }
  else
    {
      if (FRAME_EXTERNAL_TOOL_BAR (f))
        free_frame_tool_bar (f);
      FRAME_EXTERNAL_TOOL_BAR (f) = false;
    }
#else /* !USE_GTK */
  int unit = FRAME_LINE_HEIGHT (f);
  int old_height = FRAME_TOOL_BAR_HEIGHT (f);
  int lines = (height + unit - 1) / unit;
  Lisp_Object fullscreen;

  /* Make sure we redisplay all windows in this frame.  */
  fset_redisplay (f);

  /* Recalculate tool bar and frame text sizes.  */
  FRAME_TOOL_BAR_HEIGHT (f) = height;
  FRAME_TOOL_BAR_LINES (f) = lines;
  /* Store the `tool-bar-lines' and `height' frame parameters.  */
  store_frame_param (f, Qtool_bar_lines, make_fixnum (lines));
  store_frame_param (f, Qheight, make_fixnum (FRAME_LINES (f)));

  /* We also have to make sure that the internal border at the top of
     the frame, below the menu bar or tool bar, is redrawn when the
     tool bar disappears.  This is so because the internal border is
     below the tool bar if one is displayed, but is below the menu bar
     if there isn't a tool bar.  The tool bar draws into the area
     below the menu bar.  */
  if (FRAME_X_WINDOW (f) && FRAME_TOOL_BAR_HEIGHT (f) == 0)
    {
      clear_frame (f);
      clear_current_matrices (f);
    }

  if ((height < old_height) && WINDOWP (f->tool_bar_window))
    clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);

  /* Recalculate toolbar height.  */
  f->n_tool_bar_rows = 0;
  if (old_height == 0
      && (!f->after_make_frame
	  || NILP (frame_inhibit_implied_resize)
	  || (CONSP (frame_inhibit_implied_resize)
	      && NILP (Fmemq (Qtool_bar_lines, frame_inhibit_implied_resize)))))
    f->tool_bar_redisplayed = f->tool_bar_resized = false;

  adjust_frame_size (f, -1, -1,
		     ((!f->tool_bar_resized
		       && (NILP (fullscreen =
				 get_frame_param (f, Qfullscreen))
			   || EQ (fullscreen, Qfullwidth))) ? 1
		      : (old_height == 0 || height == 0) ? 2
		      : 4),
		     false, Qtool_bar_lines);

  f->tool_bar_resized = f->tool_bar_redisplayed;

  /* adjust_frame_size might not have done anything, garbage frame
     here.  */
  adjust_frame_glyphs (f);
  SET_FRAME_GARBAGED (f);
  if (FRAME_X_WINDOW (f))
    x_clear_under_internal_border (f);

#endif /* USE_GTK */
}


static void
x_set_internal_border_width (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  int border;

  CHECK_TYPE_RANGED_INTEGER (int, arg);
  border = max (XFIXNUM (arg), 0);

  if (border != FRAME_INTERNAL_BORDER_WIDTH (f))
    {
      f->internal_border_width = border;

#ifdef USE_X_TOOLKIT
      if (FRAME_X_OUTPUT (f)->edit_widget)
	widget_store_internal_border (FRAME_X_OUTPUT (f)->edit_widget);
#endif

      if (FRAME_X_WINDOW (f))
	{
	  adjust_frame_size (f, -1, -1, 3, false, Qinternal_border_width);
	  x_clear_under_internal_border (f);
	}
    }

}


/* Set the foreground color for scroll bars on frame F to VALUE.
   VALUE should be a string, a color name.  If it isn't a string or
   isn't a valid color name, do nothing.  OLDVAL is the old value of
   the frame parameter.  */

static void
x_set_scroll_bar_foreground (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  unsigned long pixel;

  if (STRINGP (value))
    pixel = x_decode_color (f, value, BLACK_PIX_DEFAULT (f));
  else
    pixel = -1;

  if (f->output_data.x->scroll_bar_foreground_pixel != -1)
    unload_color (f, f->output_data.x->scroll_bar_foreground_pixel);

  f->output_data.x->scroll_bar_foreground_pixel = pixel;
  if (FRAME_X_WINDOW (f) && FRAME_VISIBLE_P (f))
    {
      /* Remove all scroll bars because they have wrong colors.  */
      if (FRAME_TERMINAL (f)->condemn_scroll_bars_hook)
	(*FRAME_TERMINAL (f)->condemn_scroll_bars_hook) (f);
      if (FRAME_TERMINAL (f)->judge_scroll_bars_hook)
	(*FRAME_TERMINAL (f)->judge_scroll_bars_hook) (f);

      update_face_from_frame_parameter (f, Qscroll_bar_foreground, value);
      redraw_frame (f);
    }
}


/* Set the background color for scroll bars on frame F to VALUE VALUE
   should be a string, a color name.  If it isn't a string or isn't a
   valid color name, do nothing.  OLDVAL is the old value of the frame
   parameter.  */

static void
x_set_scroll_bar_background (struct frame *f, Lisp_Object value, Lisp_Object oldval)
{
  unsigned long pixel;

  if (STRINGP (value))
    pixel = x_decode_color (f, value, WHITE_PIX_DEFAULT (f));
  else
    pixel = -1;

  if (f->output_data.x->scroll_bar_background_pixel != -1)
    unload_color (f, f->output_data.x->scroll_bar_background_pixel);

#if defined (USE_LUCID) && defined (USE_TOOLKIT_SCROLL_BARS)
  /* Scrollbar shadow colors.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel != -1)
    {
      unload_color (f, f->output_data.x->scroll_bar_top_shadow_pixel);
      f->output_data.x->scroll_bar_top_shadow_pixel = -1;
    }
  if (f->output_data.x->scroll_bar_bottom_shadow_pixel != -1)
    {
      unload_color (f, f->output_data.x->scroll_bar_bottom_shadow_pixel);
      f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
    }
#endif /* USE_LUCID && USE_TOOLKIT_SCROLL_BARS */

  f->output_data.x->scroll_bar_background_pixel = pixel;
  if (FRAME_X_WINDOW (f) && FRAME_VISIBLE_P (f))
    {
      /* Remove all scroll bars because they have wrong colors.  */
      if (FRAME_TERMINAL (f)->condemn_scroll_bars_hook)
	(*FRAME_TERMINAL (f)->condemn_scroll_bars_hook) (f);
      if (FRAME_TERMINAL (f)->judge_scroll_bars_hook)
	(*FRAME_TERMINAL (f)->judge_scroll_bars_hook) (f);

      update_face_from_frame_parameter (f, Qscroll_bar_background, value);
      redraw_frame (f);
    }
}


/* Encode Lisp string STRING as a text in a format appropriate for
   XICCC (X Inter Client Communication Conventions).

   If STRING contains only ASCII characters, do no conversion and
   return the string data of STRING.  Otherwise, encode the text by
   CODING_SYSTEM, and return a newly allocated memory area which
   should be freed by `xfree' by a caller.

   Store the byte length of resulting text in *TEXT_BYTES.

   If the text contains only ASCII and Latin-1, store true in *STRING_P,
   which means that the `encoding' of the result can be `STRING'.
   Otherwise store false in *STRINGP, which means that the `encoding' of
   the result should be `COMPOUND_TEXT'.  */

static unsigned char *
x_encode_text (Lisp_Object string, Lisp_Object coding_system,
	       ptrdiff_t *text_bytes, bool *stringp, bool *freep)
{
  int result = string_xstring_p (string);
  struct coding_system coding;

  if (result == 0)
    {
      /* No multibyte character in OBJ.  We need not encode it.  */
      *text_bytes = SBYTES (string);
      *stringp = true;
      *freep = false;
      return SDATA (string);
    }

  setup_coding_system (coding_system, &coding);
  coding.mode |= (CODING_MODE_SAFE_ENCODING | CODING_MODE_LAST_BLOCK);
  /* We suppress producing escape sequences for composition.  */
  coding.common_flags &= ~CODING_ANNOTATION_MASK;
  coding.destination = xnmalloc (SCHARS (string), 2);
  coding.dst_bytes = SCHARS (string) * 2;
  encode_coding_object (&coding, string, 0, 0,
			SCHARS (string), SBYTES (string), Qnil);
  *text_bytes = coding.produced;
  *stringp = (result == 1 || !EQ (coding_system, Qcompound_text));
  *freep = true;
  return coding.destination;
}


/* Set the WM name to NAME for frame F. Also set the icon name.
   If the frame already has an icon name, use that, otherwise set the
   icon name to NAME.  */

static void
x_set_name_internal (struct frame *f, Lisp_Object name)
{
  if (FRAME_X_WINDOW (f))
    {
      block_input ();
      {
	XTextProperty text, icon;
	ptrdiff_t bytes;
	bool stringp;
	bool do_free_icon_value = false, do_free_text_value = false;
	Lisp_Object coding_system;
	Lisp_Object encoded_name;
	Lisp_Object encoded_icon_name;

	/* As ENCODE_UTF_8 may cause GC and relocation of string data,
	   we use it before x_encode_text that may return string data.  */
	encoded_name = ENCODE_UTF_8 (name);

	coding_system = Qcompound_text;
	/* Note: Encoding strategy

	   We encode NAME by compound-text and use "COMPOUND-TEXT" in
	   text.encoding.  But, there are non-internationalized window
	   managers which don't support that encoding.  So, if NAME
	   contains only ASCII and 8859-1 characters, encode it by
	   iso-latin-1, and use "STRING" in text.encoding hoping that
	   such window managers at least analyze this format correctly,
	   i.e. treat 8-bit bytes as 8859-1 characters.

	   We may also be able to use "UTF8_STRING" in text.encoding
	   in the future which can encode all Unicode characters.
	   But, for the moment, there's no way to know that the
	   current window manager supports it or not.

	   Either way, we also set the _NET_WM_NAME and _NET_WM_ICON_NAME
	   properties.  Per the EWMH specification, those two properties
	   are always UTF8_STRING.  This matches what gtk_window_set_title()
	   does in the USE_GTK case. */
	text.value = x_encode_text (name, coding_system, &bytes,
				    &stringp, &do_free_text_value);
	text.encoding = (stringp ? XA_STRING
			 : FRAME_DISPLAY_INFO (f)->Xatom_COMPOUND_TEXT);
	text.format = 8;
	text.nitems = bytes;

	if (!STRINGP (f->icon_name))
	  {
	    icon = text;
	    encoded_icon_name = encoded_name;
	  }
	else
	  {
	    /* See the above comment "Note: Encoding strategy".  */
	    icon.value = x_encode_text (f->icon_name, coding_system, &bytes,
					&stringp, &do_free_icon_value);
	    icon.encoding = (stringp ? XA_STRING
			     : FRAME_DISPLAY_INFO (f)->Xatom_COMPOUND_TEXT);
	    icon.format = 8;
	    icon.nitems = bytes;

	    encoded_icon_name = ENCODE_UTF_8 (f->icon_name);
	  }

#ifdef USE_GTK
        gtk_window_set_title (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
                              SSDATA (encoded_name));
#else /* not USE_GTK */
	XSetWMName (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), &text);
	XChangeProperty (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
			 FRAME_DISPLAY_INFO (f)->Xatom_net_wm_name,
			 FRAME_DISPLAY_INFO (f)->Xatom_UTF8_STRING,
			 8, PropModeReplace,
			 SDATA (encoded_name),
			 SBYTES (encoded_name));
#endif /* not USE_GTK */

	XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), &icon);
	XChangeProperty (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
			 FRAME_DISPLAY_INFO (f)->Xatom_net_wm_icon_name,
			 FRAME_DISPLAY_INFO (f)->Xatom_UTF8_STRING,
			 8, PropModeReplace,
			 SDATA (encoded_icon_name),
			 SBYTES (encoded_icon_name));

	if (do_free_icon_value)
	  xfree (icon.value);
	if (do_free_text_value)
	  xfree (text.value);
      }
      unblock_input ();
    }
}

/* Change the name of frame F to NAME.  If NAME is nil, set F's name to
       x_id_name.

   If EXPLICIT is true, that indicates that lisp code is setting the
       name; if NAME is a string, set F's name to NAME and set
       F->explicit_name; if NAME is Qnil, then clear F->explicit_name.

   If EXPLICIT is false, that indicates that Emacs redisplay code is
       suggesting a new name, which lisp code should override; if
       F->explicit_name is set, ignore the new name; otherwise, set it.  */

static void
x_set_name (struct frame *f, Lisp_Object name, bool explicit)
{
  /* Make sure that requests from lisp code override requests from
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
	 update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
	update_mode_lines = 37;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  /* If NAME is nil, set the name to the x_id_name.  */
  if (NILP (name))
    {
      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (!strcmp (FRAME_DISPLAY_INFO (f)->x_id_name,
		   SSDATA (f->name)))
	return;
      name = build_string (FRAME_DISPLAY_INFO (f)->x_id_name);
    }
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  fset_name (f, name);

  /* For setting the frame title, the title parameter should override
     the name parameter.  */
  if (! NILP (f->title))
    name = f->title;

  x_set_name_internal (f, name);
}

/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
static void
x_explicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  x_set_name (f, arg, true);
}

/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
x_implicitly_set_name (struct frame *f, Lisp_Object arg, Lisp_Object oldval)
{
  x_set_name (f, arg, false);
}

/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.  */

static void
x_set_title (struct frame *f, Lisp_Object name, Lisp_Object old_name)
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 38;

  fset_title (f, name);

  if (NILP (name))
    name = f->name;
  else
    CHECK_STRING (name);

  x_set_name_internal (f, name);
}

void
x_set_scroll_bar_default_width (struct frame *f)
{
  int unit = FRAME_COLUMN_WIDTH (f);
#ifdef USE_TOOLKIT_SCROLL_BARS
#ifdef USE_GTK
  int minw = xg_get_default_scrollbar_width (f);
#else
  int minw = 16;
#endif
  /* A minimum width of 14 doesn't look good for toolkit scroll bars.  */
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (minw + unit - 1) / unit;
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f) = minw;
#else
  /* The width of a non-toolkit scrollbar is 14 pixels.  */
  FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + unit - 1) / unit;
  FRAME_CONFIG_SCROLL_BAR_WIDTH (f)
    = FRAME_CONFIG_SCROLL_BAR_COLS (f) * unit;
#endif
}

void
x_set_scroll_bar_default_height (struct frame *f)
{
  int height = FRAME_LINE_HEIGHT (f);
#ifdef USE_TOOLKIT_SCROLL_BARS
#ifdef USE_GTK
  int min_height = xg_get_default_scrollbar_height (f);
#else
  int min_height = 16;
#endif
  /* A minimum height of 14 doesn't look good for toolkit scroll bars.  */
  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = min_height;
  FRAME_CONFIG_SCROLL_BAR_LINES (f) = (min_height + height - 1) / height;
#else
  /* The height of a non-toolkit scrollbar is 14 pixels.  */
  FRAME_CONFIG_SCROLL_BAR_LINES (f) = (14 + height - 1) / height;

  /* Use all of that space (aside from required margins) for the
     scroll bar.  */
  FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) = 14;
#endif
}


/* Record in frame F the specified or default value according to ALIST
   of the parameter named PROP (a Lisp symbol).  If no value is
   specified for PROP, look for an X default for XPROP on the frame
   named NAME.  If that is not found either, use the value DEFLT.  */

static Lisp_Object
x_default_scroll_bar_color_parameter (struct frame *f,
				      Lisp_Object alist, Lisp_Object prop,
				      const char *xprop, const char *xclass,
				      bool foreground_p)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Lisp_Object tem;

  tem = gui_display_get_arg (dpyinfo, alist, prop, xprop, xclass,
                             RES_TYPE_STRING);
  if (EQ (tem, Qunbound))
    {
#ifdef USE_TOOLKIT_SCROLL_BARS

      /* See if an X resource for the scroll bar color has been
	 specified.  */
      AUTO_STRING (foreground, "foreground");
      AUTO_STRING (background, "foreground");
      AUTO_STRING (verticalScrollBar, "verticalScrollBar");
      tem = (gui_display_get_resource
	     (dpyinfo, foreground_p ? foreground : background,
	      empty_unibyte_string,
	      verticalScrollBar,
	      empty_unibyte_string));
      if (!STRINGP (tem))
	{
	  /* If nothing has been specified, scroll bars will use a
	     toolkit-dependent default.  Because these defaults are
	     difficult to get at without actually creating a scroll
	     bar, use nil to indicate that no color has been
	     specified.  */
	  tem = Qnil;
	}

#else /* not USE_TOOLKIT_SCROLL_BARS */

      tem = Qnil;

#endif /* not USE_TOOLKIT_SCROLL_BARS */
    }

  AUTO_FRAME_ARG (arg, prop, tem);
  gui_set_frame_parameters (f, arg);
  return tem;
}




#ifdef USE_X_TOOLKIT

/* If the WM_PROTOCOLS property does not already contain WM_TAKE_FOCUS,
   WM_DELETE_WINDOW, and WM_SAVE_YOURSELF, then add them.  (They may
   already be present because of the toolkit (Motif adds some of them,
   for example, but Xt doesn't).  */

static void
hack_wm_protocols (struct frame *f, Widget widget)
{
  Display *dpy = XtDisplay (widget);
  Window w = XtWindow (widget);
  bool need_delete = true;
  bool need_focus = true;
  bool need_save = true;

  block_input ();
  {
    Atom type;
    unsigned char *catoms;
    int format = 0;
    unsigned long nitems = 0;
    unsigned long bytes_after;

    if ((XGetWindowProperty (dpy, w,
			     FRAME_DISPLAY_INFO (f)->Xatom_wm_protocols,
			     0, 100, False, XA_ATOM,
			     &type, &format, &nitems, &bytes_after,
			     &catoms)
	 == Success)
	&& format == 32 && type == XA_ATOM)
      {
	Atom *atoms = (Atom *) catoms;
	while (nitems > 0)
	  {
	    nitems--;
	    if (atoms[nitems]
		== FRAME_DISPLAY_INFO (f)->Xatom_wm_delete_window)
	      need_delete = false;
	    else if (atoms[nitems]
		     == FRAME_DISPLAY_INFO (f)->Xatom_wm_take_focus)
	      need_focus = false;
	    else if (atoms[nitems]
		     == FRAME_DISPLAY_INFO (f)->Xatom_wm_save_yourself)
	      need_save = false;
	  }
      }
    if (catoms)
      XFree (catoms);
  }
  {
    Atom props[10];
    int count = 0;
    if (need_delete)
      props[count++] = FRAME_DISPLAY_INFO (f)->Xatom_wm_delete_window;
    if (need_focus)
      props[count++] = FRAME_DISPLAY_INFO (f)->Xatom_wm_take_focus;
    if (need_save)
      props[count++] = FRAME_DISPLAY_INFO (f)->Xatom_wm_save_yourself;
    if (count)
      XChangeProperty (dpy, w, FRAME_DISPLAY_INFO (f)->Xatom_wm_protocols,
		       XA_ATOM, 32, PropModeAppend,
		       (unsigned char *) props, count);
  }
  unblock_input ();
}
#endif



/* Support routines for XIC (X Input Context).  */

#ifdef HAVE_X_I18N

static XFontSet xic_create_xfontset (struct frame *);
static XIMStyle best_xim_style (XIMStyles *);


/* Supported XIM styles, ordered by preference.  */

static const XIMStyle supported_xim_styles[] =
{
  XIMPreeditPosition | XIMStatusArea,
  XIMPreeditPosition | XIMStatusNothing,
  XIMPreeditPosition | XIMStatusNone,
  XIMPreeditNothing | XIMStatusArea,
  XIMPreeditNothing | XIMStatusNothing,
  XIMPreeditNothing | XIMStatusNone,
  XIMPreeditNone | XIMStatusArea,
  XIMPreeditNone | XIMStatusNothing,
  XIMPreeditNone | XIMStatusNone,
  0,
};


#if defined HAVE_X_WINDOWS && defined USE_X_TOOLKIT
/* Create an X fontset on frame F with base font name BASE_FONTNAME.  */

static const char xic_default_fontset[] = "-*-*-*-r-normal--14-*-*-*-*-*-*-*";

/* Create an Xt fontset spec from the name of a base font.
   If `motif' is True use the Motif syntax.  */
char *
xic_create_fontsetname (const char *base_fontname, bool motif)
{
  const char *sep = motif ? ";" : ",";
  char *fontsetname;
  char *z;

  /* Make a fontset name from the base font name.  */
  if (xic_default_fontset == base_fontname)
    {
      /* There is no base font name, use the default.  */
      fontsetname = xmalloc (strlen (base_fontname) + 2);
      z = stpcpy (fontsetname, base_fontname);
    }
  else
    {
      /* Make a fontset name from the base font name.
	 The font set will be made of the following elements:
	 - the base font.
	 - the base font where the charset spec is replaced by -*-*.
	 - the same but with the family also replaced with -*-*-.  */
      const char *p = base_fontname;
      ptrdiff_t i;

      for (i = 0; *p; p++)
	if (*p == '-') i++;
      if (i != 14)
	{
	  /* As the font name doesn't conform to XLFD, we can't
	     modify it to generalize it to allcs and allfamilies.
	     Use the specified font plus the default.  */
	  fontsetname = xmalloc (strlen (base_fontname)
				 + strlen (xic_default_fontset) + 3);
	  z = stpcpy (fontsetname, base_fontname);
	  z = stpcpy (z, sep);
	  z = stpcpy (z, xic_default_fontset);
	}
      else
	{
	  ptrdiff_t len;
	  const char *p1 = NULL, *p2 = NULL, *p3 = NULL;
	  char *font_allcs = NULL;
	  char *font_allfamilies = NULL;
	  char *font_all = NULL;
	  const char *allcs = "*-*-*-*-*-*-*";
	  const char *allfamilies = "-*-*-";
	  const char *all = "*-*-*-*-";
	  char *base;

	  for (i = 0, p = base_fontname; i < 8; p++)
	    {
	      if (*p == '-')
		{
		  i++;
		  if (i == 3)
		    p1 = p + 1;
		  else if (i == 7)
		    p2 = p + 1;
		  else if (i == 6)
		    p3 = p + 1;
		}
	    }
	  /* If base_fontname specifies ADSTYLE, make it a
	     wildcard.  */
	  if (*p3 != '*')
	    {
	      ptrdiff_t diff = (p2 - p3) - 2;

	      base = alloca (strlen (base_fontname) + 1);
	      memcpy (base, base_fontname, p3 - base_fontname);
	      base[p3 - base_fontname] = '*';
	      base[(p3 - base_fontname) + 1] = '-';
	      strcpy (base + (p3 - base_fontname) + 2, p2);
	      p = base + (p - base_fontname) - diff;
	      p1 = base + (p1 - base_fontname);
	      p2 = base + (p2 - base_fontname) - diff;
	      base_fontname = base;
	    }

	  /* Build the font spec that matches all charsets.  */
	  len = p - base_fontname + strlen (allcs) + 1;
	  font_allcs = alloca (len);
	  memcpy (font_allcs, base_fontname, p - base_fontname);
	  strcpy (font_allcs + (p - base_fontname), allcs);

	  /* Build the font spec that matches all families and
	     add-styles.  */
	  len = p - p1 + strlen (allcs) + strlen (allfamilies) + 1;
	  font_allfamilies = alloca (len);
	  strcpy (font_allfamilies, allfamilies);
	  memcpy (font_allfamilies + strlen (allfamilies), p1, p - p1);
	  strcpy (font_allfamilies + strlen (allfamilies) + (p - p1), allcs);

	  /* Build the font spec that matches all.  */
	  len = p - p2 + strlen (allcs) + strlen (all) + strlen (allfamilies) + 1;
	  font_all = alloca (len);
	  z = stpcpy (font_all, allfamilies);
	  z = stpcpy (z, all);
	  memcpy (z, p2, p - p2);
	  strcpy (z + (p - p2), allcs);

	  /* Build the actual font set name.  */
	  len = strlen (base_fontname) + strlen (font_allcs)
	    + strlen (font_allfamilies) + strlen (font_all) + 5;
	  fontsetname = xmalloc (len);
	  z = stpcpy (fontsetname, base_fontname);
	  z = stpcpy (z, sep);
	  z = stpcpy (z, font_allcs);
	  z = stpcpy (z, sep);
	  z = stpcpy (z, font_allfamilies);
	  z = stpcpy (z, sep);
	  z = stpcpy (z, font_all);
	}
    }
  if (motif)
    strcpy (z, ":");
  return fontsetname;
}
#endif /* HAVE_X_WINDOWS && USE_X_TOOLKIT */

#ifdef DEBUG_XIC_FONTSET
static void
print_fontset_result (XFontSet xfs, char *name, char **missing_list,
		      int missing_count)
{
  if (xfs)
    fprintf (stderr, "XIC Fontset created: %s\n", name);
  else
    {
      fprintf (stderr, "XIC Fontset failed: %s\n", name);
      while (missing_count-- > 0)
	{
	  fprintf (stderr, "  missing: %s\n", *missing_list);
	  missing_list++;
	}
    }

}
#endif

static XFontSet
xic_create_xfontset (struct frame *f)
{
  XFontSet xfs = NULL;
  struct font *font = FRAME_FONT (f);
  int pixel_size = font->pixel_size;
  Lisp_Object rest, frame;

  /* See if there is another frame already using same fontset.  */
  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *cf = XFRAME (frame);

      if (cf != f && FRAME_LIVE_P (f) && FRAME_X_P (cf)
          && FRAME_DISPLAY_INFO (cf) == FRAME_DISPLAY_INFO (f)
	  && FRAME_FONT (f)
	  && FRAME_FONT (f)->pixel_size == pixel_size)
        {
          xfs = FRAME_XIC_FONTSET (cf);
          break;
        }
    }

  if (! xfs)
    {
      char buf[256];
      char **missing_list;
      int missing_count;
      char *def_string;
      const char *xlfd_format = "-*-*-medium-r-normal--%d-*-*-*-*-*";

      sprintf (buf, xlfd_format, pixel_size);
      missing_list = NULL;
      xfs = XCreateFontSet (FRAME_X_DISPLAY (f), buf,
			    &missing_list, &missing_count, &def_string);
#ifdef DEBUG_XIC_FONTSET
      print_fontset_result (xfs, buf, missing_list, missing_count);
#endif
      if (missing_list)
	XFreeStringList (missing_list);
      if (! xfs)
	{
	  /* List of pixel sizes most likely available.  Find one that
	     is closest to pixel_size.  */
	  int sizes[] = {0, 8, 10, 11, 12, 14, 17, 18, 20, 24, 26, 34, 0};
	  int *smaller, *larger;

	  for (smaller = sizes; smaller[1]; smaller++)
	    if (smaller[1] >= pixel_size)
	      break;
	  larger = smaller + 1;
	  if (*larger == pixel_size)
	    larger++;
	  while (*smaller || *larger)
	    {
	      int this_size;

	      if (! *larger)
		this_size = *smaller--;
	      else if (! *smaller)
		this_size = *larger++;
	      else if (pixel_size - *smaller < *larger - pixel_size)
		this_size = *smaller--;
	      else
		this_size = *larger++;
	      sprintf (buf, xlfd_format, this_size);
	      missing_list = NULL;
	      xfs = XCreateFontSet (FRAME_X_DISPLAY (f), buf,
				    &missing_list, &missing_count, &def_string);
#ifdef DEBUG_XIC_FONTSET
	      print_fontset_result (xfs, buf, missing_list, missing_count);
#endif
	      if (missing_list)
		XFreeStringList (missing_list);
	      if (xfs)
		break;
	    }
	}
      if (! xfs)
	{
	  const char *last_resort = "-*-*-*-r-normal--*-*-*-*-*-*";

	  missing_list = NULL;
	  xfs = XCreateFontSet (FRAME_X_DISPLAY (f), last_resort,
				&missing_list, &missing_count, &def_string);
#ifdef DEBUG_XIC_FONTSET
	  print_fontset_result (xfs, last_resort, missing_list, missing_count);
#endif
	  if (missing_list)
	    XFreeStringList (missing_list);
	}

    }

  return xfs;
}

/* Free the X fontset of frame F if it is the last frame using it.  */

void
xic_free_xfontset (struct frame *f)
{
  Lisp_Object rest, frame;
  bool shared_p = false;

  if (!FRAME_XIC_FONTSET (f))
    return;

  /* See if there is another frame sharing the same fontset.  */
  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *cf = XFRAME (frame);
      if (cf != f && FRAME_LIVE_P (f) && FRAME_X_P (cf)
          && FRAME_DISPLAY_INFO (cf) == FRAME_DISPLAY_INFO (f)
          && FRAME_XIC_FONTSET (cf) == FRAME_XIC_FONTSET (f))
        {
          shared_p = true;
          break;
        }
    }

  if (!shared_p)
    /* The fontset is not used anymore.  It is safe to free it.  */
    XFreeFontSet (FRAME_X_DISPLAY (f), FRAME_XIC_FONTSET (f));

  FRAME_XIC_FONTSET (f) = NULL;
}


/* Value is the best input style, given user preferences USER (already
   checked to be supported by Emacs), and styles supported by the
   input method XIM.  */

static XIMStyle
best_xim_style (XIMStyles *xim)
{
  int i, j;
  int nr_supported = ARRAYELTS (supported_xim_styles);

  for (i = 0; i < nr_supported; ++i)
    for (j = 0; j < xim->count_styles; ++j)
      if (supported_xim_styles[i] == xim->supported_styles[j])
	return supported_xim_styles[i];

  /* Return the default style.  */
  return XIMPreeditNothing | XIMStatusNothing;
}

/* Create XIC for frame F. */

void
create_frame_xic (struct frame *f)
{
  XIM xim;
  XIC xic = NULL;
  XFontSet xfs = NULL;
  XVaNestedList status_attr = NULL;
  XVaNestedList preedit_attr = NULL;
  XRectangle s_area;
  XPoint spot;
  XIMStyle xic_style;

  if (FRAME_XIC (f))
    goto out;

  xim = FRAME_X_XIM (f);
  if (!xim || ! FRAME_X_XIM_STYLES(f))
    goto out;

  /* Determine XIC style.  */
  xic_style = best_xim_style (FRAME_X_XIM_STYLES (f));

  /* Create X fontset. */
  if (xic_style & (XIMPreeditPosition | XIMStatusArea))
    {
      xfs = xic_create_xfontset (f);
      if (!xfs)
        goto out;

      FRAME_XIC_FONTSET (f) = xfs;
    }

  if (xic_style & XIMPreeditPosition)
    {
      spot.x = 0; spot.y = 1;
      preedit_attr = XVaCreateNestedList (0,
					  XNFontSet, xfs,
					  XNForeground,
					  FRAME_FOREGROUND_PIXEL (f),
					  XNBackground,
					  FRAME_BACKGROUND_PIXEL (f),
					  (xic_style & XIMPreeditPosition
					   ? XNSpotLocation
					   : NULL),
					  &spot,
					  NULL);

      if (!preedit_attr)
        goto out;
    }

  if (xic_style & XIMStatusArea)
    {
      s_area.x = 0; s_area.y = 0; s_area.width = 1; s_area.height = 1;
      status_attr = XVaCreateNestedList (0,
                                         XNArea,
                                         &s_area,
                                         XNFontSet,
                                         xfs,
                                         XNForeground,
                                         FRAME_FOREGROUND_PIXEL (f),
                                         XNBackground,
                                         FRAME_BACKGROUND_PIXEL (f),
                                         NULL);

      if (!status_attr)
        goto out;
    }

  if (preedit_attr && status_attr)
    xic = XCreateIC (xim,
                     XNInputStyle, xic_style,
                     XNClientWindow, FRAME_X_WINDOW (f),
                     XNFocusWindow, FRAME_X_WINDOW (f),
                     XNStatusAttributes, status_attr,
                     XNPreeditAttributes, preedit_attr,
                     NULL);
  else if (preedit_attr)
    xic = XCreateIC (xim,
                     XNInputStyle, xic_style,
                     XNClientWindow, FRAME_X_WINDOW (f),
                     XNFocusWindow, FRAME_X_WINDOW (f),
                     XNPreeditAttributes, preedit_attr,
                     NULL);
  else if (status_attr)
    xic = XCreateIC (xim,
                     XNInputStyle, xic_style,
                     XNClientWindow, FRAME_X_WINDOW (f),
                     XNFocusWindow, FRAME_X_WINDOW (f),
                     XNStatusAttributes, status_attr,
                     NULL);
  else
    xic = XCreateIC (xim,
                     XNInputStyle, xic_style,
                     XNClientWindow, FRAME_X_WINDOW (f),
                     XNFocusWindow, FRAME_X_WINDOW (f),
                     NULL);

  if (!xic)
    goto out;

  FRAME_XIC (f) = xic;
  FRAME_XIC_STYLE (f) = xic_style;
  xfs = NULL; /* Don't free below.  */

 out:

  if (xfs)
    free_frame_xic (f);

  if (preedit_attr)
    XFree (preedit_attr);

  if (status_attr)
    XFree (status_attr);
}


/* Destroy XIC and free XIC fontset of frame F, if any. */

void
free_frame_xic (struct frame *f)
{
  if (FRAME_XIC (f) == NULL)
    return;

  XDestroyIC (FRAME_XIC (f));
  xic_free_xfontset (f);

  FRAME_XIC (f) = NULL;
}


/* Place preedit area for XIC of window W's frame to specified
   pixel position X/Y.  X and Y are relative to window W.  */

void
xic_set_preeditarea (struct window *w, int x, int y)
{
  struct frame *f = XFRAME (w->frame);
  XVaNestedList attr;
  XPoint spot;

  spot.x = WINDOW_TO_FRAME_PIXEL_X (w, x) + WINDOW_LEFT_FRINGE_WIDTH (w);
  spot.y = WINDOW_TO_FRAME_PIXEL_Y (w, y) + FONT_BASE (FRAME_FONT (f));
  attr = XVaCreateNestedList (0, XNSpotLocation, &spot, NULL);
  XSetICValues (FRAME_XIC (f), XNPreeditAttributes, attr, NULL);
  XFree (attr);
}


/* Place status area for XIC in bottom right corner of frame F.. */

void
xic_set_statusarea (struct frame *f)
{
  XIC xic = FRAME_XIC (f);
  XVaNestedList attr;
  XRectangle area;
  XRectangle *needed;

  /* Negotiate geometry of status area.  If input method has existing
     status area, use its current size.  */
  area.x = area.y = area.width = area.height = 0;
  attr = XVaCreateNestedList (0, XNAreaNeeded, &area, NULL);
  XSetICValues (xic, XNStatusAttributes, attr, NULL);
  XFree (attr);

  attr = XVaCreateNestedList (0, XNAreaNeeded, &needed, NULL);
  XGetICValues (xic, XNStatusAttributes, attr, NULL);
  XFree (attr);

  if (needed->width == 0) /* Use XNArea instead of XNAreaNeeded */
    {
      attr = XVaCreateNestedList (0, XNArea, &needed, NULL);
      XGetICValues (xic, XNStatusAttributes, attr, NULL);
      XFree (attr);
    }

  area.width  = needed->width;
  area.height = needed->height;
  area.x = FRAME_PIXEL_WIDTH (f) - area.width - FRAME_INTERNAL_BORDER_WIDTH (f);
  area.y = (FRAME_PIXEL_HEIGHT (f) - area.height
	    - FRAME_MENUBAR_HEIGHT (f)
	    - FRAME_TOOLBAR_TOP_HEIGHT (f)
            - FRAME_INTERNAL_BORDER_WIDTH (f));
  XFree (needed);

  attr = XVaCreateNestedList (0, XNArea, &area, NULL);
  XSetICValues (xic, XNStatusAttributes, attr, NULL);
  XFree (attr);
}


/* Set X fontset for XIC of frame F, using base font name
   BASE_FONTNAME.  Called when a new Emacs fontset is chosen.  */

void
xic_set_xfontset (struct frame *f, const char *base_fontname)
{
  XVaNestedList attr;
  XFontSet xfs;

  xic_free_xfontset (f);

  xfs = xic_create_xfontset (f);

  attr = XVaCreateNestedList (0, XNFontSet, xfs, NULL);
  if (FRAME_XIC_STYLE (f) & XIMPreeditPosition)
    XSetICValues (FRAME_XIC (f), XNPreeditAttributes, attr, NULL);
  if (FRAME_XIC_STYLE (f) & XIMStatusArea)
    XSetICValues (FRAME_XIC (f), XNStatusAttributes, attr, NULL);
  XFree (attr);

  FRAME_XIC_FONTSET (f) = xfs;
}

#endif /* HAVE_X_I18N */




void
x_mark_frame_dirty (struct frame *f)
{
  if (FRAME_X_DOUBLE_BUFFERED_P (f) && !FRAME_X_NEED_BUFFER_FLIP (f))
    FRAME_X_NEED_BUFFER_FLIP (f) = true;
}

static void
set_up_x_back_buffer (struct frame *f)
{
#ifdef HAVE_XDBE
  block_input ();
  if (FRAME_X_WINDOW (f) && !FRAME_X_DOUBLE_BUFFERED_P (f))
    {
#ifdef USE_CAIRO
      x_cr_destroy_frame_context (f);
#endif
      FRAME_X_RAW_DRAWABLE (f) = FRAME_X_WINDOW (f);
      if (FRAME_DISPLAY_INFO (f)->supports_xdbe)
        {
          /* If allocating a back buffer fails, either because the
             server ran out of memory or we don't have the right kind
             of visual, just use single-buffered rendering.  */
          x_catch_errors (FRAME_X_DISPLAY (f));
          FRAME_X_RAW_DRAWABLE (f) = XdbeAllocateBackBufferName (
            FRAME_X_DISPLAY (f),
            FRAME_X_WINDOW (f),
            XdbeCopied);
          if (x_had_errors_p (FRAME_X_DISPLAY (f)))
            FRAME_X_RAW_DRAWABLE (f) = FRAME_X_WINDOW (f);
          x_uncatch_errors_after_check ();
        }
    }
  unblock_input ();
#endif
}

void
tear_down_x_back_buffer (struct frame *f)
{
#ifdef HAVE_XDBE
  block_input ();
  if (FRAME_X_WINDOW (f) && FRAME_X_DOUBLE_BUFFERED_P (f))
    {
      if (FRAME_X_DOUBLE_BUFFERED_P (f))
        {
#ifdef USE_CAIRO
	  x_cr_destroy_frame_context (f);
#endif
          XdbeDeallocateBackBufferName (FRAME_X_DISPLAY (f),
                                        FRAME_X_DRAWABLE (f));
          FRAME_X_RAW_DRAWABLE (f) = FRAME_X_WINDOW (f);
        }
    }
  unblock_input ();
#endif
}

/* Set up double buffering if the frame parameters don't prohibit
   it.  */
void
initial_set_up_x_back_buffer (struct frame *f)
{
  block_input ();
  eassert (FRAME_X_WINDOW (f));
  FRAME_X_RAW_DRAWABLE (f) = FRAME_X_WINDOW (f);
  if (NILP (CDR (Fassq (Qinhibit_double_buffering, f->param_alist))))
    set_up_x_back_buffer (f);
  unblock_input ();
}

#ifdef USE_X_TOOLKIT

/* Create and set up the X widget for frame F.  */

static void
x_window (struct frame *f, long window_prompting)
{
  XClassHint class_hints;
  XSetWindowAttributes attributes;
  unsigned long attribute_mask;
  Widget shell_widget;
  Widget pane_widget;
  Widget frame_widget;
  Arg al[25];
  int ac;

  block_input ();

  /* Use the resource name as the top-level widget name
     for looking up resources.  Make a non-Lisp copy
     for the window manager, so GC relocation won't bother it.

     Elsewhere we specify the window name for the window manager.  */
  f->namebuf = xlispstrdup (Vx_resource_name);

  ac = 0;
  XtSetArg (al[ac], XtNallowShellResize, 1); ac++;
  XtSetArg (al[ac], XtNinput, 1); ac++;
  XtSetArg (al[ac], XtNmappedWhenManaged, 0); ac++;
  XtSetArg (al[ac], XtNborderWidth, f->border_width); ac++;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  shell_widget = XtAppCreateShell (f->namebuf, EMACS_CLASS,
				   applicationShellWidgetClass,
				   FRAME_X_DISPLAY (f), al, ac);

  f->output_data.x->widget = shell_widget;
  /* maybe_set_screen_title_format (shell_widget); */

  pane_widget = lw_create_widget ("main", "pane", widget_id_tick++,
				  NULL, shell_widget, False,
				  NULL, NULL, NULL, NULL);

  ac = 0;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  XtSetArg (al[ac], XtNborderWidth, 0); ac++;
  XtSetValues (pane_widget, al, ac);
  f->output_data.x->column_widget = pane_widget;

  /* mappedWhenManaged to false tells to the paned window to not map/unmap
     the emacs screen when changing menubar.  This reduces flickering.  */

  ac = 0;
  XtSetArg (al[ac], XtNmappedWhenManaged, 0); ac++;
  XtSetArg (al[ac], (char *) XtNshowGrip, 0); ac++;
  XtSetArg (al[ac], (char *) XtNallowResize, 1); ac++;
  XtSetArg (al[ac], (char *) XtNresizeToPreferred, 1); ac++;
  XtSetArg (al[ac], (char *) XtNemacsFrame, f); ac++;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  XtSetArg (al[ac], XtNborderWidth, 0); ac++;
  frame_widget = XtCreateWidget (f->namebuf, emacsFrameClass (), pane_widget,
				 al, ac);

  f->output_data.x->edit_widget = frame_widget;

  XtManageChild (frame_widget);

  /* Do some needed geometry management.  */
  {
    Arg gal[3];
    int gac = 0;
    int extra_borders = 0;
    int menubar_size
      = (f->output_data.x->menubar_widget
	 ? (f->output_data.x->menubar_widget->core.height
	    + f->output_data.x->menubar_widget->core.border_width)
	 : 0);

#if false /* Experimentally, we now get the right results
	     for -geometry -0-0 without this.  24 Aug 96, rms.  */
    if (FRAME_EXTERNAL_MENU_BAR (f))
      {
        Dimension ibw = 0;
        XtVaGetValues (pane_widget, XtNinternalBorderWidth, &ibw, NULL);
        menubar_size += ibw;
      }
#endif

    FRAME_MENUBAR_HEIGHT (f) = menubar_size;

#ifndef USE_LUCID
    /* Motif seems to need this amount added to the sizes
       specified for the shell widget.  The Athena/Lucid widgets don't.
       Both conclusions reached experimentally.  -- rms.  */
    XtVaGetValues (f->output_data.x->edit_widget, XtNinternalBorderWidth,
		   &extra_borders, NULL);
    extra_borders *= 2;
#endif

    f->shell_position = xmalloc (sizeof "=x++" + 4 * INT_STRLEN_BOUND (int));

    /* Convert our geometry parameters into a geometry string
       and specify it.
       Note that we do not specify here whether the position
       is a user-specified or program-specified one.
       We pass that information later, in x_wm_set_size_hint.  */
    {
      int left = f->left_pos;
      bool xneg = (window_prompting & XNegative) != 0;
      int top = f->top_pos;
      bool yneg = (window_prompting & YNegative) != 0;
      if (xneg)
	left = -left;
      if (yneg)
	top = -top;

      if (window_prompting & USPosition)
	sprintf (f->shell_position, "=%dx%d%c%d%c%d",
		 FRAME_PIXEL_WIDTH (f) + extra_borders,
		 FRAME_PIXEL_HEIGHT (f) + menubar_size + extra_borders,
		 (xneg ? '-' : '+'), left,
		 (yneg ? '-' : '+'), top);
      else
        {
          sprintf (f->shell_position, "=%dx%d",
                   FRAME_PIXEL_WIDTH (f) + extra_borders,
                   FRAME_PIXEL_HEIGHT (f) + menubar_size + extra_borders);

          /* Setting x and y when the position is not specified in
             the geometry string will set program position in the WM hints.
             If Emacs had just one program position, we could set it in
             fallback resources, but since each make-frame call can specify
             different program positions, this is easier.  */
          XtSetArg (gal[gac], XtNx, left); gac++;
          XtSetArg (gal[gac], XtNy, top); gac++;
        }
    }

    XtSetArg (gal[gac], XtNgeometry, f->shell_position); gac++;
    XtSetValues (shell_widget, gal, gac);
  }

  XtManageChild (pane_widget);
  XtRealizeWidget (shell_widget);

  if (FRAME_X_EMBEDDED_P (f))
    XReparentWindow (FRAME_X_DISPLAY (f), XtWindow (shell_widget),
		     f->output_data.x->parent_desc, 0, 0);

  FRAME_X_WINDOW (f) = XtWindow (frame_widget);
  initial_set_up_x_back_buffer (f);
  validate_x_resource_name ();

  class_hints.res_name = SSDATA (Vx_resource_name);
  class_hints.res_class = SSDATA (Vx_resource_class);
  XSetClassHint (FRAME_X_DISPLAY (f), XtWindow (shell_widget), &class_hints);

#ifdef HAVE_X_I18N
  FRAME_XIC (f) = NULL;
  if (use_xim)
    create_frame_xic (f);
#endif

  f->output_data.x->wm_hints.input = True;
  f->output_data.x->wm_hints.flags |= InputHint;
  XSetWMHints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	       &f->output_data.x->wm_hints);

  hack_wm_protocols (f, shell_widget);

#ifdef X_TOOLKIT_EDITRES
  XtAddEventHandler (shell_widget, 0, True, _XEditResCheckMessages, 0);
#endif

  /* Do a stupid property change to force the server to generate a
     PropertyNotify event so that the event_stream server timestamp will
     be initialized to something relevant to the time we created the window.
     */
  XChangeProperty (XtDisplay (frame_widget), XtWindow (frame_widget),
		   FRAME_DISPLAY_INFO (f)->Xatom_wm_protocols,
		   XA_ATOM, 32, PropModeAppend, NULL, 0);

  /* Make all the standard events reach the Emacs frame.  */
  attributes.event_mask = STANDARD_EVENT_SET;

#ifdef HAVE_X_I18N
  if (FRAME_XIC (f))
    {
      /* XIM server might require some X events. */
      unsigned long fevent = NoEventMask;
      XGetICValues (FRAME_XIC (f), XNFilterEvents, &fevent, NULL);
      attributes.event_mask |= fevent;
    }
#endif /* HAVE_X_I18N */

  attributes.override_redirect = FRAME_OVERRIDE_REDIRECT (f);
  attribute_mask = CWEventMask | CWOverrideRedirect;
  XChangeWindowAttributes (XtDisplay (shell_widget), XtWindow (shell_widget),
			   attribute_mask, &attributes);

  XtMapWidget (frame_widget);

  /* x_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the X server hasn't been told.  */
  {
    Lisp_Object name;
    bool explicit = f->explicit_name;

    f->explicit_name = false;
    name = f->name;
    fset_name (f, Qnil);
    x_set_name (f, name, explicit);
  }

  if (FRAME_UNDECORATED (f))
    {
      Display *dpy = FRAME_X_DISPLAY (f);
      PropMotifWmHints hints;
      Atom prop = XInternAtom (dpy, "_MOTIF_WM_HINTS", False);

      memset (&hints, 0, sizeof(hints));
      hints.flags = MWM_HINTS_DECORATIONS;
      hints.decorations = 0;

      /* For some reason the third and fourth arguments in the following
	 call must be identical: In the corresponding XGetWindowProperty
	 call in getMotifHints, xfwm has the third and seventh args both
	 display_info->atoms[MOTIF_WM_HINTS].  Obviously, YMMV.   */
      XChangeProperty (dpy, FRAME_OUTER_WINDOW (f), prop, prop, 32,
		       PropModeReplace, (unsigned char *) &hints,
		       PROP_MOTIF_WM_HINTS_ELEMENTS);
    }

  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 f->output_data.x->current_cursor
                 = f->output_data.x->text_cursor);

  unblock_input ();

  /* This is a no-op, except under Motif.  Make sure main areas are
     set to something reasonable, in case we get an error later.  */
  lw_set_main_areas (pane_widget, 0, frame_widget);
}

#else /* not USE_X_TOOLKIT */
#ifdef USE_GTK
static void
x_window (struct frame *f)
{
  if (! xg_create_frame_widgets (f))
    error ("Unable to create window");

#ifdef HAVE_X_I18N
  FRAME_XIC (f) = NULL;
  if (use_xim)
  {
    block_input ();
    create_frame_xic (f);
    if (FRAME_XIC (f))
      {
	/* XIM server might require some X events. */
	unsigned long fevent = NoEventMask;
	XGetICValues (FRAME_XIC (f), XNFilterEvents, &fevent, NULL);

	if (fevent != NoEventMask)
	  {
	    XSetWindowAttributes attributes;
	    XWindowAttributes wattr;
	    unsigned long attribute_mask;

	    XGetWindowAttributes (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				  &wattr);
	    attributes.event_mask = wattr.your_event_mask | fevent;
	    attribute_mask = CWEventMask;
	    XChangeWindowAttributes (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				     attribute_mask, &attributes);
	  }
      }
    unblock_input ();
  }
#endif
}

#else /*! USE_GTK */
/* Create and set up the X window for frame F.  */

static void
x_window (struct frame *f)
{
  XClassHint class_hints;
  XSetWindowAttributes attributes;
  unsigned long attribute_mask;

  attributes.background_pixel = FRAME_BACKGROUND_PIXEL (f);
  attributes.border_pixel = f->output_data.x->border_pixel;
  attributes.bit_gravity = StaticGravity;
  attributes.backing_store = NotUseful;
  attributes.save_under = True;
  attributes.event_mask = STANDARD_EVENT_SET;
  attributes.colormap = FRAME_X_COLORMAP (f);
  attributes.override_redirect = FRAME_OVERRIDE_REDIRECT (f);
  attribute_mask = (CWBackPixel | CWBorderPixel | CWBitGravity | CWEventMask
		    | CWOverrideRedirect | CWColormap);

  block_input ();
  FRAME_X_WINDOW (f)
    = XCreateWindow (FRAME_X_DISPLAY (f),
		     f->output_data.x->parent_desc,
		     f->left_pos,
		     f->top_pos,
		     FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f),
		     f->border_width,
		     CopyFromParent, /* depth */
		     InputOutput, /* class */
		     FRAME_X_VISUAL (f),
                     attribute_mask, &attributes);
  initial_set_up_x_back_buffer (f);

#ifdef HAVE_X_I18N
  if (use_xim)
    {
      create_frame_xic (f);
      if (FRAME_XIC (f))
	{
	  /* XIM server might require some X events. */
	  unsigned long fevent = NoEventMask;
	  XGetICValues (FRAME_XIC (f), XNFilterEvents, &fevent, NULL);
	  attributes.event_mask |= fevent;
	  attribute_mask = CWEventMask;
	  XChangeWindowAttributes (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				   attribute_mask, &attributes);
	}
    }
#endif /* HAVE_X_I18N */

  validate_x_resource_name ();

  class_hints.res_name = SSDATA (Vx_resource_name);
  class_hints.res_class = SSDATA (Vx_resource_class);
  XSetClassHint (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &class_hints);

  /* This indicates that we use the "Passive Input" input model.
     Unless we do this, we don't get the Focus{In,Out} events that we
     need to draw the cursor correctly.  Accursed bureaucrats.
   XWhipsAndChains (FRAME_X_DISPLAY (f), IronMaiden, &TheRack);  */

  f->output_data.x->wm_hints.input = True;
  f->output_data.x->wm_hints.flags |= InputHint;
  XSetWMHints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	       &f->output_data.x->wm_hints);
  f->output_data.x->wm_hints.icon_pixmap = None;

  /* Request "save yourself" and "delete window" commands from wm.  */
  {
    Atom protocols[2];
    protocols[0] = FRAME_DISPLAY_INFO (f)->Xatom_wm_delete_window;
    protocols[1] = FRAME_DISPLAY_INFO (f)->Xatom_wm_save_yourself;
    XSetWMProtocols (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), protocols, 2);
  }

  /* x_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the X server hasn't been told.  */
  {
    Lisp_Object name;
    bool explicit = f->explicit_name;

    f->explicit_name = false;
    name = f->name;
    fset_name (f, Qnil);
    x_set_name (f, name, explicit);
  }

  if (FRAME_UNDECORATED (f))
    {
      Display *dpy = FRAME_X_DISPLAY (f);
      PropMotifWmHints hints;
      Atom prop = XInternAtom (dpy, "_MOTIF_WM_HINTS", False);

      memset (&hints, 0, sizeof(hints));
      hints.flags = MWM_HINTS_DECORATIONS;
      hints.decorations = 0;

      /* For some reason the third and fourth arguments in the following
	 call must be identical: In the corresponding XGetWindowProperty
	 call in getMotifHints, xfwm has the third and seventh args both
	 display_info->atoms[MOTIF_WM_HINTS].  Obviously, YMMV.   */
      XChangeProperty (dpy, FRAME_OUTER_WINDOW (f), prop, prop, 32,
		       PropModeReplace, (unsigned char *) &hints,
		       PROP_MOTIF_WM_HINTS_ELEMENTS);
    }


  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 f->output_data.x->current_cursor
                 = f->output_data.x->text_cursor);

  unblock_input ();

  if (FRAME_X_WINDOW (f) == 0)
    error ("Unable to create window");
}

#endif /* not USE_GTK */
#endif /* not USE_X_TOOLKIT */

/* Verify that the icon position args for this window are valid.  */

static void
x_icon_verify (struct frame *f, Lisp_Object parms)
{
  Lisp_Object icon_x, icon_y;

  /* Set the position of the icon.  Note that twm groups all
     icons in an icon window.  */
  icon_x = gui_frame_get_and_record_arg (f, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = gui_frame_get_and_record_arg (f, parms, Qicon_top, 0, 0, RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_FIXNUM (icon_x);
      CHECK_FIXNUM (icon_y);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");
}

/* Handle the icon stuff for this window.  Perhaps later we might
   want an x_set_icon_position which can be called interactively as
   well.  */

static void
x_icon (struct frame *f, Lisp_Object parms)
{
  /* Set the position of the icon.  Note that twm groups all
     icons in an icon window.  */
  Lisp_Object icon_x
    = gui_frame_get_and_record_arg (f, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  Lisp_Object icon_y
    = gui_frame_get_and_record_arg (f, parms, Qicon_top, 0, 0, RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_TYPE_RANGED_INTEGER (int, icon_x);
      CHECK_TYPE_RANGED_INTEGER (int, icon_y);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");

  block_input ();

  if (! EQ (icon_x, Qunbound))
    x_wm_set_icon_position (f, XFIXNUM (icon_x), XFIXNUM (icon_y));

#if false /* gui_display_get_arg removes the visibility parameter as a
	     side effect, but x_create_frame still needs it.  */
  /* Start up iconic or window? */
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  x_wm_set_window_state
    (f, (EQ (gui_display_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
                                  RES_TYPE_SYMBOL),
	     Qicon)
	 ? IconicState
	 : NormalState));
#endif

  x_text_icon (f, SSDATA ((!NILP (f->icon_name)
			   ? f->icon_name
			   : f->name)));

  unblock_input ();
}

/* Make the GCs needed for this window, setting the
   background, border and mouse colors; also create the
   mouse cursor and the gray border tile.  */

static void
x_make_gc (struct frame *f)
{
  XGCValues gc_values;

  block_input ();

  /* Create the GCs of this frame.
     Note that many default values are used.  */

  gc_values.foreground = FRAME_FOREGROUND_PIXEL (f);
  gc_values.background = FRAME_BACKGROUND_PIXEL (f);
  gc_values.line_width = 0;	/* Means 1 using fast algorithm.  */
  f->output_data.x->normal_gc
    = XCreateGC (FRAME_X_DISPLAY (f),
                 FRAME_X_DRAWABLE (f),
		 GCLineWidth | GCForeground | GCBackground,
		 &gc_values);

  /* Reverse video style.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = FRAME_FOREGROUND_PIXEL (f);
  f->output_data.x->reverse_gc
    = XCreateGC (FRAME_X_DISPLAY (f),
                 FRAME_X_DRAWABLE (f),
		 GCForeground | GCBackground | GCLineWidth,
		 &gc_values);

  /* Cursor has cursor-color background, background-color foreground.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = f->output_data.x->cursor_pixel;
  gc_values.fill_style = FillOpaqueStippled;
  f->output_data.x->cursor_gc
    = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
		 (GCForeground | GCBackground
		  | GCFillStyle | GCLineWidth),
		 &gc_values);

  /* Create the gray border tile used when the pointer is not in
     the frame.  Since this depends on the frame's pixel values,
     this must be done on a per-frame basis.  */
  f->output_data.x->border_tile
    = (XCreatePixmapFromBitmapData
       (FRAME_X_DISPLAY (f), FRAME_DISPLAY_INFO (f)->root_window,
	gray_bits, gray_width, gray_height,
	FRAME_FOREGROUND_PIXEL (f),
	FRAME_BACKGROUND_PIXEL (f),
	DefaultDepth (FRAME_X_DISPLAY (f), FRAME_X_SCREEN_NUMBER (f))));

  unblock_input ();
}


/* Free what was allocated in x_make_gc.  */

void
x_free_gcs (struct frame *f)
{
  Display *dpy = FRAME_X_DISPLAY (f);

  block_input ();

  if (f->output_data.x->normal_gc)
    {
      XFreeGC (dpy, f->output_data.x->normal_gc);
      f->output_data.x->normal_gc = 0;
    }

  if (f->output_data.x->reverse_gc)
    {
      XFreeGC (dpy, f->output_data.x->reverse_gc);
      f->output_data.x->reverse_gc = 0;
    }

  if (f->output_data.x->cursor_gc)
    {
      XFreeGC (dpy, f->output_data.x->cursor_gc);
      f->output_data.x->cursor_gc = 0;
    }

  if (f->output_data.x->border_tile)
    {
      XFreePixmap (dpy, f->output_data.x->border_tile);
      f->output_data.x->border_tile = 0;
    }

  unblock_input ();
}


/* Handler for signals raised during x_create_frame and
   Fx_create_tip_frame.  FRAME is the frame which is partially
   constructed.  */

static Lisp_Object
unwind_create_frame (Lisp_Object frame)
{
  struct frame *f = XFRAME (frame);

  /* If frame is already dead, nothing to do.  This can happen if the
     display is disconnected after the frame has become official, but
     before Fx_create_frame removes the unwind protect.  */
  if (!FRAME_LIVE_P (f))
    return Qnil;

  /* If frame is ``official'', nothing to do.  */
  if (NILP (Fmemq (frame, Vframe_list)))
    {
#if defined GLYPH_DEBUG && defined ENABLE_CHECKING
      struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
#endif

      /* If the frame's image cache refcount is still the same as our
	 private shadow variable, it means we are unwinding a frame
	 for which we didn't yet call init_frame_faces, where the
	 refcount is incremented.  Therefore, we increment it here, so
	 that free_frame_faces, called in x_free_frame_resources
	 below, will not mistakenly decrement the counter that was not
	 incremented yet to account for this new frame.  */
      if (FRAME_IMAGE_CACHE (f) != NULL
	  && FRAME_IMAGE_CACHE (f)->refcount == image_cache_refcount)
	FRAME_IMAGE_CACHE (f)->refcount++;

      x_free_frame_resources (f);
      free_glyphs (f);

#if defined GLYPH_DEBUG && defined ENABLE_CHECKING
      /* Check that reference counts are indeed correct.  */
      eassert (dpyinfo->reference_count == dpyinfo_refcount);
      eassert (dpyinfo->terminal->image_cache->refcount == image_cache_refcount);
#endif
      return Qt;
    }

  return Qnil;
}

static void
do_unwind_create_frame (Lisp_Object frame)
{
  unwind_create_frame (frame);
}

void
x_default_font_parameter (struct frame *f, Lisp_Object parms)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Lisp_Object font_param = gui_display_get_arg (dpyinfo, parms, Qfont, NULL, NULL,
                                                RES_TYPE_STRING);
  Lisp_Object font = Qnil;
  if (EQ (font_param, Qunbound))
    font_param = Qnil;

  if (NILP (font_param))
    {
      /* System font should take precedence over X resources.  We suggest this
         regardless of font-use-system-font because .emacs may not have been
         read yet.  */
      const char *system_font = xsettings_get_system_font ();
      if (system_font)
	font = font_open_by_name (f, build_unibyte_string (system_font));
    }

  if (NILP (font))
      font = !NILP (font_param) ? font_param
      : gui_display_get_arg (dpyinfo, parms, Qfont, "font", "Font",
                             RES_TYPE_STRING);

  if (! FONTP (font) && ! STRINGP (font))
    {
      const char *names[]
	= {
#if defined USE_CAIRO || defined HAVE_XFT
	    /* This will find the normal Xft font.  */
 	    "monospace-10",
#endif
	    "-adobe-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1",
	    "-misc-fixed-medium-r-normal-*-*-140-*-*-c-*-iso8859-1",
	    "-*-*-medium-r-normal-*-*-140-*-*-c-*-iso8859-1",
	    /* This was formerly the first thing tried, but it finds
	       too many fonts and takes too long.  */
	    "-*-*-medium-r-*-*-*-*-*-*-c-*-iso8859-1",
	    /* If those didn't work, look for something which will
	       at least work.  */
	    "-*-fixed-*-*-*-*-*-140-*-*-c-*-iso8859-1",
	    "fixed",
	    NULL };
      int i;

      for (i = 0; names[i]; i++)
	{
	  font = font_open_by_name (f, build_unibyte_string (names[i]));
	  if (! NILP (font))
	    break;
	}
      if (NILP (font))
	error ("No suitable font was found");
    }
  else if (!NILP (font_param))
    {
      /* Remember the explicit font parameter, so we can re-apply it after
	 we've applied the `default' face settings.  */
      AUTO_FRAME_ARG (arg, Qfont_parameter, font_param);
      gui_set_frame_parameters (f, arg);
    }

  /* This call will make X resources override any system font setting.  */
  gui_default_parameter (f, parms, Qfont, font, "font", "Font", RES_TYPE_STRING);
}


DEFUN ("x-wm-set-size-hint", Fx_wm_set_size_hint, Sx_wm_set_size_hint,
       0, 1, 0,
       doc: /* Send the size hints for frame FRAME to the window manager.
If FRAME is omitted or nil, use the selected frame.
Signal error if FRAME is not an X frame.  */)
  (Lisp_Object frame)
{
  struct frame *f = decode_window_system_frame (frame);

  block_input ();
  x_wm_set_size_hint (f, 0, false);
  unblock_input ();
  return Qnil;
}

static void
set_machine_and_pid_properties (struct frame *f)
{
  /* This will set WM_CLIENT_MACHINE and WM_LOCALE_NAME.  */
  XSetWMProperties (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), NULL, NULL,
                    NULL, 0, NULL, NULL, NULL);
  pid_t pid = getpid ();
  if (pid <= 0xffffffffu)
    {
      unsigned long xpid = pid;
      XChangeProperty (FRAME_X_DISPLAY (f),
		       FRAME_OUTER_WINDOW (f),
		       XInternAtom (FRAME_X_DISPLAY (f),
				    "_NET_WM_PID",
				    False),
		       XA_CARDINAL, 32, PropModeReplace,
		       (unsigned char *) &xpid, 1);
    }
}

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* Make a new X window, which is called a "frame" in Emacs terms.
Return an Emacs frame object.  PARMS is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use, then
`default-minibuffer-frame' must be a frame whose minibuffer can be
shared by the new frame.

This function is an internal primitive--use `make-frame' instead.  */)
  (Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  bool minibuffer_only = false;
  bool undecorated = false, override_redirect = false;
  long window_prompting = 0;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object display;
  struct x_display_info *dpyinfo = NULL;
  Lisp_Object parent, parent_frame;
  struct kboard *kb;
  int x_width = 0, x_height = 0;

  parms = Fcopy_alist (parms);

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = gui_display_get_arg (dpyinfo, parms, Qterminal, 0, 0,
                                 RES_TYPE_NUMBER);
  if (EQ (display, Qunbound))
    display = gui_display_get_arg (dpyinfo, parms, Qdisplay, 0, 0,
                                   RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_x_display_info (display);
  kb = dpyinfo->terminal->kboard;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  name = gui_display_get_arg (dpyinfo, parms, Qname, "name", "Name",
                              RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = gui_display_get_arg (dpyinfo, parms, Qparent_id, NULL, NULL,
                                RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_FIXNUM (parent);

  frame = Qnil;
  tem = gui_display_get_arg (dpyinfo,
                             parms, Qminibuffer, "minibuffer", "Minibuffer",
                             RES_TYPE_SYMBOL);
  if (EQ (tem, Qnone) || NILP (tem))
    f = make_frame_without_minibuffer (Qnil, kb, display);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = true;
    }
  else if (WINDOWP (tem))
    f = make_frame_without_minibuffer (tem, kb, display);
  else
    f = make_frame (true);

  parent_frame = gui_display_get_arg (dpyinfo,
                                      parms,
                                      Qparent_frame,
                                      NULL,
                                      NULL,
                                      RES_TYPE_SYMBOL);
  /* Accept parent-frame iff parent-id was not specified.  */
  if (!NILP (parent)
      || EQ (parent_frame, Qunbound)
      || NILP (parent_frame)
      || !FRAMEP (parent_frame)
      || !FRAME_LIVE_P (XFRAME (parent_frame))
      || !FRAME_X_P (XFRAME (parent_frame)))
    parent_frame = Qnil;

  fset_parent_frame (f, parent_frame);
  store_frame_param (f, Qparent_frame, parent_frame);

  if (!NILP (tem = (gui_display_get_arg (dpyinfo,
                                         parms,
                                         Qundecorated,
                                         NULL,
                                         NULL,
                                         RES_TYPE_BOOLEAN)))
      && !(EQ (tem, Qunbound)))
    undecorated = true;

  FRAME_UNDECORATED (f) = undecorated;
  store_frame_param (f, Qundecorated, undecorated ? Qt : Qnil);

  if (!NILP (tem = (gui_display_get_arg (dpyinfo,
                                         parms,
                                         Qoverride_redirect,
                                         NULL,
                                         NULL,
                                         RES_TYPE_BOOLEAN)))
      && !(EQ (tem, Qunbound)))
    override_redirect = true;

  FRAME_OVERRIDE_REDIRECT (f) = override_redirect;
  store_frame_param (f, Qoverride_redirect, override_redirect ? Qt : Qnil);

  XSETFRAME (frame, f);

  f->terminal = dpyinfo->terminal;

  f->output_method = output_x_window;
  f->output_data.x = xzalloc (sizeof *f->output_data.x);
  f->output_data.x->icon_bitmap = -1;
  FRAME_FONTSET (f) = -1;
  f->output_data.x->scroll_bar_foreground_pixel = -1;
  f->output_data.x->scroll_bar_background_pixel = -1;
#if defined (USE_LUCID) && defined (USE_TOOLKIT_SCROLL_BARS)
  f->output_data.x->scroll_bar_top_shadow_pixel = -1;
  f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
#endif /* USE_LUCID && USE_TOOLKIT_SCROLL_BARS */
  f->output_data.x->white_relief.pixel = -1;
  f->output_data.x->black_relief.pixel = -1;

  fset_icon_name (f, gui_display_get_arg (dpyinfo,
                                          parms,
                                          Qicon_name,
                                          "iconName",
                                          "Title",
                                          RES_TYPE_STRING));
  if (! STRINGP (f->icon_name))
    fset_icon_name (f, Qnil);

  FRAME_DISPLAY_INFO (f) = dpyinfo;

  /* With FRAME_DISPLAY_INFO set up, this unwind-protect is safe.  */
  record_unwind_protect (do_unwind_create_frame, frame);

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    FRAME_FOREGROUND_PIXEL (f) = -1;
    FRAME_BACKGROUND_PIXEL (f) = -1;
    f->output_data.x->cursor_pixel = -1;
    f->output_data.x->cursor_foreground_pixel = -1;
    f->output_data.x->border_pixel = -1;
    f->output_data.x->mouse_pixel = -1;

    black = build_string ("black");
    FRAME_FOREGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_foreground_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->mouse_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
  }

  /* Specify the parent under which to make this X window.  */
  if (!NILP (parent))
    {
      f->output_data.x->parent_desc = (Window) XFIXNAT (parent);
      f->output_data.x->explicit_parent = true;
    }
  else
    {
      f->output_data.x->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
      f->output_data.x->explicit_parent = false;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      fset_name (f, build_string (dpyinfo->x_id_name));
      f->explicit_name = false;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = true;
      /* Use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

#ifdef USE_CAIRO
  register_font_driver (&ftcrfont_driver, f);
#ifdef HAVE_HARFBUZZ
  register_font_driver (&ftcrhbfont_driver, f);
#endif	/* HAVE_HARFBUZZ */
#else
#ifdef HAVE_FREETYPE
#ifdef HAVE_XFT
  register_font_driver (&xftfont_driver, f);
#ifdef HAVE_HARFBUZZ
  register_font_driver (&xfthbfont_driver, f);
#endif
#else	/* not HAVE_XFT */
  register_font_driver (&ftxfont_driver, f);
#endif	/* not HAVE_XFT */
#endif	/* HAVE_FREETYPE */
#endif	/* not USE_CAIRO */
  register_font_driver (&xfont_driver, f);

  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;
#ifdef GLYPH_DEBUG
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
                         "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  x_default_font_parameter (f, parms);
  if (!FRAME_FONT (f))
    {
      delete_frame (frame, Qnoelisp);
      error ("Invalid frame font");
    }

  /* Frame contents get displaced if an embedded X window has a border.  */
  if (! FRAME_X_EMBEDDED_P (f))
    gui_default_parameter (f, parms, Qborder_width, make_fixnum (0),
                           "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  /* This defaults to 1 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = gui_display_get_arg (dpyinfo, parms, Qinternal_border_width,
                                   "internalBorder", "internalBorder",
                                   RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }
  gui_default_parameter (f, parms, Qinternal_border_width,
#ifdef USE_GTK /* We used to impose 0 in xg_create_frame_widgets.  */
                         make_fixnum (0),
#else
                         make_fixnum (1),
#endif
                         "internalBorderWidth", "internalBorderWidth",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_divider_width, make_fixnum (0),
                         NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qbottom_divider_width, make_fixnum (0),
                         NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qvertical_scroll_bars,
#if defined (USE_GTK) && defined (USE_TOOLKIT_SCROLL_BARS)
                         Qright,
#else
                         Qleft,
#endif
                         "verticalScrollBars", "ScrollBars",
                         RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qhorizontal_scroll_bars, Qnil,
                         "horizontalScrollBars", "ScrollBars",
                         RES_TYPE_SYMBOL);
  /* Also do the stuff which must be set before the window exists.  */
  gui_default_parameter (f, parms, Qforeground_color, build_string ("black"),
                         "foreground", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qbackground_color, build_string ("white"),
                         "background", "Background", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qmouse_color, build_string ("black"),
                         "pointerColor", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qborder_color, build_string ("black"),
                         "borderColor", "BorderColor", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qscreen_gamma, Qnil,
                         "screenGamma", "ScreenGamma", RES_TYPE_FLOAT);
  gui_default_parameter (f, parms, Qline_spacing, Qnil,
                         "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qleft_fringe, Qnil,
                         "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_fringe, Qnil,
                         "rightFringe", "RightFringe", RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qno_special_glyphs, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);

  x_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_foreground,
					"scrollBarForeground",
					"ScrollBarForeground", true);
  x_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_background,
					"scrollBarBackground",
					"ScrollBarBackground", false);

  /* Init faces before gui_default_parameter is called for the
     scroll-bar-width parameter because otherwise we end up in
     init_iterator with a null face cache, which should not happen.  */
  init_frame_faces (f);

  /* We have to call adjust_frame_size here since otherwise
     x_set_tool_bar_lines will already work with the character sizes
     installed by init_frame_faces while the frame's pixel size is still
     calculated from a character size of 1 and we subsequently hit the
     (height >= 0) assertion in window_box_height.

     The non-pixelwise code apparently worked around this because it
     had one frame line vs one toolbar line which left us with a zero
     root window height which was obviously wrong as well ...

     Also process `min-width' and `min-height' parameters right here
     because `frame-windows-min-size' needs them.  */
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_width, NULL, NULL,
                             RES_TYPE_NUMBER);
  if (FIXNUMP (tem))
    store_frame_param (f, Qmin_width, tem);
  tem = gui_display_get_arg (dpyinfo, parms, Qmin_height, NULL, NULL,
                             RES_TYPE_NUMBER);
  if (FIXNUMP (tem))
    store_frame_param (f, Qmin_height, tem);
  adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		     FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 5, true,
		     Qx_create_frame_1);

  /* Set the menu-bar-lines and tool-bar-lines parameters.  We don't
     look up the X resources controlling the menu-bar and tool-bar
     here; they are processed specially at startup, and reflected in
     the values of the mode variables.  */

  gui_default_parameter (f, parms, Qmenu_bar_lines,
                         NILP (Vmenu_bar_mode)
                         ? make_fixnum (0) : make_fixnum (1),
                         NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qtab_bar_lines,
                         NILP (Vtab_bar_mode)
                         ? make_fixnum (0) : make_fixnum (1),
                         NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qtool_bar_lines,
                         NILP (Vtool_bar_mode)
                         ? make_fixnum (0) : make_fixnum (1),
                         NULL, NULL, RES_TYPE_NUMBER);

  gui_default_parameter (f, parms, Qbuffer_predicate, Qnil,
                         "bufferPredicate", "BufferPredicate",
                         RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qtitle, Qnil,
                         "title", "Title", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qwait_for_wm, Qt,
                         "waitForWM", "WaitForWM", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qtool_bar_position,
                         FRAME_TOOL_BAR_POSITION (f), 0, 0, RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qinhibit_double_buffering, Qnil,
                         "inhibitDoubleBuffering", "InhibitDoubleBuffering",
                         RES_TYPE_BOOLEAN);

  /* Compute the size of the X window.  */
  window_prompting = gui_figure_window_size (f, parms, true, true,
                                             &x_width, &x_height);

  tem = gui_display_get_arg (dpyinfo, parms, Qunsplittable, 0, 0,
                             RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  x_icon_verify (f, parms);

  /* Create the X widget or window.  */
#ifdef USE_X_TOOLKIT
  x_window (f, window_prompting);
#else
  x_window (f);
#endif

  x_icon (f, parms);
  x_make_gc (f);

  /* Now consider the frame official.  */
  f->terminal->reference_count++;
  FRAME_DISPLAY_INFO (f)->reference_count++;
  Vframe_list = Fcons (frame, Vframe_list);

  /* We need to do this after creating the X window, so that the
     icon-creation functions can say whose icon they're describing.  */
  gui_default_parameter (f, parms, Qicon_type, Qt,
                         "bitmapIcon", "BitmapIcon", RES_TYPE_BOOLEAN);

  gui_default_parameter (f, parms, Qauto_raise, Qnil,
                         "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qauto_lower, Qnil,
                         "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qcursor_type, Qbox,
                         "cursorType", "CursorType", RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qscroll_bar_width, Qnil,
                         "scrollBarWidth", "ScrollBarWidth",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qscroll_bar_height, Qnil,
                         "scrollBarHeight", "ScrollBarHeight",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qalpha, Qnil,
                         "alpha", "Alpha", RES_TYPE_NUMBER);

  if (!NILP (parent_frame))
    {
      struct frame *p = XFRAME (parent_frame);

      block_input ();
      XReparentWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		       FRAME_X_WINDOW (p), f->left_pos, f->top_pos);
#ifdef USE_GTK
      if (EQ (x_gtk_resize_child_frames, Qresize_mode))
	gtk_container_set_resize_mode
	  (GTK_CONTAINER (FRAME_GTK_OUTER_WIDGET (f)), GTK_RESIZE_IMMEDIATE);
#endif
      unblock_input ();
    }

  gui_default_parameter (f, parms, Qno_focus_on_map, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qno_accept_focus, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
  /* Create the menu bar.  */
  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f))
    {
      /* If this signals an error, we haven't set size hints for the
	 frame and we didn't make it visible.  */
      initialize_frame_menubar (f);

#ifndef USE_GTK
      /* This is a no-op, except under Motif where it arranges the
	 main window for the widgets on it.  */
      lw_set_main_areas (f->output_data.x->column_widget,
			 f->output_data.x->menubar_widget,
			 f->output_data.x->edit_widget);
#endif /* not USE_GTK */
    }
#endif /* USE_X_TOOLKIT || USE_GTK */

  /* Consider frame official, now.  */
  f->can_set_window_size = true;

  if (x_width > 0)
    SET_FRAME_WIDTH (f, x_width);
  if (x_height > 0)
    SET_FRAME_HEIGHT (f, x_height);

  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  block_input ();
  x_wm_set_size_hint (f, window_prompting, false);
  unblock_input ();

  adjust_frame_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
		     0, true, Qx_create_frame_2);

  /* Process fullscreen parameter here in the hope that normalizing a
     fullheight/fullwidth frame will produce the size set by the last
     adjust_frame_size call.  */
  gui_default_parameter (f, parms, Qfullscreen, Qnil,
                         "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  /* Make the window appear on the frame and enable display, unless
     the caller says not to.  However, with explicit parent, Emacs
     cannot control visibility, so don't try.  */
  if (!f->output_data.x->explicit_parent)
    {
      Lisp_Object visibility
	= gui_display_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
                               RES_TYPE_SYMBOL);

      if (EQ (visibility, Qicon))
	x_iconify_frame (f);
      else
	{
	  if (EQ (visibility, Qunbound))
	    visibility = Qt;

	  if (!NILP (visibility))
	    x_make_frame_visible (f);
	}

      store_frame_param (f, Qvisibility, visibility);
    }

  block_input ();

  /* Set machine name and pid for the purpose of window managers.  */
  set_machine_and_pid_properties (f);

  /* Set the WM leader property.  GTK does this itself, so this is not
     needed when using GTK.  */
  if (dpyinfo->client_leader_window != 0)
    {
      XChangeProperty (FRAME_X_DISPLAY (f),
		       FRAME_OUTER_WINDOW (f),
		       dpyinfo->Xatom_wm_client_leader,
		       XA_WINDOW, 32, PropModeReplace,
		       (unsigned char *) &dpyinfo->client_leader_window, 1);
    }

  unblock_input ();

  /* Works iff frame has been already mapped.  */
  gui_default_parameter (f, parms, Qskip_taskbar, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);
  /* The `z-group' parameter works only for visible frames.  */
  gui_default_parameter (f, parms, Qz_group, Qnil,
                         NULL, NULL, RES_TYPE_SYMBOL);

  /* Initialize `default-minibuffer-frame' in case this is the first
     frame on this terminal.  */
  if (FRAME_HAS_MINIBUF_P (f)
      && (!FRAMEP (KVAR (kb, Vdefault_minibuffer_frame))
          || !FRAME_LIVE_P (XFRAME (KVAR (kb, Vdefault_minibuffer_frame)))))
    kset_default_minibuffer_frame (kb, frame);

  /* All remaining specified parameters, which have not been "used" by
     gui_display_get_arg and friends, now go in the misc. alist of the
     frame.  */
  for (tem = parms; CONSP (tem); tem = XCDR (tem))
    if (CONSP (XCAR (tem)) && !NILP (XCAR (XCAR (tem))))
      fset_param_alist (f, Fcons (XCAR (tem), f->param_alist));

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;

 return unbind_to (count, frame);
}


DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* Internal function called by `color-defined-p'.
\(Note that the Nextstep version of this function ignores FRAME.)  */)
  (Lisp_Object color, Lisp_Object frame)
{
  XColor foo;
  struct frame *f = decode_window_system_frame (frame);

  CHECK_STRING (color);

  if (x_defined_color (f, SSDATA (color), &foo, false, false))
    return Qt;
  else
    return Qnil;
}

DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* Internal function called by `color-values'.
\(Note that the Nextstep version of this function ignores FRAME.)  */)
  (Lisp_Object color, Lisp_Object frame)
{
  XColor foo;
  struct frame *f = decode_window_system_frame (frame);

  CHECK_STRING (color);

  if (x_defined_color (f, SSDATA (color), &foo, false, false))
    return list3i (foo.red, foo.green, foo.blue);
  else
    return Qnil;
}

DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* Internal function called by `display-color-p'.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  if (dpyinfo->n_planes <= 2)
    return Qnil;

  switch (dpyinfo->visual->class)
    {
    case StaticColor:
    case PseudoColor:
    case TrueColor:
    case DirectColor:
      return Qt;

    default:
      return Qnil;
    }
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p,
       0, 1, 0,
       doc: /* Return t if the X display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  if (dpyinfo->n_planes <= 1)
    return Qnil;

  switch (dpyinfo->visual->class)
    {
    case StaticColor:
    case PseudoColor:
    case TrueColor:
    case DirectColor:
    case StaticGray:
    case GrayScale:
      return Qt;

    default:
      return Qnil;
    }
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* Return the width in pixels of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.
\(On MS Windows, this function does not accept terminal objects.)

On \"multi-monitor\" setups this refers to the pixel width for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_fixnum (x_display_pixel_width (dpyinfo));
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* Return the height in pixels of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.
\(On MS Windows, this function does not accept terminal objects.)

On \"multi-monitor\" setups this refers to the pixel height for all
physical monitors associated with TERMINAL.  To get information for
each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_fixnum (x_display_pixel_height (dpyinfo));
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* Return the number of bitplanes of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.
\(On MS Windows, this function does not accept terminal objects.)  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_fixnum (dpyinfo->n_planes);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* Return the number of color cells of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.
\(On MS Windows, this function does not accept terminal objects.)  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  int nr_planes = DisplayPlanes (dpyinfo->display,
                                 XScreenNumberOfScreen (dpyinfo->screen));

  /* Truncate nr_planes to 24 to avoid integer overflow.
     Some displays says 32, but only 24 bits are actually significant.
     There are only very few and rare video cards that have more than
     24 significant bits.  Also 24 bits is more than 16 million colors,
     it "should be enough for everyone".  */
  if (nr_planes > 24) nr_planes = 24;

  return make_fixnum (1 << nr_planes);
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* Return the maximum request size of the X server of display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On MS Windows, this function just returns 1.
On Nextstep, this function just returns nil.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_fixnum (MAXREQUEST (dpyinfo->display));
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Return the "vendor ID" string of the GUI software on TERMINAL.

\(Labeling every distributor as a "vendor" embodies the false assumption
that operating systems cannot be developed and distributed noncommercially.)
The optional argument TERMINAL specifies which display to ask about.

For GNU and Unix systems, this queries the X server software.
For MS Windows and Nextstep the result is hard-coded.

TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  const char *vendor = ServerVendor (dpyinfo->display);

  if (! vendor) vendor = "";
  return build_string (vendor);
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Return the version numbers of the GUI software on TERMINAL.
The value is a list of three integers specifying the version of the GUI
software in use.

For GNU and Unix system, the first 2 numbers are the version of the X
Protocol used on TERMINAL and the 3rd number is the distributor-specific
release number.  For MS Windows, the 3 numbers report the OS major and
minor version and build number.  For Nextstep, the first 2 numbers are
hard-coded and the 3rd represents the OS version.

See also the function `x-server-vendor'.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  Display *dpy = dpyinfo->display;

  return list3i (ProtocolVersion (dpy), ProtocolRevision (dpy),
		 VendorRelease (dpy));
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Return the number of screens on the X server of display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On MS Windows, this function just returns 1.
On Nextstep, "screen" is in X terminology, not that of Nextstep.
For the number of physical monitors, use `(length
\(display-monitor-attributes-list TERMINAL))' instead.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_fixnum (ScreenCount (dpyinfo->display));
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* Return the height in millimeters of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.
\(On MS Windows, this function does not accept terminal objects.)

On \"multi-monitor\" setups this refers to the height in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_fixnum (HeightMMOfScreen (dpyinfo->screen));
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* Return the width in millimeters of the X display TERMINAL.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.
\(On MS Windows, this function does not accept terminal objects.)

On \"multi-monitor\" setups this refers to the width in millimeters for
all physical monitors associated with TERMINAL.  To get information
for each physical monitor, use `display-monitor-attributes-list'.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  return make_fixnum (WidthMMOfScreen (dpyinfo->screen));
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* Return an indication of whether X display TERMINAL does backing store.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

The value may be `always', `when-mapped', or `not-useful'.
On Nextstep, the value may be `buffered', `retained', or `non-retained'.
On MS Windows, this returns nothing useful.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  Lisp_Object result;

  switch (DoesBackingStore (dpyinfo->screen))
    {
    case Always:
      result = intern ("always");
      break;

    case WhenMapped:
      result = intern ("when-mapped");
      break;

    case NotUseful:
      result = intern ("not-useful");
      break;

    default:
      error ("Strange value for BackingStore parameter of screen");
    }

  return result;
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* Return the visual class of the X display TERMINAL.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.
\(On MS Windows, the second and last result above are not possible.)

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.
\(On MS Windows, this function does not accept terminal objects.)  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  Lisp_Object result;

  switch (dpyinfo->visual->class)
    {
    case StaticGray:
      result = intern ("static-gray");
      break;
    case GrayScale:
      result = intern ("gray-scale");
      break;
    case StaticColor:
      result = intern ("static-color");
      break;
    case PseudoColor:
      result = intern ("pseudo-color");
      break;
    case TrueColor:
      result = intern ("true-color");
      break;
    case DirectColor:
      result = intern ("direct-color");
      break;
    default:
      error ("Display has an unknown visual class");
    }

  return result;
}

DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* Return t if the X display TERMINAL supports the save-under feature.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

On MS Windows, this just returns nil.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  if (DoesSaveUnders (dpyinfo->screen) == True)
    return Qt;
  else
    return Qnil;
}

#if !defined USE_GTK || !defined HAVE_GTK3

/* Store the geometry of the workarea on display DPYINFO into *RECT.
   Return false if and only if the workarea information cannot be
   obtained via the _NET_WORKAREA root window property.  */

static bool
x_get_net_workarea (struct x_display_info *dpyinfo, XRectangle *rect)
{
  Display *dpy = dpyinfo->display;
  long offset, max_len;
  Atom target_type, actual_type;
  unsigned long actual_size, bytes_remaining;
  int rc, actual_format;
  unsigned char *tmp_data = NULL;
  bool result = false;

  x_catch_errors (dpy);
  offset = 0;
  max_len = 1;
  target_type = XA_CARDINAL;
  rc = XGetWindowProperty (dpy, dpyinfo->root_window,
			   dpyinfo->Xatom_net_current_desktop,
			   offset, max_len, False, target_type,
			   &actual_type, &actual_format, &actual_size,
			   &bytes_remaining, &tmp_data);
  if (rc == Success && actual_type == target_type && !x_had_errors_p (dpy)
      && actual_format == 32 && actual_size == max_len)
    {
      long current_desktop = ((long *) tmp_data)[0];

      XFree (tmp_data);
      tmp_data = NULL;

      offset = 4 * current_desktop;
      max_len = 4;
      rc = XGetWindowProperty (dpy, dpyinfo->root_window,
			       dpyinfo->Xatom_net_workarea,
			       offset, max_len, False, target_type,
			       &actual_type, &actual_format, &actual_size,
			       &bytes_remaining, &tmp_data);
      if (rc == Success && actual_type == target_type && !x_had_errors_p (dpy)
	  && actual_format == 32 && actual_size == max_len)
	{
	  long *values = (long *) tmp_data;

	  rect->x = values[0];
	  rect->y = values[1];
	  rect->width = values[2];
	  rect->height = values[3];

	  XFree (tmp_data);
	  tmp_data = NULL;

	  result = true;
	}
    }
  if (tmp_data)
    XFree (tmp_data);
  x_uncatch_errors ();

  return result;
}

/* Return monitor number where F is "most" or closest to.  */
static int
x_get_monitor_for_frame (struct frame *f,
                         struct MonitorInfo *monitors,
                         int n_monitors)
{
  XRectangle frect;
  int area = 0, dist = -1;
  int best_area = -1, best_dist = -1;
  int i;

  if (n_monitors == 1) return 0;
  frect.x = f->left_pos;
  frect.y = f->top_pos;
  frect.width = FRAME_PIXEL_WIDTH (f);
  frect.height = FRAME_PIXEL_HEIGHT (f);

  for (i = 0; i < n_monitors; ++i)
    {
      struct MonitorInfo *mi = &monitors[i];
      XRectangle res;
      int a = 0;

      if (mi->geom.width == 0) continue;

      if (gui_intersect_rectangles (&mi->geom, &frect, &res))
        {
          a = res.width * res.height;
          if (a > area)
	    {
	      area = a;
	      best_area = i;
	    }
        }

      if (a == 0 && area == 0)
        {
          int dx, dy, d;
          if (frect.x + frect.width < mi->geom.x)
            dx = mi->geom.x - frect.x + frect.width;
          else if (frect.x > mi->geom.x + mi->geom.width)
            dx = frect.x - mi->geom.x + mi->geom.width;
          else
            dx = 0;
          if (frect.y + frect.height < mi->geom.y)
            dy = mi->geom.y - frect.y + frect.height;
          else if (frect.y > mi->geom.y + mi->geom.height)
            dy = frect.y - mi->geom.y + mi->geom.height;
          else
            dy = 0;

          d = dx*dx + dy*dy;
          if (dist == -1 || dist > d)
            {
              dist = d;
              best_dist = i;
            }
        }
    }

  return best_area != -1 ? best_area : (best_dist != -1 ? best_dist : 0);
}

static Lisp_Object
x_make_monitor_attribute_list (struct MonitorInfo *monitors,
                               int n_monitors,
                               int primary_monitor,
                               struct x_display_info *dpyinfo,
                               const char *source)
{
  Lisp_Object monitor_frames = make_nil_vector (n_monitors);
  Lisp_Object frame, rest;

  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_X_P (f)
	  && FRAME_DISPLAY_INFO (f) == dpyinfo
	  && !FRAME_TOOLTIP_P (f))
	{
	  int i = x_get_monitor_for_frame (f, monitors, n_monitors);
	  ASET (monitor_frames, i, Fcons (frame, AREF (monitor_frames, i)));
	}
    }

  return make_monitor_attribute_list (monitors, n_monitors, primary_monitor,
                                      monitor_frames, source);
}

static Lisp_Object
x_get_monitor_attributes_fallback (struct x_display_info *dpyinfo)
{
  struct MonitorInfo monitor;
  XRectangle workarea_r;

  /* Fallback: treat (possibly) multiple physical monitors as if they
     formed a single monitor as a whole.  This should provide a
     consistent result at least on single monitor environments.  */
  monitor.geom.x = monitor.geom.y = 0;
  monitor.geom.width = x_display_pixel_width (dpyinfo);
  monitor.geom.height = x_display_pixel_height (dpyinfo);
  monitor.mm_width = WidthMMOfScreen (dpyinfo->screen);
  monitor.mm_height = HeightMMOfScreen (dpyinfo->screen);
  monitor.name = xstrdup ("combined screen");

  if (x_get_net_workarea (dpyinfo, &workarea_r))
    monitor.work = workarea_r;
  else
    monitor.work = monitor.geom;
  return x_make_monitor_attribute_list (&monitor, 1, 0, dpyinfo, "fallback");
}


#ifdef HAVE_XINERAMA
static Lisp_Object
x_get_monitor_attributes_xinerama (struct x_display_info *dpyinfo)
{
  int n_monitors, i;
  Lisp_Object attributes_list = Qnil;
  Display *dpy = dpyinfo->display;
  XineramaScreenInfo *info = XineramaQueryScreens (dpy, &n_monitors);
  struct MonitorInfo *monitors;
  double mm_width_per_pixel, mm_height_per_pixel;

  if (! info || n_monitors == 0)
    {
      if (info)
	XFree (info);
      return attributes_list;
    }

  mm_width_per_pixel = ((double) WidthMMOfScreen (dpyinfo->screen)
			/ x_display_pixel_width (dpyinfo));
  mm_height_per_pixel = ((double) HeightMMOfScreen (dpyinfo->screen)
			 / x_display_pixel_height (dpyinfo));
  monitors = xzalloc (n_monitors * sizeof *monitors);
  for (i = 0; i < n_monitors; ++i)
    {
      struct MonitorInfo *mi = &monitors[i];
      XRectangle workarea_r;

      mi->geom.x = info[i].x_org;
      mi->geom.y = info[i].y_org;
      mi->geom.width = info[i].width;
      mi->geom.height = info[i].height;
      mi->mm_width = mi->geom.width * mm_width_per_pixel + 0.5;
      mi->mm_height = mi->geom.height * mm_height_per_pixel + 0.5;
      mi->name = 0;

      /* Xinerama usually have primary monitor first, just use that.  */
      if (i == 0 && x_get_net_workarea (dpyinfo, &workarea_r))
	{
	  mi->work = workarea_r;
	  if (! gui_intersect_rectangles (&mi->geom, &mi->work, &mi->work))
	    mi->work = mi->geom;
	}
      else
	mi->work = mi->geom;
    }
  XFree (info);

  attributes_list = x_make_monitor_attribute_list (monitors,
                                                   n_monitors,
                                                   0,
                                                   dpyinfo,
                                                   "Xinerama");
  free_monitors (monitors, n_monitors);
  return attributes_list;
}
#endif /* HAVE_XINERAMA */


#ifdef HAVE_XRANDR
static Lisp_Object
x_get_monitor_attributes_xrandr (struct x_display_info *dpyinfo)
{
  Lisp_Object attributes_list = Qnil;
  XRRScreenResources *resources;
  Display *dpy = dpyinfo->display;
  int i, n_monitors, primary = -1;
  RROutput pxid = None;
  struct MonitorInfo *monitors;

#define RANDR13_LIBRARY \
  (RANDR_MAJOR > 1 || (RANDR_MAJOR == 1 && RANDR_MINOR >= 3))

#if RANDR13_LIBRARY
  /* Check if the display supports 1.3 too.  */
  bool randr13_avail = (dpyinfo->xrandr_major_version > 1
			|| (dpyinfo->xrandr_major_version == 1
			    && dpyinfo->xrandr_minor_version >= 3));

  if (randr13_avail)
    resources = XRRGetScreenResourcesCurrent (dpy, dpyinfo->root_window);
  else
    resources = XRRGetScreenResources (dpy, dpyinfo->root_window);
#else
  resources = XRRGetScreenResources (dpy, dpyinfo->root_window);
#endif
  if (! resources || resources->noutput == 0)
    {
      if (resources)
	XRRFreeScreenResources (resources);
      return Qnil;
    }
  n_monitors = resources->noutput;
  monitors = xzalloc (n_monitors * sizeof *monitors);

#if RANDR13_LIBRARY
  if (randr13_avail)
    pxid = XRRGetOutputPrimary (dpy, dpyinfo->root_window);
#endif

  for (i = 0; i < n_monitors; ++i)
    {
      XRROutputInfo *info = XRRGetOutputInfo (dpy, resources,
                                              resources->outputs[i]);
      if (!info)
	continue;

      if (strcmp (info->name, "default") == 0)
        {
          /* Non XRandr 1.2 driver, does not give useful data.  */
	  XRRFreeOutputInfo (info);
	  XRRFreeScreenResources (resources);
          free_monitors (monitors, n_monitors);
          return Qnil;
        }

      if (info->connection != RR_Disconnected && info->crtc != None)
        {
          XRRCrtcInfo *crtc = XRRGetCrtcInfo (dpy, resources, info->crtc);
          struct MonitorInfo *mi = &monitors[i];
          XRectangle workarea_r;

          if (! crtc)
	    {
	      XRRFreeOutputInfo (info);
	      continue;
	    }

          mi->geom.x = crtc->x;
          mi->geom.y = crtc->y;
          mi->geom.width = crtc->width;
          mi->geom.height = crtc->height;
          mi->mm_width = info->mm_width;
          mi->mm_height = info->mm_height;
          mi->name = xstrdup (info->name);

          if (pxid != None && pxid == resources->outputs[i])
            primary = i;
          else if (primary == -1 && strcmp (info->name, "LVDS") == 0)
            primary = i;

          if (i == primary && x_get_net_workarea (dpyinfo, &workarea_r))
            {
              mi->work= workarea_r;
              if (! gui_intersect_rectangles (&mi->geom, &mi->work, &mi->work))
                mi->work = mi->geom;
            }
          else
            mi->work = mi->geom;

          XRRFreeCrtcInfo (crtc);
        }
      XRRFreeOutputInfo (info);
    }
  XRRFreeScreenResources (resources);

  attributes_list = x_make_monitor_attribute_list (monitors,
                                                   n_monitors,
                                                   primary,
                                                   dpyinfo,
                                                   "XRandr");
  free_monitors (monitors, n_monitors);
  return attributes_list;
}
#endif /* HAVE_XRANDR */

static Lisp_Object
x_get_monitor_attributes (struct x_display_info *dpyinfo)
{
  Lisp_Object attributes_list = Qnil;
  Display *dpy = dpyinfo->display;

  (void) dpy; /* Suppress unused variable warning.  */

#ifdef HAVE_XRANDR
  int xrr_event_base, xrr_error_base;
  bool xrr_ok = false;
  xrr_ok = XRRQueryExtension (dpy, &xrr_event_base, &xrr_error_base);
  if (xrr_ok)
    {
      XRRQueryVersion (dpy, &dpyinfo->xrandr_major_version,
		       &dpyinfo->xrandr_minor_version);
      xrr_ok = ((dpyinfo->xrandr_major_version == 1
		 && dpyinfo->xrandr_minor_version >= 2)
		|| dpyinfo->xrandr_major_version > 1);
    }

  if (xrr_ok)
    attributes_list = x_get_monitor_attributes_xrandr (dpyinfo);
#endif /* HAVE_XRANDR */

#ifdef HAVE_XINERAMA
  if (NILP (attributes_list))
    {
      int xin_event_base, xin_error_base;
      bool xin_ok = false;
      xin_ok = XineramaQueryExtension (dpy, &xin_event_base, &xin_error_base);
      if (xin_ok && XineramaIsActive (dpy))
        attributes_list = x_get_monitor_attributes_xinerama (dpyinfo);
    }
#endif /* HAVE_XINERAMA */

  if (NILP (attributes_list))
    attributes_list = x_get_monitor_attributes_fallback (dpyinfo);

  return attributes_list;
}

#endif /* !USE_GTK */

DEFUN ("x-display-monitor-attributes-list", Fx_display_monitor_attributes_list,
       Sx_display_monitor_attributes_list,
       0, 1, 0,
       doc: /* Return a list of physical monitor attributes on the X display TERMINAL.

The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be a terminal object, a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.

In addition to the standard attribute keys listed in
`display-monitor-attributes-list', the following keys are contained in
the attributes:

 source -- String describing the source from which multi-monitor
	   information is obtained, one of \"Gdk\", \"XRandr\",
	   \"Xinerama\", or \"fallback\"

Internal use only, use `display-monitor-attributes-list' instead.  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  Lisp_Object attributes_list = Qnil;

#ifdef USE_GTK
  GdkDisplay *gdpy;
#if ! GTK_CHECK_VERSION (3, 22, 0)
  double mm_width_per_pixel, mm_height_per_pixel;
  GdkScreen *gscreen;
#endif
  gint primary_monitor = 0, n_monitors, i;
  Lisp_Object monitor_frames, rest, frame;
  static const char *source = "Gdk";
  struct MonitorInfo *monitors;

  block_input ();
  gdpy = gdk_x11_lookup_xdisplay (dpyinfo->display);
#if GTK_CHECK_VERSION (3, 22, 0)
  n_monitors = gdk_display_get_n_monitors (gdpy);
#else
  gscreen = gdk_display_get_default_screen (gdpy);
  n_monitors = gdk_screen_get_n_monitors (gscreen);
  primary_monitor = gdk_screen_get_primary_monitor (gscreen);
  /* Fallback if gdk_screen_get_monitor_{width,height}_mm fail */
  mm_width_per_pixel = ((double) WidthMMOfScreen (dpyinfo->screen)
			/ x_display_pixel_width (dpyinfo));
  mm_height_per_pixel = ((double) HeightMMOfScreen (dpyinfo->screen)
			 / x_display_pixel_height (dpyinfo));
#endif
  monitor_frames = make_nil_vector (n_monitors);
  monitors = xzalloc (n_monitors * sizeof *monitors);

  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_X_P (f)
	  && FRAME_DISPLAY_INFO (f) == dpyinfo
	  && !FRAME_TOOLTIP_P (f))
	{
	  GdkWindow *gwin = gtk_widget_get_window (FRAME_GTK_WIDGET (f));

#if GTK_CHECK_VERSION (3, 22, 0)
          for (i = 0; i < n_monitors; i++)
            if (gdk_display_get_monitor_at_window (gdpy, gwin)
                == gdk_display_get_monitor (gdpy, i))
              break;
#else
	  i = gdk_screen_get_monitor_at_window (gscreen, gwin);
#endif
	  ASET (monitor_frames, i, Fcons (frame, AREF (monitor_frames, i)));
	}
    }

  for (i = 0; i < n_monitors; ++i)
    {
      gint width_mm, height_mm;
      GdkRectangle rec, work;
      struct MonitorInfo *mi = &monitors[i];
      int scale = 1;

#if GTK_CHECK_VERSION (3, 22, 0)
      GdkMonitor *monitor = gdk_display_get_monitor (gdpy, i);
      if (gdk_monitor_is_primary (monitor))
        primary_monitor = i;
      gdk_monitor_get_geometry (monitor, &rec);
#else
      gdk_screen_get_monitor_geometry (gscreen, i, &rec);
#endif

#if GTK_CHECK_VERSION (3, 22, 0)
      width_mm = gdk_monitor_get_width_mm (monitor);
      height_mm = gdk_monitor_get_height_mm (monitor);
#else
      width_mm = gdk_screen_get_monitor_width_mm (gscreen, i);
      height_mm = gdk_screen_get_monitor_height_mm (gscreen, i);
      if (width_mm < 0)
	width_mm = rec.width * mm_width_per_pixel + 0.5;
      if (height_mm < 0)
	height_mm = rec.height * mm_height_per_pixel + 0.5;
#endif
#if GTK_CHECK_VERSION (3, 22, 0)
      gdk_monitor_get_workarea (monitor, &work);
#elif defined HAVE_GTK3
      gdk_screen_get_monitor_workarea (gscreen, i, &work);
#else
      /* Emulate the behavior of GTK+ 3.4.  */
      {
	XRectangle workarea_r;

	if (i == primary_monitor && x_get_net_workarea (dpyinfo, &workarea_r))
	  {
	    work.x = workarea_r.x;
	    work.y = workarea_r.y;
	    work.width = workarea_r.width;
	    work.height = workarea_r.height;
	    if (! gdk_rectangle_intersect (&rec, &work, &work))
              work = rec;
          }
        else
          work = rec;
      }
#endif

      /* GTK returns scaled sizes for the workareas.  */
#if GTK_CHECK_VERSION (3, 22, 0)
      scale = gdk_monitor_get_scale_factor (monitor);
#elif defined HAVE_GTK3
      scale = gdk_screen_get_monitor_scale_factor (gscreen, i);
#endif
      rec.x *= scale;
      rec.y *= scale;
      rec.width *= scale;
      rec.height *= scale;
      work.x *= scale;
      work.y *= scale;
      work.width *= scale;
      work.height *= scale;

      mi->geom.x = rec.x;
      mi->geom.y = rec.y;
      mi->geom.width = rec.width;
      mi->geom.height = rec.height;
      mi->work.x = work.x;
      mi->work.y = work.y;
      mi->work.width = work.width;
      mi->work.height = work.height;
      mi->mm_width = width_mm;
      mi->mm_height = height_mm;

#if GTK_CHECK_VERSION (3, 22, 0)
      dupstring (&mi->name, (gdk_monitor_get_model (monitor)));
#else
      mi->name = gdk_screen_get_monitor_plug_name (gscreen, i);
#endif
    }

  attributes_list = make_monitor_attribute_list (monitors,
                                                 n_monitors,
                                                 primary_monitor,
                                                 monitor_frames,
                                                 source);
  free_monitors (monitors, n_monitors);
  unblock_input ();
#else  /* not USE_GTK */

  block_input ();
  attributes_list = x_get_monitor_attributes (dpyinfo);
  unblock_input ();

#endif	/* not USE_GTK */

  return attributes_list;
}

/* Return geometric attributes of FRAME.  According to the value of
   ATTRIBUTES return the outer edges of FRAME (Qouter_edges), the native
   edges of FRAME (Qnative_edges), or the inner edges of frame
   (Qinner_edges).  Any other value means to return the geometry as
   returned by Fx_frame_geometry.  */
static Lisp_Object
frame_geometry (Lisp_Object frame, Lisp_Object attribute)
{
  struct frame *f = decode_live_frame (frame);
  /**   XWindowAttributes atts; **/
  Window rootw;
  unsigned int ign, native_width, native_height, x_border_width = 0;
  int x_native = 0, y_native = 0, xptr = 0, yptr = 0;
  int left_off = 0, right_off = 0, top_off = 0, bottom_off = 0;
  int outer_left, outer_top, outer_right, outer_bottom;
  int native_left, native_top, native_right, native_bottom;
  int inner_left, inner_top, inner_right, inner_bottom;
  int internal_border_width;
  bool menu_bar_external = false, tool_bar_external = false;
  int menu_bar_height = 0, menu_bar_width = 0;
  int tab_bar_height = 0, tab_bar_width = 0;
  int tool_bar_height = 0, tool_bar_width = 0;

  if (FRAME_INITIAL_P (f) || !FRAME_X_P (f) || !FRAME_OUTER_WINDOW (f))
    return Qnil;

  block_input ();
  XGetGeometry (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		&rootw, &x_native, &y_native, &native_width, &native_height,
		&x_border_width, &ign);
  /**   XGetWindowAttributes (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), &atts); **/
  if (!FRAME_PARENT_FRAME (f))
    x_real_pos_and_offsets (f, &left_off, &right_off, &top_off, &bottom_off,
			    NULL, NULL, &xptr, &yptr, NULL);
  unblock_input ();

  /**   native_width = atts.width; **/
  /**   native_height = atts.height; **/

  if (FRAME_PARENT_FRAME (f))
    {
      Lisp_Object parent, edges;

      XSETFRAME (parent, FRAME_PARENT_FRAME (f));
      edges = Fx_frame_edges (parent, Qnative_edges);
      if (!NILP (edges))
	{
	  x_native += XFIXNUM (Fnth (make_fixnum (0), edges));
	  y_native += XFIXNUM (Fnth (make_fixnum (1), edges));
	}

      outer_left = x_native;
      outer_top = y_native;
      outer_right = outer_left + native_width + 2 * x_border_width;
      outer_bottom = outer_top + native_height + 2 * x_border_width;

      native_left = x_native + x_border_width;
      native_top = y_native + x_border_width;
      native_right = native_left + native_width;
      native_bottom = native_top + native_height;
    }
  else
    {
      outer_left = xptr;
      outer_top = yptr;
      outer_right = outer_left + left_off + native_width + right_off;
      outer_bottom = outer_top + top_off + native_height + bottom_off;

      native_left = outer_left + left_off;
      native_top = outer_top + top_off;
      native_right = native_left + native_width;
      native_bottom = native_top + native_height;
    }

  internal_border_width = FRAME_INTERNAL_BORDER_WIDTH (f);
  inner_left = native_left + internal_border_width;
  inner_top = native_top + internal_border_width;
  inner_right = native_right - internal_border_width;
  inner_bottom = native_bottom - internal_border_width;

#ifdef HAVE_EXT_MENU_BAR
  menu_bar_external = true;
  menu_bar_height = FRAME_MENUBAR_HEIGHT (f);
  native_top += menu_bar_height;
  inner_top += menu_bar_height;
#else
  menu_bar_height = FRAME_MENU_BAR_HEIGHT (f);
  inner_top += menu_bar_height;
#endif
  menu_bar_width = menu_bar_height ? native_width : 0;

  tab_bar_height = FRAME_TAB_BAR_HEIGHT (f);
  tab_bar_width = (tab_bar_height
		    ? native_width - 2 * internal_border_width
		    : 0);
  inner_top += tab_bar_height;

#ifdef HAVE_EXT_TOOL_BAR
  tool_bar_external = true;
  if (EQ (FRAME_TOOL_BAR_POSITION (f), Qleft))
    {
      tool_bar_width = FRAME_TOOLBAR_WIDTH (f);
      native_left += tool_bar_width;
      inner_left += tool_bar_width;
      tool_bar_height
	= tool_bar_width ? native_height - menu_bar_height : 0;
    }
  else if (EQ (FRAME_TOOL_BAR_POSITION (f), Qtop))
    {
      tool_bar_height = FRAME_TOOLBAR_HEIGHT (f);
      native_top += tool_bar_height;
      inner_top += tool_bar_height;
      tool_bar_width = tool_bar_height ? native_width : 0;
    }
  else if (EQ (FRAME_TOOL_BAR_POSITION (f), Qright))
    {
      tool_bar_width = FRAME_TOOLBAR_WIDTH (f);
      native_right -= tool_bar_width;
      inner_right -= tool_bar_width;
      tool_bar_height
	= tool_bar_width ? native_height - menu_bar_height : 0;
    }
  else
    {
      tool_bar_height = FRAME_TOOLBAR_HEIGHT (f);
      native_bottom -= tool_bar_height;
      inner_bottom -= tool_bar_height;
      tool_bar_width = tool_bar_height ? native_width : 0;
    }
#else
  tool_bar_height = FRAME_TOOL_BAR_HEIGHT (f);
  tool_bar_width = (tool_bar_height
		    ? native_width - 2 * internal_border_width
		    : 0);
  inner_top += tool_bar_height;
#endif

  /* Construct list.  */
  if (EQ (attribute, Qouter_edges))
    return list4i (outer_left, outer_top, outer_right, outer_bottom);
  else if (EQ (attribute, Qnative_edges))
    return list4i (native_left, native_top, native_right, native_bottom);
  else if (EQ (attribute, Qinner_edges))
    return list4i (inner_left, inner_top, inner_right, inner_bottom);
  else
    return
       list (Fcons (Qouter_position,
		    Fcons (make_fixnum (outer_left),
			   make_fixnum (outer_top))),
	     Fcons (Qouter_size,
		    Fcons (make_fixnum (outer_right - outer_left),
			   make_fixnum (outer_bottom - outer_top))),
	     /* Approximate.  */
	     Fcons (Qexternal_border_size,
		    Fcons (make_fixnum (right_off),
			   make_fixnum (bottom_off))),
	     Fcons (Qouter_border_width, make_fixnum (x_border_width)),
	     /* Approximate.  */
	     Fcons (Qtitle_bar_size,
		    Fcons (make_fixnum (0),
			   make_fixnum (top_off - bottom_off))),
	     Fcons (Qmenu_bar_external, menu_bar_external ? Qt : Qnil),
	     Fcons (Qmenu_bar_size,
		    Fcons (make_fixnum (menu_bar_width),
			   make_fixnum (menu_bar_height))),
	     Fcons (Qtab_bar_size,
		    Fcons (make_fixnum (tab_bar_width),
			   make_fixnum (tab_bar_height))),
	     Fcons (Qtool_bar_external, tool_bar_external ? Qt : Qnil),
	     Fcons (Qtool_bar_position, FRAME_TOOL_BAR_POSITION (f)),
	     Fcons (Qtool_bar_size,
		    Fcons (make_fixnum (tool_bar_width),
			   make_fixnum (tool_bar_height))),
	     Fcons (Qinternal_border_width,
		    make_fixnum (internal_border_width)));
}

DEFUN ("x-frame-geometry", Fx_frame_geometry, Sx_frame_geometry, 0, 1, 0,
       doc: /* Return geometric attributes of FRAME.
FRAME must be a live frame and defaults to the selected one.  The return
value is an association list of the attributes listed below.  All height
and width values are in pixels.

`outer-position' is a cons of the outer left and top edges of FRAME
  relative to the origin - the position (0, 0) - of FRAME's display.

`outer-size' is a cons of the outer width and height of FRAME.  The
  outer size includes the title bar and the external borders as well as
  any menu and/or tool bar of frame.  For a child frame the value
  includes FRAME's X borders, if any.

`external-border-size' is a cons of the horizontal and vertical width of
  FRAME's external borders as supplied by the window manager.

`title-bar-size' is a cons of the width and height of the title bar of
  FRAME as supplied by the window manager.  If both of them are zero,
  FRAME has no title bar.  If only the width is zero, Emacs was not
  able to retrieve the width information.

`menu-bar-external', if non-nil, means the menu bar is external (never
  included in the inner edges of FRAME).

`menu-bar-size' is a cons of the width and height of the menu bar of
  FRAME.

`tool-bar-external', if non-nil, means the tool bar is external (never
  included in the inner edges of FRAME).

`tool-bar-position' tells on which side the tool bar on FRAME is and can
  be one of `left', `top', `right' or `bottom'.  If this is nil, FRAME
  has no tool bar.

`tool-bar-size' is a cons of the width and height of the tool bar of
  FRAME.

`internal-border-width' is the width of the internal border of
  FRAME.

`outer-border-width' is the width of the X border of FRAME.  The X
  border is usually shown only for frames without window manager
  decorations, such as child and tooltip frames.  */)
  (Lisp_Object frame)
{
  return frame_geometry (frame, Qnil);
}

DEFUN ("x-frame-edges", Fx_frame_edges, Sx_frame_edges, 0, 2, 0,
       doc: /* Return edge coordinates of FRAME.
FRAME must be a live frame and defaults to the selected one.  The return
value is a list of the form (LEFT, TOP, RIGHT, BOTTOM).  All values are
in pixels relative to the origin - the position (0, 0) - of FRAME's
display.

If optional argument TYPE is the symbol `outer-edges', return the outer
edges of FRAME.  The outer edges comprise the decorations of the window
manager (like the title bar or external borders) as well as any external
menu or tool bar of FRAME.  If optional argument TYPE is the symbol
`native-edges' or nil, return the native edges of FRAME.  The native
edges exclude the decorations of the window manager and any external
menu or tool bar of FRAME.  If TYPE is the symbol `inner-edges', return
the inner edges of FRAME.  These edges exclude title bar, any borders,
menu bar or tool bar of FRAME.  */)
  (Lisp_Object frame, Lisp_Object type)
{
  return frame_geometry (frame, ((EQ (type, Qouter_edges)
				  || EQ (type, Qinner_edges))
				 ? type
				 : Qnative_edges));
}

/**
 * x_frame_list_z_order:
 *
 * Recursively add list of all frames on the display specified via
 * DPYINFO and whose window-system window's parent is specified by
 * WINDOW to FRAMES and return FRAMES.
 */
static Lisp_Object
x_frame_list_z_order (Display* dpy, Window window)
{
  Window root, parent, *children;
  unsigned int nchildren;
  int i;
  Lisp_Object frames = Qnil;

  block_input ();
  if (XQueryTree (dpy, window, &root, &parent, &children, &nchildren))
    {
      unblock_input ();
      for (i = 0; i < nchildren; i++)
	{
	  Lisp_Object frame, tail;

	  FOR_EACH_FRAME (tail, frame)
            {
              struct frame *cf = XFRAME (frame);
              /* With a reparenting window manager the parent_desc
                 field usually specifies the topmost windows of our
                 frames.  Otherwise FRAME_OUTER_WINDOW should do.  */
              if (FRAME_X_P (cf)
                  && (cf->output_data.x->parent_desc == children[i]
                      || FRAME_OUTER_WINDOW (cf) == children[i]))
                frames = Fcons (frame, frames);
            }
	}

      if (children) XFree ((char *)children);
    }
  else
    unblock_input ();

  return frames;
}


DEFUN ("x-frame-list-z-order", Fx_frame_list_z_order,
       Sx_frame_list_z_order, 0, 1, 0,
       doc: /* Return list of Emacs' frames, in Z (stacking) order.
The optional argument TERMINAL specifies which display to ask about.
TERMINAL should be either a frame or a display name (a string).  If
omitted or nil, that stands for the selected frame's display.  Return
nil if TERMINAL contains no Emacs frame.

As a special case, if TERMINAL is non-nil and specifies a live frame,
return the child frames of that frame in Z (stacking) order.

Frames are listed from topmost (first) to bottommost (last).  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);
  Display *dpy = dpyinfo->display;
  Window window;

  if (FRAMEP (terminal) && FRAME_LIVE_P (XFRAME (terminal)))
    window = FRAME_X_WINDOW (XFRAME (terminal));
  else
    window = dpyinfo->root_window;

  return x_frame_list_z_order (dpy, window);
}

/**
 * x_frame_restack:
 *
 * Restack frame F1 below frame F2, above if ABOVE_FLAG is non-nil.  In
 * practice this is a two-step action: The first step removes F1's
 * window-system window from the display.  The second step reinserts
 * F1's window below (above if ABOVE_FLAG is true) that of F2.
 */
static void
x_frame_restack (struct frame *f1, struct frame *f2, bool above_flag)
{
#ifdef USE_GTK
  block_input ();
  xg_frame_restack (f1, f2, above_flag);
  unblock_input ();
#else
  Display *dpy = FRAME_X_DISPLAY (f1);
  Window window1 = FRAME_OUTER_WINDOW (f1);
  XWindowChanges wc;
  unsigned long mask = (CWSibling | CWStackMode);

  wc.sibling = FRAME_OUTER_WINDOW (f2);
  wc.stack_mode = above_flag ? Above : Below;
  block_input ();
  /* Configure the window manager window (a normal XConfigureWindow
     won't cut it).  This should also work for child frames.  */
  XReconfigureWMWindow (dpy, window1, FRAME_X_SCREEN_NUMBER (f1), mask, &wc);
  unblock_input ();
#endif /* USE_GTK */
}


DEFUN ("x-frame-restack", Fx_frame_restack, Sx_frame_restack, 2, 3, 0,
       doc: /* Restack FRAME1 below FRAME2.
This means that if both frames are visible and the display areas of
these frames overlap, FRAME2 (partially) obscures FRAME1.  If optional
third argument ABOVE is non-nil, restack FRAME1 above FRAME2.  This
means that if both frames are visible and the display areas of these
frames overlap, FRAME1 (partially) obscures FRAME2.

This may be thought of as an atomic action performed in two steps: The
first step removes FRAME1's window-step window from the display.  The
second step reinserts FRAME1's window below (above if ABOVE is true)
that of FRAME2.  Hence the position of FRAME2 in its display's Z
\(stacking) order relative to all other frames excluding FRAME1 remains
unaltered.

Some window managers may refuse to restack windows.  */)
     (Lisp_Object frame1, Lisp_Object frame2, Lisp_Object above)
{
  struct frame *f1 = decode_live_frame (frame1);
  struct frame *f2 = decode_live_frame (frame2);

  if (! (FRAME_OUTER_WINDOW (f1) && FRAME_OUTER_WINDOW (f2)))
    error ("Cannot restack frames");
  x_frame_restack (f1, f2, !NILP (above));
  return Qt;
}


DEFUN ("x-mouse-absolute-pixel-position", Fx_mouse_absolute_pixel_position,
       Sx_mouse_absolute_pixel_position, 0, 0, 0,
       doc: /* Return absolute position of mouse cursor in pixels.
The position is returned as a cons cell (X . Y) of the coordinates of
the mouse cursor position in pixels relative to a position (0, 0) of the
selected frame's display.  */)
  (void)
{
  struct frame *f = SELECTED_FRAME ();
  Window root, dummy_window;
  int x, y, dummy;

  if (FRAME_INITIAL_P (f) || !FRAME_X_P (f))
    return Qnil;

  block_input ();
  XQueryPointer (FRAME_X_DISPLAY (f),
                 DefaultRootWindow (FRAME_X_DISPLAY (f)),
                 &root, &dummy_window, &x, &y, &dummy, &dummy,
                 (unsigned int *) &dummy);
  unblock_input ();

  return Fcons (make_fixnum (x), make_fixnum (y));
}

DEFUN ("x-set-mouse-absolute-pixel-position", Fx_set_mouse_absolute_pixel_position,
       Sx_set_mouse_absolute_pixel_position, 2, 2, 0,
       doc: /* Move mouse pointer to absolute pixel position (X, Y).
The coordinates X and Y are interpreted in pixels relative to a position
\(0, 0) of the selected frame's display.  */)
  (Lisp_Object x, Lisp_Object y)
  {
  struct frame *f = SELECTED_FRAME ();

  if (FRAME_INITIAL_P (f) || !FRAME_X_P (f))
    return Qnil;

  CHECK_TYPE_RANGED_INTEGER (int, x);
  CHECK_TYPE_RANGED_INTEGER (int, y);

  block_input ();
  XWarpPointer (FRAME_X_DISPLAY (f), None, DefaultRootWindow (FRAME_X_DISPLAY (f)),
		0, 0, 0, 0, XFIXNUM (x), XFIXNUM (y));
  unblock_input ();

  return Qnil;
}

/************************************************************************
			      X Displays
 ************************************************************************/


/* Mapping visual names to visuals.  */

static struct visual_class
{
  const char *name;
  int class;
}
visual_classes[] =
{
  {"StaticGray",	StaticGray},
  {"GrayScale",		GrayScale},
  {"StaticColor",	StaticColor},
  {"PseudoColor",	PseudoColor},
  {"TrueColor",		TrueColor},
  {"DirectColor",	DirectColor},
  {NULL, 0}
};


#ifndef HAVE_XSCREENNUMBEROFSCREEN

/* Value is the screen number of screen SCR.  This is a substitute for
   the X function with the same name when that doesn't exist.  */

int
XScreenNumberOfScreen (scr)
    register Screen *scr;
{
  Display *dpy = scr->display;
  int i;

  for (i = 0; i < dpy->nscreens; ++i)
    if (scr == dpy->screens + i)
      break;

  return i;
}

#endif /* not HAVE_XSCREENNUMBEROFSCREEN */


/* Select the visual that should be used on display DPYINFO.  Set
   members of DPYINFO appropriately.  Called from x_term_init.  */

void
select_visual (struct x_display_info *dpyinfo)
{
  Display *dpy = dpyinfo->display;
  Screen *screen = dpyinfo->screen;

  /* See if a visual is specified.  */
  AUTO_STRING (visualClass, "visualClass");
  AUTO_STRING (VisualClass, "VisualClass");
  Lisp_Object value = gui_display_get_resource (dpyinfo, visualClass,
                                                VisualClass, Qnil, Qnil);

  if (STRINGP (value))
    {
      /* VALUE should be of the form CLASS-DEPTH, where CLASS is one
	 of `PseudoColor', `TrueColor' etc. and DEPTH is the color
	 depth, a decimal number.  NAME is compared with case ignored.  */
      char *s = alloca (SBYTES (value) + 1);
      char *dash;
      int i, class = -1;
      XVisualInfo vinfo;

      lispstpcpy (s, value);
      dash = strchr (s, '-');
      if (dash)
	{
	  dpyinfo->n_planes = atoi (dash + 1);
	  *dash = '\0';
	}
      else
	/* We won't find a matching visual with depth 0, so that
	   an error will be printed below.  */
	dpyinfo->n_planes = 0;

      /* Determine the visual class.  */
      for (i = 0; visual_classes[i].name; ++i)
	if (xstrcasecmp (s, visual_classes[i].name) == 0)
	  {
	    class = visual_classes[i].class;
	    break;
	  }

      /* Look up a matching visual for the specified class.  */
      if (class == -1
	  || !XMatchVisualInfo (dpy, XScreenNumberOfScreen (screen),
				dpyinfo->n_planes, class, &vinfo))
	fatal ("Invalid visual specification '%s'",
	       SSDATA (ENCODE_SYSTEM (value)));

      dpyinfo->visual = vinfo.visual;
    }
  else
    {
      int n_visuals;
      XVisualInfo *vinfo, vinfo_template;

      dpyinfo->visual = DefaultVisualOfScreen (screen);

      vinfo_template.visualid = XVisualIDFromVisual (dpyinfo->visual);
      vinfo_template.screen = XScreenNumberOfScreen (screen);
      vinfo = XGetVisualInfo (dpy, VisualIDMask | VisualScreenMask,
			      &vinfo_template, &n_visuals);
      if (n_visuals <= 0)
	fatal ("Can't get proper X visual info");

      dpyinfo->n_planes = vinfo->depth;
      XFree (vinfo);
    }
}


/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */

static struct x_display_info *
x_display_info_for_name (Lisp_Object name)
{
  struct x_display_info *dpyinfo;

  CHECK_STRING (name);

  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    if (!NILP (Fstring_equal (XCAR (dpyinfo->name_list_element), name)))
      return dpyinfo;

  /* Use this general default value to start with.  */
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  dpyinfo = x_term_init (name, 0, SSDATA (Vx_resource_name));

  if (dpyinfo == 0)
    error ("Cannot connect to X server %s", SDATA (name));

  XSETFASTINT (Vwindow_system_version, 11);

  return dpyinfo;
}


DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0,
       doc: /* Open a connection to a display server.
DISPLAY is the name of the display to connect to.
Optional second arg XRM-STRING is a string of resources in xrdb format.
If the optional third arg MUST-SUCCEED is non-nil,
terminate Emacs if we can't open the connection.
\(In the Nextstep version, the last two arguments are currently ignored.)  */)
  (Lisp_Object display, Lisp_Object xrm_string, Lisp_Object must_succeed)
{
  char *xrm_option;
  struct x_display_info *dpyinfo;

  CHECK_STRING (display);
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string);

  xrm_option = NILP (xrm_string) ? 0 : SSDATA (xrm_string);

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = x_term_init (display, xrm_option,
			 SSDATA (Vx_resource_name));

  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Cannot connect to X server %s.\n\
Check the DISPLAY environment variable or use `-d'.\n\
Also use the `xauth' program to verify that you have the proper\n\
authorization information needed to connect the X server.\n\
An insecure way to solve the problem may be to use `xhost'.\n",
	       SDATA (display));
      else
	error ("Cannot connect to X server %s", SDATA (display));
    }

  XSETFASTINT (Vwindow_system_version, 11);
  return Qnil;
}

DEFUN ("x-close-connection", Fx_close_connection,
       Sx_close_connection, 1, 1, 0,
       doc: /* Close the connection to TERMINAL's X server.
For TERMINAL, specify a terminal object, a frame or a display name (a
string).  If TERMINAL is nil, that stands for the selected frame's terminal.
\(On MS Windows, this function does not accept terminal objects.)  */)
  (Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  if (dpyinfo->reference_count > 0)
    error ("Display still has frames on it");

  x_delete_terminal (dpyinfo->terminal);

  return Qnil;
}

DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* Return the list of display names that Emacs has connections to.  */)
  (void)
{
  Lisp_Object result = Qnil;
  struct x_display_info *xdi;

  for (xdi = x_display_list; xdi; xdi = xdi->next)
    result = Fcons (XCAR (xdi->name_list_element), result);

  return result;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize, 1, 2, 0,
       doc: /* If ON is non-nil, report X errors as soon as the erring request is made.
This function has an effect only on X Windows.  With MS Windows, it is
defined but does nothing.

If ON is nil, allow buffering of requests.
Turning on synchronization prohibits the Xlib routines from buffering
requests and seriously degrades performance, but makes debugging much
easier.
The optional second argument TERMINAL specifies which display to act on.
TERMINAL should be a terminal object, a frame or a display name (a string).
If TERMINAL is omitted or nil, that stands for the selected frame's display.  */)
  (Lisp_Object on, Lisp_Object terminal)
{
  struct x_display_info *dpyinfo = check_x_display_info (terminal);

  XSynchronize (dpyinfo->display, !NILP (on));

  return Qnil;
}

/* Wait for responses to all X commands issued so far for frame F.  */

void
x_sync (struct frame *f)
{
  block_input ();
  XSync (FRAME_X_DISPLAY (f), False);
  unblock_input ();
}


/***********************************************************************
                           Window properties
 ***********************************************************************/

DEFUN ("x-change-window-property", Fx_change_window_property,
       Sx_change_window_property, 2, 7, 0,
       doc: /* Change window property PROP to VALUE on the X window of FRAME.
PROP must be a string.  VALUE may be a string or a list of conses,
numbers and/or strings.  If an element in the list is a string, it is
converted to an atom and the value of the atom is used.  If an element
is a cons, it is converted to a 32 bit number where the car is the 16
top bits and the cdr is the lower 16 bits.

FRAME nil or omitted means use the selected frame.
If TYPE is given and non-nil, it is the name of the type of VALUE.
 If TYPE is not given or nil, the type is STRING.
FORMAT gives the size in bits of each element if VALUE is a list.
 It must be one of 8, 16 or 32.
 If VALUE is a string or FORMAT is nil or not given, FORMAT defaults to 8.
If OUTER-P is non-nil, the property is changed for the outer X window of
 FRAME.  Default is to change on the edit X window.
If WINDOW-ID is non-nil, change the property of that window instead
 of FRAME's X window; the number 0 denotes the root window.  This argument
 is separate from FRAME because window IDs are not unique across X
 displays or screens on the same display, so FRAME provides context
 for the window ID. */)
  (Lisp_Object prop, Lisp_Object value, Lisp_Object frame,
   Lisp_Object type, Lisp_Object format, Lisp_Object outer_p,
   Lisp_Object window_id)
{
  struct frame *f = decode_window_system_frame (frame);
  Atom prop_atom;
  Atom target_type = XA_STRING;
  int element_format = 8;
  unsigned char *data;
  int nelements;
  Window target_window;

  CHECK_STRING (prop);

  if (! NILP (format))
    {
      CHECK_FIXNUM (format);

      if (XFIXNUM (format) != 8 && XFIXNUM (format) != 16
          && XFIXNUM (format) != 32)
        error ("FORMAT must be one of 8, 16 or 32");
      element_format = XFIXNUM (format);
    }

  if (CONSP (value))
    {
      ptrdiff_t elsize;

      nelements = x_check_property_data (value);
      if (nelements == -1)
        error ("Bad data in VALUE, must be number, string or cons");

      /* The man page for XChangeProperty:
	     "If the specified format is 32, the property data must be a
	      long array."
	 This applies even if long is more than 32 bits.  The X library
	 converts to 32 bits before sending to the X server.  */
      elsize = element_format == 32 ? sizeof (long) : element_format >> 3;
      data = xnmalloc (nelements, elsize);

      x_fill_property_data (FRAME_X_DISPLAY (f), value, data, element_format);
    }
  else
    {
      ptrdiff_t elsize;

      CHECK_STRING (value);
      data = SDATA (value);
      if (INT_MAX < SBYTES (value))
	error ("VALUE too long");

      /* See comment above about longs and format=32 */
      elsize = element_format == 32 ? sizeof (long) : element_format >> 3;
      if (SBYTES (value) % elsize != 0)
        error ("VALUE must contain an integral number of octets for FORMAT");
      nelements = SBYTES (value) / elsize;
    }

  if (! NILP (window_id))
    {
      CONS_TO_INTEGER (window_id, Window, target_window);
      if (! target_window)
        target_window = FRAME_DISPLAY_INFO (f)->root_window;
    }
  else
    {
      if (! NILP (outer_p))
        target_window = FRAME_OUTER_WINDOW (f);
      else
        target_window = FRAME_X_WINDOW (f);
    }

  block_input ();
  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (prop), False);
  if (! NILP (type))
    {
      CHECK_STRING (type);
      target_type = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (type), False);
    }

  XChangeProperty (FRAME_X_DISPLAY (f), target_window,
		   prop_atom, target_type, element_format, PropModeReplace,
		   data, nelements);

  if (CONSP (value)) xfree (data);

  /* Make sure the property is set when we return.  */
  XFlush (FRAME_X_DISPLAY (f));
  unblock_input ();

  return value;
}


DEFUN ("x-delete-window-property", Fx_delete_window_property,
       Sx_delete_window_property, 1, 3, 0,
       doc: /* Remove window property PROP from X window of FRAME.
FRAME nil or omitted means use the selected frame.
If WINDOW-ID is non-nil, remove property from that window instead
 of FRAME's X window; the number 0 denotes the root window.  This
 argument is separate from FRAME because window IDs are not unique
 across X displays or screens on the same display, so FRAME provides
 context for the window ID.

Value is PROP.  */)
  (Lisp_Object prop, Lisp_Object frame, Lisp_Object window_id)
{
  struct frame *f = decode_window_system_frame (frame);
  Window target_window = FRAME_X_WINDOW (f);
  Atom prop_atom;

  CHECK_STRING (prop);

  if (! NILP (window_id))
    {
      CONS_TO_INTEGER (window_id, Window, target_window);
      if (! target_window)
        target_window = FRAME_DISPLAY_INFO (f)->root_window;
    }

  block_input ();
  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (prop), False);
  XDeleteProperty (FRAME_X_DISPLAY (f), target_window, prop_atom);

  /* Make sure the property is removed when we return.  */
  XFlush (FRAME_X_DISPLAY (f));
  unblock_input ();

  return prop;
}


static Lisp_Object
x_window_property_intern (struct frame *f,
                          Window target_window,
                          Atom prop_atom,
                          Atom target_type,
                          Lisp_Object delete_p,
                          Lisp_Object vector_ret_p,
                          bool *found)
{
  unsigned char *tmp_data = NULL;
  Lisp_Object prop_value = Qnil;
  Atom actual_type;
  int actual_format;
  unsigned long actual_size, bytes_remaining;
  int rc;

  rc = XGetWindowProperty (FRAME_X_DISPLAY (f), target_window,
			   prop_atom, 0, 0, False, target_type,
			   &actual_type, &actual_format, &actual_size,
			   &bytes_remaining, &tmp_data);

  *found = actual_format != 0;

  if (rc == Success && *found)
    {
      XFree (tmp_data);
      tmp_data = NULL;

      rc = XGetWindowProperty (FRAME_X_DISPLAY (f), target_window,
                               prop_atom, 0, bytes_remaining,
                               ! NILP (delete_p), target_type,
                               &actual_type, &actual_format,
                               &actual_size, &bytes_remaining,
                               &tmp_data);
      if (rc == Success && tmp_data)
        {
          /* The man page for XGetWindowProperty says:
             "If the returned format is 32, the returned data is represented
             as a long array and should be cast to that type to obtain the
             elements."
             This applies even if long is more than 32 bits, the X library
             converts from 32 bit elements received from the X server to long
             and passes the long array to us.  Thus, for that case memcpy can not
             be used.  We convert to a 32 bit type here, because so much code
             assume on that.

             The bytes and offsets passed to XGetWindowProperty refers to the
             property and those are indeed in 32 bit quantities if format is
             32.  */

          if (LONG_WIDTH > 32 && actual_format == 32)
            {
              unsigned long i;
              int  *idata = (int *) tmp_data;
              long *ldata = (long *) tmp_data;

              for (i = 0; i < actual_size; ++i)
                idata[i] = (int) ldata[i];
            }

          if (NILP (vector_ret_p))
            prop_value = make_string ((char *) tmp_data,
                                      (actual_format >> 3) * actual_size);
          else
            prop_value = x_property_data_to_lisp (f,
                                                  tmp_data,
                                                  actual_type,
                                                  actual_format,
                                                  actual_size);
        }

      if (tmp_data) XFree (tmp_data);
    }

  return prop_value;
}

DEFUN ("x-window-property", Fx_window_property, Sx_window_property,
       1, 6, 0,
       doc: /* Value is the value of window property PROP on FRAME.
If FRAME is nil or omitted, use the selected frame.

On X Windows, the following optional arguments are also accepted:
If TYPE is nil or omitted, get the property as a string.
 Otherwise TYPE is the name of the atom that denotes the expected type.
If WINDOW-ID is non-nil, get the property of that window instead of
 FRAME's X window; the number 0 denotes the root window.  This argument
 is separate from FRAME because window IDs are not unique across X
 displays or screens on the same display, so FRAME provides context
 for the window ID.
If DELETE-P is non-nil, delete the property after retrieving it.
If VECTOR-RET-P is non-nil, return a vector of values instead of a string.

Return value is nil if FRAME doesn't have a property with name PROP or
if PROP has no value of TYPE (always a string in the MS Windows case). */)
  (Lisp_Object prop, Lisp_Object frame, Lisp_Object type,
   Lisp_Object window_id, Lisp_Object delete_p, Lisp_Object vector_ret_p)
{
  struct frame *f = decode_window_system_frame (frame);
  Atom prop_atom;
  Lisp_Object prop_value = Qnil;
  Atom target_type = XA_STRING;
  Window target_window = FRAME_X_WINDOW (f);
  bool found;

  CHECK_STRING (prop);

  if (! NILP (window_id))
    {
      CONS_TO_INTEGER (window_id, Window, target_window);
      if (! target_window)
        target_window = FRAME_DISPLAY_INFO (f)->root_window;
    }

  block_input ();
  if (STRINGP (type))
    {
      if (strcmp ("AnyPropertyType", SSDATA (type)) == 0)
        target_type = AnyPropertyType;
      else
        target_type = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (type), False);
    }

  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (prop), False);
  prop_value = x_window_property_intern (f,
                                         target_window,
                                         prop_atom,
                                         target_type,
                                         delete_p,
                                         vector_ret_p,
                                         &found);
  if (NILP (prop_value)
      && ! found
      && NILP (window_id)
      && target_window != FRAME_OUTER_WINDOW (f))
    {
      prop_value = x_window_property_intern (f,
                                             FRAME_OUTER_WINDOW (f),
                                             prop_atom,
                                             target_type,
                                             delete_p,
                                             vector_ret_p,
                                             &found);
    }


  unblock_input ();
  return prop_value;
}

DEFUN ("x-window-property-attributes", Fx_window_property_attributes, Sx_window_property_attributes,
       1, 3, 0,
       doc: /* Retrieve metadata about window property PROP on FRAME.
If FRAME is nil or omitted, use the selected frame.
If WINDOW-ID is non-nil, get the property of that window instead of
 FRAME's X window; the number 0 denotes the root window.  This
 argument is separate from FRAME because window IDs are not unique
 across X displays or screens on the same display, so FRAME provides
 context for the window ID.

Return value is nil if FRAME doesn't have a property named PROP.
Otherwise, the return value is a vector with the following fields:

0. The property type, as an integer.  The symbolic name of
 the type can be obtained with `x-get-atom-name'.
1. The format of each element; one of 8, 16, or 32.
2. The length of the property, in number of elements. */)
  (Lisp_Object prop, Lisp_Object frame, Lisp_Object window_id)
{
  struct frame *f = decode_window_system_frame (frame);
  Window target_window = FRAME_X_WINDOW (f);
  Atom prop_atom;
  Lisp_Object prop_attr = Qnil;
  Atom actual_type;
  int actual_format;
  unsigned long actual_size, bytes_remaining;
  unsigned char *tmp_data = NULL;
  int rc;

  CHECK_STRING (prop);

  if (! NILP (window_id))
    {
      CONS_TO_INTEGER (window_id, Window, target_window);
      if (! target_window)
	target_window = FRAME_DISPLAY_INFO (f)->root_window;
    }

  block_input ();

  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), SSDATA (prop), False);
  rc = XGetWindowProperty (FRAME_X_DISPLAY (f), target_window,
			   prop_atom, 0, 0, False, AnyPropertyType,
			   &actual_type, &actual_format, &actual_size,
			   &bytes_remaining, &tmp_data);
  if (rc == Success          /* no invalid params */
      && actual_format == 0  /* but prop not found */
      && NILP (window_id)
      && target_window != FRAME_OUTER_WINDOW (f))
    {
      /* analogous behavior to x-window-property: if property isn't found
         on the frame's inner window and no alternate window id was
         provided, try the frame's outer window. */
      target_window = FRAME_OUTER_WINDOW (f);
      rc = XGetWindowProperty (FRAME_X_DISPLAY (f), target_window,
                               prop_atom, 0, 0, False, AnyPropertyType,
                               &actual_type, &actual_format, &actual_size,
                               &bytes_remaining, &tmp_data);
    }

  if (rc == Success && actual_format != 0)
    {
      XFree (tmp_data);

      prop_attr = make_uninit_vector (3);
      ASET (prop_attr, 0, make_fixnum (actual_type));
      ASET (prop_attr, 1, make_fixnum (actual_format));
      ASET (prop_attr, 2, make_fixnum (bytes_remaining / (actual_format >> 3)));
    }

  unblock_input ();
  return prop_attr;
}

/***********************************************************************
				Tool tips
 ***********************************************************************/

static void compute_tip_xy (struct frame *, Lisp_Object, Lisp_Object,
			    Lisp_Object, int, int, int *, int *);

/* The frame of the currently visible tooltip.  */
static Lisp_Object tip_frame;

/* The window-system window corresponding to the frame of the
   currently visible tooltip.  */
Window tip_window;

/* A timer that hides or deletes the currently visible tooltip when it
   fires.  */
static Lisp_Object tip_timer;

/* STRING argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_string;

/* Normalized FRAME argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_frame;

/* PARMS argument of last `x-show-tip' call.  */
static Lisp_Object tip_last_parms;


static void
unwind_create_tip_frame (Lisp_Object frame)
{
  Lisp_Object deleted;

  deleted = unwind_create_frame (frame);
  if (EQ (deleted, Qt))
    {
      tip_window = None;
      tip_frame = Qnil;
    }
}


/* Create a frame for a tooltip on the display described by DPYINFO.
   PARMS is a list of frame parameters.  TEXT is the string to
   display in the tip frame.  Value is the frame.

   Note that functions called here, esp. gui_default_parameter can
   signal errors, for instance when a specified color name is
   undefined.  We have to make sure that we're in a consistent state
   when this happens.  */

static Lisp_Object
x_create_tip_frame (struct x_display_info *dpyinfo, Lisp_Object parms)
{
  struct frame *f;
  Lisp_Object frame;
  Lisp_Object name;
  int width, height;
  ptrdiff_t count = SPECPDL_INDEX ();
  bool face_change_before = face_change;
  int x_width = 0, x_height = 0;

  if (!dpyinfo->terminal->name)
    error ("Terminal is not live, can't create new frames on it");

  parms = Fcopy_alist (parms);

  /* Get the name of the frame to use for resource lookup.  */
  name = gui_display_get_arg (dpyinfo, parms, Qname, "name", "Name",
                              RES_TYPE_STRING);
  if (!STRINGP (name)
      && !EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");

  frame = Qnil;
  f = make_frame (false);
  f->wants_modeline = false;
  XSETFRAME (frame, f);
  record_unwind_protect (unwind_create_tip_frame, frame);

  f->terminal = dpyinfo->terminal;

  /* By setting the output method, we're essentially saying that
     the frame is live, as per FRAME_LIVE_P.  If we get a signal
     from this point on, x_destroy_window might screw up reference
     counts etc.  */
  f->output_method = output_x_window;
  f->output_data.x = xzalloc (sizeof *f->output_data.x);
  f->output_data.x->icon_bitmap = -1;
  FRAME_FONTSET (f) = -1;
  f->output_data.x->scroll_bar_foreground_pixel = -1;
  f->output_data.x->scroll_bar_background_pixel = -1;
#if defined (USE_LUCID) && defined (USE_TOOLKIT_SCROLL_BARS)
  f->output_data.x->scroll_bar_top_shadow_pixel = -1;
  f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
#endif /* USE_LUCID && USE_TOOLKIT_SCROLL_BARS */
  f->output_data.x->white_relief.pixel = -1;
  f->output_data.x->black_relief.pixel = -1;

  f->tooltip = true;
  fset_icon_name (f, Qnil);
  FRAME_DISPLAY_INFO (f) = dpyinfo;
  f->output_data.x->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;
  f->output_data.x->explicit_parent = false;

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    FRAME_FOREGROUND_PIXEL (f) = -1;
    FRAME_BACKGROUND_PIXEL (f) = -1;
    f->output_data.x->cursor_pixel = -1;
    f->output_data.x->cursor_foreground_pixel = -1;
    f->output_data.x->border_pixel = -1;
    f->output_data.x->mouse_pixel = -1;

    black = build_string ("black");
    FRAME_FOREGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    FRAME_BACKGROUND_PIXEL (f)
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_foreground_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->mouse_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
  }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      fset_name (f, build_string (dpyinfo->x_id_name));
      f->explicit_name = false;
    }
  else
    {
      fset_name (f, name);
      f->explicit_name = true;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

#ifdef USE_CAIRO
  register_font_driver (&ftcrfont_driver, f);
#ifdef HAVE_HARFBUZZ
  register_font_driver (&ftcrhbfont_driver, f);
#endif	/* HAVE_HARFBUZZ */
#else
#ifdef HAVE_FREETYPE
#ifdef HAVE_XFT
  register_font_driver (&xftfont_driver, f);
#ifdef HAVE_HARFBUZZ
  register_font_driver (&xfthbfont_driver, f);
#endif
#else	/* not HAVE_XFT */
  register_font_driver (&ftxfont_driver, f);
#endif	/* not HAVE_XFT */
#endif	/* HAVE_FREETYPE */
#endif	/* not USE_CAIRO */
  register_font_driver (&xfont_driver, f);

  image_cache_refcount =
    FRAME_IMAGE_CACHE (f) ? FRAME_IMAGE_CACHE (f)->refcount : 0;
#ifdef GLYPH_DEBUG
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */

  gui_default_parameter (f, parms, Qfont_backend, Qnil,
                         "fontBackend", "FontBackend", RES_TYPE_STRING);

  /* Extract the window parameters from the supplied values that are
     needed to determine window geometry.  */
  x_default_font_parameter (f, parms);

  gui_default_parameter (f, parms, Qborder_width, make_fixnum (0),
                         "borderWidth", "BorderWidth", RES_TYPE_NUMBER);

  /* This defaults to 2 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = gui_display_get_arg (dpyinfo, parms, Qinternal_border_width,
                                   "internalBorder", "internalBorder",
                                   RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }

  gui_default_parameter (f, parms, Qinternal_border_width, make_fixnum (1),
                         "internalBorderWidth", "internalBorderWidth",
                         RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qright_divider_width, make_fixnum (0),
                         NULL, NULL, RES_TYPE_NUMBER);
  gui_default_parameter (f, parms, Qbottom_divider_width, make_fixnum (0),
                         NULL, NULL, RES_TYPE_NUMBER);

  /* Also do the stuff which must be set before the window exists.  */
  gui_default_parameter (f, parms, Qforeground_color, build_string ("black"),
                         "foreground", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qbackground_color, build_string ("white"),
                         "background", "Background", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qmouse_color, build_string ("black"),
                         "pointerColor", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qcursor_color, build_string ("black"),
                         "cursorColor", "Foreground", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qborder_color, build_string ("black"),
                         "borderColor", "BorderColor", RES_TYPE_STRING);
  gui_default_parameter (f, parms, Qno_special_glyphs, Qnil,
                         NULL, NULL, RES_TYPE_BOOLEAN);

  /* Init faces before gui_default_parameter is called for the
     scroll-bar-width parameter because otherwise we end up in
     init_iterator with a null face cache, which should not happen.  */
  init_frame_faces (f);

  f->output_data.x->parent_desc = FRAME_DISPLAY_INFO (f)->root_window;

  gui_default_parameter (f, parms, Qinhibit_double_buffering, Qnil,
                         "inhibitDoubleBuffering", "InhibitDoubleBuffering",
                         RES_TYPE_BOOLEAN);

  gui_figure_window_size (f, parms, false, false, &x_width, &x_height);

  {
    XSetWindowAttributes attrs;
    unsigned long mask;
    Atom type = FRAME_DISPLAY_INFO (f)->Xatom_net_window_type_tooltip;

    block_input ();
    mask = CWBackPixel | CWOverrideRedirect | CWEventMask | CWCursor;
    if (DoesSaveUnders (dpyinfo->screen))
      mask |= CWSaveUnder;

    /* Window managers look at the override-redirect flag to determine
       whether or net to give windows a decoration (Xlib spec, chapter
       3.2.8).  */
    attrs.override_redirect = True;
    attrs.save_under = True;
    attrs.background_pixel = FRAME_BACKGROUND_PIXEL (f);
    attrs.cursor =
      f->output_data.x->current_cursor
      = f->output_data.x->text_cursor;
    /* Arrange for getting MapNotify and UnmapNotify events.  */
    attrs.event_mask = StructureNotifyMask;
    tip_window
      = FRAME_X_WINDOW (f)
      = XCreateWindow (FRAME_X_DISPLAY (f),
		       FRAME_DISPLAY_INFO (f)->root_window,
		       /* x, y, width, height */
		       0, 0, 1, 1,
		       /* Border.  */
		       f->border_width,
		       CopyFromParent, InputOutput, CopyFromParent,
                       mask, &attrs);
    initial_set_up_x_back_buffer (f);
    XChangeProperty (FRAME_X_DISPLAY (f), tip_window,
                     FRAME_DISPLAY_INFO (f)->Xatom_net_window_type,
                     XA_ATOM, 32, PropModeReplace,
                     (unsigned char *)&type, 1);
    unblock_input ();
  }

  x_make_gc (f);

  gui_default_parameter (f, parms, Qauto_raise, Qnil,
                         "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qauto_lower, Qnil,
                         "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  gui_default_parameter (f, parms, Qcursor_type, Qbox,
                         "cursorType", "CursorType", RES_TYPE_SYMBOL);
  gui_default_parameter (f, parms, Qalpha, Qnil,
                         "alpha", "Alpha", RES_TYPE_NUMBER);

  /* Dimensions, especially FRAME_LINES (f), must be done via change_frame_size.
     Change will not be effected unless different from the current
     FRAME_LINES (f).  */
  width = FRAME_COLS (f);
  height = FRAME_LINES (f);
  SET_FRAME_COLS (f, 0);
  SET_FRAME_LINES (f, 0);
  change_frame_size (f, width, height, true, false, false, false);

  /* Add `tooltip' frame parameter's default value. */
  if (NILP (Fframe_parameter (frame, Qtooltip)))
    {
      AUTO_FRAME_ARG (arg, Qtooltip, Qt);
      Fmodify_frame_parameters (frame, arg);
    }

  /* FIXME - can this be done in a similar way to normal frames?
     https://lists.gnu.org/r/emacs-devel/2007-10/msg00641.html */

  /* Set the `display-type' frame parameter before setting up faces. */
  {
    Lisp_Object disptype;

    if (FRAME_DISPLAY_INFO (f)->n_planes == 1)
      disptype = Qmono;
    else if (FRAME_DISPLAY_INFO (f)->visual->class == GrayScale
             || FRAME_DISPLAY_INFO (f)->visual->class == StaticGray)
      disptype = intern ("grayscale");
    else
      disptype = intern ("color");

    if (NILP (Fframe_parameter (frame, Qdisplay_type)))
      {
	AUTO_FRAME_ARG (arg, Qdisplay_type, disptype);
	Fmodify_frame_parameters (frame, arg);
      }
  }

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.

     Frame parameters may be changed if .Xdefaults contains
     specifications for the default font.  For example, if there is an
     `Emacs.default.attributeBackground: pink', the `background-color'
     attribute of the frame get's set, which let's the internal border
     of the tooltip frame appear in pink.  Prevent this.  */
  {
    Lisp_Object bg = Fframe_parameter (frame, Qbackground_color);

    call2 (Qface_set_after_frame_default, frame, Qnil);

    if (!EQ (bg, Fframe_parameter (frame, Qbackground_color)))
      {
	AUTO_FRAME_ARG (arg, Qbackground_color, bg);
	Fmodify_frame_parameters (frame, arg);
      }
  }

  f->no_split = true;

  /* Now that the frame will be official, it counts as a reference to
     its display and terminal.  */
  FRAME_DISPLAY_INFO (f)->reference_count++;
  f->terminal->reference_count++;

  /* It is now ok to make the frame official even if we get an error
     below.  And the frame needs to be on Vframe_list or making it
     visible won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);
  f->can_set_window_size = true;

  /* Setting attributes of faces of the tooltip frame from resources
     and similar will set face_change, which leads to the clearing of
     all current matrices.  Since this isn't necessary here, avoid it
     by resetting face_change to the value it had before we created
     the tip frame.  */
  face_change = face_change_before;

  /* Discard the unwind_protect.  */
  return unbind_to (count, frame);
}


/* Compute where to display tip frame F.  PARMS is the list of frame
   parameters for F.  DX and DY are specified offsets from the current
   location of the mouse.  WIDTH and HEIGHT are the width and height
   of the tooltip.  Return coordinates relative to the root window of
   the display in *ROOT_X, and *ROOT_Y.  */

static void
compute_tip_xy (struct frame *f,
		Lisp_Object parms, Lisp_Object dx, Lisp_Object dy,
		int width, int height, int *root_x, int *root_y)
{
  Lisp_Object left, top, right, bottom;
  int win_x, win_y;
  Window root, child;
  unsigned pmask;
  int min_x, min_y, max_x, max_y = -1;

  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));
  right = Fcdr (Fassq (Qright, parms));
  bottom = Fcdr (Fassq (Qbottom, parms));

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if ((!FIXNUMP (left) && !FIXNUMP (right))
      || (!FIXNUMP (top) && !FIXNUMP (bottom)))
    {
      Lisp_Object frame, attributes, monitor, geometry;

      block_input ();
      XQueryPointer (FRAME_X_DISPLAY (f), FRAME_DISPLAY_INFO (f)->root_window,
		     &root, &child, root_x, root_y, &win_x, &win_y, &pmask);
      unblock_input ();

      XSETFRAME(frame, f);
      attributes = Fx_display_monitor_attributes_list (frame);

      /* Try to determine the monitor where the mouse pointer is and
         its geometry.  See bug#22549.  */
      while (CONSP (attributes))
	{
          monitor = XCAR (attributes);
          geometry = Fassq (Qgeometry, monitor);
          if (CONSP (geometry))
            {
              min_x = XFIXNUM (Fnth (make_fixnum (1), geometry));
              min_y = XFIXNUM (Fnth (make_fixnum (2), geometry));
              max_x = min_x + XFIXNUM (Fnth (make_fixnum (3), geometry));
              max_y = min_y + XFIXNUM (Fnth (make_fixnum (4), geometry));
              if (min_x <= *root_x && *root_x < max_x
                  && min_y <= *root_y && *root_y < max_y)
                {
                  break;
                }
              max_y = -1;
            }

          attributes = XCDR (attributes);
	}
    }

  /* It was not possible to determine the monitor's geometry, so we
     assign some sane defaults here: */
  if ( max_y < 0 )
    {
      min_x = 0;
      min_y = 0;
      max_x = x_display_pixel_width (FRAME_DISPLAY_INFO (f));
      max_y = x_display_pixel_height (FRAME_DISPLAY_INFO (f));
    }

  if (FIXNUMP (top))
    *root_y = XFIXNUM (top);
  else if (FIXNUMP (bottom))
    *root_y = XFIXNUM (bottom) - height;
  else if (*root_y + XFIXNUM (dy) <= min_y)
    *root_y = min_y; /* Can happen for negative dy */
  else if (*root_y + XFIXNUM (dy) + height <= max_y)
    /* It fits below the pointer */
    *root_y += XFIXNUM (dy);
  else if (height + XFIXNUM (dy) + min_y <= *root_y)
    /* It fits above the pointer.  */
    *root_y -= height + XFIXNUM (dy);
  else
    /* Put it on the top.  */
    *root_y = min_y;

  if (FIXNUMP (left))
    *root_x = XFIXNUM (left);
  else if (FIXNUMP (right))
    *root_x = XFIXNUM (right) - width;
  else if (*root_x + XFIXNUM (dx) <= min_x)
    *root_x = 0; /* Can happen for negative dx */
  else if (*root_x + XFIXNUM (dx) + width <= max_x)
    /* It fits to the right of the pointer.  */
    *root_x += XFIXNUM (dx);
  else if (width + XFIXNUM (dx) + min_x <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XFIXNUM (dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = min_x;
}


/**
 * x_hide_tip:
 *
 * Hide currently visible tooltip and cancel its timer.
 *
 * If GTK+ system tooltips are used, this will try to hide the tooltip
 * referenced by the x_output structure of tooltip_last_frame.  For
 * Emacs tooltips this will try to make tooltip_frame invisible (if
 * DELETE is false) or delete tooltip_frame (if DELETE is true).
 *
 * Return Qt if the tooltip was either deleted or made invisible, Qnil
 * otherwise.
 */
static Lisp_Object
x_hide_tip (bool delete)
{
  if (!NILP (tip_timer))
    {
      call1 (Qcancel_timer, tip_timer);
      tip_timer = Qnil;
    }

#ifdef USE_GTK
  /* Any GTK+ system tooltip can be found via the x_output structure of
     tip_last_frame, provided that frame is still live.  Any Emacs
     tooltip is found via the tip_frame variable.  Note that the current
     value of x_gtk_use_system_tooltips might not be the same as used
     for the tooltip we have to hide, see Bug#30399.  */
  if ((NILP (tip_last_frame) && NILP (tip_frame))
      || (!x_gtk_use_system_tooltips
	  && !delete
	  && FRAMEP (tip_frame)
	  && FRAME_LIVE_P (XFRAME (tip_frame))
	  && !FRAME_VISIBLE_P (XFRAME (tip_frame))))
    /* Either there's no tooltip to hide or it's an already invisible
       Emacs tooltip and we don't want to change its type.  Return
       quickly.  */
    return Qnil;
  else
    {
      ptrdiff_t count;
      Lisp_Object was_open = Qnil;

      count = SPECPDL_INDEX ();
      specbind (Qinhibit_redisplay, Qt);
      specbind (Qinhibit_quit, Qt);

      /* Try to hide the GTK+ system tip first.  */
      if (FRAMEP (tip_last_frame))
	{
	  struct frame *f = XFRAME (tip_last_frame);

	  if (FRAME_LIVE_P (f))
	    {
	      if (xg_hide_tooltip (f))
		was_open = Qt;
	    }
	}

      /* When using GTK+ system tooltips (compare Bug#41200) reset
	 tip_last_frame.  It will be reassigned when showing the next
	 GTK+ system tooltip.  */
      if (x_gtk_use_system_tooltips)
	tip_last_frame = Qnil;

      /* Now look whether there's an Emacs tip around.  */
      if (FRAMEP (tip_frame))
	{
	  struct frame *f = XFRAME (tip_frame);

	  if (FRAME_LIVE_P (f))
	    {
	      if (delete || x_gtk_use_system_tooltips)
		{
		  /* Delete the Emacs tooltip frame when DELETE is true
		     or we change the tooltip type from an Emacs one to
		     a GTK+ system one.  */
		  delete_frame (tip_frame, Qnil);
		  tip_frame = Qnil;
		}
	      else
		x_make_frame_invisible (f);

	      was_open = Qt;
	    }
	  else
	    tip_frame = Qnil;
	}
      else
	tip_frame = Qnil;

      return unbind_to (count, was_open);
    }
#else /* not USE_GTK */
  if (NILP (tip_frame)
      || (!delete
	  && FRAMEP (tip_frame)
	  && FRAME_LIVE_P (XFRAME (tip_frame))
	  && !FRAME_VISIBLE_P (XFRAME (tip_frame))))
    return Qnil;
  else
    {
      ptrdiff_t count;
      Lisp_Object was_open = Qnil;

      count = SPECPDL_INDEX ();
      specbind (Qinhibit_redisplay, Qt);
      specbind (Qinhibit_quit, Qt);

      if (FRAMEP (tip_frame))
	{
	  struct frame *f = XFRAME (tip_frame);

	  if (FRAME_LIVE_P (f))
	    {
	      if (delete)
		{
		  delete_frame (tip_frame, Qnil);
		  tip_frame = Qnil;
		}
	      else
		x_make_frame_invisible (XFRAME (tip_frame));

#ifdef USE_LUCID
	      /* Bloodcurdling hack alert: The Lucid menu bar widget's
		 redisplay procedure is not called when a tip frame over
		 menu items is unmapped.  Redisplay the menu manually...  */
	      {
		Widget w;
		struct frame *f = SELECTED_FRAME ();

		if (FRAME_X_P (f) && FRAME_LIVE_P (f))
		  {
		    w = f->output_data.x->menubar_widget;

		    if (!DoesSaveUnders (FRAME_DISPLAY_INFO (f)->screen)
			&& w != NULL)
		      {
			block_input ();
			xlwmenu_redisplay (w);
			unblock_input ();
		      }
		  }
	      }
#endif /* USE_LUCID */

	      was_open = Qt;
	    }
	  else
	    tip_frame = Qnil;
	}
      else
	tip_frame = Qnil;

      return unbind_to (count, was_open);
    }
#endif /* USE_GTK */
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a "tooltip" window on frame FRAME.
A tooltip window is a small X window displaying a string.

This is an internal function; Lisp code should call `tooltip-show'.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be used to
change the tooltip's appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout of 5 seconds.

If the list of frame parameters PARMS contains a `left' parameter,
display the tooltip at that x-position.  If the list of frame parameters
PARMS contains no `left' but a `right' parameter, display the tooltip
right-adjusted at that x-position. Otherwise display it at the
x-position of the mouse, with offset DX added (default is 5 if DX isn't
specified).

Likewise for the y-position: If a `top' frame parameter is specified, it
determines the position of the upper edge of the tooltip window.  If a
`bottom' parameter but no `top' frame parameter is specified, it
determines the position of the lower edge of the tooltip window.
Otherwise display the tooltip window at the y-position of the mouse,
with offset DY added (default is -10).

A tooltip's maximum size is specified by `x-max-tooltip-size'.
Text larger than the specified size is clipped.  */)
  (Lisp_Object string, Lisp_Object frame, Lisp_Object parms,
   Lisp_Object timeout, Lisp_Object dx, Lisp_Object dy)
{
  struct frame *f, *tip_f;
  struct window *w;
  int root_x, root_y;
  struct buffer *old_buffer;
  struct text_pos pos;
  int width, height;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  ptrdiff_t count = SPECPDL_INDEX ();
  ptrdiff_t count_1;
  Lisp_Object window, size, tip_buf;
  AUTO_STRING (tip, " *tip*");

  specbind (Qinhibit_redisplay, Qt);

  CHECK_STRING (string);
  if (SCHARS (string) == 0)
    string = make_unibyte_string (" ", 1);

  if (NILP (frame))
    frame = selected_frame;
  f = decode_window_system_frame (frame);

  if (NILP (timeout))
    timeout = make_fixnum (5);
  else
    CHECK_FIXNAT (timeout);

  if (NILP (dx))
    dx = make_fixnum (5);
  else
    CHECK_FIXNUM (dx);

  if (NILP (dy))
    dy = make_fixnum (-10);
  else
    CHECK_FIXNUM (dy);

#ifdef USE_GTK
  if (x_gtk_use_system_tooltips)
    {
      bool ok;

      /* Hide a previous tip, if any.  */
      Fx_hide_tip ();

      block_input ();
      ok = xg_prepare_tooltip (f, string, &width, &height);
      if (ok)
        {
	  compute_tip_xy (f, parms, dx, dy, width, height, &root_x, &root_y);
          xg_show_tooltip (f, root_x, root_y);
	  tip_last_frame = frame;
        }

      unblock_input ();
      if (ok) goto start_timer;
    }
#endif /* USE_GTK */

  if (FRAMEP (tip_frame) && FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      if (FRAME_VISIBLE_P (XFRAME (tip_frame))
	  && EQ (frame, tip_last_frame)
	  && !NILP (Fequal_including_properties (tip_last_string, string))
	  && !NILP (Fequal (tip_last_parms, parms)))
	{
	  /* Only DX and DY have changed.  */
	  tip_f = XFRAME (tip_frame);
	  if (!NILP (tip_timer))
	    {
	      call1 (Qcancel_timer, tip_timer);
	      tip_timer = Qnil;
	    }

	  block_input ();
	  compute_tip_xy (tip_f, parms, dx, dy, FRAME_PIXEL_WIDTH (tip_f),
			  FRAME_PIXEL_HEIGHT (tip_f), &root_x, &root_y);
	  XMoveWindow (FRAME_X_DISPLAY (tip_f), FRAME_X_WINDOW (tip_f),
		       root_x, root_y);
	  unblock_input ();

	  goto start_timer;
	}
      else if (tooltip_reuse_hidden_frame && EQ (frame, tip_last_frame))
	{
	  bool delete = false;
	  Lisp_Object tail, elt, parm, last;

	  /* Check if every parameter in PARMS has the same value in
	     tip_last_parms.  This may destruct tip_last_parms which,
	     however, will be recreated below.  */
	  for (tail = parms; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      parm = Fcar (elt);
	      /* The left, top, right and bottom parameters are handled
		 by compute_tip_xy so they can be ignored here.  */
	      if (!EQ (parm, Qleft) && !EQ (parm, Qtop)
		  && !EQ (parm, Qright) && !EQ (parm, Qbottom))
		{
		  last = Fassq (parm, tip_last_parms);
		  if (NILP (Fequal (Fcdr (elt), Fcdr (last))))
		    {
		      /* We lost, delete the old tooltip.  */
		      delete = true;
		      break;
		    }
		  else
		    tip_last_parms =
		      call2 (Qassq_delete_all, parm, tip_last_parms);
		}
	      else
		tip_last_parms =
		  call2 (Qassq_delete_all, parm, tip_last_parms);
	    }

	  /* Now check if every parameter in what is left of
	     tip_last_parms with a non-nil value has an association in
	     PARMS.  */
	  for (tail = tip_last_parms; CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      parm = Fcar (elt);
	      if (!EQ (parm, Qleft) && !EQ (parm, Qtop) && !EQ (parm, Qright)
		  && !EQ (parm, Qbottom) && !NILP (Fcdr (elt)))
		{
		  /* We lost, delete the old tooltip.  */
		  delete = true;
		  break;
		}
	    }

	  x_hide_tip (delete);
	}
      else
	x_hide_tip (true);
    }
  else
    x_hide_tip (true);

  tip_last_frame = frame;
  tip_last_string = string;
  tip_last_parms = parms;

  if (!FRAMEP (tip_frame) || !FRAME_LIVE_P (XFRAME (tip_frame)))
    {
      /* Add default values to frame parameters.  */
      if (NILP (Fassq (Qname, parms)))
	parms = Fcons (Fcons (Qname, build_string ("tooltip")), parms);
      if (NILP (Fassq (Qinternal_border_width, parms)))
	parms = Fcons (Fcons (Qinternal_border_width, make_fixnum (3)), parms);
      if (NILP (Fassq (Qborder_width, parms)))
	parms = Fcons (Fcons (Qborder_width, make_fixnum (1)), parms);
      if (NILP (Fassq (Qborder_color, parms)))
	parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")), parms);
      if (NILP (Fassq (Qbackground_color, parms)))
	parms = Fcons (Fcons (Qbackground_color, build_string ("lightyellow")),
		       parms);

      /* Create a frame for the tooltip, and record it in the global
	 variable tip_frame.  */
      if (NILP (tip_frame = x_create_tip_frame (FRAME_DISPLAY_INFO (f), parms)))
	/* Creating the tip frame failed.  */
	return unbind_to (count, Qnil);
    }

  tip_f = XFRAME (tip_frame);
  window = FRAME_ROOT_WINDOW (tip_f);
  tip_buf = Fget_buffer_create (tip);
  /* We will mark the tip window a "pseudo-window" below, and such
     windows cannot have display margins.  */
  bset_left_margin_cols (XBUFFER (tip_buf), make_fixnum (0));
  bset_right_margin_cols (XBUFFER (tip_buf), make_fixnum (0));
  set_window_buffer (window, tip_buf, false, false);
  w = XWINDOW (window);
  w->pseudo_window_p = true;

  /* Set up the frame's root window.  Note: The following code does not
     try to size the window or its frame correctly.  Its only purpose is
     to make the subsequent text size calculations work.  The right
     sizes should get installed when the toolkit gets back to us.  */
  w->left_col = 0;
  w->top_line = 0;
  w->pixel_left = 0;
  w->pixel_top = 0;

  if (CONSP (Vx_max_tooltip_size)
      && RANGED_FIXNUMP (1, XCAR (Vx_max_tooltip_size), INT_MAX)
      && RANGED_FIXNUMP (1, XCDR (Vx_max_tooltip_size), INT_MAX))
    {
      w->total_cols = XFIXNAT (XCAR (Vx_max_tooltip_size));
      w->total_lines = XFIXNAT (XCDR (Vx_max_tooltip_size));
    }
  else
    {
      w->total_cols = 80;
      w->total_lines = 40;
    }

  w->pixel_width = w->total_cols * FRAME_COLUMN_WIDTH (tip_f);
  w->pixel_height = w->total_lines * FRAME_LINE_HEIGHT (tip_f);
  FRAME_TOTAL_COLS (tip_f) = w->total_cols;
  adjust_frame_glyphs (tip_f);

  /* Insert STRING into root window's buffer and fit the frame to the
     buffer.  */
  count_1 = SPECPDL_INDEX ();
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (w->contents));
  bset_truncate_lines (current_buffer, Qnil);
  specbind (Qinhibit_read_only, Qt);
  specbind (Qinhibit_modification_hooks, Qt);
  specbind (Qinhibit_point_motion_hooks, Qt);
  Ferase_buffer ();
  Finsert (1, &string);
  clear_glyph_matrix (w->desired_matrix);
  clear_glyph_matrix (w->current_matrix);
  SET_TEXT_POS (pos, BEGV, BEGV_BYTE);
  try_window (window, pos, TRY_WINDOW_IGNORE_FONTS_CHANGE);
  /* Calculate size of tooltip window.  */
  size = Fwindow_text_pixel_size (window, Qnil, Qnil, Qnil,
				  make_fixnum (w->pixel_height), Qnil);
  /* Add the frame's internal border to calculated size.  */
  width = XFIXNUM (Fcar (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);
  height = XFIXNUM (Fcdr (size)) + 2 * FRAME_INTERNAL_BORDER_WIDTH (tip_f);

  /* Calculate position of tooltip frame.  */
  compute_tip_xy (tip_f, parms, dx, dy, width, height, &root_x, &root_y);

  /* Show tooltip frame.  */
  block_input ();
  XMoveResizeWindow (FRAME_X_DISPLAY (tip_f), FRAME_X_WINDOW (tip_f),
		     root_x, root_y, width, height);
  XMapRaised (FRAME_X_DISPLAY (tip_f), FRAME_X_WINDOW (tip_f));
  unblock_input ();

#ifdef USE_CAIRO
  x_cr_update_surface_desired_size (tip_f, width, height);
#endif	/* USE_CAIRO */

  w->must_be_updated_p = true;
  update_single_window (w);
  flush_frame (tip_f);
  set_buffer_internal_1 (old_buffer);
  unbind_to (count_1, Qnil);
  windows_or_buffers_changed = old_windows_or_buffers_changed;

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
		     intern ("x-hide-tip"));

  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.  */)
  (void)
{
  return x_hide_tip (!tooltip_reuse_hidden_frame);
}

DEFUN ("x-double-buffered-p", Fx_double_buffered_p, Sx_double_buffered_p,
       0, 1, 0,
       doc: /* Return t if FRAME is being double buffered.  */)
     (Lisp_Object frame)
{
  struct frame *f = decode_live_frame (frame);
  return FRAME_X_DOUBLE_BUFFERED_P (f) ? Qt : Qnil;
}


/***********************************************************************
			File selection dialog
 ***********************************************************************/

DEFUN ("x-uses-old-gtk-dialog", Fx_uses_old_gtk_dialog,
       Sx_uses_old_gtk_dialog,
       0, 0, 0,
       doc: /* Return t if the old Gtk+ file selection dialog is used.  */)
  (void)
{
#ifdef USE_GTK
  if (use_dialog_box
      && use_file_dialog
      && window_system_available (SELECTED_FRAME ())
      && xg_uses_old_file_dialog ())
    return Qt;
#endif
  return Qnil;
}


#ifdef USE_MOTIF
/* Callback for "OK" and "Cancel" on file selection dialog.  */

static void
file_dialog_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
  int *result = client_data;
  XmAnyCallbackStruct *cb = call_data;
  *result = cb->reason;
}


/* Callback for unmapping a file selection dialog.  This is used to
   capture the case where a dialog is closed via a window manager's
   closer button, for example. Using a XmNdestroyCallback didn't work
   in this case.  */

static void
file_dialog_unmap_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
  int *result = client_data;
  *result = XmCR_CANCEL;
}

static void
clean_up_file_dialog (void *arg)
{
  Widget dialog = arg;

  /* Clean up.  */
  block_input ();
  XtUnmanageChild (dialog);
  XtDestroyWidget (dialog);
  x_menu_set_in_use (false);
  unblock_input ();
}


DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 5, 0,
       doc: /* SKIP: real doc in USE_GTK definition in xfns.c.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object default_filename,
   Lisp_Object mustmatch, Lisp_Object only_dir_p)
{
  int result;
  struct frame *f = SELECTED_FRAME ();
  Lisp_Object file = Qnil;
  Lisp_Object decoded_file;
  Widget dialog, text, help;
  Arg al[10];
  int ac = 0;
  XmString dir_xmstring, pattern_xmstring;
  ptrdiff_t count = SPECPDL_INDEX ();

  check_window_system (f);

  if (popup_activated ())
    error ("Trying to use a menu from within a menu-entry");

  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);

  block_input ();

  /* Create the dialog with PROMPT as title, using DIR as initial
     directory and using "*" as pattern.  */
  dir = Fexpand_file_name (dir, Qnil);
  dir_xmstring = XmStringCreateLocalized (SSDATA (dir));
  pattern_xmstring = XmStringCreateLocalized ("*");

  XtSetArg (al[ac], XmNtitle, SDATA (prompt)); ++ac;
  XtSetArg (al[ac], XmNdirectory, dir_xmstring); ++ac;
  XtSetArg (al[ac], XmNpattern, pattern_xmstring); ++ac;
  XtSetArg (al[ac], XmNresizePolicy, XmRESIZE_GROW); ++ac;
  XtSetArg (al[ac], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL); ++ac;
  dialog = XmCreateFileSelectionDialog (f->output_data.x->widget,
					"fsb", al, ac);
  XmStringFree (dir_xmstring);
  XmStringFree (pattern_xmstring);

  /* Add callbacks for OK and Cancel.  */
  XtAddCallback (dialog, XmNokCallback, file_dialog_cb,
		 (XtPointer) &result);
  XtAddCallback (dialog, XmNcancelCallback, file_dialog_cb,
		 (XtPointer) &result);
  XtAddCallback (dialog, XmNunmapCallback, file_dialog_unmap_cb,
		 (XtPointer) &result);

  /* Remove the help button since we can't display help.  */
  help = XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON);
  XtUnmanageChild (help);

  /* Mark OK button as default.  */
  XtVaSetValues (XmFileSelectionBoxGetChild (dialog, XmDIALOG_OK_BUTTON),
		 XmNshowAsDefault, True, NULL);

  /* If MUSTMATCH is non-nil, disable the file entry field of the
     dialog, so that the user must select a file from the files list
     box.  We can't remove it because we wouldn't have a way to get at
     the result file name, then.  */
  text = XmFileSelectionBoxGetChild (dialog, XmDIALOG_TEXT);
  if (!NILP (mustmatch))
    {
      Widget label;
      label = XmFileSelectionBoxGetChild (dialog, XmDIALOG_SELECTION_LABEL);
      XtSetSensitive (text, False);
      XtSetSensitive (label, False);
    }

  /* Manage the dialog, so that list boxes get filled.  */
  XtManageChild (dialog);

  if (STRINGP (default_filename))
    {
      XmString default_xmstring;
      Widget wtext = XmFileSelectionBoxGetChild (dialog, XmDIALOG_TEXT);
      Widget list = XmFileSelectionBoxGetChild (dialog, XmDIALOG_LIST);

      XmTextPosition last_pos = XmTextFieldGetLastPosition (wtext);
      XmTextFieldReplace (wtext, 0, last_pos,
                          (SSDATA (Ffile_name_nondirectory (default_filename))));

      /* Select DEFAULT_FILENAME in the files list box.  DEFAULT_FILENAME
         must include the path for this to work.  */

      default_xmstring = XmStringCreateLocalized (SSDATA (default_filename));

      if (XmListItemExists (list, default_xmstring))
        {
          int item_pos = XmListItemPos (list, default_xmstring);
          /* Select the item and scroll it into view.  */
          XmListSelectPos (list, item_pos, True);
          XmListSetPos (list, item_pos);
        }

      XmStringFree (default_xmstring);
    }

  record_unwind_protect_ptr (clean_up_file_dialog, dialog);

  /* Process events until the user presses Cancel or OK.  */
  x_menu_set_in_use (true);
  result = 0;
  while (result == 0)
    {
      XEvent event;
      x_menu_wait_for_event (0);
      XtAppNextEvent (Xt_app_con, &event);
      if (event.type == KeyPress
          && FRAME_X_DISPLAY (f) == event.xkey.display)
        {
          KeySym keysym = XLookupKeysym (&event.xkey, 0);

          /* Pop down on C-g.  */
          if (keysym == XK_g && (event.xkey.state & ControlMask) != 0)
            XtUnmanageChild (dialog);
        }

      (void) x_dispatch_event (&event, FRAME_X_DISPLAY (f));
    }

  /* Get the result.  */
  if (result == XmCR_OK)
    {
      XmString text_string;
      String data;

      XtVaGetValues (dialog, XmNtextString, &text_string, NULL);
      XmStringGetLtoR (text_string, XmFONTLIST_DEFAULT_TAG, &data);
      XmStringFree (text_string);
      file = build_string (data);
      XtFree (data);
    }
  else
    file = Qnil;

  unblock_input ();

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    quit ();

  decoded_file = DECODE_FILE (file);

  return unbind_to (count, decoded_file);
}

#endif /* USE_MOTIF */

#ifdef USE_GTK

static void
clean_up_dialog (void)
{
  x_menu_set_in_use (false);
}

DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 5, 0,
       doc: /* Read file name, prompting with PROMPT in directory DIR.
Use a file selection dialog.  Select DEFAULT-FILENAME in the dialog's file
selection box, if specified.  If MUSTMATCH is non-nil, the returned file
or directory must exist.

This function is defined only on NS, MS Windows, and X Windows with the
Motif or Gtk toolkits.  With the Motif toolkit, ONLY-DIR-P is ignored.
Otherwise, if ONLY-DIR-P is non-nil, the user can select only directories.
On MS Windows 7 and later, the file selection dialog "remembers" the last
directory where the user selected a file, and will open that directory
instead of DIR on subsequent invocations of this function with the same
value of DIR as in previous invocations; this is standard MS Windows behavior.  */)
  (Lisp_Object prompt, Lisp_Object dir, Lisp_Object default_filename, Lisp_Object mustmatch, Lisp_Object only_dir_p)
{
  struct frame *f = SELECTED_FRAME ();
  char *fn;
  Lisp_Object file = Qnil;
  Lisp_Object decoded_file;
  ptrdiff_t count = SPECPDL_INDEX ();
  char *cdef_file;

  check_window_system (f);

  if (popup_activated ())
    error ("Trying to use a menu from within a menu-entry");
  else
    x_menu_set_in_use (true);

  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);
  record_unwind_protect_void (clean_up_dialog);

  block_input ();

  if (STRINGP (default_filename))
    cdef_file = SSDATA (default_filename);
  else
    cdef_file = SSDATA (dir);

  fn = xg_get_file_name (f, SSDATA (prompt), cdef_file,
                         ! NILP (mustmatch),
                         ! NILP (only_dir_p));

  if (fn)
    {
      file = build_string (fn);
      xfree (fn);
    }

  unblock_input ();

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    quit ();

  decoded_file = DECODE_FILE (file);

  return unbind_to (count, decoded_file);
}


#ifdef HAVE_FREETYPE

DEFUN ("x-select-font", Fx_select_font, Sx_select_font, 0, 2, 0,
       doc: /* Read a font using a GTK dialog.
Return either a font spec (for GTK versions >= 3.2) or a string
containing a GTK-style font name.

FRAME is the frame on which to pop up the font chooser.  If omitted or
nil, it defaults to the selected frame. */)
  (Lisp_Object frame, Lisp_Object ignored)
{
  struct frame *f = decode_window_system_frame (frame);
  Lisp_Object font;
  Lisp_Object font_param;
  char *default_name = NULL;
  ptrdiff_t count = SPECPDL_INDEX ();

  if (popup_activated ())
    error ("Trying to use a menu from within a menu-entry");
  else
    x_menu_set_in_use (true);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);
  record_unwind_protect_void (clean_up_dialog);

  block_input ();

  XSETFONT (font, FRAME_FONT (f));
  font_param = Ffont_get (font, QCname);
  if (STRINGP (font_param))
    default_name = xlispstrdup (font_param);
  else
    {
      font_param = Fframe_parameter (frame, Qfont_parameter);
      if (STRINGP (font_param))
        default_name = xlispstrdup (font_param);
    }

  font = xg_get_font (f, default_name);
  xfree (default_name);

  unblock_input ();

  if (NILP (font))
    quit ();

  return unbind_to (count, font);
}
#endif /* HAVE_FREETYPE */

#endif /* USE_GTK */


/***********************************************************************
			       Keyboard
 ***********************************************************************/

#ifdef HAVE_XKB
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#endif

DEFUN ("x-backspace-delete-keys-p", Fx_backspace_delete_keys_p,
       Sx_backspace_delete_keys_p, 0, 1, 0,
       doc: /* Check if both Backspace and Delete keys are on the keyboard of FRAME.
FRAME nil means use the selected frame.
Value is t if we know that both keys are present, and are mapped to the
usual X keysyms.  Value is `lambda' if we cannot determine if both keys are
present and mapped to the usual X keysyms.  */)
  (Lisp_Object frame)
{
#ifndef HAVE_XKB
  return Qlambda;
#else
  XkbDescPtr kb;
  struct frame *f = decode_window_system_frame (frame);
  Display *dpy = FRAME_X_DISPLAY (f);
  Lisp_Object have_keys;
  int major, minor, op, event, error_code;

  block_input ();

  /* Check library version in case we're dynamically linked.  */
  major = XkbMajorVersion;
  minor = XkbMinorVersion;
  if (!XkbLibraryVersion (&major, &minor))
    {
      unblock_input ();
      return Qlambda;
    }

  /* Check that the server supports XKB.  */
  major = XkbMajorVersion;
  minor = XkbMinorVersion;
  if (!XkbQueryExtension (dpy, &op, &event, &error_code, &major, &minor))
    {
      unblock_input ();
      return Qlambda;
    }

  /* In this code we check that the keyboard has physical keys with names
     that start with BKSP (Backspace) and DELE (Delete), and that they
     generate keysym XK_BackSpace and XK_Delete respectively.
     This function is used to test if normal-erase-is-backspace should be
     turned on.
     An alternative approach would be to just check if XK_BackSpace and
     XK_Delete are mapped to any key.  But if any of those are mapped to
     some non-intuitive key combination (Meta-Shift-Ctrl-whatever) and the
     user doesn't know about it, it is better to return false here.
     It is more obvious to the user what to do if she/he has two keys
     clearly marked with names/symbols and one key does something not
     expected (i.e. she/he then tries the other).
     The cases where Backspace/Delete is mapped to some other key combination
     are rare, and in those cases, normal-erase-is-backspace can be turned on
     manually.  */

  have_keys = Qnil;
  kb = XkbGetMap (dpy, XkbAllMapComponentsMask, XkbUseCoreKbd);
  if (kb)
    {
      int delete_keycode = 0, backspace_keycode = 0, i;

      if (XkbGetNames (dpy, XkbAllNamesMask, kb) == Success)
	{
	  for (i = kb->min_key_code;
	       (i < kb->max_key_code
		&& (delete_keycode == 0 || backspace_keycode == 0));
	       ++i)
	    {
	      /* The XKB symbolic key names can be seen most easily in
		 the PS file generated by `xkbprint -label name
		 $DISPLAY'.  */
	      if (memcmp ("DELE", kb->names->keys[i].name, 4) == 0)
		delete_keycode = i;
	      else if (memcmp ("BKSP", kb->names->keys[i].name, 4) == 0)
		backspace_keycode = i;
	    }

	  XkbFreeNames (kb, 0, True);
	}

      /* As of libX11-1.6.2, XkbGetMap manual says that you should use
	 XkbFreeClientMap to free the data returned by XkbGetMap.  But
	 this function just frees the data referenced from KB and not
	 KB itself.  To free KB as well, call XkbFreeKeyboard.  */
      XkbFreeKeyboard (kb, XkbAllMapComponentsMask, True);

      if (delete_keycode
	  && backspace_keycode
	  && XKeysymToKeycode (dpy, XK_Delete) == delete_keycode
	  && XKeysymToKeycode (dpy, XK_BackSpace) == backspace_keycode)
	have_keys = Qt;
    }
  unblock_input ();
  return have_keys;
#endif
}



/***********************************************************************
			       Printing
 ***********************************************************************/

#ifdef USE_CAIRO
DEFUN ("x-export-frames", Fx_export_frames, Sx_export_frames, 0, 2, 0,
       doc: /* Return image data of FRAMES in TYPE format.
FRAMES should be nil (the selected frame), a frame, or a list of
frames (each of which corresponds to one page).  Each frame should be
visible.  Optional arg TYPE should be either `pdf' (default), `png',
`postscript', or `svg'.  Supported types are determined by the
compile-time configuration of cairo.

Note: Text drawn with the `x' font backend is shown with hollow boxes
unless TYPE is `png'.  */)
     (Lisp_Object frames, Lisp_Object type)
{
  Lisp_Object rest, tmp;
  cairo_surface_type_t surface_type;

  if (!CONSP (frames))
    frames = list1 (frames);

  tmp = Qnil;
  for (rest = frames; CONSP (rest); rest = XCDR (rest))
    {
      struct frame *f = decode_window_system_frame (XCAR (rest));
      Lisp_Object frame;

      XSETFRAME (frame, f);
      if (!FRAME_VISIBLE_P (f))
	error ("Frames to be exported must be visible.");
      tmp = Fcons (frame, tmp);
    }
  frames = Fnreverse (tmp);

#ifdef CAIRO_HAS_PDF_SURFACE
  if (NILP (type) || EQ (type, Qpdf))
    surface_type = CAIRO_SURFACE_TYPE_PDF;
  else
#endif
#ifdef CAIRO_HAS_PNG_FUNCTIONS
  if (EQ (type, Qpng))
    {
      if (!NILP (XCDR (frames)))
	error ("PNG export cannot handle multiple frames.");
      surface_type = CAIRO_SURFACE_TYPE_IMAGE;
    }
  else
#endif
#ifdef CAIRO_HAS_PS_SURFACE
  if (EQ (type, Qpostscript))
    surface_type = CAIRO_SURFACE_TYPE_PS;
  else
#endif
#ifdef CAIRO_HAS_SVG_SURFACE
  if (EQ (type, Qsvg))
    {
      /* For now, we stick to SVG 1.1.  */
      if (!NILP (XCDR (frames)))
	error ("SVG export cannot handle multiple frames.");
      surface_type = CAIRO_SURFACE_TYPE_SVG;
    }
  else
#endif
    error ("Unsupported export type");

  return x_cr_export_frames (frames, surface_type);
}

#ifdef USE_GTK
DEFUN ("x-page-setup-dialog", Fx_page_setup_dialog, Sx_page_setup_dialog, 0, 0, 0,
       doc: /* Pop up a page setup dialog.
The current page setup can be obtained using `x-get-page-setup'.  */)
     (void)
{
  block_input ();
  xg_page_setup_dialog ();
  unblock_input ();

  return Qnil;
}

DEFUN ("x-get-page-setup", Fx_get_page_setup, Sx_get_page_setup, 0, 0, 0,
       doc: /* Return the value of the current page setup.
The return value is an alist containing the following keys:

  orientation: page orientation (symbol `portrait', `landscape',
	`reverse-portrait', or `reverse-landscape').
  width, height: page width/height in points not including margins.
  left-margin, right-margin, top-margin, bottom-margin: print margins,
	which is the parts of the page that the printer cannot print
	on, in points.

The paper width can be obtained as the sum of width, left-margin, and
right-margin values if the page orientation is `portrait' or
`reverse-portrait'.  Otherwise, it is the sum of width, top-margin,
and bottom-margin values.  Likewise, the paper height is the sum of
height, top-margin, and bottom-margin values if the page orientation
is `portrait' or `reverse-portrait'.  Otherwise, it is the sum of
height, left-margin, and right-margin values.  */)
     (void)
{
  Lisp_Object result;

  block_input ();
  result = xg_get_page_setup ();
  unblock_input ();

  return result;
}

DEFUN ("x-print-frames-dialog", Fx_print_frames_dialog, Sx_print_frames_dialog, 0, 1, "",
       doc: /* Pop up a print dialog to print the current contents of FRAMES.
FRAMES should be nil (the selected frame), a frame, or a list of
frames (each of which corresponds to one page).  Each frame should be
visible.

Note: Text drawn with the `x' font backend is shown with hollow boxes.  */)
     (Lisp_Object frames)
{
  Lisp_Object rest, tmp;
  int count;

  if (!CONSP (frames))
    frames = list1 (frames);

  tmp = Qnil;
  for (rest = frames; CONSP (rest); rest = XCDR (rest))
    {
      struct frame *f = decode_window_system_frame (XCAR (rest));
      Lisp_Object frame;

      XSETFRAME (frame, f);
      if (!FRAME_VISIBLE_P (f))
	error ("Frames to be printed must be visible.");
      tmp = Fcons (frame, tmp);
    }
  frames = Fnreverse (tmp);

  /* Make sure the current matrices are up-to-date.  */
  count = SPECPDL_INDEX ();
  specbind (Qredisplay_dont_pause, Qt);
  redisplay_preserve_echo_area (32);
  unbind_to (count, Qnil);

  block_input ();
  xg_print_frames_dialog (frames);
  unblock_input ();

  return Qnil;
}
#endif	/* USE_GTK */
#endif	/* USE_CAIRO */

#ifdef USE_GTK
#ifdef HAVE_GTK3
#if GTK_CHECK_VERSION (3, 14, 0)
DEFUN ("x-gtk-debug", Fx_gtk_debug, Sx_gtk_debug, 1, 1, 0,
       doc: /* Toggle interactive GTK debugging.   */)
  (Lisp_Object enable)
{
  gboolean enable_debug = !NILP (enable);

  block_input ();
  gtk_window_set_interactive_debugging (enable_debug);
  unblock_input ();

  return NILP (enable) ? Qnil : Qt;
}
#endif /* GTK_CHECK_VERSION (3, 14, 0) */
#endif /* HAVE_GTK3 */
#endif	/* USE_GTK */

/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Keep this list in the same order as frame_parms in frame.c.
   Use 0 for unsupported frame parameters.  */

frame_parm_handler x_frame_parm_handlers[] =
{
  gui_set_autoraise,
  gui_set_autolower,
  x_set_background_color,
  x_set_border_color,
  gui_set_border_width,
  x_set_cursor_color,
  x_set_cursor_type,
  gui_set_font,
  x_set_foreground_color,
  x_set_icon_name,
  x_set_icon_type,
  x_set_internal_border_width,
  gui_set_right_divider_width,
  gui_set_bottom_divider_width,
  x_set_menu_bar_lines,
  x_set_mouse_color,
  x_explicitly_set_name,
  gui_set_scroll_bar_width,
  gui_set_scroll_bar_height,
  x_set_title,
  gui_set_unsplittable,
  gui_set_vertical_scroll_bars,
  gui_set_horizontal_scroll_bars,
  gui_set_visibility,
  x_set_tab_bar_lines,
  x_set_tool_bar_lines,
  x_set_scroll_bar_foreground,
  x_set_scroll_bar_background,
  gui_set_screen_gamma,
  gui_set_line_spacing,
  gui_set_left_fringe,
  gui_set_right_fringe,
  x_set_wait_for_wm,
  gui_set_fullscreen,
  gui_set_font_backend,
  gui_set_alpha,
  x_set_sticky,
  x_set_tool_bar_position,
  x_set_inhibit_double_buffering,
  x_set_undecorated,
  x_set_parent_frame,
  x_set_skip_taskbar,
  x_set_no_focus_on_map,
  x_set_no_accept_focus,
  x_set_z_group,
  x_set_override_redirect,
  gui_set_no_special_glyphs,
};

void
syms_of_xfns (void)
{
  DEFSYM (Qundefined_color, "undefined-color");
  DEFSYM (Qcompound_text, "compound-text");
  DEFSYM (Qcancel_timer, "cancel-timer");
  DEFSYM (Qfont_parameter, "font-parameter");
  DEFSYM (Qmono, "mono");
  DEFSYM (Qassq_delete_all, "assq-delete-all");
  DEFSYM (Qhide, "hide");
  DEFSYM (Qresize_mode, "resize-mode");

#ifdef USE_CAIRO
  DEFSYM (Qpdf, "pdf");

  DEFSYM (Qorientation, "orientation");
  DEFSYM (Qtop_margin, "top-margin");
  DEFSYM (Qbottom_margin, "bottom-margin");
  DEFSYM (Qportrait, "portrait");
  DEFSYM (Qlandscape, "landscape");
  DEFSYM (Qreverse_portrait, "reverse-portrait");
  DEFSYM (Qreverse_landscape, "reverse-landscape");
#endif

  Fput (Qundefined_color, Qerror_conditions,
	pure_list (Qundefined_color, Qerror));
  Fput (Qundefined_color, Qerror_message,
	build_pure_c_string ("Undefined color"));

  DEFVAR_LISP ("x-pointer-shape", Vx_pointer_shape,
    doc: /* The shape of the pointer when over text.
Changing the value does not affect existing frames
unless you set the mouse color.  */);
  Vx_pointer_shape = Qnil;

#if false /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-nontext-pointer-shape", Vx_nontext_pointer_shape,
    doc: /* The shape of the pointer when not over text.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
#endif
  Vx_nontext_pointer_shape = Qnil;

  DEFVAR_LISP ("x-hourglass-pointer-shape", Vx_hourglass_pointer_shape,
    doc: /* The shape of the pointer when Emacs is busy.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_hourglass_pointer_shape = Qnil;

#if false /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-mode-pointer-shape", Vx_mode_pointer_shape,
    doc: /* The shape of the pointer when over the mode line.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
#endif
  Vx_mode_pointer_shape = Qnil;

  DEFVAR_LISP ("x-sensitive-text-pointer-shape",
	      Vx_sensitive_text_pointer_shape,
	       doc: /* The shape of the pointer when over mouse-sensitive text.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_sensitive_text_pointer_shape = Qnil;

  DEFVAR_LISP ("x-window-horizontal-drag-cursor",
	      Vx_window_horizontal_drag_shape,
  doc: /* Pointer shape to use for indicating a window can be dragged horizontally.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_horizontal_drag_shape = Qnil;

  DEFVAR_LISP ("x-window-vertical-drag-cursor",
	      Vx_window_vertical_drag_shape,
  doc: /* Pointer shape to use for indicating a window can be dragged vertically.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_vertical_drag_shape = Qnil;

  DEFVAR_LISP ("x-window-left-edge-cursor",
	       Vx_window_left_edge_shape,
  doc: /* Pointer shape indicating a left x-window edge can be dragged.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_left_edge_shape = Qnil;

  DEFVAR_LISP ("x-window-top-left-corner-cursor",
	       Vx_window_top_left_corner_shape,
  doc: /* Pointer shape indicating a top left x-window corner can be dragged.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_top_left_corner_shape = Qnil;

  DEFVAR_LISP ("x-window-top-edge-cursor",
	       Vx_window_top_edge_shape,
  doc: /* Pointer shape indicating a top x-window edge can be dragged.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_top_edge_shape = Qnil;

  DEFVAR_LISP ("x-window-top-right-corner-cursor",
	       Vx_window_top_right_corner_shape,
  doc: /* Pointer shape indicating a top right x-window corner can be dragged.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_top_right_corner_shape = Qnil;

  DEFVAR_LISP ("x-window-right-edge-cursor",
	       Vx_window_right_edge_shape,
  doc: /* Pointer shape indicating a right x-window edge can be dragged.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_right_edge_shape = Qnil;

  DEFVAR_LISP ("x-window-bottom-right-corner-cursor",
	       Vx_window_bottom_right_corner_shape,
  doc: /* Pointer shape indicating a bottom right x-window corner can be dragged.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_bottom_right_corner_shape = Qnil;

  DEFVAR_LISP ("x-window-bottom-edge-cursor",
	       Vx_window_bottom_edge_shape,
  doc: /* Pointer shape indicating a bottom x-window edge can be dragged.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_bottom_edge_shape = Qnil;

  DEFVAR_LISP ("x-window-bottom-left-corner-cursor",
	       Vx_window_bottom_left_corner_shape,
  doc: /* Pointer shape indicating a bottom left x-window corner can be dragged.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_bottom_left_corner_shape = Qnil;

  DEFVAR_LISP ("x-cursor-fore-pixel", Vx_cursor_fore_pixel,
    doc: /* A string indicating the foreground color of the cursor box.  */);
  Vx_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("x-max-tooltip-size", Vx_max_tooltip_size,
    doc: /* Maximum size for tooltips.
Value is a pair (COLUMNS . ROWS).  Text larger than this is clipped.  */);
  Vx_max_tooltip_size = Fcons (make_fixnum (80), make_fixnum (40));

  DEFVAR_LISP ("x-no-window-manager", Vx_no_window_manager,
    doc: /* Non-nil if no X window manager is in use.
Emacs doesn't try to figure this out; this is always nil
unless you set it to something else.  */);
  /* We don't have any way to find this out, so set it to nil
     and maybe the user would like to set it to t.  */
  Vx_no_window_manager = Qnil;

  DEFVAR_LISP ("x-pixel-size-width-font-regexp",
	       Vx_pixel_size_width_font_regexp,
    doc: /* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.

Since Emacs gets the width of a font matching this regexp from the
PIXEL_SIZE field of the name, the font-finding mechanism gets faster for
such a font.  This is especially effective for large fonts such as
Chinese, Japanese, and Korean.  */);
  Vx_pixel_size_width_font_regexp = Qnil;

/* This is not ifdef:ed, so other builds than GTK can customize it.  */
  DEFVAR_BOOL ("x-gtk-use-old-file-dialog", x_gtk_use_old_file_dialog,
    doc: /* Non-nil means prompt with the old GTK file selection dialog.
If nil or if the file selection dialog is not available, the new GTK file
chooser is used instead.  To turn off all file dialogs set the
variable `use-file-dialog'.  */);
  x_gtk_use_old_file_dialog = false;

  DEFVAR_BOOL ("x-gtk-show-hidden-files", x_gtk_show_hidden_files,
    doc: /* If non-nil, the GTK file chooser will by default show hidden files.
Note that this is just the default, there is a toggle button on the file
chooser to show or not show hidden files on a case by case basis.  */);
  x_gtk_show_hidden_files = false;

  DEFVAR_BOOL ("x-gtk-file-dialog-help-text", x_gtk_file_dialog_help_text,
    doc: /* If non-nil, the GTK file chooser will show additional help text.
If more space for files in the file chooser dialog is wanted, set this to nil
to turn the additional text off.  */);
  x_gtk_file_dialog_help_text = true;

  DEFVAR_BOOL ("x-gtk-use-system-tooltips", x_gtk_use_system_tooltips,
    doc: /* If non-nil with a Gtk+ built Emacs, the Gtk+ tooltip is used.
Otherwise use Emacs own tooltip implementation.
When using Gtk+ tooltips, the tooltip face is not used.  */);
  x_gtk_use_system_tooltips = true;

  DEFVAR_LISP ("x-gtk-resize-child-frames", x_gtk_resize_child_frames,
    doc: /* If non-nil, resize child frames specially with GTK builds.
If this is nil, resize child frames like any other frames.  This is the
default and usually works with most desktops.  Some desktop environments
(GNOME shell in particular when using the mutter window manager),
however, may refuse to resize a child frame when Emacs is built with
GTK3.  For those environments, the two settings below are provided.

If this equals the symbol 'hide', Emacs temporarily hides the child
frame during resizing.  This approach seems to work reliably, may
however induce some flicker when the frame is made visible again.

If this equals the symbol 'resize-mode', Emacs uses GTK's resize mode to
always trigger an immediate resize of the child frame.  This method is
deprecated by GTK and may not work in future versions of that toolkit.
It also may freeze Emacs when used with other desktop environments.  It
avoids, however, the unpleasent flicker induced by the hiding approach.

This variable is considered a temporary workaround and will be hopefully
eliminated in future versions of Emacs.  */);
  x_gtk_resize_child_frames = Qnil;

  /* Tell Emacs about this window system.  */
  Fprovide (Qx, Qnil);

#ifdef USE_X_TOOLKIT
  Fprovide (intern_c_string ("x-toolkit"), Qnil);
#ifdef USE_MOTIF
  Fprovide (intern_c_string ("motif"), Qnil);

  DEFVAR_LISP ("motif-version-string", Vmotif_version_string,
	       doc: /* Version info for LessTif/Motif.  */);
  Vmotif_version_string = build_string (XmVERSION_STRING);
#endif /* USE_MOTIF */
#endif /* USE_X_TOOLKIT */

#ifdef USE_GTK
  /* Provide x-toolkit also for GTK.  Internally GTK does not use Xt so it
     is not an X toolkit in that sense (USE_X_TOOLKIT is not defined).
     But for a user it is a toolkit for X, and indeed, configure
     accepts --with-x-toolkit=gtk.  */
  Fprovide (intern_c_string ("x-toolkit"), Qnil);
  Fprovide (intern_c_string ("gtk"), Qnil);
  Fprovide (intern_c_string ("move-toolbar"), Qnil);

  DEFVAR_LISP ("gtk-version-string", Vgtk_version_string,
               doc: /* Version info for GTK+.  */);
  {
    char gtk_version[sizeof ".." + 3 * INT_STRLEN_BOUND (int)];
    int len = sprintf (gtk_version, "%d.%d.%d",
		       GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
    Vgtk_version_string = make_pure_string (gtk_version, len, len, false);
  }
#endif /* USE_GTK */

#ifdef USE_CAIRO
  Fprovide (intern_c_string ("cairo"), Qnil);

  DEFVAR_LISP ("cairo-version-string", Vcairo_version_string,
               doc: /* Version info for cairo.  */);
  {
    char cairo_version[sizeof ".." + 3 * INT_STRLEN_BOUND (int)];
    int len = sprintf (cairo_version, "%d.%d.%d",
		       CAIRO_VERSION_MAJOR, CAIRO_VERSION_MINOR,
                       CAIRO_VERSION_MICRO);
    Vcairo_version_string = make_pure_string (cairo_version, len, len, false);
  }
#endif

  /* X window properties.  */
  defsubr (&Sx_change_window_property);
  defsubr (&Sx_delete_window_property);
  defsubr (&Sx_window_property);
  defsubr (&Sx_window_property_attributes);

  defsubr (&Sxw_display_color_p);
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);
  defsubr (&Sx_server_max_request_size);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sx_display_mm_width);
  defsubr (&Sx_display_mm_height);
  defsubr (&Sx_display_screens);
  defsubr (&Sx_display_planes);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_display_backing_store);
  defsubr (&Sx_display_save_under);
  defsubr (&Sx_display_monitor_attributes_list);
  defsubr (&Sx_frame_geometry);
  defsubr (&Sx_frame_edges);
  defsubr (&Sx_frame_list_z_order);
  defsubr (&Sx_frame_restack);
  defsubr (&Sx_mouse_absolute_pixel_position);
  defsubr (&Sx_set_mouse_absolute_pixel_position);
  defsubr (&Sx_wm_set_size_hint);
  defsubr (&Sx_create_frame);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_synchronize);
  defsubr (&Sx_backspace_delete_keys_p);
  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
  defsubr (&Sx_double_buffered_p);
  tip_timer = Qnil;
  staticpro (&tip_timer);
  tip_frame = Qnil;
  staticpro (&tip_frame);
  tip_last_frame = Qnil;
  staticpro (&tip_last_frame);
  tip_last_string = Qnil;
  staticpro (&tip_last_string);
  tip_last_parms = Qnil;
  staticpro (&tip_last_parms);

  defsubr (&Sx_uses_old_gtk_dialog);
#if defined (USE_MOTIF) || defined (USE_GTK)
  defsubr (&Sx_file_dialog);
#endif

#if defined (USE_GTK) && defined (HAVE_FREETYPE)
  defsubr (&Sx_select_font);
#endif

#ifdef USE_CAIRO
  defsubr (&Sx_export_frames);
#ifdef USE_GTK
  defsubr (&Sx_page_setup_dialog);
  defsubr (&Sx_get_page_setup);
  defsubr (&Sx_print_frames_dialog);
#endif
#endif
#ifdef USE_GTK
#ifdef HAVE_GTK3
#if GTK_CHECK_VERSION (3, 14, 0)
  defsubr (&Sx_gtk_debug);
#endif
#endif
#endif
}
