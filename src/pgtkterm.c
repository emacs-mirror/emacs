/* Pure Gtk+-3 communication module.      -*- coding: utf-8 -*-

Copyright (C) 1989, 1993-1994, 2005-2006, 2008-2017 Free Software
Foundation, Inc.

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

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>

#include <fcntl.h>
#include <math.h>
#include <pthread.h>
#include <sys/types.h>
#include <time.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>

#include <c-ctype.h>
#include <c-strcase.h>
#include <ftoastr.h>

#include "lisp.h"
#include "blockinput.h"
#include "sysselect.h"
#include "gtkutil.h"
#include "systime.h"
#include "character.h"
#include "xwidget.h"
#include "fontset.h"
#include "composite.h"
#include "ccl.h"
#include "dynlib.h"

#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "menu.h"
#include "window.h"
#include "keyboard.h"
#include "atimer.h"
#include "buffer.h"
#include "font.h"
#include "xsettings.h"
#include "pgtkselect.h"

#define STORE_KEYSYM_FOR_DEBUG(keysym) ((void)0)

#define FRAME_CR_CONTEXT(f)	((f)->output_data.pgtk->cr_context)
#define FRAME_CR_SURFACE(f)	((f)->output_data.pgtk->cr_surface)

struct pgtk_display_info *x_display_list; /* Chain of existing displays */
extern Lisp_Object tip_frame;

static struct event_queue_t {
  union buffered_input_event *q;
  int nr, cap;
} event_q = {
  NULL, 0, 0,
};

/* Non-zero timeout value means ignore next mouse click if it arrives
   before that timeout elapses (i.e. as part of the same sequence of
   events resulting from clicking on a frame to select it).  */

static Time ignore_next_mouse_click_timeout;

static Lisp_Object xg_default_icon_file;

static void pgtk_delete_display (struct pgtk_display_info *dpyinfo);
static void pgtk_clear_frame_area(struct frame *f, int x, int y, int width, int height);
static void pgtk_fill_rectangle(struct frame *f, unsigned long color, int x, int y, int width, int height);
static void pgtk_clip_to_row (struct window *w, struct glyph_row *row,
				enum glyph_row_area area, cairo_t *cr);
static struct frame *
pgtk_any_window_to_frame (GdkWindow *window);


static void evq_enqueue(union buffered_input_event *ev)
{
  struct event_queue_t *evq = &event_q;
  if (evq->cap == 0) {
    evq->cap = 4;
    evq->q = xmalloc(sizeof *evq->q * evq->cap);
  }

  if (evq->nr >= evq->cap) {
    evq->cap += evq->cap / 2;
    evq->q = xrealloc(evq->q, sizeof *evq->q * evq->cap);
  }

  evq->q[evq->nr++] = *ev;
  raise(SIGIO);
}

static int evq_flush(struct input_event *hold_quit)
{
  struct event_queue_t *evq = &event_q;
  int i, n = evq->nr;
  for (i = 0; i < n; i++)
    kbd_buffer_store_buffered_event (&evq->q[i], hold_quit);
  evq->nr = 0;
  return n;
}

void
mark_pgtkterm(void)
{
  struct event_queue_t *evq = &event_q;
  int i, n = evq->nr;
  for (i = 0; i < n; i++) {
    union buffered_input_event *ev = &evq->q[i];
    mark_object (ev->ie.x);
    mark_object (ev->ie.y);
    mark_object (ev->ie.frame_or_window);
    mark_object (ev->ie.arg);
  }

  struct pgtk_display_info *dpyinfo;
  for (dpyinfo = x_display_list; dpyinfo != NULL; dpyinfo = dpyinfo->next) {
#if false  /* marked in alloc.c:compact_font_caches() */
    mark_object (dpyinfo->name_list_element);
#endif
    mark_object (dpyinfo->rdb);
  }
}

char *
get_keysym_name (int keysym)
/* --------------------------------------------------------------------------
    Called by keyboard.c.  Not sure if the return val is important, except
    that it be unique.
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("x_get_ksysym_name");
  static char value[16];
  sprintf (value, "%d", keysym);
  return value;
}

void
frame_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
/* --------------------------------------------------------------------------
     Programmatically reposition mouse pointer in pixel coordinates
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("frame_set_mouse_pixel_position");
}

/* Free X resources of frame F.  */

void
x_free_frame_resources (struct frame *f)
{
  struct pgtk_display_info *dpyinfo;
  Mouse_HLInfo *hlinfo;

  PGTK_TRACE ("x_free_frame_resources");
  check_window_system (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);
  hlinfo = MOUSE_HL_INFO (f);

  block_input ();

  free_frame_faces (f);

#define CLEAR_IF_EQ(FIELD)	\
  do { if (f == dpyinfo->FIELD) dpyinfo->FIELD = 0; } while (false)

  CLEAR_IF_EQ(x_focus_frame);
  CLEAR_IF_EQ(highlight_frame);
  CLEAR_IF_EQ(x_focus_event_frame);
  CLEAR_IF_EQ(last_mouse_frame);
  CLEAR_IF_EQ(last_mouse_motion_frame);
  CLEAR_IF_EQ(last_mouse_glyph_frame);

#undef CLEAR_IF_EQ

  if (f == hlinfo->mouse_face_mouse_frame)
    reset_mouse_highlight (hlinfo);

  gtk_widget_destroy(FRAME_GTK_OUTER_WIDGET(f));

  if (FRAME_X_OUTPUT(f)->cr_surface_visible_bell != NULL) {
    cairo_surface_destroy(FRAME_X_OUTPUT(f)->cr_surface_visible_bell);
    FRAME_X_OUTPUT(f)->cr_surface_visible_bell = NULL;
  }

  if (FRAME_X_OUTPUT(f)->atimer_visible_bell != NULL) {
    cancel_atimer(FRAME_X_OUTPUT(f)->atimer_visible_bell);
    FRAME_X_OUTPUT(f)->atimer_visible_bell = NULL;
  }

  xfree (f->output_data.pgtk);
  f->output_data.pgtk = NULL;

  unblock_input ();
}

void
x_destroy_window (struct frame *f)
/* --------------------------------------------------------------------------
     External: Delete the window
   -------------------------------------------------------------------------- */
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  PGTK_TRACE ("x_destroy_window");

  check_window_system (f);
  if (dpyinfo->gdpy != NULL)
    x_free_frame_resources (f);

  dpyinfo->reference_count--;
}

/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

static void
x_calc_absolute_position (struct frame *f)
{
  int flags = f->size_hint_flags;
  struct frame *p = FRAME_PARENT_FRAME (f);

  /* We have nothing to do if the current position
     is already for the top-left corner.  */
  if (! ((flags & XNegative) || (flags & YNegative)))
    return;

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if ((flags & XNegative) && (f->left_pos <= 0))
    {
      int width = FRAME_PIXEL_WIDTH (f);

      /* A frame that has been visible at least once should have outer
	 edges.  */
      if (FRAME_X_OUTPUT(f)->has_been_visible && !p)
	{
	  Lisp_Object frame;
	  Lisp_Object edges = Qnil;

	  XSETFRAME (frame, f);
	  edges = Fpgtk_frame_edges (frame, Qouter_edges);
	  if (!NILP (edges))
	    width = (XFIXNUM (Fnth (make_fixnum (2), edges))
		     - XFIXNUM (Fnth (make_fixnum (0), edges)));
	}

      if (p)
	f->left_pos = (FRAME_PIXEL_WIDTH (p) - width - 2 * f->border_width
		       + f->left_pos);
      else
	f->left_pos = (x_display_pixel_width (FRAME_DISPLAY_INFO (f))
		       - width + f->left_pos);

    }

  if ((flags & YNegative) && (f->top_pos <= 0))
    {
      int height = FRAME_PIXEL_HEIGHT (f);

      if (FRAME_X_OUTPUT(f)->has_been_visible && !p)
	{
	  Lisp_Object frame;
	  Lisp_Object edges = Qnil;

	  XSETFRAME (frame, f);
	  if (NILP (edges))
	    edges = Fpgtk_frame_edges (frame, Qouter_edges);
	  if (!NILP (edges))
	    height = (XFIXNUM (Fnth (make_fixnum (3), edges))
		      - XFIXNUM (Fnth (make_fixnum (1), edges)));
	}

      if (p)
	f->top_pos = (FRAME_PIXEL_HEIGHT (p) - height - 2 * f->border_width
		       + f->top_pos);
      else
	f->top_pos = (x_display_pixel_height (FRAME_DISPLAY_INFO (f))
		      - height + f->top_pos);
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

static void
x_set_offset (struct frame *f, int xoff, int yoff, int change_gravity)
/* --------------------------------------------------------------------------
     External: Position the window
   -------------------------------------------------------------------------- */
{
  /* not working on wayland. */

  PGTK_TRACE("x_set_offset: %d,%d,%d.", xoff, yoff, change_gravity);

  if (change_gravity > 0)
    {
      PGTK_TRACE("x_set_offset: change_gravity > 0");
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

  block_input ();
  x_wm_set_size_hint (f, 0, false);

  /* When a position change was requested and the outer GTK widget
     has been realized already, leave it to gtk_window_move to DTRT
     and return.  Used for Bug#25851 and Bug#25943.  */
  if (change_gravity != 0 && FRAME_GTK_OUTER_WIDGET (f)) {
    PGTK_TRACE("x_set_offset: move to %d,%d.", f->left_pos, f->top_pos);
    gtk_window_move (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
		     f->left_pos, f->top_pos);
  }

  unblock_input ();
}

static void
pgtk_set_window_size (struct frame *f,
                   bool change_gravity,
                   int width,
                   int height,
                   bool pixelwise)
/* --------------------------------------------------------------------------
     Adjust window pixel size based on given character grid size
     Impl is a bit more complex than other terms, need to do some
     internal clipping.
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("pgtk_set_window_size(%dx%d, %s)", width, height, pixelwise ? "pixel" : "char");
  int pixelwidth, pixelheight;

  block_input ();

  gtk_widget_get_size_request(FRAME_GTK_WIDGET(f), &pixelwidth, &pixelheight);
  PGTK_TRACE("old: %dx%d", pixelwidth, pixelheight);

  if (pixelwise)
    {
      pixelwidth = FRAME_TEXT_TO_PIXEL_WIDTH (f, width);
      pixelheight = FRAME_TEXT_TO_PIXEL_HEIGHT (f, height);
    }
  else
    {
      pixelwidth =  FRAME_TEXT_COLS_TO_PIXEL_WIDTH   (f, width);
      pixelheight = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, height);
    }

  frame_size_history_add
    (f, Qx_set_window_size_1, width, height,
     list5 (Fcons (make_fixnum (pixelwidth), make_fixnum (pixelheight)),
	    Fcons (make_fixnum (pixelwidth), make_fixnum (pixelheight)),
	    make_fixnum (f->border_width),
	    make_fixnum (FRAME_PGTK_TITLEBAR_HEIGHT (f)),
	    make_fixnum (FRAME_TOOLBAR_HEIGHT (f))));

  PGTK_TRACE("new: %dx%d", pixelwidth, pixelheight);
  for (GtkWidget *w = FRAME_GTK_WIDGET(f); w != NULL; w = gtk_widget_get_parent(w)) {
    PGTK_TRACE("%p %s %d %d", w, G_OBJECT_TYPE_NAME(w), gtk_widget_get_mapped(w), gtk_widget_get_visible(w));
    gint wd, hi;
    gtk_widget_get_size_request(w, &wd, &hi);
    PGTK_TRACE(" %dx%d", wd, hi);
    GtkAllocation alloc;
    gtk_widget_get_allocation(w, &alloc);
    PGTK_TRACE(" %dx%d+%d+%d", alloc.width, alloc.height, alloc.x, alloc.y);
  }

  PGTK_TRACE("pgtk_set_window_size: %p: %dx%d.", f, width, height);
  f->output_data.pgtk->preferred_width = pixelwidth;
  f->output_data.pgtk->preferred_height = pixelheight;
  x_wm_set_size_hint(f, 0, 0);
  xg_frame_set_char_size (f, FRAME_PIXEL_TO_TEXT_WIDTH(f, pixelwidth), FRAME_PIXEL_TO_TEXT_HEIGHT(f, pixelheight));
  gtk_widget_queue_resize (FRAME_GTK_OUTER_WIDGET (f));

  unblock_input ();
}

void
pgtk_iconify_frame (struct frame *f)
/* --------------------------------------------------------------------------
     External: Iconify window
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("pgtk_iconify_frame");

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_DISPLAY_INFO (f)->highlight_frame == f)
    FRAME_DISPLAY_INFO (f)->highlight_frame = 0;

  if (FRAME_ICONIFIED_P (f))
    return;

  block_input ();

#if 0
  x_set_bitmap_icon (f);
#endif

  if (FRAME_GTK_OUTER_WIDGET (f))
    {
      if (! FRAME_VISIBLE_P (f))
        gtk_widget_show_all (FRAME_GTK_OUTER_WIDGET (f));

      gtk_window_iconify (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
      SET_FRAME_VISIBLE (f, 0);
      SET_FRAME_ICONIFIED (f, true);
      unblock_input ();
      return;
    }

  /* Make sure the X server knows where the window should be positioned,
     in case the user deiconifies with the window manager.  */
  if (! FRAME_VISIBLE_P (f)
      && ! FRAME_ICONIFIED_P (f)
#if 0
      && ! FRAME_X_EMBEDDED_P (f)
#endif
      )
    x_set_offset (f, f->left_pos, f->top_pos, 0);

#if 0
  if (!FRAME_VISIBLE_P (f))
    {
      /* If the frame was withdrawn, before, we must map it.  */
      XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
    }
#endif

  SET_FRAME_ICONIFIED (f, true);
  SET_FRAME_VISIBLE (f, 0);

  unblock_input ();
}

static gboolean
pgtk_make_frame_visible_wait_for_map_event_cb (GtkWidget *widget, GdkEventAny *event, gpointer user_data)
{
  int *foundptr = user_data;
  *foundptr = 1;
  return FALSE;
}

static gboolean
pgtk_make_frame_visible_wait_for_map_event_timeout (gpointer user_data)
{
  int *timedoutptr = user_data;
  *timedoutptr = 1;
  return FALSE;
}

void
pgtk_make_frame_visible (struct frame *f)
/* --------------------------------------------------------------------------
     External: Show the window (X11 semantics)
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("pgtk_make_frame_visible");

  GtkWidget *win = FRAME_GTK_OUTER_WIDGET (f);

  if (! FRAME_VISIBLE_P (f))
    {
      gtk_widget_show(win);
      gtk_window_deiconify(GTK_WINDOW(win));

      if (FLOATP (Vpgtk_wait_for_event_timeout)) {
	guint msec = (guint) (XFLOAT_DATA (Vpgtk_wait_for_event_timeout) * 1000);
	int found = 0;
	int timed_out = 0;
	gulong id = g_signal_connect(win, "map-event", G_CALLBACK(pgtk_make_frame_visible_wait_for_map_event_cb), &found);
	guint src = g_timeout_add(msec, pgtk_make_frame_visible_wait_for_map_event_timeout, &timed_out);
	while (!found && !timed_out)
	  gtk_main_iteration();
	g_signal_handler_disconnect (win, id);
	if (!timed_out)
	  g_source_remove(src);
      }
    }
}


void
pgtk_make_frame_invisible (struct frame *f)
/* --------------------------------------------------------------------------
     External: Hide the window (X11 semantics)
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("pgtk_make_frame_invisible");

  GtkWidget *win = FRAME_OUTPUT_DATA(f)->widget;

  gtk_widget_hide(win);

  SET_FRAME_VISIBLE (f, 0);
  SET_FRAME_ICONIFIED (f, false);
}

static void
pgtk_make_frame_visible_invisible (struct frame *f, bool visible)
{
  if (visible)
    pgtk_make_frame_visible (f);
  else
    pgtk_make_frame_invisible (f);
}

static Lisp_Object
pgtk_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  PGTK_TRACE("pgtk_new_font");
  struct font *font = XFONT_OBJECT (font_object);
  int font_ascent, font_descent;

  if (fontset < 0)
    fontset = fontset_from_font (font_object);
  FRAME_FONTSET (f) = fontset;

  if (FRAME_FONT (f) == font) {
    /* This font is already set in frame F.  There's nothing more to
       do.  */
    PGTK_TRACE("already set.");
    return font_object;
  }

  FRAME_FONT (f) = font;
  PGTK_TRACE("font:");
  PGTK_TRACE("  %p", font);
  PGTK_TRACE("  name: %s", SSDATA(font_get_name(font_object)));
  PGTK_TRACE("  width: %d..%d", font->min_width, font->max_width);
  PGTK_TRACE("  pixel_size: %d", font->pixel_size);
  PGTK_TRACE("  height: %d", font->height);
  PGTK_TRACE("  space_width: %d", font->space_width);
  PGTK_TRACE("  average_width: %d", font->average_width);
  PGTK_TRACE("  asc/desc: %d,%d", font->ascent, font->descent);
  PGTK_TRACE("  ul thickness: %d", font->underline_thickness);
  PGTK_TRACE("  ul position: %d", font->underline_position);
  PGTK_TRACE("  vertical_centering: %d", font->vertical_centering);
  PGTK_TRACE("  baseline_offset: %d", font->baseline_offset);
  PGTK_TRACE("  relative_compose: %d", font->relative_compose);
  PGTK_TRACE("  default_ascent: %d", font->default_ascent);
  PGTK_TRACE("  encoding_charset: %d", font->encoding_charset);
  PGTK_TRACE("  repertory_charset: %d", font->repertory_charset);

  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;
  get_font_ascent_descent (font, &font_ascent, &font_descent);
  FRAME_LINE_HEIGHT (f) = font_ascent + font_descent;

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

  /* Compute the scroll bar height in character lines.  */
  if (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) > 0)
    {
      int height = FRAME_LINE_HEIGHT (f);
      FRAME_CONFIG_SCROLL_BAR_LINES (f)
	= (FRAME_CONFIG_SCROLL_BAR_HEIGHT (f) + height - 1) / height;
    }
  else
    {
      int height = FRAME_LINE_HEIGHT (f);
      FRAME_CONFIG_SCROLL_BAR_LINES (f) = (14 + height - 1) / height;
    }

  /* Now make the frame display the given font.  */
  if (FRAME_GTK_WIDGET (f) != NULL)
    adjust_frame_size (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
		       FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 3,
		       false, Qfont);

  PGTK_TRACE("set new.");
  return font_object;
}

int
x_display_pixel_height (struct pgtk_display_info *dpyinfo)
{
  PGTK_TRACE("x_display_pixel_height");

  GdkDisplay *gdpy = dpyinfo->gdpy;
  GdkScreen *gscr = gdk_display_get_default_screen(gdpy);
  PGTK_TRACE(" = %d", gdk_screen_get_height(gscr));
  return gdk_screen_get_height(gscr);
}

int
x_display_pixel_width (struct pgtk_display_info *dpyinfo)
{
  PGTK_TRACE("x_display_pixel_width");

  GdkDisplay *gdpy = dpyinfo->gdpy;
  GdkScreen *gscr = gdk_display_get_default_screen(gdpy);
  PGTK_TRACE(" = %d", gdk_screen_get_width(gscr));
  return gdk_screen_get_width(gscr);
}

void
x_set_no_focus_on_map (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* Set frame F's `no-focus-on-map' parameter which, if non-nil, means
 * that F's window-system window does not want to receive input focus
 * when it is mapped.  (A frame's window is mapped when the frame is
 * displayed for the first time and when the frame changes its state
 * from `iconified' or `invisible' to `visible'.)
 *
 * Some window managers may not honor this parameter. */
{
  PGTK_TRACE("x_set_no_accept_focus_on_map");
  /* doesn't work on wayland. */

  if (!EQ (new_value, old_value))
    {
      xg_set_no_focus_on_map (f, new_value);
      FRAME_NO_FOCUS_ON_MAP (f) = !NILP (new_value);
    }
}

void
x_set_no_accept_focus (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/*  Set frame F's `no-accept-focus' parameter which, if non-nil, hints
 * that F's window-system window does not want to receive input focus
 * via mouse clicks or by moving the mouse into it.
 *
 * If non-nil, this may have the unwanted side-effect that a user cannot
 * scroll a non-selected frame with the mouse.
 *
 * Some window managers may not honor this parameter. */
{
  /* doesn't work on wayland. */
  PGTK_TRACE("x_set_no_accept_focus");

  xg_set_no_accept_focus (f, new_value);
  FRAME_NO_ACCEPT_FOCUS (f) = !NILP (new_value);
}

void
x_set_z_group (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
/* Set frame F's `z-group' parameter.  If `above', F's window-system
   window is displayed above all windows that do not have the `above'
   property set.  If nil, F's window is shown below all windows that
   have the `above' property set and above all windows that have the
   `below' property set.  If `below', F's window is displayed below
   all windows that do.

   Some window managers may not honor this parameter. */
{
  /* doesn't work on wayland. */
  PGTK_TRACE("x_set_z_group");

  if (NILP (new_value))
    {
      gtk_window_set_keep_above (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), FALSE);
      gtk_window_set_keep_below (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), FALSE);
      FRAME_Z_GROUP (f) = z_group_none;
    }
  else if (EQ (new_value, Qabove))
    {
      gtk_window_set_keep_above (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), TRUE);
      gtk_window_set_keep_below (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), FALSE);
      FRAME_Z_GROUP (f) = z_group_above;
    }
  else if (EQ (new_value, Qabove_suspended))
    {
      gtk_window_set_keep_above (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), FALSE);
      FRAME_Z_GROUP (f) = z_group_above_suspended;
    }
  else if (EQ (new_value, Qbelow))
    {
      gtk_window_set_keep_above (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), FALSE);
      gtk_window_set_keep_below (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), TRUE);
      FRAME_Z_GROUP (f) = z_group_below;
    }
  else
    error ("Invalid z-group specification");
}

static void
pgtk_initialize_display_info (struct pgtk_display_info *dpyinfo)
/* --------------------------------------------------------------------------
      Initialize global info and storage for display.
   -------------------------------------------------------------------------- */
{
    dpyinfo->resx = 72.27; /* used 75.0, but this makes pt == pixel, expected */
    dpyinfo->resy = 72.27;
    dpyinfo->color_p = 1;
    dpyinfo->n_planes = 32;
    dpyinfo->root_window = 42; /* a placeholder.. */
    dpyinfo->highlight_frame = dpyinfo->x_focus_frame = NULL;
    dpyinfo->n_fonts = 0;
    dpyinfo->smallest_font_height = 1;
    dpyinfo->smallest_char_width = 1;

    reset_mouse_highlight (&dpyinfo->mouse_highlight);
}

/* Set S->gc to a suitable GC for drawing glyph string S in cursor
   face.  */

static void
x_set_cursor_gc (struct glyph_string *s)
{
  PGTK_TRACE("x_set_cursor_gc.");
  if (s->font == FRAME_FONT (s->f)
      && s->face->background == FRAME_BACKGROUND_PIXEL (s->f)
      && s->face->foreground == FRAME_FOREGROUND_PIXEL (s->f)
      && !s->cmp)
    PGTK_TRACE("x_set_cursor_gc: 1."),
    s->xgcv = FRAME_X_OUTPUT(s->f)->cursor_xgcv;
  else
    {
      /* Cursor on non-default face: must merge.  */
      Emacs_GC xgcv;

      PGTK_TRACE("x_set_cursor_gc: 2.");
      xgcv.background = FRAME_X_OUTPUT(s->f)->cursor_color;
      xgcv.foreground = s->face->background;
      PGTK_TRACE("x_set_cursor_gc: 3. %08lx, %08lx.", xgcv.background, xgcv.foreground);

      /* If the glyph would be invisible, try a different foreground.  */
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      PGTK_TRACE("x_set_cursor_gc: 4. %08lx, %08lx.", xgcv.background, xgcv.foreground);
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = FRAME_X_OUTPUT(s->f)->cursor_foreground_color;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      PGTK_TRACE("x_set_cursor_gc: 5. %08lx, %08lx.", xgcv.background, xgcv.foreground);

      /* Make sure the cursor is distinct from text in this face.  */
      if (xgcv.background == s->face->background
	  && xgcv.foreground == s->face->foreground)
	{
	  xgcv.background = s->face->foreground;
	  xgcv.foreground = s->face->background;
	}
      PGTK_TRACE("x_set_cursor_gc: 6. %08lx, %08lx.", xgcv.background, xgcv.foreground);

      IF_DEBUG (x_check_font (s->f, s->font));

      s->xgcv = xgcv;
    }
}


/* Set up S->gc of glyph string S for drawing text in mouse face.  */

static void
x_set_mouse_face_gc (struct glyph_string *s)
{
  int face_id;
  struct face *face;

  /* What face has to be used last for the mouse face?  */
  face_id = MOUSE_HL_INFO (s->f)->mouse_face_face_id;
  face = FACE_FROM_ID_OR_NULL (s->f, face_id);
  if (face == NULL)
    face = FACE_FROM_ID (s->f, MOUSE_FACE_ID);

  if (s->first_glyph->type == CHAR_GLYPH)
    face_id = FACE_FOR_CHAR (s->f, face, s->first_glyph->u.ch, -1, Qnil);
  else
    face_id = FACE_FOR_CHAR (s->f, face, 0, -1, Qnil);
  s->face = FACE_FROM_ID (s->f, face_id);
  prepare_face_for_display (s->f, s->face);

  if (s->font == s->face->font) {
    s->xgcv.foreground = s->face->foreground;
    s->xgcv.background = s->face->background;
  } else
    {
      /* Otherwise construct scratch_cursor_gc with values from FACE
	 except for FONT.  */
      Emacs_GC xgcv;

      xgcv.background = s->face->background;
      xgcv.foreground = s->face->foreground;

      s->xgcv = xgcv;

    }
}


/* Set S->gc of glyph string S to a GC suitable for drawing a mode line.
   Faces to use in the mode line have already been computed when the
   matrix was built, so there isn't much to do, here.  */

static void
x_set_mode_line_face_gc (struct glyph_string *s)
{
  s->xgcv.foreground = s->face->foreground;
  s->xgcv.background = s->face->background;
}


/* Set S->gc of glyph string S for drawing that glyph string.  Set
   S->stippled_p to a non-zero value if the face of S has a stipple
   pattern.  */

static void
x_set_glyph_string_gc (struct glyph_string *s)
{
  PGTK_TRACE("x_set_glyph_string_gc: s->f:    %08lx, %08lx", s->f->background_pixel, s->f->foreground_pixel);
  PGTK_TRACE("x_set_glyph_string_gc: s->face: %08lx, %08lx", s->face->background, s->face->foreground);
  prepare_face_for_display (s->f, s->face);
  PGTK_TRACE("x_set_glyph_string_gc: s->face: %08lx, %08lx", s->face->background, s->face->foreground);

  if (s->hl == DRAW_NORMAL_TEXT)
    {
      s->xgcv.foreground = s->face->foreground;
      s->xgcv.background = s->face->background;
      s->stippled_p = s->face->stipple != 0;
      PGTK_TRACE("x_set_glyph_string_gc: %08lx, %08lx", s->xgcv.background, s->xgcv.foreground);
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      x_set_mode_line_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
      PGTK_TRACE("x_set_glyph_string_gc: %08lx, %08lx", s->xgcv.background, s->xgcv.foreground);
    }
  else if (s->hl == DRAW_CURSOR)
    {
      x_set_cursor_gc (s);
      s->stippled_p = false;
      PGTK_TRACE("x_set_glyph_string_gc: %08lx, %08lx", s->xgcv.background, s->xgcv.foreground);
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      x_set_mouse_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
      PGTK_TRACE("x_set_glyph_string_gc: %08lx, %08lx", s->xgcv.background, s->xgcv.foreground);
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      s->xgcv.foreground = s->face->foreground;
      s->xgcv.background = s->face->background;
      s->stippled_p = s->face->stipple != 0;
      PGTK_TRACE("x_set_glyph_string_gc: %08lx, %08lx", s->xgcv.background, s->xgcv.foreground);
    }
  else
    emacs_abort ();
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static void
x_set_glyph_string_clipping (struct glyph_string *s, cairo_t *cr)
{
  XRectangle r[2];
  int n = get_glyph_string_clip_rects (s, r, 2);
  PGTK_TRACE("x_set_glyph_string_clipping: n=%d.", n);

  if (n > 0) {
    for (int i = 0; i < n; i++) {
      PGTK_TRACE("x_set_glyph_string_clipping: r[%d]: %ux%u+%d+%d.",
		 i, r[i].width, r[i].height, r[i].x, r[i].y);
      cairo_rectangle(cr, r[i].x, r[i].y, r[i].width, r[i].height);
    }
    cairo_clip(cr);
  }
  PGTK_TRACE("clip result:");
  cairo_rectangle_list_t *rects = cairo_copy_clip_rectangle_list(cr);
  for (int i = 0; i < rects->num_rectangles; i++) {
    PGTK_TRACE(" rect[%d]: %dx%d+%d+%d.",
	       i,
	       (int) rects->rectangles[i].width,
	       (int) rects->rectangles[i].height,
	       (int) rects->rectangles[i].x,
	       (int) rects->rectangles[i].y);
  }
  cairo_rectangle_list_destroy(rects);
}


/* Set SRC's clipping for output of glyph string DST.  This is called
   when we are drawing DST's left_overhang or right_overhang only in
   the area of SRC.  */

static void
x_set_glyph_string_clipping_exactly (struct glyph_string *src, struct glyph_string *dst, cairo_t *cr)
{
  dst->clip[0].x = src->x;
  dst->clip[0].y = src->y;
  dst->clip[0].width = src->width;
  dst->clip[0].height = src->height;
  dst->num_clips = 1;

  cairo_rectangle(cr, src->x, src->y, src->width, src->height);
  cairo_clip(cr);
}


/* RIF:
   Compute left and right overhang of glyph string S.  */

static void
pgtk_compute_glyph_string_overhangs (struct glyph_string *s)
{
  if (s->cmp == NULL
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))
    {
      struct font_metrics metrics;

      if (s->first_glyph->type == CHAR_GLYPH)
	{
	  unsigned *code = alloca (sizeof (unsigned) * s->nchars);
	  struct font *font = s->font;
	  int i;

	  for (i = 0; i < s->nchars; i++)
	    code[i] = s->char2b[i];
	  font->driver->text_extents (font, code, s->nchars, &metrics);
	}
      else
	{
	  Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);

	  composition_gstring_width (gstring, s->cmp_from, s->cmp_to, &metrics);
	}
      s->right_overhang = (metrics.rbearing > metrics.width
			   ? metrics.rbearing - metrics.width : 0);
      s->left_overhang = metrics.lbearing < 0 ? - metrics.lbearing : 0;
    }
  else if (s->cmp)
    {
      s->right_overhang = s->cmp->rbearing - s->cmp->pixel_width;
      s->left_overhang = - s->cmp->lbearing;
    }
}


/* Fill rectangle X, Y, W, H with background color of glyph string S.  */

static void
x_clear_glyph_string_rect (struct glyph_string *s, int x, int y, int w, int h)
{
  pgtk_fill_rectangle(s->f, s->xgcv.background, x, y, w, h);
}


static cairo_surface_t *
create_background_surface_by_face (struct frame *f, struct face *face, int x, int y, int width, int height)
{
  cairo_surface_t *surface = cairo_surface_create_similar (FRAME_CR_SURFACE (f),
							  CAIRO_CONTENT_COLOR,
							  width,
							  height);

  {
    cairo_t *cr = cairo_create (surface);

    double r = ((face->background >> 16) & 0xff) / 255.0;
    double g = ((face->background >>  8) & 0xff) / 255.0;
    double b = ((face->background >>  0) & 0xff) / 255.0;
    cairo_set_source_rgb (cr, r, g, b);
    cairo_paint (cr);

    cairo_destroy (cr);
  }

  if (face->stipple != 0) {
    GdkPixbuf *pixbuf = FRAME_DISPLAY_INFO (f)->bitmaps[face->stipple - 1].img;
    GdkPixbuf *pb = gdk_pixbuf_add_alpha (pixbuf, TRUE, 255, 255, 255);
    cairo_surface_t *mask = cairo_surface_create_similar_image (FRAME_CR_SURFACE (f),
								CAIRO_FORMAT_A1,
								width,
								height);

    {
      cairo_t *cr = cairo_create (mask);
      gdk_cairo_set_source_pixbuf (cr, pb, 0, 0);
      cairo_pattern_set_extend (cairo_get_source (cr), CAIRO_EXTEND_REPEAT);
      cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
      cairo_paint (cr);
      cairo_destroy (cr);
    }

    {
      cairo_t *cr = cairo_create (surface);
      double r = ((face->foreground >> 16) & 0xff) / 255.0;
      double g = ((face->foreground >>  8) & 0xff) / 255.0;
      double b = ((face->foreground >>  0) & 0xff) / 255.0;
      cairo_set_source_rgb (cr, r, g, b);
      cairo_mask_surface (cr, mask, 0, 0);
      cairo_destroy (cr);
    }

    cairo_surface_destroy (mask);
    g_object_unref (pb);
  }

  return surface;
}

static cairo_surface_t *
create_background_surface (struct glyph_string *s, int x, int y, int width, int height)
{
  return create_background_surface_by_face (s->f, s->face, x, y, width, height);
}

/* Draw the background of glyph_string S.  If S->background_filled_p
   is non-zero don't draw it.  FORCE_P non-zero means draw the
   background even if it wouldn't be drawn normally.  This is used
   when a string preceding S draws into the background of S, or S
   contains the first component of a composition.  */

static void
x_draw_glyph_string_background (struct glyph_string *s, bool force_p)
{
  PGTK_TRACE("x_draw_glyph_string_background: 0.");
  /* Nothing to do if background has already been drawn or if it
     shouldn't be drawn in the first place.  */
  if (!s->background_filled_p)
    {
      PGTK_TRACE("x_draw_glyph_string_background: 1.");
      int box_line_width = max (s->face->box_line_width, 0);

      PGTK_TRACE("x_draw_glyph_string_background: 2. %d, %d.",
		   FONT_HEIGHT (s->font), s->height - 2 * box_line_width);
      PGTK_TRACE("x_draw_glyph_string_background: 2. %d.", FONT_TOO_HIGH(s->font));
      PGTK_TRACE("x_draw_glyph_string_background: 2. %d.", s->font_not_found_p);
      PGTK_TRACE("x_draw_glyph_string_background: 2. %d.", s->extends_to_end_of_line_p);
      PGTK_TRACE("x_draw_glyph_string_background: 2. %d.", force_p);

      if (s->stippled_p)
	{
	  /* Fill background with a stipple pattern.  */

	  cairo_surface_t *bg = create_background_surface (s,
							   s->x, s->y + box_line_width,
							   s->background_width, s->height - 2 * box_line_width);

	  cairo_t *cr = pgtk_begin_cr_clip (s->f);
	  cairo_set_source_surface (cr, bg, s->x, s->y + box_line_width);
	  cairo_rectangle (cr,
			   s->x, s->y + box_line_width,
			   s->background_width, s->height - 2 * box_line_width);
	  cairo_fill (cr);
	  pgtk_end_cr_clip (s->f);

	  cairo_surface_destroy (bg);

	  s->background_filled_p = true;
	}
      else
	if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	       /* When xdisp.c ignores FONT_HEIGHT, we cannot trust
		  font dimensions, since the actual glyphs might be
		  much smaller.  So in that case we always clear the
		  rectangle with background color.  */
	       || FONT_TOO_HIGH (s->font)
	       || s->font_not_found_p
	       || s->extends_to_end_of_line_p
	       || force_p)
	{
	  PGTK_TRACE("x_draw_glyph_string_background: 3.");
	  x_clear_glyph_string_rect (s, s->x, s->y + box_line_width,
				     s->background_width,
				     s->height - 2 * box_line_width);
	  s->background_filled_p = true;
	}
    }
}


static void
pgtk_draw_rectangle (struct frame *f, unsigned long color, int x, int y, int width, int height)
{
  cairo_t *cr;

  cr = pgtk_begin_cr_clip (f);
  pgtk_set_cr_source_with_color (f, color);
  cairo_rectangle (cr, x + 0.5, y + 0.5, width, height);
  cairo_set_line_width (cr, 1);
  cairo_stroke (cr);
  pgtk_end_cr_clip (f);
}

/* Draw the foreground of glyph string S.  */

static void
x_draw_glyph_string_foreground (struct glyph_string *s)
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
	  pgtk_draw_rectangle (s->f,
				 s->face->foreground, x, s->y, g->pixel_width - 1,
				 s->height - 1);
	  x += g->pixel_width;
	}
    }
  else
    {
      struct font *font = s->font;
      int boff = font->baseline_offset;
      int y;

      if (font->vertical_centering)
	boff = VCENTER_BASELINE_OFFSET (font, s->f) - boff;

      y = s->ybase - boff;
      if (s->for_overlaps
	  || (s->background_filled_p && s->hl != DRAW_CURSOR))
	font->driver->draw (s, 0, s->nchars, x, y, false);
      else
	font->driver->draw (s, 0, s->nchars, x, y, true);
      if (s->face->overstrike)
	font->driver->draw (s, 0, s->nchars, x + 1, y, false);
    }
}

/* Draw the foreground of composite glyph string S.  */

static void
x_draw_composite_glyph_string_foreground (struct glyph_string *s)
{
  int i, j, x;
  struct font *font = s->font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + eabs (s->face->box_line_width);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->cmp_from is the index
     of the first character drawn for glyphs of this composition.
     S->cmp_from == 0 means we are drawing the very first character of
     this composition.  */

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->cmp_from == 0)
	pgtk_draw_rectangle (s->f, s->face->foreground, x, s->y,
			       s->width - 1, s->height - 1);
    }
  else if (! s->first_glyph->u.cmp.automatic)
    {
      int y = s->ybase;

      for (i = 0, j = s->cmp_from; i < s->nchars; i++, j++)
	/* TAB in a composition means display glyphs with padding
	   space on the left or right.  */
	if (COMPOSITION_GLYPH (s->cmp, j) != '\t')
	  {
	    int xx = x + s->cmp->offsets[j * 2];
	    int yy = y - s->cmp->offsets[j * 2 + 1];

	    font->driver->draw (s, j, j + 1, xx, yy, false);
	    if (s->face->overstrike)
	      font->driver->draw (s, j, j + 1, xx + 1, yy, false);
	  }
    }
  else
    {
      Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);
      Lisp_Object glyph;
      int y = s->ybase;
      int width = 0;

      for (i = j = s->cmp_from; i < s->cmp_to; i++)
	{
	  glyph = LGSTRING_GLYPH (gstring, i);
	  if (NILP (LGLYPH_ADJUSTMENT (glyph)))
	    width += LGLYPH_WIDTH (glyph);
	  else
	    {
	      int xoff, yoff, wadjust;

	      if (j < i)
		{
		  font->driver->draw (s, j, i, x, y, false);
		  if (s->face->overstrike)
		    font->driver->draw (s, j, i, x + 1, y, false);
		  x += width;
		}
	      xoff = LGLYPH_XOFF (glyph);
	      yoff = LGLYPH_YOFF (glyph);
	      wadjust = LGLYPH_WADJUST (glyph);
	      font->driver->draw (s, i, i + 1, x + xoff, y + yoff, false);
	      if (s->face->overstrike)
		font->driver->draw (s, i, i + 1, x + xoff + 1, y + yoff,
				    false);
	      x += wadjust;
	      j = i + 1;
	      width = 0;
	    }
	}
      if (j < i)
	{
	  font->driver->draw (s, j, i, x, y, false);
	  if (s->face->overstrike)
	    font->driver->draw (s, j, i, x + 1, y, false);
	}
    }
}


/* Draw the foreground of glyph string S for glyphless characters.  */

static void
x_draw_glyphless_glyph_string_foreground (struct glyph_string *s)
{
  struct glyph *glyph = s->first_glyph;
  unsigned char2b[8];
  int x, i, j;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + eabs (s->face->box_line_width);
  else
    x = s->x;

  s->char2b = char2b;

  for (i = 0; i < s->nchars; i++, glyph++)
    {
#ifdef GCC_LINT
      enum { PACIFY_GCC_BUG_81401 = 1 };
#else
      enum { PACIFY_GCC_BUG_81401 = 0 };
#endif
      char buf[7 + PACIFY_GCC_BUG_81401];
      char *str = NULL;
      int len = glyph->u.glyphless.len;

      if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_ACRONYM)
	{
	  if (len > 0
	      && CHAR_TABLE_P (Vglyphless_char_display)
	      && (CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (Vglyphless_char_display))
		  >= 1))
	    {
	      Lisp_Object acronym
		= (! glyph->u.glyphless.for_no_font
		   ? CHAR_TABLE_REF (Vglyphless_char_display,
				     glyph->u.glyphless.ch)
		   : XCHAR_TABLE (Vglyphless_char_display)->extras[0]);
	      if (STRINGP (acronym))
		str = SSDATA (acronym);
	    }
	}
      else if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_HEX_CODE)
	{
	  unsigned int ch = glyph->u.glyphless.ch;
	  eassume (ch <= MAX_CHAR);
	  sprintf (buf, "%0*X", ch < 0x10000 ? 4 : 6, ch);
	  str = buf;
	}

      if (str)
	{
	  int upper_len = (len + 1) / 2;

	  /* It is assured that all LEN characters in STR is ASCII.  */
	  for (j = 0; j < len; j++)
	    char2b[j] = s->font->driver->encode_char (s->font, str[j]) & 0xFFFF;
	  s->font->driver->draw (s, 0, upper_len,
				 x + glyph->slice.glyphless.upper_xoff,
				 s->ybase + glyph->slice.glyphless.upper_yoff,
				 false);
	  s->font->driver->draw (s, upper_len, len,
				 x + glyph->slice.glyphless.lower_xoff,
				 s->ybase + glyph->slice.glyphless.lower_yoff,
				 false);
	}
      if (glyph->u.glyphless.method != GLYPHLESS_DISPLAY_THIN_SPACE)
	pgtk_draw_rectangle (s->f, s->face->foreground,
			       x, s->ybase - glyph->ascent,
			       glyph->pixel_width - 1,
			       glyph->ascent + glyph->descent - 1);
      x += glyph->pixel_width;
   }
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

static bool
x_alloc_lighter_color (struct frame *f, unsigned long *pixel, double factor, int delta)
{
  Emacs_Color color, new;
  long bright;
  bool success_p;

  /* Get RGB color values.  */
  color.pixel = *pixel;
  pgtk_query_color (f, &color);

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  eassert (factor >= 0);
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
  new.pixel = new.red >> 8 << 16 | new.green >> 8 << 8 | new.blue >> 8;
  success_p = true;
  if (success_p)
    {
      if (new.pixel == *pixel)
	{
	  /* If we end up with the same color as before, try adding
	     delta to the RGB values.  */
	  new.red = min (0xffff, delta + color.red);
	  new.green = min (0xffff, delta + color.green);
	  new.blue = min (0xffff, delta + color.blue);
	  new.pixel = new.red >> 8 << 16 | new.green >> 8 << 8 | new.blue >> 8;
	  success_p = true;
	}
      else
	success_p = true;
      *pixel = new.pixel;
    }

  return success_p;
}

static void
x_fill_trapezoid_for_relief (struct frame *f, unsigned long color, int x, int y,
			     int width, int height, int top_p)
{
  cairo_t *cr;

  cr = pgtk_begin_cr_clip (f);
  pgtk_set_cr_source_with_color (f, color);
  cairo_move_to (cr, top_p ? x : x + height, y);
  cairo_line_to (cr, x, y + height);
  cairo_line_to (cr, top_p ? x + width - height : x + width, y + height);
  cairo_line_to (cr, x + width, y);
  cairo_fill (cr);
  pgtk_end_cr_clip (f);
}

enum corners
  {
    CORNER_BOTTOM_RIGHT,	/* 0 -> pi/2 */
    CORNER_BOTTOM_LEFT,		/* pi/2 -> pi */
    CORNER_TOP_LEFT,		/* pi -> 3pi/2 */
    CORNER_TOP_RIGHT,		/* 3pi/2 -> 2pi */
    CORNER_LAST
  };

static void
x_erase_corners_for_relief (struct frame *f, unsigned long color, int x, int y,
			    int width, int height,
			    double radius, double margin, int corners)
{
  cairo_t *cr;
  int i;

  cr = pgtk_begin_cr_clip (f);
  pgtk_set_cr_source_with_color (f, color);
  for (i = 0; i < CORNER_LAST; i++)
    if (corners & (1 << i))
      {
	double xm, ym, xc, yc;

	if (i == CORNER_TOP_LEFT || i == CORNER_BOTTOM_LEFT)
	  xm = x - margin, xc = xm + radius;
	else
	  xm = x + width + margin, xc = xm - radius;
	if (i == CORNER_TOP_LEFT || i == CORNER_TOP_RIGHT)
	  ym = y - margin, yc = ym + radius;
	else
	  ym = y + height + margin, yc = ym - radius;

	cairo_move_to (cr, xm, ym);
	cairo_arc (cr, xc, yc, radius, i * M_PI_2, (i + 1) * M_PI_2);
      }
  cairo_clip (cr);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
  pgtk_end_cr_clip (f);
}

/* Set up the foreground color for drawing relief lines of glyph
   string S.  RELIEF is a pointer to a struct relief containing the GC
   with which lines will be drawn.  Use a color that is FACTOR or
   DELTA lighter or darker than the relief's background which is found
   in S->f->output_data.pgtk->relief_background.  If such a color cannot
   be allocated, use DEFAULT_PIXEL, instead.  */

static void
x_setup_relief_color (struct frame *f, struct relief *relief, double factor,
		      int delta, unsigned long default_pixel)
{
  Emacs_GC xgcv;
  struct pgtk_output *di = FRAME_X_OUTPUT(f);
  unsigned long pixel;
  unsigned long background = di->relief_background;

  /* Allocate new color.  */
  xgcv.foreground = default_pixel;
  pixel = background;
  if (x_alloc_lighter_color (f, &pixel, factor, delta))
    xgcv.foreground = relief->pixel = pixel;

  relief->xgcv = xgcv;
}

/* Set up colors for the relief lines around glyph string S.  */

static void
x_setup_relief_colors (struct glyph_string *s)
{
  struct pgtk_output *di = FRAME_X_OUTPUT(s->f);
  unsigned long color;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
    color = IMAGE_BACKGROUND (s->img, s->f, 0);
  else
    {
      /* Get the background color of the face.  */
      color = s->xgcv.background;
    }

  if (TRUE)
    {
      di->relief_background = color;
      x_setup_relief_color (s->f, &di->white_relief, 1.2, 0x8000,
			    WHITE_PIX_DEFAULT (s->f));
      x_setup_relief_color (s->f, &di->black_relief, 0.6, 0x4000,
			    BLACK_PIX_DEFAULT (s->f));
    }
}


static void
x_set_clip_rectangles (struct frame *f, cairo_t *cr, XRectangle *rectangles, int n)
{
  if (n > 0) {
    for (int i = 0; i < n; i++) {
      cairo_rectangle(cr,
		      rectangles[i].x,
		      rectangles[i].y,
		      rectangles[i].width,
		      rectangles[i].height);
    }
    cairo_clip(cr);
  }
}

/* Draw a relief on frame F inside the rectangle given by LEFT_X,
   TOP_Y, RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the relief
   to draw, it must be >= 0.  RAISED_P means draw a raised
   relief.  LEFT_P means draw a relief on the left side of
   the rectangle.  RIGHT_P means draw a relief on the right
   side of the rectangle.  CLIP_RECT is the clipping rectangle to use
   when drawing.  */

static void
x_draw_relief_rect (struct frame *f,
		    int left_x, int top_y, int right_x, int bottom_y,
		    int width, bool raised_p, bool top_p, bool bot_p,
		    bool left_p, bool right_p,
		    XRectangle *clip_rect)
{
  unsigned long top_left_color, bottom_right_color;
  int corners = 0;

  cairo_t *cr = pgtk_begin_cr_clip(f);

  if (raised_p)
    {
      top_left_color = FRAME_X_OUTPUT(f)->white_relief.xgcv.foreground;
      bottom_right_color = FRAME_X_OUTPUT(f)->black_relief.xgcv.foreground;
    }
  else
    {
      top_left_color = FRAME_X_OUTPUT(f)->black_relief.xgcv.foreground;
      bottom_right_color = FRAME_X_OUTPUT(f)->white_relief.xgcv.foreground;
    }

  x_set_clip_rectangles (f, cr, clip_rect, 1);

  if (left_p)
    {
      pgtk_fill_rectangle (f, top_left_color, left_x, top_y,
			     width, bottom_y + 1 - top_y);
      if (top_p)
	corners |= 1 << CORNER_TOP_LEFT;
      if (bot_p)
	corners |= 1 << CORNER_BOTTOM_LEFT;
    }
  if (right_p)
    {
      pgtk_fill_rectangle (f, bottom_right_color, right_x + 1 - width, top_y,
			     width, bottom_y + 1 - top_y);
      if (top_p)
	corners |= 1 << CORNER_TOP_RIGHT;
      if (bot_p)
	corners |= 1 << CORNER_BOTTOM_RIGHT;
    }
  if (top_p)
    {
      if (!right_p)
	pgtk_fill_rectangle (f, top_left_color, left_x, top_y,
			       right_x + 1 - left_x, width);
      else
	x_fill_trapezoid_for_relief (f, top_left_color, left_x, top_y,
				     right_x + 1 - left_x, width, 1);
    }
  if (bot_p)
    {
      if (!left_p)
	pgtk_fill_rectangle (f, bottom_right_color, left_x, bottom_y + 1 - width,
			       right_x + 1 - left_x, width);
      else
	x_fill_trapezoid_for_relief (f, bottom_right_color,
				     left_x, bottom_y + 1 - width,
				     right_x + 1 - left_x, width, 0);
    }
  if (left_p && width != 1)
    pgtk_fill_rectangle (f, bottom_right_color, left_x, top_y,
			   1, bottom_y + 1 - top_y);
  if (top_p && width != 1)
    pgtk_fill_rectangle (f, bottom_right_color, left_x, top_y,
			   right_x + 1 - left_x, 1);
  if (corners)
    {
      x_erase_corners_for_relief (f, FRAME_BACKGROUND_PIXEL (f), left_x, top_y,
				  right_x - left_x + 1, bottom_y - top_y + 1,
				  6, 1, corners);
    }

  pgtk_end_cr_clip(f);
}

/* Draw a box on frame F inside the rectangle given by LEFT_X, TOP_Y,
   RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the lines to
   draw, it must be >= 0.  LEFT_P means draw a line on the
   left side of the rectangle.  RIGHT_P means draw a line
   on the right side of the rectangle.  CLIP_RECT is the clipping
   rectangle to use when drawing.  */

static void
x_draw_box_rect (struct glyph_string *s,
		 int left_x, int top_y, int right_x, int bottom_y, int width,
		 bool left_p, bool right_p, XRectangle *clip_rect)
{
  unsigned long foreground_backup;

  cairo_t *cr = pgtk_begin_cr_clip(s->f);

  foreground_backup = s->xgcv.foreground;
  s->xgcv.foreground = s->face->box_color;

  x_set_clip_rectangles (s->f, cr, clip_rect, 1);

  /* Top.  */
  pgtk_fill_rectangle (s->f, s->xgcv.foreground,
			 left_x, top_y, right_x - left_x + 1, width);

  /* Left.  */
  if (left_p)
    pgtk_fill_rectangle (s->f, s->xgcv.foreground,
			   left_x, top_y, width, bottom_y - top_y + 1);

  /* Bottom.  */
  pgtk_fill_rectangle (s->f, s->xgcv.foreground,
			 left_x, bottom_y - width + 1, right_x - left_x + 1, width);

  /* Right.  */
  if (right_p)
    pgtk_fill_rectangle (s->f, s->xgcv.foreground,
			   right_x - width + 1, top_y, width, bottom_y - top_y + 1);

  s->xgcv.foreground = foreground_backup;

  pgtk_end_cr_clip(s->f);
}


/* Draw a box around glyph string S.  */

static void
x_draw_glyph_string_box (struct glyph_string *s)
{
  int width, left_x, right_x, top_y, bottom_y, last_x;
  bool raised_p, left_p, right_p;
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
			  width, raised_p, true, true, left_p, right_p,
			  &clip_rect);
    }
}

static void
x_get_scale_factor(int *scale_x, int *scale_y)
{
  *scale_x = *scale_y = 1;
}

static void
x_draw_horizontal_wave (struct frame *f, unsigned long color, int x, int y,
			int width, int height, int wave_length)
{
  cairo_t *cr;
  double dx = wave_length, dy = height - 1;
  int xoffset, n;

  cr = pgtk_begin_cr_clip (f);
  pgtk_set_cr_source_with_color (f, color);
  cairo_rectangle (cr, x, y, width, height);
  cairo_clip (cr);

  if (x >= 0)
    {
      xoffset = x % (wave_length * 2);
      if (xoffset == 0)
	xoffset = wave_length * 2;
    }
  else
    xoffset = x % (wave_length * 2) + wave_length * 2;
  n = (width + xoffset) / wave_length + 1;
  if (xoffset > wave_length)
    {
      xoffset -= wave_length;
      --n;
      y += height - 1;
      dy = -dy;
    }

  cairo_move_to (cr, x - xoffset + 0.5, y + 0.5);
  while (--n >= 0)
    {
      cairo_rel_line_to (cr, dx, dy);
      dy = -dy;
    }
  cairo_set_line_width (cr, 1);
  cairo_stroke (cr);
  pgtk_end_cr_clip (f);
}

/*
   Draw a wavy line under S. The wave fills wave_height pixels from y0.

                    x0         wave_length = 2
                                 --
                y0   *   *   *   *   *
                     |* * * * * * * * *
    wave_height = 3  | *   *   *   *

*/
static void
x_draw_underwave (struct glyph_string *s, unsigned long color)
{
  /* Adjust for scale/HiDPI.  */
  int scale_x, scale_y;

  x_get_scale_factor (&scale_x, &scale_y);

  int wave_height = 3 * scale_y, wave_length = 2 * scale_x;

  x_draw_horizontal_wave (s->f, color, s->x, s->ybase - wave_height + 3,
			  s->width, wave_height, wave_length);
}

/* Draw a relief around the image glyph string S.  */

static void
x_draw_image_relief (struct glyph_string *s)
{
  int x1, y1, thick;
  bool raised_p, top_p, bot_p, left_p, right_p;
  int extra_x, extra_y;
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

  x1 = x + s->slice.width - 1;
  y1 = y + s->slice.height - 1;

  extra_x = extra_y = 0;
  if (s->face->id == TOOL_BAR_FACE_ID)
    {
      if (CONSP (Vtool_bar_button_margin)
	  && INTEGERP (XCAR (Vtool_bar_button_margin))
	  && INTEGERP (XCDR (Vtool_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtool_bar_button_margin));
	  extra_y = XFIXNUM (XCDR (Vtool_bar_button_margin));
	}
      else if (INTEGERP (Vtool_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtool_bar_button_margin);
    }

  top_p = bot_p = left_p = right_p = false;

  if (s->slice.x == 0)
    x -= thick + extra_x, left_p = true;
  if (s->slice.y == 0)
    y -= thick + extra_y, top_p = true;
  if (s->slice.x + s->slice.width == s->img->width)
    x1 += thick + extra_x, right_p = true;
  if (s->slice.y + s->slice.height == s->img->height)
    y1 += thick + extra_y, bot_p = true;

  x_setup_relief_colors (s);
  get_glyph_string_clip_rect (s, &r);
  x_draw_relief_rect (s->f, x, y, x1, y1, thick, raised_p,
		      top_p, bot_p, left_p, right_p, &r);
}

/* Draw part of the background of glyph string S.  X, Y, W, and H
   give the rectangle to draw.  */

static void
x_draw_glyph_string_bg_rect (struct glyph_string *s, int x, int y, int w, int h)
{
  if (s->stippled_p)
    {
      /* Fill background with a stipple pattern.  */

      cairo_surface_t *bg = create_background_surface (s, x, y, w, h);

      cairo_t *cr = pgtk_begin_cr_clip (s->f);
      cairo_set_source_surface (cr, bg, x, y);
      cairo_rectangle (cr, x, y, w, h);
      cairo_fill (cr);
      pgtk_end_cr_clip (s->f);

      cairo_surface_destroy (bg);
    }
  else
    x_clear_glyph_string_rect (s, x, y, w, h);
}

/* Draw foreground of image glyph string S.  */

static void
x_draw_image_foreground (struct glyph_string *s)
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

  /* Draw a rectangle if image could not be loaded.  */
  pgtk_draw_rectangle (s->f, s->xgcv.foreground, x, y,
		       s->slice.width - 1, s->slice.height - 1);
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
x_draw_image_glyph_string (struct glyph_string *s)
{
  int box_line_hwidth = eabs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height;

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
      || s->stippled_p
      || s->width != s->background_width)
    {
      if (s->img->mask)
	{
	  cairo_surface_t *bg = create_background_surface (s, s->x, s->y, s->background_width, s->height);

	  cairo_t *cr = pgtk_begin_cr_clip (s->f);
	  cairo_set_source_surface (cr, bg, s->x, s->y);
	  cairo_rectangle (cr, s->x, s->y, s->background_width, s->height);
	  cairo_fill (cr);
	  pgtk_end_cr_clip (s->f);

	  cairo_surface_destroy (bg);
	}
      else
	{
	  int x = s->x;
	  int y = s->y;
	  int width = s->background_width;

	  if (s->first_glyph->left_box_line_p
	      && s->slice.x == 0)
	    {
	      x += box_line_hwidth;
	      width -= box_line_hwidth;
	    }

	  if (s->slice.y == 0)
	    y += box_line_vwidth;

	  cairo_surface_t *bg = create_background_surface (s, x, y, width, height);

	  cairo_t *cr = pgtk_begin_cr_clip (s->f);
	  cairo_set_source_surface (cr, bg, x, y);
	  cairo_rectangle (cr, x, y, width, height);
	  cairo_fill (cr);
	  pgtk_end_cr_clip (s->f);

	  cairo_surface_destroy (bg);
	}

      s->background_filled_p = true;
    }

  /* Draw the foreground.  */
  if (s->img->cr_data)
    {
      cairo_t *cr = pgtk_begin_cr_clip (s->f);

      int x = s->x + s->img->hmargin;
      int y = s->y + s->img->vmargin;
      int width = s->background_width;

      cairo_translate (cr, x - s->slice.x, y - s->slice.y);
      cairo_set_source (cr, s->img->cr_data);
      cairo_rectangle (cr, 0, 0, width, height);
      cairo_fill (cr);
      pgtk_end_cr_clip (s->f);
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
x_draw_stretch_glyph_string (struct glyph_string *s)
{
  eassert (s->first_glyph->type == STRETCH_GLYPH);

  if (s->hl == DRAW_CURSOR
      && !x_stretch_cursor_p)
    {
      /* If `x-stretch-cursor' is nil, don't draw a block cursor as
	 wide as the stretch glyph.  */
      int width, background_width = s->background_width;
      int x = s->x;

      if (!s->row->reversed_p)
	{
	  int left_x = window_box_left_offset (s->w, TEXT_AREA);

	  if (x < left_x)
	    {
	      background_width -= left_x - x;
	      x = left_x;
	    }
	}
      else
	{
	  /* In R2L rows, draw the cursor on the right edge of the
	     stretch glyph.  */
	  int right_x = window_box_right (s->w, TEXT_AREA);

	  if (x + background_width > right_x)
	    background_width -= x - right_x;
	  x += background_width;
	}
      width = min (FRAME_COLUMN_WIDTH (s->f), background_width);
      if (s->row->reversed_p)
	x -= width;

      /* Draw cursor.  */
      x_draw_glyph_string_bg_rect (s, x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < background_width)
	{
	  int y = s->y;
	  int w = background_width - width, h = s->height;
	  XRectangle r;
	  unsigned long color;

	  if (!s->row->reversed_p)
	    x += width;
	  else
	    x = s->x;
	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      x_set_mouse_face_gc (s);
	      color = s->xgcv.foreground;
	    }
	  else
	    color = s->face->foreground;

	  cairo_t *cr = pgtk_begin_cr_clip(s->f);

	  get_glyph_string_clip_rect (s, &r);
	  x_set_clip_rectangles (s->f, cr, &r, 1);

	  if (s->face->stipple)
	    {
	      /* Fill background with a stipple pattern.  */
	      cairo_surface_t *bg = create_background_surface (s, x, y, w, h);
	      cairo_t *cr = pgtk_begin_cr_clip (s->f);
	      cairo_set_source_surface (cr, bg, x, y);
	      cairo_rectangle (cr, x, y, w, h);
	      cairo_fill (cr);
	      pgtk_end_cr_clip (s->f);
	      cairo_surface_destroy (bg);
	    }
	  else
	    {
	      pgtk_fill_rectangle(s->f, color, x, y, w, h);
	    }

	  pgtk_end_cr_clip(s->f);
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

  s->background_filled_p = true;
}

static void pgtk_draw_glyph_string(struct glyph_string *s)
{
  PGTK_TRACE("draw_glyph_string.");
  PGTK_TRACE("draw_glyph_string: x=%d, y=%d, width=%d, height=%d.",
	       s->x, s->y, s->width, s->height);

  bool relief_drawn_p = false;

  /* If S draws into the background of its successors, draw the
     background of the successors first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
	  {
	    cairo_t *cr = pgtk_begin_cr_clip(next->f);
	    PGTK_TRACE("pgtk_draw_glyph_string: 1.");
	    x_set_glyph_string_gc (next);
	    x_set_glyph_string_clipping (next, cr);
	    if (next->first_glyph->type == STRETCH_GLYPH)
	      x_draw_stretch_glyph_string (next);
	    else
	      x_draw_glyph_string_background (next, true);
	    next->num_clips = 0;
	    pgtk_end_cr_clip(next->f);
	  }
    }

  /* Set up S->gc, set clipping and draw S.  */
  PGTK_TRACE("pgtk_draw_glyph_string: 2.");
  x_set_glyph_string_gc (s);

  cairo_t *cr = pgtk_begin_cr_clip(s->f);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps
      && s->face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))

    {
      PGTK_TRACE("pgtk_draw_glyph_string: 2.1.");
      x_set_glyph_string_clipping (s, cr);
      x_draw_glyph_string_background (s, true);
      x_draw_glyph_string_box (s);
      x_set_glyph_string_clipping (s, cr);
      relief_drawn_p = true;
    }
  else if (!s->clip_head /* draw_glyphs didn't specify a clip mask. */
	   && !s->clip_tail
	   && ((s->prev && s->prev->hl != s->hl && s->left_overhang)
	       || (s->next && s->next->hl != s->hl && s->right_overhang)))
    /* We must clip just this glyph.  left_overhang part has already
       drawn when s->prev was drawn, and right_overhang part will be
       drawn later when s->next is drawn. */
    PGTK_TRACE("pgtk_draw_glyph_string: 2.2."),
    x_set_glyph_string_clipping_exactly (s, s, cr);
  else
    PGTK_TRACE("pgtk_draw_glyph_string: 2.3."),
    x_set_glyph_string_clipping (s, cr);

  switch (s->first_glyph->type)
    {
    case IMAGE_GLYPH:
      PGTK_TRACE("pgtk_draw_glyph_string: 2.4.");
      x_draw_image_glyph_string (s);
      break;

    case XWIDGET_GLYPH:
      PGTK_TRACE("pgtk_draw_glyph_string: 2.5.");
      x_draw_xwidget_glyph_string (s);
      break;

    case STRETCH_GLYPH:
      PGTK_TRACE("pgtk_draw_glyph_string: 2.6.");
      x_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
      PGTK_TRACE("pgtk_draw_glyph_string: 2.7.");
      if (s->for_overlaps)
	s->background_filled_p = true;
      else
	x_draw_glyph_string_background (s, false);
      x_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      PGTK_TRACE("pgtk_draw_glyph_string: 2.8.");
      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
	s->background_filled_p = true;
      else
	x_draw_glyph_string_background (s, true);
      x_draw_composite_glyph_string_foreground (s);
      break;

    case GLYPHLESS_GLYPH:
      PGTK_TRACE("pgtk_draw_glyph_string: 2.9.");
      if (s->for_overlaps)
	s->background_filled_p = true;
      else
	x_draw_glyph_string_background (s, true);
      x_draw_glyphless_glyph_string_foreground (s);
      break;

    default:
      emacs_abort ();
    }

  if (!s->for_overlaps)
    {
      /* Draw underline.  */
      if (s->face->underline)
	{
	  if (s->face->underline == FACE_UNDER_WAVE)
	    {
	      if (s->face->underline_defaulted_p)
		x_draw_underwave (s, s->xgcv.foreground);
	      else
		{
		  x_draw_underwave (s, s->face->underline_color);
		}
	    }
	  else if (s->face->underline == FACE_UNDER_LINE)
	    {
	      unsigned long thickness, position;
	      int y;

	      if (s->prev && s->prev->face->underline
		  && s->prev->face->underline == FACE_UNDER_LINE)
		{
		  /* We use the same underline style as the previous one.  */
		  thickness = s->prev->underline_thickness;
		  position = s->prev->underline_position;
		}
	      else
		{
		  struct font *font = font_for_underline_metrics (s);

                  /* Get the underline thickness.  Default is 1 pixel.  */
                  if (font && font->underline_thickness > 0)
                    thickness = font->underline_thickness;
                  else
                    thickness = 1;
                  if (x_underline_at_descent_line)
                    position = (s->height - thickness) - (s->ybase - s->y);
                  else
                    {
                      /* Get the underline position.  This is the recommended
                         vertical offset in pixels from the baseline to the top of
                         the underline.  This is a signed value according to the
                         specs, and its default is

                         ROUND ((maximum descent) / 2), with
                         ROUND(x) = floor (x + 0.5)  */

                      if (x_use_underline_position_properties
                          && font && font->underline_position >= 0)
                        position = font->underline_position;
                      else if (font)
                        position = (font->descent + 1) / 2;
                      else
                        position = underline_minimum_offset;
                    }
                  position = max (position, underline_minimum_offset);
                }
              /* Check the sanity of thickness and position.  We should
                 avoid drawing underline out of the current line area.  */
              if (s->y + s->height <= s->ybase + position)
                position = (s->height - 1) - (s->ybase - s->y);
              if (s->y + s->height < s->ybase + position + thickness)
                thickness = (s->y + s->height) - (s->ybase + position);
              s->underline_thickness = thickness;
              s->underline_position = position;
              y = s->ybase + position;
              if (s->face->underline_defaulted_p)
                pgtk_fill_rectangle (s->f, s->xgcv.foreground,
				       s->x, y, s->width, thickness);
              else
                {
                  pgtk_fill_rectangle (s->f, s->face->underline_color,
					 s->x, y, s->width, thickness);
                }
            }
        }
      /* Draw overline.  */
      if (s->face->overline_p)
	{
	  unsigned long dy = 0, h = 1;

	  if (s->face->overline_color_defaulted_p)
	    pgtk_fill_rectangle (s->f, s->xgcv.foreground, s->x, s->y + dy,
				   s->width, h);
	  else
	    {
	      pgtk_fill_rectangle (s->f, s->face->overline_color, s->x, s->y + dy,
				     s->width, h);
	    }
	}

      /* Draw strike-through.  */
      if (s->face->strike_through_p)
	{
	  /* Y-coordinate and height of the glyph string's first
	     glyph.  We cannot use s->y and s->height because those
	     could be larger if there are taller display elements
	     (e.g., characters displayed with a larger font) in the
	     same glyph row.  */
	  int glyph_y = s->ybase - s->first_glyph->ascent;
	  int glyph_height = s->first_glyph->ascent + s->first_glyph->descent;
	  /* Strike-through width and offset from the glyph string's
	     top edge.  */
          unsigned long h = 1;
          unsigned long dy = (glyph_height - h) / 2;

	  if (s->face->strike_through_color_defaulted_p)
	    pgtk_fill_rectangle (s->f, s->xgcv.foreground, s->x, glyph_y + dy,
				   s->width, h);
	  else
	    {
	      pgtk_fill_rectangle (s->f, s->face->strike_through_color, s->x, glyph_y + dy,
				     s->width, h);
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
		PGTK_TRACE("pgtk_draw_glyph_string: 3.");
		x_set_glyph_string_gc (prev);
		cairo_save(cr);
		x_set_glyph_string_clipping_exactly (s, prev, cr);
		if (prev->first_glyph->type == CHAR_GLYPH)
		  x_draw_glyph_string_foreground (prev);
		else
		  x_draw_composite_glyph_string_foreground (prev);
		prev->hl = save;
		prev->num_clips = 0;
		cairo_restore(cr);
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
		PGTK_TRACE("pgtk_draw_glyph_string: 4.");
		x_set_glyph_string_gc (next);
		cairo_save(cr);
		x_set_glyph_string_clipping_exactly (s, next, cr);
		if (next->first_glyph->type == CHAR_GLYPH)
		  x_draw_glyph_string_foreground (next);
		else
		  x_draw_composite_glyph_string_foreground (next);
		cairo_restore(cr);
		next->hl = save;
		next->num_clips = 0;
		next->clip_head = s->next;
	      }
	}
    }

  /* Reset clipping.  */
  pgtk_end_cr_clip(s->f);
  s->num_clips = 0;
}

/* RIF: Define cursor CURSOR on frame F.  */

static void
pgtk_define_frame_cursor (struct frame *f, Emacs_Cursor cursor)
{
  if (!f->pointer_invisible
      && FRAME_X_OUTPUT(f)->current_cursor != cursor)
    gdk_window_set_cursor(gtk_widget_get_window(FRAME_GTK_WIDGET(f)), cursor);
  FRAME_X_OUTPUT(f)->current_cursor = cursor;
}

static void pgtk_after_update_window_line(struct window *w, struct glyph_row *desired_row)
{
  PGTK_TRACE("after_update_window_line.");

  struct frame *f;
  int width, height;

  /* begin copy from other terms */
  eassert (w);

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = 1;

  /* When a window has disappeared, make sure that no rest of
     full-width rows stays visible in the internal border.  */
  if (windows_or_buffers_changed
      && desired_row->full_width_p
      && (f = XFRAME (w->frame),
	  width = FRAME_INTERNAL_BORDER_WIDTH (f),
	  width != 0)
      && (height = desired_row->visible_height,
	  height > 0))
    {
      int y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, desired_row->y));

      block_input ();
      pgtk_clear_frame_area (f, 0, y, width, height);
      pgtk_clear_frame_area (f,
			       FRAME_PIXEL_WIDTH (f) - width,
			       y, width, height);
      unblock_input ();
    }
}

static void pgtk_clear_frame_area(struct frame *f, int x, int y, int width, int height)
{
  PGTK_TRACE("clear_frame_area.");
  pgtk_clear_area (f, x, y, width, height);
}

/* Draw a hollow box cursor on window W in glyph row ROW.  */

static void
x_draw_hollow_cursor (struct window *w, struct glyph_row *row)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  int x, y, wd, h;
  struct glyph *cursor_glyph;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute frame-relative coordinates for phys cursor.  */
  get_phys_cursor_geometry (w, row, cursor_glyph, &x, &y, &h);
  wd = w->phys_cursor_width - 1;

  /* The foreground of cursor_gc is typically the same as the normal
     background color, which can cause the cursor box to be invisible.  */
  cairo_t *cr = pgtk_begin_cr_clip(f);
  pgtk_set_cr_source_with_color(f, FRAME_X_OUTPUT(f)->cursor_color);

  /* When on R2L character, show cursor at the right edge of the
     glyph, unless the cursor box is as wide as the glyph or wider
     (the latter happens when x-stretch-cursor is non-nil).  */
  if ((cursor_glyph->resolved_level & 1) != 0
      && cursor_glyph->pixel_width > wd)
    {
      x += cursor_glyph->pixel_width - wd;
      if (wd > 0)
	wd -= 1;
    }
  /* Set clipping, draw the rectangle, and reset clipping again.  */
  pgtk_clip_to_row (w, row, TEXT_AREA, cr);
  pgtk_draw_rectangle (f, FRAME_X_OUTPUT(f)->cursor_color, x, y, wd, h - 1);
  pgtk_end_cr_clip(f);
}

/* Draw a bar cursor on window W in glyph row ROW.

   Implementation note: One would like to draw a bar cursor with an
   angle equal to the one given by the font property XA_ITALIC_ANGLE.
   Unfortunately, I didn't find a font yet that has this property set.
   --gerd.  */

static void
x_draw_bar_cursor (struct window *w, struct glyph_row *row, int width, enum text_cursor_kinds kind)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph *cursor_glyph;

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Experimental avoidance of cursor on xwidget.  */
  if (cursor_glyph->type == XWIDGET_GLYPH)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      struct glyph_row *r;
      r = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      draw_phys_cursor_glyph (w, r, DRAW_CURSOR);
    }
  else
    {
      struct face *face = FACE_FROM_ID (f, cursor_glyph->face_id);
      unsigned long color;

      cairo_t *cr = pgtk_begin_cr_clip(f);

      /* If the glyph's background equals the color we normally draw
	 the bars cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == FRAME_X_OUTPUT(f)->cursor_color)
	color = face->foreground;
      else
	color = FRAME_X_OUTPUT(f)->cursor_color;

      pgtk_clip_to_row (w, row, TEXT_AREA, cr);

      if (kind == BAR_CURSOR)
	{
	  int x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

	  if (width < 0)
	    width = FRAME_CURSOR_WIDTH (f);
	  width = min (cursor_glyph->pixel_width, width);

	  w->phys_cursor_width = width;

	  /* If the character under cursor is R2L, draw the bar cursor
	     on the right of its glyph, rather than on the left.  */
	  if ((cursor_glyph->resolved_level & 1) != 0)
	    x += cursor_glyph->pixel_width - width;

	  pgtk_fill_rectangle (f, color, x,
				 WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
				 width, row->height);
	}
      else /* HBAR_CURSOR */
	{
	  int dummy_x, dummy_y, dummy_h;
	  int x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

	  if (width < 0)
	    width = row->height;

	  width = min (row->height, width);

	  get_phys_cursor_geometry (w, row, cursor_glyph, &dummy_x,
				    &dummy_y, &dummy_h);

	  if ((cursor_glyph->resolved_level & 1) != 0
	      && cursor_glyph->pixel_width > w->phys_cursor_width - 1)
	    x += cursor_glyph->pixel_width - w->phys_cursor_width + 1;
	  pgtk_fill_rectangle (f, color, x,
				 WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y +
							  row->height - width),
				 w->phys_cursor_width - 1, width);
	}

      pgtk_end_cr_clip(f);
    }
}

/* RIF: Draw cursor on window W.  */

static void
pgtk_draw_window_cursor (struct window *w, struct glyph_row *glyph_row, int x,
		      int y, enum text_cursor_kinds cursor_type,
		      int cursor_width, bool on_p, bool active_p)
{
  PGTK_TRACE("draw_window_cursor: %d, %d, %d, %d, %d, %d.",
	       x, y, cursor_type, cursor_width, on_p, active_p);
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  PGTK_TRACE("%p\n", f->output_data.pgtk);

  if (on_p)
    {
      w->phys_cursor_type = cursor_type;
      w->phys_cursor_on_p = true;

      if (glyph_row->exact_window_width_line_p
	  && (glyph_row->reversed_p
	      ? (w->phys_cursor.hpos < 0)
	      : (w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])))
	{
	  glyph_row->cursor_in_fringe_p = true;
	  draw_fringe_bitmap (w, glyph_row, glyph_row->reversed_p);
	}
      else
	{
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
	      emacs_abort ();
	    }
	}

#ifdef HAVE_X_I18N
      if (w == XWINDOW (f->selected_window))
	if (FRAME_XIC (f) && (FRAME_XIC_STYLE (f) & XIMPreeditPosition))
	  xic_set_preeditarea (w, x, y);
#endif
    }

  gtk_widget_queue_draw(FRAME_GTK_WIDGET(f));
}

static void
pgtk_copy_bits (struct frame *f, cairo_rectangle_t *src_rect, cairo_rectangle_t *dst_rect)
{
  PGTK_TRACE ("pgtk_copy_bits: %dx%d+%d+%d -> %dx%d+%d+%d",
	      (int) src_rect->width,
	      (int) src_rect->height,
	      (int) src_rect->x,
	      (int) src_rect->y,
	      (int) dst_rect->width,
	      (int) dst_rect->height,
	      (int) dst_rect->x,
	      (int) dst_rect->y);

  cairo_t *cr;
  cairo_surface_t *surface;  /* temporary surface */

  surface = cairo_surface_create_similar(FRAME_CR_SURFACE(f), CAIRO_CONTENT_COLOR_ALPHA,
					 (int) src_rect->width,
					 (int) src_rect->height);

  cr = cairo_create(surface);
  cairo_set_source_surface(cr, FRAME_CR_SURFACE(f), -src_rect->x, -src_rect->y);
  cairo_rectangle(cr, 0, 0, src_rect->width, src_rect->height);
  cairo_clip(cr);
  cairo_paint(cr);
  cairo_destroy(cr);

  cr = pgtk_begin_cr_clip(f);
  cairo_set_source_surface(cr, surface, dst_rect->x, dst_rect->y);
  cairo_rectangle(cr, dst_rect->x, dst_rect->y, dst_rect->width, dst_rect->height);
  cairo_clip(cr);
  cairo_paint(cr);
  pgtk_end_cr_clip(f);

  cairo_surface_destroy(surface);
}

/* Scroll part of the display as described by RUN.  */

static void
pgtk_scroll_run (struct window *w, struct run *run)
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;

  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the left and right
     fringe of W.  */
  window_box (w, ANY_AREA, &x, &y, &width, &height);

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
      /* Scrolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
    }

  block_input ();

  /* Cursor off.  Will be switched on again in x_update_window_end.  */
  gui_clear_cursor (w);

  {
    cairo_rectangle_t src_rect = { x, from_y, width, height };
    cairo_rectangle_t dst_rect = { x, to_y, width, height };
    pgtk_copy_bits (f, &src_rect , &dst_rect);
  }

  unblock_input ();
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
pgtk_update_begin (struct frame *f)
{
  if (! NILP (tip_frame) && XFRAME (tip_frame) == f
      && ! FRAME_VISIBLE_P (f))
    return;

  if (! FRAME_CR_SURFACE (f))
    {
      int width, height;
      if (FRAME_GTK_WIDGET (f))
	{
	  GdkWindow *w = gtk_widget_get_window (FRAME_GTK_WIDGET (f));
	  width = gdk_window_get_width (w);
	  height = gdk_window_get_height (w);
	}
      else
	{
	  width = FRAME_PIXEL_WIDTH (f);
	  height = FRAME_PIXEL_HEIGHT (f);
	  if (! FRAME_EXTERNAL_TOOL_BAR (f))
	    height += FRAME_TOOL_BAR_HEIGHT (f);
	  if (! FRAME_EXTERNAL_MENU_BAR (f))
	    height += FRAME_MENU_BAR_HEIGHT (f);
	}

      if (width > 0 && height > 0)
	{
	  block_input();
	  FRAME_CR_SURFACE (f) = cairo_image_surface_create
	    (CAIRO_FORMAT_ARGB32, width, height);
	  unblock_input();
	}
    }

  pgtk_clear_under_internal_border (f);
}

/* Start update of window W.  */

static void
pgtk_update_window_begin (struct window *w)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);

  w->output_cursor = w->cursor;

  block_input ();

  if (f == hlinfo->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      hlinfo->mouse_face_defer = true;

      /* If F needs to be redrawn, simply forget about any prior mouse
	 highlighting.  */
      if (FRAME_GARBAGED_P (f))
	hlinfo->mouse_face_window = Qnil;
    }

  unblock_input ();
}


/* Draw a vertical window border from (x,y0) to (x,y1)  */

static void
pgtk_draw_vertical_window_border (struct window *w, int x, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;
  cairo_t *cr;

  cr = pgtk_begin_cr_clip (f);

  face = FACE_FROM_ID_OR_NULL (f, VERTICAL_BORDER_FACE_ID);
  if (face)
    pgtk_set_cr_source_with_color (f, face->foreground);

  cairo_rectangle (cr, x, y0, 1, y1 - y0);
  cairo_fill (cr);

  pgtk_end_cr_clip (f);
}

/* Draw a window divider from (x0,y0) to (x1,y1)  */

static void
pgtk_draw_window_divider (struct window *w, int x0, int x1, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_FACE_ID);
  struct face *face_first
    = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID);
  struct face *face_last
    = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_LAST_PIXEL_FACE_ID);
  unsigned long color = face ? face->foreground : FRAME_FOREGROUND_PIXEL (f);
  unsigned long color_first = (face_first
			       ? face_first->foreground
			       : FRAME_FOREGROUND_PIXEL (f));
  unsigned long color_last = (face_last
			      ? face_last->foreground
			      : FRAME_FOREGROUND_PIXEL (f));
  cairo_t *cr = pgtk_begin_cr_clip (f);

  if (y1 - y0 > x1 - x0 && x1 - x0 > 2)
    /* Vertical.  */
    {
      pgtk_set_cr_source_with_color (f, color_first);
      cairo_rectangle (cr, x0, y0, 1, y1 - y0);
      cairo_fill(cr);
      pgtk_set_cr_source_with_color (f, color);
      cairo_rectangle (cr, x0 + 1, y0, x1 - x0 - 2, y1 - y0);
      cairo_fill(cr);
      pgtk_set_cr_source_with_color (f, color_last);
      cairo_rectangle (cr, x1 - 1, y0, 1, y1 - y0);
      cairo_fill(cr);
    }
  else if (x1 - x0 > y1 - y0 && y1 - y0 > 3)
    /* Horizontal.  */
    {
      pgtk_set_cr_source_with_color (f, color_first);
      cairo_rectangle (cr, x0, y0, x1 - x0, 1);
      cairo_fill(cr);
      pgtk_set_cr_source_with_color (f, color);
      cairo_rectangle (cr, x0, y0 + 1, x1 - x0, y1 - y0 - 2);
      cairo_fill(cr);
      pgtk_set_cr_source_with_color (f, color_last);
      cairo_rectangle (cr, x0, y1 - 1, x1 - x0, 1);
      cairo_fill(cr);
    }
  else
    {
      pgtk_set_cr_source_with_color (f, color);
      cairo_rectangle (cr, x0, y0, x1 - x0, y1 - y0);
      cairo_fill(cr);
    }

  pgtk_end_cr_clip (f);
}

/* End update of window W.

   Draw vertical borders between horizontally adjacent windows, and
   display W's cursor if CURSOR_ON_P is non-zero.

   MOUSE_FACE_OVERWRITTEN_P non-zero means that some row containing
   glyphs in mouse-face were overwritten.  In that case we have to
   make sure that the mouse-highlight is properly redrawn.

   W may be a menu bar pseudo-window in case we don't have X toolkit
   support.  Such windows don't have a cursor, so don't display it
   here.  */

static void
pgtk_update_window_end (struct window *w, bool cursor_on_p,
		     bool mouse_face_overwritten_p)
{
  if (!w->pseudo_window_p)
    {
      block_input ();

      if (cursor_on_p)
	display_and_set_cursor (w, true,
				w->output_cursor.hpos, w->output_cursor.vpos,
				w->output_cursor.x, w->output_cursor.y);

      if (draw_window_fringes (w, true))
	{
	  if (WINDOW_RIGHT_DIVIDER_WIDTH (w))
	    gui_draw_right_divider (w);
	  else
	    gui_draw_vertical_border (w);
	}

      unblock_input ();
    }

  /* If a row with mouse-face was overwritten, arrange for
     XTframe_up_to_date to redisplay the mouse highlight.  */
  if (mouse_face_overwritten_p)
    {
      Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (XFRAME (w->frame));

      hlinfo->mouse_face_beg_row = hlinfo->mouse_face_beg_col = -1;
      hlinfo->mouse_face_end_row = hlinfo->mouse_face_end_col = -1;
      hlinfo->mouse_face_window = Qnil;
    }
}

/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
pgtk_update_end (struct frame *f)
{
  /* Mouse highlight may be displayed again.  */
  MOUSE_HL_INFO (f)->mouse_face_defer = false;
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

   Set *TIMESTAMP to the server time-stamp for the time at which the mouse
   was at this position.

   Don't store anything if we don't have a valid set of values to report.

   This clears the mouse_moved flag, so we can wait for the next mouse
   movement.  */

static void
pgtk_mouse_position (struct frame **fp, int insist, Lisp_Object *bar_window,
		     enum scroll_bar_part *part, Lisp_Object *x, Lisp_Object *y,
		     Time *timestamp)
{
  struct frame *f1;
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (*fp);
  int win_x, win_y;
  GdkSeat *seat;
  GdkDevice *device;

  block_input ();

  Lisp_Object frame, tail;

  /* Clear the mouse-moved flag for every frame on this display.  */
  FOR_EACH_FRAME (tail, frame)
    if (FRAME_PGTK_P (XFRAME (frame))
	&& FRAME_X_DISPLAY (XFRAME (frame)) == FRAME_X_DISPLAY (*fp))
      XFRAME (frame)->mouse_moved = false;

  dpyinfo->last_mouse_scroll_bar = NULL;

  seat = gdk_display_get_default_seat(dpyinfo->gdpy);
  device = gdk_seat_get_pointer(seat);

  if (gui_mouse_grabbed (dpyinfo)) {
    GdkWindow *win;
    GdkModifierType mask;
    /* get x, y relative to edit window of f1. */
    f1 = dpyinfo->last_mouse_frame;
    win = gtk_widget_get_window(FRAME_GTK_WIDGET(f1));
    win = gdk_window_get_device_position(win, device, &win_x, &win_y, &mask);
  } else {
    GdkWindow *win;
    GdkModifierType mask;
    /* 1. get frame where the pointer is on. */
    win = gtk_widget_get_window(FRAME_GTK_WIDGET(*fp));
    win = gdk_window_get_device_position(win, device, &win_x, &win_y, &mask);
    if (win != NULL)
      f1 = pgtk_any_window_to_frame(win);
    else
      f1 = SELECTED_FRAME();

    /* 2. get x, y relative to edit window of the frame. */
    win = gtk_widget_get_window(FRAME_GTK_WIDGET(f1));
    win = gdk_window_get_device_position(win, device, &win_x, &win_y, &mask);
  }

  if (f1 != NULL) {
    dpyinfo = FRAME_DISPLAY_INFO (f1);
    remember_mouse_glyph (f1, win_x, win_y, &dpyinfo->last_mouse_glyph);
    dpyinfo->last_mouse_glyph_frame = f1;

    *bar_window = Qnil;
    *part = 0;
    *fp = f1;
    XSETINT (*x, win_x);
    XSETINT (*y, win_y);
    *timestamp = dpyinfo->last_mouse_movement_time;
  }

  unblock_input ();
}

/* Fringe bitmaps.  */

static int max_fringe_bmp = 0;
static cairo_pattern_t **fringe_bmp = 0;

static void
pgtk_define_fringe_bitmap (int which, unsigned short *bits, int h, int wd)
{
  int i, stride;
  cairo_surface_t *surface;
  unsigned char *data;
  cairo_pattern_t *pattern;

  if (which >= max_fringe_bmp)
    {
      i = max_fringe_bmp;
      max_fringe_bmp = which + 20;
      fringe_bmp = (cairo_pattern_t **) xrealloc (fringe_bmp, max_fringe_bmp * sizeof (cairo_pattern_t *));
      while (i < max_fringe_bmp)
	fringe_bmp[i++] = 0;
    }

  block_input ();

  surface = cairo_image_surface_create (CAIRO_FORMAT_A1, wd, h);
  stride = cairo_image_surface_get_stride (surface);
  data = cairo_image_surface_get_data (surface);

  for (i = 0; i < h; i++)
    {
      *((unsigned short *) data) = bits[i];
      data += stride;
    }

  cairo_surface_mark_dirty (surface);
  pattern = cairo_pattern_create_for_surface (surface);
  cairo_surface_destroy (surface);

  unblock_input ();

  fringe_bmp[which] = pattern;
}

static void
pgtk_destroy_fringe_bitmap (int which)
{
  if (which >= max_fringe_bmp)
    return;

  if (fringe_bmp[which])
    {
      block_input ();
      cairo_pattern_destroy (fringe_bmp[which]);
      unblock_input ();
    }
  fringe_bmp[which] = 0;
}

static void
pgtk_clip_to_row (struct window *w, struct glyph_row *row,
		    enum glyph_row_area area, cairo_t *cr)
{
  int window_x, window_y, window_width;
  cairo_rectangle_int_t rect;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  rect.x = window_x;
  rect.y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  rect.y = max (rect.y, window_y);
  rect.width = window_width;
  rect.height = row->visible_height;

  cairo_rectangle(cr, rect.x, rect.y, rect.width, rect.height);
  cairo_clip(cr);
}

static void
pgtk_cr_draw_image (struct frame *f, Emacs_GC *gc, cairo_pattern_t *image,
		 int src_x, int src_y, int width, int height,
		 int dest_x, int dest_y, bool overlay_p)
{
  cairo_t *cr;
  cairo_matrix_t matrix;
  cairo_surface_t *surface;
  cairo_format_t format;

  PGTK_TRACE("pgtk_cr_draw_image: 0: %d,%d,%d,%d,%d,%d,%d.", src_x, src_y, width, height, dest_x, dest_y, overlay_p);
  cr = pgtk_begin_cr_clip (f);
  if (overlay_p)
    cairo_rectangle (cr, dest_x, dest_y, width, height);
  else
    {
      pgtk_set_cr_source_with_gc_background (f, gc);
      cairo_rectangle (cr, dest_x, dest_y, width, height);
      cairo_fill_preserve (cr);
    }
  cairo_clip (cr);
  cairo_matrix_init_translate (&matrix, src_x - dest_x, src_y - dest_y);
  cairo_pattern_set_matrix (image, &matrix);
  cairo_pattern_get_surface (image, &surface);
  format = cairo_image_surface_get_format (surface);
  if (format != CAIRO_FORMAT_A8 && format != CAIRO_FORMAT_A1)
    {
      PGTK_TRACE("other format.");
      cairo_set_source (cr, image);
      cairo_fill (cr);
    }
  else
    {
      if (format == CAIRO_FORMAT_A8)
	PGTK_TRACE("format A8.");
      else if (format == CAIRO_FORMAT_A1)
	PGTK_TRACE("format A1.");
      else
	PGTK_TRACE("format ??.");
      pgtk_set_cr_source_with_gc_foreground (f, gc);
      cairo_rectangle_list_t *rects = cairo_copy_clip_rectangle_list(cr);
      PGTK_TRACE("rects:");
      PGTK_TRACE(" status: %u", rects->status);
      PGTK_TRACE(" rectangles:");
      for (int i = 0; i < rects->num_rectangles; i++) {
	PGTK_TRACE("  %fx%f+%f+%f",
		rects->rectangles[i].width,
		rects->rectangles[i].height,
		rects->rectangles[i].x,
		rects->rectangles[i].y);
      }
      cairo_rectangle_list_destroy(rects);
      cairo_mask (cr, image);
    }
  pgtk_end_cr_clip (f);
  PGTK_TRACE("pgtk_cr_draw_image: 9.");
}

static void
pgtk_draw_fringe_bitmap (struct window *w, struct glyph_row *row, struct draw_fringe_bitmap_params *p)
{
  PGTK_TRACE("draw_fringe_bitmap.");

  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face = p->face;

  cairo_t *cr = pgtk_begin_cr_clip(f);
  cairo_save(cr);

  /* Must clip because of partially visible lines.  */
  pgtk_clip_to_row (w, row, ANY_AREA, cr);

  if (p->bx >= 0 && !p->overlay_p)
    {
      /* In case the same realized face is used for fringes and
	 for something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 FillSolid in x_draw_glyph_string_background.  */
      if (face->stipple) {
	cairo_surface_t *bg = create_background_surface_by_face(f, face, p->bx, p->by, p->nx, p->ny);
	cairo_t *cr = pgtk_begin_cr_clip (f);
	cairo_set_source_surface (cr, bg, p->bx, p->by);
	cairo_rectangle (cr, p->bx, p->by, p->nx, p->ny);
	cairo_fill (cr);
	pgtk_end_cr_clip (f);
	cairo_surface_destroy (bg);
      } else {
	pgtk_set_cr_source_with_color(f, face->background);
	cairo_rectangle(cr, p->bx, p->by, p->nx, p->ny);
	cairo_fill(cr);
      }
    }

  PGTK_TRACE("which: %d, max_fringe_bmp: %d.", p->which, max_fringe_bmp);
  if (p->which && p->which < max_fringe_bmp)
    {
      Emacs_GC gcv;

      PGTK_TRACE("cursor_p=%d.", p->cursor_p);
      PGTK_TRACE("overlay_p_p=%d.", p->overlay_p);
      PGTK_TRACE("background=%08lx.", face->background);
      PGTK_TRACE("cursor_color=%08lx.", FRAME_X_OUTPUT(f)->cursor_color);
      PGTK_TRACE("foreground=%08lx.", face->foreground);
      gcv.foreground = (p->cursor_p
		       ? (p->overlay_p ? face->background
			  : FRAME_X_OUTPUT(f)->cursor_color)
		       : face->foreground);
      gcv.background = face->background;
      pgtk_cr_draw_image (f, &gcv, fringe_bmp[p->which], 0, p->dh,
		       p->wd, p->h, p->x, p->y, p->overlay_p);
    }

  cairo_restore(cr);
}

static struct atimer *hourglass_atimer = NULL;
static int hourglass_enter_count = 0;

static void hourglass_cb(struct atimer *timer)
{
  /*NOP*/
}

static void
pgtk_show_hourglass(struct frame *f)
{
  struct pgtk_output *x = FRAME_X_OUTPUT(f);
  if (x->hourglass_widget != NULL)
    gtk_widget_destroy(x->hourglass_widget);
  x->hourglass_widget = gtk_event_box_new();   /* gtk_event_box is GDK_INPUT_ONLY. */
  gtk_widget_set_has_window(x->hourglass_widget, true);
  gtk_fixed_put(GTK_FIXED(FRAME_GTK_WIDGET(f)), x->hourglass_widget, 0, 0);
  gtk_widget_show(x->hourglass_widget);
  gtk_widget_set_size_request(x->hourglass_widget, 30000, 30000);
  gdk_window_raise(gtk_widget_get_window(x->hourglass_widget));
  gdk_window_set_cursor(gtk_widget_get_window(x->hourglass_widget), x->hourglass_cursor);

  /* For cursor animation, we receive signals, set pending_signals, and dispatch. */
  if (hourglass_enter_count++ == 0) {
    struct timespec ts = make_timespec(0, 50 * 1000 * 1000);
    if (hourglass_atimer != NULL)
      cancel_atimer(hourglass_atimer);
    hourglass_atimer = start_atimer(ATIMER_CONTINUOUS, ts, hourglass_cb, NULL);
  }

  /* Cursor frequently stops animation. gtk's bug? */
}

static void
pgtk_hide_hourglass(struct frame *f)
{
  struct pgtk_output *x = FRAME_X_OUTPUT(f);
  if (--hourglass_enter_count == 0) {
    if (hourglass_atimer != NULL) {
      cancel_atimer(hourglass_atimer);
      hourglass_atimer = NULL;
    }
  }
  if (x->hourglass_widget != NULL) {
    gtk_widget_destroy(x->hourglass_widget);
    x->hourglass_widget = NULL;
  }
}

/* Flushes changes to display.  */
static void
pgtk_flush_display (struct frame *f)
{
}

extern frame_parm_handler pgtk_frame_parm_handlers[];

static struct redisplay_interface pgtk_redisplay_interface =
{
  pgtk_frame_parm_handlers,
  gui_produce_glyphs,
  gui_write_glyphs,
  gui_insert_glyphs,
  gui_clear_end_of_line,
  pgtk_scroll_run,
  pgtk_after_update_window_line,
  pgtk_update_window_begin,
  pgtk_update_window_end,
  pgtk_flush_display,
  gui_clear_window_mouse_face,
  gui_get_glyph_overhangs,
  gui_fix_overlapping_area,
  pgtk_draw_fringe_bitmap,
  pgtk_define_fringe_bitmap,
  pgtk_destroy_fringe_bitmap,
  pgtk_compute_glyph_string_overhangs,
  pgtk_draw_glyph_string,
  pgtk_define_frame_cursor,
  pgtk_clear_frame_area,
  pgtk_clear_under_internal_border,
  pgtk_draw_window_cursor,
  pgtk_draw_vertical_window_border,
  pgtk_draw_window_divider,
  NULL, // pgtk_shift_glyphs_for_insert,
  pgtk_show_hourglass,
  pgtk_hide_hourglass
};

static void
pgtk_redraw_scroll_bars (struct frame *f)
{
  PGTK_TRACE("pgtk_redraw_scroll_bars");
}

void
pgtk_clear_frame (struct frame *f)
/* --------------------------------------------------------------------------
      External (hook): Erase the entire frame
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("pgtk_clear_frame");
 /* comes on initial frame because we have
    after-make-frame-functions = select-frame */
  if (!FRAME_DEFAULT_FACE (f))
    return;

  // mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));

  block_input ();

  pgtk_clear_area(f, 0, 0, FRAME_PIXEL_WIDTH(f), FRAME_PIXEL_HEIGHT(f));

  /* as of 2006/11 or so this is now needed */
  pgtk_redraw_scroll_bars (f);
  unblock_input ();
}

/* Invert the middle quarter of the frame for .15 sec.  */

static void
recover_from_visible_bell(struct atimer *timer)
{
  struct frame *f = timer->client_data;

  if (FRAME_X_OUTPUT(f)->cr_surface_visible_bell != NULL) {
    cairo_surface_destroy(FRAME_X_OUTPUT(f)->cr_surface_visible_bell);
    FRAME_X_OUTPUT(f)->cr_surface_visible_bell = NULL;
  }

  if (FRAME_X_OUTPUT(f)->atimer_visible_bell != NULL)
    FRAME_X_OUTPUT(f)->atimer_visible_bell = NULL;

  gtk_widget_queue_draw(FRAME_GTK_WIDGET(f));
}

static void
pgtk_flash (struct frame *f)
{
  block_input ();

  {
    cairo_surface_t *surface_orig = FRAME_CR_SURFACE(f);

    int width = cairo_image_surface_get_width(surface_orig);
    int height = cairo_image_surface_get_height(surface_orig);
    cairo_surface_t *surface = cairo_surface_create_similar(surface_orig, CAIRO_CONTENT_COLOR_ALPHA,
							    width, height);

    cairo_t *cr = cairo_create(surface);
    cairo_set_source_surface(cr, surface_orig, 0, 0);
    cairo_rectangle(cr, 0, 0, width, height);
    cairo_clip(cr);
    cairo_paint(cr);

    cairo_set_source_rgb (cr, 1, 1, 1);
    cairo_set_operator (cr, CAIRO_OPERATOR_DIFFERENCE);

#define XFillRectangle(d, win, gc, x, y, w, h) \
    ( cairo_rectangle (cr, x, y, w, h), cairo_fill (cr) )

    {
      /* Get the height not including a menu bar widget.  */
      int height = FRAME_PIXEL_HEIGHT (f);
      /* Height of each line to flash.  */
      int flash_height = FRAME_LINE_HEIGHT (f);
      /* These will be the left and right margins of the rectangles.  */
      int flash_left = FRAME_INTERNAL_BORDER_WIDTH (f);
      int flash_right = FRAME_PIXEL_WIDTH (f) - FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = flash_right - flash_left;

      /* If window is tall, flash top and bottom line.  */
      if (height > 3 * FRAME_LINE_HEIGHT (f))
	{
	  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
			  flash_left,
			  (FRAME_INTERNAL_BORDER_WIDTH (f)
			   + FRAME_TOP_MARGIN_HEIGHT (f)),
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

      FRAME_X_OUTPUT(f)->cr_surface_visible_bell = surface;
      gtk_widget_queue_draw(FRAME_GTK_WIDGET(f));

      {
	struct timespec delay = make_timespec (0, 50 * 1000 * 1000);
	if (FRAME_X_OUTPUT(f)->atimer_visible_bell != NULL) {
	  cancel_atimer(FRAME_X_OUTPUT(f)->atimer_visible_bell);
	  FRAME_X_OUTPUT(f)->atimer_visible_bell = NULL;
	}
	FRAME_X_OUTPUT(f)->atimer_visible_bell = start_atimer(ATIMER_RELATIVE, delay, recover_from_visible_bell, f);
      }

#undef XFillRectangle
    }
  }

  unblock_input ();
}

/* Make audible bell.  */

static void
pgtk_ring_bell (struct frame *f)
{
  if (visible_bell)
    {
      pgtk_flash(f);
    }
  else
    {
      block_input ();
      gtk_widget_error_bell(FRAME_GTK_WIDGET(f));
      unblock_input ();
    }
}

/* Read events coming from the X server.
   Return as soon as there are no more events to be read.

   Return the number of characters stored into the buffer,
   thus pretending to be `read' (except the characters we store
   in the keyboard buffer can be multibyte, so are not necessarily
   C chars).  */

static int
pgtk_read_socket (struct terminal *terminal, struct input_event *hold_quit)
{
  PGTK_TRACE("pgtk_read_socket: enter.");
  GMainContext *context;
  bool context_acquired = false;
  int count;

  count = evq_flush(hold_quit);
  if (count > 0) {
    PGTK_TRACE("pgtk_read_socket: leave(1).");
    return count;
  }

  context = g_main_context_default ();
  context_acquired = g_main_context_acquire (context);

  block_input ();
  PGTK_TRACE("pgtk_read_socket: 3: errno=%d.", errno);

  if (context_acquired) {
    PGTK_TRACE("pgtk_read_socket: 4.1: acquired.");
    while (g_main_context_pending (context)) {
      PGTK_TRACE("pgtk_read_socket: 4: dispatch...");
      g_main_context_dispatch (context);
      PGTK_TRACE("pgtk_read_socket: 5: dispatch... done.");
    }
  }

  PGTK_TRACE("pgtk_read_socket: 7: errno=%d.", errno);
  unblock_input ();

  if (context_acquired)
    g_main_context_release (context);

  count = evq_flush(hold_quit);
  if (count > 0) {
    PGTK_TRACE("pgtk_read_socket: leave(2).");
    return count;
  }

  PGTK_TRACE("pgtk_read_socket: leave(3).");
  return 0;
}

int
pgtk_select (int fds_lim, fd_set *rfds, fd_set *wfds, fd_set *efds,
	     struct timespec *timeout, sigset_t *sigmask)
{
  fd_set all_rfds, all_wfds;
  struct timespec tmo;
  struct timespec *tmop = timeout;

  GMainContext *context;
  bool have_wfds = wfds != NULL;
  GPollFD gfds_buf[128];
  GPollFD *gfds = gfds_buf;
  int gfds_size = ARRAYELTS (gfds_buf);
  int n_gfds, retval = 0, our_fds = 0, max_fds = fds_lim - 1;
  bool context_acquired = false;
  int i, nfds, tmo_in_millisec, must_free = 0;
  bool need_to_dispatch;

  PGTK_TRACE("pgtk_select: enter.");

  if (event_q.nr >= 1) {
    PGTK_TRACE("pgtk_select: raise.");
    raise(SIGIO);
    errno = EINTR;
    return -1;
  }

  context = g_main_context_default ();
  context_acquired = g_main_context_acquire (context);
  /* FIXME: If we couldn't acquire the context, we just silently proceed
     because this function handles more than just glib file descriptors.
     Note that, as implemented, this failure is completely silent: there is
     no feedback to the caller.  */

  if (rfds) all_rfds = *rfds;
  else FD_ZERO (&all_rfds);
  if (wfds) all_wfds = *wfds;
  else FD_ZERO (&all_wfds);

  n_gfds = (context_acquired
	    ? g_main_context_query (context, G_PRIORITY_LOW, &tmo_in_millisec,
				    gfds, gfds_size)
	    : -1);

  if (gfds_size < n_gfds)
    {
      /* Avoid using SAFE_NALLOCA, as that implicitly refers to the
	 current thread.  Using xnmalloc avoids thread-switching
	 problems here.  */
      gfds = xnmalloc (n_gfds, sizeof *gfds);
      must_free = 1;
      gfds_size = n_gfds;
      n_gfds = g_main_context_query (context, G_PRIORITY_LOW, &tmo_in_millisec,
				     gfds, gfds_size);
    }

  for (i = 0; i < n_gfds; ++i)
    {
      if (gfds[i].events & G_IO_IN)
        {
          FD_SET (gfds[i].fd, &all_rfds);
          if (gfds[i].fd > max_fds) max_fds = gfds[i].fd;
        }
      if (gfds[i].events & G_IO_OUT)
        {
          FD_SET (gfds[i].fd, &all_wfds);
          if (gfds[i].fd > max_fds) max_fds = gfds[i].fd;
          have_wfds = true;
        }
    }

  if (must_free)
    xfree (gfds);

  if (n_gfds >= 0 && tmo_in_millisec >= 0)
    {
      tmo = make_timespec (tmo_in_millisec / 1000,
			   1000 * 1000 * (tmo_in_millisec % 1000));
      if (!timeout || timespec_cmp (tmo, *timeout) < 0)
	tmop = &tmo;
    }

  fds_lim = max_fds + 1;
  nfds = thread_select (pselect, fds_lim,
			&all_rfds, have_wfds ? &all_wfds : NULL, efds,
			tmop, sigmask);
  if (nfds < 0)
    retval = nfds;
  else if (nfds > 0)
    {
      for (i = 0; i < fds_lim; ++i)
        {
          if (FD_ISSET (i, &all_rfds))
            {
              if (rfds && FD_ISSET (i, rfds)) ++retval;
              else ++our_fds;
            }
          else if (rfds)
            FD_CLR (i, rfds);

          if (have_wfds && FD_ISSET (i, &all_wfds))
            {
              if (wfds && FD_ISSET (i, wfds)) ++retval;
              else ++our_fds;
            }
          else if (wfds)
            FD_CLR (i, wfds);

          if (efds && FD_ISSET (i, efds))
            ++retval;
        }
    }

  /* If Gtk+ is in use eventually gtk_main_iteration will be called,
     unless retval is zero.  */
  need_to_dispatch = retval == 0;
  if (need_to_dispatch && context_acquired)
    {
      int pselect_errno = errno;
      PGTK_TRACE("retval=%d.", retval);
      PGTK_TRACE("need_to_dispatch=%d.", need_to_dispatch);
      PGTK_TRACE("context_acquired=%d.", context_acquired);
      PGTK_TRACE("pselect_errno=%d.", pselect_errno);
      /* Prevent g_main_dispatch recursion, that would occur without
         block_input wrapper, because event handlers call
         unblock_input.  Event loop recursion was causing Bug#15801.  */
      block_input ();
      while (g_main_context_pending (context)) {
	PGTK_TRACE("dispatch...");
        g_main_context_dispatch (context);
	PGTK_TRACE("dispatch... done.");
      }
      unblock_input ();
      errno = pselect_errno;
    }

  if (context_acquired)
    g_main_context_release (context);

  /* To not have to recalculate timeout, return like this.  */
  if ((our_fds > 0 || (nfds == 0 && tmop == &tmo)) && (retval == 0))
    {
      retval = -1;
      errno = EINTR;
    }

  PGTK_TRACE("pgtk_select: leave.");
  return retval;
}


/* Lisp window being scrolled.  Set when starting to interact with
   a toolkit scroll bar, reset to nil when ending the interaction.  */

static Lisp_Object window_being_scrolled;

static void
pgtk_send_scroll_bar_event (Lisp_Object window, enum scroll_bar_part part,
			    int portion, int whole, bool horizontal)
{
  union buffered_input_event inev;

  EVENT_INIT (inev.ie);

  inev.ie.kind = horizontal ? HORIZONTAL_SCROLL_BAR_CLICK_EVENT : SCROLL_BAR_CLICK_EVENT;
  inev.ie.frame_or_window = window;
  inev.ie.arg = Qnil;
  inev.ie.timestamp = 0;
  inev.ie.code = 0;
  inev.ie.part = part;
  inev.ie.x = make_fixnum(portion);
  inev.ie.y = make_fixnum(whole);
  inev.ie.modifiers = 0;

  evq_enqueue(&inev);
}


/* Scroll bar callback for GTK scroll bars.  WIDGET is the scroll
   bar widget.  DATA is a pointer to the scroll_bar structure. */

static gboolean
xg_scroll_callback (GtkRange     *range,
                    GtkScrollType scroll,
                    gdouble       value,
                    gpointer      user_data)
{
  int whole = 0, portion = 0;
  struct scroll_bar *bar = user_data;
  enum scroll_bar_part part = scroll_bar_nowhere;
  GtkAdjustment *adj = GTK_ADJUSTMENT (gtk_range_get_adjustment (range));
  PGTK_TRACE("xg_scroll_callback:");

  if (xg_ignore_gtk_scrollbar) return false;
  PGTK_TRACE("xg_scroll_callback: not ignored.");

  PGTK_TRACE("xg_scroll_callback: scroll=%u.", scroll);
  switch (scroll)
    {
    case GTK_SCROLL_JUMP:
#if 0
      /* Buttons 1 2 or 3 must be grabbed.  */
      if (FRAME_DISPLAY_INFO (f)->grabbed != 0
          && FRAME_DISPLAY_INFO (f)->grabbed < (1 << 4))
#endif
        {
	  if (bar->horizontal)
	    {
	      part = scroll_bar_horizontal_handle;
	      whole = (int)(gtk_adjustment_get_upper (adj) -
			    gtk_adjustment_get_page_size (adj));
	      portion = min ((int)value, whole);
	      bar->dragging = portion;
	    }
	  else
	    {
	      part = scroll_bar_handle;
	      whole = gtk_adjustment_get_upper (adj) -
		gtk_adjustment_get_page_size (adj);
	      portion = min ((int)value, whole);
	      bar->dragging = portion;
	    }
	}
      break;
    case GTK_SCROLL_STEP_BACKWARD:
      part = (bar->horizontal
	      ? scroll_bar_left_arrow : scroll_bar_up_arrow);
      bar->dragging = -1;
      break;
    case GTK_SCROLL_STEP_FORWARD:
      part = (bar->horizontal
	      ? scroll_bar_right_arrow : scroll_bar_down_arrow);
      bar->dragging = -1;
      break;
    case GTK_SCROLL_PAGE_BACKWARD:
      part = (bar->horizontal
	      ? scroll_bar_before_handle : scroll_bar_above_handle);
      bar->dragging = -1;
      break;
    case GTK_SCROLL_PAGE_FORWARD:
      part = (bar->horizontal
	      ? scroll_bar_after_handle : scroll_bar_below_handle);
      bar->dragging = -1;
      break;
    default:
      break;
    }

  PGTK_TRACE("xg_scroll_callback: part=%u, scroll_bar_nowhere=%d.", part, scroll_bar_nowhere);
  if (part != scroll_bar_nowhere)
    {
      window_being_scrolled = bar->window;
      pgtk_send_scroll_bar_event (bar->window, part, portion, whole,
				  bar->horizontal);
    }

  return false;
}

/* Callback for button release. Sets dragging to -1 when dragging is done.  */

static gboolean
xg_end_scroll_callback (GtkWidget *widget,
                        GdkEventButton *event,
                        gpointer user_data)
{
  struct scroll_bar *bar = user_data;
  PGTK_TRACE("xg_end_scroll_callback:");
  bar->dragging = -1;
  if (WINDOWP (window_being_scrolled))
    {
      pgtk_send_scroll_bar_event (window_being_scrolled,
				  scroll_bar_end_scroll, 0, 0, bar->horizontal);
      window_being_scrolled = Qnil;
    }

  return false;
}

#define SCROLL_BAR_NAME "verticalScrollBar"
#define SCROLL_BAR_HORIZONTAL_NAME "horizontalScrollBar"

/* Create the widget for scroll bar BAR on frame F.  Record the widget
   and X window of the scroll bar in BAR.  */

static void
x_create_toolkit_scroll_bar (struct frame *f, struct scroll_bar *bar)
{
  const char *scroll_bar_name = SCROLL_BAR_NAME;

  block_input ();
  xg_create_scroll_bar (f, bar, G_CALLBACK (xg_scroll_callback),
                        G_CALLBACK (xg_end_scroll_callback),
                        scroll_bar_name);
  unblock_input ();
}

static void
x_create_horizontal_toolkit_scroll_bar (struct frame *f, struct scroll_bar *bar)
{
  const char *scroll_bar_name = SCROLL_BAR_HORIZONTAL_NAME;

  block_input ();
  xg_create_horizontal_scroll_bar (f, bar, G_CALLBACK (xg_scroll_callback),
				   G_CALLBACK (xg_end_scroll_callback),
				   scroll_bar_name);
  unblock_input ();
}

/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

static void
x_set_toolkit_scroll_bar_thumb (struct scroll_bar *bar, int portion, int position, int whole)
{
  xg_set_toolkit_scroll_bar_thumb (bar, portion, position, whole);
}

static void
x_set_toolkit_horizontal_scroll_bar_thumb (struct scroll_bar *bar, int portion, int position, int whole)
{
  xg_set_toolkit_horizontal_scroll_bar_thumb (bar, portion, position, whole);
}



/* Create a scroll bar and return the scroll bar vector for it.  W is
   the Emacs window on which to create the scroll bar. TOP, LEFT,
   WIDTH and HEIGHT are the pixel coordinates and dimensions of the
   scroll bar. */

static struct scroll_bar *
x_scroll_bar_create (struct window *w, int top, int left,
		     int width, int height, bool horizontal)
{
  struct frame *f = XFRAME (w->frame);
  struct scroll_bar *bar
    = ALLOCATE_PSEUDOVECTOR (struct scroll_bar, prev, PVEC_OTHER);
  Lisp_Object barobj;

  block_input ();

  if (horizontal)
    x_create_horizontal_toolkit_scroll_bar (f, bar);
  else
    x_create_toolkit_scroll_bar (f, bar);

  XSETWINDOW (bar->window, w);
  bar->top = top;
  bar->left = left;
  bar->width = width;
  bar->height = height;
  bar->start = 0;
  bar->end = 0;
  bar->dragging = -1;
  bar->horizontal = horizontal;

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (barobj, bar);
  fset_scroll_bars (f, barobj);
  if (!NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  /* Map the window/widget.  */
  {
    if (horizontal)
      xg_update_horizontal_scrollbar_pos (f, bar->x_window, top,
					  left, width, max (height, 1));
    else
      xg_update_scrollbar_pos (f, bar->x_window, top,
			       left, width, max (height, 1));
    }

  unblock_input ();
  return bar;
}

/* Destroy scroll bar BAR, and set its Emacs window's scroll bar to
   nil.  */

static void
x_scroll_bar_remove (struct scroll_bar *bar)
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  block_input ();

  xg_remove_scroll_bar (f, bar->x_window);

  /* Dissociate this scroll bar from its window.  */
  if (bar->horizontal)
    wset_horizontal_scroll_bar (XWINDOW (bar->window), Qnil);
  else
    wset_vertical_scroll_bar (XWINDOW (bar->window), Qnil);

  unblock_input ();
}

/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */

static void
pgtk_set_vertical_scroll_bar (struct window *w, int portion, int whole, int position)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object barobj;
  struct scroll_bar *bar;
  int top, height, left, width;
  int window_y, window_height;

  /* Get window dimensions.  */
  window_box (w, ANY_AREA, 0, &window_y, 0, &window_height);
  top = window_y;
  height = window_height;
  left = WINDOW_SCROLL_BAR_AREA_X (w);
  width = WINDOW_SCROLL_BAR_AREA_WIDTH (w);
  PGTK_TRACE("pgtk_set_vertical_scroll_bar: has_vertical_scroll_bar: %d", WINDOW_HAS_VERTICAL_SCROLL_BAR(w));
  PGTK_TRACE("pgtk_set_vertical_scroll_bar: config_scroll_bar_width: %d", WINDOW_CONFIG_SCROLL_BAR_WIDTH(w));
  PGTK_TRACE("pgtk_set_vertical_scroll_bar: scroll_bar_width: %d", w->scroll_bar_width);
  PGTK_TRACE("pgtk_set_vertical_scroll_bar: config_scroll_bar_width: %d", FRAME_CONFIG_SCROLL_BAR_WIDTH (WINDOW_XFRAME (w)));
  PGTK_TRACE("pgtk_set_vertical_scroll_bar: %dx%d+%d+%d", width, height, left, top);

  /* Does the scroll bar exist yet?  */
  if (NILP (w->vertical_scroll_bar))
    {
      if (width > 0 && height > 0)
	{
	  block_input ();
          pgtk_clear_area (f, left, top, width, height);
	  unblock_input ();
	}

      bar = x_scroll_bar_create (w, top, left, width, max (height, 1), false);
    }
  else
    {
      /* It may just need to be moved and resized.  */
      unsigned int mask = 0;

      bar = XSCROLL_BAR (w->vertical_scroll_bar);

      block_input ();

      if (left != bar->left)
	mask |= 1;
      if (top != bar->top)
	mask |= 1;
      if (width != bar->width)
	mask |= 1;
      if (height != bar->height)
	mask |= 1;

      /* Move/size the scroll bar widget.  */
      if (mask)
	{
	  /* Since toolkit scroll bars are smaller than the space reserved
	     for them on the frame, we have to clear "under" them.  */
	  if (width > 0 && height > 0)
	    pgtk_clear_area (f, left, top, width, height);
          xg_update_scrollbar_pos (f, bar->x_window, top,
				   left, width, max (height, 1));
	}

      /* Remember new settings.  */
      bar->left = left;
      bar->top = top;
      bar->width = width;
      bar->height = height;

      unblock_input ();
    }

  x_set_toolkit_scroll_bar_thumb (bar, portion, position, whole);

  XSETVECTOR (barobj, bar);
  wset_vertical_scroll_bar (w, barobj);
}


static void
pgtk_set_horizontal_scroll_bar (struct window *w, int portion, int whole, int position)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object barobj;
  struct scroll_bar *bar;
  int top, height, left, width;
  int window_x, window_width;
  int pixel_width = WINDOW_PIXEL_WIDTH (w);

  /* Get window dimensions.  */
  window_box (w, ANY_AREA, &window_x, 0, &window_width, 0);
  left = window_x;
  width = window_width;
  top = WINDOW_SCROLL_BAR_AREA_Y (w);
  height = WINDOW_SCROLL_BAR_AREA_HEIGHT (w);

  /* Does the scroll bar exist yet?  */
  if (NILP (w->horizontal_scroll_bar))
    {
      if (width > 0 && height > 0)
	{
	  block_input ();

	  /* Clear also part between window_width and
	     WINDOW_PIXEL_WIDTH.  */
	  pgtk_clear_area (f, left, top, pixel_width, height);
	  unblock_input ();
	}

      bar = x_scroll_bar_create (w, top, left, width, height, true);
    }
  else
    {
      /* It may just need to be moved and resized.  */
      unsigned int mask = 0;

      bar = XSCROLL_BAR (w->horizontal_scroll_bar);

      block_input ();

      if (left != bar->left)
	mask |= 1;
      if (top != bar->top)
	mask |= 1;
      if (width != bar->width)
	mask |= 1;
      if (height != bar->height)
	mask |= 1;

      /* Move/size the scroll bar widget.  */
      if (mask)
	{
	  /* Since toolkit scroll bars are smaller than the space reserved
	     for them on the frame, we have to clear "under" them.  */
	  if (width > 0 && height > 0)
	    pgtk_clear_area (f,
			  WINDOW_LEFT_EDGE_X (w), top,
			  pixel_width - WINDOW_RIGHT_DIVIDER_WIDTH (w), height);
          xg_update_horizontal_scrollbar_pos (f, bar->x_window, top, left,
					      width, height);
	}

      /* Remember new settings.  */
      bar->left = left;
      bar->top = top;
      bar->width = width;
      bar->height = height;

      unblock_input ();
    }

  x_set_toolkit_horizontal_scroll_bar_thumb (bar, portion, position, whole);

  XSETVECTOR (barobj, bar);
  wset_horizontal_scroll_bar (w, barobj);
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
pgtk_condemn_scroll_bars (struct frame *frame)
{
  if (!NILP (FRAME_SCROLL_BARS (frame)))
    {
      if (!NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
	{
	  /* Prepend scrollbars to already condemned ones.  */
	  Lisp_Object last = FRAME_SCROLL_BARS (frame);

	  while (!NILP (XSCROLL_BAR (last)->next))
	    last = XSCROLL_BAR (last)->next;

	  XSCROLL_BAR (last)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
	  XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = last;
	}

      fset_condemned_scroll_bars (frame, FRAME_SCROLL_BARS (frame));
      fset_scroll_bars (frame, Qnil);
    }
}


/* Un-mark WINDOW's scroll bar for deletion in this judgment cycle.
   Note that WINDOW isn't necessarily condemned at all.  */

static void
pgtk_redeem_scroll_bar (struct window *w)
{
  struct scroll_bar *bar;
  Lisp_Object barobj;
  struct frame *f;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (w->vertical_scroll_bar) && NILP (w->horizontal_scroll_bar))
    emacs_abort ();

  if (!NILP (w->vertical_scroll_bar) && WINDOW_HAS_VERTICAL_SCROLL_BAR (w))
    {
      bar = XSCROLL_BAR (w->vertical_scroll_bar);
      /* Unlink it from the condemned list.  */
      f = XFRAME (WINDOW_FRAME (w));
      if (NILP (bar->prev))
	{
	  /* If the prev pointer is nil, it must be the first in one of
	     the lists.  */
	  if (EQ (FRAME_SCROLL_BARS (f), w->vertical_scroll_bar))
	    /* It's not condemned.  Everything's fine.  */
	    goto horizontal;
	  else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		       w->vertical_scroll_bar))
	    fset_condemned_scroll_bars (f, bar->next);
	  else
	    /* If its prev pointer is nil, it must be at the front of
	       one or the other!  */
	    emacs_abort ();
	}
      else
	XSCROLL_BAR (bar->prev)->next = bar->next;

      if (! NILP (bar->next))
	XSCROLL_BAR (bar->next)->prev = bar->prev;

      bar->next = FRAME_SCROLL_BARS (f);
      bar->prev = Qnil;
      XSETVECTOR (barobj, bar);
      fset_scroll_bars (f, barobj);
      if (! NILP (bar->next))
	XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
    }

 horizontal:
  if (!NILP (w->horizontal_scroll_bar) && WINDOW_HAS_HORIZONTAL_SCROLL_BAR (w))
    {
      bar = XSCROLL_BAR (w->horizontal_scroll_bar);
      /* Unlink it from the condemned list.  */
      f = XFRAME (WINDOW_FRAME (w));
      if (NILP (bar->prev))
	{
	  /* If the prev pointer is nil, it must be the first in one of
	     the lists.  */
	  if (EQ (FRAME_SCROLL_BARS (f), w->horizontal_scroll_bar))
	    /* It's not condemned.  Everything's fine.  */
	    return;
	  else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		       w->horizontal_scroll_bar))
	    fset_condemned_scroll_bars (f, bar->next);
	  else
	    /* If its prev pointer is nil, it must be at the front of
	       one or the other!  */
	    emacs_abort ();
	}
      else
	XSCROLL_BAR (bar->prev)->next = bar->next;

      if (! NILP (bar->next))
	XSCROLL_BAR (bar->next)->prev = bar->prev;

      bar->next = FRAME_SCROLL_BARS (f);
      bar->prev = Qnil;
      XSETVECTOR (barobj, bar);
      fset_scroll_bars (f, barobj);
      if (! NILP (bar->next))
	XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
    }
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */

static void
pgtk_judge_scroll_bars (struct frame *f)
{
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  fset_condemned_scroll_bars (f, Qnil);

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

static void set_fullscreen_state(struct frame *f)
{
  GtkWindow *widget = GTK_WINDOW(FRAME_GTK_OUTER_WIDGET(f));
  switch (f->want_fullscreen) {
  case FULLSCREEN_NONE:
    PGTK_TRACE("pgtk_fullscreen_hook: none.");
    gtk_window_unfullscreen(widget);
    gtk_window_unmaximize(widget);
    store_frame_param(f, Qfullscreen, Qnil);
    break;

  case FULLSCREEN_BOTH:
    PGTK_TRACE("pgtk_fullscreen_hook: both.");
    gtk_window_unmaximize(widget);
    gtk_window_fullscreen(widget);
    store_frame_param(f, Qfullscreen, Qfullboth);
    break;

  case FULLSCREEN_MAXIMIZED:
    PGTK_TRACE("pgtk_fullscreen_hook: maximized.");
    gtk_window_unfullscreen(widget);
    gtk_window_maximize(widget);
    store_frame_param(f, Qfullscreen, Qmaximized);
    break;

  case FULLSCREEN_WIDTH:
  case FULLSCREEN_HEIGHT:
    PGTK_TRACE("pgtk_fullscreen_hook: width or height.");
    /* Not supported by gtk. Ignore them.*/
  }

  f->want_fullscreen = FULLSCREEN_NONE;
}

static void
pgtk_fullscreen_hook (struct frame *f)
{
  PGTK_TRACE("pgtk_fullscreen_hook:");
  if (FRAME_VISIBLE_P (f))
    {
      block_input ();
      set_fullscreen_state(f);
      unblock_input ();
    }
}

/* This function is called when the last frame on a display is deleted. */
void
pgtk_delete_terminal (struct terminal *terminal)
{
  struct pgtk_display_info *dpyinfo = terminal->display_info.pgtk;

  /* Protect against recursive calls.  delete_frame in
     delete_terminal calls us back when it deletes our last frame.  */
  if (!terminal->name)
    return;

  block_input ();

  /* Normally, the display is available...  */
  if (dpyinfo->gdpy)
    {
      image_destroy_all_bitmaps (dpyinfo);

      xg_display_close (dpyinfo->gdpy);

      /* Do not close the connection here because it's already closed
	 by X(t)CloseDisplay (Bug#18403).  */
      dpyinfo->gdpy = NULL;
    }

  pgtk_delete_display (dpyinfo);
  unblock_input ();
}

/* Store F's background color into *BGCOLOR.  */
static void
pgtk_query_frame_background_color (struct frame *f, Emacs_Color *bgcolor)
{
  bgcolor->pixel = FRAME_BACKGROUND_PIXEL (f);
  pgtk_query_color (f, bgcolor);
}

static void
pgtk_free_pixmap (struct frame *_f, Emacs_Pixmap pixmap)
{
  if (pixmap)
    {
      xfree (pixmap->data);
      xfree (pixmap);
    }
}

void
pgtk_focus_frame (struct frame *f, bool noactivate)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  GtkWidget *wid = FRAME_GTK_OUTER_WIDGET(f);

  if (dpyinfo->x_focus_frame != f)
    {
      block_input ();
      gtk_window_present (GTK_WINDOW (wid));
      unblock_input ();
    }
}


static void set_opacity_recursively (GtkWidget *w, gpointer data)
{
  gtk_widget_set_opacity (w, *(double *) data);
  if (GTK_IS_CONTAINER (w))
    gtk_container_foreach (GTK_CONTAINER (w), set_opacity_recursively, data);
}

static void
x_set_frame_alpha (struct frame *f)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  double alpha = 1.0;
  double alpha_min = 1.0;

  if (dpyinfo->highlight_frame == f)
    alpha = f->alpha[0];
  else
    alpha = f->alpha[1];

  if (alpha < 0.0)
    return;

  if (FLOATP (Vframe_alpha_lower_limit))
    alpha_min = XFLOAT_DATA (Vframe_alpha_lower_limit);
  else if (FIXNUMP (Vframe_alpha_lower_limit))
    alpha_min = (XFIXNUM (Vframe_alpha_lower_limit)) / 100.0;

  if (alpha > 1.0)
    alpha = 1.0;
  else if (alpha < alpha_min && alpha_min <= 1.0)
    alpha = alpha_min;

#if 0
  /* If there is a parent from the window manager, put the property there
     also, to work around broken window managers that fail to do that.
     Do this unconditionally as this function is called on reparent when
     alpha has not changed on the frame.  */

  if (!FRAME_PARENT_FRAME (f))
    {
      Window parent = x_find_topmost_parent (f);
      if (parent != None)
	XChangeProperty (dpy, parent, dpyinfo->Xatom_net_wm_window_opacity,
			 XA_CARDINAL, 32, PropModeReplace,
			 (unsigned char *) &opac, 1);
    }
#endif

  set_opacity_recursively (FRAME_GTK_OUTER_WIDGET (f), &alpha);
  /* without this, blending mode is strange on wayland. */
  gtk_widget_queue_resize_no_redraw (FRAME_GTK_OUTER_WIDGET (f));
}

static void
frame_highlight (struct frame *f)
{
  /* We used to only do this if Vx_no_window_manager was non-nil, but
     the ICCCM (section 4.1.6) says that the window's border pixmap
     and border pixel are window attributes which are "private to the
     client", so we can always change it to whatever we want.  */
  block_input ();
  /* I recently started to get errors in this XSetWindowBorder, depending on
     the window-manager in use, tho something more is at play since I've been
     using that same window-manager binary for ever.  Let's not crash just
     because of this (bug#9310).  */
#if 0
  x_catch_errors (FRAME_X_DISPLAY (f));
  XSetWindowBorder (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    FRAME_X_OUTPUT(f)->border_pixel);
  x_uncatch_errors ();
#endif
  unblock_input ();
  gui_update_cursor (f, true);
  x_set_frame_alpha (f);
}

static void
frame_unhighlight (struct frame *f)
{
  /* We used to only do this if Vx_no_window_manager was non-nil, but
     the ICCCM (section 4.1.6) says that the window's border pixmap
     and border pixel are window attributes which are "private to the
     client", so we can always change it to whatever we want.  */
  block_input ();
  /* Same as above for XSetWindowBorder (bug#9310).  */
#if 0
  x_catch_errors (FRAME_X_DISPLAY (f));
  XSetWindowBorderPixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			  FRAME_X_OUTPUT(f)->border_tile);
  x_uncatch_errors ();
#endif
  unblock_input ();
  gui_update_cursor (f, true);
  x_set_frame_alpha (f);
}


static void
pgtk_frame_rehighlight (struct pgtk_display_info *dpyinfo)
{
  struct frame *old_highlight = dpyinfo->highlight_frame;

  if (dpyinfo->x_focus_frame)
    {
      dpyinfo->highlight_frame
	= ((FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame))
	   : dpyinfo->x_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->highlight_frame))
	{
	  fset_focus_frame (dpyinfo->x_focus_frame, Qnil);
	  dpyinfo->highlight_frame = dpyinfo->x_focus_frame;
	}
    }
  else
    dpyinfo->highlight_frame = 0;

  if (dpyinfo->highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (dpyinfo->highlight_frame)
	frame_highlight (dpyinfo->highlight_frame);
    }
}

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   mini-buffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or un-highlighted; we only use it to find
   the appropriate X display info.  */

static void
XTframe_rehighlight (struct frame *frame)
{
  pgtk_frame_rehighlight (FRAME_DISPLAY_INFO (frame));
}

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the Lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occurred, so the
   Lisp code can tell when the switch took place by examining the events.  */

static void
x_new_focus_frame (struct pgtk_display_info *dpyinfo, struct frame *frame)
{
  struct frame *old_focus = dpyinfo->x_focus_frame;
  /* doesn't work on wayland */

  if (frame != dpyinfo->x_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of x_focus_frame.  */
      dpyinfo->x_focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	gdk_window_lower (gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (old_focus)));

      if (dpyinfo->x_focus_frame && dpyinfo->x_focus_frame->auto_raise)
	gdk_window_raise (gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (dpyinfo->x_focus_frame)));
    }

  pgtk_frame_rehighlight (dpyinfo);
}

static struct terminal *
pgtk_create_terminal (struct pgtk_display_info *dpyinfo)
/* --------------------------------------------------------------------------
      Set up use of Gtk before we make the first connection.
   -------------------------------------------------------------------------- */
{
  struct terminal *terminal;

  terminal = create_terminal (output_pgtk, &pgtk_redisplay_interface);

  terminal->display_info.pgtk = dpyinfo;
  dpyinfo->terminal = terminal;

  terminal->clear_frame_hook = pgtk_clear_frame;
  terminal->ring_bell_hook = pgtk_ring_bell;
  terminal->update_begin_hook = pgtk_update_begin;
  terminal->update_end_hook = pgtk_update_end;
  terminal->read_socket_hook = pgtk_read_socket;
  // terminal->frame_up_to_date_hook = pgtk_frame_up_to_date;
  terminal->mouse_position_hook = pgtk_mouse_position;
  terminal->frame_rehighlight_hook = XTframe_rehighlight;
  // terminal->frame_raise_lower_hook = pgtk_frame_raise_lower;
  terminal->frame_visible_invisible_hook = pgtk_make_frame_visible_invisible;
  terminal->fullscreen_hook = pgtk_fullscreen_hook;
  terminal->menu_show_hook = pgtk_menu_show;
  terminal->activate_menubar_hook = pgtk_activate_menubar;
  terminal->popup_dialog_hook = pgtk_popup_dialog;
  terminal->set_vertical_scroll_bar_hook = pgtk_set_vertical_scroll_bar;
  terminal->set_horizontal_scroll_bar_hook = pgtk_set_horizontal_scroll_bar;
  terminal->condemn_scroll_bars_hook = pgtk_condemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = pgtk_redeem_scroll_bar;
  terminal->judge_scroll_bars_hook = pgtk_judge_scroll_bars;
  terminal->get_string_resource_hook = pgtk_get_string_resource;
  terminal->delete_frame_hook = x_destroy_window;
  terminal->delete_terminal_hook = pgtk_delete_terminal;
  terminal->query_frame_background_color = pgtk_query_frame_background_color;
  terminal->defined_color_hook = pgtk_defined_color;
  terminal->set_new_font_hook = pgtk_new_font;
  terminal->implicit_set_name_hook = pgtk_implicitly_set_name;
  terminal->iconify_frame_hook = pgtk_iconify_frame;
  terminal->set_scroll_bar_default_width_hook = pgtk_set_scroll_bar_default_width;
  terminal->set_scroll_bar_default_height_hook = pgtk_set_scroll_bar_default_height;
  terminal->set_window_size_hook = pgtk_set_window_size;
  terminal->query_colors = pgtk_query_colors;
  terminal->get_focus_frame = x_get_focus_frame;
  terminal->focus_frame_hook = pgtk_focus_frame;
  terminal->set_frame_offset_hook = x_set_offset;
  terminal->free_pixmap = pgtk_free_pixmap;

  /* Other hooks are NULL by default.  */

  return terminal;
}

struct pgtk_window_is_of_frame_recursive_t {
  GdkWindow *window;
  bool result;
};

static void
pgtk_window_is_of_frame_recursive(GtkWidget *widget, gpointer data)
{
  struct pgtk_window_is_of_frame_recursive_t *datap = data;

  if (datap->result)
    return;

  if (gtk_widget_get_window(widget) == datap->window) {
    datap->result = true;
    return;
  }

  if (GTK_IS_CONTAINER(widget))
    gtk_container_foreach(GTK_CONTAINER(widget), pgtk_window_is_of_frame_recursive, datap);
}

static bool
pgtk_window_is_of_frame(struct frame *f, GdkWindow *window)
{
  struct pgtk_window_is_of_frame_recursive_t data;
  data.window = window;
  data.result = false;
  pgtk_window_is_of_frame_recursive(FRAME_GTK_OUTER_WIDGET(f), &data);
  return data.result;
}

/* Like x_window_to_frame but also compares the window with the widget's
   windows.  */
static struct frame *
pgtk_any_window_to_frame (GdkWindow *window)
{
  Lisp_Object tail, frame;
  struct frame *f, *found = NULL;

  if (window == NULL)
    return NULL;

  FOR_EACH_FRAME (tail, frame)
    {
      if (found)
        break;
      f = XFRAME (frame);
      if (FRAME_PGTK_P (f))
	{
	  if (pgtk_window_is_of_frame(f, window))
	    found = f;
	}
    }

  return found;
}

static gboolean
pgtk_handle_event(GtkWidget *widget, GdkEvent *event, gpointer *data)
{
#ifdef PGTK_DEBUG
  const char *type_name = G_OBJECT_TYPE_NAME(widget);
  switch (event->type) {
  case GDK_NOTHING:               PGTK_TRACE("GDK_NOTHING"); break;
  case GDK_DELETE:                PGTK_TRACE("GDK_DELETE"); break;
  case GDK_DESTROY:               PGTK_TRACE("GDK_DESTROY"); break;
  case GDK_EXPOSE:                PGTK_TRACE("GDK_EXPOSE"); break;
  case GDK_MOTION_NOTIFY:         PGTK_TRACE("GDK_MOTION_NOTIFY"); break;
  case GDK_BUTTON_PRESS:          PGTK_TRACE("GDK_BUTTON_PRESS"); break;
  case GDK_2BUTTON_PRESS:         PGTK_TRACE("GDK_2BUTTON_PRESS"); break;
  case GDK_3BUTTON_PRESS:         PGTK_TRACE("GDK_3BUTTON_PRESS"); break;
  case GDK_BUTTON_RELEASE:        PGTK_TRACE("GDK_BUTTON_RELEASE"); break;
  case GDK_KEY_PRESS:             PGTK_TRACE("GDK_KEY_PRESS"); break;
  case GDK_KEY_RELEASE:           PGTK_TRACE("GDK_KEY_RELEASE"); break;
  case GDK_ENTER_NOTIFY:          PGTK_TRACE("GDK_ENTER_NOTIFY"); break;
  case GDK_LEAVE_NOTIFY:          PGTK_TRACE("GDK_LEAVE_NOTIFY"); break;
  case GDK_FOCUS_CHANGE:          PGTK_TRACE("GDK_FOCUS_CHANGE"); break;
  case GDK_CONFIGURE:             PGTK_TRACE("GDK_CONFIGURE"); break;
  case GDK_MAP:                   PGTK_TRACE("GDK_MAP"); break;
  case GDK_UNMAP:                 PGTK_TRACE("GDK_UNMAP"); break;
  case GDK_PROPERTY_NOTIFY:       PGTK_TRACE("GDK_PROPERTY_NOTIFY"); break;
  case GDK_SELECTION_CLEAR:       PGTK_TRACE("GDK_SELECTION_CLEAR"); break;
  case GDK_SELECTION_REQUEST:     PGTK_TRACE("GDK_SELECTION_REQUEST"); break;
  case GDK_SELECTION_NOTIFY:      PGTK_TRACE("GDK_SELECTION_NOTIFY"); break;
  case GDK_PROXIMITY_IN:          PGTK_TRACE("GDK_PROXIMITY_IN"); break;
  case GDK_PROXIMITY_OUT:         PGTK_TRACE("GDK_PROXIMITY_OUT"); break;
  case GDK_DRAG_ENTER:            PGTK_TRACE("GDK_DRAG_ENTER"); break;
  case GDK_DRAG_LEAVE:            PGTK_TRACE("GDK_DRAG_LEAVE"); break;
  case GDK_DRAG_MOTION:           PGTK_TRACE("GDK_DRAG_MOTION"); break;
  case GDK_DRAG_STATUS:           PGTK_TRACE("GDK_DRAG_STATUS"); break;
  case GDK_DROP_START:            PGTK_TRACE("GDK_DROP_START"); break;
  case GDK_DROP_FINISHED:         PGTK_TRACE("GDK_DROP_FINISHED"); break;
  case GDK_CLIENT_EVENT:          PGTK_TRACE("GDK_CLIENT_EVENT"); break;
  case GDK_VISIBILITY_NOTIFY:     PGTK_TRACE("GDK_VISIBILITY_NOTIFY"); break;
  case GDK_SCROLL:                PGTK_TRACE("GDK_SCROLL"); break;
  case GDK_WINDOW_STATE:          PGTK_TRACE("GDK_WINDOW_STATE"); break;
  case GDK_SETTING:               PGTK_TRACE("GDK_SETTING"); break;
  case GDK_OWNER_CHANGE:          PGTK_TRACE("GDK_OWNER_CHANGE"); break;
  case GDK_GRAB_BROKEN:           PGTK_TRACE("GDK_GRAB_BROKEN"); break;
  case GDK_DAMAGE:                PGTK_TRACE("GDK_DAMAGE"); break;
  case GDK_TOUCH_BEGIN:           PGTK_TRACE("GDK_TOUCH_BEGIN"); break;
  case GDK_TOUCH_UPDATE:          PGTK_TRACE("GDK_TOUCH_UPDATE"); break;
  case GDK_TOUCH_END:             PGTK_TRACE("GDK_TOUCH_END"); break;
  case GDK_TOUCH_CANCEL:          PGTK_TRACE("GDK_TOUCH_CANCEL"); break;
  case GDK_TOUCHPAD_SWIPE:        PGTK_TRACE("GDK_TOUCHPAD_SWIPE"); break;
  case GDK_TOUCHPAD_PINCH:        PGTK_TRACE("GDK_TOUCHPAD_PINCH"); break;
  case GDK_PAD_BUTTON_PRESS:      PGTK_TRACE("GDK_PAD_BUTTON_PRESS"); break;
  case GDK_PAD_BUTTON_RELEASE:    PGTK_TRACE("GDK_PAD_BUTTON_RELEASE"); break;
  case GDK_PAD_RING:              PGTK_TRACE("GDK_PAD_RING"); break;
  case GDK_PAD_STRIP:             PGTK_TRACE("GDK_PAD_STRIP"); break;
  case GDK_PAD_GROUP_MODE:        PGTK_TRACE("GDK_PAD_GROUP_MODE"); break;
  default:                        PGTK_TRACE("GDK_EVENT %d", event->type);
  }
  PGTK_TRACE(" Widget is %s", type_name);
#endif
  return FALSE;
}

static void
pgtk_fill_rectangle(struct frame *f, unsigned long color, int x, int y, int width, int height)
{
  PGTK_TRACE("pgtk_fill_rectangle");
  cairo_t *cr;
  cr = pgtk_begin_cr_clip (f);
  pgtk_set_cr_source_with_color (f, color);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
  pgtk_end_cr_clip (f);
}

void
pgtk_clear_under_internal_border (struct frame *f)
{
  PGTK_TRACE("pgtk_clear_under_internal_border");
  if (FRAME_INTERNAL_BORDER_WIDTH (f) > 0)
    {
      int border = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int height = FRAME_PIXEL_HEIGHT (f);
      int margin = 0;
      struct face *face = FACE_FROM_ID_OR_NULL (f, INTERNAL_BORDER_FACE_ID);

      block_input ();

      if (face)
	{
	  pgtk_fill_rectangle (f, color, 0, margin, width, border);
	  pgtk_fill_rectangle (f, color, 0, 0, border, height);
	  pgtk_fill_rectangle (f, color, width - border, 0, border, height);
	  pgtk_fill_rectangle (f, color, 0, height - border, width, border);
	}
      else
	{
	  pgtk_clear_area (f, 0, 0, border, height);
	  pgtk_clear_area (f, 0, margin, width, border);
	  pgtk_clear_area (f, width - border, 0, border, height);
	  pgtk_clear_area (f, 0, height - border, width, border);
	}

      unblock_input ();
    }
}

static void print_widget_tree_recursive(GtkWidget *w, gpointer user_data)
{
  const char *indent = user_data;
  char buf[1024] = "";
  int len = 0;
  len += sprintf(buf + len, "%s", indent);
  len += sprintf(buf + len, "%p %s mapped:%d visible:%d", w, G_OBJECT_TYPE_NAME(w), gtk_widget_get_mapped(w), gtk_widget_get_visible(w));
  gint wd, hi;
  gtk_widget_get_size_request(w, &wd, &hi);
  len += sprintf(buf + len, " size_req:%dx%d", wd, hi);
  GtkAllocation alloc;
  gtk_widget_get_allocation(w, &alloc);
  len += sprintf(buf + len, " alloc:%dx%d+%d+%d", alloc.width, alloc.height, alloc.x, alloc.y);
  len += sprintf(buf + len, " haswin:%d", gtk_widget_get_has_window(w));
  len += sprintf(buf + len, " gdkwin:%p", gtk_widget_get_window(w));
  PGTK_TRACE("%s", buf);

  if (GTK_IS_CONTAINER(w)) {
    strcpy(buf, indent);
    strcat(buf, "  ");
    gtk_container_foreach(GTK_CONTAINER(w), print_widget_tree_recursive, buf);
  }
}

static void print_widget_tree(GtkWidget *w)
{
  char indent[1] = "";
  w = gtk_widget_get_toplevel(w);
  print_widget_tree_recursive(w, indent);
}

static gboolean
pgtk_handle_draw(GtkWidget *widget, cairo_t *cr, gpointer *data)
{
  struct frame *f;

  PGTK_TRACE("pgtk_handle_draw");

  print_widget_tree(widget);

  GdkWindow *win = gtk_widget_get_window(widget);

  PGTK_TRACE("  win=%p", win);
  if (win != NULL) {
    cairo_surface_t *src = NULL;
    f = pgtk_any_window_to_frame(win);
    PGTK_TRACE("  f=%p", f);
    if (f != NULL) {
      src = FRAME_X_OUTPUT(f)->cr_surface_visible_bell;
      if (src == NULL)
	src = FRAME_CR_SURFACE(f);
    }
    PGTK_TRACE("  surface=%p", src);
    if (src != NULL) {
      PGTK_TRACE("  resized_p=%d", f->resized_p);
      PGTK_TRACE("  garbaged=%d", f->garbaged);
      PGTK_TRACE("  scroll_bar_width=%f", (double) PGTK_SCROLL_BAR_WIDTH(f));
      // PGTK_TRACE("  scroll_bar_adjust=%d", PGTK_SCROLL_BAR_ADJUST(f));
      PGTK_TRACE("  scroll_bar_cols=%d", FRAME_SCROLL_BAR_COLS(f));
      PGTK_TRACE("  column_width=%d", FRAME_COLUMN_WIDTH(f));
      cairo_set_source_surface(cr, src, 0, 0);
      cairo_paint(cr);
    }
  }
  return FALSE;
}

static void size_allocate(GtkWidget *widget, GtkAllocation *alloc, gpointer *user_data)
{
  PGTK_TRACE("size-alloc: %dx%d+%d+%d.", alloc->width, alloc->height, alloc->x, alloc->y);

  struct frame *f = pgtk_any_window_to_frame (gtk_widget_get_window(widget));
  if (f) {
    PGTK_TRACE("%dx%d", alloc->width, alloc->height);
    xg_frame_resized(f, alloc->width, alloc->height);
  }
}

int
pgtk_gtk_to_emacs_modifiers (int state)
{
  int mod_ctrl = ctrl_modifier;
  int mod_meta = meta_modifier;
  int mod_alt  = alt_modifier;
  int mod_hyper = hyper_modifier;
  int mod_super = super_modifier;
  Lisp_Object tem;

  tem = Fget (Vx_ctrl_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_ctrl = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_alt_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_alt = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_meta_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_meta = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_hyper_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_hyper = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_super_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_super = XFIXNUM (tem) & INT_MAX;

  return (  ((state & GDK_SHIFT_MASK)     ? shift_modifier : 0)
            | ((state & GDK_CONTROL_MASK) ? mod_ctrl	: 0)
            | ((state & GDK_META_MASK)	  ? mod_meta	: 0)
            | ((state & GDK_MOD1_MASK)	  ? mod_alt	: 0)
            | ((state & GDK_SUPER_MASK)	  ? mod_super	: 0)
            | ((state & GDK_HYPER_MASK)	  ? mod_hyper	: 0));
}

static int
pgtk_emacs_to_gtk_modifiers (EMACS_INT state)
{
  EMACS_INT mod_ctrl = ctrl_modifier;
  EMACS_INT mod_meta = meta_modifier;
  EMACS_INT mod_alt  = alt_modifier;
  EMACS_INT mod_hyper = hyper_modifier;
  EMACS_INT mod_super = super_modifier;

  Lisp_Object tem;

  tem = Fget (Vx_ctrl_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_ctrl = XFIXNUM (tem);
  tem = Fget (Vx_alt_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_alt = XFIXNUM (tem);
  tem = Fget (Vx_meta_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_meta = XFIXNUM (tem);
  tem = Fget (Vx_hyper_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_hyper = XFIXNUM (tem);
  tem = Fget (Vx_super_keysym, Qmodifier_value);
  if (INTEGERP (tem)) mod_super = XFIXNUM (tem);


  return (  ((state & mod_alt)		? GDK_MOD1_MASK    : 0)
            | ((state & mod_super)	? GDK_SUPER_MASK   : 0)
            | ((state & mod_hyper)	? GDK_HYPER_MASK   : 0)
            | ((state & shift_modifier)	? GDK_SHIFT_MASK   : 0)
            | ((state & mod_ctrl)	? GDK_CONTROL_MASK : 0)
            | ((state & mod_meta)	? GDK_META_MASK    : 0));
}

#define IsCursorKey(keysym)       (0xff50 <= (keysym) && (keysym) < 0xff60)
#define IsMiscFunctionKey(keysym) (0xff60 <= (keysym) && (keysym) < 0xff6c)
#define IsKeypadKey(keysym)       (0xff80 <= (keysym) && (keysym) < 0xffbe)
#define IsFunctionKey(keysym)     (0xffbe <= (keysym) && (keysym) < 0xffe1)

static gboolean key_press_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  struct coding_system coding;
  union buffered_input_event inev;
  ptrdiff_t nbytes = 0;
  Mouse_HLInfo *hlinfo;

  USE_SAFE_ALLOCA;

  PGTK_TRACE("key_press_event");

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  struct frame *f = pgtk_any_window_to_frame(gtk_widget_get_window(widget));
  hlinfo = MOUSE_HL_INFO(f);

  /* If mouse-highlight is an integer, input clears out
     mouse highlighting.  */
  if (!hlinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight))
    {
      clear_mouse_face (hlinfo);
      hlinfo->mouse_face_hidden = true;
    }

  if (f != 0)
    {
      guint keysym, orig_keysym;
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
      guint state = event->key.state;

      state |= pgtk_emacs_to_gtk_modifiers (extra_keyboard_modifiers);
      modifiers = state;

      /* This will have to go some day...  */

      /* make_lispy_event turns chars into control chars.
	 Don't do it here because XLookupString is too eager.  */
      state &= ~GDK_CONTROL_MASK;
      state &= ~(GDK_META_MASK
		 | GDK_SUPER_MASK
		 | GDK_HYPER_MASK
		 | GDK_MOD1_MASK);

      nbytes = event->key.length;
      if (nbytes > copy_bufsiz)
	nbytes = copy_bufsiz;
      memcpy(copy_bufptr, event->key.string, nbytes);

      keysym = event->key.keyval;
      orig_keysym = keysym;

      /* Common for all keysym input events.  */
      XSETFRAME (inev.ie.frame_or_window, f);
      inev.ie.modifiers = pgtk_gtk_to_emacs_modifiers (modifiers);
      inev.ie.timestamp = event->key.time;

      /* First deal with keysyms which have defined
	 translations to characters.  */
      if (keysym >= 32 && keysym < 128)
	/* Avoid explicitly decoding each ASCII character.  */
	{
	  inev.ie.kind = ASCII_KEYSTROKE_EVENT;
	  inev.ie.code = keysym;
	  goto done;
	}

      /* Keysyms directly mapped to Unicode characters.  */
      if (keysym >= 0x01000000 && keysym <= 0x0110FFFF)
	{
	  if (keysym < 0x01000080)
	    inev.ie.kind = ASCII_KEYSTROKE_EVENT;
	  else
	    inev.ie.kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	  inev.ie.code = keysym & 0xFFFFFF;
	  goto done;
	}

      /* Now non-ASCII.  */
      if (HASH_TABLE_P (Vpgtk_keysym_table)
	  && (c = Fgethash (make_fixnum (keysym),
			    Vpgtk_keysym_table,
			    Qnil),
	      FIXNATP (c)))
	{
	  inev.ie.kind = (SINGLE_BYTE_CHAR_P (XFIXNAT (c))
			  ? ASCII_KEYSTROKE_EVENT
			  : MULTIBYTE_CHAR_KEYSTROKE_EVENT);
	  inev.ie.code = XFIXNAT (c);
	  goto done;
	}

      /* Random non-modifier sorts of keysyms.  */
      if (((keysym >= GDK_KEY_BackSpace && keysym <= GDK_KEY_Escape)
	   || keysym == GDK_KEY_Delete
#ifdef GDK_KEY_ISO_Left_Tab
	   || (keysym >= GDK_KEY_ISO_Left_Tab
	       && keysym <= GDK_KEY_ISO_Enter)
#endif
	   || IsCursorKey (keysym) /* 0xff50 <= x < 0xff60 */
	   || IsMiscFunctionKey (keysym) /* 0xff60 <= x < VARIES */
#ifdef HPUX
	   /* This recognizes the "extended function
	      keys".  It seems there's no cleaner way.
	      Test IsModifierKey to avoid handling
	      mode_switch incorrectly.  */
	   || (GDK_KEY_Select <= keysym && keysym < GDK_KEY_KP_Space)
#endif
#ifdef GDK_KEY_dead_circumflex
	   || orig_keysym == GDK_KEY_dead_circumflex
#endif
#ifdef GDK_KEY_dead_grave
	   || orig_keysym == GDK_KEY_dead_grave
#endif
#ifdef GDK_KEY_dead_tilde
	   || orig_keysym == GDK_KEY_dead_tilde
#endif
#ifdef GDK_KEY_dead_diaeresis
	   || orig_keysym == GDK_KEY_dead_diaeresis
#endif
#ifdef GDK_KEY_dead_macron
	   || orig_keysym == GDK_KEY_dead_macron
#endif
#ifdef GDK_KEY_dead_degree
	   || orig_keysym == GDK_KEY_dead_degree
#endif
#ifdef GDK_KEY_dead_acute
	   || orig_keysym == GDK_KEY_dead_acute
#endif
#ifdef GDK_KEY_dead_cedilla
	   || orig_keysym == GDK_KEY_dead_cedilla
#endif
#ifdef GDK_KEY_dead_breve
	   || orig_keysym == GDK_KEY_dead_breve
#endif
#ifdef GDK_KEY_dead_ogonek
	   || orig_keysym == GDK_KEY_dead_ogonek
#endif
#ifdef GDK_KEY_dead_caron
	   || orig_keysym == GDK_KEY_dead_caron
#endif
#ifdef GDK_KEY_dead_doubleacute
	   || orig_keysym == GDK_KEY_dead_doubleacute
#endif
#ifdef GDK_KEY_dead_abovedot
	   || orig_keysym == GDK_KEY_dead_abovedot
#endif
	   || IsKeypadKey (keysym) /* 0xff80 <= x < 0xffbe */
	   || IsFunctionKey (keysym) /* 0xffbe <= x < 0xffe1 */
	   /* Any "vendor-specific" key is ok.  */
	   || (orig_keysym & (1 << 28))
	   || (keysym != GDK_KEY_VoidSymbol && nbytes == 0))
	  && ! (event->key.is_modifier
		/* The symbols from GDK_KEY_ISO_Lock
		   to GDK_KEY_ISO_Last_Group_Lock
		   don't have real modifiers but
		   should be treated similarly to
		   Mode_switch by Emacs. */
#if defined GDK_KEY_ISO_Lock && defined GDK_KEY_ISO_Last_Group_Lock
		|| (GDK_KEY_ISO_Lock <= orig_keysym
		    && orig_keysym <= GDK_KEY_ISO_Last_Group_Lock)
#endif
		))
	{
	  STORE_KEYSYM_FOR_DEBUG (keysym);
	  /* make_lispy_event will convert this to a symbolic
	     key.  */
	  inev.ie.kind = NON_ASCII_KEYSTROKE_EVENT;
	  inev.ie.code = keysym;
	  goto done;
	}

      {	/* Raw bytes, not keysym.  */
	ptrdiff_t i;
	int nchars, len;

	for (i = 0, nchars = 0; i < nbytes; i++)
	  {
	    if (ASCII_CHAR_P (copy_bufptr[i]))
	      nchars++;
	    STORE_KEYSYM_FOR_DEBUG (copy_bufptr[i]);
	  }

	if (nchars < nbytes)
	  {
	    /* Decode the input data.  */

	    /* The input should be decoded with `coding_system'
	       which depends on which X*LookupString function
	       we used just above and the locale.  */
	    setup_coding_system (coding_system, &coding);
	    coding.src_multibyte = false;
	    coding.dst_multibyte = true;
	    /* The input is converted to events, thus we can't
	       handle composition.  Anyway, there's no XIM that
	       gives us composition information.  */
	    coding.common_flags &= ~CODING_ANNOTATION_MASK;

	    SAFE_NALLOCA (coding.destination, MAX_MULTIBYTE_LENGTH,
			  nbytes);
	    coding.dst_bytes = MAX_MULTIBYTE_LENGTH * nbytes;
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
	    int ch;
	    if (nchars == nbytes)
	      ch = copy_bufptr[i], len = 1;
	    else
	      ch = string_char_and_length (copy_bufptr + i, &len);
	    inev.ie.kind = (SINGLE_BYTE_CHAR_P (ch)
			    ? ASCII_KEYSTROKE_EVENT
			    : MULTIBYTE_CHAR_KEYSTROKE_EVENT);
	    inev.ie.code = ch;
	    evq_enqueue (&inev);
	  }

	// count += nchars;

	inev.ie.kind = NO_EVENT;  /* Already stored above.  */

	if (keysym == GDK_KEY_VoidSymbol)
	  goto done;
      }
    }

 done:
  if (inev.ie.kind != NO_EVENT)
    {
      XSETFRAME (inev.ie.frame_or_window, f);
      evq_enqueue (&inev);
      // count++;
    }

  SAFE_FREE();

  return TRUE;
}

static gboolean key_release_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  PGTK_TRACE("key_release_event");
  return TRUE;
}

static gboolean configure_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  struct frame *f = pgtk_any_window_to_frame (event->configure.window);
  if (f && widget == FRAME_GTK_OUTER_WIDGET (f)) {
    PGTK_TRACE("%dx%d", event->configure.width, event->configure.height);
    xg_frame_resized(f, event->configure.width, event->configure.height);
  }
  return TRUE;
}

static gboolean map_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  struct frame *f = pgtk_any_window_to_frame (event->any.window);
  union buffered_input_event inev;

  PGTK_TRACE("map_event");

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  if (f)
    {
      bool iconified = FRAME_ICONIFIED_P (f);

      /* Check if fullscreen was specified before we where mapped the
	 first time, i.e. from the command line.  */
      if (!FRAME_X_OUTPUT(f)->has_been_visible)
	{
	  set_fullscreen_state(f);
	}

      if (!iconified)
	{
	  /* The `z-group' is reset every time a frame becomes
	     invisible.  Handle this here.  */
	  if (FRAME_Z_GROUP (f) == z_group_above)
	    x_set_z_group (f, Qabove, Qnil);
	  else if (FRAME_Z_GROUP (f) == z_group_below)
	    x_set_z_group (f, Qbelow, Qnil);
	}

      SET_FRAME_VISIBLE (f, 1);
      SET_FRAME_ICONIFIED (f, false);
      FRAME_X_OUTPUT(f)->has_been_visible = true;

      if (iconified)
	{
	  inev.ie.kind = DEICONIFY_EVENT;
	  XSETFRAME (inev.ie.frame_or_window, f);
	}
      else if (! NILP (Vframe_list) && ! NILP (XCDR (Vframe_list)))
	/* Force a redisplay sooner or later to update the
	   frame titles in case this is the second frame.  */
	record_asynch_buffer_change ();
    }

  if (inev.ie.kind != NO_EVENT)
    evq_enqueue(&inev);
  return FALSE;
}

static gboolean window_state_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  struct frame *f = pgtk_any_window_to_frame (event->window_state.window);
  union buffered_input_event inev;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  if (f) {
    if (event->window_state.new_window_state & GDK_WINDOW_STATE_FOCUSED)
      {
	if (FRAME_ICONIFIED_P (f))
	  {
	    /* Gnome shell does not iconify us when C-z is pressed.
	       It hides the frame.  So if our state says we aren't
	       hidden anymore, treat it as deiconified.  */
	    SET_FRAME_VISIBLE (f, 1);
	    SET_FRAME_ICONIFIED (f, false);
	    FRAME_X_OUTPUT(f)->has_been_visible = true;
	    inev.ie.kind = DEICONIFY_EVENT;
	    XSETFRAME (inev.ie.frame_or_window, f);
	  }
      }
  }

  if (inev.ie.kind != NO_EVENT)
    evq_enqueue(&inev);
  return TRUE;
}

static gboolean delete_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  struct frame *f = pgtk_any_window_to_frame (event->any.window);
  union buffered_input_event inev;

  PGTK_TRACE("delete_event");

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  if (f) {
    inev.ie.kind = DELETE_WINDOW_EVENT;
    XSETFRAME (inev.ie.frame_or_window, f);
  }

  if (inev.ie.kind != NO_EVENT)
    evq_enqueue(&inev);
  return TRUE;
}

/* The focus may have changed.  Figure out if it is a real focus change,
   by checking both FocusIn/Out and Enter/LeaveNotify events.

   Returns FOCUS_IN_EVENT event in *BUFP. */

/* Handle FocusIn and FocusOut state changes for FRAME.
   If FRAME has focus and there exists more than one frame, puts
   a FOCUS_IN_EVENT into *BUFP.  */

static void
x_focus_changed (gboolean is_enter, int state, struct pgtk_display_info *dpyinfo, struct frame *frame, union buffered_input_event *bufp)
{
  if (is_enter)
    {
      if (dpyinfo->x_focus_event_frame != frame)
        {
          x_new_focus_frame (dpyinfo, frame);
          dpyinfo->x_focus_event_frame = frame;

          /* Don't stop displaying the initial startup message
             for a switch-frame event we don't need.  */
          /* When run as a daemon, Vterminal_frame is always NIL.  */
          bufp->ie.arg = (((NILP (Vterminal_frame)
                         || ! FRAME_PGTK_P (XFRAME (Vterminal_frame))
                         || EQ (Fdaemonp (), Qt))
			&& CONSP (Vframe_list)
			&& !NILP (XCDR (Vframe_list)))
		       ? Qt : Qnil);
          bufp->ie.kind = FOCUS_IN_EVENT;
          XSETFRAME (bufp->ie.frame_or_window, frame);
        }

      frame->output_data.pgtk->focus_state |= state;

    }
  else
    {
      frame->output_data.pgtk->focus_state &= ~state;

      if (dpyinfo->x_focus_event_frame == frame)
        {
          dpyinfo->x_focus_event_frame = 0;
          x_new_focus_frame (dpyinfo, 0);

          bufp->ie.kind = FOCUS_OUT_EVENT;
          XSETFRAME (bufp->ie.frame_or_window, frame);
        }

      if (frame->pointer_invisible)
	XTtoggle_invisible_pointer (frame, false);
    }
}

static gboolean
enter_notify_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  PGTK_TRACE("enter_notify_event");
  union buffered_input_event inev;
  struct frame *focus_frame = pgtk_any_window_to_frame(gtk_widget_get_window(widget));
  int focus_state
    = focus_frame ? focus_frame->output_data.pgtk->focus_state : 0;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  if (!(focus_state & FOCUS_EXPLICIT))
    x_focus_changed (TRUE,
		     FOCUS_IMPLICIT,
		     FRAME_DISPLAY_INFO(focus_frame), focus_frame, &inev);
  if (inev.ie.kind != NO_EVENT)
    evq_enqueue (&inev);
  return TRUE;
}

static gboolean
leave_notify_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  PGTK_TRACE("leave_notify_event");
  union buffered_input_event inev;
  struct frame *focus_frame = pgtk_any_window_to_frame(gtk_widget_get_window(widget));
  int focus_state
    = focus_frame ? focus_frame->output_data.pgtk->focus_state : 0;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  if (!(focus_state & FOCUS_EXPLICIT))
    x_focus_changed (FALSE,
		     FOCUS_IMPLICIT,
		     FRAME_DISPLAY_INFO(focus_frame), focus_frame, &inev);
  if (inev.ie.kind != NO_EVENT)
    evq_enqueue (&inev);
  return TRUE;
}

static gboolean
focus_in_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  PGTK_TRACE("focus_in_event");
  union buffered_input_event inev;
  struct frame *frame = pgtk_any_window_to_frame(gtk_widget_get_window(widget));

  if (frame == NULL)
    return TRUE;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  x_focus_changed (TRUE, FOCUS_IMPLICIT,
		   FRAME_DISPLAY_INFO(frame), frame, &inev);
  if (inev.ie.kind != NO_EVENT)
    evq_enqueue (&inev);
  return TRUE;
}

static gboolean
focus_out_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  PGTK_TRACE("focus_out_event");
  union buffered_input_event inev;
  struct frame *frame = pgtk_any_window_to_frame(gtk_widget_get_window(widget));

  if (frame == NULL)
    return TRUE;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  x_focus_changed (FALSE, FOCUS_IMPLICIT,
		   FRAME_DISPLAY_INFO(frame), frame, &inev);
  if (inev.ie.kind != NO_EVENT)
    evq_enqueue(&inev);
  return TRUE;
}

/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static bool
note_mouse_movement (struct frame *frame, const GdkEventMotion *event)
{
  XRectangle *r;
  struct pgtk_display_info *dpyinfo;

  if (!FRAME_X_OUTPUT (frame))
    return false;

  dpyinfo = FRAME_DISPLAY_INFO (frame);
  dpyinfo->last_mouse_movement_time = event->time;
  dpyinfo->last_mouse_motion_frame = frame;
  dpyinfo->last_mouse_motion_x = event->x;
  dpyinfo->last_mouse_motion_y = event->y;

  if (event->window != gtk_widget_get_window(FRAME_GTK_WIDGET (frame)))
    {
      frame->mouse_moved = true;
      dpyinfo->last_mouse_scroll_bar = NULL;
      note_mouse_highlight (frame, -1, -1);
      dpyinfo->last_mouse_glyph_frame = NULL;
      return true;
    }


  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  r = &dpyinfo->last_mouse_glyph;
  if (frame != dpyinfo->last_mouse_glyph_frame
      || event->x < r->x || event->x >= r->x + r->width
      || event->y < r->y || event->y >= r->y + r->height)
    {
      frame->mouse_moved = true;
      dpyinfo->last_mouse_scroll_bar = NULL;
      note_mouse_highlight (frame, event->x, event->y);
      /* Remember which glyph we're now on.  */
      remember_mouse_glyph (frame, event->x, event->y, r);
      dpyinfo->last_mouse_glyph_frame = frame;
      return true;
    }

  return false;
}

static gboolean
motion_notify_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  PGTK_TRACE("motion_notify_event");
  union buffered_input_event inev;
  struct frame *f, *frame;
  struct pgtk_display_info *dpyinfo;
  Mouse_HLInfo *hlinfo;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  previous_help_echo_string = help_echo_string;
  help_echo_string = Qnil;

  frame = pgtk_any_window_to_frame(gtk_widget_get_window(widget));
  dpyinfo = FRAME_DISPLAY_INFO (frame);
  f = (gui_mouse_grabbed (dpyinfo) ? dpyinfo->last_mouse_frame
       : pgtk_any_window_to_frame(gtk_widget_get_window(widget)));
  hlinfo = MOUSE_HL_INFO (f);

  if (hlinfo->mouse_face_hidden)
    {
      hlinfo->mouse_face_hidden = false;
      clear_mouse_face (hlinfo);
    }

  if (f && xg_event_is_for_scrollbar (f, event))
    f = 0;
  if (f)
    {
      /* Maybe generate a SELECT_WINDOW_EVENT for
	 `mouse-autoselect-window' but don't let popup menus
	 interfere with this (Bug#1261).  */
      if (!NILP (Vmouse_autoselect_window)
	  /* Don't switch if we're currently in the minibuffer.
	     This tries to work around problems where the
	     minibuffer gets unselected unexpectedly, and where
	     you then have to move your mouse all the way down to
	     the minibuffer to select it.  */
	  && !MINI_WINDOW_P (XWINDOW (selected_window))
	  /* With `focus-follows-mouse' non-nil create an event
	     also when the target window is on another frame.  */
	  && (f == XFRAME (selected_frame)
	      || !NILP (focus_follows_mouse)))
	{
	  static Lisp_Object last_mouse_window;
	  Lisp_Object window = window_from_coordinates
	    (f, event->motion.x, event->motion.y, 0, false);

	  /* A window will be autoselected only when it is not
	     selected now and the last mouse movement event was
	     not in it.  The remainder of the code is a bit vague
	     wrt what a "window" is.  For immediate autoselection,
	     the window is usually the entire window but for GTK
	     where the scroll bars don't count.  For delayed
	     autoselection the window is usually the window's text
	     area including the margins.  */
	  if (WINDOWP (window)
	      && !EQ (window, last_mouse_window)
	      && !EQ (window, selected_window))
	    {
	      inev.ie.kind = SELECT_WINDOW_EVENT;
	      inev.ie.frame_or_window = window;
	    }

	  /* Remember the last window where we saw the mouse.  */
	  last_mouse_window = window;
	}

      if (!note_mouse_movement (f, &event->motion))
	help_echo_string = previous_help_echo_string;
    }
  else
    {
      /* If we move outside the frame, then we're
	 certainly no longer on any text in the frame.  */
      clear_mouse_face (hlinfo);
    }

  /* If the contents of the global variable help_echo_string
     has changed, generate a HELP_EVENT.  */
  int do_help = 0;
  if (!NILP (help_echo_string)
      || !NILP (previous_help_echo_string))
    do_help = 1;

  if (inev.ie.kind != NO_EVENT)
    evq_enqueue (&inev);

  if (do_help > 0) {
    Lisp_Object frame;
    union buffered_input_event inev;

    if (f)
      XSETFRAME (frame, f);
    else
      frame = Qnil;

    inev.ie.kind = HELP_EVENT;
    inev.ie.frame_or_window = frame;
    inev.ie.arg = help_echo_object;
    inev.ie.x = help_echo_window;
    inev.ie.y = help_echo_string;
    inev.ie.timestamp = help_echo_pos;
    evq_enqueue(&inev);
  }

  return TRUE;
}

/* Mouse clicks and mouse movement.  Rah.

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

/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static Lisp_Object
construct_mouse_click (struct input_event *result,
		       const GdkEventButton *event,
		       struct frame *f)
{
  /* Make the event type NO_EVENT; we'll change that when we decide
     otherwise.  */
  result->kind = MOUSE_CLICK_EVENT;
  result->code = event->button - 1;
  result->timestamp = event->time;
  result->modifiers = (pgtk_gtk_to_emacs_modifiers (event->state)
		       | (event->type == GDK_BUTTON_RELEASE
			  ? up_modifier
			  : down_modifier));

  XSETINT (result->x, event->x);
  XSETINT (result->y, event->y);
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}

static gboolean
button_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  PGTK_TRACE("button_event: type=%d, button=%u.", event->button.type, event->button.button);
  union buffered_input_event inev;
  struct frame *f, *frame;
  struct pgtk_display_info *dpyinfo;

  /* If we decide we want to generate an event to be seen
     by the rest of Emacs, we put it here.  */
  bool tool_bar_p = false;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  /* ignore double click and triple click. */
  if (event->type != GDK_BUTTON_PRESS && event->type != GDK_BUTTON_RELEASE)
    return TRUE;

  frame = pgtk_any_window_to_frame(gtk_widget_get_window(widget));
  dpyinfo = FRAME_DISPLAY_INFO (frame);

  dpyinfo->last_mouse_glyph_frame = NULL;
#if 0
  x_display_set_last_user_time (dpyinfo, event->button.time);
#endif

  if (gui_mouse_grabbed (dpyinfo))
    f = dpyinfo->last_mouse_frame;
  else
    {
      f = pgtk_any_window_to_frame(gtk_widget_get_window(widget));

      if (f && event->button.type == GDK_BUTTON_PRESS
	  && !FRAME_NO_ACCEPT_FOCUS (f))
	{
	  /* When clicking into a child frame or when clicking
	     into a parent frame with the child frame selected and
	     `no-accept-focus' is not set, select the clicked
	     frame.  */
	  struct frame *hf = dpyinfo->highlight_frame;

	  if (FRAME_PARENT_FRAME (f) || (hf && frame_ancestor_p (f, hf)))
	    {
	      block_input ();
#if 0
	      XSetInputFocus (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
			      RevertToParent, CurrentTime);
	      if (FRAME_PARENT_FRAME (f))
		XRaiseWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f));
#endif
	      unblock_input ();
	    }
	}
    }

  if (f && xg_event_is_for_scrollbar (f, event))
    f = 0;
  if (f)
    {
      if (!tool_bar_p)
	{
	  if (ignore_next_mouse_click_timeout)
	    {
	      if (event->type == GDK_BUTTON_PRESS
		  && event->button.time > ignore_next_mouse_click_timeout)
		{
		  ignore_next_mouse_click_timeout = 0;
		  construct_mouse_click (&inev.ie, &event->button, f);
		}
	      if (event->type == GDK_BUTTON_RELEASE)
		ignore_next_mouse_click_timeout = 0;
	    }
	  else
	    construct_mouse_click (&inev.ie, &event->button, f);
	}
#if 0
      if (FRAME_X_EMBEDDED_P (f))
	xembed_send_message (f, event->button.time,
			     XEMBED_REQUEST_FOCUS, 0, 0, 0);
#endif
    }

  if (event->type == GDK_BUTTON_PRESS)
    {
      dpyinfo->grabbed |= (1 << event->button.button);
      dpyinfo->last_mouse_frame = f;
    }
  else
    dpyinfo->grabbed &= ~(1 << event->button.button);

  /* Ignore any mouse motion that happened before this event;
     any subsequent mouse-movement Emacs events should reflect
     only motion after the ButtonPress/Release.  */
  if (f != 0)
    f->mouse_moved = false;

  if (inev.ie.kind != NO_EVENT)
    evq_enqueue (&inev);
  return TRUE;
}

static gboolean
scroll_event(GtkWidget *widget, GdkEvent *event, gpointer *user_data)
{
  PGTK_TRACE("scroll_event");
  union buffered_input_event inev;
  struct frame *f, *frame;
  struct pgtk_display_info *dpyinfo;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;

  frame = pgtk_any_window_to_frame(gtk_widget_get_window(widget));
  dpyinfo = FRAME_DISPLAY_INFO (frame);

  if (gui_mouse_grabbed (dpyinfo))
    f = dpyinfo->last_mouse_frame;
  else
    f = pgtk_any_window_to_frame(gtk_widget_get_window(widget));

  inev.ie.kind = WHEEL_EVENT;
  inev.ie.timestamp = event->scroll.time;
  inev.ie.modifiers = pgtk_gtk_to_emacs_modifiers (event->scroll.state);
  XSETINT (inev.ie.x, event->scroll.x);
  XSETINT (inev.ie.y, event->scroll.y);
  XSETFRAME (inev.ie.frame_or_window, f);
  inev.ie.arg = Qnil;

  switch (event->scroll.direction) {
  case GDK_SCROLL_UP:
    inev.ie.kind = WHEEL_EVENT;
    inev.ie.modifiers |= up_modifier;
    break;
  case GDK_SCROLL_DOWN:
    inev.ie.kind = WHEEL_EVENT;
    inev.ie.modifiers |= down_modifier;
    break;
  case GDK_SCROLL_LEFT:
    inev.ie.kind = HORIZ_WHEEL_EVENT;
    inev.ie.modifiers |= up_modifier;
    break;
  case GDK_SCROLL_RIGHT:
    inev.ie.kind = HORIZ_WHEEL_EVENT;
    inev.ie.modifiers |= down_modifier;
    break;
  case GDK_SCROLL_SMOOTH:
    if (event->scroll.delta_y >= 0.5) {
      inev.ie.kind = WHEEL_EVENT;
      inev.ie.modifiers |= down_modifier;
    } else if (event->scroll.delta_y <= -0.5) {
      inev.ie.kind = WHEEL_EVENT;
      inev.ie.modifiers |= up_modifier;
    } else if (event->scroll.delta_x >= 0.5) {
      inev.ie.kind = HORIZ_WHEEL_EVENT;
      inev.ie.modifiers |= down_modifier;
    } else if (event->scroll.delta_x <= -0.5) {
      inev.ie.kind = HORIZ_WHEEL_EVENT;
      inev.ie.modifiers |= up_modifier;
    } else
      return TRUE;
    break;
  default:
    return TRUE;
  }

  if (inev.ie.kind != NO_EVENT)
    evq_enqueue (&inev);
  return TRUE;
}

static gboolean drag_drop(GtkWidget *widget,
			  GdkDragContext *context,
			  gint x, gint y,
			  guint time_,
			  gpointer user_data)
{
  PGTK_TRACE("drag_drop");
  GdkAtom target = gtk_drag_dest_find_target(widget, context, NULL);
  PGTK_TRACE("drag_drop: target: %p", (void *) target);

  if (target == GDK_NONE) {
    gtk_drag_finish(context, TRUE, FALSE, time_);
    return FALSE;
  }

  gtk_drag_get_data(widget, context, target, time_);

  return TRUE;
}

static void drag_data_received(GtkWidget *widget, GdkDragContext *context,
			       gint x, gint y,
			       GtkSelectionData *data,
			       guint info, guint time_,
			       gpointer user_data)
{
  PGTK_TRACE("drag_data_received:");
  struct frame *f = pgtk_any_window_to_frame(gtk_widget_get_window(widget));
  gchar **uris = gtk_selection_data_get_uris(data);

  if (uris != NULL) {
    for (int i = 0; uris[i] != NULL; i++) {
      union buffered_input_event inev;
      Lisp_Object arg = Qnil;

      PGTK_TRACE("drag_data_received: uri: %s", uris[i]);

      EVENT_INIT (inev.ie);
      inev.ie.kind = NO_EVENT;
      inev.ie.arg = Qnil;

      arg = list2(Qurl, build_string(uris[i]));

      inev.ie.kind = DRAG_N_DROP_EVENT;
      inev.ie.modifiers = 0;
      XSETINT(inev.ie.x, x);
      XSETINT(inev.ie.y, y);
      XSETFRAME(inev.ie.frame_or_window, f);
      inev.ie.arg = arg;
      inev.ie.timestamp = 0;

      evq_enqueue (&inev);
    }
  }
  PGTK_TRACE("drag_data_received: that's all.");

  gtk_drag_finish(context, TRUE, FALSE, time_);
}

void
pgtk_set_event_handler(struct frame *f)
{
  gtk_drag_dest_set(FRAME_GTK_WIDGET(f), GTK_DEST_DEFAULT_ALL, NULL, 0, GDK_ACTION_COPY);
  gtk_drag_dest_add_uri_targets(FRAME_GTK_WIDGET(f));

  g_signal_connect(G_OBJECT(FRAME_GTK_OUTER_WIDGET(f)), "window-state-event", G_CALLBACK(window_state_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_OUTER_WIDGET(f)), "delete-event", G_CALLBACK(delete_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_OUTER_WIDGET(f)), "map-event", G_CALLBACK(map_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_OUTER_WIDGET(f)), "event", G_CALLBACK(pgtk_handle_event), NULL);

  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "size-allocate", G_CALLBACK(size_allocate), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "key-press-event", G_CALLBACK(key_press_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "key-release-event", G_CALLBACK(key_release_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "focus-in-event", G_CALLBACK(focus_in_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "focus-out-event", G_CALLBACK(focus_out_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "enter-notify-event", G_CALLBACK(enter_notify_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "leave-notify-event", G_CALLBACK(leave_notify_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "motion-notify-event", G_CALLBACK(motion_notify_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "button-press-event", G_CALLBACK(button_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "button-release-event", G_CALLBACK(button_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "scroll-event", G_CALLBACK(scroll_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "selection-clear-event", G_CALLBACK(pgtk_selection_lost), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "configure-event", G_CALLBACK(configure_event), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "drag-drop", G_CALLBACK(drag_drop), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "drag-data-received", G_CALLBACK(drag_data_received), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "draw", G_CALLBACK(pgtk_handle_draw), NULL);
  g_signal_connect(G_OBJECT(FRAME_GTK_WIDGET(f)), "event", G_CALLBACK(pgtk_handle_event), NULL);
}

static void
my_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
		const gchar *msg, gpointer user_data)
{
  if (!strstr (msg, "g_set_prgname"))
      fprintf (stderr, "%s-WARNING **: %s", log_domain, msg);
}

/* Test whether two display-name strings agree up to the dot that separates
   the screen number from the server number.  */
static bool
same_x_server (const char *name1, const char *name2)
{
  bool seen_colon = false;
  Lisp_Object sysname = Fsystem_name ();
  const char *system_name = SSDATA (sysname);
  ptrdiff_t system_name_length = SBYTES (sysname);
  ptrdiff_t length_until_period = 0;

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
	seen_colon = true;
      if (seen_colon && *name1 == '.')
	return true;
    }
  return (seen_colon
	  && (*name1 == '.' || *name1 == '\0')
	  && (*name2 == '.' || *name2 == '\0'));
}

/* Open a connection to X display DISPLAY_NAME, and return
   the structure that describes the open display.
   If we cannot contact the display, return null.  */

struct pgtk_display_info *
pgtk_term_init (Lisp_Object display_name, char *resource_name)
{
  GdkDisplay *dpy;
  struct terminal *terminal;
  struct pgtk_display_info *dpyinfo;
  static int x_initialized = 0;
  static unsigned x_display_id = 0;
  static char *initial_display = NULL;
  char *dpy_name;
  Lisp_Object lisp_dpy_name = Qnil;

  block_input ();

  if (!x_initialized)
    {
      Fset_input_interrupt_mode (Qt);
      baud_rate = 19200;

#ifdef USE_CAIRO
      gui_init_fringe (&pgtk_redisplay_interface);
#endif

      ++x_initialized;
    }

  dpy_name = SSDATA (display_name);
  if (strlen(dpy_name) == 0 && initial_display != NULL)
    dpy_name = initial_display;
  lisp_dpy_name = build_string (dpy_name);

  {
#define NUM_ARGV 10
    int argc;
    char *argv[NUM_ARGV];
    char **argv2 = argv;
    guint id;

    if (x_initialized++ > 1)
      {
	xg_display_open (dpy_name, &dpy);
      }
    else
      {
        static char display_opt[] = "--display";
        static char name_opt[] = "--name";

        for (argc = 0; argc < NUM_ARGV; ++argc)
          argv[argc] = 0;

        argc = 0;
        argv[argc++] = initial_argv[0];

        if (strlen(dpy_name) != 0)
          {
            argv[argc++] = display_opt;
            argv[argc++] = dpy_name;
          }

        argv[argc++] = name_opt;
        argv[argc++] = resource_name;

	/* Work around GLib bug that outputs a faulty warning. See
	   https://bugzilla.gnome.org/show_bug.cgi?id=563627.  */
	id = g_log_set_handler ("GLib", G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL
				  | G_LOG_FLAG_RECURSION, my_log_handler, NULL);

	/* gtk_init does set_locale.  Fix locale before and after.  */
	fixup_locale ();
	unrequest_sigio (); /* See comment in x_display_ok.  */
	gtk_init (&argc, &argv2);
	request_sigio ();
	fixup_locale ();


        g_log_remove_handler ("GLib", id);

        xg_initialize ();

        dpy = DEFAULT_GDK_DISPLAY ();

	initial_display = g_strdup (gdk_display_get_name(dpy));
	dpy_name = initial_display;
	lisp_dpy_name = build_string(dpy_name);
      }
  }

  /* Detect failure.  */
  if (dpy == 0)
    {
      unblock_input ();
      return 0;
    }


  dpyinfo = xzalloc (sizeof *dpyinfo);
  pgtk_initialize_display_info (dpyinfo);
  terminal = pgtk_create_terminal (dpyinfo);

  {
    struct pgtk_display_info *share;

    for (share = x_display_list; share; share = share->next)
      if (same_x_server (SSDATA (XCAR (share->name_list_element)), dpy_name))
	break;
    if (share)
      terminal->kboard = share->terminal->kboard;
    else
      {
	terminal->kboard = allocate_kboard (Qpgtk);

	/* Don't let the initial kboard remain current longer than necessary.
	   That would cause problems if a file loaded on startup tries to
	   prompt in the mini-buffer.  */
	if (current_kboard == initial_kboard)
	  current_kboard = terminal->kboard;
      }
    terminal->kboard->reference_count++;
  }

  /* Put this display on the chain.  */
  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;

  dpyinfo->name_list_element = Fcons (lisp_dpy_name, Qnil);
  dpyinfo->gdpy = dpy;

  /* https://lists.gnu.org/r/emacs-devel/2015-11/msg00194.html  */
  dpyinfo->smallest_font_height = 1;
  dpyinfo->smallest_char_width = 1;

  /* Set the name of the terminal. */
  terminal->name = xlispstrdup (lisp_dpy_name);

  Lisp_Object system_name = Fsystem_name ();
  ptrdiff_t nbytes;
  if (INT_ADD_WRAPV (SBYTES (Vinvocation_name), SBYTES (system_name) + 2,
		     &nbytes))
    memory_full (SIZE_MAX);
  dpyinfo->x_id = ++x_display_id;
  dpyinfo->x_id_name = xmalloc (nbytes);
  char *nametail = lispstpcpy (dpyinfo->x_id_name, Vinvocation_name);
  *nametail++ = '@';
  lispstpcpy (nametail, system_name);

#if 0
  /* Figure out which modifier bits mean what.  */
  x_find_modifier_meanings (dpyinfo);
#endif

  /* Get the scroll bar cursor.  */
  /* We must create a GTK cursor, it is required for GTK widgets.  */
  dpyinfo->xg_cursor = xg_create_default_cursor (dpyinfo->gdpy);

#if 0
  dpyinfo->vertical_scroll_bar_cursor
    = XCreateFontCursor (dpyinfo->display, XC_sb_v_double_arrow);

  dpyinfo->horizontal_scroll_bar_cursor
    = XCreateFontCursor (dpyinfo->display, XC_sb_h_double_arrow);
#endif

  reset_mouse_highlight (&dpyinfo->mouse_highlight);

  {
    GdkScreen *gscr = gdk_display_get_default_screen(dpyinfo->gdpy);
    gdouble dpi = gdk_screen_get_resolution(gscr);
    dpyinfo->resx = dpi;
    dpyinfo->resy = dpi;
  }

#if 0
  x_setup_pointer_blanking (dpyinfo);
#endif

  xsettings_initialize (dpyinfo);

#ifdef F_SETOWN
  fcntl (dpyinfo->connection, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN) */

  if (interrupt_input)
    init_sigio (dpyinfo->connection);

  pgtk_selection_init();

  unblock_input ();

  return dpyinfo;
}

/* Get rid of display DPYINFO, deleting all frames on it,
   and without sending any more commands to the X server.  */

static void
pgtk_delete_display (struct pgtk_display_info *dpyinfo)
{
  struct terminal *t;

  /* Close all frames and delete the generic struct terminal for this
     X display.  */
  for (t = terminal_list; t; t = t->next_terminal)
    if (t->type == output_pgtk && t->display_info.pgtk == dpyinfo)
      {
        delete_terminal (t);
        break;
      }

  if (x_display_list == dpyinfo)
    x_display_list = dpyinfo->next;
  else
    {
      struct pgtk_display_info *tail;

      for (tail = x_display_list; tail; tail = tail->next)
	if (tail->next == dpyinfo)
	  tail->next = tail->next->next;
    }

  xfree (dpyinfo);
}

char *
pgtk_xlfd_to_fontname (const char *xlfd)
/* --------------------------------------------------------------------------
    Convert an X font name (XLFD) to an Gtk font name.
    Only family is used.
    The string returned is temporarily allocated.
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("pgtk_xlfd_to_fontname");
  char *name = xmalloc (180);

  if (!strncmp (xlfd, "--", 2)) {
    if (sscanf (xlfd, "--%179[^-]-", name) != 1)
      name[0] = '\0';
  } else {
    if (sscanf (xlfd, "-%*[^-]-%179[^-]-", name) != 1)
      name[0] = '\0';
  }

  /* stopgap for malformed XLFD input */
  if (strlen (name) == 0)
    strcpy (name, "Monospace");

  PGTK_TRACE("converted '%s' to '%s'", xlfd, name);
  return name;
}

bool
pgtk_defined_color (struct frame *f,
                  const char *name,
                  Emacs_Color *color_def,
                  bool alloc,
                  bool makeIndex)
/* --------------------------------------------------------------------------
         Return true if named color found, and set color_def rgb accordingly.
         If makeIndex and alloc are nonzero put the color in the color_table,
         and set color_def pixel to the resulting index.
         If makeIndex is zero, set color_def pixel to ARGB.
         Return false if not found
   -------------------------------------------------------------------------- */
{
  // PGTK_TRACE("pgtk_defined_color(%s)", name);
  int r;

  block_input ();
  r = pgtk_parse_color (name, color_def);
  unblock_input ();
  return r;
}

/* On frame F, translate the color name to RGB values.  Use cached
   information, if possible.

   Note that there is currently no way to clean old entries out of the
   cache.  However, it is limited to names in the server's database,
   and names we've actually looked up; list-colors-display is probably
   the most color-intensive case we're likely to hit.  */

int pgtk_parse_color (const char *color_name, Emacs_Color *color)
{
  PGTK_TRACE("pgtk_parse_color: %s", color_name);

  GdkRGBA rgba;
  if (gdk_rgba_parse(&rgba, color_name)) {
    color->red = rgba.red * 65535;
    color->green = rgba.green * 65535;
    color->blue = rgba.blue * 65535;
    color->pixel =
      (unsigned long) 0xff << 24 |
      (color->red >> 8) << 16 |
      (color->green >> 8) << 8 |
      (color->blue >> 8) << 0;
    return 1;
  }
  return 0;
}

int
pgtk_lisp_to_color (Lisp_Object color, Emacs_Color *col)
/* --------------------------------------------------------------------------
     Convert a Lisp string object to a NS color
   -------------------------------------------------------------------------- */
{
  PGTK_TRACE("pgtk_lisp_to_color");
  if (STRINGP (color))
    return !pgtk_parse_color (SSDATA (color), col);
  else if (SYMBOLP (color))
    return !pgtk_parse_color (SSDATA (SYMBOL_NAME (color)), col);
  return 1;
}

/* On frame F, translate pixel colors to RGB values for the NCOLORS
   colors in COLORS.  On W32, we no longer try to map colors to
   a palette.  */
void
pgtk_query_colors (struct frame *f, Emacs_Color *colors, int ncolors)
{
  PGTK_TRACE("pgtk_query_colors");
  int i;

  for (i = 0; i < ncolors; i++)
    {
      unsigned long pixel = colors[i].pixel;
      /* Convert to a 16 bit value in range 0 - 0xffff. */
#define GetRValue(p) (((p) >> 16) & 0xff)
#define GetGValue(p) (((p) >> 8) & 0xff)
#define GetBValue(p) (((p) >> 0) & 0xff)
      colors[i].red = GetRValue (pixel) * 257;
      colors[i].green = GetGValue (pixel) * 257;
      colors[i].blue = GetBValue (pixel) * 257;
      PGTK_TRACE("pixel: %lx, red: %d, blue %d, green %d", colors[i].pixel, colors[i].red, colors[i].blue, colors[i].green);
    }
}

void
pgtk_query_color (struct frame *f, Emacs_Color *color)
{
  PGTK_TRACE("pgtk_query_color");
  pgtk_query_colors (f, color, 1);
}

void
pgtk_clear_area (struct frame *f, int x, int y, int width, int height)
{
  PGTK_TRACE("pgtk_clear_area: %dx%d+%d+%d.", width, height, x, y);
  cairo_t *cr;

  eassert (width > 0 && height > 0);

  cr = pgtk_begin_cr_clip (f);
  PGTK_TRACE("back color %08lx.", (unsigned long) FRAME_X_OUTPUT(f)->background_color);
  pgtk_set_cr_source_with_color (f, FRAME_X_OUTPUT(f)->background_color);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
  pgtk_end_cr_clip (f);
}


void
syms_of_pgtkterm (void)
{
  /* from 23+ we need to tell emacs what modifiers there are.. */
  DEFSYM (Qmodifier_value, "modifier-value");
  DEFSYM (Qalt, "alt");
  DEFSYM (Qhyper, "hyper");
  DEFSYM (Qmeta, "meta");
  DEFSYM (Qsuper, "super");
  DEFSYM (Qcontrol, "control");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");

  DEFSYM (Qfile, "file");
  DEFSYM (Qurl, "url");

  DEFSYM (Qlatin_1, "latin-1");

  xg_default_icon_file = build_pure_c_string ("icons/hicolor/scalable/apps/emacs.svg");
  staticpro (&xg_default_icon_file);

  DEFSYM (Qx_gtk_map_stock, "x-gtk-map-stock");


  Fput (Qalt, Qmodifier_value, make_fixnum (alt_modifier));
  Fput (Qhyper, Qmodifier_value, make_fixnum (hyper_modifier));
  Fput (Qmeta, Qmodifier_value, make_fixnum (meta_modifier));
  Fput (Qsuper, Qmodifier_value, make_fixnum (super_modifier));
  Fput (Qcontrol, Qmodifier_value, make_fixnum (ctrl_modifier));

  DEFVAR_LISP ("x-ctrl-keysym", Vx_ctrl_keysym,
    doc: /* Which keys Emacs uses for the ctrl modifier.
This should be one of the symbols `ctrl', `alt', `hyper', `meta',
`super'.  For example, `ctrl' means use the Ctrl_L and Ctrl_R keysyms.
The default is nil, which is the same as `ctrl'.  */);
  Vx_ctrl_keysym = Qnil;

  DEFVAR_LISP ("x-alt-keysym", Vx_alt_keysym,
    doc: /* Which keys Emacs uses for the alt modifier.
This should be one of the symbols `ctrl', `alt', `hyper', `meta',
`super'.  For example, `alt' means use the Alt_L and Alt_R keysyms.
The default is nil, which is the same as `alt'.  */);
  Vx_alt_keysym = Qnil;

  DEFVAR_LISP ("x-hyper-keysym", Vx_hyper_keysym,
    doc: /* Which keys Emacs uses for the hyper modifier.
This should be one of the symbols `ctrl', `alt', `hyper', `meta',
`super'.  For example, `hyper' means use the Hyper_L and Hyper_R
keysyms.  The default is nil, which is the same as `hyper'.  */);
  Vx_hyper_keysym = Qnil;

  DEFVAR_LISP ("x-meta-keysym", Vx_meta_keysym,
    doc: /* Which keys Emacs uses for the meta modifier.
This should be one of the symbols `ctrl', `alt', `hyper', `meta',
`super'.  For example, `meta' means use the Meta_L and Meta_R keysyms.
The default is nil, which is the same as `meta'.  */);
  Vx_meta_keysym = Qnil;

  DEFVAR_LISP ("x-super-keysym", Vx_super_keysym,
    doc: /* Which keys Emacs uses for the super modifier.
This should be one of the symbols `ctrl', `alt', `hyper', `meta',
`super'.  For example, `super' means use the Super_L and Super_R
keysyms.  The default is nil, which is the same as `super'.  */);
  Vx_super_keysym = Qnil;

  /* TODO: move to common code */
  DEFVAR_LISP ("x-toolkit-scroll-bars", Vx_toolkit_scroll_bars,
	       doc: /* Which toolkit scroll bars Emacs uses, if any.
A value of nil means Emacs doesn't use toolkit scroll bars.
With the X Window system, the value is a symbol describing the
X toolkit.  Possible values are: gtk, motif, xaw, or xaw3d.
With MS Windows or Nextstep, the value is t.  */);
  // Vx_toolkit_scroll_bars = Qt;
  Vx_toolkit_scroll_bars = intern_c_string ("gtk");

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /*Non-nil means make use of UNDERLINE_POSITION font properties.
A value of nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, for example 7x13 on XFree prior
to 4.1, set this to nil. */);
  x_use_underline_position_properties = 0;

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* Non-nil means to draw the underline at the same place as the descent line.
A value of nil means to draw the underline according to the value of the
variable `x-use-underline-position-properties', which is usually at the
baseline level.  The default value is nil.  */);
  x_underline_at_descent_line = 0;

  DEFVAR_BOOL ("x-gtk-use-window-move", x_gtk_use_window_move,
    doc: /* Non-nil means rely on gtk_window_move to set frame positions.
If this variable is t (the default), the GTK build uses the function
gtk_window_move to set or store frame positions and disables some time
consuming frame position adjustments.  In newer versions of GTK, Emacs
always uses gtk_window_move and ignores the value of this variable.  */);
  x_gtk_use_window_move = true;

  DEFSYM (Qx_gtk_map_stock, "x-gtk-map-stock");

  DEFVAR_LISP ("pgtk-wait-for-event-timeout", Vpgtk_wait_for_event_timeout,
    doc: /* How long to wait for X events.

Emacs will wait up to this many seconds to receive X events after
making changes which affect the state of the graphical interface.
Under some window managers this can take an indefinite amount of time,
so it is important to limit the wait.

If set to a non-float value, there will be no wait at all.  */);
  Vpgtk_wait_for_event_timeout = make_float (0.1);

  DEFVAR_LISP ("pgtk-keysym-table", Vpgtk_keysym_table,
    doc: /* Hash table of character codes indexed by X keysym codes.  */);
  Vpgtk_keysym_table = make_hash_table (hashtest_eql, 900,
					DEFAULT_REHASH_SIZE,
					DEFAULT_REHASH_THRESHOLD,
					Qnil, false);

  window_being_scrolled = Qnil;
  staticpro(&window_being_scrolled);

  /* Tell Emacs about this window system.  */
  Fprovide (Qpgtk, Qnil);

}

cairo_t *
pgtk_begin_cr_clip (struct frame *f)
{
  cairo_t *cr = FRAME_CR_CONTEXT (f);

  PGTK_TRACE("pgtk_begin_cr_clip");
  if (! FRAME_CR_SURFACE (f))
    {
      FRAME_CR_SURFACE(f) = gdk_window_create_similar_surface(gtk_widget_get_window (FRAME_GTK_WIDGET (f)),
							      CAIRO_CONTENT_COLOR_ALPHA,
							      FRAME_PIXEL_WIDTH (f),
							      FRAME_PIXEL_HEIGHT (f));
    }

  if (!cr)
    {
      cr = cairo_create (FRAME_CR_SURFACE (f));
      FRAME_CR_CONTEXT (f) = cr;
    }

  cairo_save (cr);

  return cr;
}

void
pgtk_end_cr_clip (struct frame *f)
{
  PGTK_TRACE("pgtk_end_cr_clip");
  cairo_restore (FRAME_CR_CONTEXT (f));

  GtkWidget *widget = FRAME_GTK_WIDGET(f);
  gtk_widget_queue_draw(widget);
}

void
pgtk_set_cr_source_with_gc_foreground (struct frame *f, Emacs_GC *gc)
{
  PGTK_TRACE("pgtk_set_cr_source_with_gc_foreground: %08lx", gc->foreground);
  pgtk_set_cr_source_with_color(f, gc->foreground);
}

void
pgtk_set_cr_source_with_gc_background (struct frame *f, Emacs_GC *gc)
{
  PGTK_TRACE("pgtk_set_cr_source_with_gc_background: %08lx", gc->background);
  pgtk_set_cr_source_with_color(f, gc->background);
}

void
pgtk_set_cr_source_with_color (struct frame *f, unsigned long color)
{
  PGTK_TRACE("pgtk_set_cr_source_with_color: %08lx.", color);
  Emacs_Color col;
  col.pixel = color;
  pgtk_query_color(f, &col);
  cairo_set_source_rgb (FRAME_CR_CONTEXT (f), col.red / 65535.0,
			col.green / 65535.0, col.blue / 65535.0);
}

void
pgtk_cr_draw_frame (cairo_t *cr, struct frame *f)
{
  PGTK_TRACE("pgtk_cr_draw_frame");
  cairo_set_source_surface(cr, FRAME_CR_SURFACE(f), 0, 0);
  cairo_paint(cr);
}

void
pgtk_cr_destroy_surface(struct frame *f)
{
  PGTK_TRACE("pgtk_cr_destroy_surface");
  if (FRAME_CR_CONTEXT(f) != NULL) {
    cairo_destroy(FRAME_CR_CONTEXT(f));
    FRAME_CR_CONTEXT(f) = NULL;
  }
  if (FRAME_CR_SURFACE(f) != NULL) {
    cairo_surface_destroy(FRAME_CR_SURFACE(f));
    FRAME_CR_SURFACE(f) = NULL;
  }
  SET_FRAME_GARBAGED (f);
}

void
init_pgtkterm (void)
{
}
