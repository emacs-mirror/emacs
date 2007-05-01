/* Functions for creating and updating GTK widgets.
   Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"

#ifdef USE_GTK
#include <string.h>
#include <signal.h>
#include <stdio.h>
#include "lisp.h"
#include "xterm.h"
#include "blockinput.h"
#include "syssignal.h"
#include "window.h"
#include "atimer.h"
#include "gtkutil.h"
#include "termhooks.h"
#include "keyboard.h"
#include "charset.h"
#include "coding.h"
#include <gdk/gdkkeysyms.h>


#define FRAME_TOTAL_PIXEL_HEIGHT(f) \
  (FRAME_PIXEL_HEIGHT (f) + FRAME_MENUBAR_HEIGHT (f) + FRAME_TOOLBAR_HEIGHT (f))

/* Avoid "differ in sign" warnings */
#define SSDATA(x)  ((char *) SDATA (x))


/***********************************************************************
                      Display handling functions
 ***********************************************************************/

#ifdef HAVE_GTK_MULTIDISPLAY

/* Return the GdkDisplay that corresponds to the X display DPY.  */

static GdkDisplay *
xg_get_gdk_display (dpy)
     Display *dpy;
{
  return gdk_x11_lookup_xdisplay (dpy);
}

/* When the GTK widget W is to be created on a display for F that
   is not the default display, set the display for W.
   W can be a GtkMenu or a GtkWindow widget.  */

static void
xg_set_screen (w, f)
     GtkWidget *w;
     FRAME_PTR f;
{
  if (FRAME_X_DISPLAY (f) != GDK_DISPLAY ())
    {
      GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (FRAME_X_DISPLAY (f));
      GdkScreen *gscreen = gdk_display_get_default_screen (gdpy);

      if (GTK_IS_MENU (w))
        gtk_menu_set_screen (GTK_MENU (w), gscreen);
      else
        gtk_window_set_screen (GTK_WINDOW (w), gscreen);
    }
}


#else /* not HAVE_GTK_MULTIDISPLAY */

/* Make some defines so we can use the GTK 2.2 functions when
   compiling with GTK 2.0.  */

#define xg_set_screen(w, f)
#define gdk_xid_table_lookup_for_display(dpy, w)    gdk_xid_table_lookup (w)
#define gdk_pixmap_foreign_new_for_display(dpy, p)  gdk_pixmap_foreign_new (p)
#define gdk_cursor_new_for_display(dpy, c)          gdk_cursor_new (c)
#define gdk_x11_lookup_xdisplay(dpy)                0
#define GdkDisplay                                  void

#endif /* not HAVE_GTK_MULTIDISPLAY */

/* Open a display named by DISPLAY_NAME.  The display is returned in *DPY.
   *DPY is set to NULL if the display can't be opened.

   Returns non-zero if display could be opened, zero if display could not
   be opened, and less than zero if the GTK version doesn't support
   multipe displays.  */

int
xg_display_open (display_name, dpy)
     char *display_name;
     Display **dpy;
{
#ifdef HAVE_GTK_MULTIDISPLAY
  GdkDisplay *gdpy;

  gdpy = gdk_display_open (display_name);
  *dpy = gdpy ? GDK_DISPLAY_XDISPLAY (gdpy) : NULL;

  return gdpy != NULL;

#else /* not HAVE_GTK_MULTIDISPLAY */

  return -1;
#endif /* not HAVE_GTK_MULTIDISPLAY */
}


/* Close display DPY.  */

void
xg_display_close (Display *dpy)
{
#ifdef HAVE_GTK_MULTIDISPLAY
  GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (dpy);

  /* If this is the default display, we must change it before calling
     dispose, otherwise it will crash on some Gtk+ versions.  */
  if (gdk_display_get_default () == gdpy)
    {
      struct x_display_info *dpyinfo;
      Display *new_dpy = 0;
      GdkDisplay *gdpy_new;

      /* Find another display.  */
      for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
        if (dpyinfo->display != dpy)
          {
            new_dpy = dpyinfo->display;
            break;
          }

      if (! new_dpy) return; /* Emacs will exit anyway.  */

      gdpy_new = gdk_x11_lookup_xdisplay (new_dpy);
      gdk_display_manager_set_default_display (gdk_display_manager_get (),
                                               gdpy_new);
    }

  /* GTK 2.2-2.8 has a bug that makes gdk_display_close crash (bug
     http://bugzilla.gnome.org/show_bug.cgi?id=85715).  This way
     we can continue running, but there will be memory leaks.  */

#if GTK_MAJOR_VERSION == 2 && GTK_MINOR_VERSION < 10
  g_object_run_dispose (G_OBJECT (gdpy));
#else
  /* This seems to be fixed in GTK 2.10. */
  gdk_display_close (gdpy);
#endif
#endif /* HAVE_GTK_MULTIDISPLAY */
}


/***********************************************************************
                      Utility functions
 ***********************************************************************/
/* The timer for scroll bar repetition and menu bar timeouts.
   NULL if no timer is started.  */
static struct atimer *xg_timer;


/* The next two variables and functions are taken from lwlib.  */
static widget_value *widget_value_free_list;
static int malloc_cpt;

/* Allocate a widget_value structure, either by taking one from the
   widget_value_free_list or by malloc:ing a new one.

   Return a pointer to the allocated structure.  */

widget_value *
malloc_widget_value ()
{
  widget_value *wv;
  if (widget_value_free_list)
    {
      wv = widget_value_free_list;
      widget_value_free_list = wv->free_list;
      wv->free_list = 0;
    }
  else
    {
      wv = (widget_value *) xmalloc (sizeof (widget_value));
      malloc_cpt++;
    }
  memset (wv, 0, sizeof (widget_value));
  return wv;
}

/* This is analogous to free.  It frees only what was allocated
   by malloc_widget_value, and no substructures.  */

void
free_widget_value (wv)
     widget_value *wv;
{
  if (wv->free_list)
    abort ();

  if (malloc_cpt > 25)
    {
      /* When the number of already allocated cells is too big,
	 We free it.  */
      free (wv);
      malloc_cpt--;
    }
  else
    {
      wv->free_list = widget_value_free_list;
      widget_value_free_list = wv;
    }
}


/* Create and return the cursor to be used for popup menus and
   scroll bars on display DPY.  */

GdkCursor *
xg_create_default_cursor (dpy)
     Display *dpy;
{
  GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (dpy);
  return gdk_cursor_new_for_display (gdpy, GDK_LEFT_PTR);
}

/* Apply GMASK to GPIX and return a GdkPixbuf with an alpha channel.  */

static GdkPixbuf *
xg_get_pixbuf_from_pix_and_mask (gpix, gmask, cmap)
     GdkPixmap *gpix;
     GdkPixmap *gmask;
     GdkColormap *cmap;
{
  int x, y, width, height, rowstride, mask_rowstride;
  GdkPixbuf *icon_buf, *tmp_buf;
  guchar *pixels;
  guchar *mask_pixels;

  gdk_drawable_get_size (gpix, &width, &height);
  tmp_buf = gdk_pixbuf_get_from_drawable (NULL, gpix, cmap,
                                          0, 0, 0, 0, width, height);
  icon_buf = gdk_pixbuf_add_alpha (tmp_buf, FALSE, 0, 0, 0);
  g_object_unref (G_OBJECT (tmp_buf));

  if (gmask)
    {
      GdkPixbuf *mask_buf = gdk_pixbuf_get_from_drawable (NULL,
                                                          gmask,
                                                          NULL,
                                                          0, 0, 0, 0,
                                                          width, height);
      guchar *pixels = gdk_pixbuf_get_pixels (icon_buf);
      guchar *mask_pixels = gdk_pixbuf_get_pixels (mask_buf);
      int rowstride = gdk_pixbuf_get_rowstride (icon_buf);
      int mask_rowstride = gdk_pixbuf_get_rowstride (mask_buf);
      int y;

      for (y = 0; y < height; ++y)
        {
          guchar *iconptr, *maskptr;
          int x;

          iconptr = pixels + y * rowstride;
          maskptr = mask_pixels + y * mask_rowstride;

          for (x = 0; x < width; ++x)
            {
              /* In a bitmap, RGB is either 255/255/255 or 0/0/0.  Checking
                 just R is sufficient.  */
              if (maskptr[0] == 0)
                iconptr[3] = 0; /* 0, 1, 2 is R, G, B.  3 is alpha.  */

              iconptr += rowstride/width;
              maskptr += mask_rowstride/width;
            }
        }

      g_object_unref (G_OBJECT (mask_buf));
    }

  return icon_buf;
}

/* For the image defined in IMG, make and return a GtkImage.  For displays with
   8 planes or less we must make a GdkPixbuf and apply the mask manually.
   Otherwise the highlightning and dimming the tool bar code in GTK does
   will look bad.  For display with more than 8 planes we just use the
   pixmap and mask directly.  For monochrome displays, GTK doesn't seem
   able to use external pixmaps, it looks bad whatever we do.
   The image is defined on the display where frame F is.
   WIDGET is used to find the GdkColormap to use for the GdkPixbuf.
   If OLD_WIDGET is NULL, a new widget is constructed and returned.
   If OLD_WIDGET is not NULL, that widget is modified.  */

static GtkWidget *
xg_get_image_for_pixmap (f, img, widget, old_widget)
     FRAME_PTR f;
     struct image *img;
     GtkWidget *widget;
     GtkImage *old_widget;
{
  GdkPixmap *gpix;
  GdkPixmap *gmask;
  GdkDisplay *gdpy;

  /* If we have a file, let GTK do all the image handling.
     This seems to be the only way to make insensitive and activated icons
     look good in all cases.  */
  Lisp_Object specified_file = Qnil;
  Lisp_Object tail;
  Lisp_Object file;
  extern Lisp_Object QCfile;

  for (tail = XCDR (img->spec);
       NILP (specified_file) && CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    if (EQ (XCAR (tail), QCfile))
      specified_file = XCAR (XCDR (tail));

  /* We already loaded the image once before calling this
     function, so this only fails if the image file has been removed.
     In that case, use the pixmap already loaded.  */

  if (STRINGP (specified_file)
      && STRINGP (file = x_find_image_file (specified_file)))
    {
      if (! old_widget)
        old_widget = GTK_IMAGE (gtk_image_new_from_file (SSDATA (file)));
      else
        gtk_image_set_from_file (old_widget, SSDATA (file));

      return GTK_WIDGET (old_widget);
    }

  /* No file, do the image handling ourselves.  This will look very bad
     on a monochrome display, and sometimes bad on all displays with
     certain themes.  */

  gdpy = gdk_x11_lookup_xdisplay (FRAME_X_DISPLAY (f));
  gpix = gdk_pixmap_foreign_new_for_display (gdpy, img->pixmap);
  gmask = img->mask ? gdk_pixmap_foreign_new_for_display (gdpy, img->mask) : 0;

  if (x_screen_planes (f) > 8 || x_screen_planes (f) == 1)
    {
      if (! old_widget)
        old_widget = GTK_IMAGE (gtk_image_new_from_pixmap (gpix, gmask));
      else
        gtk_image_set_from_pixmap (old_widget, gpix, gmask);
    }
  else
    {

      /* This is a workaround to make icons look good on pseudo color
         displays.  Apparently GTK expects the images to have an alpha
         channel.  If they don't, insensitive and activated icons will
         look bad.  This workaround does not work on monochrome displays,
         and is not needed on true color/static color displays (i.e.
         16 bits and higher).  */
      GdkColormap *cmap = gtk_widget_get_colormap (widget);
      GdkPixbuf *icon_buf = xg_get_pixbuf_from_pix_and_mask (gpix, gmask, cmap);

      if (! old_widget)
        old_widget = GTK_IMAGE (gtk_image_new_from_pixbuf (icon_buf));
      else
        gtk_image_set_from_pixbuf (old_widget, icon_buf);

      g_object_unref (G_OBJECT (icon_buf));
    }

  g_object_unref (G_OBJECT (gpix));
  if (gmask) g_object_unref (G_OBJECT (gmask));

  return GTK_WIDGET (old_widget);
}


/* Set CURSOR on W and all widgets W contain.  We must do like this
   for scroll bars and menu because they create widgets internally,
   and it is those widgets that are visible.  */

static void
xg_set_cursor (w, cursor)
     GtkWidget *w;
     GdkCursor *cursor;
{
  GList *children = gdk_window_peek_children (w->window);

  gdk_window_set_cursor (w->window, cursor);

  /* The scroll bar widget has more than one GDK window (had to look at
     the source to figure this out), and there is no way to set cursor
     on widgets in GTK.  So we must set the cursor for all GDK windows.
     Ditto for menus.  */

  for ( ; children; children = g_list_next (children))
    gdk_window_set_cursor (GDK_WINDOW (children->data), cursor);
}

/*  Timer function called when a timeout occurs for xg_timer.
    This function processes all GTK events in a recursive event loop.
    This is done because GTK timer events are not seen by Emacs event
    detection, Emacs only looks for X events.  When a scroll bar has the
    pointer (detected by button press/release events below) an Emacs
    timer is started, and this function can then check if the GTK timer
    has expired by calling the GTK event loop.
    Also, when a menu is active, it has a small timeout before it
    pops down the sub menu under it.  */

static void
xg_process_timeouts (timer)
     struct atimer *timer;
{
  BLOCK_INPUT;
  /* Ideally we would like to just handle timer events, like the Xt version
     of this does in xterm.c, but there is no such feature in GTK.  */
  while (gtk_events_pending ())
    gtk_main_iteration ();
  UNBLOCK_INPUT;
}

/* Start the xg_timer with an interval of 0.1 seconds, if not already started.
   xg_process_timeouts is called when the timer expires.  The timer
   started is continuous, i.e. runs until xg_stop_timer is called.  */

static void
xg_start_timer ()
{
  if (! xg_timer)
    {
      EMACS_TIME interval;
      EMACS_SET_SECS_USECS (interval, 0, 100000);
      xg_timer = start_atimer (ATIMER_CONTINUOUS,
                               interval,
                               xg_process_timeouts,
                               0);
    }
}

/* Stop the xg_timer if started.  */

static void
xg_stop_timer ()
{
  if (xg_timer)
    {
      cancel_atimer (xg_timer);
      xg_timer = 0;
    }
}

/* Insert NODE into linked LIST.  */

static void
xg_list_insert (xg_list_node *list, xg_list_node *node)
{
  xg_list_node *list_start = list->next;

  if (list_start) list_start->prev = node;
  node->next = list_start;
  node->prev = 0;
  list->next = node;
}

/* Remove NODE from linked LIST.  */

static void
xg_list_remove (xg_list_node *list, xg_list_node *node)
{
  xg_list_node *list_start = list->next;
  if (node == list_start)
    {
      list->next = node->next;
      if (list->next) list->next->prev = 0;
    }
  else
    {
      node->prev->next = node->next;
      if (node->next) node->next->prev = node->prev;
    }
}

/* Allocate and return a utf8 version of STR.  If STR is already
   utf8 or NULL, just return STR.
   If not, a new string is allocated and the caller must free the result
   with g_free.  */

static char *
get_utf8_string (str)
     char *str;
{
  char *utf8_str = str;

  if (!str) return NULL;

  /* If not UTF-8, try current locale.  */
  if (!g_utf8_validate (str, -1, NULL))
    utf8_str = g_locale_to_utf8 (str, -1, 0, 0, 0);

  if (!utf8_str) 
    {
      /* Probably some control characters in str.  Escape them. */
      size_t nr_bad = 0;
      gsize bytes_read;
      gsize bytes_written;
      unsigned char *p = (unsigned char *)str;
      char *cp, *up;
      GError *error = NULL;

      while (! (cp = g_locale_to_utf8 ((char *)p, -1, &bytes_read,
                                       &bytes_written, &error))
             && error->code == G_CONVERT_ERROR_ILLEGAL_SEQUENCE)
        {
          ++nr_bad;
          p += bytes_written+1;
          g_error_free (error);
          error = NULL;
        }

      if (error) 
        {
          g_error_free (error);
          error = NULL;
        }
      if (cp) g_free (cp);

      up = utf8_str = xmalloc (strlen (str) + nr_bad * 4 + 1);
      p = (unsigned char *)str;

      while (! (cp = g_locale_to_utf8 ((char *)p, -1, &bytes_read,
                                       &bytes_written, &error))
             && error->code == G_CONVERT_ERROR_ILLEGAL_SEQUENCE)
        {
          strncpy (up, (char *)p, bytes_written);
          sprintf (up + bytes_written, "\\%03o", p[bytes_written]);
          up[bytes_written+4] = '\0';
          up += bytes_written+4;
          p += bytes_written+1;
          g_error_free (error);
          error = NULL;
        }

      if (cp) 
        {
          strcat (utf8_str, cp);
          g_free (cp);
        }
      if (error) 
        {
          g_error_free (error);
          error = NULL;
        }
    }
  return utf8_str;
}



/***********************************************************************
    General functions for creating widgets, resizing, events, e.t.c.
 ***********************************************************************/

/* Make a geometry string and pass that to GTK.  It seems this is the
   only way to get geometry position right if the user explicitly
   asked for a position when starting Emacs.
   F is the frame we shall set geometry for.  */

static void
xg_set_geometry (f)
     FRAME_PTR f;
{
  if (f->size_hint_flags & USPosition)
  {
    int left = f->left_pos;
    int xneg = f->size_hint_flags & XNegative;
    int top = f->top_pos;
    int yneg = f->size_hint_flags & YNegative;
    char geom_str[32];

    if (xneg)
      left = -left;
    if (yneg)
      top = -top;

    sprintf (geom_str, "=%dx%d%c%d%c%d",
             FRAME_PIXEL_WIDTH (f),
             FRAME_TOTAL_PIXEL_HEIGHT (f),
             (xneg ? '-' : '+'), left,
             (yneg ? '-' : '+'), top);

    if (!gtk_window_parse_geometry (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
                                    geom_str))
      fprintf (stderr, "Failed to parse: '%s'\n", geom_str);
  } else if (f->size_hint_flags & PPosition) {
    gtk_window_move (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
                     f->left_pos, f->top_pos);
  }
}


/* Resize the outer window of frame F after chainging the height.
   This happend when the menu bar or the tool bar is added or removed.
   COLUMNS/ROWS is the size the edit area shall have after the resize.  */

static void
xg_resize_outer_widget (f, columns, rows)
     FRAME_PTR f;
     int columns;
     int rows;
{
  /* If we are not mapped yet, set geometry once again, as window
     height now have changed.  */
  if (! GTK_WIDGET_MAPPED (FRAME_GTK_OUTER_WIDGET (f)))
    xg_set_geometry (f);

  xg_frame_set_char_size (f, columns, rows);
  gdk_window_process_all_updates ();
}

/* Function to handle resize of our widgets.  Since Emacs has some layouts
   that does not fit well with GTK standard containers, we do most layout
   manually.
   F is the frame to resize.
   PIXELWIDTH, PIXELHEIGHT is the new size in pixels.  */

void
xg_resize_widgets (f, pixelwidth, pixelheight)
     FRAME_PTR f;
     int pixelwidth, pixelheight;
{
  int mbheight = FRAME_MENUBAR_HEIGHT (f);
  int tbheight = FRAME_TOOLBAR_HEIGHT (f);
  int rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, (pixelheight
						   - mbheight - tbheight));
  int columns = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, pixelwidth);

  if (FRAME_GTK_WIDGET (f)
      && (columns != FRAME_COLS (f)
	  || rows != FRAME_LINES (f)
          || pixelwidth != FRAME_PIXEL_WIDTH (f)
	  || pixelheight != FRAME_PIXEL_HEIGHT (f)))
    {
      struct x_output *x = f->output_data.x;
      GtkAllocation all;

      all.y = mbheight + tbheight;
      all.x = 0;

      all.width = pixelwidth;
      all.height = pixelheight - mbheight - tbheight;

      gtk_widget_size_allocate (x->edit_widget, &all);

      change_frame_size (f, rows, columns, 0, 1, 0);
      SET_FRAME_GARBAGED (f);
      cancel_mouse_face (f);
    }
}


/* Update our widget size to be COLS/ROWS characters for frame F.  */

void
xg_frame_set_char_size (f, cols, rows)
     FRAME_PTR f;
     int cols;
     int rows;
{
  int pixelheight = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, rows)
    + FRAME_MENUBAR_HEIGHT (f) + FRAME_TOOLBAR_HEIGHT (f);
  int pixelwidth;

  /* Take into account the size of the scroll bar.  Always use the
     number of columns occupied by the scroll bar here otherwise we
     might end up with a frame width that is not a multiple of the
     frame's character width which is bad for vertically split
     windows.  */
  f->scroll_bar_actual_width
    = FRAME_SCROLL_BAR_COLS (f) * FRAME_COLUMN_WIDTH (f);

  compute_fringe_widths (f, 0);

  /* FRAME_TEXT_COLS_TO_PIXEL_WIDTH uses scroll_bar_actual_width, so call it
     after calculating that value.  */
  pixelwidth = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, cols);

  /* Must resize our top level widget.  Font size may have changed,
     but not rows/cols.  */
  gtk_window_resize (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
                     pixelwidth, pixelheight);
  xg_resize_widgets (f, pixelwidth, pixelheight);
  x_wm_set_size_hint (f, 0, 0);
  SET_FRAME_GARBAGED (f);
  cancel_mouse_face (f);
}

/* Convert an X Window WSESC on display DPY to its corresponding GtkWidget.
   Must be done like this, because GtkWidget:s can have "hidden"
   X Window that aren't accessible.

   Return 0 if no widget match WDESC.  */

GtkWidget *
xg_win_to_widget (dpy, wdesc)
     Display *dpy;
     Window wdesc;
{
  gpointer gdkwin;
  GtkWidget *gwdesc = 0;

  BLOCK_INPUT;

  gdkwin = gdk_xid_table_lookup_for_display (gdk_x11_lookup_xdisplay (dpy),
                                             wdesc);
  if (gdkwin)
    {
      GdkEvent event;
      event.any.window = gdkwin;
      gwdesc = gtk_get_event_widget (&event);
    }

  UNBLOCK_INPUT;
  return gwdesc;
}

/* Fill in the GdkColor C so that it represents PIXEL.
   W is the widget that color will be used for.  Used to find colormap.  */

static void
xg_pix_to_gcolor (w, pixel, c)
     GtkWidget *w;
     unsigned long pixel;
     GdkColor *c;
{
  GdkColormap *map = gtk_widget_get_colormap (w);
  gdk_colormap_query_color (map, pixel, c);
}

/* Create and set up the GTK widgets for frame F.
   Return 0 if creation failed, non-zero otherwise.  */

int
xg_create_frame_widgets (f)
     FRAME_PTR f;
{
  GtkWidget *wtop;
  GtkWidget *wvbox;
  GtkWidget *wfixed;
  GdkColor bg;
  GtkRcStyle *style;
  int i;
  char *title = 0;

  BLOCK_INPUT;

  wtop = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  xg_set_screen (wtop, f);

  wvbox = gtk_vbox_new (FALSE, 0);
  wfixed = gtk_fixed_new ();  /* Must have this to place scroll bars  */

  if (! wtop || ! wvbox || ! wfixed)
    {
      if (wtop) gtk_widget_destroy (wtop);
      if (wvbox) gtk_widget_destroy (wvbox);
      if (wfixed) gtk_widget_destroy (wfixed);

      UNBLOCK_INPUT;
      return 0;
    }

  /* Use same names as the Xt port does.  I.e. Emacs.pane.emacs by default */
  gtk_widget_set_name (wtop, EMACS_CLASS);
  gtk_widget_set_name (wvbox, "pane");
  gtk_widget_set_name (wfixed, SSDATA (Vx_resource_name));

  /* If this frame has a title or name, set it in the title bar.  */
  if (! NILP (f->title)) title = SSDATA (ENCODE_UTF_8 (f->title));
  else if (! NILP (f->name)) title = SSDATA (ENCODE_UTF_8 (f->name));

  if (title) gtk_window_set_title (GTK_WINDOW (wtop), title);

  FRAME_GTK_OUTER_WIDGET (f) = wtop;
  FRAME_GTK_WIDGET (f) = wfixed;
  f->output_data.x->vbox_widget = wvbox;

  gtk_fixed_set_has_window (GTK_FIXED (wfixed), TRUE);

  gtk_widget_set_size_request (wfixed, FRAME_PIXEL_WIDTH (f),
                               FRAME_PIXEL_HEIGHT (f));

  gtk_container_add (GTK_CONTAINER (wtop), wvbox);
  gtk_box_pack_end (GTK_BOX (wvbox), wfixed, TRUE, TRUE, 0);

  if (FRAME_EXTERNAL_TOOL_BAR (f))
    update_frame_tool_bar (f);

  /* The tool bar is created but first there are no items in it.
     This causes it to be zero height.  Later items are added, but then
     the frame is already mapped, so there is a "jumping" resize.
     This makes geometry handling difficult, for example -0-0 will end
     up in the wrong place as tool bar height has not been taken into account.
     So we cheat a bit by setting a height that is what it will have
     later on when tool bar items are added.  */
  if (FRAME_EXTERNAL_TOOL_BAR (f) && f->n_tool_bar_items == 0)
    FRAME_TOOLBAR_HEIGHT (f) = 38;


  /* We don't want this widget double buffered, because we draw on it
     with regular X drawing primitives, so from a GTK/GDK point of
     view, the widget is totally blank.  When an expose comes, this
     will make the widget blank, and then Emacs redraws it.  This flickers
     a lot, so we turn off double buffering.  */
  gtk_widget_set_double_buffered (wfixed, FALSE);

  /* GTK documents says use gtk_window_set_resizable.  But then a user
     can't shrink the window from its starting size.  */
  gtk_window_set_policy (GTK_WINDOW (wtop), TRUE, TRUE, TRUE);
  gtk_window_set_wmclass (GTK_WINDOW (wtop),
                          SSDATA (Vx_resource_name),
                          SSDATA (Vx_resource_class));

  /* Add callback to do nothing on WM_DELETE_WINDOW.  The default in
     GTK is to destroy the widget.  We want Emacs to do that instead.  */
  g_signal_connect (G_OBJECT (wtop), "delete-event",
                    G_CALLBACK (gtk_true), 0);

  /* Convert our geometry parameters into a geometry string
     and specify it.
     GTK will itself handle calculating the real position this way.  */
  xg_set_geometry (f);

  gtk_widget_add_events (wfixed,
                         GDK_POINTER_MOTION_MASK
                         | GDK_EXPOSURE_MASK
                         | GDK_BUTTON_PRESS_MASK
                         | GDK_BUTTON_RELEASE_MASK
                         | GDK_KEY_PRESS_MASK
                         | GDK_ENTER_NOTIFY_MASK
                         | GDK_LEAVE_NOTIFY_MASK
                         | GDK_FOCUS_CHANGE_MASK
                         | GDK_STRUCTURE_MASK
                         | GDK_VISIBILITY_NOTIFY_MASK);

  /* Must realize the windows so the X window gets created.  It is used
     by callers of this function.  */
  gtk_widget_realize (wfixed);
  FRAME_X_WINDOW (f) = GTK_WIDGET_TO_X_WIN (wfixed);

  /* Since GTK clears its window by filling with the background color,
     we must keep X and GTK background in sync.  */
  xg_pix_to_gcolor (wfixed, f->output_data.x->background_pixel, &bg);
  gtk_widget_modify_bg (wfixed, GTK_STATE_NORMAL, &bg);

  /* Also, do not let any background pixmap to be set, this looks very
     bad as Emacs overwrites the background pixmap with its own idea
     of background color.  */
  style = gtk_widget_get_modifier_style (wfixed);

  /* Must use g_strdup because gtk_widget_modify_style does g_free.  */
  style->bg_pixmap_name[GTK_STATE_NORMAL] = g_strdup ("<none>");
  gtk_widget_modify_style (wfixed, style);

  /* GTK does not set any border, and they look bad with GTK.  */
  f->border_width = 0;
  f->internal_border_width = 0;

  UNBLOCK_INPUT;

  return 1;
}

/* Set the normal size hints for the window manager, for frame F.
   FLAGS is the flags word to use--or 0 meaning preserve the flags
   that the window now has.
   If USER_POSITION is nonzero, we set the User Position
   flag (this is useful when FLAGS is 0).  */

void
x_wm_set_size_hint (f, flags, user_position)
     FRAME_PTR f;
     long flags;
     int user_position;
{
  if (FRAME_GTK_OUTER_WIDGET (f))
  {
    /* Must use GTK routines here, otherwise GTK resets the size hints
       to its own defaults.  */
    GdkGeometry size_hints;
    gint hint_flags = 0;
    int base_width, base_height;
    int min_rows = 0, min_cols = 0;
    int win_gravity = f->win_gravity;

    if (flags)
      {
        memset (&size_hints, 0, sizeof (size_hints));
        f->output_data.x->size_hints = size_hints;
        f->output_data.x->hint_flags = hint_flags;
      }
     else
       flags = f->size_hint_flags;

    size_hints = f->output_data.x->size_hints;
    hint_flags = f->output_data.x->hint_flags;

    hint_flags |= GDK_HINT_RESIZE_INC | GDK_HINT_MIN_SIZE;
    size_hints.width_inc = FRAME_COLUMN_WIDTH (f);
    size_hints.height_inc = FRAME_LINE_HEIGHT (f);

    hint_flags |= GDK_HINT_BASE_SIZE;
    base_width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, 0);
    base_height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, 0)
      + FRAME_MENUBAR_HEIGHT (f) + FRAME_TOOLBAR_HEIGHT (f);

    check_frame_size (f, &min_rows, &min_cols);

    size_hints.base_width = base_width;
    size_hints.base_height = base_height;
    size_hints.min_width  = base_width + min_cols * size_hints.width_inc;
    size_hints.min_height = base_height + min_rows * size_hints.height_inc;


    /* These currently have a one to one mapping with the X values, but I
       don't think we should rely on that.  */
    hint_flags |= GDK_HINT_WIN_GRAVITY;
    size_hints.win_gravity = 0;
    if (win_gravity == NorthWestGravity)
      size_hints.win_gravity = GDK_GRAVITY_NORTH_WEST;
    else if (win_gravity == NorthGravity)
      size_hints.win_gravity = GDK_GRAVITY_NORTH;
    else if (win_gravity == NorthEastGravity)
      size_hints.win_gravity = GDK_GRAVITY_NORTH_EAST;
    else if (win_gravity == WestGravity)
      size_hints.win_gravity = GDK_GRAVITY_WEST;
    else if (win_gravity == CenterGravity)
      size_hints.win_gravity = GDK_GRAVITY_CENTER;
    else if (win_gravity == EastGravity)
      size_hints.win_gravity = GDK_GRAVITY_EAST;
    else if (win_gravity == SouthWestGravity)
      size_hints.win_gravity = GDK_GRAVITY_SOUTH_WEST;
    else if (win_gravity == SouthGravity)
      size_hints.win_gravity = GDK_GRAVITY_SOUTH;
    else if (win_gravity == SouthEastGravity)
      size_hints.win_gravity = GDK_GRAVITY_SOUTH_EAST;
    else if (win_gravity == StaticGravity)
      size_hints.win_gravity = GDK_GRAVITY_STATIC;

    if (flags & PPosition) hint_flags |= GDK_HINT_POS;
    if (flags & USPosition) hint_flags |= GDK_HINT_USER_POS;
    if (flags & USSize) hint_flags |= GDK_HINT_USER_SIZE;

    if (user_position)
      {
        hint_flags &= ~GDK_HINT_POS;
        hint_flags |= GDK_HINT_USER_POS;
      }

    BLOCK_INPUT;

    gtk_window_set_geometry_hints (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
                                   FRAME_GTK_OUTER_WIDGET (f),
                                   &size_hints,
                                   hint_flags);

    f->output_data.x->size_hints = size_hints;
    f->output_data.x->hint_flags = hint_flags;
    UNBLOCK_INPUT;
  }
}

/* Change background color of a frame.
   Since GTK uses the background colour to clear the window, we must
   keep the GTK and X colors in sync.
   F is the frame to change,
   BG is the pixel value to change to.  */

void
xg_set_background_color (f, bg)
     FRAME_PTR f;
     unsigned long bg;
{
  if (FRAME_GTK_WIDGET (f))
    {
      GdkColor gdk_bg;

      BLOCK_INPUT;
      xg_pix_to_gcolor (FRAME_GTK_WIDGET (f), bg, &gdk_bg);
      gtk_widget_modify_bg (FRAME_GTK_WIDGET (f), GTK_STATE_NORMAL, &gdk_bg);
      UNBLOCK_INPUT;
    }
}


/* Set the frame icon to ICON_PIXMAP/MASK.  This must be done with GTK
   functions so GTK does not overwrite the icon.  */

void
xg_set_frame_icon (f, icon_pixmap, icon_mask)
     FRAME_PTR f;
     Pixmap icon_pixmap;
     Pixmap icon_mask;
{
    GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (FRAME_X_DISPLAY (f));
    GdkPixmap *gpix = gdk_pixmap_foreign_new_for_display (gdpy, icon_pixmap);
    GdkPixmap *gmask = gdk_pixmap_foreign_new_for_display (gdpy, icon_mask);
    GdkPixbuf *gp = xg_get_pixbuf_from_pix_and_mask (gpix, gmask, NULL);

    gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), gp);
}



/***********************************************************************
                      Dialog functions
 ***********************************************************************/
/* Return the dialog title to use for a dialog of type KEY.
   This is the encoding used by lwlib.  We use the same for GTK.  */

static char *
get_dialog_title (char key)
{
  char *title = "";

  switch (key) {
  case 'E': case 'e':
    title = "Error";
    break;

  case 'I': case 'i':
    title = "Information";
    break;

  case 'L': case 'l':
    title = "Prompt";
    break;

  case 'P': case 'p':
    title = "Prompt";
    break;

  case 'Q': case 'q':
    title = "Question";
    break;
  }

  return title;
}

/* Callback for dialogs that get WM_DELETE_WINDOW.  We pop down
   the dialog, but return TRUE so the event does not propagate further
   in GTK.  This prevents GTK from destroying the dialog widget automatically
   and we can always destrou the widget manually, regardles of how
   it was popped down (button press or WM_DELETE_WINDOW).
   W is the dialog widget.
   EVENT is the GdkEvent that represents WM_DELETE_WINDOW (not used).
   user_data is NULL (not used).

   Returns TRUE to end propagation of event.  */

static gboolean
dialog_delete_callback (w, event, user_data)
     GtkWidget *w;
     GdkEvent *event;
     gpointer user_data;
{
  gtk_widget_unmap (w);
  return TRUE;
}

/* Create a popup dialog window.  See also xg_create_widget below.
   WV is a widget_value describing the dialog.
   SELECT_CB is the callback to use when a button has been pressed.
   DEACTIVATE_CB is the callback to use when the dialog pops down.

   Returns the GTK dialog widget.  */

static GtkWidget *
create_dialog (wv, select_cb, deactivate_cb)
     widget_value *wv;
     GCallback select_cb;
     GCallback deactivate_cb;
{
  char *title = get_dialog_title (wv->name[0]);
  int total_buttons = wv->name[1] - '0';
  int right_buttons = wv->name[4] - '0';
  int left_buttons;
  int button_nr = 0;
  int button_spacing = 10;
  GtkWidget *wdialog = gtk_dialog_new ();
  widget_value *item;
  GtkBox *cur_box;
  GtkWidget *wvbox;
  GtkWidget *whbox_up;
  GtkWidget *whbox_down;

  /* If the number of buttons is greater than 4, make two rows of buttons
     instead.  This looks better.  */
  int make_two_rows = total_buttons > 4;

  if (right_buttons == 0) right_buttons = total_buttons/2;
  left_buttons = total_buttons - right_buttons;

  gtk_window_set_title (GTK_WINDOW (wdialog), title);
  gtk_widget_set_name (wdialog, "emacs-dialog");

  cur_box = GTK_BOX (GTK_DIALOG (wdialog)->action_area);

  if (make_two_rows)
    {
      wvbox = gtk_vbox_new (TRUE, button_spacing);
      whbox_up = gtk_hbox_new (FALSE, 0);
      whbox_down = gtk_hbox_new (FALSE, 0);

      gtk_box_pack_start (cur_box, wvbox, FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (wvbox), whbox_up, FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (wvbox), whbox_down, FALSE, FALSE, 0);

      cur_box = GTK_BOX (whbox_up);
    }

  g_signal_connect (G_OBJECT (wdialog), "delete-event",
                    G_CALLBACK (dialog_delete_callback), 0);

  if (deactivate_cb)
    {
      g_signal_connect (G_OBJECT (wdialog), "close", deactivate_cb, 0);
      g_signal_connect (G_OBJECT (wdialog), "response", deactivate_cb, 0);
    }

  for (item = wv->contents; item; item = item->next)
    {
      char *utf8_label = get_utf8_string (item->value);
      GtkWidget *w;
      GtkRequisition req;

      if (item->name && strcmp (item->name, "message") == 0)
        {
          /* This is the text part of the dialog.  */
          w = gtk_label_new (utf8_label);
          gtk_box_pack_start (GTK_BOX (GTK_DIALOG (wdialog)->vbox),
                              gtk_label_new (""),
                              FALSE, FALSE, 0);
          gtk_box_pack_start (GTK_BOX (GTK_DIALOG (wdialog)->vbox), w,
                              TRUE, TRUE, 0);
          gtk_misc_set_alignment (GTK_MISC (w), 0.1, 0.5);

          /* Try to make dialog look better.  Must realize first so
             the widget can calculate the size it needs.  */
          gtk_widget_realize (w);
          gtk_widget_size_request (w, &req);
          gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (wdialog)->vbox),
                               req.height);
	  if (item->value && strlen (item->value) > 0)
            button_spacing = 2*req.width/strlen (item->value);
        }
      else
        {
          /* This is one button to add to the dialog.  */
          w = gtk_button_new_with_label (utf8_label);
          if (! item->enabled)
            gtk_widget_set_sensitive (w, FALSE);
          if (select_cb)
            g_signal_connect (G_OBJECT (w), "clicked",
                              select_cb, item->call_data);

          gtk_box_pack_start (cur_box, w, TRUE, TRUE, button_spacing);
          if (++button_nr == left_buttons)
            {
              if (make_two_rows)
                cur_box = GTK_BOX (whbox_down);
              else
                gtk_box_pack_start (cur_box,
                                    gtk_label_new (""),
                                    TRUE, TRUE,
                                    button_spacing);
            }
        }

     if (utf8_label && utf8_label != item->value)
       g_free (utf8_label);
    }

  return wdialog;
}



/***********************************************************************
                      File dialog functions
 ***********************************************************************/
/* Return non-zero if the old file selection dialog is being used.
   Return zero if not.  */

int
xg_uses_old_file_dialog ()
{
#ifdef HAVE_GTK_FILE_BOTH
  extern int x_gtk_use_old_file_dialog;
  return x_gtk_use_old_file_dialog;
#else /* ! HAVE_GTK_FILE_BOTH */

#ifdef HAVE_GTK_FILE_SELECTION_NEW
  return 1;
#else
  return 0;
#endif

#endif /* ! HAVE_GTK_FILE_BOTH */
}


/* Function that is called when the file dialog pops down.
   W is the dialog widget, RESPONSE is the response code.
   USER_DATA is what we passed in to g_signal_connect (pointer to int).  */

static void
xg_file_response_cb (w,
                     response,
                     user_data)
     GtkDialog *w;
     gint response;
     gpointer user_data;
{
  int *ptr = (int *) user_data;
  *ptr = response;
}


/*  Destroy the dialog.  This makes it pop down.  */

static Lisp_Object
pop_down_file_dialog (arg)
     Lisp_Object arg;
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (arg);
  BLOCK_INPUT;
  gtk_widget_destroy (GTK_WIDGET (p->pointer));
  UNBLOCK_INPUT;
  return Qnil;
}

typedef char * (*xg_get_file_func) P_ ((GtkWidget *));

#ifdef HAVE_GTK_FILE_CHOOSER_DIALOG_NEW

/* Return the selected file for file chooser dialog W.
   The returned string must be free:d.  */

static char *
xg_get_file_name_from_chooser (w)
     GtkWidget *w;
{
  return gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (w));
}

/* Callback called when the "Show hidden files" toggle is pressed.
   WIDGET is the toggle widget, DATA is the file chooser dialog.  */

static void
xg_toggle_visibility_cb (widget, data)
     GtkWidget *widget;
     gpointer data;
{
  GtkFileChooser *dialog = GTK_FILE_CHOOSER (data);
  gboolean visible;
  g_object_get (G_OBJECT (dialog), "show-hidden", &visible, NULL);
  g_object_set (G_OBJECT (dialog), "show-hidden", !visible, NULL);
}


/* Callback called when a property changes in a file chooser.
   GOBJECT is the file chooser dialog, ARG1 describes the property.
   USER_DATA is the toggle widget in the file chooser dialog.
   We use this to update the "Show hidden files" toggle when the user
   changes that property by right clicking in the file list.  */

static void
xg_toggle_notify_cb (gobject, arg1, user_data)
     GObject *gobject;
     GParamSpec *arg1;
     gpointer user_data;
{
  extern int x_gtk_show_hidden_files;

  if (strcmp (arg1->name, "show-hidden") == 0)
    {
      GtkFileChooser *dialog = GTK_FILE_CHOOSER (gobject);
      GtkWidget *wtoggle = GTK_WIDGET (user_data);
      gboolean visible, toggle_on;

      g_object_get (G_OBJECT (gobject), "show-hidden", &visible, NULL);
      toggle_on = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (wtoggle));

      if (!!visible != !!toggle_on)
        {
          g_signal_handlers_block_by_func (G_OBJECT (wtoggle),
                                           G_CALLBACK (xg_toggle_visibility_cb),
                                           gobject);
          gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (wtoggle), visible);
          g_signal_handlers_unblock_by_func
            (G_OBJECT (wtoggle),
             G_CALLBACK (xg_toggle_visibility_cb),
             gobject);
        }
      x_gtk_show_hidden_files = visible;
    }
}

/* Read a file name from the user using a file chooser dialog.
   F is the current frame.
   PROMPT is a prompt to show to the user.  May not be NULL.
   DEFAULT_FILENAME is a default selection to be displayed.  May be NULL.
   If MUSTMATCH_P is non-zero, the returned file name must be an existing
   file.  *FUNC is set to a function that can be used to retrieve the
   selected file name from the returned widget.

   Returns the created widget.  */

static GtkWidget *
xg_get_file_with_chooser (f, prompt, default_filename,
                          mustmatch_p, only_dir_p, func)
     FRAME_PTR f;
     char *prompt;
     char *default_filename;
     int mustmatch_p, only_dir_p;
     xg_get_file_func *func;
{
  char message[1024];

  GtkWidget *filewin, *wtoggle, *wbox, *wmessage;
  GtkWindow *gwin = GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f));
  GtkFileChooserAction action = (mustmatch_p ?
                                 GTK_FILE_CHOOSER_ACTION_OPEN :
                                 GTK_FILE_CHOOSER_ACTION_SAVE);
  extern int x_gtk_show_hidden_files;
  extern int x_gtk_file_dialog_help_text;


  if (only_dir_p)
    action = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER;

  filewin = gtk_file_chooser_dialog_new (prompt, gwin, action,
                                         GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                         (mustmatch_p || only_dir_p ?
                                          GTK_STOCK_OPEN : GTK_STOCK_OK),
                                         GTK_RESPONSE_OK,
                                         NULL);
  gtk_file_chooser_set_local_only (GTK_FILE_CHOOSER (filewin), TRUE);

  wbox = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (wbox);
  wtoggle = gtk_check_button_new_with_label ("Show hidden files.");
  
  if (x_gtk_show_hidden_files) 
    {
      g_object_set (G_OBJECT (filewin), "show-hidden", TRUE, NULL);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (wtoggle), TRUE);
    }
  gtk_widget_show (wtoggle);
  g_signal_connect (G_OBJECT (wtoggle), "clicked",
                    G_CALLBACK (xg_toggle_visibility_cb), filewin);
  g_signal_connect (G_OBJECT (filewin), "notify",
                    G_CALLBACK (xg_toggle_notify_cb), wtoggle);

  if (x_gtk_file_dialog_help_text)
    {
      message[0] = '\0';
      /* Gtk+ 2.10 has the file name text entry box integrated in the dialog.
         Show the C-l help text only for versions < 2.10.  */
      if (gtk_check_version (2, 10, 0) && action != GTK_FILE_CHOOSER_ACTION_SAVE)
        strcat (message, "\nType C-l to display a file name text entry box.\n");
      strcat (message, "\nIf you don't like this file selector, use the "
              "corresponding\nkey binding or customize "
              "use-file-dialog to turn it off.");
    
      wmessage = gtk_label_new (message);
      gtk_widget_show (wmessage);
    }

  gtk_box_pack_start (GTK_BOX (wbox), wtoggle, FALSE, FALSE, 0);
  if (x_gtk_file_dialog_help_text)
    gtk_box_pack_start (GTK_BOX (wbox), wmessage, FALSE, FALSE, 0);
  gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER (filewin), wbox);

  if (default_filename)
    {
      Lisp_Object file;
      struct gcpro gcpro1;
      GCPRO1 (file);
      char *utf8_filename;

      file = build_string (default_filename);

      /* File chooser does not understand ~/... in the file name.  It must be
         an absolute name starting with /.  */
      if (default_filename[0] != '/')
        file = Fexpand_file_name (file, Qnil);
      
      utf8_filename = SSDATA (ENCODE_UTF_8 (file));
      if (! NILP (Ffile_directory_p (file)))
        gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (filewin),
                                             utf8_filename);
      else
        {
          gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (filewin),
                                         utf8_filename);
          if (action == GTK_FILE_CHOOSER_ACTION_SAVE)
            {
              char *cp = strrchr (utf8_filename, '/');
              if (cp) ++cp;
              else cp = utf8_filename;
              gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (filewin), cp);
            }
        }

      UNGCPRO;
    }

  *func = xg_get_file_name_from_chooser;
  return filewin;
}
#endif /* HAVE_GTK_FILE_CHOOSER_DIALOG_NEW */

#ifdef HAVE_GTK_FILE_SELECTION_NEW

/* Return the selected file for file selector dialog W.
   The returned string must be free:d.  */

static char *
xg_get_file_name_from_selector (w)
     GtkWidget *w;
{
  GtkFileSelection *filesel = GTK_FILE_SELECTION (w);
  return xstrdup ((char*) gtk_file_selection_get_filename (filesel));
}

/* Create a file selection dialog.
   F is the current frame.
   PROMPT is a prompt to show to the user.  May not be NULL.
   DEFAULT_FILENAME is a default selection to be displayed.  May be NULL.
   If MUSTMATCH_P is non-zero, the returned file name must be an existing
   file.  *FUNC is set to a function that can be used to retrieve the
   selected file name from the returned widget.

   Returns the created widget.  */

static GtkWidget *
xg_get_file_with_selection (f, prompt, default_filename,
                            mustmatch_p, only_dir_p, func)
     FRAME_PTR f;
     char *prompt;
     char *default_filename;
     int mustmatch_p, only_dir_p;
     xg_get_file_func *func;
{
  GtkWidget *filewin;
  GtkFileSelection *filesel;

  filewin = gtk_file_selection_new (prompt);
  filesel = GTK_FILE_SELECTION (filewin);

  if (default_filename)
    gtk_file_selection_set_filename (filesel, default_filename);

  if (mustmatch_p)
    {
      /* The selection_entry part of filesel is not documented.  */
      gtk_widget_set_sensitive (filesel->selection_entry, FALSE);
      gtk_file_selection_hide_fileop_buttons (filesel);
    }

  *func = xg_get_file_name_from_selector;

  return filewin;
}
#endif /* HAVE_GTK_FILE_SELECTION_NEW */

/* Read a file name from the user using a file dialog, either the old
   file selection dialog, or the new file chooser dialog.  Which to use
   depends on what the GTK version used has, and what the value of
   gtk-use-old-file-dialog.
   F is the current frame.
   PROMPT is a prompt to show to the user.  May not be NULL.
   DEFAULT_FILENAME is a default selection to be displayed.  May be NULL.
   If MUSTMATCH_P is non-zero, the returned file name must be an existing
   file.

   Returns a file name or NULL if no file was selected.
   The returned string must be freed by the caller.  */

char *
xg_get_file_name (f, prompt, default_filename, mustmatch_p, only_dir_p)
     FRAME_PTR f;
     char *prompt;
     char *default_filename;
     int mustmatch_p, only_dir_p;
{
  GtkWidget *w = 0;
  int count = SPECPDL_INDEX ();
  char *fn = 0;
  int filesel_done = 0;
  xg_get_file_func func;

#if defined (HAVE_GTK_AND_PTHREAD) && defined (__SIGRTMIN)
  /* I really don't know why this is needed, but without this the GLIBC add on
     library linuxthreads hangs when the Gnome file chooser backend creates
     threads.  */
  sigblock (sigmask (__SIGRTMIN));
#endif /* HAVE_GTK_AND_PTHREAD */

#ifdef HAVE_GTK_FILE_BOTH

  if (xg_uses_old_file_dialog ())
    w = xg_get_file_with_selection (f, prompt, default_filename,
                                    mustmatch_p, only_dir_p, &func);
  else
    w = xg_get_file_with_chooser (f, prompt, default_filename,
                                  mustmatch_p, only_dir_p, &func);

#else /* not HAVE_GTK_FILE_BOTH */

#ifdef HAVE_GTK_FILE_SELECTION_NEW
  w = xg_get_file_with_selection (f, prompt, default_filename,
                                  mustmatch_p, only_dir_p, &func);
#endif
#ifdef HAVE_GTK_FILE_CHOOSER_DIALOG_NEW
  w = xg_get_file_with_chooser (f, prompt, default_filename,
                                mustmatch_p, only_dir_p, &func);
#endif

#endif /* HAVE_GTK_FILE_BOTH */

  xg_set_screen (w, f);
  gtk_widget_set_name (w, "emacs-filedialog");
  gtk_window_set_transient_for (GTK_WINDOW (w),
                                GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
  gtk_window_set_destroy_with_parent (GTK_WINDOW (w), TRUE);
  gtk_window_set_modal (GTK_WINDOW (w), TRUE);

  g_signal_connect (G_OBJECT (w),
                    "response",
                    G_CALLBACK (xg_file_response_cb),
                    &filesel_done);

  /* Don't destroy the widget if closed by the window manager close button.  */
  g_signal_connect (G_OBJECT (w), "delete-event", G_CALLBACK (gtk_true), NULL);

  gtk_widget_show (w);

  record_unwind_protect (pop_down_file_dialog, make_save_value (w, 0));
  while (! filesel_done)
    {
      x_menu_wait_for_event (0);
      gtk_main_iteration ();
    }

#if defined (HAVE_GTK_AND_PTHREAD) && defined (__SIGRTMIN)
  sigunblock (sigmask (__SIGRTMIN));
#endif

  if (filesel_done == GTK_RESPONSE_OK)
    fn = (*func) (w);

  unbind_to (count, Qnil);

  return fn;
}


/***********************************************************************
	                Menu functions.
 ***********************************************************************/

/* The name of menu items that can be used for citomization.  Since GTK
   RC files are very crude and primitive, we have to set this on all
   menu item names so a user can easily cutomize menu items.  */

#define MENU_ITEM_NAME "emacs-menuitem"


/* Linked list of all allocated struct xg_menu_cb_data.  Used for marking
   during GC.  The next member points to the items.  */
static xg_list_node xg_menu_cb_list;

/* Linked list of all allocated struct xg_menu_item_cb_data.  Used for marking
   during GC.  The next member points to the items.  */
static xg_list_node xg_menu_item_cb_list;

/* Allocate and initialize CL_DATA if NULL, otherwise increase ref_count.
   F is the frame CL_DATA will be initialized for.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.

   The menu bar and all sub menus under the menu bar in a frame
   share the same structure, hence the reference count.

   Returns CL_DATA if CL_DATA is not NULL,  or a pointer to a newly
   allocated xg_menu_cb_data if CL_DATA is NULL.  */

static xg_menu_cb_data *
make_cl_data (cl_data, f, highlight_cb)
     xg_menu_cb_data *cl_data;
     FRAME_PTR f;
     GCallback highlight_cb;
{
  if (! cl_data)
    {
      cl_data = (xg_menu_cb_data*) xmalloc (sizeof (*cl_data));
      cl_data->f = f;
      cl_data->menu_bar_vector = f->menu_bar_vector;
      cl_data->menu_bar_items_used = f->menu_bar_items_used;
      cl_data->highlight_cb = highlight_cb;
      cl_data->ref_count = 0;

      xg_list_insert (&xg_menu_cb_list, &cl_data->ptrs);
    }

  cl_data->ref_count++;

  return cl_data;
}

/* Update CL_DATA with values from frame F and with HIGHLIGHT_CB.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.

   When the menu bar is updated, menu items may have been added and/or
   removed, so menu_bar_vector and menu_bar_items_used change.  We must
   then update CL_DATA since it is used to determine which menu
   item that is invoked in the menu.
   HIGHLIGHT_CB could change, there is no check that the same
   function is given when modifying a menu bar as was given when
   creating the menu bar.  */

static void
update_cl_data (cl_data, f, highlight_cb)
     xg_menu_cb_data *cl_data;
     FRAME_PTR f;
     GCallback highlight_cb;
{
  if (cl_data)
    {
      cl_data->f = f;
      cl_data->menu_bar_vector = f->menu_bar_vector;
      cl_data->menu_bar_items_used = f->menu_bar_items_used;
      cl_data->highlight_cb = highlight_cb;
    }
}

/* Decrease reference count for CL_DATA.
   If reference count is zero, free CL_DATA.  */

static void
unref_cl_data (cl_data)
     xg_menu_cb_data *cl_data;
{
  if (cl_data && cl_data->ref_count > 0)
    {
      cl_data->ref_count--;
      if (cl_data->ref_count == 0)
        {
          xg_list_remove (&xg_menu_cb_list, &cl_data->ptrs);
          xfree (cl_data);
        }
    }
}

/* Function that marks all lisp data during GC.  */

void
xg_mark_data ()
{
  xg_list_node *iter;

  for (iter = xg_menu_cb_list.next; iter; iter = iter->next)
    mark_object (((xg_menu_cb_data *) iter)->menu_bar_vector);

  for (iter = xg_menu_item_cb_list.next; iter; iter = iter->next)
    {
      xg_menu_item_cb_data *cb_data = (xg_menu_item_cb_data *) iter;

      if (! NILP (cb_data->help))
        mark_object (cb_data->help);
    }
}


/* Callback called when a menu item is destroyed.  Used to free data.
   W is the widget that is being destroyed (not used).
   CLIENT_DATA points to the xg_menu_item_cb_data associated with the W.  */

static void
menuitem_destroy_callback (w, client_data)
     GtkWidget *w;
     gpointer client_data;
{
  if (client_data)
    {
      xg_menu_item_cb_data *data = (xg_menu_item_cb_data*) client_data;
      xg_list_remove (&xg_menu_item_cb_list, &data->ptrs);
      xfree (data);
    }
}

/* Callback called when the pointer enters/leaves a menu item.
   W is the parent of the menu item.
   EVENT is either an enter event or leave event.
   CLIENT_DATA is not used.

   Returns FALSE to tell GTK to keep processing this event.  */

static gboolean
menuitem_highlight_callback (w, event, client_data)
     GtkWidget *w;
     GdkEventCrossing *event;
     gpointer client_data;
{
  GdkEvent ev;
  GtkWidget *subwidget;
  xg_menu_item_cb_data *data;

  ev.crossing = *event;
  subwidget = gtk_get_event_widget (&ev);
  data = (xg_menu_item_cb_data *) g_object_get_data (G_OBJECT (subwidget),
                                                     XG_ITEM_DATA);
  if (data)
    {
      if (! NILP (data->help) && data->cl_data->highlight_cb)
        {
          gpointer call_data = event->type == GDK_LEAVE_NOTIFY ? 0 : data;
          GtkCallback func = (GtkCallback) data->cl_data->highlight_cb;
          (*func) (subwidget, call_data);
        }
    }

  return FALSE;
}

/* Callback called when a menu is destroyed.  Used to free data.
   W is the widget that is being destroyed (not used).
   CLIENT_DATA points to the xg_menu_cb_data associated with W.  */

static void
menu_destroy_callback (w, client_data)
     GtkWidget *w;
     gpointer client_data;
{
  unref_cl_data ((xg_menu_cb_data*) client_data);
}

/* Callback called when a menu does a grab or ungrab.  That means the
   menu has been activated or deactivated.
   Used to start a timer so the small timeout the menus in GTK uses before
   popping down a menu is seen by Emacs (see xg_process_timeouts above).
   W is the widget that does the grab (not used).
   UNGRAB_P is TRUE if this is an ungrab, FALSE if it is a grab.
   CLIENT_DATA is NULL (not used).  */

static void
menu_grab_callback (GtkWidget *widget,
                    gboolean ungrab_p,
                    gpointer client_data)
{
  /* Keep track of total number of grabs.  */
  static int cnt;

  if (ungrab_p) cnt--;
  else cnt++;

  if (cnt > 0 && ! xg_timer) xg_start_timer ();
  else if (cnt == 0 && xg_timer) xg_stop_timer ();
}

/* Make a GTK widget that contains both UTF8_LABEL and UTF8_KEY (both
   must be non-NULL) and can be inserted into a menu item.

   Returns the GtkHBox.  */

static GtkWidget *
make_widget_for_menu_item (utf8_label, utf8_key)
     char *utf8_label;
     char *utf8_key;
{
  GtkWidget *wlbl;
  GtkWidget *wkey;
  GtkWidget *wbox;

  wbox = gtk_hbox_new (FALSE, 0);
  wlbl = gtk_label_new (utf8_label);
  wkey = gtk_label_new (utf8_key);

  gtk_misc_set_alignment (GTK_MISC (wlbl), 0.0, 0.5);
  gtk_misc_set_alignment (GTK_MISC (wkey), 0.0, 0.5);

  gtk_box_pack_start (GTK_BOX (wbox), wlbl, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (wbox), wkey, FALSE, FALSE, 0);

  gtk_widget_set_name (wlbl, MENU_ITEM_NAME);
  gtk_widget_set_name (wkey, MENU_ITEM_NAME);
  gtk_widget_set_name (wbox, MENU_ITEM_NAME);

  return wbox;
}

/* Make and return a menu item widget with the key to the right.
   UTF8_LABEL is the text for the menu item (GTK uses UTF8 internally).
   UTF8_KEY is the text representing the key binding.
   ITEM is the widget_value describing the menu item.

   GROUP is an in/out parameter.  If the menu item to be created is not
   part of any radio menu group, *GROUP contains NULL on entry and exit.
   If the menu item to be created is part of a radio menu group, on entry
   *GROUP contains the group to use, or NULL if this is the first item
   in the group.  On exit, *GROUP contains the radio item group.

   Unfortunately, keys don't line up as nicely as in Motif,
   but the MacOS X version doesn't either, so I guess that is OK.  */

static GtkWidget *
make_menu_item (utf8_label, utf8_key, item, group)
     char *utf8_label;
     char *utf8_key;
     widget_value *item;
     GSList **group;
{
  GtkWidget *w;
  GtkWidget *wtoadd = 0;

  /* It has been observed that some menu items have a NULL name field.
     This will lead to this function being called with a NULL utf8_label.
     GTK crashes on that so we set a blank label.  Why there is a NULL
     name remains to be investigated.  */
  if (! utf8_label) utf8_label = " ";

  if (utf8_key)
    wtoadd = make_widget_for_menu_item (utf8_label, utf8_key);

  if (item->button_type == BUTTON_TYPE_TOGGLE)
    {
      *group = NULL;
      if (utf8_key) w = gtk_check_menu_item_new ();
      else w = gtk_check_menu_item_new_with_label (utf8_label);
      gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (w), item->selected);
    }
  else if (item->button_type == BUTTON_TYPE_RADIO)
    {
      if (utf8_key) w = gtk_radio_menu_item_new (*group);
      else w = gtk_radio_menu_item_new_with_label (*group, utf8_label);
      *group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (w));
      if (item->selected)
        gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (w), TRUE);
    }
  else
    {
      *group = NULL;
      if (utf8_key) w = gtk_menu_item_new ();
      else w = gtk_menu_item_new_with_label (utf8_label);
    }

  if (wtoadd) gtk_container_add (GTK_CONTAINER (w), wtoadd);
  if (! item->enabled) gtk_widget_set_sensitive (w, FALSE);

  return w;
}

/* Return non-zero if LABEL specifies a separator (GTK only has one
   separator type)  */

static int
xg_separator_p (char *label)
{
  if (! label) return 0;
  else if (strlen (label) > 3
	   && strncmp (label, "--", 2) == 0
	   && label[2] != '-')
    {
      static char* separator_names[] = {
        "space",
	"no-line",
	"single-line",
	"double-line",
	"single-dashed-line",
	"double-dashed-line",
	"shadow-etched-in",
	"shadow-etched-out",
	"shadow-etched-in-dash",
	"shadow-etched-out-dash",
	"shadow-double-etched-in",
	"shadow-double-etched-out",
	"shadow-double-etched-in-dash",
	"shadow-double-etched-out-dash",
        0,
      };

      int i;

      label += 2;
      for (i = 0; separator_names[i]; ++i)
	if (strcmp (label, separator_names[i]) == 0)
          return 1;
    }
  else
    {
      /* Old-style separator, maybe.  It's a separator if it contains
	 only dashes.  */
      while (*label == '-')
	++label;
      if (*label == 0) return 1;
    }

  return 0;
}

static int xg_detached_menus;

/* Returns non-zero if there are detached menus.  */

int
xg_have_tear_offs ()
{
  return xg_detached_menus > 0;
}

/* Callback invoked when a detached menu window is removed.  Here we
   decrease the xg_detached_menus count.
   WIDGET is the top level window that is removed (the parent of the menu).
   CLIENT_DATA is not used.  */

static void
tearoff_remove (widget, client_data)
     GtkWidget *widget;
     gpointer client_data;
{
  if (xg_detached_menus > 0) --xg_detached_menus;
}

/* Callback invoked when a menu is detached.  It increases the
   xg_detached_menus count.
   WIDGET is the GtkTearoffMenuItem.
   CLIENT_DATA is not used.  */

static void
tearoff_activate (widget, client_data)
     GtkWidget *widget;
     gpointer client_data;
{
  GtkWidget *menu = gtk_widget_get_parent (widget);
  if (gtk_menu_get_tearoff_state (GTK_MENU (menu)))
    {
      ++xg_detached_menus;
      g_signal_connect (G_OBJECT (gtk_widget_get_toplevel (widget)),
                        "destroy",
                        G_CALLBACK (tearoff_remove), 0);
    }
}


/* Create a menu item widget, and connect the callbacks.
   ITEM decribes the menu item.
   F is the frame the created menu belongs to.
   SELECT_CB is the callback to use when a menu item is selected.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.
   CL_DATA points to the callback data to be used for this menu.
   GROUP is an in/out parameter.  If the menu item to be created is not
   part of any radio menu group, *GROUP contains NULL on entry and exit.
   If the menu item to be created is part of a radio menu group, on entry
   *GROUP contains the group to use, or NULL if this is the first item
   in the group.  On exit, *GROUP contains the radio item group.

   Returns the created GtkWidget.  */

static GtkWidget *
xg_create_one_menuitem (item, f, select_cb, highlight_cb, cl_data, group)
     widget_value *item;
     FRAME_PTR f;
     GCallback select_cb;
     GCallback highlight_cb;
     xg_menu_cb_data *cl_data;
     GSList **group;
{
  char *utf8_label;
  char *utf8_key;
  GtkWidget *w;
  xg_menu_item_cb_data *cb_data;

  utf8_label = get_utf8_string (item->name);
  utf8_key = get_utf8_string (item->key);

  w = make_menu_item (utf8_label, utf8_key, item, group);

  if (utf8_label && utf8_label != item->name) g_free (utf8_label);
  if (utf8_key && utf8_key != item->key) g_free (utf8_key);

  cb_data = xmalloc (sizeof (xg_menu_item_cb_data));

  xg_list_insert (&xg_menu_item_cb_list, &cb_data->ptrs);

  cb_data->select_id = 0;
  cb_data->help = item->help;
  cb_data->cl_data = cl_data;
  cb_data->call_data = item->call_data;

  g_signal_connect (G_OBJECT (w),
                    "destroy",
                    G_CALLBACK (menuitem_destroy_callback),
                    cb_data);

  /* Put cb_data in widget, so we can get at it when modifying menubar  */
  g_object_set_data (G_OBJECT (w), XG_ITEM_DATA, cb_data);

  /* final item, not a submenu  */
  if (item->call_data && ! item->contents)
    {
      if (select_cb)
        cb_data->select_id
          = g_signal_connect (G_OBJECT (w), "activate", select_cb, cb_data);
    }

  return w;
}

/* Callback called when keyboard traversal (started by menu-bar-open) ends.
   WMENU is the menu for which traversal has been done.  DATA points to the
   frame for WMENU.  We must release grabs, some bad interaction between GTK
   and Emacs makes the menus keep the grabs.  */

static void
menu_nav_ended (wmenu, data)
     GtkMenuShell *wmenu;
     gpointer data;
{
  FRAME_PTR f = (FRAME_PTR) data;

  if (FRAME_X_OUTPUT (f)->menubar_widget)
    {
      GtkMenuShell *w = GTK_MENU_SHELL (FRAME_X_OUTPUT (f)->menubar_widget);
      Display *dpy = FRAME_X_DISPLAY (f);

      BLOCK_INPUT;
      gtk_menu_shell_deactivate (w);
      gtk_menu_shell_deselect (w);

      XUngrabKeyboard (dpy, CurrentTime);
      XUngrabPointer (dpy, CurrentTime);
      UNBLOCK_INPUT;
    }
}


static GtkWidget *create_menus P_ ((widget_value *, FRAME_PTR, GCallback,
				    GCallback, GCallback, int, int, int,
				    GtkWidget *, xg_menu_cb_data *, char *));

/* Create a full menu tree specified by DATA.
   F is the frame the created menu belongs to.
   SELECT_CB is the callback to use when a menu item is selected.
   DEACTIVATE_CB is the callback to use when a sub menu is not shown anymore.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.
   POP_UP_P is non-zero if we shall create a popup menu.
   MENU_BAR_P is non-zero if we shall create a menu bar.
   ADD_TEAROFF_P is non-zero if we shall add a teroff menu item.  Ignored
   if MENU_BAR_P is non-zero.
   TOPMENU is the topmost GtkWidget that others shall be placed under.
   It may be NULL, in that case we create the appropriate widget
   (menu bar or menu item depending on POP_UP_P and MENU_BAR_P)
   CL_DATA is the callback data we shall use for this menu, or NULL
   if we haven't set the first callback yet.
   NAME is the name to give to the top level menu if this function
   creates it.  May be NULL to not set any name.

   Returns the top level GtkWidget.  This is TOPLEVEL if TOPLEVEL is
   not NULL.

   This function calls itself to create submenus.  */

static GtkWidget *
create_menus (data, f, select_cb, deactivate_cb, highlight_cb,
              pop_up_p, menu_bar_p, add_tearoff_p, topmenu, cl_data, name)
     widget_value *data;
     FRAME_PTR f;
     GCallback select_cb;
     GCallback deactivate_cb;
     GCallback highlight_cb;
     int pop_up_p;
     int menu_bar_p;
     int add_tearoff_p;
     GtkWidget *topmenu;
     xg_menu_cb_data *cl_data;
     char *name;
{
  widget_value *item;
  GtkWidget *wmenu = topmenu;
  GSList *group = NULL;

  if (! topmenu)
    {
      if (! menu_bar_p)
      {
        wmenu = gtk_menu_new ();
        xg_set_screen (wmenu, f);
        /* Connect this to the menu instead of items so we get enter/leave for
           disabled items also.  TODO:  Still does not get enter/leave for
           disabled items in detached menus.  */
        g_signal_connect (G_OBJECT (wmenu),
                          "enter-notify-event",
                          G_CALLBACK (menuitem_highlight_callback),
                          NULL);
        g_signal_connect (G_OBJECT (wmenu),
                          "leave-notify-event",
                          G_CALLBACK (menuitem_highlight_callback),
                          NULL);
      }
      else wmenu = gtk_menu_bar_new ();

      /* Fix up grabs after keyboard traversal ends.  */
      g_signal_connect (G_OBJECT (wmenu),
                        "selection-done",
                        G_CALLBACK (menu_nav_ended),
                        f);

      /* Put cl_data on the top menu for easier access.  */
      cl_data = make_cl_data (cl_data, f, highlight_cb);
      g_object_set_data (G_OBJECT (wmenu), XG_FRAME_DATA, (gpointer)cl_data);
      g_signal_connect (G_OBJECT (wmenu), "destroy",
                        G_CALLBACK (menu_destroy_callback), cl_data);

      if (name)
        gtk_widget_set_name (wmenu, name);

      if (deactivate_cb)
        g_signal_connect (G_OBJECT (wmenu),
                          "selection-done", deactivate_cb, 0);

      g_signal_connect (G_OBJECT (wmenu),
                        "grab-notify", G_CALLBACK (menu_grab_callback), 0);
    }

  if (! menu_bar_p && add_tearoff_p)
    {
      GtkWidget *tearoff = gtk_tearoff_menu_item_new ();
      gtk_menu_shell_append (GTK_MENU_SHELL (wmenu), tearoff);

      g_signal_connect (G_OBJECT (tearoff), "activate",
                        G_CALLBACK (tearoff_activate), 0);
    }

  for (item = data; item; item = item->next)
    {
      GtkWidget *w;

      if (pop_up_p && !item->contents && !item->call_data
          && !xg_separator_p (item->name))
        {
          char *utf8_label;
          /* A title for a popup.  We do the same as GTK does when
             creating titles, but it does not look good.  */
          group = NULL;
          utf8_label = get_utf8_string (item->name);

          gtk_menu_set_title (GTK_MENU (wmenu), utf8_label);
          w = gtk_menu_item_new_with_label (utf8_label);
          gtk_widget_set_sensitive (w, FALSE);
          if (utf8_label && utf8_label != item->name) g_free (utf8_label);
        }
      else if (xg_separator_p (item->name))
        {
          group = NULL;
          /* GTK only have one separator type.  */
          w = gtk_separator_menu_item_new ();
        }
      else
        {
          w = xg_create_one_menuitem (item,
                                      f,
                                      item->contents ? 0 : select_cb,
                                      highlight_cb,
                                      cl_data,
                                      &group);

          /* Create a possibly empty submenu for menu bar items, since some
             themes don't highlight items correctly without it. */
          if (item->contents || menu_bar_p)
            {
              GtkWidget *submenu = create_menus (item->contents,
                                                 f,
                                                 select_cb,
                                                 deactivate_cb,
                                                 highlight_cb,
                                                 0,
                                                 0,
                                                 add_tearoff_p,
                                                 0,
                                                 cl_data,
                                                 0);
              gtk_menu_item_set_submenu (GTK_MENU_ITEM (w), submenu);
            }
        }

      gtk_menu_shell_append (GTK_MENU_SHELL (wmenu), w);
      gtk_widget_set_name (w, MENU_ITEM_NAME);
    }

  return wmenu;
}

/* Create a menubar, popup menu or dialog, depending on the TYPE argument.
   TYPE can be "menubar", "popup" for popup menu, or "dialog" for a dialog
   with some text and buttons.
   F is the frame the created item belongs to.
   NAME is the name to use for the top widget.
   VAL is a widget_value structure describing items to be created.
   SELECT_CB is the callback to use when a menu item is selected or
   a dialog button is pressed.
   DEACTIVATE_CB is the callback to use when an item is deactivated.
   For a menu, when a sub menu is not shown anymore, for a dialog it is
   called when the dialog is popped down.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.

   Returns the widget created.  */

GtkWidget *
xg_create_widget (type, name, f, val,
                  select_cb, deactivate_cb, highlight_cb)
     char *type;
     char *name;
     FRAME_PTR f;
     widget_value *val;
     GCallback select_cb;
     GCallback deactivate_cb;
     GCallback highlight_cb;
{
  GtkWidget *w = 0;
  int menu_bar_p = strcmp (type, "menubar") == 0;
  int pop_up_p = strcmp (type, "popup") == 0;

  if (strcmp (type, "dialog") == 0)
    {
      w = create_dialog (val, select_cb, deactivate_cb);
      xg_set_screen (w, f);
      gtk_window_set_transient_for (GTK_WINDOW (w),
                                    GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
      gtk_window_set_destroy_with_parent (GTK_WINDOW (w), TRUE);
      gtk_widget_set_name (w, "emacs-dialog");
      gtk_window_set_modal (GTK_WINDOW (w), TRUE);
    }
  else if (menu_bar_p || pop_up_p)
    {
      w = create_menus (val->contents,
                        f,
                        select_cb,
                        deactivate_cb,
                        highlight_cb,
                        pop_up_p,
                        menu_bar_p,
                        menu_bar_p,
                        0,
                        0,
                        name);

      /* Set the cursor to an arrow for popup menus when they are mapped.
         This is done by default for menu bar menus.  */
      if (pop_up_p)
        {
          /* Must realize so the GdkWindow inside the widget is created.  */
          gtk_widget_realize (w);
          xg_set_cursor (w, FRAME_X_DISPLAY_INFO (f)->xg_cursor);
        }
    }
  else
    {
      fprintf (stderr, "bad type in xg_create_widget: %s, doing nothing\n",
               type);
    }

  return w;
}

/* Return the label for menu item WITEM.  */

static const char *
xg_get_menu_item_label (witem)
     GtkMenuItem *witem;
{
  GtkLabel *wlabel = GTK_LABEL (gtk_bin_get_child (GTK_BIN (witem)));
  return gtk_label_get_label (wlabel);
}

/* Return non-zero if the menu item WITEM has the text LABEL.  */

static int
xg_item_label_same_p (witem, label)
     GtkMenuItem *witem;
     char *label;
{
  int is_same = 0;
  char *utf8_label = get_utf8_string (label);
  const char *old_label = witem ? xg_get_menu_item_label (witem) : 0;

  if (! old_label && ! utf8_label)
    is_same = 1;
  else if (old_label && utf8_label)
    is_same = strcmp (utf8_label, old_label) == 0;

  if (utf8_label && utf8_label != label) g_free (utf8_label);

  return is_same;
}

/* Destroy widgets in LIST.  */

static void
xg_destroy_widgets (list)
     GList *list;
{
  GList *iter;

  for (iter = list; iter; iter = g_list_next (iter))
    {
      GtkWidget *w = GTK_WIDGET (iter->data);

      /* Destroying the widget will remove it from the container it is in.  */
      gtk_widget_destroy (w);
    }
}

/* Update the top level names in MENUBAR (i.e. not submenus).
   F is the frame the menu bar belongs to.
   *LIST is a list with the current menu bar names (menu item widgets).
   ITER is the item within *LIST that shall be updated.
   POS is the numerical position, starting at 0, of ITER in *LIST.
   VAL describes what the menu bar shall look like after the update.
   SELECT_CB is the callback to use when a menu item is selected.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.
   CL_DATA points to the callback data to be used for this menu bar.

   This function calls itself to walk through the menu bar names.  */

static void
xg_update_menubar (menubar, f, list, iter, pos, val,
                   select_cb, highlight_cb, cl_data)
     GtkWidget *menubar;
     FRAME_PTR f;
     GList **list;
     GList *iter;
     int pos;
     widget_value *val;
     GCallback select_cb;
     GCallback highlight_cb;
     xg_menu_cb_data *cl_data;
{
  if (! iter && ! val)
    return;
  else if (iter && ! val)
    {
      /* Item(s) have been removed.  Remove all remaining items.  */
      xg_destroy_widgets (iter);

      /* All updated.  */
      val = 0;
      iter = 0;
    }
  else if (! iter && val)
    {
      /* Item(s) added.  Add all new items in one call.  */
      create_menus (val, f, select_cb, 0, highlight_cb,
                    0, 1, 0, menubar, cl_data, 0);

      /* All updated.  */
      val = 0;
      iter = 0;
    }
  /* Below this neither iter or val is NULL */
  else if (xg_item_label_same_p (GTK_MENU_ITEM (iter->data), val->name))
    {
      /* This item is still the same, check next item.  */
      val = val->next;
      iter = g_list_next (iter);
      ++pos;
    }
  else /* This item is changed.  */
    {
      GtkMenuItem *witem = GTK_MENU_ITEM (iter->data);
      GtkMenuItem *witem2 = 0;
      int val_in_menubar = 0;
      int iter_in_new_menubar = 0;
      GList *iter2;
      widget_value *cur;

      /* See if the changed entry (val) is present later in the menu bar  */
      for (iter2 = iter;
           iter2 && ! val_in_menubar;
           iter2 = g_list_next (iter2))
        {
          witem2 = GTK_MENU_ITEM (iter2->data);
          val_in_menubar = xg_item_label_same_p (witem2, val->name);
        }

      /* See if the current entry (iter) is present later in the
         specification for the new menu bar.  */
      for (cur = val; cur && ! iter_in_new_menubar; cur = cur->next)
        iter_in_new_menubar = xg_item_label_same_p (witem, cur->name);

      if (val_in_menubar && ! iter_in_new_menubar)
        {
          int nr = pos;

          /*  This corresponds to:
                Current:  A B C
                New:      A C
              Remove B.  */

          gtk_widget_ref (GTK_WIDGET (witem));
          gtk_container_remove (GTK_CONTAINER (menubar), GTK_WIDGET (witem));
          gtk_widget_destroy (GTK_WIDGET (witem));

          /* Must get new list since the old changed.  */
          g_list_free (*list);
          *list = iter = gtk_container_get_children (GTK_CONTAINER (menubar));
          while (nr-- > 0) iter = g_list_next (iter);
        }
      else if (! val_in_menubar && ! iter_in_new_menubar)
        {
          /*  This corresponds to:
                Current:  A B C
                New:      A X C
              Rename B to X.  This might seem to be a strange thing to do,
              since if there is a menu under B it will be totally wrong for X.
              But consider editing a C file.  Then there is a C-mode menu
              (corresponds to B above).
              If then doing C-x C-f the minibuf menu (X above) replaces the
              C-mode menu.  When returning from the minibuffer, we get
              back the C-mode menu.  Thus we do:
                Rename B to X (C-mode to minibuf menu)
                Rename X to B (minibuf to C-mode menu).
              If the X menu hasn't been invoked, the menu under B
              is up to date when leaving the minibuffer.  */
          GtkLabel *wlabel = GTK_LABEL (gtk_bin_get_child (GTK_BIN (witem)));
          char *utf8_label = get_utf8_string (val->name);
          GtkWidget *submenu = gtk_menu_item_get_submenu (witem);

          gtk_label_set_text (wlabel, utf8_label);

          /* If this item has a submenu that has been detached, change
             the title in the WM decorations also.  */
          if (submenu && gtk_menu_get_tearoff_state (GTK_MENU (submenu)))
            /* Set the title of the detached window.  */
            gtk_menu_set_title (GTK_MENU (submenu), utf8_label);

          iter = g_list_next (iter);
          val = val->next;
          ++pos;
        }
      else if (! val_in_menubar && iter_in_new_menubar)
        {
          /*  This corresponds to:
                Current:  A B C
                New:      A X B C
              Insert X.  */

          int nr = pos;
          GList *group = 0;
          GtkWidget *w = xg_create_one_menuitem (val,
                                                 f,
                                                 select_cb,
                                                 highlight_cb,
                                                 cl_data,
                                                 &group);

          /* Create a possibly empty submenu for menu bar items, since some
             themes don't highlight items correctly without it. */
          GtkWidget *submenu = create_menus (NULL, f,
                                             select_cb, NULL, highlight_cb,
                                             0, 0, 0, 0, cl_data, 0);
          gtk_widget_set_name (w, MENU_ITEM_NAME);
          gtk_menu_shell_insert (GTK_MENU_SHELL (menubar), w, pos);
          gtk_menu_item_set_submenu (GTK_MENU_ITEM (w), submenu);

          g_list_free (*list);
          *list = iter = gtk_container_get_children (GTK_CONTAINER (menubar));
          while (nr-- > 0) iter = g_list_next (iter);
          iter = g_list_next (iter);
          val = val->next;
          ++pos;
        }
      else /* if (val_in_menubar && iter_in_new_menubar) */
        {
          int nr = pos;
          /*  This corresponds to:
                Current:  A B C
                New:      A C B
              Move C before B  */

          gtk_widget_ref (GTK_WIDGET (witem2));
          gtk_container_remove (GTK_CONTAINER (menubar), GTK_WIDGET (witem2));
          gtk_menu_shell_insert (GTK_MENU_SHELL (menubar),
                                 GTK_WIDGET (witem2), pos);
          gtk_widget_unref (GTK_WIDGET (witem2));

          g_list_free (*list);
          *list = iter = gtk_container_get_children (GTK_CONTAINER (menubar));
          while (nr-- > 0) iter = g_list_next (iter);
          if (iter) iter = g_list_next (iter);
          val = val->next;
          ++pos;
      }
    }

  /* Update the rest of the menu bar.  */
  xg_update_menubar (menubar, f, list, iter, pos, val,
                     select_cb, highlight_cb, cl_data);
}

/* Update the menu item W so it corresponds to VAL.
   SELECT_CB is the callback to use when a menu item is selected.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.
   CL_DATA is the data to set in the widget for menu invokation.  */

static void
xg_update_menu_item (val, w, select_cb, highlight_cb, cl_data)
     widget_value *val;
     GtkWidget *w;
     GCallback select_cb;
     GCallback highlight_cb;
     xg_menu_cb_data *cl_data;
{
  GtkWidget *wchild;
  GtkLabel *wlbl = 0;
  GtkLabel *wkey = 0;
  char *utf8_label;
  char *utf8_key;
  const char *old_label = 0;
  const char *old_key = 0;
  xg_menu_item_cb_data *cb_data;

  wchild = gtk_bin_get_child (GTK_BIN (w));
  utf8_label = get_utf8_string (val->name);
  utf8_key = get_utf8_string (val->key);

  /* See if W is a menu item with a key.  See make_menu_item above.  */
  if (GTK_IS_HBOX (wchild))
    {
      GList *list = gtk_container_get_children (GTK_CONTAINER (wchild));

      wlbl = GTK_LABEL (list->data);
      wkey = GTK_LABEL (list->next->data);
      g_list_free (list);

      if (! utf8_key)
        {
          /* Remove the key and keep just the label.  */
          gtk_widget_ref (GTK_WIDGET (wlbl));
          gtk_container_remove (GTK_CONTAINER (w), wchild);
          gtk_container_add (GTK_CONTAINER (w), GTK_WIDGET (wlbl));
          wkey = 0;
        }

    }
  else /* Just a label.  */
    {
      wlbl = GTK_LABEL (wchild);

      /* Check if there is now a key.  */
      if (utf8_key)
        {
          GtkWidget *wtoadd = make_widget_for_menu_item (utf8_label, utf8_key);
          GList *list = gtk_container_get_children (GTK_CONTAINER (wtoadd));

          wlbl = GTK_LABEL (list->data);
          wkey = GTK_LABEL (list->next->data);
          g_list_free (list);

          gtk_container_remove (GTK_CONTAINER (w), wchild);
          gtk_container_add (GTK_CONTAINER (w), wtoadd);
        }
    }


  if (wkey) old_key = gtk_label_get_label (wkey);
  if (wlbl) old_label = gtk_label_get_label (wlbl);

  if (wkey && utf8_key && (! old_key || strcmp (utf8_key, old_key) != 0))
    gtk_label_set_text (wkey, utf8_key);

  if (! old_label || strcmp (utf8_label, old_label) != 0)
    gtk_label_set_text (wlbl, utf8_label);

  if (utf8_key && utf8_key != val->key) g_free (utf8_key);
  if (utf8_label && utf8_label != val->name) g_free (utf8_label);

  if (! val->enabled && GTK_WIDGET_SENSITIVE (w))
    gtk_widget_set_sensitive (w, FALSE);
  else if (val->enabled && ! GTK_WIDGET_SENSITIVE (w))
    gtk_widget_set_sensitive (w, TRUE);

  cb_data = (xg_menu_item_cb_data*) g_object_get_data (G_OBJECT (w),
                                                       XG_ITEM_DATA);
  if (cb_data)
    {
      cb_data->call_data = val->call_data;
      cb_data->help = val->help;
      cb_data->cl_data = cl_data;

      /* We assume the callback functions don't change.  */
      if (val->call_data && ! val->contents)
        {
          /* This item shall have a select callback.  */
          if (! cb_data->select_id)
            cb_data->select_id
              = g_signal_connect (G_OBJECT (w), "activate",
                                  select_cb, cb_data);
        }
      else if (cb_data->select_id)
        {
          g_signal_handler_disconnect (w, cb_data->select_id);
          cb_data->select_id = 0;
        }
    }
}

/* Update the toggle menu item W so it corresponds to VAL.  */

static void
xg_update_toggle_item (val, w)
     widget_value *val;
     GtkWidget *w;
{
  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (w), val->selected);
}

/* Update the radio menu item W so it corresponds to VAL.  */

static void
xg_update_radio_item (val, w)
     widget_value *val;
     GtkWidget *w;
{
  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (w), val->selected);
}

/* Update the sub menu SUBMENU and all its children so it corresponds to VAL.
   SUBMENU may be NULL, in that case a new menu is created.
   F is the frame the menu bar belongs to.
   VAL describes the contents of the menu bar.
   SELECT_CB is the callback to use when a menu item is selected.
   DEACTIVATE_CB is the callback to use when a sub menu is not shown anymore.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.
   CL_DATA is the call back data to use for any newly created items.

   Returns the updated submenu widget, that is SUBMENU unless SUBMENU
   was NULL.  */

static GtkWidget *
xg_update_submenu (submenu, f, val,
                   select_cb, deactivate_cb, highlight_cb, cl_data)
     GtkWidget *submenu;
     FRAME_PTR f;
     widget_value *val;
     GCallback select_cb;
     GCallback deactivate_cb;
     GCallback highlight_cb;
     xg_menu_cb_data *cl_data;
{
  GtkWidget *newsub = submenu;
  GList *list = 0;
  GList *iter;
  widget_value *cur;
  int has_tearoff_p = 0;
  GList *first_radio = 0;

  if (submenu)
    list = gtk_container_get_children (GTK_CONTAINER (submenu));

  for (cur = val, iter = list;
       cur && iter;
       iter = g_list_next (iter), cur = cur->next)
  {
    GtkWidget *w = GTK_WIDGET (iter->data);

    /* Skip tearoff items, they have no counterpart in val.  */
    if (GTK_IS_TEAROFF_MENU_ITEM (w))
      {
        has_tearoff_p = 1;
        iter = g_list_next (iter);
        if (iter) w = GTK_WIDGET (iter->data);
        else break;
      }

    /* Remember first radio button in a group.  If we get a mismatch in
       a radio group we must rebuild the whole group so that the connections
       in GTK becomes correct.  */
    if (cur->button_type == BUTTON_TYPE_RADIO && ! first_radio)
      first_radio = iter;
    else if (cur->button_type != BUTTON_TYPE_RADIO
             && ! GTK_IS_RADIO_MENU_ITEM (w))
      first_radio = 0;

    if (GTK_IS_SEPARATOR_MENU_ITEM (w))
      {
        if (! xg_separator_p (cur->name))
          break;
      }
    else if (GTK_IS_CHECK_MENU_ITEM (w))
      {
        if (cur->button_type != BUTTON_TYPE_TOGGLE)
          break;
        xg_update_toggle_item (cur, w);
        xg_update_menu_item (cur, w, select_cb, highlight_cb, cl_data);
      }
    else if (GTK_IS_RADIO_MENU_ITEM (w))
      {
        if (cur->button_type != BUTTON_TYPE_RADIO)
          break;
        xg_update_radio_item (cur, w);
        xg_update_menu_item (cur, w, select_cb, highlight_cb, cl_data);
      }
    else if (GTK_IS_MENU_ITEM (w))
      {
        GtkMenuItem *witem = GTK_MENU_ITEM (w);
        GtkWidget *sub;

        if (cur->button_type != BUTTON_TYPE_NONE ||
            xg_separator_p (cur->name))
          break;

        xg_update_menu_item (cur, w, select_cb, highlight_cb, cl_data);

        sub = gtk_menu_item_get_submenu (witem);
        if (sub && ! cur->contents)
          {
            /* Not a submenu anymore.  */
            gtk_widget_ref (sub);
            gtk_menu_item_remove_submenu (witem);
            gtk_widget_destroy (sub);
          }
        else if (cur->contents)
          {
            GtkWidget *nsub;

            nsub = xg_update_submenu (sub, f, cur->contents,
                                      select_cb, deactivate_cb,
                                      highlight_cb, cl_data);

            /* If this item just became a submenu, we must set it.  */
            if (nsub != sub)
              gtk_menu_item_set_submenu (witem, nsub);
          }
      }
    else
      {
        /* Structural difference.  Remove everything from here and down
           in SUBMENU.  */
        break;
      }
  }

  /* Remove widgets from first structual change.  */
  if (iter)
    {
      /* If we are adding new menu items below, we must remove from
         first radio button so that radio groups become correct.  */
      if (cur && first_radio) xg_destroy_widgets (first_radio);
      else xg_destroy_widgets (iter);
    }

  if (cur)
    {
      /* More items added.  Create them.  */
      newsub = create_menus (cur,
                             f,
                             select_cb,
                             deactivate_cb,
                             highlight_cb,
                             0,
                             0,
                             ! has_tearoff_p,
                             submenu,
                             cl_data,
                             0);
    }

  if (list) g_list_free (list);

  return newsub;
}

/* Update the MENUBAR.
   F is the frame the menu bar belongs to.
   VAL describes the contents of the menu bar.
   If DEEP_P is non-zero, rebuild all but the top level menu names in
   the MENUBAR.  If DEEP_P is zero, just rebuild the names in the menubar.
   SELECT_CB is the callback to use when a menu item is selected.
   DEACTIVATE_CB is the callback to use when a sub menu is not shown anymore.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.  */

void
xg_modify_menubar_widgets (menubar, f, val, deep_p,
                           select_cb, deactivate_cb, highlight_cb)
     GtkWidget *menubar;
     FRAME_PTR f;
     widget_value *val;
     int deep_p;
     GCallback select_cb;
     GCallback deactivate_cb;
     GCallback highlight_cb;
{
  xg_menu_cb_data *cl_data;
  GList *list = gtk_container_get_children (GTK_CONTAINER (menubar));

  if (! list) return;

  cl_data = (xg_menu_cb_data*) g_object_get_data (G_OBJECT (menubar),
                                                  XG_FRAME_DATA);

  xg_update_menubar (menubar, f, &list, list, 0, val->contents,
                     select_cb, highlight_cb, cl_data);

  if (deep_p)
    {
      widget_value *cur;

      /* Update all sub menus.
         We must keep the submenus (GTK menu item widgets) since the
         X Window in the XEvent that activates the menu are those widgets.  */

      /* Update cl_data, menu_item things in F may have changed.  */
      update_cl_data (cl_data, f, highlight_cb);

      for (cur = val->contents; cur; cur = cur->next)
        {
          GList *iter;
          GtkWidget *sub = 0;
          GtkWidget *newsub;
          GtkMenuItem *witem;

          /* Find sub menu that corresponds to val and update it.  */
          for (iter = list ; iter; iter = g_list_next (iter))
            {
              witem = GTK_MENU_ITEM (iter->data);
              if (xg_item_label_same_p (witem, cur->name))
                {
                  sub = gtk_menu_item_get_submenu (witem);
                  break;
                }
            }

          newsub = xg_update_submenu (sub,
                                      f,
                                      cur->contents,
                                      select_cb,
                                      deactivate_cb,
                                      highlight_cb,
                                      cl_data);
          /* sub may still be NULL.  If we just updated non deep and added
             a new menu bar item, it has no sub menu yet.  So we set the
             newly created sub menu under witem.  */
          if (newsub != sub)
            {
              xg_set_screen (newsub, f);
              gtk_menu_item_set_submenu (witem, newsub);
            }
        }
    }

  g_list_free (list);
  gtk_widget_show_all (menubar);
}

/* Recompute all the widgets of frame F, when the menu bar has been
   changed.  Value is non-zero if widgets were updated.  */

int
xg_update_frame_menubar (f)
     FRAME_PTR f;
{
  struct x_output *x = f->output_data.x;
  GtkRequisition req;

  if (!x->menubar_widget || GTK_WIDGET_MAPPED (x->menubar_widget))
    return 0;

  BLOCK_INPUT;

  gtk_box_pack_start (GTK_BOX (x->vbox_widget), x->menubar_widget,
                      FALSE, FALSE, 0);
  gtk_box_reorder_child (GTK_BOX (x->vbox_widget), x->menubar_widget, 0);

  gtk_widget_show_all (x->menubar_widget);
  gtk_widget_size_request (x->menubar_widget, &req);

  FRAME_MENUBAR_HEIGHT (f) = req.height;

  /* The height has changed, resize outer widget and set columns
     rows to what we had before adding the menu bar.  */
  xg_resize_outer_widget (f, FRAME_COLS (f), FRAME_LINES (f));

  SET_FRAME_GARBAGED (f);
  UNBLOCK_INPUT;

  return 1;
}

/* Get rid of the menu bar of frame F, and free its storage.
   This is used when deleting a frame, and when turning off the menu bar.  */

void
free_frame_menubar (f)
     FRAME_PTR f;
{
  struct x_output *x = f->output_data.x;

  if (x->menubar_widget)
    {
      BLOCK_INPUT;

      gtk_container_remove (GTK_CONTAINER (x->vbox_widget), x->menubar_widget);
       /* The menubar and its children shall be deleted when removed from
          the container.  */
      x->menubar_widget = 0;
      FRAME_MENUBAR_HEIGHT (f) = 0;

      /* The height has changed, resize outer widget and set columns
         rows to what we had before removing the menu bar.  */
      xg_resize_outer_widget (f, FRAME_COLS (f), FRAME_LINES (f));

      SET_FRAME_GARBAGED (f);
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
                      Scroll bar functions
 ***********************************************************************/


/* Setting scroll bar values invokes the callback.  Use this variable
   to indicate that callback should do nothing.  */

int xg_ignore_gtk_scrollbar;

/* SET_SCROLL_BAR_X_WINDOW assumes the second argument fits in
   32 bits.  But we want to store pointers, and they may be larger
   than 32 bits.  Keep a mapping from integer index to widget pointers
   to get around the 32 bit limitation.  */

static struct
{
  GtkWidget **widgets;
  int max_size;
  int used;
} id_to_widget;

/* Grow this much every time we need to allocate more  */

#define ID_TO_WIDGET_INCR  32

/* Store the widget pointer W in id_to_widget and return the integer index.  */

static int
xg_store_widget_in_map (w)
     GtkWidget *w;
{
  int i;

  if (id_to_widget.max_size == id_to_widget.used)
    {
      int new_size = id_to_widget.max_size + ID_TO_WIDGET_INCR;

      id_to_widget.widgets = xrealloc (id_to_widget.widgets,
                                       sizeof (GtkWidget *)*new_size);

      for (i = id_to_widget.max_size; i < new_size; ++i)
        id_to_widget.widgets[i] = 0;
      id_to_widget.max_size = new_size;
    }

  /* Just loop over the array and find a free place.  After all,
     how many scroll bars are we creating?  Should be a small number.
     The check above guarantees we will find a free place.  */
  for (i = 0; i < id_to_widget.max_size; ++i)
    {
      if (! id_to_widget.widgets[i])
        {
          id_to_widget.widgets[i] = w;
          ++id_to_widget.used;

          return i;
        }
    }

  /* Should never end up here  */
  abort ();
}

/* Remove pointer at IDX from id_to_widget.
   Called when scroll bar is destroyed.  */

static void
xg_remove_widget_from_map (idx)
     int idx;
{
  if (idx < id_to_widget.max_size && id_to_widget.widgets[idx] != 0)
    {
      id_to_widget.widgets[idx] = 0;
      --id_to_widget.used;
    }
}

/* Get the widget pointer at IDX from id_to_widget. */

static GtkWidget *
xg_get_widget_from_map (idx)
     int idx;
{
  if (idx < id_to_widget.max_size && id_to_widget.widgets[idx] != 0)
    return id_to_widget.widgets[idx];

  return 0;
}

/* Return the scrollbar id for X Window WID on display DPY.
   Return -1 if WID not in id_to_widget.  */

int
xg_get_scroll_id_for_window (dpy, wid)
     Display *dpy;
     Window wid;
{
  int idx;
  GtkWidget *w;

  w = xg_win_to_widget (dpy, wid);

  if (w)
    {
      for (idx = 0; idx < id_to_widget.max_size; ++idx)
        if (id_to_widget.widgets[idx] == w)
          return idx;
    }

  return -1;
}

/* Callback invoked when scroll bar WIDGET is destroyed.
   DATA is the index into id_to_widget for WIDGET.
   We free pointer to last scroll bar values here and remove the index.  */

static void
xg_gtk_scroll_destroy (widget, data)
     GtkWidget *widget;
     gpointer data;
{
  gpointer p;
  int id = (int) (EMACS_INT) data; /* The EMACS_INT cast avoids a warning. */

  p = g_object_get_data (G_OBJECT (widget), XG_LAST_SB_DATA);
  if (p) xfree (p);
  xg_remove_widget_from_map (id);
}

/* Callback for button press/release events.  Used to start timer so that
   the scroll bar repetition timer in GTK gets handeled.
   Also, sets bar->dragging to Qnil when dragging (button release) is done.
   WIDGET is the scroll bar widget the event is for (not used).
   EVENT contains the event.
   USER_DATA points to the struct scrollbar structure.

   Returns FALSE to tell GTK that it shall continue propagate the event
   to widgets.  */

static gboolean
scroll_bar_button_cb (widget, event, user_data)
     GtkWidget *widget;
     GdkEventButton *event;
     gpointer user_data;
{
  if (event->type == GDK_BUTTON_PRESS && ! xg_timer)
    xg_start_timer ();
  else if (event->type == GDK_BUTTON_RELEASE)
    {
      struct scroll_bar *bar = (struct scroll_bar *) user_data;
      if (xg_timer) xg_stop_timer ();
      bar->dragging = Qnil;
    }

  return FALSE;
}

/* Create a scroll bar widget for frame F.  Store the scroll bar
   in BAR.
   SCROLL_CALLBACK is the callback to invoke when the value of the
   bar changes.
   SCROLL_BAR_NAME is the name we use for the scroll bar.  Can be used
   to set resources for the widget.  */

void
xg_create_scroll_bar (f, bar, scroll_callback, scroll_bar_name)
     FRAME_PTR f;
     struct scroll_bar *bar;
     GCallback scroll_callback;
     char *scroll_bar_name;
{
  GtkWidget *wscroll;
  GtkWidget *webox;
  GtkObject *vadj;
  int scroll_id;

  /* Page, step increment values are not so important here, they
     will be corrected in x_set_toolkit_scroll_bar_thumb. */
  vadj = gtk_adjustment_new (XG_SB_MIN, XG_SB_MIN, XG_SB_MAX,
                             0.1, 0.1, 0.1);

  wscroll = gtk_vscrollbar_new (GTK_ADJUSTMENT (vadj));
  webox = gtk_event_box_new ();
  gtk_widget_set_name (wscroll, scroll_bar_name);
  gtk_range_set_update_policy (GTK_RANGE (wscroll), GTK_UPDATE_CONTINUOUS);

  scroll_id = xg_store_widget_in_map (wscroll);

  g_signal_connect (G_OBJECT (wscroll),
                    "value-changed",
                    scroll_callback,
                    (gpointer) bar);
  /* The EMACS_INT cast avoids a warning. */
  g_signal_connect (G_OBJECT (wscroll),
                    "destroy",
                    G_CALLBACK (xg_gtk_scroll_destroy),
                    (gpointer) (EMACS_INT) scroll_id);

  /* Connect to button press and button release to detect if any scroll bar
     has the pointer.  */
  g_signal_connect (G_OBJECT (wscroll),
                    "button-press-event",
                    G_CALLBACK (scroll_bar_button_cb),
                    (gpointer) bar);
  g_signal_connect (G_OBJECT (wscroll),
                    "button-release-event",
                    G_CALLBACK (scroll_bar_button_cb),
                    (gpointer) bar);

  /* The scroll bar widget does not draw on a window of its own.  Instead
     it draws on the parent window, in this case the edit widget.  So
     whenever the edit widget is cleared, the scroll bar needs to redraw
     also, which causes flicker.  Put an event box between the edit widget
     and the scroll bar, so the scroll bar instead draws itself on the
     event box window.  */
  gtk_fixed_put (GTK_FIXED (f->output_data.x->edit_widget), webox, -1, -1);
  gtk_container_add (GTK_CONTAINER (webox), wscroll);


  /* Set the cursor to an arrow.  */
  xg_set_cursor (webox, FRAME_X_DISPLAY_INFO (f)->xg_cursor);

  SET_SCROLL_BAR_X_WINDOW (bar, scroll_id);
}

/* Make the scroll bar represented by SCROLLBAR_ID visible.  */

void
xg_show_scroll_bar (scrollbar_id)
     int scrollbar_id;
{
  GtkWidget *w = xg_get_widget_from_map (scrollbar_id);
  if (w)
    gtk_widget_show_all (gtk_widget_get_parent (w));
}

/* Remove the scroll bar represented by SCROLLBAR_ID from the frame F.  */

void
xg_remove_scroll_bar (f, scrollbar_id)
     FRAME_PTR f;
     int scrollbar_id;
{
  GtkWidget *w = xg_get_widget_from_map (scrollbar_id);
  if (w)
    {
      GtkWidget *wparent = gtk_widget_get_parent (w);
      gtk_widget_destroy (w);
      gtk_widget_destroy (wparent);
      SET_FRAME_GARBAGED (f);
    }
}

/* Update the position of the vertical scroll bar represented by SCROLLBAR_ID
   in frame F.
   TOP/LEFT are the new pixel positions where the bar shall appear.
   WIDTH, HEIGHT is the size in pixels the bar shall have.  */

void
xg_update_scrollbar_pos (f, scrollbar_id, top, left, width, height)
     FRAME_PTR f;
     int scrollbar_id;
     int top;
     int left;
     int width;
     int height;
{

  GtkWidget *wscroll = xg_get_widget_from_map (scrollbar_id);

  if (wscroll)
    {
      GtkWidget *wfixed = f->output_data.x->edit_widget;
      GtkWidget *wparent = gtk_widget_get_parent (wscroll);

      /* Move and resize to new values.  */
      gtk_fixed_move (GTK_FIXED (wfixed), wparent, left, top);
      gtk_widget_set_size_request (wscroll, width, height);
      gtk_widget_queue_draw (wparent);
      gdk_window_process_all_updates ();
      /* GTK does not redraw until the main loop is entered again, but
         if there are no X events pending we will not enter it.  So we sync
         here to get some events.  */
      x_sync (f);
      SET_FRAME_GARBAGED (f);
      cancel_mouse_face (f);
    }
}

/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

void
xg_set_toolkit_scroll_bar_thumb (bar, portion, position, whole)
     struct scroll_bar *bar;
     int portion, position, whole;
{
  GtkWidget *wscroll = xg_get_widget_from_map (SCROLL_BAR_X_WINDOW (bar));

  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  if (wscroll && NILP (bar->dragging))
    {
      GtkAdjustment *adj;
      gdouble shown;
      gdouble top;
      int size, value;
      int new_step;
      int changed = 0;

      adj = gtk_range_get_adjustment (GTK_RANGE (wscroll));

      /* We do the same as for MOTIF in xterm.c, assume 30 chars per line
         rather than the real portion value.  This makes the thumb less likely
         to resize and that looks better.  */
      portion = WINDOW_TOTAL_LINES (XWINDOW (bar->window)) * 30;
      /* When the thumb is at the bottom, position == whole.
         So we need to increase `whole' to make space for the thumb.  */
      whole += portion;

      if (whole <= 0)
        top = 0, shown = 1;
      else
        {
          top = (gdouble) position / whole;
          shown = (gdouble) portion / whole;
        }

      size = shown * XG_SB_RANGE;
      size = min (size, XG_SB_RANGE);
      size = max (size, 1);

      value = top * XG_SB_RANGE;
      value = min (value, XG_SB_MAX - size);
      value = max (value, XG_SB_MIN);

      /* Assume all lines are of equal size.  */
      new_step = size / max (1, FRAME_LINES (f));

      if ((int) adj->page_size != size
          || (int) adj->step_increment != new_step)
        {
          adj->page_size = size;
          adj->step_increment = new_step;
          /* Assume a page increment is about 95% of the page size  */
          adj->page_increment = (int) (0.95*adj->page_size);
          changed = 1;
        }

      if (changed || (int) gtk_range_get_value (GTK_RANGE (wscroll)) != value)
      {
        GtkWidget *wfixed = f->output_data.x->edit_widget;

        BLOCK_INPUT;

        /* gtk_range_set_value invokes the callback.  Set
           ignore_gtk_scrollbar to make the callback do nothing  */
        xg_ignore_gtk_scrollbar = 1;

        if ((int) gtk_range_get_value (GTK_RANGE (wscroll)) != value)
          gtk_range_set_value (GTK_RANGE (wscroll), (gdouble)value);
        else if (changed)
          gtk_adjustment_changed (adj);

        xg_ignore_gtk_scrollbar = 0;

        UNBLOCK_INPUT;
      }
    }
}


/***********************************************************************
                      Tool bar functions
 ***********************************************************************/
/* The key for the data we put in the GtkImage widgets.  The data is
   the image used by Emacs.  We use this to see if we need to update
   the GtkImage with a new image.  */
#define XG_TOOL_BAR_IMAGE_DATA "emacs-tool-bar-image"

/* The key for storing the latest modifiers so the activate callback can
   get them.  */
#define XG_TOOL_BAR_LAST_MODIFIER "emacs-tool-bar-modifier"

/* The key for storing the button widget in its proxy menu item. */
#define XG_TOOL_BAR_PROXY_BUTTON "emacs-tool-bar-proxy-button"

static gboolean
xg_tool_bar_button_cb (widget, event, user_data)
    GtkWidget      *widget;
    GdkEventButton *event;
    gpointer        user_data;
{
  /* Casts to avoid warnings when gpointer is 64 bits and int is 32 bits */
  gpointer ptr = (gpointer) (EMACS_INT) event->state;
  g_object_set_data (G_OBJECT (widget), XG_TOOL_BAR_LAST_MODIFIER, ptr);
  return FALSE;
}


/* Callback function invoked when a tool bar item is pressed.
   W is the button widget in the tool bar that got pressed,
   CLIENT_DATA is an integer that is the index of the button in the
   tool bar.  0 is the first button.  */

static void
xg_tool_bar_callback (w, client_data)
     GtkWidget *w;
     gpointer client_data;
{
  /* The EMACS_INT cast avoids a warning. */
  int idx = (int) (EMACS_INT) client_data;
  int mod = (int) (EMACS_INT) g_object_get_data (G_OBJECT (w),
                                                 XG_TOOL_BAR_LAST_MODIFIER);

  FRAME_PTR f = (FRAME_PTR) g_object_get_data (G_OBJECT (w), XG_FRAME_DATA);
  Lisp_Object key, frame;
  struct input_event event;
  EVENT_INIT (event);

  if (! f || ! f->n_tool_bar_items || NILP (f->tool_bar_items))
    return;

  idx *= TOOL_BAR_ITEM_NSLOTS;

  key = AREF (f->tool_bar_items, idx + TOOL_BAR_ITEM_KEY);
  XSETFRAME (frame, f);
  event.kind = TOOL_BAR_EVENT;
  event.frame_or_window = frame;
  event.arg = frame;
  kbd_buffer_store_event (&event);

  event.kind = TOOL_BAR_EVENT;
  event.frame_or_window = frame;
  event.arg = key;
  /* Convert between the modifier bits GDK uses and the modifier bits
     Emacs uses.  This assumes GDK an X masks are the same, which they are when
     this is written.  */
  event.modifiers = x_x_to_emacs_modifiers (FRAME_X_DISPLAY_INFO (f), mod);
  kbd_buffer_store_event (&event);
}

/* Callback function invoked when a tool bar item is pressed in a detached
   tool bar or the overflow drop down menu.
   We just call xg_tool_bar_callback.
   W is the menu item widget that got pressed,
   CLIENT_DATA is an integer that is the index of the button in the
   tool bar.  0 is the first button.  */

static void
xg_tool_bar_proxy_callback (w, client_data)
     GtkWidget *w;
     gpointer client_data;
{
  GtkWidget *wbutton = GTK_WIDGET (g_object_get_data (G_OBJECT (w),
                                                      XG_TOOL_BAR_PROXY_BUTTON));
  xg_tool_bar_callback (wbutton, client_data);
}

/* This callback is called when a tool item should create a proxy item,
   such as for the overflow menu.  Also called when the tool bar is detached.
   If we don't create a proxy menu item, the detached tool bar will be
   blank.  */

static gboolean
xg_tool_bar_menu_proxy (toolitem, user_data)
     GtkToolItem *toolitem;
     gpointer user_data;
{
  GtkWidget *weventbox = gtk_bin_get_child (GTK_BIN (toolitem));
  GtkButton *wbutton = GTK_BUTTON (gtk_bin_get_child (GTK_BIN (weventbox)));
  GtkWidget *wmenuitem = gtk_image_menu_item_new ();
  GtkWidget *wmenuimage;

  if (gtk_button_get_use_stock (wbutton)) 
    wmenuimage = gtk_image_new_from_stock (gtk_button_get_label (wbutton),
                                           GTK_ICON_SIZE_MENU);
  else
    {
      GtkImage *wimage = GTK_IMAGE (gtk_bin_get_child (GTK_BIN (wbutton)));
      GtkSettings *settings = gtk_widget_get_settings (GTK_WIDGET (wbutton));
      GtkImageType store_type = gtk_image_get_storage_type (wimage);
      if (store_type == GTK_IMAGE_STOCK)
        {
          gchar *stock_id;
          gtk_image_get_stock (wimage, &stock_id, NULL);
          wmenuimage = gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_MENU);
        }
      else if (store_type == GTK_IMAGE_ICON_SET)
        {
          GtkIconSet *icon_set;
          gtk_image_get_icon_set (wimage, &icon_set, NULL);
          wmenuimage = gtk_image_new_from_icon_set (icon_set,
                                                    GTK_ICON_SIZE_MENU);
        }
      else if (store_type == GTK_IMAGE_PIXBUF)
        {
          gint width, height;
      
          if (settings &&
              gtk_icon_size_lookup_for_settings (settings, GTK_ICON_SIZE_MENU,
                                                 &width, &height))
            {
              GdkPixbuf *src_pixbuf, *dest_pixbuf;

              src_pixbuf = gtk_image_get_pixbuf (wimage);
              dest_pixbuf = gdk_pixbuf_scale_simple (src_pixbuf, width, height,
                                                     GDK_INTERP_BILINEAR);

              wmenuimage = gtk_image_new_from_pixbuf (dest_pixbuf);
            }
        }
    }
  if (wmenuimage)
    gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (wmenuitem), wmenuimage);

  g_signal_connect (G_OBJECT (wmenuitem),
                    "activate",
                    GTK_SIGNAL_FUNC (xg_tool_bar_proxy_callback),
                    user_data);

  g_object_set_data (G_OBJECT (wmenuitem), XG_TOOL_BAR_PROXY_BUTTON,
                     (gpointer) wbutton);
  gtk_tool_item_set_proxy_menu_item (toolitem, "Emacs toolbar item", wmenuitem);

  return TRUE;
}

/* This callback is called when a tool bar is detached.  We must set
   the height of the tool bar to zero when this happens so frame sizes
   are correctly calculated.
   WBOX is the handle box widget that enables detach/attach of the tool bar.
   W is the tool bar widget.
   CLIENT_DATA is a pointer to the frame the tool bar belongs to.  */

static void
xg_tool_bar_detach_callback (wbox, w, client_data)
     GtkHandleBox *wbox;
     GtkWidget *w;
     gpointer client_data;
{
  FRAME_PTR f = (FRAME_PTR) client_data;
  extern int x_gtk_whole_detached_tool_bar;

  g_object_set (G_OBJECT (w), "show-arrow", !x_gtk_whole_detached_tool_bar,
		NULL);

  if (f)
    {
      FRAME_X_OUTPUT (f)->toolbar_detached = 1;

      /* When detaching a tool bar, not everything dissapear.  There are
         a few pixels left that are used to drop the tool bar back into
         place.  */
      FRAME_TOOLBAR_HEIGHT (f) = 2;

      /* The height has changed, resize outer widget and set columns
         rows to what we had before detaching the tool bar.  */
      xg_resize_outer_widget (f, FRAME_COLS (f), FRAME_LINES (f));
    }
}

/* This callback is called when a tool bar is reattached.  We must set
   the height of the tool bar when this happens so frame sizes
   are correctly calculated.
   WBOX is the handle box widget that enables detach/attach of the tool bar.
   W is the tool bar widget.
   CLIENT_DATA is a pointer to the frame the tool bar belongs to.  */

static void
xg_tool_bar_attach_callback (wbox, w, client_data)
     GtkHandleBox *wbox;
     GtkWidget *w;
     gpointer client_data;
{
  FRAME_PTR f = (FRAME_PTR) client_data;
  g_object_set (G_OBJECT (w), "show-arrow", TRUE, NULL);

  if (f)
    {
      GtkRequisition req;

      FRAME_X_OUTPUT (f)->toolbar_detached = 0;

      gtk_widget_size_request (w, &req);
      FRAME_TOOLBAR_HEIGHT (f) = req.height;

      /* The height has changed, resize outer widget and set columns
         rows to what we had before attaching the tool bar.  */
      xg_resize_outer_widget (f, FRAME_COLS (f), FRAME_LINES (f));
    }
}

/* This callback is called when the mouse enters or leaves a tool bar item.
   It is used for displaying and hiding the help text.
   W is the tool bar item, a button.
   EVENT is either an enter event or leave event.
   CLIENT_DATA is an integer that is the index of the button in the
   tool bar.  0 is the first button.

   Returns FALSE to tell GTK to keep processing this event.  */

static gboolean
xg_tool_bar_help_callback (w, event, client_data)
     GtkWidget *w;
     GdkEventCrossing *event;
     gpointer client_data;
{
  /* The EMACS_INT cast avoids a warning. */
  int idx = (int) (EMACS_INT) client_data;
  FRAME_PTR f = (FRAME_PTR) g_object_get_data (G_OBJECT (w), XG_FRAME_DATA);
  Lisp_Object help, frame;

  if (! f || ! f->n_tool_bar_items || NILP (f->tool_bar_items))
    return FALSE;

  if (event->type == GDK_ENTER_NOTIFY)
    {
      idx *= TOOL_BAR_ITEM_NSLOTS;
      help = AREF (f->tool_bar_items, idx + TOOL_BAR_ITEM_HELP);

      if (NILP (help))
        help = AREF (f->tool_bar_items, idx + TOOL_BAR_ITEM_CAPTION);
    }
  else
    help = Qnil;

  XSETFRAME (frame, f);
  kbd_buffer_store_help_event (frame, help);

  return FALSE;
}


/* This callback is called when a tool bar item shall be redrawn.
   It modifies the expose event so that the GtkImage widget redraws the
   whole image.  This to overcome a bug that makes GtkImage draw the image
   in the wrong place when it tries to redraw just a part of the image.
   W is the GtkImage to be redrawn.
   EVENT is the expose event for W.
   CLIENT_DATA is unused.

   Returns FALSE to tell GTK to keep processing this event.  */

static gboolean
xg_tool_bar_item_expose_callback (w, event, client_data)
     GtkWidget *w;
     GdkEventExpose *event;
     gpointer client_data;
{
  gint width, height;

  gdk_drawable_get_size (event->window, &width, &height);

  event->area.x -= width > event->area.width ? width-event->area.width : 0;
  event->area.y -= height > event->area.height ? height-event->area.height : 0;

  event->area.x = max (0, event->area.x);
  event->area.y = max (0, event->area.y);

  event->area.width = max (width, event->area.width);
  event->area.height = max (height, event->area.height);

  return FALSE;
}

/* This callback is called when a tool bar shall be redrawn.
   We need to update the tool bar from here in case the image cache
   has deleted the pixmaps used in the tool bar.
   W is the GtkToolbar to be redrawn.
   EVENT is the expose event for W.
   CLIENT_DATA is pointing to the frame for this tool bar.

   Returns FALSE to tell GTK to keep processing this event.  */

static gboolean
xg_tool_bar_expose_callback (w, event, client_data)
     GtkWidget *w;
     GdkEventExpose *event;
     gpointer client_data;
{
  update_frame_tool_bar ((FRAME_PTR) client_data);
  return FALSE;
}

/* Create a tool bar for frame F.  */

static void
xg_create_tool_bar (f)
     FRAME_PTR f;
{
  struct x_output *x = f->output_data.x;
  GtkRequisition req;
  int vbox_pos = x->menubar_widget ? 1 : 0;

  x->toolbar_widget = gtk_toolbar_new ();
  x->handlebox_widget = gtk_handle_box_new ();
  x->toolbar_detached = 0;

  gtk_container_add (GTK_CONTAINER (x->handlebox_widget),
                     x->toolbar_widget);

  gtk_box_pack_start (GTK_BOX (x->vbox_widget), x->handlebox_widget,
                      FALSE, FALSE, 0);

  gtk_box_reorder_child (GTK_BOX (x->vbox_widget), x->handlebox_widget,
                         vbox_pos);

  gtk_widget_set_name (x->toolbar_widget, "emacs-toolbar");

  /* We only have icons, so override any user setting.  We could
     use the caption property of the toolbar item (see update_frame_tool_bar
     below), but some of those strings are long, making the toolbar so
     long it does not fit on the screen.  The GtkToolbar widget makes every
     item equal size, so the longest caption determine the size of every
     tool bar item.  I think the creators of the GtkToolbar widget
     counted on 4 or 5 character long strings.  */
  gtk_toolbar_set_style (GTK_TOOLBAR (x->toolbar_widget), GTK_TOOLBAR_ICONS);
  gtk_toolbar_set_orientation (GTK_TOOLBAR (x->toolbar_widget),
                               GTK_ORIENTATION_HORIZONTAL);

  g_signal_connect (G_OBJECT (x->handlebox_widget), "child-detached",
                    G_CALLBACK (xg_tool_bar_detach_callback), f);
  g_signal_connect (G_OBJECT (x->handlebox_widget), "child-attached",
                    G_CALLBACK (xg_tool_bar_attach_callback), f);
  g_signal_connect (G_OBJECT (x->toolbar_widget),
                    "expose-event",
                    G_CALLBACK (xg_tool_bar_expose_callback),
                    f);

  gtk_widget_show_all (x->handlebox_widget);

  gtk_widget_size_request (x->toolbar_widget, &req);
  FRAME_TOOLBAR_HEIGHT (f) = req.height;

  /* The height has changed, resize outer widget and set columns
     rows to what we had before adding the tool bar.  */
  xg_resize_outer_widget (f, FRAME_COLS (f), FRAME_LINES (f));

  SET_FRAME_GARBAGED (f);
}

/* Update the tool bar for frame F.  Add new buttons and remove old.  */

void
update_frame_tool_bar (f)
     FRAME_PTR f;
{
  int i;
  GtkRequisition old_req, new_req;
  struct x_output *x = f->output_data.x;
  int hmargin = 0, vmargin = 0;
  GtkToolItem *ti;

  if (! FRAME_GTK_WIDGET (f))
    return;

  BLOCK_INPUT;

  if (INTEGERP (Vtool_bar_button_margin)
      && XINT (Vtool_bar_button_margin) > 0)
    {
      hmargin = XFASTINT (Vtool_bar_button_margin);
      vmargin = XFASTINT (Vtool_bar_button_margin);
    }
  else if (CONSP (Vtool_bar_button_margin))
    {
      if (INTEGERP (XCAR (Vtool_bar_button_margin))
          && XINT (XCAR (Vtool_bar_button_margin)) > 0)
        hmargin = XFASTINT (XCAR (Vtool_bar_button_margin));

      if (INTEGERP (XCDR (Vtool_bar_button_margin))
          && XINT (XCDR (Vtool_bar_button_margin)) > 0)
        vmargin = XFASTINT (XCDR (Vtool_bar_button_margin));
    }

  /* The natural size (i.e. when GTK uses 0 as margin) looks best,
     so take DEFAULT_TOOL_BAR_BUTTON_MARGIN to mean "default for GTK",
     i.e. zero.  This means that margins less than
     DEFAULT_TOOL_BAR_BUTTON_MARGIN has no effect.  */
  hmargin = max (0, hmargin - DEFAULT_TOOL_BAR_BUTTON_MARGIN);
  vmargin = max (0, vmargin - DEFAULT_TOOL_BAR_BUTTON_MARGIN);

  if (! x->toolbar_widget)
    xg_create_tool_bar (f);

  gtk_widget_size_request (x->toolbar_widget, &old_req);

  for (i = 0; i < f->n_tool_bar_items; ++i)
    {
#define PROP(IDX) AREF (f->tool_bar_items, i * TOOL_BAR_ITEM_NSLOTS + (IDX))

      int enabled_p = !NILP (PROP (TOOL_BAR_ITEM_ENABLED_P));
      int selected_p = !NILP (PROP (TOOL_BAR_ITEM_SELECTED_P));
      int idx;
      int img_id;
      struct image *img;
      Lisp_Object image;
      GtkWidget *wbutton;
      GtkWidget *weventbox;

      ti = gtk_toolbar_get_nth_item (GTK_TOOLBAR (x->toolbar_widget), i);

      if (ti)
        {
          weventbox = gtk_bin_get_child (GTK_BIN (ti));
          wbutton = gtk_bin_get_child (GTK_BIN (weventbox));
        }

      /* If image is a vector, choose the image according to the
	 button state.  */
      image = PROP (TOOL_BAR_ITEM_IMAGES);
      if (VECTORP (image))
	{
	  if (enabled_p)
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_ENABLED_SELECTED
		   : TOOL_BAR_IMAGE_ENABLED_DESELECTED);
	  else
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_DISABLED_SELECTED
		   : TOOL_BAR_IMAGE_DISABLED_DESELECTED);

	  xassert (ASIZE (image) >= idx);
	  image = AREF (image, idx);
	}
      else
	idx = -1;

      /* Ignore invalid image specifications.  */
      if (!valid_image_p (image))
        {
          if (ti) gtk_widget_hide_all (GTK_WIDGET (ti));
          continue;
        }

      img_id = lookup_image (f, image);
      img = IMAGE_FROM_ID (f, img_id);
      prepare_image_for_display (f, img);

      if (img->load_failed_p || img->pixmap == None)
        {
          if (ti)
	    gtk_widget_hide_all (GTK_WIDGET (ti));
	  else
            {
              /* Insert an empty (non-image) button */
              weventbox = gtk_event_box_new ();
              wbutton = gtk_button_new ();
              gtk_button_set_focus_on_click (GTK_BUTTON (wbutton), FALSE);
              gtk_button_set_relief (GTK_BUTTON (wbutton), GTK_RELIEF_NONE);
              gtk_container_add (GTK_CONTAINER (weventbox), wbutton);
              ti = gtk_tool_item_new ();
              gtk_container_add (GTK_CONTAINER (ti), weventbox);
              gtk_toolbar_insert (GTK_TOOLBAR (x->toolbar_widget), ti, i);
            }
          continue;
        }

      if (ti == NULL)
        {
          GtkWidget *w = xg_get_image_for_pixmap (f, img, x->widget, NULL);
          gtk_misc_set_padding (GTK_MISC (w), hmargin, vmargin);
          wbutton = gtk_button_new ();
          gtk_button_set_focus_on_click (GTK_BUTTON (wbutton), FALSE);
          gtk_button_set_relief (GTK_BUTTON (wbutton), GTK_RELIEF_NONE);
          gtk_container_add (GTK_CONTAINER (wbutton), w);
          weventbox = gtk_event_box_new ();
          gtk_container_add (GTK_CONTAINER (weventbox), wbutton);
          ti = gtk_tool_item_new ();
          gtk_container_add (GTK_CONTAINER (ti), weventbox);
          gtk_toolbar_insert (GTK_TOOLBAR (x->toolbar_widget), ti, i);


          /* The EMACS_INT cast avoids a warning. */
          g_signal_connect (G_OBJECT (ti), "create-menu-proxy",
                            GTK_SIGNAL_FUNC (xg_tool_bar_menu_proxy),
                            (gpointer) (EMACS_INT) i);

          g_signal_connect (G_OBJECT (wbutton), "clicked",
                            GTK_SIGNAL_FUNC (xg_tool_bar_callback),
                            (gpointer) (EMACS_INT) i);

          gtk_widget_show_all (GTK_WIDGET (ti));

          /* Save the image so we can see if an update is needed when
             this function is called again.  */
          g_object_set_data (G_OBJECT (w), XG_TOOL_BAR_IMAGE_DATA,
                             (gpointer)img->pixmap);

          g_object_set_data (G_OBJECT (weventbox), XG_FRAME_DATA, (gpointer)f);

          /* Catch expose events to overcome an annoying redraw bug, see
             comment for xg_tool_bar_item_expose_callback.  */
          g_signal_connect (G_OBJECT (ti),
                            "expose-event",
                            G_CALLBACK (xg_tool_bar_item_expose_callback),
                            0);

          gtk_widget_set_sensitive (wbutton, enabled_p);
          gtk_tool_item_set_homogeneous (ti, FALSE);
          
          /* Callback to save modifyer mask (Shift/Control, etc).  GTK makes
             no distinction based on modifiers in the activate callback,
             so we have to do it ourselves.  */
          g_signal_connect (wbutton, "button-release-event",
                            GTK_SIGNAL_FUNC (xg_tool_bar_button_cb),
                            NULL);

          g_object_set_data (G_OBJECT (wbutton), XG_FRAME_DATA, (gpointer)f);

          /* Use enter/leave notify to show help.  We use the events
             rather than the GtkButton specific signals "enter" and
             "leave", so we can have only one callback.  The event
             will tell us what kind of event it is.  */
          /* The EMACS_INT cast avoids a warning. */
          g_signal_connect (G_OBJECT (weventbox),
                            "enter-notify-event",
                            G_CALLBACK (xg_tool_bar_help_callback),
                            (gpointer) (EMACS_INT) i);
          g_signal_connect (G_OBJECT (weventbox),
                            "leave-notify-event",
                            G_CALLBACK (xg_tool_bar_help_callback),
                            (gpointer) (EMACS_INT) i);
        }
      else
        {
          GtkWidget *wimage = gtk_bin_get_child (GTK_BIN (wbutton));
          Pixmap old_img = (Pixmap)g_object_get_data (G_OBJECT (wimage),
                                                      XG_TOOL_BAR_IMAGE_DATA);
          gtk_misc_set_padding (GTK_MISC (wimage), hmargin, vmargin);

          if (old_img != img->pixmap)
            (void) xg_get_image_for_pixmap (f, img, x->widget, wimage);

          g_object_set_data (G_OBJECT (wimage), XG_TOOL_BAR_IMAGE_DATA,
                             (gpointer)img->pixmap);

          gtk_widget_set_sensitive (wbutton, enabled_p);
          gtk_widget_show_all (GTK_WIDGET (ti));
       }

#undef PROP
    }

  /* Remove buttons not longer needed.  We just hide them so they
     can be reused later on.  */
  do
    {
      ti = gtk_toolbar_get_nth_item (GTK_TOOLBAR (x->toolbar_widget), i++);
      if (ti) gtk_widget_hide_all (GTK_WIDGET (ti));
    } while (ti != NULL);

  gtk_widget_size_request (x->toolbar_widget, &new_req);
  if (old_req.height != new_req.height
      && ! FRAME_X_OUTPUT (f)->toolbar_detached)
    {
      FRAME_TOOLBAR_HEIGHT (f) = new_req.height;
      xg_resize_outer_widget (f, FRAME_COLS (f), FRAME_LINES (f));
    }

  UNBLOCK_INPUT;
}

/* Deallocate all resources for the tool bar on frame F.
   Remove the tool bar.  */

void
free_frame_tool_bar (f)
     FRAME_PTR f;
{
  struct x_output *x = f->output_data.x;

  if (x->toolbar_widget)
    {
      BLOCK_INPUT;
      gtk_container_remove (GTK_CONTAINER (x->vbox_widget),
                            x->handlebox_widget);
      x->toolbar_widget = 0;
      x->handlebox_widget = 0;
      FRAME_TOOLBAR_HEIGHT (f) = 0;

      /* The height has changed, resize outer widget and set columns
         rows to what we had before removing the tool bar.  */
      xg_resize_outer_widget (f, FRAME_COLS (f), FRAME_LINES (f));

      SET_FRAME_GARBAGED (f);
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
                      Initializing
 ***********************************************************************/
void
xg_initialize ()
{
  GtkBindingSet *binding_set;

#if HAVE_XFT
  /* Work around a bug with corrupted data if libXft gets unloaded.  This way
     we keep it permanently linked in.  */
  XftInit (0);
#endif
  xg_ignore_gtk_scrollbar = 0;
  xg_detached_menus = 0;
  xg_menu_cb_list.prev = xg_menu_cb_list.next =
    xg_menu_item_cb_list.prev = xg_menu_item_cb_list.next = 0;

  id_to_widget.max_size = id_to_widget.used = 0;
  id_to_widget.widgets = 0;

  /* Remove F10 as a menu accelerator, it does not mix well with Emacs key
     bindings.  It doesn't seem to be any way to remove properties,
     so we set it to VoidSymbol which in X means "no key".  */
  gtk_settings_set_string_property (gtk_settings_get_default (),
                                    "gtk-menu-bar-accel",
                                    "VoidSymbol",
                                    EMACS_CLASS);

  /* Make GTK text input widgets use Emacs style keybindings.  This is
     Emacs after all.  */
  gtk_settings_set_string_property (gtk_settings_get_default (),
                                    "gtk-key-theme-name",
                                    "Emacs",
                                    EMACS_CLASS);

  /* Make dialogs close on C-g.  Since file dialog inherits from
     dialog, this works for them also.  */
  binding_set = gtk_binding_set_by_class (gtk_type_class (GTK_TYPE_DIALOG));
  gtk_binding_entry_add_signal (binding_set, GDK_g, GDK_CONTROL_MASK,
                                "close", 0);

  /* Make menus close on C-g.  */
  binding_set = gtk_binding_set_by_class (gtk_type_class (GTK_TYPE_MENU_SHELL));
  gtk_binding_entry_add_signal (binding_set, GDK_g, GDK_CONTROL_MASK,
                                "cancel", 0);
}

#endif /* USE_GTK */

/* arch-tag: fe7104da-bc1e-4aba-9bd1-f349c528f7e3
   (do not change this comment) */
