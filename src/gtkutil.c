/* Functions for creating and updating GTK widgets.

Copyright (C) 2003-2017 Free Software Foundation, Inc.

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

#ifdef USE_GTK
#include <float.h>
#include <stdio.h>

#include <c-ctype.h>

#include "lisp.h"
#include "dispextern.h"
#include "frame.h"
#include "systime.h"
#include "xterm.h"
#include "blockinput.h"
#include "window.h"
#include "gtkutil.h"
#include "termhooks.h"
#include "keyboard.h"
#include "coding.h"

#include <gdk/gdkkeysyms.h>

#ifdef HAVE_XFT
#include <X11/Xft/Xft.h>
#endif

#ifdef HAVE_GTK3
#include <gtk/gtkx.h>
#include "emacsgtkfixed.h"
#endif

#ifdef HAVE_XDBE
#include <X11/extensions/Xdbe.h>
#endif

#ifndef HAVE_GTK_WIDGET_SET_HAS_WINDOW
#define gtk_widget_set_has_window(w, b) \
  (gtk_fixed_set_has_window (GTK_FIXED (w), b))
#endif
#ifndef HAVE_GTK_DIALOG_GET_ACTION_AREA
#define gtk_dialog_get_action_area(w) ((w)->action_area)
#define gtk_dialog_get_content_area(w) ((w)->vbox)
#endif
#ifndef HAVE_GTK_WIDGET_GET_SENSITIVE
#define gtk_widget_get_sensitive(w) (GTK_WIDGET_SENSITIVE (w))
#endif
#ifndef HAVE_GTK_ADJUSTMENT_GET_PAGE_SIZE
#define gtk_adjustment_set_page_size(w, s) ((w)->page_size = (s))
#define gtk_adjustment_set_page_increment(w, s) ((w)->page_increment = (s))
#define gtk_adjustment_get_step_increment(w) ((w)->step_increment)
#define gtk_adjustment_set_step_increment(w, s) ((w)->step_increment = (s))
#endif
#if GTK_CHECK_VERSION (2, 12, 0)
#define remove_submenu(w) gtk_menu_item_set_submenu ((w), NULL)
#else
#define remove_submenu(w) gtk_menu_item_remove_submenu ((w))
#endif

#if ! GTK_CHECK_VERSION (2, 14, 0)
#define gtk_adjustment_configure(adj, xvalue, xlower,            \
                                 xupper, xstep_increment,        \
                                 xpage_increment, xpagesize)     \
  do {                                                           \
    adj->lower = xlower;                                         \
    adj->upper = xupper;                                         \
    adj->page_size = xpagesize;                                  \
    gtk_adjustment_set_value (adj, xvalue);                      \
    adj->page_increment = xpage_increment;                       \
    adj->step_increment = xstep_increment;                       \
  } while (0)
#endif /* < Gtk+ 2.14 */

#ifdef HAVE_FREETYPE
#if GTK_CHECK_VERSION (3, 2, 0)
#define USE_NEW_GTK_FONT_CHOOSER 1
#else
#define USE_NEW_GTK_FONT_CHOOSER 0
#define gtk_font_chooser_dialog_new(x, y) \
  gtk_font_selection_dialog_new (x)
#undef GTK_FONT_CHOOSER
#define GTK_FONT_CHOOSER(x) GTK_FONT_SELECTION_DIALOG (x)
#define  gtk_font_chooser_set_font(x, y) \
  gtk_font_selection_dialog_set_font_name (x, y)
#endif
#endif /* HAVE_FREETYPE */

#if GTK_CHECK_VERSION (3, 10, 0)
#define XG_TEXT_CANCEL "Cancel"
#define XG_TEXT_OK     "OK"
#define XG_TEXT_OPEN   "Open"
#else
#define XG_TEXT_CANCEL GTK_STOCK_CANCEL
#define XG_TEXT_OK     GTK_STOCK_OK
#define XG_TEXT_OPEN   GTK_STOCK_OPEN
#endif

#ifndef HAVE_GTK3
#ifdef USE_GTK_TOOLTIP
#define gdk_window_get_screen(w) gdk_drawable_get_screen (w)
#endif
#define gdk_window_get_geometry(w, a, b, c, d) \
  gdk_window_get_geometry (w, a, b, c, d, 0)
#define gdk_x11_window_lookup_for_display(d, w) \
  gdk_xid_table_lookup_for_display (d, w)
#define gtk_box_new(ori, spacing)                                       \
  ((ori) == GTK_ORIENTATION_HORIZONTAL                                  \
   ? gtk_hbox_new (FALSE, (spacing)) : gtk_vbox_new (FALSE, (spacing)))
#define gtk_scrollbar_new(ori, spacing)                                 \
  ((ori) == GTK_ORIENTATION_HORIZONTAL                                  \
   ? gtk_hscrollbar_new ((spacing)) : gtk_vscrollbar_new ((spacing)))
#ifndef GDK_KEY_g
#define GDK_KEY_g GDK_g
#endif
#endif /* HAVE_GTK3 */

#define XG_BIN_CHILD(x) gtk_bin_get_child (GTK_BIN (x))

static void update_theme_scrollbar_width (void);
static void update_theme_scrollbar_height (void);

#define TB_INFO_KEY "xg_frame_tb_info"
struct xg_frame_tb_info
{
  Lisp_Object last_tool_bar;
  Lisp_Object style;
  int n_last_items;
  int hmargin, vmargin;
  GtkTextDirection dir;
};

static GtkWidget * xg_get_widget_from_map (ptrdiff_t idx);


/***********************************************************************
                      Display handling functions
 ***********************************************************************/

/* Keep track of the default display, or NULL if there is none.  Emacs
   may close all its displays.  */

static GdkDisplay *gdpy_def;

/* When the GTK widget W is to be created on a display for F that
   is not the default display, set the display for W.
   W can be a GtkMenu or a GtkWindow widget.  */

static void
xg_set_screen (GtkWidget *w, struct frame *f)
{
  if (FRAME_X_DISPLAY (f) != DEFAULT_GDK_DISPLAY ())
    {
      GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (FRAME_X_DISPLAY (f));
      GdkScreen *gscreen = gdk_display_get_default_screen (gdpy);

      if (GTK_IS_MENU (w))
        gtk_menu_set_screen (GTK_MENU (w), gscreen);
      else
        gtk_window_set_screen (GTK_WINDOW (w), gscreen);
    }
}


/* Open a display named by DISPLAY_NAME.  The display is returned in *DPY.
   *DPY is set to NULL if the display can't be opened.

   Returns non-zero if display could be opened, zero if display could not
   be opened, and less than zero if the GTK version doesn't support
   multiple displays.  */

void
xg_display_open (char *display_name, Display **dpy)
{
  GdkDisplay *gdpy;

  unrequest_sigio ();  /* See comment in x_display_ok, xterm.c.  */
  gdpy = gdk_display_open (display_name);
  request_sigio ();
  if (!gdpy_def && gdpy)
    {
      gdpy_def = gdpy;
      gdk_display_manager_set_default_display (gdk_display_manager_get (),
					       gdpy);
    }

  *dpy = gdpy ? GDK_DISPLAY_XDISPLAY (gdpy) : NULL;
}

/* Scaling/HiDPI functions. */
static int
xg_get_gdk_scale (void)
{
  const char *sscale = getenv ("GDK_SCALE");

  if (sscale)
    {
      long scale = atol (sscale);
      if (0 < scale)
	return min (scale, INT_MAX);
    }

  return 1;
}

int
xg_get_scale (struct frame *f)
{
#if GTK_CHECK_VERSION (3, 10, 0)
  if (FRAME_GTK_WIDGET (f))
    return gtk_widget_get_scale_factor (FRAME_GTK_WIDGET (f));
#endif
  return xg_get_gdk_scale ();
}

/* Close display DPY.  */

void
xg_display_close (Display *dpy)
{
  GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (dpy);

  /* If this is the default display, try to change it before closing.
     If there is no other display to use, gdpy_def is set to NULL, and
     the next call to xg_display_open resets the default display.  */
  if (gdk_display_get_default () == gdpy)
    {
      struct x_display_info *dpyinfo;
      GdkDisplay *gdpy_new = NULL;

      /* Find another display.  */
      for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
        if (dpyinfo->display != dpy)
          {
	    gdpy_new = gdk_x11_lookup_xdisplay (dpyinfo->display);
	    gdk_display_manager_set_default_display (gdk_display_manager_get (),
						     gdpy_new);
            break;
          }
      gdpy_def = gdpy_new;
    }

#if GTK_CHECK_VERSION (2, 0, 0) && ! GTK_CHECK_VERSION (2, 10, 0)
  /* GTK 2.2-2.8 has a bug that makes gdk_display_close crash (bug
     http://bugzilla.gnome.org/show_bug.cgi?id=85715).  This way we
     can continue running, but there will be memory leaks.  */
  g_object_run_dispose (G_OBJECT (gdpy));
#else
  /* This seems to be fixed in GTK 2.10. */
  gdk_display_close (gdpy);
#endif
}


/***********************************************************************
                      Utility functions
 ***********************************************************************/

/* Create and return the cursor to be used for popup menus and
   scroll bars on display DPY.  */

GdkCursor *
xg_create_default_cursor (Display *dpy)
{
  GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (dpy);
  return gdk_cursor_new_for_display (gdpy, GDK_LEFT_PTR);
}

/* Apply GMASK to GPIX and return a GdkPixbuf with an alpha channel.  */

static GdkPixbuf *
xg_get_pixbuf_from_pix_and_mask (struct frame *f,
                                 Pixmap pix,
                                 Pixmap mask)
{
  GdkPixbuf *icon_buf = 0;
  int iunused;
  Window wunused;
  unsigned int width, height, depth, uunused;

  if (FRAME_DISPLAY_INFO (f)->red_bits != 8)
    return 0;
  XGetGeometry (FRAME_X_DISPLAY (f), pix, &wunused, &iunused, &iunused,
                &width, &height, &uunused, &depth);
  if (depth != 24)
    return 0;
  XImage *xim = XGetImage (FRAME_X_DISPLAY (f), pix, 0, 0, width, height,
			   ~0, XYPixmap);
  if (xim)
    {
      XImage *xmm = (! mask ? 0
		     : XGetImage (FRAME_X_DISPLAY (f), mask, 0, 0,
				  width, height, ~0, XYPixmap));
      icon_buf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, width, height);
      if (icon_buf)
	{
	  guchar *pixels = gdk_pixbuf_get_pixels (icon_buf);
	  int rowjunkwidth = gdk_pixbuf_get_rowstride (icon_buf) - width * 4;
	  for (int y = 0; y < height; y++, pixels += rowjunkwidth)
	    for (int x = 0; x < width; x++)
	      {
		unsigned long rgb = XGetPixel (xim, x, y);
		*pixels++ = (rgb >> 16) & 255;
		*pixels++ = (rgb >> 8) & 255;
		*pixels++ = rgb & 255;
		*pixels++ = xmm && !XGetPixel (xmm, x, y) ? 0 : 255;
	      }
	}

      if (xmm)
	XDestroyImage (xmm);
      XDestroyImage (xim);
    }

  return icon_buf;
}

static Lisp_Object
file_for_image (Lisp_Object image)
{
  Lisp_Object specified_file = Qnil;
  Lisp_Object tail;

  for (tail = XCDR (image);
       NILP (specified_file) && CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    if (EQ (XCAR (tail), QCfile))
      specified_file = XCAR (XCDR (tail));

  return specified_file;
}

/* For the image defined in IMG, make and return a GtkImage.  For displays with
   8 planes or less we must make a GdkPixbuf and apply the mask manually.
   Otherwise the highlighting and dimming the tool bar code in GTK does
   will look bad.  For display with more than 8 planes we just use the
   pixmap and mask directly.  For monochrome displays, GTK doesn't seem
   able to use external pixmaps, it looks bad whatever we do.
   The image is defined on the display where frame F is.
   WIDGET is used to find the GdkColormap to use for the GdkPixbuf.
   If OLD_WIDGET is NULL, a new widget is constructed and returned.
   If OLD_WIDGET is not NULL, that widget is modified.  */

static GtkWidget *
xg_get_image_for_pixmap (struct frame *f,
                         struct image *img,
                         GtkWidget *widget,
                         GtkImage *old_widget)
{
  GdkPixbuf *icon_buf;

  /* If we have a file, let GTK do all the image handling.
     This seems to be the only way to make insensitive and activated icons
     look good in all cases.  */
  Lisp_Object specified_file = file_for_image (img->spec);
  Lisp_Object file;

  /* We already loaded the image once before calling this
     function, so this only fails if the image file has been removed.
     In that case, use the pixmap already loaded.  */

  if (STRINGP (specified_file)
      && STRINGP (file = x_find_image_file (specified_file)))
    {
      char *encoded_file = SSDATA (ENCODE_FILE (file));
      if (! old_widget)
        old_widget = GTK_IMAGE (gtk_image_new_from_file (encoded_file));
      else
        gtk_image_set_from_file (old_widget, encoded_file);

      return GTK_WIDGET (old_widget);
    }

  /* No file, do the image handling ourselves.  This will look very bad
     on a monochrome display, and sometimes bad on all displays with
     certain themes.  */

  /* This is a workaround to make icons look good on pseudo color
     displays.  Apparently GTK expects the images to have an alpha
     channel.  If they don't, insensitive and activated icons will
     look bad.  This workaround does not work on monochrome displays,
     and is strictly not needed on true color/static color displays (i.e.
     16 bits and higher).  But we do it anyway so we get a pixbuf that is
     not associated with the img->pixmap.  The img->pixmap may be removed
     by clearing the image cache and then the tool bar redraw fails, since
     Gtk+ assumes the pixmap is always there.  */
  icon_buf = xg_get_pixbuf_from_pix_and_mask (f, img->pixmap, img->mask);

  if (icon_buf)
    {
      if (! old_widget)
        old_widget = GTK_IMAGE (gtk_image_new_from_pixbuf (icon_buf));
      else
        gtk_image_set_from_pixbuf (old_widget, icon_buf);

      g_object_unref (G_OBJECT (icon_buf));
    }

  return GTK_WIDGET (old_widget);
}


/* Set CURSOR on W and all widgets W contain.  We must do like this
   for scroll bars and menu because they create widgets internally,
   and it is those widgets that are visible.  */

static void
xg_set_cursor (GtkWidget *w, GdkCursor *cursor)
{
  GdkWindow *window = gtk_widget_get_window (w);
  GList *children = gdk_window_peek_children (window);

  gdk_window_set_cursor (window, cursor);

  /* The scroll bar widget has more than one GDK window (had to look at
     the source to figure this out), and there is no way to set cursor
     on widgets in GTK.  So we must set the cursor for all GDK windows.
     Ditto for menus.  */

  for ( ; children; children = g_list_next (children))
    gdk_window_set_cursor (GDK_WINDOW (children->data), cursor);
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
   utf8 or NULL, just return a copy of STR.
   A new string is allocated and the caller must free the result
   with g_free.  */

static char *
get_utf8_string (const char *str)
{
  char *utf8_str;

  if (!str) return NULL;

  /* If not UTF-8, try current locale.  */
  if (!g_utf8_validate (str, -1, NULL))
    utf8_str = g_locale_to_utf8 (str, -1, 0, 0, 0);
  else
    return g_strdup (str);

  if (!utf8_str)
    {
      /* Probably some control characters in str.  Escape them. */
      ptrdiff_t len;
      ptrdiff_t nr_bad = 0;
      gsize bytes_read;
      gsize bytes_written;
      unsigned char *p = (unsigned char *)str;
      char *cp, *up;
      GError *err = NULL;

      while (! (cp = g_locale_to_utf8 ((char *)p, -1, &bytes_read,
                                       &bytes_written, &err))
             && err->code == G_CONVERT_ERROR_ILLEGAL_SEQUENCE)
        {
          ++nr_bad;
          p += bytes_written+1;
          g_error_free (err);
          err = NULL;
        }

      if (err)
        {
          g_error_free (err);
          err = NULL;
        }
      if (cp) g_free (cp);

      len = strlen (str);
      ptrdiff_t alloc;
      if (INT_MULTIPLY_WRAPV (nr_bad, 4, &alloc)
	  || INT_ADD_WRAPV (len + 1, alloc, &alloc)
	  || SIZE_MAX < alloc)
	memory_full (SIZE_MAX);
      up = utf8_str = xmalloc (alloc);
      p = (unsigned char *)str;

      while (! (cp = g_locale_to_utf8 ((char *)p, -1, &bytes_read,
                                       &bytes_written, &err))
             && err->code == G_CONVERT_ERROR_ILLEGAL_SEQUENCE)
        {
          memcpy (up, p, bytes_written);
          up += bytes_written;
          up += sprintf (up, "\\%03o", p[bytes_written]);
          p += bytes_written + 1;
          g_error_free (err);
          err = NULL;
        }

      if (cp)
        {
          strcpy (up, cp);
          g_free (cp);
        }
      if (err)
        {
          g_error_free (err);
          err = NULL;
        }
    }
  return utf8_str;
}

/* Check for special colors used in face spec for region face.
   The colors are fetched from the Gtk+ theme.
   Return true if color was found, false if not.  */

bool
xg_check_special_colors (struct frame *f,
                         const char *color_name,
                         XColor *color)
{
  bool success_p = 0;
  bool get_bg = strcmp ("gtk_selection_bg_color", color_name) == 0;
  bool get_fg = !get_bg && strcmp ("gtk_selection_fg_color", color_name) == 0;

  if (! FRAME_GTK_WIDGET (f) || ! (get_bg || get_fg))
    return success_p;

  block_input ();
  {
#ifdef HAVE_GTK3
    GtkStyleContext *gsty
      = gtk_widget_get_style_context (FRAME_GTK_OUTER_WIDGET (f));
    GdkRGBA col;
    char buf[sizeof "rgb://rrrr/gggg/bbbb"];
    int state = GTK_STATE_FLAG_SELECTED|GTK_STATE_FLAG_FOCUSED;
    if (get_fg)
      gtk_style_context_get_color (gsty, state, &col);
    else
      {
        GdkRGBA *c;
        /* FIXME: Retrieving the background color is deprecated in
           GTK+ 3.16.  New versions of GTK+ don’t use the concept of a
           single background color any more, so we shouldn’t query for
           it.  */
        gtk_style_context_get (gsty, state,
                               GTK_STYLE_PROPERTY_BACKGROUND_COLOR, &c,
                               NULL);
        col = *c;
        gdk_rgba_free (c);
      }

    unsigned short
      r = col.red * 65535,
      g = col.green * 65535,
      b = col.blue * 65535;
    sprintf (buf, "rgb:%04x/%04x/%04x", r, g, b);
    success_p = x_parse_color (f, buf, color) != 0;
#else
    GtkStyle *gsty = gtk_widget_get_style (FRAME_GTK_WIDGET (f));
    GdkColor *grgb = get_bg
      ? &gsty->bg[GTK_STATE_SELECTED]
      : &gsty->fg[GTK_STATE_SELECTED];

    color->red = grgb->red;
    color->green = grgb->green;
    color->blue = grgb->blue;
    color->pixel = grgb->pixel;
    success_p = 1;
#endif

  }
  unblock_input ();
  return success_p;
}



/***********************************************************************
                              Tooltips
 ***********************************************************************/
/* Gtk+ calls this callback when the parent of our tooltip dummy changes.
   We use that to pop down the tooltip.  This happens if Gtk+ for some
   reason wants to change or hide the tooltip.  */

#ifdef USE_GTK_TOOLTIP

static void
hierarchy_ch_cb (GtkWidget *widget,
                 GtkWidget *previous_toplevel,
                 gpointer   user_data)
{
  struct frame *f = user_data;
  struct x_output *x = f->output_data.x;
  GtkWidget *top = gtk_widget_get_toplevel (x->ttip_lbl);

  if (! top || ! GTK_IS_WINDOW (top))
      gtk_widget_hide (previous_toplevel);
}

/* Callback called when Gtk+ thinks a tooltip should be displayed.
   We use it to get the tooltip window and the tooltip widget so
   we can manipulate the ourselves.

   Return FALSE ensures that the tooltip is not shown.  */

static gboolean
qttip_cb (GtkWidget  *widget,
          gint        xpos,
          gint        ypos,
          gboolean    keyboard_mode,
          GtkTooltip *tooltip,
          gpointer    user_data)
{
  struct frame *f = user_data;
  struct x_output *x = f->output_data.x;
  if (x->ttip_widget == NULL)
    {
      GtkWidget *p;
      GList *list, *iter;

      g_object_set (G_OBJECT (widget), "has-tooltip", FALSE, NULL);
      x->ttip_widget = tooltip;
      g_object_ref (G_OBJECT (tooltip));
      x->ttip_lbl = gtk_label_new ("");
      g_object_ref (G_OBJECT (x->ttip_lbl));
      gtk_tooltip_set_custom (tooltip, x->ttip_lbl);
      x->ttip_window = GTK_WINDOW (gtk_widget_get_toplevel (x->ttip_lbl));

      /* Change stupid Gtk+ default line wrapping.  */
      p = gtk_widget_get_parent (x->ttip_lbl);
      list = gtk_container_get_children (GTK_CONTAINER (p));
      for (iter = list; iter; iter = g_list_next (iter))
        {
          GtkWidget *w = GTK_WIDGET (iter->data);
          if (GTK_IS_LABEL (w))
            gtk_label_set_line_wrap (GTK_LABEL (w), FALSE);
        }
      g_list_free (list);

      /* ATK needs an empty title for some reason.  */
      gtk_window_set_title (x->ttip_window, "");
      /* Realize so we can safely get screen later on.  */
      gtk_widget_realize (GTK_WIDGET (x->ttip_window));
      gtk_widget_realize (x->ttip_lbl);

      g_signal_connect (x->ttip_lbl, "hierarchy-changed",
                        G_CALLBACK (hierarchy_ch_cb), f);
    }
  return FALSE;
}

#endif /* USE_GTK_TOOLTIP */

/* Prepare a tooltip to be shown, i.e. calculate WIDTH and HEIGHT.
   Return true if a system tooltip is available.  */

bool
xg_prepare_tooltip (struct frame *f,
                    Lisp_Object string,
                    int *width,
                    int *height)
{
#ifndef USE_GTK_TOOLTIP
  return 0;
#else
  struct x_output *x = f->output_data.x;
  GtkWidget *widget;
  GdkWindow *gwin;
  GdkScreen *screen;
  GtkSettings *settings;
  gboolean tt_enabled = TRUE;
  GtkRequisition req;
  Lisp_Object encoded_string;

  if (!x->ttip_lbl) return 0;

  block_input ();
  encoded_string = ENCODE_UTF_8 (string);
  widget = GTK_WIDGET (x->ttip_lbl);
  gwin = gtk_widget_get_window (GTK_WIDGET (x->ttip_window));
  screen = gdk_window_get_screen (gwin);
  settings = gtk_settings_get_for_screen (screen);
  g_object_get (settings, "gtk-enable-tooltips", &tt_enabled, NULL);
  if (tt_enabled)
    {
      g_object_set (settings, "gtk-enable-tooltips", FALSE, NULL);
      /* Record that we disabled it so it can be enabled again.  */
      g_object_set_data (G_OBJECT (x->ttip_window), "restore-tt",
                         (gpointer)f);
    }

  /* Prevent Gtk+ from hiding tooltip on mouse move and such.  */
  g_object_set_data (G_OBJECT
                     (gtk_widget_get_display (GTK_WIDGET (x->ttip_window))),
                     "gdk-display-current-tooltip", NULL);

  /* Put our dummy widget in so we can get callbacks for unrealize and
     hierarchy-changed.  */
  gtk_tooltip_set_custom (x->ttip_widget, widget);
  gtk_tooltip_set_text (x->ttip_widget, SSDATA (encoded_string));
  gtk_widget_get_preferred_size (GTK_WIDGET (x->ttip_window), NULL, &req);
  if (width) *width = req.width;
  if (height) *height = req.height;

  unblock_input ();

  return 1;
#endif /* USE_GTK_TOOLTIP */
}

/* Show the tooltip at ROOT_X and ROOT_Y.
   xg_prepare_tooltip must have been called before this function.  */

void
xg_show_tooltip (struct frame *f, int root_x, int root_y)
{
#ifdef USE_GTK_TOOLTIP
  struct x_output *x = f->output_data.x;
  if (x->ttip_window)
    {
      block_input ();
      gtk_window_move (x->ttip_window, root_x / xg_get_scale (f),
		       root_y / xg_get_scale (f));
      gtk_widget_show_all (GTK_WIDGET (x->ttip_window));
      unblock_input ();
    }
#endif
}

/* Hide tooltip if shown.  Do nothing if not shown.
   Return true if tip was hidden, false if not (i.e. not using
   system tooltips).  */

bool
xg_hide_tooltip (struct frame *f)
{
  bool ret = 0;
#ifdef USE_GTK_TOOLTIP
  if (f->output_data.x->ttip_window)
    {
      GtkWindow *win = f->output_data.x->ttip_window;
      block_input ();
      gtk_widget_hide (GTK_WIDGET (win));

      if (g_object_get_data (G_OBJECT (win), "restore-tt"))
        {
          GdkWindow *gwin = gtk_widget_get_window (GTK_WIDGET (win));
          GdkScreen *screen = gdk_window_get_screen (gwin);
          GtkSettings *settings = gtk_settings_get_for_screen (screen);
          g_object_set (settings, "gtk-enable-tooltips", TRUE, NULL);
        }
      unblock_input ();

      ret = 1;
    }
#endif
  return ret;
}


/***********************************************************************
    General functions for creating widgets, resizing, events, e.t.c.
 ***********************************************************************/

#if ! GTK_CHECK_VERSION (3, 22, 0)
static void
my_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
		const gchar *msg, gpointer user_data)
{
  if (!strstr (msg, "visible children"))
    fprintf (stderr, "XX %s-WARNING **: %s\n", log_domain, msg);
}
#endif

/* Make a geometry string and pass that to GTK.  It seems this is the
   only way to get geometry position right if the user explicitly
   asked for a position when starting Emacs.
   F is the frame we shall set geometry for.  */

static void
xg_set_geometry (struct frame *f)
{
  if (f->size_hint_flags & (USPosition | PPosition))
    {
#if ! GTK_CHECK_VERSION (3, 22, 0)
      if (x_gtk_use_window_move)
	{
#endif
	  /* Handle negative positions without consulting
	     gtk_window_parse_geometry (Bug#25851).  The position will
	     be off by scrollbar width + window manager decorations.  */
	  if (f->size_hint_flags & XNegative)
	    f->left_pos = (x_display_pixel_width (FRAME_DISPLAY_INFO (f))
			   - FRAME_PIXEL_WIDTH (f) + f->left_pos);

	  if (f->size_hint_flags & YNegative)
	    f->top_pos = (x_display_pixel_height (FRAME_DISPLAY_INFO (f))
			  - FRAME_PIXEL_HEIGHT (f) + f->top_pos);

	  gtk_window_move (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			   f->left_pos, f->top_pos);

	  /* Reset size hint flags.  */
	  f->size_hint_flags &= ~ (XNegative | YNegative);
# if ! GTK_CHECK_VERSION (3, 22, 0)
	}
      else
	{
	  int left = f->left_pos;
	  int xneg = f->size_hint_flags & XNegative;
	  int top = f->top_pos;
	  int yneg = f->size_hint_flags & YNegative;
	  char geom_str[sizeof "=x--" + 4 * INT_STRLEN_BOUND (int)];
	  guint id;

	  if (xneg)
	    left = -left;
	  if (yneg)
	    top = -top;

	  sprintf (geom_str, "=%dx%d%c%d%c%d",
		   FRAME_PIXEL_WIDTH (f),
		   FRAME_PIXEL_HEIGHT (f),
		   (xneg ? '-' : '+'), left,
		   (yneg ? '-' : '+'), top);

	  /* Silence warning about visible children.  */
	  id = g_log_set_handler ("Gtk", G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL
				  | G_LOG_FLAG_RECURSION, my_log_handler, NULL);

	  if (!gtk_window_parse_geometry (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
					  geom_str))
	    fprintf (stderr, "Failed to parse: '%s'\n", geom_str);

	  g_log_remove_handler ("Gtk", id);
	}
#endif
    }
}

/* Function to handle resize of our frame.  As we have a Gtk+ tool bar
   and a Gtk+ menu bar, we get resize events for the edit part of the
   frame only.  We let Gtk+ deal with the Gtk+ parts.
   F is the frame to resize.
   PIXELWIDTH, PIXELHEIGHT is the new size in pixels.  */

void
xg_frame_resized (struct frame *f, int pixelwidth, int pixelheight)
{
  int width, height;

  if (pixelwidth == -1 && pixelheight == -1)
    {
      if (FRAME_GTK_WIDGET (f) && gtk_widget_get_mapped (FRAME_GTK_WIDGET (f)))
	gdk_window_get_geometry (gtk_widget_get_window (FRAME_GTK_WIDGET (f)),
				 0, 0, &pixelwidth, &pixelheight);
      else
	return;
    }

  width = FRAME_PIXEL_TO_TEXT_WIDTH (f, pixelwidth);
  height = FRAME_PIXEL_TO_TEXT_HEIGHT (f, pixelheight);

  frame_size_history_add
    (f, Qxg_frame_resized, width, height, Qnil);

  if (width != FRAME_TEXT_WIDTH (f)
      || height != FRAME_TEXT_HEIGHT (f)
      || pixelwidth != FRAME_PIXEL_WIDTH (f)
      || pixelheight != FRAME_PIXEL_HEIGHT (f))
    {
      x_clear_under_internal_border (f);
      change_frame_size (f, width, height, 0, 1, 0, 1);
      SET_FRAME_GARBAGED (f);
      cancel_mouse_face (f);
    }
}

/* Resize the outer window of frame F after changing the height.
   COLUMNS/ROWS is the size the edit area shall have after the resize.  */

void
xg_frame_set_char_size (struct frame *f, int width, int height)
{
  int pixelwidth = FRAME_TEXT_TO_PIXEL_WIDTH (f, width);
  int pixelheight = FRAME_TEXT_TO_PIXEL_HEIGHT (f, height);
  Lisp_Object fullscreen = get_frame_param (f, Qfullscreen);
  gint gwidth, gheight;
  int totalheight
    = pixelheight + FRAME_TOOLBAR_HEIGHT (f) + FRAME_MENUBAR_HEIGHT (f);
  int totalwidth = pixelwidth + FRAME_TOOLBAR_WIDTH (f);

  if (FRAME_PIXEL_HEIGHT (f) == 0)
    return;

  gtk_window_get_size (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
		       &gwidth, &gheight);

  /* Do this before resize, as we don't know yet if we will be resized.  */
  x_clear_under_internal_border (f);

  totalheight /= xg_get_scale (f);
  totalwidth /= xg_get_scale (f);

  x_wm_set_size_hint (f, 0, 0);

  /* Resize the top level widget so rows and columns remain constant.

     When the frame is fullheight and we only want to change the width
     or it is fullwidth and we only want to change the height we should
     be able to preserve the fullscreen property.  However, due to the
     fact that we have to send a resize request anyway, the window
     manager will abolish it.  At least the respective size should
     remain unchanged but giving the frame back its normal size will
     be broken ... */
  if (EQ (fullscreen, Qfullwidth) && width == FRAME_TEXT_WIDTH (f))
    {
      frame_size_history_add
	(f, Qxg_frame_set_char_size_1, width, height,
	 list2 (make_number (gheight), make_number (totalheight)));

      gtk_window_resize (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			 gwidth, totalheight);
    }
  else if (EQ (fullscreen, Qfullheight) && height == FRAME_TEXT_HEIGHT (f))
    {
      frame_size_history_add
	(f, Qxg_frame_set_char_size_2, width, height,
	 list2 (make_number (gwidth), make_number (totalwidth)));

      gtk_window_resize (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			 totalwidth, gheight);
    }
  else
    {
      frame_size_history_add
	(f, Qxg_frame_set_char_size_3, width, height,
	 list2 (make_number (totalwidth), make_number (totalheight)));

      gtk_window_resize (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			 totalwidth, totalheight);
      fullscreen = Qnil;
    }

  SET_FRAME_GARBAGED (f);
  cancel_mouse_face (f);

  /* We can not call change_frame_size for a mapped frame,
     we can not set pixel width/height either.  The window manager may
     override our resize request, XMonad does this all the time.
     The best we can do is try to sync, so lisp code sees the updated
     size as fast as possible.
     For unmapped windows, we can set rows/cols.  When
     the frame is mapped again we will (hopefully) get the correct size.  */
  if (FRAME_VISIBLE_P (f))
    {
      /* Must call this to flush out events */
      (void)gtk_events_pending ();
      gdk_flush ();
      x_wait_for_event (f, ConfigureNotify);

      if (!NILP (fullscreen))
	/* Try to restore fullscreen state.  */
	{
	  store_frame_param (f, Qfullscreen, fullscreen);
	  x_set_fullscreen (f, fullscreen, fullscreen);
	}
    }
  else
    adjust_frame_size (f, width, height, 5, 0, Qxg_frame_set_char_size);

}

/* Handle height/width changes (i.e. add/remove/move menu/toolbar).
   The policy is to keep the number of editable lines.  */

#if 0
static void
xg_height_or_width_changed (struct frame *f)
{
  gtk_window_resize (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
                     FRAME_TOTAL_PIXEL_WIDTH (f),
                     FRAME_TOTAL_PIXEL_HEIGHT (f));
  f->output_data.x->hint_flags = 0;
  x_wm_set_size_hint (f, 0, 0);
}
#endif

/* Convert an X Window WSESC on display DPY to its corresponding GtkWidget.
   Must be done like this, because GtkWidget:s can have "hidden"
   X Window that aren't accessible.

   Return 0 if no widget match WDESC.  */

GtkWidget *
xg_win_to_widget (Display *dpy, Window wdesc)
{
  gpointer gdkwin;
  GtkWidget *gwdesc = 0;

  block_input ();

  gdkwin = gdk_x11_window_lookup_for_display (gdk_x11_lookup_xdisplay (dpy),
                                              wdesc);
  if (gdkwin)
    {
      GdkEvent event;
      event.any.window = gdkwin;
      event.any.type = GDK_NOTHING;
      gwdesc = gtk_get_event_widget (&event);
    }

  unblock_input ();
  return gwdesc;
}

/* Set the background of widget W to PIXEL.  */

static void
xg_set_widget_bg (struct frame *f, GtkWidget *w, unsigned long pixel)
{
#ifdef HAVE_GTK3
  XColor xbg;
  xbg.pixel = pixel;
  if (XQueryColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), &xbg))
    {
      const char format[] = "* { background-color: #%02x%02x%02x; }";
      /* The format is always longer than the resulting string.  */
      char buffer[sizeof format];
      int n = snprintf(buffer, sizeof buffer, format,
                       xbg.red >> 8, xbg.green >> 8, xbg.blue >> 8);
      eassert (n > 0);
      eassert (n < sizeof buffer);
      GtkCssProvider *provider = gtk_css_provider_new ();
      gtk_css_provider_load_from_data (provider, buffer, -1, NULL);
      gtk_style_context_add_provider (gtk_widget_get_style_context(w),
                                      GTK_STYLE_PROVIDER (provider),
                                      GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
      g_clear_object (&provider);
    }
#else
  GdkColor bg;
  GdkColormap *map = gtk_widget_get_colormap (w);
  gdk_colormap_query_color (map, pixel, &bg);
  gtk_widget_modify_bg (FRAME_GTK_WIDGET (f), GTK_STATE_NORMAL, &bg);
#endif
}

/* Callback called when the gtk theme changes.
   We notify lisp code so it can fix faces used for region for example.  */

static void
style_changed_cb (GObject *go,
                  GParamSpec *spec,
                  gpointer user_data)
{
  struct input_event event;
  GdkDisplay *gdpy = user_data;
  const char *display_name = gdk_display_get_name (gdpy);
  Display *dpy = GDK_DISPLAY_XDISPLAY (gdpy);

  EVENT_INIT (event);
  event.kind = CONFIG_CHANGED_EVENT;
  event.frame_or_window = build_string (display_name);
  /* Theme doesn't change often, so intern is called seldom.  */
  event.arg = intern ("theme-name");
  kbd_buffer_store_event (&event);

  update_theme_scrollbar_width ();
  update_theme_scrollbar_height ();

  /* If scroll bar width changed, we need set the new size on all frames
     on this display.  */
  if (dpy)
    {
      Lisp_Object rest, frame;
      FOR_EACH_FRAME (rest, frame)
        {
          struct frame *f = XFRAME (frame);
          if (FRAME_LIVE_P (f)
              && FRAME_X_P (f)
              && FRAME_X_DISPLAY (f) == dpy)
            {
              x_set_scroll_bar_default_width (f);
              x_set_scroll_bar_default_height (f);
              xg_frame_set_char_size (f, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f));
            }
        }
    }
}

/* Called when a delete-event occurs on WIDGET.  */

static gboolean
delete_cb (GtkWidget *widget,
           GdkEvent  *event,
           gpointer user_data)
{
  return TRUE;
}

/* Create and set up the GTK widgets for frame F.
   Return true if creation succeeded.  */

bool
xg_create_frame_widgets (struct frame *f)
{
  GtkWidget *wtop;
  GtkWidget *wvbox, *whbox;
  GtkWidget *wfixed;
#ifndef HAVE_GTK3
  GtkRcStyle *style;
#endif
  char *title = 0;

  block_input ();

  if (FRAME_X_EMBEDDED_P (f))
    {
      GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (FRAME_X_DISPLAY (f));
      wtop = gtk_plug_new_for_display (gdpy, f->output_data.x->parent_desc);
    }
  else
    wtop = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  /* gtk_window_set_has_resize_grip is a Gtk+ 3.0 function but Ubuntu
     has backported it to Gtk+ 2.0 and they add the resize grip for
     Gtk+ 2.0 applications also.  But it has a bug that makes Emacs loop
     forever, so disable the grip.  */
#if (! GTK_CHECK_VERSION (3, 0, 0) \
     && defined HAVE_GTK_WINDOW_SET_HAS_RESIZE_GRIP)
  gtk_window_set_has_resize_grip (GTK_WINDOW (wtop), FALSE);
#endif

  xg_set_screen (wtop, f);

  wvbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  whbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
  gtk_box_set_homogeneous (GTK_BOX (wvbox), FALSE);
  gtk_box_set_homogeneous (GTK_BOX (whbox), FALSE);

#ifdef HAVE_GTK3
  wfixed = emacs_fixed_new (f);
#else
  wfixed = gtk_fixed_new ();
#endif

  if (! wtop || ! wvbox || ! whbox || ! wfixed)
    {
      if (wtop) gtk_widget_destroy (wtop);
      if (wvbox) gtk_widget_destroy (wvbox);
      if (whbox) gtk_widget_destroy (whbox);
      if (wfixed) gtk_widget_destroy (wfixed);

      unblock_input ();
      return 0;
    }

  /* Use same names as the Xt port does.  I.e. Emacs.pane.emacs by default */
  gtk_widget_set_name (wtop, EMACS_CLASS);
  gtk_widget_set_name (wvbox, "pane");
  gtk_widget_set_name (wfixed, SSDATA (Vx_resource_name));

  /* If this frame has a title or name, set it in the title bar.  */
  if (! NILP (f->title))
    title = SSDATA (ENCODE_UTF_8 (f->title));
  else if (! NILP (f->name))
    title = SSDATA (ENCODE_UTF_8 (f->name));

  if (title)
    gtk_window_set_title (GTK_WINDOW (wtop), title);

  if (FRAME_UNDECORATED (f))
    {
      gtk_window_set_decorated (GTK_WINDOW (wtop), FALSE);
      store_frame_param (f, Qundecorated, Qt);
    }

  FRAME_GTK_OUTER_WIDGET (f) = wtop;
  FRAME_GTK_WIDGET (f) = wfixed;
  f->output_data.x->vbox_widget = wvbox;
  f->output_data.x->hbox_widget = whbox;

  gtk_widget_set_has_window (wfixed, TRUE);

  gtk_container_add (GTK_CONTAINER (wtop), wvbox);
  gtk_box_pack_start (GTK_BOX (wvbox), whbox, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (whbox), wfixed, TRUE, TRUE, 0);

  if (FRAME_EXTERNAL_TOOL_BAR (f))
    update_frame_tool_bar (f);

  /* We don't want this widget double buffered, because we draw on it
     with regular X drawing primitives, so from a GTK/GDK point of
     view, the widget is totally blank.  When an expose comes, this
     will make the widget blank, and then Emacs redraws it.  This flickers
     a lot, so we turn off double buffering.
     FIXME: gtk_widget_set_double_buffered is deprecated and might stop
     working in the future.  We need to migrate away from combining
     X and GTK+ drawing to a pure GTK+ build.  */
  gtk_widget_set_double_buffered (wfixed, FALSE);

#if ! GTK_CHECK_VERSION (3, 22, 0)
  gtk_window_set_wmclass (GTK_WINDOW (wtop),
                          SSDATA (Vx_resource_name),
                          SSDATA (Vx_resource_class));
#endif

  /* Add callback to do nothing on WM_DELETE_WINDOW.  The default in
     GTK is to destroy the widget.  We want Emacs to do that instead.  */
  g_signal_connect (G_OBJECT (wtop), "delete-event",
                    G_CALLBACK (delete_cb), f);

  /* Convert our geometry parameters into a geometry string
     and specify it.
     GTK will itself handle calculating the real position this way.  */
  xg_set_geometry (f);
  f->win_gravity
    = gtk_window_get_gravity (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));

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
  initial_set_up_x_back_buffer (f);

  /* Since GTK clears its window by filling with the background color,
     we must keep X and GTK background in sync.  */
  xg_set_widget_bg (f, wfixed, FRAME_BACKGROUND_PIXEL (f));

#ifndef HAVE_GTK3
  /* Also, do not let any background pixmap to be set, this looks very
     bad as Emacs overwrites the background pixmap with its own idea
     of background color.  */
  style = gtk_widget_get_modifier_style (wfixed);

  /* Must use g_strdup because gtk_widget_modify_style does g_free.  */
  style->bg_pixmap_name[GTK_STATE_NORMAL] = g_strdup ("<none>");
  gtk_widget_modify_style (wfixed, style);
#else
  gtk_widget_set_can_focus (wfixed, TRUE);
  gtk_window_set_resizable (GTK_WINDOW (wtop), TRUE);
#endif

  if (FRAME_OVERRIDE_REDIRECT (f))
    {
      GdkWindow *gwin = gtk_widget_get_window (wtop);

      if (gwin)
	gdk_window_set_override_redirect (gwin, TRUE);
    }

#ifdef USE_GTK_TOOLTIP
  /* Steal a tool tip window we can move ourselves.  */
  f->output_data.x->ttip_widget = 0;
  f->output_data.x->ttip_lbl = 0;
  f->output_data.x->ttip_window = 0;
  gtk_widget_set_tooltip_text (wtop, "Dummy text");
  g_signal_connect (wtop, "query-tooltip", G_CALLBACK (qttip_cb), f);
#endif

  {
    GdkScreen *screen = gtk_widget_get_screen (wtop);
    GtkSettings *gs = gtk_settings_get_for_screen (screen);
    /* Only connect this signal once per screen.  */
    if (! g_signal_handler_find (G_OBJECT (gs),
                                 G_SIGNAL_MATCH_FUNC,
                                 0, 0, 0,
                                 (gpointer) G_CALLBACK (style_changed_cb),
                                 0))
      {
        g_signal_connect (G_OBJECT (gs), "notify::gtk-theme-name",
                          G_CALLBACK (style_changed_cb),
                          gdk_screen_get_display (screen));
      }
  }

  unblock_input ();

  return 1;
}

void
xg_free_frame_widgets (struct frame *f)
{
  if (FRAME_GTK_OUTER_WIDGET (f))
    {
#ifdef USE_GTK_TOOLTIP
      struct x_output *x = f->output_data.x;
#endif
      struct xg_frame_tb_info *tbinfo
        = g_object_get_data (G_OBJECT (FRAME_GTK_OUTER_WIDGET (f)),
                             TB_INFO_KEY);
      if (tbinfo)
        xfree (tbinfo);

      /* x_free_frame_resources should have taken care of it */
      eassert (!FRAME_X_DOUBLE_BUFFERED_P (f));
      gtk_widget_destroy (FRAME_GTK_OUTER_WIDGET (f));
      FRAME_X_WINDOW (f) = 0; /* Set to avoid XDestroyWindow in xterm.c */
      FRAME_X_RAW_DRAWABLE (f) = 0;
      FRAME_GTK_OUTER_WIDGET (f) = 0;
#ifdef USE_GTK_TOOLTIP
      if (x->ttip_lbl)
        gtk_widget_destroy (x->ttip_lbl);
      if (x->ttip_widget)
        g_object_unref (G_OBJECT (x->ttip_widget));
#endif
    }
}

/* Set the normal size hints for the window manager, for frame F.
   FLAGS is the flags word to use--or 0 meaning preserve the flags
   that the window now has.
   If USER_POSITION, set the User Position
   flag (this is useful when FLAGS is 0).  */

void
x_wm_set_size_hint (struct frame *f, long int flags, bool user_position)
{
  /* Must use GTK routines here, otherwise GTK resets the size hints
     to its own defaults.  */
  GdkGeometry size_hints;
  gint hint_flags = 0;
  int base_width, base_height;
  int min_rows = 0, min_cols = 0;
  int win_gravity = f->win_gravity;
  Lisp_Object fs_state, frame;
  int scale = xg_get_scale (f);

  /* Don't set size hints during initialization; that apparently leads
     to a race condition.  See the thread at
     https://lists.gnu.org/r/emacs-devel/2008-10/msg00033.html  */
  if (NILP (Vafter_init_time)
      || !FRAME_GTK_OUTER_WIDGET (f)
      || FRAME_PARENT_FRAME (f))
    return;

  XSETFRAME (frame, f);
  fs_state = Fframe_parameter (frame, Qfullscreen);
  if ((EQ (fs_state, Qmaximized) || EQ (fs_state, Qfullboth)) &&
      (x_wm_supports (f, FRAME_DISPLAY_INFO (f)->Xatom_net_wm_state) ||
       x_wm_supports (f, FRAME_DISPLAY_INFO (f)->Xatom_net_wm_state_fullscreen)))
    {
      /* Don't set hints when maximized or fullscreen.  Apparently KWin and
         Gtk3 don't get along and the frame shrinks (!).
      */
      return;
    }

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
  size_hints.width_inc = frame_resize_pixelwise ? 1 : FRAME_COLUMN_WIDTH (f);
  size_hints.height_inc = frame_resize_pixelwise ? 1 : FRAME_LINE_HEIGHT (f);

  hint_flags |= GDK_HINT_BASE_SIZE;
  /* Use one row/col here so base_height/width does not become zero.
     Gtk+ and/or Unity on Ubuntu 12.04 can't handle it.
     Obviously this makes the row/col value displayed off by 1.  */
  base_width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, 1) + FRAME_TOOLBAR_WIDTH (f);
  base_height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, 1)
    + FRAME_MENUBAR_HEIGHT (f) + FRAME_TOOLBAR_HEIGHT (f);

  if (min_cols > 0) --min_cols; /* We used one col in base_width = ... 1); */
  if (min_rows > 0) --min_rows; /* We used one row in base_height = ... 1); */

  size_hints.base_width = base_width;
  size_hints.base_height = base_height;
  size_hints.min_width  = base_width + min_cols * FRAME_COLUMN_WIDTH (f);
  size_hints.min_height = base_height + min_rows * FRAME_LINE_HEIGHT (f);

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

  if (x_gtk_use_window_move)
    {
      if (flags & PPosition) hint_flags |= GDK_HINT_POS;
      if (flags & USPosition) hint_flags |= GDK_HINT_USER_POS;
      if (flags & USSize) hint_flags |= GDK_HINT_USER_SIZE;
    }

  if (user_position)
    {
      hint_flags &= ~GDK_HINT_POS;
      hint_flags |= GDK_HINT_USER_POS;
    }

  size_hints.base_width /= scale;
  size_hints.base_height /= scale;
  size_hints.width_inc /= scale;
  size_hints.height_inc /= scale;

  if (hint_flags != f->output_data.x->hint_flags
      || memcmp (&size_hints,
		 &f->output_data.x->size_hints,
		 sizeof (size_hints)) != 0)
    {
      block_input ();
      gtk_window_set_geometry_hints (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
                                     NULL, &size_hints, hint_flags);
      f->output_data.x->size_hints = size_hints;
      f->output_data.x->hint_flags = hint_flags;
      unblock_input ();
    }
}

/* Change background color of a frame.
   Since GTK uses the background color to clear the window, we must
   keep the GTK and X colors in sync.
   F is the frame to change,
   BG is the pixel value to change to.  */

void
xg_set_background_color (struct frame *f, unsigned long bg)
{
  if (FRAME_GTK_WIDGET (f))
    {
      block_input ();
      xg_set_widget_bg (f, FRAME_GTK_WIDGET (f), FRAME_BACKGROUND_PIXEL (f));

      Lisp_Object bar;
      for (bar = FRAME_SCROLL_BARS (f);
           !NILP (bar);
           bar = XSCROLL_BAR (bar)->next)
        {
          GtkWidget *scrollbar =
            xg_get_widget_from_map (XSCROLL_BAR (bar)->x_window);
          GtkWidget *webox = gtk_widget_get_parent (scrollbar);
          xg_set_widget_bg (f, webox, FRAME_BACKGROUND_PIXEL (f));
        }

      unblock_input ();
    }
}

/* Change the frame's decoration (title bar + resize borders).  This
   might not work with all window managers.  */
void
xg_set_undecorated (struct frame *f, Lisp_Object undecorated)
{
  if (FRAME_GTK_WIDGET (f))
    {
      block_input ();
      gtk_window_set_decorated (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
				NILP (undecorated) ? TRUE : FALSE);
      unblock_input ();
    }
}


/* Restack F1 below F2, above if ABOVE_FLAG is true.  This might not
   work with all window managers.  */
void
xg_frame_restack (struct frame *f1, struct frame *f2, bool above_flag)
{
#if GTK_CHECK_VERSION (2, 18, 0)
  block_input ();
  if (FRAME_GTK_OUTER_WIDGET (f1) && FRAME_GTK_OUTER_WIDGET (f2))
    {
      GdkWindow *gwin1 = gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (f1));
      GdkWindow *gwin2 = gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (f2));
      Lisp_Object frame1, frame2;

      XSETFRAME (frame1, f1);
      XSETFRAME (frame2, f2);

      gdk_window_restack (gwin1, gwin2, above_flag);
      x_sync (f1);
    }
  unblock_input ();
#endif
}


/* Don't show frame in taskbar, don't ALT-TAB to it.  */
void
xg_set_skip_taskbar (struct frame *f, Lisp_Object skip_taskbar)
{
  block_input ();
  if (FRAME_GTK_WIDGET (f))
    gdk_window_set_skip_taskbar_hint
      (gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (f)),
       NILP (skip_taskbar) ? FALSE : TRUE);
  unblock_input ();
}


/* Don't give frame focus.  */
void
xg_set_no_focus_on_map (struct frame *f, Lisp_Object no_focus_on_map)
{
  block_input ();
  if (FRAME_GTK_WIDGET (f))
    {
      GtkWindow *gwin = GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f));
      gboolean g_no_focus_on_map = NILP (no_focus_on_map) ? TRUE : FALSE;

      gtk_window_set_focus_on_map (gwin, g_no_focus_on_map);
    }
  unblock_input ();
}


void
xg_set_no_accept_focus (struct frame *f, Lisp_Object no_accept_focus)
{
  block_input ();
  if (FRAME_GTK_WIDGET (f))
    {
      GtkWindow *gwin = GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f));
      gboolean g_no_accept_focus = NILP (no_accept_focus) ? TRUE : FALSE;

      gtk_window_set_accept_focus (gwin, g_no_accept_focus);
    }
  unblock_input ();
}

void
xg_set_override_redirect (struct frame *f, Lisp_Object override_redirect)
{
  block_input ();

  if (FRAME_GTK_OUTER_WIDGET (f))
    {
      GdkWindow *gwin = gtk_widget_get_window (FRAME_GTK_OUTER_WIDGET (f));

      gdk_window_set_override_redirect (gwin, NILP (override_redirect) ? FALSE : TRUE);
    }

  unblock_input ();
}

/* Set the frame icon to ICON_PIXMAP/MASK.  This must be done with GTK
   functions so GTK does not overwrite the icon.  */

void
xg_set_frame_icon (struct frame *f, Pixmap icon_pixmap, Pixmap icon_mask)
{
  GdkPixbuf *gp = xg_get_pixbuf_from_pix_and_mask (f,
                                                   icon_pixmap,
                                                   icon_mask);
  if (gp)
    gtk_window_set_icon (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)), gp);
}



/***********************************************************************
                      Dialog functions
 ***********************************************************************/
/* Return the dialog title to use for a dialog of type KEY.
   This is the encoding used by lwlib.  We use the same for GTK.  */

static const char *
get_dialog_title (char key)
{
  const char *title = "";

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
   and we can always destroy the widget manually, regardless of how
   it was popped down (button press or WM_DELETE_WINDOW).
   W is the dialog widget.
   EVENT is the GdkEvent that represents WM_DELETE_WINDOW (not used).
   user_data is NULL (not used).

   Returns TRUE to end propagation of event.  */

static gboolean
dialog_delete_callback (GtkWidget *w, GdkEvent *event, gpointer user_data)
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
create_dialog (widget_value *wv,
               GCallback select_cb,
               GCallback deactivate_cb)
{
  const char *title = get_dialog_title (wv->name[0]);
  int total_buttons = wv->name[1] - '0';
  int right_buttons = wv->name[4] - '0';
  int left_buttons;
  int button_nr = 0;
  int button_spacing = 10;
  GtkWidget *wdialog = gtk_dialog_new ();
  GtkDialog *wd = GTK_DIALOG (wdialog);
  widget_value *item;
  GtkWidget *whbox_down;

  /* If the number of buttons is greater than 4, make two rows of buttons
     instead.  This looks better.  */
  bool make_two_rows = total_buttons > 4;

#if GTK_CHECK_VERSION (3, 12, 0)
  GtkBuilder *gbld = gtk_builder_new ();
  GObject *go = gtk_buildable_get_internal_child (GTK_BUILDABLE (wd),
                                                  gbld,
                                                  "action_area");
  GtkBox *cur_box = GTK_BOX (go);
  g_object_unref (G_OBJECT (gbld));
#else
  GtkBox *cur_box = GTK_BOX (gtk_dialog_get_action_area (wd));
#endif

  if (right_buttons == 0) right_buttons = total_buttons/2;
  left_buttons = total_buttons - right_buttons;

  gtk_window_set_title (GTK_WINDOW (wdialog), title);
  gtk_widget_set_name (wdialog, "emacs-dialog");


  if (make_two_rows)
    {
      GtkWidget *wvbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, button_spacing);
      GtkWidget *whbox_up = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
      gtk_box_set_homogeneous (GTK_BOX (wvbox), TRUE);
      gtk_box_set_homogeneous (GTK_BOX (whbox_up), FALSE);
      whbox_down = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
      gtk_box_set_homogeneous (GTK_BOX (whbox_down), FALSE);

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
          GtkBox *wvbox = GTK_BOX (gtk_dialog_get_content_area (wd));
          /* This is the text part of the dialog.  */
          w = gtk_label_new (utf8_label);
          gtk_box_pack_start (wvbox, gtk_label_new (""), FALSE, FALSE, 0);
          gtk_box_pack_start (wvbox, w, TRUE, TRUE, 0);
#if GTK_CHECK_VERSION (3, 14, 0)
          gtk_widget_set_halign (w, GTK_ALIGN_START);
          gtk_widget_set_valign (w, GTK_ALIGN_CENTER);
#else
          gtk_misc_set_alignment (GTK_MISC (w), 0.1, 0.5);
#endif
          /* Try to make dialog look better.  Must realize first so
             the widget can calculate the size it needs.  */
          gtk_widget_realize (w);
          gtk_widget_get_preferred_size (w, NULL, &req);
          gtk_box_set_spacing (wvbox, req.height);
	  if (item->value && strlen (item->value) > 0)
            button_spacing = 2*req.width/strlen (item->value);
          if (button_spacing < 10) button_spacing = 10;
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
            }
        }

     if (utf8_label)
       g_free (utf8_label);
    }

  return wdialog;
}

struct xg_dialog_data
{
  GMainLoop *loop;
  int response;
  GtkWidget *w;
  guint timerid;
};

/* Function that is called when the file or font dialogs pop down.
   W is the dialog widget, RESPONSE is the response code.
   USER_DATA is what we passed in to g_signal_connect.  */

static void
xg_dialog_response_cb (GtkDialog *w,
		       gint response,
		       gpointer user_data)
{
  struct xg_dialog_data *dd = user_data;
  dd->response = response;
  g_main_loop_quit (dd->loop);
}


/*  Destroy the dialog.  This makes it pop down.  */

static void
pop_down_dialog (void *arg)
{
  struct xg_dialog_data *dd = arg;

  block_input ();
  if (dd->w) gtk_widget_destroy (dd->w);
  if (dd->timerid != 0) g_source_remove (dd->timerid);

  g_main_loop_quit (dd->loop);
  g_main_loop_unref (dd->loop);

  unblock_input ();
}

/* If there are any emacs timers pending, add a timeout to main loop in DATA.
   Pass DATA as gpointer so we can use this as a callback.  */

static gboolean
xg_maybe_add_timer (gpointer data)
{
  struct xg_dialog_data *dd = data;
  struct timespec next_time = timer_check ();

  dd->timerid = 0;

  if (timespec_valid_p (next_time))
    {
      time_t s = next_time.tv_sec;
      int per_ms = TIMESPEC_RESOLUTION / 1000;
      int ms = (next_time.tv_nsec + per_ms - 1) / per_ms;
      if (s <= ((guint) -1 - ms) / 1000)
	dd->timerid = g_timeout_add (s * 1000 + ms, xg_maybe_add_timer, dd);
    }
  return FALSE;
}


/* Pops up a modal dialog W and waits for response.
   We don't use gtk_dialog_run because we want to process emacs timers.
   The dialog W is not destroyed when this function returns.  */

static int
xg_dialog_run (struct frame *f, GtkWidget *w)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  struct xg_dialog_data dd;

  xg_set_screen (w, f);
  gtk_window_set_transient_for (GTK_WINDOW (w),
                                GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
  gtk_window_set_destroy_with_parent (GTK_WINDOW (w), TRUE);
  gtk_window_set_modal (GTK_WINDOW (w), TRUE);

  dd.loop = g_main_loop_new (NULL, FALSE);
  dd.response = GTK_RESPONSE_CANCEL;
  dd.w = w;
  dd.timerid = 0;

  g_signal_connect (G_OBJECT (w),
                    "response",
                    G_CALLBACK (xg_dialog_response_cb),
                    &dd);
  /* Don't destroy the widget if closed by the window manager close button.  */
  g_signal_connect (G_OBJECT (w), "delete-event", G_CALLBACK (gtk_true), NULL);
  gtk_widget_show (w);

  record_unwind_protect_ptr (pop_down_dialog, &dd);

  (void) xg_maybe_add_timer (&dd);
  g_main_loop_run (dd.loop);

  dd.w = 0;
  unbind_to (count, Qnil);

  return dd.response;
}


/***********************************************************************
                      File dialog functions
 ***********************************************************************/
/* Return true if the old file selection dialog is being used.  */

bool
xg_uses_old_file_dialog (void)
{
#ifdef HAVE_GTK_FILE_SELECTION_NEW
  return x_gtk_use_old_file_dialog;
#else
  return 0;
#endif
}


typedef char * (*xg_get_file_func) (GtkWidget *);

/* Return the selected file for file chooser dialog W.
   The returned string must be free:d.  */

static char *
xg_get_file_name_from_chooser (GtkWidget *w)
{
  return gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (w));
}

/* Callback called when the "Show hidden files" toggle is pressed.
   WIDGET is the toggle widget, DATA is the file chooser dialog.  */

static void
xg_toggle_visibility_cb (GtkWidget *widget, gpointer data)
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
xg_toggle_notify_cb (GObject *gobject, GParamSpec *arg1, gpointer user_data)
{
  if (strcmp (arg1->name, "show-hidden") == 0)
    {
      GtkWidget *wtoggle = GTK_WIDGET (user_data);
      gboolean visible, toggle_on;

      g_object_get (G_OBJECT (gobject), "show-hidden", &visible, NULL);
      toggle_on = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (wtoggle));

      if (!!visible != !!toggle_on)
        {
          gpointer cb = (gpointer) G_CALLBACK (xg_toggle_visibility_cb);
          g_signal_handlers_block_by_func (G_OBJECT (wtoggle), cb, gobject);
          gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (wtoggle), visible);
          g_signal_handlers_unblock_by_func (G_OBJECT (wtoggle), cb, gobject);
        }
      x_gtk_show_hidden_files = visible;
    }
}

/* Read a file name from the user using a file chooser dialog.
   F is the current frame.
   PROMPT is a prompt to show to the user.  May not be NULL.
   DEFAULT_FILENAME is a default selection to be displayed.  May be NULL.
   If MUSTMATCH_P, the returned file name must be an existing
   file.  (Actually, this only has cosmetic effects, the user can
   still enter a non-existing file.)  *FUNC is set to a function that
   can be used to retrieve the selected file name from the returned widget.

   Returns the created widget.  */

static GtkWidget *
xg_get_file_with_chooser (struct frame *f,
			  char *prompt,
			  char *default_filename,
			  bool mustmatch_p, bool only_dir_p,
			  xg_get_file_func *func)
{
  char msgbuf[1024];

  GtkWidget *filewin, *wtoggle, *wbox;
  GtkWidget *wmessage UNINIT;
  GtkWindow *gwin = GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f));
  GtkFileChooserAction action = (mustmatch_p ?
                                 GTK_FILE_CHOOSER_ACTION_OPEN :
                                 GTK_FILE_CHOOSER_ACTION_SAVE);

  if (only_dir_p)
    action = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER;

  filewin = gtk_file_chooser_dialog_new (prompt, gwin, action,
                                         XG_TEXT_CANCEL, GTK_RESPONSE_CANCEL,
                                         (mustmatch_p || only_dir_p ?
                                          XG_TEXT_OPEN : XG_TEXT_OK),
                                         GTK_RESPONSE_OK,
                                         NULL);
  gtk_file_chooser_set_local_only (GTK_FILE_CHOOSER (filewin), TRUE);

  wbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  gtk_box_set_homogeneous (GTK_BOX (wbox), FALSE);
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
      char *z = msgbuf;
      /* Gtk+ 2.10 has the file name text entry box integrated in the dialog.
         Show the C-l help text only for versions < 2.10.  */
      if (gtk_check_version (2, 10, 0) && action != GTK_FILE_CHOOSER_ACTION_SAVE)
        z = stpcpy (z, "\nType C-l to display a file name text entry box.\n");
      strcpy (z, "\nIf you don't like this file selector, use the "
              "corresponding\nkey binding or customize "
              "use-file-dialog to turn it off.");

      wmessage = gtk_label_new (msgbuf);
      gtk_widget_show (wmessage);
    }

  gtk_box_pack_start (GTK_BOX (wbox), wtoggle, FALSE, FALSE, 0);
  if (x_gtk_file_dialog_help_text)
    gtk_box_pack_start (GTK_BOX (wbox), wmessage, FALSE, FALSE, 0);
  gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER (filewin), wbox);

  if (default_filename)
    {
      Lisp_Object file;
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
    }

  *func = xg_get_file_name_from_chooser;
  return filewin;
}

#ifdef HAVE_GTK_FILE_SELECTION_NEW

/* Return the selected file for file selector dialog W.
   The returned string must be free:d.  */

static char *
xg_get_file_name_from_selector (GtkWidget *w)
{
  GtkFileSelection *filesel = GTK_FILE_SELECTION (w);
  return xstrdup (gtk_file_selection_get_filename (filesel));
}

/* Create a file selection dialog.
   F is the current frame.
   PROMPT is a prompt to show to the user.  May not be NULL.
   DEFAULT_FILENAME is a default selection to be displayed.  May be NULL.
   If MUSTMATCH_P, the returned file name must be an existing
   file.  *FUNC is set to a function that can be used to retrieve the
   selected file name from the returned widget.

   Returns the created widget.  */

static GtkWidget *
xg_get_file_with_selection (struct frame *f,
                            char *prompt,
                            char *default_filename,
                            bool mustmatch_p, bool only_dir_p,
                            xg_get_file_func *func)
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
   If MUSTMATCH_P, the returned file name must be an existing
   file.

   Returns a file name or NULL if no file was selected.
   The returned string must be freed by the caller.  */

char *
xg_get_file_name (struct frame *f,
                  char *prompt,
                  char *default_filename,
                  bool mustmatch_p,
                  bool only_dir_p)
{
  GtkWidget *w = 0;
  char *fn = 0;
  int filesel_done = 0;
  xg_get_file_func func;

#ifdef HAVE_GTK_FILE_SELECTION_NEW

  if (xg_uses_old_file_dialog ())
    w = xg_get_file_with_selection (f, prompt, default_filename,
                                    mustmatch_p, only_dir_p, &func);
  else
    w = xg_get_file_with_chooser (f, prompt, default_filename,
                                  mustmatch_p, only_dir_p, &func);

#else /* not HAVE_GTK_FILE_SELECTION_NEW */
  w = xg_get_file_with_chooser (f, prompt, default_filename,
                                mustmatch_p, only_dir_p, &func);
#endif /* not HAVE_GTK_FILE_SELECTION_NEW */

  gtk_widget_set_name (w, "emacs-filedialog");

  filesel_done = xg_dialog_run (f, w);
  if (filesel_done == GTK_RESPONSE_OK)
    fn = (*func) (w);

  gtk_widget_destroy (w);
  return fn;
}

/***********************************************************************
                      GTK font chooser
 ***********************************************************************/

#ifdef HAVE_FREETYPE

#if USE_NEW_GTK_FONT_CHOOSER

#define XG_WEIGHT_TO_SYMBOL(w)			\
  (w <= PANGO_WEIGHT_THIN ? Qextra_light	\
   : w <= PANGO_WEIGHT_ULTRALIGHT ? Qlight	\
   : w <= PANGO_WEIGHT_LIGHT ? Qsemi_light	\
   : w < PANGO_WEIGHT_MEDIUM ? Qnormal		\
   : w <= PANGO_WEIGHT_SEMIBOLD ? Qsemi_bold	\
   : w <= PANGO_WEIGHT_BOLD ? Qbold		\
   : w <= PANGO_WEIGHT_HEAVY ? Qextra_bold	\
   : Qultra_bold)

#define XG_STYLE_TO_SYMBOL(s)			\
  (s == PANGO_STYLE_OBLIQUE ? Qoblique		\
   : s == PANGO_STYLE_ITALIC ? Qitalic		\
   : Qnormal)

#endif /* USE_NEW_GTK_FONT_CHOOSER */


static char *x_last_font_name;

/* Pop up a GTK font selector and return the name of the font the user
   selects, as a C string.  The returned font name follows GTK's own
   format:

   `FAMILY [VALUE1 VALUE2] SIZE'

   This can be parsed using font_parse_fcname in font.c.
   DEFAULT_NAME, if non-zero, is the default font name.  */

Lisp_Object
xg_get_font (struct frame *f, const char *default_name)
{
  GtkWidget *w;
  int done = 0;
  Lisp_Object font = Qnil;

  w = gtk_font_chooser_dialog_new
    ("Pick a font", GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));

  if (default_name)
    {
      /* Convert fontconfig names to Gtk names, i.e. remove - before
	 number */
      char *p = strrchr (default_name, '-');
      if (p)
        {
          char *ep = p+1;
          while (c_isdigit (*ep))
            ++ep;
          if (*ep == '\0') *p = ' ';
        }
    }
  else if (x_last_font_name)
    default_name = x_last_font_name;

  if (default_name)
    gtk_font_chooser_set_font (GTK_FONT_CHOOSER (w), default_name);

  gtk_widget_set_name (w, "emacs-fontdialog");
  done = xg_dialog_run (f, w);
  if (done == GTK_RESPONSE_OK)
    {
#if USE_NEW_GTK_FONT_CHOOSER
      /* Use the GTK3 font chooser.  */
      PangoFontDescription *desc
	= gtk_font_chooser_get_font_desc (GTK_FONT_CHOOSER (w));

      if (desc)
	{
	  const char *name   = pango_font_description_get_family (desc);
	  gint        size   = pango_font_description_get_size (desc);
	  PangoWeight weight = pango_font_description_get_weight (desc);
	  PangoStyle  style  = pango_font_description_get_style (desc);

#ifdef USE_CAIRO
#define FONT_TYPE_WANTED (Qftcr)
#else
#define FONT_TYPE_WANTED (Qxft)
#endif
	  font = CALLN (Ffont_spec,
			QCname, build_string (name),
			QCsize, make_float (pango_units_to_double (size)),
			QCweight, XG_WEIGHT_TO_SYMBOL (weight),
			QCslant, XG_STYLE_TO_SYMBOL (style),
			QCtype,
                        FONT_TYPE_WANTED);

	  pango_font_description_free (desc);
	  dupstring (&x_last_font_name, name);
	}

#else /* Use old font selector, which just returns the font name.  */

      char *font_name
	= gtk_font_selection_dialog_get_font_name (GTK_FONT_CHOOSER (w));

      if (font_name)
	{
	  font = build_string (font_name);
	  g_free (x_last_font_name);
	  x_last_font_name = font_name;
	}
#endif /* USE_NEW_GTK_FONT_CHOOSER */
    }

  gtk_widget_destroy (w);
  return font;
}
#endif /* HAVE_FREETYPE */



/***********************************************************************
	                Menu functions.
 ***********************************************************************/

/* The name of menu items that can be used for customization.  Since GTK
   RC files are very crude and primitive, we have to set this on all
   menu item names so a user can easily customize menu items.  */

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
make_cl_data (xg_menu_cb_data *cl_data, struct frame *f, GCallback highlight_cb)
{
  if (! cl_data)
    {
      cl_data = xmalloc (sizeof *cl_data);
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
update_cl_data (xg_menu_cb_data *cl_data,
                struct frame *f,
                GCallback highlight_cb)
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
unref_cl_data (xg_menu_cb_data *cl_data)
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
xg_mark_data (void)
{
  xg_list_node *iter;
  Lisp_Object rest, frame;

  for (iter = xg_menu_cb_list.next; iter; iter = iter->next)
    mark_object (((xg_menu_cb_data *) iter)->menu_bar_vector);

  for (iter = xg_menu_item_cb_list.next; iter; iter = iter->next)
    {
      xg_menu_item_cb_data *cb_data = (xg_menu_item_cb_data *) iter;

      if (! NILP (cb_data->help))
        mark_object (cb_data->help);
    }

  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);

      if (FRAME_X_P (f) && FRAME_GTK_OUTER_WIDGET (f))
        {
          struct xg_frame_tb_info *tbinfo
            = g_object_get_data (G_OBJECT (FRAME_GTK_OUTER_WIDGET (f)),
                                 TB_INFO_KEY);
          if (tbinfo)
            {
              mark_object (tbinfo->last_tool_bar);
              mark_object (tbinfo->style);
            }
        }
    }
}

/* Callback called when a menu item is destroyed.  Used to free data.
   W is the widget that is being destroyed (not used).
   CLIENT_DATA points to the xg_menu_item_cb_data associated with the W.  */

static void
menuitem_destroy_callback (GtkWidget *w, gpointer client_data)
{
  if (client_data)
    {
      xg_menu_item_cb_data *data = client_data;
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
menuitem_highlight_callback (GtkWidget *w,
                             GdkEventCrossing *event,
                             gpointer client_data)
{
  GdkEvent ev;
  GtkWidget *subwidget;
  xg_menu_item_cb_data *data;

  ev.crossing = *event;
  subwidget = gtk_get_event_widget (&ev);
  data = g_object_get_data (G_OBJECT (subwidget), XG_ITEM_DATA);
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
menu_destroy_callback (GtkWidget *w, gpointer client_data)
{
  unref_cl_data (client_data);
}

/* Make a GTK widget that contains both UTF8_LABEL and UTF8_KEY (both
   must be non-NULL) and can be inserted into a menu item.

   Returns the GtkHBox.  */

static GtkWidget *
make_widget_for_menu_item (const char *utf8_label, const char *utf8_key)
{
  GtkWidget *wlbl;
  GtkWidget *wkey;
  GtkWidget *wbox;

  wbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
  gtk_box_set_homogeneous (GTK_BOX (wbox), FALSE);
  wlbl = gtk_label_new (utf8_label);
  wkey = gtk_label_new (utf8_key);

#if GTK_CHECK_VERSION (3, 14, 0)
  gtk_widget_set_halign (wlbl, GTK_ALIGN_START);
  gtk_widget_set_valign (wlbl, GTK_ALIGN_CENTER);
  gtk_widget_set_halign (wkey, GTK_ALIGN_START);
  gtk_widget_set_valign (wkey, GTK_ALIGN_CENTER);
#else
  gtk_misc_set_alignment (GTK_MISC (wlbl), 0.0, 0.5);
  gtk_misc_set_alignment (GTK_MISC (wkey), 0.0, 0.5);
#endif
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
   but the macOS version doesn't either, so I guess that is OK.  */

static GtkWidget *
make_menu_item (const char *utf8_label,
                const char *utf8_key,
                widget_value *item,
                GSList **group)
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

/* Create a menu item widget, and connect the callbacks.
   ITEM describes the menu item.
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
xg_create_one_menuitem (widget_value *item,
                        struct frame *f,
                        GCallback select_cb,
                        GCallback highlight_cb,
                        xg_menu_cb_data *cl_data,
                        GSList **group)
{
  char *utf8_label;
  char *utf8_key;
  GtkWidget *w;
  xg_menu_item_cb_data *cb_data;

  utf8_label = get_utf8_string (item->name);
  utf8_key = get_utf8_string (item->key);

  w = make_menu_item (utf8_label, utf8_key, item, group);

  if (utf8_label) g_free (utf8_label);
  if (utf8_key) g_free (utf8_key);

  cb_data = xmalloc (sizeof *cb_data);

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

/* Create a full menu tree specified by DATA.
   F is the frame the created menu belongs to.
   SELECT_CB is the callback to use when a menu item is selected.
   DEACTIVATE_CB is the callback to use when a sub menu is not shown anymore.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.
   If POP_UP_P, create a popup menu.
   If MENU_BAR_P, create a menu bar.
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
create_menus (widget_value *data,
              struct frame *f,
              GCallback select_cb,
              GCallback deactivate_cb,
              GCallback highlight_cb,
              bool pop_up_p,
              bool menu_bar_p,
              GtkWidget *topmenu,
              xg_menu_cb_data *cl_data,
              const char *name)
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
      else
        {
          wmenu = gtk_menu_bar_new ();
          /* Set width of menu bar to a small value so it doesn't enlarge
             a small initial frame size.  The width will be set to the
             width of the frame later on when it is added to a container.
             height -1: Natural height.  */
          gtk_widget_set_size_request (wmenu, 1, -1);
        }

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
    }

  for (item = data; item; item = item->next)
    {
      GtkWidget *w;

      if (pop_up_p && !item->contents && !item->call_data
          && !menu_separator_name_p (item->name))
        {
          char *utf8_label;
          /* A title for a popup.  We do the same as GTK does when
             creating titles, but it does not look good.  */
          group = NULL;
          utf8_label = get_utf8_string (item->name);

          w = gtk_menu_item_new_with_label (utf8_label);
          gtk_widget_set_sensitive (w, FALSE);
          if (utf8_label) g_free (utf8_label);
        }
      else if (menu_separator_name_p (item->name))
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
xg_create_widget (const char *type, const char *name, struct frame *f,
		  widget_value *val, GCallback select_cb,
		  GCallback deactivate_cb, GCallback highlight_cb)
{
  GtkWidget *w = 0;
  bool menu_bar_p = strcmp (type, "menubar") == 0;
  bool pop_up_p = strcmp (type, "popup") == 0;

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
                        0,
                        0,
                        name);

      /* Set the cursor to an arrow for popup menus when they are mapped.
         This is done by default for menu bar menus.  */
      if (pop_up_p)
        {
          /* Must realize so the GdkWindow inside the widget is created.  */
          gtk_widget_realize (w);
          xg_set_cursor (w, FRAME_DISPLAY_INFO (f)->xg_cursor);
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
xg_get_menu_item_label (GtkMenuItem *witem)
{
  GtkLabel *wlabel = GTK_LABEL (XG_BIN_CHILD (witem));
  return gtk_label_get_label (wlabel);
}

/* Return true if the menu item WITEM has the text LABEL.  */

static bool
xg_item_label_same_p (GtkMenuItem *witem, const char *label)
{
  bool is_same = 0;
  char *utf8_label = get_utf8_string (label);
  const char *old_label = witem ? xg_get_menu_item_label (witem) : 0;

  if (! old_label && ! utf8_label)
    is_same = 1;
  else if (old_label && utf8_label)
    is_same = strcmp (utf8_label, old_label) == 0;

  if (utf8_label) g_free (utf8_label);

  return is_same;
}

/* Destroy widgets in LIST.  */

static void
xg_destroy_widgets (GList *list)
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
xg_update_menubar (GtkWidget *menubar,
		   struct frame *f,
		   GList **list,
		   GList *iter,
		   int pos,
		   widget_value *val,
		   GCallback select_cb,
		   GCallback deactivate_cb,
		   GCallback highlight_cb,
		   xg_menu_cb_data *cl_data)
{
  if (! iter && ! val)
    return;
  else if (iter && ! val)
    {
      /* Item(s) have been removed.  Remove all remaining items.  */
      xg_destroy_widgets (iter);

      /* Add a blank entry so the menubar doesn't collapse to nothing. */
      gtk_menu_shell_insert (GTK_MENU_SHELL (menubar),
                             gtk_menu_item_new_with_label (""),
                             0);
      /* All updated.  */
      val = 0;
      iter = 0;
    }
  else if (! iter && val)
    {
      /* Item(s) added.  Add all new items in one call.  */
      create_menus (val, f, select_cb, deactivate_cb, highlight_cb,
                    0, 1, menubar, cl_data, 0);

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
      bool val_in_menubar = 0;
      bool iter_in_new_menubar = 0;
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

          g_object_ref (G_OBJECT (witem));
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
          GtkLabel *wlabel = GTK_LABEL (XG_BIN_CHILD (witem));
          char *utf8_label = get_utf8_string (val->name);

          /* GTK menu items don't notice when their labels have been
             changed from underneath them, so we have to explicitly
             use g_object_notify to tell listeners (e.g., a GMenuModel
             bridge that might be loaded) that the item's label has
             changed.  */
          gtk_label_set_text (wlabel, utf8_label);
#if GTK_CHECK_VERSION (2, 16, 0)
          g_object_notify (G_OBJECT (witem), "label");
#endif
          if (utf8_label) g_free (utf8_label);
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
          GSList *group = 0;
          GtkWidget *w = xg_create_one_menuitem (val,
                                                 f,
                                                 select_cb,
                                                 highlight_cb,
                                                 cl_data,
                                                 &group);

          /* Create a possibly empty submenu for menu bar items, since some
             themes don't highlight items correctly without it. */
          GtkWidget *submenu = create_menus (NULL, f,
                                             select_cb, deactivate_cb,
                                             highlight_cb,
                                             0, 0, 0, cl_data, 0);

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

          g_object_ref (G_OBJECT (witem2));
          gtk_container_remove (GTK_CONTAINER (menubar), GTK_WIDGET (witem2));
          gtk_menu_shell_insert (GTK_MENU_SHELL (menubar),
                                 GTK_WIDGET (witem2), pos);
          g_object_unref (G_OBJECT (witem2));

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
                     select_cb, deactivate_cb, highlight_cb, cl_data);
}

/* Update the menu item W so it corresponds to VAL.
   SELECT_CB is the callback to use when a menu item is selected.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.
   CL_DATA is the data to set in the widget for menu invocation.  */

static void
xg_update_menu_item (widget_value *val,
                     GtkWidget *w,
                     GCallback select_cb,
                     GCallback highlight_cb,
                     xg_menu_cb_data *cl_data)
{
  GtkWidget *wchild;
  GtkLabel *wlbl = 0;
  GtkLabel *wkey = 0;
  char *utf8_label;
  char *utf8_key;
  const char *old_label = 0;
  const char *old_key = 0;
  xg_menu_item_cb_data *cb_data;
  bool label_changed = false;

  wchild = XG_BIN_CHILD (w);
  utf8_label = get_utf8_string (val->name);
  utf8_key = get_utf8_string (val->key);

  /* See if W is a menu item with a key.  See make_menu_item above.  */
  if (GTK_IS_BOX (wchild))
    {
      GList *list = gtk_container_get_children (GTK_CONTAINER (wchild));

      wlbl = GTK_LABEL (list->data);
      wkey = GTK_LABEL (list->next->data);
      g_list_free (list);

      if (! utf8_key)
        {
          /* Remove the key and keep just the label.  */
          g_object_ref (G_OBJECT (wlbl));
          gtk_container_remove (GTK_CONTAINER (w), wchild);
          gtk_container_add (GTK_CONTAINER (w), GTK_WIDGET (wlbl));
          g_object_unref (G_OBJECT (wlbl));
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
    {
      label_changed = true;
      gtk_label_set_text (wkey, utf8_key);
    }

  if (! old_label || strcmp (utf8_label, old_label) != 0)
    {
      label_changed = true;
      gtk_label_set_text (wlbl, utf8_label);
    }

  if (utf8_key) g_free (utf8_key);
  if (utf8_label) g_free (utf8_label);

  if (! val->enabled && gtk_widget_get_sensitive (w))
    gtk_widget_set_sensitive (w, FALSE);
  else if (val->enabled && ! gtk_widget_get_sensitive (w))
    gtk_widget_set_sensitive (w, TRUE);

  cb_data = g_object_get_data (G_OBJECT (w), XG_ITEM_DATA);
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

#if GTK_CHECK_VERSION (2, 16, 0)
  if (label_changed) /* See comment in xg_update_menubar.  */
    g_object_notify (G_OBJECT (w), "label");
#endif
}

/* Update the toggle menu item W so it corresponds to VAL.  */

static void
xg_update_toggle_item (widget_value *val, GtkWidget *w)
{
  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (w), val->selected);
}

/* Update the radio menu item W so it corresponds to VAL.  */

static void
xg_update_radio_item (widget_value *val, GtkWidget *w)
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
xg_update_submenu (GtkWidget *submenu,
		   struct frame *f,
		   widget_value *val,
		   GCallback select_cb,
		   GCallback deactivate_cb,
		   GCallback highlight_cb,
		   xg_menu_cb_data *cl_data)
{
  GtkWidget *newsub = submenu;
  GList *list = 0;
  GList *iter;
  widget_value *cur;
  GList *first_radio = 0;

  if (submenu)
    list = gtk_container_get_children (GTK_CONTAINER (submenu));

  for (cur = val, iter = list;
       cur && iter;
       iter = g_list_next (iter), cur = cur->next)
  {
    GtkWidget *w = GTK_WIDGET (iter->data);

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
        if (! menu_separator_name_p (cur->name))
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
            menu_separator_name_p (cur->name))
          break;

        xg_update_menu_item (cur, w, select_cb, highlight_cb, cl_data);

        sub = gtk_menu_item_get_submenu (witem);
        if (sub && ! cur->contents)
          {
            /* Not a submenu anymore.  */
            g_object_ref (G_OBJECT (sub));
            remove_submenu (witem);
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

  /* Remove widgets from first structural change.  */
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
   If DEEP_P, rebuild all but the top level menu names in
   the MENUBAR.  If DEEP_P is zero, just rebuild the names in the menubar.
   SELECT_CB is the callback to use when a menu item is selected.
   DEACTIVATE_CB is the callback to use when a sub menu is not shown anymore.
   HIGHLIGHT_CB is the callback to call when entering/leaving menu items.  */

void
xg_modify_menubar_widgets (GtkWidget *menubar, struct frame *f,
			   widget_value *val, bool deep_p,
                           GCallback select_cb, GCallback deactivate_cb,
			   GCallback highlight_cb)
{
  xg_menu_cb_data *cl_data;
  GList *list = gtk_container_get_children (GTK_CONTAINER (menubar));

  if (! list) return;

  cl_data = g_object_get_data (G_OBJECT (menubar), XG_FRAME_DATA);

  xg_update_menubar (menubar, f, &list, list, 0, val->contents,
                     select_cb, deactivate_cb, highlight_cb, cl_data);

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
          GtkMenuItem *witem = 0;

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
          if (newsub != sub && witem != 0)
            {
              xg_set_screen (newsub, f);
              gtk_menu_item_set_submenu (witem, newsub);
            }
        }
    }

  g_list_free (list);
  gtk_widget_show_all (menubar);
}

/* Callback called when the menu bar W is mapped.
   Used to find the height of the menu bar if we didn't get it
   after showing the widget.  */

static void
menubar_map_cb (GtkWidget *w, gpointer user_data)
{
  GtkRequisition req;
  struct frame *f = user_data;
  gtk_widget_get_preferred_size (w, NULL, &req);
  if (FRAME_MENUBAR_HEIGHT (f) != req.height)
    {
      FRAME_MENUBAR_HEIGHT (f) = req.height;
      adjust_frame_size (f, -1, -1, 2, 0, Qmenu_bar_lines);
    }
}

/* Recompute all the widgets of frame F, when the menu bar has been
   changed.  */

void
xg_update_frame_menubar (struct frame *f)
{
  struct x_output *x = f->output_data.x;
  GtkRequisition req;

  if (!x->menubar_widget || gtk_widget_get_mapped (x->menubar_widget))
    return;

  if (x->menubar_widget && gtk_widget_get_parent (x->menubar_widget))
    return; /* Already done this, happens for frames created invisible.  */

  block_input ();

  gtk_box_pack_start (GTK_BOX (x->vbox_widget), x->menubar_widget,
                      FALSE, FALSE, 0);
  gtk_box_reorder_child (GTK_BOX (x->vbox_widget), x->menubar_widget, 0);

  g_signal_connect (x->menubar_widget, "map", G_CALLBACK (menubar_map_cb), f);
  gtk_widget_show_all (x->menubar_widget);
  gtk_widget_get_preferred_size (x->menubar_widget, NULL, &req);

  if (FRAME_MENUBAR_HEIGHT (f) != req.height)
    {
      FRAME_MENUBAR_HEIGHT (f) = req.height;
      adjust_frame_size (f, -1, -1, 2, 0, Qmenu_bar_lines);
    }
  unblock_input ();
}

/* Get rid of the menu bar of frame F, and free its storage.
   This is used when deleting a frame, and when turning off the menu bar.  */

void
free_frame_menubar (struct frame *f)
{
  struct x_output *x = f->output_data.x;

  if (x->menubar_widget)
    {
      block_input ();

      gtk_container_remove (GTK_CONTAINER (x->vbox_widget), x->menubar_widget);
       /* The menubar and its children shall be deleted when removed from
          the container.  */
      x->menubar_widget = 0;
      FRAME_MENUBAR_HEIGHT (f) = 0;
      adjust_frame_size (f, -1, -1, 2, 0, Qmenu_bar_lines);
      unblock_input ();
    }
}

bool
xg_event_is_for_menubar (struct frame *f, const XEvent *event)
{
  struct x_output *x = f->output_data.x;
  GList *iter;
  GdkRectangle rec;
  GList *list;
  GdkDisplay *gdpy;
  GdkWindow *gw;
  GdkEvent gevent;
  GtkWidget *gwdesc;

  if (! x->menubar_widget) return 0;

  if (! (event->xbutton.x >= 0
         && event->xbutton.x < FRAME_PIXEL_WIDTH (f)
         && event->xbutton.y >= 0
         && event->xbutton.y < FRAME_MENUBAR_HEIGHT (f)
         && event->xbutton.same_screen))
    return 0;

  gdpy = gdk_x11_lookup_xdisplay (FRAME_X_DISPLAY (f));
  gw = gdk_x11_window_lookup_for_display (gdpy, event->xbutton.window);
  if (! gw) return 0;
  gevent.any.window = gw;
  gevent.any.type = GDK_NOTHING;
  gwdesc = gtk_get_event_widget (&gevent);
  if (! gwdesc) return 0;
  if (! GTK_IS_MENU_BAR (gwdesc)
      && ! GTK_IS_MENU_ITEM (gwdesc)
      && ! gtk_widget_is_ancestor (x->menubar_widget, gwdesc))
    return 0;

  list = gtk_container_get_children (GTK_CONTAINER (x->menubar_widget));
  if (! list) return 0;
  rec.x = event->xbutton.x;
  rec.y = event->xbutton.y;
  rec.width = 1;
  rec.height = 1;

  for (iter = list ; iter; iter = g_list_next (iter))
    {
      GtkWidget *w = GTK_WIDGET (iter->data);
      if (gtk_widget_get_mapped (w) && gtk_widget_intersect (w, &rec, NULL))
        break;
    }
  g_list_free (list);
  return iter != 0;
}



/***********************************************************************
                      Scroll bar functions
 ***********************************************************************/


/* Setting scroll bar values invokes the callback.  Use this variable
   to indicate that callback should do nothing.  */

bool xg_ignore_gtk_scrollbar;

/* Width and height of scroll bars for the current theme.  */
static int scroll_bar_width_for_theme;
static int scroll_bar_height_for_theme;

/* Xlib's `Window' fits in 32 bits.  But we want to store pointers, and they
   may be larger than 32 bits.  Keep a mapping from integer index to widget
   pointers to get around the 32 bit limitation.  */

static struct
{
  GtkWidget **widgets;
  ptrdiff_t max_size;
  ptrdiff_t used;
} id_to_widget;

/* Grow this much every time we need to allocate more  */

#define ID_TO_WIDGET_INCR  32

/* Store the widget pointer W in id_to_widget and return the integer index.  */

static ptrdiff_t
xg_store_widget_in_map (GtkWidget *w)
{
  ptrdiff_t i;

  if (id_to_widget.max_size == id_to_widget.used)
    {
      ptrdiff_t new_size;
      if (TYPE_MAXIMUM (Window) - ID_TO_WIDGET_INCR < id_to_widget.max_size)
	memory_full (SIZE_MAX);

      new_size = id_to_widget.max_size + ID_TO_WIDGET_INCR;
      id_to_widget.widgets = xnrealloc (id_to_widget.widgets,
					new_size, sizeof (GtkWidget *));

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
  emacs_abort ();
}

/* Remove pointer at IDX from id_to_widget.
   Called when scroll bar is destroyed.  */

static void
xg_remove_widget_from_map (ptrdiff_t idx)
{
  if (idx < id_to_widget.max_size && id_to_widget.widgets[idx] != 0)
    {
      id_to_widget.widgets[idx] = 0;
      --id_to_widget.used;
    }
}

/* Get the widget pointer at IDX from id_to_widget. */

static GtkWidget *
xg_get_widget_from_map (ptrdiff_t idx)
{
  if (idx < id_to_widget.max_size && id_to_widget.widgets[idx] != 0)
    return id_to_widget.widgets[idx];

  return 0;
}

static void
update_theme_scrollbar_width (void)
{
#ifdef HAVE_GTK3
  GtkAdjustment *vadj;
#else
  GtkObject *vadj;
#endif
  GtkWidget *wscroll;
  int w = 0, b = 0;

  vadj = gtk_adjustment_new (XG_SB_MIN, XG_SB_MIN, XG_SB_MAX, 0.1, 0.1, 0.1);
  wscroll = gtk_scrollbar_new (GTK_ORIENTATION_VERTICAL, GTK_ADJUSTMENT (vadj));
  g_object_ref_sink (G_OBJECT (wscroll));
  gtk_widget_style_get (wscroll, "slider-width", &w, "trough-border", &b, NULL);
  gtk_widget_destroy (wscroll);
  g_object_unref (G_OBJECT (wscroll));
  w += 2*b;
#ifndef HAVE_GTK3
  if (w < 16) w = 16;
#endif
  scroll_bar_width_for_theme = w;
}

static void
update_theme_scrollbar_height (void)
{
#ifdef HAVE_GTK3
  GtkAdjustment *hadj;
#else
  GtkObject *hadj;
#endif
  GtkWidget *wscroll;
  int w = 0, b = 0;

  hadj = gtk_adjustment_new (YG_SB_MIN, YG_SB_MIN, YG_SB_MAX, 0.1, 0.1, 0.1);
  wscroll = gtk_scrollbar_new (GTK_ORIENTATION_HORIZONTAL, GTK_ADJUSTMENT (hadj));
  g_object_ref_sink (G_OBJECT (wscroll));
  gtk_widget_style_get (wscroll, "slider-width", &w, "trough-border", &b, NULL);
  gtk_widget_destroy (wscroll);
  g_object_unref (G_OBJECT (wscroll));
  w += 2*b;
  if (w < 12) w = 12;
  scroll_bar_height_for_theme = w;
}

int
xg_get_default_scrollbar_width (struct frame *f)
{
  return scroll_bar_width_for_theme * xg_get_scale (f);
}

int
xg_get_default_scrollbar_height (struct frame *f)
{
  /* Apparently there's no default height for themes.  */
  return scroll_bar_width_for_theme * xg_get_scale (f);
}

/* Return the scrollbar id for X Window WID on display DPY.
   Return -1 if WID not in id_to_widget.  */

ptrdiff_t
xg_get_scroll_id_for_window (Display *dpy, Window wid)
{
  ptrdiff_t idx;
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
xg_gtk_scroll_destroy (GtkWidget *widget, gpointer data)
{
  intptr_t id = (intptr_t) data;
  xg_remove_widget_from_map (id);
}

static void
xg_finish_scroll_bar_creation (struct frame *f,
                               GtkWidget *wscroll,
                               struct scroll_bar *bar,
                               GCallback scroll_callback,
                               GCallback end_callback,
                               const char *scroll_bar_name)
{
  GtkWidget *webox = gtk_event_box_new ();

  gtk_widget_set_name (wscroll, scroll_bar_name);
#ifndef HAVE_GTK3
  gtk_range_set_update_policy (GTK_RANGE (wscroll), GTK_UPDATE_CONTINUOUS);
#endif
  g_object_set_data (G_OBJECT (wscroll), XG_FRAME_DATA, (gpointer)f);

  ptrdiff_t scroll_id = xg_store_widget_in_map (wscroll);

  g_signal_connect (G_OBJECT (wscroll),
                    "destroy",
                    G_CALLBACK (xg_gtk_scroll_destroy),
                    (gpointer) scroll_id);
  g_signal_connect (G_OBJECT (wscroll),
                    "change-value",
                    scroll_callback,
                    (gpointer) bar);
  g_signal_connect (G_OBJECT (wscroll),
                    "button-release-event",
                    end_callback,
                    (gpointer) bar);

  /* The scroll bar widget does not draw on a window of its own.  Instead
     it draws on the parent window, in this case the edit widget.  So
     whenever the edit widget is cleared, the scroll bar needs to redraw
     also, which causes flicker.  Put an event box between the edit widget
     and the scroll bar, so the scroll bar instead draws itself on the
     event box window.  */
  gtk_fixed_put (GTK_FIXED (f->output_data.x->edit_widget), webox, -1, -1);
  gtk_container_add (GTK_CONTAINER (webox), wscroll);

  xg_set_widget_bg (f, webox, FRAME_BACKGROUND_PIXEL (f));

  /* N.B. The event box doesn't become a real X11 window until we ask
     for its XID via GTK_WIDGET_TO_X_WIN.  If the event box is not a
     real X window, it and its scroll-bar child try to draw on the
     Emacs main window, which we draw over using Xlib.  */
  gtk_widget_realize (webox);
  GTK_WIDGET_TO_X_WIN (webox);

  /* Set the cursor to an arrow.  */
  xg_set_cursor (webox, FRAME_DISPLAY_INFO (f)->xg_cursor);

  bar->x_window = scroll_id;
}

/* Create a scroll bar widget for frame F.  Store the scroll bar
   in BAR.
   SCROLL_CALLBACK is the callback to invoke when the value of the
   bar changes.
   END_CALLBACK is the callback to invoke when scrolling ends.
   SCROLL_BAR_NAME is the name we use for the scroll bar.  Can be used
   to set resources for the widget.  */

void
xg_create_scroll_bar (struct frame *f,
                      struct scroll_bar *bar,
                      GCallback scroll_callback,
                      GCallback end_callback,
                      const char *scroll_bar_name)
{
  GtkWidget *wscroll;
#ifdef HAVE_GTK3
  GtkAdjustment *vadj;
#else
  GtkObject *vadj;
#endif

  /* Page, step increment values are not so important here, they
     will be corrected in x_set_toolkit_scroll_bar_thumb. */
  vadj = gtk_adjustment_new (XG_SB_MIN, XG_SB_MIN, XG_SB_MAX,
                             0.1, 0.1, 0.1);

  wscroll = gtk_scrollbar_new (GTK_ORIENTATION_VERTICAL, GTK_ADJUSTMENT (vadj));

  xg_finish_scroll_bar_creation (f, wscroll, bar, scroll_callback,
                                 end_callback, scroll_bar_name);
  bar->horizontal = 0;
}

/* Create a horizontal scroll bar widget for frame F.  Store the scroll
   bar in BAR.  SCROLL_CALLBACK is the callback to invoke when the value
   of the bar changes.  END_CALLBACK is the callback to invoke when
   scrolling ends.  SCROLL_BAR_NAME is the name we use for the scroll
   bar.  Can be used to set resources for the widget.  */

void
xg_create_horizontal_scroll_bar (struct frame *f,
				 struct scroll_bar *bar,
				 GCallback scroll_callback,
				 GCallback end_callback,
				 const char *scroll_bar_name)
{
  GtkWidget *wscroll;
#ifdef HAVE_GTK3
  GtkAdjustment *hadj;
#else
  GtkObject *hadj;
#endif

  /* Page, step increment values are not so important here, they
     will be corrected in x_set_toolkit_scroll_bar_thumb. */
  hadj = gtk_adjustment_new (YG_SB_MIN, YG_SB_MIN, YG_SB_MAX,
                             0.1, 0.1, 0.1);

  wscroll = gtk_scrollbar_new (GTK_ORIENTATION_HORIZONTAL, GTK_ADJUSTMENT (hadj));

  xg_finish_scroll_bar_creation (f, wscroll, bar, scroll_callback,
                                 end_callback, scroll_bar_name);
  bar->horizontal = 1;
}

/* Remove the scroll bar represented by SCROLLBAR_ID from the frame F.  */

void
xg_remove_scroll_bar (struct frame *f, ptrdiff_t scrollbar_id)
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
xg_update_scrollbar_pos (struct frame *f,
                         ptrdiff_t scrollbar_id,
                         int top,
                         int left,
                         int width,
                         int height)
{
  GtkWidget *wscroll = xg_get_widget_from_map (scrollbar_id);
  if (wscroll)
    {
      GtkWidget *wfixed = f->output_data.x->edit_widget;
      GtkWidget *wparent = gtk_widget_get_parent (wscroll);
      gint msl;
      int scale = xg_get_scale (f);

      top /= scale;
      left /= scale;
      height /= scale;
      left -= (scale - 1) * ((width / scale) >> 1);

      /* Clear out old position.  */
      int oldx = -1, oldy = -1, oldw, oldh;
      if (gtk_widget_get_parent (wparent) == wfixed)
        {
          gtk_container_child_get (GTK_CONTAINER (wfixed), wparent,
                                   "x", &oldx, "y", &oldy, NULL);
          gtk_widget_get_size_request (wscroll, &oldw, &oldh);
        }

      /* Move and resize to new values.  */
      gtk_fixed_move (GTK_FIXED (wfixed), wparent, left, top);
      gtk_widget_style_get (wscroll, "min-slider-length", &msl, NULL);
      bool hidden = height < msl;
      if (hidden)
        {
          /* No room.  Hide scroll bar as some themes output a warning if
             the height is less than the min size.  */
          gtk_widget_hide (wparent);
          gtk_widget_hide (wscroll);
        }
      else
        {
          gtk_widget_show_all (wparent);
          gtk_widget_set_size_request (wscroll, width, height);
        }
      if (oldx != -1 && oldw > 0 && oldh > 0)
        {
          /* Clear under old scroll bar position.  */
          oldw += (scale - 1) * oldw;
	  oldx -= (scale - 1) * oldw;
          x_clear_area (f, oldx, oldy, oldw, oldh);
        }

      if (!hidden)
	{
	  GtkWidget *scrollbar = xg_get_widget_from_map (scrollbar_id);
	  GtkWidget *webox = gtk_widget_get_parent (scrollbar);

	  /* Don't obscure any child frames.  */
	  XLowerWindow (FRAME_X_DISPLAY (f), GTK_WIDGET_TO_X_WIN (webox));
	}

      /* GTK does not redraw until the main loop is entered again, but
         if there are no X events pending we will not enter it.  So we sync
         here to get some events.  */

      x_sync (f);
      SET_FRAME_GARBAGED (f);
      cancel_mouse_face (f);
    }
}


/* Update the position of the horizontal scroll bar represented by SCROLLBAR_ID
   in frame F.
   TOP/LEFT are the new pixel positions where the bar shall appear.
   WIDTH, HEIGHT is the size in pixels the bar shall have.  */

void
xg_update_horizontal_scrollbar_pos (struct frame *f,
				    ptrdiff_t scrollbar_id,
				    int top,
				    int left,
				    int width,
				    int height)
{

  GtkWidget *wscroll = xg_get_widget_from_map (scrollbar_id);

  if (wscroll)
    {
      GtkWidget *wfixed = f->output_data.x->edit_widget;
      GtkWidget *wparent = gtk_widget_get_parent (wscroll);
      gint msl;

      /* Clear out old position.  */
      int oldx = -1, oldy = -1, oldw, oldh;
      if (gtk_widget_get_parent (wparent) == wfixed)
        {
          gtk_container_child_get (GTK_CONTAINER (wfixed), wparent,
                                   "x", &oldx, "y", &oldy, NULL);
          gtk_widget_get_size_request (wscroll, &oldw, &oldh);
        }

      /* Move and resize to new values.  */
      gtk_fixed_move (GTK_FIXED (wfixed), wparent, left, top);
      gtk_widget_style_get (wscroll, "min-slider-length", &msl, NULL);
      if (msl > width)
        {
          /* No room.  Hide scroll bar as some themes output a warning if
             the width is less than the min size.  */
          gtk_widget_hide (wparent);
          gtk_widget_hide (wscroll);
        }
      else
        {
          gtk_widget_show_all (wparent);
          gtk_widget_set_size_request (wscroll, width, height);
        }
      if (oldx != -1 && oldw > 0 && oldh > 0)
        /* Clear under old scroll bar position.  */
        x_clear_area (f, oldx, oldy, oldw, oldh);

      /* GTK does not redraw until the main loop is entered again, but
         if there are no X events pending we will not enter it.  So we sync
         here to get some events.  */

      {
	GtkWidget *scrollbar =
	  xg_get_widget_from_map (scrollbar_id);
	GtkWidget *webox = gtk_widget_get_parent (scrollbar);

	/* Don't obscure any child frames.  */
	XLowerWindow (FRAME_X_DISPLAY (f), GTK_WIDGET_TO_X_WIN (webox));
      }

      x_sync (f);
      SET_FRAME_GARBAGED (f);
      cancel_mouse_face (f);
    }
}


/* Get the current value of the range, truncated to an integer.  */

static int
int_gtk_range_get_value (GtkRange *range)
{
  return gtk_range_get_value (range);
}


/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

void
xg_set_toolkit_scroll_bar_thumb (struct scroll_bar *bar,
                                 int portion,
                                 int position,
                                 int whole)
{
  GtkWidget *wscroll = xg_get_widget_from_map (bar->x_window);

  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  if (wscroll && bar->dragging == -1)
    {
      GtkAdjustment *adj;
      gdouble shown;
      gdouble top;
      int size, value;
      int old_size;
      int new_step;
      bool changed = 0;

      adj = gtk_range_get_adjustment (GTK_RANGE (wscroll));

      if (scroll_bar_adjust_thumb_portion_p)
        {
          /* We do the same as for MOTIF in xterm.c, use 30 chars per
             line rather than the real portion value.  This makes the
             thumb less likely to resize and that looks better.  */
          portion = WINDOW_TOTAL_LINES (XWINDOW (bar->window)) * 30;

          /* When the thumb is at the bottom, position == whole.
             So we need to increase `whole' to make space for the thumb.  */
          whole += portion;
        }

      if (whole <= 0)
        top = 0, shown = 1;
      else
        {
          top = (gdouble) position / whole;
          shown = (gdouble) portion / whole;
        }

      size = clip_to_bounds (1, shown * XG_SB_RANGE, XG_SB_RANGE);
      value = clip_to_bounds (XG_SB_MIN, top * XG_SB_RANGE, XG_SB_MAX - size);

      /* Assume all lines are of equal size.  */
      new_step = size / max (1, FRAME_LINES (f));

      old_size = gtk_adjustment_get_page_size (adj);
      if (old_size != size)
	{
	  int old_step = gtk_adjustment_get_step_increment (adj);
	  if (old_step != new_step)
	    {
	      gtk_adjustment_set_page_size (adj, size);
	      gtk_adjustment_set_step_increment (adj, new_step);
	      /* Assume a page increment is about 95% of the page size  */
	      gtk_adjustment_set_page_increment (adj, size - size / 20);
	      changed = 1;
	    }
	}

      if (changed || int_gtk_range_get_value (GTK_RANGE (wscroll)) != value)
      {
        block_input ();

        /* gtk_range_set_value invokes the callback.  Set
           ignore_gtk_scrollbar to make the callback do nothing  */
        xg_ignore_gtk_scrollbar = 1;

        if (int_gtk_range_get_value (GTK_RANGE (wscroll)) != value)
          gtk_range_set_value (GTK_RANGE (wscroll), (gdouble)value);
#if ! GTK_CHECK_VERSION (3, 18, 0)
        else if (changed)
          gtk_adjustment_changed (adj);
#endif

        xg_ignore_gtk_scrollbar = 0;

        unblock_input ();
      }
    }
}

/* Set the thumb size and position of horizontal scroll bar BAR.  We are
   currently displaying PORTION out of a whole WHOLE, and our position
   POSITION.  */
void
xg_set_toolkit_horizontal_scroll_bar_thumb (struct scroll_bar *bar,
					    int portion,
					    int position,
					    int whole)
{
  GtkWidget *wscroll = xg_get_widget_from_map (bar->x_window);

  if (wscroll && bar->dragging == -1)
    {
      GtkAdjustment *adj;
      int lower = 0;
      int upper = max (whole - 1, 0);
      int pagesize = min (upper, max (portion, 0));
      int value = max (0, min (position, upper - pagesize));
      /* These should be set to something more <portion, whole>
	 related.  */
      int page_increment = 4;
      int step_increment = 1;

      block_input ();
      adj = gtk_range_get_adjustment (GTK_RANGE (wscroll));
      gtk_adjustment_configure (adj, (gdouble) value, (gdouble) lower,
				(gdouble) upper, (gdouble) step_increment,
				(gdouble) page_increment, (gdouble) pagesize);
#if ! GTK_CHECK_VERSION (3, 18, 0)
      gtk_adjustment_changed (adj);
#endif
      unblock_input ();
    }
}

/* Return true if EVENT is for a scroll bar in frame F.
   When the same X window is used for several Gtk+ widgets, we cannot
   say for sure based on the X window alone if an event is for the
   frame.  This function does additional checks.  */

bool
xg_event_is_for_scrollbar (struct frame *f, const XEvent *event)
{
  bool retval = 0;

  if (f && event->type == ButtonPress && event->xbutton.button < 4)
    {
      /* Check if press occurred outside the edit widget.  */
      GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (FRAME_X_DISPLAY (f));
      GdkWindow *gwin;
#ifdef HAVE_GTK3
#if GTK_CHECK_VERSION (3, 20, 0)
      GdkDevice *gdev
        = gdk_seat_get_pointer (gdk_display_get_default_seat (gdpy));
#else
      GdkDevice *gdev = gdk_device_manager_get_client_pointer
        (gdk_display_get_device_manager (gdpy));
#endif
      gwin = gdk_device_get_window_at_position (gdev, NULL, NULL);
#else
      gwin = gdk_display_get_window_at_pointer (gdpy, NULL, NULL);
#endif
      retval = gwin != gtk_widget_get_window (f->output_data.x->edit_widget);
    }
  else if (f
           && ((event->type == ButtonRelease && event->xbutton.button < 4)
               || event->type == MotionNotify))
    {
      /* If we are releasing or moving the scroll bar, it has the grab.  */
      GtkWidget *w = gtk_grab_get_current ();
      retval = w != 0 && GTK_IS_SCROLLBAR (w);
    }

  return retval;
}


/***********************************************************************
			       Printing
 ***********************************************************************/
#ifdef USE_CAIRO
static GtkPrintSettings *print_settings = NULL;
static GtkPageSetup *page_setup = NULL;

void
xg_page_setup_dialog (void)
{
  GtkPageSetup *new_page_setup = NULL;

  if (print_settings == NULL)
    print_settings = gtk_print_settings_new ();
  new_page_setup = gtk_print_run_page_setup_dialog (NULL, page_setup,
						    print_settings);
  if (page_setup)
    g_object_unref (page_setup);
  page_setup = new_page_setup;
}

Lisp_Object
xg_get_page_setup (void)
{
  Lisp_Object orientation_symbol;

  if (page_setup == NULL)
    page_setup = gtk_page_setup_new ();

  switch (gtk_page_setup_get_orientation (page_setup))
    {
    case GTK_PAGE_ORIENTATION_PORTRAIT:
      orientation_symbol = Qportrait;
      break;
    case GTK_PAGE_ORIENTATION_LANDSCAPE:
      orientation_symbol = Qlandscape;
      break;
    case GTK_PAGE_ORIENTATION_REVERSE_PORTRAIT:
      orientation_symbol = Qreverse_portrait;
      break;
    case GTK_PAGE_ORIENTATION_REVERSE_LANDSCAPE:
      orientation_symbol = Qreverse_landscape;
      break;
    default:
      eassume (false);
    }

  return listn (CONSTYPE_HEAP, 7,
		Fcons (Qorientation, orientation_symbol),
#define MAKE_FLOAT_PAGE_SETUP(f)  make_float (f (page_setup, GTK_UNIT_POINTS))
		Fcons (Qwidth,
		       MAKE_FLOAT_PAGE_SETUP (gtk_page_setup_get_page_width)),
		Fcons (Qheight,
		       MAKE_FLOAT_PAGE_SETUP (gtk_page_setup_get_page_height)),
		Fcons (Qleft_margin,
		       MAKE_FLOAT_PAGE_SETUP (gtk_page_setup_get_left_margin)),
		Fcons (Qright_margin,
		       MAKE_FLOAT_PAGE_SETUP (gtk_page_setup_get_right_margin)),
		Fcons (Qtop_margin,
		       MAKE_FLOAT_PAGE_SETUP (gtk_page_setup_get_top_margin)),
		Fcons (Qbottom_margin,
		       MAKE_FLOAT_PAGE_SETUP (gtk_page_setup_get_bottom_margin))
#undef MAKE_FLOAT_PAGE_SETUP
		);
}

static void
draw_page (GtkPrintOperation *operation, GtkPrintContext *context,
	   gint page_nr, gpointer user_data)
{
  Lisp_Object frames = *((Lisp_Object *) user_data);
  struct frame *f = XFRAME (Fnth (make_number (page_nr), frames));
  cairo_t *cr = gtk_print_context_get_cairo_context (context);

  x_cr_draw_frame (cr, f);
}

void
xg_print_frames_dialog (Lisp_Object frames)
{
  GtkPrintOperation *print;
  GtkPrintOperationResult res;

  print = gtk_print_operation_new ();
  if (print_settings != NULL)
    gtk_print_operation_set_print_settings (print, print_settings);
  if (page_setup != NULL)
    gtk_print_operation_set_default_page_setup (print, page_setup);
  gtk_print_operation_set_n_pages (print, XINT (Flength (frames)));
  g_signal_connect (print, "draw-page", G_CALLBACK (draw_page), &frames);
  res = gtk_print_operation_run (print, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                                 NULL, NULL);
  if (res == GTK_PRINT_OPERATION_RESULT_APPLY)
    {
      if (print_settings != NULL)
        g_object_unref (print_settings);
      print_settings =
	g_object_ref (gtk_print_operation_get_print_settings (print));
    }
  g_object_unref (print);
}

#endif	/* USE_CAIRO */



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

/* The key for the data we put in the GtkImage widgets.  The data is
   the stock name used by Emacs.  We use this to see if we need to update
   the GtkImage with a new image.  */
#define XG_TOOL_BAR_STOCK_NAME "emacs-tool-bar-stock-name"

/* As above, but this is used for named theme widgets, as opposed to
   stock items.  */
#define XG_TOOL_BAR_ICON_NAME "emacs-tool-bar-icon-name"

/* Callback function invoked when a tool bar item is pressed.
   W is the button widget in the tool bar that got pressed,
   CLIENT_DATA is an integer that is the index of the button in the
   tool bar.  0 is the first button.  */

static gboolean
xg_tool_bar_button_cb (GtkWidget *widget,
                       GdkEventButton *event,
                       gpointer user_data)
{
  intptr_t state = event->state;
  gpointer ptr = (gpointer) state;
  g_object_set_data (G_OBJECT (widget), XG_TOOL_BAR_LAST_MODIFIER, ptr);
  return FALSE;
}


/* Callback function invoked when a tool bar item is pressed.
   W is the button widget in the tool bar that got pressed,
   CLIENT_DATA is an integer that is the index of the button in the
   tool bar.  0 is the first button.  */

static void
xg_tool_bar_callback (GtkWidget *w, gpointer client_data)
{
  intptr_t idx = (intptr_t) client_data;
  gpointer gmod = g_object_get_data (G_OBJECT (w), XG_TOOL_BAR_LAST_MODIFIER);
  intptr_t mod = (intptr_t) gmod;

  struct frame *f = g_object_get_data (G_OBJECT (w), XG_FRAME_DATA);
  Lisp_Object key, frame;
  struct input_event event;
  EVENT_INIT (event);

  if (! f || ! f->n_tool_bar_items || NILP (f->tool_bar_items))
    return;

  idx *= TOOL_BAR_ITEM_NSLOTS;

  key = AREF (f->tool_bar_items, idx + TOOL_BAR_ITEM_KEY);
  XSETFRAME (frame, f);

  /* We generate two events here.  The first one is to set the prefix
     to `(tool_bar)', see keyboard.c.  */
  event.kind = TOOL_BAR_EVENT;
  event.frame_or_window = frame;
  event.arg = frame;
  kbd_buffer_store_event (&event);

  event.kind = TOOL_BAR_EVENT;
  event.frame_or_window = frame;
  event.arg = key;
  /* Convert between the modifier bits GDK uses and the modifier bits
     Emacs uses.  This assumes GDK and X masks are the same, which they are when
     this is written.  */
  event.modifiers = x_x_to_emacs_modifiers (FRAME_DISPLAY_INFO (f), mod);
  kbd_buffer_store_event (&event);

  /* Return focus to the frame after we have clicked on a detached
     tool bar button. */
  x_focus_frame (f, false);
}

static GtkWidget *
xg_get_tool_bar_widgets (GtkWidget *vb, GtkWidget **wimage)
{
  GList *clist = gtk_container_get_children (GTK_CONTAINER (vb));
  GtkWidget *c1 = clist->data;
  GtkWidget *c2 = clist->next ? clist->next->data : NULL;

  *wimage = GTK_IS_IMAGE (c1) ? c1 : c2;
  g_list_free (clist);
  return GTK_IS_LABEL (c1) ? c1 : c2;
}


/* This callback is called when the mouse enters or leaves a tool bar item.
   It is used for displaying and hiding the help text.
   W is the tool bar item, a button.
   EVENT is either an enter event or leave event.
   CLIENT_DATA is an integer that is the index of the button in the
   tool bar.  0 is the first button.

   Returns FALSE to tell GTK to keep processing this event.  */

static gboolean
xg_tool_bar_help_callback (GtkWidget *w,
                           GdkEventCrossing *event,
                           gpointer client_data)
{
  intptr_t idx = (intptr_t) client_data;
  struct frame *f = g_object_get_data (G_OBJECT (w), XG_FRAME_DATA);
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

#ifndef HAVE_GTK3
static gboolean
xg_tool_bar_item_expose_callback (GtkWidget *w,
                                  GdkEventExpose *event,
                                  gpointer client_data)
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
#endif

#ifdef HAVE_GTK_ORIENTABLE_SET_ORIENTATION
#define toolbar_set_orientation(w, o) \
  gtk_orientable_set_orientation (GTK_ORIENTABLE (w), o)
#else
#define toolbar_set_orientation(w, o) \
  gtk_toolbar_set_orientation (GTK_TOOLBAR (w), o)
#endif

/* Attach a tool bar to frame F.  */

static void
xg_pack_tool_bar (struct frame *f, Lisp_Object pos)
{
  struct x_output *x = f->output_data.x;
  bool into_hbox = EQ (pos, Qleft) || EQ (pos, Qright);
  GtkWidget *top_widget = x->toolbar_widget;

  toolbar_set_orientation (x->toolbar_widget,
                           into_hbox
                           ? GTK_ORIENTATION_VERTICAL
                           : GTK_ORIENTATION_HORIZONTAL);

  if (into_hbox)
    {
      gtk_box_pack_start (GTK_BOX (x->hbox_widget), top_widget,
                          FALSE, FALSE, 0);

      if (EQ (pos, Qleft))
        gtk_box_reorder_child (GTK_BOX (x->hbox_widget),
                               top_widget,
                               0);
      x->toolbar_in_hbox = true;
    }
  else
    {
      bool vbox_pos = x->menubar_widget != 0;
      gtk_box_pack_start (GTK_BOX (x->vbox_widget), top_widget,
                          FALSE, FALSE, 0);

      if (EQ (pos, Qtop))
        gtk_box_reorder_child (GTK_BOX (x->vbox_widget),
                               top_widget,
                               vbox_pos);
      x->toolbar_in_hbox = false;
    }
  x->toolbar_is_packed = true;
}

static bool xg_update_tool_bar_sizes (struct frame *f);

static void
tb_size_cb (GtkWidget    *widget,
            GdkRectangle *allocation,
            gpointer      user_data)
{
  /* When tool bar is created it has one preferred size.  But when size is
     allocated between widgets, it may get another.  So we must update
     size hints if tool bar size changes.  Seen on Fedora 18 at least.  */
  struct frame *f = user_data;

  if (xg_update_tool_bar_sizes (f))
    {
      frame_size_history_add (f, Qtb_size_cb, 0, 0, Qnil);
      adjust_frame_size (f, -1, -1, 5, 0, Qtool_bar_lines);
    }
}

/* Create a tool bar for frame F.  */

static void
xg_create_tool_bar (struct frame *f)
{
  struct x_output *x = f->output_data.x;
#if GTK_CHECK_VERSION (3, 3, 6)
  GtkStyleContext *gsty;
#endif
  struct xg_frame_tb_info *tbinfo
    = g_object_get_data (G_OBJECT (FRAME_GTK_OUTER_WIDGET (f)),
                         TB_INFO_KEY);
  if (! tbinfo)
    {
      tbinfo = xmalloc (sizeof (*tbinfo));
      tbinfo->last_tool_bar = Qnil;
      tbinfo->style = Qnil;
      tbinfo->hmargin = tbinfo->vmargin = 0;
      tbinfo->dir = GTK_TEXT_DIR_NONE;
      tbinfo->n_last_items = 0;
      g_object_set_data (G_OBJECT (FRAME_GTK_OUTER_WIDGET (f)),
                         TB_INFO_KEY,
                         tbinfo);
    }

  x->toolbar_widget = gtk_toolbar_new ();

  gtk_widget_set_name (x->toolbar_widget, "emacs-toolbar");

  gtk_toolbar_set_style (GTK_TOOLBAR (x->toolbar_widget), GTK_TOOLBAR_ICONS);
  toolbar_set_orientation (x->toolbar_widget, GTK_ORIENTATION_HORIZONTAL);
  g_signal_connect (x->toolbar_widget, "size-allocate",
                    G_CALLBACK (tb_size_cb), f);
#if GTK_CHECK_VERSION (3, 3, 6)
  gsty = gtk_widget_get_style_context (x->toolbar_widget);
  gtk_style_context_add_class (gsty, "primary-toolbar");
#endif
}


#define PROP(IDX) AREF (f->tool_bar_items, i * TOOL_BAR_ITEM_NSLOTS + (IDX))

/* Find the right-to-left image named by RTL in the tool bar images for F.
   Returns IMAGE if RTL is not found.  */

static Lisp_Object
find_rtl_image (struct frame *f, Lisp_Object image, Lisp_Object rtl)
{
  int i;
  Lisp_Object file, rtl_name;

  rtl_name = Ffile_name_nondirectory (rtl);

  for (i = 0; i < f->n_tool_bar_items; ++i)
    {
      Lisp_Object rtl_image = PROP (TOOL_BAR_ITEM_IMAGES);
      if (!NILP (file = file_for_image (rtl_image)))
        {
          file = call1 (intern ("file-name-sans-extension"),
                       Ffile_name_nondirectory (file));
          if (! NILP (Fequal (file, rtl_name)))
            {
              image = rtl_image;
              break;
            }
        }
    }

  return image;
}

static GtkToolItem *
xg_make_tool_item (struct frame *f,
                   GtkWidget *wimage,
                   GtkWidget **wbutton,
                   const char *label,
                   int i, bool horiz, bool text_image)
{
  GtkToolItem *ti = gtk_tool_item_new ();
  GtkWidget *vb = gtk_box_new (horiz
                               ? GTK_ORIENTATION_HORIZONTAL
                               : GTK_ORIENTATION_VERTICAL,
                               0);
  GtkWidget *wb = gtk_button_new ();
  /* The eventbox is here so we can have tooltips on disabled items.  */
  GtkWidget *weventbox = gtk_event_box_new ();
#if GTK_CHECK_VERSION (3, 3, 6)
  GtkCssProvider *css_prov = gtk_css_provider_new ();
  GtkStyleContext *gsty;

  gtk_css_provider_load_from_data (css_prov,
				   "GtkEventBox {"
				   "    background-color: transparent;"
				   "}",
				   -1, NULL);

  gsty = gtk_widget_get_style_context (weventbox);
  gtk_style_context_add_provider (gsty,
				  GTK_STYLE_PROVIDER (css_prov),
				  GTK_STYLE_PROVIDER_PRIORITY_USER);
  g_object_unref (css_prov);
#endif

  gtk_box_set_homogeneous (GTK_BOX (vb), FALSE);

  if (wimage && !text_image)
    gtk_box_pack_start (GTK_BOX (vb), wimage, TRUE, TRUE, 0);
  if (label)
    gtk_box_pack_start (GTK_BOX (vb), gtk_label_new (label), TRUE, TRUE, 0);
  if (wimage && text_image)
    gtk_box_pack_start (GTK_BOX (vb), wimage, TRUE, TRUE, 0);

#if GTK_CHECK_VERSION (3, 20, 0)
  gtk_widget_set_focus_on_click (wb, FALSE);
#else
  gtk_button_set_focus_on_click (GTK_BUTTON (wb), FALSE);
#endif
  gtk_button_set_relief (GTK_BUTTON (wb), GTK_RELIEF_NONE);
  gtk_container_add (GTK_CONTAINER (wb), vb);
  gtk_container_add (GTK_CONTAINER (weventbox), wb);
  gtk_container_add (GTK_CONTAINER (ti), weventbox);

  if (wimage || label)
    {
      intptr_t ii = i;
      gpointer gi = (gpointer) ii;

      g_signal_connect (G_OBJECT (wb), "clicked",
                        G_CALLBACK (xg_tool_bar_callback),
                        gi);

      g_object_set_data (G_OBJECT (weventbox), XG_FRAME_DATA, (gpointer)f);

#ifndef HAVE_GTK3
      /* Catch expose events to overcome an annoying redraw bug, see
         comment for xg_tool_bar_item_expose_callback.  */
      g_signal_connect (G_OBJECT (ti),
                        "expose-event",
                        G_CALLBACK (xg_tool_bar_item_expose_callback),
                        0);
#endif
      gtk_tool_item_set_homogeneous (ti, FALSE);

      /* Callback to save modifier mask (Shift/Control, etc).  GTK makes
         no distinction based on modifiers in the activate callback,
         so we have to do it ourselves.  */
      g_signal_connect (wb, "button-release-event",
                        G_CALLBACK (xg_tool_bar_button_cb),
                        NULL);

      g_object_set_data (G_OBJECT (wb), XG_FRAME_DATA, (gpointer)f);

      /* Use enter/leave notify to show help.  We use the events
         rather than the GtkButton specific signals "enter" and
         "leave", so we can have only one callback.  The event
         will tell us what kind of event it is.  */
      g_signal_connect (G_OBJECT (weventbox),
                        "enter-notify-event",
                        G_CALLBACK (xg_tool_bar_help_callback),
                        gi);
      g_signal_connect (G_OBJECT (weventbox),
                        "leave-notify-event",
                        G_CALLBACK (xg_tool_bar_help_callback),
                        gi);
    }

  if (wbutton) *wbutton = wb;

  return ti;
}

static bool
is_box_type (GtkWidget *vb, bool is_horizontal)
{
#ifdef HAVE_GTK3
  bool ret = 0;
  if (GTK_IS_BOX (vb))
    {
      GtkOrientation ori = gtk_orientable_get_orientation (GTK_ORIENTABLE (vb));
      ret = (ori == GTK_ORIENTATION_HORIZONTAL && is_horizontal)
        || (ori == GTK_ORIENTATION_VERTICAL && ! is_horizontal);
    }
  return ret;
#else
  return is_horizontal ? GTK_IS_VBOX (vb) : GTK_IS_HBOX (vb);
#endif
}


static bool
xg_tool_item_stale_p (GtkWidget *wbutton, const char *stock_name,
		      const char *icon_name, const struct image *img,
		      const char *label, bool horiz)
{
  gpointer old;
  GtkWidget *wimage;
  GtkWidget *vb = XG_BIN_CHILD (wbutton);
  GtkWidget *wlbl = xg_get_tool_bar_widgets (vb, &wimage);

  /* Check if the tool icon matches.  */
  if (stock_name && wimage)
    {
      old = g_object_get_data (G_OBJECT (wimage),
			       XG_TOOL_BAR_STOCK_NAME);
      if (!old || strcmp (old, stock_name))
	return 1;
    }
  else if (icon_name && wimage)
    {
      old = g_object_get_data (G_OBJECT (wimage),
			       XG_TOOL_BAR_ICON_NAME);
      if (!old || strcmp (old, icon_name))
	return 1;
    }
  else if (wimage)
    {
      gpointer gold_img = g_object_get_data (G_OBJECT (wimage),
                                             XG_TOOL_BAR_IMAGE_DATA);
      Pixmap old_img = (Pixmap) gold_img;
      if (old_img != img->pixmap)
	return 1;
    }

  /* Check button configuration and label.  */
  if (is_box_type (vb, horiz)
      || (label ? (wlbl == NULL) : (wlbl != NULL)))
    return 1;

  /* Ensure label is correct.  */
  if (label && wlbl)
    gtk_label_set_text (GTK_LABEL (wlbl), label);
  return 0;
}

static bool
xg_update_tool_bar_sizes (struct frame *f)
{
  struct x_output *x = f->output_data.x;
  GtkRequisition req;
  int nl = 0, nr = 0, nt = 0, nb = 0;
  GtkWidget *top_widget = x->toolbar_widget;

  gtk_widget_get_preferred_size (GTK_WIDGET (top_widget), NULL, &req);
  if (x->toolbar_in_hbox)
    {
      int pos;
      gtk_container_child_get (GTK_CONTAINER (x->hbox_widget),
                               top_widget,
                               "position", &pos, NULL);
      if (pos == 0) nl = req.width;
      else nr = req.width;
    }
  else
    {
      int pos;
      gtk_container_child_get (GTK_CONTAINER (x->vbox_widget),
                               top_widget,
                               "position", &pos, NULL);
      if (pos == 0 || (pos == 1 && x->menubar_widget)) nt = req.height;
      else nb = req.height;
    }

  if (nl != FRAME_TOOLBAR_LEFT_WIDTH (f)
      || nr != FRAME_TOOLBAR_RIGHT_WIDTH (f)
      || nt != FRAME_TOOLBAR_TOP_HEIGHT (f)
      || nb != FRAME_TOOLBAR_BOTTOM_HEIGHT (f))
    {
      FRAME_TOOLBAR_RIGHT_WIDTH (f) = FRAME_TOOLBAR_LEFT_WIDTH (f)
        = FRAME_TOOLBAR_TOP_HEIGHT (f) = FRAME_TOOLBAR_BOTTOM_HEIGHT (f) = 0;
      FRAME_TOOLBAR_LEFT_WIDTH (f) = nl;
      FRAME_TOOLBAR_RIGHT_WIDTH (f) = nr;
      FRAME_TOOLBAR_TOP_HEIGHT (f) = nt;
      FRAME_TOOLBAR_BOTTOM_HEIGHT (f) = nb;

      return true;
    }
  else
    return false;
}

static char *
find_icon_from_name (char *name,
                     GtkIconTheme *icon_theme,
                     char **icon_name)
{
#if ! GTK_CHECK_VERSION (3, 10, 0)
  GtkStockItem stock_item;
#endif

  if (name[0] == 'n' && name[1] == ':')
    {
      *icon_name = name + 2;
      name = NULL;

      if (! gtk_icon_theme_has_icon (icon_theme, *icon_name))
        *icon_name = NULL;
    }

#if ! GTK_CHECK_VERSION (3, 10, 0)
  else if (gtk_stock_lookup (name, &stock_item))
    *icon_name = NULL;
#endif
  else if (gtk_icon_theme_has_icon (icon_theme, name))
    {
      *icon_name = name;
      name = NULL;
    }
  else
    {
      name = NULL;
      *icon_name = NULL;
    }

  return name;
}


/* Update the tool bar for frame F.  Add new buttons and remove old.  */

void
update_frame_tool_bar (struct frame *f)
{
  int i, j;
  struct x_output *x = f->output_data.x;
  int hmargin = 0, vmargin = 0;
  GtkToolbar *wtoolbar;
  GtkToolItem *ti;
  GtkTextDirection dir;
  Lisp_Object style;
  bool text_image, horiz;
  struct xg_frame_tb_info *tbinfo;
  GdkScreen *screen;
  GtkIconTheme *icon_theme;


  if (! FRAME_GTK_WIDGET (f))
    return;

  block_input ();

  if (RANGED_INTEGERP (1, Vtool_bar_button_margin, INT_MAX))
    {
      hmargin = XFASTINT (Vtool_bar_button_margin);
      vmargin = XFASTINT (Vtool_bar_button_margin);
    }
  else if (CONSP (Vtool_bar_button_margin))
    {
      if (RANGED_INTEGERP (1, XCAR (Vtool_bar_button_margin), INT_MAX))
        hmargin = XFASTINT (XCAR (Vtool_bar_button_margin));

      if (RANGED_INTEGERP (1, XCDR (Vtool_bar_button_margin), INT_MAX))
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

  wtoolbar = GTK_TOOLBAR (x->toolbar_widget);
  dir = gtk_widget_get_direction (GTK_WIDGET (wtoolbar));

  style = Ftool_bar_get_system_style ();
  screen = gtk_widget_get_screen (GTK_WIDGET (wtoolbar));
  icon_theme = gtk_icon_theme_get_for_screen (screen);

  /* Are we up to date? */
  tbinfo = g_object_get_data (G_OBJECT (FRAME_GTK_OUTER_WIDGET (f)),
                              TB_INFO_KEY);

  if (! NILP (tbinfo->last_tool_bar) && ! NILP (f->tool_bar_items)
      && tbinfo->n_last_items == f->n_tool_bar_items
      && tbinfo->hmargin == hmargin && tbinfo->vmargin == vmargin
      && tbinfo->dir == dir
      && ! NILP (Fequal (tbinfo->style, style))
      && ! NILP (Fequal (tbinfo->last_tool_bar, f->tool_bar_items)))
    {
      unblock_input ();
      return;
    }

  tbinfo->last_tool_bar = f->tool_bar_items;
  tbinfo->n_last_items = f->n_tool_bar_items;
  tbinfo->style = style;
  tbinfo->hmargin = hmargin;
  tbinfo->vmargin = vmargin;
  tbinfo->dir = dir;

  text_image = EQ (style, Qtext_image_horiz);
  horiz = EQ (style, Qboth_horiz) || text_image;

  for (i = j = 0; i < f->n_tool_bar_items; ++i)
    {
      bool enabled_p = !NILP (PROP (TOOL_BAR_ITEM_ENABLED_P));
      bool selected_p = !NILP (PROP (TOOL_BAR_ITEM_SELECTED_P));
      int idx;
      ptrdiff_t img_id;
      int icon_size = 0;
      struct image *img = NULL;
      Lisp_Object image;
      Lisp_Object stock = Qnil;
      char *stock_name = NULL;
      char *icon_name = NULL;
      Lisp_Object rtl;
      GtkWidget *wbutton = NULL;
      Lisp_Object specified_file;
      bool vert_only = ! NILP (PROP (TOOL_BAR_ITEM_VERT_ONLY));
      const char *label
	= (EQ (style, Qimage) || (vert_only && horiz)) ? NULL
	: STRINGP (PROP (TOOL_BAR_ITEM_LABEL))
	? SSDATA (PROP (TOOL_BAR_ITEM_LABEL))
	: "";

      ti = gtk_toolbar_get_nth_item (GTK_TOOLBAR (wtoolbar), j);

      /* If this is a separator, use a gtk separator item.  */
      if (EQ (PROP (TOOL_BAR_ITEM_TYPE), Qt))
	{
	  if (ti == NULL || !GTK_IS_SEPARATOR_TOOL_ITEM (ti))
	    {
	      if (ti)
		gtk_container_remove (GTK_CONTAINER (wtoolbar),
				      GTK_WIDGET (ti));
	      ti = gtk_separator_tool_item_new ();
	      gtk_toolbar_insert (GTK_TOOLBAR (wtoolbar), ti, j);
	    }
	  j++;
	  continue;
	}

      /* Otherwise, the tool-bar item is an ordinary button.  */

      if (ti && GTK_IS_SEPARATOR_TOOL_ITEM (ti))
	{
	  gtk_container_remove (GTK_CONTAINER (wtoolbar), GTK_WIDGET (ti));
	  ti = NULL;
	}

      if (ti) wbutton = XG_BIN_CHILD (XG_BIN_CHILD (ti));

      /* Ignore invalid image specifications.  */
      image = PROP (TOOL_BAR_ITEM_IMAGES);
      if (!valid_image_p (image))
        {
          if (ti)
	    gtk_container_remove (GTK_CONTAINER (wtoolbar),
				  GTK_WIDGET (ti));
          continue;
        }

      specified_file = file_for_image (image);
      if (!NILP (specified_file) && !NILP (Ffboundp (Qx_gtk_map_stock)))
        stock = call1 (Qx_gtk_map_stock, specified_file);

      if (CONSP (stock))
        {
          Lisp_Object tem;
          for (tem = stock; CONSP (tem); tem = XCDR (tem))
            if (! NILP (tem) && STRINGP (XCAR (tem)))
              {
                stock_name = find_icon_from_name (SSDATA (XCAR (tem)),
                                                  icon_theme,
                                                  &icon_name);
                if (stock_name || icon_name) break;
              }
        }
      else if (STRINGP (stock))
        {
          stock_name = find_icon_from_name (SSDATA (stock),
                                            icon_theme,
                                            &icon_name);
        }

      if (stock_name || icon_name)
        icon_size = gtk_toolbar_get_icon_size (wtoolbar);

      if (stock_name == NULL && icon_name == NULL)
        {
          /* No stock image, or stock item not known.  Try regular
             image.  If image is a vector, choose it according to the
             button state.  */
          if (dir == GTK_TEXT_DIR_RTL
              && !NILP (rtl = PROP (TOOL_BAR_ITEM_RTL_IMAGE))
              && STRINGP (rtl))
	    image = find_rtl_image (f, image, rtl);

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

              eassert (ASIZE (image) >= idx);
              image = AREF (image, idx);
            }
          else
            idx = -1;

          img_id = lookup_image (f, image);
          img = IMAGE_FROM_ID (f, img_id);
          prepare_image_for_display (f, img);

          if (img->load_failed_p || img->pixmap == None)
            {
              if (ti)
		gtk_container_remove (GTK_CONTAINER (wtoolbar),
				      GTK_WIDGET (ti));
              continue;
            }
        }

      /* If there is an existing widget, check if it's stale; if so,
	 remove it and make a new tool item from scratch.  */
      if (ti && xg_tool_item_stale_p (wbutton, stock_name, icon_name,
				      img, label, horiz))
	{
	  gtk_container_remove (GTK_CONTAINER (wtoolbar),
				GTK_WIDGET (ti));
	  ti = NULL;
	}

      if (ti == NULL)
        {
          GtkWidget *w;

	  /* Save the image so we can see if an update is needed the
	     next time we call xg_tool_item_match_p.  */
	  if (EQ (style, Qtext))
	    w = NULL;
	  else if (stock_name)
            {

#if GTK_CHECK_VERSION (3, 10, 0)
              w = gtk_image_new_from_icon_name (stock_name, icon_size);
#else
              w = gtk_image_new_from_stock (stock_name, icon_size);
#endif
              g_object_set_data_full (G_OBJECT (w), XG_TOOL_BAR_STOCK_NAME,
                                      (gpointer) xstrdup (stock_name),
                                      (GDestroyNotify) xfree);
            }
          else if (icon_name)
            {
              w = gtk_image_new_from_icon_name (icon_name, icon_size);
              g_object_set_data_full (G_OBJECT (w), XG_TOOL_BAR_ICON_NAME,
                                      (gpointer) xstrdup (icon_name),
                                      (GDestroyNotify) xfree);
            }
          else
            {
              w = xg_get_image_for_pixmap (f, img, x->widget, NULL);
              g_object_set_data (G_OBJECT (w), XG_TOOL_BAR_IMAGE_DATA,
                                 (gpointer)img->pixmap);
            }

#if GTK_CHECK_VERSION (3, 14, 0)
          if (w)
            {
              gtk_widget_set_margin_start (w, hmargin);
              gtk_widget_set_margin_end (w, hmargin);
              gtk_widget_set_margin_top (w, vmargin);
              gtk_widget_set_margin_bottom (w, vmargin);
            }
#else
	  if (w) gtk_misc_set_padding (GTK_MISC (w), hmargin, vmargin);
#endif
          ti = xg_make_tool_item (f, w, &wbutton, label, i, horiz, text_image);
          gtk_toolbar_insert (GTK_TOOLBAR (wtoolbar), ti, j);
        }

#undef PROP

      gtk_widget_set_sensitive (wbutton, enabled_p);
      j++;
    }

  /* Remove buttons not longer needed.  */
  do
    {
      ti = gtk_toolbar_get_nth_item (GTK_TOOLBAR (wtoolbar), j);
      if (ti)
	gtk_container_remove (GTK_CONTAINER (wtoolbar), GTK_WIDGET (ti));
    } while (ti != NULL);

  if (f->n_tool_bar_items != 0)
    {
      if (! x->toolbar_is_packed)
        xg_pack_tool_bar (f, FRAME_TOOL_BAR_POSITION (f));
      gtk_widget_show_all (x->toolbar_widget);
      if (xg_update_tool_bar_sizes (f))
	{
	  int inhibit
	    = ((f->after_make_frame
		&& !f->tool_bar_resized
		&& (EQ (frame_inhibit_implied_resize, Qt)
		    || (CONSP (frame_inhibit_implied_resize)
			&& !NILP (Fmemq (Qtool_bar_lines,
					 frame_inhibit_implied_resize))))
		/* This will probably fail to DTRT in the
		   fullheight/-width cases.  */
		&& NILP (get_frame_param (f, Qfullscreen)))
	       ? 0
	       : 2);

	  frame_size_history_add (f, Qupdate_frame_tool_bar, 0, 0, Qnil);
	  adjust_frame_size (f, -1, -1, inhibit, 0, Qtool_bar_lines);
	}
      f->tool_bar_resized = f->tool_bar_redisplayed;
    }

  unblock_input ();
}

/* Deallocate all resources for the tool bar on frame F.
   Remove the tool bar.  */

void
free_frame_tool_bar (struct frame *f)
{
  struct x_output *x = f->output_data.x;

  if (x->toolbar_widget)
    {
      struct xg_frame_tb_info *tbinfo;
      GtkWidget *top_widget = x->toolbar_widget;

      block_input ();
      if (x->toolbar_is_packed)
        {
          if (x->toolbar_in_hbox)
            gtk_container_remove (GTK_CONTAINER (x->hbox_widget),
                                  top_widget);
          else
            gtk_container_remove (GTK_CONTAINER (x->vbox_widget),
                                  top_widget);
        }
      else
        gtk_widget_destroy (x->toolbar_widget);

      x->toolbar_widget = 0;
      x->toolbar_widget = 0;
      x->toolbar_is_packed = false;
      FRAME_TOOLBAR_TOP_HEIGHT (f) = FRAME_TOOLBAR_BOTTOM_HEIGHT (f) = 0;
      FRAME_TOOLBAR_LEFT_WIDTH (f) = FRAME_TOOLBAR_RIGHT_WIDTH (f) = 0;

      tbinfo = g_object_get_data (G_OBJECT (FRAME_GTK_OUTER_WIDGET (f)),
                                  TB_INFO_KEY);
      if (tbinfo)
        {
          xfree (tbinfo);
          g_object_set_data (G_OBJECT (FRAME_GTK_OUTER_WIDGET (f)),
                             TB_INFO_KEY,
                             NULL);
        }

      frame_size_history_add (f, Qfree_frame_tool_bar, 0, 0, Qnil);
      adjust_frame_size (f, -1, -1, 2, 0, Qtool_bar_lines);

      unblock_input ();
    }
}

void
xg_change_toolbar_position (struct frame *f, Lisp_Object pos)
{
  struct x_output *x = f->output_data.x;
  GtkWidget *top_widget = x->toolbar_widget;

  if (! x->toolbar_widget || ! top_widget)
    return;

  block_input ();
  g_object_ref (top_widget);
  if (x->toolbar_is_packed)
    {
      if (x->toolbar_in_hbox)
        gtk_container_remove (GTK_CONTAINER (x->hbox_widget),
                              top_widget);
      else
        gtk_container_remove (GTK_CONTAINER (x->vbox_widget),
                              top_widget);
    }

  xg_pack_tool_bar (f, pos);
  g_object_unref (top_widget);

  if (xg_update_tool_bar_sizes (f))
    {
      frame_size_history_add (f, Qxg_change_toolbar_position, 0, 0, Qnil);
      adjust_frame_size (f, -1, -1, 2, 0, Qtool_bar_lines);
    }


  unblock_input ();
}



/***********************************************************************
                      Initializing
***********************************************************************/
void
xg_initialize (void)
{
  GtkBindingSet *binding_set;
  GtkSettings *settings;

#if HAVE_XFT
  /* Work around a bug with corrupted data if libXft gets unloaded.  This way
     we keep it permanently linked in.  */
  XftInit (0);
#endif

  gdpy_def = NULL;
  xg_ignore_gtk_scrollbar = 0;
  xg_menu_cb_list.prev = xg_menu_cb_list.next =
    xg_menu_item_cb_list.prev = xg_menu_item_cb_list.next = 0;

  id_to_widget.max_size = id_to_widget.used = 0;
  id_to_widget.widgets = 0;

  settings = gtk_settings_get_for_screen (gdk_display_get_default_screen
                                          (gdk_display_get_default ()));
#if ! GTK_CHECK_VERSION (3, 10, 0)
  /* Remove F10 as a menu accelerator, it does not mix well with Emacs key
     bindings.  It doesn't seem to be any way to remove properties,
     so we set it to "" which in means "no key".  */
  gtk_settings_set_string_property (settings,
                                    "gtk-menu-bar-accel",
                                    "",
                                    EMACS_CLASS);
#endif

  /* Make GTK text input widgets use Emacs style keybindings.  This is
     Emacs after all.  */
#if GTK_CHECK_VERSION (3, 16, 0)
  g_object_set (settings, "gtk-key-theme-name", "Emacs", NULL);
#else
  gtk_settings_set_string_property (settings,
                                    "gtk-key-theme-name",
                                    "Emacs",
                                    EMACS_CLASS);
#endif

  /* Make dialogs close on C-g.  Since file dialog inherits from
     dialog, this works for them also.  */
  binding_set = gtk_binding_set_by_class (g_type_class_ref (GTK_TYPE_DIALOG));
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_g, GDK_CONTROL_MASK,
                                "close", 0);

  /* Make menus close on C-g.  */
  binding_set = gtk_binding_set_by_class (g_type_class_ref
                                          (GTK_TYPE_MENU_SHELL));
  gtk_binding_entry_add_signal (binding_set, GDK_KEY_g, GDK_CONTROL_MASK,
                                "cancel", 0);
  update_theme_scrollbar_width ();
  update_theme_scrollbar_height ();

#ifdef HAVE_FREETYPE
  x_last_font_name = NULL;
#endif
}

#endif /* USE_GTK */
