/* Defines some widget utility functions.

Copyright (C) 1992 Lucid, Inc.
Copyright (C) 1994, 2001-2020 Free Software Foundation, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#include <config.h>

#include <setjmp.h>
#include <lisp.h>

#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include "lwlib-utils.h"
#include "lwlib.h"

/* Redisplay the contents of the widget, without first clearing it. */
void
XtNoClearRefreshWidget (Widget widget)
{
  XEvent event;

  event.type = Expose;
  event.xexpose.serial = 0;
  event.xexpose.send_event = 0;
  event.xexpose.display = XtDisplay (widget);
  event.xexpose.window = XtWindow (widget);
  event.xexpose.x = 0;
  event.xexpose.y = 0;
  event.xexpose.width = widget->core.width;
  event.xexpose.height = widget->core.height;
  event.xexpose.count = 0;

  (*widget->core.widget_class->core_class.expose)
    (widget, &event, (Region)NULL);
}


/*
 * Apply a function to all the subwidgets of a given widget recursively.
*/
void
XtApplyToWidgets (Widget w, XtApplyToWidgetsProc proc, XtPointer arg)
{
  if (XtIsComposite (w))
    {
      CompositeWidget cw = (CompositeWidget) w;
      /* We have to copy the children list before mapping over it, because
	 the procedure might add/delete elements, which would lose badly.
	 */
      int nkids = cw->composite.num_children;
      Widget *kids = (Widget *) xmalloc (sizeof (Widget) * nkids);
      int i;
      memcpy ((char *) kids, (char *) cw->composite.children,
	      sizeof (Widget) * nkids);
      for (i = 0; i < nkids; i++)
/* This prevent us from using gadgets, why is it here? */
/*	if (XtIsWidget (kids [i])) */
	  {
	    /* do the kiddies first in case we're destroying */
	    XtApplyToWidgets (kids [i], proc, arg);
	    proc (kids [i], arg);
	  }
      xfree (kids);
    }
}


/*
 * Apply a function to all the subwidgets of a given widget recursively.
 * Stop as soon as the function returns non NULL and returns this as a value.
 */
void *
XtApplyUntilToWidgets (Widget w, XtApplyUntilToWidgetsProc proc, XtPointer arg)
{
  void* result;
  if (XtIsComposite (w))
    {
      CompositeWidget cw = (CompositeWidget)w;
      int i;
      for (i = 0; i < cw->composite.num_children; i++)
	if (XtIsWidget (cw->composite.children [i])){
	  result = proc (cw->composite.children [i], arg);
	  if (result)
	    return result;
	  result = XtApplyUntilToWidgets (cw->composite.children [i], proc,
					  arg);
	  if (result)
	    return result;
	}
    }
  return NULL;
}


/*
 * Returns a copy of the list of all children of a composite widget
 */
Widget *
XtCompositeChildren (Widget widget, unsigned int *number)
{
  CompositeWidget cw = (CompositeWidget)widget;
  Widget* result;
  int n;
  int i;

  if (!XtIsComposite (widget))
    {
      *number = 0;
      return NULL;
    }
  n = cw->composite.num_children;
  result = (Widget*)(void*)XtMalloc (n * sizeof (Widget));
  *number = n;
  for (i = 0; i < n; i++)
    result [i] = cw->composite.children [i];
  return result;
}

Boolean
XtWidgetBeingDestroyedP (Widget widget)
{
  return widget->core.being_destroyed;
}

#ifdef USE_CAIRO
/* Xft emulation on cairo.  */
#include <math.h>
#include <cairo-ft.h>
#include <cairo-xlib.h>

XftFont *
crxft_font_open_name (Display *dpy, int screen, const char *name)
{
  XftFont *pub = NULL;
  FcPattern *pattern = FcNameParse ((FcChar8 *) name);
  if (pattern)
    {
      FcConfigSubstitute (NULL, pattern, FcMatchPattern);
      double dpi;
      if (FcPatternGetDouble (pattern, FC_DPI, 0, &dpi) == FcResultNoMatch)
	{
	  char *v = XGetDefault (dpy, "Xft", FC_DPI);
	  if (v == NULL || sscanf (v, "%lf", &dpi) != 1)
	    dpi = ((DisplayHeight (dpy, screen) * 25.4)
		   / DisplayHeightMM (dpy, screen));
	  FcPatternAddDouble (pattern, FC_DPI, dpi);
	}
      FcDefaultSubstitute (pattern);
      cairo_font_face_t *font_face
	= cairo_ft_font_face_create_for_pattern (pattern);
      if (font_face)
	{
	  double pixel_size;
	  if ((FcPatternGetDouble (pattern, FC_PIXEL_SIZE, 0, &pixel_size)
	       != FcResultMatch)
	      || pixel_size < 1)
	    pixel_size = 10;

	  pub = xmalloc (sizeof (*pub));
	  cairo_matrix_t font_matrix, ctm;
	  cairo_matrix_init_scale (&font_matrix, pixel_size, pixel_size);
	  cairo_matrix_init_identity (&ctm);
	  cairo_font_options_t *options = cairo_font_options_create ();
	  cairo_ft_font_options_substitute (options, pattern);
	  pub->scaled_font = cairo_scaled_font_create (font_face, &font_matrix,
						       &ctm, options);
	  cairo_font_face_destroy (font_face);
	  cairo_font_options_destroy (options);

	  cairo_font_extents_t extents;
	  cairo_scaled_font_extents (pub->scaled_font, &extents);
	  pub->ascent = lround (extents.ascent);
	  pub->descent = lround (extents.descent);
	  pub->height = lround (extents.height);
	  pub->max_advance_width = lround (extents.max_x_advance);
	}
      FcPatternDestroy (pattern);
    }
  if (pub && pub->height <= 0)
    {
      crxft_font_close (pub);
      pub = NULL;
    }
  return pub;
}

void
crxft_font_close (XftFont *pub)
{
  cairo_scaled_font_destroy (pub->scaled_font);
  xfree (pub);
}

cairo_t *
crxft_draw_create (Display *dpy, Drawable drawable, Visual *visual)
{
  cairo_t *cr = NULL;
  Window root;
  int x, y;
  unsigned int width, height, border_width, depth;

  if (!XGetGeometry (dpy, drawable, &root, &x, &y, &width, &height,
		     &border_width, &depth))
    return NULL;

  cairo_surface_t *surface = cairo_xlib_surface_create (dpy, drawable, visual,
							width, height);
  if (surface)
    {
      cr = cairo_create (surface);
      cairo_surface_destroy (surface);
    }

  return cr;
}

static void
crxft_set_source_color (cairo_t *cr, const XftColor *color)
{
  cairo_set_source_rgba (cr, color->color.red / 65535.0,
			 color->color.green / 65535.0,
			 color->color.blue / 65535.0,
			 color->color.alpha / 65535.0);
}

void
crxft_draw_rect (cairo_t *cr, const XftColor *color, int x, int y,
		 unsigned int width, unsigned int height)
{
  crxft_set_source_color (cr, color);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
}

void
crxft_draw_string (cairo_t *cr, const XftColor *color, XftFont *pub,
		   int x, int y, const FcChar8 *string, int len)
{
  char *buf = xmalloc (len + 1);
  memcpy (buf, string, len);
  buf[len] = '\0';
  crxft_set_source_color (cr, color);
  cairo_set_scaled_font (cr, pub->scaled_font);
  cairo_move_to (cr, x, y);
  cairo_show_text (cr, buf);
  xfree (buf);
}

void
crxft_text_extents (XftFont *pub, const FcChar8 *string, int len,
		    XGlyphInfo *extents)
{
  char *buf = xmalloc (len + 1);
  memcpy (buf, string, len);
  buf[len] = '\0';
  cairo_text_extents_t text_extents;
  cairo_scaled_font_text_extents (pub->scaled_font, buf, &text_extents);
  xfree (buf);
  extents->x = ceil (- text_extents.x_bearing);
  extents->y = ceil (- text_extents.y_bearing);
  extents->width = (ceil (text_extents.x_bearing + text_extents.width)
		    + extents->x);
  extents->height = (ceil (text_extents.y_bearing + text_extents.height)
		     + extents->y);
  extents->xOff = lround (text_extents.x_advance);
  extents->yOff = lround (text_extents.y_advance);
}
#endif	/* USE_CAIRO */
