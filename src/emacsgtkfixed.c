/* A Gtk Widget that inherits GtkFixed, but can be shrunk.
This file is only use when compiling with Gtk+ 3.

Copyright (C) 2011-2024 Free Software Foundation, Inc.

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

#include "lisp.h"
#include "frame.h"
#ifdef HAVE_PGTK
#include "pgtkterm.h"
#else
#include "xterm.h"
#endif
#include "emacsgtkfixed.h"

/* Silence a bogus diagnostic; see GNOME bug 683906.  */
#if GNUC_PREREQ (4, 7, 0) && ! GLIB_CHECK_VERSION (2, 35, 7)
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wunused-local-typedefs"
#endif

typedef struct _EmacsFixed EmacsFixed;
typedef struct _EmacsFixedClass EmacsFixedClass;

struct _EmacsFixedPrivate
{
  struct frame *f;
};


static void emacs_fixed_get_preferred_width  (GtkWidget *widget,
                                              gint      *minimum,
                                              gint      *natural);
static void emacs_fixed_get_preferred_height (GtkWidget *widget,
                                              gint      *minimum,
                                              gint      *natural);
#ifndef HAVE_PGTK
static GType emacs_fixed_get_type (void);
#endif
G_DEFINE_TYPE (EmacsFixed, emacs_fixed, GTK_TYPE_FIXED)

static EmacsFixed *
EMACS_FIXED (GtkWidget *widget)
{
  return G_TYPE_CHECK_INSTANCE_CAST (widget, emacs_fixed_get_type (),
				     EmacsFixed);
}

static void
emacs_fixed_class_init (EmacsFixedClass *klass)
{
  GtkWidgetClass *widget_class;

  widget_class = (GtkWidgetClass *) klass;

  widget_class->get_preferred_width = emacs_fixed_get_preferred_width;
  widget_class->get_preferred_height = emacs_fixed_get_preferred_height;
  g_type_class_add_private (klass, sizeof (EmacsFixedPrivate));
}

static void
emacs_fixed_init (EmacsFixed *fixed)
{
  fixed->priv = G_TYPE_INSTANCE_GET_PRIVATE (fixed, emacs_fixed_get_type (),
                                             EmacsFixedPrivate);
  fixed->priv->f = 0;
}

GtkWidget *
emacs_fixed_new (struct frame *f)
{
  EmacsFixed *fixed = g_object_new (emacs_fixed_get_type (), NULL);
  EmacsFixedPrivate *priv = fixed->priv;
  priv->f = f;
  return GTK_WIDGET (fixed);
}

static void
emacs_fixed_get_preferred_width (GtkWidget *widget,
                                 gint      *minimum,
                                 gint      *natural)
{
  EmacsFixed *fixed = EMACS_FIXED (widget);
  EmacsFixedPrivate *priv = fixed->priv;
#ifdef HAVE_PGTK
  int w = priv->f->output_data.pgtk->size_hints.min_width;
  if (minimum) *minimum = w;
  if (natural) *natural = priv->f->output_data.pgtk->preferred_width;
#else
  int w = priv->f->output_data.x->size_hints.min_width;
  if (minimum) *minimum = w;
  if (natural) *natural = w;
#endif
}

static void
emacs_fixed_get_preferred_height (GtkWidget *widget,
                                  gint      *minimum,
                                  gint      *natural)
{
  EmacsFixed *fixed = EMACS_FIXED (widget);
  EmacsFixedPrivate *priv = fixed->priv;
#ifdef HAVE_PGTK
  int h = priv->f->output_data.pgtk->size_hints.min_height;
  if (minimum) *minimum = h;
  if (natural) *natural = priv->f->output_data.pgtk->preferred_height;
#else
  int h = priv->f->output_data.x->size_hints.min_height;
  if (minimum) *minimum = h;
  if (natural) *natural = h;
#endif
}


#ifndef HAVE_PGTK

/* Override the X function so we can intercept Gtk+ 3 calls.
   Use our values for min_width/height so that KDE don't freak out
   (Bug#8919), and so users can resize our frames as they wish.  */

void
XSetWMSizeHints (Display *d,
                 Window w,
                 XSizeHints *hints,
                 Atom prop)
{
  struct x_display_info *dpyinfo = x_display_info_for_display (d);
  struct frame *f = x_top_window_to_frame (dpyinfo, w);
  long data[18];
  data[0] = hints->flags;
  data[1] = hints->x;
  data[2] = hints->y;
  data[3] = hints->width;
  data[4] = hints->height;
  data[5] = hints->min_width;
  data[6] = hints->min_height;
  data[7] = hints->max_width;
  data[8] = hints->max_height;
  data[9] = hints->width_inc;
  data[10] = hints->height_inc;
  data[11] = hints->min_aspect.x;
  data[12] = hints->min_aspect.y;
  data[13] = hints->max_aspect.x;
  data[14] = hints->max_aspect.y;
  data[15] = hints->base_width;
  data[16] = hints->base_height;
  data[17] = hints->win_gravity;

  if ((hints->flags & PMinSize) && f)
    {
      /* Overriding the size hints with our own values of min_width
	 and min_height used to work, but these days just results in
	 frames resizing unpredictably and emitting GTK warnings while
	 Emacs fights with GTK over the size of the frame.  So instead
	 of doing that, just respect the hints set by GTK, but make
	 sure they are an integer multiple of the resize increments so
	 that bug#8919 stays fixed.  */

      /* int w = f->output_data.x->size_hints.min_width;
         int h = f->output_data.x->size_hints.min_height;

	 data[5] = w;
	 data[6] = h; */

      /* Make sure min_width and min_height are multiples of width_inc
	 and height_inc.  */

      if (hints->flags & PResizeInc)
	{
	  /* Some versions of GTK set PResizeInc even if the
	     increments are at their initial values.  */

	  if (hints->width_inc && data[5] % hints->width_inc)
	    data[5] += (hints->width_inc - (data[5] % hints->width_inc));
	  if (hints->height_inc && data[6] % hints->height_inc)
	    data[6] += (hints->height_inc - (data[6] % hints->height_inc));
	}
    }

  XChangeProperty (d, w, prop, XA_WM_SIZE_HINTS, 32, PropModeReplace,
		   (unsigned char *) data, 18);
}

/* Override this X11 function.
   This function is in the same X11 file as the one above.  So we must
   provide it also.  */

void
XSetWMNormalHints (Display *d, Window w, XSizeHints *hints)
{
  XSetWMSizeHints (d, w, hints, XA_WM_NORMAL_HINTS);
}

#endif
