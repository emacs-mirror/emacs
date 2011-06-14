/* A Gtk Widget that inherits GtkFixed, but can be shrinked. 

Copyright (C) 2011  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include "emacsgtkfixed.h"


struct _EmacsFixedPrivate
{
  int minwidth, minheight;
};


static void emacs_fixed_get_preferred_width  (GtkWidget *widget,
                                              gint      *minimum,
                                              gint      *natural);
static void emacs_fixed_get_preferred_height (GtkWidget *widget,
                                              gint      *minimum,
                                              gint      *natural);
G_DEFINE_TYPE (EmacsFixed, emacs_fixed, GTK_TYPE_FIXED)

static void
emacs_fixed_class_init (EmacsFixedClass *klass)
{
  GtkWidgetClass *widget_class;
  GtkFixedClass *fixed_class;

  widget_class = (GtkWidgetClass*) klass;
  fixed_class = (GtkFixedClass*) klass;

  widget_class->get_preferred_width = emacs_fixed_get_preferred_width;
  widget_class->get_preferred_height = emacs_fixed_get_preferred_height;
  g_type_class_add_private (klass, sizeof (EmacsFixedPrivate));
}

static GType
emacs_fixed_child_type (GtkFixed *container)
{
  return GTK_TYPE_WIDGET;
}

static void
emacs_fixed_init (EmacsFixed *fixed)
{
  fixed->priv = G_TYPE_INSTANCE_GET_PRIVATE (fixed, EMACS_TYPE_FIXED,
                                             EmacsFixedPrivate);
  fixed->priv->minwidth = fixed->priv->minheight = 0;
}

/**
 * emacs_fixed_new:
 *
 * Creates a new #EmacsFixed.
 *
 * Returns: a new #EmacsFixed.
 */
GtkWidget*
emacs_fixed_new (void)
{
  return g_object_new (EMACS_TYPE_FIXED, NULL);
}

static GtkWidgetClass *
get_parent_class (EmacsFixed *fixed)
{
  EmacsFixedClass *klass = EMACS_FIXED_GET_CLASS (fixed);
  GtkFixedClass *parent_class = g_type_class_peek_parent (klass);
  return (GtkWidgetClass*) parent_class;
}

static void
emacs_fixed_get_preferred_width (GtkWidget *widget,
                                 gint      *minimum,
                                 gint      *natural)
{
  EmacsFixed *fixed = EMACS_FIXED (widget);
  EmacsFixedPrivate *priv = fixed->priv;
  GtkWidgetClass *widget_class = get_parent_class (fixed);
  widget_class->get_preferred_width (widget, minimum, natural);
  if (minimum) *minimum = priv->minwidth;
}

static void
emacs_fixed_get_preferred_height (GtkWidget *widget,
                                  gint      *minimum,
                                  gint      *natural)
{
  EmacsFixed *fixed = EMACS_FIXED (widget);
  EmacsFixedPrivate *priv = fixed->priv;
  GtkWidgetClass *widget_class = get_parent_class (fixed);
  widget_class->get_preferred_height (widget, minimum, natural);
  if (minimum) *minimum = priv->minheight;
}

void
emacs_fixed_set_min_size (EmacsFixed *widget, int width, int height)
{
  EmacsFixedPrivate *priv = widget->priv;
  GtkWidgetClass *widget_class = get_parent_class (widget);
  int mw, nw, mh, nh;

  widget_class->get_preferred_height (GTK_WIDGET (widget), &mh, &nh);
  widget_class->get_preferred_width (GTK_WIDGET (widget), &mw, &nw);

  /* Gtk complains if min size is less than natural size.  */
  if (width <= nw) priv->minwidth = width;
  if (height <= nh) priv->minheight = height;
}
