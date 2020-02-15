/* Support for embedding graphical components in a buffer.

Copyright (C) 2011-2020 Free Software Foundation, Inc.

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

#ifndef XWIDGET_H_INCLUDED
#define XWIDGET_H_INCLUDED

#include "lisp.h"

struct glyph_matrix;
struct glyph_string;
struct xwidget;
struct xwidget_view;
struct window;

#ifdef HAVE_XWIDGETS
# include <gtk/gtk.h>

struct xwidget
{
  union vectorlike_header header;

  /* Auxiliary data.  */
  Lisp_Object plist;

  /* The widget type.  */
  Lisp_Object type;

  /* The buffer where the xwidget lives.  */
  Lisp_Object buffer;

  /* A title used for button labels, for instance.  */
  Lisp_Object title;

  /* Vector of currently executing scripts with callbacks.  */
  Lisp_Object script_callbacks;
  /* Here ends the Lisp part.  script_callbacks is the marker field.  */

  int height;
  int width;

  /* For offscreen widgets, unused if not osr.  */
  GtkWidget *widget_osr;
  GtkWidget *widgetwindow_osr;

  /* Kill silently if Emacs is exited.  */
  bool_bf kill_without_query : 1;
} GCALIGNED_STRUCT;

struct xwidget_view
{
  union vectorlike_header header;
  Lisp_Object model;
  Lisp_Object w;
  /* Here ends the lisp part.  "w" is the marker field.  */

  /* If touched by redisplay.  */
  bool redisplayed;

  /* The "live" instance isn't drawn.  */
  bool hidden;

  GtkWidget *widget;
  GtkWidget *widgetwindow;
  GtkWidget *emacswindow;
  int x;
  int y;
  int clip_right;
  int clip_bottom;
  int clip_top;
  int clip_left;

  long handler_id;
} GCALIGNED_STRUCT;
#endif

/* Test for xwidget pseudovector.  */
#define XWIDGETP(x) PSEUDOVECTORP (x, PVEC_XWIDGET)
#define XXWIDGET(a) (eassert (XWIDGETP (a)), \
		     XUNTAG (a, Lisp_Vectorlike, struct xwidget))

#define CHECK_XWIDGET(x) \
  CHECK_TYPE (XWIDGETP (x), Qxwidgetp, x)

/* Test for xwidget_view pseudovector.  */
#define XWIDGET_VIEW_P(x) PSEUDOVECTORP (x, PVEC_XWIDGET_VIEW)
#define XXWIDGET_VIEW(a) (eassert (XWIDGET_VIEW_P (a)), \
			  XUNTAG (a, Lisp_Vectorlike, struct xwidget_view))

#define CHECK_XWIDGET_VIEW(x) \
  CHECK_TYPE (XWIDGET_VIEW_P (x), Qxwidget_view_p, x)

#define XG_XWIDGET "emacs_xwidget"
#define XG_XWIDGET_VIEW "emacs_xwidget_view"

#ifdef HAVE_XWIDGETS
void syms_of_xwidget (void);
bool valid_xwidget_spec_p (Lisp_Object);
void xwidget_view_delete_all_in_window (struct window *);
void x_draw_xwidget_glyph_string (struct glyph_string *);
struct xwidget *lookup_xwidget (Lisp_Object spec);
void xwidget_end_redisplay (struct window *, struct glyph_matrix *);
void kill_buffer_xwidgets (Lisp_Object);
#else
INLINE_HEADER_BEGIN
INLINE void syms_of_xwidget (void) {}
INLINE bool valid_xwidget_spec_p (Lisp_Object obj) { return false; }
INLINE void xwidget_view_delete_all_in_window (struct window *w) {}
INLINE void x_draw_xwidget_glyph_string (struct glyph_string *s) { eassume (0); }
INLINE struct xwidget *lookup_xwidget (Lisp_Object obj) { eassume (0); }
INLINE void xwidget_end_redisplay (struct window *w, struct glyph_matrix *m) {}
INLINE void kill_buffer_xwidgets (Lisp_Object buf) {}
INLINE_HEADER_END
#endif

#endif /* XWIDGET_H_INCLUDED */
