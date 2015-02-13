/* Support for embedding graphical components in a buffer.

Copyright (C) 2011-2015 Free Software Foundation, Inc.

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

#ifndef XWIDGET_H_INCLUDED
#define XWIDGET_H_INCLUDED

void x_draw_xwidget_glyph_string (struct glyph_string *s);
void syms_of_xwidget (void);

//extern Lisp_Object Qxwidget;


bool valid_xwidget_spec_p (Lisp_Object object);

#include <gtk/gtk.h>


/*
each xwidget instance/model is described by this struct.

lisp pseudovector.


 */
struct xwidget
{
  struct vectorlike_header header;
  Lisp_Object plist;		//auxilliary data
  Lisp_Object type;		//the widget type
  Lisp_Object buffer;		//buffer where xwidget lives
  Lisp_Object title;		//a title that is used for button labels for instance

  //here ends the lisp part.
  //"height" is the marker field
  int height;
  int width;

  //for offscreen widgets, unused if not osr
  GtkWidget *widget_osr;
  GtkWidget *widgetwindow_osr;
  //this is used if the widget (webkit) is to be wrapped in a scrolled window,
  GtkWidget *widgetscrolledwindow_osr;
  /* Non-nil means kill silently if Emacs is exited. */
  unsigned int kill_without_query:1;

};


//struct for each xwidget view
struct xwidget_view
{
  struct vectorlike_header header;
  Lisp_Object model;
  Lisp_Object w;

  //here ends the lisp part.
  //"redisplayed" is the marker field
  int redisplayed;		//if touched by redisplay

  int hidden;			//if the "live" instance isnt drawn

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
};

/* Test for xwidget pseudovector*/
#define XWIDGETP(x) PSEUDOVECTORP (x, PVEC_XWIDGET)
#define XXWIDGET(a) (eassert (XWIDGETP(a)), \
                     (struct xwidget *) XUNTAG(a, Lisp_Vectorlike))

#define CHECK_XWIDGET(x) \
  CHECK_TYPE (XWIDGETP (x), Qxwidgetp, x)

/* Test for xwidget_view pseudovector */
#define XWIDGET_VIEW_P(x) PSEUDOVECTORP (x, PVEC_XWIDGET_VIEW)
#define XXWIDGET_VIEW(a) (eassert (XWIDGET_VIEW_P(a)), \
                          (struct xwidget_view *) XUNTAG(a, Lisp_Vectorlike))

#define CHECK_XWIDGET_VIEW(x) \
  CHECK_TYPE (XWIDGET_VIEW_P (x), Qxwidget_view_p, x)

struct xwidget_type
{
  /* A symbol uniquely identifying the xwidget type, */
  Lisp_Object *type;

  /* Check that SPEC is a valid image specification for the given
     image type.  Value is non-zero if SPEC is valid.  */
  int (*valid_p) (Lisp_Object spec);

  /* Next in list of all supported image types.  */
  struct xwidget_type *next;
};


struct xwidget *xwidget_from_id (int id);

void xwidget_start_redisplay (void);
void xwidget_end_redisplay (struct window *w, struct glyph_matrix *matrix);

void xwidget_touch (struct xwidget_view *xw);

struct xwidget *lookup_xwidget (Lisp_Object spec);
#define XG_XWIDGET "emacs_xwidget"
#define XG_XWIDGET_VIEW "emacs_xwidget_view"
void xwidget_view_delete_all_in_window (struct window *w);

void kill_buffer_xwidgets (Lisp_Object buffer);
#endif /* XWIDGET_H_INCLUDED */
