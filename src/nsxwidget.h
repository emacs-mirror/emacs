/* Header for NS Cocoa part of xwidget and webkit widget.

Copyright (C) 2019-2026 Free Software Foundation, Inc.

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

#ifndef NSXWIDGET_H_INCLUDED
#define NSXWIDGET_H_INCLUDED

/* This file can be included from non-objc files through 'xwidget.h'.  */
#ifdef __OBJC__
#import <AppKit/NSView.h>
#endif

#include "dispextern.h"
#include "lisp.h"
#include "xwidget.h"

/* Functions for xwidget webkit.  */

bool nsxwidget_is_web_view (struct xwidget *xw);
Lisp_Object nsxwidget_webkit_uri (struct xwidget *xw);
Lisp_Object nsxwidget_webkit_title (struct xwidget *xw);
void nsxwidget_webkit_goto_uri (struct xwidget *xw, const char *uri);
void nsxwidget_webkit_goto_history (struct xwidget *xw, int rel_pos);
double nsxwidget_webkit_estimated_load_progress(struct xwidget *xw);
void nsxwidget_webkit_stop_loading (struct xwidget *xw);
void nsxwidget_webkit_zoom (struct xwidget *xw, double zoom_change);
void nsxwidget_webkit_execute_script (struct xwidget *xw, const char *script,
                                      Lisp_Object fun);

/* Functions for xwidget model.  */

#ifdef __OBJC__
@interface XwWindow : NSView
@property struct xwidget *xw;
@end
#endif

void nsxwidget_init (struct xwidget *xw);
void nsxwidget_kill (struct xwidget *xw);
void nsxwidget_resize (struct xwidget *xw);
Lisp_Object nsxwidget_get_size (struct xwidget *xw);

/* Functions for xwidget view.  */

#ifdef __OBJC__
@interface XvWindow : NSView
@property struct xwidget *xw;
@property struct xwidget_view *xv;
@end
#endif

void nsxwidget_init_view (struct xwidget_view *xv,
                          struct xwidget *xww,
                          struct glyph_string *s,
                          int x, int y);
void nsxwidget_delete_view (struct xwidget_view *xv);

void nsxwidget_show_view (struct xwidget_view *xv);
void nsxwidget_hide_view (struct xwidget_view *xv);
void nsxwidget_resize_view (struct xwidget_view *xv,
                            int widget, int height);

void nsxwidget_move_view (struct xwidget_view *xv, int x, int y);
void nsxwidget_move_widget_in_view (struct xwidget_view *xv, int x, int y);
void nsxwidget_set_needsdisplay (struct xwidget_view *xv);

#endif /* NSXWIDGET_H_INCLUDED */
