/* Internals of a lightweight menubar widget.

Copyright (C) 2002-2023 Free Software Foundation, Inc.
Copyright (C) 1992 Lucid, Inc.

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

#ifndef _XlwMenuP_h
#define _XlwMenuP_h

#include "xlwmenu.h"
#include <X11/CoreP.h>
#if defined USE_CAIRO || defined HAVE_XFT
#ifdef USE_CAIRO
#include "lwlib-utils.h"
#else  /* HAVE_XFT */
#include <X11/Xft/Xft.h>
#endif
#endif

/* Elements in the stack arrays. */
typedef struct _window_state
{
  Widget        w;
  Window	window;
  Pixmap        pixmap;
  Position	x;
  Position	y;
  Dimension	width;
  Dimension	height;
  Dimension	label_width;
  int           max_rest_width;

  /* Width of toggle buttons or radio buttons.  */
  Dimension     button_width;
#if defined USE_CAIRO || defined HAVE_XFT
  XftDraw*      xft_draw;
#endif
} window_state;


/* New fields for the XlwMenu widget instance record */
typedef struct _XlwMenu_part
{
  /* slots set by the resources */
#ifdef HAVE_X_I18N
  XFontSet	fontSet;
  XFontSetExtents *font_extents;
#endif
#if defined USE_CAIRO || defined HAVE_XFT
  int           default_face;
  XftFont*      xft_font;
  XftColor      xft_fg, xft_bg, xft_disabled_fg, xft_highlight_fg;
#endif
  String	fontName;
  XFontStruct*	font;
  Pixel		foreground;
  Pixel		disabled_foreground;
  Pixel		button_foreground;
  Pixel		highlight_foreground;
  Pixel		highlight_background;
  Dimension	margin;
  Dimension	horizontal_spacing;
  Dimension	vertical_spacing;
  Dimension	arrow_spacing;
  Dimension	shadow_thickness;
  Dimension	border_thickness;
  Pixel 	top_shadow_color;
  Pixel 	bottom_shadow_color;
  Pixmap	top_shadow_pixmap;
  Pixmap	bottom_shadow_pixmap;
  Pixel 	top_highlight_shadow_color;
  Pixel 	bottom_highlight_shadow_color;
  Pixmap	top_highlight_shadow_pixmap;
  Pixmap	bottom_highlight_shadow_pixmap;
  Cursor	cursor_shape;
  XtCallbackList	open;
  XtCallbackList	select, highlight;
  XtCallbackList        enter, leave;
  widget_value*	contents;
  int		horizontal;

  /* True means top_shadow_color and/or bottom_shadow_color must be freed.  */
  Boolean free_top_shadow_color_p;
  Boolean free_bottom_shadow_color_p;
  Boolean free_top_highlight_shadow_color_p;
  Boolean free_bottom_highlight_shadow_color_p;

  /* State of the XlwMenu */
  int                   top_depth;
  int			old_depth;
  widget_value**	old_stack;
  int			old_stack_length;
  widget_value*         inside_entry;

  /* New state after the user moved */
  int			new_depth;
  widget_value**	new_stack;
  int			new_stack_length;

  /* Window resources */
  window_state*		windows;
  int			windows_length;

  /* Internal part, set by the XlwMenu */
  GC			foreground_gc;
  GC			button_gc;
  GC			background_gc;
  GC			disabled_gc;
  GC			highlight_foreground_gc;
  GC			highlight_background_gc;
  GC			inactive_button_gc;
  GC			shadow_top_gc;
  GC			shadow_bottom_gc;
  GC			highlight_shadow_top_gc;
  GC			highlight_shadow_bottom_gc;
  Cursor		cursor;
  Boolean		popped_up;
  Pixmap		gray_pixmap;
} XlwMenuPart;

/* Full instance record declaration */
typedef struct _XlwMenuRec
{
  CorePart	core;
  XlwMenuPart	menu;
} XlwMenuRec;

/* New fields for the XlwMenu widget class record */
typedef struct
{
  int	dummy;
} XlwMenuClassPart;

/* Full class record declaration. */
typedef struct _XlwMenuClassRec
{
  CoreClassPart		core_class;
  XlwMenuClassPart	menu_class;
} XlwMenuClassRec;

/* Class pointer. */
extern XlwMenuClassRec xlwMenuClassRec;

#endif /* _XlwMenuP_h */
