#ifndef _XlwMenuP_h
#define _XlwMenuP_h

#include "xlwmenu.h"
#include <X11/CoreP.h>

/* Elements in the stack arrays. */
typedef struct _window_state
{
  Window	window;
  Position	x;
  Position	y;
  Dimension	width;
  Dimension	height;
  Dimension	label_width;

  /* Width of toggle buttons or radio buttons.  */
  Dimension     button_width;
} window_state;


/* New fields for the XlwMenu widget instance record */
typedef struct _XlwMenu_part
{
  /* slots set by the resources */
  XFontStruct*	font;
  Pixel		foreground;
  Pixel		disabled_foreground;
  Pixel		button_foreground;
  Dimension	margin;
  Dimension	horizontal_spacing;
  Dimension	vertical_spacing;
  Dimension	arrow_spacing;
  Dimension	shadow_thickness;
  Pixel 	top_shadow_color;
  Pixel 	bottom_shadow_color;
  Pixmap	top_shadow_pixmap;
  Pixmap	bottom_shadow_pixmap;
  Cursor	cursor_shape;
  XtCallbackList	open;
  XtCallbackList	select, highlight;
  widget_value*	contents;
  int		horizontal;

  /* True means top_shadow_color and/or bottom_shadow_color must be freed.  */
  unsigned free_top_shadow_color_p : 1;
  unsigned free_bottom_shadow_color_p : 1;

  /* State of the XlwMenu */
  int			old_depth;
  widget_value**	old_stack;
  int			old_stack_length;

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
  GC			inactive_button_gc;
  GC			shadow_top_gc;
  GC			shadow_bottom_gc;
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
