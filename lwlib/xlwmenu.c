/* Implements a lightweight menubar widget.
   Copyright (C) 1992 Lucid, Inc.
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Created by devin@lucid.com */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "../src/lisp.h"

#include <stdio.h>

#include <sys/types.h>
#if (defined __sun) && !(defined SUNOS41)
#define SUNOS41
#include <X11/Xos.h>
#undef SUNOS41
#else
#include <X11/Xos.h>
#endif
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include "xlwmenuP.h"

#ifdef emacs

/* Defined in xfns.c.  When config.h defines `static' as empty, we get
   redefinition errors when gray_bitmap is included more than once, so
   we're referring to the one include in xfns.c here.  */

extern int gray_bitmap_width;
extern int gray_bitmap_height;
extern char *gray_bitmap_bits;

/* Defined in xterm.c.  */
extern int x_alloc_nearest_color_for_widget __P ((Widget, Colormap, XColor*));
extern int x_alloc_lighter_color_for_widget __P ((Widget, Display*, Colormap,
						  unsigned long *,
						  double, int));
extern int x_catch_errors __P ((Display*));
extern int x_uncatch_errors __P ((Display*, int));
extern int x_had_errors_p __P ((Display*));
extern int x_clear_errors __P ((Display*));
extern unsigned long x_copy_dpy_color __P ((Display *, Colormap,
					    unsigned long));

/* Defined in xfaces.c.  */
extern void x_free_dpy_colors __P ((Display *, Screen *, Colormap,
				    unsigned long *pixels, int npixels));
#else /* not emacs */

#include <X11/bitmaps/gray>
#define gray_bitmap_width	gray_width
#define gray_bitmap_height	gray_height
#define gray_bitmap_bits	gray_bits

#endif /* not emacs */

static int pointer_grabbed;
static XEvent menu_post_event;

XFontStruct *xlwmenu_default_font;

static char
xlwMenuTranslations [] =
"<BtnDown>:	  start()\n\
<Motion>:	  drag()\n\
<BtnUp>:	  select()\n\
<Key>Shift_L:     nothing()\n\
<Key>Shift_R:     nothing()\n\
<Key>Meta_L:      nothing()\n\
<Key>Meta_R:      nothing()\n\
<Key>Control_L:   nothing()\n\
<Key>Control_R:   nothing()\n\
<Key>Hyper_L:     nothing()\n\
<Key>Hyper_R:     nothing()\n\
<Key>Super_L:     nothing()\n\
<Key>Super_R:     nothing()\n\
<Key>Alt_L:       nothing()\n\
<Key>Alt_R:       nothing()\n\
<Key>Caps_Lock:   nothing()\n\
<Key>Shift_Lock:  nothing()\n\
<KeyUp>Shift_L:   nothing()\n\
<KeyUp>Shift_R:   nothing()\n\
<KeyUp>Meta_L:    nothing()\n\
<KeyUp>Meta_R:    nothing()\n\
<KeyUp>Control_L: nothing()\n\
<KeyUp>Control_R: nothing()\n\
<KeyUp>Hyper_L:   nothing()\n\
<KeyUp>Hyper_R:   nothing()\n\
<KeyUp>Super_L:   nothing()\n\
<KeyUp>Super_R:   nothing()\n\
<KeyUp>Alt_L:     nothing()\n\
<KeyUp>Alt_R:     nothing()\n\
<KeyUp>Caps_Lock: nothing()\n\
<KeyUp>Shift_Lock:nothing()\n\
<Key>Return:      select()\n\
<Key>Down:        down()\n\
<Key>Up:          up()\n\
<Key>Left:        left()\n\
<Key>Right:       right()\n\
<Key>:            key()\n\
<KeyUp>:          key()\n\
";

/* FIXME: Space should toggle toggleable menu item but not remove the menu
   so you can toggle the next one without entering the menu again.  */

/* FIXME: Should ESC close one level of menu structure or the complete menu?  */

/* FIXME: F10 should enter the menu, the first one in the menu-bar.  */

#define offset(field) XtOffset(XlwMenuWidget, field)
static XtResource
xlwMenuResources[] =
{
  {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(menu.font),XtRString, "XtDefaultFont"},
  {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(menu.foreground), XtRString, "XtDefaultForeground"},
  {XtNdisabledForeground, XtCDisabledForeground, XtRPixel, sizeof(Pixel),
   offset(menu.disabled_foreground), XtRString, (XtPointer)NULL},
  {XtNbuttonForeground, XtCButtonForeground, XtRPixel, sizeof(Pixel),
     offset(menu.button_foreground), XtRString, "XtDefaultForeground"},
  {XtNmargin, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.margin), XtRImmediate, (XtPointer)1},
  {XtNhorizontalSpacing, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.horizontal_spacing), XtRImmediate, (XtPointer)3},
  {XtNverticalSpacing, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.vertical_spacing), XtRImmediate, (XtPointer)2},
  {XtNarrowSpacing, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.arrow_spacing), XtRImmediate, (XtPointer)10},

  {XmNshadowThickness, XmCShadowThickness, XtRDimension,
     sizeof (Dimension), offset (menu.shadow_thickness),
     XtRImmediate, (XtPointer)1},
  {XmNtopShadowColor, XmCTopShadowColor, XtRPixel, sizeof (Pixel),
     offset (menu.top_shadow_color), XtRImmediate, (XtPointer)-1},
  {XmNbottomShadowColor, XmCBottomShadowColor, XtRPixel, sizeof (Pixel),
     offset (menu.bottom_shadow_color), XtRImmediate, (XtPointer)-1},
  {XmNtopShadowPixmap, XmCTopShadowPixmap, XtRPixmap, sizeof (Pixmap),
     offset (menu.top_shadow_pixmap), XtRImmediate, (XtPointer)None},
  {XmNbottomShadowPixmap, XmCBottomShadowPixmap, XtRPixmap, sizeof (Pixmap),
     offset (menu.bottom_shadow_pixmap), XtRImmediate, (XtPointer)None},

  {XtNopen, XtCCallback, XtRCallback, sizeof(XtPointer),
     offset(menu.open), XtRCallback, (XtPointer)NULL},
  {XtNselect, XtCCallback, XtRCallback, sizeof(XtPointer),
     offset(menu.select), XtRCallback, (XtPointer)NULL},
  {XtNhighlightCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
     offset(menu.highlight), XtRCallback, (XtPointer)NULL},
  {XtNmenu, XtCMenu, XtRPointer, sizeof(XtPointer),
     offset(menu.contents), XtRImmediate, (XtPointer)NULL},
  {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(menu.cursor_shape), XtRString, (XtPointer)"right_ptr"},
  {XtNhorizontal, XtCHorizontal, XtRInt, sizeof(int),
     offset(menu.horizontal), XtRImmediate, (XtPointer)True},
};
#undef offset

static Boolean XlwMenuSetValues();
static void XlwMenuRealize();
static void XlwMenuRedisplay();
static void XlwMenuResize();
static void XlwMenuInitialize();
static void XlwMenuRedisplay();
static void XlwMenuDestroy();
static void XlwMenuClassInitialize();
static void Start();
static void Drag();
static void Down();
static void Up();
static void Left();
static void Right();
static void Select();
static void Key();
static void Nothing();
static int separator_height ();

static XtActionsRec
xlwMenuActionsList [] =
{
  {"start",		Start},
  {"drag",		Drag},
  {"down",		Down},
  {"up",		Up},
  {"left",		Left},
  {"right",		Right},
  {"select",		Select},
  {"key",		Key},
  {"nothing",		Nothing},
};

#define SuperClass ((CoreWidgetClass)&coreClassRec)

XlwMenuClassRec xlwMenuClassRec =
{
  {  /* CoreClass fields initialization */
    (WidgetClass) SuperClass,		/* superclass		  */
    "XlwMenu",				/* class_name		  */
    sizeof(XlwMenuRec),			/* size			  */
    XlwMenuClassInitialize,		/* class_initialize	  */
    NULL,				/* class_part_initialize  */
    FALSE,				/* class_inited		  */
    XlwMenuInitialize,			/* initialize		  */
    NULL,				/* initialize_hook	  */
    XlwMenuRealize,			/* realize		  */
    xlwMenuActionsList,			/* actions		  */
    XtNumber(xlwMenuActionsList),	/* num_actions		  */
    xlwMenuResources,			/* resources		  */
    XtNumber(xlwMenuResources),		/* resource_count	  */
    NULLQUARK,				/* xrm_class		  */
    TRUE,				/* compress_motion	  */
    TRUE,				/* compress_exposure	  */
    TRUE,				/* compress_enterleave    */
    FALSE,				/* visible_interest	  */
    XlwMenuDestroy,			/* destroy		  */
    XlwMenuResize,			/* resize		  */
    XlwMenuRedisplay,			/* expose		  */
    XlwMenuSetValues,			/* set_values		  */
    NULL,				/* set_values_hook	  */
    XtInheritSetValuesAlmost,		/* set_values_almost	  */
    NULL,				/* get_values_hook	  */
    NULL,				/* accept_focus		  */
    XtVersion,				/* version		  */
    NULL,				/* callback_private	  */
    xlwMenuTranslations,		/* tm_table		  */
    XtInheritQueryGeometry,		/* query_geometry	  */
    XtInheritDisplayAccelerator,	/* display_accelerator	  */
    NULL				/* extension		  */
  },  /* XlwMenuClass fields initialization */
  {
    0					/* dummy */
  },
};

WidgetClass xlwMenuWidgetClass = (WidgetClass) &xlwMenuClassRec;

int submenu_destroyed;

/* For debug, if installation-directory is non-nil this is not an installed
   Emacs.   In that case we do not grab the keyboard to make it easier to
   debug. */
#define GRAB_KEYBOARD  (EQ (Vinstallation_directory, Qnil))

static int next_release_must_exit;

/* Utilities */

/* Ungrab pointer and keyboard */
static void
ungrab_all (w, ungrabtime)
     Widget w;
     Time ungrabtime;
{
  XtUngrabPointer (w, ungrabtime);
  if (GRAB_KEYBOARD) XtUngrabKeyboard (w, ungrabtime);
}

/* Like abort, but remove grabs from widget W before.  */

static void
abort_gracefully (w)
     Widget w;
{
  if (XtIsShell (XtParent (w)))
    XtRemoveGrab (w);
  ungrab_all (w, CurrentTime);
  abort ();
}

static void
push_new_stack (mw, val)
     XlwMenuWidget mw;
     widget_value* val;
{
  if (!mw->menu.new_stack)
    {
      mw->menu.new_stack_length = 10;
      mw->menu.new_stack =
	(widget_value**)XtCalloc (mw->menu.new_stack_length,
				  sizeof (widget_value*));
    }
  else if (mw->menu.new_depth == mw->menu.new_stack_length)
    {
      mw->menu.new_stack_length *= 2;
      mw->menu.new_stack =
	(widget_value**)XtRealloc ((char*)mw->menu.new_stack,
				   mw->menu.new_stack_length * sizeof (widget_value*));
    }
  mw->menu.new_stack [mw->menu.new_depth++] = val;
}

static void
pop_new_stack_if_no_contents (mw)
     XlwMenuWidget mw;
{
  if (mw->menu.new_depth)
    {
      if (!mw->menu.new_stack [mw->menu.new_depth - 1]->contents)
	mw->menu.new_depth -= 1;
    }
}

static void
make_old_stack_space (mw, n)
     XlwMenuWidget mw;
     int n;
{
  if (!mw->menu.old_stack)
    {
      mw->menu.old_stack_length = 10;
      mw->menu.old_stack =
	(widget_value**)XtCalloc (mw->menu.old_stack_length,
				  sizeof (widget_value*));
    }
  else if (mw->menu.old_stack_length < n)
    {
      mw->menu.old_stack_length *= 2;
      mw->menu.old_stack =
	(widget_value**)XtRealloc ((char*)mw->menu.old_stack,
				   mw->menu.old_stack_length * sizeof (widget_value*));
    }
}

/* Size code */
int
string_width (mw, s)
     XlwMenuWidget mw;
     char *s;
{
  XCharStruct xcs;
  int drop;

  XTextExtents (mw->menu.font, s, strlen (s), &drop, &drop, &drop, &xcs);
  return xcs.width;
}

static int
arrow_width (mw)
     XlwMenuWidget mw;
{
  return (mw->menu.font->ascent * 3/4) | 1;
}

/* Return the width of toggle buttons of widget MW.  */

static int
toggle_button_width (mw)
     XlwMenuWidget mw;
{
  return ((mw->menu.font->ascent + mw->menu.font->descent) * 2 / 3) | 1;
}


/* Return the width of radio buttons of widget MW.  */

static int
radio_button_width (mw)
     XlwMenuWidget mw;
{
  return toggle_button_width (mw) * 1.41;
}


static XtResource
nameResource[] =
{
  {"labelString",  "LabelString", XtRString, sizeof(String),
     0, XtRImmediate, 0},
};

static char*
resource_widget_value (mw, val)
     XlwMenuWidget mw;
     widget_value *val;
{
  if (!val->toolkit_data)
    {
      char* resourced_name = NULL;
      char* complete_name;
      XtGetSubresources ((Widget) mw,
			 (XtPointer) &resourced_name,
			 val->name, val->name,
			 nameResource, 1, NULL, 0);
      if (!resourced_name)
	resourced_name = val->name;
      if (!val->value)
	{
	  complete_name = (char *) XtMalloc (strlen (resourced_name) + 1);
	  strcpy (complete_name, resourced_name);
	}
      else
	{
	  int complete_length =
	    strlen (resourced_name) + strlen (val->value) + 2;
	  complete_name = XtMalloc (complete_length);
	  *complete_name = 0;
	  strcat (complete_name, resourced_name);
	  strcat (complete_name, " ");
	  strcat (complete_name, val->value);
	}

      val->toolkit_data = complete_name;
      val->free_toolkit_data = True;
    }
  return (char*)val->toolkit_data;
}

/* Returns the sizes of an item */
static void
size_menu_item (mw, val, horizontal_p, label_width, rest_width, button_width,
		height)
     XlwMenuWidget mw;
     widget_value* val;
     int horizontal_p;
     int* label_width;
     int* rest_width;
     int* button_width;
     int* height;
{
  enum menu_separator separator;

  if (lw_separator_p (val->name, &separator, 0))
    {
      *height = separator_height (separator);
      *label_width = 1;
      *rest_width = 0;
      *button_width = 0;
    }
  else
    {
      *height =
	mw->menu.font->ascent + mw->menu.font->descent
	  + 2 * mw->menu.vertical_spacing + 2 * mw->menu.shadow_thickness;

      *label_width =
	string_width (mw, resource_widget_value (mw, val))
	  + mw->menu.horizontal_spacing + mw->menu.shadow_thickness;

      *rest_width =  mw->menu.horizontal_spacing + mw->menu.shadow_thickness;
      if (!horizontal_p)
	{
	  if (val->contents)
	    /* Add width of the arrow displayed for submenus.  */
	    *rest_width += arrow_width (mw) + mw->menu.arrow_spacing;
	  else if (val->key)
	    /* Add width of key equivalent string.  */
	    *rest_width += (string_width (mw, val->key)
			    + mw->menu.arrow_spacing);

	  if (val->button_type == BUTTON_TYPE_TOGGLE)
	    *button_width = (toggle_button_width (mw)
			     + mw->menu.horizontal_spacing);
	  else if (val->button_type == BUTTON_TYPE_RADIO)
	    *button_width = (radio_button_width (mw)
			     + mw->menu.horizontal_spacing);
	}
    }
}

static void
size_menu (mw, level)
     XlwMenuWidget mw;
     int level;
{
  unsigned int  label_width = 0;
  int		rest_width = 0;
  int		button_width = 0;
  int		max_rest_width = 0;
  int		max_button_width = 0;
  unsigned int  height = 0;
  int		horizontal_p = mw->menu.horizontal && (level == 0);
  widget_value*	val;
  window_state*	ws;

  if (level >= mw->menu.old_depth)
    abort_gracefully ((Widget) mw);

  ws = &mw->menu.windows [level];
  ws->width = 0;
  ws->height = 0;
  ws->label_width = 0;
  ws->button_width = 0;

  for (val = mw->menu.old_stack [level]->contents; val; val = val->next)
    {
      size_menu_item (mw, val, horizontal_p, &label_width, &rest_width,
		      &button_width, &height);
      if (horizontal_p)
	{
	  ws->width += label_width + rest_width;
	  if (height > ws->height)
	    ws->height = height;
	}
      else
	{
	  if (label_width > ws->label_width)
	    ws->label_width = label_width;
	  if (rest_width > max_rest_width)
	    max_rest_width = rest_width;
	  if (button_width > max_button_width)
	    max_button_width = button_width;
	  ws->height += height;
	}
    }

  if (horizontal_p)
    ws->label_width = ws->button_width = 0;
  else
    {
      ws->width = ws->label_width + max_rest_width + max_button_width;
      ws->button_width = max_button_width;
    }

  ws->width += 2 * mw->menu.shadow_thickness;
  ws->height += 2 * mw->menu.shadow_thickness;

  if (horizontal_p)
    {
      ws->width += 2 * mw->menu.margin;
      ws->height += 2 * mw->menu.margin;
    }
}


/* Display code */

static void
draw_arrow (mw, window, gc, x, y, width, down_p)
     XlwMenuWidget mw;
     Window window;
     GC gc;
     int x;
     int y;
     int width;
     int down_p;
{
  Display *dpy = XtDisplay (mw);
  GC top_gc = mw->menu.shadow_top_gc;
  GC bottom_gc = mw->menu.shadow_bottom_gc;
  int thickness = mw->menu.shadow_thickness;
  int height = width;
  XPoint pt[10];
  /* alpha = atan (0.5)
     factor = (1 + sin (alpha)) / cos (alpha) */
  double factor = 1.62;
  int thickness2 = thickness * factor;

  y += (mw->menu.font->ascent + mw->menu.font->descent - height) / 2;

  if (down_p)
    {
      GC temp;
      temp = top_gc;
      top_gc = bottom_gc;
      bottom_gc = temp;
    }

  pt[0].x = x;
  pt[0].y = y + height;
  pt[1].x = x + thickness;
  pt[1].y = y + height - thickness2;
  pt[2].x = x + thickness2;
  pt[2].y = y + thickness2;
  pt[3].x = x;
  pt[3].y = y;
  XFillPolygon (dpy, window, top_gc, pt, 4, Convex, CoordModeOrigin);

  pt[0].x = x;
  pt[0].y = y;
  pt[1].x = x + thickness;
  pt[1].y = y + thickness2;
  pt[2].x = x + width - thickness2;
  pt[2].y = y + height / 2;
  pt[3].x = x + width;
  pt[3].y = y + height / 2;
  XFillPolygon (dpy, window, top_gc, pt, 4, Convex, CoordModeOrigin);

  pt[0].x = x;
  pt[0].y = y + height;
  pt[1].x = x + thickness;
  pt[1].y = y + height - thickness2;
  pt[2].x = x + width - thickness2;
  pt[2].y = y + height / 2;
  pt[3].x = x + width;
  pt[3].y = y + height / 2;
  XFillPolygon (dpy, window, bottom_gc, pt, 4, Convex, CoordModeOrigin);
}



static void
draw_shadow_rectangle (mw, window, x, y, width, height, erase_p, down_p)
     XlwMenuWidget mw;
     Window window;
     int x;
     int y;
     int width;
     int height;
     int erase_p;
     int down_p;
{
  Display *dpy = XtDisplay (mw);
  GC top_gc = !erase_p ? mw->menu.shadow_top_gc : mw->menu.background_gc;
  GC bottom_gc = !erase_p ? mw->menu.shadow_bottom_gc : mw->menu.background_gc;
  int thickness = mw->menu.shadow_thickness;
  XPoint points [4];

  if (!erase_p && down_p)
    {
      GC temp;
      temp = top_gc;
      top_gc = bottom_gc;
      bottom_gc = temp;
    }

  points [0].x = x;
  points [0].y = y;
  points [1].x = x + width;
  points [1].y = y;
  points [2].x = x + width - thickness;
  points [2].y = y + thickness;
  points [3].x = x;
  points [3].y = y + thickness;
  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x;
  points [0].y = y + thickness;
  points [1].x = x;
  points [1].y = y + height;
  points [2].x = x + thickness;
  points [2].y = y + height - thickness;
  points [3].x = x + thickness;
  points [3].y = y + thickness;
  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x + width;
  points [0].y = y;
  points [1].x = x + width - thickness;
  points [1].y = y + thickness;
  points [2].x = x + width - thickness;
  points [2].y = y + height - thickness;
  points [3].x = x + width;
  points [3].y = y + height - thickness;
  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x;
  points [0].y = y + height;
  points [1].x = x + width;
  points [1].y = y + height;
  points [2].x = x + width;
  points [2].y = y + height - thickness;
  points [3].x = x + thickness;
  points [3].y = y + height - thickness;
  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);
}


static void
draw_shadow_rhombus (mw, window, x, y, width, height, erase_p, down_p)
     XlwMenuWidget mw;
     Window window;
     int x;
     int y;
     int width;
     int height;
     int erase_p;
     int down_p;
{
  Display *dpy = XtDisplay (mw);
  GC top_gc = !erase_p ? mw->menu.shadow_top_gc : mw->menu.background_gc;
  GC bottom_gc = !erase_p ? mw->menu.shadow_bottom_gc : mw->menu.background_gc;
  int thickness = mw->menu.shadow_thickness;
  XPoint points [4];

  if (!erase_p && down_p)
    {
      GC temp;
      temp = top_gc;
      top_gc = bottom_gc;
      bottom_gc = temp;
    }

  points [0].x = x;
  points [0].y = y + height / 2;
  points [1].x = x + thickness;
  points [1].y = y + height / 2;
  points [2].x = x + width / 2;
  points [2].y = y + thickness;
  points [3].x = x + width / 2;
  points [3].y = y;
  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x + width / 2;
  points [0].y = y;
  points [1].x = x + width / 2;
  points [1].y = y + thickness;
  points [2].x = x + width - thickness;
  points [2].y = y + height / 2;
  points [3].x = x + width;
  points [3].y = y + height / 2;
  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x;
  points [0].y = y + height / 2;
  points [1].x = x + thickness;
  points [1].y = y + height / 2;
  points [2].x = x + width / 2;
  points [2].y = y + height - thickness;
  points [3].x = x + width / 2;
  points [3].y = y + height;
  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x + width / 2;
  points [0].y = y + height;
  points [1].x = x + width / 2;
  points [1].y = y + height - thickness;
  points [2].x = x + width - thickness;
  points [2].y = y + height / 2;
  points [3].x = x + width;
  points [3].y = y + height / 2;
  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);
}


/* Draw a toggle button on widget MW, X window WINDOW.  X/Y is the
   top-left corner of the menu item.  SELECTED_P non-zero means the
   toggle button is selected.  */

static void
draw_toggle (mw, window, x, y, selected_p)
     XlwMenuWidget mw;
     Window window;
     int x, y, selected_p;
{
  int width, height;

  width = toggle_button_width (mw);
  height = width;
  x += mw->menu.horizontal_spacing;
  y += (mw->menu.font->ascent - height) / 2;
  draw_shadow_rectangle (mw, window, x, y, width, height, False, selected_p);
}


/* Draw a radio button on widget MW, X window WINDOW.  X/Y is the
   top-left corner of the menu item.  SELECTED_P non-zero means the
   toggle button is selected.  */

static void
draw_radio (mw, window, x, y, selected_p)
     XlwMenuWidget mw;
     Window window;
     int x, y, selected_p;
{
  int width, height;

  width = radio_button_width (mw);
  height = width;
  x += mw->menu.horizontal_spacing;
  y += (mw->menu.font->ascent - height) / 2;
  draw_shadow_rhombus (mw, window, x, y, width, height, False, selected_p);
}


/* Draw a menu separator on widget MW, X window WINDOW.  X/Y is the
   top-left corner of the menu item.  WIDTH is the width of the
   separator to draw.  TYPE is the separator type.  */

static void
draw_separator (mw, window, x, y, width, type)
     XlwMenuWidget mw;
     Window window;
     int x, y, width;
     enum menu_separator type;
{
  Display *dpy = XtDisplay (mw);
  XGCValues xgcv;

  switch (type)
    {
    case SEPARATOR_NO_LINE:
      break;

    case SEPARATOR_SINGLE_LINE:
      XDrawLine (dpy, window, mw->menu.foreground_gc,
		 x, y, x + width, y);
      break;

    case SEPARATOR_DOUBLE_LINE:
      draw_separator (mw, window, x, y, width, SEPARATOR_SINGLE_LINE);
      draw_separator (mw, window, x, y + 2, width, SEPARATOR_SINGLE_LINE);
      break;

    case SEPARATOR_SINGLE_DASHED_LINE:
      xgcv.line_style = LineOnOffDash;
      XChangeGC (dpy, mw->menu.foreground_gc, GCLineStyle, &xgcv);
      XDrawLine (dpy, window, mw->menu.foreground_gc,
		 x, y, x + width, y);
      xgcv.line_style = LineSolid;
      XChangeGC (dpy, mw->menu.foreground_gc, GCLineStyle, &xgcv);
      break;

    case SEPARATOR_DOUBLE_DASHED_LINE:
      draw_separator (mw, window, x, y, width,
		      SEPARATOR_SINGLE_DASHED_LINE);
      draw_separator (mw, window, x, y + 2, width,
		      SEPARATOR_SINGLE_DASHED_LINE);
      break;

    case SEPARATOR_SHADOW_ETCHED_IN:
      XDrawLine (dpy, window, mw->menu.shadow_bottom_gc,
		 x, y, x + width, y);
      XDrawLine (dpy, window, mw->menu.shadow_top_gc,
		 x, y + 1, x + width, y + 1);
      break;

    case SEPARATOR_SHADOW_ETCHED_OUT:
      XDrawLine (dpy, window, mw->menu.shadow_top_gc,
		 x, y, x + width, y);
      XDrawLine (dpy, window, mw->menu.shadow_bottom_gc,
		 x, y + 1, x + width, y + 1);
      break;

    case SEPARATOR_SHADOW_ETCHED_IN_DASH:
      xgcv.line_style = LineOnOffDash;
      XChangeGC (dpy, mw->menu.shadow_bottom_gc, GCLineStyle, &xgcv);
      XChangeGC (dpy, mw->menu.shadow_top_gc, GCLineStyle, &xgcv);
      draw_separator (mw, window, x, y, width, SEPARATOR_SHADOW_ETCHED_IN);
      xgcv.line_style = LineSolid;
      XChangeGC (dpy, mw->menu.shadow_bottom_gc, GCLineStyle, &xgcv);
      XChangeGC (dpy, mw->menu.shadow_top_gc, GCLineStyle, &xgcv);
      break;

    case SEPARATOR_SHADOW_ETCHED_OUT_DASH:
      xgcv.line_style = LineOnOffDash;
      XChangeGC (dpy, mw->menu.shadow_bottom_gc, GCLineStyle, &xgcv);
      XChangeGC (dpy, mw->menu.shadow_top_gc, GCLineStyle, &xgcv);
      draw_separator (mw, window, x, y, width, SEPARATOR_SHADOW_ETCHED_OUT);
      xgcv.line_style = LineSolid;
      XChangeGC (dpy, mw->menu.shadow_bottom_gc, GCLineStyle, &xgcv);
      XChangeGC (dpy, mw->menu.shadow_top_gc, GCLineStyle, &xgcv);
      break;

    case SEPARATOR_SHADOW_DOUBLE_ETCHED_IN:
      draw_separator (mw, window, x, y, width, SEPARATOR_SHADOW_ETCHED_IN);
      draw_separator (mw, window, x, y + 3, width, SEPARATOR_SHADOW_ETCHED_IN);
      break;

    case SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT:
      draw_separator (mw, window, x, y, width,
		      SEPARATOR_SHADOW_ETCHED_OUT);
      draw_separator (mw, window, x, y + 3, width,
		      SEPARATOR_SHADOW_ETCHED_OUT);
      break;

    case SEPARATOR_SHADOW_DOUBLE_ETCHED_IN_DASH:
      xgcv.line_style = LineOnOffDash;
      XChangeGC (dpy, mw->menu.shadow_bottom_gc, GCLineStyle, &xgcv);
      XChangeGC (dpy, mw->menu.shadow_top_gc, GCLineStyle, &xgcv);
      draw_separator (mw, window, x, y, width,
		      SEPARATOR_SHADOW_DOUBLE_ETCHED_IN);
      xgcv.line_style = LineSolid;
      XChangeGC (dpy, mw->menu.shadow_bottom_gc, GCLineStyle, &xgcv);
      XChangeGC (dpy, mw->menu.shadow_top_gc, GCLineStyle, &xgcv);
      break;

    case SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT_DASH:
      xgcv.line_style = LineOnOffDash;
      XChangeGC (dpy, mw->menu.shadow_bottom_gc, GCLineStyle, &xgcv);
      XChangeGC (dpy, mw->menu.shadow_top_gc, GCLineStyle, &xgcv);
      draw_separator (mw, window, x, y, width,
		      SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT);
      xgcv.line_style = LineSolid;
      XChangeGC (dpy, mw->menu.shadow_bottom_gc, GCLineStyle, &xgcv);
      XChangeGC (dpy, mw->menu.shadow_top_gc, GCLineStyle, &xgcv);
      break;

    default:
      abort ();
    }
}


/* Return the pixel height of menu separator SEPARATOR.  */

static int
separator_height (separator)
     enum menu_separator separator;
{
  switch (separator)
    {
    case SEPARATOR_NO_LINE:
      return 2;

    case SEPARATOR_SINGLE_LINE:
    case SEPARATOR_SINGLE_DASHED_LINE:
      return 1;

    case SEPARATOR_DOUBLE_LINE:
    case SEPARATOR_DOUBLE_DASHED_LINE:
      return 3;

    case SEPARATOR_SHADOW_ETCHED_IN:
    case SEPARATOR_SHADOW_ETCHED_OUT:
    case SEPARATOR_SHADOW_ETCHED_IN_DASH:
    case SEPARATOR_SHADOW_ETCHED_OUT_DASH:
      return 2;

    case SEPARATOR_SHADOW_DOUBLE_ETCHED_IN:
    case SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT:
    case SEPARATOR_SHADOW_DOUBLE_ETCHED_IN_DASH:
    case SEPARATOR_SHADOW_DOUBLE_ETCHED_OUT_DASH:
      return 5;

    default:
      abort ();
    }
}


/* Display the menu item and increment where.x and where.y to show how large
   the menu item was.  */

static void
display_menu_item (mw, val, ws, where, highlighted_p, horizontal_p,
		   just_compute_p)
     XlwMenuWidget mw;
     widget_value* val;
     window_state* ws;
     XPoint* where;
     Boolean highlighted_p;
     Boolean horizontal_p;
     Boolean just_compute_p;
{
  GC deco_gc;
  GC text_gc;
  int font_ascent = mw->menu.font->ascent;
  int font_descent = mw->menu.font->descent;
  int shadow = mw->menu.shadow_thickness;
  int margin = mw->menu.margin;
  int h_spacing = mw->menu.horizontal_spacing;
  int v_spacing = mw->menu.vertical_spacing;
  int label_width;
  int rest_width;
  int button_width;
  int height;
  int width;
  enum menu_separator separator;
  int separator_p = lw_separator_p (val->name, &separator, 0);

  /* compute the sizes of the item */
  size_menu_item (mw, val, horizontal_p, &label_width, &rest_width,
		  &button_width, &height);

  if (horizontal_p)
    width = label_width + rest_width;
  else
    {
      label_width = ws->label_width;
      width = ws->width - 2 * shadow;
    }

  /* Only highlight an enabled item that has a callback. */
  if (highlighted_p)
    if (!val->enabled || !(val->call_data || val->contents))
      highlighted_p = 0;

  /* do the drawing. */
  if (!just_compute_p)
    {
      /* Add the shadow border of the containing menu */
      int x = where->x + shadow;
      int y = where->y + shadow;

      if (horizontal_p)
	{
	  x += margin;
	  y += margin;
	}

      /* pick the foreground and background GC. */
      if (val->enabled)
	text_gc = mw->menu.foreground_gc;
      else
	text_gc = mw->menu.disabled_gc;
      deco_gc = mw->menu.foreground_gc;

      if (separator_p)
	{
	  draw_separator (mw, ws->window, x, y, width, separator);
	}
      else
	{
	  int x_offset = x + h_spacing + shadow;
	  char* display_string = resource_widget_value (mw, val);
	  draw_shadow_rectangle (mw, ws->window, x, y, width, height, True,
				 False);

	  /* Deal with centering a menu title. */
	  if (!horizontal_p && !val->contents && !val->call_data)
	    {
	      int l = string_width (mw, display_string);

	      if (width > l)
		x_offset = (width - l) >> 1;
	    }
	  else if (!horizontal_p && ws->button_width)
	    x_offset += ws->button_width;


          XDrawString (XtDisplay (mw), ws->window, text_gc, x_offset,
		       y + v_spacing + shadow + font_ascent,
		       display_string, strlen (display_string));

	  if (!horizontal_p)
	    {
	      if (val->button_type == BUTTON_TYPE_TOGGLE)
		draw_toggle (mw, ws->window, x, y + v_spacing + shadow,
			     val->selected);
	      else if (val->button_type == BUTTON_TYPE_RADIO)
		draw_radio (mw, ws->window, x, y + v_spacing + shadow,
			    val->selected);

	      if (val->contents)
		{
		  int a_w = arrow_width (mw);
		  draw_arrow (mw, ws->window, deco_gc,
			      x + width - a_w
			      - mw->menu.horizontal_spacing
			      - mw->menu.shadow_thickness,
			      y + v_spacing + shadow, a_w,
			      highlighted_p);
		}
	      else if (val->key)
		{
		  XDrawString (XtDisplay (mw), ws->window, text_gc,
			       x + label_width + mw->menu.arrow_spacing,
			       y + v_spacing + shadow + font_ascent,
			       val->key, strlen (val->key));
		}
	    }
	  else
	    {
	      XDrawRectangle (XtDisplay (mw), ws->window,
			      mw->menu.background_gc,
			      x + shadow, y + shadow,
			      label_width + h_spacing - 1,
			      font_ascent + font_descent + 2 * v_spacing - 1);
	      draw_shadow_rectangle (mw, ws->window, x, y, width, height,
				     True, False);
	    }

	  if (highlighted_p)
	    draw_shadow_rectangle (mw, ws->window, x, y, width, height, False,
				   False);
	}
    }

  where->x += width;
  where->y += height;
}

static void
display_menu (mw, level, just_compute_p, highlighted_pos, hit, hit_return,
	      this, that)
     XlwMenuWidget mw;
     int level;
     Boolean just_compute_p;
     XPoint* highlighted_pos;
     XPoint* hit;
     widget_value** hit_return;
     widget_value* this;
     widget_value* that;
{
  widget_value*	val;
  widget_value* following_item;
  window_state* ws;
  XPoint	where;
  int horizontal_p = mw->menu.horizontal && (level == 0);
  int highlighted_p;
  int just_compute_this_one_p;
  /* This is set nonzero if the element containing HIGHLIGHTED_POS
     is disabled, so that we do not return any subsequent element either.  */
  int no_return = 0;
  enum menu_separator separator;

  if (level >= mw->menu.old_depth)
    abort_gracefully ((Widget) mw);

  if (level < mw->menu.old_depth - 1)
    following_item = mw->menu.old_stack [level + 1];
  else
    following_item = NULL;

  if (hit)
    *hit_return = NULL;

  where.x = 0;
  where.y = 0;

  ws = &mw->menu.windows [level];
  for (val = mw->menu.old_stack [level]->contents; val; val = val->next)
    {
      highlighted_p = val == following_item;
      if (highlighted_p && highlighted_pos)
	{
	  if (horizontal_p)
	    highlighted_pos->x = where.x;
	  else
	    highlighted_pos->y = where.y;
	}

      just_compute_this_one_p =
	just_compute_p || ((this || that) && val != this &&  val != that);

      display_menu_item (mw, val, ws, &where, highlighted_p, horizontal_p,
			 just_compute_this_one_p);

      if (highlighted_p && highlighted_pos)
	{
	  if (horizontal_p)
	    highlighted_pos->y = where.y;
	  else
	    highlighted_pos->x = where.x;
	}

      if (hit
	  && !*hit_return
	  && (horizontal_p ? hit->x < where.x : hit->y < where.y)
	  && !lw_separator_p (val->name, &separator, 0)
	  && !no_return)
	{
	  if (val->enabled)
	    *hit_return = val;
	  else
	    no_return = 1;
	}

      if (horizontal_p)
	where.y = 0;
      else
	where.x = 0;
    }

  if (!just_compute_p)
    draw_shadow_rectangle (mw, ws->window, 0, 0, ws->width, ws->height,
			   False, False);
}

/* Motion code */
static void
set_new_state (mw, val, level)
     XlwMenuWidget mw;
     widget_value* val;
     int level;
{
  int i;

  mw->menu.new_depth = 0;
  for (i = 0; i < level; i++)
    push_new_stack (mw, mw->menu.old_stack [i]);
  push_new_stack (mw, val);
}

static void
make_windows_if_needed (mw, n)
     XlwMenuWidget mw;
     int n;
{
  int i;
  int start_at;
  XSetWindowAttributes xswa;
  int mask;
  Window root = RootWindowOfScreen (DefaultScreenOfDisplay (XtDisplay (mw)));
  window_state* windows;

  if (mw->menu.windows_length >= n)
    return;

  xswa.save_under = True;
  xswa.override_redirect = True;
  xswa.background_pixel = mw->core.background_pixel;
  xswa.border_pixel = mw->core.border_pixel;
  xswa.event_mask =
    ExposureMask | PointerMotionMask | PointerMotionHintMask
      | ButtonReleaseMask | ButtonPressMask;
  xswa.cursor = mw->menu.cursor_shape;
  mask = CWSaveUnder | CWOverrideRedirect | CWBackPixel | CWBorderPixel
    | CWEventMask | CWCursor;

  if (!mw->menu.windows)
    {
      mw->menu.windows =
	(window_state*)XtMalloc (n * sizeof (window_state));
      start_at = 0;
    }
  else
    {
      mw->menu.windows =
	(window_state*)XtRealloc ((char*)mw->menu.windows,
				  n * sizeof (window_state));
      start_at = mw->menu.windows_length;
    }
  mw->menu.windows_length = n;

  windows = mw->menu.windows;

  for (i = start_at; i < n; i++)
   {
     windows [i].x = 0;
     windows [i].y = 0;
     windows [i].width = 1;
     windows [i].height = 1;
     windows [i].window =
       XCreateWindow (XtDisplay (mw), root, 0, 0, 1, 1,
		      0, 0, CopyFromParent, CopyFromParent, mask, &xswa);
  }
}

/* Value is non-zero if WINDOW is part of menu bar widget W.  */

int
xlwmenu_window_p (w, window)
     Widget w;
     Window window;
{
  XlwMenuWidget mw = (XlwMenuWidget) w;
  int i;
  
  for (i = 0; i < mw->menu.windows_length; ++i)
    if (window == mw->menu.windows[i].window)
      break;

  return i < mw->menu.windows_length;
}

/* Make the window fit in the screen */
static void
fit_to_screen (mw, ws, previous_ws, horizontal_p)
     XlwMenuWidget mw;
     window_state* ws;
     window_state* previous_ws;
     Boolean horizontal_p;
{
  unsigned int screen_width = WidthOfScreen (XtScreen (mw));
  unsigned int screen_height = HeightOfScreen (XtScreen (mw));
  /* 1 if we are unable to avoid an overlap between
     this menu and the parent menu in the X dimension.  */
  int horizontal_overlap = 0;

  if (ws->x < 0)
    ws->x = 0;
  else if (ws->x + ws->width > screen_width)
    {
      if (!horizontal_p)
	/* The addition of shadow-thickness for a sub-menu's position is
	   to reflect a similar adjustment when the menu is displayed to
	   the right of the invoking menu-item; it makes the sub-menu
	   look more `attached' to the menu-item.  */
	ws->x = previous_ws->x - ws->width + mw->menu.shadow_thickness;
      else
	ws->x = screen_width - ws->width;
      if (ws->x < 0)
	{
	  ws->x = 0;
	  horizontal_overlap = 1;
	}
    }
  /* If we overlap in X, try to avoid overlap in Y.  */
  if (horizontal_overlap
      && ws->y < previous_ws->y + previous_ws->height
      && previous_ws->y < ws->y + ws->height)
    {
      /* Put this menu right below or right above PREVIOUS_WS
	 if there's room.  */
      if (previous_ws->y + previous_ws->height + ws->height < screen_height)
	ws->y = previous_ws->y + previous_ws->height;
      else if (previous_ws->y - ws->height > 0)
	ws->y = previous_ws->y - ws->height;
    }

  if (ws->y < 0)
    ws->y = 0;
  else if (ws->y + ws->height > screen_height)
    {
      if (horizontal_p)
	ws->y = previous_ws->y - ws->height;
      else
	ws->y = screen_height - ws->height;
      if (ws->y < 0)
        ws->y = 0;
    }
}

/* Updates old_stack from new_stack and redisplays. */
static void
remap_menubar (mw)
     XlwMenuWidget mw;
{
  int i;
  int last_same;
  XPoint selection_position;
  int old_depth = mw->menu.old_depth;
  int new_depth = mw->menu.new_depth;
  widget_value** old_stack;
  widget_value** new_stack;
  window_state* windows;
  widget_value* old_selection;
  widget_value* new_selection;

  /* Check that enough windows and old_stack are ready. */
  make_windows_if_needed (mw, new_depth);
  make_old_stack_space (mw, new_depth);
  windows = mw->menu.windows;
  old_stack = mw->menu.old_stack;
  new_stack = mw->menu.new_stack;

  /* compute the last identical different entry */
  for (i = 1; i < old_depth && i < new_depth; i++)
    if (old_stack [i] != new_stack [i])
      break;
  last_same = i - 1;

  /* Memorize the previously selected item to be able to refresh it */
  old_selection = last_same + 1 < old_depth ? old_stack [last_same + 1] : NULL;
  if (old_selection && !old_selection->enabled)
    old_selection = NULL;
  new_selection = last_same + 1 < new_depth ? new_stack [last_same + 1] : NULL;
  if (new_selection && !new_selection->enabled)
    new_selection = NULL;

  /* Call callback when the hightlighted item changes.  */
  if (old_selection || new_selection)
    XtCallCallbackList ((Widget)mw, mw->menu.highlight,
			(XtPointer) new_selection);

  /* updates old_state from new_state.  It has to be done now because
     display_menu (called below) uses the old_stack to know what to display. */
  for (i = last_same + 1; i < new_depth; i++)
    old_stack [i] = new_stack [i];
  mw->menu.old_depth = new_depth;

  /* refresh the last selection */
  selection_position.x = 0;
  selection_position.y = 0;
  display_menu (mw, last_same, new_selection == old_selection,
		&selection_position, NULL, NULL, old_selection, new_selection);

  /* Now place the new menus.  */
  for (i = last_same + 1; i < new_depth && new_stack[i]->contents; i++)
    {
      window_state *previous_ws = &windows[i - 1];
      window_state *ws = &windows[i];

      ws->x = (previous_ws->x + selection_position.x
	       + mw->menu.shadow_thickness);
      if (mw->menu.horizontal && i == 1)
	ws->x += mw->menu.margin;

#if 0
      if (!mw->menu.horizontal || i > 1)
	ws->x += mw->menu.shadow_thickness;
#endif

      ws->y = (previous_ws->y + selection_position.y
	       + mw->menu.shadow_thickness);
      if (mw->menu.horizontal && i == 1)
	ws->y += mw->menu.margin;

      size_menu (mw, i);

      fit_to_screen (mw, ws, previous_ws, mw->menu.horizontal && i == 1);

      XClearWindow (XtDisplay (mw), ws->window);
      XMoveResizeWindow (XtDisplay (mw), ws->window, ws->x, ws->y,
			 ws->width, ws->height);
      XMapRaised (XtDisplay (mw), ws->window);
      display_menu (mw, i, False, &selection_position, NULL, NULL, NULL, NULL);
    }

  /* unmap the menus that popped down */
  for (i = new_depth - 1; i < old_depth; i++)
    if (i >= new_depth || !new_stack[i]->contents)
      XUnmapWindow (XtDisplay (mw), windows[i].window);
}

static Boolean
motion_event_is_in_menu (mw, ev, level, relative_pos)
     XlwMenuWidget mw;
     XMotionEvent* ev;
     int level;
     XPoint* relative_pos;
{
  window_state* ws = &mw->menu.windows [level];
  int shadow = level == 0 ? 0 : mw->menu.shadow_thickness;
  int x = ws->x + shadow;
  int y = ws->y + shadow;
  relative_pos->x = ev->x_root - x;
  relative_pos->y = ev->y_root - y;
  return (x - shadow < ev->x_root && ev->x_root < x + ws->width
	  && y - shadow < ev->y_root && ev->y_root < y + ws->height);
}

static Boolean
map_event_to_widget_value (mw, ev, val, level)
     XlwMenuWidget mw;
     XMotionEvent* ev;
     widget_value** val;
     int* level;
{
  int 		i;
  XPoint	relative_pos;
  window_state*	ws;

  *val = NULL;

  /* Find the window */
  for (i = mw->menu.old_depth - 1; i >= 0; i--)
    {
      ws = &mw->menu.windows [i];
      if (ws && motion_event_is_in_menu (mw, ev, i, &relative_pos))
	{
	  display_menu (mw, i, True, NULL, &relative_pos, val, NULL, NULL);

	  if (*val)
	    {
	      *level = i + 1;
	      return True;
	    }
	}
    }
  return False;
}

/* Procedures */
static void
make_drawing_gcs (mw)
     XlwMenuWidget mw;
{
  XGCValues xgcv;
  XColor temp;
  int delta;

  xgcv.font = mw->menu.font->fid;
  xgcv.foreground = mw->menu.foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.foreground_gc = XtGetGC ((Widget)mw,
				    GCFont | GCForeground | GCBackground,
				    &xgcv);

  xgcv.font = mw->menu.font->fid;
  xgcv.foreground = mw->menu.button_foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.button_gc = XtGetGC ((Widget)mw,
				GCFont | GCForeground | GCBackground,
				&xgcv);

  xgcv.font = mw->menu.font->fid;
  xgcv.background = mw->core.background_pixel;

#define BRIGHTNESS(color) (((color) & 0xff) + (((color) >> 8) & 0xff) + (((color) >> 16) & 0xff))

  /* Allocate color for disabled menu-items.  */
  if (BRIGHTNESS(mw->menu.foreground) < BRIGHTNESS(mw->core.background_pixel))
    {
      delta = 2.3;
      temp.pixel = mw->menu.foreground;
    }
  else
    {
      delta = 1.2;
      temp.pixel = mw->core.background_pixel;
    }

  x_alloc_lighter_color_for_widget ((Widget) mw, XtDisplay ((Widget) mw),
				    mw->core.colormap,
				    &temp.pixel,
				    delta,
				    0x8000);
  mw->menu.disabled_foreground = temp.pixel;

  if (mw->menu.foreground == mw->menu.disabled_foreground
      || mw->core.background_pixel == mw->menu.disabled_foreground)
    {
      /* Too few colors, use stipple.  */
      xgcv.foreground = mw->menu.foreground;
      xgcv.fill_style = FillStippled;
      xgcv.stipple = mw->menu.gray_pixmap;
      mw->menu.disabled_gc = XtGetGC ((Widget)mw,
				      (GCFont | GCForeground | GCBackground
				       | GCFillStyle | GCStipple), &xgcv);
    }
  else
    {
      /* Many colors available, use disabled pixel.  */
      xgcv.foreground = mw->menu.disabled_foreground;
      mw->menu.disabled_gc = XtGetGC ((Widget)mw,
				      (GCFont | GCForeground | GCBackground), &xgcv);
    }

  xgcv.font = mw->menu.font->fid;
  xgcv.foreground = mw->menu.button_foreground;
  xgcv.background = mw->core.background_pixel;
  xgcv.fill_style = FillStippled;
  xgcv.stipple = mw->menu.gray_pixmap;
  mw->menu.inactive_button_gc = XtGetGC ((Widget)mw,
				  (GCFont | GCForeground | GCBackground
				   | GCFillStyle | GCStipple), &xgcv);

  xgcv.font = mw->menu.font->fid;
  xgcv.foreground = mw->core.background_pixel;
  xgcv.background = mw->menu.foreground;
  mw->menu.background_gc = XtGetGC ((Widget)mw,
				    GCFont | GCForeground | GCBackground,
				    &xgcv);
}

static void
release_drawing_gcs (mw)
     XlwMenuWidget mw;
{
  XtReleaseGC ((Widget) mw, mw->menu.foreground_gc);
  XtReleaseGC ((Widget) mw, mw->menu.button_gc);
  XtReleaseGC ((Widget) mw, mw->menu.disabled_gc);
  XtReleaseGC ((Widget) mw, mw->menu.inactive_button_gc);
  XtReleaseGC ((Widget) mw, mw->menu.background_gc);
  /* let's get some segvs if we try to use these... */
  mw->menu.foreground_gc = (GC) -1;
  mw->menu.button_gc = (GC) -1;
  mw->menu.disabled_gc = (GC) -1;
  mw->menu.inactive_button_gc = (GC) -1;
  mw->menu.background_gc = (GC) -1;
}

#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		   ? ((unsigned long) (x)) : ((unsigned long) (y)))

static void
make_shadow_gcs (mw)
     XlwMenuWidget mw;
{
  XGCValues xgcv;
  unsigned long pm = 0;
  Display *dpy = XtDisplay ((Widget) mw);
  Screen *screen = XtScreen ((Widget) mw);
  Colormap cmap = mw->core.colormap;
  XColor topc, botc;
  int top_frobbed = 0, bottom_frobbed = 0;

  mw->menu.free_top_shadow_color_p = 0;
  mw->menu.free_bottom_shadow_color_p = 0;

  if (mw->menu.top_shadow_color == -1)
    mw->menu.top_shadow_color = mw->core.background_pixel;
  else
    mw->menu.top_shadow_color = mw->menu.top_shadow_color;

  if (mw->menu.bottom_shadow_color == -1)
    mw->menu.bottom_shadow_color = mw->menu.foreground;
  else
    mw->menu.bottom_shadow_color = mw->menu.bottom_shadow_color;

  if (mw->menu.top_shadow_color == mw->core.background_pixel ||
      mw->menu.top_shadow_color == mw->menu.foreground)
    {
      topc.pixel = mw->core.background_pixel;
#ifdef emacs
      if (x_alloc_lighter_color_for_widget ((Widget) mw, dpy, cmap,
					    &topc.pixel,
					    1.2, 0x8000))
#else
      XQueryColor (dpy, cmap, &topc);
      /* don't overflow/wrap! */
      topc.red   = MINL (65535, topc.red   * 1.2);
      topc.green = MINL (65535, topc.green * 1.2);
      topc.blue  = MINL (65535, topc.blue  * 1.2);
      if (XAllocColor (dpy, cmap, &topc))
#endif
	{
	  mw->menu.top_shadow_color = topc.pixel;
	  mw->menu.free_top_shadow_color_p = 1;
	  top_frobbed = 1;
	}
    }
  if (mw->menu.bottom_shadow_color == mw->menu.foreground ||
      mw->menu.bottom_shadow_color == mw->core.background_pixel)
    {
      botc.pixel = mw->core.background_pixel;
#ifdef emacs
      if (x_alloc_lighter_color_for_widget ((Widget) mw, dpy, cmap,
					    &botc.pixel,
					    0.6, 0x4000))
#else
      XQueryColor (dpy, cmap, &botc);
      botc.red   *= 0.6;
      botc.green *= 0.6;
      botc.blue  *= 0.6;
      if (XAllocColor (dpy, cmap, &botc))
#endif
	{
	  mw->menu.bottom_shadow_color = botc.pixel;
	  mw->menu.free_bottom_shadow_color_p = 1;
	  bottom_frobbed = 1;
	}
    }

  if (top_frobbed && bottom_frobbed)
    {
      if (topc.pixel == botc.pixel)
	{
	  if (botc.pixel == mw->menu.foreground)
	    {
	      if (mw->menu.free_top_shadow_color_p)
		{
		  x_free_dpy_colors (dpy, screen, cmap,
				     &mw->menu.top_shadow_color, 1);
		  mw->menu.free_top_shadow_color_p = 0;
		}
	      mw->menu.top_shadow_color = mw->core.background_pixel;
	    }
	  else
	    {
	      if (mw->menu.free_bottom_shadow_color_p)
		{
		  x_free_dpy_colors (dpy, screen, cmap,
				     &mw->menu.bottom_shadow_color, 1);
		  mw->menu.free_bottom_shadow_color_p = 0;
		}
	      mw->menu.bottom_shadow_color = mw->menu.foreground;
	    }
	}
    }

  if (!mw->menu.top_shadow_pixmap &&
      mw->menu.top_shadow_color == mw->core.background_pixel)
    {
      mw->menu.top_shadow_pixmap = mw->menu.gray_pixmap;
      if (mw->menu.free_top_shadow_color_p)
	{
	  x_free_dpy_colors (dpy, screen, cmap, &mw->menu.top_shadow_color, 1);
	  mw->menu.free_top_shadow_color_p = 0;
	}
      mw->menu.top_shadow_color = mw->menu.foreground;
    }
  if (!mw->menu.bottom_shadow_pixmap &&
      mw->menu.bottom_shadow_color == mw->core.background_pixel)
    {
      mw->menu.bottom_shadow_pixmap = mw->menu.gray_pixmap;
      if (mw->menu.free_bottom_shadow_color_p)
	{
	  x_free_dpy_colors (dpy, screen, cmap,
			     &mw->menu.bottom_shadow_color, 1);
	  mw->menu.free_bottom_shadow_color_p = 0;
	}
      mw->menu.bottom_shadow_color = mw->menu.foreground;
    }

  xgcv.fill_style = FillStippled;
  xgcv.foreground = mw->menu.top_shadow_color;
  xgcv.stipple = mw->menu.top_shadow_pixmap;
  pm = (xgcv.stipple ? GCStipple|GCFillStyle : 0);
  mw->menu.shadow_top_gc = XtGetGC ((Widget)mw, GCForeground | pm, &xgcv);

  xgcv.foreground = mw->menu.bottom_shadow_color;
  xgcv.stipple = mw->menu.bottom_shadow_pixmap;
  pm = (xgcv.stipple ? GCStipple|GCFillStyle : 0);
  mw->menu.shadow_bottom_gc = XtGetGC ((Widget)mw, GCForeground | pm, &xgcv);
}


static void
release_shadow_gcs (mw)
     XlwMenuWidget mw;
{
  Display *dpy = XtDisplay ((Widget) mw);
  Screen *screen = XtScreen ((Widget) mw);
  Colormap cmap = mw->core.colormap;
  Pixel px[2];
  int i = 0;

  if (mw->menu.free_top_shadow_color_p)
    px[i++] = mw->menu.top_shadow_color;
  if (mw->menu.free_bottom_shadow_color_p)
    px[i++] = mw->menu.bottom_shadow_color;
  if (i > 0)
    x_free_dpy_colors (dpy, screen, cmap, px, i);

  XtReleaseGC ((Widget) mw, mw->menu.shadow_top_gc);
  XtReleaseGC ((Widget) mw, mw->menu.shadow_bottom_gc);
}

static void
XlwMenuInitialize (request, mw, args, num_args)
     Widget request;
     XlwMenuWidget mw;
     ArgList args;
     Cardinal *num_args;
{
  /* Get the GCs and the widget size */
  XSetWindowAttributes xswa;
  int mask;

  Window window = RootWindowOfScreen (DefaultScreenOfDisplay (XtDisplay (mw)));
  Display* display = XtDisplay (mw);

#if 0
  widget_value *tem = (widget_value *) XtMalloc (sizeof (widget_value));

  /* _XtCreate is freeing the object that was passed to us,
     so make a copy that we will actually keep.  */
  lwlib_bcopy (mw->menu.contents, tem, sizeof (widget_value));
  mw->menu.contents = tem;
#endif

/*  mw->menu.cursor = XCreateFontCursor (display, mw->menu.cursor_shape); */
  mw->menu.cursor = mw->menu.cursor_shape;

  mw->menu.gray_pixmap
    = XCreatePixmapFromBitmapData (display, window, gray_bitmap_bits,
				   gray_bitmap_width, gray_bitmap_height,
				   (unsigned long)1, (unsigned long)0, 1);

  /* I don't understand why this ends up 0 sometimes,
     but it does.  This kludge works around it.
     Can anyone find a real fix?   -- rms.  */
  if (mw->menu.font == 0)
    mw->menu.font = xlwmenu_default_font;

  make_drawing_gcs (mw);
  make_shadow_gcs (mw);

  xswa.background_pixel = mw->core.background_pixel;
  xswa.border_pixel = mw->core.border_pixel;
  mask = CWBackPixel | CWBorderPixel;

  mw->menu.popped_up = False;

  mw->menu.old_depth = 1;
  mw->menu.old_stack = (widget_value**)XtMalloc (sizeof (widget_value*));
  mw->menu.old_stack_length = 1;
  mw->menu.old_stack [0] = mw->menu.contents;

  mw->menu.new_depth = 0;
  mw->menu.new_stack = 0;
  mw->menu.new_stack_length = 0;
  push_new_stack (mw, mw->menu.contents);

  mw->menu.windows = (window_state*)XtMalloc (sizeof (window_state));
  mw->menu.windows_length = 1;
  mw->menu.windows [0].x = 0;
  mw->menu.windows [0].y = 0;
  mw->menu.windows [0].width = 0;
  mw->menu.windows [0].height = 0;
  size_menu (mw, 0);

  mw->core.width = mw->menu.windows [0].width;
  mw->core.height = mw->menu.windows [0].height;
}

static void
XlwMenuClassInitialize ()
{
}

static void
XlwMenuRealize (w, valueMask, attributes)
     Widget w;
     Mask *valueMask;
     XSetWindowAttributes *attributes;
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  XSetWindowAttributes xswa;
  int mask;

  (*xlwMenuWidgetClass->core_class.superclass->core_class.realize)
    (w, valueMask, attributes);

  xswa.save_under = True;
  xswa.cursor = mw->menu.cursor_shape;
  mask = CWSaveUnder | CWCursor;
  XChangeWindowAttributes (XtDisplay (w), XtWindow (w), mask, &xswa);

  mw->menu.windows [0].window = XtWindow (w);
  mw->menu.windows [0].x = w->core.x;
  mw->menu.windows [0].y = w->core.y;
  mw->menu.windows [0].width = w->core.width;
  mw->menu.windows [0].height = w->core.height;
}

/* Only the toplevel menubar/popup is a widget so it's the only one that
   receives expose events through Xt.  So we repaint all the other panes
   when receiving an Expose event. */
static void
XlwMenuRedisplay (w, ev, region)
     Widget w;
     XEvent* ev;
     Region region;
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  int i;

  /* If we have a depth beyond 1, it's because a submenu was displayed.
     If the submenu has been destroyed, set the depth back to 1.  */
  if (submenu_destroyed)
    {
      mw->menu.old_depth = 1;
      submenu_destroyed = 0;
    }

  for (i = 0; i < mw->menu.old_depth; i++)
    display_menu (mw, i, False, NULL, NULL, NULL, NULL, NULL);
}


/* Part of a hack to make the menu redisplay when a tooltip frame
   over a menu item is unmapped.  */

void
xlwmenu_redisplay (w)
     Widget w;
{
  XlwMenuRedisplay (w, NULL, None);
}

static void
XlwMenuDestroy (w)
     Widget w;
{
  int i;
  XlwMenuWidget mw = (XlwMenuWidget) w;

  if (pointer_grabbed)
    ungrab_all ((Widget)w, CurrentTime);
  pointer_grabbed = 0;

  submenu_destroyed = 1;

  release_drawing_gcs (mw);
  release_shadow_gcs (mw);

  /* this doesn't come from the resource db but is created explicitly
     so we must free it ourselves. */
  XFreePixmap (XtDisplay (mw), mw->menu.gray_pixmap);
  mw->menu.gray_pixmap = (Pixmap) -1;

#if 0
  /* Do free mw->menu.contents because nowadays we copy it
     during initialization.  */
  XtFree (mw->menu.contents);
#endif

  /* Don't free mw->menu.contents because that comes from our creator.
     The `*_stack' elements are just pointers into `contents' so leave
     that alone too.  But free the stacks themselves. */
  if (mw->menu.old_stack) XtFree ((char *) mw->menu.old_stack);
  if (mw->menu.new_stack) XtFree ((char *) mw->menu.new_stack);

  /* Remember, you can't free anything that came from the resource
     database.  This includes:
         mw->menu.cursor
         mw->menu.top_shadow_pixmap
         mw->menu.bottom_shadow_pixmap
         mw->menu.font
     Also the color cells of top_shadow_color, bottom_shadow_color,
     foreground, and button_foreground will never be freed until this
     client exits.  Nice, eh?
   */

  /* start from 1 because the one in slot 0 is w->core.window */
  for (i = 1; i < mw->menu.windows_length; i++)
    XDestroyWindow (XtDisplay (mw), mw->menu.windows [i].window);
  if (mw->menu.windows)
    XtFree ((char *) mw->menu.windows);
}

static Boolean
XlwMenuSetValues (current, request, new)
     Widget current;
     Widget request;
     Widget new;
{
  XlwMenuWidget oldmw = (XlwMenuWidget)current;
  XlwMenuWidget newmw = (XlwMenuWidget)new;
  Boolean redisplay = False;
  int i;

  if (newmw->menu.contents
      && newmw->menu.contents->contents
      && newmw->menu.contents->contents->change >= VISIBLE_CHANGE)
    redisplay = True;
  /* Do redisplay if the contents are entirely eliminated.  */
  if (newmw->menu.contents
      && newmw->menu.contents->contents == 0
      && newmw->menu.contents->change >= VISIBLE_CHANGE)
    redisplay = True;

  if (newmw->core.background_pixel != oldmw->core.background_pixel
      || newmw->menu.foreground != oldmw->menu.foreground
      || newmw->menu.font != oldmw->menu.font)
    {
      release_drawing_gcs (newmw);
      make_drawing_gcs (newmw);

      release_shadow_gcs (newmw);
      /* Cause the shadow colors to be recalculated.  */
      newmw->menu.top_shadow_color = -1;
      newmw->menu.bottom_shadow_color = -1;
      make_shadow_gcs (newmw);

      redisplay = True;

      if (XtIsRealized (current))
	/* If the menu is currently displayed, change the display.  */
	for (i = 0; i < oldmw->menu.windows_length; i++)
	  {
	    XSetWindowBackground (XtDisplay (oldmw),
				  oldmw->menu.windows [i].window,
				  newmw->core.background_pixel);
	    /* clear windows and generate expose events */
	    XClearArea (XtDisplay (oldmw), oldmw->menu.windows[i].window,
			0, 0, 0, 0, True);
	  }
    }

  return redisplay;
}

static void
XlwMenuResize (w)
     Widget w;
{
  XlwMenuWidget mw = (XlwMenuWidget)w;

  if (mw->menu.popped_up)
    {
      /* Don't allow the popup menu to resize itself.  */
      mw->core.width = mw->menu.windows [0].width;
      mw->core.height = mw->menu.windows [0].height;
      mw->core.parent->core.width = mw->core.width ;
      mw->core.parent->core.height = mw->core.height ;
    }
  else
    {
      mw->menu.windows [0].width = mw->core.width;
      mw->menu.windows [0].height = mw->core.height;
    }
}

/* Action procedures */
static void
handle_single_motion_event (mw, ev)
     XlwMenuWidget mw;
     XMotionEvent* ev;
{
  widget_value*	val;
  int 		level;

  if (!map_event_to_widget_value (mw, ev, &val, &level))
    pop_new_stack_if_no_contents (mw);
  else
    set_new_state (mw, val, level);
  remap_menubar (mw);

  /* Sync with the display.  Makes it feel better on X terms. */
  XSync (XtDisplay (mw), False);
}

static void
handle_motion_event (mw, ev)
     XlwMenuWidget mw;
     XMotionEvent* ev;
{
  int x = ev->x_root;
  int y = ev->y_root;
  int state = ev->state;

  handle_single_motion_event (mw, ev);

  /* allow motion events to be generated again */
  if (ev->is_hint
      && XQueryPointer (XtDisplay (mw), ev->window,
			&ev->root, &ev->subwindow,
			&ev->x_root, &ev->y_root,
			&ev->x, &ev->y,
			&ev->state)
      && ev->state == state
      && (ev->x_root != x || ev->y_root != y))
    handle_single_motion_event (mw, ev);
}

static void
Start (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
  XlwMenuWidget mw = (XlwMenuWidget)w;

  if (!mw->menu.popped_up)
    {
      menu_post_event = *ev;
      pop_up_menu (mw, (XButtonPressedEvent*) ev);
    }
  else
    {
      /* If we push a button while the menu is posted semipermanently,
	 releasing the button should always pop the menu down.  */
      next_release_must_exit = 1;

      /* notes the absolute position of the menubar window */
      mw->menu.windows [0].x = ev->xmotion.x_root - ev->xmotion.x;
      mw->menu.windows [0].y = ev->xmotion.y_root - ev->xmotion.y;

      /* handles the down like a move, slots are compatible */
      handle_motion_event (mw, &ev->xmotion);
    }
}

static void
Drag (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  if (mw->menu.popped_up)
    handle_motion_event (mw, &ev->xmotion);
}

/* Do nothing.
   This is how we handle presses and releases of modifier keys.  */
static void
Nothing (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
}

widget_value *
find_first_selectable (mw, item)
     XlwMenuWidget mw;
     widget_value *item;
{
  widget_value *current = item;
  enum menu_separator separator;

  while (lw_separator_p (current->name, &separator, 0) || !current->enabled)
    if (current->next)
      current=current->next;
    else
	return NULL;

  return current;
}

widget_value *
find_next_selectable (mw, item)
     XlwMenuWidget mw;
     widget_value *item;
{
  widget_value *current = item;
  enum menu_separator separator;

  while (current->next && (current=current->next) &&
	 (lw_separator_p (current->name, &separator, 0) || !current->enabled))
    ;

  if (current == item)
    {
      if (mw->menu.old_depth < 2)
	return current;
      current = mw->menu.old_stack [mw->menu.old_depth - 2]->contents;

      while (lw_separator_p (current->name, &separator, 0) || !current->enabled)
	{
	  if (current->next)
	    current=current->next;

	  if (current == item)
	    break;
	}

    }

  return current;
}

widget_value *
find_prev_selectable (mw, item)
     XlwMenuWidget mw;
     widget_value *item;
{
  widget_value *current = item;
  widget_value *prev = item;

  while ((current=find_next_selectable (mw, current)) != item)
    {
      if (prev == current)
	break;
      prev=current;
    }

  return prev;
}

static void
Down (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
  XlwMenuWidget mw = (XlwMenuWidget) w;
  widget_value* selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];

  /* Inside top-level menu-bar?  */
  if (mw->menu.old_depth == 2)
    /* When <down> in the menu-bar is pressed, display the corresponding
       sub-menu and select the first selectable menu item there.  */
    set_new_state (mw, find_first_selectable (mw, selected_item->contents), mw->menu.old_depth);
  else
    /* Highlight next possible (enabled and not separator) menu item.  */
    set_new_state (mw, find_next_selectable (mw, selected_item), mw->menu.old_depth - 1);

  remap_menubar (mw);
}

static void
Up (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
  XlwMenuWidget mw = (XlwMenuWidget) w;
  widget_value* selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];

  /* Inside top-level menu-bar?  */
  if (mw->menu.old_depth == 2)
    {
      /* FIXME: this is tricky.  <up> in the menu-bar should select the
	 last selectable item in the list.  So we select the first
	 selectable one and find the previous selectable item.  Is there
	 a better way?  */
      set_new_state (mw, find_first_selectable (mw, selected_item->contents), mw->menu.old_depth);
      remap_menubar (mw);
      selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];
      set_new_state (mw, find_prev_selectable (mw, selected_item), mw->menu.old_depth - 1);
    }
  else
    /* Highlight previous (enabled and not separator) menu item.  */
    set_new_state (mw, find_prev_selectable (mw, selected_item), mw->menu.old_depth - 1);

  remap_menubar (mw);
}

static void
Left (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
  XlwMenuWidget mw = (XlwMenuWidget) w;
  widget_value* selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];

  /* Inside top-level menu-bar?  */
  if (mw->menu.old_depth == 2)
    /* When <left> in the menu-bar is pressed, display the previous item on
       the menu-bar. If the current item is the first one, highlight the
       last item in the menubar (probably Help).  */
    set_new_state (mw, find_prev_selectable (mw, selected_item), mw->menu.old_depth - 1);
  else
    {
      pop_new_stack_if_no_contents (mw);
      set_new_state (mw, mw->menu.old_stack [mw->menu.old_depth - 2], mw->menu.old_depth - 2);
    }

  remap_menubar (mw);
}

static void
Right (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
  XlwMenuWidget mw = (XlwMenuWidget) w;
  widget_value* selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];

  /* Inside top-level menu-bar?  */
  if (mw->menu.old_depth == 2)
    /* When <right> in the menu-bar is pressed, display the next item on
       the menu-bar. If the current item is the last one, highlight the
       first item (probably File).  */
    set_new_state (mw, find_next_selectable (mw, selected_item), mw->menu.old_depth - 1);
  else if (selected_item->contents)     /* Is this menu item expandable?  */
    {
      set_new_state (mw, selected_item->contents, mw->menu.old_depth);
      remap_menubar (mw);
      selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];
      if (!selected_item->enabled && find_first_selectable (mw, selected_item))
	set_new_state (mw, find_first_selectable (mw, selected_item), mw->menu.old_depth - 1);
    }
  else
    {
      pop_new_stack_if_no_contents (mw);
      set_new_state (mw, mw->menu.old_stack [mw->menu.old_depth - 2], mw->menu.old_depth - 2);
    }

  remap_menubar (mw);
}

/* Handle key press and release events while menu is popped up.
   Our action is to get rid of the menu.  */
static void
Key (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
  XlwMenuWidget mw = (XlwMenuWidget)w;

  /* Pop down everything.  */
  mw->menu.new_depth = 1;
  remap_menubar (mw);

  if (mw->menu.popped_up)
    {
      mw->menu.popped_up = False;
      ungrab_all ((Widget)mw, ev->xmotion.time);
      if (XtIsShell (XtParent ((Widget) mw)))
	XtPopdown (XtParent ((Widget) mw));
      else
	{
	  XtRemoveGrab ((Widget) mw);
	  display_menu (mw, 0, False, NULL, NULL, NULL, NULL, NULL);
	}
    }

  /* callback */
  XtCallCallbackList ((Widget)mw, mw->menu.select, (XtPointer)0);
}

static void
Select (w, ev, params, num_params)
     Widget w;
     XEvent *ev;
     String *params;
     Cardinal *num_params;
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  widget_value* selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];

  /* If user releases the button quickly, without selecting anything,
     after the initial down-click that brought the menu up,
     do nothing.  */
  if ((selected_item == 0
       || ((widget_value *) selected_item)->call_data == 0)
      && !next_release_must_exit
      && (ev->xbutton.time - menu_post_event.xbutton.time
	  < XtGetMultiClickTime (XtDisplay (w))))
    return;

  /* pop down everything.  */
  mw->menu.new_depth = 1;
  remap_menubar (mw);

  if (mw->menu.popped_up)
    {
      mw->menu.popped_up = False;
      ungrab_all ((Widget)mw, ev->xmotion.time);
      if (XtIsShell (XtParent ((Widget) mw)))
	XtPopdown (XtParent ((Widget) mw));
      else
	{
	  XtRemoveGrab ((Widget) mw);
	  display_menu (mw, 0, False, NULL, NULL, NULL, NULL, NULL);
	}
    }

  /* callback */
  XtCallCallbackList ((Widget)mw, mw->menu.select, (XtPointer)selected_item);
}


/* Special code to pop-up a menu */
void
pop_up_menu (mw, event)
     XlwMenuWidget mw;
     XButtonPressedEvent* event;
{
  int		x = event->x_root;
  int		y = event->y_root;
  int		w;
  int		h;
  int		borderwidth = mw->menu.shadow_thickness;
  Screen*	screen = XtScreen (mw);
  Display       *display = XtDisplay (mw);
  int count;

  next_release_must_exit = 0;

  XtCallCallbackList ((Widget)mw, mw->menu.open, NULL);

  if (XtIsShell (XtParent ((Widget)mw)))
    size_menu (mw, 0);

  w = mw->menu.windows [0].width;
  h = mw->menu.windows [0].height;

  x -= borderwidth;
  y -= borderwidth;
  if (x < borderwidth)
    x = borderwidth;
  if (x + w + 2 * borderwidth > WidthOfScreen (screen))
    x = WidthOfScreen (screen) - w - 2 * borderwidth;
  if (y < borderwidth)
    y = borderwidth;
  if (y + h + 2 * borderwidth> HeightOfScreen (screen))
    y = HeightOfScreen (screen) - h - 2 * borderwidth;

  mw->menu.popped_up = True;
  if (XtIsShell (XtParent ((Widget)mw)))
    {
      XtConfigureWidget (XtParent ((Widget)mw), x, y, w, h,
			 XtParent ((Widget)mw)->core.border_width);
      XtPopup (XtParent ((Widget)mw), XtGrabExclusive);
      display_menu (mw, 0, False, NULL, NULL, NULL, NULL, NULL);
      mw->menu.windows [0].x = x + borderwidth;
      mw->menu.windows [0].y = y + borderwidth;
    }
  else
    {
      XEvent *ev = (XEvent *) event;

      XtAddGrab ((Widget) mw, True, True);

      /* notes the absolute position of the menubar window */
      mw->menu.windows [0].x = ev->xmotion.x_root - ev->xmotion.x;
      mw->menu.windows [0].y = ev->xmotion.y_root - ev->xmotion.y;
    }

#ifdef emacs
  count = x_catch_errors (display);
#endif
  if (XtGrabPointer ((Widget)mw, False,
                     (PointerMotionMask
                      | PointerMotionHintMask
                      | ButtonReleaseMask
                      | ButtonPressMask),
                     GrabModeAsync, GrabModeAsync, None,
                     mw->menu.cursor_shape,
                     event->time) == Success)
    {
      if (! GRAB_KEYBOARD
          || XtGrabKeyboard ((Widget)mw, False, GrabModeAsync,
                             GrabModeAsync, event->time) == Success)
        {
          XtSetKeyboardFocus((Widget)mw, None);
          pointer_grabbed = 1;
        }
      else
        XtUngrabPointer ((Widget)mw, event->time);
    }

#ifdef emacs
  if (x_had_errors_p (display))
    {
      pointer_grabbed = 0;
      XtUngrabPointer ((Widget)mw, event->time);
    }
  x_uncatch_errors (display, count);
#endif

  handle_motion_event (mw, (XMotionEvent*)event);
}
