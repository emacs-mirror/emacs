/* The emacs frame widget.
   Copyright (C) 1992-1993, 2000-2017 Free Software Foundation, Inc.

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

/* Emacs 19 face widget ported by Fred Pierresteguy */

/* This file has been censored by the Communications Decency Act.
   That law was passed under the guise of a ban on pornography, but
   it bans far more than that.  This file did not contain pornography,
   but it was censored nonetheless.

   For information on US government censorship of the Internet, and
   what you can do to bring back freedom of the press, see the web
   site http://www.vtw.org/
   */

#include <config.h>
#include "widget.h"

#include <stdio.h>
#include <stdlib.h>

#include "lisp.h"
#include "xterm.h"
#include "frame.h"

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include "widgetprv.h"
#include <X11/ObjectP.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include "../lwlib/lwlib.h"

static void EmacsFrameInitialize (Widget request, Widget new, ArgList dum1, Cardinal *dum2);
static void EmacsFrameDestroy (Widget widget);
static void EmacsFrameRealize (Widget widget, XtValueMask *mask, XSetWindowAttributes *attrs);
static void EmacsFrameResize (Widget widget);
static XtGeometryResult EmacsFrameQueryGeometry (Widget widget, XtWidgetGeometry *request, XtWidgetGeometry *result);


#define offset(field) offsetof (EmacsFrameRec, emacs_frame.field)

static XtResource resources[] = {
  {(char *) XtNgeometry, (char *) XtCGeometry, XtRString, sizeof (String),
     offset (geometry), XtRString, (XtPointer) 0},
  {XtNiconic, XtCIconic, XtRBoolean, sizeof (Boolean),
     offset (iconic), XtRImmediate, (XtPointer) False},

  {(char *) XtNemacsFrame, (char *) XtCEmacsFrame,
     XtRPointer, sizeof (XtPointer),
     offset (frame), XtRImmediate, 0},

  {(char *) XtNminibuffer, (char *) XtCMinibuffer, XtRInt, sizeof (int),
     offset (minibuffer), XtRImmediate, (XtPointer)0},
  {(char *) XtNunsplittable, (char *) XtCUnsplittable,
     XtRBoolean, sizeof (Boolean),
     offset (unsplittable), XtRImmediate, (XtPointer)0},
  {(char *) XtNinternalBorderWidth, (char *) XtCInternalBorderWidth,
     XtRInt, sizeof (int),
     offset (internal_border_width), XtRImmediate, (XtPointer)4},
  {(char *) XtNinterline, (char *) XtCInterline, XtRInt, sizeof (int),
     offset (interline), XtRImmediate, (XtPointer)0},
  {(char *) XtNforeground, (char *) XtCForeground, XtRPixel, sizeof (Pixel),
     offset (foreground_pixel), XtRString, (char *) "XtDefaultForeground"},
  {(char *) XtNcursorColor, (char *) XtCForeground, XtRPixel, sizeof (Pixel),
     offset (cursor_color), XtRString, (char *) "XtDefaultForeground"},
  {(char *) XtNbarCursor, (char *) XtCBarCursor, XtRBoolean, sizeof (Boolean),
     offset (bar_cursor), XtRImmediate, (XtPointer)0},
  {(char *) XtNvisualBell, (char *) XtCVisualBell, XtRBoolean, sizeof (Boolean),
     offset (visual_bell), XtRImmediate, (XtPointer)0},
  {(char *) XtNbellVolume, (char *) XtCBellVolume, XtRInt, sizeof (int),
     offset (bell_volume), XtRImmediate, (XtPointer)0},
};

#undef offset

/*
static XtActionsRec
emacsFrameActionsTable [] = {
  {"keypress",  key_press},
  {"focus_in",  emacs_frame_focus_handler},
  {"focus_out", emacs_frame_focus_handler},
};

static char
emacsFrameTranslations [] = "\
<KeyPress>: keypress()\n\
<FocusIn>:  focus_in()\n\
<FocusOut>: focus_out()\n\
";
*/

static EmacsFrameClassRec emacsFrameClassRec = {
    { /* core fields */
    /* superclass		*/	0, /* filled in by emacsFrameClass */
    /* class_name		*/	(char *) "EmacsFrame",
    /* widget_size		*/	sizeof (EmacsFrameRec),
    /* class_initialize		*/	0,
    /* class_part_initialize	*/	0,
    /* class_inited		*/	FALSE,
    /* initialize		*/	EmacsFrameInitialize,
    /* initialize_hook		*/	0,
    /* realize			*/	EmacsFrameRealize,
    /* actions			*/	0, /*emacsFrameActionsTable*/
    /* num_actions		*/	0, /*XtNumber (emacsFrameActionsTable)*/
    /* resources		*/	resources,
    /* resource_count		*/	XtNumber (resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	EmacsFrameDestroy,
    /* resize			*/	EmacsFrameResize,
    /* expose			*/	XtInheritExpose,

    /* Emacs never does XtSetvalues on this widget, so we have no code
       for it. */
    /* set_values		*/	0, /* Not supported */
    /* set_values_hook		*/	0,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	0,
    /* accept_focus		*/	XtInheritAcceptFocus,
    /* version			*/	XtVersion,
    /* callback_private		*/	0,
    /* tm_table			*/	0, /*emacsFrameTranslations*/
    /* query_geometry		*/	EmacsFrameQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	0
    }
};

WidgetClass
emacsFrameClass (void)
{
  /* Set the superclass here rather than relying on static
     initialization, to work around an unexelf.c bug on x86 platforms
     that use the GNU Gold linker (Bug#27248).  */
  emacsFrameClassRec.core_class.superclass = &widgetClassRec;

  return (WidgetClass) &emacsFrameClassRec;
}

static void
get_default_char_pixel_size (EmacsFrame ew, int *pixel_width, int *pixel_height)
{
  struct frame *f = ew->emacs_frame.frame;
  *pixel_width = FRAME_COLUMN_WIDTH (f);
  *pixel_height = FRAME_LINE_HEIGHT (f);
}

static void
pixel_to_char_size (EmacsFrame ew, Dimension pixel_width, Dimension pixel_height, int *char_width, int *char_height)
{
  struct frame *f = ew->emacs_frame.frame;
  *char_width = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, (int) pixel_width);
  *char_height = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, (int) pixel_height);
}

static void
pixel_to_text_size (EmacsFrame ew, Dimension pixel_width, Dimension pixel_height, int *text_width, int *text_height)
{
  struct frame *f = ew->emacs_frame.frame;
  *text_width = FRAME_PIXEL_TO_TEXT_WIDTH (f, (int) pixel_width);
  *text_height = FRAME_PIXEL_TO_TEXT_HEIGHT (f, (int) pixel_height);
}

static void
char_to_pixel_size (EmacsFrame ew, int char_width, int char_height, Dimension *pixel_width, Dimension *pixel_height)
{
  struct frame *f = ew->emacs_frame.frame;
  *pixel_width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, char_width);
  *pixel_height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, char_height);
}

static void
round_size_to_char (EmacsFrame ew, Dimension in_width, Dimension in_height, Dimension *out_width, Dimension *out_height)
{
  int char_width;
  int char_height;
  pixel_to_char_size (ew, in_width, in_height, &char_width, &char_height);
  char_to_pixel_size (ew, char_width, char_height, out_width, out_height);
}

static Widget
get_wm_shell (Widget w)
{
  Widget wmshell;

  for (wmshell = XtParent (w);
       wmshell && !XtIsWMShell (wmshell);
       wmshell = XtParent (wmshell));

  return wmshell;
}

#if 0 /* Currently not used.  */

static void
mark_shell_size_user_specified (Widget wmshell)
{
  if (! XtIsWMShell (wmshell)) emacs_abort ();
  /* This is kind of sleazy, but I can't see how else to tell it to make it
     mark the WM_SIZE_HINTS size as user specified when appropriate. */
  ((WMShellWidget) wmshell)->wm.size_hints.flags |= USSize;
}

#endif


static void
set_frame_size (EmacsFrame ew)
{
  /* The widget hierarchy is

	argv[0]			emacsShell	pane	Frame-NAME
	ApplicationShell	EmacsShell	Paned	EmacsFrame

     We accept geometry specs in this order:

	*Frame-NAME.geometry
	*EmacsFrame.geometry
	Emacs.geometry

     Other possibilities for widget hierarchies might be

	argv[0]			frame		pane	Frame-NAME
	ApplicationShell	EmacsShell	Paned	EmacsFrame
     or
	argv[0]			Frame-NAME	pane	Frame-NAME
	ApplicationShell	EmacsShell	Paned	EmacsFrame
     or
	argv[0]			Frame-NAME	pane	emacsTextPane
	ApplicationShell	EmacsFrame	Paned	EmacsTextPane

     With the current setup, the text-display-area is the part which is
     an emacs "frame", since that's the only part managed by emacs proper
     (the menubar and the parent of the menubar and all that sort of thing
     are managed by lwlib.)

     The EmacsShell widget is simply a replacement for the Shell widget
     which is able to deal with using an externally-supplied window instead
     of always creating its own.  It is not actually emacs specific, and
     should possibly have class "Shell" instead of "EmacsShell" to simplify
     the resources.

   */

  /* Hairily merged geometry */
  struct frame *f = ew->emacs_frame.frame;
  int w = FRAME_COLS (f);
  int h = FRAME_LINES (f);
  Widget wmshell = get_wm_shell ((Widget) ew);
  Dimension pixel_width, pixel_height;
  /* Each Emacs shell is now independent and top-level.  */

  if (! XtIsSubclass (wmshell, shellWidgetClass)) emacs_abort ();

  char_to_pixel_size (ew, w, h, &pixel_width, &pixel_height);
  ew->core.width = (frame_resize_pixelwise
		    ? FRAME_PIXEL_WIDTH (f)
		    : pixel_width);
  ew->core.height = (frame_resize_pixelwise
		     ? FRAME_PIXEL_HEIGHT (f)
		     : pixel_height);

  frame_size_history_add
    (f, Qset_frame_size, FRAME_TEXT_WIDTH (f), FRAME_TEXT_HEIGHT (f),
     list2 (make_number (ew->core.width), make_number (ew->core.height)));
}

static void
update_wm_hints (EmacsFrame ew)
{
  Widget wmshell = get_wm_shell ((Widget) ew);
  int cw;
  int ch;
  Dimension rounded_width;
  Dimension rounded_height;
  int char_width;
  int char_height;
  int base_width;
  int base_height;
  int min_rows = 0, min_cols = 0;

  /* This happens when the frame is just created.  */
  if (! wmshell) return;

  pixel_to_char_size (ew, ew->core.width, ew->core.height,
		      &char_width, &char_height);
  char_to_pixel_size (ew, char_width, char_height,
		      &rounded_width, &rounded_height);
  get_default_char_pixel_size (ew, &cw, &ch);

  base_width = (wmshell->core.width - ew->core.width
		+ (rounded_width - (char_width * cw)));
  base_height = (wmshell->core.height - ew->core.height
		 + (rounded_height - (char_height * ch)));

  /* This is kind of sleazy, but I can't see how else to tell it to
     make it mark the WM_SIZE_HINTS size as user specified.
   */
/*  ((WMShellWidget) wmshell)->wm.size_hints.flags |= USSize;*/

  XtVaSetValues (wmshell,
		 XtNbaseWidth, (XtArgVal) base_width,
		 XtNbaseHeight, (XtArgVal) base_height,
		 XtNwidthInc, (XtArgVal) (frame_resize_pixelwise ? 1 : cw),
		 XtNheightInc, (XtArgVal) (frame_resize_pixelwise ? 1 : ch),
		 XtNminWidth, (XtArgVal) (base_width + min_cols * cw),
		 XtNminHeight, (XtArgVal) (base_height + min_rows * ch),
		 NULL);
}

void
widget_update_wm_size_hints (Widget widget)
{
  EmacsFrame ew = (EmacsFrame) widget;
  update_wm_hints (ew);
}

static void
update_various_frame_slots (EmacsFrame ew)
{
  struct frame *f = ew->emacs_frame.frame;

  f->internal_border_width = ew->emacs_frame.internal_border_width;
}

static void
update_from_various_frame_slots (EmacsFrame ew)
{
  struct frame *f = ew->emacs_frame.frame;
  struct x_output *x = f->output_data.x;

  ew->core.height = FRAME_PIXEL_HEIGHT (f) - x->menubar_height;
  ew->core.width = FRAME_PIXEL_WIDTH (f);
  ew->core.background_pixel = FRAME_BACKGROUND_PIXEL (f);
  ew->emacs_frame.internal_border_width = f->internal_border_width;
  ew->emacs_frame.foreground_pixel = FRAME_FOREGROUND_PIXEL (f);
  ew->emacs_frame.cursor_color = x->cursor_pixel;
  ew->core.border_pixel = x->border_pixel;
}

static void
EmacsFrameInitialize (Widget request, Widget new, ArgList dum1, Cardinal *dum2)
{
  EmacsFrame ew = (EmacsFrame) new;

  if (!ew->emacs_frame.frame)
    {
      fprintf (stderr,
	       "can't create an emacs frame widget without a frame\n");
      exit (1);
    }

  update_from_various_frame_slots (ew);
  set_frame_size (ew);
}

static void
resize_cb (Widget widget,
           XtPointer closure,
           XEvent *event,
           Boolean *continue_to_dispatch)
{
  EmacsFrameResize (widget);
}


static void
EmacsFrameRealize (Widget widget, XtValueMask *mask, XSetWindowAttributes *attrs)
{
  EmacsFrame ew = (EmacsFrame) widget;

  /* This used to contain SubstructureRedirectMask, but this turns out
     to be a problem with XIM on Solaris, and events from that mask
     don't seem to be used.  Let's check that.  */
  attrs->event_mask = (STANDARD_EVENT_SET
		       | PropertyChangeMask
		       | SubstructureNotifyMask);
  *mask |= CWEventMask;
  XtCreateWindow (widget, InputOutput, (Visual *) CopyFromParent, *mask,
		  attrs);
  /* Some ConfigureNotify events does not end up in EmacsFrameResize so
     make sure we get them all.  Seen with xfcwm4 for example.  */
  XtAddRawEventHandler (widget, StructureNotifyMask, False, resize_cb, NULL);
  update_wm_hints (ew);
}

static void
EmacsFrameDestroy (Widget widget)
{
  /* All GCs are now freed in x_free_frame_resources.  */
}

static void
EmacsFrameResize (Widget widget)
{
  EmacsFrame ew = (EmacsFrame) widget;
  struct frame *f = ew->emacs_frame.frame;
  int width, height;

  pixel_to_text_size (ew, ew->core.width, ew->core.height, &width, &height);

  frame_size_history_add
    (f, QEmacsFrameResize, width, height,
     list5 (make_number (ew->core.width), make_number (ew->core.height),
	    make_number (FRAME_TOP_MARGIN_HEIGHT (f)),
	    make_number (FRAME_SCROLL_BAR_AREA_HEIGHT (f)),
	    make_number (2 * FRAME_INTERNAL_BORDER_WIDTH (f))));

  change_frame_size (f, width, height, 0, 1, 0, 1);

  update_wm_hints (ew);
  update_various_frame_slots (ew);

  cancel_mouse_face (f);
}

static XtGeometryResult
EmacsFrameQueryGeometry (Widget widget, XtWidgetGeometry *request, XtWidgetGeometry *result)
{
  EmacsFrame ew = (EmacsFrame) widget;

  int mask = request->request_mode;
  Dimension ok_width, ok_height;

  if (mask & (CWWidth | CWHeight))
    {
      if (!frame_resize_pixelwise)
	round_size_to_char (ew,
			    (mask & CWWidth) ? request->width : ew->core.width,
			    ((mask & CWHeight) ? request->height
			     : ew->core.height),
			    &ok_width, &ok_height);
      if ((mask & CWWidth) && (ok_width != request->width))
	{
	  result->request_mode |= CWWidth;
	  result->width = ok_width;
	}
      if ((mask & CWHeight) && (ok_height != request->height))
	{
	  result->request_mode |= CWHeight;
	  result->height = ok_height;
	}
    }
  return result->request_mode ? XtGeometryAlmost : XtGeometryYes;
}

/* Special entry points */
void
EmacsFrameSetCharSize (Widget widget, int columns, int rows)
{
  EmacsFrame ew = (EmacsFrame) widget;
  struct frame *f = ew->emacs_frame.frame;

  if (!frame_inhibit_resize (f, 0, Qfont)
      && !frame_inhibit_resize (f, 1, Qfont))
    x_set_window_size (f, 0, columns, rows, 0);
}


void
widget_store_internal_border (Widget widget)
{
  EmacsFrame ew = (EmacsFrame) widget;
  struct frame *f = ew->emacs_frame.frame;

  ew->emacs_frame.internal_border_width = f->internal_border_width;
}
