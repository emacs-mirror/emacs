/* The emacs frame widget.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Emacs 19 face widget ported by Fred Pierresteguy */

#include <config.h>
#include <stdio.h>
#include "lisp.h"
#include "xterm.h"

#include "frame.h"

#include "dispextern.h"
#include "blockinput.h"

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include "widgetprv.h"
#include <X11/ObjectP.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include "../lwlib/lwlib.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

/* This sucks: this is the first default that x-faces.el tries.  This won't
   be used unless neither the "Emacs.EmacsFrame" resource nor the
   "Emacs.EmacsFrame" resource is set; the frame
   may have the wrong default size if this font doesn't exist, but some other
   font that x-faces.el does.  The workaround is to specify some font in the
   resource database; I don't know a solution other than duplicating the font-
   searching code from x-faces.el in this file.

   This also means that if "Emacs.EmacsFrame" is specified as a non-
   existent font, then Xt is going to substitute "XtDefaultFont" for it,
   which is a different size than this one.  The solution for this is to
   make x-faces.el try to use XtDefaultFont.  The problem with that is that
   XtDefaultFont is almost certainly variable-width.

   #### Perhaps we could have this code explicitly set XtDefaultFont to this?
 */
#define DEFAULT_FACE_FONT "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"


static void EmacsFrameInitialize (/*Widget, Widget, ArgList, Cardinal * */);
static void EmacsFrameDestroy (/* Widget */);
static void EmacsFrameRealize (/* Widget, XtValueMask*, XSetWindowAttributes* */);
void EmacsFrameResize (/* Widget widget */);
static Boolean EmacsFrameSetValues (/* Widget, Widget, Widget,
				     ArgList, Cardinal * */);
static XtGeometryResult EmacsFrameQueryGeometry (/* Widget, XtWidgetGeometry*,
						  XtWidgetGeometry* */);


#undef XtOffset
#define XtOffset(p_type,field) \
	((Cardinal) (((char *) (&(((p_type)0)->field))) - ((char *)0)))
#define offset(field) XtOffset(EmacsFrame, emacs_frame.field)

static XtResource resources[] = {
  {XtNgeometry, XtCGeometry, XtRString, sizeof(String),
     offset (geometry), XtRString, (XtPointer) 0},
  {XtNiconic, XtCIconic, XtRBoolean, sizeof(Boolean),
     offset (iconic), XtRImmediate, (XtPointer) False},

  {XtNemacsFrame, XtCEmacsFrame, XtRPointer, sizeof (XtPointer),
     offset (frame), XtRImmediate, 0},

  {XtNminibuffer, XtCMinibuffer, XtRInt, sizeof (int),
     offset (minibuffer), XtRImmediate, (XtPointer)0},
  {XtNunsplittable, XtCUnsplittable, XtRBoolean, sizeof (Boolean),
     offset (unsplittable), XtRImmediate, (XtPointer)0},
  {XtNinternalBorderWidth, XtCInternalBorderWidth, XtRInt, sizeof (int),
     offset (internal_border_width), XtRImmediate, (XtPointer)4},
  {XtNinterline, XtCInterline, XtRInt, sizeof (int),
     offset (interline), XtRImmediate, (XtPointer)0},
  {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font),XtRString, DEFAULT_FACE_FONT},
  {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground_pixel), XtRString, "XtDefaultForeground"},
  {XtNcursorColor, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(cursor_color), XtRString, "XtDefaultForeground"},
  {XtNbarCursor, XtCBarCursor, XtRBoolean, sizeof (Boolean),
     offset (bar_cursor), XtRImmediate, (XtPointer)0},
  {XtNvisualBell, XtCVisualBell, XtRBoolean, sizeof (Boolean),
     offset (visual_bell), XtRImmediate, (XtPointer)0},
  {XtNbellVolume, XtCBellVolume, XtRInt, sizeof (int),
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

EmacsFrameClassRec emacsFrameClassRec = {
    { /* core fields */
    /* superclass		*/	&widgetClassRec,
    /* class_name		*/	"EmacsFrame",
    /* widget_size		*/	sizeof(EmacsFrameRec),
    /* class_initialize		*/	0,
    /* class_part_initialize	*/	0,
    /* class_inited		*/	FALSE,
    /* initialize		*/	EmacsFrameInitialize,
    /* initialize_hook		*/	0,
    /* realize			*/	EmacsFrameRealize,
    /* actions			*/	0, /*emacsFrameActionsTable*/
    /* num_actions		*/	0, /*XtNumber (emacsFrameActionsTable)*/
    /* resources		*/	resources,
    /* resource_count		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	EmacsFrameDestroy,
    /* resize			*/	EmacsFrameResize,
    /* expose			*/	XtInheritExpose,
    /* set_values		*/	EmacsFrameSetValues,
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

WidgetClass emacsFrameClass = (WidgetClass) &emacsFrameClassRec;

static void
get_default_char_pixel_size (ew, pixel_width, pixel_height)
     EmacsFrame ew;
     int* pixel_width;
     int* pixel_height;
{
  struct frame* f = ew->emacs_frame.frame;
  *pixel_width = FONT_WIDTH (f->display.x->font);
  *pixel_height = f->display.x->line_height;
}

static void
pixel_to_char_size (ew, pixel_width, pixel_height, char_width, char_height)
     EmacsFrame ew;
     Dimension pixel_width;
     Dimension pixel_height;
     int* char_width;
     int* char_height;
{
  struct frame* f = ew->emacs_frame.frame;
  *char_width = PIXEL_TO_CHAR_WIDTH (f, pixel_width);
  *char_height = PIXEL_TO_CHAR_HEIGHT (f, pixel_height);
}

static void
char_to_pixel_size (ew, char_width, char_height, pixel_width, pixel_height)
     EmacsFrame ew;
     int char_width;
     int char_height;
     Dimension* pixel_width;
     Dimension* pixel_height;
{
  struct frame* f = ew->emacs_frame.frame;
  *pixel_width = CHAR_TO_PIXEL_WIDTH (f, char_width);
  *pixel_height = CHAR_TO_PIXEL_HEIGHT (f, char_height);
}

static void
round_size_to_char (ew, in_width, in_height, out_width, out_height)
     EmacsFrame ew;
     Dimension in_width;
     Dimension in_height;
     Dimension* out_width;
     Dimension* out_height;
{
  int char_width;
  int char_height;
  pixel_to_char_size (ew, in_width, in_height, &char_width, &char_height);
  char_to_pixel_size (ew, char_width, char_height, out_width, out_height);
}

static Widget
get_wm_shell (w)
     Widget w;
{
  Widget wmshell;

  for (wmshell = XtParent (w);
       wmshell && !XtIsWMShell (wmshell);
       wmshell = XtParent (wmshell));

  return wmshell;
}

static void
mark_shell_size_user_specified (wmshell)
     Widget wmshell;
{
  if (! XtIsWMShell (wmshell)) abort ();
  /* This is kind of sleazy, but I can't see how else to tell it to make it
     mark the WM_SIZE_HINTS size as user specified when appropriate. */
  ((WMShellWidget) wmshell)->wm.size_hints.flags |= USSize;
}


/* Can't have static frame locals because of some broken compilers.
   Normally, initializing a variable like this doesn't work in emacs,
   but it's ok in this file because it must come after lastfile (and
   thus have its data not go into text space) because Xt needs to
   write to initialized data objects too.
 */
static Boolean first_frame_p = True;

static void
set_frame_size (ew)
     EmacsFrame ew;
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

  /* Geometry of the AppShell */
  int app_flags = 0;
  int app_x = 0;
  int app_y = 0;
  unsigned int app_w = 0;
  unsigned int app_h = 0;
  
  /* Geometry of the EmacsFrame */
  int frame_flags = 0;
  int frame_x = 0;
  int frame_y = 0;
  unsigned int frame_w = 0;
  unsigned int frame_h = 0;
  
  /* Hairily merged geometry */
  int x = 0;
  int y = 0;
  unsigned int w = ew->emacs_frame.frame->width;
  unsigned int h = ew->emacs_frame.frame->height;
  int flags = 0;
  
  Widget wmshell = get_wm_shell ((Widget) ew);
  /* Each Emacs shell is now independent and top-level.  */
  Widget app_shell = wmshell;
  
  if (! XtIsSubclass (wmshell, shellWidgetClass)) abort ();

  /* We don't need this for the momment. The geometry is computed in 
     xfns.c.  */
#if 0
  /* If the EmacsFrame doesn't have a geometry but the shell does,
     treat that as the geometry of the frame.  (Is this bogus?
     I'm not sure.) */
  if (ew->emacs_frame.geometry == 0)
    XtVaGetValues (wmshell, XtNgeometry, &ew->emacs_frame.geometry, 0);

  /* If the Shell is iconic, then the EmacsFrame is iconic.  (Is
     this bogus? I'm not sure.) */
  if (!ew->emacs_frame.iconic)
    XtVaGetValues (wmshell, XtNiconic, &ew->emacs_frame.iconic, 0);
  
  
  {
    char *geom = 0;
    XtVaGetValues (app_shell, XtNgeometry, &geom, 0);
    if (geom)
      app_flags = XParseGeometry (geom, &app_x, &app_y, &app_w, &app_h);
  }
  
  if (ew->emacs_frame.geometry)
    frame_flags = XParseGeometry (ew->emacs_frame.geometry,
				   &frame_x, &frame_y,
				   &frame_w, &frame_h);
  
  if (first_frame_p)
    {
      /* If this is the first frame created:
         ====================================

         - Use the ApplicationShell's size/position, if specified.
           (This is "Emacs.geometry", or the "-geometry" command line arg.)
         - Else use the EmacsFrame's size/position.
           (This is "*Frame-NAME.geometry")

	 - If the AppShell is iconic, the frame should be iconic.

	 AppShell comes first so that -geometry always applies to the first
	 frame created, even if there is an "every frame" entry in the
	 resource database.
       */
      if (app_flags & (XValue | YValue))
	{
	  x = app_x; y = app_y;
	  flags |= (app_flags & (XValue | YValue | XNegative | YNegative));
	}
      else if (frame_flags & (XValue | YValue))
	{
	  x = frame_x; y = frame_y;
	  flags |= (frame_flags & (XValue | YValue | XNegative | YNegative));
	}

      if (app_flags & (WidthValue | HeightValue))
	{
	  w = app_w; h = app_h;
	  flags |= (app_flags & (WidthValue | HeightValue));
	}
      else if (frame_flags & (WidthValue | HeightValue))
	{
	  w = frame_w; h = frame_h;
	  flags |= (frame_flags & (WidthValue | HeightValue));
	}

      /* If the AppShell is iconic, then the EmacsFrame is iconic. */
      if (!ew->emacs_frame.iconic)
	XtVaGetValues (app_shell, XtNiconic, &ew->emacs_frame.iconic, 0);

      first_frame_p = False;
    }
  else
    {
      /* If this is not the first frame created:
         ========================================

         - use the EmacsFrame's size/position if specified
         - Otherwise, use the ApplicationShell's size, but not position.

         So that means that one can specify the position of the first frame
         with "Emacs.geometry" or `-geometry'; but can only specify the
	 position of subsequent frames with "*Frame-NAME.geometry".

	 AppShell comes second so that -geometry does not apply to subsequent
	 frames when there is an "every frame" entry in the resource db,
	 but does apply to the first frame.
       */
      if (frame_flags & (XValue | YValue))
	{
	  x = frame_x; y = frame_y;
	  flags |= (frame_flags & (XValue | YValue | XNegative | YNegative));
	}

      if (frame_flags & (WidthValue | HeightValue))
	{
	  w = frame_w; h = frame_h;
	  flags |= (frame_flags & (WidthValue | HeightValue));
	}
      else if (app_flags & (WidthValue | HeightValue))
	{
	  w = app_w;
	  h = app_h;
	  flags |= (app_flags & (WidthValue | HeightValue));
	}
    }
#endif /* 0 */
  {
    struct frame* frame = ew->emacs_frame.frame;
    Dimension pixel_width, pixel_height;
    char shell_position [32];

    /* Take into account the size of the scrollbar */
    frame->display.x->vertical_scroll_bar_extra
      = (!FRAME_HAS_VERTICAL_SCROLL_BARS (frame)
	 ? 0
	 : FRAME_SCROLL_BAR_PIXEL_WIDTH (frame) > 0
	 ? FRAME_SCROLL_BAR_PIXEL_WIDTH (frame)
	 : (FRAME_SCROLL_BAR_COLS (frame)
	    * FONT_WIDTH (frame->display.x->font)));

    change_frame_size (frame, h, w, 1, 0);
    char_to_pixel_size (ew, w, h, &pixel_width, &pixel_height);
    ew->core.width = pixel_width;
    ew->core.height = pixel_height;

#if 0 /* xfns.c takes care of this now.  */
    /* If a position was specified, assign it to the shell widget.
       (Else WM won't do anything with it.)
     */
    if (flags & (XValue | YValue))
      {
	/* the tricky things with the sign is to make sure that
	   -0 is printed -0. */
	int len;
	char *tem;
	sprintf (shell_position, "=%c%d%c%d",
		 flags & XNegative ? '-' : '+', x < 0 ? -x : x,
		 flags & YNegative ? '-' : '+', y < 0 ? -y : y);
	len = strlen (shell_position) + 1;
	tem = (char *) xmalloc (len);
	strncpy (tem, shell_position, len);
	XtVaSetValues (wmshell, XtNgeometry, tem, 0);
      }
    else if (flags & (WidthValue | HeightValue))
      {
	int len;
	char *tem;
	sprintf (shell_position, "=%dx%d", pixel_width, pixel_height);
	len = strlen (shell_position) + 1;
	tem = (char *) xmalloc (len);
	strncpy (tem, shell_position, len);
	XtVaSetValues (wmshell, XtNgeometry, tem, 0);
      }

    /* If the geometry spec we're using has W/H components, mark the size
       in the WM_SIZE_HINTS as user specified. */
    if (flags & (WidthValue | HeightValue))
      mark_shell_size_user_specified (wmshell);

    /* Also assign the iconic status of the frame to the Shell, so that
       the WM sees it. */
    XtVaSetValues (wmshell, XtNiconic, ew->emacs_frame.iconic, 0);
#endif /* 0 */
  }
}


static void
update_wm_hints (ew)
     EmacsFrame ew;
{
  Widget wmshell = get_wm_shell ((Widget)ew);
  int cw;
  int ch;
  Dimension rounded_width;
  Dimension rounded_height;
  int char_width;
  int char_height;
  int base_width;
  int base_height;
  int min_rows = 0, min_cols = 0;

  check_frame_size (ew->emacs_frame.frame, &min_rows, &min_cols);

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
		 XtNbaseWidth, base_width,
		 XtNbaseHeight, base_height,
		 XtNwidthInc, cw, 
		 XtNheightInc, ch,
		 XtNminWidth, base_width + min_cols * cw,
		 XtNminHeight, base_height + min_rows * ch,
		 0);
}

static void
create_frame_gcs (ew)
     EmacsFrame ew;
{
  struct frame *s = ew->emacs_frame.frame;

  s->display.x->normal_gc
    = XCreateGC (XtDisplay (ew), RootWindowOfScreen (XtScreen (ew)),
		 (unsigned long)0, (XGCValues *)0);
  s->display.x->reverse_gc
    = XCreateGC (XtDisplay (ew), RootWindowOfScreen (XtScreen (ew)),
		 (unsigned long)0, (XGCValues *)0);
  s->display.x->cursor_gc
    = XCreateGC (XtDisplay (ew), RootWindowOfScreen (XtScreen (ew)),
		 (unsigned long)0, (XGCValues *)0);
}

static char setup_frame_cursor_bits[] =
{
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

static void
setup_frame_gcs (ew)
     EmacsFrame ew;
{
  XGCValues gc_values;
  struct frame* s = ew->emacs_frame.frame;
  Pixmap blank_stipple, blank_tile;

  /* We have to initialize all of our GCs to have a stipple/tile, otherwise
     XGetGCValues returns uninitialized data when we query the stipple
     (instead of None or something sensible) and it makes things hard.

     This should be fixed for real by not querying the GCs but instead having
     some GC-based cache instead of the current face-based cache which doesn't
     effectively cache all of the GC settings we need to use.
   */

  blank_stipple
    = XCreateBitmapFromData (XtDisplay (ew),
			     RootWindowOfScreen (XtScreen (ew)),
			     setup_frame_cursor_bits, 2, 2);

  /* use fg = 0, bg = 1 below, but it's irrelevant since this pixmap should
     never actually get used as a background tile!
   */
  blank_tile
    = XCreatePixmapFromBitmapData (XtDisplay(ew),
				   RootWindowOfScreen (XtScreen (ew)),
				   setup_frame_cursor_bits, 2, 2,
				   (unsigned long)0, (unsigned long)1,
				   ew->core.depth);

  /* Normal video */
  gc_values.font = ew->emacs_frame.font->fid;
  gc_values.foreground = ew->emacs_frame.foreground_pixel;
  gc_values.background = ew->core.background_pixel;
  gc_values.graphics_exposures = False;
  gc_values.stipple = blank_stipple;
  gc_values.tile = blank_tile;
  XChangeGC (XtDisplay (ew), s->display.x->normal_gc,
	     (GCFont | GCForeground | GCBackground | GCGraphicsExposures
	      | GCStipple | GCTile),
	     &gc_values);

  /* Reverse video style. */
  gc_values.font = ew->emacs_frame.font->fid;
  gc_values.foreground = ew->core.background_pixel;
  gc_values.background = ew->emacs_frame.foreground_pixel;
  gc_values.graphics_exposures = False;
  gc_values.stipple = blank_stipple;
  gc_values.tile = blank_tile;
  XChangeGC (XtDisplay (ew), s->display.x->reverse_gc,
	     (GCFont | GCForeground | GCBackground | GCGraphicsExposures
	      | GCStipple | GCTile),
	     &gc_values);

  /* Cursor has to have an empty stipple. */
  gc_values.font = ew->emacs_frame.font->fid;
  gc_values.foreground = ew->core.background_pixel;
  gc_values.background = ew->emacs_frame.cursor_color;
  gc_values.graphics_exposures = False;
  gc_values.tile = blank_tile;
  gc_values.stipple
    = XCreateBitmapFromData (XtDisplay (ew),
			     RootWindowOfScreen (XtScreen (ew)),
			     setup_frame_cursor_bits, 16, 16);
  XChangeGC (XtDisplay (ew), s->display.x->cursor_gc,
	     (GCFont | GCForeground | GCBackground | GCGraphicsExposures
	      | GCStipple | GCTile),
	     &gc_values);
}

static void
update_various_frame_slots (ew)
     EmacsFrame ew;
{
  struct x_display* x = ew->emacs_frame.frame->display.x;
  x->pixel_height = ew->core.height;
  x->pixel_width = ew->core.width;
  x->internal_border_width = ew->emacs_frame.internal_border_width;

}

static void
update_from_various_frame_slots (ew)
     EmacsFrame ew;
{
  struct x_display* x = ew->emacs_frame.frame->display.x;
  ew->core.height = x->pixel_height;
  ew->core.width = x->pixel_width;
  ew->core.background_pixel = x->background_pixel;
  ew->emacs_frame.internal_border_width = x->internal_border_width;
  ew->emacs_frame.font = x->font;
  ew->emacs_frame.foreground_pixel = x->foreground_pixel;
  ew->emacs_frame.cursor_color = x->cursor_pixel;
  ew->core.border_pixel = x->border_pixel;
}

static void 
EmacsFrameInitialize (request, new, dum1, dum2)
     Widget request;
     Widget new;
     ArgList dum1;
     Cardinal *dum2;
{
  EmacsFrame ew = (EmacsFrame)new;

  if (!ew->emacs_frame.frame)
    {
      fprintf (stderr,
	       "can't create an emacs frame widget without a frame\n");
      exit (1);
    }

#if 0 /* done in xfns.c */
  /* If the "Emacs.EmacsFrame.{default,Face}.{attributeFont,AttributeFont}"
     resource is set, then it always overrides "Emacs.EmacsFrame.{font,Font}".
     It's unfortunate that we have to do this, but we need to know the font
     size for frame-sizing purposes before the faces get initialized.  If
     the "default.attributeFont" isn't set, then we use the font of this
     EmacsFrame itself, defaulting to XtDefaultFont.  Up in the lisp code,
     the "default" face will use the frame's font if its own is not set,
     so everything stays in sync -- it's not possible for the frame's font
     and the default face's font to be different.
   */
  {
    XFontStruct *f = 0;
    XtResource face_res;
    face_res.resource_name = "attributeFont";
    face_res.resource_class = "AttributeFont";
    face_res.resource_type = XtRFontStruct;
    face_res.resource_size = sizeof (XFontStruct *);
    face_res.resource_offset = 0;
    face_res.default_type = XtRImmediate;
    face_res.default_addr = 0;
    XtGetSubresources ((Widget) ew, (XtPointer) &f, "default", "Face",
		       &face_res, 1, NULL, 0);
      
    if (f)
	ew->emacs_frame.font = f;
    else if (! ew->emacs_frame.font)
      {
	fprintf (stderr, "emacs frame widget could not load a font\n");
	exit (1);
      }
  }

/* Update the font field in frame */
  ew->emacs_frame.frame->display.x->font = ew->emacs_frame.font;
#endif

  update_from_various_frame_slots (ew);
  set_frame_size (ew); 
/*create_frame_gcs (ew);
  setup_frame_gcs (ew);
  update_various_frame_slots (ew); */
}


static void
EmacsFrameRealize (widget, mask, attrs)
     Widget widget;
     XtValueMask *mask;
     XSetWindowAttributes *attrs;
{
  EmacsFrame ew = (EmacsFrame)widget;

  attrs->event_mask = (KeyPressMask | ExposureMask | ButtonPressMask |
		       ButtonReleaseMask | StructureNotifyMask |
		       FocusChangeMask | PointerMotionHintMask |
		       PointerMotionMask | LeaveWindowMask | EnterWindowMask |
		       VisibilityChangeMask | PropertyChangeMask |
		       StructureNotifyMask | SubstructureNotifyMask |
		       SubstructureRedirectMask);
  *mask |= CWEventMask;
  XtCreateWindow (widget, InputOutput, (Visual *)CopyFromParent, *mask,
		  attrs);
  update_wm_hints (ew); 
}

extern void free_frame_faces (/* struct frame * */);

static void
EmacsFrameDestroy (widget)
     Widget widget;
{
  EmacsFrame ew = (EmacsFrame) widget;
  struct frame* s = ew->emacs_frame.frame;

  if (! s) abort ();
  if (! s->display.x) abort ();
  if (! s->display.x->normal_gc) abort ();

  /* this would be called from Fdelete_frame() but it needs to free some
     stuff after the widget has been finalized but before the widget has
     been freed. */
  free_frame_faces (s);

  BLOCK_INPUT;
  /* need to be careful that the face-freeing code doesn't free these too */
  XFreeGC (XtDisplay (widget), s->display.x->normal_gc);
  XFreeGC (XtDisplay (widget), s->display.x->reverse_gc);
  XFreeGC (XtDisplay (widget), s->display.x->cursor_gc);
  UNBLOCK_INPUT;
}

void
EmacsFrameResize (widget)
     Widget widget;
{
  EmacsFrame ew = (EmacsFrame)widget;
  struct frame *f = ew->emacs_frame.frame;
  int columns;
  int rows;

  pixel_to_char_size (ew, ew->core.width, ew->core.height, &columns, &rows);
  change_frame_size (f, rows, columns, 1, 0);
  update_wm_hints (ew); 
  update_various_frame_slots (ew);
}

static Boolean
EmacsFrameSetValues (cur_widget, req_widget, new_widget, dum1, dum2)
     Widget cur_widget;
     Widget req_widget;
     Widget new_widget;
     ArgList dum1;
     Cardinal *dum2;
{
  EmacsFrame cur = (EmacsFrame)cur_widget;
  EmacsFrame new = (EmacsFrame)new_widget;

  Boolean needs_a_refresh = False;
  Boolean has_to_recompute_size;
  Boolean has_to_recompute_gcs;
  Boolean has_to_update_hints;

  int char_width, char_height;
  Dimension pixel_width;
  Dimension pixel_height;
  
  has_to_recompute_gcs = (cur->emacs_frame.font != new->emacs_frame.font
			  || (cur->emacs_frame.foreground_pixel
			      != new->emacs_frame.foreground_pixel)
			  || (cur->core.background_pixel
			      != new->core.background_pixel)
			  );
  
  has_to_recompute_size = (cur->emacs_frame.font != new->emacs_frame.font
			   && cur->core.width == new->core.width
			   && cur->core.height == new->core.height);

  has_to_update_hints = (cur->emacs_frame.font != new->emacs_frame.font);

  if (has_to_recompute_gcs)
    {
      setup_frame_gcs (new);
      needs_a_refresh = True;
    }
			  
  if (has_to_recompute_size)
    {
      pixel_width = new->core.width;
      pixel_height = new->core.height;
      pixel_to_char_size (new, pixel_width, pixel_height, &char_width,
			  &char_height);
      char_to_pixel_size (new, char_width, char_height, &pixel_width,
			  &pixel_height);
      new->core.width = pixel_width;
      new->core.height = pixel_height;

      change_frame_size (new->emacs_frame.frame, char_height, char_width,
			  1, 0);
      needs_a_refresh = True;
    }

  if (has_to_update_hints)
    update_wm_hints (new);

  update_various_frame_slots (new);

  /* #### This doesn't work, I haven't been able to find ANY kludge that
     will let (x-create-frame '((iconic . t))) work.  It seems that changes
     to wm_shell's iconic slot have no effect after it has been realized,
     and calling XIconifyWindow doesn't work either (even thought the window
     has been created.)  Perhaps there is some property we could smash
     directly, but I'm sick of this for now.  Xt is a steaming pile of shit!
   */
  if (cur->emacs_frame.iconic != new->emacs_frame.iconic)
    {
      Widget wmshell = get_wm_shell ((Widget) cur);
      XtVaSetValues (wmshell, XtNiconic, new->emacs_frame.iconic, 0);
    }

  return needs_a_refresh;
}

static XtGeometryResult
EmacsFrameQueryGeometry (widget, request, result)
     Widget widget;
     XtWidgetGeometry* request;
     XtWidgetGeometry* result;
{
  EmacsFrame ew = (EmacsFrame)widget;

  int mask = request->request_mode;
  Dimension ok_width, ok_height;

  if (mask & (CWWidth | CWHeight))
    {
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

/* Special entrypoints */
void
EmacsFrameSetCharSize (widget, columns, rows)
     Widget widget;
     int columns;
     int rows;
{
  EmacsFrame ew = (EmacsFrame) widget;
  Dimension pixel_width, pixel_height, granted_width, granted_height;
  XtGeometryResult result;
  struct frame *f = ew->emacs_frame.frame;
  Arg al[2];
  int ac = 0;
  
  if (columns < 3) columns = 3;  /* no way buddy */

  check_frame_size (f, &rows, &columns);
  f->display.x->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->display.x->font)));

  char_to_pixel_size (ew, columns, rows, &pixel_width, &pixel_height);

  /* Recompute the entire geometry management.  */
  if (ew->core.width != pixel_width || ew->core.height != pixel_height)
    {
      int hdelta = pixel_height - ew->core.height;
      int column_widget_height = f->display.x->column_widget->core.height;
      int old_left = f->display.x->widget->core.x;
      int old_top = f->display.x->widget->core.y;

      lw_refigure_widget (f->display.x->column_widget, False);
      ac = 0;
      XtSetArg (al[ac], XtNheight, pixel_height); ac++;
      XtSetArg (al[ac], XtNwidth, pixel_width); ac++;
      XtSetValues ((Widget) ew, al, ac);
 
      ac = 0;
      XtSetArg (al[ac], XtNheight, column_widget_height + hdelta); ac++;
      XtSetArg (al[ac], XtNwidth, pixel_width); ac++;
      XtSetValues (f->display.x->column_widget, al, ac);
      lw_refigure_widget (f->display.x->column_widget, True);

      /* These seem to get clobbered.  I don't know why. - rms.  */
      f->display.x->widget->core.x = old_left;
      f->display.x->widget->core.y = old_top;
    }

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);
}
