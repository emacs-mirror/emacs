/* Functions for the X window system.
   Copyright (C) 1989, 1992, 1993 Free Software Foundation.

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

/* Completely rewritten by Richard Stallman.  */

/* Rewritten for X11 by Joseph Arceneaux */

#if 0
#include <stdio.h>
#endif
#include <signal.h>
#include <config.h>
#include "lisp.h"
#include "xterm.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "dispextern.h"
#include "keyboard.h"
#include "blockinput.h"

#ifdef HAVE_X_WINDOWS
extern void abort ();

#ifndef VMS
#if 1 /* Used to be #ifdef EMACS_BITMAP_FILES, but this should always work.  */
#include "bitmaps/gray.xbm"
#else
#include <X11/bitmaps/gray>
#endif
#else
#include "[.bitmaps]gray.xbm"
#endif

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#ifdef HAVE_X11
/* X Resource data base */
static XrmDatabase xrdb;

/* The class of this X application.  */
#define EMACS_CLASS "Emacs"

#ifdef HAVE_X11R4
#define MAXREQUEST(dpy) (XMaxRequestSize (dpy))
#else
#define MAXREQUEST(dpy) ((dpy)->max_request_size)
#endif

/* The name we're using in resource queries.  */
Lisp_Object Vx_resource_name;

/* Title name and application name for X stuff. */
extern char *x_id_name;

/* The background and shape of the mouse pointer, and shape when not
   over text or in the modeline. */
Lisp_Object Vx_pointer_shape, Vx_nontext_pointer_shape, Vx_mode_pointer_shape;

/* Color of chars displayed in cursor box. */
Lisp_Object Vx_cursor_fore_pixel;

/* The screen being used.  */
static Screen *x_screen;

/* The X Visual we are using for X windows (the default) */
Visual *screen_visual;

/* Height of this X screen in pixels. */
int x_screen_height;

/* Width of this X screen in pixels. */
int x_screen_width;

/* Number of planes for this screen. */
int x_screen_planes;

/* Non nil if no window manager is in use. */
Lisp_Object Vx_no_window_manager;

/* `t' if a mouse button is depressed. */

Lisp_Object Vmouse_depressed;

extern unsigned int x_mouse_x, x_mouse_y, x_mouse_grabbed;

/* Atom for indicating window state to the window manager. */
extern Atom Xatom_wm_change_state;

/* Communication with window managers. */
extern Atom Xatom_wm_protocols;

/* Kinds of protocol things we may receive. */
extern Atom Xatom_wm_take_focus;
extern Atom Xatom_wm_save_yourself;
extern Atom Xatom_wm_delete_window;

/* Other WM communication */
extern Atom Xatom_wm_configure_denied; /* When our config request is denied */
extern Atom Xatom_wm_window_moved;     /* When the WM moves us. */

#else	/* X10 */

/* Default size of an Emacs window.  */
static char *default_window = "=80x24+0+0";

#define MAXICID 80
char iconidentity[MAXICID];
#define ICONTAG "emacs@"
char minibuffer_iconidentity[MAXICID];
#define MINIBUFFER_ICONTAG "minibuffer@"

#endif /* X10 */

/* The last 23 bits of the timestamp of the last mouse button event. */
Time mouse_timestamp;

/* Evaluate this expression to rebuild the section of syms_of_xfns
   that initializes and staticpros the symbols declared below.  Note
   that Emacs 18 has a bug that keeps C-x C-e from being able to
   evaluate this expression.

(progn
  ;; Accumulate a list of the symbols we want to initialize from the
  ;; declarations at the top of the file.
  (goto-char (point-min))
  (search-forward "/\*&&& symbols declared here &&&*\/\n")
  (let (symbol-list)
    (while (looking-at "Lisp_Object \\(Q[a-z_]+\\)")
      (setq symbol-list
	    (cons (buffer-substring (match-beginning 1) (match-end 1))
		  symbol-list))
      (forward-line 1))
    (setq symbol-list (nreverse symbol-list))
    ;; Delete the section of syms_of_... where we initialize the symbols.
    (search-forward "\n  /\*&&& init symbols here &&&*\/\n")
    (let ((start (point)))
      (while (looking-at "^  Q")
	(forward-line 2))
      (kill-region start (point)))
    ;; Write a new symbol initialization section.
    (while symbol-list
      (insert (format "  %s = intern (\"" (car symbol-list)))
      (let ((start (point)))
	(insert (substring (car symbol-list) 1))
	(subst-char-in-region start (point) ?_ ?-))
      (insert (format "\");\n  staticpro (&%s);\n" (car symbol-list)))
      (setq symbol-list (cdr symbol-list)))))

  */        

/*&&& symbols declared here &&&*/
Lisp_Object Qauto_raise;
Lisp_Object Qauto_lower;
Lisp_Object Qbackground_color;
Lisp_Object Qbar;
Lisp_Object Qborder_color;
Lisp_Object Qborder_width;
Lisp_Object Qbox;
Lisp_Object Qcursor_color;
Lisp_Object Qcursor_type;
Lisp_Object Qfont;
Lisp_Object Qforeground_color;
Lisp_Object Qgeometry;
/* Lisp_Object Qicon; */
Lisp_Object Qicon_left;
Lisp_Object Qicon_top;
Lisp_Object Qicon_type;
Lisp_Object Qinternal_border_width;
Lisp_Object Qleft;
Lisp_Object Qmouse_color;
Lisp_Object Qnone;
Lisp_Object Qparent_id;
Lisp_Object Qsuppress_icon;
Lisp_Object Qtop;
Lisp_Object Qundefined_color;
Lisp_Object Qvertical_scroll_bars;
Lisp_Object Qvisibility;
Lisp_Object Qwindow_id;
Lisp_Object Qx_frame_parameter;

/* The below are defined in frame.c. */
extern Lisp_Object Qheight, Qminibuffer, Qname, Qonly, Qwidth;
extern Lisp_Object Qunsplittable, Qmenu_bar_lines;

extern Lisp_Object Vwindow_system_version;


/* Error if we are not connected to X.  */
static void
check_x ()
{
  if (x_current_display == 0)
    error ("X windows are not in use or not initialized");
}

/* Return the Emacs frame-object corresponding to an X window.
   It could be the frame's main window or an icon window.  */

/* This function can be called during GC, so use XGCTYPE.  */

struct frame *
x_window_to_frame (wdesc)
     int wdesc;
{
  Lisp_Object tail, frame;
  struct frame *f;

  for (tail = Vframe_list; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      frame = XCONS (tail)->car;
      if (XGCTYPE (frame) != Lisp_Frame)
        continue;
      f = XFRAME (frame);
      if (FRAME_X_WINDOW (f) == wdesc
          || f->display.x->icon_desc == wdesc)
        return f;
    }
  return 0;
}


/* Connect the frame-parameter names for X frames
   to the ways of passing the parameter values to the window system.

   The name of a parameter, as a Lisp symbol,
   has an `x-frame-parameter' property which is an integer in Lisp
   but can be interpreted as an `enum x_frame_parm' in C.  */

enum x_frame_parm
{
  X_PARM_FOREGROUND_COLOR,
  X_PARM_BACKGROUND_COLOR,
  X_PARM_MOUSE_COLOR,
  X_PARM_CURSOR_COLOR,
  X_PARM_BORDER_COLOR,
  X_PARM_ICON_TYPE,
  X_PARM_FONT,
  X_PARM_BORDER_WIDTH,
  X_PARM_INTERNAL_BORDER_WIDTH,
  X_PARM_NAME,
  X_PARM_AUTORAISE,
  X_PARM_AUTOLOWER,
  X_PARM_VERT_SCROLL_BAR,
  X_PARM_VISIBILITY,
  X_PARM_MENU_BAR_LINES
};


struct x_frame_parm_table
{
  char *name;
  void (*setter)( /* struct frame *frame, Lisp_Object val, oldval */ );
};

void x_set_foreground_color ();
void x_set_background_color ();
void x_set_mouse_color ();
void x_set_cursor_color ();
void x_set_border_color ();
void x_set_cursor_type ();
void x_set_icon_type ();
void x_set_font ();
void x_set_border_width ();
void x_set_internal_border_width ();
void x_explicitly_set_name ();
void x_set_autoraise ();
void x_set_autolower ();
void x_set_vertical_scroll_bars ();
void x_set_visibility ();
void x_set_menu_bar_lines ();

static struct x_frame_parm_table x_frame_parms[] =
{
  "foreground-color", x_set_foreground_color,
  "background-color", x_set_background_color,
  "mouse-color", x_set_mouse_color,
  "cursor-color", x_set_cursor_color,
  "border-color", x_set_border_color,
  "cursor-type", x_set_cursor_type,
  "icon-type", x_set_icon_type,
  "font", x_set_font,
  "border-width", x_set_border_width,
  "internal-border-width", x_set_internal_border_width,
  "name", x_explicitly_set_name,
  "auto-raise", x_set_autoraise,
  "auto-lower", x_set_autolower,
  "vertical-scroll-bars", x_set_vertical_scroll_bars,
  "visibility", x_set_visibility,
  "menu-bar-lines", x_set_menu_bar_lines,
};

/* Attach the `x-frame-parameter' properties to
   the Lisp symbol names of parameters relevant to X.  */

init_x_parm_symbols ()
{
  int i;

  for (i = 0; i < sizeof (x_frame_parms) / sizeof (x_frame_parms[0]); i++)
    Fput (intern (x_frame_parms[i].name), Qx_frame_parameter,
	  make_number (i));
}

/* Change the parameters of FRAME as specified by ALIST.
   If a parameter is not specially recognized, do nothing;
   otherwise call the `x_set_...' function for that parameter.  */

void
x_set_frame_parameters (f, alist)
     FRAME_PTR f;
     Lisp_Object alist;
{
  Lisp_Object tail;

  /* If both of these parameters are present, it's more efficient to
     set them both at once.  So we wait until we've looked at the
     entire list before we set them.  */
  Lisp_Object width, height;

  /* Same here.  */
  Lisp_Object left, top;

  /* Record in these vectors all the parms specified.  */
  Lisp_Object *parms;
  Lisp_Object *values;
  int i;
  
  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    i++;

  parms = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));
  values = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));

  /* Extract parm names and values into those vectors.  */

  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt, prop, val;

      elt = Fcar (tail);
      parms[i] = Fcar (elt);
      values[i] = Fcdr (elt);
      i++;
    }

  width = height = top = left = Qunbound;

  /* Now process them in reverse of specified order.  */
  for (i--; i >= 0; i--)
    {
      Lisp_Object prop, val;

      prop = parms[i];
      val = values[i];

      if (EQ (prop, Qwidth))
	width = val;
      else if (EQ (prop, Qheight))
	height = val;
      else if (EQ (prop, Qtop))
	top = val;
      else if (EQ (prop, Qleft))
	left = val;
      else
	{
	  register Lisp_Object param_index = Fget (prop, Qx_frame_parameter);
	  register Lisp_Object old_value = get_frame_param (f, prop);

	  store_frame_param (f, prop, val);
	  if (XTYPE (param_index) == Lisp_Int
	      && XINT (param_index) >= 0
	      && (XINT (param_index)
		  < sizeof (x_frame_parms)/sizeof (x_frame_parms[0])))
	    (*x_frame_parms[XINT (param_index)].setter)(f, val, old_value);
	}
    }

  /* Don't die if just one of these was set.  */
  if (EQ (left, Qunbound))
    XSET (left, Lisp_Int, f->display.x->left_pos);
  if (EQ (top, Qunbound))
    XSET (top, Lisp_Int, f->display.x->top_pos);

  /* Don't die if just one of these was set.  */
  if (EQ (width, Qunbound))
    XSET (width, Lisp_Int, FRAME_WIDTH (f));
  if (EQ (height, Qunbound))
    XSET (height, Lisp_Int, FRAME_HEIGHT (f));

  /* Don't set these parameters these unless they've been explicitly
     specified.  The window might be mapped or resized while we're in
     this function, and we don't want to override that unless the lisp
     code has asked for it.

     Don't set these parameters unless they actually differ from the
     window's current parameters; the window may not actually exist
     yet.  */
  {
    Lisp_Object frame;

    check_frame_size (f, &height, &width);

    XSET (frame, Lisp_Frame, f);

    if ((NUMBERP (width) && XINT (width) != FRAME_WIDTH (f))
	|| (NUMBERP (height) && XINT (height) != FRAME_HEIGHT (f)))
      Fset_frame_size (frame, width, height);
    if ((NUMBERP (left) && XINT (left) != f->display.x->left_pos)
	|| (NUMBERP (top) && XINT (top) != f->display.x->top_pos))
      Fset_frame_position (frame, left, top);
  }
}

/* Insert a description of internally-recorded parameters of frame X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to the X window system
   and whose values are not correctly recorded in the frame's
   param_alist need to be considered here.  */

x_report_frame_params (f, alistptr)
     struct frame *f;
     Lisp_Object *alistptr;
{
  char buf[16];

  store_in_alist (alistptr, Qleft, make_number (f->display.x->left_pos));
  store_in_alist (alistptr, Qtop, make_number (f->display.x->top_pos));
  store_in_alist (alistptr, Qborder_width,
       	   make_number (f->display.x->border_width));
  store_in_alist (alistptr, Qinternal_border_width,
       	   make_number (f->display.x->internal_border_width));
  sprintf (buf, "%d", FRAME_X_WINDOW (f));
  store_in_alist (alistptr, Qwindow_id,
       	   build_string (buf));
  store_in_alist (alistptr, Qvisibility,
		  (FRAME_VISIBLE_P (f) ? Qt
		   : FRAME_ICONIFIED_P (f) ? Qicon : Qnil));
}

/* Decide if color named COLOR is valid for the display
   associated with the selected frame. */
int
defined_color (color, color_def)
     char *color;
     Color *color_def;
{
  register int foo;
  Colormap screen_colormap;

  BLOCK_INPUT;
#ifdef HAVE_X11
  screen_colormap
    = DefaultColormap (x_current_display, XDefaultScreen (x_current_display));

  foo = XParseColor (x_current_display, screen_colormap,
       	      color, color_def)
    && XAllocColor (x_current_display, screen_colormap, color_def);
#else
  foo = XParseColor (color, color_def) && XGetHardwareColor (color_def);
#endif /* not HAVE_X11 */
  UNBLOCK_INPUT;

  if (foo)
    return 1;
  else
    return 0;
}

/* Given a string ARG naming a color, compute a pixel value from it
   suitable for screen F.
   If F is not a color screen, return DEF (default) regardless of what
   ARG says.  */

int
x_decode_color (arg, def)
     Lisp_Object arg;
     int def;
{
  Color cdef;

  CHECK_STRING (arg, 0);

  if (strcmp (XSTRING (arg)->data, "black") == 0)
    return BLACK_PIX_DEFAULT;
  else if (strcmp (XSTRING (arg)->data, "white") == 0)
    return WHITE_PIX_DEFAULT;

#ifdef HAVE_X11
  if (x_screen_planes == 1)
    return def;
#else
  if (DISPLAY_CELLS == 1)
    return def;
#endif

  if (defined_color (XSTRING (arg)->data, &cdef))
    return cdef.pixel;
  else
    Fsignal (Qundefined_color, Fcons (arg, Qnil));
}

/* Functions called only from `x_set_frame_param'
   to set individual parameters.

   If FRAME_X_WINDOW (f) is 0,
   the frame is being created and its X-window does not exist yet.
   In that case, just record the parameter's new value
   in the standard place; do not attempt to change the window.  */

void
x_set_foreground_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->display.x->foreground_pixel = x_decode_color (arg, BLACK_PIX_DEFAULT);
  if (FRAME_X_WINDOW (f) != 0)
    {
#ifdef HAVE_X11
      BLOCK_INPUT;
      XSetForeground (x_current_display, f->display.x->normal_gc,
		      f->display.x->foreground_pixel);
      XSetBackground (x_current_display, f->display.x->reverse_gc,
		      f->display.x->foreground_pixel);
      UNBLOCK_INPUT;
#endif				/* HAVE_X11 */
      recompute_basic_faces (f);
      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

void
x_set_background_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  Pixmap temp;
  int mask;

  f->display.x->background_pixel = x_decode_color (arg, WHITE_PIX_DEFAULT);

  if (FRAME_X_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
#ifdef HAVE_X11
      /* The main frame area. */
      XSetBackground (x_current_display, f->display.x->normal_gc,
		      f->display.x->background_pixel);
      XSetForeground (x_current_display, f->display.x->reverse_gc,
		      f->display.x->background_pixel);
      XSetForeground (x_current_display, f->display.x->cursor_gc,
		      f->display.x->background_pixel);
      XSetWindowBackground (x_current_display, FRAME_X_WINDOW (f),
			    f->display.x->background_pixel);

#else
      temp = XMakeTile (f->display.x->background_pixel);
      XChangeBackground (FRAME_X_WINDOW (f), temp);
      XFreePixmap (temp);
#endif				/* not HAVE_X11 */
      UNBLOCK_INPUT;

      recompute_basic_faces (f);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

void
x_set_mouse_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  Cursor cursor, nontext_cursor, mode_cursor;
  int mask_color;

  if (!EQ (Qnil, arg))
    f->display.x->mouse_pixel = x_decode_color (arg, BLACK_PIX_DEFAULT);
  mask_color = f->display.x->background_pixel;
				/* No invisible pointers. */
  if (mask_color == f->display.x->mouse_pixel
	&& mask_color == f->display.x->background_pixel)
    f->display.x->mouse_pixel = f->display.x->foreground_pixel;

  BLOCK_INPUT;
#ifdef HAVE_X11

  /* It's not okay to crash if the user selects a screwy cursor.  */
  x_catch_errors ();

  if (!EQ (Qnil, Vx_pointer_shape))
    {
      CHECK_NUMBER (Vx_pointer_shape, 0);
      cursor = XCreateFontCursor (x_current_display, XINT (Vx_pointer_shape));
    }
  else
    cursor = XCreateFontCursor (x_current_display, XC_xterm);
  x_check_errors ("bad text pointer cursor: %s");

  if (!EQ (Qnil, Vx_nontext_pointer_shape))
    {
      CHECK_NUMBER (Vx_nontext_pointer_shape, 0);
      nontext_cursor = XCreateFontCursor (x_current_display,
					  XINT (Vx_nontext_pointer_shape));
    }
  else
    nontext_cursor = XCreateFontCursor (x_current_display, XC_left_ptr);
  x_check_errors ("bad nontext pointer cursor: %s");

  if (!EQ (Qnil, Vx_mode_pointer_shape))
    {
      CHECK_NUMBER (Vx_mode_pointer_shape, 0);
      mode_cursor = XCreateFontCursor (x_current_display,
					  XINT (Vx_mode_pointer_shape));
    }
  else
    mode_cursor = XCreateFontCursor (x_current_display, XC_xterm);

  /* Check and report errors with the above calls.  */
  x_check_errors ("can't set cursor shape: %s");
  x_uncatch_errors ();

  {
    XColor fore_color, back_color;

    fore_color.pixel = f->display.x->mouse_pixel;
    back_color.pixel = mask_color;
    XQueryColor (x_current_display,
		 DefaultColormap (x_current_display,
				  DefaultScreen (x_current_display)),
		 &fore_color);
    XQueryColor (x_current_display,
		 DefaultColormap (x_current_display,
				  DefaultScreen (x_current_display)),
		 &back_color);
    XRecolorCursor (x_current_display, cursor,
		    &fore_color, &back_color);
    XRecolorCursor (x_current_display, nontext_cursor,
		    &fore_color, &back_color);
    XRecolorCursor (x_current_display, mode_cursor,
		    &fore_color, &back_color);
  }
#else /* X10 */
  cursor = XCreateCursor (16, 16, MouseCursor, MouseMask,
			  0, 0,
			  f->display.x->mouse_pixel,
			  f->display.x->background_pixel,
			  GXcopy);
#endif /* X10 */

  if (FRAME_X_WINDOW (f) != 0)
    {
      XDefineCursor (XDISPLAY FRAME_X_WINDOW (f), cursor);
    }

  if (cursor != f->display.x->text_cursor && f->display.x->text_cursor != 0)
      XFreeCursor (XDISPLAY f->display.x->text_cursor);
  f->display.x->text_cursor = cursor;
#ifdef HAVE_X11
  if (nontext_cursor != f->display.x->nontext_cursor
      && f->display.x->nontext_cursor != 0)
      XFreeCursor (XDISPLAY f->display.x->nontext_cursor);
  f->display.x->nontext_cursor = nontext_cursor;

  if (mode_cursor != f->display.x->modeline_cursor
      && f->display.x->modeline_cursor != 0)
      XFreeCursor (XDISPLAY f->display.x->modeline_cursor);
  f->display.x->modeline_cursor = mode_cursor;
#endif	/* HAVE_X11 */

  XFlushQueue ();
  UNBLOCK_INPUT;
}

void
x_set_cursor_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned long fore_pixel;

  if (!EQ (Vx_cursor_fore_pixel, Qnil))
    fore_pixel = x_decode_color (Vx_cursor_fore_pixel, WHITE_PIX_DEFAULT);
  else
    fore_pixel = f->display.x->background_pixel;
  f->display.x->cursor_pixel = x_decode_color (arg, BLACK_PIX_DEFAULT);
  
  /* Make sure that the cursor color differs from the background color.  */
  if (f->display.x->cursor_pixel == f->display.x->background_pixel)
    {
      f->display.x->cursor_pixel == f->display.x->mouse_pixel;
      if (f->display.x->cursor_pixel == fore_pixel)
	fore_pixel = f->display.x->background_pixel;
    }
  f->display.x->cursor_foreground_pixel = fore_pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
#ifdef HAVE_X11
      BLOCK_INPUT;
      XSetBackground (x_current_display, f->display.x->cursor_gc,
		      f->display.x->cursor_pixel);
      XSetForeground (x_current_display, f->display.x->cursor_gc,
		      fore_pixel);
      UNBLOCK_INPUT;
#endif /* HAVE_X11 */

      if (FRAME_VISIBLE_P (f))
	{
	  x_display_cursor (f, 0);
	  x_display_cursor (f, 1);
	}
    }
}

/* Set the border-color of frame F to value described by ARG.
   ARG can be a string naming a color.
   The border-color is used for the border that is drawn by the X server.
   Note that this does not fully take effect if done before
   F has an x-window; it must be redone when the window is created.

   Note: this is done in two routines because of the way X10 works.

   Note: under X11, this is normally the province of the window manager,
   and so emacs' border colors may be overridden. */

void
x_set_border_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned char *str;
  int pix;

  CHECK_STRING (arg, 0);
  str = XSTRING (arg)->data;

#ifndef HAVE_X11
  if (!strcmp (str, "grey") || !strcmp (str, "Grey")
      || !strcmp (str, "gray") || !strcmp (str, "Gray"))
    pix = -1;
  else
#endif /* X10 */

    pix = x_decode_color (arg, BLACK_PIX_DEFAULT);

  x_set_border_pixel (f, pix);
}

/* Set the border-color of frame F to pixel value PIX.
   Note that this does not fully take effect if done before
   F has an x-window.  */

x_set_border_pixel (f, pix)
     struct frame *f;
     int pix;
{
  f->display.x->border_pixel = pix;

  if (FRAME_X_WINDOW (f) != 0 && f->display.x->border_width > 0)
    {
      Pixmap temp;
      int mask;

      BLOCK_INPUT;
#ifdef HAVE_X11
      XSetWindowBorder (x_current_display, FRAME_X_WINDOW (f),
       		 pix);
#else
      if (pix < 0)
        temp = XMakePixmap ((Bitmap) XStoreBitmap (gray_width, gray_height,
						   gray_bits),
       		     BLACK_PIX_DEFAULT, WHITE_PIX_DEFAULT);
      else
        temp = XMakeTile (pix);
      XChangeBorder (FRAME_X_WINDOW (f), temp);
      XFreePixmap (XDISPLAY temp);
#endif /* not HAVE_X11 */
      UNBLOCK_INPUT;

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

void
x_set_cursor_type (f, arg, oldval)
     FRAME_PTR f;
     Lisp_Object arg, oldval;
{
  if (EQ (arg, Qbar))
    FRAME_DESIRED_CURSOR (f) = bar_cursor;
  else
#if 0
    if (EQ (arg, Qbox))
#endif
      FRAME_DESIRED_CURSOR (f) = filled_box_cursor;
  /* Error messages commented out because people have trouble fixing
     .Xdefaults with Emacs, when it has something bad in it.  */
#if 0
  else
    error
      ("the `cursor-type' frame parameter should be either `bar' or `box'");
#endif

  /* Make sure the cursor gets redrawn.  This is overkill, but how
     often do people change cursor types?  */
  update_mode_lines++;
}

void
x_set_icon_type (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  Lisp_Object tem;
  int result;

  if (EQ (oldval, Qnil) == EQ (arg, Qnil))
    return;

  BLOCK_INPUT;
  if (NILP (arg))
    result = x_text_icon (f, 0);
  else
    result = x_bitmap_icon (f);

  if (result)
    {
      UNBLOCK_INPUT;
      error ("No icon window available.");
    }

  /* If the window was unmapped (and its icon was mapped),
     the new icon is not mapped, so map the window in its stead.  */
  if (FRAME_VISIBLE_P (f))
    XMapWindow (XDISPLAY FRAME_X_WINDOW (f));

  XFlushQueue ();
  UNBLOCK_INPUT;
}

extern Lisp_Object x_new_font ();

void
x_set_font (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  Lisp_Object result;

  CHECK_STRING (arg, 1);

  BLOCK_INPUT;
  result = x_new_font (f, XSTRING (arg)->data);
  UNBLOCK_INPUT;
  
  if (EQ (result, Qnil))
    error ("Font \"%s\" is not defined", XSTRING (arg)->data);
  else if (EQ (result, Qt))
    error ("the characters of the given font have varying widths");
  else if (STRINGP (result))
    {
      recompute_basic_faces (f);
      store_frame_param (f, Qfont, result);
    }
  else
    abort ();
}

void
x_set_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  CHECK_NUMBER (arg, 0);

  if (XINT (arg) == f->display.x->border_width)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    error ("Cannot change the border width of a window");

  f->display.x->border_width = XINT (arg);
}

void
x_set_internal_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int mask;
  int old = f->display.x->internal_border_width;

  CHECK_NUMBER (arg, 0);
  f->display.x->internal_border_width = XINT (arg);
  if (f->display.x->internal_border_width < 0)
    f->display.x->internal_border_width = 0;

  if (f->display.x->internal_border_width == old)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      x_set_window_size (f, f->width, f->height);
#if 0
      x_set_resize_hint (f);
#endif
      XFlushQueue ();
      UNBLOCK_INPUT;
      SET_FRAME_GARBAGED (f);
    }
}

void
x_set_visibility (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  Lisp_Object frame;
  XSET (frame, Lisp_Frame, f);

  if (NILP (value))
    Fmake_frame_invisible (frame);
  else if (EQ (value, Qicon))
    Ficonify_frame (frame);
  else
    Fmake_frame_visible (frame);
}

static void
x_set_menu_bar_lines_1 (window, n)
  Lisp_Object window;
  int n;
{
  struct window *w = XWINDOW (window);

  XFASTINT (w->top) += n;
  XFASTINT (w->height) -= n;

  /* Handle just the top child in a vertical split.  */
  if (!NILP (w->vchild))
    x_set_menu_bar_lines_1 (w->vchild, n);

  /* Adjust all children in a horizontal split.  */
  for (window = w->hchild; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      x_set_menu_bar_lines_1 (window, n);
    }
}

void
x_set_menu_bar_lines (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  int nlines;
  int olines = FRAME_MENU_BAR_LINES (f);

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itslef, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (XTYPE (value) == Lisp_Int)
    nlines = XINT (value);
  else
    nlines = 0;

  FRAME_MENU_BAR_LINES (f) = nlines;
  x_set_menu_bar_lines_1 (f->root_window, nlines - olines);
}

/* Change the name of frame F to NAME.  If NAME is nil, set F's name to
       x_id_name.

   If EXPLICIT is non-zero, that indicates that lisp code is setting the
       name; if NAME is a string, set F's name to NAME and set
       F->explicit_name; if NAME is Qnil, then clear F->explicit_name.

   If EXPLICIT is zero, that indicates that Emacs redisplay code is
       suggesting a new name, which lisp code should override; if
       F->explicit_name is set, ignore the new name; otherwise, set it.  */

void
x_set_name (f, name, explicit)
     struct frame *f;
     Lisp_Object name;
     int explicit;
{
  /* Make sure that requests from lisp code override requests from 
     Emacs redisplay code.  */
  if (explicit)
    {
      /* If we're switching from explicit to implicit, we had better
	 update the mode lines and thereby update the title.  */
      if (f->explicit_name && NILP (name))
	update_mode_lines = 1;

      f->explicit_name = ! NILP (name);
    }
  else if (f->explicit_name)
    return;

  /* If NAME is nil, set the name to the x_id_name.  */
  if (NILP (name))
    name = build_string (x_id_name);
  else
    CHECK_STRING (name, 0);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  if (FRAME_X_WINDOW (f))
    {
      BLOCK_INPUT;

#ifdef HAVE_X11R4
      {
	XTextProperty text;
	text.value = XSTRING (name)->data;
	text.encoding = XA_STRING;
	text.format = 8;
	text.nitems = XSTRING (name)->size;
	XSetWMName (x_current_display, FRAME_X_WINDOW (f), &text);
	XSetWMIconName (x_current_display, FRAME_X_WINDOW (f), &text);
      }
#else
      XSetIconName (XDISPLAY FRAME_X_WINDOW (f),
		    XSTRING (name)->data);
      XStoreName (XDISPLAY FRAME_X_WINDOW (f),
		  XSTRING (name)->data);
#endif

      UNBLOCK_INPUT;
    }

  f->name = name;
}

/* This function should be called when the user's lisp code has
   specified a name for the frame; the name will override any set by the
   redisplay code.  */
void
x_explicitly_set_name (f, arg, oldval)
     FRAME_PTR f;
     Lisp_Object arg, oldval;
{
  x_set_name (f, arg, 1);
}

/* This function should be called by Emacs redisplay code to set the
   name; names set this way will never override names set by the user's
   lisp code.  */
void
x_implicitly_set_name (f, arg, oldval)
     FRAME_PTR f;
     Lisp_Object arg, oldval;
{
  x_set_name (f, arg, 0);
}

void
x_set_autoraise (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->auto_raise = !EQ (Qnil, arg);
}

void
x_set_autolower (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->auto_lower = !EQ (Qnil, arg);
}

void
x_set_vertical_scroll_bars (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  if (NILP (arg) != ! FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    {
      FRAME_HAS_VERTICAL_SCROLL_BARS (f) = ! NILP (arg);

      /* We set this parameter before creating the X window for the
	 frame, so we can get the geometry right from the start.
	 However, if the window hasn't been created yet, we shouldn't
	 call x_set_window_size.  */
      if (FRAME_X_WINDOW (f))
	x_set_window_size (f, FRAME_WIDTH (f), FRAME_HEIGHT (f));
    }
}

/* Subroutines of creating an X frame.  */

#ifdef HAVE_X11

/* Make sure that Vx_resource_name is set to a reasonable value.  */
static void
validate_x_resource_name ()
{
  if (! STRINGP (Vx_resource_name))
    Vx_resource_name = make_string ("emacs", 5);
}


extern char *x_get_string_resource ();
extern XrmDatabase x_load_resources ();

DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 2, 4, 0,
  "Return the value of ATTRIBUTE, of class CLASS, from the X defaults database.\n\
This uses `NAME.ATTRIBUTE' as the key and `Emacs.CLASS' as the\n\
class, where INSTANCE is the name under which Emacs was invoked, or\n\
the name specified by the `-name' or `-rn' command-line arguments.\n\
\n\
The optional arguments COMPONENT and SUBCLASS add to the key and the\n\
class, respectively.  You must specify both of them or neither.\n\
If you specify them, the key is `NAME.COMPONENT.ATTRIBUTE'\n\
and the class is `Emacs.CLASS.SUBCLASS'.")
  (attribute, class, component, subclass)
     Lisp_Object attribute, class, component, subclass;
{
  register char *value;
  char *name_key;
  char *class_key;

  check_x ();

  CHECK_STRING (attribute, 0);
  CHECK_STRING (class, 0);

  if (!NILP (component))
    CHECK_STRING (component, 1);
  if (!NILP (subclass))
    CHECK_STRING (subclass, 2);
  if (NILP (component) != NILP (subclass))
    error ("x-get-resource: must specify both COMPONENT and SUBCLASS or neither");

  validate_x_resource_name ();

  if (NILP (component))
    {
      /* Allocate space for the components, the dots which separate them,
	 and the final '\0'.  */
      name_key = (char *) alloca (XSTRING (Vx_resource_name)->size
				  + XSTRING (attribute)->size
				  + 2);
      class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
				   + XSTRING (class)->size
				   + 2);

      sprintf (name_key, "%s.%s",
	       XSTRING (Vx_resource_name)->data,
	       XSTRING (attribute)->data);
      sprintf (class_key, "%s.%s",
	       EMACS_CLASS,
	       XSTRING (class)->data);
    }
  else
    {
      name_key = (char *) alloca (XSTRING (Vx_resource_name)->size
				  + XSTRING (component)->size
				  + XSTRING (attribute)->size
				  + 3);

      class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
				   + XSTRING (class)->size
				   + XSTRING (subclass)->size
				   + 3);

      sprintf (name_key, "%s.%s.%s",
	       XSTRING (Vx_resource_name)->data,
	       XSTRING (component)->data,
	       XSTRING (attribute)->data);
      sprintf (class_key, "%s.%s.%s",
	       EMACS_CLASS,
	       XSTRING (class)->data,
	       XSTRING (subclass)->data);
    }

  value = x_get_string_resource (xrdb, name_key, class_key);

  if (value != (char *) 0)
    return build_string (value);
  else
    return Qnil;
}

/* Used when C code wants a resource value.  */

char *
x_get_resource_string (attribute, class)
     char *attribute, *class;
{
  register char *value;
  char *name_key;
  char *class_key;

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  */
  name_key = (char *) alloca (XSTRING (Vinvocation_name)->size
			      + strlen (attribute) + 2);
  class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
			       + strlen (class) + 2);

  sprintf (name_key, "%s.%s",
	   XSTRING (Vinvocation_name)->data,
	   attribute);
  sprintf (class_key, "%s.%s", EMACS_CLASS, class);

  return x_get_string_resource (xrdb, name_key, class_key);
}

#else	/* X10 */

DEFUN ("x-get-default", Fx_get_default, Sx_get_default, 1, 1, 0,
  "Get X default ATTRIBUTE from the system, or nil if no default.\n\
Value is a string (when not nil) and ATTRIBUTE is also a string.\n\
The defaults are specified in the file `~/.Xdefaults'.")
  (arg)
     Lisp_Object arg;
{
  register unsigned char *value;

  CHECK_STRING (arg, 1);

  value = (unsigned char *) XGetDefault (XDISPLAY 
					 XSTRING (Vinvocation_name)->data,
					 XSTRING (arg)->data);
  if (value == 0)
    /* Try reversing last two args, in case this is the buggy version of X.  */
    value = (unsigned char *) XGetDefault (XDISPLAY
					   XSTRING (arg)->data,
					   XSTRING (Vinvocation_name)->data);
  if (value != 0)
    return build_string (value);
  else
    return (Qnil);
}

#define Fx_get_resource(attribute, class, component, subclass) \
  Fx_get_default(attribute)

#endif	/* X10 */

/* Types we might convert a resource string into.  */
enum resource_types
  {
    number, boolean, string, symbol
  };

/* Return the value of parameter PARAM.

   First search ALIST, then Vdefault_frame_alist, then the X defaults
   database, using ATTRIBUTE as the attribute name and CLASS as its class.

   Convert the resource to the type specified by desired_type.

   If no default is specified, return Qunbound.  If you call
   x_get_arg, make sure you deal with Qunbound in a reasonable way,
   and don't let it get stored in any lisp-visible variables!  */

static Lisp_Object
x_get_arg (alist, param, attribute, class, type)
     Lisp_Object alist, param;
     char *attribute;
     char *class;
     enum resource_types type;
{
  register Lisp_Object tem;

  tem = Fassq (param, alist);
  if (EQ (tem, Qnil))
    tem = Fassq (param, Vdefault_frame_alist);
  if (EQ (tem, Qnil))
    {

      if (attribute)
	{
	  tem = Fx_get_resource (build_string (attribute),
				 build_string (class),
				 Qnil, Qnil);

	  if (NILP (tem))
	    return Qunbound;

	  switch (type)
	    {
	    case number:
	      return make_number (atoi (XSTRING (tem)->data));

	    case boolean:
	      tem = Fdowncase (tem);
	      if (!strcmp (XSTRING (tem)->data, "on")
		  || !strcmp (XSTRING (tem)->data, "true"))
		return Qt;
	      else 
		return Qnil;

	    case string:
	      return tem;

	    case symbol:
	      /* As a special case, we map the values `true' and `on'
		 to Qt, and `false' and `off' to Qnil.  */
	      {
		Lisp_Object lower = Fdowncase (tem);
		if (!strcmp (XSTRING (tem)->data, "on")
		    || !strcmp (XSTRING (tem)->data, "true"))
		  return Qt;
		else if (!strcmp (XSTRING (tem)->data, "off")
		      || !strcmp (XSTRING (tem)->data, "false"))
		  return Qnil;
		else
		  return Fintern (tem, Qnil);
	      }

	    default:
	      abort ();
	    }
	}
      else
	return Qunbound;
    }
  return Fcdr (tem);
}

/* Record in frame F the specified or default value according to ALIST
   of the parameter named PARAM (a Lisp symbol).
   If no value is specified for PARAM, look for an X default for XPROP
   on the frame named NAME.
   If that is not found either, use the value DEFLT.  */

static Lisp_Object
x_default_parameter (f, alist, prop, deflt, xprop, xclass, type)
     struct frame *f;
     Lisp_Object alist;
     Lisp_Object prop;
     Lisp_Object deflt;
     char *xprop;
     char *xclass;
     enum resource_types type;
{
  Lisp_Object tem;

  tem = x_get_arg (alist, prop, xprop, xclass, type);
  if (EQ (tem, Qunbound))
    tem = deflt;
  x_set_frame_parameters (f, Fcons (Fcons (prop, tem), Qnil));
  return tem;
}

DEFUN ("x-parse-geometry", Fx_parse_geometry, Sx_parse_geometry, 1, 1, 0,
       "Parse an X-style geometry string STRING.\n\
Returns an alist of the form ((top . TOP), (left . LEFT) ... ).")
     (string)
     Lisp_Object string;
{
  int geometry, x, y;
  unsigned int width, height;
  Lisp_Object values[4];

  CHECK_STRING (string, 0);

  geometry = XParseGeometry ((char *) XSTRING (string)->data,
			     &x, &y, &width, &height);

  switch (geometry & 0xf)	/* Mask out {X,Y}Negative */
    {
    case (XValue | YValue):
      /* What's one pixel among friends?
	 Perhaps fix this some day by returning symbol `extreme-top'... */
      if (x == 0 && (geometry & XNegative))
	x = -1;
      if (y == 0 && (geometry & YNegative))
	y = -1;
      values[0] = Fcons (Qleft, make_number (x));
      values[1] = Fcons (Qtop, make_number (y));
      return Flist (2, values);
      break;

    case (WidthValue | HeightValue):
      values[0] = Fcons (Qwidth, make_number (width));
      values[1] = Fcons (Qheight, make_number (height));
      return Flist (2, values);
      break;

    case (XValue | YValue | WidthValue | HeightValue):
      if (x == 0 && (geometry & XNegative))
	x = -1;
      if (y == 0 && (geometry & YNegative))
	y = -1;
      values[0] = Fcons (Qwidth, make_number (width));
      values[1] = Fcons (Qheight, make_number (height));
      values[2] = Fcons (Qleft, make_number (x));
      values[3] = Fcons (Qtop, make_number (y));
      return Flist (4, values);
      break;

    case 0:
	return Qnil;
      
    default:
      error ("Must specify x and y value, and/or width and height");
    }
}

#ifdef HAVE_X11
/* Calculate the desired size and position of this window,
   or set rubber-band prompting if none. */

#define DEFAULT_ROWS 40
#define DEFAULT_COLS 80

static int
x_figure_window_size (f, parms)
     struct frame *f;
     Lisp_Object parms;
{
  register Lisp_Object tem0, tem1;
  int height, width, left, top;
  register int geometry;
  long window_prompting = 0;

  /* Default values if we fall through.
     Actually, if that happens we should get
     window manager prompting. */
  f->width = DEFAULT_COLS;
  f->height = DEFAULT_ROWS;
  /* Window managers expect that if program-specified
     positions are not (0,0), they're intentional, not defaults.  */
  f->display.x->top_pos = 0;
  f->display.x->left_pos = 0;

  tem0 = x_get_arg (parms, Qheight, 0, 0, number);
  tem1 = x_get_arg (parms, Qwidth, 0, 0, number);
  if (! EQ (tem0, Qunbound) && ! EQ (tem1, Qunbound))
    {
      CHECK_NUMBER (tem0, 0);
      CHECK_NUMBER (tem1, 0);
      f->height = XINT (tem0);
      f->width = XINT (tem1);
      window_prompting |= USSize;
    }
  else if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    error ("Must specify *both* height and width");

  f->display.x->vertical_scroll_bar_extra
    = (FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? VERTICAL_SCROLL_BAR_PIXEL_WIDTH (f)
       : 0);
  f->display.x->pixel_width = CHAR_TO_PIXEL_WIDTH (f, f->width);
  f->display.x->pixel_height = CHAR_TO_PIXEL_HEIGHT (f, f->height);

  tem0 = x_get_arg (parms, Qtop, 0, 0, number);
  tem1 = x_get_arg (parms, Qleft, 0, 0, number);
  if (! EQ (tem0, Qunbound) && ! EQ (tem1, Qunbound))
    {
      CHECK_NUMBER (tem0, 0);
      CHECK_NUMBER (tem1, 0);
      f->display.x->top_pos = XINT (tem0);
      f->display.x->left_pos = XINT (tem1);
      x_calc_absolute_position (f);
      window_prompting |= USPosition;
    }
  else if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    error ("Must specify *both* top and left corners");

#if 0 /* PPosition and PSize mean "specified explicitly,
	 by the program rather than by the user".  So it is wrong to
	 set them if nothing was specified.  */
  switch (window_prompting)
    {
    case USSize | USPosition:
      return window_prompting;
      break;

    case USSize:		/* Got the size, need the position. */
      window_prompting |= PPosition;
      return window_prompting;
      break;
	  
    case USPosition:		/* Got the position, need the size. */
      window_prompting |= PSize;
      return window_prompting;
      break;
	  
    case 0:			/* Got nothing, take both from geometry. */
      window_prompting |= PPosition | PSize;
      return window_prompting;
      break;
	  
    default:
      /* Somehow a bit got set in window_prompting that we didn't
	 put there.  */
      abort ();
    }
#endif
  return window_prompting;
}

static void
x_window (f)
     struct frame *f;
{
  XSetWindowAttributes attributes;
  unsigned long attribute_mask;
  XClassHint class_hints;

  attributes.background_pixel = f->display.x->background_pixel;
  attributes.border_pixel = f->display.x->border_pixel;
  attributes.bit_gravity = StaticGravity;
  attributes.backing_store = NotUseful;
  attributes.save_under = True;
  attributes.event_mask = STANDARD_EVENT_SET;
  attribute_mask = (CWBackPixel | CWBorderPixel | CWBitGravity
#if 0
		    | CWBackingStore | CWSaveUnder
#endif
		    | CWEventMask);

  BLOCK_INPUT;
  FRAME_X_WINDOW (f)
    = XCreateWindow (x_current_display, ROOT_WINDOW,
		     f->display.x->left_pos,
		     f->display.x->top_pos,
		     PIXEL_WIDTH (f), PIXEL_HEIGHT (f),
		     f->display.x->border_width,
		     CopyFromParent, /* depth */
		     InputOutput, /* class */
		     screen_visual, /* set in Fx_open_connection */
		     attribute_mask, &attributes);

  validate_x_resource_name ();
  class_hints.res_name = (char *) XSTRING (Vx_resource_name)->data;
  class_hints.res_class = EMACS_CLASS;
  XSetClassHint (x_current_display, FRAME_X_WINDOW (f), &class_hints);

  /* This indicates that we use the "Passive Input" input model.
     Unless we do this, we don't get the Focus{In,Out} events that we
     need to draw the cursor correctly.  Accursed bureaucrats.
   XWhipsAndChains (x_current_display, IronMaiden, &TheRack);  */

  f->display.x->wm_hints.input = True;
  f->display.x->wm_hints.flags |= InputHint;
  XSetWMHints (x_current_display, FRAME_X_WINDOW (f), &f->display.x->wm_hints);
  XSetWMProtocols (x_current_display, FRAME_X_WINDOW (f),
		   &Xatom_wm_delete_window, 1);

  /* x_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the X server hasn't been told.  */
  {
    Lisp_Object name = f->name;
    int explicit = f->explicit_name;

    f->name = Qnil;
    f->explicit_name = 0;
    x_set_name (f, name, explicit);
  }

  XDefineCursor (XDISPLAY FRAME_X_WINDOW (f),
		 f->display.x->text_cursor);
  UNBLOCK_INPUT;

  if (FRAME_X_WINDOW (f) == 0)
    error ("Unable to create window.");
}

/* Handle the icon stuff for this window.  Perhaps later we might
   want an x_set_icon_position which can be called interactively as
   well. */

static void
x_icon (f, parms)
     struct frame *f;
     Lisp_Object parms;
{
  Lisp_Object icon_x, icon_y;

  /* Set the position of the icon.  Note that twm groups all
     icons in an icon window. */
  icon_x = x_get_arg (parms, Qicon_left, 0, 0, number);
  icon_y = x_get_arg (parms, Qicon_top, 0, 0, number);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_NUMBER (icon_x, 0);
      CHECK_NUMBER (icon_y, 0);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");

  BLOCK_INPUT;

  if (! EQ (icon_x, Qunbound))
    x_wm_set_icon_position (f, XINT (icon_x), XINT (icon_y));

  /* Start up iconic or window? */
  x_wm_set_window_state
    (f, (EQ (x_get_arg (parms, Qvisibility, 0, 0, symbol), Qicon)
	 ? IconicState
	 : NormalState));

  UNBLOCK_INPUT;
}

/* Make the GC's needed for this window, setting the
   background, border and mouse colors; also create the
   mouse cursor and the gray border tile.  */

static char cursor_bits[] =
  {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  };

static void
x_make_gc (f)
     struct frame *f;
{
  XGCValues gc_values;
  GC temp_gc;
  XImage tileimage;

  BLOCK_INPUT;

  /* Create the GC's of this frame.
     Note that many default values are used. */

  /* Normal video */
  gc_values.font = f->display.x->font->fid;
  gc_values.foreground = f->display.x->foreground_pixel;
  gc_values.background = f->display.x->background_pixel;
  gc_values.line_width = 0;	/* Means 1 using fast algorithm. */
  f->display.x->normal_gc = XCreateGC (x_current_display,
				       FRAME_X_WINDOW (f),
				       GCLineWidth | GCFont
				       | GCForeground | GCBackground,
				       &gc_values);

  /* Reverse video style. */
  gc_values.foreground = f->display.x->background_pixel;
  gc_values.background = f->display.x->foreground_pixel;
  f->display.x->reverse_gc = XCreateGC (x_current_display,
					FRAME_X_WINDOW (f),
					GCFont | GCForeground | GCBackground
					| GCLineWidth,
					&gc_values);

  /* Cursor has cursor-color background, background-color foreground. */
  gc_values.foreground = f->display.x->background_pixel;
  gc_values.background = f->display.x->cursor_pixel;
  gc_values.fill_style = FillOpaqueStippled;
  gc_values.stipple
    = XCreateBitmapFromData (x_current_display, ROOT_WINDOW,
			     cursor_bits, 16, 16);
  f->display.x->cursor_gc
    = XCreateGC (x_current_display, FRAME_X_WINDOW (f),
		 (GCFont | GCForeground | GCBackground
		  | GCFillStyle | GCStipple | GCLineWidth),
		 &gc_values);

  /* Create the gray border tile used when the pointer is not in
     the frame.  Since this depends on the frame's pixel values,
     this must be done on a per-frame basis. */
  f->display.x->border_tile
    = (XCreatePixmapFromBitmapData
       (x_current_display, ROOT_WINDOW, 
	gray_bits, gray_width, gray_height,
	f->display.x->foreground_pixel,
	f->display.x->background_pixel,
	DefaultDepth (x_current_display, XDefaultScreen (x_current_display))));

  UNBLOCK_INPUT;
}
#endif /* HAVE_X11 */

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
  "Make a new X window, which is called a \"frame\" in Emacs terms.\n\
Return an Emacs frame object representing the X window.\n\
ALIST is an alist of frame parameters.\n\
If the parameters specify that the frame should not have a minibuffer,\n\
and do not specify a specific minibuffer window to use,\n\
then `default-minibuffer-frame' must be a frame whose minibuffer can\n\
be shared by the new frame.")
  (parms)
     Lisp_Object parms;
{
#ifdef HAVE_X11
  struct frame *f;
  Lisp_Object frame, tem, tem0, tem1;
  Lisp_Object name;
  int minibuffer_only = 0;
  long window_prompting = 0;
  int width, height;

  check_x ();

  name = x_get_arg (parms, Qname, "title", "Title", string);
  if (XTYPE (name) != Lisp_String
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("x-create-frame: name parameter must be a string");

  tem = x_get_arg (parms, Qminibuffer, 0, 0, symbol);
  if (EQ (tem, Qnone) || NILP (tem))
    f = make_frame_without_minibuffer (Qnil);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = 1;
    }
  else if (XTYPE (tem) == Lisp_Window)
    f = make_frame_without_minibuffer (tem);
  else
    f = make_frame (1);

  /* Note that X Windows does support scroll bars.  */
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (x_id_name);
      f->explicit_name = 0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
    }

  XSET (frame, Lisp_Frame, f);
  f->output_method = output_x_window;
  f->display.x = (struct x_display *) xmalloc (sizeof (struct x_display));
  bzero (f->display.x, sizeof (struct x_display));

  /* Note that the frame has no physical cursor right now.  */
  f->phys_cursor_x = -1;

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  {
    Lisp_Object font;

    font = x_get_arg (parms, Qfont, "font", "Font", string);
    BLOCK_INPUT;
    /* First, try whatever font the caller has specified.  */
    if (STRINGP (font))
      font = x_new_font (f, XSTRING (font)->data);
    /* Try out a font which we hope has bold and italic variations.  */
    if (!STRINGP (font))
      font = x_new_font (f, "-misc-fixed-medium-r-normal-*-*-120-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-*-medium-r-normal-*-*-120-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      /* This was formerly the first thing tried, but it finds too many fonts
	 and takes too long.  */
      font = x_new_font (f, "-*-*-medium-r-*-*-*-*-*-*-c-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (! STRINGP (font))
      font = x_new_font (f, "-*-fixed-*-*-*-*-*-120-*-*-c-*-iso8859-1");
    UNBLOCK_INPUT;
    if (! STRINGP (font))
      font = build_string ("fixed");

    x_default_parameter (f, parms, Qfont, font, 
			 "font", "Font", string);
  }
  x_default_parameter (f, parms, Qborder_width, make_number (2),
		       "borderwidth", "BorderWidth", number);
  /* This defaults to 2 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = x_get_arg (parms, Qinternal_border_width,
			 "internalBorder", "BorderWidth", number);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }
  x_default_parameter (f, parms, Qinternal_border_width, make_number (2),
		       "internalBorderWidth", "BorderWidth", number);
  x_default_parameter (f, parms, Qvertical_scroll_bars, Qt,
		       "verticalScrollBars", "ScrollBars", boolean);

  /* Also do the stuff which must be set before the window exists. */
  x_default_parameter (f, parms, Qforeground_color, build_string ("black"),
		       "foreground", "Foreground", string);
  x_default_parameter (f, parms, Qbackground_color, build_string ("white"),
		       "background", "Background", string);
  x_default_parameter (f, parms, Qmouse_color, build_string ("black"),
		       "pointerColor", "Foreground", string);
  x_default_parameter (f, parms, Qcursor_color, build_string ("black"),
		       "cursorColor", "Foreground", string);
  x_default_parameter (f, parms, Qborder_color, build_string ("black"),
		       "borderColor", "BorderColor", string);

  f->display.x->parent_desc = ROOT_WINDOW;
  window_prompting = x_figure_window_size (f, parms);

  x_window (f);
  x_icon (f, parms);
  x_make_gc (f);
  init_frame_faces (f);

  /* We need to do this after creating the X window, so that the
     icon-creation functions can say whose icon they're describing.  */
  x_default_parameter (f, parms, Qicon_type, Qnil,
		       "bitmapIcon", "BitmapIcon", symbol);

  x_default_parameter (f, parms, Qauto_raise, Qnil,
		       "autoRaise", "AutoRaiseLower", boolean);
  x_default_parameter (f, parms, Qauto_lower, Qnil,
		       "autoLower", "AutoRaiseLower", boolean);
  x_default_parameter (f, parms, Qcursor_type, Qbox,
		       "cursorType", "CursorType", symbol);

  /* Dimensions, especially f->height, must be done via change_frame_size.
     Change will not be effected unless different from the current
     f->height. */
  width = f->width;
  height = f->height;
  f->height = f->width = 0;
  change_frame_size (f, height, width, 1, 0);

  x_default_parameter (f, parms, Qmenu_bar_lines, make_number (0),
		       "menuBarLines", "MenuBarLines", number);

  tem0 = x_get_arg (parms, Qtop, 0, 0, number);
  tem1 = x_get_arg (parms, Qleft, 0, 0, number);
  BLOCK_INPUT;
  x_wm_set_size_hint (f, window_prompting, XINT (tem0), XINT (tem1));
  UNBLOCK_INPUT;

  tem = x_get_arg (parms, Qunsplittable, 0, 0, boolean);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  /* Make the window appear on the frame and enable display,
     unless the caller says not to.  */
  {
    Lisp_Object visibility = x_get_arg (parms, Qvisibility, 0, 0, symbol);

    if (EQ (visibility, Qunbound))
      visibility = Qt;

    if (EQ (visibility, Qicon))
      x_iconify_frame (f);
    else if (! NILP (visibility))
      x_make_frame_visible (f);
    else
      /* Must have been Qnil.  */
      ;
  }

  return frame;
#else /* X10 */
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  int pixelwidth, pixelheight;
  Cursor cursor;
  int height, width;
  Window parent;
  Pixmap temp;
  int minibuffer_only = 0;
  Lisp_Object vscroll, hscroll;

  if (x_current_display == 0)
    error ("X windows are not in use or not initialized");

  name = Fassq (Qname, parms);

  tem = x_get_arg (parms, Qminibuffer, 0, 0, symbol);
  if (EQ (tem, Qnone))
    f = make_frame_without_minibuffer (Qnil);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = 1;
    }
  else if (EQ (tem, Qnil) || EQ (tem, Qunbound))
    f = make_frame (1);
  else
    f = make_frame_without_minibuffer (tem);

  parent = ROOT_WINDOW;

  XSET (frame, Lisp_Frame, f);
  f->output_method = output_x_window;
  f->display.x = (struct x_display *) xmalloc (sizeof (struct x_display));
  bzero (f->display.x, sizeof (struct x_display));

  /* Some temporary default values for height and width. */
  width = 80;
  height = 40;
  f->display.x->left_pos = -1;
  f->display.x->top_pos = -1;

  /* Give the frame a default name (which may be overridden with PARMS).  */

  strncpy (iconidentity, ICONTAG, MAXICID);
  if (gethostname (&iconidentity[sizeof (ICONTAG) - 1],
		   (MAXICID - 1) - sizeof (ICONTAG)))
    iconidentity[sizeof (ICONTAG) - 2] = '\0';
  f->name = build_string (iconidentity);

  /* Extract some window parameters from the supplied values.
     These are the parameters that affect window geometry.  */

  tem = x_get_arg (parms, Qfont, "BodyFont", 0, string);
  if (EQ (tem, Qunbound))
    tem = build_string ("9x15");
  x_set_font (f, tem, Qnil);
  x_default_parameter (f, parms, Qborder_color,
		       build_string ("black"), "Border", 0, string);
  x_default_parameter (f, parms, Qbackground_color,
		       build_string ("white"), "Background", 0, string);
  x_default_parameter (f, parms, Qforeground_color,
		       build_string ("black"), "Foreground", 0, string);
  x_default_parameter (f, parms, Qmouse_color,
		       build_string ("black"), "Mouse", 0, string);
  x_default_parameter (f, parms, Qcursor_color,
		       build_string ("black"), "Cursor", 0, string);
  x_default_parameter (f, parms, Qborder_width,
		       make_number (2), "BorderWidth", 0, number);
  x_default_parameter (f, parms, Qinternal_border_width,
		       make_number (4), "InternalBorderWidth", 0, number);
  x_default_parameter (f, parms, Qauto_raise,
		       Qnil, "AutoRaise", 0, boolean);

  hscroll = EQ (x_get_arg (parms, Qhorizontal_scroll_bar, 0, 0, boolean), Qt);
  vscroll = EQ (x_get_arg (parms, Qvertical_scroll_bar, 0, 0, boolean), Qt);

  if (f->display.x->internal_border_width < 0)
    f->display.x->internal_border_width = 0;

  tem = x_get_arg (parms, Qwindow_id, 0, 0, number);
  if (!EQ (tem, Qunbound))
    {
      WINDOWINFO_TYPE wininfo;
      int nchildren;
      Window *children, root;

      CHECK_NUMBER (tem, 0);
      FRAME_X_WINDOW (f) = (Window) XINT (tem);

      BLOCK_INPUT;
      XGetWindowInfo (FRAME_X_WINDOW (f), &wininfo);
      XQueryTree (FRAME_X_WINDOW (f), &parent, &nchildren, &children);
      xfree (children);
      UNBLOCK_INPUT;

      height = PIXEL_TO_CHAR_HEIGHT (f, wininfo.height);
      width  = PIXEL_TO_CHAR_WIDTH  (f, wininfo.width);
      f->display.x->left_pos = wininfo.x;
      f->display.x->top_pos = wininfo.y;
      FRAME_SET_VISIBILITY (f, wininfo.mapped != 0);
      f->display.x->border_width = wininfo.bdrwidth;
      f->display.x->parent_desc = parent;
    }
  else
    {
      tem = x_get_arg (parms, Qparent_id, 0, 0, number);
      if (!EQ (tem, Qunbound))
	{
	  CHECK_NUMBER (tem, 0);
	  parent = (Window) XINT (tem);
	}
      f->display.x->parent_desc = parent;
      tem = x_get_arg (parms, Qheight, 0, 0, number);
      if (EQ (tem, Qunbound))
	{
	  tem = x_get_arg (parms, Qwidth, 0, 0, number);
	  if (EQ (tem, Qunbound))
	    {
	      tem = x_get_arg (parms, Qtop, 0, 0, number);
	      if (EQ (tem, Qunbound))
		tem = x_get_arg (parms, Qleft, 0, 0, number);
	    }
	}
      /* Now TEM is Qunbound if no edge or size was specified.
	 In that case, we must do rubber-banding.  */
      if (EQ (tem, Qunbound))
	{
	  tem = x_get_arg (parms, Qgeometry, 0, 0, number);
	  x_rubber_band (f,
			 &f->display.x->left_pos, &f->display.x->top_pos,
			 &width, &height,
			 (XTYPE (tem) == Lisp_String
			  ? (char *) XSTRING (tem)->data : ""),
			 XSTRING (f->name)->data,
			 !NILP (hscroll), !NILP (vscroll));
	}
      else
	{
	  /* Here if at least one edge or size was specified.
	     Demand that they all were specified, and use them.  */
	  tem = x_get_arg (parms, Qheight, 0, 0, number);
	  if (EQ (tem, Qunbound))
	    error ("Height not specified");
	  CHECK_NUMBER (tem, 0);
	  height = XINT (tem);

	  tem = x_get_arg (parms, Qwidth, 0, 0, number);
	  if (EQ (tem, Qunbound))
	    error ("Width not specified");
	  CHECK_NUMBER (tem, 0);
	  width = XINT (tem);

	  tem = x_get_arg (parms, Qtop, 0, 0, number);
	  if (EQ (tem, Qunbound))
	    error ("Top position not specified");
	  CHECK_NUMBER (tem, 0);
	  f->display.x->left_pos = XINT (tem);

	  tem = x_get_arg (parms, Qleft, 0, 0, number);
	  if (EQ (tem, Qunbound))
	    error ("Left position not specified");
	  CHECK_NUMBER (tem, 0);
	  f->display.x->top_pos = XINT (tem);
	}

      pixelwidth  = CHAR_TO_PIXEL_WIDTH  (f, width);
      pixelheight = CHAR_TO_PIXEL_HEIGHT (f, height);
      
      BLOCK_INPUT;
      FRAME_X_WINDOW (f)
	= XCreateWindow (parent,
			 f->display.x->left_pos,   /* Absolute horizontal offset */
			 f->display.x->top_pos,    /* Absolute Vertical offset */
			 pixelwidth, pixelheight,
			 f->display.x->border_width,
			 BLACK_PIX_DEFAULT, WHITE_PIX_DEFAULT);
      UNBLOCK_INPUT;
      if (FRAME_X_WINDOW (f) == 0)
	error ("Unable to create window.");
    }

  /* Install the now determined height and width
     in the windows and in phys_lines and desired_lines.  */
  change_frame_size (f, height, width, 1, 0);
  XSelectInput (FRAME_X_WINDOW (f), KeyPressed | ExposeWindow
		| ButtonPressed | ButtonReleased | ExposeRegion | ExposeCopy
		| EnterWindow | LeaveWindow | UnmapWindow );
  x_set_resize_hint (f);

  /* Tell the server the window's default name.  */
  XStoreName (XDISPLAY FRAME_X_WINDOW (f), XSTRING (f->name)->data);

  /* Now override the defaults with all the rest of the specified
     parms.  */
  tem = x_get_arg (parms, Qunsplittable, 0, 0, boolean);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  /* Do not create an icon window if the caller says not to */
  if (!EQ (x_get_arg (parms, Qsuppress_icon, 0, 0, boolean), Qt)
      || f->display.x->parent_desc != ROOT_WINDOW)
    {
      x_text_icon (f, iconidentity);
      x_default_parameter (f, parms, Qicon_type, Qnil,
			   "BitmapIcon", 0, symbol);
    }

  /* Tell the X server the previously set values of the
     background, border and mouse colors; also create the mouse cursor.  */
  BLOCK_INPUT;
  temp = XMakeTile (f->display.x->background_pixel);
  XChangeBackground (FRAME_X_WINDOW (f), temp);
  XFreePixmap (temp);
  UNBLOCK_INPUT;
  x_set_border_pixel (f, f->display.x->border_pixel);

  x_set_mouse_color (f, Qnil, Qnil);

  /* Now override the defaults with all the rest of the specified parms.  */

  Fmodify_frame_parameters (frame, parms);

  /* Make the window appear on the frame and enable display.  */
  {
    Lisp_Object visibility = x_get_arg (parms, Qvisibility, 0, 0, symbol);

    if (EQ (visibility, Qunbound))
      visibility = Qt;

    if (! EQ (visibility, Qicon)
	&& ! NILP (visibility))
      x_make_window_visible (f);
  }

  SET_FRAME_GARBAGED (f);

  return frame;
#endif /* X10 */
}

DEFUN ("focus-frame", Ffocus_frame, Sfocus_frame, 1, 1, 0,
  "Set the focus on FRAME.")
  (frame)
     Lisp_Object frame;
{
  CHECK_LIVE_FRAME (frame, 0);

  if (FRAME_X_P (XFRAME (frame)))
    {
      BLOCK_INPUT;
      x_focus_on_frame (XFRAME (frame));
      UNBLOCK_INPUT;
      return frame;
    }

  return Qnil;
}

DEFUN ("unfocus-frame", Funfocus_frame, Sunfocus_frame, 0, 0, 0,
  "If a frame has been focused, release it.")
  ()
{
  if (x_focus_frame)
    {
      BLOCK_INPUT;
      x_unfocus_frame (x_focus_frame);
      UNBLOCK_INPUT;
    }

  return Qnil;
}

#ifndef HAVE_X11
/* Computes an X-window size and position either from geometry GEO
   or with the mouse.

   F is a frame.  It specifies an X window which is used to
   determine which display to compute for.  Its font, borders
   and colors control how the rectangle will be displayed.

   X and Y are where to store the positions chosen.
   WIDTH and HEIGHT are where to store the sizes chosen.

   GEO is the geometry that may specify some of the info.
   STR is a prompt to display.
   HSCROLL and VSCROLL say whether we have horiz and vert scroll bars.  */

int
x_rubber_band (f, x, y, width, height, geo, str, hscroll, vscroll)
     struct frame *f;
     int *x, *y, *width, *height;
     char *geo;
     char *str;
     int hscroll, vscroll;
{
  OpaqueFrame frame;
  Window tempwindow;
  WindowInfo wininfo;
  int border_color;
  int background_color;
  Lisp_Object tem;
  int mask;

  BLOCK_INPUT;

  background_color = f->display.x->background_pixel;
  border_color = f->display.x->border_pixel;

  frame.bdrwidth = f->display.x->border_width;
  frame.border = XMakeTile (border_color);
  frame.background = XMakeTile (background_color);
  tempwindow = XCreateTerm (str, "emacs", geo, default_window, &frame, 10, 5,
			    (2 * f->display.x->internal_border_width
			     + (vscroll ? VSCROLL_WIDTH : 0)),
			    (2 * f->display.x->internal_border_width
			     + (hscroll ? HSCROLL_HEIGHT : 0)),
			    width, height, f->display.x->font,
			    FONT_WIDTH (f->display.x->font),
			    FONT_HEIGHT (f->display.x->font));
  XFreePixmap (frame.border);
  XFreePixmap (frame.background);

  if (tempwindow != 0)
    {
      XQueryWindow (tempwindow, &wininfo);
      XDestroyWindow (tempwindow);
      *x = wininfo.x;
      *y = wininfo.y;
    }

  /* Coordinates we got are relative to the root window.
     Convert them to coordinates relative to desired parent window
     by scanning from there up to the root.  */
  tempwindow = f->display.x->parent_desc;
  while (tempwindow != ROOT_WINDOW)
    {
      int nchildren;
      Window *children;
      XQueryWindow (tempwindow, &wininfo);
      *x -= wininfo.x;
      *y -= wininfo.y;
      XQueryTree (tempwindow, &tempwindow, &nchildren, &children);
      xfree (children);
    }

  UNBLOCK_INPUT;
  return tempwindow != 0;
}
#endif /* not HAVE_X11 */

DEFUN ("x-list-fonts", Fx_list_fonts, Sx_list_fonts, 1, 3, 0,
  "Return a list of the names of available fonts matching PATTERN.\n\
If optional arguments FACE and FRAME are specified, return only fonts\n\
the same size as FACE on FRAME.\n\
\n\
PATTERN is a string, perhaps with wildcard characters;\n\
  the * character matches any substring, and\n\
  the ? character matches any single character.\n\
  PATTERN is case-insensitive.\n\
FACE is a face name - a symbol.\n\
\n\
The return value is a list of strings, suitable as arguments to\n\
set-face-font.\n\
\n\
The list does not include fonts Emacs can't use (i.e.  proportional\n\
fonts), even if they match PATTERN and FACE.")
  (pattern, face, frame)
    Lisp_Object pattern, face, frame;
{
  int num_fonts;
  char **names;
  XFontStruct *info;
  XFontStruct *size_ref;
  Lisp_Object list;

  CHECK_STRING (pattern, 0);
  if (!NILP (face))
    CHECK_SYMBOL (face, 1);
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame, 2);

  if (NILP (face))
    size_ref = 0;
  else
    {
      FRAME_PTR f = NILP (frame) ? selected_frame : XFRAME (frame);
      int face_id = face_name_id_number (f, face);

      if (face_id < 0 || face_id >= FRAME_N_PARAM_FACES (f)
	  || FRAME_PARAM_FACES (f) [face_id] == 0)
	size_ref = f->display.x->font;
      else
	{
	  size_ref = FRAME_PARAM_FACES (f) [face_id]->font;
	  if (size_ref == (XFontStruct *) (~0))
	    size_ref = f->display.x->font;
	}
    }

  BLOCK_INPUT;
  names = XListFontsWithInfo (x_current_display,
			      XSTRING (pattern)->data,
			      2000, /* maxnames */
			      &num_fonts, /* count_return */
			      &info); /* info_return */
  UNBLOCK_INPUT;

  list = Qnil;

  if (names)
    {
      Lisp_Object *tail;
      int i;

      tail = &list;
      for (i = 0; i < num_fonts; i++)
	if (! size_ref 
	    || same_size_fonts (&info[i], size_ref))
	  {
	    *tail = Fcons (build_string (names[i]), Qnil);
	    tail = &XCONS (*tail)->cdr;
	  }

      XFreeFontInfo (names, info, num_fonts);
    }

  return list;
}


DEFUN ("x-color-defined-p", Fx_color_defined_p, Sx_color_defined_p, 1, 1, 0,
  "Return t if the current X display supports the color named COLOR.")
  (color)
     Lisp_Object color;
{
  Color foo;
  
  check_x ();
  CHECK_STRING (color, 0);

  if (defined_color (XSTRING (color)->data, &foo))
    return Qt;
  else
    return Qnil;
}

DEFUN ("x-display-color-p", Fx_display_color_p, Sx_display_color_p, 0, 0, 0,
  "Return t if the X screen currently in use supports color.")
  ()
{
  check_x ();

  if (x_screen_planes <= 2)
    return Qnil;

  switch (screen_visual->class)
    {
    case StaticColor:
    case PseudoColor:
    case TrueColor:
    case DirectColor:
      return Qt;

    default:
      return Qnil;
    }
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
  0, 1, 0,
  "Returns the width in pixels of the display FRAME is on.")
  (frame)
     Lisp_Object frame;
{
  Display *dpy = x_current_display;
  check_x ();
  return make_number (DisplayWidth (dpy, DefaultScreen (dpy)));
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
  Sx_display_pixel_height, 0, 1, 0,
  "Returns the height in pixels of the display FRAME is on.")
  (frame)
     Lisp_Object frame;
{
  Display *dpy = x_current_display;
  check_x ();
  return make_number (DisplayHeight (dpy, DefaultScreen (dpy)));
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
  0, 1, 0,
  "Returns the number of bitplanes of the display FRAME is on.")
  (frame)
     Lisp_Object frame;
{
  Display *dpy = x_current_display;
  check_x ();
  return make_number (DisplayPlanes (dpy, DefaultScreen (dpy)));
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
  0, 1, 0,
  "Returns the number of color cells of the display FRAME is on.")
  (frame)
     Lisp_Object frame;
{
  Display *dpy = x_current_display;
  check_x ();
  return make_number (DisplayCells (dpy, DefaultScreen (dpy)));
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
  0, 1, 0,
  "Returns the maximum request size of the X server FRAME is using.")
  (frame)
     Lisp_Object frame;
{
  Display *dpy = x_current_display;
  check_x ();
  return make_number (MAXREQUEST (dpy));
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
  "Returns the vendor ID string of the X server FRAME is on.")
  (frame)
     Lisp_Object frame;
{
  Display *dpy = x_current_display;
  char *vendor;
  check_x ();
  vendor = ServerVendor (dpy);
  if (! vendor) vendor = "";
  return build_string (vendor);
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
  "Returns the version numbers of the X server in use.\n\
The value is a list of three integers: the major and minor\n\
version numbers of the X Protocol in use, and the vendor-specific release\n\
number.  See also the variable `x-server-vendor'.")
  (frame)
     Lisp_Object frame;
{
  Display *dpy = x_current_display;

  check_x ();
  return Fcons (make_number (ProtocolVersion (dpy)),
		Fcons (make_number (ProtocolRevision (dpy)),
		       Fcons (make_number (VendorRelease (dpy)), Qnil)));
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
  "Returns the number of screens on the X server FRAME is on.")
  (frame)
     Lisp_Object frame;
{
  check_x ();
  return make_number (ScreenCount (x_current_display));
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
  "Returns the height in millimeters of the X screen FRAME is on.")
  (frame)
     Lisp_Object frame;
{
  check_x ();
  return make_number (HeightMMOfScreen (x_screen));
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
  "Returns the width in millimeters of the X screen FRAME is on.")
  (frame)
     Lisp_Object frame;
{
  check_x ();
  return make_number (WidthMMOfScreen (x_screen));
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
  Sx_display_backing_store, 0, 1, 0,
  "Returns an indication of whether the X screen FRAME is on does backing store.\n\
The value may be `always', `when-mapped', or `not-useful'.")
  (frame)
     Lisp_Object frame;
{
  check_x ();

  switch (DoesBackingStore (x_screen))
    {
    case Always:
      return intern ("always");

    case WhenMapped:
      return intern ("when-mapped");

    case NotUseful:
      return intern ("not-useful");

    default:
      error ("Strange value for BackingStore parameter of screen");
    }
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
  Sx_display_visual_class, 0, 1, 0,
  "Returns the visual class of the display `screen' is on.\n\
The value is one of the symbols `static-gray', `gray-scale',\n\
`static-color', `pseudo-color', `true-color', or `direct-color'.")
	(screen)
     Lisp_Object screen;
{
  check_x ();

  switch (screen_visual->class)
    {
    case StaticGray:  return (intern ("static-gray"));
    case GrayScale:   return (intern ("gray-scale"));
    case StaticColor: return (intern ("static-color"));
    case PseudoColor: return (intern ("pseudo-color"));
    case TrueColor:   return (intern ("true-color"));
    case DirectColor: return (intern ("direct-color"));
    default:
      error ("Display has an unknown visual class");
    }
}

DEFUN ("x-display-save-under", Fx_display_save_under,
  Sx_display_save_under, 0, 1, 0,
  "Returns t if the X screen FRAME is on supports the save-under feature.")
  (frame)
     Lisp_Object frame;
{
  check_x ();

  if (DoesSaveUnders (x_screen) == True)
    return Qt;
  else
    return Qnil;
}

x_pixel_width (f)
     register struct frame *f;
{
  return PIXEL_WIDTH (f);
}

x_pixel_height (f)
     register struct frame *f;
{
  return PIXEL_HEIGHT (f);
}

x_char_width (f)
     register struct frame *f;
{
  return FONT_WIDTH (f->display.x->font);
}

x_char_height (f)
     register struct frame *f;
{
  return FONT_HEIGHT (f->display.x->font);
}

#if 0  /* These no longer seem like the right way to do things.  */

/* Draw a rectangle on the frame with left top corner including
   the character specified by LEFT_CHAR and TOP_CHAR.  The rectangle is
   CHARS by LINES wide and long and is the color of the cursor. */

void
x_rectangle (f, gc, left_char, top_char, chars, lines)
     register struct frame *f;
     GC gc;
     register int top_char, left_char, chars, lines;
{
  int width;
  int height;
  int left = (left_char * FONT_WIDTH (f->display.x->font)
		    + f->display.x->internal_border_width);
  int top = (top_char *  FONT_HEIGHT (f->display.x->font)
		   + f->display.x->internal_border_width);

  if (chars < 0)
    width = FONT_WIDTH (f->display.x->font) / 2;
  else
    width = FONT_WIDTH (f->display.x->font) * chars;
  if (lines < 0)
    height = FONT_HEIGHT (f->display.x->font) / 2;
  else
    height = FONT_HEIGHT (f->display.x->font) * lines;

  XDrawRectangle (x_current_display, FRAME_X_WINDOW (f),
		  gc, left, top, width, height);
}

DEFUN ("x-draw-rectangle", Fx_draw_rectangle, Sx_draw_rectangle, 5, 5, 0,
  "Draw a rectangle on FRAME between coordinates specified by\n\
numbers X0, Y0, X1, Y1 in the cursor pixel.")
  (frame, X0, Y0, X1, Y1)
     register Lisp_Object frame, X0, X1, Y0, Y1;
{
  register int x0, y0, x1, y1, top, left, n_chars, n_lines;

  CHECK_LIVE_FRAME (frame, 0);
  CHECK_NUMBER (X0, 0);
  CHECK_NUMBER (Y0, 1);
  CHECK_NUMBER (X1, 2);
  CHECK_NUMBER (Y1, 3);

  x0 = XINT (X0);
  x1 = XINT (X1);
  y0 = XINT (Y0);
  y1 = XINT (Y1);

  if (y1 > y0)
    {
      top = y0;
      n_lines = y1 - y0 + 1;
    }
  else
    {
      top = y1;
      n_lines = y0 - y1 + 1;
    }

  if (x1 > x0)
    {
      left = x0;
      n_chars = x1 - x0 + 1;
    }
  else
    {
      left = x1;
      n_chars = x0 - x1 + 1;
    }

  BLOCK_INPUT;
  x_rectangle (XFRAME (frame), XFRAME (frame)->display.x->cursor_gc,
	       left, top, n_chars, n_lines);
  UNBLOCK_INPUT;

  return Qt;
}

DEFUN ("x-erase-rectangle", Fx_erase_rectangle, Sx_erase_rectangle, 5, 5, 0,
  "Draw a rectangle drawn on FRAME between coordinates\n\
X0, Y0, X1, Y1 in the regular background-pixel.")
  (frame, X0, Y0, X1, Y1)
  register Lisp_Object frame, X0, Y0, X1, Y1;
{
  register int x0, y0, x1, y1, top, left, n_chars, n_lines;

  CHECK_FRAME (frame, 0);
  CHECK_NUMBER (X0, 0);
  CHECK_NUMBER (Y0, 1);
  CHECK_NUMBER (X1, 2);
  CHECK_NUMBER (Y1, 3);

  x0 = XINT (X0);
  x1 = XINT (X1);
  y0 = XINT (Y0);
  y1 = XINT (Y1);

  if (y1 > y0)
    {
      top = y0;
      n_lines = y1 - y0 + 1;
    }
  else
    {
      top = y1;
      n_lines = y0 - y1 + 1;
    }

  if (x1 > x0)
    {
      left = x0;
      n_chars = x1 - x0 + 1;
    }
  else
    {
      left = x1;
      n_chars = x0 - x1 + 1;
    }

  BLOCK_INPUT;
  x_rectangle (XFRAME (frame), XFRAME (frame)->display.x->reverse_gc,
	       left, top, n_chars, n_lines);
  UNBLOCK_INPUT;

  return Qt;
}

/* Draw lines around the text region beginning at the character position
   TOP_X, TOP_Y and ending at BOTTOM_X and BOTTOM_Y.  GC specifies the
   pixel and line characteristics. */

#define line_len(line) (FRAME_CURRENT_GLYPHS (f)->used[(line)])

static void
outline_region (f, gc, top_x, top_y, bottom_x, bottom_y)
     register struct frame *f;
     GC gc;
     int  top_x, top_y, bottom_x, bottom_y;
{
  register int ibw = f->display.x->internal_border_width;
  register int font_w = FONT_WIDTH (f->display.x->font);
  register int font_h = FONT_HEIGHT (f->display.x->font);
  int y = top_y;
  int x = line_len (y);
  XPoint *pixel_points = (XPoint *)
    alloca (((bottom_y - top_y + 2) * 4) * sizeof (XPoint));
  register XPoint *this_point = pixel_points;

  /* Do the horizontal top line/lines */
  if (top_x == 0)
    {
      this_point->x = ibw;
      this_point->y = ibw + (font_h * top_y);
      this_point++;
      if (x == 0)
	this_point->x = ibw + (font_w / 2); /* Half-size for newline chars. */
      else
	this_point->x = ibw + (font_w * x);
      this_point->y = (this_point - 1)->y;
    }
  else
    {
      this_point->x = ibw;
      this_point->y = ibw + (font_h * (top_y + 1));
      this_point++;
      this_point->x = ibw + (font_w * top_x);
      this_point->y = (this_point - 1)->y;
      this_point++;
      this_point->x = (this_point - 1)->x;
      this_point->y = ibw + (font_h * top_y);
      this_point++;
      this_point->x = ibw + (font_w * x);
      this_point->y = (this_point - 1)->y;
    }

  /* Now do the right side. */
  while (y < bottom_y)
    {				/* Right vertical edge */
      this_point++;
      this_point->x = (this_point - 1)->x;
      this_point->y = ibw + (font_h * (y + 1));
      this_point++;

      y++;			/* Horizontal connection to next line */
      x = line_len (y);
      if (x == 0)
	this_point->x = ibw + (font_w / 2);
      else
	this_point->x = ibw + (font_w * x);

      this_point->y = (this_point - 1)->y;
    }

  /* Now do the bottom and connect to the top left point. */
  this_point->x = ibw + (font_w * (bottom_x + 1));

  this_point++;
  this_point->x = (this_point - 1)->x;
  this_point->y = ibw + (font_h * (bottom_y + 1));
  this_point++;
  this_point->x = ibw;
  this_point->y = (this_point - 1)->y;
  this_point++;
  this_point->x = pixel_points->x;
  this_point->y = pixel_points->y;

  XDrawLines (x_current_display, FRAME_X_WINDOW (f),
	      gc, pixel_points,
	      (this_point - pixel_points + 1), CoordModeOrigin);
}

DEFUN ("x-contour-region", Fx_contour_region, Sx_contour_region, 1, 1, 0,
  "Highlight the region between point and the character under the mouse\n\
selected frame.")
  (event)
     register Lisp_Object event;
{
  register int x0, y0, x1, y1;
  register struct frame *f = selected_frame;
  register int p1, p2;

  CHECK_CONS (event, 0);

  BLOCK_INPUT;
  x0 = XINT (Fcar (Fcar (event)));
  y0 = XINT (Fcar (Fcdr (Fcar (event))));

  /* If the mouse is past the end of the line, don't that area. */
  /* ReWrite this... */

  x1 = f->cursor_x;
  y1 = f->cursor_y;

  if (y1 > y0)			/* point below mouse */
    outline_region (f, f->display.x->cursor_gc,
		    x0, y0, x1, y1);
  else if (y1 < y0)		/* point above mouse */
    outline_region (f, f->display.x->cursor_gc,
		    x1, y1, x0, y0);
  else				/* same line: draw horizontal rectangle */
    {
      if (x1 > x0)
	x_rectangle (f, f->display.x->cursor_gc,
		     x0, y0, (x1 - x0 + 1), 1);
      else if (x1 < x0)
	  x_rectangle (f, f->display.x->cursor_gc,
		       x1, y1, (x0 - x1 + 1), 1);
    }

  XFlush (x_current_display);
  UNBLOCK_INPUT;

  return Qnil;
}

DEFUN ("x-uncontour-region", Fx_uncontour_region, Sx_uncontour_region, 1, 1, 0,
  "Erase any highlighting of the region between point and the character\n\
at X, Y on the selected frame.")
  (event)
     register Lisp_Object event;
{
  register int x0, y0, x1, y1;
  register struct frame *f = selected_frame;

  BLOCK_INPUT;
  x0 = XINT (Fcar (Fcar (event)));
  y0 = XINT (Fcar (Fcdr (Fcar (event))));
  x1 = f->cursor_x;
  y1 = f->cursor_y;

  if (y1 > y0)			/* point below mouse */
    outline_region (f, f->display.x->reverse_gc,
		      x0, y0, x1, y1);
  else if (y1 < y0)		/* point above mouse */
    outline_region (f, f->display.x->reverse_gc,
		      x1, y1, x0, y0);
  else				/* same line: draw horizontal rectangle */
    {
      if (x1 > x0)
	x_rectangle (f, f->display.x->reverse_gc,
		     x0, y0, (x1 - x0 + 1), 1);
      else if (x1 < x0)
	x_rectangle (f, f->display.x->reverse_gc,
		     x1, y1, (x0 - x1 + 1), 1);
    }
  UNBLOCK_INPUT;

  return Qnil;
}

#if 0
int contour_begin_x, contour_begin_y;
int contour_end_x, contour_end_y;
int contour_npoints;

/* Clip the top part of the contour lines down (and including) line Y_POS.
   If X_POS is in the middle (rather than at the end) of the line, drop
   down a line at that character. */

static void
clip_contour_top (y_pos, x_pos)
{
  register XPoint *begin = contour_lines[y_pos].top_left;
  register XPoint *end;
  register int npoints;
  register struct display_line *line = selected_frame->phys_lines[y_pos + 1];

  if (x_pos >= line->len - 1)	/* Draw one, straight horizontal line. */
    {
      end = contour_lines[y_pos].top_right;
      npoints = (end - begin + 1);
      XDrawLines (x_current_display, contour_window,
		  contour_erase_gc, begin_erase, npoints, CoordModeOrigin);

      bcopy (end, begin + 1, contour_last_point - end + 1);
      contour_last_point -= (npoints - 2);
      XDrawLines (x_current_display, contour_window,
		  contour_erase_gc, begin, 2, CoordModeOrigin);
      XFlush (x_current_display);

      /* Now, update contour_lines structure. */
    }
				/* ______.         */
  else				/*       |________*/
    {
      register XPoint *p = begin + 1;
      end = contour_lines[y_pos].bottom_right;
      npoints = (end - begin + 1);
      XDrawLines (x_current_display, contour_window,
		  contour_erase_gc, begin_erase, npoints, CoordModeOrigin);

      p->y = begin->y;
      p->x = ibw + (font_w * (x_pos + 1));
      p++;
      p->y = begin->y + font_h;
      p->x = (p - 1)->x;
      bcopy (end, begin + 3, contour_last_point - end + 1);
      contour_last_point -= (npoints - 5);
      XDrawLines (x_current_display, contour_window,
		  contour_erase_gc, begin, 4, CoordModeOrigin);
      XFlush (x_current_display);

      /* Now, update contour_lines structure. */
    }
}

/* Erase the top horizontal lines of the contour, and then extend
   the contour upwards. */

static void
extend_contour_top (line)
{
}

static void
clip_contour_bottom (x_pos, y_pos)
     int x_pos, y_pos;
{
}

static void
extend_contour_bottom (x_pos, y_pos)
{
}

DEFUN ("x-select-region", Fx_select_region, Sx_select_region, 1, 1, "e",
  "")
  (event)
     Lisp_Object event;
{
 register struct frame *f = selected_frame;
 register int point_x = f->cursor_x;
 register int point_y = f->cursor_y;
 register int mouse_below_point;
 register Lisp_Object obj;
 register int x_contour_x, x_contour_y;

 x_contour_x = x_mouse_x;
 x_contour_y = x_mouse_y;
 if (x_contour_y > point_y || (x_contour_y == point_y
			       && x_contour_x > point_x))
   {
     mouse_below_point = 1;
     outline_region (f, f->display.x->cursor_gc, point_x, point_y,
		     x_contour_x, x_contour_y);
   }
 else
   {
     mouse_below_point = 0;
     outline_region (f, f->display.x->cursor_gc, x_contour_x, x_contour_y,
		     point_x, point_y);
   }

 while (1)
   {
     obj = read_char (-1, 0, 0, Qnil, 0);
     if (XTYPE (obj) != Lisp_Cons)
       break;

     if (mouse_below_point)
       {
	 if (x_mouse_y <= point_y)                /* Flipped. */
	   {
	     mouse_below_point = 0;

	     outline_region (f, f->display.x->reverse_gc, point_x, point_y,
			     x_contour_x, x_contour_y);
	     outline_region (f, f->display.x->cursor_gc, x_mouse_x, x_mouse_y,
			     point_x, point_y);
	   }
	 else if (x_mouse_y < x_contour_y)	  /* Bottom clipped. */
	   {
	     clip_contour_bottom (x_mouse_y);
	   }
	 else if (x_mouse_y > x_contour_y)	  /* Bottom extended. */
	   {
	     extend_bottom_contour (x_mouse_y);
	   }

	 x_contour_x = x_mouse_x;
	 x_contour_y = x_mouse_y;
       }
     else  /* mouse above or same line as point */
       {
	 if (x_mouse_y >= point_y)		  /* Flipped. */
	   {
	     mouse_below_point = 1;

	     outline_region (f, f->display.x->reverse_gc,
			     x_contour_x, x_contour_y, point_x, point_y);
	     outline_region (f, f->display.x->cursor_gc, point_x, point_y,
			     x_mouse_x, x_mouse_y);
	   }
	 else if (x_mouse_y > x_contour_y)	  /* Top clipped. */
	   {
	     clip_contour_top (x_mouse_y);
	   }
	 else if (x_mouse_y < x_contour_y)	  /* Top extended. */
	   {
	     extend_contour_top (x_mouse_y);
	   }
       }
   }

 unread_command_event = obj;
 if (mouse_below_point)
   {
     contour_begin_x = point_x;
     contour_begin_y = point_y;
     contour_end_x = x_contour_x;
     contour_end_y = x_contour_y;
   }
 else
   {
     contour_begin_x = x_contour_x;
     contour_begin_y = x_contour_y;
     contour_end_x = point_x;
     contour_end_y = point_y;
   }
}
#endif

DEFUN ("x-horizontal-line", Fx_horizontal_line, Sx_horizontal_line, 1, 1, "e",
  "")
  (event)
     Lisp_Object event;
{
  register Lisp_Object obj;
  struct frame *f = selected_frame;
  register struct window *w = XWINDOW (selected_window);
  register GC line_gc = f->display.x->cursor_gc;
  register GC erase_gc = f->display.x->reverse_gc;
#if 0
  char dash_list[] = {6, 4, 6, 4};
  int dashes = 4;
  XGCValues gc_values;
#endif
  register int previous_y;
  register int line = (x_mouse_y + 1) * FONT_HEIGHT (f->display.x->font)
    + f->display.x->internal_border_width;
  register int left = f->display.x->internal_border_width
    + (w->left
       * FONT_WIDTH (f->display.x->font));
  register int right = left + (w->width
			       * FONT_WIDTH (f->display.x->font))
    - f->display.x->internal_border_width;

#if 0
  BLOCK_INPUT;
  gc_values.foreground = f->display.x->cursor_pixel;
  gc_values.background = f->display.x->background_pixel;
  gc_values.line_width = 1;
  gc_values.line_style = LineOnOffDash;
  gc_values.cap_style = CapRound;
  gc_values.join_style = JoinRound;

  line_gc = XCreateGC (x_current_display, FRAME_X_WINDOW (f),
		       GCLineStyle | GCJoinStyle | GCCapStyle
		       | GCLineWidth | GCForeground | GCBackground,
		       &gc_values);
  XSetDashes (x_current_display, line_gc, 0, dash_list, dashes);
  gc_values.foreground = f->display.x->background_pixel;
  gc_values.background = f->display.x->foreground_pixel;
  erase_gc = XCreateGC (x_current_display, FRAME_X_WINDOW (f),
		       GCLineStyle | GCJoinStyle | GCCapStyle
		       | GCLineWidth | GCForeground | GCBackground,
		       &gc_values);
  XSetDashes (x_current_display, erase_gc, 0, dash_list, dashes);
#endif

  while (1)
    {
      BLOCK_INPUT;
      if (x_mouse_y >= XINT (w->top)
	  && x_mouse_y < XINT (w->top) + XINT (w->height) - 1)
	{
	  previous_y = x_mouse_y;
	  line = (x_mouse_y + 1) * FONT_HEIGHT (f->display.x->font)
	    + f->display.x->internal_border_width;
	  XDrawLine (x_current_display, FRAME_X_WINDOW (f),
		     line_gc, left, line, right, line);
	}
      XFlushQueue ();
      UNBLOCK_INPUT;

      do
	{
	  obj = read_char (-1, 0, 0, Qnil, 0);
	  if ((XTYPE (obj) != Lisp_Cons)
	      || (! EQ (Fcar (Fcdr (Fcdr (obj))),
		       Qvertical_scroll_bar))
	      || x_mouse_grabbed)
	    {
	      BLOCK_INPUT;
	      XDrawLine (x_current_display, FRAME_X_WINDOW (f),
			 erase_gc, left, line, right, line);
	      UNBLOCK_INPUT;
	      unread_command_event = obj;
#if 0
	      XFreeGC (x_current_display, line_gc);
	      XFreeGC (x_current_display, erase_gc);
#endif 
	      return Qnil;
	    }
	}
      while (x_mouse_y == previous_y);

      BLOCK_INPUT;
      XDrawLine (x_current_display, FRAME_X_WINDOW (f),
		 erase_gc, left, line, right, line);
      UNBLOCK_INPUT;
    }
}
#endif

/* Offset in buffer of character under the pointer, or 0. */
int mouse_buffer_offset;

#if 0
/* These keep track of the rectangle following the pointer. */
int mouse_track_top, mouse_track_left, mouse_track_width;

DEFUN ("x-track-pointer", Fx_track_pointer, Sx_track_pointer, 0, 0, 0,
  "Track the pointer.")
  ()
{
  static Cursor current_pointer_shape;
  FRAME_PTR f = x_mouse_frame;

  BLOCK_INPUT;
  if (EQ (Vmouse_frame_part, Qtext_part)
      && (current_pointer_shape != f->display.x->nontext_cursor))
    {
      unsigned char c;
      struct buffer *buf;

      current_pointer_shape = f->display.x->nontext_cursor;
      XDefineCursor (x_current_display,
		     FRAME_X_WINDOW (f),
		     current_pointer_shape);

      buf = XBUFFER (XWINDOW (Vmouse_window)->buffer);
      c = *(BUF_CHAR_ADDRESS (buf, mouse_buffer_offset));
    }
  else if (EQ (Vmouse_frame_part, Qmodeline_part)
	   && (current_pointer_shape != f->display.x->modeline_cursor))
    {
      current_pointer_shape = f->display.x->modeline_cursor;
      XDefineCursor (x_current_display,
		     FRAME_X_WINDOW (f),
		     current_pointer_shape);
    }

  XFlushQueue ();
  UNBLOCK_INPUT;
}
#endif

#if 0
DEFUN ("x-track-pointer", Fx_track_pointer, Sx_track_pointer, 1, 1, "e",
  "Draw rectangle around character under mouse pointer, if there is one.")
  (event)
     Lisp_Object event;
{
  struct window *w = XWINDOW (Vmouse_window);
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct buffer *b = XBUFFER (w->buffer);
  Lisp_Object obj;

  if (! EQ (Vmouse_window, selected_window))
      return Qnil;

  if (EQ (event, Qnil))
    {
      int x, y;

      x_read_mouse_position (selected_frame, &x, &y);
    }

  BLOCK_INPUT;
  mouse_track_width = 0;
  mouse_track_left = mouse_track_top = -1;

  do
    {
      if ((x_mouse_x != mouse_track_left
	   && (x_mouse_x < mouse_track_left
	       || x_mouse_x > (mouse_track_left + mouse_track_width)))
	  || x_mouse_y != mouse_track_top)
	{
	  int hp = 0;		/* Horizontal position */
	  int len = FRAME_CURRENT_GLYPHS (f)->used[x_mouse_y];
	  int p = FRAME_CURRENT_GLYPHS (f)->bufp[x_mouse_y];
	  int tab_width = XINT (b->tab_width);
	  int ctl_arrow_p = !NILP (b->ctl_arrow);
	  unsigned char c;
	  int mode_line_vpos = XFASTINT (w->height) + XFASTINT (w->top) - 1;
	  int in_mode_line = 0;

	  if (! FRAME_CURRENT_GLYPHS (f)->enable[x_mouse_y])
	    break;

	  /* Erase previous rectangle. */
	  if (mouse_track_width)
	    {
	      x_rectangle (f, f->display.x->reverse_gc,
			   mouse_track_left, mouse_track_top,
			   mouse_track_width, 1);

	      if ((mouse_track_left == f->phys_cursor_x
		   || mouse_track_left == f->phys_cursor_x - 1)
		  && mouse_track_top == f->phys_cursor_y)
		{
		  x_display_cursor (f, 1);
		}
	    }

	  mouse_track_left = x_mouse_x;
	  mouse_track_top = x_mouse_y;
	  mouse_track_width = 0;

	  if (mouse_track_left > len) /* Past the end of line. */
	    goto draw_or_not;

	  if (mouse_track_top == mode_line_vpos)
	    {
	      in_mode_line = 1;
	      goto draw_or_not;
	    }

	  if (tab_width <= 0 || tab_width > 20) tab_width = 8;
	  do
	    {
	      c = FETCH_CHAR (p);
	      if (len == f->width && hp == len - 1 && c != '\n')
		goto draw_or_not;

	      switch (c)
		{
		case '\t':
		  mouse_track_width = tab_width - (hp % tab_width);
		  p++;
		  hp += mouse_track_width;
		  if (hp > x_mouse_x)
		    {
		      mouse_track_left = hp - mouse_track_width;
		      goto draw_or_not;
		    }
		  continue;

		case '\n':
		  mouse_track_width = -1;
		  goto draw_or_not;

		default:
		  if (ctl_arrow_p && (c < 040 || c == 0177))
		    {
		      if (p > ZV)
			goto draw_or_not;

		      mouse_track_width = 2;
		      p++;
		      hp +=2;
		      if (hp > x_mouse_x)
			{
			  mouse_track_left = hp - mouse_track_width;
			  goto draw_or_not;
			}
		    }
		  else
		    {
		      mouse_track_width = 1;
		      p++;
		      hp++;
		    }
		  continue;
		}
	    }
	  while (hp <= x_mouse_x);

	draw_or_not:
	  if (mouse_track_width) /* Over text; use text pointer shape. */
	    {
	      XDefineCursor (x_current_display,
			     FRAME_X_WINDOW (f),
			     f->display.x->text_cursor);
	      x_rectangle (f, f->display.x->cursor_gc,
			   mouse_track_left, mouse_track_top,
			   mouse_track_width, 1);
	    }
	  else if (in_mode_line)
	    XDefineCursor (x_current_display,
			   FRAME_X_WINDOW (f),
			   f->display.x->modeline_cursor);
	  else
	    XDefineCursor (x_current_display,
			   FRAME_X_WINDOW (f),
			   f->display.x->nontext_cursor);
	}

      XFlush (x_current_display);
      UNBLOCK_INPUT;

      obj = read_char (-1, 0, 0, Qnil, 0);
      BLOCK_INPUT;
    }
  while (XTYPE (obj) == Lisp_Cons		   /* Mouse event */
	 && EQ (Fcar (Fcdr (Fcdr (obj))), Qnil)	   /* Not scroll bar */
	 && EQ (Vmouse_depressed, Qnil)              /* Only motion events */
	 && EQ (Vmouse_window, selected_window)	   /* In this window */
	 && x_mouse_frame);

  unread_command_event = obj;

  if (mouse_track_width)
    {
      x_rectangle (f, f->display.x->reverse_gc,
		   mouse_track_left, mouse_track_top,
		   mouse_track_width, 1);
      mouse_track_width = 0;
      if ((mouse_track_left == f->phys_cursor_x
	   || mouse_track_left - 1 == f->phys_cursor_x)
	  && mouse_track_top == f->phys_cursor_y)
	{
	  x_display_cursor (f, 1);
	}
    }
  XDefineCursor (x_current_display,
		 FRAME_X_WINDOW (f),
		 f->display.x->nontext_cursor);
  XFlush (x_current_display);
  UNBLOCK_INPUT;

  return Qnil;
}
#endif

#if 0
#include "glyphs.h"

/* Draw a pixmap specified by IMAGE_DATA of dimensions WIDTH and HEIGHT
   on the frame F at position X, Y. */

x_draw_pixmap (f, x, y, image_data, width, height)
     struct frame *f;
     int x, y, width, height;
     char *image_data;
{
  Pixmap image;

  image = XCreateBitmapFromData (x_current_display,
				 FRAME_X_WINDOW (f), image_data,
				 width, height);
  XCopyPlane (x_current_display, image, FRAME_X_WINDOW (f),
	      f->display.x->normal_gc, 0, 0, width, height, x, y);
}
#endif

#ifndef HAVE_X11
DEFUN ("x-store-cut-buffer", Fx_store_cut_buffer, Sx_store_cut_buffer,
  1, 1, "sStore text in cut buffer: ",
  "Store contents of STRING into the cut buffer of the X window system.")
  (string)
     register Lisp_Object string;
{
  int mask;

  CHECK_STRING (string, 1);
  if (! FRAME_X_P (selected_frame))
    error ("Selected frame does not understand X protocol.");

  BLOCK_INPUT;
  XStoreBytes ((char *) XSTRING (string)->data, XSTRING (string)->size);
  UNBLOCK_INPUT;

  return Qnil;
}

DEFUN ("x-get-cut-buffer", Fx_get_cut_buffer, Sx_get_cut_buffer, 0, 0, 0,
  "Return contents of cut buffer of the X window system, as a string.")
  ()
{
  int len;
  register Lisp_Object string;
  int mask;
  register char *d;

  BLOCK_INPUT;
  d = XFetchBytes (&len);
  string = make_string (d, len);
  XFree (d);
  UNBLOCK_INPUT;
  return string;
}
#endif	/* X10 */

#if 0 /* I'm told these functions are superfluous
	 given the ability to bind function keys.  */

#ifdef HAVE_X11
DEFUN ("x-rebind-key", Fx_rebind_key, Sx_rebind_key, 3, 3, 0,
"Rebind X keysym KEYSYM, with MODIFIERS, to generate NEWSTRING.\n\
KEYSYM is a string which conforms to the X keysym definitions found\n\
in X11/keysymdef.h, sans the initial XK_. MODIFIERS is nil or a\n\
list of strings specifying modifier keys such as Control_L, which must\n\
also be depressed for NEWSTRING to appear.")
  (x_keysym, modifiers, newstring)
     register Lisp_Object x_keysym;
     register Lisp_Object modifiers;
     register Lisp_Object newstring;
{
  char *rawstring;
  register KeySym keysym;
  KeySym modifier_list[16];

  check_x ();
  CHECK_STRING (x_keysym, 1);
  CHECK_STRING (newstring, 3);

  keysym = XStringToKeysym ((char *) XSTRING (x_keysym)->data);
  if (keysym == NoSymbol)
    error ("Keysym does not exist");

  if (NILP (modifiers))
    XRebindKeysym (x_current_display, keysym, modifier_list, 0,
		   XSTRING (newstring)->data, XSTRING (newstring)->size);
  else
    {
      register Lisp_Object rest, mod;
      register int i = 0;

      for (rest = modifiers; !NILP (rest); rest = Fcdr (rest))
	{
	  if (i == 16)
	    error ("Can't have more than 16 modifiers");

	  mod = Fcar (rest);
	  CHECK_STRING (mod, 3);
	  modifier_list[i] = XStringToKeysym ((char *) XSTRING (mod)->data);
#ifndef HAVE_X11R5
	  if (modifier_list[i] == NoSymbol
	      || !(IsModifierKey (modifier_list[i]) 
                   || ((unsigned)(modifier_list[i]) == XK_Mode_switch)
                   || ((unsigned)(modifier_list[i]) == XK_Num_Lock)))
#else
	  if (modifier_list[i] == NoSymbol
	      || !IsModifierKey (modifier_list[i]))
#endif
	    error ("Element is not a modifier keysym");
	  i++;
	}

      XRebindKeysym (x_current_display, keysym, modifier_list, i,
		     XSTRING (newstring)->data, XSTRING (newstring)->size);
    }

  return Qnil;
}
  
DEFUN ("x-rebind-keys", Fx_rebind_keys, Sx_rebind_keys, 2, 2, 0,
  "Rebind KEYCODE to list of strings STRINGS.\n\
STRINGS should be a list of 16 elements, one for each shift combination.\n\
nil as element means don't change.\n\
See the documentation of `x-rebind-key' for more information.")
  (keycode, strings)
     register Lisp_Object keycode;
     register Lisp_Object strings;
{
  register Lisp_Object item;
  register unsigned char *rawstring;
  KeySym rawkey, modifier[1];
  int strsize;
  register unsigned i;

  check_x ();
  CHECK_NUMBER (keycode, 1);
  CHECK_CONS (strings, 2);
  rawkey = (KeySym) ((unsigned) (XINT (keycode))) & 255;
  for (i = 0; i <= 15; strings = Fcdr (strings), i++)
    {
      item = Fcar (strings);
      if (!NILP (item))
	{
	  CHECK_STRING (item, 2);
	  strsize = XSTRING (item)->size;
	  rawstring = (unsigned char *) xmalloc (strsize);
	  bcopy (XSTRING (item)->data, rawstring, strsize);
	  modifier[1] = 1 << i;
	  XRebindKeysym (x_current_display, rawkey, modifier, 1,
			 rawstring, strsize);
	}
    }
  return Qnil;
}
#endif /* HAVE_X11 */
#endif /* 0 */

#ifdef HAVE_X11

#ifndef HAVE_XSCREENNUMBEROFSCREEN
int
XScreenNumberOfScreen (scr)
    register Screen *scr;
{
  register Display *dpy;
  register Screen *dpyscr;
  register int i;

  dpy = scr->display;
  dpyscr = dpy->screens;

  for (i = 0; i < dpy->nscreens; i++, dpyscr++)
    if (scr == dpyscr)
      return i;

  return -1;
}
#endif /* not HAVE_XSCREENNUMBEROFSCREEN */

Visual *
select_visual (screen, depth)
     Screen *screen;
     unsigned int *depth;
{
  Visual *v;
  XVisualInfo *vinfo, vinfo_template;
  int n_visuals;

  v = DefaultVisualOfScreen (screen);

#ifdef HAVE_X11R4
  vinfo_template.visualid = XVisualIDFromVisual (v);
#else
  vinfo_template.visualid = v->visualid;
#endif

  vinfo_template.screen = XScreenNumberOfScreen (screen);

  vinfo = XGetVisualInfo (x_current_display,
			  VisualIDMask | VisualScreenMask, &vinfo_template,
			  &n_visuals);
  if (n_visuals != 1)
    fatal ("Can't get proper X visual info");

  if ((1 << vinfo->depth) == vinfo->colormap_size)
    *depth = vinfo->depth;
  else
    {
      int i = 0;
      int n = vinfo->colormap_size - 1;
      while (n)
	{
	  n = n >> 1;
	  i++;
	}
      *depth = i;
    }

  XFree ((char *) vinfo);
  return v;
}
#endif /* HAVE_X11 */

DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 2, 0, "Open a connection to an X server.\n\
DISPLAY is the name of the display to connect to.\n\
Optional second arg XRM_STRING is a string of resources in xrdb format.")
  (display, xrm_string)
     Lisp_Object display, xrm_string;
{
  unsigned int n_planes;
  unsigned char *xrm_option;

  CHECK_STRING (display, 0);
  if (x_current_display != 0)
    error ("X server connection is already initialized");
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string, 1);

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input. */
  x_term_init (XSTRING (display)->data);

#ifdef HAVE_X11
  XFASTINT (Vwindow_system_version) = 11;

  if (! NILP (xrm_string))
    xrm_option = (unsigned char *) XSTRING (xrm_string)->data;
  else
    xrm_option = (unsigned char *) 0;

  validate_x_resource_name ();

  BLOCK_INPUT;
  xrdb = x_load_resources (x_current_display, xrm_option,
			   (char *) XSTRING (Vx_resource_name)->data,
			   EMACS_CLASS);
  UNBLOCK_INPUT;
#ifdef HAVE_XRMSETDATABASE
  XrmSetDatabase (x_current_display, xrdb);
#else
  x_current_display->db = xrdb;
#endif

  x_screen = DefaultScreenOfDisplay (x_current_display);

  screen_visual = select_visual (x_screen, &n_planes);
  x_screen_planes = n_planes;
  x_screen_height = HeightOfScreen (x_screen);
  x_screen_width = WidthOfScreen (x_screen);

  /* X Atoms used by emacs. */
  Xatoms_of_xselect ();
  BLOCK_INPUT;
  Xatom_wm_protocols =     XInternAtom (x_current_display, "WM_PROTOCOLS",
					False);
  Xatom_wm_take_focus =    XInternAtom (x_current_display, "WM_TAKE_FOCUS",
					False);
  Xatom_wm_save_yourself = XInternAtom (x_current_display, "WM_SAVE_YOURSELF",
					False);
  Xatom_wm_delete_window = XInternAtom (x_current_display, "WM_DELETE_WINDOW",
					False);
  Xatom_wm_change_state =  XInternAtom (x_current_display, "WM_CHANGE_STATE",
					False);
  Xatom_wm_configure_denied =  XInternAtom (x_current_display,
					    "WM_CONFIGURE_DENIED", False);
  Xatom_wm_window_moved =  XInternAtom (x_current_display, "WM_MOVED",
					False);
  UNBLOCK_INPUT;
#else /* not HAVE_X11 */
  XFASTINT (Vwindow_system_version) = 10;
#endif /* not HAVE_X11 */
  return Qnil;
}

DEFUN ("x-close-current-connection", Fx_close_current_connection,
       Sx_close_current_connection,
       0, 0, 0, "Close the connection to the current X server.")
  ()
{
#ifdef HAVE_X11
  /* This is ONLY used when killing emacs;  For switching displays
     we'll have to take care of setting CloseDownMode elsewhere. */

  if (x_current_display)
    {
      BLOCK_INPUT;
      XSetCloseDownMode (x_current_display, DestroyAll);
      XCloseDisplay (x_current_display);
      x_current_display = 0;
    }
  else
    fatal ("No current X display connection to close\n");
#endif
  return Qnil;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize,
       1, 1, 0, "If ON is non-nil, report X errors as soon as the erring request is made.\n\
If ON is nil, allow buffering of requests.\n\
Turning on synchronization prohibits the Xlib routines from buffering\n\
requests and seriously degrades performance, but makes debugging much\n\
easier.")
  (on)
    Lisp_Object on;
{
  check_x ();

  XSynchronize (x_current_display, !EQ (on, Qnil));

  return Qnil;
}


syms_of_xfns ()
{
  /* This is zero if not using X windows.  */
  x_current_display = 0;

  /* The section below is built by the lisp expression at the top of the file,
     just above where these variables are declared.  */
  /*&&& init symbols here &&&*/
  Qauto_raise = intern ("auto-raise");
  staticpro (&Qauto_raise);
  Qauto_lower = intern ("auto-lower");
  staticpro (&Qauto_lower);
  Qbackground_color = intern ("background-color");
  staticpro (&Qbackground_color);
  Qbar = intern ("bar");
  staticpro (&Qbar);
  Qborder_color = intern ("border-color");
  staticpro (&Qborder_color);
  Qborder_width = intern ("border-width");
  staticpro (&Qborder_width);
  Qbox = intern ("box");
  staticpro (&Qbox);
  Qcursor_color = intern ("cursor-color");
  staticpro (&Qcursor_color);
  Qcursor_type = intern ("cursor-type");
  staticpro (&Qcursor_type);
  Qfont = intern ("font");
  staticpro (&Qfont);
  Qforeground_color = intern ("foreground-color");
  staticpro (&Qforeground_color);
  Qgeometry = intern ("geometry");
  staticpro (&Qgeometry);
  Qicon_left = intern ("icon-left");
  staticpro (&Qicon_left);
  Qicon_top = intern ("icon-top");
  staticpro (&Qicon_top);
  Qicon_type = intern ("icon-type");
  staticpro (&Qicon_type);
  Qinternal_border_width = intern ("internal-border-width");
  staticpro (&Qinternal_border_width);
  Qleft = intern ("left");
  staticpro (&Qleft);
  Qmouse_color = intern ("mouse-color");
  staticpro (&Qmouse_color);
  Qnone = intern ("none");
  staticpro (&Qnone);
  Qparent_id = intern ("parent-id");
  staticpro (&Qparent_id);
  Qsuppress_icon = intern ("suppress-icon");
  staticpro (&Qsuppress_icon);
  Qtop = intern ("top");
  staticpro (&Qtop);
  Qundefined_color = intern ("undefined-color");
  staticpro (&Qundefined_color);
  Qvertical_scroll_bars = intern ("vertical-scroll-bars");
  staticpro (&Qvertical_scroll_bars);
  Qvisibility = intern ("visibility");
  staticpro (&Qvisibility);
  Qwindow_id = intern ("window-id");
  staticpro (&Qwindow_id);
  Qx_frame_parameter = intern ("x-frame-parameter");
  staticpro (&Qx_frame_parameter);
  /* This is the end of symbol initialization.  */

  Fput (Qundefined_color, Qerror_conditions,
	Fcons (Qundefined_color, Fcons (Qerror, Qnil)));
  Fput (Qundefined_color, Qerror_message,
	build_string ("Undefined color"));

  init_x_parm_symbols ();

  DEFVAR_INT ("mouse-buffer-offset", &mouse_buffer_offset,
    "The buffer offset of the character under the pointer.");
  mouse_buffer_offset = 0;

  DEFVAR_LISP ("x-pointer-shape", &Vx_pointer_shape,
    "The shape of the pointer when over text.\n\
Changing the value does not affect existing frames\n\
unless you set the mouse color.");
  Vx_pointer_shape = Qnil;

  DEFVAR_LISP ("x-resource-name", &Vx_resource_name,
    "The name Emacs uses to look up X resources; for internal use only.\n\
`x-get-resource' uses this as the first component of the instance name\n\
when requesting resource values.\n\
Emacs initially sets `x-resource-name' to the name under which Emacs\n\
was invoked, or to the value specified with the `-name' or `-rn'\n\
switches, if present.");
  Vx_resource_name = Qnil;

#if 0
  DEFVAR_INT ("x-nontext-pointer-shape", &Vx_nontext_pointer_shape,
	      "The shape of the pointer when not over text.");
#endif
  Vx_nontext_pointer_shape = Qnil;

#if 0
  DEFVAR_INT ("x-mode-pointer-shape", &Vx_mode_pointer_shape,
	      "The shape of the pointer when over the mode line.");
#endif
  Vx_mode_pointer_shape = Qnil;

  DEFVAR_LISP ("x-cursor-fore-pixel", &Vx_cursor_fore_pixel,
	       "A string indicating the foreground color of the cursor box.");
  Vx_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("mouse-grabbed", &Vmouse_depressed,
	       "Non-nil if a mouse button is currently depressed.");
  Vmouse_depressed = Qnil;

  DEFVAR_LISP ("x-no-window-manager", &Vx_no_window_manager,
	       "t if no X window manager is in use.");

#ifdef HAVE_X11
  defsubr (&Sx_get_resource);
#if 0
  defsubr (&Sx_draw_rectangle);
  defsubr (&Sx_erase_rectangle);
  defsubr (&Sx_contour_region);
  defsubr (&Sx_uncontour_region);
#endif
  defsubr (&Sx_display_color_p);
  defsubr (&Sx_list_fonts);
  defsubr (&Sx_color_defined_p);
  defsubr (&Sx_server_max_request_size);
  defsubr (&Sx_server_vendor);
  defsubr (&Sx_server_version);
  defsubr (&Sx_display_pixel_width);
  defsubr (&Sx_display_pixel_height);
  defsubr (&Sx_display_mm_width);
  defsubr (&Sx_display_mm_height);
  defsubr (&Sx_display_screens);
  defsubr (&Sx_display_planes);
  defsubr (&Sx_display_color_cells);
  defsubr (&Sx_display_visual_class);
  defsubr (&Sx_display_backing_store);
  defsubr (&Sx_display_save_under);
#if 0
  defsubr (&Sx_rebind_key);
  defsubr (&Sx_rebind_keys);
  defsubr (&Sx_track_pointer);
  defsubr (&Sx_grab_pointer);
  defsubr (&Sx_ungrab_pointer);
#endif
#else
  defsubr (&Sx_get_default);
  defsubr (&Sx_store_cut_buffer);
  defsubr (&Sx_get_cut_buffer);
#endif
  defsubr (&Sx_parse_geometry);
  defsubr (&Sx_create_frame);
  defsubr (&Sfocus_frame);
  defsubr (&Sunfocus_frame);
#if 0
  defsubr (&Sx_horizontal_line);
#endif
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_current_connection);
  defsubr (&Sx_synchronize);
}

#endif /* HAVE_X_WINDOWS */
