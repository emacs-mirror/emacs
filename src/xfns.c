/* Functions for the X window system.
   Copyright (C) 1989, 92, 93, 94, 95, 96, 1997, 1998, 1999, 2000, 2001
     Free Software Foundation.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <config.h>
#include <signal.h>
#include <stdio.h>
#include <math.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* This makes the fields of a Display accessible, in Xlib header files.  */

#define XLIB_ILLEGAL_ACCESS

#include "lisp.h"
#include "xterm.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "intervals.h"
#include "dispextern.h"
#include "keyboard.h"
#include "blockinput.h"
#include <epaths.h>
#include "character.h"
#include "charset.h"
#include "coding.h"
#include "fontset.h"
#include "systime.h"
#include "termhooks.h"
#include "atimer.h"

#ifdef HAVE_X_WINDOWS

#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef VMS
#if 1 /* Used to be #ifdef EMACS_BITMAP_FILES, but this should always work.  */
#include "bitmaps/gray.xbm"
#else
#include <X11/bitmaps/gray>
#endif
#else
#include "[.bitmaps]gray.xbm"
#endif

#ifdef USE_X_TOOLKIT
#include <X11/Shell.h>

#ifndef USE_MOTIF
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Label.h>
#endif /* USE_MOTIF */

#ifdef USG
#undef USG	/* ####KLUDGE for Solaris 2.2 and up */
#include <X11/Xos.h>
#define USG
#else
#include <X11/Xos.h>
#endif

#include "widget.h"

#include "../lwlib/lwlib.h"

#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/FileSB.h>
#endif

/* Do the EDITRES protocol if running X11R5
   Exception: HP-UX (at least version A.09.05) has X11R5 without EditRes */

#if (XtSpecificationRelease >= 5) && !defined(NO_EDITRES)
#define HACK_EDITRES
extern void _XEditResCheckMessages ();
#endif /* R5 + Athena */

/* Unique id counter for widgets created by the Lucid Widget Library.  */

extern LWLIB_ID widget_id_tick;

#ifdef USE_LUCID
/* This is part of a kludge--see lwlib/xlwmenu.c.  */
extern XFontStruct *xlwmenu_default_font;
#endif

extern void free_frame_menubar ();
extern double atof ();

#ifdef USE_MOTIF

/* LessTif/Motif version info.  */

static Lisp_Object Vmotif_version_string;

#endif /* USE_MOTIF */

#endif /* USE_X_TOOLKIT */

#ifdef HAVE_X11R4
#define MAXREQUEST(dpy) (XMaxRequestSize (dpy))
#else
#define MAXREQUEST(dpy) ((dpy)->max_request_size)
#endif

/* The gray bitmap `bitmaps/gray'.  This is done because xterm.c uses
   it, and including `bitmaps/gray' more than once is a problem when
   config.h defines `static' as an empty replacement string.  */

int gray_bitmap_width = gray_width;
int gray_bitmap_height = gray_height;
char *gray_bitmap_bits = gray_bits;

/* The name we're using in resource queries.  Most often "emacs".  */

Lisp_Object Vx_resource_name;

/* The application class we're using in resource queries.
   Normally "Emacs".  */

Lisp_Object Vx_resource_class;

/* Non-zero means we're allowed to display an hourglass cursor.  */

int display_hourglass_p;

/* The background and shape of the mouse pointer, and shape when not
   over text or in the modeline.  */

Lisp_Object Vx_pointer_shape, Vx_nontext_pointer_shape, Vx_mode_pointer_shape;
Lisp_Object Vx_hourglass_pointer_shape;

/* The shape when over mouse-sensitive text.  */

Lisp_Object Vx_sensitive_text_pointer_shape;

/* If non-nil, the pointer shape to indicate that windows can be
   dragged horizontally.  */

Lisp_Object Vx_window_horizontal_drag_shape;

/* Color of chars displayed in cursor box.  */

Lisp_Object Vx_cursor_fore_pixel;

/* Nonzero if using X.  */

static int x_in_use;

/* Non nil if no window manager is in use.  */

Lisp_Object Vx_no_window_manager;

/* Search path for bitmap files.  */

Lisp_Object Vx_bitmap_file_path;

/* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.  */

Lisp_Object Vx_pixel_size_width_font_regexp;

Lisp_Object Qauto_raise;
Lisp_Object Qauto_lower;
Lisp_Object Qbar;
Lisp_Object Qborder_color;
Lisp_Object Qborder_width;
Lisp_Object Qbox;
Lisp_Object Qcursor_color;
Lisp_Object Qcursor_type;
Lisp_Object Qgeometry;
Lisp_Object Qicon_left;
Lisp_Object Qicon_top;
Lisp_Object Qicon_type;
Lisp_Object Qicon_name;
Lisp_Object Qinternal_border_width;
Lisp_Object Qleft;
Lisp_Object Qright;
Lisp_Object Qmouse_color;
Lisp_Object Qnone;
Lisp_Object Qouter_window_id;
Lisp_Object Qparent_id;
Lisp_Object Qscroll_bar_width;
Lisp_Object Qsuppress_icon;
extern Lisp_Object Qtop;
Lisp_Object Qundefined_color;
Lisp_Object Qvertical_scroll_bars;
Lisp_Object Qvisibility;
Lisp_Object Qwindow_id;
Lisp_Object Qx_frame_parameter;
Lisp_Object Qx_resource_name;
Lisp_Object Quser_position;
Lisp_Object Quser_size;
extern Lisp_Object Qdisplay;
Lisp_Object Qscroll_bar_foreground, Qscroll_bar_background;
Lisp_Object Qscreen_gamma, Qline_spacing, Qcenter;
Lisp_Object Qcompound_text, Qcancel_timer;
Lisp_Object Qwait_for_wm;
Lisp_Object Qfullscreen;
Lisp_Object Qfullwidth;
Lisp_Object Qfullheight;
Lisp_Object Qfullboth;

/* The below are defined in frame.c.  */

extern Lisp_Object Qheight, Qminibuffer, Qname, Qonly, Qwidth;
extern Lisp_Object Qunsplittable, Qmenu_bar_lines, Qbuffer_predicate, Qtitle;
extern Lisp_Object Qtool_bar_lines;

extern Lisp_Object Vwindow_system_version;

Lisp_Object Qface_set_after_frame_default;

#if GLYPH_DEBUG
int image_cache_refcount, dpyinfo_refcount;
#endif



/* Error if we are not connected to X.  */

void
check_x ()
{
  if (! x_in_use)
    error ("X windows are not in use or not initialized");
}

/* Nonzero if we can use mouse menus.
   You should not call this unless HAVE_MENUS is defined.  */

int
have_menus_p ()
{
  return x_in_use;
}

/* Extract a frame as a FRAME_PTR, defaulting to the selected frame
   and checking validity for X.  */

FRAME_PTR
check_x_frame (frame)
     Lisp_Object frame;
{
  FRAME_PTR f;

  if (NILP (frame))
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);
  if (! FRAME_X_P (f))
    error ("Non-X frame used");
  return f;
}

/* Let the user specify an X display with a frame.
   nil stands for the selected frame--or, if that is not an X frame,
   the first X display on the list.  */

static struct x_display_info *
check_x_display_info (frame)
     Lisp_Object frame;
{
  struct x_display_info *dpyinfo = NULL;
  
  if (NILP (frame))
    {
      struct frame *sf = XFRAME (selected_frame);
      
      if (FRAME_X_P (sf) && FRAME_LIVE_P (sf))
	dpyinfo = FRAME_X_DISPLAY_INFO (sf);
      else if (x_display_list != 0)
	dpyinfo = x_display_list;
      else
	error ("X windows are not in use or not initialized");
    }
  else if (STRINGP (frame))
    dpyinfo = x_display_info_for_name (frame);
  else
    {
      FRAME_PTR f;

      CHECK_LIVE_FRAME (frame);
      f = XFRAME (frame);
      if (! FRAME_X_P (f))
	error ("Non-X frame used");
      dpyinfo = FRAME_X_DISPLAY_INFO (f);
    }

  return dpyinfo;
}


/* Return the Emacs frame-object corresponding to an X window.
   It could be the frame's main window or an icon window.  */

/* This function can be called during GC, so use GC_xxx type test macros.  */

struct frame *
x_window_to_frame (dpyinfo, wdesc)
     struct x_display_info *dpyinfo;
     int wdesc;
{
  Lisp_Object tail, frame;
  struct frame *f;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_X_DISPLAY_INFO (f) != dpyinfo)
	continue;
      if (f->output_data.x->hourglass_window == wdesc)
	return f;
#ifdef USE_X_TOOLKIT
      if ((f->output_data.x->edit_widget 
	   && XtWindow (f->output_data.x->edit_widget) == wdesc)
	  /* A tooltip frame?  */
	  || (!f->output_data.x->edit_widget
	      && FRAME_X_WINDOW (f) == wdesc)
          || f->output_data.x->icon_desc == wdesc)
        return f;
#else /* not USE_X_TOOLKIT */
      if (FRAME_X_WINDOW (f) == wdesc
          || f->output_data.x->icon_desc == wdesc)
        return f;
#endif /* not USE_X_TOOLKIT */
    }
  return 0;
}

#ifdef USE_X_TOOLKIT
/* Like x_window_to_frame but also compares the window with the widget's
   windows.  */

struct frame *
x_any_window_to_frame (dpyinfo, wdesc)
     struct x_display_info *dpyinfo;
     int wdesc;
{
  Lisp_Object tail, frame;
  struct frame *f, *found;
  struct x_output *x;

  found = NULL;
  for (tail = Vframe_list; GC_CONSP (tail) && !found; tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      
      f = XFRAME (frame);
      if (FRAME_X_P (f) && FRAME_X_DISPLAY_INFO (f) == dpyinfo)
	{
	  /* This frame matches if the window is any of its widgets.  */
	  x = f->output_data.x;
	  if (x->hourglass_window == wdesc)
	    found = f;
	  else if (x->widget)
	    {
	      if (wdesc == XtWindow (x->widget) 
		  || wdesc == XtWindow (x->column_widget) 
		  || wdesc == XtWindow (x->edit_widget))
		found = f;
	      /* Match if the window is this frame's menubar.  */
	      else if (lw_window_is_in_menubar (wdesc, x->menubar_widget))
		found = f;
	    }
	  else if (FRAME_X_WINDOW (f) == wdesc)
	    /* A tooltip frame.  */
	    found = f;
	}
    }
  
  return found;
}

/* Likewise, but exclude the menu bar widget.  */

struct frame *
x_non_menubar_window_to_frame (dpyinfo, wdesc)
     struct x_display_info *dpyinfo;
     int wdesc;
{
  Lisp_Object tail, frame;
  struct frame *f;
  struct x_output *x;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_X_DISPLAY_INFO (f) != dpyinfo)
	continue;
      x = f->output_data.x;
      /* This frame matches if the window is any of its widgets.  */
      if (x->hourglass_window == wdesc)
	return f;
      else if (x->widget)
	{
	  if (wdesc == XtWindow (x->widget) 
	      || wdesc == XtWindow (x->column_widget) 
	      || wdesc == XtWindow (x->edit_widget))
	    return f;
	}
      else if (FRAME_X_WINDOW (f) == wdesc)
	/* A tooltip frame.  */
	return f;
    }
  return 0;
}

/* Likewise, but consider only the menu bar widget.  */

struct frame *
x_menubar_window_to_frame (dpyinfo, wdesc)
     struct x_display_info *dpyinfo;
     int wdesc;
{
  Lisp_Object tail, frame;
  struct frame *f;
  struct x_output *x;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_X_DISPLAY_INFO (f) != dpyinfo)
	continue;
      x = f->output_data.x;
      /* Match if the window is this frame's menubar.  */
      if (x->menubar_widget
	  && lw_window_is_in_menubar (wdesc, x->menubar_widget))
	return f;
    }
  return 0;
}

/* Return the frame whose principal (outermost) window is WDESC.
   If WDESC is some other (smaller) window, we return 0.  */

struct frame *
x_top_window_to_frame (dpyinfo, wdesc)
     struct x_display_info *dpyinfo;
     int wdesc;
{
  Lisp_Object tail, frame;
  struct frame *f;
  struct x_output *x;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_X_DISPLAY_INFO (f) != dpyinfo)
	continue;
      x = f->output_data.x;

      if (x->widget)
	{
	  /* This frame matches if the window is its topmost widget.  */
	  if (wdesc == XtWindow (x->widget))
	    return f;
#if 0 /* I don't know why it did this,
	 but it seems logically wrong,
	 and it causes trouble for MapNotify events.  */
	  /* Match if the window is this frame's menubar.  */
	  if (x->menubar_widget 
	      && wdesc == XtWindow (x->menubar_widget))
	    return f;
#endif
	}
      else if (FRAME_X_WINDOW (f) == wdesc)
	/* Tooltip frame.  */
	return f;
    }
  return 0;
}
#endif /* USE_X_TOOLKIT */



/* Code to deal with bitmaps.  Bitmaps are referenced by their bitmap
   id, which is just an int that this section returns.  Bitmaps are
   reference counted so they can be shared among frames.

   Bitmap indices are guaranteed to be > 0, so a negative number can
   be used to indicate no bitmap.

   If you use x_create_bitmap_from_data, then you must keep track of
   the bitmaps yourself.  That is, creating a bitmap from the same
   data more than once will not be caught.  */


/* Functions to access the contents of a bitmap, given an id.  */

int
x_bitmap_height (f, id)
     FRAME_PTR f;
     int id;
{
  return FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].height;
}

int
x_bitmap_width (f, id)
     FRAME_PTR f;
     int id;
{
  return FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].width;
}

int
x_bitmap_pixmap (f, id)
     FRAME_PTR f;
     int id;
{
  return FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].pixmap;
}


/* Allocate a new bitmap record.  Returns index of new record.  */

static int
x_allocate_bitmap_record (f)
     FRAME_PTR f;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  int i;

  if (dpyinfo->bitmaps == NULL)
    {
      dpyinfo->bitmaps_size = 10;
      dpyinfo->bitmaps
	= (struct x_bitmap_record *) xmalloc (dpyinfo->bitmaps_size * sizeof (struct x_bitmap_record));
      dpyinfo->bitmaps_last = 1;
      return 1;
    }

  if (dpyinfo->bitmaps_last < dpyinfo->bitmaps_size)
    return ++dpyinfo->bitmaps_last;

  for (i = 0; i < dpyinfo->bitmaps_size; ++i)
    if (dpyinfo->bitmaps[i].refcount == 0)
      return i + 1;

  dpyinfo->bitmaps_size *= 2;
  dpyinfo->bitmaps
    = (struct x_bitmap_record *) xrealloc (dpyinfo->bitmaps,
					   dpyinfo->bitmaps_size * sizeof (struct x_bitmap_record));
  return ++dpyinfo->bitmaps_last;
}

/* Add one reference to the reference count of the bitmap with id ID.  */

void
x_reference_bitmap (f, id)
     FRAME_PTR f;
     int id;
{
  ++FRAME_X_DISPLAY_INFO (f)->bitmaps[id - 1].refcount;
}

/* Create a bitmap for frame F from a HEIGHT x WIDTH array of bits at BITS.  */

int
x_create_bitmap_from_data (f, bits, width, height)
     struct frame *f;
     char *bits;
     unsigned int width, height;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Pixmap bitmap;
  int id;

  bitmap = XCreateBitmapFromData (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				  bits, width, height);

  if (! bitmap)
    return -1;

  id = x_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].file = NULL;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;

  return id;
}

/* Create bitmap from file FILE for frame F.  */

int
x_create_bitmap_from_file (f, file)
     struct frame *f;
     Lisp_Object file;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  unsigned int width, height;
  Pixmap bitmap;
  int xhot, yhot, result, id;
  Lisp_Object found;
  int fd;
  char *filename;

  /* Look for an existing bitmap with the same name.  */
  for (id = 0; id < dpyinfo->bitmaps_last; ++id)
    {
      if (dpyinfo->bitmaps[id].refcount
	  && dpyinfo->bitmaps[id].file
	  && !strcmp (dpyinfo->bitmaps[id].file, (char *) XSTRING (file)->data))
	{
	  ++dpyinfo->bitmaps[id].refcount;
	  return id + 1;
	}
    }

  /* Search bitmap-file-path for the file, if appropriate.  */
  fd = openp (Vx_bitmap_file_path, file, Qnil, &found, 0);
  if (fd < 0)
    return -1;
  emacs_close (fd);

  filename = (char *) XSTRING (found)->data;

  result = XReadBitmapFile (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    filename, &width, &height, &bitmap, &xhot, &yhot);
  if (result != BitmapSuccess)
    return -1;

  id = x_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].file
    = (char *) xmalloc (STRING_BYTES (XSTRING (file)) + 1);
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  strcpy (dpyinfo->bitmaps[id - 1].file, XSTRING (file)->data);

  return id;
}

/* Remove reference to bitmap with id number ID.  */

void
x_destroy_bitmap (f, id)
     FRAME_PTR f;
     int id;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  if (id > 0)
    {
      --dpyinfo->bitmaps[id - 1].refcount;
      if (dpyinfo->bitmaps[id - 1].refcount == 0)
	{
	  BLOCK_INPUT;
	  XFreePixmap (FRAME_X_DISPLAY (f), dpyinfo->bitmaps[id - 1].pixmap);
	  if (dpyinfo->bitmaps[id - 1].file)
	    {
	      xfree (dpyinfo->bitmaps[id - 1].file);
	      dpyinfo->bitmaps[id - 1].file = NULL;
	    }
	  UNBLOCK_INPUT;
	}
    }
}

/* Free all the bitmaps for the display specified by DPYINFO.  */

static void
x_destroy_all_bitmaps (dpyinfo)
     struct x_display_info *dpyinfo;
{
  int i;
  for (i = 0; i < dpyinfo->bitmaps_last; i++)
    if (dpyinfo->bitmaps[i].refcount > 0)
      {
	XFreePixmap (dpyinfo->display, dpyinfo->bitmaps[i].pixmap);
	if (dpyinfo->bitmaps[i].file)
	  xfree (dpyinfo->bitmaps[i].file);
      }
  dpyinfo->bitmaps_last = 0;
}

/* Connect the frame-parameter names for X frames
   to the ways of passing the parameter values to the window system.

   The name of a parameter, as a Lisp symbol,
   has an `x-frame-parameter' property which is an integer in Lisp
   that is an index in this table.  */

struct x_frame_parm_table
{
  char *name;
  void (*setter) P_ ((struct frame *, Lisp_Object, Lisp_Object));
};

static Lisp_Object unwind_create_frame P_ ((Lisp_Object));
static Lisp_Object unwind_create_tip_frame P_ ((Lisp_Object));
static void x_change_window_heights P_ ((Lisp_Object, int));
static void x_disable_image P_ ((struct frame *, struct image *));
void x_set_foreground_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_set_line_spacing P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_set_wait_for_wm P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_set_fullscreen P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_background_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_mouse_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_cursor_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_border_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_cursor_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_icon_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_icon_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_set_fringe_width P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_font P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_border_width P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_internal_border_width P_ ((struct frame *, Lisp_Object,
				      Lisp_Object));
void x_explicitly_set_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_autoraise P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_autolower P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_vertical_scroll_bars P_ ((struct frame *, Lisp_Object,
				     Lisp_Object));
void x_set_visibility P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_menu_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_scroll_bar_width P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_title P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_unsplittable P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_tool_bar_lines P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_scroll_bar_foreground P_ ((struct frame *, Lisp_Object,
				      Lisp_Object));
void x_set_scroll_bar_background P_ ((struct frame *, Lisp_Object,
				      Lisp_Object));
static Lisp_Object x_default_scroll_bar_color_parameter P_ ((struct frame *,
							     Lisp_Object,
							     Lisp_Object,
							     char *, char *,
							     int));
static void x_set_screen_gamma P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_edge_detection P_ ((struct frame *, struct image *, Lisp_Object,
				  Lisp_Object));
static void init_color_table P_ ((void));
static void free_color_table P_ ((void));
static unsigned long *colors_in_color_table P_ ((int *n));
static unsigned long lookup_rgb_color P_ ((struct frame *f, int r, int g, int b));
static unsigned long lookup_pixel_color P_ ((struct frame *f, unsigned long p));



static struct x_frame_parm_table x_frame_parms[] =
{
  {"auto-raise",		x_set_autoraise},
  {"auto-lower",		x_set_autolower},
  {"background-color",		x_set_background_color},
  {"border-color",		x_set_border_color},
  {"border-width",		x_set_border_width},
  {"cursor-color",		x_set_cursor_color},
  {"cursor-type",		x_set_cursor_type},
  {"font",			x_set_font},
  {"foreground-color",		x_set_foreground_color},
  {"icon-name",			x_set_icon_name},
  {"icon-type",			x_set_icon_type},
  {"internal-border-width",	x_set_internal_border_width},
  {"menu-bar-lines",		x_set_menu_bar_lines},
  {"mouse-color",		x_set_mouse_color},
  {"name",			x_explicitly_set_name},
  {"scroll-bar-width",		x_set_scroll_bar_width},
  {"title",			x_set_title},
  {"unsplittable",		x_set_unsplittable},
  {"vertical-scroll-bars",	x_set_vertical_scroll_bars},
  {"visibility",		x_set_visibility},
  {"tool-bar-lines",		x_set_tool_bar_lines},
  {"scroll-bar-foreground",	x_set_scroll_bar_foreground},
  {"scroll-bar-background",	x_set_scroll_bar_background},
  {"screen-gamma",		x_set_screen_gamma},
  {"line-spacing",		x_set_line_spacing},
  {"left-fringe",		x_set_fringe_width},
  {"right-fringe",		x_set_fringe_width},
  {"wait-for-wm",		x_set_wait_for_wm},
  {"fullscreen",                x_set_fullscreen},
  
};

/* Attach the `x-frame-parameter' properties to
   the Lisp symbol names of parameters relevant to X.  */

void
init_x_parm_symbols ()
{
  int i;

  for (i = 0; i < sizeof (x_frame_parms) / sizeof (x_frame_parms[0]); i++)
    Fput (intern (x_frame_parms[i].name), Qx_frame_parameter,
	  make_number (i));
}


/* Really try to move where we want to be in case of fullscreen.  Some WMs
   moves the window where we tell them.  Some (mwm, twm) moves the outer
   window manager window there instead.
   Try to compensate for those WM here. */
static void
x_fullscreen_move (f, new_top, new_left)
     struct frame *f;
     int new_top;
     int new_left;
{
  if (new_top != f->output_data.x->top_pos
      || new_left != f->output_data.x->left_pos)
    {
      int move_x = new_left + f->output_data.x->x_pixels_outer_diff;
      int move_y = new_top + f->output_data.x->y_pixels_outer_diff;

      f->output_data.x->want_fullscreen |= FULLSCREEN_MOVE_WAIT;
      x_set_offset (f, move_x, move_y, 1);
    }
}

/* Change the parameters of frame F as specified by ALIST.
   If a parameter is not specially recognized, do nothing special;
   otherwise call the `x_set_...' function for that parameter.
   Except for certain geometry properties, always call store_frame_param
   to store the new value in the parameter alist.  */

void
x_set_frame_parameters (f, alist)
     FRAME_PTR f;
     Lisp_Object alist;
{
  Lisp_Object tail;

  /* If both of these parameters are present, it's more efficient to
     set them both at once.  So we wait until we've looked at the
     entire list before we set them.  */
  int width, height;

  /* Same here.  */
  Lisp_Object left, top;

  /* Same with these.  */
  Lisp_Object icon_left, icon_top;

  /* Record in these vectors all the parms specified.  */
  Lisp_Object *parms;
  Lisp_Object *values;
  int i, p;
  int left_no_change = 0, top_no_change = 0;
  int icon_left_no_change = 0, icon_top_no_change = 0;
  int fullscreen_is_being_set = 0;

  struct gcpro gcpro1, gcpro2;

  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    i++;

  parms = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));
  values = (Lisp_Object *) alloca (i * sizeof (Lisp_Object));

  /* Extract parm names and values into those vectors.  */

  i = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt;

      elt = Fcar (tail);
      parms[i] = Fcar (elt);
      values[i] = Fcdr (elt);
      i++;
    }
  /* TAIL and ALIST are not used again below here.  */
  alist = tail = Qnil;

  GCPRO2 (*parms, *values);
  gcpro1.nvars = i;
  gcpro2.nvars = i;

  /* There is no need to gcpro LEFT, TOP, ICON_LEFT, or ICON_TOP,
     because their values appear in VALUES and strings are not valid.  */
  top = left = Qunbound;
  icon_left = icon_top = Qunbound;

  /* Provide default values for HEIGHT and WIDTH.  */
  if (FRAME_NEW_WIDTH (f))
    width = FRAME_NEW_WIDTH (f);
  else
    width = FRAME_WIDTH (f);

  if (FRAME_NEW_HEIGHT (f))
    height = FRAME_NEW_HEIGHT (f);
  else
    height = FRAME_HEIGHT (f);

  /* Process foreground_color and background_color before anything else.
     They are independent of other properties, but other properties (e.g.,
     cursor_color) are dependent upon them.  */
  /* Process default font as well, since fringe widths depends on it.  */
  /* Also, process fullscreen, width and height depend upon that */
  for (p = 0; p < i; p++) 
    {
      Lisp_Object prop, val;

      prop = parms[p];
      val = values[p];
      if (EQ (prop, Qforeground_color)
	  || EQ (prop, Qbackground_color)
	  || EQ (prop, Qfont)
          || EQ (prop, Qfullscreen))
	{
	  register Lisp_Object param_index, old_value;

	  old_value = get_frame_param (f, prop);
 	  fullscreen_is_being_set |= EQ (prop, Qfullscreen);
	  
	  if (NILP (Fequal (val, old_value)))
	    {
	      store_frame_param (f, prop, val);

	      param_index = Fget (prop, Qx_frame_parameter);
	      if (NATNUMP (param_index)
		  && (XFASTINT (param_index)
		      < sizeof (x_frame_parms)/sizeof (x_frame_parms[0])))
		(*x_frame_parms[XINT (param_index)].setter)(f, val, old_value);
	    }
	}
    }

  /* Now process them in reverse of specified order.  */
  for (i--; i >= 0; i--)
    {
      Lisp_Object prop, val;

      prop = parms[i];
      val = values[i];

      if (EQ (prop, Qwidth) && NUMBERP (val))
	width = XFASTINT (val);
      else if (EQ (prop, Qheight) && NUMBERP (val))
	height = XFASTINT (val);
      else if (EQ (prop, Qtop))
	top = val;
      else if (EQ (prop, Qleft))
	left = val;
      else if (EQ (prop, Qicon_top))
	icon_top = val;
      else if (EQ (prop, Qicon_left))
	icon_left = val;
      else if (EQ (prop, Qforeground_color)
	       || EQ (prop, Qbackground_color)
	       || EQ (prop, Qfont)
               || EQ (prop, Qfullscreen))
	/* Processed above.  */
	continue;
      else
	{
	  register Lisp_Object param_index, old_value;

	  old_value = get_frame_param (f, prop);

	  store_frame_param (f, prop, val);

	  param_index = Fget (prop, Qx_frame_parameter);
	  if (NATNUMP (param_index)
	      && (XFASTINT (param_index)
		  < sizeof (x_frame_parms)/sizeof (x_frame_parms[0])))
	    (*x_frame_parms[XINT (param_index)].setter)(f, val, old_value);
	}
    }

  /* Don't die if just one of these was set.  */
  if (EQ (left, Qunbound))
    {
      left_no_change = 1;
      if (f->output_data.x->left_pos < 0)
	left = Fcons (Qplus, Fcons (make_number (f->output_data.x->left_pos), Qnil));
      else
	XSETINT (left, f->output_data.x->left_pos);
    }
  if (EQ (top, Qunbound))
    {
      top_no_change = 1;
      if (f->output_data.x->top_pos < 0)
	top = Fcons (Qplus, Fcons (make_number (f->output_data.x->top_pos), Qnil));
      else
	XSETINT (top, f->output_data.x->top_pos);
    }

  /* If one of the icon positions was not set, preserve or default it.  */
  if (EQ (icon_left, Qunbound) || ! INTEGERP (icon_left))
    {
      icon_left_no_change = 1;
      icon_left = Fcdr (Fassq (Qicon_left, f->param_alist));
      if (NILP (icon_left))
	XSETINT (icon_left, 0);
    }
  if (EQ (icon_top, Qunbound) || ! INTEGERP (icon_top))
    {
      icon_top_no_change = 1;
      icon_top = Fcdr (Fassq (Qicon_top, f->param_alist));
      if (NILP (icon_top))
	XSETINT (icon_top, 0);
    }

  if (FRAME_VISIBLE_P (f) && fullscreen_is_being_set)
    {
      /* If the frame is visible already and the fullscreen parameter is
         being set, it is too late to set WM manager hints to specify
         size and position.
         Here we first get the width, height and position that applies to
         fullscreen.  We then move the frame to the appropriate
         position.  Resize of the frame is taken care of in the code after
         this if-statement. */
      int new_left, new_top;
      
      x_fullscreen_adjust (f, &width, &height, &new_top, &new_left);
      x_fullscreen_move (f, new_top, new_left);
    }
  
  /* Don't set these parameters unless they've been explicitly
     specified.  The window might be mapped or resized while we're in
     this function, and we don't want to override that unless the lisp
     code has asked for it.

     Don't set these parameters unless they actually differ from the
     window's current parameters; the window may not actually exist
     yet.  */
  {
    Lisp_Object frame;

    check_frame_size (f, &height, &width);

    XSETFRAME (frame, f);

    if (width != FRAME_WIDTH (f)
	|| height != FRAME_HEIGHT (f)
	|| FRAME_NEW_HEIGHT (f) || FRAME_NEW_WIDTH (f))
      Fset_frame_size (frame, make_number (width), make_number (height));

    if ((!NILP (left) || !NILP (top))
	&& ! (left_no_change && top_no_change)
	&& ! (NUMBERP (left) && XINT (left) == f->output_data.x->left_pos
	      && NUMBERP (top) && XINT (top) == f->output_data.x->top_pos))
      {
	int leftpos = 0;
	int toppos = 0;

	/* Record the signs.  */
	f->output_data.x->size_hint_flags &= ~ (XNegative | YNegative);
	if (EQ (left, Qminus))
	  f->output_data.x->size_hint_flags |= XNegative;
	else if (INTEGERP (left))
	  {
	    leftpos = XINT (left);
	    if (leftpos < 0)
	      f->output_data.x->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qminus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = - XINT (XCAR (XCDR (left)));
	    f->output_data.x->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qplus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = XINT (XCAR (XCDR (left)));
	  }

	if (EQ (top, Qminus))
	  f->output_data.x->size_hint_flags |= YNegative;
	else if (INTEGERP (top))
	  {
	    toppos = XINT (top);
	    if (toppos < 0)
	      f->output_data.x->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qminus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = - XINT (XCAR (XCDR (top)));
	    f->output_data.x->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qplus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = XINT (XCAR (XCDR (top)));
	  }


	/* Store the numeric value of the position.  */
	f->output_data.x->top_pos = toppos;
	f->output_data.x->left_pos = leftpos;

	f->output_data.x->win_gravity = NorthWestGravity;

	/* Actually set that position, and convert to absolute.  */
	x_set_offset (f, leftpos, toppos, -1);
      }

    if ((!NILP (icon_left) || !NILP (icon_top))
	&& ! (icon_left_no_change && icon_top_no_change))
      x_wm_set_icon_position (f, XINT (icon_left), XINT (icon_top));
  }

  UNGCPRO;
}

/* Store the screen positions of frame F into XPTR and YPTR.
   These are the positions of the containing window manager window,
   not Emacs's own window.  */

void
x_real_positions (f, xptr, yptr)
     FRAME_PTR f;
     int *xptr, *yptr;
{
  int win_x, win_y, outer_x, outer_y;
  int real_x = 0, real_y = 0;
  int had_errors = 0;
  Window win = f->output_data.x->parent_desc;

  int count;

  BLOCK_INPUT;

  count = x_catch_errors (FRAME_X_DISPLAY (f));

  if (win == FRAME_X_DISPLAY_INFO (f)->root_window)
    win = FRAME_OUTER_WINDOW (f);

  /* This loop traverses up the containment tree until we hit the root
     window.  Window managers may intersect many windows between our window
     and the root window.  The window we find just before the root window
     should be the outer WM window. */
  for (;;)
    {
      Window wm_window, rootw;
      Window *tmp_children;
      unsigned int tmp_nchildren;

      XQueryTree (FRAME_X_DISPLAY (f), win, &rootw,
                  &wm_window, &tmp_children, &tmp_nchildren);
      XFree ((char *) tmp_children);

      had_errors = x_had_errors_p (FRAME_X_DISPLAY (f));

      if (wm_window == rootw || had_errors)
        break;

      win = wm_window;
    }
    
  if (! had_errors)
    {
      int ign;
      Window child, rootw;
          
      /* Get the real coordinates for the WM window upper left corner */
      XGetGeometry (FRAME_X_DISPLAY (f), win,
                    &rootw, &real_x, &real_y, &ign, &ign, &ign, &ign);

      /* Translate real coordinates to coordinates relative to our
         window.  For our window, the upper left corner is 0, 0.
         Since the upper left corner of the WM window is outside
         our window, win_x and win_y will be negative:

         ------------------          ---> x
         |      title                |
         | -----------------         v y
         | |  our window
      */
      XTranslateCoordinates (FRAME_X_DISPLAY (f),

			     /* From-window, to-window.  */
			     FRAME_X_DISPLAY_INFO (f)->root_window,
                             FRAME_X_WINDOW (f),

			     /* From-position, to-position.  */
                             real_x, real_y, &win_x, &win_y,

			     /* Child of win.  */
			     &child);

      if (FRAME_X_WINDOW (f) == FRAME_OUTER_WINDOW (f))
	{
          outer_x = win_x;
          outer_y = win_y;
	}
      else
        {
          XTranslateCoordinates (FRAME_X_DISPLAY (f),

                                 /* From-window, to-window.  */
                                 FRAME_X_DISPLAY_INFO (f)->root_window,
                                 FRAME_OUTER_WINDOW (f),
                                     
                                 /* From-position, to-position.  */
                                 real_x, real_y, &outer_x, &outer_y,
                         
                                 /* Child of win.  */
                                 &child);
    }

      had_errors = x_had_errors_p (FRAME_X_DISPLAY (f));
    }
      
  x_uncatch_errors (FRAME_X_DISPLAY (f), count);
      
  UNBLOCK_INPUT;

  if (had_errors) return;
      
  f->output_data.x->x_pixels_diff = -win_x;
  f->output_data.x->y_pixels_diff = -win_y;
  f->output_data.x->x_pixels_outer_diff = -outer_x;
  f->output_data.x->y_pixels_outer_diff = -outer_y;

  *xptr = real_x;
  *yptr = real_y;
}

/* Insert a description of internally-recorded parameters of frame X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to the X window system
   and whose values are not correctly recorded in the frame's
   param_alist need to be considered here.  */

void
x_report_frame_params (f, alistptr)
     struct frame *f;
     Lisp_Object *alistptr;
{
  char buf[16];
  Lisp_Object tem;

  /* Represent negative positions (off the top or left screen edge)
     in a way that Fmodify_frame_parameters will understand correctly.  */
  XSETINT (tem, f->output_data.x->left_pos);
  if (f->output_data.x->left_pos >= 0)
    store_in_alist (alistptr, Qleft, tem);
  else
    store_in_alist (alistptr, Qleft, Fcons (Qplus, Fcons (tem, Qnil)));

  XSETINT (tem, f->output_data.x->top_pos);
  if (f->output_data.x->top_pos >= 0)
    store_in_alist (alistptr, Qtop, tem);
  else
    store_in_alist (alistptr, Qtop, Fcons (Qplus, Fcons (tem, Qnil)));

  store_in_alist (alistptr, Qborder_width,
       	   make_number (f->output_data.x->border_width));
  store_in_alist (alistptr, Qinternal_border_width,
       	   make_number (f->output_data.x->internal_border_width));
  store_in_alist (alistptr, Qleft_fringe,
       	   make_number (f->output_data.x->left_fringe_width));
  store_in_alist (alistptr, Qright_fringe,
       	   make_number (f->output_data.x->right_fringe_width));
  store_in_alist (alistptr, Qscroll_bar_width,
           make_number (FRAME_HAS_VERTICAL_SCROLL_BARS (f)
                        ? FRAME_SCROLL_BAR_PIXEL_WIDTH(f)
                        : 0));
  sprintf (buf, "%ld", (long) FRAME_X_WINDOW (f));
  store_in_alist (alistptr, Qwindow_id,
       	   build_string (buf));
#ifdef USE_X_TOOLKIT
  /* Tooltip frame may not have this widget.  */
  if (f->output_data.x->widget)
#endif
    sprintf (buf, "%ld", (long) FRAME_OUTER_WINDOW (f));
  store_in_alist (alistptr, Qouter_window_id,
       	   build_string (buf));
  store_in_alist (alistptr, Qicon_name, f->icon_name);
  FRAME_SAMPLE_VISIBILITY (f);
  store_in_alist (alistptr, Qvisibility,
		  (FRAME_VISIBLE_P (f) ? Qt
		   : FRAME_ICONIFIED_P (f) ? Qicon : Qnil));
  store_in_alist (alistptr, Qdisplay,
		  XCAR (FRAME_X_DISPLAY_INFO (f)->name_list_element));

  if (f->output_data.x->parent_desc == FRAME_X_DISPLAY_INFO (f)->root_window)
    tem = Qnil;
  else
    XSETFASTINT (tem, f->output_data.x->parent_desc);
  store_in_alist (alistptr, Qparent_id, tem);
}



/* Gamma-correct COLOR on frame F.  */

void
gamma_correct (f, color)
     struct frame *f;
     XColor *color;
{
  if (f->gamma)
    {
      color->red = pow (color->red / 65535.0, f->gamma) * 65535.0 + 0.5;
      color->green = pow (color->green / 65535.0, f->gamma) * 65535.0 + 0.5;
      color->blue = pow (color->blue / 65535.0, f->gamma) * 65535.0 + 0.5;
    }
}


/* Decide if color named COLOR_NAME is valid for use on frame F.  If
   so, return the RGB values in COLOR.  If ALLOC_P is non-zero,
   allocate the color.  Value is zero if COLOR_NAME is invalid, or
   no color could be allocated.  */

int
x_defined_color (f, color_name, color, alloc_p)
     struct frame *f;
     char *color_name;
     XColor *color;
     int alloc_p;
{
  int success_p;
  Display *dpy = FRAME_X_DISPLAY (f);
  Colormap cmap = FRAME_X_COLORMAP (f);

  BLOCK_INPUT;
  success_p = XParseColor (dpy, cmap, color_name, color);
  if (success_p && alloc_p)
    success_p = x_alloc_nearest_color (f, cmap, color);
  UNBLOCK_INPUT;

  return success_p;
}


/* Return the pixel color value for color COLOR_NAME on frame F.  If F
   is a monochrome frame, return MONO_COLOR regardless of what ARG says.
   Signal an error if color can't be allocated.  */

int
x_decode_color (f, color_name, mono_color)
     FRAME_PTR f;
     Lisp_Object color_name;
     int mono_color;
{
  XColor cdef;

  CHECK_STRING (color_name);

#if 0 /* Don't do this.  It's wrong when we're not using the default
	 colormap, it makes freeing difficult, and it's probably not
	 an important optimization.  */
  if (strcmp (XSTRING (color_name)->data, "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (XSTRING (color_name)->data, "white") == 0)
    return WHITE_PIX_DEFAULT (f);
#endif

  /* Return MONO_COLOR for monochrome frames.  */
  if (FRAME_X_DISPLAY_INFO (f)->n_planes == 1)
    return mono_color;

  /* x_defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (x_defined_color (f, XSTRING (color_name)->data, &cdef, 1))
    return cdef.pixel;

  Fsignal (Qerror, Fcons (build_string ("Undefined color"),
			  Fcons (color_name, Qnil)));
  return 0;
}



/* Change the `line-spacing' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.  */

static void
x_set_line_spacing (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  if (NILP (new_value))
    f->extra_line_spacing = 0;
  else if (NATNUMP (new_value))
    f->extra_line_spacing = XFASTINT (new_value);
  else
    Fsignal (Qerror, Fcons (build_string ("Invalid line-spacing"),
			    Fcons (new_value, Qnil)));
  if (FRAME_VISIBLE_P (f))
    redraw_frame (f);
}


/* Change the `wait-for-wm' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.
   See also the comment of wait_for_wm in struct x_output.  */

static void
x_set_wait_for_wm (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  f->output_data.x->wait_for_wm = !NILP (new_value);
}


/* Change the `fullscreen' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value. */

static void
x_set_fullscreen (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  if (NILP (new_value))
    f->output_data.x->want_fullscreen = FULLSCREEN_NONE;
  else if (EQ (new_value, Qfullboth))
    f->output_data.x->want_fullscreen = FULLSCREEN_BOTH;
  else if (EQ (new_value, Qfullwidth))
    f->output_data.x->want_fullscreen = FULLSCREEN_WIDTH;
  else if (EQ (new_value, Qfullheight))
    f->output_data.x->want_fullscreen = FULLSCREEN_HEIGHT;
}


/* Change the `screen-gamma' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new
   value.  */

static void
x_set_screen_gamma (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  if (NILP (new_value))
    f->gamma = 0;
  else if (NUMBERP (new_value) && XFLOATINT (new_value) > 0)
    /* The value 0.4545 is the normal viewing gamma.  */
    f->gamma = 1.0 / (0.4545 * XFLOATINT (new_value));
  else
    Fsignal (Qerror, Fcons (build_string ("Invalid screen-gamma"),
			    Fcons (new_value, Qnil)));

  clear_face_cache (0);
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
  struct x_output *x = f->output_data.x;
  unsigned long fg, old_fg;

  fg = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  old_fg = x->foreground_pixel;
  x->foreground_pixel = fg;

  if (FRAME_X_WINDOW (f) != 0)
    {
      Display *dpy = FRAME_X_DISPLAY (f);
      
      BLOCK_INPUT;
      XSetForeground (dpy, x->normal_gc, fg);
      XSetBackground (dpy, x->reverse_gc, fg);

      if (x->cursor_pixel == old_fg)
	{
	  unload_color (f, x->cursor_pixel);
	  x->cursor_pixel = x_copy_color (f, fg);
	  XSetBackground (dpy, x->cursor_gc, x->cursor_pixel);
	}
      
      UNBLOCK_INPUT;
      
      update_face_from_frame_parameter (f, Qforeground_color, arg);
      
      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
      
  unload_color (f, old_fg);
}

void
x_set_background_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  struct x_output *x = f->output_data.x;
  unsigned long bg;

  bg = x_decode_color (f, arg, WHITE_PIX_DEFAULT (f));
  unload_color (f, x->background_pixel);
  x->background_pixel = bg;

  if (FRAME_X_WINDOW (f) != 0)
    {
      Display *dpy = FRAME_X_DISPLAY (f);
      
      BLOCK_INPUT;
      XSetBackground (dpy, x->normal_gc, bg);
      XSetForeground (dpy, x->reverse_gc, bg);
      XSetWindowBackground (dpy, FRAME_X_WINDOW (f), bg);
      XSetForeground (dpy, x->cursor_gc, bg);

#ifndef USE_TOOLKIT_SCROLL_BARS /* Turns out to be annoying with
				   toolkit scroll bars.  */
      {
	Lisp_Object bar;
	for (bar = FRAME_SCROLL_BARS (f);
	     !NILP (bar);
	     bar = XSCROLL_BAR (bar)->next)
	  {
	    Window window = SCROLL_BAR_X_WINDOW (XSCROLL_BAR (bar));
	    XSetWindowBackground (dpy, window, bg);
	  }
      }
#endif /* USE_TOOLKIT_SCROLL_BARS */

      UNBLOCK_INPUT;
      update_face_from_frame_parameter (f, Qbackground_color, arg);

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

void
x_set_mouse_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  struct x_output *x = f->output_data.x;
  Display *dpy = FRAME_X_DISPLAY (f);
  Cursor cursor, nontext_cursor, mode_cursor, cross_cursor;
  Cursor hourglass_cursor, horizontal_drag_cursor;
  int count;
  unsigned long pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  unsigned long mask_color = x->background_pixel;

  /* Don't let pointers be invisible.  */
  if (mask_color == pixel)
    {
      x_free_colors (f, &pixel, 1);
      pixel = x_copy_color (f, x->foreground_pixel);
    }

  unload_color (f, x->mouse_pixel);
  x->mouse_pixel = pixel;

  BLOCK_INPUT;

  /* It's not okay to crash if the user selects a screwy cursor.  */
  count = x_catch_errors (dpy);

  if (!NILP (Vx_pointer_shape))
    {
      CHECK_NUMBER (Vx_pointer_shape);
      cursor = XCreateFontCursor (dpy, XINT (Vx_pointer_shape));
    }
  else
    cursor = XCreateFontCursor (dpy, XC_xterm);
  x_check_errors (dpy, "bad text pointer cursor: %s");

  if (!NILP (Vx_nontext_pointer_shape))
    {
      CHECK_NUMBER (Vx_nontext_pointer_shape);
      nontext_cursor
	= XCreateFontCursor (dpy, XINT (Vx_nontext_pointer_shape));
    }
  else
    nontext_cursor = XCreateFontCursor (dpy, XC_left_ptr);
  x_check_errors (dpy, "bad nontext pointer cursor: %s");

  if (!NILP (Vx_hourglass_pointer_shape))
    {
      CHECK_NUMBER (Vx_hourglass_pointer_shape);
      hourglass_cursor
	= XCreateFontCursor (dpy, XINT (Vx_hourglass_pointer_shape));
    }
  else
    hourglass_cursor = XCreateFontCursor (dpy, XC_watch);
  x_check_errors (dpy, "bad hourglass pointer cursor: %s");
  
  x_check_errors (dpy, "bad nontext pointer cursor: %s");
  if (!NILP (Vx_mode_pointer_shape))
    {
      CHECK_NUMBER (Vx_mode_pointer_shape);
      mode_cursor = XCreateFontCursor (dpy, XINT (Vx_mode_pointer_shape));
    }
  else
    mode_cursor = XCreateFontCursor (dpy, XC_xterm);
  x_check_errors (dpy, "bad modeline pointer cursor: %s");

  if (!NILP (Vx_sensitive_text_pointer_shape))
    {
      CHECK_NUMBER (Vx_sensitive_text_pointer_shape);
      cross_cursor
	= XCreateFontCursor (dpy, XINT (Vx_sensitive_text_pointer_shape));
    }
  else
    cross_cursor = XCreateFontCursor (dpy, XC_crosshair);

  if (!NILP (Vx_window_horizontal_drag_shape))
    {
      CHECK_NUMBER (Vx_window_horizontal_drag_shape);
      horizontal_drag_cursor
	= XCreateFontCursor (dpy, XINT (Vx_window_horizontal_drag_shape));
    }
  else
    horizontal_drag_cursor
      = XCreateFontCursor (dpy, XC_sb_h_double_arrow);

  /* Check and report errors with the above calls.  */
  x_check_errors (dpy, "can't set cursor shape: %s");
  x_uncatch_errors (dpy, count);

  {
    XColor fore_color, back_color;

    fore_color.pixel = x->mouse_pixel;
    x_query_color (f, &fore_color);
    back_color.pixel = mask_color;
    x_query_color (f, &back_color);
    
    XRecolorCursor (dpy, cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, nontext_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, mode_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, cross_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, hourglass_cursor, &fore_color, &back_color);
    XRecolorCursor (dpy, horizontal_drag_cursor, &fore_color, &back_color);
  }

  if (FRAME_X_WINDOW (f) != 0)
    XDefineCursor (dpy, FRAME_X_WINDOW (f), cursor);

  if (cursor != x->text_cursor
      && x->text_cursor != 0)
    XFreeCursor (dpy, x->text_cursor);
  x->text_cursor = cursor;

  if (nontext_cursor != x->nontext_cursor
      && x->nontext_cursor != 0)
    XFreeCursor (dpy, x->nontext_cursor);
  x->nontext_cursor = nontext_cursor;

  if (hourglass_cursor != x->hourglass_cursor
      && x->hourglass_cursor != 0)
    XFreeCursor (dpy, x->hourglass_cursor);
  x->hourglass_cursor = hourglass_cursor;

  if (mode_cursor != x->modeline_cursor
      && x->modeline_cursor != 0)
    XFreeCursor (dpy, f->output_data.x->modeline_cursor);
  x->modeline_cursor = mode_cursor;
  
  if (cross_cursor != x->cross_cursor
      && x->cross_cursor != 0)
    XFreeCursor (dpy, x->cross_cursor);
  x->cross_cursor = cross_cursor;

  if (horizontal_drag_cursor != x->horizontal_drag_cursor
      && x->horizontal_drag_cursor != 0)
    XFreeCursor (dpy, x->horizontal_drag_cursor);
  x->horizontal_drag_cursor = horizontal_drag_cursor;

  XFlush (dpy);
  UNBLOCK_INPUT;

  update_face_from_frame_parameter (f, Qmouse_color, arg);
}

void
x_set_cursor_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned long fore_pixel, pixel;
  int fore_pixel_allocated_p = 0, pixel_allocated_p = 0;
  struct x_output *x = f->output_data.x;

  if (!NILP (Vx_cursor_fore_pixel))
    {
      fore_pixel = x_decode_color (f, Vx_cursor_fore_pixel,
				   WHITE_PIX_DEFAULT (f));
      fore_pixel_allocated_p = 1;
    }
  else
    fore_pixel = x->background_pixel;
  
  pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  pixel_allocated_p = 1;

  /* Make sure that the cursor color differs from the background color.  */
  if (pixel == x->background_pixel)
    {
      if (pixel_allocated_p)
	{
	  x_free_colors (f, &pixel, 1);
	  pixel_allocated_p = 0;
	}
      
      pixel = x->mouse_pixel;
      if (pixel == fore_pixel)
	{
	  if (fore_pixel_allocated_p)
	    {
	      x_free_colors (f, &fore_pixel, 1);
	      fore_pixel_allocated_p = 0;
	    }
	  fore_pixel = x->background_pixel;
	}
    }

  unload_color (f, x->cursor_foreground_pixel);
  if (!fore_pixel_allocated_p)
    fore_pixel = x_copy_color (f, fore_pixel);
  x->cursor_foreground_pixel = fore_pixel;

  unload_color (f, x->cursor_pixel);
  if (!pixel_allocated_p)
    pixel = x_copy_color (f, pixel);
  x->cursor_pixel = pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      XSetBackground (FRAME_X_DISPLAY (f), x->cursor_gc, x->cursor_pixel);
      XSetForeground (FRAME_X_DISPLAY (f), x->cursor_gc, fore_pixel);
      UNBLOCK_INPUT;

      if (FRAME_VISIBLE_P (f))
	{
	  x_update_cursor (f, 0);
	  x_update_cursor (f, 1);
	}
    }

  update_face_from_frame_parameter (f, Qcursor_color, arg);
}

/* Set the border-color of frame F to value described by ARG.
   ARG can be a string naming a color.
   The border-color is used for the border that is drawn by the X server.
   Note that this does not fully take effect if done before
   F has an x-window; it must be redone when the window is created.

   Note: this is done in two routines because of the way X10 works.

   Note: under X11, this is normally the province of the window manager,
   and so emacs' border colors may be overridden.  */

void
x_set_border_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int pix;

  CHECK_STRING (arg);
  pix = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  x_set_border_pixel (f, pix);
  update_face_from_frame_parameter (f, Qborder_color, arg);
}

/* Set the border-color of frame F to pixel value PIX.
   Note that this does not fully take effect if done before
   F has an x-window.  */

void
x_set_border_pixel (f, pix)
     struct frame *f;
     int pix;
{
  unload_color (f, f->output_data.x->border_pixel);
  f->output_data.x->border_pixel = pix;

  if (FRAME_X_WINDOW (f) != 0 && f->output_data.x->border_width > 0)
    {
      BLOCK_INPUT;
      XSetWindowBorder (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			(unsigned long)pix);
      UNBLOCK_INPUT;

      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}


/* Value is the internal representation of the specified cursor type
   ARG.  If type is BAR_CURSOR, return in *WIDTH the specified width
   of the bar cursor.  */

enum text_cursor_kinds
x_specified_cursor_type (arg, width)
     Lisp_Object arg;
     int *width;
{
  enum text_cursor_kinds type;
  
  if (EQ (arg, Qbar))
    {
      type = BAR_CURSOR;
      *width = 2;
    }
  else if (CONSP (arg)
	   && EQ (XCAR (arg), Qbar)
	   && INTEGERP (XCDR (arg))
	   && XINT (XCDR (arg)) >= 0)
    {
      type = BAR_CURSOR;
      *width = XINT (XCDR (arg));
    }
  else if (NILP (arg))
    type = NO_CURSOR;
  else
    /* Treat anything unknown as "box cursor".
       It was bad to signal an error; people have trouble fixing
       .Xdefaults with Emacs, when it has something bad in it.  */
    type = FILLED_BOX_CURSOR;

  return type;
}

void
x_set_cursor_type (f, arg, oldval)
     FRAME_PTR f;
     Lisp_Object arg, oldval;
{
  int width;
  
  FRAME_DESIRED_CURSOR (f) = x_specified_cursor_type (arg, &width);
  f->output_data.x->cursor_width = width;

  /* Make sure the cursor gets redrawn.  This is overkill, but how
     often do people change cursor types?  */
  update_mode_lines++;
}

void
x_set_icon_type (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!STRINGP (oldval) && EQ (oldval, Qnil) == EQ (arg, Qnil))
    return;

  BLOCK_INPUT;
  if (NILP (arg))
    result = x_text_icon (f,
			  (char *) XSTRING ((!NILP (f->icon_name)
					     ? f->icon_name
					     : f->name))->data);
  else
    result = x_bitmap_icon (f, arg);

  if (result)
    {
      UNBLOCK_INPUT;
      error ("No icon window available");
    }

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
}

/* Return non-nil if frame F wants a bitmap icon.  */

Lisp_Object
x_icon_type (f)
     FRAME_PTR f;
{
  Lisp_Object tem;

  tem = assq_no_quit (Qicon_type, f->param_alist);
  if (CONSP (tem))
    return XCDR (tem);
  else
    return Qnil;
}

void
x_set_icon_name (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!STRINGP (oldval) && EQ (oldval, Qnil) == EQ (arg, Qnil))
    return;

  f->icon_name = arg;

  if (f->output_data.x->icon_bitmap != 0)
    return;

  BLOCK_INPUT;

  result = x_text_icon (f,
			(char *) XSTRING ((!NILP (f->icon_name)
					   ? f->icon_name
					   : !NILP (f->title)
					   ? f->title
					   : f->name))->data);

  if (result)
    {
      UNBLOCK_INPUT;
      error ("No icon window available");
    }

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;
}

void
x_set_font (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  Lisp_Object result;
  Lisp_Object fontset_name;
  Lisp_Object frame;
  int old_fontset = f->output_data.x->fontset;

  CHECK_STRING (arg);

  fontset_name = Fquery_fontset (arg, Qnil);

  BLOCK_INPUT;
  result = (STRINGP (fontset_name)
	    ? x_new_fontset (f, XSTRING (fontset_name)->data)
	    : x_new_font (f, XSTRING (arg)->data));
  UNBLOCK_INPUT;
  
  if (EQ (result, Qnil))
    error ("Font `%s' is not defined", XSTRING (arg)->data);
  else if (EQ (result, Qt))
    error ("The characters of the given font have varying widths");
  else if (STRINGP (result))
    {
      if (STRINGP (fontset_name))
	{
	  /* Fontset names are built from ASCII font names, so the
	     names may be equal despite there was a change.  */
	  if (old_fontset == f->output_data.x->fontset)
	    return;
	}
      else if (!NILP (Fequal (result, oldval)))
	return;
      
      store_frame_param (f, Qfont, result);
      recompute_basic_faces (f);
    }
  else
    abort ();

  do_pending_window_change (0);

  /* Don't call `face-set-after-frame-default' when faces haven't been
     initialized yet.  This is the case when called from
     Fx_create_frame.  In that case, the X widget or window doesn't
     exist either, and we can end up in x_report_frame_params with a
     null widget which gives a segfault.  */
  if (FRAME_FACE_CACHE (f))
    {
      XSETFRAME (frame, f);
      call1 (Qface_set_after_frame_default, frame);
    }
}

static void
x_set_fringe_width (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  x_compute_fringe_widths (f, 1);
}

void
x_set_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  CHECK_NUMBER (arg);

  if (XINT (arg) == f->output_data.x->border_width)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    error ("Cannot change the border width of a window");

  f->output_data.x->border_width = XINT (arg);
}

void
x_set_internal_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int old = f->output_data.x->internal_border_width;

  CHECK_NUMBER (arg);
  f->output_data.x->internal_border_width = XINT (arg);
  if (f->output_data.x->internal_border_width < 0)
    f->output_data.x->internal_border_width = 0;

#ifdef USE_X_TOOLKIT
  if (f->output_data.x->edit_widget)
    widget_store_internal_border (f->output_data.x->edit_widget);
#endif

  if (f->output_data.x->internal_border_width == old)
    return;

  if (FRAME_X_WINDOW (f) != 0)
    {
      x_set_window_size (f, 0, f->width, f->height);
      SET_FRAME_GARBAGED (f);
      do_pending_window_change (0);
    }
  else
    SET_FRAME_GARBAGED (f);
}

void
x_set_visibility (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  Lisp_Object frame;
  XSETFRAME (frame, f);

  if (NILP (value))
    Fmake_frame_invisible (frame, Qt);
  else if (EQ (value, Qicon))
    Ficonify_frame (frame);
  else
    Fmake_frame_visible (frame);
}


/* Change window heights in windows rooted in WINDOW by N lines.  */

static void
x_change_window_heights (window, n)
  Lisp_Object window;
  int n;
{
  struct window *w = XWINDOW (window);

  XSETFASTINT (w->top, XFASTINT (w->top) + n);
  XSETFASTINT (w->height, XFASTINT (w->height) - n);

  if (INTEGERP (w->orig_top))
    XSETFASTINT (w->orig_top, XFASTINT (w->orig_top) + n);
  if (INTEGERP (w->orig_height))
    XSETFASTINT (w->orig_height, XFASTINT (w->orig_height) - n);

  /* Handle just the top child in a vertical split.  */
  if (!NILP (w->vchild))
    x_change_window_heights (w->vchild, n);

  /* Adjust all children in a horizontal split.  */
  for (window = w->hchild; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      x_change_window_heights (window, n);
    }
}

void
x_set_menu_bar_lines (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  int nlines;
#ifndef USE_X_TOOLKIT
  int olines = FRAME_MENU_BAR_LINES (f);
#endif

  /* Right now, menu bars don't work properly in minibuf-only frames;
     most of the commands try to apply themselves to the minibuffer
     frame itself, and get an error because you can't switch buffers
     in or split the minibuffer window.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  if (INTEGERP (value))
    nlines = XINT (value);
  else
    nlines = 0;

  /* Make sure we redisplay all windows in this frame.  */
  windows_or_buffers_changed++;

#ifdef USE_X_TOOLKIT
  FRAME_MENU_BAR_LINES (f) = 0;
  if (nlines)
    {
      FRAME_EXTERNAL_MENU_BAR (f) = 1;
      if (FRAME_X_P (f) && f->output_data.x->menubar_widget == 0)
	/* Make sure next redisplay shows the menu bar.  */
	XWINDOW (FRAME_SELECTED_WINDOW (f))->update_mode_line = Qt;
    }
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 1)
	free_frame_menubar (f);
      FRAME_EXTERNAL_MENU_BAR (f) = 0;
      if (FRAME_X_P (f))
	f->output_data.x->menubar_widget = 0;
    }
#else /* not USE_X_TOOLKIT */
  FRAME_MENU_BAR_LINES (f) = nlines;
  x_change_window_heights (f->root_window, nlines - olines);
#endif /* not USE_X_TOOLKIT */
  adjust_glyphs (f);
}


/* Set the number of lines used for the tool bar of frame F to VALUE.
   VALUE not an integer, or < 0 means set the lines to zero.  OLDVAL
   is the old number of tool bar lines.  This function changes the
   height of all windows on frame F to match the new tool bar height.
   The frame's height doesn't change.  */

void
x_set_tool_bar_lines (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  int delta, nlines, root_height;
  Lisp_Object root_window;

  /* Treat tool bars like menu bars.  */
  if (FRAME_MINIBUF_ONLY_P (f))
    return;

  /* Use VALUE only if an integer >= 0.  */
  if (INTEGERP (value) && XINT (value) >= 0)
    nlines = XFASTINT (value);
  else
    nlines = 0;

  /* Make sure we redisplay all windows in this frame.  */
  ++windows_or_buffers_changed;

  delta = nlines - FRAME_TOOL_BAR_LINES (f);

  /* Don't resize the tool-bar to more than we have room for.  */
  root_window = FRAME_ROOT_WINDOW (f);
  root_height = XINT (XWINDOW (root_window)->height);
  if (root_height - delta < 1)
    {
      delta = root_height - 1;
      nlines = FRAME_TOOL_BAR_LINES (f) + delta;
    }

  FRAME_TOOL_BAR_LINES (f) = nlines;
  x_change_window_heights (root_window, delta);
  adjust_glyphs (f);
  
  /* We also have to make sure that the internal border at the top of
     the frame, below the menu bar or tool bar, is redrawn when the
     tool bar disappears.  This is so because the internal border is
     below the tool bar if one is displayed, but is below the menu bar
     if there isn't a tool bar.  The tool bar draws into the area
     below the menu bar.  */
  if (FRAME_X_WINDOW (f) && FRAME_TOOL_BAR_LINES (f) == 0)
    {
      updating_frame = f;
      clear_frame ();
      clear_current_matrices (f);
      updating_frame = NULL;
    }

  /* If the tool bar gets smaller, the internal border below it
     has to be cleared.  It was formerly part of the display
     of the larger tool bar, and updating windows won't clear it.  */
  if (delta < 0)
    {
      int height = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = PIXEL_WIDTH (f);
      int y = nlines * CANON_Y_UNIT (f);

      BLOCK_INPUT;
      x_clear_area (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    0, y, width, height, False);
      UNBLOCK_INPUT;

      if (WINDOWP (f->tool_bar_window))
	clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);
    }
}


/* Set the foreground color for scroll bars on frame F to VALUE.
   VALUE should be a string, a color name.  If it isn't a string or
   isn't a valid color name, do nothing.  OLDVAL is the old value of
   the frame parameter.  */

void
x_set_scroll_bar_foreground (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  unsigned long pixel;
  
  if (STRINGP (value))
    pixel = x_decode_color (f, value, BLACK_PIX_DEFAULT (f));
  else
    pixel = -1;

  if (f->output_data.x->scroll_bar_foreground_pixel != -1)
    unload_color (f, f->output_data.x->scroll_bar_foreground_pixel);
  
  f->output_data.x->scroll_bar_foreground_pixel = pixel;
  if (FRAME_X_WINDOW (f) && FRAME_VISIBLE_P (f))
    {
      /* Remove all scroll bars because they have wrong colors.  */
      if (condemn_scroll_bars_hook)
	(*condemn_scroll_bars_hook) (f);
      if (judge_scroll_bars_hook)
	(*judge_scroll_bars_hook) (f);

      update_face_from_frame_parameter (f, Qscroll_bar_foreground, value);
      redraw_frame (f);
    }
}


/* Set the background color for scroll bars on frame F to VALUE VALUE
   should be a string, a color name.  If it isn't a string or isn't a
   valid color name, do nothing.  OLDVAL is the old value of the frame
   parameter.  */

void
x_set_scroll_bar_background (f, value, oldval)
     struct frame *f;
     Lisp_Object value, oldval;
{
  unsigned long pixel;

  if (STRINGP (value))
    pixel = x_decode_color (f, value, WHITE_PIX_DEFAULT (f));
  else
    pixel = -1;
  
  if (f->output_data.x->scroll_bar_background_pixel != -1)
    unload_color (f, f->output_data.x->scroll_bar_background_pixel);
  
#ifdef USE_TOOLKIT_SCROLL_BARS
  /* Scrollbar shadow colors.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel != -1)
    {
      unload_color (f, f->output_data.x->scroll_bar_top_shadow_pixel);
      f->output_data.x->scroll_bar_top_shadow_pixel = -1;
    }
  if (f->output_data.x->scroll_bar_bottom_shadow_pixel != -1)
    {
      unload_color (f, f->output_data.x->scroll_bar_bottom_shadow_pixel);
      f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
    }
#endif /* USE_TOOLKIT_SCROLL_BARS */

  f->output_data.x->scroll_bar_background_pixel = pixel;
  if (FRAME_X_WINDOW (f) && FRAME_VISIBLE_P (f))
    {
      /* Remove all scroll bars because they have wrong colors.  */
      if (condemn_scroll_bars_hook)
	(*condemn_scroll_bars_hook) (f);
      if (judge_scroll_bars_hook)
	(*judge_scroll_bars_hook) (f);
      
      update_face_from_frame_parameter (f, Qscroll_bar_background, value);
      redraw_frame (f);
    }
}


/* Encode Lisp string STRING as a text in a format appropriate for
   XICCC (X Inter Client Communication Conventions).

   If STRING contains only ASCII characters, do no conversion and
   return the string data of STRING.  Otherwise, encode the text by
   CODING_SYSTEM, and return a newly allocated memory area which
   should be freed by `xfree' by a caller.

   SELECTIONP non-zero means the string is being encoded for an X
   selection, so it is safe to run pre-write conversions (which
   may run Lisp code).

   Store the byte length of resulting text in *TEXT_BYTES.

   If the text contains only ASCII and Latin-1, store 1 in *STRING_P,
   which means that the `encoding' of the result can be `STRING'.
   Otherwise store 0 in *STRINGP, which means that the `encoding' of
   the result should be `COMPOUND_TEXT'.  */

unsigned char *
x_encode_text (string, coding_system, selectionp, text_bytes, stringp)
     Lisp_Object string, coding_system;
     int *text_bytes, *stringp;
     int selectionp;
{
  int result = string_xstring_p (string);
  struct coding_system coding;

  if (result == 0)
    {
      /* No multibyte character in OBJ.  We need not encode it.  */
      *text_bytes = STRING_BYTES (XSTRING (string));
      *stringp = 1;
      return XSTRING (string)->data;
    }

  setup_coding_system (coding_system, &coding);
  coding.mode |= (CODING_MODE_SAFE_ENCODING | CODING_MODE_LAST_BLOCK);
  /* We suppress producing escape sequences for composition.  */
  coding.common_flags &= ~CODING_ANNOTATION_MASK;
  coding.dst_bytes = XSTRING (string)->size * 2;
  coding.destination = (unsigned char *) xmalloc (coding.dst_bytes);
  encode_coding_object (&coding, string, 0, 0,
			XSTRING (string)->size,
			STRING_BYTES (XSTRING (string)), Qnil);
  *text_bytes = coding.produced;
  *stringp = (result == 1 || !EQ (coding_system, Qcompound_text));
  return coding.destination;
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
    {
      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (!strcmp (FRAME_X_DISPLAY_INFO (f)->x_id_name,
		   XSTRING (f->name)->data))
	return;
      name = build_string (FRAME_X_DISPLAY_INFO (f)->x_id_name);
    }
  else
    CHECK_STRING (name);

  /* Don't change the name if it's already NAME.  */
  if (! NILP (Fstring_equal (name, f->name)))
    return;

  f->name = name;

  /* For setting the frame title, the title parameter should override
     the name parameter.  */
  if (! NILP (f->title))
    name = f->title;

  if (FRAME_X_WINDOW (f))
    {
      BLOCK_INPUT;
#ifdef HAVE_X11R4
      {
	XTextProperty text, icon;
	int bytes, stringp;
	Lisp_Object coding_system;

	coding_system = Vlocale_coding_system;
	if (NILP (coding_system))
	  coding_system = Qcompound_text;
	text.value = x_encode_text (name, coding_system, 0, &bytes, &stringp);
	text.encoding = (stringp ? XA_STRING
			 : FRAME_X_DISPLAY_INFO (f)->Xatom_COMPOUND_TEXT);
	text.format = 8;
	text.nitems = bytes;

	if (NILP (f->icon_name))
	  {
	    icon = text;
	  }
	else
	  {
	    icon.value = x_encode_text (f->icon_name, coding_system, 0,
					&bytes, &stringp);
	    icon.encoding = (stringp ? XA_STRING
			     : FRAME_X_DISPLAY_INFO (f)->Xatom_COMPOUND_TEXT);
	    icon.format = 8;
	    icon.nitems = bytes;
	  }
#ifdef USE_X_TOOLKIT
	XSetWMName (FRAME_X_DISPLAY (f),
		    XtWindow (f->output_data.x->widget), &text);
	XSetWMIconName (FRAME_X_DISPLAY (f), XtWindow (f->output_data.x->widget),
			&icon);
#else /* not USE_X_TOOLKIT */
	XSetWMName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &text);
	XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &icon);
#endif /* not USE_X_TOOLKIT */
	if (!NILP (f->icon_name)
	    && icon.value != XSTRING (f->icon_name)->data)
	  xfree (icon.value);
	if (text.value != XSTRING (name)->data)
	  xfree (text.value);
      }
#else /* not HAVE_X11R4 */
      XSetIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    XSTRING (name)->data);
      XStoreName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  XSTRING (name)->data);
#endif /* not HAVE_X11R4 */
      UNBLOCK_INPUT;
    }
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

/* Change the title of frame F to NAME.
   If NAME is nil, use the frame name as the title.

   If EXPLICIT is non-zero, that indicates that lisp code is setting the
       name; if NAME is a string, set F's name to NAME and set
       F->explicit_name; if NAME is Qnil, then clear F->explicit_name.

   If EXPLICIT is zero, that indicates that Emacs redisplay code is
       suggesting a new name, which lisp code should override; if
       F->explicit_name is set, ignore the new name; otherwise, set it.  */

void
x_set_title (f, name, old_name)
     struct frame *f;
     Lisp_Object name, old_name;
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 1;

  f->title = name;

  if (NILP (name))
    name = f->name;
  else
    CHECK_STRING (name);

  if (FRAME_X_WINDOW (f))
    {
      BLOCK_INPUT;
#ifdef HAVE_X11R4
      {
	XTextProperty text, icon;
	int bytes, stringp;
	Lisp_Object coding_system;

	coding_system = Vlocale_coding_system;
	if (NILP (coding_system))
	  coding_system = Qcompound_text;
	text.value = x_encode_text (name, coding_system, 0, &bytes, &stringp);
	text.encoding = (stringp ? XA_STRING
			 : FRAME_X_DISPLAY_INFO (f)->Xatom_COMPOUND_TEXT);
	text.format = 8;
	text.nitems = bytes;

	if (NILP (f->icon_name))
	  {
	    icon = text;
	  }
	else
	  {
	    icon.value = x_encode_text (f->icon_name, coding_system, 0,
					&bytes, &stringp);
	    icon.encoding = (stringp ? XA_STRING
			     : FRAME_X_DISPLAY_INFO (f)->Xatom_COMPOUND_TEXT);
	    icon.format = 8;
	    icon.nitems = bytes;
	  }
#ifdef USE_X_TOOLKIT
	XSetWMName (FRAME_X_DISPLAY (f),
		    XtWindow (f->output_data.x->widget), &text);
	XSetWMIconName (FRAME_X_DISPLAY (f), XtWindow (f->output_data.x->widget),
			&icon);
#else /* not USE_X_TOOLKIT */
	XSetWMName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &text);
	XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &icon);
#endif /* not USE_X_TOOLKIT */
	if (!NILP (f->icon_name)
	    && icon.value != XSTRING (f->icon_name)->data)
	  xfree (icon.value);
	if (text.value != XSTRING (name)->data)
	  xfree (text.value);
      }
#else /* not HAVE_X11R4 */
      XSetIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    XSTRING (name)->data);
      XStoreName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  XSTRING (name)->data);
#endif /* not HAVE_X11R4 */
      UNBLOCK_INPUT;
    }
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
x_set_unsplittable (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->no_split = !NILP (arg);
}

void
x_set_vertical_scroll_bars (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  if ((EQ (arg, Qleft) && FRAME_HAS_VERTICAL_SCROLL_BARS_ON_RIGHT (f))
      || (EQ (arg, Qright) && FRAME_HAS_VERTICAL_SCROLL_BARS_ON_LEFT (f))
      || (NILP (arg) && FRAME_HAS_VERTICAL_SCROLL_BARS (f))
      || (!NILP (arg) && ! FRAME_HAS_VERTICAL_SCROLL_BARS (f)))
    {
      FRAME_VERTICAL_SCROLL_BAR_TYPE (f)
	= (NILP (arg)
	   ? vertical_scroll_bar_none
	   : EQ (Qright, arg)
	   ? vertical_scroll_bar_right 
	   : vertical_scroll_bar_left);

      /* We set this parameter before creating the X window for the
	 frame, so we can get the geometry right from the start.
	 However, if the window hasn't been created yet, we shouldn't
	 call x_set_window_size.  */
      if (FRAME_X_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
      do_pending_window_change (0);
    }
}

void
x_set_scroll_bar_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int wid = FONT_WIDTH (f->output_data.x->font);

  if (NILP (arg))
    {
#ifdef USE_TOOLKIT_SCROLL_BARS
      /* A minimum width of 14 doesn't look good for toolkit scroll bars.  */
      int width = 16 + 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM;
      FRAME_SCROLL_BAR_COLS (f) = (width + wid - 1) / wid;
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = width;
#else
      /* Make the actual width at least 14 pixels and a multiple of a
	 character width.  */
      FRAME_SCROLL_BAR_COLS (f) = (14 + wid - 1) / wid;
      
      /* Use all of that space (aside from required margins) for the
	 scroll bar.  */
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = 0;
#endif

      if (FRAME_X_WINDOW (f))
        x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
      do_pending_window_change (0);
    }
  else if (INTEGERP (arg) && XINT (arg) > 0
	   && XFASTINT (arg) != FRAME_SCROLL_BAR_PIXEL_WIDTH (f))
    {
      if (XFASTINT (arg) <= 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM)
	XSETINT (arg, 2 * VERTICAL_SCROLL_BAR_WIDTH_TRIM + 1);

      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = XFASTINT (arg);
      FRAME_SCROLL_BAR_COLS (f) = (XFASTINT (arg) + wid-1) / wid;
      if (FRAME_X_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
    }

  change_frame_size (f, 0, FRAME_WIDTH (f), 0, 0, 0);
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.hpos = 0;
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.x = 0;
}



/* Subroutines of creating an X frame.  */

/* Make sure that Vx_resource_name is set to a reasonable value.
   Fix it up, or set it to `emacs' if it is too hopeless.  */

static void
validate_x_resource_name ()
{
  int len = 0;
  /* Number of valid characters in the resource name.  */
  int good_count = 0;
  /* Number of invalid characters in the resource name.  */
  int bad_count = 0;
  Lisp_Object new;
  int i;

  if (!STRINGP (Vx_resource_class))
    Vx_resource_class = build_string (EMACS_CLASS);

  if (STRINGP (Vx_resource_name))
    {
      unsigned char *p = XSTRING (Vx_resource_name)->data;
      int i;

      len = STRING_BYTES (XSTRING (Vx_resource_name));

      /* Only letters, digits, - and _ are valid in resource names.
	 Count the valid characters and count the invalid ones.  */
      for (i = 0; i < len; i++)
	{
	  int c = p[i];
	  if (! ((c >= 'a' && c <= 'z')
		 || (c >= 'A' && c <= 'Z')
		 || (c >= '0' && c <= '9')
		 || c == '-' || c == '_'))
	    bad_count++;
	  else
	    good_count++;
	}
    }
  else
    /* Not a string => completely invalid.  */
    bad_count = 5, good_count = 0;

  /* If name is valid already, return.  */
  if (bad_count == 0)
    return;

  /* If name is entirely invalid, or nearly so, use `emacs'.  */
  if (good_count == 0
      || (good_count == 1 && bad_count > 0))
    {
      Vx_resource_name = build_string ("emacs");
      return;
    }

  /* Name is partly valid.  Copy it and replace the invalid characters
     with underscores.  */

  Vx_resource_name = new = Fcopy_sequence (Vx_resource_name);

  for (i = 0; i < len; i++)
    {
      int c = XSTRING (new)->data[i];
      if (! ((c >= 'a' && c <= 'z')
	     || (c >= 'A' && c <= 'Z')
	     || (c >= '0' && c <= '9')
	     || c == '-' || c == '_'))
	XSTRING (new)->data[i] = '_';
    }
}


extern char *x_get_string_resource ();

DEFUN ("x-get-resource", Fx_get_resource, Sx_get_resource, 2, 4, 0,
       doc: /* Return the value of ATTRIBUTE, of class CLASS, from the X defaults database.
This uses `INSTANCE.ATTRIBUTE' as the key and `Emacs.CLASS' as the
class, where INSTANCE is the name under which Emacs was invoked, or
the name specified by the `-name' or `-rn' command-line arguments.

The optional arguments COMPONENT and SUBCLASS add to the key and the
class, respectively.  You must specify both of them or neither.
If you specify them, the key is `INSTANCE.COMPONENT.ATTRIBUTE'
and the class is `Emacs.CLASS.SUBCLASS'.  */)
     (attribute, class, component, subclass)
     Lisp_Object attribute, class, component, subclass;
{
  register char *value;
  char *name_key;
  char *class_key;

  check_x ();

  CHECK_STRING (attribute);
  CHECK_STRING (class);

  if (!NILP (component))
    CHECK_STRING (component);
  if (!NILP (subclass))
    CHECK_STRING (subclass);
  if (NILP (component) != NILP (subclass))
    error ("x-get-resource: must specify both COMPONENT and SUBCLASS or neither");

  validate_x_resource_name ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  Make them big enough for the worst case.  */
  name_key = (char *) alloca (STRING_BYTES (XSTRING (Vx_resource_name))
			      + (STRINGP (component)
				 ? STRING_BYTES (XSTRING (component)) : 0)
			      + STRING_BYTES (XSTRING (attribute))
			      + 3);

  class_key = (char *) alloca (STRING_BYTES (XSTRING (Vx_resource_class))
			       + STRING_BYTES (XSTRING (class))
			       + (STRINGP (subclass)
				  ? STRING_BYTES (XSTRING (subclass)) : 0)
			       + 3);

  /* Start with emacs.FRAMENAME for the name (the specific one)
     and with `Emacs' for the class key (the general one).  */
  strcpy (name_key, XSTRING (Vx_resource_name)->data);
  strcpy (class_key, XSTRING (Vx_resource_class)->data);

  strcat (class_key, ".");
  strcat (class_key, XSTRING (class)->data);

  if (!NILP (component))
    {
      strcat (class_key, ".");
      strcat (class_key, XSTRING (subclass)->data);

      strcat (name_key, ".");
      strcat (name_key, XSTRING (component)->data);
    }

  strcat (name_key, ".");
  strcat (name_key, XSTRING (attribute)->data);

  value = x_get_string_resource (check_x_display_info (Qnil)->xrdb,
				 name_key, class_key);

  if (value != (char *) 0)
    return build_string (value);
  else
    return Qnil;
}

/* Get an X resource, like Fx_get_resource, but for display DPYINFO.  */

Lisp_Object
display_x_get_resource (dpyinfo, attribute, class, component, subclass)
     struct x_display_info *dpyinfo;
     Lisp_Object attribute, class, component, subclass;
{
  register char *value;
  char *name_key;
  char *class_key;

  CHECK_STRING (attribute);
  CHECK_STRING (class);

  if (!NILP (component))
    CHECK_STRING (component);
  if (!NILP (subclass))
    CHECK_STRING (subclass);
  if (NILP (component) != NILP (subclass))
    error ("x-get-resource: must specify both COMPONENT and SUBCLASS or neither");

  validate_x_resource_name ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  Make them big enough for the worst case.  */
  name_key = (char *) alloca (STRING_BYTES (XSTRING (Vx_resource_name))
			      + (STRINGP (component)
				 ? STRING_BYTES (XSTRING (component)) : 0)
			      + STRING_BYTES (XSTRING (attribute))
			      + 3);

  class_key = (char *) alloca (STRING_BYTES (XSTRING (Vx_resource_class))
			       + STRING_BYTES (XSTRING (class))
			       + (STRINGP (subclass)
				  ? STRING_BYTES (XSTRING (subclass)) : 0)
			       + 3);

  /* Start with emacs.FRAMENAME for the name (the specific one)
     and with `Emacs' for the class key (the general one).  */
  strcpy (name_key, XSTRING (Vx_resource_name)->data);
  strcpy (class_key, XSTRING (Vx_resource_class)->data);

  strcat (class_key, ".");
  strcat (class_key, XSTRING (class)->data);

  if (!NILP (component))
    {
      strcat (class_key, ".");
      strcat (class_key, XSTRING (subclass)->data);

      strcat (name_key, ".");
      strcat (name_key, XSTRING (component)->data);
    }

  strcat (name_key, ".");
  strcat (name_key, XSTRING (attribute)->data);

  value = x_get_string_resource (dpyinfo->xrdb, name_key, class_key);

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
  char *name_key;
  char *class_key;
  struct frame *sf = SELECTED_FRAME ();

  /* Allocate space for the components, the dots which separate them,
     and the final '\0'.  */
  name_key = (char *) alloca (STRING_BYTES (XSTRING (Vinvocation_name))
			      + strlen (attribute) + 2);
  class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
			       + strlen (class) + 2);

  sprintf (name_key, "%s.%s",
	   XSTRING (Vinvocation_name)->data,
	   attribute);
  sprintf (class_key, "%s.%s", EMACS_CLASS, class);

  return x_get_string_resource (FRAME_X_DISPLAY_INFO (sf)->xrdb,
				name_key, class_key);
}

/* Types we might convert a resource string into.  */
enum resource_types
{
  RES_TYPE_NUMBER,
  RES_TYPE_FLOAT,
  RES_TYPE_BOOLEAN,
  RES_TYPE_STRING,
  RES_TYPE_SYMBOL
};

/* Return the value of parameter PARAM.

   First search ALIST, then Vdefault_frame_alist, then the X defaults
   database, using ATTRIBUTE as the attribute name and CLASS as its class.

   Convert the resource to the type specified by desired_type.

   If no default is specified, return Qunbound.  If you call
   x_get_arg, make sure you deal with Qunbound in a reasonable way,
   and don't let it get stored in any Lisp-visible variables!  */

static Lisp_Object
x_get_arg (dpyinfo, alist, param, attribute, class, type)
     struct x_display_info *dpyinfo;
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
	  tem = display_x_get_resource (dpyinfo,
					build_string (attribute),
					build_string (class),
					Qnil, Qnil);

	  if (NILP (tem))
	    return Qunbound;

	  switch (type)
	    {
	    case RES_TYPE_NUMBER:
	      return make_number (atoi (XSTRING (tem)->data));

	    case RES_TYPE_FLOAT:
	      return make_float (atof (XSTRING (tem)->data));

	    case RES_TYPE_BOOLEAN:
	      tem = Fdowncase (tem);
	      if (!strcmp (XSTRING (tem)->data, "on")
		  || !strcmp (XSTRING (tem)->data, "true"))
		return Qt;
	      else 
		return Qnil;

	    case RES_TYPE_STRING:
	      return tem;

	    case RES_TYPE_SYMBOL:
	      /* As a special case, we map the values `true' and `on'
		 to Qt, and `false' and `off' to Qnil.  */
	      {
		Lisp_Object lower;
		lower = Fdowncase (tem);
		if (!strcmp (XSTRING (lower)->data, "on")
		    || !strcmp (XSTRING (lower)->data, "true"))
		  return Qt;
		else if (!strcmp (XSTRING (lower)->data, "off")
		      || !strcmp (XSTRING (lower)->data, "false"))
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

/* Like x_get_arg, but also record the value in f->param_alist.  */

static Lisp_Object
x_get_and_record_arg (f, alist, param, attribute, class, type)
     struct frame *f;
     Lisp_Object alist, param;
     char *attribute;
     char *class;
     enum resource_types type;
{
  Lisp_Object value;

  value = x_get_arg (FRAME_X_DISPLAY_INFO (f), alist, param,
		     attribute, class, type);
  if (! NILP (value))
    store_frame_param (f, param, value);

  return value;
}

/* Record in frame F the specified or default value according to ALIST
   of the parameter named PROP (a Lisp symbol).
   If no value is specified for PROP, look for an X default for XPROP
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

  tem = x_get_arg (FRAME_X_DISPLAY_INFO (f), alist, prop, xprop, xclass, type);
  if (EQ (tem, Qunbound))
    tem = deflt;
  x_set_frame_parameters (f, Fcons (Fcons (prop, tem), Qnil));
  return tem;
}


/* Record in frame F the specified or default value according to ALIST
   of the parameter named PROP (a Lisp symbol).  If no value is
   specified for PROP, look for an X default for XPROP on the frame
   named NAME.  If that is not found either, use the value DEFLT.  */

static Lisp_Object
x_default_scroll_bar_color_parameter (f, alist, prop, xprop, xclass,
				      foreground_p)
     struct frame *f;
     Lisp_Object alist;
     Lisp_Object prop;
     char *xprop;
     char *xclass;
     int foreground_p;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
  Lisp_Object tem;

  tem = x_get_arg (dpyinfo, alist, prop, xprop, xclass, RES_TYPE_STRING);
  if (EQ (tem, Qunbound))
    {
#ifdef USE_TOOLKIT_SCROLL_BARS

      /* See if an X resource for the scroll bar color has been
	 specified.  */
      tem = display_x_get_resource (dpyinfo,
				    build_string (foreground_p
						  ? "foreground"
						  : "background"),
				    empty_string,
				    build_string ("verticalScrollBar"),
				    empty_string);
      if (!STRINGP (tem))
	{
	  /* If nothing has been specified, scroll bars will use a
	     toolkit-dependent default.  Because these defaults are
	     difficult to get at without actually creating a scroll
	     bar, use nil to indicate that no color has been
	     specified.  */
	  tem = Qnil;
	}
      
#else /* not USE_TOOLKIT_SCROLL_BARS */
      
      tem = Qnil;
      
#endif /* not USE_TOOLKIT_SCROLL_BARS */
    }

  x_set_frame_parameters (f, Fcons (Fcons (prop, tem), Qnil));
  return tem;
}



DEFUN ("x-parse-geometry", Fx_parse_geometry, Sx_parse_geometry, 1, 1, 0,
       doc: /* Parse an X-style geometry string STRING.
Returns an alist of the form ((top . TOP), (left . LEFT) ... ).
The properties returned may include `top', `left', `height', and `width'.
The value of `left' or `top' may be an integer,
or a list (+ N) meaning N pixels relative to top/left corner,
or a list (- N) meaning -N pixels relative to bottom/right corner.  */)
     (string)
     Lisp_Object string;
{
  int geometry, x, y;
  unsigned int width, height;
  Lisp_Object result;

  CHECK_STRING (string);

  geometry = XParseGeometry ((char *) XSTRING (string)->data,
			     &x, &y, &width, &height);

#if 0
  if (!!(geometry & XValue) != !!(geometry & YValue))
    error ("Must specify both x and y position, or neither");
#endif

  result = Qnil;
  if (geometry & XValue)
    {
      Lisp_Object element;

      if (x >= 0 && (geometry & XNegative))
	element = Fcons (Qleft, Fcons (Qminus, Fcons (make_number (-x), Qnil)));
      else if (x < 0 && ! (geometry & XNegative))
	element = Fcons (Qleft, Fcons (Qplus, Fcons (make_number (x), Qnil)));
      else
	element = Fcons (Qleft, make_number (x));
      result = Fcons (element, result);
    }

  if (geometry & YValue)
    {
      Lisp_Object element;

      if (y >= 0 && (geometry & YNegative))
	element = Fcons (Qtop, Fcons (Qminus, Fcons (make_number (-y), Qnil)));
      else if (y < 0 && ! (geometry & YNegative))
	element = Fcons (Qtop, Fcons (Qplus, Fcons (make_number (y), Qnil)));
      else
	element = Fcons (Qtop, make_number (y));
      result = Fcons (element, result);
    }

  if (geometry & WidthValue)
    result = Fcons (Fcons (Qwidth, make_number (width)), result);
  if (geometry & HeightValue)
    result = Fcons (Fcons (Qheight, make_number (height)), result);

  return result;
}

/* Calculate the desired size and position of this window,
   and return the flags saying which aspects were specified.

   This function does not make the coordinates positive.  */

#define DEFAULT_ROWS 40
#define DEFAULT_COLS 80

static int
x_figure_window_size (f, parms)
     struct frame *f;
     Lisp_Object parms;
{
  register Lisp_Object tem0, tem1, tem2;
  long window_prompting = 0;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  /* Default values if we fall through.
     Actually, if that happens we should get
     window manager prompting.  */
  SET_FRAME_WIDTH (f, DEFAULT_COLS);
  f->height = DEFAULT_ROWS;
  /* Window managers expect that if program-specified
     positions are not (0,0), they're intentional, not defaults.  */
  f->output_data.x->top_pos = 0;
  f->output_data.x->left_pos = 0;

  tem0 = x_get_arg (dpyinfo, parms, Qheight, 0, 0, RES_TYPE_NUMBER);
  tem1 = x_get_arg (dpyinfo, parms, Qwidth, 0, 0, RES_TYPE_NUMBER);
  tem2 = x_get_arg (dpyinfo, parms, Quser_size, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (!EQ (tem0, Qunbound))
	{
	  CHECK_NUMBER (tem0);
	  f->height = XINT (tem0);
	}
      if (!EQ (tem1, Qunbound))
	{
	  CHECK_NUMBER (tem1);
	  SET_FRAME_WIDTH (f, XINT (tem1));
	}
      if (!NILP (tem2) && !EQ (tem2, Qunbound))
	window_prompting |= USSize;
      else
	window_prompting |= PSize;
    }

  f->output_data.x->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.x->font)));

  x_compute_fringe_widths (f, 0);

  f->output_data.x->pixel_width = CHAR_TO_PIXEL_WIDTH (f, f->width);
  f->output_data.x->pixel_height = CHAR_TO_PIXEL_HEIGHT (f, f->height);

  tem0 = x_get_arg (dpyinfo, parms, Qtop, 0, 0, RES_TYPE_NUMBER);
  tem1 = x_get_arg (dpyinfo, parms, Qleft, 0, 0, RES_TYPE_NUMBER);
  tem2 = x_get_arg (dpyinfo, parms, Quser_position, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (EQ (tem0, Qminus))
	{
	  f->output_data.x->top_pos = 0;
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qminus)
	       && CONSP (XCDR (tem0))
	       && INTEGERP (XCAR (XCDR (tem0))))
	{
	  f->output_data.x->top_pos = - XINT (XCAR (XCDR (tem0)));
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qplus)
	       && CONSP (XCDR (tem0))
	       && INTEGERP (XCAR (XCDR (tem0))))
	{
	  f->output_data.x->top_pos = XINT (XCAR (XCDR (tem0)));
	}
      else if (EQ (tem0, Qunbound))
	f->output_data.x->top_pos = 0;
      else
	{
	  CHECK_NUMBER (tem0);
	  f->output_data.x->top_pos = XINT (tem0);
	  if (f->output_data.x->top_pos < 0)
	    window_prompting |= YNegative;
	}

      if (EQ (tem1, Qminus))
	{
	  f->output_data.x->left_pos = 0;
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qminus)
	       && CONSP (XCDR (tem1))
	       && INTEGERP (XCAR (XCDR (tem1))))
	{
	  f->output_data.x->left_pos = - XINT (XCAR (XCDR (tem1)));
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qplus)
	       && CONSP (XCDR (tem1))
	       && INTEGERP (XCAR (XCDR (tem1))))
	{
	  f->output_data.x->left_pos = XINT (XCAR (XCDR (tem1)));
	}
      else if (EQ (tem1, Qunbound))
	f->output_data.x->left_pos = 0;
      else
	{
	  CHECK_NUMBER (tem1);
	  f->output_data.x->left_pos = XINT (tem1);
	  if (f->output_data.x->left_pos < 0)
	    window_prompting |= XNegative;
	}

      if (!NILP (tem2) && ! EQ (tem2, Qunbound))
	window_prompting |= USPosition;
      else
	window_prompting |= PPosition;
    }

  if (f->output_data.x->want_fullscreen != FULLSCREEN_NONE)
    {
      int left, top;
      int width, height;
      
      /* It takes both for some WM:s to place it where we want */
      window_prompting = USPosition | PPosition;
      x_fullscreen_adjust (f, &width, &height, &top, &left);
      f->width = width;
      f->height = height;
      f->output_data.x->pixel_width = CHAR_TO_PIXEL_WIDTH (f, f->width);
      f->output_data.x->pixel_height = CHAR_TO_PIXEL_HEIGHT (f, f->height);
      f->output_data.x->left_pos = left;
      f->output_data.x->top_pos = top;
    }
  
  return window_prompting;
}

#if !defined (HAVE_X11R4) && !defined (HAVE_XSETWMPROTOCOLS)

Status
XSetWMProtocols (dpy, w, protocols, count)
     Display *dpy;
     Window w;
     Atom *protocols;
     int count;
{
  Atom prop;
  prop = XInternAtom (dpy, "WM_PROTOCOLS", False);
  if (prop == None) return False;
  XChangeProperty (dpy, w, prop, XA_ATOM, 32, PropModeReplace,
		   (unsigned char *) protocols, count);
  return True;
}
#endif /* not HAVE_X11R4 && not HAVE_XSETWMPROTOCOLS */

#ifdef USE_X_TOOLKIT

/* If the WM_PROTOCOLS property does not already contain WM_TAKE_FOCUS,
   WM_DELETE_WINDOW, and WM_SAVE_YOURSELF, then add them.  (They may
   already be present because of the toolkit (Motif adds some of them,
   for example, but Xt doesn't).  */

static void
hack_wm_protocols (f, widget)
     FRAME_PTR f;
     Widget widget;
{
  Display *dpy = XtDisplay (widget);
  Window w = XtWindow (widget);
  int need_delete = 1;
  int need_focus = 1;
  int need_save = 1;

  BLOCK_INPUT;
  {
    Atom type, *atoms = 0;
    int format = 0;
    unsigned long nitems = 0;
    unsigned long bytes_after;

    if ((XGetWindowProperty (dpy, w,
			     FRAME_X_DISPLAY_INFO (f)->Xatom_wm_protocols,
			     (long)0, (long)100, False, XA_ATOM,
			     &type, &format, &nitems, &bytes_after,
			     (unsigned char **) &atoms)
	 == Success)
	&& format == 32 && type == XA_ATOM)
      while (nitems > 0)
	{
	  nitems--;
 	  if (atoms[nitems] == FRAME_X_DISPLAY_INFO (f)->Xatom_wm_delete_window)
	    need_delete = 0;
	  else if (atoms[nitems] == FRAME_X_DISPLAY_INFO (f)->Xatom_wm_take_focus)
	    need_focus = 0;
	  else if (atoms[nitems] == FRAME_X_DISPLAY_INFO (f)->Xatom_wm_save_yourself)
	    need_save = 0;
	}
    if (atoms) XFree ((char *) atoms);
  }
  {
    Atom props [10];
    int count = 0;
    if (need_delete)
      props[count++] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_delete_window;
    if (need_focus)
      props[count++] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_take_focus;
    if (need_save)
      props[count++] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_save_yourself;
    if (count)
      XChangeProperty (dpy, w, FRAME_X_DISPLAY_INFO (f)->Xatom_wm_protocols,
		       XA_ATOM, 32, PropModeAppend,
		       (unsigned char *) props, count);
  }
  UNBLOCK_INPUT;
}
#endif



/* Support routines for XIC (X Input Context).  */

#ifdef HAVE_X_I18N

static XFontSet xic_create_xfontset P_ ((struct frame *, char *));
static XIMStyle best_xim_style P_ ((XIMStyles *, XIMStyles *));


/* Supported XIM styles, ordered by preferenc.  */

static XIMStyle supported_xim_styles[] =
{
  XIMPreeditPosition | XIMStatusArea,
  XIMPreeditPosition | XIMStatusNothing,
  XIMPreeditPosition | XIMStatusNone,
  XIMPreeditNothing | XIMStatusArea,
  XIMPreeditNothing | XIMStatusNothing,
  XIMPreeditNothing | XIMStatusNone,
  XIMPreeditNone | XIMStatusArea,
  XIMPreeditNone | XIMStatusNothing,
  XIMPreeditNone | XIMStatusNone,
  0,
};


/* Create an X fontset on frame F with base font name
   BASE_FONTNAME.. */

static XFontSet
xic_create_xfontset (f, base_fontname)
     struct frame *f;
     char *base_fontname;
{
  XFontSet xfs;
  char **missing_list;
  int missing_count;
  char *def_string;
  
  xfs = XCreateFontSet (FRAME_X_DISPLAY (f),
			base_fontname, &missing_list,
			&missing_count, &def_string);
  if (missing_list)
    XFreeStringList (missing_list);
  
  /* No need to free def_string. */
  return xfs;
}


/* Value is the best input style, given user preferences USER (already
   checked to be supported by Emacs), and styles supported by the
   input method XIM.  */

static XIMStyle
best_xim_style (user, xim)
     XIMStyles *user;
     XIMStyles *xim;
{
  int i, j;

  for (i = 0; i < user->count_styles; ++i)
    for (j = 0; j < xim->count_styles; ++j)
      if (user->supported_styles[i] == xim->supported_styles[j])
	return user->supported_styles[i];

  /* Return the default style.  */
  return XIMPreeditNothing | XIMStatusNothing;
}

/* Create XIC for frame F. */

static XIMStyle xic_style;

void
create_frame_xic (f)
     struct frame *f;
{
  XIM xim;
  XIC xic = NULL;
  XFontSet xfs = NULL;

  if (FRAME_XIC (f))
    return;
  
  xim = FRAME_X_XIM (f);
  if (xim)
    {
      XRectangle s_area;
      XPoint spot;
      XVaNestedList preedit_attr;
      XVaNestedList status_attr;
      char *base_fontname;
      int fontset;

      s_area.x = 0; s_area.y = 0; s_area.width = 1; s_area.height = 1;
      spot.x = 0; spot.y = 1;
      /* Create X fontset. */
      fontset = FRAME_FONTSET (f);
      if (fontset < 0)
	base_fontname = "-*-*-*-r-normal--14-*-*-*-*-*-*-*";
      else
	{
	  /* Determine the base fontname from the ASCII font name of
	     FONTSET.  */
	  char *ascii_font = (char *) XSTRING (fontset_ascii (fontset))->data;
	  char *p = ascii_font;
	  int i;

	  for (i = 0; *p; p++)
	    if (*p == '-') i++;
	  if (i != 14)
	    /* As the font name doesn't conform to XLFD, we can't
	       modify it to get a suitable base fontname for the
	       frame.  */
	    base_fontname = "-*-*-*-r-normal--14-*-*-*-*-*-*-*";
	  else
	    {
	      int len = strlen (ascii_font) + 1;
	      char *p1 = NULL;

	      for (i = 0, p = ascii_font; i < 8; p++)
		{
		  if (*p == '-')
		    {
		      i++;
		      if (i == 3)
			p1 = p + 1;
		    }
		}
	      base_fontname = (char *) alloca (len);
	      bzero (base_fontname, len);
	      strcpy (base_fontname, "-*-*-");
	      bcopy (p1, base_fontname + 5, p - p1);
	      strcat (base_fontname, "*-*-*-*-*-*-*");
	    }
	}
      xfs = xic_create_xfontset (f, base_fontname);

      /* Determine XIC style.  */
      if (xic_style == 0)
	{
	  XIMStyles supported_list;
	  supported_list.count_styles = (sizeof supported_xim_styles
					 / sizeof supported_xim_styles[0]);
	  supported_list.supported_styles = supported_xim_styles;
	  xic_style = best_xim_style (&supported_list,
				      FRAME_X_XIM_STYLES (f));
	}

      preedit_attr = XVaCreateNestedList (0,
					  XNFontSet, xfs,
					  XNForeground,
					  FRAME_FOREGROUND_PIXEL (f),
					  XNBackground,
					  FRAME_BACKGROUND_PIXEL (f),
					  (xic_style & XIMPreeditPosition
					   ? XNSpotLocation
					   : NULL),
					  &spot,
					  NULL);
      status_attr = XVaCreateNestedList (0,
					 XNArea,
					 &s_area,
					 XNFontSet,
					 xfs,
					 XNForeground,
					 FRAME_FOREGROUND_PIXEL (f),
					 XNBackground,
					 FRAME_BACKGROUND_PIXEL (f),
					 NULL);

      xic = XCreateIC (xim,
		       XNInputStyle, xic_style,
		       XNClientWindow, FRAME_X_WINDOW(f),
		       XNFocusWindow, FRAME_X_WINDOW(f),
		       XNStatusAttributes, status_attr,
		       XNPreeditAttributes, preedit_attr,
		       NULL);
      XFree (preedit_attr);
      XFree (status_attr);
    }
  
  FRAME_XIC (f) = xic;
  FRAME_XIC_STYLE (f) = xic_style;
  FRAME_XIC_FONTSET (f) = xfs;
}


/* Destroy XIC and free XIC fontset of frame F, if any. */

void
free_frame_xic (f)
     struct frame *f;
{
  if (FRAME_XIC (f) == NULL)
    return;
  
  XDestroyIC (FRAME_XIC (f));
  if (FRAME_XIC_FONTSET (f))
    XFreeFontSet (FRAME_X_DISPLAY (f), FRAME_XIC_FONTSET (f));

  FRAME_XIC (f) = NULL;
  FRAME_XIC_FONTSET (f) = NULL;
}


/* Place preedit area for XIC of window W's frame to specified
   pixel position X/Y.  X and Y are relative to window W.  */

void
xic_set_preeditarea (w, x, y)
     struct window *w;
     int x, y;
{
  struct frame *f = XFRAME (w->frame);
  XVaNestedList attr;
  XPoint spot;
      
  spot.x = WINDOW_TO_FRAME_PIXEL_X (w, x);
  spot.y = WINDOW_TO_FRAME_PIXEL_Y (w, y) + FONT_BASE (FRAME_FONT (f));
  attr = XVaCreateNestedList (0, XNSpotLocation, &spot, NULL);
  XSetICValues (FRAME_XIC (f), XNPreeditAttributes, attr, NULL);
  XFree (attr);
}


/* Place status area for XIC in bottom right corner of frame F.. */

void
xic_set_statusarea (f)
     struct frame *f;
{
  XIC xic = FRAME_XIC (f);
  XVaNestedList attr;
  XRectangle area;
  XRectangle *needed;

  /* Negotiate geometry of status area.  If input method has existing
     status area, use its current size.  */
  area.x = area.y = area.width = area.height = 0;
  attr = XVaCreateNestedList (0, XNAreaNeeded, &area, NULL);
  XSetICValues (xic, XNStatusAttributes, attr, NULL);
  XFree (attr);
  
  attr = XVaCreateNestedList (0, XNAreaNeeded, &needed, NULL);
  XGetICValues (xic, XNStatusAttributes, attr, NULL);
  XFree (attr);

  if (needed->width == 0) /* Use XNArea instead of XNAreaNeeded */
    {
      attr = XVaCreateNestedList (0, XNArea, &needed, NULL);
      XGetICValues (xic, XNStatusAttributes, attr, NULL);
      XFree (attr);
    }

  area.width  = needed->width;
  area.height = needed->height;
  area.x = PIXEL_WIDTH (f) - area.width - FRAME_INTERNAL_BORDER_WIDTH (f);
  area.y = (PIXEL_HEIGHT (f) - area.height
	    - FRAME_MENUBAR_HEIGHT (f) - FRAME_INTERNAL_BORDER_WIDTH (f));
  XFree (needed);

  attr = XVaCreateNestedList (0, XNArea, &area, NULL);
  XSetICValues(xic, XNStatusAttributes, attr, NULL);
  XFree (attr);
}


/* Set X fontset for XIC of frame F, using base font name
   BASE_FONTNAME.  Called when a new Emacs fontset is chosen.  */

void
xic_set_xfontset (f, base_fontname)
     struct frame *f;
     char *base_fontname;
{
  XVaNestedList attr;
  XFontSet xfs;

  xfs = xic_create_xfontset (f, base_fontname);

  attr = XVaCreateNestedList (0, XNFontSet, xfs, NULL);
  if (FRAME_XIC_STYLE (f) & XIMPreeditPosition)
    XSetICValues (FRAME_XIC (f), XNPreeditAttributes, attr, NULL);
  if (FRAME_XIC_STYLE (f) & XIMStatusArea)
    XSetICValues (FRAME_XIC (f), XNStatusAttributes, attr, NULL);
  XFree (attr);
  
  if (FRAME_XIC_FONTSET (f))
    XFreeFontSet (FRAME_X_DISPLAY (f), FRAME_XIC_FONTSET (f));
  FRAME_XIC_FONTSET (f) = xfs;
}

#endif /* HAVE_X_I18N */



#ifdef USE_X_TOOLKIT

/* Create and set up the X widget for frame F.  */

static void
x_window (f, window_prompting, minibuffer_only)
     struct frame *f;
     long window_prompting;
     int minibuffer_only;
{
  XClassHint class_hints;
  XSetWindowAttributes attributes;
  unsigned long attribute_mask;
  Widget shell_widget;
  Widget pane_widget;
  Widget frame_widget;
  Arg al [25];
  int ac;

  BLOCK_INPUT;

  /* Use the resource name as the top-level widget name
     for looking up resources.  Make a non-Lisp copy
     for the window manager, so GC relocation won't bother it.

     Elsewhere we specify the window name for the window manager.  */
     
  {
    char *str = (char *) XSTRING (Vx_resource_name)->data;
    f->namebuf = (char *) xmalloc (strlen (str) + 1);
    strcpy (f->namebuf, str);
  }

  ac = 0;
  XtSetArg (al[ac], XtNallowShellResize, 1); ac++;
  XtSetArg (al[ac], XtNinput, 1); ac++;
  XtSetArg (al[ac], XtNmappedWhenManaged, 0); ac++;
  XtSetArg (al[ac], XtNborderWidth, f->output_data.x->border_width); ac++;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_X_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  shell_widget = XtAppCreateShell (f->namebuf, EMACS_CLASS,
				   applicationShellWidgetClass,
				   FRAME_X_DISPLAY (f), al, ac);

  f->output_data.x->widget = shell_widget;
  /* maybe_set_screen_title_format (shell_widget); */

  pane_widget = lw_create_widget ("main", "pane", widget_id_tick++,
				  (widget_value *) NULL,
				  shell_widget, False,
				  (lw_callback) NULL,
				  (lw_callback) NULL,
				  (lw_callback) NULL,
				  (lw_callback) NULL);

  ac = 0;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_X_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  XtSetValues (pane_widget, al, ac);
  f->output_data.x->column_widget = pane_widget;

  /* mappedWhenManaged to false tells to the paned window to not map/unmap 
     the emacs screen when changing menubar.  This reduces flickering.  */

  ac = 0;
  XtSetArg (al[ac], XtNmappedWhenManaged, 0); ac++;
  XtSetArg (al[ac], XtNshowGrip, 0); ac++;
  XtSetArg (al[ac], XtNallowResize, 1); ac++;
  XtSetArg (al[ac], XtNresizeToPreferred, 1); ac++;
  XtSetArg (al[ac], XtNemacsFrame, f); ac++;
  XtSetArg (al[ac], XtNvisual, FRAME_X_VISUAL (f)); ac++;
  XtSetArg (al[ac], XtNdepth, FRAME_X_DISPLAY_INFO (f)->n_planes); ac++;
  XtSetArg (al[ac], XtNcolormap, FRAME_X_COLORMAP (f)); ac++;
  frame_widget = XtCreateWidget (f->namebuf, emacsFrameClass, pane_widget,
				 al, ac);
 
  f->output_data.x->edit_widget = frame_widget;
 
  XtManageChild (frame_widget); 

  /* Do some needed geometry management.  */
  {
    int len;
    char *tem, shell_position[32];
    Arg al[2];
    int ac = 0;
    int extra_borders = 0;
    int menubar_size 
      = (f->output_data.x->menubar_widget
	 ? (f->output_data.x->menubar_widget->core.height
	    + f->output_data.x->menubar_widget->core.border_width)
	 : 0);

#if 0 /* Experimentally, we now get the right results
	 for -geometry -0-0 without this.  24 Aug 96, rms.  */
    if (FRAME_EXTERNAL_MENU_BAR (f))
      {
        Dimension ibw = 0;
        XtVaGetValues (pane_widget, XtNinternalBorderWidth, &ibw, NULL);
        menubar_size += ibw;
      }
#endif

    f->output_data.x->menubar_height = menubar_size;

#ifndef USE_LUCID
    /* Motif seems to need this amount added to the sizes
       specified for the shell widget.  The Athena/Lucid widgets don't.
       Both conclusions reached experimentally.  -- rms.  */
    XtVaGetValues (f->output_data.x->edit_widget, XtNinternalBorderWidth,
		   &extra_borders, NULL);
    extra_borders *= 2;
#endif

    /* Convert our geometry parameters into a geometry string
       and specify it.
       Note that we do not specify here whether the position
       is a user-specified or program-specified one.
       We pass that information later, in x_wm_set_size_hints.  */
    {
      int left = f->output_data.x->left_pos;
      int xneg = window_prompting & XNegative;
      int top = f->output_data.x->top_pos;
      int yneg = window_prompting & YNegative;
      if (xneg)
	left = -left;
      if (yneg)
	top = -top;

      if (window_prompting & USPosition)
	sprintf (shell_position, "=%dx%d%c%d%c%d",
		 PIXEL_WIDTH (f) + extra_borders, 
		 PIXEL_HEIGHT (f) + menubar_size + extra_borders,
		 (xneg ? '-' : '+'), left,
		 (yneg ? '-' : '+'), top);
      else
	sprintf (shell_position, "=%dx%d",
		 PIXEL_WIDTH (f) + extra_borders, 
		 PIXEL_HEIGHT (f) + menubar_size + extra_borders);
    }

    len = strlen (shell_position) + 1;
    /* We don't free this because we don't know whether
       it is safe to free it while the frame exists.
       It isn't worth the trouble of arranging to free it
       when the frame is deleted.  */
    tem = (char *) xmalloc (len);
    strncpy (tem, shell_position, len);
    XtSetArg (al[ac], XtNgeometry, tem); ac++;
    XtSetValues (shell_widget, al, ac);
  }

  XtManageChild (pane_widget);
  XtRealizeWidget (shell_widget);

  FRAME_X_WINDOW (f) = XtWindow (frame_widget); 

  validate_x_resource_name ();

  class_hints.res_name = (char *) XSTRING (Vx_resource_name)->data;
  class_hints.res_class = (char *) XSTRING (Vx_resource_class)->data;
  XSetClassHint (FRAME_X_DISPLAY (f), XtWindow (shell_widget), &class_hints);

#ifdef HAVE_X_I18N
  FRAME_XIC (f) = NULL;
#ifdef USE_XIM
  create_frame_xic (f);
#endif
#endif

  f->output_data.x->wm_hints.input = True;
  f->output_data.x->wm_hints.flags |= InputHint;
  XSetWMHints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	       &f->output_data.x->wm_hints);

  hack_wm_protocols (f, shell_widget);

#ifdef HACK_EDITRES
  XtAddEventHandler (shell_widget, 0, True, _XEditResCheckMessages, 0);
#endif

  /* Do a stupid property change to force the server to generate a
     PropertyNotify event so that the event_stream server timestamp will
     be initialized to something relevant to the time we created the window.
     */
  XChangeProperty (XtDisplay (frame_widget), XtWindow (frame_widget),
		   FRAME_X_DISPLAY_INFO (f)->Xatom_wm_protocols,
		   XA_ATOM, 32, PropModeAppend,
		   (unsigned char*) NULL, 0);

  /* Make all the standard events reach the Emacs frame.  */
  attributes.event_mask = STANDARD_EVENT_SET;

#ifdef HAVE_X_I18N
  if (FRAME_XIC (f))
    {
      /* XIM server might require some X events. */
      unsigned long fevent = NoEventMask;
      XGetICValues(FRAME_XIC (f), XNFilterEvents, &fevent, NULL);
      attributes.event_mask |= fevent;
    }
#endif /* HAVE_X_I18N */
  
  attribute_mask = CWEventMask;
  XChangeWindowAttributes (XtDisplay (shell_widget), XtWindow (shell_widget),
			   attribute_mask, &attributes);

  XtMapWidget (frame_widget);

  /* x_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the X server hasn't been told.  */
  {
    Lisp_Object name;
    int explicit = f->explicit_name;

    f->explicit_name = 0;
    name = f->name;
    f->name = Qnil;
    x_set_name (f, name, explicit);
  }

  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 f->output_data.x->text_cursor);

  UNBLOCK_INPUT;

  /* This is a no-op, except under Motif.  Make sure main areas are
     set to something reasonable, in case we get an error later.  */
  lw_set_main_areas (pane_widget, 0, frame_widget);
}

#else /* not USE_X_TOOLKIT */

/* Create and set up the X window for frame F.  */

void
x_window (f)
     struct frame *f;

{
  XClassHint class_hints;
  XSetWindowAttributes attributes;
  unsigned long attribute_mask;

  attributes.background_pixel = f->output_data.x->background_pixel;
  attributes.border_pixel = f->output_data.x->border_pixel;
  attributes.bit_gravity = StaticGravity;
  attributes.backing_store = NotUseful;
  attributes.save_under = True;
  attributes.event_mask = STANDARD_EVENT_SET;
  attributes.colormap = FRAME_X_COLORMAP (f);
  attribute_mask = (CWBackPixel | CWBorderPixel | CWBitGravity | CWEventMask
		    | CWColormap);

  BLOCK_INPUT;
  FRAME_X_WINDOW (f)
    = XCreateWindow (FRAME_X_DISPLAY (f),
		     f->output_data.x->parent_desc,
		     f->output_data.x->left_pos,
		     f->output_data.x->top_pos,
		     PIXEL_WIDTH (f), PIXEL_HEIGHT (f),
		     f->output_data.x->border_width,
		     CopyFromParent, /* depth */
		     InputOutput, /* class */
		     FRAME_X_VISUAL (f),
		     attribute_mask, &attributes);

#ifdef HAVE_X_I18N
#ifdef USE_XIM
  create_frame_xic (f);
  if (FRAME_XIC (f))
    {
      /* XIM server might require some X events. */
      unsigned long fevent = NoEventMask;
      XGetICValues(FRAME_XIC (f), XNFilterEvents, &fevent, NULL);
      attributes.event_mask |= fevent;
      attribute_mask = CWEventMask;
      XChangeWindowAttributes (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			       attribute_mask, &attributes);
    }
#endif
#endif /* HAVE_X_I18N */
  
  validate_x_resource_name ();

  class_hints.res_name = (char *) XSTRING (Vx_resource_name)->data;
  class_hints.res_class = (char *) XSTRING (Vx_resource_class)->data;
  XSetClassHint (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &class_hints);

  /* The menubar is part of the ordinary display;
     it does not count in addition to the height of the window.  */
  f->output_data.x->menubar_height = 0;

  /* This indicates that we use the "Passive Input" input model.
     Unless we do this, we don't get the Focus{In,Out} events that we
     need to draw the cursor correctly.  Accursed bureaucrats.
   XWhipsAndChains (FRAME_X_DISPLAY (f), IronMaiden, &TheRack);  */

  f->output_data.x->wm_hints.input = True;
  f->output_data.x->wm_hints.flags |= InputHint;
  XSetWMHints (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
	       &f->output_data.x->wm_hints);
  f->output_data.x->wm_hints.icon_pixmap = None;

  /* Request "save yourself" and "delete window" commands from wm.  */
  {
    Atom protocols[2];
    protocols[0] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_delete_window;
    protocols[1] = FRAME_X_DISPLAY_INFO (f)->Xatom_wm_save_yourself;
    XSetWMProtocols (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), protocols, 2);
  }

  /* x_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the X server hasn't been told.  */
  {
    Lisp_Object name;
    int explicit = f->explicit_name;

    f->explicit_name = 0;
    name = f->name;
    f->name = Qnil;
    x_set_name (f, name, explicit);
  }

  XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 f->output_data.x->text_cursor);

  UNBLOCK_INPUT;

  if (FRAME_X_WINDOW (f) == 0)
    error ("Unable to create window");
}

#endif /* not USE_X_TOOLKIT */

/* Handle the icon stuff for this window.  Perhaps later we might
   want an x_set_icon_position which can be called interactively as
   well.  */

static void
x_icon (f, parms)
     struct frame *f;
     Lisp_Object parms;
{
  Lisp_Object icon_x, icon_y;
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  /* Set the position of the icon.  Note that twm groups all
     icons in an icon window.  */
  icon_x = x_get_and_record_arg (f, parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = x_get_and_record_arg (f, parms, Qicon_top, 0, 0, RES_TYPE_NUMBER);
  if (!EQ (icon_x, Qunbound) && !EQ (icon_y, Qunbound))
    {
      CHECK_NUMBER (icon_x);
      CHECK_NUMBER (icon_y);
    }
  else if (!EQ (icon_x, Qunbound) || !EQ (icon_y, Qunbound))
    error ("Both left and top icon corners of icon must be specified");

  BLOCK_INPUT;

  if (! EQ (icon_x, Qunbound))
    x_wm_set_icon_position (f, XINT (icon_x), XINT (icon_y));

  /* Start up iconic or window? */
  x_wm_set_window_state
    (f, (EQ (x_get_arg (dpyinfo, parms, Qvisibility, 0, 0, RES_TYPE_SYMBOL),
	     Qicon)
	 ? IconicState
	 : NormalState));

  x_text_icon (f, (char *) XSTRING ((!NILP (f->icon_name)
				     ? f->icon_name
				     : f->name))->data);

  UNBLOCK_INPUT;
}

/* Make the GCs needed for this window, setting the
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

  BLOCK_INPUT;

  /* Create the GCs of this frame.
     Note that many default values are used.  */

  /* Normal video */
  gc_values.font = f->output_data.x->font->fid;
  gc_values.foreground = f->output_data.x->foreground_pixel;
  gc_values.background = f->output_data.x->background_pixel;
  gc_values.line_width = 0;	/* Means 1 using fast algorithm.  */
  f->output_data.x->normal_gc
    = XCreateGC (FRAME_X_DISPLAY (f),
		 FRAME_X_WINDOW (f),
		 GCLineWidth | GCFont | GCForeground | GCBackground,
		 &gc_values);

  /* Reverse video style.  */
  gc_values.foreground = f->output_data.x->background_pixel;
  gc_values.background = f->output_data.x->foreground_pixel;
  f->output_data.x->reverse_gc
    = XCreateGC (FRAME_X_DISPLAY (f),
		 FRAME_X_WINDOW (f),
		 GCFont | GCForeground | GCBackground | GCLineWidth,
		 &gc_values);

  /* Cursor has cursor-color background, background-color foreground.  */
  gc_values.foreground = f->output_data.x->background_pixel;
  gc_values.background = f->output_data.x->cursor_pixel;
  gc_values.fill_style = FillOpaqueStippled;
  gc_values.stipple
    = XCreateBitmapFromData (FRAME_X_DISPLAY (f),
			     FRAME_X_DISPLAY_INFO (f)->root_window,
			     cursor_bits, 16, 16);
  f->output_data.x->cursor_gc
    = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 (GCFont | GCForeground | GCBackground
		  | GCFillStyle /* | GCStipple */ | GCLineWidth),
		 &gc_values);

  /* Reliefs.  */
  f->output_data.x->white_relief.gc = 0;
  f->output_data.x->black_relief.gc = 0;

  /* Create the gray border tile used when the pointer is not in
     the frame.  Since this depends on the frame's pixel values,
     this must be done on a per-frame basis.  */
  f->output_data.x->border_tile
    = (XCreatePixmapFromBitmapData
       (FRAME_X_DISPLAY (f), FRAME_X_DISPLAY_INFO (f)->root_window, 
	gray_bits, gray_width, gray_height,
	f->output_data.x->foreground_pixel,
	f->output_data.x->background_pixel,
	DefaultDepth (FRAME_X_DISPLAY (f), FRAME_X_SCREEN_NUMBER (f))));

  UNBLOCK_INPUT;
}


/* Free what was was allocated in x_make_gc.  */

void
x_free_gcs (f)
     struct frame *f;
{
  Display *dpy = FRAME_X_DISPLAY (f);

  BLOCK_INPUT;
  
  if (f->output_data.x->normal_gc)
    {
      XFreeGC (dpy, f->output_data.x->normal_gc);
      f->output_data.x->normal_gc = 0;
    }

  if (f->output_data.x->reverse_gc)
    {
      XFreeGC (dpy, f->output_data.x->reverse_gc);
      f->output_data.x->reverse_gc = 0;
    }
  
  if (f->output_data.x->cursor_gc)
    {
      XFreeGC (dpy, f->output_data.x->cursor_gc);
      f->output_data.x->cursor_gc = 0;
    }

  if (f->output_data.x->border_tile)
    {
      XFreePixmap (dpy, f->output_data.x->border_tile);
      f->output_data.x->border_tile = 0;
    }

  UNBLOCK_INPUT;
}


/* Handler for signals raised during x_create_frame and
   x_create_top_frame.  FRAME is the frame which is partially
   constructed.  */

static Lisp_Object
unwind_create_frame (frame)
     Lisp_Object frame;
{
  struct frame *f = XFRAME (frame);

  /* If frame is ``official'', nothing to do.  */
  if (!CONSP (Vframe_list) || !EQ (XCAR (Vframe_list), frame))
    {
#if GLYPH_DEBUG
      struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);
#endif
      
      x_free_frame_resources (f);

      /* Check that reference counts are indeed correct.  */
      xassert (dpyinfo->reference_count == dpyinfo_refcount);
      xassert (dpyinfo->image_cache->refcount == image_cache_refcount);
      return Qt;
    }
  
  return Qnil;
}


DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
       doc: /* Make a new X window, which is called a "frame" in Emacs terms.
Returns an Emacs frame object.
ALIST is an alist of frame parameters.
If the parameters specify that the frame should not have a minibuffer,
and do not specify a specific minibuffer window to use,
then `default-minibuffer-frame' must be a frame whose minibuffer can
be shared by the new frame.

This function is an internal primitive--use `make-frame' instead.  */)
     (parms)
     Lisp_Object parms;
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  int minibuffer_only = 0;
  long window_prompting = 0;
  int width, height;
  int count = BINDING_STACK_SIZE ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  Lisp_Object display;
  struct x_display_info *dpyinfo = NULL;
  Lisp_Object parent;
  struct kboard *kb;

  check_x ();

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = x_get_arg (dpyinfo, parms, Qdisplay, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_x_display_info (display);
#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  name = x_get_arg (dpyinfo, parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = x_get_arg (dpyinfo, parms, Qparent_id, NULL, NULL, RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  GCPRO4 (parms, parent, name, frame);
  tem = x_get_arg (dpyinfo, parms, Qminibuffer, "minibuffer", "Minibuffer",
		   RES_TYPE_SYMBOL);
  if (EQ (tem, Qnone) || NILP (tem))
    f = make_frame_without_minibuffer (Qnil, kb, display);
  else if (EQ (tem, Qonly))
    {
      f = make_minibuffer_frame ();
      minibuffer_only = 1;
    }
  else if (WINDOWP (tem))
    f = make_frame_without_minibuffer (tem, kb, display);
  else
    f = make_frame (1);

  XSETFRAME (frame, f);

  /* Note that X Windows does support scroll bars.  */
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;

  f->output_method = output_x_window;
  f->output_data.x = (struct x_output *) xmalloc (sizeof (struct x_output));
  bzero (f->output_data.x, sizeof (struct x_output));
  f->output_data.x->icon_bitmap = -1;
  f->output_data.x->fontset = -1;
  f->output_data.x->scroll_bar_foreground_pixel = -1;
  f->output_data.x->scroll_bar_background_pixel = -1;
#ifdef USE_TOOLKIT_SCROLL_BARS
  f->output_data.x->scroll_bar_top_shadow_pixel = -1;
  f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
#endif /* USE_TOOLKIT_SCROLL_BARS */
  record_unwind_protect (unwind_create_frame, frame);

  f->icon_name
    = x_get_arg (dpyinfo, parms, Qicon_name, "iconName", "Title",
		 RES_TYPE_STRING);
  if (! STRINGP (f->icon_name))
    f->icon_name = Qnil;

  FRAME_X_DISPLAY_INFO (f) = dpyinfo;
#if GLYPH_DEBUG
  image_cache_refcount = FRAME_X_IMAGE_CACHE (f)->refcount;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;
    struct gcpro gcpro1;

    /* Function x_decode_color can signal an error.  Make
       sure to initialize color slots so that we won't try
       to free colors we haven't allocated.  */
    f->output_data.x->foreground_pixel = -1;
    f->output_data.x->background_pixel = -1;
    f->output_data.x->cursor_pixel = -1;
    f->output_data.x->cursor_foreground_pixel = -1;
    f->output_data.x->border_pixel = -1;
    f->output_data.x->mouse_pixel = -1;
    
    black = build_string ("black");
    GCPRO1 (black);
    f->output_data.x->foreground_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->background_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_foreground_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->mouse_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    UNGCPRO;
  }

  /* Specify the parent under which to make this X window.  */

  if (!NILP (parent))
    {
      f->output_data.x->parent_desc = (Window) XFASTINT (parent);
      f->output_data.x->explicit_parent = 1;
    }
  else
    {
      f->output_data.x->parent_desc = FRAME_X_DISPLAY_INFO (f)->root_window;
      f->output_data.x->explicit_parent = 0;
    }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (dpyinfo->x_id_name);
      f->explicit_name = 0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  {
    Lisp_Object font;

    font = x_get_arg (dpyinfo, parms, Qfont, "font", "Font", RES_TYPE_STRING);

    BLOCK_INPUT;
    /* First, try whatever font the caller has specified.  */
    if (STRINGP (font))
      {
	tem = Fquery_fontset (font, Qnil);
	if (STRINGP (tem))
	  font = x_new_fontset (f, XSTRING (tem)->data);
	else
	  font = x_new_font (f, XSTRING (font)->data);
      }
    
    /* Try out a font which we hope has bold and italic variations.  */
    if (!STRINGP (font))
      font = x_new_font (f, "-adobe-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1");
    if (!STRINGP (font))
      font = x_new_font (f, "-misc-fixed-medium-r-normal-*-*-140-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-*-medium-r-normal-*-*-140-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      /* This was formerly the first thing tried, but it finds too many fonts
	 and takes too long.  */
      font = x_new_font (f, "-*-*-medium-r-*-*-*-*-*-*-c-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (! STRINGP (font))
      font = x_new_font (f, "-*-fixed-*-*-*-*-*-140-*-*-c-*-iso8859-1");
    UNBLOCK_INPUT;
    if (! STRINGP (font))
      font = build_string ("fixed");

    x_default_parameter (f, parms, Qfont, font,
			 "font", "Font", RES_TYPE_STRING);
  }

#ifdef USE_LUCID
  /* Prevent lwlib/xlwmenu.c from crashing because of a bug
     whereby it fails to get any font.  */
  xlwmenu_default_font = f->output_data.x->font;
#endif

  x_default_parameter (f, parms, Qborder_width, make_number (2),
		       "borderWidth", "BorderWidth", RES_TYPE_NUMBER);
  
  /* This defaults to 1 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = x_get_arg (dpyinfo, parms, Qinternal_border_width,
			 "internalBorder", "internalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }
  x_default_parameter (f, parms, Qinternal_border_width, make_number (1),
		       "internalBorderWidth", "internalBorderWidth",
		       RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qvertical_scroll_bars, Qleft,
		       "verticalScrollBars", "ScrollBars",
		       RES_TYPE_SYMBOL);

  /* Also do the stuff which must be set before the window exists.  */
  x_default_parameter (f, parms, Qforeground_color, build_string ("black"),
		       "foreground", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qbackground_color, build_string ("white"),
		       "background", "Background", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qmouse_color, build_string ("black"),
		       "pointerColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qcursor_color, build_string ("black"),
		       "cursorColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qborder_color, build_string ("black"),
		       "borderColor", "BorderColor", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qscreen_gamma, Qnil,
		       "screenGamma", "ScreenGamma", RES_TYPE_FLOAT);
  x_default_parameter (f, parms, Qline_spacing, Qnil,
		       "lineSpacing", "LineSpacing", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qleft_fringe, Qnil,
		       "leftFringe", "LeftFringe", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qright_fringe, Qnil,
		       "rightFringe", "RightFringe", RES_TYPE_NUMBER);

  x_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_foreground,
					"scrollBarForeground",
					"ScrollBarForeground", 1);
  x_default_scroll_bar_color_parameter (f, parms, Qscroll_bar_background,
					"scrollBarBackground",
					"ScrollBarBackground", 0);

  /* Init faces before x_default_parameter is called for scroll-bar
     parameters because that function calls x_set_scroll_bar_width,
     which calls change_frame_size, which calls Fset_window_buffer,
     which runs hooks, which call Fvertical_motion.  At the end, we
     end up in init_iterator with a null face cache, which should not
     happen.  */
  init_frame_faces (f);
  
  x_default_parameter (f, parms, Qmenu_bar_lines, make_number (1),
		       "menuBar", "MenuBar", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qtool_bar_lines, make_number (1),
		       "toolBar", "ToolBar", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qbuffer_predicate, Qnil,
		       "bufferPredicate", "BufferPredicate",
		       RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtitle, Qnil,
		       "title", "Title", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qwait_for_wm, Qt,
		       "waitForWM", "WaitForWM", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qfullscreen, Qnil,
                       "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  f->output_data.x->parent_desc = FRAME_X_DISPLAY_INFO (f)->root_window;

  /* Add the tool-bar height to the initial frame height so that the
     user gets a text display area of the size he specified with -g or
     via .Xdefaults.  Later changes of the tool-bar height don't
     change the frame size.  This is done so that users can create
     tall Emacs frames without having to guess how tall the tool-bar
     will get.  */
  if (FRAME_TOOL_BAR_LINES (f))
    {
      int margin, relief, bar_height;
      
      relief = (tool_bar_button_relief >= 0
		? tool_bar_button_relief
		: DEFAULT_TOOL_BAR_BUTTON_RELIEF);

      if (INTEGERP (Vtool_bar_button_margin)
	  && XINT (Vtool_bar_button_margin) > 0)
	margin = XFASTINT (Vtool_bar_button_margin);
      else if (CONSP (Vtool_bar_button_margin)
	       && INTEGERP (XCDR (Vtool_bar_button_margin))
	       && XINT (XCDR (Vtool_bar_button_margin)) > 0)
	margin = XFASTINT (XCDR (Vtool_bar_button_margin));
      else
	margin = 0;
	  
      bar_height = DEFAULT_TOOL_BAR_IMAGE_HEIGHT + 2 * margin + 2 * relief;
      f->height += (bar_height + CANON_Y_UNIT (f) - 1) / CANON_Y_UNIT (f);
    }

  /* Compute the size of the X window.  */
  window_prompting = x_figure_window_size (f, parms);

  if (window_prompting & XNegative)
    {
      if (window_prompting & YNegative)
	f->output_data.x->win_gravity = SouthEastGravity;
      else
	f->output_data.x->win_gravity = NorthEastGravity;
    }
  else
    {
      if (window_prompting & YNegative)
	f->output_data.x->win_gravity = SouthWestGravity;
      else
	f->output_data.x->win_gravity = NorthWestGravity;
    }

  f->output_data.x->size_hint_flags = window_prompting;

  tem = x_get_arg (dpyinfo, parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  /* Create the X widget or window.  */
#ifdef USE_X_TOOLKIT
  x_window (f, window_prompting, minibuffer_only);
#else
  x_window (f);
#endif
  
  x_icon (f, parms);
  x_make_gc (f);

  /* Now consider the frame official.  */
  FRAME_X_DISPLAY_INFO (f)->reference_count++;
  Vframe_list = Fcons (frame, Vframe_list);

  /* We need to do this after creating the X window, so that the
     icon-creation functions can say whose icon they're describing.  */
  x_default_parameter (f, parms, Qicon_type, Qnil,
		       "bitmapIcon", "BitmapIcon", RES_TYPE_SYMBOL);

  x_default_parameter (f, parms, Qauto_raise, Qnil,
		       "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qauto_lower, Qnil,
		       "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qcursor_type, Qbox,
		       "cursorType", "CursorType", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qscroll_bar_width, Qnil,
		       "scrollBarWidth", "ScrollBarWidth",
		       RES_TYPE_NUMBER);

  /* Dimensions, especially f->height, must be done via change_frame_size.
     Change will not be effected unless different from the current
     f->height.  */
  width = f->width;
  height = f->height;
  
  f->height = 0;
  SET_FRAME_WIDTH (f, 0);
  change_frame_size (f, height, width, 1, 0, 0);

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.  If we
     don't do this, the `menu' face for instance won't have the right
     colors, and the menu bar won't appear in the specified colors for
     new frames.  */
  call1 (Qface_set_after_frame_default, frame);

#ifdef USE_X_TOOLKIT
  /* Create the menu bar.  */
  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f))
    {
      /* If this signals an error, we haven't set size hints for the
	 frame and we didn't make it visible.  */
      initialize_frame_menubar (f);

      /* This is a no-op, except under Motif where it arranges the
	 main window for the widgets on it.  */
      lw_set_main_areas (f->output_data.x->column_widget,
			 f->output_data.x->menubar_widget,
			 f->output_data.x->edit_widget);
    }
#endif /* USE_X_TOOLKIT */

  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  BLOCK_INPUT;
  x_wm_set_size_hint (f, window_prompting, 0);
  UNBLOCK_INPUT;

  /* Make the window appear on the frame and enable display, unless
     the caller says not to.  However, with explicit parent, Emacs
     cannot control visibility, so don't try.  */
  if (! f->output_data.x->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = x_get_arg (dpyinfo, parms, Qvisibility, 0, 0,
			      RES_TYPE_SYMBOL);
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

  UNGCPRO;

  /* Make sure windows on this frame appear in calls to next-window
     and similar functions.  */
  Vwindow_list = Qnil;
  
  return unbind_to (count, frame);
}


/* FRAME is used only to get a handle on the X display.  We don't pass the
   display info directly because we're called from frame.c, which doesn't
   know about that structure.  */

Lisp_Object
x_get_focus_frame (frame)
     struct frame *frame;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (frame);
  Lisp_Object xfocus;
  if (! dpyinfo->x_focus_frame)
    return Qnil;

  XSETFRAME (xfocus, dpyinfo->x_focus_frame);
  return xfocus;
}


/* In certain situations, when the window manager follows a
   click-to-focus policy, there seems to be no way around calling
   XSetInputFocus to give another frame the input focus .

   In an ideal world, XSetInputFocus should generally be avoided so
   that applications don't interfere with the window manager's focus
   policy.  But I think it's okay to use when it's clearly done
   following a user-command.  */

DEFUN ("x-focus-frame", Fx_focus_frame, Sx_focus_frame, 1, 1, 0,
       doc: /* Set the input focus to FRAME.
FRAME nil means use the selected frame.  */)
     (frame)
     Lisp_Object frame;
{
  struct frame *f = check_x_frame (frame);
  Display *dpy = FRAME_X_DISPLAY (f);
  int count;

  BLOCK_INPUT;
  count = x_catch_errors (dpy);
  XSetInputFocus (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  RevertToParent, CurrentTime);
  x_uncatch_errors (dpy, count);
  UNBLOCK_INPUT;
  
  return Qnil;
}


DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* Internal function called by `color-defined-p', which see.  */)
     (color, frame)
     Lisp_Object color, frame;
{
  XColor foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color);

  if (x_defined_color (f, XSTRING (color)->data, &foo, 0))
    return Qt;
  else
    return Qnil;
}

DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
       doc: /* Internal function called by `color-values', which see.  */)
     (color, frame)
     Lisp_Object color, frame;
{
  XColor foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color);

  if (x_defined_color (f, XSTRING (color)->data, &foo, 0))
    {
      Lisp_Object rgb[3];

      rgb[0] = make_number (foo.red);
      rgb[1] = make_number (foo.green);
      rgb[2] = make_number (foo.blue);
      return Flist (3, rgb);
    }
  else
    return Qnil;
}

DEFUN ("xw-display-color-p", Fxw_display_color_p, Sxw_display_color_p, 0, 1, 0,
       doc: /* Internal function called by `display-color-p', which see.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  if (dpyinfo->n_planes <= 2)
    return Qnil;

  switch (dpyinfo->visual->class)
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

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p, Sx_display_grayscale_p,
       0, 1, 0,
       doc: /* Return t if the X display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  if (dpyinfo->n_planes <= 1)
    return Qnil;

  switch (dpyinfo->visual->class)
    {
    case StaticColor:
    case PseudoColor:
    case TrueColor:
    case DirectColor:
    case StaticGray:
    case GrayScale:
      return Qt;

    default:
      return Qnil;
    }
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width, Sx_display_pixel_width,
       0, 1, 0,
       doc: /* Returns the width in pixels of the X display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->width);
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* Returns the height in pixels of the X display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->height);
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* Returns the number of bitplanes of the X display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->n_planes);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* Returns the number of color cells of the X display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (DisplayCells (dpyinfo->display,
				    XScreenNumberOfScreen (dpyinfo->screen)));
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* Returns the maximum request size of the X server of display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (MAXREQUEST (dpyinfo->display));
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Returns the vendor ID string of the X server of display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);
  char *vendor = ServerVendor (dpyinfo->display);

  if (! vendor) vendor = "";
  return build_string (vendor);
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Returns the version numbers of the X server of display DISPLAY.
The value is a list of three integers: the major and minor
version numbers of the X Protocol in use, and the vendor-specific release
number.  See also the function `x-server-vendor'.

The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);
  Display *dpy = dpyinfo->display;

  return Fcons (make_number (ProtocolVersion (dpy)),
		Fcons (make_number (ProtocolRevision (dpy)),
		       Fcons (make_number (VendorRelease (dpy)), Qnil)));
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Return the number of screens on the X server of display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (ScreenCount (dpyinfo->display));
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
       doc: /* Return the height in millimeters of the X display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (HeightMMOfScreen (dpyinfo->screen));
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
       doc: /* Return the width in millimeters of the X display DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (WidthMMOfScreen (dpyinfo->screen));
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
       Sx_display_backing_store, 0, 1, 0,
       doc: /* Returns an indication of whether X display DISPLAY does backing store.
The value may be `always', `when-mapped', or `not-useful'.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);
  Lisp_Object result;

  switch (DoesBackingStore (dpyinfo->screen))
    {
    case Always:
      result = intern ("always");
      break;

    case WhenMapped:
      result = intern ("when-mapped");
      break;

    case NotUseful:
      result = intern ("not-useful");
      break;

    default:
      error ("Strange value for BackingStore parameter of screen");
      result = Qnil;
    }

  return result;
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* Return the visual class of the X display DISPLAY.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.

The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);
  Lisp_Object result;

  switch (dpyinfo->visual->class)
    {
    case StaticGray:
      result = intern ("static-gray");
      break;
    case GrayScale:
      result = intern ("gray-scale");
      break;
    case StaticColor:
      result = intern ("static-color");
      break;
    case PseudoColor:
      result = intern ("pseudo-color");
      break;
    case TrueColor:
      result = intern ("true-color");
      break;
    case DirectColor:
      result = intern ("direct-color");
      break;
    default:
      error ("Display has an unknown visual class");
      result = Qnil;
    }
  
  return result;
}

DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* Returns t if the X display DISPLAY supports the save-under feature.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  if (DoesSaveUnders (dpyinfo->screen) == True)
    return Qt;
  else
    return Qnil;
}

int
x_pixel_width (f)
     register struct frame *f;
{
  return PIXEL_WIDTH (f);
}

int
x_pixel_height (f)
     register struct frame *f;
{
  return PIXEL_HEIGHT (f);
}

int
x_char_width (f)
     register struct frame *f;
{
  return FONT_WIDTH (f->output_data.x->font);
}

int
x_char_height (f)
     register struct frame *f;
{
  return f->output_data.x->line_height;
}

int
x_screen_planes (f)
     register struct frame *f;
{
  return FRAME_X_DISPLAY_INFO (f)->n_planes;
}



/************************************************************************
			      X Displays
 ************************************************************************/


/* Mapping visual names to visuals.  */

static struct visual_class
{
  char *name;
  int class;
}
visual_classes[] =
{
  {"StaticGray",	StaticGray},
  {"GrayScale",		GrayScale},
  {"StaticColor",	StaticColor},
  {"PseudoColor",	PseudoColor},
  {"TrueColor",		TrueColor},
  {"DirectColor",	DirectColor},
  {NULL, 0}
};


#ifndef HAVE_XSCREENNUMBEROFSCREEN

/* Value is the screen number of screen SCR.  This is a substitute for
   the X function with the same name when that doesn't exist.  */

int
XScreenNumberOfScreen (scr)
    register Screen *scr;
{
  Display *dpy = scr->display;
  int i;

  for (i = 0; i < dpy->nscreens; ++i)
    if (scr == dpy->screens + i)
      break;

  return i;
}

#endif /* not HAVE_XSCREENNUMBEROFSCREEN */


/* Select the visual that should be used on display DPYINFO.  Set
   members of DPYINFO appropriately.  Called from x_term_init.  */

void
select_visual (dpyinfo)
     struct x_display_info *dpyinfo;
{
  Display *dpy = dpyinfo->display;
  Screen *screen = dpyinfo->screen;
  Lisp_Object value;

  /* See if a visual is specified.  */
  value = display_x_get_resource (dpyinfo,
				  build_string ("visualClass"),
				  build_string ("VisualClass"),
				  Qnil, Qnil);
  if (STRINGP (value))
    {
      /* VALUE should be of the form CLASS-DEPTH, where CLASS is one
	 of `PseudoColor', `TrueColor' etc. and DEPTH is the color
	 depth, a decimal number.  NAME is compared with case ignored.  */
      char *s = (char *) alloca (STRING_BYTES (XSTRING (value)) + 1);
      char *dash;
      int i, class = -1;
      XVisualInfo vinfo;

      strcpy (s, XSTRING (value)->data);
      dash = index (s, '-');
      if (dash)
	{
	  dpyinfo->n_planes = atoi (dash + 1);
	  *dash = '\0';
	}
      else
	/* We won't find a matching visual with depth 0, so that
	   an error will be printed below.  */
	dpyinfo->n_planes = 0;

      /* Determine the visual class.  */
      for (i = 0; visual_classes[i].name; ++i)
	if (xstricmp (s, visual_classes[i].name) == 0)
	  {
	    class = visual_classes[i].class;
	    break;
	  }

      /* Look up a matching visual for the specified class.  */
      if (class == -1
	  || !XMatchVisualInfo (dpy, XScreenNumberOfScreen (screen),
				dpyinfo->n_planes, class, &vinfo))
	fatal ("Invalid visual specification `%s'", XSTRING (value)->data);
      
      dpyinfo->visual = vinfo.visual;
    }
  else
    {
      int n_visuals;
      XVisualInfo *vinfo, vinfo_template;
      
      dpyinfo->visual = DefaultVisualOfScreen (screen);

#ifdef HAVE_X11R4
      vinfo_template.visualid = XVisualIDFromVisual (dpyinfo->visual);
#else
      vinfo_template.visualid = dpyinfo->visual->visualid;
#endif
      vinfo_template.screen = XScreenNumberOfScreen (screen);
      vinfo = XGetVisualInfo (dpy, VisualIDMask | VisualScreenMask,
			      &vinfo_template, &n_visuals);
      if (n_visuals != 1)
	fatal ("Can't get proper X visual info");

      dpyinfo->n_planes = vinfo->depth;
      XFree ((char *) vinfo);
    }
}


/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */

struct x_display_info *
x_display_info_for_name (name)
     Lisp_Object name;
{
  Lisp_Object names;
  struct x_display_info *dpyinfo;

  CHECK_STRING (name);

  if (! EQ (Vwindow_system, intern ("x")))
    error ("Not using X Windows");

  for (dpyinfo = x_display_list, names = x_display_name_list;
       dpyinfo;
       dpyinfo = dpyinfo->next, names = XCDR (names))
    {
      Lisp_Object tem;
      tem = Fstring_equal (XCAR (XCAR (names)), name);
      if (!NILP (tem))
	return dpyinfo;
    }

  /* Use this general default value to start with.  */
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  dpyinfo = x_term_init (name, (char *)0,
			 (char *) XSTRING (Vx_resource_name)->data);

  if (dpyinfo == 0)
    error ("Cannot connect to X server %s", XSTRING (name)->data);

  x_in_use = 1;
  XSETFASTINT (Vwindow_system_version, 11);

  return dpyinfo;
}


DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0,
       doc: /* Open a connection to an X server.
DISPLAY is the name of the display to connect to.
Optional second arg XRM-STRING is a string of resources in xrdb format.
If the optional third arg MUST-SUCCEED is non-nil,
terminate Emacs if we can't open the connection.  */)
     (display, xrm_string, must_succeed)
     Lisp_Object display, xrm_string, must_succeed;
{
  unsigned char *xrm_option;
  struct x_display_info *dpyinfo;

  CHECK_STRING (display);
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string);

  if (! EQ (Vwindow_system, intern ("x")))
    error ("Not using X Windows");

  if (! NILP (xrm_string))
    xrm_option = (unsigned char *) XSTRING (xrm_string)->data;
  else
    xrm_option = (unsigned char *) 0;

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = x_term_init (display, xrm_option,
			 (char *) XSTRING (Vx_resource_name)->data);

  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Cannot connect to X server %s.\n\
Check the DISPLAY environment variable or use `-d'.\n\
Also use the `xhost' program to verify that it is set to permit\n\
connections from your machine.\n",
	       XSTRING (display)->data);
      else
	error ("Cannot connect to X server %s", XSTRING (display)->data);
    }

  x_in_use = 1;

  XSETFASTINT (Vwindow_system_version, 11);
  return Qnil;
}

DEFUN ("x-close-connection", Fx_close_connection,
       Sx_close_connection, 1, 1, 0,
       doc: /* Close the connection to DISPLAY's X server.
For DISPLAY, specify either a frame or a display name (a string).
If DISPLAY is nil, that stands for the selected frame's display.  */)
     (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);
  int i;

  if (dpyinfo->reference_count > 0)
    error ("Display still has frames on it");

  BLOCK_INPUT;
  /* Free the fonts in the font table.  */
  for (i = 0; i < dpyinfo->n_fonts; i++)
    if (dpyinfo->font_table[i].name)
      {
	if (dpyinfo->font_table[i].name != dpyinfo->font_table[i].full_name)
	  xfree (dpyinfo->font_table[i].full_name);
	xfree (dpyinfo->font_table[i].name);
	XFreeFont (dpyinfo->display, dpyinfo->font_table[i].font);
      }

  x_destroy_all_bitmaps (dpyinfo);
  XSetCloseDownMode (dpyinfo->display, DestroyAll);

#ifdef USE_X_TOOLKIT
  XtCloseDisplay (dpyinfo->display);
#else
  XCloseDisplay (dpyinfo->display);
#endif

  x_delete_display (dpyinfo);
  UNBLOCK_INPUT;

  return Qnil;
}

DEFUN ("x-display-list", Fx_display_list, Sx_display_list, 0, 0, 0,
       doc: /* Return the list of display names that Emacs has connections to.  */)
     ()
{
  Lisp_Object tail, result;

  result = Qnil;
  for (tail = x_display_name_list; ! NILP (tail); tail = XCDR (tail))
    result = Fcons (XCAR (XCAR (tail)), result);

  return result;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize, 1, 2, 0,
       doc: /* If ON is non-nil, report X errors as soon as the erring request is made.
If ON is nil, allow buffering of requests.
Turning on synchronization prohibits the Xlib routines from buffering
requests and seriously degrades performance, but makes debugging much
easier.
The optional second argument DISPLAY specifies which display to act on.
DISPLAY should be either a frame or a display name (a string).
If DISPLAY is omitted or nil, that stands for the selected frame's display.  */)
     (on, display)
    Lisp_Object display, on;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  XSynchronize (dpyinfo->display, !EQ (on, Qnil));

  return Qnil;
}

/* Wait for responses to all X commands issued so far for frame F.  */

void
x_sync (f)
     FRAME_PTR f;
{
  BLOCK_INPUT;
  XSync (FRAME_X_DISPLAY (f), False);
  UNBLOCK_INPUT;
}


/***********************************************************************
			    Image types
 ***********************************************************************/

/* Value is the number of elements of vector VECTOR.  */

#define DIM(VECTOR)	(sizeof (VECTOR) / sizeof *(VECTOR))

/* List of supported image types.  Use define_image_type to add new
   types.  Use lookup_image_type to find a type for a given symbol.  */

static struct image_type *image_types;

/* The symbol `image' which is the car of the lists used to represent
   images in Lisp.  */

extern Lisp_Object Qimage;

/* The symbol `xbm' which is used as the type symbol for XBM images.  */

Lisp_Object Qxbm;

/* Keywords.  */

extern Lisp_Object QCwidth, QCheight, QCforeground, QCbackground, QCfile;
extern Lisp_Object QCdata;
Lisp_Object QCtype, QCascent, QCmargin, QCrelief;
Lisp_Object QCconversion, QCcolor_symbols, QCheuristic_mask;
Lisp_Object QCindex, QCmatrix, QCcolor_adjustment, QCmask;

/* Other symbols.  */

Lisp_Object Qlaplace, Qemboss, Qedge_detection, Qheuristic;

/* Time in seconds after which images should be removed from the cache
   if not displayed.  */

Lisp_Object Vimage_cache_eviction_delay;

/* Function prototypes.  */

static void define_image_type P_ ((struct image_type *type));
static struct image_type *lookup_image_type P_ ((Lisp_Object symbol));
static void image_error P_ ((char *format, Lisp_Object, Lisp_Object));
static void x_laplace P_ ((struct frame *, struct image *));
static void x_emboss P_ ((struct frame *, struct image *));
static int x_build_heuristic_mask P_ ((struct frame *, struct image *,
				       Lisp_Object));


/* Define a new image type from TYPE.  This adds a copy of TYPE to
   image_types and adds the symbol *TYPE->type to Vimage_types.  */

static void
define_image_type (type)
     struct image_type *type;
{
  /* Make a copy of TYPE to avoid a bus error in a dumped Emacs.
     The initialized data segment is read-only.  */
  struct image_type *p = (struct image_type *) xmalloc (sizeof *p);
  bcopy (type, p, sizeof *p);
  p->next = image_types;
  image_types = p;
  Vimage_types = Fcons (*p->type, Vimage_types);
}


/* Look up image type SYMBOL, and return a pointer to its image_type
   structure.  Value is null if SYMBOL is not a known image type.  */

static INLINE struct image_type *
lookup_image_type (symbol)
     Lisp_Object symbol;
{
  struct image_type *type;

  for (type = image_types; type; type = type->next)
    if (EQ (symbol, *type->type))
      break;

  return type;
}


/* Value is non-zero if OBJECT is a valid Lisp image specification.  A
   valid image specification is a list whose car is the symbol
   `image', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported image type.  The rest of the property list depends on the
   image type.  */

int
valid_image_p (object)
     Lisp_Object object;
{
  int valid_p = 0;
  
  if (CONSP (object) && EQ (XCAR (object), Qimage))
    {
      Lisp_Object tem;

      for (tem = XCDR (object); CONSP (tem); tem = XCDR (tem))
	if (EQ (XCAR (tem), QCtype))
	  {
	    tem = XCDR (tem);
	    if (CONSP (tem) && SYMBOLP (XCAR (tem)))
	      {
		struct image_type *type;
		type = lookup_image_type (XCAR (tem));
		if (type)
		  valid_p = type->valid_p (object);
	      }
	    
	    break;
	  }
    }

  return valid_p;
}


/* Log error message with format string FORMAT and argument ARG.
   Signaling an error, e.g. when an image cannot be loaded, is not a
   good idea because this would interrupt redisplay, and the error
   message display would lead to another redisplay.  This function
   therefore simply displays a message.  */

static void
image_error (format, arg1, arg2)
     char *format;
     Lisp_Object arg1, arg2;
{
  add_to_log (format, arg1, arg2);
}



/***********************************************************************
			 Image specifications
 ***********************************************************************/

enum image_value_type
{
  IMAGE_DONT_CHECK_VALUE_TYPE,
  IMAGE_STRING_VALUE,
  IMAGE_STRING_OR_NIL_VALUE,
  IMAGE_SYMBOL_VALUE,
  IMAGE_POSITIVE_INTEGER_VALUE,
  IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,
  IMAGE_NON_NEGATIVE_INTEGER_VALUE,
  IMAGE_ASCENT_VALUE,
  IMAGE_INTEGER_VALUE,
  IMAGE_FUNCTION_VALUE,
  IMAGE_NUMBER_VALUE,
  IMAGE_BOOL_VALUE
};

/* Structure used when parsing image specifications.  */

struct image_keyword
{
  /* Name of keyword.  */
  char *name;

  /* The type of value allowed.  */
  enum image_value_type type;

  /* Non-zero means key must be present.  */
  int mandatory_p;

  /* Used to recognize duplicate keywords in a property list.  */
  int count;

  /* The value that was found.  */
  Lisp_Object value;
};


static int parse_image_spec P_ ((Lisp_Object, struct image_keyword *,
				 int, Lisp_Object));
static Lisp_Object image_spec_value P_ ((Lisp_Object, Lisp_Object, int *));


/* Parse image spec SPEC according to KEYWORDS.  A valid image spec
   has the format (image KEYWORD VALUE ...).  One of the keyword/
   value pairs must be `:type TYPE'.  KEYWORDS is a vector of
   image_keywords structures of size NKEYWORDS describing other
   allowed keyword/value pairs.  Value is non-zero if SPEC is valid.  */

static int
parse_image_spec (spec, keywords, nkeywords, type)
     Lisp_Object spec;
     struct image_keyword *keywords;
     int nkeywords;
     Lisp_Object type;
{
  int i;
  Lisp_Object plist;

  if (!CONSP (spec) || !EQ (XCAR (spec), Qimage))
    return 0;

  plist = XCDR (spec);
  while (CONSP (plist))
    {
      Lisp_Object key, value;

      /* First element of a pair must be a symbol.  */
      key = XCAR (plist);
      plist = XCDR (plist);
      if (!SYMBOLP (key))
	return 0;

      /* There must follow a value.  */
      if (!CONSP (plist))
	return 0;
      value = XCAR (plist);
      plist = XCDR (plist);

      /* Find key in KEYWORDS.  Error if not found.  */
      for (i = 0; i < nkeywords; ++i)
	if (strcmp (keywords[i].name, XSYMBOL (key)->name->data) == 0)
	  break;

      if (i == nkeywords)
	continue;

      /* Record that we recognized the keyword.  If a keywords
	 was found more than once, it's an error.  */
      keywords[i].value = value;
      ++keywords[i].count;
      
      if (keywords[i].count > 1)
	return 0;

      /* Check type of value against allowed type.  */
      switch (keywords[i].type)
	{
	case IMAGE_STRING_VALUE:
	  if (!STRINGP (value))
	    return 0;
	  break;

	case IMAGE_STRING_OR_NIL_VALUE:
	  if (!STRINGP (value) && !NILP (value))
	    return 0;
	  break;

	case IMAGE_SYMBOL_VALUE:
	  if (!SYMBOLP (value))
	    return 0;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE:
	  if (!INTEGERP (value) || XINT (value) <= 0)
	    return 0;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR:
	  if (INTEGERP (value) && XINT (value) >= 0)
	    break;
	  if (CONSP (value)
	      && INTEGERP (XCAR (value)) && INTEGERP (XCDR (value))
	      && XINT (XCAR (value)) >= 0 && XINT (XCDR (value)) >= 0)
	    break;
	  return 0;

	case IMAGE_ASCENT_VALUE:
	  if (SYMBOLP (value) && EQ (value, Qcenter))
	    break;
	  else if (INTEGERP (value)
		   && XINT (value) >= 0
		   && XINT (value) <= 100)
	    break;
	  return 0;
	      
	case IMAGE_NON_NEGATIVE_INTEGER_VALUE:
	  if (!INTEGERP (value) || XINT (value) < 0)
	    return 0;
	  break;

	case IMAGE_DONT_CHECK_VALUE_TYPE:
	  break;

	case IMAGE_FUNCTION_VALUE:
	  value = indirect_function (value);
	  if (SUBRP (value) 
	      || COMPILEDP (value)
	      || (CONSP (value) && EQ (XCAR (value), Qlambda)))
	    break;
	  return 0;

	case IMAGE_NUMBER_VALUE:
	  if (!INTEGERP (value) && !FLOATP (value))
	    return 0;
	  break;

	case IMAGE_INTEGER_VALUE:
	  if (!INTEGERP (value))
	    return 0;
	  break;

	case IMAGE_BOOL_VALUE:
	  if (!NILP (value) && !EQ (value, Qt))
	    return 0;
	  break;

	default:
	  abort ();
	  break;
	}

      if (EQ (key, QCtype) && !EQ (type, value))
	return 0;
    }

  /* Check that all mandatory fields are present.  */
  for (i = 0; i < nkeywords; ++i)
    if (keywords[i].mandatory_p && keywords[i].count == 0)
      return 0;

  return NILP (plist);
}


/* Return the value of KEY in image specification SPEC.  Value is nil
   if KEY is not present in SPEC.  if FOUND is not null, set *FOUND
   to 1 if KEY was found in SPEC, set it to 0 otherwise.  */

static Lisp_Object
image_spec_value (spec, key, found)
     Lisp_Object spec, key;
     int *found;
{
  Lisp_Object tail;
  
  xassert (valid_image_p (spec));

  for (tail = XCDR (spec);
       CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), key))
	{
	  if (found)
	    *found = 1;
	  return XCAR (XCDR (tail));
	}
    }
  
  if (found)
    *found = 0;
  return Qnil;
}
     

DEFUN ("image-size", Fimage_size, Simage_size, 1, 3, 0,
       doc: /* Return the size of image SPEC as pair (WIDTH . HEIGHT).
PIXELS non-nil means return the size in pixels, otherwise return the
size in canonical character units.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
     (spec, pixels, frame)
     Lisp_Object spec, pixels, frame;
{
  Lisp_Object size;

  size = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = check_x_frame (frame);
      int id = lookup_image (f, spec);
      struct image *img = IMAGE_FROM_ID (f, id);
      int width = img->width + 2 * img->hmargin;
      int height = img->height + 2 * img->vmargin;
  
      if (NILP (pixels))
	size = Fcons (make_float ((double) width / CANON_X_UNIT (f)),
		      make_float ((double) height / CANON_Y_UNIT (f)));
      else
	size = Fcons (make_number (width), make_number (height));
    }
  else
    error ("Invalid image specification");

  return size;
}


DEFUN ("image-mask-p", Fimage_mask_p, Simage_mask_p, 1, 2, 0,
       doc: /* Return t if image SPEC has a mask bitmap.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.  */)
     (spec, frame)
     Lisp_Object spec, frame;
{
  Lisp_Object mask;

  mask = Qnil;
  if (valid_image_p (spec))
    {
      struct frame *f = check_x_frame (frame);
      int id = lookup_image (f, spec);
      struct image *img = IMAGE_FROM_ID (f, id);
      if (img->mask)
	mask = Qt;
    }
  else
    error ("Invalid image specification");

  return mask;
}



/***********************************************************************
		 Image type independent image structures
 ***********************************************************************/

static struct image *make_image P_ ((Lisp_Object spec, unsigned hash));
static void free_image P_ ((struct frame *f, struct image *img));


/* Allocate and return a new image structure for image specification
   SPEC.  SPEC has a hash value of HASH.  */

static struct image *
make_image (spec, hash)
     Lisp_Object spec;
     unsigned hash;
{
  struct image *img = (struct image *) xmalloc (sizeof *img);
  
  xassert (valid_image_p (spec));
  bzero (img, sizeof *img);
  img->type = lookup_image_type (image_spec_value (spec, QCtype, NULL));
  xassert (img->type != NULL);
  img->spec = spec;
  img->data.lisp_val = Qnil;
  img->ascent = DEFAULT_IMAGE_ASCENT;
  img->hash = hash;
  return img;
}


/* Free image IMG which was used on frame F, including its resources.  */

static void
free_image (f, img)
     struct frame *f;
     struct image *img;
{
  if (img)
    {
      struct image_cache *c = FRAME_X_IMAGE_CACHE (f);

      /* Remove IMG from the hash table of its cache.  */
      if (img->prev)
	img->prev->next = img->next;
      else
	c->buckets[img->hash % IMAGE_CACHE_BUCKETS_SIZE] = img->next;

      if (img->next)
	img->next->prev = img->prev;

      c->images[img->id] = NULL;

      /* Free resources, then free IMG.  */
      img->type->free (f, img);
      xfree (img);
    }
}


/* Prepare image IMG for display on frame F.  Must be called before
   drawing an image.  */

void
prepare_image_for_display (f, img)
     struct frame *f;
     struct image *img;
{
  EMACS_TIME t;

  /* We're about to display IMG, so set its timestamp to `now'.  */
  EMACS_GET_TIME (t);
  img->timestamp = EMACS_SECS (t);

  /* If IMG doesn't have a pixmap yet, load it now, using the image
     type dependent loader function.  */
  if (img->pixmap == None && !img->load_failed_p)
    img->load_failed_p = img->type->load (f, img) == 0;
}
     

/* Value is the number of pixels for the ascent of image IMG when
   drawn in face FACE.  */

int
image_ascent (img, face)
     struct image *img;
     struct face *face;
{
  int height = img->height + img->vmargin;
  int ascent;

  if (img->ascent == CENTERED_IMAGE_ASCENT)
    {
      if (face->font)
	/* This expression is arranged so that if the image can't be
	   exactly centered, it will be moved slightly up.  This is
	   because a typical font is `top-heavy' (due to the presence
	   uppercase letters), so the image placement should err towards
	   being top-heavy too.  It also just generally looks better.  */
	ascent = (height + face->font->ascent - face->font->descent + 1) / 2;
      else
	ascent = height / 2;
    }
  else
    ascent = height * img->ascent / 100.0;

  return ascent;
}


/* Image background colors.  */

static unsigned long
four_corners_best (ximg, width, height)
     XImage *ximg;
     unsigned long width, height;
{
  unsigned long corners[4], best;
  int i, best_count;

  /* Get the colors at the corners of ximg.  */
  corners[0] = XGetPixel (ximg, 0, 0);
  corners[1] = XGetPixel (ximg, width - 1, 0);
  corners[2] = XGetPixel (ximg, width - 1, height - 1);
  corners[3] = XGetPixel (ximg, 0, height - 1);

  /* Choose the most frequently found color as background.  */
  for (i = best_count = 0; i < 4; ++i)
    {
      int j, n;
	  
      for (j = n = 0; j < 4; ++j)
	if (corners[i] == corners[j])
	  ++n;

      if (n > best_count)
	best = corners[i], best_count = n;
    }

  return best;
}

/* Return the `background' field of IMG.  If IMG doesn't have one yet,
   it is guessed heuristically.  If non-zero, XIMG is an existing XImage
   object to use for the heuristic.  */

unsigned long
image_background (img, f, ximg)
     struct image *img;
     struct frame *f;
     XImage *ximg;
{
  if (! img->background_valid)
    /* IMG doesn't have a background yet, try to guess a reasonable value.  */
    {
      int free_ximg = !ximg;

      if (! ximg)
	ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
			  0, 0, img->width, img->height, ~0, ZPixmap);

      img->background = four_corners_best (ximg, img->width, img->height);

      if (free_ximg)
	XDestroyImage (ximg);

      img->background_valid = 1;
    }

  return img->background;
}

/* Return the `background_transparent' field of IMG.  If IMG doesn't
   have one yet, it is guessed heuristically.  If non-zero, MASK is an
   existing XImage object to use for the heuristic.  */

int
image_background_transparent (img, f, mask)
     struct image *img;
     struct frame *f;
     XImage *mask;
{
  if (! img->background_transparent_valid)
    /* IMG doesn't have a background yet, try to guess a reasonable value.  */
    {
      if (img->mask)
	{
	  int free_mask = !mask;

	  if (! mask)
	    mask = XGetImage (FRAME_X_DISPLAY (f), img->mask,
			      0, 0, img->width, img->height, ~0, ZPixmap);

	  img->background_transparent
	    = !four_corners_best (mask, img->width, img->height);

	  if (free_mask)
	    XDestroyImage (mask);
	}
      else
	img->background_transparent = 0;

      img->background_transparent_valid = 1;
    }

  return img->background_transparent;
}


/***********************************************************************
		  Helper functions for X image types
 ***********************************************************************/

static void x_clear_image_1 P_ ((struct frame *, struct image *, int,
				 int, int));
static void x_clear_image P_ ((struct frame *f, struct image *img));
static unsigned long x_alloc_image_color P_ ((struct frame *f,
					      struct image *img,
					      Lisp_Object color_name,
					      unsigned long dflt));


/* Clear X resources of image IMG on frame F.  PIXMAP_P non-zero means
   free the pixmap if any.  MASK_P non-zero means clear the mask
   pixmap if any.  COLORS_P non-zero means free colors allocated for
   the image, if any.  */

static void
x_clear_image_1 (f, img, pixmap_p, mask_p, colors_p)
     struct frame *f;
     struct image *img;
     int pixmap_p, mask_p, colors_p;
{
  if (pixmap_p && img->pixmap)
    {
      XFreePixmap (FRAME_X_DISPLAY (f), img->pixmap);
      img->pixmap = None;
      img->background_valid = 0;
    }

  if (mask_p && img->mask)
    {
      XFreePixmap (FRAME_X_DISPLAY (f), img->mask);
      img->mask = None;
      img->background_transparent_valid = 0;
    }
      
  if (colors_p && img->ncolors)
    {
      x_free_colors (f, img->colors, img->ncolors);
      xfree (img->colors);
      img->colors = NULL;
      img->ncolors = 0;
    }
}

/* Free X resources of image IMG which is used on frame F.  */

static void
x_clear_image (f, img)
     struct frame *f;
     struct image *img;
{
  BLOCK_INPUT;
  x_clear_image_1 (f, img, 1, 1, 1);
  UNBLOCK_INPUT;
}


/* Allocate color COLOR_NAME for image IMG on frame F.  If color
   cannot be allocated, use DFLT.  Add a newly allocated color to
   IMG->colors, so that it can be freed again.  Value is the pixel
   color.  */

static unsigned long
x_alloc_image_color (f, img, color_name, dflt)
     struct frame *f;
     struct image *img;
     Lisp_Object color_name;
     unsigned long dflt;
{
  XColor color;
  unsigned long result;

  xassert (STRINGP (color_name));

  if (x_defined_color (f, XSTRING (color_name)->data, &color, 1))
    {
      /* This isn't called frequently so we get away with simply
	 reallocating the color vector to the needed size, here.  */
      ++img->ncolors;
      img->colors =
	(unsigned long *) xrealloc (img->colors,
				    img->ncolors * sizeof *img->colors);
      img->colors[img->ncolors - 1] = color.pixel;
      result = color.pixel;
    }
  else
    result = dflt;

  return result;
}



/***********************************************************************
			     Image Cache
 ***********************************************************************/

static void cache_image P_ ((struct frame *f, struct image *img));
static void postprocess_image P_ ((struct frame *, struct image *));


/* Return a new, initialized image cache that is allocated from the
   heap.  Call free_image_cache to free an image cache.  */

struct image_cache *
make_image_cache ()
{
  struct image_cache *c = (struct image_cache *) xmalloc (sizeof *c);
  int size;
  
  bzero (c, sizeof *c);
  c->size = 50;
  c->images = (struct image **) xmalloc (c->size * sizeof *c->images);
  size = IMAGE_CACHE_BUCKETS_SIZE * sizeof *c->buckets;
  c->buckets = (struct image **) xmalloc (size);
  bzero (c->buckets, size);
  return c;
}


/* Free image cache of frame F.  Be aware that X frames share images
   caches.  */

void
free_image_cache (f)
     struct frame *f;
{
  struct image_cache *c = FRAME_X_IMAGE_CACHE (f);
  if (c)
    {
      int i;

      /* Cache should not be referenced by any frame when freed.  */
      xassert (c->refcount == 0);
      
      for (i = 0; i < c->used; ++i)
	free_image (f, c->images[i]);
      xfree (c->images);
      xfree (c->buckets);
      xfree (c);
      FRAME_X_IMAGE_CACHE (f) = NULL;
    }
}


/* Clear image cache of frame F.  FORCE_P non-zero means free all
   images.  FORCE_P zero means clear only images that haven't been
   displayed for some time.  Should be called from time to time to
   reduce the number of loaded images.  If image-eviction-seconds is
   non-nil, this frees images in the cache which weren't displayed for
   at least that many seconds.  */

void
clear_image_cache (f, force_p)
     struct frame *f;
     int force_p;
{
  struct image_cache *c = FRAME_X_IMAGE_CACHE (f);

  if (c && INTEGERP (Vimage_cache_eviction_delay))
    {
      EMACS_TIME t;
      unsigned long old;
      int i, nfreed;

      EMACS_GET_TIME (t);
      old = EMACS_SECS (t) - XFASTINT (Vimage_cache_eviction_delay);

      /* Block input so that we won't be interrupted by a SIGIO
	 while being in an inconsistent state.  */
      BLOCK_INPUT;
      
      for (i = nfreed = 0; i < c->used; ++i)
	{
	  struct image *img = c->images[i];
	  if (img != NULL
	      && (force_p || img->timestamp < old))
	    {
	      free_image (f, img);
	      ++nfreed;
	    }
	}

      /* We may be clearing the image cache because, for example,
	 Emacs was iconified for a longer period of time.  In that
	 case, current matrices may still contain references to
	 images freed above.  So, clear these matrices.  */
      if (nfreed)
	{
	  Lisp_Object tail, frame;
	  
	  FOR_EACH_FRAME (tail, frame)
	    {
	      struct frame *f = XFRAME (frame);
	      if (FRAME_X_P (f)
		  && FRAME_X_IMAGE_CACHE (f) == c)
		clear_current_matrices (f);
	    }

	  ++windows_or_buffers_changed;
	}

      UNBLOCK_INPUT;
    }
}


DEFUN ("clear-image-cache", Fclear_image_cache, Sclear_image_cache,
       0, 1, 0,
       doc: /* Clear the image cache of FRAME.
FRAME nil or omitted means use the selected frame.
FRAME t means clear the image caches of all frames.  */)
     (frame)
     Lisp_Object frame;
{
  if (EQ (frame, Qt))
    {
      Lisp_Object tail;
      
      FOR_EACH_FRAME (tail, frame)
	if (FRAME_X_P (XFRAME (frame)))
	  clear_image_cache (XFRAME (frame), 1);
    }
  else
    clear_image_cache (check_x_frame (frame), 1);

  return Qnil;
}


/* Compute masks and transform image IMG on frame F, as specified
   by the image's specification,  */

static void
postprocess_image (f, img)
     struct frame *f;
     struct image *img;
{
  /* Manipulation of the image's mask.  */
  if (img->pixmap)
    {
      Lisp_Object conversion, spec;
      Lisp_Object mask;

      spec = img->spec;
      
      /* `:heuristic-mask t'
	 `:mask heuristic'
	 means build a mask heuristically.
	 `:heuristic-mask (R G B)'
	 `:mask (heuristic (R G B))'
	 means build a mask from color (R G B) in the
	 image.
	 `:mask nil'
	 means remove a mask, if any.  */
	      
      mask = image_spec_value (spec, QCheuristic_mask, NULL);
      if (!NILP (mask))
	x_build_heuristic_mask (f, img, mask);
      else
	{
	  int found_p;
		    
	  mask = image_spec_value (spec, QCmask, &found_p);
		  
	  if (EQ (mask, Qheuristic))
	    x_build_heuristic_mask (f, img, Qt);
	  else if (CONSP (mask)
		   && EQ (XCAR (mask), Qheuristic))
	    {
	      if (CONSP (XCDR (mask)))
		x_build_heuristic_mask (f, img, XCAR (XCDR (mask)));
	      else
		x_build_heuristic_mask (f, img, XCDR (mask));
	    }
	  else if (NILP (mask) && found_p && img->mask)
	    {
	      XFreePixmap (FRAME_X_DISPLAY (f), img->mask);
	      img->mask = None;
	    }
	}
 
	  
      /* Should we apply an image transformation algorithm?  */
      conversion = image_spec_value (spec, QCconversion, NULL);
      if (EQ (conversion, Qdisabled))
	x_disable_image (f, img);
      else if (EQ (conversion, Qlaplace))
	x_laplace (f, img);
      else if (EQ (conversion, Qemboss))
	x_emboss (f, img);
      else if (CONSP (conversion)
	       && EQ (XCAR (conversion), Qedge_detection))
	{
	  Lisp_Object tem;
	  tem = XCDR (conversion);
	  if (CONSP (tem))
	    x_edge_detection (f, img,
			      Fplist_get (tem, QCmatrix),
			      Fplist_get (tem, QCcolor_adjustment));
	}
    }
}


/* Return the id of image with Lisp specification SPEC on frame F.
   SPEC must be a valid Lisp image specification (see valid_image_p).  */

int
lookup_image (f, spec)
     struct frame *f;
     Lisp_Object spec;
{
  struct image_cache *c = FRAME_X_IMAGE_CACHE (f);
  struct image *img;
  int i;
  unsigned hash;
  struct gcpro gcpro1;
  EMACS_TIME now;

  /* F must be a window-system frame, and SPEC must be a valid image
     specification.  */
  xassert (FRAME_WINDOW_P (f));
  xassert (valid_image_p (spec));
  
  GCPRO1 (spec);

  /* Look up SPEC in the hash table of the image cache.  */
  hash = sxhash (spec, 0);
  i = hash % IMAGE_CACHE_BUCKETS_SIZE;

  for (img = c->buckets[i]; img; img = img->next)
    if (img->hash == hash && !NILP (Fequal (img->spec, spec)))
      break;

  /* If not found, create a new image and cache it.  */
  if (img == NULL)
    {
      extern Lisp_Object Qpostscript;
      
      BLOCK_INPUT;
      img = make_image (spec, hash);
      cache_image (f, img);
      img->load_failed_p = img->type->load (f, img) == 0;

      /* If we can't load the image, and we don't have a width and
	 height, use some arbitrary width and height so that we can
	 draw a rectangle for it.  */
      if (img->load_failed_p)
	{
	  Lisp_Object value;

	  value = image_spec_value (spec, QCwidth, NULL);
	  img->width = (INTEGERP (value)
			? XFASTINT (value) : DEFAULT_IMAGE_WIDTH);
	  value = image_spec_value (spec, QCheight, NULL);
	  img->height = (INTEGERP (value)
			 ? XFASTINT (value) : DEFAULT_IMAGE_HEIGHT);
	}
      else
	{
	  /* Handle image type independent image attributes
	     `:ascent ASCENT', `:margin MARGIN', `:relief RELIEF',
	     `:background COLOR'.  */
	  Lisp_Object ascent, margin, relief, bg;

	  ascent = image_spec_value (spec, QCascent, NULL);
	  if (INTEGERP (ascent))
	    img->ascent = XFASTINT (ascent);
	  else if (EQ (ascent, Qcenter))
	    img->ascent = CENTERED_IMAGE_ASCENT;
	  
	  margin = image_spec_value (spec, QCmargin, NULL);
	  if (INTEGERP (margin) && XINT (margin) >= 0)
	    img->vmargin = img->hmargin = XFASTINT (margin);
	  else if (CONSP (margin) && INTEGERP (XCAR (margin))
		   && INTEGERP (XCDR (margin)))
	    {
	      if (XINT (XCAR (margin)) > 0)
		img->hmargin = XFASTINT (XCAR (margin));
	      if (XINT (XCDR (margin)) > 0)
		img->vmargin = XFASTINT (XCDR (margin));
	    }
	  
	  relief = image_spec_value (spec, QCrelief, NULL);
	  if (INTEGERP (relief))
	    {
	      img->relief = XINT (relief);
	      img->hmargin += abs (img->relief);
	      img->vmargin += abs (img->relief);
	    }

	  if (! img->background_valid)
	    {
	      bg = image_spec_value (img->spec, QCbackground, NULL);
	      if (!NILP (bg))
		{
		  img->background
		    = x_alloc_image_color (f, img, bg,
					   FRAME_BACKGROUND_PIXEL (f));
		  img->background_valid = 1;
		}
	    }

	  /* Do image transformations and compute masks, unless we
	     don't have the image yet.  */
	  if (!EQ (*img->type->type, Qpostscript))
	    postprocess_image (f, img);
	}

      UNBLOCK_INPUT;
      xassert (!interrupt_input_blocked);
    }

  /* We're using IMG, so set its timestamp to `now'.  */
  EMACS_GET_TIME (now);
  img->timestamp = EMACS_SECS (now);
  
  UNGCPRO;
  
  /* Value is the image id.  */
  return img->id;
}


/* Cache image IMG in the image cache of frame F.  */

static void
cache_image (f, img)
     struct frame *f;
     struct image *img;
{
  struct image_cache *c = FRAME_X_IMAGE_CACHE (f);
  int i;

  /* Find a free slot in c->images.  */
  for (i = 0; i < c->used; ++i)
    if (c->images[i] == NULL)
      break;

  /* If no free slot found, maybe enlarge c->images.  */
  if (i == c->used && c->used == c->size)
    {
      c->size *= 2;
      c->images = (struct image **) xrealloc (c->images,
					      c->size * sizeof *c->images);
    }

  /* Add IMG to c->images, and assign IMG an id.  */
  c->images[i] = img;
  img->id = i;
  if (i == c->used)
    ++c->used;

  /* Add IMG to the cache's hash table.  */
  i = img->hash % IMAGE_CACHE_BUCKETS_SIZE;
  img->next = c->buckets[i];
  if (img->next)
    img->next->prev = img;
  img->prev = NULL;
  c->buckets[i] = img;
}


/* Call FN on every image in the image cache of frame F.  Used to mark
   Lisp Objects in the image cache.  */

void
forall_images_in_image_cache (f, fn)
     struct frame *f;
     void (*fn) P_ ((struct image *img));
{
  if (FRAME_LIVE_P (f) && FRAME_X_P (f))
    {
      struct image_cache *c = FRAME_X_IMAGE_CACHE (f);
      if (c)
	{
	  int i;
	  for (i = 0; i < c->used; ++i)
	    if (c->images[i])
	      fn (c->images[i]);
	}
    }
}



/***********************************************************************
			    X support code
 ***********************************************************************/

static int x_create_x_image_and_pixmap P_ ((struct frame *, int, int, int,
					    XImage **, Pixmap *));
static void x_destroy_x_image P_ ((XImage *));
static void x_put_x_image P_ ((struct frame *, XImage *, Pixmap, int, int));


/* Create an XImage and a pixmap of size WIDTH x HEIGHT for use on
   frame F.  Set *XIMG and *PIXMAP to the XImage and Pixmap created.
   Set (*XIMG)->data to a raster of WIDTH x HEIGHT pixels allocated
   via xmalloc.  Print error messages via image_error if an error
   occurs.  Value is non-zero if successful.  */

static int
x_create_x_image_and_pixmap (f, width, height, depth, ximg, pixmap)
     struct frame *f;
     int width, height, depth;
     XImage **ximg;
     Pixmap *pixmap;
{
  Display *display = FRAME_X_DISPLAY (f);
  Screen *screen = FRAME_X_SCREEN (f);
  Window window = FRAME_X_WINDOW (f);

  xassert (interrupt_input_blocked);

  if (depth <= 0)
    depth = DefaultDepthOfScreen (screen);
  *ximg = XCreateImage (display, DefaultVisualOfScreen (screen),
			depth, ZPixmap, 0, NULL, width, height,
			depth > 16 ? 32 : depth > 8 ? 16 : 8, 0);
  if (*ximg == NULL)
    {
      image_error ("Unable to allocate X image", Qnil, Qnil);
      return 0;
    }

  /* Allocate image raster.  */
  (*ximg)->data = (char *) xmalloc ((*ximg)->bytes_per_line * height);

  /* Allocate a pixmap of the same size.  */
  *pixmap = XCreatePixmap (display, window, width, height, depth);
  if (*pixmap == None)
    {
      x_destroy_x_image (*ximg);
      *ximg = NULL;
      image_error ("Unable to create X pixmap", Qnil, Qnil);
      return 0;
    }

  return 1;
}


/* Destroy XImage XIMG.  Free XIMG->data.  */

static void
x_destroy_x_image (ximg)
     XImage *ximg;
{
  xassert (interrupt_input_blocked);
  if (ximg)
    {
      xfree (ximg->data);
      ximg->data = NULL;
      XDestroyImage (ximg);
    }
}


/* Put XImage XIMG into pixmap PIXMAP on frame F.  WIDTH and HEIGHT
   are width and height of both the image and pixmap.  */

static void
x_put_x_image (f, ximg, pixmap, width, height)
     struct frame *f;
     XImage *ximg;
     Pixmap pixmap;
{
  GC gc;
  
  xassert (interrupt_input_blocked);
  gc = XCreateGC (FRAME_X_DISPLAY (f), pixmap, 0, NULL);
  XPutImage (FRAME_X_DISPLAY (f), pixmap, gc, ximg, 0, 0, 0, 0, width, height);
  XFreeGC (FRAME_X_DISPLAY (f), gc);
}



/***********************************************************************
			      File Handling
 ***********************************************************************/

static Lisp_Object x_find_image_file P_ ((Lisp_Object));
static char *slurp_file P_ ((char *, int *));


/* Find image file FILE.  Look in data-directory, then
   x-bitmap-file-path.  Value is the full name of the file found, or
   nil if not found.  */

static Lisp_Object
x_find_image_file (file)
     Lisp_Object file;
{
  Lisp_Object file_found, search_path;
  struct gcpro gcpro1, gcpro2;
  int fd;

  file_found = Qnil;
  search_path = Fcons (Vdata_directory, Vx_bitmap_file_path);
  GCPRO2 (file_found, search_path);

  /* Try to find FILE in data-directory, then x-bitmap-file-path.  */
  fd = openp (search_path, file, Qnil, &file_found, 0);
  
  if (fd == -1)
    file_found = Qnil;
  else
    close (fd);

  UNGCPRO;
  return file_found;
}


/* Read FILE into memory.  Value is a pointer to a buffer allocated
   with xmalloc holding FILE's contents.  Value is null if an error
   occurred.  *SIZE is set to the size of the file.  */

static char *
slurp_file (file, size)
     char *file;
     int *size;
{
  FILE *fp = NULL;
  char *buf = NULL;
  struct stat st;

  if (stat (file, &st) == 0
      && (fp = fopen (file, "r")) != NULL
      && (buf = (char *) xmalloc (st.st_size),
	  fread (buf, 1, st.st_size, fp) == st.st_size))
    {
      *size = st.st_size;
      fclose (fp);
    }
  else
    {
      if (fp)
	fclose (fp);
      if (buf)
	{
	  xfree (buf);
	  buf = NULL;
	}
    }
  
  return buf;
}



/***********************************************************************
			      XBM images
 ***********************************************************************/

static int xbm_scan P_ ((char **, char *, char *, int *));
static int xbm_load P_ ((struct frame *f, struct image *img));
static int xbm_load_image P_ ((struct frame *f, struct image *img,
			       char *, char *));
static int xbm_image_p P_ ((Lisp_Object object));
static int xbm_read_bitmap_data P_ ((char *, char *, int *, int *,
				     unsigned char **));
static int xbm_file_p P_ ((Lisp_Object));


/* Indices of image specification fields in xbm_format, below.  */

enum xbm_keyword_index
{
  XBM_TYPE,
  XBM_FILE,
  XBM_WIDTH,
  XBM_HEIGHT,
  XBM_DATA,
  XBM_FOREGROUND,
  XBM_BACKGROUND,
  XBM_ASCENT,
  XBM_MARGIN,
  XBM_RELIEF,
  XBM_ALGORITHM,
  XBM_HEURISTIC_MASK,
  XBM_MASK,
  XBM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid XBM image specifications.  */

static struct image_keyword xbm_format[XBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":width",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":height",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":data",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,   0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0}
};

/* Structure describing the image type XBM.  */

static struct image_type xbm_type =
{
  &Qxbm,
  xbm_image_p,
  xbm_load,
  x_clear_image,
  NULL
};

/* Tokens returned from xbm_scan.  */

enum xbm_token
{
  XBM_TK_IDENT = 256,
  XBM_TK_NUMBER
};

  
/* Return non-zero if OBJECT is a valid XBM-type image specification.
   A valid specification is a list starting with the symbol `image'
   The rest of the list is a property list which must contain an
   entry `:type xbm..

   If the specification specifies a file to load, it must contain
   an entry `:file FILENAME' where FILENAME is a string.

   If the specification is for a bitmap loaded from memory it must
   contain `:width WIDTH', `:height HEIGHT', and `:data DATA', where
   WIDTH and HEIGHT are integers > 0.  DATA may be:

   1. a string large enough to hold the bitmap data, i.e. it must
   have a size >= (WIDTH + 7) / 8 * HEIGHT

   2. a bool-vector of size >= WIDTH * HEIGHT

   3. a vector of strings or bool-vectors, one for each line of the
   bitmap.

   4. A string containing an in-memory XBM file.  WIDTH and HEIGHT
   may not be specified in this case because they are defined in the
   XBM file.

   Both the file and data forms may contain the additional entries
   `:background COLOR' and `:foreground COLOR'.  If not present,
   foreground and background of the frame on which the image is
   displayed is used.  */

static int
xbm_image_p (object)
     Lisp_Object object;
{
  struct image_keyword kw[XBM_LAST];
  
  bcopy (xbm_format, kw, sizeof kw);
  if (!parse_image_spec (object, kw, XBM_LAST, Qxbm))
    return 0;

  xassert (EQ (kw[XBM_TYPE].value, Qxbm));

  if (kw[XBM_FILE].count)
    {
      if (kw[XBM_WIDTH].count || kw[XBM_HEIGHT].count || kw[XBM_DATA].count)
	return 0;
    }
  else if (kw[XBM_DATA].count && xbm_file_p (kw[XBM_DATA].value))
    {
      /* In-memory XBM file.  */
      if (kw[XBM_WIDTH].count || kw[XBM_HEIGHT].count || kw[XBM_FILE].count)
	return 0;
    }
  else
    {
      Lisp_Object data;
      int width, height;

      /* Entries for `:width', `:height' and `:data' must be present.  */
      if (!kw[XBM_WIDTH].count
	  || !kw[XBM_HEIGHT].count
	  || !kw[XBM_DATA].count)
	return 0;

      data = kw[XBM_DATA].value;
      width = XFASTINT (kw[XBM_WIDTH].value);
      height = XFASTINT (kw[XBM_HEIGHT].value);
      
      /* Check type of data, and width and height against contents of
	 data.  */
      if (VECTORP (data))
	{
	  int i;
	  
	  /* Number of elements of the vector must be >= height.  */
	  if (XVECTOR (data)->size < height)
	    return 0;

	  /* Each string or bool-vector in data must be large enough
	     for one line of the image.  */
	  for (i = 0; i < height; ++i)
	    {
	      Lisp_Object elt = XVECTOR (data)->contents[i];

	      if (STRINGP (elt))
		{
		  if (XSTRING (elt)->size
		      < (width + BITS_PER_CHAR - 1) / BITS_PER_CHAR)
		    return 0;
		}
	      else if (BOOL_VECTOR_P (elt))
		{
		  if (XBOOL_VECTOR (elt)->size < width)
		    return 0;
		}
	      else
		return 0;
	    }
	}
      else if (STRINGP (data))
	{
	  if (XSTRING (data)->size
	      < (width + BITS_PER_CHAR - 1) / BITS_PER_CHAR * height)
	    return 0;
	}
      else if (BOOL_VECTOR_P (data))
	{
	  if (XBOOL_VECTOR (data)->size < width * height)
	    return 0;
	}
      else
	return 0;
    }

  return 1;
}


/* Scan a bitmap file.  FP is the stream to read from.  Value is
   either an enumerator from enum xbm_token, or a character for a
   single-character token, or 0 at end of file.  If scanning an
   identifier, store the lexeme of the identifier in SVAL.  If
   scanning a number, store its value in *IVAL.  */

static int
xbm_scan (s, end, sval, ival)
     char **s, *end;
     char *sval;
     int *ival;
{
  int c;

 loop:
  
  /* Skip white space.  */
  while (*s < end && (c = *(*s)++, isspace (c)))
    ;

  if (*s >= end)
    c = 0;
  else if (isdigit (c))
    {
      int value = 0, digit;
      
      if (c == '0' && *s < end)
	{
	  c = *(*s)++;
	  if (c == 'x' || c == 'X')
	    {
	      while (*s < end)
		{
		  c = *(*s)++;
		  if (isdigit (c))
		    digit = c - '0';
		  else if (c >= 'a' && c <= 'f')
		    digit = c - 'a' + 10;
		  else if (c >= 'A' && c <= 'F')
		    digit = c - 'A' + 10;
		  else
		    break;
		  value = 16 * value + digit;
		}
	    }
	  else if (isdigit (c))
	    {
	      value = c - '0';
	      while (*s < end
		     && (c = *(*s)++, isdigit (c)))
		value = 8 * value + c - '0';
	    }
	}
      else
	{
	  value = c - '0';
	  while (*s < end
		 && (c = *(*s)++, isdigit (c)))
	    value = 10 * value + c - '0';
	}

      if (*s < end)
	*s = *s - 1;
      *ival = value;
      c = XBM_TK_NUMBER;
    }
  else if (isalpha (c) || c == '_')
    {
      *sval++ = c;
      while (*s < end
	     && (c = *(*s)++, (isalnum (c) || c == '_')))
	*sval++ = c;
      *sval = 0;
      if (*s < end)
	*s = *s - 1;
      c = XBM_TK_IDENT;
    }
  else if (c == '/' && **s == '*')
    {
      /* C-style comment.  */
      ++*s;
      while (**s && (**s != '*' || *(*s + 1) != '/'))
	++*s;
      if (**s)
	{
	  *s += 2;
	  goto loop;
	}
    }

  return c;
}


/* Replacement for XReadBitmapFileData which isn't available under old
   X versions.  CONTENTS is a pointer to a buffer to parse; END is the
   buffer's end.  Set *WIDTH and *HEIGHT to the width and height of
   the image.  Return in *DATA the bitmap data allocated with xmalloc.
   Value is non-zero if successful.  DATA null means just test if
   CONTENTS looks like an in-memory XBM file.  */

static int
xbm_read_bitmap_data (contents, end, width, height, data)
     char *contents, *end;
     int *width, *height;
     unsigned char **data;
{
  char *s = contents;
  char buffer[BUFSIZ];
  int padding_p = 0;
  int v10 = 0;
  int bytes_per_line, i, nbytes;
  unsigned char *p;
  int value;
  int LA1;

#define match() \
     LA1 = xbm_scan (&s, end, buffer, &value)

#define expect(TOKEN)		\
     if (LA1 != (TOKEN)) 	\
       goto failure;		\
     else			\
       match ()	

#define expect_ident(IDENT)					\
     if (LA1 == XBM_TK_IDENT && strcmp (buffer, (IDENT)) == 0)	\
       match ();						\
     else							\
       goto failure

  *width = *height = -1;
  if (data)
    *data = NULL;
  LA1 = xbm_scan (&s, end, buffer, &value);

  /* Parse defines for width, height and hot-spots.  */
  while (LA1 == '#')
    {
      match ();
      expect_ident ("define");
      expect (XBM_TK_IDENT);

      if (LA1 == XBM_TK_NUMBER);
	{
          char *p = strrchr (buffer, '_');
	  p = p ? p + 1 : buffer;
          if (strcmp (p, "width") == 0)
	    *width = value;
          else if (strcmp (p, "height") == 0)
	    *height = value;
	}
      expect (XBM_TK_NUMBER);
    }

  if (*width < 0 || *height < 0)
    goto failure;
  else if (data == NULL)
    goto success;

  /* Parse bits.  Must start with `static'.  */
  expect_ident ("static");
  if (LA1 == XBM_TK_IDENT)
    {
      if (strcmp (buffer, "unsigned") == 0)
	{
	  match (); 
	  expect_ident ("char");
	}
      else if (strcmp (buffer, "short") == 0)
	{
	  match ();
	  v10 = 1;
	  if (*width % 16 && *width % 16 < 9)
	    padding_p = 1;
	}
      else if (strcmp (buffer, "char") == 0)
	match ();
      else
	goto failure;
    }
  else 
    goto failure;

  expect (XBM_TK_IDENT);
  expect ('[');
  expect (']');
  expect ('=');
  expect ('{');

  bytes_per_line = (*width + 7) / 8 + padding_p;
  nbytes = bytes_per_line * *height;
  p = *data = (char *) xmalloc (nbytes);

  if (v10)
    {
      for (i = 0; i < nbytes; i += 2)
	{
	  int val = value;
	  expect (XBM_TK_NUMBER);

	  *p++ = val;
	  if (!padding_p || ((i + 2) % bytes_per_line))
	    *p++ = value >> 8;
	  
	  if (LA1 == ',' || LA1 == '}')
	    match ();
	  else
	    goto failure;
	}
    }
  else
    {
      for (i = 0; i < nbytes; ++i)
	{
	  int val = value;
	  expect (XBM_TK_NUMBER);
	  
	  *p++ = val;
	  
	  if (LA1 == ',' || LA1 == '}')
	    match ();
	  else
	    goto failure;
	}
    }

 success:
  return 1;

 failure:
  
  if (data && *data)
    {
      xfree (*data);
      *data = NULL;
    }
  return 0;

#undef match
#undef expect
#undef expect_ident
}


/* Load XBM image IMG which will be displayed on frame F from buffer
   CONTENTS.  END is the end of the buffer.  Value is non-zero if
   successful.  */

static int
xbm_load_image (f, img, contents, end)
     struct frame *f;
     struct image *img;
     char *contents, *end;
{
  int rc;
  unsigned char *data;
  int success_p = 0;
  
  rc = xbm_read_bitmap_data (contents, end, &img->width, &img->height, &data);
  if (rc)
    {
      int depth = DefaultDepthOfScreen (FRAME_X_SCREEN (f));
      unsigned long foreground = FRAME_FOREGROUND_PIXEL (f);
      unsigned long background = FRAME_BACKGROUND_PIXEL (f);
      Lisp_Object value;
      
      xassert (img->width > 0 && img->height > 0);

      /* Get foreground and background colors, maybe allocate colors.  */
      value = image_spec_value (img->spec, QCforeground, NULL);
      if (!NILP (value))
	foreground = x_alloc_image_color (f, img, value, foreground);
      value = image_spec_value (img->spec, QCbackground, NULL);
      if (!NILP (value))
	{
	  background = x_alloc_image_color (f, img, value, background);
	  img->background = background;
	  img->background_valid = 1;
	}

      img->pixmap
	= XCreatePixmapFromBitmapData (FRAME_X_DISPLAY (f),
				       FRAME_X_WINDOW (f),
				       data,
				       img->width, img->height,
				       foreground, background,
				       depth);
      xfree (data);

      if (img->pixmap == None)
	{
	  x_clear_image (f, img);
	  image_error ("Unable to create X pixmap for `%s'", img->spec, Qnil);
	}
      else
	success_p = 1;
    }
  else
    image_error ("Error loading XBM image `%s'", img->spec, Qnil);

  return success_p;
}


/* Value is non-zero if DATA looks like an in-memory XBM file.  */

static int
xbm_file_p (data)
     Lisp_Object data;
{
  int w, h;
  return (STRINGP (data)
	  && xbm_read_bitmap_data (XSTRING (data)->data,
				   (XSTRING (data)->data
				    + STRING_BYTES (XSTRING (data))),
				   &w, &h, NULL));
}

    
/* Fill image IMG which is used on frame F with pixmap data.  Value is
   non-zero if successful.  */

static int
xbm_load (f, img)
     struct frame *f;
     struct image *img;
{
  int success_p = 0;
  Lisp_Object file_name;

  xassert (xbm_image_p (img->spec));

  /* If IMG->spec specifies a file name, create a non-file spec from it.  */
  file_name = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (file_name))
    {
      Lisp_Object file;
      char *contents;
      int size;
      struct gcpro gcpro1;
      
      file = x_find_image_file (file_name);
      GCPRO1 (file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file_name, Qnil);
	  UNGCPRO;
	  return 0;
	}

      contents = slurp_file (XSTRING (file)->data, &size);
      if (contents == NULL)
	{
	  image_error ("Error loading XBM image `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}

      success_p = xbm_load_image (f, img, contents, contents + size);
      UNGCPRO;
    }
  else
    {
      struct image_keyword fmt[XBM_LAST];
      Lisp_Object data;
      int depth;
      unsigned long foreground = FRAME_FOREGROUND_PIXEL (f);
      unsigned long background = FRAME_BACKGROUND_PIXEL (f);
      char *bits;
      int parsed_p;
      int in_memory_file_p = 0;

      /* See if data looks like an in-memory XBM file.  */
      data = image_spec_value (img->spec, QCdata, NULL);
      in_memory_file_p = xbm_file_p (data);

      /* Parse the image specification.  */
      bcopy (xbm_format, fmt, sizeof fmt);
      parsed_p = parse_image_spec (img->spec, fmt, XBM_LAST, Qxbm);
      xassert (parsed_p);

      /* Get specified width, and height.  */
      if (!in_memory_file_p)
	{
	  img->width = XFASTINT (fmt[XBM_WIDTH].value);
	  img->height = XFASTINT (fmt[XBM_HEIGHT].value);
	  xassert (img->width > 0 && img->height > 0);
	}

      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[XBM_FOREGROUND].count
	  && STRINGP (fmt[XBM_FOREGROUND].value))
	foreground = x_alloc_image_color (f, img, fmt[XBM_FOREGROUND].value,
					  foreground);
      if (fmt[XBM_BACKGROUND].count
	  && STRINGP (fmt[XBM_BACKGROUND].value))
	background = x_alloc_image_color (f, img, fmt[XBM_BACKGROUND].value,
					  background);

      if (in_memory_file_p)
	success_p = xbm_load_image (f, img, XSTRING (data)->data,
				    (XSTRING (data)->data
				     + STRING_BYTES (XSTRING (data))));
      else
	{
	  if (VECTORP (data))
	    {
	      int i;
	      char *p;
	      int nbytes = (img->width + BITS_PER_CHAR - 1) / BITS_PER_CHAR;
	  
	      p = bits = (char *) alloca (nbytes * img->height);
	      for (i = 0; i < img->height; ++i, p += nbytes)
		{
		  Lisp_Object line = XVECTOR (data)->contents[i];
		  if (STRINGP (line))
		    bcopy (XSTRING (line)->data, p, nbytes);
		  else
		    bcopy (XBOOL_VECTOR (line)->data, p, nbytes);
		}
	    }
	  else if (STRINGP (data))
	    bits = XSTRING (data)->data;
	  else
	    bits = XBOOL_VECTOR (data)->data;

	  /* Create the pixmap.  */
	  depth = DefaultDepthOfScreen (FRAME_X_SCREEN (f));
	  img->pixmap
	    = XCreatePixmapFromBitmapData (FRAME_X_DISPLAY (f),
					   FRAME_X_WINDOW (f),
					   bits,
					   img->width, img->height,
					   foreground, background,
					   depth);
	  if (img->pixmap)
	    success_p = 1;
	  else
	    {
	      image_error ("Unable to create pixmap for XBM image `%s'",
			   img->spec, Qnil);
	      x_clear_image (f, img);
	    }
	}
    }

  return success_p;
}
  


/***********************************************************************
			      XPM images
 ***********************************************************************/

#if HAVE_XPM 

static int xpm_image_p P_ ((Lisp_Object object));
static int xpm_load P_ ((struct frame *f, struct image *img));
static int xpm_valid_color_symbols_p P_ ((Lisp_Object));

#include "X11/xpm.h"

/* The symbol `xpm' identifying XPM-format images.  */

Lisp_Object Qxpm;

/* Indices of image specification fields in xpm_format, below.  */

enum xpm_keyword_index
{
  XPM_TYPE,
  XPM_FILE,
  XPM_DATA,
  XPM_ASCENT,
  XPM_MARGIN,
  XPM_RELIEF,
  XPM_ALGORITHM,
  XPM_HEURISTIC_MASK,
  XPM_MASK,
  XPM_COLOR_SYMBOLS,
  XPM_BACKGROUND,
  XPM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid XPM image specifications.  */

static struct image_keyword xpm_format[XPM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":color-symbols",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type XBM.  */

static struct image_type xpm_type =
{
  &Qxpm,
  xpm_image_p,
  xpm_load,
  x_clear_image,
  NULL
};


/* Define ALLOC_XPM_COLORS if we can use Emacs' own color allocation
   functions for allocating image colors.  Our own functions handle
   color allocation failures more gracefully than the ones on the XPM
   lib.  */

#if defined XpmAllocColor && defined XpmFreeColors && defined XpmColorClosure
#define ALLOC_XPM_COLORS
#endif

#ifdef ALLOC_XPM_COLORS

static void xpm_init_color_cache P_ ((struct frame *, XpmAttributes *));
static void xpm_free_color_cache P_ ((void));
static int xpm_lookup_color P_ ((struct frame *, char *, XColor *));
static int xpm_color_bucket P_ ((char *));
static struct xpm_cached_color *xpm_cache_color P_ ((struct frame *, char *,
						     XColor *, int));

/* An entry in a hash table used to cache color definitions of named
   colors.  This cache is necessary to speed up XPM image loading in
   case we do color allocations ourselves.  Without it, we would need
   a call to XParseColor per pixel in the image.  */

struct xpm_cached_color
{
  /* Next in collision chain.  */
  struct xpm_cached_color *next;

  /* Color definition (RGB and pixel color).  */
  XColor color;

  /* Color name.  */
  char name[1];
};

/* The hash table used for the color cache, and its bucket vector
   size.  */

#define XPM_COLOR_CACHE_BUCKETS	1001
struct xpm_cached_color **xpm_color_cache;

/* Initialize the color cache.  */

static void
xpm_init_color_cache (f, attrs)
     struct frame *f;
     XpmAttributes *attrs;
{
  size_t nbytes = XPM_COLOR_CACHE_BUCKETS * sizeof *xpm_color_cache;
  xpm_color_cache = (struct xpm_cached_color **) xmalloc (nbytes);
  memset (xpm_color_cache, 0, nbytes);
  init_color_table ();

  if (attrs->valuemask & XpmColorSymbols)
    {
      int i;
      XColor color;
      
      for (i = 0; i < attrs->numsymbols; ++i)
	if (XParseColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
			 attrs->colorsymbols[i].value, &color))
	  {
	    color.pixel = lookup_rgb_color (f, color.red, color.green,
					    color.blue);
	    xpm_cache_color (f, attrs->colorsymbols[i].name, &color, -1);
	  }
    }
}


/* Free the color cache.  */

static void
xpm_free_color_cache ()
{
  struct xpm_cached_color *p, *next;
  int i;

  for (i = 0; i < XPM_COLOR_CACHE_BUCKETS; ++i)
    for (p = xpm_color_cache[i]; p; p = next)
      {
	next = p->next;
	xfree (p);
      }

  xfree (xpm_color_cache);
  xpm_color_cache = NULL;
  free_color_table ();
}


/* Return the bucket index for color named COLOR_NAME in the color
   cache.  */

static int
xpm_color_bucket (color_name)
     char *color_name;
{
  unsigned h = 0;
  char *s;
  
  for (s = color_name; *s; ++s)
    h = (h << 2) ^ *s;
  return h %= XPM_COLOR_CACHE_BUCKETS;
}


/* On frame F, cache values COLOR for color with name COLOR_NAME.
   BUCKET, if >= 0, is a precomputed bucket index.  Value is the cache
   entry added.  */

static struct xpm_cached_color *
xpm_cache_color (f, color_name, color, bucket)
     struct frame *f;
     char *color_name;
     XColor *color;
     int bucket;
{
  size_t nbytes;
  struct xpm_cached_color *p;
  
  if (bucket < 0)
    bucket = xpm_color_bucket (color_name);
      
  nbytes = sizeof *p + strlen (color_name);
  p = (struct xpm_cached_color *) xmalloc (nbytes);
  strcpy (p->name, color_name);
  p->color = *color;
  p->next = xpm_color_cache[bucket];
  xpm_color_cache[bucket] = p;
  return p;
}


/* Look up color COLOR_NAME for frame F in the color cache.  If found,
   return the cached definition in *COLOR.  Otherwise, make a new
   entry in the cache and allocate the color.  Value is zero if color
   allocation failed.  */

static int
xpm_lookup_color (f, color_name, color)
     struct frame *f;
     char *color_name;
     XColor *color;
{
  struct xpm_cached_color *p;
  int h = xpm_color_bucket (color_name);

  for (p = xpm_color_cache[h]; p; p = p->next)
    if (strcmp (p->name, color_name) == 0)
      break;

  if (p != NULL)
    *color = p->color;
  else if (XParseColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f),
			color_name, color))
    {
      color->pixel = lookup_rgb_color (f, color->red, color->green,
				       color->blue);
      p = xpm_cache_color (f, color_name, color, h);
    }
  
  return p != NULL;
}


/* Callback for allocating color COLOR_NAME.  Called from the XPM lib.
   CLOSURE is a pointer to the frame on which we allocate the
   color.  Return in *COLOR the allocated color.  Value is non-zero
   if successful.  */

static int
xpm_alloc_color (dpy, cmap, color_name, color, closure)
     Display *dpy;
     Colormap cmap;
     char *color_name;
     XColor *color;
     void *closure;
{
  return xpm_lookup_color ((struct frame *) closure, color_name, color);
}


/* Callback for freeing NPIXELS colors contained in PIXELS.  CLOSURE
   is a pointer to the frame on which we allocate the color.  Value is
   non-zero if successful.  */

static int
xpm_free_colors (dpy, cmap, pixels, npixels, closure)
     Display *dpy;
     Colormap cmap;
     Pixel *pixels;
     int npixels;
     void *closure;
{
  return 1;
}

#endif /* ALLOC_XPM_COLORS */


/* Value is non-zero if COLOR_SYMBOLS is a valid color symbols list
   for XPM images.  Such a list must consist of conses whose car and
   cdr are strings.  */

static int
xpm_valid_color_symbols_p (color_symbols)
     Lisp_Object color_symbols;
{
  while (CONSP (color_symbols))
    {
      Lisp_Object sym = XCAR (color_symbols);
      if (!CONSP (sym)
	  || !STRINGP (XCAR (sym))
	  || !STRINGP (XCDR (sym)))
	break;
      color_symbols = XCDR (color_symbols);
    }

  return NILP (color_symbols);
}


/* Value is non-zero if OBJECT is a valid XPM image specification.  */

static int
xpm_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[XPM_LAST];
  bcopy (xpm_format, fmt, sizeof fmt);
  return (parse_image_spec (object, fmt, XPM_LAST, Qxpm)
	  /* Either `:file' or `:data' must be present.  */
	  && fmt[XPM_FILE].count + fmt[XPM_DATA].count == 1
	  /* Either no `:color-symbols' or it's a list of conses
	     whose car and cdr are strings.  */
	  && (fmt[XPM_COLOR_SYMBOLS].count == 0
	      || xpm_valid_color_symbols_p (fmt[XPM_COLOR_SYMBOLS].value)));
}


/* Load image IMG which will be displayed on frame F.  Value is
   non-zero if successful.  */

static int
xpm_load (f, img)
     struct frame *f;
     struct image *img;
{
  int rc;
  XpmAttributes attrs;
  Lisp_Object specified_file, color_symbols;

  /* Configure the XPM lib.  Use the visual of frame F.  Allocate
     close colors.  Return colors allocated.  */
  bzero (&attrs, sizeof attrs);
  attrs.visual = FRAME_X_VISUAL (f);
  attrs.colormap = FRAME_X_COLORMAP (f);
  attrs.valuemask |= XpmVisual;
  attrs.valuemask |= XpmColormap;

#ifdef ALLOC_XPM_COLORS
  /* Allocate colors with our own functions which handle
     failing color allocation more gracefully.  */
  attrs.color_closure = f;
  attrs.alloc_color = xpm_alloc_color;
  attrs.free_colors = xpm_free_colors;
  attrs.valuemask |= XpmAllocColor | XpmFreeColors | XpmColorClosure;
#else /* not ALLOC_XPM_COLORS */
  /* Let the XPM lib allocate colors.  */
  attrs.valuemask |= XpmReturnAllocPixels;
#ifdef XpmAllocCloseColors
  attrs.alloc_close_colors = 1;
  attrs.valuemask |= XpmAllocCloseColors;
#else /* not XpmAllocCloseColors */
  attrs.closeness = 600;
  attrs.valuemask |= XpmCloseness;
#endif /* not XpmAllocCloseColors */
#endif /* ALLOC_XPM_COLORS */

  /* If image specification contains symbolic color definitions, add
     these to `attrs'.  */
  color_symbols = image_spec_value (img->spec, QCcolor_symbols, NULL);
  if (CONSP (color_symbols))
    {
      Lisp_Object tail;
      XpmColorSymbol *xpm_syms;
      int i, size;
      
      attrs.valuemask |= XpmColorSymbols;

      /* Count number of symbols.  */
      attrs.numsymbols = 0;
      for (tail = color_symbols; CONSP (tail); tail = XCDR (tail))
	++attrs.numsymbols;

      /* Allocate an XpmColorSymbol array.  */
      size = attrs.numsymbols * sizeof *xpm_syms;
      xpm_syms = (XpmColorSymbol *) alloca (size);
      bzero (xpm_syms, size);
      attrs.colorsymbols = xpm_syms;

      /* Fill the color symbol array.  */
      for (tail = color_symbols, i = 0;
	   CONSP (tail);
	   ++i, tail = XCDR (tail))
	{
	  Lisp_Object name = XCAR (XCAR (tail));
	  Lisp_Object color = XCDR (XCAR (tail));
	  xpm_syms[i].name = (char *) alloca (XSTRING (name)->size + 1);
	  strcpy (xpm_syms[i].name, XSTRING (name)->data);
	  xpm_syms[i].value = (char *) alloca (XSTRING (color)->size + 1);
	  strcpy (xpm_syms[i].value, XSTRING (color)->data);
	}
    }

  /* Create a pixmap for the image, either from a file, or from a
     string buffer containing data in the same format as an XPM file.  */
#ifdef ALLOC_XPM_COLORS
  xpm_init_color_cache (f, &attrs);
#endif
  
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (specified_file))
    {
      Lisp_Object file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  return 0;
	}
      
      rc = XpmReadFileToPixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				XSTRING (file)->data, &img->pixmap, &img->mask,
				&attrs);
    }
  else
    {
      Lisp_Object buffer = image_spec_value (img->spec, QCdata, NULL);
      rc = XpmCreatePixmapFromBuffer (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
				      XSTRING (buffer)->data,
				      &img->pixmap, &img->mask,
				      &attrs);
    }

  if (rc == XpmSuccess)
    {
#ifdef ALLOC_XPM_COLORS
      img->colors = colors_in_color_table (&img->ncolors);
#else /* not ALLOC_XPM_COLORS */
      int i;

      img->ncolors = attrs.nalloc_pixels;
      img->colors = (unsigned long *) xmalloc (img->ncolors
					       * sizeof *img->colors);
      for (i = 0; i < attrs.nalloc_pixels; ++i)
	{
	  img->colors[i] = attrs.alloc_pixels[i];
#ifdef DEBUG_X_COLORS
	  register_color (img->colors[i]);
#endif
	}
#endif /* not ALLOC_XPM_COLORS */

      img->width = attrs.width;
      img->height = attrs.height;
      xassert (img->width > 0 && img->height > 0);

      /* The call to XpmFreeAttributes below frees attrs.alloc_pixels.  */
      XpmFreeAttributes (&attrs);
    }
  else
    {
      switch (rc)
	{
	case XpmOpenFailed:
	  image_error ("Error opening XPM file (%s)", img->spec, Qnil);
	  break;
	  
	case XpmFileInvalid:
	  image_error ("Invalid XPM file (%s)", img->spec, Qnil);
	  break;
	  
	case XpmNoMemory:
	  image_error ("Out of memory (%s)", img->spec, Qnil);
	  break;
	  
	case XpmColorFailed:
	  image_error ("Color allocation error (%s)", img->spec, Qnil);
	  break;
	  
	default:
	  image_error ("Unknown error (%s)", img->spec, Qnil);
	  break;
	}
    }

#ifdef ALLOC_XPM_COLORS
  xpm_free_color_cache ();
#endif
  return rc == XpmSuccess;
}

#endif /* HAVE_XPM != 0 */


/***********************************************************************
			     Color table
 ***********************************************************************/

/* An entry in the color table mapping an RGB color to a pixel color.  */

struct ct_color
{
  int r, g, b;
  unsigned long pixel;

  /* Next in color table collision list.  */
  struct ct_color *next;
};

/* The bucket vector size to use.  Must be prime.  */

#define CT_SIZE 101

/* Value is a hash of the RGB color given by R, G, and B.  */

#define CT_HASH_RGB(R, G, B) (((R) << 16) ^ ((G) << 8) ^ (B))

/* The color hash table.  */

struct ct_color **ct_table;

/* Number of entries in the color table.  */

int ct_colors_allocated;

/* Initialize the color table.  */

static void
init_color_table ()
{
  int size = CT_SIZE * sizeof (*ct_table);
  ct_table = (struct ct_color **) xmalloc (size);
  bzero (ct_table, size);
  ct_colors_allocated = 0;
}


/* Free memory associated with the color table.  */

static void
free_color_table ()
{
  int i;
  struct ct_color *p, *next;

  for (i = 0; i < CT_SIZE; ++i)
    for (p = ct_table[i]; p; p = next)
      {
	next = p->next;
	xfree (p);
      }

  xfree (ct_table);
  ct_table = NULL;
}


/* Value is a pixel color for RGB color R, G, B on frame F.  If an
   entry for that color already is in the color table, return the
   pixel color of that entry.  Otherwise, allocate a new color for R,
   G, B, and make an entry in the color table.  */

static unsigned long
lookup_rgb_color (f, r, g, b)
     struct frame *f;
     int r, g, b;
{
  unsigned hash = CT_HASH_RGB (r, g, b);
  int i = hash % CT_SIZE;
  struct ct_color *p;

  for (p = ct_table[i]; p; p = p->next)
    if (p->r == r && p->g == g && p->b == b)
      break;

  if (p == NULL)
    {
      XColor color;
      Colormap cmap;
      int rc;

      color.red = r;
      color.green = g;
      color.blue = b;
      
      cmap = FRAME_X_COLORMAP (f);
      rc = x_alloc_nearest_color (f, cmap, &color);

      if (rc)
	{
	  ++ct_colors_allocated;
      
	  p = (struct ct_color *) xmalloc (sizeof *p);
	  p->r = r;
	  p->g = g;
	  p->b = b;
	  p->pixel = color.pixel;
	  p->next = ct_table[i];
	  ct_table[i] = p;
	}
      else
	return FRAME_FOREGROUND_PIXEL (f);
    }

  return p->pixel;
}


/* Look up pixel color PIXEL which is used on frame F in the color
   table.  If not already present, allocate it.  Value is PIXEL.  */

static unsigned long
lookup_pixel_color (f, pixel)
     struct frame *f;
     unsigned long pixel;
{
  int i = pixel % CT_SIZE;
  struct ct_color *p;

  for (p = ct_table[i]; p; p = p->next)
    if (p->pixel == pixel)
      break;

  if (p == NULL)
    {
      XColor color;
      Colormap cmap;
      int rc;

      cmap = FRAME_X_COLORMAP (f);
      color.pixel = pixel;
      x_query_color (f, &color);
      rc = x_alloc_nearest_color (f, cmap, &color);

      if (rc)
	{
	  ++ct_colors_allocated;
      
	  p = (struct ct_color *) xmalloc (sizeof *p);
	  p->r = color.red;
	  p->g = color.green;
	  p->b = color.blue;
	  p->pixel = pixel;
	  p->next = ct_table[i];
	  ct_table[i] = p;
	}
      else
	return FRAME_FOREGROUND_PIXEL (f);
    }
  
  return p->pixel;
}


/* Value is a vector of all pixel colors contained in the color table,
   allocated via xmalloc.  Set *N to the number of colors.  */

static unsigned long *
colors_in_color_table (n)
     int *n;
{
  int i, j;
  struct ct_color *p;
  unsigned long *colors;

  if (ct_colors_allocated == 0)
    {
      *n = 0;
      colors = NULL;
    }
  else
    {
      colors = (unsigned long *) xmalloc (ct_colors_allocated
					  * sizeof *colors);
      *n = ct_colors_allocated;
      
      for (i = j = 0; i < CT_SIZE; ++i)
	for (p = ct_table[i]; p; p = p->next)
	  colors[j++] = p->pixel;
    }

  return colors;
}



/***********************************************************************
			      Algorithms
 ***********************************************************************/

static XColor *x_to_xcolors P_ ((struct frame *, struct image *, int));
static void x_from_xcolors P_ ((struct frame *, struct image *, XColor *));
static void x_detect_edges P_ ((struct frame *, struct image *, int[9], int));

/* Non-zero means draw a cross on images having `:conversion
   disabled'.  */

int cross_disabled_images;

/* Edge detection matrices for different edge-detection
   strategies.  */

static int emboss_matrix[9] = {
   /* x - 1	x	x + 1  */
        2,     -1,  	  0,		/* y - 1 */
       -1,      0,        1,		/* y     */
        0,      1,       -2		/* y + 1 */
};

static int laplace_matrix[9] = {
   /* x - 1	x	x + 1  */
        1,      0,  	  0,		/* y - 1 */
        0,      0,        0,		/* y     */
        0,      0,       -1		/* y + 1 */
};

/* Value is the intensity of the color whose red/green/blue values
   are R, G, and B.  */

#define COLOR_INTENSITY(R, G, B) ((2 * (R) + 3 * (G) + (B)) / 6)


/* On frame F, return an array of XColor structures describing image
   IMG->pixmap.  Each XColor structure has its pixel color set.  RGB_P
   non-zero means also fill the red/green/blue members of the XColor
   structures.  Value is a pointer to the array of XColors structures,
   allocated with xmalloc; it must be freed by the caller.  */

static XColor *
x_to_xcolors (f, img, rgb_p)
     struct frame *f;
     struct image *img;
     int rgb_p;
{
  int x, y;
  XColor *colors, *p;
  XImage *ximg;

  colors = (XColor *) xmalloc (img->width * img->height * sizeof *colors);

  /* Get the X image IMG->pixmap.  */
  ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
		    0, 0, img->width, img->height, ~0, ZPixmap);

  /* Fill the `pixel' members of the XColor array.  I wished there
     were an easy and portable way to circumvent XGetPixel.  */
  p = colors;
  for (y = 0; y < img->height; ++y)
    {
      XColor *row = p;
      
      for (x = 0; x < img->width; ++x, ++p)
	p->pixel = XGetPixel (ximg, x, y);

      if (rgb_p)
	x_query_colors (f, row, img->width);
    }

  XDestroyImage (ximg);
  return colors;
}


/* Create IMG->pixmap from an array COLORS of XColor structures, whose
   RGB members are set.  F is the frame on which this all happens.
   COLORS will be freed; an existing IMG->pixmap will be freed, too.  */

static void
x_from_xcolors (f, img, colors)
     struct frame *f;
     struct image *img;
     XColor *colors;
{
  int x, y;
  XImage *oimg;
  Pixmap pixmap;
  XColor *p;
  
  init_color_table ();
  
  x_create_x_image_and_pixmap (f, img->width, img->height, 0,
			       &oimg, &pixmap);
  p = colors;
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x, ++p)
      {
	unsigned long pixel;
	pixel = lookup_rgb_color (f, p->red, p->green, p->blue);
	XPutPixel (oimg, x, y, pixel);
      }

  xfree (colors);
  x_clear_image_1 (f, img, 1, 0, 1);

  x_put_x_image (f, oimg, pixmap, img->width, img->height);
  x_destroy_x_image (oimg);
  img->pixmap = pixmap;
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
}


/* On frame F, perform edge-detection on image IMG.

   MATRIX is a nine-element array specifying the transformation
   matrix.  See emboss_matrix for an example.
   
   COLOR_ADJUST is a color adjustment added to each pixel of the
   outgoing image.  */

static void
x_detect_edges (f, img, matrix, color_adjust)
     struct frame *f;
     struct image *img;
     int matrix[9], color_adjust;
{
  XColor *colors = x_to_xcolors (f, img, 1);
  XColor *new, *p;
  int x, y, i, sum;

  for (i = sum = 0; i < 9; ++i)
    sum += abs (matrix[i]);

#define COLOR(A, X, Y) ((A) + (Y) * img->width + (X))

  new = (XColor *) xmalloc (img->width * img->height * sizeof *new);

  for (y = 0; y < img->height; ++y)
    {
      p = COLOR (new, 0, y);
      p->red = p->green = p->blue = 0xffff/2;
      p = COLOR (new, img->width - 1, y);
      p->red = p->green = p->blue = 0xffff/2;
    }
  
  for (x = 1; x < img->width - 1; ++x)
    {
      p = COLOR (new, x, 0);
      p->red = p->green = p->blue = 0xffff/2;
      p = COLOR (new, x, img->height - 1);
      p->red = p->green = p->blue = 0xffff/2;
    }

  for (y = 1; y < img->height - 1; ++y)
    {
      p = COLOR (new, 1, y);
      
      for (x = 1; x < img->width - 1; ++x, ++p)
	{
	  int r, g, b, y1, x1;

	  r = g = b = i = 0;
	  for (y1 = y - 1; y1 < y + 2; ++y1)
	    for (x1 = x - 1; x1 < x + 2; ++x1, ++i)
	      if (matrix[i])
	        {
	          XColor *t = COLOR (colors, x1, y1);
		  r += matrix[i] * t->red;
		  g += matrix[i] * t->green;
		  b += matrix[i] * t->blue;
		}

	  r = (r / sum + color_adjust) & 0xffff;
	  g = (g / sum + color_adjust) & 0xffff;
	  b = (b / sum + color_adjust) & 0xffff;
	  p->red = p->green = p->blue = COLOR_INTENSITY (r, g, b);
	}
    }

  xfree (colors);
  x_from_xcolors (f, img, new);

#undef COLOR
}


/* Perform the pre-defined `emboss' edge-detection on image IMG
   on frame F.  */

static void
x_emboss (f, img)
     struct frame *f;
     struct image *img;
{
  x_detect_edges (f, img, emboss_matrix, 0xffff / 2);
}


/* Perform the pre-defined `laplace' edge-detection on image IMG
   on frame F.  */

static void
x_laplace (f, img)
     struct frame *f;
     struct image *img;
{
  x_detect_edges (f, img, laplace_matrix, 45000);
}


/* Perform edge-detection on image IMG on frame F, with specified
   transformation matrix MATRIX and color-adjustment COLOR_ADJUST.

   MATRIX must be either

   - a list of at least 9 numbers in row-major form
   - a vector of at least 9 numbers

   COLOR_ADJUST nil means use a default; otherwise it must be a
   number.  */

static void
x_edge_detection (f, img, matrix, color_adjust)
     struct frame *f;
     struct image *img;
     Lisp_Object matrix, color_adjust;
{
  int i = 0;
  int trans[9];
  
  if (CONSP (matrix))
    {
      for (i = 0;
	   i < 9 && CONSP (matrix) && NUMBERP (XCAR (matrix));
	   ++i, matrix = XCDR (matrix))
	trans[i] = XFLOATINT (XCAR (matrix));
    }
  else if (VECTORP (matrix) && ASIZE (matrix) >= 9)
    {
      for (i = 0; i < 9 && NUMBERP (AREF (matrix, i)); ++i)
	trans[i] = XFLOATINT (AREF (matrix, i));
    }

  if (NILP (color_adjust))
    color_adjust = make_number (0xffff / 2);

  if (i == 9 && NUMBERP (color_adjust))
    x_detect_edges (f, img, trans, (int) XFLOATINT (color_adjust));
}


/* Transform image IMG on frame F so that it looks disabled.  */

static void
x_disable_image (f, img)
     struct frame *f;
     struct image *img;
{
  struct x_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  if (dpyinfo->n_planes >= 2)
    {
      /* Color (or grayscale).  Convert to gray, and equalize.  Just
	 drawing such images with a stipple can look very odd, so
	 we're using this method instead.  */
      XColor *colors = x_to_xcolors (f, img, 1);
      XColor *p, *end;
      const int h = 15000;
      const int l = 30000;

      for (p = colors, end = colors + img->width * img->height;
	   p < end;
	   ++p)
	{
	  int i = COLOR_INTENSITY (p->red, p->green, p->blue);
	  int i2 = (0xffff - h - l) * i / 0xffff + l;
	  p->red = p->green = p->blue = i2;
	}

      x_from_xcolors (f, img, colors);
    }

  /* Draw a cross over the disabled image, if we must or if we
     should.  */
  if (dpyinfo->n_planes < 2 || cross_disabled_images)
    {
      Display *dpy = FRAME_X_DISPLAY (f);
      GC gc;

      gc = XCreateGC (dpy, img->pixmap, 0, NULL);
      XSetForeground (dpy, gc, BLACK_PIX_DEFAULT (f));
      XDrawLine (dpy, img->pixmap, gc, 0, 0,
		 img->width - 1, img->height - 1);
      XDrawLine (dpy, img->pixmap, gc, 0, img->height - 1,
		 img->width - 1, 0);
      XFreeGC (dpy, gc);

      if (img->mask)
	{
	  gc = XCreateGC (dpy, img->mask, 0, NULL);
	  XSetForeground (dpy, gc, WHITE_PIX_DEFAULT (f));
	  XDrawLine (dpy, img->mask, gc, 0, 0,
		     img->width - 1, img->height - 1);
	  XDrawLine (dpy, img->mask, gc, 0, img->height - 1,
		     img->width - 1, 0);
	  XFreeGC (dpy, gc);
	}
    }
}


/* Build a mask for image IMG which is used on frame F.  FILE is the
   name of an image file, for error messages.  HOW determines how to
   determine the background color of IMG.  If it is a list '(R G B)',
   with R, G, and B being integers >= 0, take that as the color of the
   background.  Otherwise, determine the background color of IMG
   heuristically.  Value is non-zero if successful. */

static int
x_build_heuristic_mask (f, img, how)
     struct frame *f;
     struct image *img;
     Lisp_Object how;
{
  Display *dpy = FRAME_X_DISPLAY (f);
  XImage *ximg, *mask_img;
  int x, y, rc, use_img_background;
  unsigned long bg = 0;

  if (img->mask)
    {
      XFreePixmap (FRAME_X_DISPLAY (f), img->mask);
      img->mask = None;
      img->background_transparent_valid = 0;
    }

  /* Create an image and pixmap serving as mask.  */
  rc = x_create_x_image_and_pixmap (f, img->width, img->height, 1,
				    &mask_img, &img->mask);
  if (!rc)
    return 0;

  /* Get the X image of IMG->pixmap.  */
  ximg = XGetImage (dpy, img->pixmap, 0, 0, img->width, img->height,
		    ~0, ZPixmap);

  /* Determine the background color of ximg.  If HOW is `(R G B)'
     take that as color.  Otherwise, use the image's background color. */
  use_img_background = 1;
  
  if (CONSP (how))
    {
      int rgb[3], i;

      for (i = 0; i < 3 && CONSP (how) && NATNUMP (XCAR (how)); ++i)
	{
	  rgb[i] = XFASTINT (XCAR (how)) & 0xffff;
	  how = XCDR (how);
	}

      if (i == 3 && NILP (how))
	{
	  char color_name[30];
	  sprintf (color_name, "#%04x%04x%04x", rgb[0], rgb[1], rgb[2]);
	  bg = x_alloc_image_color (f, img, build_string (color_name), 0);
	  use_img_background = 0;
	}
    }
  
  if (use_img_background)
    bg = four_corners_best (ximg, img->width, img->height);

  /* Set all bits in mask_img to 1 whose color in ximg is different
     from the background color bg.  */
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x)
      XPutPixel (mask_img, x, y, XGetPixel (ximg, x, y) != bg);

  /* Fill in the background_transparent field while we have the mask handy. */
  image_background_transparent (img, f, mask_img);

  /* Put mask_img into img->mask.  */
  x_put_x_image (f, mask_img, img->mask, img->width, img->height);
  x_destroy_x_image (mask_img);
  XDestroyImage (ximg);
  
  return 1;
}



/***********************************************************************
		       PBM (mono, gray, color)
 ***********************************************************************/

static int pbm_image_p P_ ((Lisp_Object object));
static int pbm_load P_ ((struct frame *f, struct image *img));
static int pbm_scan_number P_ ((unsigned char **, unsigned char *));

/* The symbol `pbm' identifying images of this type.  */

Lisp_Object Qpbm;

/* Indices of image specification fields in gs_format, below.  */

enum pbm_keyword_index
{
  PBM_TYPE,
  PBM_FILE,
  PBM_DATA,
  PBM_ASCENT,
  PBM_MARGIN,
  PBM_RELIEF,
  PBM_ALGORITHM,
  PBM_HEURISTIC_MASK,
  PBM_MASK,
  PBM_FOREGROUND,
  PBM_BACKGROUND,
  PBM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword pbm_format[PBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":foreground",	IMAGE_STRING_OR_NIL_VALUE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `pbm'.  */

static struct image_type pbm_type =
{
  &Qpbm,
  pbm_image_p,
  pbm_load,
  x_clear_image,
  NULL
};


/* Return non-zero if OBJECT is a valid PBM image specification.  */

static int
pbm_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[PBM_LAST];
  
  bcopy (pbm_format, fmt, sizeof fmt);
  
  if (!parse_image_spec (object, fmt, PBM_LAST, Qpbm))
    return 0;

  /* Must specify either :data or :file.  */
  return fmt[PBM_DATA].count + fmt[PBM_FILE].count == 1;
}


/* Scan a decimal number from *S and return it.  Advance *S while
   reading the number.  END is the end of the string.  Value is -1 at
   end of input.  */

static int
pbm_scan_number (s, end)
     unsigned char **s, *end;
{
  int c = 0, val = -1;

  while (*s < end)
    {
      /* Skip white-space.  */
      while (*s < end && (c = *(*s)++, isspace (c)))
	;

      if (c == '#')
	{
	  /* Skip comment to end of line.  */
	  while (*s < end && (c = *(*s)++, c != '\n'))
	    ;
	}
      else if (isdigit (c))
	{
	  /* Read decimal number.  */
	  val = c - '0';
	  while (*s < end && (c = *(*s)++, isdigit (c)))
	    val = 10 * val + c - '0';
	  break;
	}
      else
	break;
    }

  return val;
}


/* Load PBM image IMG for use on frame F.  */

static int 
pbm_load (f, img)
     struct frame *f;
     struct image *img;
{
  int raw_p, x, y;
  int width, height, max_color_idx = 0;
  XImage *ximg;
  Lisp_Object file, specified_file;
  enum {PBM_MONO, PBM_GRAY, PBM_COLOR} type;
  struct gcpro gcpro1;
  unsigned char *contents = NULL;
  unsigned char *end, *p;
  int size;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  file = Qnil;
  GCPRO1 (file);

  if (STRINGP (specified_file))
    {
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      contents = slurp_file (XSTRING (file)->data, &size);
      if (contents == NULL)
	{
	  image_error ("Error reading `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      p = contents;
      end = contents + size;
    }
  else
    {
      Lisp_Object data;
      data = image_spec_value (img->spec, QCdata, NULL);
      p = XSTRING (data)->data;
      end = p + STRING_BYTES (XSTRING (data));
    }

  /* Check magic number.  */
  if (end - p < 2 || *p++ != 'P')
    {
      image_error ("Not a PBM image: `%s'", img->spec, Qnil);
    error:
      xfree (contents);
      UNGCPRO;
      return 0;
    }

  switch (*p++)
    {
    case '1':
      raw_p = 0, type = PBM_MONO;
      break;
      
    case '2':
      raw_p = 0, type = PBM_GRAY;
      break;

    case '3':
      raw_p = 0, type = PBM_COLOR;
      break;

    case '4':
      raw_p = 1, type = PBM_MONO;
      break;
      
    case '5':
      raw_p = 1, type = PBM_GRAY;
      break;
      
    case '6':
      raw_p = 1, type = PBM_COLOR;
      break;

    default:
      image_error ("Not a PBM image: `%s'", img->spec, Qnil);
      goto error;
    }

  /* Read width, height, maximum color-component.  Characters
     starting with `#' up to the end of a line are ignored.  */
  width = pbm_scan_number (&p, end);
  height = pbm_scan_number (&p, end);

  if (type != PBM_MONO)
    {
      max_color_idx = pbm_scan_number (&p, end);
      if (raw_p && max_color_idx > 255)
	max_color_idx = 255;
    }
  
  if (width < 0
      || height < 0
      || (type != PBM_MONO && max_color_idx < 0))
    goto error;

  if (!x_create_x_image_and_pixmap (f, width, height, 0,
				    &ximg, &img->pixmap))
    goto error;
  
  /* Initialize the color hash table.  */
  init_color_table ();

  if (type == PBM_MONO)
    {
      int c = 0, g;
      struct image_keyword fmt[PBM_LAST];
      unsigned long fg = FRAME_FOREGROUND_PIXEL (f);
      unsigned long bg = FRAME_BACKGROUND_PIXEL (f);

      /* Parse the image specification.  */
      bcopy (pbm_format, fmt, sizeof fmt);
      parse_image_spec (img->spec, fmt, PBM_LAST, Qpbm);
      
      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[PBM_FOREGROUND].count
	  && STRINGP (fmt[PBM_FOREGROUND].value))
	fg = x_alloc_image_color (f, img, fmt[PBM_FOREGROUND].value, fg);
      if (fmt[PBM_BACKGROUND].count
	  && STRINGP (fmt[PBM_BACKGROUND].value))
	{
	  bg = x_alloc_image_color (f, img, fmt[PBM_BACKGROUND].value, bg);
	  img->background = bg;
	  img->background_valid = 1;
	}
      
      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    if (raw_p)
	      {
		if ((x & 7) == 0)
		  c = *p++;
		g = c & 0x80;
		c <<= 1;
	      }
	    else
	      g = pbm_scan_number (&p, end);

	    XPutPixel (ximg, x, y, g ? fg : bg);
	  }
    }
  else
    {
      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    int r, g, b;
	    
	    if (type == PBM_GRAY)
	      r = g = b = raw_p ? *p++ : pbm_scan_number (&p, end);
	    else if (raw_p)
	      {
		r = *p++;
		g = *p++;
		b = *p++;
	      }
	    else
	      {
		r = pbm_scan_number (&p, end);
		g = pbm_scan_number (&p, end);
		b = pbm_scan_number (&p, end);
	      }
	    
	    if (r < 0 || g < 0 || b < 0)
	      {
		xfree (ximg->data);
		ximg->data = NULL;
		XDestroyImage (ximg);
		image_error ("Invalid pixel value in image `%s'",
			     img->spec, Qnil);
		goto error;
	      }
	    
	    /* RGB values are now in the range 0..max_color_idx.
	       Scale this to the range 0..0xffff supported by X.  */
	    r = (double) r * 65535 / max_color_idx;
	    g = (double) g * 65535 / max_color_idx;
	    b = (double) b * 65535 / max_color_idx;
	    XPutPixel (ximg, x, y, lookup_rgb_color (f, r, g, b));
	  }
    }
  
  /* Store in IMG->colors the colors allocated for the image, and
     free the color table.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);
  
  /* Put the image into a pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
      
  img->width = width;
  img->height = height;

  UNGCPRO;
  xfree (contents);
  return 1;
}



/***********************************************************************
				 PNG
 ***********************************************************************/

#if HAVE_PNG

#include <png.h>

/* Function prototypes.  */

static int png_image_p P_ ((Lisp_Object object));
static int png_load P_ ((struct frame *f, struct image *img));

/* The symbol `png' identifying images of this type.  */

Lisp_Object Qpng;

/* Indices of image specification fields in png_format, below.  */

enum png_keyword_index
{
  PNG_TYPE,
  PNG_DATA,
  PNG_FILE,
  PNG_ASCENT,
  PNG_MARGIN,
  PNG_RELIEF,
  PNG_ALGORITHM,
  PNG_HEURISTIC_MASK,
  PNG_MASK,
  PNG_BACKGROUND,
  PNG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword png_format[PNG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `png'.  */

static struct image_type png_type =
{
  &Qpng,
  png_image_p,
  png_load,
  x_clear_image,
  NULL
};


/* Return non-zero if OBJECT is a valid PNG image specification.  */

static int
png_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[PNG_LAST];
  bcopy (png_format, fmt, sizeof fmt);
  
  if (!parse_image_spec (object, fmt, PNG_LAST, Qpng))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[PNG_FILE].count + fmt[PNG_DATA].count == 1;
}


/* Error and warning handlers installed when the PNG library
   is initialized.  */

static void
my_png_error (png_ptr, msg)
     png_struct *png_ptr;
     char *msg;
{
  xassert (png_ptr != NULL);
  image_error ("PNG error: %s", build_string (msg), Qnil);
  longjmp (png_ptr->jmpbuf, 1);
}


static void
my_png_warning (png_ptr, msg)
     png_struct *png_ptr;
     char *msg;
{
  xassert (png_ptr != NULL);
  image_error ("PNG warning: %s", build_string (msg), Qnil);
}

/* Memory source for PNG decoding.  */

struct png_memory_storage
{
  unsigned char *bytes;		/* The data       */
  size_t len;			/* How big is it? */
  int index;			/* Where are we?  */
};


/* Function set as reader function when reading PNG image from memory.
   PNG_PTR is a pointer to the PNG control structure.  Copy LENGTH
   bytes from the input to DATA.  */

static void
png_read_from_memory (png_ptr, data, length)
     png_structp png_ptr;
     png_bytep data;
     png_size_t length;
{
  struct png_memory_storage *tbr
    = (struct png_memory_storage *) png_get_io_ptr (png_ptr);

  if (length > tbr->len - tbr->index)
    png_error (png_ptr, "Read error");
  
  bcopy (tbr->bytes + tbr->index, data, length);
  tbr->index = tbr->index + length;
}

/* Load PNG image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int
png_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  Lisp_Object specified_data;
  int x, y, i;
  XImage *ximg, *mask_img = NULL;
  struct gcpro gcpro1;
  png_struct *png_ptr = NULL;
  png_info *info_ptr = NULL, *end_info = NULL;
  FILE *volatile fp = NULL;
  png_byte sig[8];
  png_byte * volatile pixels = NULL;
  png_byte ** volatile rows = NULL;
  png_uint_32 width, height;
  int bit_depth, color_type, interlace_type;
  png_byte channels;
  png_uint_32 row_bytes;
  int transparent_p;
  char *gamma_str;
  double screen_gamma, image_gamma;
  int intent;
  struct png_memory_storage tbr;  /* Data to be read */

  /* Find out what file to load.  */
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);
  file = Qnil;
  GCPRO1 (file);

  if (NILP (specified_data))
    {
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}

      /* Open the image file.  */
      fp = fopen (XSTRING (file)->data, "rb");
      if (!fp)
	{
	  image_error ("Cannot open image file `%s'", file, Qnil);
	  UNGCPRO;
	  fclose (fp);
	  return 0;
	}

      /* Check PNG signature.  */
      if (fread (sig, 1, sizeof sig, fp) != sizeof sig
	  || !png_check_sig (sig, sizeof sig))
	{
	  image_error ("Not a PNG file: `%s'", file, Qnil);
	  UNGCPRO;
	  fclose (fp);
	  return 0;
	}
    }
  else
    {
      /* Read from memory.  */
      tbr.bytes = XSTRING (specified_data)->data;
      tbr.len = STRING_BYTES (XSTRING (specified_data));
      tbr.index = 0;

      /* Check PNG signature.  */
      if (tbr.len < sizeof sig
	  || !png_check_sig (tbr.bytes, sizeof sig))
	{
	  image_error ("Not a PNG image: `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}

      /* Need to skip past the signature.  */
      tbr.bytes += sizeof (sig);
    }

  /* Initialize read and info structs for PNG lib.  */
  png_ptr = png_create_read_struct (PNG_LIBPNG_VER_STRING, NULL,
				    my_png_error, my_png_warning);
  if (!png_ptr)
    {
      if (fp) fclose (fp);
      UNGCPRO;
      return 0;
    }

  info_ptr = png_create_info_struct (png_ptr);
  if (!info_ptr)
    {
      png_destroy_read_struct (&png_ptr, NULL, NULL);
      if (fp) fclose (fp);
      UNGCPRO;
      return 0;
    }

  end_info = png_create_info_struct (png_ptr);
  if (!end_info)
    {
      png_destroy_read_struct (&png_ptr, &info_ptr, NULL);
      if (fp) fclose (fp);
      UNGCPRO;
      return 0;
    }

  /* Set error jump-back.  We come back here when the PNG library
     detects an error.  */
  if (setjmp (png_ptr->jmpbuf))
    {
    error:
      if (png_ptr)
        png_destroy_read_struct (&png_ptr, &info_ptr, &end_info);
      xfree (pixels);
      xfree (rows);
      if (fp) fclose (fp);
      UNGCPRO;
      return 0;
    }

  /* Read image info.  */
  if (!NILP (specified_data))
    png_set_read_fn (png_ptr, (void *) &tbr, png_read_from_memory);
  else
    png_init_io (png_ptr, fp);

  png_set_sig_bytes (png_ptr, sizeof sig);
  png_read_info (png_ptr, info_ptr);
  png_get_IHDR (png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
	        &interlace_type, NULL, NULL);

  /* If image contains simply transparency data, we prefer to 
     construct a clipping mask.  */
  if (png_get_valid (png_ptr, info_ptr, PNG_INFO_tRNS))
    transparent_p = 1;
  else
    transparent_p = 0;

  /* This function is easier to write if we only have to handle 
     one data format: RGB or RGBA with 8 bits per channel.  Let's
     transform other formats into that format.  */

  /* Strip more than 8 bits per channel.  */
  if (bit_depth == 16)
    png_set_strip_16 (png_ptr);

  /* Expand data to 24 bit RGB, or 8 bit grayscale, with alpha channel
     if available.  */
  png_set_expand (png_ptr);

  /* Convert grayscale images to RGB.  */
  if (color_type == PNG_COLOR_TYPE_GRAY 
      || color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
    png_set_gray_to_rgb (png_ptr);

  /* The value 2.2 is a guess for PC monitors from PNG example.c.  */
  gamma_str = getenv ("SCREEN_GAMMA");
  screen_gamma = gamma_str ? atof (gamma_str) : 2.2;

  /* Tell the PNG lib to handle gamma correction for us.  */

#if defined(PNG_READ_sRGB_SUPPORTED) || defined(PNG_WRITE_sRGB_SUPPORTED)
  if (png_get_sRGB (png_ptr, info_ptr, &intent))
    /* There is a special chunk in the image specifying the gamma.  */
    png_set_sRGB (png_ptr, info_ptr, intent);
  else
#endif
  if (png_get_gAMA (png_ptr, info_ptr, &image_gamma))
    /* Image contains gamma information.  */
    png_set_gamma (png_ptr, screen_gamma, image_gamma);
  else
    /* Use a default of 0.5 for the image gamma.  */
    png_set_gamma (png_ptr, screen_gamma, 0.5);

  /* Handle alpha channel by combining the image with a background
     color.  Do this only if a real alpha channel is supplied.  For
     simple transparency, we prefer a clipping mask.  */
  if (!transparent_p)
    {
      png_color_16 *image_bg;
      Lisp_Object specified_bg
	= image_spec_value (img->spec, QCbackground, NULL);

      if (STRINGP (specified_bg))
	/* The user specified `:background', use that.  */
	{
	  XColor color;
	  if (x_defined_color (f, XSTRING (specified_bg)->data, &color, 0))
	    {
	      png_color_16 user_bg;

	      bzero (&user_bg, sizeof user_bg);
	      user_bg.red = color.red;
	      user_bg.green = color.green;
	      user_bg.blue = color.blue;

	      png_set_background (png_ptr, &user_bg,
				  PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
	    }
	}
      else if (png_get_bKGD (png_ptr, info_ptr, &image_bg))
	/* Image contains a background color with which to 
	   combine the image.  */
	png_set_background (png_ptr, image_bg,
			    PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
      else
	{
	  /* Image does not contain a background color with which
	     to combine the image data via an alpha channel.  Use 
	     the frame's background instead.  */
	  XColor color;
	  Colormap cmap;
	  png_color_16 frame_background;

	  cmap = FRAME_X_COLORMAP (f);
	  color.pixel = FRAME_BACKGROUND_PIXEL (f);
	  x_query_color (f, &color);

	  bzero (&frame_background, sizeof frame_background);
	  frame_background.red = color.red;
	  frame_background.green = color.green;
	  frame_background.blue = color.blue;

	  png_set_background (png_ptr, &frame_background,
			      PNG_BACKGROUND_GAMMA_SCREEN, 0, 1.0);
	}
    }

  /* Update info structure.  */
  png_read_update_info (png_ptr, info_ptr);

  /* Get number of channels.  Valid values are 1 for grayscale images
     and images with a palette, 2 for grayscale images with transparency
     information (alpha channel), 3 for RGB images, and 4 for RGB
     images with alpha channel, i.e. RGBA.  If conversions above were
     sufficient we should only have 3 or 4 channels here.  */
  channels = png_get_channels (png_ptr, info_ptr);
  xassert (channels == 3 || channels == 4);

  /* Number of bytes needed for one row of the image.  */
  row_bytes = png_get_rowbytes (png_ptr, info_ptr);

  /* Allocate memory for the image.  */
  pixels = (png_byte *) xmalloc (row_bytes * height * sizeof *pixels);
  rows = (png_byte **) xmalloc (height * sizeof *rows);
  for (i = 0; i < height; ++i)
    rows[i] = pixels + i * row_bytes;

  /* Read the entire image.  */
  png_read_image (png_ptr, rows);
  png_read_end (png_ptr, info_ptr);
  if (fp)
    {
      fclose (fp);
      fp = NULL;
    }
  
  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg,
				    &img->pixmap))
    goto error;
  
  /* Create an image and pixmap serving as mask if the PNG image
     contains an alpha channel.  */
  if (channels == 4
      && !transparent_p
      && !x_create_x_image_and_pixmap (f, width, height, 1,
				       &mask_img, &img->mask))
    {
      x_destroy_x_image (ximg);
      XFreePixmap (FRAME_X_DISPLAY (f), img->pixmap);
      img->pixmap = None;
      goto error;
    }

  /* Fill the X image and mask from PNG data.  */
  init_color_table ();

  for (y = 0; y < height; ++y)
    {
      png_byte *p = rows[y];

      for (x = 0; x < width; ++x)
	{
	  unsigned r, g, b;

	  r = *p++ << 8;
	  g = *p++ << 8;
	  b = *p++ << 8;
	  XPutPixel (ximg, x, y, lookup_rgb_color (f, r, g, b));

	  /* An alpha channel, aka mask channel, associates variable
	     transparency with an image.  Where other image formats 
	     support binary transparency---fully transparent or fully 
	     opaque---PNG allows up to 254 levels of partial transparency.
	     The PNG library implements partial transparency by combining
	     the image with a specified background color.

	     I'm not sure how to handle this here nicely: because the
	     background on which the image is displayed may change, for
	     real alpha channel support, it would be necessary to create 
	     a new image for each possible background.  

	     What I'm doing now is that a mask is created if we have
	     boolean transparency information.  Otherwise I'm using
	     the frame's background color to combine the image with.  */

	  if (channels == 4)
	    {
	      if (mask_img)
		XPutPixel (mask_img, x, y, *p > 0);
	      ++p;
	    }
	}
    }

  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    /* Set IMG's background color from the PNG image, unless the user
       overrode it.  */
    {
      png_color_16 *bg;
      if (png_get_bKGD (png_ptr, info_ptr, &bg))
	{
	  img->background = lookup_rgb_color (f, bg->red, bg->green, bg->blue);
	  img->background_valid = 1;
	}
    }

  /* Remember colors allocated for this image.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();

  /* Clean up.  */
  png_destroy_read_struct (&png_ptr, &info_ptr, &end_info);
  xfree (rows);
  xfree (pixels);

  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy. */
  IMAGE_BACKGROUND (img, f, ximg);

  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);

  /* Same for the mask.  */
  if (mask_img)
    {
      /* Fill in the background_transparent field while we have the mask
	 handy. */
      image_background_transparent (img, f, mask_img);

      x_put_x_image (f, mask_img, img->mask, img->width, img->height);
      x_destroy_x_image (mask_img);
    }

  UNGCPRO;
  return 1;
}

#endif /* HAVE_PNG != 0 */



/***********************************************************************
				 JPEG
 ***********************************************************************/

#if HAVE_JPEG

/* Work around a warning about HAVE_STDLIB_H being redefined in
   jconfig.h.  */
#ifdef HAVE_STDLIB_H
#define HAVE_STDLIB_H_1
#undef HAVE_STDLIB_H
#endif /* HAVE_STLIB_H */

#include <jpeglib.h>
#include <jerror.h>
#include <setjmp.h>

#ifdef HAVE_STLIB_H_1
#define HAVE_STDLIB_H 1
#endif

static int jpeg_image_p P_ ((Lisp_Object object));
static int jpeg_load P_ ((struct frame *f, struct image *img));

/* The symbol `jpeg' identifying images of this type.  */

Lisp_Object Qjpeg;

/* Indices of image specification fields in gs_format, below.  */

enum jpeg_keyword_index
{
  JPEG_TYPE,
  JPEG_DATA,
  JPEG_FILE,
  JPEG_ASCENT,
  JPEG_MARGIN,
  JPEG_RELIEF,
  JPEG_ALGORITHM,
  JPEG_HEURISTIC_MASK,
  JPEG_MASK,
  JPEG_BACKGROUND,
  JPEG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword jpeg_format[JPEG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversions",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `jpeg'.  */

static struct image_type jpeg_type =
{
  &Qjpeg,
  jpeg_image_p,
  jpeg_load,
  x_clear_image,
  NULL
};


/* Return non-zero if OBJECT is a valid JPEG image specification.  */

static int
jpeg_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[JPEG_LAST];
  
  bcopy (jpeg_format, fmt, sizeof fmt);
  
  if (!parse_image_spec (object, fmt, JPEG_LAST, Qjpeg))
    return 0;

  /* Must specify either the :data or :file keyword.  */
  return fmt[JPEG_FILE].count + fmt[JPEG_DATA].count == 1;
}


struct my_jpeg_error_mgr
{
  struct jpeg_error_mgr pub;
  jmp_buf setjmp_buffer;
};


static void
my_error_exit (cinfo)
     j_common_ptr cinfo;
{
  struct my_jpeg_error_mgr *mgr = (struct my_jpeg_error_mgr *) cinfo->err;
  longjmp (mgr->setjmp_buffer, 1);
}


/* Init source method for JPEG data source manager.  Called by
   jpeg_read_header() before any data is actually read.  See
   libjpeg.doc from the JPEG lib distribution.  */

static void
our_init_source (cinfo)
     j_decompress_ptr cinfo;
{
}


/* Fill input buffer method for JPEG data source manager.  Called
   whenever more data is needed.  We read the whole image in one step,
   so this only adds a fake end of input marker at the end.  */

static boolean
our_fill_input_buffer (cinfo)
     j_decompress_ptr cinfo;
{
  /* Insert a fake EOI marker.  */
  struct jpeg_source_mgr *src = cinfo->src;
  static JOCTET buffer[2];

  buffer[0] = (JOCTET) 0xFF;
  buffer[1] = (JOCTET) JPEG_EOI;

  src->next_input_byte = buffer;
  src->bytes_in_buffer = 2;
  return TRUE;
}


/* Method to skip over NUM_BYTES bytes in the image data.  CINFO->src
   is the JPEG data source manager.  */

static void
our_skip_input_data (cinfo, num_bytes)
     j_decompress_ptr cinfo;
     long num_bytes;
{
  struct jpeg_source_mgr *src = (struct jpeg_source_mgr *) cinfo->src;

  if (src)
    {
      if (num_bytes > src->bytes_in_buffer)
	ERREXIT (cinfo, JERR_INPUT_EOF);
      
      src->bytes_in_buffer -= num_bytes;
      src->next_input_byte += num_bytes;
    }
}


/* Method to terminate data source.  Called by
   jpeg_finish_decompress() after all data has been processed.  */

static void
our_term_source (cinfo)
     j_decompress_ptr cinfo;
{
}


/* Set up the JPEG lib for reading an image from DATA which contains
   LEN bytes.  CINFO is the decompression info structure created for
   reading the image.  */

static void
jpeg_memory_src (cinfo, data, len)
     j_decompress_ptr cinfo;
     JOCTET *data;
     unsigned int len;
{
  struct jpeg_source_mgr *src;

  if (cinfo->src == NULL)
    {
      /* First time for this JPEG object?  */
      cinfo->src = (struct jpeg_source_mgr *)
	(*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				    sizeof (struct jpeg_source_mgr));
      src = (struct jpeg_source_mgr *) cinfo->src;
      src->next_input_byte = data;
    }
  
  src = (struct jpeg_source_mgr *) cinfo->src;
  src->init_source = our_init_source;
  src->fill_input_buffer = our_fill_input_buffer;
  src->skip_input_data = our_skip_input_data;
  src->resync_to_restart = jpeg_resync_to_restart; /* Use default method.  */
  src->term_source = our_term_source;
  src->bytes_in_buffer = len;
  src->next_input_byte = data;
}


/* Load image IMG for use on frame F.  Patterned after example.c
   from the JPEG lib.  */

static int 
jpeg_load (f, img)
     struct frame *f;
     struct image *img;
{
  struct jpeg_decompress_struct cinfo;
  struct my_jpeg_error_mgr mgr;
  Lisp_Object file, specified_file;
  Lisp_Object specified_data;
  FILE * volatile fp = NULL;
  JSAMPARRAY buffer;
  int row_stride, x, y;
  XImage *ximg = NULL;
  int rc;
  unsigned long *colors;
  int width, height;
  struct gcpro gcpro1;

  /* Open the JPEG file.  */
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);
  file = Qnil;
  GCPRO1 (file);

  if (NILP (specified_data))
    {
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}
  
      fp = fopen (XSTRING (file)->data, "r");
      if (fp == NULL)
	{
	  image_error ("Cannot open `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }

  /* Customize libjpeg's error handling to call my_error_exit when an
     error is detected.  This function will perform a longjmp.  */
  cinfo.err = jpeg_std_error (&mgr.pub);
  mgr.pub.error_exit = my_error_exit;
  
  if ((rc = setjmp (mgr.setjmp_buffer)) != 0)
    {
      if (rc == 1)
	{
	  /* Called from my_error_exit.  Display a JPEG error.  */
	  char buffer[JMSG_LENGTH_MAX];
	  cinfo.err->format_message ((j_common_ptr) &cinfo, buffer);
	  image_error ("Error reading JPEG image `%s': %s", img->spec,
		       build_string (buffer));
	}
	  
      /* Close the input file and destroy the JPEG object.  */
      if (fp)
	fclose ((FILE *) fp);
      jpeg_destroy_decompress (&cinfo);

      /* If we already have an XImage, free that.  */
      x_destroy_x_image (ximg);

      /* Free pixmap and colors.  */
      x_clear_image (f, img);
      
      UNGCPRO;
      return 0;
    }

  /* Create the JPEG decompression object.  Let it read from fp.
	 Read the JPEG image header.  */
  jpeg_create_decompress (&cinfo);

  if (NILP (specified_data))
    jpeg_stdio_src (&cinfo, (FILE *) fp);
  else
    jpeg_memory_src (&cinfo, XSTRING (specified_data)->data,
		     STRING_BYTES (XSTRING (specified_data)));

  jpeg_read_header (&cinfo, TRUE);

  /* Customize decompression so that color quantization will be used.
	 Start decompression.  */
  cinfo.quantize_colors = TRUE;
  jpeg_start_decompress (&cinfo);
  width = img->width = cinfo.output_width;
  height = img->height = cinfo.output_height;

  /* Create X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    longjmp (mgr.setjmp_buffer, 2);

  /* Allocate colors.  When color quantization is used,
     cinfo.actual_number_of_colors has been set with the number of
     colors generated, and cinfo.colormap is a two-dimensional array
     of color indices in the range 0..cinfo.actual_number_of_colors.
     No more than 255 colors will be generated.  */
  {
    int i, ir, ig, ib;

    if (cinfo.out_color_components > 2)
      ir = 0, ig = 1, ib = 2;
    else if (cinfo.out_color_components > 1)
      ir = 0, ig = 1, ib = 0;
    else
      ir = 0, ig = 0, ib = 0;

    /* Use the color table mechanism because it handles colors that
       cannot be allocated nicely.  Such colors will be replaced with
       a default color, and we don't have to care about which colors
       can be freed safely, and which can't.  */
    init_color_table ();
    colors = (unsigned long *) alloca (cinfo.actual_number_of_colors
				       * sizeof *colors);
  
    for (i = 0; i < cinfo.actual_number_of_colors; ++i)
      {
	/* Multiply RGB values with 255 because X expects RGB values
	   in the range 0..0xffff.  */
	int r = cinfo.colormap[ir][i] << 8;
	int g = cinfo.colormap[ig][i] << 8;
	int b = cinfo.colormap[ib][i] << 8;
	colors[i] = lookup_rgb_color (f, r, g, b);
      }

    /* Remember those colors actually allocated.  */
    img->colors = colors_in_color_table (&img->ncolors);
    free_color_table ();
  }

  /* Read pixels.  */
  row_stride = width * cinfo.output_components;
  buffer = cinfo.mem->alloc_sarray ((j_common_ptr) &cinfo, JPOOL_IMAGE,
				    row_stride, 1);
  for (y = 0; y < height; ++y)
    {
      jpeg_read_scanlines (&cinfo, buffer, 1);
      for (x = 0; x < cinfo.output_width; ++x)
	XPutPixel (ximg, x, y, colors[buffer[0][x]]);
    }

  /* Clean up.  */
  jpeg_finish_decompress (&cinfo);
  jpeg_destroy_decompress (&cinfo);
  if (fp)
    fclose ((FILE *) fp);

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);
  
  /* Put the image into the pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  UNGCPRO;
  return 1;
}

#endif /* HAVE_JPEG */



/***********************************************************************
				 TIFF
 ***********************************************************************/

#if HAVE_TIFF

#include <tiffio.h>

static int tiff_image_p P_ ((Lisp_Object object));
static int tiff_load P_ ((struct frame *f, struct image *img));

/* The symbol `tiff' identifying images of this type.  */

Lisp_Object Qtiff;

/* Indices of image specification fields in tiff_format, below.  */

enum tiff_keyword_index
{
  TIFF_TYPE,
  TIFF_DATA,
  TIFF_FILE,
  TIFF_ASCENT,
  TIFF_MARGIN,
  TIFF_RELIEF,
  TIFF_ALGORITHM,
  TIFF_HEURISTIC_MASK,
  TIFF_MASK,
  TIFF_BACKGROUND,
  TIFF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword tiff_format[TIFF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversions",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `tiff'.  */

static struct image_type tiff_type =
{
  &Qtiff,
  tiff_image_p,
  tiff_load,
  x_clear_image,
  NULL
};


/* Return non-zero if OBJECT is a valid TIFF image specification.  */

static int
tiff_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[TIFF_LAST];
  bcopy (tiff_format, fmt, sizeof fmt);
  
  if (!parse_image_spec (object, fmt, TIFF_LAST, Qtiff))
    return 0;
  
  /* Must specify either the :data or :file keyword.  */
  return fmt[TIFF_FILE].count + fmt[TIFF_DATA].count == 1;
}


/* Reading from a memory buffer for TIFF images Based on the PNG
   memory source, but we have to provide a lot of extra functions.
   Blah.

   We really only need to implement read and seek, but I am not
   convinced that the TIFF library is smart enough not to destroy
   itself if we only hand it the function pointers we need to
   override.  */

typedef struct
{
  unsigned char *bytes;
  size_t len;
  int index;
}
tiff_memory_source;


static size_t
tiff_read_from_memory (data, buf, size)
     thandle_t data;
     tdata_t buf;
     tsize_t size;
{
  tiff_memory_source *src = (tiff_memory_source *) data;

  if (size > src->len - src->index)
    return (size_t) -1;
  bcopy (src->bytes + src->index, buf, size);
  src->index += size;
  return size;
}


static size_t
tiff_write_from_memory (data, buf, size)
     thandle_t data;
     tdata_t buf;
     tsize_t size;
{
  return (size_t) -1;
}


static toff_t
tiff_seek_in_memory (data, off, whence)
     thandle_t data;
     toff_t off;
     int whence;
{
  tiff_memory_source *src = (tiff_memory_source *) data;
  int idx;

  switch (whence)
    {
    case SEEK_SET:		/* Go from beginning of source.  */
      idx = off;
      break;
      
    case SEEK_END:		/* Go from end of source.  */
      idx = src->len + off;
      break;
      
    case SEEK_CUR:		/* Go from current position.  */
      idx = src->index + off;
      break;
      
    default:			/* Invalid `whence'.   */
      return -1;
    }
  
  if (idx > src->len || idx < 0)
    return -1;
  
  src->index = idx;
  return src->index;
}


static int
tiff_close_memory (data)
     thandle_t data;
{
  /* NOOP */
  return 0;
}


static int
tiff_mmap_memory (data, pbase, psize)
     thandle_t data;
     tdata_t *pbase;
     toff_t *psize;
{
  /* It is already _IN_ memory. */
  return 0;
}


static void
tiff_unmap_memory (data, base, size)
     thandle_t data;
     tdata_t base;
     toff_t size;
{
  /* We don't need to do this. */
}


static toff_t
tiff_size_of_memory (data)
     thandle_t data;
{
  return ((tiff_memory_source *) data)->len;
}


static void
tiff_error_handler (title, format, ap)
     const char *title, *format;
     va_list ap;
{
  char buf[512];
  int len;
  
  len = sprintf (buf, "TIFF error: %s ", title);
  vsprintf (buf + len, format, ap);
  add_to_log (buf, Qnil, Qnil);
}


static void
tiff_warning_handler (title, format, ap)
     const char *title, *format;
     va_list ap;
{
  char buf[512];
  int len;
  
  len = sprintf (buf, "TIFF warning: %s ", title);
  vsprintf (buf + len, format, ap);
  add_to_log (buf, Qnil, Qnil);
}


/* Load TIFF image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int
tiff_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  Lisp_Object specified_data;
  TIFF *tiff;
  int width, height, x, y;
  uint32 *buf;
  int rc;
  XImage *ximg;
  struct gcpro gcpro1;
  tiff_memory_source memsrc;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);
  file = Qnil;
  GCPRO1 (file);

  TIFFSetErrorHandler (tiff_error_handler);
  TIFFSetWarningHandler (tiff_warning_handler);

  if (NILP (specified_data))
    {
      /* Read from a file */
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}
	  
      /* Try to open the image file.  */
      tiff = TIFFOpen (XSTRING (file)->data, "r");
      if (tiff == NULL)
	{
	  image_error ("Cannot open `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }
  else
    {
      /* Memory source! */
      memsrc.bytes = XSTRING (specified_data)->data;
      memsrc.len = STRING_BYTES (XSTRING (specified_data));
      memsrc.index = 0;

      tiff = TIFFClientOpen ("memory_source", "r", &memsrc,
			     (TIFFReadWriteProc) tiff_read_from_memory,
			     (TIFFReadWriteProc) tiff_write_from_memory,
			     tiff_seek_in_memory,
			     tiff_close_memory,
			     tiff_size_of_memory,
			     tiff_mmap_memory,
			     tiff_unmap_memory);

      if (!tiff)
	{
	  image_error ("Cannot open memory source for `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }

  /* Get width and height of the image, and allocate a raster buffer
     of width x height 32-bit values.  */
  TIFFGetField (tiff, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField (tiff, TIFFTAG_IMAGELENGTH, &height);
  buf = (uint32 *) xmalloc (width * height * sizeof *buf);
  
  rc = TIFFReadRGBAImage (tiff, width, height, buf, 0);
  TIFFClose (tiff);
  if (!rc)
    {
      image_error ("Error reading TIFF image `%s'", img->spec, Qnil);
      xfree (buf);
      UNGCPRO;
      return 0;
    }

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    {
      xfree (buf);
      UNGCPRO;
      return 0;
    }

  /* Initialize the color table.  */
  init_color_table ();

  /* Process the pixel raster.  Origin is in the lower-left corner.  */
  for (y = 0; y < height; ++y)
    {
      uint32 *row = buf + y * width;
      
      for (x = 0; x < width; ++x)
	{
	  uint32 abgr = row[x];
	  int r = TIFFGetR (abgr) << 8;
	  int g = TIFFGetG (abgr) << 8;
	  int b = TIFFGetB (abgr) << 8;
	  XPutPixel (ximg, x, height - 1 - y, lookup_rgb_color (f, r, g, b)); 
	}
    }

  /* Remember the colors allocated for the image.  Free the color table.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
      
  img->width = width;
  img->height = height;

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);

  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  xfree (buf);

  UNGCPRO;
  return 1;
}

#endif /* HAVE_TIFF != 0 */



/***********************************************************************
				 GIF
 ***********************************************************************/

#if HAVE_GIF

#include <gif_lib.h>

static int gif_image_p P_ ((Lisp_Object object));
static int gif_load P_ ((struct frame *f, struct image *img));

/* The symbol `gif' identifying images of this type.  */

Lisp_Object Qgif;

/* Indices of image specification fields in gif_format, below.  */

enum gif_keyword_index
{
  GIF_TYPE,
  GIF_DATA,
  GIF_FILE,
  GIF_ASCENT,
  GIF_MARGIN,
  GIF_RELIEF,
  GIF_ALGORITHM,
  GIF_HEURISTIC_MASK,
  GIF_MASK,
  GIF_IMAGE,
  GIF_BACKGROUND,
  GIF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword gif_format[GIF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":image",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `gif'.  */

static struct image_type gif_type =
{
  &Qgif,
  gif_image_p,
  gif_load,
  x_clear_image,
  NULL
};


/* Return non-zero if OBJECT is a valid GIF image specification.  */

static int
gif_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[GIF_LAST];
  bcopy (gif_format, fmt, sizeof fmt);
  
  if (!parse_image_spec (object, fmt, GIF_LAST, Qgif))
    return 0;
  
  /* Must specify either the :data or :file keyword.  */
  return fmt[GIF_FILE].count + fmt[GIF_DATA].count == 1;
}


/* Reading a GIF image from memory
   Based on the PNG memory stuff to a certain extent. */

typedef struct
{
  unsigned char *bytes;
  size_t len;
  int index;
}
gif_memory_source;


/* Make the current memory source available to gif_read_from_memory.
   It's done this way because not all versions of libungif support
   a UserData field in the GifFileType structure.  */
static gif_memory_source *current_gif_memory_src;

static int
gif_read_from_memory (file, buf, len)
     GifFileType *file;
     GifByteType *buf;
     int len;
{
  gif_memory_source *src = current_gif_memory_src;

  if (len > src->len - src->index)
    return -1;

  bcopy (src->bytes + src->index, buf, len);
  src->index += len;
  return len;
}


/* Load GIF image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int
gif_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  Lisp_Object specified_data;
  int rc, width, height, x, y, i;
  XImage *ximg;
  ColorMapObject *gif_color_map;
  unsigned long pixel_colors[256];
  GifFileType *gif;
  struct gcpro gcpro1;
  Lisp_Object image;
  int ino, image_left, image_top, image_width, image_height;
  gif_memory_source memsrc;
  unsigned char *raster;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  specified_data = image_spec_value (img->spec, QCdata, NULL);
  file = Qnil;
  GCPRO1 (file);

  if (NILP (specified_data))
    {
      file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
	  UNGCPRO;
	  return 0;
	}
  
      /* Open the GIF file.  */
      gif = DGifOpenFileName (XSTRING (file)->data);
      if (gif == NULL)
	{
	  image_error ("Cannot open `%s'", file, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }
  else
    {
      /* Read from memory! */
      current_gif_memory_src = &memsrc;
      memsrc.bytes = XSTRING (specified_data)->data;
      memsrc.len = STRING_BYTES (XSTRING (specified_data));
      memsrc.index = 0;

      gif = DGifOpen(&memsrc, gif_read_from_memory);
      if (!gif)
	{
	  image_error ("Cannot open memory source `%s'", img->spec, Qnil);
	  UNGCPRO;
	  return 0;
	}
    }

  /* Read entire contents.  */
  rc = DGifSlurp (gif);
  if (rc == GIF_ERROR)
    {
      image_error ("Error reading `%s'", img->spec, Qnil);
      DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  image = image_spec_value (img->spec, QCindex, NULL);
  ino = INTEGERP (image) ? XFASTINT (image) : 0;
  if (ino >= gif->ImageCount)
    {
      image_error ("Invalid image number `%s' in image `%s'",
		   image, img->spec);
      DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  width = img->width = max (gif->SWidth, gif->Image.Left + gif->Image.Width);
  height = img->height = max (gif->SHeight, gif->Image.Top + gif->Image.Height);

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    {
      DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }
  
  /* Allocate colors.  */
  gif_color_map = gif->SavedImages[ino].ImageDesc.ColorMap;
  if (!gif_color_map)
    gif_color_map = gif->SColorMap;
  init_color_table ();
  bzero (pixel_colors, sizeof pixel_colors);
  
  for (i = 0; i < gif_color_map->ColorCount; ++i)
    {
      int r = gif_color_map->Colors[i].Red << 8;
      int g = gif_color_map->Colors[i].Green << 8;
      int b = gif_color_map->Colors[i].Blue << 8;
      pixel_colors[i] = lookup_rgb_color (f, r, g, b);
    }

  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();

  /* Clear the part of the screen image that are not covered by
     the image from the GIF file.  Full animated GIF support 
     requires more than can be done here (see the gif89 spec,
     disposal methods).  Let's simply assume that the part
     not covered by a sub-image is in the frame's background color.  */
  image_top = gif->SavedImages[ino].ImageDesc.Top;
  image_left = gif->SavedImages[ino].ImageDesc.Left;
  image_width = gif->SavedImages[ino].ImageDesc.Width;
  image_height = gif->SavedImages[ino].ImageDesc.Height;

  for (y = 0; y < image_top; ++y)
    for (x = 0; x < width; ++x)
      XPutPixel (ximg, x, y, FRAME_BACKGROUND_PIXEL (f));

  for (y = image_top + image_height; y < height; ++y)
    for (x = 0; x < width; ++x)
      XPutPixel (ximg, x, y, FRAME_BACKGROUND_PIXEL (f));

  for (y = image_top; y < image_top + image_height; ++y)
    {
      for (x = 0; x < image_left; ++x)
	XPutPixel (ximg, x, y, FRAME_BACKGROUND_PIXEL (f));
      for (x = image_left + image_width; x < width; ++x)
	XPutPixel (ximg, x, y, FRAME_BACKGROUND_PIXEL (f));
    }

  /* Read the GIF image into the X image.  We use a local variable
     `raster' here because RasterBits below is a char *, and invites
     problems with bytes >= 0x80.  */
  raster = (unsigned char *) gif->SavedImages[ino].RasterBits;
  
  if (gif->SavedImages[ino].ImageDesc.Interlace)
    {
      static int interlace_start[] = {0, 4, 2, 1};
      static int interlace_increment[] = {8, 8, 4, 2};
      int pass;
      int row = interlace_start[0];

      pass = 0;

      for (y = 0; y < image_height; y++)
	{
	  if (row >= image_height)
	    {
	      row = interlace_start[++pass];
	      while (row >= image_height)
		row = interlace_start[++pass];
	    }
	  
	  for (x = 0; x < image_width; x++)
	    {
	      int i = raster[(y * image_width) + x];
	      XPutPixel (ximg, x + image_left, row + image_top,
			 pixel_colors[i]);
	    }
	  
	  row += interlace_increment[pass];
	}
    }
  else
    {
      for (y = 0; y < image_height; ++y)
	for (x = 0; x < image_width; ++x)
	  {
	    int i = raster[y * image_width + x];
	    XPutPixel (ximg, x + image_left, y + image_top, pixel_colors[i]);
	  }
    }
  
  DGifCloseFile (gif);

  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);
  
  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
      
  UNGCPRO;
  return 1;
}

#endif /* HAVE_GIF != 0 */



/***********************************************************************
				Ghostscript
 ***********************************************************************/

static int gs_image_p P_ ((Lisp_Object object));
static int gs_load P_ ((struct frame *f, struct image *img));
static void gs_clear_image P_ ((struct frame *f, struct image *img));

/* The symbol `postscript' identifying images of this type.  */

Lisp_Object Qpostscript;

/* Keyword symbols.  */

Lisp_Object QCloader, QCbounding_box, QCpt_width, QCpt_height;

/* Indices of image specification fields in gs_format, below.  */

enum gs_keyword_index
{
  GS_TYPE,
  GS_PT_WIDTH,
  GS_PT_HEIGHT,
  GS_FILE,
  GS_LOADER,
  GS_BOUNDING_BOX,
  GS_ASCENT,
  GS_MARGIN,
  GS_RELIEF,
  GS_ALGORITHM,
  GS_HEURISTIC_MASK,
  GS_MASK,
  GS_BACKGROUND,
  GS_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword gs_format[GS_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":pt-width",		IMAGE_POSITIVE_INTEGER_VALUE,		1},
  {":pt-height",	IMAGE_POSITIVE_INTEGER_VALUE,		1},
  {":file",		IMAGE_STRING_VALUE,			1},
  {":loader",		IMAGE_FUNCTION_VALUE,			0},
  {":bounding-box",	IMAGE_DONT_CHECK_VALUE_TYPE,		1},
  {":ascent",		IMAGE_ASCENT_VALUE,			0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",		IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",	IMAGE_STRING_OR_NIL_VALUE,		0}
};

/* Structure describing the image type `ghostscript'.  */

static struct image_type gs_type =
{
  &Qpostscript,
  gs_image_p,
  gs_load,
  gs_clear_image,
  NULL
};


/* Free X resources of Ghostscript image IMG which is used on frame F.  */

static void
gs_clear_image (f, img)
     struct frame *f;
     struct image *img;
{
  /* IMG->data.ptr_val may contain a recorded colormap.  */
  xfree (img->data.ptr_val);
  x_clear_image (f, img);
}


/* Return non-zero if OBJECT is a valid Ghostscript image
   specification.  */

static int
gs_image_p (object)
     Lisp_Object object;
{
  struct image_keyword fmt[GS_LAST];
  Lisp_Object tem;
  int i;
  
  bcopy (gs_format, fmt, sizeof fmt);
  
  if (!parse_image_spec (object, fmt, GS_LAST, Qpostscript))
    return 0;

  /* Bounding box must be a list or vector containing 4 integers.  */
  tem = fmt[GS_BOUNDING_BOX].value;
  if (CONSP (tem))
    {
      for (i = 0; i < 4; ++i, tem = XCDR (tem))
	if (!CONSP (tem) || !INTEGERP (XCAR (tem)))
	  return 0;
      if (!NILP (tem))
	return 0;
    }
  else if (VECTORP (tem))
    {
      if (XVECTOR (tem)->size != 4)
	return 0;
      for (i = 0; i < 4; ++i)
	if (!INTEGERP (XVECTOR (tem)->contents[i]))
	  return 0;
    }
  else
    return 0;

  return 1;
}


/* Load Ghostscript image IMG for use on frame F.  Value is non-zero
   if successful.  */

static int
gs_load (f, img)
     struct frame *f;
     struct image *img;
{
  char buffer[100];
  Lisp_Object window_and_pixmap_id = Qnil, loader, pt_height, pt_width;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object frame;
  double in_width, in_height;
  Lisp_Object pixel_colors = Qnil;

  /* Compute pixel size of pixmap needed from the given size in the
     image specification.  Sizes in the specification are in pt.  1 pt
     = 1/72 in, xdpi and ydpi are stored in the frame's X display
     info.  */
  pt_width = image_spec_value (img->spec, QCpt_width, NULL);
  in_width = XFASTINT (pt_width) / 72.0;
  img->width = in_width * FRAME_X_DISPLAY_INFO (f)->resx;
  pt_height = image_spec_value (img->spec, QCpt_height, NULL);
  in_height = XFASTINT (pt_height) / 72.0;
  img->height = in_height * FRAME_X_DISPLAY_INFO (f)->resy;

  /* Create the pixmap.  */
  xassert (img->pixmap == None);
  img->pixmap = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			       img->width, img->height,
			       DefaultDepthOfScreen (FRAME_X_SCREEN (f)));

  if (!img->pixmap)
    {
      image_error ("Unable to create pixmap for `%s'", img->spec, Qnil);
      return 0;
    }
    
  /* Call the loader to fill the pixmap.  It returns a process object
     if successful.  We do not record_unwind_protect here because
     other places in redisplay like calling window scroll functions
     don't either.  Let the Lisp loader use `unwind-protect' instead.  */
  GCPRO2 (window_and_pixmap_id, pixel_colors);

  sprintf (buffer, "%lu %lu",
	   (unsigned long) FRAME_X_WINDOW (f),
	   (unsigned long) img->pixmap);
  window_and_pixmap_id = build_string (buffer);
  
  sprintf (buffer, "%lu %lu",
	   FRAME_FOREGROUND_PIXEL (f),
	   FRAME_BACKGROUND_PIXEL (f));
  pixel_colors = build_string (buffer);
  
  XSETFRAME (frame, f);
  loader = image_spec_value (img->spec, QCloader, NULL);
  if (NILP (loader))
    loader = intern ("gs-load-image");

  img->data.lisp_val = call6 (loader, frame, img->spec,
			      make_number (img->width),
			      make_number (img->height),
			      window_and_pixmap_id,
			      pixel_colors);
  UNGCPRO;
  return PROCESSP (img->data.lisp_val);
}


/* Kill the Ghostscript process that was started to fill PIXMAP on
   frame F.  Called from XTread_socket when receiving an event
   telling Emacs that Ghostscript has finished drawing.  */

void
x_kill_gs_process (pixmap, f)
     Pixmap pixmap;
     struct frame *f;
{
  struct image_cache *c = FRAME_X_IMAGE_CACHE (f);
  int class, i;
  struct image *img;

  /* Find the image containing PIXMAP.  */
  for (i = 0; i < c->used; ++i)
    if (c->images[i]->pixmap == pixmap)
      break;

  /* Should someone in between have cleared the image cache, for
     instance, give up.  */
  if (i == c->used)
    return;
  
  /* Kill the GS process.  We should have found PIXMAP in the image
     cache and its image should contain a process object.  */
  img = c->images[i];
  xassert (PROCESSP (img->data.lisp_val));
  Fkill_process (img->data.lisp_val, Qnil);
  img->data.lisp_val = Qnil;

  /* On displays with a mutable colormap, figure out the colors
     allocated for the image by looking at the pixels of an XImage for
     img->pixmap.  */
  class = FRAME_X_VISUAL (f)->class;
  if (class != StaticColor && class != StaticGray && class != TrueColor)
    {
      XImage *ximg;

      BLOCK_INPUT;

      /* Try to get an XImage for img->pixmep.  */
      ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
			0, 0, img->width, img->height, ~0, ZPixmap);
      if (ximg)
	{
	  int x, y;
	  
	  /* Initialize the color table.  */
	  init_color_table ();
      
	  /* For each pixel of the image, look its color up in the
	     color table.  After having done so, the color table will
	     contain an entry for each color used by the image.  */
	  for (y = 0; y < img->height; ++y)
	    for (x = 0; x < img->width; ++x)
	      {
		unsigned long pixel = XGetPixel (ximg, x, y);
		lookup_pixel_color (f, pixel);
	      }

	  /* Record colors in the image.  Free color table and XImage.  */
	  img->colors = colors_in_color_table (&img->ncolors);
	  free_color_table ();
	  XDestroyImage (ximg);

#if 0 /* This doesn't seem to be the case.  If we free the colors
	 here, we get a BadAccess later in x_clear_image when
	 freeing the colors.  */
	  /* We have allocated colors once, but Ghostscript has also
	     allocated colors on behalf of us.  So, to get the
	     reference counts right, free them once.  */
	  if (img->ncolors)
	    x_free_colors (f, img->colors, img->ncolors);
#endif
	}
      else
	image_error ("Cannot get X image of `%s'; colors will not be freed",
		     img->spec, Qnil);
      
      UNBLOCK_INPUT;
    }

  /* Now that we have the pixmap, compute mask and transform the
     image if requested.  */
  BLOCK_INPUT;
  postprocess_image (f, img);
  UNBLOCK_INPUT;
}



/***********************************************************************
                           Window properties
 ***********************************************************************/

DEFUN ("x-change-window-property", Fx_change_window_property,
       Sx_change_window_property, 2, 3, 0,
       doc: /* Change window property PROP to VALUE on the X window of FRAME.
PROP and VALUE must be strings.  FRAME nil or omitted means use the
selected frame.  Value is VALUE.  */)
     (prop, value, frame)
     Lisp_Object frame, prop, value;
{
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  CHECK_STRING (value);

  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), XSTRING (prop)->data, False);
  XChangeProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   prop_atom, XA_STRING, 8, PropModeReplace,
		   XSTRING (value)->data, XSTRING (value)->size);

  /* Make sure the property is set when we return.  */
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;

  return value;
}


DEFUN ("x-delete-window-property", Fx_delete_window_property,
       Sx_delete_window_property, 1, 2, 0,
       doc: /* Remove window property PROP from X window of FRAME.
FRAME nil or omitted means use the selected frame.  Value is PROP.  */)
     (prop, frame)
     Lisp_Object prop, frame;
{
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), XSTRING (prop)->data, False);
  XDeleteProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), prop_atom);

  /* Make sure the property is removed when we return.  */
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;

  return prop;
}


DEFUN ("x-window-property", Fx_window_property, Sx_window_property,
       1, 2, 0,
       doc: /* Value is the value of window property PROP on FRAME.
If FRAME is nil or omitted, use the selected frame.  Value is nil
if FRAME hasn't a property with name PROP or if PROP has no string
value.  */)
     (prop, frame)
     Lisp_Object prop, frame;
{
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;
  int rc;
  Lisp_Object prop_value = Qnil;
  char *tmp_data = NULL;
  Atom actual_type;
  int actual_format;
  unsigned long actual_size, bytes_remaining;

  CHECK_STRING (prop);
  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_X_DISPLAY (f), XSTRING (prop)->data, False);
  rc = XGetWindowProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			   prop_atom, 0, 0, False, XA_STRING,
			   &actual_type, &actual_format, &actual_size,
			   &bytes_remaining, (unsigned char **) &tmp_data);
  if (rc == Success)
    {
      int size = bytes_remaining;

      XFree (tmp_data);
      tmp_data = NULL;

      rc = XGetWindowProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			       prop_atom, 0, bytes_remaining,
			       False, XA_STRING,
			       &actual_type, &actual_format, 
			       &actual_size, &bytes_remaining, 
			       (unsigned char **) &tmp_data);
      if (rc == Success && tmp_data)
	prop_value = make_string (tmp_data, size);

      XFree (tmp_data);
    }

  UNBLOCK_INPUT;
  return prop_value;
}



/***********************************************************************
				Busy cursor
 ***********************************************************************/

/* If non-null, an asynchronous timer that, when it expires, displays
   an hourglass cursor on all frames.  */

static struct atimer *hourglass_atimer;

/* Non-zero means an hourglass cursor is currently shown.  */

static int hourglass_shown_p;

/* Number of seconds to wait before displaying an hourglass cursor.  */

static Lisp_Object Vhourglass_delay;

/* Default number of seconds to wait before displaying an hourglass
   cursor.  */

#define DEFAULT_HOURGLASS_DELAY 1

/* Function prototypes.  */

static void show_hourglass P_ ((struct atimer *));
static void hide_hourglass P_ ((void));


/* Cancel a currently active hourglass timer, and start a new one.  */

void
start_hourglass ()
{
  EMACS_TIME delay;
  int secs, usecs = 0;
  
  cancel_hourglass ();

  if (INTEGERP (Vhourglass_delay)
      && XINT (Vhourglass_delay) > 0)
    secs = XFASTINT (Vhourglass_delay);
  else if (FLOATP (Vhourglass_delay)
	   && XFLOAT_DATA (Vhourglass_delay) > 0)
    {
      Lisp_Object tem;
      tem = Ftruncate (Vhourglass_delay, Qnil);
      secs = XFASTINT (tem);
      usecs = (XFLOAT_DATA (Vhourglass_delay) - secs) * 1000000;
    }
  else
    secs = DEFAULT_HOURGLASS_DELAY;
  
  EMACS_SET_SECS_USECS (delay, secs, usecs);
  hourglass_atimer = start_atimer (ATIMER_RELATIVE, delay,
				     show_hourglass, NULL);
}


/* Cancel the hourglass cursor timer if active, hide a busy cursor if
   shown.  */

void
cancel_hourglass ()
{
  if (hourglass_atimer)
    {
      cancel_atimer (hourglass_atimer);
      hourglass_atimer = NULL;
    }
  
  if (hourglass_shown_p)
    hide_hourglass ();
}


/* Timer function of hourglass_atimer.  TIMER is equal to
   hourglass_atimer.

   Display an hourglass pointer on all frames by mapping the frames'
   hourglass_window.  Set the hourglass_p flag in the frames'
   output_data.x structure to indicate that an hourglass cursor is
   shown on the frames.  */

static void
show_hourglass (timer)
     struct atimer *timer;
{
  /* The timer implementation will cancel this timer automatically
     after this function has run.  Set hourglass_atimer to null
     so that we know the timer doesn't have to be canceled.  */
  hourglass_atimer = NULL;

  if (!hourglass_shown_p)
    {
      Lisp_Object rest, frame;
  
      BLOCK_INPUT;
  
      FOR_EACH_FRAME (rest, frame)
	{
	  struct frame *f = XFRAME (frame);
	  
	  if (FRAME_LIVE_P (f) && FRAME_X_P (f) && FRAME_X_DISPLAY (f))
	    {
	      Display *dpy = FRAME_X_DISPLAY (f);
	      
#ifdef USE_X_TOOLKIT
	      if (f->output_data.x->widget)
#else
	      if (FRAME_OUTER_WINDOW (f))
#endif
		{
		  f->output_data.x->hourglass_p = 1;
	
		  if (!f->output_data.x->hourglass_window)
		    {
		      unsigned long mask = CWCursor;
		      XSetWindowAttributes attrs;
	    
		      attrs.cursor = f->output_data.x->hourglass_cursor;
	    
		      f->output_data.x->hourglass_window
			= XCreateWindow (dpy, FRAME_OUTER_WINDOW (f),
					 0, 0, 32000, 32000, 0, 0,
					 InputOnly,
					 CopyFromParent,
					 mask, &attrs);
		    }
	
		  XMapRaised (dpy, f->output_data.x->hourglass_window);
		  XFlush (dpy);
		}
	    }
	}

      hourglass_shown_p = 1;
      UNBLOCK_INPUT;
    }
}


/* Hide the hourglass pointer on all frames, if it is currently
   shown.  */

static void
hide_hourglass ()
{
  if (hourglass_shown_p)
    {
      Lisp_Object rest, frame;

      BLOCK_INPUT;
      FOR_EACH_FRAME (rest, frame)
	{
	  struct frame *f = XFRAME (frame);
      
	  if (FRAME_X_P (f)
	      /* Watch out for newly created frames.  */
	      && f->output_data.x->hourglass_window)
	    {
	      XUnmapWindow (FRAME_X_DISPLAY (f),
			    f->output_data.x->hourglass_window);
	      /* Sync here because XTread_socket looks at the
		 hourglass_p flag that is reset to zero below.  */
	      XSync (FRAME_X_DISPLAY (f), False);
	      f->output_data.x->hourglass_p = 0;
	    }
	}

      hourglass_shown_p = 0;
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
				Tool tips
 ***********************************************************************/

static Lisp_Object x_create_tip_frame P_ ((struct x_display_info *,
					   Lisp_Object, Lisp_Object));
static void compute_tip_xy P_ ((struct frame *, Lisp_Object, Lisp_Object,
				Lisp_Object, int, int, int *, int *));
     
/* The frame of a currently visible tooltip.  */

Lisp_Object tip_frame;

/* If non-nil, a timer started that hides the last tooltip when it
   fires.  */

Lisp_Object tip_timer;
Window tip_window;

/* If non-nil, a vector of 3 elements containing the last args
   with which x-show-tip was called.  See there.  */

Lisp_Object last_show_tip_args;

/* Maximum size for tooltips; a cons (COLUMNS . ROWS).  */

Lisp_Object Vx_max_tooltip_size;


static Lisp_Object
unwind_create_tip_frame (frame)
     Lisp_Object frame;
{
  Lisp_Object deleted;

  deleted = unwind_create_frame (frame);
  if (EQ (deleted, Qt))
    {
      tip_window = None;
      tip_frame = Qnil;
    }
  
  return deleted;
}


/* Create a frame for a tooltip on the display described by DPYINFO.
   PARMS is a list of frame parameters.  TEXT is the string to
   display in the tip frame.  Value is the frame.

   Note that functions called here, esp. x_default_parameter can
   signal errors, for instance when a specified color name is
   undefined.  We have to make sure that we're in a consistent state
   when this happens.  */

static Lisp_Object
x_create_tip_frame (dpyinfo, parms, text)
     struct x_display_info *dpyinfo;
     Lisp_Object parms, text;
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  long window_prompting = 0;
  int width, height;
  int count = BINDING_STACK_SIZE ();
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct kboard *kb;
  int face_change_count_before = face_change_count;
  Lisp_Object buffer;
  struct buffer *old_buffer;

  check_x ();

  /* Use this general default value to start with until we know if
     this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  /* Get the name of the frame to use for resource lookup.  */
  name = x_get_arg (dpyinfo, parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && !EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");
  Vx_resource_name = name;

  frame = Qnil;
  GCPRO3 (parms, name, frame);
  f = make_frame (1);
  XSETFRAME (frame, f);

  buffer = Fget_buffer_create (build_string (" *tip*"));
  Fset_window_buffer (FRAME_ROOT_WINDOW (f), buffer);
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (buffer));
  current_buffer->truncate_lines = Qnil;
  Ferase_buffer ();
  Finsert (1, &text);
  set_buffer_internal_1 (old_buffer);
  
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;
  record_unwind_protect (unwind_create_tip_frame, frame);

  /* By setting the output method, we're essentially saying that
     the frame is live, as per FRAME_LIVE_P.  If we get a signal
     from this point on, x_destroy_window might screw up reference
     counts etc.  */
  f->output_method = output_x_window;
  f->output_data.x = (struct x_output *) xmalloc (sizeof (struct x_output));
  bzero (f->output_data.x, sizeof (struct x_output));
  f->output_data.x->icon_bitmap = -1;
  f->output_data.x->fontset = -1;
  f->output_data.x->scroll_bar_foreground_pixel = -1;
  f->output_data.x->scroll_bar_background_pixel = -1;
#ifdef USE_TOOLKIT_SCROLL_BARS
  f->output_data.x->scroll_bar_top_shadow_pixel = -1;
  f->output_data.x->scroll_bar_bottom_shadow_pixel = -1;
#endif /* USE_TOOLKIT_SCROLL_BARS */
  f->icon_name = Qnil;
  FRAME_X_DISPLAY_INFO (f) = dpyinfo;
#if GLYPH_DEBUG
  image_cache_refcount = FRAME_X_IMAGE_CACHE (f)->refcount;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif
  f->output_data.x->parent_desc = FRAME_X_DISPLAY_INFO (f)->root_window;
  f->output_data.x->explicit_parent = 0;

  /* These colors will be set anyway later, but it's important
     to get the color reference counts right, so initialize them!  */
  {
    Lisp_Object black;
    struct gcpro gcpro1;
    
    black = build_string ("black");
    GCPRO1 (black);
    f->output_data.x->foreground_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->background_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->cursor_foreground_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->border_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    f->output_data.x->mouse_pixel
      = x_decode_color (f, black, BLACK_PIX_DEFAULT (f));
    UNGCPRO;
  }

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (dpyinfo->x_id_name);
      f->explicit_name = 0;
    }
  else
    {
      f->name = name;
      f->explicit_name = 1;
      /* use the frame's title when getting resources for this frame.  */
      specbind (Qx_resource_name, name);
    }

  /* Extract the window parameters from the supplied values that are
     needed to determine window geometry.  */
  {
    Lisp_Object font;

    font = x_get_arg (dpyinfo, parms, Qfont, "font", "Font", RES_TYPE_STRING);

    BLOCK_INPUT;
    /* First, try whatever font the caller has specified.  */
    if (STRINGP (font))
      {
	tem = Fquery_fontset (font, Qnil);
	if (STRINGP (tem))
	  font = x_new_fontset (f, XSTRING (tem)->data);
	else
	  font = x_new_font (f, XSTRING (font)->data);
      }
    
    /* Try out a font which we hope has bold and italic variations.  */
    if (!STRINGP (font))
      font = x_new_font (f, "-adobe-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-1");
    if (!STRINGP (font))
      font = x_new_font (f, "-misc-fixed-medium-r-normal-*-*-140-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-*-medium-r-normal-*-*-140-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      /* This was formerly the first thing tried, but it finds too many fonts
	 and takes too long.  */
      font = x_new_font (f, "-*-*-medium-r-*-*-*-*-*-*-c-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (! STRINGP (font))
      font = x_new_font (f, "-*-fixed-*-*-*-*-*-140-*-*-c-*-iso8859-1");
    UNBLOCK_INPUT;
    if (! STRINGP (font))
      font = build_string ("fixed");

    x_default_parameter (f, parms, Qfont, font,
			 "font", "Font", RES_TYPE_STRING);
  }

  x_default_parameter (f, parms, Qborder_width, make_number (2),
		       "borderWidth", "BorderWidth", RES_TYPE_NUMBER);
  
  /* This defaults to 2 in order to match xterm.  We recognize either
     internalBorderWidth or internalBorder (which is what xterm calls
     it).  */
  if (NILP (Fassq (Qinternal_border_width, parms)))
    {
      Lisp_Object value;

      value = x_get_arg (dpyinfo, parms, Qinternal_border_width,
			 "internalBorder", "internalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }

  x_default_parameter (f, parms, Qinternal_border_width, make_number (1),
		       "internalBorderWidth", "internalBorderWidth",
		       RES_TYPE_NUMBER);

  /* Also do the stuff which must be set before the window exists.  */
  x_default_parameter (f, parms, Qforeground_color, build_string ("black"),
		       "foreground", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qbackground_color, build_string ("white"),
		       "background", "Background", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qmouse_color, build_string ("black"),
		       "pointerColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qcursor_color, build_string ("black"),
		       "cursorColor", "Foreground", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qborder_color, build_string ("black"),
		       "borderColor", "BorderColor", RES_TYPE_STRING);

  /* Init faces before x_default_parameter is called for scroll-bar
     parameters because that function calls x_set_scroll_bar_width,
     which calls change_frame_size, which calls Fset_window_buffer,
     which runs hooks, which call Fvertical_motion.  At the end, we
     end up in init_iterator with a null face cache, which should not
     happen.  */
  init_frame_faces (f);
  
  f->output_data.x->parent_desc = FRAME_X_DISPLAY_INFO (f)->root_window;
  window_prompting = x_figure_window_size (f, parms);

  if (window_prompting & XNegative)
    {
      if (window_prompting & YNegative)
	f->output_data.x->win_gravity = SouthEastGravity;
      else
	f->output_data.x->win_gravity = NorthEastGravity;
    }
  else
    {
      if (window_prompting & YNegative)
	f->output_data.x->win_gravity = SouthWestGravity;
      else
	f->output_data.x->win_gravity = NorthWestGravity;
    }

  f->output_data.x->size_hint_flags = window_prompting;
  {
    XSetWindowAttributes attrs;
    unsigned long mask;
    
    BLOCK_INPUT;
    mask = CWBackPixel | CWOverrideRedirect | CWEventMask;
    if (DoesSaveUnders (dpyinfo->screen))
      mask |= CWSaveUnder;
    
    /* Window managers look at the override-redirect flag to determine
       whether or net to give windows a decoration (Xlib spec, chapter
       3.2.8).  */
    attrs.override_redirect = True;
    attrs.save_under = True;
    attrs.background_pixel = FRAME_BACKGROUND_PIXEL (f);
    /* Arrange for getting MapNotify and UnmapNotify events.  */
    attrs.event_mask = StructureNotifyMask;
    tip_window
      = FRAME_X_WINDOW (f)
      = XCreateWindow (FRAME_X_DISPLAY (f),
		       FRAME_X_DISPLAY_INFO (f)->root_window,
		       /* x, y, width, height */
		       0, 0, 1, 1,
		       /* Border.  */
		       1,
		       CopyFromParent, InputOutput, CopyFromParent,
		       mask, &attrs);
    UNBLOCK_INPUT;
  }

  x_make_gc (f);

  x_default_parameter (f, parms, Qauto_raise, Qnil,
		       "autoRaise", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qauto_lower, Qnil,
		       "autoLower", "AutoRaiseLower", RES_TYPE_BOOLEAN);
  x_default_parameter (f, parms, Qcursor_type, Qbox,
		       "cursorType", "CursorType", RES_TYPE_SYMBOL);

  /* Dimensions, especially f->height, must be done via change_frame_size.
     Change will not be effected unless different from the current
     f->height.  */
  width = f->width;
  height = f->height;
  f->height = 0;
  SET_FRAME_WIDTH (f, 0);
  change_frame_size (f, height, width, 1, 0, 0);

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.

     Frame parameters may be changed if .Xdefaults contains
     specifications for the default font.  For example, if there is an
     `Emacs.default.attributeBackground: pink', the `background-color'
     attribute of the frame get's set, which let's the internal border
     of the tooltip frame appear in pink.  Prevent this.  */
  {
    Lisp_Object bg = Fframe_parameter (frame, Qbackground_color);

    /* Set tip_frame here, so that */
    tip_frame = frame;
    call1 (Qface_set_after_frame_default, frame);
    
    if (!EQ (bg, Fframe_parameter (frame, Qbackground_color)))
      Fmodify_frame_parameters (frame, Fcons (Fcons (Qbackground_color, bg),
					      Qnil));
  }
  
  f->no_split = 1;

  UNGCPRO;

  /* It is now ok to make the frame official even if we get an error
     below.  And the frame needs to be on Vframe_list or making it
     visible won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);

  /* Now that the frame is official, it counts as a reference to
     its display.  */
  FRAME_X_DISPLAY_INFO (f)->reference_count++;

  /* Setting attributes of faces of the tooltip frame from resources
     and similar will increment face_change_count, which leads to the
     clearing of all current matrices.  Since this isn't necessary
     here, avoid it by resetting face_change_count to the value it
     had before we created the tip frame.  */
  face_change_count = face_change_count_before;

  /* Discard the unwind_protect.  */
  return unbind_to (count, frame);
}


/* Compute where to display tip frame F.  PARMS is the list of frame
   parameters for F.  DX and DY are specified offsets from the current
   location of the mouse.  WIDTH and HEIGHT are the width and height
   of the tooltip.  Return coordinates relative to the root window of
   the display in *ROOT_X, and *ROOT_Y.  */

static void
compute_tip_xy (f, parms, dx, dy, width, height, root_x, root_y)
     struct frame *f;
     Lisp_Object parms, dx, dy;
     int width, height;
     int *root_x, *root_y;
{
  Lisp_Object left, top;
  int win_x, win_y;
  Window root, child;
  unsigned pmask;
  
  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));
  
  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if (!INTEGERP (left) || !INTEGERP (top))
    {
      BLOCK_INPUT;
      XQueryPointer (FRAME_X_DISPLAY (f), FRAME_X_DISPLAY_INFO (f)->root_window,
		     &root, &child, root_x, root_y, &win_x, &win_y, &pmask);
      UNBLOCK_INPUT;
    }

  if (INTEGERP (top))
    *root_y = XINT (top);
  else if (*root_y + XINT (dy) - height < 0)
    *root_y -= XINT (dy);
  else
    {
      *root_y -= height;
      *root_y += XINT (dy);
    }

  if (INTEGERP (left))
    *root_x = XINT (left);
  else if (*root_x + XINT (dx) + width <= FRAME_X_DISPLAY_INFO (f)->width)
    /* It fits to the right of the pointer.  */
    *root_x += XINT (dx);
  else if (width + XINT (dx) <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XINT (dx);
  else
    /* Put it left-justified on the screen--it ought to fit that way.  */
    *root_x = 0;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a "tooltip" window on frame FRAME.
A tooltip window is a small X window displaying a string.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be used to
change the tooltip's appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout of 5 seconds.

If the list of frame parameters PARAMS contains a `left' parameters,
the tooltip is displayed at that x-position.  Otherwise it is
displayed at the mouse position, with offset DX added (default is 5 if
DX isn't specified).  Likewise for the y-position; if a `top' frame
parameter is specified, it determines the y-position of the tooltip
window, otherwise it is displayed at the mouse position, with offset
DY added (default is -10).

A tooltip's maximum size is specified by `x-max-tooltip-size'.
Text larger than the specified size is clipped.  */)
     (string, frame, parms, timeout, dx, dy)
     Lisp_Object string, frame, parms, timeout, dx, dy;
{
  struct frame *f;
  struct window *w;
  int root_x, root_y;
  struct buffer *old_buffer;
  struct text_pos pos;
  int i, width, height;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  int count = BINDING_STACK_SIZE ();
  
  specbind (Qinhibit_redisplay, Qt);

  GCPRO4 (string, parms, frame, timeout);

  CHECK_STRING (string);
  f = check_x_frame (frame);
  if (NILP (timeout))
    timeout = make_number (5);
  else
    CHECK_NATNUM (timeout);
  
  if (NILP (dx))
    dx = make_number (5);
  else
    CHECK_NUMBER (dx);
  
  if (NILP (dy))
    dy = make_number (-10);
  else
    CHECK_NUMBER (dy);

  if (NILP (last_show_tip_args))
    last_show_tip_args = Fmake_vector (make_number (3), Qnil);

  if (!NILP (tip_frame))
    {
      Lisp_Object last_string = AREF (last_show_tip_args, 0);
      Lisp_Object last_frame = AREF (last_show_tip_args, 1);
      Lisp_Object last_parms = AREF (last_show_tip_args, 2);

      if (EQ (frame, last_frame)
	  && !NILP (Fequal (last_string, string))
	  && !NILP (Fequal (last_parms, parms)))
	{
	  struct frame *f = XFRAME (tip_frame);
	  
	  /* Only DX and DY have changed.  */
	  if (!NILP (tip_timer))
	    {
	      Lisp_Object timer = tip_timer;
	      tip_timer = Qnil;
	      call1 (Qcancel_timer, timer);
	    }

	  BLOCK_INPUT;
	  compute_tip_xy (f, parms, dx, dy, PIXEL_WIDTH (f),
			  PIXEL_HEIGHT (f), &root_x, &root_y);
	  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		       root_x, root_y);
	  UNBLOCK_INPUT;
	  goto start_timer;
	}
    }

  /* Hide a previous tip, if any.  */
  Fx_hide_tip ();

  ASET (last_show_tip_args, 0, string);
  ASET (last_show_tip_args, 1, frame);
  ASET (last_show_tip_args, 2, parms);

  /* Add default values to frame parameters.  */
  if (NILP (Fassq (Qname, parms)))
    parms = Fcons (Fcons (Qname, build_string ("tooltip")), parms);
  if (NILP (Fassq (Qinternal_border_width, parms)))
    parms = Fcons (Fcons (Qinternal_border_width, make_number (3)), parms);
  if (NILP (Fassq (Qborder_width, parms)))
    parms = Fcons (Fcons (Qborder_width, make_number (1)), parms);
  if (NILP (Fassq (Qborder_color, parms)))
    parms = Fcons (Fcons (Qborder_color, build_string ("lightyellow")), parms);
  if (NILP (Fassq (Qbackground_color, parms)))
    parms = Fcons (Fcons (Qbackground_color, build_string ("lightyellow")),
		   parms);

  /* Create a frame for the tooltip, and record it in the global
     variable tip_frame.  */
  frame = x_create_tip_frame (FRAME_X_DISPLAY_INFO (f), parms, string);
  f = XFRAME (frame);

  /* Set up the frame's root window.  */
  w = XWINDOW (FRAME_ROOT_WINDOW (f));
  w->left = w->top = make_number (0);
  
  if (CONSP (Vx_max_tooltip_size)
      && INTEGERP (XCAR (Vx_max_tooltip_size))
      && XINT (XCAR (Vx_max_tooltip_size)) > 0
      && INTEGERP (XCDR (Vx_max_tooltip_size))
      && XINT (XCDR (Vx_max_tooltip_size)) > 0)
    {
      w->width = XCAR (Vx_max_tooltip_size);
      w->height = XCDR (Vx_max_tooltip_size);
    }
  else
    {
      w->width = make_number (80);
      w->height = make_number (40);
    }
  
  f->window_width = XINT (w->width);
  adjust_glyphs (f);
  w->pseudo_window_p = 1;

  /* Display the tooltip text in a temporary buffer.  */
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (XWINDOW (FRAME_ROOT_WINDOW (f))->buffer));
  current_buffer->truncate_lines = Qnil;
  clear_glyph_matrix (w->desired_matrix);
  clear_glyph_matrix (w->current_matrix);
  SET_TEXT_POS (pos, BEGV, BEGV_BYTE);
  try_window (FRAME_ROOT_WINDOW (f), pos);

  /* Compute width and height of the tooltip.  */
  width = height = 0;
  for (i = 0; i < w->desired_matrix->nrows; ++i)
    {
      struct glyph_row *row = &w->desired_matrix->rows[i];
      struct glyph *last;
      int row_width;

      /* Stop at the first empty row at the end.  */
      if (!row->enabled_p || !row->displays_text_p)
	break;

      /* Let the row go over the full width of the frame.  */
      row->full_width_p = 1;

      /* There's a glyph at the end of rows that is used to place
	 the cursor there.  Don't include the width of this glyph.  */
      if (row->used[TEXT_AREA])
	{
	  last = &row->glyphs[TEXT_AREA][row->used[TEXT_AREA] - 1];
	  row_width = row->pixel_width - last->pixel_width;
	}
      else
	row_width = row->pixel_width;
      
      height += row->height;
      width = max (width, row_width);
    }

  /* Add the frame's internal border to the width and height the X
     window should have.  */
  height += 2 * FRAME_INTERNAL_BORDER_WIDTH (f);
  width += 2 * FRAME_INTERNAL_BORDER_WIDTH (f);

  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  compute_tip_xy (f, parms, dx, dy, width, height, &root_x, &root_y);

  BLOCK_INPUT;
  XMoveResizeWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     root_x, root_y, width, height);
  XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
  UNBLOCK_INPUT;
  
  /* Draw into the window.  */
  w->must_be_updated_p = 1;
  update_single_window (w, 1);

  /* Restore original current buffer.  */
  set_buffer_internal_1 (old_buffer);
  windows_or_buffers_changed = old_windows_or_buffers_changed;

 start_timer:
  /* Let the tip disappear after timeout seconds.  */
  tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
		     intern ("x-hide-tip"));

  UNGCPRO;
  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
       doc: /* Hide the current tooltip window, if there is any.
Value is t if tooltip was open, nil otherwise.  */)
     ()
{
  int count;
  Lisp_Object deleted, frame, timer;
  struct gcpro gcpro1, gcpro2;

  /* Return quickly if nothing to do.  */
  if (NILP (tip_timer) && NILP (tip_frame))
    return Qnil;
  
  frame = tip_frame;
  timer = tip_timer;
  GCPRO2 (frame, timer);
  tip_frame = tip_timer = deleted = Qnil;
  
  count = BINDING_STACK_SIZE ();
  specbind (Qinhibit_redisplay, Qt);
  specbind (Qinhibit_quit, Qt);
  
  if (!NILP (timer))
    call1 (Qcancel_timer, timer);

  if (FRAMEP (frame))
    {
      Fdelete_frame (frame, Qnil);
      deleted = Qt;

#ifdef USE_LUCID
      /* Bloodcurdling hack alert: The Lucid menu bar widget's
	 redisplay procedure is not called when a tip frame over menu
	 items is unmapped.  Redisplay the menu manually...  */
      {
	struct frame *f = SELECTED_FRAME ();
	Widget w = f->output_data.x->menubar_widget;
	extern void xlwmenu_redisplay P_ ((Widget));

	if (!DoesSaveUnders (FRAME_X_DISPLAY_INFO (f)->screen)
	    && w != NULL)
	  {
	    BLOCK_INPUT;
	    xlwmenu_redisplay (w);
	    UNBLOCK_INPUT;
	  }
      }
#endif /* USE_LUCID */
    }

  UNGCPRO;
  return unbind_to (count, deleted);
}



/***********************************************************************
			File selection dialog
 ***********************************************************************/

#ifdef USE_MOTIF

/* Callback for "OK" and "Cancel" on file selection dialog.  */

static void
file_dialog_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer call_data, client_data;
{
  int *result = (int *) client_data;
  XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *) call_data;
  *result = cb->reason;
}


/* Callback for unmapping a file selection dialog.  This is used to
   capture the case where a dialog is closed via a window manager's
   closer button, for example. Using a XmNdestroyCallback didn't work
   in this case.  */

static void
file_dialog_unmap_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer call_data, client_data;
{
  int *result = (int *) client_data;
  *result = XmCR_CANCEL;
}


DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 4, 0,
       doc: /* Read file name, prompting with PROMPT in directory DIR.
Use a file selection dialog.
Select DEFAULT-FILENAME in the dialog's file selection box, if
specified.  Don't let the user enter a file name in the file
selection dialog's entry field, if MUSTMATCH is non-nil.  */)
     (prompt, dir, default_filename, mustmatch)
     Lisp_Object prompt, dir, default_filename, mustmatch;
{
  int result;
  struct frame *f = SELECTED_FRAME ();
  Lisp_Object file = Qnil;
  Widget dialog, text, list, help;
  Arg al[10];
  int ac = 0;
  extern XtAppContext Xt_app_con;
  XmString dir_xmstring, pattern_xmstring;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

  GCPRO5 (prompt, dir, default_filename, mustmatch, file);
  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Prevent redisplay.  */
  specbind (Qinhibit_redisplay, Qt);

  BLOCK_INPUT;

  /* Create the dialog with PROMPT as title, using DIR as initial
     directory and using "*" as pattern.  */
  dir = Fexpand_file_name (dir, Qnil);
  dir_xmstring = XmStringCreateLocalized (XSTRING (dir)->data);
  pattern_xmstring = XmStringCreateLocalized ("*");
    
  XtSetArg (al[ac], XmNtitle, XSTRING (prompt)->data); ++ac;
  XtSetArg (al[ac], XmNdirectory, dir_xmstring); ++ac;
  XtSetArg (al[ac], XmNpattern, pattern_xmstring); ++ac;
  XtSetArg (al[ac], XmNresizePolicy, XmRESIZE_GROW); ++ac;
  XtSetArg (al[ac], XmNdialogStyle, XmDIALOG_APPLICATION_MODAL); ++ac;
  dialog = XmCreateFileSelectionDialog (f->output_data.x->widget,
					"fsb", al, ac);
  XmStringFree (dir_xmstring);
  XmStringFree (pattern_xmstring);

  /* Add callbacks for OK and Cancel.  */
  XtAddCallback (dialog, XmNokCallback, file_dialog_cb,
		 (XtPointer) &result);
  XtAddCallback (dialog, XmNcancelCallback, file_dialog_cb,
		 (XtPointer) &result);
  XtAddCallback (dialog, XmNunmapCallback, file_dialog_unmap_cb,
		 (XtPointer) &result);

  /* Disable the help button since we can't display help.  */
  help = XmFileSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON);
  XtSetSensitive (help, False);

  /* Mark OK button as default.  */ 
  XtVaSetValues (XmFileSelectionBoxGetChild (dialog, XmDIALOG_OK_BUTTON),
		 XmNshowAsDefault, True, NULL);

  /* If MUSTMATCH is non-nil, disable the file entry field of the
     dialog, so that the user must select a file from the files list
     box.  We can't remove it because we wouldn't have a way to get at
     the result file name, then.  */
  text = XmFileSelectionBoxGetChild (dialog, XmDIALOG_TEXT);
  if (!NILP (mustmatch))
    {
      Widget label;
      label = XmFileSelectionBoxGetChild (dialog, XmDIALOG_SELECTION_LABEL);
      XtSetSensitive (text, False);
      XtSetSensitive (label, False);
    }

  /* Manage the dialog, so that list boxes get filled.  */
  XtManageChild (dialog);

  /* Select DEFAULT_FILENAME in the files list box.  DEFAULT_FILENAME
     must include the path for this to work.  */
  list = XmFileSelectionBoxGetChild (dialog, XmDIALOG_LIST);
  if (STRINGP (default_filename))
    {
      XmString default_xmstring;
      int item_pos;

      default_xmstring
	= XmStringCreateLocalized (XSTRING (default_filename)->data);

      if (!XmListItemExists (list, default_xmstring))
	{
	  /* Add a new item if DEFAULT_FILENAME is not in the list.  */
	  XmListAddItem (list, default_xmstring, 0);
	  item_pos = 0;
	}
      else
	item_pos = XmListItemPos (list, default_xmstring);
      XmStringFree (default_xmstring);

      /* Select the item and scroll it into view.  */
      XmListSelectPos (list, item_pos, True);
      XmListSetPos (list, item_pos);
    }

  /* Process events until the user presses Cancel or OK.  Block
     and unblock input here so that we get a chance of processing
     expose events.  */
  UNBLOCK_INPUT;
  result = 0;
  while (result == 0)
    {
      BLOCK_INPUT;
      XtAppProcessEvent (Xt_app_con, XtIMAll);
      UNBLOCK_INPUT;
    }
  BLOCK_INPUT;

  /* Get the result.  */
  if (result == XmCR_OK)
    {
      XmString text;
      String data;
      
      XtVaGetValues (dialog, XmNtextString, &text, NULL);
      XmStringGetLtoR (text, XmFONTLIST_DEFAULT_TAG, &data);
      XmStringFree (text);
      file = build_string (data);
      XtFree (data);
    }
  else
    file = Qnil;

  /* Clean up.  */
  XtUnmanageChild (dialog);
  XtDestroyWidget (dialog);
  UNBLOCK_INPUT;
  UNGCPRO;

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    Fsignal (Qquit, Qnil);
  
  return unbind_to (count, file);
}

#endif /* USE_MOTIF */



/***********************************************************************
			       Keyboard
 ***********************************************************************/

#ifdef HAVE_XKBGETKEYBOARD
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#endif

DEFUN ("x-backspace-delete-keys-p", Fx_backspace_delete_keys_p,
       Sx_backspace_delete_keys_p, 0, 1, 0,
       doc: /* Check if both Backspace and Delete keys are on the keyboard of FRAME.
FRAME nil means use the selected frame.
Value is t if we know that both keys are present, and are mapped to the
usual X keysyms.  */)
     (frame)
     Lisp_Object frame;
{
#ifdef HAVE_XKBGETKEYBOARD
  XkbDescPtr kb;
  struct frame *f = check_x_frame (frame);
  Display *dpy = FRAME_X_DISPLAY (f);
  Lisp_Object have_keys;
  int major, minor, op, event, error;

  BLOCK_INPUT;

  /* Check library version in case we're dynamically linked.  */
  major = XkbMajorVersion;
  minor = XkbMinorVersion;
  if (!XkbLibraryVersion (&major, &minor))
    {
      UNBLOCK_INPUT;
      return Qnil;
    }

  /* Check that the server supports XKB.  */
  major = XkbMajorVersion;
  minor = XkbMinorVersion;
  if (!XkbQueryExtension (dpy, &op, &event, &error, &major, &minor))
    {
      UNBLOCK_INPUT;
      return Qnil;
    }
  
  have_keys = Qnil;
  kb = XkbGetMap (dpy, XkbAllMapComponentsMask, XkbUseCoreKbd);
  if (kb)
    {
      int delete_keycode = 0, backspace_keycode = 0, i;

      if (XkbGetNames (dpy, XkbAllNamesMask, kb) == Success)
	{
	  for (i = kb->min_key_code;
	       (i < kb->max_key_code
		&& (delete_keycode == 0 || backspace_keycode == 0));
	       ++i)
	    {
	      /* The XKB symbolic key names can be seen most easily in
		 the PS file generated by `xkbprint -label name
		 $DISPLAY'.  */
	      if (bcmp ("DELE", kb->names->keys[i].name, 4) == 0)
		delete_keycode = i;
	      else if (bcmp ("BKSP", kb->names->keys[i].name, 4) == 0)
		backspace_keycode = i;
	    }

	  XkbFreeNames (kb, 0, True);
	}

      XkbFreeClientMap (kb, 0, True);
  
      if (delete_keycode
	  && backspace_keycode
	  && XKeysymToKeycode (dpy, XK_Delete) == delete_keycode
	  && XKeysymToKeycode (dpy, XK_BackSpace) == backspace_keycode)
	have_keys = Qt;
    }
  UNBLOCK_INPUT;
  return have_keys;
#else /* not HAVE_XKBGETKEYBOARD */
  return Qnil;
#endif /* not HAVE_XKBGETKEYBOARD */
}



/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_xfns ()
{
  /* This is zero if not using X windows.  */
  x_in_use = 0;

  /* The section below is built by the lisp expression at the top of the file,
     just above where these variables are declared.  */
  /*&&& init symbols here &&&*/
  Qauto_raise = intern ("auto-raise");
  staticpro (&Qauto_raise);
  Qauto_lower = intern ("auto-lower");
  staticpro (&Qauto_lower);
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
  Qgeometry = intern ("geometry");
  staticpro (&Qgeometry);
  Qicon_left = intern ("icon-left");
  staticpro (&Qicon_left);
  Qicon_top = intern ("icon-top");
  staticpro (&Qicon_top);
  Qicon_type = intern ("icon-type");
  staticpro (&Qicon_type);
  Qicon_name = intern ("icon-name");
  staticpro (&Qicon_name);
  Qinternal_border_width = intern ("internal-border-width");
  staticpro (&Qinternal_border_width);
  Qleft = intern ("left");
  staticpro (&Qleft);
  Qright = intern ("right");
  staticpro (&Qright);
  Qmouse_color = intern ("mouse-color");
  staticpro (&Qmouse_color);
  Qnone = intern ("none");
  staticpro (&Qnone);
  Qparent_id = intern ("parent-id");
  staticpro (&Qparent_id);
  Qscroll_bar_width = intern ("scroll-bar-width");
  staticpro (&Qscroll_bar_width);
  Qsuppress_icon = intern ("suppress-icon");
  staticpro (&Qsuppress_icon);
  Qundefined_color = intern ("undefined-color");
  staticpro (&Qundefined_color);
  Qvertical_scroll_bars = intern ("vertical-scroll-bars");
  staticpro (&Qvertical_scroll_bars);
  Qvisibility = intern ("visibility");
  staticpro (&Qvisibility);
  Qwindow_id = intern ("window-id");
  staticpro (&Qwindow_id);
  Qouter_window_id = intern ("outer-window-id");
  staticpro (&Qouter_window_id);
  Qx_frame_parameter = intern ("x-frame-parameter");
  staticpro (&Qx_frame_parameter);
  Qx_resource_name = intern ("x-resource-name");
  staticpro (&Qx_resource_name);
  Quser_position = intern ("user-position");
  staticpro (&Quser_position);
  Quser_size = intern ("user-size");
  staticpro (&Quser_size);
  Qscroll_bar_foreground = intern ("scroll-bar-foreground");
  staticpro (&Qscroll_bar_foreground);
  Qscroll_bar_background = intern ("scroll-bar-background");
  staticpro (&Qscroll_bar_background);
  Qscreen_gamma = intern ("screen-gamma");
  staticpro (&Qscreen_gamma);
  Qline_spacing = intern ("line-spacing");
  staticpro (&Qline_spacing);
  Qcenter = intern ("center");
  staticpro (&Qcenter);
  Qcompound_text = intern ("compound-text");
  staticpro (&Qcompound_text);
  Qcancel_timer = intern ("cancel-timer");
  staticpro (&Qcancel_timer);
  Qwait_for_wm = intern ("wait-for-wm");
  staticpro (&Qwait_for_wm);
  Qfullscreen = intern ("fullscreen");
  staticpro (&Qfullscreen);
  Qfullwidth = intern ("fullwidth");
  staticpro (&Qfullwidth);
  Qfullheight = intern ("fullheight");
  staticpro (&Qfullheight);
  Qfullboth = intern ("fullboth");
  staticpro (&Qfullboth);
  /* This is the end of symbol initialization.  */

  /* Text property `display' should be nonsticky by default.  */
  Vtext_property_default_nonsticky
    = Fcons (Fcons (Qdisplay, Qt), Vtext_property_default_nonsticky);


  Qlaplace = intern ("laplace");
  staticpro (&Qlaplace);
  Qemboss = intern ("emboss");
  staticpro (&Qemboss);
  Qedge_detection = intern ("edge-detection");
  staticpro (&Qedge_detection);
  Qheuristic = intern ("heuristic");
  staticpro (&Qheuristic);
  QCmatrix = intern (":matrix");
  staticpro (&QCmatrix);
  QCcolor_adjustment = intern (":color-adjustment");
  staticpro (&QCcolor_adjustment);
  QCmask = intern (":mask");
  staticpro (&QCmask);
 
  Qface_set_after_frame_default = intern ("face-set-after-frame-default");
  staticpro (&Qface_set_after_frame_default);

  Fput (Qundefined_color, Qerror_conditions,
	Fcons (Qundefined_color, Fcons (Qerror, Qnil)));
  Fput (Qundefined_color, Qerror_message,
	build_string ("Undefined color"));

  init_x_parm_symbols ();

  DEFVAR_BOOL ("cross-disabled-images", &cross_disabled_images,
    doc: /* Non-nil means always draw a cross over disabled images.
Disabled images are those having an `:conversion disabled' property.
A cross is always drawn on black & white displays.  */);
  cross_disabled_images = 0;

  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path,
    doc: /* List of directories to search for bitmap files for X.  */);
  Vx_bitmap_file_path = decode_env_path ((char *) 0, PATH_BITMAPS);

  DEFVAR_LISP ("x-pointer-shape", &Vx_pointer_shape,
    doc: /* The shape of the pointer when over text.
Changing the value does not affect existing frames
unless you set the mouse color.  */);
  Vx_pointer_shape = Qnil;

  DEFVAR_LISP ("x-resource-name", &Vx_resource_name,
    doc: /* The name Emacs uses to look up X resources.
`x-get-resource' uses this as the first component of the instance name
when requesting resource values.
Emacs initially sets `x-resource-name' to the name under which Emacs
was invoked, or to the value specified with the `-name' or `-rn'
switches, if present.

It may be useful to bind this variable locally around a call
to `x-get-resource'.  See also the variable `x-resource-class'.  */);
  Vx_resource_name = Qnil;

  DEFVAR_LISP ("x-resource-class", &Vx_resource_class,
    doc: /* The class Emacs uses to look up X resources.
`x-get-resource' uses this as the first component of the instance class
when requesting resource values.

Emacs initially sets `x-resource-class' to "Emacs".

Setting this variable permanently is not a reasonable thing to do,
but binding this variable locally around a call to `x-get-resource'
is a reasonable practice.  See also the variable `x-resource-name'.  */);
  Vx_resource_class = build_string (EMACS_CLASS);

#if 0 /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-nontext-pointer-shape", &Vx_nontext_pointer_shape,
    doc: /* The shape of the pointer when not over text.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
#endif
  Vx_nontext_pointer_shape = Qnil;

  DEFVAR_LISP ("x-hourglass-pointer-shape", &Vx_hourglass_pointer_shape,
    doc: /* The shape of the pointer when Emacs is busy.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_hourglass_pointer_shape = Qnil;

  DEFVAR_BOOL ("display-hourglass", &display_hourglass_p,
    doc: /* Non-zero means Emacs displays an hourglass pointer on window systems.  */);
  display_hourglass_p = 1;
  
  DEFVAR_LISP ("hourglass-delay", &Vhourglass_delay,
    doc: /* *Seconds to wait before displaying an hourglass pointer.
Value must be an integer or float.  */);
  Vhourglass_delay = make_number (DEFAULT_HOURGLASS_DELAY);

#if 0 /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-mode-pointer-shape", &Vx_mode_pointer_shape,
    doc: /* The shape of the pointer when over the mode line.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
#endif
  Vx_mode_pointer_shape = Qnil;

  DEFVAR_LISP ("x-sensitive-text-pointer-shape",
	      &Vx_sensitive_text_pointer_shape,
	       doc: /* The shape of the pointer when over mouse-sensitive text.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_sensitive_text_pointer_shape = Qnil;

  DEFVAR_LISP ("x-window-horizontal-drag-cursor",
	      &Vx_window_horizontal_drag_shape,
  doc: /* Pointer shape to use for indicating a window can be dragged horizontally.
This variable takes effect when you create a new frame
or when you set the mouse color.  */);
  Vx_window_horizontal_drag_shape = Qnil;

  DEFVAR_LISP ("x-cursor-fore-pixel", &Vx_cursor_fore_pixel,
    doc: /* A string indicating the foreground color of the cursor box.  */);
  Vx_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("x-max-tooltip-size", &Vx_max_tooltip_size,
    doc: /* Maximum size for tooltips.  Value is a pair (COLUMNS . ROWS).
Text larger than this is clipped.  */);
  Vx_max_tooltip_size = Fcons (make_number (80), make_number (40));
  
  DEFVAR_LISP ("x-no-window-manager", &Vx_no_window_manager,
    doc: /* Non-nil if no X window manager is in use.
Emacs doesn't try to figure this out; this is always nil
unless you set it to something else.  */);
  /* We don't have any way to find this out, so set it to nil
     and maybe the user would like to set it to t.  */
  Vx_no_window_manager = Qnil;

  DEFVAR_LISP ("x-pixel-size-width-font-regexp",
	       &Vx_pixel_size_width_font_regexp,
    doc: /* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.

Since Emacs gets width of a font matching with this regexp from
PIXEL_SIZE field of the name, font finding mechanism gets faster for
such a font.  This is especially effective for such large fonts as
Chinese, Japanese, and Korean.  */);
  Vx_pixel_size_width_font_regexp = Qnil;

  DEFVAR_LISP ("image-cache-eviction-delay", &Vimage_cache_eviction_delay,
    doc: /* Time after which cached images are removed from the cache.
When an image has not been displayed this many seconds, remove it
from the image cache.  Value must be an integer or nil with nil
meaning don't clear the cache.  */);
  Vimage_cache_eviction_delay = make_number (30 * 60);

#ifdef USE_X_TOOLKIT
  Fprovide (intern ("x-toolkit"), Qnil);
#ifdef USE_MOTIF
  Fprovide (intern ("motif"), Qnil);

  DEFVAR_LISP ("motif-version-string", &Vmotif_version_string,
	       doc: /* Version info for LessTif/Motif.  */);
  Vmotif_version_string = build_string (XmVERSION_STRING);
#endif /* USE_MOTIF */
#endif /* USE_X_TOOLKIT */

  defsubr (&Sx_get_resource);

  /* X window properties.  */
  defsubr (&Sx_change_window_property);
  defsubr (&Sx_delete_window_property);
  defsubr (&Sx_window_property);

  defsubr (&Sxw_display_color_p);
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sxw_color_defined_p);
  defsubr (&Sxw_color_values);
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
  defsubr (&Sx_parse_geometry);
  defsubr (&Sx_create_frame);
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_synchronize);
  defsubr (&Sx_focus_frame);
  defsubr (&Sx_backspace_delete_keys_p);
  
  /* Setting callback functions for fontset handler.  */
  get_font_info_func = x_get_font_info;

#if 0 /* This function pointer doesn't seem to be used anywhere.
	 And the pointer assigned has the wrong type, anyway.  */
  list_fonts_func = x_list_fonts;
#endif
  
  load_font_func = x_load_font;
  find_ccl_program_func = x_find_ccl_program;
  query_font_func = x_query_font;
  set_frame_fontset_func = x_set_font;
  check_window_system_func = check_x;

  /* Images.  */
  Qxbm = intern ("xbm");
  staticpro (&Qxbm);
  QCtype = intern (":type");
  staticpro (&QCtype);
  QCconversion = intern (":conversion");
  staticpro (&QCconversion);
  QCheuristic_mask = intern (":heuristic-mask");
  staticpro (&QCheuristic_mask);
  QCcolor_symbols = intern (":color-symbols");
  staticpro (&QCcolor_symbols);
  QCascent = intern (":ascent");
  staticpro (&QCascent);
  QCmargin = intern (":margin");
  staticpro (&QCmargin);
  QCrelief = intern (":relief");
  staticpro (&QCrelief);
  Qpostscript = intern ("postscript");
  staticpro (&Qpostscript);
  QCloader = intern (":loader");
  staticpro (&QCloader);
  QCbounding_box = intern (":bounding-box");
  staticpro (&QCbounding_box);
  QCpt_width = intern (":pt-width");
  staticpro (&QCpt_width);
  QCpt_height = intern (":pt-height");
  staticpro (&QCpt_height);
  QCindex = intern (":index");
  staticpro (&QCindex);
  Qpbm = intern ("pbm");
  staticpro (&Qpbm);

#if HAVE_XPM
  Qxpm = intern ("xpm");
  staticpro (&Qxpm);
#endif
  
#if HAVE_JPEG
  Qjpeg = intern ("jpeg");
  staticpro (&Qjpeg);
#endif 

#if HAVE_TIFF
  Qtiff = intern ("tiff");
  staticpro (&Qtiff);
#endif 

#if HAVE_GIF
  Qgif = intern ("gif");
  staticpro (&Qgif);
#endif

#if HAVE_PNG
  Qpng = intern ("png");
  staticpro (&Qpng);
#endif

  defsubr (&Sclear_image_cache);
  defsubr (&Simage_size);
  defsubr (&Simage_mask_p);

  hourglass_atimer = NULL;
  hourglass_shown_p = 0;

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
  tip_timer = Qnil;
  staticpro (&tip_timer);
  tip_frame = Qnil;
  staticpro (&tip_frame);

  last_show_tip_args = Qnil;
  staticpro (&last_show_tip_args);

#ifdef USE_MOTIF
  defsubr (&Sx_file_dialog);
#endif
}


void
init_xfns ()
{
  image_types = NULL;
  Vimage_types = Qnil;
  
  define_image_type (&xbm_type);
  define_image_type (&gs_type);
  define_image_type (&pbm_type);
  
#if HAVE_XPM
  define_image_type (&xpm_type);
#endif
  
#if HAVE_JPEG
  define_image_type (&jpeg_type);
#endif
  
#if HAVE_TIFF
  define_image_type (&tiff_type);
#endif
  
#if HAVE_GIF
  define_image_type (&gif_type);
#endif
  
#if HAVE_PNG
  define_image_type (&png_type);
#endif
}

#endif /* HAVE_X_WINDOWS */
