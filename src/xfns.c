/* Functions for the X window system.
   Copyright (C) 1989, 92, 93, 94, 95, 96, 1997, 1998, 1999
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

/* Image support (XBM, XPM, PBM, JPEG, TIFF, GIF, PNG, GS). tooltips,
   tool-bars, busy-cursor, file selection dialog added by Gerd
   Moellmann <gerd@gnu.org>.  */

/* Completely rewritten by Richard Stallman.  */

/* Rewritten for X11 by Joseph Arceneaux */

#include <config.h>
#include <signal.h>
#include <stdio.h>
#include <math.h>

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
#include "charset.h"
#include "fontset.h"
#include "systime.h"
#include "termhooks.h"

#ifdef HAVE_X_WINDOWS

#include <ctype.h>

/* On some systems, the character-composition stuff is broken in X11R5.  */

#if defined (HAVE_X11R5) && ! defined (HAVE_X11R6)
#ifdef X11R5_INHIBIT_I18N
#define X_I18N_INHIBITED
#endif
#endif

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

#endif /* USE_X_TOOLKIT */

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

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
unsigned char *gray_bitmap_bits = gray_bits;

/* The name we're using in resource queries.  Most often "emacs".  */

Lisp_Object Vx_resource_name;

/* The application class we're using in resource queries.
   Normally "Emacs".  */

Lisp_Object Vx_resource_class;

/* Non-zero means we're allowed to display a busy cursor.  */

int display_busy_cursor_p;

/* The background and shape of the mouse pointer, and shape when not
   over text or in the modeline.  */

Lisp_Object Vx_pointer_shape, Vx_nontext_pointer_shape, Vx_mode_pointer_shape;
Lisp_Object Vx_busy_pointer_shape;

/* The shape when over mouse-sensitive text.  */

Lisp_Object Vx_sensitive_text_pointer_shape;

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
Lisp_Object Qscreen_gamma;

/* The below are defined in frame.c.  */

extern Lisp_Object Qheight, Qminibuffer, Qname, Qonly, Qwidth;
extern Lisp_Object Qunsplittable, Qmenu_bar_lines, Qbuffer_predicate, Qtitle;
extern Lisp_Object Qtool_bar_lines;

extern Lisp_Object Vwindow_system_version;

Lisp_Object Qface_set_after_frame_default;


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
  CHECK_LIVE_FRAME (frame, 0);
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
  if (NILP (frame))
    {
      struct frame *sf = XFRAME (selected_frame);
      
      if (FRAME_X_P (sf) && FRAME_LIVE_P (sf))
	return FRAME_X_DISPLAY_INFO (sf);
      else if (x_display_list != 0)
	return x_display_list;
      else
	error ("X windows are not in use or not initialized");
    }
  else if (STRINGP (frame))
    return x_display_info_for_name (frame);
  else
    {
      FRAME_PTR f;

      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
      if (! FRAME_X_P (f))
	error ("Non-X frame used");
      return FRAME_X_DISPLAY_INFO (f);
    }
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
      if (x->widget)
	{
	  if (wdesc == XtWindow (x->widget) 
	      || wdesc == XtWindow (x->column_widget) 
	      || wdesc == XtWindow (x->edit_widget))
	    return f;
	  /* Match if the window is this frame's menubar.  */
	  if (lw_window_is_in_menubar (wdesc, x->menubar_widget))
	    return f;
	}
      else if (FRAME_X_WINDOW (f) == wdesc)
	/* A tooltip frame.  */
	return f;
    }
  return 0;
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
      if (x->widget)
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
  fd = openp (Vx_bitmap_file_path, file, "", &found, 0);
  if (fd < 0)
    return -1;
  /* XReadBitmapFile won't handle magic file names.  */
  if (fd == 0)
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

void x_set_foreground_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_background_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_mouse_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_cursor_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_border_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_cursor_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_icon_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_icon_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
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

static struct x_frame_parm_table x_frame_parms[] =
{
  "auto-raise", x_set_autoraise,
  "auto-lower", x_set_autolower,
  "background-color", x_set_background_color,
  "border-color", x_set_border_color,
  "border-width", x_set_border_width,
  "cursor-color", x_set_cursor_color,
  "cursor-type", x_set_cursor_type,
  "font", x_set_font,
  "foreground-color", x_set_foreground_color,
  "icon-name", x_set_icon_name,
  "icon-type", x_set_icon_type,
  "internal-border-width", x_set_internal_border_width,
  "menu-bar-lines", x_set_menu_bar_lines,
  "mouse-color", x_set_mouse_color,
  "name", x_explicitly_set_name,
  "scroll-bar-width", x_set_scroll_bar_width,
  "title", x_set_title,
  "unsplittable", x_set_unsplittable,
  "vertical-scroll-bars", x_set_vertical_scroll_bars,
  "visibility", x_set_visibility,
  "tool-bar-lines", x_set_tool_bar_lines,
  "scroll-bar-foreground", x_set_scroll_bar_foreground,
  "scroll-bar-background", x_set_scroll_bar_background,
  "screen-gamma", x_set_screen_gamma
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

/* Change the parameters of frame F as specified by ALIST.
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
  for (p = 0; p < i; p++) 
    {
      Lisp_Object prop, val;

      prop = parms[p];
      val = values[p];
      if (EQ (prop, Qforeground_color) || EQ (prop, Qbackground_color))
	{
	  register Lisp_Object param_index, old_value;

	  param_index = Fget (prop, Qx_frame_parameter);
	  old_value = get_frame_param (f, prop);
	  store_frame_param (f, prop, val);
 	  if (NATNUMP (param_index)
	      && (XFASTINT (param_index)
		  < sizeof (x_frame_parms)/sizeof (x_frame_parms[0])))
	    (*x_frame_parms[XINT (param_index)].setter)(f, val, old_value);
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
      else if (EQ (prop, Qforeground_color) || EQ (prop, Qbackground_color))
	/* Processed above.  */
	continue;
      else
	{
	  register Lisp_Object param_index, old_value;

	  param_index = Fget (prop, Qx_frame_parameter);
	  old_value = get_frame_param (f, prop);
	  store_frame_param (f, prop, val);
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
  int win_x, win_y;
  Window child;

  /* This is pretty gross, but seems to be the easiest way out of
     the problem that arises when restarting window-managers.  */

#ifdef USE_X_TOOLKIT
  Window outer = (f->output_data.x->widget
		  ? XtWindow (f->output_data.x->widget)
		  : FRAME_X_WINDOW (f));
#else
  Window outer = f->output_data.x->window_desc;
#endif
  Window tmp_root_window;
  Window *tmp_children;
  int tmp_nchildren;

  while (1)
    {
      int count = x_catch_errors (FRAME_X_DISPLAY (f));
      Window outer_window;

      XQueryTree (FRAME_X_DISPLAY (f), outer, &tmp_root_window,
		  &f->output_data.x->parent_desc,
		  &tmp_children, &tmp_nchildren);
      XFree ((char *) tmp_children);

      win_x = win_y = 0;

      /* Find the position of the outside upper-left corner of
	 the inner window, with respect to the outer window.  */
      if (f->output_data.x->parent_desc != FRAME_X_DISPLAY_INFO (f)->root_window)
	outer_window = f->output_data.x->parent_desc;
      else
	outer_window = outer;

      XTranslateCoordinates (FRAME_X_DISPLAY (f),

			     /* From-window, to-window.  */
			     outer_window,
			     FRAME_X_DISPLAY_INFO (f)->root_window,

			     /* From-position, to-position.  */
			     0, 0, &win_x, &win_y,

			     /* Child of win.  */
			     &child);

      /* It is possible for the window returned by the XQueryNotify
	 to become invalid by the time we call XTranslateCoordinates.
	 That can happen when you restart some window managers.
	 If so, we get an error in XTranslateCoordinates.
	 Detect that and try the whole thing over.  */
      if (! x_had_errors_p (FRAME_X_DISPLAY (f)))
	{
	  x_uncatch_errors (FRAME_X_DISPLAY (f), count);
	  break;
	}

      x_uncatch_errors (FRAME_X_DISPLAY (f), count);
    }

  *xptr = win_x;
  *yptr = win_y;
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


/* Decide if color named COLOR is valid for the display associated with
   the selected frame; if so, return the rgb values in COLOR_DEF.
   If ALLOC is nonzero, allocate a new colormap cell.  */

int
x_defined_color (f, color, color_def, alloc)
     FRAME_PTR f;
     char *color;
     XColor *color_def;
     int alloc;
{
  register int status;
  Colormap screen_colormap;
  Display *display = FRAME_X_DISPLAY (f);

  BLOCK_INPUT;
  screen_colormap = DefaultColormap (display, XDefaultScreen (display));

  status = XParseColor (display, screen_colormap, color, color_def);
  if (status && alloc) 
    {
      /* Apply gamma correction.  */
      gamma_correct (f, color_def);
      
      status = XAllocColor (display, screen_colormap, color_def);
      if (!status)
	{
	  /* If we got to this point, the colormap is full, so we're 
	     going to try and get the next closest color.
	     The algorithm used is a least-squares matching, which is
	     what X uses for closest color matching with StaticColor visuals.  */

	  XColor *cells;
	  int no_cells;
	  int nearest;
	  long nearest_delta, trial_delta;
	  int x;

	  no_cells = XDisplayCells (display, XDefaultScreen (display));
	  cells = (XColor *) alloca (sizeof (XColor) * no_cells);

	  for (x = 0; x < no_cells; x++) 
	    cells[x].pixel = x;

	  XQueryColors (display, screen_colormap, cells, no_cells);
	  nearest = 0;
	  /* I'm assuming CSE so I'm not going to condense this. */
	  nearest_delta = ((((color_def->red >> 8) - (cells[0].red >> 8))
			    * ((color_def->red >> 8) - (cells[0].red >> 8)))
			   +
			   (((color_def->green >> 8) - (cells[0].green >> 8))
			    * ((color_def->green >> 8) - (cells[0].green >> 8)))
			   +
			   (((color_def->blue >> 8) - (cells[0].blue >> 8))
			    * ((color_def->blue >> 8) - (cells[0].blue >> 8))));
	  for (x = 1; x < no_cells; x++) 
	    {
	      trial_delta = ((((color_def->red >> 8) - (cells[x].red >> 8))
			      * ((color_def->red >> 8) - (cells[x].red >> 8)))
			     +
			     (((color_def->green >> 8) - (cells[x].green >> 8))
			      * ((color_def->green >> 8) - (cells[x].green >> 8)))
			     +
			     (((color_def->blue >> 8) - (cells[x].blue >> 8))
			      * ((color_def->blue >> 8) - (cells[x].blue >> 8))));
	      if (trial_delta < nearest_delta) 
		{
		  XColor temp;
		  temp.red = cells[x].red;
		  temp.green = cells[x].green;
		  temp.blue = cells[x].blue;
		  status = XAllocColor (display, screen_colormap, &temp);
		  if (status)
		    {
		      nearest = x;
		      nearest_delta = trial_delta;
		    }
		}
	    }
	  color_def->red = cells[nearest].red;
	  color_def->green = cells[nearest].green;
	  color_def->blue = cells[nearest].blue;
	  status = XAllocColor (display, screen_colormap, color_def);
	}
    }
  UNBLOCK_INPUT;

  if (status)
    return 1;
  else
    return 0;
}

/* Given a string ARG naming a color, compute a pixel value from it
   suitable for screen F.
   If F is not a color screen, return DEF (default) regardless of what
   ARG says.  */

int
x_decode_color (f, arg, def)
     FRAME_PTR f;
     Lisp_Object arg;
     int def;
{
  XColor cdef;

  CHECK_STRING (arg, 0);

  if (strcmp (XSTRING (arg)->data, "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (XSTRING (arg)->data, "white") == 0)
    return WHITE_PIX_DEFAULT (f);

  if (FRAME_X_DISPLAY_INFO (f)->n_planes == 1)
    return def;

  /* x_defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (x_defined_color (f, XSTRING (arg)->data, &cdef, 1))
    return cdef.pixel;

  Fsignal (Qerror, Fcons (build_string ("undefined color"),
			  Fcons (arg, Qnil)));
}

/* Change the `screen-gamma' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value.  */

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
    Fsignal (Qerror, Fcons (build_string ("Illegal screen-gamma"),
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
  unsigned long pixel
    = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));

  unload_color (f, f->output_data.x->foreground_pixel);
  f->output_data.x->foreground_pixel = pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      XSetForeground (FRAME_X_DISPLAY (f), f->output_data.x->normal_gc,
		      f->output_data.x->foreground_pixel);
      XSetBackground (FRAME_X_DISPLAY (f), f->output_data.x->reverse_gc,
		      f->output_data.x->foreground_pixel);
      UNBLOCK_INPUT;
      update_face_from_frame_parameter (f, Qforeground_color, arg);
      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

void
x_set_background_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned long pixel
    = x_decode_color (f, arg, WHITE_PIX_DEFAULT (f));

  unload_color (f, f->output_data.x->background_pixel);
  f->output_data.x->background_pixel = pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      /* The main frame area.  */
      XSetBackground (FRAME_X_DISPLAY (f), f->output_data.x->normal_gc,
		      f->output_data.x->background_pixel);
      XSetForeground (FRAME_X_DISPLAY (f), f->output_data.x->reverse_gc,
		      f->output_data.x->background_pixel);
      XSetForeground (FRAME_X_DISPLAY (f), f->output_data.x->cursor_gc,
		      f->output_data.x->background_pixel);
      XSetWindowBackground (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    f->output_data.x->background_pixel);
      {
	Lisp_Object bar;
	for (bar = FRAME_SCROLL_BARS (f); !NILP (bar);
	     bar = XSCROLL_BAR (bar)->next)
	  XSetWindowBackground (FRAME_X_DISPLAY (f),
				SCROLL_BAR_X_WINDOW (XSCROLL_BAR (bar)),
				f->output_data.x->background_pixel);
      }
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
  Cursor cursor, nontext_cursor, mode_cursor, cross_cursor;
  Cursor busy_cursor;
  int count;
  unsigned long pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  unsigned long mask_color = f->output_data.x->background_pixel;

  /* Don't let pointers be invisible.  */
  if (mask_color == pixel
      && mask_color == f->output_data.x->background_pixel)
    pixel = f->output_data.x->foreground_pixel;

  unload_color (f, f->output_data.x->mouse_pixel);
  f->output_data.x->mouse_pixel = pixel;

  BLOCK_INPUT;

  /* It's not okay to crash if the user selects a screwy cursor.  */
  count = x_catch_errors (FRAME_X_DISPLAY (f));

  if (!EQ (Qnil, Vx_pointer_shape))
    {
      CHECK_NUMBER (Vx_pointer_shape, 0);
      cursor = XCreateFontCursor (FRAME_X_DISPLAY (f), XINT (Vx_pointer_shape));
    }
  else
    cursor = XCreateFontCursor (FRAME_X_DISPLAY (f), XC_xterm);
  x_check_errors (FRAME_X_DISPLAY (f), "bad text pointer cursor: %s");

  if (!EQ (Qnil, Vx_nontext_pointer_shape))
    {
      CHECK_NUMBER (Vx_nontext_pointer_shape, 0);
      nontext_cursor = XCreateFontCursor (FRAME_X_DISPLAY (f),
					  XINT (Vx_nontext_pointer_shape));
    }
  else
    nontext_cursor = XCreateFontCursor (FRAME_X_DISPLAY (f), XC_left_ptr);
  x_check_errors (FRAME_X_DISPLAY (f), "bad nontext pointer cursor: %s");

  if (!EQ (Qnil, Vx_busy_pointer_shape))
    {
      CHECK_NUMBER (Vx_busy_pointer_shape, 0);
      busy_cursor = XCreateFontCursor (FRAME_X_DISPLAY (f),
				       XINT (Vx_busy_pointer_shape));
    }
  else
    busy_cursor = XCreateFontCursor (FRAME_X_DISPLAY (f), XC_watch);
  x_check_errors (FRAME_X_DISPLAY (f), "bad busy pointer cursor: %s");
  
  x_check_errors (FRAME_X_DISPLAY (f), "bad nontext pointer cursor: %s");
  if (!EQ (Qnil, Vx_mode_pointer_shape))
    {
      CHECK_NUMBER (Vx_mode_pointer_shape, 0);
      mode_cursor = XCreateFontCursor (FRAME_X_DISPLAY (f),
				       XINT (Vx_mode_pointer_shape));
    }
  else
    mode_cursor = XCreateFontCursor (FRAME_X_DISPLAY (f), XC_xterm);
  x_check_errors (FRAME_X_DISPLAY (f), "bad modeline pointer cursor: %s");

  if (!EQ (Qnil, Vx_sensitive_text_pointer_shape))
    {
      CHECK_NUMBER (Vx_sensitive_text_pointer_shape, 0);
      cross_cursor
	= XCreateFontCursor (FRAME_X_DISPLAY (f),
			     XINT (Vx_sensitive_text_pointer_shape));
    }
  else
    cross_cursor = XCreateFontCursor (FRAME_X_DISPLAY (f), XC_crosshair);

  /* Check and report errors with the above calls.  */
  x_check_errors (FRAME_X_DISPLAY (f), "can't set cursor shape: %s");
  x_uncatch_errors (FRAME_X_DISPLAY (f), count);

  {
    XColor fore_color, back_color;

    fore_color.pixel = f->output_data.x->mouse_pixel;
    back_color.pixel = mask_color;
    XQueryColor (FRAME_X_DISPLAY (f),
		 DefaultColormap (FRAME_X_DISPLAY (f),
				  DefaultScreen (FRAME_X_DISPLAY (f))),
		 &fore_color);
    XQueryColor (FRAME_X_DISPLAY (f),
		 DefaultColormap (FRAME_X_DISPLAY (f),
				  DefaultScreen (FRAME_X_DISPLAY (f))),
		 &back_color);
    XRecolorCursor (FRAME_X_DISPLAY (f), cursor,
		    &fore_color, &back_color);
    XRecolorCursor (FRAME_X_DISPLAY (f), nontext_cursor,
		    &fore_color, &back_color);
    XRecolorCursor (FRAME_X_DISPLAY (f), mode_cursor,
		    &fore_color, &back_color);
    XRecolorCursor (FRAME_X_DISPLAY (f), cross_cursor,
                    &fore_color, &back_color);
    XRecolorCursor (FRAME_X_DISPLAY (f), busy_cursor,
                    &fore_color, &back_color);
  }

  if (FRAME_X_WINDOW (f) != 0)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), cursor);

  if (cursor != f->output_data.x->text_cursor && f->output_data.x->text_cursor != 0)
    XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->text_cursor);
  f->output_data.x->text_cursor = cursor;

  if (nontext_cursor != f->output_data.x->nontext_cursor
      && f->output_data.x->nontext_cursor != 0)
    XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->nontext_cursor);
  f->output_data.x->nontext_cursor = nontext_cursor;

  if (busy_cursor != f->output_data.x->busy_cursor
      && f->output_data.x->busy_cursor != 0)
    XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->busy_cursor);
  f->output_data.x->busy_cursor = busy_cursor;

  if (mode_cursor != f->output_data.x->modeline_cursor
      && f->output_data.x->modeline_cursor != 0)
    XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->modeline_cursor);
  f->output_data.x->modeline_cursor = mode_cursor;
  
  if (cross_cursor != f->output_data.x->cross_cursor
      && f->output_data.x->cross_cursor != 0)
    XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->cross_cursor);
  f->output_data.x->cross_cursor = cross_cursor;

  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;

  update_face_from_frame_parameter (f, Qmouse_color, arg);
}

void
x_set_cursor_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned long fore_pixel, pixel;

  if (!EQ (Vx_cursor_fore_pixel, Qnil))
    fore_pixel = x_decode_color (f, Vx_cursor_fore_pixel,
				 WHITE_PIX_DEFAULT (f));
  else
    fore_pixel = f->output_data.x->background_pixel;
  pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));

  /* Make sure that the cursor color differs from the background color.  */
  if (pixel == f->output_data.x->background_pixel)
    {
      pixel = f->output_data.x->mouse_pixel;
      if (pixel == fore_pixel)
	fore_pixel = f->output_data.x->background_pixel;
    }

  unload_color (f, f->output_data.x->cursor_foreground_pixel);
  f->output_data.x->cursor_foreground_pixel = fore_pixel;

  unload_color (f, f->output_data.x->cursor_pixel);
  f->output_data.x->cursor_pixel = pixel;

  if (FRAME_X_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      XSetBackground (FRAME_X_DISPLAY (f), f->output_data.x->cursor_gc,
		      f->output_data.x->cursor_pixel);
      XSetForeground (FRAME_X_DISPLAY (f), f->output_data.x->cursor_gc,
		      fore_pixel);
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

  CHECK_STRING (arg, 0);
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

void
x_set_cursor_type (f, arg, oldval)
     FRAME_PTR f;
     Lisp_Object arg, oldval;
{
  if (EQ (arg, Qbar))
    {
      FRAME_DESIRED_CURSOR (f) = BAR_CURSOR;
      f->output_data.x->cursor_width = 2;
    }
  else if (CONSP (arg) && EQ (XCAR (arg), Qbar)
	   && INTEGERP (XCDR (arg)))
    {
      FRAME_DESIRED_CURSOR (f) = BAR_CURSOR;
      f->output_data.x->cursor_width = XINT (XCDR (arg));
    }
  else
    /* Treat anything unknown as "box cursor".
       It was bad to signal an error; people have trouble fixing
       .Xdefaults with Emacs, when it has something bad in it.  */
    FRAME_DESIRED_CURSOR (f) = FILLED_BOX_CURSOR;

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

  CHECK_STRING (arg, 1);

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

void
x_set_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  CHECK_NUMBER (arg, 0);

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

  CHECK_NUMBER (arg, 0);
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

static void
x_set_menu_bar_lines_1 (window, n)
  Lisp_Object window;
  int n;
{
  struct window *w = XWINDOW (window);

  XSETFASTINT (w->top, XFASTINT (w->top) + n);
  XSETFASTINT (w->height, XFASTINT (w->height) - n);

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
  x_set_menu_bar_lines_1 (f->root_window, nlines - olines);
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
  int delta, nlines;

  /* Use VALUE only if an integer >= 0.  */
  if (INTEGERP (value) && XINT (value) >= 0)
    nlines = XFASTINT (value);
  else
    nlines = 0;

  /* Make sure we redisplay all windows in this frame.  */
  ++windows_or_buffers_changed;

  delta = nlines - FRAME_TOOL_BAR_LINES (f);
  FRAME_TOOL_BAR_LINES (f) = nlines;
  x_set_menu_bar_lines_1 (FRAME_ROOT_WINDOW (f), delta);
  adjust_glyphs (f);
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
    CHECK_STRING (name, 0);

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
	Lisp_Object icon_name;

	text.value = XSTRING (name)->data;
	text.encoding = XA_STRING;
	text.format = 8;
	text.nitems = STRING_BYTES (XSTRING (name));

	icon_name = (!NILP (f->icon_name) ? f->icon_name : name);

	icon.value = XSTRING (icon_name)->data;
	icon.encoding = XA_STRING;
	icon.format = 8;
	icon.nitems = STRING_BYTES (XSTRING (icon_name));
#ifdef USE_X_TOOLKIT
	XSetWMName (FRAME_X_DISPLAY (f),
		    XtWindow (f->output_data.x->widget), &text);
	XSetWMIconName (FRAME_X_DISPLAY (f), XtWindow (f->output_data.x->widget),
			&icon);
#else /* not USE_X_TOOLKIT */
	XSetWMName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &text);
	XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &icon);
#endif /* not USE_X_TOOLKIT */
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
    CHECK_STRING (name, 0);

  if (FRAME_X_WINDOW (f))
    {
      BLOCK_INPUT;
#ifdef HAVE_X11R4
      {
	XTextProperty text, icon;
	Lisp_Object icon_name;

	text.value = XSTRING (name)->data;
	text.encoding = XA_STRING;
	text.format = 8;
	text.nitems = STRING_BYTES (XSTRING (name));

	icon_name = (!NILP (f->icon_name) ? f->icon_name : name);

	icon.value = XSTRING (icon_name)->data;
	icon.encoding = XA_STRING;
	icon.format = 8;
	icon.nitems = STRING_BYTES (XSTRING (icon_name));
#ifdef USE_X_TOOLKIT
	XSetWMName (FRAME_X_DISPLAY (f),
		    XtWindow (f->output_data.x->widget), &text);
	XSetWMIconName (FRAME_X_DISPLAY (f), XtWindow (f->output_data.x->widget),
			&icon);
#else /* not USE_X_TOOLKIT */
	XSetWMName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &text);
	XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), &icon);
#endif /* not USE_X_TOOLKIT */
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
  "Return the value of ATTRIBUTE, of class CLASS, from the X defaults database.\n\
This uses `INSTANCE.ATTRIBUTE' as the key and `Emacs.CLASS' as the\n\
class, where INSTANCE is the name under which Emacs was invoked, or\n\
the name specified by the `-name' or `-rn' command-line arguments.\n\
\n\
The optional arguments COMPONENT and SUBCLASS add to the key and the\n\
class, respectively.  You must specify both of them or neither.\n\
If you specify them, the key is `INSTANCE.COMPONENT.ATTRIBUTE'\n\
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
				    build_string (""),
				    build_string ("verticalScrollBar"),
				    build_string (""));
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
       "Parse an X-style geometry string STRING.\n\
Returns an alist of the form ((top . TOP), (left . LEFT) ... ).\n\
The properties returned may include `top', `left', `height', and `width'.\n\
The value of `left' or `top' may be an integer,\n\
or a list (+ N) meaning N pixels relative to top/left corner,\n\
or a list (- N) meaning -N pixels relative to bottom/right corner.")
     (string)
     Lisp_Object string;
{
  int geometry, x, y;
  unsigned int width, height;
  Lisp_Object result;

  CHECK_STRING (string, 0);

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
	  CHECK_NUMBER (tem0, 0);
	  f->height = XINT (tem0);
	}
      if (!EQ (tem1, Qunbound))
	{
	  CHECK_NUMBER (tem1, 0);
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
  f->output_data.x->flags_areas_extra
    = FRAME_FLAGS_AREA_WIDTH (f);
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
	  CHECK_NUMBER (tem0, 0);
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
	  CHECK_NUMBER (tem1, 0);
	  f->output_data.x->left_pos = XINT (tem1);
	  if (f->output_data.x->left_pos < 0)
	    window_prompting |= XNegative;
	}

      if (!NILP (tem2) && ! EQ (tem2, Qunbound))
	window_prompting |= USPosition;
      else
	window_prompting |= PPosition;
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
				  (lw_callback) NULL);

  f->output_data.x->column_widget = pane_widget;

  /* mappedWhenManaged to false tells to the paned window to not map/unmap 
     the emacs screen when changing menubar.  This reduces flickering.  */

  ac = 0;
  XtSetArg (al[ac], XtNmappedWhenManaged, 0); ac++;
  XtSetArg (al[ac], XtNshowGrip, 0); ac++;
  XtSetArg (al[ac], XtNallowResize, 1); ac++;
  XtSetArg (al[ac], XtNresizeToPreferred, 1); ac++;
  XtSetArg (al[ac], XtNemacsFrame, f); ac++;
  frame_widget = XtCreateWidget (f->namebuf,
				  emacsFrameClass,
				  pane_widget, al, ac);
 
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
#ifndef X_I18N_INHIBITED
  { 
    XIM xim;
    XIC xic = NULL;

    xim = XOpenIM (FRAME_X_DISPLAY (f), NULL, NULL, NULL);

    if (xim)
      {
	xic = XCreateIC (xim,  
			 XNInputStyle,   XIMPreeditNothing | XIMStatusNothing,
			 XNClientWindow, FRAME_X_WINDOW(f),
			 XNFocusWindow,  FRAME_X_WINDOW(f),
			 NULL);

	if (xic == 0)
	  {
	    XCloseIM (xim);
	    xim = NULL;
	  }
      }
    FRAME_XIM (f) = xim;
    FRAME_XIC (f) = xic;
  }
#else /* X_I18N_INHIBITED */
  FRAME_XIM (f) = 0;
  FRAME_XIC (f) = 0;
#endif /* X_I18N_INHIBITED */
#endif /* HAVE_X_I18N */

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
  attribute_mask = (CWBackPixel | CWBorderPixel | CWBitGravity
#if 0
		    | CWBackingStore | CWSaveUnder
#endif
		    | CWEventMask);

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
		     FRAME_X_DISPLAY_INFO (f)->visual,
		     attribute_mask, &attributes);
#ifdef HAVE_X_I18N
#ifndef X_I18N_INHIBITED
  { 
    XIM xim;
    XIC xic = NULL;

    xim = XOpenIM (FRAME_X_DISPLAY(f), NULL, NULL, NULL);

    if (xim)
      {
	xic = XCreateIC (xim,  
			 XNInputStyle,   XIMPreeditNothing | XIMStatusNothing,
			 XNClientWindow, FRAME_X_WINDOW(f),
			 XNFocusWindow,  FRAME_X_WINDOW(f),
			 NULL);

	if (!xic)
	  {
	    XCloseIM (xim);
	    xim = NULL;
	  }
      }

    FRAME_XIM (f) = xim;
    FRAME_XIC (f) = xic;
  }
#else /* X_I18N_INHIBITED */
  FRAME_XIM (f) = 0;
  FRAME_XIC (f) = 0;
#endif /* X_I18N_INHIBITED */
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
    (f, (EQ (x_get_arg (dpyinfo, parms, Qvisibility, 0, 0, RES_TYPE_SYMBOL),
	     Qicon)
	 ? IconicState
	 : NormalState));

  x_text_icon (f, (char *) XSTRING ((!NILP (f->icon_name)
				     ? f->icon_name
				     : f->name))->data);

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

  BLOCK_INPUT;

  /* Create the GC's of this frame.
     Note that many default values are used.  */

  /* Normal video */
  gc_values.font = f->output_data.x->font->fid;
  gc_values.foreground = f->output_data.x->foreground_pixel;
  gc_values.background = f->output_data.x->background_pixel;
  gc_values.line_width = 0;	/* Means 1 using fast algorithm.  */
  f->output_data.x->normal_gc = XCreateGC (FRAME_X_DISPLAY (f),
				       FRAME_X_WINDOW (f),
				       GCLineWidth | GCFont
				       | GCForeground | GCBackground,
				       &gc_values);

  /* Reverse video style.  */
  gc_values.foreground = f->output_data.x->background_pixel;
  gc_values.background = f->output_data.x->foreground_pixel;
  f->output_data.x->reverse_gc = XCreateGC (FRAME_X_DISPLAY (f),
					FRAME_X_WINDOW (f),
					GCFont | GCForeground | GCBackground
					| GCLineWidth,
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
	DefaultDepth (FRAME_X_DISPLAY (f),
		      XScreenNumberOfScreen (FRAME_X_SCREEN (f)))));

  UNBLOCK_INPUT;
}

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
  "Make a new X window, which is called a \"frame\" in Emacs terms.\n\
Returns an Emacs frame object.\n\
ALIST is an alist of frame parameters.\n\
If the parameters specify that the frame should not have a minibuffer,\n\
and do not specify a specific minibuffer window to use,\n\
then `default-minibuffer-frame' must be a frame whose minibuffer can\n\
be shared by the new frame.\n\
\n\
This function is an internal primitive--use `make-frame' instead.")
  (parms)
     Lisp_Object parms;
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  int minibuffer_only = 0;
  long window_prompting = 0;
  int width, height;
  int count = specpdl_ptr - specpdl;
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
    CHECK_NUMBER (parent, 0);

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

  f->icon_name
    = x_get_arg (dpyinfo, parms, Qicon_name, "iconName", "Title",
		 RES_TYPE_STRING);
  if (! STRINGP (f->icon_name))
    f->icon_name = Qnil;

  FRAME_X_DISPLAY_INFO (f) = dpyinfo;
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif

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

  /* Create fontsets from `global_fontset_alist' before handling fonts.  */
  for (tem = Vglobal_fontset_alist; CONSP (tem); tem = XCDR (tem))
    fs_register_fontset (f, XCAR (tem));

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
  x_default_parameter (f, parms, Qtool_bar_lines, make_number (0),
		       "toolBar", "ToolBar", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qbuffer_predicate, Qnil,
		       "bufferPredicate", "BufferPredicate",
		       RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtitle, Qnil,
		       "title", "Title", RES_TYPE_STRING);

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

  tem = x_get_arg (dpyinfo, parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  /* Create the X widget or window.  Add the tool-bar height to the
     initial frame height so that the user gets a text display area of
     the size he specified with -g or via .Xdefaults.  Later changes
     of the tool-bar height don't change the frame size.  This is done
     so that users can create tall Emacs frames without having to
     guess how tall the tool-bar will get.  */
  f->height += FRAME_TOOL_BAR_LINES (f);

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

  /* Set up faces after all frame parameters are known.  */
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


DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
  "Internal function called by `color-defined-p', which see.")
  (color, frame)
     Lisp_Object color, frame;
{
  XColor foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color, 1);

  if (x_defined_color (f, XSTRING (color)->data, &foo, 0))
    return Qt;
  else
    return Qnil;
}

DEFUN ("xw-color-values", Fxw_color_values, Sxw_color_values, 1, 2, 0,
  "Internal function called by `color-values', which see.")
  (color, frame)
     Lisp_Object color, frame;
{
  XColor foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color, 1);

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
  "Internal function called by `display-color-p', which see.")
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
  "Return t if the X display supports shades of gray.\n\
Note that color displays do support shades of gray.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
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
  "Returns the width in pixels of the X display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->width);
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
  Sx_display_pixel_height, 0, 1, 0,
  "Returns the height in pixels of the X display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->height);
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
  0, 1, 0,
  "Returns the number of bitplanes of the X display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->n_planes);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
  0, 1, 0,
  "Returns the number of color cells of the X display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
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
  "Returns the maximum request size of the X server of display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (MAXREQUEST (dpyinfo->display));
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
  "Returns the vendor ID string of the X server of display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);
  char *vendor = ServerVendor (dpyinfo->display);

  if (! vendor) vendor = "";
  return build_string (vendor);
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
  "Returns the version numbers of the X server of display DISPLAY.\n\
The value is a list of three integers: the major and minor\n\
version numbers of the X Protocol in use, and the vendor-specific release\n\
number.  See also the function `x-server-vendor'.\n\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
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
  "Returns the number of screens on the X server of display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (ScreenCount (dpyinfo->display));
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
  "Returns the height in millimeters of the X display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (HeightMMOfScreen (dpyinfo->screen));
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
  "Returns the width in millimeters of the X display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  return make_number (WidthMMOfScreen (dpyinfo->screen));
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
  Sx_display_backing_store, 0, 1, 0,
  "Returns an indication of whether X display DISPLAY does backing store.\n\
The value may be `always', `when-mapped', or `not-useful'.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  switch (DoesBackingStore (dpyinfo->screen))
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
  "Returns the visual class of the X display DISPLAY.\n\
The value is one of the symbols `static-gray', `gray-scale',\n\
`static-color', `pseudo-color', `true-color', or `direct-color'.\n\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
	(display)
     Lisp_Object display;
{
  struct x_display_info *dpyinfo = check_x_display_info (display);

  switch (dpyinfo->visual->class)
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
  "Returns t if the X display DISPLAY supports the save-under feature.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
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

#if 0  /* These no longer seem like the right way to do things.  */

/* Draw a rectangle on the frame with left top corner including
   the character specified by LEFT_CHAR and TOP_CHAR.  The rectangle is
   CHARS by LINES wide and long and is the color of the cursor.  */

void
x_rectangle (f, gc, left_char, top_char, chars, lines)
     register struct frame *f;
     GC gc;
     register int top_char, left_char, chars, lines;
{
  int width;
  int height;
  int left = (left_char * FONT_WIDTH (f->output_data.x->font)
		    + f->output_data.x->internal_border_width);
  int top = (top_char * f->output_data.x->line_height
		   + f->output_data.x->internal_border_width);

  if (chars < 0)
    width = FONT_WIDTH (f->output_data.x->font) / 2;
  else
    width = FONT_WIDTH (f->output_data.x->font) * chars;
  if (lines < 0)
    height = f->output_data.x->line_height / 2;
  else
    height = f->output_data.x->line_height * lines;

  XDrawRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
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
  x_rectangle (XFRAME (frame), XFRAME (frame)->output_data.x->cursor_gc,
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
  x_rectangle (XFRAME (frame), XFRAME (frame)->output_data.x->reverse_gc,
	       left, top, n_chars, n_lines);
  UNBLOCK_INPUT;

  return Qt;
}

/* Draw lines around the text region beginning at the character position
   TOP_X, TOP_Y and ending at BOTTOM_X and BOTTOM_Y.  GC specifies the
   pixel and line characteristics.  */

#define line_len(line) (FRAME_CURRENT_GLYPHS (f)->used[(line)])

static void
outline_region (f, gc, top_x, top_y, bottom_x, bottom_y)
     register struct frame *f;
     GC gc;
     int  top_x, top_y, bottom_x, bottom_y;
{
  register int ibw = f->output_data.x->internal_border_width;
  register int font_w = FONT_WIDTH (f->output_data.x->font);
  register int font_h = f->output_data.x->line_height;
  int y = top_y;
  int x = line_len (y);
  XPoint *pixel_points
    = (XPoint *) alloca (((bottom_y - top_y + 2) * 4) * sizeof (XPoint));
  register XPoint *this_point = pixel_points;

  /* Do the horizontal top line/lines */
  if (top_x == 0)
    {
      this_point->x = ibw;
      this_point->y = ibw + (font_h * top_y);
      this_point++;
      if (x == 0)
	this_point->x = ibw + (font_w / 2); /* Half-size for newline chars.  */
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

  /* Now do the right side.  */
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

  /* Now do the bottom and connect to the top left point.  */
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

  XDrawLines (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
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
  struct window *w = XWINDOW (FRAME_SELECTED_WINDOW (f));
  register int p1, p2;

  CHECK_CONS (event, 0);

  BLOCK_INPUT;
  x0 = XINT (Fcar (Fcar (event)));
  y0 = XINT (Fcar (Fcdr (Fcar (event))));

  /* If the mouse is past the end of the line, don't that area.  */
  /* ReWrite this...  */

  /* Where the cursor is.  */
  x1 = WINDOW_TO_FRAME_PIXEL_X (w, w->cursor.x);
  y1 = WINDOW_TO_FRAME_PIXEL_Y (w, w->cursor.y);

  if (y1 > y0)			/* point below mouse */
    outline_region (f, f->output_data.x->cursor_gc,
		    x0, y0, x1, y1);
  else if (y1 < y0)		/* point above mouse */
    outline_region (f, f->output_data.x->cursor_gc,
		    x1, y1, x0, y0);
  else				/* same line: draw horizontal rectangle */
    {
      if (x1 > x0)
	x_rectangle (f, f->output_data.x->cursor_gc,
		     x0, y0, (x1 - x0 + 1), 1);
      else if (x1 < x0)
	  x_rectangle (f, f->output_data.x->cursor_gc,
		       x1, y1, (x0 - x1 + 1), 1);
    }

  XFlush (FRAME_X_DISPLAY (f));
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
  struct window *w = XWINDOW (FRAME_SELECTED_WINDOW (f));

  BLOCK_INPUT;
  x0 = XINT (Fcar (Fcar (event)));
  y0 = XINT (Fcar (Fcdr (Fcar (event))));
  x1 = WINDOW_TO_FRAME_PIXEL_X (w, w->cursor.x);
  y1 = WINDOW_TO_FRAME_PIXEL_Y (w, w->cursor.y);

  if (y1 > y0)			/* point below mouse */
    outline_region (f, f->output_data.x->reverse_gc,
		      x0, y0, x1, y1);
  else if (y1 < y0)		/* point above mouse */
    outline_region (f, f->output_data.x->reverse_gc,
		      x1, y1, x0, y0);
  else				/* same line: draw horizontal rectangle */
    {
      if (x1 > x0)
	x_rectangle (f, f->output_data.x->reverse_gc,
		     x0, y0, (x1 - x0 + 1), 1);
      else if (x1 < x0)
	x_rectangle (f, f->output_data.x->reverse_gc,
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
   down a line at that character.  */

static void
clip_contour_top (y_pos, x_pos)
{
  register XPoint *begin = contour_lines[y_pos].top_left;
  register XPoint *end;
  register int npoints;
  register struct display_line *line = selected_frame->phys_lines[y_pos + 1];

  if (x_pos >= line->len - 1)	/* Draw one, straight horizontal line.  */
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

      /* Now, update contour_lines structure.  */
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

      /* Now, update contour_lines structure.  */
    }
}

/* Erase the top horizontal lines of the contour, and then extend
   the contour upwards.  */

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
 struct window *w = XWINDOW (FRAME_SELECTED_WINDOW (f));
 register int point_x = WINDOW_TO_FRAME_PIXEL_X (w, w->cursor.x);
 register int point_y = WINDOW_TO_FRAME_PIXEL_Y (w, w->cursor.y);
 register int mouse_below_point;
 register Lisp_Object obj;
 register int x_contour_x, x_contour_y;

 x_contour_x = x_mouse_x;
 x_contour_y = x_mouse_y;
 if (x_contour_y > point_y || (x_contour_y == point_y
			       && x_contour_x > point_x))
   {
     mouse_below_point = 1;
     outline_region (f, f->output_data.x->cursor_gc, point_x, point_y,
		     x_contour_x, x_contour_y);
   }
 else
   {
     mouse_below_point = 0;
     outline_region (f, f->output_data.x->cursor_gc, x_contour_x, x_contour_y,
		     point_x, point_y);
   }

 while (1)
   {
     obj = read_char (-1, 0, 0, Qnil, 0);
     if (!CONSP (obj))
       break;

     if (mouse_below_point)
       {
	 if (x_mouse_y <= point_y)                /* Flipped.  */
	   {
	     mouse_below_point = 0;

	     outline_region (f, f->output_data.x->reverse_gc, point_x, point_y,
			     x_contour_x, x_contour_y);
	     outline_region (f, f->output_data.x->cursor_gc, x_mouse_x, x_mouse_y,
			     point_x, point_y);
	   }
	 else if (x_mouse_y < x_contour_y)	  /* Bottom clipped.  */
	   {
	     clip_contour_bottom (x_mouse_y);
	   }
	 else if (x_mouse_y > x_contour_y)	  /* Bottom extended.  */
	   {
	     extend_bottom_contour (x_mouse_y);
	   }

	 x_contour_x = x_mouse_x;
	 x_contour_y = x_mouse_y;
       }
     else  /* mouse above or same line as point */
       {
	 if (x_mouse_y >= point_y)		  /* Flipped.  */
	   {
	     mouse_below_point = 1;

	     outline_region (f, f->output_data.x->reverse_gc,
			     x_contour_x, x_contour_y, point_x, point_y);
	     outline_region (f, f->output_data.x->cursor_gc, point_x, point_y,
			     x_mouse_x, x_mouse_y);
	   }
	 else if (x_mouse_y > x_contour_y)	  /* Top clipped.  */
	   {
	     clip_contour_top (x_mouse_y);
	   }
	 else if (x_mouse_y < x_contour_y)	  /* Top extended.  */
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
  register GC line_gc = f->output_data.x->cursor_gc;
  register GC erase_gc = f->output_data.x->reverse_gc;
#if 0
  char dash_list[] = {6, 4, 6, 4};
  int dashes = 4;
  XGCValues gc_values;
#endif
  register int previous_y;
  register int line = (x_mouse_y + 1) * f->output_data.x->line_height
    + f->output_data.x->internal_border_width;
  register int left = f->output_data.x->internal_border_width
    + (WINDOW_LEFT_MARGIN (w)
       * FONT_WIDTH (f->output_data.x->font));
  register int right = left + (w->width
			       * FONT_WIDTH (f->output_data.x->font))
    - f->output_data.x->internal_border_width;

#if 0
  BLOCK_INPUT;
  gc_values.foreground = f->output_data.x->cursor_pixel;
  gc_values.background = f->output_data.x->background_pixel;
  gc_values.line_width = 1;
  gc_values.line_style = LineOnOffDash;
  gc_values.cap_style = CapRound;
  gc_values.join_style = JoinRound;

  line_gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		       GCLineStyle | GCJoinStyle | GCCapStyle
		       | GCLineWidth | GCForeground | GCBackground,
		       &gc_values);
  XSetDashes (FRAME_X_DISPLAY (f), line_gc, 0, dash_list, dashes);
  gc_values.foreground = f->output_data.x->background_pixel;
  gc_values.background = f->output_data.x->foreground_pixel;
  erase_gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		       GCLineStyle | GCJoinStyle | GCCapStyle
		       | GCLineWidth | GCForeground | GCBackground,
		       &gc_values);
  XSetDashes (FRAME_X_DISPLAY (f), erase_gc, 0, dash_list, dashes);
  UNBLOCK_INPUT;
#endif

  while (1)
    {
      BLOCK_INPUT;
      if (x_mouse_y >= XINT (w->top)
	  && x_mouse_y < XINT (w->top) + XINT (w->height) - 1)
	{
	  previous_y = x_mouse_y;
	  line = (x_mouse_y + 1) * f->output_data.x->line_height
	    + f->output_data.x->internal_border_width;
	  XDrawLine (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     line_gc, left, line, right, line);
	}
      XFlush (FRAME_X_DISPLAY (f));
      UNBLOCK_INPUT;

      do
	{
	  obj = read_char (-1, 0, 0, Qnil, 0);
	  if (!CONSP (obj)
	      || (! EQ (Fcar (Fcdr (Fcdr (obj))),
			Qvertical_scroll_bar))
	      || x_mouse_grabbed)
	    {
	      BLOCK_INPUT;
	      XDrawLine (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			 erase_gc, left, line, right, line);
	      unread_command_event = obj;
#if 0
	      XFreeGC (FRAME_X_DISPLAY (f), line_gc);
	      XFreeGC (FRAME_X_DISPLAY (f), erase_gc);
#endif 
	      UNBLOCK_INPUT;
	      return Qnil;
	    }
	}
      while (x_mouse_y == previous_y);

      BLOCK_INPUT;
      XDrawLine (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		 erase_gc, left, line, right, line);
      UNBLOCK_INPUT;
    }
}
#endif

#if 0
/* These keep track of the rectangle following the pointer.  */
int mouse_track_top, mouse_track_left, mouse_track_width;

/* Offset in buffer of character under the pointer, or 0.  */
int mouse_buffer_offset;

DEFUN ("x-track-pointer", Fx_track_pointer, Sx_track_pointer, 0, 0, 0,
  "Track the pointer.")
  ()
{
  static Cursor current_pointer_shape;
  FRAME_PTR f = x_mouse_frame;

  BLOCK_INPUT;
  if (EQ (Vmouse_frame_part, Qtext_part)
      && (current_pointer_shape != f->output_data.x->nontext_cursor))
    {
      unsigned char c;
      struct buffer *buf;

      current_pointer_shape = f->output_data.x->nontext_cursor;
      XDefineCursor (FRAME_X_DISPLAY (f),
		     FRAME_X_WINDOW (f),
		     current_pointer_shape);

      buf = XBUFFER (XWINDOW (Vmouse_window)->buffer);
      c = *(BUF_CHAR_ADDRESS (buf, mouse_buffer_offset));
    }
  else if (EQ (Vmouse_frame_part, Qmodeline_part)
	   && (current_pointer_shape != f->output_data.x->modeline_cursor))
    {
      current_pointer_shape = f->output_data.x->modeline_cursor;
      XDefineCursor (FRAME_X_DISPLAY (f),
		     FRAME_X_WINDOW (f),
		     current_pointer_shape);
    }

  XFlush (FRAME_X_DISPLAY (f));
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

	  /* Erase previous rectangle.  */
	  if (mouse_track_width)
	    {
	      x_rectangle (f, f->output_data.x->reverse_gc,
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

	  if (mouse_track_left > len) /* Past the end of line.  */
	    goto draw_or_not;

	  if (mouse_track_top == mode_line_vpos)
	    {
	      in_mode_line = 1;
	      goto draw_or_not;
	    }

	  if (tab_width <= 0 || tab_width > 20) tab_width = 8;
	  do
	    {
	      c = FETCH_BYTE (p);
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
	  if (mouse_track_width) /* Over text; use text pointer shape.  */
	    {
	      XDefineCursor (FRAME_X_DISPLAY (f),
			     FRAME_X_WINDOW (f),
			     f->output_data.x->text_cursor);
	      x_rectangle (f, f->output_data.x->cursor_gc,
			   mouse_track_left, mouse_track_top,
			   mouse_track_width, 1);
	    }
	  else if (in_mode_line)
	    XDefineCursor (FRAME_X_DISPLAY (f),
			   FRAME_X_WINDOW (f),
			   f->output_data.x->modeline_cursor);
	  else
	    XDefineCursor (FRAME_X_DISPLAY (f),
			   FRAME_X_WINDOW (f),
			   f->output_data.x->nontext_cursor);
	}

      XFlush (FRAME_X_DISPLAY (f));
      UNBLOCK_INPUT;

      obj = read_char (-1, 0, 0, Qnil, 0);
      BLOCK_INPUT;
    }
  while (CONSP (obj)		   /* Mouse event */
	 && EQ (Fcar (Fcdr (Fcdr (obj))), Qnil)	   /* Not scroll bar */
	 && EQ (Vmouse_depressed, Qnil)              /* Only motion events */
	 && EQ (Vmouse_window, selected_window)	   /* In this window */
	 && x_mouse_frame);

  unread_command_event = obj;

  if (mouse_track_width)
    {
      x_rectangle (f, f->output_data.x->reverse_gc,
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
  XDefineCursor (FRAME_X_DISPLAY (f),
		 FRAME_X_WINDOW (f),
		 f->output_data.x->nontext_cursor);
  XFlush (FRAME_X_DISPLAY (f));
  UNBLOCK_INPUT;

  return Qnil;
}
#endif

#if 0
#include "glyphs.h"

/* Draw a pixmap specified by IMAGE_DATA of dimensions WIDTH and HEIGHT
   on the frame F at position X, Y.  */

x_draw_pixmap (f, x, y, image_data, width, height)
     struct frame *f;
     int x, y, width, height;
     char *image_data;
{
  Pixmap image;

  image = XCreateBitmapFromData (FRAME_X_DISPLAY (f),
				 FRAME_X_WINDOW (f), image_data,
				 width, height);
  XCopyPlane (FRAME_X_DISPLAY (f), image, FRAME_X_WINDOW (f),
	      f->output_data.x->normal_gc, 0, 0, width, height, x, y);
}
#endif

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
		   XSTRING (newstring)->data,
		   STRING_BYTES (XSTRING (newstring)));
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
		     XSTRING (newstring)->data,
		     STRING_BYTES (XSTRING (newstring)));
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
	  strsize = STRING_BYTES (XSTRING (item));
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
select_visual (dpy, screen, depth)
     Display *dpy;
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

  vinfo = XGetVisualInfo (dpy,
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

/* Return the X display structure for the display named NAME.
   Open a new connection if necessary.  */

struct x_display_info *
x_display_info_for_name (name)
     Lisp_Object name;
{
  Lisp_Object names;
  struct x_display_info *dpyinfo;

  CHECK_STRING (name, 0);

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

  dpyinfo = x_term_init (name, (unsigned char *)0,
			 (char *) XSTRING (Vx_resource_name)->data);

  if (dpyinfo == 0)
    error ("Cannot connect to X server %s", XSTRING (name)->data);

  x_in_use = 1;
  XSETFASTINT (Vwindow_system_version, 11);

  return dpyinfo;
}

DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0, "Open a connection to an X server.\n\
DISPLAY is the name of the display to connect to.\n\
Optional second arg XRM-STRING is a string of resources in xrdb format.\n\
If the optional third arg MUST-SUCCEED is non-nil,\n\
terminate Emacs if we can't open the connection.")
  (display, xrm_string, must_succeed)
     Lisp_Object display, xrm_string, must_succeed;
{
  unsigned char *xrm_option;
  struct x_display_info *dpyinfo;

  CHECK_STRING (display, 0);
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string, 1);

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
   "Close the connection to DISPLAY's X server.\n\
For DISPLAY, specify either a frame or a display name (a string).\n\
If DISPLAY is nil, that stands for the selected frame's display.")
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
	xfree (dpyinfo->font_table[i].name);
	/* Don't free the full_name string;
	   it is always shared with something else.  */
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
  "Return the list of display names that Emacs has connections to.")
  ()
{
  Lisp_Object tail, result;

  result = Qnil;
  for (tail = x_display_name_list; ! NILP (tail); tail = XCDR (tail))
    result = Fcons (XCAR (XCAR (tail)), result);

  return result;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize, 1, 2, 0,
   "If ON is non-nil, report X errors as soon as the erring request is made.\n\
If ON is nil, allow buffering of requests.\n\
Turning on synchronization prohibits the Xlib routines from buffering\n\
requests and seriously degrades performance, but makes debugging much\n\
easier.\n\
The optional second argument DISPLAY specifies which display to act on.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If DISPLAY is omitted or nil, that stands for the selected frame's display.")
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

/* A list of symbols, one for each supported image type.  */

Lisp_Object Vimage_types;

/* The symbol `image' which is the car of the lists used to represent
   images in Lisp.  */

extern Lisp_Object Qimage;

/* The symbol `xbm' which is used as the type symbol for XBM images.  */

Lisp_Object Qxbm;

/* Keywords.  */

Lisp_Object QCtype, QCdata, QCascent, QCmargin, QCrelief;
extern Lisp_Object QCwidth, QCheight, QCforeground, QCbackground, QCfile;
Lisp_Object QCalgorithm, QCcolor_symbols, QCheuristic_mask;
Lisp_Object QCindex;

/* Other symbols.  */

Lisp_Object Qlaplace;

/* Time in seconds after which images should be removed from the cache
   if not displayed.  */

Lisp_Object Vimage_cache_eviction_delay;

/* Function prototypes.  */

static void define_image_type P_ ((struct image_type *type));
static struct image_type *lookup_image_type P_ ((Lisp_Object symbol));
static void image_error P_ ((char *format, Lisp_Object, Lisp_Object));
static void x_laplace P_ ((struct frame *, struct image *));
static int x_build_heuristic_mask P_ ((struct frame *, Lisp_Object,
				       struct image *, Lisp_Object));


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
      Lisp_Object symbol = Fplist_get (XCDR (object), QCtype);
      struct image_type *type = lookup_image_type (symbol);
      
      if (type)
	valid_p = type->valid_p (object);
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
  IMAGE_SYMBOL_VALUE,
  IMAGE_POSITIVE_INTEGER_VALUE,
  IMAGE_NON_NEGATIVE_INTEGER_VALUE,
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

	case IMAGE_SYMBOL_VALUE:
	  if (!SYMBOLP (value))
	    return 0;
	  break;

	case IMAGE_POSITIVE_INTEGER_VALUE:
	  if (!INTEGERP (value) || XINT (value) <= 0)
	    return 0;
	  break;

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
  if (img->pixmap == 0 && !img->load_failed_p)
    img->load_failed_p = img->type->load (f, img) == 0;
}
     


/***********************************************************************
		  Helper functions for X image types
 ***********************************************************************/

static void x_clear_image P_ ((struct frame *f, struct image *img));
static unsigned long x_alloc_image_color P_ ((struct frame *f,
					      struct image *img,
					      Lisp_Object color_name,
					      unsigned long dflt));

/* Free X resources of image IMG which is used on frame F.  */

static void
x_clear_image (f, img)
     struct frame *f;
     struct image *img;
{
  if (img->pixmap)
    {
      BLOCK_INPUT;
      XFreePixmap (FRAME_X_DISPLAY (f), img->pixmap);
      img->pixmap = 0;
      UNBLOCK_INPUT;
    }
      
  if (img->ncolors)
    {
      int class = FRAME_X_DISPLAY_INFO (f)->visual->class;
      
      /* If display has an immutable color map, freeing colors is not
	 necessary and some servers don't allow it.  So don't do it.  */
      if (class != StaticColor
	  && class != StaticGray
	  && class != TrueColor)
	{
	  Colormap cmap;
	  BLOCK_INPUT;
	  cmap = DefaultColormapOfScreen (FRAME_X_DISPLAY_INFO (f)->screen);
	  XFreeColors (FRAME_X_DISPLAY (f), cmap, img->colors,
		       img->ncolors, 0);
	  UNBLOCK_INPUT;
	}
      
      xfree (img->colors);
      img->colors = NULL;
      img->ncolors = 0;
    }
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
      xfree (c);
      xfree (c->buckets);
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
      int i, any_freed_p = 0;

      EMACS_GET_TIME (t);
      old = EMACS_SECS (t) - XFASTINT (Vimage_cache_eviction_delay);
      
      for (i = 0; i < c->used; ++i)
	{
	  struct image *img = c->images[i];
	  if (img != NULL
	      && (force_p
		  || (img->timestamp > old)))
	    {
	      free_image (f, img);
	      any_freed_p = 1;
	    }
	}

      /* We may be clearing the image cache because, for example,
	 Emacs was iconified for a longer period of time.  In that
	 case, current matrices may still contain references to
	 images freed above.  So, clear these matrices.  */
      if (any_freed_p)
	{
	  clear_current_matrices (f);
	  ++windows_or_buffers_changed;
	}
    }
}


DEFUN ("clear-image-cache", Fclear_image_cache, Sclear_image_cache,
       0, 1, 0,
  "Clear the image cache of FRAME.\n\
FRAME nil or omitted means use the selected frame.\n\
FRAME t means clear the image caches of all frames.")
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
      img = make_image (spec, hash);
      cache_image (f, img);
      img->load_failed_p = img->type->load (f, img) == 0;
      xassert (!interrupt_input_blocked);

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
	     `:ascent PERCENT', `:margin MARGIN', `:relief RELIEF'.  */
	  Lisp_Object ascent, margin, relief, algorithm, heuristic_mask;
	  Lisp_Object file;

	  ascent = image_spec_value (spec, QCascent, NULL);
	  if (INTEGERP (ascent))
	    img->ascent = XFASTINT (ascent);
	  
	  margin = image_spec_value (spec, QCmargin, NULL);
	  if (INTEGERP (margin) && XINT (margin) >= 0)
	    img->margin = XFASTINT (margin);
	  
	  relief = image_spec_value (spec, QCrelief, NULL);
	  if (INTEGERP (relief))
	    {
	      img->relief = XINT (relief);
	      img->margin += abs (img->relief);
	    }

	  /* Should we apply a Laplace edge-detection algorithm?  */
	  algorithm = image_spec_value (spec, QCalgorithm, NULL);
	  if (img->pixmap && EQ (algorithm, Qlaplace))
	    x_laplace (f, img);

	  /* Should we built a mask heuristically?  */
	  heuristic_mask = image_spec_value (spec, QCheuristic_mask, NULL);
	  if (img->pixmap && !img->mask && !NILP (heuristic_mask))
	    {
	      file = image_spec_value (spec, QCfile, NULL);
	      x_build_heuristic_mask (f, file, img, heuristic_mask);
	    }
	}
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

static int x_create_x_image_and_pixmap P_ ((struct frame *, Lisp_Object,
					    int, int, int, XImage **,
					    Pixmap *));
static void x_destroy_x_image P_ ((XImage *));
static void x_put_x_image P_ ((struct frame *, XImage *, Pixmap, int, int));


/* Create an XImage and a pixmap of size WIDTH x HEIGHT for use on
   frame F.  Set *XIMG and *PIXMAP to the XImage and Pixmap created.
   Set (*XIMG)->data to a raster of WIDTH x HEIGHT pixels allocated
   via xmalloc.  Print error messages via image_error if an error
   occurs.  FILE is the name of an image file being processed, for
   error messages.  Value is non-zero if successful.  */

static int
x_create_x_image_and_pixmap (f, file, width, height, depth, ximg, pixmap)
     struct frame *f;
     Lisp_Object file;
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
      image_error ("Unable to allocate X image for %s", file, Qnil);
      return 0;
    }

  /* Allocate image raster.  */
  (*ximg)->data = (char *) xmalloc ((*ximg)->bytes_per_line * height);

  /* Allocate a pixmap of the same size.  */
  *pixmap = XCreatePixmap (display, window, width, height, depth);
  if (*pixmap == 0)
    {
      x_destroy_x_image (*ximg);
      *ximg = NULL;
      image_error ("Unable to create pixmap for `%s'", file, Qnil);
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
			      Searching files
 ***********************************************************************/

static Lisp_Object x_find_image_file P_ ((Lisp_Object));

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
  fd = openp (search_path, file, "", &file_found, 0);
  
  if (fd < 0)
    file_found = Qnil;
  else
    close (fd);

  UNGCPRO;
  return file_found;
}



/***********************************************************************
			      XBM images
 ***********************************************************************/

static int xbm_load P_ ((struct frame *f, struct image *img));
static int xbm_load_image_from_file P_ ((struct frame *f, struct image *img,
					 Lisp_Object file));
static int xbm_image_p P_ ((Lisp_Object object));
static int xbm_read_bitmap_file_data P_ ((char *, int *, int *,
					  unsigned char **));


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
  {":foreground",	IMAGE_STRING_VALUE,			0},
  {":background",	IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":algorithm",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0}
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

   Both the file and data forms may contain the additional entries
   `:background COLOR' and `:foreground COLOR'.  If not present,
   foreground and background of the frame on which the image is
   displayed, is used.  */

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

  /* Baseline must be a value between 0 and 100 (a percentage).  */
  if (kw[XBM_ASCENT].count
      && XFASTINT (kw[XBM_ASCENT].value) > 100)
    return 0;
  
  return 1;
}


/* Scan a bitmap file.  FP is the stream to read from.  Value is
   either an enumerator from enum xbm_token, or a character for a
   single-character token, or 0 at end of file.  If scanning an
   identifier, store the lexeme of the identifier in SVAL.  If
   scanning a number, store its value in *IVAL.  */

static int
xbm_scan (fp, sval, ival)
     FILE *fp;
     char *sval;
     int *ival;
{
  int c;
  
  /* Skip white space.  */
  while ((c = fgetc (fp)) != EOF && isspace (c))
    ;

  if (c == EOF)
    c = 0;
  else if (isdigit (c))
    {
      int value = 0, digit;
      
      if (c == '0')
	{
	  c = fgetc (fp);
	  if (c == 'x' || c == 'X')
	    {
	      while ((c = fgetc (fp)) != EOF)
		{
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
	      while ((c = fgetc (fp)) != EOF
		     && isdigit (c))
		value = 8 * value + c - '0';
	    }
	}
      else
	{
	  value = c - '0';
	  while ((c = fgetc (fp)) != EOF
		 && isdigit (c))
	    value = 10 * value + c - '0';
	}

      if (c != EOF)
	ungetc (c, fp);
      *ival = value;
      c = XBM_TK_NUMBER;
    }
  else if (isalpha (c) || c == '_')
    {
      *sval++ = c;
      while ((c = fgetc (fp)) != EOF
	     && (isalnum (c) || c == '_'))
	*sval++ = c;
      *sval = 0;
      if (c != EOF)
	ungetc (c, fp);
      c = XBM_TK_IDENT;
    }

  return c;
}


/* Replacement for XReadBitmapFileData which isn't available under old
   X versions.  FILE is the name of the bitmap file to read.  Set
   *WIDTH and *HEIGHT to the width and height of the image.  Return in
   *DATA the bitmap data allocated with xmalloc.  Value is non-zero if
   successful.  */

static int
xbm_read_bitmap_file_data (file, width, height, data)
     char *file;
     int *width, *height;
     unsigned char **data;
{
  FILE *fp;
  char buffer[BUFSIZ];
  int padding_p = 0;
  int v10 = 0;
  int bytes_per_line, i, nbytes;
  unsigned char *p;
  int value;
  int LA1;

#define match() \
     LA1 = xbm_scan (fp, buffer, &value)

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

  fp = fopen (file, "r");
  if (fp == NULL)
    return 0;

  *width = *height = -1;
  *data = NULL;
  LA1 = xbm_scan (fp, buffer, &value);

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

  fclose (fp);
  return 1;

 failure:
  
  fclose (fp);
  if (*data)
    {
      xfree (*data);
      *data = NULL;
    }
  return 0;

#undef match
#undef expect
#undef expect_ident
}


/* Load XBM image IMG which will be displayed on frame F from file
   SPECIFIED_FILE.  Value is non-zero if successful.  */

static int
xbm_load_image_from_file (f, img, specified_file)
     struct frame *f;
     struct image *img;
     Lisp_Object specified_file;
{
  int rc;
  unsigned char *data;
  int success_p = 0;
  Lisp_Object file;
  struct gcpro gcpro1;
  
  xassert (STRINGP (specified_file));
  file = Qnil;
  GCPRO1 (file);

  file = x_find_image_file (specified_file);
  if (!STRINGP (file))
    {
      image_error ("Cannot find image file %s", specified_file, Qnil);
      UNGCPRO;
      return 0;
    }
	  
  rc = xbm_read_bitmap_file_data (XSTRING (file)->data, &img->width,
				  &img->height, &data);
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
	background = x_alloc_image_color (f, img, value, background);

      BLOCK_INPUT;
      img->pixmap
	= XCreatePixmapFromBitmapData (FRAME_X_DISPLAY (f),
				       FRAME_X_WINDOW (f),
				       data,
				       img->width, img->height,
				       foreground, background,
				       depth);
      xfree (data);

      if (img->pixmap == 0)
	{
	  x_clear_image (f, img);
	  image_error ("Unable to create X pixmap for `%s'", file, Qnil);
	}
      else
	success_p = 1;
      
      UNBLOCK_INPUT;
    }
  else
    image_error ("Error loading XBM image %s", img->spec, Qnil);

  UNGCPRO;
  return success_p;
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
    success_p = xbm_load_image_from_file (f, img, file_name);
  else
    {
      struct image_keyword fmt[XBM_LAST];
      Lisp_Object data;
      int depth;
      unsigned long foreground = FRAME_FOREGROUND_PIXEL (f);
      unsigned long background = FRAME_BACKGROUND_PIXEL (f);
      char *bits;
      int parsed_p;

      /* Parse the list specification.  */
      bcopy (xbm_format, fmt, sizeof fmt);
      parsed_p = parse_image_spec (img->spec, fmt, XBM_LAST, Qxbm);
      xassert (parsed_p);

      /* Get specified width, and height.  */
      img->width = XFASTINT (fmt[XBM_WIDTH].value);
      img->height = XFASTINT (fmt[XBM_HEIGHT].value);
      xassert (img->width > 0 && img->height > 0);

      BLOCK_INPUT;
      
      if (fmt[XBM_ASCENT].count)
	img->ascent = XFASTINT (fmt[XBM_ASCENT].value);

      /* Get foreground and background colors, maybe allocate colors.  */
      if (fmt[XBM_FOREGROUND].count)
	foreground = x_alloc_image_color (f, img, fmt[XBM_FOREGROUND].value,
					  foreground);
      if (fmt[XBM_BACKGROUND].count)
	background = x_alloc_image_color (f, img, fmt[XBM_BACKGROUND].value,
					  background);

      /* Set bits to the bitmap image data.  */
      data = fmt[XBM_DATA].value;
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
	  image_error ("Unable to create pixmap for XBM image", Qnil, Qnil);
	  x_clear_image (f, img);
	}

      UNBLOCK_INPUT;
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
  XPM_COLOR_SYMBOLS,
  XPM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid XPM image specifications.  */

static struct image_keyword xpm_format[XPM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":data",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":algorithm",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":color-symbols",	IMAGE_DONT_CHECK_VALUE_TYPE,		0}
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
	      || xpm_valid_color_symbols_p (fmt[XPM_COLOR_SYMBOLS].value))
	  && (fmt[XPM_ASCENT].count == 0
	      || XFASTINT (fmt[XPM_ASCENT].value) < 100));
}


/* Load image IMG which will be displayed on frame F.  Value is
   non-zero if successful.  */

static int
xpm_load (f, img)
     struct frame *f;
     struct image *img;
{
  int rc, i;
  XpmAttributes attrs;
  Lisp_Object specified_file, color_symbols;

  /* Configure the XPM lib.  Use the visual of frame F.  Allocate
     close colors.  Return colors allocated.  */
  bzero (&attrs, sizeof attrs);
  attrs.visual = FRAME_X_DISPLAY_INFO (f)->visual;
  attrs.valuemask |= XpmVisual;
  attrs.valuemask |= XpmReturnAllocPixels;
#ifdef XpmAllocCloseColors
  attrs.alloc_close_colors = 1;
  attrs.valuemask |= XpmAllocCloseColors;
#else
  attrs.closeness = 600;
  attrs.valuemask |= XpmCloseness;
#endif

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
  BLOCK_INPUT;
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  if (STRINGP (specified_file))
    {
      Lisp_Object file = x_find_image_file (specified_file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file %s", specified_file, Qnil);
	  UNBLOCK_INPUT;
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
  UNBLOCK_INPUT;

  if (rc == XpmSuccess)
    {
      /* Remember allocated colors.  */
      img->ncolors = attrs.nalloc_pixels;
      img->colors = (unsigned long *) xmalloc (img->ncolors
					       * sizeof *img->colors);
      for (i = 0; i < attrs.nalloc_pixels; ++i)
	img->colors[i] = attrs.alloc_pixels[i];

      img->width = attrs.width;
      img->height = attrs.height;
      xassert (img->width > 0 && img->height > 0);

      /* The call to XpmFreeAttributes below frees attrs.alloc_pixels.  */
      BLOCK_INPUT;
      XpmFreeAttributes (&attrs);
      UNBLOCK_INPUT;
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

/* Function prototypes.  */

static void init_color_table P_ ((void));
static void free_color_table P_ ((void));
static unsigned long *colors_in_color_table P_ ((int *n));
static unsigned long lookup_rgb_color P_ ((struct frame *f, int r, int g, int b));
static unsigned long lookup_pixel_color P_ ((struct frame *f, unsigned long p));


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
      
      BLOCK_INPUT;
      cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
      rc = x_alloc_nearest_color (f, cmap, &color);
      UNBLOCK_INPUT;

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

      BLOCK_INPUT;
      
      cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
      color.pixel = pixel;
      XQueryColor (FRAME_X_DISPLAY (f), cmap, &color);
      rc = x_alloc_nearest_color (f, cmap, &color);
      UNBLOCK_INPUT;

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

static void x_laplace_write_row P_ ((struct frame *, long *,
				     int, XImage *, int));
static void x_laplace_read_row P_ ((struct frame *, Colormap,
				    XColor *, int, XImage *, int));


/* Fill COLORS with RGB colors from row Y of image XIMG.  F is the
   frame we operate on, CMAP is the color-map in effect, and WIDTH is
   the width of one row in the image.  */

static void
x_laplace_read_row (f, cmap, colors, width, ximg, y)
     struct frame *f;
     Colormap cmap;
     XColor *colors;
     int width;
     XImage *ximg;
     int y;
{
  int x;

  for (x = 0; x < width; ++x)
    colors[x].pixel = XGetPixel (ximg, x, y);

  XQueryColors (FRAME_X_DISPLAY (f), cmap, colors, width);
}


/* Write row Y of image XIMG.  PIXELS is an array of WIDTH longs
   containing the pixel colors to write.  F is the frame we are
   working on.  */

static void
x_laplace_write_row (f, pixels, width, ximg, y)
     struct frame *f;
     long *pixels;
     int width;
     XImage *ximg;
     int y;
{
  int x;
  
  for (x = 0; x < width; ++x)
    XPutPixel (ximg, x, y, pixels[x]);
}


/* Transform image IMG which is used on frame F with a Laplace
   edge-detection algorithm.  The result is an image that can be used
   to draw disabled buttons, for example.  */

static void
x_laplace (f, img)
     struct frame *f;
     struct image *img;
{
  Colormap cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
  XImage *ximg, *oimg;
  XColor *in[3];
  long *out;
  Pixmap pixmap;
  int x, y, i;
  long pixel;
  int in_y, out_y, rc;
  int mv2 = 45000;

  BLOCK_INPUT;

  /* Get the X image IMG->pixmap.  */
  ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
		    0, 0, img->width, img->height, ~0, ZPixmap);

  /* Allocate 3 input rows, and one output row of colors.  */
  for (i = 0; i < 3; ++i)
    in[i] = (XColor *) alloca (img->width * sizeof (XColor));
  out = (long *) alloca (img->width * sizeof (long));

  /* Create an X image for output.  */
  rc = x_create_x_image_and_pixmap (f, Qnil, img->width, img->height, 0,
				    &oimg, &pixmap);

  /* Fill first two rows.  */
  x_laplace_read_row (f, cmap, in[0], img->width, ximg, 0);
  x_laplace_read_row (f, cmap, in[1], img->width, ximg, 1);
  in_y = 2;

  /* Write first row, all zeros.  */
  init_color_table ();
  pixel = lookup_rgb_color (f, 0, 0, 0);
  for (x = 0; x < img->width; ++x)
    out[x] = pixel;
  x_laplace_write_row (f, out, img->width, oimg, 0);
  out_y = 1;

  for (y = 2; y < img->height; ++y)
    {
      int rowa = y % 3;
      int rowb = (y + 2) % 3;

      x_laplace_read_row (f, cmap, in[rowa], img->width, ximg, in_y++);

      for (x = 0; x < img->width - 2; ++x)
	{
	  int r = in[rowa][x].red + mv2 - in[rowb][x + 2].red;
	  int g = in[rowa][x].green + mv2 - in[rowb][x + 2].green;
	  int b = in[rowa][x].blue + mv2 - in[rowb][x + 2].blue;
	  
	  out[x + 1] = lookup_rgb_color (f, r & 0xffff, g & 0xffff,
					 b & 0xffff);
	}

      x_laplace_write_row (f, out, img->width, oimg, out_y++);
    }

  /* Write last line, all zeros.  */
  for (x = 0; x < img->width; ++x)
    out[x] = pixel;
  x_laplace_write_row (f, out, img->width, oimg, out_y);

  /* Free the input image, and free resources of IMG.  */
  XDestroyImage (ximg);
  x_clear_image (f, img);
  
  /* Put the output image into pixmap, and destroy it.  */
  x_put_x_image (f, oimg, pixmap, img->width, img->height);
  x_destroy_x_image (oimg);

  /* Remember new pixmap and colors in IMG.  */
  img->pixmap = pixmap;
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();

  UNBLOCK_INPUT;
}


/* Build a mask for image IMG which is used on frame F.  FILE is the
   name of an image file, for error messages.  HOW determines how to
   determine the background color of IMG.  If it is a list '(R G B)',
   with R, G, and B being integers >= 0, take that as the color of the
   background.  Otherwise, determine the background color of IMG
   heuristically.  Value is non-zero if successful. */

static int
x_build_heuristic_mask (f, file, img, how)
     struct frame *f;
     Lisp_Object file;
     struct image *img;
     Lisp_Object how;
{
  Display *dpy = FRAME_X_DISPLAY (f);
  XImage *ximg, *mask_img;
  int x, y, rc, look_at_corners_p;
  unsigned long bg;

  BLOCK_INPUT;
  
  /* Create an image and pixmap serving as mask.  */
  rc = x_create_x_image_and_pixmap (f, file, img->width, img->height, 1,
				    &mask_img, &img->mask);
  if (!rc)
    {
      UNBLOCK_INPUT;
      return 0;
    }

  /* Get the X image of IMG->pixmap.  */
  ximg = XGetImage (dpy, img->pixmap, 0, 0, img->width, img->height,
		    ~0, ZPixmap);

  /* Determine the background color of ximg.  If HOW is `(R G B)'
     take that as color.  Otherwise, try to determine the color
     heuristically. */
  look_at_corners_p = 1;
  
  if (CONSP (how))
    {
      int rgb[3], i = 0;

      while (i < 3
	     && CONSP (how)
	     && NATNUMP (XCAR (how)))
	{
	  rgb[i] = XFASTINT (XCAR (how)) & 0xffff;
	  how = XCDR (how);
	}

      if (i == 3 && NILP (how))
	{
	  char color_name[30];
	  XColor exact, color;
	  Colormap cmap;

	  sprintf (color_name, "#%04x%04x%04x", rgb[0], rgb[1], rgb[2]);
	  
	  cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
	  if (XLookupColor (dpy, cmap, color_name, &exact, &color))
	    {
	      bg = color.pixel;
	      look_at_corners_p = 0;
	    }
	}
    }
  
  if (look_at_corners_p)
    {
      unsigned long corners[4];
      int i, best_count;

      /* Get the colors at the corners of ximg.  */
      corners[0] = XGetPixel (ximg, 0, 0);
      corners[1] = XGetPixel (ximg, img->width - 1, 0);
      corners[2] = XGetPixel (ximg, img->width - 1, img->height - 1);
      corners[3] = XGetPixel (ximg, 0, img->height - 1);

      /* Choose the most frequently found color as background.  */
      for (i = best_count = 0; i < 4; ++i)
	{
	  int j, n;
	  
	  for (j = n = 0; j < 4; ++j)
	    if (corners[i] == corners[j])
	      ++n;

	  if (n > best_count)
	    bg = corners[i], best_count = n;
	}
    }

  /* Set all bits in mask_img to 1 whose color in ximg is different
     from the background color bg.  */
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x)
      XPutPixel (mask_img, x, y, XGetPixel (ximg, x, y) != bg);

  /* Put mask_img into img->mask.  */
  x_put_x_image (f, mask_img, img->mask, img->width, img->height);
  x_destroy_x_image (mask_img);
  XDestroyImage (ximg);
  
  UNBLOCK_INPUT;
  return 1;
}



/***********************************************************************
		       PBM (mono, gray, color)
 ***********************************************************************/

static int pbm_image_p P_ ((Lisp_Object object));
static int pbm_load P_ ((struct frame *f, struct image *img));
static int pbm_scan_number P_ ((FILE *fp));

/* The symbol `pbm' identifying images of this type.  */

Lisp_Object Qpbm;

/* Indices of image specification fields in gs_format, below.  */

enum pbm_keyword_index
{
  PBM_TYPE,
  PBM_FILE,
  PBM_ASCENT,
  PBM_MARGIN,
  PBM_RELIEF,
  PBM_ALGORITHM,
  PBM_HEURISTIC_MASK,
  PBM_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword pbm_format[PBM_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			1},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":algorithm",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0}
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
  
  if (!parse_image_spec (object, fmt, PBM_LAST, Qpbm)
      || (fmt[PBM_ASCENT].count 
	  && XFASTINT (fmt[PBM_ASCENT].value) > 100))
    return 0;
  return 1;
}


/* Scan a decimal number from PBM input file FP and return it.  Value
   is -1 at end of file or if an error occurs.  */

static int
pbm_scan_number (fp)
     FILE *fp;
{
  int c, val = -1;

  while (!feof (fp))
    {
      /* Skip white-space.  */
      while ((c = fgetc (fp)) != EOF && isspace (c))
	;

      if (c == '#')
	{
	  /* Skip comment to end of line.  */
	  while ((c = fgetc (fp)) != EOF && c != '\n')
	    ;
	}
      else if (isdigit (c))
	{
	  /* Read decimal number.  */
	  val = c - '0';
	  while ((c = fgetc (fp)) != EOF && isdigit (c))
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
  FILE *fp;
  char magic[2];
  int raw_p, x, y;
  int width, height, max_color_idx = 0;
  XImage *ximg;
  Lisp_Object file, specified_file;
  enum {PBM_MONO, PBM_GRAY, PBM_COLOR} type;
  struct gcpro gcpro1;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  file = x_find_image_file (specified_file);
  GCPRO1 (file);
  if (!STRINGP (file))
    {
      image_error ("Cannot find image file %s", specified_file, Qnil);
      UNGCPRO;
      return 0;
    }

  fp = fopen (XSTRING (file)->data, "r");
  if (fp == NULL)
    {
      UNGCPRO;
      return 0;
    }

  /* Read first two characters.  */
  if (fread (magic, sizeof *magic, 2, fp) != 2)
    {
      fclose (fp);
      image_error ("Not a PBM image file: %s", file, Qnil);
      UNGCPRO;
      return 0;
    }

  if (*magic != 'P')
    {
      fclose (fp);
      image_error ("Not a PBM image file: %s", file, Qnil);
      UNGCPRO;
      return 0;
    }

  switch (magic[1])
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
      fclose (fp);
      image_error ("Not a PBM image file: %s", file, Qnil);
      UNGCPRO;
      return 0;
    }

  /* Read width, height, maximum color-component.  Characters
     starting with `#' up to the end of a line are ignored.  */
  width = pbm_scan_number (fp);
  height = pbm_scan_number (fp);

  if (type != PBM_MONO)
    {
      max_color_idx = pbm_scan_number (fp);
      if (raw_p && max_color_idx > 255)
	max_color_idx = 255;
    }
  
  if (width < 0 || height < 0
      || (type != PBM_MONO && max_color_idx < 0))
    {
      fclose (fp);
      UNGCPRO;
      return 0;
    }

  BLOCK_INPUT;
  if (!x_create_x_image_and_pixmap (f, file, width, height, 0,
				    &ximg, &img->pixmap))
    {
      fclose (fp);
      UNBLOCK_INPUT;
      UNGCPRO;
      return 0;
    }
  
  /* Initialize the color hash table.  */
  init_color_table ();

  if (type == PBM_MONO)
    {
      int c = 0, g;
      
      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    if (raw_p)
	      {
		if ((x & 7) == 0)
		  c = fgetc (fp);
		g = c & 0x80;
		c <<= 1;
	      }
	    else
	      g = pbm_scan_number (fp);

	    XPutPixel (ximg, x, y, (g
				    ? FRAME_FOREGROUND_PIXEL (f)
				    : FRAME_BACKGROUND_PIXEL (f)));
	  }
    }
  else
    {
      for (y = 0; y < height; ++y)
	for (x = 0; x < width; ++x)
	  {
	    int r, g, b;
	    
	    if (type == PBM_GRAY)
	      r = g = b = raw_p ? fgetc (fp) : pbm_scan_number (fp);
	    else if (raw_p)
	      {
		r = fgetc (fp);
		g = fgetc (fp);
		b = fgetc (fp);
	      }
	    else
	      {
		r = pbm_scan_number (fp);
		g = pbm_scan_number (fp);
		b = pbm_scan_number (fp);
	      }
	    
	    if (r < 0 || g < 0 || b < 0)
	      {
		fclose (fp);
		xfree (ximg->data);
		ximg->data = NULL;
		XDestroyImage (ximg);
		UNBLOCK_INPUT;
		image_error ("Invalid pixel value in file `%s'",
			     file, Qnil);
		UNGCPRO;
		return 0;
	      }
	    
	    /* RGB values are now in the range 0..max_color_idx.
	       Scale this to the range 0..0xffff supported by X.  */
	    r = (double) r * 65535 / max_color_idx;
	    g = (double) g * 65535 / max_color_idx;
	    b = (double) b * 65535 / max_color_idx;
	    XPutPixel (ximg, x, y, lookup_rgb_color (f, r, g, b));
	  }
    }
  
  fclose (fp);

  /* Store in IMG->colors the colors allocated for the image, and
     free the color table.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
  
  /* Put the image into a pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  UNBLOCK_INPUT;
      
  img->width = width;
  img->height = height;

  UNGCPRO;
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
  PNG_FILE,
  PNG_ASCENT,
  PNG_MARGIN,
  PNG_RELIEF,
  PNG_ALGORITHM,
  PNG_HEURISTIC_MASK,
  PNG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword png_format[PNG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			1},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":algorithm",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0}
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
  
  if (!parse_image_spec (object, fmt, PNG_LAST, Qpng)
      || (fmt[PNG_ASCENT].count 
	  && XFASTINT (fmt[PNG_ASCENT].value) > 100))
    return 0;
  return 1;
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


/* Load PNG image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int
png_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  int x, y, i;
  XImage *ximg, *mask_img = NULL;
  struct gcpro gcpro1;
  png_struct *png_ptr = NULL;
  png_info *info_ptr = NULL, *end_info = NULL;
  FILE *fp;
  png_byte sig[8];
  png_byte *pixels = NULL;
  png_byte **rows = NULL;
  png_uint_32 width, height;
  int bit_depth, color_type, interlace_type;
  png_byte channels;
  png_uint_32 row_bytes;
  int transparent_p;
  char *gamma_str;
  double screen_gamma, image_gamma;
  int intent;

  /* Find out what file to load.  */
  specified_file = image_spec_value (img->spec, QCfile, NULL);
  file = x_find_image_file (specified_file);
  GCPRO1 (file);
  if (!STRINGP (file))
    {
      image_error ("Cannot find image file %s", specified_file, Qnil);
      UNGCPRO;
      return 0;
    }

  /* Open the image file.  */
  fp = fopen (XSTRING (file)->data, "rb");
  if (!fp)
    {
      image_error ("Cannot open image file %s", file, Qnil);
      UNGCPRO;
      fclose (fp);
      return 0;
    }

  /* Check PNG signature.  */
  if (fread (sig, 1, sizeof sig, fp) != sizeof sig
      || !png_check_sig (sig, sizeof sig))
    {
      image_error ("Not a PNG file: %s", file, Qnil);
      UNGCPRO;
      fclose (fp);
      return 0;
    }

  /* Initialize read and info structs for PNG lib.  */
  png_ptr = png_create_read_struct (PNG_LIBPNG_VER_STRING, NULL,
				    my_png_error, my_png_warning);
  if (!png_ptr)
    {
      fclose (fp);
      UNGCPRO;
      return 0;
    }

  info_ptr = png_create_info_struct (png_ptr);
  if (!info_ptr)
    {
      png_destroy_read_struct (&png_ptr, NULL, NULL);
      fclose (fp);
      UNGCPRO;
      return 0;
    }

  end_info = png_create_info_struct (png_ptr);
  if (!end_info)
    {
      png_destroy_read_struct (&png_ptr, &info_ptr, NULL);
      fclose (fp);
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
      if (fp)
	fclose (fp);
      UNGCPRO;
      return 0;
    }

  /* Read image info.  */
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
      png_color_16 *image_background;

      if (png_get_bKGD (png_ptr, info_ptr, &image_background))
	/* Image contains a background color with which to 
	   combine the image.  */
	png_set_background (png_ptr, image_background,
			    PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
      else
	{
	  /* Image does not contain a background color with which
	     to combine the image data via an alpha channel.  Use 
	     the frame's background instead.  */
	  XColor color;
	  Colormap cmap;
	  png_color_16 frame_background;

	  BLOCK_INPUT;
	  cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
	  color.pixel = FRAME_BACKGROUND_PIXEL (f);
	  XQueryColor (FRAME_X_DISPLAY (f), cmap, &color);
	  UNBLOCK_INPUT;

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
  fclose (fp);
  fp = NULL;
  
  BLOCK_INPUT;

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, file, width, height, 0, &ximg,
				    &img->pixmap))
    {
      UNBLOCK_INPUT;
      goto error;
    }
  
  /* Create an image and pixmap serving as mask if the PNG image
     contains an alpha channel.  */
  if (channels == 4
      && !transparent_p
      && !x_create_x_image_and_pixmap (f, file, width, height, 1,
				       &mask_img, &img->mask))
    {
      x_destroy_x_image (ximg);
      XFreePixmap (FRAME_X_DISPLAY (f), img->pixmap);
      img->pixmap = 0;
      UNBLOCK_INPUT;
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

  /* Remember colors allocated for this image.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();

  /* Clean up.  */
  png_destroy_read_struct (&png_ptr, &info_ptr, &end_info);
  xfree (rows);
  xfree (pixels);

  img->width = width;
  img->height = height;

  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);

  /* Same for the mask.  */
  if (mask_img)
    {
      x_put_x_image (f, mask_img, img->mask, img->width, img->height);
      x_destroy_x_image (mask_img);
    }

  UNBLOCK_INPUT;
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
  JPEG_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword jpeg_format[JPEG_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":data",     IMAGE_STRING_VALUE,         0},
  {":file",		IMAGE_STRING_VALUE,			0},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":algorithm",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0}
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
  
  if (!parse_image_spec (object, fmt, JPEG_LAST, Qjpeg)
      || (fmt[JPEG_ASCENT].count 
	  && XFASTINT (fmt[JPEG_ASCENT].value) > 100))
    return 0;

  /* Must specify either the :data or :file keyword.  This should
     probably be moved up into parse_image_spec, since it seems to be
     a general requirement. */
  return fmt[JPEG_FILE].count || fmt[JPEG_DATA].count;
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
	{
	  ERREXIT (cinfo, JERR_INPUT_EOF);
	  /*NOTREACHED*/
	}
      
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
  FILE *fp = NULL;
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

  /* Reading from :data takes precedence.  */
  if (NILP (specified_data))
    {
      file = x_find_image_file (specified_file);
      GCPRO1 (file);
      if (!STRINGP (file))
	{
	  image_error ("Cannot find image file %s", specified_file, Qnil);
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

  /* Customize libjpeg's error handling to call my_error_exit
     when an error is detected.  This function will perform
     a longjmp.  */
  mgr.pub.error_exit = my_error_exit;
  cinfo.err = jpeg_std_error (&mgr.pub);
  
  if ((rc = setjmp (mgr.setjmp_buffer)) != 0)
    {
      if (rc == 1)
	{
	  /* Called from my_error_exit.  Display a JPEG error.  */
	  char buffer[JMSG_LENGTH_MAX];
	  cinfo.err->format_message ((j_common_ptr) &cinfo, buffer);
	  image_error ("Error reading JPEG file `%s': %s", file,
		       build_string (buffer));
	}
	  
      /* Close the input file and destroy the JPEG object.  */
	  if (fp) fclose (fp);
      jpeg_destroy_decompress (&cinfo);

      BLOCK_INPUT;
      
      /* If we already have an XImage, free that.  */
      x_destroy_x_image (ximg);

      /* Free pixmap and colors.  */
      x_clear_image (f, img);
      
      UNBLOCK_INPUT;
      UNGCPRO;
      return 0;
    }

  /* Create the JPEG decompression object.  Let it read from fp.
     Read the JPEG image header.  */
  jpeg_create_decompress (&cinfo);

  if (NILP (specified_data))
    jpeg_stdio_src (&cinfo, fp);
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

  BLOCK_INPUT;

  /* Create X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, file, width, height, 0, &ximg,
				    &img->pixmap))
    {
      UNBLOCK_INPUT;
      longjmp (mgr.setjmp_buffer, 2);
    }

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
  if (fp) fclose (fp);
  
  /* Put the image into the pixmap.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  UNBLOCK_INPUT;
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
  TIFF_FILE,
  TIFF_ASCENT,
  TIFF_MARGIN,
  TIFF_RELIEF,
  TIFF_ALGORITHM,
  TIFF_HEURISTIC_MASK,
  TIFF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword tiff_format[TIFF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			1},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":algorithm",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0}
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
  
  if (!parse_image_spec (object, fmt, TIFF_LAST, Qtiff)
      || (fmt[TIFF_ASCENT].count 
	  && XFASTINT (fmt[TIFF_ASCENT].value) > 100))
    return 0;
  return 1;
}


/* Load TIFF image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int
tiff_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  TIFF *tiff;
  int width, height, x, y;
  uint32 *buf;
  int rc;
  XImage *ximg;
  struct gcpro gcpro1;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  file = x_find_image_file (specified_file);
  GCPRO1 (file);
  if (!STRINGP (file))
    {
      image_error ("Cannot find image file %s", file, Qnil);
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

  /* Get width and height of the image, and allocate a raster buffer
     of width x height 32-bit values.  */
  TIFFGetField (tiff, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField (tiff, TIFFTAG_IMAGELENGTH, &height);
  buf = (uint32 *) xmalloc (width * height * sizeof *buf);
  
  rc = TIFFReadRGBAImage (tiff, width, height, buf, 0);
  TIFFClose (tiff);
  if (!rc)
    {
      image_error ("Error reading `%s'", file, Qnil);
      xfree (buf);
      UNGCPRO;
      return 0;
    }

  BLOCK_INPUT;

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, file, width, height, 0, &ximg, 
				    &img->pixmap))
    {
      UNBLOCK_INPUT;
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

  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  xfree (buf);
  UNBLOCK_INPUT;
      
  img->width = width;
  img->height = height;

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
  GIF_FILE,
  GIF_ASCENT,
  GIF_MARGIN,
  GIF_RELIEF,
  GIF_ALGORITHM,
  GIF_HEURISTIC_MASK,
  GIF_IMAGE,
  GIF_LAST
};

/* Vector of image_keyword structures describing the format
   of valid user-defined image specifications.  */

static struct image_keyword gif_format[GIF_LAST] =
{
  {":type",		IMAGE_SYMBOL_VALUE,			1},
  {":file",		IMAGE_STRING_VALUE,			1},
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":algorithm",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":image",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0}
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
  
  if (!parse_image_spec (object, fmt, GIF_LAST, Qgif)
      || (fmt[GIF_ASCENT].count 
	  && XFASTINT (fmt[GIF_ASCENT].value) > 100))
    return 0;
  return 1;
}


/* Load GIF image IMG for use on frame F.  Value is non-zero if
   successful.  */

static int
gif_load (f, img)
     struct frame *f;
     struct image *img;
{
  Lisp_Object file, specified_file;
  int rc, width, height, x, y, i;
  XImage *ximg;
  ColorMapObject *gif_color_map;
  unsigned long pixel_colors[256];
  GifFileType *gif;
  struct gcpro gcpro1;
  Lisp_Object image;
  int ino, image_left, image_top, image_width, image_height;

  specified_file = image_spec_value (img->spec, QCfile, NULL);
  file = x_find_image_file (specified_file);
  GCPRO1 (file);
  if (!STRINGP (file))
    {
      image_error ("Cannot find image file %s", specified_file, Qnil);
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

  /* Read entire contents.  */
  rc = DGifSlurp (gif);
  if (rc == GIF_ERROR)
    {
      image_error ("Error reading `%s'", file, Qnil);
      DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  image = image_spec_value (img->spec, QCindex, NULL);
  ino = INTEGERP (image) ? XFASTINT (image) : 0;
  if (ino >= gif->ImageCount)
    {
      image_error ("Invalid image number `%s'", image, Qnil);
      DGifCloseFile (gif);
      UNGCPRO;
      return 0;
    }

  width = img->width = gif->SWidth;
  height = img->height = gif->SHeight;

  BLOCK_INPUT;

  /* Create the X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, file, width, height, 0, &ximg,
				    &img->pixmap))
    {
      UNBLOCK_INPUT;
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

  /* Read the GIF image into the X image.  */
  if (gif->SavedImages[ino].ImageDesc.Interlace)
    {
      static int interlace_start[] = {0, 4, 2, 1};
      static int interlace_increment[] = {8, 8, 4, 2};
      int pass, inc;
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
	      unsigned int i
		= gif->SavedImages[ino].RasterBits[(y * image_width) + x];
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
	    unsigned i = gif->SavedImages[ino].RasterBits[y * image_width + x];
	    XPutPixel (ximg, x + image_left, y + image_top, pixel_colors[i]);
	  }
    }
  
  DGifCloseFile (gif);
  
  /* Put the image into the pixmap, then free the X image and its buffer.  */
  x_put_x_image (f, ximg, img->pixmap, width, height);
  x_destroy_x_image (ximg);
  UNBLOCK_INPUT;
      
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
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE,		0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":algorithm",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0}
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
  
  if (!parse_image_spec (object, fmt, GS_LAST, Qpostscript)
      || (fmt[GS_ASCENT].count 
	  && XFASTINT (fmt[GS_ASCENT].value) > 100))
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
  BLOCK_INPUT;
  xassert (img->pixmap == 0);
  img->pixmap = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			       img->width, img->height,
			       DefaultDepthOfScreen (FRAME_X_SCREEN (f)));
  UNBLOCK_INPUT;

  if (!img->pixmap)
    {
      image_error ("Unable to create pixmap for `%s'",
		   image_spec_value (img->spec, QCfile, NULL), Qnil);
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

  /* Kill the GS process.  We should have found PIXMAP in the image
     cache and its image should contain a process object.  */
  xassert (i < c->used);
  img = c->images[i];
  xassert (PROCESSP (img->data.lisp_val));
  Fkill_process (img->data.lisp_val, Qnil);
  img->data.lisp_val = Qnil;

  /* On displays with a mutable colormap, figure out the colors
     allocated for the image by looking at the pixels of an XImage for
     img->pixmap.  */
  class = FRAME_X_DISPLAY_INFO (f)->visual->class;
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
	    {
	      Colormap cmap = DefaultColormapOfScreen (FRAME_X_SCREEN (f));
	      XFreeColors (FRAME_X_DISPLAY (f), cmap,
			   img->colors, img->ncolors, 0);
	    }
#endif
	}
      else
	image_error ("Cannot get X image of `%s'; colors will not be freed",
		     image_spec_value (img->spec, QCfile, NULL), Qnil);
      
      UNBLOCK_INPUT;
    }
}



/***********************************************************************
                           Window properties
 ***********************************************************************/

DEFUN ("x-change-window-property", Fx_change_window_property,
       Sx_change_window_property, 2, 3, 0,
  "Change window property PROP to VALUE on the X window of FRAME.\n\
PROP and VALUE must be strings.  FRAME nil or omitted means use the\n\
selected frame.  Value is VALUE.")
  (prop, value, frame)
     Lisp_Object frame, prop, value;
{
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop, 1);
  CHECK_STRING (value, 2);

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
  "Remove window property PROP from X window of FRAME.\n\
FRAME nil or omitted means use the selected frame.  Value is PROP.")
  (prop, frame)
     Lisp_Object prop, frame;
{
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop, 1);
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
  "Value is the value of window property PROP on FRAME.\n\
If FRAME is nil or omitted, use the selected frame.  Value is nil\n\
if FRAME hasn't a property with name PROP or if PROP has no string\n\
value.")
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

  CHECK_STRING (prop, 1);
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
      if (rc == Success)
	prop_value = make_string (tmp_data, size);

      XFree (tmp_data);
    }

  UNBLOCK_INPUT;
  return prop_value;
}



/***********************************************************************
				Busy cursor
 ***********************************************************************/

/* The implementation partly follows a patch from
   F.Pierresteguy@frcl.bull.fr dated 1994.  */

/* Setting inhibit_busy_cursor to 2 inhibits busy-cursor display until
   the next X event is read and we enter XTread_socket again.  Setting
   it to 1 inhibits busy-cursor display for direct commands.  */

int inhibit_busy_cursor;

/* Incremented with each call to x-display-busy-cursor.
   Decremented in x-undisplay-busy-cursor.  */

static int busy_count;


DEFUN ("x-show-busy-cursor", Fx_show_busy_cursor,
       Sx_show_busy_cursor, 0, 0, 0,
  "Show a busy cursor, if not already shown.\n\
Each call to this function must be matched by a call to\n\
`x-hide-busy-cursor' to make the busy pointer disappear again.")
  ()
{
  ++busy_count;
  if (busy_count == 1)
    {
      Lisp_Object rest, frame;

      FOR_EACH_FRAME (rest, frame)
	if (FRAME_X_P (XFRAME (frame)))
	  {
	    struct frame *f = XFRAME (frame);
	    
	    BLOCK_INPUT;
	    f->output_data.x->busy_p = 1;
	    
	    if (!f->output_data.x->busy_window)
	      {
		unsigned long mask = CWCursor;
		XSetWindowAttributes attrs;

		attrs.cursor = f->output_data.x->busy_cursor;
		
		f->output_data.x->busy_window
		  = XCreateWindow (FRAME_X_DISPLAY (f),
				   FRAME_OUTER_WINDOW (f),
				   0, 0, 32000, 32000, 0, 0,
				   InputOnly,
				   CopyFromParent,
				   mask, &attrs);
	      }

	    XMapRaised (FRAME_X_DISPLAY (f), f->output_data.x->busy_window);
	    UNBLOCK_INPUT;
	  }
    }

  return Qnil;
}


DEFUN ("x-hide-busy-cursor", Fx_hide_busy_cursor,
       Sx_hide_busy_cursor, 0, 1, 0,
  "Hide a busy-cursor.\n\
A busy-cursor will actually be undisplayed when a matching\n\
`x-hide-busy-cursor' is called for each `x-show-busy-cursor'\n\
issued.  FORCE non-nil means hide the busy-cursor forcibly,\n\
not counting calls.")
  (force)
     Lisp_Object force;
{
  Lisp_Object rest, frame;

  if (busy_count == 0)
    return Qnil;

  if (!NILP (force) && busy_count != 0)
    busy_count = 1;

  --busy_count;
  if (busy_count != 0)
    return Qnil;

  FOR_EACH_FRAME (rest, frame)
    {
      struct frame *f = XFRAME (frame);
      
      if (FRAME_X_P (f)
	  /* Watch out for newly created frames.  */
	  && f->output_data.x->busy_window)
	{
	  
	  BLOCK_INPUT;
	  XUnmapWindow (FRAME_X_DISPLAY (f), f->output_data.x->busy_window);
	  /* Sync here because XTread_socket looks at the busy_p flag
	     that is reset to zero below.  */
	  XSync (FRAME_X_DISPLAY (f), False);
	  UNBLOCK_INPUT;
	  f->output_data.x->busy_p = 0;
	}
    }

  return Qnil;
}



/***********************************************************************
				Tool tips
 ***********************************************************************/

static Lisp_Object x_create_tip_frame P_ ((struct x_display_info *,
					   Lisp_Object));
     
/* The frame of a currently visible tooltip, or null.  */

struct frame *tip_frame;

/* If non-nil, a timer started that hides the last tooltip when it
   fires.  */

Lisp_Object tip_timer;
Window tip_window;

/* Create a frame for a tooltip on the display described by DPYINFO.
   PARMS is a list of frame parameters.  Value is the frame.  */

static Lisp_Object
x_create_tip_frame (dpyinfo, parms)
     struct x_display_info *dpyinfo;
     Lisp_Object parms;
{
  struct frame *f;
  Lisp_Object frame, tem;
  Lisp_Object name;
  long window_prompting = 0;
  int width, height;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct kboard *kb;

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
  tip_frame = f = make_frame (1);
  XSETFRAME (frame, f);
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 0;

  f->output_method = output_x_window;
  f->output_data.x = (struct x_output *) xmalloc (sizeof (struct x_output));
  bzero (f->output_data.x, sizeof (struct x_output));
  f->output_data.x->icon_bitmap = -1;
  f->output_data.x->fontset = -1;
  f->icon_name = Qnil;
  FRAME_X_DISPLAY_INFO (f) = dpyinfo;
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif
  f->output_data.x->parent_desc = FRAME_X_DISPLAY_INFO (f)->root_window;
  f->output_data.x->explicit_parent = 0;

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

  /* Create fontsets from `global_fontset_alist' before handling fonts.  */
  for (tem = Vglobal_fontset_alist; CONSP (tem); tem = XCDR (tem))
    fs_register_fontset (f, XCAR (tem));

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
    mask = CWBackPixel | CWOverrideRedirect | CWSaveUnder | CWEventMask;
    /* Window managers looks at the override-redirect flag to
       determine whether or net to give windows a decoration (Xlib
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

  f->no_split = 1;

  UNGCPRO;

  /* It is now ok to make the frame official even if we get an error
     below.  And the frame needs to be on Vframe_list or making it
     visible won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);

  /* Now that the frame is official, it counts as a reference to
     its display.  */
  FRAME_X_DISPLAY_INFO (f)->reference_count++;

  return unbind_to (count, frame);
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 4, 0,
  "Show STRING in a \"tooltip\" window on frame FRAME.\n\
A tooltip window is a small X window displaying STRING at\n\
the current mouse position.\n\
FRAME nil or omitted means use the selected frame.\n\
PARMS is an optional list of frame parameters which can be\n\
used to change the tooltip's appearance.\n\
Automatically hide the tooltip after TIMEOUT seconds.\n\
TIMEOUT nil means use the default timeout of 5 seconds.")
  (string, frame, parms, timeout)
     Lisp_Object string, frame, parms, timeout;
{
  struct frame *f;
  struct window *w;
  Window root, child;
  Lisp_Object buffer;
  struct buffer *old_buffer;
  struct text_pos pos;
  int i, width, height;
  int root_x, root_y, win_x, win_y;
  unsigned pmask;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int old_windows_or_buffers_changed = windows_or_buffers_changed;
  int count = specpdl_ptr - specpdl;
  
  specbind (Qinhibit_redisplay, Qt);

  GCPRO4 (string, parms, frame, timeout);

  CHECK_STRING (string, 0);
  f = check_x_frame (frame);
  if (NILP (timeout))
    timeout = make_number (5);
  else
    CHECK_NATNUM (timeout, 2);

  /* Hide a previous tip, if any.  */
  Fx_hide_tip ();

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
  frame = x_create_tip_frame (FRAME_X_DISPLAY_INFO (f), parms);
  tip_frame = f = XFRAME (frame);

  /* Set up the frame's root window.  Currently we use a size of 80
     columns x 40 lines.  If someone wants to show a larger tip, he
     will loose.  I don't think this is a realistic case.  */
  w = XWINDOW (FRAME_ROOT_WINDOW (f));
  w->left = w->top = make_number (0);
  w->width = 80;
  w->height = 40;
  adjust_glyphs (f);
  w->pseudo_window_p = 1;

  /* Display the tooltip text in a temporary buffer.  */
  buffer = Fget_buffer_create (build_string (" *tip*"));
  Fset_window_buffer (FRAME_ROOT_WINDOW (f), buffer);
  old_buffer = current_buffer;
  set_buffer_internal_1 (XBUFFER (buffer));
  Ferase_buffer ();
  Finsert (make_number (1), &string);
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

      /* There's a glyph at the end of rows that is use to place
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
  BLOCK_INPUT;
  XQueryPointer (FRAME_X_DISPLAY (f), FRAME_X_DISPLAY_INFO (f)->root_window,
		 &root, &child, &root_x, &root_y, &win_x, &win_y, &pmask);
  XMoveResizeWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     root_x + 5, root_y - height - 5, width, height);
  XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
  UNBLOCK_INPUT;

  /* Draw into the window.  */
  w->must_be_updated_p = 1;
  update_single_window (w, 1);

  /* Restore original current buffer.  */
  set_buffer_internal_1 (old_buffer);
  windows_or_buffers_changed = old_windows_or_buffers_changed;

  /* Let the tip disappear after timeout seconds.  */
  tip_timer = call3 (intern ("run-at-time"), timeout, Qnil,
		     intern ("x-hide-tip"));

  UNGCPRO;
  return unbind_to (count, Qnil);
}


DEFUN ("x-hide-tip", Fx_hide_tip, Sx_hide_tip, 0, 0, 0,
  "Hide the current tooltip window, if there is any.\n\
Value is t is tooltip was open, nil otherwise.")
  ()
{
  int count = specpdl_ptr - specpdl;
  int deleted_p = 0;
  
  specbind (Qinhibit_redisplay, Qt);
  
  if (!NILP (tip_timer))
    {
      call1 (intern ("cancel-timer"), tip_timer);
      tip_timer = Qnil;
    }

  if (tip_frame)
    {
      Lisp_Object frame;
      
      XSETFRAME (frame, tip_frame);
      Fdelete_frame (frame, Qt);
      tip_frame = NULL;
      deleted_p = 1;
    }

  return unbind_to (count, deleted_p ? Qt : Qnil);
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


DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 4, 0,
  "Read file name, prompting with PROMPT in directory DIR.\n\
Use a file selection dialog.\n\
Select DEFAULT-FILENAME in the dialog's file selection box, if\n\
specified.  Don't let the user enter a file name in the file\n\
selection dialog's entry field, if MUSTMATCH is non-nil.")
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
  char *title;
  XmString dir_xmstring, pattern_xmstring;
  int popup_activated_flag;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

  GCPRO5 (prompt, dir, default_filename, mustmatch, file);
  CHECK_STRING (prompt, 0);
  CHECK_STRING (dir, 1);

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

  /* Process all events until the user presses Cancel or OK.  */
  for (result = 0; result == 0;)
    {
      XEvent event;
      Widget widget, parent;
      
      XtAppNextEvent (Xt_app_con, &event);

      /* See if the receiver of the event is one of the widgets of
	 the file selection dialog.  If so, dispatch it.  If not,
	 discard it.  */
      widget = XtWindowToWidget (event.xany.display, event.xany.window);
      parent = widget;
      while (parent && parent != dialog)
	parent = XtParent (parent);
      
      if (parent == dialog
	  || (event.type == Expose
	      && !process_expose_from_menu (event)))
	XtDispatchEvent (&event);
    }

  /* Get the result.  */
  if (result == XmCR_OK)
    {
      XmString text;
      String data;
      
      XtVaGetValues (dialog, XmNtextString, &text, 0);
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
				Tests
 ***********************************************************************/

#if GLYPH_DEBUG

DEFUN ("imagep", Fimagep, Simagep, 1, 1, 0,
  "Value is non-nil if SPEC is a valid image specification.")
  (spec)
     Lisp_Object spec;
{
  return valid_image_p (spec) ? Qt : Qnil;
}


DEFUN ("lookup-image", Flookup_image, Slookup_image, 1, 1, 0, "")
  (spec)
     Lisp_Object spec;
{
  int id = -1;
  
  if (valid_image_p (spec))
    id = lookup_image (SELECTED_FRAME (), spec);

  debug_print (spec);
  return make_number (id);
}

#endif /* GLYPH_DEBUG != 0 */



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
  /* This is the end of symbol initialization.  */

  /* Text property `display' should be nonsticky by default.  */
  Vtext_property_default_nonsticky
    = Fcons (Fcons (Qdisplay, Qt), Vtext_property_default_nonsticky);


  Qlaplace = intern ("laplace");
  staticpro (&Qlaplace);
  
  Qface_set_after_frame_default = intern ("face-set-after-frame-default");
  staticpro (&Qface_set_after_frame_default);

  Fput (Qundefined_color, Qerror_conditions,
	Fcons (Qundefined_color, Fcons (Qerror, Qnil)));
  Fput (Qundefined_color, Qerror_message,
	build_string ("Undefined color"));

  init_x_parm_symbols ();

  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path,
    "List of directories to search for bitmap files for X.");
  Vx_bitmap_file_path = decode_env_path ((char *) 0, PATH_BITMAPS);

  DEFVAR_LISP ("x-pointer-shape", &Vx_pointer_shape,
    "The shape of the pointer when over text.\n\
Changing the value does not affect existing frames\n\
unless you set the mouse color.");
  Vx_pointer_shape = Qnil;

  DEFVAR_LISP ("x-resource-name", &Vx_resource_name,
    "The name Emacs uses to look up X resources.\n\
`x-get-resource' uses this as the first component of the instance name\n\
when requesting resource values.\n\
Emacs initially sets `x-resource-name' to the name under which Emacs\n\
was invoked, or to the value specified with the `-name' or `-rn'\n\
switches, if present.\n\
\n\
It may be useful to bind this variable locally around a call\n\
to `x-get-resource'.  See also the variable `x-resource-class'.");
  Vx_resource_name = Qnil;

  DEFVAR_LISP ("x-resource-class", &Vx_resource_class,
    "The class Emacs uses to look up X resources.\n\
`x-get-resource' uses this as the first component of the instance class\n\
when requesting resource values.\n\
Emacs initially sets `x-resource-class' to \"Emacs\".\n\
\n\
Setting this variable permanently is not a reasonable thing to do,\n\
but binding this variable locally around a call to `x-get-resource'\n\
is a reasonable practice.  See also the variable `x-resource-name'.");
  Vx_resource_class = build_string (EMACS_CLASS);

#if 0 /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-nontext-pointer-shape", &Vx_nontext_pointer_shape,
	      "The shape of the pointer when not over text.\n\
This variable takes effect when you create a new frame\n\
or when you set the mouse color.");
#endif
  Vx_nontext_pointer_shape = Qnil;

  DEFVAR_LISP ("x-busy-pointer-shape", &Vx_busy_pointer_shape,
    "The shape of the pointer when Emacs is busy.\n\
This variable takes effect when you create a new frame\n\
or when you set the mouse color.");
  Vx_busy_pointer_shape = Qnil;

  DEFVAR_BOOL ("display-busy-cursor", &display_busy_cursor_p,
    "Non-zero means Emacs displays a busy cursor on window systems.");
  display_busy_cursor_p = 1;
  
#if 0 /* This doesn't really do anything.  */
  DEFVAR_LISP ("x-mode-pointer-shape", &Vx_mode_pointer_shape,
	      "The shape of the pointer when over the mode line.\n\
This variable takes effect when you create a new frame\n\
or when you set the mouse color.");
#endif
  Vx_mode_pointer_shape = Qnil;

  DEFVAR_LISP ("x-sensitive-text-pointer-shape",
	      &Vx_sensitive_text_pointer_shape,
	      "The shape of the pointer when over mouse-sensitive text.\n\
This variable takes effect when you create a new frame\n\
or when you set the mouse color.");
  Vx_sensitive_text_pointer_shape = Qnil;

  DEFVAR_LISP ("x-cursor-fore-pixel", &Vx_cursor_fore_pixel,
	       "A string indicating the foreground color of the cursor box.");
  Vx_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("x-no-window-manager", &Vx_no_window_manager,
	       "Non-nil if no X window manager is in use.\n\
Emacs doesn't try to figure this out; this is always nil\n\
unless you set it to something else.");
  /* We don't have any way to find this out, so set it to nil
     and maybe the user would like to set it to t.  */
  Vx_no_window_manager = Qnil;

  DEFVAR_LISP ("x-pixel-size-width-font-regexp",
	       &Vx_pixel_size_width_font_regexp,
     "Regexp matching a font name whose width is the same as `PIXEL_SIZE'.\n\
\n\
Since Emacs gets width of a font matching with this regexp from\n\
PIXEL_SIZE field of the name, font finding mechanism gets faster for\n\
such a font.  This is especially effective for such large fonts as\n\
Chinese, Japanese, and Korean.");
  Vx_pixel_size_width_font_regexp = Qnil;

  DEFVAR_LISP ("image-cache-eviction-delay", &Vimage_cache_eviction_delay,
     "Time after which cached images are removed from the cache.\n\
When an image has not been displayed this many seconds, remove it\n\
from the image cache.  Value must be an integer or nil with nil\n\
meaning don't clear the cache.");
  Vimage_cache_eviction_delay = make_number (30 * 60);

  DEFVAR_LISP ("image-types", &Vimage_types,
     "List of supported image types.\n\
Each element of the list is a symbol for a supported image type.");
  Vimage_types = Qnil;

#ifdef USE_X_TOOLKIT
  Fprovide (intern ("x-toolkit"));
#endif
#ifdef USE_MOTIF
  Fprovide (intern ("motif"));
#endif

  defsubr (&Sx_get_resource);

  /* X window properties.  */
  defsubr (&Sx_change_window_property);
  defsubr (&Sx_delete_window_property);
  defsubr (&Sx_window_property);

#if 0
  defsubr (&Sx_draw_rectangle);
  defsubr (&Sx_erase_rectangle);
  defsubr (&Sx_contour_region);
  defsubr (&Sx_uncontour_region);
#endif
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
#if 0
  defsubr (&Sx_rebind_key);
  defsubr (&Sx_rebind_keys);
  defsubr (&Sx_track_pointer);
  defsubr (&Sx_grab_pointer);
  defsubr (&Sx_ungrab_pointer);
#endif
  defsubr (&Sx_parse_geometry);
  defsubr (&Sx_create_frame);
#if 0
  defsubr (&Sx_horizontal_line);
#endif
  defsubr (&Sx_open_connection);
  defsubr (&Sx_close_connection);
  defsubr (&Sx_display_list);
  defsubr (&Sx_synchronize);

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
  QCalgorithm = intern (":algorithm");
  staticpro (&QCalgorithm);
  QCheuristic_mask = intern (":heuristic-mask");
  staticpro (&QCheuristic_mask);
  QCcolor_symbols = intern (":color-symbols");
  staticpro (&QCcolor_symbols);
  QCdata = intern (":data");
  staticpro (&QCdata);
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

#if GLYPH_DEBUG
  defsubr (&Simagep);
  defsubr (&Slookup_image);
#endif

  /* Busy-cursor.  */
  defsubr (&Sx_show_busy_cursor);
  defsubr (&Sx_hide_busy_cursor);
  busy_count = 0;
  inhibit_busy_cursor = 0;

  defsubr (&Sx_show_tip);
  defsubr (&Sx_hide_tip);
  staticpro (&tip_timer);
  tip_timer = Qnil;

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
