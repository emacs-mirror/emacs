/* Graphical user interface functions for the Microsoft W32 API.
   Copyright (C) 1989, 92, 93, 94, 95, 1996 Free Software Foundation, Inc.

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

/* Added by Kevin Gallo */

#include <config.h>

#include <signal.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>

#include "lisp.h"
#include "charset.h"
#include "fontset.h"
#include "w32term.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "dispextern.h"
#include "keyboard.h"
#include "blockinput.h"
#include "epaths.h"
#include "w32heap.h"
#include "termhooks.h"
#include "coding.h"
#include "ccl.h"

#include <commdlg.h>
#include <shellapi.h>

extern void abort ();
extern void free_frame_menubar ();
extern struct scroll_bar *x_window_to_scroll_bar ();
extern int w32_console_toggle_lock_key (int vk_code, Lisp_Object new_state);
extern int quit_char;

extern char *lispy_function_keys[];

/* The colormap for converting color names to RGB values */
Lisp_Object Vw32_color_map;

/* Non nil if alt key presses are passed on to Windows.  */
Lisp_Object Vw32_pass_alt_to_system;

/* Non nil if alt key is translated to meta_modifier, nil if it is translated
   to alt_modifier.  */
Lisp_Object Vw32_alt_is_meta;

/* If non-zero, the windows virtual key code for an alternative quit key. */
Lisp_Object Vw32_quit_key;

/* Non nil if left window key events are passed on to Windows (this only
   affects whether "tapping" the key opens the Start menu).  */
Lisp_Object Vw32_pass_lwindow_to_system;

/* Non nil if right window key events are passed on to Windows (this
   only affects whether "tapping" the key opens the Start menu).  */
Lisp_Object Vw32_pass_rwindow_to_system;

/* Virtual key code used to generate "phantom" key presses in order
   to stop system from acting on Windows key events.  */
Lisp_Object Vw32_phantom_key_code;

/* Modifier associated with the left "Windows" key, or nil to act as a
   normal key.  */
Lisp_Object Vw32_lwindow_modifier;

/* Modifier associated with the right "Windows" key, or nil to act as a
   normal key.  */
Lisp_Object Vw32_rwindow_modifier;

/* Modifier associated with the "Apps" key, or nil to act as a normal
   key.  */
Lisp_Object Vw32_apps_modifier;

/* Value is nil if Num Lock acts as a function key.  */
Lisp_Object Vw32_enable_num_lock;

/* Value is nil if Caps Lock acts as a function key.  */
Lisp_Object Vw32_enable_caps_lock;

/* Modifier associated with Scroll Lock, or nil to act as a normal key.  */
Lisp_Object Vw32_scroll_lock_modifier;

/* Switch to control whether we inhibit requests for italicised fonts (which
   are synthesized, look ugly, and are trashed by cursor movement under NT). */
Lisp_Object Vw32_enable_italics;

/* Enable palette management. */
Lisp_Object Vw32_enable_palette;

/* Control how close left/right button down events must be to
   be converted to a middle button down event. */
Lisp_Object Vw32_mouse_button_tolerance;

/* Minimum interval between mouse movement (and scroll bar drag)
   events that are passed on to the event loop. */
Lisp_Object Vw32_mouse_move_interval;

/* The name we're using in resource queries.  */
Lisp_Object Vx_resource_name;

/* Non nil if no window manager is in use.  */
Lisp_Object Vx_no_window_manager;

/* The background and shape of the mouse pointer, and shape when not
   over text or in the modeline.  */
Lisp_Object Vx_pointer_shape, Vx_nontext_pointer_shape, Vx_mode_pointer_shape;
/* The shape when over mouse-sensitive text.  */
Lisp_Object Vx_sensitive_text_pointer_shape;

/* Color of chars displayed in cursor box.  */
Lisp_Object Vx_cursor_fore_pixel;

/* Nonzero if using Windows.  */
static int w32_in_use;

/* Search path for bitmap files.  */
Lisp_Object Vx_bitmap_file_path;

/* Regexp matching a font name whose width is the same as `PIXEL_SIZE'.  */
Lisp_Object Vx_pixel_size_width_font_regexp;

/* Alist of bdf fonts and the files that define them.  */
Lisp_Object Vw32_bdf_filename_alist;

Lisp_Object Vw32_system_coding_system;

/* A flag to control whether fonts are matched strictly or not.  */
int w32_strict_fontnames;

/* A flag to control whether we should only repaint if GetUpdateRect
   indicates there is an update region.  */
int w32_strict_painting;

/* Evaluate this expression to rebuild the section of syms_of_w32fns
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
Lisp_Object Qforeground_color;
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
Lisp_Object Qparent_id;
Lisp_Object Qscroll_bar_width;
Lisp_Object Qsuppress_icon;
Lisp_Object Qtop;
Lisp_Object Qundefined_color;
Lisp_Object Qvertical_scroll_bars;
Lisp_Object Qvisibility;
Lisp_Object Qwindow_id;
Lisp_Object Qx_frame_parameter;
Lisp_Object Qx_resource_name;
Lisp_Object Quser_position;
Lisp_Object Quser_size;
Lisp_Object Qdisplay;

Lisp_Object Qhyper;
Lisp_Object Qsuper;
Lisp_Object Qmeta;
Lisp_Object Qalt;
Lisp_Object Qctrl;
Lisp_Object Qcontrol;
Lisp_Object Qshift;

/* State variables for emulating a three button mouse. */
#define LMOUSE 1
#define MMOUSE 2
#define RMOUSE 4

static int button_state = 0;
static W32Msg saved_mouse_button_msg;
static unsigned mouse_button_timer;	/* non-zero when timer is active */
static W32Msg saved_mouse_move_msg;
static unsigned mouse_move_timer;

/* W95 mousewheel handler */
unsigned int msh_mousewheel = 0;	

#define MOUSE_BUTTON_ID	1
#define MOUSE_MOVE_ID	2

/* The below are defined in frame.c.  */
extern Lisp_Object Qheight, Qminibuffer, Qname, Qonly, Qwidth;
extern Lisp_Object Qunsplittable, Qmenu_bar_lines, Qbuffer_predicate, Qtitle;

extern Lisp_Object Vwindow_system_version;

Lisp_Object Qface_set_after_frame_default;

extern Lisp_Object last_mouse_scroll_bar;
extern int last_mouse_scroll_bar_pos;

/* From w32term.c. */
extern Lisp_Object Vw32_num_mouse_buttons;
extern Lisp_Object Vw32_recognize_altgr;


/* Error if we are not connected to MS-Windows.  */
void
check_w32 ()
{
  if (! w32_in_use)
    error ("MS-Windows not in use or not initialized");
}

/* Nonzero if we can use mouse menus.
   You should not call this unless HAVE_MENUS is defined.  */
  
int
have_menus_p ()
{
  return w32_in_use;
}

/* Extract a frame as a FRAME_PTR, defaulting to the selected frame
   and checking validity for W32.  */

FRAME_PTR
check_x_frame (frame)
     Lisp_Object frame;
{
  FRAME_PTR f;

  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
    }
  if (! FRAME_W32_P (f))
    error ("non-w32 frame used");
  return f;
}

/* Let the user specify an display with a frame.
   nil stands for the selected frame--or, if that is not a w32 frame,
   the first display on the list.  */

static struct w32_display_info *
check_x_display_info (frame)
     Lisp_Object frame;
{
  if (NILP (frame))
    {
      if (FRAME_W32_P (selected_frame))
	return FRAME_W32_DISPLAY_INFO (selected_frame);
      else
	return &one_w32_display_info;
    }
  else if (STRINGP (frame))
    return x_display_info_for_name (frame);
  else
    {
      FRAME_PTR f;

      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
      if (! FRAME_W32_P (f))
	error ("non-w32 frame used");
      return FRAME_W32_DISPLAY_INFO (f);
    }
}

/* Return the Emacs frame-object corresponding to an w32 window.
   It could be the frame's main window or an icon window.  */

/* This function can be called during GC, so use GC_xxx type test macros.  */

struct frame *
x_window_to_frame (dpyinfo, wdesc)
     struct w32_display_info *dpyinfo;
     HWND wdesc;
{
  Lisp_Object tail, frame;
  struct frame *f;

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCONS (tail)->cdr)
    {
      frame = XCONS (tail)->car;
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (f->output_data.nothing == 1 
	  || FRAME_W32_DISPLAY_INFO (f) != dpyinfo)
	continue;
      if (FRAME_W32_WINDOW (f) == wdesc)
        return f;
    }
  return 0;
}



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
  return FRAME_W32_DISPLAY_INFO (f)->bitmaps[id - 1].height;
}

int
x_bitmap_width (f, id)
     FRAME_PTR f;
     int id;
{
  return FRAME_W32_DISPLAY_INFO (f)->bitmaps[id - 1].width;
}

int
x_bitmap_pixmap (f, id)
     FRAME_PTR f;
     int id;
{
  return (int) FRAME_W32_DISPLAY_INFO (f)->bitmaps[id - 1].pixmap;
}


/* Allocate a new bitmap record.  Returns index of new record.  */

static int
x_allocate_bitmap_record (f)
     FRAME_PTR f;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  int i;

  if (dpyinfo->bitmaps == NULL)
    {
      dpyinfo->bitmaps_size = 10;
      dpyinfo->bitmaps
	= (struct w32_bitmap_record *) xmalloc (dpyinfo->bitmaps_size * sizeof (struct w32_bitmap_record));
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
    = (struct w32_bitmap_record *) xrealloc (dpyinfo->bitmaps,
					   dpyinfo->bitmaps_size * sizeof (struct w32_bitmap_record));
  return ++dpyinfo->bitmaps_last;
}

/* Add one reference to the reference count of the bitmap with id ID.  */

void
x_reference_bitmap (f, id)
     FRAME_PTR f;
     int id;
{
  ++FRAME_W32_DISPLAY_INFO (f)->bitmaps[id - 1].refcount;
}

/* Create a bitmap for frame F from a HEIGHT x WIDTH array of bits at BITS.  */

int
x_create_bitmap_from_data (f, bits, width, height)
     struct frame *f;
     char *bits;
     unsigned int width, height;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  Pixmap bitmap;
  int id;

  bitmap = CreateBitmap (width, height,
			 FRAME_W32_DISPLAY_INFO (XFRAME (frame))->n_planes,
			 FRAME_W32_DISPLAY_INFO (XFRAME (frame))->n_cbits,
			 bits);

  if (! bitmap)
    return -1;

  id = x_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].file = NULL;
  dpyinfo->bitmaps[id - 1].hinst = NULL;
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
  return -1;
#if 0
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  unsigned int width, height;
  Pixmap bitmap;
  int xhot, yhot, result, id;
  Lisp_Object found;
  int fd;
  char *filename;
  HINSTANCE hinst;

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
  /* LoadLibraryEx won't handle special files handled by Emacs handler.  */
  if (fd == 0)
    return -1;
  close (fd);

  filename = (char *) XSTRING (found)->data;

  hinst = LoadLibraryEx (filename, NULL, LOAD_LIBRARY_AS_DATAFILE);

  if (hinst == NULL)
      return -1;

  
  result = XReadBitmapFile (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
			    filename, &width, &height, &bitmap, &xhot, &yhot);
  if (result != BitmapSuccess)
    return -1;

  id = x_allocate_bitmap_record (f);
  dpyinfo->bitmaps[id - 1].pixmap = bitmap;
  dpyinfo->bitmaps[id - 1].refcount = 1;
  dpyinfo->bitmaps[id - 1].file = (char *) xmalloc (XSTRING (file)->size + 1);
  dpyinfo->bitmaps[id - 1].depth = 1;
  dpyinfo->bitmaps[id - 1].height = height;
  dpyinfo->bitmaps[id - 1].width = width;
  strcpy (dpyinfo->bitmaps[id - 1].file, XSTRING (file)->data);

  return id;
#endif
}

/* Remove reference to bitmap with id number ID.  */

void
x_destroy_bitmap (f, id)
     FRAME_PTR f;
     int id;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  if (id > 0)
    {
      --dpyinfo->bitmaps[id - 1].refcount;
      if (dpyinfo->bitmaps[id - 1].refcount == 0)
	{
	  BLOCK_INPUT;
	  DeleteObject (dpyinfo->bitmaps[id - 1].pixmap);
	  if (dpyinfo->bitmaps[id - 1].file)
	    {
	      free (dpyinfo->bitmaps[id - 1].file);
	      dpyinfo->bitmaps[id - 1].file = NULL;
	    }
	  UNBLOCK_INPUT;
	}
    }
}

/* Free all the bitmaps for the display specified by DPYINFO.  */

static void
x_destroy_all_bitmaps (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  int i;
  for (i = 0; i < dpyinfo->bitmaps_last; i++)
    if (dpyinfo->bitmaps[i].refcount > 0)
      {
	DeleteObject (dpyinfo->bitmaps[i].pixmap);
	if (dpyinfo->bitmaps[i].file)
	  free (dpyinfo->bitmaps[i].file);
      }
  dpyinfo->bitmaps_last = 0;
}

/* Connect the frame-parameter names for W32 frames
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
void x_set_icon_name ();
void x_set_font ();
void x_set_border_width ();
void x_set_internal_border_width ();
void x_explicitly_set_name ();
void x_set_autoraise ();
void x_set_autolower ();
void x_set_vertical_scroll_bars ();
void x_set_visibility ();
void x_set_menu_bar_lines ();
void x_set_scroll_bar_width ();
void x_set_title ();
void x_set_unsplittable ();

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
};

/* Attach the `x-frame-parameter' properties to
   the Lisp symbol names of parameters relevant to W32.  */

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
      Lisp_Object elt, prop, val;

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
  width = FRAME_WIDTH (f);
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
      if (f->output_data.w32->left_pos < 0)
	left = Fcons (Qplus, Fcons (make_number (f->output_data.w32->left_pos), Qnil));
      else
	XSETINT (left, f->output_data.w32->left_pos);
    }
  if (EQ (top, Qunbound))
    {
      top_no_change = 1;
      if (f->output_data.w32->top_pos < 0)
	top = Fcons (Qplus, Fcons (make_number (f->output_data.w32->top_pos), Qnil));
      else
	XSETINT (top, f->output_data.w32->top_pos);
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

    if (XINT (width) != FRAME_WIDTH (f)
	|| XINT (height) != FRAME_HEIGHT (f))
      Fset_frame_size (frame, make_number (width), make_number (height));

    if ((!NILP (left) || !NILP (top))
	&& ! (left_no_change && top_no_change)
	&& ! (NUMBERP (left) && XINT (left) == f->output_data.w32->left_pos
	      && NUMBERP (top) && XINT (top) == f->output_data.w32->top_pos))
      {
	int leftpos = 0;
	int toppos = 0;

	/* Record the signs.  */
	f->output_data.w32->size_hint_flags &= ~ (XNegative | YNegative);
	if (EQ (left, Qminus))
	  f->output_data.w32->size_hint_flags |= XNegative;
	else if (INTEGERP (left))
	  {
	    leftpos = XINT (left);
	    if (leftpos < 0)
	      f->output_data.w32->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCONS (left)->car, Qminus)
		 && CONSP (XCONS (left)->cdr)
		 && INTEGERP (XCONS (XCONS (left)->cdr)->car))
	  {
	    leftpos = - XINT (XCONS (XCONS (left)->cdr)->car);
	    f->output_data.w32->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCONS (left)->car, Qplus)
		 && CONSP (XCONS (left)->cdr)
		 && INTEGERP (XCONS (XCONS (left)->cdr)->car))
	  {
	    leftpos = XINT (XCONS (XCONS (left)->cdr)->car);
	  }

	if (EQ (top, Qminus))
	  f->output_data.w32->size_hint_flags |= YNegative;
	else if (INTEGERP (top))
	  {
	    toppos = XINT (top);
	    if (toppos < 0)
	      f->output_data.w32->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCONS (top)->car, Qminus)
		 && CONSP (XCONS (top)->cdr)
		 && INTEGERP (XCONS (XCONS (top)->cdr)->car))
	  {
	    toppos = - XINT (XCONS (XCONS (top)->cdr)->car);
	    f->output_data.w32->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCONS (top)->car, Qplus)
		 && CONSP (XCONS (top)->cdr)
		 && INTEGERP (XCONS (XCONS (top)->cdr)->car))
	  {
	    toppos = XINT (XCONS (XCONS (top)->cdr)->car);
	  }


	/* Store the numeric value of the position.  */
	f->output_data.w32->top_pos = toppos;
	f->output_data.w32->left_pos = leftpos;

	f->output_data.w32->win_gravity = NorthWestGravity;

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
  POINT pt;

  {
      RECT rect;
      
      GetClientRect(FRAME_W32_WINDOW(f), &rect);
      AdjustWindowRect(&rect, f->output_data.w32->dwStyle, FRAME_EXTERNAL_MENU_BAR(f));
      
      pt.x = rect.left;
      pt.y = rect.top;
  }

  ClientToScreen (FRAME_W32_WINDOW(f), &pt);

  *xptr = pt.x;
  *yptr = pt.y;
}

/* Insert a description of internally-recorded parameters of frame X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to W32
   and whose values are not correctly recorded in the frame's
   param_alist need to be considered here.  */

x_report_frame_params (f, alistptr)
     struct frame *f;
     Lisp_Object *alistptr;
{
  char buf[16];
  Lisp_Object tem;

  /* Represent negative positions (off the top or left screen edge)
     in a way that Fmodify_frame_parameters will understand correctly.  */
  XSETINT (tem, f->output_data.w32->left_pos);
  if (f->output_data.w32->left_pos >= 0)
    store_in_alist (alistptr, Qleft, tem);
  else
    store_in_alist (alistptr, Qleft, Fcons (Qplus, Fcons (tem, Qnil)));

  XSETINT (tem, f->output_data.w32->top_pos);
  if (f->output_data.w32->top_pos >= 0)
    store_in_alist (alistptr, Qtop, tem);
  else
    store_in_alist (alistptr, Qtop, Fcons (Qplus, Fcons (tem, Qnil)));

  store_in_alist (alistptr, Qborder_width,
       	   make_number (f->output_data.w32->border_width));
  store_in_alist (alistptr, Qinternal_border_width,
       	   make_number (f->output_data.w32->internal_border_width));
  sprintf (buf, "%ld", (long) FRAME_W32_WINDOW (f));
  store_in_alist (alistptr, Qwindow_id,
       	   build_string (buf));
  store_in_alist (alistptr, Qicon_name, f->icon_name);
  FRAME_SAMPLE_VISIBILITY (f);
  store_in_alist (alistptr, Qvisibility,
		  (FRAME_VISIBLE_P (f) ? Qt
		   : FRAME_ICONIFIED_P (f) ? Qicon : Qnil));
  store_in_alist (alistptr, Qdisplay,
		  XCONS (FRAME_W32_DISPLAY_INFO (f)->name_list_element)->car);
}


DEFUN ("w32-define-rgb-color", Fw32_define_rgb_color, Sw32_define_rgb_color, 4, 4, 0,
  "Convert RGB numbers to a windows color reference and associate with NAME (a string).\n\
This adds or updates a named color to w32-color-map, making it available for use.\n\
The original entry's RGB ref is returned, or nil if the entry is new.")
    (red, green, blue, name)
    Lisp_Object red, green, blue, name;
{
  Lisp_Object rgb;
  Lisp_Object oldrgb = Qnil;
  Lisp_Object entry;

  CHECK_NUMBER (red, 0);
  CHECK_NUMBER (green, 0);
  CHECK_NUMBER (blue, 0);
  CHECK_STRING (name, 0);

  XSET (rgb, Lisp_Int, RGB(XUINT (red), XUINT (green), XUINT (blue)));

  BLOCK_INPUT;

  /* replace existing entry in w32-color-map or add new entry. */
  entry = Fassoc (name, Vw32_color_map);
  if (NILP (entry))
    {
      entry = Fcons (name, rgb);
      Vw32_color_map = Fcons (entry, Vw32_color_map);
    }
  else
    {
      oldrgb = Fcdr (entry);
      Fsetcdr (entry, rgb);
    }

  UNBLOCK_INPUT;

  return (oldrgb);
}

DEFUN ("w32-load-color-file", Fw32_load_color_file, Sw32_load_color_file, 1, 1, 0,
  "Create an alist of color entries from an external file (ie. rgb.txt).\n\
Assign this value to w32-color-map to replace the existing color map.\n\
\
The file should define one named RGB color per line like so:\
  R G B   name\n\
where R,G,B are numbers between 0 and 255 and name is an arbitrary string.")
    (filename)
    Lisp_Object filename;
{
  FILE *fp;
  Lisp_Object cmap = Qnil;
  Lisp_Object abspath;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);

  fp = fopen (XSTRING (filename)->data, "rt");
  if (fp)
    {
      char buf[512];
      int red, green, blue;
      int num;

      BLOCK_INPUT;

      while (fgets (buf, sizeof (buf), fp) != NULL) {
	if (sscanf (buf, "%u %u %u %n", &red, &green, &blue, &num) == 3)
	  {
	    char *name = buf + num;
	    num = strlen (name) - 1;
	    if (name[num] == '\n')
	      name[num] = 0;
	    cmap = Fcons (Fcons (build_string (name),
				 make_number (RGB (red, green, blue))),
			  cmap);
	  }
      }
      fclose (fp);

      UNBLOCK_INPUT;
    }

  return cmap;
}

/* The default colors for the w32 color map */
typedef struct colormap_t 
{
  char *name;
  COLORREF colorref;
} colormap_t;

colormap_t w32_color_map[] = 
{
  {"snow"                      , PALETTERGB (255,250,250)},
  {"ghost white"               , PALETTERGB (248,248,255)},
  {"GhostWhite"                , PALETTERGB (248,248,255)},
  {"white smoke"               , PALETTERGB (245,245,245)},
  {"WhiteSmoke"                , PALETTERGB (245,245,245)},
  {"gainsboro"                 , PALETTERGB (220,220,220)},
  {"floral white"              , PALETTERGB (255,250,240)},
  {"FloralWhite"               , PALETTERGB (255,250,240)},
  {"old lace"                  , PALETTERGB (253,245,230)},
  {"OldLace"                   , PALETTERGB (253,245,230)},
  {"linen"                     , PALETTERGB (250,240,230)},
  {"antique white"             , PALETTERGB (250,235,215)},
  {"AntiqueWhite"              , PALETTERGB (250,235,215)},
  {"papaya whip"               , PALETTERGB (255,239,213)},
  {"PapayaWhip"                , PALETTERGB (255,239,213)},
  {"blanched almond"           , PALETTERGB (255,235,205)},
  {"BlanchedAlmond"            , PALETTERGB (255,235,205)},
  {"bisque"                    , PALETTERGB (255,228,196)},
  {"peach puff"                , PALETTERGB (255,218,185)},
  {"PeachPuff"                 , PALETTERGB (255,218,185)},
  {"navajo white"              , PALETTERGB (255,222,173)},
  {"NavajoWhite"               , PALETTERGB (255,222,173)},
  {"moccasin"                  , PALETTERGB (255,228,181)},
  {"cornsilk"                  , PALETTERGB (255,248,220)},
  {"ivory"                     , PALETTERGB (255,255,240)},
  {"lemon chiffon"             , PALETTERGB (255,250,205)},
  {"LemonChiffon"              , PALETTERGB (255,250,205)},
  {"seashell"                  , PALETTERGB (255,245,238)},
  {"honeydew"                  , PALETTERGB (240,255,240)},
  {"mint cream"                , PALETTERGB (245,255,250)},
  {"MintCream"                 , PALETTERGB (245,255,250)},
  {"azure"                     , PALETTERGB (240,255,255)},
  {"alice blue"                , PALETTERGB (240,248,255)},
  {"AliceBlue"                 , PALETTERGB (240,248,255)},
  {"lavender"                  , PALETTERGB (230,230,250)},
  {"lavender blush"            , PALETTERGB (255,240,245)},
  {"LavenderBlush"             , PALETTERGB (255,240,245)},
  {"misty rose"                , PALETTERGB (255,228,225)},
  {"MistyRose"                 , PALETTERGB (255,228,225)},
  {"white"                     , PALETTERGB (255,255,255)},
  {"black"                     , PALETTERGB (  0,  0,  0)},
  {"dark slate gray"           , PALETTERGB ( 47, 79, 79)},
  {"DarkSlateGray"             , PALETTERGB ( 47, 79, 79)},
  {"dark slate grey"           , PALETTERGB ( 47, 79, 79)},
  {"DarkSlateGrey"             , PALETTERGB ( 47, 79, 79)},
  {"dim gray"                  , PALETTERGB (105,105,105)},
  {"DimGray"                   , PALETTERGB (105,105,105)},
  {"dim grey"                  , PALETTERGB (105,105,105)},
  {"DimGrey"                   , PALETTERGB (105,105,105)},
  {"slate gray"                , PALETTERGB (112,128,144)},
  {"SlateGray"                 , PALETTERGB (112,128,144)},
  {"slate grey"                , PALETTERGB (112,128,144)},
  {"SlateGrey"                 , PALETTERGB (112,128,144)},
  {"light slate gray"          , PALETTERGB (119,136,153)},
  {"LightSlateGray"            , PALETTERGB (119,136,153)},
  {"light slate grey"          , PALETTERGB (119,136,153)},
  {"LightSlateGrey"            , PALETTERGB (119,136,153)},
  {"gray"                      , PALETTERGB (190,190,190)},
  {"grey"                      , PALETTERGB (190,190,190)},
  {"light grey"                , PALETTERGB (211,211,211)},
  {"LightGrey"                 , PALETTERGB (211,211,211)},
  {"light gray"                , PALETTERGB (211,211,211)},
  {"LightGray"                 , PALETTERGB (211,211,211)},
  {"midnight blue"             , PALETTERGB ( 25, 25,112)},
  {"MidnightBlue"              , PALETTERGB ( 25, 25,112)},
  {"navy"                      , PALETTERGB (  0,  0,128)},
  {"navy blue"                 , PALETTERGB (  0,  0,128)},
  {"NavyBlue"                  , PALETTERGB (  0,  0,128)},
  {"cornflower blue"           , PALETTERGB (100,149,237)},
  {"CornflowerBlue"            , PALETTERGB (100,149,237)},
  {"dark slate blue"           , PALETTERGB ( 72, 61,139)},
  {"DarkSlateBlue"             , PALETTERGB ( 72, 61,139)},
  {"slate blue"                , PALETTERGB (106, 90,205)},
  {"SlateBlue"                 , PALETTERGB (106, 90,205)},
  {"medium slate blue"         , PALETTERGB (123,104,238)},
  {"MediumSlateBlue"           , PALETTERGB (123,104,238)},
  {"light slate blue"          , PALETTERGB (132,112,255)},
  {"LightSlateBlue"            , PALETTERGB (132,112,255)},
  {"medium blue"               , PALETTERGB (  0,  0,205)},
  {"MediumBlue"                , PALETTERGB (  0,  0,205)},
  {"royal blue"                , PALETTERGB ( 65,105,225)},
  {"RoyalBlue"                 , PALETTERGB ( 65,105,225)},
  {"blue"                      , PALETTERGB (  0,  0,255)},
  {"dodger blue"               , PALETTERGB ( 30,144,255)},
  {"DodgerBlue"                , PALETTERGB ( 30,144,255)},
  {"deep sky blue"             , PALETTERGB (  0,191,255)},
  {"DeepSkyBlue"               , PALETTERGB (  0,191,255)},
  {"sky blue"                  , PALETTERGB (135,206,235)},
  {"SkyBlue"                   , PALETTERGB (135,206,235)},
  {"light sky blue"            , PALETTERGB (135,206,250)},
  {"LightSkyBlue"              , PALETTERGB (135,206,250)},
  {"steel blue"                , PALETTERGB ( 70,130,180)},
  {"SteelBlue"                 , PALETTERGB ( 70,130,180)},
  {"light steel blue"          , PALETTERGB (176,196,222)},
  {"LightSteelBlue"            , PALETTERGB (176,196,222)},
  {"light blue"                , PALETTERGB (173,216,230)},
  {"LightBlue"                 , PALETTERGB (173,216,230)},
  {"powder blue"               , PALETTERGB (176,224,230)},
  {"PowderBlue"                , PALETTERGB (176,224,230)},
  {"pale turquoise"            , PALETTERGB (175,238,238)},
  {"PaleTurquoise"             , PALETTERGB (175,238,238)},
  {"dark turquoise"            , PALETTERGB (  0,206,209)},
  {"DarkTurquoise"             , PALETTERGB (  0,206,209)},
  {"medium turquoise"          , PALETTERGB ( 72,209,204)},
  {"MediumTurquoise"           , PALETTERGB ( 72,209,204)},
  {"turquoise"                 , PALETTERGB ( 64,224,208)},
  {"cyan"                      , PALETTERGB (  0,255,255)},
  {"light cyan"                , PALETTERGB (224,255,255)},
  {"LightCyan"                 , PALETTERGB (224,255,255)},
  {"cadet blue"                , PALETTERGB ( 95,158,160)},
  {"CadetBlue"                 , PALETTERGB ( 95,158,160)},
  {"medium aquamarine"         , PALETTERGB (102,205,170)},
  {"MediumAquamarine"          , PALETTERGB (102,205,170)},
  {"aquamarine"                , PALETTERGB (127,255,212)},
  {"dark green"                , PALETTERGB (  0,100,  0)},
  {"DarkGreen"                 , PALETTERGB (  0,100,  0)},
  {"dark olive green"          , PALETTERGB ( 85,107, 47)},
  {"DarkOliveGreen"            , PALETTERGB ( 85,107, 47)},
  {"dark sea green"            , PALETTERGB (143,188,143)},
  {"DarkSeaGreen"              , PALETTERGB (143,188,143)},
  {"sea green"                 , PALETTERGB ( 46,139, 87)},
  {"SeaGreen"                  , PALETTERGB ( 46,139, 87)},
  {"medium sea green"          , PALETTERGB ( 60,179,113)},
  {"MediumSeaGreen"            , PALETTERGB ( 60,179,113)},
  {"light sea green"           , PALETTERGB ( 32,178,170)},
  {"LightSeaGreen"             , PALETTERGB ( 32,178,170)},
  {"pale green"                , PALETTERGB (152,251,152)},
  {"PaleGreen"                 , PALETTERGB (152,251,152)},
  {"spring green"              , PALETTERGB (  0,255,127)},
  {"SpringGreen"               , PALETTERGB (  0,255,127)},
  {"lawn green"                , PALETTERGB (124,252,  0)},
  {"LawnGreen"                 , PALETTERGB (124,252,  0)},
  {"green"                     , PALETTERGB (  0,255,  0)},
  {"chartreuse"                , PALETTERGB (127,255,  0)},
  {"medium spring green"       , PALETTERGB (  0,250,154)},
  {"MediumSpringGreen"         , PALETTERGB (  0,250,154)},
  {"green yellow"              , PALETTERGB (173,255, 47)},
  {"GreenYellow"               , PALETTERGB (173,255, 47)},
  {"lime green"                , PALETTERGB ( 50,205, 50)},
  {"LimeGreen"                 , PALETTERGB ( 50,205, 50)},
  {"yellow green"              , PALETTERGB (154,205, 50)},
  {"YellowGreen"               , PALETTERGB (154,205, 50)},
  {"forest green"              , PALETTERGB ( 34,139, 34)},
  {"ForestGreen"               , PALETTERGB ( 34,139, 34)},
  {"olive drab"                , PALETTERGB (107,142, 35)},
  {"OliveDrab"                 , PALETTERGB (107,142, 35)},
  {"dark khaki"                , PALETTERGB (189,183,107)},
  {"DarkKhaki"                 , PALETTERGB (189,183,107)},
  {"khaki"                     , PALETTERGB (240,230,140)},
  {"pale goldenrod"            , PALETTERGB (238,232,170)},
  {"PaleGoldenrod"             , PALETTERGB (238,232,170)},
  {"light goldenrod yellow"    , PALETTERGB (250,250,210)},
  {"LightGoldenrodYellow"      , PALETTERGB (250,250,210)},
  {"light yellow"              , PALETTERGB (255,255,224)},
  {"LightYellow"               , PALETTERGB (255,255,224)},
  {"yellow"                    , PALETTERGB (255,255,  0)},
  {"gold"                      , PALETTERGB (255,215,  0)},
  {"light goldenrod"           , PALETTERGB (238,221,130)},
  {"LightGoldenrod"            , PALETTERGB (238,221,130)},
  {"goldenrod"                 , PALETTERGB (218,165, 32)},
  {"dark goldenrod"            , PALETTERGB (184,134, 11)},
  {"DarkGoldenrod"             , PALETTERGB (184,134, 11)},
  {"rosy brown"                , PALETTERGB (188,143,143)},
  {"RosyBrown"                 , PALETTERGB (188,143,143)},
  {"indian red"                , PALETTERGB (205, 92, 92)},
  {"IndianRed"                 , PALETTERGB (205, 92, 92)},
  {"saddle brown"              , PALETTERGB (139, 69, 19)},
  {"SaddleBrown"               , PALETTERGB (139, 69, 19)},
  {"sienna"                    , PALETTERGB (160, 82, 45)},
  {"peru"                      , PALETTERGB (205,133, 63)},
  {"burlywood"                 , PALETTERGB (222,184,135)},
  {"beige"                     , PALETTERGB (245,245,220)},
  {"wheat"                     , PALETTERGB (245,222,179)},
  {"sandy brown"               , PALETTERGB (244,164, 96)},
  {"SandyBrown"                , PALETTERGB (244,164, 96)},
  {"tan"                       , PALETTERGB (210,180,140)},
  {"chocolate"                 , PALETTERGB (210,105, 30)},
  {"firebrick"                 , PALETTERGB (178,34, 34)},
  {"brown"                     , PALETTERGB (165,42, 42)},
  {"dark salmon"               , PALETTERGB (233,150,122)},
  {"DarkSalmon"                , PALETTERGB (233,150,122)},
  {"salmon"                    , PALETTERGB (250,128,114)},
  {"light salmon"              , PALETTERGB (255,160,122)},
  {"LightSalmon"               , PALETTERGB (255,160,122)},
  {"orange"                    , PALETTERGB (255,165,  0)},
  {"dark orange"               , PALETTERGB (255,140,  0)},
  {"DarkOrange"                , PALETTERGB (255,140,  0)},
  {"coral"                     , PALETTERGB (255,127, 80)},
  {"light coral"               , PALETTERGB (240,128,128)},
  {"LightCoral"                , PALETTERGB (240,128,128)},
  {"tomato"                    , PALETTERGB (255, 99, 71)},
  {"orange red"                , PALETTERGB (255, 69,  0)},
  {"OrangeRed"                 , PALETTERGB (255, 69,  0)},
  {"red"                       , PALETTERGB (255,  0,  0)},
  {"hot pink"                  , PALETTERGB (255,105,180)},
  {"HotPink"                   , PALETTERGB (255,105,180)},
  {"deep pink"                 , PALETTERGB (255, 20,147)},
  {"DeepPink"                  , PALETTERGB (255, 20,147)},
  {"pink"                      , PALETTERGB (255,192,203)},
  {"light pink"                , PALETTERGB (255,182,193)},
  {"LightPink"                 , PALETTERGB (255,182,193)},
  {"pale violet red"           , PALETTERGB (219,112,147)},
  {"PaleVioletRed"             , PALETTERGB (219,112,147)},
  {"maroon"                    , PALETTERGB (176, 48, 96)},
  {"medium violet red"         , PALETTERGB (199, 21,133)},
  {"MediumVioletRed"           , PALETTERGB (199, 21,133)},
  {"violet red"                , PALETTERGB (208, 32,144)},
  {"VioletRed"                 , PALETTERGB (208, 32,144)},
  {"magenta"                   , PALETTERGB (255,  0,255)},
  {"violet"                    , PALETTERGB (238,130,238)},
  {"plum"                      , PALETTERGB (221,160,221)},
  {"orchid"                    , PALETTERGB (218,112,214)},
  {"medium orchid"             , PALETTERGB (186, 85,211)},
  {"MediumOrchid"              , PALETTERGB (186, 85,211)},
  {"dark orchid"               , PALETTERGB (153, 50,204)},
  {"DarkOrchid"                , PALETTERGB (153, 50,204)},
  {"dark violet"               , PALETTERGB (148,  0,211)},
  {"DarkViolet"                , PALETTERGB (148,  0,211)},
  {"blue violet"               , PALETTERGB (138, 43,226)},
  {"BlueViolet"                , PALETTERGB (138, 43,226)},
  {"purple"                    , PALETTERGB (160, 32,240)},
  {"medium purple"             , PALETTERGB (147,112,219)},
  {"MediumPurple"              , PALETTERGB (147,112,219)},
  {"thistle"                   , PALETTERGB (216,191,216)},
  {"gray0"                     , PALETTERGB (  0,  0,  0)},
  {"grey0"                     , PALETTERGB (  0,  0,  0)},
  {"dark grey"                 , PALETTERGB (169,169,169)},
  {"DarkGrey"                  , PALETTERGB (169,169,169)},
  {"dark gray"                 , PALETTERGB (169,169,169)},
  {"DarkGray"                  , PALETTERGB (169,169,169)},
  {"dark blue"                 , PALETTERGB (  0,  0,139)},
  {"DarkBlue"                  , PALETTERGB (  0,  0,139)},
  {"dark cyan"                 , PALETTERGB (  0,139,139)},
  {"DarkCyan"                  , PALETTERGB (  0,139,139)},
  {"dark magenta"              , PALETTERGB (139,  0,139)},
  {"DarkMagenta"               , PALETTERGB (139,  0,139)},
  {"dark red"                  , PALETTERGB (139,  0,  0)},
  {"DarkRed"                   , PALETTERGB (139,  0,  0)},
  {"light green"               , PALETTERGB (144,238,144)},
  {"LightGreen"                , PALETTERGB (144,238,144)},
};

DEFUN ("w32-default-color-map", Fw32_default_color_map, Sw32_default_color_map,
       0, 0, 0, "Return the default color map.")
     ()
{
  int i;
  colormap_t *pc = w32_color_map;
  Lisp_Object cmap;
  
  BLOCK_INPUT;
  
  cmap = Qnil;
  
  for (i = 0; i < sizeof (w32_color_map) / sizeof (w32_color_map[0]); 
       pc++, i++)
    cmap = Fcons (Fcons (build_string (pc->name),
			 make_number (pc->colorref)),
		  cmap);
  
  UNBLOCK_INPUT;
  
  return (cmap);
}

Lisp_Object 
w32_to_x_color (rgb)
     Lisp_Object rgb;
{
  Lisp_Object color;
  
  CHECK_NUMBER (rgb, 0);
  
  BLOCK_INPUT;
  
  color = Frassq (rgb, Vw32_color_map);
  
  UNBLOCK_INPUT;
  
  if (!NILP (color))
    return (Fcar (color));
  else
    return Qnil;
}

COLORREF
w32_color_map_lookup (colorname)
     char *colorname;
{
  Lisp_Object tail, ret = Qnil;

  BLOCK_INPUT;

  for (tail = Vw32_color_map; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;

      elt = Fcar (tail);
      if (!CONSP (elt)) continue;

      tem = Fcar (elt);

      if (lstrcmpi (XSTRING (tem)->data, colorname) == 0)
	{
	  ret = XUINT (Fcdr (elt));
	  break;
	}

      QUIT;
    }


  UNBLOCK_INPUT;

  return ret;
}

COLORREF 
x_to_w32_color (colorname)
     char * colorname;
{
  register Lisp_Object tail, ret = Qnil;
  
  BLOCK_INPUT;

  if (colorname[0] == '#')
    {
      /* Could be an old-style RGB Device specification.  */
      char *color;
      int size;
      color = colorname + 1;
      
      size = strlen(color);
      if (size == 3 || size == 6 || size == 9 || size == 12)
	{
	  UINT colorval;
	  int i, pos;
	  pos = 0;
	  size /= 3;
	  colorval = 0;
	  
	  for (i = 0; i < 3; i++)
	    {
	      char *end;
	      char t;
	      unsigned long value;

	      /* The check for 'x' in the following conditional takes into
		 account the fact that strtol allows a "0x" in front of
		 our numbers, and we don't.  */
	      if (!isxdigit(color[0]) || color[1] == 'x')
		break;
	      t = color[size];
	      color[size] = '\0';
	      value = strtoul(color, &end, 16);
	      color[size] = t;
	      if (errno == ERANGE || end - color != size)
		break;
	      switch (size)
		{
		case 1:
		  value = value * 0x10;
		  break;
		case 2:
		  break;
		case 3:
		  value /= 0x10;
		  break;
		case 4:
		  value /= 0x100;
		  break;
		}
	      colorval |= (value << pos);
	      pos += 0x8;
	      if (i == 2)
		{
		  UNBLOCK_INPUT;
		  return (colorval);
		}
	      color = end;
	    }
	}
    }
  else if (strnicmp(colorname, "rgb:", 4) == 0)
    {
      char *color;
      UINT colorval;
      int i, pos;
      pos = 0;

      colorval = 0;
      color = colorname + 4;
      for (i = 0; i < 3; i++)
	{
	  char *end;
	  unsigned long value;
	  
	  /* The check for 'x' in the following conditional takes into
	     account the fact that strtol allows a "0x" in front of
	     our numbers, and we don't.  */
	  if (!isxdigit(color[0]) || color[1] == 'x')
	    break;
	  value = strtoul(color, &end, 16);
	  if (errno == ERANGE)
	    break;
	  switch (end - color)
	    {
	    case 1:
	      value = value * 0x10 + value;
	      break;
	    case 2:
	      break;
	    case 3:
	      value /= 0x10;
	      break;
	    case 4:
	      value /= 0x100;
	      break;
	    default:
	      value = ULONG_MAX;
	    }
	  if (value == ULONG_MAX)
	    break;
	  colorval |= (value << pos);
	  pos += 0x8;
	  if (i == 2)
	    {
	      if (*end != '\0')
		break;
	      UNBLOCK_INPUT;
	      return (colorval);
	    }
	  if (*end != '/')
	    break;
	  color = end + 1;
	}
    }
  else if (strnicmp(colorname, "rgbi:", 5) == 0)
    {
      /* This is an RGB Intensity specification.  */
      char *color;
      UINT colorval;
      int i, pos;
      pos = 0;

      colorval = 0;
      color = colorname + 5;
      for (i = 0; i < 3; i++)
	{
	  char *end;
	  double value;
	  UINT val;

	  value = strtod(color, &end);
	  if (errno == ERANGE)
	    break;
	  if (value < 0.0 || value > 1.0)
	    break;
	  val = (UINT)(0x100 * value);
	  /* We used 0x100 instead of 0xFF to give an continuous
             range between 0.0 and 1.0 inclusive.  The next statement
             fixes the 1.0 case.  */
	  if (val == 0x100)
	    val = 0xFF;
	  colorval |= (val << pos);
	  pos += 0x8;
	  if (i == 2)
	    {
	      if (*end != '\0')
		break;
	      UNBLOCK_INPUT;
	      return (colorval);
	    }
	  if (*end != '/')
	    break;
	  color = end + 1;
	}
    }
  /* I am not going to attempt to handle any of the CIE color schemes
     or TekHVC, since I don't know the algorithms for conversion to
     RGB.  */

  /* If we fail to lookup the color name in w32_color_map, then check the
     colorname to see if it can be crudely approximated: If the X color 
     ends in a number (e.g., "darkseagreen2"), strip the number and
     return the result of looking up the base color name.  */
  ret = w32_color_map_lookup (colorname);
  if (NILP (ret)) 
    {
      int len = strlen (colorname);

      if (isdigit (colorname[len - 1])) 
	{
	  char *ptr, *approx = alloca (len);

	  strcpy (approx, colorname);
	  ptr = &approx[len - 1];
	  while (ptr > approx && isdigit (*ptr)) 
	      *ptr-- = '\0';

	  ret = w32_color_map_lookup (approx);
	}
    }
  
  UNBLOCK_INPUT;
  return ret;
}


void
w32_regenerate_palette (FRAME_PTR f)
{
  struct w32_palette_entry * list;
  LOGPALETTE *          log_palette;
  HPALETTE              new_palette;
  int                   i;

  /* don't bother trying to create palette if not supported */
  if (! FRAME_W32_DISPLAY_INFO (f)->has_palette)
    return;

  log_palette = (LOGPALETTE *)
    alloca (sizeof (LOGPALETTE) +
	     FRAME_W32_DISPLAY_INFO (f)->num_colors * sizeof (PALETTEENTRY));
  log_palette->palVersion = 0x300;
  log_palette->palNumEntries = FRAME_W32_DISPLAY_INFO (f)->num_colors;

  list = FRAME_W32_DISPLAY_INFO (f)->color_list;
  for (i = 0;
       i < FRAME_W32_DISPLAY_INFO (f)->num_colors;
       i++, list = list->next)
    log_palette->palPalEntry[i] = list->entry;

  new_palette = CreatePalette (log_palette);

  enter_crit ();

  if (FRAME_W32_DISPLAY_INFO (f)->palette)
    DeleteObject (FRAME_W32_DISPLAY_INFO (f)->palette);
  FRAME_W32_DISPLAY_INFO (f)->palette = new_palette;

  /* Realize display palette and garbage all frames. */
  release_frame_dc (f, get_frame_dc (f));

  leave_crit ();
}

#define W32_COLOR(pe)  RGB (pe.peRed, pe.peGreen, pe.peBlue)
#define SET_W32_COLOR(pe, color) \
  do \
    { \
      pe.peRed = GetRValue (color); \
      pe.peGreen = GetGValue (color); \
      pe.peBlue = GetBValue (color); \
      pe.peFlags = 0; \
    } while (0)

#if 0
/* Keep these around in case we ever want to track color usage. */
void
w32_map_color (FRAME_PTR f, COLORREF color)
{
  struct w32_palette_entry * list = FRAME_W32_DISPLAY_INFO (f)->color_list;

  if (NILP (Vw32_enable_palette))
    return;

  /* check if color is already mapped */
  while (list)
    {
      if (W32_COLOR (list->entry) == color)
        {
	  ++list->refcount;
	  return;
	}
      list = list->next;
    }

  /* not already mapped, so add to list and recreate Windows palette */
  list = (struct w32_palette_entry *)
    xmalloc (sizeof (struct w32_palette_entry));
  SET_W32_COLOR (list->entry, color);
  list->refcount = 1;
  list->next = FRAME_W32_DISPLAY_INFO (f)->color_list;
  FRAME_W32_DISPLAY_INFO (f)->color_list = list;
  FRAME_W32_DISPLAY_INFO (f)->num_colors++;

  /* set flag that palette must be regenerated */
  FRAME_W32_DISPLAY_INFO (f)->regen_palette = TRUE;
}

void
w32_unmap_color (FRAME_PTR f, COLORREF color)
{
  struct w32_palette_entry * list = FRAME_W32_DISPLAY_INFO (f)->color_list;
  struct w32_palette_entry **prev = &FRAME_W32_DISPLAY_INFO (f)->color_list;

  if (NILP (Vw32_enable_palette))
    return;

  /* check if color is already mapped */
  while (list)
    {
      if (W32_COLOR (list->entry) == color)
        {
	  if (--list->refcount == 0)
	    {
	      *prev = list->next;
	      xfree (list);
	      FRAME_W32_DISPLAY_INFO (f)->num_colors--;
	      break;
	    }
	  else
	    return;
	}
      prev = &list->next;
      list = list->next;
    }

  /* set flag that palette must be regenerated */
  FRAME_W32_DISPLAY_INFO (f)->regen_palette = TRUE;
}
#endif

/* Decide if color named COLOR is valid for the display associated with
   the selected frame; if so, return the rgb values in COLOR_DEF.
   If ALLOC is nonzero, allocate a new colormap cell.  */

int
defined_color (f, color, color_def, alloc)
     FRAME_PTR f;
     char *color;
     COLORREF *color_def;
     int alloc;
{
  register Lisp_Object tem;

  tem = x_to_w32_color (color);

  if (!NILP (tem)) 
    {
      if (!NILP (Vw32_enable_palette))
	{
	  struct w32_palette_entry * entry =
	    FRAME_W32_DISPLAY_INFO (f)->color_list;
	  struct w32_palette_entry ** prev =
	    &FRAME_W32_DISPLAY_INFO (f)->color_list;
      
	  /* check if color is already mapped */
	  while (entry)
	    {
	      if (W32_COLOR (entry->entry) == XUINT (tem))
		break;
	      prev = &entry->next;
	      entry = entry->next;
	    }

	  if (entry == NULL && alloc)
	    {
	      /* not already mapped, so add to list */
	      entry = (struct w32_palette_entry *)
		xmalloc (sizeof (struct w32_palette_entry));
	      SET_W32_COLOR (entry->entry, XUINT (tem));
	      entry->next = NULL;
	      *prev = entry;
	      FRAME_W32_DISPLAY_INFO (f)->num_colors++;

	      /* set flag that palette must be regenerated */
	      FRAME_W32_DISPLAY_INFO (f)->regen_palette = TRUE;
	    }
	}
      /* Ensure COLORREF value is snapped to nearest color in (default)
	 palette by simulating the PALETTERGB macro.  This works whether
	 or not the display device has a palette. */
      *color_def = XUINT (tem) | 0x2000000;
      return 1;
    }
  else 
    {
      return 0;
    }
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
  COLORREF cdef;

  CHECK_STRING (arg, 0);

  if (strcmp (XSTRING (arg)->data, "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (XSTRING (arg)->data, "white") == 0)
    return WHITE_PIX_DEFAULT (f);

  if ((FRAME_W32_DISPLAY_INFO (f)->n_planes * FRAME_W32_DISPLAY_INFO (f)->n_cbits) == 1)
    return def;

  /* defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (defined_color (f, XSTRING (arg)->data, &cdef, 1))
    return cdef;

  /* defined_color failed; return an ultimate default.  */
  return def;
}

/* Functions called only from `x_set_frame_param'
   to set individual parameters.

   If FRAME_W32_WINDOW (f) is 0,
   the frame is being created and its window does not exist yet.
   In that case, just record the parameter's new value
   in the standard place; do not attempt to change the window.  */

void
x_set_foreground_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  f->output_data.w32->foreground_pixel
    = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));

  if (FRAME_W32_WINDOW (f) != 0)
    {
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

  f->output_data.w32->background_pixel
    = x_decode_color (f, arg, WHITE_PIX_DEFAULT (f));

  if (FRAME_W32_WINDOW (f) != 0)
    {
      SetWindowLong (FRAME_W32_WINDOW (f), WND_BACKGROUND_INDEX, f->output_data.w32->background_pixel);

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
#if 0
  Cursor cursor, nontext_cursor, mode_cursor, cross_cursor;
#endif
  int count;
  int mask_color;

  if (!EQ (Qnil, arg))
    f->output_data.w32->mouse_pixel
      = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  mask_color = f->output_data.w32->background_pixel;
				/* No invisible pointers.  */
  if (mask_color == f->output_data.w32->mouse_pixel
	&& mask_color == f->output_data.w32->background_pixel)
    f->output_data.w32->mouse_pixel = f->output_data.w32->foreground_pixel;

#if 0
  BLOCK_INPUT;

  /* It's not okay to crash if the user selects a screwy cursor.  */
  count = x_catch_errors (FRAME_W32_DISPLAY (f));

  if (!EQ (Qnil, Vx_pointer_shape))
    {
      CHECK_NUMBER (Vx_pointer_shape, 0);
      cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XINT (Vx_pointer_shape));
    }
  else
    cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_xterm);
  x_check_errors (FRAME_W32_DISPLAY (f), "bad text pointer cursor: %s");

  if (!EQ (Qnil, Vx_nontext_pointer_shape))
    {
      CHECK_NUMBER (Vx_nontext_pointer_shape, 0);
      nontext_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f),
					  XINT (Vx_nontext_pointer_shape));
    }
  else
    nontext_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_left_ptr);
  x_check_errors (FRAME_W32_DISPLAY (f), "bad nontext pointer cursor: %s");

  if (!EQ (Qnil, Vx_mode_pointer_shape))
    {
      CHECK_NUMBER (Vx_mode_pointer_shape, 0);
      mode_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f),
				       XINT (Vx_mode_pointer_shape));
    }
  else
    mode_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_xterm);
  x_check_errors (FRAME_W32_DISPLAY (f), "bad modeline pointer cursor: %s");

  if (!EQ (Qnil, Vx_sensitive_text_pointer_shape))
    {
      CHECK_NUMBER (Vx_sensitive_text_pointer_shape, 0);
      cross_cursor
	= XCreateFontCursor (FRAME_W32_DISPLAY (f),
			     XINT (Vx_sensitive_text_pointer_shape));
    }
  else
    cross_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_crosshair);

  /* Check and report errors with the above calls.  */
  x_check_errors (FRAME_W32_DISPLAY (f), "can't set cursor shape: %s");
  x_uncatch_errors (FRAME_W32_DISPLAY (f), count);

  {
    XColor fore_color, back_color;

    fore_color.pixel = f->output_data.w32->mouse_pixel;
    back_color.pixel = mask_color;
    XQueryColor (FRAME_W32_DISPLAY (f),
		 DefaultColormap (FRAME_W32_DISPLAY (f),
				  DefaultScreen (FRAME_W32_DISPLAY (f))),
		 &fore_color);
    XQueryColor (FRAME_W32_DISPLAY (f),
		 DefaultColormap (FRAME_W32_DISPLAY (f),
				  DefaultScreen (FRAME_W32_DISPLAY (f))),
		 &back_color);
    XRecolorCursor (FRAME_W32_DISPLAY (f), cursor,
		    &fore_color, &back_color);
    XRecolorCursor (FRAME_W32_DISPLAY (f), nontext_cursor,
		    &fore_color, &back_color);
    XRecolorCursor (FRAME_W32_DISPLAY (f), mode_cursor,
		    &fore_color, &back_color);
    XRecolorCursor (FRAME_W32_DISPLAY (f), cross_cursor,
                    &fore_color, &back_color);
  }

  if (FRAME_W32_WINDOW (f) != 0)
    {
      XDefineCursor (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f), cursor);
    }

  if (cursor != f->output_data.w32->text_cursor && f->output_data.w32->text_cursor != 0)
    XFreeCursor (FRAME_W32_DISPLAY (f), f->output_data.w32->text_cursor);
  f->output_data.w32->text_cursor = cursor;

  if (nontext_cursor != f->output_data.w32->nontext_cursor
      && f->output_data.w32->nontext_cursor != 0)
    XFreeCursor (FRAME_W32_DISPLAY (f), f->output_data.w32->nontext_cursor);
  f->output_data.w32->nontext_cursor = nontext_cursor;

  if (mode_cursor != f->output_data.w32->modeline_cursor
      && f->output_data.w32->modeline_cursor != 0)
    XFreeCursor (FRAME_W32_DISPLAY (f), f->output_data.w32->modeline_cursor);
  f->output_data.w32->modeline_cursor = mode_cursor;
  if (cross_cursor != f->output_data.w32->cross_cursor
      && f->output_data.w32->cross_cursor != 0)
    XFreeCursor (FRAME_W32_DISPLAY (f), f->output_data.w32->cross_cursor);
  f->output_data.w32->cross_cursor = cross_cursor;

  XFlush (FRAME_W32_DISPLAY (f));
  UNBLOCK_INPUT;
#endif
}

void
x_set_cursor_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned long fore_pixel;

  if (!EQ (Vx_cursor_fore_pixel, Qnil))
    fore_pixel = x_decode_color (f, Vx_cursor_fore_pixel,
				 WHITE_PIX_DEFAULT (f));
  else
    fore_pixel = f->output_data.w32->background_pixel;
  f->output_data.w32->cursor_pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  
  /* Make sure that the cursor color differs from the background color.  */
  if (f->output_data.w32->cursor_pixel == f->output_data.w32->background_pixel)
    {
      f->output_data.w32->cursor_pixel = f->output_data.w32->mouse_pixel;
      if (f->output_data.w32->cursor_pixel == fore_pixel)
	fore_pixel = f->output_data.w32->background_pixel;
    }
  f->output_data.w32->cursor_foreground_pixel = fore_pixel;

  if (FRAME_W32_WINDOW (f) != 0)
    {
      if (FRAME_VISIBLE_P (f))
	{
	  x_display_cursor (f, 0);
	  x_display_cursor (f, 1);
	}
    }
}

/* Set the border-color of frame F to pixel value PIX.
   Note that this does not fully take effect if done before
   F has an window.  */
void
x_set_border_pixel (f, pix)
     struct frame *f;
     int pix;
{
  f->output_data.w32->border_pixel = pix;

  if (FRAME_W32_WINDOW (f) != 0 && f->output_data.w32->border_width > 0)
    {
      if (FRAME_VISIBLE_P (f))
        redraw_frame (f);
    }
}

/* Set the border-color of frame F to value described by ARG.
   ARG can be a string naming a color.
   The border-color is used for the border that is drawn by the server.
   Note that this does not fully take effect if done before
   F has a window; it must be redone when the window is created.  */

void
x_set_border_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned char *str;
  int pix;

  CHECK_STRING (arg, 0);
  str = XSTRING (arg)->data;

  pix = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));

  x_set_border_pixel (f, pix);
}

void
x_set_cursor_type (f, arg, oldval)
     FRAME_PTR f;
     Lisp_Object arg, oldval;
{
  if (EQ (arg, Qbar))
    {
      FRAME_DESIRED_CURSOR (f) = bar_cursor;
      f->output_data.w32->cursor_width = 2;
    }
  else if (CONSP (arg) && EQ (XCONS (arg)->car, Qbar)
	   && INTEGERP (XCONS (arg)->cdr))
    {
      FRAME_DESIRED_CURSOR (f) = bar_cursor;
      f->output_data.w32->cursor_width = XINT (XCONS (arg)->cdr);
    }
  else
    /* Treat anything unknown as "box cursor".
       It was bad to signal an error; people have trouble fixing
       .Xdefaults with Emacs, when it has something bad in it.  */
    FRAME_DESIRED_CURSOR (f) = filled_box_cursor;

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

  if (NILP (arg) && NILP (oldval))
    return;

  if (STRINGP (arg) && STRINGP (oldval) 
      && EQ (Fstring_equal (oldval, arg), Qt))
    return;

  if (SYMBOLP (arg) && SYMBOLP (oldval) && EQ (arg, oldval))
    return;

  BLOCK_INPUT;

  result = x_bitmap_icon (f, arg);
  if (result)
    {
      UNBLOCK_INPUT;
      error ("No icon window available");
    }

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
    return XCONS (tem)->cdr;
  else
    return Qnil;
}

void
x_set_icon_name (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  Lisp_Object tem;
  int result;

  if (STRINGP (arg))
    {
      if (STRINGP (oldval) && EQ (Fstring_equal (oldval, arg), Qt))
	return;
    }
  else if (!STRINGP (oldval) && EQ (oldval, Qnil) == EQ (arg, Qnil))
    return;

  f->icon_name = arg;

#if 0
  if (f->output_data.w32->icon_bitmap != 0)
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

  /* If the window was unmapped (and its icon was mapped),
     the new icon is not mapped, so map the window in its stead.  */
  if (FRAME_VISIBLE_P (f))
    {
#ifdef USE_X_TOOLKIT
      XtPopup (f->output_data.w32->widget, XtGrabNone);
#endif
      XMapWindow (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f));
    }

  XFlush (FRAME_W32_DISPLAY (f));
  UNBLOCK_INPUT;
#endif
}

extern Lisp_Object x_new_font ();
extern Lisp_Object x_new_fontset();

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

  XSETFRAME (frame, f);
  call1 (Qface_set_after_frame_default, frame);
}

void
x_set_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  CHECK_NUMBER (arg, 0);

  if (XINT (arg) == f->output_data.w32->border_width)
    return;

  if (FRAME_W32_WINDOW (f) != 0)
    error ("Cannot change the border width of a window");

  f->output_data.w32->border_width = XINT (arg);
}

void
x_set_internal_border_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int mask;
  int old = f->output_data.w32->internal_border_width;

  CHECK_NUMBER (arg, 0);
  f->output_data.w32->internal_border_width = XINT (arg);
  if (f->output_data.w32->internal_border_width < 0)
    f->output_data.w32->internal_border_width = 0;

  if (f->output_data.w32->internal_border_width == old)
    return;

  if (FRAME_W32_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      x_set_window_size (f, 0, f->width, f->height);
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
  XSETFRAME (frame, f);

  if (NILP (value))
    Fmake_frame_invisible (frame, Qt);
  else if (EQ (value, Qicon))
    Ficonify_frame (frame);
  else
    Fmake_frame_visible (frame);
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

  if (INTEGERP (value))
    nlines = XINT (value);
  else
    nlines = 0;

  FRAME_MENU_BAR_LINES (f) = 0;
  if (nlines)
    FRAME_EXTERNAL_MENU_BAR (f) = 1;
  else
    {
      if (FRAME_EXTERNAL_MENU_BAR (f) == 1)
	free_frame_menubar (f);
      FRAME_EXTERNAL_MENU_BAR (f) = 0;

      /* Adjust the frame size so that the client (text) dimensions
	 remain the same.  This depends on FRAME_EXTERNAL_MENU_BAR being
	 set correctly.  */
      x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
    }
}

/* Change the name of frame F to NAME.  If NAME is nil, set F's name to
       w32_id_name.

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

  /* If NAME is nil, set the name to the w32_id_name.  */
  if (NILP (name))
    {
      /* Check for no change needed in this very common case
	 before we do any consing.  */
      if (!strcmp (FRAME_W32_DISPLAY_INFO (f)->w32_id_name,
		   XSTRING (f->name)->data))
	return;
      name = build_string (FRAME_W32_DISPLAY_INFO (f)->w32_id_name);
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

  if (FRAME_W32_WINDOW (f))
    {
      BLOCK_INPUT;
      SetWindowText(FRAME_W32_WINDOW (f), XSTRING (name)->data);
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
x_set_title (f, name)
     struct frame *f;
     Lisp_Object name;
{
  /* Don't change the title if it's already NAME.  */
  if (EQ (name, f->title))
    return;

  update_mode_lines = 1;

  f->title = name;

  if (NILP (name))
    name = f->name;

  if (FRAME_W32_WINDOW (f))
    {
      BLOCK_INPUT;
      SetWindowText(FRAME_W32_WINDOW (f), XSTRING (name)->data);
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
      FRAME_VERTICAL_SCROLL_BAR_TYPE (f) = NILP (arg) ?
	vertical_scroll_bar_none :
	/* Put scroll bars on the right by default, as is conventional
           on MS-Windows.  */
	EQ (Qleft, arg)
	? vertical_scroll_bar_left 
	: vertical_scroll_bar_right;

      /* We set this parameter before creating the window for the
	 frame, so we can get the geometry right from the start.
	 However, if the window hasn't been created yet, we shouldn't
	 call x_set_window_size.  */
      if (FRAME_W32_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
    }
}

void
x_set_scroll_bar_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  if (NILP (arg))
    {
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = 0;
      FRAME_SCROLL_BAR_COLS (f) = 2;
    }
  else if (INTEGERP (arg) && XINT (arg) > 0
	   && XFASTINT (arg) != FRAME_SCROLL_BAR_PIXEL_WIDTH (f))
    {
      int wid = FONT_WIDTH (f->output_data.w32->font);
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = XFASTINT (arg);
      FRAME_SCROLL_BAR_COLS (f) = (XFASTINT (arg) + wid-1) / wid;
      if (FRAME_W32_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
    }
}

/* Subroutines of creating an frame.  */

/* Make sure that Vx_resource_name is set to a reasonable value.
   Fix it up, or set it to `emacs' if it is too hopeless.  */

static void
validate_x_resource_name ()
{
  int len;
  /* Number of valid characters in the resource name.  */
  int good_count = 0;
  /* Number of invalid characters in the resource name.  */
  int bad_count = 0;
  Lisp_Object new;
  int i;

  if (STRINGP (Vx_resource_name))
    {
      unsigned char *p = XSTRING (Vx_resource_name)->data;
      int i;

      len = XSTRING (Vx_resource_name)->size;

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
  name_key = (char *) alloca (XSTRING (Vx_resource_name)->size
			      + (STRINGP (component)
				 ? XSTRING (component)->size : 0)
			      + XSTRING (attribute)->size
			      + 3);

  class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
			       + XSTRING (class)->size
			       + (STRINGP (subclass)
				  ? XSTRING (subclass)->size : 0)
			       + 3);

  /* Start with emacs.FRAMENAME for the name (the specific one)
     and with `Emacs' for the class key (the general one).  */
  strcpy (name_key, XSTRING (Vx_resource_name)->data);
  strcpy (class_key, EMACS_CLASS);

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

  value = x_get_string_resource (Qnil,
				 name_key, class_key);

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

  return x_get_string_resource (selected_frame,
				name_key, class_key);
}

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
   and don't let it get stored in any Lisp-visible variables!  */

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
  int height, width, left, top;
  register int geometry;
  long window_prompting = 0;

  /* Default values if we fall through.
     Actually, if that happens we should get
     window manager prompting.  */
  SET_FRAME_WIDTH (f, DEFAULT_COLS);
  f->height = DEFAULT_ROWS;
  /* Window managers expect that if program-specified
     positions are not (0,0), they're intentional, not defaults.  */
  f->output_data.w32->top_pos = 0;
  f->output_data.w32->left_pos = 0;

  tem0 = x_get_arg (parms, Qheight, 0, 0, number);
  tem1 = x_get_arg (parms, Qwidth, 0, 0, number);
  tem2 = x_get_arg (parms, Quser_size, 0, 0, number);
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

  f->output_data.w32->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.w32->font)));
  f->output_data.w32->pixel_width = CHAR_TO_PIXEL_WIDTH (f, f->width);
  f->output_data.w32->pixel_height = CHAR_TO_PIXEL_HEIGHT (f, f->height);

  tem0 = x_get_arg (parms, Qtop, 0, 0, number);
  tem1 = x_get_arg (parms, Qleft, 0, 0, number);
  tem2 = x_get_arg (parms, Quser_position, 0, 0, number);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (EQ (tem0, Qminus))
	{
	  f->output_data.w32->top_pos = 0;
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCONS (tem0)->car, Qminus)
	       && CONSP (XCONS (tem0)->cdr)
	       && INTEGERP (XCONS (XCONS (tem0)->cdr)->car))
	{
	  f->output_data.w32->top_pos = - XINT (XCONS (XCONS (tem0)->cdr)->car);
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCONS (tem0)->car, Qplus)
	       && CONSP (XCONS (tem0)->cdr)
	       && INTEGERP (XCONS (XCONS (tem0)->cdr)->car))
	{
	  f->output_data.w32->top_pos = XINT (XCONS (XCONS (tem0)->cdr)->car);
	}
      else if (EQ (tem0, Qunbound))
	f->output_data.w32->top_pos = 0;
      else
	{
	  CHECK_NUMBER (tem0, 0);
	  f->output_data.w32->top_pos = XINT (tem0);
	  if (f->output_data.w32->top_pos < 0)
	    window_prompting |= YNegative;
	}

      if (EQ (tem1, Qminus))
	{
	  f->output_data.w32->left_pos = 0;
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCONS (tem1)->car, Qminus)
	       && CONSP (XCONS (tem1)->cdr)
	       && INTEGERP (XCONS (XCONS (tem1)->cdr)->car))
	{
	  f->output_data.w32->left_pos = - XINT (XCONS (XCONS (tem1)->cdr)->car);
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCONS (tem1)->car, Qplus)
	       && CONSP (XCONS (tem1)->cdr)
	       && INTEGERP (XCONS (XCONS (tem1)->cdr)->car))
	{
	  f->output_data.w32->left_pos = XINT (XCONS (XCONS (tem1)->cdr)->car);
	}
      else if (EQ (tem1, Qunbound))
	f->output_data.w32->left_pos = 0;
      else
	{
	  CHECK_NUMBER (tem1, 0);
	  f->output_data.w32->left_pos = XINT (tem1);
	  if (f->output_data.w32->left_pos < 0)
	    window_prompting |= XNegative;
	}

      if (!NILP (tem2) && ! EQ (tem2, Qunbound))
	window_prompting |= USPosition;
      else
	window_prompting |= PPosition;
    }

  return window_prompting;
}



extern LRESULT CALLBACK w32_wnd_proc ();

BOOL 
w32_init_class (hinst)
     HINSTANCE hinst;
{
  WNDCLASS wc;

  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = (WNDPROC) w32_wnd_proc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = WND_EXTRA_BYTES;
  wc.hInstance = hinst;
  wc.hIcon = LoadIcon (hinst, EMACS_CLASS);
  wc.hCursor = LoadCursor (NULL, IDC_ARROW);
  wc.hbrBackground = NULL; /* GetStockObject (WHITE_BRUSH);  */
  wc.lpszMenuName = NULL;
  wc.lpszClassName = EMACS_CLASS;

  return (RegisterClass (&wc));
}

HWND 
w32_createscrollbar (f, bar)
     struct frame *f;
     struct scroll_bar * bar;
{
  return (CreateWindow ("SCROLLBAR", "", SBS_VERT | WS_CHILD | WS_VISIBLE,
			/* Position and size of scroll bar.  */
			XINT(bar->left), XINT(bar->top), 
			XINT(bar->width), XINT(bar->height),
			FRAME_W32_WINDOW (f),
			NULL,
			hinst,
			NULL));
}

void 
w32_createwindow (f)
     struct frame *f;
{
  HWND hwnd;
  RECT rect;

  rect.left = rect.top = 0;
  rect.right = PIXEL_WIDTH (f);
  rect.bottom = PIXEL_HEIGHT (f);
      
  AdjustWindowRect (&rect, f->output_data.w32->dwStyle,
		    FRAME_EXTERNAL_MENU_BAR (f));
  
  /* Do first time app init */
  
  if (!hprevinst)
    {
      w32_init_class (hinst);
    }
  
  FRAME_W32_WINDOW (f) = hwnd
    = CreateWindow (EMACS_CLASS,
		    f->namebuf,
		    f->output_data.w32->dwStyle | WS_CLIPCHILDREN,
		    f->output_data.w32->left_pos,
		    f->output_data.w32->top_pos,
		    rect.right - rect.left,
		    rect.bottom - rect.top,
		    NULL,
		    NULL,
		    hinst,
		    NULL);

  if (hwnd)
    {
      SetWindowLong (hwnd, WND_FONTWIDTH_INDEX, FONT_WIDTH (f->output_data.w32->font));
      SetWindowLong (hwnd, WND_LINEHEIGHT_INDEX, f->output_data.w32->line_height);
      SetWindowLong (hwnd, WND_BORDER_INDEX, f->output_data.w32->internal_border_width);
      SetWindowLong (hwnd, WND_SCROLLBAR_INDEX, f->output_data.w32->vertical_scroll_bar_extra);
      SetWindowLong (hwnd, WND_BACKGROUND_INDEX, f->output_data.w32->background_pixel);

      /* Enable drag-n-drop.  */
      DragAcceptFiles (hwnd, TRUE);
      
      /* Do this to discard the default setting specified by our parent. */
      ShowWindow (hwnd, SW_HIDE);
    }
}

void 
my_post_msg (wmsg, hwnd, msg, wParam, lParam)
     W32Msg * wmsg;
     HWND hwnd;
     UINT msg;
     WPARAM wParam;
     LPARAM lParam;
{
  wmsg->msg.hwnd = hwnd;
  wmsg->msg.message = msg;
  wmsg->msg.wParam = wParam;
  wmsg->msg.lParam = lParam;
  wmsg->msg.time = GetMessageTime ();

  post_msg (wmsg);
}

/* GetKeyState and MapVirtualKey on Windows 95 do not actually distinguish
   between left and right keys as advertised.  We test for this
   support dynamically, and set a flag when the support is absent.  If
   absent, we keep track of the left and right control and alt keys
   ourselves.  This is particularly necessary on keyboards that rely
   upon the AltGr key, which is represented as having the left control
   and right alt keys pressed.  For these keyboards, we need to know
   when the left alt key has been pressed in addition to the AltGr key
   so that we can properly support M-AltGr-key sequences (such as M-@
   on Swedish keyboards).  */

#define EMACS_LCONTROL 0
#define EMACS_RCONTROL 1
#define EMACS_LMENU    2
#define EMACS_RMENU    3

static int modifiers[4];
static int modifiers_recorded;
static int modifier_key_support_tested;

static void
test_modifier_support (unsigned int wparam)
{
  unsigned int l, r;

  if (wparam != VK_CONTROL && wparam != VK_MENU)
    return;
  if (wparam == VK_CONTROL)
    {
      l = VK_LCONTROL;
      r = VK_RCONTROL;
    }
  else
    {
      l = VK_LMENU;
      r = VK_RMENU;
    }
  if (!(GetKeyState (l) & 0x8000) && !(GetKeyState (r) & 0x8000))
    modifiers_recorded = 1;
  else
    modifiers_recorded = 0;
  modifier_key_support_tested = 1;
}

static void
record_keydown (unsigned int wparam, unsigned int lparam)
{
  int i;

  if (!modifier_key_support_tested)
    test_modifier_support (wparam);

  if ((wparam != VK_CONTROL && wparam != VK_MENU) || !modifiers_recorded)
    return;

  if (wparam == VK_CONTROL)
    i = (lparam & 0x1000000) ? EMACS_RCONTROL : EMACS_LCONTROL;
  else
    i = (lparam & 0x1000000) ? EMACS_RMENU : EMACS_LMENU;

  modifiers[i] = 1;
}

static void
record_keyup (unsigned int wparam, unsigned int lparam)
{
  int i;

  if ((wparam != VK_CONTROL && wparam != VK_MENU) || !modifiers_recorded)
    return;

  if (wparam == VK_CONTROL)
    i = (lparam & 0x1000000) ? EMACS_RCONTROL : EMACS_LCONTROL;
  else
    i = (lparam & 0x1000000) ? EMACS_RMENU : EMACS_LMENU;

  modifiers[i] = 0;
}

/* Emacs can lose focus while a modifier key has been pressed.  When
   it regains focus, be conservative and clear all modifiers since 
   we cannot reconstruct the left and right modifier state.  */
static void
reset_modifiers ()
{
  SHORT ctrl, alt;

  if (GetFocus () == NULL)
    /* Emacs doesn't have keyboard focus.  Do nothing.  */
    return;

  ctrl = GetAsyncKeyState (VK_CONTROL);
  alt = GetAsyncKeyState (VK_MENU);

  if (!(ctrl & 0x08000))
    /* Clear any recorded control modifier state.  */
    modifiers[EMACS_RCONTROL] = modifiers[EMACS_LCONTROL] = 0;

  if (!(alt & 0x08000))
    /* Clear any recorded alt modifier state.  */
    modifiers[EMACS_RMENU] = modifiers[EMACS_LMENU] = 0;

  /* Update the state of all modifier keys, because modifiers used in
     hot-key combinations can get stuck on if Emacs loses focus as a
     result of a hot-key being pressed.  */
  {
    BYTE keystate[256];

#define CURRENT_STATE(key) ((GetAsyncKeyState (key) & 0x8000) >> 8)

    GetKeyboardState (keystate);
    keystate[VK_SHIFT] = CURRENT_STATE (VK_SHIFT);
    keystate[VK_CONTROL] = CURRENT_STATE (VK_CONTROL);
    keystate[VK_LCONTROL] = CURRENT_STATE (VK_LCONTROL);
    keystate[VK_RCONTROL] = CURRENT_STATE (VK_RCONTROL);
    keystate[VK_MENU] = CURRENT_STATE (VK_MENU);
    keystate[VK_LMENU] = CURRENT_STATE (VK_LMENU);
    keystate[VK_RMENU] = CURRENT_STATE (VK_RMENU);
    keystate[VK_LWIN] = CURRENT_STATE (VK_LWIN);
    keystate[VK_RWIN] = CURRENT_STATE (VK_RWIN);
    keystate[VK_APPS] = CURRENT_STATE (VK_APPS);
    SetKeyboardState (keystate);
  }
}

/* Synchronize modifier state with what is reported with the current
   keystroke.  Even if we cannot distinguish between left and right
   modifier keys, we know that, if no modifiers are set, then neither
   the left or right modifier should be set.  */
static void
sync_modifiers ()
{
  if (!modifiers_recorded)
    return;

  if (!(GetKeyState (VK_CONTROL) & 0x8000)) 
    modifiers[EMACS_RCONTROL] = modifiers[EMACS_LCONTROL] = 0;

  if (!(GetKeyState (VK_MENU) & 0x8000)) 
    modifiers[EMACS_RMENU] = modifiers[EMACS_LMENU] = 0;
}

static int
modifier_set (int vkey)
{
  if (vkey == VK_CAPITAL || vkey == VK_SCROLL)
    return (GetKeyState (vkey) & 0x1);
  if (!modifiers_recorded)
    return (GetKeyState (vkey) & 0x8000);

  switch (vkey)
    {
    case VK_LCONTROL:
      return modifiers[EMACS_LCONTROL];
    case VK_RCONTROL:
      return modifiers[EMACS_RCONTROL];
    case VK_LMENU:
      return modifiers[EMACS_LMENU];
    case VK_RMENU:
      return modifiers[EMACS_RMENU];
    }
  return (GetKeyState (vkey) & 0x8000);
}

/* Convert between the modifier bits W32 uses and the modifier bits
   Emacs uses.  */

unsigned int
w32_key_to_modifier (int key)
{
  Lisp_Object key_mapping;

  switch (key)
    {
    case VK_LWIN:
      key_mapping = Vw32_lwindow_modifier;
      break;
    case VK_RWIN:
      key_mapping = Vw32_rwindow_modifier;
      break;
    case VK_APPS:
      key_mapping = Vw32_apps_modifier;
      break;
    case VK_SCROLL:
      key_mapping = Vw32_scroll_lock_modifier;
      break;
    default:
      key_mapping = Qnil;
    }

  /* NB. This code runs in the input thread, asychronously to the lisp
     thread, so we must be careful to ensure access to lisp data is
     thread-safe.  The following code is safe because the modifier
     variable values are updated atomically from lisp and symbols are
     not relocated by GC.  Also, we don't have to worry about seeing GC
     markbits here.  */
  if (EQ (key_mapping, Qhyper))
    return hyper_modifier;
  if (EQ (key_mapping, Qsuper))
    return super_modifier;
  if (EQ (key_mapping, Qmeta))
    return meta_modifier;
  if (EQ (key_mapping, Qalt))
    return alt_modifier;
  if (EQ (key_mapping, Qctrl))
    return ctrl_modifier;
  if (EQ (key_mapping, Qcontrol)) /* synonym for ctrl */
    return ctrl_modifier;
  if (EQ (key_mapping, Qshift))
    return shift_modifier;

  /* Don't generate any modifier if not explicitly requested.  */
  return 0;
}

unsigned int
w32_get_modifiers ()
{
  return ((modifier_set (VK_SHIFT)   ? shift_modifier : 0) |
	  (modifier_set (VK_CONTROL) ? ctrl_modifier  : 0) |
	  (modifier_set (VK_LWIN)    ? w32_key_to_modifier (VK_LWIN) : 0) |
	  (modifier_set (VK_RWIN)    ? w32_key_to_modifier (VK_RWIN) : 0) |
	  (modifier_set (VK_APPS)    ? w32_key_to_modifier (VK_APPS) : 0) |
	  (modifier_set (VK_SCROLL)  ? w32_key_to_modifier (VK_SCROLL) : 0) |
          (modifier_set (VK_MENU)    ?
	   ((NILP (Vw32_alt_is_meta)) ? alt_modifier : meta_modifier) : 0));
}

/* We map the VK_* modifiers into console modifier constants
   so that we can use the same routines to handle both console
   and window input.  */

static int
construct_console_modifiers ()
{
  int mods;

  mods = 0;
  mods |= (modifier_set (VK_SHIFT)) ? SHIFT_PRESSED : 0;
  mods |= (modifier_set (VK_CAPITAL)) ? CAPSLOCK_ON : 0;
  mods |= (modifier_set (VK_SCROLL)) ? SCROLLLOCK_ON : 0;
  mods |= (modifier_set (VK_NUMLOCK)) ? NUMLOCK_ON : 0;
  mods |= (modifier_set (VK_LCONTROL)) ? LEFT_CTRL_PRESSED : 0;
  mods |= (modifier_set (VK_RCONTROL)) ? RIGHT_CTRL_PRESSED : 0;
  mods |= (modifier_set (VK_LMENU)) ? LEFT_ALT_PRESSED : 0;
  mods |= (modifier_set (VK_RMENU)) ? RIGHT_ALT_PRESSED : 0;
  mods |= (modifier_set (VK_LWIN)) ? LEFT_WIN_PRESSED : 0;
  mods |= (modifier_set (VK_RWIN)) ? RIGHT_WIN_PRESSED : 0;
  mods |= (modifier_set (VK_APPS)) ? APPS_PRESSED : 0;

  return mods;
}

static int
w32_get_key_modifiers (unsigned int wparam, unsigned int lparam)
{
  int mods;

  /* Convert to emacs modifiers.  */
  mods = w32_kbd_mods_to_emacs (construct_console_modifiers (), wparam);

  return mods;
}

unsigned int
map_keypad_keys (unsigned int virt_key, unsigned int extended)
{
  if (virt_key < VK_CLEAR || virt_key > VK_DELETE)
    return virt_key;

  if (virt_key == VK_RETURN)
    return (extended ? VK_NUMPAD_ENTER : VK_RETURN);

  if (virt_key >= VK_PRIOR && virt_key <= VK_DOWN)
    return (!extended ? (VK_NUMPAD_PRIOR + (virt_key - VK_PRIOR)) : virt_key);

  if (virt_key == VK_INSERT || virt_key == VK_DELETE)
    return (!extended ? (VK_NUMPAD_INSERT + (virt_key - VK_INSERT)) : virt_key);

  if (virt_key == VK_CLEAR)
    return (!extended ? VK_NUMPAD_CLEAR : virt_key);

  return virt_key;
}

/* List of special key combinations which w32 would normally capture,
   but emacs should grab instead.  Not directly visible to lisp, to
   simplify synchronization.  Each item is an integer encoding a virtual
   key code and modifier combination to capture.  */
Lisp_Object w32_grabbed_keys;

#define HOTKEY(vk,mods)       make_number (((vk) & 255) | ((mods) << 8))
#define HOTKEY_ID(k)          (XFASTINT (k) & 0xbfff)
#define HOTKEY_VK_CODE(k)     (XFASTINT (k) & 255)
#define HOTKEY_MODIFIERS(k)   (XFASTINT (k) >> 8)

/* Register hot-keys for reserved key combinations when Emacs has
   keyboard focus, since this is the only way Emacs can receive key
   combinations like Alt-Tab which are used by the system.  */

static void
register_hot_keys (hwnd)
     HWND hwnd;
{
  Lisp_Object keylist;

  /* Use GC_CONSP, since we are called asynchronously.  */
  for (keylist = w32_grabbed_keys; GC_CONSP (keylist); keylist = XCDR (keylist))
    {
      Lisp_Object key = XCAR (keylist);

      /* Deleted entries get set to nil.  */
      if (!INTEGERP (key))
	continue;

      RegisterHotKey (hwnd, HOTKEY_ID (key),
		      HOTKEY_MODIFIERS (key), HOTKEY_VK_CODE (key));
    }
}

static void
unregister_hot_keys (hwnd)
     HWND hwnd;
{
  Lisp_Object keylist;

  /* Use GC_CONSP, since we are called asynchronously.  */
  for (keylist = w32_grabbed_keys; GC_CONSP (keylist); keylist = XCDR (keylist))
    {
      Lisp_Object key = XCAR (keylist);

      if (!INTEGERP (key))
	continue;

      UnregisterHotKey (hwnd, HOTKEY_ID (key));
    }
}

/* Main message dispatch loop. */

static void
w32_msg_pump (deferred_msg * msg_buf)
{
  MSG msg;
  int result;
  HWND focus_window;

  msh_mousewheel = RegisterWindowMessage (MSH_MOUSEWHEEL);
  
  while (GetMessage (&msg, NULL, 0, 0))
    {
      if (msg.hwnd == NULL)
	{
	  switch (msg.message)
	    {
	    case WM_NULL:
	      /* Produced by complete_deferred_msg; just ignore.  */
	      break;
	    case WM_EMACS_CREATEWINDOW:
	      w32_createwindow ((struct frame *) msg.wParam);
	      if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE, 0, 0))
		abort ();
	      break;
	    case WM_EMACS_SETLOCALE:
	      SetThreadLocale (msg.wParam);
	      /* Reply is not expected.  */
	      break;
	    case WM_EMACS_SETKEYBOARDLAYOUT:
	      result = (int) ActivateKeyboardLayout ((HKL) msg.wParam, 0);
	      if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE,
				      result, 0))
		abort ();
	      break;
	    case WM_EMACS_REGISTER_HOT_KEY:
	      focus_window = GetFocus ();
	      if (focus_window != NULL)
		RegisterHotKey (focus_window,
				HOTKEY_ID (msg.wParam),
				HOTKEY_MODIFIERS (msg.wParam),
				HOTKEY_VK_CODE (msg.wParam));
	      /* Reply is not expected.  */
	      break;
	    case WM_EMACS_UNREGISTER_HOT_KEY:
	      focus_window = GetFocus ();
	      if (focus_window != NULL)
		UnregisterHotKey (focus_window, HOTKEY_ID (msg.wParam));
	      /* Mark item as erased.  NB: this code must be
                 thread-safe.  The next line is okay because the cons
                 cell is never made into garbage and is not relocated by
                 GC.  */
	      XCAR ((Lisp_Object) msg.lParam) = Qnil;
	      if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE, 0, 0))
		abort ();
	      break;
	    case WM_EMACS_TOGGLE_LOCK_KEY:
	      {
		int vk_code = (int) msg.wParam;
		int cur_state = (GetKeyState (vk_code) & 1);
		Lisp_Object new_state = (Lisp_Object) msg.lParam;

		/* NB: This code must be thread-safe.  It is safe to
                   call NILP because symbols are not relocated by GC,
                   and pointer here is not touched by GC (so the markbit
                   can't be set).  Numbers are safe because they are
                   immediate values.  */
		if (NILP (new_state)
		    || (NUMBERP (new_state)
			&& (XUINT (new_state)) & 1 != cur_state))
		  {
		    one_w32_display_info.faked_key = vk_code;

		    keybd_event ((BYTE) vk_code,
				 (BYTE) MapVirtualKey (vk_code, 0),
				 KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
		    keybd_event ((BYTE) vk_code,
				 (BYTE) MapVirtualKey (vk_code, 0),
				 KEYEVENTF_EXTENDEDKEY | 0, 0);
		    keybd_event ((BYTE) vk_code,
				 (BYTE) MapVirtualKey (vk_code, 0),
				 KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
		    cur_state = !cur_state;
		  }
		if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE,
					cur_state, 0))
		  abort ();
	      }
	      break;
	    default:
	      DebPrint (("msg %x not expected by w32_msg_pump\n", msg.message));
	    }
	}
      else
	{
	  DispatchMessage (&msg);
	}

      /* Exit nested loop when our deferred message has completed.  */
      if (msg_buf->completed)
	break;
    }
}

deferred_msg * deferred_msg_head;

static deferred_msg *
find_deferred_msg (HWND hwnd, UINT msg)
{
  deferred_msg * item;

  /* Don't actually need synchronization for read access, since
     modification of single pointer is always atomic.  */
  /* enter_crit (); */

  for (item = deferred_msg_head; item != NULL; item = item->next)
    if (item->w32msg.msg.hwnd == hwnd
	&& item->w32msg.msg.message == msg)
      break;

  /* leave_crit (); */

  return item;
}

static LRESULT
send_deferred_msg (deferred_msg * msg_buf,
		   HWND hwnd,
		   UINT msg,
		   WPARAM wParam,
		   LPARAM lParam)
{
  /* Only input thread can send deferred messages.  */
  if (GetCurrentThreadId () != dwWindowsThreadId)
    abort ();

  /* It is an error to send a message that is already deferred.  */
  if (find_deferred_msg (hwnd, msg) != NULL)
    abort ();

  /* Enforced synchronization is not needed because this is the only
     function that alters deferred_msg_head, and the following critical
     section is guaranteed to only be serially reentered (since only the
     input thread can call us).  */

  /* enter_crit (); */

  msg_buf->completed = 0;
  msg_buf->next = deferred_msg_head;
  deferred_msg_head = msg_buf;
  my_post_msg (&msg_buf->w32msg, hwnd, msg, wParam, lParam);

  /* leave_crit (); */

  /* Start a new nested message loop to process other messages until
     this one is completed.  */
  w32_msg_pump (msg_buf);

  deferred_msg_head = msg_buf->next;

  return msg_buf->result;
}

void
complete_deferred_msg (HWND hwnd, UINT msg, LRESULT result)
{
  deferred_msg * msg_buf = find_deferred_msg (hwnd, msg);

  if (msg_buf == NULL)
    /* Message may have been cancelled, so don't abort().  */
    return;

  msg_buf->result = result;
  msg_buf->completed = 1;

  /* Ensure input thread is woken so it notices the completion.  */
  PostThreadMessage (dwWindowsThreadId, WM_NULL, 0, 0);
}

void
cancel_all_deferred_msgs ()
{
  deferred_msg * item;

  /* Don't actually need synchronization for read access, since
     modification of single pointer is always atomic.  */
  /* enter_crit (); */

  for (item = deferred_msg_head; item != NULL; item = item->next)
    {
      item->result = 0;
      item->completed = 1;
    }

  /* leave_crit (); */

  /* Ensure input thread is woken so it notices the completion.  */
  PostThreadMessage (dwWindowsThreadId, WM_NULL, 0, 0);
}

DWORD 
w32_msg_worker (dw)
     DWORD dw;
{
  MSG msg;
  deferred_msg dummy_buf;

  /* Ensure our message queue is created */
  
  PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE);
  
  if (!PostThreadMessage (dwMainThreadId, WM_EMACS_DONE, 0, 0))
    abort ();

  memset (&dummy_buf, 0, sizeof (dummy_buf));
  dummy_buf.w32msg.msg.hwnd = NULL;
  dummy_buf.w32msg.msg.message = WM_NULL;

  /* This is the inital message loop which should only exit when the
     application quits.  */
  w32_msg_pump (&dummy_buf);

  return 0;
}

static void
post_character_message (hwnd, msg, wParam, lParam, modifiers)
     HWND hwnd;
     UINT msg;
     WPARAM wParam;
     LPARAM lParam;
     DWORD  modifiers;

{
  W32Msg wmsg;

  wmsg.dwModifiers = modifiers;

  /* Detect quit_char and set quit-flag directly.  Note that we
     still need to post a message to ensure the main thread will be
     woken up if blocked in sys_select(), but we do NOT want to post
     the quit_char message itself (because it will usually be as if
     the user had typed quit_char twice).  Instead, we post a dummy
     message that has no particular effect. */
  {
    int c = wParam;
    if (isalpha (c) && wmsg.dwModifiers == ctrl_modifier)
      c = make_ctrl_char (c) & 0377;
    if (c == quit_char
	|| (wmsg.dwModifiers == 0 &&
	    XFASTINT (Vw32_quit_key) && wParam == XFASTINT (Vw32_quit_key)))
      {
	Vquit_flag = Qt;

	/* The choice of message is somewhat arbitrary, as long as
	   the main thread handler just ignores it. */
	msg = WM_NULL;

	/* Interrupt any blocking system calls.  */
	signal_quit ();

	/* As a safety precaution, forcibly complete any deferred
           messages.  This is a kludge, but I don't see any particularly
           clean way to handle the situation where a deferred message is
           "dropped" in the lisp thread, and will thus never be
           completed, eg. by the user trying to activate the menubar
           when the lisp thread is busy, and then typing C-g when the
           menubar doesn't open promptly (with the result that the
           menubar never responds at all because the deferred
           WM_INITMENU message is never completed).  Another problem
           situation is when the lisp thread calls SendMessage (to send
           a window manager command) when a message has been deferred;
           the lisp thread gets blocked indefinitely waiting for the
           deferred message to be completed, which itself is waiting for
           the lisp thread to respond.

	   Note that we don't want to block the input thread waiting for
	   a reponse from the lisp thread (although that would at least
	   solve the deadlock problem above), because we want to be able
	   to receive C-g to interrupt the lisp thread.  */
	cancel_all_deferred_msgs ();
      }
  }

  my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
}

/* Main window procedure */

LRESULT CALLBACK 
w32_wnd_proc (hwnd, msg, wParam, lParam)
     HWND hwnd;
     UINT msg;
     WPARAM wParam;
     LPARAM lParam;
{
  struct frame *f;
  struct w32_display_info *dpyinfo = &one_w32_display_info;
  W32Msg wmsg;
  int windows_translate;
  int key;

  /* Note that it is okay to call x_window_to_frame, even though we are
     not running in the main lisp thread, because frame deletion
     requires the lisp thread to synchronize with this thread.  Thus, if
     a frame struct is returned, it can be used without concern that the
     lisp thread might make it disappear while we are using it.

     NB. Walking the frame list in this thread is safe (as long as
     writes of Lisp_Object slots are atomic, which they are on Windows).
     Although delete-frame can destructively modify the frame list while
     we are walking it, a garbage collection cannot occur until after
     delete-frame has synchronized with this thread.

     It is also safe to use functions that make GDI calls, such as
     w32_clear_rect, because these functions must obtain a DC handle
     from the frame struct using get_frame_dc which is thread-aware.  */

  switch (msg) 
    {
    case WM_ERASEBKGND:
      f = x_window_to_frame (dpyinfo, hwnd);
      if (f)
	{
	  GetUpdateRect (hwnd, &wmsg.rect, FALSE);
	  w32_clear_rect (f, NULL, &wmsg.rect);

#if defined (W32_DEBUG_DISPLAY)
          DebPrint (("WM_ERASEBKGND: erasing %d,%d-%d,%d\n",
                     wmsg.rect.left, wmsg.rect.top, wmsg.rect.right,
                     wmsg.rect.bottom));
#endif /* W32_DEBUG_DISPLAY */
	}
      return 1;
    case WM_PALETTECHANGED:
      /* ignore our own changes */
      if ((HWND)wParam != hwnd)
        {
	  f = x_window_to_frame (dpyinfo, hwnd);
	  if (f)
	    /* get_frame_dc will realize our palette and force all
	       frames to be redrawn if needed. */
	    release_frame_dc (f, get_frame_dc (f));
	}
      return 0;
    case WM_PAINT:
      {
  	PAINTSTRUCT paintStruct;
        RECT update_rect;

        /* MSDN Docs say not to call BeginPaint if GetUpdateRect
           fails.  Apparently this can happen under some
           circumstances.  */
        if (!w32_strict_painting || GetUpdateRect (hwnd, &update_rect, FALSE))
          {
            enter_crit ();
            BeginPaint (hwnd, &paintStruct);

	    if (w32_strict_painting)
	      /* The rectangles returned by GetUpdateRect and BeginPaint
		 do not always match.  GetUpdateRect seems to be the
		 more reliable of the two.  */
	      wmsg.rect = update_rect;
	    else
	      wmsg.rect = paintStruct.rcPaint;

#if defined (W32_DEBUG_DISPLAY)
            DebPrint (("WM_PAINT: painting %d,%d-%d,%d\n", wmsg.rect.left,
                       wmsg.rect.top, wmsg.rect.right, wmsg.rect.bottom));
            DebPrint (("WM_PAINT: update region is %d,%d-%d,%d\n",
                       update_rect.left, update_rect.top,
                       update_rect.right, update_rect.bottom));
#endif
            EndPaint (hwnd, &paintStruct);
            leave_crit ();

            my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
          
            return 0;
          }

	/* If GetUpdateRect returns 0 (meaning there is no update
           region), assume the whole window needs to be repainted.  */
	GetClientRect(hwnd, &wmsg.rect);
	my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
        return 0;
      }

    case WM_INPUTLANGCHANGE:
      /* Inform lisp thread of keyboard layout changes.  */
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);

      /* Clear dead keys in the keyboard state; for simplicity only
         preserve modifier key states.  */
      {
	int i;
	BYTE keystate[256];

	GetKeyboardState (keystate);
	for (i = 0; i < 256; i++)
	  if (1
	      && i != VK_SHIFT
	      && i != VK_LSHIFT
	      && i != VK_RSHIFT
	      && i != VK_CAPITAL
	      && i != VK_NUMLOCK
	      && i != VK_SCROLL
	      && i != VK_CONTROL
	      && i != VK_LCONTROL
	      && i != VK_RCONTROL
	      && i != VK_MENU
	      && i != VK_LMENU
	      && i != VK_RMENU
	      && i != VK_LWIN
	      && i != VK_RWIN)
	    keystate[i] = 0;
	SetKeyboardState (keystate);
      }
      goto dflt;

    case WM_HOTKEY:
      /* Synchronize hot keys with normal input.  */
      PostMessage (hwnd, WM_KEYDOWN, HIWORD (lParam), 0);
      return (0);

    case WM_KEYUP:
    case WM_SYSKEYUP:
      record_keyup (wParam, lParam);
      goto dflt;

    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
      /* Ignore keystrokes we fake ourself; see below.  */
      if (dpyinfo->faked_key == wParam)
	{
	  dpyinfo->faked_key = 0;
	  /* Make sure TranslateMessage sees them though (as long as
	     they don't produce WM_CHAR messages).  This ensures that
	     indicator lights are toggled promptly on Windows 9x, for
	     example.  */
	  if (lispy_function_keys[wParam] != 0)
	    {
	      windows_translate = 1;
	      goto translate;
	    }
	  return 0;
	}

      /* Synchronize modifiers with current keystroke.  */
      sync_modifiers ();
      record_keydown (wParam, lParam);
      wParam = map_keypad_keys (wParam, (lParam & 0x1000000L) != 0);

      windows_translate = 0;

      switch (wParam)
	{
	case VK_LWIN:
	  if (NILP (Vw32_pass_lwindow_to_system))
	    {
	      /* Prevent system from acting on keyup (which opens the
		 Start menu if no other key was pressed) by simulating a
		 press of Space which we will ignore.  */
	      if (GetAsyncKeyState (wParam) & 1)
		{
		  if (NUMBERP (Vw32_phantom_key_code))
		    key = XUINT (Vw32_phantom_key_code) & 255;
		  else
		    key = VK_SPACE;
		  dpyinfo->faked_key = key;
		  keybd_event (key, (BYTE) MapVirtualKey (key, 0), 0, 0);
		}
	    }
	  if (!NILP (Vw32_lwindow_modifier))
	    return 0;
	  break;
	case VK_RWIN:
	  if (NILP (Vw32_pass_rwindow_to_system))
	    {
	      if (GetAsyncKeyState (wParam) & 1)
		{
		  if (NUMBERP (Vw32_phantom_key_code))
		    key = XUINT (Vw32_phantom_key_code) & 255;
		  else
		    key = VK_SPACE;
		  dpyinfo->faked_key = key;
		  keybd_event (key, (BYTE) MapVirtualKey (key, 0), 0, 0);
		}
	    }
	  if (!NILP (Vw32_rwindow_modifier))
	    return 0;
	  break;
  	case VK_APPS:
	  if (!NILP (Vw32_apps_modifier))
	    return 0;
	  break;
	case VK_MENU:
	  if (NILP (Vw32_pass_alt_to_system)) 
	    /* Prevent DefWindowProc from activating the menu bar if an
               Alt key is pressed and released by itself.  */
	    return 0;
	  windows_translate = 1;
	  break;
	case VK_CAPITAL: 
	  /* Decide whether to treat as modifier or function key.  */
	  if (NILP (Vw32_enable_caps_lock))
	    goto disable_lock_key;
	  windows_translate = 1;
	  break;
	case VK_NUMLOCK:
	  /* Decide whether to treat as modifier or function key.  */
	  if (NILP (Vw32_enable_num_lock))
	    goto disable_lock_key;
	  windows_translate = 1;
	  break;
	case VK_SCROLL:
	  /* Decide whether to treat as modifier or function key.  */
	  if (NILP (Vw32_scroll_lock_modifier))
	    goto disable_lock_key;
	  windows_translate = 1;
	  break;
	disable_lock_key:
	  /* Ensure the appropriate lock key state (and indicator light)
             remains in the same state. We do this by faking another
             press of the relevant key.  Apparently, this really is the
             only way to toggle the state of the indicator lights.  */
	  dpyinfo->faked_key = wParam;
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | 0, 0);
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
	  /* Ensure indicator lights are updated promptly on Windows 9x
             (TranslateMessage apparently does this), after forwarding
             input event.  */
	  post_character_message (hwnd, msg, wParam, lParam,
				  w32_get_key_modifiers (wParam, lParam));
	  windows_translate = 1;
	  break;
	case VK_CONTROL: 
	case VK_SHIFT:
	case VK_PROCESSKEY:  /* Generated by IME.  */
	  windows_translate = 1;
	  break;
	case VK_CANCEL:
	  /* Windows maps Ctrl-Pause (aka Ctrl-Break) into VK_CANCEL,
             which is confusing for purposes of key binding; convert
	     VK_CANCEL events into VK_PAUSE events.  */
	  wParam = VK_PAUSE;
	  break;
	case VK_PAUSE:
	  /* Windows maps Ctrl-NumLock into VK_PAUSE, which is confusing
             for purposes of key binding; convert these back into
             VK_NUMLOCK events, at least when we want to see NumLock key
             presses.  (Note that there is never any possibility that
             VK_PAUSE with Ctrl really is C-Pause as per above.)  */
	  if (NILP (Vw32_enable_num_lock) && modifier_set (VK_CONTROL))
	    wParam = VK_NUMLOCK;
	  break;
	default:
	  /* If not defined as a function key, change it to a WM_CHAR message. */
	  if (lispy_function_keys[wParam] == 0)
	    {
	      DWORD modifiers = construct_console_modifiers ();

	      if (!NILP (Vw32_recognize_altgr)
		  && modifier_set (VK_LCONTROL) && modifier_set (VK_RMENU))
		{
		  /* Always let TranslateMessage handle AltGr key chords;
		     for some reason, ToAscii doesn't always process AltGr
		     chords correctly.  */
		  windows_translate = 1;
		}
	      else if ((modifiers & (~SHIFT_PRESSED & ~CAPSLOCK_ON)) != 0)
		{
		  /* Handle key chords including any modifiers other
		     than shift directly, in order to preserve as much
		     modifier information as possible.  */
		  if ('A' <= wParam && wParam <= 'Z')
		    {
		      /* Don't translate modified alphabetic keystrokes,
			 so the user doesn't need to constantly switch
			 layout to type control or meta keystrokes when
			 the normal layout translates alphabetic
			 characters to non-ascii characters.  */
		      if (!modifier_set (VK_SHIFT))
			wParam += ('a' - 'A');
		      msg = WM_CHAR;
		    }
		  else
		    {
		      /* Try to handle other keystrokes by determining the
			 base character (ie. translating the base key plus
			 shift modifier).  */
		      int add;
		      int isdead = 0;
		      KEY_EVENT_RECORD key;
		  
		      key.bKeyDown = TRUE;
		      key.wRepeatCount = 1;
		      key.wVirtualKeyCode = wParam;
		      key.wVirtualScanCode = (lParam & 0xFF0000) >> 16;
		      key.uChar.AsciiChar = 0;
		      key.dwControlKeyState = modifiers;

		      add = w32_kbd_patch_key (&key);
		      /* 0 means an unrecognised keycode, negative means
			 dead key.  Ignore both.  */
		      while (--add >= 0)
			{
			  /* Forward asciified character sequence.  */
			  post_character_message
			    (hwnd, WM_CHAR, key.uChar.AsciiChar, lParam,
			     w32_get_key_modifiers (wParam, lParam));
			  w32_kbd_patch_key (&key);
			}
		      return 0;
		    }
		}
	      else
		{
		  /* Let TranslateMessage handle everything else.  */
		  windows_translate = 1;
		}
	    }
	}

    translate:
      if (windows_translate)
	{
	  MSG windows_msg = { hwnd, msg, wParam, lParam, 0, {0,0} };

	  windows_msg.time = GetMessageTime ();
	  TranslateMessage (&windows_msg);
	  goto dflt;
	}

      /* Fall through */
      
    case WM_SYSCHAR:
    case WM_CHAR:
      post_character_message (hwnd, msg, wParam, lParam,
			      w32_get_key_modifiers (wParam, lParam));
      break;

      /* Simulate middle mouse button events when left and right buttons
	 are used together, but only if user has two button mouse. */
    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
      if (XINT (Vw32_num_mouse_buttons) == 3)
	goto handle_plain_button;

      {
	int this = (msg == WM_LBUTTONDOWN) ? LMOUSE : RMOUSE;
	int other = (msg == WM_LBUTTONDOWN) ? RMOUSE : LMOUSE;

	if (button_state & this)
	  return 0;

	if (button_state == 0)
	  SetCapture (hwnd);

	button_state |= this;

	if (button_state & other)
	  {
	    if (mouse_button_timer)
	      {
		KillTimer (hwnd, mouse_button_timer);
		mouse_button_timer = 0;

		/* Generate middle mouse event instead. */
		msg = WM_MBUTTONDOWN;
		button_state |= MMOUSE;
	      }
	    else if (button_state & MMOUSE)
	      {
		/* Ignore button event if we've already generated a
		   middle mouse down event.  This happens if the
		   user releases and press one of the two buttons
		   after we've faked a middle mouse event. */
		return 0;
	      }
	    else
	      {
		/* Flush out saved message. */
		post_msg (&saved_mouse_button_msg);
	      }
	    wmsg.dwModifiers = w32_get_modifiers ();
	    my_post_msg (&wmsg, hwnd, msg, wParam, lParam);

	    /* Clear message buffer. */
	    saved_mouse_button_msg.msg.hwnd = 0;
	  }
	else
	  {
	    /* Hold onto message for now. */
	    mouse_button_timer =
	      SetTimer (hwnd, MOUSE_BUTTON_ID,
			XINT (Vw32_mouse_button_tolerance), NULL);
	    saved_mouse_button_msg.msg.hwnd = hwnd;
	    saved_mouse_button_msg.msg.message = msg;
	    saved_mouse_button_msg.msg.wParam = wParam;
	    saved_mouse_button_msg.msg.lParam = lParam;
	    saved_mouse_button_msg.msg.time = GetMessageTime ();
	    saved_mouse_button_msg.dwModifiers = w32_get_modifiers ();
	  }
      }
      return 0;

    case WM_LBUTTONUP:
    case WM_RBUTTONUP:
      if (XINT (Vw32_num_mouse_buttons) == 3)
	goto handle_plain_button;

      {
	int this = (msg == WM_LBUTTONUP) ? LMOUSE : RMOUSE;
	int other = (msg == WM_LBUTTONUP) ? RMOUSE : LMOUSE;

	if ((button_state & this) == 0)
	  return 0;

	button_state &= ~this;

	if (button_state & MMOUSE)
	  {
	    /* Only generate event when second button is released. */
	    if ((button_state & other) == 0)
	      {
		msg = WM_MBUTTONUP;
		button_state &= ~MMOUSE;

		if (button_state) abort ();
	      }
	    else
	      return 0;
	  }
	else
	  {
	    /* Flush out saved message if necessary. */
	    if (saved_mouse_button_msg.msg.hwnd)
	      {
		post_msg (&saved_mouse_button_msg);
	      }
	  }
	wmsg.dwModifiers = w32_get_modifiers ();
	my_post_msg (&wmsg, hwnd, msg, wParam, lParam);

	/* Always clear message buffer and cancel timer. */
	saved_mouse_button_msg.msg.hwnd = 0;
	KillTimer (hwnd, mouse_button_timer);
	mouse_button_timer = 0;

	if (button_state == 0)
	  ReleaseCapture ();
      }
      return 0;

    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    handle_plain_button:
      {
	BOOL up;
	int button;

	if (parse_button (msg, &button, &up))
	  {
	    if (up) ReleaseCapture ();
	    else SetCapture (hwnd);
	    button = (button == 0) ? LMOUSE : 
	      ((button == 1) ? MMOUSE  : RMOUSE);
	    if (up)
	      button_state &= ~button;
	    else
	      button_state |= button;
	  }
      }
      
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      return 0;

    case WM_VSCROLL:
    case WM_MOUSEMOVE:
      if (XINT (Vw32_mouse_move_interval) <= 0
	  || (msg == WM_MOUSEMOVE && button_state == 0))
  	{
	  wmsg.dwModifiers = w32_get_modifiers ();
	  my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
	  return 0;
  	}
  
      /* Hang onto mouse move and scroll messages for a bit, to avoid
	 sending such events to Emacs faster than it can process them.
	 If we get more events before the timer from the first message
	 expires, we just replace the first message. */

      if (saved_mouse_move_msg.msg.hwnd == 0)
	mouse_move_timer =
	  SetTimer (hwnd, MOUSE_MOVE_ID,
		    XINT (Vw32_mouse_move_interval), NULL);

      /* Hold onto message for now. */
      saved_mouse_move_msg.msg.hwnd = hwnd;
      saved_mouse_move_msg.msg.message = msg;
      saved_mouse_move_msg.msg.wParam = wParam;
      saved_mouse_move_msg.msg.lParam = lParam;
      saved_mouse_move_msg.msg.time = GetMessageTime ();
      saved_mouse_move_msg.dwModifiers = w32_get_modifiers ();
  
      return 0;

    case WM_MOUSEWHEEL:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      return 0;

    case WM_DROPFILES:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      return 0;

    case WM_TIMER:
      /* Flush out saved messages if necessary. */
      if (wParam == mouse_button_timer)
	{
	  if (saved_mouse_button_msg.msg.hwnd)
	    {
	      post_msg (&saved_mouse_button_msg);
	      saved_mouse_button_msg.msg.hwnd = 0;
	    }
	  KillTimer (hwnd, mouse_button_timer);
	  mouse_button_timer = 0;
	}
      else if (wParam == mouse_move_timer)
	{
	  if (saved_mouse_move_msg.msg.hwnd)
	    {
	      post_msg (&saved_mouse_move_msg);
	      saved_mouse_move_msg.msg.hwnd = 0;
	    }
	  KillTimer (hwnd, mouse_move_timer);
	  mouse_move_timer = 0;
	}
      return 0;
  
    case WM_NCACTIVATE:
      /* Windows doesn't send us focus messages when putting up and
	 taking down a system popup dialog as for Ctrl-Alt-Del on Windows 95.
	 The only indication we get that something happened is receiving
	 this message afterwards.  So this is a good time to reset our
	 keyboard modifiers' state. */
      reset_modifiers ();
      goto dflt;

    case WM_INITMENU:
      button_state = 0;
      ReleaseCapture ();
      /* We must ensure menu bar is fully constructed and up to date
	 before allowing user interaction with it.  To achieve this
	 we send this message to the lisp thread and wait for a
	 reply (whose value is not actually needed) to indicate that
	 the menu bar is now ready for use, so we can now return.

	 To remain responsive in the meantime, we enter a nested message
	 loop that can process all other messages.

	 However, we skip all this if the message results from calling
	 TrackPopupMenu - in fact, we must NOT attempt to send the lisp
	 thread a message because it is blocked on us at this point.  We
	 set menubar_active before calling TrackPopupMenu to indicate
	 this (there is no possibility of confusion with real menubar
	 being active).  */

      f = x_window_to_frame (dpyinfo, hwnd);
      if (f
	  && (f->output_data.w32->menubar_active
	      /* We can receive this message even in the absence of a
		 menubar (ie. when the system menu is activated) - in this
		 case we do NOT want to forward the message, otherwise it
		 will cause the menubar to suddenly appear when the user
		 had requested it to be turned off!  */
	      || f->output_data.w32->menubar_widget == NULL))
	return 0;

      {
	deferred_msg msg_buf;

	/* Detect if message has already been deferred; in this case
	   we cannot return any sensible value to ignore this.  */
	if (find_deferred_msg (hwnd, msg) != NULL)
	  abort ();

	return send_deferred_msg (&msg_buf, hwnd, msg, wParam, lParam);
      }

    case WM_EXITMENULOOP:
      f = x_window_to_frame (dpyinfo, hwnd);

      /* Indicate that menubar can be modified again.  */
      if (f)
	f->output_data.w32->menubar_active = 0;
      goto dflt;

    case WM_MEASUREITEM:
      f = x_window_to_frame (dpyinfo, hwnd);
      if (f)
	{
	  MEASUREITEMSTRUCT * pMis = (MEASUREITEMSTRUCT *) lParam;

	  if (pMis->CtlType == ODT_MENU)
	    {
	      /* Work out dimensions for popup menu titles. */
	      char * title = (char *) pMis->itemData;
	      HDC hdc = GetDC (hwnd);
	      HFONT menu_font = GetCurrentObject (hdc, OBJ_FONT);
	      LOGFONT menu_logfont;
	      HFONT old_font;
	      SIZE size;

	      GetObject (menu_font, sizeof (menu_logfont), &menu_logfont);
	      menu_logfont.lfWeight = FW_BOLD;
	      menu_font = CreateFontIndirect (&menu_logfont);
	      old_font = SelectObject (hdc, menu_font);

	      GetTextExtentPoint32 (hdc, title, strlen (title), &size);
	      pMis->itemWidth = size.cx;
	      pMis->itemHeight = GetSystemMetrics (SM_CYMENUSIZE);
	      if (pMis->itemHeight < size.cy)
		pMis->itemHeight = size.cy;

	      SelectObject (hdc, old_font);
	      DeleteObject (menu_font);
	      ReleaseDC (hwnd, hdc);
	      return TRUE;
	    }
	}
      return 0;

    case WM_DRAWITEM:
      f = x_window_to_frame (dpyinfo, hwnd);
      if (f)
	{
	  DRAWITEMSTRUCT * pDis = (DRAWITEMSTRUCT *) lParam;

	  if (pDis->CtlType == ODT_MENU)
	    {
	      /* Draw popup menu title. */
	      char * title = (char *) pDis->itemData;
	      HDC hdc = pDis->hDC;
	      HFONT menu_font = GetCurrentObject (hdc, OBJ_FONT);
	      LOGFONT menu_logfont;
	      HFONT old_font;

	      GetObject (menu_font, sizeof (menu_logfont), &menu_logfont);
	      menu_logfont.lfWeight = FW_BOLD;
	      menu_font = CreateFontIndirect (&menu_logfont);
	      old_font = SelectObject (hdc, menu_font);

	      /* Always draw title as if not selected.  */
	      ExtTextOut (hdc,
			  pDis->rcItem.left + GetSystemMetrics (SM_CXMENUCHECK),
			  pDis->rcItem.top,
			  ETO_OPAQUE, &pDis->rcItem,
			  title, strlen (title), NULL);

	      SelectObject (hdc, old_font);
	      DeleteObject (menu_font);
	      return TRUE;
	    }
	}
      return 0;

#if 0
      /* Still not right - can't distinguish between clicks in the
	 client area of the frame from clicks forwarded from the scroll
	 bars - may have to hook WM_NCHITTEST to remember the mouse
	 position and then check if it is in the client area ourselves.  */
    case WM_MOUSEACTIVATE:
      /* Discard the mouse click that activates a frame, allowing the
	 user to click anywhere without changing point (or worse!).
	 Don't eat mouse clicks on scrollbars though!!  */
      if (LOWORD (lParam) == HTCLIENT )
	return MA_ACTIVATEANDEAT;
      goto dflt;
#endif

    case WM_ACTIVATEAPP:
    case WM_ACTIVATE:
    case WM_WINDOWPOSCHANGED:
    case WM_SHOWWINDOW:
      /* Inform lisp thread that a frame might have just been obscured
	 or exposed, so should recheck visibility of all frames.  */
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      goto dflt;

    case WM_SETFOCUS:
      dpyinfo->faked_key = 0;
      reset_modifiers ();
      register_hot_keys (hwnd);
      goto command;
    case WM_KILLFOCUS:
      unregister_hot_keys (hwnd);
      button_state = 0;
      ReleaseCapture ();
    case WM_MOVE:
    case WM_SIZE:
    case WM_COMMAND:
    command:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      goto dflt;

    case WM_CLOSE:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      return 0;

    case WM_WINDOWPOSCHANGING:
      {
	WINDOWPLACEMENT wp;
	LPWINDOWPOS lppos = (WINDOWPOS *) lParam;

	wp.length = sizeof (WINDOWPLACEMENT);
	GetWindowPlacement (hwnd, &wp);
	
	if (wp.showCmd != SW_SHOWMINIMIZED && (lppos->flags & SWP_NOSIZE) == 0)
	  {
	    RECT rect;
	    int wdiff;
	    int hdiff;
	    DWORD font_width;
	    DWORD line_height;
	    DWORD internal_border;
	    DWORD scrollbar_extra;
	    RECT wr;
	    
	    wp.length = sizeof(wp);
	    GetWindowRect (hwnd, &wr);
	    
	    enter_crit ();
	    
	    font_width = GetWindowLong (hwnd, WND_FONTWIDTH_INDEX);
	    line_height = GetWindowLong (hwnd, WND_LINEHEIGHT_INDEX);
	    internal_border = GetWindowLong (hwnd, WND_BORDER_INDEX);
	    scrollbar_extra = GetWindowLong (hwnd, WND_SCROLLBAR_INDEX);
	    
	    leave_crit ();
	    
	    memset (&rect, 0, sizeof (rect));
	    AdjustWindowRect (&rect, GetWindowLong (hwnd, GWL_STYLE), 
			      GetMenu (hwnd) != NULL);

	    /* Force width and height of client area to be exact
	       multiples of the character cell dimensions.  */
	    wdiff = (lppos->cx - (rect.right - rect.left)
		     - 2 * internal_border - scrollbar_extra)
	      % font_width;
	    hdiff = (lppos->cy - (rect.bottom - rect.top)
		     - 2 * internal_border)
	      % line_height;
	    
	    if (wdiff || hdiff)
	      {
		/* For right/bottom sizing we can just fix the sizes.  
		   However for top/left sizing we will need to fix the X 
		   and Y positions as well.  */
		
		lppos->cx -= wdiff;
		lppos->cy -= hdiff;
		
		if (wp.showCmd != SW_SHOWMAXIMIZED 
		    && (lppos->flags & SWP_NOMOVE) == 0)
		  {
		    if (lppos->x != wr.left || lppos->y != wr.top)
		      {
			lppos->x += wdiff;
			lppos->y += hdiff;
		      }
		    else
		      {
			lppos->flags |= SWP_NOMOVE;
		      }
		  }
		
		return 0;
	      }
	  }
      }
      
      goto dflt;

    case WM_GETMINMAXINFO:
      /* Hack to correct bug that allows Emacs frames to be resized
	 below the Minimum Tracking Size.  */
      ((LPMINMAXINFO) lParam)->ptMinTrackSize.y++;
      return 0;

    case WM_EMACS_CREATESCROLLBAR:
      return (LRESULT) w32_createscrollbar ((struct frame *) wParam,
					    (struct scroll_bar *) lParam);

    case WM_EMACS_SHOWWINDOW:
      return ShowWindow ((HWND) wParam, (WPARAM) lParam);

    case WM_EMACS_SETFOREGROUND:
      {
        HWND foreground_window;
        DWORD foreground_thread, retval;

        /* On NT 5.0, and apparently Windows 98, it is necessary to
           attach to the thread that currently has focus in order to
           pull the focus away from it.  */
        foreground_window = GetForegroundWindow ();
	foreground_thread = GetWindowThreadProcessId (foreground_window, NULL);
        if (!foreground_window
            || foreground_thread == GetCurrentThreadId ()
            || !AttachThreadInput (GetCurrentThreadId (),
                                   foreground_thread, TRUE))
          foreground_thread = 0;

        retval = SetForegroundWindow ((HWND) wParam);

        /* Detach from the previous foreground thread.  */
        if (foreground_thread)
          AttachThreadInput (GetCurrentThreadId (),
                             foreground_thread, FALSE);

        return retval;
      }

    case WM_EMACS_SETWINDOWPOS:
      {
	WINDOWPOS * pos = (WINDOWPOS *) wParam;
	return SetWindowPos (hwnd, pos->hwndInsertAfter,
			     pos->x, pos->y, pos->cx, pos->cy, pos->flags);
      }

    case WM_EMACS_DESTROYWINDOW:
      DragAcceptFiles ((HWND) wParam, FALSE);
      return DestroyWindow ((HWND) wParam);

    case WM_EMACS_TRACKPOPUPMENU:
      {
	UINT flags;
	POINT *pos;
	int retval;
	pos = (POINT *)lParam;
	flags = TPM_CENTERALIGN;
	if (button_state & LMOUSE)
	  flags |= TPM_LEFTBUTTON;
	else if (button_state & RMOUSE)
	  flags |= TPM_RIGHTBUTTON;
	
	/* Remember we did a SetCapture on the initial mouse down event,
	   so for safety, we make sure the capture is cancelled now.  */
	ReleaseCapture ();
	button_state = 0;

	/* Use menubar_active to indicate that WM_INITMENU is from
           TrackPopupMenu below, and should be ignored.  */
	f = x_window_to_frame (dpyinfo, hwnd);
	if (f)
	  f->output_data.w32->menubar_active = 1;
	
	if (TrackPopupMenu ((HMENU)wParam, flags, pos->x, pos->y, 
			    0, hwnd, NULL))
	  {
	    MSG amsg;
	    /* Eat any mouse messages during popupmenu */
	    while (PeekMessage (&amsg, hwnd, WM_MOUSEFIRST, WM_MOUSELAST,
				PM_REMOVE));
	    /* Get the menu selection, if any */
	    if (PeekMessage (&amsg, hwnd, WM_COMMAND, WM_COMMAND, PM_REMOVE))
	      {
		retval =  LOWORD (amsg.wParam);
	      }
	    else
	      {
		retval = 0;
	      }
	  }
	else
	  {
	    retval = -1;
	  }

	return retval;
      }

    default:
      /* Check for messages registered at runtime. */
      if (msg == msh_mousewheel)
	{
	  wmsg.dwModifiers = w32_get_modifiers ();
	  my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
	  return 0;
	}
      
    dflt:
      return DefWindowProc (hwnd, msg, wParam, lParam);
    }
  

  /* The most common default return code for handled messages is 0.  */
  return 0;
}

void 
my_create_window (f)
     struct frame * f;
{
  MSG msg;

  if (!PostThreadMessage (dwWindowsThreadId, WM_EMACS_CREATEWINDOW, (WPARAM)f, 0))
    abort ();
  GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
}

/* Create and set up the w32 window for frame F.  */

static void
w32_window (f, window_prompting, minibuffer_only)
     struct frame *f;
     long window_prompting;
     int minibuffer_only;
{
  BLOCK_INPUT;

  /* Use the resource name as the top-level window name
     for looking up resources.  Make a non-Lisp copy
     for the window manager, so GC relocation won't bother it.

     Elsewhere we specify the window name for the window manager.  */
     
  {
    char *str = (char *) XSTRING (Vx_resource_name)->data;
    f->namebuf = (char *) xmalloc (strlen (str) + 1);
    strcpy (f->namebuf, str);
  }

  my_create_window (f);

  validate_x_resource_name ();

  /* x_set_name normally ignores requests to set the name if the
     requested name is the same as the current name.  This is the one
     place where that assumption isn't correct; f->name is set, but
     the server hasn't been told.  */
  {
    Lisp_Object name;
    int explicit = f->explicit_name;

    f->explicit_name = 0;
    name = f->name;
    f->name = Qnil;
    x_set_name (f, name, explicit);
  }

  UNBLOCK_INPUT;

  if (!minibuffer_only && FRAME_EXTERNAL_MENU_BAR (f))
    initialize_frame_menubar (f);

  if (FRAME_W32_WINDOW (f) == 0)
    error ("Unable to create window");
}

/* Handle the icon stuff for this window.  Perhaps later we might
   want an x_set_icon_position which can be called interactively as
   well.  */

static void
x_icon (f, parms)
     struct frame *f;
     Lisp_Object parms;
{
  Lisp_Object icon_x, icon_y;

  /* Set the position of the icon.  Note that Windows 95 groups all
     icons in the tray.  */
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

#if 0 /* TODO */
  /* Start up iconic or window? */
  x_wm_set_window_state
    (f, (EQ (x_get_arg (parms, Qvisibility, 0, 0, symbol), Qicon)
	 ? IconicState
	 : NormalState));

  x_text_icon (f, (char *) XSTRING ((!NILP (f->icon_name)
				     ? f->icon_name
				     : f->name))->data);
#endif

  UNBLOCK_INPUT;
}

DEFUN ("x-create-frame", Fx_create_frame, Sx_create_frame,
       1, 1, 0,
  "Make a new window, which is called a \"frame\" in Emacs terms.\n\
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
  struct w32_display_info *dpyinfo;
  Lisp_Object parent;
  struct kboard *kb;

  check_w32 ();

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = x_get_arg (parms, Qdisplay, 0, 0, string);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_x_display_info (display);
#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  name = x_get_arg (parms, Qname, "name", "Name", string);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = x_get_arg (parms, Qparent_id, NULL, NULL, number);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent, 0);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  GCPRO4 (parms, parent, name, frame);
  tem = x_get_arg (parms, Qminibuffer, 0, 0, symbol);
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

  /* Note that Windows does support scroll bars.  */
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;
  /* By default, make scrollbars the system standard width. */
  f->scroll_bar_pixel_width = GetSystemMetrics (SM_CXVSCROLL);

  f->output_method = output_w32;
  f->output_data.w32 = (struct w32_output *) xmalloc (sizeof (struct w32_output));
  bzero (f->output_data.w32, sizeof (struct w32_output));

  FRAME_FONTSET (f) = -1;

  f->icon_name
    = x_get_arg (parms, Qicon_name, "iconName", "Title", string);
  if (! STRINGP (f->icon_name))
    f->icon_name = Qnil;

/*  FRAME_W32_DISPLAY_INFO (f) = dpyinfo; */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif

  /* Specify the parent under which to make this window.  */

  if (!NILP (parent))
    {
      f->output_data.w32->parent_desc = (Window) parent;
      f->output_data.w32->explicit_parent = 1;
    }
  else
    {
      f->output_data.w32->parent_desc = FRAME_W32_DISPLAY_INFO (f)->root_window;
      f->output_data.w32->explicit_parent = 0;
    }

  /* Note that the frame has no physical cursor right now.  */
  f->phys_cursor_x = -1;

  /* Set the name; the functions to which we pass f expect the name to
     be set.  */
  if (EQ (name, Qunbound) || NILP (name))
    {
      f->name = build_string (dpyinfo->w32_id_name);
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
  for (tem = Vglobal_fontset_alist; CONSP (tem); tem = XCONS (tem)->cdr)
    fs_register_fontset (f, XCONS (tem)->car);

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  {
    Lisp_Object font;

    font = x_get_arg (parms, Qfont, "font", "Font", string);
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
      font = x_new_font (f, "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-Courier-normal-r-*-*-*-97-*-*-c-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (! STRINGP (font))
      font = x_new_font (f, "-*-Fixedsys-normal-r-*-*-*-*-90-*-c-*-iso8859-1");
    UNBLOCK_INPUT;
    if (! STRINGP (font))
      font = build_string ("Fixedsys");

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
  /* Default internalBorderWidth to 0 on Windows to match other programs.  */
  x_default_parameter (f, parms, Qinternal_border_width, make_number (0),
		       "internalBorderWidth", "BorderWidth", number);
  x_default_parameter (f, parms, Qvertical_scroll_bars, Qt,
		       "verticalScrollBars", "ScrollBars", boolean);

  /* Also do the stuff which must be set before the window exists.  */
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

  x_default_parameter (f, parms, Qmenu_bar_lines, make_number (1),
		       "menuBar", "MenuBar", number);
  x_default_parameter (f, parms, Qscroll_bar_width, Qnil,
		       "scrollBarWidth", "ScrollBarWidth", number);
  x_default_parameter (f, parms, Qbuffer_predicate, Qnil,
		       "bufferPredicate", "BufferPredicate", symbol);
  x_default_parameter (f, parms, Qtitle, Qnil,
		       "title", "Title", string);

  f->output_data.w32->dwStyle = WS_OVERLAPPEDWINDOW;
  f->output_data.w32->parent_desc = FRAME_W32_DISPLAY_INFO (f)->root_window;
  window_prompting = x_figure_window_size (f, parms);

  if (window_prompting & XNegative)
    {
      if (window_prompting & YNegative)
	f->output_data.w32->win_gravity = SouthEastGravity;
      else
	f->output_data.w32->win_gravity = NorthEastGravity;
    }
  else
    {
      if (window_prompting & YNegative)
	f->output_data.w32->win_gravity = SouthWestGravity;
      else
	f->output_data.w32->win_gravity = NorthWestGravity;
    }

  f->output_data.w32->size_hint_flags = window_prompting;

  w32_window (f, window_prompting, minibuffer_only);
  x_icon (f, parms);
  init_frame_faces (f);

  /* We need to do this after creating the window, so that the
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
     f->height.  */
  width = f->width;
  height = f->height;
  f->height = 0;
  SET_FRAME_WIDTH (f, 0);
  change_frame_size (f, height, width, 1, 0);

  /* Tell the server what size and position, etc, we want,
     and how badly we want them.  */
  BLOCK_INPUT;
  x_wm_set_size_hint (f, window_prompting, 0);
  UNBLOCK_INPUT;

  tem = x_get_arg (parms, Qunsplittable, 0, 0, boolean);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  UNGCPRO;

  /* It is now ok to make the frame official
     even if we get an error below.
     And the frame needs to be on Vframe_list
     or making it visible won't work.  */
  Vframe_list = Fcons (frame, Vframe_list);

  /* Now that the frame is official, it counts as a reference to
     its display.  */
  FRAME_W32_DISPLAY_INFO (f)->reference_count++;

  /* Make the window appear on the frame and enable display,
     unless the caller says not to.  However, with explicit parent,
     Emacs cannot control visibility, so don't try.  */
  if (! f->output_data.w32->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = x_get_arg (parms, Qvisibility, 0, 0, symbol);
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

  return unbind_to (count, frame);
}

/* FRAME is used only to get a handle on the X display.  We don't pass the
   display info directly because we're called from frame.c, which doesn't
   know about that structure.  */
Lisp_Object
x_get_focus_frame (frame)
     struct frame *frame;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (frame);
  Lisp_Object xfocus;
  if (! dpyinfo->w32_focus_frame)
    return Qnil;

  XSETFRAME (xfocus, dpyinfo->w32_focus_frame);
  return xfocus;
}

DEFUN ("w32-focus-frame", Fw32_focus_frame, Sw32_focus_frame, 1, 1, 0,
  "Give FRAME input focus, raising to foreground if necessary.")
  (frame)
     Lisp_Object frame;
{
  x_focus_on_frame (check_x_frame (frame));
  return Qnil;
}


struct font_info *w32_load_bdf_font (struct frame *f, char *fontname,
                                     int size, char* filename);

struct font_info *
w32_load_system_font (f,fontname,size)
     struct frame *f;
     char * fontname;
     int size;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  Lisp_Object font_names;

  /* Get a list of all the fonts that match this name.  Once we
     have a list of matching fonts, we compare them against the fonts
     we already have loaded by comparing names.  */
  font_names = w32_list_fonts (f, build_string (fontname), size, 100);

  if (!NILP (font_names))
  {
      Lisp_Object tail;
      int i;
#if 0 /* This code has nasty side effects that cause Emacs to crash.  */

      /* First check if any are already loaded, as that is cheaper
         than loading another one. */
      for (i = 0; i < dpyinfo->n_fonts; i++)
	for (tail = font_names; CONSP (tail); tail = XCONS (tail)->cdr)
	  if (!strcmp (dpyinfo->font_table[i].name,
		       XSTRING (XCONS (tail)->car)->data)
	      || !strcmp (dpyinfo->font_table[i].full_name,
			  XSTRING (XCONS (tail)->car)->data))
	    return (dpyinfo->font_table + i);
#endif
      fontname = (char *) XSTRING (XCONS (font_names)->car)->data;
    }
  else if (w32_strict_fontnames)
    {
      /* If EnumFontFamiliesEx was available, we got a full list of
         fonts back so stop now to avoid the possibility of loading a
         random font.  If we had to fall back to EnumFontFamilies, the
         list is incomplete, so continue whether the font we want was
         listed or not. */
      HMODULE gdi32 = GetModuleHandle ("gdi32.dll");
      FARPROC enum_font_families_ex
        = GetProcAddress (gdi32, "EnumFontFamiliesExA");
      if (enum_font_families_ex)
        return NULL;
    }

  /* Load the font and add it to the table. */
  {
    char *full_name, *encoding;
    XFontStruct *font;
    struct font_info *fontp;
    LOGFONT lf;
    BOOL ok;

    if (!fontname || !x_to_w32_font (fontname, &lf))
      return (NULL);

    if (!*lf.lfFaceName)
        /* If no name was specified for the font, we get a random font
           from CreateFontIndirect - this is not particularly
           desirable, especially since CreateFontIndirect does not
           fill out the missing name in lf, so we never know what we
           ended up with. */
      return NULL;

    font = (XFontStruct *) xmalloc (sizeof (XFontStruct));

    /* Set bdf to NULL to indicate that this is a Windows font.  */
    font->bdf = NULL;

    BLOCK_INPUT;

    font->hfont = CreateFontIndirect (&lf);

    if (font->hfont == NULL) 
      {
	ok = FALSE;
      } 
    else 
      {
	HDC hdc;
	HANDLE oldobj;

	hdc = GetDC (dpyinfo->root_window);
	oldobj = SelectObject (hdc, font->hfont);
	ok = GetTextMetrics (hdc, &font->tm);
	SelectObject (hdc, oldobj);
	ReleaseDC (dpyinfo->root_window, hdc);

	/* [andrewi, 25-Apr-99] A number of fixed pitch fonts,
           eg. Courier New and perhaps others, report a max width which
           is larger than the average character width, at least on some
           NT systems (I don't understand why - my best guess is that it
           results from installing the CJK language packs for NT4).
           Unfortunately, this forces the redisplay code in dumpglyphs
           to draw text character by character.

	   I don't like this hack, but it seems better to force the max
	   width to match the average width if the font is marked as
	   fixed pitch, for the sake of redisplay performance.  */

	if ((font->tm.tmPitchAndFamily & TMPF_FIXED_PITCH) == 0)
	  font->tm.tmMaxCharWidth = font->tm.tmAveCharWidth;
      }

    UNBLOCK_INPUT;

    if (!ok)
      {
	w32_unload_font (dpyinfo, font);
	return (NULL);
      }

    /* Do we need to create the table?  */
    if (dpyinfo->font_table_size == 0)
      {
	dpyinfo->font_table_size = 16;
	dpyinfo->font_table
	  = (struct font_info *) xmalloc (dpyinfo->font_table_size
					  * sizeof (struct font_info));
      }
    /* Do we need to grow the table?  */
    else if (dpyinfo->n_fonts
	     >= dpyinfo->font_table_size)
      {
	dpyinfo->font_table_size *= 2;
	dpyinfo->font_table
	  = (struct font_info *) xrealloc (dpyinfo->font_table,
					   (dpyinfo->font_table_size
					    * sizeof (struct font_info)));
      }

    fontp = dpyinfo->font_table + dpyinfo->n_fonts;

    /* Now fill in the slots of *FONTP.  */
    BLOCK_INPUT;
    fontp->font = font;
    fontp->font_idx = dpyinfo->n_fonts;
    fontp->name = (char *) xmalloc (strlen (fontname) + 1);
    bcopy (fontname, fontp->name, strlen (fontname) + 1);

    /* Work out the font's full name.  */
    full_name = (char *)xmalloc (100);
    if (full_name && w32_to_x_font (&lf, full_name, 100))
        fontp->full_name = full_name;
    else
      {
        /* If all else fails - just use the name we used to load it.  */
        xfree (full_name);
        fontp->full_name = fontp->name;
      }

    fontp->size = FONT_WIDTH (font);
    fontp->height = FONT_HEIGHT (font);

    /* The slot `encoding' specifies how to map a character
       code-points (0x20..0x7F or 0x2020..0x7F7F) of each charset to
       the font code-points (0:0x20..0x7F, 1:0xA0..0xFF, 0:0x2020..0x7F7F,
       the font code-points (0:0x20..0x7F, 1:0xA0..0xFF,
       0:0x2020..0x7F7F, 1:0xA0A0..0xFFFF, 3:0x20A0..0x7FFF, or
       2:0xA020..0xFF7F).  For the moment, we don't know which charset
       uses this font.  So, we set informatoin in fontp->encoding[1]
       which is never used by any charset.  If mapping can't be
       decided, set FONT_ENCODING_NOT_DECIDED.  */

    /* SJIS fonts need to be set to type 4, all others seem to work as
       type FONT_ENCODING_NOT_DECIDED.  */
    encoding = strrchr (fontp->name, '-');
    if (encoding && stricmp (encoding+1, "sjis") == 0)
      fontp->encoding[1] = 4;
    else
      fontp->encoding[1] = FONT_ENCODING_NOT_DECIDED;

    /* The following three values are set to 0 under W32, which is
       what they get set to if XGetFontProperty fails under X.  */
    fontp->baseline_offset = 0;
    fontp->relative_compose = 0;
    fontp->default_ascent = 0;

    UNBLOCK_INPUT;
    dpyinfo->n_fonts++;

    return fontp;
  }
}

/* Load font named FONTNAME of size SIZE for frame F, and return a
   pointer to the structure font_info while allocating it dynamically.
   If loading fails, return NULL. */
struct font_info *
w32_load_font (f,fontname,size)
struct frame *f;
char * fontname;
int size;
{
  Lisp_Object bdf_fonts;
  struct font_info *retval = NULL;

  bdf_fonts = w32_list_bdf_fonts (build_string (fontname));

  while (!retval && CONSP (bdf_fonts))
    {
      char *bdf_name, *bdf_file;
      Lisp_Object bdf_pair;

      bdf_name = XSTRING (XCONS (bdf_fonts)->car)->data;
      bdf_pair = Fassoc (XCONS (bdf_fonts)->car, Vw32_bdf_filename_alist);
      bdf_file = XSTRING (XCONS (bdf_pair)->cdr)->data;

      retval = w32_load_bdf_font (f, bdf_name, size, bdf_file);

      bdf_fonts = XCONS (bdf_fonts)->cdr;
    }

  if (retval)
    return retval;

  return w32_load_system_font(f, fontname, size);
}


void 
w32_unload_font (dpyinfo, font)
     struct w32_display_info *dpyinfo;
     XFontStruct * font;
{
  if (font) 
    {
      if (font->bdf) w32_free_bdf_font (font->bdf);

      if (font->hfont) DeleteObject(font->hfont);
      xfree (font);
    }
}

/* The font conversion stuff between x and w32 */

/* X font string is as follows (from faces.el)
 * (let ((- 		"[-?]")
 *      (foundry	"[^-]+")
 *      (family 	"[^-]+")
 *      (weight		"\\(bold\\|demibold\\|medium\\)")		; 1
 *      (weight\?	"\\([^-]*\\)")					; 1
 *      (slant		"\\([ior]\\)")					; 2
 *      (slant\?	"\\([^-]?\\)")					; 2
 *      (swidth		"\\([^-]*\\)")					; 3
 *      (adstyle	"[^-]*")					; 4
 *      (pixelsize	"[0-9]+")
 *      (pointsize	"[0-9][0-9]+")
 *      (resx		"[0-9][0-9]+")
 *      (resy		"[0-9][0-9]+")
 *      (spacing	"[cmp?*]")
 *      (avgwidth	"[0-9]+")
 *      (registry	"[^-]+")
 *      (encoding	"[^-]+")
 *      )
 *  (setq x-font-regexp
 *	(concat "\\`\\*?[-?*]"
 *		foundry - family - weight\? - slant\? - swidth - adstyle -
 *		pixelsize - pointsize - resx - resy - spacing - registry -
 *		encoding "[-?*]\\*?\\'"
 *		))
 *  (setq x-font-regexp-head
 *	(concat "\\`[-?*]" foundry - family - weight\? - slant\?
 *		"\\([-*?]\\|\\'\\)"))
 *  (setq x-font-regexp-slant (concat - slant -))
 *  (setq x-font-regexp-weight (concat - weight -))
 * nil)	    
 */
    
#define FONT_START       "[-?]"
#define FONT_FOUNDRY     "[^-]+"
#define FONT_FAMILY      "\\([^-]+\\)"                      /* 1 */
#define FONT_WEIGHT      "\\(bold\\|demibold\\|medium\\)"   /* 2 */
#define FONT_WEIGHT_Q    "\\([^-]*\\)"                      /* 2 */
#define FONT_SLANT       "\\([ior]\\)"                      /* 3 */
#define FONT_SLANT_Q     "\\([^-]?\\)"                      /* 3 */
#define FONT_SWIDTH      "\\([^-]*\\)"                      /* 4 */
#define FONT_ADSTYLE     "[^-]*"
#define FONT_PIXELSIZE   "[^-]*"
#define FONT_POINTSIZE   "\\([0-9][0-9]+\\|\\*\\)"          /* 5 */
#define FONT_RESX        "[0-9][0-9]+"
#define FONT_RESY        "[0-9][0-9]+"
#define FONT_SPACING     "[cmp?*]"
#define FONT_AVGWIDTH    "[0-9]+"
#define FONT_REGISTRY    "[^-]+"
#define FONT_ENCODING    "[^-]+"

#define FONT_REGEXP      ("\\`\\*?[-?*]"     \
			  FONT_FOUNDRY   "-" \
			  FONT_FAMILY    "-" \
			  FONT_WEIGHT_Q  "-" \
			  FONT_SLANT_Q   "-" \
			  FONT_SWIDTH    "-" \
			  FONT_ADSTYLE   "-" \
			  FONT_PIXELSIZE "-" \
			  FONT_POINTSIZE "-" \
			  "[-?*]\\|\\'")

#define FONT_REGEXP_HEAD ("\\`[-?*]"        \
			  FONT_FOUNDRY  "-" \
			  FONT_FAMILY   "-" \
			  FONT_WEIGHT_Q "-" \
			  FONT_SLANT_Q      \
			  "\\([-*?]\\|\\'\\)")

#define FONT_REGEXP_SLANT  "-" FONT_SLANT  "-"
#define FONT_REGEXP_WEIGHT "-" FONT_WEIGHT "-"

LONG 
x_to_w32_weight (lpw)
     char * lpw;
{
  if (!lpw) return (FW_DONTCARE);

  if (stricmp (lpw,"heavy") == 0)             return FW_HEAVY;
  else if (stricmp (lpw,"extrabold") == 0)    return FW_EXTRABOLD;
  else if (stricmp (lpw,"bold") == 0)         return FW_BOLD;
  else if (stricmp (lpw,"demibold") == 0)     return FW_SEMIBOLD;
  else if (stricmp (lpw,"semibold") == 0)     return FW_SEMIBOLD;
  else if (stricmp (lpw,"medium") == 0)       return FW_MEDIUM;
  else if (stricmp (lpw,"normal") == 0)       return FW_NORMAL;
  else if (stricmp (lpw,"light") == 0)        return FW_LIGHT;
  else if (stricmp (lpw,"extralight") == 0)   return FW_EXTRALIGHT;
  else if (stricmp (lpw,"thin") == 0)         return FW_THIN;
  else
    return FW_DONTCARE;
}


char * 
w32_to_x_weight (fnweight)
     int fnweight;
{
  if (fnweight >= FW_HEAVY)      return "heavy";
  if (fnweight >= FW_EXTRABOLD)  return "extrabold";
  if (fnweight >= FW_BOLD)       return "bold";
  if (fnweight >= FW_SEMIBOLD)   return "demibold";
  if (fnweight >= FW_MEDIUM)     return "medium";
  if (fnweight >= FW_NORMAL)     return "normal";
  if (fnweight >= FW_LIGHT)      return "light";
  if (fnweight >= FW_EXTRALIGHT) return "extralight";
  if (fnweight >= FW_THIN)       return "thin";
  else
    return "*";
}

LONG
x_to_w32_charset (lpcs)
    char * lpcs;
{
  if (!lpcs) return (0);

  if (stricmp (lpcs,"ansi") == 0)                return ANSI_CHARSET;
  else if (stricmp (lpcs,"iso8859-1") == 0)      return ANSI_CHARSET;
  else if (stricmp (lpcs, "ms-symbol") == 0)     return SYMBOL_CHARSET;
  /* Map all Japanese charsets to the Windows Shift-JIS charset.  */
  else if (strnicmp (lpcs, "jis", 3) == 0)       return SHIFTJIS_CHARSET;
  /* Map all GB charsets to the Windows GB2312 charset.  */
  else if (strnicmp (lpcs, "gb2312", 6) == 0)    return GB2312_CHARSET;
  /* Map all Big5 charsets to the Windows Big5 charset.  */
  else if (strnicmp (lpcs, "big5", 4) == 0)      return CHINESEBIG5_CHARSET;
  else if (stricmp (lpcs, "ksc5601.1987") == 0)  return HANGEUL_CHARSET;
  else if (stricmp (lpcs, "ms-oem") == 0)	 return OEM_CHARSET;

#ifdef EASTEUROPE_CHARSET
  else if (stricmp (lpcs, "iso8859-2") == 0)     return EASTEUROPE_CHARSET;
  else if (stricmp (lpcs, "iso8859-3") == 0)     return TURKISH_CHARSET;
  else if (stricmp (lpcs, "iso8859-4") == 0)     return BALTIC_CHARSET;
  else if (stricmp (lpcs, "iso8859-5") == 0)     return RUSSIAN_CHARSET;
  else if (stricmp (lpcs, "koi8") == 0)          return RUSSIAN_CHARSET;
  else if (stricmp (lpcs, "iso8859-6") == 0)     return ARABIC_CHARSET;
  else if (stricmp (lpcs, "iso8859-7") == 0)     return GREEK_CHARSET;
  else if (stricmp (lpcs, "iso8859-8") == 0)     return HEBREW_CHARSET;
  else if (stricmp (lpcs, "iso8859-9") == 0)     return TURKISH_CHARSET;
#ifndef VIETNAMESE_CHARSET
#define VIETNAMESE_CHARSET 163
#endif
  /* Map all Viscii charsets to the Windows Vietnamese charset.  */
  else if (strnicmp (lpcs, "viscii", 6) == 0)    return VIETNAMESE_CHARSET;
  else if (strnicmp (lpcs, "vscii", 5) == 0)     return VIETNAMESE_CHARSET;
  /* Map all TIS charsets to the Windows Thai charset.  */
  else if (strnicmp (lpcs, "tis620", 6) == 0)    return THAI_CHARSET;
  else if (stricmp (lpcs, "mac") == 0)           return MAC_CHARSET;
  else if (stricmp (lpcs, "ksc5601.1992") == 0)  return JOHAB_CHARSET;
  /* For backwards compatibility with previous 20.4 pretests, map
     non-specific KSC charsets to the Windows Hangeul charset.  */
  else if (strnicmp (lpcs, "ksc5601", 7) == 0)   return HANGEUL_CHARSET;
  else if (stricmp (lpcs, "johab") == 0)         return JOHAB_CHARSET;
#endif

#ifdef UNICODE_CHARSET
  else if (stricmp (lpcs,"iso10646") == 0)       return UNICODE_CHARSET;
  else if (stricmp (lpcs, "unicode") == 0)       return UNICODE_CHARSET;
#endif
  else if (lpcs[0] == '#')			 return atoi (lpcs + 1);
  else
    return DEFAULT_CHARSET;
}

char *
w32_to_x_charset (fncharset)
    int fncharset;
{
  static char buf[16];

  switch (fncharset)
    {
      /* ansi is considered iso8859-1, as most modern ansi fonts are.  */
    case ANSI_CHARSET:        return "iso8859-1";
    case DEFAULT_CHARSET:     return "ascii-*";
    case SYMBOL_CHARSET:      return "ms-symbol";
    case SHIFTJIS_CHARSET:    return "jisx0208-sjis";
    case HANGEUL_CHARSET:     return "ksc5601.1987-*";
    case GB2312_CHARSET:      return "gb2312-*";
    case CHINESEBIG5_CHARSET: return "big5-*";
    case OEM_CHARSET:         return "ms-oem";

      /* More recent versions of Windows (95 and NT4.0) define more
         character sets.  */
#ifdef EASTEUROPE_CHARSET
    case EASTEUROPE_CHARSET: return "iso8859-2";
    case TURKISH_CHARSET:    return "iso8859-9";
    case BALTIC_CHARSET:     return "iso8859-4";

      /* W95 with international support but not IE4 often has the
         KOI8-R codepage but not ISO8859-5.  */
    case RUSSIAN_CHARSET:
      if (!IsValidCodePage(28595) && IsValidCodePage(20886))
        return "koi8-r";
      else
        return "iso8859-5";
    case ARABIC_CHARSET:     return "iso8859-6";
    case GREEK_CHARSET:      return "iso8859-7";
    case HEBREW_CHARSET:     return "iso8859-8";
    case VIETNAMESE_CHARSET: return "viscii1.1-*";
    case THAI_CHARSET:       return "tis620-*";
    case MAC_CHARSET:        return "mac-*";
    case JOHAB_CHARSET:      return "ksc5601.1992-*";

#endif

#ifdef UNICODE_CHARSET
    case UNICODE_CHARSET:  return "iso10646-unicode";
#endif
    }
  /* Encode numerical value of unknown charset.  */
  sprintf (buf, "*-#%u", fncharset);
  return buf;
}

BOOL 
w32_to_x_font (lplogfont, lpxstr, len)
     LOGFONT * lplogfont;
     char * lpxstr;
     int len;
{
  char *fontname;
  char height_pixels[8];
  char height_dpi[8];
  char width_pixels[8];
  char *fontname_dash;
  int display_resy = one_w32_display_info.height_in;
  int display_resx = one_w32_display_info.width_in;
  int bufsz;
  struct coding_system coding;

  if (!lpxstr) abort ();

  if (!lplogfont)
    return FALSE;

  setup_coding_system (Fcheck_coding_system (Vw32_system_coding_system),
                       &coding);
  coding.mode |= CODING_MODE_LAST_BLOCK;
  bufsz = decoding_buffer_size (&coding, LF_FACESIZE);

  fontname = alloca(sizeof(*fontname) * bufsz);
  decode_coding (&coding, lplogfont->lfFaceName, fontname,
                 strlen(lplogfont->lfFaceName), bufsz - 1);
  *(fontname + coding.produced) = '\0';

  /* Replace dashes with underscores so the dashes are not
     misinterpreted.  */
  fontname_dash = fontname;
  while (fontname_dash = strchr (fontname_dash, '-'))
      *fontname_dash = '_';

  if (lplogfont->lfHeight)
    {
      sprintf (height_pixels, "%u", abs (lplogfont->lfHeight));
      sprintf (height_dpi, "%u",
	       abs (lplogfont->lfHeight) * 720 / display_resy);
    }
  else
    {
      strcpy (height_pixels, "*");
      strcpy (height_dpi, "*");
    }
  if (lplogfont->lfWidth)
    sprintf (width_pixels, "%u", lplogfont->lfWidth * 10);
  else
    strcpy (width_pixels, "*");

  _snprintf (lpxstr, len - 1,
	     "-*-%s-%s-%c-*-*-%s-%s-%d-%d-%c-%s-%s",
                                                     /* foundry */
	     fontname,                               /* family */
	     w32_to_x_weight (lplogfont->lfWeight),  /* weight */
	     lplogfont->lfItalic?'i':'r',            /* slant */
                                                     /* setwidth name */
                                                     /* add style name */
	     height_pixels,                          /* pixel size */
	     height_dpi,                             /* point size */
             display_resx,                           /* resx */
             display_resy,                           /* resy */
	     ((lplogfont->lfPitchAndFamily & 0x3) == VARIABLE_PITCH)
             ? 'p' : 'c',                            /* spacing */
	     width_pixels,                           /* avg width */
	     w32_to_x_charset (lplogfont->lfCharSet) /* charset registry
                                                        and encoding*/
	     );

  lpxstr[len - 1] = 0;		/* just to be sure */
  return (TRUE);
}

BOOL 
x_to_w32_font (lpxstr, lplogfont)
     char * lpxstr;
     LOGFONT * lplogfont;
{
  struct coding_system coding;

  if (!lplogfont) return (FALSE);

  memset (lplogfont, 0, sizeof (*lplogfont));

  /* Set default value for each field.  */
#if 1
  lplogfont->lfOutPrecision = OUT_DEFAULT_PRECIS;
  lplogfont->lfClipPrecision = CLIP_DEFAULT_PRECIS;
  lplogfont->lfQuality = DEFAULT_QUALITY;
#else
  /* go for maximum quality */
  lplogfont->lfOutPrecision = OUT_STROKE_PRECIS;
  lplogfont->lfClipPrecision = CLIP_STROKE_PRECIS;
  lplogfont->lfQuality = PROOF_QUALITY;
#endif

  lplogfont->lfCharSet = DEFAULT_CHARSET;
  lplogfont->lfWeight = FW_DONTCARE;
  lplogfont->lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;

  if (!lpxstr)
    return FALSE;

  /* Provide a simple escape mechanism for specifying Windows font names
   * directly -- if font spec does not beginning with '-', assume this
   * format:
   *   "<font name>[:height in pixels[:width in pixels[:weight]]]"
   */
  
  if (*lpxstr == '-')
    {
      int fields, tem;
      char name[50], weight[20], slant, pitch, pixels[10], height[10],
        width[10], resy[10], remainder[20];
      char * encoding;
      int dpi = one_w32_display_info.height_in;

      fields = sscanf (lpxstr,
		       "-%*[^-]-%49[^-]-%19[^-]-%c-%*[^-]-%*[^-]-%9[^-]-%9[^-]-%*[^-]-%9[^-]-%c-%9[^-]-%19s",
		       name, weight, &slant, pixels, height, resy, &pitch, width, remainder);
      if (fields == EOF) return (FALSE);

      if (fields > 0 && name[0] != '*')
        {
	  int bufsize;
	  unsigned char *buf;

          setup_coding_system
            (Fcheck_coding_system (Vw32_system_coding_system), &coding);
	  bufsize = encoding_buffer_size (&coding, strlen (name));
	  buf = (unsigned char *) alloca (bufsize);
          coding.mode |= CODING_MODE_LAST_BLOCK;
          encode_coding (&coding, name, buf, strlen (name), bufsize);
	  if (coding.produced >= LF_FACESIZE)
	    coding.produced = LF_FACESIZE - 1;
	  buf[coding.produced] = 0;
	  strcpy (lplogfont->lfFaceName, buf);
	}
      else
        {
	  lplogfont->lfFaceName[0] = 0;
	}

      fields--;

      lplogfont->lfWeight = x_to_w32_weight ((fields > 0 ? weight : ""));

      fields--;

      if (!NILP (Vw32_enable_italics))
	lplogfont->lfItalic = (fields > 0 && slant == 'i');

      fields--;

      if (fields > 0 && pixels[0] != '*')
	lplogfont->lfHeight = atoi (pixels);

      fields--;
      fields--;
      if (fields > 0 && resy[0] != '*')
        {
          tem = atoi (pixels);
          if (tem > 0) dpi = tem;
        }

      if (fields > -1 && lplogfont->lfHeight == 0 && height[0] != '*')
	lplogfont->lfHeight = atoi (height) * dpi / 720;

      if (fields > 0)
      lplogfont->lfPitchAndFamily =
	(fields > 0 && pitch == 'p') ? VARIABLE_PITCH : FIXED_PITCH;

      fields--;

      if (fields > 0 && width[0] != '*')
	lplogfont->lfWidth = atoi (width) / 10;

      fields--;

      /* Strip the trailing '-' if present. (it shouldn't be, as it
         fails the test against xlfn-tight-regexp in fontset.el).  */
      {
	int len = strlen (remainder);
	if (len > 0 && remainder[len-1] == '-')
	  remainder[len-1] = 0;
      }
      encoding = remainder;
      if (strncmp (encoding, "*-", 2) == 0)
	encoding += 2;
      lplogfont->lfCharSet = x_to_w32_charset (fields > 0 ? encoding : "");
    }
  else
    {
      int fields;
      char name[100], height[10], width[10], weight[20];

      fields = sscanf (lpxstr,
		       "%99[^:]:%9[^:]:%9[^:]:%19s",
		       name, height, width, weight);

      if (fields == EOF) return (FALSE);

      if (fields > 0)
        {
	  strncpy (lplogfont->lfFaceName,name, LF_FACESIZE);
	  lplogfont->lfFaceName[LF_FACESIZE-1] = 0;
	}
      else
        {
	  lplogfont->lfFaceName[0] = 0;
	}

      fields--;

      if (fields > 0)
	lplogfont->lfHeight = atoi (height);

      fields--;

      if (fields > 0)
	lplogfont->lfWidth = atoi (width);

      fields--;

      lplogfont->lfWeight = x_to_w32_weight ((fields > 0 ? weight : ""));
    }

  /* This makes TrueType fonts work better. */
  lplogfont->lfHeight = - abs (lplogfont->lfHeight);
  
  return (TRUE);
}

BOOL 
w32_font_match (lpszfont1, lpszfont2)
    char * lpszfont1;
    char * lpszfont2;
{
  char * s1 = lpszfont1, *e1, *w1;
  char * s2 = lpszfont2, *e2, *w2;
  
  if (s1 == NULL || s2 == NULL) return (FALSE);
  
  if (*s1 == '-') s1++;
  if (*s2 == '-') s2++;
  
  while (1) 
    {
      int len1, len2, len3=0;

      e1 = strchr (s1, '-');
      e2 = strchr (s2, '-');
      w1 = strchr (s1, '*');
      w2 = strchr (s2, '*');

      if (e1 == NULL)
        len1 = strlen (s1);
      else
      len1 = e1 - s1;
      if (e2 == NULL)
        len2 = strlen (s1);
      else
      len2 = e2 - s2;

      if (w1 && w1 < e1)
        len3 = w1 - s1;
      if (w2 && w2 < e2 && ( len3 == 0 || (w2 - s2) < len3))
        len3 = w2 - s2;

      /* Whole field is not a wildcard, and ...*/
      if (*s1 != '*' && *s2 != '*' && *s1 != '-' && *s2 != '-'
          /* Lengths are different and there are no wildcards, or ... */
	  && ((len1 != len2 && len3 == 0) ||
              /* strings don't match up until first wildcard or end.  */
              strnicmp (s1, s2, len3 > 0 ? len3 : len1) != 0))
	return (FALSE);

      if (e1 == NULL || e2 == NULL)
        return (TRUE);

      s1 = e1 + 1;
      s2 = e2 + 1;
    }
}

/* Callback functions, and a structure holding info they need, for
   listing system fonts on W32. We need one set of functions to do the
   job properly, but these don't work on NT 3.51 and earlier, so we
   have a second set which don't handle character sets properly to
   fall back on.

   In both cases, there are two passes made. The first pass gets one
   font from each family, the second pass lists all the fonts from
   each family.  */

typedef struct enumfont_t 
{
  HDC hdc;
  int numFonts;
  LOGFONT logfont;
  XFontStruct *size_ref;
  Lisp_Object *pattern;
  Lisp_Object *tail;
} enumfont_t;

int CALLBACK 
enum_font_cb2 (lplf, lptm, FontType, lpef)
    ENUMLOGFONT * lplf;
    NEWTEXTMETRIC * lptm;
    int FontType;
    enumfont_t * lpef;
{
  if (lplf->elfLogFont.lfStrikeOut || lplf->elfLogFont.lfUnderline)
    return (1);
  
  /* Check that the character set matches if it was specified */
  if (lpef->logfont.lfCharSet != DEFAULT_CHARSET &&
      lplf->elfLogFont.lfCharSet != lpef->logfont.lfCharSet)
    return (1);

  /* We want all fonts cached, so don't compare sizes just yet */
  /*    if (!lpef->size_ref || lptm->tmMaxCharWidth == FONT_WIDTH (lpef->size_ref)) */
  {
    char buf[100];
    Lisp_Object width = Qnil;

    if (!NILP (*(lpef->pattern)) && FontType != RASTER_FONTTYPE)
      {
        /* Scalable fonts are as big as you want them to be.  */
	lplf->elfLogFont.lfHeight = lpef->logfont.lfHeight;
	lplf->elfLogFont.lfWidth = lpef->logfont.lfWidth;
      }
    /* Make sure the height used here is the same as everywhere
       else (ie character height, not cell height).  */
    else if (lplf->elfLogFont.lfHeight > 0)
      lplf->elfLogFont.lfHeight = lptm->tmInternalLeading - lptm->tmHeight;

    /* The MaxCharWidth is not valid at this stage for scalable fonts. */
    if (FontType == RASTER_FONTTYPE)
        width = make_number (lptm->tmMaxCharWidth);

    if (!w32_to_x_font (&(lplf->elfLogFont), buf, 100))
      return (0);

    if (NILP (*(lpef->pattern))
        || w32_font_match (buf, XSTRING (*(lpef->pattern))->data))
      {
	*lpef->tail = Fcons (Fcons (build_string (buf), width), Qnil);
	lpef->tail = &(XCONS (*lpef->tail)->cdr);
	lpef->numFonts++;
      }
  }
  
  return (1);
}

int CALLBACK 
enum_font_cb1 (lplf, lptm, FontType, lpef)
     ENUMLOGFONT * lplf;
     NEWTEXTMETRIC * lptm;
     int FontType;
     enumfont_t * lpef;
{
  return EnumFontFamilies (lpef->hdc,
			   lplf->elfLogFont.lfFaceName,
			   (FONTENUMPROC) enum_font_cb2,
			   (LPARAM) lpef);
}


int CALLBACK
enum_fontex_cb2 (lplf, lptm, font_type, lpef)
     ENUMLOGFONTEX * lplf;
     NEWTEXTMETRICEX * lptm;
     int font_type;
     enumfont_t * lpef;
{
  /* We are not interested in the extra info we get back from the 'Ex
     version - only the fact that we get character set variations
     enumerated seperately.  */
  return enum_font_cb2 ((ENUMLOGFONT *) lplf, (NEWTEXTMETRIC *) lptm,
                        font_type, lpef);
}

int CALLBACK
enum_fontex_cb1 (lplf, lptm, font_type, lpef)
     ENUMLOGFONTEX * lplf;
     NEWTEXTMETRICEX * lptm;
     int font_type;
     enumfont_t * lpef;
{
  HMODULE gdi32 = GetModuleHandle ("gdi32.dll");
  FARPROC enum_font_families_ex
    = GetProcAddress ( gdi32, "EnumFontFamiliesExA");
  /* We don't really expect EnumFontFamiliesEx to disappear once we
     get here, so don't bother handling it gracefully.  */
  if (enum_font_families_ex == NULL)
    error ("gdi32.dll has disappeared!");
  return enum_font_families_ex (lpef->hdc,
                                &lplf->elfLogFont,
                                (FONTENUMPROC) enum_fontex_cb2,
                                (LPARAM) lpef, 0);
}

/* Interface to fontset handler. (adapted from mw32font.c in Meadow
   and xterm.c in Emacs 20.3) */

Lisp_Object w32_list_bdf_fonts (Lisp_Object pattern, int max_names)
{
  char *fontname, *ptnstr;
  Lisp_Object list, tem, newlist = Qnil;
  int n_fonts = 0;

  list = Vw32_bdf_filename_alist;
  ptnstr = XSTRING (pattern)->data;

  for ( ; CONSP (list); list = XCONS (list)->cdr)
    {
      tem = XCONS (list)->car;
      if (CONSP (tem))
        fontname = XSTRING (XCONS (tem)->car)->data;
      else if (STRINGP (tem))
        fontname = XSTRING (tem)->data;
      else
        continue;

      if (w32_font_match (fontname, ptnstr))
        {
          newlist = Fcons (XCONS (tem)->car, newlist);
          n_fonts++;
          if (n_fonts >= max_names)
            break;
        }
    }

  return newlist;
}

Lisp_Object w32_list_synthesized_fonts (FRAME_PTR f, Lisp_Object pattern,
                                        int size, int max_names);

/* Return a list of names of available fonts matching PATTERN on frame
   F.  If SIZE is not 0, it is the size (maximum bound width) of fonts
   to be listed.  Frame F NULL means we have not yet created any
   frame, which means we can't get proper size info, as we don't have
   a device context to use for GetTextMetrics.
   MAXNAMES sets a limit on how many fonts to match.  */

Lisp_Object
w32_list_fonts (FRAME_PTR f, Lisp_Object pattern, int size, int maxnames )
{
  Lisp_Object patterns, key, tem, tpat;
  Lisp_Object list = Qnil, newlist = Qnil, second_best = Qnil;
  struct w32_display_info *dpyinfo = &one_w32_display_info;
  int n_fonts = 0;

  patterns = Fassoc (pattern, Valternate_fontname_alist);
  if (NILP (patterns))
    patterns = Fcons (pattern, Qnil);

  for (; CONSP (patterns); patterns = XCONS (patterns)->cdr)
    {
      enumfont_t ef;

      tpat = XCONS (patterns)->car;

      /* See if we cached the result for this particular query.
         The cache is an alist of the form:
           ((PATTERN (FONTNAME . WIDTH) ...) ...)
      */
      if (tem = XCONS (dpyinfo->name_list_element)->cdr,
          !NILP (list = Fassoc (tpat, tem)))
        {
          list = Fcdr_safe (list);
          /* We have a cached list. Don't have to get the list again.  */
          goto label_cached;
        }

      BLOCK_INPUT;
      /* At first, put PATTERN in the cache.  */
      list = Qnil;
      ef.pattern = &tpat;
      ef.tail = &list;
      ef.numFonts = 0;

      /* Use EnumFontFamiliesEx where it is available, as it knows
         about character sets.  Fall back to EnumFontFamilies for
         older versions of NT that don't support the 'Ex function.  */
      x_to_w32_font (STRINGP (tpat) ? XSTRING (tpat)->data :
                     NULL, &ef.logfont);
      {
        LOGFONT font_match_pattern;
        HMODULE gdi32 = GetModuleHandle ("gdi32.dll");
        FARPROC enum_font_families_ex
          = GetProcAddress ( gdi32, "EnumFontFamiliesExA");

        /* We do our own pattern matching so we can handle wildcards.  */
        font_match_pattern.lfFaceName[0] = 0;
        font_match_pattern.lfPitchAndFamily = 0;
        /* We can use the charset, because if it is a wildcard it will
           be DEFAULT_CHARSET anyway.  */
        font_match_pattern.lfCharSet = ef.logfont.lfCharSet;

        ef.hdc = GetDC (dpyinfo->root_window);

        if (enum_font_families_ex)
          enum_font_families_ex (ef.hdc,
                                 &font_match_pattern,
                                 (FONTENUMPROC) enum_fontex_cb1,
                                 (LPARAM) &ef, 0);
        else
          EnumFontFamilies (ef.hdc, NULL, (FONTENUMPROC) enum_font_cb1,
                            (LPARAM)&ef);

        ReleaseDC (dpyinfo->root_window, ef.hdc);
      }

      UNBLOCK_INPUT;

      /* Make a list of the fonts we got back.
         Store that in the font cache for the display. */
      XCONS (dpyinfo->name_list_element)->cdr
        = Fcons (Fcons (tpat, list),
                 XCONS (dpyinfo->name_list_element)->cdr);

    label_cached:
      if (NILP (list)) continue; /* Try the remaining alternatives.  */

      newlist = second_best = Qnil;

      /* Make a list of the fonts that have the right width.  */          
      for (; CONSP (list); list = XCONS (list)->cdr)
        {
          int found_size;
          tem = XCONS (list)->car;

          if (!CONSP (tem))
            continue;
          if (NILP (XCONS (tem)->car))
            continue;
          if (!size)
            {
              newlist = Fcons (XCONS (tem)->car, newlist);
              n_fonts++;
              if (n_fonts >= maxnames)
                break;
              else
                continue;
            }
          if (!INTEGERP (XCONS (tem)->cdr))
            {
              /* Since we don't yet know the size of the font, we must
                 load it and try GetTextMetrics.  */
              W32FontStruct thisinfo;
              LOGFONT lf;
              HDC hdc;
              HANDLE oldobj;

              if (!x_to_w32_font (XSTRING (XCONS (tem)->car)->data, &lf))
                continue;

              BLOCK_INPUT;
              thisinfo.bdf = NULL;
              thisinfo.hfont = CreateFontIndirect (&lf);
              if (thisinfo.hfont == NULL)
                continue;

              hdc = GetDC (dpyinfo->root_window);
              oldobj = SelectObject (hdc, thisinfo.hfont);
              if (GetTextMetrics (hdc, &thisinfo.tm))
                XCONS (tem)->cdr = make_number (FONT_WIDTH (&thisinfo));
              else
                XCONS (tem)->cdr = make_number (0);
              SelectObject (hdc, oldobj);
              ReleaseDC (dpyinfo->root_window, hdc);
              DeleteObject(thisinfo.hfont);
              UNBLOCK_INPUT;
            }
          found_size = XINT (XCONS (tem)->cdr);
          if (found_size == size)
            {
              newlist = Fcons (XCONS (tem)->car, newlist);
              n_fonts++;
              if (n_fonts >= maxnames)
                break;
            }
          /* keep track of the closest matching size in case
             no exact match is found.  */
          else if (found_size > 0)
            {
              if (NILP (second_best))
                second_best = tem;
                  
              else if (found_size < size)
                {
                  if (XINT (XCONS (second_best)->cdr) > size
                      || XINT (XCONS (second_best)->cdr) < found_size)
                    second_best = tem;
                }
              else
                {
                  if (XINT (XCONS (second_best)->cdr) > size
                      && XINT (XCONS (second_best)->cdr) >
                      found_size)
                    second_best = tem;
                }
            }
        }

      if (!NILP (newlist))
        break;
      else if (!NILP (second_best))
        {
          newlist = Fcons (XCONS (second_best)->car, Qnil);
          break;
        }
    }

  /* Include any bdf fonts.  */
  if (n_fonts < maxnames)
  {
    Lisp_Object combined[2];
    combined[0] = w32_list_bdf_fonts (pattern, maxnames - n_fonts);
    combined[1] = newlist;
    newlist = Fnconc(2, combined);
  }

  /* If we can't find a font that matches, check if Windows would be
     able to synthesize it from a different style.  */
  if (NILP (newlist) && !NILP (Vw32_enable_italics))
    newlist = w32_list_synthesized_fonts (f, pattern, size, maxnames);

  return newlist;
}

Lisp_Object
w32_list_synthesized_fonts (f, pattern, size, max_names)
     FRAME_PTR f;
     Lisp_Object pattern;
     int size;
     int max_names;
{
  int fields;
  char *full_pattn, *new_pattn, foundary[50], family[50], *pattn_part2;
  char style[20], slant;
  Lisp_Object matches, match, tem, synthed_matches = Qnil;

  full_pattn = XSTRING (pattern)->data;

  pattn_part2 = alloca (XSTRING (pattern)->size);
  /* Allow some space for wildcard expansion.  */
  new_pattn = alloca (XSTRING (pattern)->size + 100);

  fields = sscanf (full_pattn, "-%49[^-]-%49[^-]-%19[^-]-%c-%s",
                   foundary, family, style, &slant, pattn_part2);
  if (fields == EOF || fields < 5)
    return Qnil;

  /* If the style and slant are wildcards already there is no point
     checking again (and we don't want to keep recursing).  */
  if (*style == '*' && slant == '*')
    return Qnil;

  sprintf (new_pattn, "-%s-%s-*-*-%s", foundary, family, pattn_part2);

  matches = w32_list_fonts (f, build_string (new_pattn), size, max_names);

  for ( ; CONSP (matches); matches = XCONS (matches)->cdr)
    {
      tem = XCONS (matches)->car;
      if (!STRINGP (tem))
        continue;

      full_pattn = XSTRING (tem)->data;
      fields = sscanf (full_pattn, "-%49[^-]-%49[^-]-%*[^-]-%*c-%s",
                       foundary, family, pattn_part2);
      if (fields == EOF || fields < 3)
        continue;

      sprintf (new_pattn, "-%s-%s-%s-%c-%s", foundary, family, style,
               slant, pattn_part2);

      synthed_matches = Fcons (build_string (new_pattn),
                               synthed_matches);
    }

  return synthed_matches;
}


/* Return a pointer to struct font_info of font FONT_IDX of frame F.  */
struct font_info *
w32_get_font_info (f, font_idx)
     FRAME_PTR f;
     int font_idx;
{
  return (FRAME_W32_FONT_TABLE (f) + font_idx);
}


struct font_info*
w32_query_font (struct frame *f, char *fontname)
{
  int i;
  struct font_info *pfi;

  pfi = FRAME_W32_FONT_TABLE (f);

  for (i = 0; i < one_w32_display_info.n_fonts ;i++, pfi++)
    {
      if (strcmp(pfi->name, fontname) == 0) return pfi;
    }

  return NULL;
}

/* Find a CCL program for a font specified by FONTP, and set the member
 `encoder' of the structure.  */

void
w32_find_ccl_program (fontp)
     struct font_info *fontp;
{
  Lisp_Object list, elt;

  for (list = Vfont_ccl_encoder_alist; CONSP (list); list = XCONS (list)->cdr)
    {
      elt = XCONS (list)->car;
      if (CONSP (elt)
	  && STRINGP (XCONS (elt)->car)
	  && (fast_c_string_match_ignore_case (XCONS (elt)->car, fontp->name)
	      >= 0))
	break;
    }
  if (! NILP (list))
    {
      struct ccl_program *ccl
	= (struct ccl_program *) xmalloc (sizeof (struct ccl_program));

      if (setup_ccl_program (ccl, XCONS (elt)->cdr) < 0)
	xfree (ccl);
      else
	fontp->font_encoder = ccl;
    }
}


#if 1
#include "x-list-font.c"
#else
DEFUN ("x-list-fonts", Fx_list_fonts, Sx_list_fonts, 1, 4, 0,
  "Return a list of the names of available fonts matching PATTERN.\n\
If optional arguments FACE and FRAME are specified, return only fonts\n\
the same size as FACE on FRAME.\n\
\n\
PATTERN is a string, perhaps with wildcard characters;\n\
  the * character matches any substring, and\n\
  the ? character matches any single character.\n\
  PATTERN is case-insensitive.\n\
FACE is a face name--a symbol.\n\
\n\
The return value is a list of strings, suitable as arguments to\n\
set-face-font.\n\
\n\
Fonts Emacs can't use (i.e. proportional fonts) may or may not be excluded\n\
even if they match PATTERN and FACE.\n\
\n\
The optional fourth argument MAXIMUM sets a limit on how many\n\
fonts to match.  The first MAXIMUM fonts are reported.")
  (pattern, face, frame, maximum)
    Lisp_Object pattern, face, frame, maximum;
{
  int num_fonts;
  char **names;
  XFontStruct *info;
  XFontStruct *size_ref;
  Lisp_Object namelist;
  Lisp_Object list;
  FRAME_PTR f;
  enumfont_t ef;

  CHECK_STRING (pattern, 0);
  if (!NILP (face))
    CHECK_SYMBOL (face, 1);

  f = check_x_frame (frame);

  /* Determine the width standard for comparison with the fonts we find.  */

  if (NILP (face))
    size_ref = 0;
  else
    {
      int face_id;

      /* Don't die if we get called with a terminal frame.  */
      if (! FRAME_W32_P (f))
	error ("non-w32 frame used in `x-list-fonts'");

      face_id = face_name_id_number (f, face);

      if (face_id < 0 || face_id >= FRAME_N_PARAM_FACES (f)
	  || FRAME_PARAM_FACES (f) [face_id] == 0)
	size_ref = f->output_data.w32->font;
      else
	{
	  size_ref = FRAME_PARAM_FACES (f) [face_id]->font;
	  if (size_ref == (XFontStruct *) (~0))
	    size_ref = f->output_data.w32->font;
	}
    }

  /* See if we cached the result for this particular query.  */
  list = Fassoc (pattern,
		 XCONS (FRAME_W32_DISPLAY_INFO (f)->name_list_element)->cdr);

  /* We have info in the cache for this PATTERN.  */
  if (!NILP (list))
    {
      Lisp_Object tem, newlist;

      /* We have info about this pattern.  */
      list = XCONS (list)->cdr;

      if (size_ref == 0)
	return list;

      BLOCK_INPUT;

      /* Filter the cached info and return just the fonts that match FACE.  */
      newlist = Qnil;
      for (tem = list; CONSP (tem); tem = XCONS (tem)->cdr)
	{
	  struct font_info *fontinf;
          XFontStruct *thisinfo = NULL;

          fontinf = w32_load_font (f, XSTRING (XCONS (tem)->car)->data, 0);
          if (fontinf)
            thisinfo = (XFontStruct *)fontinf->font;
          if (thisinfo && same_size_fonts (thisinfo, size_ref))
	    newlist = Fcons (XCONS (tem)->car, newlist);

	  w32_unload_font (FRAME_W32_DISPLAY_INFO (f), thisinfo);
        }

      UNBLOCK_INPUT;

      return newlist;
    }

  BLOCK_INPUT;

  namelist = Qnil;
  ef.pattern = &pattern;
  ef.tail &namelist;
  ef.numFonts = 0;
  x_to_w32_font (STRINGP (pattern) ? XSTRING (pattern)->data : NULL, &ef.logfont);

  {
    ef.hdc = GetDC (FRAME_W32_WINDOW (f));

    EnumFontFamilies (ef.hdc, NULL, (FONTENUMPROC) enum_font_cb1, (LPARAM)&ef);
    
    ReleaseDC  (FRAME_W32_WINDOW (f), ef.hdc);
  }

  UNBLOCK_INPUT;

  if (ef.numFonts)
    {
      int i;
      Lisp_Object cur;

      /* Make a list of all the fonts we got back.
	 Store that in the font cache for the display.  */
      XCONS (FRAME_W32_DISPLAY_INFO (f)->name_list_element)->cdr
	= Fcons (Fcons (pattern, namelist),
		 XCONS (FRAME_W32_DISPLAY_INFO (f)->name_list_element)->cdr);

      /* Make a list of the fonts that have the right width.  */
      list = Qnil;
      cur=namelist;
      for (i = 0; i < ef.numFonts; i++)
        {
	  int keeper;

	  if (!size_ref)
	    keeper = 1;
	  else
	    {
	      struct font_info *fontinf;
              XFontStruct *thisinfo = NULL;

	      BLOCK_INPUT;
	      fontinf = w32_load_font (f, XSTRING (Fcar (cur))->data, 0);
              if (fontinf)
                thisinfo = (XFontStruct *)fontinf->font;

	      keeper = thisinfo && same_size_fonts (thisinfo, size_ref);

	      w32_unload_font (FRAME_W32_DISPLAY_INFO (f), thisinfo);

	      UNBLOCK_INPUT;
	    }
          if (keeper)
	    list = Fcons (build_string (XSTRING (Fcar (cur))->data), list);

	  cur = Fcdr (cur);
        }
      list = Fnreverse (list);
    }

  return list;
}
#endif

DEFUN ("w32-find-bdf-fonts", Fw32_find_bdf_fonts, Sw32_find_bdf_fonts,
       1, 1, 0,
       "Return a list of BDF fonts in DIR, suitable for appending to\n\
w32-bdf-filename-alist.  Fonts which do not contain an xfld description\n\
will not be included in the list. DIR may be a list of directories.")
     (directory)
     Lisp_Object directory;
{
  Lisp_Object list = Qnil;
  struct gcpro gcpro1, gcpro2;

  if (!CONSP (directory))
    return w32_find_bdf_fonts_in_dir (directory);

  for ( ; CONSP (directory); directory = XCONS (directory)->cdr)
    {
      Lisp_Object pair[2];
      pair[0] = list;
      pair[1] = Qnil;
      GCPRO2 (directory, list);
      pair[1] = w32_find_bdf_fonts_in_dir( XCONS (directory)->car );
      list = Fnconc( 2, pair );
      UNGCPRO;
    }
  return list;
}

/* Find BDF files in a specified directory.  (use GCPRO when calling,
   as this calls lisp to get a directory listing).  */
Lisp_Object w32_find_bdf_fonts_in_dir( Lisp_Object directory )
{
  Lisp_Object filelist, list = Qnil;
  char fontname[100];

  if (!STRINGP(directory))
    return Qnil;

  filelist = Fdirectory_files (directory, Qt,
                              build_string (".*\\.[bB][dD][fF]"), Qt);

  for ( ; CONSP(filelist); filelist = XCONS (filelist)->cdr)
    {
      Lisp_Object filename = XCONS (filelist)->car;
      if (w32_BDF_to_x_font (XSTRING (filename)->data, fontname, 100))
          store_in_alist (&list, build_string (fontname), filename);
    }
  return list;
}


DEFUN ("x-color-defined-p", Fx_color_defined_p, Sx_color_defined_p, 1, 2, 0,
       "Return non-nil if color COLOR is supported on frame FRAME.\n\
If FRAME is omitted or nil, use the selected frame.")
  (color, frame)
     Lisp_Object color, frame;
{
  COLORREF foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color, 1);

  if (defined_color (f, XSTRING (color)->data, &foo, 0))
    return Qt;
  else
    return Qnil;
}

DEFUN ("x-color-values", Fx_color_values, Sx_color_values, 1, 2, 0,
  "Return a description of the color named COLOR on frame FRAME.\n\
The value is a list of integer RGB values--(RED GREEN BLUE).\n\
These values appear to range from 0 to 65280 or 65535, depending\n\
on the system; white is (65280 65280 65280) or (65535 65535 65535).\n\
If FRAME is omitted or nil, use the selected frame.")
  (color, frame)
     Lisp_Object color, frame;
{
  COLORREF foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color, 1);

  if (defined_color (f, XSTRING (color)->data, &foo, 0))
    {
      Lisp_Object rgb[3];

      rgb[0] = make_number ((GetRValue (foo) << 8) | GetRValue (foo));
      rgb[1] = make_number ((GetGValue (foo) << 8) | GetGValue (foo));
      rgb[2] = make_number ((GetBValue (foo) << 8) | GetBValue (foo));
      return Flist (3, rgb);
    }
  else
    return Qnil;
}

DEFUN ("x-display-color-p", Fx_display_color_p, Sx_display_color_p, 0, 1, 0,
  "Return t if the X display supports color.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  if ((dpyinfo->n_planes * dpyinfo->n_cbits) <= 2)
    return Qnil;

  return Qt;
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
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  if ((dpyinfo->n_planes * dpyinfo->n_cbits) <= 1)
    return Qnil;

  return Qt;
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
  struct w32_display_info *dpyinfo = check_x_display_info (display);

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
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->height);
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
  0, 1, 0,
  "Returns the number of bitplanes of the display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->n_planes * dpyinfo->n_cbits);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
  0, 1, 0,
  "Returns the number of color cells of the display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  HDC hdc;
  int cap;

  hdc = GetDC (dpyinfo->root_window);
  if (dpyinfo->has_palette)
    cap = GetDeviceCaps (hdc,SIZEPALETTE);
  else
    cap = GetDeviceCaps (hdc,NUMCOLORS);
  
  ReleaseDC (dpyinfo->root_window, hdc);
  
  return make_number (cap);
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
  0, 1, 0,
  "Returns the maximum request size of the server of display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (1);
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
  "Returns the vendor ID string of the W32 system (Microsoft).\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  char *vendor = "Microsoft Corp.";

  if (! vendor) vendor = "";
  return build_string (vendor);
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
  "Returns the version numbers of the server of display DISPLAY.\n\
The value is a list of three integers: the major and minor\n\
version numbers, and the vendor-specific release\n\
number.  See also the function `x-server-vendor'.\n\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return Fcons (make_number (w32_major_version),
		Fcons (make_number (w32_minor_version), Qnil));
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
  "Returns the number of screens on the server of display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (1);
}

DEFUN ("x-display-mm-height", Fx_display_mm_height, Sx_display_mm_height, 0, 1, 0,
  "Returns the height in millimeters of the X display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  HDC hdc;
  int cap;

  hdc = GetDC (dpyinfo->root_window);
  
  cap = GetDeviceCaps (hdc, VERTSIZE);
  
  ReleaseDC (dpyinfo->root_window, hdc);
  
  return make_number (cap);
}

DEFUN ("x-display-mm-width", Fx_display_mm_width, Sx_display_mm_width, 0, 1, 0,
  "Returns the width in millimeters of the X display DISPLAY.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  HDC hdc;
  int cap;

  hdc = GetDC (dpyinfo->root_window);
  
  cap = GetDeviceCaps (hdc, HORZSIZE);
  
  ReleaseDC (dpyinfo->root_window, hdc);
  
  return make_number (cap);
}

DEFUN ("x-display-backing-store", Fx_display_backing_store,
  Sx_display_backing_store, 0, 1, 0,
  "Returns an indication of whether display DISPLAY does backing store.\n\
The value may be `always', `when-mapped', or `not-useful'.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  return intern ("not-useful");
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
  Sx_display_visual_class, 0, 1, 0,
  "Returns the visual class of the display DISPLAY.\n\
The value is one of the symbols `static-gray', `gray-scale',\n\
`static-color', `pseudo-color', `true-color', or `direct-color'.\n\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
	(display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

#if 0
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
#endif

  error ("Display has an unknown visual class");
}

DEFUN ("x-display-save-under", Fx_display_save_under,
  Sx_display_save_under, 0, 1, 0,
  "Returns t if the display DISPLAY supports the save-under feature.\n\
The optional argument DISPLAY specifies which display to ask about.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If omitted or nil, that stands for the selected frame's display.")
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

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
  return FONT_WIDTH (f->output_data.w32->font);
}

int
x_char_height (f)
     register struct frame *f;
{
  return f->output_data.w32->line_height;
}

int
x_screen_planes (frame)
     Lisp_Object frame;
{
  return (FRAME_W32_DISPLAY_INFO (XFRAME (frame))->n_planes * 
	  FRAME_W32_DISPLAY_INFO (XFRAME (frame))->n_cbits);
}

/* Return the display structure for the display named NAME.
   Open a new connection if necessary.  */

struct w32_display_info *
x_display_info_for_name (name)
     Lisp_Object name;
{
  Lisp_Object names;
  struct w32_display_info *dpyinfo;

  CHECK_STRING (name, 0);

  for (dpyinfo = &one_w32_display_info, names = w32_display_name_list;
       dpyinfo;
       dpyinfo = dpyinfo->next, names = XCONS (names)->cdr)
    {
      Lisp_Object tem;
      tem = Fstring_equal (XCONS (XCONS (names)->car)->car, name);
      if (!NILP (tem))
	return dpyinfo;
    }

  /* Use this general default value to start with.  */
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  dpyinfo = w32_term_init (name, (unsigned char *)0,
			     (char *) XSTRING (Vx_resource_name)->data);

  if (dpyinfo == 0)
    error ("Cannot connect to server %s", XSTRING (name)->data);

  w32_in_use = 1;
  XSETFASTINT (Vwindow_system_version, 3);

  return dpyinfo;
}

DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0, "Open a connection to a server.\n\
DISPLAY is the name of the display to connect to.\n\
Optional second arg XRM-STRING is a string of resources in xrdb format.\n\
If the optional third arg MUST-SUCCEED is non-nil,\n\
terminate Emacs if we can't open the connection.")
  (display, xrm_string, must_succeed)
     Lisp_Object display, xrm_string, must_succeed;
{
  unsigned int n_planes;
  unsigned char *xrm_option;
  struct w32_display_info *dpyinfo;

  CHECK_STRING (display, 0);
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string, 1);

  if (! EQ (Vwindow_system, intern ("w32")))
    error ("Not using Microsoft Windows");

  /* Allow color mapping to be defined externally; first look in user's
     HOME directory, then in Emacs etc dir for a file called rgb.txt. */
  {
    Lisp_Object color_file;
    struct gcpro gcpro1;

    color_file = build_string("~/rgb.txt");

    GCPRO1 (color_file);

    if (NILP (Ffile_readable_p (color_file)))
      color_file =
	Fexpand_file_name (build_string ("rgb.txt"),
			   Fsymbol_value (intern ("data-directory")));

    Vw32_color_map = Fw32_load_color_file (color_file);

    UNGCPRO;
  }
  if (NILP (Vw32_color_map))
    Vw32_color_map = Fw32_default_color_map ();

  if (! NILP (xrm_string))
    xrm_option = (unsigned char *) XSTRING (xrm_string)->data;
  else
    xrm_option = (unsigned char *) 0;

  /* Use this general default value to start with.  */
  /* First remove .exe suffix from invocation-name - it looks ugly. */
  {
    char basename[ MAX_PATH ], *str;

    strcpy (basename, XSTRING (Vinvocation_name)->data);
    str = strrchr (basename, '.');
    if (str) *str = 0;
    Vinvocation_name = build_string (basename);
  }
  Vx_resource_name = Vinvocation_name;

  validate_x_resource_name ();

  /* This is what opens the connection and sets x_current_display.
     This also initializes many symbols, such as those used for input.  */
  dpyinfo = w32_term_init (display, xrm_option,
			     (char *) XSTRING (Vx_resource_name)->data);

  if (dpyinfo == 0)
    {
      if (!NILP (must_succeed))
	fatal ("Cannot connect to server %s.\n",
	       XSTRING (display)->data);
      else
	error ("Cannot connect to server %s", XSTRING (display)->data);
    }

  w32_in_use = 1;

  XSETFASTINT (Vwindow_system_version, 3);
  return Qnil;
}

DEFUN ("x-close-connection", Fx_close_connection,
       Sx_close_connection, 1, 1, 0,
   "Close the connection to DISPLAY's server.\n\
For DISPLAY, specify either a frame or a display name (a string).\n\
If DISPLAY is nil, that stands for the selected frame's display.")
  (display)
  Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  struct w32_display_info *tail;
  int i;

  if (dpyinfo->reference_count > 0)
    error ("Display still has frames on it");

  BLOCK_INPUT;
  /* Free the fonts in the font table.  */
  for (i = 0; i < dpyinfo->n_fonts; i++)
    {
      if (dpyinfo->font_table[i].name)
	free (dpyinfo->font_table[i].name);
      /* Don't free the full_name string;
	 it is always shared with something else.  */
      w32_unload_font (dpyinfo, dpyinfo->font_table[i].font);
    }
  x_destroy_all_bitmaps (dpyinfo);

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
  for (tail = w32_display_name_list; ! NILP (tail); tail = XCONS (tail)->cdr)
    result = Fcons (XCONS (XCONS (tail)->car)->car, result);

  return result;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize, 1, 2, 0,
   "If ON is non-nil, report errors as soon as the erring request is made.\n\
If ON is nil, allow buffering of requests.\n\
This is a noop on W32 systems.\n\
The optional second argument DISPLAY specifies which display to act on.\n\
DISPLAY should be either a frame or a display name (a string).\n\
If DISPLAY is omitted or nil, that stands for the selected frame's display.")
  (on, display)
    Lisp_Object display, on;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return Qnil;
}


/* These are the w32 specialized functions */

DEFUN ("w32-select-font", Fw32_select_font, Sw32_select_font, 0, 1, 0,
   "This will display the W32 font dialog and return an X font string corresponding to the selection.")
  (frame)
     Lisp_Object frame;
{
  FRAME_PTR f = check_x_frame (frame);
  CHOOSEFONT cf;
  LOGFONT lf;
  TEXTMETRIC tm;
  HDC hdc;
  HANDLE oldobj;
  char buf[100];

  bzero (&cf, sizeof (cf));
  bzero (&lf, sizeof (lf));

  cf.lStructSize = sizeof (cf);
  cf.hwndOwner = FRAME_W32_WINDOW (f);
  cf.Flags = CF_FIXEDPITCHONLY | CF_FORCEFONTEXIST | CF_SCREENFONTS;
  cf.lpLogFont = &lf;

  /* Initialize as much of the font details as we can from the current
     default font.  */
  hdc = GetDC (FRAME_W32_WINDOW (f));
  oldobj = SelectObject (hdc, FRAME_FONT (f)->hfont);
  GetTextFace (hdc, LF_FACESIZE, lf.lfFaceName);
  if (GetTextMetrics (hdc, &tm))
    {
      lf.lfHeight = tm.tmInternalLeading - tm.tmHeight;
      lf.lfWeight = tm.tmWeight;
      lf.lfItalic = tm.tmItalic;
      lf.lfUnderline = tm.tmUnderlined;
      lf.lfStrikeOut = tm.tmStruckOut;
      lf.lfPitchAndFamily = tm.tmPitchAndFamily;
      lf.lfCharSet = tm.tmCharSet;
      cf.Flags |= CF_INITTOLOGFONTSTRUCT;
    }
  SelectObject (hdc, oldobj);
  ReleaseDC (FRAME_W32_WINDOW(f), hdc);

  if (!ChooseFont (&cf) || !w32_to_x_font (&lf, buf, 100))
      return Qnil;

  return build_string (buf);
}

DEFUN ("w32-send-sys-command", Fw32_send_sys_command, Sw32_send_sys_command, 1, 2, 0,
   "Send frame a Windows WM_SYSCOMMAND message of type COMMAND.\n\
Some useful values for command are 0xf030 to maximise frame (0xf020\n\
to minimize), 0xf120 to restore frame to original size, and 0xf100\n\
to activate the menubar for keyboard access.  0xf140 activates the\n\
screen saver if defined.\n\
\n\
If optional parameter FRAME is not specified, use selected frame.")
  (command, frame)
     Lisp_Object command, frame;
{
  WPARAM code;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_NUMBER (command, 0);

  PostMessage (FRAME_W32_WINDOW (f), WM_SYSCOMMAND, XINT (command), 0);

  return Qnil;
}

DEFUN ("w32-shell-execute", Fw32_shell_execute, Sw32_shell_execute, 2, 4, 0,
  "Get Windows to perform OPERATION on DOCUMENT.\n\
This is a wrapper around the ShellExecute system function, which\n\
invokes the application registered to handle OPERATION for DOCUMENT.\n\
OPERATION is typically \"open\", \"print\" or \"explore\", and DOCUMENT\n\
is typically the name of a document file or URL, but can also be a\n\
program executable to run or a directory to open in the Windows Explorer.\n\
\n\
If DOCUMENT is a program executable, PARAMETERS can be a list of command\n\
line parameters, but otherwise should be nil.\n\
\n\
SHOW-FLAG can be used to control whether the invoked application is hidden\n\
or minimized.  If SHOw-FLAG is nil, the application is displayed normally,\n\
otherwise it is an integer representing a ShowWindow flag:\n\
\n\
  0 - start hidden\n\
  1 - start normally\n\
  3 - start maximized\n\
  6 - start minimized")
  (operation, document, parameters, show_flag)
     Lisp_Object operation, document, parameters, show_flag;
{
  Lisp_Object current_dir;

  CHECK_STRING (operation, 0);
  CHECK_STRING (document, 0);

  /* Encode filename and current directory.  */
  current_dir = ENCODE_FILE (current_buffer->directory);
  document = ENCODE_FILE (document);
  if ((int) ShellExecute (NULL,
			  XSTRING (operation)->data,
			  XSTRING (document)->data,
			  (STRINGP (parameters) ?
			   XSTRING (parameters)->data : NULL),
			  XSTRING (current_dir)->data,
			  (INTEGERP (show_flag) ?
			   XINT (show_flag) : SW_SHOWDEFAULT))
      > 32)
    return Qt;
  error ("ShellExecute failed");
}

/* Lookup virtual keycode from string representing the name of a
   non-ascii keystroke into the corresponding virtual key, using
   lispy_function_keys.  */
static int
lookup_vk_code (char *key)
{
  int i;

  for (i = 0; i < 256; i++)
    if (lispy_function_keys[i] != 0
	&& strcmp (lispy_function_keys[i], key) == 0)
      return i;

  return -1;
}

/* Convert a one-element vector style key sequence to a hot key
   definition.  */
static int
w32_parse_hot_key (key)
     Lisp_Object key;
{
  /* Copied from Fdefine_key and store_in_keymap.  */
  register Lisp_Object c;
  int vk_code;
  int lisp_modifiers;
  int w32_modifiers;
  struct gcpro gcpro1;

  CHECK_VECTOR (key, 0);

  if (XFASTINT (Flength (key)) != 1)
    return Qnil;

  GCPRO1 (key);

  c = Faref (key, make_number (0));

  if (CONSP (c) && lucid_event_type_list_p (c))
    c = Fevent_convert_list (c);

  UNGCPRO;

  if (! INTEGERP (c) && ! SYMBOLP (c))
    error ("Key definition is invalid");

  /* Work out the base key and the modifiers.  */
  if (SYMBOLP (c))
    {
      c = parse_modifiers (c);
      lisp_modifiers = Fcar (Fcdr (c));
      c = Fcar (c);
      if (!SYMBOLP (c))
	abort ();
      vk_code = lookup_vk_code (XSYMBOL (c)->name->data);
    }
  else if (INTEGERP (c))
    {
      lisp_modifiers = XINT (c) & ~CHARACTERBITS;
      /* Many ascii characters are their own virtual key code.  */
      vk_code = XINT (c) & CHARACTERBITS;
    }

  if (vk_code < 0 || vk_code > 255)
    return Qnil;

  if ((lisp_modifiers & meta_modifier) != 0
      && !NILP (Vw32_alt_is_meta))
    lisp_modifiers |= alt_modifier;

  /* Convert lisp modifiers to Windows hot-key form.  */
  w32_modifiers  = (lisp_modifiers & hyper_modifier)    ? MOD_WIN : 0;
  w32_modifiers |= (lisp_modifiers & alt_modifier)      ? MOD_ALT : 0;
  w32_modifiers |= (lisp_modifiers & ctrl_modifier)     ? MOD_CONTROL : 0;
  w32_modifiers |= (lisp_modifiers & shift_modifier)    ? MOD_SHIFT : 0;

  return HOTKEY (vk_code, w32_modifiers);
}

DEFUN ("w32-register-hot-key", Fw32_register_hot_key, Sw32_register_hot_key, 1, 1, 0,
   "Register KEY as a hot-key combination.\n\
Certain key combinations like Alt-Tab are reserved for system use on\n\
Windows, and therefore are normally intercepted by the system.  However,\n\
most of these key combinations can be received by registering them as\n\
hot-keys, overriding their special meaning.\n\
\n\
KEY must be a one element key definition in vector form that would be\n\
acceptable to `define-key' (e.g. [A-tab] for Alt-Tab).  The meta\n\
modifier is interpreted as Alt if `w32-alt-is-meta' is t, and hyper\n\
is always interpreted as the Windows modifier keys.\n\
\n\
The return value is the hotkey-id if registered, otherwise nil.")
  (key)
     Lisp_Object key;
{
  key = w32_parse_hot_key (key);

  if (NILP (Fmemq (key, w32_grabbed_keys)))
    {
      /* Reuse an empty slot if possible.  */
      Lisp_Object item = Fmemq (Qnil, w32_grabbed_keys);

      /* Safe to add new key to list, even if we have focus.  */
      if (NILP (item))
	w32_grabbed_keys = Fcons (key, w32_grabbed_keys);
      else
	XCAR (item) = key;

      /* Notify input thread about new hot-key definition, so that it
	 takes effect without needing to switch focus.  */
      PostThreadMessage (dwWindowsThreadId, WM_EMACS_REGISTER_HOT_KEY,
			 (WPARAM) key, 0);
    }

  return key;
}

DEFUN ("w32-unregister-hot-key", Fw32_unregister_hot_key, Sw32_unregister_hot_key, 1, 1, 0,
   "Unregister HOTKEY as a hot-key combination.")
  (key)
     Lisp_Object key;
{
  Lisp_Object item;

  if (!INTEGERP (key))
    key = w32_parse_hot_key (key);

  item = Fmemq (key, w32_grabbed_keys);

  if (!NILP (item))
    {
      /* Notify input thread about hot-key definition being removed, so
	 that it takes effect without needing focus switch.  */
      if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_UNREGISTER_HOT_KEY,
			     (WPARAM) XINT (XCAR (item)), (LPARAM) item))
	{
	  MSG msg;
	  GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
	}
      return Qt;
    }
  return Qnil;
}

DEFUN ("w32-registered-hot-keys", Fw32_registered_hot_keys, Sw32_registered_hot_keys, 0, 0, 0,
   "Return list of registered hot-key IDs.")
  ()
{
  return Fcopy_sequence (w32_grabbed_keys);
}

DEFUN ("w32-reconstruct-hot-key", Fw32_reconstruct_hot_key, Sw32_reconstruct_hot_key, 1, 1, 0,
   "Convert hot-key ID to a lisp key combination.")
  (hotkeyid)
     Lisp_Object hotkeyid;
{
  int vk_code, w32_modifiers;
  Lisp_Object key;

  CHECK_NUMBER (hotkeyid, 0);

  vk_code = HOTKEY_VK_CODE (hotkeyid);
  w32_modifiers = HOTKEY_MODIFIERS (hotkeyid);

  if (lispy_function_keys[vk_code])
    key = intern (lispy_function_keys[vk_code]);
  else
    key = make_number (vk_code);

  key = Fcons (key, Qnil);
  if (w32_modifiers & MOD_SHIFT)
    key = Fcons (Qshift, key);
  if (w32_modifiers & MOD_CONTROL)
    key = Fcons (Qctrl, key);
  if (w32_modifiers & MOD_ALT)
    key = Fcons (NILP (Vw32_alt_is_meta) ? Qalt : Qmeta, key);
  if (w32_modifiers & MOD_WIN)
    key = Fcons (Qhyper, key);

  return key;
}

DEFUN ("w32-toggle-lock-key", Fw32_toggle_lock_key, Sw32_toggle_lock_key, 1, 2, 0,
   "Toggle the state of the lock key KEY.\n\
KEY can be `capslock', `kp-numlock', or `scroll'.\n\
If the optional parameter NEW-STATE is a number, then the state of KEY\n\
is set to off if the low bit of NEW-STATE is zero, otherwise on.")
  (key, new_state)
     Lisp_Object key, new_state;
{
  int vk_code;
  int cur_state;

  if (EQ (key, intern ("capslock")))
    vk_code = VK_CAPITAL;
  else if (EQ (key, intern ("kp-numlock")))
    vk_code = VK_NUMLOCK;
  else if (EQ (key, intern ("scroll")))
    vk_code = VK_SCROLL;
  else
    return Qnil;

  if (!dwWindowsThreadId)
    return make_number (w32_console_toggle_lock_key (vk_code, new_state));

  if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_TOGGLE_LOCK_KEY,
			 (WPARAM) vk_code, (LPARAM) new_state))
    {
      MSG msg;
      GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
      return make_number (msg.wParam);
    }
  return Qnil;
}

syms_of_w32fns ()
{
  /* This is zero if not using MS-Windows.  */
  w32_in_use = 0;

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
  Qx_resource_name = intern ("x-resource-name");
  staticpro (&Qx_resource_name);
  Quser_position = intern ("user-position");
  staticpro (&Quser_position);
  Quser_size = intern ("user-size");
  staticpro (&Quser_size);
  Qdisplay = intern ("display");
  staticpro (&Qdisplay);
  /* This is the end of symbol initialization.  */

  Qhyper = intern ("hyper");
  staticpro (&Qhyper);
  Qsuper = intern ("super");
  staticpro (&Qsuper);
  Qmeta = intern ("meta");
  staticpro (&Qmeta);
  Qalt = intern ("alt");
  staticpro (&Qalt);
  Qctrl = intern ("ctrl");
  staticpro (&Qctrl);
  Qcontrol = intern ("control");
  staticpro (&Qcontrol);
  Qshift = intern ("shift");
  staticpro (&Qshift);

  Qface_set_after_frame_default = intern ("face-set-after-frame-default");
  staticpro (&Qface_set_after_frame_default);

  Fput (Qundefined_color, Qerror_conditions,
	Fcons (Qundefined_color, Fcons (Qerror, Qnil)));
  Fput (Qundefined_color, Qerror_message,
	build_string ("Undefined color"));

  staticpro (&w32_grabbed_keys);
  w32_grabbed_keys = Qnil;

  DEFVAR_LISP ("w32-color-map", &Vw32_color_map,
	       "An array of color name mappings for windows.");
  Vw32_color_map = Qnil;

  DEFVAR_LISP ("w32-pass-alt-to-system", &Vw32_pass_alt_to_system,
	       "Non-nil if alt key presses are passed on to Windows.\n\
When non-nil, for example, alt pressed and released and then space will\n\
open the System menu.  When nil, Emacs silently swallows alt key events.");
  Vw32_pass_alt_to_system = Qnil;

  DEFVAR_LISP ("w32-alt-is-meta", &Vw32_alt_is_meta,
	       "Non-nil if the alt key is to be considered the same as the meta key.\n\
When nil, Emacs will translate the alt key to the Alt modifier, and not Meta.");
  Vw32_alt_is_meta = Qt;

  DEFVAR_INT ("w32-quit-key", &Vw32_quit_key,
	       "If non-zero, the virtual key code for an alternative quit key.");
  XSETINT (Vw32_quit_key, 0);

  DEFVAR_LISP ("w32-pass-lwindow-to-system", 
	       &Vw32_pass_lwindow_to_system,
	       "Non-nil if the left \"Windows\" key is passed on to Windows.\n\
When non-nil, the Start menu is opened by tapping the key.");
  Vw32_pass_lwindow_to_system = Qt;

  DEFVAR_LISP ("w32-pass-rwindow-to-system", 
	       &Vw32_pass_rwindow_to_system,
	       "Non-nil if the right \"Windows\" key is passed on to Windows.\n\
When non-nil, the Start menu is opened by tapping the key.");
  Vw32_pass_rwindow_to_system = Qt;

  DEFVAR_INT ("w32-phantom-key-code",
	       &Vw32_phantom_key_code,
	       "Virtual key code used to generate \"phantom\" key presses.\n\
Value is a number between 0 and 255.\n\
\n\
Phantom key presses are generated in order to stop the system from\n\
acting on \"Windows\" key events when `w32-pass-lwindow-to-system' or\n\
`w32-pass-rwindow-to-system' is nil.");
  /* Although 255 is technically not a valid key code, it works and
     means that this hack won't interfere with any real key code.  */
  Vw32_phantom_key_code = 255;

  DEFVAR_LISP ("w32-enable-num-lock", 
	       &Vw32_enable_num_lock,
	       "Non-nil if Num Lock should act normally.\n\
Set to nil to see Num Lock as the key `kp-numlock'.");
  Vw32_enable_num_lock = Qt;

  DEFVAR_LISP ("w32-enable-caps-lock", 
	       &Vw32_enable_caps_lock,
	       "Non-nil if Caps Lock should act normally.\n\
Set to nil to see Caps Lock as the key `capslock'.");
  Vw32_enable_caps_lock = Qt;

  DEFVAR_LISP ("w32-scroll-lock-modifier",
	       &Vw32_scroll_lock_modifier,
	       "Modifier to use for the Scroll Lock on state.\n\
The value can be hyper, super, meta, alt, control or shift for the\n\
respective modifier, or nil to see Scroll Lock as the key `scroll'.\n\
Any other value will cause the key to be ignored.");
  Vw32_scroll_lock_modifier = Qt;

  DEFVAR_LISP ("w32-lwindow-modifier",
	       &Vw32_lwindow_modifier,
	       "Modifier to use for the left \"Windows\" key.\n\
The value can be hyper, super, meta, alt, control or shift for the\n\
respective modifier, or nil to appear as the key `lwindow'.\n\
Any other value will cause the key to be ignored.");
  Vw32_lwindow_modifier = Qnil;

  DEFVAR_LISP ("w32-rwindow-modifier",
	       &Vw32_rwindow_modifier,
	       "Modifier to use for the right \"Windows\" key.\n\
The value can be hyper, super, meta, alt, control or shift for the\n\
respective modifier, or nil to appear as the key `rwindow'.\n\
Any other value will cause the key to be ignored.");
  Vw32_rwindow_modifier = Qnil;

  DEFVAR_LISP ("w32-apps-modifier",
	       &Vw32_apps_modifier,
	       "Modifier to use for the \"Apps\" key.\n\
The value can be hyper, super, meta, alt, control or shift for the\n\
respective modifier, or nil to appear as the key `apps'.\n\
Any other value will cause the key to be ignored.");
  Vw32_apps_modifier = Qnil;

  DEFVAR_LISP ("w32-enable-italics", &Vw32_enable_italics,
	       "Non-nil enables selection of artificially italicized fonts.");
  Vw32_enable_italics = Qnil;

  DEFVAR_LISP ("w32-enable-palette", &Vw32_enable_palette,
	       "Non-nil enables Windows palette management to map colors exactly.");
  Vw32_enable_palette = Qt;

  DEFVAR_INT ("w32-mouse-button-tolerance",
	      &Vw32_mouse_button_tolerance,
	      "Analogue of double click interval for faking middle mouse events.\n\
The value is the minimum time in milliseconds that must elapse between\n\
left/right button down events before they are considered distinct events.\n\
If both mouse buttons are depressed within this interval, a middle mouse\n\
button down event is generated instead.");
  XSETINT (Vw32_mouse_button_tolerance, GetDoubleClickTime () / 2);

  DEFVAR_INT ("w32-mouse-move-interval",
	      &Vw32_mouse_move_interval,
	      "Minimum interval between mouse move events.\n\
The value is the minimum time in milliseconds that must elapse between\n\
successive mouse move (or scroll bar drag) events before they are\n\
reported as lisp events.");
  XSETINT (Vw32_mouse_move_interval, 0);

  init_x_parm_symbols ();

  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path,
    "List of directories to search for bitmap files for w32.");
  Vx_bitmap_file_path = decode_env_path ((char *) 0, "PATH");

  DEFVAR_LISP ("x-pointer-shape", &Vx_pointer_shape,
    "The shape of the pointer when over text.\n\
Changing the value does not affect existing frames\n\
unless you set the mouse color.");
  Vx_pointer_shape = Qnil;

  DEFVAR_LISP ("x-resource-name", &Vx_resource_name,
    "The name Emacs uses to look up resources; for internal use only.\n\
`x-get-resource' uses this as the first component of the instance name\n\
when requesting resource values.\n\
Emacs initially sets `x-resource-name' to the name under which Emacs\n\
was invoked, or to the value specified with the `-name' or `-rn'\n\
switches, if present.");
  Vx_resource_name = Qnil;

  Vx_nontext_pointer_shape = Qnil;

  Vx_mode_pointer_shape = Qnil;

  DEFVAR_INT ("x-sensitive-text-pointer-shape",
	      &Vx_sensitive_text_pointer_shape,
	      "The shape of the pointer when over mouse-sensitive text.\n\
This variable takes effect when you create a new frame\n\
or when you set the mouse color.");
  Vx_sensitive_text_pointer_shape = Qnil;

  DEFVAR_LISP ("x-cursor-fore-pixel", &Vx_cursor_fore_pixel,
	       "A string indicating the foreground color of the cursor box.");
  Vx_cursor_fore_pixel = Qnil;

  DEFVAR_LISP ("x-no-window-manager", &Vx_no_window_manager,
	       "Non-nil if no window manager is in use.\n\
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

  DEFVAR_LISP ("w32-bdf-filename-alist",
               &Vw32_bdf_filename_alist,
               "List of bdf fonts and their corresponding filenames.");
  Vw32_bdf_filename_alist = Qnil;

  DEFVAR_BOOL ("w32-strict-fontnames",
               &w32_strict_fontnames,
  "Non-nil means only use fonts that are exact matches for those requested.\n\
Default is nil, which allows old fontnames that are not XLFD compliant,\n\
and allows third-party CJK display to work by specifying false charset\n\
fields to trick Emacs into translating to Big5, SJIS etc.\n\
Setting this to t will prevent wrong fonts being selected when\n\
fontsets are automatically created.");
  w32_strict_fontnames = 0;

  DEFVAR_BOOL ("w32-strict-painting",
               &w32_strict_painting,
  "Non-nil means use strict rules for repainting frames.\n\
Set this to nil to get the old behaviour for repainting; this should\n\
only be necessary if the default setting causes problems.");
  w32_strict_painting = 1;

  DEFVAR_LISP ("w32-system-coding-system",
               &Vw32_system_coding_system,
  "Coding system used by Windows system functions, such as for font names.");
  Vw32_system_coding_system = Qnil;

  defsubr (&Sx_get_resource);
  defsubr (&Sx_list_fonts);
  defsubr (&Sx_display_color_p);
  defsubr (&Sx_display_grayscale_p);
  defsubr (&Sx_color_defined_p);
  defsubr (&Sx_color_values);
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

  /* W32 specific functions */

  defsubr (&Sw32_focus_frame);
  defsubr (&Sw32_select_font);
  defsubr (&Sw32_define_rgb_color);
  defsubr (&Sw32_default_color_map);
  defsubr (&Sw32_load_color_file);
  defsubr (&Sw32_send_sys_command);
  defsubr (&Sw32_shell_execute);
  defsubr (&Sw32_register_hot_key);
  defsubr (&Sw32_unregister_hot_key);
  defsubr (&Sw32_registered_hot_keys);
  defsubr (&Sw32_reconstruct_hot_key);
  defsubr (&Sw32_toggle_lock_key);
  defsubr (&Sw32_find_bdf_fonts);

  /* Setting callback functions for fontset handler.  */
  get_font_info_func = w32_get_font_info;
  list_fonts_func = w32_list_fonts;
  load_font_func = w32_load_font;
  find_ccl_program_func = w32_find_ccl_program;
  query_font_func = w32_query_font;
  set_frame_fontset_func = x_set_font;
  check_window_system_func = check_w32;
}

#undef abort

void 
w32_abort()
{
  int button;
  button = MessageBox (NULL,
		       "A fatal error has occurred!\n\n"
		       "Select Abort to exit, Retry to debug, Ignore to continue",
		       "Emacs Abort Dialog",
		       MB_ICONEXCLAMATION | MB_TASKMODAL
		       | MB_SETFOREGROUND | MB_ABORTRETRYIGNORE);
  switch (button)
    {
    case IDRETRY:
      DebugBreak ();
      break;
    case IDIGNORE:
      break;
    case IDABORT:
    default:
      abort ();
      break;
    }
}

/* For convenience when debugging.  */
int
w32_last_error()
{
  return GetLastError ();
}
