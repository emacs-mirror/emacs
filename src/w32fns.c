/* Graphical user interface functions for the Microsoft W32 API.
   Copyright (C) 1989, 92, 93, 94, 95, 1996, 1997, 1998, 1999, 2000, 2001
     Free Software Foundation, Inc.

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
#include "dispextern.h"
#include "w32term.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "buffer.h"
#include "fontset.h"
#include "intervals.h"
#include "blockinput.h"
#include "epaths.h"
#include "w32heap.h"
#include "termhooks.h"
#include "coding.h"
#include "ccl.h"
#include "systime.h"

#include "bitmaps/gray.xbm"

#include <commdlg.h>
#include <shellapi.h>
#include <ctype.h>

extern void free_frame_menubar ();
extern void x_compute_fringe_widths P_ ((struct frame *, int));
extern double atof ();
extern int w32_console_toggle_lock_key P_ ((int, Lisp_Object));
extern void w32_menu_display_help P_ ((HWND, HMENU, UINT, UINT));
extern void w32_free_menu_strings P_ ((HWND));

extern int quit_char;

/* A definition of XColor for non-X frames.  */
#ifndef HAVE_X_WINDOWS
typedef struct {
  unsigned long pixel;
  unsigned short red, green, blue;
  char flags;
  char pad;
} XColor;
#endif

extern char *lispy_function_keys[];

/* The gray bitmap `bitmaps/gray'.  This is done because w32term.c uses
   it, and including `bitmaps/gray' more than once is a problem when
   config.h defines `static' as an empty replacement string.  */

int gray_bitmap_width = gray_width;
int gray_bitmap_height = gray_height;
unsigned char *gray_bitmap_bits = gray_bits;

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

/* Switch to control whether we inhibit requests for synthesized bold
   and italic versions of fonts.  */
int w32_enable_synthesized_fonts;

/* Enable palette management. */
Lisp_Object Vw32_enable_palette;

/* Control how close left/right button down events must be to
   be converted to a middle button down event. */
Lisp_Object Vw32_mouse_button_tolerance;

/* Minimum interval between mouse movement (and scroll bar drag)
   events that are passed on to the event loop. */
Lisp_Object Vw32_mouse_move_interval;

/* Flag to indicate if XBUTTON events should be passed on to Windows.  */
int w32_pass_extra_mouse_buttons_to_system;

/* The name we're using in resource queries.  */
Lisp_Object Vx_resource_name;

/* Non nil if no window manager is in use.  */
Lisp_Object Vx_no_window_manager;

/* Non-zero means we're allowed to display a hourglass pointer.  */

int display_hourglass_p;

/* The background and shape of the mouse pointer, and shape when not
   over text or in the modeline.  */

Lisp_Object Vx_pointer_shape, Vx_nontext_pointer_shape, Vx_mode_pointer_shape;
Lisp_Object Vx_hourglass_pointer_shape, Vx_window_horizontal_drag_shape;

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

/* A flag to control whether fonts are matched strictly or not.  */
int w32_strict_fontnames;

/* A flag to control whether we should only repaint if GetUpdateRect
   indicates there is an update region.  */
int w32_strict_painting;

/* Associative list linking character set strings to Windows codepages. */
Lisp_Object Vw32_charset_info_alist;

/* VIETNAMESE_CHARSET is not defined in some versions of MSVC.  */
#ifndef VIETNAMESE_CHARSET
#define VIETNAMESE_CHARSET 163
#endif

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
Lisp_Object Qparent_id;
Lisp_Object Qscroll_bar_width;
Lisp_Object Qsuppress_icon;
Lisp_Object Qundefined_color;
Lisp_Object Qvertical_scroll_bars;
Lisp_Object Qvisibility;
Lisp_Object Qwindow_id;
Lisp_Object Qx_frame_parameter;
Lisp_Object Qx_resource_name;
Lisp_Object Quser_position;
Lisp_Object Quser_size;
Lisp_Object Qscreen_gamma;
Lisp_Object Qline_spacing;
Lisp_Object Qcenter;
Lisp_Object Qcancel_timer;
Lisp_Object Qhyper;
Lisp_Object Qsuper;
Lisp_Object Qmeta;
Lisp_Object Qalt;
Lisp_Object Qctrl;
Lisp_Object Qcontrol;
Lisp_Object Qshift;

Lisp_Object Qw32_charset_ansi;
Lisp_Object Qw32_charset_default;
Lisp_Object Qw32_charset_symbol;
Lisp_Object Qw32_charset_shiftjis;
Lisp_Object Qw32_charset_hangeul;
Lisp_Object Qw32_charset_gb2312;
Lisp_Object Qw32_charset_chinesebig5;
Lisp_Object Qw32_charset_oem;

#ifndef JOHAB_CHARSET
#define JOHAB_CHARSET 130
#endif
#ifdef JOHAB_CHARSET
Lisp_Object Qw32_charset_easteurope;
Lisp_Object Qw32_charset_turkish;
Lisp_Object Qw32_charset_baltic;
Lisp_Object Qw32_charset_russian;
Lisp_Object Qw32_charset_arabic;
Lisp_Object Qw32_charset_greek;
Lisp_Object Qw32_charset_hebrew;
Lisp_Object Qw32_charset_vietnamese;
Lisp_Object Qw32_charset_thai;
Lisp_Object Qw32_charset_johab;
Lisp_Object Qw32_charset_mac;
#endif

#ifdef UNICODE_CHARSET
Lisp_Object Qw32_charset_unicode;
#endif

Lisp_Object Qfullscreen;
Lisp_Object Qfullwidth;
Lisp_Object Qfullheight;
Lisp_Object Qfullboth;

extern Lisp_Object Qtop;
extern Lisp_Object Qdisplay;

/* State variables for emulating a three button mouse. */
#define LMOUSE 1
#define MMOUSE 2
#define RMOUSE 4

static int button_state = 0;
static W32Msg saved_mouse_button_msg;
static unsigned mouse_button_timer = 0;	/* non-zero when timer is active */
static W32Msg saved_mouse_move_msg;
static unsigned mouse_move_timer = 0;

/* Window that is tracking the mouse.  */
static HWND track_mouse_window;
FARPROC track_mouse_event_fn;

/* W95 mousewheel handler */
unsigned int msh_mousewheel = 0;	

/* Timers */
#define MOUSE_BUTTON_ID	1
#define MOUSE_MOVE_ID	2
#define MENU_FREE_ID 3
/* The delay (milliseconds) before a menu is freed after WM_EXITMENULOOP
   is received.  */
#define MENU_FREE_DELAY 1000
static unsigned menu_free_timer = 0;

/* The below are defined in frame.c.  */

extern Lisp_Object Qheight, Qminibuffer, Qname, Qonly, Qwidth;
extern Lisp_Object Qunsplittable, Qmenu_bar_lines, Qbuffer_predicate, Qtitle;
extern Lisp_Object Qtool_bar_lines;

extern Lisp_Object Vwindow_system_version;

Lisp_Object Qface_set_after_frame_default;

#ifdef GLYPH_DEBUG
int image_cache_refcount, dpyinfo_refcount;
#endif


/* From w32term.c. */
extern Lisp_Object Vw32_num_mouse_buttons;
extern Lisp_Object Vw32_recognize_altgr;

extern HWND w32_system_caret_hwnd;

extern int w32_system_caret_height;
extern int w32_system_caret_x;
extern int w32_system_caret_y;
extern int w32_use_visible_system_caret;

static HWND w32_visible_system_caret_hwnd;


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
    frame = selected_frame;
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);
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
      struct frame *sf = XFRAME (selected_frame);
      
      if (FRAME_W32_P (sf) && FRAME_LIVE_P (sf))
	return FRAME_W32_DISPLAY_INFO (sf);
      else
	return &one_w32_display_info;
    }
  else if (STRINGP (frame))
    return x_display_info_for_name (frame);
  else
    {
      FRAME_PTR f;

      CHECK_LIVE_FRAME (frame);
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

  for (tail = Vframe_list; GC_CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!GC_FRAMEP (frame))
        continue;
      f = XFRAME (frame);
      if (!FRAME_W32_P (f) || FRAME_W32_DISPLAY_INFO (f) != dpyinfo)
	continue;
      if (f->output_data.w32->hourglass_window == wdesc)
        return f;

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
#if 0 /* TODO : bitmap support */
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  unsigned int width, height;
  HBITMAP bitmap;
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
  fd = openp (Vx_bitmap_file_path, file, Qnil, &found, 0);
  if (fd < 0)
    return -1;
  emacs_close (fd);

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
#endif  /* TODO */
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
     struct w32_display_info *dpyinfo;
{
  int i;
  for (i = 0; i < dpyinfo->bitmaps_last; i++)
    if (dpyinfo->bitmaps[i].refcount > 0)
      {
	DeleteObject (dpyinfo->bitmaps[i].pixmap);
	if (dpyinfo->bitmaps[i].file)
	  xfree (dpyinfo->bitmaps[i].file);
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
  void (*setter) P_ ((struct frame *, Lisp_Object, Lisp_Object));
};

BOOL my_show_window P_ ((struct frame *, HWND, int));
void my_set_window_pos P_ ((HWND, HWND, int, int, int, int, UINT));
static Lisp_Object unwind_create_frame P_ ((Lisp_Object));
static Lisp_Object unwind_create_tip_frame P_ ((Lisp_Object));
static void x_change_window_heights P_ ((Lisp_Object, int));
/* TODO: Native Input Method support; see x_create_im.  */
void x_set_foreground_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_set_line_spacing P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_set_fullscreen P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_background_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_mouse_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_cursor_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_border_color P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_cursor_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_icon_type P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_icon_name P_ ((struct frame *, Lisp_Object, Lisp_Object));
void x_set_font P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_set_fringe_width P_ ((struct frame *, Lisp_Object, Lisp_Object));
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
static void x_set_screen_gamma P_ ((struct frame *, Lisp_Object, Lisp_Object));
static void x_edge_detection P_ ((struct frame *, struct image *, Lisp_Object,
				  Lisp_Object));

static struct x_frame_parm_table x_frame_parms[] =
{
  {"auto-raise", x_set_autoraise},
  {"auto-lower", x_set_autolower},
  {"background-color", x_set_background_color},
  {"border-color", x_set_border_color},
  {"border-width", x_set_border_width},
  {"cursor-color", x_set_cursor_color},
  {"cursor-type", x_set_cursor_type},
  {"font", x_set_font},
  {"foreground-color", x_set_foreground_color},
  {"icon-name", x_set_icon_name},
  {"icon-type", x_set_icon_type},
  {"internal-border-width", x_set_internal_border_width},
  {"menu-bar-lines", x_set_menu_bar_lines},
  {"mouse-color", x_set_mouse_color},
  {"name", x_explicitly_set_name},
  {"scroll-bar-width", x_set_scroll_bar_width},
  {"title", x_set_title},
  {"unsplittable", x_set_unsplittable},
  {"vertical-scroll-bars", x_set_vertical_scroll_bars},
  {"visibility", x_set_visibility},
  {"tool-bar-lines", x_set_tool_bar_lines},
  {"screen-gamma", x_set_screen_gamma},
  {"line-spacing", x_set_line_spacing},
  {"left-fringe", x_set_fringe_width},
  {"right-fringe", x_set_fringe_width},
  {"fullscreen", x_set_fullscreen},
};

/* Attach the `x-frame-parameter' properties to
   the Lisp symbol names of parameters relevant to W32.  */

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
  if (new_top != f->output_data.w32->top_pos
      || new_left != f->output_data.w32->left_pos)
    {
      int move_x = new_left;
      int move_y = new_top;

      f->output_data.w32->want_fullscreen |= FULLSCREEN_MOVE_WAIT;
      x_set_offset (f, move_x, move_y, 1);
    }
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
	else if (CONSP (left) && EQ (XCAR (left), Qminus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = - XINT (XCAR (XCDR (left)));
	    f->output_data.w32->size_hint_flags |= XNegative;
	  }
	else if (CONSP (left) && EQ (XCAR (left), Qplus)
		 && CONSP (XCDR (left))
		 && INTEGERP (XCAR (XCDR (left))))
	  {
	    leftpos = XINT (XCAR (XCDR (left)));
	  }

	if (EQ (top, Qminus))
	  f->output_data.w32->size_hint_flags |= YNegative;
	else if (INTEGERP (top))
	  {
	    toppos = XINT (top);
	    if (toppos < 0)
	      f->output_data.w32->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qminus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = - XINT (XCAR (XCDR (top)));
	    f->output_data.w32->size_hint_flags |= YNegative;
	  }
	else if (CONSP (top) && EQ (XCAR (top), Qplus)
		 && CONSP (XCDR (top))
		 && INTEGERP (XCAR (XCDR (top))))
	  {
	    toppos = XINT (XCAR (XCDR (top)));
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
  RECT rect;

  GetClientRect(FRAME_W32_WINDOW(f), &rect);
  AdjustWindowRect(&rect, f->output_data.w32->dwStyle, FRAME_EXTERNAL_MENU_BAR(f));

  pt.x = rect.left;
  pt.y = rect.top;

  ClientToScreen (FRAME_W32_WINDOW(f), &pt);

  /* Remember x_pixels_diff and y_pixels_diff.  */
  f->output_data.w32->x_pixels_diff = pt.x - rect.left;
  f->output_data.w32->y_pixels_diff = pt.y - rect.top;

  *xptr = pt.x;
  *yptr = pt.y;
}

/* Insert a description of internally-recorded parameters of frame X
   into the parameter alist *ALISTPTR that is to be given to the user.
   Only parameters that are specific to W32
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
  store_in_alist (alistptr, Qleft_fringe,
       	   make_number (f->output_data.w32->left_fringe_width));
  store_in_alist (alistptr, Qright_fringe,
       	   make_number (f->output_data.w32->right_fringe_width));
  store_in_alist (alistptr, Qscroll_bar_width,
           make_number (FRAME_HAS_VERTICAL_SCROLL_BARS (f)
                        ? FRAME_SCROLL_BAR_PIXEL_WIDTH(f)
                        : 0));
  sprintf (buf, "%ld", (long) FRAME_W32_WINDOW (f));
  store_in_alist (alistptr, Qwindow_id,
       	   build_string (buf));
  store_in_alist (alistptr, Qicon_name, f->icon_name);
  FRAME_SAMPLE_VISIBILITY (f);
  store_in_alist (alistptr, Qvisibility,
		  (FRAME_VISIBLE_P (f) ? Qt
		   : FRAME_ICONIFIED_P (f) ? Qicon : Qnil));
  store_in_alist (alistptr, Qdisplay,
		  XCAR (FRAME_W32_DISPLAY_INFO (f)->name_list_element));
}


DEFUN ("w32-define-rgb-color", Fw32_define_rgb_color,
       Sw32_define_rgb_color, 4, 4, 0,
       doc: /* Convert RGB numbers to a windows color reference and associate with NAME.
This adds or updates a named color to w32-color-map, making it
available for use.  The original entry's RGB ref is returned, or nil
if the entry is new.  */)
    (red, green, blue, name)
    Lisp_Object red, green, blue, name;
{
  Lisp_Object rgb;
  Lisp_Object oldrgb = Qnil;
  Lisp_Object entry;

  CHECK_NUMBER (red);
  CHECK_NUMBER (green);
  CHECK_NUMBER (blue);
  CHECK_STRING (name);

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

DEFUN ("w32-load-color-file", Fw32_load_color_file,
       Sw32_load_color_file, 1, 1, 0,
       doc: /* Create an alist of color entries from an external file.
Assign this value to w32-color-map to replace the existing color map.

The file should define one named RGB color per line like so:
  R G B   name
where R,G,B are numbers between 0 and 255 and name is an arbitrary string.  */)
    (filename)
    Lisp_Object filename;
{
  FILE *fp;
  Lisp_Object cmap = Qnil;
  Lisp_Object abspath;

  CHECK_STRING (filename);
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
       0, 0, 0, doc: /* Return the default color map.  */)
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
  
  CHECK_NUMBER (rgb);
  
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
  register Lisp_Object ret = Qnil;

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
	  char *ptr, *approx = alloca (len + 1);

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


/* Gamma-correct COLOR on frame F.  */

void
gamma_correct (f, color)
     struct frame *f;
     COLORREF *color;
{
  if (f->gamma)
    {
      *color = PALETTERGB (
        pow (GetRValue (*color) / 255.0, f->gamma) * 255.0 + 0.5,
        pow (GetGValue (*color) / 255.0, f->gamma) * 255.0 + 0.5,
        pow (GetBValue (*color) / 255.0, f->gamma) * 255.0 + 0.5);
    }
}


/* Decide if color named COLOR is valid for the display associated with
   the selected frame; if so, return the rgb values in COLOR_DEF.
   If ALLOC is nonzero, allocate a new colormap cell.  */

int
w32_defined_color (f, color, color_def, alloc)
     FRAME_PTR f;
     char *color;
     XColor *color_def;
     int alloc;
{
  register Lisp_Object tem;
  COLORREF w32_color_ref;

  tem = x_to_w32_color (color);

  if (!NILP (tem)) 
    {
      if (f)
        {
          /* Apply gamma correction.  */
          w32_color_ref = XUINT (tem);
          gamma_correct (f, &w32_color_ref);
          XSETINT (tem, w32_color_ref);
        }

      /* Map this color to the palette if it is enabled. */
      if (!NILP (Vw32_enable_palette))
	{
	  struct w32_palette_entry * entry =
	    one_w32_display_info.color_list;
	  struct w32_palette_entry ** prev =
	    &one_w32_display_info.color_list;
      
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
	      one_w32_display_info.num_colors++;

	      /* set flag that palette must be regenerated */
	      one_w32_display_info.regen_palette = TRUE;
	    }
	}
      /* Ensure COLORREF value is snapped to nearest color in (default)
	 palette by simulating the PALETTERGB macro.  This works whether
	 or not the display device has a palette. */
      w32_color_ref = XUINT (tem) | 0x2000000;

      color_def->pixel = w32_color_ref;
      color_def->red = GetRValue (w32_color_ref);
      color_def->green = GetGValue (w32_color_ref);
      color_def->blue = GetBValue (w32_color_ref);

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
  XColor cdef;

  CHECK_STRING (arg);

  if (strcmp (XSTRING (arg)->data, "black") == 0)
    return BLACK_PIX_DEFAULT (f);
  else if (strcmp (XSTRING (arg)->data, "white") == 0)
    return WHITE_PIX_DEFAULT (f);

  if ((FRAME_W32_DISPLAY_INFO (f)->n_planes * FRAME_W32_DISPLAY_INFO (f)->n_cbits) == 1)
    return def;

  /* w32_defined_color is responsible for coping with failures
     by looking for a near-miss.  */
  if (w32_defined_color (f, XSTRING (arg)->data, &cdef, 1))
    return cdef.pixel;

  /* defined_color failed; return an ultimate default.  */
  return def;
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


/* Change the `fullscreen' frame parameter of frame F.  OLD_VALUE is
   the previous value of that parameter, NEW_VALUE is the new value. */

static void
x_set_fullscreen (f, new_value, old_value)
     struct frame *f;
     Lisp_Object new_value, old_value;
{
  if (NILP (new_value))
    f->output_data.w32->want_fullscreen = FULLSCREEN_NONE;
  else if (EQ (new_value, Qfullboth))
    f->output_data.w32->want_fullscreen = FULLSCREEN_BOTH;
  else if (EQ (new_value, Qfullwidth))
    f->output_data.w32->want_fullscreen = FULLSCREEN_WIDTH;
  else if (EQ (new_value, Qfullheight))
    f->output_data.w32->want_fullscreen = FULLSCREEN_HEIGHT;
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
    Fsignal (Qerror, Fcons (build_string ("Invalid screen-gamma"),
			    Fcons (new_value, Qnil)));

  clear_face_cache (0);
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
  struct w32_output *x = f->output_data.w32;
  PIX_TYPE fg, old_fg;

  fg = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  old_fg = FRAME_FOREGROUND_PIXEL (f);
  FRAME_FOREGROUND_PIXEL (f) = fg;

  if (FRAME_W32_WINDOW (f) != 0)
    {
      if (x->cursor_pixel == old_fg)
	x->cursor_pixel = fg;

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
  FRAME_BACKGROUND_PIXEL (f)
    = x_decode_color (f, arg, WHITE_PIX_DEFAULT (f));

  if (FRAME_W32_WINDOW (f) != 0)
    {
      SetWindowLong (FRAME_W32_WINDOW (f), WND_BACKGROUND_INDEX,
                     FRAME_BACKGROUND_PIXEL (f));

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
  int count;
  int mask_color;

  if (!EQ (Qnil, arg))
    f->output_data.w32->mouse_pixel
      = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  mask_color = FRAME_BACKGROUND_PIXEL (f);

  /* Don't let pointers be invisible.  */
  if (mask_color == f->output_data.w32->mouse_pixel
	&& mask_color == FRAME_BACKGROUND_PIXEL (f))
    f->output_data.w32->mouse_pixel = FRAME_FOREGROUND_PIXEL (f);

#if 0 /* TODO : cursor changes */
  BLOCK_INPUT;

  /* It's not okay to crash if the user selects a screwy cursor.  */
  count = x_catch_errors (FRAME_W32_DISPLAY (f));

  if (!EQ (Qnil, Vx_pointer_shape))
    {
      CHECK_NUMBER (Vx_pointer_shape);
      cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XINT (Vx_pointer_shape));
    }
  else
    cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_xterm);
  x_check_errors (FRAME_W32_DISPLAY (f), "bad text pointer cursor: %s");

  if (!EQ (Qnil, Vx_nontext_pointer_shape))
    {
      CHECK_NUMBER (Vx_nontext_pointer_shape);
      nontext_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f),
					  XINT (Vx_nontext_pointer_shape));
    }
  else
    nontext_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_left_ptr);
  x_check_errors (FRAME_W32_DISPLAY (f), "bad nontext pointer cursor: %s");

  if (!EQ (Qnil, Vx_hourglass_pointer_shape))
    {
      CHECK_NUMBER (Vx_hourglass_pointer_shape);
      hourglass_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f),
					    XINT (Vx_hourglass_pointer_shape));
    }
  else
    hourglass_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_watch);
  x_check_errors (FRAME_W32_DISPLAY (f), "bad busy pointer cursor: %s");
  
  x_check_errors (FRAME_W32_DISPLAY (f), "bad nontext pointer cursor: %s");
  if (!EQ (Qnil, Vx_mode_pointer_shape))
    {
      CHECK_NUMBER (Vx_mode_pointer_shape);
      mode_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f),
				       XINT (Vx_mode_pointer_shape));
    }
  else
    mode_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_xterm);
  x_check_errors (FRAME_W32_DISPLAY (f), "bad modeline pointer cursor: %s");

  if (!EQ (Qnil, Vx_sensitive_text_pointer_shape))
    {
      CHECK_NUMBER (Vx_sensitive_text_pointer_shape);
      cross_cursor
	= XCreateFontCursor (FRAME_W32_DISPLAY (f),
			     XINT (Vx_sensitive_text_pointer_shape));
    }
  else
    cross_cursor = XCreateFontCursor (FRAME_W32_DISPLAY (f), XC_crosshair);

  if (!NILP (Vx_window_horizontal_drag_shape))
    {
      CHECK_NUMBER (Vx_window_horizontal_drag_shape);
      horizontal_drag_cursor
	= XCreateFontCursor (FRAME_X_DISPLAY (f),
			     XINT (Vx_window_horizontal_drag_shape));
    }
  else
    horizontal_drag_cursor
      = XCreateFontCursor (FRAME_X_DISPLAY (f), XC_sb_h_double_arrow);

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
    XRecolorCursor (FRAME_W32_DISPLAY (f), hourglass_cursor,
                    &fore_color, &back_color);
  }

  if (FRAME_W32_WINDOW (f) != 0)
    XDefineCursor (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f), cursor);

  if (cursor != f->output_data.w32->text_cursor && f->output_data.w32->text_cursor != 0)
    XFreeCursor (FRAME_W32_DISPLAY (f), f->output_data.w32->text_cursor);
  f->output_data.w32->text_cursor = cursor;

  if (nontext_cursor != f->output_data.w32->nontext_cursor
      && f->output_data.w32->nontext_cursor != 0)
    XFreeCursor (FRAME_W32_DISPLAY (f), f->output_data.w32->nontext_cursor);
  f->output_data.w32->nontext_cursor = nontext_cursor;

  if (hourglass_cursor != f->output_data.w32->hourglass_cursor
      && f->output_data.w32->hourglass_cursor != 0)
    XFreeCursor (FRAME_W32_DISPLAY (f), f->output_data.w32->hourglass_cursor);
  f->output_data.w32->hourglass_cursor = hourglass_cursor;

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

  update_face_from_frame_parameter (f, Qmouse_color, arg);
#endif /* TODO */
}

/* Defined in w32term.c. */
void x_update_cursor (struct frame *f, int on_p);

void
x_set_cursor_color (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  unsigned long fore_pixel, pixel;

  if (!NILP (Vx_cursor_fore_pixel))
    fore_pixel = x_decode_color (f, Vx_cursor_fore_pixel,
                                 WHITE_PIX_DEFAULT (f));
  else
    fore_pixel = FRAME_BACKGROUND_PIXEL (f);

  pixel = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  
  /* Make sure that the cursor color differs from the background color.  */
  if (pixel == FRAME_BACKGROUND_PIXEL (f))
    {
      pixel = f->output_data.w32->mouse_pixel;
      if (pixel == fore_pixel)
	fore_pixel = FRAME_BACKGROUND_PIXEL (f);
    }

  f->output_data.w32->cursor_foreground_pixel = fore_pixel;
  f->output_data.w32->cursor_pixel = pixel;

  if (FRAME_W32_WINDOW (f) != 0)
    {
      BLOCK_INPUT;
      /* Update frame's cursor_gc.  */
      f->output_data.w32->cursor_gc->foreground = fore_pixel;
      f->output_data.w32->cursor_gc->background = pixel;

      UNBLOCK_INPUT;

      if (FRAME_VISIBLE_P (f))
	{
	  x_update_cursor (f, 0);
	  x_update_cursor (f, 1);
	}
    }

  update_face_from_frame_parameter (f, Qcursor_color, arg);
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
  int pix;

  CHECK_STRING (arg);
  pix = x_decode_color (f, arg, BLACK_PIX_DEFAULT (f));
  x_set_border_pixel (f, pix);
  update_face_from_frame_parameter (f, Qborder_color, arg);
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
  f->output_data.w32->cursor_width = width;

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
    return XCDR (tem);
  else
    return Qnil;
}

void
x_set_icon_name (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
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
  int old_fontset = FRAME_FONTSET(f);

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
	  if (old_fontset == FRAME_FONTSET (f))
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
  int old = f->output_data.w32->internal_border_width;

  CHECK_NUMBER (arg);
  f->output_data.w32->internal_border_width = XINT (arg);
  if (f->output_data.w32->internal_border_width < 0)
    f->output_data.w32->internal_border_width = 0;

  if (f->output_data.w32->internal_border_width == old)
    return;

  if (FRAME_W32_WINDOW (f) != 0)
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
  int olines = FRAME_MENU_BAR_LINES (f);

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
      do_pending_window_change (0);
    }
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
  if (FRAME_W32_WINDOW (f) && FRAME_TOOL_BAR_LINES (f) == 0)
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
      {
        HDC hdc = get_frame_dc (f);
        w32_clear_area (f, hdc, 0, y, width, height);
        release_frame_dc (f, hdc);
      }
      UNBLOCK_INPUT;

      if (WINDOWP (f->tool_bar_window))
	clear_glyph_matrix (XWINDOW (f->tool_bar_window)->current_matrix);
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
    CHECK_STRING (name);

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
      if (STRING_MULTIBYTE (name))
	name = ENCODE_SYSTEM (name);

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

  if (FRAME_W32_WINDOW (f))
    {
      if (STRING_MULTIBYTE (name))
	name = ENCODE_SYSTEM (name);

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
      do_pending_window_change (0);
    }
}

void
x_set_scroll_bar_width (f, arg, oldval)
     struct frame *f;
     Lisp_Object arg, oldval;
{
  int wid = FONT_WIDTH (f->output_data.w32->font);

  if (NILP (arg))
    {
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = GetSystemMetrics (SM_CXVSCROLL);
      FRAME_SCROLL_BAR_COLS (f) = (FRAME_SCROLL_BAR_PIXEL_WIDTH (f) +
                                   wid - 1) / wid;
      if (FRAME_W32_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
      do_pending_window_change (0);
    }
  else if (INTEGERP (arg) && XINT (arg) > 0
	   && XFASTINT (arg) != FRAME_SCROLL_BAR_PIXEL_WIDTH (f))
    {
      FRAME_SCROLL_BAR_PIXEL_WIDTH (f) = XFASTINT (arg);
      FRAME_SCROLL_BAR_COLS (f) = (FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
                                   + wid-1) / wid;
      if (FRAME_W32_WINDOW (f))
	x_set_window_size (f, 0, FRAME_WIDTH (f), FRAME_HEIGHT (f));
      do_pending_window_change (0);
    }
  change_frame_size (f, 0, FRAME_WIDTH (f), 0, 0, 0);
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.hpos = 0;
  XWINDOW (FRAME_SELECTED_WINDOW (f))->cursor.x = 0; 
}

/* Subroutines of creating an frame.  */

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

  class_key = (char *) alloca ((sizeof (EMACS_CLASS) - 1)
			       + STRING_BYTES (XSTRING (class))
			       + (STRINGP (subclass)
				  ? STRING_BYTES (XSTRING (subclass)) : 0)
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

  return x_get_string_resource (sf, name_key, class_key);
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
   w32_get_arg, make sure you deal with Qunbound in a reasonable way,
   and don't let it get stored in any Lisp-visible variables!  */

static Lisp_Object
w32_get_arg (alist, param, attribute, class, type)
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

  tem = w32_get_arg (alist, prop, xprop, xclass, type);
  if (EQ (tem, Qunbound))
    tem = deflt;
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

  /* Default values if we fall through.
     Actually, if that happens we should get
     window manager prompting.  */
  SET_FRAME_WIDTH (f, DEFAULT_COLS);
  f->height = DEFAULT_ROWS;
  /* Window managers expect that if program-specified
     positions are not (0,0), they're intentional, not defaults.  */
  f->output_data.w32->top_pos = 0;
  f->output_data.w32->left_pos = 0;

  /* Ensure that old new_width and new_height will not override the
     values set here.  */
  FRAME_NEW_WIDTH (f) = 0;
  FRAME_NEW_HEIGHT (f) = 0;

  tem0 = w32_get_arg (parms, Qheight, 0, 0, RES_TYPE_NUMBER);
  tem1 = w32_get_arg (parms, Qwidth, 0, 0, RES_TYPE_NUMBER);
  tem2 = w32_get_arg (parms, Quser_size, 0, 0, RES_TYPE_NUMBER);
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

  f->output_data.w32->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.w32->font)));

  x_compute_fringe_widths (f, 0);

  f->output_data.w32->pixel_width = CHAR_TO_PIXEL_WIDTH (f, f->width);
  f->output_data.w32->pixel_height = CHAR_TO_PIXEL_HEIGHT (f, f->height);

  tem0 = w32_get_arg (parms, Qtop, 0, 0, RES_TYPE_NUMBER);
  tem1 = w32_get_arg (parms, Qleft, 0, 0, RES_TYPE_NUMBER);
  tem2 = w32_get_arg (parms, Quser_position, 0, 0, RES_TYPE_NUMBER);
  if (! EQ (tem0, Qunbound) || ! EQ (tem1, Qunbound))
    {
      if (EQ (tem0, Qminus))
	{
	  f->output_data.w32->top_pos = 0;
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qminus)
	       && CONSP (XCDR (tem0))
	       && INTEGERP (XCAR (XCDR (tem0))))
	{
	  f->output_data.w32->top_pos = - XINT (XCAR (XCDR (tem0)));
	  window_prompting |= YNegative;
	}
      else if (CONSP (tem0) && EQ (XCAR (tem0), Qplus)
	       && CONSP (XCDR (tem0))
	       && INTEGERP (XCAR (XCDR (tem0))))
	{
	  f->output_data.w32->top_pos = XINT (XCAR (XCDR (tem0)));
	}
      else if (EQ (tem0, Qunbound))
	f->output_data.w32->top_pos = 0;
      else
	{
	  CHECK_NUMBER (tem0);
	  f->output_data.w32->top_pos = XINT (tem0);
	  if (f->output_data.w32->top_pos < 0)
	    window_prompting |= YNegative;
	}

      if (EQ (tem1, Qminus))
	{
	  f->output_data.w32->left_pos = 0;
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qminus)
	       && CONSP (XCDR (tem1))
	       && INTEGERP (XCAR (XCDR (tem1))))
	{
	  f->output_data.w32->left_pos = - XINT (XCAR (XCDR (tem1)));
	  window_prompting |= XNegative;
	}
      else if (CONSP (tem1) && EQ (XCAR (tem1), Qplus)
	       && CONSP (XCDR (tem1))
	       && INTEGERP (XCAR (XCDR (tem1))))
	{
	  f->output_data.w32->left_pos = XINT (XCAR (XCDR (tem1)));
	}
      else if (EQ (tem1, Qunbound))
	f->output_data.w32->left_pos = 0;
      else
	{
	  CHECK_NUMBER (tem1);
	  f->output_data.w32->left_pos = XINT (tem1);
	  if (f->output_data.w32->left_pos < 0)
	    window_prompting |= XNegative;
	}

      if (!NILP (tem2) && ! EQ (tem2, Qunbound))
	window_prompting |= USPosition;
      else
	window_prompting |= PPosition;
    }

  if (f->output_data.w32->want_fullscreen != FULLSCREEN_NONE)
    {
      int left, top;
      int width, height;

      /* It takes both for some WM:s to place it where we want */
      window_prompting = USPosition | PPosition;
      x_fullscreen_adjust (f, &width, &height, &top, &left);
      f->width = width;
      f->height = height;
      f->output_data.w32->pixel_width = CHAR_TO_PIXEL_WIDTH (f, f->width);
      f->output_data.w32->pixel_height = CHAR_TO_PIXEL_HEIGHT (f, f->height);
      f->output_data.w32->left_pos = left;
      f->output_data.w32->top_pos = top;
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
			XINT(bar->left) + VERTICAL_SCROLL_BAR_WIDTH_TRIM,
                        XINT(bar->top), 
			XINT(bar->width) - VERTICAL_SCROLL_BAR_WIDTH_TRIM * 2,
                        XINT(bar->height),
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
      SetWindowLong (hwnd, WND_BACKGROUND_INDEX, FRAME_BACKGROUND_PIXEL (f));

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
	      XSETCAR ((Lisp_Object) msg.lParam, Qnil);
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
			&& ((XUINT (new_state)) & 1) != cur_state))
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
          HDC hdc = get_frame_dc (f);
	  GetUpdateRect (hwnd, &wmsg.rect, FALSE);
	  w32_clear_rect (f, hdc, &wmsg.rect);
          release_frame_dc (f, hdc);

#if defined (W32_DEBUG_DISPLAY)
          DebPrint (("WM_ERASEBKGND (frame %p): erasing %d,%d-%d,%d\n",
		     f,
                     wmsg.rect.left, wmsg.rect.top,
		     wmsg.rect.right, wmsg.rect.bottom));
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
	bzero (&update_rect, sizeof (update_rect));

	f = x_window_to_frame (dpyinfo, hwnd);
	if (f == 0)
	  {
            DebPrint (("WM_PAINT received for unknown window %p\n", hwnd));
	    return 0;
	  }

        /* MSDN Docs say not to call BeginPaint if GetUpdateRect
           fails.  Apparently this can happen under some
           circumstances.  */
        if (GetUpdateRect (hwnd, &update_rect, FALSE) || !w32_strict_painting)
          {
            enter_crit ();
            BeginPaint (hwnd, &paintStruct);

	    /* The rectangles returned by GetUpdateRect and BeginPaint
	       do not always match.  Play it safe by assuming both areas
	       are invalid.  */
	    UnionRect (&(wmsg.rect), &update_rect, &(paintStruct.rcPaint));

#if defined (W32_DEBUG_DISPLAY)
            DebPrint (("WM_PAINT (frame %p): painting %d,%d-%d,%d\n",
		       f,
		       wmsg.rect.left, wmsg.rect.top,
		       wmsg.rect.right, wmsg.rect.bottom));
            DebPrint (("  [update region is %d,%d-%d,%d]\n",
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
      if (XINT (Vw32_num_mouse_buttons) > 2)
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
      if (XINT (Vw32_num_mouse_buttons) > 2)
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

    case WM_XBUTTONDOWN:
    case WM_XBUTTONUP:
      if (w32_pass_extra_mouse_buttons_to_system)
	goto dflt;
      /* else fall through and process them.  */
    case WM_MBUTTONDOWN:
    case WM_MBUTTONUP:
    handle_plain_button:
      {
	BOOL up;
	int button;

	if (parse_button (msg, HIWORD (wParam), &button, &up))
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

      /* Need to return true for XBUTTON messages, false for others,
         to indicate that we processed the message.  */
      return (msg == WM_XBUTTONDOWN || msg == WM_XBUTTONUP);

    case WM_MOUSEMOVE:
      /* If the mouse has just moved into the frame, start tracking
	 it, so we will be notified when it leaves the frame.  Mouse
	 tracking only works under W98 and NT4 and later. On earlier
	 versions, there is no way of telling when the mouse leaves the
	 frame, so we just have to put up with help-echo and mouse
	 highlighting remaining while the frame is not active.  */
      if (track_mouse_event_fn && !track_mouse_window)
	{
	  TRACKMOUSEEVENT tme;
	  tme.cbSize = sizeof (tme);
	  tme.dwFlags = TME_LEAVE;
	  tme.hwndTrack = hwnd;

	  track_mouse_event_fn (&tme);
	  track_mouse_window = hwnd;
	}
    case WM_VSCROLL:
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
      else if (wParam == menu_free_timer)
	{
	  KillTimer (hwnd, menu_free_timer);
	  menu_free_timer = 0;
	  f = x_window_to_frame (dpyinfo, hwnd);
	  if (!f->output_data.w32->menu_command_in_progress)
	    {
	      /* Free memory used by owner-drawn and help-echo strings.  */
	      w32_free_menu_strings (hwnd);
	      f->output_data.w32->menubar_active = 0;
	    }
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

      /* If a menu command is not already in progress, check again
	 after a short delay, since Windows often (always?) sends the
	 WM_EXITMENULOOP before the corresponding WM_COMMAND message.  */
      if (f && !f->output_data.w32->menu_command_in_progress)
	menu_free_timer = SetTimer (hwnd, MENU_FREE_ID, MENU_FREE_DELAY, NULL);
      goto dflt;

    case WM_MENUSELECT:
      /* Direct handling of help_echo in menus.  Should be safe now
	 that we generate the help_echo by placing a help event in the
	 keyboard buffer.  */
      {
	HMENU menu = (HMENU) lParam;
	UINT menu_item = (UINT) LOWORD (wParam);
	UINT flags = (UINT) HIWORD (wParam);

	w32_menu_display_help (hwnd, menu, menu_item, flags);
      }
      return 0;

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

              pMis->itemHeight = GetSystemMetrics (SM_CYMENUSIZE);
              if (title)
                {
                  GetTextExtentPoint32 (hdc, title, strlen (title), &size);
                  pMis->itemWidth = size.cx;
                  if (pMis->itemHeight < size.cy)
                    pMis->itemHeight = size.cy;
                }
              else
                pMis->itemWidth = 0;

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
              if (title)
                {
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
                              pDis->rcItem.left
                              + GetSystemMetrics (SM_CXMENUCHECK),
                              pDis->rcItem.top,
                              ETO_OPAQUE, &pDis->rcItem,
                              title, strlen (title), NULL);

                  SelectObject (hdc, old_font);
                  DeleteObject (menu_font);
                }
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

    case WM_MOUSELEAVE:
      /* No longer tracking mouse.  */
      track_mouse_window = NULL;

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
      /* Relinquish the system caret.  */
      if (w32_system_caret_hwnd)
	{
	  w32_visible_system_caret_hwnd = NULL;
	  w32_system_caret_hwnd = NULL;
	  DestroyCaret ();
	}
      goto command;
    case WM_COMMAND:
      f = x_window_to_frame (dpyinfo, hwnd);
      if (f && HIWORD (wParam) == 0)
	{
	  f->output_data.w32->menu_command_in_progress = 1;
	  if (menu_free_timer)
	    {
	      KillTimer (hwnd, menu_free_timer);
	      menu_free_timer = 0;	  
	    }
	}
    case WM_MOVE:
    case WM_SIZE:
    command:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      goto dflt;

    case WM_CLOSE:
      wmsg.dwModifiers = w32_get_modifiers ();
      my_post_msg (&wmsg, hwnd, msg, wParam, lParam);
      return 0;

    case WM_WINDOWPOSCHANGING:
      /* Don't restrict the sizing of tip frames.  */
      if (hwnd == tip_window)
	return 0;
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
      /* Hack to allow resizing the Emacs frame above the screen size.
	 Note that Windows 9x limits coordinates to 16-bits.  */
      ((LPMINMAXINFO) lParam)->ptMaxTrackSize.x = 32767;
      ((LPMINMAXINFO) lParam)->ptMaxTrackSize.y = 32767;
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

    case WM_EMACS_HIDE_CARET:
      return HideCaret (hwnd);

    case WM_EMACS_SHOW_CARET:
      return ShowCaret (hwnd);

    case WM_EMACS_DESTROY_CARET:
      w32_system_caret_hwnd = NULL;
      w32_visible_system_caret_hwnd = NULL;
      return DestroyCaret ();

    case WM_EMACS_TRACK_CARET:
      /* If there is currently no system caret, create one.  */
      if (w32_system_caret_hwnd == NULL)
	{
	  /* Use the default caret width, and avoid changing it
	     unneccesarily, as it confuses screen reader software.  */
	  w32_system_caret_hwnd = hwnd;
	  CreateCaret (hwnd, NULL, 0,
		       w32_system_caret_height);
	}
      
      if (!SetCaretPos (w32_system_caret_x, w32_system_caret_y))
	return 0;
      /* Ensure visible caret gets turned on when requested.  */
      else if (w32_use_visible_system_caret
	       && w32_visible_system_caret_hwnd != hwnd)
	{
	  w32_visible_system_caret_hwnd = hwnd;
	  return ShowCaret (hwnd);
	}
      /* Ensure visible caret gets turned off when requested.  */
      else if (!w32_use_visible_system_caret
	       && w32_visible_system_caret_hwnd)
	{
	  w32_visible_system_caret_hwnd = NULL;
	  return HideCaret (hwnd);
	}
      else
	return 1;

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


/* Create a tooltip window. Unlike my_create_window, we do not do this
   indirectly via the Window thread, as we do not need to process Window
   messages for the tooltip.  Creating tooltips indirectly also creates
   deadlocks when tooltips are created for menu items.  */
void 
my_create_tip_window (f)
     struct frame *f;
{
  RECT rect;

  rect.left = rect.top = 0;
  rect.right = PIXEL_WIDTH (f);
  rect.bottom = PIXEL_HEIGHT (f);

  AdjustWindowRect (&rect, f->output_data.w32->dwStyle,
		    FRAME_EXTERNAL_MENU_BAR (f));

  tip_window = FRAME_W32_WINDOW (f)
    = CreateWindow (EMACS_CLASS,
		    f->namebuf,
		    f->output_data.w32->dwStyle,
		    f->output_data.w32->left_pos,
		    f->output_data.w32->top_pos,
		    rect.right - rect.left,
		    rect.bottom - rect.top,
		    FRAME_W32_WINDOW (SELECTED_FRAME ()), /* owner */
		    NULL,
		    hinst,
		    NULL);

  if (tip_window)
    {
      SetWindowLong (tip_window, WND_FONTWIDTH_INDEX, FONT_WIDTH (f->output_data.w32->font));
      SetWindowLong (tip_window, WND_LINEHEIGHT_INDEX, f->output_data.w32->line_height);
      SetWindowLong (tip_window, WND_BORDER_INDEX, f->output_data.w32->internal_border_width);
      SetWindowLong (tip_window, WND_BACKGROUND_INDEX, FRAME_BACKGROUND_PIXEL (f));

      /* Tip frames have no scrollbars.  */
      SetWindowLong (tip_window, WND_SCROLLBAR_INDEX, 0);

      /* Do this to discard the default setting specified by our parent. */
      ShowWindow (tip_window, SW_HIDE);
    }
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
  icon_x = w32_get_arg (parms, Qicon_left, 0, 0, RES_TYPE_NUMBER);
  icon_y = w32_get_arg (parms, Qicon_top, 0, 0, RES_TYPE_NUMBER);
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

#if 0 /* TODO */
  /* Start up iconic or window? */
  x_wm_set_window_state
    (f, (EQ (w32_get_arg (parms, Qvisibility, 0, 0, RES_TYPE_SYMBOL), Qicon)
	 ? IconicState
	 : NormalState));

  x_text_icon (f, (char *) XSTRING ((!NILP (f->icon_name)
				     ? f->icon_name
				     : f->name))->data);
#endif

  UNBLOCK_INPUT;
}


static void
x_make_gc (f)
     struct frame *f;
{
  XGCValues gc_values;

  BLOCK_INPUT;

  /* Create the GC's of this frame.
     Note that many default values are used.  */

  /* Normal video */
  gc_values.font = f->output_data.w32->font;

  /* Cursor has cursor-color background, background-color foreground.  */
  gc_values.foreground = FRAME_BACKGROUND_PIXEL (f);
  gc_values.background = f->output_data.w32->cursor_pixel;
  f->output_data.w32->cursor_gc
    = XCreateGC (NULL, FRAME_W32_WINDOW (f),
		 (GCFont | GCForeground | GCBackground),
		 &gc_values);

  /* Reliefs.  */
  f->output_data.w32->white_relief.gc = 0;
  f->output_data.w32->black_relief.gc = 0;

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
#ifdef GLYPH_DEBUG
      struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
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
       doc: /* Make a new window, which is called a \"frame\" in Emacs terms.
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
  struct w32_display_info *dpyinfo = NULL;
  Lisp_Object parent;
  struct kboard *kb;

  check_w32 ();

  /* Use this general default value to start with
     until we know if this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

  display = w32_get_arg (parms, Qdisplay, 0, 0, RES_TYPE_STRING);
  if (EQ (display, Qunbound))
    display = Qnil;
  dpyinfo = check_x_display_info (display);
#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  name = w32_get_arg (parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && ! EQ (name, Qunbound)
      && ! NILP (name))
    error ("Invalid frame name--not a string or nil");

  if (STRINGP (name))
    Vx_resource_name = name;

  /* See if parent window is specified.  */
  parent = w32_get_arg (parms, Qparent_id, NULL, NULL, RES_TYPE_NUMBER);
  if (EQ (parent, Qunbound))
    parent = Qnil;
  if (! NILP (parent))
    CHECK_NUMBER (parent);

  /* make_frame_without_minibuffer can run Lisp code and garbage collect.  */
  /* No need to protect DISPLAY because that's not used after passing
     it to make_frame_without_minibuffer.  */
  frame = Qnil;
  GCPRO4 (parms, parent, name, frame);
  tem = w32_get_arg (parms, Qminibuffer, "minibuffer", "Minibuffer",
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

  /* Note that Windows does support scroll bars.  */
  FRAME_CAN_HAVE_SCROLL_BARS (f) = 1;
  /* By default, make scrollbars the system standard width. */
  f->scroll_bar_pixel_width = GetSystemMetrics (SM_CXVSCROLL);

  f->output_method = output_w32;
  f->output_data.w32 =
    (struct w32_output *) xmalloc (sizeof (struct w32_output));
  bzero (f->output_data.w32, sizeof (struct w32_output));
  FRAME_FONTSET (f) = -1;
  record_unwind_protect (unwind_create_frame, frame);

  f->icon_name
    = w32_get_arg (parms, Qicon_name, "iconName", "Title", RES_TYPE_STRING);
  if (! STRINGP (f->icon_name))
    f->icon_name = Qnil;

/*  FRAME_W32_DISPLAY_INFO (f) = dpyinfo; */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif

  /* Specify the parent under which to make this window.  */

  if (!NILP (parent))
    {
      f->output_data.w32->parent_desc = (Window) XFASTINT (parent);
      f->output_data.w32->explicit_parent = 1;
    }
  else
    {
      f->output_data.w32->parent_desc = FRAME_W32_DISPLAY_INFO (f)->root_window;
      f->output_data.w32->explicit_parent = 0;
    }

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

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  {
    Lisp_Object font;

    font = w32_get_arg (parms, Qfont, "font", "Font", RES_TYPE_STRING);

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
      font = x_new_font (f, "-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-Courier-normal-r-*-*-13-*-*-*-c-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (! STRINGP (font))
      font = x_new_font (f, "-*-Fixedsys-normal-r-*-*-12-*-*-*-c-*-iso8859-1");
    UNBLOCK_INPUT;
    if (! STRINGP (font))
      font = build_string ("Fixedsys");

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

      value = w32_get_arg (parms, Qinternal_border_width,
			 "internalBorder", "InternalBorder", RES_TYPE_NUMBER);
      if (! EQ (value, Qunbound))
	parms = Fcons (Fcons (Qinternal_border_width, value),
		       parms);
    }
  /* Default internalBorderWidth to 0 on Windows to match other programs.  */
  x_default_parameter (f, parms, Qinternal_border_width, make_number (0),
		       "internalBorderWidth", "InternalBorder", RES_TYPE_NUMBER);
  x_default_parameter (f, parms, Qvertical_scroll_bars, Qright,
		       "verticalScrollBars", "ScrollBars", RES_TYPE_SYMBOL);

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
		       "bufferPredicate", "BufferPredicate", RES_TYPE_SYMBOL);
  x_default_parameter (f, parms, Qtitle, Qnil,
		       "title", "Title", RES_TYPE_STRING);
  x_default_parameter (f, parms, Qfullscreen, Qnil,
                       "fullscreen", "Fullscreen", RES_TYPE_SYMBOL);

  f->output_data.w32->dwStyle = WS_OVERLAPPEDWINDOW;
  f->output_data.w32->parent_desc = FRAME_W32_DISPLAY_INFO (f)->root_window;

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

  tem = w32_get_arg (parms, Qunsplittable, 0, 0, RES_TYPE_BOOLEAN);
  f->no_split = minibuffer_only || EQ (tem, Qt);

  w32_window (f, window_prompting, minibuffer_only);
  x_icon (f, parms);

  x_make_gc (f);

  /* Now consider the frame official.  */
  FRAME_W32_DISPLAY_INFO (f)->reference_count++;
  Vframe_list = Fcons (frame, Vframe_list);

  /* We need to do this after creating the window, so that the
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
		       "scrollBarWidth", "ScrollBarWidth", RES_TYPE_NUMBER);

  /* Dimensions, especially f->height, must be done via change_frame_size.
     Change will not be effected unless different from the current
     f->height.  */
  width = f->width;
  height = f->height;

  f->height = 0;
  SET_FRAME_WIDTH (f, 0);
  change_frame_size (f, height, width, 1, 0, 0);

  /* Tell the server what size and position, etc, we want, and how
     badly we want them.  This should be done after we have the menu
     bar so that its size can be taken into account.  */
  BLOCK_INPUT;
  x_wm_set_size_hint (f, window_prompting, 0);
  UNBLOCK_INPUT;

  /* Avoid a bug that causes the new frame to never become visible if
     an echo area message is displayed during the following call1.  */
  specbind(Qredisplay_dont_pause, Qt);

  /* Set up faces after all frame parameters are known.  This call
     also merges in face attributes specified for new frames.  If we
     don't do this, the `menu' face for instance won't have the right
     colors, and the menu bar won't appear in the specified colors for
     new frames.  */
  call1 (Qface_set_after_frame_default, frame);

  /* Make the window appear on the frame and enable display, unless
     the caller says not to.  However, with explicit parent, Emacs
     cannot control visibility, so don't try.  */
  if (! f->output_data.w32->explicit_parent)
    {
      Lisp_Object visibility;

      visibility = w32_get_arg (parms, Qvisibility, 0, 0, RES_TYPE_SYMBOL);
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
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (frame);
  Lisp_Object xfocus;
  if (! dpyinfo->w32_focus_frame)
    return Qnil;

  XSETFRAME (xfocus, dpyinfo->w32_focus_frame);
  return xfocus;
}

DEFUN ("w32-focus-frame", Fw32_focus_frame, Sw32_focus_frame, 1, 1, 0,
       doc: /* Give FRAME input focus, raising to foreground if necessary.  */)
  (frame)
     Lisp_Object frame;
{
  x_focus_on_frame (check_x_frame (frame));
  return Qnil;
}


/* Return the charset portion of a font name.  */
char * xlfd_charset_of_font (char * fontname)
{
  char *charset, *encoding;

  encoding = strrchr(fontname, '-');
  if (!encoding || encoding == fontname)
    return NULL;

  for (charset = encoding - 1; charset >= fontname; charset--)
    if (*charset == '-')
      break;

  if (charset == fontname || strcmp(charset, "-*-*") == 0)
    return NULL;

  return charset + 1;
}

struct font_info *w32_load_bdf_font (struct frame *f, char *fontname,
                                     int size, char* filename);
static Lisp_Object w32_list_bdf_fonts (Lisp_Object pattern, int max_names);
static BOOL w32_to_x_font (LOGFONT * lplf, char * lpxstr, int len,
			   char * charset);
static BOOL x_to_w32_font (char *lpxstr, LOGFONT *lplogfont);

static struct font_info *
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

      /* First check if any are already loaded, as that is cheaper
         than loading another one. */
      for (i = 0; i < dpyinfo->n_fonts; i++)
	for (tail = font_names; CONSP (tail); tail = XCDR (tail))
	  if (dpyinfo->font_table[i].name
              && (!strcmp (dpyinfo->font_table[i].name,
                           XSTRING (XCAR (tail))->data)
                  || !strcmp (dpyinfo->font_table[i].full_name,
                              XSTRING (XCAR (tail))->data)))
	    return (dpyinfo->font_table + i);

      fontname = (char *) XSTRING (XCAR (font_names))->data;
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
    char *full_name, *encoding, *charset;
    XFontStruct *font;
    struct font_info *fontp;
    LOGFONT lf;
    BOOL ok;
    int codepage;
    int i;

    if (!fontname || !x_to_w32_font (fontname, &lf))
      return (NULL);

    if (!*lf.lfFaceName)
        /* If no name was specified for the font, we get a random font
           from CreateFontIndirect - this is not particularly
           desirable, especially since CreateFontIndirect does not
           fill out the missing name in lf, so we never know what we
           ended up with. */
      return NULL;

    /* Specify anti-aliasing to prevent Cleartype fonts being used,
       since those fonts leave garbage behind.  */
    lf.lfQuality = ANTIALIASED_QUALITY;

    font = (XFontStruct *) xmalloc (sizeof (XFontStruct));
    bzero (font, sizeof (*font));

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

        codepage = w32_codepage_for_font (fontname);

	hdc = GetDC (dpyinfo->root_window);
	oldobj = SelectObject (hdc, font->hfont);

	ok = GetTextMetrics (hdc, &font->tm);
        if (codepage == CP_UNICODE)
          font->double_byte_p = 1;
        else
	  {
	    /* Unfortunately, some fonts (eg. MingLiU, a big5 ttf font)
               don't report themselves as double byte fonts, when
               patently they are.  So instead of trusting
               GetFontLanguageInfo, we check the properties of the
               codepage directly, since that is ultimately what we are
               working from anyway.  */
	    /* font->double_byte_p = GetFontLanguageInfo(hdc) & GCP_DBCS; */
	    CPINFO cpi = {0};
	    GetCPInfo (codepage, &cpi);
	    font->double_byte_p = cpi.MaxCharSize > 1;
	  }

	SelectObject (hdc, oldobj);
	ReleaseDC (dpyinfo->root_window, hdc);
        /* Fill out details in lf according to the font that was
           actually loaded.  */
        lf.lfHeight = font->tm.tmInternalLeading - font->tm.tmHeight;
        lf.lfWidth = font->tm.tmAveCharWidth;
        lf.lfWeight = font->tm.tmWeight;
        lf.lfItalic = font->tm.tmItalic;
        lf.lfCharSet = font->tm.tmCharSet;
        lf.lfPitchAndFamily = ((font->tm.tmPitchAndFamily & TMPF_FIXED_PITCH)
                               ? VARIABLE_PITCH : FIXED_PITCH);
        lf.lfOutPrecision = ((font->tm.tmPitchAndFamily & TMPF_VECTOR)
                             ? OUT_STROKE_PRECIS : OUT_STRING_PRECIS);

	w32_cache_char_metrics (font);
      }

    UNBLOCK_INPUT;

    if (!ok)
      {
	w32_unload_font (dpyinfo, font);
	return (NULL);
      }

    /* Find a free slot in the font table.  */
    for (i = 0; i < dpyinfo->n_fonts; ++i)
      if (dpyinfo->font_table[i].name == NULL)
	break;

    /* If no free slot found, maybe enlarge the font table.  */
    if (i == dpyinfo->n_fonts
	&& dpyinfo->n_fonts == dpyinfo->font_table_size)
      {
	int sz;
	dpyinfo->font_table_size = max (16, 2 * dpyinfo->font_table_size);
	sz = dpyinfo->font_table_size * sizeof *dpyinfo->font_table;
	dpyinfo->font_table
	  = (struct font_info *) xrealloc (dpyinfo->font_table, sz);
      }

    fontp = dpyinfo->font_table + i;
    if (i == dpyinfo->n_fonts)
      ++dpyinfo->n_fonts;

    /* Now fill in the slots of *FONTP.  */
    BLOCK_INPUT;
    fontp->font = font;
    fontp->font_idx = i;
    fontp->name = (char *) xmalloc (strlen (fontname) + 1);
    bcopy (fontname, fontp->name, strlen (fontname) + 1);

    charset = xlfd_charset_of_font (fontname);

  /* Cache the W32 codepage for a font.  This makes w32_encode_char
     (called for every glyph during redisplay) much faster.  */
    fontp->codepage = codepage;

    /* Work out the font's full name.  */
    full_name = (char *)xmalloc (100);
    if (full_name && w32_to_x_font (&lf, full_name, 100, charset))
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
       the font code-points (0:0x20..0x7F, 1:0xA0..0xFF), or
       (0:0x20..0x7F, 1:0xA0..0xFF,
       (0:0x2020..0x7F7F, 1:0xA0A0..0xFFFF, 3:0x20A0..0x7FFF,
       2:0xA020..0xFF7F).  For the moment, we don't know which charset
       uses this font.  So, we set information in fontp->encoding[1]
       which is never used by any charset.  If mapping can't be
       decided, set FONT_ENCODING_NOT_DECIDED.  */

    /* SJIS fonts need to be set to type 4, all others seem to work as
       type FONT_ENCODING_NOT_DECIDED.  */
    encoding = strrchr (fontp->name, '-');
    if (encoding && strnicmp (encoding+1, "sjis", 4) == 0)
      fontp->encoding[1] = 4;
    else
      fontp->encoding[1] = FONT_ENCODING_NOT_DECIDED;

    /* The following three values are set to 0 under W32, which is
       what they get set to if XGetFontProperty fails under X.  */
    fontp->baseline_offset = 0;
    fontp->relative_compose = 0;
    fontp->default_ascent = 0;

    /* Set global flag fonts_changed_p to non-zero if the font loaded
       has a character with a smaller width than any other character
       before, or if the font loaded has a smaller height than any
       other font loaded before.  If this happens, it will make a
       glyph matrix reallocation necessary.  */
    fonts_changed_p |= x_compute_min_glyph_bounds (f);
    UNBLOCK_INPUT;
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

  bdf_fonts = w32_list_bdf_fonts (build_string (fontname), 1);

  while (!retval && CONSP (bdf_fonts))
    {
      char *bdf_name, *bdf_file;
      Lisp_Object bdf_pair;

      bdf_name = XSTRING (XCAR (bdf_fonts))->data;
      bdf_pair = Fassoc (XCAR (bdf_fonts), Vw32_bdf_filename_alist);
      bdf_file = XSTRING (XCDR (bdf_pair))->data;

      retval = w32_load_bdf_font (f, bdf_name, size, bdf_file);

      bdf_fonts = XCDR (bdf_fonts);
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
      if (font->per_char) xfree (font->per_char);
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
 */

static LONG 
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


static char * 
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

static LONG
x_to_w32_charset (lpcs)
    char * lpcs;
{
  Lisp_Object this_entry, w32_charset;
  char *charset;
  int len = strlen (lpcs);

  /* Support "*-#nnn" format for unknown charsets.  */
  if (strncmp (lpcs, "*-#", 3) == 0)
    return atoi (lpcs + 3);

  /* Handle wildcards by ignoring them; eg. treat "big5*-*" as "big5".  */
  charset = alloca (len + 1);
  strcpy (charset, lpcs);
  lpcs = strchr (charset, '*');
  if (lpcs)
    *lpcs = 0;

  /* Look through w32-charset-info-alist for the character set.
     Format of each entry is
       (CHARSET_NAME . (WINDOWS_CHARSET . CODEPAGE)).
  */
  this_entry = Fassoc (build_string(charset), Vw32_charset_info_alist);

  if (NILP(this_entry))
    {
      /* At startup, we want iso8859-1 fonts to come up properly. */
      if (stricmp(charset, "iso8859-1") == 0)
        return ANSI_CHARSET;
      else
        return DEFAULT_CHARSET;
    }

  w32_charset = Fcar (Fcdr (this_entry));

  /* Translate Lisp symbol to number.  */
  if (w32_charset == Qw32_charset_ansi)
    return ANSI_CHARSET;
  if (w32_charset == Qw32_charset_symbol)
    return SYMBOL_CHARSET;
  if (w32_charset == Qw32_charset_shiftjis)
    return SHIFTJIS_CHARSET;
  if (w32_charset == Qw32_charset_hangeul)
    return HANGEUL_CHARSET;
  if (w32_charset == Qw32_charset_chinesebig5)
    return CHINESEBIG5_CHARSET;
  if (w32_charset == Qw32_charset_gb2312)
    return GB2312_CHARSET;
  if (w32_charset == Qw32_charset_oem)
    return OEM_CHARSET;
#ifdef JOHAB_CHARSET
  if (w32_charset == Qw32_charset_johab)
    return JOHAB_CHARSET;
  if (w32_charset == Qw32_charset_easteurope)
    return EASTEUROPE_CHARSET;
  if (w32_charset == Qw32_charset_turkish)
    return TURKISH_CHARSET;
  if (w32_charset == Qw32_charset_baltic)
    return BALTIC_CHARSET;
  if (w32_charset == Qw32_charset_russian)
    return RUSSIAN_CHARSET;
  if (w32_charset == Qw32_charset_arabic)
    return ARABIC_CHARSET;
  if (w32_charset == Qw32_charset_greek)
    return GREEK_CHARSET;
  if (w32_charset == Qw32_charset_hebrew)
    return HEBREW_CHARSET;
  if (w32_charset == Qw32_charset_vietnamese)
    return VIETNAMESE_CHARSET;
  if (w32_charset == Qw32_charset_thai)
    return THAI_CHARSET;
  if (w32_charset == Qw32_charset_mac)
    return MAC_CHARSET;
#endif /* JOHAB_CHARSET */
#ifdef UNICODE_CHARSET
  if (w32_charset == Qw32_charset_unicode)
    return UNICODE_CHARSET;
#endif

  return DEFAULT_CHARSET;
}


static char *
w32_to_x_charset (fncharset)
    int fncharset;
{
  static char buf[32];
  Lisp_Object charset_type;

  switch (fncharset)
    {
    case ANSI_CHARSET:
      /* Handle startup case of w32-charset-info-alist not
         being set up yet. */
      if (NILP(Vw32_charset_info_alist))
        return "iso8859-1";
      charset_type = Qw32_charset_ansi;
      break;
    case DEFAULT_CHARSET:
      charset_type = Qw32_charset_default;
      break;
    case SYMBOL_CHARSET:
      charset_type = Qw32_charset_symbol;
      break;
    case SHIFTJIS_CHARSET:
      charset_type = Qw32_charset_shiftjis;
      break;
    case HANGEUL_CHARSET:
      charset_type = Qw32_charset_hangeul;
      break;
    case GB2312_CHARSET:
      charset_type = Qw32_charset_gb2312;
      break;
    case CHINESEBIG5_CHARSET:
      charset_type = Qw32_charset_chinesebig5;
      break;
    case OEM_CHARSET:
      charset_type = Qw32_charset_oem;
      break;

      /* More recent versions of Windows (95 and NT4.0) define more
         character sets.  */
#ifdef EASTEUROPE_CHARSET
    case EASTEUROPE_CHARSET:
      charset_type = Qw32_charset_easteurope;
      break;
    case TURKISH_CHARSET:
      charset_type = Qw32_charset_turkish;
      break;
    case BALTIC_CHARSET:
      charset_type = Qw32_charset_baltic;
      break;
    case RUSSIAN_CHARSET:
      charset_type = Qw32_charset_russian;
      break;
    case ARABIC_CHARSET:
      charset_type = Qw32_charset_arabic;
      break;
    case GREEK_CHARSET:
      charset_type = Qw32_charset_greek;
      break;
    case HEBREW_CHARSET:
      charset_type = Qw32_charset_hebrew;
      break;
    case VIETNAMESE_CHARSET:
      charset_type = Qw32_charset_vietnamese;
      break;
    case THAI_CHARSET:
      charset_type = Qw32_charset_thai;
      break;
    case MAC_CHARSET:
      charset_type = Qw32_charset_mac;
      break;
    case JOHAB_CHARSET:
      charset_type = Qw32_charset_johab;
      break;
#endif

#ifdef UNICODE_CHARSET
    case UNICODE_CHARSET:
      charset_type = Qw32_charset_unicode;
      break;
#endif
    default:
      /* Encode numerical value of unknown charset.  */
      sprintf (buf, "*-#%u", fncharset);
      return buf;
    }
  
  {
    Lisp_Object rest;
    char * best_match = NULL;

    /* Look through w32-charset-info-alist for the character set.
       Prefer ISO codepages, and prefer lower numbers in the ISO
       range. Only return charsets for codepages which are installed.

       Format of each entry is
         (CHARSET_NAME . (WINDOWS_CHARSET . CODEPAGE)).
    */
    for (rest = Vw32_charset_info_alist; CONSP (rest); rest = XCDR (rest))
      {
        char * x_charset;
        Lisp_Object w32_charset;
        Lisp_Object codepage;

        Lisp_Object this_entry = XCAR (rest);

        /* Skip invalid entries in alist. */
        if (!CONSP (this_entry) || !STRINGP (XCAR (this_entry))
            || !CONSP (XCDR (this_entry))
            || !SYMBOLP (XCAR (XCDR (this_entry))))
          continue;

        x_charset = XSTRING (XCAR (this_entry))->data;
        w32_charset = XCAR (XCDR (this_entry));
        codepage = XCDR (XCDR (this_entry));

        /* Look for Same charset and a valid codepage (or non-int
           which means ignore).  */
        if (w32_charset == charset_type
            && (!INTEGERP (codepage) || codepage == CP_DEFAULT
                || IsValidCodePage (XINT (codepage))))
          {
            /* If we don't have a match already, then this is the
               best.  */
            if (!best_match)
              best_match = x_charset;
            /* If this is an ISO codepage, and the best so far isn't,
               then this is better.  */
            else if (strnicmp (best_match, "iso", 3) != 0
                     && strnicmp (x_charset, "iso", 3) == 0)
              best_match = x_charset;
            /* If both are ISO8859 codepages, choose the one with the
               lowest number in the encoding field.  */
            else if (strnicmp (best_match, "iso8859-", 8) == 0
                     && strnicmp (x_charset, "iso8859-", 8) == 0)
              {
                int best_enc = atoi (best_match + 8);
                int this_enc = atoi (x_charset + 8);
                if (this_enc > 0 && this_enc < best_enc)
                  best_match = x_charset;
              }                
          }
      }

    /* If no match, encode the numeric value. */
    if (!best_match)
      {
        sprintf (buf, "*-#%u", fncharset);
        return buf;
      }

    strncpy(buf, best_match, 31);
    buf[31] = '\0';
    return buf;
  }
}


/* Return all the X charsets that map to a font.  */
static Lisp_Object
w32_to_all_x_charsets (fncharset)
    int fncharset;
{
  static char buf[32];
  Lisp_Object charset_type;
  Lisp_Object retval = Qnil;

  switch (fncharset)
    {
    case ANSI_CHARSET:
      /* Handle startup case of w32-charset-info-alist not
         being set up yet. */
      if (NILP(Vw32_charset_info_alist))
        return Fcons (build_string ("iso8859-1"), Qnil);

      charset_type = Qw32_charset_ansi;
      break;
    case DEFAULT_CHARSET:
      charset_type = Qw32_charset_default;
      break;
    case SYMBOL_CHARSET:
      charset_type = Qw32_charset_symbol;
      break;
    case SHIFTJIS_CHARSET:
      charset_type = Qw32_charset_shiftjis;
      break;
    case HANGEUL_CHARSET:
      charset_type = Qw32_charset_hangeul;
      break;
    case GB2312_CHARSET:
      charset_type = Qw32_charset_gb2312;
      break;
    case CHINESEBIG5_CHARSET:
      charset_type = Qw32_charset_chinesebig5;
      break;
    case OEM_CHARSET:
      charset_type = Qw32_charset_oem;
      break;

      /* More recent versions of Windows (95 and NT4.0) define more
         character sets.  */
#ifdef EASTEUROPE_CHARSET
    case EASTEUROPE_CHARSET:
      charset_type = Qw32_charset_easteurope;
      break;
    case TURKISH_CHARSET:
      charset_type = Qw32_charset_turkish;
      break;
    case BALTIC_CHARSET:
      charset_type = Qw32_charset_baltic;
      break;
    case RUSSIAN_CHARSET:
      charset_type = Qw32_charset_russian;
      break;
    case ARABIC_CHARSET:
      charset_type = Qw32_charset_arabic;
      break;
    case GREEK_CHARSET:
      charset_type = Qw32_charset_greek;
      break;
    case HEBREW_CHARSET:
      charset_type = Qw32_charset_hebrew;
      break;
    case VIETNAMESE_CHARSET:
      charset_type = Qw32_charset_vietnamese;
      break;
    case THAI_CHARSET:
      charset_type = Qw32_charset_thai;
      break;
    case MAC_CHARSET:
      charset_type = Qw32_charset_mac;
      break;
    case JOHAB_CHARSET:
      charset_type = Qw32_charset_johab;
      break;
#endif

#ifdef UNICODE_CHARSET
    case UNICODE_CHARSET:
      charset_type = Qw32_charset_unicode;
      break;
#endif
    default:
      /* Encode numerical value of unknown charset.  */
      sprintf (buf, "*-#%u", fncharset);
      return Fcons (build_string (buf), Qnil);
    }
  
  {
    Lisp_Object rest;
    /* Look through w32-charset-info-alist for the character set.
       Only return charsets for codepages which are installed.

       Format of each entry in Vw32_charset_info_alist is
         (CHARSET_NAME . (WINDOWS_CHARSET . CODEPAGE)).
    */
    for (rest = Vw32_charset_info_alist; CONSP (rest); rest = XCDR (rest))
      {
        Lisp_Object x_charset;
        Lisp_Object w32_charset;
        Lisp_Object codepage;

        Lisp_Object this_entry = XCAR (rest);

        /* Skip invalid entries in alist. */
        if (!CONSP (this_entry) || !STRINGP (XCAR (this_entry))
            || !CONSP (XCDR (this_entry))
            || !SYMBOLP (XCAR (XCDR (this_entry))))
          continue;

        x_charset = XCAR (this_entry);
        w32_charset = XCAR (XCDR (this_entry));
        codepage = XCDR (XCDR (this_entry));

        /* Look for Same charset and a valid codepage (or non-int
           which means ignore).  */
        if (w32_charset == charset_type
            && (!INTEGERP (codepage) || codepage == CP_DEFAULT
                || IsValidCodePage (XINT (codepage))))
          {
	    retval = Fcons (x_charset, retval);
          }
      }

    /* If no match, encode the numeric value. */
    if (NILP (retval))
      {
        sprintf (buf, "*-#%u", fncharset);
        return Fcons (build_string (buf), Qnil);
      }

    return retval;
  }
}

/* Get the Windows codepage corresponding to the specified font.  The
   charset info in the font name is used to look up
   w32-charset-to-codepage-alist.  */
int 
w32_codepage_for_font (char *fontname)
{
  Lisp_Object codepage, entry;
  char *charset_str, *charset, *end;

  if (NILP (Vw32_charset_info_alist))
    return CP_DEFAULT;

  /* Extract charset part of font string.  */
  charset = xlfd_charset_of_font (fontname);

  if (!charset)
    return CP_UNKNOWN;

  charset_str = (char *) alloca (strlen (charset) + 1);
  strcpy (charset_str, charset);

#if 0
  /* Remove leading "*-".  */
  if (strncmp ("*-", charset_str, 2) == 0)
    charset = charset_str + 2;
  else
#endif
    charset = charset_str;

  /* Stop match at wildcard (including preceding '-'). */
  if (end = strchr (charset, '*'))
      {
        if (end > charset && *(end-1) == '-')
          end--;
        *end = '\0';
      }

  entry = Fassoc (build_string(charset), Vw32_charset_info_alist);
  if (NILP (entry))
    return CP_UNKNOWN;

  codepage = Fcdr (Fcdr (entry));

  if (NILP (codepage))
    return CP_8BIT;
  else if (XFASTINT (codepage) == XFASTINT (Qt))
    return CP_UNICODE;
  else if (INTEGERP (codepage))
    return XINT (codepage);
  else
    return CP_UNKNOWN;
}


static BOOL 
w32_to_x_font (lplogfont, lpxstr, len, specific_charset)
     LOGFONT * lplogfont;
     char * lpxstr;
     int len;
     char * specific_charset;
{
  char* fonttype;
  char *fontname;
  char height_pixels[8];
  char height_dpi[8];
  char width_pixels[8];
  char *fontname_dash;
  int display_resy = (int) one_w32_display_info.resy;
  int display_resx = (int) one_w32_display_info.resx;
  int bufsz;
  struct coding_system coding;

  if (!lpxstr) abort ();

  if (!lplogfont)
    return FALSE;

  if (lplogfont->lfOutPrecision == OUT_STRING_PRECIS)
    fonttype = "raster";
  else if (lplogfont->lfOutPrecision == OUT_STROKE_PRECIS)
    fonttype = "outline";
  else
    fonttype = "unknown";

  setup_coding_system (Fcheck_coding_system (Vlocale_coding_system),
                       &coding);
  coding.src_multibyte = 0;
  coding.dst_multibyte = 1;
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
	     "-%s-%s-%s-%c-normal-normal-%s-%s-%d-%d-%c-%s-%s",
             fonttype,                               /* foundry */
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
	     specific_charset ? specific_charset
             : w32_to_x_charset (lplogfont->lfCharSet) 
             /* charset registry and encoding */
	     );

  lpxstr[len - 1] = 0;		/* just to be sure */
  return (TRUE);
}

static BOOL 
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
        width[10], resy[10], remainder[50];
      char * encoding;
      int dpi = (int) one_w32_display_info.resy;

      fields = sscanf (lpxstr,
		       "-%*[^-]-%49[^-]-%19[^-]-%c-%*[^-]-%*[^-]-%9[^-]-%9[^-]-%*[^-]-%9[^-]-%c-%9[^-]-%49s",
		       name, weight, &slant, pixels, height, resy, &pitch, width, remainder);
      if (fields == EOF)
	return (FALSE);

      /* In the general case when wildcards cover more than one field,
	 we don't know which field is which, so don't fill any in.
	 However, we need to cope with this particular form, which is
	 generated by font_list_1 (invoked by try_font_list):
	     "-raster-6x10-*-gb2312*-*"
	 and make sure to correctly parse the charset field.  */
      if (fields == 3)
	{
	  fields = sscanf (lpxstr,
			   "-%*[^-]-%49[^-]-*-%49s",
			   name, remainder);
	}
      else if (fields < 9)
	{
	  fields = 0;
	  remainder[0] = 0;
	}

      if (fields > 0 && name[0] != '*')
        {
	  int bufsize;
	  unsigned char *buf;

          setup_coding_system
            (Fcheck_coding_system (Vlocale_coding_system), &coding);
	  coding.src_multibyte = 1;
	  coding.dst_multibyte = 1;
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
	  lplogfont->lfFaceName[0] = '\0';
	}

      fields--;

      lplogfont->lfWeight = x_to_w32_weight ((fields > 0 ? weight : ""));

      fields--;

      lplogfont->lfItalic = (fields > 0 && slant == 'i');

      fields--;

      if (fields > 0 && pixels[0] != '*')
	lplogfont->lfHeight = atoi (pixels);

      fields--;
      fields--;
      if (fields > 0 && resy[0] != '*')
        {
          tem = atoi (resy);
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
         fails the test against xlfd-tight-regexp in fontset.el).  */
      {
	int len = strlen (remainder);
	if (len > 0 && remainder[len-1] == '-')
	  remainder[len-1] = 0;
      }
      encoding = remainder;
#if 0
      if (strncmp (encoding, "*-", 2) == 0)
	encoding += 2;
#endif
      lplogfont->lfCharSet = x_to_w32_charset (encoding);
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

/* Strip the pixel height and point height from the given xlfd, and
   return the pixel height. If no pixel height is specified, calculate
   one from the point height, or if that isn't defined either, return
   0 (which usually signifies a scalable font).
*/
static int
xlfd_strip_height (char *fontname)
{
  int pixel_height, field_number;
  char *read_from, *write_to;

  xassert (fontname);

  pixel_height = field_number = 0;
  write_to = NULL;

  /* Look for height fields.  */
  for (read_from = fontname; *read_from; read_from++)
    {
      if (*read_from == '-')
        {
          field_number++;
          if (field_number == 7) /* Pixel height.  */
            {
              read_from++;
              write_to = read_from;

              /* Find end of field.  */
              for (;*read_from && *read_from != '-'; read_from++)
                ;

              /* Split the fontname at end of field.  */
              if (*read_from)
                {
                  *read_from = '\0';
                  read_from++;
                }
              pixel_height = atoi (write_to);
              /* Blank out field. */
              if (read_from > write_to)
                {
                  *write_to = '-';
                  write_to++;
                }
              /* If the pixel height field is at the end (partial xlfd),
                 return now.  */
              else
                return pixel_height;

              /* If we got a pixel height, the point height can be
                 ignored. Just blank it out and break now.  */
              if (pixel_height)
                {
                  /* Find end of point size field.  */
                  for (; *read_from && *read_from != '-'; read_from++)
                    ;

                  if (*read_from)
                    read_from++;

                  /* Blank out the point size field.  */
                  if (read_from > write_to)
                    {
                      *write_to = '-';
                      write_to++;
                    }
                  else
                    return pixel_height;

                  break;
                }
              /* If the point height is already blank, break now.  */
              if (*read_from == '-')
                {
                  read_from++;
                  break;
                }
            }
          else if (field_number == 8)
            {
              /* If we didn't get a pixel height, try to get the point
                 height and convert that.  */
              int point_size;
              char *point_size_start = read_from++;

              /* Find end of field.  */
              for (; *read_from && *read_from != '-'; read_from++)
                ;

              if (*read_from)
                {
                  *read_from = '\0';
                  read_from++;
                }

              point_size = atoi (point_size_start);

              /* Convert to pixel height. */
              pixel_height = point_size
                           * one_w32_display_info.height_in / 720;

              /* Blank out this field and break.  */
              *write_to = '-';
              write_to++;
              break;
            }
        }
    }

  /* Shift the rest of the font spec into place.  */
  if (write_to && read_from > write_to)
    {
      for (; *read_from; read_from++, write_to++)
        *write_to = *read_from;
      *write_to = '\0';
    }

  return pixel_height;
}

/* Assume parameter 1 is fully qualified, no wildcards. */
static BOOL 
w32_font_match (fontname, pattern)
    char * fontname;
    char * pattern;
{
  char *regex = alloca (strlen (pattern) * 2 + 3);
  char *font_name_copy = alloca (strlen (fontname) + 1);
  char *ptr;

  /* Copy fontname so we can modify it during comparison.  */
  strcpy (font_name_copy, fontname);

  ptr = regex;
  *ptr++ = '^';

  /* Turn pattern into a regexp and do a regexp match.  */
  for (; *pattern; pattern++)
    {
      if (*pattern == '?')
        *ptr++ = '.';
      else if (*pattern == '*')
        {
          *ptr++ = '.';
          *ptr++ = '*';
        }
      else
        *ptr++ = *pattern;
    }
  *ptr = '$';
  *(ptr + 1) = '\0';

  /* Strip out font heights and compare them seperately, since
     rounding error can cause mismatches. This also allows a
     comparison between a font that declares only a pixel height and a
     pattern that declares the point height.
  */
  {
    int font_height, pattern_height;

    font_height = xlfd_strip_height (font_name_copy);
    pattern_height = xlfd_strip_height (regex);

    /* Compare now, and don't bother doing expensive regexp matching
       if the heights differ.  */
    if (font_height && pattern_height && (font_height != pattern_height))
      return FALSE;
  }

  return (fast_c_string_match_ignore_case (build_string (regex),
                                           font_name_copy) >= 0);
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
  Lisp_Object list;
  Lisp_Object *tail;
} enumfont_t;


static void
enum_font_maybe_add_to_list (enumfont_t *, LOGFONT *, char *, Lisp_Object);


static int CALLBACK 
enum_font_cb2 (lplf, lptm, FontType, lpef)
    ENUMLOGFONT * lplf;
    NEWTEXTMETRIC * lptm;
    int FontType;
    enumfont_t * lpef;
{
  /* Ignore struck out and underlined versions of fonts.  */
  if (lplf->elfLogFont.lfStrikeOut || lplf->elfLogFont.lfUnderline)
    return 1;

  /* Only return fonts with names starting with @ if they were
     explicitly specified, since Microsoft uses an initial @ to
     denote fonts for vertical writing, without providing a more
     convenient way of identifying them.  */
  if (lplf->elfLogFont.lfFaceName[0] == '@'
      && lpef->logfont.lfFaceName[0] != '@')
    return 1;

  /* Check that the character set matches if it was specified */
  if (lpef->logfont.lfCharSet != DEFAULT_CHARSET &&
      lplf->elfLogFont.lfCharSet != lpef->logfont.lfCharSet)
    return 1;

  {
    char buf[100];
    Lisp_Object width = Qnil;
    Lisp_Object charset_list = Qnil;
    char *charset = NULL;

    /* Truetype fonts do not report their true metrics until loaded */
    if (FontType != RASTER_FONTTYPE)
      {
	if (!NILP (*(lpef->pattern)))
	  {
	    /* Scalable fonts are as big as you want them to be.  */
	    lplf->elfLogFont.lfHeight = lpef->logfont.lfHeight;
	    lplf->elfLogFont.lfWidth = lpef->logfont.lfWidth;
	    width = make_number (lpef->logfont.lfWidth);
	  }
	else
	  {
	    lplf->elfLogFont.lfHeight = 0;
	    lplf->elfLogFont.lfWidth = 0;
	  }
      }

    /* Make sure the height used here is the same as everywhere
       else (ie character height, not cell height).  */
    if (lplf->elfLogFont.lfHeight > 0)
      {
        /* lptm can be trusted for RASTER fonts, but not scalable ones. */
        if (FontType == RASTER_FONTTYPE)
          lplf->elfLogFont.lfHeight = lptm->tmInternalLeading - lptm->tmHeight;
        else
          lplf->elfLogFont.lfHeight = -lplf->elfLogFont.lfHeight;
      }

    if (!NILP (*(lpef->pattern)))
      {
        charset = xlfd_charset_of_font (XSTRING(*(lpef->pattern))->data);

	/* We already checked charsets above, but DEFAULT_CHARSET
           slipped through.  So only allow exact matches for DEFAULT_CHARSET.  */
	if (charset
	    && strncmp (charset, "*-*", 3) != 0
	    && lpef->logfont.lfCharSet == DEFAULT_CHARSET
	    && strcmp (charset, w32_to_x_charset (DEFAULT_CHARSET)) != 0)
	  return 1;
      }

    if (charset)
      charset_list = Fcons (build_string (charset), Qnil);
    else
      charset_list = w32_to_all_x_charsets (lplf->elfLogFont.lfCharSet);

    /* Loop through the charsets.  */
    for ( ; CONSP (charset_list); charset_list = Fcdr (charset_list))
      {
	Lisp_Object this_charset = Fcar (charset_list);
	charset = XSTRING (this_charset)->data;

	/* List bold and italic variations if w32-enable-synthesized-fonts
	   is non-nil and this is a plain font.  */
	if (w32_enable_synthesized_fonts
	    && lplf->elfLogFont.lfWeight == FW_NORMAL
	    && lplf->elfLogFont.lfItalic == FALSE)
	  {
	    enum_font_maybe_add_to_list (lpef, &(lplf->elfLogFont),
					 charset, width);
	    /* bold.  */
	    lplf->elfLogFont.lfWeight = FW_BOLD;
	    enum_font_maybe_add_to_list (lpef, &(lplf->elfLogFont),
					 charset, width);
	    /* bold italic.  */
	    lplf->elfLogFont.lfItalic = TRUE;
	    enum_font_maybe_add_to_list (lpef, &(lplf->elfLogFont),
					 charset, width);
	    /* italic.  */
	    lplf->elfLogFont.lfWeight = FW_NORMAL;
	    enum_font_maybe_add_to_list (lpef, &(lplf->elfLogFont),
					 charset, width);
	  }
	else
	  enum_font_maybe_add_to_list (lpef, &(lplf->elfLogFont),
				       charset, width);
      }
  }

  return 1;
}

static void
enum_font_maybe_add_to_list (lpef, logfont, match_charset, width)
     enumfont_t * lpef;
     LOGFONT * logfont;
     char * match_charset;
     Lisp_Object width;
{
  char buf[100];

  if (!w32_to_x_font (logfont, buf, 100, match_charset))
    return;

  if (NILP (*(lpef->pattern))
      || w32_font_match (buf, XSTRING (*(lpef->pattern))->data))
    {
      /* Check if we already listed this font.  This may happen if
         w32_enable_synthesized_fonts is non-nil, and there are real
         bold and italic versions of the font.  */
      Lisp_Object font_name = build_string (buf);
      if (NILP (Fmember (font_name, lpef->list)))
	{
	  *lpef->tail = Fcons (Fcons (build_string (buf), width), Qnil);
	  lpef->tail = &(XCDR (*lpef->tail));
	  lpef->numFonts++;
	}
    }
}


static int CALLBACK 
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


static int CALLBACK
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

static int CALLBACK
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

static Lisp_Object w32_list_bdf_fonts (Lisp_Object pattern, int max_names)
{
  char *fontname, *ptnstr;
  Lisp_Object list, tem, newlist = Qnil;
  int n_fonts = 0;

  list = Vw32_bdf_filename_alist;
  ptnstr = XSTRING (pattern)->data;

  for ( ; CONSP (list); list = XCDR (list))
    {
      tem = XCAR (list);
      if (CONSP (tem))
        fontname = XSTRING (XCAR (tem))->data;
      else if (STRINGP (tem))
        fontname = XSTRING (tem)->data;
      else
        continue;

      if (w32_font_match (fontname, ptnstr))
        {
          newlist = Fcons (XCAR (tem), newlist);
          n_fonts++;
          if (n_fonts >= max_names)
            break;
        }
    }

  return newlist;
}


/* Return a list of names of available fonts matching PATTERN on frame
   F.  If SIZE is not 0, it is the size (maximum bound width) of fonts
   to be listed.  Frame F NULL means we have not yet created any
   frame, which means we can't get proper size info, as we don't have
   a device context to use for GetTextMetrics.
   MAXNAMES sets a limit on how many fonts to match.  */

Lisp_Object
w32_list_fonts (f, pattern, size, maxnames)
     struct frame *f;
     Lisp_Object pattern;
     int size;
     int maxnames;
{
  Lisp_Object patterns, key = Qnil, tem, tpat;
  Lisp_Object list = Qnil, newlist = Qnil, second_best = Qnil;
  struct w32_display_info *dpyinfo = &one_w32_display_info;
  int n_fonts = 0;

  patterns = Fassoc (pattern, Valternate_fontname_alist);
  if (NILP (patterns))
    patterns = Fcons (pattern, Qnil);

  for (; CONSP (patterns); patterns = XCDR (patterns))
    {
      enumfont_t ef;
      int codepage;

      tpat = XCAR (patterns);

      if (!STRINGP (tpat))
        continue;

      /* Avoid expensive EnumFontFamilies functions if we are not
         going to be able to output one of these anyway. */
      codepage = w32_codepage_for_font (XSTRING (tpat)->data);
      if (codepage != CP_8BIT && codepage != CP_UNICODE
          && codepage != CP_DEFAULT && codepage != CP_UNKNOWN
	  && !IsValidCodePage(codepage))
        continue;

      /* See if we cached the result for this particular query.
         The cache is an alist of the form:
           ((PATTERN (FONTNAME . WIDTH) ...) ...)
      */
      if (tem = XCDR (dpyinfo->name_list_element),
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
      ef.list = list;
      ef.tail = &list;
      ef.numFonts = 0;

      /* Use EnumFontFamiliesEx where it is available, as it knows
         about character sets.  Fall back to EnumFontFamilies for
         older versions of NT that don't support the 'Ex function.  */
      x_to_w32_font (XSTRING (tpat)->data, &ef.logfont);
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
      XSETCDR (dpyinfo->name_list_element,
	       Fcons (Fcons (tpat, list),
		      XCDR (dpyinfo->name_list_element)));

    label_cached:
      if (NILP (list)) continue; /* Try the remaining alternatives.  */

      newlist = second_best = Qnil;

      /* Make a list of the fonts that have the right width.  */          
      for (; CONSP (list); list = XCDR (list))
        {
          int found_size;
          tem = XCAR (list);

          if (!CONSP (tem))
            continue;
          if (NILP (XCAR (tem)))
            continue;
          if (!size)
            {
              newlist = Fcons (XCAR (tem), newlist);
              n_fonts++;
              if (n_fonts >= maxnames)
                break;
              else
                continue;
            }
          if (!INTEGERP (XCDR (tem)))
            {
              /* Since we don't yet know the size of the font, we must
                 load it and try GetTextMetrics.  */
              W32FontStruct thisinfo;
              LOGFONT lf;
              HDC hdc;
              HANDLE oldobj;

              if (!x_to_w32_font (XSTRING (XCAR (tem))->data, &lf))
                continue;

              BLOCK_INPUT;
              thisinfo.bdf = NULL;
              thisinfo.hfont = CreateFontIndirect (&lf);
              if (thisinfo.hfont == NULL)
                continue;

              hdc = GetDC (dpyinfo->root_window);
              oldobj = SelectObject (hdc, thisinfo.hfont);
              if (GetTextMetrics (hdc, &thisinfo.tm))
                XSETCDR (tem, make_number (FONT_WIDTH (&thisinfo)));
              else
                XSETCDR (tem, make_number (0));
              SelectObject (hdc, oldobj);
              ReleaseDC (dpyinfo->root_window, hdc);
              DeleteObject(thisinfo.hfont);
              UNBLOCK_INPUT;
            }
          found_size = XINT (XCDR (tem));
          if (found_size == size)
            {
              newlist = Fcons (XCAR (tem), newlist);
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
                  if (XINT (XCDR (second_best)) > size
                      || XINT (XCDR (second_best)) < found_size)
                    second_best = tem;
                }
              else
                {
                  if (XINT (XCDR (second_best)) > size
                      && XINT (XCDR (second_best)) >
                      found_size)
                    second_best = tem;
                }
            }
        }

      if (!NILP (newlist))
        break;
      else if (!NILP (second_best))
        {
          newlist = Fcons (XCAR (second_best), Qnil);
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

  return newlist;
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

  for (list = Vfont_ccl_encoder_alist; CONSP (list); list = XCDR (list))
    {
      elt = XCAR (list);
      if (CONSP (elt)
	  && STRINGP (XCAR (elt))
	  && (fast_c_string_match_ignore_case (XCAR (elt), fontp->name)
	      >= 0))
	break;
    }
  if (! NILP (list))
    {
      struct ccl_program *ccl
	= (struct ccl_program *) xmalloc (sizeof (struct ccl_program));

      if (setup_ccl_program (ccl, XCDR (elt)) < 0)
	xfree (ccl);
      else
	fontp->font_encoder = ccl;
    }
}


/* Find BDF files in a specified directory.  (use GCPRO when calling,
   as this calls lisp to get a directory listing).  */
static Lisp_Object
w32_find_bdf_fonts_in_dir (Lisp_Object directory)
{
  Lisp_Object filelist, list = Qnil;
  char fontname[100];

  if (!STRINGP(directory))
    return Qnil;

  filelist = Fdirectory_files (directory, Qt,
                              build_string (".*\\.[bB][dD][fF]"), Qt);

  for ( ; CONSP(filelist); filelist = XCDR (filelist))
    {
      Lisp_Object filename = XCAR (filelist);
      if (w32_BDF_to_x_font (XSTRING (filename)->data, fontname, 100))
          store_in_alist (&list, build_string (fontname), filename);
    }
  return list;
}

DEFUN ("w32-find-bdf-fonts", Fw32_find_bdf_fonts, Sw32_find_bdf_fonts,
       1, 1, 0,
       doc: /* Return a list of BDF fonts in DIR.
The list is suitable for appending to w32-bdf-filename-alist.  Fonts
which do not contain an xlfd description will not be included in the
list. DIR may be a list of directories.  */)
     (directory)
     Lisp_Object directory;
{
  Lisp_Object list = Qnil;
  struct gcpro gcpro1, gcpro2;

  if (!CONSP (directory))
    return w32_find_bdf_fonts_in_dir (directory);

  for ( ; CONSP (directory); directory = XCDR (directory))
    {
      Lisp_Object pair[2];
      pair[0] = list;
      pair[1] = Qnil;
      GCPRO2 (directory, list);
      pair[1] = w32_find_bdf_fonts_in_dir( XCAR (directory) );
      list = Fnconc( 2, pair );
      UNGCPRO;
    }
  return list;
}


DEFUN ("xw-color-defined-p", Fxw_color_defined_p, Sxw_color_defined_p, 1, 2, 0,
       doc: /* Internal function called by `color-defined-p', which see.  */)
  (color, frame)
     Lisp_Object color, frame;
{
  XColor foo;
  FRAME_PTR f = check_x_frame (frame);

  CHECK_STRING (color);

  if (w32_defined_color (f, XSTRING (color)->data, &foo, 0))
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

  if (w32_defined_color (f, XSTRING (color)->data, &foo, 0))
    {
      Lisp_Object rgb[3];

      rgb[0] = make_number ((GetRValue (foo.pixel) << 8)
                            | GetRValue (foo.pixel));
      rgb[1] = make_number ((GetGValue (foo.pixel) << 8)
                            | GetGValue (foo.pixel));
      rgb[2] = make_number ((GetBValue (foo.pixel) << 8)
                            | GetBValue (foo.pixel));
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
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  if ((dpyinfo->n_planes * dpyinfo->n_cbits) <= 2)
    return Qnil;

  return Qt;
}

DEFUN ("x-display-grayscale-p", Fx_display_grayscale_p,
       Sx_display_grayscale_p, 0, 1, 0,
       doc: /* Return t if the X display supports shades of gray.
Note that color displays do support shades of gray.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  if ((dpyinfo->n_planes * dpyinfo->n_cbits) <= 1)
    return Qnil;

  return Qt;
}

DEFUN ("x-display-pixel-width", Fx_display_pixel_width,
       Sx_display_pixel_width, 0, 1, 0,
       doc: /* Returns the width in pixels of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->width);
}

DEFUN ("x-display-pixel-height", Fx_display_pixel_height,
       Sx_display_pixel_height, 0, 1, 0,
       doc: /* Returns the height in pixels of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->height);
}

DEFUN ("x-display-planes", Fx_display_planes, Sx_display_planes,
       0, 1, 0,
       doc: /* Returns the number of bitplanes of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (dpyinfo->n_planes * dpyinfo->n_cbits);
}

DEFUN ("x-display-color-cells", Fx_display_color_cells, Sx_display_color_cells,
       0, 1, 0,
       doc: /* Returns the number of color cells of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
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

  /* We force 24+ bit depths to 24-bit, both to prevent an overflow
     and because probably is more meaningful on Windows anyway */
  if (cap < 0)
    cap = 1 << min(dpyinfo->n_planes * dpyinfo->n_cbits, 24);
  
  ReleaseDC (dpyinfo->root_window, hdc);
  
  return make_number (cap);
}

DEFUN ("x-server-max-request-size", Fx_server_max_request_size,
       Sx_server_max_request_size,
       0, 1, 0,
       doc: /* Returns the maximum request size of the server of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);

  return make_number (1);
}

DEFUN ("x-server-vendor", Fx_server_vendor, Sx_server_vendor, 0, 1, 0,
       doc: /* Returns the vendor ID string of the W32 system (Microsoft).
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  return build_string ("Microsoft Corp.");
}

DEFUN ("x-server-version", Fx_server_version, Sx_server_version, 0, 1, 0,
       doc: /* Returns the version numbers of the server of DISPLAY.
The value is a list of three integers: the major and minor
version numbers, and the vendor-specific release
number.  See also the function `x-server-vendor'.

The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  return Fcons (make_number (w32_major_version),
		Fcons (make_number (w32_minor_version),
		       Fcons (make_number (w32_build_number), Qnil)));
}

DEFUN ("x-display-screens", Fx_display_screens, Sx_display_screens, 0, 1, 0,
       doc: /* Returns the number of screens on the server of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  return make_number (1);
}

DEFUN ("x-display-mm-height", Fx_display_mm_height,
       Sx_display_mm_height, 0, 1, 0,
       doc: /* Returns the height in millimeters of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
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
       doc: /* Returns the width in millimeters of DISPLAY.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
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
       doc: /* Returns an indication of whether DISPLAY does backing store.
The value may be `always', `when-mapped', or `not-useful'.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
  return intern ("not-useful");
}

DEFUN ("x-display-visual-class", Fx_display_visual_class,
       Sx_display_visual_class, 0, 1, 0,
       doc: /* Returns the visual class of DISPLAY.
The value is one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.

The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
	(display)
     Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
  Lisp_Object result = Qnil;

  if (dpyinfo->has_palette)
      result = intern ("pseudo-color");
  else if (dpyinfo->n_planes * dpyinfo->n_cbits == 1)
      result = intern ("static-grey");
  else if (dpyinfo->n_planes * dpyinfo->n_cbits == 4)
      result = intern ("static-color");
  else if (dpyinfo->n_planes * dpyinfo->n_cbits > 8)
      result = intern ("true-color");

  return result;
}

DEFUN ("x-display-save-under", Fx_display_save_under,
       Sx_display_save_under, 0, 1, 0,
       doc: /* Returns t if DISPLAY supports the save-under feature.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display.  */)
  (display)
     Lisp_Object display;
{
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
x_screen_planes (f)
     register struct frame *f;
{
  return FRAME_W32_DISPLAY_INFO (f)->n_planes;
}

/* Return the display structure for the display named NAME.
   Open a new connection if necessary.  */

struct w32_display_info *
x_display_info_for_name (name)
     Lisp_Object name;
{
  Lisp_Object names;
  struct w32_display_info *dpyinfo;

  CHECK_STRING (name);

  for (dpyinfo = &one_w32_display_info, names = w32_display_name_list;
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

  dpyinfo = w32_term_init (name, (unsigned char *)0,
			     (char *) XSTRING (Vx_resource_name)->data);

  if (dpyinfo == 0)
    error ("Cannot connect to server %s", XSTRING (name)->data);

  w32_in_use = 1;
  XSETFASTINT (Vwindow_system_version, 3);

  return dpyinfo;
}

DEFUN ("x-open-connection", Fx_open_connection, Sx_open_connection,
       1, 3, 0, doc: /* Open a connection to a server.
DISPLAY is the name of the display to connect to.
Optional second arg XRM-STRING is a string of resources in xrdb format.
If the optional third arg MUST-SUCCEED is non-nil,
terminate Emacs if we can't open the connection.  */)
  (display, xrm_string, must_succeed)
     Lisp_Object display, xrm_string, must_succeed;
{
  unsigned char *xrm_option;
  struct w32_display_info *dpyinfo;

  /* If initialization has already been done, return now to avoid
     overwriting critical parts of one_w32_display_info.  */
  if (w32_in_use)
    return Qnil;

  CHECK_STRING (display);
  if (! NILP (xrm_string))
    CHECK_STRING (xrm_string);

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
       doc: /* Close the connection to DISPLAY's server.
For DISPLAY, specify either a frame or a display name (a string).
If DISPLAY is nil, that stands for the selected frame's display.  */)
  (display)
  Lisp_Object display;
{
  struct w32_display_info *dpyinfo = check_x_display_info (display);
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
        w32_unload_font (dpyinfo, dpyinfo->font_table[i].font);
      }
  x_destroy_all_bitmaps (dpyinfo);

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
  for (tail = w32_display_name_list; ! NILP (tail); tail = XCDR (tail))
    result = Fcons (XCAR (XCAR (tail)), result);

  return result;
}

DEFUN ("x-synchronize", Fx_synchronize, Sx_synchronize, 1, 2, 0,
       doc: /* This is a noop on W32 systems.  */)
     (on, display)
     Lisp_Object display, on;
{
  return Qnil;
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
extern Lisp_Object QCdata, QCtype;
Lisp_Object QCascent, QCmargin, QCrelief;
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
   message display would lead to another redisplay. This function
   therefore simply displays a message. */

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
  if (img->pixmap == 0 && !img->load_failed_p)
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
	ascent = height / 2 - (FONT_DESCENT(face->font)
                               - FONT_BASE(face->font)) / 2;
      else
	ascent = height / 2;
    }
  else
    ascent = (int) (height * img->ascent / 100.0);

  return ascent;
}



/* Image background colors.  */

/* Find the "best" corner color of a bitmap.  XIMG is assumed to a device
   context with the bitmap selected.  */
static COLORREF
four_corners_best (ximg, width, height)
     HDC ximg;
     unsigned long width, height;
{
  COLORREF corners[4], best;
  int i, best_count;

  /* Get the colors at the corners of ximg.  */
  corners[0] = GetPixel (ximg, 0, 0);
  corners[1] = GetPixel (ximg, width - 1, 0);
  corners[2] = GetPixel (ximg, width - 1, height - 1);
  corners[3] = GetPixel (ximg, 0, height - 1);

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
#if 0 /* TODO: Image support.  */
      int free_ximg = !ximg;

      if (! ximg)
	ximg = XGetImage (FRAME_X_DISPLAY (f), img->pixmap,
			  0, 0, img->width, img->height, ~0, ZPixmap);

      img->background = four_corners_best (ximg, img->width, img->height);

      if (free_ximg)
	XDestroyImage (ximg);

      img->background_valid = 1;
#endif
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
#if 0 /* TODO: Image support.  */
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
#endif
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
      DeleteObject (img->pixmap);
      img->pixmap = NULL;
      img->background_valid = 0;
    }

  if (mask_p && img->mask)
    {
      DeleteObject (img->mask);
      img->mask = NULL;
      img->background_transparent_valid = 0;
    }
      
  if (colors_p && img->ncolors)
    {
#if 0  /* TODO: color table support.  */
      x_free_colors (f, img->colors, img->ncolors);
#endif
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
  if (img->pixmap)
    {
      BLOCK_INPUT;
      DeleteObject (img->pixmap);
      img->pixmap = 0;
      UNBLOCK_INPUT;
    }

  if (img->ncolors)
    {
#if 0 /* TODO: color table support  */

      int class = FRAME_W32_DISPLAY_INFO (f)->visual->class;
      
      /* If display has an immutable color map, freeing colors is not
	 necessary and some servers don't allow it.  So don't do it.  */
      if (class != StaticColor
	  && class != StaticGray
	  && class != TrueColor)
	{
	  Colormap cmap;
	  BLOCK_INPUT;
	  cmap = DefaultColormapOfScreen (FRAME_W32_DISPLAY_INFO (f)->screen);
	  XFreeColors (FRAME_W32_DISPLAY (f), cmap, img->colors,
		       img->ncolors, 0);
	  UNBLOCK_INPUT;
	}
#endif
      
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

  if (w32_defined_color (f, XSTRING (color_name)->data, &color, 1))
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
	      && (force_p || (img->timestamp < old)))
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
	      if (FRAME_W32_P (f)
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
	if (FRAME_W32_P (XFRAME (frame)))
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
#if 0  /* TODO: image support.  */
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
	      DeleteObject (img->mask);
	      img->mask = NULL;
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
#endif
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
	     `:ascent PERCENT', `:margin MARGIN', `:relief RELIEF',
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
  if (FRAME_LIVE_P (f) && FRAME_W32_P (f))
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
			    W32 support code
 ***********************************************************************/

static int x_create_x_image_and_pixmap P_ ((struct frame *, int, int, int,
                                            XImage **, Pixmap *));
static void x_destroy_x_image P_ ((XImage *));
static void x_put_x_image P_ ((struct frame *, XImage *, Pixmap, int, int));


/* Create an XImage and a pixmap of size WIDTH x HEIGHT for use on
   frame F.  Set *XIMG and *PIXMAP to the XImage and Pixmap created.
   Set (*XIMG)->data to a raster of WIDTH x HEIGHT pixels allocated
   via xmalloc.  DEPTH of zero signifies a 24 bit image, otherwise
   DEPTH should indicate the bit depth of the image.  Print error
   messages via image_error if an error occurs.  Value is non-zero if
   successful.  */

static int
x_create_x_image_and_pixmap (f, width, height, depth, ximg, pixmap)
     struct frame *f;
     int width, height, depth;
     XImage **ximg;
     Pixmap *pixmap;
{
  BITMAPINFOHEADER *header;
  HDC hdc;
  int scanline_width_bits;
  int remainder;
  int palette_colors = 0;

  if (depth == 0)
    depth = 24;

  if (depth != 1 && depth != 4 && depth != 8
      && depth != 16 && depth != 24 && depth != 32)
    {
      image_error ("Invalid image bit depth specified", Qnil, Qnil);
      return 0;
    }

  scanline_width_bits = width * depth;
  remainder = scanline_width_bits % 32;

  if (remainder)
    scanline_width_bits += 32 - remainder;

  /* Bitmaps with a depth less than 16 need a palette.  */
  /* BITMAPINFO structure already contains the first RGBQUAD.  */
  if (depth < 16)
    palette_colors = 1 << depth - 1;

  *ximg = xmalloc (sizeof (XImage) + palette_colors * sizeof (RGBQUAD));
  if (*ximg == NULL)
    {
      image_error ("Unable to allocate memory for XImage", Qnil, Qnil);
      return 0;
    }

  header = &((*ximg)->info.bmiHeader);
  bzero (&((*ximg)->info), sizeof (BITMAPINFO));
  header->biSize = sizeof (*header);
  header->biWidth = width;
  header->biHeight = -height;  /* negative indicates a top-down bitmap.  */
  header->biPlanes = 1;
  header->biBitCount = depth;
  header->biCompression = BI_RGB;
  header->biClrUsed = palette_colors;

  hdc = get_frame_dc (f);

  /* Create a DIBSection and raster array for the bitmap,
     and store its handle in *pixmap.  */
  *pixmap = CreateDIBSection (hdc, &((*ximg)->info), DIB_RGB_COLORS,
			      &((*ximg)->data), NULL, 0);

  /* Realize display palette and garbage all frames. */
  release_frame_dc (f, hdc);

  if (*pixmap == NULL)
    {
      DWORD err = GetLastError();
      Lisp_Object errcode;
      /* All system errors are < 10000, so the following is safe.  */
      XSETINT (errcode, (int) err);
      image_error ("Unable to create bitmap, error code %d", errcode, Qnil);
      x_destroy_x_image (*ximg);
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
      /* Data will be freed by DestroyObject.  */
      ximg->data = NULL;
      xfree (ximg);
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

#if TODO  /* W32 specific image code.  */
  GC gc;

  xassert (interrupt_input_blocked);
  gc = XCreateGC (NULL, pixmap, 0, NULL);
  XPutImage (NULL, pixmap, gc, ximg, 0, 0, 0, 0, width, height);
  XFreeGC (NULL, gc);
#endif
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
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
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
  while (*s < end &&(c = *(*s)++, isspace (c)))
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


/* XBM bits seem to be backward within bytes compared with how
   Windows does things.  */
static unsigned char reflect_byte (unsigned char orig)
{
  int i;
  unsigned char reflected = 0x00;
  for (i = 0; i < 8; i++)
    {
      if (orig & (0x01 << i))
	reflected |= 0x80 >> i;
    }
  return reflected;
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
  int bytes_in_per_line, bytes_out_per_line, i, nbytes;
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
      /* On Windows, all images need padding to 16 bit boundaries.  */
      if (*width % 16 && *width % 16 < 9)
	padding_p = 1;

      if (strcmp (buffer, "unsigned") == 0)
	{
	  match (); 
	  expect_ident ("char");
	}
      else if (strcmp (buffer, "short") == 0)
	{
	  match ();
	  v10 = 1;
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

  /* Bytes per line on input.  Only count padding for v10 XBMs.  */
  bytes_in_per_line = (*width + 7) / 8 + (v10 ? padding_p : 0);
  bytes_out_per_line = (*width + 7) / 8 + padding_p;

  nbytes = bytes_in_per_line * *height;
  p = *data = (char *) xmalloc (bytes_out_per_line * *height);

  if (v10)
    {
      for (i = 0; i < nbytes; i += 2)
	{
	  int val = value;
	  expect (XBM_TK_NUMBER);

	  *p++ = reflect_byte (val);
	  if (!padding_p || ((i + 2) % bytes_in_per_line))
	    *p++ = reflect_byte (value >> 8);
	  
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
	  
	  *p++ = reflect_byte (val);
	  if (padding_p && ((i + 1) % bytes_in_per_line) == 0)
	    *p++ = 0;

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
   CONTENTS.  END is the end of the buffer. Value is non-zero if
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
	= CreateBitmap (img->width, img->height, 1, 1, data);

      xfree (data);

      if (img->pixmap == 0)
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
#ifdef TODO /* full image support.  */
	  /* Create the pixmap.  */
	  depth = one_w32_display_info.n_cbits;
	  img->pixmap
	    = XCreatePixmapFromBitmapData (FRAME_X_DISPLAY (f),
					   FRAME_X_WINDOW (f),
					   bits,
					   img->width, img->height,
					   foreground, background,
					   depth);
#endif
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
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
  {":margin",		IMAGE_POSITIVE_INTEGER_VALUE_OR_PAIR,	0},
  {":relief",		IMAGE_INTEGER_VALUE,			0},
  {":conversion",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":heuristic-mask",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":mask",             IMAGE_DONT_CHECK_VALUE_TYPE,            0},
  {":color-symbols",	IMAGE_DONT_CHECK_VALUE_TYPE,		0},
  {":background",       IMAGE_STRING_OR_NIL_VALUE,              0}
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
  attrs.visual = FRAME_X_VISUAL (f);
  attrs.colormap = FRAME_X_COLORMAP (f);
  attrs.valuemask |= XpmVisual;
  attrs.valuemask |= XpmColormap;
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
	  image_error ("Cannot find image file `%s'", specified_file, Qnil);
          UNBLOCK_INPUT;
	  return 0;
	}
      
      rc = XpmReadFileToPixmap (NULL, FRAME_W32_WINDOW (f),
				XSTRING (file)->data, &img->pixmap, &img->mask,
				&attrs);
    }
  else
    {
      Lisp_Object buffer = image_spec_value (img->spec, QCdata, NULL);
      rc = XpmCreatePixmapFromBuffer (NULL, FRAME_W32_WINDOW (f),
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


#if 0 /* TODO : Color tables on W32.  */
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
      COLORREF color;
      Colormap cmap;
      int rc;

      color = PALETTERGB (r, g, b);

      ++ct_colors_allocated;

      p = (struct ct_color *) xmalloc (sizeof *p);
      p->r = r;
      p->g = g;
      p->b = b;
      p->pixel = color;
      p->next = ct_table[i];
      ct_table[i] = p;
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
      XQueryColor (NULL, cmap, &color);
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

#endif /* TODO */


/***********************************************************************
			      Algorithms
 ***********************************************************************/
static XColor *x_to_xcolors P_ ((struct frame *, struct image *, int));
static void x_from_xcolors P_ ((struct frame *, struct image *, XColor *));
static void x_detect_edges P_ ((struct frame *, struct image *, int[9], int));
static void XPutPixel (XImage *, int, int, COLORREF);

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
#if 0 /* TODO: implement image colors.  */
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
#endif
  return colors;
}

/* Put a pixel of COLOR at position X, Y in XIMG.  XIMG must have been
   created with CreateDIBSection, with the pointer to the bit values
   stored in ximg->data.  */

static void XPutPixel (ximg, x, y, color)
     XImage * ximg;
     int x, y;
     COLORREF color;
{
  int width = ximg->info.bmiHeader.biWidth;
  int height = ximg->info.bmiHeader.biHeight;
  int rowbytes = width * 3;
  unsigned char * pixel;

  /* Don't support putting pixels in images with palettes.  */
  xassert (ximg->info.bmiHeader.biBitCount == 24);

  /* Ensure scanlines are aligned on 4 byte boundaries.  */
  if (rowbytes % 4)
    rowbytes += 4 - (rowbytes % 4);

  pixel = ximg->data + y * rowbytes + x * 3;
  *pixel = 255 - GetRValue (color);
  *(pixel + 1) = 255 - GetGValue (color);
  *(pixel + 2) = 255 - GetBValue (color);
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
#if 0   /* TODO: color tables.  */
  init_color_table ();
#endif
  x_create_x_image_and_pixmap (f, img->width, img->height, 0,
			       &oimg, &pixmap);
  p = colors;
  for (y = 0; y < img->height; ++y)
    for (x = 0; x < img->width; ++x, ++p)
      {
	unsigned long pixel;
#if 0  /* TODO: color tables.  */
	pixel = lookup_rgb_color (f, p->red, p->green, p->blue);
#else
	pixel = PALETTERGB (p->red, p->green, p->blue);
#endif
	XPutPixel (oimg, x, y, pixel);
      }

  xfree (colors);
  x_clear_image_1 (f, img, 1, 0, 1);

  x_put_x_image (f, oimg, pixmap, img->width, img->height);
  x_destroy_x_image (oimg);
  img->pixmap = pixmap;
#if 0  /* TODO: color tables.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif
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


/* Transform image IMG which is used on frame F with a Laplace
   edge-detection algorithm.  The result is an image that can be used
   to draw disabled buttons, for example.  */

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
  struct w32_display_info *dpyinfo = FRAME_X_DISPLAY_INFO (f);

  if (dpyinfo->n_planes * dpyinfo->n_cbits >= 2)
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
  if (dpyinfo->n_planes * dpyinfo->n_cbits < 2 || cross_disabled_images)
    {
#if 0 /* TODO: full image support  */
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
#endif
    }
}


/* Build a mask for image IMG which is used on frame F. FILE is the
   name of an image file, for error messages. HOW determines how to
   determine the background color of IMG. If it is a list '(R G B)',
   with R, G, and B being integers >= 0, take that as the color of the
   background. Otherwise, determine the background color of IMG
   heuristically. Value is non-zero if successful. */

static int
x_build_heuristic_mask (f, img, how)
     struct frame *f;
     struct image *img;
     Lisp_Object how;
{
#if 0 /* TODO: full image support.  */
  Display *dpy = FRAME_W32_DISPLAY (f);
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
     take that as color.  Otherwise, use the image's background color.  */
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
#else
  return 0;
#endif
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
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
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
  
  if (!parse_image_spec (object, fmt, PBM_LAST, Qpbm)
      || (fmt[PBM_ASCENT].count 
	  && XFASTINT (fmt[PBM_ASCENT].value) > 100))
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
  int c, val = -1;

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


/* Read FILE into memory.  Value is a pointer to a buffer allocated
   with xmalloc holding FILE's contents.  Value is null if an error
   occured.  *SIZE is set to the size of the file.  */

static char *
pbm_read_file (file, size)
     Lisp_Object file;
     int *size;
{
  FILE *fp = NULL;
  char *buf = NULL;
  struct stat st;

  if (stat (XSTRING (file)->data, &st) == 0
      && (fp = fopen (XSTRING (file)->data, "r")) != NULL
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

  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg, &img->pixmap))
    goto error;

#if 0  /* TODO: color tables.  */
  /* Initialize the color hash table.  */
  init_color_table ();
#endif

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
		x_destroy_x_image (ximg);
		image_error ("Invalid pixel value in image `%s'",
			     img->spec, Qnil);
                goto error;
	      }
	    
	    /* RGB values are now in the range 0..max_color_idx.
	       Scale this to the range 0..0xff supported by W32.  */
	    r = (int) ((double) r * 255 / max_color_idx);
	    g = (int) ((double) g * 255 / max_color_idx);
	    b = (int) ((double) b * 255 / max_color_idx);
	    XPutPixel (ximg, x, y,
#if 0  /* TODO: color tables.  */
		       lookup_rgb_color (f, r, g, b));
#else
	    PALETTERGB (r, g, b));
#endif
	  }
    }

#if 0  /* TODO: color tables.  */
  /* Store in IMG->colors the colors allocated for the image, and
     free the color table.  */
  img->colors = colors_in_color_table (&img->ncolors);
  free_color_table ();
#endif
  /* Maybe fill in the background field while we have ximg handy.  */
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
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
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
  
  if (!parse_image_spec (object, fmt, PNG_LAST, Qpng)
      || (fmt[PNG_ASCENT].count 
	  && XFASTINT (fmt[PNG_ASCENT].value) > 100))
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
  png_byte *volatile pixels = NULL;
  png_byte **volatile rows = NULL;
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
          image_error ("Not a PNG file:` %s'", file, Qnil);
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
      png_color_16 *image_background;
      Lisp_Object specified_bg
	= image_spec_value (img->spec, QCbackground, NULL);


      if (STRINGP (specified_bg))
	/* The user specified `:background', use that.  */
	{
	  COLORREF color;
	  if (w32_defined_color (f, XSTRING (specified_bg)->data, &color, 0))
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
      else if (png_get_bKGD (png_ptr, info_ptr, &image_background))
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
      XFreePixmap (FRAME_W32_DISPLAY (f), img->pixmap);
      img->pixmap = 0;
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
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
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
  
  if (!parse_image_spec (object, fmt, JPEG_LAST, Qjpeg)
      || (fmt[JPEG_ASCENT].count 
	  && XFASTINT (fmt[JPEG_ASCENT].value) > 100))
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
     error is detected. This function will perform a longjmp. */
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
        fclose (fp);
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

  /* Create X image and pixmap.  */
  if (!x_create_x_image_and_pixmap (f, width, height, 0, &ximg,
				    &img->pixmap))
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
    fclose (fp);
  
  /* Maybe fill in the background field while we have ximg handy. */
  if (NILP (image_spec_value (img->spec, QCbackground, NULL)))
    IMAGE_BACKGROUND (img, f, ximg);
  
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
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
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
  
  if (!parse_image_spec (object, fmt, TIFF_LAST, Qtiff)
      || (fmt[TIFF_ASCENT].count 
	  && XFASTINT (fmt[TIFF_ASCENT].value) > 100))
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
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
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
  
  if (!parse_image_spec (object, fmt, GIF_LAST, Qgif)
      || (fmt[GIF_ASCENT].count 
	  && XFASTINT (fmt[GIF_ASCENT].value) > 100))
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

  width = img->width = gif->SWidth;
  height = img->height = gif->SHeight;

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
	    int i = raster[y* image_width + x];
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

Lisp_Object Qpostscript;

#ifdef HAVE_GHOSTSCRIPT
static int gs_image_p P_ ((Lisp_Object object));
static int gs_load P_ ((struct frame *f, struct image *img));
static void gs_clear_image P_ ((struct frame *f, struct image *img));

/* The symbol `postscript' identifying images of this type.  */

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
  {":ascent",		IMAGE_NON_NEGATIVE_INTEGER_VALUE,	0},
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
  img->width = in_width * FRAME_W32_DISPLAY_INFO (f)->resx;
  pt_height = image_spec_value (img->spec, QCpt_height, NULL);
  in_height = XFASTINT (pt_height) / 72.0;
  img->height = in_height * FRAME_W32_DISPLAY_INFO (f)->resy;

  /* Create the pixmap.  */
  BLOCK_INPUT;
  xassert (img->pixmap == 0);
  img->pixmap = XCreatePixmap (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
			       img->width, img->height,
			       one_w32_display_info.n_cbits);
  UNBLOCK_INPUT;

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
	   (unsigned long) FRAME_W32_WINDOW (f),
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
  class = FRAME_W32_DISPLAY_INFO (f)->visual->class;
  if (class != StaticColor && class != StaticGray && class != TrueColor)
    {
      XImage *ximg;

      BLOCK_INPUT;

      /* Try to get an XImage for img->pixmep.  */
      ximg = XGetImage (FRAME_W32_DISPLAY (f), img->pixmap,
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
	    x_free_colors (FRAME_W32_DISPLAY (f), cmap,
			   img->colors, img->ncolors, 0);
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

#endif /* HAVE_GHOSTSCRIPT */


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
#if 0 /* TODO : port window properties to W32 */
  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  CHECK_STRING (value);

  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), XSTRING (prop)->data, False);
  XChangeProperty (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
		   prop_atom, XA_STRING, 8, PropModeReplace,
		   XSTRING (value)->data, XSTRING (value)->size);

  /* Make sure the property is set when we return.  */
  XFlush (FRAME_W32_DISPLAY (f));
  UNBLOCK_INPUT;

#endif /* TODO */

  return value;
}


DEFUN ("x-delete-window-property", Fx_delete_window_property,
       Sx_delete_window_property, 1, 2, 0,
       doc: /* Remove window property PROP from X window of FRAME.
FRAME nil or omitted means use the selected frame.  Value is PROP.  */)
  (prop, frame)
     Lisp_Object prop, frame;
{
#if 0 /* TODO : port window properties to W32 */

  struct frame *f = check_x_frame (frame);
  Atom prop_atom;

  CHECK_STRING (prop);
  BLOCK_INPUT;
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), XSTRING (prop)->data, False);
  XDeleteProperty (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f), prop_atom);

  /* Make sure the property is removed when we return.  */
  XFlush (FRAME_W32_DISPLAY (f));
  UNBLOCK_INPUT;
#endif  /* TODO */

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
#if 0 /* TODO : port window properties to W32 */

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
  prop_atom = XInternAtom (FRAME_W32_DISPLAY (f), XSTRING (prop)->data, False);
  rc = XGetWindowProperty (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
			   prop_atom, 0, 0, False, XA_STRING,
			   &actual_type, &actual_format, &actual_size,
			   &bytes_remaining, (unsigned char **) &tmp_data);
  if (rc == Success)
    {
      int size = bytes_remaining;

      XFree (tmp_data);
      tmp_data = NULL;

      rc = XGetWindowProperty (FRAME_W32_DISPLAY (f), FRAME_W32_WINDOW (f),
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

#endif /* TODO */
  return Qnil;
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
#if 0 /* TODO: cursor shape changes.  */
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
#endif
}


/* Cancel the hourglass cursor timer if active, hide an hourglass
   cursor if shown.  */

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

   Display an hourglass cursor on all frames by mapping the frames'
   hourglass_window.  Set the hourglass_p flag in the frames'
   output_data.x structure to indicate that an hourglass cursor is
   shown on the frames.  */

static void
show_hourglass (timer)
     struct atimer *timer;
{
#if 0  /* TODO: cursor shape changes.  */
  /* The timer implementation will cancel this timer automatically
     after this function has run.  Set hourglass_atimer to null
     so that we know the timer doesn't have to be canceled.  */
  hourglass_atimer = NULL;

  if (!hourglass_shown_p)
    {
      Lisp_Object rest, frame;
  
      BLOCK_INPUT;
  
      FOR_EACH_FRAME (rest, frame)
	if (FRAME_W32_P (XFRAME (frame)))
	  {
	    struct frame *f = XFRAME (frame);
	
	    f->output_data.w32->hourglass_p = 1;
	
	    if (!f->output_data.w32->hourglass_window)
	      {
		unsigned long mask = CWCursor;
		XSetWindowAttributes attrs;
	    
		attrs.cursor = f->output_data.w32->hourglass_cursor;
	    
		f->output_data.w32->hourglass_window
		  = XCreateWindow (FRAME_X_DISPLAY (f),
				   FRAME_OUTER_WINDOW (f),
				   0, 0, 32000, 32000, 0, 0,
				   InputOnly,
				   CopyFromParent,
				   mask, &attrs);
	      }
	
	    XMapRaised (FRAME_X_DISPLAY (f),
			f->output_data.w32->hourglass_window);
	    XFlush (FRAME_X_DISPLAY (f));
	  }

      hourglass_shown_p = 1;
      UNBLOCK_INPUT;
    }
#endif
}


/* Hide the hourglass cursor on all frames, if it is currently shown.  */

static void
hide_hourglass ()
{
#if 0 /* TODO: cursor shape changes.  */
  if (hourglass_shown_p)
    {
      Lisp_Object rest, frame;

      BLOCK_INPUT;
      FOR_EACH_FRAME (rest, frame)
	{
	  struct frame *f = XFRAME (frame);
      
	  if (FRAME_W32_P (f)
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
#endif
}



/***********************************************************************
				Tool tips
 ***********************************************************************/

static Lisp_Object x_create_tip_frame P_ ((struct w32_display_info *,
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
      tip_window = NULL;
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
     struct w32_display_info *dpyinfo;
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

  check_w32 ();

  /* Use this general default value to start with until we know if
     this frame has a specified name.  */
  Vx_resource_name = Vinvocation_name;

#ifdef MULTI_KBOARD
  kb = dpyinfo->kboard;
#else
  kb = &the_only_kboard;
#endif

  /* Get the name of the frame to use for resource lookup.  */
  name = w32_get_arg (parms, Qname, "name", "Name", RES_TYPE_STRING);
  if (!STRINGP (name)
      && !EQ (name, Qunbound)
      && !NILP (name))
    error ("Invalid frame name--not a string or nil");
  Vx_resource_name = name;

  frame = Qnil;
  GCPRO3 (parms, name, frame);
  /* Make a frame without minibuffer nor mode-line.  */
  f = make_frame (0);
  f->wants_modeline = 0;
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
  f->output_method = output_w32;
  f->output_data.w32 =
    (struct w32_output *) xmalloc (sizeof (struct w32_output));
  bzero (f->output_data.w32, sizeof (struct w32_output));

  FRAME_FONTSET (f)  = -1;
  f->icon_name = Qnil;

#if 0 /* GLYPH_DEBUG TODO: image support.  */
  image_cache_refcount = FRAME_X_IMAGE_CACHE (f)->refcount;
  dpyinfo_refcount = dpyinfo->reference_count;
#endif /* GLYPH_DEBUG */
#ifdef MULTI_KBOARD
  FRAME_KBOARD (f) = kb;
#endif
  f->output_data.w32->parent_desc = FRAME_W32_DISPLAY_INFO (f)->root_window;
  f->output_data.w32->explicit_parent = 0;

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

  /* Extract the window parameters from the supplied values
     that are needed to determine window geometry.  */
  {
    Lisp_Object font;

    font = w32_get_arg (parms, Qfont, "font", "Font", RES_TYPE_STRING);

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
      font = x_new_font (f, "-*-Courier New-normal-r-*-*-*-100-*-*-c-*-iso8859-1");
    if (! STRINGP (font))
      font = x_new_font (f, "-*-Courier-normal-r-*-*-13-*-*-*-c-*-iso8859-1");
    /* If those didn't work, look for something which will at least work.  */
    if (! STRINGP (font))
      font = x_new_font (f, "-*-Fixedsys-normal-r-*-*-12-*-*-*-c-*-iso8859-1");
    UNBLOCK_INPUT;
    if (! STRINGP (font))
      font = build_string ("Fixedsys");

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

      value = w32_get_arg (parms, Qinternal_border_width,
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

  f->output_data.w32->dwStyle = WS_BORDER | WS_POPUP | WS_DISABLED;
  f->output_data.w32->parent_desc = FRAME_W32_DISPLAY_INFO (f)->root_window;

  window_prompting = x_figure_window_size (f, parms);

  /* No fringes on tip frame.  */
  f->output_data.w32->fringes_extra = 0;
  f->output_data.w32->fringe_cols = 0;
  f->output_data.w32->left_fringe_width = 0;
  f->output_data.w32->right_fringe_width = 0;

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

  BLOCK_INPUT;
  my_create_tip_window (f);
  UNBLOCK_INPUT;

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
  FRAME_W32_DISPLAY_INFO (f)->reference_count++;

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
  
  /* User-specified position?  */
  left = Fcdr (Fassq (Qleft, parms));
  top  = Fcdr (Fassq (Qtop, parms));
  
  /* Move the tooltip window where the mouse pointer is.  Resize and
     show it.  */
  if (!INTEGERP (left) || !INTEGERP (top))
    {
      POINT pt;

      BLOCK_INPUT;
      GetCursorPos (&pt);
      *root_x = pt.x;
      *root_y = pt.y;
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
  else if (*root_x + XINT (dx) + width <= FRAME_W32_DISPLAY_INFO (f)->width)
    /* It fits to the right of the pointer.  */
    *root_x += XINT (dx);
  else if (width + XINT (dx) <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XINT (dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = 0;
}


DEFUN ("x-show-tip", Fx_show_tip, Sx_show_tip, 1, 6, 0,
       doc: /* Show STRING in a \"tooltip\" window on frame FRAME.
A tooltip window is a small window displaying a string.

FRAME nil or omitted means use the selected frame.

PARMS is an optional list of frame parameters which can be
used to change the tooltip's appearance.

Automatically hide the tooltip after TIMEOUT seconds.  TIMEOUT nil
means use the default timeout of 5 seconds.

If the list of frame parameters PARAMS contains a `left' parameter,
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

	  /* Put tooltip in topmost group and in position.  */
	  SetWindowPos (FRAME_W32_WINDOW (f), HWND_TOPMOST,
			root_x, root_y, 0, 0,
			SWP_NOSIZE | SWP_NOACTIVATE);

	  /* Ensure tooltip is on top of other topmost windows (eg menus).  */
	  SetWindowPos (FRAME_W32_WINDOW (f), HWND_TOP,
			0, 0, 0, 0,
			SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);

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

  /* Block input until the tip has been fully drawn, to avoid crashes
     when drawing tips in menus.  */
  BLOCK_INPUT;

  /* Create a frame for the tooltip, and record it in the global
     variable tip_frame.  */
  frame = x_create_tip_frame (FRAME_W32_DISPLAY_INFO (f), parms, string);
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

#ifdef TODO /* Investigate why some fonts need more width than is
	       calculated for some tooltips.  */
      /* There's a glyph at the end of rows that is use to place
	 the cursor there.  Don't include the width of this glyph.  */
      if (row->used[TEXT_AREA])
	{
	  last = &row->glyphs[TEXT_AREA][row->used[TEXT_AREA] - 1];
	  row_width = row->pixel_width - last->pixel_width;
	}
      else
#endif
	row_width = row->pixel_width;
      
      /* TODO: find why tips do not draw along baseline as instructed.  */
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

  {
    /* Adjust Window size to take border into account.  */
    RECT rect;
    rect.left = rect.top = 0;
    rect.right = width;
    rect.bottom = height;
    AdjustWindowRect (&rect, f->output_data.w32->dwStyle,
		      FRAME_EXTERNAL_MENU_BAR (f));

    /* Position and size tooltip, and put it in the topmost group.  */
    SetWindowPos (FRAME_W32_WINDOW (f), HWND_TOPMOST,
		  root_x, root_y, rect.right - rect.left,
		  rect.bottom - rect.top, SWP_NOACTIVATE);

    /* Ensure tooltip is on top of other topmost windows (eg menus).  */
    SetWindowPos (FRAME_W32_WINDOW (f), HWND_TOP,
		  0, 0, 0, 0,
		  SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);

    /* Let redisplay know that we have made the frame visible already.  */
    f->async_visible = 1;

    ShowWindow (FRAME_W32_WINDOW (f), SW_SHOWNOACTIVATE);
  }

  /* Draw into the window.  */
  w->must_be_updated_p = 1;
  update_single_window (w, 1);

  UNBLOCK_INPUT;

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
    }

  UNGCPRO;
  return unbind_to (count, deleted);
}



/***********************************************************************
			File selection dialog
 ***********************************************************************/

extern Lisp_Object Qfile_name_history;

DEFUN ("x-file-dialog", Fx_file_dialog, Sx_file_dialog, 2, 4, 0,
       doc: /* Read file name, prompting with PROMPT in directory DIR.
Use a file selection dialog.
Select DEFAULT-FILENAME in the dialog's file selection box, if
specified.  Ensure that file exists if MUSTMATCH is non-nil.  */)
  (prompt, dir, default_filename, mustmatch)
     Lisp_Object prompt, dir, default_filename, mustmatch;
{
  struct frame *f = SELECTED_FRAME ();
  Lisp_Object file = Qnil;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  char filename[MAX_PATH + 1];
  char init_dir[MAX_PATH + 1];
  int use_dialog_p = 1;

  GCPRO5 (prompt, dir, default_filename, mustmatch, file);
  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Create the dialog with PROMPT as title, using DIR as initial
     directory and using "*" as pattern.  */
  dir = Fexpand_file_name (dir, Qnil);
  strncpy (init_dir, XSTRING (dir)->data, MAX_PATH);
  init_dir[MAX_PATH] = '\0';
  unixtodos_filename (init_dir);

  if (STRINGP (default_filename))
    {
      char *file_name_only;
      char *full_path_name = XSTRING (default_filename)->data;

      unixtodos_filename (full_path_name);

      file_name_only = strrchr (full_path_name, '\\');
      if (!file_name_only)
        file_name_only = full_path_name;
      else
        {
          file_name_only++;

          /* If default_file_name is a directory, don't use the open
             file dialog, as it does not support selecting
             directories. */
          if (!(*file_name_only))
            use_dialog_p = 0;
        }

      strncpy (filename, file_name_only, MAX_PATH);
      filename[MAX_PATH] = '\0';
    }
  else
    filename[0] = '\0';

  if (use_dialog_p)
    {
      OPENFILENAME file_details;

      /* Prevent redisplay.  */
      specbind (Qinhibit_redisplay, Qt);
      BLOCK_INPUT;

      bzero (&file_details, sizeof (file_details));
      file_details.lStructSize = sizeof (file_details);
      file_details.hwndOwner = FRAME_W32_WINDOW (f);
      /* Undocumented Bug in Common File Dialog:
         If a filter is not specified, shell links are not resolved.  */
      file_details.lpstrFilter = "ALL Files (*.*)\0*.*\0\0";
      file_details.lpstrFile = filename;
      file_details.nMaxFile = sizeof (filename);
      file_details.lpstrInitialDir = init_dir;
      file_details.lpstrTitle = XSTRING (prompt)->data;
      file_details.Flags = OFN_HIDEREADONLY | OFN_NOCHANGEDIR;

      if (!NILP (mustmatch))
        file_details.Flags |= OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST;

      if (GetOpenFileName (&file_details))
        {
          dostounix_filename (filename);
          file = DECODE_FILE(build_string (filename));
        }
      else
        file = Qnil;

      UNBLOCK_INPUT;
      file = unbind_to (count, file);
    }
  /* Open File dialog will not allow folders to be selected, so resort
     to minibuffer completing reads for directories. */
  else
    file = Fcompleting_read (prompt, intern ("read-file-name-internal"),
                             dir, mustmatch, dir, Qfile_name_history,
                             default_filename, Qnil);

  UNGCPRO;

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    Fsignal (Qquit, Qnil);

  return unbind_to (count, file);
}



/***********************************************************************
                         w32 specialized functions
 ***********************************************************************/

DEFUN ("w32-select-font", Fw32_select_font, Sw32_select_font, 0, 2, 0,
       doc: /* Select a font using the W32 font dialog.
Returns an X font string corresponding to the selection.  */)
  (frame, include_proportional)
     Lisp_Object frame, include_proportional;
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
  cf.Flags = CF_FORCEFONTEXIST | CF_SCREENFONTS | CF_NOVERTFONTS;

  /* Unless include_proportional is non-nil, limit the selection to
     monospaced fonts.  */
  if (NILP (include_proportional))
    cf.Flags |= CF_FIXEDPITCHONLY;

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
      lf.lfCharSet = tm.tmCharSet;
      cf.Flags |= CF_INITTOLOGFONTSTRUCT;
    }
  SelectObject (hdc, oldobj);
  ReleaseDC (FRAME_W32_WINDOW (f), hdc);

  if (!ChooseFont (&cf) || !w32_to_x_font (&lf, buf, 100, NULL))
      return Qnil;

  return build_string (buf);
}

DEFUN ("w32-send-sys-command", Fw32_send_sys_command,
       Sw32_send_sys_command, 1, 2, 0,
       doc: /* Send frame a Windows WM_SYSCOMMAND message of type COMMAND.
Some useful values for command are #xf030 to maximise frame (#xf020
to minimize), #xf120 to restore frame to original size, and #xf100
to activate the menubar for keyboard access.  #xf140 activates the
screen saver if defined.

If optional parameter FRAME is not specified, use selected frame.  */)
  (command, frame)
     Lisp_Object command, frame;
{
  FRAME_PTR f = check_x_frame (frame);

  CHECK_NUMBER (command);

  PostMessage (FRAME_W32_WINDOW (f), WM_SYSCOMMAND, XINT (command), 0);

  return Qnil;
}

DEFUN ("w32-shell-execute", Fw32_shell_execute, Sw32_shell_execute, 2, 4, 0,
       doc: /* Get Windows to perform OPERATION on DOCUMENT.
This is a wrapper around the ShellExecute system function, which
invokes the application registered to handle OPERATION for DOCUMENT.
OPERATION is typically \"open\", \"print\" or \"explore\" (but can be
nil for the default action), and DOCUMENT is typically the name of a
document file or URL, but can also be a program executable to run or
a directory to open in the Windows Explorer.

If DOCUMENT is a program executable, PARAMETERS can be a string
containing command line parameters, but otherwise should be nil.

SHOW-FLAG can be used to control whether the invoked application is hidden
or minimized.  If SHOW-FLAG is nil, the application is displayed normally,
otherwise it is an integer representing a ShowWindow flag:

  0 - start hidden
  1 - start normally
  3 - start maximized
  6 - start minimized  */)
  (operation, document, parameters, show_flag)
     Lisp_Object operation, document, parameters, show_flag;
{
  Lisp_Object current_dir;

  CHECK_STRING (document);

  /* Encode filename and current directory.  */
  current_dir = ENCODE_FILE (current_buffer->directory);
  document = ENCODE_FILE (document);
  if ((int) ShellExecute (NULL,
			  (STRINGP (operation) ?
			   XSTRING (operation)->data : NULL),
			  XSTRING (document)->data,
			  (STRINGP (parameters) ?
			   XSTRING (parameters)->data : NULL),
			  XSTRING (current_dir)->data,
			  (INTEGERP (show_flag) ?
			   XINT (show_flag) : SW_SHOWDEFAULT))
      > 32)
    return Qt;
  error ("ShellExecute failed: %s", w32_strerror (0));
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

  CHECK_VECTOR (key);

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

  /* Supply defs missing from mingw32.  */
#ifndef MOD_ALT
#define MOD_ALT         0x0001
#define MOD_CONTROL     0x0002
#define MOD_SHIFT       0x0004
#define MOD_WIN         0x0008
#endif

  /* Convert lisp modifiers to Windows hot-key form.  */
  w32_modifiers  = (lisp_modifiers & hyper_modifier)    ? MOD_WIN : 0;
  w32_modifiers |= (lisp_modifiers & alt_modifier)      ? MOD_ALT : 0;
  w32_modifiers |= (lisp_modifiers & ctrl_modifier)     ? MOD_CONTROL : 0;
  w32_modifiers |= (lisp_modifiers & shift_modifier)    ? MOD_SHIFT : 0;

  return HOTKEY (vk_code, w32_modifiers);
}

DEFUN ("w32-register-hot-key", Fw32_register_hot_key,
       Sw32_register_hot_key, 1, 1, 0,
       doc: /* Register KEY as a hot-key combination.
Certain key combinations like Alt-Tab are reserved for system use on
Windows, and therefore are normally intercepted by the system.  However,
most of these key combinations can be received by registering them as
hot-keys, overriding their special meaning.

KEY must be a one element key definition in vector form that would be
acceptable to `define-key' (e.g. [A-tab] for Alt-Tab).  The meta
modifier is interpreted as Alt if `w32-alt-is-meta' is t, and hyper
is always interpreted as the Windows modifier keys.

The return value is the hotkey-id if registered, otherwise nil.  */)
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
	XSETCAR (item, key);

      /* Notify input thread about new hot-key definition, so that it
	 takes effect without needing to switch focus.  */
      PostThreadMessage (dwWindowsThreadId, WM_EMACS_REGISTER_HOT_KEY,
			 (WPARAM) key, 0);
    }

  return key;
}

DEFUN ("w32-unregister-hot-key", Fw32_unregister_hot_key,
       Sw32_unregister_hot_key, 1, 1, 0,
       doc: /* Unregister HOTKEY as a hot-key combination.  */)
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

DEFUN ("w32-registered-hot-keys", Fw32_registered_hot_keys,
       Sw32_registered_hot_keys, 0, 0, 0,
       doc: /* Return list of registered hot-key IDs.  */)
  ()
{
  return Fcopy_sequence (w32_grabbed_keys);
}

DEFUN ("w32-reconstruct-hot-key", Fw32_reconstruct_hot_key,
       Sw32_reconstruct_hot_key, 1, 1, 0,
       doc: /* Convert hot-key ID to a lisp key combination.  */)
  (hotkeyid)
     Lisp_Object hotkeyid;
{
  int vk_code, w32_modifiers;
  Lisp_Object key;

  CHECK_NUMBER (hotkeyid);

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

DEFUN ("w32-toggle-lock-key", Fw32_toggle_lock_key,
       Sw32_toggle_lock_key, 1, 2, 0,
       doc: /* Toggle the state of the lock key KEY.
KEY can be `capslock', `kp-numlock', or `scroll'.
If the optional parameter NEW-STATE is a number, then the state of KEY
is set to off if the low bit of NEW-STATE is zero, otherwise on.  */)
  (key, new_state)
     Lisp_Object key, new_state;
{
  int vk_code;

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

DEFUN ("file-system-info", Ffile_system_info, Sfile_system_info, 1, 1, 0,
       doc: /* Return storage information about the file system FILENAME is on.
Value is a list of floats (TOTAL FREE AVAIL), where TOTAL is the total
storage of the file system, FREE is the free storage, and AVAIL is the
storage available to a non-superuser.  All 3 numbers are in bytes.
If the underlying system call fails, value is nil.  */)
  (filename)
  Lisp_Object filename;
{
  Lisp_Object encoded, value;

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);
  encoded = ENCODE_FILE (filename);

  value = Qnil;

  /* Determining the required information on Windows turns out, sadly,
     to be more involved than one would hope.  The original Win32 api
     call for this will return bogus information on some systems, but we
     must dynamically probe for the replacement api, since that was
     added rather late on.  */
  {
    HMODULE hKernel = GetModuleHandle ("kernel32");
    BOOL (*pfn_GetDiskFreeSpaceEx)
      (char *, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER)
      = (void *) GetProcAddress (hKernel, "GetDiskFreeSpaceEx");

    /* On Windows, we may need to specify the root directory of the
       volume holding FILENAME.  */
    char rootname[MAX_PATH];
    char *name = XSTRING (encoded)->data;

    /* find the root name of the volume if given */
    if (isalpha (name[0]) && name[1] == ':')
      {
	rootname[0] = name[0];
	rootname[1] = name[1];
	rootname[2] = '\\';
	rootname[3] = 0;
      }
    else if (IS_DIRECTORY_SEP (name[0]) && IS_DIRECTORY_SEP (name[1]))
      {
	char *str = rootname;
	int slashes = 4;
	do
	  {
	    if (IS_DIRECTORY_SEP (*name) && --slashes == 0)
	      break;
	    *str++ = *name++;
	  }
	while ( *name );

	*str++ = '\\';
	*str = 0;
      }

    if (pfn_GetDiskFreeSpaceEx)
      {
	/* Unsigned large integers cannot be cast to double, so
	   use signed ones instead.  */
	LARGE_INTEGER availbytes;
	LARGE_INTEGER freebytes;
	LARGE_INTEGER totalbytes;

	if (pfn_GetDiskFreeSpaceEx(rootname,
				   (ULARGE_INTEGER *)&availbytes,
				   (ULARGE_INTEGER *)&totalbytes,
				   (ULARGE_INTEGER *)&freebytes))
	  value = list3 (make_float ((double) totalbytes.QuadPart),
			 make_float ((double) freebytes.QuadPart),
			 make_float ((double) availbytes.QuadPart));
      }
    else
      {
	DWORD sectors_per_cluster;
	DWORD bytes_per_sector;
	DWORD free_clusters;
	DWORD total_clusters;

	if (GetDiskFreeSpace(rootname,
			     &sectors_per_cluster,
			     &bytes_per_sector,
			     &free_clusters,
			     &total_clusters))
	  value = list3 (make_float ((double) total_clusters
				     * sectors_per_cluster * bytes_per_sector),
			 make_float ((double) free_clusters
				     * sectors_per_cluster * bytes_per_sector),
			 make_float ((double) free_clusters
				     * sectors_per_cluster * bytes_per_sector));
      }
  }

  return value;
}

/***********************************************************************
			    Initialization
 ***********************************************************************/

void
syms_of_w32fns ()
{
  HMODULE user32_lib = GetModuleHandle ("user32.dll");

  /* This is zero if not using MS-Windows.  */
  w32_in_use = 0;

  /* TrackMouseEvent not available in all versions of Windows, so must load
     it dynamically.  Do it once, here, instead of every time it is used.  */
  track_mouse_event_fn = GetProcAddress (user32_lib, "TrackMouseEvent");
  track_mouse_window = NULL;

  w32_visible_system_caret_hwnd = NULL;

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
  Qx_frame_parameter = intern ("x-frame-parameter");
  staticpro (&Qx_frame_parameter);
  Qx_resource_name = intern ("x-resource-name");
  staticpro (&Qx_resource_name);
  Quser_position = intern ("user-position");
  staticpro (&Quser_position);
  Quser_size = intern ("user-size");
  staticpro (&Quser_size);
  Qscreen_gamma = intern ("screen-gamma");
  staticpro (&Qscreen_gamma);
  Qline_spacing = intern ("line-spacing");
  staticpro (&Qline_spacing);
  Qcenter = intern ("center");
  staticpro (&Qcenter);
  Qcancel_timer = intern ("cancel-timer");
  staticpro (&Qcancel_timer);
  Qfullscreen = intern ("fullscreen");
  staticpro (&Qfullscreen);
  Qfullwidth = intern ("fullwidth");
  staticpro (&Qfullwidth);
  Qfullheight = intern ("fullheight");
  staticpro (&Qfullheight);
  Qfullboth = intern ("fullboth");
  staticpro (&Qfullboth);

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

  staticpro (&w32_grabbed_keys);
  w32_grabbed_keys = Qnil;

  DEFVAR_LISP ("w32-color-map", &Vw32_color_map,
	       doc: /* An array of color name mappings for windows.  */);
  Vw32_color_map = Qnil;

  DEFVAR_LISP ("w32-pass-alt-to-system", &Vw32_pass_alt_to_system,
	       doc: /* Non-nil if alt key presses are passed on to Windows.
When non-nil, for example, alt pressed and released and then space will
open the System menu.  When nil, Emacs silently swallows alt key events.  */);
  Vw32_pass_alt_to_system = Qnil;

  DEFVAR_LISP ("w32-alt-is-meta", &Vw32_alt_is_meta,
	       doc: /* Non-nil if the alt key is to be considered the same as the meta key.
When nil, Emacs will translate the alt key to the Alt modifier, and not Meta.  */);
  Vw32_alt_is_meta = Qt;

  DEFVAR_INT ("w32-quit-key", &Vw32_quit_key,
	      doc: /* If non-zero, the virtual key code for an alternative quit key.  */);
  XSETINT (Vw32_quit_key, 0);

  DEFVAR_LISP ("w32-pass-lwindow-to-system", 
	       &Vw32_pass_lwindow_to_system,
	       doc: /* Non-nil if the left \"Windows\" key is passed on to Windows.
When non-nil, the Start menu is opened by tapping the key.  */);
  Vw32_pass_lwindow_to_system = Qt;

  DEFVAR_LISP ("w32-pass-rwindow-to-system", 
	       &Vw32_pass_rwindow_to_system,
	       doc: /* Non-nil if the right \"Windows\" key is passed on to Windows.
When non-nil, the Start menu is opened by tapping the key.  */);
  Vw32_pass_rwindow_to_system = Qt;

  DEFVAR_INT ("w32-phantom-key-code",
	       &Vw32_phantom_key_code,
	      doc: /* Virtual key code used to generate \"phantom\" key presses.
Value is a number between 0 and 255.

Phantom key presses are generated in order to stop the system from
acting on \"Windows\" key events when `w32-pass-lwindow-to-system' or
`w32-pass-rwindow-to-system' is nil.  */);
  /* Although 255 is technically not a valid key code, it works and
     means that this hack won't interfere with any real key code.  */
  Vw32_phantom_key_code = 255;

  DEFVAR_LISP ("w32-enable-num-lock", 
	       &Vw32_enable_num_lock,
	       doc: /* Non-nil if Num Lock should act normally.
Set to nil to see Num Lock as the key `kp-numlock'.  */);
  Vw32_enable_num_lock = Qt;

  DEFVAR_LISP ("w32-enable-caps-lock", 
	       &Vw32_enable_caps_lock,
	       doc: /* Non-nil if Caps Lock should act normally.
Set to nil to see Caps Lock as the key `capslock'.  */);
  Vw32_enable_caps_lock = Qt;

  DEFVAR_LISP ("w32-scroll-lock-modifier",
	       &Vw32_scroll_lock_modifier,
	       doc: /* Modifier to use for the Scroll Lock on state.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to see Scroll Lock as the key `scroll'.
Any other value will cause the key to be ignored.  */);
  Vw32_scroll_lock_modifier = Qt;

  DEFVAR_LISP ("w32-lwindow-modifier",
	       &Vw32_lwindow_modifier,
	       doc: /* Modifier to use for the left \"Windows\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the key `lwindow'.
Any other value will cause the key to be ignored.  */);
  Vw32_lwindow_modifier = Qnil;

  DEFVAR_LISP ("w32-rwindow-modifier",
	       &Vw32_rwindow_modifier,
	       doc: /* Modifier to use for the right \"Windows\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the key `rwindow'.
Any other value will cause the key to be ignored.  */);
  Vw32_rwindow_modifier = Qnil;

  DEFVAR_LISP ("w32-apps-modifier",
	       &Vw32_apps_modifier,
	       doc: /* Modifier to use for the \"Apps\" key.
The value can be hyper, super, meta, alt, control or shift for the
respective modifier, or nil to appear as the key `apps'.
Any other value will cause the key to be ignored.  */);
  Vw32_apps_modifier = Qnil;

  DEFVAR_BOOL ("w32-enable-synthesized-fonts", &w32_enable_synthesized_fonts,
	       doc: /* Non-nil enables selection of artificially italicized and bold fonts.  */);
  w32_enable_synthesized_fonts = 0;

  DEFVAR_LISP ("w32-enable-palette", &Vw32_enable_palette,
	       doc: /* Non-nil enables Windows palette management to map colors exactly.  */);
  Vw32_enable_palette = Qt;

  DEFVAR_INT ("w32-mouse-button-tolerance",
	      &Vw32_mouse_button_tolerance,
	      doc: /* Analogue of double click interval for faking middle mouse events.
The value is the minimum time in milliseconds that must elapse between
left/right button down events before they are considered distinct events.
If both mouse buttons are depressed within this interval, a middle mouse
button down event is generated instead.  */);
  XSETINT (Vw32_mouse_button_tolerance, GetDoubleClickTime () / 2);

  DEFVAR_INT ("w32-mouse-move-interval",
	      &Vw32_mouse_move_interval,
	      doc: /* Minimum interval between mouse move events.
The value is the minimum time in milliseconds that must elapse between
successive mouse move (or scroll bar drag) events before they are
reported as lisp events.  */);
  XSETINT (Vw32_mouse_move_interval, 0);

  DEFVAR_BOOL ("w32-pass-extra-mouse-buttons-to-system",
	       &w32_pass_extra_mouse_buttons_to_system,
	       doc: /* Non-nil if the fourth and fifth mouse buttons are passed to Windows.
Recent versions of Windows support mice with up to five buttons.
Since most applications don't support these extra buttons, most mouse
drivers will allow you to map them to functions at the system level.
If this variable is non-nil, Emacs will pass them on, allowing the
system to handle them.  */);
  w32_pass_extra_mouse_buttons_to_system = 0;

  init_x_parm_symbols ();

  DEFVAR_LISP ("x-bitmap-file-path", &Vx_bitmap_file_path,
	       doc: /* List of directories to search for bitmap files for w32.  */);
  Vx_bitmap_file_path = decode_env_path ((char *) 0, "PATH");

  DEFVAR_LISP ("x-pointer-shape", &Vx_pointer_shape,
	       doc: /* The shape of the pointer when over text.
Changing the value does not affect existing frames
unless you set the mouse color.  */);
  Vx_pointer_shape = Qnil;

  DEFVAR_LISP ("x-resource-name", &Vx_resource_name,
	       doc: /* The name Emacs uses to look up resources; for internal use only.
`x-get-resource' uses this as the first component of the instance name
when requesting resource values.
Emacs initially sets `x-resource-name' to the name under which Emacs
was invoked, or to the value specified with the `-name' or `-rn'
switches, if present.  */);
  Vx_resource_name = Qnil;

  Vx_nontext_pointer_shape = Qnil;

  Vx_mode_pointer_shape = Qnil;

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
	       doc: /* Maximum size for tooltips.
Value is a pair (COLUMNS . ROWS). Text larger than this is clipped.  */);
  Vx_max_tooltip_size = Fcons (make_number (80), make_number (40));
  
  DEFVAR_LISP ("x-no-window-manager", &Vx_no_window_manager,
	       doc: /* Non-nil if no window manager is in use.
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

  DEFVAR_LISP ("w32-bdf-filename-alist",
               &Vw32_bdf_filename_alist,
               doc: /* List of bdf fonts and their corresponding filenames.  */);
  Vw32_bdf_filename_alist = Qnil;

  DEFVAR_BOOL ("w32-strict-fontnames",
               &w32_strict_fontnames,
	       doc: /* Non-nil means only use fonts that are exact matches for those requested.
Default is nil, which allows old fontnames that are not XLFD compliant,
and allows third-party CJK display to work by specifying false charset
fields to trick Emacs into translating to Big5, SJIS etc.
Setting this to t will prevent wrong fonts being selected when
fontsets are automatically created.  */);
  w32_strict_fontnames = 0;

  DEFVAR_BOOL ("w32-strict-painting",
               &w32_strict_painting,
	       doc: /* Non-nil means use strict rules for repainting frames.
Set this to nil to get the old behaviour for repainting; this should
only be necessary if the default setting causes problems.  */);
  w32_strict_painting = 1;

  DEFVAR_LISP ("w32-charset-info-alist",
               &Vw32_charset_info_alist,
               doc: /* Alist linking Emacs character sets to Windows fonts and codepages.
Each entry should be of the form:

   (CHARSET_NAME . (WINDOWS_CHARSET . CODEPAGE))

where CHARSET_NAME is a string used in font names to identify the charset,
WINDOWS_CHARSET is a symbol that can be one of:
w32-charset-ansi, w32-charset-default, w32-charset-symbol,
w32-charset-shiftjis, w32-charset-hangeul, w32-charset-gb2312,
w32-charset-chinesebig5,
#ifdef JOHAB_CHARSET
w32-charset-johab, w32-charset-hebrew,
w32-charset-arabic, w32-charset-greek, w32-charset-turkish,
w32-charset-vietnamese, w32-charset-thai, w32-charset-easteurope,
w32-charset-russian, w32-charset-mac, w32-charset-baltic,
#endif
#ifdef UNICODE_CHARSET
w32-charset-unicode,
#endif
or w32-charset-oem.
CODEPAGE should be an integer specifying the codepage that should be used
to display the character set, t to do no translation and output as Unicode,
or nil to do no translation and output as 8 bit (or multibyte on far-east
versions of Windows) characters.  */);
    Vw32_charset_info_alist = Qnil;

  staticpro (&Qw32_charset_ansi);
  Qw32_charset_ansi = intern ("w32-charset-ansi");
  staticpro (&Qw32_charset_symbol);
  Qw32_charset_symbol = intern ("w32-charset-symbol");
  staticpro (&Qw32_charset_shiftjis);
  Qw32_charset_shiftjis = intern ("w32-charset-shiftjis");
  staticpro (&Qw32_charset_hangeul);
  Qw32_charset_hangeul = intern ("w32-charset-hangeul");
  staticpro (&Qw32_charset_chinesebig5);
  Qw32_charset_chinesebig5 = intern ("w32-charset-chinesebig5");
  staticpro (&Qw32_charset_gb2312);
  Qw32_charset_gb2312 = intern ("w32-charset-gb2312");
  staticpro (&Qw32_charset_oem);
  Qw32_charset_oem = intern ("w32-charset-oem");

#ifdef JOHAB_CHARSET
  {
    static int w32_extra_charsets_defined = 1;
    DEFVAR_BOOL ("w32-extra-charsets-defined", &w32_extra_charsets_defined,
		 doc: /* Internal variable.  */);

    staticpro (&Qw32_charset_johab);
    Qw32_charset_johab = intern ("w32-charset-johab");
    staticpro (&Qw32_charset_easteurope);
    Qw32_charset_easteurope = intern ("w32-charset-easteurope");
    staticpro (&Qw32_charset_turkish);
    Qw32_charset_turkish = intern ("w32-charset-turkish");
    staticpro (&Qw32_charset_baltic);
    Qw32_charset_baltic = intern ("w32-charset-baltic");
    staticpro (&Qw32_charset_russian);
    Qw32_charset_russian = intern ("w32-charset-russian");
    staticpro (&Qw32_charset_arabic);
    Qw32_charset_arabic = intern ("w32-charset-arabic");
    staticpro (&Qw32_charset_greek);
    Qw32_charset_greek = intern ("w32-charset-greek");
    staticpro (&Qw32_charset_hebrew);
    Qw32_charset_hebrew = intern ("w32-charset-hebrew");
    staticpro (&Qw32_charset_vietnamese);
    Qw32_charset_vietnamese = intern ("w32-charset-vietnamese");
    staticpro (&Qw32_charset_thai);
    Qw32_charset_thai = intern ("w32-charset-thai");
    staticpro (&Qw32_charset_mac);
    Qw32_charset_mac = intern ("w32-charset-mac");
  }
#endif

#ifdef UNICODE_CHARSET
  {
    static int w32_unicode_charset_defined = 1;
    DEFVAR_BOOL ("w32-unicode-charset-defined",
                 &w32_unicode_charset_defined,
		 doc: /* Internal variable.  */);

    staticpro (&Qw32_charset_unicode);
    Qw32_charset_unicode = intern ("w32-charset-unicode");
#endif

  defsubr (&Sx_get_resource);
#if 0 /* TODO: Port to W32 */
  defsubr (&Sx_change_window_property);
  defsubr (&Sx_delete_window_property);
  defsubr (&Sx_window_property);
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

  defsubr (&Sfile_system_info);

  /* Setting callback functions for fontset handler.  */
  get_font_info_func = w32_get_font_info;

#if 0 /* This function pointer doesn't seem to be used anywhere.
	 And the pointer assigned has the wrong type, anyway.  */
  list_fonts_func = w32_list_fonts;
#endif

  load_font_func = w32_load_font;
  find_ccl_program_func = w32_find_ccl_program;
  query_font_func = w32_query_font;
  set_frame_fontset_func = x_set_font;
  check_window_system_func = check_w32;

  /* Images.  */
  Qxbm = intern ("xbm");
  staticpro (&Qxbm);
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
#if 0 /* TODO: These need entries at top of file.  */
  QCloader = intern (":loader");
  staticpro (&QCloader);
  QCbounding_box = intern (":bounding-box");
  staticpro (&QCbounding_box);
  QCpt_width = intern (":pt-width");
  staticpro (&QCpt_width);
  QCpt_height = intern (":pt-height");
  staticpro (&QCpt_height);
#endif
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

#if GLYPH_DEBUG
  defsubr (&Simagep);
  defsubr (&Slookup_image);
#endif

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

  defsubr (&Sx_file_dialog);
}


void
init_xfns ()
{
  image_types = NULL;
  Vimage_types = Qnil;

  define_image_type (&pbm_type);
  define_image_type (&xbm_type);
#if 0 /* TODO : Image support for W32 */
  define_image_type (&gs_type);
#endif
  
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
